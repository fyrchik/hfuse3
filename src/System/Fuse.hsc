-----------------------------------------------------------------------------
-- |
-- Module      :  System.Fuse
-- Copyright   :  (c) Jérémy Bobbio, Taru Karttunen
-- License     :  BSD-style
--
-- Maintainer  :  Montez Fitzpatrick
-- Stability   :  experimental
-- Portability :  GHC 6.4-7.8.2
--
-- A binding for the FUSE (Filesystem in USErspace) library
-- (<http://fuse.sourceforge.net/>), which allows filesystems to be implemented
-- as userspace processes.
--
-- The binding tries to follow as much as possible current Haskell POSIX
-- interface in "System.Posix.Files" and "System.Posix.Directory".
--
-- FUSE uses POSIX threads, so any Haskell application using this library must
-- be linked against a threaded runtime system (eg. using the @threaded@ GHC
-- option).
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
module System.Fuse
    ( -- * Using FUSE

      -- $intro

      module Foreign.C.Error
    , module System.Fuse.FuseOpt
    , FuseOperations(..)
    , defaultFuseOps
    , fuseMain -- :: FuseOperations fh -> (Exception -> IO Errno) -> IO ()
    , fuseMainOpts -- :: Storable a  => FuseOperations fh -> (Exception -> IO Errno) -> OptSpec a -> IO ()
    , fM -- just testing, for later use
    , fuse_get_context
    , fuse_main
    , fuseRun -- :: String -> [String] -> FuseOperations fh -> (Exception -> IO Errno) -> IO ()
    , defaultExceptionHandler -- :: Exception -> IO Errno
      -- * Operations datatypes
    , FileStat(..)
    , EntryType(..)
    , FileSystemStats(..)
    , SyncType(..)
      -- * FUSE Context
    , getFuseContext -- :: IO FuseContext
    , FuseContext(fuseCtxUserID, fuseCtxGroupID, fuseCtxProcessID, fuseCtxPrivateData)
      -- * File modes
    , entryTypeToFileMode -- :: EntryType -> FileMode
    , fileModeToEntryType -- :: FileMode -> EntryType
    , OpenMode(..)
    , OpenFileFlags(..)
    , intersectFileModes -- :: FileMode
    , unionFileModes -- :: FileMode
    ) where

import Prelude hiding ( Read )

import System.Fuse.CTypes
import System.Fuse.FuseOperations
import System.Fuse.FuseOpt
import System.Fuse.Types

import Control.Monad
import Control.Exception as E(Exception, handle, finally, SomeException)
import qualified Data.ByteString.Char8    as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe   as B
import Foreign
import Foreign.C
import Foreign.C.Error
import Foreign.Marshal
import System.Environment ( getProgName, getArgs )
import System.IO ( hPutStrLn, stderr, withFile, stdin, stdout, IOMode(..) )
import System.Posix.Types
import System.Posix.Files ( accessModes, intersectFileModes, unionFileModes )
import System.Posix.Directory(changeWorkingDirectory)
import System.Posix.Process(forkProcess,createSession,exitImmediately)
import System.Posix.IO ( OpenMode(..), OpenFileFlags(..) )
import qualified System.Posix.Signals as Signals
import GHC.IO.Handle(hDuplicateTo)
import System.Exit


#if MIN_VERSION_base(4,6,0)
import System.IO.Error (catchIOError,ioeGetErrorString)

catch = catchIOError
#else
import System.IO.Error (catch,ioeGetErrorString)
#endif

-- TODO: FileMode -> Permissions
-- TODO: Arguments !
-- TODO: implement binding to fuse_invalidate
-- TODO: bind fuse_*xattr

#define FUSE_USE_VERSION 30

#include <sys/statfs.h>

#include <dirent.h>
#include <fuse.h>
#include <fuse3/fuse_lowlevel.h>
#include <fcntl.h>

{- $intro
'FuseOperations' contains a field for each filesystem operations that can be called
by FUSE. Think like if you were implementing a file system inside the Linux kernel.

Each actions must return a POSIX error code, also called 'Errno' reflecting
operation result. For actions not using 'Either', you should return 'eOK' in case
of success.

Read and writes are done with Haskell 'ByteString' type.

-}

{-  All operations should return the negated error value (-errno) on
      error.
-}

{-
    There is no create() operation, mknod() will be called for
    creation of all non directory, non symlink nodes.
-}

-- | Returned by 'getFuseContext'.
data FuseContext = FuseContext
    { fuseCtxUserID :: UserID
    , fuseCtxGroupID :: GroupID
    , fuseCtxProcessID :: ProcessID
    , fuseCtxPrivateData :: Ptr ()
    }

-- | Returns the context of the program doing the current FUSE call.
getFuseContext :: IO FuseContext
getFuseContext =
    do pCtx <- fuse_get_context
       userID <- (#peek struct fuse_context, uid) pCtx
       groupID <- (#peek struct fuse_context, gid) pCtx
       processID <- (#peek struct fuse_context, pid) pCtx
       pData <-(#peek struct fuse_context, private_data) pCtx
       return $ FuseContext { fuseCtxUserID = userID
                            , fuseCtxGroupID = groupID
                            , fuseCtxProcessID = processID
                            , fuseCtxPrivateData = pData
                            }

-- Allocates a fuse_args struct to hold the commandline arguments.
withFuseArgs :: String -> [String] -> (Ptr CFuseArgs -> IO b) -> IO b
withFuseArgs prog args f =
    do let allArgs = (prog:args)
           argc = length allArgs
       withMany withCString allArgs (\ cArgs ->
           withArray cArgs $ (\ pArgv ->
               allocaBytes (#size struct fuse_args) (\ fuseArgs ->
                    do (#poke struct fuse_args, argc) fuseArgs argc
                       (#poke struct fuse_args, argv) fuseArgs pArgv
                       (#poke struct fuse_args, allocated) fuseArgs (0::CInt)
                       finally (f fuseArgs)
                               (fuse_opt_free_args fuseArgs))))

withStructFuse :: forall e fh b. Exception e
               => Ptr CFuseArgs
               -> FuseOperations fh
               -> (e -> IO Errno)
               -> (Ptr CStructFuse -> IO b)
               -> IO b
withStructFuse = withStructFuseOpts nullPtr

withStructFuseOpts pData pArgs ops handler f =
    withFuseOps ops handler $ \pOps -> do
      structFuse <- fuse_new pArgs pOps (#size struct fuse_operations) pData
      if structFuse == nullPtr
        then fail ""
        else E.finally (f structFuse)
                       (fuse_destroy structFuse)

-- Calls fuse_parse_cmdline to parses the part of the commandline arguments that
-- we care about. fuse_parse_cmdline will modify the CFuseArgs struct passed in
-- to remove those arguments; the CFuseArgs struct containing remaining arguments
-- must be passed to fuse_mount/fuse_new.
--
-- The multithreaded runtime will be used regardless of the threading flag!
-- See the comment in fuse_session_exit for why.
-- TODO: refactor return type
fuseParseCommandLine :: Ptr CFuseArgs -> IO (Maybe (Maybe String, Bool, Bool))
fuseParseCommandLine pArgs =
    allocaBytes (#size struct fuse_cmdline_opts) $ \pOpts ->
        do retval <- fuse_parse_cmdline pArgs pOpts
           if retval == 0
               then do cMountPt <- (#peek struct fuse_cmdline_opts, mountpoint) pOpts
                       mountPt  <- if cMountPt /= nullPtr
                                     then do a <- peekCString cMountPt
                                             free cMountPt
                                             return $ Just a
                                     else return Nothing
                       singleThreaded <- (#peek struct fuse_cmdline_opts, singlethread) pOpts
                       foreground     <- (#peek struct fuse_cmdline_opts, foreground)   pOpts
                       return $ Just (mountPt, (singleThreaded :: CInt) == 0, (foreground :: CInt) == 1)
               else return Nothing

-- haskell version of daemon(2)
-- Mimic's daemon()s use of _exit() instead of exit(); we depend on this in fuseMainReal,
-- because otherwise we'll unmount the filesystem when the foreground process exits.
daemon f = forkProcess d >> exitImmediately ExitSuccess
  where d = catch (do createSession
                      changeWorkingDirectory "/"
                      -- need to open /dev/null twice because hDuplicateTo can't dup a
                      -- ReadWriteMode to a ReadMode handle
                      withFile "/dev/null" WriteMode (\devNullOut ->
                         do hDuplicateTo devNullOut stdout
                            hDuplicateTo devNullOut stderr)
                      withFile "/dev/null" ReadMode (`hDuplicateTo` stdin)
                      f
                      exitSuccess)
                  (const exitFailure)

-- Installs signal handlers for the duration of the main loop.
withSignalHandlers exitHandler f =
    do let sigHandler = Signals.CatchOnce exitHandler
       Signals.installHandler Signals.keyboardSignal sigHandler Nothing
       Signals.installHandler Signals.lostConnection sigHandler Nothing
       Signals.installHandler Signals.softwareTermination sigHandler Nothing
       Signals.installHandler Signals.openEndedPipe Signals.Ignore Nothing
       E.finally f
                 (do Signals.installHandler Signals.keyboardSignal Signals.Default Nothing
                     Signals.installHandler Signals.lostConnection Signals.Default Nothing
                     Signals.installHandler Signals.softwareTermination Signals.Default Nothing
                     Signals.installHandler Signals.openEndedPipe Signals.Default Nothing)


-- Mounts the filesystem, forks, and then starts fuse
fuseMainReal pData foreground ops handler pArgs mountPt =
    withCString mountPt (\cMountPt ->
      withStructFuseOpts pData pArgs ops handler (\pFuse -> do
          fuse_mount pFuse cMountPt
          pctx <- fuse_get_context
          E.finally
               (if foreground -- finally ready to fork
                 then changeWorkingDirectory "/" >> procMain pFuse
                 else daemon (procMain pFuse))
               (fuse_unmount pFuse)))

    -- here, we're finally inside the daemon process, we can run the main loop
    where procMain pFuse = do session <- fuse_get_session pFuse
                              -- calling fuse_session_exit to exit the main loop only
                              -- appears to work with the multithreaded fuse loop.
                              -- In the single-threaded case, FUSE depends on their
                              -- recv() call to finish with EINTR when signals arrive.
                              -- This doesn't happen with GHC's signal handling in place.
                              pctx <- fuse_get_context
                              withSignalHandlers (fuse_session_exit session) $
                                 do retVal <- fuse_loop_mt pFuse 0
                                    -- TODO: add opt clone_fd ^
                                    if retVal == 1 
                                      then exitSuccess
                                      else exitFailure
                                    return ()

-- | Main function of FUSE.
-- This is all that has to be called from the @main@ function. On top of
-- the 'FuseOperations' record with filesystem implementation, you must give
-- an exception handler converting Haskell exceptions to 'Errno'.
-- 
-- This function does the following:
--
--   * parses command line options (@-d@, @-s@ and @-h@) ;
--
--   * passes all options after @--@ to the fusermount program ;
--
--   * mounts the filesystem by calling @fusermount@ ;
--
--   * installs signal handlers for 'System.Posix.Signals.keyboardSignal',
--     'System.Posix.Signals.lostConnection',
--     'System.Posix.Signals.softwareTermination' and
--     'System.Posix.Signals.openEndedPipe' ;
--
--   * registers an exit handler to unmount the filesystem on program exit ;
--
--   * registers the operations ;
--
--   * calls FUSE event loop.

foreign import ccall "fuse.h fuse_main_real"
    fuse_main_real :: Int -> Ptr CString -> Ptr CFuseOperations -> CSize -> Ptr a -> IO Int

fuse_main argc pArgv pOps p = fuse_main_real argc pArgv pOps (#size struct fuse_operations) p

fM :: (Exception e, Storable a) => FuseOperations fh -> (e -> IO Errno) -> OptSpec a -> IO ()
fM ops handler opts = do
    prog <- getProgName
    args <- getArgs
    withFuseOps ops handler (\pOps ->
        withFuseArgs prog args (\pArgs -> do
           (res, ret) <- fuseOptParse pArgs opts
           p <- new res
           let argv = prog : args
               argc = length args
           withMany withCString argv (\pArgs ->
               withArray pArgs (\pArgv -> do
                   fuse_main_real argc pArgv pOps (#size struct fuse_operations) p
                   return ()))))

fuseMain :: Exception e => FuseOperations fh -> (e -> IO Errno) -> IO ()
fuseMain ops handler = do
    -- this used to be implemented using libfuse's fuse_main. Doing this will fork()
    -- from C behind the GHC runtime's back, which deadlocks in GHC 6.8.
    -- Instead, we reimplement fuse_main in Haskell using the forkProcess and the
    -- lower-level fuse_new/fuse_loop_mt API.
    fuseMainOpts (const ops) handler noOpt

fuseMainOpts :: (Exception e, Storable a)
             => (a -> FuseOperations fh)
             -> (e -> IO Errno)
             -> OptSpec a
             -> IO ()
fuseMainOpts ops handler opts = do
    prog <- getProgName
    args <- getArgs
    fuseRunOpts prog args ops handler opts

fuseRun :: String -> [String] -> Exception e => FuseOperations fh -> (e -> IO Errno) -> IO ()
fuseRun prog args ops handler = fuseRunOpts prog args (const ops) handler noOpt

fuseRunOpts :: String -> [String]
            -> (Exception e, Storable a) => (a -> FuseOperations fh) -> (e -> IO Errno)
            -> OptSpec a -> IO ()
fuseRunOpts prog args ops handler opts =
    catch
      (withFuseArgs prog args (\pArgs -> do
            (res, ret) <- fuseOptParse pArgs opts
            cmd <- fuseParseCommandLine pArgs
            case cmd of
                Nothing -> fail ""
                Just (Nothing, _, _) -> fail "Usage error: mount point required"
                Just (Just mountPt, _, foreground) -> do
                    fuseMainReal nullPtr foreground (ops res) handler pArgs mountPt
                    return ()))
      ((\errStr -> unless (null errStr) (putStrLn errStr) >> exitFailure) . ioeGetErrorString)



--fuse_opt_parse :: Ptr CFuseArgs -> Ptr () -> Ptr FuseOpt -> Ptr () -> IO CInt
-- | 'fuseOptParse' parses
-- 2nd and 3rd arguments must correspond to each other, because
-- offset in 'FuseOpt' is used to determine location of opt in Storable instance
-- TODO: think on better interface AND implement error handling
fuseOptParse :: Storable a => Ptr CFuseArgs -> OptSpec a -> IO (a, CInt)
fuseOptParse _     ([],res)    = return (res, 0)
fuseOptParse pArgs (opts,res)  = do
    withArray0 fuseOptEnd opts $ \pOpts -> do
        with res $ \pData -> do
            ret <- fuse_opt_parse pArgs pData pOpts nullPtr
            res <- peek pData
            return (res, ret)

-----------------------------------------------------------------------------
-- C land

---
-- exported C called from Haskell
---

foreign import ccall safe "fuse.h fuse_mount"
    fuse_mount :: Ptr CStructFuse -> CString -> IO CInt

foreign import ccall safe "fuse.h fuse_unmount"
    fuse_unmount :: Ptr CStructFuse -> IO ()

foreign import ccall safe "fuse.h fuse_get_session"
    fuse_get_session :: Ptr CStructFuse -> IO (Ptr CFuseSession)

foreign import ccall safe "fuse.h fuse_session_exit"
    fuse_session_exit :: Ptr CFuseSession -> IO ()

foreign import ccall safe "fuse.h fuse_set_signal_handlers"
    fuse_set_signal_handlers :: Ptr CFuseSession -> IO CInt

foreign import ccall safe "fuse.h fuse_remove_signal_handlers"
    fuse_remove_signal_handlers :: Ptr CFuseSession -> IO ()

foreign import ccall safe "fuse3/fuse_opt.h fuse_opt_parse"
    fuse_opt_parse :: Ptr CFuseArgs -> Ptr a -> Ptr FuseOpt -> Ptr () -> IO CInt

foreign import ccall safe "fuse3/fuse_lowlevel.h fuse_parse_cmdline"
    fuse_parse_cmdline :: Ptr CFuseArgs -> Ptr CFuseCmdlineOpts -> IO CInt

foreign import ccall safe "fuse.h fuse_new"
    fuse_new :: Ptr CFuseArgs -> Ptr CFuseOperations -> CSize -> Ptr () -> IO (Ptr CStructFuse)

foreign import ccall safe "fuse.h fuse_destroy"
    fuse_destroy :: Ptr CStructFuse -> IO ()

foreign import ccall safe "fuse.h fuse_opt_free_args"
    fuse_opt_free_args :: Ptr CFuseArgs -> IO ()

foreign import ccall safe "fuse.h fuse_loop_mt"
    fuse_loop_mt :: Ptr CStructFuse -> CInt -> IO CInt

foreign import ccall safe "fuse.h fuse_get_context"
    fuse_get_context :: IO (Ptr CFuseContext)
