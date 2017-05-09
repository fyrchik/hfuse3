module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import System.Environment ( getProgName, getArgs )

import System.Fuse

type HT = ()

data Options = Options { name :: CString, contents :: CString }
    deriving (Eq, Show)

instance Storable Options where
    sizeOf _ = 2 * sizeOf (nullPtr :: CString)
    alignment _  = alignment (undefined :: CString)
    peek ptr = do
        let p = castPtr ptr :: Ptr CString
        p1 <- peek p
        p2 <- peek (p `plusPtr` sizeOf (nullPtr :: CString))
        return $ Options p1 p2

    poke ptr (Options n c) = do
        let p = castPtr ptr :: Ptr CString
        poke p n
        poke (p `plusPtr` sizeOf (nullPtr :: CString)) c

main :: IO ()
main = fuseMainRealOpts
         [FuseOpt "--name" FuseOptString, FuseOpt "--contents" FuseOptString]
         helloFSOps
         defaultExceptionHandler

helloFSOps :: FuseOptResult -> FuseOperations HT
helloFSOps userData =
    defaultFuseOps { fuseGetFileStat = helloGetFileStat
                   , fuseOpen        = helloOpen
                   , fuseRead        = helloRead
                   , fuseOpenDirectory = helloOpenDirectory
                   , fuseReadDirectory = helloReadDirectory
                   , fuseGetFileSystemStats = helloGetFileSystemStats
                   , fuseInit = helloInit userData
                   , fuseDestroy = helloDestroy
                   }

helloInit :: FuseOptResult -> IO (Ptr ())
helloInit userData = do
    let a x = case x of { Just (Left s) -> newCString s ; _ -> return nullPtr }
    pn <- a $ lookup "--name" userData
    pc <- a $ lookup "--contents" userData
    p <- new $ Options pn pc
    return $ castPtr p

helloDestroy :: Ptr () -> IO ()
helloDestroy ptr = do
    unless (p == nullPtr) (peek p >>= freeOpts >> free p)
  where p = castPtr ptr :: Ptr Options
        freeOpts (Options n c) = do
            unless (n == nullPtr) (free n)
            unless (c == nullPtr) (free c)

helloString :: B.ByteString
helloString = B.pack "Hello World, HFuse!\n"

helloPath :: FilePath
helloPath = "hello"

getPrivateData :: FuseContext -> IO Options
getPrivateData ctx =
    let ptr = fuseCtxPrivateData ctx
     in if ptr == nullPtr then return $ Options nullPtr nullPtr
                          else peek (castPtr ptr :: Ptr Options)

_getContents :: Options -> IO B.ByteString
_getContents (Options _ p) =
    if p == nullPtr then return helloString
                    else B.pack <$> peekCString p

_getName :: Options -> IO String
_getName (Options p _) =
    if p == nullPtr then return helloPath
                    else peekCString p

helloNameFromContext :: FuseContext -> IO B.ByteString
helloNameFromContext ctx = do
    let pData = fuseCtxPrivateData ctx
    if pData == nullPtr
      then print "lul" >> return helloString
      else print "kek" >>
           peek (castPtr pData :: Ptr Options) >>= \o ->
           if contents o == nullPtr
             then return helloString
             else peekCString (contents o) >>= (return . B.pack)


dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

fileStat ctx = FileStat { statEntryType = RegularFile
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , groupReadMode
                                           , otherReadMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = fromIntegral $ B.length helloString
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }

helloGetFileStat :: FilePath -> IO (Either Errno FileStat)
helloGetFileStat "/" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx
helloGetFileStat path = do
    ctx <- getFuseContext
    helloPath <- getPrivateData ctx >>= _getName
    if path == '/':helloPath
      then return $ Right $ fileStat ctx
      else return $ Left eNOENT

helloOpenDirectory "/" = return eOK
helloOpenDirectory _   = return eNOENT

helloReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
helloReadDirectory "/" = do
    ctx <- getFuseContext
    n <- getPrivateData ctx >>= _getName
    return $ Right [(".",  dirStat ctx)
                   ,("..", dirStat ctx)
                   ,(n, fileStat ctx)
                   ]
helloReadDirectory _ = return (Left (eNOENT))

helloOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
helloOpen path mode flags = do
    helloPath <- getFuseContext >>= getPrivateData >>= _getName
    if path == '/':helloPath
      then case mode of
            ReadOnly -> return (Right ())
            _        -> return (Left eACCES)
      else return (Left eNOENT)


helloRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
helloRead path _ byteCount offset = do
    d <- getFuseContext >>= getPrivateData
    helloPath <- _getName d
    if path == '/':helloPath
      then do helloString <- _getContents d
              return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) helloString
      else return $ Left eNOENT

helloGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
helloGetFileSystemStats str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
