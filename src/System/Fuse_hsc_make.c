#include "/home/dzeta/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/template-hsc.h"
#line 75 "Fuse.hsc"
#if MIN_VERSION_base(4,6,0)
#line 77 "Fuse.hsc"
#else 
#line 79 "Fuse.hsc"
#endif 
#line 86 "Fuse.hsc"
#define FUSE_USE_VERSION 30
#line 88 "Fuse.hsc"
#if defined MACFUSE || defined __FreeBSD__
#line 89 "Fuse.hsc"
#include <sys/mount.h>
#line 90 "Fuse.hsc"
#else 
#line 91 "Fuse.hsc"
#include <sys/statfs.h>
#line 92 "Fuse.hsc"
#endif 
#line 94 "Fuse.hsc"
#include <dirent.h>
#line 95 "Fuse.hsc"
#include <fuse.h>
#line 96 "Fuse.hsc"
#include <fuse3/fuse_lowlevel.h>
#line 97 "Fuse.hsc"
#include <fcntl.h>
#line 900 "Fuse.hsc"
#if MIN_VERSION_base(4,6,0)
#line 902 "Fuse.hsc"
#else 
#line 903 "Fuse.hsc"
#endif 

int main (void)
{
#line 75 "Fuse.hsc"
#if MIN_VERSION_base(4,6,0)
#line 77 "Fuse.hsc"
#else 
#line 79 "Fuse.hsc"
#endif 
    hsc_printf ("{-# OPTIONS_GHC %s #-}\n", "-optc-DFUSE_USE_VERSION=30");
#line 88 "Fuse.hsc"
#if defined MACFUSE || defined __FreeBSD__
#line 90 "Fuse.hsc"
#else 
#line 92 "Fuse.hsc"
#endif 
#line 900 "Fuse.hsc"
#if MIN_VERSION_base(4,6,0)
#line 902 "Fuse.hsc"
#else 
#line 903 "Fuse.hsc"
#endif 
    hsc_line (1, "System/Fuse.hsc");
    hsc_fputs ("-----------------------------------------------------------------------------\n"
           "", hsc_stdout());
    hsc_line (2, "System/Fuse.hsc");
    hsc_fputs ("-- |\n"
           "-- Module      :  System.Fuse\n"
           "-- Copyright   :  (c) J\303\251r\303\251my Bobbio, Taru Karttunen\n"
           "-- License     :  BSD-style\n"
           "--\n"
           "-- Maintainer  :  Montez Fitzpatrick\n"
           "-- Stability   :  experimental\n"
           "-- Portability :  GHC 6.4-7.8.2\n"
           "--\n"
           "-- A binding for the FUSE (Filesystem in USErspace) library\n"
           "-- (<http://fuse.sourceforge.net/>), which allows filesystems to be implemented\n"
           "-- as userspace processes.\n"
           "--\n"
           "-- The binding tries to follow as much as possible current Haskell POSIX\n"
           "-- interface in \"System.Posix.Files\" and \"System.Posix.Directory\".\n"
           "--\n"
           "-- FUSE uses POSIX threads, so any Haskell application using this library must\n"
           "-- be linked against a threaded runtime system (eg. using the @threaded@ GHC\n"
           "-- option).\n"
           "--\n"
           "-----------------------------------------------------------------------------\n"
           "{-# LANGUAGE FlexibleContexts #-}\n"
           "{-# LANGUAGE Rank2Types #-}\n"
           "{-# LANGUAGE CPP #-}\n"
           "module System.Fuse\n"
           "    ( -- * Using FUSE\n"
           "\n"
           "      -- $intro\n"
           "\n"
           "      module Foreign.C.Error\n"
           "    , FuseOperations(..)\n"
           "    , defaultFuseOps\n"
           "    , fuseMain -- :: FuseOperations fh -> (Exception -> IO Errno) -> IO ()\n"
           "    , fuseRun -- :: String -> [String] -> FuseOperations fh -> (Exception -> IO Errno) -> IO ()\n"
           "    , defaultExceptionHandler -- :: Exception -> IO Errno\n"
           "      -- * Operations datatypes\n"
           "    , FileStat(..)\n"
           "    , EntryType(..)\n"
           "    , FileSystemStats(..)\n"
           "    , SyncType(..)\n"
           "      -- * FUSE Context\n"
           "    , getFuseContext -- :: IO FuseContext\n"
           "    , FuseContext(fuseCtxUserID, fuseCtxGroupID, fuseCtxProcessID)\n"
           "      -- * File modes\n"
           "    , entryTypeToFileMode -- :: EntryType -> FileMode\n"
           "    , fileModeToEntryType -- :: FileMode -> EntryType\n"
           "    , OpenMode(..)\n"
           "    , OpenFileFlags(..)\n"
           "    , intersectFileModes -- :: FileMode\n"
           "    , unionFileModes -- :: FileMode\n"
           "    ) where\n"
           "\n"
           "import Prelude hiding ( Read )\n"
           "\n"
           "import Control.Monad\n"
           "import Control.Exception as E(Exception, handle, finally, SomeException)\n"
           "import qualified Data.ByteString.Char8    as B\n"
           "import qualified Data.ByteString.Internal as B\n"
           "import qualified Data.ByteString.Unsafe   as B\n"
           "import Foreign\n"
           "import Foreign.C\n"
           "import Foreign.C.Error\n"
           "import Foreign.Marshal\n"
           "import System.Environment ( getProgName, getArgs )\n"
           "import System.IO ( hPutStrLn, stderr, withFile, stdin, stdout, IOMode(..) )\n"
           "import System.Posix.Types\n"
           "import System.Posix.Files ( accessModes, intersectFileModes, unionFileModes )\n"
           "import System.Posix.Directory(changeWorkingDirectory)\n"
           "import System.Posix.Process(forkProcess,createSession,exitImmediately)\n"
           "import System.Posix.IO ( OpenMode(..), OpenFileFlags(..) )\n"
           "import qualified System.Posix.Signals as Signals\n"
           "import GHC.IO.Handle(hDuplicateTo)\n"
           "import System.Exit\n"
           "", hsc_stdout());
#line 75 "Fuse.hsc"
#if MIN_VERSION_base(4,6,0)
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (76, "System/Fuse.hsc");
    hsc_fputs ("import System.IO.Error (catchIOError,ioeGetErrorString)\n"
           "", hsc_stdout());
#line 77 "Fuse.hsc"
#else 
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (78, "System/Fuse.hsc");
    hsc_fputs ("import System.IO.Error (catch,ioeGetErrorString)\n"
           "", hsc_stdout());
#line 79 "Fuse.hsc"
#endif 
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (80, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "-- TODO: FileMode -> Permissions\n"
           "-- TODO: Arguments !\n"
           "-- TODO: implement binding to fuse_invalidate\n"
           "-- TODO: bind fuse_*xattr\n"
           "\n"
           "", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (87, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "", hsc_stdout());
#line 88 "Fuse.hsc"
#if defined MACFUSE || defined __FreeBSD__
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (89, "System/Fuse.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (90, "System/Fuse.hsc");
    hsc_fputs ("", hsc_stdout());
#line 90 "Fuse.hsc"
#else 
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (91, "System/Fuse.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (92, "System/Fuse.hsc");
    hsc_fputs ("", hsc_stdout());
#line 92 "Fuse.hsc"
#endif 
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (93, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (95, "System/Fuse.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (96, "System/Fuse.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (97, "System/Fuse.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (98, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "{- $intro\n"
           "\'FuseOperations\' contains a field for each filesystem operations that can be called\n"
           "by FUSE. Think like if you were implementing a file system inside the Linux kernel.\n"
           "\n"
           "Each actions must return a POSIX error code, also called \'Errno\' reflecting\n"
           "operation result. For actions not using \'Either\', you should return \'eOK\' in case\n"
           "of success.\n"
           "\n"
           "Read and writes are done with Haskell \'ByteString\' type.\n"
           "\n"
           "-}\n"
           "\n"
           "{-  All operations should return the negated error value (-errno) on\n"
           "      error.\n"
           "-}\n"
           "\n"
           "{- | Used by \'fuseGetFileStat\'.  Corresponds to @struct stat@ from @stat.h@;\n"
           "     @st_dev@, @st_ino@ and @st_blksize@ are omitted, since (from the libfuse\n"
           "     documentation): \\\"the @st_dev@ and @st_blksize@ fields are ignored.  The\n"
           "     @st_ino@ field is ignored except if the use_ino mount option is given.\\\"\n"
           "\n"
           "     /TODO: at some point the inode field will probably be needed./\n"
           "-}\n"
           "data FileStat = FileStat { statEntryType :: EntryType\n"
           "                         , statFileMode :: FileMode\n"
           "                         , statLinkCount :: LinkCount\n"
           "                         , statFileOwner :: UserID\n"
           "                         , statFileGroup :: GroupID\n"
           "                         , statSpecialDeviceID :: DeviceID\n"
           "                         , statFileSize :: FileOffset\n"
           "                         , statBlocks :: Integer\n"
           "                         , statAccessTime :: EpochTime\n"
           "                         , statModificationTime :: EpochTime\n"
           "                         , statStatusChangeTime :: EpochTime\n"
           "                         }\n"
           "    deriving Show\n"
           "\n"
           "{- FIXME: I don\'t know how to determine the alignment of struct stat without\n"
           " - making unportable assumptions about the order of elements within it.  Hence,\n"
           " - FileStat is not an instance of Storable.  But it should be, rather than this\n"
           " - next function existing!\n"
           " -}\n"
           "\n"
           "fileStatToCStat :: FileStat -> Ptr CStat -> IO ()\n"
           "fileStatToCStat stat pStat = do\n"
           "    let mode = (entryTypeToFileMode (statEntryType stat)\n"
           "             `unionFileModes`\n"
           "               (statFileMode stat `intersectFileModes` accessModes))\n"
           "    let block_count = (fromIntegral (statBlocks stat) :: (", hsc_stdout());
#line 147 "Fuse.hsc"
    hsc_type (blkcnt_t);
    hsc_fputs ("))\n"
           "", hsc_stdout());
    hsc_line (148, "System/Fuse.hsc");
    hsc_fputs ("    (", hsc_stdout());
#line 148 "Fuse.hsc"
    hsc_poke (struct stat, st_mode);
    hsc_fputs (")   pStat mode\n"
           "", hsc_stdout());
    hsc_line (149, "System/Fuse.hsc");
    hsc_fputs ("    (", hsc_stdout());
#line 149 "Fuse.hsc"
    hsc_poke (struct stat, st_nlink);
    hsc_fputs (")  pStat (statLinkCount  stat)\n"
           "", hsc_stdout());
    hsc_line (150, "System/Fuse.hsc");
    hsc_fputs ("    (", hsc_stdout());
#line 150 "Fuse.hsc"
    hsc_poke (struct stat, st_uid);
    hsc_fputs (")    pStat (statFileOwner  stat)\n"
           "", hsc_stdout());
    hsc_line (151, "System/Fuse.hsc");
    hsc_fputs ("    (", hsc_stdout());
#line 151 "Fuse.hsc"
    hsc_poke (struct stat, st_gid);
    hsc_fputs (")    pStat (statFileGroup  stat)\n"
           "", hsc_stdout());
    hsc_line (152, "System/Fuse.hsc");
    hsc_fputs ("    (", hsc_stdout());
#line 152 "Fuse.hsc"
    hsc_poke (struct stat, st_rdev);
    hsc_fputs (")   pStat (statSpecialDeviceID stat)\n"
           "", hsc_stdout());
    hsc_line (153, "System/Fuse.hsc");
    hsc_fputs ("    (", hsc_stdout());
#line 153 "Fuse.hsc"
    hsc_poke (struct stat, st_size);
    hsc_fputs (")   pStat (statFileSize   stat)\n"
           "", hsc_stdout());
    hsc_line (154, "System/Fuse.hsc");
    hsc_fputs ("    (", hsc_stdout());
#line 154 "Fuse.hsc"
    hsc_poke (struct stat, st_blocks);
    hsc_fputs (") pStat block_count\n"
           "", hsc_stdout());
    hsc_line (155, "System/Fuse.hsc");
    hsc_fputs ("    (", hsc_stdout());
#line 155 "Fuse.hsc"
    hsc_poke (struct stat, st_atime);
    hsc_fputs (")  pStat (statAccessTime stat)\n"
           "", hsc_stdout());
    hsc_line (156, "System/Fuse.hsc");
    hsc_fputs ("    (", hsc_stdout());
#line 156 "Fuse.hsc"
    hsc_poke (struct stat, st_mtime);
    hsc_fputs (")  pStat (statModificationTime stat)\n"
           "", hsc_stdout());
    hsc_line (157, "System/Fuse.hsc");
    hsc_fputs ("    (", hsc_stdout());
#line 157 "Fuse.hsc"
    hsc_poke (struct stat, st_ctime);
    hsc_fputs (")  pStat (statStatusChangeTime stat)\n"
           "", hsc_stdout());
    hsc_line (158, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "\n"
           "-- | The Unix type of a node in the filesystem.\n"
           "data EntryType\n"
           "    = Unknown            -- ^ Unknown entry type\n"
           "    | NamedPipe\n"
           "    | CharacterSpecial\n"
           "    | Directory\n"
           "    | BlockSpecial\n"
           "    | RegularFile\n"
           "    | SymbolicLink\n"
           "    | Socket\n"
           "      deriving(Show)\n"
           "\n"
           "entryTypeToDT :: EntryType -> Int\n"
           "entryTypeToDT Unknown          = (", hsc_stdout());
#line 173 "Fuse.hsc"
    hsc_const (DT_UNKNOWN);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (174, "System/Fuse.hsc");
    hsc_fputs ("entryTypeToDT NamedPipe        = (", hsc_stdout());
#line 174 "Fuse.hsc"
    hsc_const (DT_FIFO);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (175, "System/Fuse.hsc");
    hsc_fputs ("entryTypeToDT CharacterSpecial = (", hsc_stdout());
#line 175 "Fuse.hsc"
    hsc_const (DT_CHR);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (176, "System/Fuse.hsc");
    hsc_fputs ("entryTypeToDT Directory        = (", hsc_stdout());
#line 176 "Fuse.hsc"
    hsc_const (DT_DIR);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (177, "System/Fuse.hsc");
    hsc_fputs ("entryTypeToDT BlockSpecial     = (", hsc_stdout());
#line 177 "Fuse.hsc"
    hsc_const (DT_BLK);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (178, "System/Fuse.hsc");
    hsc_fputs ("entryTypeToDT RegularFile      = (", hsc_stdout());
#line 178 "Fuse.hsc"
    hsc_const (DT_REG);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (179, "System/Fuse.hsc");
    hsc_fputs ("entryTypeToDT SymbolicLink     = (", hsc_stdout());
#line 179 "Fuse.hsc"
    hsc_const (DT_LNK);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (180, "System/Fuse.hsc");
    hsc_fputs ("entryTypeToDT Socket           = (", hsc_stdout());
#line 180 "Fuse.hsc"
    hsc_const (DT_SOCK);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (181, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "fileTypeModes :: FileMode\n"
           "fileTypeModes = (", hsc_stdout());
#line 183 "Fuse.hsc"
    hsc_const (S_IFMT);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (184, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "blockSpecialMode :: FileMode\n"
           "blockSpecialMode = (", hsc_stdout());
#line 186 "Fuse.hsc"
    hsc_const (S_IFBLK);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (187, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "characterSpecialMode :: FileMode\n"
           "characterSpecialMode = (", hsc_stdout());
#line 189 "Fuse.hsc"
    hsc_const (S_IFCHR);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (190, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "namedPipeMode :: FileMode\n"
           "namedPipeMode = (", hsc_stdout());
#line 192 "Fuse.hsc"
    hsc_const (S_IFIFO);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (193, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "regularFileMode :: FileMode\n"
           "regularFileMode = (", hsc_stdout());
#line 195 "Fuse.hsc"
    hsc_const (S_IFREG);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (196, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "directoryMode :: FileMode\n"
           "directoryMode = (", hsc_stdout());
#line 198 "Fuse.hsc"
    hsc_const (S_IFDIR);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (199, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "symbolicLinkMode :: FileMode\n"
           "symbolicLinkMode = (", hsc_stdout());
#line 201 "Fuse.hsc"
    hsc_const (S_IFLNK);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (202, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "socketMode :: FileMode\n"
           "socketMode = (", hsc_stdout());
#line 204 "Fuse.hsc"
    hsc_const (S_IFSOCK);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (205, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "-- | Converts an \'EntryType\' into the corresponding POSIX \'FileMode\'.\n"
           "entryTypeToFileMode :: EntryType -> FileMode\n"
           "entryTypeToFileMode Unknown          = 0\n"
           "entryTypeToFileMode NamedPipe        = namedPipeMode\n"
           "entryTypeToFileMode CharacterSpecial = characterSpecialMode\n"
           "entryTypeToFileMode Directory        = directoryMode\n"
           "entryTypeToFileMode BlockSpecial     = blockSpecialMode\n"
           "entryTypeToFileMode RegularFile      = regularFileMode\n"
           "entryTypeToFileMode SymbolicLink     = symbolicLinkMode\n"
           "entryTypeToFileMode Socket           = socketMode\n"
           "\n"
           "fileModeToEntryType :: FileMode -> EntryType\n"
           "fileModeToEntryType mode\n"
           "    | fileType == namedPipeMode        = NamedPipe\n"
           "    | fileType == characterSpecialMode = CharacterSpecial\n"
           "    | fileType == directoryMode        = Directory\n"
           "    | fileType == blockSpecialMode     = BlockSpecial\n"
           "    | fileType == regularFileMode      = RegularFile\n"
           "    | fileType == symbolicLinkMode     = SymbolicLink\n"
           "    | fileType == socketMode           = Socket\n"
           "    where fileType = mode .&. (", hsc_stdout());
#line 226 "Fuse.hsc"
    hsc_const (S_IFMT);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (227, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "{-\n"
           "    There is no create() operation, mknod() will be called for\n"
           "    creation of all non directory, non symlink nodes.\n"
           "-}\n"
           "\n"
           "-- | Type used by the \'fuseGetFileSystemStats\'.\n"
           "data FileSystemStats = FileSystemStats\n"
           "    { fsStatBlockSize :: Integer\n"
           "      -- ^ Optimal transfer block size. FUSE default is 512.\n"
           "    , fsStatBlockCount :: Integer\n"
           "      -- ^ Total data blocks in file system.\n"
           "    , fsStatBlocksFree :: Integer\n"
           "      -- ^ Free blocks in file system.\n"
           "    , fsStatBlocksAvailable :: Integer\n"
           "      -- ^ Free blocks available to non-superusers.\n"
           "    , fsStatFileCount :: Integer\n"
           "      -- ^ Total file nodes in file system.\n"
           "    , fsStatFilesFree :: Integer\n"
           "      -- ^ Free file nodes in file system.\n"
           "    , fsStatMaxNameLength :: Integer\n"
           "      -- ^ Maximum length of filenames. FUSE default is 255.\n"
           "    }\n"
           "\n"
           "\n"
           "-- | Used by \'fuseSynchronizeFile\' and \'fuseSynchronizeDirectory\'.\n"
           "data SyncType\n"
           "    = FullSync\n"
           "    -- ^ Synchronize all in-core parts of a file to disk: file content and\n"
           "    -- metadata.\n"
           "    | DataSync\n"
           "    -- ^ Synchronize only the file content.\n"
           "    deriving (Eq, Enum)\n"
           "\n"
           "\n"
           "-- | Returned by \'getFuseContext\'.\n"
           "data FuseContext = FuseContext\n"
           "    { fuseCtxUserID :: UserID\n"
           "    , fuseCtxGroupID :: GroupID\n"
           "    , fuseCtxProcessID :: ProcessID\n"
           "    }\n"
           "\n"
           "-- | Returns the context of the program doing the current FUSE call.\n"
           "getFuseContext :: IO FuseContext\n"
           "getFuseContext =\n"
           "    do pCtx <- fuse_get_context\n"
           "       userID <- (", hsc_stdout());
#line 273 "Fuse.hsc"
    hsc_peek (struct fuse_context, uid);
    hsc_fputs (") pCtx\n"
           "", hsc_stdout());
    hsc_line (274, "System/Fuse.hsc");
    hsc_fputs ("       groupID <- (", hsc_stdout());
#line 274 "Fuse.hsc"
    hsc_peek (struct fuse_context, gid);
    hsc_fputs (") pCtx\n"
           "", hsc_stdout());
    hsc_line (275, "System/Fuse.hsc");
    hsc_fputs ("       processID <- (", hsc_stdout());
#line 275 "Fuse.hsc"
    hsc_peek (struct fuse_context, pid);
    hsc_fputs (") pCtx\n"
           "", hsc_stdout());
    hsc_line (276, "System/Fuse.hsc");
    hsc_fputs ("       return $ FuseContext { fuseCtxUserID = userID\n"
           "                            , fuseCtxGroupID = groupID\n"
           "                            , fuseCtxProcessID = processID\n"
           "                            }\n"
           "\n"
           "-- | This record, given to \'fuseMain\', binds each required file system\n"
           "--   operations.\n"
           "--\n"
           "--   Each field is named against \'System.Posix\' names. Matching Linux system\n"
           "--   calls are also given as a reference.\n"
           "--\n"
           "--   @fh@ is the file handle type returned by \'fuseOpen\' and subsequently passed\n"
           "--   to all other file operations.\n"
           "data FuseOperations fh = FuseOperations\n"
           "      { -- | Implements \'System.Posix.Files.getSymbolicLinkStatus\' operation\n"
           "        --   (POSIX @lstat(2)@).\n"
           "        fuseGetFileStat :: FilePath -> IO (Either Errno FileStat),\n"
           "\n"
           "        -- | Implements \'System.Posix.Files.readSymbolicLink\' operation (POSIX\n"
           "        --   @readlink(2)@).  The returned \'FilePath\' might be truncated\n"
           "        --   depending on caller buffer size.\n"
           "        fuseReadSymbolicLink :: FilePath -> IO (Either Errno FilePath),\n"
           "\n"
           "        -- | Implements \'System.Posix.Files.createDevice\' (POSIX @mknod(2)@).\n"
           "        --   This function will also be called for regular file creation.\n"
           "        fuseCreateDevice :: FilePath -> EntryType -> FileMode\n"
           "                         -> DeviceID -> IO Errno,\n"
           "\n"
           "        -- | Implements \'System.Posix.Directory.createDirectory\' (POSIX\n"
           "        --   @mkdir(2)@).\n"
           "        fuseCreateDirectory :: FilePath -> FileMode -> IO Errno,\n"
           "\n"
           "        -- | Implements \'System.Posix.Files.removeLink\' (POSIX @unlink(2)@).\n"
           "        fuseRemoveLink :: FilePath -> IO Errno,\n"
           "\n"
           "        -- | Implements \'System.Posix.Directory.removeDirectory\' (POSIX\n"
           "        --   @rmdir(2)@).\n"
           "        fuseRemoveDirectory :: FilePath -> IO Errno,\n"
           "\n"
           "        -- | Implements \'System.Posix.Files.createSymbolicLink\' (POSIX\n"
           "        --   @symlink(2)@).\n"
           "        fuseCreateSymbolicLink :: FilePath -> FilePath -> IO Errno,\n"
           "\n"
           "        -- | Implements \'System.Posix.Files.rename\' (POSIX @rename(2)@).\n"
           "        fuseRename :: FilePath -> FilePath -> IO Errno,\n"
           "\n"
           "        -- | Implements \'System.Posix.Files.createLink\' (POSIX @link(2)@).\n"
           "        fuseCreateLink :: FilePath -> FilePath -> IO Errno,\n"
           "\n"
           "        -- | Implements \'System.Posix.Files.setFileMode\' (POSIX @chmod(2)@).\n"
           "        fuseSetFileMode :: FilePath -> FileMode -> IO Errno,\n"
           "\n"
           "        -- | Implements \'System.Posix.Files.setOwnerAndGroup\' (POSIX\n"
           "        --   @chown(2)@).\n"
           "        fuseSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO Errno,\n"
           "\n"
           "        -- | Implements \'System.Posix.Files.setFileSize\' (POSIX @truncate(2)@).\n"
           "        fuseSetFileSize :: FilePath -> FileOffset -> IO Errno,\n"
           "\n"
           "        -- | Implements \'System.Posix.Files.setFileTimes\'\n"
           "        --   (POSIX @utime(2)@).\n"
           "        fuseSetFileTimes :: FilePath -> EpochTime -> EpochTime -> IO Errno,\n"
           "\n"
           "        -- | Implements \'System.Posix.Files.openFd\' (POSIX @open(2)@).  On\n"
           "        --   success, returns \'Right\' of a filehandle-like value that will be\n"
           "        --   passed to future file operations; on failure, returns \'Left\' of the\n"
           "        --   appropriate \'Errno\'.\n"
           "        --\n"
           "        --   No creation, exclusive access or truncating flags will be passed.\n"
           "        --   This should check that the operation is permitted for the given\n"
           "        --   flags.\n"
           "        fuseOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh),\n"
           "\n"
           "        -- | Implements Unix98 @pread(2)@. It differs from\n"
           "        --   \'System.Posix.Files.fdRead\' by the explicit \'FileOffset\' argument.\n"
           "        --   The @fuse.h@ documentation stipulates that this \\\"should return\n"
           "        --   exactly the number of bytes requested except on EOF or error,\n"
           "        --   otherwise the rest of the data will be substituted with zeroes.\\\"\n"
           "        fuseRead :: FilePath -> fh -> ByteCount -> FileOffset\n"
           "                 -> IO (Either Errno B.ByteString),\n"
           "\n"
           "        -- | Implements Unix98 @pwrite(2)@. It differs\n"
           "        --   from \'System.Posix.Files.fdWrite\' by the explicit \'FileOffset\' argument.\n"
           "        fuseWrite :: FilePath -> fh -> B.ByteString -> FileOffset\n"
           "                  -> IO (Either Errno ByteCount),\n"
           "\n"
           "        -- | Implements @statfs(2)@.\n"
           "        fuseGetFileSystemStats :: String -> IO (Either Errno FileSystemStats),\n"
           "\n"
           "        -- | Called when @close(2)@ has been called on an open file.\n"
           "        --   Note: this does not mean that the file is released.  This function may be\n"
           "        --   called more than once for each @open(2)@.  The return value is passed on\n"
           "        --   to the @close(2)@ system call.\n"
           "        fuseFlush :: FilePath -> fh -> IO Errno,\n"
           "\n"
           "        -- | Called when an open file has all file descriptors closed and all\n"
           "        -- memory mappings unmapped.  For every @open@ call there will be\n"
           "        -- exactly one @release@ call with the same flags.  It is possible to\n"
           "        -- have a file opened more than once, in which case only the last\n"
           "        -- release will mean that no more reads or writes will happen on the\n"
           "        -- file.\n"
           "        fuseRelease :: FilePath -> fh -> IO (),\n"
           "\n"
           "        -- | Implements @fsync(2)@.\n"
           "        fuseSynchronizeFile :: FilePath -> SyncType -> IO Errno,\n"
           "\n"
           "        -- | Implements @opendir(3)@.  This method should check if the open\n"
           "        --   operation is permitted for this directory.\n"
           "        fuseOpenDirectory :: FilePath -> IO Errno,\n"
           "\n"
           "        -- | Implements @readdir(3)@.  The entire contents of the directory\n"
           "        --   should be returned as a list of tuples (corresponding to the first\n"
           "        --   mode of operation documented in @fuse.h@).\n"
           "        fuseReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)]),\n"
           "\n"
           "        -- | Implements @closedir(3)@.\n"
           "        fuseReleaseDirectory :: FilePath -> IO Errno,\n"
           "\n"
           "        -- | Synchronize the directory\'s contents; analogous to\n"
           "        --   \'fuseSynchronizeFile\'.\n"
           "        fuseSynchronizeDirectory :: FilePath -> SyncType -> IO Errno,\n"
           "\n"
           "        -- | Check file access permissions; this will be called for the\n"
           "        --   access() system call.  If the @default_permissions@ mount option\n"
           "        --   is given, this method is not called.  This method is also not\n"
           "        --   called under Linux kernel versions 2.4.x\n"
           "        fuseAccess :: FilePath -> Int -> IO Errno, -- FIXME present a nicer type to Haskell\n"
           "\n"
           "        -- | Initializes the filesystem.  This is called before all other\n"
           "        --   operations.\n"
           "        fuseInit :: IO (),\n"
           "\n"
           "        -- | Called on filesystem exit to allow cleanup.\n"
           "        fuseDestroy :: IO ()\n"
           "      }\n"
           "\n"
           "-- | Empty \\/ default versions of the FUSE operations.\n"
           "defaultFuseOps :: FuseOperations fh\n"
           "defaultFuseOps =\n"
           "    FuseOperations { fuseGetFileStat = \\_ -> return (Left eNOSYS)\n"
           "                   , fuseReadSymbolicLink = \\_ -> return (Left eNOSYS)\n"
           "                   , fuseCreateDevice = \\_ _ _ _ ->  return eNOSYS\n"
           "                   , fuseCreateDirectory = \\_ _ -> return eNOSYS\n"
           "                   , fuseRemoveLink = \\_ -> return eNOSYS\n"
           "                   , fuseRemoveDirectory = \\_ -> return eNOSYS\n"
           "                   , fuseCreateSymbolicLink = \\_ _ -> return eNOSYS\n"
           "                   , fuseRename = \\_ _ -> return eNOSYS\n"
           "                   , fuseCreateLink = \\_ _ -> return eNOSYS\n"
           "                   , fuseSetFileMode = \\_ _ -> return eNOSYS\n"
           "                   , fuseSetOwnerAndGroup = \\_ _ _ -> return eNOSYS\n"
           "                   , fuseSetFileSize = \\_ _ -> return eNOSYS\n"
           "                   , fuseSetFileTimes = \\_ _ _ -> return eNOSYS\n"
           "                   , fuseOpen =   \\_ _ _   -> return (Left eNOSYS)\n"
           "                   , fuseRead =   \\_ _ _ _ -> return (Left eNOSYS)\n"
           "                   , fuseWrite =  \\_ _ _ _ -> return (Left eNOSYS)\n"
           "                   , fuseGetFileSystemStats = \\_ -> return (Left eNOSYS)\n"
           "                   , fuseFlush = \\_ _ -> return eOK\n"
           "                   , fuseRelease = \\_ _ -> return ()\n"
           "                   , fuseSynchronizeFile = \\_ _ -> return eNOSYS\n"
           "                   , fuseOpenDirectory = \\_ -> return eNOSYS\n"
           "                   , fuseReadDirectory = \\_ -> return (Left eNOSYS)\n"
           "                   , fuseReleaseDirectory = \\_ -> return eNOSYS\n"
           "                   , fuseSynchronizeDirectory = \\_ _ -> return eNOSYS\n"
           "                   , fuseAccess = \\_ _ -> return eNOSYS\n"
           "                   , fuseInit = return ()\n"
           "                   , fuseDestroy = return ()\n"
           "                   }\n"
           "\n"
           "-- Allocates a fuse_args struct to hold the commandline arguments.\n"
           "withFuseArgs :: String -> [String] -> (Ptr CFuseArgs -> IO b) -> IO b\n"
           "withFuseArgs prog args f =\n"
           "    do let allArgs = (prog:args)\n"
           "           argc = length allArgs\n"
           "       withMany withCString allArgs (\\ cArgs ->\n"
           "           withArray cArgs $ (\\ pArgv ->\n"
           "               allocaBytes (", hsc_stdout());
#line 451 "Fuse.hsc"
    hsc_size (struct fuse_args);
    hsc_fputs (") (\\ fuseArgs ->\n"
           "", hsc_stdout());
    hsc_line (452, "System/Fuse.hsc");
    hsc_fputs ("                    do (", hsc_stdout());
#line 452 "Fuse.hsc"
    hsc_poke (struct fuse_args, argc);
    hsc_fputs (") fuseArgs argc\n"
           "", hsc_stdout());
    hsc_line (453, "System/Fuse.hsc");
    hsc_fputs ("                       (", hsc_stdout());
#line 453 "Fuse.hsc"
    hsc_poke (struct fuse_args, argv);
    hsc_fputs (") fuseArgs pArgv\n"
           "", hsc_stdout());
    hsc_line (454, "System/Fuse.hsc");
    hsc_fputs ("                       (", hsc_stdout());
#line 454 "Fuse.hsc"
    hsc_poke (struct fuse_args, allocated);
    hsc_fputs (") fuseArgs (0::CInt)\n"
           "", hsc_stdout());
    hsc_line (455, "System/Fuse.hsc");
    hsc_fputs ("                       finally (f fuseArgs)\n"
           "                               (fuse_opt_free_args fuseArgs))))\n"
           "\n"
           "withStructFuse :: forall e fh b. Exception e\n"
           "               => Ptr CFuseArgs\n"
           "               -> FuseOperations fh\n"
           "               -> (e -> IO Errno)\n"
           "               -> (Ptr CStructFuse -> IO b)\n"
           "               -> IO b\n"
           "withStructFuse pArgs ops handler f =\n"
           "    allocaBytes (", hsc_stdout());
#line 465 "Fuse.hsc"
    hsc_size (struct fuse_operations);
    hsc_fputs (") $ \\ pOps -> do\n"
           "", hsc_stdout());
    hsc_line (466, "System/Fuse.hsc");
    hsc_fputs ("      bzero pOps (", hsc_stdout());
#line 466 "Fuse.hsc"
    hsc_size (struct fuse_operations);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (467, "System/Fuse.hsc");
    hsc_fputs ("      mkGetAttr    wrapGetAttr    >>= (", hsc_stdout());
#line 467 "Fuse.hsc"
    hsc_poke (struct fuse_operations, getattr);
    hsc_fputs (")    pOps\n"
           "", hsc_stdout());
    hsc_line (468, "System/Fuse.hsc");
    hsc_fputs ("      mkReadLink   wrapReadLink   >>= (", hsc_stdout());
#line 468 "Fuse.hsc"
    hsc_poke (struct fuse_operations, readlink);
    hsc_fputs (")   pOps\n"
           "", hsc_stdout());
    hsc_line (469, "System/Fuse.hsc");
    hsc_fputs ("      mkMkNod      wrapMkNod      >>= (", hsc_stdout());
#line 469 "Fuse.hsc"
    hsc_poke (struct fuse_operations, mknod);
    hsc_fputs (")      pOps\n"
           "", hsc_stdout());
    hsc_line (470, "System/Fuse.hsc");
    hsc_fputs ("      mkMkDir      wrapMkDir      >>= (", hsc_stdout());
#line 470 "Fuse.hsc"
    hsc_poke (struct fuse_operations, mkdir);
    hsc_fputs (")      pOps\n"
           "", hsc_stdout());
    hsc_line (471, "System/Fuse.hsc");
    hsc_fputs ("      mkUnlink     wrapUnlink     >>= (", hsc_stdout());
#line 471 "Fuse.hsc"
    hsc_poke (struct fuse_operations, unlink);
    hsc_fputs (")     pOps\n"
           "", hsc_stdout());
    hsc_line (472, "System/Fuse.hsc");
    hsc_fputs ("      mkRmDir      wrapRmDir      >>= (", hsc_stdout());
#line 472 "Fuse.hsc"
    hsc_poke (struct fuse_operations, rmdir);
    hsc_fputs (")      pOps\n"
           "", hsc_stdout());
    hsc_line (473, "System/Fuse.hsc");
    hsc_fputs ("      mkSymLink    wrapSymLink    >>= (", hsc_stdout());
#line 473 "Fuse.hsc"
    hsc_poke (struct fuse_operations, symlink);
    hsc_fputs (")    pOps\n"
           "", hsc_stdout());
    hsc_line (474, "System/Fuse.hsc");
    hsc_fputs ("      mkRename     wrapRename     >>= (", hsc_stdout());
#line 474 "Fuse.hsc"
    hsc_poke (struct fuse_operations, rename);
    hsc_fputs (")     pOps\n"
           "", hsc_stdout());
    hsc_line (475, "System/Fuse.hsc");
    hsc_fputs ("      mkLink       wrapLink       >>= (", hsc_stdout());
#line 475 "Fuse.hsc"
    hsc_poke (struct fuse_operations, link);
    hsc_fputs (")       pOps\n"
           "", hsc_stdout());
    hsc_line (476, "System/Fuse.hsc");
    hsc_fputs ("      mkChMod      wrapChMod      >>= (", hsc_stdout());
#line 476 "Fuse.hsc"
    hsc_poke (struct fuse_operations, chmod);
    hsc_fputs (")      pOps\n"
           "", hsc_stdout());
    hsc_line (477, "System/Fuse.hsc");
    hsc_fputs ("      mkChOwn      wrapChOwn      >>= (", hsc_stdout());
#line 477 "Fuse.hsc"
    hsc_poke (struct fuse_operations, chown);
    hsc_fputs (")      pOps\n"
           "", hsc_stdout());
    hsc_line (478, "System/Fuse.hsc");
    hsc_fputs ("      mkTruncate   wrapTruncate   >>= (", hsc_stdout());
#line 478 "Fuse.hsc"
    hsc_poke (struct fuse_operations, truncate);
    hsc_fputs (")   pOps\n"
           "", hsc_stdout());
    hsc_line (479, "System/Fuse.hsc");
    hsc_fputs ("      mkOpen       wrapOpen       >>= (", hsc_stdout());
#line 479 "Fuse.hsc"
    hsc_poke (struct fuse_operations, open);
    hsc_fputs (")       pOps\n"
           "", hsc_stdout());
    hsc_line (480, "System/Fuse.hsc");
    hsc_fputs ("      mkRead       wrapRead       >>= (", hsc_stdout());
#line 480 "Fuse.hsc"
    hsc_poke (struct fuse_operations, read);
    hsc_fputs (")       pOps\n"
           "", hsc_stdout());
    hsc_line (481, "System/Fuse.hsc");
    hsc_fputs ("      mkWrite      wrapWrite      >>= (", hsc_stdout());
#line 481 "Fuse.hsc"
    hsc_poke (struct fuse_operations, write);
    hsc_fputs (")      pOps\n"
           "", hsc_stdout());
    hsc_line (482, "System/Fuse.hsc");
    hsc_fputs ("      mkStatFS     wrapStatFS     >>= (", hsc_stdout());
#line 482 "Fuse.hsc"
    hsc_poke (struct fuse_operations, statfs);
    hsc_fputs (")     pOps\n"
           "", hsc_stdout());
    hsc_line (483, "System/Fuse.hsc");
    hsc_fputs ("      mkFlush      wrapFlush      >>= (", hsc_stdout());
#line 483 "Fuse.hsc"
    hsc_poke (struct fuse_operations, flush);
    hsc_fputs (")      pOps\n"
           "", hsc_stdout());
    hsc_line (484, "System/Fuse.hsc");
    hsc_fputs ("      mkRelease    wrapRelease    >>= (", hsc_stdout());
#line 484 "Fuse.hsc"
    hsc_poke (struct fuse_operations, release);
    hsc_fputs (")    pOps\n"
           "", hsc_stdout());
    hsc_line (485, "System/Fuse.hsc");
    hsc_fputs ("      mkFSync      wrapFSync      >>= (", hsc_stdout());
#line 485 "Fuse.hsc"
    hsc_poke (struct fuse_operations, fsync);
    hsc_fputs (")      pOps\n"
           "", hsc_stdout());
    hsc_line (486, "System/Fuse.hsc");
    hsc_fputs ("      -- TODO: Implement these\n"
           "      (", hsc_stdout());
#line 487 "Fuse.hsc"
    hsc_poke (struct fuse_operations, setxattr);
    hsc_fputs (")    pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (488, "System/Fuse.hsc");
    hsc_fputs ("      (", hsc_stdout());
#line 488 "Fuse.hsc"
    hsc_poke (struct fuse_operations, getxattr);
    hsc_fputs (")    pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (489, "System/Fuse.hsc");
    hsc_fputs ("      (", hsc_stdout());
#line 489 "Fuse.hsc"
    hsc_poke (struct fuse_operations, listxattr);
    hsc_fputs (")   pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (490, "System/Fuse.hsc");
    hsc_fputs ("      (", hsc_stdout());
#line 490 "Fuse.hsc"
    hsc_poke (struct fuse_operations, removexattr);
    hsc_fputs (") pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (491, "System/Fuse.hsc");
    hsc_fputs ("      mkOpenDir    wrapOpenDir    >>= (", hsc_stdout());
#line 491 "Fuse.hsc"
    hsc_poke (struct fuse_operations, opendir);
    hsc_fputs (")    pOps\n"
           "", hsc_stdout());
    hsc_line (492, "System/Fuse.hsc");
    hsc_fputs ("      mkReadDir    wrapReadDir    >>= (", hsc_stdout());
#line 492 "Fuse.hsc"
    hsc_poke (struct fuse_operations, readdir);
    hsc_fputs (")    pOps\n"
           "", hsc_stdout());
    hsc_line (493, "System/Fuse.hsc");
    hsc_fputs ("      mkReleaseDir wrapReleaseDir >>= (", hsc_stdout());
#line 493 "Fuse.hsc"
    hsc_poke (struct fuse_operations, releasedir);
    hsc_fputs (") pOps\n"
           "", hsc_stdout());
    hsc_line (494, "System/Fuse.hsc");
    hsc_fputs ("      mkFSyncDir   wrapFSyncDir   >>= (", hsc_stdout());
#line 494 "Fuse.hsc"
    hsc_poke (struct fuse_operations, fsyncdir);
    hsc_fputs (")   pOps\n"
           "", hsc_stdout());
    hsc_line (495, "System/Fuse.hsc");
    hsc_fputs ("      mkAccess     wrapAccess     >>= (", hsc_stdout());
#line 495 "Fuse.hsc"
    hsc_poke (struct fuse_operations, access);
    hsc_fputs (")     pOps\n"
           "", hsc_stdout());
    hsc_line (496, "System/Fuse.hsc");
    hsc_fputs ("      mkInit       wrapInit       >>= (", hsc_stdout());
#line 496 "Fuse.hsc"
    hsc_poke (struct fuse_operations, init);
    hsc_fputs (")       pOps\n"
           "", hsc_stdout());
    hsc_line (497, "System/Fuse.hsc");
    hsc_fputs ("      mkDestroy    wrapDestroy    >>= (", hsc_stdout());
#line 497 "Fuse.hsc"
    hsc_poke (struct fuse_operations, destroy);
    hsc_fputs (")    pOps\n"
           "", hsc_stdout());
    hsc_line (498, "System/Fuse.hsc");
    hsc_fputs ("      -- TODO: Implement these\n"
           "      (", hsc_stdout());
#line 499 "Fuse.hsc"
    hsc_poke (struct fuse_operations, create);
    hsc_fputs (")    pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (500, "System/Fuse.hsc");
    hsc_fputs ("      (", hsc_stdout());
#line 500 "Fuse.hsc"
    hsc_poke (struct fuse_operations, lock);
    hsc_fputs (")      pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (501, "System/Fuse.hsc");
    hsc_fputs ("      (", hsc_stdout());
#line 501 "Fuse.hsc"
    hsc_poke (struct fuse_operations, utimens);
    hsc_fputs (")   pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (502, "System/Fuse.hsc");
    hsc_fputs ("      (", hsc_stdout());
#line 502 "Fuse.hsc"
    hsc_poke (struct fuse_operations, bmap);
    hsc_fputs (")      pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (503, "System/Fuse.hsc");
    hsc_fputs ("      (", hsc_stdout());
#line 503 "Fuse.hsc"
    hsc_poke (struct fuse_operations, ioctl);
    hsc_fputs (")     pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (504, "System/Fuse.hsc");
    hsc_fputs ("      (", hsc_stdout());
#line 504 "Fuse.hsc"
    hsc_poke (struct fuse_operations, poll);
    hsc_fputs (")      pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (505, "System/Fuse.hsc");
    hsc_fputs ("      (", hsc_stdout());
#line 505 "Fuse.hsc"
    hsc_poke (struct fuse_operations, write_buf);
    hsc_fputs (") pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (506, "System/Fuse.hsc");
    hsc_fputs ("      (", hsc_stdout());
#line 506 "Fuse.hsc"
    hsc_poke (struct fuse_operations, read_buf);
    hsc_fputs (")  pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (507, "System/Fuse.hsc");
    hsc_fputs ("      (", hsc_stdout());
#line 507 "Fuse.hsc"
    hsc_poke (struct fuse_operations, flock);
    hsc_fputs (")     pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (508, "System/Fuse.hsc");
    hsc_fputs ("      (", hsc_stdout());
#line 508 "Fuse.hsc"
    hsc_poke (struct fuse_operations, fallocate);
    hsc_fputs (") pOps nullPtr\n"
           "", hsc_stdout());
    hsc_line (509, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "      structFuse <- fuse_new pArgs pOps (", hsc_stdout());
#line 510 "Fuse.hsc"
    hsc_size (struct fuse_operations);
    hsc_fputs (") nullPtr\n"
           "", hsc_stdout());
    hsc_line (511, "System/Fuse.hsc");
    hsc_fputs ("      if structFuse == nullPtr\n"
           "        then fail \"\"\n"
           "        else E.finally (f structFuse)\n"
           "                       (fuse_destroy structFuse)\n"
           "    where fuseHandler :: e -> IO CInt\n"
           "          fuseHandler e = handler e >>= return . negate . unErrno\n"
           "          wrapGetAttr :: CGetAttr\n"
           "          wrapGetAttr pFilePath pStat = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 eitherFileStat <- (fuseGetFileStat ops) filePath\n"
           "                 case eitherFileStat of\n"
           "                   Left (Errno errno) -> return (- errno)\n"
           "                   Right stat         -> do fileStatToCStat stat pStat\n"
           "                                            return okErrno\n"
           "\n"
           "          wrapReadLink :: CReadLink\n"
           "          wrapReadLink pFilePath pBuf bufSize = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 return (- unErrno eNOSYS)\n"
           "                 eitherTarget <- (fuseReadSymbolicLink ops) filePath\n"
           "                 case eitherTarget of\n"
           "                   Left (Errno errno) -> return (- errno)\n"
           "                   Right target ->\n"
           "                   -- This will truncate target if it\'s longer than the buffer\n"
           "                   -- can hold, which is correct according to fuse.h\n"
           "                     do pokeCStringLen0 (pBuf, (fromIntegral bufSize)) target\n"
           "                        return okErrno\n"
           "\n"
           "          wrapMkNod :: CMkNod\n"
           "          wrapMkNod pFilePath mode dev = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 (Errno errno) <- (fuseCreateDevice ops) filePath\n"
           "                                      (fileModeToEntryType mode) mode dev\n"
           "                 return (- errno)\n"
           "          wrapMkDir :: CMkDir\n"
           "          wrapMkDir pFilePath mode = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 (Errno errno) <- (fuseCreateDirectory ops) filePath mode\n"
           "                 return (- errno)\n"
           "          wrapUnlink :: CUnlink\n"
           "          wrapUnlink pFilePath = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 (Errno errno) <- (fuseRemoveLink ops) filePath\n"
           "                 return (- errno)\n"
           "          wrapRmDir :: CRmDir\n"
           "          wrapRmDir pFilePath = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 (Errno errno) <- (fuseRemoveDirectory ops) filePath\n"
           "                 return (- errno)\n"
           "          wrapSymLink :: CSymLink\n"
           "          wrapSymLink pSource pDestination = handle fuseHandler $\n"
           "              do source <- peekCString pSource\n"
           "                 destination <- peekCString pDestination\n"
           "                 (Errno errno) <- (fuseCreateSymbolicLink ops) source destination\n"
           "                 return (- errno)\n"
           "          wrapRename :: CRename\n"
           "          wrapRename pOld pNew _ = handle fuseHandler $\n"
           "              do old <- peekCString pOld\n"
           "                 new <- peekCString pNew\n"
           "                 (Errno errno) <- (fuseRename ops) old new\n"
           "                 return (- errno)\n"
           "          wrapLink :: CLink\n"
           "          wrapLink pSource pDestination = handle fuseHandler $\n"
           "              do source <- peekCString pSource\n"
           "                 destination <- peekCString pDestination\n"
           "                 (Errno errno) <- (fuseCreateLink ops) source destination\n"
           "                 return (- errno)\n"
           "          wrapChMod :: CChMod\n"
           "          wrapChMod pFilePath mode = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 (Errno errno) <- (fuseSetFileMode ops) filePath mode\n"
           "                 return (- errno)\n"
           "          wrapChOwn :: CChOwn\n"
           "          wrapChOwn pFilePath uid gid = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 (Errno errno) <- (fuseSetOwnerAndGroup ops) filePath uid gid\n"
           "                 return (- errno)\n"
           "          wrapTruncate :: CTruncate\n"
           "          wrapTruncate pFilePath off = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 (Errno errno) <- (fuseSetFileSize ops) filePath off\n"
           "                 return (- errno)\n"
           "          wrapOpen :: COpen\n"
           "          wrapOpen pFilePath pFuseFileInfo = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 (flags :: CInt) <- (", hsc_stdout());
#line 596 "Fuse.hsc"
    hsc_peek (struct fuse_file_info, flags);
    hsc_fputs (") pFuseFileInfo\n"
           "", hsc_stdout());
    hsc_line (597, "System/Fuse.hsc");
    hsc_fputs ("                 let append    = (", hsc_stdout());
#line 597 "Fuse.hsc"
    hsc_const (O_APPEND);
    hsc_fputs (")   .&. flags == (", hsc_stdout());
#line 597 "Fuse.hsc"
    hsc_const (O_APPEND);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (598, "System/Fuse.hsc");
    hsc_fputs ("                     noctty    = (", hsc_stdout());
#line 598 "Fuse.hsc"
    hsc_const (O_NOCTTY);
    hsc_fputs (")   .&. flags == (", hsc_stdout());
#line 598 "Fuse.hsc"
    hsc_const (O_NOCTTY);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (599, "System/Fuse.hsc");
    hsc_fputs ("                     nonBlock  = (", hsc_stdout());
#line 599 "Fuse.hsc"
    hsc_const (O_NONBLOCK);
    hsc_fputs (") .&. flags == (", hsc_stdout());
#line 599 "Fuse.hsc"
    hsc_const (O_NONBLOCK);
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (600, "System/Fuse.hsc");
    hsc_fputs ("                     how | (", hsc_stdout());
#line 600 "Fuse.hsc"
    hsc_const (O_RDWR);
    hsc_fputs (")   .&. flags == (", hsc_stdout());
#line 600 "Fuse.hsc"
    hsc_const (O_RDWR);
    hsc_fputs (") = ReadWrite\n"
           "", hsc_stdout());
    hsc_line (601, "System/Fuse.hsc");
    hsc_fputs ("                         | (", hsc_stdout());
#line 601 "Fuse.hsc"
    hsc_const (O_WRONLY);
    hsc_fputs (") .&. flags == (", hsc_stdout());
#line 601 "Fuse.hsc"
    hsc_const (O_WRONLY);
    hsc_fputs (") = WriteOnly\n"
           "", hsc_stdout());
    hsc_line (602, "System/Fuse.hsc");
    hsc_fputs ("                         | otherwise = ReadOnly\n"
           "                     openFileFlags = OpenFileFlags { append = append\n"
           "                                                   , exclusive = False\n"
           "                                                   , noctty = noctty\n"
           "                                                   , nonBlock = nonBlock\n"
           "                                                   , trunc = False\n"
           "                                                   }\n"
           "                 result <- (fuseOpen ops) filePath how openFileFlags\n"
           "                 case result of\n"
           "                    Left (Errno errno) -> return (- errno)\n"
           "                    Right cval         -> do\n"
           "                        sptr <- newStablePtr cval\n"
           "                        (", hsc_stdout());
#line 614 "Fuse.hsc"
    hsc_poke (struct fuse_file_info, fh);
    hsc_fputs (") pFuseFileInfo $ castStablePtrToPtr sptr\n"
           "", hsc_stdout());
    hsc_line (615, "System/Fuse.hsc");
    hsc_fputs ("                        return okErrno\n"
           "\n"
           "          wrapRead :: CRead\n"
           "          wrapRead pFilePath pBuf bufSiz off pFuseFileInfo = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 cVal <- getFH pFuseFileInfo\n"
           "                 eitherRead <- (fuseRead ops) filePath cVal bufSiz off\n"
           "                 case eitherRead of\n"
           "                   Left (Errno errno) -> return (- errno)\n"
           "                   Right bytes  ->\n"
           "                     do let len = fromIntegral bufSiz `min` B.length bytes\n"
           "                        bsToBuf pBuf bytes len\n"
           "                        return (fromIntegral len)\n"
           "          wrapWrite :: CWrite\n"
           "          wrapWrite pFilePath pBuf bufSiz off pFuseFileInfo = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 cVal <- getFH pFuseFileInfo\n"
           "                 buf  <- B.packCStringLen (pBuf, fromIntegral bufSiz)\n"
           "                 eitherBytes <- (fuseWrite ops) filePath cVal buf off\n"
           "                 case eitherBytes of\n"
           "                   Left  (Errno errno) -> return (- errno)\n"
           "                   Right bytes         -> return (fromIntegral bytes)\n"
           "          wrapStatFS :: CStatFS\n"
           "          wrapStatFS pStr pStatVFS = handle fuseHandler $\n"
           "            do str <- peekCString pStr\n"
           "               eitherStatVFS <- (fuseGetFileSystemStats ops) str\n"
           "               case eitherStatVFS of\n"
           "                 Left (Errno errno) -> return (- errno)\n"
           "                 Right stat         ->\n"
           "                   do (", hsc_stdout());
#line 644 "Fuse.hsc"
    hsc_poke (struct statvfs, f_bsize);
    hsc_fputs (") pStatVFS\n"
           "", hsc_stdout());
    hsc_line (645, "System/Fuse.hsc");
    hsc_fputs ("                          (fromIntegral (fsStatBlockSize stat) :: (", hsc_stdout());
#line 645 "Fuse.hsc"
    hsc_type (long);
    hsc_fputs ("))\n"
           "", hsc_stdout());
    hsc_line (646, "System/Fuse.hsc");
    hsc_fputs ("                      (", hsc_stdout());
#line 646 "Fuse.hsc"
    hsc_poke (struct statvfs, f_blocks);
    hsc_fputs (") pStatVFS\n"
           "", hsc_stdout());
    hsc_line (647, "System/Fuse.hsc");
    hsc_fputs ("                          (fromIntegral (fsStatBlockCount stat) :: (", hsc_stdout());
#line 647 "Fuse.hsc"
    hsc_type (fsblkcnt_t);
    hsc_fputs ("))\n"
           "", hsc_stdout());
    hsc_line (648, "System/Fuse.hsc");
    hsc_fputs ("                      (", hsc_stdout());
#line 648 "Fuse.hsc"
    hsc_poke (struct statvfs, f_bfree);
    hsc_fputs (") pStatVFS\n"
           "", hsc_stdout());
    hsc_line (649, "System/Fuse.hsc");
    hsc_fputs ("                          (fromIntegral (fsStatBlocksFree stat) :: (", hsc_stdout());
#line 649 "Fuse.hsc"
    hsc_type (fsblkcnt_t);
    hsc_fputs ("))\n"
           "", hsc_stdout());
    hsc_line (650, "System/Fuse.hsc");
    hsc_fputs ("                      (", hsc_stdout());
#line 650 "Fuse.hsc"
    hsc_poke (struct statvfs, f_bavail);
    hsc_fputs (") pStatVFS\n"
           "", hsc_stdout());
    hsc_line (651, "System/Fuse.hsc");
    hsc_fputs ("                          (fromIntegral (fsStatBlocksAvailable stat) :: (", hsc_stdout());
#line 651 "Fuse.hsc"
    hsc_type (fsblkcnt_t);
    hsc_fputs ("))\n"
           "", hsc_stdout());
    hsc_line (652, "System/Fuse.hsc");
    hsc_fputs ("                      (", hsc_stdout());
#line 652 "Fuse.hsc"
    hsc_poke (struct statvfs, f_files);
    hsc_fputs (") pStatVFS\n"
           "", hsc_stdout());
    hsc_line (653, "System/Fuse.hsc");
    hsc_fputs ("                           (fromIntegral (fsStatFileCount stat) :: (", hsc_stdout());
#line 653 "Fuse.hsc"
    hsc_type (fsfilcnt_t);
    hsc_fputs ("))\n"
           "", hsc_stdout());
    hsc_line (654, "System/Fuse.hsc");
    hsc_fputs ("                      (", hsc_stdout());
#line 654 "Fuse.hsc"
    hsc_poke (struct statvfs, f_ffree);
    hsc_fputs (") pStatVFS\n"
           "", hsc_stdout());
    hsc_line (655, "System/Fuse.hsc");
    hsc_fputs ("                          (fromIntegral (fsStatFilesFree stat) :: (", hsc_stdout());
#line 655 "Fuse.hsc"
    hsc_type (fsfilcnt_t);
    hsc_fputs ("))\n"
           "", hsc_stdout());
    hsc_line (656, "System/Fuse.hsc");
    hsc_fputs ("                      (", hsc_stdout());
#line 656 "Fuse.hsc"
    hsc_poke (struct statvfs, f_namemax);
    hsc_fputs (") pStatVFS\n"
           "", hsc_stdout());
    hsc_line (657, "System/Fuse.hsc");
    hsc_fputs ("                          (fromIntegral (fsStatMaxNameLength stat) :: (", hsc_stdout());
#line 657 "Fuse.hsc"
    hsc_type (long);
    hsc_fputs ("))\n"
           "", hsc_stdout());
    hsc_line (658, "System/Fuse.hsc");
    hsc_fputs ("                      return 0\n"
           "          wrapFlush :: CFlush\n"
           "          wrapFlush pFilePath pFuseFileInfo = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 cVal     <- getFH pFuseFileInfo\n"
           "                 (Errno errno) <- (fuseFlush ops) filePath cVal\n"
           "                 return (- errno)\n"
           "          wrapRelease :: CRelease\n"
           "          wrapRelease pFilePath pFuseFileInfo = E.finally (handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 cVal     <- getFH pFuseFileInfo\n"
           "                 -- TODO: deal with these flags\?\n"
           "--                 flags <- (#peek struct fuse_file_info, flags) pFuseFileInfo\n"
           "                 (fuseRelease ops) filePath cVal\n"
           "                 return 0) (delFH pFuseFileInfo)\n"
           "          wrapFSync :: CFSync\n"
           "          wrapFSync pFilePath isFullSync pFuseFileInfo = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 (Errno errno) <- (fuseSynchronizeFile ops)\n"
           "                                      filePath (toEnum isFullSync)\n"
           "                 return (- errno)\n"
           "          wrapOpenDir :: COpenDir\n"
           "          wrapOpenDir pFilePath pFuseFileInfo = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 -- XXX: Should we pass flags from pFuseFileInfo\?\n"
           "                 (Errno errno) <- (fuseOpenDirectory ops) filePath\n"
           "                 return (- errno)\n"
           "\n"
           "          wrapReadDir :: CReadDir\n"
           "          wrapReadDir pFilePath pBuf pFillDir off pFuseFileInfo _ =\n"
           "            handle fuseHandler $ do\n"
           "              filePath <- peekCString pFilePath\n"
           "              let fillDir = mkFillDir pFillDir\n"
           "              let filler :: (FilePath, FileStat) -> IO ()\n"
           "                  filler (fileName, fileStat) =\n"
           "                    withCString fileName $ \\ pFileName ->\n"
           "                      allocaBytes (", hsc_stdout());
#line 694 "Fuse.hsc"
    hsc_size (struct stat);
    hsc_fputs (") $ \\ pFileStat ->\n"
           "", hsc_stdout());
    hsc_line (695, "System/Fuse.hsc");
    hsc_fputs ("                        do fileStatToCStat fileStat pFileStat\n"
           "                           fillDir pBuf pFileName pFileStat 0 0\n"
           "                           -- Ignoring return value of pFillDir, namely 1 if\n"
           "                           -- pBuff is full.\n"
           "                           return ()\n"
           "              eitherContents <- (fuseReadDirectory ops) filePath -- XXX fileinfo\n"
           "              case eitherContents of\n"
           "                Left (Errno errno) -> return (- errno)\n"
           "                Right contents     -> mapM filler contents >> return okErrno\n"
           "\n"
           "          wrapReleaseDir :: CReleaseDir\n"
           "          wrapReleaseDir pFilePath pFuseFileInfo = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 (Errno errno) <- (fuseReleaseDirectory ops) filePath\n"
           "                 return (- errno)\n"
           "          wrapFSyncDir :: CFSyncDir\n"
           "          wrapFSyncDir pFilePath isFullSync pFuseFileInfo = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 (Errno errno) <- (fuseSynchronizeDirectory ops)\n"
           "                                      filePath (toEnum isFullSync)\n"
           "                 return (- errno)\n"
           "          wrapAccess :: CAccess\n"
           "          wrapAccess pFilePath at = handle fuseHandler $\n"
           "              do filePath <- peekCString pFilePath\n"
           "                 (Errno errno) <- (fuseAccess ops) filePath (fromIntegral at)\n"
           "                 return (- errno)\n"
           "          wrapInit :: CInit\n"
           "          wrapInit pFuseConnInfo =\n"
           "            handle (\\e -> defaultExceptionHandler e >> return nullPtr) $\n"
           "              do fuseInit ops\n"
           "                 return nullPtr\n"
           "          wrapDestroy :: CDestroy\n"
           "          wrapDestroy _ = handle (\\e -> defaultExceptionHandler e >> return ()) $\n"
           "              do fuseDestroy ops\n"
           "\n"
           "-- | Default exception handler.\n"
           "-- Print the exception on error output and returns \'eFAULT\'.\n"
           "defaultExceptionHandler :: (SomeException -> IO Errno)\n"
           "defaultExceptionHandler e = hPutStrLn stderr (show e) >> return eFAULT\n"
           "\n"
           "-- Calls fuse_parse_cmdline to parses the part of the commandline arguments that\n"
           "-- we care about. fuse_parse_cmdline will modify the CFuseArgs struct passed in\n"
           "-- to remove those arguments; the CFuseArgs struct containing remaining arguments\n"
           "-- must be passed to fuse_mount/fuse_new.\n"
           "--\n"
           "-- The multithreaded runtime will be used regardless of the threading flag!\n"
           "-- See the comment in fuse_session_exit for why.\n"
           "-- TODO: refactor return type\n"
           "fuseParseCommandLine :: Ptr CFuseArgs -> IO (Maybe (Maybe String, Bool, Bool))\n"
           "fuseParseCommandLine pArgs = do\n"
           "    allocaBytes (", hsc_stdout());
#line 745 "Fuse.hsc"
    hsc_size (struct fuse_cmdline_opts);
    hsc_fputs (") $ \\pOpts ->\n"
           "", hsc_stdout());
    hsc_line (746, "System/Fuse.hsc");
    hsc_fputs ("        do retval <- fuse_parse_cmdline pArgs pOpts\n"
           "           print retval\n"
           "           if retval == 0\n"
           "               then do cMountPt <- (", hsc_stdout());
#line 749 "Fuse.hsc"
    hsc_peek (struct fuse_cmdline_opts, mountpoint);
    hsc_fputs (") pOpts\n"
           "", hsc_stdout());
    hsc_line (750, "System/Fuse.hsc");
    hsc_fputs ("                       mountPt  <- if cMountPt /= nullPtr\n"
           "                                     then do a <- peekCString cMountPt\n"
           "                                             free cMountPt\n"
           "                                             return $ Just a\n"
           "                                     else return $ Nothing\n"
           "                       singleThreaded <- (", hsc_stdout());
#line 755 "Fuse.hsc"
    hsc_peek (struct fuse_cmdline_opts, singlethread);
    hsc_fputs (") pOpts\n"
           "", hsc_stdout());
    hsc_line (756, "System/Fuse.hsc");
    hsc_fputs ("                       foreground     <- (", hsc_stdout());
#line 756 "Fuse.hsc"
    hsc_peek (struct fuse_cmdline_opts, foreground);
    hsc_fputs (")   pOpts\n"
           "", hsc_stdout());
    hsc_line (757, "System/Fuse.hsc");
    hsc_fputs ("                       show_help      <- (", hsc_stdout());
#line 757 "Fuse.hsc"
    hsc_peek (struct fuse_cmdline_opts, show_help);
    hsc_fputs (")    pOpts\n"
           "", hsc_stdout());
    hsc_line (758, "System/Fuse.hsc");
    hsc_fputs ("                       print mountPt\n"
           "                       print (singleThreaded :: Int)\n"
           "                       print (foreground :: Int)\n"
           "                       print (show_help :: Int)\n"
           "                       return $ Just (mountPt, (singleThreaded :: Int) == 0, (foreground :: Int) == 1)\n"
           "               else return Nothing\n"
           "\n"
           "-- haskell version of daemon(2)\n"
           "-- Mimic\'s daemon()s use of _exit() instead of exit(); we depend on this in fuseMainReal,\n"
           "-- because otherwise we\'ll unmount the filesystem when the foreground process exits.\n"
           "daemon f = forkProcess d >> exitImmediately ExitSuccess\n"
           "  where d = catch (do createSession\n"
           "                      changeWorkingDirectory \"/\"\n"
           "                      -- need to open /dev/null twice because hDuplicateTo can\'t dup a\n"
           "                      -- ReadWriteMode to a ReadMode handle\n"
           "                      withFile \"/dev/null\" WriteMode (\\devNullOut ->\n"
           "                         do hDuplicateTo devNullOut stdout\n"
           "                            hDuplicateTo devNullOut stderr)\n"
           "                      withFile \"/dev/null\" ReadMode (\\devNullIn -> hDuplicateTo devNullIn stdin)\n"
           "                      f\n"
           "                      exitWith ExitSuccess)\n"
           "                  (const exitFailure)\n"
           "\n"
           "-- Installs signal handlers for the duration of the main loop.\n"
           "withSignalHandlers exitHandler f =\n"
           "    do let sigHandler = Signals.CatchOnce exitHandler\n"
           "       Signals.installHandler Signals.keyboardSignal sigHandler Nothing\n"
           "       Signals.installHandler Signals.lostConnection sigHandler Nothing\n"
           "       Signals.installHandler Signals.softwareTermination sigHandler Nothing\n"
           "       Signals.installHandler Signals.openEndedPipe Signals.Ignore Nothing\n"
           "       E.finally f\n"
           "                 (do Signals.installHandler Signals.keyboardSignal Signals.Default Nothing\n"
           "                     Signals.installHandler Signals.lostConnection Signals.Default Nothing\n"
           "                     Signals.installHandler Signals.softwareTermination Signals.Default Nothing\n"
           "                     Signals.installHandler Signals.openEndedPipe Signals.Default Nothing)\n"
           "\n"
           "\n"
           "-- Mounts the filesystem, forks, and then starts fuse\n"
           "fuseMainReal foreground ops handler pArgs mountPt = do\n"
           "    print foreground\n"
           "    withCString mountPt (\\cMountPt ->\n"
           "      do withStructFuse pArgs ops handler (\\pFuse -> do\n"
           "          fuse_mount pFuse cMountPt\n"
           "          E.finally\n"
           "               (if foreground -- finally ready to fork\n"
           "                 then changeWorkingDirectory \"/\" >> (procMain pFuse)\n"
           "                 else daemon (procMain pFuse))\n"
           "               (fuse_unmount pFuse)))\n"
           "\n"
           "    -- here, we\'re finally inside the daemon process, we can run the main loop\n"
           "    where procMain pFuse = do session <- fuse_get_session pFuse\n"
           "                              -- calling fuse_session_exit to exit the main loop only\n"
           "                              -- appears to work with the multithreaded fuse loop.\n"
           "                              -- In the single-threaded case, FUSE depends on their\n"
           "                              -- recv() call to finish with EINTR when signals arrive.\n"
           "                              -- This doesn\'t happen with GHC\'s signal handling in place.\n"
           "                              withSignalHandlers (fuse_session_exit session) $\n"
           "                                 do retVal <- fuse_loop_mt pFuse 0\n"
           "                                    -- TODO: add opt clone_fd ^\n"
           "                                    if retVal == 1 \n"
           "                                      then exitWith ExitSuccess\n"
           "                                      else exitFailure\n"
           "                                    return ()\n"
           "\n"
           "-- | Main function of FUSE.\n"
           "-- This is all that has to be called from the @main@ function. On top of\n"
           "-- the \'FuseOperations\' record with filesystem implementation, you must give\n"
           "-- an exception handler converting Haskell exceptions to \'Errno\'.\n"
           "-- \n"
           "-- This function does the following:\n"
           "--\n"
           "--   * parses command line options (@-d@, @-s@ and @-h@) ;\n"
           "--\n"
           "--   * passes all options after @--@ to the fusermount program ;\n"
           "--\n"
           "--   * mounts the filesystem by calling @fusermount@ ;\n"
           "--\n"
           "--   * installs signal handlers for \'System.Posix.Signals.keyboardSignal\',\n"
           "--     \'System.Posix.Signals.lostConnection\',\n"
           "--     \'System.Posix.Signals.softwareTermination\' and\n"
           "--     \'System.Posix.Signals.openEndedPipe\' ;\n"
           "--\n"
           "--   * registers an exit handler to unmount the filesystem on program exit ;\n"
           "--\n"
           "--   * registers the operations ;\n"
           "--\n"
           "--   * calls FUSE event loop.\n"
           "\n"
           "foreign import ccall \"fuse.h fuse_main_real\"\n"
           "    fuse_main_real :: CInt -> Ptr CString -> Ptr CFuseOperations -> CSize -> Ptr () -> IO CInt\n"
           "\n"
           "fM :: exception e => FuseOperations fh -> (e -> IO Errno) -> IO Int\n"
           "fM ops handler = do\n"
           "    prog <- getProgName\n"
           "    args <- getArgs\n"
           "    withStructFuse prog args (\\pFuse ->\n"
           "       pOps <- (", hsc_stdout());
#line 854 "Fuse.hsc"
    hsc_ptr (struct fuse, fuse_fs);
    hsc_fputs (") pFuse\n"
           "", hsc_stdout());
    hsc_line (855, "System/Fuse.hsc");
    hsc_fputs ("       withFuseArgs prog args (\\pArgs ->\n"
           "           let argv = prog:args\n"
           "               argc = length args\n"
           "           withMany withCString argv (\\pArgs ->\n"
           "               withArray pArgs (\\pArgv ->\n"
           "                   fuse_main_real argc pArgv pOps (", hsc_stdout());
#line 860 "Fuse.hsc"
    hsc_size (struct fuse_operations);
    hsc_fputs (") nullPtr\n"
           "", hsc_stdout());
    hsc_line (861, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "fuseMain :: Exception e => FuseOperations fh -> (e -> IO Errno) -> IO ()\n"
           "fuseMain ops handler = do\n"
           "    -- this used to be implemented using libfuse\'s fuse_main. Doing this will fork()\n"
           "    -- from C behind the GHC runtime\'s back, which deadlocks in GHC 6.8.\n"
           "    -- Instead, we reimplement fuse_main in Haskell using the forkProcess and the\n"
           "    -- lower-level fuse_new/fuse_loop_mt API.\n"
           "    prog <- getProgName\n"
           "    args <- getArgs\n"
           "    fuseRun prog args ops handler\n"
           "\n"
           "fuseRun :: String -> [String] -> Exception e => FuseOperations fh -> (e -> IO Errno) -> IO ()\n"
           "fuseRun prog args ops handler =\n"
           "    catch\n"
           "       (withFuseArgs prog args (\\pArgs ->\n"
           "         do cmd <- fuseParseCommandLine pArgs\n"
           "            case cmd of\n"
           "              Nothing -> fail \"\"\n"
           "              Just (Nothing, _, _) -> fail \"Usage error: mount point required\"\n"
           "              Just (Just mountPt, _, foreground) -> fuseMainReal foreground ops handler pArgs mountPt))\n"
           "       ((\\errStr -> when (not $ null errStr) (putStrLn errStr) >> exitFailure) . ioeGetErrorString)\n"
           "\n"
           "-----------------------------------------------------------------------------\n"
           "-- Miscellaneous utilities\n"
           "\n"
           "unErrno :: Errno -> CInt\n"
           "unErrno (Errno errno) = errno\n"
           "\n"
           "okErrno :: CInt\n"
           "okErrno = unErrno eOK\n"
           "\n"
           "pokeCStringLen :: CStringLen -> String -> IO ()\n"
           "pokeCStringLen (pBuf, bufSize) src =\n"
           "    pokeArray pBuf $ take bufSize $ map castCharToCChar src\n"
           "\n"
           "pokeCStringLen0 :: CStringLen -> String -> IO ()\n"
           "pokeCStringLen0 (pBuf, bufSize) src =\n"
           "    pokeArray0 0 pBuf $ take (bufSize - 1) $ map castCharToCChar src\n"
           "\n"
           "", hsc_stdout());
#line 900 "Fuse.hsc"
#if MIN_VERSION_base(4,6,0)
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (901, "System/Fuse.hsc");
    hsc_fputs ("catch = catchIOError\n"
           "", hsc_stdout());
#line 902 "Fuse.hsc"
#else 
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (903, "System/Fuse.hsc");
    hsc_fputs ("", hsc_stdout());
#line 903 "Fuse.hsc"
#endif 
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (904, "System/Fuse.hsc");
    hsc_fputs ("\n"
           "-----------------------------------------------------------------------------\n"
           "-- C land\n"
           "\n"
           "---\n"
           "-- exported C called from Haskell\n"
           "---\n"
           "\n"
           "data CFuseArgs -- struct fuse_args\n"
           "\n"
           "data CFuseChan -- struct fuse_chan\n"
           "foreign import ccall safe \"fuse.h fuse_mount\"\n"
           "    fuse_mount :: Ptr CStructFuse -> CString -> IO CInt\n"
           "\n"
           "foreign import ccall safe \"fuse.h fuse_unmount\"\n"
           "    fuse_unmount :: Ptr CStructFuse -> IO ()\n"
           "\n"
           "data CFuseSession -- struct fuse_session\n"
           "foreign import ccall safe \"fuse.h fuse_get_session\"\n"
           "    fuse_get_session :: Ptr CStructFuse -> IO (Ptr CFuseSession)\n"
           "\n"
           "foreign import ccall safe \"fuse.h fuse_session_exit\"\n"
           "    fuse_session_exit :: Ptr CFuseSession -> IO ()\n"
           "\n"
           "foreign import ccall safe \"fuse.h fuse_set_signal_handlers\"\n"
           "    fuse_set_signal_handlers :: Ptr CFuseSession -> IO CInt\n"
           "\n"
           "foreign import ccall safe \"fuse.h fuse_remove_signal_handlers\"\n"
           "    fuse_remove_signal_handlers :: Ptr CFuseSession -> IO ()\n"
           "\n"
           "data CFuseCmdlineOpts -- struct fuse_cmdline_opts\n"
           "foreign import ccall safe \"fuse3/fuse_lowlevel.h fuse_parse_cmdline\"\n"
           "    fuse_parse_cmdline :: Ptr CFuseArgs -> Ptr CFuseCmdlineOpts -> IO CInt\n"
           "\n"
           "data CStructFuse -- struct fuse\n"
           "data CFuseOperations -- struct fuse_operations\n"
           "foreign import ccall safe \"fuse.h fuse_new\"\n"
           "    fuse_new :: Ptr CFuseArgs -> Ptr CFuseOperations -> CSize -> Ptr () -> IO (Ptr CStructFuse)\n"
           "\n"
           "foreign import ccall safe \"fuse.h fuse_destroy\"\n"
           "    fuse_destroy :: Ptr CStructFuse -> IO ()\n"
           "\n"
           "foreign import ccall safe \"fuse.h fuse_opt_free_args\"\n"
           "    fuse_opt_free_args :: Ptr CFuseArgs -> IO ()\n"
           "\n"
           "foreign import ccall safe \"fuse.h fuse_loop_mt\"\n"
           "    fuse_loop_mt :: Ptr CStructFuse -> CInt -> IO CInt\n"
           "\n"
           "data CFuseContext\n"
           "foreign import ccall safe \"fuse.h fuse_get_context\"\n"
           "    fuse_get_context :: IO (Ptr CFuseContext)\n"
           "\n"
           "---\n"
           "-- dynamic Haskell called from C\n"
           "---\n"
           "\n"
           "data CFuseFileInfo -- struct fuse_file_info\n"
           "data CFuseConnInfo -- struct fuse_conn_info\n"
           "\n"
           "data CStat -- struct stat\n"
           "type CGetAttr = CString -> Ptr CStat -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkGetAttr :: CGetAttr -> IO (FunPtr CGetAttr)\n"
           "\n"
           "type CReadLink = CString -> CString -> CSize -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkReadLink :: CReadLink -> IO (FunPtr CReadLink)\n"
           "\n"
           "type CMkNod = CString -> CMode -> CDev -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkMkNod :: CMkNod -> IO (FunPtr CMkNod)\n"
           "\n"
           "type CMkDir = CString -> CMode -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkMkDir :: CMkDir -> IO (FunPtr CMkDir)\n"
           "\n"
           "type CUnlink = CString -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkUnlink :: CUnlink -> IO (FunPtr CUnlink)\n"
           "\n"
           "type CRmDir = CString -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkRmDir :: CRmDir -> IO (FunPtr CRmDir)\n"
           "\n"
           "type CSymLink = CString -> CString -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkSymLink :: CSymLink -> IO (FunPtr CSymLink)\n"
           "\n"
           "type CRename = CString -> CString -> CUInt -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkRename :: CRename -> IO (FunPtr CRename)\n"
           "\n"
           "type CLink = CString -> CString -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkLink :: CLink -> IO (FunPtr CLink)\n"
           "\n"
           "type CChMod = CString -> CMode -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkChMod :: CChMod -> IO (FunPtr CChMod)\n"
           "\n"
           "type CChOwn = CString -> CUid -> CGid -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkChOwn :: CChOwn -> IO (FunPtr CChOwn)\n"
           "\n"
           "type CTruncate = CString -> COff -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkTruncate :: CTruncate -> IO (FunPtr CTruncate)\n"
           "\n"
           "type COpen = CString -> Ptr CFuseFileInfo -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkOpen :: COpen -> IO (FunPtr COpen)\n"
           "\n"
           "type CRead = CString -> CString -> CSize -> COff -> Ptr CFuseFileInfo -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkRead :: CRead -> IO (FunPtr CRead)\n"
           "\n"
           "type CWrite = CString -> CString -> CSize -> COff -> Ptr CFuseFileInfo -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkWrite :: CWrite -> IO (FunPtr CWrite)\n"
           "\n"
           "data CStructStatVFS -- struct fuse_stat_fs\n"
           "type CStatFS = CString -> Ptr CStructStatVFS -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkStatFS :: CStatFS -> IO (FunPtr CStatFS)\n"
           "\n"
           "type CFlush = CString -> Ptr CFuseFileInfo -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkFlush :: CFlush -> IO (FunPtr CFlush)\n"
           "\n"
           "type CRelease = CString -> Ptr CFuseFileInfo -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkRelease :: CRelease -> IO (FunPtr CRelease)\n"
           "\n"
           "type CFSync = CString -> Int -> Ptr CFuseFileInfo -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkFSync :: CFSync -> IO (FunPtr CFSync) \n"
           "\n"
           "-- XXX add *xattr bindings\n"
           "\n"
           "type COpenDir = CString -> Ptr CFuseFileInfo -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkOpenDir :: COpenDir -> IO (FunPtr COpenDir)\n"
           "\n"
           "type CReadDir = CString -> Ptr CFillDirBuf -> FunPtr CFillDir -> COff\n"
           "             -> Ptr CFuseFileInfo -> CUInt -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkReadDir :: CReadDir -> IO (FunPtr CReadDir)\n"
           "\n"
           "type CReleaseDir = CString -> Ptr CFuseFileInfo -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkReleaseDir :: CReleaseDir -> IO (FunPtr CReleaseDir)\n"
           "\n"
           "type CFSyncDir = CString -> Int -> Ptr CFuseFileInfo -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkFSyncDir :: CFSyncDir -> IO (FunPtr CFSyncDir)\n"
           "\n"
           "type CAccess = CString -> CInt -> IO CInt\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkAccess :: CAccess -> IO (FunPtr CAccess)\n"
           "\n"
           "-- CInt because anything would be fine as we don\'t use them\n"
           "type CInit = Ptr CFuseConnInfo -> IO (Ptr CInt)\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkInit :: CInit -> IO (FunPtr CInit)\n"
           "\n"
           "type CDestroy = Ptr CInt -> IO ()\n"
           "foreign import ccall safe \"wrapper\"\n"
           "    mkDestroy :: CDestroy -> IO (FunPtr CDestroy)\n"
           "\n"
           "----\n"
           "\n"
           "bsToBuf :: Ptr a -> B.ByteString -> Int -> IO ()\n"
           "bsToBuf dst bs len = do\n"
           "  let l = fromIntegral $ min len $ B.length bs\n"
           "  B.unsafeUseAsCString bs $ \\src -> B.memcpy (castPtr dst) (castPtr src) l\n"
           "  return ()\n"
           "\n"
           "-- Get filehandle\n"
           "getFH pFuseFileInfo = do\n"
           "  sptr <- (", hsc_stdout());
#line 1083 "Fuse.hsc"
    hsc_peek (struct fuse_file_info, fh);
    hsc_fputs (") pFuseFileInfo\n"
           "", hsc_stdout());
    hsc_line (1084, "System/Fuse.hsc");
    hsc_fputs ("  cVal <- deRefStablePtr $ castPtrToStablePtr sptr\n"
           "  return cVal\n"
           "\n"
           "delFH pFuseFileInfo = do\n"
           "  sptr <- (", hsc_stdout());
#line 1088 "Fuse.hsc"
    hsc_peek (struct fuse_file_info, fh);
    hsc_fputs (") pFuseFileInfo\n"
           "", hsc_stdout());
    hsc_line (1089, "System/Fuse.hsc");
    hsc_fputs ("  freeStablePtr $ castPtrToStablePtr sptr\n"
           "\n"
           "\n"
           "---\n"
           "-- dynamic C called from Haskell\n"
           "---\n"
           "\n"
           "data CDirHandle -- fuse_dirh_t\n"
           "type CDirFil = Ptr CDirHandle -> CString -> Int -> IO CInt -- fuse_dirfil_t\n"
           "foreign import ccall safe \"dynamic\"\n"
           "    mkDirFil :: FunPtr CDirFil -> CDirFil\n"
           "\n"
           "data CFillDirBuf -- void\n"
           "type CFillDir = Ptr CFillDirBuf -> CString -> Ptr CStat -> COff -> CUInt -> IO CInt\n"
           "\n"
           "foreign import ccall safe \"dynamic\"\n"
           "    mkFillDir :: FunPtr CFillDir -> CFillDir\n"
           "\n"
           "foreign import ccall safe \"bzero\"\n"
           "    bzero :: Ptr a -> Int -> IO ()\n"
           "\n"
           "", hsc_stdout());
    return 0;
}
