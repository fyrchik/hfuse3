module System.Fuse.Types
    ( EntryType(..)

    , entryTypeToDT
    , entryTypeToFileMode
    , fileModeToEntryType

    , fileTypeModes
    , blockSpecialMode
    , characterSpecialMode
    , namedPipeMode
    , regularFileMode
    , directoryMode
    , symbolicLinkMode
    , socketMode
    ) where

import Data.Bits ((.&.))
import System.Posix.Types (FileMode)

#include <sys/stat.h>
#include <dirent.h>

-- | The Unix type of a node in the filesystem.
data EntryType
    = Unknown            -- ^ Unknown entry type
    | NamedPipe
    | CharacterSpecial
    | Directory
    | BlockSpecial
    | RegularFile
    | SymbolicLink
    | Socket
      deriving(Show)

entryTypeToDT :: EntryType -> Int
entryTypeToDT Unknown          = (#const DT_UNKNOWN)
entryTypeToDT NamedPipe        = (#const DT_FIFO)
entryTypeToDT CharacterSpecial = (#const DT_CHR)
entryTypeToDT Directory        = (#const DT_DIR)
entryTypeToDT BlockSpecial     = (#const DT_BLK)
entryTypeToDT RegularFile      = (#const DT_REG)
entryTypeToDT SymbolicLink     = (#const DT_LNK)
entryTypeToDT Socket           = (#const DT_SOCK)

fileTypeModes :: FileMode
fileTypeModes = (#const S_IFMT)

blockSpecialMode :: FileMode
blockSpecialMode = (#const S_IFBLK)

characterSpecialMode :: FileMode
characterSpecialMode = (#const S_IFCHR)

namedPipeMode :: FileMode
namedPipeMode = (#const S_IFIFO)

regularFileMode :: FileMode
regularFileMode = (#const S_IFREG)

directoryMode :: FileMode
directoryMode = (#const S_IFDIR)

symbolicLinkMode :: FileMode
symbolicLinkMode = (#const S_IFLNK)

socketMode :: FileMode
socketMode = (#const S_IFSOCK)

-- | Converts an 'EntryType' into the corresponding POSIX 'FileMode'.
entryTypeToFileMode :: EntryType -> FileMode
entryTypeToFileMode Unknown          = 0
entryTypeToFileMode NamedPipe        = namedPipeMode
entryTypeToFileMode CharacterSpecial = characterSpecialMode
entryTypeToFileMode Directory        = directoryMode
entryTypeToFileMode BlockSpecial     = blockSpecialMode
entryTypeToFileMode RegularFile      = regularFileMode
entryTypeToFileMode SymbolicLink     = symbolicLinkMode
entryTypeToFileMode Socket           = socketMode

fileModeToEntryType :: FileMode -> EntryType
fileModeToEntryType mode
    | fileType == namedPipeMode        = NamedPipe
    | fileType == characterSpecialMode = CharacterSpecial
    | fileType == directoryMode        = Directory
    | fileType == blockSpecialMode     = BlockSpecial
    | fileType == regularFileMode      = RegularFile
    | fileType == symbolicLinkMode     = SymbolicLink
    | fileType == socketMode           = Socket
    where fileType = mode .&. (#const S_IFMT)
