module System.Fuse.Types
    ( EntryType(..)
    ) where

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
