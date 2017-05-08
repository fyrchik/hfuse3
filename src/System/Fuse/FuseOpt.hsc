module System.Fuse.FuseOpt
    ( FuseOpt(..)
    , OptSpec
    , noOpt
    , fuseOptEnd
    ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include <fuse3/fuse_opt.h>

-- | 'OptSpec' is used in parsing of options
-- 'a' must be an instance of Storable
type OptSpec a = ([FuseOpt], a)

-- this is ugly
noOpt = ([], 0::CInt)

-- | 'FuseOpt' represents string options for fuse.
data FuseOpt = FuseOpt
    { optTempl  :: String
    , optOffset :: CULong
    , optValue  :: CInt
    }

fuseOptEnd :: FuseOpt
fuseOptEnd = FuseOpt "" 0 0

-- | 'Storable' instance for 'FuseOpt'.
-- Here we make no difference between NULL pointer
-- and pointer to empty string, so both 'poke' as options
-- with empty template.
instance Storable FuseOpt where
    sizeOf    _ = (#size struct fuse_opt)
    alignment _ = alignment (0 :: CULong)

    peek ptr = FuseOpt <$> getTempl
                       <*> (#peek struct fuse_opt, offset) ptr
                       <*> (#peek struct fuse_opt, value) ptr
      where getTempl :: IO String
            getTempl = do
              p <- (#peek struct fuse_opt, offset) ptr
              if p /= nullPtr
                then peekCString p
                else return ""

    poke ptr (FuseOpt t o v) = do
        setTempl t
        (#poke struct fuse_opt, offset) ptr o
        (#poke struct fuse_opt, value) ptr v
      where setTempl :: String -> IO ()
            setTempl "" = (#poke struct fuse_opt, templ) ptr nullPtr
            setTempl s = newCString s >>= (#poke struct fuse_opt, templ) ptr
