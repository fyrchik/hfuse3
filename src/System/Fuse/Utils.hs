module System.Fuse.Utils
    ( memset
    , unErrno
    , okErrno
    , pokeCStringLen
    , pokeCStringLen0
    ) where

import Foreign.C.Error
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr

foreign import ccall safe "memset"
    memset :: Ptr a -> CInt -> CSize -> IO ()

unErrno :: Errno -> CInt
unErrno (Errno errno) = errno

okErrno :: CInt
okErrno = unErrno eOK

pokeCStringLen :: CStringLen -> String -> IO ()
pokeCStringLen (pBuf, bufSize) src =
    pokeArray pBuf $ take bufSize $ map castCharToCChar src

pokeCStringLen0 :: CStringLen -> String -> IO ()
pokeCStringLen0 (pBuf, bufSize) src =
    pokeArray0 0 pBuf $ take (bufSize - 1) $ map castCharToCChar src
