module System.Fuse.FuseOpt
    ( FuseOpt(..)
    , FuseOptType(..)
    , FuseOptResult
    , fuseOptParse
    ) where

import System.Fuse.CTypes
import System.Fuse.Utils ( memset )

import Control.Monad ( foldM, foldM_ )

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

#include <fuse3/fuse_opt.h>

data FuseOptType = FuseOptString | FuseOptBool
    deriving (Show, Eq)

-- | 'FuseOpt' represents string options for fuse.
data FuseOpt = FuseOpt String FuseOptType
    deriving (Show, Eq)

type FuseOptResult = [(String, Either String Bool)]

-- | 'fuseOptParse' parses command line options (2nd argument)
fuseOptParse :: Ptr CFuseArgs -- ^ Pointer to struct fuse_args
             -> [FuseOpt]     -- ^ List of option names and types
             -> IO (FuseOptResult, CInt)
                -- ^ Returns list of parsed options and error code
fuseOptParse _  [] = return ([], 0)
fuseOptParse pArgs opts = do
    let len = length opts
        sfo = (#size struct fuse_opt)
        sel = max (sizeOf (0 :: CULong)) (sizeOf (nullPtr :: CString))
        ifStr t a b = if t == FuseOptString then a else b
        pokeOpt :: Ptr a -> Int -> FuseOpt -> IO Int
        pokeOpt pOpts off (FuseOpt name t) = do
            cs <- newCString (name ++ ifStr t "=%s" "")
            let p = pOpts `plusPtr` off
            (#poke struct fuse_opt, templ) p cs
            (#poke struct fuse_opt, offset) p (off * sel)
            (#poke struct fuse_opt, value) p (ifStr t 0 1 :: CInt)
            return $ off + sfo
        peekRes :: Ptr a -> Ptr b -> FuseOptResult -> (FuseOpt, Int) -> IO FuseOptResult
        peekRes pData pOpts res (FuseOpt n t, num) = do
            let p = pOpts `plusPtr` (num * sfo)
            pTempl <- (#peek struct fuse_opt, templ) p
            free $ (pTempl :: CString)
            off <- (#peek struct fuse_opt, offset) p
            let pd = pData `plusPtr` off
            ifStr t (do { pt <- peek (castPtr pd :: Ptr CString)
                        ; if pt == nullPtr
                             then return res
                             else peekCString pt >>= (\s ->
                                  free pt >> (return $ (n, Left s) : res))})
                    (do { num <- peek (castPtr pd :: Ptr CULong)
                        ; if num == 0 then return res
                                      else return $ (n, Right True) : res})
    allocaBytes (sfo * (len+1)) $ (\pOpts ->
        allocaBytes (sel * len) $ (\pData -> do
            memset pOpts 0 (fromIntegral $ sfo * (len+1))
            foldM_ (pokeOpt pOpts) 0 opts
            ret <- fuse_opt_parse pArgs pData pOpts nullPtr
            res <- foldM (peekRes pData pOpts) [] (zip opts [0..])
            return (res, ret)))



---
-- C IMPORTS
---

foreign import ccall safe "fuse3/fuse_opt.h fuse_opt_parse"
    fuse_opt_parse :: Ptr CFuseArgs -> Ptr () -> Ptr CFuseOpt -> Ptr () -> IO CInt
