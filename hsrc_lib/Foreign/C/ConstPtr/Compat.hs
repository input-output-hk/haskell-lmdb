{-# LANGUAGE CPP #-}

module Foreign.C.ConstPtr.Compat (ConstPtr, unConstPtr) where

import           Foreign.Ptr
#if MIN_VERSION_GLASGOW_HASKELL(9,6,1,0)
import qualified Foreign.C.ConstPtr as F
#endif

# if MIN_VERSION_GLASGOW_HASKELL(9,6,1,0)
type ConstPtr = F.ConstPtr
unConstPtr :: ConstPtr a -> Ptr a
unConstPtr = F.unConstPtr
# else
type ConstPtr = Ptr
unConstPtr :: ConstPtr a -> Ptr a
unConstPtr = id
# endif
