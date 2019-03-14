{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ > 802
module Text.Utf8.Compat where

import qualified Data.ByteString.Builder  as BB
import           Data.Text.Encoding.Error
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Builder   as TB
import qualified Data.Text.Lazy.Encoding  as TL
import           System.IO

#else
module Text.Utf8.Compat
  ( Semigroup(..)
  , module Text.Utf8.Compat
  ) where

import qualified Data.ByteString.Builder  as BB
import           Data.Text.Encoding.Error
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Builder   as TB
import qualified Data.Text.Lazy.Encoding  as TL
import           System.IO

import           Data.Semigroup
#endif


--
-- internal helper functions
--

decUtf8TL :: BB.Builder -> TL.Text
decUtf8TL = TL.decodeUtf8With lenientDecode . BB.toLazyByteString

encUtf8TB :: TB.Builder -> BB.Builder
encUtf8TB = TL.encodeUtf8Builder . TB.toLazyText

putBuilder :: BB.Builder -> IO ()
putBuilder = BB.hPutBuilder stdout
