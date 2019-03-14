{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

{-------------------------------------------------------------------------------

Acknowledgements
~~~~~~~~~~~~~~~~

  * Utf8 and Renderable were adapted from rio package:
    http://hackage.haskell.org/package/rio-0.1.8.0/docs/src/RIO.Prelude.Display.html

  * FromUtf8, cvt and cvtLn was modelled after the core types and classes of
    the fmt package:
    http://hackage.haskell.org/package/fmt-0.6.1.1/docs/Fmt-Internal-Core.html

-------------------------------------------------------------------------------}

module Text.Utf8
  (
  -- * Overview
  -- $overview

  -- * Utf8
    Utf8(..)
  -- * FromUtf8
  , FromUtf8(..)
  , cvtLn
  -- * Renderable
  , Renderable(..)
  -- * Toolkit Functions
  , renderShowable
  , unsafeInjectByteStringIntoUtf8
  , utf8ToText
  , utf8ToLazyText
  , writeFileUtf8
  ) where

import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.Builder  as BB
import           Data.Coerce
import           Data.Int
import           Data.String
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Text.Encoding.Error
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Builder   as TB
import qualified Data.Text.Lazy.Encoding  as TL
import           Data.Word
import           Fmt
import           Fmt.Internal.Core
import           System.IO
import           Text.Utf8.Compat


{- $overview

This module provides a standalone package for a wrapped 'B.ByteString'
'B.Builder' for working with UTF-8 encode byte strings. It is adaped from
the @Utf8Builder@ type and @Display@ clas from the @rio@ package,
and the @FromBuilder@ class of the @fmt@ package, while endeavouring to
integrate the 'fmt' types and clases to allow easy interoperation
between both systems.
-}


--
-- Utf8
--

-- | a UTF-8 encoded 'B.ByteString' 'BB.Builder'
newtype Utf8 = Utf8 { getUtf8 :: BB.Builder }
  deriving (Semigroup,Monoid)

instance IsString    Utf8 where fromString  = Utf8 . BB.stringUtf8
instance Buildable   Utf8 where build       = cvt
instance FromBuilder Utf8 where fromBuilder = render


--
-- FromUtf8, cvt
--

-- | for converting Utf8 into other text types
class FromUtf8 a where
  -- | make a 'Utf8' into whatever text type you need it to be
  cvt :: Utf8 -> a

-- | add a newline to the 'Utf8' before converting it
cvtLn :: FromUtf8 a => Utf8 -> a
cvtLn u8 = cvt $ u8 <> "\n"

instance FromUtf8 Utf8                where cvt = id
instance FromUtf8 BB.Builder          where cvt = coerce
instance FromUtf8 TB.Builder          where cvt = TB.fromText         . cvt
instance FromUtf8 T.Text              where cvt = TL.toStrict         . cvt
instance FromUtf8 TL.Text             where cvt = decUtf8TL           . coerce
instance FromUtf8 B.ByteString        where cvt = BL.toStrict         . cvt
instance FromUtf8 BL.ByteString       where cvt = BB.toLazyByteString . coerce
instance a ~ Char => FromUtf8 [a]     where cvt = T.unpack            . cvt
instance a ~ ()   => FromUtf8 (IO a)  where cvt = putBuilder          . coerce


--
-- Renderable
--

-- | for rendering things into Utf8
class Renderable a where
  -- | render @a@ into a 'Utf8'
  render :: a -> Utf8
  render = render . renderText

  -- | render @a@ into a @Text@
  renderText :: a -> T.Text
  renderText = utf8ToText . render

instance Renderable Utf8           where render = id
instance Renderable T.Text         where render = coerce . T.encodeUtf8Builder
instance Renderable TL.Text        where render = foldMap render . TL.toChunks
instance Renderable TB.Builder     where render = coerce . encUtf8TB
instance Renderable Char           where render = coerce . BB.charUtf8
instance Renderable Integer        where render = coerce . BB.integerDec
instance Renderable Float          where render = coerce . BB.floatDec
instance Renderable Double         where render = coerce . BB.doubleDec
instance Renderable Int            where render = coerce . BB.intDec
instance Renderable Int8           where render = coerce . BB.int8Dec
instance Renderable Int16          where render = coerce . BB.int16Dec
instance Renderable Int32          where render = coerce . BB.int32Dec
instance Renderable Int64          where render = coerce . BB.int64Dec
instance Renderable Word           where render = coerce . BB.wordDec
instance Renderable Word8          where render = coerce . BB.word8Dec
instance Renderable Word16         where render = coerce . BB.word16Dec
instance Renderable Word32         where render = coerce . BB.word32Dec
instance Renderable Word64         where render = coerce . BB.word64Dec
instance Renderable SomeException  where render = fromString . displayException
instance Renderable IOException    where render = fromString . displayException


--
-- Utf8 toolkit functions
--

-- | show into 'Utf8'
renderShowable :: Show a => a -> Utf8
renderShowable = fromString . show

-- | Convert a 'B.ByteString' into a 'Utf8'; NB, if
-- the @ByteString@ contains text using a non-UTF-8 encoding
-- then 'bad things' (TM) are liable to happen
unsafeInjectByteStringIntoUtf8 :: B.ByteString -> Utf8
unsafeInjectByteStringIntoUtf8 = Utf8 . BB.byteString

-- | Convert a 'Utf8' value into a strict 'T.Text'.
utf8ToText :: Utf8 -> T.Text
utf8ToText =
    T.decodeUtf8With lenientDecode
      . BL.toStrict
      . BB.toLazyByteString
      . coerce

-- | Convert a 'Utf8' value into a lazy 'T.Text'.
utf8ToLazyText :: Utf8 -> TL.Text
utf8ToLazyText =
    TL.decodeUtf8With lenientDecode
      . BB.toLazyByteString
      . coerce

-- | Write 'Utf8' to file.
writeFileUtf8 :: MonadIO m => FilePath -> Utf8 -> m ()
writeFileUtf8 fp (Utf8 builder) =
    liftIO $ withBinaryFile fp WriteMode $ \h -> BB.hPutBuilder h builder
