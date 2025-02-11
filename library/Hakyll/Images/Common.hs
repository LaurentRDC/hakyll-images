{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Hakyll.Images.Common
-- Description : Types and utilities for Hakyll.Images
-- Copyright   : (c) Laurent P René de Cotret, 2019 - present
-- License     : BSD3
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : unstable
-- Portability : portable
module Hakyll.Images.Common
  ( Image (..),
    withImageContent,
    ImageContent,
    decodeContent,
    WithMetadata (..),
    ImageFormat (..),
    loadImage,
    encode,
  )
where

import Codec.Picture (decodeImageWithMetadata)
import Codec.Picture.Metadata (Metadatas)
import qualified Codec.Picture.Metadata as Meta
import qualified Codec.Picture.Metadata.Exif as Meta
import Codec.Picture.Saving
import Codec.Picture.Saving.WithMetadata
  ( imageToBitmapWithMetadata,
    imageToJpgWithMetadata,
    imageToPngWithMetadata,
  )
import Codec.Picture.Types (DynamicImage)
import Data.Bifunctor (second)
import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Char (toLower)
import Data.Either (fromRight)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Hakyll.Core.Compiler (Compiler, getResourceLBS, getUnderlyingExtension)
import Hakyll.Core.Item (Item (..))
import Hakyll.Core.Writable (Writable (..))
import Prelude hiding (readFile)

-- Supported (i.e. encodable) image formats
data ImageFormat
  = Jpeg
  | Png
  | Bitmap
  | Tiff
  | Gif
  deriving (Eq, Generic)

-- Automatic derivation of Binary instances requires Generic
instance Binary ImageFormat

data Image = Image
  { format :: !ImageFormat,
    image :: !ByteString
  }
  deriving (Typeable)

-- Implementation note
-- We need to keep the content of an image as a bytestring, as
-- much as possible, because Hakyll's Items must be serializable.

data WithMetadata a
  = MkWithMetadata
  { getData :: !a,
    getMetadata :: !Metadatas
  }

type ImageContent = WithMetadata DynamicImage

decodeContent :: ByteString -> WithMetadata DynamicImage
decodeContent im = case decodeImageWithMetadata im of
  Left msg -> error msg
  Right content -> uncurry MkWithMetadata (second pruneMetadatas content)
    where
      -- \| Prune metadata to only keep tags which are absolutely necessary
      -- (of which there are very few).
      pruneMetadatas :: Metadatas -> Metadatas
      pruneMetadatas meta =
        foldMap (\(k, v) -> Meta.singleton (Meta.Exif k) v) $
          filter (\(k, _) -> k == Meta.TagOrientation) $
            Meta.extractExifMetas meta

instance Functor WithMetadata where
  fmap f (MkWithMetadata a m) = (MkWithMetadata (f a) m)

-- When writing to disk, we ignore the image format.
-- Trusting users to route correctly.
instance Writable Image where
  -- Write the bytestring content
  write fp item = write fp (image <$> item)

-- Binary instance looks similar to the binary instance for a Hakyll Item
instance Binary Image where
  put (Image fmt content) = put fmt >> put content
  get = Image <$> get <*> get

-- | Load an image from a file.
-- This function can be combined with other compilers.
--
-- @
-- match "*.jpg" $ do
--    route idRoute
--    compile $ loadImage >>= compressJpgCompiler 50
-- @
loadImage :: Compiler (Item Image)
loadImage = do
  content <- fmap toStrict <$> getResourceLBS
  fmt <- fromExt <$> getUnderlyingExtension
  return $ Image fmt <$> content

-- | Translation between file extensions and image formats.
-- It is important to keep track of image formats because Hakyll
-- compilers provides raw bytestrings and filenames.
--
-- This function is case-insensitive
fromExt :: String -> ImageFormat
fromExt ext = fromExt' $ toLower <$> ext
  where
    fromExt' ".jpeg" = Jpeg
    fromExt' ".jpg" = Jpeg
    fromExt' ".png" = Png
    fromExt' ".bmp" = Bitmap
    fromExt' ".tif" = Tiff
    fromExt' ".tiff" = Tiff
    fromExt' ".gif" = Gif
    fromExt' ext' = error $ "Unsupported format: " <> ext'

-- Encode images based on file extension
--
-- Unfortunately, the upstream library `JuicyPixels` does not have support
-- for encoding metadata of some image formats, including `Tiff` and `Gif`.
encode :: ImageFormat -> ImageContent -> Image
encode Jpeg (MkWithMetadata im meta) = Image Jpeg $ (toStrict . imageToJpgWithMetadata 100 meta) im
encode Png (MkWithMetadata im meta) = Image Png $ (toStrict . imageToPngWithMetadata meta) im
encode Bitmap (MkWithMetadata im meta) = Image Bitmap $ (toStrict . imageToBitmapWithMetadata meta) im
encode Tiff (MkWithMetadata im _) = Image Tiff $ (toStrict . imageToTiff) im
encode Gif (MkWithMetadata im _) = Image Gif $ (toStrict . fromRight (error "Could not parse gif") . imageToGif) im

-- | Map over the content of an `Image`, decoded into an `ImageContent`.
withImageContent ::
  (ImageContent -> ImageContent) ->
  -- | Encoder function
  (ImageFormat -> ImageContent -> Image) ->
  (Image -> Image)
withImageContent f encoder (Image fmt bts) =
  let content = decodeContent bts
   in encoder fmt $ f content
