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
    ImageFormat (..),
    loadImage,
    encode,
  )
where

import Codec.Picture.Saving
import Codec.Picture.Types (DynamicImage)
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
  { format :: ImageFormat,
    image :: ByteString
  }
  deriving (Typeable)

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
--     route idRoute
--     compile $ loadImage
--         >>= compressJpgCompiler 50
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
encode :: ImageFormat -> DynamicImage -> Image
encode Jpeg im = Image Jpeg $ (toStrict . imageToJpg 100) im
encode Png im = Image Png $ (toStrict . imageToPng) im
encode Bitmap im = Image Bitmap $ (toStrict . imageToBitmap) im
encode Tiff im = Image Tiff $ (toStrict . imageToTiff) im
encode Gif im = Image Gif $ (toStrict . fromRight (error "Could not parse gif") . imageToGif) im
