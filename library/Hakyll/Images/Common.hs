
{-|
Module      : Hakyll.Images.Common
Description : Types and utilities for Hakyll.Images
Copyright   : (c) Laurent P René de Cotret, 2019
License     : BSD3
Maintainer  : laurent.decotret@outlook.com
Stability   : stable
Portability : portable
-}

module Hakyll.Images.Common ( Image 
                            , ImageFormat
                            , fromExt
                            , loadImage
                            , encode
                            ) where

import Prelude                          hiding (readFile)

import Codec.Picture.Types              (DynamicImage)
import Codec.Picture.Saving

import Data.ByteString.Lazy             (toStrict)
import Data.ByteString                  (ByteString)

import Hakyll.Core.Compiler             (Compiler, getResourceLBS)
import Hakyll.Core.Item                 (Item(..))


type Image = ByteString

-- Supported (i.e. encodable) image formats
data ImageFormat
    = Jpeg 
    | Png
    | Bitmap
    | Tiff

-- | Load an image from a file.
-- This function can be combined with other compilers.
--
-- @
-- match "*.jpg" $ do
--     route idRoute
--     compile $ loadImage 
--         >>= compressJpgCompiler 50
-- @
loadImage :: Compiler Image
loadImage = (toStrict . itemBody) <$> getResourceLBS

-- | Translation between file extensions and image formats.
-- It is important to keep track of image formats because Hakyll
-- compilers provides raw bytestrings and filenames
fromExt :: String -> ImageFormat
fromExt ".jpeg" = Jpeg
fromExt ".jpg"  = Jpeg
fromExt ".png"  = Png
fromExt ".bmp"  = Bitmap
fromExt ".tif"  = Tiff
fromExt ".tiff" = Tiff
fromExt ext     = error $ "Unsupported format: " <> ext

-- Encode images based on file extension
encode :: ImageFormat -> DynamicImage -> Image
encode Jpeg = toStrict . imageToJpg 100
encode Png  = toStrict . imageToPng
encode Bitmap = toStrict . imageToBitmap
encode Tiff = toStrict . imageToTiff