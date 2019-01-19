
{-|
Module      : Hakyll.Images.Common
Description : Types and utilities for Hakyll.Images
Copyright   : (c) Laurent P René de Cotret, 2019
License     : BSD3
Maintainer  : laurent.decotret@outlook.com
Stability   : unstable
Portability : portable
-}

module Hakyll.Images.Common ( Image
                            , Image_(..)
                            , ImageFormat(..)
                            , format
                            , image
                            , loadImage
                            , encode
                            ) where

import Prelude                          hiding (readFile)

import Codec.Picture.Types              (DynamicImage)
import Codec.Picture.Saving

import Data.ByteString.Lazy             (toStrict)
import Data.ByteString                  (ByteString)

import Hakyll.Core.Compiler             (Compiler, getResourceLBS, getUnderlyingExtension)
import Hakyll.Core.Item                 (Item(..))

-- Supported (i.e. encodable) image formats
data ImageFormat
    = Jpeg 
    | Png
    | Bitmap
    | Tiff
    deriving (Eq)

-- Polymorphic type only to get an instance of functor
data Image_ a = Image ImageFormat a

instance Functor Image_ where
    fmap f (Image fmt a) = Image fmt (f a)

type Image = Image_ ByteString

-- | Extract format from an image
format :: Image_ a -> ImageFormat
format (Image fmt _) = fmt

-- | Extract data from image
image :: Image_ a -> a
image (Image _ im) = im

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
    return $ (Image fmt) <$> content

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
encode Jpeg im   = Image Jpeg   $ (toStrict . imageToJpg 100) im
encode Png im    = Image Png    $ (toStrict . imageToPng) im
encode Bitmap im = Image Bitmap $ (toStrict . imageToBitmap) im
encode Tiff im   = Image Tiff   $ (toStrict . imageToTiff) im