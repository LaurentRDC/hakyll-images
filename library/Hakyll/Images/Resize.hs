
{-|
Module      : Hakyll.Images.Resize
Description : Hakyll compiler to resize images
Copyright   : (c) Laurent P René de Cotret, 2019
License     : BSD3
Maintainer  : laurent.decotret@outlook.com
Stability   : stable
Portability : portable

This module defines two Hakyll compilers. The first one, 'resizeImageCompiler', 
is used to resize images to specific dimensions. The aspect ratio might not be the same.

The other compiler, `scaleImageCompiler`, scales images to fit within a specified 
box while preserving aspect ratio.

@
    import Hakyll
    import Hakyll.Images        (resizeImageCompiler, scaleImageCompiler)
    
    (... omitted ...)
    
    hakyll $ do

        (... omitted ...)
        -- Resize all profile pictures with .png extensions to 64x48
        match "profiles/**.png" $ do
            route idRoute
            compile (resizeImageCompiler 64 48)
        
        -- Scale images to fit within a 600x400 box
        match "images/**" $ do
            route idRoute
            compile (scaleImageCompiler 600 400)
        
        (... omitted ...)
@
-}
module Hakyll.Images.Resize
    ( Width, Height
    , resize
    , resizeImageCompiler
    , scale
    , scaleImageCompiler
    ) where

import           Codec.Picture            (convertRGBA8, decodeImage)
import           Codec.Picture.Types      (DynamicImage(..), imageHeight, imageWidth)
import           Codec.Picture.Saving
import           Codec.Picture.Extra      (scaleBilinear)

import           Data.ByteString.Lazy     (ByteString, toStrict)
import qualified Data.ByteString as BS
import           Data.Ratio               ((%))

import           Hakyll.Core.Identifier   (toFilePath)
import           Hakyll.Core.Item         (Item(..))
import           Hakyll.Core.Compiler     (Compiler)

import           Hakyll.Images.Common     (Image)

type Width = Int
type Height = Int

decodeImage' :: BS.ByteString -> DynamicImage
decodeImage' im = case decodeImage im of
    Left msg -> error msg
    Right image -> image 

-- | Resize an image to specified width and height using the bilinear transform.
-- The aspect ratio may not be respected.
--
-- In the process, an image is converted to RGBA8. Therefore, some information
-- loss may occur.
resize :: Width -> Height -> DynamicImage -> DynamicImage
resize w h = ImageRGBA8 . (scaleBilinear w h) . convertRGBA8

-- | Compiler that resizes images to a specific dimensions. Aspect ratio
-- may not be preserved.
resizeImageCompiler :: Width -> Height -> Item Image -> Compiler (Item Image)
resizeImageCompiler w h item = do
    let ext = (fromExt . toFilePath . itemIdentifier) item 
    return $ (encode ext . resize w h . decodeImage' . toStrict) <$> item

-- | Scale an image to a size that will fit in the specified width and height,
-- while preserving aspect ratio.
--
-- In the process, an image is converted to RGBA8. Therefore, some information
-- loss may occur.
scale :: Width -> Height -> DynamicImage -> DynamicImage
scale w h img = resize maxWidth maxHeight img
    where
        img' = convertRGBA8 img -- Required to extract height and width
        (imgWidth, imgHeight) = (imageWidth img', imageHeight img')
        -- Find the smallest resizing that will accomodate both the width 
        -- and height.
        resizing = min (w % imgWidth) (h % imgHeight)
        maxWidth = round (resizing * fromIntegral imgWidth)
        maxHeight = round (resizing * fromIntegral imgHeight)

-- | Compiler that rescales images to fit within dimensions. Aspect ratio
-- will be preserved.
scaleImageCompiler :: Width -> Height -> Item Image -> Compiler (Item Image)
scaleImageCompiler w h item = do
    let ext = (fromExt . toFilePath . itemIdentifier) item 
    return $ (encode ext . scale w h . decodeImage' . toStrict) <$> item

-- Supported (i.e. encodable) image formats
data ImageFormat
    = Jpeg 
    | Png
    | Bitmap
    | Tiff

-- | Translation between file extensions and image formats
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
encode :: ImageFormat -> DynamicImage -> ByteString
encode Jpeg = imageToJpg 100
encode Png  = imageToPng
encode Bitmap = imageToBitmap
encode Tiff = imageToTiff