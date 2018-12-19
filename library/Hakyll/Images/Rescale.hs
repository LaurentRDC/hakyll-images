
{-|
Module      : Hakyll.Images.Rescale
Description : Hakyll compiler to rescale images
Copyright   : (c) Laurent P René de Cotret, 2018
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
        -- Resize all profile pictures to 64x48
        match "profiles/**.jpg" $ do
            route idRoute
            compile (resizeImageCompiler 64 48)
        
        -- Scale images to fit within a 600x400 box
        match "images/**.jpg" $ do
            route idRoute
            compile (scaleImageCompiler 600 400)
        
        (... omitted ...)
@
-}
module Hakyll.Images.Rescale
    ( Width, Height
    , resize
    , resizeImageCompiler
    , scale
    , scaleImageCompiler
    ) where

import Codec.Picture            (convertRGBA8, decodeImage)
import Codec.Picture.Types
import Codec.Picture.Saving    
import Codec.Picture.Extra      (scaleBilinear)

import Data.ByteString.Lazy     (ByteString, toStrict)
import Data.Ratio               ((%))

import Hakyll.Core.Item
import Hakyll.Core.Compiler

type Width = Int
type Height = Int

-- | Encode a dynamic image to a bytestring based on the file extension
encode :: String -> DynamicImage -> ByteString
encode ".jpg" = imageToJpg 100
encode ".png" = imageToPng
encode ".bmp" = imageToBitmap
encode ".tiff" = imageToTiff
encode fmt = error $ "Unsupported format " <> fmt

-- | Resize an image to specified width and height using the bilinear transform.
-- The aspect ratio may not be respected.
--
-- In the process, an image is converted to RGBA8. Therefore, some information
-- loss may occur.
resize :: Width -> Height -> DynamicImage -> DynamicImage
resize w h = ImageRGBA8 . (scaleBilinear w h) . convertRGBA8

-- | Compiler that resizes images to a specific dimensions. Aspect ratio
-- may not be preserved.
resizeImageCompiler :: Width -> Height -> Compiler (Item ByteString)
resizeImageCompiler w h = do
    ext <- getUnderlyingExtension
    imageItem <- fmap (decodeImage . toStrict) <$> getResourceLBS
    case itemBody imageItem of
        Left msg -> error msg
        Right image -> do
            let resized = resize w h image
            return $ Item (itemIdentifier imageItem) (encode ext resized)

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
scaleImageCompiler :: Width -> Height -> Compiler (Item ByteString)
scaleImageCompiler w h = do
    ext <- getUnderlyingExtension
    imageItem <- fmap (decodeImage . toStrict) <$> getResourceLBS
    case itemBody imageItem of
        Left msg -> error msg
        Right image -> do
            let rescaled = scale w h image
            return $ Item (itemIdentifier imageItem) (encode ext rescaled)