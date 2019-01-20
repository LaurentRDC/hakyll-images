
{-|
Module      : Hakyll.Images.Resize
Description : Hakyll compiler to resize images
Copyright   : (c) Laurent P René de Cotret, 2019
License     : BSD3
Maintainer  : laurent.decotret@outlook.com
Stability   : unstable
Portability : portable

This module defines two Hakyll compilers. The first one, 'resizeImageCompiler', 
is used to resize images to specific dimensions. The aspect ratio might not be the same.

The other compiler, `scaleImageCompiler`, scales images to fit within a specified 
box while preserving aspect ratio.

@
    import Hakyll
    import Hakyll.Images        ( loadImage
                                , resizeImageCompiler 
                                , scaleImageCompiler
                                )
    
    hakyll $ do

        -- Resize all profile pictures with .png extensions to 64x48
        match "profiles/**.png" $ do
            route idRoute
            compile $ loadImage
                >>= resizeImageCompiler 64 48
        
        -- Scale images to fit within a 600x400 box
        match "images/**" $ do
            route idRoute
            compile $ loadImage
                >>= scaleImageCompiler 600 400
        
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

import Codec.Picture            (convertRGBA8, decodeImage)
import Codec.Picture.Types      (DynamicImage(..), imageHeight, imageWidth)
import Codec.Picture.Extra      (scaleBilinear)

import Data.ByteString          (ByteString)
import Data.Ratio               ((%))

import Hakyll.Core.Item         (Item(..))
import Hakyll.Core.Compiler     (Compiler)

import Hakyll.Images.Common     (Image(..), encode)

type Width = Int
type Height = Int

decodeImage' :: ByteString -> DynamicImage
decodeImage' im = case decodeImage im of
    Left msg -> error msg
    Right im' -> im' 

-- | Resize an image to specified width and height using the bilinear transform.
-- The aspect ratio may not be respected.
--
-- In the process, an image is converted to RGBA8. Therefore, some information
-- loss may occur.
resize :: Width -> Height -> DynamicImage -> DynamicImage
resize w h = ImageRGBA8 . (scaleBilinear w h) . convertRGBA8

-- | Compiler that resizes images to a specific dimensions. Aspect ratio
-- may not be preserved.
--
-- @
-- match "*.png" $ do
--     route idRoute
--     compile $ loadImage 
--         >>= resizeImageCompiler 48 64
-- @
--
-- Note that in the resizing process, images will be converted to RGBA8.
resizeImageCompiler :: Width -> Height -> Item Image -> Compiler (Item Image)
resizeImageCompiler w h item =
    let fmt = (format . itemBody) item
    in return $ (encode fmt . resize w h . decodeImage' . image) <$> item

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
--
-- @
-- match "*.tiff" $ do
--     route idRoute
--     compile $ loadImage 
--         >>= scaleImageCompiler 48 64
-- @
--
-- Note that in the resizing process, images will be converted to RGBA8.
scaleImageCompiler :: Width -> Height -> Item Image -> Compiler (Item Image)
scaleImageCompiler w h item =
    let fmt = (format . itemBody) item
    in return $ (encode fmt . scale w h . decodeImage' . image) <$> item