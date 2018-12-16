{-# LANGUAGE FlexibleContexts    #-}
{-|
Module      : Hakyll.Images.Resize
Description : Hakyll compiler to control image size
Copyright   : (c) Laurent P René de Cotret, 2018
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : stable
Portability : portable
-}
module Hakyll.Images.Resize (
      Width
    , Height
    , scaleImage
    , scaleImageToFit
) where

-- To represent scaling of pictures
import Data.Ratio                       ((%))

import Data.ByteString.Lazy             (ByteString, toStrict)
import qualified Data.ByteString        as SB
import Control.Monad                    (join)
import Codec.Picture                    (Image(..), readImage, decodeImage, encodeJpeg, DynamicImage)
import Codec.Picture.Types              (Pixel, PixelBaseComponent)
import Codec.Picture.Extra              (scaleBilinear)

import System.FilePath                  (takeExtension)
import Hakyll.Images.Utils              (imageToByteString)

import Hakyll.Core.Item                 (Item(..))
import Hakyll.Core.Compiler             (Compiler, getResourceLBS, getResourceFilePath, makeItem)
import Hakyll.Core.Compiler.Internal    (compilerProvider, compilerUnderlying, compilerAsk)

type Width = Int
type Height = Int
    
-- | Scale image to (width, height) using bilinear interpolation
scaleImage :: Width 
           -> Height 
           -> ByteString
           -> ByteString
scaleImage w h src = case im of
    Right dynImage -> (encodeJpeg . scaleBilinear w h) dynImage
    Left _         -> error $ "Scaling the image failed."
    where im = (decodeImage . toStrict) src

scaleImageCompiler :: Width -> Height -> Compiler (Item ByteString)
scaleImageCompiler w h = do
    fname <- getResourceFilePath
    let ext = takeExtension fname
    image <- fmap (decodeImage . toStrict) <$> getResourceLBS
    case itemBody image of
        Left msg -> error msg
        Right im -> do 
            join $ return $ (makeItem . imageToByteString ext) im


-- | Scale image so that it will fit in a box of (width, height) pixels.
-- Images that fit inside the box (width, height) are left unchanged, 
-- i.e. images are never made larger.
scaleImageToFit :: (Pixel a, Bounded (PixelBaseComponent a), Integral (PixelBaseComponent a)) 
                => Width 
                -> Height 
                -> Image a 
                -> Image a
scaleImageToFit maxWidth maxHeight im = scaleImage newWidth newHeight im
    where
        (imWidth, imHeight) = (imageWidth im, imageHeight im)
        -- Select the scaling that reduces either width or height by the most
        minScaling = minimum [imWidth % maxWidth, imHeight % maxHeight]
        -- We only want to resize to smaller size, therefore at most scaling by 1
        scaleFactor = maximum [1 % 1, minScaling]
        (newWidth, newHeight) = (round $ (toEnum imWidth * scaleFactor), round $ (toEnum imHeight * scaleFactor))