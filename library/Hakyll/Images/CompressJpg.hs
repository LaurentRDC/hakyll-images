
{-|
Module      : Hakyll.Images.CompressJpg
Description : Hakyll compiler to compress Jpeg images
Copyright   : (c) Laurent P René de Cotret, 2019
License     : BSD3
Maintainer  : laurent.decotret@outlook.com
Stability   : stable
Portability : portable

This module defines a Hakyll compiler, 'compressJpgCompiler', which can be used to
re-encode Jpeg images at a lower quality during website compilation. Original images are
left unchanged, but compressed images can be up to 10x smaller.

The @compressJpgCompiler@ is expected to be used like this:

@
    import Hakyll
    import Hakyll.Images        ( loadImage
                                , compressJpgCompiler
                                )
    
    hakyll $ do

        -- Compress all source Jpegs to a Jpeg quality of 50
        match "images/**.jpg" $ do
            route idRoute
            compile $ loadImage
                >>= compressJpgCompiler 50
        
        (... omitted ...)
@
-}
module Hakyll.Images.CompressJpg
    ( JpgQuality
    , compressJpgCompiler
    , compressJpg
    ) where

import Data.ByteString.Lazy             (toStrict)

import Codec.Picture.Jpg                (decodeJpeg)
import Codec.Picture.Saving             (imageToJpg)

import Hakyll.Core.Item                 (Item(..))
import Hakyll.Core.Compiler             (Compiler)

import Hakyll.Images.Common             ( Image
                                        , Image_(..)
                                        , ImageFormat(..)
                                        , image
                                        , format
                                        )


-- | Jpeg encoding quality, from 0 (lower quality) to 100 (best quality).
type JpgQuality = Int


-- | Compress a JPG bytestring to a certain quality setting.
-- The quality should be between 0 (lowest quality) and 100 (best quality).
-- An error is raised if the image cannot be decoded, or if the 
-- encoding quality is out-of-bounds
compressJpg :: JpgQuality -> Image -> Image
compressJpg quality src = if (format src) /= Jpeg
        then error $ "Image is not a JPEG."
        else 
            case decodeJpeg $ image src of
                Left _         -> error $ "Loading the image failed."
                Right dynImage -> 
                    if (quality < 0 || quality > 100)
                        then error $ "JPEG encoding quality should be between 0 and 100."
                        else Image Jpeg $ (toStrict $ imageToJpg quality dynImage)

-- | Compiler that compresses a JPG image to a certain quality setting.
-- The quality should be between 0 (lowest quality) and 100 (best quality).
-- An error is raised if the image cannot be decoded.
--
-- @
-- match "*.jpg" $ do
--     route idRoute
--     compile $ loadImage 
--         >>= compressJpgCompiler 50
-- @
compressJpgCompiler :: JpgQuality -> Item Image -> Compiler (Item Image)
compressJpgCompiler quality item = return $ compressJpg quality <$> item 