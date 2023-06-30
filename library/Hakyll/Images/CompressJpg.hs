{-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- |
-- Module      : Hakyll.Images.CompressJpg
-- Description : Hakyll compiler to compress Jpeg images
-- Copyright   : (c) Laurent P René de Cotret, 2019 - present
-- License     : BSD3
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : unstable
-- Portability : portable
--
-- This module defines a Hakyll compiler, 'compressJpgCompiler', which can be used to
-- re-encode Jpeg images at a lower quality during website compilation. Original images are
-- left unchanged, but compressed images can be up to 10x smaller.
--
-- The @compressJpgCompiler@ is expected to be used like this:
--
-- @
--     import Hakyll
--     import Hakyll.Images        ( loadImage
--                                 , compressJpgCompiler
--                                 )
--
--     hakyll $ do
--
--         -- Compress all source Jpegs to a Jpeg quality of 50
--         match "images/**.jpg" $ do
--             route idRoute
--             compile $ loadImage
--                 >>= compressJpgCompiler 50
--
--         (... omitted ...)
-- @
module Hakyll.Images.CompressJpg
  ( JpgQuality,
    compressJpgCompiler,
    compressJpg,
  )
where

import Codec.Picture.Types (DynamicImage(..), dropTransparency, pixelMap)
import Codec.Picture.Jpg (decodeJpegWithMetadata, encodeDirectJpegAtQualityWithMetadata)
import Data.ByteString.Lazy (toStrict)
import Hakyll.Core.Compiler (Compiler)
import Hakyll.Core.Item (Item (..))
import Hakyll.Images.Common
  ( Image (..),
    ImageFormat (..),
    format,
    image,
  )
import Numeric.Natural (Natural)

-- | Jpeg encoding quality, from 0 (lower quality) to 100 (best quality).
-- @since 1.2.0
newtype JpgQuality = JpgQuality Natural
  deriving (Num, Eq, Enum, Ord, Real, Integral)


-- | @JpgQuality@ smart constructor. Ensures that @JpgQuality@ is always
-- in the interval [0, 100]. Numbers outside this range will result in either
-- a quality of 0 or 100.
--
-- @since 1.2.0
mkJpgQuality :: Integral a => a -> JpgQuality
mkJpgQuality q | q < 0     = JpgQuality 0
               | q > 100   = JpgQuality 100
               | otherwise = JpgQuality (fromIntegral q)


-- | Compress a JPG bytestring to a certain quality setting.
-- The quality should be between 0 (lowest quality) and 100 (best quality).
-- An error is raised if the image cannot be decoded.
--
-- In the rare case where the JPEG data contains transparency information, it will be dropped.
compressJpg :: Integral a => a -> Image -> Image
compressJpg quality' src =
  if format src /= Jpeg
    then error "Image is not a JPEG."
    -- It is important to preserve metadata, such as orientation (issue #11).
    else case decodeJpegWithMetadata $ image src of
      Left msg -> error $ "Loading the image failed for the following reason: " <> msg
      Right (dynImage, meta) -> 
         Image Jpeg $ toStrict $ case dynImage of 
          (ImageY8 img)     -> (encodeDirectJpegAtQualityWithMetadata (fromIntegral quality) meta img)
          (ImageCMYK8 img)  -> (encodeDirectJpegAtQualityWithMetadata (fromIntegral quality) meta img)
          (ImageRGB8 img)   -> (encodeDirectJpegAtQualityWithMetadata (fromIntegral quality) meta img)
          (ImageYCbCr8 img) -> (encodeDirectJpegAtQualityWithMetadata (fromIntegral quality) meta img)
          -- Out of the 5 possible image types that can be returned by `decodeJpegWithMetadata`, only 1
          -- has transparency. This is also the only image type which cannot be re-encoded directly;
          -- we need to remove transparency.
          (ImageYA8 img)    -> (encodeDirectJpegAtQualityWithMetadata (fromIntegral quality) meta (pixelMap dropTransparency img))
          _ -> error "Loading the image failed because the color space is unknown." 
  where 
    quality = mkJpgQuality quality'


-- | Compiler that compresses a JPG image to a certain quality setting.
-- The quality should be between 0 (lowest quality) and 100 (best quality).
-- Values outside of this range will be normalized to the interval [0, 100].
-- An error is raised if the image cannot be decoded.
--
-- @
-- match "*.jpg" $ do
--     route idRoute
--     compile $ loadImage
--         >>= compressJpgCompiler 50
-- @
compressJpgCompiler :: Integral a => a -> Item Image -> Compiler (Item Image)
compressJpgCompiler quality = return . fmap (compressJpg quality)
