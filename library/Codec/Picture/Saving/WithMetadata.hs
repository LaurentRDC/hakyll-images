{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Helper functions to save dynamic images to other file format,
-- while preserving metadata when possible.
--
-- This module is modified from `Codec.Picture.Saving` from JuicyPixels
module Codec.Picture.Saving.WithMetadata
  ( imageToJpgWithMetadata,
    imageToPngWithMetadata,
    imageToBitmapWithMetadata,
  )
where

import Codec.Picture.Bitmap (encodeBitmapWithMetadata)
import Codec.Picture.Jpg (encodeDirectJpegAtQualityWithMetadata, encodeJpegAtQualityWithMetadata)
import Codec.Picture.Metadata (Metadatas)
import Codec.Picture.Png (PngSavable (encodePngWithMetadata))
import Codec.Picture.Types
  ( ColorConvertible (promoteImage),
    ColorSpaceConvertible (convertImage),
    DynamicImage (..),
    Image (..),
    Pixel (PixelBaseComponent),
    Pixel8,
    PixelF,
    PixelRGB16,
    PixelRGB8 (..),
    PixelRGBA8,
    PixelRGBF (..),
    dropAlphaLayer,
    pixelMap,
  )
import Data.Bits (unsafeShiftR)
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Storable as V
import Data.Word (Word16, Word32, Word8)

componentToLDR :: Float -> Word8
componentToLDR = truncate . (255 *) . min 1.0 . max 0.0

toStandardDef :: Image PixelRGBF -> Image PixelRGB8
toStandardDef = pixelMap pixelConverter
  where
    pixelConverter (PixelRGBF rf gf bf) = PixelRGB8 r g b
      where
        r = componentToLDR rf
        g = componentToLDR gf
        b = componentToLDR bf

greyScaleToStandardDef :: Image PixelF -> Image Pixel8
greyScaleToStandardDef = pixelMap componentToLDR

from16to8 ::
  ( PixelBaseComponent source ~ Word16,
    PixelBaseComponent dest ~ Word8
  ) =>
  Image source ->
  Image dest
from16to8
  Image
    { imageWidth = w,
      imageHeight = h,
      imageData = arr
    } = Image w h transformed
    where
      transformed = V.map toWord8 arr
      toWord8 v = fromIntegral (v `unsafeShiftR` 8)

from32to8 ::
  ( PixelBaseComponent source ~ Word32,
    PixelBaseComponent dest ~ Word8
  ) =>
  Image source ->
  Image dest
from32to8
  Image
    { imageWidth = w,
      imageHeight = h,
      imageData = arr
    } = Image w h transformed
    where
      transformed = V.map toWord8 arr
      toWord8 v = fromIntegral (v `unsafeShiftR` 24)

from32to16 ::
  ( PixelBaseComponent source ~ Word32,
    PixelBaseComponent dest ~ Word16
  ) =>
  Image source ->
  Image dest
from32to16
  Image
    { imageWidth = w,
      imageHeight = h,
      imageData = arr
    } = Image w h transformed
    where
      transformed = V.map toWord16 arr
      toWord16 v = fromIntegral (v `unsafeShiftR` 16)

-- | This function will try to do anything to encode an image
-- as JPEG, make all color conversion and such. Equivalent
-- of 'decodeImage' for jpeg encoding
-- Save Y or YCbCr Jpeg only, all other colorspaces are converted.
-- To save a RGB or CMYK JPEG file, use the
-- 'Codec.Picture.Jpg.Internal.encodeDirectJpegAtQualityWithMetadata' function
imageToJpgWithMetadata :: Int -> Metadatas -> DynamicImage -> L.ByteString
imageToJpgWithMetadata quality meta dynImage =
  let encodeAtQuality = encodeJpegAtQualityWithMetadata (fromIntegral quality) meta
      encodeWithMeta = encodeDirectJpegAtQualityWithMetadata (fromIntegral quality) meta
   in case dynImage of
        ImageYCbCr8 img -> encodeAtQuality img
        ImageCMYK8 img -> imageToJpgWithMetadata quality meta . ImageRGB8 $ convertImage img
        ImageCMYK16 img -> imageToJpgWithMetadata quality meta . ImageRGB16 $ convertImage img
        ImageRGB8 img -> encodeAtQuality (convertImage img)
        ImageRGBF img -> imageToJpgWithMetadata quality meta . ImageRGB8 $ toStandardDef img
        ImageRGBA8 img -> encodeAtQuality (convertImage $ dropAlphaLayer img)
        ImageYF img -> imageToJpgWithMetadata quality meta . ImageY8 $ greyScaleToStandardDef img
        ImageY8 img -> encodeWithMeta img
        ImageYA8 img -> encodeWithMeta $ dropAlphaLayer img
        ImageY16 img -> imageToJpgWithMetadata quality meta . ImageY8 $ from16to8 img
        ImageYA16 img -> imageToJpgWithMetadata quality meta . ImageYA8 $ from16to8 img
        ImageY32 img -> imageToJpgWithMetadata quality meta . ImageY8 $ from32to8 img
        ImageRGB16 img -> imageToJpgWithMetadata quality meta . ImageRGB8 $ from16to8 img
        ImageRGBA16 img -> imageToJpgWithMetadata quality meta . ImageRGBA8 $ from16to8 img

-- | This function will try to do anything to encode an image
-- as PNG, make all color conversion and such. Equivalent
-- of 'decodeImage' for PNG encoding
imageToPngWithMetadata :: Metadatas -> DynamicImage -> L.ByteString
imageToPngWithMetadata meta (ImageYCbCr8 img) = encodePngWithMetadata meta (convertImage img :: Image PixelRGB8)
imageToPngWithMetadata meta (ImageCMYK8 img) = encodePngWithMetadata meta (convertImage img :: Image PixelRGB8)
imageToPngWithMetadata meta (ImageCMYK16 img) = encodePngWithMetadata meta (convertImage img :: Image PixelRGB16)
imageToPngWithMetadata meta (ImageRGB8 img) = encodePngWithMetadata meta img
imageToPngWithMetadata meta (ImageRGBF img) = encodePngWithMetadata meta $ toStandardDef img
imageToPngWithMetadata meta (ImageRGBA8 img) = encodePngWithMetadata meta img
imageToPngWithMetadata meta (ImageY8 img) = encodePngWithMetadata meta img
imageToPngWithMetadata meta (ImageYF img) = encodePngWithMetadata meta $ greyScaleToStandardDef img
imageToPngWithMetadata meta (ImageYA8 img) = encodePngWithMetadata meta img
imageToPngWithMetadata meta (ImageY16 img) = encodePngWithMetadata meta img
imageToPngWithMetadata meta (ImageY32 img) = imageToPngWithMetadata meta . ImageY16 $ from32to16 img
imageToPngWithMetadata meta (ImageYA16 img) = encodePngWithMetadata meta img
imageToPngWithMetadata meta (ImageRGB16 img) = encodePngWithMetadata meta img
imageToPngWithMetadata meta (ImageRGBA16 img) = encodePngWithMetadata meta img

-- | This function will try to do anything to encode an image
-- as bitmap, make all color conversion and such. Equivalent
-- of 'decodeImage' for Bitmap encoding
imageToBitmapWithMetadata :: Metadatas -> DynamicImage -> L.ByteString
imageToBitmapWithMetadata meta (ImageYCbCr8 img) = encodeBitmapWithMetadata meta (convertImage img :: Image PixelRGB8)
imageToBitmapWithMetadata meta (ImageCMYK8 img) = encodeBitmapWithMetadata meta (convertImage img :: Image PixelRGB8)
imageToBitmapWithMetadata meta (ImageCMYK16 img) = imageToBitmapWithMetadata meta . ImageRGB16 $ convertImage img
imageToBitmapWithMetadata meta (ImageRGBF img) = encodeBitmapWithMetadata meta $ toStandardDef img
imageToBitmapWithMetadata meta (ImageRGB8 img) = encodeBitmapWithMetadata meta img
imageToBitmapWithMetadata meta (ImageRGBA8 img) = encodeBitmapWithMetadata meta img
imageToBitmapWithMetadata meta (ImageY8 img) = encodeBitmapWithMetadata meta img
imageToBitmapWithMetadata meta (ImageYF img) = encodeBitmapWithMetadata meta $ greyScaleToStandardDef img
imageToBitmapWithMetadata meta (ImageYA8 img) = encodeBitmapWithMetadata meta (promoteImage img :: Image PixelRGBA8)
imageToBitmapWithMetadata meta (ImageY16 img) = imageToBitmapWithMetadata meta . ImageY8 $ from16to8 img
imageToBitmapWithMetadata meta (ImageY32 img) = imageToBitmapWithMetadata meta . ImageY8 $ from32to8 img
imageToBitmapWithMetadata meta (ImageYA16 img) = imageToBitmapWithMetadata meta . ImageYA8 $ from16to8 img
imageToBitmapWithMetadata meta (ImageRGB16 img) = imageToBitmapWithMetadata meta . ImageRGB8 $ from16to8 img
imageToBitmapWithMetadata meta (ImageRGBA16 img) = imageToBitmapWithMetadata meta . ImageRGBA8 $ from16to8 img
