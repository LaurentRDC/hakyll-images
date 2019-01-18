
{-|
Module      : Hakyll.Images
Description : Hakyll utilities for image files
Copyright   : (c) Laurent P René de Cotret, 2019
License     : BSD3
Maintainer  : laurent.decotret@outlook.com
Stability   : stable
Portability : portable

This package defines a few Hakyll compilers. These compilers help deal with images
in the context of Hakyll programs, such as JPEG compression or image resizing.
-}
module Hakyll.Images (
    -- Basic types and functions
      Image
    , loadImage
    -- Jpg compression
    , JpgQuality
    , compressJpg
    , compressJpgCompiler
    -- Image scaling
    , Width, Height
    , resize
    , resizeImageCompiler
    , scale
    , scaleImageCompiler
) where

import Hakyll.Images.CompressJpg
import Hakyll.Images.Resize
import Hakyll.Images.Common