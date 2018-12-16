
{-|
Module      : Hakyll.Images
Description : Hakyll utilities for image file types
Copyright   : (c) Laurent P René de Cotret, 2018
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : stable
Portability : portable
-}
module Hakyll.Images (
    -- Jpg compression
      JpgQuality
    , compressJpg
    , compressJpgCompiler
    -- general image resizing
    -- , Width
    -- , Height
    -- , scaleImage
    -- , scaleImageToFit
) where

import Hakyll.Images.CompressJpg (compressJpg, compressJpgCompiler, JpgQuality)
-- import Hakyll.Images.Resize      (scaleImage, scaleImageToFit, Width, Height)