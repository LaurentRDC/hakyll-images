
{-|
Module      : Hakyll.Images
Description : Hakyll utilities for image files
Copyright   : (c) Laurent P René de Cotret, 2018
License     : BSD3
Maintainer  : laurent.decotret@outlook.com
Stability   : stable
Portability : portable
-}
module Hakyll.Images (
    -- Jpg compression
      JpgQuality
    , compressJpg
    , compressJpgCompiler
    -- Image scaling
    , Width, Height
    , resize
    , scale
) where

import Hakyll.Images.CompressJpg
import Hakyll.Images.Rescale