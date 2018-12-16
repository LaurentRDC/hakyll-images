
{-|
Module      : Hakyll.Images
Description : Hakyll utilities for image files
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
) where

import Hakyll.Images.CompressJpg (compressJpg, compressJpgCompiler, JpgQuality)