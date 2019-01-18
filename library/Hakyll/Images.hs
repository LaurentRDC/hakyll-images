
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

Items must be loaded before compilers can be used, like so:
@
    import Hakyll
    import Hakyll.Images        ( resizeImageCompiler )
    
    hakyll $ do

        -- Resize all profile pictures with .png extensions to 64x48
        match "profiles/**.png" $ do
            route idRoute
            compile $ loadImage
                >>= resizeImageCompiler 64 48
        
        (... omitted ...)
@

Compilers can be sequenced easily as well:

@
    import Hakyll
    import Hakyll.Images        ( compressJpgCompiler
                                , scaleImageCompiler 
                                )
    
    hakyll $ do

        -- Resize all JPEgs to fit inside of 800x600
        -- Also compress to a quality of 25/100
        match "pictures/**.jpg" $ do
            route idRoute
            compile $ loadImage
                >>= scaleImageCompiler 800 600
                >>= compressJpgCompiler 25
        
        (... omitted ...)
@

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