# hakyll-images

[![Build status](https://ci.appveyor.com/api/projects/status/kf12xsgrx1l26b3y?svg=true)](https://ci.appveyor.com/project/LaurentRDC/hakyll-images)

## A Haskell package containing utilities to deal with images in the context of Hakyll

[Hakyll](https://hackage.haskell.org/package/hakyll) is a static website compiler library. As one of the benefits of static websites is their small size, this repository aims at providing utilities to work with images in the context of Hakyll. Example usage includes:

* Re-encoding Jpeg images at a lower quality to make them much smaller;
* Re-sizing images to fit within a certain shape

## Usage

`hakyll-images` is meant to be integrated within a Hakyll program. For example, to compress all Jpeg images present in your source:

```haskell
import Hakyll
import Hakyll.Images        (compressJpgCompiler)

(... omitted ...)

hakyll $ do

    (... omitted ...)

    -- Compress all source Jpegs to a Jpeg quality of 50
    match "files/*.jpg" $ do
            route idRoute
            compilte (compressJpgCompiler) 50

    (... omitted ...)
```

Take a look at the [documentation](hackage.haskell.org/package/hakyll-images) for more usage examples.

## Installation

`hakyll-images` is available on Hackage. Using the [`cabal-install`](https://www.haskell.org/cabal/) tool:

```bash
cabal update
cabal install hakyll-images
```