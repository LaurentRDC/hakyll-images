# hakyll-images

[![Hackage version](https://img.shields.io/hackage/v/hakyll-images.svg)](http://hackage.haskell.org/package/hakyll-images)[![Build status] (https://ci.appveyor.com/api/projects/status/kf12xsgrx1l26b3y?svg=true)](https://ci.appveyor.com/project/LaurentRDC/hakyll-images)

## A Haskell package containing utilities to deal with images in the context of Hakyll

[Hakyll](https://hackage.haskell.org/package/hakyll) is a static website compiler library. As one of the benefits of static websites is their small size, this repository aims at providing utilities to work with images in the context of Hakyll. Example usage includes:

* Re-encoding Jpeg images at a lower quality to make them much smaller;
* Re-sizing images to fit within a certain shape (work in-progress)

## Usage

`hakyll-images` is meant to be integrated within a Hakyll program. For example, to compress all Jpeg images present in your source:

```haskell
import Hakyll
import Hakyll.Images        (compressJpgCompiler)

(... omitted ...)

hakyll $ do

    (... omitted ...)

    -- Compress all source Jpegs to a Jpeg quality of 50 (maximum of 100)
    match "images/**.jpg" $ do
            route idRoute
            compile (compressJpgCompiler 50)

    (... omitted ...)
```

Take a look at the [documentation](hackage.haskell.org/package/hakyll-images) for more usage examples.

## Upcoming features

Here are the upcoming features of `hakyll-images`:

- [ ] Resize images to size

- [ ] Scale to fit within bounds

- [ ] Format conversion

If you would like a feature added, consider creating [an issue on Github](https://github.com/LaurentRDC/hakyll-images/issues/)

## Installation

### From Hackage

`hakyll-images` is available on [Hackage](https://hackage.haskell.org). Using the [`cabal-install`](https://www.haskell.org/cabal/) tool:

```bash
cabal update
cabal install hakyll-images
```

### From source

Building from source can be done using [`stack`](https://docs.haskellstack.org/en/stable/README/) or [`cabal`](https://www.haskell.org/cabal/):

```bash
git clone github.com/LaurentRDC/hakyll-images.git
cd hakyll-images
stack install # Alternatively, `cabal install`
```

## Documentation

The documentation for the latest release is available on the [Hackage page](http://hackage.haskell.org/package/hakyll-images/). 

## Support  Issues / Feature requests

All support requests (e.g. installation issues, unclear documentation, bugs, etc.) should of [filed on Github as an issue](https://github.com/LaurentRDC/hakyll-images/issues/)

## License

This package is made available under the BSD 3-clause license. For more details, see the [LICENSE.md](https://github.com/LaurentRDC/hakyll-images/blob/master/LICENSE.md)