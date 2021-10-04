# Change log

## Release 1.1.1

* Updated the test suite to support `Hakyll.Core.Runtime.RunMode` (added in hakyll 4.15) (#10).

## Release 1.1.0

* Added support for GIFs (#9).
* Releases are now automatically uploaded via Github Actions

## Release 1.0.1

* The module `Hakyll.Images.Metadata` is now exposed (#5).

## Release 1.0.0

* Updated dependency bounds.

## Release 0.4.4

* Added the `imageMetadata` compiler, to extract metadata from images.

## Release 0.4.3

* hakyll-images now handles file extensions in a case-insensitive manner (extension of PR #4).

## Release 0.4.2

* Added `ensureFitCompiler`, a Hakyll compiler much like `scaleImageCompiler` but that will only scale images down.

## Release 0.4.1

* Added some regression tests
* Simplified type architecture (no surface changes)

## Release 0.4

* Fixed an issue from version 0.3.1 where some type instances were missing to write images to disk.

## Release 0.3.1

* Change underlying image type to carry image format around.

## Release 0.3

* Refactored the internal structure to allow for composition of compilers

## Release 0.1.1

* Exposed `resizeImageCompiler` and `scaleImageCompiler` to the base `Hakyll.Images` module

## Release 0.1.0

* added `resizeImageCompiler` to resize images to a specific shape;
* added `scaleImageCompiler` to scale images while keeping aspect ratio.

## Release 0.0.1

* Added compressJpgCompiler to compress JPEGs.
