
module Hakyll.Images.Utils (
    imageToByteString
) where

import Data.ByteString.Lazy             (ByteString)
import Codec.Picture                    (DynamicImage)
import Codec.Picture.Saving  

-- | Encode an image to a bytestring based on an extension.
-- Will throw an exception on unknown extensions
imageToByteString :: String
                  -> DynamicImage
                  -> ByteString
imageToByteString ext = case ext of
    ".jpg"  -> imageToJpg 100
    ".jpeg" -> imageToJpg 100
    ".png"  -> imageToPng
    -- "gif"  -> imageToGif
    ".bmp"  -> imageToBitmap
    ".tif"  -> imageToBitmap
    ".tiff" -> imageToTiff
    _ -> error ("Unknown extensions " ++ ext)
