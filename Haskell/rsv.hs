import qualified Data.Text as T
import Data.Text.Encoding ( encodeUtf8Builder, decodeUtf8)
import Data.ByteString.Lazy (ByteString, split, toStrict, isSuffixOf)
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Builder (singleton, toLazyByteString, Builder)
import Data.Word (Word8)

rowTerminatorByte   = 0xFD :: Word8
nullValueByte       = 0xFE :: Word8
valueTerminatorByte = 0xFF :: Word8

encodeRSV :: [[Maybe T.Text]] -> ByteString
encodeRSV = toLazyByteString . encodeRows
  where
    encodeRows = mconcat . map encodeRow
    encodeRow ts = mconcat (map encodeValue ts) <> encodeRowEnd
    encodeValue t = maybe encodeNull encodeUtf8Builder t <> encodeValueEnd
    encodeRowEnd   = singleton rowTerminatorByte
    encodeValueEnd = singleton valueTerminatorByte
    encodeNull     = singleton nullValueByte

decodeRsvUsingSplit :: ByteString -> [[Maybe T.Text]]
decodeRsvUsingSplit = decodeRows
  where
    decodeRows = map decodeRow . findRows
    decodeRow  = map decodeValue . findValues
    decodeValue valueBS
      | valueBS == encodedNull = Nothing
      | otherwise              = Just $ decodeUtf8 $ toStrict valueBS
    encodedNull = LBS.singleton nullValueByte
    findRows   = separateTerminated rowTerminatorByte "row"
    findValues = separateTerminated valueTerminatorByte "value"
    separateTerminated terminator unitName bs
      | LBS.null bs               = []
      | LBS.last bs == terminator = init $ split terminator bs
      | otherwise                 = error $ "unterminated " ++ unitName
