module Cardano.Api.MetaData
  (
    -- * Transaction metadata type
    TxMetadata(..)

    -- * Constructing metadata
  , makeTransactionMetadata
  , TxMetadataValue(..)

    -- * Validating metadata
  , validateTxMetadata
  , TxMetadataValidationError (..)

    -- * Converstion to\/from JSON
  , TxMetadataJsonMapping (..)
  , metadataFromJson
  , metadataToJson
  , TxMetadataJsonConversionError (..)
  ) where

import           Prelude

import           Data.Maybe (fromMaybe)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.Scientific as Scientific
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Text.Lazy
import           Data.Word (Word64)

import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson.Text
import qualified Data.Attoparsec.ByteString.Char8 as Atto

import           Control.Monad (guard)
import           Control.Applicative (Alternative(..))

import           Cardano.Api.Typed


-- | Validate transaction metadata. This is for use with existing constructed
-- metadata values, e.g. constructed manually or decoded from CBOR directly.
--
validateTxMetadata :: TxMetadata
                   -> Either (NonEmpty TxMetadataValidationError) TxMetadata
validateTxMetadata txMd@(TxMetadata mdMap) =
    -- Collect all errors and do a top-level check to see if there are any.
    maybe (Right txMd) Left
  . nonEmpty
  . foldMap validateTxMetadataValue
  $ mdMap

-- collect all errors in a monoidal fold style
validateTxMetadataValue :: TxMetadataValue -> [TxMetadataValidationError]
validateTxMetadataValue (TxMetaNumber n) =
    [ TxMetadataNumberOutOfRange n
    |    n >         fromIntegral (maxBound :: Word64)
      || n < negate (fromIntegral (maxBound :: Word64))
    ]
validateTxMetadataValue (TxMetaBytes bs) =
    [ TxMetadataBytesTooLong len
    | let len = BS.length bs
    , len > txMetadataByteStringMaxLength
    ]
validateTxMetadataValue (TxMetaText txt) =
    [ TxMetadataTextTooLong len
    | let len = BS.length (Text.encodeUtf8 txt)
    , len > txMetadataTextStringMaxByteLength
    ]
validateTxMetadataValue (TxMetaList xs) =
    foldMap validateTxMetadataValue xs

validateTxMetadataValue (TxMetaMap kvs) =
    foldMap (\(k, v) -> validateTxMetadataValue k
                     <> validateTxMetadataValue v)
            kvs

-- | The maximum byte length of a transaction metadata text string value.
txMetadataTextStringMaxByteLength :: Int
txMetadataTextStringMaxByteLength = 64

-- | The maximum length of a transaction metadata byte string value.
txMetadataByteStringMaxLength :: Int
txMetadataByteStringMaxLength = 64


-- | A transaction metadata validation error.
--
data TxMetadataValidationError =

    -- | The number is outside the maximum range of @-2^64-1 .. 2^64-1@.
    --
    TxMetadataNumberOutOfRange !Integer

    -- | The length of a text string metadatum value exceeds the maximum of
    -- 64 bytes as UTF8.
    --
  | TxMetadataTextTooLong !Int

    -- | The length of a byte string metadatum value exceeds the maximum of
    -- 64 bytes.
    --
  | TxMetadataBytesTooLong !Int
  deriving (Eq, Show)

instance Error TxMetadataValidationError where
  displayError (TxMetadataNumberOutOfRange n) =
      "Numeric metadata value "
        <> show n
        <> " is outside the range -(2^64-1) .. 2^64-1."
  displayError (TxMetadataTextTooLong actualLen) =
      "Text string metadata value must consist of at most "
        <> show txMetadataTextStringMaxByteLength
        <> " UTF8 bytes, but it consists of "
        <> show actualLen
        <> " bytes."
  displayError (TxMetadataBytesTooLong actualLen) =
      "Byte string metadata value must consist of at most "
        <> show txMetadataByteStringMaxLength
        <> " bytes, but it consists of "
        <> show actualLen
        <> " bytes."


-- ----------------------------------------------------------------------------
-- JSON conversion
--

-- | Tx metadata is similar to JSON but not exactly the same. It has some
-- deliberate limitations such as no support for floating point numbers or
-- special forms for null or boolean values. It also has limitations on the
-- length of strings. On the other hand, unlike JSON, it distinguishes between
-- byte strings and text strings. It also supports any value as map keys rather
-- than just string.
--
-- We provide two different mappings between tx metadata and JSON, useful
-- for different purposes:
--
-- 1. A mapping that allows almost any JSON value to be converted into
--    tx metadata. This does not require a specific JSON schema for the
--    input. It does not expose the full representation capability of tx
--    metadata.
--
-- 2. A mapping that exposes the full representation capability of tx
--    metadata, but relies on a specific JSON schema for the input JSON.
--
-- In the \"no schema"\ mapping, the idea is that (almost) any JSON can be
-- turned into tx metadata and then converted back, without loss. That is, we
-- can round-trip the JSON.
--
-- The subset of JSON supported is all JSON except:
-- * No null or bool values
-- * No floating point, only integers in the range of a 64bit signed integer
-- * A limitation on string lengths
--
-- The approach for this mapping is to use whichever representation as tx
-- metadata is most compact. In particular:
--
-- * JSON lists and maps represented as CBOR lists and maps
-- * JSON strings represented as CBOR strings
-- * JSON hex strings with \"0x\" prefix represented as CBOR byte strings
-- * JSON integer numbers represented as CBOR signed or unsigned numbers
-- * JSON maps with string keys that parse as numbers or hex byte strings,
--   represented as CBOR map keys that are actually numbers or byte strings.
--
-- The string length limit depends on whether the hex string representation
-- is used or not. For text strings the limit is 64 bytes for the UTF8
-- representation of the text string. For byte strings the limit is 64 bytes
-- for the raw byte form (ie not the input hex, but after hex decoding).
--
-- In the \"detailed schema\" mapping, the idea is that we expose the full
-- representation capability of the tx metadata in the form of a JSON schema.
-- This means the full representation is available and can be controlled
-- precisely. It also means any tx metadata can be converted into the JSON and
-- back without loss. That is we can round-trip the tx metadata via the JSON and
-- also round-trip schema-compliant JSON via tx metadata.
--
data TxMetadataJsonMapping =

       -- | Use the \"no schema\" mapping between JSON and tx metadata as
       -- described above.
       TxMetadataJsonNoSchema

       -- | Use the \"detailed schema\" mapping between JSON and tx metadata as
       -- described above.
     | TxMetadataJsonDetailedSchema


-- | Convert a value from JSON into tx metadata, using the given choice of
-- mapping between JSON and tx metadata.
--
-- This may fail with a conversion error if the JSON is outside the supported
-- subset for the chosen mapping. See 'TxMetadataJsonMapping' for the details.
--
metadataFromJson :: TxMetadataJsonMapping
                 -> Aeson.Value
                 -> Either TxMetadataJsonConversionError TxMetadata
metadataFromJson TxMetadataJsonNoSchema       = metadataFromJsonNoSchema
metadataFromJson TxMetadataJsonDetailedSchema = metadataFromJsonDetailedSchema


-- | Convert a tx metadata value into JSON , using the given choice of mapping
-- between JSON and tx metadata.
--
-- This conversion is total but is not necessarily invertible.
-- See 'TxMetadataJsonMapping' for the details.
--
metadataToJson :: TxMetadataJsonMapping
               -> TxMetadata
               -> Aeson.Value
metadataToJson TxMetadataJsonNoSchema       = metadataToJsonNoSchema
metadataToJson TxMetadataJsonDetailedSchema = metadataToJsonDetailedSchema


-- ----------------------------------------------------------------------------
-- JSON conversion using the the "no schema" style
--

metadataToJsonNoSchema :: TxMetadata -> Aeson.Value
metadataToJsonNoSchema (TxMetadata mdMap) =
    Aeson.object
      [ (Text.pack (show k), conv v)
      | (k, v) <- Map.toList mdMap ]
  where
    conv :: TxMetadataValue -> Aeson.Value
    conv (TxMetaNumber n) = Aeson.Number (fromInteger n)
    conv (TxMetaBytes bs) = Aeson.String (bytesPrefix
                                       <> Text.decodeLatin1 (Base16.encode bs))

    conv (TxMetaText txt) = Aeson.String txt
    conv (TxMetaList  vs) = Aeson.Array (Vector.fromList (map conv vs))
    conv (TxMetaMap  kvs) = Aeson.object
                              [ (convKey k, conv v)
                              | (k, v) <- kvs ]

    -- Metadata allows any value as a key, not just string as JSON does.
    -- For simple types we just convert them to string dirctly.
    -- For structured keys we render them as JSON and use that as the string.
    convKey :: TxMetadataValue -> Text
    convKey (TxMetaNumber n) = Text.pack (show n)
    convKey (TxMetaBytes bs) = bytesPrefix
                            <> Text.decodeLatin1 (Base16.encode bs)
    convKey (TxMetaText txt) = txt
    convKey v                = Text.Lazy.toStrict
                             . Aeson.Text.encodeToLazyText
                             . conv
                             $ v

metadataFromJsonNoSchema :: Aeson.Value
                         -> Either TxMetadataJsonConversionError
                                   TxMetadata
metadataFromJsonNoSchema vtop =
    case vtop of
      -- The top level has to be an object
      -- with unsigned integer (decimal or hex) keys
      Aeson.Object m ->
          fmap (TxMetadata . Map.fromList)
        . mapM (\(k,v) -> (,) <$> parseUnsigned k
                              <*> (validate =<< conv v))
        $ HashMap.toList m

      _ -> Left ConversionErrToplevelNotMap
  where
    parseUnsigned :: Text -> Either TxMetadataJsonConversionError Word64
    parseUnsigned = maybe (Left ConversionErrToplevelBadKey) Right
                  . parseAll pUnsigned

    validate :: TxMetadataValue
             -> Either TxMetadataJsonConversionError TxMetadataValue
    validate v = case validateTxMetadataValue v of
                   []      -> Right v
                   err : _ -> Left (ConversionErrRange err)

    conv :: Aeson.Value
         -> Either TxMetadataJsonConversionError TxMetadataValue
    conv Aeson.Null   = Left ConversionErrNullNotAllowed
    conv Aeson.Bool{} = Left ConversionErrBoolNotAllowed

    conv (Aeson.Number d) =
      case Scientific.floatingOrInteger d :: Either Double Integer of
        Left  n -> Left (ConversionErrNumberNotInteger n)
        Right n -> Right (TxMetaNumber n)

    conv (Aeson.String s)
      | Just s' <- Text.stripPrefix bytesPrefix s
      , let bs' = Text.encodeUtf8 s'
      , (bs, trailing) <- Base16.decode bs'
      , BS.null trailing
      , not (BS.any (\c -> c >= 'A' && c <= 'F') bs')
      = Right (TxMetaBytes bs)

    conv (Aeson.String s) = Right (TxMetaText s)

    conv (Aeson.Array vs) =
        fmap TxMetaList
      . traverse conv
      $ Vector.toList vs

    conv (Aeson.Object kvs) =
        fmap TxMetaMap
      . traverse (\(k,v) -> (,) (convKey k) <$> conv v)
      . List.sortOn fst
      $ HashMap.toList kvs

    convKey :: Text -> TxMetadataValue
    convKey s =
      fromMaybe (TxMetaText s) $
      parseAll ((fmap TxMetaNumber pSigned <* Atto.endOfInput)
            <|> (fmap TxMetaBytes  pBytes  <* Atto.endOfInput)) s

-- | JSON strings that are base16 encoded and prefixed with 'bytesPrefix' will
-- be encoded as CBOR bytestrings.
bytesPrefix :: Text
bytesPrefix = "0x"


-- ----------------------------------------------------------------------------
-- JSON conversion using the "detailed schema" style
--

metadataToJsonDetailedSchema :: TxMetadata -> Aeson.Value
metadataToJsonDetailedSchema (TxMetadata mdMap) =
    Aeson.object
      [ (Text.pack (show k), conv v)
      | (k, v) <- Map.toList mdMap ]
  where
    conv :: TxMetadataValue -> Aeson.Value
    conv (TxMetaNumber n) = singleFieldObject "int"
                          . Aeson.Number
                          $ fromInteger n
    conv (TxMetaBytes bs) = singleFieldObject "bytes"
                          . Aeson.String
                          $ Text.decodeLatin1 (Base16.encode bs)
    conv (TxMetaText txt) = singleFieldObject "string"
                          . Aeson.String
                          $ txt
    conv (TxMetaList  vs) = singleFieldObject "list"
                          . Aeson.Array
                          $ Vector.fromList (map conv vs)
    conv (TxMetaMap  kvs) = singleFieldObject "map"
                          . Aeson.Array
                          $ Vector.fromList
                              [ Aeson.object [ ("k", conv k), ("v", conv v) ]
                              | (k, v) <- kvs ]

    singleFieldObject name v = Aeson.object [(name, v)]

metadataFromJsonDetailedSchema :: Aeson.Value
                               -> Either TxMetadataJsonConversionError
                                         TxMetadata
metadataFromJsonDetailedSchema vtop =
    case vtop of
      -- The top level has to be an object
      -- with unsigned integer (decimal or hex) keys
      Aeson.Object m ->
          fmap (TxMetadata . Map.fromList)
        . mapM (\(k,v) -> (,) <$> parseUnsigned k
                              <*> (validate =<< conv v))
        $ HashMap.toList m

      _ -> Left ConversionErrToplevelNotMap
  where
    parseUnsigned :: Text -> Either TxMetadataJsonConversionError Word64
    parseUnsigned = maybe (Left ConversionErrToplevelBadKey) Right
                  . parseAll pUnsigned

    validate :: TxMetadataValue
             -> Either TxMetadataJsonConversionError TxMetadataValue
    validate v = case validateTxMetadataValue v of
                   []      -> Right v
                   err : _ -> Left (ConversionErrRange err)

    conv :: Aeson.Value
         -> Either TxMetadataJsonConversionError TxMetadataValue
    conv (Aeson.Object m) =
      case HashMap.toList m of
        [("int", Aeson.Number d)] ->
          case Scientific.floatingOrInteger d :: Either Double Integer of
            Left  n -> Left (ConversionErrNumberNotInteger n)
            Right n -> Right (TxMetaNumber n)

        [("bytes", Aeson.String s)]
          | (bs, trailing) <- Base16.decode (Text.encodeUtf8 s)
          , BS.null trailing -> Right (TxMetaBytes bs)

        [("string", Aeson.String s)] -> Right (TxMetaText s)

        [("list", Aeson.Array vs)] ->
            fmap TxMetaList
          . traverse conv
          $ Vector.toList vs

        [("map", Aeson.Array kvs)] ->
            fmap TxMetaMap
          . traverse convKeyValuePair
          $ Vector.toList kvs

        _ -> Left ConversionErrSchemaError

    conv _ = Left ConversionErrSchemaError

    convKeyValuePair :: Aeson.Value
                     -> Either TxMetadataJsonConversionError
                               (TxMetadataValue, TxMetadataValue)
    convKeyValuePair (Aeson.Object m)
      | HashMap.size m == 2
      , Just k <- m HashMap.!? "k"
      , Just v <- m HashMap.!? "v"
      = (,) <$> conv k <*> conv v

    convKeyValuePair _ = Left ConversionErrSchemaError


data TxMetadataJsonConversionError =
       ConversionErrToplevelNotMap
     | ConversionErrToplevelBadKey
     | ConversionErrNullNotAllowed
     | ConversionErrBoolNotAllowed
     | ConversionErrNumberNotInteger !Double
     | ConversionErrSchemaError
     | ConversionErrRange TxMetadataValidationError
  deriving (Eq, Show)


-- ----------------------------------------------------------------------------
-- Shared parsing utils
--

parseAll :: Atto.Parser a -> Text -> Maybe a
parseAll p = either (const Nothing) Just
           . Atto.parseOnly p
           . Text.encodeUtf8

pUnsigned :: Atto.Parser Word64
pUnsigned = (Atto.decimal     <* Atto.endOfInput)
        <|> (Atto.hexadecimal <* Atto.endOfInput)

pSigned :: Atto.Parser Integer
pSigned = Atto.signed Atto.decimal

pBytes :: Atto.Parser ByteString
pBytes = do
  _ <- Atto.string "0x"
  remaining <- Atto.takeByteString
  let (bs, trailing) = Base16.decode remaining
      hexUpper c = c >= 'A' && c <= 'F'
  guard (BS.null trailing && not (BS.any hexUpper remaining))
  return bs

