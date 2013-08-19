{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Conduit
import Data.Maybe
import Data.Word
import System.IO (stdout)
import qualified Data.Conduit.Binary    as CB
import qualified Data.Binary.Get        as G
import qualified Data.ByteString        as B
import qualified Data.Text              as T


data FileRecord =
  FileRecord
  { recordHeader :: !FileRecordHeader
  , recordAttributeList :: ![FileAttribute]
  }
  deriving (Show, Read, Eq)


data FileRecordHeader =
  FileRecordHeader
  { recordLogFileSeqNum :: !Word64
  , recordHardLinkCount :: !Word16
  , recordFlags         :: !Word16
  , recordRealSize      :: !Word32
  , recordAllocSize     :: !Word32
  , recordRefBaseFile   :: !Word64
  , recordNextAttrId    :: !Word16
  , recordNum           :: !Word32
  , recordUpdSeq        :: ![Word16]
  }
  deriving (Show, Read, Eq)


data FileAttribute =
  FileAttribute
  { attrType    :: !AttributeType
  , attrLength  :: !Word32
  , attrFlags   :: !Word16
  , attrId      :: !Word16
  , attrContent :: !FileAttributeContent
  , attrName    :: !T.Text
  }
  deriving (Show, Read, Eq)


data FileAttributeContent
  = FileAttributeResidentContent
    { attrconIsIndexed :: !Bool
    , attrconValue     :: !B.ByteString
    }
  | FileAttributeNonResidentContent
    { attrconStartVCN        :: !Word64
    , attrconLastVCN         :: !Word64
    , attrconCompUnitSize    :: !Word16
    , attrconAllocSize       :: !Word64
    , attrconRealSize        :: !Word64
    , attrconInitializedSize :: !Word64
    , attrconDataRuns        :: ![DataRun]
    }
  deriving (Show, Read, Eq)

data AttributeType
  = AttrTypeStandardInformation
  | AttrTypeAttributeList
  | AttrTypeFileName
  | AttrTypeVolumeVersion
  | AttrTypeObjectId
  | AttrTypeSecurityDescriptor
  | AttrTypeVolumeName
  | AttrTypeVolumeInformation
  | AttrTypeData
  | AttrTypeIndexRoot
  | AttrTypeIndexAllocation
  | AttrTypeBitmap
  | AttrTypeSymbolicLink
  | AttrTypeReparsePoint
  | AttrTypeEaInformation
  | AttrTypeEa
  | AttrTypePropertySet
  | AttrTypeLoggedUtilityStream
  deriving (Show, Read, Eq, Ord, Enum)


type DataRun = (Int, Int)


-- toAttributeType isLaterThanNT value
toAttributeType :: Bool -> Word32 -> Maybe AttributeType
toAttributeType _      0x10 = Just AttrTypeStandardInformation
toAttributeType _      0x20 = Just AttrTypeAttributeList
toAttributeType _      0x30 = Just AttrTypeFileName
toAttributeType False  0x40 = Just AttrTypeVolumeVersion
toAttributeType True   0x40 = Just AttrTypeObjectId
toAttributeType _      0x50 = Just AttrTypeSecurityDescriptor
toAttributeType _      0x60 = Just AttrTypeVolumeName
toAttributeType _      0x70 = Just AttrTypeVolumeInformation
toAttributeType _      0x80 = Just AttrTypeData
toAttributeType _      0x90 = Just AttrTypeIndexRoot
toAttributeType _      0xA0 = Just AttrTypeIndexAllocation
toAttributeType _      0xB0 = Just AttrTypeBitmap
toAttributeType False  0xC0 = Just AttrTypeSymbolicLink
toAttributeType True   0xC0 = Just AttrTypeReparsePoint
toAttributeType _      0xD0 = Just AttrTypeEaInformation
toAttributeType _      0xE0 = Just AttrTypeEa
toAttributeType False  0xF0 = Just AttrTypePropertySet
toAttributeType True  0x100 = Just AttrTypeLoggedUtilityStream
toAttributeType _         _ = Nothing


bsToIntBE :: B.ByteString -> Int
bsToIntBE = B.foldl' (\x y -> x `shiftL` 8 .|. (fromIntegral y)) 0


bsToIntLE :: B.ByteString -> Int
bsToIntLE = B.foldr' (\x y -> (fromIntegral x) .|. y `shiftL` 8) 0


getConditional :: (Show a, Monad m) => m a -> (a -> Bool) -> m a
getConditional parser cond = do
  x <- parser
  if cond x then return x else fail ("Unmet condition: " ++ show x)


getList :: Monad m => m a -> Int -> m [a]
getList = flip replicateM


getListMaybe :: Monad m => m (Maybe a) -> m [a]
getListMaybe parser = do
  x <- parser
  case x of
    Just x' -> getListMaybe parser >>= return . (x' :)
    Nothing -> return $ []


getFileRecord :: G.Get FileRecord
getFileRecord = do
  header     <- getFileRecordHeader
  attributes <- getAttributeList
  return $ FileRecord header attributes


getFileRecordHeader :: G.Get FileRecordHeader
getFileRecordHeader = do
  magic <- getConditional (G.getByteString 4) (== "FILE")
  offUpdSeq     <- G.getWord16le
  sizeUpdSeqArr <- G.getWord16le
  logSeqNum     <- G.getWord64le
  seqNum        <- G.getWord16le
  refCount      <- G.getWord16le
  offAttr       <- G.getWord16le
  flags         <- G.getWord16le
  realSize      <- G.getWord32le
  allocSize     <- G.getWord32le
  refBaseFile   <- G.getWord64le
  nextAttrId    <- G.getWord16le
  G.skip 2 -- alignment
  recordNumber  <- G.getWord32le
  updSeq        <- G.getWord16le
  updSeqArr     <- getList G.getWord16le (fromIntegral sizeUpdSeqArr - 1)
  return FileRecordHeader{..}


getAttribute :: G.Get (Maybe (FileAttribute))
getAttribute = (getConditional G.getWord64le (== 0xFFFFFFFF) *> pure Nothing) <|> do
  attrType      <- return . fromJust . toAttributeType True =<< G.getWord32le
  attrLength    <- G.getWord32le
  isNonResident <- return . (/= 0x00) =<< G.getWord8
  nameLength    <- return . fromIntegral =<< G.getWord8
  nameOffset    <- G.getWord16le
  attrFlags     <- G.getWord16le
  attrId        <- G.getWord16le
  if isNonResident then do
    {- non-resident attribute -}
    attrconStartVCN        <- G.getWord64le
    attrconLastVCN         <- G.getWord64le
    dataRunOffset          <- G.getWord16le
    attrconCompUnitSize    <- G.getWord16le
    G.skip 4               -- padding
    attrconAllocSize       <- G.getWord64le
    attrconRealSize        <- G.getWord64le
    attrconInitializedSize <- G.getWord64le
    attrName               <- G.getByteString nameLength
    attrconDataRuns        <- getDataRun
    let attrContent         = FileAttributeNonResidentContent{..}
    return FileAttribute{..}
    else do
    {- resident attribute -}
    attrconLength    <- return . fromIntegral =<< G.getWord32le
    attrconOffset    <- G.getWord32le
    attrconIsIndexed <- G.getWord8
    G.skip 1         -- padding
    attrName         <- G.getByteString nameLength
    attrconValue     <- G.getByteString attrLength
    let attrContent   = FileAttributeResidentContent{..}
    return FileAttribute{..}


getAttributeList :: G.Get [FileAttribute]
getAttributeList = getListMaybe getAttribute


getDataRunElem :: G.Get (Maybe (Int, Int))
getDataRunElem = (getConditional G.getWord8 (== 0) *> pure Nothing) <|> do
  header <- G.getWord8
  let
    size_offset = fromIntegral $ header `shiftR` 8
    size_length = fromIntegral $ header .&. 0x0f
  length <- G.getByteString size_length
  offset <- G.getByteString size_offset
  return $ Just (bsToIntLE length, bsToIntLE offset)


getDataRun :: G.Get [(Int, Int)]
getDataRun = getListMaybe getDataRunElem


main :: IO ()
main = do
  runResourceT $
    CB.sourceFile "README.md"
    $$ CB.sinkHandle stdout
