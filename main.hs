{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.Conduit
import Data.Maybe
import Data.Word
import System.IO (stdout)
import qualified Data.Conduit.Binary    as CB
import qualified Data.Binary.Get        as G
import qualified Data.ByteString        as B
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T

data MFTReaderSetting =
  MFTReaderSetting
  deriving (Show, Read, Eq)


isLaterOrEqualXP :: MFTReaderSetting -> Bool
isLaterOrEqualXP = const True

isLaterOrEqual2k :: MFTReaderSetting -> Bool
isLaterOrEqual2k = const True


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
  , recordUpdSeqNum     :: !Word16
  , recordUpdSeqArr     :: ![Word16]
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
    { attrconIndexedFlag :: !Word8
    , attrconValue       :: !B.ByteString
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
toAttributeType :: MFTReaderSetting -> Word32 -> Maybe AttributeType
toAttributeType setting value = case value of
  0x10             -> Just AttrTypeStandardInformation
  0x20             -> Just AttrTypeAttributeList
  0x30             -> Just AttrTypeFileName
  0x40 | isNT      -> Just AttrTypeVolumeVersion
       | otherwise -> Just AttrTypeObjectId
  0x50             -> Just AttrTypeSecurityDescriptor
  0x60             -> Just AttrTypeVolumeName
  0x70             -> Just AttrTypeVolumeInformation
  0x80             -> Just AttrTypeData
  0x90             -> Just AttrTypeIndexRoot
  0xA0             -> Just AttrTypeIndexAllocation
  0xB0             -> Just AttrTypeBitmap
  0xC0 | isNT      -> Just AttrTypeSymbolicLink
       | otherwise -> Just AttrTypeReparsePoint
  0xD0             -> Just AttrTypeEaInformation
  0xE0             -> Just AttrTypeEa
  0xF0  | isNT     -> Just AttrTypePropertySet
  0x100 | is2k     -> Just AttrTypeLoggedUtilityStream
  _                -> Nothing
  where
    is2k = isLaterOrEqual2k setting
    isNT = not is2k


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


getFileRecord :: MFTReaderSetting -> G.Get FileRecord
getFileRecord setting = do
  header     <- getFileRecordHeader setting
  attributes <- getAttributeList setting
  return $ FileRecord header attributes


getFileRecordHeader :: MFTReaderSetting -> G.Get FileRecordHeader
getFileRecordHeader setting = do
  magic <- getConditional (G.getByteString 4) (== "FILE")
  offUpdSeq           <- G.getWord16le
  sizeUpdSeqArr       <- G.getWord16le
  recordLogFileSeqNum <- G.getWord64le
  seqNum              <- G.getWord16le
  recordHardLinkCount <- G.getWord16le
  offAttr             <- G.getWord16le
  recordFlags         <- G.getWord16le
  recordRealSize      <- G.getWord32le
  recordAllocSize     <- G.getWord32le
  recordRefBaseFile   <- G.getWord64le
  recordNextAttrId    <- G.getWord16le
  G.skip               $ if isLaterOrEqualXP setting then 2 else 0
  recordNum           <- if isLaterOrEqualXP setting then G.getWord32le else return 0
  recordUpdSeqNum     <- G.getWord16le
  recordUpdSeqArr     <- getList G.getWord16le (fromIntegral sizeUpdSeqArr - 1)
  return FileRecordHeader{..}


getAttribute :: MFTReaderSetting -> G.Get (Maybe (FileAttribute))
getAttribute setting = (getConditional G.getWord64le (== 0xFFFFFFFF) *> pure Nothing) <|> do
  attrType      <- return . fromJust . toAttributeType setting =<< G.getWord32le
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
    attrName               <- getTextUtf16le nameLength
    attrconDataRuns        <- getDataRun
    let attrContent         = FileAttributeNonResidentContent{..}
    (return . Just) FileAttribute{..}
    else do
    {- resident attribute -}
    attrconLength      <- return . fromIntegral =<< G.getWord32le
    attrconOffset      <- G.getWord32le
    attrconIndexedFlag <- G.getWord8
    G.skip 1           -- padding
    attrName           <- getTextUtf16le nameLength
    attrconValue       <- G.getByteString $ fromIntegral attrLength
    let attrContent     = FileAttributeResidentContent{..}
    (return . Just) FileAttribute{..}
  where
    getTextUtf16le = return . T.decodeUtf16LE <=< G.getByteString


getAttributeList :: MFTReaderSetting -> G.Get [FileAttribute]
getAttributeList = getListMaybe . getAttribute


getDataRunElem :: G.Get (Maybe (Int, Int))
getDataRunElem = runMaybeT $ do
  header <- lift $ G.getWord8
  when (header == 0x00) (fail "end of data runs")
  let
    size_offset = fromIntegral $ header `shiftR` 8
    size_length = fromIntegral $ header .&. 0x0f
  length <- lift $ G.getByteString size_length
  offset <- lift $ G.getByteString size_offset
  return $ (bsToIntLE length, bsToIntLE offset)


getDataRun :: G.Get [(Int, Int)]
getDataRun = getListMaybe getDataRunElem


main :: IO ()
main = do
  runResourceT $
    CB.sourceFile "README.md"
    $$ CB.sinkHandle stdout
