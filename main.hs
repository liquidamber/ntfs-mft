{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Debug.Trace
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Conduit
import Data.Conduit.Serialization.Binary
import Data.Maybe
import Data.Word
import Numeric
import System.IO (stdout)
import System.Environment (getArgs)
import qualified Control.Exception.Lifted as E
import qualified Data.Conduit.Binary    as CB
import qualified Data.Conduit.List      as CL
import qualified Data.ByteString        as B
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T


data MFTReaderSetting =
  MFTReaderSetting
  deriving (Show, Read, Eq)


defaultMFTReaderSetting :: MFTReaderSetting
defaultMFTReaderSetting = MFTReaderSetting


isLaterOrEqualXP :: MFTReaderSetting -> Bool
isLaterOrEqualXP = const True


isLaterOrEqual2k :: MFTReaderSetting -> Bool
isLaterOrEqual2k = const True


getMFTRecordLength :: MFTReaderSetting -> Int
getMFTRecordLength = const 1024


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


getFileRecord :: MFTReaderSetting -> Get FileRecord
getFileRecord setting = do
  posHead    <- bytesRead
  header     <- getFileRecordHeader setting
  attributes <- getAttributeList setting
  posLast    <- bytesRead
  skip $ f (getMFTRecordLength setting) - f (posLast - posHead)
  r $! FileRecord header attributes
    where
      f x = trace ("getFileRecord: " ++ showHex x "") fromIntegral x
      r x = return $! trace (show x) x


getFileRecordHeader :: MFTReaderSetting -> Get FileRecordHeader
getFileRecordHeader setting = do
  magic <- getConditional (getByteString 4) (== "FILE")
  offUpdSeq           <- getWord16le
  sizeUpdSeqArr       <- getWord16le
  recordLogFileSeqNum <- getWord64le
  seqNum              <- getWord16le
  recordHardLinkCount <- getWord16le
  offAttr             <- getWord16le
  recordFlags         <- getWord16le
  recordRealSize      <- getWord32le
  recordAllocSize     <- getWord32le
  recordRefBaseFile   <- getWord64le
  recordNextAttrId    <- getWord16le
  skip                 $ if isLaterOrEqualXP setting then 2 else 0
  recordNum           <- if isLaterOrEqualXP setting then getWord32le else return 0
  recordUpdSeqNum     <- getWord16le
  recordUpdSeqArr     <- getList getWord16le (fromIntegral sizeUpdSeqArr - 1)
  skip =<< return . fromIntegral . (offAttr -) . fromIntegral =<< bytesRead
  return $! FileRecordHeader{..}


getAttributeType :: MFTReaderSetting -> Get AttributeType
getAttributeType setting = do
  posHead       <- bytesRead -- debug
  attrid <- getWord32le
  let mattr = toAttributeType setting attrid
  case mattr of
    Just attr -> return attr
    Nothing   -> fail $ "Undefined attribute type: 0x" ++ showHex attrid " at " ++ showHex posHead ""


getAttribute :: MFTReaderSetting -> Get (Maybe (FileAttribute))
getAttribute setting = (getConditional getWord32le (== 0xFFFFFFFF) *> pure Nothing) <|> do
  posHead       <- return . (\x -> trace ("attributeBegin: 0x" ++ showHex x "") x) =<< bytesRead
  attrType      <- getAttributeType setting
  attrLength    <- getWord32le
  isNonResident <- return . (/= 0x00) =<< getWord8
  nameLength    <- return .  (\x -> trace ("attrnameLength: 0x" ++ showHex x "") x) . fromIntegral =<< getWord8
  nameOffset    <- getWord16le
  attrFlags     <- getWord16le
  attrId        <- getWord16le
  if isNonResident then do
    {- non-resident attribute -}
    attrconStartVCN        <- getWord64le
    attrconLastVCN         <- getWord64le
    dataRunOffset          <- getWord16le
    attrconCompUnitSize    <- getWord16le
    skip 4               -- padding
    attrconAllocSize       <- getWord64le
    attrconRealSize        <- getWord64le
    attrconInitializedSize <- getWord64le
    attrName               <- getTextUtf16le $ 2 * nameLength
    attrconDataRuns        <- getDataRun
    posTail                <- bytesRead
    let restLength = f attrLength - f (posTail - posHead)
          where
            f x = trace ("getAttr: " ++ showHex x "") fromIntegral x
    skip $ trace ("restLength: 0x" ++ showHex restLength "") restLength
    let attrContent         = FileAttributeNonResidentContent{..}
    return $! Just $! FileAttribute{..}
    else do
    {- resident attribute -}
    attrconLength      <- return . fromIntegral =<< getWord32le
    attrconOffset      <- getWord16le
    attrconIndexedFlag <- getWord8
    skip 1           -- padding
    attrName           <- getTextUtf16le $ 2 * nameLength
    attrconValue       <- getByteString $ fromIntegral $ trace ("attrConLength: 0x" ++ showHex attrconLength "") attrconLength
    let restLength = fromIntegral attrLength - 0x18 - 2 * nameLength - attrconLength
    skip $ trace ("restLength: 0x" ++ showHex restLength "") restLength
    let attrContent     = FileAttributeResidentContent{..}
    return $! Just $! FileAttribute{..}
  where
    getTextUtf16le = return . T.decodeUtf16LE <=< getByteString


getAttributeList :: MFTReaderSetting -> Get [FileAttribute]
getAttributeList = getListMaybe . getAttribute


getDataRunElem :: Get (Maybe (Int, Int))
getDataRunElem = runMaybeT $ do
  header <- lift $ return . (\x -> trace ("datarun headder: 0x" ++ showHex x "") x) =<< getWord8
  when (header == 0x00) (fail "end of data runs")
  let
    size_offset = fromIntegral $ header `shiftR` 4
    size_length = fromIntegral $ header .&. 0x0f
  length <- lift $ getByteString size_length
  offset <- lift $ getByteString size_offset
  return $! (bsToIntLE length, bsToIntLE offset)


getDataRun :: Get [(Int, Int)]
getDataRun = getListMaybe getDataRunElem


main :: IO ()
main = do
  target <- return . head =<< getArgs
  E.handle handler $ do
    fr <-
      runResourceT $
      CB.sourceFile target
      $= conduitGet (getFileRecord defaultMFTReaderSetting)
      $$ CL.consume
    print fr
  where
    handler e = do
      putStrLn $ "===ERROR OCCURED==="
      putStr "Offset: 0x"
      putStrLn $ flip showHex "" $ offset e
      putStrLn $ content e
--end
