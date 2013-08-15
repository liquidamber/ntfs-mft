{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Conduit
import Data.Word
import System.IO (stdout)
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.Conduit.Binary as CB

data MFTHeader =
  MFTHeader
  { headerType    :: !Word32
  , headerLength  :: !Word32
  , isNonResident :: !Word8
  , nameLength    :: !Word8
  , offsetContent :: !Word16
  , isCompressed  :: !Word16
  , identificator :: !Word16
  } deriving (Show, Read, Eq)


{-
0x10 	  	$STANDARD_INFORMATION
0x20 	  	$ATTRIBUTE_LIST
0x30 	  	$FILE_NAME
0x40 	NT 	$VOLUME_VERSION
0x40 	2K 	$OBJECT_ID
0x50 	  	$SECURITY_DESCRIPTOR
0x60 	  	$VOLUME_NAME
0x70 	  	$VOLUME_INFORMATION
0x80 	  	$DATA
0x90 	  	$INDEX_ROOT
0xA0 	  	$INDEX_ALLOCATION
0xB0 	  	$BITMAP
0xC0 	NT 	$SYMBOLIC_LINK
0xC0 	2K 	$REPARSE_POINT
0xD0 	  	$EA_INFORMATION
0xE0 	  	$EA
0xF0 	NT 	$PROPERTY_SET
0x100 	2K 	$LOGGED_UTILITY_STREAM
-}
data AttributeType
  = StandardInformation
  | AttributeList
  | FileName
  | VolumeVersion
  | ObjectId
  | SecurityDescriptor
  | VolumeName
  | VolumeInformation
  | Data
  | IndexRoot
  | IndexAllocation
  | Bitmap
  | SymbolicLink
  | ReparsePoint
  | EaInformation
  | Ea
  | PropertySet
  | LoggedUtilityStream


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


getFileRecordHeader :: G.Get () -- G.Get FileRecordHeader
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
  skip 2 -- alignment
  recordNumber  <- G.getWord32le
  updSeq        <- G.getWord16le
  updSeqArr     <- getList G.getWord16le (fromIntegral sizeUpdSeqArr - 1)
  return ()


getAttrElem :: G.Get (Maybe (FileAttribute))
getAttrElem = (getConditional G.getWord64 (== 0xFFFFFFFF) *> pure Nothing) <|> do
  attrType    <- G.getWord32le
  totalLength <- G.getWord32le
  isResident  <- G.getWord8
  nameLength  <- G.getWord8
  nameOffset  <- G.getWord16le
  flags       <- G.getWord16le
  attrId      <- G.getWord16le
  {- resident attribute -}
  -- attrLength    <- G.getWord32le
  -- attrOffset    <- G.getWord32le
  -- isIndexed     <- G.getWord8
  -- skip 1 -- padding
  -- attrName      <- G.getByteString nameLength
  -- attrBody      <- G.getByteString attrLength
  {- non-resident attribute -}
  -- startVCN      <- G.getWord64le
  -- lastVCN       <- G.getWord64le
  -- dataRunOffset <- G.getWord16le
  -- compUnitSize  <- G.getWord16le
  -- skip 4 -- padding
  -- allocSize     <- G.getWord64le
  -- realSize      <- G.getWord64le
  -- initializedN  <- G.getWord64le
  -- attrName      <- G.getByteString nameLength
  -- attrDataRun   <- getDataRun


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
