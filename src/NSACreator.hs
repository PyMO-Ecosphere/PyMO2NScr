{-# LANGUAGE OverloadedStrings #-}

module NSACreator (createNSA) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, word32LE, hPutBuilder)
import qualified Data.ByteString.Builder as B
import System.IO (withBinaryFile, IOMode(WriteMode))
import Data.Word (Word32)

-- | Create an NSA archive file.
--   The NSA format appears to consist of a series of file entries followed by file data.
--   Each entry: null-terminated filename, compression type (1 byte),
--   offset (4 bytes, little-endian), length (4 bytes, little-endian).
--   File data is concatenated after all entries.
--   Compression type 0 indicates no compression.
--   The encodeFilename function converts the filename String to a ByteString
--   with appropriate encoding (e.g., UTF-8, GBK, ShiftJIS).
createNSA :: (String -> BS.ByteString) -> FilePath -> [(FilePath, BL.ByteString)] -> IO ()
createNSA encodeFilename nsaOutputFileName fileList = do
    -- Calculate offsets for each file
    let -- Entry size for a file: encoded filename + null + compr(1) + offset(4) + length(4)
        entrySize :: FilePath -> Word32
        entrySize fp = fromIntegral $ BS.length (encodeFilename fp) + 1 + 1 + 4 + 4

        totalEntrySize = sum (map (entrySize . fst) fileList) + 1  -- +1 for null terminator entry

        -- Calculate offsets for each file (starting after entries)
        fileSizes = map (fromIntegral . BL.length . snd) fileList :: [Word32]
        offsets = scanl (+) totalEntrySize fileSizes

        fileEntries = zip3 (map fst fileList) offsets fileSizes

    -- Write NSA file
    withBinaryFile nsaOutputFileName WriteMode $ \h -> do
        -- Build and write all entries
        let entriesBuilder = mconcat $ map (buildEntry encodeFilename) fileEntries
        hPutBuilder h entriesBuilder
        -- Write null terminator entry (empty filename)
        hPutBuilder h (B.word8 0)

        -- Write file data
        mapM_ (\(_, bs) -> BL.hPut h bs) fileList

-- Build a single file entry as Builder
buildEntry :: (String -> BS.ByteString) -> (FilePath, Word32, Word32) -> Builder
buildEntry encodeFilename (filePath, offset, fileLength) =
    B.byteString (encodeFilename filePath)  -- encoded filename bytes
    <> B.word8 0         -- null terminator
    <> B.word8 0         -- compression type (0 = no compression)
    <> word32LE offset   -- offset (32-bit little endian)
    <> word32LE fileLength   -- length (32-bit little endian)