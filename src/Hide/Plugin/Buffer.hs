-- | Immutable measured buffer reads for trusted linked plugins.
--
-- The host supplies reads after checking authority. A read retains live tree
-- content and its representation, excluding separate saved baseline/Undo roots.
-- Deleted provenance leaves remain retained but are invisible to live reads.
-- Reads may outlive their source buffer; revocation cannot erase an already
-- granted immutable read. Large reads and subsequent evaluation belong on a
-- worker. This module provides no mutable desktop, saving or edit submission.
module Hide.Plugin.Buffer
  ( BufferRead, ContentVersion, CharOffset(..), ByteOffset(..), LineNumber(..)
  , TextRange(..), ByteRange(..), RangeError(..), BufferRepresentation(..)
  , representation, readLength, readLineCount, readText, readBytes, readLines, readLine
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Hide.Buffer as B
import Hide.Plugin.BufferHost (ContentVersion)

-- | Immutable content reference with no structural Eq/Show instance.
type BufferRead = B.BufferContent
-- | Zero-based Unicode character offset, independent of UTF-8/UTF-16 and cells.
newtype CharOffset = CharOffset Int deriving (Eq,Show)
-- | Zero-based offset in a byte buffer's original byte representation.
newtype ByteOffset = ByteOffset Int deriving (Eq,Show)
-- | Zero-based editor row, including a final empty row after a newline.
newtype LineNumber = LineNumber Int deriving (Eq,Show)
-- | Half-open Unicode character range. Reads validate both endpoints.
data TextRange = TextRange CharOffset CharOffset deriving (Eq,Show)
-- | Half-open original byte range. Reads validate both endpoints.
data ByteRange = ByteRange ByteOffset ByteOffset deriving (Eq,Show)
data RangeError = WrongRepresentation | InvalidRange deriving (Eq,Show)
data BufferRepresentation = TextBuffer | ByteBuffer deriving (Eq,Show)

representation :: BufferRead -> BufferRepresentation
representation b=if B.contentByteMode b then ByteBuffer else TextBuffer

-- | Cached live length, in characters for text and bytes for byte buffers.
-- Use the matching range type when reading; this is metadata, not a conversion.
readLength :: BufferRead -> Int
readLength = B.contentLength
-- | Cached editor-row count; byte buffers do not expose text rows.
readLineCount :: BufferRead -> Either RangeError Int
readLineCount b=require TextBuffer b >> pure (B.contentLineCount b)

-- | Read an exact character range using measured splits; never silently clamp.
readText :: BufferRead -> TextRange -> Either RangeError Text
readText b (TextRange (CharOffset a) (CharOffset z))=do
  require TextBuffer b
  validRange (readLength b) a z
  pure (B.contentSlice b a (z-a))

-- | Read an exact original byte range without flattening/encoding other content.
readBytes :: BufferRead -> ByteRange -> Either RangeError ByteString
readBytes b (ByteRange (ByteOffset a) (ByteOffset z))=do
  require ByteBuffer b
  validRange (readLength b) a z
  -- Each stored Latin-1 character is exactly one original byte.
  pure (B.contentByteSlice b a (z-a))

-- | Read complete source rows, preserving original LF/CRLF terminators.
-- The count must fit within remaining editor rows. Zero rows at EOF are valid.
readLines :: BufferRead -> LineNumber -> Int -> Either RangeError Text
readLines b (LineNumber row) count=do
  total<-readLineCount b
  if row<0 || row>total || count<0 || count>total-row then Left InvalidRange else do
    let a=B.contentLineOffset b row
        z=B.contentLineOffset b (row+count)
    pure (B.contentSlice b a (z-a))

-- | Read one editor row without its LF/CRLF terminator; rejects missing rows.
readLine :: BufferRead -> LineNumber -> Either RangeError Text
readLine b (LineNumber row)=do
  total<-readLineCount b
  if row<0 || row>=total then Left InvalidRange else pure (B.contentLineAt b row)

require :: BufferRepresentation -> BufferRead -> Either RangeError ()
require wanted b=if representation b==wanted then Right () else Left WrongRepresentation
validRange :: Int -> Int -> Int -> Either RangeError ()
validRange size a z=if a<0 || z<a || z>size then Left InvalidRange else Right ()
