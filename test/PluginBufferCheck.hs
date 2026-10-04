{-# LANGUAGE OverloadedStrings #-}
module PluginBufferCheck (checks) where

import Control.Exception (evaluate)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Hide.Buffer
import qualified Hide.Plugin.Buffer as P
import qualified Hide.Plugin.BufferHost as Host

checks :: IO ()
checks = do
  let check name ok=unless ok (error ("plugin buffer: "++name))
      b=newBuffer "λ😀\r\nsecond\n"
      captured=Host.captureRead b
      edited=replaceSelection (Selection 0 1) "changed" b
  check "text offsets count characters and preserve line endings"
    (P.readText captured (P.TextRange (P.CharOffset 1) (P.CharOffset 4))==Right "😀\r\n")
  check "line reads preserve original CRLF and LF terminators"
    (P.readLines captured (P.LineNumber 0) 2==Right "λ😀\r\nsecond\n")
  check "retained content stays immutable after edits"
    (P.readLines captured (P.LineNumber 0) 1==Right "λ😀\r\n" && contents edited/="λ😀\r\nsecond\n")
  check "final empty editor line can be read"
    (P.readLines captured (P.LineNumber 2) 1==Right "")
  check "invalid ranges and overflowing counts are rejected"
    (all isLeft [P.readText captured (P.TextRange (P.CharOffset (-1)) (P.CharOffset 2)),
      P.readText captured (P.TextRange (P.CharOffset 2) (P.CharOffset 1)),
      P.readLines captured (P.LineNumber 1) maxBound,
      P.readLines captured (P.LineNumber 3) 1])
  let removed=replaceSelection (Selection 2 6) "" (newBuffer "a\nb\nc\nd\n")
      replaced=replaceSelection (Selection 2 4) "λ\n" removed
      changed=Host.captureRead replaced
  check "live ranges and rows exclude deleted provenance leaves"
    (P.readText changed (P.TextRange (P.CharOffset 0) (P.CharOffset 4))==Right "a\nλ\n" &&
      P.readLines changed (P.LineNumber 1) 2==Right "λ\n" &&
      P.readLine changed (P.LineNumber 1)==Right "λ" &&
      bufferLineChanges replaced/=(0,0))
  let raw=BS.pack [0,127,128,255,10]
      bytes=Host.captureRead (newByteBuffer raw)
  check "byte reads preserve all values without UTF-8 encoding"
    (P.readBytes bytes (P.ByteRange (P.ByteOffset 1) (P.ByteOffset 4))==Right (BS.pack [127,128,255]))
  check "text and byte readers reject the other representation"
    (isLeft (P.readText bytes (P.TextRange (P.CharOffset 0) (P.CharOffset 1))) &&
      isLeft (P.readLines bytes (P.LineNumber 0) 1) &&
      isLeft (P.readBytes captured (P.ByteRange (P.ByteOffset 0) (P.ByteOffset 1))))
  let opaque=b {saved=error "read forced saved baseline",undoStack=error "read forced Undo",redoStack=error "read forced Redo"}
  _<-evaluate (Host.captureRead opaque)
  check "capture and reads exclude saved text and history"
    (P.readText (Host.captureRead opaque) (P.TextRange (P.CharOffset 0) (P.CharOffset 1))==Right "λ")
  version<-Host.captureVersion b
  check "unchanged buffer version remains current" =<< Host.versionCurrent version b
  check "text edit invalidates version" . not =<< Host.versionCurrent version edited
  check "equal-revision replacement invalidates version" . not =<<
    Host.versionCurrent version ((newBuffer (T.copy (contents b))) {revision=revision b})
  check "version capture excludes saved text and history" =<<
    (Host.captureVersion opaque >>= \v->Host.versionCurrent v opaque)
  putStrLn "plugin buffer checks passed"
  where
    isLeft (Left _)=True
    isLeft _=False
