{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Help (layoutMarkdown) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Maybe (isJust)
import THC.Edit.Buffer (columnOffset, displayColumn, nextCharacter)

-- A small README renderer: no HTML, reference links, escapes, block quotes,
-- or nested inline markup. Fenced code stays verbatim and may scroll sideways.
-- A glyph wider than the entire viewport is kept intact (notably CJK at width 1).
layoutMarkdown :: Int -> Text -> Text
layoutMarkdown requested = T.intercalate "\n" . blocks . T.splitOn "\n" . T.replace "\r\n" "\n"
  where
    width = max 1 requested
    blocks [] = []
    blocks (line:rest)
      | T.null (T.strip line) = "" : blocks rest
      | Just marker <- fence line =
          let (code, after) = break (T.isPrefixOf marker . T.stripStart) rest
          in code ++ blocks (drop 1 after)
      | Just title <- heading line = wrap width (T.toUpper (inline title)) ++ blocks rest
      | separator:remaining <- rest, T.any (== '|') line, tableSeparator separator =
          let (rows, after) = span (T.any (== '|')) remaining
          in table width (map cells (line:rows)) ++ blocks after
      | Just (prefix, body) <- listItem line =
          let (continuation, after) = span (\s -> not (T.null s) && isSpace (T.index s 0) && not (special s)) rest
              content = inline (T.unwords (body : map T.strip continuation))
              indent = columns prefix
              rendered = if indent >= width then wrap width (prefix <> content)
                else case wrap (width - indent) content of
                  [] -> [T.stripEnd prefix]
                  first:more -> (prefix <> first) : map (T.replicate indent " " <>) more
          in rendered ++ blocks after
      | otherwise =
          let (paragraph, after) = span (not . special) rest
          in wrap width (inline (T.unwords (line:paragraph))) ++ blocks after
    special s = T.null (T.strip s) || isJust (fence s) || isJust (heading s)
      || isJust (listItem s) || T.any (== '|') s

columns :: Text -> Int
columns text = displayColumn text (T.length text)

wrap :: Int -> Text -> [Text]
wrap width = go "" . T.words
  where
    go current [] = [current | not (T.null current)]
    go current wordsLeft@(word:rest)
      | not (T.null current) =
          if columns (current <> " " <> word) <= width
          then go (current <> " " <> word) rest
          else current : go "" wordsLeft
      | columns word <= width = go word rest
      | otherwise =
          let (part, remaining) = T.splitAt (max (nextCharacter word 0) (columnOffset word width)) word
          in part : go "" ([remaining | not (T.null remaining)] ++ rest)

heading :: Text -> Maybe Text
heading line = let (marks, rest) = T.span (== '#') (T.stripStart line)
  in if not (T.null marks) && T.length marks <= 6 && T.isPrefixOf " " rest
     then Just (T.strip rest) else Nothing

fence :: Text -> Maybe Text
fence line
  | "```" `T.isPrefixOf` stripped = Just "```"
  | "~~~" `T.isPrefixOf` stripped = Just "~~~"
  | otherwise = Nothing
  where stripped = T.stripStart line

listItem :: Text -> Maybe (Text, Text)
listItem line =
  let (indent, content) = T.span isSpace line
      (number, suffix) = T.span isDigit content
  in case T.uncons content of
    Just (bullet, rest) | bullet `elem` ("-*+" :: String), " " `T.isPrefixOf` rest ->
      Just (indent <> "- ", T.strip rest)
    _ | not (T.null number), ". " `T.isPrefixOf` suffix ->
      Just (indent <> number <> ". ", T.strip (T.drop 2 suffix))
    _ -> Nothing

cells :: Text -> [Text]
cells = map (inline . T.strip) . T.splitOn "|" . T.dropAround (== '|') . T.strip

tableSeparator :: Text -> Bool
tableSeparator line = T.any (== '|') line && all valid (cells line)
  where valid cell = T.any (== '-') cell && T.all (`elem` ("-: " :: String)) cell

table :: Int -> [[Text]] -> [Text]
table _ [] = []
table width rows@(header:_)
  | width < 4 * count - 3 = concatMap (wrap width . T.intercalate "; ") rows
  | otherwise = concatMap render rows
  where
    count = length header
    sizes = [min ((width - 3 * (count - 1)) `div` count)
                 (maximum (1 : [columns cell | row <- rows, cell <- take 1 (drop i row)]))
            | i <- [0 .. count - 1]]
    render row =
      let wrapped = zipWith (\size cell -> case wrap size cell of [] -> [""]; ls -> ls) sizes (take count (row ++ repeat ""))
          height = maximum (map length wrapped)
          at i ls = case drop i ls of line:_ -> line; [] -> ""
      in [T.stripEnd (T.intercalate " | " (zipWith (\size ls -> let text = at i ls in text <> T.replicate (size - columns text) " ") sizes wrapped))
         | i <- [0 .. height - 1]]

inline :: Text -> Text
inline = T.pack . go False . T.unpack
  where
    go _ [] = []
    go _ ('`':rest) = case break (== '`') rest of
      (code, _:after) -> code ++ go False after
      _ -> '`' : go False rest
    go _ ('[':rest) = case break (== ']') rest of
      (label, ']':'(':url) -> case break (== ')') url of
        (_, ')':after) -> go False label ++ go False after
        _ -> '[' : go False rest
      _ -> '[' : go False rest
    go previous (c:rest)
      | c `elem` ("*_" :: String), not previous =
          let (extra, body) = span (== c) rest
              marker = T.pack (c:extra)
              (inside, after) = T.breakOn marker (T.pack body)
          in if not (T.null inside) && not (T.null after) && not (isSpace (T.index inside 0))
             then go False (T.unpack inside) ++ go False (T.unpack (T.drop (T.length marker) after))
             else c : go False rest
      | otherwise = c : go (isAlphaNum c) rest
