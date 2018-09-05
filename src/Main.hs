{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Text.HTML.TagSoup as TS
import           Text.HTML.TagSoup (Tag(..))
import qualified Control.Monad.State as State
import           Control.Monad.State (StateT)
import qualified Data.Aeson as A
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  input <-
    case args of
      [] ->
        T.getContents
      ["--test"] ->
        pure ("<div class=\"foo bar\" aria-whatever=\"none\"><!-- something --><p>test <strong>ŻĄĄĄĄĄ</strong>   </p>   \n<li>test2</li>test3</div>" :: Text)

  let tags = TS.parseTags input
  State.evalStateT (mapM_ process tags) ""

type M = StateT Text IO

indent :: M ()
indent = State.modify ("  "<>)

dedent :: M ()
dedent = State.modify (T.drop 2)

output :: Text -> M ()
output str = do
  indentation <- State.get
  State.lift $ T.putStrLn $ indentation <> str

process :: TS.Tag Text -> M ()
process = \case
  TagOpen tag [] -> do
    output $ "el " <> escapeString tag <> " do"
    indent
  TagOpen tag attrs -> do
    output $ "elAttr " <> escapeString tag <> " (" <> ppAttrs attrs <> ") do"
    indent
  TagText str | T.all isSpace str ->
    pure ()
  TagText str ->
    output $ "text " <> escapeString (T.strip str)
  TagComment str ->
    output $ "{-" <> str <> "-}"
  TagClose _ ->
    dedent
  _ ->
    pure ()

ppAttrs :: [(Text, Text)] -> Text
ppAttrs = T.intercalate " <> " . map (\(k, v) -> escapeString k <> ":=" <> escapeString v)

-- Very hacky way of escaping strings.
-- But better than @show@, since @show@ escapes all non-ascii characters,
-- including Polish diacritics.
escapeString :: Text -> Text
escapeString = T.decodeUtf8 . BL.toStrict . A.encode
