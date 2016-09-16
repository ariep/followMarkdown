module Main where

import           Control.Applicative (many, (<*), (*>))
import           Control.Monad       (when)
import qualified Data.Attoparsec.Text as A
import           Data.Text           (Text)
import qualified Pipes                as P
import           Pipes               ((>->))
-- import qualified Pipes.Prelude        as P
import qualified Pipes.Prelude.Text   as P
import qualified Pipes.Safe           as P
import           System.Environment  (getArgs)
import           System.Exit         (exitFailure)


main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ do
    putStrLn "Usage: followMarkdown input.md output.md"
    exitFailure
  let [inputFile, outputFile] = args
  P.runSafeT . P.runEffect $
    process inputFile >-> P.writeFileLn outputFile

process :: FilePath -> P.Producer Text M ()
process path = P.for (P.readFileLn path) processLine
 where
  processLine line = case A.parseOnly linkParser line of
    -- Parsing the line as a markdown link failed.
    Left _  -> P.yield line    -- So return the line as it is.
    -- Parsing the line succeeded, giving a link target.
    Right t -> do -- So continue by dumping the contents of the target file.
      -- Add empty lines before and after the include, to keep markdown blocks in
      -- different files separate.
      P.yield ""
      P.readFileLn t 
      P.yield ""

type M = P.SafeT IO

linkParser :: A.Parser FilePath
linkParser = A.skipSpace *> bullet *> A.skipSpace *> link <* A.endOfInput
 where
  bullet = A.satisfy ( \ c -> c == '*' || c == '-')
  link = A.char '[' *> many (A.notChar ']') *> A.char ']' *>
    A.char '(' *> many (A.notChar ')') <* A.char ')' <* A.skipSpace
