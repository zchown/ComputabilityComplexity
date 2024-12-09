module TestTspParser where

import Control.Monad (filterM)
import Data.List (isSuffixOf)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec
import TspLibParser

findTspGzFiles :: FilePath -> IO [FilePath]
findTspGzFiles dir = do
  contents <- listDirectory dir
  let fullPaths = map (dir </>) contents
  let isTspGz path = ".tsp.gz" `isSuffixOf` path
  matchingFiles <-
    filterM
      (\path -> do
         isFile <- doesFileExist path
         return (isFile && isTspGz path))
      fullPaths
  return matchingFiles

runTspParserTest :: IO ()
runTspParserTest =
  hspec $ do
    describe "read all files no errors" $ do
      it "reads all files without errors" $ do
        files <- findTspGzFiles "tsp_problems"
        results <-
          traverse
            (\file -> do
               putStr $ "Parsing " ++ file ++ "... "
               result <- parseTspFile file
               case result of
                 Left err -> do
                   putStrLn $ "FAILED\n    Error: " ++ show err
                   return result
                 Right _ -> do
                   putStrLn "PASSED"
                   return result)
            files
        let totalFiles = length files
        let failedFiles = length $ filter isLeft results
        let passedFiles = totalFiles - failedFiles
        putStrLn $ "\nSummary:"
        putStrLn $ "  Total files: " ++ show totalFiles
        putStrLn $ "  Passed: " ++ show passedFiles
        putStrLn $ "  Failed: " ++ show failedFiles
        all (either (const False) (const True)) results `shouldBe` True
  where
    isLeft (Left _) = True
    isLefopp = False
