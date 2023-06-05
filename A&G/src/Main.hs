import SubsetConstruction
import TestWords
import ReadWrite
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
          ("-n" : file : _) -> 
               do
               content <- readFile file
               let dfa = constructDFAFromNFA (lines content)
               printDFA dfa
          
          ("-t" : file : _) -> do
               content <- readFile file
               let dfa = processDFA (lines content)
               wordsToTest <- getContents 
               let wordsList = lines wordsToTest
               mapM_ (\word -> putStrLn (word ++ ": " ++ if simulateDFA_ dfa word then "accepted" else "not accepted")) wordsList

          _ -> putStrLn "Unknown command"