import SubsetConstruction
import TestWords
import ReadWrite
import System.Environment
import Types


import Minimization 

import Data.Set (Set)
import Data.Set qualified as Set


printDFA :: DFA -> IO ()
printDFA dfa = do
     mapM_ putStr (showDFA dfa)


printDFA_ :: DFA_ -> IO()
printDFA_ dfa = do
     mapM_ putStr (showDFA_ dfa)


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

          ("-m" : file : _) ->
               do
               content <- readFile file
               let dfa = processDFA (lines content)
               printDFA_ (newDFA dfa)

          _ -> putStrLn ("Argument need to be provided!\nAvailable commands are:\n" 
                    ++ "1) runghc Main.hs -n file.nfa > file.dfa\n"
                    ++ "2) runghc Main.hs -t file.dfa < words.txt\n"
                    ++ "3) runghc Main.hs -m file.dfa > file_.dfa")