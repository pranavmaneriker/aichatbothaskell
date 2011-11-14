module Main where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import System.Directory
import Char
import ParseAIML
import Directory
import System.FilePath.Posix

main = do{ dir<-getCurrentDirectory
	 ; parser<-superParser $ dir++"/aiml"
	 ; startChat parser
	 }
	   
startChat parser = do
		      putStr "Enter your chat:"
		      usermsg<-getUserMsg
		      processedMsg<-(preprocess usermsg)
		      flag<-isBye processedMsg
		      putStr "Bot:"
		      reply<-getResponse parser processedMsg
		      sendResponse reply
		      if flag then
			stopChat
			else do
			  startChat parser


getUserMsg = getLine
sendResponse = putStrLn
stopChat = return ()
isBye msg = return ((compare msg "BYE") == EQ)

preprocess :: String -> IO String
preprocess msg = return (fmap toUpper msg)

getResponse :: Parser String -> String -> IO String
getResponse parser input = do{ case (parse parser "" input) of
				  Left _ -> return "PATTERN MISMATCH"
				  Right x -> return x
			     }
		      
superParser :: String -> IO (Parser String)
superParser dir = do{ files<-getDirectoryContents dir
		    ; parseAimlFiles $ getProperFileList dir files
		    }

getProperFileList :: String -> [String] -> [String]
getProperFileList dir files = case files of
				   [] -> []
				   x:xs -> case (takeExtension x) of
						".aiml" -> (dir++("/"++x)):(getProperFileList dir xs)
						_ -> getProperFileList dir xs

append :: String -> String -> String
append st1 st2 = st1++st2