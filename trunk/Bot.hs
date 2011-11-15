module Bot where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import System.Directory
import Char
import ParseAIML
import Directory
import System.FilePath.Posix

import Control.Monad (unless)
import Network.Socket hiding (recv)
import qualified Data.ByteString as S
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C

port = "3000"

main = do{ dir<-getCurrentDirectory
	 ; parser<-superParser $ dir++"/aiml"
	 ; startChat parser
	 }
	   
startChat parser = do
		      putStr "Enter your chat:"
		      usermsg<-getUserMsg
		      let processedMsg = (preProcess usermsg)
		      let flag = isBye processedMsg
		      putStr "Bot:"
		      let reply = postProcess $ getResponse parser processedMsg ["","",""]
		      sendResponse reply
		      if flag then
			stopChat
			else do
			  startChat parser

-- startChat parser = withSocketsDo $
--     do {
-- 	   ; addrinfos <- getAddrInfo
--                     (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
--                     Nothing (Just port)
--        ; let serveraddr = head addrinfos
--        ; sock <- socket (addrFamily serveraddr) Stream defaultProtocol
--        ; bindSocket sock (addrAddress serveraddr)
--        ; listen sock 1
-- 	   ; loop sock parser
--        ; sClose sock
-- 	   }
-- 
-- loop sock parser = do	 {
-- 						 ;	putStrLn "Waiting for connection..."
-- 						 ;	(conn, _) <- accept sock
-- 						 ;	putStrLn "Client connected."
-- 						 ;	talk conn parser
-- 						 ;	sClose conn
-- 						 ;	putStrLn "Client disconnected."
-- 						 ;	loop sock parser
-- 						 }
-- 				where
-- 				  talk conn parser =
-- 									  do {
-- 										 ; msg <- recv conn 1024
-- 										 ; let processedMsg = (preProcess $ C.unpack msg)
-- 										 ; let reply = C.pack $ postProcess $ getResponse parser processedMsg ["",""]
-- 										 ; unless (S.null msg) $ sendAll conn reply >> talk conn parser
-- 										 }


getUserMsg = getLine
sendResponse = putStrLn
stopChat = return ()
isBye msg = ((compare msg "BYE") == EQ)

preProcess :: String -> String
preProcess msg = (fmap toUpper msg)

postProcess :: String -> String
postProcess msg = case (parse random "" msg) of
		       Left _ -> msg
		       Right a -> a

getResponse :: [Parser [String]] -> String -> [String] -> String
getResponse parser input str = do{
				  case parser of
					[] -> show str -- head $ tail $ tail str
					x:xs -> case (parse x "" input) of
						      Left _ -> getResponse xs input str
						      Right a -> if ((length (head a)) > (length (head str)))
								    then getResponse xs input a
								    else if ((length (head a)) < (length (head str)))
									    then getResponse xs input str
									    else if ((length (head (tail a))) < (length (head(tail str))))
										 then getResponse xs input a
										 else getResponse xs input str
			     }
		      
superParser :: String -> IO [Parser [String]]
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