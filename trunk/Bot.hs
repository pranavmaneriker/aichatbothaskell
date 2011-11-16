module Bot where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import System.Directory
import Char
import ParseAIML
import Directory
import System.FilePath.Posix
import Data.List

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
		      let reply = getResponse parser parser processedMsg ["","",""]
		      sendResponse reply
		      if flag then
			stopChat
			else do
			  startChat parser

-- startChat parser = withSocketsDo $
    -- do {
	   -- ; addrinfos <- getAddrInfo
                    -- (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    -- Nothing (Just port)
       -- ; let serveraddr = head addrinfos
       -- ; sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       -- ; bindSocket sock (addrAddress serveraddr)
       -- ; listen sock 1
	   -- ; loop sock parser
       -- ; sClose sock
	   -- }

loop sock parser = do	 {
						 ;	putStrLn "Waiting for connection..."
						 ;	(conn, _) <- accept sock
						 ;	putStrLn "Client connected."
						 ;	talk conn parser
						 ;	sClose conn
						 ;	putStrLn "Client disconnected."
						 ;	loop sock parser
						 }
				where
				  talk conn parser =
									  do {
										 ; msg <- recv conn 1024
										 ; let processedMsg = (preProcess $ C.unpack msg)
										 ; let reply = C.pack $ getResponse parser parser processedMsg ["","",""]
										 ; unless (S.null msg) $ sendAll conn reply >> talk conn parser
										 }


getUserMsg = getLine
sendResponse = putStrLn
stopChat = return ()
isBye msg = ((compare msg "BYE") == EQ)

preProcess :: String -> String
preProcess msg = case (parse (manyTill anyChar (oneOf ".?!,")) "" (fmap toUpper msg)) of
					Left _ -> (fmap toUpper msg)
					Right a -> a

postProcess :: [Parser [String]] -> String -> String
postProcess parser msg = finalMsg
				where
				 pickRandom = case (parse randomElement "" msg) of
						Left _ -> msg
						Right a -> a
				 recursed = case (parse srai "" pickRandom) of
						Left _ -> pickRandom
						Right (p,a,n) -> p ++ (getResponse parser parser a ["","",""]) ++ n
				 finalMsg = pickRandom --recursed

lengthWOSpace = lengthWOSpace1 0
				where
					lengthWOSpace1 n str = case str of
											[] -> n
											x:xs -> if (x == ' ')
														then lengthWOSpace1 n xs
														else lengthWOSpace1 (n+1) xs
														
contains :: [String] -> String -> Bool
contains strList str = (length $ intersect strList [str]) > 0
														
getResponse :: [Parser [String]] -> [Parser [String]] -> String -> [String] -> String
getResponse parser parserCopy input str = do{
				  case parser of
					[] -> finalMsg
							where
								finalMsg = postProcess parserCopy $ head $ tail $ tail str
					x:xs -> case (parse x "" input) of
						      Left _ -> getResponse xs parserCopy input str
						      Right a -> if ((lengthWOSpace (head a)) > (lengthWOSpace (head str)))
								    then getResponse xs parserCopy input a
								    else if ((lengthWOSpace (head a)) < (lengthWOSpace (head str)))
									    then getResponse xs parserCopy input str
									    else if ((lengthWOSpace (head (tail a))) < (lengthWOSpace (head(tail str))))
										 then getResponse xs parserCopy input a
										 else getResponse xs parserCopy input str
			     }
		      
superParser :: String -> IO [Parser [String]]
superParser dir = do{ files<-getDirectoryContents dir
		    --; parserExact <- (parseAimlFiles (getProperFileList dir files) True)
			; parser <-(parseAimlFiles (getProperFileList dir files) False)
			; return parser
		    }

getProperFileList :: String -> [String] -> [String]
getProperFileList dir files = case files of
				   [] -> []
				   x:xs -> case (takeExtension x) of
						".aiml" -> (dir++("/"++x)):(getProperFileList dir xs)
						_ -> getProperFileList dir xs

append :: String -> String -> String
append st1 st2 = st1++st2