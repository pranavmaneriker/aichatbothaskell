module Bot where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import System.Directory
import Char
import ParseAIML
import Directory
import System.FilePath.Posix
import Data.List
import Data.Char
import Data.Time.Clock

import Control.Monad (unless)
import Network.Socket hiding (recv)
import qualified Data.ByteString as S
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C

import ProcessBotTags

port = "3000"

main = do{ dir<-getCurrentDirectory
	 ; parser<-superParser $ dir++"/aiml"
	 ; startChat parser
	 }
	   
-- startChat parser = do
		      -- putStr "Enter your chat:"
		      -- usermsg<-getUserMsg
		      -- let processedMsg = (preProcess usermsg)
		      -- let flag = isBye processedMsg
		      -- putStr "Bot:"
		      -- let reply = getResponse parser parser processedMsg [[""],[],[""],[]] []
		      -- sendResponse reply
		      -- if flag then
			-- stopChat
			-- else do
			  -- startChat parser

startChat parser = withSocketsDo $
    do {
	   ; addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       ; let serveraddr = head addrinfos
       ; sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       ; bindSocket sock (addrAddress serveraddr)
       ; listen sock 1
	   ; loop sock parser
       ; sClose sock
	   }

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
										 ; let reply = C.pack $ getResponse parser parser processedMsg [[""],[],[""],[]] []
										 ; unless (S.null msg) $ sendAll conn reply >> talk conn parser
										 ; t <- getCurrentTime
										 ; unless (S.null msg) $ appendFile "logfile.txt" $ (show t) ++ " -> User : " ++ (C.unpack msg) ++ "\n"
										 ; unless (S.null msg) $ appendFile "logfile.txt" $ (show t) ++ " -> Bot : " ++ (C.unpack reply) ++ "\n"
										 }


getUserMsg = getLine
sendResponse = putStrLn
-- stopChat = return ()
isBye msg = ((compare msg "BYE") == EQ)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

preProcess :: String -> String
preProcess msg = preProcessedString
					where
						phaseOne = case (parse (manyTill anyChar (oneOf ".?!,")) "" (fmap toUpper msg)) of
									Left _ -> (fmap toUpper msg)
									Right a -> a
						preProcessedString = trim $ processChatSlang phaseOne

isSubstring :: String -> String -> Bool
isSubstring sub str = case str of
			   [] -> case sub of
				  [] -> True
				  _ -> False
			   x:xs -> case sub of
				  [] -> True
				  y:ys -> ((x==y) && (chkEqual ys xs)) || (isSubstring sub xs)
				  
				  
chkEqual :: String -> String -> Bool
chkEqual [] _ = True
chkEqual _ [] = False
chkEqual x y = ((head x)==(head y)) && chkEqual (tail x) (tail y)

hasSraiTag :: String -> Bool
hasSraiTag str = isSubstring "<srai>" str

postProcess :: [Parser ParserReturnType] -> [String] -> String -> String
postProcess parser visited msg = finalMsg
				where
				 pickRandom = case (parse randomElement "" msg) of
						Left _ -> msg
						Right a -> a
				 thoughtStrip = case (parse thinkElement "" pickRandom) of
						Left _ -> pickRandom
						Right a -> a
				 recursed = getSraiTags parser thoughtStrip
				 
				 finalMsg = processBotTags recursed
				 

getSraiTags :: [Parser ParserReturnType] -> String -> String
getSraiTags parser str = if hasSraiTag str then
				      case (parse (srai parser getResponse) "" str) of
				      Left _ -> str
				      Right a -> getSraiTags parser a
				    else
				      str

lengthWOSpace :: String -> Int
lengthWOSpace = lengthWOSpace1 0
				where
				lengthWOSpace1 n str = case str of
							    [] -> n
							    x:xs -> if (x == ' ')
									then lengthWOSpace1 n xs
									else lengthWOSpace1 (n+1) xs
														
contains :: [String] -> String -> Bool
contains strList str = (length $ intersect strList [str]) > 0
														
getResponse :: [Parser ParserReturnType] -> [Parser ParserReturnType] -> String -> ParserReturnType -> [String] -> String
getResponse parser parserCopy input str visited = do{
				  case parser of
					[] -> finalMsg
							where
								finalMsg = postProcess parserCopy ((head $ head $ tail $ tail str):visited) $ head $ head $ tail $ tail str
					x:xs -> case (parse x "" input) of
						      Left _ -> getResponse xs parserCopy input str visited
						      Right a -> if ((length $ head $ head a) > (length $ head $ head str)) && (not $ contains visited (head $ head $ tail $ tail a))
								    then getResponse xs parserCopy input a visited
								    else if ((length $ head $ head a) < (length $ head $ head str) && (not $ contains visited (head $ head $ tail $ tail str)))
									    then getResponse xs parserCopy input str visited
										else if ((totalLengthStarUnder a) < (totalLengthStarUnder str) && (not $ contains visited (head $ head $ tail $ tail a)))
										 then getResponse xs parserCopy input a visited
										 else getResponse xs parserCopy input str visited
-- 						      Right a -> if ((lengthWOSpace (head a)) > (lengthWOSpace (head str)) && (not $ contains visited (head $ tail $ tail a)))
-- 								    then getResponse xs parserCopy input a visited
-- 								    else if ((lengthWOSpace (head a)) < (lengthWOSpace (head str)) && (not $ contains visited (head $ tail $ tail str)))
-- 									    then getResponse xs parserCopy input str visited
-- 									    else if ((lengthWOSpace (head (tail a))) < (lengthWOSpace (head(tail str))) && (not $ contains visited (head $ tail $ tail a)))
-- 										 then getResponse xs parserCopy input a visited
-- 										 else getResponse xs parserCopy input str visited
			     }
		      
totalLengthStarUnder :: ParserReturnType -> Int
totalLengthStarUnder a = (foldr (+) 0 (map length $ head $ tail a)) + (foldr (+) 0 (map length $ head $ tail $ tail $ tail a))
		      
superParser :: String -> IO [Parser ParserReturnType]
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

