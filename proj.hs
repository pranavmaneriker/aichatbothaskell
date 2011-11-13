module Main where

import Brain
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Char

main = startChat
startChat = do
		putStr "Enter your chat:"
		usermsg<-getUserMsg
		processedMsg<-(preprocess usermsg)
		flag<-isBye processedMsg
		putStr "Bot:"
		reply<-getResponse processedMsg
		sendResponse reply
		if flag then
		  stopChat
		  else do
		    startChat


getUserMsg = getLine
sendResponse = putStrLn
stopChat = return ()
isBye msg = return ((compare msg "BYE") == EQ)

preprocess :: String -> IO String
preprocess msg = return (fmap toUpper msg)

-- Defining parsers

parseString :: String -> Parser String
parseString str = string str

parseHi :: Parser String
parseHi = parseString "HI"

anyString :: Parser String
anyString = many1 anyChar
	      
word :: Parser String
word = many1 letter

sentence :: Parser [String]
sentence = do{ words <- sepBy1 word separator
	     ; many punctuation
             ; return words
             }
                
separator :: Parser ()
separator = skipMany1 (space <|> punctuation)

punctuation :: Parser Char
punctuation = 	oneOf ".,?!"

parserContainingp :: Parser a -> Parser a
parserContainingp p = do{ 
			  found<-p
			; return found
			}

parseAfterStar :: Parser String -> Parser String -- Parses *p
parseAfterStar p = manyTill (letter <|> punctuation <|> space) (try p)

getResponse :: String -> IO String
getResponse input = case (parse (parseAfterStar parseHi) "" input) of
			   Left err -> do{ return unknownInput
					 }
			   Right x  -> return "Hi"

