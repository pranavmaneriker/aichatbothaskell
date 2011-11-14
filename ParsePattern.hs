module ParsePattern where

import Text.ParserCombinators.Parsec
			      
word :: Parser String
word = many1 letter

star :: Parser String
star = do{ char '*'
	 ; return "*"
	 }

underscore :: Parser String
underscore = do{ char '_'
	       ; return "_"
	       }

wordOrStarOrUnderscore :: Parser String
wordOrStarOrUnderscore = word <|> star <|> underscore

patternContent :: Parser [String]
patternContent = do{ w <- sepBy1 wordOrStarOrUnderscore separator
		   ; return w
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

genParserFromPattern :: String -> Parser String
-- genParserFromPattern pat = do{ w<-patternContent
-- 			      ; genParserFromWords w
-- 			 }
genParserFromPattern p = case (parse patternContent "" p) of
			      Left err -> do{ pzero
					    }	
			      Right x  -> genParserFromWords x
			
genParserFromWords :: [String] -> Parser String
genParserFromWords w = case w of
			    [] -> return ""
			    x:xs -> case x of
					 "*" -> do{ s<-manyTill anyChar (try (genParserFromWords xs))
						  ; return s
						  }
					 "_" -> do{ s<-manyTill anyChar (try (genParserFromWords xs))
						  ; return s
						  }
					 _   -> (string x) >> (genParserFromWords xs)