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

genParserFromPattern :: String -> (String,String) -> Parser (String,String)
-- genParserFromPattern pat = do{ w<-patternContent
-- 			      ; genParserFromWords w
-- 			 }
genParserFromPattern p tuple = case (parse patternContent "" p) of
				    Left err -> do{ pzero
						  }	
				    Right x  -> genParserFromWords x tuple

manyTill1 :: Parser Char -> ((String,String) -> Parser (String,String)) -> (String,String) -> Parser (String,String)
manyTill1 p end (e,st) = do{ output<-p
			  ; manyTill0 p end (e++[output],st++[output])
			  }
			  <|>
			  do{ fail ""
			    }

manyTill0 :: Parser Char -> ((String,String) -> Parser (String,String)) -> (String,String) -> Parser (String,String)
manyTill0 p end (e,st) = do{ (e1,st1)<-(try $ end (e,st))
			   ; return (e1,st1)
			   }
			   <|>
			 do{ manyTill1 p end (e,st)
			   }

eol :: (String,String) -> Parser (String,String)
eol (p,pstar) = do{ eof
		  ; return (p,pstar)
		  }
	 

genParserFromWords :: [String] -> (String,String) -> Parser (String,String)
genParserFromWords w (p,pstar) = case w of
				    [] -> eol (p,pstar)
				    x:xs -> case x of
						"*" -> do{ manyTill1 anyChar (genParserFromWords xs) (p,pstar)
							  }
						"_" -> do{ manyTill1 anyChar (genParserFromWords xs) (p,pstar)
							  }
						_   -> do{ p1<-(string x)
							 ; (skipMany (space <|> punctuation))
							 ; (genParserFromWords xs (p++p1,pstar))
							 }