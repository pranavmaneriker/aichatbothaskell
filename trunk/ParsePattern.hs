module ParsePattern where

import Text.ParserCombinators.Parsec

type LocalParserType = (String,[String],[String])

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

genParserFromPattern :: String -> LocalParserType -> Parser LocalParserType
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
			   
newManyTill1 :: Parser Char -> Parser String -> (LocalParserType -> Parser LocalParserType) -> LocalParserType -> String -> Parser LocalParserType
newManyTill1 p beforeEnd end (e,st,un) ch = case ch of 
						"*" -> do{ do{ output<-p
							      ; newManyTill0 p beforeEnd end (e++[output],appendToLastString st output,un) ch
							      }
							    <|>
							    do{ fail ""
							      }
							 }
						"_" -> do{ do{ output<-p
							       ; newManyTill0 p beforeEnd end (e++[output],st,appendToLastString un output) ch
							       }
							     <|>
							     do{ fail ""
							       }
							 }

newManyTill0 :: Parser Char -> Parser String -> (LocalParserType -> Parser LocalParserType) -> LocalParserType -> String -> Parser LocalParserType
newManyTill0 p beforeEnd end (e,st,un) ch = case ch of 
						 "*" -> do{ try (do{ sp<-beforeEnd
								   ; (e1,st1,un1)<-(end (e++sp,st,un))
								   ; return (e1,st1,un1)
								   })
							  }
							<|>
							do{ newManyTill1 p beforeEnd end (e,st,un) ch
							  }
						 "_" -> do{ try (do{ sp<-beforeEnd
								   ; (e1,st1,un1)<-(end (e++sp,st,un))
								   ; return (e1,st1,un1)
								   })
							  }
							  <|>
							do{ newManyTill1 p beforeEnd end (e,st,un) ch
							  }

eol :: LocalParserType -> Parser LocalParserType
eol (p,pstar,punder) = do{ eof
			 ; return (p,pstar,punder)
			 }

appendToLastString :: [String] -> Char -> [String]
appendToLastString arr ch = case arr of
				 [x] -> [(x++[ch])]
				 x:xs -> x:(appendToLastString xs ch)

genParserFromWords :: [String] -> LocalParserType -> Parser LocalParserType
genParserFromWords w (p,pstar,punder) = case w of
				    [x] ->  case x of
						"*" -> do{ p1<-many1 anyChar
							 ; return (p++p1,(foldr (:) [p1] pstar),punder)
							  }
						"_" -> do{ p1<-many1 anyChar
							 ; return (p++p1,pstar,(foldr (:) [p1] punder))
							 }
						_   -> do{ p1<-(string x)
							 ; return (p++p1,pstar,punder)
							 }
				    x:xs -> case x of
						"*" -> do{ newManyTill1 anyChar (many1 (space <|> punctuation)) (genParserFromWords xs) (p,(foldr (:) [""] pstar),punder) x
							  -- manyTill0 anyChar (genParserFromWords xs) (p,pstar)
							  }
						"_" -> do{ newManyTill1 anyChar (many1 (space <|> punctuation)) (genParserFromWords xs) (p,pstar,(foldr (:) [""] punder)) x
							  }
						_   -> do{ p1<-(string x)
							 ; sp<-(many1 (space <|> punctuation))
							 ; (genParserFromWords xs (p++(p1++sp),pstar,punder)) 
							 }