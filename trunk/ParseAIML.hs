module ParseAIML where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import ParsePattern

import Random

parseAimlFiles :: [String] -> Bool -> IO [Parser [String]]
parseAimlFiles files exact = case files of
							[] -> return [] --- $ [fail ["",""]]
							x:xs -> do{ p<-parseAimlFile x exact
								  ; ps<-parseAimlFiles xs exact
								  ; return $ joinParsers p ps
								  }

parseAimlFile :: String -> Bool -> IO [Parser [String]]
parseAimlFile filePath exact = do{ p<-(parseFromFile (parseAiml exact) filePath)
							   ; case p of
									Left _ -> return [] -- $ fail ["",""]
									Right x -> return x
							   }


joinParsers :: [Parser [String]] -> [Parser [String]] -> [Parser [String]]
joinParsers p1 p2 = case p1 of
			 [] -> p2
			 x:xs -> joinParsers xs $ x:p2

-- (<@>) :: Parser String -> Parser String -> Parser String
-- (p1 <@> p2) = case (parse p1 "" str) of
-- 		       Left err -> p2
-- 		       Right x -> case (parse p2 "" str) of
-- 					Left err -> p1
-- 					Right y -> if ( (length x) > (length y) )
-- 						      then p2 else p1
						     
						     
					

aimlStart :: Parser String
aimlStart = string "<aiml version=\"1.0\">"

aimlEnd :: Parser String
aimlEnd = string "</aiml>"

catStart :: Parser String
catStart = string "<category>"

catEnd :: Parser String
catEnd = string "</category>"

patStart :: Parser String
patStart = string "<pattern>"

patEnd :: Parser String
patEnd = string "</pattern>"

thatStart :: Parser String
thatStart = string "<that>"

thatEnd :: Parser String
thatEnd = string "</that>"

tempStart :: Parser String
tempStart = string "<template>"

tempEnd :: Parser String
tempEnd = string "</template>"

commentStart :: Parser String
commentStart = string "<!--"

commentEnd :: Parser String
commentEnd = string "-->"

randomStart :: Parser String
randomStart = string "<random>"

randomEnd :: Parser String
randomEnd = string "</random>"

sraiStart :: Parser String
sraiStart = string "<srai>"

sraiEnd :: Parser String
sraiEnd = string "</srai>"

liStart :: Parser String
liStart = string "<li>"

liEnd :: Parser String
liEnd = string "</li>"

comment :: Parser String
comment = do{ try commentStart
	    ; manyTill anyChar (try commentEnd)
	    }

comments :: Parser String
comments = do{ many (space <|> newline)
	     ; comment
	     ; do{ many (space <|> newline) 
		 ; comments
	         }
	     ; many (space <|> newline)
	     }
	   <|> many (space <|> newline)

parseAiml :: Bool -> Parser [Parser [String]]
parseAiml exact = do{ manyTill anyChar (try aimlStart)
				  ; comments
				  ; categories <- manyTill (cat exact) (try aimlEnd)
				  ; return categories
				  }

category :: Bool ->Parser (Parser [String])
category exact = do{ catStart
				 ; skipMany (space <|> newline)
				 ; pat<-pattern
				 ; skipMany (space <|> newline)
				 ; th<-eitherThatOrEmptyString
				 ; skipMany (space <|> newline)
				 ; temp<-template
				 ; skipMany (space <|> newline)
				 ; catEnd
				 ; return (buildParser pat th temp exact)
				 }

	      
cat :: Bool -> Parser (Parser [String])
cat exact = do{ skipMany (space <|> newline)
	; c<-(category exact)
	; skipMany (space <|> newline)
	; return c
	}

pattern :: Parser String
pattern = do{ patStart
	    ; p<-manyTill anyChar (try patEnd)
	    ; return p
	    }
	    
template :: Parser String
template = do{ tempStart
	    ; t<-manyTill anyChar (try tempEnd)
	    ; return t
	    }
	    
that :: Parser String
that	 = do{ thatStart
	    ; t<-manyTill anyChar (try thatEnd)
	    ; return t
	    }

li :: Parser String
li = do{ liStart
       ; liStr<-manyTill anyChar (try liEnd)
       ; return liStr
       }

liWithSpace :: Parser String
liWithSpace = do{ skipMany space
		; t <- li
		; skipMany space
		; return t
		}

randomElement :: Parser String
randomElement = do{ prev <- manyTill anyChar (try randomStart)
	   ; t<-manyTill liWithSpace (try randomEnd)
	   ; nxt <- many anyChar
	   ; let r = mkStdGen 18276
	   ; let (rawTargetNum, _) = next r
	   ; let index = rawTargetNum `mod` (length t)
	   ; return $ prev ++ (elementAt index t) ++ nxt
	   }
	  <|>
	  do{ t <- many anyChar
		  ; return $ "fail: " ++ t
		  }
	
srai :: Parser (String, String, String)
srai = do { prev <- manyTill anyChar (try sraiStart)
		  ; t <- manyTill anyChar (try sraiEnd)
		  ; nxt <- many anyChar
		  ; return (prev, t, nxt)
		  }
	
elementAt :: Int -> [String] -> String
elementAt index str = case index of
			   0 -> head str
			   _ -> elementAt (index-1) (tail str)

eitherThatOrEmptyString :: Parser String
eitherThatOrEmptyString = (try that) <|> (return "")

buildParser :: String -> String -> String -> Bool -> Parser [String]
buildParser pat th temp exact = if exact
									then try (do{ p<-(string pat) 
												; return [p,"",temp]
												}
											  )
									else try (do{(p1,pstar)<-(genParserFromPattern pat ("",""))
												; return [p1,pstar,temp]
												}
											  )