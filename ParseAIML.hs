module ParseAIML where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import ParsePattern

parseAimlFiles :: [String] -> IO (Parser String)
parseAimlFiles files = case files of
			    [] -> return pzero
			    x:xs -> do{ p<-parseAimlFile x
				      ; ps<-parseAimlFiles xs
				      ; return (p <|> ps)
				      }

parseAimlFile :: String -> IO (Parser String)
parseAimlFile filePath = do{ p<-(parseFromFile parseAiml filePath)
			   ; case p of
				    Left _ -> return pzero
				    Right x -> return x
			   }

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

parseAiml :: Parser (Parser String)
parseAiml = do{ manyTill anyChar (try aimlStart)
	      ; comments
	      ; categories <- manyTill cat (try aimlEnd)
	      ; return (choice categories)
	      }

category :: Parser (Parser String)
category = do{ catStart
	     ; skipMany (space <|> newline)
	     ; pat<-pattern
	     ; skipMany (space <|> newline)
	     ; th<-eitherThatOrEmptyString
	     ; skipMany (space <|> newline)
	     ; temp<-template
	     ; skipMany (space <|> newline)
	     ; catEnd
	     ; return (buildParser pat th temp)
	     }

	      
cat :: Parser (Parser String)
cat = do{ skipMany (space <|> newline)
	; c<-category
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
	    
eitherThatOrEmptyString :: Parser String
eitherThatOrEmptyString = (try that) <|> (return "")

buildParser :: String -> String -> String -> Parser String
buildParser pat th temp = try (do{ genParserFromPattern pat
				 ; return temp
				 }
			      )
