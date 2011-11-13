module ParseAIML where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

parseAimlFile :: String -> IO (Either ParseError [[String]])
parseAimlFile filePath = parseFromFile parseAiml filePath

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

parseAiml :: Parser [[String]]
parseAiml = do{ manyTill anyChar (try aimlStart)
	      ; comments
	      ; categories <- manyTill cat (try aimlEnd)
	      ; return categories
	      }

category :: Parser [String]
category = do{ catStart
	     ; skipMany (space <|> newline)
	     ; pat<-pattern
	     ; skipMany (space <|> newline)
	     ; th<-eitherThatOrEmptyString
	     ; skipMany (space <|> newline)
	     ; temp<-template
	     ; skipMany (space <|> newline)
	     ; catEnd
	     ; return [pat,th,temp]
	     }

	      
cat :: Parser [String]
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