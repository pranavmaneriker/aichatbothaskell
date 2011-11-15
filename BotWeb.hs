module Main where

import Network.CGI
import Text.XHtml
import Bot
import ParseAIML
import Directory
import System.IO.Unsafe
 
inputForm = ("The bot is unavailable at the moment due to a problem with the request/server. Please check back later or check the format of your request." ++ "<br />")
 
greet n = ("<i>You: " ++ n ++ "</i><br />" ++ "Bot: " ++ unsafePerformIO (ask n) ++ "<br />")

ask usermsg = do { dir<-getCurrentDirectory
				 ; parser<-superParser $ dir++"/aiml"
				 ; let processedMsg = (preprocess usermsg)
				 ; return (getResponse parser processedMsg)
				 }
				
cgiMain = do{ mn <- getInput "question"
			; let x = maybe inputForm greet mn
            ; output x
			}
			
main = runCGI $ handleErrors cgiMain
