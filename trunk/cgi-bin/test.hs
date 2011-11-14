import Network.CGI
import Text.XHtml
 
inputForm = ("The bot is unavailable at the moment due to a problem with the request/server. Please check back later or check the format of your request." ++ "<br />")
 
greet n = ("<i>You: " ++ n ++ "</i><br />" ++ "Bot: " ++ ask n ++ "<br />")
 
ask = reverse
 
cgiMain = do mn <- getInput "question"
             let x = maybe inputForm greet mn
             output x
 
main = runCGI $ handleErrors cgiMain
