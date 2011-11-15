module Main where

import Network.CGI
import Text.XHtml

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
 
port = "3000"
 
inputForm = ("The bot is unavailable at the moment due to a problem with the request/server. Please check back later or check the format of your request." ++ "<br />")
 
greet n = do
			m <- liftIO (ask n)
			return ("<i>You: " ++ n ++ "</i><br />" ++ "Bot: " ++ m ++ "<br />")

ask usermsg = withSocketsDo $
							do addrinfos <- getAddrInfo Nothing (Just "") (Just port)
							   let serveraddr = head addrinfos
							   sock <- socket (addrFamily serveraddr) Stream defaultProtocol
							   connect sock (addrAddress serveraddr)
							   sendAll sock $ C.pack usermsg
							   msg <- recv sock 1024
							   sClose sock
							   return (C.unpack msg)
				
cgiMain = do{ mn <- getInput "question"
			; x <- maybe (return inputForm) greet mn
            ; output x
			}
			
main = runCGI $ handleErrors cgiMain
