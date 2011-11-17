module ProcessBotTags where

import Data.List.Utils

processBotTags str = processedStr
					 where
						replace0 = replace "<bot name=\"botmaster\"/>" "botmasters" str
						replace1 = replace "<bot name=\"genus\"/>" "AI bots" replace0		--check
						replace2 = replace "<bot name=\"species\"/>" "chat bots" replace1	--check
						replace3 = replace "<bot name=\"order\"/>" "electric" replace2		--check
						replace4 = replace "<bot name=\"location\"/>" "Kanpur, UP" replace3
						replace5 = replace "<bot name=\"gender\"/>" "Male" replace4
						replace6 = replace "<bot name=\"family\"/>" "bots" replace5			--check
						replace7 = replace "<bot name=\"name\"/>" "Chatty" replace6
						replace8 = replace "<bot name=\"kingdom\"/>" "ChatBots" replace7	--check
						replace9 = replace "<bot name=\"age\"/>" "1 week" replace8
						replace10 = replace "<bot name=\"birthplace\"/>" "Kanpur, UP" replace9
						replace11 = replace "<bot name=\"favoritefood\"/>" "electricity" replace10
						replace12 = replace "<bot name=\"master\"/>" "Shantanu & Saurabh" replace11
						replace13 = replace "<bot name=\"emotions\"/>" "Happy, Sad" replace12
						replace14 = replace "<bot name=\"emotion\"/>" "happy" replace13
						replace15 = replace "<bot name=\"party\"/>" "liberist" replace14
						replace16 = replace "<bot name=\"celebrities\"/>" "Salman, Shahrukh" replace15
						replace17 = replace "<bot name=\"arch\"/>" "Intel 64 bit" replace16
						replace18 = replace "<bot name=\"version\"/>" "0.1" replace17
						replace19 = replace "<bot name=\"talkabout\"/>" "anything" replace18
						replace20 = replace "<bot name=\"birthday\"/>" "10th November, 2011" replace19
						replace21 = replace "<bot name=\"language\"/>" "Haskell" replace20
						replace22 = replace "<bot name=\"phylum\"/>" "Bot" replace21			--check
						replace23 = replace "<bot name=\"website\"/>" "http://www.program-o.com" replace22
						replace24 = replace "<bot name=\"favoritebook\"/>" "Wheel of Time by Robert Jordan" replace23
						replace25 = replace "<bot name=\"favoritesport\"/>" "Football" replace24
						replace26 = replace "<bot name=\"favoritesong\"/>" "While my guitar gently weeps" replace25
						replace27 = replace "<bot name=\"hockeyteam\"/>" "Nicks" replace26
						replace28 = replace "<bot name=\"favoritemovie\"/>" "Godfather" replace27
						replace29 = replace "<bot name=\"favoriteband\"/>" "Children of Bodom" replace28
						replace30 = replace "<bot name=\"etype\"/>" "simple" replace29		--check
						replace31 = replace "<bot name=\"class\"/>" "ChatBot" replace30
						replace32 = replace "<bot name=\"email\"/>" "saraswat@iitk.ac.in/saurax@iitk.ac.in" replace31
						replace33 = replace "<bot name=\"feeling\"/>" "Happy" replace32
						replace34 = replace "<bot name=\"feelings\"/>" "Happy, Sad" replace33
						replace35 = replace "<bot name=\"sign\"/>" "Virgo" replace34
						replace36 = replace "<bot name=\"nationality\"/>" "Indian" replace35
						replace37 = replace "<bot name=\"looklike\"/>" "laptop" replace36
						replace38 = replace "<bot name=\"forfun\"/>" "chat" replace37
						replace39 = replace "<bot name=\"wear\"/>" "plastic" replace38
						replace40 = replace "<bot name=\"kindmusic\"/>" "rock" replace39
						replace41 = replace "<bot name=\"os\"/>" "Windows" replace40
						replace42 = replace "<bot name=\"question\"/>" "What do you like?" replace41
						replace43 = replace "<bot name=\"vocabulary\"/>" "100000" replace42 -- number of words
						replace44 = replace "<bot name=\"dailyclients\"/>" "10" replace43
						replace45 = replace "<bot name=\"nclients\"/>" "1" replace44
						replace46 = replace "<bot name=\"totalclients\"/>" "100" replace45
						replace47 = replace "<bot name=\"ndevelopers\"/>" "2" replace46
						replace48 = replace "<bot name=\"memory\"/>" "50 MB" replace47
						replace49 = replace "<bot name=\"hair\"/>" "0 m" replace48
						replace50 = replace "<bot name=\"build\"/>" "v0.1" replace49
						replace51 = replace "<bot name=\"orientation\"/>" "straight" replace50
						replace52 = replace "<bot name=\"boyfriend\"/>" "none" replace51
						replace53 = replace "<bot name=\"girlfriend\"/>" "Eliza" replace52
						replace54 = replace "<bot name=\"baseballteam\"/>" "Chicago Bulls" replace53
						replace55 = replace "<bot name=\"footballteam\"/>" "Manchester United" replace54
						replace56 = replace "<bot name=\"president\"/>" "Pratibha Patil" replace55
						replace57 = replace "<bot name=\"state\"/>" "Uttar Pradesh" replace56
						processedStr = replace57
						
processChatSlang str = processedStr
						where
						replace0 = replace "'" "" $ " " ++ str ++ " "
						replace1 = replace " IM " "I AM " replace0
						replace2 = replace " U " " YOU " replace1
						replace3 = replace " R " " ARE " replace2
						replace4 = replace " NT " " NOT " replace3
						replace5 = replace " LETS " " LET US " replace4
						replace6 = replace " WANNA " " WANT TO " replace5
						replace7 = replace " HV " " HAVE " replace6
						replace8 = replace " WS " " WAS " replace7
						replace9 = replace " DOIN " " DOING " replace8
						replace10 = replace " BBYE " " BYE " replace9
						replace11 = replace " UR " " YOUR " replace10
						replace12 = replace " GUD " " GOOD " replace11
						replace13 = replace " WASSUP " " HOW ARE YOU " replace12
						replace14 = replace " THATS " " THAT IS " replace13
						replace15 = replace " YEAH " " YES " replace14
						replace16 = replace " DONT " " DO NOT " replace15
						replace17 = replace " DERE " " ARE YOU PRESENT " replace16
						replace18 = replace " WHT " " WHAT " replace17
						processedStr = replace18