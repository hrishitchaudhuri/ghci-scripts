getLine :: IO String
getLine = do
            c <- getChar
            if (c == '\n')
            then
               return ""
            else
               do
                 cs <- Main.getLine
                 return (c:cs)
