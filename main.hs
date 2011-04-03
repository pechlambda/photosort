module Main where

import System( getArgs )
import Manager
import GUI

main :: IO ()
main = getArgs >>= launch

--должен быть единственный аргумент с путём до фотографий, иначе запускаем gui
launch :: [String] -> IO ()
launch [x] = processDirectory x
launch  _  = startGUI
            
