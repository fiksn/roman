import Roman
import Data.Maybe(fromMaybe)

main :: IO ()
main = interact $ fromMaybe "" . convert . filter (/= '\n')
