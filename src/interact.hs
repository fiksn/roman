import Roman
import Data.Maybe(fromMaybe)

main = interact $ fromMaybe "" . convert . filter (/= '\n')
