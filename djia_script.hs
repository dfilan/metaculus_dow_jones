{-# LANGUAGE LambdaCase #-}

import Text.Parsec
import Text.CSV
import Data.Char

today :: Float
today = 23433.16

high :: Float
high = 25000.0

highRatio :: Float
highRatio = high / today

low :: Float
low = 20000.0

lowRatio :: Float
lowRatio = low / today

isRelativelyHigh :: Float -> Float -> Bool
isRelativelyHigh now future = (future / now) > highRatio

isRelativelyLow :: Float -> Float -> Bool
isRelativelyLow now future = (future / now) < lowRatio

higherBeforeLower :: Float -> [Float] -> Bool
higherBeforeLower x list = ((daysUntil (isRelativelyHigh x) list)
                            < (daysUntil (isRelativelyLow x) list))

daysUntil :: (Float -> Bool) -> [Float] -> Int
daysUntil prop list = length $ takeWhile (not . prop) list

mapList :: (a -> [a] -> b) -> [a] -> [b]
mapList myFunc = \case
  []   -> []
  x:xs -> (myFunc x xs):(mapList myFunc xs)

fracTrue :: [Bool] -> Float
fracTrue []   = 0
fracTrue list = ((fromIntegral $ length $ filter id list)
                 / (fromIntegral $ length list))

takeWhileComparable :: [Float] -> [Float]
takeWhileComparable [] = []
takeWhileComparable (x:xs)
 | elem True $ map lowOrHigh xs = x:(takeWhileComparable xs)
 | otherwise                    = []
 where lowOrHigh y = (isRelativelyLow x y) || (isRelativelyHigh x y)

proportionHigher :: [Float] -> Float
proportionHigher = fracTrue . (mapList higherBeforeLower) . takeWhileComparable

myFile :: FilePath
myFile = "./dja-performance-report-daily.csv"

stringToFloat :: String -> Float
stringToFloat = (stringToFloat' 0 0) . (map myDigitToInt)

stringToFloat' :: Float -> Int -> [Either () Int] -> Float
stringToFloat' acc 0  = \case
  []           -> acc
  (Left ()):xs -> stringToFloat' acc 1 xs
  (Right n):xs -> stringToFloat' (10*acc + n') 0 xs
      where n' = fromIntegral n
stringToFloat' acc m = \case
  []           -> acc
  (Left ()):xs -> stringToFloat' acc m xs
  (Right n):xs -> (stringToFloat' (acc + n'*10**(-m')) (1+m) xs)
      where n' = fromIntegral n
            m' = fromIntegral m

myDigitToInt :: Char -> Either () Int
myDigitToInt '.' = Left ()
myDigitToInt x   = Right $ digitToInt x

nicelyProcess :: CSV -> [Float]
nicelyProcess =  (map stringToFloat) . concat

main :: IO ()
main = do
  myCSV <- parseCSVFromFile myFile
  print $ (proportionHigher . nicelyProcess) <$> myCSV
