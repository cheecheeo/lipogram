{-# LANGUAGE TemplateHaskell #-}
import qualified Data.List          as L
import qualified Data.Ord           as O

import qualified Data.Lens.Common   as Lens
import qualified Data.Lens.Template as LTH

data CountState = CountState {
  _lineCountEnd         :: Int,
  _byteCountEnd         :: Int,
  _lipogramCount        :: Int,
  _currentLipogramCount :: Int
} deriving (Show)

$( LTH.makeLens ''CountState )

-- | Apply @f@ to each value whose key satisfies @p key@
updateState :: [(a, b)] -> (a -> Bool) -> (b -> b) -> [(a, b)]
updateState state p f = map (\(x, t) -> (x, (if p x then f else id) t)) state

-- | Increment the currentLipogramCount of all @CountState@s
incLipogramCounts :: [(a, CountState)] -> [(a, CountState)]
incLipogramCounts state = updateState state (const True) (Lens.modL currentLipogramCount succ)

processCharacter :: (Int, Int, [(Char, CountState)]) -> Char -> (Int, Int, [(Char, CountState)])
processCharacter (byteCount0, lineCount0, state) c =
      (
      byteCount1,
      lineCount1,
      incLipogramCounts $
        updateState state (\lipogramVariable -> c == lipogramVariable)
          (\(CountState lineEnd end maxCount countSoFar) ->
            if countSoFar > maxCount then
              CountState lineCount1 byteCount0 countSoFar 0
            else
              CountState lineEnd end maxCount 0)
      )
  where byteCount1 = succ byteCount0
        lineCount1 = (if c == '\n' then succ else id) lineCount0

thd :: (a, b, c) -> c
thd (_, _, x) = x

lipograms :: [Char] -> [Char] -> [(Char, CountState)]
lipograms text chars = thd $
  L.foldl'
    processCharacter
    (initialByteCount, initialLineCount, initialState)
    text
  where initialState = zip chars (repeat (CountState 0 0 0 0))
        initialByteCount = 1
        initialLineCount = 1

main :: IO ()
main = do
  let fileName = "hamlet.txt"
  t <- readFile fileName
  let (letter, countState) = L.maximumBy (O.comparing (Lens.getL lipogramCount . snd)) (lipograms t ['e', 't', 'a', 'o'])
  putStrLn $ "The longest lipogram for character "
            ++ (show letter) ++ " in '" ++ fileName ++ "' ends on character "
            ++ (show $ Lens.getL byteCountEnd countState)
            ++ " on line " ++ (show $ Lens.getL lineCountEnd countState)
            ++ " and lasts " ++ (show $ Lens.getL lipogramCount countState)
            ++ " characters."
