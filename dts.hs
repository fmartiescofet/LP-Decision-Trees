import Data.List
import Data.Ord


data DTree = Node String [(String,DTree)] | Leaf String deriving Show
type Matrix a = [[a]]

main :: IO ()
main = do
    contents <- readFile "short.data"
    let d = (transpose $  lines $ filter (/=',') contents)
    let classification = head d
    let dat = tail d
    print (getBestAttr classification dat)

    
    

buildDTree :: [Char] -> Matrix Char -> DTree

buildDTree classification d = Leaf "S"

-- Tranposa una matriu
--transposeM :: [[a]]->[[a]]
--transpose ([]:_) = []
--transpose x = (map head x) : transpose (map tail x)

-- Retorna la llista sense repeticions
unique :: [Char] -> [Char]
unique [] = []
unique (x : xs) = x : unique (filter (x /=) xs)

-- Compta el numero d'elements que compleixen una condició
countBy :: (a -> Bool) -> [a] -> Int
countBy cond = foldr (\x cnt -> if cond x then cnt + 1 else cnt) 0


-- Retorna l'index de l'element més gran (en cas d'empat el de més a la dreta)
maxIndex ::  Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]


computeAccuracy :: [Char] -> [Char] -> Int
computeAccuracy classification attribute = sum $ map (\x -> f (zip classification attribute) x) (unique attribute)
  where f pairs c = maximum [countBy (\x -> fst x == 'p' && snd x == c) pairs, countBy (\x -> fst x == 'e' && snd x == c) pairs]

getBestAttr :: [Char] -> Matrix Char -> Int
getBestAttr classification d = maxIndex $ map (\x -> computeAccuracy classification x) d