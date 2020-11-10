import Data.List
import Data.Ord


data DTree = Node String [(Char,DTree)] | Leaf String
type Matrix a = [[a]]


instance Show DTree where
  --show (Node atr l) = atr ++ "\n" ++ foldl (++) "\n" (map f l)
    --where f el = fst el : "\n" ++ "\t"++ (show (snd el))
  --show (Node atr l) = atr ++":\n" ++ (foldl f "" l)
    --where f i el = i ++ "\n" ++ show el
  --show (Leaf s) = s
  show a = dTreeToStr a 0

dTreeToStr :: DTree -> Int -> String

dTreeToStr (Node attr l) level = attr ++ "\n" ++ (concatList (level+1) (map (\(x,y) -> (x, dTreeToStr y (level+2))) l))
  where 
    concatList _ [] = ""
    concatList ntabs (x:xs) = ([1..ntabs] >> " ") ++ [fst x] ++ "\n" ++ ([1..ntabs+1] >> " ") ++ snd x ++(concatList ntabs xs)
dTreeToStr (Leaf s) _ = s ++ "\n"

main :: IO ()
main = do
    contents <- readFile "short.data"
    let attributes = ["cap-shape","cap-color","gill-color"]
    let d = (transpose $  lines $ filter (/=',') contents)
    let classification = head d
    let dat = tail d
    print (getBestAttr classification dat)
    let arbre = (buildDTree attributes classification dat)
    print arbre
    
    

buildDTree :: [String] -> [Char] -> Matrix Char -> DTree

buildDTree (attr:[]) classification d = Node (attr) (map (\x -> f x) (unique $ d !! 0))
  where 
    f x
      | (countBy (==('p',x)) zipped) >= (countBy (==('e',x)) zipped) = (x, Leaf "poisonous")
      | otherwise = (x, Leaf "edible")
      where
        zipped = zip classification (d !! 0)

buildDTree attributes classification d = Node (attributes !! index) (map (\x -> f x) (unique $ d !! index))
  where 
    index = getBestAttr classification d
    f x
      | all (\y -> fst y == 'p') (filter (\y -> snd y == x) (zip classification (d !! index))) = (x, Leaf "poisonous")
      | all (\y -> fst y == 'e') (filter (\y -> snd y == x) (zip classification (d !! index))) = (x, Leaf "edible")
      | otherwise = (x, buildDTree attributes_f (head data_f) (tail data_f))
      where 
        data_f = filterData classification d x index
        attributes_f = take index attributes ++ drop (index+1) attributes

filterData :: [Char] -> Matrix Char -> Char -> Int -> Matrix Char

filterData classification d t index = take (index+1) lines_t ++ drop (index+2) lines_t
  where 
    lines_filtered = filter (\l -> (l !! (index + 1)) == t) (transpose $ classification:d)
    lines_t = transpose lines_filtered


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