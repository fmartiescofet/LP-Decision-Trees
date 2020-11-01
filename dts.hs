
data DTree = Node String [(String,DTree)] | Leaf String deriving Show
type Matrix a = [[a]]

main :: IO ()
main = do
    contents <- readFile "short.data"
    print (transpose $  lines $ filter (/=',') contents)
    
    

buildDTree :: [String] -> [String] -> DTree

buildDTree attr dat = Leaf "S"

-- Tranposa una matriu
transpose :: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- Retorna la llista sense repeticions
unique :: [Char] -> [Char]
unique [] = []
unique (x : xs) = x : unique (filter (x /=) xs)

countBy :: (a -> Bool) -> [a] -> Int
countBy cond = foldr (\x cnt -> if cond x then cnt + 1 else cnt) 0


computeAccuracy :: [Char] -> [Char] -> [Int]

computeAccuracy classification attribute = map (\x -> f (zip classification attribute) x) (unique attribute)
  where f pairs c = maximum [countBy (\x -> fst x == 'p' && snd x == c) pairs, countBy (\x -> fst x == 'e' && snd x == c) pairs]

