


data DTree = Node String [(Char,DTree)] | Leaf String
type Matrix a = [[a]]

instance Show DTree where
  show a = dTreeToStr a 0

dTreeToStr :: DTree -> Int -> String

dTreeToStr (Node attr l) level = attr ++ "\n" ++ (concatList (level+1) (map (\(x,y) -> (x, dTreeToStr y (level+2))) l))
  where 
    concatList _ [] = ""
    concatList ntabs (x:xs) = ([1..ntabs] >> " ") ++ [fst x] ++ "\n" ++ ([1..ntabs+1] >> " ") ++ snd x ++(concatList ntabs xs)

dTreeToStr (Leaf s) _ = s ++ "\n"

attributes = ["cap-shape","cap-surface","cap-color","bruises?","odor","gill-attachment","gill-spacing","gill-size","gill-color","stalk-shape","stalk-root","stalk-surface-above-ring","stalk-surface-below-ring","stalk-color-above-ring","stalk-color-below-ring","veil-type","veil-color","ring-number","ring-type","spore-print-color","population","habitat"]


main :: IO ()
main = do
    contents <- readFile "agaricus-lepiota.data"
    let d = (transpose $  lines $ filter (/=',') contents)
    let classification = head d
    let dat = tail d
    let arbre = (buildDTree attributes classification dat)
    print arbre
    classificationIO arbre

classificationIO :: DTree -> IO ()
classificationIO (Node attr l) = do
  putStrLn $ "Which " ++ attr ++ "?"
  line <- getLine
  let tree2 = lookup (line !! 0) l
  case tree2 of
    Just a -> classificationIO a
    Nothing -> do
      putStrLn "Incorrect option"
      classificationIO (Node attr l)
  
classificationIO (Leaf s) = do
  putStrLn s



    
    

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
transpose :: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- Retorna la llista sense repeticions
unique :: [Char] -> [Char]
unique [] = []
unique (x : xs) = x : unique (filter (x /=) xs)

-- Compta el numero d'elements que compleixen una condició
countBy :: (a -> Bool) -> [a] -> Int
countBy cond = foldr (\x cnt -> if cond x then cnt + 1 else cnt) 0

maxim :: (Ord a) => [a] -> (a, Int)
maxim l = pmaxim l 0
  where
    pmaxim :: (Ord a) => [a] -> Int -> (a, Int) -- Internal function to do the work
    pmaxim [] _  = error "Empty list"           -- List is empty, error
    pmaxim [x] xi = (x, xi)                     -- List has one item, return it and the index
    pmaxim (x:xs) xi                            -- More than one item, break list apart
      | x > t     = (x, xi)                     -- If current item is bigger, return it and its index
      | otherwise = (t, ti)                     -- If list tail has a bigger item, return that
      where (t, ti) = pmaxim xs (xi + 1)        -- Get max of tail of the list


-- Retorna l'index de l'element més gran (en cas d'empat el de més a la dreta)
maxIndex ::  Ord a => [a] -> Int
--maxIndex = fst . maximumBy (comparing snd) . zip [0..]
maxIndex = snd . maxim

computeAccuracy :: [Char] -> [Char] -> Int
computeAccuracy classification attribute = sum $ map (\x -> f (zip classification attribute) x) (unique attribute)
  where f pairs c = maximum [countBy (== ('p',c)) pairs, countBy (== ('e',c)) pairs]

getBestAttr :: [Char] -> Matrix Char -> Int
getBestAttr classification d = maxIndex $ map (\x -> computeAccuracy classification x) d