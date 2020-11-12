-- Data type to represent a decsion tree
data DTree = Node String [(Char,DTree)] | Leaf String
type Matrix a = [[a]]

instance Show DTree where
  show a = dTreeToStr a 0

-- Converts a decision tree to a string so it can be printed
dTreeToStr :: DTree -> Int -> String
dTreeToStr (Node attr l) nspaces = attr ++ "\n" ++ concatList (nspaces+1) (map (\(x,y) -> (x, dTreeToStr y (nspaces+2))) l)
  where 
    concatList _ [] = ""
    concatList nspaces (x:xs) = ([1..nspaces] >> " ") ++ [fst x] ++ "\n" ++ ([1..nspaces+1] >> " ") ++ snd x ++ concatList nspaces xs
dTreeToStr (Leaf s) _ = s ++ "\n"

-- List of attributes names
attributes = ["cap-shape","cap-surface","cap-color","bruises?","odor","gill-attachment","gill-spacing","gill-size","gill-color","stalk-shape","stalk-root","stalk-surface-above-ring","stalk-surface-below-ring","stalk-color-above-ring","stalk-color-below-ring","veil-type","veil-color","ring-number","ring-type","spore-print-color","population","habitat"]

-- Main program, it reads the data from the file and constructs the decision tree
main :: IO ()
main = do
    contents <- readFile "agaricus-lepiota.data"
    let d = transpose $ lines $ filter (/=',') contents
    let classification = head d
    let dat = tail d
    let arbre = buildDTree attributes classification dat
    print arbre
    classificationIO arbre

-- Program to classify interactively a mushroom
classificationIO :: DTree -> IO ()
classificationIO (Node attr l) = do
  let options = foldl (\x y -> x ++ ", " ++ [fst y]) "" l
  putStrLn $ "Which " ++ attr ++ "?\nOptions:" ++ tail options
  line <- getLine
  let tree2 = lookup (head line) l
  case tree2 of
    Just a -> classificationIO a
    Nothing -> do
      putStrLn "Incorrect option"
      classificationIO (Node attr l)
  
classificationIO (Leaf s) = do
  putStrLn s


-- Builds the decision tree from a list of attributes, and the data matrix
buildDTree :: [String] -> [Char] -> Matrix Char -> DTree
-- If there's only one attribute I classify the examples as the most present in the examples
buildDTree [attr] classification d = Node attr (map f (unique $ head d))
  where 
    f x
      | countBy (==('p',x)) zipped >= countBy (==('e',x)) zipped = (x, Leaf "poisonous")
      | otherwise = (x, Leaf "edible")
      where
        zipped = zip classification $ head d

buildDTree attributes classification d = Node (attributes !! index) (map f (unique $ d !! index))
  where 
    index = getBestAttr classification d
    f x
      | all (\y -> fst y == 'p') (filter (\y -> snd y == x) (zip classification (d !! index))) = (x, Leaf "poisonous")
      | all (\y -> fst y == 'e') (filter (\y -> snd y == x) (zip classification (d !! index))) = (x, Leaf "edible")
      | otherwise = (x, buildDTree attributes_f (head data_f) (tail data_f))
      where 
        data_f = filterData classification d x index
        attributes_f = take index attributes ++ drop (index+1) attributes

-- Returns only the lines that the attribute on position index is equal to t and drops the attribute
filterData :: [Char] -> Matrix Char -> Char -> Int -> Matrix Char
filterData classification d t index = take (index+1) lines_t ++ drop (index+2) lines_t
  where 
    lines_filtered = filter (\l -> (l !! (index + 1)) == t) (transpose $ classification:d)
    lines_t = transpose lines_filtered

-- Tranpose a matrix
transpose :: Matrix a -> Matrix a
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- Returns a list with no repetitions
unique :: [Char] -> [Char]
unique [] = []
unique (x : xs) = x : unique (filter (x /=) xs)

-- Counts the elements that fulfill a condition
countBy :: (a -> Bool) -> [a] -> Int
countBy cond = foldr (\x cnt -> if cond x then cnt + 1 else cnt) 0

-- Returns a lis of pairs containing the maximum elements and their index of a list
maxims :: (Ord a) => [a] -> [(a, Int)]
maxims l = recmaxim l 0
  where
    recmaxim :: (Ord a) => [a] -> Int -> [(a, Int)]
    recmaxim [x] xi = [(x, xi)]
    recmaxim (x:xs) xi
      | x > t     = [(x, xi)]
      | x == t    = (x, xi):l
      | otherwise = l
      where 
        l = recmaxim xs (xi + 1)
        t = fst $ head l

-- Computes the number number of example that will be filtered if we make a node with the attribute
computeNFiltered :: [Char] -> [Char] -> Int
computeNFiltered classification attribute = sum $ map (f (zip classification attribute)) (unique attribute)
  where 
    f pairs c
      | countBy (== ('p',c)) pairs == 0 = countBy (== ('e',c)) pairs
      | countBy (== ('e',c)) pairs == 0 = countBy (== ('p',c)) pairs
      | otherwise = 0

-- Computes accuracy for an attribute
computeAccuracy :: [Char] -> [Char] -> Int
computeAccuracy classification attribute = sum $ map (f (zip classification attribute)) (unique attribute)
  where f pairs c = maximum [countBy (== ('p',c)) pairs, countBy (== ('e',c)) pairs]

-- Returns the index of the best attribute to split the dataset
getBestAttr :: [Char] -> Matrix Char -> Int
getBestAttr classification d = snd $ maximum $ map (\(_,index) -> (computeNFiltered classification (d !! index),index)) (maxims $ map (computeAccuracy classification) d)