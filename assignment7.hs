-- Answer the following questions based the NTree definition in Lecture slides 9 Algebraic Types and slides 9-1 Huffman Code. 

-- type definitions from the slides:
data NTree = NilT | Node Integer NTree NTree
    deriving (Show, Eq, Read, Ord)

-- huffman code types from the slides:
data Bit = L | R deriving (Eq, Show)
type HCode = [Bit]
type Table = [(Char, HCode)]

-- generic tree type (for questions 6-8)
data Tree a = Leaf a Int | NodeT Int (Tree a) (Tree a)
    deriving (Show, Eq)

{-
1. Give line-by-line calculations of 
    sumTree Node 3 (Node 4 NilT NilT) NilT
    depth Node 3 (Node 4 NilT NilT) NilT
-}

{-
sumTree (Node 3 (Node 4 NilT NilT) NilT)
= 3 + sumTree (Node 4 NilT NilT) + sumTree NilT
= 3 + (4 + sumTree NilT + sumTree NilT) + sumTree NilT
= 3 + (4 + 0 + 0) + 0
= 7
-}

{-
depth (Node 3 (Node 4 NilT NilT) NilT)
= 1 + max (depth (Node 4 NilT NilT)) (depth NilT)
= 1 + max (1 + max (depth NilT) (depth NilT)) 0
= 1 + max (1 + max 0 0) 0
= 1 + max (1 + 0) 0
= 1 + max 1 0
= 1 + 1
= 2
-}

sumTree :: NTree -> Integer
sumTree NilT = 0
sumTree (Node n left right) = n + sumTree left + sumTree right

depth :: NTree -> Integer
depth NilT = 0
depth (Node n left right) = 1 + max (depth left) (depth right)

{-
2. Define functions to return the left and right hand sub-trees of an NTree.
-}

-- return the left subtree
leftSubtree :: NTree -> NTree
leftSubtree NilT = NilT -- if the tree is empty, return NilT
leftSubtree (Node _ left _) = left

-- return the right subtree
rightSubtree :: NTree -> NTree
rightSubtree NilT = NilT -- if the tree is empty, return NilT
rightSubtree (Node _ _ right) = right

{-
3. Define a function to decide whether a number is an element of an NTree.
-}

isElement :: Integer -> NTree -> Bool
isElement _ NilT = False -- base case. if the tree is empty, return False.
isElement x (Node n left right)
    | x == n        = True -- if the value is the current node, return True.
    | otherwise     = isElement x left || isElement x right -- otherwise, search both subtrees.

{-
4. Define functions to find the maximum and minimum values held in an NTree.
-}

maxValue :: NTree -> Integer
maxValue NilT = error "Tree is empty"
maxValue (Node v NilT NilT) = v
maxValue (Node v left right) = max v (max (maxValueOr v left) (maxValueOr v right))
  where
    maxValueOr defaultVal NilT = defaultVal
    maxValueOr _ (Node x l r)  = maxValue (Node x l r)

minValue :: NTree -> Integer
minValue NilT = error "Tree is empty"
minValue (Node v NilT NilT) = v
minValue (Node v left right) = min v (min (minValueOr v left) (minValueOr v right))
  where
    minValueOr defaultVal NilT = defaultVal
    minValueOr _ (Node x l r)  = minValue (Node x l r)

{-
5. A tree is reflected by swapping left and right sub-trees, recursively. Define a function to reflect an NTree.
-}

swapSubtrees :: NTree -> NTree
swapSubtrees NilT = NilT -- base case. if the tree is empty, there are no subtrees to swap, so return an empty tree.
swapSubtrees (Node n left right) = Node n (swapSubtrees right) (swapSubtrees left)

{-
6. Define a function maxTree to find the maximum value of a tree.
    maxTree :: Ord a => Tree a -> Maybe a 
-}

maxTree :: NTree -> Maybe Integer
maxTree NilT = Nothing
maxTree (Node v left right) =
        Just (foldr max v (vals left ++ vals right))
    where
        vals NilT = []
        vals (Node x l r) = x : vals l ++ vals r

{-
7. Define a function heightTree to find the height of a tree.
    heightTree :: Ord a => Tree Tree a -> Int
-}

heightTree :: Tree a -> Int
heightTree (Leaf _ _) = 0 -- leaf has height of 0
heightTree (NodeT _ left right) = 1 + max (heightTree left) (heightTree right) -- find the height of the tallest subtree and then add 1

{-
8. Define a function inorderTree to sort all the values in the tree in ascending order. 
    inorderTree :: Ord a => Tree a -> [a]
-}

inorderTree :: Ord a => Tree a -> [a]
inorderTree (Leaf x _) = [x]
inorderTree (NodeT _ left right) = inorderTree left ++ inorderTree right

{-
9. Define function to give printable versions of Huffman tables
    showTable :: Table -> String
-}

showTable :: Table -> String
showTable [] = ""
showTable ((ch, code):rest) = ch : "->" ++ showCode code ++ "\n" ++ showTable rest
    where
        -- helper function to convert huffman code (list of bits) to string
        showCode [] = ""
        showCode (L:xs) = "L" ++ showCode xs
        showCode (r:xs) = "R" ++ showCode xs

{-
10. Define functions to convert the Huffman code to binary format. For instance, ```'LRRL' -> '0110'```.
-}
codeToBinary :: HCode -> String
codeToBinary [] = ""
codeToBinary (L:rest) = "0" ++ codeToBinary rest  -- L becomes 0
codeToBinary (R:rest) = "1" ++ codeToBinary rest  -- R becomes 1

stringToBinary :: String -> String
stringToBinary str = codeToBinary (parseString str)
    where
        parseString [] = []
        parseString ('L':rest) = L : parseString rest
        parseString ('R':rest) = R : parseString rest
        parseString (_:rest) = parseString rest  -- ignore invalid characters

-- main function to test functions from questions 1-10
-- for each question, comment/uncomment accordingly
main :: IO ()
main = do
    let tree = Node 3 (Node 4 NilT NilT) NilT

    -- 1
    putStrLn ("sumTree result: " ++ show (sumTree tree))
    putStrLn ("depth result: " ++ show (depth tree))

    -- 2
    putStrLn ("left subtree: " ++ show (leftSubtree tree))
    putStrLn ("right subtree: " ++ show (rightSubtree tree))

    -- 3
    print (isElement 3 tree)
    print (isElement 4 tree)
    print (isElement 100 tree)
    
    -- 4
    putStrLn ("Maximum value in tree: " ++ show (maxValue tree))
    putStrLn ("Minimum value in tree: " ++ show (minValue tree))
    
    -- 5
    -- print original tree
    putStrLn "Original tree:"
    print tree
    -- print reflected tree
    let reflected = swapSubtrees tree
    putStrLn "\nReflected tree:"
    print reflected

    -- 6
    {-
    print tree
    putStrLn ("Maximum value: " ++ show (maxTree tree))
    
    let emptyTree = NilT
    putStrLn ("Maximum of empty tree: " ++ show (maxTree emptyTree))
    -}

    -- 7
    {-
    let tree = NodeT 5 (Leaf 'a' 1) (NodeT 3 (Leaf 'b' 1) (Leaf 'c' 1))
                
    print tree
    putStrLn ("Height of tree: " ++ show (heightTree tree))

    let leafOnly = Leaf 'x' 1
    putStrLn ("Height of a single leaf: " ++ show (heightTree leafOnly))
    -}

    -- 8
    {-
    let tree = NodeT 5 (Leaf 'a' 1) (NodeT 3 (Leaf 'c' 1) (Leaf 'b' 1))

    print tree
    putStrLn ("Inorder traversal (sorted): " ++ show (inorderTree tree)) -- expected output: acb
    -}

    -- 9
    {-
    let table =
            [ ('a', [L, R])
            , ('b', [R])
            , ('c', [L, L, R]) ]
    putStrLn "Huffman Table:"
    putStrLn (showTable table)
    -}

    -- 10
    {-
    -- test codeToBinary with an HCode value
    let code1 = [L, R, R, L]
    putStrLn ("codeToBinary [L,R,R,L] = " ++ codeToBinary code1)

    -- test stringToBinary
    let str1 = "LRRL"
    putStrLn ("stringToBinary \"LRRL\" = " ++ stringToBinary str1)

    -- another test with extra characters that should be ignored
    let str2 = "LXRBL"
    putStrLn ("stringToBinary \"LXRBL\" = " ++ stringToBinary str2)
-}