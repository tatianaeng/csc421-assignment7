-- Answer the following questions based the NTree definition in Lecture slides 9 Algebraic Types and slides 9-1 Huffman Code. 

{-
1. Give line-by-line calculations of 
    
    sumTree Node 3 (Node 4 NilT NilT) NilT
    depth Node 3 (Node 4 NilT NilT) NilT
-}

{-
2. Define functions to return the left and right hand sub-trees of an NTree.
-}

{-
3. Define a function to decide whether a number is an element of an NTree.
-}

{-
4. Define functions to find the maximum and minimum values held in an NTree.
-}

{-
5. A tree is reflected by swapping left and right sub-trees, recursively. Define a function to reflect an NTree.
-}

{-
6. Define a function maxTree to find the maximum value of a tree.
    maxTree :: Ord a => Tree a -> Maybe a 
-}

{-
7. Define a function heightTree to find the height of a tree.
    heightTree :: Ord a => Tree Tree a -> Int
-}

{-
8. Define a function inorderTree to sort all the values in the tree in ascending order. 
    inorderTree :: Ord a => Tree a -> [a]
-}

{-
9. Define function to give printable versions of Huffman tables
    showTable :: Table -> String
-}

{-
10. Define functions to convert the Huffman code to binary format. For instance, ```'LRRL' -> '0110'```.
-}