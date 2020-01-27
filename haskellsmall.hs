data DFA state symbol = DFA
    { alphabet :: [symbol]
    , states   :: [state]
    , initial  :: state
    , transit  :: state -> symbol -> Maybe state
    , accept   :: state -> Bool
    }

-- A DFA for the language over {A,B,C} containing strings where A occurs
-- exactly once.

oneA :: DFA Int ABC
oneA = DFA
    { alphabet = [A,B,C]
    , states   = [0,1]
    , initial  = 0
    , transit  = delta
    , accept   = (== 1)
    }
    where
    delta 0 A = Just 1
    delta 0 _ = Just 0
    delta 1 A = Nothing
    delta 1 _ = Just 1

-- A DFA for the language over {A,B,C} containing strings where A occurs
-- exactly three times.
--
-- > accepts threeA [A,B,A,A]
-- True

threeA :: DFA Int ABC
threeA = DFA
    { alphabet = [A,B,C]
    , states = [0,1,2,3]
    , initial = 0
    , transit = delta
    , accept = (==3)
    }
    where
    delta 0 A = Just 1
    delta 0 _ = Just 0
    delta 1 A = Just 2
    delta 1 _ = Just 1
    delta 2 A = Just 3
    delta 2 _ = Just 2
    delta 3 A = Nothing
    delta 3 _ = Just 3

    data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)


-- Return a list of every path from a root to a leaf in the tree, in order
-- from left to right. Nodes with one child (one Tip and one Bin) are not
-- considered leaf nodes.
--
-- > paths Tip
-- []
-- > paths (Bin Tip 'a' Tip)
-- ["a"]
-- > paths (Bin (Bin Tip 'a' Tip) 'b' (Bin Tip 'c' (Bin Tip 'd' Tip)))
-- > ["ba", "bcd"]

paths :: Tree a -> [[a]]
paths Tip = []
paths (Bin Tip x Tip) = [[x]]
paths (Bin l x r) = map (x:) (paths l ++ paths r)


-- Return a tree with the same shape as its argument, but with each node label
-- replaced with a pair containing the original label in the second field.
-- The first field should be the height of the subtree rooted at that node.
--
-- heights Tip
-- > Tip
-- heights (Bin (Bin Tip 'a' Tip) 'b' (Bin Tip 'c' (Bin Tip 'd' Tip)))
-- > Bin (Bin Tip (1,'a') Tip) (3,'b') (Bin Tip (2,'c') (Bin Tip (1,'d') Tip))

heights :: Tree a -> Tree (Integer, a)
heights Tip = Tip
heights (Bin l n r) = Bin (heights l) (max (findHeight l) (findHeight r), n) (heights r)


findHeight :: Tree a -> Integer
findHeight Tip = 1
findHeight (Bin treeLeft x treeRight) = 1 + max (findHeight treeLeft) (findHeight treeRight)
