-- Maciej Herdon 418267
module HashTree where
import Hashable32

data Tree a = Leaf {val :: Hash, leafVal :: a} | Twig {val :: Hash, son :: Tree a} | Node {val :: Hash, lSon, rSon :: Tree a}

instance Show a => Show (Tree a) where
  showsPrec n (Leaf a x) = (++ showHash a ++ " " ++ show x) . (++ replicate n ' ')
  showsPrec n (Twig a s) = showsPrec (n+2) s . (++ "\n") . (++ showHash a ++ " +" ) . (++ replicate n ' ')
  showsPrec n (Node a l r) = showSon r . showSon l . (++ showHash a ++ " -" ) . (++ replicate n ' ')
    where showSon t = showsPrec (n+2) t . (++ "\n")

leaf :: Hashable a => a -> Tree a
leaf x = Leaf (hash x) x

twig :: Hashable a => Tree a -> Tree a
twig t = Twig (hash (a, a)) t
  where a = val t

node :: Hashable a =>  Tree a -> Tree a -> Tree a
node x y = Node (hash (a, a')) x y
  where a = val x
        a' = val y

buildTree :: Hashable a => [a] -> Tree a
buildTree [] = error "Not supported"
buildTree xs = buildTreeFromTreeList [leaf x | x <- xs]

buildTreeFromTreeList :: Hashable a => [Tree a] -> Tree a
buildTreeFromTreeList x = if length x == 1 then head x 
                          else buildTreeFromTreeList $ helper x
  where helper (x:y:zs) = node x y : helper zs
        helper (x:zs) = twig x : helper zs
        helper [] = []

treeHash :: Tree a -> Hash
treeHash = val

drawTree :: Show a => Tree a -> String
drawTree = (++ "\n") . show

type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

instance Show a => Show (MerkleProof a) where
  showsPrec 0 (MerkleProof a p) = showString "MerkleProof (" . showString (show a) . showString ") " . showString (showMerklePath p) 
  showsPrec n (MerkleProof a p) = showString "(MerkleProof " . showString (show a) . showChar ' ' . showString (showMerklePath p) . showString ")"

showMerklePath :: MerklePath -> String
showMerklePath [] = ""
showMerklePath ((Left x):xs) = "<" ++ showHash x ++ showMerklePath xs
showMerklePath ((Right x):xs) = ">" ++ showHash x ++ showMerklePath xs

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof e t = let paths = merklePaths e t in
                  if null paths then Nothing
                  else Just $ MerkleProof e (head paths)

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths e t = createPaths e t []

createPaths :: Hashable a => a -> Tree a -> MerklePath -> [MerklePath]
createPaths e (Node a l r) p = createPaths e l ((Left . val $ r) : p) ++ createPaths e r ((Right . val $ l) : p)
createPaths e (Twig a s) p = createPaths e s ((Left . val $ s) : p)
createPaths e (Leaf a x) p = [reverse p | hash e == hash x] -- We compare hashes since we can't add (Eq a) => .. to the interface.

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof a p) = foldr helper (hash a) p == h
  where helper :: Either Hash Hash -> Hash -> Hash
        helper (Left x) a = hash (a, x)
        helper (Right x) a = hash (x, a)