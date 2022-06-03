module Reduction (
    Literal,
    Clause,
    CNF,

    Vertex,
    Vertexes,
    Edges,
    Graph,

    Color,
    Coloring,
    Solution,
    Solver,

    generateCNF,
    recoverAnswer,
    solve
) where

type Literal = Int
type Clause  = [Literal]
type CNF     = [Clause]

type Vertex = Int
type Vertexes = [Vertex]
type Edges  = [(Vertex, Vertex)]
data Graph = RecordType { size :: Int, vertexes :: Vertexes, edges :: Edges }

data Color = R | G | B
    deriving (Show, Enum)

type Coloring = [Color]

newtype Solution = Solution [Int]
    deriving (Show, Eq, Ord)

type Solver = CNF -> Maybe Solution

solve :: Solver -> Graph -> Coloring
solve solver graph = recoverAnswer (size graph) $ solver $ generateCNF' graph

recoverAnswer :: Int -> Maybe Solution -> Coloring
recoverAnswer _ Nothing = []
recoverAnswer _ (Just (Solution [])) = []
recoverAnswer n (Just (Solution xs)) = helper n xs []
    where helper n [] res = res
          helper n (x:xs) res | x < 0 = helper n xs res
                              | otherwise = helper n xs (colorByLiteral n x : res)

colorByLiteral :: Int -> Int -> Color
colorByLiteral n lit = toEnum $ snd $ fromSingleToPair n lit

fromSingleToPair :: Int -> Int -> (Int, Int)
fromSingleToPair n idx = ((idx - 1) `div` n, (idx - 1) `mod` n)

generateCNF' :: Graph -> CNF
generateCNF' graph = let sz = size graph in vertexHasColor sz ++ vertexesColorBound sz ++ edgesHasDiffColors sz (edges graph)

generateCNF :: Int -> Edges -> CNF
generateCNF n edges = vertexHasColor n ++ vertexesColorBound n ++ edgesHasDiffColors n edges

{- Clauses for condition: each vertex has at least one color -}
vertexHasColor :: Int -> CNF
vertexHasColor n = do
    vertex <- [0..(n - 1)]
    let clause = [ fromPairToSingle n (vertex, color) | color <- [0..2] ]
    return clause

{- Clauses for condition: each vertex has not more than one color -}
vertexesColorBound :: Int -> CNF
vertexesColorBound n = helper n (n - 1) []
    where helper n i list | i >= 0    = helper n (i - 1) (vertexBoundColor n i ++ list)
                          | otherwise = list

vertexBoundColor :: Int -> Vertex -> CNF
vertexBoundColor n i = do
    f <- getPossibleColors i
    s <- getPossibleColors i
    let flit = fromPairToSingle n f
    let slit = fromPairToSingle n s
    if flit < slit && (flit /= slit)
        then return [-flit, -slit] -- by De Morgans Law
        else []

getPossibleColors :: Vertex -> [(Int, Int)]
getPossibleColors i = [ (i, j) | j <- [0..2] ]

{- Clauses for condition: each edge's vertexes has different colors -}
edgesHasDiffColors :: Int -> Edges -> CNF
edgesHasDiffColors n edges = do
    e <- getPossibleEdges n
    f <- getPossibleColors (fst e)
    s <- getPossibleColors (snd e)
    if snd f == snd s
        then do
            let flit = fromPairToSingle n f
            let slit = fromPairToSingle n s
            if flit /= slit && (e `elem` edges)
                then return [-flit, -slit] -- by De Morgans Law
                else []
        else []

getPossibleEdges :: Int -> Edges
getPossibleEdges n = do
                    i <- [0..(n - 1)]
                    j <- [0..(n - 1)]
                    if i /= j then return (i, j)
                    else []

fromPairToSingle :: Int -> (Int, Int) -> Int
fromPairToSingle n (i, j) = n * i + j + 1

