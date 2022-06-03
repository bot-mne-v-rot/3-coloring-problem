module Reduction (
    Var(Var),
    Literal(Lit, NLit),
    Clause,
    CNF,

    Edges,
    Graph(Graph),

    Color(R, G, B),
    Coloring,
    Solution,
    SatSolver,

    generateCNF,
    recoverAnswer,
    solve
) where

data Color = R | G | B
    deriving (Enum, Eq)

type Vertex = Int
type Coloring = [Color]
type Edges = [(Vertex, Vertex)]
data Graph = Graph { getVertices :: [Vertex], getEdges :: Edges }

data Var = Var Vertex Color
    deriving Eq

data Literal  = Lit Var | NLit Var
type Clause   = [Literal]
type CNF      = [Clause]
type Solution = [(Var, Bool)]

type SatSolver = CNF -> Maybe Solution

solve :: SatSolver -> Graph -> Maybe Coloring
solve solver graph = recoverAnswer graph $ solver $ generateCNF graph

-- Redefining them because hs-to-coq failes to translate them
infixl 3 <|>
(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x <|> _      = Just x
_      <|> Just x = Just x
_      <|> _      = Nothing 

guard :: Bool -> Maybe ()
guard True  = Just ()
guard False = Nothing 


recoverAnswer :: Graph -> Maybe Solution -> Maybe Coloring
recoverAnswer _ Nothing = Nothing
recoverAnswer graph (Just xs) =
    mapM f (getVertices graph)
    where f v = do
            let col c = do
                val <- lookup (Var v c) xs
                guard val
                return c
            col R <|> col G <|> col B

generateCNF :: Graph -> CNF
generateCNF g =
    vertexHasColor g ++
    verticesColorUniqueness g ++
    adjVerticesHaveDiffColors g

{- Clauses for condition: each vertex has at least one color -}
vertexHasColor :: Graph -> CNF
vertexHasColor g = do
    v <- getVertices g
    return [ Lit $ Var v c | c <- [R, G, B] ]

{- Clauses for condition: no more than one color is assigned to each vertex -}
verticesColorUniqueness :: Graph -> CNF
verticesColorUniqueness g = do
    v <- getVertices g
    let diffCols c1 c2 = [ NLit $ Var v c1, NLit $ Var v c2 ]
    [ diffCols R B, diffCols B G, diffCols R G ]

{- Clauses for condition: adjacent vertices have different colors -}
adjVerticesHaveDiffColors :: Graph -> CNF
adjVerticesHaveDiffColors g = do
    (u, v) <- getEdges g
    [ [ NLit $ Var u c, NLit $ Var v c ] | c <- [R, G, B] ]
