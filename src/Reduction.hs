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
    
import Control.Monad
import Control.Applicative

type Edges v = [(v, v)]
data Graph v = Graph { getVertices :: [v], getEdges :: Edges v }

data Var v = Var v Color
    deriving (Show, Eq)

data Literal v  = Lit (Var v) | NLit (Var v)
type Clause v   = [Literal v]
type CNF v      = [Clause v]
type Solution v = [(Var v, Bool)]

data Color = R | G | B
    deriving (Show, Enum, Eq)

type Coloring = [Color]

type SatSolver v = CNF v -> Maybe (Solution v)

solve :: Eq v => SatSolver v -> Graph v -> Maybe Coloring
solve solver graph = recoverAnswer graph $ solver $ generateCNF graph

recoverAnswer :: Eq v => Graph v -> Maybe (Solution v) -> Maybe Coloring
recoverAnswer _ Nothing = Nothing
recoverAnswer graph (Just xs) =
    mapM f (getVertices graph)
    where f v = do
            let col c = do
                val <- lookup (Var v c) xs
                guard val
                return c
            col R <|> col G <|> col B

generateCNF :: Graph v -> CNF v
generateCNF g =
    vertexHasColor g ++
    verticesColorUniqueness g ++
    adjVerticesHaveDiffColors g

{- Clauses for condition: each vertex has at least one color -}
vertexHasColor :: Graph v -> CNF v
vertexHasColor g = do
    v <- getVertices g
    return [ Lit $ Var v c | c <- [R, G, B] ]

{- Clauses for condition: no more than one color is assigned to each vertex -}
verticesColorUniqueness :: Graph v -> CNF v
verticesColorUniqueness g = do
    v <- getVertices g
    let diffCols c1 c2 = [ NLit $ Var v c1, NLit $ Var v c2 ]
    [ diffCols R B, diffCols B G, diffCols R G ]

{- Clauses for condition: adjacent vertices have different colors -}
adjVerticesHaveDiffColors :: Graph v -> CNF v
adjVerticesHaveDiffColors g = do
    (u, v) <- getEdges g
    [ [ NLit $ Var u c, NLit $ Var v c ] | c <- [R, G, B] ]
