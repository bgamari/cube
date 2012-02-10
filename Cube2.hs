import Debug.Trace
import Data.Monoid
import Data.Function
import Data.Tuple
import Data.List

(<>) = mappend -- Isn't this export from Data.Monoid

data Sign = Plus | Minus deriving (Show, Eq)
                                  
data Axis2 = X2 | Y2 deriving (Show, Eq)
data Dir2 = Dir2 Sign Axis2 deriving (Eq)
                                     
-- | Some convenient names for 2D directions
north = Dir2 Plus Y2
south = Dir2 Minus Y2
east = Dir2 Plus X2
west = Dir2 Minus X2
       
instance Show Dir2 where
  show (Dir2 Plus X2) = "east"
  show (Dir2 Minus X2) = "west"
  show (Dir2 Plus Y2) = "north"
  show (Dir2 Minus Y2) = "south"


data Axis3 = X | Y | Z deriving (Show, Eq)
data Dir3 = Dir3 Sign Axis3 deriving (Show, Eq)

type Face = Dir3

-- | Some convenient names for 3D directions
px = Dir3 Plus X
nx = Dir3 Minus X
py = Dir3 Plus Y
ny = Dir3 Minus Y
pz = Dir3 Plus Z
nz = Dir3 Minus Z

-- | Inverses are nice
class Invertible a where
  invert :: a -> a
  
-- | A monoid with an inverse is a group
class (Invertible a, Monoid a) => Group a
  
-- | Signs are a group
instance Monoid Sign where
  mempty = Plus
  a `mappend` b | a /= b = Minus
  a `mappend` b          = Plus
instance Invertible Sign where
  invert = (Minus <>)
instance Group Sign
         
-- | Directions are invertible
instance Invertible Dir2 where
  invert (Dir2 s a) = Dir2 (invert s) a
instance Invertible Dir3 where
  invert (Dir3 s a) = Dir3 (invert s) a

-- * Walking on a cube
  
-- | The binary operator of a cube mapping
-- This maps from a face and a two-dimentional direction in which to walk
-- to a destination face
prod :: Face -> Dir2 -> Face
prod (Dir3 s Z) (Dir2 s' X2) = Dir3 (s <> s') X
prod (Dir3 s Z) (Dir2 s' Y2) = Dir3 s' Y
                
prod (Dir3 s Y) (Dir2 s' X2) = Dir3 s' X
prod (Dir3 s Y) (Dir2 s' Y2) = Dir3 (invert $ s <> s') Z

prod (Dir3 s X) (Dir2 s' X2) = Dir3 (invert $ s <> s') Z
prod (Dir3 s X) (Dir2 s' Y2) = Dir3 s' Y
                
-- | Project a face and a three-dimensional direction down to a
-- two-dimensional direction
project2d :: Face -> Dir3 -> Dir2
project2d (Dir3 Plus Z)  (Dir3 s X)  = Dir2 s X2
project2d (Dir3 Plus Z)  (Dir3 s Y)  = Dir2 s Y2
project2d (Dir3 Minus Z) (Dir3 s X)  = Dir2 (invert s) X2
project2d (Dir3 Minus Z) (Dir3 s Y)  = Dir2 s Y2
                                       
project2d (Dir3 Plus Y)  (Dir3 s X)  = Dir2 s X2
project2d (Dir3 Plus Y)  (Dir3 s Z)  = Dir2 (invert s) Y2
project2d (Dir3 Minus Y) (Dir3 s X)  = Dir2 s X2
project2d (Dir3 Minus Y) (Dir3 s Z)  = Dir2 s Y2
                                       
project2d (Dir3 Plus X)  (Dir3 s Y)  = Dir2 s Y2
project2d (Dir3 Plus X)  (Dir3 s Z)  = Dir2 (invert s) X2
project2d (Dir3 Minus X) (Dir3 s Y)  = Dir2 s Y2
project2d (Dir3 Minus X) (Dir3 s Z)  = Dir2 s X2
                          
project2d _ _ = error "Invalid projection"
          
--  | Walk on a cube
walk' :: Face -> Dir2 -> (Face, Dir3)
walk' face dir = (face `prod` dir, invert face)
                 
walk :: Face -> Dir2 -> (Face, Dir2)
walk face dir = let (f',d') = walk' face dir
                in (f', project2d f' d')

  
-- * Cube puzzle
  
type Prong = Int
incProng, decProng :: Prong -> Prong
incProng p = (p+1+5) `mod` 5
decProng p = (p-1+5) `mod` 5

data State = State { prong :: Prong
                   , face :: Face
                   , direction :: Dir2
                   }
           deriving (Show, Eq)
  
forbiddenTransitions :: [(Face, Dir2)]
forbiddenTransitions = [ (nx, west), (nz, east)
                       , (pz, north), (py, south)
                       , (ny, east), (px, south)
                       , (px, east), (nz, west)
                       ]
  
transitionAllowed :: Face -> Dir2 -> Bool 
transitionAllowed f d = (f,d) `notElem` forbiddenTransitions

sym :: [(a,a)] -> [(a,a)]
sym as = map swap as ++ as
rh = sym [(north,west), (south,east)] -- _|
lh = sym [(north,east), (south,west)] -- |_

-- | The allowed rotations on a given face
faceRots :: Face -> [(Dir2,Dir2)]
faceRots f | f == pz = rh
           | f == py = lh
           | f == nx = rh
           | f == ny = lh
           | f == px = rh
           | f == nz = rh
  
-- | Find all permitted transitions from a given face/orientation
transitions :: State -> [State]
transitions s@(State {prong=p, face=f, direction=d}) =
     (do ((f',d'), sign) <- map (\(dir,sign)->(walk f dir, sign))
                          $ filter (transitionAllowed f . fst)
                          $ [(d, Plus), (invert d, Minus)]
         let p' = case sign of Plus  -> incProng p
                               Minus -> decProng p
         return $ s {prong=p', face=f', direction=if sign==Minus then invert d' else d'})
  ++ (do d' <- map snd $ filter (\(a,_)->a==d) (faceRots f)
         return $ s {direction=d'})
     
search :: State -> State -> [[State]]
search dest start = search' dest start [start]

search' :: State -> State -> [State] -> [[State]]
search' dest start visited
  | dest == start  = [visited]
  | otherwise      = let trans = transitions start
                     in concat $ map (\s->search' dest s (s:visited)) $ trans \\ visited
                        
printSoln :: [State] -> IO ()
printSoln soln = do
  mapM_ print $ reverse soln
  print $ length soln
  print $ length $ map head $ groupBy ((==) `on` direction) soln
  putStrLn ""

main = do
  let steps = iterate (uncurry walk) (pz,east)
  --print $ take 8 steps
  
  let start = State {face=pz, prong=0, direction=north}
      dest  = State {face=nz, prong=4, direction=south}
  mapM_ printSoln $ sortBy (compare `on` length) $ take 1000 $ search dest start
