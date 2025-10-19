module Entities where

import qualified Data.Map as Map

-- Usamos newtype para crear un nuevo tipo (No alias) y poder crear instancias de functor, applicative...
newtype V2 a = V2 { unV2 :: (a, a) } -- Con unV2 haskell nos crea la función inversa (unpack) para sacar una tupla (a,a) de un V2
  deriving (Show, Eq)

-- Constructor de V2
v2 :: a -> a -> V2 a
v2 x y = V2 (x, y)

-- Instancia de Functor para mapear funciones sobre las dos componentes a la vez.
instance Functor V2 where
  fmap f (V2 (x,y)) = V2 (f x, f y)

-- Instancia de Applicative para operar entre dos V2 componente a componente.
instance Applicative V2 where
  pure a = V2 (a,a) --Introducir en nuestro contexto de V2
  V2 (fx,fy) <*> V2 (x,y) = V2 (fx x, fy y) 


-- Tipos actualizados para usar V2
type Point     = V2 Double
type Vector    = V2 Double
type Angle     = Double
type Distance  = Double
type Size      = V2 Double
type Position  = Point
type Rectangle = [Point]
type Life      = Int


data GameObject = GameObject
  { objId     :: Int
  , objPos    :: Position
  , objVel    :: Vector
  , objAngle  :: Angle
  , objSize   :: Size
  } deriving (Show, Eq)

data RobotType = LIGHT | MEDIUM | HEAVY deriving (Show, Eq)

data Robot = Robot
  { robotBase   :: GameObject
  , robotVerts  :: Rectangle
  , robotHealth :: Double
  , radarLength :: Double
  , turretAngle :: Angle
  , robotMem    :: AgentMemory
  , robotType   :: RobotType
  } deriving (Show, Eq)

data Projectile = Projectile
  { projBase   :: GameObject
  , projDamage :: Double
  , projOwner  :: Int
  , projCooldown :: Int
  } deriving (Show, Eq)

data Explosion = Explosion
  { explBase   :: GameObject
  , explRadius :: Double
  , explTime   :: Int
  } deriving (Show, Eq)

data GameState = GameState
  { robots      :: [Robot]
  , projectiles :: [Projectile]
  , explosions  :: [Explosion]
  , worldSize   :: Size
  } deriving (Show, Eq)

data AgentMemoryValue
  = MInt Int | MString String | MPoint Point | MBool Bool
  | MDouble Double | MAction Action | MColEvent CollisionEvent
  deriving (Show, Eq)

type AgentMemory = Map.Map String AgentMemoryValue

data Action = STOP Robot | MOVE_FORWARD Robot | MOVE_BACKWARD Robot | ROTATE_TURRET Robot Angle | ROTATE_ROBOT Robot Angle | FIRE Robot
  deriving (Eq, Show)

data CollisionEvent = ROBOT_PROJECTILE Robot Projectile | ROBOT_ROBOT Robot Robot | ROBOT_EXPLOSION Robot Explosion
  deriving (Eq, Show)

-- Tipos de errores 
data CreationError = InvalidHealth Double | InvalidDamage Double | NegativeRadar Double deriving (Show, Eq)

-- Actualizado para devolvern un V2
getRobotSize :: RobotType -> Size
getRobotSize t = case t of
    LIGHT  -> v2 40 20
    MEDIUM -> v2 60 30
    HEAVY  -> v2 80 40