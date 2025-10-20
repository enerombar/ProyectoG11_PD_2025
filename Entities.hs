module Entities where

import qualified Data.Map as Map

newtype V2 a = V2 { unV2 :: (a, a) }
  deriving (Show, Eq)

v2 :: a -> a -> V2 a
v2 x y = V2 (x, y)

instance Functor V2 where
  fmap f (V2 (x,y)) = V2 (f x, f y)

instance Applicative V2 where
  pure a = V2 (a,a)
  V2 (fx,fy) <*> V2 (x,y) = V2 (fx x, fy y)

type Point     = V2 Float
type Vector    = V2 Float
type Angle     = Float 
type Distance  = Float 
type Size      = V2 Float
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

data IABehavior = AGGRESSIVE | BALANCED | DEFENSIVE | PEACEFUL
  deriving (Show, Eq)

data Robot = Robot
  { robotBase   :: GameObject
  , robotVerts  :: Rectangle
  , robotHealth :: Float
  , robotMaxHealth :: Float
  , radarLength :: Float 
  , turretAngle :: Angle
  , robotMem    :: AgentMemory
  , robotName   :: String
  , robotType   :: RobotType
  , robotBehavior :: IABehavior
  , robotWanderTimer :: Int
  , robotCooldown :: Int 
  , robotHitTimer :: Int 
  , robotCollisionTimer :: Int
  } deriving (Show, Eq)

data Projectile = Projectile
  { projBase   :: GameObject
  , projDamage :: Float
  , projOwner  :: Int
  , projCooldown :: Int
  } deriving (Show, Eq)

data Explosion = Explosion
  { explBase   :: GameObject
  , explRadius :: Float 
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
  | MFloat Float | MAction Action | MColEvent CollisionEvent
  deriving (Show, Eq)

type AgentMemory = Map.Map String AgentMemoryValue

data ActionType
  = STOP_ACTION
  | MOVE_FORWARD_ACTION
  | MOVE_BACKWARD_ACTION
  | ROTATE_TURRET_ACTION Angle
  | ROTATE_ROBOT_ACTION Angle
  | FIRE_ACTION
  | RESET_WANDER_TIMER_ACTION -- Acción interna de la IA
  | SET_MEMORY_ACTION String (Maybe AgentMemoryValue) -- Para guardar estado en la IA
  deriving (Eq, Show)

data Action = Action
  { actionRobot :: Robot
  , actionType  :: ActionType
  } deriving (Eq, Show)

data CollisionEvent = ROBOT_PROJECTILE Robot Projectile | ROBOT_ROBOT Robot Robot | ROBOT_EXPLOSION Robot Explosion
  deriving (Eq, Show)

data CreationError = InvalidHealth Float | InvalidDamage Float | NegativeRadar Float deriving (Show, Eq)


isRobotAlive :: Robot -> Bool
isRobotAlive r = robotHealth r > 0

getRobotSize :: RobotType -> Size
getRobotSize t = case t of
    LIGHT  -> v2 40 20
    MEDIUM -> v2 60 30
    HEAVY  -> v2 80 40

getTurretSize :: RobotType -> Size
getTurretSize t = case t of
    LIGHT  -> v2 30 7
    MEDIUM -> v2 40 10
    HEAVY  -> v2 50 14

-- Salud Máxima
getMaxHealth :: RobotType -> Float
getMaxHealth t = case t of
    LIGHT  -> 80.0
    MEDIUM -> 100.0
    HEAVY  -> 120.0

-- Cooldown de Disparo (en frames)
getFireCooldown :: RobotType -> Int
getFireCooldown t = case t of
    LIGHT  -> 30 -- Más rápido
    MEDIUM -> 50
    HEAVY  -> 70 -- Más lento

-- Velocidad Máxima (unidades por segundo)
getMaxSpeed :: RobotType -> Float
getMaxSpeed t = case t of
    LIGHT  -> 65.0 -- Más rápido
    MEDIUM -> 50.0
    HEAVY  -> 35.0 -- Más lento

-- Velocidad de Rotación de Torreta (radianes por frame)
getTurretRotationSpeed :: RobotType -> Angle
getTurretRotationSpeed t = case t of
    LIGHT  -> 0.1
    MEDIUM -> 0.07
    HEAVY  -> 0.03 -- Más lento

getChassisRotationSpeed :: RobotType -> Angle
getChassisRotationSpeed t = case t of
    LIGHT  -> 0.07 -- Más rápido
    MEDIUM -> 0.05
    HEAVY  -> 0.02 -- Más lento

-- Peso para colisiones
getRobotWeight :: RobotType -> Float
getRobotWeight t = case t of
    LIGHT  -> 1.0
    MEDIUM -> 2.5
    HEAVY  -> 4.0

-- Daño del Proyectil
getProjectileDamage :: RobotType -> Float
getProjectileDamage t = case t of
    LIGHT  -> 4.0  -- Menos daño
    MEDIUM -> 9.0 -- Daño estándar
    HEAVY  -> 16.0 -- Más daño

-- Radio del Proyectil
getProjectileRadius :: RobotType -> Float
getProjectileRadius t = case t of
    LIGHT  -> 3.0 -- Pequeño
    MEDIUM -> 4.0 -- Mediano
    HEAVY  -> 5.0 -- Grande