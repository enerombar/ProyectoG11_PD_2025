module Entities where
-- Este módulo define todas las estructuras de datos y tipos principales del juego.

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Un tipo genérico para vectores 2D.
newtype V2 a = V2 { unV2 :: (a, a) }
  deriving (Show, Eq)

-- Constructor auxiliar para V2
v2 :: a -> a -> V2 a
v2 x y = V2 (x, y)

-- Instancias estándar para V2
instance Functor V2 where
  fmap f (V2 (x,y)) = V2 (f x, f y)

instance Applicative V2 where
  pure a = V2 (a,a)
  V2 (fx,fy) <*> V2 (x,y) = V2 (fx x, fy y)

-- --- Aliases de tipos para claridad ---

type Point     = V2 Float -- Un punto en el espacio 2D
type Vector    = V2 Float -- Un vector para velocidad o dirección
type Angle     = Float    -- Un ángulo en radianes
type Distance  = Float    -- Una distancia
type Size      = V2 Float -- Dimensiones (ancho, alto)
type Position  = Point    -- Posición de un objeto
type Rectangle = [Point]  -- Una lista de puntos (vértices) para definir una forma
type Life      = Int      -- Vida o duración (en frames)

-- --- Estructuras de Datos Principales ---

-- Objeto base del juego, con propiedades físicas.
data GameObject = GameObject
  { objId     :: Int      -- Identificador único
  , objPos    :: Position -- Posición actual
  , objVel    :: Vector   -- Velocidad actual
  , objAngle  :: Angle    -- Ángulo del objeto (chasis)
  , objSize   :: Size     -- Dimensiones (ancho, alto)
  } deriving (Show, Eq)

-- Configuración inicial para crear un Robot.
data TankConfig = TankConfig
  { tcType      :: RobotType  -- Tipo de chasis
  , tcBehavior  :: IABehavior -- Comportamiento de IA
  } deriving (Show, Eq)

-- Tipos de robots (chasis).
data RobotType = LIGHT | MEDIUM | HEAVY deriving (Show, Eq)

-- Comportamientos de la IA.
data IABehavior = AGGRESSIVE | BALANCED | DEFENSIVE | PEACEFUL | RAMMER | PLAYER
  deriving (Show, Eq, Ord, Read)

-- Estructura principal que representa un tanque (controlado por IA o jugador).
data Robot = Robot
  { -- Propiedades físicas y de estado base
    robotBase   :: GameObject
  , robotVerts  :: Rectangle -- Vértices para colisión y renderizado
    -- Salud
  , robotHealth :: Float
  , robotMaxHealth :: Float
    -- Combate y sensores
  , radarLength :: Float     -- Longitud del "radar" para detectar otros robots
  , turretAngle :: Angle     -- Ángulo de la torreta (independiente del chasis)
  , robotAmmo   :: Int       -- Munición actual en el cargador
    -- IA, configuración y estado
  , robotMem    :: AgentMemory -- Memoria interna de la IA
  , robotName   :: String
  , robotType   :: RobotType
  , robotBehavior :: IABehavior
    -- Temporizadores internos
  , robotWanderTimer :: Int   -- Temporizador para la IA (paseo)
  , robotCooldown :: Int      -- Temporizador para el cooldown de disparo
  , robotHitTimer :: Int      -- Temporizador para visualización de "hit"
  , robotCollisionTimer :: Int -- Temporizador para cooldown de colisión
  , robotReloadTimer :: Int  -- Temporizador para la recarga de munición
  , robotSpeedBoostTimer :: Int -- Duración restante del power-up de velocidad
  , robotShieldTimer :: Int   -- Duración restante del power-up de escudo
  } deriving (Show, Eq)

-- Representa una bala disparada.
data Projectile = Projectile
  { projBase   :: GameObject -- Propiedades físicas
  , projDamage :: Float      -- Daño que inflige
  , projOwner  :: Int        -- ID del robot que disparó
  , projLifetime :: Int      -- Tframes restantes antes de desaparecer
  } deriving (Show, Eq)

-- Representa una explosión (visual y de daño).
data Explosion = Explosion
  { explBase   :: GameObject -- Posición y tamaño
  , explRadius :: Float      -- Radio de la explosión
  , explTime   :: Int        -- Duración (frames)
  } deriving (Show, Eq)

-- Contenedor principal del estado completo del juego en un instante.
data GameState = GameState
  { -- Entidades activas
    robots      :: [Robot]
  , projectiles :: [Projectile]
  , explosions  :: [Explosion]
    -- Configuración del mundo
  , gameDimensions :: Size -- Dimensiones del campo de batalla
    -- Estado de entrada del jugador
  , playerKeys :: Set Char -- Teclas que el jugador está presionando
  , playerTargetTurretAngle :: Maybe Angle -- Ángulo al que la torreta del jugador intenta apuntar
  , playerWantsToFire :: Bool -- Si el jugador está intentando disparar
    -- Estado de los Power-ups
  , powerUpsEnabled :: Bool      -- Configuración: ¿Están habilitados los power-ups?
  , powerUp :: Maybe PowerUp -- El power-up actualmente activo en el mapa
  , powerUpSpawnTimer :: Int   -- Temporizador hasta que aparezca el próximo power-up
  } deriving (Show)


-- --- Sección de IA y Acciones ---

-- Tipos de valores que la IA puede guardar en su memoria.
data AgentMemoryValue
  = MInt Int | MString String | MPoint Point | MBool Bool
  | MFloat Float | MAction Action | MColEvent CollisionEvent
  deriving (Show, Eq)

-- Memoria de la IA (un mapa de clave-valor).
type AgentMemory = Map.Map String AgentMemoryValue

-- Tipos de acciones que un robot puede decidir ejecutar.
data ActionType
  = STOP_ACTION             -- Detenerse
  | MOVE_FORWARD_ACTION     -- Moverse hacia adelante
  | MOVE_BACKWARD_ACTION    -- Moverse hacia atrás
  | ROTATE_TURRET_ACTION Angle -- Rotar torreta a un ángulo absoluto
  | ROTATE_ROBOT_ACTION Angle  -- Rotar chasis a un ángulo absoluto
  | FIRE_ACTION             -- Disparar
  | RESET_WANDER_TIMER_ACTION -- Acción interna de la IA para resetear su temporizador de paseo
  | SET_MEMORY_ACTION String (Maybe AgentMemoryValue) -- Acción de la IA para guardar/borrar un valor en su memoria
  deriving (Eq, Show)

-- Una acción específica a ser ejecutada por un robot.
data Action = Action
  { actionRobot :: Robot
  , actionType  :: ActionType
  } deriving (Eq, Show)


-- --- Eventos y Errores ---

-- Eventos de colisión que pueden ocurrir.
data CollisionEvent
    = ROBOT_PROJECTILE Robot Projectile -- Un robot es golpeado por un proyectil
    | ROBOT_ROBOT Robot Robot       -- Dos robots colisionan
    | ROBOT_EXPLOSION Robot Explosion -- Un robot es alcanzado por una explosión
    | ROBOT_POWERUP Robot PowerUp   -- Un robot recoge un power-up
    deriving (Eq, Show)

-- Errores que pueden ocurrir al crear entidades.
data CreationError = InvalidHealth Float | InvalidDamage Float | NegativeRadar Float deriving (Show, Eq)


-- --- Power-ups ---

-- Tipos de power-ups disponibles.
data PowerUpType = Health | AmmoBoost | SpeedBoost | Shield
  deriving (Show, Eq, Enum, Bounded, Ord)

-- Representa un power-up en el mapa.
data PowerUp = PowerUp
  { puType   :: PowerUpType
  , puPos    :: Position -- Posición en el mapa
  , puRadius :: Float    -- Radio para detección de recogida
  , puTimer  :: Int      -- Cuánto tiempo permanece antes de reubicarse
  } deriving (Show, Eq)


-- --- Funciones Auxiliares ---

-- Comprueba si un robot sigue vivo.
isRobotAlive :: Robot -> Bool
isRobotAlive r = robotHealth r > 0


-- --- === Constantes de Balance del Juego === ---
-- Estas funciones definen las estadísticas de cada tipo de robot.

-- Tamaño del chasis del robot (hitbox).
getRobotSize :: RobotType -> Size
getRobotSize t = case t of
    LIGHT  -> v2 50 50
    MEDIUM -> v2 80 80
    HEAVY  -> v2 110 110

-- Tamaño de la torreta.
getTurretSize :: RobotType -> Size
getTurretSize t = case t of
    LIGHT  -> v2 25 6
    MEDIUM -> v2 36 8
    HEAVY  -> v2 45 12

-- Salud Máxima.
getMaxHealth :: RobotType -> Float
getMaxHealth t = case t of
    LIGHT  -> 80.0
    MEDIUM -> 100.0
    HEAVY  -> 120.0

-- Cooldown de Disparo (en frames).
getFireCooldown :: RobotType -> Int
getFireCooldown t = case t of
    LIGHT  -> 30 -- Más rápido
    MEDIUM -> 50
    HEAVY  -> 70 -- Más lento

-- Velocidad Máxima (unidades por segundo).
getMaxSpeed :: RobotType -> Float
getMaxSpeed t = case t of
    LIGHT  -> 65.0 -- Más rápido
    MEDIUM -> 50.0
    HEAVY  -> 35.0 -- Más lento

-- Velocidad de Rotación de Torreta (radianes por frame).
getTurretRotationSpeed :: RobotType -> Angle
getTurretRotationSpeed t = case t of
    LIGHT  -> 0.1  -- Más rápido
    MEDIUM -> 0.07
    HEAVY  -> 0.03 -- Más lento

-- Velocidad de Rotación del Chasis (radianes por frame).
getChassisRotationSpeed :: RobotType -> Angle
getChassisRotationSpeed t = case t of
    LIGHT  -> 0.07 -- Más rápido
    MEDIUM -> 0.05
    HEAVY  -> 0.02 -- Más lento

-- Peso para colisiones (afecta cómo se empujan).
getRobotWeight :: RobotType -> Float
getRobotWeight t = case t of
    LIGHT  -> 1.0
    MEDIUM -> 2.5
    HEAVY  -> 4.0

-- Daño del Proyectil.
getProjectileDamage :: RobotType -> Float
getProjectileDamage t = case t of
    LIGHT  -> 4.0  -- Menos daño
    MEDIUM -> 9.0  -- Daño estándar
    HEAVY  -> 16.0 -- Más daño

-- Radio del Proyectil (para colisiones).
getProjectileRadius :: RobotType -> Float
getProjectileRadius t = case t of
    LIGHT  -> 3.0 -- Pequeño
    MEDIUM -> 4.0 -- Mediano
    HEAVY  -> 5.0 -- Grande

-- Tamaño del Cargador (balas antes de recargar).
getMagazineSize :: RobotType -> Int
getMagazineSize t = case t of
    LIGHT  -> 10 -- Muchas balas
    MEDIUM -> 7
    HEAVY  -> 3  -- Pocas balas, pero potentes

-- Tiempo de Recarga (en frames).
getReloadTime :: RobotType -> Int
getReloadTime t = case t of
    LIGHT  -> 90  -- 1.5 segundos (a 60fps)
    MEDIUM -> 120 -- 2.0 segundos
    HEAVY  -> 180 -- 3.0 segundos

-- --- Acceso a Estado (Helpers) ---
-- Simples 'getters' para el estado del robot.

getSpeedBoostTimer :: Robot -> Int
getSpeedBoostTimer = robotSpeedBoostTimer

getShieldTimer :: Robot -> Int
getShieldTimer = robotShieldTimer