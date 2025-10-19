module Game where

import Entities
import Math
import qualified Data.Map as Map
import Data.Either (rights)

-- Constante global: límites del mundo de juego
gameSize :: Size
-- CORRECCIÓN: Se reemplazó la coma por un espacio
gameSize = v2 500.0 500.0

-- Constructor con comprobaciones para Robot
createRobot :: Int -> RobotType -> Position -> Angle -> Double -> Double -> Either CreationError Robot
createRobot rid rtype pos ang health radar
    | health <= 0 = Left (InvalidHealth health)
    | radar < 0   = Left (NegativeRadar radar)
    | otherwise   = Right $ Robot
        { robotBase   = base
        , robotVerts  = createRectangleRobot rtype pos ang
        , robotHealth = health
        , radarLength = radar
        , turretAngle = ang
        , robotMem    = Map.empty
        , robotType   = rtype
        }
    where base = GameObject rid pos (pure 0) ang (getRobotSize rtype)

-- Constructor con comprobaciones para Proyectil
createProjectile :: GameObject -> Double -> Int -> Int -> Either CreationError Projectile
createProjectile base damage owner cooldown
    | damage <= 0 = Left (InvalidDamage damage)
    | otherwise   = Right $ Projectile base damage owner cooldown

-- Estado de juego de ejemplo
exampleGameState :: GameState
exampleGameState =
  GameState
    { robots = rights -- Nos quedamos solo con los robots creados correctamente (es decir que el either no sea Left)
        [ createRobot 1 MEDIUM (v2 100 100) 0 100.0 180.0
        , createRobot 2 LIGHT  (v2 360 340) (pi/4) 100.0 180.0
        ]
    , projectiles = []
    , explosions  = []
    , worldSize   = gameSize
    }