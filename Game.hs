module Game where

import Entities
import Math
import qualified Data.Map as Map
import Data.Either (rights)

-- Constante global: límites del mundo de juego
gameSize :: Size
gameSize = v2 600.0 600.0 

-- Constructor con comprobaciones para Robot 
createRobot :: Int -> String -> RobotType -> IABehavior -> Position -> Angle -> Float -> Float -> Either CreationError Robot
createRobot rid rname rtype behavior pos ang _health radar
    | radar < 0   = Left (NegativeRadar radar)
    | otherwise   = Right $ Robot
        { robotBase   = base
        , robotVerts  = createRectangleRobot rtype pos ang
        , robotHealth = maxH
        , robotMaxHealth = maxH
        , radarLength = radar
        , turretAngle = ang
        , robotMem    = Map.empty
        , robotType   = rtype
        , robotBehavior = behavior
        , robotName   = rname
        , robotCooldown = 0
        , robotHitTimer = 0
        , robotWanderTimer = 0
        , robotCollisionTimer = 0
        }
    where 
      base = GameObject rid pos (pure 0) ang (getRobotSize rtype)
      maxH = getMaxHealth rtype

-- Constructor con comprobaciones para Proyectil 
createProjectile :: GameObject -> Float -> Int -> Int -> Either CreationError Projectile
createProjectile base damage owner cooldown
    | damage <= 0 = Left (InvalidDamage damage)
    | otherwise   = Right $ Projectile base damage owner cooldown

exampleGameState :: GameState
exampleGameState =
  GameState
    { robots = rights
        [ createRobot 1 "R1"  HEAVY DEFENSIVE (v2 100 100) 0 100.0 180.0
        , createRobot 2 "R2"  MEDIUM  DEFENSIVE  (v2 360 340) (pi/4) 100.0 180.0
        , createRobot 3 "R3"   LIGHT  DEFENSIVE (v2 300 140) (pi/2) 100.0 180.0
        ]
    , projectiles = []
    , explosions  = []
    , worldSize   = gameSize
    }