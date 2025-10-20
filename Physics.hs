module Physics where

import Math
import Entities
import Data.Maybe --Uso del fromMaybe (Maybe ya inlcuido en Prelude por defecto)

checkCollision :: Rectangle -> Rectangle -> Maybe Bool
checkCollision r1 r2 = do -- Depurar posible fallo en uniqueAxes
    axes1 <- uniqueAxes r1 
    axes2 <- uniqueAxes r2
    let axes = axes1 ++ axes2
    return (all axisOverlap axes)
    --Como axes1 y axes2 son independientes podriamos haber hecho fmap (all axisOverlap) $ (++) <$> uniqueAxes r1 <*> uniqueAxes r2 
    -- Pero es menos legible 
  where
    axisOverlap axis = overlap (project r1 axis) (project r2 axis)

-- Detecta colisiones robot-proyectil usando el estilo Applicative de Álvaro
detectRobotProjectileCollisions :: GameState -> [CollisionEvent]
detectRobotProjectileCollisions gs =
    let allPairs = ROBOT_PROJECTILE <$> robots gs <*> projectiles gs
    in filter isValidCollision allPairs
    where
      isValidCollision :: CollisionEvent -> Bool
      isValidCollision (ROBOT_PROJECTILE robot projectile) =
        projOwner projectile /= objId (robotBase robot) &&
        fromMaybe False (isPointInsideRectangle (objPos $ projBase projectile) (robotVerts robot))
      isValidCollision _ = False  -- Evitamos warning de "non-exhaustive patterns"

-- Detecta colisiones robot-robot
detectRobotRobotCollisions :: GameState -> [CollisionEvent]
detectRobotRobotCollisions gs =
  [ ROBOT_ROBOT r1 r2
    | (r1, r2) <- uniquePairs (robots gs)
    , fromMaybe False (checkCollision (robotVerts r1) (robotVerts r2)) --Por defecto colisión falsa si checkCollision devuelve Nothing
  ]

-- Sacar de una lista los pares de elementos diferentes únicos
uniquePairs :: [a] -> [(a, a)]
uniquePairs [] = []
uniquePairs (x:xs) = [(x, y) | y <- xs] ++ uniquePairs xs

-- Detecta colisiones robot-explosión
detectRobotExplosionCollisions :: GameState -> [CollisionEvent]
detectRobotExplosionCollisions gs =
    let allPairs = ROBOT_EXPLOSION <$> robots gs <*> explosions gs
    in filter isNearExplosion allPairs
    where
      isNearExplosion :: CollisionEvent -> Bool
      isNearExplosion (ROBOT_EXPLOSION robot explosion) =
        distance (objPos $ robotBase robot) (objPos $ explBase explosion) < explRadius explosion
      isNearExplosion _ = False  -- Evitar warning de "non-exhaustive patterns"

-- Determina todas las colisiones del juego
checkCollisions :: GameState -> [CollisionEvent]
checkCollisions gs =
  detectRobotProjectileCollisions gs ++
  detectRobotRobotCollisions gs ++
  detectRobotExplosionCollisions gs