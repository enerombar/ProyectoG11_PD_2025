module Physics where

import Math
import Entities
import Data.Maybe (fromMaybe) -- Usado para manejar el 'Maybe' de checkCollision

-- ########## Detección de Colisiones (SAT) ##########

-- Comprueba colisión entre dos rectángulos (polígonos) usando SAT (Separating Axis Theorem)
-- Devuelve 'Maybe Bool' porque 'uniqueAxes' (de Math.hs) puede fallar (devolver Nothing)
checkCollision :: Rectangle -> Rectangle -> Maybe Bool
checkCollision r1 r2 = do
    axes1 <- uniqueAxes r1 -- Ejes de r1
    axes2 <- uniqueAxes r2 -- Ejes de r2
    let axes = axes1 ++ axes2 -- Todos los ejes a probar
    -- Hay colisión si *NO* hay un eje separador (es decir, si *TODOS* se solapan)
    return (all axisOverlap axes)
    -- (Alternativa funcional menos legible: fmap (all axisOverlap) $ (++) <$> uniqueAxes r1 <*> uniqueAxes r2)
  where
    -- Comprueba si las proyecciones de los rectángulos se solapan en un eje dado
    axisOverlap axis = overlap (project r1 axis) (project r2 axis)

-- ########## Generadores de Eventos de Colisión ##########

-- ---------- Detección de Colisiones: Robot-Proyectil ----------
detectRobotProjectileCollisions :: GameState -> [CollisionEvent]
detectRobotProjectileCollisions gs =
  [ ROBOT_PROJECTILE r p
  | r <- robots gs, isRobotAlive r -- Para cada robot vivo
  , p <- projectiles gs -- Para cada proyectil
  , projOwner p /= objId (robotBase r) -- Evitar fuego amigo (el proyectil no es del propio robot)
  -- Comprobar colisión SAT (manejando el 'Maybe Bool')
  , fromMaybe False (checkCollision (robotRectangle r) (projectileRect p))
  ]
  where
    -- Helper para crear el 'hitbox' (Rectangle) de un proyectil
    projectileRect :: Projectile -> Rectangle
    projectileRect p =
      let base = projBase p
          (V2 (w,h)) = objSize base
          halfW = max 2 (w*0.5) -- Asegurar un tamaño mínimo de hitbox
          halfH = max 2 (h*0.5)
          V2 (cx,cy) = objPos base
          -- Rectángulo base (sin rotar) centrado en la posición
          rect0 = [ v2 (cx - halfW) (cy - halfH)
                  , v2 (cx + halfW) (cy - halfH)
                  , v2 (cx + halfW) (cy + halfH)
                  , v2 (cx - halfW) (cy + halfH) ]
      in rotateRectangle rect0 (objPos base) (objAngle base)

-- ---------- Detección de Colisiones: Robot-Robot ----------
detectRobotRobotCollisions :: GameState -> [CollisionEvent]
detectRobotRobotCollisions gs =
  [ ROBOT_ROBOT r1 r2
  | (r1,r2) <- unorderedPairs (robots gs) -- Usar pares no ordenados para evitar (r1,r1) y (r2,r1)
  , isRobotAlive r1, isRobotAlive r2 -- Ambos robots deben estar vivos
  , let rect1 = robotRectangle r1
        rect2 = robotRectangle r2
  , fromMaybe False (checkCollision rect1 rect2) -- Comprobar colisión SAT
  ]

-- ---------- Detección de Colisiones: Robot-PowerUp ----------
-- (Esta detección usa una simple comprobación de radio, no SAT)
detectRobotPowerUpCollisions :: GameState -> [CollisionEvent]
detectRobotPowerUpCollisions gs =
  case powerUp gs of
    Nothing -> [] -- No hay power-up en el mapa
    Just pu ->
      [ ROBOT_POWERUP r pu
      | r <- robots gs, isRobotAlive r
      , let robotCenter = objPos (robotBase r)
            (V2 (rw, rh)) = objSize (robotBase r)
            robotApproxRadius = max rw rh * 0.5 -- Aproximación simple del radio del robot
            pickupDist = robotApproxRadius + puRadius pu -- Calcular radio de recogida (suma de radios)
      -- Comprobar si la distancia es menor que el radio de recogida
      , distance robotCenter (puPos pu) < pickupDist
      ]

-- ---------- Detección de Colisiones: Robot-Explosión ----------
-- (Esta detección usa una simple comprobación de radio)
detectRobotExplosionCollisions :: GameState -> [CollisionEvent]
detectRobotExplosionCollisions gs =
    let allPairs = [ (r, e) | r <- robots gs, isRobotAlive r, e <- explosions gs ] -- Genera pares robot-explosión
        collisions = filter isNear allPairs -- Filtra los que están colisionando
    in map toEvent collisions -- Convierte los pares filtrados en eventos
    where
      -- Comprueba si el robot está dentro del radio de la explosión
      isNear :: (Robot, Explosion) -> Bool
      isNear (robot, explosion) =
        let
          dist = distance (objPos $ robotBase robot) (objPos $ explBase explosion)
          radius = explRadius explosion
        in dist < radius

      -- Convierte (Robot, Explosion) a CollisionEvent
      toEvent :: (Robot, Explosion) -> CollisionEvent
      toEvent (robot, explosion) = ROBOT_EXPLOSION robot explosion

-- ########## Agregador de Colisiones ##########

-- Determina *todas* las colisiones que ocurren en el frame actual
checkCollisions :: GameState -> [CollisionEvent]
checkCollisions gs =
  detectRobotProjectileCollisions gs ++
  detectRobotRobotCollisions gs ++
  detectRobotExplosionCollisions gs ++
  detectRobotPowerUpCollisions gs

-- ########## Funciones Auxiliares (Helpers) ##########

-- Crea el 'hitbox' (Rectangle) de un robot a partir de sus datos
robotRectangle :: Robot -> Rectangle
robotRectangle r =
  let base = robotBase r
      pos  = objPos base
      ang  = objAngle base
      (V2 (w,h)) = objSize base
      halfW = w * 0.5
      halfH = h * 0.5
      -- Rectángulo AABB (sin rotar) centrado en la posición
      rect0 = [ v2 (cx - halfW) (cy - halfH)
              , v2 (cx + halfW) (cy - halfH)
              , v2 (cx + halfW) (cy + halfH)
              , v2 (cx - halfW) (cy + halfH) ]
        where V2 (cx,cy) = pos
  in rotateRectangle rect0 pos ang -- Rotar el rectángulo a la orientación del robot

-- Genera pares únicos (a, b) de una lista, asegurando que 'a' siempre
-- apareció antes que 'b' en la lista original.
-- Evita duplicados (b,a) y pares (a,a).
unorderedPairs :: [a] -> [(a,a)]
unorderedPairs []     = []
unorderedPairs (x:xs) = [(x,y) | y <- xs] ++ unorderedPairs xs