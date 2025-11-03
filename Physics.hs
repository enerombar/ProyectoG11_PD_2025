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
  , projOwner p /= objId (robotBase r) -- Evitar fuego amigo
  -- Comprobar colisión SAT (manejando el 'Maybe Bool')
  -- Usa el helper 'projectileRect'
  , fromMaybe False (checkCollision (robotRectangle r) (projectileRect p))
  ]

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

-- ---------- Detección de Colisiones: Robot-Obstáculo ----------
detectRobotObstacleCollisions :: GameState -> [CollisionEvent]
detectRobotObstacleCollisions gs =
  [ ROBOT_OBSTACLE r o
  | r <- robots gs, isRobotAlive r -- Para cada robot vivo
  , o <- obstacles gs              -- Para cada obstáculo
  -- Comprueba la colisión basado en el tipo de obstáculo
  , checkRobotObstacleCollision r o
  ]

-- Helper para gestionar colisión Robot-Obstáculo (Rect vs Circ)
checkRobotObstacleCollision :: Robot -> Obstacle -> Bool
checkRobotObstacleCollision r o = 
  let
    -- Lógica común para colisión circular
    checkCircularCollision = 
      let
        -- Aproximación circular para el robot
        (V2 (rw, rh)) = objSize (robotBase r)
        robotRadius = max rw rh * 0.5 -- Radio simple
        robotPos = objPos (robotBase r)
        
        -- Círculo del obstáculo
        obstaclePos = obsPos o
        (V2 (ow, _)) = obsSize o -- El radio se basa en el ancho (ya que W=H)
        obstacleRadius = ow * 0.5
        
        -- Comprobación de colisión Círculo-Círculo
        dist = distance robotPos obstaclePos
      in
        dist < (robotRadius + obstacleRadius)
  in
    case obsType o of
      -- Casos Rectangulares: Usar SAT (checkCollision)
      WALL -> fromMaybe False (checkCollision (robotVerts r) (obsVerts o))
      DAMAGE_ZONE_RECT -> fromMaybe False (checkCollision (robotVerts r) (obsVerts o))
      
      -- Casos Circulares: Usar helper 'checkCircularCollision'
      DAMAGE_ZONE -> checkCircularCollision
      STORM_ZONE  -> checkCircularCollision
      MINA_INACTIVA -> checkCircularCollision
      MINA_ACTIVA _ -> checkCircularCollision
      TORRE_TESLA{} -> checkCircularCollision

-- ---------- Detección de Colisiones: Proyectil-Obstáculo ----------
detectProjectileObstacleCollisions :: GameState -> [CollisionEvent]
detectProjectileObstacleCollisions gs =
  [ PROJECTILE_OBSTACLE p o
  | p <- projectiles gs -- Para cada proyectil
  , o <- obstacles gs   -- Para cada obstáculo
  -- Los proyectiles deben chocar con WALL y DAMAGE_ZONE_RECT (ambos rect)
  , (case obsType o of { WALL -> True; DAMAGE_ZONE_RECT -> True; TORRE_TESLA{} -> True; _ -> False })
  -- Comprobar colisión SAT (rectangular)
  , fromMaybe False (checkCollision (projectileRect p) (obsVerts o))
  ]

-- ########## Agregador de Colisiones ##########

-- Determina *todas* las colisiones que ocurren en el frame actual
checkCollisions :: GameState -> [CollisionEvent]
checkCollisions gs =
  detectRobotProjectileCollisions gs ++
  detectRobotRobotCollisions gs ++
  detectRobotExplosionCollisions gs ++
  detectRobotPowerUpCollisions gs ++
  detectRobotObstacleCollisions gs ++
  detectProjectileObstacleCollisions gs

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
