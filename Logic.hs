module Logic where

import Entities
import Math
import Game
import Physics (checkCollision)
import IA (getAIActions) 

import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Map as Map

-- Constantes 
speedDec :: Float
speedDec = 0.1

explosionDuration :: Int
explosionDuration = 30 -- Duración en frames (0.5 segundos a 60fps)

baseRobotCollisionDamage :: Float --Daño base de colisión sin contar los pesos
baseRobotCollisionDamage = 5.0 

damageFlashTimer :: Int
damageFlashTimer = 10 --Tiempo de parpadeo al recibir daño

wanderTimerResetValue :: Int
wanderTimerResetValue = 120 

robotCollisionCooldown :: Int
robotCollisionCooldown = 30 -- 0.5 segundos de inmunidad para daño por colisión

-- ######## Lógica Robots ######

countActiveRobots :: GameState -> Int
countActiveRobots gs = length [ r | r <- robots gs, isRobotAlive r ]

updateRobotVelocity :: Vector -> Robot -> Robot
updateRobotVelocity v r = r { robotBase = (robotBase r) { objVel = v } }

updateVelocity :: Robot -> ActionType -> Robot
updateVelocity r act =
  case act of
    MOVE_FORWARD_ACTION ->
      let dir = dirFromAngle angle
          speed = getMaxSpeed (robotType r)
          vel = fmap (* speed) dir
      in  updateRobotVelocity vel r
    MOVE_BACKWARD_ACTION ->
      let dir = dirFromAngle angle
          speed = getMaxSpeed (robotType r)
          vel = fmap (* (-speed)) dir
      in  updateRobotVelocity vel r
    STOP_ACTION ->
      let currentVel = objVel (robotBase r)
          newVel = fmap (* speedDec) currentVel
      in  updateRobotVelocity newVel r
    _ -> r
  where
    angle = objAngle (robotBase r)

-- Rotaciones 
rotateTurretBy :: Robot -> Angle -> Robot
rotateTurretBy r d = r { turretAngle = turretAngle r + d }

rotateRobotBy :: Robot -> Angle -> Robot
rotateRobotBy r d = r { robotBase = base { objAngle = newAng }, robotVerts = newVerts }
  where
    base    = robotBase r
    newAng  = objAngle base + d
    pos     = objPos base
    newVerts = createRectangleRobot (robotType r) pos newAng

-- Aplica una acción a un robot y devuelve el robot resultante.
applyAction :: Robot -> ActionType -> Robot
applyAction r act =
  case act of
    -- Movimiento: actualiza la velocidad según la acción (adelante/atrás/parar).
    STOP_ACTION           -> updateVelocity r act
    MOVE_FORWARD_ACTION   -> updateVelocity r act
    MOVE_BACKWARD_ACTION  -> updateVelocity r act

    -- Rotación de la torreta
    ROTATE_TURRET_ACTION d ->
      let maxRot = getTurretRotationSpeed (robotType r)
          normalized_d = normalizeAngle d -- Normaliza el ángulo recibido
          clamped_d = max (-maxRot) (min maxRot normalized_d) -- Limita al máximo permitido
      in rotateTurretBy r clamped_d

    -- Rotación del chasis/robot
    ROTATE_ROBOT_ACTION d  ->
      let maxRot = getChassisRotationSpeed (robotType r)
          normalized_d = normalizeAngle d
          clamped_d = max (-maxRot) (min maxRot normalized_d)
      in rotateRobotBy r clamped_d

    FIRE_ACTION            -> r

    RESET_WANDER_TIMER_ACTION -> r { robotWanderTimer = wanderTimerResetValue } 
    -- Una vez tomada una decisión resetea contador a 120 para no volver a tomar otra decisión hasta acabar esta
    
    -- Operaciones sobre la memoria del robot: inserta o elimina una clave.
    SET_MEMORY_ACTION key (Just val) ->
      r { robotMem = Map.insert key val (robotMem r) }
    SET_MEMORY_ACTION key Nothing ->
      r { robotMem = Map.delete key (robotMem r) }


updatePosition :: Robot -> Float -> [Robot] -> Robot
updatePosition r dt allRobots =
  let
    -- Extraemos la base del robot y sus componentes
    base = robotBase r
    (x, y) = unV2 (objPos base)
    (vx, vy) = unV2 (objVel base)
    (w, h) = unV2 (objSize base)
    (worldW, worldH) = unV2 gameSize

    -- Posiciones potenciales según la velocidad y el delta de tiempo
    newX_pot = x + vx * dt
    newY_pot = y + vy * dt

    -- Comprobación de colisiones con los límites del mundo en X:
    -- si se sale del límite, colocamos la posición en el borde y
    -- invertimos o corregimos la componente de la velocidad para
    -- simular rebote / parada contra la pared.
    (finalX, finalVx) =
      if newX_pot - w/2 < 0 then (w/2, abs vx)
      else if newX_pot + w/2 > worldW then (worldW - w/2, -abs vx)
      else (newX_pot, vx)

    -- Igual para la componente Y
    (finalY, finalVy) =
      if newY_pot - h/2 < 0 then (h/2, abs vy)
      else if newY_pot + h/2 > worldH then (worldH - h/2, -abs vy)
      else (newY_pot, vy)

    -- Creamos la posición y velocidad potenciales finales
    potentialPos = v2 finalX finalY
    potentialVel = v2 finalVx finalVy
    potentialAngle = objAngle base

    -- Calculamos los vértices del robot en la nueva posición para
    -- usarlo en la detección de colisiones con otros robots
    potentialVerts = createRectangleRobot (robotType r) potentialPos potentialAngle

    -- Filtramos el propio robot de la lista para comparar solo con otros
    otherRobots = filter (\other -> objId (robotBase other) /= objId (robotBase r)) allRobots

    -- Revisamos si la nueva forma colisiona con algún otro robot
    isColliding = any (fromMaybe False . checkCollision potentialVerts . robotVerts) otherRobots
  in
    if isColliding then
      -- Si hay colisión con otro robot, no movemos al robot.
      -- En su lugar aplicamos una desaceleración suave a la velocidad actual.
      let currentVel = objVel base
          newVel = fmap (* speedDec) currentVel
      in r { robotBase = (robotBase r) { objVel = newVel } }
    else
      -- Si no hay colisión, aplicamos la nueva posición, velocidad y vértices.
      let newBase = base { objPos = potentialPos, objVel = potentialVel }
      in r { robotBase = newBase, robotVerts = potentialVerts }


-- ########## Lógica cooldown y disparo ########

spawnProjectile :: Robot -> Either CreationError Projectile
spawnProjectile r =
    createProjectile base damage owner cooldown
    where
        owner = objId $ robotBase r
        damage = getProjectileDamage (robotType r)
        cooldown = getFireCooldown (robotType r)
        robotPos = objPos $ robotBase r
        turretAng = turretAngle r
        dir = dirFromAngle turretAng

        radius = getProjectileRadius (robotType r)
        projSize = v2 radius radius
        
        base = GameObject
            { objId = -1 --Por ahora no id's únicos. Ya tienen el de owner
            , objPos = addV robotPos (scaleV 50 dir)
            , objVel = scaleV 200 dir
            , objAngle = turretAng
            , objSize = projSize
            }

applyFireAction :: Robot -> GameState -> GameState
applyFireAction r gs
    | robotCooldown r <= 0 =
        case spawnProjectile r of
            Right p ->
                let newRobot = r { robotCooldown = projCooldown p }
                in gs { robots = map (\rb -> if objId (robotBase rb) == objId (robotBase r) then newRobot else rb) (robots gs)
                      , projectiles = p : projectiles gs } --Cambiamos solo el cooldown del que dispara y añadimos el proyectil 
            Left _ -> gs
    | otherwise = gs

applyActionToState :: Action -> GameState -> GameState
applyActionToState (Action r FIRE_ACTION) gs = applyFireAction r gs
applyActionToState action gs =
  gs { robots = fmap (applyActionToRobot action) (robots gs) }

applyActionToRobot :: Action -> Robot -> Robot
applyActionToRobot action r =
  if (objId . robotBase $ actionRobot action) == objId (robotBase r)
  then applyAction r (actionType action)
  else r

applyActions :: [Action] -> GameState -> GameState
applyActions actions gs = foldl (flip applyActionToState) gs actions
--foldl espera una función acumulador -> elemento_lista -> acumulador 
-- por lo que es necesario flip para invertir los argumentos de applyActionToState

decreaseCooldown :: GameState -> GameState
decreaseCooldown gs = gs { 
              robots = map updateRobotTimers (robots gs),
              explosions = filter isExplosionAlive $ map updateExplosionTimer (explosions gs)
              }
    where
        updateRobotTimers r = r
            { robotCooldown = max 0 (robotCooldown r - 1) --Cooldown de disparar
            , robotHitTimer = max 0 (robotHitTimer r - 1) --Cooldown de recibir disparo
            , robotWanderTimer = max 0 (robotWanderTimer r - 1)  --Cooldown de estado wander
            , robotCollisionTimer = max 0 (robotCollisionTimer r - 1) --Cooldown de haber colisionado con otro robot
            }

updateProjectilePosition :: Projectile -> Float -> Maybe Projectile
updateProjectilePosition p dt
    | isInBounds newPos (v2 0 0) gameSize = -- (v2 0 0) es la esquina inferior izquierda que consideramos origen de coord (diferente que Gloss)
        Just p { projBase = (projBase p) { objPos = newPos } }
    | otherwise = Nothing 
  where
    base = projBase p
    vel = objVel base
    pos = objPos base
    newPos = addV pos (scaleV dt vel) -- fórmula de física newPos = oldPos + (velocity * time)

updatePhysics :: Float -> GameState -> GameState
updatePhysics dt gs =
  let allRobots = robots gs
  in gs {
    robots = map (\r -> updatePosition r dt allRobots) allRobots
    ,
    projectiles = mapMaybe (`updateProjectilePosition` dt) (projectiles gs) --mapMaybe :: (a -> Maybe b) -> [a] -> [b]
  }

-- Esta función aplica daño por colisión y activa el temporizador 
applyCollisionDamage :: Robot -> Float -> Robot -> Robot
applyCollisionDamage targetRobot damage r =
  if objId (robotBase targetRobot) == objId (robotBase r)
  then r { robotHealth = robotHealth r - damage
         , robotHitTimer = damageFlashTimer -- Activa el parpadeo
         , robotCollisionTimer = robotCollisionCooldown
         }
  else r

resolveCollisions :: [CollisionEvent] -> GameState -> GameState
resolveCollisions collisions gs =
  foldl (flip resolveCollision) gs collisions
--foldl espera una función acumulador -> elemento_lista -> acumulador 
-- por lo que es necesario flip para invertir los argumentos de resolveCollision

resolveCollision :: CollisionEvent -> GameState -> GameState
resolveCollision (ROBOT_PROJECTILE r p) gs =
  gs {
    robots = map (applyDamageIfMatch r (projDamage p)) (robots gs) 
  , projectiles = filter (/= p) (projectiles gs) --Quitamos el proyectil que colisionó
  }
resolveCollision (ROBOT_ROBOT r1 r2) gs =
  -- Solo aplica daño si AMBOS robots no están en cooldown de colisión
  if robotCollisionTimer r1 == 0 && robotCollisionTimer r2 == 0 then
    let
      w1 = getRobotWeight (robotType r1)
      w2 = getRobotWeight (robotType r2)
      totalWeight = w1 + w2 
      damageTo_r1 = baseRobotCollisionDamage * (w2 / totalWeight) --Daño a cada uno en función de la diferencia de peso 
      damageTo_r2 = baseRobotCollisionDamage * (w1 / totalWeight)
    in
    gs {
      robots = map (applyCollisionDamage r1 damageTo_r1 . applyCollisionDamage r2 damageTo_r2) (robots gs)
    }
  else
    gs -- No hay daño si están en cooldown

resolveCollision (ROBOT_EXPLOSION _ _) gs = gs


-- ########## Lógica de explosiones ##########

createExplosionFromRobot :: Robot -> Explosion
createExplosionFromRobot r =
  let
    (V2 (w, h)) = objSize (robotBase r)
    radius = (max w h) * 0.75 -- Un radio basado en el tamaño del robot
    -- La base de la explosión está en la pos del robot, sin velocidad
    base = (robotBase r) { objVel = pure 0, objSize = v2 radius radius }
  in
    Explosion
      { explBase = base
      , explRadius = radius
      , explTime = explosionDuration
      }

updateExplosionTimer :: Explosion -> Explosion
updateExplosionTimer e = e { explTime = max 0 (explTime e - 1) }

isExplosionAlive :: Explosion -> Bool
isExplosionAlive e = explTime e > 0

-- ########## LÓGICA DE BOTS ##########

exampleBotActions :: GameState -> [Action]
exampleBotActions = getAIActions

normalizeAngle :: Angle -> Angle
normalizeAngle ang = atan2 (sin ang) (cos ang)

applyDamageIfMatch :: Robot -> Float -> Robot -> Robot
applyDamageIfMatch targetRobot damage r = --Aplica el daño al robot de la lista que coincida en id con el golpeado  
  if objId (robotBase targetRobot) == objId (robotBase r)
  then r { robotHealth = robotHealth r - damage
         , robotHitTimer = damageFlashTimer --Activa el parpadeo de recibir daño
         }
  else r