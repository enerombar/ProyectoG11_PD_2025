module Logic where

import Entities
import Math
import Game
import Physics (checkCollision) -- Mantener esta importación

import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)


-- ########## Constantes ##########

projectileMaxLifetime :: Int
projectileMaxLifetime = 70 -- Tiempo de vida en frames (ej: ~1.2s a 60fps)

healthPackAmount :: Float
healthPackAmount = 35.0 -- Cantidad de curación del paquete de salud

ammoBoostAmount :: Int
ammoBoostAmount = 5 -- Cantidad de munición del paquete de munición

speedBoostFactor :: Float
speedBoostFactor = 2.0 -- Factor multiplicador de velocidad (2.0 = 100% más rápido)

speedBoostDuration :: Int
speedBoostDuration = 420 -- Duración del boost de velocidad (7 segundos a 60fps)

shieldDuration :: Int
shieldDuration = 600 -- Duración del escudo (10 segundos a 60fps)

powerUpRadiusConst :: Float
powerUpRadiusConst = 15.0 -- Radio de los power-ups

speedDec :: Float
speedDec = 0.1 -- Factor de fricción/deceleración cuando no se acelera

explosionDamageConstant :: Float
explosionDamageConstant = 15.0 -- Daño base de las explosiones

explosionDuration :: Int
explosionDuration = 30       -- Duración de las explosiones (0.5 segundos a 60fps)

baseRobotCollisionDamage :: Float -- Daño base por colisión entre robots (sin factor de peso)
baseRobotCollisionDamage = 5.0

damageFlashTimer :: Int
damageFlashTimer = 10 -- Duración del parpadeo al recibir daño (frames)

wanderTimerResetValue :: Int
wanderTimerResetValue = 120 -- Valor de reinicio para el temporizador de 'paseo' de la IA

robotCollisionCooldown :: Int
robotCollisionCooldown = 30 -- Inmunidad al daño por colisión tras un choque (0.5s)

-- ########## Lógica Robots ##########

-- Encuentra el robot del jugador en el GameState
findPlayerRobot :: GameState -> Maybe Robot
findPlayerRobot gs = listToMaybe $ filter (\r -> robotBehavior r == PLAYER) (robots gs)

-- Aplica las acciones basadas en la entrada del jugador (teclas, ratón) almacenada en GameState
applyPlayerActionsFromState :: GameState -> GameState
applyPlayerActionsFromState gs =
  case findPlayerRobot gs of
    Nothing -> gs -- No hay jugador, no hacer nada
    Just player ->
      let
        actions = calculatePlayerActions player gs
        -- Aplica las acciones y reinicia la bandera de disparo
        gsAfterPlayerActions = applyActions actions gs
      in gsAfterPlayerActions { playerWantsToFire = False } -- Reinicia la bandera de disparo

-- Calcula la lista de acciones para el jugador basándose en el GameState
calculatePlayerActions :: Robot -> GameState -> [Action]
calculatePlayerActions player gs =
  let
    keys = playerKeys gs
    targetAngleOpt = playerTargetTurretAngle gs
    wantsToFire = playerWantsToFire gs

    -- Acciones de Movimiento
    moveActions = case (Set.member 'w' keys, Set.member 's' keys) of
      (True, False) -> [Action player MOVE_FORWARD_ACTION]
      (False, True) -> [Action player MOVE_BACKWARD_ACTION]
      _             -> [Action player STOP_ACTION] -- Parar si ambas o ninguna están pulsadas

    -- Acciones de Rotación del Chasis
    rotationActions = case (Set.member 'a' keys, Set.member 'd' keys) of
       (True, False) -> [Action player (ROTATE_ROBOT_ACTION (getChassisRotationSpeed (robotType player) * 2))] -- Multiplicar para mayor respuesta
       (False, True) -> [Action player (ROTATE_ROBOT_ACTION (-getChassisRotationSpeed (robotType player) * 2))]
       _             -> [] -- Sin rotación si ambas o ninguna

    -- Acción de apuntado de la torreta (hacia el ratón)
    turretAction = case targetAngleOpt of
       Nothing -> []
       Just targetAngle ->
         let
           currentTurretAngle = turretAngle player
           angleDelta = normalizeAngle (targetAngle - currentTurretAngle)
           -- La lógica de 'clamping' (límite de vel. de giro) se aplica en 'applyAction'
         in [Action player (ROTATE_TURRET_ACTION angleDelta)]

    -- Acción de Disparo
    fireAction = if wantsToFire then [Action player FIRE_ACTION] else []

  in moveActions ++ rotationActions ++ turretAction ++ fireAction

-- Cuenta los robots activos (vivos)
countActiveRobots :: GameState -> Int
countActiveRobots gs = length [ r | r <- robots gs, isRobotAlive r ]

-- Actualiza el vector de velocidad de un robot
updateRobotVelocity :: Vector -> Robot -> Robot
updateRobotVelocity v r = r { robotBase = (robotBase r) { objVel = v } }

-- Calcula y actualiza la velocidad del robot según una acción de movimiento
updateVelocity :: Robot -> ActionType -> Robot
updateVelocity r act =
  let boostActive = robotSpeedBoostTimer r > 0
      speedMultiplier = if boostActive then speedBoostFactor else 1.0
  in case act of
    MOVE_FORWARD_ACTION ->
      let dir = dirFromAngle angle
          speed = getMaxSpeed (robotType r) * speedMultiplier -- Aplicar boost
          vel = fmap (* speed) dir
      in  updateRobotVelocity vel r
    MOVE_BACKWARD_ACTION ->
      let dir = dirFromAngle angle
          speed = getMaxSpeed (robotType r) * speedMultiplier -- Aplicar boost
          vel = fmap (* (-speed)) dir
      in  updateRobotVelocity vel r
    STOP_ACTION ->
      let currentVel = objVel (robotBase r)
          newVel = fmap (* speedDec) currentVel -- Aplicar fricción
      in  updateRobotVelocity newVel r
    _ -> r
  where
    angle = objAngle (robotBase r)

-- Rotaciones
rotateTurretBy :: Robot -> Angle -> Robot
rotateTurretBy r d = r { turretAngle = normalizeAngle (turretAngle r + d) } -- Normalizar ángulo

rotateRobotBy :: Robot -> Angle -> Robot
rotateRobotBy r d = r { robotBase = base { objAngle = newAng }, robotVerts = newVerts }
  where
    base    = robotBase r
    newAng  = normalizeAngle (objAngle base + d) -- Normalizar ángulo
    pos     = objPos base
    newVerts = createRectangleRobot (robotType r) pos newAng

-- Aplica una acción a un robot y devuelve el robot modificado.
applyAction :: Robot -> ActionType -> Robot
applyAction r act =
  let result = case act of
                 STOP_ACTION           -> updateVelocity r act
                 MOVE_FORWARD_ACTION   -> updateVelocity r act
                 MOVE_BACKWARD_ACTION  -> updateVelocity r act
                 ROTATE_TURRET_ACTION d ->
                   let maxRot = getTurretRotationSpeed (robotType r)
                       normalized_d = normalizeAngle d
                       -- Limita la rotación a la velocidad máxima (clamping)
                       clamped_d = max (-maxRot) (min maxRot normalized_d)
                   in rotateTurretBy r clamped_d
                 ROTATE_ROBOT_ACTION d  ->
                   let maxRot = getChassisRotationSpeed (robotType r)
                       normalized_d = normalizeAngle d
                       -- Limita la rotación a la velocidad máxima (clamping)
                       clamped_d = max (-maxRot) (min maxRot normalized_d)
                   in rotateRobotBy r clamped_d
                 FIRE_ACTION            -> r -- Se maneja por separado en applyFireAction
                 RESET_WANDER_TIMER_ACTION -> r { robotWanderTimer = wanderTimerResetValue }
                 -- Acciones para la IA (memoria)
                 SET_MEMORY_ACTION key (Just val) -> r { robotMem = Map.insert key val (robotMem r) }
                 SET_MEMORY_ACTION key Nothing -> r { robotMem = Map.delete key (robotMem r) }
  in result

-- Actualiza la posición del robot, manejando límites del mundo y colisiones con otros robots.
updatePosition :: GameState -> Robot -> Float -> [Robot] -> Robot
updatePosition gs r dt allRobots =
  let
    base = robotBase r
    (x, y) = unV2 (objPos base)
    (vx, vy) = unV2 (objVel base)
    (V2 (worldW, worldH)) = gameDimensions gs -- Usar dimensiones del juego
    borderThickness :: Float
    borderThickness = 10.0 -- Grosor virtual para los rectángulos de borde

    -- 1. Definir los rectángulos de los bordes del mapa
    --    (x, y, ancho, alto) -> vértices
    createBorderRect :: Float -> Float -> Float -> Float -> Rectangle
    createBorderRect bx by bw bh =
        [ v2 bx by, v2 (bx + bw) by, v2 (bx + bw) (by + bh), v2 bx (by + bh) ]

    topBorder    = createBorderRect (-borderThickness) (-borderThickness) (worldW + 2 * borderThickness) borderThickness
    bottomBorder = createBorderRect (-borderThickness) worldH (worldW + 2 * borderThickness) borderThickness
    leftBorder   = createBorderRect (-borderThickness) 0 borderThickness worldH
    rightBorder  = createBorderRect worldW 0 borderThickness worldH

    borders = [("top", topBorder), ("bottom", bottomBorder), ("left", leftBorder), ("right", rightBorder)]

    -- 2. Calcular nueva posición y rectángulo potenciales
    newX_pot = x + vx * dt
    newY_pot = y + vy * dt
    potentialPos = v2 newX_pot newY_pot
    potentialAngle = objAngle base
    potentialVerts = createRectangleRobot (robotType r) potentialPos potentialAngle

    -- 3. Comprobar colisión con los bordes usando SAT
    borderCollisions = filter (\(_, borderRect) -> fromMaybe False (checkCollision potentialVerts borderRect)) borders

    -- 4. Calcular la respuesta a la colisión con bordes (si existe)
    (finalX, finalVx) = foldl handleBorderCollisionX (newX_pot, vx) borderCollisions
    (finalY, finalVy) = foldl handleBorderCollisionY (newY_pot, vy) borderCollisions

    -- Funciones auxiliares para la respuesta a colisión con bordes
    handleBorderCollisionX :: (Float, Float) -> (String, Rectangle) -> (Float, Float)
    handleBorderCollisionX (posX, velX) (borderName, _)
        | borderName == "left" && velX < 0  = (x - velX * dt * 0.5, -velX * 0.5) -- Rebota en borde izquierdo si iba hacia él
        | borderName == "right" && velX > 0 = (x - velX * dt * 0.5, -velX * 0.5) -- Rebota en borde derecho si iba hacia él
        | otherwise                         = (posX, velX) -- Sin colisión relevante en X

    handleBorderCollisionY :: (Float, Float) -> (String, Rectangle) -> (Float, Float)
    handleBorderCollisionY (posY, velY) (borderName, _)
        | borderName == "top" && velY < 0    = (y - velY * dt * 0.5, -velY * 0.5) -- Rebota en borde superior si iba hacia él
        | borderName == "bottom" && velY > 0 = (y - velY * dt * 0.5, -velY * 0.5) -- Rebota en borde inferior si iba hacia él
        | otherwise                          = (posY, velY) -- Sin colisión relevante en Y


    -- 5. Posición y velocidad finales después de considerar los bordes
    finalPosAfterBorders = v2 finalX finalY
    finalVelAfterBorders = v2 finalVx finalVy
    finalVertsAfterBorders = createRectangleRobot (robotType r) finalPosAfterBorders potentialAngle -- Recalcular vértices con la pos final

    -- 6. Comprobar colisión con otros robots (usando la posición final tras bordes)
    otherRobots = filter (\other -> objId (robotBase other) /= objId base && isRobotAlive other) allRobots
    isCollidingWithRobot = any (fromMaybe False . checkCollision finalVertsAfterBorders . robotVerts) otherRobots

  in
    -- 7. Decisión final: aplicar movimiento o detenerse por colisión con robot
    if isCollidingWithRobot && robotBehavior r /= RAMMER then
      -- Detener el robot (aplicar fricción) y mantenerlo en su posición *anterior* (antes del movimiento de este frame)
      updateVelocity (r { robotBase = base { objPos = v2 x y } }) STOP_ACTION -- Se queda en (x,y)
    else
      -- Sin colisión con robot (o es RAMMER), actualizar a la posición/velocidad calculada (que incluye rebotes de bordes)
      let newBase = base { objPos = finalPosAfterBorders, objVel = finalVelAfterBorders }
      in r { robotBase = newBase, robotVerts = finalVertsAfterBorders } -- Usa la posición/velocidad calculada

-- Genera un proyectil desde un robot.
spawnProjectile :: Robot -> Either CreationError Projectile
spawnProjectile r =
    -- Pasa projectileMaxLifetime como argumento de 'lifetime'
    createProjectile base damage owner projectileMaxLifetime
    where
        owner = objId $ robotBase r
        damage = getProjectileDamage (robotType r)
        robotPos = objPos $ robotBase r
        turretAng = turretAngle r
        dir = dirFromAngle turretAng
        radius = getProjectileRadius (robotType r)
        projSize = v2 radius radius
        -- Calcular la posición de aparición un poco por delante de la torreta
        turretOffset = getTurretSize (robotType r)
        spawnDist = (snd $ unV2 turretOffset) / 2 + radius + 5 -- Distancia adelante
        spawnPos = addV robotPos (scaleV spawnDist dir)

        base = GameObject
            { objId = -1 -- Los proyectiles no necesitan IDs únicas (se gestionan por instancia)
            , objPos = spawnPos
            , objVel = scaleV 200 dir               -- Velocidad del proyectil
            , objAngle = turretAng
            , objSize = projSize
            }

-- Maneja la acción FIRE_ACTION, considerando cooldown, munición y recarga.
applyFireAction :: Robot -> GameState -> GameState
applyFireAction r gs
    -- 1. No puede disparar si el arma está en cooldown
    | robotCooldown r > 0 = gs

    -- 2. No puede disparar si está recargando
    | robotReloadTimer r > 0 = gs

    -- 3. Si no está recargando Y no tiene munición -> INICIAR RECARGA
    | robotAmmo r <= 0 =
        let
          -- Iniciar el temporizador de recarga
          reloadingRobot = r { robotReloadTimer = getReloadTime (robotType r) }
        in
          -- Actualizar solo ese robot en el estado del juego
          gs { robots = map (\rb -> if objId (robotBase rb) == objId (robotBase r) then reloadingRobot else rb) (robots gs) }

    -- 4. Si pasa todas las comprobaciones (tiene munición, no recarga, no cooldown) -> ¡DISPARAR!
    | otherwise =
        case spawnProjectile r of
            Right p ->
                let
                  newAmmo = robotAmmo r - 1 -- Consumir una bala
                  -- Establecer el cooldown del arma
                  newRobot = r { robotCooldown = getFireCooldown (robotType r) -- Usar el cooldown base del robot
                               , robotAmmo = newAmmo
                               }
                in
                  gs { robots = map (\rb -> if objId (robotBase rb) == objId (robotBase r) then newRobot else rb) (robots gs)
                     , projectiles = p : projectiles gs }
            Left _ -> gs -- Error al generar el proyectil, no hacer nada


-- Aplica una única acción (modificando un robot o proyectiles) al GameState.
applyActionToState :: Action -> GameState -> GameState
applyActionToState (Action r FIRE_ACTION) gs = applyFireAction r gs -- Caso especial para disparar
applyActionToState action gs =
  gs { robots = fmap (applyActionToRobot action) (robots gs) }

-- Función auxiliar para aplicar una acción solo al robot que coincide.
applyActionToRobot :: Action -> Robot -> Robot
applyActionToRobot action r =
  if (objId . robotBase $ actionRobot action) == objId (robotBase r)
  then applyAction r (actionType action)
  else r

-- Aplica una lista de acciones secuencialmente al GameState.
applyActions :: [Action] -> GameState -> GameState
-- foldl espera (acumulador -> elemento -> acumulador), por lo que 'flip' es necesario
applyActions actions gs = foldl (flip applyActionToState) gs actions


-- Reduce todos los temporizadores activos (cooldowns, efectos, vida de proyectiles) en el GameState.
decreaseCooldown :: GameState -> GameState
decreaseCooldown gs =
    let updatedRobots = map updateRobotTimers (robots gs)
        updatedExplosions = filter isExplosionAlive $ map updateExplosionTimer (explosions gs)
        -- Actualizar tiempo de vida de proyectiles
        updatedProjectiles = map updateProjectileLifetime (projectiles gs)
        (updatedPowerUp, nextSpawnTimer) = updatePowerUpTimers gs
    in gs { robots = updatedRobots
          , explosions = updatedExplosions
          , projectiles = updatedProjectiles -- Usar proyectiles actualizados
          , powerUp = updatedPowerUp
          , powerUpSpawnTimer = nextSpawnTimer
          }
    where
        -- Función auxiliar para actualizar todos los temporizadores de un robot
        updateRobotTimers :: Robot -> Robot
        updateRobotTimers r =
          let newCooldown = max 0 (robotCooldown r - 1)
              newHitTimer = max 0 (robotHitTimer r - 1)
              newWanderTimer = max 0 (robotWanderTimer r - 1)
              newCollisionTimer = max 0 (robotCollisionTimer r - 1)
              newReloadTimer = max 0 (robotReloadTimer r - 1)
              -- Reducir temporizadores de efectos
              newSpeedBoostTimer = max 0 (robotSpeedBoostTimer r - 1)
              newShieldTimer = max 0 (robotShieldTimer r - 1)

              -- Comprobar si la recarga termina en este frame
              (finalAmmo, finalReloadTimer)
                | robotReloadTimer r == 1 && newReloadTimer == 0 = (getMagazineSize (robotType r), 0) -- Recarga completa
                | otherwise = (robotAmmo r, newReloadTimer) -- Sigue recargando o no estaba recargando

          in r { robotCooldown = newCooldown
               , robotHitTimer = newHitTimer
               , robotWanderTimer = newWanderTimer
               , robotCollisionTimer = newCollisionTimer
               , robotReloadTimer = finalReloadTimer
               , robotAmmo = finalAmmo
               -- Actualizar temporizadores de efectos
               , robotSpeedBoostTimer = newSpeedBoostTimer
               , robotShieldTimer = newShieldTimer
               }

        -- Función auxiliar para actualizar el temporizador del power-up activo o el de reaparición
        updatePowerUpTimers :: GameState -> (Maybe PowerUp, Int)
        updatePowerUpTimers state =
          let currentPU = powerUp state
              currentSpawnTimer = powerUpSpawnTimer state
          in case currentPU of
               Just pu ->
                 let newPUTimer = max 0 (puTimer pu - 1)
                 in if newPUTimer == 0
                      then (Nothing, currentSpawnTimer) -- El Power-up expira del mapa
                      else (Just (pu { puTimer = newPUTimer }), currentSpawnTimer)
               Nothing -> (Nothing, max 0 (currentSpawnTimer - 1)) -- Solo reducir el temporizador de reaparición

        -- Función auxiliar para reducir el tiempo de vida de un proyectil
        updateProjectileLifetime :: Projectile -> Projectile
        updateProjectileLifetime p = p { projLifetime = max 0 (projLifetime p - 1) }


-- ########## Lógica Proyectiles y Física ##########

-- Crea una explosión genérica
createExplosion :: Position -> Float -> Int -> Explosion
createExplosion pos radius time =
  let base = GameObject
             { objId = -1 -- No se necesita ID única para explosiones
             , objPos = pos
             , objVel = pure 0
             , objAngle = 0
             , objSize = v2 radius radius -- Usar el radio para el tamaño
             }
  in Explosion { explBase = base, explRadius = radius, explTime = time }

-- Crea una explosión a partir del estado de un proyectil (al chocar o expirar)
createExplosionFromProjectile :: Projectile -> Explosion
createExplosionFromProjectile p =
  let radius = 15.0 -- Radio de explosión estándar para proyectiles
  in createExplosion (objPos $ projBase p) radius explosionDuration -- Usar la última posición del proyectil

-- Actualiza la posición de un proyectil.
-- Devuelve (Left Explosion) si choca con un borde.
-- Devuelve (Right Projectile) si sigue en vuelo.
updateProjectilePosition :: GameState -> Projectile -> Float -> Either Explosion Projectile
updateProjectilePosition gs p dt =
    let base = projBase p
        vel = objVel base
        pos = objPos base
        newPos = addV pos (scaleV dt vel)
    in if isInBounds newPos (v2 0 0) (gameDimensions gs) then
           -- Sigue dentro de los límites, actualizar posición
           Right (p { projBase = base { objPos = newPos } })
       else
           -- Chocó con el borde, crear explosión en la *última posición válida* (pos)
           Left (createExplosionFromProjectile p)

-- Actualiza la física del juego (movimiento de robots y proyectiles)
updatePhysics :: Float -> GameState -> GameState
updatePhysics dt gs =
  let
    allRobots = robots gs
    -- Proyectiles del paso anterior (después de decreaseCooldown)
    projectilesBeforeMove = projectiles gs

    -- 1. Comprobar expiración de tiempo de vida *antes* de moverlos
    lifetimeExpired = filter (\p -> projLifetime p <= 0) projectilesBeforeMove
    projectilesToMove = filter (\p -> projLifetime p > 0) projectilesBeforeMove
    lifetimeExplosions = map createExplosionFromProjectile lifetimeExpired

    -- 2. Mover proyectiles restantes y comprobar colisiones con bordes
    moveResults = map (\p -> updateProjectilePosition gs p dt) projectilesToMove
    boundaryExplosions = [expl | Left expl <- moveResults]
    stillFlying        = [proj | Right proj <- moveResults]

    -- 3. Combinar todas las explosiones nuevas de este paso
    newExplosions = lifetimeExplosions ++ boundaryExplosions

    -- 4. Actualizar posiciones de robots (esto maneja colisiones de robots con paredes)
    updatedRobots = map (\r -> updatePosition gs r dt allRobots) allRobots

  in gs { robots = updatedRobots
        , projectiles = stillFlying -- Mantener solo los proyectiles que siguen volando
        , explosions = newExplosions ++ explosions gs -- Añadir las nuevas explosiones a las existentes
        }

-- ########## Lógica de explosiones ##########

-- Crea una explosión a partir de un robot (cuando muere)
createExplosionFromRobot :: Robot -> Explosion
createExplosionFromRobot r =
  let
    (V2 (w, h)) = objSize (robotBase r)
    radius = (max w h) * 0.75 -- Radio basado en el tamaño del robot
  in
    createExplosion (objPos $ robotBase r) radius explosionDuration

-- Reduce el temporizador de la explosión (llamado por decreaseCooldown)
updateExplosionTimer :: Explosion -> Explosion
updateExplosionTimer e = e { explTime = max 0 (explTime e - 1) }

-- Comprueba si el temporizador de una explosión ha terminado
isExplosionAlive :: Explosion -> Bool
isExplosionAlive e = explTime e > 0

-- ########## Lógica Colisiones y Powerups ##########

-- Aplica daño y activa temporizadores (parpadeo, inmunidad) por una colisión robot-robot
applyCollisionDamage :: Robot -> Float -> Robot -> Robot
applyCollisionDamage targetRobot damage r =
  if objId (robotBase targetRobot) == objId (robotBase r)
  then r { robotHealth = robotHealth r - damage
         , robotHitTimer = damageFlashTimer -- Activar parpadeo
         , robotCollisionTimer = robotCollisionCooldown -- Dar inmunidad breve
         }
  else r

-- Resuelve una lista de eventos de colisión detectados
resolveCollisions :: [CollisionEvent] -> GameState -> GameState
resolveCollisions collisions gs =
  foldl (flip resolveCollision) gs collisions

-- Resuelve un único evento de colisión según su tipo
resolveCollision :: CollisionEvent -> GameState -> GameState
resolveCollision (ROBOT_PROJECTILE r p) gs =
  gs { robots = map (applyDamageIfMatch r (projDamage p)) (robots gs)
     -- Eliminar el proyectil específico involucrado en la colisión
     , projectiles = filter (\proj -> objId (projBase proj) /= objId (projBase p) || projOwner proj /= projOwner p) (projectiles gs)
     }
resolveCollision (ROBOT_ROBOT r1 r2) gs
  -- Ignorar la colisión si alguno de los robots es inmune (por una colisión reciente)
  | robotCollisionTimer r1 > 0 || robotCollisionTimer r2 > 0 = gs
  | otherwise =
      let w1 = getRobotWeight (robotType r1)
          w2 = getRobotWeight (robotType r2)
          totalW = max 1e-6 (w1 + w2) -- Avoid division by zero if weights are somehow zero
          baseDmg = baseRobotCollisionDamage
          ramMult = 2.5 -- Multiplicador de daño para robots tipo 'RAMMER'
          isRammer1 = robotBehavior r1 == RAMMER
          isRammer2 = robotBehavior r2 == RAMMER

          -- Cálculo de daño basado en la proporción de peso y estado 'RAMMER'
          dmgToR2 = baseDmg * (w1 / totalW) * (if isRammer1 then ramMult else 1.0)
          dmgToR1 = baseDmg * (w2 / totalW) * (if isRammer2 then ramMult else 1.0)

          -- Resolución física simple de empuje (push-back)
          p1 = objPos (robotBase r1); p2 = objPos (robotBase r2)
          delta = subV p2 p1
          dist  = max 1e-3 (norm delta) -- Evitar división por cero
          dir   = fmap (/ dist) delta
          -- Radio aproximado para cálculo de solapamiento
          approxRadius rb = let V2(rw,rh) = objSize (robotBase rb) in sqrt ((rw*0.5)^2 + (rh*0.5)^2) * 0.9 -- Slightly smaller radius for stability
          overlap = max 0 ((approxRadius r1 + approxRadius r2) - dist) -- Asegurar que el solapamiento no sea negativo
          -- Cantidad de empuje proporcional al peso del oponente
          push1 = fmap (* (-overlap * (w2 / totalW))) dir
          push2 = fmap (* ( overlap * (w1 / totalW))) dir

          -- Aplicar daño PRIMERO
          r1_after_dmg = applyDamageIfMatch r1 dmgToR1 r1
          r2_after_dmg = applyDamageIfMatch r2 dmgToR2 r2

          -- Calcular posiciones tentativas después del empuje
          pos1_pushed = addV (objPos (robotBase r1_after_dmg)) push1
          pos2_pushed = addV (objPos (robotBase r2_after_dmg)) push2

          -- Crear robots con posiciones tentativas (aún no clampeadas)
          r1_tentative = r1_after_dmg { robotBase = (robotBase r1_after_dmg) { objPos = pos1_pushed } }
          r2_tentative = r2_after_dmg { robotBase = (robotBase r2_after_dmg) { objPos = pos2_pushed } }

          -- Aplicar clamping a las posiciones tentativas y actualizar vértices
          r1_clamped = clampRobotPosition gs r1_tentative
          r2_clamped = clampRobotPosition gs r2_tentative

          -- Establecer el temporizador de inmunidad DESPUÉS de clampear
          r1' = r1_clamped { robotCollisionTimer = robotCollisionCooldown }
          r2' = r2_clamped { robotCollisionTimer = robotCollisionCooldown }

          -- Función auxiliar para actualizar la lista de robots
          updateRobotList rb robotsList = map (\x -> if objId (robotBase x) == objId (robotBase rb) then rb else x) robotsList

      in gs { robots = updateRobotList r2' (updateRobotList r1' (robots gs)) }

-- Aplicar daño de explosión (la detección de radio se hace en Physics.hs)
resolveCollision (ROBOT_EXPLOSION r e) gs =
  gs { robots = map (applyDamageIfMatch r explosionDamageConstant) (robots gs) }

-- Manejar recogida de power-up
resolveCollision (ROBOT_POWERUP r pu) gs =
  -- Aplicar el efecto al robot y eliminar el power-up del mapa
  gs { robots = map (applyPowerUpIfMatch r pu) (robots gs)
     , powerUp = Nothing -- Eliminar el power-up recogido
     }

-- Aplica daño a un robot objetivo, teniendo en cuenta los escudos
applyDamageIfMatch :: Robot -> Float -> Robot -> Robot
applyDamageIfMatch targetRobot damage r =
  if objId (robotBase targetRobot) == objId (robotBase r) then
    if robotShieldTimer r > 0 then -- Comprobar si el escudo está activo
      -- Escudo activo: No aplicar daño, solo un breve parpadeo como feedback
      r { robotHitTimer = 5 }
    else
      -- Escudo inactivo: Aplicar daño y parpadeo estándar
      r { robotHealth = max 0 (robotHealth r - damage) -- Asegurar que la vida no baje de 0
        , robotHitTimer = damageFlashTimer
        }
  else
    r

-- Aplica el efecto de un power-up al robot objetivo
applyPowerUpIfMatch :: Robot -> PowerUp -> Robot -> Robot
applyPowerUpIfMatch targetRobot pu r =
  if objId (robotBase targetRobot) == objId (robotBase r) then
    case puType pu of
      Health     -> r { robotHealth = min (robotMaxHealth r) (robotHealth r + healthPackAmount) }
      AmmoBoost  -> let maxAllowed = 2 * getMagazineSize (robotType r) -- Limitar la munición máxima
                    in r { robotAmmo = min maxAllowed (robotAmmo r + ammoBoostAmount) }
      SpeedBoost -> r { robotSpeedBoostTimer = speedBoostDuration }
      Shield     -> r { robotShieldTimer = shieldDuration }
  else
    r

-- Genera un tipo de PowerUp aleatorio
randomPowerUpType :: IO PowerUpType
randomPowerUpType = do
  -- Usa las instancias de Enum y Bounded para PowerUpType
  idx <- randomRIO (fromEnum (minBound :: PowerUpType), fromEnum (maxBound :: PowerUpType))
  return $ toEnum idx

-- Genera una posición aleatoria dentro de los límites del juego (respetando un margen)
randomPowerUpPosition :: GameState -> IO Position
randomPowerUpPosition gs = do
  let (V2 (w, h)) = gameDimensions gs
      margin = 50.0 -- Mantener los power-ups alejados de los bordes
  x <- randomRIO (margin, w - margin)
  y <- randomRIO (margin, h - margin)
  return $ v2 x y

-- Crea un nuevo PowerUp en una localización y tipo aleatorios
createNewPowerUp :: GameState -> IO PowerUp
createNewPowerUp gs = do
  puType <- randomPowerUpType
  pos <- randomPowerUpPosition gs
  return $ PowerUp
    { puType = puType
    , puPos = pos
    , puRadius = powerUpRadiusConst
    , puTimer = powerUpDuration -- Cuánto tiempo permanece en el mapa
    }

-- Actualiza el estado de aparición de power-ups:
-- Genera uno si el temporizador ha llegado a cero y están habilitados.
updatePowerUpSpawning :: GameState -> GameState
updatePowerUpSpawning gs
  | not (powerUpsEnabled gs) = gs -- No hacer nada si están deshabilitados globalmente
  | powerUpSpawnTimer gs <= 0 = -- ¡Hora de aparecer/reubicar!
      unsafePerformIO $ do -- Necesario para la generación aleatoria en un contexto puro
        newPowerUp <- createNewPowerUp gs
        return $ gs { powerUp = Just newPowerUp
                    , powerUpSpawnTimer = powerUpSpawnInterval -- Reiniciar el temporizador de aparición
                    }
  | otherwise = gs -- El temporizador sigue corriendo, no hacer nada

-- Restringe (clampea) la posición de un robot dentro de los límites del juego
-- y actualiza sus vértices correspondientemente.
clampRobotPosition :: GameState -> Robot -> Robot
clampRobotPosition gs r =
  let base = robotBase r
      (V2 (x, y)) = objPos base
      (V2 (w, h)) = objSize base
      halfW = w / 2
      halfH = h / 2
      (V2 (worldW, worldH)) = gameDimensions gs
      angle = objAngle base
      rType = robotType r

      -- Restringe las coordenadas, asegurando que los bordes del robot permanezcan dentro
      finalX = max halfW (min (worldW - halfW) x)
      finalY = max halfH (min (worldH - halfH) y)

      clampedPos = v2 finalX finalY
      -- Recalcula los vértices basándose en la posición restringida
      newVerts = createRectangleRobot rType clampedPos angle
  in
    -- Devuelve el robot con la posición potencialmente ajustada y los vértices actualizados
    r { robotBase = base { objPos = clampedPos }, robotVerts = newVerts }