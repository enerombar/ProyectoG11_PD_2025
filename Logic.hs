module Logic where

-- Importamos 'obstacles' de forma cualificada para evitar colisiones de nombres.
-- Es decir, se importa todo el módulo Entities, pero 'obstacles' se accede como Entities.obstacles.
import Entities hiding (obstacles)
import qualified Entities as E (GameState(obstacles))
import Math
import Game
import Physics (checkCollision) -- Mantener esta importación

import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.Either (partitionEithers) -- MODIFICACIÓN: Añadido para las minas


-- ########## Constantes ##########

obstacleDamageAmount :: Float
obstacleDamageAmount = 12.0 -- Daño al chocar con un obstáculo de daño

stuckVelocityThreshold :: Float
stuckVelocityThreshold = 0.1 -- Umbral de velocidad "casi cero"

obstacleCollisionCooldown :: Int
obstacleCollisionCooldown = 20 -- Inmunidad al daño de obstáculo tras un toque (frames)

projectileMaxLifetime :: Int
projectileMaxLifetime = 70 -- Tiempo de vida en frames (ej: ~1.2s a 60fps)

healthPackAmount :: Float
healthPackAmount = 35.0 -- Cantidad de curación del powerup de salud

ammoBoostAmount :: Int
ammoBoostAmount = 5 -- Cantidad de munición del powerup de munición

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
explosionDuration = 30         -- Duración de las explosiones (0.5 segundos a 60fps)

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
      -- Ralentizaciones: Tesla (leve) y Tormenta (fuerte)
      teslaMult = case Map.lookup "teslaSlow" (robotMem r) of
                    Just (MInt v) | v > 0 -> 0.85 -- Ralentización leve
                    _ -> 1.0
      stormMult = case Map.lookup "stormSlow" (robotMem r) of
                    Just (MInt v) | v > 0 -> 0.65 -- Ralentización fuerte
                    _ -> 1.0
      totalMultiplier = speedMultiplier * teslaMult * stormMult
  in case act of
    MOVE_FORWARD_ACTION ->
      let dir = dirFromAngle angle
          speed = getMaxSpeed (robotType r) * totalMultiplier -- Aplicar boost y posible slow
          vel = fmap (* speed) dir
      in  updateRobotVelocity vel r
    MOVE_BACKWARD_ACTION ->
      let dir = dirFromAngle angle
          speed = getMaxSpeed (robotType r) * totalMultiplier -- Aplicar boost y posible slow
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

-- Intenta rotar, pero comprueba colisiones con muros.
-- Si la rotación causa colisión, no la aplica.
tryRotateRobotBy :: Robot -> Angle -> GameState -> Robot
tryRotateRobotBy r d gs =
  let
    base = robotBase r
    oldPos = objPos base -- La rotación ocurre en la posición actual
    (V2 (worldW, worldH)) = gameDimensions gs
    borderThickness :: Float
    borderThickness = 10.0

    -- 1. Calcular el nuevo ángulo y los nuevos vértices
    newAng   = normalizeAngle (objAngle base + d)
    newVerts = createRectangleRobot (robotType r) oldPos newAng

    -- 2. Comprobar colisión SÓLO con muros (WALL)
    wallObstacles = filter isBlockingObstacle (E.obstacles gs)
    isBlockingObstacle o = case obsType o of
                              WALL -> True
                              TORRE_TESLA{} -> True
                              _ -> False
    isCollidingWithWall = any (fromMaybe False . checkCollision newVerts . obsVerts) wallObstacles

    -- 3. Definir y comprobar colisión con bordes
    -- Definimos los 4 rectángulos que forman los bordes
    createBorderRect bx by bw bh = [ v2 bx by, v2 (bx + bw) by, v2 (bx + bw) (by + bh), v2 bx (by + bh) ]
    topBorder    = createBorderRect (-borderThickness) (-borderThickness) (worldW + 2 * borderThickness) borderThickness
    bottomBorder = createBorderRect (-borderThickness) worldH (worldW + 2 * borderThickness) borderThickness
    leftBorder   = createBorderRect (-borderThickness) 0 borderThickness worldH
    rightBorder  = createBorderRect worldW 0 borderThickness worldH
    -- Lista de rectángulos de borde
    borderRects = [topBorder, bottomBorder, leftBorder, rightBorder]

    -- Comprobar si los nuevos vértices colisionan con algún borde
    isCollidingWithBorder = any (fromMaybe False . checkCollision newVerts) borderRects

  in
    -- 4. Comprobar ambas colisiones
    if isCollidingWithWall || isCollidingWithBorder then
      -- Colisión detectada: Devuelve el robot sin rotar
      r
    else
      -- Sin colisión: Aplica la rotación (actualiza ángulo y vértices)
      r { robotBase = base { objAngle = newAng }, robotVerts = newVerts }

-- Aplica una acción a un robot y devuelve el robot modificado.
applyAction :: Robot -> ActionType -> GameState -> Robot
applyAction r act gs = -- << AÑADIDO gs
  let result = case act of
                STOP_ACTION             -> updateVelocity r act
                MOVE_FORWARD_ACTION     -> updateVelocity r act
                MOVE_BACKWARD_ACTION    -> updateVelocity r act
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
                  -- Usa la nueva función que comprueba colisiones
                  in tryRotateRobotBy r clamped_d gs
                  
                FIRE_ACTION             -> r -- Se maneja por separado en applyFireAction
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
    (V2 (worldW, worldH)) = gameDimensions gs
    borderThickness :: Float
    borderThickness = 10.0

    -- 1. Definir bordes del mapa (sin cambios)
    createBorderRect bx by bw bh = [ v2 bx by, v2 (bx + bw) by, v2 (bx + bw) (by + bh), v2 bx (by + bh) ]
    topBorder    = createBorderRect (-borderThickness) (-borderThickness) (worldW + 2 * borderThickness) borderThickness
    bottomBorder = createBorderRect (-borderThickness) worldH (worldW + 2 * borderThickness) borderThickness
    leftBorder   = createBorderRect (-borderThickness) 0 borderThickness worldH
    rightBorder  = createBorderRect worldW 0 borderThickness worldH
    borders = [("top", topBorder), ("bottom", bottomBorder), ("left", leftBorder), ("right", rightBorder)]

    -- 2. Calcular nueva posición y rectángulo potenciales (sin cambios)
    newX_pot = x + vx * dt
    newY_pot = y + vy * dt
    potentialPos = v2 newX_pot newY_pot
    potentialAngle = objAngle base
    -- NOTA: Usamos finalVertsAfterBorders más abajo, pero 'potentialVerts' se podría usar aquí si no hubiese bordes.
    -- potentialVerts = createRectangleRobot (robotType r) potentialPos potentialAngle

    -- 3. Comprobar colisión con los bordes (sin cambios)
    borderCollisions = filter (\(_, borderRect) -> fromMaybe False (checkCollision (createRectangleRobot (robotType r) potentialPos potentialAngle) borderRect)) borders

    -- 4. Calcular la respuesta a la colisión con bordes (sin cambios)
    (finalX_afterBorders, finalVx_afterBorders) = foldl handleBorderCollisionX (newX_pot, vx) borderCollisions
    (finalY_afterBorders, finalVy_afterBorders) = foldl handleBorderCollisionY (newY_pot, vy) borderCollisions

    handleBorderCollisionX (posX, velX) (borderName, _)
        | borderName == "left" && velX < 0  = (x - velX * dt * 0.5, -velX * 0.5)
        | borderName == "right" && velX > 0 = (x - velX * dt * 0.5, -velX * 0.5)
        | otherwise                         = (posX, velX)
    handleBorderCollisionY (posY, velY) (borderName, _)
        | borderName == "top" && velY < 0    = (y - velY * dt * 0.5, -velY * 0.5)
        | borderName == "bottom" && velY > 0 = (y - velY * dt * 0.5, -velY * 0.5)
        | otherwise                          = (posY, velY)

    -- 5. Posición, velocidad y vértices finales después de considerar los bordES (sin cambios)
    finalPosAfterBorders = v2 finalX_afterBorders finalY_afterBorders
    finalVelAfterBorders = v2 finalVx_afterBorders finalVy_afterBorders
    finalVertsAfterBorders = createRectangleRobot (robotType r) finalPosAfterBorders potentialAngle -- Recalcular vértices con la pos final

    -- 6. Comprobar colisión con otros robots (sin cambios)
    otherRobots = filter (\other -> objId (robotBase other) /= objId base && isRobotAlive other) allRobots
    isCollidingWithRobot = any (fromMaybe False . checkCollision finalVertsAfterBorders . robotVerts) otherRobots

    -- 7. Comprobar colisión con obstáculos de tipo WALL (sin cambios)
    -- Usamos E.obstacles para acceder a la lista desde el GameState importado cualificadamente
    wallObstacles = filter isBlockingObstacle (E.obstacles gs)
    isBlockingObstacle o = case obsType o of
                             WALL -> True
                             TORRE_TESLA{} -> True
                             _ -> False
    isCollidingWithWall = any (fromMaybe False . checkCollision finalVertsAfterBorders . obsVerts) wallObstacles

  in
    -- *** LÓGICA DE RESOLUCIÓN DE MOVIMIENTO MODIFICADA ***

    -- Caso 1: Colisión con Robot (sin ser RAMMER) -> Parada completa (comportamiento anterior)
    if (isCollidingWithRobot && robotBehavior r /= RAMMER) then
      -- Detener el robot (aplicar fricción) y mantenerlo en su posición *anterior* (antes del movimiento de este frame)
      updateVelocity (r { robotBase = base { objPos = v2 x y } }) STOP_ACTION -- Se queda en (x,y)

    -- Caso 2: Colisión con Muro (sin ser RAMMER) -> Lógica de deslizamiento
    else if (isCollidingWithWall && robotBehavior r /= RAMMER) then
        -- Intentar mover solo en X (usando la nueva X calculada, pero la Y antigua)
        let
          posX_only = v2 finalX_afterBorders y -- (newX, oldY)
          vertsX_only = createRectangleRobot (robotType r) posX_only potentialAngle
          collidesX_only = any (fromMaybe False . checkCollision vertsX_only . obsVerts) wallObstacles
        
        -- Intentar mover solo en Y (usando la X antigua, pero la nueva Y calculada)
          posY_only = v2 x finalY_afterBorders -- (oldX, newY)
          vertsY_only = createRectangleRobot (robotType r) posY_only potentialAngle
          collidesY_only = any (fromMaybe False . checkCollision vertsY_only . obsVerts) wallObstacles

        in
          if not collidesX_only then
            -- Movimiento X es válido (desliza verticalmente)
            -- Aplicamos la posición X-only y amortiguamos la velocidad Y (fricción)
            let newBase = base { objPos = posX_only, objVel = v2 finalVx_afterBorders (vy * speedDec) }
            in r { robotBase = newBase, robotVerts = vertsX_only }
            
          else if not collidesY_only then
            -- Movimiento Y es válido (desliza horizontalmente)
            -- Aplicamos la posición Y-only y amortiguamos la velocidad X (fricción)
            let newBase = base { objPos = posY_only, objVel = v2 (vx * speedDec) finalVy_afterBorders }
            in r { robotBase = newBase, robotVerts = vertsY_only }
            
          else
            -- Atascado en una esquina (ambos movimientos colisionan), parar
            updateVelocity (r { robotBase = base { objPos = v2 x y } }) STOP_ACTION -- Se queda en (x,y)

    -- Caso 3: Sin colisión bloqueante (o es RAMMER) -> Movimiento normal
    else
      -- Sin colisión bloqueante (o es RAMMER), actualizar a la posición/velocidad calculada tras los bordes
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
            , objVel = scaleV 200 dir           -- Velocidad del proyectil
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
  gs { robots = fmap (applyActionToRobot action gs) (robots gs) }

-- Función auxiliar para aplicar una acción solo al robot que coincide.
applyActionToRobot :: Action -> GameState -> Robot -> Robot
applyActionToRobot action gs r = -- << ACEPTA gs
  if (objId . robotBase $ actionRobot action) == objId (robotBase r)
  then applyAction r (actionType action) gs -- << PASA gs
  else r

-- Aplica una lista de acciones secuencialmente al GameState.
applyActions :: [Action] -> GameState -> GameState
-- foldl espera (acumulador -> elemento -> acumulador), por lo que 'flip' es necesario
applyActions actions gs = foldl (flip applyActionToState) gs actions

-- Reduce todos los temporizadores activos (cooldowns, efectos, vida de proyectiles) en el GameState.
-- MODIFICADO: Ahora también actualiza los contadores de las minas.
decreaseCooldown :: GameState -> GameState
decreaseCooldown gs =
    let updatedRobots = map updateRobotTimers (robots gs)
        updatedExplosions = filter isExplosionAlive $ map updateExplosionTimer (explosions gs)
        updatedProjectiles = map updateProjectileLifetime (projectiles gs)
        (updatedPowerUp, nextSpawnTimer) = updatePowerUpTimers gs
        -- MODIFICACIÓN (LÍNEA NUEVA):
        updatedObstacles = map updateObstacleTimer (E.obstacles gs)
    in gs { robots = updatedRobots
          , explosions = updatedExplosions
          , projectiles = updatedProjectiles
          , powerUp = updatedPowerUp
          , powerUpSpawnTimer = nextSpawnTimer
          -- MODIFICACIÓN (LÍNEA MODIFICADA):
          , E.obstacles = updatedObstacles
          }
    where
        -- MODIFICACIÓN (FUNCIÓN HELPER NUEVA):
        -- Reduce el contador de cualquier mina activa
        updateObstacleTimer :: Obstacle -> Obstacle
        updateObstacleTimer obs = case obsType obs of
          MINA_ACTIVA c -> obs { obsType = MINA_ACTIVA (max 0 (c - 1)) }
          _             -> obs

        updateRobotTimers :: Robot -> Robot
        updateRobotTimers r =
          let newCooldown = max 0 (robotCooldown r - 1)
              newHitTimer = max 0 (robotHitTimer r - 1)
              newWanderTimer = max 0 (robotWanderTimer r - 1)
              newCollisionTimer = max 0 (robotCollisionTimer r - 1)
              newObstacleCollisionTimer = max 0 (robotObstacleCollisionTimer r - 1)
              newReloadTimer = max 0 (robotReloadTimer r - 1)
              newSpeedBoostTimer = max 0 (robotSpeedBoostTimer r - 1)
              newShieldTimer = max 0 (robotShieldTimer r - 1)
              newStuckTimer = if norm (objVel (robotBase r)) < stuckVelocityThreshold
                              then robotStuckTimer r + 1
                              else 0

              -- Decrease custom slow timers in memory
              mem0 = robotMem r
              mem1 = case Map.lookup "teslaSlow" mem0 of
                       Just (MInt v) -> if v > 0 then Map.insert "teslaSlow" (MInt (v - 1)) mem0 else Map.delete "teslaSlow" mem0
                       _             -> mem0
              mem2 = case Map.lookup "stormSlow" mem1 of
                       Just (MInt v) -> if v > 0 then Map.insert "stormSlow" (MInt (v - 1)) mem1 else Map.delete "stormSlow" mem1
                       _             -> mem1

              (finalAmmo, finalReloadTimer)
                | robotReloadTimer r == 1 && newReloadTimer == 0 = (getMagazineSize (robotType r), 0)
                | otherwise = (robotAmmo r, newReloadTimer)

          in r { robotCooldown = newCooldown
               , robotHitTimer = newHitTimer
               , robotWanderTimer = newWanderTimer
               , robotCollisionTimer = newCollisionTimer
               , robotObstacleCollisionTimer = newObstacleCollisionTimer
               , robotReloadTimer = finalReloadTimer
               , robotAmmo = finalAmmo
               , robotSpeedBoostTimer = newSpeedBoostTimer
               , robotShieldTimer = newShieldTimer
               , robotStuckTimer = newStuckTimer
               , robotMem = mem2
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

-- MODIFICACIÓN (NUEVA FUNCIÓN HELPER):
-- Helper para crear una explosión a partir de una mina
crearExplosionDeMina :: Obstacle -> Explosion
crearExplosionDeMina o =
  let (V2 (w, _)) = obsSize o
      radius = w * 1.5 -- Hacemos la explosión un 50% más grande que la mina
  in createExplosion (obsPos o) radius explosionDuration -- Usamos la constante existente

-- MODIFICACIÓN (NUEVA FUNCIÓN):
-- Gestiona las minas que han llegado a 0 en su contador
resolveExplodingMines :: GameState -> GameState
resolveExplodingMines gs =
  let
    -- 1. Clasifica los obstáculos: los que explotan (Left) y los que no (Right)
    (minasExplotando, minasRestantes) = partitionEithers $ map clasificarMina (E.obstacles gs)

    -- 2. Función de clasificación
    clasificarMina obs = case obsType obs of
      MINA_ACTIVA c | c <= 0 -> Left obs -- Explota
      _                      -> Right obs -- Se queda
          
    -- 3. Crea una explosión por cada mina que ha llegado a 0
    nuevasExplosiones = map crearExplosionDeMina minasExplotando
  in
    -- 4. Devuelve el estado:
    --    - Lista de obstáculos: solo los que no explotaron.
    --    - Lista de explosiones: las nuevas + las que ya existían.
    gs { E.obstacles = minasRestantes
       , explosions = nuevasExplosiones ++ explosions gs
       }

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

    -- 1. Comprobar expiración de tiempo de vida antes de moverlos
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
          totalW = max 1e-6 (w1 + w2) -- Evitar división por cero
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
          approxRadius rb = let V2(rw,rh) = objSize (robotBase rb) in sqrt ((rw*0.5)^2 + (rh*0.5)^2) * 0.9 -- Factor de ajuste
          overlap = max 0 ((approxRadius r1 + approxRadius r2) - dist) -- Asegurar que el solapamiento no sea negativo
          -- Cantidad de empuje proporcional al peso del oponente
          push1 = fmap (* (-overlap * (w2 / totalW))) dir
          push2 = fmap (* ( overlap * (w1 / totalW))) dir

          -- Aplicar daño primario
          r1_after_dmg = applyDamageIfMatch r1 dmgToR1 r1
          r2_after_dmg = applyDamageIfMatch r2 dmgToR2 r2

          -- 1. Calcular posiciones y vértices tentativos después del empuje
          pos1_pushed = addV (objPos (robotBase r1_after_dmg)) push1
          pos2_pushed = addV (objPos (robotBase r2_after_dmg)) push2
          -- Recalculamos vértices en la posición tentativa para la comprobación
          verts1_pushed = createRectangleRobot (robotType r1) pos1_pushed (objAngle $ robotBase r1)
          verts2_pushed = createRectangleRobot (robotType r2) pos2_pushed (objAngle $ robotBase r2)

          -- 2. Obtener los obstáculos WALL
          wallObstacles = filter isBlockingObstacle (E.obstacles gs)
          isBlockingObstacle o = case obsType o of
                             WALL -> True
                             TORRE_TESLA{} -> True
                             _ -> False

          -- 3. Comprobar si las posiciones *tentativas* colisionan con algún WALL
          collidesWithWall1 = any (fromMaybe False . checkCollision verts1_pushed . obsVerts) wallObstacles
          collidesWithWall2 = any (fromMaybe False . checkCollision verts2_pushed . obsVerts) wallObstacles

          -- 4. Decidir qué posición usar para cada robot ANTES de clampear a los bordes del mundo
          -- Si la posición empujada colisiona con un muro, usamos la posición ANTES del empuje.
          -- Si no, usamos la posición empujada.
          finalPos1 = if collidesWithWall1 then objPos (robotBase r1_after_dmg) else pos1_pushed
          finalPos2 = if collidesWithWall2 then objPos (robotBase r2_after_dmg) else pos2_pushed

          -- 5. Crear los robots con la posición decidida (antes del clamp final)
          -- Nota: Los vértices aquí NO se usan para la siguiente lógica, solo la posición.
          -- Los recalculará clampRobotPosition.
          r1_to_clamp = r1_after_dmg { robotBase = (robotBase r1_after_dmg) { objPos = finalPos1 } }
          r2_to_clamp = r2_after_dmg { robotBase = (robotBase r2_after_dmg) { objPos = finalPos2 } }

          -- 6. Aplicar clamping a los bordes del mundo a las posiciones finales decididas
          -- clampRobotPosition también recalcula los vértices finales correctamente.
          r1_clamped = clampRobotPosition gs r1_to_clamp
          r2_clamped = clampRobotPosition gs r2_to_clamp

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

-- Manejar colisión de robot con obstáculo (añadida lógica de MINA)
resolveCollision (ROBOT_OBSTACLE r o) gs =
  case obsType o of
    -- Si es un MURO, no hacemos nada aquí. La física (updatePosition) ya lo detuvo.
    WALL -> gs
    DAMAGE_ZONE_RECT -> applyDamageLogic r gs
    DAMAGE_ZONE -> applyDamageLogic r gs
    STORM_ZONE -> applyStormSlow r gs
    TORRE_TESLA{} -> gs
    MINA_INACTIVA ->
      -- La mina se activa. No hace daño aún.
      -- Cambiamos el estado de ESE obstáculo en la lista global
      let
        activarMina obs = if obsId obs == obsId o
                          then obs { obsType = MINA_ACTIVA { contador = 120 } } -- 120 frames = 2 segundos
                          else obs
      in
        gs { E.obstacles = map activarMina (E.obstacles gs) }

    MINA_ACTIVA _ -> gs -- Ya está activada, la colisión no hace nada más

  where
    --  Helper local para evitar duplicar el código de daño
    applyDamageLogic :: Robot -> GameState -> GameState
    applyDamageLogic robot gsState =
      -- Ignorar si el robot ya está en cooldown por daño de obstáculo
      if robotObstacleCollisionTimer robot > 0 then
        gsState
      else
        -- Aplicar daño y activar temporizador de inmunidad a obstáculos
        gsState { robots = map (applyObstacleDamageIfMatch robot obstacleDamageAmount) (robots gsState) }

    -- Helper local para aplicar el efecto de ralentización de la tormenta
    applyStormSlow :: Robot -> GameState -> GameState
    applyStormSlow robot gsState =
      let rid = objId (robotBase robot)
          stormFrames = 30
          setSlow r' = if objId (robotBase r') == rid
                       then r' { robotMem = Map.insert "stormSlow" (MInt stormFrames) (robotMem r') }
                       else r'
      in gsState { robots = map setSlow (robots gsState) }

resolveCollision (PROJECTILE_OBSTACLE p o) gs =
  -- CAMBIO: Ahora sabemos que 'o' puede ser WALL o DAMAGE_ZONE_RECT.
  -- Ambos son rectangulares y deben destruir el proyectil.
  let
    -- 1. Eliminar el proyectil
    -- Comparamos directamente gracias a 'deriving (Eq)' en Projectile
    updatedProjectiles = filter (\proj -> proj /= p) (projectiles gs)

    -- 2. Crear una explosión donde estaba el proyectil
    newExplosion = createExplosionFromProjectile p
    updatedExplosions = newExplosion : explosions gs
  in
    gs { projectiles = updatedProjectiles, explosions = updatedExplosions }

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

-- Aplica el efecto de un obstáculo al robot objetivo
applyObstacleDamageIfMatch :: Robot -> Float -> Robot -> Robot
applyObstacleDamageIfMatch targetRobot damage r =
  if objId (robotBase targetRobot) == objId (robotBase r) then
    if robotShieldTimer r > 0 then -- El escudo también protege de obstáculos
      r { robotHitTimer = 5 } -- Solo feedback visual
    else
      -- Aplica daño y activa los temporizadores
      -- Aseguramos que el daño de obstáculo no sea letal instantáneo
      let frameDamage = damage / 5.0 -- Dividido para que no sea tan severo
      in r { robotHealth = max 0 (robotHealth r - frameDamage)
         , robotHitTimer = damageFlashTimer -- Parpadeo
         , robotObstacleCollisionTimer = obstacleCollisionCooldown -- Activa inmunidad a obstáculos
         }
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
randomPowerUpPosition gs = generateUntilValid
  where
    (V2 (w, h)) = gameDimensions gs
    margin = 50.0 -- Margen con los bordes
    allObstacles = E.obstacles gs -- Obtener la lista de obstáculos

    -- Genera una posición y comprueba si es válida. Si no, vuelve a intentarlo.
    generateUntilValid :: IO Position
    generateUntilValid = do
      x <- randomRIO (margin, w - margin)
      y <- randomRIO (margin, h - margin)
      let potentialPos = v2 x y

      -- Crear un 'hitbox' simple para el power-up (un cuadrado basado en su radio)
      let puRadius = powerUpRadiusConst
          puHalfSize = puRadius
          puVerts = [ v2 (x - puHalfSize) (y - puHalfSize)
                    , v2 (x + puHalfSize) (y - puHalfSize)
                    , v2 (x + puHalfSize) (y + puHalfSize)
                    , v2 (x - puHalfSize) (y + puHalfSize) ]

      -- Comprobar si colisiona con algún obstáculo
      let isColliding = any (\obs -> fromMaybe False (checkCollision puVerts (obsVerts obs))) allObstacles

      if isColliding
        then generateUntilValid -- Si colisiona, intenta generar otra posición
        else return potentialPos -- Si no colisiona, esta posición es válida

-- Crea un nuevo PowerUp en una localización y tipo aleatorios
createNewPowerUp :: GameState -> IO PowerUp
createNewPowerUp gs = do
  puType <- randomPowerUpType
  pos <- randomPowerUpPosition gs -- Pasar 'gs' para que pueda ver los obstáculos
  return $ PowerUp
    { puType = puType
    , puPos = pos
    , puRadius = powerUpRadiusConst
    , puTimer = powerUpDuration
    }

-- Actualiza el estado de aparición de power-ups:
-- Genera uno si el temporizador ha llegado a cero y están habilitados.
updatePowerUpSpawning :: GameState -> GameState
updatePowerUpSpawning gs
  | not (powerUpsEnabled gs) = gs
  | powerUpSpawnTimer gs <= 0 =
      -- Usamos gs aquí al llamar a createNewPowerUp
      unsafePerformIO $ do
        newPowerUp <- createNewPowerUp gs
        return $ gs { powerUp = Just newPowerUp
                    , powerUpSpawnTimer = powerUpSpawnInterval
                    }
  | otherwise = gs

-- Restringe (clampea) la posición de un robot dentro de los límites del juego
-- y actualiza sus vértices correspondientes.
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

-- Aplica la logica de las Torres Tesla: decrementa su temporizador, elige objetivo y dispara
updateTeslaTowers :: GameState -> GameState
updateTeslaTowers gs =
  let
    -- Disminuir el temporizador de ralentizacion en memoria de cada robot (si existe)
    decSlow r = case Map.lookup "teslaSlow" (robotMem r) of
                  Just (MInt v) | v > 0 -> r { robotMem = Map.insert "teslaSlow" (MInt (v - 1)) (robotMem r) }
                  Just (MInt _)          -> r { robotMem = Map.delete "teslaSlow" (robotMem r) }
                  _ -> r
    robots0 = map decSlow (robots gs)

    -- Procesa todas las torres y acumula daños/efectos a aplicar
    (robotsFinal, obstaclesFinal) = foldl step (robots0, []) (E.obstacles gs)

    step (rs, accObs) obs = case obsType obs of
      TORRE_TESLA range cooldown timer ->
        let
          -- Decrementa el timer localmente (si > 0)
          t' = max 0 (timer - 1)
          posT = obsPos obs
          -- Robots en rango
          inRange = [ r | r <- rs, isRobotAlive r, distance (objPos $ robotBase r) posT < range ]
          -- Objetivo aprox: el más cercano al centro de la torre
          targetM = if null inRange then Nothing else Just (minimumByDist inRange posT)
          -- Si puede disparar y hay objetivo, aplicar daño y slow
          canFire = t' <= 0 && targetM /= Nothing
          rs' = case targetM of
                  Just tgt | canFire ->
                    let rs1 = map (applyDamageIfMatch tgt teslaDamagePerShot) rs
                        addSlow rbt = if objId (robotBase rbt) == objId (robotBase tgt)
                                      then rbt { robotMem = Map.insert "teslaSlow" (MInt teslaSlowFrames) (robotMem rbt)
                                               , robotHitTimer = max (robotHitTimer rbt) 5
                                               }
                                      else rbt
                    in map addSlow rs1
                  _ -> rs
          -- Resetear cooldown si disparo
          timerNext = if canFire then cooldown else t'
          obs' = obs { obsType = TORRE_TESLA range cooldown timerNext }
        in (rs', obs' : accObs)
      _ -> (rs, obs : accObs)

    minimumByDist lst p =
      let distTo r = distance (objPos $ robotBase r) p
      in foldl1 (\a b -> if distTo a <= distTo b then a else b) lst

    teslaDamagePerShot = 4.0   -- Daño leve por disparo
    teslaSlowFrames    = 30    -- 0.5s a 60fps

  in gs { robots = robotsFinal, E.obstacles = reverse obstaclesFinal }

