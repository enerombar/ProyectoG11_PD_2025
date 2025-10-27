module IA where

import Entities
import Math
import Data.Ord (comparing)
import Data.List (minimumBy, sortOn) -- Importado para ordenar
import Data.Maybe (fromMaybe, listToMaybe) -- Importado para manejar Maybe
import qualified Data.Map as Map

-- --- Constantes de Comportamiento de IA ---

-- Tolerancia para el disparo. No necesita apuntar perfectamente.
fireTolerance :: Angle
fireTolerance = 0.06 -- (Unos 3.4 grados)

-- Ángulo de giro para la IA de 'wander' (deambular).
smallWanderTurn :: Angle
smallWanderTurn = pi / 6 -- (30 grados)

-- Margen con la pared para activar la evasión.
wallMargin :: Float
wallMargin = 15.0

-- Ángulo de giro al detectar una pared.
wallTurn :: Angle
wallTurn = pi / 2 -- (90 grados)

-- Tolerancia para el giro de 'wander'. Evita bucles si no llega al ángulo exacto.
wanderTurnTolerance :: Angle
wanderTurnTolerance = 0.1

-- Factor de distancia para decidir si ir a por un power-up.
-- Si mi_distancia <= (distancia_enemigo_mas_cercano * factor), iré.
powerUpDistanceFactor :: Float
powerUpDistanceFactor = 1.2 -- Ir si eres hasta un 20% más lejano que el enemigo más cercano.

-- *** NUEVA CONSTANTE ***
-- Distancia mínima para considerar que se ha "investigado" la última posición vista.
investigationClearDistance :: Float
investigationClearDistance = 25.0

-- --- Funciones de Detección y Percepción ---

-- Comprueba si r2 está dentro del rango de radar de r1.
detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 = distance (objPos $ robotBase r1) (objPos $ robotBase r2) <= radarLength r1

-- Comprueba si el robot está demasiado cerca de los límites del mundo.
isNearWall :: GameState -> Robot -> Bool
isNearWall gs r =
  let
    (x, y) = unV2 (objPos $ robotBase r)
    (w, h) = unV2 (objSize $ robotBase r)
    (V2 (worldW, worldH)) = gameDimensions gs -- Dimensiones del mundo
  in
        (x - w/2 < 0 + wallMargin) -- Cerca del borde izquierdo
     || (x + w/2 > worldW - wallMargin) -- Cerca del borde derecho
     || (y - h/2 < 0 + wallMargin) -- Cerca del borde superior
     || (y + h/2 > worldH - wallMargin) -- Cerca del borde inferior

-- Búsqueda del enemigo vivo más cercano detectado por el radar de 'self'.
findClosestEnemy :: Robot -> GameState -> Maybe Robot
findClosestEnemy self gs =
  let myId = objId (robotBase self)
      myPos = objPos (robotBase self)
      -- Filtra enemigos: que no sea yo, que esté vivo, y que lo detecte mi radar.
      enemies = filter (\r -> objId (robotBase r) /= myId && isRobotAlive r && detectedAgent self r) (robots gs)
  in
    case enemies of
      [] -> Nothing
      lista -> Just $ minimumBy (comparing (distance myPos . objPos . robotBase)) lista

-- Encuentra el robot enemigo vivo más cercano A UN PUNTO dado (p.ej. un power-up).
-- Nota: No usa el radar, considera a todos los robots como competidores.
findClosestEnemyToPoint :: Point -> Robot -> GameState -> Maybe Robot
findClosestEnemyToPoint targetPos self gs =
  let myId = objId (robotBase self)
      -- Considera a TODOS los otros robots vivos como potenciales competidores.
      enemies = filter (\r -> objId (robotBase r) /= myId && isRobotAlive r) (robots gs)
  in
    case enemies of
      [] -> Nothing
      lista -> Just $ minimumBy (comparing (distance targetPos . objPos . robotBase)) lista

-- Distancia del "sensor" frontal para detectar obstáculos.
feelerDistance :: Robot -> Float
feelerDistance r = let (V2 (w, _)) = objSize (robotBase r) in w / 2 + 10.0 -- 10 unidades por delante del robot

-- Comprueba si un punto "sensor" delante del robot está dentro de otro robot.
isPathBlocked :: Robot -> GameState -> Bool
isPathBlocked r gs =
  let
    myPos = objPos (robotBase r)
    myAngle = objAngle (robotBase r)
    myDir = dirFromAngle myAngle

    -- Calcula el punto del "sensor"
    feelerPoint = addV myPos (scaleV (feelerDistance r) myDir)

    -- Comprueba contra todos los OTROS robots
    otherRobots = filter (\other -> objId (robotBase other) /= objId (robotBase r)) (robots gs)
  in
    -- Devuelve True si el sensor está dentro de CUALQUIER otro robot
    any (\other -> fromMaybe False (isPointInsideRectangle feelerPoint (robotVerts other))) otherRobots

-- Genera acción de movimiento, evitando paredes.
-- Nota: 'isPathBlocked' no se usa aquí; esta función solo maneja paredes.
getSafeMoveAction :: Robot -> GameState -> [Action]
getSafeMoveAction r gs
  | isNearWall gs r = [ Action r (ROTATE_ROBOT_ACTION wallTurn) -- Gira si está cerca de la pared
                   , Action r MOVE_FORWARD_ACTION ]
  | otherwise    = [ Action r MOVE_FORWARD_ACTION ] -- Avanza si no

-- (Las siguientes funciones no se usan actualmente en la lógica de IA, pero se mantienen)

-- Devuelve el primer enemigo detectado.
firstDetectedEnemy :: Robot -> GameState -> Maybe Robot
firstDetectedEnemy self gs =
  let meId = objId (robotBase self)
      esEnemigo r = objId (robotBase r) /= meId && isRobotAlive r && detectedAgent self r
  in case filter esEnemigo (robots gs) of
       []    -> Nothing
       (x:_) -> Just x

-- Calcula el giro relativo del chasis necesario para apuntar a un objetivo.
bodyTurnTo :: Robot -> Robot -> Angle
bodyTurnTo self target =
  let myPos      = objPos (robotBase self)
      targetPos  = objPos (robotBase target)
      desired    = angleToTarget myPos targetPos
  in normalizeAngle (desired - objAngle (robotBase self))

-- Radio de separación deseado para evitar colisiones.
desiredSeparation :: Robot -> Robot -> Float
desiredSeparation a b =
  let V2 (wa,ha) = objSize (robotBase a)
      V2 (wb,hb) = objSize (robotBase b)
      ra = 0.5 * sqrt (wa*wa + ha*ha) -- Radio aproximado
      rb = 0.5 * sqrt (wb*wb + hb*hb)
  in 1.2 * (ra + rb)  -- 120% de la suma de radios

-- Comprueba si 'other' es una amenaza (RAMMER o más pesado).
isThreat :: Robot -> Robot -> Bool
isThreat me other =
  robotBehavior other == RAMMER || getRobotWeight (robotType other) > getRobotWeight (robotType me)

-- Calcula un vector de "empuje" para separarse de otros robots cercanos.
separationVector :: Robot -> [Robot] -> Vector
separationVector me others =
  let myPos = objPos (robotBase me)
      steer = foldr addV (v2 0 0)
        [ let d = subV myPos (objPos (robotBase o))
              dist = norm d
          in if dist > 1e-3 && dist < desiredSeparation me o
               then fmap (/ (dist*dist)) d -- Fuerza inversa al cuadrado de la distancia
               else v2 0 0
        | o <- others, objId (robotBase o) /= objId (robotBase me), isRobotAlive o
        ]
  in steer

-- --- Lógica de 'Wander' (Deambular) ---

-- Lógica de deambulación mejorada, usando la memoria del agente.
wanderActions :: Robot -> GameState -> [Action]
wanderActions r gs
  -- 1. Si está cerca de la pared, gira 90º, borra el objetivo de giro y avanza.
  | isNearWall gs r =
      [ Action r (ROTATE_ROBOT_ACTION wallTurn)
      , Action r (SET_MEMORY_ACTION "targetWanderAngle" Nothing) -- Borra ángulo objetivo
      , Action r MOVE_FORWARD_ACTION -- Avanza
      ]

  -- 2. Si está en medio de un giro (tiene un "targetWanderAngle" en memoria).
  | Just (MFloat targetAngle) <- Map.lookup "targetWanderAngle" (robotMem r) =
      let
        currentAngle = objAngle (robotBase r)
        delta = normalizeAngle (targetAngle - currentAngle)
      in
        if abs delta < wanderTurnTolerance then
          -- 2a. Giro completado: Borra el objetivo y sigue recto.
          [ Action r (SET_MEMORY_ACTION "targetWanderAngle" Nothing)
          ] ++ getSafeMoveAction r gs
        else
          -- 2b. Sigue girando suavemente hacia el objetivo.
          [ Action r (ROTATE_ROBOT_ACTION delta)
          ] ++ getSafeMoveAction r gs

  -- 3. Si el temporizador de 'wander' llega a 0, inicia un nuevo giro.
  | robotWanderTimer r <= 0 =
      let
        currentAngle = objAngle (robotBase r)
        -- Gira en sentidos opuestos según el ID para parecer menos artificial.
        turnAngle = if objId (robotBase r) `mod` 2 == 0
                    then smallWanderTurn
                    else -smallWanderTurn
        targetAngle = normalizeAngle (currentAngle + turnAngle)
      in
        -- Guarda el ángulo objetivo en memoria y resetea el timer.
        [ Action r (SET_MEMORY_ACTION "targetWanderAngle" (Just (MFloat targetAngle)))
        , Action r RESET_WANDER_TIMER_ACTION
        ] -- No se mueve en este frame, solo decide girar.

  -- 4. Si ninguna de las anteriores se cumple, sigue adelante.
  | otherwise =
      getSafeMoveAction r gs

-- --- Funciones Auxiliares de Acción ---

-- Genera acciones para girar el chasis hacia un punto y avanzar.
actionsToMoveTowards :: Robot -> Position -> [Action]
actionsToMoveTowards r targetPos =
  let myPos = objPos (robotBase r)
      targetAngle = angleToTarget myPos targetPos
      bodyAngleDelta = normalizeAngle (targetAngle - objAngle (robotBase r))
  in [ Action r (ROTATE_ROBOT_ACTION bodyAngleDelta) -- Girar chasis
     , Action r MOVE_FORWARD_ACTION ]                -- Avanzar

-- Genera acciones para apuntar la torreta a un enemigo y disparar si está alineada.
actionsToAimAndFire :: Robot -> Robot -> [Action]
actionsToAimAndFire self target =
  let targetPos = objPos $ robotBase target
      myPos = objPos $ robotBase self
      targetAngle = angleToTarget myPos targetPos
      currentTurretAngle = turretAngle self
      angleDelta = normalizeAngle (targetAngle - currentTurretAngle)
      rotationAction = Action self (ROTATE_TURRET_ACTION angleDelta)
  in if abs angleDelta < fireTolerance then
        -- Si está apuntado (dentro de la tolerancia), gira y dispara.
        [ rotationAction, Action self FIRE_ACTION ]
     else
        -- Si no, solo gira la torreta.
        [ rotationAction ]

-- *** NUEVA FUNCIÓN AUXILIAR ***
-- Intenta investigar la última posición conocida del enemigo o, si no, deambula.
investigateOrWander :: Robot -> GameState -> [Action]
investigateOrWander r gs =
  case Map.lookup "last_seen_pos" (robotMem r) of
    Just (MPoint rememberedPos) ->
      let myPos = objPos (robotBase r)
          distToLastSeen = distance myPos rememberedPos
      in if distToLastSeen < investigationClearDistance then
           -- Ya llegamos/estamos cerca: olvidar y empezar a deambular
           [ Action r (SET_MEMORY_ACTION "last_seen_pos" Nothing) ] ++ wanderActions r gs
         else
           -- Ir hacia la última posición vista
           actionsToMoveTowards r rememberedPos
    Nothing ->
      -- No hay nada en memoria, simplemente deambular
      wanderActions r gs

-- --- Despachador Principal de IA ---

-- Obtiene las acciones para todos los robots controlados por la IA.
getAIActions :: GameState -> [Action]
getAIActions gs =
    let activeAIRobots = filter (\r -> isRobotAlive r && robotBehavior r /= PLAYER) (robots gs)
    in concatMap (`runAI` gs) activeAIRobots

-- Elige qué comportamiento ejecutar según el 'robotBehavior'.
runAI :: Robot -> GameState -> [Action]
runAI r gs =
  case robotBehavior r of
    AGGRESSIVE -> aggressiveAI r gs
    BALANCED   -> balancedAI r gs
    DEFENSIVE  -> defensiveAI r gs
    PEACEFUL   -> peacefulAI r gs
    RAMMER     -> rammerAI r gs
    PLAYER     -> [] -- El jugador no es controlado por esta función.

-- --- Definiciones de Comportamiento Específicos ---

-- Comprueba si el robot 'r' debería ir a por el power-up 'pu'.
-- Compara su distancia con la del enemigo más cercano al power-up.
shouldGoForPowerUp :: Robot -> PowerUp -> GameState -> Bool
shouldGoForPowerUp r pu gs =
  let
    powerUpPosition = puPos pu
    myDist = distance (objPos $ robotBase r) powerUpPosition
    closestEnemyToPU = findClosestEnemyToPoint powerUpPosition r gs
  in
    case closestEnemyToPU of
      Nothing -> True -- Nadie más compite, ¡ve a por él!
      Just enemy -> myDist <= distance (objPos $ robotBase enemy) powerUpPosition * powerUpDistanceFactor


-- 1. AGRESIVO:
--    - Prioriza power-up si es ventajoso.
--    - Si ve un enemigo, lo ataca y persigue (guardando su pos).
--    - Si no ve enemigo pero recuerda dónde vio uno, investiga.
--    - Si no, deambula.
aggressiveAI :: Robot -> GameState -> [Action]
aggressiveAI r gs =
  case powerUp gs of
    Just pu | shouldGoForPowerUp r pu gs -> -- Hay un power-up y decido ir
            let moveActions = actionsToMoveTowards r (puPos pu)
                nearbyEnemy = findClosestEnemy r gs -- Busca enemigo cercano mientras va al PU
                shootActions = case nearbyEnemy of
                                 Just target -> Action r (SET_MEMORY_ACTION "last_seen_pos" (Just (MPoint (objPos $ robotBase target)))) : actionsToAimAndFire r target
                                 Nothing -> []
            in moveActions ++ shootActions

    _ -> -- No hay power-up, o no es ventajoso ir
      case findClosestEnemy r gs of
        Just target -> -- Enemigo visible
          Action r (SET_MEMORY_ACTION "last_seen_pos" (Just (MPoint (objPos $ robotBase target)))) -- Guarda pos
          : actionsToMoveTowards r (objPos $ robotBase target) -- Persigue
          ++ actionsToAimAndFire r target -- Dispara
        Nothing -> -- No hay enemigo visible
          investigateOrWander r gs -- Investiga o deambula


-- 2. BALANCEADO:
--    - Prioriza power-up si es ventajoso.
--    - Si ve un enemigo, le dispara (guardando su pos) pero NO se mueve.
--    - Si no ve enemigo pero recuerda dónde vio uno, investiga (moviéndose).
--    - Si no, deambula.
balancedAI :: Robot -> GameState -> [Action]
balancedAI r gs =
  case powerUp gs of
    Just pu | shouldGoForPowerUp r pu gs -> -- Hay un power-up y decido ir
            let moveActions = actionsToMoveTowards r (puPos pu)
                nearbyEnemy = findClosestEnemy r gs -- Busca enemigo cercano mientras va al PU
                shootActions = case nearbyEnemy of
                                 Just target -> Action r (SET_MEMORY_ACTION "last_seen_pos" (Just (MPoint (objPos $ robotBase target)))) : actionsToAimAndFire r target
                                 Nothing -> []
            in moveActions ++ shootActions

    _ -> -- No hay power-up, o no es ventajoso ir
      case findClosestEnemy r gs of
        Just target -> -- Enemigo visible
           Action r (SET_MEMORY_ACTION "last_seen_pos" (Just (MPoint (objPos $ robotBase target)))) -- Guarda pos
           : actionsToAimAndFire r target -- Solo dispara
        Nothing -> -- No hay enemigo visible
          investigateOrWander r gs -- Investiga o deambula


-- 3. DEFENSIVO:
--    - Prioriza power-up si es ventajoso.
--    - Si ve un enemigo, le dispara (guardando su pos).
--    - Si el enemigo está muy cerca, retrocede mientras dispara.
--    - Si no ve enemigo pero recuerda dónde vio uno, investiga (avanzando).
--    - Si no, deambula.
defensiveAI :: Robot -> GameState -> [Action]
defensiveAI r gs =
  case powerUp gs of
    Just pu | shouldGoForPowerUp r pu gs -> -- Hay un power-up y decido ir
            let moveActions = actionsToMoveTowards r (puPos pu)
                nearbyEnemy = findClosestEnemy r gs -- Busca enemigo cercano mientras va al PU
                shootActions = case nearbyEnemy of
                                 Just target -> Action r (SET_MEMORY_ACTION "last_seen_pos" (Just (MPoint (objPos $ robotBase target)))) : actionsToAimAndFire r target
                                 Nothing -> []
            in moveActions ++ shootActions

    _ -> -- No hay power-up, o no es ventajoso ir
      case findClosestEnemy r gs of
        Just target -> -- Enemigo visible
            let targetPos = objPos $ robotBase target
                myPos = objPos $ robotBase r
                dist = distance myPos targetPos
                aimActions = actionsToAimAndFire r target
                retreatAction = [Action r MOVE_BACKWARD_ACTION]
                savePosAction = Action r (SET_MEMORY_ACTION "last_seen_pos" (Just (MPoint targetPos)))
            in if dist < radarLength r * 0.5 then
                 savePosAction : aimActions ++ retreatAction -- Guarda, dispara y retrocede
               else
                 savePosAction : aimActions -- Guarda y dispara
        Nothing -> -- No hay enemigo visible
          investigateOrWander r gs -- Investiga o deambula


-- 4. PACÍFICO:
--    - Prioriza power-up si es ventajoso.
--    - NUNCA dispara ni guarda posiciones de enemigos.
--    - Si no va a por un power-up, deambula.
peacefulAI :: Robot -> GameState -> [Action]
peacefulAI r gs =
  case powerUp gs of
    Just pu | shouldGoForPowerUp r pu gs -> -- Hay un power-up y decido ir
           actionsToMoveTowards r (puPos pu) -- Solo se mueve

    _ -> -- No hay power-up, o no es ventajoso ir
      -- Olvida cualquier posición guardada (por si acaso cambió de rol) y deambula
      [ Action r (SET_MEMORY_ACTION "last_seen_pos" Nothing) ] ++ wanderActions r gs


-- 5. RAMMER (Embestidor):
--    - Prioriza power-up si es ventajoso.
--    - NUNCA dispara.
--    - Si ve un enemigo, lo persigue (priorizando <= peso) y guarda su pos.
--    - Si no ve enemigo pero recuerda dónde vio uno, investiga.
--    - Si no, deambula.
rammerAI :: Robot -> GameState -> [Action]
rammerAI r gs =
  case powerUp gs of
    Just pu | shouldGoForPowerUp r pu gs -> -- Hay un power-up y decido ir
           actionsToMoveTowards r (puPos pu) -- Solo se mueve hacia el powerup

    _ -> -- No hay power-up, o no es ventajoso ir
      ramClosestOrElseInvestigateWander r gs
  where
      ramClosestOrElseInvestigateWander self state =
          let myPos = objPos (robotBase self)
              detectedEnemies = filter (\x -> objId (robotBase x) /= objId (robotBase self) && isRobotAlive x && detectedAgent self x) (robots state)
              smallerOrEqual = [ o | o <- detectedEnemies, getRobotWeight (robotType o) <= getRobotWeight (robotType self) ]
              target = if not (null smallerOrEqual)
                       then Just $ minimumBy (comparing (distance myPos . objPos . robotBase)) smallerOrEqual
                       else findClosestEnemy self state -- Usa findClosestEnemy aquí por si no hay <= peso
          in case target of
              Just t  -> -- Enemigo visible
                Action self (SET_MEMORY_ACTION "last_seen_pos" (Just (MPoint (objPos $ robotBase t)))) -- Guarda pos
                : actionsToMoveTowards self (objPos $ robotBase t) -- Persigue
              Nothing -> -- No hay enemigo visible
                investigateOrWander self state -- Investiga o deambula