module IA where

import Entities
import Math
import Data.Ord (comparing)
import Data.List (minimumBy, sortOn) -- Importado para ordenar
import Data.Maybe (fromMaybe, listToMaybe) -- Importado para manejar Maybe
import qualified Data.Map as Map

-- --- Constantes de Comportamiento de IA ---

-- Tiempo (en frames) que un bot debe estar quieto para activar la maniobra de "desatasco".
stuckTimeThreshold :: Int
stuckTimeThreshold = 60 -- 1 segundo a 60fps

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

-- Comprueba si hay un obstáculo 'WALL' entre 'self' y 'target'.
hasLineOfSight :: Robot -> Robot -> GameState -> Bool
hasLineOfSight self target gs =
  let
    myPos = objPos (robotBase self)
    targetPos = objPos (robotBase target)
    
    -- Solo nos importan los muros, no las zonas de daño
    wallObstacles = filter (\o -> obsType o == WALL) (obstacles gs)
    
    -- Comprueba si el segmento (myPos, targetPos) intersecta *alguno* de los muros
    isBlocked = any (\obs -> fromMaybe False (checkSegmentRectIntersection myPos targetPos (obsVerts obs))) wallObstacles
    
  in
    not isBlocked -- Hay línea de visión si NO está bloqueado

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
feelerDistance r = let (V2 (w, _)) = objSize (robotBase r) in w / 2 + 20.0

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
    robotBlocked = any (\other -> fromMaybe False (isPointInsideRectangle feelerPoint (robotVerts other))) otherRobots
    
    -- Comprueba contra TODOS los obstáculos
    allObstacles = obstacles gs
    obstacleBlocked = any (\obs -> fromMaybe False (isPointInsideRectangle feelerPoint (obsVerts obs))) allObstacles

  in
    -- Devuelve True si el sensor está dentro de un robot O un obstáculo
    robotBlocked || obstacleBlocked

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
  -- 1. <<< LÓGICA DE MANIOBRA >>>
  -- 1a. Fase 1: Acabamos de retroceder. Ahora debemos empezar a girar.
  | Just (MString "reversing_wall") <- Map.lookup "maneuver" (robotMem r) =
      let
        currentAngle = objAngle (robotBase r)
        -- Gira en sentidos opuestos según el ID para parecer menos artificial
        turnAngle = if objId (robotBase r) `mod` 2 == 0 then wallTurn else -wallTurn
        targetAngle = normalizeAngle (currentAngle + turnAngle)
      in
        -- Guarda el ángulo objetivo, cambia el estado de la maniobra e inicia el giro
        [ Action r (SET_MEMORY_ACTION "maneuver" (Just (MString "turning_from_wall")))
        , Action r (SET_MEMORY_ACTION "targetWanderAngle" (Just (MFloat targetAngle)))
        , Action r (ROTATE_ROBOT_ACTION turnAngle) -- Inicia el giro este mismo frame
        ]

  -- 1b. Fase 2: Estamos en medio del giro de evasión.
  | Just (MString "turning_from_wall") <- Map.lookup "maneuver" (robotMem r) =
      case Map.lookup "targetWanderAngle" (robotMem r) of
        -- Si no hay ángulo objetivo (raro), abortar maniobra
        Nothing -> [ Action r (SET_MEMORY_ACTION "maneuver" Nothing) ] 
        
        Just (MFloat targetAngle) ->
          let
            currentAngle = objAngle (robotBase r)
            delta = normalizeAngle (targetAngle - currentAngle)
          in
            if abs delta < wanderTurnTolerance then
              -- Giro completado: Borra la maniobra y el ángulo.
              -- NO te muevas. Deja que la IA principal decida en el próximo frame.
              [ Action r (SET_MEMORY_ACTION "targetWanderAngle" Nothing)
              , Action r (SET_MEMORY_ACTION "maneuver" Nothing)
              ]
            else
              -- Sigue girando (suavemente, `applyAction` lo clampeará)
              [ Action r (ROTATE_ROBOT_ACTION delta)
              ]

  -- 2. Si está cerca de la pared o bloqueado (Y NO está en una maniobra)
  | isNearWall gs r || isPathBlocked r gs =
      -- Inicia la Fase 1 de la maniobra: Muévete hacia atrás y guarda en memoria.
      [ Action r (SET_MEMORY_ACTION "maneuver" (Just (MString "reversing_wall")))
        -- Borra cualquier objetivo de paseo anterior
      , Action r (SET_MEMORY_ACTION "targetWanderAngle" Nothing) 
      , Action r MOVE_BACKWARD_ACTION -- ¡Acción principal: solo moverse hacia atrás!
      ]

  -- 3. Si está en medio de un giro de 'wander' NORMAL (no de evasión).
  --    (Esta lógica es la que ahora usa "turning_from_wall")
  | Just (MFloat targetAngle) <- Map.lookup "targetWanderAngle" (robotMem r) =
      let
        currentAngle = objAngle (robotBase r)
        delta = normalizeAngle (targetAngle - currentAngle)
      in
        if abs delta < wanderTurnTolerance then
          -- 3a. Giro completado: Borra el objetivo y sigue recto.
          [ Action r (SET_MEMORY_ACTION "targetWanderAngle" Nothing)
          , Action r MOVE_FORWARD_ACTION
          ]
        else
          -- 3b. Sigue girando suavemente hacia el objetivo.
          [ Action r (ROTATE_ROBOT_ACTION delta)
          , Action r MOVE_FORWARD_ACTION
          ]

  -- 4. Si el temporizador de 'wander' llega a 0, inicia un nuevo giro NORMAL.
  | robotWanderTimer r <= 0 =
      let
        currentAngle = objAngle (robotBase r)
        turnAngle = if objId (robotBase r) `mod` 2 == 0
                    then smallWanderTurn
                    else -smallWanderTurn
        targetAngle = normalizeAngle (currentAngle + turnAngle)
      in
        [ Action r (SET_MEMORY_ACTION "targetWanderAngle" (Just (MFloat targetAngle)))
        , Action r RESET_WANDER_TIMER_ACTION
        ] 

  -- 5. Si ninguna de las anteriores se cumple, sigue adelante.
  | otherwise =
      [ Action r MOVE_FORWARD_ACTION ]

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
actionsToAimAndFire :: Robot -> Robot -> GameState -> [Action]
actionsToAimAndFire self target gs =
  let targetPos = objPos $ robotBase target
      myPos = objPos $ robotBase self
      targetAngle = angleToTarget myPos targetPos
      currentTurretAngle = turretAngle self
      angleDelta = normalizeAngle (targetAngle - currentTurretAngle)
      rotationAction = Action self (ROTATE_TURRET_ACTION angleDelta)
      
      -- Comprobación de Línea de Visión
      canShoot = hasLineOfSight self target gs
      
  in if abs angleDelta < fireTolerance && canShoot then
        -- Si está apuntado (dentro de la tolerancia) Y tiene LOS, gira y dispara.
        [ rotationAction, Action self FIRE_ACTION ]
     else
        -- Si no, solo gira la torreta.
        [ rotationAction ]

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
           -- Ir hacia la última posición vista...
           if isPathBlocked r gs || isNearWall gs r then
             -- Inicia la maniobra de evasión.
             [ Action r (SET_MEMORY_ACTION "maneuver" (Just (MString "reversing_wall")))
             , Action r (SET_MEMORY_ACTION "targetWanderAngle" Nothing)
             , Action r MOVE_BACKWARD_ACTION
             ]
           else
             -- Camino libre: seguir investigando.
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
runAI r gs
  -- PRIORIDAD 1: DESATASCO FÍSICO (STUCK TIMER)
  | robotStuckTimer r > stuckTimeThreshold =
      let
        moveDirChoice = (robotStuckTimer r `div` 60) `mod` 2
        unstickMoveAction = if moveDirChoice == 0
                            then MOVE_BACKWARD_ACTION
                            else MOVE_FORWARD_ACTION
        turnDirChoice = (robotStuckTimer r `div` 120) `mod` 2
        unstickTurnAngle = if turnDirChoice == 0 then wallTurn else -wallTurn
      in
        [ Action r unstickMoveAction
        , Action r (ROTATE_ROBOT_ACTION unstickTurnAngle)
        , Action r (SET_MEMORY_ACTION "maneuver" Nothing)
        , Action r (SET_MEMORY_ACTION "targetWanderAngle" Nothing)
        ]
  
  -- PRIORIDAD 2: MANIOBRA DE EVASIÓN EN CURSO
  -- Fase 1: Está retrocediendo
  | Just (MString "reversing_wall") <- Map.lookup "maneuver" (robotMem r) =
      -- Llama a wanderActions (ejecutará Bloque 1a)
      wanderActions r gs
  
  -- Fase 2: Está girando
  | Just (MString "turning_from_wall") <- Map.lookup "maneuver" (robotMem r) =
      -- Llama a wanderActions (ejecutará Bloque 1b)
      wanderActions r gs

  -- PRIORIDAD 3: COMPORTAMIENTO NORMAL (Si no hay maniobra activa)
  | otherwise = case robotBehavior r of
      AGGRESSIVE -> aggressiveAI r gs
      BALANCED   -> balancedAI r gs
      DEFENSIVE  -> defensiveAI r gs
      PEACEFUL   -> peacefulAI r gs
      RAMMER     -> rammerAI r gs
      PLAYER     -> []

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
      Nothing -> True -- Nadie más compite, por lo que va a por el power-up
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
            let
                nearbyEnemy = findClosestEnemy r gs
                -- Apunta/dispara a enemigos cercanos incluso si vas a por el power-up
                shootActions = case nearbyEnemy of
                                 Just target -> Action r (SET_MEMORY_ACTION "last_seen_pos" (Just (MPoint (objPos $ robotBase target)))) : actionsToAimAndFire r target gs
                                 Nothing -> []
                
                -- Comprueba si el camino al power-up está bloqueado
                moveActions = if isPathBlocked r gs || isNearWall gs r then
                                -- Camino bloqueado: Evadir (retroceder/girar)
                                wanderActions r gs
                              else
                                -- Camino libre: Ir a por el power-up
                                actionsToMoveTowards r (puPos pu)
            in
               moveActions ++ shootActions

    _ -> -- No hay power-up, o no es ventajoso ir
      case findClosestEnemy r gs of
        Just target -> -- Enemigo visible
          let
            -- Las acciones base son siempre guardar pos y apuntar/disparar
            baseActions = Action r (SET_MEMORY_ACTION "last_seen_pos" (Just (MPoint (objPos $ robotBase target))))
                          : actionsToAimAndFire r target gs
          in
            if isPathBlocked r gs || isNearWall gs r then
              -- Inicia la maniobra.
              baseActions ++
                [ Action r (SET_MEMORY_ACTION "maneuver" (Just (MString "reversing_wall")))
                , Action r (SET_MEMORY_ACTION "targetWanderAngle" Nothing)
                , Action r MOVE_BACKWARD_ACTION
                ]
            else
              -- Camino libre: Perseguir.
              baseActions ++ actionsToMoveTowards r (objPos $ robotBase target)
              
        Nothing -> -- No hay enemigo visible
          investigateOrWander r gs

-- 2. BALANCEADO:
--    - Prioriza power-up si es ventajoso.
--    - Si ve un enemigo, le dispara (guardando su pos) pero NO se mueve.
--    - Si no ve enemigo pero recuerda dónde vio uno, investiga (moviéndose).
--    - Si no, deambula.
balancedAI :: Robot -> GameState -> [Action]
balancedAI r gs =
  case powerUp gs of
    Just pu | shouldGoForPowerUp r pu gs -> -- Hay un power-up y decido ir
            let
                nearbyEnemy = findClosestEnemy r gs
                shootActions = case nearbyEnemy of
                                 Just target -> Action r (SET_MEMORY_ACTION "last_seen_pos" (Just (MPoint (objPos $ robotBase target)))) : actionsToAimAndFire r target gs
                                 Nothing -> []
                

                moveActions = if isPathBlocked r gs || isNearWall gs r then
                                -- Inicia la maniobra de evasión.
                                [ Action r (SET_MEMORY_ACTION "maneuver" (Just (MString "reversing_wall")))
                                , Action r (SET_MEMORY_ACTION "targetWanderAngle" Nothing)
                                , Action r MOVE_BACKWARD_ACTION
                                ]
                              else
                                actionsToMoveTowards r (puPos pu)
            in moveActions ++ shootActions

    _ -> -- No hay power-up, o no es ventajoso ir
      case findClosestEnemy r gs of
        Just target -> -- Enemigo visible
           Action r (SET_MEMORY_ACTION "last_seen_pos" (Just (MPoint (objPos $ robotBase target)))) -- Guarda pos
           : actionsToAimAndFire r target gs -- Solo dispara
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
            let
                nearbyEnemy = findClosestEnemy r gs
                shootActions = case nearbyEnemy of
                                 Just target -> Action r (SET_MEMORY_ACTION "last_seen_pos" (Just (MPoint (objPos $ robotBase target)))) : actionsToAimAndFire r target gs
                                 Nothing -> []
                moveActions = if isPathBlocked r gs || isNearWall gs r then
                                wanderActions r gs
                              else
                                actionsToMoveTowards r (puPos pu)
            in moveActions ++ shootActions

    _ -> -- No hay power-up, o no es ventajoso ir
      case findClosestEnemy r gs of
        -- ... (La lógica de retroceder cuando el enemigo está cerca está bien,
        -- ya que 'MOVE_BACKWARD_ACTION' no se bloqueará si 'isPathBlocked' es true)
        Just target -> -- Enemigo visible
            let targetPos = objPos $ robotBase target
                myPos = objPos $ robotBase r
                dist = distance myPos targetPos
                aimActions = actionsToAimAndFire r target gs
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
           if isPathBlocked r gs || isNearWall gs r then
             -- Inicia la maniobra de evasión.
             [ Action r (SET_MEMORY_ACTION "maneuver" (Just (MString "reversing_wall")))
             , Action r (SET_MEMORY_ACTION "targetWanderAngle" Nothing)
             , Action r MOVE_BACKWARD_ACTION
             ]
           else
             actionsToMoveTowards r (puPos pu) -- Ir

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
           if isPathBlocked r gs || isNearWall gs r then
             -- Inicia la maniobra de evasión.
             [ Action r (SET_MEMORY_ACTION "maneuver" (Just (MString "reversing_wall")))
             , Action r (SET_MEMORY_ACTION "targetWanderAngle" Nothing)
             , Action r MOVE_BACKWARD_ACTION
             ]
           else
             actionsToMoveTowards r (puPos pu) -- Ir

    _ -> -- No hay power-up, o no es ventajoso ir
      ramClosestOrElseInvestigateWander r gs
  where
      ramClosestOrElseInvestigateWander self state =
          let myPos = objPos (robotBase self)
              detectedEnemies = filter (\x -> objId (robotBase x) /= objId (robotBase self) && isRobotAlive x && detectedAgent self x) (robots state)
              smallerOrEqual = [ o | o <- detectedEnemies, getRobotWeight (robotType o) <= getRobotWeight (robotType self) ]
              target = if not (null smallerOrEqual)
                       then Just $ minimumBy (comparing (distance myPos . objPos . robotBase)) smallerOrEqual
                       else findClosestEnemy self state
          in case target of
              Just t  -> -- Enemigo visible
                let
                  baseActions = [Action self (SET_MEMORY_ACTION "last_seen_pos" (Just (MPoint (objPos $ robotBase t))))] -- Guarda pos
                in
                  if isPathBlocked self state || isNearWall state self then
                    -- Camino bloqueado: NO perseguir.
                    -- Ejecutar la lógica de evasión de 'wander'.
                    baseActions ++ wanderActions self state
                  else
                    -- Camino libre: Perseguir.
                    baseActions ++ actionsToMoveTowards self (objPos $ robotBase t)

              Nothing -> -- No hay enemigo visible
                investigateOrWander self state -- Investiga o deambula