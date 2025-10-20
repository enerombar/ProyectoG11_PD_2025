module IA where

import Entities
import Math
import Game (gameSize) -- Necesario para isNearWall
import Data.Ord (comparing)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

-- Constante de tolerancia para el disparo. No necesita apuntar perfecto antes de disparar. 
fireTolerance :: Angle
fireTolerance = 0.06 -- (Unos 3.4 grados)

smallWanderTurn :: Angle
smallWanderTurn = pi / 6 -- (30 grados)

wallMargin :: Float
wallMargin = 15.0

wallTurn :: Angle
wallTurn = pi / 2 -- (90 grados)

wanderTurnTolerance :: Angle -- Tolerancia para el giro (No necesita giro perfecto para evitar bucles)
wanderTurnTolerance = 0.1

-- Lógica de detección
detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 = distance (objPos $ robotBase r1) (objPos $ robotBase r2) <= radarLength r1

-- Lógica de paredes
isNearWall :: Robot -> Bool
isNearWall r =
  let
    (x, y) = unV2 (objPos $ robotBase r)
    (w, h) = unV2 (objSize $ robotBase r)
    (worldW, worldH) = unV2 gameSize
  in
        (x - w/2 < wallMargin)
     || (x + w/2 > worldW - wallMargin)
     || (y - h/2 < wallMargin)
     || (y + h/2 > worldH - wallMargin)

-- Búsqueda de enemigo más cercano
findClosestEnemy :: Robot -> GameState -> Maybe Robot
findClosestEnemy self gs =
  let myId = objId (robotBase self)
      myPos = objPos (robotBase self)
      enemies = filter (\r -> objId (robotBase r) /= myId && isRobotAlive r && detectedAgent self r) (robots gs)
  in
    case enemies of
      [] -> Nothing
      lista -> Just $ minimumBy (comparing (distance myPos . objPos . robotBase)) lista

feelerDistance :: Robot -> Float --Detectar obstaculos inmediatamente delante 
feelerDistance r = let (V2 (w, _)) = objSize (robotBase r) in w / 2 + 10.0 -- 10 unidades por delante del centro del robot

-- Comprueba si un punto "sensor" delante del robot está dentro de otro robot
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

-- Devuelve la acción de movimiento apropiada (avanzar o esquivar)
getSafeMoveAction :: Robot -> GameState -> [Action]
getSafeMoveAction r gs
    | isPathBlocked r gs =
        -- Camino bloqueado: frena y gira
        [ Action r STOP_ACTION
        , Action r (ROTATE_ROBOT_ACTION wallTurn) -- Gira 90 grados
        ]
    | otherwise =
        -- Camino despejado: avanza
        [ Action r MOVE_FORWARD_ACTION ]

-- Lógica de deambulación mejorada con estado en memoria
wanderActions :: Robot -> GameState -> [Action]
wanderActions r gs
  -- 1. Si está cerca de la pared, gira 90º
  | isNearWall r =
      [ Action r (ROTATE_ROBOT_ACTION wallTurn)
      , Action r (SET_MEMORY_ACTION "targetWanderAngle" Nothing) -- Borramos cualquier angulo objetivo de memoria
      ] ++ getSafeMoveAction r gs --Avanzamos una vez girado los 90º

  -- 2. Comprobar si estamos en medio de un giro de "wander"
  | Just (MFloat targetAngle) <- Map.lookup "targetWanderAngle" (robotMem r) =
      let
        currentAngle = objAngle (robotBase r)
        delta = normalizeAngle (targetAngle - currentAngle)
      in
        if abs delta < wanderTurnTolerance then
          -- 2a. Giro completado: Borra el objetivo y sigue recto
          [ Action r (SET_MEMORY_ACTION "targetWanderAngle" Nothing)
          ] ++ getSafeMoveAction r gs
        else
          -- 2b. Sigue girando suavemente
          [ Action r (ROTATE_ROBOT_ACTION delta)
          ] ++ getSafeMoveAction r gs

  -- 3. Si el timer llega a 0, INICIA un nuevo giro
  | robotWanderTimer r <= 0 =
      let
        currentAngle = objAngle (robotBase r)
        turnAngle = if objId (robotBase r) `mod` 2 == 0 -- Dar sensacion menos artificial. No giran todos en mismo sentido 
                    then smallWanderTurn
                    else -smallWanderTurn
        targetAngle = normalizeAngle (currentAngle + turnAngle)
      in
        [ Action r (SET_MEMORY_ACTION "targetWanderAngle" (Just (MFloat targetAngle)))
        , Action r RESET_WANDER_TIMER_ACTION
        ] -- No se mueve en este frame, solo decide girar
         
  -- 4. Si ninguna de las anteriores se cumple sigue adelante
  | otherwise =
      getSafeMoveAction r gs

-- --- Función principal de despacho de IA ---
getAIActions :: GameState -> [Action]
getAIActions gs =
    let activeRobots = filter isRobotAlive (robots gs)
    in concatMap (`runAI` gs) activeRobots

-- Elige qué comportamiento ejecutar
runAI :: Robot -> GameState -> [Action]
runAI r gs =
  case robotBehavior r of
    AGGRESSIVE -> aggressiveAI r gs
    BALANCED   -> balancedAI r gs
    DEFENSIVE  -> defensiveAI r gs
    PEACEFUL   -> peacefulAI r gs

-- --- Definiciones de Comportamiento ---

-- 1. AGRESIVO: Además de disparar se mueve hacia el enemigo
aggressiveAI :: Robot -> GameState -> [Action]
aggressiveAI r gs =
  case findClosestEnemy r gs of
    Just target -> --encuentra enemigo más cercano
      let
        targetPos = objPos $ robotBase target
        myPos = objPos $ robotBase r
        targetAngle = angleToTarget myPos targetPos --Angulo hacia enemigo
        currentTurretAngle = turretAngle r
        angleDelta = normalizeAngle (targetAngle - currentTurretAngle) --Calcula cuanto debe girar torreta para apuntar
        bodyAngleDelta = normalizeAngle (targetAngle - objAngle (robotBase r))
        rotationAction = Action r (ROTATE_TURRET_ACTION angleDelta) --Apuntamos a enemigo
        actions =
          if abs angleDelta < fireTolerance then --Angulo casi perfecto para disparar
            [ rotationAction
            , Action r FIRE_ACTION --Gira torreta y dispara
            , Action r (ROTATE_ROBOT_ACTION bodyAngleDelta) --Gira cuerpo hacia enemigo para acercarse
            , Action r MOVE_FORWARD_ACTION
            ]
          else --Sino igual pero sin disparar
            [ rotationAction
            , Action r (ROTATE_ROBOT_ACTION bodyAngleDelta)
            , Action r MOVE_FORWARD_ACTION
            ]
      in actions
    Nothing -> wanderActions r gs

-- 2. BALANCEADO: Dispara pero mantiene la posición cuando encuentra enemigo
balancedAI :: Robot -> GameState -> [Action] 
balancedAI r gs =
  case findClosestEnemy r gs of
    Just target ->
      let
        targetPos = objPos $ robotBase target
        myPos = objPos $ robotBase r
        targetAngle = angleToTarget myPos targetPos
        currentTurretAngle = turretAngle r
        angleDelta = normalizeAngle (targetAngle - currentTurretAngle)
        rotationAction = Action r (ROTATE_TURRET_ACTION angleDelta)
        actions =
          if abs angleDelta < fireTolerance then
            [ rotationAction, Action r FIRE_ACTION ] --Gira torreta pero no se mueve hacia enemigo ni gira su cuerpo
          else
            [ rotationAction ]
      in actions
    Nothing -> wanderActions r gs

-- 3. DEFENSIVO: Dispara y va retrocediendo cuando encuentra enemigos
defensiveAI :: Robot -> GameState -> [Action]
defensiveAI r gs =
  case findClosestEnemy r gs of
    Just target ->
      let
        targetPos = objPos $ robotBase target
        myPos = objPos $ robotBase r
        dist = distance myPos targetPos
        targetAngle = angleToTarget myPos targetPos
        currentTurretAngle = turretAngle r
        angleDelta = normalizeAngle (targetAngle - currentTurretAngle)
        rotationAction = Action r (ROTATE_TURRET_ACTION angleDelta)
        actions =
          if dist < radarLength r * 0.5 then
            if abs angleDelta < fireTolerance then
              [ rotationAction, Action r FIRE_ACTION, Action r MOVE_BACKWARD_ACTION ]
            else
              [ rotationAction, Action r MOVE_BACKWARD_ACTION, Action r (ROTATE_ROBOT_ACTION (pi / 8)) ] -- Gira un poco mientras se aleja
          else
            [ rotationAction ]
      in actions
    Nothing -> wanderActions r gs

-- 4. PACÍFICO: Se limita a explorar 
peacefulAI :: Robot -> GameState -> [Action]
peacefulAI r gs = wanderActions r gs

-- --- Funciones Auxiliares ---

-- Normaliza un ángulo entre -pi y pi
normalizeAngle :: Angle -> Angle
normalizeAngle ang = atan2 (sin ang) (cos ang)