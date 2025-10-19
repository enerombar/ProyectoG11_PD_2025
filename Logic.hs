module Logic where

import Entities
import Math
import Game

import Data.Ord (comparing)
import Data.List (minimumBy)
import Data.Maybe

-- Constantes
speedInc :: Double
speedInc = 60.0

speedDec :: Double
speedDec = 0.1

-- Lógica de detección y estado 
detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 = distance (objPos $ robotBase r1) (objPos $ robotBase r2) <= radarLength r1

isRobotAlive :: Robot -> Bool
isRobotAlive r = robotHealth r > 0

countActiveRobots :: GameState -> Int
countActiveRobots gs = length [ r | r <- robots gs, isRobotAlive r ]

updateRobotVelocity :: Vector -> Robot -> Robot
updateRobotVelocity v r = r { robotBase = (robotBase r) { objVel = v } }

-- Actualiza la velocidad usando Functores
updateVelocity :: Robot -> Action -> Robot
updateVelocity r act =
  case act of
    MOVE_FORWARD _ ->
      let dir = dirFromAngle angle
          vel = fmap (* speedInc) dir -- escala ambos ejes
      in  updateRobotVelocity vel r
    MOVE_BACKWARD _ ->
      let dir = dirFromAngle angle
          vel = fmap (* (-speedInc)) dir -- escala con signo contrario
      in  updateRobotVelocity vel r
    STOP _ ->
      let currentVel = objVel (robotBase r)
          newVel = fmap (* speedDec) currentVel -- desaceleración
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
    base     = robotBase r
    newAng   = objAngle base + d
    pos      = objPos base
    newVerts = createRectangleRobot (robotType r) pos newAng

-- Aplicar acción
applyAction :: Robot -> Action -> Robot
applyAction r act =
  case act of
    STOP _            -> updateVelocity r act
    MOVE_FORWARD _    -> updateVelocity r act
    MOVE_BACKWARD _   -> updateVelocity r act
    ROTATE_TURRET _ d -> rotateTurretBy r d
    ROTATE_ROBOT _ d  -> rotateRobotBy r d
    _                 -> r -- Disparo se implementará más adelante

-- Actualiza las velocidades y rotación de todos los robots según las acciones
applyAllRobotActionsFmap :: [Action] -> GameState -> GameState
applyAllRobotActionsFmap acts gs = gs { robots = fmap (uncurry applyAction) (zip (robots gs) acts) }
-- uncurry applyAction crea una nueva función que espera tuplas (Robot, Action) en vez de dos parámetros Robot y Action


-- Actualiza la posición
updatePosition :: Robot -> Double -> Maybe Robot
updatePosition r dt
    -- Usamos la constante global 'gameSize' del módulo Game, como en la versión original de Ángel.
    | all (\p -> isInBounds p (v2 0 0) gameSize) translatedVerts = --v2 en minúsculas constructor de nuestro newtype V2
        Just r { robotBase = (robotBase r) { objPos = newPos' }, robotVerts = translatedVerts }
    | otherwise = Nothing -- Si se sale de los límites => Nothing
  where
    v = objVel $ robotBase r
    translatedVerts = translatePoints (robotVerts r) (fmap (* dt) v) --fmap para escalar el vector velocidad 
    newPos' = fromMaybe (objPos (robotBase r)) (center translatedVerts) --Si la traslación falla tomamos la antigua posición

-- Comportamiento de los bots 
exampleBotActions :: GameState -> [Action]
exampleBotActions gs =
    let activeRobots = filter isRobotAlive (robots gs) --Filtramos solo los robots vivos
    in concatMap (`actionsForRobot` gs) activeRobots --Aplicamos actionsForRobot a cada robot vivo

-- Decide la acción para un robot en función de si tiene un enemigo cerca.
actionsForRobot :: Robot -> GameState -> [Action]
actionsForRobot r gs =
  case findClosestEnemy r gs of -- Si encontramos algún
    Just target ->
      let targetAngle = angleToTarget (objPos $ robotBase r) (objPos $ robotBase target)
      in [ROTATE_TURRET r (targetAngle - turretAngle r), FIRE r]
    Nothing ->     -- Si no hay objetivos cerca avanza
      [MOVE_FORWARD r] 

-- Encuentra el robot enemigo más cercano
findClosestEnemy :: Robot -> GameState -> Maybe Robot
findClosestEnemy self gs =
  let myId = objId (robotBase self)
      myPos = objPos (robotBase self)
      -- Filtra para obtener solo a los enemigos vivos que están en el rango del radar
      enemies = filter (\r -> objId (robotBase r) /= myId && isRobotAlive r && detectedAgent self r) (robots gs)
  in
    case enemies of
      [] -> Nothing -- No enemigos cerca
      lista -> Just $ minimumBy (comparing (distance myPos . objPos . robotBase)) lista
