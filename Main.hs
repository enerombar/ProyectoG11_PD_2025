module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game -- Para la función 'play'
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Juicy (loadJuicy) -- Función para cargar imágenes desde archivos

-- Importamos de forma selectiva para evitar ambigüedades
import Entities
import Math (rad2deg)
import Game
import Logic -- Importa todas las funciones de lógica del bucle: applyActions, updatePhysics, resolveCollisions, etc.
import Physics (checkCollisions) -- Solo necesitamos checkCollisions de Physics

--- Configuración de la Ventana ---

(gameSideF, _) = unV2 gameSize -- Asumimos mundo cuadrado
gameSideI :: Int
gameSideI = round gameSideF

fps :: Int
fps = 60

window :: Display
window = InWindow "Torneo de Robots" (gameSideI, gameSideI) (100, 100)

backgroundColor :: Color
backgroundColor = black

-- --- Punto de Entrada: Main ---

main :: IO ()
main = do
  -- Cargamos las imágenes (hay tener la carpeta 'assets' con las imágenes)
  maybeBgImage <- loadJuicy "assets/mapa.jpg" --Como no es seguro que se puedan cargar son tipo Maybe
  maybeChasisImage <- loadJuicy "assets/chasis.png"
  maybeTurretImage <- loadJuicy "assets/torreta.png"

  let chasisImgWidth = 700.0
  let chasisImgHeight = 300.0
  let turretImgWidth = 600.0
  let turretImgHeight = 150.0
  
  -- Comprobamos con fromMaybe que las imágenes se hayan cargado bien (sean Just)
  -- Sino usamos fondo blanco o bien rectángulos de color como sustitutos
  let bgImage = fromMaybe (Color white $ rectangleSolid gameSideF gameSideF) maybeBgImage
  let chasisImage = fromMaybe (Color black $ rectangleSolid chasisImgWidth chasisImgHeight) maybeChasisImage
  let turretImage = fromMaybe (Color chartreuse $ rectangleSolid turretImgWidth turretImgHeight) maybeTurretImage

  -- Iniciamos el juego
  play
    window
    backgroundColor
    fps
    exampleGameState
    (drawHandler bgImage chasisImage turretImage)
    eventHandler -- Inutilizado. No hay interacción exterior ni teclado, ni ratón,...
    updateHandler

--- 2. Lógica del Torneo (Bucle Principal) ---

updateHandler :: Float -> GameState -> GameState --Funcion para actualizar juego. Se llama "fps veces" por seg por Gloss
updateHandler dt gs =
  let
    -- 1. Disminuir cooldowns de robots y gestionar vida de explosiones
    gsCooldowns = decreaseCooldown gs

    -- 2. Obtener acciones de los bots VIVOS (IA.hs filtra internamente)
    allActions = exampleBotActions gsCooldowns

    -- 3. Aplicar acciones (movimiento, fuego condicional)
    gsAfterActions = applyActions allActions gsCooldowns

    -- 4. Actualizar posiciones (física)
    gsAfterPhysics = updatePhysics dt gsAfterActions

    -- 5. Comprobar colisiones
    collisions = checkCollisions gsAfterPhysics

    -- 6. Aplicar efectos de colisión y limpiar proyectiles
    gsAfterDamage = resolveCollisions collisions gsAfterPhysics

    -- 7. Separar robots vivos de muertos 
    livingRobots = filter isRobotAlive (robots gsAfterDamage)
    deadRobots   = filter (not . isRobotAlive) (robots gsAfterDamage)
    
    -- 8. Crear explosiones para los recién muertos 
    newExplosions = map createExplosionFromRobot deadRobots

    -- 9. Construir el estado final
    gsFinal = gsAfterDamage
      { robots = livingRobots -- <-- Solo los vivos pasan a la siguiente iteración
      , explosions = newExplosions ++ explosions gsAfterDamage -- <-- Añade las nuevas explosiones
      }

  in gsFinal


--- 1. Lógica de Renderizado ---
drawHandler :: Picture -> Picture -> Picture -> GameState -> Picture
drawHandler bgImage chasisImage turretImage gs =
  -- Centramos el mundo del juego en la ventana
  Translate (- (gameSideF / 2)) (- (gameSideF / 2)) $
    Pictures $
      [ Translate (gameSideF / 2) (gameSideF / 2) bgImage ] ++ robotPics ++ projPics ++ explosionPics
  where
    robotPics = map (drawRobot chasisImage turretImage) (robots gs)
    projPics  = map drawProjectile (projectiles gs)
    explosionPics = map drawExplosion (explosions gs)


drawRobot :: Picture -> Picture -> Robot -> Picture
drawRobot chasisImage turretImage r =
  -- Si el temporizador es > 0 y es un frame par, parpadea en blanco: Animacion de daño 
  if (robotHitTimer r > 0) && (robotHitTimer r `mod` 8 < 4)
    then Pictures [whiteBody, whiteTurret, healthBar, nameTag] 
    else Pictures [body, turret, healthBar, nameTag]     
  where
    base = robotBase r
    (x, y) = unV2 (objPos base)
    angleDeg = negate $ rad2deg (objAngle base)
    turretAngleDeg = negate $ rad2deg (turretAngle r)

    -- --- Dibujo del Chasis ---
    (w, h) = unV2 (objSize base) --

    chasisImgWidth = 700.0
    chasisImgHeight = 300.0
    body = Translate x y $ Rotate angleDeg $ Scale (w / chasisImgWidth) (h / chasisImgHeight) chasisImage

    whiteBody = Translate x y $ Rotate angleDeg $ Color white $ rectangleSolid w h

    -- --- Dibujo de la Torreta ---
    (turretWidth, turretHeight) = unV2 (getTurretSize (robotType r))
    turretImgWidth = 600.0
    turretImgHeight = 150.0
    turret = Translate x y $ Rotate turretAngleDeg $ Rotate 180 $ Scale (turretWidth / turretImgWidth) (turretHeight / turretImgHeight) turretImage

    whiteTurret = Translate x y $ Rotate turretAngleDeg $ Rotate 180 $ Color white $ rectangleSolid turretWidth turretHeight

    -- --- Barra de Vida ---
    health = robotHealth r
    maxHealth = robotMaxHealth r 
    healthRatio = health / maxHealth

    healthColor
      | healthRatio > 0.6 = green
      | healthRatio > 0.2 = orange
      | otherwise = red

    healthWidth = healthRatio * (w * 0.8 - 2)

    healthBar = Translate x (y + h / 2 + 10) $ Pictures
      [ Color black $ rectangleWire (w * 0.8) 8
      , Color healthColor $ Translate ((- (w * 0.4)) + healthWidth / 2) 0 $ rectangleSolid healthWidth 6
      ]

    textString = robotName r ++ " (" ++ show (robotType r) ++ ")"
    horizontalOffset = - (fromIntegral (length textString) * 4.0)

    -- --- Etiqueta del Nombre ---
    nameTag = Translate x (y + h / 2 + 22) $ -- Posición (encima de la barra de vida)
              Translate horizontalOffset 0 $
              Scale 0.1 0.1 $                 -- Tamaño (pequeño)
              Color white $                   
              Text textString -- El texto del nombre

drawProjectile :: Projectile -> Picture
drawProjectile p = Translate x y $ Color black $ circleSolid radius
  where
    base = projBase p
    (x, y) = unV2 (objPos base)
    (radius, _) = unV2 (objSize base)

drawExplosion :: Explosion -> Picture
drawExplosion e = Translate x y $ Color orange $ circleSolid radius
  where
    (x, y) = unV2 (objPos (explBase e))
    radius = explRadius e

-- 4. Manejador de Eventos --
eventHandler :: Event -> GameState -> GameState
eventHandler _ gs = gs