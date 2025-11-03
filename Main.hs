module Main where

-- Imports de Librerías
import Graphics.Gloss hiding (Text)
import Graphics.Gloss (Picture(..), Color, Display(..), Point, Vector, Path, Display(..), pictures, translate, rotate, scale, color, blank, line, polygon, circleSolid, thickCircle, rectangleSolid, rectangleWire, circle)
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..), KeyState(..), Modifiers(..), MouseButton(..), play)
import Graphics.Gloss.Juicy (loadJuicy)
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitSuccess)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.List (findIndex)
import Data.Char (toLower)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Imports Locales
import Entities
import Math (rad2deg, angleToTarget, distance)
import Game
import Logic -- Importa applyActions, updatePhysics, resolveCollisions, etc.
import Physics (checkCollisions)
import IA (getAIActions)

-------------------------------------------------------------------------------
-- Configuración de la Ventana
-------------------------------------------------------------------------------

availableResolutions :: [(Float, Float)]
availableResolutions = [(1280, 720), (900, 600)]

defaultResolution :: (Float, Float)
defaultResolution = head availableResolutions

fps :: Int
fps = 60

window :: Display
window = InWindow "Torneo de Drones" (round $ fst defaultResolution, round $ snd defaultResolution) (100, 100)

backgroundColor :: Color
backgroundColor = black

-------------------------------------------------------------------------------
-- Constantes de Dimensiones de Assets
-------------------------------------------------------------------------------

-- Dimensiones originales de las imágenes de assets (para escalar)
playerChasisImgWidth, playerChasisImgHeight :: Float
playerChasisImgWidth  = 317.0
playerChasisImgHeight = 332.0

aiChasisImgWidth, aiChasisImgHeight :: Float
aiChasisImgWidth  = 1024.0
aiChasisImgHeight = 1050.0

turretImgWidth, turretImgHeight :: Float
turretImgWidth  = 600.0
turretImgHeight = 150.0

bulletImgWidth, bulletImgHeight :: Float
bulletImgWidth  = 172.0
bulletImgHeight = 512.0

explosionFrameWidth, explosionFrameHeight :: Float
explosionFrameWidth  = 300.0
explosionFrameHeight = 300.0

bgWidth, bgHeight :: Float
bgWidth  = 3480.0
bgHeight = 2160.0

-------------------------------------------------------------------------------
-- Estado de la Aplicación
-------------------------------------------------------------------------------

data MenuMode = MenuHome | MenuConfig deriving (Show, Eq)

data MenuState = MenuState
  { msConfigs :: [TankConfig]
  , msMode    :: MenuMode
  , msWidth  :: Float
  , msHeight :: Float
  , msPowerUpsEnabled :: Bool
  , msShowHitboxes :: Bool
  } deriving (Show, Eq)

data AppState
  = InMenu MenuState
  | InGame GameState
  | GameOver (Maybe String) -- El String contiene el nombre del ganador
  deriving (Show)

-- Registro para agrupar todas las imágenes cargadas
data GameAssets = GameAssets
  { aBgImage             :: Picture
  , aPlayerChasisImage   :: Picture
  , aAIChasisImage       :: Picture
  , aPlayerChasisHitImage:: Picture
  , aAIChasisHitImage    :: Picture
  , aPlayerTurretImage   :: Picture
  , aAITurretImage       :: Picture
  , aTurretHitImage      :: Picture
  , aBulletImage         :: Picture
  , aExplosionFrames     :: [Picture]
  , aPowerUpPics         :: Map.Map PowerUpType Picture
  , aObstacleWallPic     :: Picture
  , aObstacleDamageRectPic :: Picture
  , aObstacleDamagePic   :: Picture 
  , aMinaInactivaPic :: Picture
  , aMinaActivaPic :: Picture
  , aObstacleTeslaPic :: Picture
  , aObstacleStormPic :: Picture
  , aBlueChasisOverlay :: Picture
  , aTurretBlueOverlay :: Picture
  }

-- Estado inicial del menú
initialMenuState :: MenuState
initialMenuState = MenuState
  { msConfigs = [ TankConfig HEAVY AGGRESSIVE
                , TankConfig MEDIUM BALANCED
                ]
  , msMode = MenuHome
  , msWidth  = fst defaultResolution
  , msHeight = snd defaultResolution
  , msPowerUpsEnabled = True
  , msShowHitboxes = False
  }

-------------------------------------------------------------------------------
-- Punto de Entrada: Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  let explosionFrameNames = map (\n -> "assets/explosion/expl_" ++ show n ++ ".png") [0..6]
  let (defW, defH) = defaultResolution

  -- Carga de Assets
  maybeBgImage             <- loadJuicy "assets/mapa.jpg"
  maybePlayerChasisImage   <- loadJuicy "assets/drones/dron.png"
  maybeAIChasisImage       <- loadJuicy "assets/drones/dron_robot.png"
  maybePlayerChasisHitImage<- loadJuicy "assets/drones/dron_blanco.png"
  maybeAIChasisHitImage    <- loadJuicy "assets/drones/dron_robot_blanco.png"
  maybePlayerTurretImage   <- loadJuicy "assets/torretas/torreta.png"
  maybeAITurretImage       <- loadJuicy "assets/torretas/torreta_robot.png"
  maybeTurretHitImage      <- loadJuicy "assets/torretas/torreta_blanca.png"
  maybeTurretBlueImage     <- loadJuicy "assets/torretas/torreta_azul.png"
  maybeBulletImage         <- loadJuicy "assets/items/bullet.png"
  maybeExplosionFrames     <- mapM loadJuicy explosionFrameNames
  maybeHealthPU            <- loadJuicy "assets/items/health_powerup.png"
  maybeAmmoPU              <- loadJuicy "assets/items/bullet_powerup.png"
  maybeSpeedPU             <- loadJuicy "assets/items/speed_powerup.png"
  maybeShieldPU            <- loadJuicy "assets/items/shield_powerup.png"
  maybeObstacleWall        <- loadJuicy "assets/obstaculos/wall.png"
  maybeObstacleDamageRect  <- loadJuicy "assets/obstaculos/damage_zone_rect.png"
  maybeObstacleDamage      <- loadJuicy "assets/obstaculos/damage_zone.png"
  maybeMinaInactiva        <- loadJuicy "assets/obstaculos/mina_inactiva.png"
  maybeMinaActiva          <- loadJuicy "assets/obstaculos/mina_activa.png"
  maybeObstacleTesla       <- loadJuicy "assets/obstaculos/torre_tesla.png"
  maybeObstacleStorm       <- loadJuicy "assets/obstaculos/tormenta.png"
  maybeBlueChasis          <- loadJuicy "assets/drones/dron_azul.png"


  -- Imágenes de fallback
  let bgImage = fromMaybe (Color white $ rectangleSolid defW defH) maybeBgImage
  let playerChasisImage = fromMaybe (Color blue $ rectangleSolid playerChasisImgWidth playerChasisImgHeight) maybePlayerChasisImage
  let aiChasisImage = fromMaybe (Color red $ rectangleSolid aiChasisImgWidth aiChasisImgHeight) maybeAIChasisImage
  let playerChasisHitImage = fromMaybe (Color white $ rectangleSolid playerChasisImgWidth playerChasisImgHeight) maybePlayerChasisHitImage
  let aiChasisHitImage = fromMaybe (Color white $ rectangleSolid aiChasisImgWidth aiChasisImgHeight) maybeAIChasisHitImage
  let playerTurretImage = fromMaybe (Color green $ rectangleSolid turretImgWidth turretImgHeight) maybePlayerTurretImage
  let aiTurretImage = fromMaybe (Color yellow $ rectangleSolid turretImgWidth turretImgHeight) maybeAITurretImage
  let turretHitImage = fromMaybe (Color white $ rectangleSolid turretImgWidth turretImgHeight) maybeTurretHitImage
  let turretBlueOverlay = fromMaybe (Color (withAlpha 0.5 blue) $ rectangleSolid turretImgWidth turretImgHeight) maybeTurretBlueImage
  let bulletImage = fromMaybe (Color yellow $ rectangleSolid bulletImgWidth bulletImgHeight) maybeBulletImage
  let explosionFrames = map (fromMaybe Blank) maybeExplosionFrames
  let healthPUPic = fromMaybe (Color green $ circleSolid 20) maybeHealthPU
  let ammoPUPic   = fromMaybe (Color yellow $ rectangleSolid 30 30) maybeAmmoPU
  let speedPUPic  = fromMaybe (Color cyan $ ThickCircle 15 5) maybeSpeedPU
  let shieldPUPic = fromMaybe (Color blue $ ThickCircle 15 5) maybeShieldPU
  let obstacleWallPic   = fromMaybe (Color (greyN 0.5) $ rectangleSolid 100 100) maybeObstacleWall
  let obstacleDamagePic = fromMaybe (Color (dark magenta) $ circleSolid 50) maybeObstacleDamage
  let obstacleDamageRectPic = fromMaybe (Color (dark red) $ rectangleWire 100 100) maybeObstacleDamageRect
  let minaInactivaPic = fromMaybe (Color (greyN 0.5) $ circleSolid 30) maybeMinaInactiva
  let minaActivaPic = fromMaybe (Color (dark red) $ circleSolid 30) maybeMinaActiva
  let obstacleTeslaPic = fromMaybe (Color (dim cyan) $ circleSolid 30) maybeObstacleTesla
  let obstacleStormPic = fromMaybe (Color (withAlpha 0.35 blue) $ circleSolid 30) maybeObstacleStorm
  let blueChasisOverlay = fromMaybe (Color (withAlpha 0.5 blue) $ rectangleSolid playerChasisImgWidth playerChasisImgHeight) maybeBlueChasis

  let powerUpPics = Map.fromList
        [ (Health, healthPUPic)
        , (AmmoBoost, ammoPUPic)
        , (SpeedBoost, speedPUPic)
        , (Shield, shieldPUPic)
        ]

  -- Agrupa todos los assets en un solo registro
  let assets = GameAssets
        { aBgImage              = bgImage
        , aPlayerChasisImage    = playerChasisImage
        , aAIChasisImage        = aiChasisImage
        , aPlayerChasisHitImage = playerChasisHitImage
        , aAIChasisHitImage     = aiChasisHitImage
        , aPlayerTurretImage    = playerTurretImage
        , aAITurretImage        = aiTurretImage
        , aTurretHitImage       = turretHitImage
        , aTurretBlueOverlay    = turretBlueOverlay
        , aBulletImage          = bulletImage
        , aExplosionFrames      = explosionFrames
        , aPowerUpPics          = powerUpPics
        , aObstacleWallPic      = obstacleWallPic
        , aObstacleDamagePic    = obstacleDamagePic
        , aObstacleDamageRectPic = obstacleDamageRectPic
        , aMinaInactivaPic = minaInactivaPic
        , aMinaActivaPic = minaActivaPic
        , aObstacleTeslaPic = obstacleTeslaPic
        , aObstacleStormPic = obstacleStormPic
        , aBlueChasisOverlay = blueChasisOverlay
        }

  let initialState = InMenu initialMenuState

  -- Iniciar el juego
  play
    window
    backgroundColor
    fps
    initialState
    (drawApp assets) -- Pasa el registro de assets unificado
    eventHandler
    updateHandler

-------------------------------------------------------------------------------
-- Lógica del Bucle Principal del Juego (Update)
-------------------------------------------------------------------------------

-- Esta función se ejecuta en cada fotograma
updateGame :: Float -> GameState -> GameState
updateGame dt gs =
  let
    -- 1. Reducir todos los cooldowns y temporizadores
    gsCooldowns = decreaseCooldown gs

    -- 1.5. Comprobar si alguna mina ha explotado
    gsMinesResolved = resolveExplodingMines gsCooldowns

    -- 2. Comprobar y potencialmente spawnear/reubicar un powerup
    --    (Usamos gsMinesResolved como base ahora)
    gsPowerUpSpawned = updatePowerUpSpawning gsMinesResolved

    -- 3. Aplicar acciones del jugador (movimiento, disparo)
    gsAfterPlayer = applyPlayerActionsFromState gsPowerUpSpawned

    -- 4. Obtener acciones de la IA
    aiActions = getAIActions gsAfterPlayer

    -- 5. Aplicar acciones de la IA
    gsAfterActions = applyActions aiActions gsAfterPlayer

    -- 5.5. Torres Tesla (ataques + ralentizacion)
    gsAfterTesla = updateTeslaTowers gsAfterActions

    -- 6. Actualizar físicas (mover objetos)
    gsAfterPhysics = updatePhysics dt gsAfterTesla

    -- 7. Comprobar TODAS las colisiones
    allCollisions = checkCollisions gsAfterPhysics

    -- 8. Resolver colisiones (daño, efectos, eliminar entidades, activar minas)
    gsAfterCollisions = resolveCollisions allCollisions gsAfterPhysics

    -- 9. Manejar muertes de robots y crear explosiones
    livingRobots = filter isRobotAlive (robots gsAfterCollisions)
    deadRobots   = filter (not . isRobotAlive) (robots gsAfterCollisions)
    newExplosions = map createExplosionFromRobot deadRobots

    -- 10. Estado final del fotograma
    gsFinal = gsAfterCollisions
      { robots = livingRobots
      , explosions = newExplosions ++ explosions gsAfterCollisions
      }
  in gsFinal


-------------------------------------------------------------------------------
-- Lógica de Renderizado (Dibujo)
-------------------------------------------------------------------------------

-- Dibuja el estado del juego (InGame)
drawGame :: GameAssets -> GameState -> Picture
drawGame assets gs =
  let (V2 (worldW, worldH)) = gameDimensions gs
      mapBorder = Color white $ Line [ (0, 0), (worldW, 0), (worldW, worldH), (0, worldH), (0, 0) ]
      powerUpPic = case powerUp gs of
          Nothing -> Blank
          Just pu -> drawPowerUp (aPowerUpPics assets) pu
      controlsPic = drawPlayerControls gs

      -- Dibujo de entidades
      robotPics     = map (drawRobot assets gs) (robots gs)
      projPics      = map drawProjectile (projectiles gs)
      explosionPics = map (drawExplosion (aExplosionFrames assets)) (explosions gs)
      obstaclePics  = map (drawObstacle assets gs) (obstacles gs)

  in Translate (-worldW / 2) (-worldH / 2) $
     Pictures $
       [ -- Fondo y borde
         Translate (worldW / 2) (worldH / 2) $
         Scale (worldW / bgWidth) (worldH / bgHeight) (aBgImage assets)
       , mapBorder 
       ]
       -- Entidades (Obstáculos primero, para que los robots pasen por encima visualmente)
       ++ obstaclePics
       ++ robotPics      
       ++ [drawTeslaBeams gs]
       ++ projPics      
       ++ explosionPics 
       ++ [powerUpPic]  
       
       -- UI (al final)
       ++ [controlsPic]

-- Dibuja la UI de controles del jugador
drawPlayerControls :: GameState -> Picture
drawPlayerControls gs =
  case findPlayerRobot gs of
    Nothing -> Blank -- Si no hay jugador, no dibuja nada
    Just _  ->
      let
        (V2 (worldW, worldH)) = gameDimensions gs
        margin = 20.0
        lineHeight = 18.0
        controls =
          [ "W, S: Mover Adelante/Atrás"
          , "A, D: Rotar Dron"
          , "Ratón: Apuntar Arma"
          , "Clic Izquierdo: Disparar"
          ]
        numLines = fromIntegral $ length controls
        boxWidth = 250.0
        boxHeight = numLines * lineHeight + margin
        boxPadding = 10.0

        textPics = zipWith (\lineText lineNum ->
                     Translate 0 (-lineNum * lineHeight) $
                     Scale 0.12 0.12 $
                     Color white $
                     Text lineText
                     ) controls [0..]

        -- Posición de la esquina inferior derecha del mundo
        bottomRightX = worldW + 20
        bottomRightY = 0
        boxCenterX = bottomRightX - margin - boxWidth / 2
        boxCenterY = bottomRightY + margin + boxHeight / 2
        textBlockX = -boxWidth / 2 + boxPadding
        textBlockY = boxHeight / 2 - margin / 2 - (lineHeight * 0.5)

      in
        Translate boxCenterX boxCenterY $
        Pictures
          [ Color (withAlpha 0.3 black) $ rectangleSolid boxWidth boxHeight
          , Translate textBlockX textBlockY $ Pictures textPics
          ]

-- Dibuja un robot individual, su UI y sus efectos
drawRobot :: GameAssets -> GameState -> Robot -> Picture
drawRobot assets gs r =
  let
    -- Propiedades base (sin cambios)
    base = robotBase r
    (x, y) = unV2 (objPos base)
    (w, h) = unV2 (objSize base)
    angleDeg = negate $ rad2deg (objAngle base)
    turretAngleDeg = negate $ rad2deg (turretAngle r)

    -- Selección de estado e imagen (sin cambios)
    isPlayer = robotBehavior r == PLAYER
    isHit = (robotHitTimer r > 0) && (robotHitTimer r `mod` 8 < 4)

    selectedChasisImage = case (isPlayer, isHit) of
      (True,  True)  -> aPlayerChasisHitImage assets
      (True,  False) -> aPlayerChasisImage assets
      (False, True)  -> aAIChasisHitImage assets
      (False, False) -> aAIChasisImage assets

    selectedTurretImage = if robotBehavior r == RAMMER then Blank else -- Los RAMMER no tienen torreta visual
      case (isPlayer, isHit) of
        (_,     True)  -> aTurretHitImage assets
        (True,  False) -> aPlayerTurretImage assets
        (False, False) -> aAITurretImage assets

    -- Dimensiones de la imagen (sin cambios)
    (imgW, imgH) = if isPlayer
                   then (playerChasisImgWidth, playerChasisImgHeight)
                   else (aiChasisImgWidth, aiChasisImgHeight)

    -- Dibujo del chasis (sin cambios)
    baseChasis = Scale (w / imgW) (h / imgH) selectedChasisImage
    rammerDecals = Pictures [Translate (w * 0.25) (h * 0.15) $ Rotate (-30) $ Color (dark red) $ rectangleSolid (w * 0.4) (h * 0.2)
        , Translate (w * 0.25) (h * (-0.15)) $ Rotate 30 $ Color (dark red) $ rectangleSolid (w * 0.4) (h * 0.2)]
    body = Translate x y $ Rotate angleDeg $
             if robotBehavior r == RAMMER
             then Pictures [ baseChasis, rammerDecals ]
             else baseChasis

    -- Overlay azul de ralentización
    slowActive =
      let isTesla = case Map.lookup "teslaSlow" (robotMem r) of
                      Just (MInt v) | v > 0 -> True
                      _ -> False
          isStorm = case Map.lookup "stormSlow" (robotMem r) of
                      Just (MInt v) | v > 0 -> True
                      _ -> False
      in isTesla || isStorm
    blueOverlayScaled = Scale (w / playerChasisImgWidth) (h / playerChasisImgHeight) (aBlueChasisOverlay assets)
    blueOverlay = if slowActive
                    then Translate x y $ Rotate angleDeg $ Color (withAlpha 0.45 white) blueOverlayScaled
                    else Blank

    -- Dibujo de la torreta (sin cambios)
    (turretWidth, turretHeight) = unV2 (getTurretSize (robotType r))
    turret = Translate x y $ Rotate turretAngleDeg $ Rotate 180 $
             Scale (turretWidth / turretImgWidth) (turretHeight / turretImgHeight) $
             selectedTurretImage

    -- Overlay azul para la torreta cuando está ralentizado
    turretBlueOverlayPic = if slowActive
                              then Translate x y $ Rotate turretAngleDeg $ Rotate 180 $
                                   Color (withAlpha 0.45 white) $
                                   Scale (turretWidth / turretImgWidth) (turretHeight / turretImgHeight) $
                                   aTurretBlueOverlay assets
                              else Blank

    -- --- NUEVO: Dibujo de la Hitbox ---
    hitboxVerts = robotVerts r -- Obtener los vértices de la hitbox
    hitboxPoints = map unV2 hitboxVerts -- Convertir V2 Float a (Float, Float)
    -- Dibuja líneas conectando los puntos, cerrando el polígono
    hitboxPic = if gsShowHitboxes gs then -- <<< USA EL FLAG
                  Color yellow $ Line (hitboxPoints ++ [head hitboxPoints])
                else
                  Blank

    -- Helpers (sin cambios)
    effects = drawRobotEffects r
    ui = drawRobotUI (aBulletImage assets) r

  -- Añadimos 'hitboxPic' al final para que se dibuje encima
  in Pictures [body, blueOverlay, turret, turretBlueOverlayPic, effects, ui, hitboxPic]

-- Dibuja los efectos visuales (escudo, velocidad) de un robot
drawRobotEffects :: Robot -> Picture
drawRobotEffects r =
  let
    base = robotBase r
    (x, y) = unV2 (objPos base)
    (w, h) = unV2 (objSize base)
    angleDeg = negate $ rad2deg (objAngle base)

    shieldActive = robotShieldTimer r > 0
    speedActive = robotSpeedBoostTimer r > 0
    slowActive = case Map.lookup "teslaSlow" (robotMem r) of
                   Just (MInt v) | v > 0 -> True
                   _ -> False

    shieldOverlay = if shieldActive
                      then Color (withAlpha 0.4 blue) $ circle (max w h * 0.7)
                      else Blank

    speedOverlay = if speedActive
                     then Color (withAlpha 0.6 cyan) $ Pictures
                          [ Translate (-w * 0.6) (h * 0.1) $ rectangleSolid (w * 0.3) (h * 0.1)
                          , Translate (-w * 0.6) (-h * 0.1) $ rectangleSolid (w * 0.3) (h * 0.1)
                          ]
                     else Blank

  in Translate x y $ Rotate angleDeg $ Pictures [shieldOverlay, speedOverlay]

-- Dibuja la UI (vida, munición, nombre) de un robot
drawRobotUI :: Picture -> Robot -> Picture
drawRobotUI bulletImage r =
  let
    base = robotBase r
    (x, y) = unV2 (objPos base)
    (w, h) = unV2 (objSize base)

    -- Barra de Vida
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

    -- Iconos de Munición o Recarga
    ammoIconsDisplay =
      let
        posY = y + h / 2 + 22
        drawBulletIcons =
          let
            iconHeight = 15.0
            iconScale  = iconHeight / bulletImgHeight
            iconWidth  = bulletImgWidth * iconScale
            spacing = iconWidth + 2.0
            numBullets = robotAmmo r
            totalWidth = fromIntegral numBullets * spacing
            startX = - (totalWidth / 2) + (spacing / 2)
          in
            Pictures
              [ Translate (startX + spacing * fromIntegral i) 0 $
                Scale iconScale iconScale bulletImage
              | i <- [0 .. numBullets - 1]
              ]
        drawText t c =
          let horizontalOffset = - (fromIntegral (length t) * 3.5)
          in Translate horizontalOffset 0 $ Scale 0.1 0.1 $ Color c $ Text t
        drawReloadAnimation =
          let
            totalReloadTime = fromIntegral $ getReloadTime (robotType r)
            currentReloadTime = fromIntegral $ robotReloadTimer r
            progress = (totalReloadTime - currentReloadTime) / totalReloadTime
            barTotalWidth = w * 0.5
            barHeight = 6.0
            barFillWidth = progress * (barTotalWidth - 2)
            fillOffset = (- (barTotalWidth / 2)) + 1 + (barFillWidth / 2)
          in
            Pictures
              [ Color (greyN 0.5) $ rectangleWire barTotalWidth barHeight
              , Color yellow $
                Translate fillOffset 0 $
                rectangleSolid barFillWidth (barHeight - 2)
              ]
      in
        Translate x posY $
          if robotReloadTimer r > 0 then
             drawReloadAnimation
          else
             if robotAmmo r <= 0 then
               drawText "(!)" red
             else
               drawBulletIcons

    -- Etiqueta de Nombre
    textString = robotName r
    horizontalOffset = - (fromIntegral (length textString) * 4.0)
    nameTag = Translate x (y + h / 2 + 34) $
              Translate horizontalOffset 0 $
              Scale 0.1 0.1 $
              Color white $
              Text textString

  in Pictures [ healthBar, nameTag, if robotBehavior r /= RAMMER then ammoIconsDisplay else Blank ]

-- Dibuja un proyectil usando su imagen
drawProjectile :: Projectile -> Picture
drawProjectile p = Translate x y $ Color black $ circleSolid radius
  where
    base = projBase p
    (x, y) = unV2 (objPos base)
    (radius, _) = unV2 (objSize base) -- Usa el tamaño del objeto como radio

-- Dibuja un power-up
drawPowerUp :: Map.Map PowerUpType Picture -> PowerUp -> Picture
drawPowerUp pics pu =
  let (V2 (x, y)) = puPos pu
      pic = fromMaybe Blank (Map.lookup (puType pu) pics)
      -- Animación sutil de pulsación
      pulseFactor = 1.0 + 0.1 * sin (fromIntegral (puTimer pu) / 10.0)
  in Translate x y $ Scale pulseFactor pulseFactor pic

-- Dibuja una explosión
drawExplosion :: [Picture] -> Explosion -> Picture
drawExplosion explosionFrames e =
  Translate x y $
  Scale scaleFactor scaleFactor $
  frameToDraw

  where
    (x, y) = unV2 (objPos (explBase e))
    (radius, _) = unV2 (objSize (explBase e))
    totalFrames = length explosionFrames

    -- Calcula el índice del fotograma (de 0 a 6) basado en el tiempo restante
    elapsedTime = fromIntegral (explosionDuration - explTime e)
    totalDuration = fromIntegral explosionDuration
    frameIndex = min (totalFrames - 1) (floor (elapsedTime / totalDuration * fromIntegral totalFrames))

    frameToDraw = explosionFrames !! frameIndex

    -- Calcula la escala para que la explosión coincida con el tamaño del robot
    frameH_enPantalla = radius * 2.0
    scaleFactor = frameH_enPantalla / explosionFrameHeight

-- Dibuja un obstáculo
drawObstacle :: GameAssets -> GameState -> Obstacle -> Picture
drawObstacle assets gs o =
  let (V2 (x, y)) = obsPos o
      (V2 (w, h)) = obsSize o

      -- Dimensiones originales de las imágenes
      wallImgW = 786.0
      wallImgH = 257.0
      damageRectImgW = 648.0
      damageRectImgH = 548.0
      damageImgW = 579.0 -- (Circular)
      damageImgH = 634.0
      minaImgSize = 512.0

      (obstacleSprite, hitboxShape) = case obsType o of
          WALL ->
            let 
                wallPic = aObstacleWallPic assets
                -- Lógica de rotación y escalado
                sprite = if h > w then
                           let scaleX = w / wallImgH
                               scaleY = h / wallImgW
                           in Translate x y $ Rotate 90 $ Scale scaleX scaleY wallPic
                         else
                           let scaleX = w / wallImgW
                               scaleY = h / wallImgH
                           in Translate x y $ Scale scaleX scaleY wallPic
                
                -- Hitbox rectangular
                hitboxVerts = obsVerts o
                hitboxPoints = map unV2 hitboxVerts
                shape = Line (hitboxPoints ++ [head hitboxPoints])
            in (sprite, Color yellow shape)

          DAMAGE_ZONE_RECT ->
            let
                damageRectPic = aObstacleDamageRectPic assets
                -- Escalar para ajustar al tamaño (w, h) basado en las dimensiones de la imagen
                scaleX = w / damageRectImgW
                scaleY = h / damageRectImgH
                sprite = Translate x y $ Scale scaleX scaleY damageRectPic
                
                -- Hitbox rectangular (igual que WALL)
                hitboxVerts = obsVerts o
                hitboxPoints = map unV2 hitboxVerts
                shape = Line (hitboxPoints ++ [head hitboxPoints])
            in (sprite, Color (dark orange) shape)

          -- Zona de daño circular (imagen)
          DAMAGE_ZONE ->
            let 
                damagePic = aObstacleDamagePic assets
                -- Para el círculo, w y h son iguales (ratio 1.0)
                -- Escalamos la imagen para que quepa en el diámetro (w)
                scale = w / (max damageImgW damageImgH) -- Escala uniforme
                sprite = Translate x y $ Scale scale scale damagePic
                
                radius = w * 0.5
                shape = thickCircle radius 1.5
            in (sprite, Translate x y $ Color (dark magenta) shape)

          -- Zona de tormenta
          STORM_ZONE ->
            let
                stormImgSize = 512.0
                scale = w / stormImgSize
                sprite = Translate x y $ Scale scale scale (aObstacleStormPic assets)
                radius  = w * 0.5
                shape   = thickCircle radius 1.5
            in (sprite, Translate x y $ Color blue shape)

          MINA_INACTIVA ->
            let
                scale = w / minaImgSize -- 'w' es el diámetro (W=H para círculos)
                sprite = Translate x y $ Scale scale scale (aMinaInactivaPic assets)
                radius = w * 0.5
                shape = thickCircle radius 1.5
            in (sprite, Translate x y $ Color yellow shape) -- Hitbox amarilla

          MINA_ACTIVA c ->
            let
                pic = if c `mod` 20 < 10 -- Parpadea cada 10 frames
                      then aMinaActivaPic assets
                      else Blank -- Desaparece
                scale = w / minaImgSize
                sprite = Translate x y $ Scale scale scale pic
                radius = w * 0.5
                shape = thickCircle radius 1.5
            in (sprite, Translate x y $ Color red shape) -- Hitbox roja

          -- Torre Tesla. El rango solo se muestra cuando gsShowHitboxes = True (va en hitboxShape)
          TORRE_TESLA{ teslaRange = rng } ->
            let
                teslaImgSize = 512.0
                scale = w / teslaImgSize
                sprite = Translate x y $ Scale scale scale (aObstacleTeslaPic assets)
                radius = w * 0.5
                hitShape = Color cyan (thickCircle radius 1.5)
                rangeShape = Color (withAlpha 0.25 cyan) (thickCircle rng 2)
                combined = Pictures [Translate x y hitShape, Translate x y rangeShape]
            in (sprite, combined)

      hitboxPic = if gsShowHitboxes gs then hitboxShape else Blank

  -- Dibuja el sprite primero, luego el hitbox encima
  in Pictures [obstacleSprite, hitboxPic]

-- Dibuja los rayos azules de las torres Tesla hacia los robots afectados
drawTeslaBeams :: GameState -> Picture
drawTeslaBeams gs = Pictures (mapMaybe beamForTower (obstacles gs))
  where
    beamForTower :: Obstacle -> Maybe Picture
    beamForTower o = case obsType o of
      TORRE_TESLA{ teslaRange = rng } ->
        let pT@(V2 (tx,ty)) = obsPos o
            slowedRobots = [ r | r <- robots gs
                             , isRobotAlive r
                             , let m = Map.lookup "teslaSlow" (robotMem r)
                             , case m of { Just (MInt v) -> v > 0; _ -> False }
                             , distance (objPos $ robotBase r) pT < rng ]
        in case nearestTo pT slowedRobots of
             Just r -> let (V2 (rx,ry)) = objPos (robotBase r)
                       in Just $ Color (withAlpha 0.95 blue) $ Line [(tx,ty),(rx,ry)]
             Nothing -> Nothing
      _ -> Nothing

    nearestTo _ [] = Nothing
    nearestTo p (r:rs) = Just (foldl (closer p) r rs)
    closer p a b = if distance (objPos $ robotBase a) p <= distance (objPos $ robotBase b) p then a else b

-------------------------------------------------------------------------------
-- Manejador de Eventos (Input)
-------------------------------------------------------------------------------

-- Convierte coordenadas de ventana Gloss (centro 0,0) a mundo (esquina sup-izq 0,0)
windowToWorld :: GameState -> (Float, Float) -> (Float, Float)
windowToWorld gs (mx, my) = (worldX, worldY)
  where
    (V2 (w, h)) = gameDimensions gs
    worldX = mx + w / 2
    worldY = my + h / 2

-- Manejador de eventos específico para 'InGame'
eventHandlerGame :: Event -> GameState -> GameState

-- Tecla 'R' -> spawnea un RAMMER (para pruebas)
eventHandlerGame (EventKey (Char 'r') Down _ _) gs = unsafePerformIO $ do
  let (V2 (w, h)) = gameDimensions gs
      margin = 50.0
  posX <- randomRIO (margin, w - margin)
  posY <- randomRIO (margin, h - margin)
  let pos = v2 posX posY
  case createRobot (1 + length (robots gs)) "RAMMER" HEAVY RAMMER pos 0 0.0 500.0 of
    Right r -> return (gs { robots = r : robots gs })
    Left _  -> return gs

-- Manejo de Controles del Jugador (W, A, S, D)
eventHandlerGame (EventKey (Char key) keyState _ _) gs =
    let lowerKey = toLower key
        relevantKeys = Set.fromList ['w', 'a', 's', 'd']
        currentKeys = playerKeys gs
    in if Set.member lowerKey relevantKeys then
           case keyState of
                Down -> gs { playerKeys = Set.insert lowerKey currentKeys }
                Up   -> gs { playerKeys = Set.delete lowerKey currentKeys }
       else
           gs

-- Disparo del Jugador (Clic Izquierdo)
eventHandlerGame (EventKey (MouseButton LeftButton) Down _ _) gs =
    gs { playerWantsToFire = True }

-- Apuntado del Jugador (Movimiento del Ratón)
eventHandlerGame (EventMotion (mx, my)) gs =
    case findPlayerRobot gs of
        Nothing -> gs
        Just player ->
            let (worldX, worldY) = windowToWorld gs (mx, my)
                playerPos = objPos $ robotBase player
                targetAngle = angleToTarget playerPos (v2 worldX worldY)
            in gs { playerTargetTurretAngle = Just targetAngle }

eventHandlerGame _ gs = gs


-------------------------------------------------------------------------------
-- Lógica del Menú (Dibujo y Eventos)
-------------------------------------------------------------------------------

-- Constantes de diseño del menú
labelW, typeBtnW, behBtnW, checkBtnW, colGap, colGapCheck, rowH, rowGap, panelPad, headerH, footerH :: Float
labelW    = 160
typeBtnW = 140
behBtnW  = 220
checkBtnW = 40
colGap    = 22
colGapCheck = 14
rowH      = 34
rowGap    = 14
panelPad = 28
headerH  = 90
footerH  = 160

rowW :: Float
rowW = labelW + colGap + typeBtnW + colGap + behBtnW + colGapCheck + checkBtnW

panelW :: Float
panelW = rowW + 2 * panelPad

maxRows :: Int
maxRows = 5

-- Funciones helper para ciclar opciones en el menú
nextType :: RobotType -> RobotType
nextType LIGHT  = MEDIUM
nextType MEDIUM = HEAVY
nextType HEAVY  = LIGHT

behaviors :: [IABehavior]
behaviors = [AGGRESSIVE, BALANCED, DEFENSIVE, PEACEFUL, RAMMER]

nextBehavior :: IABehavior -> IABehavior
nextBehavior b = case dropWhile (/= b) (behaviors ++ [head behaviors]) of
                   (_:x:_) -> x
                   _       -> head behaviors

-- Dibuja un botón genérico
button :: Float -> Float -> Float -> Float -> Color -> String -> Picture
button cx cy w h col label =
  let
      lightCol = withAlpha 0.5 white
      darkCol  = withAlpha 0.5 black
      halfW = w / 2
      halfH = h / 2
      b = 2.0
      horizontalOffset = -(fromIntegral (length label) * 4.8)
      verticalOffset = -7
  in
  Translate cx cy $
    Pictures
      [ Color col $ rectangleSolid w h
      , Color lightCol $ Pictures
        [ Translate 0 (halfH - b/2) $ rectangleSolid (w - b) b
        , Translate (-halfW + b/2) 0 $ rectangleSolid b (h - b)
        ]
      , Color darkCol $ Pictures
        [ Translate 0 (-halfH + b/2) $ rectangleSolid (w - b) b
        , Translate (halfW - b/2) 0 $ rectangleSolid b (h - b)
        ]
      , Translate horizontalOffset verticalOffset $
        Scale 0.12 0.12 $
        Color white $
        Text label
      ]

-- Dibuja un checkbox
checkbox :: Float -> Float -> Float -> Bool -> Picture
checkbox cx cy size isChecked =
  let border = Color (greyN 0.8) $ rectangleWire size size
      checkMark = if isChecked
                    then Color cyan $ Pictures
                                  [ Rotate 45  $ rectangleSolid (size * 0.7) (size * 0.15)
                                  , Rotate (-45) $ rectangleSolid (size * 0.7) (size * 0.15)
                                  ]
                    else Blank
  in Translate cx cy $ Pictures [border, checkMark]

-- Comprueba si un punto (px,py) está dentro de un rectángulo (cx,cy,w,h)
inRect :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
inRect px py cx cy w h = px >= cx - w/2 && px <= cx + w/2 && py >= cy - h/2 && py <= cy + h/2

-- Dibujo del menú (dispatcha al modo correcto)
drawMenu :: MenuState -> Picture
drawMenu ms = case msMode ms of
  MenuHome   -> drawMenuHome ms
  MenuConfig -> drawMenuConfig ms

-- Dibuja la pantalla principal (MenuHome)
drawMenuHome :: MenuState -> Picture
drawMenuHome ms =
  let homeW = panelW
      homeH = 350
      topY  = homeH / 2
      titleY = topY - 45
      resY   = titleY - 65
      startY = resY - 70
      exitY  = startY - 55

      panel = Pictures
        [ Color (withAlpha 0.80 (greyN 0.10)) $ rectangleSolid homeW homeH
        , Color (withAlpha 0.95 (greyN 0.35)) $ rectangleWire homeW homeH
        ]

      titleText = "Torneo de Drones"
      titleOffset = - (fromIntegral (length titleText) * 12.5)
      title = Translate 0 titleY $
              Translate titleOffset (-15) $ Scale 0.3 0.3 $ Color white $ Text titleText

      -- Controles de Resolución
      resText = "Resolucion: " ++ show (round $ msWidth ms) ++ "x" ++ show (round $ msHeight ms)
      resTextOffset = - (fromIntegral (length resText) * 4)
      resLabel = Translate resTextOffset 0 $ Scale 0.1 0.1 $ Color white $ Text resText
      resControls = Translate 0 resY $ Pictures
        [ Translate 0 10 resLabel
        , button (-80) (-15) 40 rowH (greyN 0.4) "<"
        , button ( 80) (-15) 40 rowH (greyN 0.4) ">"
        ]

      buttons = Pictures
        [ button 0 startY 240 44 (dark green) "Configurar"
        , button 0 exitY  160 40 (withAlpha 0.9 red) "Salir"
        ]
  in Pictures [panel, title, resControls, buttons]

-- Dibuja la pantalla de configuración de tanques (MenuConfig)
drawMenuConfig :: MenuState -> Picture
drawMenuConfig ms =
  let cfgs = msConfigs ms
      n    = length cfgs

      -- Layout
      panelH = headerH + fromIntegral maxRows * rowH + fromIntegral (maxRows - 1) * rowGap + footerH
      topY   = panelH / 2
      titleY = topY - panelPad - 20
      rowsAreaH = fromIntegral maxRows * rowH + fromIntegral (maxRows - 1) * rowGap
      rowsStartY = titleY - 40 - rowsAreaH / 2
      stepY = rowH + rowGap
      labelX = - panelW / 2 + panelPad + labelW / 2
      typeX  = labelX + labelW / 2 + colGap + typeBtnW / 2
      behX   = typeX + typeBtnW / 2 + colGap + behBtnW / 2
      checkX = behX + behBtnW / 2 + colGapCheck + checkBtnW / 2
      addX   = panelW / 2 - panelPad - 85
      addY   = titleY
      playY  = - panelH / 2 + panelPad + 55
      powerUpCheckY = playY - 45
      powerUpCheckX = 0
      hitboxCheckY = powerUpCheckY
      hitboxCheckX = -200

      panel = Pictures [ Color (withAlpha 0.85 (dark (dark azure))) $ rectangleSolid panelW panelH
                       , Color (withAlpha 0.95 white) $ rectangleWire panelW panelH ]
      titleText = "Configurar Partida"
      titleOffset = - (fromIntegral (length titleText) * 6.5)
      title = Translate 0 titleY $ Translate titleOffset (-10) $ Scale 0.2 0.2 $ Color white $ Text titleText

      headerLabels = Pictures
        [ Translate (typeX - 30) (rowsStartY + rowH/2 + 80) $ Scale 0.10 0.10 $ Color (greyN 0.8) $ Text "Tamano"
        , Translate (behX - 60)  (rowsStartY + rowH/2 + 80) $ Scale 0.10 0.10 $ Color (greyN 0.8) $ Text "Comportamiento (IA)"
        , Translate (checkX - 20) (rowsStartY + rowH/2 + 80) $ Scale 0.10 0.10 $ Color (greyN 0.8) $ Text "Jugador"
        , button addX addY 150 32 (withAlpha 0.8 (dark blue)) "Anadir dron"
        ]

      -- Dibuja cada fila de configuración
      rowPics = concat
        [ let y = rowsStartY - fromIntegral i * stepY + 50
              c = cfgs !! i
              t = tcType c
              b = tcBehavior c
              isPlayer = b == PLAYER
              labelT = show t
              labelB = if isPlayer then "N/A" else show b
              labelColor = if isPlayer then cyan else white
              labelPic = Translate labelX y $ Translate (- (fromIntegral (length ("Dron " ++ show (i+1))) * 4)) 0 $ Scale 0.12 0.12 $ Color labelColor $ Text ("Dron " ++ show (i+1))
              typeBtnPic = button typeX y typeBtnW rowH (withAlpha 0.6 (greyN 0.2)) labelT
              -- El botón de comportamiento se deshabilita si es jugador
              behBtnPic = if isPlayer then Translate behX y $ Pictures [ Color (greyN 0.15) $ rectangleSolid behBtnW rowH, Color (greyN 0.4) $ rectangleWire behBtnW rowH, Translate (-15) (-6) $ Scale 0.12 0.12 $ Color (greyN 0.6) $ Text "N/A (Jugador)" ] else button behX y behBtnW rowH (withAlpha 0.6 (greyN 0.2)) labelB
              checkBtnPic = checkbox checkX y (rowH * 0.7) isPlayer
          in [labelPic, typeBtnPic, behBtnPic, checkBtnPic]
        | i <- [0..n-1]
        ]

      hasPlayer = any (\cfg -> tcBehavior cfg == PLAYER) cfgs
      playButtonText = if n >= 2 then "Jugar" else "Jugar (min 2)"
      playButtonColor = if n >= 2 then dark green else greyN 0.5

      footer = Pictures
        [ button 0 playY 240 42 playButtonColor playButtonText
        , Translate 100 (playY - 65) $ Scale 0.1 0.1 $
            Color (if hasPlayer then cyan else greyN 0.7) $
            Text (if hasPlayer then "Un jugador seleccionado" else "Marca la casilla para ser jugador")
        , Translate powerUpCheckX powerUpCheckY $ Pictures -- Checkbox Powerups
            [ checkbox (-80) 0 (rowH * 0.6) (msPowerUpsEnabled ms)
            , Translate (-60) (-5) $ Scale 0.1 0.1 $ Color white $ Text "Activar Powerups"
            ]
        , Translate hitboxCheckX hitboxCheckY $ Pictures
            [ checkbox (-80) 0 (rowH * 0.6) (msShowHitboxes ms)
            , Translate (-60) (-5) $ Scale 0.1 0.1 $ Color white $ Text "Mostrar Hitboxes"
            ]
        ]
  in Pictures ([panel, title, headerLabels] ++ rowPics ++ [footer])


-- Pantalla de fin de partida (GameOver)
drawGameOver :: Maybe String -> Picture
drawGameOver winM =
  let overW = panelW
      overH = 260
      topY  = overH/2
      titleY = topY - 40
      msgY   = titleY - 40
      menuY  = -10
      exitY  = -60
      panel = Pictures
        [ Color (withAlpha 0.80 (greyN 0.10)) $ rectangleSolid overW overH
        , Color (withAlpha 0.95 (greyN 0.35)) $ rectangleWire overW overH
        ]
      title = Translate (-100) titleY $ Scale 0.20 0.20 $ Color white $ Text "Partida terminada"
      msgText = case winM of
        Just name -> "Ganador: " ++ name
        Nothing   -> "Empate"
      msg = Translate (- (fromIntegral (length msgText) * 4)) msgY $ Scale 0.14 0.14 $ Color white $ Text msgText
      buttons = Pictures
        [ button 0 menuY 200 42 (dark (dark blue)) "Volver al menu"
        , button 0 exitY 160 40 (withAlpha 0.9 red) "Salir"
        ]
  in Pictures [panel, title, msg, buttons]

-- Función principal de dibujo: decide qué pantalla dibujar
drawApp :: GameAssets -> AppState -> Picture
drawApp assets appState = case appState of
  InMenu ms    -> Pictures [ drawMenuBg (msWidth ms, msHeight ms), drawMenu ms ]
  InGame gs    -> drawGame assets gs
  GameOver winM -> Pictures [ drawMenuBg defaultResolution, drawGameOver winM ]
  where
    -- Dibuja el fondo escalado y oscurecido para los menús
    drawMenuBg :: (Float, Float) -> Picture
    drawMenuBg (w, h) = Pictures
      [ Scale (w / bgWidth) (h / bgHeight) (aBgImage assets)
      , Color (withAlpha 0.6 black) (rectangleSolid w h)
      ]

-------------------------------------------------------------------------------
-- Manejador de Eventos Principal
-------------------------------------------------------------------------------

eventHandler :: Event -> AppState -> AppState

-- Tecla 'F' (atajo para volver al menú)
eventHandler (EventKey (Char 'f') Down _ _) _ = InMenu initialMenuState

-- Eventos 'InGame' (se delegan a eventHandlerGame)
eventHandler ev@(EventKey (Char key) _ _ _) st@(InGame gs) | toLower key `elem` ['w', 'a', 's', 'd', 'r'] =
    InGame (eventHandlerGame ev gs)
eventHandler ev@(EventMotion _) st@(InGame gs) = InGame (eventHandlerGame ev gs)
eventHandler ev@(EventKey (MouseButton LeftButton) Down _ _) st@(InGame gs) =
    InGame (eventHandlerGame ev gs)

-- Eventos 'InMenu'
eventHandler ev st@(InMenu ms) = case msMode ms of

    -- Manejador para 'MenuHome'
    MenuHome ->
      case ev of
        EventKey (MouseButton LeftButton) Down _ (mx, my) ->
          let
            -- Coordenadas de los botones (deben coincidir con drawMenuHome)
            homeH = 350
            topY  = homeH / 2
            titleY = topY - 45
            resY   = titleY - 65
            startY = resY - 70
            exitY  = startY - 55
            resBtnW = 40
            resBtnH = rowH
            resLeftX = -80
            resRightX = 80

            clickedResLeft  = inRect mx my resLeftX (resY - 15) resBtnW resBtnH
            clickedResRight = inRect mx my resRightX (resY - 15) resBtnW resBtnH
            clickedStart = inRect mx my 0 startY 240 44
            clickedExit  = inRect mx my 0 exitY  160 40

            -- Lógica para ciclar resoluciones
            cycleResolution :: Int -> MenuState -> MenuState
            cycleResolution dir ms' =
              let currentRes = (msWidth ms', msHeight ms')
                  currentIndex = fromMaybe 0 $ findIndex (== currentRes) availableResolutions
                  newIndex = (currentIndex + dir + length availableResolutions) `mod` length availableResolutions
                  (newW, newH) = availableResolutions !! newIndex
              in ms' { msWidth = newW, msHeight = newH }

          in if clickedStart then InMenu ms { msMode = MenuConfig }
             else if clickedExit then unsafePerformIO (exitSuccess >> return st)
             else if clickedResLeft then InMenu (cycleResolution (-1) ms)
             else if clickedResRight then InMenu (cycleResolution 1 ms)
             else InMenu ms
        _ -> InMenu ms

    -- Manejador para 'MenuConfig'
    MenuConfig ->
      case ev of
        EventKey (MouseButton LeftButton) Down _ (mx, my) ->
          let cfgs = msConfigs ms
              len = length cfgs

              -- Coordenadas de layout (deben coincidir con drawMenuConfig)
              panelH = headerH + fromIntegral maxRows * rowH + fromIntegral (maxRows - 1) * rowGap + footerH
              topY   = panelH / 2
              titleY = topY - panelPad - 20
              rowsStartY = titleY - 40 - (fromIntegral maxRows * rowH + fromIntegral (maxRows - 1) * rowGap) / 2
              stepY = rowH + rowGap
              labelX = - panelW / 2 + panelPad + labelW / 2
              typeX  = labelX + labelW / 2 + colGap + typeBtnW / 2
              behX   = typeX + typeBtnW / 2 + colGap + behBtnW / 2
              checkX = behX + behBtnW / 2 + colGapCheck + checkBtnW / 2
              addX   = panelW / 2 - panelPad - 85
              addY   = titleY
              playY  = - panelH / 2 + panelPad + 55
              powerUpCheckY = playY - 45
              powerUpCheckX = -60
              checkSize = rowH * 0.7
              powerUpCheckSize = rowH * 0.6
              hitboxCheckY = powerUpCheckY
              hitboxCheckX = -280
              hitboxCheckSize = powerUpCheckSize

              clickedAdd  = inRect mx my addX addY 170 32
              clickedPlay = inRect mx my 0   playY 240 42
              clickedPowerUpCheck = inRect mx my powerUpCheckX powerUpCheckY powerUpCheckSize powerUpCheckSize
              clickedHitboxCheck = inRect mx my hitboxCheckX hitboxCheckY hitboxCheckSize hitboxCheckSize

              hasPlayer = isJust $ findIndex (\cfg -> tcBehavior cfg == PLAYER) cfgs

              -- Comprueba si el clic fue en un botón de una fila
              clickOnRow i y =
                  let sizeRect = inRect mx my typeX y typeBtnW rowH
                      behRect  = inRect mx my behX  y behBtnW  rowH
                      checkRect = inRect mx my checkX y checkSize checkSize
                  in (sizeRect, behRect, checkRect)

              -- Aplica el cambio a la configuración de la fila 'i'
              applyRowClicks i c y =
                  let (sizeHit, behHit, checkHit) = clickOnRow i y
                      currentBehavior = tcBehavior c
                      isPlayer = currentBehavior == PLAYER
                  in if sizeHit then c { tcType = nextType (tcType c) }
                     else if behHit && not isPlayer then c { tcBehavior = nextBehavior currentBehavior }
                     else if checkHit then
                         if isPlayer then c { tcBehavior = BALANCED } -- Deselecciona
                         else if not hasPlayer then c { tcBehavior = PLAYER } -- Selecciona
                         else c -- Ya hay un jugador, no hace nada
                     else c

              newCfgs = [ applyRowClicks i c (rowsStartY - fromIntegral i * stepY + 50) | (i,c) <- zip [0..] cfgs ]
              intermediateMs = ms { msConfigs = newCfgs }
              afterPowerUpMs = if clickedPowerUpCheck
                                   then intermediateMs { msPowerUpsEnabled = not (msPowerUpsEnabled intermediateMs) }
                                   else intermediateMs

              finalMs = if clickedHitboxCheck
                                  then afterPowerUpMs { msShowHitboxes = not (msShowHitboxes afterPowerUpMs) }
                                  else afterPowerUpMs

          in if clickedAdd && len < maxRows then
               InMenu afterPowerUpMs { msConfigs = msConfigs afterPowerUpMs ++ [TankConfig MEDIUM BALANCED] }

             else if clickedPlay && len >= 2 then
               -- Inicia el juego
               let currentWidth = msWidth afterPowerUpMs
                   currentHeight = msHeight afterPowerUpMs
                   enablePowerups = msPowerUpsEnabled afterPowerUpMs
                   showHitboxesFlag = msShowHitboxes finalMs
                   newStateIO = do
                     gs <- startGameFromConfigs currentWidth currentHeight enablePowerups showHitboxesFlag (msConfigs finalMs)
                     return (InGame gs)
               in unsafePerformIO newStateIO

             else
               InMenu finalMs
        _ -> InMenu ms

-- Eventos 'GameOver'
eventHandler ev st@(GameOver _) =
    case ev of
      EventKey (MouseButton LeftButton) Down _ (mx, my) ->
        let menuY = -10; exitY = -60
            clickedMenu = inRect mx my 0 menuY 200 42
            clickedExit = inRect mx my 0 exitY  160 40
        in if clickedMenu then InMenu initialMenuState
           else if clickedExit then unsafePerformIO (exitSuccess >> return st)
           else st
      _ -> st

-- Fallback
eventHandler _ st = st

-------------------------------------------------------------------------------
-- Manejador de Actualización Principal
-------------------------------------------------------------------------------

updateHandler :: Float -> AppState -> AppState
updateHandler _ st@(InMenu _) = st -- El menú no se actualiza con el tiempo
updateHandler dt (InGame gs) =
  let gs' = updateGame dt gs
      alive = filter isRobotAlive (robots gs')
      numAlive = length alive

      -- Comprueba si hay un ganador
      winnerName = case alive of
                         [r] -> Just (robotName r) -- Queda 1: ganador
                         []  -> Nothing            -- Quedan 0: empate
                         _   -> Nothing            -- Quedan >1: el juego continúa

  in if numAlive <= 1 then GameOver winnerName else InGame gs'
updateHandler _ st@(GameOver _) = st -- GameOver no se actualiza

