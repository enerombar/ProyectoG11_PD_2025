-- Fichero: torneos.hs
-- (VERSIÓN MODIFICADA - Reemplazar)

-- Este es el nuevo módulo Main para ejecutar torneos consecutivos.
module Main where

-- Imports de Librerías
import Graphics.Gloss hiding (Text)
import Graphics.Gloss (Picture(..), Color, Display(..), Point, Vector, Path, Display(..), pictures, translate, rotate, scale, color, blank, line, polygon, circleSolid, thickCircle, rectangleSolid, rectangleWire, circle)
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..), KeyState(..), Modifiers(..), MouseButton(..), play)
import Graphics.Gloss.Juicy (loadJuicy)
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitSuccess)
import System.IO (readFile, writeFile) -- <--- MODIFICADO
import Control.Exception (try, SomeException)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, isJust, mapMaybe, listToMaybe)
import Data.List (findIndex, intercalate)
import Data.Char (toLower, isSpace)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Imports Locales
import Entities
import Math (rad2deg, angleToTarget, distance)
import Game (startGameFromConfigs, createRobot) 
import Logic 
import Physics (checkCollisions) --
import IA (getAIActions) 
import Estadisticas

-------------------------------------------------------------------------------
-- NUEVA SECCIÓN: Configuración de Torneo
-------------------------------------------------------------------------------

-- | Almacena la configuración leída de config.txt
data Config = Config
  { cfgConfigs   :: [TankConfig] -- Lista de bots (IA y jugador)
  , cfgWidth     :: Float        -- Ancho del mapa
  , cfgHeight    :: Float        -- Alto del mapa
  , cfgDuration  :: Int          -- Duración en *frames*
  , cfgNumGames  :: Int          -- Número de partidas
  , cfgPowerUps  :: Bool         -- Powerups habilitados
  , cfgHitboxes  :: Bool         -- Hitboxes visibles
  } deriving (Show)

-- | Estado principal de la aplicación de torneos
data TournamentState = TournamentState
  { tsGameState  :: GameState -- El estado del juego actual
  , tsConfig     :: Config    -- La configuración cargada
  , tsGamesLeft  :: Int       -- Cuántas partidas faltan
  , tsTimer      :: Int       -- Temporizador de partida actual (en frames)
  , tsStats      :: StatsGlobales -- Para guardar stats
  }

-- | Configuración por defecto si config.txt falla
defaultConfig :: Config
defaultConfig = Config
  { cfgConfigs = [ TankConfig HEAVY AGGRESSIVE 
                 , TankConfig MEDIUM BALANCED 
                 ]
  , cfgWidth = 1280
  , cfgHeight = 720
  , cfgDuration = 120 * 60 -- 120 segundos
  , cfgNumGames = 3
  , cfgPowerUps = True
  , cfgHitboxes = False
  }

-- --- Helpers de Parsing ---

-- Quita espacios en blanco de ambos extremos
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- Divide un string por un delimitador (simplificado)
splitOn :: Char -> String -> [String]
splitOn delim s = case dropWhile (==delim) s of
                    "" -> []
                    s' -> w : splitOn delim s''
                            where (w, s'') = break (==delim) s'

-- Parsea una línea del tipo "clave = valor"
parseLine :: String -> Maybe (String, String)
parseLine line =
  let trimmedLine = trim line
  in if null trimmedLine || head trimmedLine == '#'
     then Nothing
     else case break (=='=') trimmedLine of
            (key, '=':val) -> Just (trim (map toLower key), trim val)
            _              -> Nothing

-- Parsea una definición de bot "HEAVY AGGRESSIVE"
parseBotConfig :: String -> Maybe TankConfig
parseBotConfig s =
  case words (trim s) of
    [typeStr, behaviorStr] ->
      do
        botType <- readMaybe typeStr :: Maybe RobotType 
        behavior <- readMaybe behaviorStr :: Maybe IABehavior 
        return (TankConfig botType behavior) 
    _ -> Nothing

-- Parsea "true" o "false"
parseBool :: String -> Maybe Bool
parseBool s = case map toLower s of
                "true" -> Just True
                "false" -> Just False
                _ -> Nothing

-- | Lee y parsea el archivo config.txt
parseConfig :: String -> IO Config
parseConfig content = do
  let lines' = lines content
  let pairs = mapMaybe parseLine lines'
  let getValue k = fmap trim (lookup k pairs)
 
  -- Parsear bots
  let botsStr = fromMaybe "" (getValue "bots")
  let playerStr = getValue "player_bot"
 
  let botConfigs = mapMaybe parseBotConfig (splitOn ',' botsStr)
  let playerConfig = case playerStr of
        Nothing -> Nothing
        Just tStr -> fmap (\t -> TankConfig t PLAYER) (readMaybe tStr :: Maybe RobotType) 
        
  let allConfigs = botConfigs ++ (maybe [] (:[]) playerConfig)
 
  -- Parsear valores numéricos y booleanos
  let width = fromMaybe (cfgWidth defaultConfig) (getValue "width" >>= readMaybe)
  let height = fromMaybe (cfgHeight defaultConfig) (getValue "height" >>= readMaybe)
  let durationSec = fromMaybe (cfgDuration defaultConfig `div` 60) (getValue "duration" >>= readMaybe)
  let numGames = fromMaybe (cfgNumGames defaultConfig) (getValue "num_tournaments" >>= readMaybe)
  let powerups = fromMaybe (cfgPowerUps defaultConfig) (getValue "powerups" >>= parseBool)
  let hitboxes = fromMaybe (cfgHitboxes defaultConfig) (getValue "show_hitboxes" >>= parseBool)

  let finalConfigs = if null allConfigs then cfgConfigs defaultConfig else allConfigs

  putStrLn $ "Configuración cargada: " ++ show (length finalConfigs) ++ " robots."
  if isJust playerConfig then putStrLn "  (Jugador incluido)" else putStrLn "  (Solo IAs)"

  return Config
    { cfgConfigs = finalConfigs
    , cfgWidth = width
    , cfgHeight = height
    , cfgDuration = durationSec * 60 -- Convertir a frames
    , cfgNumGames = numGames
    , cfgPowerUps = powerups
    , cfgHitboxes = hitboxes
    }

-------------------------------------------------------------------------------
-- Configuración Aleatoria (Nueva)
-------------------------------------------------------------------------------

-- | Elige un elemento aleatorio de una lista
choice :: [a] -> IO a
choice xs = do
  i <- randomRIO (0, length xs - 1)
  return (xs !! i)

-- | Entero aleatorio en [lo, hi]
randInt :: Int -> Int -> IO Int
randInt lo hi = randomRIO (lo, hi)

-- | Booleano aleatorio
randBool :: IO Bool
randBool = do b <- randomRIO (0,1 :: Int)
              return (b == 1)

-- | Genera un Config aleatorio sin leer config.txt
randomConfig :: IO Config
randomConfig = do
  -- Dimensiones posibles
  let resolutions = [(1280,720),(1366,768),(1600,900)]
  (w,h) <- choice resolutions

  -- Powerups / hitboxes
  pwr <- randBool
  let hitb = False

  -- Duración (segundos) y número de partidas
  durSec <- randInt 60 180
  nGames <- randInt 1 5

  -- Bots IA: entre 2 y 6
  nBots <- randInt 2 6
  botPairs <- mapM (\_ -> do
                      t <- choice [LIGHT, MEDIUM, HEAVY]
                      b <- choice [AGGRESSIVE, BALANCED, DEFENSIVE, PEACEFUL, RAMMER]
                      return (t,b)) [1..nBots]
  let botsCfgs = [ TankConfig t b | (t,b) <- botPairs ]

  -- ¿Jugador?
  withPlayer <- randBool
  playerCfg <- if withPlayer
                  then do t <- choice [LIGHT, MEDIUM, HEAVY]
                          return [TankConfig t PLAYER]
                  else return []

  let allCfgs = botsCfgs ++ playerCfg

  -- Mensaje informativo
  putStrLn ("[CFG] Tamaño: " ++ show w ++ "x" ++ show h)
  putStrLn ("[CFG] Powerups: " ++ show pwr ++ ", Hitboxes: " ++ show hitb)
  putStrLn ("[CFG] Duración (s): " ++ show durSec ++ ", Partidas: " ++ show nGames)
  putStrLn ("[CFG] Bots: " ++ intercalate ", " [ show t ++ " " ++ show b | (t,b) <- botPairs ])
  putStrLn ("[CFG] Jugador: " ++ if withPlayer then "sí" else "no")

  return Config
    { cfgConfigs = if null allCfgs then cfgConfigs defaultConfig else allCfgs
    , cfgWidth = fromIntegral w
    , cfgHeight = fromIntegral h
    , cfgDuration = durSec * 60
    , cfgNumGames = nGames
    , cfgPowerUps = pwr
    , cfgHitboxes = hitb
    }

-- | Formatea una sección de texto describiendo la configuración usada
formatConfigSection :: Config -> String
formatConfigSection cfg =
  let fmt (TankConfig t b) = show t ++ " " ++ show b
      botsTxt = intercalate ", " (map fmt (cfgConfigs cfg))
  in unlines
      [ "--- CONFIGURACIÓN USADA ---"
      , "Dimensiones: " ++ show (round (cfgWidth cfg)) ++ "x" ++ show (round (cfgHeight cfg))
      , "Powerups: " ++ show (cfgPowerUps cfg)
      , "Hitboxes: " ++ show (cfgHitboxes cfg)
      , "Duración (s): " ++ show (cfgDuration cfg `div` 60)
      , "Número de torneos: " ++ show (cfgNumGames cfg)
      , "Participantes: " ++ botsTxt
      , ""
      ]

-------------------------------------------------------------------------------
-- Configuración de la Ventana (Modificada)
-------------------------------------------------------------------------------

fps :: Int
fps = 60 

-- Modificada para aceptar tamaño desde Config
window :: Float -> Float -> Display
window w h = InWindow "Torneo de Drones" (round w, round h) (100, 100) 

backgroundColor :: Color
backgroundColor = black 

-------------------------------------------------------------------------------
-- Constantes de Dimensiones de Assets (Copiado de Main.hs)
-------------------------------------------------------------------------------
 
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
-- Estado de la Aplicación (Copiado de Main.hs)
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Punto de Entrada: Main (NUEVO)
-------------------------------------------------------------------------------

main :: IO ()
main = do
  -- 1. Generar configuración aleatoria (sin leer config.txt)
  putStrLn "Generando configuración aleatoria..."
  config <- randomConfig

  putStrLn $ "Iniciando serie de " ++ show (cfgNumGames config) ++ " torneos."
 
  -- 2. Cargar Assets (Lógica de Main.hs)  
  assets <- loadAssets

  -- 3. Crear GameState inicial
  let (w, h) = (cfgWidth config, cfgHeight config)
  initialGame <- startGameFromConfigs w h (cfgPowerUps config) (cfgHitboxes config) (cfgConfigs config) 
 
  -- 4. Crear TournamentState inicial
  let initialState = TournamentState
        { tsGameState = initialGame
        , tsConfig = config
        , tsGamesLeft = cfgNumGames config
        , tsTimer = cfgDuration config
        , tsStats = inicializarStatsGlobales
        }
 
  -- 5. Iniciar 'play' con los nuevos manejadores
  play
    (window (cfgWidth config) (cfgHeight config))
    backgroundColor
    fps
    initialState
    (drawHandler assets) -- NUEVO manejador de dibujo
    eventHandler         -- NUEVO manejador de eventos
    updateHandler        -- NUEVO manejador de actualización

-- | Carga todos los assets del juego (Copiado de Main.hs)  
loadAssets :: IO GameAssets
loadAssets = do
  let explosionFrameNames = map (\n -> "assets/explosion/expl_" ++ show n ++ ".png") [0..6]
  let (defW, defH) = (1280, 720) -- Fallback resolution for assets

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
  let shieldPUPic = fromMaybe (Color blue $ ThickCircle 15 5) maybeSpeedPU
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
  return GameAssets
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

-------------------------------------------------------------------------------
-- Lógica del Bucle Principal del Juego (Update) (Copiado de Main.hs)
-------------------------------------------------------------------------------

-- Esta función se ejecuta en cada fotograma 
-- **** MODIFICADA PARA ESTADÍSTICAS ****
-- EN torneos.hs (Función updateGame CORREGIDA)

updateGame :: Float -> GameState -> GameState
updateGame dt gs =
  let
    -- 1. Reducir todos los cooldowns y temporizadores
    gsCooldowns = decreaseCooldown gs

    -- 1.5. Comprobar si alguna mina ha explotado
    gsMinesResolved = resolveExplodingMines gsCooldowns

    -- 2. Comprobar y potencialmente spawnear/reubicar un powerup
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

    -- 8.5. Incrementar tiempo vivo (en segundos) para robots que sigan vivos
    -- **** INICIO DE LA CORRECCIÓN ****
    incTime r = if robotHealth r > 0 then r { robotTiempoVivo = robotTiempoVivo r + dt } else r
    gsAfterTime = gsAfterCollisions { robots = map incTime (robots gsAfterCollisions) }

    -- 9. Manejar muertes de robots y crear explosiones
    livingRobots = filter isRobotAlive (robots gsAfterTime)
    deadRobots   = filter (not . isRobotAlive) (robots gsAfterTime)
    newExplosions = map createExplosionFromRobot deadRobots

    -- 10. Estado final del fotograma
    gsFinal = gsAfterTime
      { robots = livingRobots
      , explosions = newExplosions ++ explosions gsAfterTime
      -- **** MODIFICACIÓN ESTADÍSTICAS: Guardar los muertos ****
      , gsDeadRobots = deadRobots ++ gsDeadRobots gsAfterTime
      }
  in gsFinal
-------------------------------------------------------------------------------
-- Lógica de Renderizado (Dibujo) (Copiado de Main.hs)
-------------------------------------------------------------------------------

-- | NUEVO Manejador de Dibujo
-- Dibuja el juego Y la UI del torneo
drawHandler :: GameAssets -> TournamentState -> Picture
drawHandler assets ts =
  let
    -- 1. Dibuja el estado del juego (lógica de 'drawGame' de Main.hs) 
    gs = tsGameState ts
    (V2 (worldW, worldH)) = gameDimensions gs 
    mapBorder = Color white $ Line [ (0, 0), (worldW, 0), (worldW, worldH), (0, worldH), (0, 0) ]
    powerUpPic = case powerUp gs of 
        Nothing -> Blank
        Just pu -> drawPowerUp (aPowerUpPics assets) pu
    controlsPic = drawPlayerControls gs 

    robotPics     = map (drawRobot assets gs) (robots gs) 
    projPics      = map drawProjectile (projectiles gs) 
    explosionPics = map (drawExplosion (aExplosionFrames assets)) (explosions gs) 
    obstaclePics  = map (drawObstacle assets gs) (obstacles gs) 

    gamePic = Translate (-worldW / 2) (-worldH / 2) $
        Pictures $
          [ Translate (worldW / 2) (worldH / 2) $
            Scale (worldW / bgWidth) (worldH / bgHeight) (aBgImage assets)
          , mapBorder 
          ]
          ++ obstaclePics
          ++ robotPics     
          ++ [drawTeslaBeams gs] 
          ++ projPics     
          ++ explosionPics 
          ++ [powerUpPic]  
          ++ [controlsPic]
    
    -- 2. Dibuja la UI del Torneo
    timerSecs = tsTimer ts `div` 60
    timerText = "Tiempo: " ++ show timerSecs
    gamesText = "Partidas restantes: " ++ show (tsGamesLeft ts)
    
    uiPic = Translate (-worldW / 2 + 20) (worldH / 2 - 40) $ -- Esquina sup-izquierda
            Scale 0.15 0.15 $
            Pictures
              [ Translate 0 0 $ Color white $ Text gamesText
              , Translate 0 (-150) $ Color white $ Text timerText
              ]

  in Pictures [gamePic, uiPic]


-- Dibuja la UI de controles del jugador (Copiado de Main.hs)  
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

-- Dibuja un robot individual (Copiado de Main.hs)  
drawRobot :: GameAssets -> GameState -> Robot -> Picture
drawRobot assets gs r =
  let
    base = robotBase r
    (x, y) = unV2 (objPos base) 
    (w, h) = unV2 (objSize base) 
    angleDeg = negate $ rad2deg (objAngle base) 
    turretAngleDeg = negate $ rad2deg (turretAngle r) 
    isPlayer = robotBehavior r == PLAYER 
    isHit = (robotHitTimer r > 0) && (robotHitTimer r `mod` 8 < 4) 

    selectedChasisImage = case (isPlayer, isHit) of
      (True,  True)  -> aPlayerChasisHitImage assets
      (True,  False) -> aPlayerChasisImage assets
      (False, True)  -> aAIChasisHitImage assets
      (False, False) -> aAIChasisImage assets

    selectedTurretImage = if robotBehavior r == RAMMER then Blank else 
      case (isPlayer, isHit) of
        (_,     True)  -> aTurretHitImage assets
        (True,  False) -> aPlayerTurretImage assets
        (False, False) -> aAITurretImage assets

    (imgW, imgH) = if isPlayer
                      then (playerChasisImgWidth, playerChasisImgHeight)
                      else (aiChasisImgWidth, aiChasisImgHeight)

    baseChasis = Scale (w / imgW) (h / imgH) selectedChasisImage
    rammerDecals = Pictures [Translate (w * 0.25) (h * 0.15) $ Rotate (-30) $ Color (dark red) $ rectangleSolid (w * 0.4) (h * 0.2)
        , Translate (w * 0.25) (h * (-0.15)) $ Rotate 30 $ Color (dark red) $ rectangleSolid (w * 0.4) (h * 0.2)]
    body = Translate x y $ Rotate angleDeg $
             if robotBehavior r == RAMMER 
             then Pictures [ baseChasis, rammerDecals ]
             else baseChasis

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

    (turretWidth, turretHeight) = unV2 (getTurretSize (robotType r)) 
    turret = Translate x y $ Rotate turretAngleDeg $ Rotate 180 $
             Scale (turretWidth / turretImgWidth) (turretHeight / turretImgHeight) $
             selectedTurretImage

    turretBlueOverlayPic = if slowActive
                              then Translate x y $ Rotate turretAngleDeg $ Rotate 180 $
                                     Color (withAlpha 0.45 white) $
                                     Scale (turretWidth / turretImgWidth) (turretHeight / turretImgHeight) $
                                     aTurretBlueOverlay assets
                              else Blank

    hitboxVerts = robotVerts r 
    hitboxPoints = map unV2 hitboxVerts 
    hitboxPic = if gsShowHitboxes gs then 
                  Color yellow $ Line (hitboxPoints ++ [head hitboxPoints])
                else
                  Blank
    effects = drawRobotEffects r 
    ui = drawRobotUI (aBulletImage assets) r 

  in Pictures [body, blueOverlay, turret, turretBlueOverlayPic, effects, ui, hitboxPic]

-- Dibuja efectos (Copiado de Main.hs)  
drawRobotEffects :: Robot -> Picture
drawRobotEffects r =
  let
    base = robotBase r
    (x, y) = unV2 (objPos base)
    (w, h) = unV2 (objSize base)
    angleDeg = negate $ rad2deg (objAngle base)
    shieldActive = robotShieldTimer r > 0 
    speedActive = robotSpeedBoostTimer r > 0 
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

-- Dibuja UI de robot (Copiado de Main.hs)  
drawRobotUI :: Picture -> Robot -> Picture
drawRobotUI bulletImage r =
  let
    base = robotBase r
    (x, y) = unV2 (objPos base)
    (w, h) = unV2 (objSize base)
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

    textString = robotName r 
    horizontalOffset = - (fromIntegral (length textString) * 4.0)
    nameTag = Translate x (y + h / 2 + 34) $
              Translate horizontalOffset 0 $
              Scale 0.1 0.1 $
              Color white $
              Text textString
  in Pictures [ healthBar, nameTag, if robotBehavior r /= RAMMER then ammoIconsDisplay else Blank ] 

-- Dibuja proyectil (Copiado de Main.hs)  
drawProjectile :: Projectile -> Picture
drawProjectile p = Translate x y $ Color black $ circleSolid radius
  where
    base = projBase p 
    (x, y) = unV2 (objPos base) 
    (radius, _) = unV2 (objSize base) 

-- Dibuja powerup (Copiado de Main.hs)  
drawPowerUp :: Map.Map PowerUpType Picture -> PowerUp -> Picture
drawPowerUp pics pu =
  let (V2 (x, y)) = puPos pu 
      pic = fromMaybe Blank (Map.lookup (puType pu) pics) 
      pulseFactor = 1.0 + 0.1 * sin (fromIntegral (puTimer pu) / 10.0) 
  in Translate x y $ Scale pulseFactor pulseFactor pic

-- Dibuja explosión (Copiado de Main.hs)  
drawExplosion :: [Picture] -> Explosion -> Picture
drawExplosion explosionFrames e =
  Translate x y $
  Scale scaleFactor scaleFactor $
  frameToDraw
  where
    (x, y) = unV2 (objPos (explBase e)) 
    (radius, _) = unV2 (objSize (explBase e)) 
    totalFrames = length explosionFrames
    elapsedTime = fromIntegral (explosionDuration - explTime e) 
    totalDuration = fromIntegral explosionDuration 
    frameIndex = min (totalFrames - 1) (floor (elapsedTime / totalDuration * fromIntegral totalFrames))
    frameToDraw = explosionFrames !! frameIndex
    frameH_enPantalla = radius * 2.0
    scaleFactor = frameH_enPantalla / explosionFrameHeight

-- Dibuja obstáculo (Copiado de Main.hs)  
drawObstacle :: GameAssets -> GameState -> Obstacle -> Picture
drawObstacle assets gs o =
  let (V2 (x, y)) = obsPos o 
      (V2 (w, h)) = obsSize o 
      wallImgW = 786.0
      wallImgH = 257.0
      damageRectImgW = 648.0
      damageRectImgH = 548.0
      damageImgW = 579.0
      damageImgH = 634.0
      minaImgSize = 512.0

      (obstacleSprite, hitboxShape) = case obsType o of 
          WALL ->
            let 
                wallPic = aObstacleWallPic assets
                sprite = if h > w then
                           let scaleX = w / wallImgH
                               scaleY = h / wallImgW
                           in Translate x y $ Rotate 90 $ Scale scaleX scaleY wallPic
                         else
                           let scaleX = w / wallImgW
                               scaleY = h / wallImgH
                           in Translate x y $ Scale scaleX scaleY wallPic
                hitboxVerts = obsVerts o 
                hitboxPoints = map unV2 hitboxVerts 
                shape = Line (hitboxPoints ++ [head hitboxPoints])
            in (sprite, Color yellow shape)
          DAMAGE_ZONE_RECT ->
            let
                damageRectPic = aObstacleDamageRectPic assets
                scaleX = w / damageRectImgW
                scaleY = h / damageRectImgH
                sprite = Translate x y $ Scale scaleX scaleY damageRectPic
                hitboxVerts = obsVerts o 
                hitboxPoints = map unV2 hitboxVerts 
                shape = Line (hitboxPoints ++ [head hitboxPoints])
            in (sprite, Color (dark orange) shape)
          DAMAGE_ZONE ->
            let 
                damagePic = aObstacleDamagePic assets
                scale = w / (max damageImgW damageImgH)
                sprite = Translate x y $ Scale scale scale damagePic
                radius = w * 0.5
                shape = thickCircle radius 1.5
            in (sprite, Translate x y $ Color (dark magenta) shape)
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
                scale = w / minaImgSize
                sprite = Translate x y $ Scale scale scale (aMinaInactivaPic assets)
                radius = w * 0.5
                shape = thickCircle radius 1.5
            in (sprite, Translate x y $ Color yellow shape)
          MINA_ACTIVA c -> 
            let
                pic = if c `mod` 20 < 10
                       then aMinaActivaPic assets
                       else Blank
                scale = w / minaImgSize
                sprite = Translate x y $ Scale scale scale pic
                radius = w * 0.5
                shape = thickCircle radius 1.5
            in (sprite, Translate x y $ Color red shape)
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
  in Pictures [obstacleSprite, hitboxPic]

-- Dibuja rayos tesla (Copiado de Main.hs)  
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
-- Manejador de Eventos (Input) (Adaptado de Main.hs)
-------------------------------------------------------------------------------

-- Convierte coordenadas de ventana Gloss (Copiado de Main.hs)  
windowToWorld :: GameState -> (Float, Float) -> (Float, Float)
windowToWorld gs (mx, my) = (worldX, worldY)
  where
    (V2 (w, h)) = gameDimensions gs
    worldX = mx + w / 2
    worldY = my + h / 2

-- | NUEVO Manejador de Eventos Principal
-- Simplemente pasa los eventos al manejador del juego interno.
eventHandler :: Event -> TournamentState -> TournamentState
eventHandler ev ts =
  let newGameState = eventHandlerGame ev (tsGameState ts)
  in ts { tsGameState = newGameState }

-- | Manejador de eventos específico para 'InGame' (Copiado de Main.hs)  
eventHandlerGame :: Event -> GameState -> GameState
 
eventHandlerGame (EventKey (Char 'f') Down _ _) gs = unsafePerformIO $ do
  -- 'f' ahora reinicia la partida actual, no vuelve al menú
  putStrLn "Partida reiniciada manualmente."
  -- No podemos recargar la config aquí fácilmente, así que solo
  -- reiniciamos el juego con la configuración actual.
  startGameFromConfigs (fst (unV2 (gameDimensions gs))) (snd (unV2 (gameDimensions gs))) (powerUpsEnabled gs) (gsShowHitboxes gs) [] -- ¡Ojo! Pierde los configs
 
eventHandlerGame (EventKey (Char 'r') Down _ _) gs = unsafePerformIO $ do
  let (V2 (w, h)) = gameDimensions gs
      margin = 50.0
  posX <- randomRIO (margin, w - margin)
  posY <- randomRIO (margin, h - margin)
  let pos = v2 posX posY
  case createRobot (1 + length (robots gs)) "RAMMER" HEAVY RAMMER pos 0 0.0 500.0 of 
    Right r -> return (gs { robots = r : robots gs })
    Left _  -> return gs

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

eventHandlerGame (EventKey (MouseButton LeftButton) Down _ _) gs =
    gs { playerWantsToFire = True } 

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
-- Manejador de Actualización Principal (NUEVO)
-------------------------------------------------------------------------------

-- **** MODIFICADO PARA ESTADÍSTICAS ****
updateHandler :: Float -> TournamentState -> TournamentState
updateHandler dt ts =
  let
    config = tsConfig ts
    gs = tsGameState ts
    
    -- 1. Actualizar el estado del juego interno
    gs' = updateGame dt gs
    
    -- 2. Actualizar temporizador del torneo
    timer' = tsTimer ts - 1
    
    -- 3. Comprobar condiciones de fin de partida
    alive = robots gs' -- 'robots' en gs' AHORA solo contiene los vivos
    numAlive = length alive
    
    isTimeUp = timer' <= 0
    isWinner = numAlive <= 1
    
    gamesLeft = tsGamesLeft ts
    
    -- Traer el estado de estadísticas actual
    currentStats = tsStats ts
    
  in
    -- Comprobar si la partida terminó (por tiempo o por ganador)
    if (isTimeUp || isWinner) then
      let
        -- **** INICIO MODIFICACIÓN ESTADÍSTICAS ****
        
        -- 1. RECOLECTAR STATS DE LA RONDA TERMINADA
        --    Pasamos los robots vivos (robots gs') y los muertos (gsDeadRobots gs')
        statsDeEstaRonda = recolectarStatsRonda (robots gs') (gsDeadRobots gs')
        
        -- 2. AGREGARLAS AL TOTAL
        statsNuevas = agregarRonda statsDeEstaRonda currentStats
        
        -- **** FIN MODIFICACIÓN ESTADÍSTICAS ****
        
      in
        if gamesLeft > 1 then
          -- Recargar la siguiente partida
          let
            (w, h) = (cfgWidth config, cfgHeight config)
            -- ¡IMPORTANTE! startGameFromConfigs es IO. 
            -- Usamos unsafePerformIO porque 'play' es un bucle puro.
            newGame = unsafePerformIO $ do 
              putStrLn $ "Partida terminada. " ++ if isTimeUp then "(Tiempo agotado)" else "(Ganador: " ++ (fromMaybe "Empate" (srGanador statsDeEstaRonda)) ++ ")"
              putStrLn $ "Cargando siguiente partida... (" ++ show (gamesLeft - 1) ++ " restantes)"
              startGameFromConfigs w h (cfgPowerUps config) (cfgHitboxes config) (cfgConfigs config) 
          in
            ts { tsGameState = newGame
               , tsGamesLeft = gamesLeft - 1
               , tsTimer = cfgDuration config -- Reiniciar temporizador
               , tsStats = statsNuevas        -- <--- GUARDAR NUEVAS STATS
               }
               
        else
          -- Última partida terminada. Guardar y Salir.
          let
            -- 3. GENERAR REPORTE FINAL (con configuración usada)
            reporteFinal = formatConfigSection config ++ generarReporteAgregado statsNuevas
          in
            unsafePerformIO $ do
              putStrLn "Serie de torneos finalizada."
              
              -- 4. ESCRIBIR EN EL ARCHIVO
              _ <- try (writeFile "estadisticas.txt" reporteFinal) :: IO (Either SomeException ())
              putStrLn "Archivo 'estadisticas.txt' guardado."
              
              exitSuccess 
              return ts -- Necesario para que compile, aunque exitSuccess termina
              
    else
      -- La partida continúa
      ts { tsGameState = gs', tsTimer = timer', tsStats = currentStats } -- <--- Pasar las stats sin modificar
