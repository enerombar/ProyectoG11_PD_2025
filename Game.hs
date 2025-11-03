module Game where
-- Este módulo maneja la creación de entidades y la inicialización del estado del juego.

import Entities
import Math
import qualified Data.Map as Map
import Data.Either (rights)
import Data.Set (Set)
import System.Random (randomRIO, randomIO)
import qualified Data.Set as Set

-- --- Constantes del Juego: Obstáculos ---

numObstacles :: Int
numObstacles = 15 -- Número de obstáculos a generar

obstacleMargin :: Float
obstacleMargin = 70.0 -- Margen mínimo con los bordes del mapa

obstacleMinSeparation :: Float
obstacleMinSeparation = 80.0 -- Separación mínima entre obstáculos

wallAspectRatio :: Float
wallAspectRatio = 786.0 / 257.0 -- wall.png (W/H)

damageZoneRectAspectRatio :: Float
damageZoneRectAspectRatio = 648.0 / 548.0 -- damage_zone_rect.png (W/H)

damageZoneCircularAspectRatio :: Float
damageZoneCircularAspectRatio = 1.0 -- damage_zone.png (se tratará como círculo, W=H)

obstacleRobotMinSeparation :: Float
obstacleRobotMinSeparation = 120.0 -- Separación mínima entre un obstáculo y un robot al inicio

minObstacleSize, maxObstacleSize :: Float
minObstacleSize = 40.0
maxObstacleSize = 100.0

-- --- Constantes del Juego: Power-ups ---

-- Intervalo de 'spawn' (aparición o reubicación) de power-ups, en frames.
powerUpSpawnInterval :: Int 
powerUpSpawnInterval =  300 -- 5 segundos a 60fps

-- Duración de un power-up en el mapa antes de desaparecer/reubicarse, en frames.
powerUpDuration :: Int 
powerUpDuration = 1200 -- 20 segundos a 60fps

-- --- Constructores Seguros de Entidades ---

-- Constructor con comprobaciones para Robot.
-- Devuelve un error (Left) o un Robot (Right).
createRobot :: Int -> String -> RobotType -> IABehavior -> Position -> Angle -> Float -> Float -> Either CreationError Robot
createRobot rid rname rtype behavior pos ang _health radar
    | radar < 0   = Left (NegativeRadar radar) -- El radar no puede ser negativo
    | otherwise   = Right $ Robot
        { robotBase   = base
        , robotVerts  = createRectangleRobot rtype pos ang -- Vértices para colisión
        , robotHealth = maxH
        , robotMaxHealth = maxH
        , radarLength = radar
        , turretAngle = ang -- Ángulo inicial de la torreta
        , robotMem    = Map.empty -- Memoria de IA inicial vacía
        , robotType   = rtype
        , robotBehavior = behavior
        , robotName   = rname
        -- Inicialización de temporizadores
        , robotCooldown = 0
        , robotHitTimer = 0
        , robotWanderTimer = 0
        , robotCollisionTimer = 0
        , robotObstacleCollisionTimer = 0
        , robotReloadTimer = 0
        , robotAmmo = getMagazineSize rtype -- Cargador lleno al inicio
        -- Inicializa los temporizadores de efectos (power-ups)
        , robotSpeedBoostTimer = 0
        , robotShieldTimer = 0
        , robotStuckTimer = 0
        }
    where
      -- Crea el GameObject base con los parámetros
      base = GameObject rid pos (pure 0) ang (getRobotSize rtype)
      -- Obtiene la salud máxima según el tipo de robot
      maxH = getMaxHealth rtype

-- Constructor con comprobaciones para Proyectil.
createProjectile :: GameObject -> Float -> Int -> Int -> Either CreationError Projectile
createProjectile base damage owner lifetime
    | damage <= 0 = Left (InvalidDamage damage) -- El daño no puede ser cero o negativo
    | otherwise   = Right $ Projectile base damage owner lifetime

-- Crea un obstáculo AABB (rectangular, no rotado)
createObstacle :: Int -> ObstacleType -> Position -> Size -> Obstacle
createObstacle oid otype pos size@(V2 (w, h)) =
  let V2 (cx, cy) = pos
      halfW = w / 2
      halfH = h / 2
      -- Vértices AABB centrados en la posición
      verts = [ v2 (cx - halfW) (cy - halfH) -- Arriba-Izquierda
              , v2 (cx + halfW) (cy - halfH) -- Arriba-Derecha
              , v2 (cx + halfW) (cy + halfH) -- Abajo-Derecha
              , v2 (cx - halfW) (cy + halfH) -- Abajo-Izquierda
              ]
  in Obstacle oid otype pos size verts

-- Inserta un robot ya creado en el estado de juego.
addRobot :: Robot -> GameState -> GameState
addRobot r gs = gs { robots = r : robots gs }

-- --- Lógica de Aparición (Spawn) Aleatoria ---

-- Parámetros de 'spawn'
numInitialRobots :: Int
numInitialRobots = 5 -- Número de robots para 'exampleGameState'

spawnMargin :: Float
spawnMargin = 60.0 -- Margen mínimo con el borde del mapa para aparecer

spawnSeparation :: Float
spawnSeparation = 100.0 -- Separación mínima entre robots al aparecer

-- Genera una lista de N posiciones iniciales aleatorias.
-- Es una acción IO (Input/Output) porque necesita generar números aleatorios.
-- Asegura que las posiciones respeten los márgenes y la separación mínima.

generateRandomPositions :: Size          -- Dimensiones del juego
                        -> Int           -- Número de posiciones a generar
                        -> Float         -- Margen con los bordes
                        -> Float         -- Separación mínima entre las nuevas posiciones
                        -> [Position]    -- Posiciones existentes a evitar
                        -> Float         -- Separación mínima con las posiciones existentes
                        -> IO [Position] -- Resultado

generateRandomPositions gameDims numToGen margin minSep existingToAvoid minSepFromExisting = go numToGen []
  where
    (V2 (w, h)) = gameDims
    minX = margin
    maxX = w - margin
    minY = margin
    maxY = h - margin

    -- Genera una posición que no esté cerca ni de las ya generadas en esta tanda, ni de las que pasamos por parámetro
    generateOneValidPosition :: [Position] -> IO Position
    generateOneValidPosition generatedSoFar = do
      x <- randomRIO (minX, maxX)
      y <- randomRIO (minX, maxY)
      let newPos = v2 x y
      -- Comprueba si está demasiado cerca de las generadas en esta tanda
      let tooCloseToSelf = any (\p -> distance newPos p < minSep) generatedSoFar
      -- Comprueba si está demasiado cerca de las que había que evitar (ej: spawns de robots)
      let tooCloseToExisting = any (\p -> distance newPos p < minSepFromExisting) existingToAvoid
      
      if tooCloseToSelf || tooCloseToExisting
        then generateOneValidPosition generatedSoFar -- Reintenta
        else return newPos -- Posición válida

    -- Función auxiliar recursiva
    go :: Int -> [Position] -> IO [Position]
    go 0 acc = return acc -- Caso base: terminamos
    go k acc = do
      newPos <- generateOneValidPosition acc
      go (k-1) (newPos : acc) -- Sigue con k-1

-- --- Funciones de Inicialización del Estado del Juego ---

-- Genera un estado de juego de ejemplo (usado para pruebas rápidas).
-- ¡Nota: Es una acción IO (devuelve 'IO GameState') debido a la aleatoriedad!
exampleGameState :: IO GameState
exampleGameState = do

  -- 1. Definimos las dimensiones y generamos posiciones y ángulos aleatorios
  let (defW, defH) = (800, 600)
      defaultDims = v2 defW defH
  positions <- generateRandomPositions defaultDims numInitialRobots spawnMargin spawnSeparation [] 0.0
  angles <- mapM (\_ -> randomRIO (0, 2 * pi)) [1..numInitialRobots]

  -- 2. Definimos las plantillas de los robots a crear
  let robotDefs =
        [ ("R1", HEAVY, AGGRESSIVE)
        , ("R2", MEDIUM, PEACEFUL)
        ]

  -- 3. Combinamos las plantillas con los datos aleatorios
  let ids = [1..numInitialRobots]
  -- Usamos 'take' por si 'robotDefs' tiene más elementos que 'numInitialRobots'
  let combinedData = zip (zip ids (take numInitialRobots robotDefs)) (zip positions angles)

  -- Creamos los robots usando el constructor 'createRobot'
  -- 'rights' filtra y extrae solo los robots creados exitosamente (ignora los Left)
  let initialRobots = rights $ map
        (\(((idx, (rname, rtype, rbehav)), (pos, ang))) ->
          -- Pasamos 0.0 para salud (se ignora) y 100.0 para radar
          createRobot idx rname rtype rbehav pos ang 0.0 100.0
        ) combinedData

  -- 4. Devolvemos el GameState final "envuelto" en IO (usando 'return')
  return $ GameState
    { robots = initialRobots
    , projectiles = []
    , explosions  = []
    , obstacles = [] -- SIN OBSTÁCULOS EN EL EJEMPLO
    , gameDimensions = defaultDims
    , playerKeys = Set.empty
    , playerTargetTurretAngle = Nothing
    , playerWantsToFire = False
    -- Inicializa el estado de los power-ups
    , powerUpsEnabled = True -- Habilitados por defecto en el ejemplo
    , powerUp = Nothing      -- Sin power-up activo al inicio
    , powerUpSpawnTimer = powerUpSpawnInterval -- Inicia el temporizador para el primer 'spawn'
    , gsShowHitboxes = False
    }

-- Esta función crea el estado de juego inicial basado en la configuración del menú.
-- Recibe el tamaño del mapa, si los power-ups están habilitados, y la lista de 'TankConfig'.
startGameFromConfigs :: Float -> Float -> Bool -> Bool -> [TankConfig] -> IO GameState
startGameFromConfigs width height enablePowerups showHitboxes cfgs = do
  let nRobots = length cfgs
      gameDims = v2 width height
  
  -- 1. Generar posiciones de robots (sin cambios)
  robotPositions <- generateRandomPositions gameDims nRobots spawnMargin spawnSeparation [] 0.0
  robotAngles <- mapM (\_ -> randomRIO (0, 2 * pi)) [1..nRobots]
  let robotIds = [1..nRobots]

  -- 2. Crear los robots (sin cambios)
  let robotsE = [ createRobot idx ("R" ++ show idx) (tcType cfg) (tcBehavior cfg) pos ang 0.0 250.0
                | (idx, cfg, pos, ang) <- zip4 robotIds cfgs robotPositions robotAngles ]
  let robotsOnly = [ r | Right r <- robotsE ]

  -- 3. Generar posiciones, tamaños y tipos de obstáculos
  --    Evitamos las posiciones de los robots para que no spawneen unos encima de otros
  obstaclePositions <- generateRandomPositions gameDims numObstacles obstacleMargin obstacleMinSeparation robotPositions obstacleRobotMinSeparation
  
  -- MODIFICADO: Genera tipos aleatorios
  obstacleTypes <- mapM (\_ -> do 
      r <- randomRIO (0::Float, 1); 
      -- Usamos los nuevos tipos de Entities.hs
      return (if r < 0.45 then WALL
              else if r < 0.60 then DAMAGE_ZONE_RECT
              else if r < 0.75 then DAMAGE_ZONE
              else if r < 0.90 then STORM_ZONE
              else if r < 0.97 then MINA_INACTIVA
              else TORRE_TESLA { teslaRange = 220.0, teslaCooldown = 60, teslaTimer = 0 }) -- Torre Tesla
    ) [1..numObstacles]
    
  -- CAMBIO: Genera tamaños basados en el tipo para mantener el aspect ratio
  obstacleSizes <- mapM (\oType -> do
      -- Genera un ancho aleatorio basado en las constantes
      w <- randomRIO (minObstacleSize, maxObstacleSize)
      -- Calcula el alto basado en el aspect ratio del tipo
      let ratio = case oType of
                    WALL            -> wallAspectRatio
                    DAMAGE_ZONE_RECT-> damageZoneRectAspectRatio
                    DAMAGE_ZONE     -> damageZoneCircularAspectRatio
                    STORM_ZONE      -> damageZoneCircularAspectRatio
                    MINA_INACTIVA   -> damageZoneCircularAspectRatio
                    MINA_ACTIVA _   -> damageZoneCircularAspectRatio
                    TORRE_TESLA{}   -> damageZoneCircularAspectRatio
      let h = w / ratio
      return (v2 w h)
    ) obstacleTypes -- Pasa la lista de tipos generada

  let obstacleIds = [nRobots + 1 .. nRobots + numObstacles]

  -- 4. Crear los obstáculos (sin cambios, createObstacle sigue funcionando)
  let initialObstacles = [ createObstacle oid otype pos size
                         | (oid, otype, pos, size) <- zip4 obstacleIds obstacleTypes obstaclePositions obstacleSizes ]

  -- 5. Devolver el estado de juego inicial (sin cambios)
  return $ GameState
      { robots = robotsOnly
      , projectiles = []
      , explosions = []
      , obstacles = initialObstacles
      , gameDimensions = gameDims
      , playerKeys = Set.empty
      , playerTargetTurretAngle = Nothing
      , playerWantsToFire = False
      , powerUpsEnabled = enablePowerups
      , powerUp = Nothing
      , powerUpSpawnTimer = powerUpSpawnInterval
      , gsShowHitboxes = showHitboxes
      }

-- Función auxiliar para combinar cuatro listas (zip)
zip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds
zip4 _ _ _ _ = []

-- MODIFICADO: Añadidos los casos para las minas
getObstacleAspectRatio :: ObstacleType -> Float
getObstacleAspectRatio oType = case oType of
  WALL -> wallAspectRatio
  DAMAGE_ZONE_RECT -> damageZoneRectAspectRatio
  DAMAGE_ZONE -> damageZoneCircularAspectRatio
  MINA_INACTIVA -> damageZoneCircularAspectRatio -- Tratar como un círculo
  MINA_ACTIVA _ -> damageZoneCircularAspectRatio -- Tratar como un círculo
