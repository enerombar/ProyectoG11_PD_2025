module Game where
-- Este módulo maneja la creación de entidades y la inicialización del estado del juego.

import Entities
import Math
import qualified Data.Map as Map
import Data.Either (rights)
import Data.Set (Set)
import System.Random (randomRIO)
import qualified Data.Set as Set

-- --- Constantes del Juego: Power-ups ---

-- Intervalo de 'spawn' (aparición o reubicación) de power-ups, en frames.
powerUpSpawnInterval :: Int 
powerUpSpawnInterval =  600 -- 10 segundos a 60fps

-- Duración de un power-up en el mapa antes de desaparecer/reubicarse, en frames.
powerUpDuration :: Int 
powerUpDuration = 900 -- 15 segundos a 60fps

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
        , robotReloadTimer = 0
        , robotAmmo = getMagazineSize rtype -- Cargador lleno al inicio
        -- Inicializa los temporizadores de efectos (power-ups)
        , robotSpeedBoostTimer = 0
        , robotShieldTimer = 0
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
generateInitialPositions :: Size -> Int -> Float -> Float -> IO [Position]
generateInitialPositions gameDims numRobots margin minSeparation = go numRobots []
  where
    (V2 (w, h)) = gameDims -- Extrae ancho y alto de las dimensiones
    minX = margin
    maxX = w - margin
    minY = margin
    maxY = h - margin

    -- Genera una única posición válida, asegurando que no esté demasiado cerca de las existentes.
    generateOneValidPosition :: [Position] -> IO Position
    generateOneValidPosition existing = do
      -- Genera coordenadas aleatorias dentro de los márgenes
      x <- randomRIO (minX, maxX)
      y <- randomRIO (minY, maxY)
      let newPos = v2 x y
      -- Comprueba si está demasiado cerca de alguna posición ya generada
      let tooClose = any (\p -> distance newPos p < minSeparation) existing
      if tooClose
        then generateOneValidPosition existing -- Si está cerca, reintenta
        else return newPos -- Si no, devuelve la nueva posición

    -- Función auxiliar recursiva para generar 'k' posiciones
    go :: Int -> [Position] -> IO [Position]
    go 0 acc = return acc -- Caso base: se han generado todas (k=0)
    go k acc = do -- Caso recursivo
      newPos <- generateOneValidPosition acc -- Genera una nueva posición válida
      go (k-1) (newPos : acc) -- La añade y sigue con k-1


-- --- Funciones de Inicialización del Estado del Juego ---

-- Genera un estado de juego de ejemplo (usado para pruebas rápidas).
-- ¡Nota: Es una acción IO (devuelve 'IO GameState') debido a la aleatoriedad!
exampleGameState :: IO GameState
exampleGameState = do

  -- 1. Definimos las dimensiones y generamos posiciones y ángulos aleatorios
  let (defW, defH) = (800, 600)
      defaultDims = v2 defW defH
  positions <- generateInitialPositions defaultDims numInitialRobots spawnMargin spawnSeparation
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
    , gameDimensions = defaultDims
    , playerKeys = Set.empty
    , playerTargetTurretAngle = Nothing
    , playerWantsToFire = False
    -- Inicializa el estado de los power-ups
    , powerUpsEnabled = True -- Habilitados por defecto en el ejemplo
    , powerUp = Nothing      -- Sin power-up activo al inicio
    , powerUpSpawnTimer = powerUpSpawnInterval -- Inicia el temporizador para el primer 'spawn'
    }

-- Esta función crea el estado de juego inicial basado en la configuración del menú.
-- Recibe el tamaño del mapa, si los power-ups están habilitados, y la lista de 'TankConfig'.
startGameFromConfigs :: Float -> Float -> Bool -> [TankConfig] -> IO GameState
startGameFromConfigs width height enablePowerups cfgs = do
  let n = length cfgs
      gameDims = v2 width height
  
  -- Genera posiciones y ángulos aleatorios para N robots
  positions <- generateInitialPositions gameDims n spawnMargin spawnSeparation
  angles <- mapM (\_ -> randomRIO (0, 2 * pi)) [1..n]
  let ids = [1..n]

  -- Crea la lista de robots (o errores)
  let robotsE = [ createRobot idx ("R" ++ show idx) (tcType cfg) (tcBehavior cfg) pos ang 0.0 250.0
                | (idx, cfg, pos, ang) <- zip4 ids cfgs positions angles ]
  
  -- Filtra solo los robots creados exitosamente
  let robotsOnly = [ r | Right r <- robotsE ]

  -- Devuelve el estado de juego inicial
  return $ GameState
      { robots = robotsOnly
      , projectiles = []
      , explosions = []
      , gameDimensions = gameDims
      , playerKeys = Set.empty
      , playerTargetTurretAngle = Nothing
      , playerWantsToFire = False
      -- Inicializa el estado de los power-ups usando el parámetro recibido
      , powerUpsEnabled = enablePowerups
      , powerUp = Nothing
      , powerUpSpawnTimer = powerUpSpawnInterval
      }

-- Función auxiliar para combinar cuatro listas (zip)
zip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds
zip4 _ _ _ _ = []