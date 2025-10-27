module Math where

import Entities
import Data.Maybe
import Control.Applicative (liftA2) -- Para operaciones 'lift' sobre V2 (Applicative)

-- ########## Conversiones de Ángulos ##########

-- Grados a radianes
deg2rad :: Float -> Float
deg2rad deg = deg * pi / 180

-- Radianes a grados
rad2deg :: Float -> Float
rad2deg rad = rad * 180 / pi

-- ########## Operaciones Vectoriales (V2) ##########

-- Vector de dirección (unitario) a partir de un ángulo en radianes
dirFromAngle :: Angle -> Vector
dirFromAngle ang = v2 (cos ang) (sin ang)

-- Producto escalar de dos vectores
dot :: Vector -> Vector -> Float
dot (V2 (x1, y1)) (V2 (x2, y2)) = x1 * x2 + y1 * y2

-- Suma de vectores (usando liftA2 para sumar los contenidos de V2)
addV :: Vector -> Vector -> Vector
addV = liftA2 (+) 

-- Resta de vectores (usando liftA2)
subV :: Vector -> Vector -> Vector
subV = liftA2 (-) 

-- Escalado de un vector por un escalar (usando fmap)
scaleV :: Float -> Vector -> Vector
scaleV s = fmap (*s) 

-- División de un vector por un escalar (usando fmap)
divV :: Vector -> Float -> Vector
divV v s = fmap (/s) v 

-- Vector perpendicular (rotación de 90 grados anti-horario)
perp :: Vector -> Vector
perp (V2 (vx, vy)) = v2 (-vy) vx

-- Norma (magnitud o longitud) euclídea de un vector
norm :: Vector -> Float
norm (V2 (x, y)) = sqrt (x * x + y * y)

-- Normaliza un vector (lo convierte en vector unitario)
-- Devuelve Nothing si el vector es (0,0) para evitar división por cero.
normalize :: Vector -> Maybe Vector
normalize v
  | mag == 0  = Nothing
  | otherwise = Just $ fmap (/ mag) v 
  where mag = norm v

-- Distancia euclídea entre dos puntos
distance :: Point -> Point -> Float
distance p1 p2 = norm (subV p1 p2)

-- ########## Geometría y Colisiones ##########

-- Comprueba si dos rangos (proyecciones) 1D se solapan
overlap :: (Float, Float) -> (Float, Float) -> Bool
overlap (minA,maxA) (minB,maxB) = not (maxA < minB || maxB < minA)

-- Ángulo (en radianes) desde el punto 1 hacia el punto 2
angleToTarget :: Point -> Point -> Angle
angleToTarget (V2 (x1, y1)) (V2 (x2, y2)) = atan2 (y2 - y1) (x2 - x1)

-- Calcula el centro geométrico (promedio) de una lista de puntos
center :: [Point] -> Maybe Point
center [] = Nothing
center pts = Just $ fmap (/ n) s
  where
    n = fromIntegral (length pts)
    s = foldl addV (pure 0) pts -- Suma todos los puntos

-- Rota un punto alrededor de un punto central dado un ángulo
rotatePoint :: Point -> Point -> Float -> Point
rotatePoint (V2 (cx, cy)) (V2 (x, y)) angle =
  let dx = x - cx
      dy = y - cy
      cosA = cos angle
      sinA = sin angle
  in v2 (cx + dx * cosA - dy * sinA) (cy + dx * sinA + dy * cosA)

-- Rota todos los vértices de un rectángulo alrededor de un punto central
rotateRectangle :: Rectangle -> Point -> Angle -> Rectangle
rotateRectangle rect center angle =
  map (\v -> rotatePoint center v angle) rect

-- Verifica si un punto está dentro de un rectángulo delimitador (AABB - Axis-Aligned Bounding Box)
isInBounds :: Point -> Point -> Point -> Bool
isInBounds (V2 (x, y)) (V2 (minX, minY)) (V2 (maxX, maxY)) =
  x >= minX && x <= maxX && y >= minY && y <= maxY

-- Traslada (mueve) una lista de puntos por un vector de desplazamiento
translatePoints :: [Point] -> Vector -> [Point]
translatePoints pts d = map (`addV` d) pts 

-- Proyección de un rectángulo (lista de vértices) sobre un eje (vector)
-- Devuelve el rango (mínimo, máximo) de la proyección
project :: Rectangle -> Vector -> (Float, Float)
project verts axis = (minimum scalars, maximum scalars)
  where scalars = map (`dot` axis) verts

-- Crea el rectángulo de colisión (hitbox) para un robot.
-- Es ligeramente más pequeño (95%) que el tamaño visual para una mejor sensación de juego.
createRectangleRobot :: RobotType -> Point -> Angle -> Rectangle
createRectangleRobot t centerPt angle =
  rotateRectangle
    -- 1. Crear rectángulo AABB en el origen (0,0) pero con tamaño reducido
    [ v2 (cx - hw') (cy - hh') -- Vértice superior izquierdo
    , v2 (cx + hw') (cy - hh') -- Vértice superior derecho
    , v2 (cx + hw') (cy + hh') -- Vértice inferior derecho
    , v2 (cx - hw') (cy + hh') -- Vértice inferior izquierdo
    ]
    centerPt -- 2. Trasladar al centro
    angle    -- 3. Rotar
  where
    (V2 (cx, cy)) = centerPt
    (V2 (w, h))   = getRobotSize t -- Tamaño visual/base del robot
    shrinkFactor  = 0.95 -- Factor de reducción para el hitbox
    hw' = (w * 0.5) * shrinkFactor -- Mitad del ancho reducido
    hh' = (h * 0.5) * shrinkFactor -- Mitad del alto reducido


-- Devuelve las aristas (vectores) de un polígono (rectángulo)
edges :: Rectangle -> [Vector]
edges verts = zipWith subV (tail verts ++ [head verts]) verts 

-- Devuelve los ejes normales únicos (perpendiculares a las aristas) de un rectángulo.
-- Usado para el Teorema de Ejes Separadores (SAT).
uniqueAxes :: Rectangle -> Maybe [Vector]
uniqueAxes verts = sequence (map normalize unique)
  where
    -- Genera las aristas del rectángulo
    es = edges verts
    -- Crea los vectores normales (perpendiculares)
    normals = map (\(V2 (x,y)) -> v2 (-y) x) es
    
    -- Elimina ejes duplicados (p.ej., (0,1) y (0,-1) son paralelos)
    -- Un rectángulo solo tiene 2 ejes únicos.
    unique = take 2 $ foldr (\n acc -> if not (any (isParallel n) acc) then n:acc else acc) [] normals

    -- Comprueba si dos vectores son paralelos (producto escalar cercano a 1 o -1)
    isParallel (V2 (x1,y1)) (V2 (x2,y2)) =
      abs (x1*x2 + y1*y2) > 0.999  -- cos(0°) ≈ 1 o cos(180°) ≈ -1


-- Comprueba si un punto está dentro de un rectángulo (polígono convexo) usando SAT.
isPointInsideRectangle :: Point -> Rectangle -> Maybe Bool
isPointInsideRectangle pto rect = do
    axes <- uniqueAxes rect -- Obtener los ejes normales del rectángulo
    return (all axisOverlap axes) -- Comprobar si el punto se solapa en *todos* los ejes
  where
    axisOverlap axis = overlap (project rect axis) (project [pto] axis)

-- Normaliza un ángulo para que esté en el rango [-pi, pi]
-- Esencial para evitar vueltas completas y encontrar el camino más corto.
normalizeAngle :: Angle -> Angle
normalizeAngle ang = atan2 (sin ang) (cos ang)