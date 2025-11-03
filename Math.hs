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

-- ########## Lógica de Intersección de Línea de Visión ##########

-- Comprueba si el punto q está en el segmento 'pr' (para puntos colineales)
onSegment :: Point -> Point -> Point -> Bool
onSegment (V2 (px, py)) (V2 (qx, qy)) (V2 (rx, ry)) =
    (qx <= max px rx) && (qx >= min px rx) &&
    (qy <= max py ry) && (qy >= min py ry)

-- 0 -> Colineal
-- 1 -> Sentido horario (Clockwise)
-- 2 -> Sentido anti-horario (Counterclockwise)
orientation :: Point -> Point -> Point -> Int
orientation (V2 (px, py)) (V2 (qx, qy)) (V2 (rx, ry)) =
    let val = (qy - py) * (rx - qx) - (qx - px) * (ry - qy)
    in if abs val < 1e-6 -- Tolerancia para floats
       then 0 -- Colineal
       else if val > 0 then 1 else 2 -- Horario o Anti-horario

-- Comprueba si dos segmentos de línea (p1, q1) y (p2, q2) se intersectan.
doIntersect :: Point -> Point -> Point -> Point -> Bool
doIntersect p1 q1 p2 q2 =
    let o1 = orientation p1 q1 p2
        o2 = orientation p1 q1 q2
        o3 = orientation p2 q2 p1
        o4 = orientation p2 q2 q1
    in
        -- Caso General
        (o1 /= o2 && o3 /= o4) ||
        -- Casos Especiales (Colineales)
        (o1 == 0 && onSegment p1 p2 q1) ||
        (o2 == 0 && onSegment p1 q2 q1) ||
        (o3 == 0 && onSegment p2 p1 q2) ||
        (o4 == 0 && onSegment p2 q1 q2)

-- Comprueba si un segmento (p1, p2) se cruza con un rectángulo.
-- Devuelve 'Maybe Bool' para ser consistente con checkCollision.
checkSegmentRectIntersection :: Point -> Point -> Rectangle -> Maybe Bool
checkSegmentRectIntersection p1 p2 rect =
    let
        -- Obtener las 4 aristas del rectángulo
        rectEdges = zip rect (tail rect ++ [head rect])
        
        -- Comprobar si el segmento cruza alguna de las aristas
        crossesEdge = any (\(v1, v2) -> doIntersect p1 p2 v1 v2) rectEdges
        
        -- Comprobar si el segmento está *completamente dentro* del rectángulo
        -- (lo cual no contaría como cruce de arista).
        p1_inside = fromMaybe False (isPointInsideRectangle p1 rect)
        p2_inside = fromMaybe False (isPointInsideRectangle p2 rect)
        
    in
        -- Hay intersección si cruza una arista O si uno de sus puntos está dentro.
        -- (Para LOS, si el objetivo está dentro de un muro, tampoco hay visión)
        Just (crossesEdge || p1_inside || p2_inside)