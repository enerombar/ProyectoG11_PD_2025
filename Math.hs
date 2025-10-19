module Math where

import Entities
import Data.Maybe
import Control.Applicative (liftA2)

-- Conversiones de ángulos
deg2rad :: Double -> Double
deg2rad deg = deg * pi / 180

rad2deg :: Double -> Double
rad2deg rad = rad * 180 / pi

-- Dirección a partir del ángulo
dirFromAngle :: Angle -> Vector
dirFromAngle ang = v2 (cos ang) (sin ang)

-- Producto punto (adaptado para V2)
dot :: Vector -> Vector -> Double
dot (V2 (x1, y1)) (V2 (x2, y2)) = x1 * x2 + y1 * y2

-- Resta de dos vectores usando Applicative
subVec :: Vector -> Vector -> Vector
subVec = liftA2 (-)
-- Alternativa sin liftA2: v1 v2 = (-) <$> v1 <*> v2 Menos legible 
--Otra alternativa: v1 v2 = pure (-) <*> v1 <*> v2

-- Vector perpendicular
perp :: Vector -> Vector
perp (V2 (vx, vy)) = v2 (-vy) vx

-- Escala un vector usando Functor
scaleVec :: Vector -> Double -> Vector
scaleVec v s = fmap (* s) v

-- Norma euclídea
norm :: Vector -> Double
norm (V2 (x, y)) = sqrt (x * x + y * y)

-- Normaliza un vector
normalize :: Vector -> Maybe Vector
normalize v
  | mag == 0  = Nothing
  | otherwise = Just $ fmap (/ mag) v
  where mag = norm v

-- Comprobar si dos rangos se solapan
overlap :: (Double, Double) -> (Double, Double) -> Bool
overlap (minA,maxA) (minB,maxB) = not (maxA < minB || maxB < minA)

-- Distancia euclídea entre dos puntos
distance :: Point -> Point -> Double
distance p1 p2 = norm (subVec p1 p2)

-- Ángulo entre dos puntos
angleToTarget :: Point -> Point -> Angle
angleToTarget (V2 (x1, y1)) (V2 (x2, y2)) = atan2 (y2 - y1) (x2 - x1)

-- Centro de una lista de puntos
center :: [Point] -> Maybe Point
center [] = Nothing
center pts = Just $ fmap (/ n) s
  where
    n = fromIntegral (length pts)
    s = foldl (liftA2 (+)) (pure 0) pts

-- Rota un punto alrededor de un centro
rotatePoint :: Point -> Point -> Double -> Point
rotatePoint (V2 (cx, cy)) (V2 (x, y)) angle =
  let dx = x - cx
      dy = y - cy
      cosA = cos angle
      sinA = sin angle
  in v2 (cx + dx * cosA - dy * sinA) (cy + dx * sinA + dy * cosA)

-- Rotar un rectángulo 
rotateRectangle :: Rectangle -> Point -> Angle -> Rectangle
rotateRectangle rect pto angle = map (\vert -> rotatePoint pto vert angle) rect --Sobre listas fmap = map y Rectangle = [Point]

-- Verifica si un punto está dentro de los límites
isInBounds :: Point -> Point -> Point -> Bool
isInBounds (V2 (x, y)) (V2 (minX, minY)) (V2 (maxX, maxY)) =
  x >= minX && x <= maxX && y >= minY && y <= maxY

-- Traslada una lista de puntos por un vector 
translatePoints :: [Point] -> Vector -> [Point]
translatePoints pts d = map (\p -> liftA2 (+) p d) pts --Sobre listas fmap = map

-- Proyección de un rectángulo sobre un eje
project :: Rectangle -> Vector -> (Double, Double)
project verts axis = (minimum scalars, maximum scalars)
  where scalars = map (`dot` axis) verts

-- Crea los vértices del robot
createRectangleRobot :: RobotType -> Point -> Angle -> Rectangle
createRectangleRobot t centerPt angle =
  rotateRectangle
    [ v2 (cx - hw) (cy - hh)
    , v2 (cx + hw) (cy - hh)
    , v2 (cx + hw) (cy + hh)
    , v2 (cx - hw) (cy + hh)
    ]
    centerPt
    angle
  where
    (V2 (cx, cy)) = centerPt
    (V2 (w, h))   = getRobotSize t
    hw = w / 2
    hh = h / 2

-- Devuelve las aristas de un rectángulo
edges :: Rectangle -> [Vector]
edges verts = zipWith subVec (tail verts ++ [head verts]) verts

-- Devuelve las normales
uniqueAxes :: Rectangle -> Maybe [Vector]
uniqueAxes verts = sequence (map normalize (take 2 (edges verts)))--Sequence comprueba que no haya ningún Nothing y si lo hay devuelve Nothing de todo

-- Comprueba si un punto está dentro de un rectángulo
isPointInsideRectangle :: Point -> Rectangle -> Maybe Bool
isPointInsideRectangle pto rect = do
    axes <- uniqueAxes rect
    return (all axisOverlap axes)
  where
    axisOverlap axis = overlap (project rect axis) (project [pto] axis)