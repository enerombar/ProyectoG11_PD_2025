-- Fichero: Estadisticas.hs (limpio)

module Estadisticas where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (foldl', sortOn)

import Entities (Robot, robotName, robotImpactosHechos)

-- Estadisticas de un bot dentro de una ronda
data StatsBotRonda = StatsBotRonda
  { sbrImpactos   :: Int
  , sbrEsGanador  :: Bool
  , sbrMurio      :: Bool
  , sbrNombreBot  :: String
  } deriving (Show, Eq)

-- Estadisticas completas de una ronda
data StatsRonda = StatsRonda
  { srNumeroRonda :: Int
  , srGanador     :: Maybe String
  , srStatsBots   :: Map.Map String StatsBotRonda
  } deriving (Show)

-- Estado global de todas las rondas
data StatsGlobales = StatsGlobales
  { sgRondas :: [StatsRonda]
  } deriving (Show)

inicializarStatsGlobales :: StatsGlobales
inicializarStatsGlobales = StatsGlobales { sgRondas = [] }

agregarRonda :: StatsRonda -> StatsGlobales -> StatsGlobales
agregarRonda ronda stats =
  let numRonda    = 1 + length (sgRondas stats)
      rondaConNum = ronda { srNumeroRonda = numRonda }
  in stats { sgRondas = rondaConNum : sgRondas stats }

-- Construye las estadisticas de una ronda terminada
recolectarStatsRonda :: [Robot] -> [Robot] -> StatsRonda
recolectarStatsRonda robotsVivos robotsMuertos =
  let allBotsInRonda = robotsVivos ++ robotsMuertos
      ganador = case robotsVivos of
                  [r] -> Just (robotName r)
                  _   -> Nothing
      muertosNombres = map robotName robotsMuertos
      statsMap = Map.fromList $
        [ ( robotName r
          , StatsBotRonda
              { sbrImpactos   = robotImpactosHechos r
              , sbrEsGanador  = (Just (robotName r) == ganador)
              , sbrMurio      = robotName r `elem` muertosNombres
              , sbrNombreBot  = robotName r
              }
          )
        | r <- allBotsInRonda
        ]
  in StatsRonda
        { srNumeroRonda = 0  -- se rellena en agregarRonda
        , srGanador     = ganador
        , srStatsBots   = statsMap
        }

-- Genera el string final para "estadisticas.txt"
generarReporteAgregado :: StatsGlobales -> String
generarReporteAgregado stats =
  let rondas     = reverse (sgRondas stats)
      numRondas  = length rondas
  in "--- REPORTE FINAL DE TORNEOS --- \n" ++
     "Rondas jugadas: " ++ show numRondas ++ "\n\n" ++
     "--- DETALLE POR RONDA --- \n" ++
     unlines (map reporteRonda rondas) ++ "\n" ++
     "--- ESTADISTICAS AGREGADAS (GLOBALES Y MEDIAS) --- \n" ++
     reporteAgregado (sgRondas stats)

-- Reporte de una ronda
reporteRonda :: StatsRonda -> String
reporteRonda r =
  "  Ronda " ++ show (srNumeroRonda r) ++ ":\n" ++
  "    Ganador: " ++ maybe "Empate" id (srGanador r) ++ "\n" ++
  unlines (map reporteBot (Map.elems (srStatsBots r)))

-- Reporte de un bot en una ronda
reporteBot :: StatsBotRonda -> String
reporteBot s =
  let deaths = if sbrMurio s then 1 else 0
  in  "    - " ++ sbrNombreBot s ++ ":" ++
      " Impactos: " ++ show (sbrImpactos s) ++
      ", KD: " ++ show (sbrImpactos s) ++ "/" ++ show deaths ++
      (if sbrEsGanador s then " (Ganador)"
       else if sbrMurio s then " (Murio)" else " (Sobrevivio)")

-- Acumulado por bot: (impactos totales, tiempo vivo total, victorias totales)
type StatsAgregadosBot = (Int, Int)

-- Reporte agregado (incluye medias)
reporteAgregado :: [StatsRonda] -> String
reporteAgregado [] = "No se jugaron rondas.\n"
reporteAgregado rondas =
  let statsAcumulados = foldl' acumularStatsBots Map.empty rondas
      lineasReporte   = mapMaybe generarLineaAgregada (Map.toList statsAcumulados)
      -- ordenar por victorias desc y luego impactos desc
      lineasOrdenadas = sortOn (\(v, i, _) -> (-v, -i)) lineasReporte
      tablaCompetitiva = generarTablaCompetitiva rondas
  in  "Formato: (Total Victorias / Total Impactos)\n" ++
      unlines (map (\(_, _, s) -> s) lineasOrdenadas) ++
      "\n--- TABLA COMPETITIVA (Agregada) ---\n" ++ tablaCompetitiva

-- Acumulador para el foldl'
acumularStatsBots :: Map.Map String StatsAgregadosBot
                  -> StatsRonda
                  -> Map.Map String StatsAgregadosBot
acumularStatsBots mapaAcumulado ronda =
  foldl' (\acc botStats ->
            let nombre                      = sbrNombreBot botStats
                (impactos, victorias) =
                  fromMaybe (0, 0) (Map.lookup nombre acc)
                nuevosImpactos = impactos + sbrImpactos botStats
                nuevasVictorias = victorias + (if sbrEsGanador botStats then 1 else 0)
            in  Map.insert nombre (nuevosImpactos, nuevasVictorias) acc
         )
         mapaAcumulado
         (Map.elems (srStatsBots ronda))

-- Genera una linea de reporte para el acumulado de un bot
generarLineaAgregada :: (String, StatsAgregadosBot) -> Maybe (Int, Int, String)
generarLineaAgregada (nombre, (totalImpactos, totalVictorias))
  | null nombre = Nothing
  | otherwise   = Just (
      totalVictorias,
      totalImpactos,
      "  - " ++ nombre ++ ": " ++
      show totalVictorias ++ " Victorias | " ++
      show totalImpactos ++ " Impactos Totales"
    )

-- Tabla competitiva agregada basada en impactos (como proxy de kills) y muertes
generarTablaCompetitiva :: [StatsRonda] -> String
generarTablaCompetitiva rondas =
  let acum = foldl' acumRonda Map.empty rondas
      acumRonda m r = foldl' (\acc s ->
                                let n        = sbrNombreBot s
                                    (i,d,w)  = fromMaybe (0,0,0) (Map.lookup n acc)
                                    d'       = d + (if sbrMurio s then 1 else 0)
                                    w'       = w + (if sbrEsGanador s then 1 else 0)
                                    i'       = i + sbrImpactos s
                                in Map.insert n (i', d', w') acc)
                             m (Map.elems (srStatsBots r))
      rows = [ n ++ ": KD " ++ show i ++ "/" ++ show d ++ ", Victorias=" ++ show w
             | (n,(i,d,w)) <- Map.toList acum ]
  in unlines rows
