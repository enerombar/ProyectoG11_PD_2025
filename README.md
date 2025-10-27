# Torneo de Robots en Haskell

Este proyecto es una simulación de un torneo de batalla de robots (bots) 2D, desarrollado íntegramente en Haskell utilizando la biblioteca `gloss` para el renderizado y la gestión del bucle de juego.

En esta arena, múltiples robots controlados por diferentes IA o por un jugador humano compiten entre sí. Cada robot está equipado con un chasis para moverse, una torreta para apuntar y un radar para detectar enemigos. El objetivo es ser el último robot en pie. El juego incluye un menú de configuración para personalizar la partida.

![Imagen de muestra del juego](./assets/screenshot.png)
---

## Índice

- [Características Principales](#características-principales)
- [Dinámicas del Juego](#dinámicas-del-juego)
  - [Tipos de Robots](#tipos-de-robots)
  - [Inteligencia Artificial (IA)](#inteligencia-artificial-ia)
  - [Control del Jugador](#control-del-jugador)
  - [Power-ups](#power-ups)
  - [Física y Colisiones](#física-y-colisiones)
  - [Bucle Principal del Juego](#bucle-principal-del-juego)
- [Estructura del Proyecto](#estructura-del-proyecto)
- [Cómo Ejecutar el Juego](#cómo-ejecutar-el-juego)
- [Posibles Mejoras Futuras](#posibles-mejoras-futuras)

---

## Características Principales

* **Renderizado 2D con Gloss:** Utiliza `gloss` para dibujar todos los elementos: mapa, robots (chasis y torreta), proyectiles, explosiones, power-ups, UI (barras de vida, munición, nombres, controles).
* **Carga de Assets:** Carga imágenes `.png` y `.jpg` usando `gloss-juicy`.
* **Menú Interactivo:** Permite configurar la partida: añadir/eliminar robots (hasta 5), seleccionar tipo de chasis, asignar comportamiento de IA, designar un robot como jugador, activar/desactivar power-ups y cambiar la resolución.
* **Control del Jugador:** Un robot puede ser controlado por el jugador mediante teclado (WASD) y ratón (apuntar/disparar).
* **IA Modular:** Incluye múltiples comportamientos (Agresivo, Equilibrado, Defensivo, Pacífico, Embestidor) y utiliza memoria para recordar la última posición enemiga vista e investigarla.
* **Sistema de Físicas:**
    * Movimiento de robots y proyectiles con velocidad y rotación.
    * Colisiones con los bordes del mundo (rebote).
* **Detección Avanzada de Colisiones (SAT):**
    * **Robot-Proyectil:** Detección precisa para aplicar daño.
    * **Robot-Robot:** Detección mediante el **Teorema de Ejes Separadores (SAT)** para prevenir solapamiento, aplicar daño por colisión (basado en peso y tipo RAMMER) y aplicar físicas de empuje (push-back).
    * **Robot-PowerUp:** Detección por proximidad (radio).
    * **Robot-Explosión:** Detección por radio para aplicar daño en área.
* **Sistema de Combate:**
    * Salud, daño, cadencia de tiro, capacidad del cargador y tiempo de recarga variables según el tipo de robot.
    * Sistema de *cooldowns* (disparo, daño por colisión), recarga de munición y efectos visuales (parpadeo por daño).
    * Explosiones visuales al destruir un robot o impactar un proyectil.
* **Power-ups:** Objetos recolectables que otorgan ventajas temporales (salud, munición extra, velocidad, escudo).
* **Fin de Partida:** Detecta cuándo queda un solo robot o ninguno y muestra una pantalla de "Game Over" con el ganador o indicando empate.

---

## Dinámicas del Juego

### Tipos de Robots

Existen tres clases de robots, cada una con estadísticas distintas (definidas en `Entities.hs`):

| Tipo    | Salud Máx. | Vel. Máx. | Daño Proy. | Cooldown Disparo | Rotación Torreta | Rotación Chasis | Peso | Cargador | Recarga |
| :------ | :--------: | :-------: | :--------: | :--------------: | :--------------: | :-------------: | :--: | :------: | :-----: |
| LIGHT   | 80.0       | 65.0      | 4.0        | 30 frames        | Rápida (0.1)     | Rápida (0.07)   | 1.0  | 10       | 90 fr   |
| MEDIUM  | 100.0      | 50.0      | 9.0        | 50 frames        | Media (0.07)     | Media (0.05)    | 2.5  | 7        | 120 fr  |
| HEAVY   | 120.0      | 35.0      | 16.0       | 70 frames        | Lenta (0.03)     | Lenta (0.02)    | 4.0  | 3        | 180 fr  |

### Inteligencia Artificial (IA)

La IA (definida en `IA.hs`) controla a los robots no jugadores.

1.  **Prioridades:** La IA primero evalúa si ir a por un power-up (`shouldGoForPowerUp`) comparando su distancia con la del enemigo más cercano al objeto.
2.  **Detección:** Usa `radarLength` para buscar al enemigo vivo más cercano (`findClosestEnemy`).
3.  **Memoria e Investigación:** Si un enemigo detectado se pierde de vista, la IA guarda su última posición conocida (`last_seen_pos` en `robotMem`) y se dirige hacia ella (`investigateOrWander`). Si llega cerca sin encontrarlo, olvida la posición y vuelve a deambular.
4.  **Navegación:**
    * **Wander (Deambular):** Si no hay enemigo visible ni posición que investigar, deambula (`wanderActions`), usando memoria y un temporizador para giros periódicos.
    * **Evitar Paredes:** Gira 90º si se acerca a un borde (`isNearWall`).
    * **Movimiento Seguro:** Por defecto avanza (`getSafeMoveAction`), pero la lógica de colisión Robot-Robot en `Logic.hs` puede detenerlo si está bloqueado (excepto si es RAMMER).
5.  **Comportamientos de Combate:**
    * **`AGGRESSIVE`**: Persigue activamente (`actionsToMoveTowards`), apunta y dispara (`actionsToAimAndFire`). Guarda la posición del enemigo si lo ve.
    * **`BALANCED`**: Se detiene, apunta y dispara. No persigue, pero sí investiga si pierde al enemigo.
    * **`DEFENSIVE`**: Apunta y dispara. Si el enemigo está muy cerca (50% del radar), retrocede mientras dispara. Investiga si pierde al enemigo.
    * **`PEACEFUL`**: Deambula o va a por power-ups. Ignora y nunca ataca a los enemigos. Borra la memoria de posiciones.
    * **`RAMMER`**: **Nunca dispara**. Prioriza perseguir enemigos de peso igual o inferior. Si no hay, persigue al más cercano. Guarda su posición para investigar. Ignora el bloqueo de otros robots al moverse.

### Control del Jugador

Si se designa un robot como `PLAYER` en el menú de configuración:

* **W, S:** Mover chasis adelante/atrás.
* **A, D:** Rotar chasis izquierda/derecha.
* **Movimiento del Ratón:** Apuntar la torreta hacia el cursor.
* **Clic Izquierdo:** Disparar (sujeto a cooldown, munición y recarga).

Una UI simple en la esquina inferior derecha recuerda estos controles.

### Power-ups

Si están habilitados en el menú, aparecen periódicamente en el mapa (`Game.hs`, `Logic.hs`).

* **Aparición:** Cada `powerUpSpawnInterval` (10s), un power-up de tipo aleatorio aparece en una posición aleatoria.
* **Duración:** Permanece `powerUpDuration` (15s) en el mapa antes de desaparecer y reiniciar el temporizador de aparición.
* **Recogida:** Un robot lo recoge al pasar cerca (suma de radios).
* **Tipos (`Entities.hs`):**
    * **`Health`**: Restaura `healthPackAmount` (35) de vida (hasta el máximo).
    * **`AmmoBoost`**: Añade `ammoBoostAmount` (5) balas al cargador (hasta 2x tamaño máx.).
    * **`SpeedBoost`**: Aumenta la velocidad máxima por `speedBoostFactor` (x2.0) durante `speedBoostDuration` (7s).
    * **`Shield`**: Otorga invulnerabilidad al daño durante `shieldDuration` (10s).

### Física y Colisiones

Gestionadas principalmente en `Physics.hs` (detección) y `Logic.hs` (resolución).

* **Movimiento:** Actualización simple de posición: `posición + velocidad * dt`. La velocidad se aplica según las acciones (`applyAction`). Se aplica fricción (`speedDec`) si la acción es `STOP_ACTION`.
* **Colisiones con Muros:** Se detectan usando SAT contra rectángulos virtuales en los bordes (`updatePosition`). El robot rebota con velocidad reducida.
* **Colisiones Robot-Robot:**
    1.  **Detección:** Usando SAT (`checkCollision` en `Physics.hs`).
    2.  **Resolución (`resolveCollision` en `Logic.hs`):**
        * **Daño:** Ambos robots reciben daño si no están en cooldown (`robotCollisionTimer`). El daño base (`baseRobotCollisionDamage`) se escala por la proporción de peso del *otro* robot. Los `RAMMER` multiplican el daño que *infligen*.
        * **Empuje (Push-back):** Los robots se separan físicamente una distancia proporcional al solapamiento detectado y al peso del oponente. La posición final se asegura (`clampRobotPosition`) para no salirse del mapa.
        * **Inmunidad:** Se activa un breve cooldown (`robotCollisionCooldown`) para evitar daño múltiple instantáneo.
    3.  **Bloqueo (Prevención):** Antes de moverse, los robots (excepto `RAMMER`) comprueban si su *futura* posición colisionaría (`updatePosition`). Si es así, no se mueven y frenan.
* **Colisiones Robot-Proyectil:**
    * Detección SAT (`Physics.hs`).
    * Resolución (`resolveCollision`): Se aplica `projDamage`. Si el robot tiene escudo (`robotShieldTimer > 0`), el daño se anula. El proyectil se elimina. Se activa el parpadeo (`robotHitTimer`).
* **Colisiones Robot-Explosión:**
    * Detección por radio (`Physics.hs`).
    * Resolución (`resolveCollision`): Se aplica `explosionDamageConstant`. Ignorado si el escudo está activo.
* **Colisiones Robot-PowerUp:**
    * Detección por radio (`Physics.hs`).
    * Resolución (`resolveCollision`): Se aplica el efecto del power-up (`applyPowerUpIfMatch`) y el power-up se elimina del mapa.

### Bucle Principal del Juego

Controlado por `updateHandler` -> `updateGame` en `Main.hs`:

1.  **`decreaseCooldown`**: Reduce todos los contadores (disparo, parpadeo, IA wander, colisión, recarga, power-ups activos, power-up en mapa). Finaliza recargas y elimina explosiones/proyectiles expirados.
2.  **`updatePowerUpSpawning`**: Comprueba si debe aparecer un nuevo power-up.
3.  **`applyPlayerActionsFromState`**: Aplica las acciones derivadas de la entrada del jugador almacenada en `GameState`.
4.  **`getAIActions`**: Obtiene la lista de acciones decididas por la IA para todos los bots.
5.  **`applyActions`**: Ejecuta todas las acciones (jugador y IA), modificando velocidades, ángulos, memoria de IA, iniciando disparos, etc.
6.  **`updatePhysics`**: Mueve robots y proyectiles. Gestiona colisiones con muros (rebote) y expiración de proyectiles (crea explosiones). Previene movimiento si un robot (no RAMMER) chocaría con otro.
7.  **`checkCollisions`**: Detecta *todas* las colisiones ocurridas *después* del movimiento (Robot-Proy, Robot-Robot, Robot-Explosión, Robot-PowerUp).
8.  **`resolveCollisions`**: Procesa las colisiones: aplica daño (considerando escudos), aplica empuje físico (robot-robot), elimina proyectiles/power-ups, activa temporizadores (parpadeo, inmunidad por colisión).
9.  **Gestión de Muerte:** Filtra robots vivos y muertos.
10. **`createExplosionFromRobot`**: Crea explosiones visuales y de daño donde murieron los robots.
11. **Comprobación Fin de Partida:** Si quedan 1 o 0 robots, cambia el `AppState` a `GameOver`.
12. **Estado Final:** Construye el `GameState` para el siguiente frame.

---

## Estructura del Proyecto

* `Main.hs`: Punto de entrada, bucle principal de Gloss, carga de assets, renderizado, manejo de eventos, gestión del estado de la aplicación (Menú/Juego/Game Over).
* `Entities.hs`: Definiciones de tipos de datos (`GameState`, `Robot`, `Projectile`, `PowerUp`, `Action`, etc.) y constantes de estadísticas base.
* `Game.hs`: Lógica de inicialización del juego (`startGameFromConfigs`), constructores seguros (`createRobot`), generación aleatoria de posiciones y power-ups.
* `Math.hs`: Utilidades matemáticas (vectores `V2`, ángulos, rotaciones) y geometría para colisiones (SAT helpers).
* `Physics.hs`: Detección de colisiones (SAT, radio).
* `Logic.hs`: Reglas del juego: aplicación de acciones, actualización de físicas (movimiento, colisión con muros, bloqueo), resolución de colisiones (daño, efectos, empuje), gestión de cooldowns, lógica del jugador y power-ups.
* `IA.hs`: Comportamientos de la IA, detección de enemigos, navegación (wander, evitar muros), toma de decisiones (atacar, investigar, ir por power-ups).
* `juego.cabal`: Archivo de configuración de Cabal para dependencias y construcción.
* `assets/`: Carpeta que **debe contener** las imágenes (`mapa.jpg`, `screenshot.png`) y subcarpetas (`drones/`, `torretas/`, `items/`, `explosion/`) con los respectivos archivos `.png`.

---

## Cómo Ejecutar el Juego

### Prerrequisitos

1.  Tener instalado el compilador de Haskell (GHC) y la herramienta de gestión de paquetes (Cabal). La forma recomendada es usar [GHCup](https://www.haskell.org/ghcup/).
2.  Asegurarte de tener la carpeta `assets` con todas las imágenes necesarias en la raíz del proyecto.

### Pasos para la Ejecución

1.  **Clona el repositorio (o descomprime el proyecto):**
    ```bash
    git clone [URL_DEL_REPOSITORIO]
    cd [NOMBRE_CARPETA_PROYECTO]
    ```

2.  **Verifica la carpeta `assets`:**
    Confirma que la carpeta `assets` existe en la raíz y contiene `mapa.jpg`, `screenshot.png` (opcional, para el README), y las subcarpetas `drones`, `torretas`, `items`, `explosion` con sus respectivas imágenes `.png`.

3.  **Construye y ejecuta con Cabal:**
    Cabal gestionará las dependencias listadas en `juego.cabal` (`gloss`, `gloss-juicy`, `containers`, `random`) y compilará el proyecto.

    ```bash
    # (Opcional) Actualiza la lista de paquetes disponibles
    cabal update

    # Construye el proyecto (descarga dependencias si es necesario)
    cabal build

    # Ejecuta el juego (el nombre 'juego' se toma del .cabal)
    cabal run juego
    ```

4.  **Uso:**
    * Navega por el menú con el ratón.
    * En la pantalla de configuración, añade robots, cambia sus tipos y comportamientos de IA. Marca la casilla "Jugador" para controlar uno.
    * Presiona "Jugar" (necesitas al menos 2 robots).
    * Controla tu robot con WASD y el ratón si lo seleccionaste.
    * Presiona 'F' en cualquier momento para volver al menú principal.
    * Presiona 'R' durante el juego (modo InGame) para añadir un robot RAMMER aleatorio (para pruebas).

---

## Posibles Mejoras Futuras

* **Combate por Equipos:** Añadir propiedad `team` y modificar IA para cooperación/evitar fuego amigo.
* **Mejor UI:** Indicadores más claros para recarga, cooldowns, efectos de power-ups. Minimapa.
* **Sonido:** Añadir efectos de sonido para disparos, explosiones, colisiones, recogida de power-ups y música de fondo.
* **Más Power-ups/Tipos de Robots:** Introducir nuevas mecánicas o variantes de robots.
* **IA Avanzada:** Implementar estrategias más complejas: huida con poca vida, priorización de objetivos, formaciones, uso estratégico de power-ups.
* **Obstáculos en el Mapa:** Añadir paredes u otros elementos en la arena para hacer la navegación más interesante.
* **Guardar/Cargar Configuración:** Permitir guardar las configuraciones de partida del menú.