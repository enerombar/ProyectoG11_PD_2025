# Torneo de Robots en Haskell

Este proyecto es una simulación de un torneo de batalla de robots (bots) 2D, desarrollado íntegramente en Haskell utilizando la biblioteca `gloss` para el renderizado y la gestión del bucle de juego.

En esta arena, múltiples robots controlados por diferentes IA compiten entre sí. Cada robot está equipado con un chasis para moverse, una torreta para apuntar y un radar para detectar enemigos. El objetivo es ser el último robot en pie.

![Imagen de muestra del juego](./assets/screenshot.png)

---

## Índice

- [Características Principales](#características-principales)
- [Dinámicas del Juego](#dinámicas-del-juego)
  - [Tipos de Robots](#tipos-de-robots)
  - [Inteligencia Artificial (IA)](#inteligencia-artificial-ia)
  - [Física y Colisiones](#física-y-colisiones)
  - [Bucle Principal del Juego](#bucle-principal-del-juego)
- [Estructura del Proyecto](#estructura-del-proyecto)
- [Cómo Ejecutar el Juego](#cómo-ejecutar-el-juego)
- [Posibles Mejoras Futuras](#posibles-mejoras-futuras)

---

## Características Principales

* **Renderizado 2D con Gloss:** Utiliza `gloss` para dibujar todos los elementos del juego, incluyendo el mapa de fondo, los robots (con chasis y torreta independientes), barras de vida, nombres, proyectiles y explosiones.
* **Carga de Assets:** Carga imágenes `.png` y `.jpg` para las texturas usando `gloss-juicy`.
* **IA Modular:** Soporta múltiples comportamientos de IA (Agresivo, Defensivo, Equilibrado, Pacífico) que pueden ser asignados a cada robot.
* **Sistema de Físicas:**
    * Movimiento de robots y proyectiles.
    * Detección y respuesta a colisiones con los límites del mundo (rebote/parada).
* **Detección Avanzada de Colisiones:**
    * **Robot-Proyectil:** Detección de impacto para aplicar daño (punto dentro de rectángulo).
    * **Robot-Robot:** Detección mediante el **Teorema de Ejes Separadores (SAT)** (usando `checkCollision`). Esto previene que los robots se atraviesen y aplica daño por embestida.
* **Sistema de Combate:**
    * Los robots tienen salud, velocidad de disparo y daño variables según su tipo.
    * Sistema de *cooldowns* para disparos, recibir daño por colisión y parpadeo por daño.
    * Efecto visual de "parpadeo" (`robotHitTimer`) cuando un robot recibe daño.
    * Los robots generan una explosión visual al ser destruidos.

---

## Dinámicas del Juego

### Tipos de Robots

Existen tres clases de robots, cada una con sus propias estadísticas (definidas en `Entities.hs`):

| Tipo | Salud Máx. | Velocidad | Daño Proyectil | Cooldown Disparo | Rotación Torreta |
| :--- | :---: | :---: | :---: | :---: | :---: |
| **LIGHT** | 80.0 | 65.0 | 4.0 | 30 frames | Rápida (0.1) |
| **MEDIUM** | 100.0 | 50.0 | 9.0 | 50 frames | Media (0.07) |
| **HEAVY** | 120.0 | 35.0 | 16.0 | 70 frames | Lenta (0.03) |

### Inteligencia Artificial (IA)

La IA (definida en `IA.hs`) controla a todos los robots basándose en su comportamiento asignado.

1.  **Detección:** Cada robot usa su `radarLength` para escanear en busca del enemigo más cercano (`findClosestEnemy`).
2.  **Navegación:**
    * **Wander (Deambular):** Si no hay enemigos, el robot deambula (`wanderActions`). Utiliza su memoria (`robotMem`) y un temporizador (`robotWanderTimer`) para decidir cuándo girar.
    * **Evitar Paredes:** Si se acerca demasiado a un borde (`isNearWall`), gira 90 grados para evitarlo.
    * **Evitar Bloqueos:** Usa un "sensor" frontal (`isPathBlocked`) para detectar si otro robot le bloquea el paso y, en ese caso, frena y gira.
3.  **Comportamientos de Combate:**
    * **`AGGRESSIVE`**: Persigue activamente al enemigo, alineando su chasis con él, mientras le dispara.
    * **`BALANCED`**: Se detiene para apuntar y disparar con precisión al enemigo detectado. No lo persigue.
    * **`DEFENSIVE`**: Apunta y dispara, pero si el enemigo se acerca demasiado (menos del 50% de su rango de radar), retrocede mientras dispara.
    * **`PEACEFUL`**: Ignora a los enemigos y solo se dedica a deambular.

### Física y Colisiones

* **Movimiento:** La física es simple. Las acciones de la IA (ej. `MOVE_FORWARD_ACTION`) establecen una velocidad (`objVel`) en el robot. En cada frame, la posición se actualiza `(posición + velocidad * dt)`.
* **Colisiones con Muros:** Se comprueba la "siguiente" posición (`newX_pot`, `newY_pot`). Si está fuera de los límites, la posición se fija en el borde y la velocidad se invierte o corrige.
* **Colisiones Robot-Robot:**
    1.  **Bloqueo:** En `Logic.updatePosition`, antes de moverse, un robot comprueba si su *futura* posición (`potentialVerts`) colisiona con otro robot (`isColliding`). Si es así, su movimiento se cancela y su velocidad se reduce.
    2.  **Daño:** En `Logic.resolveCollision`, se comprueba si dos robots están actualmente solapados. Si es así, y si no están en *cooldown* de colisión, ambos reciben daño proporcional al peso del *otro* robot. Tienen un breve *cooldown* de inmunidad (`robotCollisionTimer`).

### Bucle Principal del Juego

El corazón del juego reside en la función `updateHandler` (en `Main.hs`):

1.  **`decreaseCooldown`**: Reduce todos los contadores (disparo, parpadeo, "wander", inmunidad por colisión).
2.  **`exampleBotActions`**: Llama a la IA (`getAIActions`) para obtener la lista de acciones de todos los robots vivos.
3.  **`applyActions`**: Traduce las acciones (ej. `MOVE_FORWARD_ACTION`, `FIRE_ACTION`) en cambios en el estado del robot (ej. actualizar velocidad, disparar si el *cooldown* es 0).
4.  **`updatePhysics`**: Mueve cada robot (`updatePosition`) y cada proyectil (`updateProjectilePosition`), gestionando las colisiones con muros y el bloqueo entre robots.
5.  **`checkCollisions`**: Detecta *todas* las colisiones (Robot-Proyectil, Robot-Robot, Robot-Explosión) y devuelve una lista de `CollisionEvent`.
6.  **`resolveCollisions`**: Procesa la lista de colisiones, aplicando daño, eliminando proyectiles y activando temporizadores de parpadeo.
7.  **Gestión de Muerte:** Separa a los robots en `livingRobots` y `deadRobots` basándose en `isRobotAlive`.
8.  **`createExplosionFromRobot`**: Crea un objeto `Explosion` por cada robot que acaba de morir.
9.  **Estado Final:** Construye el `GameState` para el siguiente frame, pasando solo los robots vivos y añadiendo las nuevas explosiones.

---

## Estructura del Proyecto

El código está organizado en módulos de Haskell:

* `Main.hs`: Punto de entrada. Define la ventana, carga los *assets* y contiene el bucle `play` de `gloss`, uniendo el renderizado (`drawHandler`) y la lógica (`updateHandler`).
* `Entities.hs`: Define todas las estructuras de datos (`GameState`, `Robot`, `Projectile`, `Action`) y las estadísticas base de los robots.
* `Game.hs`: Define el estado inicial del juego (`exampleGameState`), los constructores (ej. `createRobot`) y las constantes globales (ej. `gameSize`).
* `Math.hs`: Biblioteca de utilidades para matemáticas, vectores (`V2`), ángulos, rotaciones y geometría (proyecciones para SAT).
* `Physics.hs`: Lógica pura de *detección* de colisiones (SAT, punto-en-rectángulo, distancia).
* `Logic.hs`: Lógica de *aplicación* y *resolución*. Contiene las reglas del juego: cómo se aplica una acción, cómo se mueven los objetos, qué pasa *después* de una colisión (daño, etc.), y la gestión de *cooldowns* y explosiones.
* `IA.hs`: El "cerebro" de los robots. Define los diferentes comportamientos, la detección de enemigos y la lógica de navegación (deambular, evitar muros).
* `configuration.cabal`: Archivo de configuración de Cabal para construir el proyecto.
* `assets/`: Carpeta que **debe contener** las imágenes (`mapa.jpg`, `chasis.png`, `torreta.png`).

---

## Cómo Ejecutar el Juego

### Prerrequisitos

1.  Tener instalado el compilador de Haskell (GHC) y la herramienta de gestión de paquetes (Cabal). La forma más fácil es instalar la [Plataforma Haskell](https://www.haskell.org/ghcup/).
2.  Asegurarte de tener los *assets* (imágenes) en la carpeta correcta.

### Pasos para la Ejecución

1.  **Clona el repositorio:**
    ```bash
    git clone [https://github.com/tu-usuario/tu-repositorio.git](https://github.com/tu-usuario/tu-repositorio.git)
    cd tu-repositorio
    ```

2.  **Crea la carpeta `assets`:**
    Asegúrate de que en la raíz del proyecto exista una carpeta llamada `assets` que contenga:
    * `mapa.jpg`
    * `chasis.png`
    * `torreta.png`

3.  **Construye y ejecuta con Cabal:**
    Cabal leerá el archivo `configuration.cabal`, descargará las dependencias (como `gloss` y `gloss-juicy`) y compilará el proyecto.

    ```bash
    # Actualiza la lista de paquetes (recomendado)
    cabal update

    # Instala dependencias y construye el proyecto
    cabal build

    # Ejecuta el juego
    # (El nombre "JuegoTanque" se toma de 'executable' en el .cabal)
    cabal run JuegoTanque
    ```

4.  **Alternativa (GHC directo):**
    Si tienes las bibliotecas `gloss`, `gloss-juicy`, `containers` y `JuicyPixels` instaladas globalmente, puedes compilarlo directamente (menos recomendado para gestión de dependencias):
    ```bash
    ghc --make Main.hs -o JuegoTanque
    ./JuegoTanque
    ```

---

## Posibles Mejoras Futuras

* **Control por Teclado:** Implementar el `eventHandler` (actualmente vacío) para permitir que el jugador controle uno de los robots usando el teclado.
* **Combate por Equipos:** Añadir una propiedad `team` a los robots y modificar la IA (`findClosestEnemy`) para que no ataque a compañeros de equipo y, potencialmente, coordine ataques.
* **Daño por Explosión:** Actualmente las colisiones `ROBOT_EXPLOSION` se detectan pero no se resuelven (la función `resolveCollision` no aplica daño para este caso). Se podría implementar que apliquen daño en área.
* **Fin de Partida:** Detectar cuándo solo queda un robot (o un equipo) y mostrar un mensaje de "Ganador".
* **Puntuación:** Añadir un contador de "kills" o un sistema de puntuación.
* **Power-ups:** Añadir objetos recogibles en el mapa (curaciones, escudos, mejoras de daño).
* **Mejoras de IA:** Implementar IA que trabaje en equipo, que huya si tiene poca vida, o que priorice objetivos débiles.