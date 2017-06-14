import Datos

--1. Que dados dos agentes, indique cu´al es su relaci´on.
relacion :: Relaciones -> Agente -> Agente -> Float
relacion relaciones agenteN agenteM = relacionAgenteNM (relacionesAgenteN relaciones agenteN 1) agenteM 1

--Auxiliares:
--Recibe el agente y devuelve la fila de sus relaciones.
relacionesAgenteN :: Relaciones -> Agente -> Integer -> Set Relacion
relacionesAgenteN relaciones agenteN pos | pos == agenteN = head relaciones
                                           | pos < agenteN = relacionesAgenteN (tail relaciones) agenteN (pos+1)

--Recibe una lista de relaciones de un agente y devuelve su relacion con otro agente
relacionAgenteNM :: Set Relacion -> Agente -> Integer -> Relacion
relacionAgenteNM relacionesN agente pos | pos == agente = head relacionesN
                                        | pos < agente = relacionAgenteNM (tail relacionesN) agente (pos+1)

--2. Que dado un agente, el n´umero total de agentes del sistema y un estado determinado, indique 
--el conjunto de agentes enemigos.
enemigos :: Agente -> Integer -> Estado -> Set Agente
enemigos agente cantidadAgentes estado | not (agentePertenece estado agente) = estado
                                       | otherwise = quitarAgentes agentes estado
                                         where agentes = [1..cantidadAgentes]
                 
--Auxiliares:
--Recibe una lista de agentes y un agente, y devuelve si el agente pertence o no a la lista
agentePertenece :: Set Agente -> Agente -> Bool
agentePertenece [] _ = False
agentePertenece estado agente | head estado == agente = True
                              | otherwise = agentePertenece (tail estado) agente

--Recibe dos listas de agentes y devuelve la resta de ambas
quitarAgentes :: Set Agente -> Set Agente -> Set Agente
quitarAgentes [] _ = []
quitarAgentes (headAgentes:tailAgentes) estado | not (agentePertenece estado headAgentes) = headAgentes : quitarAgentes tailAgentes estado
                             | otherwise = quitarAgentes tailAgentes estado

--3. Que dado un agente, devuelve su frustracion que es la suma de los valores de su relación con cada uno de los
--agentes del otro bando.
frustracion :: Agente -> Relaciones -> Estado -> Frustracion
frustracion agente relaciones estado = sumatoriaFrustracion (relacionesAgenteN  relaciones agente 1) (enemigos agente (cantAgentes) estado)
                                       where cantAgentes = toInteger (length relaciones)

--Auxiliares:
--Recibe las relaciones de un agente y sus enemigos y suma la relacion con cada uno de ellos
sumatoriaFrustracion :: Set Relacion -> Estado -> Frustracion
sumatoriaFrustracion relacionesN [] = 0
sumatoriaFrustracion relacionesN enemigos = relacionAgenteNM relacionesN (head enemigos) 1 + sumatoriaFrustracion relacionesN (tail enemigos)

--4. Que dado las relaciones y un estado, devuelve la energia total del sistema, es decir, la suma de las frustraciones
--de cada agente
--Como las relaciones son simétricas, la suma de frustraciones de todos los agentes de un bando es igual a la suma de 
-- frustraciones de los agentes del otro. Luego, calculamos una y la duplicamos para la enemigos total
energia :: Relaciones -> Estado -> Energia
energia relaciones estado = 2 * (energiaUnBando relaciones estado estado)

--Auxiliares:
--Recibe la matriz de relaciones, un estado y la lista de agentes del estado sobre la cual hacemos la recursión y
-- devuelve la suma de las frustraciones de todos los agentes del estado.
energiaUnBando :: Relaciones -> Estado -> Set Agente -> Energia
energiaUnBando _ _ [] = 0
energiaUnBando relaciones estado (headAgentes:tailAgentes) = frustracion headAgentes relaciones estado + energiaUnBando relaciones estado tailAgentes 

--5. Que dado un agente y un estado, cambia al agente de bando
adyacente :: Agente -> Estado -> Estado
adyacente agente estado | agentePertenece estado agente = quitarUnAgente agente estado 
                        | otherwise = agente : estado

--No se si es mas eficiente la funcion 1 o la 2, porque:
-- en la 1 uso el pertence entonces recorro toda la lista para ver si pertenece o no y si pertenece vuelvo a recorrer la lista para quitarlo
-- y en la dos, recorro la lista una sola vez para quitarlos (pertenezca o no) y desp comparo el largo de la lista filtrada con la lista que tenia
-- si el largo es igual es porque no pertenecia y entonces lo agrego pero no se como funciona la funcion length de haskell asi que no se si evaluar el length es mas rapido... se los dejo
adyacente2 :: Agente -> Estado -> Estado
adyacente2 agente estado | length estadoSinAgente == length estado = agente : estado
                         | otherwise = estadoSinAgente 
                          where estadoSinAgente = (quitarUnAgente agente estado)

--Auxiliares:
--Recibe un agente y un estado y devuelve el estado sin ese agente
quitarUnAgente :: Agente -> Estado -> Estado
quitarUnAgente agente [] = []
quitarUnAgente agente (headEstado:tailEstado) | headEstado /= agente = headEstado : quitarUnAgente agente tailEstado
                                              | otherwise = tailEstado

esEstable :: Relaciones -> Estado -> Bool
esEstable = undefined

estadosPosibles :: Integer -> Set (Set Integer)
estadosPosibles = undefined

predicciones :: Relaciones -> [(Estado, Energia)]
predicciones = undefined




--Agunas funciones extra:
--2. Que dado un agente, el n´umero total de agentes del sistema y un estado ORDENADO, indique 
--el conjunto de agentes enemigos.
enemigosConOrden :: Agente -> Integer -> Estado -> Set Agente
enemigosConOrden agente cantidadAgentes estado | agentePertenece estado agente = agentesEnemigos estado agentes
                                       | otherwise = estado
                                         where agentes = [1..cantidadAgentes]
--Si estan ordenados!!!

--Auxiliares:
--Recibe un estado y devuelve el otro bando
agentesEnemigos :: Estado -> Set Agente -> Estado
agentesEnemigos _ [] = []
agentesEnemigos [] agentes = agentes
agentesEnemigos estado agentes | head estado == head agentes = agentesEnemigos (tail estado)  (tail agentes)
                               | otherwise = [head agentes] ++ agentesEnemigos estado (tail agentes)

--Nos parecio interesante agregar esta funcion porque:
--si el estado esta ordenado de menor a mayor esta funcion devuelve los enemigos mucho mas eficientemente