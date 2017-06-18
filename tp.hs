import Datos

--1. Que dados dos agentes, indique cual es su relacion.
relacion :: Relaciones -> Agente -> Agente -> Float
relacion relaciones agenteN agenteM = relacionAgenteNM (relacionesAgenteN relaciones agenteN) agenteM

--Auxiliares:
--Dado el agente devuelve la fila de sus relaciones.
relacionesAgenteN :: Relaciones -> Agente -> Set Relacion
relacionesAgenteN relaciones agente = relacionesFilaN relaciones agente 1

relacionesFilaN :: Relaciones -> Agente -> Integer -> Set Relacion
relacionesFilaN relaciones agenteN pos | pos == agenteN = head relaciones
                                       | pos < agenteN = relacionesFilaN (tail relaciones) agenteN (pos+1)

--Dada una lista de relaciones de un agente devuelve su relacion con otro agente
relacionAgenteNM :: Set Relacion -> Agente -> Relacion
relacionAgenteNM relaciones agente = relacionesPosicionNM relaciones agente 1

relacionesPosicionNM :: Set Relacion -> Agente -> Integer -> Relacion
relacionesPosicionNM relacionesN agente pos | pos == agente = head relacionesN
                                            | pos < agente = relacionesPosicionNM (tail relacionesN) agente (pos+1)


--2. Que dado un agente, el nuumero total de agentes del sistema y un estado determinado, indique
--el conjunto de agentes enemigos.
enemigos :: Agente -> Integer -> Estado -> Set Agente
enemigos agente cantidadAgentes estado | agentePertenece estado agente = quitarAgentes agentes estado
                                       | otherwise = estado
                                         where agentes = [1..cantidadAgentes]

--Auxiliares:
--Dada una lista de agentes y un agente, indica si el agente pertence o no a la lista
agentePertenece :: Set Agente -> Agente -> Bool
agentePertenece [] _ = False
agentePertenece (headEstado:tailEstado) agente | headEstado == agente = True
                                               | otherwise = agentePertenece (tailEstado) agente

--Recibe dos listas de agentes y devuelve la resta de ambas
quitarAgentes :: Set Agente -> Set Agente -> Set Agente
quitarAgentes [] _ = []
quitarAgentes (headAgentes:tailAgentes) estado | agentePertenece estado headAgentes = quitarAgentes tailAgentes estado
                                               | otherwise = headAgentes : quitarAgentes tailAgentes estado


--3. Que dado un agente, devuelve su frustracion que es la suma de los valores de su relación con cada uno de los
--agentes del otro bando.
frustracion :: Agente -> Relaciones -> Estado -> Frustracion
frustracion agente relaciones estado = sumatoriaFrustracion (relacionesAgenteN  relaciones agente) (enemigos agente (cantAgentes) estado)
                                       where cantAgentes = toInteger (length relaciones)

--Auxiliares:
--Dadas las relaciones de un agente y sus enemigos, suma la relacion con cada uno de ellos
sumatoriaFrustracion :: Set Relacion -> Set Agente -> Frustracion
sumatoriaFrustracion relacionesN [] = 0
sumatoriaFrustracion relacionesN enemigos = relacionAgenteNM relacionesN (head enemigos) + sumatoriaFrustracion relacionesN (tail enemigos)


--4. Que dado las relaciones y un estado, devuelve la energia total del sistema, es decir, la suma de las frustraciones
--de cada agente
--Como las relaciones son simétricas, la suma de frustraciones de todos los agentes de un bando es igual a la suma de
-- frustraciones de los agentes del otro. Luego, calculamos una y la duplicamos para la enemigos total
energia :: Relaciones -> Estado -> Energia
energia relaciones estado = 2 * (energiaUnBando relaciones estado)

--Auxiliares:
--Dada una matriz de relaciones, un estado y la lista de agentes del estado devuelve la suma de las frustraciones de todos los agentes del estado.
energiaUnBando :: Relaciones -> Estado -> Energia
energiaUnBando relaciones estado = energiaUnBandoRecursion relaciones estado estado

energiaUnBandoRecursion :: Relaciones -> Estado -> Set Agente -> Energia
energiaUnBandoRecursion _ _ [] = 0
energiaUnBandoRecursion relaciones estado (headAgentes:tailAgentes) = frustracion headAgentes relaciones estado + energiaUnBandoRecursion relaciones estado tailAgentes


--5. Que dado un agente y un estado, cambia al agente de bando
adyacente :: Agente -> Estado -> Estado
adyacente agente estado | agentePertenece estado agente = quitarUnAgente agente estado
                        | otherwise = agente : estado

--Auxiliares:
--Dado un agente y un estado, devuelve el estado sin ese agente
quitarUnAgente :: Agente -> Estado -> Estado
quitarUnAgente agente [] = []
quitarUnAgente agente (headEstado:tailEstado) | headEstado /= agente = headEstado : quitarUnAgente agente tailEstado
                                              | otherwise = tailEstado


--6.Dadas las relaciones t un estado , te dice si ese estado es estable o no
esEstable :: Relaciones -> Estado -> Bool
esEstable _ []= False
esEstable relaciones estado | energia relaciones estado <= estadoAdMenorEnergia relaciones estado adyacentes = True
                            | otherwise = False
                              where adyacentes = todosLosAdyacentes estado

--Auxiliares
--Dado un estado, devuelve todos los estados adyacentes de ese estado
todosLosAdyacentes :: Estado -> Set Estado
todosLosAdyacentes estado = todosLosAdyacentesRecursion estado estado

todosLosAdyacentesRecursion :: Estado -> Estado -> Set Estado
todosLosAdyacentesRecursion [] _ = []
todosLosAdyacentesRecursion estado estadoFijo = adyacente (head estado) estadoFijo : todosLosAdyacentesRecursion (tail estado) estadoFijo

--Dadas las relaciones, el estado y los estados adyacentes, devuelve la energia del estado con menor energia
estadoAdMenorEnergia :: Relaciones -> Estado -> Set Estado -> Energia
estadoAdMenorEnergia relaciones estado adyacentes | length adyacentes == 0 = energia relaciones estado
                                                  | energia relaciones estado <= energia relaciones headAd = estadoAdMenorEnergia relaciones estado tailAd
                                                  | otherwise = estadoAdMenorEnergia relaciones headAd tailAd
                                                    where headAd = head adyacentes
                                                          tailAd = tail adyacentes


--7. Dado el nuumero de agentes del sistema, indica las posibles formas de formar bandos sin repetir estados indistinguibles.
estadosPosibles :: Integer -> Set Estado
estadosPosibles 1 = [[1]]
estadosPosibles cantidadAgentes = (estadosCantMenos1) ++ (agregarAgenteAEstados cantidadAgentes estadosCantMenos1)
                                  where estadosCantMenos1 = estadosPosibles (cantidadAgentes-1)

--Auxiliares
--Recibe un agente y una lista de estados, devuelve la lista con ese agente en cada estado.
agregarAgenteAEstados:: Agente -> Set Estado -> Set Estado
agregarAgenteAEstados _ [] = []
agregarAgenteAEstados cantidadAgentes (headEstados:tailEstados) = (headEstados ++ [cantidadAgentes]) : (agregarAgenteAEstados cantidadAgentes tailEstados)


--8.Que enumera todos los estados estables junto con su energia.
predicciones :: Relaciones -> [(Estado, Energia)]
predicciones [[]] = []
predicciones relaciones = auxPredicciones relaciones (estadosPosibles cantidadAgentes)
                           where cantidadAgentes = toInteger (length (head relaciones))

--Auxiliare de predicciones
auxPredicciones :: Relaciones -> Set Estado -> [(Estado,Energia)]
auxPredicciones _ [] = []
auxPredicciones relaciones (headEstadosPosibles:tailEstadosPosibles) | esEstable relaciones headEstadosPosibles = [(headEstadosPosibles, energia relaciones headEstadosPosibles)] ++ siguientePrediccion
                                                                     | otherwise = siguientePrediccion
                                                                       where siguientePrediccion = auxPredicciones relaciones tailEstadosPosibles




--Otras soluciones:
--Nos parecio interesante dejar otras soluciones que encontramos para los mismos probelmas, y algunas con un pequeño comentario
esEstable2 :: Relaciones -> Estado -> Bool
esEstable2 relaciones estado = menorEnergia2 relaciones estado (toInteger (length relaciones))

menorEnergia2 :: Relaciones -> Estado -> Integer -> Bool
menorEnergia2 _ _ 0 = True
menorEnergia2 relaciones estado cantidadAgentes | energia relaciones estado > energia relaciones (adyacente cantidadAgentes estado) = False
                                                | otherwise = menorEnergia2 relaciones estado (cantidadAgentes - 1)

esEstable3 :: Relaciones -> Estado -> Bool
esEstable3 _ []=False
esEstable3 relaciones estado | energia relaciones estado <= menorEnergia3 relaciones estado (energia relaciones estado) estado = True
                             | otherwise = False

menorEnergia3 :: Relaciones -> Estado -> Energia -> Estado -> Energia
menorEnergia3 relaciones estado energiaEstado estado2 | length estado == 1 && enerAdyacente <= energiaEstado = enerAdyacente
                                                      | enerAdyacente <= energiaEstado = menorEnergia3 relaciones (tail estado) enerAdyacente estado2
                                                      | otherwise = energiaEstado
                                                        where enerAdyacente = energia relaciones (adyacente (head estado) estado2)

enemigosConOrden :: Agente -> Integer -> Estado -> Set Agente
enemigosConOrden agente cantidadAgentes estado | agentePertenece estado agente = agentesEnemigos estado agentes
                                               | otherwise = estado
                                                 where agentes = [1..cantidadAgentes]
--Es mucho mas eficiente si los agentes estan ordenados!!!
agentesEnemigos :: Estado -> Set Agente -> Estado
agentesEnemigos _ [] = []
agentesEnemigos [] agentes = agentes
agentesEnemigos estado agentes | head estado == head agentes = agentesEnemigos (tail estado)  (tail agentes)
                               | otherwise = [head agentes] ++ agentesEnemigos estado (tail agentes)

adyacente2 :: Agente -> Estado -> Estado
adyacente2 agente estado | length estadoSinAgente == length estado = agente : estado
                         | otherwise = estadoSinAgente
                          where estadoSinAgente = (quitarUnAgente agente estado)

--No sabemos si es mas eficiente la funcion 1 o la 2:
--1) Recorremos todos los agentes una vez para ver si pertenece, y en el peor de los casos por segunda vez para quitarlo.
--2) Recorremos todos los agentes una vezpara quitarlo, y comparamos con la funcion length
--No sabemos que procedimiento usa el length pero: si los recorre => los recorre 3 veces => la 1 es mas optima. Si no hay que ver como funciona.

-- la opcional:

nombreAgente :: Agente -> [String] -> Integer -> String

nombreAgente agente nombres p | agente == p = head nombres
                              | otherwise = nombreAgente agente (tail nombres) (p+1)


--
nombreTodosLosAgentes:: Estado -> [String]
nombreTodosLosAgentes [] = []
nombreTodosLosAgentes estado = [(nombreAgente (head estado) nombresSegundaGuerra 1)] ++ nombreTodosLosAgentes( tail estado)

cambiaNombres :: (Estado,Energia) -> (([String],[String]),Energia)

cambiaNombres (estado,energia) = ((nombreTodosLosAgentes estado , nombreTodosLosAgentes (agentesEnemigos estado [1..(toInteger (length nombresSegundaGuerra))])),energia )

--
cambiaTodosLosNombres :: [(Estado,Energia)] -> [(([String],[String]),Energia)]
cambiaTodosLosNombres []=[]
cambiaTodosLosNombres lista = [cambiaNombres (head lista)] ++ cambiaTodosLosNombres (tail lista)


mostrarPrediccionesSegundaGuerra :: [(([String],[String]),Energia)]
mostrarPrediccionesSegundaGuerra = cambiaTodosLosNombres (predicciones relacionesSegundaGuerra)
