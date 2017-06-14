import Datos

--Que dados dos agentes, indique cu´al es su relaci´on.
relacion :: Relaciones -> Agente -> Agente -> Float
relacion m_relaciones agenteN agenteM = relacionAgenteNM (relacionesAgenteN m_relaciones agenteN 1) agenteM 1

--Auxiliares:
--Recibe el agente y devuelve la fila de sus relaciones.
relacionesAgenteN :: Relaciones -> Agente -> Integer -> [Relacion]
relacionesAgenteN relaciones agenteN pos | pos == agenteN = head relaciones
                                           | pos < agenteN = relacionesAgenteN (tail relaciones) agenteN (pos+1)

--Recibe una lista de relaciones de un agente y devuelve su relacion con otro agente
relacionAgenteNM :: [Relacion] -> Agente -> Integer -> Relacion
relacionAgenteNM relacionesN agente pos | pos == agente = head relacionesN
                                        | pos < agente = relacionAgenteNM (tail relacionesN) agente (pos+1)

--
--Que dado un agente, el n´umero total de agentes del sistema y un estado determinado, indique 
--el conjunto de agentes enemigos.
enemigos :: Agente -> Integer -> Estado -> Set Agente
enemigos agente cantidadAgentes estado | agentePertenece estado agente = agentesEnemigos estado agentes
                                       | otherwise = estado
                                         where agentes = [1..cantidadAgentes]
--Si estan ordenados!!!

--Auxiliares:
--Recibe un estado y devuelve si el agente pertenece a ese estado
agentePertenece :: Estado -> Agente -> Bool
agentePertenece estado agente | length estado == 0 = False
                              | head estado == agente = True
                              | otherwise = agentePertenece (tail estado) agente

--compA una funcion auxiliar, para comp
agentesEnemigos :: Estado -> Set Agente -> Estado
agentesEnemigos estado agentes | length agentes == 0 = []
                               | length estado == 0 = agentes
                               | head estado == head agentes = agentesEnemigos (tail estado)  (tail agentes)
                               | otherwise = [head agentes] ++ agentesEnemigos estado (tail agentes)

frustracion :: Agente -> Relaciones -> Estado -> Frustracion
frustracion = undefined

energia :: Relaciones -> Estado -> Energia
energia = undefined

adyacente :: Agente -> Estado -> Estado
adyacente = undefined

esEstable :: Relaciones -> Estado -> Bool
esEstable = undefined

estadosPosibles :: Integer -> Set (Set Integer)
estadosPosibles = undefined

predicciones :: Relaciones -> [(Estado, Energia)]
predicciones = undefined











--
