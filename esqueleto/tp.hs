import Datos

relacion :: Relaciones -> Agente -> Agente -> Float
relacion = undefined

enemigos :: Agente -> Integer -> Estado -> [Agente]
enemigos = undefined

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
