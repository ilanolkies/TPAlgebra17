module Datos where

--a)
type Set a = [a]
type Agente = Integer
type Estado = Set Agente
type Relaciones = [[Float]]
type Frustracion = Float
type Energia = Float
type Relacion = Float

relacionesEjemplo :: Relaciones
relacionesEjemplo = [[0, 2, -3], [2, 0, 5.2], [-3, 5.2, 0]]

relaciones5 :: Relaciones
relaciones5 = [[0.0, 2.0, -3.0, 2.3, 5.4],
               [2.0, 0.0, 5.2, -2.4, 0.4],
               [-3.0, 5.2, 0, -1.7, -0.4],
               [2.3, -2.4, -1.7, 0.0, 0.2],
               [5.4, 0.4, -0.4, 0.2, 0.0]]
relacionEj2 :: Relaciones
relacionEj2 =  [[0,-1,1,-1],
                [-1,0,-1,1],
                [1,-1,0,1],
                [-1,1,1,0]]

--
relacionEj3 :: Relaciones
relacionEj3 = [[0,-1,-1,-1],
               [-1,0,-1,1],
               [-1,-1,0,1],
               [-1,1,1,0]]

--
relacionEj4 :: Relaciones
relacionEj4= [[0,1,1,-1],
              [1,0,-1,1],
              [1,-1,0,1],
              [-1,1,1,0]]


--b)
relacionesSegundaGuerra :: Relaciones
relacionesSegundaGuerra = [[0,5.530924320,-12.573430061,-1.831433535,0.140479803,1.160896182,0.639907300,0.239890009,-0.005475750,-14.513141632,0.019687442,0.272288263,0.022086270,0.044896755,0.026659824,0.227895498,0.078448497],
													[5.530924320,0,-16.217702866,-0.053598948,0.153114244,1.079521060,0.476374030,0.109059989,-0.003910200,-10.433817863,0.080345437,0.392636746,0.000646380,0.018425951,0.044402048,0.103606991,0.140048996],
													[-12.573430061,-16.217702866,0,1.569729209,-0.311316282,-2.221551180,-1.705838203,-0.079510801,-0.410535991,-39.581401825,-0.659571886,-5.926324844,0.036520272,0.071188547,-0.078591026,-0.075535260,0.105167970],
													[-1.831433535,-0.053598948,1.569729209,0,-0.245003834,-0.101032093,-0.267188996,-0.080600001,-0.002962050,-7.906081676,-0.412530959,0.303845853,0.000000000,0.013516619,0.034456499,-0.076569997,0.108810000],
													[0.140479803,0.153114244,-0.311316282,-0.245003834,0,-0.024172312,-0.065389395,0.011448001,-0.015632141,-1.534870028,-0.075495325,0.024114713,0.000609228,0.001941861,0.002549389,0.010875599,0.007885350],
													[1.160896182,1.079521060,-2.221551180,-0.101032093,-0.024172312,0,0.108018532,0.024886001,0.003511249,-2.139162302,0.016355108,-0.345348030,0.000602508,0.003901890,0.007760890,0.023641698,0.024281099],
													[0.639907300,0.476374030,-1.705838203,-0.267188996,-0.065389395,0.108018532,0,0.016692001,0.019175081,-3.395776749,0.014391925,0.039693996,0.000921586,0.001525664,0.001177613,0.015857400,0.003159000],
													[0.239890009,0.109059989,-0.079510801,-0.080600001,0.011448001,0.024886001,0.016692001,0,-0.000147000,-0.387858421,-0.001132800,0.002342400,0.000972000,0.001547000,0.000080000,0.007600000,0.000000000],
													[-0.005475750,-0.003910200,-0.410535991,-0.002962050,-0.015632141,0.003511249,0.019175081,-0.000147000,0,0.112411730,0.007326352,0.016970046,0.000346521,0.000332591,0.000024749,-0.000139650,-0.000198450],
													[-14.513141632,-10.433817863,-39.581401825,-7.906081676,-1.534870028,-2.139162302,-3.395776749,-0.387858421,0.112411730,0,-0.039151978,-8.461875916,-0.107510924,-0.240316421,-0.190340310,-0.653655469,-0.124417879],
													[0.019687442,0.080345437,-0.659571886,-0.412530959,-0.075495325,0.016355108,0.014391925,-0.001132800,0.007326352,-0.039151978,0,0.022869017,-0.000020355,0.000362446,0.001374511,-0.001076160,0.004221450],
													[0.272288263,0.392636746,-5.926324844,0.303845853,0.024114713,-0.345348030,0.039693996,0.002342400,0.016970046,-8.461875916,0.022869017,0,0.001167943,0.004083339,-0.029964989,0.002225280,0.020356921],
													[0.022086270,0.000646380,0.036520272,0.000000000,0.000609228,0.000602508,0.000921586,0.000972000,0.000346521,-0.107510924,-0.000020355,0.001167943,0,0.000390000,0.000022518,0.000923400,0.000000000],
													[0.044896755,0.018425951,0.071188547,0.013516619,0.001941861,0.003901890,0.001525664,0.001547000,0.000332591,-0.240316421,0.000362446,0.004083339,0.000390000,0,0.000321041,0.001469650,0.000905580],
													[0.026659824,0.044402048,-0.078591026,0.034456499,0.002549389,0.007760890,0.001177613,0.000080000,0.000024749,-0.190340310,0.001374511,-0.029964989,0.000022518,0.000321041,0,0.000076000,0.002308500],
													[0.227895498,0.103606991,-0.075535260,-0.076569997,0.010875599,0.023641698,0.015857400,0.007600000,-0.000139650,-0.653655469,-0.001076160,0.002225280,0.000923400,0.001469650,0.000076000,0,0.000000000],
													[0.078448497,0.140048996,0.105167970,0.108810000,0.007885350,0.024281099,0.003159000,0.000000000,-0.000198450,-0.124417879,0.004221450,0.020356921,0.000000000,0.000905580,0.002308500,0.000000000,0]]



--c)
nombresSegundaGuerra :: [String]
nombresSegundaGuerra = [ "Gran Bretana","Francia","Alemania","Italia","Hungria","Checoslovaquia","Rumania","Dinamarca","Grecia","Union Sovietica","Yugoslavia", "Polonia","Estonia","Letonia","Lituania","Finlandia","Portugal"]
