{--Práctica de Progrmación declarativa 
Nombre: Eddy Cuizaguana Cerpa
Grado en Ingenieria Informática 3ºA
Curso :2013-2014
para la realizacion de la misma se consulto informacion de: http://aprendehaskell.es/
--}

import System.IO

infixl 7 :* , :/
infixl 6 :+ , :-

data ExpArit = I Integer | V String | ExpArit :+ ExpArit | ExpArit :- ExpArit | ExpArit :* ExpArit | ExpArit :/ ExpArit 
	deriving (Show, Read)

infixl 4 :>, :<, :<=, :>=, :==, :/=,:&&, :||
data ExpBoolean = T Bool | ExpArit :> ExpArit | ExpArit :< ExpArit |
						   ExpArit :<= ExpArit |ExpArit :>= ExpArit | ExpArit :== ExpArit|
						   Not ExpBoolean | ExpBoolean :&& ExpBoolean | ExpBoolean :|| ExpBoolean | 
						   ExpArit :/= ExpArit 
	deriving (Show, Read)

data Then = Then Programa
	deriving (Show, Read)

data Else = Else Programa
	deriving (Show, Read)

infixl 4 :=
data Instruccion = String := ExpArit  | If ExpBoolean Then Else | While ExpBoolean Programa
	deriving (Show, Read)
	
type Programa = [Instruccion]

type Estado = [(String, Integer)]
	
-- Parte obligatoria
ejecuta :: Programa -> Estado -> Integer
ejecuta [] estado  = buscar estado "R" 
ejecuta (xs : ys ) estado = ejecuta ys (evaluaInstruccion xs estado)

-- Parte opcional 1 - Ejecuta desde fihero
ejecutaDesdeFichero :: IO Integer
ejecutaDesdeFichero = do
		putStrLn "Introduce el estado inicial con el siguiente formato [(\"X\",4)]: "
		estadoIncial <- getLine
		putStrLn "Introduce la ruta en donde de encuentra el fichero incluida la extencion del mismo"
		nameFile <- getLine
		programa <- readFile nameFile
		return (ejecuta ((read programa)::Programa) ((read estadoIncial)::Estado))
		
-- Parte opcional 2 - pulsando la tecla a + intro ejecutamos el programa paso a paso
ejecutaStepToStep :: Programa -> Estado -> IO ()
ejecutaStepToStep [] estado = 	do 
											putStrLn ("Estado: " ++ show(estado) )
											putStrLn "Pulsa q + enter para continuar."
											tecla <- getLine
											putStrLn ("El resultado es " ++ show (buscar estado "R"))
ejecutaStepToStep (x:[]) estado = do 
													putStrLn ("Estado: " ++ show(estado) )
													putStrLn "Pulsa q + enter para continuar."
													tecla <- getLine
	
													if (tecla == "q") then 
														if (esBucleYCumpleCondicion x estado) 														
															then 
																if (esBucleYCumpleCondicion x (evaluaInstruccion' x estado)) 
																	then ejecutaStepToStep (x:[]) (evaluaInstruccion' x estado)
																else ejecutaStepToStep [] (evaluaInstruccion' x estado) 
														else 
																ejecutaStepToStep [] (evaluaInstruccion' x estado) -- ultima instruccion y no es bucle
													else ejecutaStepToStep (x: []) estado 
													
ejecutaStepToStep (x: z : ys) estado = do 
													putStrLn ("Estado: " ++ show(estado) )
													putStrLn "Pulsa q + enter para continuar."
													tecla <- getLine
													if (tecla == "q") then 
														if esBucle x then
															if cumpleCond x estado 
																then ejecutaStepToStep (x:z:ys) (evaluaInstruccion' x estado) 
															else ejecutaStepToStep ys  (evaluaInstruccion' z estado)  -- ejecut las sig instruccion
														else 	ejecutaStepToStep (z:ys) (evaluaInstruccion' x estado)
													else ejecutaStepToStep (x:z:ys) estado 
													
													
													
esBucle (While expBooleana p)  = True
esBucle(var := exprArit)  = False 
esBucle(If expBooleana (Then p) (Else p'))  = False

cumpleCond (While expBooleana p) estado = cumpleCondicion expBooleana estado
cumpleCond (var := exprArit) estado = False 
cumpleCond (If expBooleana (Then p) (Else p')) estado = False	
	 													
{-- Funcion que dada una intruccion se encarga de verificar que la misma sea sea 
una instrucion while y que su condicion se cumpla, caso contrario devolvera False --}
esBucleYCumpleCondicion :: Instruccion -> Estado -> Bool													
esBucleYCumpleCondicion (While expBooleana p) estado = cumpleCondicion expBooleana estado
esBucleYCumpleCondicion (var := exprArit) estado = False 
esBucleYCumpleCondicion (If expBooleana (Then p) (Else p')) estado = False													
												
-- funcion encargada de ver que tipo de instrucion tiene para analizar, devuelve un estado
evaluaInstruccion :: Instruccion -> Estado -> Estado
evaluaInstruccion (var := exprArit) estado = evaluaAsignacion var exprArit estado
evaluaInstruccion (If expBooleana (Then p) (Else p')) estado = evaluaIfThenElse expBooleana p p' estado
evaluaInstruccion (While expBooleana p) estado = evaluaWhile expBooleana p estado

evaluaInstruccion' :: Instruccion -> Estado -> Estado
evaluaInstruccion' (var := exprArit) estado = evaluaAsignacion var exprArit estado 
evaluaInstruccion' (If expBooleana (Then p) (Else p')) estado = evaluaIfThenElse expBooleana p p' estado
evaluaInstruccion' (While expBooleana p) estado = evaluaWhile' expBooleana p estado


{--funcion encargada de evaluar una instrucion de asignacion--}
evaluaAsignacion :: String -> ExpArit -> Estado -> Estado
evaluaAsignacion var exprArit estado = setEstado  var (evalExpArit exprArit estado) estado

{--funcion encargada de evaluar una condicion if then else devolviendo un Estado
--}
evaluaIfThenElse :: ExpBoolean -> Programa -> Programa -> Estado -> Estado
evaluaIfThenElse expBooleana p  p' estado = if cumpleCondicion expBooleana estado 
																then evaluaPrograma p estado else evaluaPrograma p' estado

-- While , funcion que dada una expBooleana nos devuelve un nuevo Estado
evaluaWhile :: ExpBoolean -> Programa -> Estado -> Estado
evaluaWhile expBooleana [] estado = estado
evaluaWhile expBooleana p estado = if cumpleCondicion expBooleana estado 
														then evaluaWhile expBooleana p (evaluaPrograma p estado) 
														else  evaluaWhile expBooleana [] estado

evaluaWhile' :: ExpBoolean -> Programa -> Estado -> Estado
evaluaWhile' expBooleana [] estado = estado
evaluaWhile' expBooleana p estado = if cumpleCondicion expBooleana estado 
														then (evaluaPrograma p estado) else estado

-- Dado un Programa y Estado devuelve un Estado
evaluaPrograma :: Programa -> Estado -> Estado
evaluaPrograma [] estado = estado
evaluaPrograma (x:ys) estado =  evaluaPrograma ys (evaluaInstruccion x estado)

-- fucion que dado una variable, valor y un Estado nos devuelve un nuevo Estado que contiene las variables actualizadas
setEstado :: String -> Integer -> Estado -> Estado -- solo sirve cuando se quiere modificar un valor
setEstado var val estado = if nuevoEstado var val estado -- comprobamos si el nuevo estado ya existe
										then  (var,val):estado -- estado no existe, lo añadimos a la lista
										else (var,val):[ x | x <- estado , verificaVariable x var val]  

{-- comprueba si el nuevo estado ya existe--}
nuevoEstado :: String -> Integer -> Estado -> Bool
nuevoEstado var val [] = True
nuevoEstado var val (x:ys) = if verificaVariable x var val 
											then nuevoEstado var val ys else False

-- funcion que comprueba si ya existe una variable en Estado
verificaVariable :: (String, Integer) -> String -> Integer -> Bool
verificaVariable (var , val) var' val' = var /= var'  

-- evalua una expresion aritmetica, si trabaja con una variable que anteriormente fue computada lo busca en Estado
evalExpArit :: ExpArit -> Estado -> Integer
evalExpArit (I n) estado = n
evalExpArit (V v) estado = buscar estado  v
evalExpArit (e :+ e') estado = evalExpArit e estado + evalExpArit e' estado
evalExpArit (e :- e') estado = evalExpArit e estado - evalExpArit e' estado
evalExpArit (e :* e') estado = evalExpArit e estado * evalExpArit e' estado
evalExpArit (e :/ e') estado = evalExpArit e estado `div` evalExpArit e' estado

-- funcion que dad una expBooleana y Estado nos devuelve True si dicha expBooleana se evalua a True
cumpleCondicion :: ExpBoolean -> Estado -> Bool
cumpleCondicion (e :> e') estado = (evalExpArit e estado) > (evalExpArit e' estado)
cumpleCondicion (e :< e') estado = (evalExpArit e estado) < (evalExpArit e' estado)
cumpleCondicion (e :== e') estado = (evalExpArit e estado) == (evalExpArit e' estado)
cumpleCondicion (e :<= e') estado =  (cumpleCondicion (e :< e') estado) || (cumpleCondicion (e :== e') estado)
cumpleCondicion (e :>= e') estado =  (cumpleCondicion (e :> e') estado) || (cumpleCondicion (e :== e') estado)
cumpleCondicion (Not e) estado = (not) (cumpleCondicion e estado) 
cumpleCondicion (e :&& e') estado = (cumpleCondicion e estado) && (cumpleCondicion e' estado)
cumpleCondicion (e :|| e') estado = (cumpleCondicion e estado) || (cumpleCondicion e estado)
cumpleCondicion (e :/= e') estado = (evalExpArit e estado) /= (evalExpArit e' estado)

-- funcion que se encarga de buscar una variable en dentro de un Estado
buscar :: Estado -> String -> Integer
buscar ((var,val):ys) var' = if var == var' then val else buscar ys var'

-- ejemplos de ejecucion
			
s0 = [("X",5)]
factorial = [	"Y" := V "X",
					"R" := I 1,
					While (I 0 :< V "Y")
						[	"R" := (V "R" :* V "Y"),
							"Y" := (V "Y" :- I 1)
						]
				]

-- Potencia X^N
s1 = [("X", 2), ("N",10)]
pow = [
		"R" := I 1,
		"J" := I 1,
		If (V "X" :== I 0) 
			(Then ["R" := I 0]) 
			(Else [While (V "J":<= V "N") 
						[	"R" := (V "R" :* V "X"), 
							"J" := (V "J" :+ I 1)
						]
					]
			)
		]

s2 = [("X",0),("Y",0)]
bucle1 = [	"R" := I 0, 
				While (V "X" :<= I 5)
					[	"X" :=( V "X" :+ I 1),
						"Y" := I 0,
						While (V "Y" :<= I 3) 
						[	"R" := (V "R" :+ I 1),
							"Y" :=( V "Y" :+ I 1)
						]
					]
			]

s3 = [("X",0),("Y",0)]			
bucle2 = [	"R" := I 0, 
				While (V "X" :<= I 5)
					[	"X" :=( V "X" :+ I 1),
						"Y" := I 0,
						While (V "Y" :<= I 3) 
						[	"R" := (V "R" :+ I 1),
							"Y" :=( V "Y" :+ I 1)
						]
					],
				"Y" := I 0,
				"X" := I 0,
				While (V "X" :<= I 5)
					[	"X" :=( V "X" :+ I 1),
						"Y" := I 0,
						While (V "Y" :<= I 3) 
						[	"R" := (V "R" :+ I 1),
							"Y" :=( V "Y" :+ I 1)
						]
					]
			]

s5 = [("X", 4),("Y", 10),("R", 0)]
bucle3 = [	While (Not(V "Y" :< I 7)) [
					"R" := (V "R" :+ V "Y"),
					"Y" := (V "Y" :- I 1)]
		]

s4 = [("X", 4)]
if_ = [If (V "X" :> I 5) (Then ["R" := I 10]) (Else ["R" := I 5])] 
