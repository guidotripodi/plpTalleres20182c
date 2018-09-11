module Main where
import Arboles
import Test.HUnit
import Data.List



--arboles para pruebas:
maderaSolo = Brote Madera
arbol1 = Brote Hoja
arbol2 = Rama Flor (Brote Fruto) (Brote Hoja)
arbol3 = Rama Hoja (Brote Fruto) (Brote Flor)
arbol4 = Rama Madera arbol2 arbol3
arbol5 = Rama Madera arbol3 arbol2
arbol6 = Rama Madera arbol4 arbol1
arbol7 = Rama Madera arbol1 arbol5
arbol8 = Rama Madera arbol1 arbol6
arbol9 = Rama Fruto
		(Rama Fruto (Rama Fruto (Brote Fruto) (Brote Flor)) (Rama Hoja (Brote Madera) (Brote Hoja)))
		(Rama Fruto (Rama Madera (Brote Hoja) (Brote Madera)) (Rama Fruto (Brote Flor) (Brote Fruto)))
arbol10 = Rama Fruto
		(Rama Fruto (Rama Hoja (Brote Madera) (Brote Hoja)) (Rama Fruto (Brote Fruto) (Brote Flor)))
		(Rama Fruto (Rama Fruto (Brote Flor) (Brote Fruto)) (Rama Madera (Brote Hoja) (Brote Madera)))

soloUnaHoja = Brote Hoja
puroMadera = Rama Madera (Brote Madera) (Brote Madera)
tresFlores = Rama Flor (Brote Flor) (Brote Flor)
tresHojas = Rama Hoja (Brote Hoja) (Brote Hoja)

maderaYFlor = Rama Madera (Brote Flor) (Brote Madera)
otraFlor = Rama Madera (Brote Madera) (Brote Flor)

frutoSinFlor = Rama Fruto (Brote Madera) (Brote Madera)

protegido = Rama Fruto (Brote Madera) (Brote Flor)
protegidoNivel1Derecha = Rama Madera soloUnaHoja protegido

superProtegido = Rama Hoja protegido protegido

desbalanceado = Rama Fruto (Brote Madera) protegido


--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8
  ]

testsEj2 = test [
  0 ~=? peso soloUnaHoja,
  3 ~=? peso puroMadera,
  0 ~=? perfume arbol1,
  1 ~=? perfume arbol2,
  1 ~=? perfume arbol3,
  2 ~=? perfume arbol4,
  3 ~=? perfume tresFlores,
  True ~=? puedeVivir arbol1,
  True ~=? puedeVivir tresHojas,
  False ~=? puedeVivir maderaSolo,
  False ~=? puedeVivir (ultimaPrimavera tresHojas),
  False ~=? mismosComponentes arbol1 arbol3,
  False ~=? mismosComponentes arbol2 arbol6,
  True ~=? mismosComponentes arbol2 arbol3,
  True ~=? mismosComponentes arbol4 arbol5
  ]

testsEj3 = test [
  puroMadera ~=? masPesado [arbol4, puroMadera, arbol1]
  ]

testsEj4 = test [
  arbol3 ~=? crecer (\c -> if c == Flor then Hoja else if c == Hoja then Flor else c) arbol2,
  tresFlores ~=? ultimaPrimavera tresHojas,
  tresFlores ~=? ultimaPrimavera tresFlores
  ]

testsEj5 = test [
  otraFlor ~=? comer (Derecha, 1, Gula) otraFlor,
  puroMadera ~=? comer (Derecha, 0, Inanicion) puroMadera,
  puroMadera ~=? comer (Derecha, 0, Gula) puroMadera,
  puroMadera ~=? comer (Derecha, 0, Hambre) puroMadera,
  puroMadera ~=? comer (Derecha, 1, Inanicion) puroMadera,
  puroMadera ~=? comer (Derecha, 1, Gula) puroMadera,
  puroMadera ~=? comer (Derecha, 1, Hambre) puroMadera,
  protegidoNivel1Derecha ~=? comer (Derecha, 1, Gula) protegidoNivel1Derecha,
  protegidoNivel1Derecha ~=? comer (Derecha, 1, Hambre) protegidoNivel1Derecha,
  Rama Madera soloUnaHoja maderaSolo ~=? comer (Derecha, 1, Inanicion) protegidoNivel1Derecha,
  Brote Madera ~=? comer (Derecha, 0, Hambre) frutoSinFlor,
  Brote Madera ~=? comer (Derecha, 0, Inanicion) frutoSinFlor,
  Brote Madera ~=? comer (Izquierda, 0, Hambre) frutoSinFlor,
  Brote Madera ~=? comer (Izquierda, 0, Inanicion) frutoSinFlor,
  Rama Fruto
		(Rama Fruto (Brote Madera) (Rama Hoja (Brote Madera) (Brote Hoja)))
		(Rama Fruto (Rama Madera (Brote Hoja) (Brote Madera)) (Rama Fruto (Brote Flor) (Brote Fruto))) ~=? comer (Izquierda, 2, Inanicion) arbol9, 
  Rama Fruto
		(Rama Fruto (Rama Fruto (Brote Fruto) (Brote Flor)) (Rama Hoja (Brote Madera) (Brote Hoja)))
		(Rama Fruto (Rama Madera (Brote Hoja) (Brote Madera)) (Brote Madera)) ~=? comer (Derecha, 2, Inanicion) arbol9, 
  arbol10 ~=? comer (Izquierda, 2, Inanicion) arbol10,
  arbol10 ~=? comer (Derecha, 2, Inanicion) arbol10 
  ]

testsEj6 = test [
  Rama Fruto
		(Rama Fruto (Brote Madera) (Rama Hoja (Brote Madera) (Brote Hoja)))
		(Rama Fruto (Rama Madera (Brote Hoja) (Brote Madera)) (Brote Madera)) ~=? alimentar arbol9 [(Izquierda, 2, Inanicion), (Derecha, 2, Inanicion)],
  arbol9 ~=? alimentar arbol9 [(Izquierda, 2, Hambre), (Derecha, 2, Hambre)],
  Rama Fruto (Brote Madera) (Brote Madera) ~=? alimentar arbol9 [(Izquierda, 1, Inanicion), (Derecha, 1, Inanicion)],
  arbol9 ~=? alimentar arbol9 [(Izquierda, 1, Hambre), (Derecha, 1, Hambre)],
  Brote Madera ~=? alimentar arbol9 [(Izquierda, 0, Inanicion), (Derecha, 0, Inanicion)],
  arbol9 ~=? alimentar arbol9 [(Izquierda, 0, Hambre), (Derecha, 0, Hambre)]
  ]

testsEj7 = test [
	0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj8 = test [
	0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]
