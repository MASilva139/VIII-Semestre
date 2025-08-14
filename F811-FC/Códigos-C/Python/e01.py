""" eo1.py

    Programa de ejemplo en pytho 3
    Física computacional ECFM

    Héctor Pérez
    Segundo semestre 2025
"""

import numpy as np

a, b, c = map(float, input("Ingrese los coeficientes a, b, c de la ecuación cuadrática ax^2 + bx + c = 0: ").split() )

discr = b**2 -4*a*c
deno = 2*a

x1 = ( -b + np.emath.sqrt(discr) ) / deno
x2 = ( -b - np.emath.sqrt(discr) ) / deno

print( "Las soluciones son: x1 = ",x1,"; x2 = ",x2)



