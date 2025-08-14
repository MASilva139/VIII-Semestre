""" e04_main.py

    Programa de ejemplo en python 3
    Física computacional ECFM

    Héctor Pérez
    Segundo semestre 2025
"""
import e04_functors as functors

cubi_pol = functors.polinomio(3, [1, -2, 1, 1])
print("El polinomio es: ", cubi_pol)
print("El valor del polinomio en el punto x = ", 0.5, " es: ",cubi_pol(0.5))
print("La derivada del polinomio es: ", functors.poli_dev(cubi_pol) )