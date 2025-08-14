""" e03_demo.py

    Programa de ejemplo en python 3
    Física computacional ECFM

    Héctor Pérez
    Segundo semestre 2025
"""

class demo:

    def __init__(self, aValue = 0, bValue = 0):
        self.atribute_a = float(aValue)
        self.atribute_b = float(bValue)

    def set_a(self, aValue):
        self.atribute_a = float(aValue)
    
    def set_b(self, bValue):
        self.atribute_b = float(bValue)

    def show(self):
        print("atribute_a : ", self.atribute_a, "; atribute_b : ", self.atribute_b )

    def __str__(self):
        return f"a = {self.atribute_a}; b = {self.atribute_b}"
    
    def __add__(self, sumando):
        aux = demo()
        aux.set_a( self.atribute_a + sumando.atribute_a)
        aux.set_b( self.atribute_b + sumando.atribute_b)
        return aux