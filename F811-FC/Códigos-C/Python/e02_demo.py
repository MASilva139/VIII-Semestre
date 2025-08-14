""" e02_demo.py

    Programa de ejemplo en python 3
    Física computacional ECFM

    Héctor Pérez
    Segundo semestre 2025
"""

class demo:

    def __init__(self):
        self.atribute_a = float(0)
        self.atribute_b = float(0)

    def set_a(self, aValue):
        self.atribute_a = float(aValue)
    
    def set_b(self, bValue):
        self.atribute_b = float(bValue)

    def show(self):
        print("atribute_a : ", self.atribute_a, "; atribute_b : ", self.atribute_b )
