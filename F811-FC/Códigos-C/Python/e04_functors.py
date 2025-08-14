""" e04_functors.py

    Programa de ejemplo en python 3
    Física computacional ECFM

    Héctor Pérez
    Segundo semestre 2025
"""
import sys
import numpy as np

class polinomio:

    def __init__(self, grado = int(0), coeficientes = np.array([0],dtype=float), rango = np.array([-1,1],dtype=float)):
        try:
            self.n = int(grado)
        except:
            print("El grado del polinomio debe ser un número entero")
            sys.exit()

        self.coefs = np.array([],dtype=float)

        try:
            self.coefs = np.array( coeficientes, dtype= float)
        except:
            print("Los coeficientes deben ser números")

        if self.coefs.size != (self.n + 1) :
            print("El número de coeficientes debe ser ", self.n + 1, self.coefs)
            sys.exit()

        self.ran = np.array([],dtype=float)

        try:
            self.ran = np.array( rango, dtype= float)
        except:
            print("El rango deben ser especificado en números")

        if (self.ran.size != 2) and (self.ran[0] >= self.ran[1]) :
            print("Rango inválido ", self.ran)
            sys.exit()
        
    def __str__(self):
        str_aux = str( self.coefs[0] ) + "x^" + str(self.n)
        for m in range(1, self.n):
            str_aux = str_aux + " + " + str( self.coefs[m] ) + "x^" + str(self.n - m)
        str_aux = str_aux + " + " + str( self.coefs[self.n] )
        return str_aux

    def __call__(self, x = float(0)):
        aux = 0
        if ( x >= self.ran[0] ) and ( x <= self.ran[1] ):
            for m in range(self.n+1):
                aux = aux + self.coefs[m]*np.power(x,self.n - m)
            return aux
        print("Valor fuera de rango")
        sys.exit()

    def grado(self):
        return self.n
    
    def coeficientes(self):
        return self.coefs
    
    def rango(self):
        return self.ran
        

def poli_dev( poli_obj ):
    if isinstance(poli_obj, polinomio) :
        aux = [] #= np.array([],dtype=float)
        coeficientes = poli_obj.coeficientes()
        for i in range(poli_obj.grado()+1):
            aux.append( (poli_obj.grado() - i )*coeficientes[i] )
        return polinomio(poli_obj.grado()-1,aux[:-1],poli_obj.rango())
    print("Argumento inválido: no es un polinomio")
    sys.exit()

    

