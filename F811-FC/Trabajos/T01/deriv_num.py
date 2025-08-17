import pandas as pd

class Polinomio:
    def __init__(self, coeficientes):
        self.coef = coeficientes
    def __call__(self, x):
        return sum(c*(x**i) for i, c in enumerate(self.coef))
    def def_derivada(self):
        derivada_coef = [i*c for i, c in enumerate(self.coef) if i>0]
        return Polinomio(derivada_coef)
    def __str__(self):
        terminos = []
        for i, c in enumerate(self.coef):
            if c == 0:
                continue
            if i == 0:
                terminos.append(f"{c}")
            elif i == 1:
                terminos.append(f"{c}" if c != 1 else "x")
            else:
                terminos.append(f"{c}x^{i}" if c != 1 else f"x^{i}")
        return " + ".join(terminos).replace("+ -", "- ")

class derivada:
    def __init__(self, h=1e-10):
        self.h = h
    def derivada_def(self, f, x):
        return (f(x+self.h)-f(x))/self.h
    def derivada_TPC(self, f, x):
        return (f(x+self.h)-f(x-self.h))/(2*self.h)
    def derivada_TPA(self, f, x):
        return (-f(x+2*self.h)+4*f(x+self.h)-3*f(x))/(2*self.h)

def main():
    p = Polinomio([1, 5, 9, 3, 7, 13])
    x_vals = [
        1.0, 0.1, 0.01, 0.001, 0.0001, 0.00001
    ]

    d_analitica = [p.def_derivada()(x) for x in x_vals]
    d_definicion = [derivada(h=1e-10).derivada_def(p, x) for x in x_vals]
    d_TPC = [derivada(h=1e-10).derivada_TPC(p, x) for x in x_vals]
    d_TPA = [derivada(h=1e-10).derivada_TPA(p, x) for x in x_vals]

    error_def = []
    abs_def = 0
    for i in range(len(d_analitica)):
        abs_def = abs(d_analitica[i] - d_definicion[i])
        error_def.append(abs_def)
    error_TPC = []
    abs_TPC = 0
    for i in range(len(d_analitica)):
        abs_TPC = abs(d_analitica[i] - d_TPC[i])
        error_TPC.append(abs_TPC)
    error_TPA = []
    abs_TPA = 0
    for i in range(len(d_analitica)):
        abs_TPA = abs(d_analitica[i] - d_TPA[i])
        error_TPA.append(abs_TPA)

    df = pd.DataFrame({
        'x': x_vals,
        'Analítica': d_analitica,
        'Definición': d_definicion,
        'M. Tres Puntos Centrales': d_TPC,
        'M. Tres Puntos Adelante': d_TPA,
        'Error Def': error_def,
        'Error TPC': error_TPC,
        'Error TPA': error_TPA
    })
    print(f"\nPara el polinomio {p} se tienen los siguientes valores para su primera derivada:\n")
    print(df)

if __name__ == "__main__":
    main()