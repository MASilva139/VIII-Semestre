# Tarea 01

## Introducción
Dentro de la primera tarea del curso de Física Computacional (F811) se empleo el uso de functors o funciones en Python para el desarrollo de dos programas implementando por medio de functors derivadas numéricas de un polinomio y el método de Newton para encontrar raíces reales de un polinomio.

Para el programa de derivadas numéricas de un polinomio se empleó la biblioteca de `pandas` como herramienta para la generación de la tabla de resultados.

## Explicación de Código (Derivada Numérica)
### Objeto Polinomio
Genera un polinomio en base a la cantidad de coeficientes ingresados dentro de la lista de coeficientes, utilizando para ello `enumerate()`, así mismo se crea un método dentro de `Polinomio` para hacer una derivada en base a los coeficientes y los términos algebraicos que los acompañan, la cual determinaría el valor real de la derivada.
```python
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
```
También se crea un método `__str__(self)` para devolver el polinomio en forma de string.

### Objeto de derivada
Se encarga de realizar las derivadas del polinomio a partir de la definición (`def derivada_def`), que corresponde a la diferencia con dos puntos adelante; por medio de la diferencia con tres puntos central (`def derivada_TPC`) y la diferencia con tres puntos adelante (`def derivada_TPA`)
```python
class derivada:
    def __init__(self, h=1e-10):
        self.h = h
    def derivada_def(self, f, x):
        return (f(x+self.h)-f(x))/self.h
    def derivada_TPC(self, f, x):
        return (f(x+self.h)-f(x-self.h))/(2*self.h)
    def derivada_TPA(self, f, x):
        return (-f(x+2*self.h)+4*f(x+self.h)-3*f(x))/(2*self.h)
```

### Objetos de las derivadas del polinomio
Se tiene que las listas derivada analítica (valor real) del polinomio $P(x)$ está descrita por `d_analitica`, mientras que la derivada según la definición, tres puntos central y tres puntos adelante corresponden a `d_definicion`, `d_TPC` y `d_TPA` respectivamente. Las listas de derivadas evalúan la derivada del polinomio en los puntos dados por `x_vals`.
```python
p = Polinomio([1, 5, 9, 3, 7, 13])
x_vals = [
    1.0, 0.1, 0.01, 0.001, 0.0001, 0.00001
]

d_analitica = [p.def_derivada()(x) for x in x_vals]
d_definicion = [derivada(h=1e-10).derivada_def(p, x) for x in x_vals]
d_TPC = [derivada(h=1e-10).derivada_TPC(p, x) for x in x_vals]
d_TPA = [derivada(h=1e-10).derivada_TPA(p, x) for x in x_vals]
```

### Errores de cada método
Calcula los errores que presenta cada método de derivación con respecto al valor original de la derivada.
```python
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
```

### Tabla de Resultados
Registra los datos de los puntos a evaluar, las derivadas y el error de cada método de derivación.
```python
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
```

<[Regresar](/F811-FC/Trabajos/T01/)>
