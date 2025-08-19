# Functors en Python
1. Implementar por medio de functors o funciones una [derivada numérica](Derivada.md) sobre un functor que represente polinomios:
    * Por medio de la definición de derivada.
    * Por medio de la fórmula de la diferencia de tres puntos centrales (**tree-point central difference formula**).
    * Por medio de la fórmula de la diferencia de tres puntos hacia adelante (**tree-point forward difference formula**).
    * Hacer una tabla comparando los resultados de estos métodos respecto del valor real para un polinomio.
2. Implementar por medio de un functor o función el método de Newton para encontrar las raíces reales de un polinomio reprecentado por un functor. El functor o función debe recibir un conjunto (array) de puntos iniciales acordes al grado del polinomio para iniciar la búsqueda y devolver un conjunto del mismo tamaño con los resultados. Se debe implementar como condición final cada búsqueda lo siguiente:
    * Que el polinomio de cero en el punto encontrado $P(x_n)=0$.
    * Que el cambio entre los dos últimos valores de las iteraciones sea menor que un valor predefinido $x_n-x_{n-1}<\varepsilon$.
    * Que el número de iteraciones exceda un valor máximo predefinido $n>n_{\mathrm{max}}$

Debe de entregarse un archivo comprimido que contenga los archivos de código fuente en Python3. El código fuente debe incluir la implementación de los functors o funciones requeridos en esta tarea además de un código que realice el llamado a estas y que muestre claramente su funcionamiento.

# Documentación del Trabajo

* [Documentación](Doc/deriv_num.md) 