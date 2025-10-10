# Modelo Continuo
De la segunda ley de Newton se tiene que la fuerza está dada por

<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-1.svg" alt="Ecuación 01" width="800">
</div>

para $i=1, 2, 3, 4$. Empleando el método de diferenciación numérica, centrada, en el tiempo, se procede a introducir una malla temporal $t^{n}=n\Delta t$. Se sabe que la segunda derivada se aproxima con diferencias centradas de forma que
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-2.svg" alt="Ecuación 02" width="800">
</div>

con un error de $O\left(\Delta t^{2}\right)$. Al sustituir en la ecuación de movimiento se tiene que
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-3.svg" alt="Ecuación 03" width="800">
</div>

donde se tiene que $\pmb{a}_{i}(\pmb{r}^{n})=\pmb{F}_{i}(\pmb{r}^{n})/m_{i}$. De esto se obtiene el actualizador de posiciones

<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-4.svg" alt="Ecuación 04" width="800">
</div>

Para la posición inicial, como falta el término $\pmb{r}_{i}^{-1}$, usando expansiones de Taylor, se tiene que

<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-5.svg" alt="Ecuación 05" width="800">
</div>

Con ello se puede reconstruir partiendo de la diferencia centrada de primer orden que

<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-6.svg" alt="Ecuación 06" width="800">
</div>

con un error de $O\left(\Delta t^{2}\right)$.

Con ello se tiene que
* La ecuación es de segundo orden en tiempo y casi simétrico, lo cual implica un posible buen comportamiento de energía a largo plazo.
* Con softening $\varepsilon$ y $\Delta t$ pequeño se tiene que la función es estable y precisa.

[[Regresar](./)]