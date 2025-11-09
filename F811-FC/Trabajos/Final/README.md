# Proyecto

Se debe realizar lo siguiente:
1. Una simulación utilizando las técnicas de Monte Carlo para calcular la eficiencia geométrica de un sistema como el de la figura 1 considerando las siguientes dimensiones (todas expresadas en milímetros)
    * $d=20.0$
    * $g=1.0$
    * $D=55.0$
    * $G=35.0$
    * $L=30.0$

    esta simulación debe incluir:
    * Generación de un punto aleatorio dentro del volumen de la fuente donde se emitirá un rayo gamma.
    * Generación de una dirección aleatoria en la que viajará el rayo gamma.
    * Determinar si el rayo gamma generado pasa por el volumen de detección.
    * Estimación de la eficiencia geométrica por medio de la ecuación 1 y la incerteza de esta.

2. Considerando los datos de la tabla 1, utilizar un método de interpolación para determinar una función para el cálculo de la eficiencia en energía
    
    **Tabla 1:** Eficiencia vs energía a partir de medidas experimentales para un detector de radiación gamma
    | Energía ($keV$) | Eficiencia | 
    |:---------------:|:----------:|
    |      200        |  0.03722   |
    |      400        |  0.02078   |
    |      600        |  0.01478   |
    |     1000        |  0.01160   |
    |     1200        |  0.00962   |
    |     1400        |  0.00725   |
    

3. Los detectores de radiación gamma no determinan la energía del fotón de manera exácta. El *proceso* tanto de interacción como de manejo de señal que lleva una detección produce mediciones que se distribuyen conforme a una distribución de Lorentz: $P(E;E_0,\Gamma)=\frac{1}{\pi}\frac{\Gamma}{(E-E_0)^2+\Gamma^2}$ donde $E_0$ es la energía esperada del fotón a medir y $\Gamma$ es la *resolución* del detector en esta energía. Suponer que la fuente de la figura 1 es de $^{137}Cs$ con una actividad de $A=300 [kBq]$. El $^{137}Cs$ emite un fotón gamma característico con una energía de $662[keV]$. Asumiendo que la resolución del detector esta energía es de $10[keV]$ y tomando en cuenta la eficiencia total (ecuación 3), obtener el espectro (histograma) esperado de salida del detector para una medición de 5 minutos por medio de una simulación de montecarlo.

# Documentación del Trabajo

* [Documentación](Doc/docu.md) 