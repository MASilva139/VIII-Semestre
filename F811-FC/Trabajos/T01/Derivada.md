# Métodos de Derivación Numérica

La derivación numérica es una técnia del análisis numérico utilizada para aproximar la derivada de una función a partir de valores discretos conocidos. Este tipo de método es fundamental en problemas donde la función:
* No tiene una expresión analítica disponible.
* Está definida mediante datos experimentales.
* Es costoso o imposible calcular la derivada exacta.
En este tipo de casos, se reemplaza el cálculo analítico por una fórmula de diferencias finitas, la cual se obtiene a partir de la expanción en series de Taylor.

## Definición Fundamental
Sea $f(x)$ una función suficientemente diferenciable. La derivada de la función en un punto $x$ está definido como:
<!--
\begin{equation}
    \begin{aligned}
        f'(x) & = \lim_{\Delta x\rightarrow0}\frac{f(x+\Delta x)-f(x)}{\Delta x}
    \end{aligned}
\end{equation}
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-01.svg" alt="Ecuación 01" width="800">
</div>

donde $\Delta x$ es un valor infinitesimal.

## Deducción General con Series de Taylor
La aproximación se obtiene expandiendo la función $f(x)$ alrededor de $x$, tal que
<!--
\begin{aligned}
    f(x\pm k\cdot\Delta x) & = \sum^{\infty}_{n=0}\frac{1}{n!}\frac{d^{n}f(x)}{dx^{n}}\left(\pm k\cdot\Delta x\right)^{n}
    \\
    f(x\pm k\cdot\Delta x) & = f(x) + \frac{df(x)}{dx}\left(\pm k\cdot\Delta x\right) + \frac{1}{2!}\frac{d^{2}f(x)}{dx^{2}}\left(\pm k\cdot\Delta x\right)^{2} + \cdots + \frac{1}{n!}\frac{d^{n}f(x)}{dx^{n}}\left(\pm k\cdot\Delta x\right)^{n}
    \\
    f(x\pm k\cdot\Delta x) & = f(x) + f'(x)\left(\pm k\cdot\Delta x\right) + \frac{1}{2!}f''(x)\left(\pm k\cdot\Delta x\right)^{2} + \cdots + \frac{1}{n!}f^{(n)}(x)\left(\pm k\cdot\Delta x\right)^{n}
\end{aligned}
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-02.svg" alt="Ecuación 02" width="800">
</div>

para $\Delta x>0$ y $x\in(a,b)$.

A partir de varias evaluaciones de $f(x\pm k\cdot\Delta x)$, se construyen combinaciones lineales para que:
* El término de $f'(x)$ se conserve.
* Los términos no deseados se cancelen hasta un orden deseado.

El primer término no anulado determina el orden de precisión.

## Clasificación de Métodos
Existen diferentes métodos de diferencias finidas:
* **Diferencias hacia adelante**:
Estos métodos aproximan la derivada usando valores desde $x$ hacia adelante. Su ventaja radica en que son útiles en límites o bordes; sin embargo, son menos precisas que la central para el mismo número de puntos.
* **Diferencia hacia atras**: 
Estos métodos utilizan valores desde $x$ hacia atrás, para aproximar la derivada. Se aplican cuando no hay datos posteriores al punto de interés.
* **Diferencias centrales**: 
Estos métodos utilizan los valores a ambos lados de $x$, para aproximar la derivada. Su ventaja radica en que tienen una mayor precisión y simetría, dado que elimina términos pares de la expresión.

## Diferencia de Tres Puntos Centrales
De la expanción de Taylor de la función $f(x)$
<!--
\begin{aligned}
    f(x\pm k\cdot\Delta x) & = f(x) + f'(x)\left(\pm k\cdot\Delta x\right) + \frac{1}{2!}f''(x)\left(\pm k\cdot\Delta x\right)^{2} + \cdots + \frac{1}{n!}f^{(n)}(x)\left(\pm k\cdot\Delta x\right)^{n}
\end{aligned}
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-03.svg" alt="Ecuación 03" width="800">
</div>

Para $k=1$ y signo $+$ se tiene $f(x+\Delta x)$, y despejando el término de $f'(x)$ de la expresión se tiene que
<!--
\begin{aligned}
    f(x+\Delta x) & = f(x) + f'(x)\Delta x + \frac{1}{2!}f''(x)\left(\Delta x\right)^{2} + \cdots + \frac{1}{n!}f^{(n)}(x)\left(\Delta x\right)^{n}
    \\
    f'(x) & = \frac{f(x+\Delta x)-f(x)}{\Delta x} - \frac{1}{2!}f''(x)\left(\Delta x\right) - \cdots - \frac{1}{n!}f^{(n)}(x)\left(\Delta x\right)^{n-1}
\end{aligned}
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-04.svg" alt="Ecuación 04" width="800">
</div>

Para $k=1$ y signo $-$ se tiene $f(x-\Delta x)$, y despejando el término de $f'(x)$ de la expresión se tiene que 
<!--
\begin{aligned}
    f(x-\Delta x) & = f(x) + f'(x)\left(-\Delta x\right) + \frac{1}{2!}f''(x)\left(-\Delta x\right)^{2} + \cdots + \frac{1}{n!}f^{(n)}(x)\left(-\Delta x\right)^{n}
    \\
    f(x-\Delta x) & = f(x) - f'(x)\Delta x + \frac{1}{2!}f''(x)\left(\Delta x\right)^{2} + \cdots + \frac{1}{n!}f^{(n)}(x)\left(-\Delta x\right)^{n}
    \\
    f'(x) & = \frac{f(x)-f(x-\Delta x)}{\Delta x} + \frac{1}{2!}f''(x)\left(\Delta x\right) - \cdots - \frac{(-1)^{n-1}}{n!}f^{(n)}(x)\left(\Delta x\right)^{n-1}
\end{aligned}
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-05.svg" alt="Ecuación 05" width="800">
</div>

Sumando ambas expresiones
<!--
\begin{aligned}
    f'(x) & = \frac{f(x+\Delta x)-f(x)}{\Delta x} - \frac{1}{2!}f''(x)\left(\Delta x\right) - \frac{1}{3!}f'''(x)(\Delta x)^{2} - \cdots - \frac{1}{n!}f^{(n)}(x)\left(\Delta x\right)^{n-1}
    \\
    f'(x) & = \frac{f(x)-f(x-\Delta x)}{\Delta x} + \frac{1}{2!}f''(x)\left(\Delta x\right) - \frac{1}{3!}f'''(x)(\Delta x)^{2} - \cdots - \frac{(-1)^{n-1}}{n!}f^{(n)}(x)\left(\Delta x\right)^{n-1}
    \\\hline
    2f'(x) & = \frac{f(x+\Delta x)-f(x)}{\Delta x} + \frac{f(x)-f(x-\Delta x)}{\Delta x} - \frac{2}{3!}f'''(x)(\Delta x)^{2} - \cdots - \frac{1}{n!}\left[1+(-1)^{n-1}\right]f^{(n)}(x)(-\Delta x)^{n-1}
    \\
    f'(x) & = \frac{f(x+\Delta x)-f(x-\Delta x)}{2\Delta x}  - \frac{1}{3!}f'''(x)(\Delta x)^{2} - \cdots - \frac{1}{2n!}\left[1+(-1)^{n-1}\right]f^{(n)}(x)(-\Delta x)^{n-1}
\end{aligned}
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-06.svg" alt="Ecuación 06" width="800">
</div>

Dado que la derivada que no se cancela es del orden $3$ se tiene que el orden de error de esta expresión es de $\mathscr{O}(\Delta x^{2})$. Al considerar que $\Delta x\rightarrow0$ se tiene que las los términos de orden superior son despreciables, de forma que
<!--
\begin{aligned}
    f'(x) & \sim \frac{f(x+\Delta x)-f(x-\Delta x)}{2\Delta x}
\end{aligned}
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-07.svg" alt="Ecuación 07" width="800">
</div>

donde los puntos usados son $x-\Delta x$, $x$, $x+\Delta x$.

## Diferencia de Tres Puntos Adelante
De la expanción de Taylor de la función $f(x)$, para el punto $x$ se tiene que $\Delta x=0$ tal que $f(x)=f(x)$. Para el punto $x+\Delta x$ se había obtenido, del anterior problema, que
<!--
\begin{aligned}
    f(x+\Delta x) & = f(x) + f'(x)\Delta x + \frac{1}{2!}f''(x)\left(\Delta x\right)^{2} + \cdots + \frac{1}{n!}f^{(n)}(x)\left(\Delta x\right)^{n}
\end{aligned}
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-08.svg" alt="Ecuación 08" width="800">
</div>

Para $x+2\Delta$ se tiene que
<!--
\begin{aligned}
    f(x+2\Delta x) & = f(x) + 2f'(x)\Delta x + \frac{1}{2!}f''(x)\left(2\Delta x\right)^{2} + \cdots + \frac{1}{n!}f^{(n)}(x)\left(2\Delta x\right)^{n}
    \\
    f(x+2\Delta x) & = f(x) + 2f'(x)\Delta x + \frac{4}{2!}f''(x)\left(\Delta x\right)^{2} + \cdots + \frac{2^{n}}{n!}f^{(n)}(x)\left(\Delta x\right)^{n}
\end{aligned}
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-09.svg" alt="Ecuación 09" width="800">
</div>

Para poder aislar $f'(x)$ se propone una solución de la forma
<!--
\begin{aligned}
    f'(x) & \sim \frac{a_{0}f(x)+a_{1}f(x+\Delta x)+a_{2}f(x+2\Delta x)}{\Delta x}
\end{aligned}
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-10.svg" alt="Ecuación 10" width="800">
</div>

con ello se tiene que
<!--
\begin{aligned}
    f'(x) & = a_{0}\left[f(x)\right] + a_{1}\left[f(x) + f'(x)\Delta x + \frac{1}{2!}f''(x)\left(\Delta x\right)^{2} + \frac{1}{3!}f'''(x)\left(\Delta x\right)^{3} + \cdots + \frac{1}{n!}f^{(n)}(x)\left(\Delta x\right)^{n}\right] + 
    \\ & \hspace{15pt} a_{2}\left[f(x) + 2f'(x)\Delta x + \frac{4}{2!}f''(x)\left(\Delta x\right)^{2} + \frac{8}{3!}f'''(x)\left(\Delta x\right)^{3} + \cdots + \frac{2^{n}}{n!}f^{(n)}(x)\left(\Delta x\right)^{n}\right]
    \\
    f'(x) & = (a_{0}+a_{1}+a_{2})f(x) + (a_{1}+2a_{2})f'(x)\Delta x + \left(\frac{1}{2!}a_{1} + \frac{4}{2!}a_{2}\right)f''(x)(\Delta x)^{2} + \left(\frac{1}{3!}a_{1} + \frac{8}{3!}a_{2}\right)f'''(x)(\Delta x)^{3} + \mathscr{O}(\Delta x^{4})
\end{aligned}
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-11.svg" alt="Ecuación 11" width="800">
</div>

y de esto se obtiene el siguiente sistema
<!--
$$
\left.\begin{aligned}
    f(x):\hspace{5pt} 0 & = a_{0} + a_{1} + a_{2}
    \\
    f'(x):\hspace{5pt} 1 & = a_{1} + 2a_{2}
    \\
    f''(x):\hspace{5pt} 0 & = \frac{1}{2}a_{1} + 2a_{2}
\end{aligned}\hspace{10pt}\right\}\hspace{10pt}\left.\begin{aligned}
    a_{0} & = -(a_{1}+a_{2})
    \\
    1 & = a_{1} + 2a_{2}
    \\
    a_{1} & = -4a_{2}
\end{aligned}\hspace{10pt}\right\}\hspace{10pt}\begin{aligned}
    a_{0} & = -3/2
    \\
    a_{1} & = 2
    \\
    a_{2} & = -1/2
\end{aligned}
$$
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-12.svg" alt="Ecuación 12" width="800">
</div>

Por tanto, se tiene que
<!--
$$
\left.\begin{aligned}
    f'(x) & \sim \frac{a_{0}f(x)+a_{1}f(x+\Delta x)+a_{2}f(x+2\Delta x)}{\Delta x}
    \\
    f'(x) & \sim \frac{-\frac{3}{2}f(x)+2f(x+\Delta x)-\frac{1}{2}f(x+2\Delta x)}{\Delta x}
\end{aligned}\hspace{5pt}\right\}\hspace{5pt}\begin{aligned}
    f'(x) & \sim \frac{-3f(x)+4f(x+\Delta x)-f(x+2\Delta x)}{2\Delta x}
\end{aligned}
$$
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-13.svg" alt="Ecuación 13" width="800">
</div>

El orden del error, o término de truncamiento, dado por el término no cancelado de $f'''(x)$ es
<!--
$$
\left.\begin{aligned}
    E(\Delta x) & = \left(\frac{1}{3!}a_{1} + \frac{8}{3!}a_{2}\right)f'''(x)(\Delta x)^{3}
    \\
    E(\Delta x) & = \left(\frac{1}{3!}[2] + \frac{8}{3!}\left[-\frac{1}{2}\right]\right)f'''(x)(\Delta x)^{3}
\end{aligned}\hspace{5pt}\right\}\hspace{5pt}\left.\begin{aligned}
    E(\Delta x) & = \left(\frac{1}{3} - \frac{2}{3}\right)f'''(x)(\Delta x)^{3}
    \\
    E(\Delta x) & = -\frac{1}{3}f'''(x)(\Delta x)^{3}
\end{aligned}\hspace{5pt}\right\}\hspace{5pt}\begin{aligned}
    E(\Delta x) & \sim -\frac{\Delta x^{2}}{3}f'''(\xi)
\end{aligned}
$$
-->
<div style="background-color: #f4f4f4; padding: 0px; display: inline-block; border-radius: 6px;">
  <img src="E01/.build/eq-14.svg" alt="Ecuación 14" width="800">
</div>

para algún $\xi\in[x,x+2\Delta x]$, de manera que la fórmula es de orden $\mathscr{O}(\Delta x^{2})$.

# Métodos a Utilizar
De lo anterior se tienen los siguientes metodos a emplear
| Tipo de Método | Puntos | Ecuación | Orden de error |
| :------ | :------: | :------: | :------: |
| Definición |  | $\lim_{\Delta x\rightarrow0}\frac{f(x+\Delta x)-f(x)}{\Delta x}$ |  |
| 3 Puntos Central | $x-\Delta x$, $x$, $x+\Delta x$ | $\frac{f(x+\Delta x)-f(x-\Delta x)}{2\Delta x}$ | $\mathscr{O}(\Delta x^{2})$ |
| 3 Puntos Adelante | $x$, $x+\Delta x$, $x+2\Delta x$ | $\frac{-3f(x) +4f(x+\Delta x)+-f(x+2\Delta x)}{2\Delta x}$ | $\mathscr{O}(\Delta x^{2})$ |

[[Regresar](README.md)]

<!--
Para exportar las páginas de un .pdf a .svg
- Ingresar a la carpeta del pdf generado por latex
- Escribir el código:
    → dvisvgm --pdf --no-fonts --exact --bbox=papersize --page=1-999 main.pdf -o eq-%p.svg
    donde 'main.pdf" es el nombre del archivo y "eq-%p.svg" es el nombre del archivo con extensión.
-->
    