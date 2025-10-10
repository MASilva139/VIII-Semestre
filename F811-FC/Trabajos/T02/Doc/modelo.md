# Tarea 02

## Introducción
Dentro de la segunda tarea del curso de Física Computacional (F-811) se empleo el uso de método de diferenciación para el desarrollo de un programa para resolver el problema de los $N$-cuerpos con $N=4$ en dos dimensiones. En dicho programa se emplearon las bibliotecas de `pandas` y `numpy` (también se dejo como comentario la parte de la grafica, pero debido a las escalas utilizadas se omitió dicho apartado) para desarrollar la tabla de resultados con los valores iniciales y finales del sistema.

## Explicación de Código (Derivada Numérica)
### Objeto cuerpo
Genera un cuerpo en base a su masa `masa`, posición `pos` y velocidad inicial `v0`
```python
class cuerpo:
    def __init__(self, masa, pos, v0):
        self.m = masa
        self.r = np.array(pos, dtype=float)
        self.v = np.array(v0, dtype=float)
```
dejando la posición y la velocidad como un tipo de dato flotante.

### Objeto de sistema se particulas
Se encarga de generar el sistema de $N$ partículas, con sus aceleraciones (`def aceleracion()`) y el tipo de método de diferencias (`def diferencias()`) que se empleará para determinar sus valores de posición y velocidad.
```python
class Sistema:
    def __init__(self, cuerpos, G=6.67e-11, eps=1e-3):
        self.cuerpos = cuerpos
        self.N = len(cuerpos)
        self.G = G
        self.eps = eps
    def aceleracion(self):
        a = np.zeros((self.N, 2), dtype=float)
        for i in range(self.N):
            for j in range(self.N):
                if i==j:
                    continue
                dr = self.cuerpos[j].r - self.cuerpos[i].r
                rad = np.dot(dr,dr) + self.eps**2
                radio = rad**(-1.5)
                a[i] += self.G*self.cuerpos[j].m*dr*radio
        return a
    def diferencias(self, h, metodo="TPC"): # Diferencias centradas
        a = self.aceleracion()
        for i in range(self.N):
            if metodo == "Def": # Diferencias hacia adelante
                self.cuerpos[i].r += h*self.cuerpos[i].v
                self.cuerpos[i].v += h*a[i]
            elif metodo == "TPC": # Tres puntos centrales
                # r(t+h) = r(t)+h*v(t)+0.5*h^2*a(t)
                self.cuerpos[i].r += h*self.cuerpos[i].v + 0.5 * h**2 *a[i]
                self.cuerpos[i].v += h*a[i]
            elif metodo == "TPA": # Tres puntos hacia atras
                # r(t+h) ≈ r(t) + h*v(t) - 0.5*h^2*a(t)
                self.cuerpos[i].r += h*self.cuerpos[i].v - 0.5 * h**2 *a[i]
                self.cuerpos[i].v += h*a[i]
            else:
                raise ValueError("Método no reconocido")
```

### Objetos de diferenciación
En este se definen las diferencias numéricas con tres puntos hacia adelante (`def derivada_Def`), tres puntos centrales (`def derivada_TPC`) y tres puntos hacia atras (`def derivada_TPA`)
```python
class Diferenciacion:
    def __init__(self, h = 1e-3):
        self.h = h
    def derivada_TPC(self, f, x):
        return (f(x + self.h) - f(x - self.h)) / (2 * self.h)
    def derivada_TPA(self, f, x):
        return (-f(x + 2 * self.h) + 4 * f(x + self.h) - 3 * f(x)) / (2 * self.h)
    def derivada_Def(self, f, x):
        return (f(x + self.h) - f(x)) / self.h
```

### Creación de cuerpos y sistema
Se configuró el sistema con sus N-cuerpos, definiendo la masa, la posición inicial y su velocidad inicial.
```python
cuerpos = [
    cuerpo(1.0e26, [1.0e8, 0.0], [0.0, 1.0e5]),
    cuerpo(1.0e16, [-1.0e8, 0.0], [0.0, -1.0e3]),
    cuerpo(0.5, [0.0, 1.5e5], [0.0, 0.0]),
    cuerpo(7.3e13, [-5.1e12, -1.5e15], [0.0, 0.0])
]

sistema = Sistema(cuerpos, G=6.67e-11, eps=1e3)
h = 1e-3
pasos = 10000

sistema_temp = Sistema([cuerpo(c.m, c.r.copy(), c.v.copy()) for c in cuerpos], G=6.67e-11, eps=1e3)
```

### Generación de Tabla de Resultados
Registra los datos de las posiciones, velocidades y aceleraciones de cada método usado.
```python
metodos = ["Def", "TPC", "TPA"]

datos =[]

for metodo in metodos:
    sistema_temp = Sistema([cuerpo(c.m, c.r.copy(), c.v.copy()) for c in cuerpos], G=6.67e-11, eps=1e3)
    
    datos_i = []
    a0 = sistema_temp.aceleracion()
    for i, c in enumerate(sistema_temp.cuerpos):
        datos_i.append({
            "Cuerpo": f"Cuerpo {i+1}",
            "r0": np.round(c.r, 6),
            "v0": np.round(c.v, 6),
            "a0": np.round(a0[i], 6)
        })

    for _ in range(pasos):
        sistema_temp.diferencias(h, metodo=metodo)

    af = sistema_temp.aceleracion()
    datos_f = []
    for i, c in enumerate(sistema_temp.cuerpos):
        datos_f.append({
            "Cuerpo": f"Cuerpo {i+1}",
            "rf": np.round(c.r, 6),
            "vf": np.round(c.v, 6),
            "af": np.round(af[i], 6)
        })

    df = pd.DataFrame({
        "Cuerpo": [d["Cuerpo"] for d in datos_i],
        "r0 (x,y)": [d["r0"] for d in datos_i],
        "v0 (x,y)": [d["v0"] for d in datos_i],
        "a0 (x,y)": [d["a0"] for d in datos_i],
        "rf (x,y)": [d["rf"] for d in datos_f],
        "vf (x,y)": [d["vf"] for d in datos_f],
        "af (x,y)": [d["af"] for d in datos_f],
    })

    print(f"\n=== Resultados para el método {metodo} ===\n")
    print(df.to_string(index=False))

    df["Metodo"] = metodo
    datos.append(df)

df_final = pd.concat(datos, ignore_index=True)
print("\n=== Tabla final ===\n")
print(df_final)

df_final.to_csv("F811-FC\Trabajos\T02\Resultados.csv", index=False)
```
Se decidió generar un csv para que se puedan observar todos los valores obtenidos, dado que la terminal no puede mostrar todos los valores de la tabla final `df_final`.

<[Regresar](/F811-FC/Trabajos/T02/)>
