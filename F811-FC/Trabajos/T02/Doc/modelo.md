# Tarea 02

## Introducción
Dentro de la segunda tarea del curso de Física Computacional (F-811) se empleo el uso de método de diferenciación para el desarrollo de un programa para resolver el problema de los $N$-cuerpos con $N=4$ en dos dimensiones. En dicho programa se emplearon las bibliotecas de `pandas` y `numpy` (también se dejo como comentario la parte de la grafica, pero debido a las escalas utilizadas se omitió dicho apartado) para desarrollar la tabla de resultados con los valores iniciales y finales del sistema.

## Explicación de Código (Derivada Numérica)
### Objeto cuerpo
Genera un cuerpo en base a su masa `masa`, posición `pos` y velocidad inicial `v0` en $2D$
```python
class cuerpo:
    def __init__(self, masa, pos, v0):
        self.m = masa
        self.r = np.array(pos, dtype=float)
        self.v = np.array(v0, dtype=float)
        self.trayectoria = []

    def agregar_posición(self):
        self.trayectoria.append(self.r.copy())
        tem_trayectoria = []
        if len(self.trayectoria) > 500:
            tem_trayectoria = self.trayectoria[1:]
            self.trayectoria = tem_trayectoria
```
dejando la posición y la velocidad como un tipo de dato flotante y declarando la propiedad de ``trayectoria``, así mismo se tiene el método de la posición para la simulación ``def agregar_posición()``.

### Objeto de sistema se particulas
Se encarga de generar el sistema de $N$ partículas, con sus aceleraciones (`def aceleracion()`) y el tipo de método de diferencias (`def diferencias()`) que se empleará para determinar sus valores de posición y velocidad.
```python
class Sistema:
    def __init__(self, cuerpos, G=100.0, eps=1e-3):
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

### Simulador de la animación en PyGame
Encapsula la interfaz pygame a partir de la ventana ``def pantalla()`` y el grid ``def grid()``, el render de las trayectorias ``def dibujar_trayectorias`` y cuerpos ``def dibujar_cuerpos``, y el bucle principal, presentado en subpasos de física por frame.
```python
class Simulador:
    def __init__(self):
        pygame.init()

        self.ventana = pygame.display.set_mode((ANCHO, ALTO))
        pygame.display.set_caption("Simulación de N-Cuerpos")
        self.time = pygame.time.Clock()
        self.colores = [
            (255, 50, 50), 
            (50, 255, 50), 
            (50, 150, 255), 
            (255, 0, 255)
        ]

        cuerpos = [
            cuerpo(100, [ANCHO/2 + 150, ALTO/2], [0.0, 3.0]),
            cuerpo(250, [ANCHO/2 - 400, ALTO/2], [10.0, -5.0]),
            cuerpo(17, [ANCHO/2 + 30, ALTO/2 + 200], [-7.0, 0.0]),
            cuerpo(139, [ANCHO/2 - 150, ALTO/2 - 150], [3.0, 3.0])
        ]

        self.sistema = Sistema(cuerpos, G=100.0, eps=1e-3)
        self.h = 0.1
        self.fps = 1
        self.paso_actual = 0
        self.fuente = pygame.font.Font(None, 24)
        self.fuente_pequena = pygame.font.Font(None, 10)
        

    def pantalla(self, pos):
        return (int(pos[0]), int(pos[1]))
    
    def grid(self):
        gray = (30, 30, 30)

        for x in range(0, ANCHO, 100):
            pygame.draw.line(self.ventana, gray, (x, 0), (x, ALTO), 1)
        for y in range(0, ALTO, 100):
            pygame.draw.line(self.ventana, gray, (0, y), (ANCHO, y), 1)

        # Ejes centrales
        pygame.draw.line(self.ventana, (80, 80, 80), (ANCHO//2, 0), (ANCHO//2, ALTO), 2)
        pygame.draw.line(self.ventana, (80, 80, 80), (0, ALTO//2), (ANCHO, ALTO//2), 2)

    def dibujar_trayectorias(self):
        for i, c in enumerate(self.sistema.cuerpos):
            if len(c.trayectoria) > 1:
                puntos = []
                for pos in c.trayectoria:
                    punto = self.pantalla(pos)
                    if 0 <= punto[0] <= ANCHO and 0 <= punto[1] <= ALTO:
                        puntos.append(punto)

                if len(puntos) > 1:
                    pygame.draw.lines(self.ventana, self.colores[i], False, puntos, 2)

    def dibujar_cuerpos(self):
        for i, c in enumerate(self.sistema.cuerpos):
            pos_pantalla = self.pantalla(c.r)
            if -50 <= pos_pantalla[0] <= ANCHO + 50 and -50 <= pos_pantalla[1] <= ALTO + 50:
                radio = max(5, int(math.sqrt(c.m)))

                pygame.draw.circle(self.ventana, self.colores[i], pos_pantalla, radio)
                pygame.draw.circle(self.ventana, (255, 255, 255), pos_pantalla, radio, 2)

                text = self.fuente_pequena.render(f"C{i+1}", True, (255, 255, 255))
                self.ventana.blit(text, (pos_pantalla[0] + radio +5, pos_pantalla[1] - 10))

                text_masa = self.fuente_pequena.render(f"m={int(c.m)}", True, (200, 200, 200))
                self.ventana.blit(text_masa, (pos_pantalla[0] + radio + 5, pos_pantalla[1] + 5))
    
    def dibujar_info(self):
        text = self.fuente.render("Simulación N-Cuerpos", True, (255, 255, 255))
        self.ventana.blit(text, (10, 10))

        t_sim = self.paso_actual * self.h
        text = self.fuente_pequena.render(f"Tiempo: {t_sim:.2f} | Pasos: {self.paso_actual}", True, (255, 255, 255))
        self.ventana.blit(text, (10, 40))

        y_legend = 70
        text_legend = self.fuente_pequena.render("Cuerpos:", True, (255, 255, 255))
        self.ventana.blit(text_legend, (10, y_legend))

        y_legend += 25
        for i, color in enumerate(self.colores):
            pygame.draw.circle(self.ventana, color, (20, y_legend + i*25), 6)
            pygame.draw.circle(self.ventana, (255, 255, 255), (20, y_legend + i*25), 6, 1)
            text = self.fuente_pequena.render(f"Cuerpo {i+1} (m={int(self.sistema.cuerpos[i].m)})", True, (255, 255, 255))
            self.ventana.blit(text, (35, y_legend + i*25 - 8))

    def actualizar(self):
        for _ in range(self.fps):
            self.sistema.diferencias(self.h, metodo="TPC")
            self.paso_actual += 1

    def dibujar(self):
        self.ventana.fill(background_color)
        self.grid()
        self.dibujar_trayectorias()
        self.dibujar_cuerpos()
        self.dibujar_info()
        pygame.display.flip()

    def exe(self):
        run = True
        while run:
            dt = self.time.tick(60) / 1000
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    run = False
            self.actualizar()
            self.dibujar()
        pygame.quit()
```
Se tiene que ``def actualizar()`` se encarga de la física, utilizando el método de diferencias centradas, y guardar las posiciones en la traza.

### Creación de cuerpos y sistema
Se configuró el sistema con sus N-cuerpos, definiendo la masa, la posición inicial (en función del tamaño de la ventana de simulación) y su velocidad inicial.
```python
def generar_datos():
    print("\n=== Generando datos numéricos ===\n")

    cuerpos = [
        cuerpo(100, [ANCHO/2 + 150, ALTO/2], [0.0, 3.0]),
        cuerpo(250, [ANCHO/2 - 400, ALTO/2], [10.0, -5.0]),
        cuerpo(17, [ANCHO/2 + 30, ALTO/2 + 200], [-7.0, 0.0]),
        cuerpo(139, [ANCHO/2 - 150, ALTO/2 - 150], [3.0, 3.0])
    ]
    h = 0.1
    pasos = 10000
    metodos = ["Def", "TPC", "TPA"]
    datos =[]

    for metodo in metodos:
        sistema_temp = Sistema([cuerpo(c.m, c.r.copy(), c.v.copy()) for c in cuerpos], G=100.0, eps=1e-3)
        
        datos_i = []
        a0 = sistema_temp.aceleracion()
        for i, c in enumerate(sistema_temp.cuerpos):
            datos_i.append({
                "Cuerpo": f"Cuerpo {i+1}",
                "r0": np.round(c.r, 3),
                "v0": np.round(c.v, 3),
                "a0": np.round(a0[i], 3)
            })

        for _ in range(pasos):
            sistema_temp.diferencias(h, metodo=metodo)

        af = sistema_temp.aceleracion()
        datos_f = []
        for i, c in enumerate(sistema_temp.cuerpos):
            datos_f.append({
                "Cuerpo": f"Cuerpo {i+1}",
                "rf": np.round(c.r, 3),
                "vf": np.round(c.v, 3),
                "af": np.round(af[i], 3)
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
``def generar_datos`` se encarga de correr las simulaciones `batch` y exportar el archivo `CSV` con estados iniciales/finales por método empleado.

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
