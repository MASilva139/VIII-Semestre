import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

class cuerpo:
    def __init__(self, masa, pos, v0):
        self.m = masa
        self.r = np.array(pos, dtype=float)
        self.v = np.array(v0, dtype=float)

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
            
class Diferenciacion:
    def __init__(self, h = 1e-3):
        self.h = h
    def derivada_TPC(self, f, x):
        return (f(x + self.h) - f(x - self.h)) / (2 * self.h)
    def derivada_TPA(self, f, x):
        return (-f(x + 2 * self.h) + 4 * f(x + self.h) - 3 * f(x)) / (2 * self.h)
    def derivada_Def(self, f, x):
        return (f(x + self.h) - f(x)) / self.h

def main():
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

    # trayectorias = [[] for _ in range(len(cuerpos))]
    # for _ in range(pasos):
    #     for i, c in enumerate(sistema_temp.cuerpos):
    #         trayectorias[i].append(c.r.copy())
    #     sistema_temp.diferencias(h, metodo="TPC")

    # trayectorias = [np.array(tray) for tray in trayectorias]

    # plt.figure(figsize=(10, 8))
    # colores = ['r', 'g', 'b', 'm']
    # for i, tray in enumerate(trayectorias):
    #     plt.plot(tray[:, 0], tray[:, 1], color=colores[i], label=f'Cuerpo {i+1}')
    #     plt.scatter(tray[0, 0], tray[0, 1], color=colores[i], marker='o')  # Posición inicial
    #     plt.scatter(tray[-1, 0], tray[-1, 1], color=colores[i], marker='x')  # Posición final
    # plt.title('Trayectorias de los cuerpos en el problema de los 4 cuerpos')
    # plt.xlabel('x')
    # plt.ylabel('y')
    # plt.legend()
    # plt.grid(True)
    # plt.axis('equal')
    # plt.show()

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
        # posiciones = np.array([c.r for c in sistema_temp.cuerpos])
        # datos[metodo].append(posiciones)

        df = pd.DataFrame({
            # "Cuerpo": [f"Cuerpo {i+1}" for i in range(len(cuerpos))],
            # "Norma_Def": [np.linalg.norm(pos) for pos in datos["Def"][0]],
            # "Norma_TPC": [np.linalg.norm(pos) for pos in datos["TPC"][0]],
            # "Norma_TPA": [np.linalg.norm(pos) for pos in datos["TPA"][0]],
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

if __name__ == "__main__":
    main()