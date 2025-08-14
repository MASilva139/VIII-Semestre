""" e02_main.py

    Programa de ejemplo en python 3
    Física computacional ECFM

    Héctor Pérez
    Segundo semestre 2025
"""
from e03_demo import demo

demo_instance_0 = demo()

demo_instance_0.show()

print(demo_instance_0)

demo_instance_1 = demo(2,3)

demo_instance_1.show()

print(demo_instance_1)

demo_instance_1.set_a(10)
demo_instance_1.set_b(30)

print(demo_instance_1)


demo_instance_0.set_a(1)
demo_instance_0.set_b(3)

demo_instance_2 = demo_instance_0 + demo_instance_1

print( demo_instance_2 )