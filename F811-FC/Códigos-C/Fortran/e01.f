      PROGRAM example01
C Programa de ejemplo en Fortran II
C Soluciones formula cuadratica
C Hector Perez
C Fisica computacional 2s-2025
      READ(5,101) IA,IB,IC
  101 FORMAT(3I5)
      IDISCR = IB**2-4*IA*IC
      IF(IDISCR) 10,20,30
   10 RX1R=-IB/(2*IA+0.0)
      RX1C=-SQRT(-IDISCR+0.0)/(2*IA)
      WRITE(6,102) RX1R,RX1C,RX1R,-RX1C
      GO TO 40
   20 RX1R=-IB/(2*IA+0.0)
      RX1C=0
      WRITE(6,102) RX1R,RX1C,RX1R,-RX1C
      GO TO 40
   30 RX1R=-IB/(2*IA+0.0)-SQRT(IDISCR+0.0)/(2*IA)
      RX2R=-IB/(2*IA+0.0)+SQRT(IDISCR+0.0)/(2*IA)
      RX1C=0
      WRITE(6,102) RX1R,RX1C,RX2R,-RX1C
  102 FORMAT(4H X1= ,F5.2,3H + ,F5.2,3H I;,4H X2= ,F5.2,3H + ,F5.2,2H I)
   40 STOP
      END