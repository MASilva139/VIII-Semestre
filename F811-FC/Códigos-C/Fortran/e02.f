************************************
* example02
*
* Programa de ejemplo en Fortran IV
* Soluciones formula cuadratica
* Hector Perez
* Fisica computacional 2s-2025
************************************
      PROGRAM example02
      INTEGER A,B,C,DISCR
	  REAL X1R,X1C,X2R
      READ(5,101) A,B,C
  101 FORMAT(3I5)
      DISCR = B**2-4*A*C
      IF(DISCR.EQ.0) GO TO 20
	  IF(DISCR.GT.0) GO TO 30
      X1R=-B/(2*A+0.0)
      X1C=-SQRT(-DISCR+0.0)/(2*A)
      WRITE(6,102) X1R,X1C,X1R,-X1C
      GO TO 40
   20 X1R=-B/(2*A+0.0)
      X1C=0
      WRITE(6,102) X1R,X1C,X1R,-X1C
      GO TO 40
   30 X1R=-B/(2*A+0.0)-SQRT(DISCR+0.0)/(2*A)
      X2R=-B/(2*A+0.0)+SQRT(DISCR+0.0)/(2*A)
      X1C=0
      WRITE(6,102) X1R,X1C,X2R,-X1C
  102 FORMAT(4H X1= ,F5.2,3H + ,F5.2,3H I;,4H X2= ,F5.2,3H + ,F5.2,2H I)
   40 STOP
      END