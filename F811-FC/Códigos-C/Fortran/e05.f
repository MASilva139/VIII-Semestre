************************************
* example05
*
* Programa de ejemplo en Fortran IV
* Soluciones formula cuadratica
* Hector Perez
* Fisica computacional 2s-2025
************************************
      PROGRAM example02
      INTEGER A,B,C
	   REAL X1R,X1C,X2R,X2C
      READ(5,101) A,B,C
  101 FORMAT(3I5)
      CALL QUADSOLVE(A,B,C,X1R,X1C,X2R,X2C)
      WRITE(6,102) X1R,X1C,X2R,X2C
  102 FORMAT(4H X1= ,F5.2,3H + ,F5.2,3H I;,4H X2= ,F5.2,3H + ,F5.2,2H I)
      STOP
      END
      SUBROUTINE QUADSOLVE(A,B,C,X1R,X1C,X2R,X2C)
      INTEGER A,B,C,DISCR
	   REAL X1R,X1C,X2R,X2C
      DISCR = B**2-4*A*C
      IF(DISCR.EQ.0) GO TO 20
	   IF(DISCR.GT.0) GO TO 30
      X1R=-B/(2*A+0.0)
      X1C=-SQRT(-DISCR+0.0)/(2*A)
      X2R=X1R
      X2C=-X1C
      GO TO 50
   20 X1R=-B/(2*A+0.0)
      X1C=0
      X2R=X1R
      X2C=0
      GO TO 50
   30 X1R=-B/(2*A+0.0)-SQRT(DISCR+0.0)/(2*A)
      X2R=-B/(2*A+0.0)+SQRT(DISCR+0.0)/(2*A)
      X1C=0
      X2C=0
   50 RETURN
      END
