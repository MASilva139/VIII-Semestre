* example06 quadsolve
*
* Programa de ejemplo en Fortran 77
* Soluciones formula cuadratica
* Hector Perez
* Fisica computacional 2s-2025

      SUBROUTINE QUADSOLVE( A, B, C, X1R, X1C, X2R, X2C )
      INTEGER A, B, C, DISCR
      REAL X1R, X1C, X2R, X2C 
      DISCR = B**2-4*A*C
      IF ( DISCR.LT.0 )  THEN
         X1R=-B/(2*A+0.0)
         X2R=X1R
         X1C=-SQRT(-DISCR+0.0)/(2*A)
         X2C=-X1C
      ELSE IF ( DISCR.EQ.0 ) THEN
         X1R=-B/(2*A+0.0)
         X2R=X1R
         X1C=0
         X2C=0
      ELSE IF ( DISCR.GT.0 ) THEN
         X1R=-B/(2*A+0.0)-SQRT(DISCR+0.0)/(2*A)
         X2R=-B/(2*A+0.0)+SQRT(DISCR+0.0)/(2*A)
         X1C=0
         X2C=0
      END IF
      RETURN
      END