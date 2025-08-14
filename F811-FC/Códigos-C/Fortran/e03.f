* example04
*
* Programa de ejemplo en Fortran 77
* Soluciones formula cuadratica
* Hector Perez
* Fisica computacional 2s-2025

      PROGRAM example04
      INTEGER A, B, C, DISCR
      REAL X1R,X1C,X2R
      READ (*,*) A, B, C
      DISCR = B**2-4*A*C
      IF ( DISCR.LT.0 )  THEN
         X1R=-B/(2*A+0.0)
         X1C=-SQRT(-DISCR+0.0)/(2*A)
         WRITE (*,*) 'X1= ', X1R, ' + ', X1C, 'i ; X2= ', X1R, ' + ', -X1C, 'i'
      ELSE IF ( DISCR.EQ.0 ) THEN
         X1R=-B/(2*A+0.0)
         X1C=0
         WRITE (*,*) 'X1= ', X1R, ' + ', X1C, 'i ; X2= ', X1R, ' + ', X1C, 'i'
      ELSE IF ( DISCR.GT.0 ) THEN
         X1R=-B/(2*A+0.0)-SQRT(DISCR+0.0)/(2*A)
         X2R=-B/(2*A+0.0)+SQRT(DISCR+0.0)/(2*A)
         X1C=0
         WRITE (*,*) 'X1= ', X1R, ' + ', X1C, 'i ; X2= ', X2R, ' + ', X1C, 'i'
      END IF
      STOP
      END