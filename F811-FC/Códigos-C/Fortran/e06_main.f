* example06 (main)
*
* Programa de ejemplo en Fortran 77
* Soluciones formula cuadratica
* Hector Perez
* Fisica computacional 2s-2025

      PROGRAM example06
      INTEGER A, B, C
      REAL X1R, X1C, X2R, X2C
      READ (*,*) A, B, C
      CALL QUADSOLVE( A, B, C, X1R, X1C, X2R, X2C)
      WRITE (*,*) 'X1= ',X1R,' + ',X1C,'i ; X2= ',X2R,' + ',X2C,'i'
      STOP
      END