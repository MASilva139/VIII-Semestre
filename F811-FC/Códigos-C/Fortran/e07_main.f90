! example07 main
!
! Programa de ejemplo en Fortran 90
! Soluciones formula cuadratica
! Hector Perez
! Fisica computacional 2s-2025

program example07

   implicit none

   integer :: A, B, C
   complex :: X1, X2
   
   read *, A, B, C

   call quadsolve( A, B, C, X1, X2 )

   print *, 'X1= ', X1, '; X2= ', X2
!   write (*, '(4H X1= ,F5.2,3H + ,F5.2,3H I;,4H X2= ,F5.2,3H + ,F5.2,2H I)') X1R, X1C, X2R, X1C

end program