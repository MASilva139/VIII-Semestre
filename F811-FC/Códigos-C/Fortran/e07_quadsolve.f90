! example07 quadsolve
!
! Programa de ejemplo en Fortran 90
! Soluciones formula cuadratica
! Hector Perez
! Fisica computacional 2s-2025

subroutine quadsolve( A, B, C, X1, X2 )

   implicit none

   integer :: A, B, C, DISCR
   complex :: X1, X2

   DISCR = B**2-4*A*C
   
   if ( DISCR < 0 )  then
      X1%re=-B/(2*A+0.0)
      X1%im=-sqrt(-DISCR+0.0)/(2*A)
      X2=conjg(X1)
   else if ( DISCR == 0 ) then
      X1%re=-B/(2*A+0.0)
      X1%im=0
      X2=conjg(X1)
   else if ( DISCR > 0 ) then
      X1%re=-B/(2*A+0.0)-sqrt(DISCR+0.0)/(2*A)
      X1%im=-B/(2*A+0.0)+sqrt(DISCR+0.0)/(2*A)
      X2=conjg(X1)
   end if

end subroutine quadsolve