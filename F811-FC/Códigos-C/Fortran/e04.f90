! example04
!
! Programa de ejemplo en Fortran 90
! Soluciones formula cuadratica
! Hector Perez
! Fisica computacional 2s-2025

program example04

   implicit none

   integer :: A, B, C, DISCR
   real :: X1R,X1C,X2R
   
   read *, A, B, C

   DISCR = B**2-4*A*C
   
   if ( DISCR < 0 )  then
      X1R=-B/(2*A+0.0)
      X1C=-sqrt(-DISCR+0.0)/(2*A)
      print *, 'X1= ', X1R, ' + ', X1C, 'i ; X2= ', X1R, ' + ', -X1C, 'i'
   else if ( DISCR == 0 ) then
      X1R=-B/(2*A+0.0)
      X1C=0
      print *, 'X1= ', X1R, ' + ', X1C, 'i ; X2= ', X1R, ' + ', -X1C, 'i'
   else if ( DISCR > 0 ) then
      X1R=-B/(2*A+0.0)-sqrt(DISCR+0.0)/(2*A)
      X2R=-B/(2*A+0.0)+sqrt(DISCR+0.0)/(2*A)
      X1C=0
      print *, 'X1= ', X1R, ' + ', X1C, 'i ; X2= ', X2R, ' + ', -X1C, 'i'
!      write (*, '(4H X1= ,F5.2,3H + ,F5.2,3H I;,4H X2= ,F5.2,3H + ,F5.2,2H I)') X1R, X1C, X2R, X1C
   end if

end program