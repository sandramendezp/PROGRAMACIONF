PROGRAM Dis

  !Calcular el periodo y distancia.

  !Declaración de variables.
  IMPLICIT NONE

  INTEGER, parameter::h=selected_real_kind(16,300)
  REAL(kind=h)::t,r,d,rT
  REAL(kind=h), parameter::pi=4.0*Atan(1.0)
  
  PRINT*,"Dame el valor de t, de R."
  READ*,t,r

d=ABS(EXP(2*pi*t)-r*EXP(2*pi*r*(3/2)))
  
PRINT*,"La distancia es:",d


   rT=(3/2)*r

   PRINT*,"El periodo es:", rT


 END PROGRAM DIS
