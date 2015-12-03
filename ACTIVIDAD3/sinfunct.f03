Program sinfunct

   !Este programa es para calcular la función Sin(x) en el intervalo [0,2pi]

!Declaración de variables.
IMPLICIT NONE

INTEGER,parameter::h=selected_real_kind(16,300)
REAL(kind=h)::x,fx,fxx,dx,taylor
INTEGER::i,npts
REAL(kind=h), parameter::pi=4.0*Atan(1.0)
REAL(Kind=h), parameter::ep=1.0e-6

  PRINT*, "Dame el número de puntos del intervalo."
  READ*,npts

 dx=(2.0*pi)/float(npts)
  PRINT*,"dx=",dx

 x=0

 Do i=1,npts+1
   x=dx*float(i-1)
   fx=sin(x)

  IF(ABS(fx).le.ep)THEN
  PRINT*,"X es un cero de la función."

ENDIF
ENDDO

   DO i=1,npts+1
    x=dx*float(i-1)
    fxx=cos(x)

  IF(ABS(fxx).le.ep)THEN
   PRINT*,"X es un cero de la derivada."

ENDIF
ENDDO

   DO i=1,1
   x=pi/8.0

   Taylor=x-(x**3.0/(3.0*2.0))

PRINT*, "Error", (1-(taylor/sin(x)))*100,"%"

ENDDO

END PROGRAM sinfunct
