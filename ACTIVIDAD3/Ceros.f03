
PROGRAM Ceros

  !Programa para encontrar los ceros de la función sin(x).
  

IMPLICIT NONE

INTEGER,parameter::h=selected_real_kind(16,300)
REAL(kind=h):: a,b,pm,fa,fb,fp,u,v
INTEGER::i
REAL(kind=h),External::F
REAL(kind=h)::Tol=1e-5


fa=f(a)
fb=f(b)

!Decir para que sirve.
PRINT*,"Este programa resuelve una ecuacion por el metodo de biseccion."

PRINT*, !Este print en para hacer espacio.

!Pedir el intervalo.
PRINT*,"Escribe el intervalo [a,b]."
READ*,a,b

i=0
DO


u=fa*fb

IF(u>0)THEN
PRINT*,"No hay raices en este intervalo."
END IF
!Sacar el punto medio.

pm=(a+b)/2.
fp=f(pm)

IF(fp==0 .OR. pm<tol)THEN
PRINT*,"La raiz es:",pm
END IF

v=fa*fp
IF(v>0)THEN
a=pm
ELSE
b=pm
END IF
i=i+1
END DO

END PROGRAM Ceros
!***************************************************************
FUNCTION F(x0)

!Declaracion de la funcion y su variable.
IMPLICIT NONE

INTEGER,parameter::h=selected_real_kind(16,300)
REAL(Kind=h)::F,x0

F=Sin(x0)

END FUNCTION 
