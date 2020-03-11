PROGRAM equa2grau

WRITE (*,*) "Escreva o coeficiente a:"
READ (*,*) a
WRITE (*,*) "Escreva o coeficiente b:"
READ (*,*) b
WRITE (*,*) "Escreva o coeficiente c:"
READ (*,*) c

d = (b**2)-4*a*c

IF (d > 0) THEN
    r1 = (-b+SQRT(d))/(2*a)
        r2 = (-b-SQRT(d))/(2*a)
        
        WRITE (*,*) "Existem 2 raízes reais"
        WRITE (*,*) "Primeira raiz:", r1
        WRITE (*,*) "Segunda raiz:", r2
ELSE IF (d == 0) THEN
        r1 = (-b)/(2*a)
        WRITE (*,*) "Existem 2 raízes reais iguas"
        WRITE (*,*) "Raizes:", r1
ELSE IF (d < 0) THEN
        WRITE (*,*) "Não há raiz real"
END IF

END PROGRAM equa2grau
