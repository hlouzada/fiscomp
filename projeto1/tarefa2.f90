PROGRAM areavetor

WRITE (*,*) "Escreva as coordenadas do 1º vetor"
READ (*,*) a1,a2,a3

WRITE (*,*) "Escreva as coordenadas do 2º vetor"
READ (*,*) b1,b2,b3

anorm = SQRT(a1**2+a2**2+a3**2)
bnorm = SQRT(b1**2+b2**2+b3**2)

theta = ACOS((a1*b1+a2*b2+a3*b3)/(anorm*bnorm))

s = anorm*bnorm*SIN(theta)/2

WRITE (*,*) "A área do triangulo é:", s

END PROGRAM areavetor
