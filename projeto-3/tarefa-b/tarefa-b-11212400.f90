REAL*8 FUNCTION regra_trapezio(a, b, n)
    REAL*8 f, h, a, b, x

    f(x) = dexp(x/2)*dcos(pi*x)

    h = (b-a)/n
    regra_trapezio = 0d0

    DO i = 1, n-1, 2
        x = a + i*h
        regra_trapezio = regra_trapezio + (f(x - h) + 2*f(x) + f(x + h)) * h/2
    END DO

    RETURN
END FUNCTION regra_trapezio

REAL*8 FUNCTION regra_simpson(a, b, n)
    REAL*8 f, h, a, b, x
    
    f(x) = dexp(x/2)*dcos(pi*x)

    h = (b-a)/n
    regra_simpson = 0d0
    
    DO i = 1, n-1, 2
        x = a + i*h
        regra_simpson = regra_simpson + (f(x - h) +4*f(x) +f(x + h)) * h/3
    END DO
    
    RETURN
END FUNCTION regra_simpson

REAL*8 FUNCTION regra_bode(a, b, n)
    REAL*8 f, h, a, b, x
    
    f(x) = dexp(x/2)*dcos(pi*x)

    h = (b-a)/n
    regra_bode = 0d0
    
    DO i = 0, n-4, 4
        x = a + i*h
        regra_bode = regra_bode + (7*f(x) + 32*f(x + h) + 12*f(x + 2*h) + 32*f(x + 3*h) + 7*f(x + 4*h)) * 2*h/45
    END DO
    
    RETURN
END FUNCTION regra_bode

PROGRAM integral
    REAL*8 regra_trapezio, regra_simpson, regra_bode
    REAL*8 pri_f, pi, x, a, b, val_integralf
    PARAMETER (pi = 4*datan(1d0))

    pri_f(x) = (2*dexp(x/2)*(dcos(pi*x) + 2*pi*dsin(pi*x))) / (1 + 4*pi**2)

    a = 0d0
    b = 1d0

    val_integralf = pri_f(a) - pri_f(b)

    OPEN(10, file='saida-b-11212400')
    DO j = 2, 12
        n = 2**j
        WRITE(10,*) (b-a)/n, abs(val_integralf - regra_trapezio(a, b, n)), & 
        abs(val_integralf - regra_simpson(a, b, n)), abs(val_integralf - regra_bode(a, b, n))
    END DO

    CLOSE(10)
    WRITE(*,*) 'Valor da Integral Exata:', val_integralf
END PROGRAM integral