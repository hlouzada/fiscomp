PROGRAM raiz
    real*8 erro, nmax, desv_direto, desv_newton, desv_seccante
    real*8 f, df, x, a, b, x_direto, x1_secante, x0_newton, x0_secante, x_secante, x_newton
    logical flag_direta, flag_newton, flag_sececante
    
    f(x) = x**3 - 21*x - 20
    df(x) = 3*x**2 - 21

    erro = 10d-6
    nmax = 100
    n = 0

    desv_direto = 2*erro
    desv_newton = 2*erro
    desv_seccante = 2*erro

    flag_direta = .true.
    flag_newton = .true.
    flag_sececante = .true.

    WRITE(*,*) "Digite os valores que compreendem as raizes para a busca direta (ex: 10 -10):"
    READ(*,*) a, b
    WRITE(*,*) "Digite os valores de X_0 para o metodo de Newton-Raphson (ex: -10):"
    READ(*,*) x0_newton
    WRITE(*,*) "Digite os valores de X_0 e X_(-1) para o metodo da secante (ex: -2 -2.5):"
    READ(*,*) x1_secante, x0_secante

    OPEN(10, file='saida-c-11212400')
    DO WHILE ( (n <= nmax) .and. (flag_direta .or. flag_newton .or. flag_sececante) )
        IF ( desv_direto**2 .gt. erro**2 ) THEN
            x_direto = (a+b)/2
            IF ( f(x_direto)*f(a) .gt. 0 ) THEN
                a = x_direto
            ELSE
                b = x_direto
            END IF
            desv_direto = f(a)-f(b)
        ELSE
            flag_direta = .false.
        END IF

        IF ( desv_newton**2 .gt. erro**2 ) THEN
            x_newton = x0_newton - f(x0_newton)/df(x0_newton)
            desv_newton = f(x_newton) - f(x0_newton)
            x0_newton = x_newton
        ELSE
            flag_newton = .false.
        END IF
            
        IF ( desv_seccante**2 .gt. erro**2 ) THEN
            x_secante = x1_secante - f(x1_secante)*(x1_secante - x0_secante)/(f(x1_secante) - f(x0_secante))
            x0_secante = x1_secante
            desv_seccante = f(x_secante) - f(x1_secante)
            x1_secante = x_secante
        ELSE
            flag_sececante = .false.
        END IF

        WRITE(10,*) n, x_direto, x_newton, x_secante
        WRITE(*,*) n, x_direto, x_newton, x_secante
        n = n + 1
    END DO
END PROGRAM raiz