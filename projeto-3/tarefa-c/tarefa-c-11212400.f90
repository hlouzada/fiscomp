PROGRAM tarefac
    real*8 erro, nmax, h, desv_direto, desv_newton, desv_seccante
    real*8 f, df, x, a, b, x_direto, x0_secante, x0_newton, df_secante, x_secante, x_newton
    logical flag_direta, flag_newton, flag_sececante
    
    f(x) = x**3 - 21*x - 20
    df(x) = 3*x**2 - 21

    erro = 10d-6
    nmax = 100
    h = 0.005
    x0_secante = -1.5
    df_secante = -3
    x0_newton = -10
    a = 10
    b = -10
    n = 0

    desv_direto = 2*erro
    desv_newton = 2*erro
    desv_seccante = 2*erro

    flag_direta = .true.
    flag_newton = .true.
    flag_sececante = .true.

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
            x_secante = x0_secante - f(x0_secante)*(x0_secante - df_secante)/(f(x0_secante) - f(df_secante))
            df_secante = x0_secante
            desv_seccante = f(x_secante) - f(x0_secante)
            x0_secante = x_secante
        ELSE
            flag_sececante = .false.
        END IF

        WRITE(*,*) n, x_direto, x_newton, x_secante
        n = n + 1
    END DO
END PROGRAM tarefac