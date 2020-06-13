PROGRAM tarefa_b
    IMPLICIT REAL(8) (a-h,o-z)

    CHARACTER(70) filename
    PARAMETER (pi = 4d0*atan(1d0))

    ! Formatacao para o nome do arquivo de saida
    WRITE(filename,'(A,I0,A)') 'saida-b',1,'-11212400'

    OPEN(10, file=filename)

    DO j = 1, 10

        ! Definicao das variaveis inicias e constantes
        theta_0 = mod(rand() + 1d-2, pi/2)
        theta = theta_0
        
        F_0 = 0d0
        gamma = 0d0
        C_omega = 0d0 ! OMEGA
        omega = 0d0 ! omega
        S_T = 0d0
        tmax = 1.2d2
        g = 9.8d0
        a_l = 9.8d0
        a_m = 1d0
        e = 1d-4 ! delta_t
        t_0 = 0d0
        i = 0
        n = 0

        ! Calculo da velocidade e da posicao angular em funcao da variacao do tempo, enquanto o tempo for menor que tmax
        ! Pendulo amortecido forcado
        ! Utilizando o metodo Euler-Cromer Com Correcao
        DO WHILE (i*e.LT.tmax)
            theta = mod(theta, 2*pi) ! garantindo que 0 <= theta <= 2pi

            omega_i = omega
            theta_i = theta

            ! aceleracao angular
            a = -((g/a_l)*sin(theta_i))-(gamma*omega_i)+(F_0*sin(C_omega*i*e))

            omega = omega_i + a*e
            theta = theta_i + omega*e

            IF (omega_i*omega.LT.0) THEN
                IF (t_0.GE.0) THEN
                    S_T = S_T + i*e - t_0
                    n = n + 1
                END IF
                t_0 = i*e
            END IF

            i = i + 1
        END DO

        S_T = 2*S_T/n ! calculo do periodo pelo metodo de Euler-Cromer com correcao

        ! Calculo numerico da integral eliptica para o periodo
        T_solucaoNum = simpson(f, e - theta_0, theta_0 - e, i, theta_0) + 2*analitico(theta_0, a_l, g, e)

        ! Calculo do Periodo
        T = 2*pi*sqrt(a_l/g)

        WRITE(10, '(F0.6,3(" ",F0.6))') theta_0, S_T, T_solucaoNum, T ! Escrever as variaveis no arquivo de saida


    END DO

    CLOSE(10)

CONTAINS
    ! Funcao da integral eliptica
    FUNCTION f(theta, theta_0)
        IMPLICIT REAL(8) (a-h,o-z)
        g = 9.8
        a_l = 9.8
        
        f = sqrt(2*a_l/g)/sqrt(cos(theta)-cos(theta_0))
        
        RETURN
    END FUNCTION

    ! Funcao para o calculo da parte analitica da integral eliptica
    FUNCTION analitico(theta_0, a_l, g, e)
        IMPLICIT REAL(8) (a-h,o-z)

        analitico = sqrt(2*a_l/g)*sqrt(e)*2/sqrt(sin(theta_0))

        RETURN
    END FUNCTION

    ! Funcao para o calculo da parte numerica da integral eliptica utilizando o metodo de simpson
    FUNCTION simpson(f, a, b, n, y)
        IMPLICIT REAL(8) (a-h,o-z)

        h = (b-a)/n
        simpson = 0e0

        DO i = 1, n-1, 2
            x = a + i*h
            simpson = simpson + ( f(x -h, y) +4*f(x, y) +f(x +h, y) ) * h/3
        END DO

        RETURN
    END FUNCTION simpson

END PROGRAM tarefa_b
