PROGRAM tarefa_b

    CALL calculo_pendulo(0d0, 1)   ! Utilizando F = 0
    CALL calculo_pendulo(5d-1, 2)  ! Utilizando F = 0.5
    CALL calculo_pendulo(1.2d0, 3) ! Utilizando F = 1.2

CONTAINS

    ! Calculo das funcoes angulares do pendulo utilizando o metodo Euler-Cromer Com Correcao
    SUBROUTINE calculo_pendulo(F_0, j)
        IMPLICIT REAL(8) (a-h,o-z)

        CHARACTER(70) filename
        PARAMETER (pi = 4d0*atan(1d0))

        ! Formatacao para o nome do arquivo de saida
        WRITE(filename,'(A,I0,A)') 'saida-b4-Fo',j,'-11212400'

        OPEN(10, file=filename)


        ! Definicao das variaveis inicias e constantes
        theta_0 = pi/15
        theta = theta_0
    
        gamma = 5d-1
        C_omega = 2d0/3d0
        omega = 0d0
        S_T = 0d0
        tmax = 3.6d2
        g = 9.8d0
        a_l = 9.8d0
        a_m = 1d0
        e = 3d-2
        t_0 = 0d0
        i = 0

        ! Calculo da velocidade e da posicao angular em funcao da variacao do tempo, enquanto o tempo for menor que tmax
        ! Pendulo amortecido forcado
        DO WHILE (i*e.LT.tmax)
            theta = mod(theta, 2*pi) ! garantindo que 0 <= theta <= 2pi

            omega_i = omega
            theta_i = theta
            
            ! aceleracao angular
            a = -((g/a_l)*sin(theta_i))-(gamma*omega_i)+(F_0*sin(C_omega*i*e))

            omega = omega_i + a*e
            theta = theta_i + omega*e

            WRITE(10, '(F0.8,2(" ",F0.8))') e*i, theta, omega ! Escrever as variaveis no arquivo de saida

            IF (omega_i*omega.LT.0) THEN
                IF (t_0.GE.0) THEN
                    S_T = i*e - t_0
                END IF
                t_0 = i*e
            END IF
            
            i = i + 1
        END DO

        CLOSE(10)

        S_T = 2*S_T ! calculo do periodo pelo metodo de Euler-Cromer com correcao

        WRITE(*,'(A,I0,A,F0.6)') "Perido para F_0(",j,"): ", S_T

    END SUBROUTINE

END PROGRAM tarefa_b
