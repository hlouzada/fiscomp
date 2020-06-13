PROGRAM tarefa_d
    IMPLICIT REAL(8) (a-h,o-z)
    PARAMETER (pi = 4d0*atan(1d0))

    ! Definicao das constantes
    theta_0 = pi/15

    CALL geracao_dado_angular_poincare(theta_0, 5d-1, 1, 1)          ! Utilizando theta_0(1) = pi/15     e     F_0(1) = 0.5
    CALL geracao_dado_angular_poincare(theta_0 + 1d-3, 5d-1, 2, 1)   ! Utilizando theta_0(2) = pi/15 + 0.001 e F_0(1) = 0.5
    CALL geracao_dado_angular_poincare(theta_0, 1.2d0, 1, 2)         ! Utilizando theta_0(1) = pi/15     e     F_0(2) = 1.2
    CALL geracao_dado_angular_poincare(theta_0 + 1d-3, 1.2d0, 2, 2)  ! Utilizando theta_0(2) = pi/15 + 0.001 e F_0(2) = 1.2
    
CONTAINS

    ! Subrotina para gerar a velocidade e a fumcao angular dado theta_0 e F_0 de uma secao de poncare
    SUBROUTINE geracao_dado_angular_poincare(theta_0,F_0,j,k)
        IMPLICIT REAL(8) (a-h,o-z)
        PARAMETER (pi = 4d0*atan(1d0))
        CHARACTER(70) filename
    
        ! Formatacao para o nome do arquivo de saida da velocidade(j) e posicao(j) angular
        WRITE(filename,'(A,2(I0,A))') 'saida-e-theta0-',j,'-F0-',k,'-11212400'
        OPEN(10, file=filename)

        ! Definicao das variaveis inicias e constantes
        theta = theta_0
        
        gamma = 5d-1
        C_omega = 2d0/3d0
        omega = 0d0
        tmax = 5.2d3
        g = 9.8d0
        a_l = 9.8d0
        a_m = 1d0
        e = 3d-2 ! delta_t
        i = 0
        n = 1

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

            ! Calculo da secao de Poincare
            IF(abs(i*e-((n*pi)/C_omega)).LT.(e/2d0)) THEN
                
                WRITE(10,'(F0.16,1(" ",F0.16))') theta, omega

                n = n + 1
            END IF

            i = i + 1
        END DO

        CLOSE(10)

    END SUBROUTINE geracao_dado_angular_poincare

END PROGRAM tarefa_d