PROGRAM tarefa_d
    IMPLICIT REAL(8) (a-h,o-z)
    PARAMETER (pi = 4d0*atan(1d0))

    ! Definicao das constantes
    e = 3d-2 ! delta_t
    theta_0 = pi/15

    CALL geracao_dado_angular(e,theta_0, 5d-1, 1, 1)          ! Utilizando theta_0(1) = pi/15     e     F_0(1) = 0.5
    CALL geracao_dado_angular(e,theta_0 + 1d-3, 5d-1, 2, 1)   ! Utilizando theta_0(2) = pi/15 + 0.001 e F_0(1) = 0.5
    CALL geracao_dado_angular(e,theta_0, 1.2d0, 1, 2)         ! Utilizando theta_0(1) = pi/15     e     F_0(2) = 1.2
    CALL geracao_dado_angular(e,theta_0 + 1d-3, 1.2d0, 2, 2)  ! Utilizando theta_0(2) = pi/15 + 0.001 e F_0(2) = 1.2

CONTAINS

    ! Funcao para gerar a velocidade e a fumcao angular dado theta_0 e F_0, e retorna a quantidade de valores gerados
    SUBROUTINE geracao_dado_angular(e,theta_0,F_0,j,k)
        IMPLICIT REAL(8) (a-h,o-z)
        PARAMETER (pi = 4d0*atan(1d0))
        CHARACTER(70) filename
    
        ! Formatacao para o nome do arquivo de saida da velocidade(j) e posicao(j) angular
        WRITE(filename,'(A,2(I0,A))') 'saida-d-theta0-',j,'-F0-',k,'-11212400'

        OPEN(10, file=filename)

        ! Definicao das variaveis inicias e constantes
        theta = theta_0
        
        gamma = 5d-1
        C_omega = 2d0/3d0
        omega = 0d0
        tmax = 1.2d2
        g = 9.8d0
        a_l = 9.8d0
        a_m = 1d0
        i = 0

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

            WRITE(10,'(F0.8,1(" ",F0.8))') theta, omega ! Escrever as variaveis no arquivo de saida

            i = i + 1
        END DO

        CLOSE(10)
        
    END SUBROUTINE geracao_dado_angular

END PROGRAM tarefa_d