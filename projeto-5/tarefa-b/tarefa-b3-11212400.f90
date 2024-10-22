PROGRAM tarefa_b
    IMPLICIT REAL(8) (a-h,o-z)

    CHARACTER(70) filename
    PARAMETER (pi = 4d0*atan(1d0))

    ! Formatacao para o nome do arquivo de saida
    WRITE(filename,'(A,I0,A)') 'saida-b',3,'-11212400'

    OPEN(10, file=filename)


    ! Definicao das variaveis inicias e constantes
    theta_0 = pi/15
    theta = theta_0
        
    F_0 = 0d0
    gamma = 5d-1
    C_omega = 0d0
    omega = 0d0
    tmax = 1.2d2
    g = 9.8d0
    a_l = 9.8d0
    e = 1d-4
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

        WRITE(10, '(F0.8,1(" ",F0.8))') e*i, theta ! Escrever as variaveis no arquivo de saida
            
        i = i + 1
    END DO

    CLOSE(10)

END PROGRAM tarefa_b
