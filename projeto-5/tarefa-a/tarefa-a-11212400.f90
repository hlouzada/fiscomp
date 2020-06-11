PROGRAM tarefa_a
    IMPLICIT REAL(8) (a-h,o-z)
    PARAMETER (pi = 4d0*atan(1d0))
    CHARACTER(70) filename

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Solucao Inadequada - Metodo de Euler-Cromer !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Definicao das variaveis inicias e constantes
    tmax = 1.2d2
    g = 9.8d0
    a_l = 9.8d0
    a_m = 1d0
    theta = pi/15
    e = 1d-4
    i = 0

    ! Formatacao para o nome do arquivo de saida
    WRITE(filename,'(A,I0,A)') 'saida-a',1,'-11212400'

    OPEN(10, file=filename)


    ! Calculo da velocidade e da posicao angular em funcao da variacao do tempo, enquanto o tempo for menor que tmax
    DO WHILE (i*e.LT.tmax)
        theta = mod(theta, 2*pi) ! garantindo que 0 <= theta <= 2pi

        omega_i = omega
        theta_i = theta

        omega = omega_i - g*theta_i*e/a_l
        theta = theta_i + omega_i*e

        E = 0.5*a_m*(omega_i*a_l)**2 + a_m*g*a_l*(1-cos(theta_i)) ! Calculo da Energia do sistema (Potencial + Cinetica)
        
        WRITE(10, '(F0.6,2(" ",F0.6))') e*i, theta, E ! Escrever as variaveis no arquivo de saida
        
        i = i + 1
    END DO

    CLOSE(10)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Solucao Adequada - Metodo de Euler-Cromer Com Correcao !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Definicao das variaveis inicias, constantes ja definias
    omega = 0d0
    theta = pi/15
    i = 0

    WRITE(filename,'(A,I0,A)') 'saida-a',2,'-11212400'

    OPEN(10, file=filename)


    ! Calculo da velocidade e da posicao angular em funcao da variacao do tempo, enquanto o tempo for menor que tmax
    DO WHILE (i*e.LT.tmax)
        theta = mod(theta, 2*pi) ! garantindo que 0 <= theta <= 2pi

        omega_i = omega
        theta_i = theta

        omega = omega_i - g*theta_i*e/a_l
        theta = theta_i + omega*e ! Correcao -> utilizar o omega_{i+1} ao inves de omega_i

        E = 0.5*a_m*(omega_i*a_l)**2 + a_m*g*a_l*(1-cos(theta_i)) ! Calculo da Energia do sistema (Potencial + Cinetica)
        
        WRITE(10, '(F0.6,2(" ",F0.6))') e*i, theta, E ! Escrever as variaveis no arquivo de saida
        
        i = i + 1
    END DO

    CLOSE(10)

END PROGRAM tarefa_a
