PROGRAM tarefa_c
    IMPLICIT REAL(8) (a-h,o-z)
    PARAMETER (pi = 4d0*atan(1d0))
    CHARACTER(70) filename

    ! Definicao das constantes
    e = 3d-2 ! delta_t
    theta_0 = pi/15

    i1 = geracao_dado_angular(e,theta_0, 5d-1, 1, 1)          ! Utilizando theta_0(1) = pi/15     e     F_0(1) = 0.5
    i2 = geracao_dado_angular(e,theta_0 + 1d-3, 5d-1, 2, 1)   ! Utilizando theta_0(2) = pi/15 + 0.001 e F_0(1) = 0.5
    i3 = geracao_dado_angular(e,theta_0, 1.2d0, 1, 2)         ! Utilizando theta_0(1) = pi/15     e     F_0(2) = 1.2
    i4 = geracao_dado_angular(e,theta_0 + 1d-3, 1.2d0, 2, 2)  ! Utilizando theta_0(2) = pi/15 + 0.001 e F_0(2) = 1.2

    ! Se por algum motivo se os numeros forem diferentes (acho so se tmax diferente nas geracao_dado_angular), mas acho que e uma verificacao desnecessaria
    IF(i1.NE.i2 .AND. i1.NE.i3 .AND. i1.NE.i4 .AND. i2.NE.i3 .AND. i2.NE.i4 .AND. i3.NE.i4) THEN
        CALL EXIT(1)
    END IF

    N_dados = i1
    DO i=1, 2
        ! Abrir o arquivo de entrada da velocidade(1) e posicao(1) angular
        WRITE(filename,'(A,2(I0,A))') 'saida-c-dados-theta0-',1,'-F0-',i,'-11212400'
        OPEN(10, file=filename)

        ! Abrir o arquivo de entrada da velocidade(2) e posicao(2) angular
        WRITE(filename,'(A,2(I0,A))') 'saida-c-dados-theta0-',2,'-F0-',i,'-11212400'
        OPEN(11, file=filename)

        ! Abrir o arquivo de saida da variacao da posicao angular em funcao do tempo
        WRITE(filename,'(A,1(I0,A))') 'saida-c-F0-',i,'-11212400'
        OPEN(12, file=filename)

        ! Abrir o arquivo de saida da variacao da posicao angular em funcao do tempo em escala mono-log
        WRITE(filename,'(A,1(I0,A))') 'saida-c-monoLog-F0-',i,'-11212400'
        OPEN(13, file=filename)

        ! Abrir o arquivo de saida da variacao da posicao angular em funcao do tempo em escala mono-log
        WRITE(filename,'(A,1(I0,A))') 'saida-c-dados-monoLog-F0-',i,'-11212400'
        OPEN(14, file=filename)

        x_med = 0
        k = 0
        ! Calcula a diferenca entre os angulos
        DO j=1, N_dados

            READ(10,*) theta_1
            READ(11,*) theta_2
               
            D_theta = (abs(theta_1-theta_2)) ! Calculando delta_theta

            ! Menor valor para D_theta >= (Precisao dupla) > -Inf
            IF(D_theta.ge.10d-45) THEN

                ! Selecionando apenas os dados com uma melhor visualisacao no grafico
                IF(D_theta.ge.1d-6) THEN
                    WRITE(12,'(F0.6,1(" ",F0.6))') j*e, D_theta ! Escreve as variaveis no arquivos de saida, grafico delta_theta X t
                    WRITE(13,'(F0.6,1(" ",F0.6))') j*e, log(D_theta) ! Escreve as variaveis no arquivos de saida, grafico ln(delta_theta) X t
                END IF

                WRITE(14,*) j*e, log(D_theta) ! Escreve os dados excluindo os valores "-Inf" para exp_lyapunov

                x_med = x_med + j*e
                k = k + 1 ! Contador de dados escritos para exp_lyapunov
            END IF

        END DO

        x_med = x_med/k ! Calculo da media dos valores de delta_theta para exp_lyapunov

        ClOSE(10)
        CLOSE(11)
        CLOSE(12)
        CLOSE(13)
        CLOSE(14) ! Importante fechar antes de chamar a funcao, pq ele abre o arquivo novamente

        ! Retorna o valor da expoente de Lyapunov a partir do fitting (realizado pela funcao exp_lyapunov)
        WRITE(*,'(A,I0,A,F0.6)') 'Expoente de Lyapunov para F_0(',i,'): ', exp_lyapunov(filename,k,x_med) 
    END DO

CONTAINS

    ! Funcao para gerar a velocidade e a fumcao angular dado theta_0 e F_0, e retorna a quantidade de valores gerados
    INTEGER FUNCTION geracao_dado_angular(e,theta_0,F_0,j,k)
        IMPLICIT REAL(8) (a-h,o-z)
        PARAMETER (pi = 4d0*atan(1d0))
        CHARACTER(70) filename
    
        ! Formatacao para o nome do arquivo de saida da velocidade(j) e posicao(j) angular
        WRITE(filename,'(A,2(I0,A))') 'saida-c-dados-theta0-',j,'-F0-',k,'-11212400'
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

            WRITE(10,*) theta ! Escrever as variaveis no arquivo de saida

            i = i + 1
        END DO

        CLOSE(10)

        geracao_dado_angular = i
        
    END FUNCTION geracao_dado_angular

    ! fitting para calcular o expoente de lyapunov utilizando o metodo dos minimos quadrados
    REAL(8) FUNCTION exp_lyapunov(filename, N, x_med)
        IMPLICIT REAL(8) (a-h,o-z)
        CHARACTER(70) filename

        x_soma1 = 0
        x_soma2 = 0

        ! Abrir o arquivo com os valores para realizar o fitting (t, ln(delta_theta))
        OPEN(unit=10, file=filename)

        DO k=1, N
            READ(10,*) x,y
            
            x_soma1 = (x - x_med)*y + x_soma1
            x_soma2 = (x - x_med)**2 + x_soma2
        END DO
        
        CLOSE(10)

        exp_lyapunov = x_soma1/x_soma2
    
    END FUNCTION exp_lyapunov

END PROGRAM tarefa_c