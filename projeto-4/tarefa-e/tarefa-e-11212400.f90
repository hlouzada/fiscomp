PROGRAM tarefa_e
    PARAMETER(pi = 4e0*atan(1e0))

    ! Calculo da posicao usando a subrotina definida
    CALL calculo_posicao(0e0, -pi/4, 1, 1)  ! alpha = -pi/4
    CALL calculo_posicao(0e0, 0e0, 1, 2)    ! alpha = 0
    CALL calculo_posicao(0e0, pi/4, 1, 3)   ! alpha = pi/4

    CALL calculo_posicao(1e1, -pi/4, 2, 1)  ! alpha = -pi/4
    CALL calculo_posicao(1e1, 0e0, 2, 2)    ! alpha = 0
    CALL calculo_posicao(1e1, pi/4, 2, 3)   ! alpha = pi/4

CONTAINS

    ! Definicao da subroutina para o calculo da posicao
    SUBROUTINE calculo_posicao(vo, alpha, i, j)
        CHARACTER(70) filename
        DIMENSION r(2), v(2), a(2), g(2), d(2)

        ! Definicao das variaveis inicias e constantes
        t = 0e0
        r = (/0e0, 1e2/)
        v = vo*(/cos(alpha), sin(alpha)/)
        g = (/0e0, 1e1/)
        d = (/3e-1, 3e-1/)
        e = 1e-4

        ! Formatacao para o nome do arquivo de saida
        WRITE(filename,'(A,2(I0,A))') 'saida-e-v',i,'-alpha',j,'-11212400'

        OPEN(10, file=filename)

        ! Calcular os valores inicias
        a = -g
        v = v + (e/2)*a
        r = r + e*v
        t = t + e

        ! Calculo da velocidade e da posicao em funcao do tempo, enquanto a pos for positiva
        DO WHILE (norm2(d*v).GE.1e-4)
            WRITE(10, '(F0.6,4(" ",F0.6))') t, r, v ! Escrever as variaveis no arquivo de saida
            a = -g
            v = v + e*a
            r = r + e*v
            t = t + e
            IF (r(2).LT.0) THEN
                r(2) = abs(r(2))
                v = abs(v) - d*abs(v)
            END IF
        END DO

        CLOSE(10)

        RETURN
    END SUBROUTINE calculo_posicao

END PROGRAM tarefa_e
