PROGRAM tarefa_e
    PARAMETER(pi = 4e0*atan(1e0))

    ! Calculo da posicao usando a subrotina definida
    CALL calculo_posicao(-pi/4, 1)  ! alpha = -pi/4
    CALL calculo_posicao(0e0, 2)    ! alpha = 0
    CALL calculo_posicao(pi/4, 3)   ! alpha = pi/4

CONTAINS

    ! Definicao da subroutina para o calculo da posicao
    SUBROUTINE calculo_posicao(alpha, i)
        CHARACTER(70) filename
        DIMENSION r(2), v(2), a(2), g(2), d(2)

        ! Definicao das variaveis inicias e constantes
        t = 0e0
        r = (/0e0, 1e3/)
        v = 1e1*(/cos(alpha), sin(alpha)/)
        g = (/0e0, 1e1/)
        d = (/3e-1, 3e-1/)
        e = 10e-3

        ! Formatacao para o nome do arquivo de saida
        WRITE(filename,'(A,I0,A)') 'saida-e-alpha',i,'-11212400'

        OPEN(10, file=filename)

        ! Calcular os valores inicias
        a = -g
        v = v + (e/2)*a
        r = r + e*v
        t = t + e

        ! Calculo da velocidade e da posicao em funcao do tempo, enquanto a pos for positiva
        DO WHILE (norm2(d*v).GE.5e-3)
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