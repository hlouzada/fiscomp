PROGRAM tarefa_a

    ! Calculo da posicao usando a subroutina definida
    CALL calculo_posicao(1e-2, 0e0, 1, 1) ! e = 0.01    Vo = 0
    CALL calculo_posicao(1e-3, 0e0, 2, 1) ! e = 0.001   Vo = 0
    CALL calculo_posicao(1e-4, 0e0, 3, 1) ! e = 0.0001  Vo = 0

    CALL calculo_posicao(1e-2, 1e1, 1, 2) ! e = 0.01    Vo = 10
    CALL calculo_posicao(1e-3, 1e1, 2, 2) ! e = 0.001   Vo = 10
    CALL calculo_posicao(1e-4, 1e1, 3, 2) ! e = 0.0001  Vo = 10

CONTAINS

    ! Definicao da subroutina para o calculo da posicao
    SUBROUTINE calculo_posicao(e, v0, i, j)
        CHARACTER(70) filename

        ! Definicao das variaveis inicias e constantes
        t = 0e0
        r = 100e0
        v = v0
        g = 10e0

        ! Formatacao para o nome do arquivo de saida
        WRITE(filename,'(A,2(I0,A))') 'saida-a-e',i,'-vo',j,'-11212400'

        OPEN(10, file=filename)

        ! Calcular os valores inicias
        a = -g
        v = v + (e/2)*a
        r = r + e*v
        t = t + e

        ! Calculo da velocidade e da posicao em funcao do tempo, enquanto a pos for positiva
        DO WHILE (r.GE.0)
            WRITE(10, '(F0.6,2(" ",F0.6))') t, r, v ! Escrever as variaveis no arquivo de saida
            a = -g
            v = v + e*a
            r = r + e*v
            t = t + e
        END DO

        CLOSE(10)

        RETURN
    END SUBROUTINE calculo_posicao

END PROGRAM tarefa_a
