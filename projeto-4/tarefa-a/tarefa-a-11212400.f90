PROGRAM tarefa_a

    CALL calculo_posicao(1e-2, 0e0, 1,1)
    CALL calculo_posicao(1e-3, 0e0, 2,1)
    CALL calculo_posicao(1e-4, 0e0, 3,1)

    CALL calculo_posicao(1e-2, 1e1, 1, 2)
    CALL calculo_posicao(1e-3, 1e1, 2, 2)
    CALL calculo_posicao(1e-4, 1e1, 3, 2)

CONTAINS

    SUBROUTINE calculo_posicao(e, v0, i, j)
        CHARACTER(70) filename

        t = 0e0
        r = 100e0
        v = v0
        g = 10e0

        WRITE(filename,'(A,2(I0,A))') 'saida-a-e',i,'-v',j,'-11212400'

        OPEN(10, file=filename)

        DO WHILE (r.GE.0)
            WRITE(10, '(F0.6,2(" ",F0.6))') t, r, v
            a = -g
            v = v + e*a
            r = r + e*v
            t = t + e
        END DO

        CLOSE(10)

        RETURN
    END SUBROUTINE calculo_posicao

END PROGRAM tarefa_a
