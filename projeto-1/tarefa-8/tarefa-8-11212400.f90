PROGRAM dimensoes
    real*16 Vd, VA, VB

    WRITE(*,*) 'Digite o raio R:'
    READ(*,*) R
    WRITE(*,*) 'Digite o número de dimensões d:'
    READ(*,*) id

    OPEN(unit=10, file='saida-8-11212400')
    OPEN(unit=11, file='saida-8a-11212400')
    OPEN(unit=12, file='saida-8b-11212400')

    pi = 4e0*atan(1e0)
    DO i = 2, id
        arg = i/2e0
        dgamma = 1e0

        DO WHILE (arg.gt.0e0)
            IF (arg.ge.1e0) THEN
                dgamma = arg*dgamma
                arg = arg-1e0
            ELSE
                dgamma = arg*sqrt(pi)*dgamma
                arg = 0e0
            END IF
        END DO

        Vd = (pi**(i/2e0) * R**i) / dgamma
        OPEN(unit=10, file='saida-8-11212400')
        
        VA = 1e0/Vd
        WRITE(11, '(I0," ",F0.8)') i, VA

        VB = 1e0/(Vd*10e-4**i)
        WRITE(12, '(I0," ",F0.8)') i, VB

    END DO

    CLOSE(10)
    CLOSE(11)
    CLOSE(12)
END PROGRAM dimensoes
