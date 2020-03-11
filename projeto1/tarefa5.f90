PROGRAM precicao
    DOUBLE PRECISION dln, dx, derr, dlogx
    
    WRITE(*,*) 'Digite um valor real entre 0 < x < 2, para x:'
    READ (*,*) x

    IF ((0.GE.x) .OR. (x.GE.2e0)) THEN
        WRITE(*,*) 'x está fora do limite permitido para o raio de convergência'
        STOP
    END IF
    
    slogx = LOG(x)
    eprec = 10e-5
    serr = 2*eprec
    sln = 0e0
    n = 0
    DO WHILE ((serr.GE.eprec) .AND. (serr.NE.ABS(slogx - sln)))
        n = n + 1
        sln = sln -(1e0 - x)**n/n
        serr = ABS(slogx - sln)
    END DO
    
    WRITE(*,*) 'Precisão Simples'
    WRITE(*,*) 'log(x):', slogx
    WRITE(*,*) 'aprox:', sln
    WRITE(*,*) 'erro:', erro
    
    dx = x
    dlogx = DLOG(dx)
    derr = 0d0
    dln = 0d0
    n = 0
    DO WHILE (derr.NE.ABS(dlogx - dln))
        n = n + 1
        dln = dln -(1 - dx)**n/n
        derr = ABS(dlogx - dln)
    END DO
    
    WRITE(*,*)
    WRITE(*,*) 'Precisão Dupla'
    WRITE(*,*) 'dlog(x):', dlogx
    WRITE(*,*) 'aprox:', dln
    WRITE(*,*) 'erro:', derr
    
END PROGRAM precicao