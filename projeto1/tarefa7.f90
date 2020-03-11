PROGRAM dimensao
    
    WRITE(*,*) 'Digite o número de dimensões d:'
    READ(*,*) nd
    WRITE(*,*) ''
    WRITE(*,*) 'Digite o número de iterações M:'
    READ(*,*) M
    
    n = 0
    DO i = 1, M
        rmod = 0
        DO j = 1, nd
            rmod = rmod + rand()**2
        END DO
        IF (rmod.le.1e0) THEN
            n = n + 1
        END IF
    END DO
    
    Vmc = (float(n)/float(M))*2e0**nd
    
    pi = 4e0*atan(1e0)
    gamma = 1e0
    nd = (nd/2e0 + 1e0)
    nd = nd - 1e0
    DO while (d.gt.0e0)
        IF (nd.ge.1e0) THEN
            gamma = nd*gamma
            nd = nd - 1e0
        else
            gamma = nd*sqrt(pi)*gamma
            nd = 0e0
        END IF
    END DO
    
    Vd = pi**(nd/2e0) / gamma
    
    WRITE(*,'("Volume por Monte Carlo: "F0.6)') Vmc
    WRITE(*,'("Volume por n-esferas: "F0.6)') Vd
END PROGRAM dimensao