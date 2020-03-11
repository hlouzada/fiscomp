PROGRAM complexo
    WRITE(*,*) 'Digite um valor para N:'
    READ(*,*) N

    pi = 4*atan(1e0)
    DO i = 1, N
        arg = 2e0*pi*i/N
        p = 3e0**(1e0/N)
        a = p*cos(arg)+2e0
        b = p*sin(arg)
        WRITE (*,'("Z"I0,":",F8.5," ",SP,F8.5,"i")') i, cmplx(a, b)
    END DO

END PROGRAM complexo