PROGRAM derivada
    REAL*16 f, df, d2f, d3f, dfrente2p, dtras2p, dsimetrica3p, dsimetrica5p, d2simetrica5p, d3antisimetrica5p
    REAL*16 x, h, ih
    REAL*16 val_df, val_d2f, val_d3f, val_dfrente2p, val_dtras2p
    REAL*16 val_dsimetrica3p, val_dsimetrica5p, val_d2simetrica5p, val_d3antisimetrica5p
    DIMENSION h(14)

    f(x) = sinh(2*x)*sin(x/4)
    df(x) = 2*cosh(2*x)*sin(x/4) + cos(x/4)*sinh(2*x)/4
    d2f(x) = cos(x/4)*cosh(2*x) + 63*sin(x/4)*sinh(2*x)/16
    d3f(x) = 61*cosh(2*x)*sin(x/4)/8 + 191*cos(x/4)*sinh(2*x)/64 

    dfrente2p(x, ih) = (f(x+ih) - f(x)) / ih
    dtras2p(x, ih) = (f(x) - f(x-ih)) / ih
    dsimetrica3p(x, ih) = (f(x+ih) - f(x-ih)) / (2*ih)
    dsimetrica5p(x, ih) = (f(x-2*ih) - 8*f(x-ih) + 8*f(x+ih) - f(x+2*ih)) / (12*ih)
    d2simetrica5p(x, ih) = (-f(x-2*ih) + 16*f(x-ih) - 30*f(x) + 16*f(x+ih) - f(x+2*ih) ) / (12*ih**2)
    d3antisimetrica5p(x, ih) = (-f(x-2*ih) + 2*f(x-ih) - 2*f(x+ih) + f(x+2*ih)) / (2*ih**3)
    
    h = (/0.5, 0.2, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005, 0.00001, 0.000001, 0.0000001, 0.00000001/)
    x = 1d0

    val_df  = df(x)
    val_d2f = d2f(x)
    val_d3f = d3f(x)
    
    OPEN(10, file='saida-a-11212400')
    DO i = 1, 14

        val_dfrente2p = dfrente2p(x, h(i))
        val_dtras2p = dtras2p(x, h(i))
        val_dsimetrica3p = dsimetrica3p(x, h(i))
        val_dsimetrica5p = dsimetrica5p(x, h(i))
        val_d2simetrica5p = d2simetrica5p(x, h(i))
        val_d3antisimetrica5p = d3antisimetrica5p(x, h(i))

        WRITE(*,*) h(i), abs(val_dfrente2p-val_df), abs(val_dtras2p-val_df), abs(val_dsimetrica3p-val_df), &
        abs(val_dsimetrica5p-val_df), abs(val_d2simetrica5p-val_d2f), abs(val_d3antisimetrica5p-val_d3f)
        WRITE(10,*) h(i), abs(val_dfrente2p-val_df), abs(val_dtras2p-val_df), abs(val_dsimetrica3p-val_df), &
        abs(val_dsimetrica5p-val_df), abs(val_d2simetrica5p-val_d2f), abs(val_d3antisimetrica5p-val_d3f)
    END DO

    WRITE(*,*) 'df/dx: ', val_df
    WRITE(*,*) 'd^2f/(dx)^2: ', val_d2f
    WRITE(*,*) 'd^3f/(dx)^3: ', val_d3f

END PROGRAM derivada