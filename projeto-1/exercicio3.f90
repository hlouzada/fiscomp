PROGRAM ordena

PARAMETER (nmax = 10000)

DIMENSION A(nmax)

OPEN(unit=1, file='entrada',action='read')

READ (1,*) A

CLOSE (1)

DO i = 1, nmax - 1
        DO j = i + 1, nmax
                IF (A(i) > A(j)) THEN
                        TEMP = A(i)
                        A(i) = A(j)
                        A(j) = TEMP
                END IF
        END DO
END DO

WRITE (*,*) "Entre o valor dos M primeiros números para ordenar"
READ (*,*) M

OPEN(unit=3, file='saida')

WRITE (3,*) "M = ", M
WRITE (3,*) (A(i), i=1, M)

CLOSE (3)

END PROGRAM ordena
