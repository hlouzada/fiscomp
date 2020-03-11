PROGRAM ordena
       PARAMETER (nmax = 10000)
       DIMENSION Anum(nmax)

       OPEN(unit=10, file='entrada-3-11212400')
       DO i = 1, nmax
              READ (10,*,end=3) Anum(i)
       END DO
3      CLOSE (10)

       DO i = 1, nmax - 1
              DO j = i + 1, nmax
                     IF (Anum(i).GT.Anum(j)) THEN
                            TEMP = Anum(i)
                            Anum(i) = Anum(j)
                            Anum(j) = TEMP
                     END IF
              END DO
       END DO

       WRITE (*,*) "Entre o valor dos M primeiros números para ordenar"
       READ (*,*) M

       OPEN(unit=11, file='saida-3-11212400')

       WRITE (11,*) "M = ", M
       
       n=0 
       DO i = 1, nmax
              IF (Anum(i).EQ.0) THEN
                     n = n + 1
              END IF
       END DO
       WRITE (*,*) n
       DO i = n+1, M+n
              WRITE (11,'(F0.8)') Anum(i)
       END DO
       CLOSE (11)

END PROGRAM ordena
