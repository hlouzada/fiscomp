PROGRAM Primos

   WRITE(*,*) 'Digite um valor para os N primeiros primos:'
   READ(*,*) N
   
   j = 1
   
   i = 2
   OPEN(unit=10,file='saida')
   WRITE(10,'(I0"º"," primo: ",I0)') j, i

   IF (N.GE.2) THEN
      DO i = 3, N, 2
   
         k = 3                       
         DO
            IF (k*k .GT. i .OR. MOD(i,k) .EQ. 0)  EXIT
            k = k + 2
         END DO
      
         IF (k*k .GT. i) THEN
            j = j + 1
            WRITE(10,'(I0"º"," primo: ",I0)') j, i
         END IF
      END DO
   END if
   WRITE(10,*)
   WRITE(10,'("Existem "I0," primos até ",I0)') j, N
   CLOSE(10)
END PROGRAM Primos