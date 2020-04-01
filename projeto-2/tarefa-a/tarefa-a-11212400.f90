PROGRAM aleatorio
    WRITE (*,*) "Digite a quantidade de N números aleatórios:"
    READ (*,*) N
    
    M = 1000000
    
    do i = 1, N
        amedia = 0e0
        
        do j = 1, M
            amedia = amedia + rand()**i
        end do
        
        WRITE (*,'(A,I0,A,1F5.3)') "<x^",i,">: ", amedia/M
    end do

END PROGRAM aleatorio
