PROGRAM bebado
        PARAMETER (p=0.5)
        PARAMETER (N=1000)
        PARAMETER (nmax=2*N+1)
        DIMENSION ipos(nmax)
        ipos = 0

        OPEN(10,file='saida-b1-11212400')
        
        WRITE(*,*) "Digite o número M de andarilhos:"
        READ(*,*) M

        WRITE(*,*) "Utilizando p: ",p

        amediaq=0
        amedia=0

        do i=1, M
                k=1
                do j=1, N
                        if (p .gt. rand()) then
                                k=k+1
                        else
                                k=k-1
                        end if
                end do
                ipos(k+N) = ipos(k+N)+1

                amedia = amedia + (K+N)
                amediaq = amediaq + (K+N)**2
        end do

        do i=-N, N
                if (ipos(i+N+1).ne.0 ) then
                        WRITE(10,'(I0,", ",I0)') i,ipos(i+N+1)
                end if
                WRITE(*,'(SP,I0,"º posição: ",SS,I0)') i,ipos(i+N+1)
        end do
        
        WRITE(*,'("<x**2>: ",1F0.3," <x>: ",1F0.3)') sqrt(amediaq/M), amedia/M

END PROGRAM bebado
