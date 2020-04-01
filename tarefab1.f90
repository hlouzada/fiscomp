PROGRAM bebado
        PARAMETER (N=1001)
        PARAMETER (p=0.5)
        PARAMETER (nmax=2*N+1)
        DIMENSION ipos(nmax)
        ipos = 0

        OPEN(10,file='saida-b1-11212400')
        WRITE(*,*) "Número de andarilhos"
        READ(*,*) M

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
        end do
        
        do i=-N, N
                WRITE(10,'(I0," ",I0)') i,ipos(i+N+1)
                WRITE(*,'(SP,I0,"º posição: ",SS,I0)') i,ipos(i+N+1)
        end do


END PROGRAM bebado
