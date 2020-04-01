PROGRAM bebado3d

       CHARACTER (22) filename
       PARAMETER (Nmax=1000000)
       PARAMETER (pi=4e0*atan(1e0))
       DIMENSION pos(Nmax,2)
       pos = 0

       WRITE(*,*) 'Digite o número N de passos (N<=10e6):'
       READ(*,*) N

       if (N.gt.Nmax) then
              WRITE(*,*) 'Valor N > 10e6 !'
              stop
       end if

       WRITE(*,*) 'Digite o número M de andarilhos:'
       READ(*,*) M

       do i=1, N
              do j=1, M
                     arg = 2*pi*rand()
                     pos(j,:) = pos(j,:) + (/cos(arg),sin(arg)/) 
              end do
       

              r2 = 0
              rmedio = 0
              delta2 = 0
       
              exp = dlog10(dfloat(i))

              if (mod(exp, 1.).eq.0) then
                     WRITE(filename, '("saida-c-10e",I0,"-11212400")') int(exp)
                     OPEN(10, file=filename)
              
                     do k = 1, M
                            WRITE(10,'(I0,", ",I0)') int(pos(k,:))
                     end do
                     CLOSE(10)

                     r2 = (sum(pos(:,1)**2) + sum(pos(:,2)**2))
                     
                     rmedio = sqrt(r2)/M
                     delta2 = r2/M - rmedio**2
              
                     WRITE(*,'("N: 10e",I0," <r>: ",1F0.3," delta**2: ",1F0.3)') int(exp), abs(rmedio), abs(delta2)
              end if
       end do

END PROGRAM bebado3d