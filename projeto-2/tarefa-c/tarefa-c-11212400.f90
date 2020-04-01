PROGRAM bebado3d

       CHARACTER (22) filename
       PARAMETER (Nmax=1000000)
       PARAMETER (pi=4e0*atan(1e0))
       COMPLEX zpos
       DIMENSION zpos(Nmax)
       zpos = 0

       WRITE(*,*) 'Digite o número N de passos (N<=10e6):'
       READ(*,*) N

       if (N.gt.Nmax) then
              WRITE(*,*) 'Valor N > 10e6 !'
              stop
       end if

       WRITE(*,*) 'Digite o número M de andarilhos:'
       READ(*,*) M

       do i=1, N
              x = 0
              y = 0
              do j=1, M
                     arg = 2*pi*rand()
                     x = x + int(cos(arg))
                     y = y + int(sin(arg))
                     zpos(j) = zpos(j) + cmplx(x,y) 
              end do
       

              rmedio = 0
              delta2 = 0
       
              exp = dlog10(dfloat(i))

              if (mod(exp, 1.).eq.0) then
                     WRITE(filename, '("saida-c-10e",I0,"-11212400")') int(exp)
                     OPEN(10, file=filename)
              
                     do k = 1, M
                            r = abs(zpos(k))
                            rmedio = rmedio + r
                            delta2 = delta2 + r**2
                     
                            WRITE(10,'(F0.1,", ",F0.1)') zpos(k)
                     end do
              
                     rmedio = rmedio/M
                     delta2 = delta2/M - rmedio**2
              
                     WRITE(*,'("N: 10e",I0," <r>: ",F0.3," delta**2: ",F0.3)') int(exp), rmedio, delta2

                     CLOSE(10)
              end if
       end do

END PROGRAM bebado3d