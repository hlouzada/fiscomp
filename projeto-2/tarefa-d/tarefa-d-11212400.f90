program entropia

       PARAMETER (M = 400)
       PARAMETER (N = 1000000)
       PARAMETER (pi = 4e0*atan(1e0))
       dimension pos(M,2), Prob(0:5000*5000)
   
       pos = 0
       Prob = 0
   
       do i=1, N
              do j=1, M
                     arg = 2*pi*rand()
                     pos(j,:) = pos(j,:) + (/cos(arg),sin(arg)/) 
              end do
       end do
   
       do i = 1, M
              iglob = abs(pos(i,1)) + abs(pos(i,2))*N
              Prob(iglob) = Prob(iglob) + 1
       end do
   
       Prob = Prob/M
   
       write(*,*) Prob
   
end program entropia