program animate
character(70) filename
character(70) newfilename

call get_command_argument(1,filename)

open(10,file=filename)

write(newfilename,'(A,A)') 'animated-', trim(filename)

write(*,*) newfilename

open(11, file=newfilename)

write(11,*)'@focus off'
write(11,*)'@g0 on'
write(11,*)'@with g0'
nuframe = 200000
do  10 iframe =1, nuframe
write(11,*)'@world xmin -1'
write(11,*)'@world xmax 1'
write(11,*)'@world ymin -1'
write(11,*)'@world xmax 1'
!
!    read the datas to animate
!
  ndata = 1  !number of points in each frame
  do i1=1,ndata
    read(10,*,end=3) x1,y1
    write(11,*)x1,y1
  end do
  





      write(11,6)"@S_ line type 0    "
6         format(a18)
      write(11,6)"@S_ symbol 1       "
      write(11,6)"@S_ symbol color 2 "
      write(11,6)"@S_ symbol size 1"
      write(11,6)"&                  "
      write(11,6)"@redraw            "
      write(11,6)"@sleep 0.00000002        "
!          write(11,6)"@kill S_           "
10  continue
3   continue
stop 
end program animate