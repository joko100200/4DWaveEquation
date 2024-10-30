program test
integer :: x


open(101,file='test.txt',status='old',action='read')
do i = 1 , 4
read(101,*) x
write(*,*) x
enddo
close(101)


end program test
