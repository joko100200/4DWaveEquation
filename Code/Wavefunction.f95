program main
implicit none

real*8, dimension(:,:) , allocatable :: phi,k
integer :: error, i, xt,yt,zt,tt,t1,t2,total,toggle,l
integer :: a !integer dummy variable
real*8 :: pxb,mxb,pyb,myb,pzb,mzb, m = 0.0d0
real*8 :: xx,yy,zz, tmp1, tmp2 !These are temp variables
real*8 :: hx,hy,hz,ht,xmin,xmax,ymin,ymax
real*8 :: zmin,zmax,tmin,tmax,x,y,z,t
namelist /inputs/ hx,hy,hz,ht,xmin,xmax,ymin,ymax &
,zmin,zmax,tmin,tmax
namelist /boundry/ pxb,mxb,pyb,myb,pzb,mzb
namelist /checks/ toggle

!!!!Variable Explination!!!!!!
!xyzt is the meshgrid
!error is the file flag
!i is the index
!xt,yt,zt,tt are the total number of points per dimension
!t is the total number of points
!hx,hy,hz,ht are the seperation between the points
!xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax these are all the 
!Total distance the mesh will go
!
!Since we only need to store the previous scalor feild and the current scalor feild
!The array we initialize here will only have dimension 2 phi(t2,2) where the first
!is the current point location in space and the second index represents phi(1) and phi(2)
!phi(2) being the new phi and phi(1) being the old phi

open(10, file='parameters.inp',status='old',action='read',&
iostat= error)
if (error .ne. 0) stop
read(10,inputs)
read(10,boundry)
read(10,checks)
close(10)

!This is such that we can include the final point. If i put 2 final I want 2 to be the final
xmax = xmax+2*hx
ymax = ymax+2*hy
zmax = zmax+2*hz
tmax = tmax+2*ht

xt= int((xmax-xmin)/hx)
yt= int((ymax-ymin)/hy)
zt= 1 !int((zmax-zmin)/hz) !For testing and visualization reasons this is 1
tt= int((tmax-tmin)/ht)

t1 = xt*yt !Total number of 2d points
t2 = t1*zt !Total number of 3d points
total = t2*tt !Total number of 4d points


open(12,file='xyzt.out',status='unknown',action='write',&
iostat=error)
if (error .ne. 0) stop

do i = 1,total
call IndexToXYZT(i,xt,yt,zt,t1,t2,hx,hy,hz,ht,xmin,ymin,zmin,tmin,x,y,z,t)
write(12,*) x,y,z,t
enddo
close(12)


!here we will define the initial scalor feild as a three dimensional guassian
!this is just for a test run
!
!We will then propagate this feild in time by saying that the laplace is zero I.E.
!There is a conservation of feild of sorts


!!!!!!!!!!!!!!!!!!!!!!!!INITAL STATE OF WAVEFUNCTION!!!!!!!!!!!!!!!!!!

if(toggle .eq. 1) then

!here we are just calculating the initial state of our scalor feilds
open(13,file='phi0.out',status='unknown',action='write')
do i = 1, t2
call IndexToXYZT(i,xt,yt,zt,t1,t2,hx,hy,hz,ht,xmin,ymin,zmin,tmin,x,y,z,t)
write(13,*) 2.d0*exp(-(x*x+y*y+z*z)*2.d0)
enddo
close(13)

open(13,file='k0.out',status='unknown',action='write')
open(14,file='xyztk0.out',status='unknown',action='write')
do i = 1, t2
call IndexToXYZT(i,xt,yt,zt,t1,t2,hx,hy,hz,ht,xmin,ymin,zmin,tmin,x,y,z,t)
write(13,*) sin(y)*cos(x)!-2.d0*exp(-((x-0.5)**2+y*y+z*z)*2.d0)
write(14,*) x,y,sin(y)*cos(x)
enddo
close(14)
close(13)







!!!!!!!!!!!!!!!!!!!!!!!!!!!!TIME PROPOGATION!!!!!!!!!!!!!!!!!!!!!!!!!
else
open(101,file='phi.out',status='new')
close(101)
open(101,file='k.out',status='new')
close(101)

allocate(phi(t2,2)) !This is the array that will store the current state of phi
allocate(k(t2,2))   !and it will store the next version of phi as it is being solved


!Reading Initital STATES
open(13,file='phi0.out',status='old',action='read',iostat=error)
if (error .ne. 0) then
write(*,*) "Calculate Ground state by toggleing switch to equal 1"
stop
endif !End of Initial state must exist condition


do i = 1, t2
read(13,*) phi(i,1)
enddo
close(13)
write(*,*) "Read initial state of Wave"


open(13,file='k0.out',status='old',action='read',iostat=error)
if (error .ne. 0) then
write(*,*) "Calculate Ground state by toggleing switch to equal 1"
stop
endif !End of Initial state must exist condition

do i = 1, t2
read(13,*) k(i,1)
enddo
close(13)
write(*,*) "Read initital state of k0"

!!!!!!!!!!!!!!!!!!!!!!!!!!!!PROPOGATION!!!!!!!!!!!!!!!!!!!!!!!!!
!Space and time split

do l = 1, tt !Time integration loop

do i = 1, t2 !phi calculation at all space points loop

!calculation of second order partial derivatives of phi()(1)

!xyzt of location of current point
call IndexToXYZT(i,xt,yt,zt,t1,t2,hx,hy,hz,ht,xmin,ymin,zmin,tmin,x,y,z,t)

!Xpartial derivative
if(x == xmin) then
call XYZTToIndex(a,x+hx,y,z,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
tmp1 = phi(a,1) 
tmp2 = mxb 
else if (x == xmax-hx) then
tmp1 = pxb
call XYZTToIndex(a,x-hx,y,z,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
tmp2 = phi(a,1) 
else
call XYZTToIndex(a,x+hx,y,z,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
tmp1 = phi(a,1)
call XYZTToIndex(a,x-hx,y,z,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
tmp2 = phi(a,1)
endif
xx = tmp1-2*phi(i,1)+tmp2

!Ypartial derivative
if(y == ymin) then
call XYZTToIndex(a,x,y+hy,z,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
tmp1 = phi(a,1) 
tmp2 = myb 
else if (y == ymax-hy) then
tmp1 = pyb
call XYZTToIndex(a,x,y-hy,z,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
tmp2 = phi(a,1) 
else
call XYZTToIndex(a,x,y+hy,z,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
tmp1 = phi(a,1)
call XYZTToIndex(a,x,y-hy,z,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
tmp2 = phi(a,1)
endif
yy = tmp1-2*phi(i,1)+tmp2

!Zpartial derivative
if(z == zmin) then
call XYZTToIndex(a,x,y,z+hz,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
tmp1 = phi(a,1) 
tmp2 = mzb 
else if (z == zmax-hz) then
tmp1 = pzb
call XYZTToIndex(a,x,y,z-hz,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
tmp2 = phi(a,1) 
else
call XYZTToIndex(a,x,y,z+hz,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
tmp1 = phi(a,1)
call XYZTToIndex(a,x,y,z-hz,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
tmp2 = phi(a,1)
endif
zz = tmp1-2*phi(i,1)+tmp2

!End of boundry conditions calculation of partial derivatives

!!!!!!!!!!!!!!TEST!!!!!!!!!!!!!
tmp1 = -xx-yy-zz + m*m*phi(i,1) !+mass term  !Yeah we are using tmp1 as the time derivative of k
tmp2 = -k(i,1)                  !Yeah we are using tmp2 as the time derivative of phi

k(i,2) = tmp1*ht+k(i,1)
phi(i,2) = tmp2*ht+phi(i,1)




!Figure out p(density feild)


enddo

open(101,file='phi.out',position='append',status='old',action='write')
open(102,file='k.out',position='append',status='old',action='write')
do a = 1,t2
write(101,*) phi(a,1)
write(102,*) k(a,1)
enddo

write(101,*) "" !Time separation
write(102,*) "" !Time separation

close(102)
close(101)


phi(:,1) = phi(:,2)
k(:,1) = k(:,2)
enddo

deallocate(k)
deallocate(phi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!HERE YOU WILL READ THE PHI.OUT FILE AND CONVERT IT TO VISUAL :)!!!!!!!!!!!!

open(103,file='xyztphi.out',status='unknown',action='write')
open(101,file='phi.out',status='old',action='read')
do l = 1, tt - 1
do i = 1, t2 !                                                            
        read(101,*,iostat=error) tmp1
        if(mod(i,t2) == 0) then
        !if(error .ne. 0) then !This is the time split for visualization purphases
                !write(*,*) "Line skip"
                write(103,*) ""
                continue
                !stop
        endif
        call IndexToXYZT(i,xt,yt,zt,t1,t2,hx,hy,hz,ht,xmin,ymin,zmin,tmin,x,y,z,t)
        write(103,*) x,y,tmp1
enddo
enddo
close(101)
close(103)





endif

end program main



!!!!!!!!!!!!!!!MESH SUBROUTINES!!!!!!!!!!!
subroutine IndexToXYZT(i,xt,yt,zt,t1,t2,hx,hy,hz,ht,xmin,ymin,zmin,tmin,x,y,z,t)
        real*8, intent(out) :: x,y,z,t
        integer :: a,b,c,d
        integer, intent(in) :: xt,t1,t2,i,zt,yt
        real*8, intent(in) :: hx,hy,hz,ht,xmin,ymin,zmin,tmin
        
        a = MOD(i-1,xt)             !integer numx
        d = int((i-1)/t2)           !integer numt
        c = int((i-1)/t1)-d*zt      !integer numz
        b = int((i-1)/xt)-c*yt-d*t1 !integer numy

        x = a*hx+xmin               !x coord value
        y = b*hy+ymin               !y coord value
        z = c*hz+zmin               !z coord value
        t = d*ht+tmin               !t coord value

        return
end subroutine IndexToXYZT

subroutine XYZTToIndex(i,x,y,z,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
        integer, intent(out) :: i
        integer :: a,b,c,d
        integer, intent(in) :: xt,t1,t2
        real*8, intent(in) :: x,y,z,t,xmin,ymin,zmin,tmin,hx,hy,hz,ht
        
        a = int((x-xmin)/hx)       !integer numx
        b = int((y-ymin)/hy)       !integer numy
        c = int((z-zmin)/hz)       !integer numz
        d = int((t-tmin)/ht)       !integer numt
        
        i = d*t2+c*t1+b*xt+a+1     !index in the mesh

        return
end subroutine XYZTToIndex
