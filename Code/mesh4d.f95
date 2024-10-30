program main
implicit none

real*8, dimension(:,:), allocatable :: W
integer :: error, i, xt,yt,zt,tt,t1,t2,total
real*8 :: hx,hy,hz,ht,xmin,xmax,ymin,ymax
real*8 :: zmin,zmax,tmin,tmax,x,y,z,t

namelist /inputs/ hx,hy,hz,ht,xmin,xmax,ymin,ymax &
,zmin,zmax,tmin,tmax

!!!!Variable Explination!!!!!!
!xyzt is the meshgrid
!error is the file flag
!i is the index
!xt,yt,zt,tt are the total number of points per dimension
!t is the total number of points
!hx,hy,hz,ht are the seperation between the points
!xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax these are all the 
!Total distance the mesh will go
!g is the minkowski metrix

open(10, file='parameters.inp',status='old',action='read',&
iostat= error)
if (error .ne. 0) stop
read(10,inputs)
close(10)

xt = int((xmax-xmin)/hx)      !total points in x
yt = int((ymax-ymin)/hy)      !total points in y
zt = int((zmax-zmin)/hz)      !total points in z
tt = int((tmax-tmin)/ht)      !total points in time

t1 = xt*yt    !Total number of 2d points
t2 = t1*zt    !Total number of 3d points
total = t2*tt !Total number of 4d points

allocate(W(t2,2)) !This is the wavefunction it's a 2d array to store the last time wavefunction
                  !and the next wavefunction in time

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!DEBUG MESH LOOP!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!open(12,file='xyzt.out',status='unknown',action='write',&
!iostat=error)
!if (error .ne. 0) stop
!This loop is just here just to bug test that IndexToXYZT works
!do i = 1,total
!call IndexToXYZT(i,xt,yt,zt,t1,t2,hx,hy,hz,ht,xmin,ymin,zmin,tmin,x,y,z,t)
!write(12,*) x,y,z,t
!enddo
!close(12)
!!!!!!!!!!!!!!!!!!!!!!!!END OF DEBUG MESH LOOP!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




!!!!!!!!!!!!!!!!!!!!!!!!







!W(:,1) = W(:,2) !This makes the old wavefunction the new wavefunction


deallocate(W)
end program main


subroutine IndexToXYZT(i,xt,yt,zt,t1,t2,hx,hy,hz,ht,xmin,ymin,zmin,tmin,x,y,z,t)
        real*8, intent(out) :: x,y,z,t
        integer :: a,b,c,d
        integer, intent(in) :: xt,t1,t2,i,zt,yt
        real*8, intent(in) :: hx,hy,hz,ht,xmin,ymin,zmin,tmin
        
        a = MOD(i-1,xt)                !integer numx
        d = int((i-1)/t2)              !integer numt
        c = int((i-1)/t1)-d*zt         !integer numz
        b = int((i-1)/xt)-c*yt-d*zt*yt !integer numy

        x = a*hx+xmin
        y = b*hy+ymin
        z = c*hz+zmin
        t = d*ht+tmin

        return
end subroutine IndexToXYZT


subroutine XYZTToIndex(i,x,y,z,t,xt,t1,t2,xmin,ymin,zmin,tmin,hx,hy,hz,ht)
        integer, intent(out) :: i
        integer :: a,b,c,d
        integer, intent(in) :: xt,t1,t2
        real*8, intent(in) :: x,y,z,t,xmin,ymin,zmin,tmin,hx,hy,hz,ht
        
        a = int((x-xmin)/hx)
        b = int((y-ymin)/hy)
        c = int((z-zmin)/hz)
        d = int((t-tmin)/ht)

        i = d*t2+c*t1+b*xt+a+1

        return
end subroutine XYZTToIndex












