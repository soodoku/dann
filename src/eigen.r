subroutine eigen(n,n,a,w,z,scr1,scr2,itag,fv1,fv2,ierr)
 implicit double precision (a-h,o-z)
integer n,err,matz, itag(n)
double precision a(n,n),w(n),z(n,n),fv1(n),fv2(n),
scr1(n),scr2(n,n)

matz=1

call rs(n,n,a,w,matz,scr2,fv1,fv2,ierr)

do i=1,n{ itag(i)=i;scr1(i)=-1.0*w(i)}



call sortdi(scr1,n,itag,1,n)


do i=1,n{scr1(i)=w(i)}

do i=1,n{ w(i)=scr1(itag(i))}
do j=1,n{ do i=1,n{ z(i,j)=scr2(i,itag(j))}}


return
end
