subroutine knncv(n,p,x,y ,predict ,perror,kn,unif,scratch )
implicit double precision (a-h,o-z)
integer n,  p, kn, y(n),predict(n),perror
double precision x(n,p), scratch(n) 
real unif(n)
perror=0
test=0d0
cvmax=1d16
do i=1,n {
	do ii=1,n{
		tot=0d0
		do j=1,p{tot=tot+(x(i,j)-x(ii,j))**2}
		scratch(ii)=dsqrt(tot)
		}
	scratch(i)=cvmax
# Do this just in case there is some magic with tie-breaking
		call knn(kn,n,1,1,scratch,y,test,predict(i),unif,d)
		if(predict(i)!=y(i)){perror=perror+1}
		}
return
end
