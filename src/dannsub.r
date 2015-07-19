subroutine dannsub(n,p,x,y,class, x0,kmetric,iter,fullw,scalar,
	epsilon,which,dist,covw,covmin,means,weight,covb,scratch)
implicit double precision (a-h,o-z)
integer n,   p, class, kmetric, which(n), y(n),iter
double precision x(n,p), weight(n), means(class,p),dist(n),scratch(n), 
	covw(p,p),x0(p),covb(p,p)
#scratch should be n+2*p^2+3*p
 double precision epsilon, wmax,covmin

logical fullw,singul,scalar
scalar=.TRUE.
fullw=.FALSE.
icv=0
do j=1,p{do k=1,p{covb(j,k)=0d0}}
do i=1,n {
	do j=1,p{x0(j)=x(i,j)}
	call nndist(n,p,class,kmetric,x,x0,icv,y,iter,
		fullw,scalar,epsilon,which,dist,covw,covmin,means,
		weight,scratch,scratch(p+1),scratch(p*p+p+1),
		scratch(2*p*p+p+1))
	do j=1,p{do k=1,p{
		tot=0d0
		do jk=1,class{tot=tot+means(jk,j)*means(jk,k)}
		covb(j,k)=covb(j,k)+tot/n
		}}
	}
return
end
