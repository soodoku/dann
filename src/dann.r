subroutine dann(n,p,x,y,class,x0,cv,ntest,predict,kmetric,kn,iter,fullw,scalar,
	epsilon,neps,which,dist,covw,covmin,means,weight,unif,scratch)
implicit double precision (a-h,o-z)
real unif(ntest)
#ntest uniform random numbers
integer n, ntest, p, class, kmetric,kn, which(n), y(n),iter,
	neps,predict(ntest,neps)
double precision x(n,p), weight(n), means(class,p),dist(n),scratch(n),x0(p,ntest),covw(p,p)
#scratch should be n+2*p^2+3*p
# if cv=T, then ntest must be n and x0 must be at least a p vector
double precision epsilon(1), wmax,covmin, cvmax

logical fullw,singul,cv,scalar
test=0d0
cvmax=1d16
do i=1,ntest{
	if(cv){ itest=1; icv=i } else{ itest=i;icv=0 }
# itest=1 simply gives it some space to work with
# 	
	call nndist(n,p,class,kmetric,x,x0(1,itest),icv,y,iter,
		fullw,scalar,epsilon(1),which,dist,covw,covmin,means,
		weight,scratch,scratch(p+1),scratch(p*p+p+1),
		scratch(2*p*p+p+1))
	do ieps=1,neps{
		if(ieps>1){
			do ii=1,n {
				scratch(ii)=dsqrt(weight(ii)+
				(epsilon(ieps)-epsilon(1))*dist(ii))}
			}
		else {
			do ii=1,n {scratch(ii)=dsqrt(weight(ii))  }
		}
# Do this just in case there is some magic with tie-breaking
		if(cv)scratch(icv)=cvmax
		call knn(kn,n,1,1,scratch,y,test,predict(i,ieps),unif(i),d)
		}
	
	}
return
end
