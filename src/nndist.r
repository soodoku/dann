subroutine nndist(n,p,class,kmetric,x,x0,cv,y,iter,fullw,scalar,epsilon,which,
	dist,covw,covmin,means,weight,values,vectors,scramat,scratch)
implicit double precision (a-h,o-z)
integer n, p, class, kmetric, which(n), y(n), iter, neps, cv
double precision x(n,p), weight(n), means(class,p),dist(n),x0(p),covw(p,p),
	values(p),vectors(p,p),scramat(p,p),scratch(1)
# scratch should be at least 2*p +n
#If cv > 0 then it is the index of the point being left out
double precision epsilon, wmax ,cvmax
integer niter,k

logical fullw,singul,scalar
double precision covmin
# A covmin threshold on variances, eigenvalues 
# Compute eucliidean distance from x0
cvmax=1d16  #This has to be some very large number 
if(cv>0){do j=1,p {x0(j)=x(cv,j)}}
 
niter=iter
do i=1,n{
	weight(i)=0d0
	do j=1,p{
		weight(i)=weight(i)+(x(i,j)-x0(j))**2
		}
	}
if(cv>0){weight(cv)=cvmax}
while(niter>0){
do i=1,n {which(i)=i}
call sortdi(weight,n,which,1,n)
wmax=weight(kmetric)
if(wmax <= 0d0){do i=1,kmetric{weight(i)=1d0}}
	else{
	do i=1,kmetric{
		dd=sqrt(weight(i)/wmax)
		weight(i)=(1d0-dd**3)**3
		}
	}
call withmean(n,p,class,x,y,kmetric,weight,which,covw,fullw,scalar,singul,
			means,scratch,dist)
#
#Now compute the two pieces of our distance
if(scalar){fullw=.FALSE.}
if(^fullw) {
	#Condition the variances
	do j=1,p{
		if(covw(j,j)<covmin){covw(j,j)=covmin}
		}
	do i=1,n{
		dist(i)=0d0
	 #Compute the  x^TW^{-1}x
		do j=1,p{
			dist(i)=dist(i)+ ((x(i,j)-x0(j))**2)/covw(j,j)
		}
	#Compute the x^TW^{-1}M^TMW^{-1}x
		do k=1,class{ scratch(k)=0d0}
		do j=1,p{
			dd=(x(i,j)-x0(j))/covw(j,j)
			do k=1,class{ scratch(k)=scratch(k)+means(k,j)*dd}
			}
		weight(i)=0d0
		do k=1,class{weight(i)=weight(i)+scratch(k)**2}
	weight(i)=weight(i)+epsilon*dist(i)
		}

	}
else{
	imatz=1
	call rs(p,p,covw,values,imatz,vectors,scratch(1),scratch(p+1),ierr)
	#Condition the variances
	do j=1,p{
		if(values(j)<covmin){values(j)=covmin}
		}
	do i=1,n{
		dist(i)=0d0
	 #Compute the  x^TW^{-1}x
		do j=1,p{ 
			dutx=0d0
			do k=1,p{dutx=dutx+vectors(k,j)*(x(i,k)-x0(k))}
			dist(i)=dist(i)+dutx*dutx/values(j)
			}
		}
	#Compute the x^TW^{-1}M^TMW^{-1}x
	#First compute MW^{-1}=MVD^{-1}V^T
	do j=1,p{do k=1,p{
		dd=0d0
		do jj=1,p{dd=dd+vectors(j,jj)*vectors(k,jj)/values(jj)}
		scramat(j,k)=dd
		}}
	do k=1,class{do j=1,p{
		dd=0d0
		do jj=1,p{dd=dd+means(k,jj)*scramat(jj,j)}
		vectors(k,j)=dd
		}}
	do i=1,n{
		do k=1,class{ scratch(k)=0d0}
		do j=1,p{
			dd=(x(i,j)-x0(j))
			do k=1,class{ scratch(k)=scratch(k)+vectors(k,j)*dd}
			}
		weight(i)=0d0
		do k=1,class{weight(i)=weight(i)+scratch(k)**2}
		weight(i)=weight(i)+epsilon*dist(i)
		}

	}
	niter=niter-1
}
return
end
