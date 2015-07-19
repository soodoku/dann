subroutine nndist2(n,p,class,kkk,rem,x,x0,cv,y,fullsc,which,
	dist,metric,covw,means,weight,values,vectors,scramat,scratch,xnew,ynew,kback)
implicit double precision (a-h,o-z)
integer n, p, class, kkk(3),  which(n), y(n), ynew(n),iter, neps, cv,kback(2)
double precision x(n,p), xnew(n,p),weight(n), means(class,p),dist(n),x0(p),covw(p,p),
	values(p),vectors(p,p),scramat(p,p),scratch(1),rate,metric(p,p)
# scratch should be at least 2*p +n
#If cv > 0 then it is the index of the point being left out
double precision epsilon, wmax ,cvmax,rem(3)
integer niter,k

logical fullsc(2),fullw,singul,scalar
double precision covmin
#
# I hate to do this but S has a 25 argument limit!!

kmetric=kkk(1)
ktarget=kkk(2)
iter=kkk(3)

fullw=fullsc(1)
scalar=fullsc(2)

rate=rem(1)
epsilon=rem(2)
covmin=rem(3)
#
#
# A covmin threshold on variances, eigenvalues 
# Compute eucliidean distance from x0
cvmax=1d16  #This has to be some very large number 
if(cv>0){do j=1,p {x0(j)=x(cv,j)}}
 # Initialize the which 
#Initially every point can enter
kold=n
knew=kmetric
do i=1,n {which(i)=i}
#initialize the metric
do i=1,p{
	for(j=i;j<=p;j=j+1){
		metric(i,j)=0d0
		}
	metric(i,i)=1d0
	}
do i=1,n{
	weight(i)=0d0
	do j=1,p{
		weight(i)=weight(i)+(x(i,j)-x0(j))**2
		}
	}
#This should get rid of the CV guy once and for all
if(cv>0){weight(cv)=cvmax}

niter=iter
singul=.FALSE.
while((niter>0)&(knew>ktarget)&^singul){

call sortdi(weight,n,which,1,kold)
wmax=weight(knew)
if(wmax <= 0d0){do i=1,knew{weight(i)=1d0}}
	else{
	do i=1,knew{
		dd=sqrt(weight(i)/wmax)
		weight(i)=(1d0-dd**3)**3
		}
	}
do i=1,kold{
	do j=1,p{
		xx=0d0
		for(k=j;k<=p;k=k+1){xx=xx+metric(j,k)*(x(which(i),k)-x0(k))}
		xnew(i,j)=xx
		}
	ynew(i)=y(which(i))
	}
call withmean2(n,p,class,xnew,ynew,knew,weight ,covw,fullw,scalar,singul,
			means,scratch,dist)
#
#call dblepr("means ",6,means,4)
#call dblepr("covw  ",6,covw,4)

#Now compute the two pieces of our distance
if(scalar){fullw=.FALSE.}
if(^fullw) {
	#Condition the variances
	do j=1,p{
		if(covw(j,j)<covmin){covw(j,j)=covmin}
		}
#update the metric
do j=1,p{do jj=1,p{
	tt=0d0
	do k=1,class{tt=tt+means(k,j)*means(k,jj)}
	scramat(j,jj)=tt/(covw(j,j)*covw(jj,jj))
}
	scramat(j,j)=scramat(j,j)+epsilon/covw(j,j)
	}
#call dblepr("scramat",6,scramat,4)
call chol(scramat,p,scratch,0,0,irank)
#call dblepr("scramat",6,scramat,4)
do j=1,p {for(jj=j;jj<=p;jj=jj+1){
	tt=0d0
	for(k=j;k<=jj;k=k+1){tt=scramat(j,k)*metric(k,jj)+tt}
	metric(j,jj)=tt
	}}
	do i=1,kold{
		dist(i)=0d0
	 #Compute the  x^TW^{-1}x
		do j=1,p{
			dist(i)=dist(i)+ (xnew(i,j)**2)/covw(j,j)
		}
	#Compute the x^TW^{-1}M^TMW^{-1}x
		do k=1,class{ scratch(k)=0d0}
		do j=1,p{
			dd=xnew(i,j) /covw(j,j)
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
#update the metric
# This must still be done
	do i=1,kold{
		dist(i)=0d0
	 #Compute the  x^TW^{-1}x
		do j=1,p{ 
			dutx=0d0
			do k=1,p{dutx=dutx+vectors(k,j)*xnew(i,k)}
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
	if(rate<1){
		kold=knew
		knew=kold*rate
	}
kback(1)=iter-niter
kback(2)=kold
}
return
end
