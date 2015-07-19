subroutine withmean(n,p,class,x,y,k,weight,which,
	covw,fullw,scalar,singul,means, sumw,tmean)
implicit double precision (a-h,o-z)
integer n, p, class, k, which(k), y(n)
double precision x(n,p), weight(k), means(class,p), sumw(class),
	tmean(p), covw(p,p)
logical singul,fullw,scalar
#
# compute a weighted between and within cov for the
# k points indexed by which

#compute the weighted means
do ic=1,class{
	do ip=1,p{means(ic,ip)=0d0}
	sumw(ic)=0d0
	}
do i=1,k{
	ii=which(i)
	ic=y(ii)
	sumw(ic)=sumw(ic)+weight(i)
	do ip=1,p{
		means(ic,ip)=means(ic,ip)+weight(i)*x(ii,ip)
		}
	}
tsumw=0d0
nonnul=0
do ip=1,p{tmean(ip)=0d0}
do ic=1,class {
	tsumw=tsumw+sumw(ic)
	if(sumw(ic)>0d0)nonnul=nonnul+1
	do ip=1,p {
		tmean(ip)=tmean(ip)+means(ic,ip)
		}
	}
do ip =1,p {
	tmean(ip)=tmean(ip)/tsumw
	do ic=1,class{
		if(sumw(ic)>0d0)means(ic,ip)=means(ic,ip)/sumw(ic)
		}
	}
singul=.true.

if(nonnul>1)singul=.false.

#Compute the within

if(fullw){
do ip1=1,p{do ip2=1,p{
	covw(ip1,ip2)=0d0
	do i=1,k{
		ii=which(i)
		covw(ip1,ip2) =covw(ip1,ip2) +weight(i)*(x(ii,ip1)-means(y(ii),ip1))*(x(ii,ip2)-means(y(ii),ip2))/tsumw
}}}
}else{
do ip1=1,p{
	ip2=ip1
	covw(ip1,ip2)=0d0
	do i=1,k{
		ii=which(i)
		covw(ip1,ip2) =covw(ip1,ip2) +weight(i)*(x(ii,ip1)-means(y(ii),ip1))*(x(ii,ip2)-means(y(ii),ip2))/tsumw
}}
}
if(scalar){
	ss=0d0
	do j=1,p{ss=ss+covw(j,j)}
	ss=ss/dble(p)
	do j=1,p{covw(j,j)=ss}
	}
#Center and weight the means  
do j=1,p{
	do i=1,class{
		means(i,j)= dsqrt(sumw(i)/tsumw)*(means(i,j)-tmean(j)) 
}}
return
end 
