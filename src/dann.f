C Output from Public domain Ratfor, version 1.01
      subroutine dann(n,p,x,y,class,x0,cv,ntest,predict,kmetric,kn,iter,
     *fullw,scalar, epsilon,neps,which,dist,covw,covmin,means,weight,uni
     *f,scratch)
      implicit double precision (a-h,o-z)
      real unif(ntest)
      integer n, ntest, p, class, kmetric,kn, which(n), y(n),iter, neps,
     *predict(ntest,neps)
      double precision x(n,p), weight(n), means(class,p),dist(n),scratch
     *(n),x0(p,ntest),covw(p,p)
      double precision epsilon(1), wmax,covmin, cvmax
      logical fullw,singul,cv,scalar
      test=0d0
      cvmax=1d16
      do23000 i=1,ntest
      if(cv)then
      itest=1
      icv=i 
      else
      itest=i
      icv=0 
      endif
      call nndist(n,p,class,kmetric,x,x0(1,itest),icv,y,iter, fullw,scal
     *ar,epsilon(1),which,dist,covw,covmin,means, weight,scratch,scratch
     *(p+1),scratch(p*p+p+1), scratch(2*p*p+p+1))
      do23004 ieps=1,neps
      if(ieps.gt.1)then
      do23008 ii=1,n 
      scratch(ii)=dsqrt(weight(ii)+ (epsilon(ieps)-epsilon(1))*dist(ii))
23008 continue
23009 continue
      else
      do23010 ii=1,n 
      scratch(ii)=dsqrt(weight(ii)) 
23010 continue
23011 continue
      endif
      if(cv)then
      scratch(icv)=cvmax
      endif
      call knn(kn,n,1,1,scratch,y,test,predict(i,ieps),unif(i),d)
23004 continue
23005 continue
23000 continue
23001 continue
      return
      end
