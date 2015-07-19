C Output from Public domain Ratfor, version 1.01
      subroutine nndist(n,p,class,kmetric,x,x0,cv,y,iter,fullw,scalar,ep
     *silon,which, dist,covw,covmin,means,weight,values,vectors,scramat,
     *scratch)
      implicit double precision (a-h,o-z)
      integer n, p, class, kmetric, which(n), y(n), iter, neps, cv
      double precision x(n,p), weight(n), means(class,p),dist(n),x0(p),c
     *ovw(p,p), values(p),vectors(p,p),scramat(p,p),scratch(1)
      double precision epsilon, wmax ,cvmax
      integer niter,k
      logical fullw,singul,scalar
      double precision covmin
      cvmax=1d16
      if(cv.gt.0)then
      do23002 j=1,p 
      x0(j)=x(cv,j)
23002 continue
23003 continue
      endif
      niter=iter
      do23004 i=1,n
      weight(i)=0d0
      do23006 j=1,p
      weight(i)=weight(i)+(x(i,j)-x0(j))**2
23006 continue
23007 continue
23004 continue
23005 continue
      if(cv.gt.0)then
      weight(cv)=cvmax
      endif
23010 if(niter.gt.0)then
      do23012 i=1,n 
      which(i)=i
23012 continue
23013 continue
      call sortdi(weight,n,which,1,n)
      wmax=weight(kmetric)
      if(wmax .le. 0d0)then
      do23016 i=1,kmetric
      weight(i)=1d0
23016 continue
23017 continue
      else
      do23018 i=1,kmetric
      dd=sqrt(weight(i)/wmax)
      weight(i)=(1d0-dd**3)**3
23018 continue
23019 continue
      endif
      call withmean(n,p,class,x,y,kmetric,weight,which,covw,fullw,scalar
     *,singul, means,scratch,dist)
      if(scalar)then
      fullw=.false.
      endif
      if(.not.fullw)then
      do23024 j=1,p
      if(covw(j,j).lt.covmin)then
      covw(j,j)=covmin
      endif
23024 continue
23025 continue
      do23028 i=1,n
      dist(i)=0d0

      do23030 j=1,p
      dist(i)=dist(i)+ ((x(i,j)-x0(j))**2)/covw(j,j)
23030 continue
23031 continue
      do23032 k=1,class
      scratch(k)=0d0
23032 continue
23033 continue
      do23034 j=1,p
      dd=(x(i,j)-x0(j))/covw(j,j)
      do23036 k=1,class
      scratch(k)=scratch(k)+means(k,j)*dd
23036 continue
23037 continue
23034 continue
23035 continue
      weight(i)=0d0
      do23038 k=1,class
      weight(i)=weight(i)+scratch(k)**2
23038 continue
23039 continue
      weight(i)=weight(i)+epsilon*dist(i)
23028 continue
23029 continue
      else
      imatz=1
      call rs(p,p,covw,values,imatz,vectors,scratch(1),scratch(p+1),ierr
     *)
      do23040 j=1,p
      if(values(j).lt.covmin)then
      values(j)=covmin
      endif
23040 continue
23041 continue
      do23044 i=1,n
      dist(i)=0d0

      do23046 j=1,p
      dutx=0d0
      do23048 k=1,p
      dutx=dutx+vectors(k,j)*(x(i,k)-x0(k))
23048 continue
23049 continue
      dist(i)=dist(i)+dutx*dutx/values(j)
23046 continue
23047 continue
23044 continue
23045 continue
      do23050 j=1,p
      do23052 k=1,p
      dd=0d0
      do23054 jj=1,p
      dd=dd+vectors(j,jj)*vectors(k,jj)/values(jj)
23054 continue
23055 continue
      scramat(j,k)=dd
23052 continue
23053 continue
23050 continue
23051 continue
      do23056 k=1,class
      do23058 j=1,p
      dd=0d0
      do23060 jj=1,p
      dd=dd+means(k,jj)*scramat(jj,j)
23060 continue
23061 continue
      vectors(k,j)=dd
23058 continue
23059 continue
23056 continue
23057 continue
      do23062 i=1,n
      do23064 k=1,class
      scratch(k)=0d0
23064 continue
23065 continue
      do23066 j=1,p
      dd=(x(i,j)-x0(j))
      do23068 k=1,class
      scratch(k)=scratch(k)+vectors(k,j)*dd
23068 continue
23069 continue
23066 continue
23067 continue
      weight(i)=0d0
      do23070 k=1,class
      weight(i)=weight(i)+scratch(k)**2
23070 continue
23071 continue
      weight(i)=weight(i)+epsilon*dist(i)
23062 continue
23063 continue
      endif
      niter=niter-1
      goto 23010
      endif
23011 continue
      return
      end
