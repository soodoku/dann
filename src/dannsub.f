C Output from Public domain Ratfor, version 1.01
      subroutine dannsub(n,p,x,y,class, x0,kmetric,iter,fullw,scalar, ep
     *silon,which,dist,covw,covmin,means,weight,covb,scratch)
      implicit double precision (a-h,o-z)
      integer n, p, class, kmetric, which(n), y(n),iter
      double precision x(n,p), weight(n), means(class,p),dist(n),scratch
     *(n), covw(p,p),x0(p),covb(p,p)
      double precision epsilon, wmax,covmin
      logical fullw,singul,scalar
      scalar=.true.
      fullw=.false.
      icv=0
      do23000 j=1,p
      do23002 k=1,p
      covb(j,k)=0d0
23002 continue
23003 continue
23000 continue
23001 continue
      do23004 i=1,n 
      do23006 j=1,p
      x0(j)=x(i,j)
23006 continue
23007 continue
      call nndist(n,p,class,kmetric,x,x0,icv,y,iter, fullw,scalar,epsilo
     *n,which,dist,covw,covmin,means, weight,scratch,scratch(p+1),scratc
     *h(p*p+p+1), scratch(2*p*p+p+1))
      do23008 j=1,p
      do23010 k=1,p
      tot=0d0
      do23012 jk=1,class
      tot=tot+means(jk,j)*means(jk,k)
23012 continue
23013 continue
      covb(j,k)=covb(j,k)+tot/n
23010 continue
23011 continue
23008 continue
23009 continue
23004 continue
23005 continue
      return
      end
