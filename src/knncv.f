C Output from Public domain Ratfor, version 1.01
      subroutine knncv(n,p,x,y ,predict ,perror,kn,unif,scratch )
      implicit double precision (a-h,o-z)
      integer n, p, kn, y(n),predict(n),perror
      double precision x(n,p), scratch(n)
      real unif(n)
      perror=0
      test=0d0
      cvmax=1d16
      do23000 i=1,n 
      do23002 ii=1,n
      tot=0d0
      do23004 j=1,p
      tot=tot+(x(i,j)-x(ii,j))**2
23004 continue
23005 continue
      scratch(ii)=dsqrt(tot)
23002 continue
23003 continue
      scratch(i)=cvmax
      call knn(kn,n,1,1,scratch,y,test,predict(i),unif,d)
      if(predict(i).ne.y(i))then
      perror=perror+1
      endif
23000 continue
23001 continue
      return
      end
