!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine wts_and_pts(lon1,lat1,wts,pts,nspot1)

      use scanlines
      use scene_las

      real(8)  :: lon1(nspot1,scans),lat1(nspot1,scans)
      real(4)  :: wts(60,4)
      integer  :: pts(60,4,2)

      real(4)   ,allocatable,dimension(:) :: x1,y1,dist
      integer   ,allocatable,dimension(:) :: isort,index
      integer   ,allocatable,dimension(:) :: nc,ns

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

! look for 4 closest point to each las spot in scanline 1

      nt0=1

      do n0=1,spots  
      x0=lon(n0,nt0)
      y0=lat(n0,nt0)

      np=0
      do nt=nt0,nt0+6
      do n1=1,nspot1
      if(nt<1     ) cycle
      if(nt>scans ) cycle
      if(n1>nspot1) cycle
      np=np+1
      enddo
      enddo

      allocate(x1(np),y1(np),nc(np),ns(np))
      allocate(isort(np),index(np),dist(np))

      np=0
      do nt=nt0,nt0+6
      do n1=1,nspot1
      if(nt<1     ) cycle
      if(nt>scans ) cycle
      if(n1>nspot1) cycle
      np=np+1
      x1(np)=lon1(n1,nt)
      y1(np)=lat1(n1,nt)
      nc(np)=nt
      ns(np)=n1
      enddo
      enddo

      ! calculate and sort distance between nearby scan points

      call gcdist(x0,y0,x1,y1,dist,np)
      call orders(2,isort,dist,index,np,1,4,0)

      ! calaulate and normalize  the inverse distance weights for the 4
      ! closest points

      swt=0
      do i=1,4
      j=index(i)
      wts(n0,i)=1./dist(j)
      pts(n0,i,1)=nc(j)-nt0
      pts(n0,i,2)=ns(j)
      swt=swt+wts(n0,i)
      enddo

      wts(n0,:)= wts(n0,:)/swt

      !print*; print*,nt0,n0
      do i=1,4
      j=index(i)
      wt=1./dist(j)
      !write(6,*)nc(j)-nt0,ns(j),dist(j),wt,wt/swt,x1(j),y1(j)
      enddo

      deallocate(isort,index,dist)
      deallocate(x1,y1,nc,ns)

      enddo

      end subroutine wts_and_pts
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine gcdist(x1,y1,x2,y2,dist,np)

      real(4) :: x1,y1,x2(np),y2(np),dist(np)
      real(4) :: havdx(np)
      real(4) :: havdy(np)
      real(4) :: cosys(np)
      real(4) :: pi180 = .0174532
      real(4) :: rade  =  6371.

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

      if(np.eq.0) return

!  compute the haversine great circle distance
!  -------------------------------------------

      do i=1,np
      havdx(i) = sin(.5*(x2(i)-x1)*pi180)**2
      havdy(i) = sin(.5*(y2(i)-y1)*pi180)**2
      cosys(i) = cos(y1*pi180)*cos(y2(i)*pi180)
      dist(i)  = rade*asin(sqrt(havdy(i)+havdx(i)*cosys(i)))
      enddo

      end  subroutine gcdist
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

