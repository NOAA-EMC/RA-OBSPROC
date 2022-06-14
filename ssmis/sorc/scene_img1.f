!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      module scene_img1 

      integer     ,allocatable,dimension(:)     :: scene_across_track
      integer     ,allocatable,dimension(:)     :: scene_channel
      real(8)     ,allocatable,dimension(:,:)   :: lat
      real(8)     ,allocatable,dimension(:,:)   :: lon
      real(4)     ,allocatable,dimension(:,:)   :: laz
      real(4)     ,allocatable,dimension(:,:)   :: eia
      character(1),allocatable,dimension(:,:)   :: sft
      integer     ,allocatable,dimension(:,:)   :: qc_fov
      real(4)     ,allocatable,dimension(:,:,:) :: tb

      end module scene_img1
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine read_img1(ncid)

      use scene_img1
      use scanlines

      character(20)  :: var
      integer        :: varid
      integer        :: pts(60,4,2)
      real(4)        :: wts(60,4)
      real(4)        :: tbr(spots,chans,scans)

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

! read the groupid and the dimensions freom the group

      call getdim(ncid,"scene_across_track",nspot)
      call getdim(ncid,"scene_channel",nchan)
      print*,'reading img1',scans,nspot,nchan

! allocate the variables to be read      

      allocate(scene_across_track(nspot))
      allocate(scene_channel(nchan))
      allocate(lat(nspot,scans))
      allocate(lon(nspot,scans))
      allocate(laz(nspot,scans))
      allocate(eia(nspot,scans))
      allocate(sft(nspot,scans))
      allocate(qc_fov(nspot,scans))
      allocate(tb(nspot,nchan,scans))

! read the group variables 

      var="scene_across_track"
      call check( nf_inq_varid(ncid,var,varid) )
      call check( nf_get_var(ncid,varid,scene_across_track) )

      var="scene_channel"
      call check( nf_inq_varid(ncid,var,varid) )
      call check( nf_get_var(ncid,varid,scene_channel) )

      var="lat"
      call check( nf_inq_varid(ncid,var,varid) )
      call check( nf_get_var(ncid,varid,lat) )

      var="lon"
      call check( nf_inq_varid(ncid,var,varid) )
      call check( nf_get_var(ncid,varid,lon) )

      var="laz"
      call check( nf_inq_varid(ncid,var,varid) )
      call check( nf_get_var(ncid,varid,laz) )

      var="eia"
      call check( nf_inq_varid(ncid,var,varid) )
      call check( nf_get_var(ncid,varid,eia) )

      !var="sft"
      !call check( nf_inq_varid(ncid,var,varid) )
      !call check( nf_get_var(ncid,varid,sft) )

      var="qc_fov"
      call check( nf_inq_varid(ncid,var,varid) )
      call check( nf_get_var(ncid,varid,qc_fov) )

      var="tb"
      call check( nf_inq_varid(ncid,var,varid) )
      call check( nf_get_var(ncid,varid,tb) )

! apply scan, channel, and fov qc

      do k=1,scans
      if(qc_scan(k)/=0) tb(:,:,k) = fill

      do j=1,nchan
      jj=scene_channel(j)+1
      if(qc_chan(jj,k)/=0) tb(:,j,k) = fill
      enddo

      do i=1,nspot
      if(qc_fov(i,k)/=0) tb(i,:,k) = fill
      enddo

      enddo

! interpolate the spots to the las locations

      call wts_and_pts(lon,lat,wts,pts,nspot)

      do k=1,scans
      do j=1,nchan
      do i=1,spots
      p1=tb(pts(i,1,2),j,k+pts(i,1,1))*wts(i,1)
      p2=tb(pts(i,2,2),j,k+pts(i,2,1))*wts(i,2)
      p3=tb(pts(i,3,2),j,k+pts(i,3,1))*wts(i,3)
      p4=tb(pts(i,4,2),j,k+pts(i,4,1))*wts(i,4)
      if(min(p1,p2,p3,p4)>0) then 
         tbr(i,j,k) = p1+p2+p3+p4
      else
         tbr(i,j,k) = fill  
      endif
      enddo;enddo;enddo

! copy btemps to scanlines

      tmbr(:, 8,:) = tbr(:,1,:)
      tmbr(:, 9,:) = tbr(:,2,:)
      tmbr(:,10,:) = tbr(:,3,:)
      tmbr(:,11,:) = tbr(:,4,:)
      
      end subroutine read_img1
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------


