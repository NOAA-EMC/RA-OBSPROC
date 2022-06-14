!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      module scene_las 

      integer     ,allocatable,dimension(:)     :: scene_across_track
      integer     ,allocatable,dimension(:)     :: scene_channel
      real(8)     ,allocatable,dimension(:,:)   :: lat
      real(8)     ,allocatable,dimension(:,:)   :: lon
      real(4)     ,allocatable,dimension(:,:)   :: laz
      real(4)     ,allocatable,dimension(:,:)   :: eia
      integer     ,allocatable,dimension(:,:)   :: qc_fov
      real(4)     ,allocatable,dimension(:,:,:) :: tb

      end module scene_las
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine read_las(ncid)

      use scene_las
      use scanlines

      character(20)  :: var
      integer        :: varid

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

! read the groupid and the dimensions from the group

      call getdim(ncid,"scene_across_track",nspot)
      call getdim(ncid,"scene_channel",nchan)
      print*,'reading las',scans,nspot,nchan

! allocate the variables to be read      

      allocate(scene_across_track(nspot))
      allocate(scene_channel(nchan))
      allocate(lat(nspot,scans))
      allocate(lon(nspot,scans))
      allocate(laz(nspot,scans))
      allocate(eia(nspot,scans))
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

! copy btemps to scanlines

      tmbr(:, 1,:) = tb(:,1,:)
      tmbr(:, 2,:) = tb(:,2,:)
      tmbr(:, 3,:) = tb(:,3,:)
      tmbr(:, 4,:) = tb(:,4,:)
      tmbr(:, 5,:) = tb(:,5,:)
      tmbr(:, 6,:) = tb(:,6,:)
      tmbr(:, 7,:) = tb(:,7,:)
      tmbr(:,24,:) = tb(:,8 ,:)
      
      end subroutine read_las
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------


