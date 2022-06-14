!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      program scene_las   

      character(255) gfname
      character(20)  var,varname,attname,attout
      character(80)  filename
      character(10)  adate,scene
      character(3)   satid
      integer        varid

      integer     ,allocatable,dimension(:)     :: scene_across_track
      integer     ,allocatable,dimension(:)     :: scene_channel
      real(8)     ,allocatable,dimension(:,:)   :: lat
      real(8)     ,allocatable,dimension(:,:)   :: lon
      real(4)     ,allocatable,dimension(:,:)   :: laz
      real(4)     ,allocatable,dimension(:,:)   :: eia
      character(1),allocatable,dimension(:,:)   :: sft
      integer     ,allocatable,dimension(:,:)   :: qc_fov
      real(4)     ,allocatable,dimension(:,:,:) :: tb

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

      read(5,'(a)') gfname
      read(5,'(a)') scene; print*,scene
      open(6,recl=160)

! open the ssmi netcdf file and read the number of scans to read

      call check(nf_open(gfname,0,ncid) )
      call getdim(ncid,'time',ntime)

! read the groupid and the dimensions freom the group

      call check( nf_inq_ncid(ncid,scene,ncid) )
      call getdim(ncid,"scene_across_track",nspot)
      call getdim(ncid,"scene_channel",nchan)

      print*,ntime,nspot,nchan

! allocate the variables to be read      

      allocate(scene_across_track(nspot))
      allocate(scene_channel(nchan))
      allocate(lat(nspot,ntime))
      allocate(lon(nspot,ntime))
      allocate(laz(nspot,ntime))
      allocate(eia(nspot,ntime))
      allocate(sft(nspot,ntime))
      allocate(qc_fov(nspot,ntime))
      allocate(tb(nspot,nchan,ntime))

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

! print a sample of the data from this group 

      write(6,*) scene_across_track(:)," scene_across_track"
      print*
      write(6,*) scene_channel(:)     ," scene_channel"
      print*


      do n=1,ntime
      if(abs(lat(1,n))>10.) cycle
      do m=1,nspot
      if(abs(tb(m,1,n))>500) cycle
      write(50,*) (m),lat(m,n),lon(m,n),tb(m,1,n)
      enddo
      enddo

      !write(6,'(20f8.2)') lat(1,:)   
      !write(6,*) lon(1,:)             ," lon"
      !print*,laz(1,1)             ," laz"
      !print*,eia(1,1)             ," eia"
      !print*,sft(1,1)             ," sft"
      !print*,qc_fov(1,1)          ," qc_fov"
      !print*,tb(1,1,1)            ," tb"

      stop
      end
