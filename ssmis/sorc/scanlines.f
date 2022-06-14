!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
      module scanlines

      ! scanline data
      integer      ,allocatable,dimension(:)   :: time
      integer      ,allocatable,dimension(:)   :: tfrac
      real(8)      ,allocatable,dimension(:)   :: orbn
      integer(1)   ,allocatable,dimension(:)   :: qc_scan
      integer(1)   ,allocatable,dimension(:)   :: pflag
      integer(1)   ,allocatable,dimension(:,:) :: qc_chan

      ! consolidated channel data
      real(4)      ,allocatable,dimension(:,:) :: tmbr(:,:,:)
      integer(2)   ,allocatable,dimension(:,:) :: wtca(:,:)
      integer(2)   ,allocatable,dimension(:,:) :: ctca(:,:)  
      integer      ,allocatable,dimension(:,:) :: sfg (:,:)  
      integer      ,allocatable,dimension(:,:) :: rfg (:,:)  

      integer :: scans,chans,spots=60    
      real(4) :: fill=-9e33

      end module scanlines
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
      subroutine read_scanlines(ncid)

      use scanlines

      character(20) :: var
      integer       :: varid

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

      call getdim(ncid,'time'   ,scans)
      call getdim(ncid,'channel',chans)

      allocate(time   (scans))
      allocate(tfrac  (scans))
      allocate(orbn   (scans))
      allocate(qc_scan(scans))
      allocate(pflag  (scans))
      allocate(qc_chan(chans,scans))
      allocate(sfg    (spots,scans))
      allocate(rfg    (spots,scans))

      var="time"
      call check( nf_inq_varid(ncid,var,varid) )
      call check( nf_get_var(ncid,varid,time)  )

      var="tfrac"
      call check( nf_inq_varid(ncid,var,varid) )
      call check( nf_get_var(ncid,varid,tfrac) )

      var="rev"
      call check( nf_inq_varid(ncid,var,varid) )
      call check( nf_get_var(ncid,varid,orbn)  )

      var="qc_scan"
      call check( nf_inq_varid(ncid,var,varid)   )
      call check( nf_get_var(ncid,varid,qc_scan) )

      var="pflag"
      call check( nf_inq_varid(ncid,var,varid) )
      call check( nf_get_var(ncid,varid,pflag) )

      var="qc_channel"
      call check( nf_inq_varid(ncid,var,varid)   )    
      call check( nf_get_var(ncid,varid,qc_chan) )

      !print*,time(1)        ," time"
      !print*,tfrac(1)       ," tfrac"
      !print*,orbn(1)        ," orbn"
      !print*,qc_scan(1)     ," qc_scan"
      !print*,pflag(1)       ," pflag"
      !print*,qc_chan(1,1)   ," qc_channel"
      !do n=1,scans
      !if(qc_scan(n)/=0) print*,n,qc_scan(n)
      !enddo
      !stop
 
      end subroutine read_scanlines
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
      subroutine read_calibration(ncid)

      use scanlines

      character(20) :: var
      integer       :: varid
      integer       :: nchan=24  

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

      allocate(tmbr(spots,nchan,scans))
      allocate(wtca(nchan,scans))
      allocate(ctca(nchan,scans))

      call check( nf_inq_ncid(ncid,"calibration",ncidc ) )

      var="hotc"
      call check( nf_inq_varid(ncidc,var,varid) )
      call check( nf_get_var(ncidc,varid,wtca ) )

      var="colc"
      call check( nf_inq_varid(ncidc,var,varid) )
      call check( nf_get_var(ncidc,varid,ctca ) )

      end subroutine read_calibration 
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------




