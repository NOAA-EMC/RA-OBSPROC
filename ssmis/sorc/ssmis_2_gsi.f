!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      program ssmis_2_gsi

      use scanlines
      use scene_las
      use netcdf

      integer , parameter :: maxarr=300

      character(255) :: gfname
      character(80)  :: bfname
      character(10)  :: cdate 
      character(3)   :: satid
      integer        :: plat,said,siid 
      real(4)        :: hour=0.    
      real(4)        :: hint=6.    
      real(8)        :: adate(6),bdate(6),dhrs,dscn,dspt
      real(8)        :: arr(maxarr),arr2(4,24),rdate

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

      open(6,recl=200)
      read(5,'(a)') gfname
      open(50,file='b021.tab')

! open the ssmi netcdf file and read the number of scans to read

      call check(nf_open(gfname,0,ncid) )

! read the satellite information

      call check( nf90_get_att(ncid, NF90_GLOBAL, "platform_identifier"      , plat) )
      call check( nf90_get_att(ncid, NF90_GLOBAL, "wmo_satellite_identifier" , said) )
      call check( nf90_get_att(ncid, NF90_GLOBAL, "wmo_instrument_identifier", siid) )

      write(satid,'("F",i2.2)') plat

! read the scanline info, channels, and calibraton for each feedhorn

      call read_scanlines(ncid)
      call read_calibration(ncid)

! read the group handles

      call check( nf_inq_ncid(ncid,"scene_las  ",ncid_las ) )
      call check( nf_inq_ncid(ncid,"scene_uas  ",ncid_uas ) )
      call check( nf_inq_ncid(ncid,"scene_img1 ",ncid_img1) )
      call check( nf_inq_ncid(ncid,"scene_img2 ",ncid_img2) )
      call check( nf_inq_ncid(ncid,"scene_env1 ",ncid_env1) )
      call check( nf_inq_ncid(ncid,"scene_env2 ",ncid_env2) )

! read the group data - do the qc - apply bias corrections - average to las spots

      call read_las (ncid_las )
      call read_uas (ncid_uas )
      call read_env1(ncid_env1)
      call read_env2(ncid_env2)
      call read_img1(ncid_img1)
      call read_img2(ncid_img2)
      print*

! fix starting date/time of first scan 

      adate(1)=1987
      adate(2)=01
      adate(3)=01
      adate(4)=0  
      adate(5)=0 
      adate(6)=0 
      dhrs = float(time(1))/3600.
      call redate(adate,dhrs,bdate)          

      adate = bdate ! save the first scan date in adate

      !write(6,*),adate
      !write(6,*),time(1)
      !write(6,*),dhrs 
      !write(6,*),bdate

      dscn=float(time(2)-time(1))/3600.
      dspt=dscn/dfloat(spots) 

! set rain and surface flags

      rfg = 0
      sfg = fill

! loop over scanlines               

      do iscan=1,scans

! find start time for the scan and increment for the spots

      dhrs=float(time(iscan)-time(1))/3600.
      call redate(adate,dhrs,bdate)
      dhrs=dhrs+.5*dspt
       
! pick a file to write this scanline in

      rdate=bdate(1)*1000000+bdate(2)*10000+bdate(3)*100+bdate(4)
      call centime(rdate,hour,hint,idate,dhr)
      write(cdate,'(I10)') i4dy(idate)
      write(bfname,*) 'SSMIS'//'.'//satid//'.'//cdate
      call outfile(bfname,lunot,iret)

! write a scan line into a bufr file

      do ispot=1,spots

      if (ispot==1) call redate(adate,dhrs,bdate)
      !write(6,'(2i6,7f10.3)') iscan,ispot,dhrs,bdate
      dhrs=dhrs+dspt        
 
      call openmb(lunot,'NC021201',idate)

      arr      = 10d10
      arr(  1) = said                     ! CODE TAB  001007  SAID      SATELLITE IDENTIFIER
      arr(  2) = 28                       ! CODE TAB  008021  TSIG      TIME SIGNIFICANCE
      arr(  3) = bdate(1)                 ! YEAR      004001  YEAR      YEAR
      arr(  4) = bdate(2)                 ! MONTH     004002  MNTH      MONTH
      arr(  5) = bdate(3)                 ! DAY       004003  DAYS      DAY
      arr(  6) = bdate(4)                 ! HOUR      004004  HOUR      HOUR
      arr(  7) = bdate(5)                 ! MINUTE    004005  MINU      MINUTE
      arr(  8) = bdate(6)                 ! SECOND    004006  SECO      SECOND
      arr(  9) = iscan                    ! NUMERIC   005041  SLNM      SCAN LINE NUMBER
      arr( 10) = ispot                    ! NUMERIC   005043  FOVN      FIELD OF VIEW NUMBER
      arr( 11) = lat(ispot,iscan)         ! DEGREE    005002  CLAT      LATITUDE (COARSE ACCURACY)
      arr( 12) = lon(ispot,iscan)         ! DEGREE    006002  CLON      LONGITUDE (COARSE ACCURACY)
      arr( 13) = sfg(ispot,iscan)         ! CODE TAB  013040  SFLG      SURFACE FLAG
      arr( 14) = rfg(ispot,iscan)         ! CODE TAB  020029  RFLAG     RAIN FLAG

      arr2=10d10
      do ich=1,24
      arr2(1,ich) = ich                   ! NUMERIC   005042  CHNM      CHANNEL NUMBER
      arr2(2,ich) = tmbr(ispot,ich,iscan) ! KELVIN    012163  TMBR      BRIGHTNESS TEMPERATURE (HIGH ACC
      arr2(3,ich) = wtca(ich,iscan)       ! NUMERIC   021083  WTCA      WARM TARGET CALIBRATION
      arr2(4,ich) = ctca(ich,iscan)       ! NUMERIC   021084  CTCA      COLD TARGET CALIBRATION
      enddo

      call ufbseq(lunot,arr,maxarr,1,iret,'NC021201')
      call ufbseq(lunot,arr2,4,24,iret,'SSMISCHN')
      call writcp(lunot)

      enddo
      enddo

      call closeout

      stop
      end
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE REDATE(ADATE,DHOUR,BDATE)
 
      DIMENSION :: MON(12)
      REAL(8)   :: ADATE(6),BDATE(6),DHOUR,HR
      DATA MON/31,28,31,30,31,30,31,31,30,31,30,31/
 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 
!  ONE WAY OR ANOTHER PARSE A TEN DIGIT DATE INTEGER
!  -------------------------------------------------
 
      IY = nint(adate(1)) 
      IM = nint(adate(2)) 
      ID = nint(adate(3))
      HR = nint(adate(4))+dhour+adate(5)/60.+adate(6)/3600.

1     IF(MOD(IY,  4).NE.0) MON(2) = 28
      IF(MOD(IY,  4).EQ.0) MON(2) = 29
      IF(MOD(IY,100).EQ.0) MON(2) = 28
      IF(MOD(IY,400).EQ.0) MON(2) = 29
 
 
      IF(HR.LT.0) THEN
         HR = HR+24
         ID = ID-1
         IF(ID.EQ.0) THEN
            IM = IM-1
            IF(IM.EQ.0) THEN
               IM = 12
               IY = IY-1
            ENDIF
            ID = MON(IM)
         ENDIF
         GOTO 1
      ELSEIF(HR.GE.24) THEN
         HR = HR-24
         ID = ID+1
         IF(ID.GT.MON(IM)) THEN
            ID = 1
            IM = IM+1
            IF(IM.GT.12) THEN
               IM = 1
               IY = IY+1
            ENDIF
         ENDIF
         GOTO 1
      ENDIF
 
      bdate(1) = iy 
      bdate(2) = im 
      bdate(3) = id 
      bdate(4) = int(hr); delh=hr-bdate(4)
      bdate(5) = int(delh*60.)
      bdate(6) = (delh*60.-bdate(5))*60.
 
      RETURN
      END
