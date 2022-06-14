!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      program rdssmi

      character(255) gfname
      character(20)  var,varname,attname,attout
      character(80)  filename   
      character(10)  adate      
      character(3)   satid      
      integer        varid,plat,said,siid

      character(1) ,allocatable,dimension(:)     :: qscn,pflg
      character(1) ,allocatable,dimension(:,:)   :: qchn,sft
      integer(4)   ,allocatable,dimension(:)     :: time,tfrc
      integer(4)   ,allocatable,dimension(:,:)   :: qfov
      real   (8)   ,allocatable,dimension(:)     :: revo          
      real   (8)   ,allocatable,dimension(:,:)   :: lat,lon
      real   (4)   ,allocatable,dimension(:,:)   :: laz
      real   (4)   ,allocatable,dimension(:,:,:) :: tbr,ica,ein

      real(8) hdr(10),slca(5,64),sbrt(2,7,64),rdate

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

      read(5,'(a)',end=100) gfname
      open(6,recl=132)
      call wrdlen

! open the ssmi netcdf file to read

      call check(nf_open(gfname,0,ncid),"opening PL file")

! read the satellite information

      call check( nf_get_att(ncid, NF90_GLOBAL, "platform_identifier"      , plat) )
      call check( nf_get_att(ncid, NF90_GLOBAL, "wmo_satellite_identifier" , said) )
      call check( nf_get_att(ncid, NF90_GLOBAL, "wmo_instrument_identifier", siid) )

      write(satid,'("F",i2.2)') plat
      print*,plat,said,siid

! get the number of scans and chans and alloc those vars

      call getdim(ncid,'time'   ,ntime)
      call getdim(ncid,'channel',nchan)

      allocate(time(ntime))
      allocate(tfrc(ntime))
      allocate(revo(ntime))
      allocate(qscn(ntime))
      allocate(pflg(ntime))
      allocate(qchn(nchan,ntime))

! get the vars dimensioned by scan

      var='time'
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,time)   ,"getting "//var)
      
      var='tfrac'
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,tfrc )  ,"getting "//var)
      
      var='rev'
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,revo)   ,"getting "//var)
      
      
      var='qc_scan'
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,qscn)   ,"getting "//var)
     
      var='pflag'
      !call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      !call check( nf_get_var(ncid,varid,pflg)   ,"getting "//var)

      var='qc_channel'
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,qchn)   ,"getting "//var)

      !do i=1,10000
      !print'(7(z2.2,1x),1x,z2.2)',qchn(:,i),qscn(i)
      !enddo
      !stop

! get the id of the scene_env group and alloc the vars indexed time by postion

      call check(nf_inq_grp_ncid(ncid,'scene_env',ncid),"getting scene_env id")
      call getdim(ncid,'scene_across_track',nspot)
      !print*,ntime,nchan,nspot

      allocate(lat (nspot,ntime))
      allocate(lon (nspot,ntime))
      allocate(laz (nspot,ntime))
      allocate(qfov(nspot,ntime))
      allocate(sft (nspot,ntime))
      allocate(tbr (nspot,nchan,ntime))
      allocate(ica (nspot,nchan,ntime))
      allocate(ein (nspot,nchan,ntime))

! get the vars indexed by scan and postion

      var='lat'
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,lat)    ,"getting "//var)

      var='lon'
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,lon)    ,"getting "//var)

      var='laz'
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,laz)    ,"getting "//var)

      var='qc_fov'
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,qfov)   ,"getting "//var)

      var='sft'
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,sft)    ,"getting "//var)

      var='tb'
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,tbr)    ,"getting "//var)

      var='ical'
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,ica)    ,"getting "//var)

      var='eia_norm'
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,ein)    ,"getting "//var)

! loop through the scans performing qc, tb offsets, and writing bufr subsets

      do itime=1,ntime
      !!print'(7(z2.2,1x),1x,z2.2)',qchn(:,itime),qscn(itime)

! first convert time into y-m-d-h-m-s

      secnds=time(itime)+float(tfrc(itime))*1e-6
      secnds=nint(secnds-1.9)
      nhours=nint(secnds)/3600 
      nminut=(nint(secnds)-(nhours*3600))/60    
      nsecnd=(nint(secnds)-(nhours*3600)-(nminut*60))
      call addate(1987010100,nhours,idate)

      IYR = IDATE/1000000
      IMN = MOD(IDATE/10000  ,100)
      IDY = MOD(IDATE/100    ,100)
      IHR = MOD(IDATE        ,100)
      IMI = nminut 
      ISC = nsecnd                     

      rdate=idate ; rdate=rdate+float(imi*60+isc)/3600.
      !!print*,rdate
      
! apply quality flags 

      if(iupm(qscn(itime),8)/=0) then
         iqscn=iqscn+1
         cycle
      endif

      do ichan=1,nchan
      if(iupm(qchn(ichan,itime),8)/=0) then
         !iqchn(ichan)=iqchn(ichan)+1
         tbr(:,ichn,itime) = -1
      endif
      enddo
      
      do ispot=1,nspot
      if(iupm(qfov(ispot,itime),8)/=0) then
         !iqfov(ispot) = iqfov(ispot)+1
         tbr(ispot,:,itime) = -1
      endif
      enddo

! apply tbr eia normalization and intersensor calibration offsets

      do ispot=1,nspot
      do ichan=1,nchan
      if(tbr(ispot,ichan,itime)>0) then
         if(ein(ispot,ichan,itime)>0) tbr(ispot,ichan,itime)=tbr(ispot,ichan,itime)+ein(ispot,ichan,itime)
         if(ica(ispot,ichan,itime)>0) tbr(ispot,ichan,itime)=tbr(ispot,ichan,itime)+ica(ispot,ichan,itime)
      endif
      enddo
      enddo

! store this scan in arrays shaped for bufrlib

! NC012001 | SAID  YEAR  MNTH  DAYS  HOUR  MINU  SECO  ORBN  SCNN {SLCA}      |
! NC012001 | {SBRT}  {SLCA85}  {SBRT85}
! SLCA     | CLATH  CLONH  SFTG  POSN
! SRDA     | CHNM  TMBR
! SBRT     | "SRDA"7

      hdr(1)=said
      hdr(2)=iyr 
      hdr(3)=imn 
      hdr(4)=idy  
      hdr(5)=ihr  
      hdr(6)=imi 
      hdr(7)=iso 
      hdr(8)=revo(itime)
      hdr(9)=itime 

      do ispot=1,nspot
      slca(1,ispot)=lat(ispot,itime)
      slca(2,ispot)=lon(ispot,itime)
      slca(3,ispot)=iupm(sft(ispot,itime),8)
      slca(4,ispot)=ispot          
      do ichan=1,nchan
      sbrt(1,ichan,ispot)=ichan  
      sbrt(2,ichan,ispot)=tbr(ispot,ichan,itime)
      !print*,tbr(ispot,ichan,itime)
      enddo
      enddo 

! identify which synoptic file will get this subset
!
!     SUBROUTINE CENTIME(RDATE,HOUR,HINT,IDATE,DHR)
!     real(8)    rdate    ! input real(8) yyyymmddhh.hh observation time
!     real       hour     ! first synoptic time in day
!     real       hint     ! increment between synoptic times
!     integer    idate    ! integer yyyymmddhh synoptic time
!     real       dhr      ! idate+dhr = rdate

      hour=0
      hint=6
      call centime(rdate,hour,hint,idate,dhr)
      write(adate,'(I10)') i4dy(idate)
      filename='SSMI.'//trim(satid)//'.'//trim(adate)
      call outfile(filename,lunot,iret)
      !!write(6,*) lunot,rdate,filename

! write completed bufr subsets 

      !print*,'writing message'
      call openmb(lunot,'NC012001',idate)
      call ufbint(lunot,hdr,10,1,iret,'SAID YEAR MNTH DAYS HOUR MINU SECO ORBN SCNN')
      call ufbint(lunot,slca,5,nspot,iret,'CLATH CLONH SFTG POSN')

      CALL DRFINI(lunot,64,1,'{SBRT}')
      CALL UFBseq(lunot,SBRT,2*7,nspot,IRET,'SBRT')

      call writcp(lunot)

      enddo ! end of itime loop

      call closbf(lunot)

100   stop
      end
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine getdim(ncid,dimname,dim)
      use netcdf
      character(*)  dimname
      integer       dim,dimid
      call check( nf90_inq_dimid(ncid,dimname,dimid)          ,"getting "//dimname//" dimid") 
      call check( nf90_inquire_dimension(ncid,dimid,len = dim),"getting "//dimname//" dim")
      end subroutine getdim
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine getatt(ncid,varname,attname,attout)
      use netcdf
      character(*)    varname,attname,attout
      integer         idvar,len
      call check( nf90_inq_varid(ncid, varname, idvar)                  ,"getting "//varname//" idvar")
      call check( nf90_inquire_attribute(ncid, idvar, attname, len=len) ,"getting "//attname//" dim")
      call check( nf90_get_att(ncid, idvar, attname, attout)            ,"getting "//attout//" dim")
      end subroutine getatt
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine check(status, loc)
         use netcdf
         integer, intent(in) :: status
         character(len=*), intent(in) :: loc
         if(status /= NF90_NOERR) then
            write (*,*) "Error at ", loc
            write (*,*) NF90_STRERROR(status)
            call bort('netcdf check')
         end if
      end subroutine check
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE ADDATE(IDATE,JH,JDATE)                                 
                                                                        
      DIMENSION   MON(12)                                               
                                                                        
      DATA MON/31,28,31,30,31,30,31,31,30,31,30,31/                     
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IY = IDATE/1000000
      IM = MOD(IDATE/10000  ,100)                                       
      ID = MOD(IDATE/100    ,100)                                       
      IH = MOD(IDATE        ,100)                                       
      IH = IH+JH                                                        
                                                                        
1     IF(MOD(IY,4)  .NE.0) MON(2) = 28                                    
      IF(MOD(IY,4)  .EQ.0) MON(2) = 29                                    
      IF(MOD(IY,100).EQ.0) MON(2) = 28                                    
      IF(MOD(IY,400).EQ.0) MON(2) = 29                                    
                                                                        
      IF(IH.LT.0) THEN                                                  
         IH = IH+24                                                     
         ID = ID-1                                                      
         IF(ID.EQ.0) THEN                                               
            IM = IM-1                                                   
            IF(IM.EQ.0) THEN                                            
               IM = 12                                                  
               IY = IY-1                                                
               IF(IY.LT.0) IY = 99                                      
            ENDIF                                                       
            ID = MON(IM)                                                
         ENDIF                                                          
         GOTO 1                                                         
      ELSEIF(IH.GE.24) THEN                                             
         IH = IH-24                                                     
         ID = ID+1                                                      
         IF(ID.GT.MON(IM)) THEN                                         
            ID = 1                                                      
            IM = IM+1                                                   
            IF(IM.GT.12) THEN                                           
               IM = 1                                                   
               IY = IY+1
               IF(IY.EQ.100) IY = 00
            ENDIF                                                       
         ENDIF                                                          
         GOTO 1                                                         
      ENDIF                                                             
                                                                        
      JDATE = IY*1000000 + IM*10000 + ID*100 + IH                       
                                                                        
      RETURN                                                            
      END                                                               
