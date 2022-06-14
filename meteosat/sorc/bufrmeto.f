!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      program rdmeto

      parameter(maxarr=2000)

      character(255) gfname
      character(20)  var,varname,attname,attout
      character(80)  filename   
      character(12)  qic(11)    
      character(10)  adate      
      character(8)   subset,prnt
      character(3)   chan       
      integer        varid,kqi(11,0:10)
      logical        print/.false./

      character(20)  time_att
      character(20)  plat_att
      integer        segx_att
      integer        segy_att
      real(4)        cfre_att

      real(4),allocatable,dimension(:):: latitude
      real(4),allocatable,dimension(:):: longitude
      real(4),allocatable,dimension(:):: sat_zen
      real(4),allocatable,dimension(:):: tar_type
      real(4),allocatable,dimension(:):: wind_meth
      real(4),allocatable,dimension(:):: lf
      real(4),allocatable,dimension(:):: lsm
      real(4),allocatable,dimension(:):: speed
      real(4),allocatable,dimension(:):: u
      real(4),allocatable,dimension(:):: v
      real(4),allocatable,dimension(:):: dir
      real(4),allocatable,dimension(:):: height
      real(4),allocatable,dimension(:):: bt
      real(4),allocatable,dimension(:):: heightstd
      real(4),allocatable,dimension(:):: qi
      real(4),allocatable,dimension(:):: qi_exc
      real(4),allocatable,dimension(:):: qi_fcst
      real(4),allocatable,dimension(:):: tvec
      real(4),allocatable,dimension(:):: qi_spd
      real(4),allocatable,dimension(:):: qi_dir
      real(4),allocatable,dimension(:):: svec
      real(4),allocatable,dimension(:):: qi_s_height
      real(4),allocatable,dimension(:):: qi_chan
      real(4),allocatable,dimension(:):: qi_height
      real(4),allocatable,dimension(:):: qi_t_height
      real(4),allocatable,dimension(:):: dir1
      real(4),allocatable,dimension(:):: dir2
      real(4),allocatable,dimension(:):: dir3
      real(4),allocatable,dimension(:):: speed1
      real(4),allocatable,dimension(:):: speed2
      real(4),allocatable,dimension(:):: speed3

      real(8) arr(maxarr),rdate,x/10d10/

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

      read(5,'(a)') gfname
      read(5,'(a)') chan    
      read(5,'(a)') prnt    

      if(prnt=='print') print=.true.

      open(6,recl=132)

      if(print) then
         print*
         write(6,*) chan,trim(gfname)
         print*
      endif

      !call wrdlen

! open the meteosat netcdf file to read

      call check(nf_open(gfname,0,ncid),"opening PL file")
      nf90_global=0

! get some information from the global attributes

      call getatt_c(ncid,NF90_GLOBAL,'time' ,time_att)
      call getatt_c(ncid,NF90_GLOBAL,'platform' ,plat_att)
      call getatt_i(ncid,NF90_GLOBAL,'segment_width' ,segx_att)
      call getatt_i(ncid,NF90_GLOBAL,'segment_height' ,segy_att)
      call getatt_r(ncid,NF90_GLOBAL,'channel_frequency_center',cfre_att)

      if(print) then
         print*,time_att
         print*,plat_att
         print*,segx_att
         print*,segy_att
         print*,cfre_att
      endif

      read(time_att,'(i4,5(1x,i2))') iyr,imo,idy,ihr,imi,isc 

      if(plat_att=='MET02') said=59
      if(plat_att=='MET03') said=50
      if(plat_att=='MET04') said=51
      if(plat_att=='MET05') said=52
      if(plat_att=='MET06') said=53
      if(plat_att=='MET07') said=54
      if(plat_att=='MET08') said=55
      if(plat_att=='MET09') said=56
      if(plat_att=='MET10') said=57
      if(plat_att=='MET11') said=70

! get the number of scans and chans and alloc those vars

      call getdim(ncid,'numAMV'   ,numamv)

      if(print) then
         print*
         print*,'numamv=',numamv
         print*
      endif

      allocate(latitude(numamv))
      allocate(longitude(numamv))
      allocate(sat_zen(numamv))
      allocate(tar_type(numamv))
      allocate(wind_meth(numamv))
      allocate(lf(numamv))
      allocate(lsm(numamv))
      allocate(speed(numamv))
      allocate(u(numamv))
      allocate(v(numamv))
      allocate(dir(numamv))
      allocate(height(numamv))
      allocate(bt(numamv))
      allocate(heightstd(numamv))
      allocate(qi(numamv))
      allocate(qi_exc(numamv))
      allocate(qi_fcst(numamv))
      allocate(tvec(numamv))
      allocate(qi_spd(numamv))
      allocate(qi_dir(numamv))
      allocate(svec(numamv))
      allocate(qi_s_height(numamv))
      allocate(qi_chan(numamv))
      allocate(qi_height(numamv))
      allocate(qi_t_height(numamv))
      allocate(dir1(numamv))
      allocate(dir2(numamv))
      allocate(dir3(numamv))
      allocate(speed1(numamv))
      allocate(speed2(numamv))
      allocate(speed3(numamv))

! get the vars dimensioned by scan


      var="latitude"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,latitude)   ,"getting "//var)

      var="longitude"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,longitude)   ,"getting "//var)

      var="sat_zen"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,sat_zen)   ,"getting "//var)

      var="tar_type"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,tar_type)   ,"getting "//var)

      var="wind_meth"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,wind_meth)   ,"getting "//var)

      var="lf"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,lf)   ,"getting "//var)

      var="lsm"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,lsm)   ,"getting "//var)

      var="speed"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,speed)   ,"getting "//var)

      var="u"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,u)   ,"getting "//var)

      var="v"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,v)   ,"getting "//var)

      var="dir"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,dir)   ,"getting "//var)

      var="height"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,height)   ,"getting "//var)

      var="bt"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,bt)   ,"getting "//var)

      var="heightstd"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,heightstd)   ,"getting "//var)

      var="qi"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,qi)   ,"getting "//var)

      var="qi_exc"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,qi_exc)   ,"getting "//var)

      var="qi_fcst"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,qi_fcst)   ,"getting "//var)

      var="tvec"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,tvec)   ,"getting "//var)

      var="qi_spd"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,qi_spd)   ,"getting "//var)

      var="qi_dir"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,qi_dir)   ,"getting "//var)

      var="svec"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,svec)   ,"getting "//var)

      var="qi_s_height"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,qi_s_height)   ,"getting "//var)

      var="qi_chan"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,qi_chan)   ,"getting "//var)

      var="qi_height"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,qi_height)   ,"getting "//var)

      var="qi_t_height"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,qi_t_height)   ,"getting "//var)

      var="dir1"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,dir1)   ,"getting "//var)

      var="dir2"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,dir2)   ,"getting "//var)

      var="dir3"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,dir3)   ,"getting "//var)

      var="speed1"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,speed1)   ,"getting "//var)

      var="speed2"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,speed2)   ,"getting "//var)

      var="speed3"
      call check( nf_inq_varid(ncid,var,varid)  ,"getting "//var//" varid")
      call check( nf_get_var(ncid,varid,speed3)   ,"getting "//var)

      if(print) then
         do n=1,numamv
         print*,latitude(n)," latitude"
         print*,longitude(n)," longitude"
         print*,sat_zen(n)," sat_zen"
         print*,tar_type(n)," tar_type"
         print*,wind_meth(n)," wind_meth"
         print*,lf(n)," lf"
         print*,lsm(n)," lsm"
         print*,speed(n)," speed"
         print*,u(n)," u"
         print*,v(n)," v"
         print*,dir(n)," dir"
         print*,height(n)," height"
         print*,bt(n)," bt"
         print*,heightstd(n)," heightstd"
         print*,qi(n)," qi"
         print*,qi_exc(n)," qi_exc"
         print*,qi_fcst(n)," qi_fcst"
         print*,tvec(n)," tvec"
         print*,qi_spd(n)," qi_spd"
         print*,qi_dir(n)," qi_dir"
         print*,svec(n)," svec"
         print*,qi_s_height(n)," qi_s_height"
         print*,qi_chan(n)," qi_chan"
         print*,qi_height(n)," qi_height"
         print*,qi_t_height(n)," qi_t_height"
         print*,dir1(n)," dir1"
         print*,dir2(n)," dir2"
         print*,dir3(n)," dir3"
         print*,speed1(n)," speed1"
         print*,speed2(n)," speed2"
         print*,speed3(n)," speed3"
         read(stdin,*)
         enddo
      endif

! diagram the qi indicators
!
      qic( 1)='qi'
      qic( 2)='tvec'       
      qic( 3)='svec'    
      qic( 4)='qi_spd'
      qic( 5)='qi_dir'
      qic( 6)='qi_exc'
      qic( 7)='qi_fcst'
      qic( 8)='qi_chan'
      qic( 9)='qi_height'
      qic(10)='qi_s_height'
      qic(11)='qi_t_height'

      do n=1,numamv
      index=nint(qi(n)         *.1); if(index>=0.and.index<=10) kqi( 1,index)=kqi( 1,index)+1
      index=nint(tvec(n)       *.1); if(index>=0.and.index<=10) kqi( 2,index)=kqi( 2,index)+1
      index=nint(svec(n)       *.1); if(index>=0.and.index<=10) kqi( 3,index)=kqi( 3,index)+1
      index=nint(qi_spd(n)     *.1); if(index>=0.and.index<=10) kqi( 4,index)=kqi( 4,index)+1
      index=nint(qi_dir(n)     *.1); if(index>=0.and.index<=10) kqi( 5,index)=kqi( 5,index)+1
      index=nint(qi_exc(n)     *.1); if(index>=0.and.index<=10) kqi( 6,index)=kqi( 6,index)+1
      index=nint(qi_fcst(n)    *.1); if(index>=0.and.index<=10) kqi( 7,index)=kqi( 7,index)+1
      index=nint(qi_chan(n)    *.1); if(index>=0.and.index<=10) kqi( 8,index)=kqi( 8,index)+1
      index=nint(qi_height(n)  *.1); if(index>=0.and.index<=10) kqi( 9,index)=kqi( 9,index)+1
      index=nint(qi_s_height(n)*.1); if(index>=0.and.index<=10) kqi(10,index)=kqi(10,index)+1
      index=nint(qi_t_height(n)*.1); if(index>=0.and.index<=10) kqi(11,index)=kqi(11,index)+1 
      enddo       

      if(print) then
         do i=1,11
         if(i==6) cycle
         if(i==7) cycle
         print'(a12,11i10)',qic(i),kqi(i,:)
         enddo
         print*
         stop
      endif

! write into bufr arrays

      open(50,file='satwnd.tab')

      ! determine which kind of subset/channel it is

      if(chan=='IR') then
         subset='NC005067'
         swcm = 1
      elseif(chan=='VI') then
         subset='NC005068'
         swcm = 2
      elseif(chan=='WV') then
         subset='NC005069'
         swcm = 3
      else
         call bort('undefined chan')
      endif


      do n=1,numamv

      ! check the quality marks

      ! assignment list for NC005067 MSG TYPE 005-067 EUMETSAT SATWIND, MTSAT IR (BUF

      arr=x

      arr(  1) = 254  !eumetsat! CODE TAB  001033  OGCE      ORIGINATING/GENERATING CENTER   
      arr(  2) = 0             ! CODE TAB  001034  GSES      IDENTIFICATION OF ORIGINATING/GE
      arr(  3) = 'Release2'    ! CCITT IA  025061  SOFTV     SOFTWARE IDENTIFICATION AND VERS
      arr(  4) = 5             ! NUMERIC   025062  DBID      DATABASE IDENTIFICATION         
      arr(  5) = said          ! CODE TAB  001007  SAID      SATELLITE IDENTIFIER            
      arr(  6) = cfre_att      ! HZ        002153  SCCF      SATELLITE CHANNEL CENTER FREQUEC
      arr(  7) = x             ! DEGREE T  001012  DOMO      DIRECTION OF MOTION OF MOVING OB
      arr(  8) = x             ! M         002026  CTRE      CROSS-TRACK RESOLUTION (AT NADIR
      arr(  9) = x             ! M         002027  ATRE      ALONG-TRACK RESOLUTION (AT NADIR
      arr( 10) = segx_att      ! M         002028  SSNX      SEGMENT SIZE AT NADIR IN X DIREC
      arr( 11) = segy_att      ! M         002029  SSNY      SEGMENT SIZE AT NADIR IN Y DIREC
      arr( 12) = x             ! FLAG TAB  002161  WDPF      WIND PROCESSING METHOD          
      arr( 13) = x             ! CODE TAB  002164  TCMD      TRACER CORRELATION METHOD       
      arr( 14) = swcm          ! CODE TAB  002023  SWCM      SATELLITE DERIVED WIND CALCULATI
      arr( 15) = lsm(n)        ! CODE TAB  008012  LSQL      LAND/SEA QUALIFIER              
      arr( 16) = x             ! CODE TAB  008013  DNQL      DAY/NIGHT QUALIFIER             
      arr( 17) = x             ! NUMERIC   001124  GPTI      GRID POINT IDENTIFIER           
      arr( 18) = latitude(n)   ! DEGREE(N  005001  CLATH     LATITUDE (HIGH ACCURACY)        
      arr( 19) = longitude(n)  ! DEGREE(E  006001  CLONH     LONGITUDE (HIGH ACCURACY)       
      arr( 20) = iyr           ! YEAR      004001  YEAR      YEAR                            
      arr( 21) = imo           ! MONTH     004002  MNTH      MONTH                           
      arr( 22) = idy           ! DAY       004003  DAYS      DAY                             
      arr( 23) = ihr           ! HOUR      004004  HOUR      HOUR                            
      arr( 24) = imi           ! MINUTE    004005  MINU      MINUTES                         
      arr( 25) = isc           ! SECONDS   004006  SECO      SECONDS                         
      arr( 26) = 3600          ! SECOND    004086  LTDS      LONG TIME PERIOD OR DISPLACEMENT
      arr( 27) = x             ! CODE TAB  002162  EHAM      EXTENDED HEIGHT ASSIGNMENT METHO
      arr( 28) = height(n)*100.! PASCALS   007004  PRLC      PRESSURE                        
      arr( 29) = dir(n)        ! DEGREES   011001  WDIR      WIND DIRECTION                  
      arr( 30) = speed(n)      ! M/SEC     011002  WSPD      WIND SPEED                      
      arr( 31) = u(n)          ! M/SEC     011003  UWND      U-COMPONENT                     
      arr( 32) = v(n)          ! M/SEC     011004  VWND      V-COMPONENT                     
      arr( 33) = x             ! DEGREES   012001  TMDBST    TEMPERATURE/DRY BULB TEMPERATURE
      arr( 34) = x             ! M         020014  HOCT      HEIGHT OF TOP OF CLOUD          
      arr( 35) = sat_zen(n)    ! DEGREE    007024  SAZA      SATELLITE ZENITH ANGLE          
      arr( 36) = nseq          ! NUMERIC   001023  OSQN      OBSERVATION SEQUENCE NUMBER     

      ! find the ouput file to write this report

      idate=iyr*1000000+imo*10000+idy*100+ihr
      rdate=idate
      rdate=rdate+float(imi*60+isc)/3600.

      call centime(rdate,0.,6.,idate,dhr)
      write(adate,'(I10)') i4dy(idate)
      filename='satwnd.'//trim(plat_att)//'.'//trim(adate)
      call outfile(filename,lunot,iret)


      ! write a subset containing this report

      call openmb(lunot,subset,idate)
      call ufbseq(lunot,arr,maxarr,1,iret,'SATDRWN2')
      call writsb(lunot)
      
      enddo
     
100   continue
      call closbf(lunot)
      stop
      end
