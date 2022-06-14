!------------------------------------------------------------------------------------------
! The format of each text file:
! type: VIS, IR, WV, WVCS (clear sky WV), SWIR (short wave IR)
! sat: GOES8, 9, 10, 11, 12, 13, 14, 15
! day: yyyymmdd - year, 2-digit month, day of month
! hms: hhmm - AMV time in hour, minute (UTC)
! lat: latitude (-90 90)
! lon: longitude (-180 180, *WEST POSITIVE*)
! pre: AMV pressure
! spd: speed in m/s
! dir: meteorological wind direction (0 to 360 degrees)
! rff: recursive filter flag (50 to 100)
! qiwf: QI with forecast (0 to 1)
! qinf: QI without forecast (0 to 1)
! zen: satellite zenith angle in degrees
! ch: cloud height method: WIN, HIST, H2O, CO2, BASE
!------------------------------------------------------------------------------------------

      program rdgoes

      integer , parameter :: maxarr=300

      character(255)      :: fname,tname
      character(80)       :: filename
      character(10)       :: adate  
      character(8)        :: subset
      character(6)        :: typ,sat,chm
      integer             :: day,hms,year,mnth,days,hour,minu,seco
      real(4)             :: lat,lon,prs,spd,dir,rff,qiwf,qinf,zen
      real(8)             :: rdate,arr(maxarr),rat
      real(8)             :: fill=10d10                  

      equivalence (sat,rat)

!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

      read(5,'(a)') fname; !print*,trim(fname)
      read(5,'(a)') tname; !print*,trim(tname)
      
      open(20,file=fname) ! goes data file
      open(50,file=tname) ! bufr table file
      
      read(20,*) ! skip the header

! read goes data loop 

1     read(20,*,end=100) typ,sat,day,hms,lat,lon,prs,spd,dir,rff,qiwf,qinf,zen,chm

      year = day/10000
      mnth = mod(day/100,100)
      days = mod(day,100)
      hour = hms/100
      minu = mod(hms,100)
      seco = 0

      rdate = day*100+hour
      rdate = rdate+float(iminu*60+seco)/3600.

      ! select assignment method 
      hamd=fill
      if(chm=='WIN ') hamd=1    ! IRW height assignment
      if(chm=='HIST') hamd=15   ! HIST ?? (missing)
      if(chm=='H2O ') hamd=3    ! H2O intercept height assignment
      if(chm=='CO2 ') hamd=4    ! CO2 slicing height assignment
      if(chm=='BASE') hamd=15   ! cloud base (missing)

      ! select platform
      said=fill
      if(sat=='GOES6 ') said=250
      if(sat=='GOES7 ') said=251
      if(sat=='GOES8 ') said=252
      if(sat=='GOES9 ') said=253
      if(sat=='GOES10') said=254
      if(sat=='GOES11') said=255
      if(sat=='GOES12') said=256
      if(sat=='GOES13') said=257
      if(sat=='GOES14') said=258
      if(sat=='GOES15') said=259

      ! select observation type
      if(typ=='IR') then
         subset='NC005010'
         swcm=1
         sccf=fill
      elseif(typ=='VIS') then
         subset='NC005012'
         swcm=2
         sccf=fill
      elseif(typ=='WV'.or.typ=='WVCS') then
         subset='NC005011'
         if(typ=='WV'  ) swcm=3
         if(typ=='WVCS') swcm=4
         sccf=fill
      elseif(typ=='SWIR') then
         subset='NC005010'
         swcm=1
         sccf=76869900000000.0
      else
         call bort('no proper type')
      endif 

      cimss = 176       ! ORIGINATING/GENERATING CENTER
      goes  = 241       ! SATELLITE CLASSIFICATION
      pccf  = qinf*100. ! percent confidence

      arr   = fill

      arr(  1) = fill      ! CODE TAB  008202  RCTS      RECEIPT TIME SIGNIFICANCE
      arr(  2) = year      ! YEAR      004200  RCYR      YEAR   - TIME OF RECEIPT
      arr(  3) = mnth      ! MONTH     004201  RCMO      MONTH  - TIME OF RECEIPT
      arr(  4) = days      ! DAY       004202  RCDY      DAY    - TIME OF RECEIPT
      arr(  5) = hour      ! HOUR      004203  RCHR      HOUR   - TIME OF RECEIPT
      arr(  6) = minu      ! MINUTE    004204  RCMI      MINUTE - TIME OF RECEIPT
      arr(  7) = rat       ! CCITT IA  001198  RPID      REPORT IDENTIFIER
      arr(  8) = fill      ! CODE TAB  033215  CORN      CORRECTED REPORT INDICATOR
      arr(  9) = said      ! CODE TAB  001007  SAID      SATELLITE IDENTIFIER
      arr( 10) = cimss     ! CODE TAB  001033  OGCE      ORIGINATING/GENERATING CENTER
      arr( 11) = goes      ! CODE TAB  002020  SCLF      SATELLITE CLASSIFICATION
      arr( 12) = zen       ! DEGREE    007024  SAZA      SATELLITE ZENITH ANGLE
      arr( 13) = fill      ! M         002028  SSNX      SEGMENT SIZE AT NADIR IN X DIREC
      arr( 14) = fill      ! M         002029  SSNY      SEGMENT SIZE AT NADIR IN Y DIREC
      arr( 15) = year      ! YEAR      004001  YEAR      YEAR
      arr( 16) = mnth      ! MONTH     004002  MNTH      MONTH
      arr( 17) = days      ! DAY       004003  DAYS      DAY
      arr( 18) = hour      ! HOUR      004004  HOUR      HOUR
      arr( 19) = minu      ! MINUTE    004005  MINU      MINUTES
      arr( 20) = seco      ! SECOND    004006  SECO      SECONDS
      arr( 21) = 0         ! HOUR      004024  TPHR      TIME PERIOD OR DISPLACEMENT
      arr( 22) = lat       ! DEGREES   005002  CLAT      LATITUDE (COARSE ACCURACY)
      arr( 23) = -lon      ! DEGREES   006002  CLON      LONGITUDE (COARSE ACCURACY)
      arr( 24) = fill      ! FLAG TAB  002152  SIDP      SATELLITE INSTRUMENT USED IN DAT
      arr( 25) = swcm      ! CODE TAB  002023  SWCM      SATELLITE DERIVED WIND CALCULATI
      arr( 26) = sccf      ! HZ        002153  SCCF      SATELLITE CHANNEL CENTER FREQUEC
      arr( 27) = fill      ! HZ        002154  SCBW      SATELLITE CHANNEL BAND WIDTH
      arr( 28) = fill      ! KELVIN    012071  CCST      COLDEST CLUSTER TEMPERATURE
      arr( 29) = fill      ! CODE TAB  002164  TCMD      TRACER CORRELATION METHOD
      arr( 30) = lscode    ! CODE TAB  008012  LSQL      LAND/SEA QUALIFIER
      arr( 31) = fill      ! CODE TAB  002057  OFGI      ORIGIN OF FIRST GUESS INFORMATIO
      arr( 32) = fill      ! CODE TAB  033216  SWQM      SDMEDIT SATELLITE WIND QUALITY M
      arr( 33) = hamd      ! CODE TAB  002163  HAMD      HEIGHT ASSIGNMENT METHOD
      arr( 34) = prs*100.  ! PASCALS   007004  PRLC      PRESSURE
      arr( 35) = dir       ! DEGREES   011001  WDIR      WIND DIRECTION
      arr( 36) = spd       ! M/SEC     011002  WSPD      WIND SPEED
      arr( 37) = cimss     ! CODE TAB  001033  OGCE      ORIGINATING/GENERATING CENTER   
      arr( 38) = 1         ! CODE TAB  001032  GNAP      GENERATING APPLICATION          
      arr( 39) = pccf      ! %         033007  PCCF      PERCENT CONFIDENCE              

! write a subset into bufr


      call centime(rdate,0.,6.,idate,dhr)
      write(adate,'(I10)') i4dy(idate)
      filename='satwnd.'//trim(sat)//'.'//trim(adate)
      call outfile(filename,lunot,iret)
      !!write(6,*) lunot,rdate,filename

      call openmb(lunot,subset,idate)
      call ufbseq(lunot,arr,maxarr,1,iret,subset)
      call writsb(lunot)

      goto 1

100   call closeout            

      end program 



