C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .
C MAIN PROGRAM: WAVE_DATAQC
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2000-03-03
C
C ABSTRACT:   READS IN DATA FILE CONTAINING CO-LOCATED ERS-2 SCATTER-
C   OMETER MEASUREMENTS (DECODED FROM BUFR IN PREVIOUS STEP DCODCLOC)   
C   AND NMC GLOBAL MODEL FIELDS.  PERFORMS SEVERAL QUALITY CHECKS FOR 
C   MISSING DATA, DATA OVER LAND AND OVER POSSIBLE REGIONS OF SEA ICE. 
C   SAVES BLOCK, CELL AND ROW NUMBER OF DATA FOR FURTHER PROCESSING.
C
C PROGRAM HISTORY LOG:
C 1993-03-09 WRITTEN INITIALLY BY VERA GERALD 
C 1994-06-21 ADDED DOCBLOCK, CLEANED UP UNNECESSARY COMMENTS - C. PETERS
C 1994-11-18 ELIMINATED SOME UNNECESSARY OUTPUT FILES        - C. PETERS
C 1996-06-28 MODIFIED TO READ ERS-2 DATA ALREADY PROPERLY 
C            SCALED, IN FLOATING POINT FORMAT                - C. PETERS
C 1998-09-03 Y2K/F90 COMPLIANT                               - C. PETERS
C 1998-10-06 MODIFIED TO OUTPUT REPORTS WITH A 4-DIGIT YEAR
C            RATHER THAN A 2-DIGIT YEAR           - D. KEYSER, C. PETERS
C 1999-08-16 ADDED COND. CODE 99                             - D. KEYSER
C
C USAGE:
C   INPUT FILES:
C     UNIT 11  - MODEL COLOCATED ERS-2 SCATTEROMETER DATA FILE
C
C   OUTPUT FILES:
C     UNIT 51  - QUALITY CONTROLLED ERS-2 SCATTEROMETER DATA FILE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:  - QCHECK
C     LIBRARY
C       W3LIB: - W3TAGB   W3TAGE
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =  99 - TROUBLE READING INPUT SCATTEROMETER DATA FILE
C
C
C REMARKS: OUTPUT FILE IN UNIT 51 IS PASSED ON FOR FURTHER PROCESSING
C   BY PROGRAM DATASORT.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM-SP
C$$$
C     PROGRAM UNPACK BUFFER: PROGRAMMERS HISTORY LOG (DETAILED)
C
C     GERALD  - MODIFIED ON MAR  9, '93                  
C     PURPOSE - READ SAT OBS MATCHUP WITH MODEL OUTPUT FROM OPC ARCHIVE
c               take data from AVN run for 00Z and 12Z
c                              FNL run for 06Z and 18Z
c     Mar 16 1993 - moved to CRAY and changed output formats .
c                 - save the block number, row number and cell number on
c                   output file.
c                 - add quality checks on several values. write to 
c                   an error output files in case of errors.       R Teb
c     Mar 24 1993 - check if the data came in order (from early to a lat
c                   time. Change order of lines if needed (create a
c                   new output file.                               R.Teb
c     April 2, 1993 - Added SST, RH and Air Temp. to the output (3F6.1).
C                                                            C. Peters
c     April 9, 1993 - Added a quality check for the SST (reject if < 0.0
c                     and added a counter for the number of records kept
c                     acceptable. Number of records and blocks added to 
c		      put as a new file called "ers1.YMMDDHHrpt".
c                                                            C. Peters
c     April 26, 1993 - Added time check for each block of 361 records.
c		       Added check of cell and row ID numers. Renamed
c                      program from unpkbuff2.f to dataqc.f
c
c		                                            P. Woiceshyn
c     June 28, 1996 - Modified slightly to read in ERS-2 data matched wi
c                     global model data.  ERS-2 data now comes already s
c                     properly, and the 30 UWI parameters are now in flo
c                     point rather than integer format.  This is due to 
c                     use of Woolen's BUFR interface in the previous ste
c
c                                                             C. Peters
c

c $=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=$=
c
c  5 May 1994 - changed from ASCII to BINARY for OPERATIONAL RUNS --  P.
  
      PROGRAM WAVE_DATAQC
 
      INTEGER CDATE,SEC                
      INTEGER IBIT(12),KIBIT(12)             
      LOGICAL LBIT(12)                       
      INTEGER IOBS(30)
      REAL OBS(30)
      CHARACTER*8 ITITLE                     
      CHARACTER*6  IDH                       
      CHARACTER*2  IDK                      
      CHARACTER*16 IDLOC                   
      REAL VALS,VALD,VSST,VATP,VSH,robs(30)
      integer siro,sjcel,pirow,pjcel
C                                        
      DIMENSION BYLON(6000),BXLAT(6000),ITMME(6000)            
      DIMENSION FYLON(6000),FXLAT(6000)                       
      DIMENSION XSCAT1(6000),XSCAT2(6000),XSCAT3(6000)       
      DIMENSION XSKP1(6000),XSKP2(6000),XSKP3(6000)         
      DIMENSION THETA1(6000),THETA2(6000),THETA3(6000)     
      DIMENSION XWS(6000),XWD(6000)                       
      DIMENSION XLOOK1(6000),XLOOK2(6000),XLOOK3(6000)   
c     keep low and high values for each possible cell.
      real cellow(19),celhgh(19)
c
c
      data cellow /22.4,26.0,28.4,30.7,32.9,35.05,37.1,
     1             39.05,40.9,42.7,44.45,46.1,47.7,49.2,
     1             50.65,52.1,53.45,54.7,55.9/,
     1     celhgh /26.0,28.4,30.7,32.9,35.05,37.1,39.05,
     1             40.9,42.7,44.45,46.1,47.7,49.2,50.65,
     1             52.1,53.45,54.7,55.9,58.0/
      CALL W3TAGB('WAVE_DATAQC',2000,0063,0081,'NP22') 

c
C-----------------------------------------------------------  
C                       READ BUFR FILE                     
C-----------------------------------------------------------
C   
c     iu = input unit number
c     iu999 = output file number to get indexes of lines with values>999
c     iuiang = output file number to get indexes of lines with inc.angle
c     iouter = output file number to get lines with conf.flags <> 0.


          iu = 11
        iout = 51
cpmw		    iff = iout   or  iff = iouter  !!!  later in program
       iu999 = 94
      iuiang = 95
      iouter = 96

      nc = 19
c-cp
      do i4 = 1,30
       iobs(i4) = 0
      end do
c-cp
c
c
      write (iuiang,124)
 124  format ('iblock,irow,jcell,oldang,ang',/,
     1        '============================') 
      write (iu999,125)
 125  format ('iblock,  irow, jcell,index in row,value',/,
     1        '=======================================') 
c
      iblock = 0
      nrec = 0
c
      ICOUNT = 0
      
      ishms = 0
      ijump = 0      

  150 CONTINUE 
      
c     get the next block of data along the ERS1 track.

      iblock = iblock + 1
c     if (iblock.eq.3) STOP
c
c1122 continue
      irocel = 0
      pirow = 0

      
      do 149 irow=1,nc
c     =================
      oldang = -88888.
      do 149 jcell=1,nc
c     =================
C                    
      irocel = irocel + 1   ! will count 1 to 361

cpw      if (ijump .eq. 1) go to 777

cpw      ijump = 0

ccccccc     change this read and define the new veriables   cccccccccccc

      READ(IU,ERR=888,END=999) OBS,VALD,VALS,VSST,VATP,VSH,CDATE      
C                                                      
      ICOUNT = ICOUNT + 1                             
      if (icount.eq.1) then
          WRITE(6,692) (OBS(I),I=1,30),VALS,VALD,CDATE
      elseif (icount.lt.30) then
          WRITE(6,693) (OBS(I),I=1,30),VALS,VALD,CDATE
      endif
  692 FORMAT(8X,'Sat ID  track instrum year    month  day    hour    ',
     1          'min seconds lat   long       -  rad.in. r.look back',
     1          ' scat',/,93x,'angle  angle  sigma',/,8x,120('='),/,1x,
     1          'IOBS',15F7.2,/,7x,
     1          'Noise  miss rad.in.r.look back sc. Noise miss rad.in',
     1          '.r.look back sc. Noise miss   wind   wind   UWI    ',
     1          'wind  wind   date',/,7x,
     1          'Kp     count angle angle  sigma    Kp    count  angle',
     1          ' angle  sigma    Kp    count  spd    dir    P.conf ',
     1          'spd   dir',/,7x,120('='),/, 2X,15F7.2,1X,2F7.1,1X,I10,
     1 ///)              
  693 FORMAT(2X,'IOBS',15F7.2,/,2X,15F7.2,1X,2F7.1,1X,I10)              
      XLAT = OBS(10)                                  
      XLON = OBS(11)                                 
cvvvvvy2k
cdak  iymd = (nint(obs(4))-(nint(obs(4))/100)*100)*10000 + 
cdak & nint(obs(5))*100 + nint(obs(6))
      iymd = nint(obs(4))*10000 + nint(obs(5))*100 + nint(obs(6))
caaaaay2k
c     if (icount.le.10) write (iout,6694) obs(9)
c6694 format ('obs(9)=seconds=',i6)
      isec = int(obs(9))
      ihms = int(obs(7))*10000 + int(obs(8))*100 + isec
      
cpw      if ((ishms .ne. ihms) .and. (irocel .ne. 1)) then
cpw      ijump = 1

cpw      go to 150
cpw      endif

cpw  777 continue
  
cpw      ishms = ihms
        
c-cp
      do 6692 i5 = 1,3
        iobs(i5) = int(obs(i5))
 6692 end do
c
      do 6693 i6 = 13,30
        robs(i6) = obs(i6)
        iobs(i6) = nint(obs(i6))
 6693 continue
c     robs(13) = robs(13)/10.
c     robs(14) = robs(14)/10.
c     robs(15) = robs(15)/100.
c     robs(18) = robs(18)/10.
c     robs(19) = robs(19)/10.
c     robs(20) = robs(20)/100.
c     robs(23) = robs(23)/10.
c     robs(24) = robs(24)/10.
c     robs(25) = robs(25)/100.
c     robs(28) = robs(28)/10.
      
      
c     sjcel = jcell
      
c     siro = irow

c     check first inc. angle to decide which cell we are at.
      do 801 i101=1,19
          if (robs(13).ge.cellow(i101) .and. 
     1        robs(13).lt.celhgh(i101)) then
            sjcel = i101
            if (irocel.eq.1) then
              siro = 1
            elseif (sjcel.lt.pjcel) then
                siro = pirow+1
c               if (siro.gt.19) siro=1
            else
                siro = pirow
            endif

            pirow = siro
            pjcel = sjcel
c           print *,'irocel,siro,sjcel=',irocel,',',siro,',',sjcel

        goto 802
        endif
 801  continue
c
c     Should never get to this area. It means that I have a reading
c     that doesn't match any cell.
      print *,' *****  error : sai(0,',i,') = ',robs(13)
      print *,'iblock, siro, sjcel = ',
     1         iblock,',',siro,',',sjcel
      print *,'****************************'
      S T O P
 802  continue
c
      if (icount.eq.1) then
cpmw         write (iout) cdate
cpmw         write (iout,123) cdate
         write (90,122) cdate
cpmw         write (18,122) cdate
         write (iouter,123) cdate
      endif
 122  format (i10)
 123  format (//,'    ERS2 SCATTEROMETER AND NMC MODEL',/,
     1           '    MODEL DATE = ',i10,/,
     1           ' ===================================',///)
c
c
c  =====================================================================
ccc   iff = iout
ccc   nrec = nrec + 1
ccc   goto 1244

c  =====================================================================
 1245 call qcheck(iobs(30),iobs(14),iobs(19),iobs(24),ierr)

      if (ierr.eq.0.and.VSST.GE.0.0) then
         iff = iout
c-cp
c-cp check for values of specific humidity less than zero
c-cp
         if (VSH.lt.0.0) VSH=0.0
c  =====================================================================
          WRITE(iff) iblock,siro,sjcel,(IOBS(I),I=1,3),
     1    iymd,ihms,xlat,xlon,
     1    (robs(i),i=13,29),iobs(30),VALS,VALD,VSST,VATP,VSH
          nrec = nrec + 1
         else 
          iff = iouter
          WRITE(iff,694) iblock,siro,sjcel,(IOBS(I),I=1,3),
     1    iymd,ihms,xlat,xlon,
     1    (robs(i),i=13,29),iobs(30),VALS,VALD,VSST,VATP,VSH
         endif
 1244 continue

cpmw      if (iblock.eq.11 .and. siro.eq.19 .and. sjcel.eq.5) print *,
cpmw     1  '***********vsst=',vsst,' iff=',iff

cvvvvvy2k
c694  format (i4,2i3,i2,i4,i2,2i7,f7.2,f8.2,   
 694  format (i4,2i3,i2,i4,i2,i9,i7,f7.2,f8.2,   
caaaaay2k
     1    3(f5.1,f6.1,f7.2,f5.0,f5.0),2f6.1,i4,f5.1,f6.1,3f6.1)
      if (oldang.gt.robs(13)) 
     1      write (iuiang,695) iblock,irow,jcell,oldang,robs(13)

 695  format (3i4,2f10.2)
      oldang = robs(13)
c     look for values greater than 999.
      do 696 i696=13,30
        if (robs(i696).ge.999.) then
          write (iu999,698) iblock,irow,jcell,i696,robs(i696)

 698      format (3i7,i9,f10.2)
          goto 697
        endif
 696  continue

 697  continue
C                                                               
      IDSAT  = INT(OBS(1)) 
      ITRK  =  INT(OBS(2))
      INSTR =  INT(OBS(3))
      MYR   =  INT(OBS(4))                                       
      MON   =  INT(OBS(5))                                      
      MDAY  =  INT(OBS(6))                                     
      IH    =  INT(OBS(7))                                    
      MIN   =  INT(OBS(8))                                   
      SEC   =  NINT(OBS(9))                                  
      ANGLE1 = OBS(13)                     
      RANGL1 = OBS(14)                    
      BSCAT1 = OBS(15)                  
      NOISA1 = INT(OBS(16))                        
      MISS1  = INT(OBS(17))                      
      ANGLE2 = OBS(18)                
      RANGL2 = OBS(19)               
      BSCAT2 = OBS(20)             
      NOISA2 = INT(OBS(21))                        
      MISS2  = INT(OBS(22))                     
      ANGLE3 = OBS(23)           
      RANGL3 = OBS(24)          
      BSCAT3 = OBS(25)        
      NOISA3 = INT(OBS(26))                   
      MISS3  = INT(OBS(27))                 
      WS   = OBS(28)        
      WD   = OBS(29)           
      ICONFF  =  INT(OBS(30))             
C                                   
C......    DECODE CONFIDENCE FLAG  
C                                 
      DO 10 II = 1,12            
      LB = II - 1               
      NB = 13-II               
      LBIT(NB) = BTEST(ICONFF,LB) 
   10 CONTINUE                   
C                               
C     WRITE(6,60)ICONFF,LBIT   
   60 FORMAT(1X ,' DECODED CONFIDENCE FLAG ',I12,1X,12L4)
C                                                       
C.........                                             
      IF(ICOUNT.eq.1) write (6,168) 
 168  format (' scatterometer',20x,'model',/,
     1  'year mon day hour  wspd    dir   wspd    dir    date',
     1/,'====================================================')
      IF(ICOUNT.LE.30) then
c       WRITE(6,68)MYR,MON,MDAY,IH,WS,VALS,WD,VALD,CDATE
        WRITE(6,68)MYR,MON,MDAY,IH,WS,WD,vals,VALD,CDATE
   68   FORMAT(1X ,4I4,4F7.1,1X,I10,/)                  
        WRITE(6,101)IDSAT,ITRK,INSTR,MYR,MON,MDAY,IH,MIN,SEC,XLAT,XLON,
     *             ANGLE1,RANGL1,BSCAT1,NOISA1,MISS1,               
     *             ANGLE2,RANGL2,BSCAT2,NOISA2,MISS2,              
     *             ANGLE3,RANGL3,BSCAT3,NOISA3,MISS3,             
     *             WD,WS,VALD,VALS,CDATE,ICONFF,LBIT             
  101   FORMAT(1X,4I5,1X,5I5,1X,2F7.1,3(/,10X,2F7.1,f7.2,2I7),         
     *       /,2X,4F7.1,1X,2I8,12L2,//)                        
C ---------------------------------------------------------- 
C     WRITE(6,1199)IDSAT,ITRK,INSTR,MYR,MON,MDAY,IH,MIN,SEC,XLAT,XLON,
C    *             ANGLE1,RANGL1,BSCAT1,NOISA1,MISS1,                
C    *             ANGLE2,RANGL2,BSCAT2,NOISA2,MISS1,               
C    *             ANGLE3,RANGL3,BSCAT3,NOISA3,MISS1,              
C    *             WS,SPDN,WD,ADIR,CDATE,ICONFF                   
C1199 FORMAT(1X ,'V.M. PRINT ',1X,3I6,1X,6I4,1X,2F6.1,           
C    *3(/,10X,4F7.1,1X,I7),/,4F7.2,1X,2I8)     
C ..                                          
      endif
C .. CHECK CONFIDENCE FLAGS, MOVE ON IF IT IS A LAND POINT  
       IF(ICOUNT.LT.10)                                    
     *WRITE(6,69)LBIT,ICONFF                              
   69 FORMAT(1X ,' GERALD ',12L2,' CONF',I5,/)           
C                                                       
C                                                      
 149  continue
      write(6,*) ' irocel = ',irocel,' iblock = ',iblock,
     1  ' sjcel = ',sjcel,' siro = ',siro

      GO TO 150                                       
C                                                    
  888 continue
      PRINT 1007                                    
 1007 FORMAT(' PROBLEM READING FT10 FILE')
      call w3tage('WAVE_DATAQC') 
      stop 99
  999 CONTINUE
      if(icount.eq.0)  then
         print *, ' PROBLEM: INPUT FILE EMPTY - STOP 99'
         call w3tage('WAVE_DATAQC') 
         stop 99
      end if
      CALL W3TAGE('WAVE_DATAQC') 
      STOP  
      END    
c
C#######################################################################
C#######################################################################
C#######################################################################

      subroutine qcheck(obs,ival14,ival19,ival24,ierr)
c     ================================================
c
c     quality check the bits of this obs
c     ierr = 1 if any of the bits is turned on
c
      integer obs
c
      ierr = 0
      ifb  = mod(obs     ,2)
      imb  = mod(obs/2   ,2)
      iab  = mod(obs/4   ,2)
      ifba = mod(obs/8   ,2)
      imba = mod(obs/16  ,2)
      iaba = mod(obs/32  ,2)
      ikpt = mod(obs/64  ,2)
      ils  = mod(obs/128 ,2)
      iaut = mod(obs/256 ,2)
      imetb= mod(obs/512 ,2)
      ifc  = mod(obs/1024,2)
      imis = mod(obs/2048,2)
c
      if (imis .gt.0 .or.         ! no flags present
     1    ifb  .gt.0 .or.         ! fore beam sigma presence
     1    imb  .gt.0 .or.         ! mid beam sigma presence
     1    iab  .gt.0 .or.         ! aft beam sigma presence
     1    ifba .gt.0 .or.         ! fore beam arcing
     1    imba .gt.0 .or.         ! mid beam arcing
     1    iaba .gt.0 .or.         ! aft beam arcing
     1    ikpt .gt.0 .or.         ! Kp >20%
c    1    iaut .gt.0 .or.         ! autonomous ambiguity removal
c    1    imetb.gt.0 .or.         ! meteorological background
     1    ifc  .gt.0 .or.         ! checksum error detection
     1    ils  .gt.0 ) then       ! land contamination
c.or.         ! land contamination
c    1    ival14.gt.200 .or. 
c    1    ival19.gt.200 .or. 
c    1    ival24.gt.200 ) then
c    1    .or.iceskp.ne.0)  then
          ierr = 1
      endif

      return
      end
