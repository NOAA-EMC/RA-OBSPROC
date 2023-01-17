C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: WAVE_DCODCLOC
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2000-03-08
C
C ABSTRACT: DECODES GLOBAL AVN, FNL, OR MRF GRIB GRIDDED ANALYSIS
C   FIELDS OF WSPD, WDIR, AIRT, RELATIVE HUMIDITY, AND OISST.
C   COLOCATES THE MODEL OUTPUT WITH THE UWI SCATTEROMETER SATEL-
C   LITE SIGMA NOTS data FOR FURTHER CORRECTIONS.
C
C PROGRAM HISTORY LOG:
C 1995-05-07  GERALD         Moved job to cray machine.
C 1998-09-03  PETERS         Y2K/F90 compliance.
C 1998-10-06  KEYSER         STDOUT format changes to account for
C                             4-digit year
C 1999-08-16  GERALD         Modified to use BAREAD to input grib
C                             files - IBM version
C 1999-08-16  KEYSER         Further modifications for IBM-SP
C
C
C USAGE:
C   INPUT FILES:
C     UNIT 11  - nmcdate
C     UNIT 12  - bufrwnds
C                scatterometer wind data in bufr format.
C     UNIT 13  - avn   gribbed file.
C     UNIT 14  - avn   gribbed index file.
C     UNIT 15  - oisst gribbed file.
C     UNIT 16  - oisst gribbed index file.
C
C   OUTPUT FILES:
C     UNIT 06  - standard output
C     UNIT 52  - decoded scatterometer wind data colocated
C                with model output.
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - AVNFLD   OISST    COLOC
C     LIBRARY:
C       IBM      - GETENV
C       W3LIB    - GETGB    W3TAGB   W3TAGE   W3FT01   W3FC05
C                - W3MOVDAT
C       BALIB    - BAOPENR  BACLOSE
C       BUFRLIB  - DATELEN  OPENBF   READMG   READSB   UFBINT
C                - UFBREP   CLOSBF
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =  20 - TROUBLE DECODING GRIBBED GLOBAL AVNFLDS.
C          =  21 - TROUBLE DECODING GRIBBED GLOBAL SSTFLDS.
C          =  22 - NO SCAT DATA AVAILABLE.
C          =  23 - TROUBLE DECODING BUFR SCAT DATA.
C
C REMARKS:
C   NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM-SP
C
C$$$
      PROGRAM WAVE_DCODCLOC
      common/spect/uu(361,181),vv(361,181),rh(361,181),airt(361,181)
      common/ssts/gsst(361,180)
       integer gerr,istop,err1
c
      call w3tagb('WAVE_DCODCLOC',2000,0068,0066,'NP22')
c
C.................................
c
         istop = 0
      call avnfld(istop)
        IF( Istop.eq.20 ) then
           print *,'read error from grib decoder iret= ',istop
           CALL W3TAGE('WAVE_DCODCLOC')
           STOP 20
c
        endif
              gerr = 0
         istop = 0
      call oisst(gerr)
        IF( gerr.eq.21 ) then
           istop = 21
           print *,'read error from gribsst decod  iret= ',istop
           CALL W3TAGE('WAVE_DCODCLOC')
           STOP 21
c
        endif
C
            err1 = 0
         istop = 0
      call coloc(err1)
         IF( err1.eq.23)then
           istop = 23
           print *,'read error from scatdecoder  iret= ',istop
           CALL W3TAGE('WAVE_DCODCLOC')
            STOP 23
c
             elseif(err1.eq.22)then
           iSTOP =  22
           print *,'scat data not available   iret= ',istop
           CALL W3TAGE('WAVE_DCODCLOC')
            STOP 22
c
         endif

        CALL W3TAGE('WAVE_DCODCLOC')
C
      stop
      end
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    AVNFLD      UNPACK GLOBAL GRIBBED AVN MODEL
C   PRGMMR: GERALD           ORG: NP21        DATE: 1999-08-16
C
C ABSTRACT: DECODES GLOBAL AVN, MRF, GDAS GRIBBED MODEL OUTPUT.
C
C PROGRAM HISTORY LOG:
C 1995-08-01  GERALD
C 1999-08-16  GERALD         Modified to use BAREAD to input grib
C                             files - IBM version
C
C
C USAGE:
C   INPUT FILES:
C     UNIT 13  - global model gribbed fields.
C     UNIT 14  - global model grib index file.
C
C   OUTPUT FILES:
C     UNIT 06  - standard output file.
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =  20 - GRIB DECODER ERR
C
C REMARKS:
C   NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM-SP
C
C$$$
C
      subroutine avnfld(istop)
C
      Parameter(jf=360*181)
      character*11 envvar
      character*80 fileb,filei
      logical lb(jf)
      INTEGER PDS,GDS,GRID
      INTEGER JPDS(25),JGDS(22),IGRD(5,3)
      INTEGER KPDS(25),KGDS(22)
      real fld(360,181)
c
      common/spect/uu(361,181),vv(361,181),
     *     rh(361,181),airt(361,181)
c
C
C                U   V   RH  AT lsmask
      data IGRD/ 33, 34, 52, 11, 81,
     *          105,105,105,105,  1,
     *           10,  10, 2,  2,  0/
c
C            INPUT UNITS FOR DECODING GRIB FILE
C
          LUGB=13
          LUGI=14
c
      envvar='XLFUNIT_   '
      write(envvar(9:10),fmt='(I2)') lugb
      call getenv(envvar,fileb)
      envvar='XLFUNIT_   '
      write(envvar(9:10),fmt='(I2)') lugi
      call getenv(envvar,filei)
      call baopenr(lugb,fileb,iret1)
      call baopenr(lugi,filei,iret2)
c
          jg = 0
C
C........    DECODE THE FILEDS
C
        DO 30 GRID = 1,4
C
          DO 10 PDS=1,25
          JPDS(PDS) = -1
   10      CONTINUE
C
             DO 20 GDS = 1,22
           JGDS(GDS) = -1
   20     CONTINUE
C
C........   GET SELECT FIELDS
C
           jPDS(5) = IGRD(GRID,1)
           jPDS(6) = IGRD(GRID,2)
           jPDS(7) = IGRD(GRID,3)
C
             print *,'call getgb'
           CALL GETGB(LUGB,LUGI,JF,jg,JPDS,JGDS,
     *                          KF,K,KPDS,KGDS,LB,Fld,IRET)
C
        IF( IRET.NE.0 ) then
           istop = 20
           return
        endif
c
            WRITE(6,61)KPDS,KF,KGDS
   61  FORMAT(2(/,2X,'PDS=',13I7),2(/,2X,' GDS=',11I7 ))
c
c....     pass field to proper array
c
      if(grid.eq.1) then
           do j=1,181
             jj = 182 - j
            do i = 1,360
             uu(i,jj)= fld(i,j)
            enddo
           enddo
c
      elseif(grid.eq.2) then
           do j=1,181
             jj = 182 - j
            do i = 1,360
             vv(i,jj) = fld(i,j)
            enddo
           enddo
c
      elseif(grid.eq.3) then
           do j=1,181
             jj = 182 - j
            do i = 1,360
             rh(i,jj) = fld(i,j)
            enddo
           enddo
c
      elseif(grid.eq.4) then
           do j=1,181
             jj = 182 - j
            do i = 1,360
             airt(i,jj) = fld(i,j)-273.15
            enddo
           enddo
c
      endif
C
   30        CONTINUE
C
c............   colocate model output with ESA wind data ....
c...     add greenwich to right side
c
          do jj = 1,181
           airt(361,jj) = airt(1,jj)
             rh(361,jj) =   rh(1,jj)
             uu(361,jj) =   uu(1,jj)
             vv(361,jj) =   vv(1,jj)
          end do
c
          write(6,66)(airt(i,90),i=1,5),(rh(i,90),i=1,5),
     *    (uu(i,90),i=1,5),(vv(i,90),i=1,5)
   66  format(2(/,1x,10f8.2))
c
  999           CONTINUE
        CALL BACLOSE (LUGB,iret)
        CALL BACLOSE (LUGI,iret)
c
c
             return
             END
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    OISST       UNPACK GLOBAL GRIB SST
C   PRGMMR: GERALD           ORG: NP21        DATE: 1999-08-16
C
C ABSTRACT: DECODES GLOBAL OISST GRIB GRIDDED FIELDS.
C
C PROGRAM HISTORY LOG:
C 1995-08-01  GERALD
C 1999-08-16  GERALD         Modified to use BAREAD to input grib
C                             files - IBM version
C
C
C USAGE:
C   INPUT FILES:
C     UNIT 15  - global sst gribbed fields.
C     UNIT 16  - global sst grib index file.
C
C   OUTPUT FILES:
C     UNIT 06  - standard output file.
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =  21 - GRIB DECODER ERR
C
C REMARKS:
C   NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM-SP
C
C$$$
C
      subroutine oisst(err)
c
      Parameter(jf=360*180)
      INTEGER PDS,GDS,GRID,err
      INTEGER JPDS(25),JGDS(22),IGRD(5,3)
      INTEGER KPDS(25),KGDS(22)
      REAl FLD(jf),out(360,180)
      character*11 envvar
      character*80 fileb,filei
      common/ssts/gsst(361,180)
      logical lb(jf)
      equivalence(out(1,1),fld(1))
c
C
C                sst
      DATA IGRD/ 11, 34,  2, 52, 11,
     *            1,105,102,105,105,
     *            0, 10,  0,  2,  2/
c
C            INPUT UNITS FOR DECODING GRIB FILE
C
          LUGB=15
          LUGI=16
c
      envvar='XLFUNIT_   '
      write(envvar(9:10),fmt='(I2)') lugb
      call getenv(envvar,fileb)
      envvar='XLFUNIT_   '
      write(envvar(9:10),fmt='(I2)') lugi
      call getenv(envvar,filei)
      call baopenr(lugb,fileb,iret1)
      call baopenr(lugi,filei,iret2)
c
          j = 0
C
C........    DECODE THE FILEDS
C
        DO 30 GRID = 1,1
C
          DO 10 PDS=1,25
          JPDS(PDS) = -1
   10      CONTINUE
C
             DO 20 GDS = 1,22
           JGDS(GDS) = -1
   20     CONTINUE
C
C........   GET SELECT FIELDS
C
           jPDS(5) = IGRD(GRID,1)
           jPDS(6) = IGRD(GRID,2)
           jPDS(7) = IGRD(GRID,3)
C
             print *,'call getgb'
           CALL GETGB(LUGB,LUGI,JF,j,JPDS,JGDS,
     *                          KF,K,KPDS,KGDS,LB,Fld,IRET)
C
          IF(IRET.NE.0) then
               err = 21
               return
          endif
c
            WRITE(6,61)KPDS,KF,KGDS
   61  FORMAT(2(/,2X,'PDS=',13I7),2(/,2X,' GDS=',11I7 ))
C
c..   flip grid to PT(1,1) =(0e,-90.0)
c
              do  jj= 1,180
                do  kk= 1,360
                 iflip = 181 - jj
                 gsst(kk,iflip) = out(kk,jj)
                end do
              end do
c
c...   add greenich to right side of grid
c
             do jj = 1,180
               gsst(361,jj) = gsst(1,jj)
             end do
C
   30        CONTINUE
C
  999           CONTINUE
        CALL BACLOSE (LUGB,iret)
        CALL BACLOSE (LUGI,iret)
c
             return
             END
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    COLOC       UNPACK SCAT WIND DATA FROM BUFFER FMT
C   PRGMMR: KEYSER           ORG: NP21        DATE: 1999-08-16
C
C ABSTRACT: DECODE ERS2 SCATTEROMETER WIND DATA FROM BUFFER FOR-
C   MAT AND WRITE DATA TO A TEMPORARY FILE FOR LATER PROCESSING.
C
C PROGRAM HISTORY LOG:
C 1993-03-01  GERALD
C 1995-05-07  GERALD          MODIFIED PROGRAM TO RUN ON C90.
C 1996-06-27  PETERS          MODIFIED PROGRAM TO USE JACK WOOLEN'S
C                             BUFR INTERFACE INSTEAD OF W3FI78. ERS-2
C			      DATA IS NOW UNPACKED USING WOOLEN'S DE-
C			      CODER ON CRAY4.  DATA NOW COMES SCALED
C                             PROPERLY AFTER CALL; DATA IS IN REAL, 
C                             NOT INTEGER FORMAT.
C 1996-07-16  PETERS          UPDATED MNEMONICS FOR NEW BUFR FORMAT 
C 1998-05-05  PETERS          REMOVED NON-Y2K COMPLIANT CALLS TO OB-
C                             SELETE W3 ROUTINES, REPLACED W/ W3MOVDAT
C 1999-08-16  KEYSER          MODIFICATIONS FOR IBM-SP
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - CONTROL CARD FOR DATA SCAN
C     UNIT 11  - NCEP PRODUCTION DATE FILE
C                GET CURRENT SYNOPTIC YYYYMMDD
C     UNIT 12  - NSS.STATAICO.ERS1.UWIBUFR
C                BUFR CODED DATASET WITH SCAT WIND DATA
C     UNIT 19  - LAND SEA TAGS FOR GLOBAL MODEL
C
C   OUTPUT FILES:
C     UNIT 52  - DECODED ERS2 SCATTEROMETER DATA COLOCATED
C                WITH THE GLOBAL OISST ANALYSIS.
C     UNIT 06  - STANDARD OUTPUT
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          <>  0 - ERROR MESSAGE FROM BUFR DECODER
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM-SP
C
C$$$
C
C     PURPOSE - READ SAT OBS FROM A BUFR FILE.....
C     FOR SELECTED TIME & COLOCATE WITH SST ANALYSIS
C ---------------------------------------------------------
      subroutine coloc(err)
c
      PARAMETER (BUFMAX=10.0E6)
      REAL ZMASK(360,181),uu,vv,art,rh
      REAL RINC(5)
      real(8)  hdr_8(5),time_8(6,2),beam_8(5,3),hdr2_8(3)
      INTEGER IDAT(8),JDAT(8)
      INTEGER   DUMY(31)
      INTEGER   FLUX
      INTEGER   ERR
c
      CHARACTER*8 SUBSET
      CHARACTER*80 HSTR, HS1, HS2, HS3, HSQ
      DIMENSION HDR(5), OBS(30), BEAM(5,3), TIME(6,2), HDR2(3)
      DATA HSTR/'NC012008'/
      DATA HS1/'SAID DOMO SIDU CLAT CLON'/
      DATA HSQ/'YEAR MNTH DAYS HOUR MINU SECO'/
      DATA HS2/'RAIA RALA BKST RARE MPCN'/
      DATA HS3/'WS10 WD10 WSPC2'/
      common/spect/uu(361,181),vv(361,181),rh(361,181),art(361,181)
      COMMON/ssts/GSST(361,180)
C
             kt = 0
          DO 93 IDUM = 1,31
            DUMY(IDUM) = 99999
C
   93      CONTINUE
C
C.................................
C..     GET LAND SEA MASK
C.................................
      READ(19)ZMASK
        write(6,69)
   69  format(1x,'  zmask read ')
        rewind 19
C
        iu = 12
C
C ... GET CURRENT CLOCK
        READ(11,FMT='(6X,I4,2I2)') IY, IM, ID
C.................................
C..     GET OISST
C.................................
               ERR = 0
C
C.....................................
C       INPUT BOGUS DATE IF NEEDED
C.....................................
C     IY = 1998
C     IM = 05
C     ID = 07
C     ihr = 00, 06, 12, or 18
c
C ..  HOUR OF DATA AND WINDOW (+/- HOURS)
c
      READ(5,501) IHR
  501 FORMAT(I2)
        flux = ihr
c
           iwndw = 3
      WRITE(6,606) IY,IM,ID,IHR,IWNDW,FLUX
  606 FORMAT(2X,' TIME  ',I4,2I2,5X,'IHR, WINDOW',2I4,' FLUX =',I3)
C
C ......     SET WINDOW SCAN FOR SYNOPTIC TIME
C
      IHR2 = IHR + IWNDW
      IHR1 = IHR - IWNDW
C
        IF(FLUX.EQ.00) GO TO 80
C
      IDAT1 = IY  *1000000 + IM  *10000 + ID  *100 + IHR1
      IDAT2 = IY  *1000000 + IM  *10000 + ID  *100 + IHR2
      MDATE = IY  *1000000 + IM  *10000 + ID  *100 + IHR
C
          GO TO 81
C
   80     CONTINUE
C
      RINC(1) = 0.0
      RINC(2) = -3.0
      RINC(3) = 0.0
      RINC(4) = 0.0
      RINC(5) = 0.0
      IDAT(1) = IY
      IDAT(2) = IM
      IDAT(3) = ID
      IDAT(4) = 0  
      IDAT(5) = IHR
      IDAT(6) = 0
      IDAT(7) = 0
      IDAT(8) = 0
      CALL W3MOVDAT(RINC,IDAT,JDAT)
      IYR1 = JDAT(1)
      IMO1 = JDAT(2)
      IDA1 = JDAT(3)
      IHR1 = JDAT(5)
      RINC(2) = 3.0
      CALL W3MOVDAT(RINC,IDAT,JDAT)
      IYR2 = JDAT(1)
      IMO2 = JDAT(2)
      IDA2 = JDAT(3)
      IHR2 = JDAT(5)
C
      IDAT1 = IYR1*1000000 + IMO1*10000 + IDA1*100 + IHR1
      IDAT2 = IYR2*1000000 + IMO2*10000 + IDA2*100 + IHR2
      MDATE = IYR2*1000000 + IMO2*10000 + IDA2*100 + IHR
C
   81         CONTINUE
C..................
C
      WRITE(6,620) IDAT1,IDAT2,MDATE
  620 FORMAT(1X,' DATE TIME WINDOW ',2I11,' MODEL OUTPUT ',I11)
      NNL = 5
    5 NWR = 0
      MAXD = 0
      MIND = 99999999
      MAXDT= 0
      MINDT= 99999999
      MAXDA= 0
      MINDA= 99999999
      X = 0.
      KT = 0
      MNWT = 0
      ICOUNT= 0
      NNWT = 0
      MNWA = 0
      NNWA = 0
      NN = 0
       land = 0
      TTIME = 0.0
C-----------------------------------------------------------------------
C                       DECODE BUFR FILE
C-----------------------------------------------------------------------
C
C  OPEN THE BUFR INPUT FILE
C  ------------------------
      LUBFI=IU
      CALL DATELEN(10)
      CALL OPENBF(LUBFI,'IN',LUBFI)
      CALL READMG(LUBFI,SUBSET,IDATE,IRET)
      IF(IRET.NE.0) GOTO 100
      PRINT*,'READING DATA FOR ',IDATE
C
C  READ A SUBSET - READ A MESSAGE WHEN NO SUBSETS - END WHEN NO MESSAGES
C  ---------------------------------------------------------------------
  150 CONTINUE
      CALL READSB(LUBFI,IRET)
      IF(IRET.NE.0) THEN
         CALL READMG(LUBFI,SUBSET,IDATE,IRET)
         IF(IRET.NE.0) GOTO 100
         GOTO 150
      ENDIF
C
      ICOUNT = ICOUNT + 1
      IF(ICOUNT.LE.500000)GO TO 250
      PRINT 1002
 1002 FORMAT(' ***** NUMBER OF OBS EXCEEDED 500000 *****')
      CALL W3TAGE('WAVE_DCODCLOC')
      STOP 200
  250 CONTINUE
C  -----------------------------------------------------------------
C  CALL UFBINT, UFBREP TO GET THE DATA                     
C  ----------------------------------------------- 
      CALL UFBINT(LUBFI,HDR_8,5,  1,IRET,HS1)  ; hdr=hdr_8
      CALL UFBREP(LUBFI,TIME_8,6,  2,IRET,HSQ) ; time=time_8
      CALL UFBREP(LUBFI,BEAM_8,5,  3,IRET,HS2) ; beam=beam_8
      CALL UFBINT(LUBFI,HDR2_8,3,  1,IRET,HS3) ; hdr2=hdr2_8
c     WRITE(6,*) 'IRET= ',IRET
      DO I = 1,3
       OBS(I) = HDR(I)
      END DO
      DO I =4,9
         IX = I - 3
         OBS(I) = TIME(IX,1)
      END DO
      IF (HDR(4).GT.BUFMAX.OR.HDR(5).GT.BUFMAX) GOTO 150
      OBS(10) = HDR(4)
      OBS(11) = HDR(5)
      OBS(12)=0.0
      INUM=13
      DO J = 1,3
       DO K = 1,5
        OBS(INUM)=BEAM(K,J)
        IF (OBS(INUM).GT.BUFMAX) OBS(INUM)=-99.
        INUM = INUM + 1
       END DO
      END DO
      DO M = 1,3
        OBS(INUM)=HDR2(M)
        IF (OBS(INUM).GT.BUFMAX) OBS(INUM)=-99.
        INUM=INUM+1
      END DO
      IF (ICOUNT.LT.100) THEN
        DO II = 1,30
         WRITE(6,*) 'I = ',II,'OBS(I)= ',OBS(II)
        END DO
      ENDIF
ccccc WRITE(3,*) OBS(4),OBS(7),OBS(8),OBS(9)
C-----------------------------------------------------------------------
C                      SEARCH FOR DATA
C-----------------------------------------------------------------------
      IF(IU.EQ.12.OR.IU.EQ.22) M = 30
C     MYR = NINT(OBS(4)) -((NINT(OBS(4)))/100)*100
      MYR = NINT(OBS(4))
      IDATC = MYR*1000000 + NINT(OBS(5))*10000 + NINT(OBS(6))*100 +
     $  NINT(OBS(7))
C
C.........       IS RETRIEVAL ON LAND
C
       SLAT = OBS(10)
       SLON = OBS(11)
         ylon = slon
       IF(SLON.LT.0) YLON = 360. + SLON
           LON = NINT(YLON)
       IF(SLAT.LT.0.0) THEN
          XL = ABS(SLAT)
          LAT = 90 + NINT(XL) + 1
        ELSE
          LAT = 90 - NINT(SLAT) + 1
      ENDIF
            if(zmask(lon,lat).lt.0.8) land= land + 1
c
         IF(ZMASK(LON,LAT).LT.0.8) GO TO 150
C
C ... TIME WINDOW
C
      IF(IDATC.GE.IDAT1.AND.IDATC.LT.IDAT2) GO TO 151
           GO TO 150
C
C ... SAVE ON FILE FOR MATCHUP WITH MODEL OUTPUT
C.....................................................
C
  151 CONTINUE
C
C..................    WRITE OUT DECODED OBS
C
      NN = NN + 1
C
C     CONVERT LON FROM NESDIS TO NMC SYSTEM ON29 FMT
      IF(SLON.GE.0.0) YLON = 360. - SLON
      IF(SLON.LT.0.0) YLON = -SLON
C
       XI = (361. -( YLON - 0.5))
       XJ = 91.0 + (sLAT + .5)
C
          CALL W3FT01(XI,XJ,GSST,SST,361,180,0,1)
C
          SST = SST -273.15
c.......
c           coloc retrieval to model fields
c
        if(slon.ge.0.0) ylon= slon + 1.
        if(slon.lt.0.0) ylon = (361. + slon)
           ylat = 91. + slat
c
         call w3ft01(ylon,ylat,uu,uuu,361,181,0,1)
         call w3ft01(ylon,ylat,vv,vvv,361,181,0,1)
           call w3fc05(uuu,vvv,wdir,wspd)
         call w3ft01(ylon,ylat,art,airt,361,181,0,1)
         call w3ft01(ylon,ylat,rh,relh,361,181,0,1)
c
C
       WRITE(52)(OBS(I),I=1,30),wdir,wspd,SST,airt,relh,mdate
C.....................................................
C
      IF(IDATC.GT.MAXD) MAXD = IDATC
      IF(IDATC.LT.MIND) MIND = IDATC
C
      WS   = OBS(28)
      WD   = OBS(29)
      SATDIR = OBS(2)
      ANGLE1 = OBS(13)
      RLOOK1 = OBS(14)
      SCAT1  = OBS(15)
      SKP1   = OBS(16)
      ANGLE2 = OBS(18)
      RLOOK2 = OBS(19)
      SCAT2  = OBS(20)
      SKP2   = OBS(21)
      ANGLE3 = OBS(23)
      RLOOK3 = OBS(24)
      SCAT3  = OBS(25)
      SKP3   = OBS(26)
      IF (MOD(ICOUNT,1000).EQ.0)
     *PRINT 1199, ICOUNT,IDATC,SLAT,SLON,Wd,Ws,wdir,wspd,
     *      sst,airt,relh
 1199 FORMAT(1X,' GERALD ',I10,I11,9F7.1,' la lo wd ws md ms st at rh')
      NNWA = NNWA + 1
      IF(OBS(28).NE.-99) MNWA = MNWA + 1
      IF(IDATC.GT.MAXDA) MAXDA= IDATC
      IF(IDATC.LT.MINDA) MINDA= IDATC
C
      NNWT = NNWT + 1
      IF(OBS(28).NE.-99) MNWT = MNWT + 1
      IF(IDATC.GT.MAXDT) MAXDT = IDATC
      IF(IDATC.LT.MINDT) MINDT = IDATC
      N = IDATC - IDAT1
      GO TO 150
  100 CONTINUE
      CALL CLOSBF(LUBFI)
      IF (ICOUNT.LT.1) THEN 
        WRITE(6,*) 'READMG ERROR W/ BUFR MSG:IRET=',IRET
        ERR = 23
        WRITE(52)DUMY
        RETURN
      ENDIF
      WRITE(6,*) 'AFTER WOOLENS INTERFACE:  ICOUNT= ',ICOUNT
      WRITE(6,*) 'NUMBER OBS WITHIN WINDOW = ',NN
C CCCCCCC
      IF(MAXDT.LT.IDAT1) WRITE(6,638) MAXDT,IDAT1
  638 FORMAT(1x,'  NO SAT DATA   ALL DATA TOO OLD  +++++++++++',2I11)
      WRITE(6,617)
  617 FORMAT(1x,'  TIMES AND TOTAL NUMBER OF WINDS OVER THE AREA')
      WRITE(6,618) IDAT1,IDAT2,ICOUNT
  618 FORMAT(2X,3I11)
C
      REWIND IU
         IF(ICOUNT.LE.1) THEN 
           ERR = 22
           WRITE(52)DUMY
         ENDIF
C
      END FILE 52
      REWIND 52
C.................
      WRITE(6,692)icount,land
  692 FORMAT(1x,'END OF JOB','count= ',i8,' land pnts= ',i8)
C
   99 CONTINUE
      CALL W3TAGE('WAVE_DCODCLOC')
      STOP
      END
