C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      PROGRAM convert 
 
      PARAMETER(I1=10000)
      CHARACTER(200) FILENAME
      CHARACTER(80)  DASTR  
      CHARACTER(20)  SATID 
      CHARACTER(10)  ADATE  
      CHARACTER(8)   SUBSET
      REAL(8)        ARR(I1),date(6),rdate

      DATA LUNIN  /20/
      DATA LUNDX  /21/ ! thin unit has the input  bufr table
      DATA LUNDO  /50/ ! this unit has the output bufr table

      data hour   /00/
      data hint   /06/

      data dastr/'YEAR MNTH DAYS HOUR MINU SECO'/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      read(5,'(a)') satid
 
C  COMPUTE AN MESSAGE INVENTORY BY SUBSETS
C  ---------------------------------------
 
      CALL OPENBF(LUNIN,'IN ',LUNDX)
      DO WHILE(IREADMG(LUNIN,SUBSET,IDATE)==0)
      DO WHILE(IREADSB(LUNIN)==0)

      !figure which synoptic time it fits
      call ufbint(lunin,date,6,1,iret,dastr)
      iyr=nint(date(1))
      imn=nint(date(2))
      idy=nint(date(3))
      ihr=nint(date(4))
      imi=nint(date(5))
      isc=nint(date(6))
      rdate=iyr*1000000+imn*10000+idy*100+ihr
      rdate=rdate+float(imi*60+isc)/3600.
      call centime(rdate,hour,hint,idate,dhr)

      ! open a synoptic file to get this profile
      write(adate,'(I10)') i4dy(idate)
      filename=trim(satid)//'.'//trim(adate)
      call outfile(filename,lunbo,iret)
      if(iret==1) then
         call openmg(lunbo,subset,idate)
         call openmg(lunbo,subset,idate)
         call closmg(lunbo)
      endif

      ! copy the profile into the synoptic file
      call openmb(lunbo,subset,idate)
      call ufbcpy(lunin,lunbo)
      call writcp(lunbo)
      enddo
      enddo

      call closbf(lunbo)
      stop
      end
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE OUTFILE(FILENAME,LUBFR,IRET)

      COMMON /RQUIET/ IPRT

      PARAMETER (NF=1)

      CHARACTER*80 FILENAME,FLNEW,FLBFR(NF)
      CHARACTER*8  SUBSET

      DATA LUNDX /50/
      DATA LBFDX /51/
      DATA LUOUT /60/

      SAVE FLBFR,LFBFR

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      iprt=1

!  SEE IF THE USER TABLE NEEDS TO BE PROCESSED ONE TIME
!  ----------------------------------------------------

      CALL STATUS(LBFDX,LUN,IL,IM)
      IF(IL.EQ.0) THEN
         CALL OPENBF(LBFDX,'IN',LUNDX)
         LFBFR = 0
      ENDIF

!  MAKE A FILE NAME FOR THIS REPORT
!  --------------------------------

      FLNEW = FILENAME

!  SEE IF WE SHOULD OPEN OR CREATE A NEW FILE
!  ------------------------------------------

      DO IOUT=1,NF
      IF(FLNEW.EQ.FLBFR(IOUT)) THEN
         LUBFR = LUOUT+IOUT
         IRET=0; RETURN
      ENDIF
      ENDDO

!  OPEN A NEW FILE UNIT
!  --------------------

      LFBFR = MAX(MOD(LFBFR+1,NF+1),1)
      FLBFR(LFBFR) = FLNEW
      LUBFR = LUOUT+LFBFR

      CALL CLOSBF(LUBFR)
      OPEN(LUBFR,FILE=FLNEW,FORM='UNFORMATTED')

!  CHECK WHETHER TO OPEN OR CREATE FILE
!  ------------------------------------

      REWIND LUBFR
      READ(LUBFR,END=10)

      IF(IPRT.GT.0) WRITE(6,*) 'OPENING  ',FLNEW(1:70)
      CALL OPENBF(LUBFR,'APN',LBFDX)
      IRET=0; RETURN

10    IF(IPRT.GT.0) WRITE(6,*) 'CREATING ',FLNEW(1:70)
      CALL OPENBF(LUBFR,'OUT',LBFDX)
      IRET=1; RETURN

!  ENTRY POINT TO CLOSE ALL CACHED FILES
!  -------------------------------------

      ENTRY CLOSEOUT
      DO IOUT=1,NF
      CALL CLOSBF(LUOUT+IOUT)
      ENDDO

      RETURN
      END
!-----------------------------------------------------------------------
!  find synoptic group containing an arbitrary observation time
!-----------------------------------------------------------------------

      SUBROUTINE CENTIME(RDATE,HOUR,HINT,IDATE,DHR)

      real(8)    rdate    ! input real(8) yyyymmddhh.hh observation time
      real       hour     ! firt synoptic time in day
      real       hint     ! increment between synoptic times
      integer    idate    ! integer yyyymmddhh synoptic time
      real       dhr      ! idate+dhr = rdate

      REAL*8 ADATE,BDATE,CDATE,DDATE

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      IDATE = RDATE
      DDATE = (IDATE/100)*100
      DOUR  = RDATE - DDATE
      WINT  = HINT/2.

      DO SOUR=HOUR,24.,HINT
      CALL RADDATE(DDATE, SOUR,ADATE)
      CALL RADDATE(ADATE,-WINT,BDATE)
      CALL RADDATE(ADATE, WINT,CDATE)
      IF(RDATE.GE.BDATE.AND.RDATE.LT.CDATE) GOTO 10
      ENDDO
      CALL BORT('CENTIME - MISSED SYNOPTIC TIME')

      print*,bdate,adate,cdate

10    IDATE = ADATE
      DHR = DOUR-SOUR

      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE RADDATE(ADATE,DHOUR,BDATE)

      DIMENSION   MON(12)
      REAL(8)     ADATE,BDATE

      DATA MON/31,28,31,30,31,30,31,31,30,31,30,31/

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  ONE WAY OR ANOTHER PARSE A TEN DIGIT DATE INTEGER
!  -------------------------------------------------

      KDATE = NINT(ADATE)
      IDATE = I4DY(KDATE)
      IY = MOD(IDATE/1000000,10000)
      IM = MOD(IDATE/10000  ,100  )
      ID = MOD(IDATE/100    ,100  )
      HR = MOD(ADATE        ,100._8 ) + DHOUR

      IF(MOD(IY,  4).NE.0) MON(2) = 28
      IF(MOD(IY,  4).EQ.0) MON(2) = 29

      IF(MOD(IY,100).EQ.0) MON(2) = 28
      IF(MOD(IY,400).EQ.0) MON(2) = 29


1     IF(HR.LT.0) THEN
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

      BDATE = IY*1000000 + IM*10000 + ID*100
      BDATE = BDATE + HR

      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE BORT(STR)
      CHARACTER*(*) STR
      CALL ERRWRT(' ')
      CALL ERRWRT('***********BUFR ARCHIVE LIBRARY ABORT**************')
      CALL ERRWRT(STR)
      CALL ERRWRT('***********BUFR ARCHIVE LIBRARY ABORT**************')
      CALL ERRWRT(' ')

      call tracebackqq()
      CALL BORT_EXIT
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE BORT2(STR1,STR2)
      CHARACTER*(*) STR1, STR2

      CALL ERRWRT(' ')
      CALL ERRWRT('***********BUFR ARCHIVE LIBRARY ABORT**************')
      CALL ERRWRT(STR1)
      CALL ERRWRT(STR2)
      CALL ERRWRT('***********BUFR ARCHIVE LIBRARY ABORT**************')
      CALL ERRWRT(' ')

      call tracebackqq()
      CALL BORT_EXIT

      END

