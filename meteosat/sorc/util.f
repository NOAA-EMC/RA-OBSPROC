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
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine getatt_c(ncid,idvar,attname,attout)
      use netcdf
      character(*)    attname
      integer         ncid,idvar,len,xtype
      character(*)    attout

      call check( nf90_inquire_attribute(ncid, idvar, attname, len=lens, xtype=xtype) ,"getting "//attname//" typ")
      if(xtype/=nf90_char) call bort('attype not char ')
      if(lens>len(attout)) call bort('attlen too small')
      call check( nf90_get_att(ncid, idvar, attname, attout) ,"getting "//attname//" val")
      !print'(i2,a10,2x,a20)',idvar,attname,attout

      end subroutine getatt_c
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine getatt_i(ncid,idvar,attname,attout)
      use netcdf
      character(*)    attname
      integer         ncid,idvar,len,xtype
      integer         attout

      call check( nf90_inquire_attribute(ncid, idvar, attname, len=lens, xtype=xtype) ,"getting "//attname//" typ")
      if(xtype/=nf90_int)  call bort('attype not int ')
      call check( nf90_get_att(ncid, idvar, attname, attout) ,"getting "//attname//" val")
      !print'(i2,a10,2x,i20)',idvar,attname,attout

      end subroutine getatt_i
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine getatt_r(ncid,idvar,attname,attout)
      use netcdf
      character(*)    attname
      integer         ncid,idvar,len,xtype
      real(4)         attout

      call check( nf90_inquire_attribute(ncid, idvar, attname, len=lens, xtype=xtype) ,"getting "//attname//" typ")
      if(xtype/=nf90_float)  call bort('attype not real')
      call check( nf90_get_att(ncid, idvar, attname, attout) ,"getting "//attname//" val")
      !print'(i2,a10,2x,g20.5)',idvar,attname,attout

      end subroutine getatt_r
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

