!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE REDATE(ADATE,DHOUR,BDATE)
 
      DIMENSION   MON(12)
      REAL(8)  :: ADATEi(6),bdate(6),dhour
 
      DATA MON/31,28,31,30,31,30,31,31,30,31,30,31/
 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 
!  ONE WAY OR ANOTHER PARSE A TEN DIGIT DATE INTEGER
!  -------------------------------------------------
 
      IY = nint(adate(1)) 
      IM = nint(adate(2)) 
      ID = nint(adate(3))
      HR = nint(adate(4))+dhour+date(5)/60.+date(6)/3600.

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
      bdate(4) = int(hr)
      bdate(5) = int(hr*60.)
      bdate(6) = (hr*60.-bdate(5))*60.

 
      RETURN
      END
