!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      program sinvsplt

      character(250)     ::  filein,fileot
      character(8)       ::  subset
      integer, parameter ::  maxsub=100
      integer            ::  sub(maxsub)
      integer            ::  lunbf=20
      real(8)            ::  sinv 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!  get input filename argument

      NARG=IARGC()
      IF(NARG/=1) THEN
        PRINT *
        PRINT *,'Usage: sinvsplt <bufrfile> will split a bufr file into different sat platforms'
        PRINT *
        CALL EXIT(2)
      ENDIF
      call getarg(1,filein)
      filein = TRIM(filein)
      open(lunbf,file=filein,form='unformatted')

!  open and read through the satellite input file 
 
      call openbf(lunbf,'IN',lunbf); call maxout(20000)

      do while(ireadmg(lunbf,subset,idate)==0)
      do while(ireadsb(lunbf)==0)

      call ufbint(lunbf,sinv,1,1,iret,'SAID')
      if(sinv<10d10) then
         isin=nint(sinv)
      else
         isin=0
      endif

!  identify which file is open for this sinv

      ISUB = 0
      DO I=1,NSUB
      IF(isin==sub(I)) ISUB = I
      ENDDO
      IF(ISUB.EQ.0) THEN
         IF(NSUB+1.GT.MAXSUB) CALL BORT('NSUB TOO BIG')
         SUB(NSUB+1) = isin      
         NSUB = NSUB+1
         ISUB = NSUB
         write(fileot,'(i3.3,".sinv")') isin
         open(50+isub,file=fileot,form='unformatted')
         call openbf(50+isub,'OUT',lunbf)
      ENDIF

!  copy this subset into the sinv file

      lunot=isub+50

      call openmb(50+isub,subset,idate)
      call ufbcpy(lunbf,lunot)
      call writsb(lunot)

      enddo
      enddo

!  close the output files to flush the buffers

      do i=1,nsub
      call closbf(50+i)
      enddo

end program sinvsplt
