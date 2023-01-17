!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      PROGRAM dates 

      PARAMETER (MAXSUB=100)

      CHARACTER*200 FILI,FILO
      CHARACTER*8   SUBSET
      CHARACTER*8   SUB(MAXSUB)
      DIMENSION     NINV(3,MAXSUB)
      LOGICAL       FIRST 

      DATA BMISS  /10E10/
      DATA LUNBF  /20/
      DATA LUNot  /50/

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! get the first message date

      read(5,'(i)') idate
      read(5,'(a)') fili
      read(5,'(a)') filo

      idate=i4dy(idate); print*,'file date=',idate

! copy the file with 2 dump date messages

      open(lunbf,file=fili,form='unformatted')
      call openbf(lunbf,'IN',lunbf)

      open(lunot,file=filo,form='unformatted')
      call openbf(lunot,'OUT',lunbf)

      first=.true. 

      do while(ireadmg(lunbf,subset,jdate)==0)
      if(first) then
         call openmg(lunot,subset,idate)
         call openmg(lunot,subset,idate)
         call closmg(lunot)
         first=.false. 
      endif
      if(nmsub(lunbf)>0) call copymg(lunbf,lunot)
      enddo

      call closbf(lunot)

      end program
