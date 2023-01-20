!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      program prep_erscat

      character(80) erstr,dastr,filename
      character(10) adate
      character(8)  subset,subout,cpid /'ERSWIND'/
      equivalence   (cpid,rpid)

      real(8) rdate,rswv,date(6),arr(15),wndv(2,4),rpid

      data subout/'NC012008'/
      data lunin /20/

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! read through  the bufr input file
! ---------------------------------

      call openbf(lunin,'IN',lunin)

      do while(ireadmg(lunin,subset,idate)==0)
      do while(ireadsb(lunin)==0)

! check to see if a valid report
! ------------------------------

      call ufbint(lunin,rswv,1,1,iret,'WSPC2')
      if(rswv>0) cycle
      iswv=rswv

! read the date/time and selected wind vector
! -------------------------------------------

      dastr='YEAR MNTH DAYS HOUR MINU SECO'
      call ufbint(lunin,date,6,1,iret,dastr)
      iyr=nint(date(1))
      imn=nint(date(2))
      idy=nint(date(3))
      ihr=nint(date(4))
      imi=nint(date(5))
      isc=nint(date(6))
      rdate=iyr*1000000+imn*10000+idy*100+ihr
      rdate=rdate+float(imi*60+isc)/3600.

      call ufbint(lunin,wndv,2,4,iret,'WS10 WD10')

! target an output file for this report
! -------------------------------------

      hour=0; hint=6
      call centime(rdate,hour,hint,idate,dhr)
      write(adate,'(I10)') i4dy(idate)
      filename='erscat.ncep.'//adate
      call outfile(filename,lunot,iret)
      if(iret==1) then
         call openmg(lunot,subout,idate)
         call openmg(lunot,subout,idate)
         call closmg(lunot)
      endif

      call openmb(lunot,subout,idate)

! copy the report into erscat format
! ----------------------------------

      erstr='SAID YEAR MNTH DAYS HOUR MINU SECO CLAT CLON WS10 WD10'
      call ufbint(lunin,arr,15,1,iret,erstr)
      arr(12)=rpid
      erstr='SAID YEAR MNTH DAYS HOUR MINU SECO CLAT CLON WS10 WD10'
      call ufbint(lunot,arr,15,1,iret,trim(erstr)//' RPID')

! write a compressed subset
! -------------------------

      call writsb(lunot)

      enddo
      enddo

! close up and finish
! -------------------

      call closeout
      end  program

