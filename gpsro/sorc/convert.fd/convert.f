C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      PROGRAM convert 
 
      PARAMETER(I1=10000)
      CHARACTER(200) FILENAME
      CHARACTER(80)  DASTR  
      CHARACTER(10)  ADATE  
      CHARACTER(8)   SUBSET,satid
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
      do while(ireadsb(LUNIN)==0)

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
      filename='gpsro.'//trim(satid)//'.'//trim(adate)
      call outfile(filename,lunbo,iret)

      ! copy the profile into the synoptic file
      call copymg(lunin,lunbo)
      !call openmb(lunbo,'NC003010',idate)
      !call ufbseq(lunin,arr,i1,1,iret,'RAOCSEQ')
      !call ufbseq(lunbo,arr,i1,1,iret,'RAOCSEQ')
      !call writsb(lunbo)
      enddo
      enddo

      call closbf(lunbo)
      stop
      end

