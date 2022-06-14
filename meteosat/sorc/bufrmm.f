!> @file
!> @brief Read BUFR file containing embedded DX BUFR tables,
!> and print each report one at a time

  program bufrmm 

  implicit none

  character(255)     :: file        !> name of filename to read
  character(8)       :: subset,nemo 
  integer, parameter :: lunit = 20
  integer, parameter :: mxarr = 1000000
  integer            :: ntab,n
  logical            :: exist
  real(8)            :: arr(mxarr),armin,armax

! get the filename to open and read

  call getarg(1,file); file=trim(adjustl(file)) 
  if (file == '') call bort('Usage: "bufrmm bufrfile nemo" will print the min/max vals of element nemo')
  inquire(file=file,exist=exist)
  if (.not.exist) call bort(file//' does not exist') 
  call getarg(2,nemo)

! collect values of nemo and find the mion and max of the array

  open(lunit,file=file,form='unformatted')
  call ufbtab(lunit,arr,1,mxarr,ntab,nemo)

  armin=+10d10
  armax=-10d10
  do n=1,ntab
  armin=min(armin,arr(n))
  armax=max(armax,arr(n))
  enddo

  print*
  print*,trim(nemo),' min=',armin,' max=',armax
  print*

  end program
