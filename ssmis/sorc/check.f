!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
 subroutine check(status)

 use netcdf

 integer, intent(in) :: status

 if(status /= NF90_NOERR) then
    write (*,*) NF90_STRERROR(status)
    call tracebackqq()
    stop
 end if

 end subroutine check
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
 subroutine getdim(ncid,dimname,dim)
 use netcdf
 character(*)  dimname
 integer       dimid,dim  
 call check( nf90_inq_dimid(ncid,dimname,dimid))         
 call check( nf90_inquire_dimension(ncid,dimid,len=dim))
 end subroutine getdim
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

