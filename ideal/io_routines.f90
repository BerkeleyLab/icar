module io_routines
	use netcdf
	implicit none
	integer,parameter::io_maxDims=10
contains

	subroutine io_getdims(filename,varname,dims)
		implicit none
		character(len=*), intent(in) :: filename,varname
		integer,intent(out) :: dims(:)
		
		integer :: ncid, varid,numDims,dimlen,i
		integer,dimension(io_maxDims) :: dimIds
		
! 		open the netcdf file
		call check(nf90_open(filename, NF90_NOWRITE, ncid))
		! Get the varid of the data_in variable, based on its name.
		call check(nf90_inq_varid(ncid, varname, varid))
		call check(nf90_inquire_variable(ncid, varid, ndims = numDims))
		call check(nf90_inquire_variable(ncid, varid, dimids = dimIds(:numDims)))
		dims(1)=numDims
		do i=1,numDims
			call check(nf90_inquire_dimension(ncid, dimIds(i), len = dimlen))
			dims(i+1)=dimlen
		end do
	end subroutine io_getdims
	
	subroutine io_read3d(filename,varname,data_in,extradim)
		implicit none
	    ! This is the name of the data_in file and variable we will read. 
		character(len=*), intent(in) :: filename, varname
		real,intent(out),allocatable :: data_in(:,:,:)
		integer, intent(in),optional :: extradim
		integer, dimension(io_maxDims)  :: diminfo !will hold dimension lengths
		integer, dimension(io_maxDims)  :: dimstart
		! This will be the netCDF ID for the file and data_in variable.
		integer :: ncid, varid,i
		
		if (present(extradim)) then
			dimstart=extradim
			dimstart(1:3)=1
		else
			dimstart=1
		endif
		
! 		Read the dimension lengths
		call io_getdims(filename,varname,diminfo)
		allocate(data_in(diminfo(2),diminfo(3),diminfo(4)))
		! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
		! the file.
		call check(nf90_open(filename, NF90_NOWRITE, ncid))
		! Get the varid of the data_in variable, based on its name.
		call check(nf90_inq_varid(ncid, varname, varid))
		
		! Read the data_in. skip the slowest varying indices if there are more than 3 dimensions (typically this will be time)
		if (diminfo(1)>3) then
			diminfo(5:diminfo(1)+1)=1 ! set count for extra dims to 1
			call check(nf90_get_var(ncid, varid, data_in,&
									dimstart(1:diminfo(1)), &				! start  = 1 or extradim
									[ (diminfo(i+1), i=1,diminfo(1)) ],&		! count=n or 1
									[ (1,           i=1,diminfo(1)) ] ))	! for all dims, stride = 1
		else		
			call check(nf90_get_var(ncid, varid, data_in))
		endif
!         function nf90_get_var(ncid, varid, values, start, stride, map)
!           integer,                         intent( in) :: ncid, varid
!           any valid type, scalar or array of any rank, &
!                                            intent(out) :: values
!           integer, dimension(:), optional, intent( in) :: start, count, stride, map
!           integer                                      :: nf90_get_var
	
		! Close the file, freeing all resources.
		call check( nf90_close(ncid) )
		
	end subroutine io_read3d

	subroutine io_read2d(filename,varname,data_in,extradim)
		implicit none
	    ! This is the name of the data_in file and variable we will read. 
		character(len=*), intent(in) :: filename, varname
		real,intent(out),allocatable :: data_in(:,:)
		integer, intent(in),optional :: extradim
		integer, dimension(io_maxDims)  :: diminfo ! will hold dimension lengths
		integer, dimension(io_maxDims)  :: dimstart
		! This will be the netCDF ID for the file and data_in variable.
		integer :: ncid, varid,i

		if (present(extradim)) then
			dimstart=extradim
			dimstart(1:2)=1
		else
			dimstart=1
		endif
		
! 		Read the dimension lengths
		call io_getdims(filename,varname,diminfo)
		print*, filename,varname,diminfo(1:diminfo(1)+1)
		allocate(data_in(diminfo(2),diminfo(3)))
		! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
		! the file.
		call check(nf90_open(filename, NF90_NOWRITE, ncid))
		! Get the varid of the data_in variable, based on its name.
		call check(nf90_inq_varid(ncid, varname, varid))
		
		! Read the data_in. skip the slowest varying indices if there are more than 3 dimensions (typically this will be time)
		if (diminfo(1)>2) then
			diminfo(4:diminfo(1)+1)=1 ! set count for extra dims to 1
			call check(nf90_get_var(ncid, varid, data_in,&
									dimstart(1:diminfo(1)), &				! start  = 1 or extradim
									[ (diminfo(i+1), i=1,diminfo(1)) ],&		! count=n or 1
									[ (1,           i=1,diminfo(1)) ] ))	! for all dims, stride = 1
		else		
			call check(nf90_get_var(ncid, varid, data_in))
		endif
! 		! Read the data_in.
! 		call check(nf90_get_var(ncid, varid, data_in))
	
		! Close the file, freeing all resources.
		call check( nf90_close(ncid) )
		
	end subroutine io_read2d

	subroutine io_write3d(filename,varname,data_out)
		implicit none
	    ! This is the name of the data file and variable we will read. 
		character(len=*), intent(in) :: filename, varname
		real,intent(in) :: data_out(:,:,:)
		
		! We are reading 2D data, a nx x ny grid. 
		integer :: nx,ny,nz
		integer, parameter :: ndims = 3
		! This will be the netCDF ID for the file and data variable.
		integer :: ncid, varid,temp_dimid,dimids(ndims)

		nx=size(data_out,1)
		nz=size(data_out,2)
		ny=size(data_out,3)
		
		! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
		! the file.
		call check( nf90_create(filename, NF90_CLOBBER, ncid) )
! 		define the dimensions
		call check( nf90_def_dim(ncid, "x", nx, temp_dimid) )
		dimids(1)=temp_dimid
		call check( nf90_def_dim(ncid, "z", nz, temp_dimid) )
		dimids(2)=temp_dimid
		call check( nf90_def_dim(ncid, "y", ny, temp_dimid) )
		dimids(3)=temp_dimid
		
		! Create the variable returns varid of the data variable
		call check( nf90_def_var(ncid, varname, NF90_REAL, dimids, varid) )
		! End define mode. This tells netCDF we are done defining metadata.
		call check( nf90_enddef(ncid) )
		
		call check( nf90_put_var(ncid, varid, data_out) )
	
		! Close the file, freeing all resources.
		call check( nf90_close(ncid) )
	end subroutine io_write3d

	subroutine io_write3di(filename,varname,data_out)
		implicit none
	    ! This is the name of the data file and variable we will read. 
		character(len=*), intent(in) :: filename, varname
		integer,intent(in) :: data_out(:,:,:)
		
		! We are reading 2D data, a nx x ny grid. 
		integer :: nx,ny,nz
		integer, parameter :: ndims = 3
		! This will be the netCDF ID for the file and data variable.
		integer :: ncid, varid,temp_dimid,dimids(ndims)

		nx=size(data_out,1)
		nz=size(data_out,2)
		ny=size(data_out,3)
		
		! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
		! the file.
		call check( nf90_create(filename, NF90_CLOBBER, ncid) )
! 		define the dimensions
		call check( nf90_def_dim(ncid, "x", nx, temp_dimid) )
		dimids(1)=temp_dimid
		call check( nf90_def_dim(ncid, "z", nz, temp_dimid) )
		dimids(2)=temp_dimid
		call check( nf90_def_dim(ncid, "y", ny, temp_dimid) )
		dimids(3)=temp_dimid
		
		! Create the variable returns varid of the data variable
		call check( nf90_def_var(ncid, varname, NF90_INT, dimids, varid) )
		! End define mode. This tells netCDF we are done defining metadata.
		call check( nf90_enddef(ncid) )
		
		call check( nf90_put_var(ncid, varid, data_out) )
	
		! Close the file, freeing all resources.
		call check( nf90_close(ncid) )
	end subroutine io_write3di


	subroutine io_write2d(filename,varname,data_out)
		implicit none
	    ! This is the name of the data file and variable we will read. 
		character(len=*), intent(in) :: filename, varname
		real,intent(in) :: data_out(:,:)
		
		! We are reading 2D data, a nx x ny grid. 
		integer :: nx,ny
		integer, parameter :: ndims = 2
		! This will be the netCDF ID for the file and data variable.
		integer :: ncid, varid,temp_dimid,dimids(ndims)

		nx=size(data_out,1)
		ny=size(data_out,2)
		
		! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
		! the file.
		call check( nf90_create(filename, NF90_CLOBBER, ncid) )
! 		define the dimensions
		call check( nf90_def_dim(ncid, "x", nx, temp_dimid) )
		dimids(1)=temp_dimid
		call check( nf90_def_dim(ncid, "y", ny, temp_dimid) )
		dimids(2)=temp_dimid
		
		! Create the variable returns varid of the data variable
		call check( nf90_def_var(ncid, varname, NF90_REAL, dimids, varid) )
		! End define mode. This tells netCDF we are done defining metadata.
		call check( nf90_enddef(ncid) )
		
		call check( nf90_put_var(ncid, varid, data_out) )
	
		! Close the file, freeing all resources.
		call check( nf90_close(ncid) )
	end subroutine io_write2d
	
	subroutine check(status)
		integer, intent ( in) :: status
    
		if(status /= nf90_noerr) then 
			print *, trim(nf90_strerror(status))
			stop "Stopped"
		end if
	end subroutine check  
	
	! This is a simple function to search for an available unit.
	! LUN_MIN and LUN_MAX define the range of possible LUNs to check.
	! The UNIT value is returned by the function, and also by the optional
	! argument. This allows the function to be used directly in an OPEN
	! statement, and optionally save the result in a local variable.
	! If no units are available, -1 is returned.
	integer function io_newunit(unit)
		implicit none
		integer, intent(out), optional :: unit
		! local
		integer, parameter :: LUN_MIN=10, LUN_MAX=1000
		logical :: opened
		integer :: lun
		! begin
		io_newunit=-1
		do lun=LUN_MIN,LUN_MAX
			inquire(unit=lun,opened=opened)
			if (.not. opened) then
				io_newunit=lun
				exit
			end if
		end do
		if (present(unit)) unit=io_newunit
	end function io_newunit
	
end module io_routines