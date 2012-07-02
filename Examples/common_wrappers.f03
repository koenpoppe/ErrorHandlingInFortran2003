! COMMON_WRAPPERS
! 
!   Tests for the common wrappers in the framework
! 
! REFERENCES
! 
!   
! 
! HISTORY
! 
!   20120702 KP - Initial version
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
!
module common_wrappers
	use error_handling
	implicit none
	private
	save
	
	public :: test_allocation
	
	type my_type
		integer, dimension(:), allocatable :: array_sz
		integer, dimension(:), allocatable :: array_lu
	end type my_type

contains

	subroutine test_allocation()
		integer, dimension(:), allocatable :: array_sz
		integer, dimension(:), allocatable :: array_lu
		integer, parameter :: lower = 0, upper = 5, nb = upper-lower+1
		type(my_type) :: type
		character(len=40), parameter :: fmt="(A15,' (',I0,':',I0,')')"
	
		write(unit=*,fmt="(A,(I0,A))") "- Allocate array(1:", nb , "):"
		call allocate( array_sz, nb )
		write(unit=*,fmt=fmt) "array_sz", lbound(array_sz), ubound(array_sz)
		call allocate( type%array_sz, nb )
		write(unit=*,fmt=fmt) "type%array_sz", lbound(type%array_sz), ubound(type%array_sz)

		write(unit=*,fmt="(A,2(I0,A))") "- Allocate array(", lower, ":", upper, "):"		
		call allocate( array_lu, lower, upper )
		write(unit=*,fmt=fmt) "array_lu", lbound(array_lu), ubound(array_lu)
		call allocate( type%array_lu, lower, upper )
		write(unit=*,fmt=fmt) "type%array_lu", lbound(type%array_lu), ubound(type%array_lu)
		
	end subroutine test_allocation

end module common_wrappers
