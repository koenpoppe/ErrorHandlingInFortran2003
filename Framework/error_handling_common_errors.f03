module error_handling_common_errors

    use error_handling_error
    implicit none
    private
    save
    
    ! Error code + message
    type, extends(error_info), public :: error_code_error
        integer :: error_code
    contains
        procedure :: write_to => error_code_error_write_to
    end type error_code_error
    
    ! IOSTAT
    type, extends(error_code_error), public :: iostat_error
    contains
        procedure :: write_to => iostat_error_write_to
    end type iostat_error
    
    ! - allocation error (includes shape of requested allocation)
    type, extends(error_info), public :: allocation_error
        integer :: error_code
#ifndef FC_NO_ALLOCATABLE_DTCOMP
        integer, dimension(:), allocatable :: requested_shape
#endif
    contains
        procedure :: write_to => allocation_error_write_to
    end type allocation_error
#ifdef FC_NO_ALLOCATABLE_DTCOMP
#ifdef FC_NO_DT_CONSTRUCTOR
	public :: allocation_error_constructor
#else
    interface allocation_error
        module procedure allocation_error_constructor
    end interface allocation_error
#endif
    integer, dimension(:), private, allocatable :: allocation_error_requested_shape
#endif

	! - enumeration error
	type, extends(error_info), public :: enumeration_error
		integer :: enum
#ifndef FC_NO_ALLOCATABLE_DTCOMP
		integer, dimension(:), allocatable :: enums
		character(len=40), dimension(:), allocatable :: enum_descriptions
		character(:), allocatable :: message
#endif
	contains
		procedure :: write_to => enum_descriptions_write_to
	end type enumeration_error
    
contains
    
#ifdef FC_NO_ALLOCATABLE_DTCOMP
    function allocation_error_constructor( error_code, requested_shape ) result( info )
        integer, intent(in) :: error_code
        integer, dimension(:), intent(in) :: requested_shape
        type(allocation_error) :: info
        
        info%error_code = error_code
        allocation_error_requested_shape = requested_shape
        
    end function allocation_error_constructor
#endif
    
    subroutine allocation_error_write_to( info, unit, prefix, suffix )
        class(allocation_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        ! TODO: remove workarround for allocatable derived type components
#ifndef FC_NO_ALLOCATABLE_DTCOMP
        associate( requested_shape => info%requested_shape )
#else
        associate( requested_shape => allocation_error_requested_shape )
#endif
            if( size(requested_shape)  == 1 ) then
                write(unit=unit,fmt="(2A,I0,A,I0,2A)") prefix, &
                    "Allocating array with ", requested_shape, & 
                    " elements did not work (error code ", info%error_code, ").", suffix
            else
                write(unit=unit,fmt=*) prefix, "Allocating array of shape ", requested_shape, & 
                    " (=", product(requested_shape), & 
                    " elements) did not work (error code ", info%error_code, ").", suffix
            end if
        end associate
    end subroutine allocation_error_write_to
    
    subroutine error_code_error_write_to( info, unit, prefix, suffix )
        class(error_code_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        write(unit=unit,fmt="(2A,I0,2A)") prefix, & 
            "Encountered error code ", info%error_code, ".", suffix
        
    end subroutine error_code_error_write_to
    
    subroutine iostat_error_write_to( info, unit, prefix, suffix )
        use ISO_FORTRAN_ENV, only: IOSTAT_END, IOSTAT_EOR
        
        class(iostat_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        select case( info%error_code )
            case(IOSTAT_END)
                write(unit=unit,fmt="(3A)") prefix, &
                    "End-of-file condition", suffix
            case(IOSTAT_EOR)
                write(unit=unit,fmt="(3A)") prefix, &
                    "End-of-record condition", suffix
            case default
                write(unit=unit,fmt="(2A,I0,2A)") prefix, &
                    "Processor dependent code ", info%error_code, ".", suffix
        end select
        
    end subroutine iostat_error_write_to

	subroutine enum_descriptions_write_to( info, unit, prefix, suffix )
	    class(enumeration_error), intent(in) :: info
	    integer, intent(in) :: unit
	    character(len=*), intent(in) :: prefix, suffix
		
		integer :: i
		logical :: show_default_message
		character(len=20) :: fmt
		
		write(unit=unit,fmt="(2A,I0,2A)",advance="no") prefix, "Unexpected enumeration value ", info%enum, ", "
		show_default_message = .true.
		if( allocated(info%message) ) then
			show_default_message = len_trim(info%message) > 0
		end if
		if( show_default_message ) then
			write(unit=unit,fmt="(A)",advance="no") "expecting one of the following"
		else
			write(unit=unit,fmt="(A)",advance="no") trim(info%message)
		end if
		write(unit=unit,fmt="(2A)") ": ", suffix
		write(unit=fmt,fmt="(A,I0,A,I0,A)") "(2A,A", maxval(len_trim(info%enum_descriptions)), & 
			",A,I", 1+ceiling(log10(real(maxval(info%enums)))), ",A)"
		do i=1,size(info%enums)
			write(unit=unit,fmt=fmt,advance="no") prefix, "- ", info%enum_descriptions(i), " (=", info%enums(i), ")", suffix
		end do
		
	end subroutine enum_descriptions_write_to
	
end module error_handling_common_errors
