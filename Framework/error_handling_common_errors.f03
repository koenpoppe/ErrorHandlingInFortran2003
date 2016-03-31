module error_handling_common_errors

    use error_handling_error
    implicit none
    private
    save
    
    ! Error code + message
    type, extends(error_info), public :: error_code_error
        integer :: error_code
    contains
        procedure :: info_message => error_code_error_info_message
    end type error_code_error
    
    ! IOSTAT
    type, extends(error_code_error), public :: iostat_error
    contains
        procedure :: info_message => iostat_error_info_message
    end type iostat_error
    
    ! - allocation error (includes shape of requested allocation)
    type, extends(error_info), public :: allocation_error
        integer :: error_code
#ifndef FC_NO_ALLOCATABLE_DTCOMP
        integer, dimension(:), allocatable :: requested_shape
#endif
    contains
        procedure :: info_message => allocation_error_info_message
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
    
    subroutine allocation_error_info_message( info, unit, prefix, suffix )
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
    end subroutine allocation_error_info_message
    
    subroutine error_code_error_info_message( info, unit, prefix, suffix )
        class(error_code_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        write(unit=unit,fmt="(2A,I0,2A)") prefix, & 
            "Encountered error code ", info%error_code, ".", suffix
        
    end subroutine error_code_error_info_message
    
    subroutine iostat_error_info_message( info, unit, prefix, suffix )
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
        
    end subroutine iostat_error_info_message

end module error_handling_common_errors
