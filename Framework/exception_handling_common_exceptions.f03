module exception_handling_common_exceptions

    use exception_handling_exception
    implicit none
    private
    save
    
    ! Error code + message
    type, extends(exception_info), public :: error_code_exception
        integer :: error_code
    contains
        procedure :: info_message => error_code_exception_info_message
    end type error_code_exception
    
    ! IOSTAT
    type, extends(error_code_exception), public :: iostat_exception
    contains
        procedure :: info_message => iostat_exception_info_message
    end type iostat_exception
    
    ! - allocation exception (includes shape of requested allocation)
    type, extends(exception_info), public :: allocation_exception
        integer :: error_code
        integer, dimension(:), allocatable :: requested_shape
    contains
        procedure :: info_message => allocation_exception_info_message
    end type allocation_exception
    
contains
    
    subroutine allocation_exception_info_message( info, message )
        class(allocation_exception), intent(in) :: info
        character(len=*), intent(out) :: message
        if( size(info%requested_shape) == 1 ) then
            write(unit=message,fmt="(A,I0,A,I0,A)") "Allocating array with ", info%requested_shape, & 
                " elements did not work (error code ", info%error_code, ")."
        else
            write(unit=message,fmt=*) "Allocating array of shape ", info%requested_shape, & 
                " (=", product(info%requested_shape), & 
                " elements) did not work (error code ", info%error_code, ")."
        end if
    end subroutine allocation_exception_info_message
    ! TODO: allocate wrapper for all types ...
    
    subroutine error_code_exception_info_message( info, message )
        class(error_code_exception), intent(in) :: info
        character(len=*), intent(out) :: message
        
        write(unit=message,fmt="(A,I0,A)") & 
            "Encountered error code ", info%error_code, "."
        
    end subroutine error_code_exception_info_message
    
    subroutine iostat_exception_info_message( info, message )
        use ISO_FORTRAN_ENV, only: IOSTAT_END, IOSTAT_EOR
    
        class(iostat_exception), intent(in) :: info
        character(len=*), intent(out) :: message
        
        select case( info%error_code )
            case(IOSTAT_END)
                message = "End-of-file condition"
            case(IOSTAT_EOR)
                message = "End-of-record condition"
            case default
                write(unit=message,fmt="(A,I0,A)") "Processor dependent code ", info%error_code, "."
        end select
        
    end subroutine iostat_exception_info_message

end module exception_handling_common_exceptions
