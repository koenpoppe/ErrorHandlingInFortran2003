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
        integer, dimension(:), allocatable :: requested_shape
    contains
        procedure :: info_message => allocation_error_info_message
    end type allocation_error
    
contains
    
    subroutine allocation_error_info_message( info, message )
        class(allocation_error), intent(in) :: info
        character(len=*), intent(out) :: message
        if( size(info%requested_shape) == 1 ) then
            write(unit=message,fmt="(A,I0,A,I0,A)") "Allocating array with ", info%requested_shape, & 
                " elements did not work (error code ", info%error_code, ")."
        else
            write(unit=message,fmt=*) "Allocating array of shape ", info%requested_shape, & 
                " (=", product(info%requested_shape), & 
                " elements) did not work (error code ", info%error_code, ")."
        end if
    end subroutine allocation_error_info_message
    ! TODO: allocate wrapper for all types ...
    
    subroutine error_code_error_info_message( info, message )
        class(error_code_error), intent(in) :: info
        character(len=*), intent(out) :: message
        
        write(unit=message,fmt="(A,I0,A)") & 
            "Encountered error code ", info%error_code, "."
        
    end subroutine error_code_error_info_message
    
    subroutine iostat_error_info_message( info, message )
        use ISO_FORTRAN_ENV, only: IOSTAT_END, IOSTAT_EOR
    
        class(iostat_error), intent(in) :: info
        character(len=*), intent(out) :: message
        
        select case( info%error_code )
            case(IOSTAT_END)
                message = "End-of-file condition"
            case(IOSTAT_EOR)
                message = "End-of-record condition"
            case default
                write(unit=message,fmt="(A,I0,A)") "Processor dependent code ", info%error_code, "."
        end select
        
    end subroutine iostat_error_info_message

end module error_handling_common_errors
