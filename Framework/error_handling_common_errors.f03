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
    
    subroutine allocation_error_info_message( info, unit, prefix, suffix )
        class(allocation_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        if( size(info%requested_shape)  == 1 ) then
            write(unit=unit,fmt="(2A,I0,A,I0,2A)") prefix, &
                "Allocating array with ", info%requested_shape, & 
                " elements did not work (error code ", info%error_code, ").", suffix
        else
            write(unit=unit,fmt=*) prefix, "Allocating array of shape ", info%requested_shape, & 
                " (=", product(info%requested_shape), & 
                " elements) did not work (error code ", info%error_code, ").", suffix
        end if
    end subroutine allocation_error_info_message
    ! TODO: allocate wrapper for all types ...
    
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
