!(doc)TITLE{Exception handling}
!(doc)SUBTITLE{Advanced exception handling in Fortran}
!(doc)VERSION{0.1}
!(doc)AUTHOR{Koen Poppe}
!(doc)DATE{2011-01-27}
!(doc)header

! 20110329 KP - TODO: split into seperate files and build as library

module exception_handling_exception

    use ISO_FORTRAN_ENV, only: OUTPUT_UNIT
    ! use ISO_FORTRAN_ENV, only: ERROR_UNIT
    implicit none
    private
    save

    !--------------------------------------------------------------------------
    ! Derived types
    !--------------------------------------------------------------------------
    
    ! 1. Exceptions itself
    type, public :: exception
        class(exception_info), pointer :: info => NULL()
        character(:), allocatable :: method
        type(exception), pointer :: reason => NULL()
        logical :: handled = .false.
    contains
        final :: exception_final
    end type exception
    
    ! 2. Abstract information type
    type, public :: exception_info ! TODO: abstract
    contains
        procedure, pass :: info_message => exception_info_info_message
    end type exception_info

    ! 3. Sentinel no-exception information type
    type, extends(exception_info), public :: no_exception
    contains
        procedure, pass :: info_message => no_exception_info_message
    end type no_exception
    
    ! 4. Message exception information type
    type, extends(exception_info), public :: message_exception
        character(:), allocatable :: message
    contains
        procedure :: info_message => message_exception_info_message
    end type message_exception
    
    !--------------------------------------------------------------------------
    ! Operations for exceptions
    !--------------------------------------------------------------------------
    
    ! 1. Creation/chaining
    public :: create_exception
    interface create_exception
        module procedure create_exception_ifail_exception
        module procedure create_exception_ifail_exception_reason 
        module procedure create_exception_ifail_exception_method
        module procedure create_exception_full
    end interface create_exception

    ! 2. Transfering
    public :: transfer_exception

    ! 3. Reporting
    public :: report_exception
    public :: report_exception_unit, set_report_exception_unit
    integer, private :: REPORT_UNIT = OUTPUT_UNIT ! or ERROR_UNIT
    
    ! 4. Discarding
    public :: discard_exception
    
    ! 5. Assignment
    public :: exception_assignment_safe
    
    ! 6. Comparing
    public :: operator(.EQ.)
    interface operator(.EQ.)
        module procedure exception_eq_exception
        module procedure exception_info_eq_exception_info
    end interface
    
    public :: operator(.NE.)
    interface operator(.NE.)
        module procedure exception_neq_exception
        module procedure exception_info_neq_exception_info
    end interface
    
contains

    !--------------------------------------------------------------------------
    ! Derived types
    !--------------------------------------------------------------------------
    
    ! 1. Exceptions itself
    
    recursive subroutine exception_final( exc )
        type(exception),intent(in out) :: exc
        
        if( .not. exc%handled .and. is_exception(exc) ) then
            call exception_final_unhandled( exc )
        end if
        
    end subroutine exception_final
    
    recursive subroutine exception_final_unhandled( exc )
        type(exception),intent(in out) :: exc
        type(exception) :: inform
        
        call create_exception( inform, message_exception("Exceptions must not be ignored, the following one was ..."), exc, &
            "exception_handling:exception_final_unhandled" )
        call report_exception( inform, fatal=.true. )
        
    end subroutine exception_final_unhandled
    
    ! 2. Abstract information type
    subroutine exception_info_info_message( info, message )
        class(exception_info), intent(in) :: info
        character(len=*), intent(out) :: message
        
        select type(info)
            type is(exception_info)
                message = "No specific information available ..."
            class default
                message = "(override the 'info_message' type bound procedure to provide a meaningfull messages)"
        end select
        
    end subroutine exception_info_info_message
    
    subroutine message_exception_info_message( info, message )
        class(message_exception), intent(inout) ::  info
        character(len=*), intent(out) :: message

        if( allocated(info%message) ) then
           message = info%message
        else
           message = "BUG? - not allocated -"
        end if

    end subroutine message_exception_info_message
    
    ! 3. Sentinel no-exception information type
    subroutine no_exception_info_message( info, message )
        class(no_exception), intent(in) :: info
        character(len=*), intent(out) :: message
        
        write(unit=message,fmt="(A,I0)") "INTERNAL ERROR: no exception here."
        
    end subroutine no_exception_info_message
    
    !--------------------------------------------------------------------------
    ! Operations for exceptions
    !--------------------------------------------------------------------------
    
    ! 1. Creation/chaining
    
    pure function empty_exception() result( exc )
        type(exception) :: exc
        exc%handled = .true. ! TODO: This is an empty exception
    end function empty_exception
    
    function is_exception( exc ) result( is_exc )
        class(exception), intent(in) :: exc
        logical :: is_exc
        
        if( associated( exc%info ) ) then
            select type( E => exc%info ) 
                type is( no_exception )
                    is_exc = .false.
                class default
                    is_exc = .true.
            end select
        else
            ! print *, "INTERNAL ERROR: not associated " ! TODO ensure
            is_exc = .false.
        end if
        
    end function is_exception
    
    subroutine exception_constructor( ifail, info, reason, method )

        ! Arguments
        type(exception), intent( out ) :: ifail
        class(exception_info), target, intent(in) :: info
        type(exception), target, intent(in out), optional :: reason
        character(len=*), intent(in), optional :: method
        
        ! Prepare the exception
        allocate( ifail%info, source=info )
        
        if( present(method) ) then
            if( len_trim(method) > 0 ) then
                ifail%method = method
            end if
        end if

        if( present(reason) ) then
            allocate( ifail%reason ) ! TODO: check allocation
            call transfer_exception( reason, ifail%reason )
        else
            nullify( ifail%reason )
        end if
        
        ifail%handled = .false.
        
    end subroutine exception_constructor
    
    subroutine create_exception_full( ifail, info, reason, method )
        
        ! Arguments
        type(exception), intent(out), optional :: ifail
        class(exception_info), target, intent(in) :: info
        type(exception), target, intent(in out) :: reason
        character(len=*), intent(in) :: method
        
        type(exception) :: inform
        
        call exception_constructor( inform, info, reason, method )
        call transfer_exception( inform, ifail )
        
    end subroutine create_exception_full

    subroutine create_exception_ifail_exception( ifail, info )
        
        ! Arguments
        type(exception), intent(out), optional ::  ifail
        class(exception_info), intent(in) :: info
        
        type(exception) :: empty
        empty = empty_exception()
        call create_exception_full( ifail, info, empty, "" )
        
    end subroutine create_exception_ifail_exception
    
    subroutine create_exception_ifail_exception_method( ifail, info, method )
        
        ! Arguments
        type(exception), intent(out), optional ::  ifail
        class(exception_info), intent(in) :: info
        character(len=*), intent(in) :: method
    
        type(exception) :: empty        
        empty = empty_exception()
        call create_exception_full( ifail, info, empty, method )
        
    end subroutine create_exception_ifail_exception_method
    
    subroutine create_exception_ifail_exception_reason( ifail, info, reason )
        
        ! Arguments
        type(exception), intent(out), optional ::  ifail
        class(exception_info), intent(in) :: info
        type(exception), intent(in out) :: reason
        
        call create_exception_full( ifail, info, reason, "" )
        
    end subroutine create_exception_ifail_exception_reason

    ! 2. Transfering
    recursive subroutine transfer_exception( inform, ifail ) ! ifail = inform
        type(exception), intent(in out) :: inform
        type(exception), intent(in out), optional :: ifail
        
        if( present(ifail) ) then
            call exception_assignment_safe( ifail, inform )
            call discard_exception( inform ) ! discard
        else
            call report_exception( inform, fatal=.true. )
        end if
        
    end subroutine transfer_exception

    ! 3. Reporting
    ! TODO: unit
    subroutine report_exception( exc, fatal )
        type(exception), intent(in out) :: exc
        logical, intent(in), optional :: fatal
        if( .not. is_exception(exc) ) return
        call write_exception_trace( exc, 0 )
        call discard_exception( exc )
        if( present(fatal) ) then
            if( fatal ) then
                write(unit=REPORT_UNIT,fmt="(A)") "FATAL -> STOP"
                stop
            end if
        end if
    end subroutine report_exception
    
    function report_exception_unit() result( unit )
        integer :: unit
        unit = REPORT_UNIT
    end function report_exception_unit
    
    subroutine set_report_exception_unit( unit )
        integer, intent(in) :: unit
        REPORT_UNIT = unit
    end subroutine set_report_exception_unit
    
    ! Format the full exception trace (Top down)
    recursive subroutine write_exception_trace( exc, level )
    
        ! Arguments
        type(exception), intent(in) :: exc
        integer, intent(in) :: level
        
        ! Local variables
        character(len=200) :: message
        
        if( level == 0 ) then
            write(unit=REPORT_UNIT,fmt="(A)",advance="no") "*** Exception"
        else
            write(unit=REPORT_UNIT,fmt="(A)",advance="no") " ** cascading"
        end if
        
        if( allocated(exc%method )) &
            write(unit=REPORT_UNIT,fmt="(3A)",advance="no") " from '", exc%method, "'"
        write(unit=REPORT_UNIT,fmt="(A)") ":"
        
        if( associated(exc%info) ) then
            message = ""
            call exc%info%info_message(message)
            write(unit=REPORT_UNIT,fmt="(2A)") "       ", trim(adjustl(message))
        end if
        
        if( associated(exc%reason) ) then
            if( exc%reason /= empty_exception() ) then
                call write_exception_trace( exc%reason, level+1 )
            end if
        end if
        
    end subroutine write_exception_trace
    
    ! 4. Discarding
    subroutine discard_exception( ifail )
        
        ! Arguments
        type(exception), intent(in out) :: ifail
        
        ! Discard the error
        ifail%handled = .true.
        
    end subroutine discard_exception
    
    ! 5. Assignment
    recursive subroutine exception_assignment_safe( ifail, inform ) ! ifail = inform
        class(exception), intent(out) :: ifail
        class(exception), intent(in out) :: inform
        
        if( associated(inform%info) ) then
            allocate( ifail%info, source=inform%info )
        end if
        
        if( allocated(inform%method) ) then
            ifail%method = inform%method
        end if
        
        if( associated(inform%reason) ) then
            allocate( ifail%reason )
            call transfer_exception( inform%reason, ifail%reason )
        end if
        
        ifail%handled = inform%handled
    
    end subroutine exception_assignment_safe
    
    ! 6. Comparing
    function exception_eq_exception( exc, cxe ) result( equal )
        class(exception), intent(in) :: exc, cxe
        logical :: equal
        
        equal = .false.
        
        ! Info
        if( associated(exc%info) .or. associated(cxe%info) ) then
            if( associated(exc%info) .and. associated(cxe%info) ) then
                print *, "TODO: compare both info's ..."
                !if( exc%info /= cxe%info ) return
            else
                return
            end if
        end if
        
        ! Method
        if( allocated(exc%method) .or. allocated(cxe%method) ) then
            if( allocated(exc%method) .or. allocated(cxe%method) ) then
                if( exc%method /= cxe%method ) return
            else
                return
            end if
        end if
        
        ! Reason
        if( associated(exc%reason) .or. associated(cxe%reason) ) then
            if( associated(exc%reason) .and. associated(cxe%reason) ) then
                print *, "TODO: compare both reasons ..."
                !if( exc%reason /= cxe%reason ) return
            else
                return
            end if
        end if
        
        equal = .true.
        
    end function exception_eq_exception
    function exception_neq_exception( exc, cxe ) result( not_equal )
        class(exception), intent(in) :: exc, cxe
        logical :: not_equal
        not_equal = .not. ( exc == cxe )
    end function exception_neq_exception
    
    function exception_info_eq_exception_info( exc, cxe ) result( equal )
        class(exception_info), intent(in) :: exc, cxe
        logical :: equal
        
        if( .not. SAME_TYPE_AS( exc, cxe ) ) then
            equal = .false.
        else
            equal = .true. ! TODO
        end if
        
    end function exception_info_eq_exception_info
    function exception_info_neq_exception_info( exc, cxe ) result( not_equal )
        class(exception_info), intent(in) :: exc, cxe
        logical :: not_equal
        not_equal = .not. ( exc == cxe )
    end function exception_info_neq_exception_info

end module exception_handling_exception

module exception_handling_legacy

    use exception_handling_exception
    implicit none
    private
    save
    
    !--------------------------------------------------------------------------
    ! Commonly used terminology
    !--------------------------------------------------------------------------
    
    public :: hard_noisy_error
    public :: soft_noisy_error
    public :: soft_silent_error
    
    !--------------------------------------------------------------------------
    ! Convienience operator overloading
    !--------------------------------------------------------------------------
    
    public :: operator(.EQ.)
    interface operator(.EQ.)
        module procedure exception_eq_int
        module procedure exception_eq_int_rev
    end interface
    
    public :: operator(.NE.)
    interface operator(.NE.)
        module procedure exception_neq_int
        module procedure exception_neq_int_rev
    end interface
    
contains

    !--------------------------------------------------------------------------
    ! Commonly used terminology
    !--------------------------------------------------------------------------
    ! TODO: unit?
    
    ! Do not report the exception
    subroutine soft_silent_error( ifail )
        type(exception), intent(in out) :: ifail
        call discard_exception( ifail )
    end subroutine soft_silent_error

    ! Report the exception, discard the error and continue execution
    subroutine soft_noisy_error( ifail )
        type(exception), intent(in out) :: ifail
        call report_exception( ifail, fatal = .false. )
    end subroutine soft_noisy_error

    ! Report the exception and abort the execution
    subroutine hard_noisy_error( ifail )
        type(exception), intent(in out) :: ifail
        call report_exception( ifail, fatal = .true. )
    end subroutine hard_noisy_error
    
    !--------------------------------------------------------------------------
    ! Convienience operator overloading
    !--------------------------------------------------------------------------
    
    function exception_eq_int( exc, code ) result( equal )
        class(exception), intent(in) :: exc
        integer, intent(in) :: code
        logical :: equal
        
        if( code == 0 ) then
            equal = .not. associated( exc%info ) ! TODO?
        else
            print *, "TODO: comparison exception with non-zero code?"
            equal = (code==0)
        end if
        
    end function exception_eq_int
    function exception_neq_int( exc, code ) result( not_equal )
        class(exception), intent(in) :: exc
        integer, intent(in) :: code
        logical :: not_equal
        not_equal = .not. ( exc == code )
    end function exception_neq_int
    function exception_eq_int_rev( code, exc ) result( equal )
        integer, intent(in) :: code
        class(exception), intent(in) :: exc
        logical :: equal
        equal = exception_eq_int( exc, code )
    end function exception_eq_int_rev
    function exception_neq_int_rev( code, exc ) result( not_equal )
        class(exception), intent(in) :: exc
        integer, intent(in) :: code
        logical :: not_equal
        not_equal = .not. ( exc == code )
    end function exception_neq_int_rev

end module exception_handling_legacy

module exception_handling_common_exception_info

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
    
    ! - memory exception (includes shape of requested allocation)
    type, extends(exception_info), public :: memory_exception
        integer :: error_code
        integer, dimension(:), allocatable :: requested_shape
    contains
        procedure :: info_message => memory_exception_info_message
    end type memory_exception
    
contains
    
    subroutine memory_exception_info_message( info, message )
        class(memory_exception), intent(in) :: info
        character(len=*), intent(out) :: message
        if( size(info%requested_shape) == 1 ) then
            write(unit=message,fmt="(A,I0,A,I0,A)") "Allocating array with ", info%requested_shape, & 
                " elements did not work (error code ", info%error_code, ")."
        else
            write(unit=message,fmt=*) "Allocating array of shape ", info%requested_shape, & 
                " (=", product(info%requested_shape), & 
                " elements) did not work (error code ", info%error_code, ")."
        end if
    end subroutine memory_exception_info_message
    ! TODO: allocate wrapper for all types ...
    
    subroutine error_code_exception_info_message( info, message )
        class(error_code_exception), intent(inout) ::  info
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

end module exception_handling_common_exception_info

! DESIGN_BY_CONTRACT 
! 
!   Utilities for writing programs according to the design by contract paradigm.
! 
! HISTORY
! 
!   20101021 KP - Rewritten version
!   20101110 KP - Added Postconditions, 
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
!
module design_by_contract

    use exception_handling_exception
    implicit none
    private
    save
    
    !--------------------------------------------------------------------------
    ! Derived types
    !--------------------------------------------------------------------------

    ! 1. General Design by Contract exception
    type, extends(message_exception) :: dbc_exception
    contains
        procedure :: info_message => exception_info_message_dbc_exception
    end type dbc_exception
    
    ! 2. Precondition
    type, extends(dbc_exception) :: precondition_exception
    end type precondition_exception
    public :: precondition_fails
    interface precondition_fails
        module procedure precondition_full
    end interface precondition_fails
    
    ! 3. Postcondition
    type, extends(dbc_exception) :: postcondition_exception
    end type postcondition_exception
    public :: postcondition_fails
    interface postcondition_fails
        module procedure postcondition_full
    end interface postcondition_fails
    
contains

    function precondition_full( ifail, condition, message ) result( failed )
        type(exception), intent(out), optional :: ifail
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        logical :: failed
        failed = .not. condition
        if( failed ) then
            call create_exception(ifail, precondition_exception(message))
        end if
    end function precondition_full

    function postcondition_full( ifail, condition, message ) result( failed )
        type(exception), intent(out), optional :: ifail
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        logical :: failed
        failed = .not. condition
        if( failed ) then
            call create_exception(ifail, postcondition_exception(message))
        end if
    end function postcondition_full

    subroutine exception_info_message_dbc_exception( info, message )
        class(dbc_exception), intent(in) :: info
        character(len=*), intent(out) :: message

        character(len=12) :: condition
        
        select type(info)
            type is(precondition_exception)
                condition = "Precondition"
            type is(postcondition_exception)
                condition = "Postcondition"
            class default
                condition = "General DBC"
        end select
        
        write(unit=message,fmt=*) condition, " violation: ", info%message
        
    end subroutine exception_info_message_dbc_exception

end module design_by_contract

! EXCEPTION_HANDLING
! 
!   Module for advanced exception handling.
! 
! HISTORY
! 
!   20100902 KP - Initial version
!   20101021 KP - Rewritten version
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
!
module exception_handling
    
    use exception_handling_exception
    use exception_handling_legacy
    use exception_handling_common_exception_info
    implicit none
    public
    save
    
end module exception_handling

!(doc)footer