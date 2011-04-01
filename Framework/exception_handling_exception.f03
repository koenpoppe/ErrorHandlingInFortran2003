! EXCEPTION_HANDLING_EXCEPTION
! 
!   Base exception and exception info types and basic operations.
! 
! HISTORY
! 
!   20110127 KP - Initial version
!   20110330 KP - Splitted in seperate files
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
!
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
        logical, private :: handled = .false.
#ifdef FC_SUPP_FINAL
    contains
        final :: exception_final
#endif
    end type exception
    
    ! 2. Abstract information type
    type, public :: exception_info ! TODO: abstract
    contains
        procedure, pass, public :: info_message => exception_info_info_message
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
    
contains

    !--------------------------------------------------------------------------
    ! Derived types
    !--------------------------------------------------------------------------
    
    ! 1. Exceptions itself
#ifdef FC_SUPP_FINAL
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
#endif
    
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
            call exc%info%info_message(message)
            write(unit=REPORT_UNIT,fmt="(2A)") "       ", trim(adjustl(message))
        end if
        
        if( associated( exc%reason ) ) then ! TODO: necessary?
            if( is_exception( exc ) ) then
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

end module exception_handling_exception