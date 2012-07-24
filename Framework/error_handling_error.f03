! ERROR_HANDLING_ERROR
! 
!   Base error and error info types and basic operations.
! 
! HISTORY
! 
!   20110127 KP - Initial version
!   20110330 KP - Splitted in seperate files
!   20111107 KP - Solved <associated> bugs that appeared by trying to avoid
!                      allocate( ifail%info, source=info )
!                 
!                 which is NOT equivalent with
!                      allocate(ifail%info)
!                      ifail%info=>info
!                 Because info is intent(in), it gets deleted at the end of the method, 
!                 and that results in a dangling pointer ...
!   20111108 KP - Changed the write_to prototype
!   20120626 KP - Added isnt_error convenience method
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
!
! TODO
! 
!   20120619 KP - Allow several reasons, for example when checking input arguments?
!               - Describe prefix/suffix -> output as XML, JSON ?
!               - Fallback version without autogenerate?
! 
module error_handling_error

    use ISO_FORTRAN_ENV, only: OUTPUT_UNIT
    ! use ISO_FORTRAN_ENV, only: ERROR_UNIT
    implicit none
    private
    save

    !--------------------------------------------------------------------------
    ! Derived types
    !--------------------------------------------------------------------------
    
    ! 1. Abstract information type
    type, public :: error_info ! TODO: abstract
    contains
        procedure, pass, public :: write_to => error_info_write_to
    end type error_info

    ! 2. Errors itself
#ifdef FC_NO_ALLOCATABLE_DTCOMP
    integer, parameter, public :: MAX_CHARACTER_LEN=1024
#else
#ifdef FC_FIXED_LENGTH_CHARACTERSTRINGS
    integer, parameter, public :: MAX_CHARACTER_LEN=1024
#endif
#endif
    type, public :: error
        class(error_info), pointer :: info => NULL()
#ifndef FC_NO_ALLOCATABLE_DTCOMP
        character(:), allocatable :: method
#else
        character(len=MAX_CHARACTER_LEN) :: method = ""
#endif
        type(error), pointer :: reason => NULL()
        logical, private :: handled = .false.
#ifndef FC_NO_FINAL_SUPPORT
    contains
        final :: error_final
#endif
    end type error
    
    public :: is_error, isnt_error

    ! 3. Sentinel no-error information type
    type, extends(error_info), public :: no_error
    contains
        procedure, pass :: write_to => no_error_write_to
    end type no_error
    
    ! 4. Message error information type
    type, extends(error_info), public :: message_error
#ifndef FC_NO_ALLOCATABLE_DTCOMP
        character(:), allocatable :: message
#else
        character(len=MAX_CHARACTER_LEN) :: message = ""
#endif
    contains
        procedure :: write_to => message_error_write_to
    end type message_error
    
    !--------------------------------------------------------------------------
    ! Operations for errors
    !--------------------------------------------------------------------------
    
    ! 1. Creation/chaining
    public :: create_error
    interface create_error
        module procedure create_error_ifail_error
        module procedure create_error_ifail_error_reason 
        module procedure create_error_ifail_error_method
        module procedure create_error_full
    end interface create_error

    ! 2. Transfering
    public :: transfer_error

    ! 3. Reporting
    public :: report_error
    public :: report_error_unit, set_report_error_unit
    integer, private :: REPORT_UNIT = OUTPUT_UNIT ! or ERROR_UNIT
    
    ! 4. Discarding
    public :: discard_error
    
    ! 5. Assignment
    public :: error_assignment_safe
    
contains

    !--------------------------------------------------------------------------
    ! Derived types
    !--------------------------------------------------------------------------

#ifndef FC_NO_FINAL_SUPPORT
    ! 1. Errors itself
    recursive subroutine error_final( exc )
        type(error),intent(in out) :: exc
        
        if( .not. exc%handled .and. is_error(exc) ) then
            call error_final_unhandled( exc )
        end if
        
    end subroutine error_final
    
    recursive subroutine error_final_unhandled( exc )
        type(error),intent(in out) :: exc
        type(error) :: inform
        
        call create_error( inform, message_error("Errors must not be ignored, the following one was ..."), exc, &
            "error_handling:error_final_unhandled" )
        call report_error( inform, fatal=.true. )
        
    end subroutine error_final_unhandled
#endif
    
    ! 2. Abstract information type
    subroutine error_info_write_to( info, unit, prefix, suffix )
        class(error_info), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        select type(info)
            type is(error_info)
                write(unit=unit,fmt="(3A)") prefix, &
                    "No specific information available ...", suffix
            class default
                write(unit=unit,fmt="(3A)") prefix, &
                    "(override the 'write_to' type bound procedure to provide a meaningful messages)", suffix
        end select
        
    end subroutine error_info_write_to
    
    subroutine message_error_write_to( info, unit, prefix, suffix )
        class(message_error), intent(in) ::  info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix

#ifndef FC_NO_ALLOCATABLE_DTCOMP
        if( allocated(info%message) ) then ! TODO precondition
#else
        if( len_trim(info%message) > 0 ) then
#endif
            write(unit=unit,fmt="(3A)") prefix, &
                trim(info%message), suffix
        else
            write(unit=unit,fmt="(3A)") prefix, &
                "INTERNAL BUG? info%message not allocated"
        end if

    end subroutine message_error_write_to
    
    ! 3. Sentinel no-error information type
    subroutine no_error_write_to( info, unit, prefix, suffix )
        class(no_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        select type( info )
            type is(no_error)
                write(unit=unit,fmt="(3A)") prefix, &
                    "INTERNAL ERROR: no error here.", suffix
            class default
                write(unit=unit,fmt="(3A)") prefix, &
                    "EXTERNAL ERROR: you should not extend the no_error derived type.", suffix
        end select
        
    end subroutine no_error_write_to
    
    !--------------------------------------------------------------------------
    ! Operations for errors
    !--------------------------------------------------------------------------
    
    ! 1. Creation/chaining
    
    pure function empty_error() result( exc )
        type(error) :: exc
        exc%handled = .true. ! TODO: This is an empty error
    end function empty_error
    
    function is_error( exc ) result( is_exc )
        class(error), intent(in), optional :: exc
        logical :: is_exc
        
        if( .not. present(exc) ) then
            is_exc = .false.
            return
        end if
        
        if( associated( exc%info ) ) then
            select type( E => exc%info ) 
                type is( no_error )
                    is_exc = .false.
                class default
                    is_exc = .true.
            end select
        else
            ! print *, "INTERNAL ERROR: not associated " ! TODO ensure
            is_exc = .false.
        end if
        
    end function is_error
    
    function isnt_error( exc ) result( isnt_exc )
        class(error), intent(in), optional :: exc
        logical :: isnt_exc
        isnt_exc = .not. is_error( exc )
    end function isnt_error
    
    subroutine error_constructor( ifail, info, reason, method )

        ! Arguments
        type(error), intent( out ) :: ifail
        class(error_info), target, intent(in) :: info
        type(error), target, intent(in out), optional :: reason
        character(len=*), intent(in), optional :: method
        
        ! Prepare the error
        allocate( ifail%info, source=info ) ! 20111107 KP (see HISTORY)
        
        if( present(method) ) then
            if( len_trim(method) > 0 ) then
                ifail%method = method
            end if
        end if

        if( present(reason) ) then
            allocate( ifail%reason ) ! TODO: check allocation
            call transfer_error( reason, ifail%reason )
        else
            nullify( ifail%reason )
        end if
        
        ifail%handled = .false.
        
    end subroutine error_constructor
    
    subroutine create_error_full( ifail, info, reason, method )
        
        ! Arguments
        type(error), intent(out), optional :: ifail
        class(error_info), target, intent(in) :: info
        type(error), target, intent(in out) :: reason
        character(len=*), intent(in) :: method
        
        type(error) :: inform
        
        call error_constructor( inform, info, reason, method )
        call transfer_error( inform, ifail )
        
    end subroutine create_error_full

    subroutine create_error_ifail_error( ifail, info )
        
        ! Arguments
        type(error), intent(out), optional ::  ifail
        class(error_info), intent(in) :: info
        
        type(error) :: empty
        empty = empty_error()
        call create_error_full( ifail, info, empty, "" )
        
    end subroutine create_error_ifail_error
    
    subroutine create_error_ifail_error_method( ifail, info, method )
        
        ! Arguments
        type(error), intent(out), optional ::  ifail
        class(error_info), intent(in) :: info
        character(len=*), intent(in) :: method
        
        type(error) :: empty
        empty = empty_error()
        call create_error_full( ifail, info, empty, method )
        
    end subroutine create_error_ifail_error_method
    
    subroutine create_error_ifail_error_reason( ifail, info, reason )
        
        ! Arguments
        type(error), intent(out), optional ::  ifail
        class(error_info), intent(in) :: info
        type(error), intent(in out) :: reason
        
        call create_error_full( ifail, info, reason, "" )
        
    end subroutine create_error_ifail_error_reason

    ! 2. Transfering
    recursive subroutine transfer_error( inform, ifail ) ! ifail = inform
        type(error), intent(in out) :: inform
        type(error), intent(in out), optional :: ifail
        
        if( present(ifail) ) then
            call error_assignment_safe( ifail, inform )
            call discard_error( inform ) ! discard
        else
            call report_error( inform, fatal=.true. )
        end if
        
    end subroutine transfer_error

    ! 3. Reporting
    subroutine report_error( exc, fatal )
        type(error), intent(in out) :: exc
        logical, intent(in), optional :: fatal
        if( .not. is_error(exc) ) return
        call write_error_trace( exc, 0 )
        call discard_error( exc )
        if( present(fatal) ) then
            if( fatal ) then
                write(unit=REPORT_UNIT,fmt="(A)") "FATAL -> STOP"
                stop
            end if
        end if
    end subroutine report_error
    
    function report_error_unit() result( unit )
        integer :: unit
        unit = REPORT_UNIT
    end function report_error_unit
    
    subroutine set_report_error_unit( unit )
        integer, intent(in) :: unit
        REPORT_UNIT = unit
    end subroutine set_report_error_unit
    
    ! Format the full error trace (Top down)
    recursive subroutine write_error_trace( exc, level )
    
        ! Arguments
        type(error), intent(in) :: exc
        integer, intent(in) :: level
        
        ! Is this actually an exception with information?
        if( .not. associated(exc%info) ) return
        
        if( level == 0 ) then
            write(unit=REPORT_UNIT,fmt="(A)",advance="no") "*** Error"
        else
            write(unit=REPORT_UNIT,fmt="(A)",advance="no") " ** cascading"
        end if

#ifndef FC_NO_ALLOCATABLE_DTCOMP
        if( allocated(exc%method) ) &
            write(unit=REPORT_UNIT,fmt="(3A)",advance="no") " from '", exc%method, "'"
#else
        if( len_trim(exc%method) > 0 ) &
            write(unit=REPORT_UNIT,fmt="(3A)",advance="no") " from '", trim(exc%method), "'"
#endif
            
            
        write(unit=REPORT_UNIT,fmt="(A)") ":"
        
        if( associated(exc%info) ) then
!             call exc%info%write_to( REPORT_UNIT, "<line>", "</line>" ) ! TODO: xml output
            call exc%info%write_to( REPORT_UNIT, "       ", "" )
        end if
        
        if( associated( exc%reason ) ) then ! TODO: necessary?
            if( is_error( exc ) ) then
                call write_error_trace( exc%reason, level+1 )
            end if
        end if
        
    end subroutine write_error_trace
    
    ! 4. Discarding
    subroutine discard_error( ifail )
        
        ! Arguments
        type(error), intent(in out) :: ifail
        
        ! Discard the error
        ifail%handled = .true.
        
    end subroutine discard_error
    
    ! 5. Assignment
    recursive subroutine error_assignment_safe( ifail, inform ) ! ifail = inform
        class(error), intent(out) :: ifail
        class(error), intent(in out) :: inform
        
        if( associated(inform%info) ) then
            allocate( ifail%info, source=inform%info ) ! 20111107 KP (see HISTORY)
            nullify( inform%info )
        end if

#ifndef FC_NO_ALLOCATABLE_DTCOMP
        if( allocated(inform%method) ) then
            ifail%method = inform%method
        end if
#else
        ifail%method = inform%method
#endif
        if( associated(inform%reason) ) then
            allocate( ifail%reason )
            call transfer_error( inform%reason, ifail%reason )
        end if
        
        ifail%handled = inform%handled
        
    end subroutine error_assignment_safe

end module error_handling_error
