! DESIGN_BY_CONTRACT 
! 
!   Utilities for writing programs according to the design by contract paradigm.
! 
! HISTORY
! 
!   20101021 KP - Rewritten version
!   20101110 KP - Added Postconditions, ...
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