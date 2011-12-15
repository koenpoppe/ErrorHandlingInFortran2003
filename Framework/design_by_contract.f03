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

    use error_handling_error
    implicit none
    private
    save
    
    !--------------------------------------------------------------------------
    ! Derived types
    !--------------------------------------------------------------------------

!     ! 1. General Design by Contract error
!     type, extends(message_error), public :: dbc_error
!         character(:), allocatable :: statement, comment, filename
!         integer :: line = -1
!     contains
!         procedure :: info_message => error_info_message_dbc_error
!     end type dbc_error
    
!     ! 2. Precondition
!     type, extends(dbc_error) :: precondition_error
!     end type precondition_error
!     public :: precondition_fails
!     interface precondition_fails
!         module procedure precondition_full
!     end interface precondition_fails
!     
!     ! 3. Postcondition
!     type, extends(dbc_error) :: postcondition_error
!     end type postcondition_error
!     public :: postcondition_fails
!     interface postcondition_fails
!         module procedure postcondition_full
!     end interface postcondition_fails
    
! contains

!     function precondition_full( ifail, condition, message ) result( failed )
!         type(error), intent(out), optional :: ifail
!         logical, intent(in) :: condition
!         character(len=*), intent(in) :: message
!         logical :: failed
!         failed = .not. condition
!         if( failed ) then
!             call create_error( ifail, precondition_error(message) )
!         end if
!     end function precondition_full
! 
!     function postcondition_full( ifail, condition, message ) result( failed )
!         type(error), intent(out), optional :: ifail
!         logical, intent(in) :: condition
!         character(len=*), intent(in) :: message
!         logical :: failed
!         failed = .not. condition
!         if( failed ) then
!             call create_error(ifail, postcondition_error(message))
!         end if
!     end function postcondition_full
! 
!     subroutine error_info_message_dbc_error( info, unit, prefix, suffix )
!         class(dbc_error), intent(in) :: info
!         integer, intent(in) :: unit
!         character(len=*), intent(in) :: prefix, suffix
! 
!         character(len=12) :: condition
!         
!         select type(info)
!             type is(precondition_error)
!                 condition = "Precondition"
!             type is(postcondition_error)
!                 condition = "Postcondition"
!             class default
!                 condition = "General DBC"
!         end select
!         
!         write(unit=unit,fmt="(5A)") prefix, condition, " violation: ", info%message, suffix
!         
!     end subroutine error_info_message_dbc_error

end module design_by_contract