! ERROR_HANDLING_LEGACY
! 
!   Utilities to simplify the migration of older codes.
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
module error_handling_legacy

    use error_handling_error
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
        module procedure error_eq_int
        module procedure error_eq_int_rev
    end interface
    
    public :: operator(.NE.)
    interface operator(.NE.)
        module procedure error_neq_int
        module procedure error_neq_int_rev
    end interface
    
contains

    !--------------------------------------------------------------------------
    ! Commonly used terminology
    !--------------------------------------------------------------------------

    ! Do not report the error
    subroutine soft_silent_error( ifail )
        type(error), intent(in out) :: ifail
        call discard_error( ifail )
    end subroutine soft_silent_error

    ! Report the error, discard the error and continue execution
    subroutine soft_noisy_error( ifail )
        type(error), intent(in out) :: ifail
        call report_error( ifail, fatal = .false. )
    end subroutine soft_noisy_error

    ! Report the error and abort the execution
    subroutine hard_noisy_error( ifail )
        type(error), intent(in out) :: ifail
        call report_error( ifail, fatal = .true. )
    end subroutine hard_noisy_error

    !--------------------------------------------------------------------------
    ! Convienience operator overloading
    !--------------------------------------------------------------------------

    function error_eq_int( exc, code ) result( equal )
        class(error), intent(in) :: exc
        integer, intent(in) :: code
        logical :: equal
        
        if( code == 0 ) then
            equal = .not. associated( exc%info ) ! TODO?
        else
            print *, "TODO: comparison error with non-zero code?"
            equal = (code==0)
        end if
        
    end function error_eq_int
    function error_neq_int( exc, code ) result( not_equal )
        class(error), intent(in) :: exc
        integer, intent(in) :: code
        logical :: not_equal
        not_equal = .not. ( exc == code )
    end function error_neq_int
    function error_eq_int_rev( code, exc ) result( equal )
        integer, intent(in) :: code
        class(error), intent(in) :: exc
        logical :: equal
        equal = error_eq_int( exc, code )
    end function error_eq_int_rev
    function error_neq_int_rev( code, exc ) result( not_equal )
        class(error), intent(in) :: exc
        integer, intent(in) :: code
        logical :: not_equal
        not_equal = .not. ( exc == code )
    end function error_neq_int_rev

end module error_handling_legacy