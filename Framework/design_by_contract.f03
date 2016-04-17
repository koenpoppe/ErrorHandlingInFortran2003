! DESIGN_BY_CONTRACT 
! 
!   Utilities for writing programs according to the design by contract paradigm.
! 
! HISTORY
! 
!   20101021 KP - Rewritten version
!   20101110 KP - Added Postconditions, ...
!   20120123 KP - DBC errors are no longer 'cachable'
!   20160417 KP - Unit tests are not be skipped
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
    ! What to check
    !--------------------------------------------------------------------------
    
    logical :: check_precondition = .true.
    logical :: check_postcondition = .true.
    logical :: check_check = .true.
    
    public :: skip_precondition
    public :: skip_postcondition
    public :: skip_check
    
    !--------------------------------------------------------------------------
    ! Settings
    !--------------------------------------------------------------------------
    
    public :: dbc_setup
    
contains
    
    !--------------------------------------------------------------------------
    ! What to check
    !--------------------------------------------------------------------------

    function skip_precondition() result(skip)
        logical :: skip
        skip = .not. check_precondition
        if( skip ) print *, "skip_precondition"
    end function skip_precondition
    
    function skip_postcondition() result(skip)
        logical :: skip
        skip = .not. check_postcondition
        if( skip ) print *, "skip_postcondition"
    end function skip_postcondition
    
    function skip_check() result(skip)
        logical :: skip
        skip = .not. check_check
        if( skip ) print *, "skip_check"
    end function skip_check

    !--------------------------------------------------------------------------
    ! Settings
    !--------------------------------------------------------------------------
    
    ! Change the verification of all types of design-by-contract checks
    !    call dbc_setup( all=.true. )
    !    call dbc_setup( all=.false. )
    ! 
    ! Change specific checks
    !    call dbc_setup( precondition=.true. )
    !    call dbc_setup( precondition=.false. )
    subroutine dbc_setup( all, precondition, postcondition, check )
        logical, intent(in) :: all, precondition, postcondition, check
        
        check_precondition  = optional_logical( precondition,  optional_logical( all, check_precondition  ) )
        check_postcondition = optional_logical( postcondition, optional_logical( all, check_postcondition ) )
        check_check         = optional_logical( check,         optional_logical( all, check_check         ) )
        
    end subroutine dbc_setup
    
    function optional_logical( opt, default )
        logical, intent(in), optional :: opt
        logical, intent(in) :: default
        logical :: optional_logical
        
        if( present(opt) ) then
            optional_logical = opt
        else
            optional_logical = default
        end if
        
    end function optional_logical

end module design_by_contract