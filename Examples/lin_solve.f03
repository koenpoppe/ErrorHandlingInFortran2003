! LIN_SOLVE 
! 
!   Utility module for solving trivial diagonal linear systems. 
!
!   This illustrates the use of the <error_handling> module.
! 
! REFERENCES
!
!   The NAG Fortran 90 Library (fl90) Manual (p 1.2.9--1.2.12)
!   The Numerical Algorithms Group (2000)
!
! HISTORY
! 
!   20110118 KP - Initial version
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
! 
#include "error_handling_oldnew.h"

module lin_solve

#ifdef ERROR_HANDLING
    use design_by_contract
    use error_handling
#endif
    
    implicit none
    private
    save
    
    integer, parameter, public :: wp = kind(1.0D0)
    
    public :: solve
#ifndef ERROR_HANDLING
    public :: print_error
#endif
    
#ifdef ERROR_HANDLING
    type, extends(error_info) :: argument_error
#ifndef FC_NO_ALLOCATABLE_DTCOMP
        character(:), allocatable :: name, actual_value
        character(:), allocatable :: allowed_values
#else
        character(len=MAX_CHARACTER_LEN) :: name = "", actual_value = ""
        character(len=MAX_CHARACTER_LEN) :: allowed_values = ""
#endif
    contains
        procedure :: write_to => argument_error_write_to
    end type argument_error
    
    type, extends(error_info) :: illconditioned_error
#ifndef FC_NO_ALLOCATABLE_DTCOMP
        character(:), allocatable :: name
#else
        character(len=MAX_CHARACTER_LEN) :: name = ""
#endif
        
        real(kind=wp) :: lambda_min, lambda_max
    contains
        procedure :: write_to => illconditioned_error_write_to
    end type illconditioned_error
#endif

contains

    function solve( diag,b, trans, ifail ) result( x )
        
        real(kind=wp), dimension(:), intent(in) :: diag
        real(kind=wp), dimension(size(diag)), intent(in) :: b
        character, intent(in), optional :: trans
        TYPE_ERROR, intent( out ), optional :: ifail
        
        real(kind=wp), dimension(size(diag)) :: x
        
        character(len=1) :: ltrans
        character(len=6), parameter :: ltrans_values = "NnTtCc"
        real(kind=wp) :: lambda_max, lambda_min
        
        ! Optional input arguments
        if( present(trans) ) then
            ltrans(1:1) = trans
        else
            ltrans = "N"
        end if
        if( verify(ltrans,ltrans_values) /= 0 ) then
#ifdef ERROR_HANDLING
            call create_error(ifail, argument_error("trans",ltrans,ltrans_values) )
#else
            call handle_error( 5001, ifail )
#endif
            return
        end if
        
        ! Eigenvalues
        lambda_max = maxval(abs(diag))
        lambda_min = minval(abs(diag))
        
        ! Ill conditioned?
        if( lambda_min < lambda_max*epsilon(1.0_wp) ) then
#ifdef ERROR_HANDLING
            call create_error(ifail, illconditioned_error("diag",lambda_min,lambda_max) )
#else
            call handle_error( 5002, ifail )
#endif
            return
        end if
        
        ! Solve
        x = b / diag

#ifndef ERROR_HANDLING
        if( present( ifail ) ) then
            ifail = 0
        end if
#endif
        
    end function solve
    
#ifdef ERROR_HANDLING
    subroutine argument_error_write_to( info, unit, prefix, suffix )
        class(argument_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        write(unit=unit,fmt="(9A)") prefix, & 
            "Argument ", trim(info%name), "='", trim(info%actual_value), "' is invalid. ", & 
            "The value must be one of the following: ", trim(info%allowed_values), suffix
        
    end subroutine argument_error_write_to
    
    subroutine illconditioned_error_write_to( info, unit, prefix, suffix )
        class(illconditioned_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        if( info%lambda_min == 0.0_wp ) then
            write(unit=unit,fmt="(5A)") prefix, &
                "The matrix ", trim(info%name), " is exactly singular", suffix
        else
            write(unit=unit,fmt="(7A,ES10.3,A)") prefix, & 
                "The matrix ", trim(info%name), " is ill conditioned: ", &
                "1/cond(", trim(info%name), ")=", info%lambda_min/info%lambda_max, suffix
        end if
        
    end subroutine illconditioned_error_write_to
#else
    subroutine print_error( inform )
        integer, intent(in) :: inform
        select case( inform )
            case( 0 )
                print *, "*** Internal error: Not an error, should never be reported..."
            
            ! lin_solve error codes
            case( 5001 ) 
                print *, "The trans argument must be one of the following characters: 'NnTtCc'"
            case( 5002 )
                print *, "The matrix is ill conditioned"
            
            case default
                print *, "*** Internal error: Unknown error code ", inform
        end select
    end subroutine print_error
    subroutine handle_error( inform, ifail )
        integer, intent(in) :: inform
        integer, intent(in out), optional :: ifail
    
        if( inform /= 0 ) then
            if( present(ifail) ) then
                if( ifail <= 0 ) then ! soft noisy or hard
                    call print_error( inform )
                end if
                if( ifail == 0 ) then
                    write(unit=*,fmt=*) "Hard error: program terminated"
                    stop
                else ! soft silent or soft noisy
                    ifail = inform
                    return
                end if
            else ! soft noisy
                call print_error( inform )
                return
            end if
        else
            if( present(ifail) ) then
                ifail = 0
            end if
        end if
    end subroutine handle_error
#endif

end module lin_solve