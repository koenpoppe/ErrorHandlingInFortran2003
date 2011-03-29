! LIN_SOLVE 
! 
!   Utility module for solving trivial diagonal linear systems. 
!
!   This illustrates the use of the <exception_handling> module.
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
#include "exception_handling.h"

module lin_solve

#ifdef EXCEPTION_HANDLING
    use design_by_contract
    use exception_handling
#endif
    
    implicit none
    private
    save
    
    integer, parameter, public :: wp = kind(1.0D0)
    
    public :: solve
#ifndef EXCEPTION_HANDLING
    public :: print_error
#endif
    
#ifdef EXCEPTION_HANDLING
    type, extends(exception_info) :: argument_exception
        character(:), allocatable :: name, actual_value
        character(:), allocatable :: allowed_values
    contains
        procedure :: info_message => exception_info_message_argument_exception
    end type argument_exception
    
    type, extends(exception_info) :: illconditioned_exception
        character(:), allocatable :: name
        real(kind=wp) :: lambda_min, lambda_max
    contains
        procedure :: info_message => exception_info_message_illconditioned_exception
    end type illconditioned_exception
#endif

contains

    function solve( diag,b, trans, ifail ) result( x )
        
        real(kind=wp), dimension(:), intent(in) :: diag
        real(kind=wp), dimension(size(diag)), intent(in) :: b
        character, intent(in), optional :: trans
        TYPE_EXCEPTION, intent( out ), optional :: ifail
        
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
#ifdef EXCEPTION_HANDLING
            call create_exception(ifail, argument_exception("trans",ltrans,ltrans_values) )
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
#ifdef EXCEPTION_HANDLING
            call create_exception(ifail, illconditioned_exception("diag",lambda_min,lambda_max) )
#else
            call handle_error( 5002, ifail )
#endif
            return
        end if
        
        ! Solve
        x = b / diag

#ifndef EXCEPTION_HANDLING
        if( present( ifail ) ) then
            ifail = 0
        end if
#endif
        
    end function solve
    
#ifdef EXCEPTION_HANDLING
    subroutine exception_info_message_argument_exception( info, message )
        class(argument_exception), intent(in) :: info
        character(len=*), intent(out) :: message
        
        write(unit=message,fmt="(7A)") & 
            "Argument ", info%name, "='", info%actual_value, "' is invalid. ", & 
            "The value must be one of the following: ", info%allowed_values
        
    end subroutine exception_info_message_argument_exception
    
    subroutine exception_info_message_illconditioned_exception( info, message )
        class(illconditioned_exception), intent(in) :: info
        character(len=*), intent(out) :: message
        
        if( info%lambda_min == 0.0_wp ) then
            write(unit=message,fmt="(3A)") &
                "The matrix ", info%name, " is exactly singular"
        else
            write(unit=message,fmt="(6A,ES10.3)") & 
                "The matrix ", info%name, " is ill conditioned: ", &
                "1/cond(", info%name, ")=", info%lambda_min/info%lambda_max
        end if
        
    end subroutine exception_info_message_illconditioned_exception
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