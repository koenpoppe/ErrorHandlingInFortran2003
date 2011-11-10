! DBC_TEST
! 
!   Set of tests for the Design-By-Contract methodology using the error handling framework
! 
! HISTORY
! 
!   20111109 KP - Initial version
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
!
#include "../Framework/error_handling_preprocessing.h"
program dbc_test
    use error_handling
    implicit none

    call factorial_test()
    call assert_ne( "abcde", "abcde" )

contains

    subroutine factorial_test()
        integer :: n, fact
        type(error) :: inform

        do n=-3,20,3
            fact = factorial( n, ifail=inform )
            if( is_error(inform) ) then
                call report_error(inform)
                print *, repeat("-",80)
            else
                write(unit=*,fmt="(A,I3,A,I12)") "n = ", n, " -> n! = ", fact
            end if
        end do
    end subroutine factorial_test

    function factorial( n, ifail ) result( fact )
        integer, intent(in) :: n
        type(error), intent(out), optional :: ifail
        integer :: fact
        
        ! Local variables
        integer :: i
        
        ! Preconditions
        ! __require_ge( n, 0, "Factorial only defined for n>=0", ifail )
        call assert_ge( n,0,"Factorial only defined for n>=0",ifail, & 
                a_name="n",filename="dbc_test.f03",line=47 )
        if( is_error(ifail) ) return
        
        ! Main
        fact = 1
        do i=2,n
            fact = fact*i
        end do
        
        ! Postconditions
        select case(n)
            case( 0:1 )
                ! __ensure_eq( fact, 1, "Definition 0! and 1!", ifail )
                call assert_eq( fact,1,"Definition 0! and 1!", ifail, & 
                    a_name="fact",b_name="1",filename="dbc_test.f03",line=61 )
            case default
                ! __ensure_ge( fact, 1, "Factorial positive", ifail, fmt="I12" )
                call assert_ge( fact,1,"Factorial positive", ifail, &
                    fmt="I20",a_name="fact",b_name="1",filename="dbc_test.f03",line=64 )
        end select
        
    end function factorial

end program dbc_test
