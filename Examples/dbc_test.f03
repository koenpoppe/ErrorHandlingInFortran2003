! DBC_TEST
! 
!   Set of tests for the Design-By-Contract methodology using the error handling framework
! 
! HISTORY
! 
!   20111109 KP - Initial version
!   20120124 KP - Added vector_relerr_test()
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
!
program dbc_test
    use error_handling
    implicit none

    ! Header
    call unittest_reset("dbc_test")

    ! Main tests
    call factorial_test()
    call vector_relerr_test()
    
    ! Specific bug tests
    call gfortran_20120809_segfault()

    ! Footer
    call unittest_report()

contains
    
    !--------------------------------------------------------------------------
    ! Factorial
    !--------------------------------------------------------------------------
    
    subroutine factorial_test()
        integer :: n, fact
        type(error) :: inform
        
        do n=-3,20,3
            write(unit=*,fmt="(A,I2,A)",advance="no") "n = ", n, " -> n! = "
            fact = factorial( n, inform )
            if( is_error(inform) ) then
                call report_error(inform)
            else
                write(unit=*,fmt="(I10)") fact
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
        call precondition_ge( n,0,"Factorial undefined for n<0", ifail, & 
                a_name="n",filename="dbc_test.f03",line=56 )
        if( is_error(ifail) ) return
        
        ! Main
        fact = 1
        do i=2,n
            fact = fact*i
        end do
        
        ! Postconditions
        select case(n)
            case( 0 )
                call postcondition_eq( fact,1,"Definition 0!", ifail, & 
                    a_name="fact",b_name="1",filename="dbc_test.f03",line=69 )
            case default
                call postcondition_ge( fact,1,"Factorial positive", ifail, &
                    fmt="I20",a_name="fact",b_name="1",filename="dbc_test.f03",line=72 )
        end select
        
    end function factorial
    
    !--------------------------------------------------------------------------
    ! Array comparisons
    !--------------------------------------------------------------------------
    
    subroutine vector_relerr_test()
        integer, parameter :: n = 7
        double precision, dimension(n,2) :: A,B
        type(error) :: inform
        
        call random_number(A)
        B = A
        B(2,1) = floor(B(2,1)*1d3)/1d3
        B(3,2) = floor(B(3,2)*1d6)/1d6
        B(6,1) = 1d13*B(6,1)
        B(7,2) = -acos(-1.0d0)
        
        call check_relerr( A,B,epsrel=1.0d-12, ifail=inform )
        call report_error( inform )
    end subroutine vector_relerr_test

    !--------------------------------------------------------------------------
    ! Specific bug tests
    !--------------------------------------------------------------------------
    
    subroutine gfortran_20120809_segfault()
        call gfortran_20120809_segfault_sub( .true. )    
    end subroutine gfortran_20120809_segfault

    subroutine gfortran_20120809_segfault_sub( success, ifail_not_present )
        logical, intent(in) :: success
        type(error), intent(out), optional :: ifail_not_present
        print *, "Asserting"
        call assert( success, ifail=ifail_not_present ); 
        print *, "Is error?"
        print *, is_error(ifail_not_present)
    end subroutine gfortran_20120809_segfault_sub
    
end program dbc_test
