#include "error_handling_oldnew.h"

program lin_solve_driver

#ifdef ERROR_HANDLING
    use error_handling
#endif
    use lin_solve
    
    implicit none

    integer, parameter :: n = 3
    integer :: i
    real(kind=wp), dimension(n) :: a,aa ! only diagonal stored
    real(kind=wp), dimension(n) :: b,bb
    
    ! Initialise the arrays a and b for passing to lin_solve
    a = (/ ( real(i,wp), i=1, n ) /)
    b = 1.0_wp
    
    ! Straightforward problem: successfull exit
    aa = a
    bb = b
    write(unit=*,fmt="(A)") "1st call to solve"
    call solve_and_report( aa,bb )
    
    ! Very ill-conditioned problem
    aa = a
    bb = b
    aa(1) = 0.1_wp*EPSILON(1.0_wp)
    write(unit=*,fmt="(A)") "2nd call to solve"
    call solve_and_report( aa,bb )
    
    ! Singular problem
    aa = a
    bb = b
    aa(1) = 0.0_wp
    write(unit=*,fmt="(A)") "3th call to solve"
    call solve_and_report( aa,bb )
    
    ! Invalid value for trans
    aa = a
    bb = b
    write(unit=*,fmt="(A)") "4th call to solve"
    call solve_and_report( aa,bb, 's' )
    
contains
    
    subroutine solve_and_report( a, b, trans )
        real(kind=wp), dimension(:), intent(in) ::  a, b
        character, intent(in), optional :: trans
        real(kind=wp), dimension(size(b)) :: x
        
        TYPE_ERROR :: inform
        
        x = solve( a,b, trans, ifail=inform )
        if( inform == 0 ) then
            write(unit=*,fmt="(A)") "Solution returned in x:"
            write(unit=*,fmt="(1X,6E11.3)") x
        else
            write(unit=*,fmt="(A)") "No solution returned in x because"
#ifdef ERROR_HANDLING
            call soft_noisy_error( inform )
#else
            call print_error( inform )
#endif
        end if
        write(unit=*,fmt=*)
        
    end subroutine solve_and_report
    
    
end program lin_solve_driver