#include "exception_handling.h"

program nversion_algorithm_driver

    use nversion_algorithm
    use exception_handling

    implicit none
    
    integer :: Ni
    integer, dimension(:), allocatable :: Ns
    Ns = (/ 100, 1000, 1000000, 1000001, 1000002 /)
    
    do Ni=1,size(Ns)
         call driver_one( Ns(Ni) )
    end do

contains

    subroutine driver_one( N )
        integer :: N
        
        TYPE_EXCEPTION :: inform
        
        write(unit=*,fmt=*)
        write(unit=*,fmt="(A,I10,A)") "=> my_nversion_algorithm(", N, " )"
#ifndef EXCEPTION_HANDLING
        inform = -1 ! soft noisy
#endif
        call my_nversion_algorithm( N, inform )
#ifdef EXCEPTION_HANDLING
        call soft_noisy_error(inform)
#endif

    end subroutine driver_one

end program nversion_algorithm_driver
