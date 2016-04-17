! COMMON_WRAPPERS_DRIVER
! 
!   Driver for the tests for the common wrappers in the framework
! 
! HISTORY
! 
!   20120702 KP - Initial version
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
!
program common_wrappers_driver
    use common_wrappers
    implicit none

    call test_allocation()
    call test_lazyallocation()

end program common_wrappers_driver
