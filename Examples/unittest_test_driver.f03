! UNITTEST_TEST
! 
!   Example of the use of the unit testing methods in the error handling framework
! 
! HISTORY
! 
!   20110412 KP - Converted from old code and general clean up
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
!
program unittest_test_driver
    use unittest_test
    implicit none
    
    integer, parameter :: unit = 16635
        
    call unittest_reset( "unittest_test_driver" )
    
    call primitive()
    call scalar()
    
    call short_vector()
    call large_vector()
    call large_matrix()
    
    call abserr_vector()
    call abserr_matrix()
    call relerr_vector()
    call relerr_matrix()
    
    ! unittest_report() here makes no sense, test are supposed to fail ...
    
end program
