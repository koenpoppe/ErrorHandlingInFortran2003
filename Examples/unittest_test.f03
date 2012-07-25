! UNITTEST_TEST
! 
!   Example of the use of the unit testing methods in the error handling framework
! 
! HISTORY
! 
!   20110412 KP - Converted from old code and general clean up
!   20120611 KP - Updated using unittest_ primitives
!   20120725 KP - Updated to ensure expected truncation with removed leading spaces
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
!
module unittest_test
    use error_handling
    implicit none
    integer, parameter :: x = 99999999
    
contains

    subroutine primitive()
        
        call unittest( .true., comment="primitive, succeeds" )
        call unittest( .false., comment="primitive, must fail" )
        
    end subroutine primitive

    subroutine scalar()

        call unittest_eq( .true., .true., comment="logical, equal" )
        call unittest_eq( .true., .false., comment="logical, different, must fail" )

        call unittest_eq( 1, 1, comment="integer, equal" )
        call unittest_eq( 1, 2, comment="integer, different, must fail" )

        call unittest_eq( "a", "b", comment="character(len=1), must fail" )

        call unittest_eq( "abcbcan", "bbcbacn", comment="character(len>1), must fail" )

    end subroutine scalar

    subroutine short_vector()
        
        call unittest_eq( (/ 1,2,3 /), (/ 1,2,3 /), comment="short_vector, equal" )
        
        ! Special
        call unittest_eq( (/ 1,2,3 /), (/ 1,2,3,5 /), comment="short_vector, different length, must fail" )

        ! small array: show all
        call unittest_eq( (/ 1,2,3 /), (/ 1,2,3 /), comment="short_vector, equal" )
        
        call unittest_eq( (/ 1,2,3 /), (/ 3,2,1 /), comment="short_vector, show all, must fail" )

    end subroutine short_vector

    subroutine large_vector()
        integer, dimension(:),   allocatable :: A,B
        
        A = (/ 1,2,x,x,x,x,x,x,x,x,x,x,3 /)
        B = (/ 1,2,x,x,x,x,x,x,x,x,x,x,3 /)
        call unittest_eq( A,B, comment="large_vector, compress middle, must fail" )
        
        ! large number: compress equal parts and show indexes
        A = (/ 1,2,x,x,x,x,x,x,x,x,x,x,3 /)
        B = (/ 3,1,x,x,x,x,x,x,x,x,x,x,2 /)
        call unittest_eq( A,B, comment="large_vector, compress middle, must fail" )
        
        A = (/ 1,2,9,x,9,x,x,x,9,x,x,x,3 /)
        B = (/ 3,2,8,x,7,x,x,x,3,x,x,x,2 /)
        call unittest_eq( A,B, comment="large_vector, compress middle, must fail" )

        A = (/ x,x,x,x,x,x,x,x,x,x,x,x,3 /)
        B = (/ x,x,x,x,x,x,x,x,x,x,x,x,4 /)
        call unittest_eq( A,B, comment="large_vector, compress leading, must fail" )

        A = (/ 1,2,x,x,x,x,x,x,x,x,x,x,9 /)
        B = (/ 2,2,x,x,x,x,x,x,x,x,x,x,9 /)
        call unittest_eq( A,B, comment="large_vector, compress trailing, must fail" )
        
        A = (/ 1,2,3,4,5,6,7,8,x,8,7,6,5,4,5,6 /)
        B = (/ 2,2,x,x,x,x,x,x,x,9,9,9,9,3,5,3 /)
        call unittest_eq( A,B, comment="large_vector, truncated, must fail" )
        
    end subroutine large_vector
    
    subroutine large_matrix()
        integer, dimension(:,:), allocatable :: A,B
        
        A = reshape((/ 1,2,x,x,x,3 /),(/3,2/))
        B = reshape((/ 1,2,x,x,x,3 /),(/3,2/))
        call unittest_eq( A,B, comment="large_matrix, equal" )
        
        A = reshape((/ 1,2,x,x,x,3 /),(/3,2/))
        B = reshape((/ 1,2,x,x,x,3 /),(/2,3/))
        call unittest_eq( A,B, comment="large_matrix, different size, must fail" )
        
        ! large matrix: compress equal parts and show indexes
        A = reshape((/ 1,2,x,x,x,3 /),(/3,2/))
        B = reshape((/ 3,1,x,x,x,2 /),(/3,2/))
        call unittest_eq( A,B, comment="large_matrix, short, must fail" )

        A = reshape((/ 1,2,x,x,x,x,x,x,x,x,x,3 /),(/4,3/))
        B = reshape((/ 3,1,x,x,x,x,x,x,x,x,x,2 /),(/4,3/))
        call unittest_eq( A,B, comment="large_matrix, compress middle, must fail" )
        
        A = reshape((/ 1,2,x,x,x,x,x,x,x,x,x,3,1,2,3 /),(/5,3/))
        B = reshape((/ 3,2,x,x,x,x,x,x,x,x,x,3,2,2,4 /),(/5,3/))
        call unittest_eq( A,B, comment="large_matrix, compress several, must fail" )

        A = reshape((/ x,x,x,x,x,x,x,x,x,x,x,3 /),(/4,3/))
        B = reshape((/ x,x,x,x,x,x,x,x,x,x,x,4 /),(/4,3/))
        call unittest_eq( A,B, comment="large_matrix, compress leading, must fail" )

        A = reshape((/ 1,2,x,x,x,x,x,x,x,x,x,9 /),(/4,3/))
        B = reshape((/ 1,2,8,x,x,x,x,x,x,x,x,9 /),(/4,3/))
        call unittest_eq( A,B, comment="large_matrix, compress trailing, must fail" )
        
    end subroutine large_matrix
    
    subroutine relerr_vector()
        double precision, dimension(:), allocatable :: A,B
        double precision, parameter :: epsrel = 1.0d-3
        
        B = 100*(/ 1.0001d0, 1.1d0, 0.9d0, 0.99999d0 /)
        A = 0*B+100
        call unittest_relerr(A,B,epsrel, comment="relerr_vector, must fail" )
        
    end subroutine relerr_vector
    
    subroutine abserr_vector()
        double precision, dimension(:), allocatable :: A,B
        double precision, parameter :: epsabs = 1.0d-3
        
        B = (/ 1.0001, 1.1, 0.9, 0.99999 /)
        A = 0*B+1
        call unittest_abserr(A,B,epsabs, comment="abserr_vector, must fail")
        
    end subroutine abserr_vector
    
    subroutine relerr_matrix()
        double precision, dimension(:,:), allocatable :: A,B
        double precision, parameter :: epsrel = 1.0d-3
        
        B = reshape( 100*(/ 1.0001d0, 1.1d0, 0.9d0, 0.99999d0 /), (/2,2/) )
        A = 0*B+100
        call unittest_relerr(A,B,epsrel, comment="relerr_matrix, must fail" )
        
    end subroutine relerr_matrix
    
    subroutine abserr_matrix()
        double precision, dimension(:,:), allocatable :: A,B
        double precision, parameter :: epsabs = 1.0d-3
        
        B = reshape( (/ 1.0001, 1.1, 0.9, 0.99999 /), (/2,2/) )
        A = 0*B+1
        call unittest_abserr(A,B,epsabs, comment="abserr_matrix, must fail" )
        
    end subroutine abserr_matrix

end module unittest_test
