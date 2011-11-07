! UNITTEST_TEST
! 
!   Example of the use of the unit testing methods in the exception handling framework
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
program unittest_test
    use exception_handling
    implicit none
        
    call unit_test_reset( "unittest_test" )
    
    call short_vector()
    call large_vectors()
    call large_matrix()
    
    call abserr_vector()
    call abserr_matrix()
    call relerr_vector()
    call relerr_matrix()
    
    call write_unit_test_report()

contains

    subroutine short_vector()
        call assert_equal( (/ 1,2,3 /), (/ 1,2,3,5 /), comment="different length" )

        ! small number: show all
        call assert_equal( (/ 1,2,3 /), (/ 1,2,3 /), comment="short, equal" )
        call assert_equal( (/ 1,2,3 /), (/ 3,2,1 /), comment="short, show all" )

    end subroutine short_vector

    subroutine large_vectors()
        integer, dimension(:),   allocatable :: A,B
    
        ! large number: compress equal parts and show indexes
        A = (/ 1,2,9,9,9,9,9,9,9,9,9,9,3 /)
        B = (/ 3,1,9,9,9,9,9,9,9,9,9,9,2 /)
        call assert_equal( A,B, comment="long, compress middle" )
        A = (/ 9,9,9,9,9,9,9,9,9,9,9,9,3 /)
        B = (/ 9,9,9,9,9,9,9,9,9,9,9,9,4 /)
        call assert_equal( A,B, comment="long, compress leading" )
        A = (/ 1,2,9,9,9,9,9,9,9,9,9,9,9 /)
        B = (/ 1,2,9,9,9,9,9,9,9,9,9,9,3 /)
        call assert_equal( A,B, comment="long, compress trailing" )
        
    end subroutine large_vectors
    
    subroutine large_matrix()
        integer, dimension(:,:), allocatable :: A,B
        
        ! large matrix: compress equal parts and show indexes
        A = reshape((/ 1,2,9,9,9,3 /),(/3,2/))
        B = reshape((/ 3,1,9,9,9,2 /),(/3,2/))
        call assert_equal( A,B, comment="matrix short" )

        A = reshape((/ 1,2,9,9,9,9,9,9,9,9,9,3 /),(/4,3/))
        B = reshape((/ 3,1,9,9,9,9,9,9,9,9,9,2 /),(/4,3/))
        call assert_equal( A,B, comment="matrix, long, compress middle" )

        A = reshape((/ 9,9,9,9,9,9,9,9,9,9,9,3 /),(/4,3/))
        B = reshape((/ 9,9,9,9,9,9,9,9,9,9,9,4 /),(/4,3/))
        call assert_equal( A,B, comment="matrix, long, compress leading" )

        A = reshape((/ 1,2,9,9,9,9,9,9,9,9,9,9 /),(/4,3/))
        B = reshape((/ 1,2,9,9,9,9,9,9,9,9,9,3 /),(/4,3/))
        call assert_equal( A,B, comment="matrix, long, compress trailing" )
        
    end subroutine large_matrix
    
    subroutine relerr_vector()
        double precision, dimension(:), allocatable :: A,B
        double precision, parameter :: epsrel = 1.0d-3
        
        B = 100*(/ 1.0001d0, 1.1d0, 0.9d0, 0.99999d0 /)
        A = 0*B+100
        call assert_relerr(A,B,epsrel)
        
    end subroutine relerr_vector
    
    subroutine abserr_vector()
        double precision, dimension(:), allocatable :: A,B
        double precision, parameter :: epsabs = 1.0d-3
    
        B = (/ 1.0001, 1.1, 0.9, 0.99999 /)
        A = 0*B+1
        call assert_abserr(A,B,epsabs)
        
    end subroutine abserr_vector
    
    subroutine relerr_matrix()
        double precision, dimension(:,:), allocatable :: A,B
        double precision, parameter :: epsrel = 1.0d-3
        
        B = reshape( 100*(/ 1.0001d0, 1.1d0, 0.9d0, 0.99999d0 /), (/2,2/) )
        A = 0*B+100
        call assert_relerr(A,B,epsrel)
        
    end subroutine relerr_matrix
    
    subroutine abserr_matrix()
        double precision, dimension(:,:), allocatable :: A,B
        double precision, parameter :: epsabs = 1.0d-3
    
        B = reshape( (/ 1.0001, 1.1, 0.9, 0.99999 /), (/2,2/) )
        A = 0*B+1
        call assert_abserr(A,B,epsabs)
        
    end subroutine abserr_matrix

end program unittest_test
