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
module unittest_test
    use error_handling
    implicit none
    
contains
    
    subroutine my_report_error( inform, comment )
        type(error), intent(in out) :: inform
        character(len=*), intent(in) :: comment
        
        if( inform /= 0 ) then
            select type( info => inform%info )
                class is( unit_test_error )
                    info%comment = comment
            end select
            call report_error( inform )
        end if
        call write_unit_test_report()
        
    end subroutine my_report_error

    subroutine primitive()
        type(error) :: inform
        
        call assert( .true., ifail=inform )
        call my_report_error( inform, "primitive, true" )
        call assert( .false., ifail=inform )
        call my_report_error( inform, "primitive, true" )
        
    end subroutine primitive

    subroutine scalar()
        type(error) :: inform

        call assert_equal( .true., .true., ifail=inform )
        call my_report_error( inform, "logical, equal" )
        call assert_equal( .true., .false., ifail=inform )
        call my_report_error( inform, "logical, different" )

        call assert_equal( 1, 1, ifail=inform )
        call my_report_error( inform, "integer, equal" )
        call assert_equal( 1, 2, ifail=inform )
        call my_report_error( inform, "integer, different" )

        call assert_equal( "a", "b", ifail=inform )
        call my_report_error( inform, "character(len=1)" )

        call assert_equal( "abcbcan", "bbcbacn", ifail=inform )
        call my_report_error( inform, "character(len>1)" )

    end subroutine scalar

    subroutine short_vector()
        type(error) :: inform
        
        call assert_equal( (/ 1,2,3 /), (/ 1,2,3 /), ifail=inform )
        call my_report_error( inform, comment="short_vector, equal" )
        
        ! Special
        call assert_equal( (/ 1,2,3 /), (/ 1,2,3,5 /), ifail=inform )
        call my_report_error( inform, comment="short_vector, different length" )

        ! small array: show all
        call assert_equal( (/ 1,2,3 /), (/ 1,2,3 /), ifail=inform )
        call my_report_error( inform, comment="short_vector, equal" )
        
        call assert_equal( (/ 1,2,3 /), (/ 3,2,1 /), ifail=inform )
        call my_report_error( inform, comment="short_vector, show all" )

    end subroutine short_vector

    subroutine large_vector()
        integer, dimension(:),   allocatable :: A,B
        type(error) :: inform
        
        A = (/ 1,2,9,9,9,9,9,9,9,9,9,9,3 /)
        B = (/ 1,2,9,9,9,9,9,9,9,9,9,9,3 /)
        call assert_equal( A,B, ifail=inform )
        call my_report_error( inform, "large_vector, compress middle" )
        
        ! large number: compress equal parts and show indexes
        A = (/ 1,2,9,9,9,9,9,9,9,9,9,9,3 /)
        B = (/ 3,1,9,9,9,9,9,9,9,9,9,9,2 /)
        call assert_equal( A,B, ifail=inform )
        call my_report_error( inform, "large_vector, compress middle" )
        
        A = (/ 1,2,9,9,9,9,9,9,9,9,9,9,3 /)
        B = (/ 3,2,8,9,7,9,9,9,3,9,9,9,2 /)
        call assert_equal( A,B, ifail=inform )
        call my_report_error( inform, "large_vector, compress middle" )

        A = (/ 9,9,9,9,9,9,9,9,9,9,9,9,3 /)
        B = (/ 9,9,9,9,9,9,9,9,9,9,9,9,4 /)
        call assert_equal( A,B, ifail=inform )
        call my_report_error( inform, "large_vector, compress leading" )

        A = (/ 1,2,9,9,9,9,9,9,9,9,9,9,9 /)
        B = (/ 2,2,9,9,9,9,9,9,9,9,9,9,9 /)
        call assert_equal( A,B, ifail=inform )
        call my_report_error( inform, "large_vector, compress trailing" )
        
        A = (/ 1,2,3,4,5,6,7,8,9,8,7,6,5 /)
        B = (/ 2,2,9,9,9,9,9,9,9,9,9,9,9 /)
        call assert_equal( A,B, ifail=inform )
        call my_report_error( inform, "large_vector, truncated" )
        
    end subroutine large_vector
    
    subroutine large_matrix()
        integer, dimension(:,:), allocatable :: A,B
        integer, dimension(:,:,:), allocatable :: BB
        type(error) :: inform
        
        A = reshape((/ 1,2,9,9,9,3 /),(/3,2/))
        B = reshape((/ 1,2,9,9,9,3 /),(/3,2/))
        call assert_equal( A,B, ifail=inform )
        call my_report_error( inform, "large_matrix, equal" )
        
        A = reshape((/ 1,2,9,9,9,3 /),(/3,2/))
        B = reshape((/ 1,2,9,9,9,3 /),(/2,3/))
        call assert_equal( A,B, ifail=inform )
        call my_report_error( inform, "large_matrix, different size" )
        
        ! large matrix: compress equal parts and show indexes
        A = reshape((/ 1,2,9,9,9,3 /),(/3,2/))
        B = reshape((/ 3,1,9,9,9,2 /),(/3,2/))
        call assert_equal( A,B, ifail=inform )
        call my_report_error( inform, "large_matrix, short" )

        A = reshape((/ 1,2,9,9,9,9,9,9,9,9,9,3 /),(/4,3/))
        B = reshape((/ 3,1,9,9,9,9,9,9,9,9,9,2 /),(/4,3/))
        call assert_equal( A,B, ifail=inform )
        call my_report_error( inform, "large_matrix, compress middle" )
        
        A = reshape((/ 1,2,9,9,9,9,9,9,9,9,9,3,1,2,3 /),(/5,3/))
        B = reshape((/ 3,2,9,9,9,9,9,9,9,9,9,3,2,2,4 /),(/5,3/))
        call assert_equal( A,B, ifail=inform )
        call my_report_error( inform, "large_matrix, compress several" )

        A = reshape((/ 9,9,9,9,9,9,9,9,9,9,9,3 /),(/4,3/))
        B = reshape((/ 9,9,9,9,9,9,9,9,9,9,9,4 /),(/4,3/))
        call assert_equal( A,B, ifail=inform )
        call my_report_error( inform, "large_matrix, compress leading" )

        A = reshape((/ 1,2,9,9,9,9,9,9,9,9,9,9 /),(/4,3/))
        B = reshape((/ 1,2,8,9,9,9,9,9,9,9,9,9 /),(/4,3/))
        call assert_equal( A,B, ifail=inform )
        call my_report_error( inform, "large_matrix, compress trailing" )
        
    end subroutine large_matrix
    
    subroutine relerr_vector()
        double precision, dimension(:), allocatable :: A,B
        double precision, parameter :: epsrel = 1.0d-3
        type(error) :: inform
        
        B = 100*(/ 1.0001d0, 1.1d0, 0.9d0, 0.99999d0 /)
        A = 0*B+100
        call assert_relerr(A,B,epsrel, ifail=inform)
        call my_report_error( inform, "relerr_vector" )
        
    end subroutine relerr_vector
    
    subroutine abserr_vector()
        double precision, dimension(:), allocatable :: A,B
        double precision, parameter :: epsabs = 1.0d-3
        type(error) :: inform
        
        B = (/ 1.0001, 1.1, 0.9, 0.99999 /)
        A = 0*B+1
        call assert_abserr(A,B,epsabs, ifail=inform)
        call my_report_error( inform, "abserr_vector")
        
    end subroutine abserr_vector
    
    subroutine relerr_matrix()
        double precision, dimension(:,:), allocatable :: A,B
        double precision, parameter :: epsrel = 1.0d-3
        type(error) :: inform
        
        B = reshape( 100*(/ 1.0001d0, 1.1d0, 0.9d0, 0.99999d0 /), (/2,2/) )
        A = 0*B+100
        call assert_relerr(A,B,epsrel, ifail=inform)
        call my_report_error( inform, "relerr_matrix" )
        
    end subroutine relerr_matrix
    
    subroutine abserr_matrix()
        double precision, dimension(:,:), allocatable :: A,B
        double precision, parameter :: epsabs = 1.0d-3
        type(error) :: inform
        
        B = reshape( (/ 1.0001, 1.1, 0.9, 0.99999 /), (/2,2/) )
        A = 0*B+1
        call assert_abserr(A,B,epsabs, ifail=inform )
        call my_report_error( inform, "abserr_matrix" )
        
    end subroutine abserr_matrix

end module unittest_test
