! NVERSION_ALGORITHM
! 
!   Algorithm with several versions, each of them slower but less demanding in 
!   memory usage. Depending on the available amount of memory, which is not
!   determinable in advance, the best algorithm is chosen.
!   
!   This illustrates the use of the <error_handling> module.
! 
! HISTORY
! 
!   20101021 KP - Initial version
!   20110104 KP - Preprocessable error code version added
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
!
#include "error_handling_oldnew.h"

module nversion_algorithm

#ifdef ERROR_HANDLING
    use error_handling
#endif
    
    implicit none
    private
    save
    
    public :: my_nversion_algorithm
    
#ifndef ERROR_HANDLING
    public :: print_error
#endif
    
    integer, dimension(:), allocatable :: workspace
    
contains

    ! Algorithm with 3 versions
    subroutine my_nversion_algorithm( N, ifail )
        
        ! Arguments
        integer, intent(in) :: N
        TYPE_ERROR_ARGUMENT, optional :: ifail
        
        ! Local variables
        TYPE_ERROR :: inform

        ! Try version 1
#ifndef ERROR_HANDLING
        inform = -1 ! soft noisy
#endif
        call allocate_workspace( N*N*N, inform )
        if( inform == 0 ) then
            write(unit=*,fmt="(A)") "Performing my_nversion_algorithm version 1."
            if( allocated(workspace) ) then
                deallocate(workspace)
            else
                print *, "*** KP: not allocated ???", N*N*N
            end if
#ifndef ERROR_HANDLING
            if( present(ifail) ) then
                ifail = 0 ! success
            end if
#endif            
            return
        end if
#ifdef ERROR_HANDLING
        call discard_error( inform ) ! Discard, we'll try something else
#endif
        
        ! Try version 2
#ifndef ERROR_HANDLING
        inform = -1 ! soft noisy
#endif
        call allocate_workspace( N*N, inform )
        if( inform == 0 ) then
            write(unit=*,fmt="(A)") "Performing my_nversion_algorithm version 2."
            if( allocated(workspace) ) then
                deallocate(workspace)
            else
                print *, "*** KP: not allocated ???", N*N
            end if
#ifndef ERROR_HANDLING
            if( present(ifail) ) then
                ifail = 0 ! success
            end if
#endif            
            return
        end if
#ifdef ERROR_HANDLING
        call discard_error( inform ) ! Discard, we'll try something else
#endif

        ! Try version 3, which is only valid for N even
        if( mod(N,2) == 1 ) then
#ifdef ERROR_HANDLING
            call create_error(inform, message_error( & 
                "Version 3 is only valid for even values of N"), &
                "nversion_algorithm:my_nversion_algorithm"  )
#else
            inform = -1 ! soft noisy error
            call handle_error( 3001, inform )
#endif
        else
#ifndef ERROR_HANDLING
            inform = -1 ! soft noisy
#endif
            call allocate_workspace( N, inform )
            if( inform == 0 ) then
                write(unit=*,fmt="(A)") "Performing my_nversion_algorithm version 3."
                if( allocated(workspace) ) then
                    deallocate(workspace)
                else
                    print *, "*** KP: not allocated ???", N
                end if
#ifndef ERROR_HANDLING
                if( present(ifail) ) then
                    ifail = 0 ! success
                end if
#endif
                return
            end if
        end if
        
        ! Bailing out, no versions left
#ifdef ERROR_HANDLING
        call create_error(ifail, message_error( &
            "Failed to perform any of the versions of the algorithm"), &
            inform,"nversion_algorithm:my_nversion_algorithm" )
#else
        call handle_error( 3002, ifail )
#endif
        
    end subroutine my_nversion_algorithm

    ! Error enhanced workspace allocation
    subroutine allocate_workspace( workspace_size, ifail )
        
        ! Arguments
        integer, intent(in) :: workspace_size
        TYPE_ERROR_ARGUMENT, optional :: ifail 
        
        ! Local arguments
        integer :: stat
        integer, parameter :: workspace_bound = 1000000
        type(allocation_error) :: info
        
        ! Enforce a fixed memory bound
        if( workspace_size > workspace_bound ) then
#ifdef ERROR_HANDLING
#ifdef FC_NO_DT_CONSTRUCTOR
            info = allocation_error_constructor( &
#else
            info = allocation_error( &
#endif
                    workspace_bound, (/ 1 /), (/ workspace_size /) )
            call create_error(ifail, info, &
                "nversion_algorithm:allocate_workspace:enforced" )
#else
            call handle_error( 81001, ifail )
#endif
            return
        end if
        
        ! Verify unreasonable sizes
        if( workspace_size < 0) then
#ifdef ERROR_HANDLING
#ifdef FC_NO_DT_CONSTRUCTOR
            info = allocation_error_constructor( &
#else
            info = allocation_error( &
#endif
                    -1, (/ 1 /), (/ workspace_size /) )
            call create_error(ifail, info, &
                "nversion_algorithm:allocate_workspace:negative" )
#else
            call handle_error( 81002, ifail )
#endif
            return
        end if
        
        ! Allocate the array
        allocate( workspace(workspace_size), stat=stat )
        if( stat /= 0 ) then
#ifdef ERROR_HANDLING
#ifdef FC_NO_DT_CONSTRUCTOR
            info = allocation_error_constructor( &
#else
            info = allocation_error( &
#endif
                stat, (/ 1 /), (/ workspace_size /) )
            call create_error(ifail, info, &
                "nversion_algorithm:allocate_workspace" )
#else
            call handle_error( 81000 + stat, ifail )
#endif
            return
        end if
        
#ifndef ERROR_HANDLING
        if( present(ifail) ) then
            ifail = 0 ! success
        end if
#endif
        
    end subroutine allocate_workspace
    
#ifndef ERROR_HANDLING
    subroutine print_error( inform )
        integer, intent(in) :: inform
        select case( inform )
            case( 0 )
                print *, "*** Internal error: Not an error, should never be reported..."
                
            ! nversion_algorithm error codes
            case( 3001 ) 
                print *, "Version 3 is only valid for even values of N"
            case( 3002 )
                print *, "Failed to perform any of the versions of the algorithm"
                
            ! allocation error codes
            case( 81001 )
                print *, "Allocation over 1000000 not allowed due to fixed memory bound"
            case( 81002 )
                print *, "Allocation of negative number of elements is not valid"
                
            ! collective error codes ...
            case default
                print *, "Allocation failed with stat=", (inform - 81000)
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
    
end module nversion_algorithm
