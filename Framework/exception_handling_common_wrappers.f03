! EXCEPTION_HANDLING_COMMON_WRAPPERS
! 
!   Collection of wrapper arround commonly used routines that need to check 
!   some condition.
! 
! HISTORY
! 
!   20110411 KP - Initial version
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
!
module exception_handling_common_wrappers
    use exception_handling_exception
    use exception_handling_common_exceptions
    implicit none
    private
    save

    interface allocate
        module procedure allocate_rank1_integer
        module procedure allocate_rank2_integer
    end interface

contains

    subroutine allocate_rank1_integer( arr, shape, ifail )
        integer, dimension(:), allocatable, intent(out) :: arr
        integer, intent( in ) :: shape
        type(exception), intent( out ), optional :: ifail
        
        integer :: stat
        allocate(arr(shape),stat=stat)
        if( stat /= 0 ) then
            call create_exception( ifail, allocation_exception( stat, (/ shape /) ) )
        end if
        
    end subroutine allocate_rank1_integer
    subroutine allocate_rank2_integer( arr, shape, ifail )
        integer, dimension(:,:), allocatable, intent(out) :: arr
        integer, dimension(2), intent( in ) :: shape
        type(exception), intent( out ), optional :: ifail
        
        integer :: stat
        allocate(arr(shape(1),shape(2)),stat=stat)
        if( stat /= 0 ) then
            call create_exception( ifail, allocation_exception( stat, shape ) )
        end if
        
    end subroutine allocate_rank2_integer
    ! 20110411 KP - TODO: auto generate ...

end module exception_handling_common_wrappers
