! EXCEPTION_HANDLING_COMMON_WRAPPERS
! 
!   Collection of wrapper arround commonly used intrinsic routines that need
!   to check some condition.
! 
! HISTORY
! 
!   20110411 KP - Initial version
!   20110413 (KP) - Re-generated
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

    public :: allocate
    interface allocate
        module procedure allocate_logical_rank1
        module procedure allocate_logical_rank2
        module procedure allocate_integer_rank1
        module procedure allocate_integer_rank2
        module procedure allocate_real_rank1
        module procedure allocate_real_rank2
        module procedure allocate_real_double_rank1
        module procedure allocate_real_double_rank2
    end interface
    

contains

    
    !--------------------------------------------------------------------------
    ! allocate
    !--------------------------------------------------------------------------
    
    subroutine allocate_logical_rank1( array, sizes, ifail )
        logical, dimension(:), allocatable, intent(out) :: array
        integer, intent(in) :: sizes
        type(exception), intent(out) :: ifail
        integer :: stat
        allocate( array( sizes ), stat=stat )
        if( stat /= 0 ) then
            call create_exception( ifail, &
                allocation_exception( stat, (/ sizes /) ) )
        end if
    end subroutine allocate_logical_rank1
    subroutine allocate_logical_rank2( array, sizes, ifail )
        logical, dimension(:,:), allocatable, intent(out) :: array
        integer, dimension(2), intent(in) :: sizes
        type(exception), intent(out) :: ifail
        integer :: stat
        allocate( array( sizes(1),sizes(2) ), stat=stat )
        if( stat /= 0 ) then
            call create_exception( ifail, &
                allocation_exception( stat, sizes ) )
        end if
    end subroutine allocate_logical_rank2
    subroutine allocate_integer_rank1( array, sizes, ifail )
        integer, dimension(:), allocatable, intent(out) :: array
        integer, intent(in) :: sizes
        type(exception), intent(out) :: ifail
        integer :: stat
        allocate( array( sizes ), stat=stat )
        if( stat /= 0 ) then
            call create_exception( ifail, &
                allocation_exception( stat, (/ sizes /) ) )
        end if
    end subroutine allocate_integer_rank1
    subroutine allocate_integer_rank2( array, sizes, ifail )
        integer, dimension(:,:), allocatable, intent(out) :: array
        integer, dimension(2), intent(in) :: sizes
        type(exception), intent(out) :: ifail
        integer :: stat
        allocate( array( sizes(1),sizes(2) ), stat=stat )
        if( stat /= 0 ) then
            call create_exception( ifail, &
                allocation_exception( stat, sizes ) )
        end if
    end subroutine allocate_integer_rank2
    subroutine allocate_real_rank1( array, sizes, ifail )
        real, dimension(:), allocatable, intent(out) :: array
        integer, intent(in) :: sizes
        type(exception), intent(out) :: ifail
        integer :: stat
        allocate( array( sizes ), stat=stat )
        if( stat /= 0 ) then
            call create_exception( ifail, &
                allocation_exception( stat, (/ sizes /) ) )
        end if
    end subroutine allocate_real_rank1
    subroutine allocate_real_rank2( array, sizes, ifail )
        real, dimension(:,:), allocatable, intent(out) :: array
        integer, dimension(2), intent(in) :: sizes
        type(exception), intent(out) :: ifail
        integer :: stat
        allocate( array( sizes(1),sizes(2) ), stat=stat )
        if( stat /= 0 ) then
            call create_exception( ifail, &
                allocation_exception( stat, sizes ) )
        end if
    end subroutine allocate_real_rank2
    subroutine allocate_real_double_rank1( array, sizes, ifail )
        real(kind=kind(1.0d0)), dimension(:), allocatable, intent(out) :: array
        integer, intent(in) :: sizes
        type(exception), intent(out) :: ifail
        integer :: stat
        allocate( array( sizes ), stat=stat )
        if( stat /= 0 ) then
            call create_exception( ifail, &
                allocation_exception( stat, (/ sizes /) ) )
        end if
    end subroutine allocate_real_double_rank1
    subroutine allocate_real_double_rank2( array, sizes, ifail )
        real(kind=kind(1.0d0)), dimension(:,:), allocatable, intent(out) :: array
        integer, dimension(2), intent(in) :: sizes
        type(exception), intent(out) :: ifail
        integer :: stat
        allocate( array( sizes(1),sizes(2) ), stat=stat )
        if( stat /= 0 ) then
            call create_exception( ifail, &
                allocation_exception( stat, sizes ) )
        end if
    end subroutine allocate_real_double_rank2


end module exception_handling_common_wrappers
	