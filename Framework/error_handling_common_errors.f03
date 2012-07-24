module error_handling_common_errors

    use error_handling_error
    implicit none
    private
    save
    
    ! Error code + message
    type, extends(error_info), public :: error_code_error
        integer :: error_code
    contains
        procedure :: write_to => error_code_error_write_to
    end type error_code_error
    
    ! IOSTAT
    type, extends(error_code_error), public :: iostat_error
    contains
        procedure :: write_to => iostat_error_write_to
    end type iostat_error
    
    ! - allocation error (includes shape of requested allocation)
    type, extends(error_info), public :: allocation_error
        integer :: error_code
#ifndef FC_NO_ALLOCATABLE_DTCOMP
        integer, dimension(:), allocatable :: requested_lower, requested_upper
#endif
    contains
        procedure :: write_to => allocation_error_write_to
    end type allocation_error
#ifdef FC_NO_ALLOCATABLE_DTCOMP
#ifdef FC_NO_DT_CONSTRUCTOR
    public :: allocation_error_constructor
#else
    interface allocation_error
        module procedure allocation_error_constructor
    end interface allocation_error
#endif
    integer, dimension(:), public, allocatable :: allocation_error_requested_lower, allocation_error_requested_upper ! TODO: private
#endif

    ! - enumeration error
    type, extends(error_info), public :: enumeration_error
        integer :: enum
#ifndef FC_NO_ALLOCATABLE_DTCOMP
        integer, dimension(:), allocatable :: enums
        character(len=40), dimension(:), allocatable :: enum_descriptions
        character(:), allocatable :: message
#endif
    contains
        procedure :: write_to => enum_descriptions_write_to
    end type enumeration_error
#ifdef FC_NO_ALLOCATABLE_DTCOMP
    integer, dimension(:), allocatable, private :: enumeration_error_enums
    character(len=40), dimension(:), allocatable, private :: enumeration_error_enum_descriptions
#ifdef FC_FIXED_LENGTH_CHARACTERSTRINGS
    character(len=MAX_CHARACTER_LEN), allocatable, private :: enumeration_error_message
#else
    character(:), allocatable, private :: enumeration_error_message
#endif
#endif

    
contains
    
#ifdef FC_NO_ALLOCATABLE_DTCOMP
    function allocation_error_constructor( error_code, requested_lower, requested_upper ) result( info )
        integer, intent(in) :: error_code
        integer, dimension(:), intent(in) :: requested_lower
        integer, dimension(:), intent(in) :: requested_upper
        type(allocation_error) :: info
        
        info%error_code = error_code
        if( allocated(allocation_error_requested_lower)) &
            deallocate(allocation_error_requested_lower)
        if( allocated(allocation_error_requested_upper)) &
            deallocate(allocation_error_requested_upper)
        !   20120724 KP - Assignment to allocatable variable is broken in ifort_beta (IFORT) 12.1.5 20120612 ...
        !         allocation_error_requested_lower = requested_lower ! F2003
        !         allocation_error_requested_upper = requested_upper ! F2003
        if( .not. allocated(allocation_error_requested_lower) ) then
            print *, "BUG: allocatable assignment allocation_error_requested_lower did not succeed!"
            allocate( allocation_error_requested_lower(size(requested_lower)) )
            allocation_error_requested_lower(:) =  requested_lower
        end if
        if( .not. allocated(allocation_error_requested_upper) ) then
            print *, "BUG: allocatable assignment allocation_error_requested_upper did not succeed!"
            allocate( allocation_error_requested_upper(size(requested_upper)) )
            allocation_error_requested_upper(:) = requested_upper
        end if
        
    end function allocation_error_constructor
#endif
    
    subroutine allocation_error_write_to( info, unit, prefix, suffix )
        class(allocation_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        integer :: i
        
        ! TODO: remove workarround for allocatable derived type components
#ifndef FC_NO_ALLOCATABLE_DTCOMP
        associate( lower=>info%requested_lower, upper=>info%requested_upper )
#else
        associate( lower=>allocation_error_requested_lower, upper=>allocation_error_requested_upper )
#endif
            if( size(lower) == 1 ) then
                write(unit=unit,fmt="(2A,I0,A,I0,A,I0,2A)") prefix, &
                    "Allocating array(", lower, ":", upper, & 
                    ") did not work (error code ", info%error_code, ").", suffix
            else
                write(unit=unit,fmt="(2A)",advance="no") prefix, "Allocating array( "
                do i=1,size(lower)
                    if( i>1 ) write(unit=unit,fmt="(A)",advance="no") ", "
                    write(unit=unit,fmt="(I0,A,I0)",advance="no") lower(i), ":", upper(i) 
                end do
                write(unit=unit,fmt="(A,I0,A,I0,2A)") &
                    " (=", product(upper-lower+1), & 
                    " elements) did not work (error code ", info%error_code, ").", suffix
            end if
        end associate
    end subroutine allocation_error_write_to
    
    subroutine error_code_error_write_to( info, unit, prefix, suffix )
        class(error_code_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        write(unit=unit,fmt="(2A,I0,2A)") prefix, & 
            "Encountered error code ", info%error_code, ".", suffix
        
    end subroutine error_code_error_write_to
    
    subroutine iostat_error_write_to( info, unit, prefix, suffix )
        use ISO_FORTRAN_ENV, only: IOSTAT_END, IOSTAT_EOR
        
        class(iostat_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        select case( info%error_code )
            case(IOSTAT_END)
                write(unit=unit,fmt="(3A)") prefix, &
                    "End-of-file condition", suffix
            case(IOSTAT_EOR)
                write(unit=unit,fmt="(3A)") prefix, &
                    "End-of-record condition", suffix
            case default
                write(unit=unit,fmt="(2A,I0,2A)") prefix, &
                    "Processor dependent code ", info%error_code, ".", suffix
        end select
        
    end subroutine iostat_error_write_to

    subroutine enum_descriptions_write_to( info, unit, prefix, suffix )
        class(enumeration_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        integer :: i
        logical :: show_default_message
        character(len=20) :: fmt
        character(:), allocatable :: message
        
#ifdef FC_NO_ALLOCATABLE_DTCOMP
        associate( enum=>info%enum, enums=>enumeration_error_enums, &
                 descriptions=>enumeration_error_enum_descriptions )
            if( allocated(enumeration_error_message) ) then
                message = enumeration_error_message
            end if
#else
        associate( enum=>info%enum, enums=>info%enums, descriptions=>info%enum_descriptions )
            if( allocated(info%message) ) then
                message = info%message
            end if
#endif
            write(unit=unit,fmt="(2A,I0,2A)",advance="no") prefix, "Unexpected enumeration value ", enum, ", "
            show_default_message = .true.
            if( allocated(message) ) then
                show_default_message = len_trim(message) > 0
            end if
            if( show_default_message ) then
                write(unit=unit,fmt="(A)",advance="no") "expecting one of the following"
            else
                write(unit=unit,fmt="(A)",advance="no") trim(message)
            end if
            write(unit=unit,fmt="(2A)") ": ", suffix
            write(unit=fmt,fmt="(A,I0,A,I0,A)") "(2A,A", maxval(len_trim(descriptions)), & 
                ",A,I", 1+ceiling(log10(real(maxval(enums)))), ",A)"
            do i=1,size(enums)
                write(unit=unit,fmt=fmt,advance="no") prefix, "- ", descriptions(i), " (=", enums(i), ")", suffix
            end do
        end associate
        
    end subroutine enum_descriptions_write_to
    
end module error_handling_common_errors
