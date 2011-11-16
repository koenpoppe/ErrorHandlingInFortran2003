!!! 20110330 KP - From error_handling_error
! Reason: not really useful

    ! 6. Comparing
    public :: operator(.EQ.)
    interface operator(.EQ.)
        module procedure error_eq_error
        module procedure error_info_eq_error_info
    end interface
    
    public :: operator(.NE.)
    interface operator(.NE.)
        module procedure error_neq_error
        module procedure error_info_neq_error_info
    end interface

contains

    ! 6. Comparing
    function error_eq_error( exc, cxe ) result( equal )
        class(error), intent(in) :: exc, cxe
        logical :: equal
        
        equal = .false.
        
        ! Info
        if( associated(exc%info) .or. associated(cxe%info) ) then
            if( associated(exc%info) .and. associated(cxe%info) ) then
                print *, "TODO: compare both info's ..."
                !if( exc%info /= cxe%info ) return
            else
                return
            end if
        end if
        
        ! Method
        if( allocated(exc%method) .or. allocated(cxe%method) ) then
            if( allocated(exc%method) .or. allocated(cxe%method) ) then
                if( exc%method /= cxe%method ) return
            else
                return
            end if
        end if
        
        ! Reason
        if( associated(exc%reason) .or. associated(cxe%reason) ) then
            if( associated(exc%reason) .and. associated(cxe%reason) ) then
                print *, "TODO: compare both reasons ..."
                !if( exc%reason /= cxe%reason ) return
            else
                return
            end if
        end if
        
        equal = .true.
        
    end function error_eq_error
    function error_neq_error( exc, cxe ) result( not_equal )
        class(error), intent(in) :: exc, cxe
        logical :: not_equal
        not_equal = .not. ( exc == cxe )
    end function error_neq_error
    
    function error_info_eq_error_info( exc, cxe ) result( equal )
        class(error_info), intent(in) :: exc, cxe
        logical :: equal

#ifndef FC_NO_SAME_TYPE_AS_SUPPORT
        if( .not. SAME_TYPE_AS( exc, cxe ) ) then
            equal = .false.
        else
            print *, "error_info_eq_error_info: different types"
            equal = .true. ! TODO
        end if
#else
        print *, "error_info_eq_error_info: no FC_NO_SAME_TYPE_AS_SUPPORT"
        equal = .true.
#endif
        
    end function error_info_eq_error_info
    function error_info_neq_error_info( exc, cxe ) result( not_equal )
        class(error_info), intent(in) :: exc, cxe
        logical :: not_equal
        not_equal = .not. ( exc == cxe )
    end function error_info_neq_error_info