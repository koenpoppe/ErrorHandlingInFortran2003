!!! 20110330 KP - From exception_handling_exception
! Reason: not really useful

    ! 6. Comparing
    public :: operator(.EQ.)
    interface operator(.EQ.)
        module procedure exception_eq_exception
        module procedure exception_info_eq_exception_info
    end interface
    
    public :: operator(.NE.)
    interface operator(.NE.)
        module procedure exception_neq_exception
        module procedure exception_info_neq_exception_info
    end interface

contains

    ! 6. Comparing
    function exception_eq_exception( exc, cxe ) result( equal )
        class(exception), intent(in) :: exc, cxe
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
        
    end function exception_eq_exception
    function exception_neq_exception( exc, cxe ) result( not_equal )
        class(exception), intent(in) :: exc, cxe
        logical :: not_equal
        not_equal = .not. ( exc == cxe )
    end function exception_neq_exception
    
    function exception_info_eq_exception_info( exc, cxe ) result( equal )
        class(exception_info), intent(in) :: exc, cxe
        logical :: equal

#ifdef FC_SUPP_SAME_TYPE_AS
        if( .not. SAME_TYPE_AS( exc, cxe ) ) then
            equal = .false.
        else
            print *, "exception_info_eq_exception_info: different types"
            equal = .true. ! TODO
        end if
#else
        print *, "exception_info_eq_exception_info: no FC_SUPP_SAME_TYPE_AS"
        equal = .true.
#endif
        
    end function exception_info_eq_exception_info
    function exception_info_neq_exception_info( exc, cxe ) result( not_equal )
        class(exception_info), intent(in) :: exc, cxe
        logical :: not_equal
        not_equal = .not. ( exc == cxe )
    end function exception_info_neq_exception_info