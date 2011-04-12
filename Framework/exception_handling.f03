!(doc)TITLE{Exception handling}
!(doc)SUBTITLE{Advanced exception handling in Fortran}
!(doc)VERSION{0.1}
!(doc)AUTHOR{Koen Poppe}
!(doc)DATE{2011-01-27}
!(doc)header

! EXCEPTION_HANDLING
! 
!   Module for advanced exception handling.
! 
! HISTORY
! 
!   20100902 KP - Initial version
!   20101021 KP - Rewritten version
! 
! AUTHOR
! 
!   Koen Poppe, Department of Computer Science,
!   Katholieke Universiteit Leuven, Celestijnenlaan 200A,
!   B-3001 Heverlee, Belgium
!   Email:  Koen.Poppe@cs.kuleuven.be
!
module exception_handling
    
    use exception_handling_exception
    use exception_handling_legacy
    use exception_handling_common_exceptions
    use exception_handling_common_wrappers
    use exception_handling_unit_test
    implicit none
    public
    save
    
end module exception_handling

!(doc)footer