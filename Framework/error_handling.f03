!(doc)TITLE{Error handling}
!(doc)SUBTITLE{Advanced error handling in Fortran}
!(doc)VERSION{0.1}
!(doc)AUTHOR{Koen Poppe}
!(doc)DATE{2011-01-27}
!(doc)header

! ERROR_HANDLING
! 
!   Module for advanced error handling.
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
module error_handling
    
    use error_handling_error
    use error_handling_legacy
    use error_handling_common_errors
    use error_handling_common_wrappers
    use error_handling_unittest
    implicit none
    public
    save
    
end module error_handling

!(doc)footer