#include "error_handling_oldnew.h"

program username_selection_driver

#ifdef ERROR_HANDLING
    use error_handling
#endif
    use username_selection

    implicit none
    
    integer :: namei
    character(len=8), dimension(:), allocatable :: usernames
    usernames = (/ "        ","Koen    ",  "Koen 123", "Koen.123" /)
    do namei=1,size(usernames)
        call run_testcase(usernames(namei))
    end do
    
    namei = size(usernames)
    write(unit=*,fmt="(A)") "-- Altering the future: the username will be existing --"
    call change_future( username_will_exist=.true.)
    call run_testcase(usernames(namei))

    namei = size(usernames)
    write(unit=*,fmt="(A)") "-- Altering the future: the connection to the server will fail --"
    call change_future( server_connection_will_fail=.true.)
    call run_testcase(usernames(namei))
    
    write(unit=*,fmt=*)

contains

    subroutine run_testcase( name )
        character(len=*), intent(in) :: name
        
        TYPE_ERROR :: inform
        
        write(unit=*,fmt="(3A)") "=> register_username( '", trim(name), "' )"
        call register_username( trim(name), inform )
        if( inform /= 0 ) then
#ifdef ERROR_HANDLING
            call soft_noisy_error( inform )
#else
            call print_error( inform )
#endif
        else
            write(unit=*,fmt="(A)") "OK."
        end if
        write(unit=*,fmt=*)
        
    end subroutine run_testcase

end program username_selection_driver
