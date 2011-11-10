#include "error_handling_oldnew.h"

program fragile_input_driver

    use fragile_input
    use error_handling

    implicit none
    
    integer, parameter :: unit = 45941
    logical, parameter :: use_scratch = .true.
    
    call prepare_input(unit)
    write(unit=unit,fmt=*) "Next is valid"
    write(unit=unit,fmt=*) "111"
    call run_testcase(unit)
    
    call prepare_input(unit)
    write(unit=unit,fmt=*) "Five"
    write(unit=unit,fmt=*) "Attempts"
    write(unit=unit,fmt=*) "Will"
    write(unit=unit,fmt=*) "Be"
    write(unit=unit,fmt=*) "Fatal"
    call run_testcase(unit)
    
    call prepare_input(unit)
    write(unit=unit,fmt=*) "End Of Input"
    call run_testcase(unit)
    
contains

    subroutine prepare_input( unit )
        integer, intent(in) ::  unit 
        if( use_scratch ) then
            open(unit=unit,status="scratch")
        else
            open(unit=unit,status="replace",file="fragile_input_driver.txt",action="write")
        end if
    end subroutine prepare_input

    subroutine run_testcase( unit )
        integer, intent(in) ::  unit 
        integer :: number
        TYPE_ERROR :: inform
        
        if( use_scratch ) then
            rewind(unit=unit)
        else
            close(unit=unit)
            open(unit=unit,status="old",file="fragile_input_driver.txt",action="read")
        end if

        call fragile_integer_input( number, unit, ifail=inform, verbose=.true., nb_attempts=5 )
        close(unit=unit)
        
        if( inform == 0 ) then
            print *, "=> Number = ", number
        else
#ifdef ERROR_HANDLING
            call soft_noisy_error( inform )
#else
            call print_error( inform )
#endif
        end if
        write(unit=*,fmt=*)
        
    end subroutine run_testcase
    
end program fragile_input_driver
