! FRAGILE_INPUT 
! 
!   Incomplete set of methods for pseudo fault tolerant user input.
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

module fragile_input

#ifdef ERROR_HANDLING
    use design_by_contract
    use error_handling
#endif
    
    use ISO_FORTRAN_ENV, only: INPUT_UNIT
#ifndef ERROR_HANDLING
    use ISO_FORTRAN_ENV, only: IOSTAT_END, IOSTAT_EOR
#endif
    
    implicit none
    private
    save
    
    public :: fragile_integer_input, integer_input
#ifndef ERROR_HANDLING
    public :: print_error
#endif

#ifdef ERROR_HANDLING
    type, extends(error_info) :: fragile_input_error
        integer :: nb_attempts
#ifdef FC_FIXED_LENGTH_CHARACTERSTRINGS
        character(MAX_CHARACTER_LEN) :: input_type = ""
#else
        character(:), allocatable :: input_type
#endif
    contains
        procedure, pass :: write_to => fragile_input_error_write_to
    end type fragile_input_error
#endif
    
contains

    ! Tries to read an integer number, allowing the user <nb_attempts> attempts.
    ! Optionally, <question> describes the meaning of the number to input.
    subroutine fragile_integer_input( number, unit, nb_attempts, question, ifail, verbose )
        
        ! Arguments
        integer, intent(out) :: number
        integer, intent(in), optional :: unit, nb_attempts
        character(len=*), intent(in), optional :: question
        TYPE_ERROR, intent(out), optional :: ifail ! NOTE: was <integer>
        logical, intent(in), optional :: verbose
        
        ! Local variables
        integer :: the_nb_attempts, attempt, lunit
        TYPE_ERROR :: inform
        logical :: lverbose
        
#ifndef ERROR_HANDLING
        ! Make sure that handle_error behaves as expected
        if( present(ifail) ) then
            ifail = +1 ! soft silent error
        end if
#endif
        
        ! Optional unit number
        if( present(unit) ) then
            lunit = unit
        else
            lunit = INPUT_UNIT
        end if
        
        ! Optional number of attempts
        if( present(nb_attempts) ) then
#ifdef ERROR_HANDLING
            call assert_ge( nb_attempts, 1, "<nb_attempts> must be positive", ifail )
            if( ifail /= 0 ) return
#else
            if( nb_attempts<1 ) then
                call handle_error( 5000, ifail )
            end if
#endif
            
            the_nb_attempts = nb_attempts
        else
            the_nb_attempts = 3 ! default
        end if
        
        ! Optional verbose mode
        if( present(verbose) ) then
            lverbose = verbose
        else
            lverbose = .false.
        end if
        
        ! Several attempts of reading an integer
        do attempt = 1, the_nb_attempts
            if( present(question) ) then
                write(unit=*,fmt="(A)",advance="no") question
            end if
            
            call integer_input( number,lunit,inform )
            
            ! Return if the integer was read correctly -> return
            ! NOTE: conventional way of checking error condition or not
            if( inform == 0 ) then 
#ifndef ERROR_HANDLING
                if( present(ifail) ) then
                    ifail = 0 ! succes
                end if
#endif
                return ! successfully read integer
            else
                if( attempt < the_nb_attempts ) then
#ifdef ERROR_HANDLING
                    ! We allow a new trail, so discard the error
                    call discard_error( inform )
#endif
                    if( lverbose ) then
                        write(unit=*,fmt="(A,I0,A)",advance="no") &
                            "Invalid integer try again... (", &
                            the_nb_attempts-attempt, " attempt"
                        if( the_nb_attempts-attempt > 1 ) & 
                            write(unit=*,fmt="(A)",advance="no") "s"
                        write(unit=*,fmt="(A)") " left)"
                    end if
                end if
            end if
        end do
        
        ! All attempts unsucessfull
#ifdef ERROR_HANDLING
        ! create_error error including the last failiure message from integer_input
        call create_error(ifail, & 
            fragile_input_error(the_nb_attempts,"integer"), &
            inform,"fragile_input:fragile_integer_input")
#else
        call handle_error( 5001, ifail )
#endif
        
    end subroutine fragile_integer_input


    ! Error enhanced integer reading
    subroutine integer_input( number, unit, ifail )
        use ISO_FORTRAN_ENV
        
        integer, intent(out) :: number
        integer, intent(in) :: unit
        TYPE_ERROR, intent(out), optional :: ifail ! NOTE: was <integer>
        
        integer :: iostat
        
#ifndef ERROR_HANDLING
        ! Make sure that handle_error behaves as expected
        if( present(ifail) ) then
            ifail = +1 ! soft silent error
        end if
#endif        
        
        read(unit=unit,fmt=*,iostat=iostat) number
        
#ifdef ERROR_HANDLING
        if( iostat /= 0 ) then
            call create_error(ifail,iostat_error(iostat),"fragile_input:integer_input")
        end if
#else
        if( iostat /= 0 ) then
            call handle_error( 82000+iostat, ifail )
        else
            if( present(ifail) ) then
                ifail = 0 ! succes
            end if
        end if
#endif
        
    end subroutine integer_input

#ifdef ERROR_HANDLING
    subroutine fragile_input_error_write_to( info, unit, prefix, suffix )
        class(fragile_input_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        write(unit=unit,fmt="(4A,I0,2A)") prefix, & 
            "Failed reading a valid ", trim(info%input_type), " within ", & 
            info%nb_attempts, " attempts", suffix
    
    end subroutine fragile_input_error_write_to
#else
    subroutine print_error( ifail )
        integer, intent(in) :: ifail
        select case( ifail )
            case( 0 )
                print *, "*** Internal error: Not an error, should never be reported..."
                
            ! fragile_input error codes
            case( 5000 )
                print *, "Precondition violated: <nb_attempts> must be positive"
            case( 5001 )
                print *, "All attempts unsucessfull"
                
            ! io-related
            case( 82000+IOSTAT_END )
                print *, "End-of-file condition encountered"
            case( 82000+IOSTAT_EOR )
                print *, "End-of-record condition encountered"
            case default
                print *, "Processor dependent code ", (ifail-82000)
            
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

end module fragile_input
