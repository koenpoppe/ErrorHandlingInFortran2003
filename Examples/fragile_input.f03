! FRAGILE_INPUT 
! 
!   Incomplete set of methods for pseudo fault tolerant user input.
!   
!   This illustrates the use of the <exception_handling> module.
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
#include "exception_handling.h"

module fragile_input

#ifdef EXCEPTION_HANDLING
    use design_by_contract
    use exception_handling
#endif
    
    use ISO_FORTRAN_ENV, only: INPUT_UNIT
#ifndef EXCEPTION_HANDLING
    use ISO_FORTRAN_ENV, only: IOSTAT_END, IOSTAT_EOR
#endif
    
    implicit none
    private
    save
    
    public :: fragile_integer_input, integer_input
#ifndef EXCEPTION_HANDLING
    public :: print_error
#endif

#ifdef EXCEPTION_HANDLING
    type, extends(exception_info) :: fragile_input_exception
        integer :: nb_attempts
        character(:),allocatable :: input_type
    contains
        procedure, pass :: info_message => exception_info_message_fragile_input_exception
    end type fragile_input_exception
#endif
    
contains

    ! Tries to read an integer number, allowing the user <nb_attempts> attempts.
    ! Optionally, <question> describes the meaning of the number to input.
    subroutine fragile_integer_input( number, unit, nb_attempts, question, ifail, verbose )
        
        ! Arguments
        integer, intent(out) :: number
        integer, intent(in), optional :: unit, nb_attempts
        character(len=*), intent(in), optional :: question
        TYPE_EXCEPTION, intent(out), optional :: ifail ! NOTE: was <integer>
        logical, intent(in), optional :: verbose
        
        ! Local variables
        integer :: the_nb_attempts, attempt, lunit
        TYPE_EXCEPTION :: inform
        logical :: lverbose
        
#ifndef EXCEPTION_HANDLING
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
#ifdef EXCEPTION_HANDLING
            if( precondition_fails(ifail,nb_attempts>=1, &
                "<nb_attempts> must be positive") ) return ! TODO: shorter
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
#ifndef EXCEPTION_HANDLING
                if( present(ifail) ) then
                    ifail = 0 ! succes
                end if
#endif
                return ! successfully read integer
            else
                if( attempt < the_nb_attempts ) then
#ifdef EXCEPTION_HANDLING
                    ! We allow a new trail, so discard the exception
                    call discard_exception( inform )
#endif
                    if( verbose ) then
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
#ifdef EXCEPTION_HANDLING
        ! create_exception exception including the last failiure message from integer_input
        call create_exception(ifail, & 
            fragile_input_exception(the_nb_attempts,"integer"), &
            inform,"fragile_input:fragile_integer_input")
#else
        call handle_error( 5001, ifail )
#endif
        
    end subroutine fragile_integer_input


    ! Exception enhanced integer reading
    subroutine integer_input( number, unit, ifail )
        use ISO_FORTRAN_ENV
        
        integer, intent(out) :: number
        integer, intent(in) :: unit
        TYPE_EXCEPTION, intent(out), optional :: ifail ! NOTE: was <integer>
        
        integer :: iostat
        
#ifndef EXCEPTION_HANDLING
        ! Make sure that handle_error behaves as expected
        if( present(ifail) ) then
            ifail = +1 ! soft silent error
        end if
#endif        
        
        read(unit=unit,fmt=*,iostat=iostat) number
        
#ifdef EXCEPTION_HANDLING
        if( iostat /= 0 ) then
            call create_exception(ifail,iostat_exception(iostat),"fragile_input:integer_input")
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

#ifdef EXCEPTION_HANDLING
    subroutine exception_info_message_fragile_input_exception( info, message )
        class(fragile_input_exception), intent(in) :: info
        character(len=*), intent(out) :: message
        
        write(unit=message,fmt="(3A,I0,A)") & 
            "Failed reading a valid ", info%input_type, " within ", & 
            info%nb_attempts, " attempts"
    
    end subroutine exception_info_message_fragile_input_exception
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
