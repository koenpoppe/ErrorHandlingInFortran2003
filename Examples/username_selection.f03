! USERNAME_SELECTION 
! 
!   Utility module for selecting a valid username
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

module username_selection

#ifdef EXCEPTION_HANDLING
    use design_by_contract
    use exception_handling
#endif
    
    implicit none
    private
    save
    
    public :: register_username
    public :: change_future
#ifndef EXCEPTION_HANDLING
    public :: print_error
#endif

    logical :: server_connection_up = .false.
    
    logical :: next_server_connection_fails = .false.
    logical :: next_username_exists = .false.
    
    
#ifdef EXCEPTION_HANDLING
    type, extends(message_exception) :: username_exception
        character(:), allocatable :: username, proposed_alternative
    contains
        procedure :: info_message => exception_info_message_username_exception
    end type username_exception
#endif

contains

    ! Register the given username if it is unique and conform the guidelines.
    ! NOTE: to simulate a duplicate, set < next_username_exists = .true. >
    subroutine register_username( username, ifail )
    
        ! Arguments
        character(len=*), intent(in) :: username
        TYPE_EXCEPTION, intent(out), optional :: ifail
        
        ! Local variables
        TYPE_EXCEPTION :: inform
        character(len=len_trim(username)) :: alternative
        integer :: i
        
#ifndef EXCEPTION_HANDLING
        if( present(ifail) ) then
            ifail = +1 ! soft silent error
        end if
#endif
        
        ! Check preconditions on username
#ifdef EXCEPTION_HANDLING
        if( precondition_fails( ifail, len_trim(username) > 0, &
            "username must not be empty" ) ) return  ! TODO: shorter
#else
        if( len_trim(username) == 0 ) then
            call handle_error( 4000, ifail )
            return
        end if
#endif
        
        ! Check validity of username
        ! - At least one digit
        if( scan(username,"1234567890") == 0 ) then
#ifdef EXCEPTION_HANDLING
            call create_exception(ifail, username_exception( & 
                "must contain one or more digits", & 
                username, username//"1" ), &
                "username_selection:register_username" )
#else
            call handle_error( 4001, ifail )
#endif
            return
        end if
        ! - No spaces
        if( scan(username," ") /= 0 ) then
            alternative = username(1:1)
            do i=2,len(username)
                if( username(i:i) /= " ") then
                    alternative = trim(alternative)//username(i:i)
                end if
            end do
#ifdef EXCEPTION_HANDLING
            call create_exception(ifail, username_exception( &
                "must not contain spaces", &
                username, alternative), &
                "username_selection:register_username" )
#else
            call handle_error( 4002, ifail )
#endif                
            return
        end if
        
        if( open_server_connection(inform) ) then
            
            ! Simulate duplicate usernames
            if( next_username_exists ) then
                next_username_exists = .false.
#ifdef EXCEPTION_HANDLING
                call create_exception(ifail, username_exception( &
                    "is already used", &
                    username, username//"_1" ), &
                    "username_selection:register_username" )
#else
                inform = 4003
                call handle_error( inform, ifail )
#endif
            else
                ! ... register username in the database ...
            end if
            
            call close_server_connection()
           
#ifndef EXCEPTION_HANDLING 
            if( inform /= 0 ) then
                return
            end if
#endif
            
        else
#ifdef EXCEPTION_HANDLING
            call create_exception(ifail, message_exception( & 
                "Could not verify the username"), &
                inform, "username_selection:register_username" )
#else
            call handle_error( 4004, ifail )
#endif
            return
        end if
        
#ifndef EXCEPTION_HANDLING
        if( present(ifail) ) then
            ifail = 0 ! succes
        end if
#endif
    
    end subroutine register_username


    ! Open the connection with the server
    ! NOTE: to simulate failiure, set < next_server_connection_fails = .true. >.
    function open_server_connection( ifail ) result( is_open )
        
        ! Arguments
        TYPE_EXCEPTION, intent(out), optional :: ifail
        logical :: is_open
        
#ifndef EXCEPTION_HANDLING
        if( present(ifail) ) then
            ifail = +1 ! soft silent error
        end if
#endif
        
        ! ... open the server connection ...
        
        ! Simulation of connection failiure 
        if( next_server_connection_fails ) then
            next_server_connection_fails = .false.
            server_connection_up = .false.
#ifdef EXCEPTION_HANDLING
            call create_exception(ifail, message_exception( &
                "Connection to server could not be established") )
#else
            call handle_error( 5001, ifail )
#endif
        else
            server_connection_up = .true.
        end if
        
        is_open = server_connection_up
        
#ifndef EXCEPTION_HANDLING
        if( present(ifail) ) then
            ifail = 0 ! succes
        end if
#endif
        
    end function open_server_connection


    ! Coses the connection with the server
    subroutine close_server_connection()
        
        ! ... close the server connection ...
        
    end subroutine close_server_connection


    subroutine change_future( server_connection_will_fail, username_will_exist )
        logical, intent(in), optional :: server_connection_will_fail
        logical, intent(in), optional :: username_will_exist
        
        if( present(server_connection_will_fail) ) &
            next_server_connection_fails = server_connection_will_fail
        if( present(username_will_exist) ) &
            next_username_exists = username_will_exist
        
    end subroutine change_future

#ifdef EXCEPTION_HANDLING
    subroutine exception_info_message_username_exception( info, message )
        class(username_exception), intent(in) :: info
        character(len=*), intent(out) :: message
        
        write(unit=message,fmt=*) "User name '", info%username, & 
            "' is invalid because it ", info%message, &
            ". Proposed alternative: '", trim(info%proposed_alternative) ,"'."
        
    end subroutine exception_info_message_username_exception
#else
    subroutine print_error( ifail )
        integer, intent(in) :: ifail
        select case( ifail )
            case( 0 )
                print *, "*** Internal error: Not an error, should never be reported..."
                
            ! username_selection error codes
            case( 4000 )
                print *, "Precondition violated: username must not be empty"
            case( 4001 ) 
                print *, "Username must contain one or more digits"
            case( 4002 )
                print *, "Username must not contain spaces"
            case( 4003 )
                print *, "Username is already used"
            case( 4004 )
                print *, "Could not verify the username"
            
            ! Server connection error codes
            case( 5001 )
                print *, "Connection to server could not be established"
            
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

end module username_selection
