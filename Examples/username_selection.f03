! USERNAME_SELECTION 
! 
!   Utility module for selecting a valid username
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

module username_selection

#ifdef ERROR_HANDLING
    use design_by_contract
    use error_handling
#endif
    
    implicit none
    private
    save
    
    public :: register_username
    public :: change_future
#ifndef ERROR_HANDLING
    public :: print_error
#endif

    logical :: server_connection_up = .false.
    
    logical :: next_server_connection_fails = .false.
    logical :: next_username_exists = .false.
    
    
#ifdef ERROR_HANDLING
    type, extends(message_error) :: username_error
#ifdef FC_FIXED_LENGTH_CHARACTERSTRINGS
        character(len=MAX_CHARACTER_LEN) :: username = "", proposed_alternative = ""
#else
        character(:), allocatable :: username, proposed_alternative
#endif
    contains
        procedure :: write_to => username_error_write_to
    end type username_error
#endif

#ifdef FC_NO_DT_CONSTRUCTOR
    public :: username_error
    interface username_error
        module procedure username_error_constructor
    end interface username_error
#endif

contains

    ! Register the given username if it is unique and conform the guidelines.
    ! NOTE: to simulate a duplicate, set < next_username_exists = .true. >
    subroutine register_username( username, ifail )
    
        ! Arguments
        character(len=*), intent(in) :: username
        TYPE_ERROR, intent(out), optional :: ifail
        
        ! Local variables
        TYPE_ERROR :: inform
        character(len=len_trim(username)) :: alternative
        integer :: i
        
#ifndef ERROR_HANDLING
        if( present(ifail) ) then
            ifail = +1 ! soft silent error
        end if
#endif
        
        ! Check preconditions on username
#ifdef ERROR_HANDLING
        call assert_gt( len_trim(username), 0, "username must not be empty", ifail, a_name="len_trim(username)" )
        if( ifail /= 0 ) return
#else
        if( len_trim(username) == 0 ) then
            call handle_error( 4000, ifail )
            return
        end if
#endif
        
        ! Check validity of username
        ! - At least one digit
        if( scan(username,"1234567890") == 0 ) then
#ifdef ERROR_HANDLING
            call create_error(ifail, username_error( & 
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
#ifdef ERROR_HANDLING
            call create_error(ifail, username_error( &
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
#ifdef ERROR_HANDLING
                call create_error(ifail, username_error( &
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
           
#ifndef ERROR_HANDLING 
            if( inform /= 0 ) then
                return
            end if
#endif
            
        else
#ifdef ERROR_HANDLING
            call create_error(ifail, message_error( & 
                "Could not verify the username"), &
                inform, "username_selection:register_username" )
#else
            call handle_error( 4004, ifail )
#endif
            return
        end if
        
#ifndef ERROR_HANDLING
        if( present(ifail) ) then
            ifail = 0 ! succes
        end if
#endif
    
    end subroutine register_username


    ! Open the connection with the server
    ! NOTE: to simulate failiure, set < next_server_connection_fails = .true. >.
    function open_server_connection( ifail ) result( is_open )
        
        ! Arguments
        TYPE_ERROR, intent(out), optional :: ifail
        logical :: is_open
        
#ifndef ERROR_HANDLING
        if( present(ifail) ) then
            ifail = +1 ! soft silent error
        end if
#endif
        
        ! ... open the server connection ...
        
        ! Simulation of connection failiure 
        if( next_server_connection_fails ) then
            next_server_connection_fails = .false.
            server_connection_up = .false.
#ifdef ERROR_HANDLING
            call create_error(ifail, message_error( &
                "Connection to server could not be established") )
#else
            call handle_error( 5001, ifail )
#endif
        else
            server_connection_up = .true.
        end if
        
        is_open = server_connection_up
        
#ifndef ERROR_HANDLING
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

#ifdef ERROR_HANDLING
#ifdef FC_NO_DT_CONSTRUCTOR
    function username_error_constructor( message, username, proposed_alternative ) result( info )
        character(len=*), intent(in) :: message, username, proposed_alternative
        type(username_error) :: info
        
        info%message = message
        info%username = username
        info%proposed_alternative = proposed_alternative
        
    end function username_error_constructor
#endif

    subroutine username_error_write_to( info, unit, prefix, suffix )
        class(username_error), intent(in) :: info
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, suffix
        
        write(unit=unit,fmt="(9A)") prefix, "User name '", trim(info%username), & 
            "' is invalid because it ", trim(info%message), &
            ". Proposed alternative: '", trim(info%proposed_alternative) ,"'.", suffix
        
    end subroutine username_error_write_to
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
