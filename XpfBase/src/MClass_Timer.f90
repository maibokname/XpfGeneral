
MODULE MClass_Timer

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains a *Timer* class that can be used to measure an elapsed time.

!** USE STATEMENTS:
    USE MBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Timer, SECONDS, MILLI_SEC, MICRO_SEC, NANO_SEC

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! parameters for timer state
    tSInt32, PARAMETER  :: STATE_RUNNING = 1
    tSInt32, PARAMETER  :: STATE_PAUSED  = 2
    tSInt32, PARAMETER  :: STATE_STOPPED = 3
    ! parameters for elapsed time unit
    tSInt32, PARAMETER  :: SECONDS   = 1
    tSInt32, PARAMETER  :: MILLI_SEC = 2
    tSInt32, PARAMETER  :: MICRO_SEC = 3
    tSInt32, PARAMETER  :: NANO_SEC  = 4

!** DERIVED TYPE DEFINITIONS
    !> a timer type that can be used to measure an elapsed time
    TYPE Timer
        PRIVATE
        tSInt64     :: Frequency    ! set to the number of processor clock counts per second
        tSInt64     :: StartTime    ! set to the processor clock by *Start* procedure
        tSInt64     :: TimePaused   ! the elapsed time the clock paused
        tSInt64     :: PauseTime    ! set to the processor clock by *Pause* procedure
        tSInt64     :: StopTime     ! set to the processor clock by *Stop* procedure
        tSInt32     :: State        ! current state of the timer
    CONTAINS
        !> **Type-Bound Subroutine**: Start <br>
        ! **Purpose**:  To start (or reset) the timer <br>
        !  **Usage**: <br>
        !   --->    CALL BenchTime%Start()
        PROCEDURE   :: Start        => Timer_Start
        !> **Type-Bound Subroutine**: Stop <br>
        ! **Purpose**:  To stop the timer <br>
        !  **Usage**: <br>
        !   --->    CALL BenchTime%Stop()
        PROCEDURE   :: Stop         => Timer_Stop
        !> **Type-Bound Subroutine**: Pause <br>
        ! **Purpose**:  To pause the timer <br>
        !  **Usage**: <br>
        !   --->    CALL BenchTime%Pause()
        PROCEDURE   :: Pause        => Timer_Pause
        !> **Type-Bound Subroutine**: Resume <br>
        ! **Purpose**:  To resume the timer <br>
        !  **Usage**: <br>
        !   --->    CALL BenchTime%Resume()
        PROCEDURE   :: Resume       => Timer_Resume
        !> **Type-Bound Subroutine**: ElapsedTime <br>
        ! **Purpose**:  ! To get elapsed time of the timer <br>
        !  **Usage**: <br>
        !   --->    CALL BenchTime%ElapsedTime()            ! default unit time (seconds) <br>
        !   --->    CALL BenchTime%ElapsedTime(MILLI_SEC)   ! unit time in milliseconds <br>
        !   --->    CALL BenchTime%ElapsedTime(MICRO_SEC)   ! unit time in microseconds
        PROCEDURE   :: ElapsedTime  => Timer_ElapsedTime
    END TYPE Timer

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Timer_Start(Clock)

!** PURPOSE OF THIS SUBROUTINE:
    !! To start (or reset) the timer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Timer), INTENT(OUT)   :: Clock

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! initialize timer components
    Clock%Frequency  = 0_kInt64
    Clock%StartTime  = 0_kInt64
    Clock%TimePaused = 0_kInt64
    Clock%PauseTime  = 0_kInt64
    Clock%StopTime   = 0_kInt64
    Clock%State      = STATE_RUNNING

    ! call system clock to get current clock count and the count rate
    CALL SYSTEM_CLOCK(Count=Clock%StartTime, Count_Rate=Clock%Frequency)

    RETURN

END SUBROUTINE Timer_Start

!******************************************************************************

SUBROUTINE Timer_Stop(Clock)

!** PURPOSE OF THIS SUBROUTINE:
    !! To stop the timer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Timer), INTENT(INOUT) :: Clock

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (Clock%State == STATE_RUNNING) THEN
        CALL SYSTEM_CLOCK(Count=Clock%StopTime)
    ELSEIF (Clock%State == STATE_PAUSED) THEN
        Clock%StopTime = Clock%PauseTime
    END IF

    Clock%State = STATE_STOPPED

    RETURN

END SUBROUTINE Timer_Stop

!******************************************************************************

SUBROUTINE Timer_Pause(Clock)

!** PURPOSE OF THIS SUBROUTINE:
    !! To pause the timer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Timer), INTENT(INOUT) :: Clock

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (Clock%State == STATE_RUNNING) THEN
        CALL SYSTEM_CLOCK(Count=Clock%PauseTime)
        Clock%State = STATE_PAUSED
    END IF

    RETURN

END SUBROUTINE Timer_Pause

!******************************************************************************

SUBROUTINE Timer_Resume(Clock)

!** PURPOSE OF THIS SUBROUTINE:
    !! To resume the timer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Timer), INTENT(INOUT) :: Clock

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: CurTime, TimePaused

! FLOW

    IF (Clock%State == STATE_PAUSED) THEN
        CALL SYSTEM_CLOCK(Count=CurTime)
        TimePaused = CurTime - Clock%PauseTime
        Clock%TimePaused = Clock%TimePaused + TimePaused
        Clock%State = STATE_RUNNING
    END IF

    RETURN

END SUBROUTINE Timer_Resume

!******************************************************************************

FUNCTION Timer_ElapsedTime(Clock, Unit) RESULT(Time)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get elapsed time of the timer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Timer), INTENT(INOUT) :: Clock    !! timer object
    tSInt32,      OPTIONAL      :: Unit
    !^ unit of the elapsed time <br>
    ! = 1 if unit time is in seconds (default) <br>
    ! = 2 if unit time is in milliseconds <br>
    ! = 3 if unit time is in microseconds <br>
    ! = 4 if unit time is in nanoseconds <br>
    ! Note that some unit time may not be accurate/available because
    !   the processor clock and its counts are system dependent.
    tRealDP                     :: Time     !! elapsed time

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: Counts    ! elapsed counts

! FLOW

    ! stop the clock if it has not yet been stopped
    IF (.NOT.(Clock%State /= STATE_STOPPED)) CALL Clock%Stop()
    
    ! get the elapsed counts/ticks
    Counts = (Clock%StopTime - Clock%StartTime - Clock%TimePaused)
    
    ! get the elapsed time in seconds
    Time = REAL(Counts, KIND=kDouble)/REAL(Clock%Frequency, KIND=kDouble)
    
    ! get the elapsed time according to the specified unit
    IF (PRESENT(Unit)) THEN
        SELECT CASE (Unit)
        CASE (MILLI_SEC)
            Time = Time*1000.0_kDouble
        CASE (MICRO_SEC)
            Time = Time*1000000.0_kDouble
        CASE (NANO_SEC)
            Time = Time*1000000000.0_kDouble
        CASE DEFAULT
            ! for seconds, do nothing
        END SELECT
    END IF

    RETURN

END FUNCTION Timer_ElapsedTime

!******************************************************************************

END MODULE MClass_Timer
    
!******************************************************************************
