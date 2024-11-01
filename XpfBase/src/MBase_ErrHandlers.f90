
MODULE MBase_ErrHandlers
       
!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains routines and parameters for error-handling tasks.   The error messages
!   are written to a (default) log file named 'XpfLibError.Log'.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil,   ONLY: ToChar => ToDecStrSigned

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! parameters indicating error level
    PUBLIC :: ErrNone
    PUBLIC :: ErrWarning
    PUBLIC :: ErrSevere
    PUBLIC :: ErrFatal
    ! advanced-level procedures to display error message(s)
    PUBLIC :: Handle_ErrDealloc
    PUBLIC :: Handle_ErrAlloc
    PUBLIC :: Handle_ErrOpen
    PUBLIC :: Handle_ErrLevel
    ! basic-level procedures to display an error message
    PUBLIC :: DisplayMessage
    PUBLIC :: DisplayWarningError
    PUBLIC :: DisplaySevereError
    PUBLIC :: DisplayFatalError
    PUBLIC :: DisplayContinueError
    ! miscellaneous error-related procedures
    PUBLIC :: CloseErrorFile
    PUBLIC :: CloseMiscOpenFiles
    PUBLIC :: SetStopOnError
    PUBLIC :: AbortProgram

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! parameters for error level
    tSInt32,  PARAMETER :: ErrNone    = 0   !! no error level
    tSInt32,  PARAMETER :: ErrWarning = 1   !! warning error level
    tSInt32,  PARAMETER :: ErrSevere  = 2   !! severe error level
    tSInt32,  PARAMETER :: ErrFatal   = 3   !! fatal error level
    ! name of error file
    tCharParam          :: ErrFilename = 'XpfLibError.Log'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    tLogical    :: DisplayCont = FalseVal
    tLogical    :: StopOnError = FalseVal
    tLogical    :: FatalError  = FalseVal

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!**************************************************************************************

SUBROUTINE DisplayErrorMessage(ErrorMessage,OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^  To display the error message on the "standard error output" unit
    !   or the indicated file unit number if specified.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,         INTENT(IN)       :: ErrorMessage
    tSInt32, OPTIONAL, INTENT(INOUT)    :: OutUnit
    !^ user specified unit number. <br>
    ! If not present, the message is written to the default log file. <br>
    ! If present and an associated file is open, the message is written to the associated file. <br>
    ! Otherwise, the message is written to a file named 'UserErrLog.Txt'.
          
!** SUBROUTINE PARAMETER DEFINITIONS:
    tCharParam  :: ErrorFormat = '(2X,A)'
  
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tLogical    :: FileOpened
    tLogical    :: Existing
    tSInt32     :: IOS
    tSInt32     :: ErrorUnit

!** FLOW
    
    IF (PRESENT(OutUnit)) THEN
        
        ! first, check whether the specified unit number is connected to an opened file
        INQUIRE(UNIT=OutUnit, OPENED=FileOpened, IOStat=IOS)
        
        IF (FileOpened.AND.(IOS == 0)) THEN

            ! simply write message to the specified file unit number
            WRITE(OutUnit, ErrorFormat) TRIM(ErrorMessage)

        ELSE

            ! open file
            OPEN(OutUnit, File='UserErrLog.Txt')

            ! then write message to the specified file unit number
            WRITE(OutUnit, ErrorFormat) TRIM(ErrorMessage)

        END IF
        
    ELSE
        
        ! first, check whether the 'XpfLibError.Log' file is opened
        INQUIRE(FILE=ErrFilename, EXIST=Existing, OPENED=FileOpened, IOStat=IOS)
        
        IF (FileOpened.AND.(IOS == 0)) THEN
            
            ! get existing unit number for the opened file
            INQUIRE(FILE=ErrFilename, NUMBER=ErrorUnit)
            
            ! then write message to the 'XpfLibError.Log' file
            WRITE(ErrorUnit, ErrorFormat) TRIM(ErrorMessage)
            
        ELSE
            
            ! open file
            IF (Existing) THEN
                OPEN(NEWUNIT=ErrorUnit, FILE=ErrFilename, POSITION='APPEND')
                ! write separation indicator to the 'XpfLibError.Log' file
                WRITE(ErrorUnit, '(A)') '------------------------------------------------------------'
                WRITE(ErrorUnit, '(A)') ''
            ELSE
                OPEN(NEWUNIT=ErrorUnit, FILE=ErrFilename)
                ! write heading to the 'XpfLibError.Log' file
                WRITE(ErrorUnit, '(A)') '  XpfLib Error Logging'
                WRITE(ErrorUnit, '(A)') '  Library Version = 0.1.0'
                WRITE(ErrorUnit, '(A)') '------------------------------------------------------------'
                WRITE(ErrorUnit, '(A)') ''
            END IF

            ! then write message to the 'XpfLibError.Log' file
            WRITE(ErrorUnit, ErrorFormat) TRIM(ErrorMessage)
            
        END IF
        
    ENDIF

    RETURN

END SUBROUTINE DisplayErrorMessage

!******************************************************************************

SUBROUTINE DisplayWarningError(ErrorMessage,OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^  To display a 'Warning Error' message.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,         INTENT(IN)       :: ErrorMessage
    tSInt32, OPTIONAL, INTENT(INOUT)    :: OutUnit
    !^ user specified unit number. <br>
    ! If not present, the message is written to the default log file. <br>
    ! If present and an associated file is open, the message is written to the associated file. <br>
    ! Otherwise, the message is written to a file named 'UserErrLog.Txt'.
 
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Verbose) CALL DisplayErrorMessage(' ++ WARNING ++ : ' // ErrorMessage, OutUnit)
    DisplayCont = FalseVal

    RETURN

END SUBROUTINE DisplayWarningError

!******************************************************************************

SUBROUTINE DisplayFatalError(ErrorMessage,OutUnit)

!** URPOSE OF THIS SUBROUTINE:
    !^  To display a 'Fatal Error' message.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,         INTENT(IN)       :: ErrorMessage
    tSInt32, OPTIONAL, INTENT(INOUT)    :: OutUnit
    !^ user specified unit number. <br>
    ! If not present, the message is written to the default log file. <br>
    ! If present and an associated file is open, the message is written to the associated file. <br>
    ! Otherwise, the message is written to a file named 'UserErrLog.Txt'.
      
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na
  
!** FLOW

    ! display message
    CALL DisplayErrorMessage(' ++ FATAL ++ : ' // ErrorMessage, OutUnit)
    
    ! set flags
    FatalError = TrueVal
    DisplayCont = TrueVal

    RETURN

END SUBROUTINE DisplayFatalError

!******************************************************************************

SUBROUTINE DisplaySevereError(ErrorMessage,OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^  To display a 'Severe Error' message.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,         INTENT(IN)       :: ErrorMessage
    tSInt32, OPTIONAL, INTENT(INOUT)    :: OutUnit
    !^ user specified unit number. <br>
    ! If not present, the message is written to the default log file. <br>
    ! If present and an associated file is open, the message is written to the associated file. <br>
    ! Otherwise, the message is written to a file named 'UserErrLog.Txt'.

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na
  
!** FLOW

    CALL DisplayErrorMessage(' ++ SEVERE ++ : ' // ErrorMessage, OutUnit)
    DisplayCont = TrueVal

    RETURN

END SUBROUTINE DisplaySevereError

!******************************************************************************

SUBROUTINE DisplayContinueError(ErrorMessage,OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^  To display a 'Continued Error' message.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,         INTENT(IN)       :: ErrorMessage
    tSInt32, OPTIONAL, INTENT(INOUT)    :: OutUnit
    !^ user specified unit number. <br>
    ! If not present, the message is written to the default log file. <br>
    ! If present and an associated file is open, the message is written to the associated file. <br>
    ! Otherwise, the message is written to a file named 'UserErrLog.Txt'.

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! display message
    IF (DisplayCont) CALL DisplayErrorMessage(' +++++ : ' // ErrorMessage, OutUnit)
    
    ! check error flags
    IF (StopOnError.AND.FatalError) THEN
        CALL AbortProgram(OutUnit)
    ELSE
        ! reset flag
        FatalError = FalseVal
    END IF

    RETURN

END SUBROUTINE DisplayContinueError

!******************************************************************************

SUBROUTINE DisplayMessage(Message,OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^  To display a (informative) message on designated output Files.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,         INTENT(IN)       :: Message
    tSInt32, OPTIONAL, INTENT(INOUT)    :: OutUnit
    !^ user specified unit number. <br>
    ! If not present, the message is written to the default log file. <br>
    ! If present and an associated file is open, the message is written to the associated file. <br>
    ! Otherwise, the message is written to a file named 'UserErrLog.Txt'.

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na
  
!** FLOW

    CALL DisplayErrorMessage(Message, OutUnit)

    RETURN

END SUBROUTINE DisplayMessage

!******************************************************************************

SUBROUTINE CloseErrorFile(OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^  This subroutine closes the general error file.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tSInt32, OPTIONAL, INTENT(IN)   :: OutUnit
    !^ user specified unit number. <br>
    ! If not present, the default file is closed. <br>
    ! If present and an associated file is open, the associated file is closed. <br>
    ! Otherwise, the file named 'UserErrLog.Txt' is closed.

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tLogical    :: FileOpened
    tSInt32     :: IOS
    tSInt32     :: ErrorUnit
   
!** FLOW

    IF (PRESENT(OutUnit)) THEN

        ! first, check whether the specified unit number is connected to an opened file
        INQUIRE(UNIT=OutUnit, OPENED=FileOpened, IOStat=IOS)
        
        IF (FileOpened.AND.(IOS == 0)) THEN
            CLOSE(ErrorUnit)
        ELSE
            INQUIRE(FILE='UserErrLog.Txt', OPENED=FileOpened, IOStat=IOS)
            IF (FileOpened.AND.(IOS == 0)) THEN
                ! get existing unit number for the opened file
                INQUIRE(FILE='UserErrLog.Txt', NUMBER=ErrorUnit)
                ! then close the 'UserErrLog.Txt' file
                CLOSE(ErrorUnit)
            END IF
        END IF
    ELSE
        INQUIRE(FILE=ErrFilename, OPENED=FileOpened, IOStat=IOS)
        IF (FileOpened.AND.(IOS == 0)) THEN
            ! get existing unit number for the opened file
            INQUIRE(FILE=ErrFilename, NUMBER=ErrorUnit)
            ! then close the 'XpfLibError.Log' file
            CLOSE(ErrorUnit)
        END IF
    END IF

    RETURN

END SUBROUTINE CloseErrorFile

!******************************************************************************

SUBROUTINE AbortProgram(OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^  This subroutine causes the program to halt due to a fatal error.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tSInt32, OPTIONAL, INTENT(INOUT)    :: OutUnit
    !^ user specified unit number. <br>
    ! If not present, messages are written to the default log file. <br>
    ! If present and an associated file is open, messages are written to the associated file. <br>
    ! Otherwise, messages are written to a file named 'UserErrLog.Txt'.

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharAlloc  :: StopMessage

!** FLOW

    StopMessage = 'XpfLib Terminated -- Fatal Error(s) Detected.'
    CALL DisplayMessage(StopMessage, OutUnit)
    CALL CloseMiscOpenFiles()
    STOP 'Program Terminated -- Error(s) Detected.'

    RETURN

END SUBROUTINE AbortProgram

!******************************************************************************

SUBROUTINE SetStopOnError(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^  To set StopOnError flag (module variable).

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tLogical, INTENT(IN)    :: Flag !! true if requesting termination of the program due to fatal error(s)

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    StopOnError = Flag

    RETURN

END SUBROUTINE SetStopOnError

!******************************************************************************

SUBROUTINE CloseMiscOpenFiles

!** PURPOSE OF THIS SUBROUTINE:
    !^  This subroutine scans potential unit numbers and closes any that are still open.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    ! na
          
!** SUBROUTINE PARAMETER DEFINITIONS:
    tSInt32, PARAMETER  :: MaxUnitNumber = 1000
  
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tLogical    :: EXIST, OPENED
    tSInt32     :: UnitNumber
    tSInt32     :: IOS
   
!** FLOW

    DO UnitNumber = 1, MaxUnitNumber
        INQUIRE(Unit=UnitNumber, EXIST=EXIST,  OPENED=OPENED, IOStat=IOS)
        IF (EXIST.AND.OPENED.AND.(IOS == 0)) CLOSE(UnitNumber)
    END DO
   
    RETURN

END SUBROUTINE CloseMiscOpenFiles

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           ERROR-HANDLING ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE Handle_ErrDealloc(SubName, ModName, Msg, Stat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To handle deallocation error if necessary (i.e. Stat /= 0).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: SubName  !! routine name 
    tCharStar, INTENT(IN)   :: ModName  !! module name
    tCharStar, INTENT(IN)   :: Msg      !! error message
    tSInt32,   INTENT(IN)   :: Stat     !! error status

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (Stat /= 0) THEN
        CALL DisplaySevereError('Message from Routine '//SubName//' in Module '//ModName//'.')
        CALL DisplayContinueError('Error occurred while trying to deallocate an entity.')
        CALL DisplayContinueError('STAT = ' // ToChar(Stat))
        CALL DisplayContinueError('ERRMSG = ' // Msg)
    END IF;

    RETURN

END SUBROUTINE Handle_ErrDealloc

!******************************************************************************

SUBROUTINE Handle_ErrAlloc(SubName, ModName, Msg, Stat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To handle allocation error if necessary (i.e. Stat /= 0).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: SubName  !! routine name 
    tCharStar, INTENT(IN)   :: ModName  !! module name
    tCharStar, INTENT(IN)   :: Msg      !! error message
    tSInt32,   INTENT(IN)   :: Stat     !! error status

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (Stat /= 0) THEN
        CALL DisplaySevereError('Message from Routine '//SubName//' in Module '//ModName//'.')
        CALL DisplayContinueError('Error occurred while trying to allocate an entity.')
        CALL DisplayContinueError('STAT = ' // ToChar(Stat))
        CALL DisplayContinueError('ERRMSG = ' // Msg)
    END IF;

    RETURN

END SUBROUTINE Handle_ErrAlloc

!******************************************************************************

SUBROUTINE Handle_ErrOpen(SubName, ModName, FileName, Level)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To handle error due to open file operation according to the specified error level (and
    !  the configuration mode defined in the "Macro - Basic Definitions.f90" file).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: SubName  !! routine name 
    tCharStar, INTENT(IN)   :: ModName  !! module name
    tCharStar, INTENT(IN)   :: FileName !! file name
    tSInt32,   INTENT(IN)   :: Level    !! error level

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

#ifdef DebugMode
    ! for debug mode, show error for level >= ErrWarning
    SELECT CASE (Level)
    CASE (ErrWarning)
        CALL DisplayWarningError('Message from Routine '//SubName//' in Module '//ModName//'.')
        CALL DisplayContinueError('Could not open "' // TRIM( FileName ) // '".')
    CASE (ErrSevere)
        CALL DisplaySevereError('Message from Routine '//SubName//' in Module '//ModName//'.')
        CALL DisplayContinueError('Could not open "' // TRIM( FileName ) // '".')
    CASE (ErrFatal)
        CALL DisplayFatalError('Message from Routine '//SubName//' in Module '//ModName//'.')
        CALL DisplayContinueError('Could not open "' // TRIM( FileName ) // '".')
    END SELECT
#else
    ! for release mode, show error for level >= ErrSevere
    SELECT CASE (Level)
    CASE (ErrSevere)
        CALL DisplaySevereError('Message from Routine '//SubName//' in Module '//ModName//'.')
        CALL DisplayContinueError('Could not open "' // TRIM( FileName ) // '".')
    CASE (ErrFatal)
        CALL DisplayFatalError('Message from Routine '//SubName//' in Module '//ModName//'.')
        CALL DisplayContinueError('Could not open "' // TRIM( FileName ) // '".')
    END SELECT
#endif

    RETURN

END SUBROUTINE Handle_ErrOpen

!******************************************************************************

SUBROUTINE Handle_ErrLevel(SubName, ModName, Level, Msg)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To handle the error occurred according to the specified error level (and the
    !  configuration mode defined in the "Macro - Basic Definitions.f90" file).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: SubName  !! routine name 
    tCharStar, INTENT(IN)   :: ModName  !! module name
    tSInt32,   INTENT(IN)   :: Level    !! error level
    tCharStar, INTENT(IN)   :: Msg      !! error message

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

#ifdef DebugMode
    ! for debug mode, show error for level >= ErrWarning
    SELECT CASE (Level)
    CASE (ErrWarning)
        CALL DisplayWarningError('Message from Routine '//SubName//' in Module '//ModName//'.')
        CALL DisplayContinueError(Msg)
    CASE (ErrSevere)
        CALL DisplaySevereError('Message from Routine '//SubName//' in Module '//ModName//'.')
        CALL DisplayContinueError(Msg)
    CASE (ErrFatal)
        CALL DisplayFatalError('Message from Routine '//SubName//' in Module '//ModName//'.')
        CALL DisplayContinueError(Msg)
    END SELECT
#else
    ! for release mode, show error for level >= ErrSevere
    SELECT CASE (Level)
    CASE (ErrSevere)
        CALL DisplaySevereError('Message from Routine '//SubName//' in Module '//ModName//'.')
        CALL DisplayContinueError(Msg)
    CASE (ErrFatal)
        CALL DisplayFatalError('Message from Routine '//SubName//' in Module '//ModName//'.')
        CALL DisplayContinueError(Msg)
    END SELECT
#endif

    RETURN

END SUBROUTINE Handle_ErrLevel

!******************************************************************************

END MODULE MBase_ErrHandlers

!******************************************************************************
