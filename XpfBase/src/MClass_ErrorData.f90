
MODULE MClass_ErrorData

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *ErrorData* derived type and its related routines.
!   The *ErrorData* type is a simple error data type that can be used to handle and
!   report errors. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_SIntUtil,       ONLY: ToChar => ToDecStrSigned

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: ErrorData

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> *ErrorData* is an error data type that can be used to handle and report errors.
    TYPE ErrorData
        PRIVATE
        tSInt32         :: Level = ErrNone      ! error level
        tSInt32         :: Code  = 0            ! error code
        tCharAlloc      :: Routine              ! routine where the error occurs
        tCharAlloc      :: Message              ! error message
        tLogical        :: Handled = FalseVal   ! error handling flag
    CONTAINS
        !> **Type-Bound Subroutine**: SetInfo <br>
        ! **Purpose**:  To set/store the error information. <br>
        !  **Usage**: <br>
        !   --->    CALL Error%SetInfo('Division by zero', 'DoDivide', ErrSevere) <br>
        !   --->    CALL Error%SetInfo('Integer overflow', 'DoMultiply', ErrWarning, ErrModule='Integer_Arithmetic') <br>
        !   --->    CALL Error%SetInfo('Invalid input', 'DoSomething', ErrWarning, ErrCode=11)
        PROCEDURE       :: SetInfo      => SetError
        !> **Type-Bound Subroutine**: GetInfo <br>
        ! **Purpose**:  To get all error information <br>
        !  **Usage**: <br>
        !   --->    CALL Error%GetInfo(ErrMsg,ErrRoutine,ErrLevel) <br>
        !   --->    CALL Error%GetInfo(Message,Routine,Level,ErrCode=Code)
        PROCEDURE       :: GetInfo      => GetErrorAll
        !> **Type-Bound Function**: GetMessage <br>
        ! **Purpose**:  To get error message <br>
        !  **Usage**: <br>
        !   --->    ErrMsg = Error%GetMessage()
        PROCEDURE       :: GetMessage   => GetErrorMessage
        !> **Type-Bound Function**: GetRoutine <br>
        ! **Purpose**:  To get name of the routine reporting the error <br>
        !  **Usage**: <br>
        !   --->    RoutineName = Error%GetRoutine()
        PROCEDURE       :: GetRoutine   => GetErrorRoutine
        !> **Type-Bound Function**: GetLevel <br>
        ! **Purpose**:  To get error level <br>
        !  **Usage**: <br>
        !   --->    ErrLevel = Error%GetLevel() <br>
        !   --->    IF (Error%GetLevel() > ErrNone) DoSomething
        PROCEDURE       :: GetLevel     => GetErrorLevel
        !> **Type-Bound Function**: GetCode <br>
        ! **Purpose**:  To get numerical code of the error <br>
        !  **Usage**: <br>
        !   --->    ErrCode = Error%GetCode()
        PROCEDURE       :: GetCode      => GetErrorCode
        !> **Type-Bound Subroutine**: Reset <br>
        ! **Purpose**:  To reset the error object to default state (no error) <br>
        !  **Usage**: <br>
        !   --->    CALL Error%Reset()
        PROCEDURE       :: Reset        => ResetError
        !> **Type-Bound Subroutine**: Report <br>
        ! **Purpose**:  To report error information <br>
        !  **Usage**: <br>
        !   --->    CALL Error%Report()         ! report to default error log file. <br>
        !   --->    CALL Error%Report(OutUnit)  ! report to a file associated with 'OutUnit'.
        PROCEDURE       :: Report       => ReportError
        ! a final subroutine supposedly automatically called when the object is out of scope
        ! FINAL           :: FinalizeErrorData
    END TYPE ErrorData

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!**************************************************************************************

SUBROUTINE SetError(ErrFlag,ErrMsg,ErrRoutine,ErrLevel,ErrModule,ErrCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To set/store the error information.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CLASS(ErrorData),    INTENT(INOUT)  :: ErrFlag      !! the error data object
    tCharStar,           INTENT(IN)     :: ErrMsg       !! error message
    tCharStar,           INTENT(IN)     :: ErrRoutine   !! name of the routine reporting the error
    tSInt32,             INTENT(IN)     :: ErrLevel
    !^ level of error <br>
    ! - ErrNone    (0)  ! no error level <br>
    ! - ErrWarning (1)  ! warning error level <br>
    ! - ErrSevere  (2)  ! severe error level <br>
    ! - ErrFatal   (3)  ! fatal error level
    tCharStar, OPTIONAL, INTENT(IN)     :: ErrModule    !! module that contains the error routine
    tSInt32,   OPTIONAL, INTENT(IN)     :: ErrCode      !! numerical code of the error
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! set required data
    ErrFlag%Level   = ErrLevel
    IF (ALLOCATED(ErrFlag%Routine)) DEALLOCATE(ErrFlag%Routine)
    IF (PRESENT(ErrModule)) THEN
        ErrFlag%Routine = 'Routine ' // TRIM(ErrRoutine) // ' in Module ' // TRIM(ErrModule)
    ELSE
        ErrFlag%Routine = 'Routine ' // TRIM(ErrRoutine)
    END IF
    IF (ALLOCATED(ErrFlag%Message)) DEALLOCATE(ErrFlag%Message)
    ErrFlag%Message = TRIM(ErrMsg)
    ! set optional data
    IF (PRESENT(ErrCode)) ErrFlag%Code = ErrCode

    RETURN

END SUBROUTINE SetError

!******************************************************************************

SUBROUTINE GetErrorAll(ErrFlag,ErrMsg,ErrRoutine,ErrLevel,ErrCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get all error information.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ErrorData), INTENT(IN)    :: ErrFlag      !! the error data object
    tCharAlloc,       INTENT(OUT)   :: ErrMsg       !! error message
    tCharAlloc,       INTENT(OUT)   :: ErrRoutine   !! name of the routine reporting the error
    tSInt32,          INTENT(OUT)   :: ErrLevel     !! level of error
    tSInt32,          INTENT(OUT)   :: ErrCode      !! numerical code of the error

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ErrMsg      = ErrFlag%Message
    ErrRoutine  = ErrFlag%Routine
    ErrLevel    = ErrFlag%Level
    ErrCode     = ErrFlag%Code

    RETURN

END SUBROUTINE GetErrorAll

!**************************************************************************************

FUNCTION GetErrorMessage(ErrFlag) RESULT(ErrMsg)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get error message.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ErrorData), INTENT(IN)    :: ErrFlag  !! the error data object
    tCharAlloc                      :: ErrMsg   !! error message

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ErrMsg = ErrFlag%Message

    RETURN

END FUNCTION GetErrorMessage

!**************************************************************************************

FUNCTION GetErrorRoutine(ErrFlag) RESULT(ErrRoutine)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get name of the routine reporting the error.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ErrorData), INTENT(IN)    :: ErrFlag      !! the error data object
    tCharAlloc                      :: ErrRoutine   !! name of the routine reporting the error

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ErrRoutine = ErrFlag%Routine

    RETURN

END FUNCTION GetErrorRoutine

!**************************************************************************************

FUNCTION GetErrorLevel(ErrFlag) RESULT(ErrLevel)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get error level.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ErrorData), INTENT(IN)    :: ErrFlag      !! the error data object
    tSInt32                         :: ErrLevel     !! level of error

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ErrLevel = ErrFlag%Level

    RETURN

END FUNCTION GetErrorLevel

!**************************************************************************************

FUNCTION GetErrorCode(ErrFlag) RESULT(ErrCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get numerical code of the error.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ErrorData), INTENT(IN)    :: ErrFlag  !! the error data object
    tSInt32                         :: ErrCode  !! numerical code of the error

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ErrCode = ErrFlag%Code

    RETURN

END FUNCTION GetErrorCode

!**************************************************************************************

SUBROUTINE ResetError(ErrFlag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To reset the error object to default state (no error).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ErrorData), INTENT(OUT)   :: ErrFlag      !! the error data object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(ErrFlag%Routine)) DEALLOCATE(ErrFlag%Routine)
    IF (ALLOCATED(ErrFlag%Message)) DEALLOCATE(ErrFlag%Message)
    ErrFlag%Code    = 0
    ErrFlag%Level   = ErrNone
    ErrFlag%Handled = FalseVal

    RETURN

END SUBROUTINE ResetError

!**************************************************************************************

SUBROUTINE ReportError(ErrFlag,OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To report error information to a default log file named 'XpfErrLog.txt'
    !  or to a user log file if the optional 'OutUnit' argument is specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ErrorData),   INTENT(INOUT)   :: ErrFlag  !! the error data object
    tSInt32,  OPTIONAL, INTENT(INOUT)   :: OutUnit  !! the output unit number to report to

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc   :: Message

!** FLOW
    
    IF ((.NOT.ErrFlag%Handled).AND.(ErrFlag%Level.NE.ErrNone)) THEN
        SELECT CASE (ErrFlag%Level)
        CASE(ErrWarning)
            CALL DisplayWarningError('Message from ' // TRIM(ErrFlag%Routine), OutUnit)
        CASE(ErrSevere)
            CALL DisplaySevereError('Message from ' // TRIM(ErrFlag%Routine), OutUnit)
        CASE(ErrFatal)
            CALL DisplayFatalError('Message from ' // TRIM(ErrFlag%Routine), OutUnit)
        END SELECT
        CALL DisplayContinueError('Message = ' // TRIM(ErrFlag%Message), OutUnit)
        Message = 'Error Code = ' // ToChar(ErrFlag%Code)
        CALL DisplayContinueError(Message, OutUnit)
        ErrFlag%Handled = TrueVal
    END IF

    RETURN

END SUBROUTINE ReportError

!**************************************************************************************

SUBROUTINE FinalizeErrorData(ErrFlag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To finalize the ErrorData object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ErrorData), INTENT(INOUT)  :: ErrFlag      !! the error data object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    CALL ErrFlag%Report()
    CALL ErrFlag%Reset()

    RETURN

END SUBROUTINE FinalizeErrorData

!**************************************************************************************

END MODULE MClass_ErrorData

!******************************************************************************
