
MODULE MClass_ErrorInfo

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *ErrorInfo* derived type and its related routines.
!   The *ErrorInfo* type is an error data type that can be used to handle and report
!   errors.  Unlike the <a href="../module/mclass_errordata.html#type-errordata">
!   ErrorData</a> type, which can only report the routine name (and optionally
!   the module name) where the error occurs, the *ErrorInfo* type provides the
!   *SetCaller* method so that all the calling routines and their modules can
!   be reported.

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_StackChar
    USE MBase_ErrHandlers
    USE MBase_SIntUtil,     ONLY: ToChar    => ToDecStrSigned

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived types
    PUBLIC :: ErrorInfo
    ! procedure
    PUBLIC :: ReportError

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> *ErrorInfo* is an error data type that can be used to handle and report errors.
    TYPE ErrorInfo
        PRIVATE
        tSInt32         :: Level = ErrNone      ! error level
        tCharAlloc      :: Message              ! error message
        TYPE(StackChar) :: SubStack             ! stack of subroutine/function names where the error occurs
        TYPE(StackChar) :: ModStack             ! stack of module names where the error occurs
        tLogical        :: Handled = FalseVal   ! error handling flag
    CONTAINS
        !> **Type-Bound Subroutine**: SetInfo <br>
        ! **Purpose**:  To set/store the error information where the error occurs. <br>
        !  **Usage**: <br>
        !   --->    CALL Error%SetInfo('DoDivide', 'ModMath', ErrSevere, 'Division by zero') <br>
        !   --->    CALL Error%SetInfo('DoMultiply', 'ModMath', ErrWarning, 'Integer overflow') <br>
        !   --->    CALL Error%SetInfo('DoSomething', 'ModMisc', ErrWarning, 'Invalid input') <br>
        !   --->    CALL Error%SetInfo(RoutineName, ModuleName, ErrorLevel, ErrorMessage) <br>
        PROCEDURE   :: SetInfo      => ErrorInfo_SetError
        !> **Type-Bound Subroutine**: SetCaller <br>
        ! **Purpose**:  To set/store the subroutine and module names of the caller. <br>
        !  **Usage**: <br>
        !   --->    CALL Error%SetCaller('DoArithmetic', 'ModMath') <br>
        !   --->    CALL Error%SetCaller('DoMainThing', 'ModMisc') <br>
        PROCEDURE   :: SetCaller    => ErrorInfo_SetRoutineCaller
        !> **Type-Bound Function**: GetLevel <br>
        ! **Purpose**:  To get error level <br>
        !  **Usage**: <br>
        !   --->    ErrLevel = Error%GetLevel() <br>
        !   --->    IF (Error%GetLevel() > ErrNone) DoSomething
        PROCEDURE   :: GetLevel     => ErrorInfo_GetErrorLevel
        !> **Type-Bound Subroutine**: Report <br>
        ! **Purpose**:  To report error information <br>
        !  **Usage**: <br>
        !   --->    CALL Error%Report()         ! report to default error log file. <br>
        !   --->    CALL Error%Report(OutUnit)  ! report to a file associated with 'OutUnit'.
        PROCEDURE   :: Report       => ErrorInfo_ReportError
        !> **Type-Bound Subroutine**: Reset <br>
        ! **Purpose**:  To reset the error object to default state (no error) <br>
        !  **Usage**: <br>
        !   --->    CALL Error%Reset()
        PROCEDURE   :: Reset        => ErrorInfo_ResetError
        !% a final subroutine supposedly automatically called when the object is out of scope
        FINAL       :: ErrorInfo_Finalize
    END TYPE ErrorInfo

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!**************************************************************************************

SUBROUTINE ErrorInfo_SetError(ErrInstance, SubName, ModName, Level, Message)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To store the error information where the error occurs.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ErrorInfo), INTENT(INOUT) :: ErrInstance  !! 'ErrorInfo' object
    tCharStar,        INTENT(IN)    :: SubName      !! name of routine where error occurs
    tCharStar,        INTENT(IN)    :: ModName      !! name of module where the routine is in
    tSInt32,          INTENT(IN)    :: Level
    !^ level of error <br>
    ! - ErrNone    (0)  ! no error level <br>
    ! - ErrWarning (1)  ! warning error level <br>
    ! - ErrSevere  (2)  ! severe error level <br>
    ! - ErrFatal   (3)  ! fatal error level
    tCharStar,        INTENT(IN)    :: Message      !! error message
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! set error information
    ErrInstance%Level   = Level
    ErrInstance%Message = Message
    CALL ErrInstance%SubStack%Push(SubName)
    CALL ErrInstance%ModStack%Push(ModName)

    RETURN

END SUBROUTINE ErrorInfo_SetError

!******************************************************************************

SUBROUTINE ErrorInfo_SetRoutineCaller(ErrInstance, SubName, ModName)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To store the subroutine and module names of the caller.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ErrorInfo), INTENT(INOUT) :: ErrInstance
    tCharStar,        INTENT(IN)    :: SubName      !! name of caller routine
    tCharStar,        INTENT(IN)    :: ModName      !! name of module where the caller is in
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL ErrInstance%SubStack%Push(SubName)
    CALL ErrInstance%ModStack%Push(ModName)

    RETURN

END SUBROUTINE ErrorInfo_SetRoutineCaller

!******************************************************************************

FUNCTION ErrorInfo_GetErrorLevel(ErrInstance) RESULT(Level)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the error level.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ErrorInfo), INTENT(IN)    :: ErrInstance  !! 'ErrorInfo' object
    tSInt32                         :: Level        !! error level

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Level = ErrInstance%Level

    RETURN

END FUNCTION ErrorInfo_GetErrorLevel

!**************************************************************************************

SUBROUTINE ErrorInfo_ResetError(ErrInstance)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the error object to default state (no error).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ErrorInfo), INTENT(OUT)   :: ErrInstance  !! 'ErrorInfo' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ErrInstance%Level   = ErrNone
    IF (ALLOCATED(ErrInstance%Message)) DEALLOCATE(ErrInstance%Message)
    CALL ErrInstance%SubStack%Destroy()
    CALL ErrInstance%ModStack%Destroy()
    ErrInstance%Handled = FalseVal

    RETURN

END SUBROUTINE ErrorInfo_ResetError

!**************************************************************************************

SUBROUTINE ErrorInfo_ReportError(ErrInstance,OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To report error information to a default log file named 'XpfErrLog.txt'
    !  or to a user log file if the optional 'OutUnit' argument is specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ErrorInfo),   INTENT(INOUT)   :: ErrInstance  !! 'ErrorInfo' object
    tSInt32,  OPTIONAL, INTENT(INOUT)   :: OutUnit      !! output unit

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName
    tCharAlloc      :: ModName
    tCharAlloc      :: Message
    tIndex          :: I

!** FLOW
    
    IF ((.NOT.ErrInstance%Handled).AND.(ErrInstance%Level > ErrNone)) THEN
        CALL DisplayMessage('++++++++++++++++++++++++++++++++++++++++++++++++++', OutUnit)
        SELECT CASE (ErrInstance%Level)
        CASE(ErrWarning)
            CALL DisplayWarningError('Message = ' // ErrInstance%Message, OutUnit)
        CASE(ErrSevere)
            CALL DisplaySevereError('Message = ' // ErrInstance%Message, OutUnit)
        CASE(ErrFatal)
            CALL DisplayFatalError('Message = ' // ErrInstance%Message, OutUnit)
        END SELECT
        CALL DisplayMessage('--------------------------------------------------', OutUnit)
        CALL DisplayMessage('The message is generated due to the following routine stack:', OutUnit)
        I = 0
        DO WHILE ((.NOT.ErrInstance%SubStack%IsEmpty()).AND. &
                  (.NOT.ErrInstance%ModStack%IsEmpty()))
            CALL ErrInstance%SubStack%Pop(SubName, Peek=TrueVal)
            CALL ErrInstance%ModStack%Pop(ModName, Peek=TrueVal)
            I = I + 1
            Message = '(' // ToChar(I) // ') Routine "' // SubName &
                          // '" in Module "' // ModName // '"'
            CALL DisplayMessage(Message, OutUnit)
        END DO
        CALL DisplayMessage('++++++++++++++++++++++++++++++++++++++++++++++++++', OutUnit)
        ErrInstance%Handled = TrueVal
    END IF

    RETURN

END SUBROUTINE ErrorInfo_ReportError

!**************************************************************************************

SUBROUTINE ErrorInfo_Finalize(ErrInstance)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the ErrorInfo object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ErrorInfo), INTENT(INOUT)  :: ErrInstance  !! 'ErrorInfo' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    CALL ErrInstance%Report()
    CALL ErrInstance%Reset()

    RETURN

END SUBROUTINE ErrorInfo_Finalize

!**************************************************************************************

SUBROUTINE ReportError(SubName, ModName, Level, Message, ErrInstance)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To report and/or set the specified error information.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,                 INTENT(IN)       :: SubName      !! name of routine where error occurs
    tCharStar,                 INTENT(IN)       :: ModName      !! name of module where the routine is in
    tSInt32,                   INTENT(IN)       :: Level
    !^ level of error <br>
    ! - ErrNone    (0)  ! no error level <br>
    ! - ErrWarning (1)  ! warning error level <br>
    ! - ErrSevere  (2)  ! severe error level <br>
    ! - ErrFatal   (3)  ! fatal error level
    tCharStar,                 INTENT(IN)       :: Message      !! error message
    TYPE(ErrorInfo), OPTIONAL, INTENT(INOUT)    :: ErrInstance  !! 'ErrorInfo' object
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (PRESENT(ErrInstance)) THEN
        CALL ErrInstance%SetInfo(SubName, ModName, Level, Message)
    ELSE
        CALL Handle_ErrLevel(SubName, ModName, Level, Message)
    END IF

    RETURN

END SUBROUTINE ReportError

!******************************************************************************

END MODULE MClass_ErrorInfo

!******************************************************************************
