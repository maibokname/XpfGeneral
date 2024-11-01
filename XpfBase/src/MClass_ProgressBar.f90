
MODULE MClass_ProgressBar

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains a *ProgressBar* class that can be used to display a progress
!   bar in the console, so that the user can see the progress of a long calculation or
!   operation. The *ProgressBar* class is an object-oriented implementation of the
!   ***progress_bar*** module of <a href="https://github.com/arjenmarkus/io_utilities">
!   I/O utilities: Small collection of Fortran modules.</a> <br>
!  <br>
! **USAGE**: <br>
!   The following code snippet illustrates a typical usage of the ProgressBar class.
!   <Pre><Code style="color:MidnightBlue;">
!   ! set up progress bar
!   CALL Bar%Setup(Progress_Delimited, Prefix = "Progress: ", Show = Progress_Value_Max, &
!                  MaxVal = REAL(OutLoop, KIND=SP))
!   ! perform a long calculation task
!   DO J = 1, OutLoop
!       DO I = 1, InLoop
!           Do Some Calculations
!       END DO
!       ! update the bar
!       CALL Bar%Update(REAL(J, KIND=SP))
!   END DO
!   ! when done, move cursor to next line
!   CALL Bar%Finish()
!   </Code></Pre>

!** USE STATEMENTS:
    USE MBase_Common
    USE ISO_C_BINDING,      ONLY: C_Carriage_Return
    USE ISO_FORTRAN_ENV,    ONLY: Output_Unit

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: ProgressBar

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    !------------------------------------------------------------
    !-----      parameters for the progress bar style       -----               
    !------------------------------------------------------------
    !% To keep the previously defined options and change selected ones only. 
    tSInt32, PARAMETER, PUBLIC  :: Progress_Keep       = 0
    !% To simply append the cursor to the previous ones without additional text.
    tSInt32, PARAMETER, PUBLIC  :: Progress_Continuous = 1
    !% To set the cursor to run up and down in a limited range.
    tSInt32, PARAMETER, PUBLIC  :: Progress_Cyclic     = 2
    !% To set the cursor to run from left to right in a limited range.
    tSInt32, PARAMETER, PUBLIC  :: Progress_Delimited  = 3
    !% To set the cursor as a rotating cursor (a succession of '-', '/', '|', '').
    tSInt32, PARAMETER, PUBLIC  :: Progress_Rotating   = 4
    !% To displays only the value and possibly the maximum value.
    tSInt32, PARAMETER, PUBLIC  :: Progress_NoCursor   = 5
    !------------------------------------------------------------
    !-----      parameters for options to show the value    -----
    !------------------------------------------------------------
    !> To show progress bar without value.
    tSInt32, PARAMETER, PUBLIC  :: Progress_None       = 0
    !> To show the value next to the progress bar.
    tSInt32, PARAMETER, PUBLIC  :: Progress_Value      = 1
    !> To show the value as well as the maximum value next to the progress bar.
    tSInt32, PARAMETER, PUBLIC  :: Progress_Value_Max  = 2

!** DERIVED TYPE DEFINITIONS
    !> a derived type to display a progress bar on the console
    TYPE ProgressBar
        PRIVATE
        ! options
        tChar       :: LeadChar  = '-'
        tChar       :: TrailChar = '>'
        tCharAlloc  :: PrefixStr
        tCharAlloc  :: SuffixStr
        tCharAlloc  :: FormatValue
        tCharAlloc  :: FormatBoth
        tSInt32     :: ShowValue = Progress_None
        ! parameters
        tRealSP     :: MaxValue    = 100.0_kSingle
        tSInt32     :: Style       = -1
        tSInt32     :: Width       = 50
        tSInt32     :: Position    =  0
        tSInt32     :: Direction   =  1
        tCharLen(4) :: CyclicChars = '-\|/'
    CONTAINS
        PROCEDURE, PRIVATE  :: ProgressBar_Next
        PROCEDURE, PRIVATE  :: ProgressBar_Value
        PROCEDURE, PRIVATE  :: Reset        => ProgressBar_Reset
        !> **Type-Bound Subroutine**: Setup <br>
        ! **Purpose**:  To set the style and other parameters for the progress bar <br>
        !  **Usage**: <br>
        !   --->    CALL Bar%Setup(Progress_Rotating)                    ! default parameters <br>
        !   --->    CALL Bar%Setup(Progress_Delimited, Leading="*")      ! set leading character <br>
        !   --->    CALL Bar%Setup(Progress_Cyclic, Prefix="Progress: ") ! set prefix text
        PROCEDURE           :: Setup        => ProgressBar_Setup
        !> **Type-Bound Subroutine**: Update <br>
        ! **Purpose**:  To show/update the progress bar <br>
        !  **Usage**: <br>
        !   --->    CALL Bar%Update()    ! show progress bar by writing the next character <br>
        !   --->    CALL Bar%Update(Val) ! show progress bar based on the specified value 'Val'
        GENERIC             :: Update       => ProgressBar_Next, &
                                               ProgressBar_Value
        !> **Type-Bound Subroutine**: Finish <br>
        ! **Purpose**:  To finish the progress bar by moving the
        !               cursor to the next line <br>
        !  **Usage**: <br>
        !   --->    CALL Bar%Finish()
        PROCEDURE           :: Finish       => ProgressBar_Finish
    END TYPE ProgressBar

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE ProgressBar_Reset(Bar)

!** PURPOSE OF THIS SUBROUTINE:
    !! To reset all options to the default values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ProgressBar), INTENT(INOUT)   :: Bar

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Bar%Position  =  0
    Bar%Direction =  1
    Bar%Style     = -1
    Bar%Width     = 50
    Bar%MaxValue  = 100.0_kSingle

    Bar%LeadChar    = '-'
    Bar%TrailChar   = '*'
    Bar%PrefixStr   = ''
    Bar%SuffixStr   = ''
    Bar%FormatValue = '(F8.2)'
    Bar%FormatBoth  = '(F8.2,''/'',F8.2)'
    Bar%ShowValue   = Progress_None

    RETURN

END SUBROUTINE ProgressBar_Reset

!******************************************************************************

SUBROUTINE ProgressBar_Finish(Bar)

!** PURPOSE OF THIS SUBROUTINE:
    !! To move the cursor to the next line.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ProgressBar), INTENT(INOUT)   :: Bar

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Bar%Position  =  0
    Bar%Direction =  1

    WRITE(Output_Unit, '(A)') ''

    RETURN

END SUBROUTINE ProgressBar_Finish

!******************************************************************************

SUBROUTINE ProgressBar_Setup(Bar, Style, MaxVal, Leading, Trailing, Prefix, &
                             Suffix, Show, Width, FormatValue, FormatBoth)

!** PURPOSE OF THIS SUBROUTINE:
    !! To set the style and other parameters for the progress bar.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% the progress bar object
    CLASS(ProgressBar),  INTENT(INOUT)  :: Bar
    !> the progress bar style where the available styles include <br>
    !   - Progress_Keep (0) <br>
    !   - Progress_Continuous (1) <br>
    !   - Progress_Cyclic (2) <br>
    !   - Progress_Delimited (3) <br>
    !   - Progress_Rotating (4) <br>
    !   - Progress_NoCursor (5)
    tSInt32,             INTENT(IN)     :: Style
    !% scale for values passed to *ProgressBar_Value* routine (calculate the length)
    tRealSP,   OPTIONAL, INTENT(IN)     :: MaxVal
    !% character to use before the cursor
    tCharStar, OPTIONAL, INTENT(IN)     :: Leading
    !% character to use to show the Position of the cursor
    tCharStar, OPTIONAL, INTENT(IN)     :: Trailing
    !% text to be shown before the progress bar
    tCharStar, OPTIONAL, INTENT(IN)     :: Prefix
    !% text to be shown after the progress bar
    tCharStar, OPTIONAL, INTENT(IN)     :: Suffix
    !> option to show the actual values where the available options include <br>
    !   - Progress_None (0) <br>
    !   - Progress_Value (1) <br>
    !   - Progress_Value_Max (2)
    tSInt32,   OPTIONAL, INTENT(IN)     :: Show
    !% width of the area for the cursor
    tSInt32,   OPTIONAL, INTENT(IN)     :: Width
    !% format to be used for the value (without the maximum)
    tCharStar, OPTIONAL, INTENT(IN)     :: FormatValue
    !% format to be used for the value and the maximum
    tCharStar, OPTIONAL, INTENT(IN)     :: FormatBoth

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    !
    ! Check style
    !
    IF (ALL(Style /= [Progress_Keep, Progress_Continuous, Progress_Cyclic, &
                      Progress_Delimited, Progress_Rotating, Progress_NoCursor])) THEN
        Bar%Style = Progress_Continuous
    ELSE
        IF (Style /= Progress_Keep) THEN
            CALL Bar%Reset()
            Bar%Style = Style
        END IF
    END IF

    !
    ! Store the various values
    !
    IF (PRESENT(MaxVal))      Bar%MaxValue = MaxVal
    IF (PRESENT(Leading))     Bar%LeadChar = leading
    IF (PRESENT(Trailing))    Bar%TrailChar = Trailing
    IF (PRESENT(Show))        Bar%ShowValue = Show
    IF (PRESENT(Width))       Bar%Width = Width
    IF (PRESENT(FormatValue)) Bar%FormatValue = FormatValue
    IF (PRESENT(FormatBoth))  Bar%FormatBoth = FormatBoth
    IF (PRESENT(Prefix)) THEN
        Bar%PrefixStr = Prefix
    ELSE
        Bar%PrefixStr = ''
    END IF
    IF (PRESENT(Suffix)) THEN
        Bar%SuffixStr = Suffix
    ELSE
        Bar%SuffixStr = ''
    END IF

    Bar%Position  =  0
    Bar%Direction =  1

    RETURN

END SUBROUTINE ProgressBar_Setup

!******************************************************************************

SUBROUTINE ProgressBar_Next(Bar)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To simply write the next character. <br>
    ! Note: Only applicable if the progress bar does not show a value.
    !       If it should show a value, simply return.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ProgressBar), INTENT(INOUT)   :: Bar

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (Bar%ShowValue /= Progress_None) RETURN

    SELECT CASE (Bar%Style)
    CASE (Progress_Continuous)
        WRITE(Output_Unit, '(A)', ADVANCE = 'NO') Bar%TrailChar
        FLUSH(Output_Unit)
    CASE (Progress_Cyclic)
        IF (Bar%Position == 0) Bar%Direction = 1
        IF (Bar%Position == Bar%Width) THEN
            Bar%Direction = -1
            Bar%Position  = Bar%Position + Bar%Direction
        END IF
        WRITE(Output_Unit, '(6A)', ADVANCE = 'NO') Bar%PrefixStr,       &
            REPEAT(Bar%LeadChar, MAX(0, Bar%Position)), Bar%TrailChar,  &
            REPEAT(Bar%LeadChar, MAX(0, Bar%Width - Bar%Position - 1)), &
            Bar%SuffixStr, C_Carriage_Return
        FLUSH(Output_Unit)
        Bar%Position = Bar%Position + Bar%Direction
    CASE (Progress_Delimited)
        IF (Bar%Position < Bar%Width) THEN
            WRITE(Output_Unit, '(6A)', ADVANCE = 'NO') Bar%PrefixStr,       &
                REPEAT(Bar%LeadChar, MAX(0, Bar%Position)), Bar%TrailChar,  &
                REPEAT(' ', MAX(0, Bar%Width - Bar%Position - 1)),          &
                Bar%SuffixStr, C_Carriage_Return
            FLUSH(Output_Unit)
            Bar%Position = Bar%Position + 1
        END IF
    CASE (Progress_Rotating)
        Bar%Position = 1 + MOD(Bar%Position, 4)
        WRITE(Output_Unit, '(4A)', ADVANCE = 'NO') Bar%PrefixStr, &
            Bar%CyclicChars(Bar%Position:Bar%Position), Bar%SuffixStr, C_Carriage_Return
        FLUSH(Output_Unit)
    CASE DEFAULT
        ! Simply ignore this possibility
    END SELECT

    RETURN

END SUBROUTINE ProgressBar_Next

!******************************************************************************

SUBROUTINE ProgressBar_Value(Bar, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To show the progress bar based on value. <br>
    !  This routine can be used with only Progress_Delimited
    !   and Progress_NoCursor styles.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ProgressBar), INTENT(INOUT)   :: Bar
    tRealSP,            INTENT(IN)      :: Value    ! the value to be used for the Position of the cursor

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(80)    :: Value_String

! FLOW
    
    SELECT CASE (Bar%ShowValue)
    CASE (Progress_None)
        RETURN
    CASE (Progress_Value)
        WRITE(Value_String, Bar%FormatValue) Value
    CASE (Progress_Value_Max)
        WRITE(Value_String, Bar%FormatBoth) Value, Bar%MaxValue
    CASE DEFAULT
        ! Simply ignore this possibility
    END SELECT

    SELECT CASE (Bar%Style)
    CASE (Progress_Delimited)
        Bar%Position = Bar%Width * MAX(0.0, MIN(Value, Bar%MaxValue)) / Bar%MaxValue
        WRITE(Output_Unit, '(7A)', ADVANCE = 'NO') Bar%PrefixStr,       &
            REPEAT(Bar%LeadChar, MAX(0, Bar%Position)), Bar%TrailChar,  &
            REPEAT(' ',          MAX(0, Bar%Width - Bar%Position - 1)), &
            Bar%SuffixStr, TRIM(Value_String), C_Carriage_Return
        FLUSH(Output_Unit)

    CASE (Progress_NoCursor)
        WRITE(Output_Unit, '(5A)', ADVANCE = 'NO') Bar%PrefixStr, &
            TRIM(Value_String), C_Carriage_Return
    CASE DEFAULT
        ! Simply ignore this possibility
    END SELECT

    RETURN

END SUBROUTINE ProgressBar_Value

!******************************************************************************

END MODULE MClass_ProgressBar
    
!******************************************************************************
