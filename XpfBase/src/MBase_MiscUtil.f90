#define HAVE_FMLIB
#define HAVE_MPFUN

MODULE MBase_MiscUtil

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various routines that are not quite fit into other existing modules
!   or some are legacy code with different interfaces from routines in other existing modules
!   with the same functionality.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_IOHandlers
    USE MBase_SIntUtil,     ONLY: ToChar => ToDecStrSigned
    USE MBase_WriteUtil
#ifdef  HAVE_FMLIB
    USE MLib_FMLib,         ONLY: FM_SET, FM_SETVAR
#endif
#ifdef  HAVE_MPFUN
    USE MLib_MPFun,         ONLY: MPINIT, MPDPW
#endif

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! machine-parameter procedures
    PUBLIC :: Machine_Real_Parameter
    PUBLIC :: Machine_Integer_Parameter
    ! bit shift procedures
    PUBLIC :: RA_Shift
    PUBLIC :: RL_Shift
    PUBLIC :: L_Shift
    ! other procedures
    PUBLIC :: InRange
    PUBLIC :: NotInRange
    PUBLIC :: LimitValue
    PUBLIC :: MinMax
    PUBLIC :: SwapArray
    PUBLIC :: MaxValLocation
    PUBLIC :: FindLowerIndex
    PUBLIC :: Index_Init
    PUBLIC :: Is_Little_Endian
#ifdef  HAVE_FMLIB
    PUBLIC :: FMLib_Init
#endif
#ifdef  HAVE_MPFUN
    PUBLIC :: MPFun_Init
#endif
    ! elemental operators
    PUBLIC :: OPERATOR(.EQUAL.)
    ! elemental procedures
    PUBLIC :: JulianDate
    PUBLIC :: Swap
    PUBLIC :: SetTimeLimit
    PUBLIC :: IsOddNumber
    PUBLIC :: IsEvenNumber
    PUBLIC :: DisplayString
    PUBLIC :: DisplayNumberAndString
    PUBLIC :: SameString
    PUBLIC :: ConvertToUpperCase
    PUBLIC :: TrimSigDigits

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: FmtA="(1X,A)"
    ! constants for string manipulation
    tCharStar, PARAMETER    :: UpperCase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    tCharStar, PARAMETER    :: LowerCase = 'abcdefghijklmnopqrstuvwxyz'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE FindLowerIndex
        !^ **Function Interface**: FindLowerIndex <br>
        ! **Purpose**:  To find index of the lower limit. <br>
        !  **Usage**: <br>
        !   --->    Indx = FindLowerIndex(Limits, Val) <br>
        !  **Note**:  The index is I where Val > Limits(I) and Val <= Limits(I+1). <br>
        MODULE PROCEDURE FindILowerIndex
        MODULE PROCEDURE FindRLowerIndex
    END INTERFACE
    INTERFACE MinMax
        !^ **Subroutine Interface**: MinMax <br>
        ! **Purpose**:  To compare values and return the minimum and maximum values. <br>
        !  **Usage**: <br>
        !   --->    CALL MinMax(AVal, BVal, MinVal, MaxVal) <br>
        !   --->    CALL MinMax(Array, MinVal, MaxVal) <br>
        MODULE PROCEDURE MinMax_INT
        MODULE PROCEDURE MinMax_REAL
        MODULE PROCEDURE MinMax_Array_INT
        MODULE PROCEDURE MinMax_Array_REAL
    END INTERFACE
    INTERFACE NotInRange
        !^ **Function Interface**: NotInRange <br>
        ! **Purpose**:  To check if the value is in the specified range or not.
        !       Return true if it is NOT.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = NotInRange(Val, UppLimit, LowLimit) <br>
        !   --->    IF (.NOT.NotInRange(Val, UppLimit, LowLimit)) DoSomething <br>
        MODULE PROCEDURE FindILowerIndex
        MODULE PROCEDURE FindRLowerIndex
        MODULE PROCEDURE NotInRange_REAL
        MODULE PROCEDURE NotInRange_INT
        MODULE PROCEDURE NotInRange_LONG
    END INTERFACE
    INTERFACE InRange
        !^ **Function Interface**: InRange <br>
        ! **Purpose**:  To check if the value is in the specified range or not.
        !       Return true if it is.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = InRange(Val, UppLimit, LowLimit) <br>
        !   --->    IF (.NOT.InRange(Val, UppLimit, LowLimit)) DoSomething <br>
        MODULE PROCEDURE InRange_REAL
        MODULE PROCEDURE InRange_INT
        MODULE PROCEDURE InRange_LONG
    END INTERFACE
    INTERFACE SwapArray
        !^ **Subroutine Interface**: SwapArray <br>
        ! **Purpose**:  To swap values of two arrays with specified staring and ending indices. <br>
        !  **Usage**: <br>
        !   --->    CALL SwapArray(IBegin, IEnd, XArr, YArr) <br>
        MODULE PROCEDURE SwapArrayReal
        MODULE PROCEDURE SwapArrayInteger
    END INTERFACE
    INTERFACE JulianDate
        !^ **Function Interface**: JulianDate <br>
        ! **Purpose**:  To find the appropriate Julian Day value for the given month and day. <br>
        !  **Usage**: <br>
        !   --->    Date = JulianDate(DayOfMonth, Month) <br>
        !   --->    Date = JulianDate(DayOfMonth, Month, 1) ! leap year set to 1 <br>
        MODULE PROCEDURE JulianDateWithoutLeapYear
        MODULE PROCEDURE JulianDateWithLeapYear
    END INTERFACE
    INTERFACE Swap
        !^ **Subroutine Interface**: Swap <br>
        ! **Purpose**:  To swap values. <br>
        !  **Usage**: <br>
        !   --->    CALL Swap(AVal, BVal) <br>
        MODULE PROCEDURE SwapInteger
        MODULE PROCEDURE SwapReal
        MODULE PROCEDURE SwapComplex
        MODULE PROCEDURE SwapCharacterString
    END INTERFACE
    INTERFACE OPERATOR (.EQUAL.)
        !^ **Operator Overload**: OPERATOR(.EQUAL.) <br>
        ! **Purpose**:  To compare values A and B and return true if they are relatively equal. <br>
        !  **Usage**: <br>
        !   --->    Flag = (AVal .EQUAL. BvAL) <br>
        !   --->    IF (.NOT.(AVal .EQUAL. BvAL)) DoSomething <br>
        MODULE PROCEDURE IsREqualR
        MODULE PROCEDURE IsREqualI
        MODULE PROCEDURE IsIEqualR
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE DisplayString(String)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To display string on screen during program execution.

!** REFERENCES:
    ! Based on code from ASHRAE's Loads Toolkit

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: String  ! String to be displayed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! none

!** FLOW

    WRITE(*,FmtA) TRIM(String)

    RETURN

END SUBROUTINE DisplayString

!******************************************************************************

SUBROUTINE DisplayNumberAndString(Number,String)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To display (at set point on screen for screen positioning models) card images
    !  during program parsing.

!** REFERENCES:
    ! Based on code from ASHRAE's Loads Toolkit

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tFloat,     INTENT(IN)   :: Number  ! number to be displayed
    tCharStar, INTENT(IN)   :: String  ! String to be displayed

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharAlloc  :: NumString

!** FLOW

    NumString = TrimSigDigits(Number, 4_kIndex)

    WRITE(*,FmtA) TRIM(String) // NumString

    RETURN

END SUBROUTINE DisplayNumberAndString

!******************************************************************************

PURE FUNCTION SameString(TestString1,TestString2) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ This function returns true if the two strings are equal (case insensitively).

!** METHODOLOGY EMPLOYED:
    ! Make both strings uppercase.  Do internal compare.

!** REFERENCES:
    ! Based on code from ASHRAE's Loads Toolkit

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: TestString1  ! First String to Test
    tCharStar, INTENT(IN)   :: TestString2  ! Second String to Test
    tLogical                :: Flag

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LEN_TRIM(TestString1) /= LEN_TRIM(TestString2)) THEN
        Flag = FalseVal
    ELSEIF (TestString1 == TestString2) THEN
        Flag = TrueVal
    ELSE
        Flag = ConvertToUpperCase(TestString1) == ConvertToUpperCase(TestString2)
    ENDIF

    RETURN

END FUNCTION SameString

!******************************************************************************

PURE FUNCTION ConvertToUpperCase(InputString) RESULT (ResultString)

!** PURPOSE OF THIS SUBROUTINE:
    !^ This function returns the Upper Case representation of the InputString.

!** METHODOLOGY EMPLOYED:
    ! Uses the Intrinsic SCAN function to scan the lowercase representation of
    ! characters (DataGlobal) for each character in the given string.

!** REFERENCES:
    ! Based on code from ASHRAE's Loads Toolkit

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: InputString  ! Input String
    tCharAlloc              :: ResultString ! Result String

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tIndex      :: Count              ! Loop Counter
    tIndex      :: Pos                ! Position in String representation
    tIndex      :: LengthInputString  ! Length (trimmed) of InputString

!** FLOW

    ! get length
    LengthInputString = LEN_TRIM(InputString)

    ! set output string with correct length
    ResultString = REPEAT(' ',LengthInputString)

    ! convert to upper case
    DO Count = 1, LengthInputString
        Pos = SCAN(LowerCase,InputString(Count:Count))
        IF (Pos /= 0) THEN
            ResultString(Count:Count) = UpperCase(Pos:Pos)
        ELSE
            ResultString(Count:Count) = InputString(Count:Count)
        ENDIF
    END DO

    RETURN

END FUNCTION ConvertToUpperCase

!******************************************************************************

FUNCTION TrimSigDigits(Value,SigDigits) RESULT(OutputString)

!** PURPOSE OF THIS SUBROUTINE:
    !^ This function accepts a number as a parameter as well as the number of
    !  significant digits after the decimal point to report and returns a string
    !  that is appropriate.

!** REFERENCES:
    ! Based on code from ASHRAE's Loads Toolkit

!** FUNCTION ARGUMENT DEFINITIONS:
    tFloat, INTENT(IN)  :: Value
    tIndex, INTENT(IN)  :: SigDigits
    tCharAlloc          :: OutputString

!** FUNCTION PARAMETER DEFINITIONS:
    tChar,   PARAMETER  :: Fmt = 'E'
#ifdef AppQuadruple
    tSInt32, PARAMETER  :: CLen   = 20
    tSInt32, PARAMETER  :: Width  = 17
    tSInt32, PARAMETER  :: Digits = 9
#else
#   ifdef AppDouble
    tSInt32, PARAMETER  :: CLen   = 30
    tSInt32, PARAMETER  :: Width  = 26
    tSInt32, PARAMETER  :: Digits = 17
#   else
    tSInt32, PARAMETER  :: CLen   = 50
    tSInt32, PARAMETER  :: Width  = 44
    tSInt32, PARAMETER  :: Digits = 35
#   endif
#endif

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tIndex          :: Pos
    tSInt32         :: NumLen
    tCharLen(CLen)  :: String
    tCharAlloc      :: EString

!** FLOW

    ! convert value to string
    NumLen = WriteFormat(Value, String, Fmt, B2D_DragonBox, Width, Digits)

    ! determine the exponent part
    Pos = INDEX(String, 'E')
    IF (Pos > 0) THEN
        EString = String(Pos:NumLen)
    ELSE
        EString = ' '
    ENDIF

    ! find the decimal point and set output string
    Pos = INDEX(String,'.')
    OutputString = String(1:Pos+SigDigits) // EString

    RETURN

END FUNCTION TrimSigDigits

!******************************************************************************

PURE FUNCTION FindRLowerIndex(Limit,Value) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find index of the lower limit.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat, INTENT(IN)   :: Limit(:)     ! limits
    tFloat, INTENT(IN)   :: Value        ! actual value
    tIndex              :: Index        ! index of the lower limit

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: LSize        ! size of the limits
    tIndex      :: I            ! working index

!** FLOW

    ! check sizes of the limits
    LSize = SIZE(Limit)

    ! find index
    DO I = 1,LSize-1
        IF ((Value > Limit(I)).AND.(Value <= Limit(I+1))) THEN
            Index = I
        END IF
    END DO

    RETURN

END FUNCTION FindRLowerIndex

!**************************************************************************************

PURE FUNCTION FindILowerIndex(Limit,Value) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find index of the lower limit.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: Limit(:)     ! limits
    tSInt32,  INTENT(IN)    :: Value        ! actual value
    tIndex                  :: Index        ! index of the lower limit

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: LSize        ! size of the limits
    tIndex      :: I            ! working index

!** FLOW

    ! check sizes of the limits
    LSize = SIZE(Limit)

    ! find index
    DO I = 1,LSize-1
        IF ((Value > Limit(I)).AND.(Value <= Limit(I+1))) THEN
            Index = I
        END IF
    END DO

    RETURN

END FUNCTION FindILowerIndex

!**************************************************************************************

PURE SUBROUTINE MinMax_REAL(A, B, MIN, MAX)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare values of A and B and return the minimum and maximum values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat, INTENT(IN)   :: A
    tFloat, INTENT(IN)   :: B
    tFloat, INTENT(OUT)  :: MIN
    tFloat, INTENT(OUT)  :: MAX

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! none

!** FLOW

    IF (A <= B) THEN
        MIN = A
        MAX = B
    ELSE
        MIN = B
        MAX = A
    END IF

    RETURN

END SUBROUTINE  MinMax_REAL

!******************************************************************************

PURE SUBROUTINE MinMax_Array_REAL(A, MIN, MAX)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare values of an array A and return the minimum and maximum values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat, INTENT(IN)   :: A(:)
    tFloat, INTENT(OUT)  :: MIN
    tFloat, INTENT(OUT)  :: MAX

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: N, I

!** FLOW

    ! determine number of A components
    N = SIZE(A)
    ! initialize maximum and minimum x, y and z values of polygon
    MAX = A(1)
    MIN = MAX
    ! determine maximum and minimum x, y and z values of polygon
    DO I = 2, N
        ! find maximum and minimum values
        IF (A(I).GT.MAX) THEN
            MAX = A(I)
        ELSEIF (A(I).LT.MIN) THEN
            MIN = A(I)
        END IF
    END DO

    RETURN

END SUBROUTINE  MinMax_Array_REAL

!******************************************************************************

PURE SUBROUTINE MinMax_INT(A, B, MIN, MAX)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare values of A and B and return the minimum and maximum values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: A
    tSInt32,  INTENT(IN)    :: B
    tSInt32,  INTENT(OUT)   :: MIN
    tSInt32,  INTENT(OUT)   :: MAX

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! none

!** FLOW

    IF (A <= B) THEN
        MIN = A
        MAX = B
    ELSE
        MIN = B
        MAX = A
    END IF

    RETURN

END SUBROUTINE  MinMax_INT

!******************************************************************************

PURE SUBROUTINE MinMax_Array_INT(A, MIN, MAX)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare values of an array A and return the minimum and maximum values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: A(:)
    tSInt32,  INTENT(OUT)   :: MIN
    tSInt32,  INTENT(OUT)   :: MAX

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: N, I

!** FLOW

    ! determine number of A components
    N = SIZE(A)
    ! initialize maximum and minimum x, y and z values of polygon
    MAX = A(1)
    MIN = MAX
    ! determine maximum and minimum x, y and z values of polygon
    DO I = 2, N
        ! find maximum and minimum values
        IF (A(I).GT.MAX) THEN
            MAX = A(I)
        ELSEIF (A(I).LT.MIN) THEN
            MIN = A(I)
        END IF
    END DO

    RETURN

END SUBROUTINE  MinMax_Array_INT

!******************************************************************************

PURE FUNCTION NotInRange_REAL(Value, UppLimit, LowLimit) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check if the value is in the specified range or not.
    !  Return true if it is NOT.  Otherwise, return false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat, INTENT(IN)   :: Value
    tFloat, INTENT(IN)   :: UppLimit
    tFloat, INTENT(IN)   :: LowLimit
    tLogical            :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF ((Value < LowLimit).OR.(Value > UppLimit)) THEN
        OutVal = TrueVal
    ELSE
        OutVal = FalseVal
    END IF

    RETURN

END FUNCTION NotInRange_REAL

!******************************************************************************

PURE FUNCTION NotInRange_INT(Value, UppLimit, LowLimit) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check if the value is in the specified range or not.
    !  Return true if it is NOT.  Otherwise, return false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: Value
    tSInt32,  INTENT(IN)    :: UppLimit
    tSInt32,  INTENT(IN)    :: LowLimit
    tLogical                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF ((Value < LowLimit).OR.(Value > UppLimit)) THEN
        OutVal = TrueVal
    ELSE
        OutVal = FalseVal
    END IF

    RETURN

END FUNCTION NotInRange_INT

!******************************************************************************

PURE FUNCTION NotInRange_LONG(Value, UppLimit, LowLimit) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check if the value is in the specified range or not.
    !  Return true if it is NOT.  Otherwise, return false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Value
    tSInt64, INTENT(IN) :: UppLimit
    tSInt64, INTENT(IN) :: LowLimit
    tLogical            :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF ((Value < LowLimit).OR.(Value > UppLimit)) THEN
        OutVal = TrueVal
    ELSE
        OutVal = FalseVal
    END IF

    RETURN

END FUNCTION NotInRange_LONG

!******************************************************************************

PURE FUNCTION InRange_REAL(Value, UppLimit, LowLimit) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check if the value is in the specified range or not.
    !  Return true if it is.  Otherwise, return false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat, INTENT(IN)   :: Value
    tFloat, INTENT(IN)   :: UppLimit
    tFloat, INTENT(IN)   :: LowLimit
    tLogical            :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF ((Value < LowLimit).OR.(Value > UppLimit)) THEN
        OutVal = FalseVal
    ELSE
        OutVal = TrueVal
    END IF

    RETURN

END FUNCTION InRange_REAL

!******************************************************************************

PURE FUNCTION InRange_INT(Value, UppLimit, LowLimit) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check if the value is in the specified range or not.
    !  Return true if it is.  Otherwise, return false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: Value
    tSInt32,  INTENT(IN)    :: UppLimit
    tSInt32,  INTENT(IN)    :: LowLimit
    tLogical                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF ((Value < LowLimit).OR.(Value > UppLimit)) THEN
        OutVal = FalseVal
    ELSE
        OutVal = TrueVal
    END IF

    RETURN

END FUNCTION InRange_INT

!******************************************************************************

PURE FUNCTION InRange_LONG(Value, UppLimit, LowLimit) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check if the value is in the specified range or not.
    !  Return true if it is.  Otherwise, return false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Value
    tSInt64, INTENT(IN) :: UppLimit
    tSInt64, INTENT(IN) :: LowLimit
    tLogical            :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF ((Value < LowLimit).OR.(Value > UppLimit)) THEN
        OutVal = FalseVal
    ELSE
        OutVal = TrueVal
    END IF

    RETURN

END FUNCTION InRange_LONG

!******************************************************************************

PURE FUNCTION LimitValue(InValue, UppLimit, LowLimit) RESULT (OutValue)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check if the value is in the specified range.  If not,
    !  limit its value to the specified upper or lower bound limit.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat, INTENT(IN)   :: InValue
    tFloat, INTENT(IN)   :: UppLimit
    tFloat, INTENT(IN)   :: LowLimit
    tFloat              ::  OutValue

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (InValue < LowLimit) THEN
        OutValue = LowLimit
    ELSEIF (InValue > UppLimit) THEN
        OutValue = UppLimit
    ELSE
        OutValue = InValue
    END IF

    RETURN

END FUNCTION LimitValue

!******************************************************************************

FUNCTION MaxValLocation(Array,FirstLoc,MaxVal) RESULT(MaxLoc)

!** PURPOSE OF THIS SUBROUTINE
    !^ To find location of maximum value in an array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat,  INTENT(IN)      :: Array(:)     ! input array
    tIndex, INTENT(IN)      :: FirstLoc     ! first position of actual array
    tFloat,  INTENT(INOUT)   :: MaxVal       ! maximum value
    tIndex                  :: MaxLoc       ! position of maximum value relative to
                                            ! first position

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: N, I, IM1

!** FLOW:

    N = SIZE(Array)
    IM1 = 1
    DO I = 2, N
        ! search for index IM of maximum array value
        IF (ABS(Array(I)) > ABS(Array(IM1))) THEN
            IM1 = I
        END IF
    END DO
    MaxVal = ABS(Array(IM1))
    MaxLoc = IM1 + FirstLoc - 1

    RETURN

END FUNCTION MaxValLocation

!******************************************************************************

PURE FUNCTION Machine_Real_Parameter(Index) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a floating point machine dependent parameter
    !  depending on the specified index.

!** METHODOLOGY:
    ! This routine can be used to obtain machine-dependent parameters for the
    ! local machine environment.  It is a function subprogram with one
    ! (input) argument, and can be referenced as follows:
    !
    ! Number = MachineRPar(Index)
    !
    ! where Index = 1, ..., 5.  The (output) value of Number above is determined by
    ! the (input) value of Index.  The results for various values of Index are
    ! discussed below.
    !
    ! MachineRPar(1) = B**(EMIN-1), the smallest positive magnitude.
    ! MachineRPar(2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
    ! MachineRPar(3) = B**(-T), the smallest relative spacing.
    ! MachineRPar(4) = B**(1-T), the largest relative spacing.
    ! MachineRPar(5) = LOG10(B)
    !
    ! where B is the base for floating point number,
    !       T is the number of base B digits
    !       EMIN is the smallest exponent E, and
    !       EMAX is the largest exponent E.

!** REFERENCE:
    ! P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
    !   a portable library, ACM Transactions on Mathematical
    !   Software 4, 2 (June 1978), pp. 177-188.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: Index
    tFloat                  :: Number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,  PARAMETER     :: BaseNum = RADIX(One) ! the base for floating point (double precision) number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT CASE (Index)
    CASE (1)
        ! the smallest positive magnitude
        Number = Small
    CASE (2)
        ! the largest magnitude
        Number = Large
    CASE (3)
        ! the smallest relative spacing
        Number = MachineEps/REAL(BaseNum, KIND=kFloat)
    CASE (4)
        ! the largest relative spacing
        Number = MachineEps
    CASE (5)
        ! LOG10(B)
        Number = LOG10(REAL(BaseNum, KIND=kFloat))
    END SELECT

    RETURN

END FUNCTION Machine_Real_Parameter

!******************************************************************************

PURE FUNCTION Machine_Integer_Parameter(Index) RESULT (Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a integer machine dependent parameter
    !  depending on the specified index.

!** METHODOLOGY:
    ! This routine can be used to obtain machine-dependent parameters for the
    ! local machine environment. It is a function subprogram with one
    ! (input) argument and can be referenced as follows:
    !
    ! K = I1MACH(I)
    !
    ! where I=1,...,16. The (output) value of K above is determined by
    ! the (input) value of I. The results for various values of I are
    ! discussed below.
    !
    ! I/O unit numbers:
    ! I1MACH( 1) = the standard input unit.
    ! I1MACH( 2) = the standard output unit.
    ! I1MACH( 3) = the standard punch unit.
    ! I1MACH( 4) = the standard error message unit.
    !
    ! Words:
    ! I1MACH( 5) = the number of bits per integer storage unit.
    ! I1MACH( 6) = the number of characters per integer storage unit.
    !
    ! Integers:
    ! assume integers are represented in the S-digit, base-A form
    !
    ! sign ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
    !
    ! where 0 <= X(I) < A for I=0,...,S-1.
    ! I1MACH( 7) = A, the base.
    ! I1MACH( 8) = S, the number of base-A digits.
    ! I1MACH( 9) = A**S - 1, the largest magnitude.
    !
    ! Floating-Point Numbers:
    ! Assume floating-point numbers are represented in the T-digit,
    ! base-B form
    ! sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
    !
    ! where 0 <= X(I) < B for I=1,...,T,
    ! 0 < X(1), and EMIN <= E <= EMAX.
    ! I1MACH(10) = B, the base.
    !
    ! Single-Precision:
    ! I1MACH(11) = T, the number of base-B digits.
    ! I1MACH(12) = EMIN, the smallest exponent E.
    ! I1MACH(13) = EMAX, the largest exponent E.
    !
    ! Double-Precision:
    ! I1MACH(14) = T, the number of base-B digits.
    ! I1MACH(15) = EMIN, the smallest exponent E.
    ! I1MACH(16) = EMAX, the largest exponent E.

!** REFERENCE:
    ! P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
    !   a portable library, ACM Transactions on Mathematical
    !   Software 4, 2 (June 1978), pp. 177-188.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: Index
    tSInt32                 :: Number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,  PARAMETER     :: BitInt   = BIT_SIZE(1)           ! the number of bits per integer storage unit
    tSInt32,  PARAMETER     :: BaseInt  = RADIX(1)              ! the base for integer number
    tSInt32,  PARAMETER     :: BaseNum  = RADIX(1.0D0)          ! the base for floating point number
    tSInt32,  PARAMETER     :: IntHuge  = HUGE(1)               ! the largest integer number
    tSInt32,  PARAMETER     :: IntDigit = DIGITS(1)             ! the number of significant digits for
                                                                !   integer number
    tSInt32,  PARAMETER     :: DPDigit  = DIGITS(1.0D0)         ! the number of significant digits for
                                                                !   double-precision floating point number
    tSInt32,  PARAMETER     :: SPDigit  = DIGITS(1.0E0)         ! the number of significant digits for
                                                                !   single-precision floating point number
    tSInt32,  PARAMETER     :: DPExpMin = MINEXPONENT(1.0D0)    ! the smallest exponent E for
                                                                !   double-precision floating point number
    tSInt32,  PARAMETER     :: DPExpMax = MAXEXPONENT(1.0D0)    ! the largest exponent E for
                                                                !   double-precision floating point number
    tSInt32,  PARAMETER     :: SPExpMin = MINEXPONENT(1.0E0)    ! the smallest exponent E for
                                                                !   single-precision floating point number
    tSInt32,  PARAMETER     :: SPExpMax = MAXEXPONENT(1.0E0)    ! the largest exponent E for
                                                                !   single-precision floating point number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! NA

!** FLOW:

    SELECT CASE (Index)
    CASE (1)
        ! the standard input unit
        Number = 5
    CASE (2)
        ! the standard output unit
        Number = 6
    CASE (3)
        ! the standard punch unit
        Number = 6
    CASE (4)
        ! the standard error message unit
        Number = 6
    CASE (5)
        ! the number of bits per integer storage unit
        Number = BitInt
    CASE (6)
        ! the number of characters per integer storage unit
        Number = 4
    CASE (7)
        ! the base for integer number
        Number = BaseInt
    CASE (8)
        ! the number of significant digits for integer number
        Number = IntDigit
    CASE (9)
        ! the largest integer number
        Number = IntHuge
    CASE (10)
        ! the base for floating point number
        Number = BaseNum
    CASE (11)
        ! the number of significant digits for single-precision floating point number
        Number = SPDigit
    CASE (12)
        ! the smallest exponent E for single-precision floating point number
        Number = SPExpMin
    CASE (13)
        ! the largest exponent E for single-precision floating point number
        Number = SPExpMax
    CASE (14)
        ! the number of significant digits for double-precision floating point number
        Number = DPDigit
    CASE (15)
        ! the smallest exponent E for double-precision floating point number
        Number = DPExpMin
    CASE (16)
        ! the largest exponent E for double-precision floating point number
        Number = DPExpMax
    END SELECT

    RETURN

END FUNCTION Machine_Integer_Parameter

!*************************************************************

PURE SUBROUTINE SwapArrayReal(IBegin,IEnd,AVal,BVal)

!** PURPOSE OF THIS SUBROUTINE
    !^ To swap values of two arrays with specified staring and ending indices.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)      :: IBegin       ! starting index
    tIndex, INTENT(IN)      :: IEnd         ! ending index
    tFloat,  INTENT(INOUT)   :: AVal(:)      ! values of A
    tFloat,  INTENT(INOUT)   :: BVal(:)      ! values of B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tFloat      :: TempVal
    tIndex      :: Index

!** FLOW:

    ! swap (values of) AVal and BVal
    DO Index = IBegin, IEnd
        TempVal = AVal(Index)
        AVal(Index) = BVal(Index)
        BVal(Index) = TempVal
    END DO

    RETURN

END SUBROUTINE SwapArrayReal

!******************************************************************************

PURE SUBROUTINE SwapArrayInteger(IBegin,IEnd,AVal,BVal)

!** PURPOSE OF THIS SUBROUTINE
    !^ To swap values of two arrays with specified staring and ending indices.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: IBegin       ! starting index
    tIndex,   INTENT(IN)    :: IEnd         ! ending index
    tSInt32,  INTENT(INOUT) :: AVal(:)      ! values of A
    tSInt32,  INTENT(INOUT) :: BVal(:)      ! values of B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: TempVal
    tIndex      :: Index

!** FLOW:

    DO Index = IBegin, IEnd
        TempVal = AVal(Index)
        AVal(Index) = BVal(Index)
        BVal(Index) = TempVal
    END DO

    RETURN

END SUBROUTINE SwapArrayInteger

!******************************************************************************

PURE FUNCTION RA_Shift(Val,Pos) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE
    !^ To perform arithmetic right shift. <br>
    !  Note: this is equivalent to the syntax ">>" in C or Java.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: Val      ! value to be shifted
    tIndex,   INTENT(IN)    :: Pos      ! number of shift position
    tSInt32                 :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Res = SHIFTA(Val, Pos)

    RETURN

END FUNCTION RA_Shift

!******************************************************************************

PURE FUNCTION RL_Shift(Val,Pos) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE
    !^ To perform logical right shift. <br>
    !  Note: this is equivalent to the syntax ">>>" in C for signed integer or Java
    !        and it is equivalent to the syntax ">>" in C for unsigned integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: Val      ! value to be shifted
    tIndex,   INTENT(IN)    :: Pos      ! number of shift position
    tSInt32                 :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Res = SHIFTR(Val, Pos)

    RETURN

END FUNCTION RL_Shift

!******************************************************************************

PURE FUNCTION L_Shift(Val,Pos) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE
    !^ To perform arithmetic/logical left shift. <br>
    !  Note: this is equivalent to the syntax "<<" in C or Java

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: Val      ! value to be shifted
    tIndex,   INTENT(IN)    :: Pos      ! number of shift position
    tSInt32                 :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Res = SHIFTL(Val, Pos)

    RETURN

END FUNCTION L_Shift

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           ELEMENTAL ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ELEMENTAL FUNCTION JulianDateWithoutLeapYear(DayOfMonth,Month) RESULT(DNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To calculate the Julian day number for the given month and day.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: DayOfMonth   !! day of month
    tSInt32,  INTENT(IN)    :: Month        !! month
    tSInt32                 :: DNum         !! day number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! determine properties
    SELECT CASE (Month)
        CASE (1)
            DNum = DayOfMonth
        CASE (2)
            DNum = 31 + DayOfMonth
        CASE (3)
            DNum = 59 + DayOfMonth
        CASE (4)
            DNum = 90 + DayOfMonth
        CASE (5)
            DNum = 120 + DayOfMonth
        CASE (6)
            DNum = 151 + DayOfMonth
        CASE (7)
            DNum = 181 + DayOfMonth
        CASE (8)
            DNum = 212 + DayOfMonth
        CASE (9)
            DNum = 243 + DayOfMonth
        CASE (10)
            DNum = 273 + DayOfMonth
        CASE (11)
            DNum = 304 + DayOfMonth
        CASE (12)
            DNum = 334 + DayOfMonth
        END SELECT

    RETURN

END FUNCTION JulianDateWithoutLeapYear

!**************************************************************************************

ELEMENTAL FUNCTION JulianDateWithLeapYear(Day,Month,LeapYearValue) RESULT(DNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the appropriate Julian Day value for the given month and day.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tSInt32,  INTENT(IN)    :: Day           !! Day of Month, not validated by month
    tSInt32,  INTENT(IN)    :: Month         !! Month, 1..12
    tSInt32,  INTENT(IN)    :: LeapYearValue !! 1 if leap year indicated, 0 if not
    tSInt32                 :: DNum          !! day number

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    DNum = JulianDateWithoutLeapYear(Day,Month)
    IF (Month >= 3) THEN
        DNum = DNum + LeapYearValue
    END IF

    RETURN

END FUNCTION JulianDateWithLeapYear

!******************************************************************************

ELEMENTAL FUNCTION SetTimeLimit(TimeIn) RESULT(TimeOut)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the limit of the hour to be between 0 and 24.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tFloat, INTENT(IN)   :: TimeIn
    tFloat              :: TimeOut

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    !na

!** FLOW

    IF (TimeIn >= 24.0_kFloat) THEN
        TimeOut = TimeIn - 24.0_kFloat
    ELSEIF (TimeIn < Zero) THEN
        TimeOut = TimeIn + 24.0_kFloat
    ELSE
        TimeOut = TimeIn
    END IF

    RETURN

END FUNCTION SetTimeLimit

!******************************************************************************

ELEMENTAL FUNCTION IsREqualR(AVal, BVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare values A and B and return true if they are relatively equal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat, INTENT(IN)   :: AVal
    tFloat, INTENT(IN)   :: BVal
    tLogical            :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tFloat       :: Diff

!** FLOW

    Diff = ABS(AVal-BVal)
    IF (Diff < MachineEps) THEN
        OutVal = TrueVal
    ELSE
        OutVal = FalseVal
    END IF

    RETURN

END FUNCTION IsREqualR

!******************************************************************************

ELEMENTAL FUNCTION IsREqualI(AVal, BVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare values A and B and return true if they are relatively equal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat,    INTENT(IN)    :: AVal
    tSInt32,  INTENT(IN)    :: BVal
    tLogical                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tFloat      :: Diff

!** FLOW

    Diff = ABS(AVal-REAL(BVal, KIND=kFloat))
    IF (Diff < MachineEps) THEN
        OutVal = TrueVal
    ELSE
        OutVal = FalseVal
    END IF

    RETURN

END FUNCTION IsREqualI

!******************************************************************************

ELEMENTAL FUNCTION IsIEqualR(AVal, BVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare values A and B and return true if they are relatively equal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: AVal
    tFloat,    INTENT(IN)    :: BVal
    tLogical                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tFloat      :: Diff

!** FLOW

    Diff = ABS(REAL(AVal, KIND=kFloat)-BVal)
    IF (Diff < MachineEps) THEN
        OutVal = TrueVal
    ELSE
        OutVal = FalseVal
    END IF

    RETURN

END FUNCTION IsIEqualR

!******************************************************************************

ELEMENTAL SUBROUTINE SwapInteger(AVal,BVal)

!** PURPOSE OF THIS SUBROUTINE
    !^ To swap values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(INOUT) :: AVal ! value of A
    tSInt32,  INTENT(INOUT) :: BVal ! value of B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: TempVal

!** FLOW:

    ! swap value
    TempVal = AVal
    AVal = BVal
    BVal = TempVal

    RETURN

END SUBROUTINE SwapInteger

!******************************************************************************

ELEMENTAL SUBROUTINE SwapReal(AVal,BVal)

!** PURPOSE OF THIS SUBROUTINE
    !^ To swap values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat, INTENT(INOUT)    :: AVal ! value of A
    tFloat, INTENT(INOUT)    :: BVal ! value of B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tFloat      :: TempVal

!** FLOW:

    ! swap value
    TempVal = AVal
    AVal = BVal
    BVal = TempVal

    RETURN

END SUBROUTINE SwapReal

!******************************************************************************

ELEMENTAL SUBROUTINE SwapComplex(AVal,BVal)

!** PURPOSE OF THIS SUBROUTINE
    !^ To swap values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCmplx, INTENT(INOUT)    :: AVal ! value of A
    tCmplx, INTENT(INOUT)    :: BVal ! value of B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCmplx   :: TempVal

!** FLOW:

    ! swap value
    TempVal = AVal
    AVal = BVal
    BVal = TempVal

    RETURN

END SUBROUTINE SwapComplex

!******************************************************************************

ELEMENTAL SUBROUTINE SwapCharacterString(AVal,BVal)

!** PURPOSE OF THIS SUBROUTINE
    !^ To swap values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,           INTENT(INOUT)  :: AVal ! value of A
    tCharLen(LEN(AVal)), INTENT(INOUT)  :: BVal ! value of B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(LEN(AVal)) :: TempVal

!** FLOW:

    ! swap value
    TempVal = AVal
    AVal = BVal
    BVal = TempVal

    RETURN

END SUBROUTINE SwapCharacterString

!******************************************************************************

ELEMENTAL FUNCTION IsOddNumber(Number) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check if the number is an odd number.
    !  If it is, return true.  Otherwise, return false.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tSInt32,  INTENT(IN)    :: Number
    tLogical                :: Flag

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: Remainder

!** FLOW

    Remainder = MOD(Number,2)
    IF (Remainder == 1) THEN
        Flag = TrueVal
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION IsOddNumber

!******************************************************************************

ELEMENTAL FUNCTION IsEvenNumber(Number) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check if the number is an even number.
    !  If it is, return true.  Otherwise, return false.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tSInt32,  INTENT(IN)    :: Number
    tLogical                :: Flag

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: Remainder

!** FLOW

    Remainder = MOD(Number,2)
    IF (Remainder == 0) THEN
        Flag = TrueVal
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION IsEvenNumber

!******************************************************************************

SUBROUTINE Index_Init(N, ID)

!** PURPOSE OF THIS SUBROUTINE
    !^ To initialize indices.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)      :: N
    tIndex, INTENT(INOUT)   :: ID(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    FORALL (I=1:N) ID(I) = I

    RETURN

END SUBROUTINE Index_Init

!**********************************************************************

PURE FUNCTION Is_Little_Endian() RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check if the current machine is little-endian or big-endian.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLogical    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (1_kInt16 == TRANSFER([1_kInt8, 0_kInt8], 0_kInt16))

    RETURN

END FUNCTION Is_Little_Endian

!******************************************************************************

#ifdef  HAVE_FMLIB
SUBROUTINE FMLib_Init(NPrec, KW)

!** PURPOSE OF THIS SUBROUTINE
    !^ To initialize the FMLIB library.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  OPTIONAL, INTENT(IN)  :: NPrec    !! precision for FM, IM and ZM types
    tSInt32,  OPTIONAL, INTENT(IN)  :: KW       !! output unit for FMLib

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: NDigits
    tSInt32     :: ErrOutUnit
    tCharAlloc  :: VarOption

! FLOW

    ! get parameters
    SET_OPTION(NDigits, 100, NPrec)
    SET_OPTION(ErrOutUnit, GetNewIOUnit(), KW)
    VarOption = 'KW = ' // ToChar(ErrOutUnit)

    ! set parameters
    CALL FM_SET(NDigits)
    CALL FM_SETVAR(VarOption)
    CALL FM_SETVAR('JFORMZ = 3')

    ! set global flag
    Is_FMLib_Initialized = TrueVal

    RETURN

END SUBROUTINE FMLib_Init
#endif

!**********************************************************************

#ifdef  HAVE_MPFUN
SUBROUTINE MPFun_Init(NPrec)

!** PURPOSE OF THIS SUBROUTINE
    !^ To initialize the MPFUN library.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  OPTIONAL, INTENT(IN)  :: NPrec    !! precision for MPR and MPC types

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: NDigits
    tSInt32     :: NWords

! FLOW

    ! get parameters
    SET_OPTION(NDigits, 500, NPrec)
    NWords = INT((NDigits/MPDPW) + 2)

    ! set parameters
    CALL MPINIT(NWords)

    ! set global flag
    Is_MPFun_Initialized = TrueVal

    RETURN

END SUBROUTINE MPFun_Init
#endif

!**********************************************************************

END MODULE MBase_MiscUtil

!******************************************************************************
