
MODULE MBase_CharConv

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains routines that perform a conversion between a number and a decimal
!   string.  The routines can be categorized into 4 groups: <br>
!   - real-to-string conversion routines, <br>
!   - real-from-string conversion routines, <br>
!   - integer-to-string conversion routines, and <br>
!   - integer-from-string conversion routines. <br>
!   For real-number conversions, routines for all three common (single, double and quadruple)
!   precisions are provided.  For integer-number conversions, only routines for 32-bit and
!   64-bit integers are provided. <br>
!   <br>
! **Technical Notes for Integer-From-String Conversions**: <br>
!   When parsing a decimal string as an integer number, the string can be interpreted as a
!   *Fortran* number, a *FPlus* number or a *JSON* number.  The following list provides a
!   description of these interpretations. <br>
!   1. A *Fortran* number (*FortNum*) is strictly interpreted as an integer number that has
!      the form as: [S]N[N...] where <br>
!       - S is a sign indicator (required if negative '-', optional if positive '+'). <br>
!       - N is a decimal digit (0 through 9). Any leading zeros, leading and trailing
!           spaces are ignored. <br>
!    Unlike Fortran constants, the optional kind parameter (_k) is not allowed here. <br>
!   2. A *FPlus* number (*FortPlus*) has a slightly more relaxed rule than that of a Fortran
!      number such that any invalid characters after characters that are valid are ignored.
!      For example, -3567e23 is treated as a valid number with a value of -3567. <br>
!   3. A *JSON* number (*JsonNum*) has a slightly stricter rule than that of a Fortran number
!      such that a plus sign and leading zeroes are not allowed. <br>
!   <br>
!   <br>
! **Technical Notes for Real-From-String Conversions**: <br>
!   Similarly, when parsing a decimal string as a real number, the string can also be interpreted
!   as a *Fortran* number, a *FPlus* number or a *JSON* number.  The following list provides a
!   description of these interpretations. <br>
!   1. A *Fortran* number (*FortNum*) is strictly interpreted as a real number that has one of the
!      two following forms: <br>
!      <1> A number without exponent part -> [S]N[N...], and <br>
!      <2> A number with exponent part    -> [S]N[N...]E[S]N[N...] <br>
!          where <br>
!          - [ ] indicates an optional field. <br>
!          - S is a sign indicator (required if negative '-', optional if positive '+'). <br>
!          - N is a decimal digit (0 through 9). A decimal point (a period) may appear anywhere
!               after the sign (but before the exponent). <br>
!          - E is an exponent indicator (either 'e' or 'E'). <br>
!      The valid number is similar to "Real" Fortran constant (literal) with some small differences. <br>
!       - A whole number without a decimal point (i.e. "Integer" constant) is considered valid. <br>
!       - The optional kind parameter (e.g. 1.23_DP) is not allowed here. <br>
!      Leading and/or trailing space(s) are allowed.  For example, "  1.23" and "1.23   " are considered
!      valid.  However, no space is allowed inside the supposedly valid number.  For instance, "1 .2 3"
!      is considered NOT valid. Therefore, this routine is not totally compatible with Fortran *READ*
!      statement where spaces inside the valid number are allowed. However, this can easily be done by
!      adding an optional 'Inside Space' flag that provide an interpretation of the spaces as 'zero' or
!      'ignored'.  Then, the input will be pre-processed according to the flag.  Nonetheless, this routine
!      neglects this optional input because it will make the routine much less efficient due to the fact
!      that we will need to scan the whole string twice and we will also need to copy the input string
!      into a buffer string and working with the buffer instead of directly handling the input string. <br>
!   2. A *FPlus* number (*FortPlus*) is interpreted as a real number with more relaxed rules than
!      a *Fortran* number.  The relaxed rules consider the following numbers as valid: <br>
!      - A number expressed in the scientific format can use 'd', 'D', 'q' and 'Q' in place of 'e' or
!        'E'. <br>
!      - A number with '+' or '-' after digits (e.g. 1.23-20 or 123+50) is considered to be a valid number
!        expressed in the scientific format where an exponent indicator is omitted. <br>
!      - Digits before any invalid character encountered are treated as a valid number and any characters
!        after the first encounter (including the first invalid one) are neglected.  Therefore, for example,
!        a '12.56ax-300' string is considered to be a valid number with a value of 12.56. <br>
!   3. A *JSON* number (*JsonNum*) has a slightly stricter rule than that of a Fortran number where its
!      differences from Fortran number are as follows: <br>
!      - Leading and trailing spaces are not allowed. <br>
!      - A plus sign as the first character is not allowed. <br>
!      - Leading zero(s) is not allowed (if 0 is the first character, the second one must either be a
!        period or an exponent indicator.) <br>
!      - A period must be followed by at least one digit. <br>
!   <br>

!** USE STATEMENTS:
    USE MBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    !--------------------------------------------------
    !   Single-Precision Real-Character Conversions
    !--------------------------------------------------
    ! Real-To-String
    PUBLIC :: RealSP_ToString_DragonBox,    RealSP_ToString_Ryu
    PUBLIC :: RealSP_ToString_Schubfach
    ! Real-From-String
    PUBLIC :: RealSP_FromString_FastFloat,  RealSP_FromString_LibC
    PUBLIC :: RealSP_FromString_YY,         RealSP_FromString_Lemire
    !--------------------------------------------------
    !   Double-Precision Real-Character Conversions
    !--------------------------------------------------
    ! Real-To-String
    PUBLIC :: RealDP_ToString_DragonBox,    RealDP_ToString_Ryu
    PUBLIC :: RealDP_ToString_Schubfach
    ! Real-From-String
    PUBLIC :: RealDP_FromString_FastFloat,  RealDP_FromString_LibC
    PUBLIC :: RealDP_FromString_YY,         RealDP_FromString_Lemire
    !--------------------------------------------------
    !   Quad-Precision Real-Character Conversions
    !--------------------------------------------------
    ! Real-To-String
    PUBLIC :: RealQP_ToString_DragonBox,    RealQP_ToString_Ryu
    PUBLIC :: RealQP_ToString_Schubfach
    ! Real-From-String
    PUBLIC :: RealQP_FromString_FastFloat,  RealQP_FromString_LibC
    PUBLIC :: RealQP_FromString_YY,         RealQP_FromString_Lemire
    !--------------------------------------------------
    !   32-Bit Integer ToChar Procedures
    !--------------------------------------------------
    PUBLIC :: I32_ToChar_Basic,             I32_ToChar_CC
    PUBLIC :: I32_ToChar_YY,                I32_ToChar_YYLL
    PUBLIC :: I32_ToChar_JEA
    !--------------------------------------------------
    !   64-Bit Integer ToChar Procedures
    !--------------------------------------------------
    PUBLIC :: I64_ToChar_Basic,             I64_ToChar_CC
    PUBLIC :: I64_ToChar_YY,                I64_ToChar_YYLL
    PUBLIC :: I64_ToChar_JEA
    !--------------------------------------------------
    !   32-Bit Integer FromChar Procedures
    !--------------------------------------------------
    PUBLIC :: I32_FromChar_CC_FortNum,      I32_FromChar_CC_FortPlus
    PUBLIC :: I32_FromChar_CC_JsonNum,      I32_FromChar_Lemire_FortPlus
    PUBLIC :: I32_FromChar_YY_JsonNum
    !--------------------------------------------------
    !   64-Bit Integer FromChar Procedures
    !--------------------------------------------------
    PUBLIC :: I64_FromChar_CC_FortNum,      I64_FromChar_CC_FortPlus
    PUBLIC :: I64_FromChar_CC_JsonNum,      I64_FromChar_Lemire_FortPlus
    PUBLIC :: I64_FromChar_YY_JsonNum

    PRIVATE          ! by default, hide all data and routines except those declared explicitly
    
!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! -----------------------------------------------------------------
    ! -----     options for type of number to be parsed           -----
    ! -----------------------------------------------------------------
    tSInt32, PARAMETER, PUBLIC  :: FortNum  = 1     ! strict Fortran number
    tSInt32, PARAMETER, PUBLIC  :: FPlusNum = 2     ! relaxed Fortran number
    tSInt32, PARAMETER, PUBLIC  :: JsonNum  = 3     ! JSON number

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE/GENERIC DEFINITIONS:
    !--------------------------------------------------
    !   Single-Precision Real-Character Conversions
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION RealSP_ToString_DragonBox(Number, IsScientific) RESULT(cStr)
            !^ To convert a single-precision floating-point value to a character
            !  (decimal) string using the DragonBox algorithm.
            tRealSP,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealSP_ToString_DragonBox
        !----------------------------------------------------------------------
        MODULE FUNCTION RealSP_ToString_Ryu(Number, IsScientific) RESULT(cStr)
            !^ To convert a single-precision floating-point value to a character
            !  (decimal) string using the Ryu algorithm.
            tRealSP,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealSP_ToString_Ryu
        !----------------------------------------------------------------------
        MODULE FUNCTION RealSP_ToString_Schubfach(Number, IsScientific) RESULT(cStr)
            !^ To convert a single-precision floating-point value to a character
            !  (decimal) string using the Schubfach algorithm.
            tRealSP,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealSP_ToString_Schubfach
        !----------------------------------------------------------------------
        MODULE FUNCTION RealSP_FromString_FastFloat(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a single-precision
            !  floating-point value using the FastFloat algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealSP                             :: Number   !! floating-point number
        END FUNCTION RealSP_FromString_FastFloat
        !----------------------------------------------------------------------
        MODULE FUNCTION RealSP_FromString_LibC(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a single-precision
            !  floating-point value using the LibC algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealSP                             :: Number   !! floating-point number
        END FUNCTION RealSP_FromString_LibC
        !----------------------------------------------------------------------
        MODULE FUNCTION RealSP_FromString_YY(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a single-precision
            !  floating-point value using the YY algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealSP                             :: Number   !! floating-point number
        END FUNCTION RealSP_FromString_YY
        !----------------------------------------------------------------------
        MODULE FUNCTION RealSP_FromString_Lemire(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a single-precision
            !  floating-point value using the Lemire algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealSP                             :: Number   !! floating-point number
        END FUNCTION RealSP_FromString_Lemire
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------
    !   Double-Precision Real-Character Conversions
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION RealDP_ToString_DragonBox(Number, IsScientific) RESULT(cStr)
            !^ To convert a double-precision floating-point value to a character
            !  (decimal) string using the DragonBox algorithm.
            tRealDP,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealDP_ToString_DragonBox
        !----------------------------------------------------------------------
        MODULE FUNCTION RealDP_ToString_Ryu(Number, IsScientific) RESULT(cStr)
            !^ To convert a double-precision floating-point value to a character
            !  (decimal) string using the Ryu algorithm.
            tRealDP,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealDP_ToString_Ryu
        !----------------------------------------------------------------------
        MODULE FUNCTION RealDP_ToString_Schubfach(Number, IsScientific) RESULT(cStr)
            !^ To convert a double-precision floating-point value to a character
            !  (decimal) string using the Schubfach algorithm.
            tRealDP,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealDP_ToString_Schubfach
        !----------------------------------------------------------------------
        MODULE FUNCTION RealDP_FromString_FastFloat(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a double-precision
            !  floating-point value using the FastFloat algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealDP                             :: Number   !! floating-point number
        END FUNCTION RealDP_FromString_FastFloat
        !----------------------------------------------------------------------
        MODULE FUNCTION RealDP_FromString_LibC(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a double-precision
            !  floating-point value using the LibC algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealDP                             :: Number   !! floating-point number
        END FUNCTION RealDP_FromString_LibC
        !----------------------------------------------------------------------
        MODULE FUNCTION RealDP_FromString_YY(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a double-precision
            !  floating-point value using the YY algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealDP                             :: Number   !! floating-point number
        END FUNCTION RealDP_FromString_YY
        !----------------------------------------------------------------------
        MODULE FUNCTION RealDP_FromString_Lemire(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a double-precision
            !  floating-point value using the Lemire algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealDP                             :: Number   !! floating-point number
        END FUNCTION RealDP_FromString_Lemire
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------
    !   Quad-Precision Real-Character Conversions
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION RealQP_ToString_DragonBox(Number, IsScientific) RESULT(cStr)
            !^ To convert a quadruple-precision floating-point value to a character
            !  (decimal) string using the DragonBox algorithm.
            tRealQP,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealQP_ToString_DragonBox
        !----------------------------------------------------------------------
        MODULE FUNCTION RealQP_ToString_Ryu(Number, IsScientific) RESULT(cStr)
            !^ To convert a quadruple-precision floating-point value to a character
            !  (decimal) string using the Ryu algorithm.
            tRealQP,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealQP_ToString_Ryu
        !----------------------------------------------------------------------
        MODULE FUNCTION RealQP_ToString_Schubfach(Number, IsScientific) RESULT(cStr)
            !^ To convert a quadruple-precision floating-point value to a character
            !  (decimal) string using the Schubfach algorithm.
            tRealQP,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealQP_ToString_Schubfach
        !----------------------------------------------------------------------
        MODULE FUNCTION RealQP_FromString_FastFloat(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a quadruple-precision
            !  floating-point value using the FastFloat algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealQP                             :: Number   !! floating-point number
        END FUNCTION RealQP_FromString_FastFloat
        !----------------------------------------------------------------------
        MODULE FUNCTION RealQP_FromString_LibC(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a quadruple-precision
            !  floating-point value using the LibC algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealQP                             :: Number   !! floating-point number
        END FUNCTION RealQP_FromString_LibC
        !----------------------------------------------------------------------
        MODULE FUNCTION RealQP_FromString_YY(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a quadruple-precision
            !  floating-point value using the YY algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealQP                             :: Number   !! floating-point number
        END FUNCTION RealQP_FromString_YY
        !----------------------------------------------------------------------
        MODULE FUNCTION RealQP_FromString_Lemire(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a quadruple-precision
            !  floating-point value using the Lemire algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealQP                             :: Number   !! floating-point number
        END FUNCTION RealQP_FromString_Lemire
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------
    !   32-Bit Integer ToChar Procedures
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION I32_ToChar_Basic(Number) RESULT(cStr)
            !^ To convert a 32-bit integer number to a character (decimal)
            !  string using the basic algorithm.
            tSInt32, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I32_ToChar_Basic
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_ToChar_CC(Number) RESULT(cStr)
            !^ To convert a 32-bit integer number to a character (decimal)
            !  string using the CC algorithm.
            tSInt32, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I32_ToChar_CC
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_ToChar_YY(Number) RESULT(cStr)
            !^ To convert a 32-bit integer number to a character (decimal)
            !  string using the YY algorithm.
            tSInt32, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I32_ToChar_YY
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_ToChar_YYLL(Number) RESULT(cStr)
            !^ To convert a 32-bit integer number to a character (decimal)
            !  string using the YY algorithm with large tables.
            tSInt32, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I32_ToChar_YYLL
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_ToChar_JEA(Number) RESULT(cStr)
            !^ To convert a 32-bit integer number to a character (decimal)
            !  string using the JEA algorithm.
            tSInt32, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I32_ToChar_JEA
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------
    !   64-Bit Integer ToChar Procedures
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION I64_ToChar_Basic(Number) RESULT(cStr)
            !^ To convert a 64-bit integer number to a character (decimal)
            !  string using the basic algorithm.
            tSInt64, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I64_ToChar_Basic
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_ToChar_CC(Number) RESULT(cStr)
            !^ To convert a 64-bit integer number to a character (decimal)
            !  string using the CC algorithm.
            tSInt64, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I64_ToChar_CC
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_ToChar_YY(Number) RESULT(cStr)
            !^ To convert a 64-bit integer number to a character (decimal)
            !  string using the YY algorithm.
            tSInt64, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I64_ToChar_YY
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_ToChar_YYLL(Number) RESULT(cStr)
            !^ To convert a 64-bit integer number to a character (decimal)
            !  string using the YY algorithm with large tables.
            tSInt64, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I64_ToChar_YYLL
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_ToChar_JEA(Number) RESULT(cStr)
            !^ To convert a 64-bit integer number to a character (decimal)
            !  string using the JEA algorithm.
            tSInt64, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I64_ToChar_JEA
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------
    !   32-Bit Integer FromChar Procedures
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION I32_FromChar_CC_FortNum(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 32-bit integer value using the
            !  CC algorithm where the string is interpreted as a Fortran number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt32                             :: Number   !! number
        END FUNCTION I32_FromChar_CC_FortNum
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_FromChar_CC_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 32-bit integer value using the
            !  CC algorithm where the string is interpreted as a FPlus number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt32                             :: Number   !! number
        END FUNCTION I32_FromChar_CC_FortPlus
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_FromChar_CC_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 32-bit integer value using the
            !  CC algorithm where the string is interpreted as a JSON number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt32                             :: Number   !! number
        END FUNCTION I32_FromChar_CC_JsonNum
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_FromChar_Lemire_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 32-bit integer value using the
            !  Lemire algorithm where the string is interpreted as a FPlus number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt32                             :: Number   !! number
        END FUNCTION I32_FromChar_Lemire_FortPlus
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_FromChar_YY_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 32-bit integer value using the
            !  YY algorithm where the string is interpreted as a JSON number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt32                             :: Number   !! number
        END FUNCTION I32_FromChar_YY_JsonNum
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------
    !   64-Bit Integer FromChar Procedures
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION I64_FromChar_CC_FortNum(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 64-bit integer value using the
            !  CC algorithm where the string is interpreted as a Fortran number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt64                             :: Number   !! number
        END FUNCTION I64_FromChar_CC_FortNum
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_FromChar_CC_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 64-bit integer value using the
            !  CC algorithm where the string is interpreted as a FPlus number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt64                             :: Number   !! number
        END FUNCTION I64_FromChar_CC_FortPlus
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_FromChar_CC_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 64-bit integer value using the
            !  CC algorithm where the string is interpreted as a JSON number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt64                             :: Number   !! number
        END FUNCTION I64_FromChar_CC_JsonNum
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_FromChar_Lemire_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 64-bit integer value using the
            !  Lemire algorithm where the string is interpreted as a FPlus number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt64                             :: Number   !! number
        END FUNCTION I64_FromChar_Lemire_FortPlus
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_FromChar_YY_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 64-bit integer value using the
            !  YY algorithm where the string is interpreted as a JSON number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt64                             :: Number   !! number
        END FUNCTION I64_FromChar_YY_JsonNum
        !----------------------------------------------------------------------
    END INTERFACE

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

END MODULE MBase_CharConv

!******************************************************************************
