
MODULE MBase_CharUtil

!^ **PURPOSE OF THIS MODULE**: <br>
    ! This module contains various utility routines and parameters relating to a character. <br>

!** USE STATEMENTS:
    USE MBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! character inquiry
    PUBLIC :: Is_Character_In_Range
    PUBLIC :: Is_Character_Letter
    PUBLIC :: Is_Character_Lowercase_Letter
    PUBLIC :: Is_Character_Uppercase_Letter
    PUBLIC :: Is_Character_Digit
    PUBLIC :: Is_Character_AlphaNum
    PUBLIC :: Is_Character_ASCII
    PUBLIC :: Is_Character_Blank
    PUBLIC :: Is_Character_WhiteSpace
    PUBLIC :: Is_Character_Control
    PUBLIC :: Is_Character_Graphical
    PUBLIC :: Is_Character_Logical
    PUBLIC :: Is_Character_Printable
    PUBLIC :: Is_Character_Sign
    PUBLIC :: Is_Character_Period
    PUBLIC :: Is_Character_Exponent
    PUBLIC :: Is_Character_HexDigit
    PUBLIC :: Is_Character_OctDigit
    PUBLIC :: Is_Character_Punctuation
    PUBLIC :: Is_Character_Numeric
    PUBLIC :: Is_Character_Integer
    ! character manipulation
    PUBLIC :: ToLowerCase
    PUBLIC :: ToUpperCase

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tCharParam          :: ModName = 'MBase_CharUtil'
    !------------------------------------------------------------------
    !-----      parameters for an individual character            -----
    !------------------------------------------------------------------
    !% back space character
    tCharParam, PUBLIC  :: CHR_BACKSPACE         = ACHAR(8)
    !% horizontal tab character
    tCharParam, PUBLIC  :: CHR_TAB               = ACHAR(9)
    !% new line character
    tCharParam, PUBLIC  :: CHR_NEWLINE           = ACHAR(10)
    !% vertical tab character
    tCharParam, PUBLIC  :: CHR_VERTICAL_TAB      = ACHAR(11)
    !% form feed character
    tCharParam, PUBLIC  :: CHR_FORM_FEED         = ACHAR(12)
    !% carriage return character
    tCharParam, PUBLIC  :: CHR_CARRIAGE_RETURN   = ACHAR(13)
    !% space character
    tCharParam, PUBLIC  :: CHR_SPACE             = ACHAR(32)
    !% double quote character
    tCharParam, PUBLIC  :: CHR_DOUBLEQUOTE       = ACHAR(34)
    !% apostrophe character (single quotation mark)
    tCharParam, PUBLIC  :: CHR_APOSTROPHE        = ACHAR(39)
    !% back slash character
    tCharParam, PUBLIC  :: CHR_BACKSLASH         = ACHAR(92)
    !% underscore character
    tCharParam, PUBLIC  :: CHR_UNDERSCORE        = ACHAR(95)
    !% exclamation mark character
    tCharParam, PUBLIC  :: CHR_EXCLAMATION_MARK  = '!'
    !% period (dot) character
    tCharParam, PUBLIC  :: CHR_PERIOD            = '.'
    !% comma character
    tCharParam, PUBLIC  :: CHR_COMMA             = ','
    !% semi-colon character
    tCharParam, PUBLIC  :: CHR_SEMICOLON         = ';'
    !% colon character
    tCharParam, PUBLIC  :: CHR_COLON             = ':'
    !% left bracket character
    tCharParam, PUBLIC  :: CHR_BRACKET_LEFT      = '['
    !% right bracket character
    tCharParam, PUBLIC  :: CHR_BRACKET_RIGHT     = ']'
    !% left parentheses character
    tCharParam, PUBLIC  :: CHR_PARENTHESES_LEFT  = '('
    !% right parentheses character
    tCharParam, PUBLIC  :: CHR_PARENTHESES_RIGHT = ')'
    !% left brace character
    tCharParam, PUBLIC  :: CHR_BRACE_LEFT        = '{'
    !% right brace character
    tCharParam, PUBLIC  :: CHR_BRACE_RIGHT       = '}'
    !% slash character
    tCharParam, PUBLIC  :: CHR_SLASH             = '/'
    !% Caret character
    tCharParam, PUBLIC  :: CHR_CARET             = '^'
    !% Grave Accent character
    tCharParam, PUBLIC  :: CHR_GRAVE_ACCENT      = '`'
    !% At Sign character
    tCharParam, PUBLIC  :: CHR_AT_SIGN           = '@'
    !% Question Mark character
    tCharParam, PUBLIC  :: CHR_QUESTION_MARK     = '?'
    !% Currency Symbol character
    tCharParam, PUBLIC  :: CHR_DOLLAR_SIGN       = '$'
    !% Vertical Bar character
    tCharParam, PUBLIC  :: CHR_VERTICAL_BAR      = '|'
    !% Asterisk character
    tCharParam, PUBLIC  :: CHR_ASTERISK          = '*'
    !% Plus Sign character
    tCharParam, PUBLIC  :: CHR_PLUS_SIGN         = '+'
    !% Minus Sign character
    tCharParam, PUBLIC  :: CHR_MINUS_SIGN        = '-'
    !------------------------------------------------------------------
    !-----              parameters for character sets             -----
    !------------------------------------------------------------------
    !% (decimal) digit character set
    tCharParam, PUBLIC  :: SET_DIGITS          = '0123456789'
    !% lower-case alphabet character set
    tCharParam, PUBLIC  :: SET_ALPHABETS_LOWER = 'abcdefghijklmnopqrstuvwxyz'
    !% upper-case alphabet character set
    tCharParam, PUBLIC  :: SET_ALPHABETS_UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    !% alphabet character set (upper + lower)
    tCharParam, PUBLIC  :: SET_ALPHABETS       = SET_ALPHABETS_UPPER // SET_ALPHABETS_LOWER
    !% alphabet and number character set (alphabet + digit)
    tCharParam, PUBLIC  :: SET_ALPHANUM        = SET_ALPHABETS // SET_DIGITS
    !% blank character set (tab + space characters)
    tCharParam, PUBLIC  :: SET_BLANKS          = CHR_TAB // CHR_SPACE
    !% white-space character set
    tCharParam, PUBLIC  :: SET_WHITESPACES     = CHR_TAB // CHR_VERTICAL_TAB //  &
                                                 CHR_NEWLINE // CHR_FORM_FEED // &
                                                 CHR_SPACE // CHR_CARRIAGE_RETURN
    !% hexadecimal digit character set
    tCharParam, PUBLIC  :: SET_HEXDIGITS       = 'abcdefABCDEF' // SET_DIGITS
    !% octal digit character set
    tCharParam, PUBLIC  :: SET_OCTDIGITS       = '01234567'
    !% logical character set
    tCharParam, PUBLIC  :: SET_LOGICAL         = 'tfTF'
    !% punctuation character set
    tCharParam, PUBLIC  :: SET_PUNCTUATIONS    = '_,;:.?![](){}@' // CHR_DOUBLEQUOTE // &
                                                 CHR_APOSTROPHE
    !% sign character set
    tCharParam, PUBLIC  :: SET_SIGNS           = '+-'
    !% exponent character set
    tCharParam, PUBLIC  :: SET_EXPONENTS       = 'EeDd'
    !% integer character set (digit + sign)
    tCharParam, PUBLIC  :: SET_INTEGERS        = SET_DIGITS // SET_SIGNS
    !% numeric character set (integer + exponent + period character)
    tCharParam, PUBLIC  :: SET_NUMERICS        = SET_INTEGERS // SET_EXPONENTS // CHR_PERIOD

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE/GENERIC DEFINITIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES OR FUNCTIONS:

!******************************************************************************

FUNCTION Is_Character_In_Range(Chr, ASCII_Min, ASCII_Max) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is in the given ASCII range
    !  from ASCII_Min to ASCII_Max.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tIndex, INTENT(IN)  :: ASCII_Min    !! minimum ASHCII index
    tIndex, INTENT(IN)  :: ASCII_Max    !! maximum ASHCII index
    tLogical            :: Flag         !! true if the character is in the range

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: ChrIndex     ! character index

!** FLOW:

    ChrIndex = IACHAR(Chr)
    Flag = ((ChrIndex >= ASCII_Min).AND.(ChrIndex <= ASCII_Max))

    RETURN

END FUNCTION Is_Character_In_Range

!******************************************************************************

FUNCTION Is_Character_Letter(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is in the 'ALPHABET' set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    !Flag = (INDEX(SET_ALPHABETS, Chr) /= 0)
    Flag = (Is_Character_Lowercase_Letter(Chr).OR.Is_Character_Uppercase_Letter(Chr))

    RETURN

END FUNCTION Is_Character_Letter

!******************************************************************************

FUNCTION Is_Character_Digit(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is in the 'DIGIT' set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    !Flag = (INDEX(SET_DIGITS, Chr) /= 0)
    Flag = ((Chr >= '0').AND.(Chr <= '9'))

    RETURN

END FUNCTION Is_Character_Digit

!******************************************************************************

FUNCTION Is_Character_AlphaNum(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is in the 'ALPHABET' set or 'DIGIT' set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    !Flag = (INDEX(SET_ALPHANUM, Chr) /= 0)
    Flag = (Is_Character_Digit(Chr).OR.Is_Character_Letter(Chr))

    RETURN

END FUNCTION Is_Character_AlphaNum

!******************************************************************************

FUNCTION Is_Character_ASCII(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is a valid ASCII character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = Is_Character_In_Range(Chr, 0_kIndex, 127_kIndex)

    RETURN

END FUNCTION Is_Character_ASCII

!******************************************************************************

FUNCTION Is_Character_Blank(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is in the 'BLANK' set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = (INDEX(SET_BLANKS, Chr) /= 0)

    RETURN

END FUNCTION Is_Character_Blank

!******************************************************************************

FUNCTION Is_Character_Control(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is a control character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = (Is_Character_In_Range(Chr, 0_kIndex, 31_kIndex).OR. &
            Is_Character_In_Range(Chr, 127_kIndex, 159_kIndex))

    RETURN

END FUNCTION Is_Character_Control

!******************************************************************************

FUNCTION Is_Character_Graphical(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is a graphical character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = Is_Character_In_Range(Chr, 33_kIndex, 126_kIndex)

    RETURN

END FUNCTION Is_Character_Graphical

!******************************************************************************

FUNCTION Is_Character_Logical(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is a logical character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = (INDEX(SET_LOGICAL, Chr) /= 0)

    RETURN

END FUNCTION Is_Character_Logical

!******************************************************************************

FUNCTION Is_Character_Printable(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is a printable character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = Is_Character_In_Range(Chr, 32_kIndex, 126_kIndex)

    RETURN

END FUNCTION Is_Character_Printable

!******************************************************************************

FUNCTION Is_Character_Lowercase_Letter(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is a 'lower-case' letter.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    !Flag = (INDEX(SET_ALPHABETS_LOWER, Chr) /= 0)
    Flag = ((Chr >= 'a').AND.(Chr <= 'z'))

    RETURN

END FUNCTION Is_Character_Lowercase_Letter

!******************************************************************************

FUNCTION Is_Character_Uppercase_Letter(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is a 'upper-case' letter.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    !Flag = (INDEX(SET_ALPHABETS_UPPER, Chr) /= 0)
    Flag = ((Chr >= 'A').AND.(Chr <= 'Z'))

    RETURN

END FUNCTION Is_Character_Uppercase_Letter

!******************************************************************************

FUNCTION Is_Character_WhiteSpace(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is in the 'WHITESPACE' set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = (INDEX(SET_WHITESPACES, Chr) /= 0)

    RETURN

END FUNCTION Is_Character_WhiteSpace

!******************************************************************************

FUNCTION Is_Character_Sign(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is a 'sign' character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    !Flag = (INDEX(SET_SIGNS, Chr) /= 0)
    Flag = ((Chr == '-').OR.(Chr == '+'))

    RETURN

END FUNCTION Is_Character_Sign

!******************************************************************************

FUNCTION Is_Character_Period(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is a 'period/dot' character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = Chr == CHR_PERIOD
    RETURN

END FUNCTION Is_Character_Period

!******************************************************************************

FUNCTION Is_Character_Exponent(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is an 'exponent' character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = (INDEX(SET_EXPONENTS, Chr) /= 0)

    RETURN

END FUNCTION Is_Character_Exponent

!******************************************************************************

FUNCTION Is_Character_HexDigit(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is in the 'HEXDIGIT' set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = (INDEX(SET_HEXDIGITS, Chr) /= 0)

    RETURN

END FUNCTION Is_Character_HexDigit

!******************************************************************************

FUNCTION Is_Character_OctDigit(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is in the 'OCTDIGIT' set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = (INDEX(SET_OCTDIGITS, Chr) /= 0)

    RETURN

END FUNCTION Is_Character_OctDigit

!******************************************************************************

FUNCTION Is_Character_Punctuation(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is in the 'PUNCTUATION' set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = (INDEX(SET_PUNCTUATIONS, Chr) /= 0)

    RETURN

END FUNCTION Is_Character_Punctuation

!******************************************************************************

FUNCTION Is_Character_Numeric(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is in the 'NUMERIC' set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = (INDEX(SET_NUMERICS, Chr) /= 0)

    RETURN

END FUNCTION Is_Character_Numeric

!******************************************************************************

FUNCTION Is_Character_Integer(Chr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given character is in the 'INTEGER' set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: Chr          !! character
    tLogical            :: Flag         !! true if the character is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    !Flag = (INDEX(SET_INTEGERS, Chr) /= 0)
    Flag = ((Is_Character_Digit(Chr)).OR.(Is_Character_Sign(Chr)))

    RETURN

END FUNCTION Is_Character_Integer

!******************************************************************************

FUNCTION ToLowerCase(ChrIn) RESULT(ChrOut)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To change case of the input character to lower case if applicable.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: ChrIn
    tChar               :: ChrOut

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32    :: ID

!** FLOW:

    ID = INDEX(SET_ALPHABETS_UPPER, ChrIn)
    IF (ID > 0) THEN
        ChrOut = SET_ALPHABETS_LOWER(ID:ID)
    ELSE
        ChrOut = ChrIn
    END IF

    RETURN

END FUNCTION ToLowerCase

!******************************************************************************

FUNCTION ToUpperCase(ChrIn) RESULT(ChrOut)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To change case of the input character to upper case if applicable.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: ChrIn
    tChar               :: ChrOut

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32    :: ID

!** FLOW:

    ID = INDEX(SET_ALPHABETS_LOWER, ChrIn)
    IF (ID > 0) THEN
        ChrOut = SET_ALPHABETS_UPPER(ID:ID)
    ELSE
        ChrOut = ChrIn
    END IF

    RETURN

END FUNCTION ToUpperCase

!******************************************************************************

END MODULE MBase_CharUtil

!******************************************************************************
