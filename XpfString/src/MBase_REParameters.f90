
MODULE MBase_REParameters

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *REProgram* type and its related helper types and routines.
!   The *REProgram* type is a string type that provides various efficient algorithms for
!   a *substring searching* operation.  The *substring searching* is a fundamental string
!   operation where given a *text* string of length N and a *pattern* string of length M,
!   find an occurrence of the *pattern* within the *text*.  <br>
!  **REFERENCES**: <br>
!   [1] <a href="https:!jakarta.apache.org/regexp/">The Apache Jakarta Project. <br>

!** USE STATEMENTS:
    USE MBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

    PUBLIC

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS (PART1):
    ! optimization parameters
    tSInt32,  PARAMETER :: OPT_HASBACKREFS = 1
    tSInt32,  PARAMETER :: OPT_HASBOL      = 2
    ! Node flags
    tSInt32,  PARAMETER :: NODE_NORMAL   = 0                        ! No flags (nothing special)
    tSInt32,  PARAMETER :: NODE_NULLABLE = 1                        ! True if node is potentially null
    tSInt32,  PARAMETER :: NODE_TOPLEVEL = 2                        ! True if top level expr
    ! Special types of 'escapes'
    tSInt32,  PARAMETER :: ESC_MASK      = ToInt32(Z'000FFFF0')   ! Escape complexity mask
    tSInt32,  PARAMETER :: ESC_BACKREF   = ToInt32(Z'000FFFFF')   ! Escape is really a backreference
    tSInt32,  PARAMETER :: ESC_COMPLEX   = ToInt32(Z'000FFFFE')   ! Escape isn't really a true character
    tSInt32,  PARAMETER :: ESC_CLASS     = ToInt32(Z'000FFFFD')   ! Escape represents a whole class of characters
    ! {m,n} stacks
    tSInt32,  PARAMETER :: bracketUnbounded = -1                    ! Unbounded value
    ! Specifies normal, case-sensitive matching behaviour.
    tSInt32,  PARAMETER :: MATCH_NORMAL          = ToInt32(Z'00000000')
    ! Flag to indicate that matching should be case-independent (folded)
    tSInt32,  PARAMETER :: MATCH_CASEINDEPENDENT = ToInt32(Z'00000001')
    ! Newlines should match as BOL/EOL (^ and $)
    tSInt32,  PARAMETER :: MATCH_MULTILINE       = ToInt32(Z'00000002')
    ! Consider all input a single body of text - newlines are matched by .
    tSInt32,  PARAMETER :: MATCH_SINGLELINE      = ToInt32(Z'00000004')
    !***********************************************
    !                                              *
    ! The format of a node in a program is:        *
    !                                              *
    ! [ OPCODE ] [ OPDATA ] [ OPNEXT ] [ OPERAND ] *
    !                                              *
    ! char OPCODE - instruction                    *
    ! char OPDATA - modifying data                 *
    ! char OPNEXT - next node (relative offset)    *
    !                                              *
    !***********************************************
                 !   Opcode              Char       Opdata/Operand  Meaning
                 !   ----------          ---------- --------------- --------------------------------------------------
    tChar,    PARAMETER :: OP_END              = 'E'  !                 end of program
    tChar,    PARAMETER :: OP_BOL              = '^'  !                 match only if at beginning of line
    tChar,    PARAMETER :: OP_EOL              = '$'  !                 match only if at end of line
    tChar,    PARAMETER :: OP_ANY              = '.'  !                 match any single character except newline
    tChar,    PARAMETER :: OP_ANYOF            = '['  ! count/ranges    match any char in the list of ranges
    tChar,    PARAMETER :: OP_BRANCH           = '|'  ! node            match this alternative or the next one
    tChar,    PARAMETER :: OP_ATOM             = 'A'  ! length/string   length of string followed by string itself
    tChar,    PARAMETER :: OP_STAR             = '*'  ! node            kleene closure
    tChar,    PARAMETER :: OP_PLUS             = '+'  ! node            positive closure
    tChar,    PARAMETER :: OP_MAYBE            = '?'  ! node            optional closure
    tChar,    PARAMETER :: OP_ESCAPE           = '\'  ! escape          special escape code char class (escape is E_* code)
    tChar,    PARAMETER :: OP_OPEN             = '('  ! number          nth opening paren
    tChar,    PARAMETER :: OP_OPEN_CLUSTER     = '<'  !                 opening cluster
    tChar,    PARAMETER :: OP_CLOSE            = ')'  ! number          nth closing paren
    tChar,    PARAMETER :: OP_CLOSE_CLUSTER    = '>'  !                 closing cluster
    tChar,    PARAMETER :: OP_BACKREF          = '#'  ! number          reference nth already matched parenthesized string
    tChar,    PARAMETER :: OP_GOTO             = 'G'  !                 nothing but a (back-)pointer
    tChar,    PARAMETER :: OP_NOTHING          = 'N'  !                 match null string such as in '(a|)'
    tChar,    PARAMETER :: OP_CONTINUE         = 'C'  !                 continue to the following command (ignore next)
    tChar,    PARAMETER :: OP_RELUCTANTSTAR    = '8'  ! none/expr       reluctant '*' (mnemonic for char is unshifted '*')
    tChar,    PARAMETER :: OP_RELUCTANTPLUS    = '='  ! none/expr       reluctant '+' (mnemonic for char is unshifted '+')
    tChar,    PARAMETER :: OP_RELUCTANTMAYBE   = '/'  ! none/expr       reluctant '?' (mnemonic for char is unshifted '?')
    tChar,    PARAMETER :: OP_POSIXCLASS       = 'P'  ! classid         one of the posix character classes
    ! Escape codes
    tChar,    PARAMETER :: E_ALNUM             = 'w'  ! Alphanumeric
    tChar,    PARAMETER :: E_NALNUM            = 'W'  ! Non-alphanumeric
    tChar,    PARAMETER :: E_BOUND             = 'b'  ! Word boundary
    tChar,    PARAMETER :: E_NBOUND            = 'B'  ! Non-word boundary
    tChar,    PARAMETER :: E_SPACE             = 's'  ! Whitespace
    tChar,    PARAMETER :: E_NSPACE            = 'S'  ! Non-whitespace
    tChar,    PARAMETER :: E_DIGIT             = 'd'  ! Digit
    tChar,    PARAMETER :: E_NDIGIT            = 'D'  ! Non-digit
    ! Posix character classes
    tChar,    PARAMETER :: POSIX_CLASS_ALNUM   = 'w'  ! Alphanumerics
    tChar,    PARAMETER :: POSIX_CLASS_ALPHA   = 'a'  ! Alphabetics
    tChar,    PARAMETER :: POSIX_CLASS_BLANK   = 'b'  ! Blanks
    tChar,    PARAMETER :: POSIX_CLASS_CNTRL   = 'c'  ! Control characters
    tChar,    PARAMETER :: POSIX_CLASS_DIGIT   = 'd'  ! Digits
    tChar,    PARAMETER :: POSIX_CLASS_GRAPH   = 'g'  ! Graphic characters
    tChar,    PARAMETER :: POSIX_CLASS_LOWER   = 'l'  ! Lowercase characters
    tChar,    PARAMETER :: POSIX_CLASS_PRINT   = 'p'  ! Printable characters
    tChar,    PARAMETER :: POSIX_CLASS_PUNCT   = '!'  ! Punctuation
    tChar,    PARAMETER :: POSIX_CLASS_SPACE   = 's'  ! Spaces
    tChar,    PARAMETER :: POSIX_CLASS_UPPER   = 'u'  ! Uppercase characters
    tChar,    PARAMETER :: POSIX_CLASS_HEXDG   = 'x'  ! Hexadecimal digits
    ! Limits
    tSInt32,  PARAMETER :: maxNode  = 65536            ! Maximum number of nodes in a program
    tSInt32,  PARAMETER :: MAX_PAREN = 16              ! Number of paren pairs (only 9 can be backrefs)
    ! Node layout constants
    tSInt32,  PARAMETER :: offsetOpcode = 0            ! Opcode offset (first character)
    tSInt32,  PARAMETER :: offsetOpdata = 1            ! Opdata offset (second char)
    tSInt32,  PARAMETER :: offsetNext   = 2            ! Next index offset (third char)
    tSInt32,  PARAMETER :: nodeSize     = 3            ! Node size (in chars)

!** DERIVED TYPE DEFINITIONS
    ! derive type for POSIX character class names
    TYPE, PRIVATE   :: PosixPair
        tCharLen(5) :: Name     ! class name
        tChar       :: C        ! character representing the class
    END TYPE

!** MODULE PARAMETERS (PART2):
    ! Lookup table for POSIX character class names
    TYPE(PosixPair), PARAMETER  :: hashPOSIX(*) =   &
        [PosixPair("alnum", POSIX_CLASS_ALNUM),     &
         PosixPair("alpha", POSIX_CLASS_ALPHA),     &
         PosixPair("blank", POSIX_CLASS_BLANK),     &
         PosixPair("cntrl", POSIX_CLASS_CNTRL),     &
         PosixPair("digit", POSIX_CLASS_DIGIT),     &
         PosixPair("graph", POSIX_CLASS_GRAPH),     &
         PosixPair("lower", POSIX_CLASS_LOWER),     &
         PosixPair("print", POSIX_CLASS_PRINT),     &
         PosixPair("punct", POSIX_CLASS_PUNCT),     &
         PosixPair("space", POSIX_CLASS_SPACE),     &
         PosixPair("upper", POSIX_CLASS_UPPER),     &
         PosixPair("hexdg", POSIX_CLASS_HEXDG)]

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES OR FUNCTIONS:

!******************************************************************************

FUNCTION hashPOSIX_GetChar(Name) RESULT(C)

!** PURPOSE OF THIS SUBROUTINE:
    !> To check whether the given character is in the given ASCII range
    !  from ASCII_Min to ASCII_Max.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharLen(5), INTENT(IN) :: Name     ! class name
    tChar                   :: C        ! character representing the class

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

!** FLOW:

    DO I = 1, SIZE(hashPOSIX)
        IF (Name == hashPOSIX(I)%Name) THEN
            C = hashPOSIX(I)%C
            RETURN
        END IF
    END DO
    C = OP_STAR

    RETURN

END FUNCTION hashPOSIX_GetChar

!******************************************************************************

END MODULE MBase_REParameters

!******************************************************************************
