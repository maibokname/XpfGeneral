
MODULE MClass_RECompiler

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *RECompiler* type and its related routines.  <br>
!   <br>
!  **REFERENCES**: <br>
!   [1] <a href="https:!jakarta.apache.org/regexp/">The Apache Jakarta Project. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_MemHandlers
    USE MBase_CharUtil
    USE MBase_ChrStr,         ONLY: CharString
    USE MBase_SIntUtil,       ONLY: ParseInteger => I32_FromChar, ToString => ToDecStrSigned
    USE MClass_StringBuilder
    USE MBase_REParameters
    USE MClass_REProgram

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: RECompiler

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define     CharCode(C)     ICHAR(C)
#define     ToChar(Code)    CHAR(Code, KIND=kChar)

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName    = 'Class_RECompiler'
    tSInt32,   PARAMETER    :: CharMinVal = 0
!    tSInt32,   PARAMETER    :: CharMaxVal = 127                     ! ASCII
    tSInt32,   PARAMETER    :: CharMaxVal = 255                     ! extended ASCII
!    tSInt32,   PARAMETER    :: CharMaxVal = ToInt32(Z'0000FFFF')  ! UTF-16
!    tSInt32,   PARAMETER    :: CharMaxVal = ToInt32(Z'0010FFFF')  ! UTF-32

!** DERIVED TYPE DEFINITIONS
    !> The *RECompiler* type is a ...
    TYPE RECompiler
        PRIVATE
        ! ----- The compiled program -----
        tChar, ALLOCATABLE  :: instruction(:)   !! The compiled regular expression 'program'
        tSInt32             :: lenInstruction   !! The amount of the instruction buffer currently in use
        ! ----- Input state for compiling regular expression -----
        tChar, ALLOCATABLE  :: pattern(:)       !! Input string
        tSInt32             :: len              !! Length of the pattern string
        tSInt32             :: idx              !! Current input index into ac
        tSInt32             :: parens           !! Total number of parentheses pairs
        ! ----- {m,n} stacks -----
        tSInt32             :: bracketMin       !! Minimum number of matches
        tSInt32             :: bracketOpt       !! Additional optional matches
    CONTAINS
        ! -----                 Private Procedures                      -----
        PROCEDURE, PRIVATE  :: Initialize       => RECompiler_Initialize
        PROCEDURE, PRIVATE  :: Ensure           => RECompiler_Ensure
        PROCEDURE, PRIVATE  :: Emit             => RECompiler_Emit
        PROCEDURE, PRIVATE  :: NodeInsert       => RECompiler_NodeInsert
        PROCEDURE, PRIVATE  :: SetNextOfEnd     => RECompiler_SetNextOfEnd
        PROCEDURE, PRIVATE  :: Node             => RECompiler_Node
        PROCEDURE, PRIVATE  :: Bracket          => RECompiler_Bracket
        PROCEDURE, PRIVATE  :: Escape           => RECompiler_Escape
        PROCEDURE, PRIVATE  :: CharacterClass   => RECompiler_CharacterClass
        PROCEDURE, PRIVATE  :: Atom             => RECompiler_Atom
        PROCEDURE, PRIVATE  :: Terminal         => RECompiler_Terminal
        PROCEDURE, PRIVATE  :: Closure          => RECompiler_Closure
        PROCEDURE, PRIVATE  :: Branch           => RECompiler_Branch
        PROCEDURE, PRIVATE  :: Expression       => RECompiler_Expression
        ! -----                 Public Procedures                       -----
        PROCEDURE           :: Compile          => RECompiler_Compile
    END TYPE RECompiler
    ! Private derived type for maintaining character ranges for character classes.
    TYPE RERange
        tIndex                  :: Size = 16_kIndex !! Capacity of current range arrays
        tSInt32,  ALLOCATABLE   :: MinRange(:)      !! Range minima
        tSInt32,  ALLOCATABLE   :: MaxRange(:)      !! Range maxima
        tSInt32                 :: Num = 0          !! Number of range array elements in use
    CONTAINS
        PROCEDURE   :: RERange_Include1
        PROCEDURE   :: RERange_Include2
        PROCEDURE   :: Initialize   => RERange_Initialize
        PROCEDURE   :: Delete       => RERange_Delete
        PROCEDURE   :: Merge        => RERange_Merge
        PROCEDURE   :: Remove       => RERange_Remove
        GENERIC     :: Include      => RERange_Include1, RERange_Include2
    END TYPE

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!-------------------------------------------------------------------------------
!                           RECompiler Procedures
!-------------------------------------------------------------------------------

SUBROUTINE RECompiler_Initialize(Compiler)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the RECompiler object.  Creates (initially empty) storage
    !  for a regular expression program.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(OUT)  :: Compiler !! RECompiler object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! Start off with a generous, yet reasonable, initial size
    IF (ALLOCATED(Compiler%instruction)) DEALLOCATE(Compiler%instruction)
    ALLOCATE(tChar :: Compiler%instruction(0:127))
    Compiler%lenInstruction = 0

    RETURN

END SUBROUTINE RECompiler_Initialize

!******************************************************************************

SUBROUTINE RECompiler_Ensure(Compiler, n)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To ensure that n more characters can fit in the program buffer.
    !  If n more can't fit, then the size is doubled until it can.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler !! RECompiler object
    tSInt32,           INTENT(IN)       :: n        !! Number of additional characters to ensure will fit

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: curlen

! FLOW

    ! Get current program length
    curlen = SIZE(Compiler%instruction)

    ! If the current length + n more is too much
    IF ((Compiler%lenInstruction + n) >= curlen) THEN
        ! Double the size of the program array until n more will fit
        DO WHILE ((Compiler%lenInstruction + n) >= curlen)
            curlen = curlen*2
        END DO
        ! Allocate new program array and move data into it
        BLOCK
            tChar, ALLOCATABLE  :: newInstruction(:)
            ALLOCATE(tChar :: newInstruction(0:curlen-1))
            newInstruction(0:Compiler%lenInstruction-1) = Compiler%instruction(0:)
            CALL MOVE_ALLOC(newInstruction, Compiler%instruction)
        END BLOCK
    END IF

    RETURN

END SUBROUTINE RECompiler_Ensure

!******************************************************************************

SUBROUTINE RECompiler_Emit(Compiler, C)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To emit a single character into the program stream.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler !! RECompiler object
    tChar,             INTENT(IN)       :: C        !! Character to add

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! Make room for character
    CALL Compiler%Ensure(1)

    ! Add character
    Compiler%instruction(Compiler%lenInstruction) = C
    Compiler%lenInstruction = Compiler%lenInstruction + 1

    RETURN

END SUBROUTINE RECompiler_Emit

!******************************************************************************

SUBROUTINE RECompiler_NodeInsert(Compiler, opcode, opdata, insertAt)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert a node with a given opcode and opdata at insertAt.  The node relative
    !  next pointer is initialized to 0.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler !! RECompiler object
    tChar,             INTENT(IN)       :: opcode   !! Opcode for new node
    tSInt32,           INTENT(IN)       :: opdata   !! Opdata for new node (only the low 16 bits are currently used)
    tSInt32,           INTENT(IN)       :: insertAt !! Index at which to insert the new node in the program

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Index, ALen, DstBase

! FLOW

    ! Make room for a new node
    CALL Compiler%Ensure(nodeSize)

    ! Move everything from insertAt to the end down nodeSize elements
    ALen = Compiler%lenInstruction - insertAt
    DstBase = insertAt + nodeSize
    DO Index = ALen-1, 0, -1
        Compiler%instruction(DstBase+Index) = Compiler%instruction(insertAt+Index)
    END DO
    Compiler%instruction(insertAt) = opcode
    Compiler%instruction(insertAt+offsetOpdata) = ToChar(opdata)
    Compiler%instruction(insertAt+offsetNext) = ToChar(0)
    Compiler%lenInstruction = Compiler%lenInstruction + nodeSize

    RETURN

END SUBROUTINE RECompiler_NodeInsert

!******************************************************************************

SUBROUTINE RECompiler_SetNextOfEnd(Compiler, nodeIn, pointToIn, ErrFlag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To append a node to the end of a node chain.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler     !! RECompiler object
    tSInt32,           INTENT(IN)       :: nodeIn       !! Start of node chain to traverse
    tSInt32,           INTENT(IN)       :: pointToIn    !! Node to have the tail of the chain point to
    tLogical,          INTENT(OUT)      :: ErrFlag      !! true if error occurred

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: next, offset, node, pointTo

! FLOW

    ErrFlag = FalseVal
    node    = nodeIn
    pointTo = pointToIn
    ASSOCIATE (instruction => Compiler%instruction, lenInstruction => Compiler%lenInstruction)
        ! Traverse the chain until the next offset is 0
        next = CharCode(instruction(node + offsetNext))
        ! while the 'node' is not the last in the chain
        ! and the 'node' is not the last in the program.
        DO WHILE ((next /= 0).AND.(node < lenInstruction))
            ! if the node we are supposed to point to is in the chain then
            ! point to the end of the program instead.
            ! Michael McCallum <gholam@xtra.co.nz>
            ! FIXME: This is a _hack_ to stop infinite programs.
            ! I believe that the implementation of the reluctant matches is wrong but
            ! have not worked out a better way yet.
            IF (node == pointTo) pointTo = lenInstruction
            node = node + next
            next = CharCode(instruction(node + offsetNext))
        END DO

        ! if we have reached the end of the program then dont set the pointTo.
        ! im not sure if this will break any thing but passes all the tests.
        IF (node < lenInstruction) THEN
            ! Some patterns result in very large programs which exceed
            ! capacity of the short used for specifying signed offset of the
            ! next instruction. Example: a{1638}
            offset = pointTo - node
            IF (offset /= ToInt32(ToInt16(offset))) THEN
                CALL Handle_ErrLevel('RECompiler_SetNextOfEnd', ModName, ErrSevere, &
                                     'Exceeded short jump range.')
                ErrFlag = TrueVal
                RETURN
            END IF

            ! Point the last node in the chain to pointTo.
            instruction(node + offsetNext) = ToChar(offset)
        END IF
    END ASSOCIATE

    RETURN

END SUBROUTINE RECompiler_SetNextOfEnd

!******************************************************************************

FUNCTION RECompiler_Node(Compiler, opcode, opdata) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler !! RECompiler object
    tChar,             INTENT(IN)       :: opcode   !! Opcode for new node
    tSInt32,           INTENT(IN)       :: opdata   !! Opdata for new node (only the low 16 bits are currently used)
    tSInt32                             :: Index    !! Index of new node in program

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! Make room for a new node
    CALL Compiler%Ensure(nodeSize)

    ASSOCIATE (instruction => Compiler%instruction, lenInstruction => Compiler%lenInstruction)
        ! Add new node at end
        instruction(lenInstruction) = opcode
        instruction(lenInstruction+offsetOpdata) = ToChar(opdata)
        instruction(lenInstruction+offsetNext) = ToChar(0)
        lenInstruction = lenInstruction + nodeSize
        ! Return index of new node
        Index = lenInstruction - nodeSize
    END ASSOCIATE

    RETURN

END FUNCTION RECompiler_Node

!******************************************************************************

SUBROUTINE RECompiler_Bracket(Compiler, ErrFlag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To match bracket {m,n} expression and put results in bracket member variables.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler !! RECompiler object
    tLogical,          INTENT(OUT)      :: ErrFlag  !! true if error occurred

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(StringBuilder) :: Number

! FLOW

    ErrFlag = FalseVal
    ASSOCIATE (pattern => Compiler%pattern, len => Compiler%len, idx => Compiler%idx, &
               bracketMin => Compiler%bracketMin, bracketOpt => Compiler%bracketOpt)
        ! Current character must be a '{'
        IF ((idx >= len).OR.(pattern(idx) /= CHR_BRACE_LEFT)) THEN
            idx = idx + 1
            CALL Handle_ErrLevel('RECompiler_Bracket', ModName, ErrSevere, 'Internal error!')
            ErrFlag = TrueVal
            RETURN
        ELSE
            idx = idx + 1
        END IF

        ! Next char must be a digit
        IF ((idx >= len).OR.(.NOT.Is_Character_Digit(pattern(idx)))) THEN
            CALL Handle_ErrLevel('RECompiler_Bracket', ModName, ErrSevere, 'Syntax error: Expected digit')
            ErrFlag = TrueVal
            RETURN
        END IF

        ! Get min ('m' of {m,n}) number
        CALL Number%CreateEmpty(InitCap=256_kIndex)
        DO WHILE ((idx < len).AND.(Is_Character_Digit(pattern(idx))))
            CALL Number%append(pattern(idx))
            idx = idx + 1
        END DO
        bracketMin = ParseInteger(Number%ToCharAlloc(ClearBuffer=.TRUE.), ErrFlag)
        IF (ErrFlag) THEN
            CALL Handle_ErrLevel('RECompiler_Bracket', ModName, ErrSevere, &
                                 'Syntax error: Expected valid number')
            ErrFlag = TrueVal
            RETURN
        END IF

        ! If out of input, fail
        IF (idx >= len) THEN
            CALL Handle_ErrLevel('RECompiler_Bracket', ModName, ErrSevere, &
                                 'Syntax error: Expected comma or right bracket')
            ErrFlag = TrueVal
            RETURN
        END IF

        ! If end of expr, optional limit is 0
        IF (pattern(idx) == CHR_BRACE_RIGHT) THEN
            idx = idx + 1
            bracketOpt = 0
            RETURN
        END IF

        ! Must have at least {m,} and maybe {m,n}.
        IF ((idx >= len).OR.(pattern(idx) /= ',')) THEN
            idx = idx + 1
            CALL Handle_ErrLevel('RECompiler_Bracket', ModName, ErrSevere, 'Syntax error: Expected comma')
            ErrFlag = TrueVal
            RETURN
        ELSE
            idx = idx + 1
        END IF

        ! If out of input, fail
        IF (idx >= len) THEN
            CALL Handle_ErrLevel('RECompiler_Bracket', ModName, ErrSevere, &
                                 'Syntax error: Expected comma or right bracket')
            ErrFlag = TrueVal
            RETURN
        END IF

        ! If {m,} max is unlimited
        IF (pattern(idx) == CHR_BRACE_RIGHT) THEN
            idx = idx + 1
            bracketOpt = bracketUnbounded
            RETURN
        END IF

        ! Next char must be a digit
        IF ((idx >= len).OR.(.NOT.Is_Character_Digit(pattern(idx)))) THEN
            CALL Handle_ErrLevel('RECompiler_Bracket', ModName, ErrSevere, 'Syntax error: Expected digit')
            ErrFlag = TrueVal
            RETURN
        END IF

        ! Get max number
        DO WHILE ((idx < len).AND.Is_Character_Digit(pattern(idx)))
            CALL Number%Append(pattern(idx))
            idx = idx + 1
        END DO
        bracketOpt = ParseInteger(Number%ToCharAlloc(ClearBuffer=.TRUE.), ErrFlag) - bracketMin
        IF (ErrFlag) THEN
            CALL Handle_ErrLevel('RECompiler_Bracket', ModName, ErrSevere, &
                                 'Syntax error: Expected valid number')
            ErrFlag = TrueVal
            RETURN
        END IF

        ! Optional repetitions must be >= 0
        IF (bracketOpt < 0) THEN
            CALL Handle_ErrLevel('RECompiler_Bracket', ModName, ErrSevere, &
                                 'Syntax error: Bad range')
            ErrFlag = TrueVal
            RETURN
        END IF

        ! Must have close brace
        IF ((idx >= len).OR.(pattern(idx) /= CHR_BRACE_RIGHT)) THEN
            CALL Handle_ErrLevel('RECompiler_Bracket', ModName, ErrSevere, &
                                 'Syntax error: Missing close brace')
            ErrFlag = TrueVal
        END IF
        idx = idx + 1
    END ASSOCIATE

    RETURN

END SUBROUTINE RECompiler_Bracket

!******************************************************************************

FUNCTION RECompiler_Escape(Compiler, ErrFlag) RESULT(EspCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To match an escape sequence.  Handles quoted chars and octal escapes as well
    !  as normal escape characters.  Always advances the input stream by the
    !  right amount.  This code "understands" the subtle difference between an
    !  octal escape and a backref.  You can access the type of ESC_CLASS or
    !  ESC_COMPLEX or ESC_BACKREF by looking at pattern(idx-1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler !! RECompiler object
    tLogical,          INTENT(OUT)      :: ErrFlag  !! true if error occurred
    tSInt32                             :: EspCode  !! escape code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tChar       :: escapeChar, c
    tSInt32     :: hexDigits, val
    tCharAlloc  :: errMsg

! FLOW

    ErrFlag = FalseVal
    ASSOCIATE (pattern => Compiler%pattern, len => Compiler%len, idx => Compiler%idx)
        ! "Shouldn't" happen
        IF (pattern(idx) /= CHR_BACKSLASH) THEN
            CALL Handle_ErrLevel('RECompiler_Escape', ModName, ErrSevere, 'Internal error!')
            ErrFlag = TrueVal
            RETURN
        END IF
        ! Escape shouldn't occur as last character in string!
        IF (idx + 1 == len) THEN
            CALL Handle_ErrLevel('RECompiler_Escape', ModName, ErrSevere, &
                                 'Syntax error: Escape terminates string')
            ErrFlag = TrueVal
            RETURN
        END IF
        ! Switch on character after backslash
        idx = idx + 2
        escapeChar = pattern(idx-1)
        SELECT CASE (escapeChar)
        CASE (E_BOUND, E_NBOUND)
            EspCode = ESC_COMPLEX
        CASE (E_ALNUM, E_NALNUM, E_SPACE, E_NSPACE, E_DIGIT, E_NDIGIT)
            EspCode = ESC_CLASS
        CASE ('x')  ! currently, not handle unicode
        ! CASE ('u', 'x')
            ! Exact required hex digits for escape type
            ! IF (escapeChar == 'u') THEN
            !    hexDigits = 4
            ! ELSE
            !    hexDigits = 2
            ! END IF
            ! note: the above code block handles unicode
            hexDigits = 2
            ! Parse up to hexDigits characters from input
            val = 0
            DO WHILE ((idx < len).AND.(hexDigits > 0))
                hexDigits = hexDigits - 1
                ! Get char
                c = pattern(idx)
                ! If it's a hexadecimal digit (0-9)
                IF ((c >= '0').AND.(c <= '9')) THEN
                    ! Compute new value
                    val = SHIFTL(val, 4) + CharCode(c) - CharCode('0')
                ELSE
                    ! If it's a hexadecimal letter (a-f)
                    c = ToLowerCase(c)
                    IF ((c >= 'a').AND.(c <= 'f')) THEN
                        ! Compute new value
                        val = SHIFTL(val, 4) + (CharCode(c) - CharCode('a')) + 10
                    ELSE
                        ! If it's not a valid digit or hex letter, the escape must be invalid
                        ! because hexDigits of input have not been absorbed yet.
                        errMsg = 'Expected ' // ToString(hexDigits) // &
                                 ' hexadecimal digits after \' // escapeChar
                        CALL Handle_ErrLevel('RECompiler_Escape', ModName, ErrSevere, errMsg)
                        ErrFlag = TrueVal
                        RETURN
                    END IF
                END IF
                idx = idx + 1
            END DO
            EspCode = val
        CASE ('t')
            EspCode = CharCode(CHR_TAB)
        CASE ('n')
            EspCode = CharCode(CHR_NEWLINE)
        CASE ('r')
            EspCode = CharCode(CHR_CARRIAGE_RETURN)
        CASE ('f')
            EspCode = CharCode(CHR_FORM_FEED)
        CASE ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
            ! An octal escape starts with a 0 or has two digits in a row
            IF (((idx < len).AND.Is_Character_Digit(pattern(idx))).OR.(escapeChar == '0')) THEN
                ! Handle \nnn octal escapes
                val = CharCode(escapeChar) - CharCode('0')
                IF ((idx < len).AND.Is_Character_Digit(pattern(idx))) THEN
                    val = SHIFTL(val, 3) + (CharCode(pattern(idx)) - CharCode('0'))
                    idx = idx + 1
                    IF ((idx < len).AND.Is_Character_Digit(pattern(idx))) THEN
                        val = SHIFTL(val, 3) + (CharCode(pattern(idx)) - CharCode('0'))
                        idx = idx + 1
                    END IF
                END IF
                EspCode = val
            ELSE
                ! It's actually a backreference (\[1-9]), not an escape
                EspCode = ESC_BACKREF
            END IF
        CASE DEFAULT
            ! Simple quoting of a character
            EspCode = CharCode(escapeChar)
        END SELECT
    END ASSOCIATE

    RETURN

END FUNCTION RECompiler_Escape

!******************************************************************************

FUNCTION RECompiler_CharacterClass(Compiler, ErrFlag) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compile a character class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler !! RECompiler object
    tLogical,          INTENT(OUT)      :: ErrFlag  !! true if error occurred
    tSInt32                             :: Indx     !! Index of class node

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar, PARAMETER    :: CHAR_INVALID = ToChar(CharMaxVal)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: errMsg
    tCharAlloc  :: charClass
    tChar       :: c

! FLOW

    ErrFlag = FalseVal
    ASSOCIATE (instruction => Compiler%instruction, pattern => Compiler%pattern, &
               len => Compiler%len, idx => Compiler%idx)
        ! Check for bad calling or empty class
        IF (pattern(idx) /= CHR_BRACKET_LEFT) THEN
            CALL Handle_ErrLevel('RECompiler_CharacterClass', ModName, ErrSevere, 'Internal error!')
            ErrFlag = TrueVal
            RETURN
        END IF
        ! Check for unterminated or empty class
        idx = idx + 1
        IF ((idx >= len).OR.(pattern(idx) == CHR_BRACKET_RIGHT)) THEN
            CALL Handle_ErrLevel('RECompiler_CharacterClass', ModName, ErrSevere, &
                                 'Syntax error: Empty or unterminated class')
            ErrFlag = TrueVal
            RETURN
        END IF
        ! Check for POSIX character class
        IF ((idx < len).AND.(pattern(idx) == CHR_COLON)) THEN
            BLOCK
                tSInt32     :: idxStart
                ! Skip colon
                idx = idx + 1
                ! POSIX character classes are denoted with lowercase ASCII strings
                idxStart = idx
                DO WHILE ((idx < len).AND.(pattern(idx) >= 'a').AND.(pattern(idx) <= 'z'))
                    idx = idx + 1
                END DO
                ! Should be a ":]" to terminate the POSIX character class
                IF (((idx + 1) < len).AND.(pattern(idx) == CHR_COLON).AND. &
                    (pattern(idx+1) == CHR_BRACKET_RIGHT)) THEN
                    ! Get character class
                    charClass = CharString(pattern(idxStart:idx-1), IsCString=FalseVal)
                    ! Select the POSIX class id
                    C = hashPOSIX_GetChar(charClass)
                    IF (C /= OP_STAR) THEN
                        ! Move past colon and right bracket
                        idx = idx + 2
                        ! Return new POSIX character class node
                        Indx = Compiler%Node(OP_POSIXCLASS, CharCode(C))
                    ELSE
                        errMsg = 'Syntax error: Invalid POSIX character class "' // charClass // '"'
                        CALL Handle_ErrLevel('RECompiler_CharacterClass', ModName, ErrSevere, errMsg)
                        ErrFlag = TrueVal
                    END IF
                    RETURN
                END IF
                CALL Handle_ErrLevel('RECompiler_CharacterClass', ModName, ErrSevere, &
                                     'Syntax error: Invalid POSIX character class syntax')
                ErrFlag = TrueVal
                RETURN
            END BLOCK
        END IF

        ! Try to build a class.  Create OP_ANYOF node
        Indx = Compiler%Node(OP_ANYOF, 0)

        ! Parse class declaration
        BLOCK
            tChar           :: last, simpleChar
            tLogical        :: inc, definingRange
            tSInt32         :: idxFirst, espcode, I
            tSInt32         :: rangeStart, rangeEnd
            TYPE(RERange)   :: range
            ! initialize
            inc = TrueVal
            definingRange = FalseVal
            idxFirst = idx
            rangeStart = CharMinVal
            CALL range%Initialize()
            WhileLoop: DO WHILE ((idx < len).AND.(pattern(idx) /= CHR_BRACKET_RIGHT))
                ! Switch on character
                switchOnCharacter: SELECT CASE (pattern(idx))
                CASE (CHR_CARET)
                    inc = .NOT.inc
                    IF (idx == idxFirst) THEN
                        CALL range%Include(CharMinVal, CharMaxVal, TrueVal)
                    END IF
                    idx = idx + 1
                    CYCLE WhileLoop
                CASE (CHR_BACKSLASH)
                    ! Escape always advances the stream
                    espcode = Compiler%Escape(ErrFlag)
                    IF (ErrFlag) RETURN
                    escapeBlock: SELECT CASE (espcode)
                    CASE (ESC_COMPLEX, ESC_BACKREF)
                        ! Word boundaries and backrefs not allowed in a character class!
                        CALL Handle_ErrLevel('RECompiler_CharacterClass', ModName, ErrSevere, &
                                                'Syntax error: Bad character class')
                        ErrFlag = TrueVal
                        RETURN
                    CASE (ESC_CLASS)
                        ! Classes can't be an endpoint of a range
                        IF (definingRange) THEN
                            CALL Handle_ErrLevel('RECompiler_CharacterClass', ModName, ErrSevere, &
                                                    'Syntax error: Bad character class')
                            ErrFlag = TrueVal
                            RETURN
                        END IF
                        ! Handle specific type of class (some are ok)
                        escapeCharacter: SELECT CASE (pattern(idx-1))
                        CASE (E_NSPACE)
                            CALL range%Include(CharMinVal, 7, inc)  ! [Min - \b )
                            CALL range%Include(ToChar(11), inc)     ! ( \n - \f )
                            CALL range%Include(14, 31, inc)         ! ( \r - ' ')
                            CALL range%Include(33, CharMaxVal, inc) ! (' ' - Max]
                            EXIT escapeCharacter
                        CASE (E_NALNUM)
                            CALL range%Include(CharMinVal, CharCode(CHR_SLASH), inc)                    ! [Min - '0')
                            CALL range%Include(CharCode(CHR_COLON), CharCode(CHR_AT_SIGN), inc)         ! ('9' - 'A')
                            CALL range%Include(CharCode(CHR_BRACKET_LEFT), CharCode(CHR_CARET), inc)    ! ('Z' - '_')
                            CALL range%Include(CHR_GRAVE_ACCENT, inc)                         ! ('_' - 'a')
                            CALL range%Include(CharCode(CHR_BRACE_LEFT), CharMaxVal, inc)               ! ('z' - Max]
                            EXIT escapeCharacter
                        CASE (E_NDIGIT)
                            CALL range%Include(CharMinVal, CharCode(CHR_SLASH), inc)    ! [Min - '0')
                            CALL range%Include(CharCode(CHR_COLON), CharMaxVal, inc)    ! ('9' - Max]
                            EXIT escapeCharacter
                        CASE (E_SPACE)
                            CALL range%Include(CHR_TAB, inc)
                            CALL range%Include(CHR_CARRIAGE_RETURN, inc)
                            CALL range%Include(CHR_FORM_FEED, inc)
                            CALL range%Include(CHR_NEWLINE, inc)
                            CALL range%Include(CHR_BACKSPACE, inc)
                            CALL range%Include(CHR_SPACE, inc)
                            EXIT escapeCharacter
                        CASE (E_ALNUM, E_DIGIT)
                            IF (pattern(idx-1) == E_ALNUM) THEN
                                CALL range%Include(CharCode('a'), CharCode('z'), inc)
                                CALL range%Include(CharCode('A'), CharCode('Z'), inc)
                                CALL range%Include(CHR_UNDERSCORE, inc)
                                ! Fall through!
                            END IF
                            CALL range%Include(CharCode('0'), CharCode('9'), inc)
                            EXIT escapeCharacter
                        END SELECT escapeCharacter
                        ! Make last char invalid (can't be a range start)
                        last = CHAR_INVALID
                        EXIT escapeBlock
                    CASE DEFAULT
                        ! Escape is simple so treat as a simple char
                        simpleChar = ToChar(espcode)
                        EXIT switchOnCharacter
                    END SELECT escapeBlock
                    CYCLE WhileLoop
                CASE ('-')
                    ! Start a range if one isn't already started
                    IF (definingRange) THEN
                        CALL Handle_ErrLevel('RECompiler_CharacterClass', ModName, ErrSevere, &
                                             'Syntax error: Bad class range')
                        ErrFlag = TrueVal
                        RETURN
                    END IF
                    definingRange = TrueVal
                    ! If no last character, start of range is 0
                    IF (last == CHAR_INVALID) THEN
                        rangeStart = 0
                    ELSE
                        rangeStart = CharCode(last)
                    END IF
                    ! Premature end of range% define up to CharMaxVal
                    idx = idx + 1
                    IF ((idx < len).AND.(pattern(idx) == CHR_BRACKET_RIGHT)) THEN
                        simpleChar = ToChar(CharMaxVal)
                        EXIT switchOnCharacter
                    END IF
                    CYCLE WhileLoop
                CASE DEFAULT
                    simpleChar = pattern(idx)
                    idx = idx + 1
                    EXIT switchOnCharacter
                END SELECT switchOnCharacter

                ! Handle simple character simpleChar
                IF (definingRange) THEN
                    ! if we are defining a range make it now
                    rangeEnd = CharCode(simpleChar)
                    ! Actually create a range if the range is ok
                    IF (rangeStart >= rangeEnd) THEN
                        CALL Handle_ErrLevel('RECompiler_CharacterClass', ModName, ErrSevere, &
                                             'Bad character class')
                        ErrFlag = TrueVal
                        RETURN
                    END IF
                    CALL range%Include(rangeStart, rangeEnd, inc)
                    ! We are done defining the range
                    last = CHAR_INVALID
                    definingRange = FalseVal
                ELSE
                    ! If simple character and not start of range, include it
                    IF ((idx >= len).OR.(pattern(idx) /= '-')) THEN
                        CALL range%Include(simpleChar, inc)
                    END IF
                    last = simpleChar
                END IF
            END DO WhileLoop

            ! Shouldn't be out of input
            IF (idx == len) THEN
                CALL Handle_ErrLevel('RECompiler_CharacterClass', ModName, ErrSevere, &
                                     'Unterminated character class')
                ErrFlag = TrueVal
                RETURN
            END IF

            ! Absorb the ']' end of class marker
            idx = idx + 1

            ! Emit character class definition
            instruction(Indx + offsetOpdata) = ToChar(range%num)
            DO I = 0, range%num-1
                CALL Compiler%Emit(ToChar(range%minRange(I)))
                CALL Compiler%Emit(ToChar(range%maxRange(I)))
            END DO
        END BLOCK
    END ASSOCIATE

    RETURN

END FUNCTION RECompiler_CharacterClass

!******************************************************************************

FUNCTION RECompiler_Atom(Compiler, ErrFlag) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To absorb an atomic character string.  This method is a little tricky because
    !  it can un-include the last character of string if a closure operator follows.
    !  This is correct because *+? have higher precedence than concatentation (thus
    !  ABC* means AB(C*) and NOT (ABC)*).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler !! RECompiler object
    tLogical,          INTENT(OUT)      :: ErrFlag  !! true if error occurred
    tSInt32                             :: Indx     !! Index of new atom node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: lenAtom, idxEscape, idxBeforeEscape, espcode
    tChar       :: c

! FLOW

    ErrFlag = FalseVal

    ! Create a string node
    Indx = Compiler%Node(OP_ATOM, 0)

    ! Length of atom
    lenAtom = 0

    ASSOCIATE (instruction => Compiler%instruction, pattern => Compiler%pattern, &
               len => Compiler%len, idx => Compiler%idx)
        ! Loop while we've got input
        atomLoop: DO WHILE (idx < len)
            ! Is there a next char?
            IF ((idx + 1) < len) THEN
                c = pattern(idx+1)
                ! If the next 'char' is an escape, look past the whole escape
                IF (pattern(idx) == CHR_BACKSLASH) THEN
                    idxEscape = idx
                    espcode = Compiler%Escape(ErrFlag)
                    IF (ErrFlag) RETURN
                    IF (idx < len) THEN
                        c = pattern(idx)
                    END IF
                    idx = idxEscape
                END IF
                ! Switch on next char
                SELECT CASE (c)
                CASE (CHR_BRACE_LEFT, CHR_QUESTION_MARK, CHR_ASTERISK, CHR_PLUS_SIGN)
                    ! If the next character is a closure operator and our atom is non-empty, the
                    ! current character should bind to the closure operator rather than the atom
                    IF (lenAtom /= 0) THEN
                        EXIT atomLoop
                    END IF
                END SELECT
            END IF

            ! Switch on current char
            SELECT CASE (pattern(idx))
            CASE (CHR_BRACKET_RIGHT, CHR_CARET, CHR_DOLLAR_SIGN, CHR_PERIOD, CHR_BRACKET_LEFT, &
                    CHR_PARENTHESES_LEFT, CHR_PARENTHESES_RIGHT, CHR_VERTICAL_BAR)
                EXIT atomLoop
            CASE (CHR_BRACE_LEFT, CHR_QUESTION_MARK, CHR_ASTERISK, CHR_PLUS_SIGN)
                ! We should have an atom by now
                IF (lenAtom == 0) THEN
                    ! No atom before closure
                    CALL Handle_ErrLevel('RECompiler_Atom', ModName, ErrSevere, &
                                         'Syntax error: Missing operand to closure')
                    ErrFlag = TrueVal
                    RETURN
                END IF
                EXIT atomLoop
            CASE (CHR_BACKSLASH)
                ! Get the escaped character (advances input automatically)
                idxBeforeEscape = idx
                espcode = Compiler%Escape(ErrFlag)
                IF (ErrFlag) RETURN
                ! Check if it's a simple escape (as opposed to, say, a backreference)
                IF (IAND(espcode, ESC_MASK) == ESC_MASK) THEN
                    ! Not a simple escape, so backup to where we were before the escape.
                    idx = idxBeforeEscape
                    EXIT atomLoop
                END IF
                ! Add escaped char to atom
                CALL Compiler%Emit(ToChar(espcode))
                lenAtom = lenAtom + 1
            CASE DEFAULT
                ! Add normal character to atom
                CALL Compiler%Emit(pattern(idx))
                idx = idx + 1
                lenAtom = lenAtom + 1
            END SELECT
        END DO atomLoop

        ! This "shouldn't" happen
        IF (lenAtom == 0) THEN
            CALL Handle_ErrLevel('RECompiler_Atom', ModName, ErrSevere, 'Internal error!')
            ErrFlag = TrueVal
            RETURN
        END IF

        ! Emit the atom length into the program
        instruction(Indx + offsetOpdata) = ToChar(lenAtom)
    END ASSOCIATE

    RETURN

END FUNCTION RECompiler_Atom

!******************************************************************************

FUNCTION RECompiler_Terminal(Compiler, Flags, ErrFlag) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To match a terminal node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler     !! RECompiler object
    tSInt32,           INTENT(INOUT)    :: Flags(0:)    !! flags
    tLogical,          INTENT(OUT)      :: ErrFlag      !! true if error occurred
    tSInt32                             :: Indx         !! Index of terminal node (closeable)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ErrFlag = FalseVal
    ASSOCIATE (pattern => Compiler%pattern, idx => Compiler%idx, parens => Compiler%parens)
        SELECT CASE (pattern(idx))
        CASE (OP_EOL, OP_BOL, OP_ANY)
            Indx = Compiler%Node(pattern(idx), 0)
            idx = idx + 1
            RETURN
        CASE (OP_ANYOF)
            Indx = Compiler%CharacterClass(ErrFlag)
            RETURN
        CASE (OP_OPEN)
            Indx = Compiler%Expression(Flags, ErrFlag)
            RETURN
        CASE (OP_CLOSE)
            CALL Handle_ErrLevel('RECompiler_Terminal', ModName, ErrSevere, &
                                 'Syntax error: Unexpected close parentheses')
            ErrFlag = TrueVal
            RETURN
        CASE (OP_BRANCH)
            CALL Handle_ErrLevel('RECompiler_Terminal', ModName, ErrSevere, 'Internal error!')
            ErrFlag = TrueVal
            RETURN
        CASE (CHR_BRACKET_RIGHT)
            CALL Handle_ErrLevel('RECompiler_Terminal', ModName, ErrSevere, &
                                 'Syntax error: Mismatched class')
            ErrFlag = TrueVal
            RETURN
        CASE (ToChar(0))
            CALL Handle_ErrLevel('RECompiler_Terminal', ModName, ErrSevere, &
                                 'Syntax error: Unexpected end of input')
            ErrFlag = TrueVal
            RETURN
        CASE (OP_MAYBE, OP_PLUS, OP_STAR, CHR_BRACE_LEFT)
            CALL Handle_ErrLevel('RECompiler_Terminal', ModName, ErrSevere, &
                                 'Syntax error: Missing operand to closure')
            ErrFlag = TrueVal
            RETURN
        CASE (OP_ESCAPE)
            BLOCK
                tSInt32     :: idxBeforeEscape, espCode, backreference
                ! Don't forget, escape() advances the input stream!
                idxBeforeEscape = idx
                espCode = Compiler%Escape(ErrFlag)
                IF (ErrFlag) RETURN
                ! Switch on escaped character
                SELECT CASE (espCode)
                CASE (ESC_CLASS, ESC_COMPLEX)
                    Flags(0) = IAND(Flags(0), NOT(NODE_NULLABLE))
                    Indx = Compiler%Node(OP_ESCAPE, CharCode(pattern(idx-1)))
                    RETURN
                CASE (ESC_BACKREF)
                    backreference = CharCode(pattern(idx-1)) - CharCode('0')
                    IF (parens <= backreference) THEN
                        CALL Handle_ErrLevel('RECompiler_Terminal', ModName, ErrSevere, &
                                             'Syntax error: Bad backreference')
                        ErrFlag = TrueVal
                        RETURN
                    END IF
                    Flags(0) = IOR(Flags(0), NODE_NULLABLE)
                    Indx = Compiler%Node(OP_BACKREF, backreference)
                    RETURN
                CASE DEFAULT
                    ! We had a simple escape and we want to have it end up in
                    ! an atom, so we back up and fall though to the default handling
                    idx = idxBeforeEscape
                    Flags(0) = IAND(Flags(0), NOT(NODE_NULLABLE))
                END SELECT
            END BLOCK
        END SELECT

        ! Everything above either fails or returns.
        ! If it wasn't one of the above, it must be the start of an atom.
        Flags(0) = IAND(Flags(0), NOT(NODE_NULLABLE))
        Indx = Compiler%Atom(ErrFlag)
    END ASSOCIATE

    RETURN

END FUNCTION RECompiler_Terminal

!******************************************************************************

FUNCTION RECompiler_Closure(Compiler, Flags, ErrFlag) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compile a possibly closured terminal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler     !! RECompiler object
    tSInt32,           INTENT(INOUT)    :: Flags(0:)    !! flags
    tLogical,          INTENT(OUT)      :: ErrFlag      !! true if error occurred
    tSInt32                             :: Indx         !! Index of closured node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: idxBeforeTerminal, n
    tSInt32     :: terminalFlags(0:0)
    tLogical    :: greedy
    tChar       :: opcode, closureType

! FLOW

    ErrFlag = FalseVal
    ASSOCIATE (instruction => Compiler%instruction, lenInstruction => Compiler%lenInstruction, &
               pattern => Compiler%pattern, len => Compiler%len, idx => Compiler%idx)
        ! Before terminal
        idxBeforeTerminal = idx
        ! Values to pass by reference to Compiler%Terminal()
        terminalFlags = NODE_NORMAL
        ! Get terminal symbol
        Indx = Compiler%Terminal(terminalFlags, ErrFlag)
        IF (ErrFlag) RETURN
        ! Or in flags from terminal symbol
        flags(0) = IOR(flags(0), terminalFlags(0))
        ! Advance input, set NODE_NULLABLE flag and do sanity checks
        IF (idx >= len) RETURN
        greedy = TrueVal
        closureType = pattern(idx)
        SELECT CASE (closureType)
        CASE (OP_MAYBE, OP_STAR, OP_PLUS, CHR_BRACE_LEFT)
            IF ((closureType == OP_MAYBE).OR.(closureType == OP_STAR)) THEN
                ! The current node can be null
                flags(0) = IOR(flags(0), NODE_NULLABLE)
            END IF
            IF (closureType /= CHR_BRACE_LEFT) THEN
                ! Eat closure character
                idx = idx + 1
            END IF
            ! Don't allow blantant stupidity
            opcode = instruction(Indx)
            IF ((opcode == OP_BOL).OR.(opcode == OP_EOL)) THEN
                CALL Handle_ErrLevel('RECompiler_Closure', ModName, ErrSevere, &
                                     'Syntax error: Bad closure operand')
                ErrFlag = TrueVal
                RETURN
            END IF
            IF (IAND(terminalFlags(0), NODE_NULLABLE) /= 0) THEN
                CALL Handle_ErrLevel('RECompiler_Closure', ModName, ErrSevere, &
                                     "Syntax error: Closure operand can't be nullable")
                ErrFlag = TrueVal
                RETURN
            END IF
        END SELECT
        ! If the next character is a '?', make the closure non-greedy (reluctant)
        IF ((idx < len).AND.(pattern(idx) == OP_MAYBE)) THEN
            idx = idx + 1
            greedy = FalseVal
        END IF
        IF (greedy) THEN
            ! Actually do the closure now
            GreedyClosure: SELECT CASE (closureType)
            CASE (CHR_BRACE_LEFT)
                CALL Compiler%Bracket(ErrFlag)
                IF (ErrFlag) RETURN
                BLOCK
                    tSInt32     :: bracketEnd, bracketMin, bracketOpt
                    tSInt32     :: dummy, pos, c, nextpos
                    ! initialize
                    bracketEnd = idx
                    bracketMin = Compiler%bracketMin
                    bracketOpt = Compiler%bracketOpt
                    ! Pointer to the last terminal
                    pos = Indx
                    ! Process min first
                    DO c = 1, bracketMin-1
                        ! Rewind stream and run it through again - more matchers coming
                        idx = idxBeforeTerminal
                        nextpos = Compiler%Terminal(terminalFlags, ErrFlag)
                        IF (ErrFlag) RETURN
                        CALL Compiler%SetNextOfEnd(pos, nextpos, ErrFlag)
                        IF (ErrFlag) RETURN
                        pos = nextpos
                    END DO
                    ! Do the right thing for maximum ({m,})
                    IF (bracketOpt == bracketUnbounded) THEN
                        ! Drop through now and closure expression.
                        ! We are done with the {m,} expr, so skip rest
                        idx = bracketEnd
                        CALL Compiler%NodeInsert(OP_STAR, 0, pos)
                        CALL Compiler%SetNextOfEnd(pos + nodeSize, pos, ErrFlag)
                        IF (ErrFlag) RETURN
                        EXIT GreedyClosure
                    ELSEIF (bracketOpt > 0) THEN
                        BLOCK
                            tSInt32     :: opt(0:bracketOpt)
                            tSInt32     :: endpos
                            ! Surround first optional terminal with MAYBE
                            CALL Compiler%NodeInsert(OP_MAYBE, 0, pos)
                            opt(0) = pos
                            ! Add all the rest optional terminals with preceeding MAYBEs
                            !for (int c = 1 c < bracketOpt c++)
                            DO c = 1, bracketOpt-1
                                opt(c) = Compiler%Node(OP_MAYBE, 0)
                                ! Rewind stream and run it through again - more matchers coming
                                idx = idxBeforeTerminal
                                dummy = Compiler%Terminal(terminalFlags, ErrFlag)
                                IF (ErrFlag) RETURN
                            END DO
                            ! Tie ends together
                            opt(bracketOpt) = Compiler%Node(OP_NOTHING, 0)
                            endpos = opt(bracketOpt)
                            !for (int c = 0 c < bracketOpt c++)
                            DO c = 0, bracketOpt-1
                                CALL Compiler%SetNextOfEnd(opt(c), endpos, ErrFlag)
                                IF (ErrFlag) RETURN
                                CALL Compiler%SetNextOfEnd(opt(c) + nodeSize, opt(c + 1), ErrFlag)
                                IF (ErrFlag) RETURN
                            END DO
                        END BLOCK
                    ELSE
                        ! Rollback terminal - no opt matchers present
                        lenInstruction = pos
                        dummy = Compiler%Node(OP_NOTHING, 0)
                    END IF
                    ! We are done. skip the reminder of {m,n} expr
                    idx = bracketEnd
                    !EXIT GreedyClosure
                END BLOCK
            CASE (OP_MAYBE)
                CALL Compiler%NodeInsert(OP_MAYBE, 0, Indx)
                n = Compiler%Node(OP_NOTHING, 0)
                CALL Compiler%SetNextOfEnd(Indx, n, ErrFlag)
                IF (ErrFlag) RETURN
                CALL Compiler%SetNextOfEnd(Indx + nodeSize, n, ErrFlag)
                !IF (ErrFlag) RETURN
                !EXIT GreedyClosure
            CASE (OP_STAR)
                CALL Compiler%NodeInsert(OP_STAR, 0, Indx)
                CALL Compiler%SetNextOfEnd(Indx + nodeSize, Indx, ErrFlag)
                !IF (ErrFlag) RETURN
                !EXIT GreedyClosure
            CASE (OP_PLUS)
                CALL Compiler%NodeInsert(OP_CONTINUE, 0, Indx)
                n = Compiler%Node(OP_PLUS, 0)
                CALL Compiler%SetNextOfEnd(Indx + nodeSize, n, ErrFlag)
                IF (ErrFlag) RETURN
                CALL Compiler%SetNextOfEnd(n, Indx, ErrFlag)
                !IF (ErrFlag) RETURN
                !EXIT GreedyClosure
            END SELECT GreedyClosure
        ELSE
            ! Actually do the closure now
            SELECT CASE (closureType)
            CASE (OP_MAYBE)
                CALL Compiler%NodeInsert(OP_RELUCTANTMAYBE, 0, Indx)
                n = Compiler%Node(OP_NOTHING, 0)
                CALL Compiler%SetNextOfEnd(Indx, n, ErrFlag)
                IF (ErrFlag) RETURN
                CALL Compiler%SetNextOfEnd(Indx + nodeSize, n, ErrFlag)
                !IF (ErrFlag) RETURN
            CASE (OP_STAR)
                CALL Compiler%NodeInsert(OP_RELUCTANTSTAR, 0, Indx)
                CALL Compiler%SetNextOfEnd(Indx + nodeSize, Indx, ErrFlag)
                !IF (ErrFlag) RETURN
            CASE (OP_PLUS)
                CALL Compiler%NodeInsert(OP_CONTINUE, 0, Indx)
                n = Compiler%Node(OP_RELUCTANTPLUS, 0)
                CALL Compiler%SetNextOfEnd(n, Indx, ErrFlag)
                IF (ErrFlag) RETURN
                CALL Compiler%SetNextOfEnd(Indx + nodeSize, n, ErrFlag)
                !IF (ErrFlag) RETURN
            END SELECT
        END IF
    END ASSOCIATE

    RETURN

END FUNCTION RECompiler_Closure

!******************************************************************************

FUNCTION RECompiler_Branch(Compiler, Flags, ErrFlag) RESULT(Ptr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compile body of one branch of an or operator (implements concatenation).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler     !! RECompiler object
    tSInt32,           INTENT(INOUT)    :: Flags(0:)    !! flags
    tLogical,          INTENT(OUT)      :: ErrFlag      !! true if error occurred
    tSInt32                             :: Ptr          !! pointer to first node in the branch

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: node, chain, closureFlags(0:0)
    tLogical    :: nullable

! FLOW

    ErrFlag = FalseVal
    ASSOCIATE (pattern => Compiler%pattern, idx => Compiler%idx, len => Compiler%len)
        ! Get each possibly closured piece and concat
        Ptr = -1
        chain = -1
        nullable = TrueVal
        DO WHILE ((idx < len).AND.(pattern(idx) /= OP_BRANCH).AND.(pattern(idx) /= OP_CLOSE))
            ! Get new node
            closureFlags(0) = NODE_NORMAL
            node = Compiler%Closure(closureFlags, ErrFlag)
            IF (ErrFlag) RETURN
            IF (closureFlags(0) == NODE_NORMAL) nullable = FalseVal
            ! If there's a chain, append to the end
            IF (chain /= -1) THEN
                CALL Compiler%SetNextOfEnd(chain, node, ErrFlag)
                IF (ErrFlag) RETURN
            END IF
            ! Chain starts at current
            chain = node
            IF (Ptr == -1) Ptr = node
        END DO
        ! If we don't run loop, make a nothing node
        IF (Ptr == -1)Ptr = Compiler%Node(OP_NOTHING, 0)
        ! Set nullable flag for this branch
        IF (nullable) Flags(0) = IOR(Flags(0), NODE_NULLABLE)
    END ASSOCIATE

    RETURN

END FUNCTION RECompiler_Branch

!******************************************************************************

FUNCTION RECompiler_Expression(Compiler, Flags, ErrFlag) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compile an expression with possible parentheses around it.  Parentheses
    !  matching is done at this level so we can tie the branch tails together.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler     !! RECompiler object
    tSInt32,           INTENT(INOUT)    :: Flags(0:)    !! flags
    tLogical,          INTENT(OUT)      :: ErrFlag      !! true if error occurred
    tSInt32                             :: Indx         !! index of expression in instruction array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: paren, closeParens, branch, nextBranch
    tSInt32     :: end, currentNode, nextNodeOffset, dummy
    tLogical    :: open

! FLOW

    ErrFlag = FalseVal
    ASSOCIATE (parens => Compiler%parens, instruction => Compiler%instruction, lenInstruction => Compiler%lenInstruction, &
               pattern => Compiler%pattern, len => Compiler%len, idx => Compiler%idx)
        ! Create open paren node unless we were called from the top level (which has no parens)
        paren = -1
        Indx = -1
        closeParens = parens
        IF ((IAND(Flags(0), NODE_TOPLEVEL) == 0).AND.(pattern(idx) == OP_OPEN)) THEN
            ! if its a cluster ( rather than a proper subexpression ie with backrefs )
            IF ((idx + 2 < len).AND.(pattern(idx+1) == OP_MAYBE).AND. &
                (pattern(idx+2) == CHR_COLON)) THEN
                paren = 2
                idx = idx + 3
                Indx = Compiler%Node(OP_OPEN_CLUSTER, 0)
            ELSE
                paren = 1
                idx = idx + 1
                Indx = Compiler%Node(OP_OPEN, parens)
                parens = parens + 1
            END IF
        END IF
        Flags(0) = IAND(Flags(0), NOT(NODE_TOPLEVEL))
        ! Process contents of first branch node
        open = FalseVal
        branch = Compiler%Branch(Flags, ErrFlag)
        IF (ErrFlag) RETURN
        IF (Indx == -1) THEN
            Indx = branch
        ELSE
            CALL Compiler%SetNextOfEnd(Indx, branch, ErrFlag)
            IF (ErrFlag) RETURN
        END IF
        ! Loop through branches
        DO WHILE ((idx < len).AND.(pattern(idx) == OP_BRANCH))
            ! Now open the first branch since there are more than one
            IF (.NOT.open) THEN
                CALL Compiler%NodeInsert(OP_BRANCH, 0, branch)
                open = TrueVal
            END IF
            idx = idx + 1
            nextBranch = Compiler%Node(OP_BRANCH, 0)
            CALL Compiler%SetNextOfEnd(branch, nextBranch, ErrFlag)
            IF (ErrFlag) RETURN
            branch = nextBranch
            dummy = Compiler%Branch(Flags, ErrFlag)
            IF (ErrFlag) RETURN
        END DO

        ! Create an ending node (either a close paren or an OP_END)
        IF (paren > 0) THEN
            IF ((idx < len).AND.(pattern(idx) == OP_CLOSE)) THEN
                idx = idx + 1
            ELSE
                CALL Handle_ErrLevel('RECompiler_Expression', ModName, ErrSevere, &
                                     'Syntax error: Missing close paren')
                ErrFlag = TrueVal
                RETURN
            END IF
            IF (paren == 1) THEN
                end = Compiler%Node(OP_CLOSE, closeParens)
            ELSE
                end = Compiler%Node(OP_CLOSE_CLUSTER, 0)
            END IF
        ELSE
            end = Compiler%Node(OP_END, 0)
        END IF
        ! Append the ending node to the Indx nodelist
        CALL Compiler%SetNextOfEnd(Indx, end, ErrFlag)
        IF (ErrFlag) RETURN
        ! Hook the ends of each branch to the end node
        currentNode = Indx
        nextNodeOffset = CharCode(instruction(currentNode + offsetNext))
        ! while the next node o
        DO WHILE ((nextNodeOffset /= 0).AND.(currentNode < lenInstruction))
            ! If branch, make the end of the branch's operand chain point to the end node.
            IF (instruction(currentNode) == OP_BRANCH) THEN
                CALL Compiler%SetNextOfEnd(currentNode + nodeSize, end, ErrFlag)
                IF (ErrFlag) RETURN
            END IF
            nextNodeOffset = CharCode(instruction(currentNode + offsetNext))
            currentNode = currentNode + nextNodeOffset
        END DO
        ! Return the node list
    END ASSOCIATE

    RETURN

END FUNCTION RECompiler_Expression

!******************************************************************************

FUNCTION RECompiler_Compile(Compiler, RegexPat, ErrFlag) RESULT(PG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compile a regular expression pattern into a program runnable by the pattern
    !  matcher class 'REMatcher'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RECompiler), INTENT(INOUT)    :: Compiler !! RECompiler object
    tCharStar,         INTENT(IN)       :: RegexPat !! regular expression pattern
    tLogical,          INTENT(OUT)      :: ErrFlag  !! true if error occurred
    TYPE(REProgram)                     :: PG       !! REProgram object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: flags(0:0), dummy, I

! FLOW

    ! Initialize variables for compilation
    CALL Compiler%Initialize()
    Compiler%len = LEN(RegexPat)    ! Precompute pattern length for speed
    ! Save pattern in instance variable
    IF (ALLOCATED(Compiler%pattern)) DEALLOCATE(Compiler%pattern)
    ALLOCATE (tChar :: Compiler%pattern(0:Compiler%len-1))
    DO i = 1, Compiler%len
        Compiler%pattern(i-1) = RegexPat(i:i)
    END DO
    Compiler%idx = 0                ! Set parsing index to the first character
    Compiler%lenInstruction = 0     ! Set emitted instruction count to zero
    Compiler%parens = 1             ! Set paren level to 1 (the implicit outer parens)

    ErrFlag = FalseVal
    ASSOCIATE (instruction => Compiler%instruction, lenInstruction => Compiler%lenInstruction, &
               pattern => Compiler%pattern, len => Compiler%len, idx => Compiler%idx, &
               parens => Compiler%parens)
        ! Initialize pass by reference flags value
        flags = NODE_TOPLEVEL
        ! Parse expression
        dummy = Compiler%Expression(flags, ErrFlag)
        IF (ErrFlag) RETURN
        ! Should be at end of input
        IF (idx /= len) THEN
            IF (pattern(idx) == OP_CLOSE) THEN
                CALL Handle_ErrLevel('RECompiler_Compile', ModName, ErrSevere, &
                                     'Syntax error: Unmatched close paren')
            ELSE
                CALL Handle_ErrLevel('RECompiler_Compile', ModName, ErrSevere, &
                                     'Syntax error: Unexpected input remains')
            END IF
            ErrFlag = TrueVal
            RETURN
        END IF
        ! Return the result
        CALL PG%Construct(instruction(0:lenInstruction-1), parens)
    END ASSOCIATE

    RETURN

END FUNCTION RECompiler_Compile

!-------------------------------------------------------------------------------
!                           RERange Procedures
!-------------------------------------------------------------------------------

SUBROUTINE RERange_Initialize(Range)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the RERange object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RERange), INTENT(OUT) :: Range    ! RERange object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Range%Size = 16_kIndex
    Range%Num = 0
    CALL MemAlloc(Range%MinRange, Range%Size, StartID=0_kIndex)
    CALL MemAlloc(Range%MaxRange, Range%Size, StartID=0_kIndex)
    Range%MinRange = 0
    Range%MaxRange = 0

    RETURN

END SUBROUTINE RERange_Initialize

!******************************************************************************

SUBROUTINE RERange_Delete(Range, Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To delete the range at a given index from the range lists.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RERange), INTENT(INOUT)   :: Range    ! RERange object
    tSInt32,        INTENT(IN)      :: Index    ! Index of range to delete from minRange and maxRange arrays

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW

    I = Index
    ASSOCIATE (num => Range%Num, minRange => Range%MinRange, maxRange => Range%MaxRange)
        ! Return if no elements left or index is out of range
        IF ((num == 0).OR.(I >= num)) RETURN
        ! Move elements down
        I = I + 1
        DO WHILE (I < num)
            IF ((I - 1) >= 0) THEN
                minRange(I-1) = minRange(I)
                maxRange(I-1) = maxRange(I)
            END IF
            I = I + 1
        END DO

        ! One less element now
        num = num - 1
    END ASSOCIATE

    RETURN

END SUBROUTINE RERange_Delete

!******************************************************************************

SUBROUTINE RERange_Merge(Range, Min, Max)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To merge a range into the range list, coalescing ranges if possible.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RERange), INTENT(INOUT)   :: Range    ! RERange object
    tSInt32,        INTENT(IN)      :: Min      ! Minimum end of range
    tSInt32,        INTENT(IN)      :: Max      ! Maximum end of range

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW

    ASSOCIATE (num => Range%Num, minRange => Range%MinRange, maxRange => Range%MaxRange)
        ! Loop through ranges
        DO I = 0, num-1
            ! Min-max is subsumed by minRange(I)-maxRange(I)
            IF ((min >= minRange(I)).AND.(max <= maxRange(I))) THEN
                RETURN
            ! Min-max subsumes minRange(I)-maxRange(I)
            ELSEIF ((min <= minRange(I)).AND.(max >= maxRange(I))) THEN
                CALL Range%Delete(I)
                CALL Range%Merge(min, max)
                RETURN
            ! Min is in the range, but max is outside
            ELSEIF ((min >= minRange(I)).AND.(min <= maxRange(I))) THEN
                BLOCK
                    tSInt32     :: newMin
                    newMin = minRange(I)
                    CALL Range%Delete(I)
                    CALL Range%Merge(newMin, max)
                END BLOCK
                RETURN
            ! Max is in the range, but min is outside
            ELSEIF ((max >= minRange(I)).AND.(max <= maxRange(I))) THEN
                BLOCK
                    tSInt32     :: newMax
                    newMax = maxRange(I)
                    CALL Range%Delete(I)
                    CALL Range%Merge(min, newMax)
                END BLOCK
                RETURN
            END IF
        END DO
    END ASSOCIATE

    ! Must not overlap any other ranges
    IF (Range%Num >= Range%Size) THEN
        BLOCK
            tSInt32,  ALLOCATABLE   :: newMin(:), newMax(:)
            Range%Size = Range%Size*2_kIndex
            CALL MemAlloc(newMin, Range%Size, StartID=0_kIndex)
            CALL MemAlloc(newMax, Range%Size, StartID=0_kIndex)
            newMin(0:Range%Num-1) = Range%MinRange(0:Range%Num-1)
            newMax(0:Range%Num-1) = Range%MaxRange(0:Range%Num-1)
            newMin(Range%Num:) = 0
            newMax(Range%Num:) = 0
            CALL MOVE_ALLOC(newMin, Range%MinRange)
            CALL MOVE_ALLOC(newMax, Range%MaxRange)
        END BLOCK
    END IF
    Range%MinRange(Range%Num) = min
    Range%MaxRange(Range%Num) = max
    Range%Num = Range%Num + 1

    RETURN

END SUBROUTINE RERange_Merge

!******************************************************************************

SUBROUTINE RERange_Remove(Range, Min, Max)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove a range by deleting or shrinking all other ranges.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RERange), INTENT(INOUT)   :: Range    ! RERange object
    tSInt32,        INTENT(IN)      :: Min      ! Minimum end of range
    tSInt32,        INTENT(IN)      :: Max      ! Maximum end of range

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW

    ASSOCIATE (num => Range%Num, minRange => Range%MinRange, maxRange => Range%MaxRange)
        ! Loop through ranges
        DO I = 0, num-1
            ! minRange(I)-maxRange(I) is subsumed by min-max
            IF ((minRange(I) >= min).AND.(maxRange(I) <= max)) THEN
                CALL Range%Delete(I)
                RETURN
            ! min-max is subsumed by minRange(I)-maxRange(I)
            ELSEIF ((min >= minRange(I)).AND.(max <= maxRange(I))) THEN
                BLOCK
                    tSInt32     :: minr, maxr
                    minr = minRange(I)
                    maxr = maxRange(I)
                    CALL Range%Delete(I)
                    IF (minr < min) CALL Range%Merge(minr, min - 1)
                    IF (max < maxr) CALL Range%Merge(max + 1, maxr)
                END BLOCK
                RETURN
            ! minRange is in the range, but maxRange is outside
            ELSEIF ((minRange(I) >= min).AND.(minRange(I) <= max)) THEN
                minRange(I) = max + 1
                RETURN
            ! maxRange is in the range, but minRange is outside
            ELSEIF ((maxRange(I) >= min).AND.(maxRange(I) <= max)) THEN
                maxRange(I) = min - 1
                RETURN
            END IF
        END DO
    END ASSOCIATE

    RETURN

END SUBROUTINE RERange_Remove

!******************************************************************************

SUBROUTINE RERange_Include1(Range, Min, Max, Inc)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To include (or excludes) the range from min to max, inclusive.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RERange), INTENT(INOUT)   :: Range    ! RERange object
    tSInt32,        INTENT(IN)      :: Min      ! Minimum end of range
    tSInt32,        INTENT(IN)      :: Max      ! Maximum end of range
    tLogical,       INTENT(IN)      :: Inc      ! True if range should be included.  False otherwise.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Inc) THEN
        CALL Range%Merge(Min, Max)
    ELSE
        CALL Range%Remove(Min, Max)
    END IF

    RETURN

END SUBROUTINE RERange_Include1

!******************************************************************************

SUBROUTINE RERange_Include2(Range, MinMax, Inc)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To include a range with the same min and max.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RERange), INTENT(INOUT)   :: Range    ! RERange object
    tChar,          INTENT(IN)      :: MinMax   ! Minimum and maximum end of range (inclusive)
    tLogical,       INTENT(IN)      :: Inc      ! True if range should be included.  False otherwise.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Range%Include(CharCode(MinMax), CharCode(MinMax), Inc)

    RETURN

END SUBROUTINE RERange_Include2

!******************************************************************************

END MODULE MClass_RECompiler

!******************************************************************************
