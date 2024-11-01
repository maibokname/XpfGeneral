
MODULE MClass_REProgram

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *REProgram* type and its related routines.  <br>
!   <br>
!  **REFERENCES**: <br>
!   [1) <a href="https:!jakarta.apache.org/regexp/">The Apache Jakarta Project. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_REParameters

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: REProgram

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     CharCode(C)     ICHAR(C)
#define     ToChar(Code)    CHAR(Code, KIND=kChar)

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName    = 'Class_REProgram'

!** DERIVED TYPE DEFINITIONS
    !> The *REProgram* type is a derive type that holds compiled regular expressions.
    TYPE REProgram
        PRIVATE
        !% The compiled regular expression 'program'
        tChar, ALLOCATABLE  :: Instruction(:)
        !% the prefix string optimization
        tChar, ALLOCATABLE  :: Prefix(:)
        !% optimization flags
        tSInt32             :: Flags
        !% maximum parentheses
        tSInt32             :: MaxParen = -1
    CONTAINS
        PROCEDURE, PRIVATE  :: SetInstructions  => REProgram_SetInstructions
        PROCEDURE           :: Construct        => REProgram_Construct
        PROCEDURE           :: Destruct         => REProgram_Destruct
        PROCEDURE           :: GetInstructions  => REProgram_GetInstructions
        PROCEDURE           :: GetPrefix        => REProgram_GetPrefix
        PROCEDURE           :: GetMaxParen      => REProgram_GetMaxParen
        PROCEDURE           :: GetFlag          => REProgram_GetFlag
    END TYPE REProgram

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

SUBROUTINE REProgram_Construct(PG, Instruction, Parens)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a regular expression program from the specified instruction.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(REProgram),   INTENT(INOUT)   :: PG               !! 'REProgram' object
    tChar,              INTENT(IN)      :: Instruction(0:)  !! RE opcode instructions
    tSInt32,  OPTIONAL, INTENT(IN)      :: Parens           !! Count of parens in the program

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL PG%SetInstructions(Instruction)
    IF (PRESENT(Parens)) PG%MaxParen = Parens

    RETURN

END SUBROUTINE REProgram_Construct

!******************************************************************************

SUBROUTINE REProgram_Destruct(PG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct the 'REProgram' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(REProgram), INTENT(INOUT) :: PG   !! 'REProgram' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(PG%Instruction)) DEALLOCATE(PG%Instruction)
    IF (ALLOCATED(PG%Prefix))      DEALLOCATE(PG%Prefix)

    RETURN

END SUBROUTINE REProgram_Destruct

!******************************************************************************

SUBROUTINE REProgram_SetInstructions(PG, Instruction)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set a new regular expression program to run.  It is this method which
    !  performs any special compile-time search optimizations.  Currently only
    !  two optimizations are in place - one which checks for backreferences
    !  (so that they can be lazily allocated) and another which attempts to
    !  find an prefix anchor string so that substantial amounts of input can
    !  potentially be skipped without running the actual program.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(REProgram), INTENT(INOUT) :: PG               !! 'REProgram' object
    tChar,            INTENT(IN)    :: Instruction(0:)  !! Program instruction buffer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: LenInstruction, Next, LenAtom, I
    tChar       :: NextOp

! FLOW

    ! Save reference to instruction array
    LenInstruction = SIZE(Instruction)
    ALLOCATE(tChar :: PG%Instruction(0:LenInstruction-1))
    PG%Instruction(0:) = Instruction(0:)
    ! Initialize other program-related variables
    PG%Flags = 0
    IF (ALLOCATED(PG%Prefix)) DEALLOCATE(PG%Prefix)
    
    ! Try various compile-time optimizations if there's a program
    IF (LenInstruction /= 0) THEN
        ! If the first node is a branch
        IF ((LenInstruction >= nodeSize).AND.(Instruction(0 + offsetOpcode) == OP_BRANCH)) THEN
            ! to the end node
            Next = ToInt16(CharCode(Instruction(0 + offsetNext)))
            IF ((Instruction(Next + offsetOpcode) == OP_END).AND.(LenInstruction >= (nodeSize * 2))) THEN
                NextOp = Instruction(nodeSize + offsetOpcode)
                ! the branch starts with an atom
                IF (NextOp == OP_ATOM) THEN
                    ! then get that atom as an prefix because there's no other choice
                    LenAtom = CharCode(Instruction(nodeSize + offsetOpdata))
                    ALLOCATE(tChar :: PG%Prefix(0:LenAtom-1))
                    PG%Prefix(0:LenAtom-1) = Instruction(nodeSize*2:)
                ! the branch starts with a BOL
                ELSEIF (NextOp == OP_BOL) THEN
                    ! then set the flag indicating that BOL is present
                    PG%Flags = IOR(PG%Flags, OPT_HASBOL)
                END IF
            END IF
        END IF

        ! Check for backreferences
        I = 0
        BackrefScanLoop: DO WHILE (I < LenInstruction)
            SELECT CASE (Instruction(I + offsetOpcode))
            CASE (OP_ANYOF)
                I = I + CharCode(Instruction(I + offsetOpdata))*2
            CASE (OP_ATOM)
                I = I + CharCode(Instruction(I + offsetOpdata))
            CASE (OP_BACKREF)
                PG%Flags = IOR(PG%Flags, OPT_HASBACKREFS)
                EXIT BackrefScanLoop
            END SELECT
            I = I + nodeSize
        END DO BackrefScanLoop
    END IF

    RETURN

END SUBROUTINE REProgram_SetInstructions

!******************************************************************************

FUNCTION REProgram_GetInstructions(PG) RESULT(Instruction)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a pointer to the program instructions.  Return null
    !  if the program has not yet been compiled.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(REProgram), TARGET, INTENT(IN)    :: PG               !! 'REProgram' object
    tChar,            POINTER               :: Instruction(:)   !! Program instructions

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(PG%Instruction)) THEN
        Instruction => PG%Instruction
    ELSE
        Instruction => NULL()
    END IF
    
    RETURN

END FUNCTION REProgram_GetInstructions

!******************************************************************************

FUNCTION REProgram_GetPrefix(PG) RESULT(Prefix)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a pointer to the program prefix.  Return null
    !  if the program has not yet been compiled.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(REProgram), TARGET, INTENT(IN)    :: PG           !! 'REProgram' object
    tChar,            POINTER               :: Prefix(:)    !! Program prefix

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(PG%Prefix)) THEN
        Prefix => PG%Prefix
    ELSE
        Prefix => NULL()
    END IF
    
    RETURN

END FUNCTION REProgram_GetPrefix

!******************************************************************************

FUNCTION REProgram_GetMaxParen(PG) RESULT(MaxParen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get maximum parentheses.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(REProgram), INTENT(IN)    :: PG       !! 'REProgram' object
    tSInt32                         :: MaxParen !! maximum parentheses

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    MaxParen = PG%MaxParen
    
    RETURN

END FUNCTION REProgram_GetMaxParen

!******************************************************************************

FUNCTION REProgram_GetFlag(PG) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(REProgram), INTENT(IN)    :: PG       !! 'REProgram' object
    tSInt32                         :: Flag     !! flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = PG%Flags
    
    RETURN

END FUNCTION REProgram_GetFlag

!******************************************************************************

END MODULE MClass_REProgram

!******************************************************************************
