
MODULE MClass_RegexFinder

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *RegexFinder* type and its related helper types and routines.
!   The *RegexFinder* type is a string type that provides various efficient algorithms for
!   a *substring searching* operation.  The *substring searching* is a fundamental string
!   operation where given a *text* string of length N and a *pattern* string of length M,
!   find an occurrence of the *pattern* within the *text*.  <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://github.com/zhztheplayer/DFA-Regex">DFA-Regex: A DFA regex engine in java. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BitmapState,  ONLY: BitmapStateManager
    USE MClass_DFAutomaton
    USE MClass_NFAutomaton
    USE MClass_SyntaxNode
    USE MClass_SyntaxTree

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: RegexFinder

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName    = 'Class_RegexFinder'

!** DERIVED TYPE DEFINITIONS
    !> The *RegexFinder* type is a string type that provides various efficient
    !  algorithms for a *substring searching* operation.
    TYPE RegexFinder
        PRIVATE
        !% the cached regular expression string pattern
        tCharAlloc          :: RegexPattern
        !% the cached regular expression string compiled pattern
        TYPE(CompiledRegex) :: CompiledPattern
    CONTAINS
        !> **Type-Bound Subroutine**: SetPattern <br>
        !  **Purpose**:  To set and compile the specified pattern. <br>
        !  **Usage**: <br>
        !   --->    CALL Finder%SetPattern(Pattern) <br>
        PROCEDURE   :: SetPattern       => RegexFinder_SetPattern
        !> **Type-Bound Function**: IsMatch <br>
        !  **Purpose**:  To check whether the specified text matches the previously compiled pattern
        !                (or the optionally specified pattern). <br>
        !  **Usage**: <br>
        !   --->    Flag = Finder%IsMatch(Text) <br>
        !   --->    IF (.NOT.Finder%IsMatch(Text, Pattern)) DoSomething <br>
        PROCEDURE   :: IsMatch          => RegexFinder_IsMatch
        !> **Type-Bound Function**: FindSubstring <br>
        !  **Purpose**:  To find a substring within the specified text that matches the previously
        !                compiled pattern (or the optionally specified pattern).  If the 'StartPos'
        !                argument is present and greater than 0 and less than the text length, the
        !                search starts at the 'StartPos' position.  Otherwise, the search start at
        !                the first position of the text.  The routine returns indices indicating
        !                the starting and ending positions of the substring if found.  If the
        !                substring is not found, both indices are set to zeros. <br>
        !  **Usage**: <br>
        !   ! find a substring within the text starting at 1 where the pattern has already been set. <br>
        !   --->    Indices = Finder%FindSubstring(Text) <br>
        !   ! find a substring within the text starting at StartPos. <br>
        !   --->    Indices = Finder%FindSubstring(Text, StartPos=StartPos) <br>
        !   ! find a substring within the text where the pattern has not yet been set. <br>
        !   --->    Indices = Finder%FindSubstring(Text, Pattern=Pattern) <br>
        !   ! find a substring within the text starting at StartPos where the pattern has not yet been set. <br>
        !   --->    Indices = Finder%FindSubstring(Text, StartPos, Pattern) <br>
        PROCEDURE   :: FindSubstring    => RegexFinder_FindSubstring
    END TYPE RegexFinder

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!--------------------------------------------------------------------------------------
!                           RegexFinder Procedures
!--------------------------------------------------------------------------------------

SUBROUTINE RegexFinder_SetPattern(Finder, Pattern)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process the specified pattern based on the specified searching algorithm.
    !  If the length of the pattern is zero, just return without doing anything.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RegexFinder), INTENT(INOUT)   :: Finder   !! 'RegexFinder' object
    tCharStar,          INTENT(IN)      :: Pattern  !! the pattern

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SyntaxTree)            :: Tree
    TYPE(SyntaxNode), POINTER   :: Root
    TYPE(NFAutomaton)           :: NFA
    TYPE(BitmapStateManager)    :: Manager
    TYPE(DFAutomaton)           :: DFA

! FLOW

    ! compile the specified pattern
    CALL Tree%Construct(Pattern)
    Root => Tree%GetRoot()
    CALL NFA%Construct(Root)
    Manager = NFA%AsBitmapStateManager()
    Finder%CompiledPattern = DFA%Construct(Manager)
    ! cache the specified pattern
    Finder%RegexPattern = Pattern
    ! free memory
    CALL DFA%Destruct()
    CALL Manager%Destruct()
    CALL NFA%Destruct()
    NULLIFY(Root)
    CALL Tree%Destruct()

    RETURN

END SUBROUTINE RegexFinder_SetPattern

!******************************************************************************

FUNCTION RegexFinder_IsMatch(Finder, Text, Pattern) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified text matches the previously compiled pattern
    !  (or the optionally specified pattern).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RegexFinder),  INTENT(INOUT)  :: Finder   !! 'RegexFinder' object
    tCharStar,           INTENT(IN)     :: Text     !! the specified text
    tCharStar, OPTIONAL, INTENT(IN)     :: Pattern  !! the specified pattern
    tLogical                            :: Flag     !! true if the specified text matches the pattern

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! compile the specified pattern if necessary
    IF (PRESENT(Pattern)) THEN
        IF (ALLOCATED(Finder%RegexPattern)) THEN
            IF (Pattern /= Finder%RegexPattern) CALL Finder%SetPattern(Pattern)
        ELSE
            CALL Finder%SetPattern(Pattern)
        END IF
    END IF

    IF (ALLOCATED(Finder%RegexPattern)) THEN
        BLOCK
            tSInt32     :: S, I, C
            S = Finder%CompiledPattern%IS
            DO I = 1, LEN(Text)
                C = IACHAR(Text(I:I))
                S = Finder%CompiledPattern%TransitionTable(C, S)
                IF (S == Finder%CompiledPattern%RS) THEN
                    ! fast failed using rejected state
                    Flag = FalseVal
                    RETURN
                END IF
            END DO
            Flag = Finder%CompiledPattern%FS(S)
        END BLOCK
    ELSE
        ! pattern has not yet been compiled.
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION RegexFinder_IsMatch

!******************************************************************************

FUNCTION RegexFinder_FindSubstring(Finder, Text, StartPos, Pattern) RESULT(Indices)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find a substring within the specified text that matches the previously
    !  compiled pattern (or the optionally specified pattern).  If the 'StartPos'
    !  argument is present and greater than 0 and less than the text length, 
    !  the search starts at the 'StartPos' position.  Otherwise, the search start
    !  at the first position of the text. <br>
    !  The routine returns indices indicating the starting and ending positions of
    !  the substring if found.  If the substring is not found, both indices are set
    !  to zeros.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RegexFinder),  INTENT(INOUT)  :: Finder       !! 'RegexFinder' object
    tCharStar,           INTENT(IN)     :: Text         !! the specified text
    tSInt32,   OPTIONAL, INTENT(IN)     :: StartPos     !! the starting position
    tCharStar, OPTIONAL, INTENT(IN)     :: Pattern      !! the specified pattern
    tSInt32                             :: Indices(2)   !! indices of the substring

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: FirstPos, LastPos

! FLOW

    ! compile the specified pattern if necessary
    IF (PRESENT(Pattern)) THEN
        IF (ALLOCATED(Finder%RegexPattern)) THEN
            IF (Pattern /= Finder%RegexPattern) CALL Finder%SetPattern(Pattern)
        ELSE
            CALL Finder%SetPattern(Pattern)
        END IF
    END IF
    
    IF (ALLOCATED(Finder%RegexPattern)) THEN
        ! determine the search range
        FirstPos = 1
        LastPos  = LEN(Text)
        IF (PRESENT(StartPos)) THEN
            IF ((StartPos > 0).AND.(StartPos <= LastPos)) FirstPos = StartPos
        END IF

        ! set initial values
        Indices = 0
        BLOCK
            tSInt32     :: S, I, C
            OuterLoop: DO WHILE (FirstPos <= LastPos)
                S = Finder%CompiledPattern%IS
                InnerLoop: DO I = FirstPos, LastPos
                    C = IACHAR(Text(I:I))
                    S = Finder%CompiledPattern%TransitionTable(C, S)
                    IF (S == Finder%CompiledPattern%RS) THEN
                        ! exit loop due to rejected state
                        EXIT InnerLoop
                    ELSEIF (Finder%CompiledPattern%FS(S)) THEN
                        ! found the substring
                        Indices(1) = FirstPos
                        Indices(2) = I
                        EXIT OuterLoop
                    END IF
                END DO InnerLoop
                FirstPos = FirstPos + 1
            END DO OuterLoop
        END BLOCK
    ELSE
        ! pattern has not yet been compiled.
        Indices = 0
    END IF

    RETURN

END FUNCTION RegexFinder_FindSubstring

!******************************************************************************

END MODULE MClass_RegexFinder

!******************************************************************************
