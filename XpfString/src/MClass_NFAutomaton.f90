
MODULE MClass_NFAutomaton

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *NFAutomaton* type and its related routines.
!   The *NFAutomaton* type is a derived type representing a ... <br>

!** USE STATEMENTS:
    USE ISO_C_BINDING,              ONLY: C_SIZEOF
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MClass_IntrusiveHashList
    USE MClass_IntrusiveLinkedLists
    USE MClass_SyntaxNode
    USE MClass_NFAState
    USE MClass_BitmapState

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: NFAutomaton

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tHash               tIndex

!** MODULE PARAMETERS:
    tCharStar, PARAMETER            :: ModName = 'Class_NFAutomaton'
    tSInt32,   PARAMETER            :: MsgLen  = 128

!** DERIVED TYPE DEFINITIONS
    !> The *NFAutomaton* type is a node type...
    TYPE NFAutomaton
        TYPE(IntrusiveLinearList)   :: List
        TYPE(IntrusiveLinearList)   :: Stack
        tSInt32                     :: NextID = 0
    CONTAINS
        PROCEDURE, PRIVATE  :: StackPop     => NFAutomaton_StackPop
        PROCEDURE   :: Construct            => NFAutomaton_Construct
        PROCEDURE   :: Destruct             => NFAutomaton_Destruct
        PROCEDURE   :: NewState             => NFAutomaton_NewState
        PROCEDURE   :: DepthFirstSearch     => NFAutomaton_DepthFirstSearch
        PROCEDURE   :: Visit                => NFAutomaton_Visit
        PROCEDURE   :: AsBitmapStateManager => NFAutomaton_AsBitmapStateManager
    END TYPE NFAutomaton

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE NFAutomaton_Construct(NFA, Root)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the NFAutomaton object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAutomaton),        INTENT(OUT)      :: NFA
    TYPE(SyntaxNode), POINTER, INTENT(INOUT)    :: Root

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(NFAState), POINTER :: InitState
    TYPE(NFAState), POINTER :: FinalState

! FLOW

    NFA%NextID = 0
    InitState  => NFA%NewState()
    FinalState => NFA%NewState()
    CALL NFA%Stack%Push(FinalState)
    CALL NFA%Stack%Push(InitState)
    CALL NFA%DepthFirstSearch(Root)
    CALL SyntaxNode_Free()
    NULLIFY(InitState, FinalState)

    RETURN

END SUBROUTINE NFAutomaton_Construct

!******************************************************************************

SUBROUTINE NFAutomaton_Destruct(NFA)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct the NFAutomaton object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAutomaton), INTENT(INOUT)   :: NFA

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    NFA%NextID = 0
    CALL NFA%Stack%Clear()
    CALL NFA%List%Clear()
    CALL NFAState_Free()

    RETURN

END SUBROUTINE NFAutomaton_Destruct

!******************************************************************************

FUNCTION NFAutomaton_NewState(NFA) RESULT(NewState)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create new NFA state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAutomaton), INTENT(INOUT)   :: NFA
    TYPE(NFAState),     POINTER         :: NewState

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL NFAState_New(NewState, NFA%NextID)
    NFA%NextID = NFA%NextID + 1
    CALL NFA%List%AddLast(NewState)

    RETURN

END FUNCTION NFAutomaton_NewState

!******************************************************************************

RECURSIVE SUBROUTINE NFAutomaton_DepthFirstSearch(NFA, Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform depth first search.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAutomaton), INTENT(INOUT)   :: NFA
    TYPE(SyntaxNode),   INTENT(INOUT)   :: Node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL NFA%Visit(Node)
    IF (Node%HasLeft()) THEN
        CALL NFA%DepthFirstSearch(Node%GetLeft())
        CALL NFA%DepthFirstSearch(Node%GetRight())
    END IF

    RETURN

END SUBROUTINE NFAutomaton_DepthFirstSearch

!******************************************************************************

FUNCTION NFAutomaton_StackPop(NFA) RESULT(State)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create new NFA state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAutomaton), INTENT(INOUT)   :: NFA
    TYPE(NFAState),     POINTER         :: State

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: Node

! FLOW

    State => NULL()
    IF (NFA%Stack%Pop(Node)) THEN
        SELECT TYPE (Node)
        TYPE IS (NFAState)
            State => Node
        END SELECT
    END IF
    NULLIFY(Node)

    RETURN

END FUNCTION NFAutomaton_StackPop

!******************************************************************************

SUBROUTINE NFAutomaton_Visit(NFA, Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform visit operation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAutomaton), INTENT(INOUT)   :: NFA
    TYPE(SyntaxNode),   INTENT(INOUT)   :: Node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(NFAState), POINTER :: IState, FState, NState

! FLOW

    SELECT CASE (Node%Type)
    CASE (LeafNode_Char)
        IState => NFA%StackPop()
        FState => NFA%StackPop()
        IF (ASSOCIATED(IState).AND.ASSOCIATED(FState)) THEN
            CALL IState%TransitionRule(Node%C, FState)
        END IF
    CASE (BranchNode_Or)
        IState => NFA%StackPop()
        FState => NFA%StackPop()
        IF (ASSOCIATED(IState).AND.ASSOCIATED(FState)) THEN
            CALL NFA%Stack%Push(FState)
            CALL NFA%Stack%Push(IState)
            CALL NFA%Stack%Push(FState)
            CALL NFA%Stack%Push(IState)
        END IF
    CASE (BranchNode_Concat)
        IState => NFA%StackPop()
        FState => NFA%StackPop()
        NState => NFA%NewState()
        IF (ASSOCIATED(IState).AND.ASSOCIATED(FState).AND.ASSOCIATED(NState)) THEN
            CALL NFA%Stack%Push(FState)
            CALL NFA%Stack%Push(NState)
            CALL NFA%Stack%Push(NState)
            CALL NFA%Stack%Push(IState)
        END IF
    CASE (BranchNode_Many)
        IState => NFA%StackPop()
        FState => NFA%StackPop()
        NState => NFA%NewState()
        IF (ASSOCIATED(IState).AND.ASSOCIATED(FState).AND.ASSOCIATED(NState)) THEN
            CALL IState%DirectRule(NState)
            CALL NState%DirectRule(FState)
            CALL NFA%Stack%Push(NState)
            CALL NFA%Stack%Push(NState)
        END IF
    CASE (LeafNode_Closure)
        IState => NFA%StackPop()
        FState => NFA%StackPop()
        IF (ASSOCIATED(IState).AND.ASSOCIATED(FState)) THEN
            CALL IState%DirectRule(FState)
        END IF
    CASE (LeafNode_Null)
        ! do nothing here
    CASE DEFAULT
        ! report error here
        BLOCK
            tCharAlloc  :: ErrMsg
            IF (Node%Type == BranchNode_LBracket) THEN
                ErrMsg = 'Invalid operation for the left-bracket node'
            ELSE
                ErrMsg = 'Invalid operation for the right-bracket node'
            END IF
            CALL Handle_ErrLevel('NFAutomaton_Visit', ModName, ErrWarning, ErrMsg)
        END BLOCK
    END SELECT

    NULLIFY(IState, FState, NState)

    RETURN

END SUBROUTINE NFAutomaton_Visit

!******************************************************************************

FUNCTION NFAutomaton_AsBitmapStateManager(NFA) RESULT(Manager)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create new NFA state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAutomaton), INTENT(INOUT)   :: NFA
    TYPE(BitmapStateManager)            :: Manager

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Manager%Construct(NFA%List)

    RETURN

END FUNCTION NFAutomaton_AsBitmapStateManager

!******************************************************************************

END MODULE MClass_NFAutomaton

!******************************************************************************
