
MODULE MClass_DFAutomaton

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *DFAutomaton* type and its related routines.
!   The *DFAutomaton* type is a derived type representing a ... <br>

!** USE STATEMENTS:
    USE ISO_C_BINDING,          ONLY: C_SIZEOF
    USE MBase_Common
    USE MBase_MemHandlers
    USE MBase_ErrHandlers
    USE MBase_SIntUtil,         ONLY: ToChar => ToDecStrSigned
    USE MClass_Object,          ONLY: Object
    USE MBase_OptimalHash32,    ONLY: HashFuncOpt => Murmur3_Hash32_Opt
    USE MClass_IntrusiveHashList
    USE MClass_IntrusiveLinkedLists
    USE MClass_TreeInteger4B
    USE MClass_PQInteger4B
    USE MClass_SyntaxTree
    USE MClass_BitmapState

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: DFAutomaton
    PUBLIC :: CompiledRegex

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tHash               tIndex

!** MODULE PARAMETERS:
    tCharStar, PARAMETER            :: ModName = 'Class_DFAutomaton'
    tSInt32,   PARAMETER            :: MsgLen  = 128

!** DERIVED TYPE DEFINITIONS
    TYPE CompiledRegex
        tSInt32,  ALLOCATABLE   :: TransitionTable(:,:)
        tSInt32                 :: IS = -1  ! initial state
        tSInt32                 :: RS = -1  ! rejected state
        tLogical, ALLOCATABLE   :: FS(:)    ! final states
    END TYPE CompiledRegex
    !> The *DFAutomaton* type is a node type...
    TYPE DFAutomaton
        TYPE(BitmapStateManager), POINTER   :: Manager   => NULL()
        TYPE(BitmapState),        POINTER   :: States(:) => NULL()
    CONTAINS
        PROCEDURE   :: Construct        => DFAutomaton_Construct
        PROCEDURE   :: Destruct         => DFAutomaton_Destruct
        PROCEDURE   :: Initialize       => DFAutomaton_Initialize
        PROCEDURE   :: NFATransMap      => DFAutomaton_NFATransMap
        PROCEDURE   :: OriginalDFA      => DFAutomaton_OriginalDFA
        PROCEDURE   :: CalculateClosure => DFAutomaton_CalculateClosure
        PROCEDURE   :: DfsClosure       => DFAutomaton_DfsClosure
        PROCEDURE   :: TraceReachable   => DFAutomaton_TraceReachable
        PROCEDURE   :: Minimize         => DFAutomaton_Minimize
    END TYPE DFAutomaton
    !> The *StatePackNode* type is a node type...
    TYPE, EXTENDS(HashListNode) :: StatePackNode
        TYPE(BitmapState),     POINTER  :: State => NULL()
        TYPE(BitmapStatePack), POINTER  :: Pack => NULL()
    CONTAINS
        PROCEDURE   :: Copy         => StatePackNode_Copy
        PROCEDURE   :: IsEqualTo    => StatePackNode_IsEqualTo
        PROCEDURE   :: ToString     => StatePackNode_ToString
        PROCEDURE   :: IsKeyEqual   => StatePackNode_IsKeyEqual
        PROCEDURE   :: MemFree      => StatePackNode_MemFree
        PROCEDURE   :: HashCode     => StatePackNode_HashCode
        PROCEDURE   :: Construct    => StatePackNode_Construct
    END TYPE StatePackNode
    !> The *StateMapNode* type is a node type...
    TYPE, EXTENDS(HashListNode) :: StateMapNode
        TYPE(BitmapState), POINTER  :: State => NULL()
        TYPE(IntrusiveHashList)     :: SubMap   ! use BitMapNode as node
    CONTAINS
        PROCEDURE   :: Copy         => StateMapNode_Copy
        PROCEDURE   :: IsEqualTo    => StateMapNode_IsEqualTo
        PROCEDURE   :: ToString     => StateMapNode_ToString
        PROCEDURE   :: IsKeyEqual   => StateMapNode_IsKeyEqual
        PROCEDURE   :: MemFree      => StateMapNode_MemFree
        PROCEDURE   :: HashCode     => StateMapNode_HashCode
        PROCEDURE   :: Construct    => StateMapNode_Construct
    END TYPE StateMapNode
    !> The *PackMapNode* type is a node type...
    TYPE, EXTENDS(HashListNode) :: PackMapNode
        TYPE(BitmapStatePack), POINTER  :: Pack => NULL()
        TYPE(IntrusiveHashList)         :: SubMap   ! use BitMapNode as node
    CONTAINS
        PROCEDURE   :: Copy         => PackMapNode_Copy
        PROCEDURE   :: IsEqualTo    => PackMapNode_IsEqualTo
        PROCEDURE   :: ToString     => PackMapNode_ToString
        PROCEDURE   :: IsKeyEqual   => PackMapNode_IsKeyEqual
        PROCEDURE   :: MemFree      => PackMapNode_MemFree
        PROCEDURE   :: HashCode     => PackMapNode_HashCode
        PROCEDURE   :: Construct    => PackMapNode_Construct
    END TYPE PackMapNode
    !> The *PackIntegerNode* type is a node type...
    TYPE, EXTENDS(HashListNode) :: PackIntegerNode
        TYPE(BitmapStatePack), POINTER  :: Pack => NULL()
        tSInt32                         :: Val
    CONTAINS
        PROCEDURE   :: Copy         => PackIntegerNode_Copy
        PROCEDURE   :: IsEqualTo    => PackIntegerNode_IsEqualTo
        PROCEDURE   :: ToString     => PackIntegerNode_ToString
        PROCEDURE   :: IsKeyEqual   => PackIntegerNode_IsKeyEqual
        PROCEDURE   :: MemFree      => PackIntegerNode_MemFree
        PROCEDURE   :: HashCode     => PackIntegerNode_HashCode
        PROCEDURE   :: Construct    => PackIntegerNode_Construct
    END TYPE PackIntegerNode
    !> The *MapSetNode* type is a node type...
    TYPE, EXTENDS(HashListNode) :: MapSetNode
        TYPE(TreeInteger4B) :: TreeMap
        TYPE(PQInteger4B)   :: PQSet
    CONTAINS
        PROCEDURE   :: Copy         => MapSetNode_Copy
        PROCEDURE   :: IsEqualTo    => MapSetNode_IsEqualTo
        PROCEDURE   :: ToString     => MapSetNode_ToString
        PROCEDURE   :: IsKeyEqual   => MapSetNode_IsKeyEqual
        PROCEDURE   :: MemFree      => MapSetNode_MemFree
        PROCEDURE   :: HashCode     => MapSetNode_HashCode
        PROCEDURE   :: Construct    => MapSetNode_Construct
    END TYPE MapSetNode
    TYPE DFANodeMemHandler
        tIndex                              :: SPNodeID = 0_kIndex
        TYPE(StatePackNode),   ALLOCATABLE  :: SPNode(:)
        tIndex                              :: SMNodeID = 0_kIndex
        TYPE(StateMapNode),    ALLOCATABLE  :: SMNode(:)
        tIndex                              :: PMNodeID = 0_kIndex
        TYPE(PackMapNode),     ALLOCATABLE  :: PMNode(:)
        tIndex                              :: PINodeID = 0_kIndex
        TYPE(PackIntegerNode), ALLOCATABLE  :: PINode(:)
        tIndex                              :: MSNodeID = 0_kIndex
        TYPE(MapSetNode),      ALLOCATABLE  :: MSNode(:)
    END TYPE DFANodeMemHandler

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    TYPE(DFANodeMemHandler), TARGET     :: DFAMemManger

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!-------------------------------------------------------------------------------
!                           DFANodeMemHandler Procedures
!-------------------------------------------------------------------------------

SUBROUTINE StatePackNode_New(Node, State, Pack)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate the node and specify its state and state pack.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(StatePackNode), POINTER, INTENT(INOUT) :: Node
    TYPE(BitmapState),            INTENT(IN)    :: State
    TYPE(BitmapStatePack),        INTENT(IN)    :: Pack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW

    IF (.NOT.ALLOCATED(DFAMemManger%SPNode)) THEN
        ! need allocation
        ALLOCATE(DFAMemManger%SPNode(64_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
        IF (AllocStat /= 0) THEN
            DFAMemManger%SPNodeID = 0_kIndex
            CALL Handle_ErrAlloc('StatePackNode_Allocate', ModName, AllocMsg, AllocStat)
            RETURN
        END IF
        DFAMemManger%SPNodeID = 1_kIndex
    ELSEIF (DFAMemManger%SPNodeID == SIZE(DFAMemManger%SPNode, KIND=kIndex)) THEN
        ! need re-allocation
        BLOCK
            TYPE(StatePackNode), ALLOCATABLE   :: NewNode(:)
            ALLOCATE(NewNode(SIZE(DFAMemManger%SPNode)*2_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
            IF (AllocStat /= 0) THEN
                CALL Handle_ErrAlloc('StatePackNode_Allocate', ModName, AllocMsg, AllocStat)
                RETURN
            END IF
            NewNode(1:SIZE(DFAMemManger%SPNode)) = DFAMemManger%SPNode
            CALL MOVE_ALLOC(NewNode, DFAMemManger%SPNode)
            DFAMemManger%SPNodeID = DFAMemManger%SPNodeID + 1
        END BLOCK
    END IF

    ! set pointer to the storage
    Node => DFAMemManger%SPNode(DFAMemManger%SPNodeID)
    CALL Node%Construct(State, Pack)

    RETURN

END SUBROUTINE StatePackNode_New

!******************************************************************************

SUBROUTINE StateMapNode_New(Node, State)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate the node and specify its state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(StateMapNode), POINTER, INTENT(INOUT)  :: Node
    TYPE(BitmapState),            INTENT(IN)    :: State

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW

    IF (.NOT.ALLOCATED(DFAMemManger%SMNode)) THEN
        ! need allocation
        ALLOCATE(DFAMemManger%SMNode(64_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
        IF (AllocStat /= 0) THEN
            DFAMemManger%SMNodeID = 0_kIndex
            CALL Handle_ErrAlloc('StateMapNode_Allocate', ModName, AllocMsg, AllocStat)
            RETURN
        END IF
        DFAMemManger%SMNodeID = 1_kIndex
    ELSEIF (DFAMemManger%SMNodeID == SIZE(DFAMemManger%SMNode, KIND=kIndex)) THEN
        ! need re-allocation
        BLOCK
            TYPE(StateMapNode), ALLOCATABLE   :: NewNode(:)
            ALLOCATE(NewNode(SIZE(DFAMemManger%SMNode)*2_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
            IF (AllocStat /= 0) THEN
                CALL Handle_ErrAlloc('StateMapNode_Allocate', ModName, AllocMsg, AllocStat)
                RETURN
            END IF
            NewNode(1:SIZE(DFAMemManger%SMNode)) = DFAMemManger%SMNode
            CALL MOVE_ALLOC(NewNode, DFAMemManger%SMNode)
            DFAMemManger%SMNodeID = DFAMemManger%SMNodeID + 1
        END BLOCK
    END IF

    ! set pointer to the storage
    Node => DFAMemManger%SMNode(DFAMemManger%SMNodeID)
    CALL Node%Construct(State)

    RETURN

END SUBROUTINE StateMapNode_New

!******************************************************************************

SUBROUTINE PackMapNode_New(Node, StatePack)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate the node and specify its state pack.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(PackMapNode), POINTER, INTENT(INOUT)   :: Node
    TYPE(BitmapStatePack),      INTENT(IN)      :: StatePack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW

    IF (.NOT.ALLOCATED(DFAMemManger%PMNode)) THEN
        ! need allocation
        ALLOCATE(DFAMemManger%PMNode(64_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
        IF (AllocStat /= 0) THEN
            DFAMemManger%PMNodeID = 0_kIndex
            CALL Handle_ErrAlloc('PackMapNode_Allocate', ModName, AllocMsg, AllocStat)
            RETURN
        END IF
        DFAMemManger%PMNodeID = 1_kIndex
    ELSEIF (DFAMemManger%PMNodeID == SIZE(DFAMemManger%PMNode, KIND=kIndex)) THEN
        ! need re-allocation
        BLOCK
            TYPE(PackMapNode), ALLOCATABLE   :: NewNode(:)
            ALLOCATE(NewNode(SIZE(DFAMemManger%PMNode)*2_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
            IF (AllocStat /= 0) THEN
                CALL Handle_ErrAlloc('PackMapNode_Allocate', ModName, AllocMsg, AllocStat)
                RETURN
            END IF
            NewNode(1:SIZE(DFAMemManger%PMNode)) = DFAMemManger%PMNode
            CALL MOVE_ALLOC(NewNode, DFAMemManger%PMNode)
            DFAMemManger%PMNodeID = DFAMemManger%PMNodeID + 1
        END BLOCK
    END IF

    ! set pointer to the storage
    Node => DFAMemManger%PMNode(DFAMemManger%PMNodeID)
    CALL Node%Construct(StatePack)

    RETURN

END SUBROUTINE PackMapNode_New

!******************************************************************************

SUBROUTINE PackIntegerNode_New(Node, StatePack, Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate the node and specify its state pack.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(PackIntegerNode), POINTER, INTENT(INOUT)   :: Node
    TYPE(BitmapStatePack),          INTENT(IN)      :: StatePack
    tSInt32,                        INTENT(IN)      :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW

    IF (.NOT.ALLOCATED(DFAMemManger%PINode)) THEN
        ! need allocation
        ALLOCATE(DFAMemManger%PINode(64_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
        IF (AllocStat /= 0) THEN
            DFAMemManger%PINodeID = 0_kIndex
            CALL Handle_ErrAlloc('PackIntegerNode_Allocate', ModName, AllocMsg, AllocStat)
            RETURN
        END IF
        DFAMemManger%PINodeID = 1_kIndex
    ELSEIF (DFAMemManger%PINodeID == SIZE(DFAMemManger%PINode, KIND=kIndex)) THEN
        ! need re-allocation
        BLOCK
            TYPE(PackIntegerNode), ALLOCATABLE   :: NewNode(:)
            ALLOCATE(NewNode(SIZE(DFAMemManger%PINode)*2_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
            IF (AllocStat /= 0) THEN
                CALL Handle_ErrAlloc('PackIntegerNode_Allocate', ModName, AllocMsg, AllocStat)
                RETURN
            END IF
            NewNode(1:SIZE(DFAMemManger%PINode)) = DFAMemManger%PINode
            CALL MOVE_ALLOC(NewNode, DFAMemManger%PINode)
            DFAMemManger%PINodeID = DFAMemManger%PINodeID + 1
        END BLOCK
    END IF

    ! set pointer to the storage
    Node => DFAMemManger%PINode(DFAMemManger%PINodeID)
    CALL Node%Construct(StatePack, Val)

    RETURN

END SUBROUTINE PackIntegerNode_New

!******************************************************************************

SUBROUTINE MapSetNode_New(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate the node and specify its state pack.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(MapSetNode), POINTER, INTENT(INOUT)    :: Node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW

    IF (.NOT.ALLOCATED(DFAMemManger%MSNode)) THEN
        ! need allocation
        ALLOCATE(DFAMemManger%MSNode(64_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
        IF (AllocStat /= 0) THEN
            DFAMemManger%MSNodeID = 0_kIndex
            CALL Handle_ErrAlloc('MapSetNode_Allocate', ModName, AllocMsg, AllocStat)
            RETURN
        END IF
        DFAMemManger%MSNodeID = 1_kIndex
    ELSEIF (DFAMemManger%MSNodeID == SIZE(DFAMemManger%MSNode, KIND=kIndex)) THEN
        ! need re-allocation
        BLOCK
            TYPE(MapSetNode), ALLOCATABLE   :: NewNode(:)
            ALLOCATE(NewNode(SIZE(DFAMemManger%MSNode)*2_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
            IF (AllocStat /= 0) THEN
                CALL Handle_ErrAlloc('MapSetNode_Allocate', ModName, AllocMsg, AllocStat)
                RETURN
            END IF
            NewNode(1:SIZE(DFAMemManger%MSNode)) = DFAMemManger%MSNode
            CALL MOVE_ALLOC(NewNode, DFAMemManger%MSNode)
            DFAMemManger%MSNodeID = DFAMemManger%MSNodeID + 1
        END BLOCK
    END IF

    ! set pointer to the storage
    Node => DFAMemManger%MSNode(DFAMemManger%MSNodeID)

    RETURN

END SUBROUTINE MapSetNode_New

!******************************************************************************

SUBROUTINE DFANodes_Free()

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free all the nodes linked to the specified node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! na

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I

! FLOW

    ! clear all components
    IF (ALLOCATED(DFAMemManger%SPNode)) THEN
        DO I = 1_kIndex, SIZE(DFAMemManger%SPNode, KIND=kIndex)
            CALL DFAMemManger%SPNode(I)%MemFree()
        END DO
        DEALLOCATE(DFAMemManger%SPNode)
    END IF
    IF (ALLOCATED(DFAMemManger%SMNode)) THEN
        DO I = 1_kIndex, SIZE(DFAMemManger%SMNode, KIND=kIndex)
            CALL DFAMemManger%SMNode(I)%MemFree()
        END DO
        DEALLOCATE(DFAMemManger%SMNode)
    END IF
    IF (ALLOCATED(DFAMemManger%PMNode)) THEN
        DO I = 1_kIndex, SIZE(DFAMemManger%PMNode, KIND=kIndex)
            CALL DFAMemManger%PMNode(I)%MemFree()
        END DO
        DEALLOCATE(DFAMemManger%PMNode)
    END IF
    IF (ALLOCATED(DFAMemManger%PINode)) THEN
        DO I = 1_kIndex, SIZE(DFAMemManger%PINode, KIND=kIndex)
            CALL DFAMemManger%PINode(I)%MemFree()
        END DO
        DEALLOCATE(DFAMemManger%PINode)
    END IF
    IF (ALLOCATED(DFAMemManger%MSNode)) THEN
        DO I = 1_kIndex, SIZE(DFAMemManger%MSNode, KIND=kIndex)
            CALL DFAMemManger%MSNode(I)%MemFree()
        END DO
        DEALLOCATE(DFAMemManger%MSNode)
    END IF
    ! reset indices
    DFAMemManger%SPNodeID = 0_kIndex
    DFAMemManger%SMNodeID = 0_kIndex
    DFAMemManger%PMNodeID = 0_kIndex
    DFAMemManger%PINodeID = 0_kIndex
    DFAMemManger%MSNodeID = 0_kIndex

    RETURN

END SUBROUTINE DFANodes_Free

!-------------------------------------------------------------------------------
!                           DFAutomaton Procedures
!-------------------------------------------------------------------------------

FUNCTION DFAutomaton_Construct(DFA, Manager) RESULT(RegexPat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the DFAutomaton object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DFAutomaton),               INTENT(OUT)   :: DFA
    TYPE(BitmapStateManager), TARGET, INTENT(IN)    :: Manager
    TYPE(CompiledRegex)                             :: RegexPat

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    DFA%Manager    => Manager
    DFA%States(0:) => Manager%GetStates()
    RegexPat%IS = -1
    RegexPat%RS = -1
    CALL DFA%Initialize(RegexPat)

    RETURN

END FUNCTION DFAutomaton_Construct

!******************************************************************************

SUBROUTINE DFAutomaton_Destruct(DFA)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct the DFAutomaton object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DFAutomaton), INTENT(INOUT)   :: DFA

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    NULLIFY(DFA%Manager)
    NULLIFY(DFA%States)
    CALL BitmapState_Free()
    CALL DFANodes_Free()

    RETURN

END SUBROUTINE DFAutomaton_Destruct

!******************************************************************************

SUBROUTINE DFAutomaton_Initialize(DFA, RegexPat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the DFAutomaton object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DFAutomaton),  INTENT(INOUT)  :: DFA
    TYPE(CompiledRegex), INTENT(INOUT)  :: RegexPat

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(BitmapState),     POINTER  :: InitState
    TYPE(BitmapState),     POINTER  :: FinalState
    TYPE(IntrusiveHashList)         :: ClosureMap   ! use StatePackNode as node
    TYPE(IntrusiveHashList)         :: NFATransMap  ! use StateMapNode as node
    TYPE(IntrusiveHashList)         :: DFATransMap  ! use PackMapNode as node
    TYPE(BitmapStatePack), POINTER  :: InitStatePack
    tIndex                          :: InitCap

! FLOW

    ! initialize
    InitState  => DFA%States(0)
    FinalState => DFA%States(1)

    ! construct map explicitly
    InitCap = SIZE(DFA%States, KIND=kIndex)
    CALL ClosureMap%Construct(InitCap)
    CALL NFATransMap%Construct(InitCap)
    CALL DFATransMap%Construct(InitCap)

    ! get closure map
    ClosureMap = DFA%CalculateClosure(DFA%States)

    ! construct a NFA first
    CALL DFA%NFATransMap(ClosureMap, NFATransMap)

    ! construct an original DFA using the constructed NFA.
    ! Each key which is set of nfa states is a new dfa state.
    InitStatePack => GetPack_StatePackNode(InitState, ClosureMap)
    CALL DFA%OriginalDFA(InitStatePack, NFATransMap, DFATransMap)

    ! construct minimum DFA
    CALL DFA%Minimize(DFATransMap, InitStatePack, FinalState, RegexPat)

    NULLIFY(InitState, FinalState, InitStatePack)

    RETURN

END SUBROUTINE DFAutomaton_Initialize

!******************************************************************************

SUBROUTINE DFAutomaton_NFATransMap(DFA, ClosureMap, NFATransMap)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the NFA transition map.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DFAutomaton),      INTENT(INOUT)  :: DFA
    TYPE(IntrusiveHashList), INTENT(INOUT)  :: ClosureMap
    TYPE(IntrusiveHashList), INTENT(INOUT)  :: NFATransMap

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(StateMapNode),    POINTER  :: NewStateMap
    TYPE(BitmapStatePack), POINTER  :: Closure
    TYPE(BitmapStatePack), POINTER  :: Reachable
    tSInt32                         :: I, J
    tChar                           :: C

! FLOW

    DO I = 0, SIZE(DFA%States)-1
        ASSOCIATE (State => DFA%States(I))
            CALL StateMapNode_New(NewStateMap, State)
            DO J = 0, EncodingLen-1
                C = ACHAR(J)
                Closure   => GetPack_StatePackNode(State, ClosureMap)
                Reachable => DFA%TraceReachable(Closure, C, ClosureMap)
                IF (.NOT.Reachable%IsEmpty()) THEN
                    BLOCK
                        TYPE(BitMapNode), POINTER   :: NewBitNode
                        CALL BitMapNode_New(NewBitNode, C, DFA%Manager, Reachable)
                        CALL NewStateMap%SubMap%Insert(NewBitNode)
                        NULLIFY(NewBitNode)
                    END BLOCK
                END IF
                CALL NFATransMap%Insert(NewStateMap)
                NULLIFY(NewStateMap, Closure, Reachable)
            END DO
        END ASSOCIATE
    END DO

    RETURN

END SUBROUTINE DFAutomaton_NFATransMap

!******************************************************************************

SUBROUTINE DFAutomaton_OriginalDFA(DFA, StateSet, NFATransMap, DFATransMap)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the original DFAutomaton object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DFAutomaton),      INTENT(INOUT)  :: DFA
    TYPE(BitmapStatePack),   INTENT(INOUT)  :: StateSet
    TYPE(IntrusiveHashList), INTENT(INOUT)  :: NFATransMap  ! use StateMapNode as node
    TYPE(IntrusiveHashList), INTENT(INOUT)  :: DFATransMap  ! use PackMapNode as node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(IntrusiveLinearList)           :: Stack    ! use BitmapStatePack as node
    TYPE(BitmapStatePack),     POINTER  :: PopNode
    TYPE(IntrusiveHashList),   POINTER  :: SubMap
    TYPE(BitmapStatePack),     POINTER  :: PackUnion
    tSInt32                             :: J
    tChar                               :: C
    TYPE(IntrusiveLinearList), POINTER  :: StateList

! FLOW

    CALL Stack%Push(StateSet)

    DO
        PopNode => StackPop(Stack)
        SubMap => GetMap_PackMapNode(PopNode, DFATransMap)
        IF (.NOT.ASSOCIATED(SubMap)) THEN
            BLOCK
                TYPE(PackMapNode), POINTER  :: NewNode
                CALL PackMapNode_New(NewNode, PopNode)
                CALL DFATransMap%Insert(NewNode)
                NULLIFY(NewNode)
            END BLOCK
        END IF
        DO J = 0, EncodingLen-1
            C = ACHAR(J)
            CALL DFA%Manager%NewEmptyPack(PackUnion)
            StateList => PopNode%AsList()
            CALL StateListLoop(StateList, NFATransMap, PackUnion, C)
            CALL PackUnion%Freeze()
            CALL CheckPackIsEmpty(PackUnion, C, DFA%Manager, SubMap, DFATransMap, Stack)
        END DO
        IF (Stack%IsEmpty()) EXIT
    END DO
    NULLIFY(PopNode, SubMap, PackUnion, StateList)

    RETURN

    CONTAINS

    FUNCTION StackPop(Stack) RESULT(PopNode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(IntrusiveLinearList), INTENT(INOUT)    :: Stack
        TYPE(BitmapStatePack),     POINTER          :: PopNode

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CLASS(DoublyLinkedNode), POINTER    :: StackNode
        tLogical                            :: Success

    ! FLOW

        PopNode => NULL()
        Success = Stack%Pop(StackNode)
        IF (Success.AND.ASSOCIATED(StackNode)) THEN
            SELECT TYPE (StackNode)
            TYPE IS (BitmapStatePack)
                PopNode => StackNode
            END SELECT
        END IF
        NULLIFY(StackNode)

        RETURN

    END FUNCTION StackPop

    !**************************************************************************

    SUBROUTINE StateListLoop(List, NFAMap, UPack, Ch)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(IntrusiveLinearList), INTENT(INOUT)    :: List
        TYPE(IntrusiveHashList),   INTENT(INOUT)    :: NFAMap  ! use StateMapNode as node
        TYPE(BitmapStatePack),     INTENT(INOUT)    :: UPack
        tChar,                     INTENT(IN)       :: Ch

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CLASS(DoublyLinkedNode), POINTER    :: ListNode
        tLogical                            :: EndOfList
        TYPE(IntrusiveHashList), POINTER    :: SubMap
        TYPE(BitmapStatePack),   POINTER    :: NFASet

    ! FLOW

        EndOfList = List%StartFirst(ListNode)
        DO WHILE (.NOT.EndOfList)
            SELECT TYPE (State => ListNode)
            TYPE IS (BitmapState)
                SubMap => GetMap_StateMapNode(State, NFAMap)
            END SELECT
            NFASet => GetPack_BitMapNode(Ch, SubMap)
            IF (ASSOCIATED(NFASet)) CALL UPack%AddAll(NFASet)
            EndOfList = List%MoveForward(ListNode)
        END DO
        NULLIFY(ListNode, SubMap, NFASet)

        RETURN

    END SUBROUTINE StateListLoop

    !**************************************************************************

    SUBROUTINE CheckPackIsEmpty(UPack, Ch, Manager, SubMap, DFAMap, Stack)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(BitmapStatePack), TARGET, INTENT(INOUT)    :: UPack
        tChar,                         INTENT(IN)       :: Ch
        TYPE(BitmapStateManager),      INTENT(INOUT)    :: Manager
        TYPE(IntrusiveHashList),       INTENT(INOUT)    :: SubMap  ! use BitMapNode as node
        TYPE(IntrusiveHashList),       INTENT(INOUT)    :: DFAMap  ! use PackMapNode as node
        TYPE(IntrusiveLinearList),     INTENT(INOUT)    :: Stack

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        IF (.NOT.UPack%IsEmpty()) THEN
            BLOCK
                TYPE(BitMapNode), POINTER   :: NewNode
                CALL BitMapNode_New(NewNode, Ch, Manager, UPack)
                CALL SubMap%Insert(NewNode)
                NULLIFY(NewNode)
            END BLOCK
            BLOCK
                TYPE(PackMapNode)   :: KeyNode
                KeyNode%Pack => UPack
                IF (.NOT.DFAMap%Contain(KeyNode)) THEN
                    CALL Stack%Push(UPack)
                END IF
                NULLIFY(KeyNode%Pack)
            END BLOCK
        END IF

        RETURN

    END SUBROUTINE CheckPackIsEmpty

    !**************************************************************************

END SUBROUTINE DFAutomaton_OriginalDFA

!******************************************************************************

FUNCTION DFAutomaton_CalculateClosure(DFA, States) RESULT(ClosureMap)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform ...

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DFAutomaton), INTENT(INOUT)   :: DFA
    TYPE(BitmapState),  INTENT(INOUT)   :: States(0:)
    TYPE(IntrusiveHashList)             :: ClosureMap   ! use StatePackNode as node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(StatePackNode),   POINTER  :: NewNode
    TYPE(BitmapStatePack), POINTER  :: Closure
    tSInt32                         :: I

! FLOW

    DO I = 0, SIZE(States)-1
        CALL DFA%Manager%NewEmptyPack(Closure)
        CALL DFA%DfsClosure(States(I), Closure)
        CALL Closure%Freeze()
        CALL StatePackNode_New(NewNode, States(I), Closure)
        CALL ClosureMap%Insert(NewNode)
        NULLIFY(NewNode, Closure)
    END DO

    RETURN

END FUNCTION DFAutomaton_CalculateClosure

!******************************************************************************

SUBROUTINE DFAutomaton_DfsClosure(DFA, State, Closure)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform ...

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DFAutomaton),    INTENT(INOUT)    :: DFA
    TYPE(BitmapState),     INTENT(INOUT)    :: State
    TYPE(BitmapStatePack), INTENT(INOUT)    :: Closure

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(IntrusiveLinearList)           :: NFAStack
    TYPE(BitmapState),         POINTER  :: Pop
    TYPE(BitmapStatePack),     POINTER  :: DirectTable
    TYPE(IntrusiveLinearList), POINTER  :: List

! FLOW

    CALL NFAStack%Push(State)
    DO
        Pop => StackPop(NFAStack)
        CALL Closure%AddState(Pop%GetID())
        DirectTable => Pop%GetDirectTable()
        List => DirectTable%AsList()
        CALL StateListLoop(List, NFAStack, Closure)
        IF (NFAStack%IsEmpty()) EXIT
    END DO
    NULLIFY(Pop, DirectTable, List)
    ASSOCIATE(Dummy => DFA); END ASSOCIATE

    RETURN

    CONTAINS

    FUNCTION StackPop(Stack) RESULT(PopNode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(IntrusiveLinearList), INTENT(INOUT)    :: Stack
        TYPE(BitmapState),         POINTER          :: PopNode

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CLASS(DoublyLinkedNode), POINTER    :: StackNode
        tLogical                            :: Success

    ! FLOW

        PopNode => NULL()
        Success = Stack%Pop(StackNode)
        IF (Success.AND.ASSOCIATED(StackNode)) THEN
            SELECT TYPE (StackNode)
            TYPE IS (BitmapState)
                PopNode => StackNode
            END SELECT
        END IF
        NULLIFY(StackNode)

        RETURN

    END FUNCTION StackPop

    !**************************************************************************

    SUBROUTINE StateListLoop(List, Stack, Closure)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(IntrusiveLinearList), INTENT(INOUT)    :: List
        TYPE(IntrusiveLinearList), INTENT(INOUT)    :: Stack
        TYPE(BitmapStatePack),     INTENT(INOUT)    :: Closure

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CLASS(DoublyLinkedNode), POINTER    :: ListNode
        tLogical                            :: EndOfList

    ! FLOW

        EndOfList = List%StartFirst(ListNode)
        DO WHILE (.NOT.EndOfList)
            SELECT TYPE (Next => ListNode)
            TYPE IS (BitmapState)
                IF (.NOT.Closure%Contain(Next%GetID())) CALL Stack%Push(Next)
            END SELECT
            EndOfList = List%MoveForward(ListNode)
        END DO
        NULLIFY(ListNode)

        RETURN

    END SUBROUTINE StateListLoop

    !**************************************************************************

END SUBROUTINE DFAutomaton_DfsClosure

!******************************************************************************

FUNCTION DFAutomaton_TraceReachable(DFA, Closure, Chr, ClosureMap) RESULT(ResPack)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform ...

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DFAutomaton),      INTENT(INOUT)  :: DFA
    TYPE(BitmapStatePack),   INTENT(INOUT)  :: Closure
    tChar,                   INTENT(IN)     :: Chr
    TYPE(IntrusiveHashList), INTENT(INOUT)  :: ClosureMap   ! use StatePackNode as node
    TYPE(BitmapStatePack),   POINTER        :: ResPack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(IntrusiveLinearList), POINTER  :: ClosureList
    CLASS(DoublyLinkedNode),   POINTER  :: ClosureNode
    TYPE(IntrusiveHashList),   POINTER  :: TransitionMap    ! use BitMapNode as node
    TYPE(BitmapStatePack),     POINTER  :: StateSet
    tLogical                            :: IsListEmpty

! FLOW

    CALL DFA%Manager%NewEmptyPack(ResPack)
    ClosureList => Closure%AsList()
    IsListEmpty = ClosureList%StartFirst(ClosureNode)
    DO WHILE (.NOT.IsListEmpty)
        SELECT TYPE (ClosureState => ClosureNode)
        TYPE IS (BitmapState)
            TransitionMap => ClosureState%GetTransitionMap()
            StateSet => GetPack_BitMapNode(Chr, TransitionMap)
            IF (ASSOCIATED(StateSet)) THEN
                CALL StateListLoop(StateSet, ResPack, ClosureMap)
            END IF
        END SELECT
        IsListEmpty = ClosureList%MoveForward(ClosureNode)
    END DO
    CALL ResPack%Freeze()

    NULLIFY(ClosureNode, ClosureList, TransitionMap, StateSet)

    RETURN

    CONTAINS

    SUBROUTINE StateListLoop(StateSet, Pack, ClosureMap)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(BitmapStatePack),   INTENT(INOUT)  :: StateSet
        TYPE(BitmapStatePack),   INTENT(INOUT)  :: Pack
        TYPE(IntrusiveHashList), INTENT(INOUT)  :: ClosureMap   ! use StatePackNode as node

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(IntrusiveLinearList), POINTER  :: List
        CLASS(DoublyLinkedNode),   POINTER  :: ListNode
        TYPE(BitmapStatePack),     POINTER  :: Other
        tLogical                            :: EndOfList

    ! FLOW

        List => StateSet%AsList()
        EndOfList = List%StartFirst(ListNode)
        DO WHILE (.NOT.EndOfList)
            SELECT TYPE (State => ListNode)
            TYPE IS (BitmapState)
                Other => GetPack_StatePackNode(State, ClosureMap)
                CALL Pack%AddAll(Other)
            END SELECT
            EndOfList = List%MoveForward(ListNode)
        END DO
        NULLIFY(List, ListNode, Other)

        RETURN

    END SUBROUTINE StateListLoop

    !**************************************************************************

END FUNCTION DFAutomaton_TraceReachable

!******************************************************************************

SUBROUTINE DFAutomaton_Minimize(DFA, TransMap, InitClosure, FinalState, RegexPat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct minimum DFA.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DFAutomaton),      INTENT(INOUT)  :: DFA
    TYPE(IntrusiveHashList), INTENT(INOUT)  :: TransMap ! use PackMapNode as node
    TYPE(BitmapStatePack),   INTENT(IN)     :: InitClosure
    TYPE(BitmapState),       INTENT(IN)     :: FinalState
    TYPE(CompiledRegex),     INTENT(INOUT)  :: RegexPat

!** SUBROUTINE DERIVED-TYPE DEFINITIONS:
    TYPE RejectState
        tSInt32     :: Val(0:EncodingLen-1) = 0
    END TYPE RejectState

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TreeInteger4B)         :: renamedDFATransitionTable    ! use integer-RejectState as key-value pair
    TYPE(TreeInteger4B)         :: finalFlags                   ! use integer-logical as key-value pair
    TYPE(TreeInteger4B)         :: groupFlags                   ! use integer-integer as key-value pair
    TYPE(IntrusiveHashList)     :: stateRenamingMap             ! use PackIntegerNode
    tSInt32                     :: initStateAfterRenaming
    tSInt32                     :: renamingStateID
    tSInt32                     :: groupTotal
    TYPE(RejectState)           :: newRejectedState

! FLOW

    ! initialize
    initStateAfterRenaming = -1
    renamingStateID = 1
    CALL stateRenamingMap%Construct()

    ! rename all states
    BLOCK
        TYPE(PackIntegerNode), POINTER  :: NewNode
        TYPE(BitmapStatePack), POINTER  :: nfaState
        CLASS(HashListNode),   POINTER  :: MapNode
        tLogical                        :: IsTheEnd
        IsTheEnd = TransMap%StartFirst(MapNode)
        DO WHILE (.NOT.IsTheEnd)
            SELECT TYPE (PackNode => MapNode)
            TYPE IS (PackMapNode)
                nfaState => PackNode%Pack
            END SELECT
            IF ((initStateAfterRenaming == -1).AND.(nfaState%IsEqual(InitClosure))) THEN
                initStateAfterRenaming = renamingStateID    ! preserve initial state id
            END IF
            CALL PackIntegerNode_New(NewNode, nfaState, renamingStateID)
            CALL stateRenamingMap%Insert(NewNode)
            renamingStateID = renamingStateID + 1
            IsTheEnd = TransMap%MoveForward(MapNode)
        END DO
        NULLIFY(MapNode, nfaState, NewNode)
    END BLOCK

    ! the rejected state 0
    CALL renamedDFATransitionTable%Insert(0, newRejectedState)
    CALL finalFlags%Insert(0, FalseVal)

    ! construct renamed dfa transition table
    RenamedDFA: BLOCK
        CLASS(HashListNode),     POINTER    :: MapNode
        TYPE(BitmapStatePack),   POINTER    :: MapKey
        TYPE(IntrusiveHashList), POINTER    :: MapVal
        tLogical                            :: IsTheEnd
        ! execute
        IsTheEnd = TransMap%StartFirst(MapNode)
        DO WHILE (.NOT.IsTheEnd)
            ! get map's key and value
            SELECT TYPE (PackNode => MapNode)
            TYPE IS (PackMapNode)
                MapKey => PackNode%Pack
                MapVal => PackNode%SubMap
            END SELECT
            renamingStateID = GetVal_PackIntegerNode(MapKey, stateRenamingMap)
            SubMapBlock: BLOCK
                CLASS(HashListNode),   POINTER  :: SubMapNode
                tChar                           :: SubMapKey
                TYPE(BitmapStatePack), POINTER  :: SubMapVal
                tLogical                        :: EndOfSubMap
                TYPE(RejectState)               :: State
                ! execute
                EndOfSubMap = MapVal%StartFirst(SubMapNode)
                DO WHILE (.NOT.EndOfSubMap)
                    ! get submap's key and value
                    SELECT TYPE (RowNode => SubMapNode)
                    TYPE IS (BitMapNode)
                        SubMapKey = RowNode%Chr
                        SubMapVal => RowNode%State
                    END SELECT
                    State%Val(IACHAR(SubMapKey)) = GetVal_PackIntegerNode(SubMapVal, stateRenamingMap)
                    EndOfSubMap = MapVal%MoveForward(SubMapNode)
                END DO
                CALL renamedDFATransitionTable%Insert(renamingStateID, State)
                NULLIFY(SubMapNode, SubMapVal)
            END BLOCK SubMapBlock
            IF (MapKey%Contain(FinalState%GetID())) THEN
                CALL finalFlags%Insert(renamingStateID, TrueVal)
            ELSE
                CALL finalFlags%Insert(renamingStateID, FalseVal)
            END IF
            IsTheEnd = TransMap%MoveForward(MapNode)
        END DO
        NULLIFY(MapNode, MapKey, MapVal)
    END BLOCK RenamedDFA

    ! group states to final states and non-final states
    GroupFlag: BLOCK
        tSInt32     :: I, FlagSize
        tLogical    :: Bool
        DO I = 0, finalFlags%GetSize()-1
            Bool = GetBool_IntBoolNode(I, finalFlags)
            IF (Bool) THEN
                CALL groupFlags%Insert(I, 0)
            ELSE
                CALL groupFlags%Insert(I, 1)
            END IF
        END DO
    END BLOCK GroupFlag

    ! splitting, group id is the final state id
    FinalStateID: BLOCK
        tSInt32                         :: prevGroupTotal, sensitiveGroup
        tSInt32                         :: SetID, group, Ch, targetState, targetGroup
        tLogical                        :: First, IsTheEnd
        TYPE(IntrusiveHashList)         :: invertMap        ! use MapSetNode
        TYPE(RejectState)               :: RejState
        TYPE(MapSetNode),    POINTER    :: NewNode
        TYPE(TreeInteger4B), POINTER    :: targetGroupTable
        TYPE(PQInteger4B),   POINTER    :: stateIDSet
        CLASS(HashListNode), POINTER    :: CurNode
        ! initialize
        groupTotal = 2
        DO
            prevGroupTotal = groupTotal
            DO sensitiveGroup = 0, prevGroupTotal-1
                !  <target group table, state id set>
                DO SetID = 0, groupFlags%GetSize()-1    ! use state id to iterate
                    group = GetInt_IntIntNode(SetID, groupFlags)
                    IF (sensitiveGroup == group) THEN
                        CALL MapSetNode_New(NewNode)
                        targetGroupTable => NewNode%TreeMap
                        DO Ch = 0, EncodingLen-1
                            RejState = GetRej_IntRejNode(SetID, renamedDFATransitionTable)
                            targetState = RejState%Val(Ch)
                            targetGroup = GetInt_IntIntNode(targetState, groupFlags)
                            CALL targetGroupTable%Insert(Ch, targetGroup)
                        END DO
                        stateIDSet => GetSet_MapSetNode(NewNode, invertMap)
                        IF (.NOT.ASSOCIATED(stateIDSet)) THEN
                            CALL invertMap%Insert(NewNode)
                            stateIDSet => NewNode%PQSet
                        END IF
                        CALL stateIDSet%Insert(SetID)
                        NULLIFY(NewNode, stateIDSet, targetGroupTable)
                    END IF
                END DO
                First = TrueVal
                IsTheEnd = invertMap%StartFirst(CurNode)
                DO WHILE (.NOT.IsTheEnd)
                    ! get map's key and value
                    SELECT TYPE (MapSet => CurNode)
                    TYPE IS (MapSetNode)
                        stateIDSet => GetSet_MapSetNode(MapSet, invertMap)
                        IF (First) THEN
                            First = FalseVal
                        ELSE
                            DO WHILE (.NOT.stateIDSet%IsEmpty())
                                IF (stateIDSet%Remove(SetID)) THEN
                                    CALL groupFlags%Insert(SetID, groupTotal)
                                END IF
                            END DO
                            groupTotal = groupTotal + 1
                        END IF
                    END SELECT
                    IsTheEnd = invertMap%MoveForward(CurNode)
                END DO
            END DO
            IF (prevGroupTotal == groupTotal) EXIT
        END DO
    END BLOCK FinalStateID

    ! determine initial group state
    RegexPat%IS = GetInt_IntIntNode(initStateAfterRenaming, groupFlags)

    ! determine rejected group state
    RegexPat%RS = GetInt_IntIntNode(0, groupFlags)

    ! determine final group states
    FinalGroupStates: BLOCK
        TYPE(TreeInteger4B) :: finalGroupFlags
        tSInt32             :: I, groupFlag
        DO I = 0, groupFlags%GetSize()-1
            groupFlag = GetInt_IntIntNode(I, groupFlags)
            IF (GetBool_IntBoolNode(I, finalFlags)) CALL finalGroupFlags%Insert(groupFlag, 0)
        END DO
        CALL MemAlloc(RegexPat%FS, ToIndex(groupTotal), StartID=0_kIndex)
        DO I = 0, groupTotal-1
            RegexPat%FS(I) = finalGroupFlags%Contain(I)
        END DO
    END BLOCK FinalGroupStates

    ! construct the output transition table
    CALL MemAlloc(RegexPat%TransitionTable, [ToIndex(EncodingLen), ToIndex(groupTotal)], &
                  StartID=[0_kIndex, 0_kIndex])
    TransTable: BLOCK
        tSInt32             :: setID, groupID, Ch
        TYPE(RejectState)   :: OrgState
        OutLoop: DO groupID = 0, groupTotal-1
            ASSOCIATE (State => RegexPat%TransitionTable(:,groupID))
                InLoop: DO setID = 0, groupFlags%GetSize()-1
                    IF (groupID == GetInt_IntIntNode(setID, groupFlags)) THEN
                        OrgState = GetRej_IntRejNode(SetID, renamedDFATransitionTable)
                        DO Ch = 0, EncodingLen-1
                            State(Ch) = GetInt_IntIntNode(OrgState%Val(Ch), groupFlags)
                        END DO
                        EXIT InLoop
                    END IF
                END DO InLoop
            END ASSOCIATE
        END DO OutLoop
    END BLOCK TransTable

    ! free memory
    CALL renamedDFATransitionTable%Destruct()
    CALL finalFlags%Destruct()
    CALL groupFlags%Destruct()

    ASSOCIATE (Dummy => DFA); END ASSOCIATE

    RETURN

    CONTAINS

    FUNCTION GetInt_IntIntNode(Key, TreeMap) RESULT(Val)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,             INTENT(IN)     :: Key
        TYPE(TreeInteger4B), INTENT(INOUT)  :: TreeMap
        tSInt32                             :: Val

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CLASS(*), ALLOCATABLE   :: Any

    ! FLOW

        Val = 0
        IF (TreeMap%GetValue(Key, Any)) THEN
            SELECT TYPE (Any)
            TYPE IS (tSInt32)
                Val = Any
            END SELECT
        END IF

        RETURN

    END FUNCTION GetInt_IntIntNode

    !**************************************************************************

    FUNCTION GetBool_IntBoolNode(Key, TreeMap) RESULT(Val)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,             INTENT(IN)     :: Key
        TYPE(TreeInteger4B), INTENT(INOUT)  :: TreeMap
        tLogical                            :: Val

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CLASS(*), ALLOCATABLE   :: Any

    ! FLOW

        Val = FalseVal
        IF (TreeMap%GetValue(Key, Any)) THEN
            SELECT TYPE (Any)
            TYPE IS (tLogical)
                Val = Any
            END SELECT
        END IF

        RETURN

    END FUNCTION GetBool_IntBoolNode

    !**************************************************************************

    FUNCTION GetRej_IntRejNode(Key, TreeMap) RESULT(Val)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,             INTENT(IN)     :: Key
        TYPE(TreeInteger4B), INTENT(INOUT)  :: TreeMap
        TYPE(RejectState)                   :: Val

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CLASS(*), ALLOCATABLE   :: Any

    ! FLOW

        Val%Val = 0
        IF (TreeMap%GetValue(Key, Any)) THEN
            SELECT TYPE (Any)
            TYPE IS (RejectState)
                Val%Val = Any%Val
            END SELECT
        END IF

        RETURN

    END FUNCTION GetRej_IntRejNode

    !**************************************************************************

END SUBROUTINE DFAutomaton_Minimize

!-------------------------------------------------------------------------------
!                               StatePackNode Procedures
!-------------------------------------------------------------------------------

FUNCTION StatePackNode_IsKeyEqual(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether *key* components of the two specified objects are equal to one another or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StatePackNode), INTENT(INOUT) :: LhsObj   !! an object
    CLASS(HashListNode),  INTENT(INOUT) :: RhsObj   !! another object
    tLogical                            :: Flag     !! true if keys of both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal
    SELECT TYPE (RhsObj)
    TYPE IS (StatePackNode)
        IF (ASSOCIATED(LhsObj%State).AND.ASSOCIATED(RhsObj%State)) THEN
            Flag = LhsObj%State%IsEqual(RhsObj%State)
        END IF
    END SELECT

    RETURN

END FUNCTION StatePackNode_IsKeyEqual

!******************************************************************************

SUBROUTINE StatePackNode_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free storage/memory of an object with pointer and/or allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StatePackNode), INTENT(INOUT) :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ASSOCIATED(Obj%State)) CALL Obj%State%Destruct()
    CALL Obj%Pack%Destruct()

    RETURN

END SUBROUTINE StatePackNode_MemFree

!******************************************************************************

FUNCTION StatePackNode_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code of the *key* component of the specified object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StatePackNode), INTENT(IN)    :: Obj  !! object
    tHash                               :: Code !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Code = 0
    IF (ASSOCIATED(Obj%State)) Code = Obj%State%HashCode()

    RETURN

END FUNCTION StatePackNode_HashCode

!******************************************************************************

SUBROUTINE StatePackNode_Construct(Node, State, Pack)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the StatePackNode object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StatePackNode),          INTENT(OUT)  :: Node
    TYPE(BitmapState),     TARGET, INTENT(IN)   :: State
    TYPE(BitmapStatePack), TARGET, INTENT(IN)   :: Pack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Node%State => State
    Node%Pack  => Pack

    RETURN

END SUBROUTINE StatePackNode_Construct

!-------------------------------------------------------------------------------
!                               StateMapNode Procedures
!-------------------------------------------------------------------------------

FUNCTION StateMapNode_IsKeyEqual(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether *key* components of the two specified objects are equal to one another or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StateMapNode), INTENT(INOUT)  :: LhsObj   !! an object
    CLASS(HashListNode), INTENT(INOUT)  :: RhsObj   !! another object
    tLogical                            :: Flag     !! true if keys of both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal
    SELECT TYPE (RhsObj)
    TYPE IS (StateMapNode)
        IF (ASSOCIATED(LhsObj%State).AND.ASSOCIATED(RhsObj%State)) THEN
            Flag = LhsObj%State%IsEqual(RhsObj%State)
        END IF
    END SELECT

    RETURN

END FUNCTION StateMapNode_IsKeyEqual

!******************************************************************************

SUBROUTINE StateMapNode_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free storage/memory of an object with pointer and/or allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StateMapNode), INTENT(INOUT)  :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ASSOCIATED(Obj%State)) CALL Obj%State%Destruct()
    CALL Obj%SubMap%Clear()

    RETURN

END SUBROUTINE StateMapNode_MemFree

!******************************************************************************

FUNCTION StateMapNode_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code of the *key* component of the specified object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StateMapNode), INTENT(IN) :: Obj  !! object
    tHash                           :: Code !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Code = 0
    IF (ASSOCIATED(Obj%State)) Code = Obj%State%HashCode()

    RETURN

END FUNCTION StateMapNode_HashCode

!******************************************************************************

SUBROUTINE StateMapNode_Construct(Node, State)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the StateMapNode object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StateMapNode),       INTENT(OUT)  :: Node
    TYPE(BitmapState), TARGET, INTENT(IN)   :: State

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Node%State => State
    CALL Node%SubMap%Construct()

    RETURN

END SUBROUTINE StateMapNode_Construct

!-------------------------------------------------------------------------------
!                               PackMapNode Procedures
!-------------------------------------------------------------------------------

FUNCTION PackMapNode_IsKeyEqual(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether *key* components of the two specified objects are equal to one another or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackMapNode),  INTENT(INOUT)  :: LhsObj   !! an object
    CLASS(HashListNode), INTENT(INOUT)  :: RhsObj   !! another object
    tLogical                            :: Flag     !! true if keys of both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal
    SELECT TYPE (RhsObj)
    TYPE IS (PackMapNode)
        IF (ASSOCIATED(LhsObj%Pack).AND.ASSOCIATED(RhsObj%Pack)) THEN
            Flag = LhsObj%Pack%IsEqual(RhsObj%Pack)
        END IF
    END SELECT

    RETURN

END FUNCTION PackMapNode_IsKeyEqual

!******************************************************************************

SUBROUTINE PackMapNode_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free storage/memory of an object with pointer and/or allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackMapNode), INTENT(INOUT)   :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ASSOCIATED(Obj%Pack)) CALL Obj%Pack%Destruct()
    CALL Obj%SubMap%Clear()

    RETURN

END SUBROUTINE PackMapNode_MemFree

!******************************************************************************

FUNCTION PackMapNode_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code of the *key* component of the specified object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackMapNode), INTENT(IN)  :: Obj  !! object
    tHash                           :: Code !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Code = 0
    IF (ASSOCIATED(Obj%Pack)) Code = Obj%Pack%HashCode()

    RETURN

END FUNCTION PackMapNode_HashCode

!******************************************************************************

SUBROUTINE PackMapNode_Construct(Node, Pack)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the PackMapNode object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackMapNode),            INTENT(OUT)  :: Node
    TYPE(BitmapStatePack), TARGET, INTENT(IN)   :: Pack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Node%Pack => Pack
    CALL Node%SubMap%Construct()

    RETURN

END SUBROUTINE PackMapNode_Construct

!-------------------------------------------------------------------------------
!                               PackIntegerNode Procedures
!------------------------------------------------------------------------------

FUNCTION PackIntegerNode_IsKeyEqual(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether *key* components of the two specified objects are equal to one another or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackIntegerNode), INTENT(INOUT)   :: LhsObj   !! an object
    CLASS(HashListNode),    INTENT(INOUT)   :: RhsObj   !! another object
    tLogical                                :: Flag     !! true if keys of both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal
    SELECT TYPE (RhsObj)
    TYPE IS (PackIntegerNode)
        IF (ASSOCIATED(LhsObj%Pack).AND.ASSOCIATED(RhsObj%Pack)) THEN
            Flag = LhsObj%Pack%IsEqual(RhsObj%Pack)
        END IF
    END SELECT

    RETURN

END FUNCTION PackIntegerNode_IsKeyEqual

!*******************************************************************************

SUBROUTINE PackIntegerNode_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free storage/memory of an object with pointer and/or allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackIntegerNode), INTENT(INOUT)   :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ASSOCIATED(Obj%Pack)) CALL Obj%Pack%Destruct()

    RETURN

END SUBROUTINE PackIntegerNode_MemFree

!*******************************************************************************

FUNCTION PackIntegerNode_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code of the *key* component of the specified object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackIntegerNode), INTENT(IN)  :: Obj  !! object
    tHash                               :: Code !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Code = 0
    IF (ASSOCIATED(Obj%Pack)) Code = Obj%Pack%HashCode()

    RETURN

END FUNCTION PackIntegerNode_HashCode

!*******************************************************************************

SUBROUTINE PackIntegerNode_Construct(Node, Pack, Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the PackIntegerNode object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackIntegerNode),        INTENT(OUT)  :: Node
    TYPE(BitmapStatePack), TARGET, INTENT(IN)   :: Pack
    tSInt32,                       INTENT(IN)   :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Node%Pack => Pack
    Node%Val = Val

    RETURN

END SUBROUTINE PackIntegerNode_Construct

!-------------------------------------------------------------------------------
!                               MapSetNode Procedures
!-------------------------------------------------------------------------------

FUNCTION MapSetNode_IsKeyEqual(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether *key* components of the two specified objects are equal to one another or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MapSetNode),   INTENT(INOUT)  :: LhsObj   !! an object
    CLASS(HashListNode), INTENT(INOUT)  :: RhsObj   !! another object
    tLogical                            :: Flag     !! true if keys of both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal
    SELECT TYPE (RhsObj)
    TYPE IS (MapSetNode)
        IF (LhsObj%TreeMap%GetSize() /= RhsObj%TreeMap%GetSize()) RETURN
        BLOCK
            tLogical    :: EndOfTree
            tSInt32     :: LhsKey, RhsKey
            EndOfTree = LhsObj%TreeMap%StartFirst(LhsKey)
            EndOfTree = RhsObj%TreeMap%StartFirst(RhsKey)
            DO WHILE (.NOT.EndOfTree)
                IF (LhsKey /= RhsKey) RETURN
                EndOfTree = LhsObj%TreeMap%MoveForward(LhsKey)
                EndOfTree = RhsObj%TreeMap%MoveForward(RhsKey)
            END DO
        END BLOCK
        Flag = TrueVal
    END SELECT

    RETURN

END FUNCTION MapSetNode_IsKeyEqual

!******************************************************************************

SUBROUTINE MapSetNode_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free storage/memory of an object with pointer and/or allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MapSetNode), INTENT(INOUT)   :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%TreeMap%Destruct()
    CALL Obj%PQSet%Destruct()

    RETURN

END SUBROUTINE MapSetNode_MemFree

!******************************************************************************

FUNCTION MapSetNode_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code of the *key* component of the specified object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MapSetNode), INTENT(IN)   :: Obj  !! object
    tHash                           :: Code !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: EndOfTree
    tSInt32     :: Key, KeySum
    tIndex      :: KeySize

! FLOW

    Code = 0
    KeySum  = 0
!    EndOfTree = Obj%TreeMap%StartFirst(Key)
    DO WHILE (.NOT.EndOfTree)
        KeySum = KeySum + Key
!        EndOfTree = Obj%TreeMap%MoveForward(Key)
    END DO
    KeySize = C_SIZEOF(KeySum)
    Code = HashFuncOpt(KeySum, KeySize, 3131133)
    ASSOCIATE (Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION MapSetNode_HashCode

!******************************************************************************

SUBROUTINE MapSetNode_Construct(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MapSetNode), INTENT(INOUT)   :: Node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Node%PQSet%CreateEmpty(32_kIndex)

    RETURN

END SUBROUTINE MapSetNode_Construct

!-------------------------------------------------------------------------------
!                               Auxiliary Procedures
!-------------------------------------------------------------------------------

FUNCTION GetPack_StatePackNode(State, HashMap) RESULT(Pack)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the BitmapStatePack object from the map for the specified state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BitmapState), TARGET, INTENT(IN)       :: State
    TYPE(IntrusiveHashList),   INTENT(INOUT)    :: HashMap ! of StatePackNode
    TYPE(BitmapStatePack),     POINTER          :: Pack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(HashListNode), POINTER    :: HashNode
    TYPE(StatePackNode)             :: SPNodeIn
    tLogical                        :: Success

! FLOW

    Pack => NULL()
    SPNodeIn%State => State
    Success = HashMap%FindNode(SPNodeIn, HashNode)
    IF (Success.AND.ASSOCIATED(HashNode)) THEN
        SELECT TYPE (HashNode)
        TYPE IS (StatePackNode)
            Pack => HashNode%Pack
        END SELECT
    END IF
    NULLIFY(HashNode, SPNodeIn%State)

    RETURN

END FUNCTION GetPack_StatePackNode

!******************************************************************************

FUNCTION GetPack_BitMapNode(C, HashMap) RESULT(Pack)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the BitmapStatePack object from the map for the specified state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,                   INTENT(IN)     :: C
    TYPE(IntrusiveHashList), INTENT(INOUT)  :: HashMap ! of BitMapNode
    TYPE(BitmapStatePack),   POINTER        :: Pack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(HashListNode), POINTER    :: HashNode
    TYPE(BitMapNode)                :: SPNodeIn
    tLogical                        :: Success

! FLOW

    Pack => NULL()
    SPNodeIn%Chr = C
    Success = HashMap%FindNode(SPNodeIn, HashNode)
    IF (Success.AND.ASSOCIATED(HashNode)) THEN
        SELECT TYPE (HashNode)
        TYPE IS (BitMapNode)
            Pack => HashNode%State
        END SELECT
    END IF
    NULLIFY(HashNode)

    RETURN

END FUNCTION GetPack_BitMapNode

!******************************************************************************

FUNCTION GetMap_StateMapNode(State, MainMap) RESULT(SubMap)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the IntrusiveHashList object from the map for the specified state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BitmapState), TARGET, INTENT(IN)       :: State
    TYPE(IntrusiveHashList),   INTENT(INOUT)    :: MainMap ! of StateMapNode
    TYPE(IntrusiveHashList),   POINTER          :: SubMap

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(HashListNode), POINTER    :: HashNode
    TYPE(StateMapNode)              :: SPNodeIn
    tLogical                        :: Success

! FLOW

    SubMap => NULL()
    SPNodeIn%State => State
    Success = MainMap%FindNode(SPNodeIn, HashNode)
    IF (Success.AND.ASSOCIATED(HashNode)) THEN
        SELECT TYPE (HashNode)
        TYPE IS (StateMapNode)
            SubMap => HashNode%SubMap
        END SELECT
    END IF
    NULLIFY(HashNode, SPNodeIn%State)

    RETURN

END FUNCTION GetMap_StateMapNode

!******************************************************************************

FUNCTION GetMap_PackMapNode(Pack, MainMap) RESULT(SubMap)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the IntrusiveHashList object from the map for the specified state pack.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BitmapStatePack), TARGET, INTENT(IN)       :: Pack
    TYPE(IntrusiveHashList),       INTENT(INOUT)    :: MainMap ! of PackMapNode
    TYPE(IntrusiveHashList),       POINTER          :: SubMap

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(HashListNode), POINTER    :: HashNode
    TYPE(PackMapNode)               :: SPNodeIn
    tLogical                        :: Success

! FLOW

    SubMap => NULL()
    SPNodeIn%Pack => Pack
    Success = MainMap%FindNode(SPNodeIn, HashNode)
    IF (Success.AND.ASSOCIATED(HashNode)) THEN
        SELECT TYPE (HashNode)
        TYPE IS (PackMapNode)
            SubMap => HashNode%SubMap
        END SELECT
    END IF
    NULLIFY(HashNode, SPNodeIn%Pack)

    RETURN

END FUNCTION GetMap_PackMapNode

!******************************************************************************

FUNCTION GetVal_PackIntegerNode(Pack, HashMap) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the integer value from the map for the specified state pack.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BitmapStatePack), TARGET, INTENT(IN)       :: Pack
    TYPE(IntrusiveHashList),       INTENT(INOUT)    :: HashMap ! of PackIntegerNode
    tSInt32                                         :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(HashListNode), POINTER    :: HashNode
    TYPE(PackIntegerNode)           :: SPNodeIn
    tLogical                        :: Success

! FLOW

    Val = -1
    SPNodeIn%Pack => Pack
    Success = HashMap%FindNode(SPNodeIn, HashNode)
    IF (Success.AND.ASSOCIATED(HashNode)) THEN
        SELECT TYPE (HashNode)
        TYPE IS (PackIntegerNode)
            Val = HashNode%Val
        END SELECT
    END IF
    NULLIFY(HashNode, SPNodeIn%Pack)

    RETURN

END FUNCTION GetVal_PackIntegerNode

!******************************************************************************

FUNCTION GetSet_MapSetNode(Node, MainMap) RESULT(PQSet)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the priority queue object from the map for the specified node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(MapSetNode),        INTENT(INOUT)  :: Node
    TYPE(IntrusiveHashList), INTENT(INOUT)  :: MainMap ! of MapSetNode
    TYPE(PQInteger4B),       POINTER        :: PQSet

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(HashListNode), POINTER    :: HashNode
    tLogical                        :: Success

! FLOW

    PQSet => NULL()
    Success = MainMap%FindNode(Node, HashNode)
    IF (Success.AND.ASSOCIATED(HashNode)) THEN
        SELECT TYPE (HashNode)
        TYPE IS (MapSetNode)
            PQSet => HashNode%PQSet
        END SELECT
    END IF
    NULLIFY(HashNode)

    RETURN

END FUNCTION GetSet_MapSetNode

!******************************************************************************

! -----------------------------------------------------------------------------
! -----                     Deferred Procedures                         -----
! -----------------------------------------------------------------------------

SUBROUTINE StatePackNode_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the StatePackNode object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StatePackNode),  INTENT(IN)   :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT)  :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)   :: IsDeep
    !^ flag indicating whether to perform deep copy or shallow copy; <br>
    !  - if true, perform shallow copy; <br>
    !  - if false, perform deep copy; <br>
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE(DstObj)
    TYPE IS (StatePackNode)
        DstObj = SrcObj
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('StatePackNode_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE StatePackNode_Copy

!******************************************************************************

FUNCTION StatePackNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StatePackNode), INTENT(IN)   :: LhsObj   !! an object
    CLASS(Object),     INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (StatePackNode)
        ! this is not correct implementation but it does not matter since we do not use this any way
        Flag = TrueVal
        ASSOCIATE (Dummy => LhsObj); END ASSOCIATE
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION StatePackNode_IsEqualTo

!******************************************************************************

FUNCTION StatePackNode_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StatePackNode), INTENT(IN)   :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! this is not correct implementation but it does not matter since we do not use this any way
    Str = 'StatePackNode'
    ASSOCIATE (Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION StatePackNode_ToString

!******************************************************************************

SUBROUTINE StateMapNode_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the StateMapNode object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StateMapNode),  INTENT(IN)   :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT)  :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)   :: IsDeep
    !^ flag indicating whether to perform deep copy or shallow copy; <br>
    !  - if true, perform shallow copy; <br>
    !  - if false, perform deep copy; <br>
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE(DstObj)
    TYPE IS (StateMapNode)
        DstObj = SrcObj
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('StateMapNode_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE StateMapNode_Copy

!******************************************************************************

FUNCTION StateMapNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StateMapNode), INTENT(IN)   :: LhsObj   !! an object
    CLASS(Object),     INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (StateMapNode)
        ! this is not correct implementation but it does not matter since we do not use this any way
        Flag = TrueVal
        ASSOCIATE (Dummy => LhsObj); END ASSOCIATE
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION StateMapNode_IsEqualTo

!******************************************************************************

FUNCTION StateMapNode_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StateMapNode), INTENT(IN)   :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! this is not correct implementation but it does not matter since we do not use this any way
    Str = 'StateMapNode'
    ASSOCIATE (Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION StateMapNode_ToString

!******************************************************************************

SUBROUTINE PackMapNode_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the PackMapNode object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackMapNode),  INTENT(IN)   :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT)  :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)   :: IsDeep
    !^ flag indicating whether to perform deep copy or shallow copy; <br>
    !  - if true, perform shallow copy; <br>
    !  - if false, perform deep copy; <br>
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE(DstObj)
    TYPE IS (PackMapNode)
        DstObj = SrcObj
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('PackMapNode_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE PackMapNode_Copy

!******************************************************************************

FUNCTION PackMapNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackMapNode), INTENT(IN)   :: LhsObj   !! an object
    CLASS(Object),     INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (PackMapNode)
        ! this is not correct implementation but it does not matter since we do not use this any way
        Flag = TrueVal
        ASSOCIATE (Dummy => LhsObj); END ASSOCIATE
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION PackMapNode_IsEqualTo

!******************************************************************************

FUNCTION PackMapNode_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackMapNode), INTENT(IN)   :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! this is not correct implementation but it does not matter since we do not use this any way
    Str = 'PackMapNode'
    ASSOCIATE (Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION PackMapNode_ToString

!******************************************************************************

SUBROUTINE PackIntegerNode_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the PackIntegerNode object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackIntegerNode),  INTENT(IN)   :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT)  :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)   :: IsDeep
    !^ flag indicating whether to perform deep copy or shallow copy; <br>
    !  - if true, perform shallow copy; <br>
    !  - if false, perform deep copy; <br>
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE(DstObj)
    TYPE IS (PackIntegerNode)
        DstObj = SrcObj
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('PackIntegerNode_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE PackIntegerNode_Copy

!******************************************************************************

FUNCTION PackIntegerNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackIntegerNode), INTENT(IN)   :: LhsObj   !! an object
    CLASS(Object),     INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (PackIntegerNode)
        ! this is not correct implementation but it does not matter since we do not use this any way
        Flag = TrueVal
        ASSOCIATE (Dummy => LhsObj); END ASSOCIATE
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION PackIntegerNode_IsEqualTo

!******************************************************************************

FUNCTION PackIntegerNode_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PackIntegerNode), INTENT(IN)   :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! this is not correct implementation but it does not matter since we do not use this any way
    Str = 'PackIntegerNode'
    ASSOCIATE (Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION PackIntegerNode_ToString

!******************************************************************************

SUBROUTINE MapSetNode_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the MapSetNode object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MapSetNode),  INTENT(IN)   :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT)  :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)   :: IsDeep
    !^ flag indicating whether to perform deep copy or shallow copy; <br>
    !  - if true, perform shallow copy; <br>
    !  - if false, perform deep copy; <br>
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE(DstObj)
    TYPE IS (MapSetNode)
        DstObj = SrcObj
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('MapSetNode_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE MapSetNode_Copy

!******************************************************************************

FUNCTION MapSetNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MapSetNode), INTENT(IN)   :: LhsObj   !! an object
    CLASS(Object),     INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (MapSetNode)
        ! this is not correct implementation but it does not matter since we do not use this any way
        Flag = TrueVal
        ASSOCIATE (Dummy => LhsObj); END ASSOCIATE
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION MapSetNode_IsEqualTo

!******************************************************************************

FUNCTION MapSetNode_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MapSetNode), INTENT(IN)   :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! this is not correct implementation but it does not matter since we do not use this any way
    Str = 'MapSetNode'
    ASSOCIATE (Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION MapSetNode_ToString

!******************************************************************************

END MODULE MClass_DFAutomaton

!******************************************************************************
