
MODULE MClass_NFAState

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *NFAState* type and its related routines.
!   The *NFAState* type is a derived type representing a ... <br>

!** USE STATEMENTS:
    USE ISO_C_BINDING,          ONLY: C_SIZEOF
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_SIntUtil,         ONLY: ToChar => ToDecStrSigned
    USE MClass_Object,          ONLY: Object
    USE MBase_OptimalHash32,    ONLY: HashFuncOpt => Murmur3_Hash32_Opt
    USE MClass_IntrusiveHashList

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: NFAState
    PUBLIC :: NFAState_New
    PUBLIC :: NFAState_Free
    PUBLIC :: HashMapNode

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tHash               tIndex

!** MODULE PARAMETERS:
    tCharStar, PARAMETER            :: ModName = 'Class_NFAState'
    tSInt32,   PARAMETER            :: MsgLen  = 128

!** DERIVED TYPE DEFINITIONS
    TYPE, EXTENDS(HashListNode) :: HashMapNode
        PRIVATE
        tChar                   :: Chr
        TYPE(IntrusiveHashList) :: Set      ! use NFAState as node
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Object* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Object* object.
        PROCEDURE   :: Copy         => HashMapNode_Copy
        !> *IsEqualTo* is a procedure deferred by the *Object* type. <br>
        !  **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => HashMapNode_IsEqualTo
        !> *ToString* is a deferred procedure to get the string representation of this object.
        PROCEDURE   :: ToString     => HashMapNode_ToString
        ! ---------------------------------------------------------------------
        PROCEDURE   :: IsKeyEqual   => HashMapNode_IsKeyEqual
        PROCEDURE   :: MemFree      => HashMapNode_FreeMemory
        PROCEDURE   :: HashCode     => HashMapNode_HashCode
        ! specific
        PROCEDURE   :: Construct    => HashMapNode_Construct
        PROCEDURE   :: GetSet       => HashMapNode_GetSet
        PROCEDURE   :: GetChar      => HashMapNode_GetChar
    END TYPE HashMapNode
    !> The *NFAState* type is a node type...
    TYPE, EXTENDS(HashListNode) :: NFAState
        PRIVATE
        TYPE(IntrusiveHashList)     :: DirectTable      ! use NFAState as node
        TYPE(IntrusiveHashList)     :: TransitionMap    ! use HashMapNode as node
        tSInt32                     :: ID
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Object* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Object* object.
        PROCEDURE   :: Copy             => NFAState_Copy
        !> *IsEqualTo* is a procedure deferred by the *Object* type. <br>
        !  **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo        => NFAState_IsEqualTo
        !> *ToString* is a deferred procedure to get the string representation of this object.
        PROCEDURE   :: ToString         => NFAState_ToString
        ! ---------------------------------------------------------------------
        PROCEDURE   :: IsKeyEqual       => NFAState_IsKeyEqual
        PROCEDURE   :: MemFree          => NFAState_FreeMemory
        PROCEDURE   :: HashCode         => NFAState_HashCode
        ! specific
        PROCEDURE   :: Construct        => NFAState_Construct
        PROCEDURE   :: TransitionRule   => NFAState_TransitionRule
        PROCEDURE   :: DirectRule       => NFAState_DirectRule
        PROCEDURE   :: GetTransitionMap => NFAState_GetTransitionMap
        PROCEDURE   :: GetDirectTable   => NFAState_GetDirectTable
        PROCEDURE   :: GetID            => NFAState_GetID
    END TYPE NFAState
    TYPE NFAStateMemHandler
        tIndex                          :: StateID = 0_kIndex
        TYPE(NFAState),    ALLOCATABLE  :: State(:)
        tIndex                          :: NodeID = 0_kIndex
        TYPE(HashMapNode), ALLOCATABLE  :: Node(:)
    END TYPE NFAStateMemHandler

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    TYPE(NFAStateMemHandler), TARGET    :: NFAMemManger

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE NFAState_New(State, ID)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate the state and specify its ID.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(NFAState), POINTER, INTENT(INOUT)  :: State
    tSInt32,                 INTENT(IN)     :: ID

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW
    
    IF (.NOT.ALLOCATED(NFAMemManger%State)) THEN
        ! need allocation
        ALLOCATE(NFAMemManger%State(64_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
        IF (AllocStat /= 0) THEN
            NFAMemManger%StateID = 0_kIndex
            CALL Handle_ErrAlloc('NFAState_Allocate', ModName, AllocMsg, AllocStat)
            RETURN
        END IF
        NFAMemManger%StateID = 1_kIndex
    ELSEIF (NFAMemManger%StateID == SIZE(NFAMemManger%State, KIND=kIndex)) THEN
        ! need re-allocation
        BLOCK
            TYPE(NFAState), ALLOCATABLE   :: NewState(:)
            ALLOCATE(NewState(SIZE(NFAMemManger%State)*2_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
            IF (AllocStat /= 0) THEN
                CALL Handle_ErrAlloc('NFAState_Allocate', ModName, AllocMsg, AllocStat)
                RETURN
            END IF
            NewState(1:SIZE(NFAMemManger%State)) = NFAMemManger%State
            CALL MOVE_ALLOC(NewState, NFAMemManger%State)
            NFAMemManger%StateID = NFAMemManger%StateID + 1
        END BLOCK
    END IF
    
    ! set pointer to the storage
    State => NFAMemManger%State(NFAMemManger%StateID)
    CALL State%Construct(ID)
    
    RETURN

END SUBROUTINE NFAState_New

!******************************************************************************

SUBROUTINE HashMapNode_New(Node, Chr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate the state and specify its ID.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(HashMapNode), POINTER, INTENT(INOUT)  :: Node
    tChar,                      INTENT(IN)     :: Chr

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW
    
    IF (.NOT.ALLOCATED(NFAMemManger%Node)) THEN
        ! need allocation
        ALLOCATE(NFAMemManger%Node(64_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
        IF (AllocStat /= 0) THEN
            NFAMemManger%NodeID = 0_kIndex
            CALL Handle_ErrAlloc('HashMapNode_Allocate', ModName, AllocMsg, AllocStat)
            RETURN
        END IF
        NFAMemManger%NodeID = 1_kIndex
    ELSEIF (NFAMemManger%NodeID == SIZE(NFAMemManger%Node, KIND=kIndex)) THEN
        ! need re-allocation
        BLOCK
            TYPE(HashMapNode), ALLOCATABLE   :: NewNode(:)
            ALLOCATE(NewNode(SIZE(NFAMemManger%Node)*2_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
            IF (AllocStat /= 0) THEN
                CALL Handle_ErrAlloc('HashMapNode_Allocate', ModName, AllocMsg, AllocStat)
                RETURN
            END IF
            NewNode(1:SIZE(NFAMemManger%Node)) = NFAMemManger%Node
            CALL MOVE_ALLOC(NewNode, NFAMemManger%Node)
            NFAMemManger%NodeID = NFAMemManger%NodeID + 1
        END BLOCK
    END IF
    
    ! set pointer to the storage
    Node => NFAMemManger%Node(NFAMemManger%NodeID)
    CALL Node%Construct(Chr)
    
    RETURN

END SUBROUTINE HashMapNode_New

!******************************************************************************

SUBROUTINE NFAState_Free()

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free all the nodes linked to the specified node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! na

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I

! FLOW

    IF (ALLOCATED(NFAMemManger%Node)) THEN
        DO I = 1_kIndex, SIZE(NFAMemManger%Node, KIND=kIndex)
            CALL NFAMemManger%Node(I)%MemFree()
        END DO
        DEALLOCATE(NFAMemManger%Node)
    END IF
    IF (ALLOCATED(NFAMemManger%State)) THEN
        DO I = 1_kIndex, SIZE(NFAMemManger%State, KIND=kIndex)
            CALL NFAMemManger%State(I)%MemFree()
        END DO
        DEALLOCATE(NFAMemManger%State)
    END IF
    NFAMemManger%StateID = 0_kIndex
    NFAMemManger%NodeID = 0_kIndex

    RETURN

END SUBROUTINE NFAState_Free

!******************************************************************************

SUBROUTINE NFAState_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the NFAState object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAState),    INTENT(IN)  :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
    !^ flag indicating whether to perform deep copy or shallow copy; <br>
    !  - if true, perform shallow copy; <br>
    !  - if false, perform deep copy; <br>
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE(DstObj)
    TYPE IS (NFAState)
        DstObj%ID = SrcObj%ID
        CALL SrcObj%DirectTable%CloneTo(DstObj%DirectTable)
        CALL SrcObj%TransitionMap%CloneTo(DstObj%TransitionMap)
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('NFAState_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE NFAState_Copy

!******************************************************************************

FUNCTION NFAState_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAState), INTENT(IN) :: LhsObj   !! an object
    CLASS(Object),   INTENT(IN) :: RhsObj   !! another object
    tLogical                    :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (NFAState)
        Flag = (LhsObj%ID /= RhsObj%ID)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION NFAState_IsEqualTo

!******************************************************************************

FUNCTION NFAState_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAState), INTENT(IN)   :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Str = 'NFAState: {ID: ' // ToChar(Obj%ID) // '}'

    RETURN

END FUNCTION NFAState_ToString

!******************************************************************************

FUNCTION NFAState_IsKeyEqual(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether *key* components of the two specified objects are equal to one another or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAState),     INTENT(INOUT)  :: LhsObj   !! an object
    CLASS(HashListNode), INTENT(INOUT)  :: RhsObj   !! another object
    tLogical                            :: Flag     !! true if keys of both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    TYPE IS (NFAState)
        Flag = (LhsObj%ID == RhsObj%ID)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION NFAState_IsKeyEqual

!******************************************************************************

SUBROUTINE NFAState_FreeMemory(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free storage/memory of an object with pointer and/or allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAState), INTENT(INOUT)  :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL Obj%DirectTable%Destruct()
    CALL Obj%TransitionMap%Destruct()

    RETURN

END SUBROUTINE NFAState_FreeMemory

!******************************************************************************

FUNCTION NFAState_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code of the *key* component of the specified object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAState), INTENT(IN) :: Obj  !! object
    tHash                       :: Code !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: KeySize

! FLOW
    
    KeySize = C_SIZEOF(Obj%ID)
    Code = HashFuncOpt(Obj%ID, KeySize, 3131133)

    RETURN

END FUNCTION NFAState_HashCode

!******************************************************************************

SUBROUTINE NFAState_Construct(State, ID)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the NFAState object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAState), INTENT(OUT)    :: State
    tSInt32,         INTENT(IN)     :: ID

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL State%DirectTable%Construct()
    CALL State%TransitionMap%Construct()
    State%ID = ID

    RETURN

END SUBROUTINE NFAState_Construct

!******************************************************************************

SUBROUTINE NFAState_TransitionRule(CurrState, Chr, State)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform transition rule.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAState),         INTENT(INOUT)  :: CurrState
    tChar,                   INTENT(IN)     :: Chr
    TYPE(NFAState), POINTER, INTENT(INOUT)  :: State

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(HashListNode),     POINTER    :: StoredNode
    TYPE(HashMapNode)                   :: KeyNode
    TYPE(HashMapNode),       POINTER    :: CurNode
    TYPE(IntrusiveHashList), POINTER    :: StateSet
    tLogical                            :: Found

! FLOW
    
    KeyNode%Chr = Chr
    Found = CurrState%TransitionMap%FindNode(KeyNode, StoredNode)
    IF (Found.AND.ASSOCIATED(StoredNode)) THEN
        SELECT TYPE (StoredNode)
        TYPE IS (HashMapNode)
            StateSet => StoredNode%Set
        END SELECT
    ELSE
        CALL HashMapNode_New(CurNode, Chr)
        StateSet => CurNode%Set
        CALL CurrState%TransitionMap%Insert(CurNode)
    END IF
    CALL StateSet%Insert(State)
    NULLIFY(StoredNode, CurNode, StateSet)

    RETURN

END SUBROUTINE NFAState_TransitionRule

!******************************************************************************

SUBROUTINE NFAState_DirectRule(CurrState, State)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform direct rule.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAState),         INTENT(INOUT)  :: CurrState
    TYPE(NFAState), POINTER, INTENT(INOUT)  :: State

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL CurrState%DirectTable%Insert(State)

    RETURN

END SUBROUTINE NFAState_DirectRule

!******************************************************************************

FUNCTION NFAState_GetID(State) RESULT(ID)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get ID of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAState), INTENT(IN) :: State    !! object
    tSInt32                     :: ID

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ID = State%ID

    RETURN

END FUNCTION NFAState_GetID

!******************************************************************************

FUNCTION NFAState_GetDirectTable(State) RESULT(Table)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the direct table of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAState), TARGET, INTENT(IN) :: State    !! object
    TYPE(IntrusiveHashList), POINTER    :: Table

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Table => State%DirectTable

    RETURN

END FUNCTION NFAState_GetDirectTable

!******************************************************************************

FUNCTION NFAState_GetTransitionMap(State) RESULT(TransMap)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the transition map of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NFAState), TARGET, INTENT(IN) :: State    !! object
    TYPE(IntrusiveHashList), POINTER    :: TransMap

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    TransMap => State%TransitionMap

    RETURN

END FUNCTION NFAState_GetTransitionMap

!******************************************************************************

SUBROUTINE HashMapNode_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the HashMapNode object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMapNode), INTENT(IN)  :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
    !^ flag indicating whether to perform deep copy or shallow copy; <br>
    !  - if true, perform shallow copy; <br>
    !  - if false, perform deep copy; <br>
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE(DstObj)
    TYPE IS (HashMapNode)
        DstObj%Chr = SrcObj%Chr
        CALL SrcObj%Set%CloneTo(DstObj%Set)
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('HashMapNode_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE HashMapNode_Copy

!******************************************************************************

FUNCTION HashMapNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMapNode), INTENT(IN)  :: LhsObj   !! an object
    CLASS(Object),      INTENT(IN)  :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (HashMapNode)
        Flag = (LhsObj%Chr == RhsObj%Chr)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION HashMapNode_IsEqualTo

!******************************************************************************

FUNCTION HashMapNode_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMapNode), INTENT(IN)  :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Str = 'HashMapNode: {Chr: ' // Obj%Chr // '}'

    RETURN

END FUNCTION HashMapNode_ToString

!******************************************************************************

FUNCTION HashMapNode_IsKeyEqual(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether *key* components of the two specified objects are equal to one another or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMapNode),  INTENT(INOUT)  :: LhsObj   !! an object
    CLASS(HashListNode), INTENT(INOUT)  :: RhsObj   !! another object
    tLogical                            :: Flag     !! true if keys of both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    TYPE IS (HashMapNode)
        Flag = (LhsObj%Chr == RhsObj%Chr)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION HashMapNode_IsKeyEqual

!******************************************************************************

SUBROUTINE HashMapNode_FreeMemory(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free storage/memory of an object with pointer and/or allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMapNode), INTENT(INOUT)   :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL Obj%Set%Destruct()

    RETURN

END SUBROUTINE HashMapNode_FreeMemory

!******************************************************************************

FUNCTION HashMapNode_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code of the *key* component of the specified object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMapNode), INTENT(IN)  :: Obj  !! object
    tHash                           :: Code !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: KeySize

! FLOW
    
    KeySize = C_SIZEOF(Obj%Chr)
    Code = HashFuncOpt(Obj%Chr, KeySize, 3131133)

    RETURN

END FUNCTION HashMapNode_HashCode

!******************************************************************************

SUBROUTINE HashMapNode_Construct(State, Chr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the HashMapNode object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMapNode), INTENT(OUT) :: State
    tChar,              INTENT(IN)  :: Chr

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL State%Set%Construct()
    State%Chr = Chr

    RETURN

END SUBROUTINE HashMapNode_Construct

!******************************************************************************

FUNCTION HashMapNode_GetSet(Node) RESULT(Set)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code of the *key* component of the specified object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMapNode), TARGET,  INTENT(IN) :: Node
    TYPE(IntrusiveHashList),     POINTER    :: Set

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Set => Node%Set

    RETURN

END FUNCTION HashMapNode_GetSet

!******************************************************************************

FUNCTION HashMapNode_GetChar(Node) RESULT(Chr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code of the *key* component of the specified object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMapNode), INTENT(IN)  :: Node
    tChar                           :: Chr

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Chr = Node%Chr

    RETURN

END FUNCTION HashMapNode_GetChar

!******************************************************************************

END MODULE MClass_NFAState
    
!******************************************************************************
