
MODULE MClass_BitmapState

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *BitmapState* type and its related routines.
!   The *BitmapState* type is a derived type representing a ... <br>

!** USE STATEMENTS:
    USE ISO_C_BINDING,          ONLY: C_SIZEOF
    USE MBase_Common
    USE MBase_MemHandlers
    USE MBase_ErrHandlers
    USE MBase_SIntUtil,         ONLY: ToChar => ToDecStrSigned
    USE MClass_Object,          ONLY: Object
    USE MBase_OptimalHash32,    ONLY: HashFuncOpt => Murmur3_Hash32_Opt
    USE MClass_IntrusiveLinkedLists
    USE MClass_IntrusiveHashList
    USE MClass_NFAState

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: BitmapState
    PUBLIC :: BitmapStateManager
    PUBLIC :: BitmapStatePack
    PUBLIC :: BitMapNode
    PUBLIC :: BitmapState_New
    PUBLIC :: BitmapStatePack_New
    PUBLIC :: BitMapNode_New
    PUBLIC :: BitmapState_Free

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tHash               tIndex

!** MODULE PARAMETERS:
    tCharStar, PARAMETER            :: ModName = 'Class_BitmapState'
    tSInt32,   PARAMETER            :: MsgLen  = 128

!** DERIVED TYPE DEFINITIONS
    !> The *BitmapStatePack* type is a node type...
    TYPE, EXTENDS(DoublyLinkedNode) :: BitmapStatePack
        PRIVATE
        tSInt64,              ALLOCATABLE   :: StateBitmap(:)
        TYPE(BitmapStateManager), POINTER   :: Manager => NULL()
        tLogical                            :: Writable
        TYPE(IntrusiveLinearList)           :: List         ! use BitmapState as node
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Object* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Object* object.
        PROCEDURE   :: Copy         => BitmapStatePack_Copy
        !> *IsEqualTo* is a procedure deferred by the *Object* type. <br>
        !  **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => BitmapStatePack_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Object* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Object* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: MemFree      => BitmapStatePack_MemFree
        !> *ToString* is a deferred procedure to get the string representation of this object.
        PROCEDURE   :: ToString     => BitmapStatePack_ToString
        !> *HashCode* is a deferred procedure to compute hash code of this object.
        PROCEDURE   :: HashCode     => BitmapStatePack_HashCode
        ! ---------------------------------------------------------------------
        ! -----                     Specific Procedures                   -----
        ! ---------------------------------------------------------------------
        PROCEDURE   :: Construct        => BitmapStatePack_Construct
        PROCEDURE   :: Destruct         => BitmapStatePack_Destruct
        PROCEDURE   :: AddState         => BitmapStatePack_AddState
        PROCEDURE   :: AddAll           => BitmapStatePack_AddAll
        PROCEDURE   :: Contain          => BitmapStatePack_Contain
        PROCEDURE   :: IsEmpty          => BitmapStatePack_IsEmpty
        PROCEDURE   :: Size             => BitmapStatePack_Size
        PROCEDURE   :: AsList           => BitmapStatePack_AsList
        PROCEDURE   :: Freeze           => BitmapStatePack_Freeze
        PROCEDURE   :: EnsureWritable   => BitmapStatePack_EnsureWritable
        PROCEDURE   :: IsEqual          => BitmapStatePack_IsEqual
    END TYPE BitmapStatePack
    !> The *BitmapState* type is a node type...
    TYPE, EXTENDS(DoublyLinkedNode) :: BitmapState
        PRIVATE
        TYPE(BitmapStatePack)               :: DirectTable
        TYPE(BitmapStateManager), POINTER   :: Manager     => NULL()
        TYPE(IntrusiveHashList)             :: TransitionMap    ! use BitMapNode as node
        tSInt32                             :: ID
        tLogical                            :: Writable = TrueVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Object* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Object* object.
        PROCEDURE   :: Copy         => BitmapState_Copy
        !> *IsEqualTo* is a procedure deferred by the *Object* type. <br>
        !  **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => BitmapState_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Object* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Object* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: MemFree      => BitmapState_MemFree
        !> *ToString* is a deferred procedure to get the string representation of this object.
        PROCEDURE   :: ToString     => BitmapState_ToString
        !> *HashCode* is a deferred procedure to compute hash code of this object.
        PROCEDURE   :: HashCode     => BitmapState_HashCode
        ! ---------------------------------------------------------------------
        ! -----                     Specific Procedures                   -----
        ! ---------------------------------------------------------------------
        PROCEDURE   :: Construct        => BitmapState_Construct
        PROCEDURE   :: Destruct         => BitmapState_Destruct
        PROCEDURE   :: TransitionRule   => BitmapState_TransitionRule
        PROCEDURE   :: DirectRule       => BitmapState_DirectRule
        PROCEDURE   :: GetTransitionMap => BitmapState_GetTransitionMap
        PROCEDURE   :: GetDirectTable   => BitmapState_GetDirectTable
        PROCEDURE   :: GetID            => BitmapState_GetID
        PROCEDURE   :: Freeze           => BitmapState_Freeze
        PROCEDURE   :: EnsureWritable   => BitmapState_EnsureWritable
        PROCEDURE   :: IsEqual          => BitmapState_IsEqual
    END TYPE BitmapState
    !> The *BitmapStateManager* type is a node type...
    TYPE BitmapStateManager
        PRIVATE
        TYPE(BitmapState), ALLOCATABLE  :: IStates(:)
    CONTAINS
        PROCEDURE   :: Construct        => BitmapStateManager_Construct
        PROCEDURE   :: Destruct         => BitmapStateManager_Destruct
        PROCEDURE   :: GetState         => BitmapStateManager_GetState
        PROCEDURE   :: GetStates        => BitmapStateManager_GetStates
        PROCEDURE   :: StateCount       => BitmapStateManager_StateCount
        PROCEDURE   :: NewEmptyPack     => BitmapStateManager_NewEmptyPack
    END TYPE BitmapStateManager
    !> The *BitMapNode* type is a node type...
    TYPE, EXTENDS(HashListNode) :: BitMapNode
        tChar                           :: Chr
        TYPE(BitmapStatePack), POINTER  :: State => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Object* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Object* object.
        PROCEDURE   :: Copy         => BitmapNode_Copy
        !> *IsEqualTo* is a procedure deferred by the *Object* type. <br>
        !  **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => BitmapNode_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Object* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Object* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: MemFree      => BitMapNode_FreeMemory
        !> *ToString* is a deferred procedure to get the string representation of this object.
        PROCEDURE   :: ToString     => BitmapNode_ToString
        !> *HashCode* is a deferred procedure to compute hash code of this object.
        PROCEDURE   :: HashCode     => BitmapNode_HashCode
        ! ---------------------------------------------------------------------
        PROCEDURE   :: IsKeyEqual   => BitMapNode_IsKeyEqual
!        PROCEDURE   :: MemFree      => BitMapNode_FreeMemory
!        PROCEDURE   :: HashCode     => BitMapNode_HashCode
        PROCEDURE   :: Construct    => BitMapNode_Construct
        ! ---------------------------------------------------------------------
    END TYPE BitMapNode
    TYPE BitmapStateMemHandler
        tIndex                              :: StateID = 0_kIndex
        TYPE(BitmapState),     ALLOCATABLE  :: BMState(:)
        tIndex                              :: PackID = 0_kIndex
        TYPE(BitmapStatePack), ALLOCATABLE  :: BMPack(:)
        tIndex                              :: NodeID = 0_kIndex
        TYPE(BitMapNode),      ALLOCATABLE  :: BMNode(:)
    END TYPE BitmapStateMemHandler

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    TYPE(BitmapStateMemHandler), TARGET :: BMSMemManger

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!-------------------------------------------------------------------------------
!                           BitmapStateMemHandler Procedures
!-------------------------------------------------------------------------------

SUBROUTINE BitmapState_New(State, ID, Manager)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate the state and specify its ID.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BitmapState), POINTER, INTENT(INOUT)  :: State
    tSInt32,                    INTENT(IN)     :: ID
    TYPE(BitmapStateManager),   INTENT(IN)     :: Manager

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW
    
    IF (.NOT.ALLOCATED(BMSMemManger%BMState)) THEN
        ! need allocation
        ALLOCATE(BMSMemManger%BMState(64_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
        IF (AllocStat /= 0) THEN
            BMSMemManger%StateID = 0_kIndex
            CALL Handle_ErrAlloc('BitmapState_Allocate', ModName, AllocMsg, AllocStat)
            RETURN
        END IF
        BMSMemManger%StateID = 1_kIndex
    ELSEIF (BMSMemManger%StateID == SIZE(BMSMemManger%BMState, KIND=kIndex)) THEN
        ! need re-allocation
        BLOCK
            TYPE(BitmapState), ALLOCATABLE   :: NewState(:)
            ALLOCATE(NewState(SIZE(BMSMemManger%BMState)*2_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
            IF (AllocStat /= 0) THEN
                CALL Handle_ErrAlloc('BitmapState_Allocate', ModName, AllocMsg, AllocStat)
                RETURN
            END IF
            NewState(1:SIZE(BMSMemManger%BMState)) = BMSMemManger%BMState
            CALL MOVE_ALLOC(NewState, BMSMemManger%BMState)
            BMSMemManger%StateID = BMSMemManger%StateID + 1
        END BLOCK
    END IF
    
    ! set pointer to the storage
    State => BMSMemManger%BMState(BMSMemManger%StateID)
    CALL State%Construct(ID, Manager)
    
    RETURN

END SUBROUTINE BitmapState_New

!******************************************************************************

SUBROUTINE BitmapStatePack_New(Pack, Manager)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate the state and specify its ID.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BitmapStatePack), POINTER, INTENT(INOUT)  :: Pack
    TYPE(BitmapStateManager),       INTENT(IN)     :: Manager

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW
    
    IF (.NOT.ALLOCATED(BMSMemManger%BMPack)) THEN
        ! need allocation
        ALLOCATE(BMSMemManger%BMPack(64_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
        IF (AllocStat /= 0) THEN
            BMSMemManger%PackID = 0_kIndex
            CALL Handle_ErrAlloc('BitmapStatePack_Allocate', ModName, AllocMsg, AllocStat)
            RETURN
        END IF
        BMSMemManger%PackID = 1_kIndex
    ELSEIF (BMSMemManger%PackID == SIZE(BMSMemManger%BMPack, KIND=kIndex)) THEN
        ! need re-allocation
        BLOCK
            TYPE(BitmapStatePack), ALLOCATABLE   :: NewState(:)
            ALLOCATE(NewState(SIZE(BMSMemManger%BMPack)*2_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
            IF (AllocStat /= 0) THEN
                CALL Handle_ErrAlloc('BitmapStatePack_Allocate', ModName, AllocMsg, AllocStat)
                RETURN
            END IF
            NewState(1:SIZE(BMSMemManger%BMPack)) = BMSMemManger%BMPack
            CALL MOVE_ALLOC(NewState, BMSMemManger%BMPack)
            BMSMemManger%PackID = BMSMemManger%PackID + 1
        END BLOCK
    END IF
    
    ! set pointer to the storage
    Pack => BMSMemManger%BMPack(BMSMemManger%PackID)
    CALL Pack%Construct(Manager)
    
    RETURN

END SUBROUTINE BitmapStatePack_New

!******************************************************************************

SUBROUTINE BitMapNode_New(Node, Chr, Manager, StatePack)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate the state and specify its ID.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BitMapNode),     POINTER,           INTENT(INOUT)  :: Node
    tChar,                                   INTENT(IN)     :: Chr
    TYPE(BitmapStateManager),                INTENT(IN)     :: Manager
    TYPE(BitmapStatePack), TARGET, OPTIONAL, INTENT(IN)     :: StatePack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW
    
    IF (.NOT.ALLOCATED(BMSMemManger%BMNode)) THEN
        ! need allocation
        ALLOCATE(BMSMemManger%BMNode(64_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
        IF (AllocStat /= 0) THEN
            BMSMemManger%NodeID = 0_kIndex
            CALL Handle_ErrAlloc('BitMapNode_Allocate', ModName, AllocMsg, AllocStat)
            RETURN
        END IF
        BMSMemManger%NodeID = 1_kIndex
    ELSEIF (BMSMemManger%NodeID == SIZE(BMSMemManger%BMNode, KIND=kIndex)) THEN
        ! need re-allocation
        BLOCK
            TYPE(BitMapNode), ALLOCATABLE   :: NewNode(:)
            ALLOCATE(NewNode(SIZE(BMSMemManger%BMNode)*2_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
            IF (AllocStat /= 0) THEN
                CALL Handle_ErrAlloc('BitMapNode_Allocate', ModName, AllocMsg, AllocStat)
                RETURN
            END IF
            NewNode(1:SIZE(BMSMemManger%BMNode)) = BMSMemManger%BMNode
            CALL MOVE_ALLOC(NewNode, BMSMemManger%BMNode)
            BMSMemManger%NodeID = BMSMemManger%NodeID + 1
        END BLOCK
    END IF
    
    ! set pointer to the storage
    Node => BMSMemManger%BMNode(BMSMemManger%NodeID)
    CALL Node%Construct(Chr, Manager, StatePack)
    
    RETURN

END SUBROUTINE BitMapNode_New

!******************************************************************************

SUBROUTINE BitmapState_Free()

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free all the nodes linked to the specified node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! na

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I

! FLOW
    
    IF (ALLOCATED(BMSMemManger%BMNode)) THEN
        DO I = 1_kIndex, SIZE(BMSMemManger%BMNode, KIND=kIndex)
            CALL BMSMemManger%BMNode(I)%MemFree()
        END DO
        DEALLOCATE(BMSMemManger%BMNode)
    END IF
    IF (ALLOCATED(BMSMemManger%BMPack)) THEN
        DO I = 1_kIndex, SIZE(BMSMemManger%BMPack, KIND=kIndex)
            CALL BMSMemManger%BMPack(I)%Destruct()
        END DO
        DEALLOCATE(BMSMemManger%BMPack)
    END IF
    IF (ALLOCATED(BMSMemManger%BMState)) THEN
        DO I = 1_kIndex, SIZE(BMSMemManger%BMState, KIND=kIndex)
        CALL BMSMemManger%BMState(I)%Destruct()
        END DO
        DEALLOCATE(BMSMemManger%BMState)
    END IF
    BMSMemManger%PackID = 0_kIndex
    BMSMemManger%StateID = 0_kIndex
    BMSMemManger%NodeID = 0_kIndex

    RETURN

END SUBROUTINE BitmapState_Free

! -----------------------------------------------------------------------------
! -----                     BitmapState Procedures                         -----
! -----------------------------------------------------------------------------

SUBROUTINE BitmapState_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the BitmapState object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), INTENT(IN)  :: SrcObj   !! source object
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
    TYPE IS (BitmapState)
        DstObj = SrcObj
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('BitmapState_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE BitmapState_Copy

!******************************************************************************

FUNCTION BitmapState_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), INTENT(IN)  :: LhsObj   !! an object
    CLASS(Object),      INTENT(IN)  :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (BitmapState)
        ! this is not correct implementation but it does not matter since we do not use this any way
        Flag = (LhsObj%Writable .NEQV. RhsObj%Writable)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION BitmapState_IsEqualTo

!******************************************************************************

SUBROUTINE BitmapState_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of the BitmapState object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), INTENT(INOUT)   :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! this is not correct implementation but it does not matter since we do not use this any way
    ASSOCIATE (Dummy => Obj); END ASSOCIATE

    RETURN

END SUBROUTINE BitmapState_MemFree

!******************************************************************************

FUNCTION BitmapState_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), INTENT(IN)  :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! this is not correct implementation but it does not matter since we do not use this any way
    Str = 'BitmapState'
    ASSOCIATE (Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION BitmapState_ToString

!-------------------------------------------------------------------------------
!                               BitmapState Procedures
!-------------------------------------------------------------------------------

SUBROUTINE BitmapState_Construct(State, ID, Manager)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the BitmapState object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState),               INTENT(OUT)   :: State
    tSInt32,                          INTENT(IN)    :: ID
    TYPE(BitmapStateManager), TARGET, INTENT(IN)    :: Manager

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    State%ID = ID
    CALL State%DirectTable%Construct(Manager)
    State%Manager => Manager
    CALL State%TransitionMap%Construct()

    RETURN

END SUBROUTINE BitmapState_Construct

!******************************************************************************

SUBROUTINE BitmapState_Destruct(State)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct the BitmapState object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), INTENT(INOUT)   :: State

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL State%DirectTable%Destruct()
    NULLIFY(State%Manager)
    CALL State%TransitionMap%Clear()

    RETURN

END SUBROUTINE BitmapState_Destruct

!******************************************************************************

SUBROUTINE BitmapState_TransitionRule(State, Chr, TargetID)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform transition rule.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), INTENT(INOUT)   :: State
    tChar,              INTENT(IN)      :: Chr
    tSInt32,            INTENT(IN)      :: TargetID

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(HashListNode),   POINTER  :: StoredNode
    TYPE(BitMapNode)                :: KeyNode
    TYPE(BitMapNode),      POINTER  :: CurNode
    TYPE(BitmapStatePack), POINTER  :: Pack
    tLogical                        :: Found

! FLOW
    
    CALL State%EnsureWritable()
    KeyNode%Chr = Chr
    Found = State%TransitionMap%FindNode(KeyNode, StoredNode)
    IF (Found.AND.ASSOCIATED(StoredNode)) THEN
        SELECT TYPE (StoredNode)
        TYPE IS (BitMapNode)
            Pack => StoredNode%State
        END SELECT
    ELSE
        CALL BitMapNode_New(CurNode, Chr, State%Manager)
        CALL State%TransitionMap%Insert(CurNode)
        Pack => CurNode%State
    END IF
    CALL Pack%AddState(TargetID)
    NULLIFY(StoredNode, CurNode, Pack)

    RETURN

END SUBROUTINE BitmapState_TransitionRule

!******************************************************************************

SUBROUTINE BitmapState_DirectRule(State, TargetID)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform direct rule.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), INTENT(INOUT)   :: State
    tSInt32,            INTENT(IN)      :: TargetID

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL State%EnsureWritable()
    CALL State%DirectTable%AddState(TargetID)

    RETURN

END SUBROUTINE BitmapState_DirectRule

!******************************************************************************

FUNCTION BitmapState_GetID(State) RESULT(ID)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get ID of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), INTENT(IN)  :: State    !! object
    tSInt32                         :: ID

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ID = State%ID

    RETURN

END FUNCTION BitmapState_GetID

!******************************************************************************

FUNCTION BitmapState_GetDirectTable(State) RESULT(Table)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the direct table of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), TARGET, INTENT(IN) :: State    !! object
    TYPE(BitmapStatePack),      POINTER    :: Table

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Table => State%DirectTable

    RETURN

END FUNCTION BitmapState_GetDirectTable

!******************************************************************************

FUNCTION BitmapState_GetTransitionMap(State) RESULT(TransMap)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the transition map of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), TARGET, INTENT(IN) :: State    !! object
    TYPE(IntrusiveHashList),    POINTER    :: TransMap

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    TransMap => State%TransitionMap

    RETURN

END FUNCTION BitmapState_GetTransitionMap

!******************************************************************************

SUBROUTINE BitmapState_Freeze(State)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform freezing operation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), INTENT(INOUT)   :: State

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(HashListNode), POINTER    :: CurEntry
    tLogical                        :: EndOfList

! FLOW
    
    CALL State%DirectTable%Freeze()
    EndOfList = State%TransitionMap%StartFirst(CurEntry)
    DO WHILE (.NOT.EndOfList)
        SELECT TYPE (MapEntry => CurEntry)
        TYPE IS (BitMapNode)
            CALL MapEntry%State%Freeze()
        END SELECT
        EndOfList = State%TransitionMap%MoveForward(CurEntry)
    END DO
    State%Writable = FalseVal

    RETURN

END SUBROUTINE BitmapState_Freeze

!******************************************************************************

SUBROUTINE BitmapState_EnsureWritable(State)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To report error if wriable is false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), INTENT(INOUT)   :: State

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (.NOT.State%Writable) THEN
        CALL Handle_ErrLevel('BitmapState_EnsureWritable', ModName, ErrWarning, &
                             'NFAIState is not writable')
    END IF

    RETURN

END SUBROUTINE BitmapState_EnsureWritable

!******************************************************************************

FUNCTION BitmapState_IsEqual(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the two specified objects are equal to one another or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), INTENT(IN)  :: LhsObj   !! an object
    TYPE(BitmapState),  INTENT(IN)  :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if keys of both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (LhsObj%ID == RhsObj%ID)

    RETURN

END FUNCTION BitmapState_IsEqual

!******************************************************************************

FUNCTION BitmapState_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code of the specified object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapState), INTENT(IN)  :: Obj  !! object
    tHash                           :: Code !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: KeySize

! FLOW
    
    KeySize = C_SIZEOF(Obj%ID)
    Code = HashFuncOpt(Obj%ID, KeySize, 3131133)

    RETURN

END FUNCTION BitmapState_HashCode

!-------------------------------------------------------------------------------
!                               BitmapStateManager Procedures
!-------------------------------------------------------------------------------

SUBROUTINE BitmapStateManager_Construct(Manager, StateList)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the BitmapStateManager object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStateManager), INTENT(OUT)      :: Manager
    TYPE(IntrusiveLinearList), INTENT(INOUT)    :: StateList

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: EndOfList

! FLOW

    ALLOCATE(Manager%IStates(0:StateList%GetSize()-1))
    BLOCK
        CLASS(DoublyLinkedNode), POINTER    :: CurNode
        tSInt32                             :: ID
        ! construct IStates
        EndOfList = StateList%StartFirst(CurNode)
        DO WHILE (.NOT.EndOfList)
            SELECT TYPE (CurState => CurNode)
            TYPE IS (NFAState)
                ID = CurState%GetID()
                CALL Manager%IStates(ID)%Construct(ID, Manager)
            END SELECT
            EndOfList = StateList%MoveForward(CurNode)
        END DO
        ! transform IStates
        EndOfList = StateList%StartFirst(CurNode)
        DO WHILE (.NOT.EndOfList)
            SELECT TYPE (CurState => CurNode)
            TYPE IS (NFAState)
                ID = CurState%GetID()
                ! loop through direct table
                CALL DirectTableLoop(CurState, Manager%IStates(ID))
                ! loop through transition map
                CALL TransitionMapLoop(CurState, Manager%IStates(ID))
            END SELECT
            EndOfList = StateList%MoveForward(CurNode)
        END DO
        ! freeze IStates
        DO ID = 0, SIZE(Manager%IStates)-1
            CALL Manager%IStates(ID)%Freeze()
        END DO
    END BLOCK
    
    RETURN

CONTAINS

    SUBROUTINE DirectTableLoop(CState, IState)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(NFAState),    INTENT(INOUT)  :: CState
        TYPE(BitmapState), INTENT(INOUT)  :: IState

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CLASS(HashListNode),     POINTER    :: ThisNode
        TYPE(IntrusiveHashList), POINTER    :: Table
        tLogical                            :: EndOfTable

    ! FLOW
    
        Table => CState%GetDirectTable()
        EndOfTable = Table%StartFirst(ThisNode)
        DO WHILE (.NOT.EndOfTable)
            SELECT TYPE (NState => ThisNode)
            TYPE IS (NFAState)
                CALL IState%DirectRule(NState%GetID())
            END SELECT
            EndOfTable = Table%MoveForward(ThisNode)
        END DO

        RETURN

    END SUBROUTINE DirectTableLoop

    !******************************************************************************

    SUBROUTINE TransitionMapLoop(CState, IState)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(NFAState),    INTENT(INOUT)  :: CState
        TYPE(BitmapState), INTENT(INOUT)  :: IState

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CLASS(HashListNode),     POINTER    :: ThisNode
        TYPE(IntrusiveHashList), POINTER    :: TransMap
        TYPE(IntrusiveHashList), POINTER    :: StateSet
        tChar                               :: Chr
        tLogical                            :: EndOfMap

    ! FLOW
    
        TransMap => CState%GetTransitionMap()
        EndOfMap = TransMap%StartFirst(ThisNode)
        DO WHILE (.NOT.EndOfMap)
            SELECT TYPE (ThisNode)
            TYPE IS (HashMapNode)
                StateSet => ThisNode%GetSet()
                Chr = ThisNode%GetChar()
            END SELECT
            BLOCK
                CLASS(HashListNode), POINTER    :: SetNode
                tLogical                        :: EndOfSet
                EndOfSet = StateSet%StartFirst(SetNode)
                DO WHILE (.NOT.EndOfSet)
                    SELECT TYPE (NState => SetNode)
                    TYPE IS (NFAState)
                        CALL IState%TransitionRule(Chr, NState%GetID())
                    END SELECT
                    EndOfSet = StateSet%MoveForward(SetNode)
                END DO
            END BLOCK
            EndOfMap = TransMap%MoveForward(ThisNode)
        END DO

        RETURN

    END SUBROUTINE TransitionMapLoop

    !******************************************************************************

END SUBROUTINE BitmapStateManager_Construct

!******************************************************************************

SUBROUTINE BitmapStateManager_Destruct(Manager)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct the BitmapStateManager object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStateManager), INTENT(INOUT)    :: Manager

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(Manager%IStates)) THEN
        BLOCK
            tSInt32     :: ID
            DO ID = 0, SIZE(Manager%IStates)-1
                CALL Manager%IStates(ID)%Destruct()
            END DO
        END BLOCK
        DEALLOCATE(Manager%IStates)
    END IF
    
    RETURN

END SUBROUTINE BitmapStateManager_Destruct

!******************************************************************************

FUNCTION BitmapStateManager_GetState(Manager, ID) RESULT(State)

!** PURPOSE OF THIS FUNCTION:
    !^ To get a pointer to the specified state of the BitmapStateManager object
    !  according to the specified ID.

!** FUNCTION ARGUMENT DECLARATIONS:
    CLASS(BitmapStateManager), TARGET, INTENT(IN)   :: Manager
    tSInt32,                           INTENT(IN)   :: ID
    TYPE(BitmapState),         POINTER              :: State

!** FUNCTION INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    State => Manager%IStates(ID)

    RETURN

END FUNCTION BitmapStateManager_GetState

!******************************************************************************

FUNCTION BitmapStateManager_GetStates(Manager) RESULT(States)

!** PURPOSE OF THIS FUNCTION:
    !^ To get a pointer to states of the BitmapStateManager object.

!** FUNCTION ARGUMENT DECLARATIONS:
    CLASS(BitmapStateManager), TARGET, INTENT(IN)   :: Manager
    TYPE(BitmapState),         POINTER              :: States(:)

!** FUNCTION INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    States => Manager%IStates

    RETURN

END FUNCTION BitmapStateManager_GetStates

!******************************************************************************

FUNCTION BitmapStateManager_StateCount(Manager) RESULT(Count)

!** PURPOSE OF THIS FUNCTION:
    !^ To get size of the states of the object.

!** FUNCTION ARGUMENT DECLARATIONS:
    CLASS(BitmapStateManager), INTENT(IN)   :: Manager
    tSInt32                                 :: Count

!** FUNCTION INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Count = SIZE(Manager%IStates)

    RETURN

END FUNCTION BitmapStateManager_StateCount

!******************************************************************************

SUBROUTINE BitmapStateManager_NewEmptyPack(Manager, Pack)

!** PURPOSE OF THIS FUNCTION:
    !^ To get size of the states of the object.

!** FUNCTION ARGUMENT DECLARATIONS:
    CLASS(BitmapStateManager),      INTENT(IN)      :: Manager
    TYPE(BitmapStatePack), POINTER, INTENT(INOUT)   :: Pack

!** FUNCTION INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL BitmapStatePack_New(Pack, Manager)

    RETURN

END SUBROUTINE BitmapStateManager_NewEmptyPack

! -----------------------------------------------------------------------------
! -----                     BitmapStatePack Procedures                         -----
! -----------------------------------------------------------------------------

SUBROUTINE BitmapStatePack_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the BitmapStatePack object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack),INTENT(IN)   :: SrcObj   !! source object
    CLASS(Object),         INTENT(OUT)  :: DstObj   !! destination object
    tLogical, OPTIONAL,    INTENT(IN)   :: IsDeep
    !^ flag indicating whether to perform deep copy or shallow copy; <br>
    !  - if true, perform shallow copy; <br>
    !  - if false, perform deep copy; <br>
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE(DstObj)
    TYPE IS (BitmapStatePack)
        DstObj = SrcObj
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('BitmapStatePack_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE BitmapStatePack_Copy

!******************************************************************************

FUNCTION BitmapStatePack_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), INTENT(IN)  :: LhsObj   !! an object
    CLASS(Object),          INTENT(IN)  :: RhsObj   !! another object
    tLogical                            :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (BitmapStatePack)
        ! this is not correct implementation but it does not matter since we do not use this any way
        Flag = (LhsObj%Writable .NEQV. RhsObj%Writable)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION BitmapStatePack_IsEqualTo

!******************************************************************************

SUBROUTINE BitmapStatePack_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of the BitmapStatePack object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), INTENT(INOUT)   :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! this is not correct implementation but it does not matter since we do not use this any way
    ASSOCIATE (Dummy => Obj); END ASSOCIATE

    RETURN

END SUBROUTINE BitmapStatePack_MemFree

!******************************************************************************

FUNCTION BitmapStatePack_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), INTENT(IN)  :: Obj
    tCharAlloc                          :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! this is not correct implementation but it does not matter since we do not use this any way
    Str = 'BitmapStatePack'
    ASSOCIATE (Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION BitmapStatePack_ToString

!-------------------------------------------------------------------------------
!                               BitmapStatePack Procedures
!-------------------------------------------------------------------------------

SUBROUTINE BitmapStatePack_Construct(Pack, Manager)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the BitmapState object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack),           INTENT(OUT)   :: Pack
    TYPE(BitmapStateManager), TARGET, INTENT(IN)    :: Manager

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length

! FLOW

    Length = (Manager%StateCount() - 1_kIndex) / 63_kIndex + 1_kIndex     ! ceiling
    CALL MemAlloc(Pack%StateBitmap, Length, StartID=0_kIndex)
    Pack%Manager => Manager
    
    RETURN

END SUBROUTINE BitmapStatePack_Construct

!******************************************************************************

SUBROUTINE BitmapStatePack_Destruct(Pack)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct the BitmapState object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), INTENT(INOUT)   :: Pack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemFree(Pack%StateBitmap)
    NULLIFY(Pack%Manager)
    CALL Pack%List%Clear()
    
    RETURN

END SUBROUTINE BitmapStatePack_Destruct

!******************************************************************************

SUBROUTINE BitmapStatePack_AddState(Pack, StateID)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add state to the pack object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), INTENT(INOUT)   :: Pack
    tSInt32,                INTENT(IN)      :: StateID

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Bucket, Offset
    tSInt64     :: F

! FLOW

    CALL Pack%EnsureWritable()
    Bucket = StateID / 63
    Offset = MOD(StateID, 63)
    F = SHIFTL(1_kInt64, Offset)
    Pack%StateBitmap(Bucket) = IOR(Pack%StateBitmap(Bucket), F)

    RETURN

END SUBROUTINE BitmapStatePack_AddState

!******************************************************************************

SUBROUTINE BitmapStatePack_AddAll(Pack, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add state to the pack object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), INTENT(INOUT)   :: Pack
    TYPE(BitmapStatePack),  INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW

    CALL Pack%EnsureWritable()
    IF (.NOT.ASSOCIATED(Pack%Manager, Other%Manager)) THEN
        CALL Handle_ErrLevel('BitmapStatePack_AddAll', ModName, ErrSevere, &
                             'Both objects must share the same manager.')
        RETURN
    END IF
    DO I = 0, SIZE(Pack%StateBitmap)-1
        Pack%StateBitmap(I) = IOR(Pack%StateBitmap(I), Other%StateBitmap(I))
    END DO

    RETURN

END SUBROUTINE BitmapStatePack_AddAll

!******************************************************************************

FUNCTION BitmapStatePack_Contain(Pack, StateID) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the pack object contains the specified ID or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), INTENT(IN)  :: Pack
    tSInt32,                INTENT(IN)  :: StateID
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Bucket, Offset
    tSInt64     :: F

! FLOW

    Bucket = StateID / 63
    Offset = MOD(StateID, 63)
    F = SHIFTL(1_kInt64, Offset)
    Flag = (IOR(Pack%StateBitmap(Bucket), F) /= 0_kInt64)

    RETURN

END FUNCTION BitmapStatePack_Contain

!******************************************************************************

FUNCTION BitmapStatePack_IsEmpty(Pack) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the pack object is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), INTENT(IN)  :: Pack
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW

    Flag = FalseVal
    ASSOCIATE (B => Pack%StateBitmap)
        DO I = 0, SIZE(B)-1
            IF (B(I) /= 0_kInt64) RETURN
        END DO
    END ASSOCIATE
    Flag = TrueVal

    RETURN

END FUNCTION BitmapStatePack_IsEmpty

!******************************************************************************

FUNCTION BitmapStatePack_AsList(Pack) RESULT(List)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the pack object as a list of states.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), TARGET, INTENT(INOUT)   :: Pack
    TYPE(IntrusiveLinearList),      POINTER         :: List

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Pack%Writable) THEN
        CALL Handle_ErrLevel('BitmapStatePack_AsList', ModName, ErrWarning, &
                             'Pack is still writable. Call AsList only when writing is finished.')
        RETURN
    END IF
    IF (Pack%List%IsEmpty()) THEN
        ! add contained states to the list
        BLOCK
            tSInt32     :: ID
            DO ID = 0, Pack%Manager%StateCount()-1
                IF (Pack%Contain(ID)) THEN
                    CALL Pack%List%AddLast(Pack%Manager%GetState(ID))
                END IF
            END DO
        END BLOCK
    END IF
    List => Pack%List

    RETURN

END FUNCTION BitmapStatePack_AsList

!******************************************************************************

FUNCTION BitmapStatePack_Size(Pack) RESULT(StateSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get size of states of the pack object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), INTENT(IN)  :: Pack
    tSInt32                             :: StateSize

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I
    tSInt64     :: B, S

! FLOW

    S = 0_kInt64
    ASSOCIATE (BState => Pack%StateBitmap)
        DO I = 0, SIZE(BState)-1
            B = BState(I)
            DO WHILE (B /= 0_kInt64)
                S = S + IAND(B, 1_kInt64)
                B = SHIFTA(B, 1)
            END DO
        END DO
    END ASSOCIATE
    StateSize = ToInt32(S)

    RETURN

END FUNCTION BitmapStatePack_Size

!******************************************************************************

SUBROUTINE BitmapStatePack_Freeze(Pack)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform freezing operation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), INTENT(INOUT)   :: Pack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Pack%Writable = FalseVal

    RETURN

END SUBROUTINE BitmapStatePack_Freeze

!******************************************************************************

SUBROUTINE BitmapStatePack_EnsureWritable(Pack)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To report error if wriable is false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), INTENT(INOUT)   :: Pack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (.NOT.Pack%Writable) THEN
        CALL Handle_ErrLevel('BitmapStatePack_EnsureWritable', ModName, ErrWarning, &
                             'NFAIState is not writable')
    END IF

    RETURN

END SUBROUTINE BitmapStatePack_EnsureWritable

!******************************************************************************

FUNCTION BitmapStatePack_IsEqual(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the two specified objects are equal to one another or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), INTENT(IN)  :: LhsObj   !! an object
    TYPE(BitmapStatePack),  INTENT(IN)  :: RhsObj   !! another object
    tLogical                            :: Flag     !! true if keys of both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = FalseVal
    IF (SIZE(LhsObj%StateBitmap) /= SIZE(RhsObj%StateBitmap)) RETURN
    BLOCK
        tIndex      :: I
        DO I = 0, SIZE(LhsObj%StateBitmap)-1
            IF (LhsObj%StateBitmap(I) /= RhsObj%StateBitmap(I)) RETURN
        END DO
    END BLOCK
    Flag = TrueVal

    RETURN

END FUNCTION BitmapStatePack_IsEqual

!******************************************************************************

FUNCTION BitmapStatePack_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code of the specified object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapStatePack), INTENT(IN)  :: Obj  !! object
    tHash                               :: Code !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: KeySize

! FLOW
    
    KeySize = C_SIZEOF(Obj%StateBitmap(1))*SIZE(Obj%StateBitmap)
    Code = HashFuncOpt(Obj%StateBitmap, KeySize, 3131133)

    RETURN

END FUNCTION BitmapStatePack_HashCode

! -----------------------------------------------------------------------------
! -----                     BitmapNode Procedures                         -----
! -----------------------------------------------------------------------------

SUBROUTINE BitmapNode_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the BitmapNode object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapNode),  INTENT(IN)   :: SrcObj   !! source object
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
    TYPE IS (BitmapNode)
        DstObj = SrcObj
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('BitmapNode_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE BitmapNode_Copy

!******************************************************************************

FUNCTION BitmapNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapNode), INTENT(IN)   :: LhsObj   !! an object
    CLASS(Object),     INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (BitmapNode)
        ! this is not correct implementation but it does not matter since we do not use this any way
        Flag = (LhsObj%Chr /= RhsObj%Chr)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION BitmapNode_IsEqualTo

!******************************************************************************

FUNCTION BitmapNode_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitmapNode), INTENT(IN)   :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! this is not correct implementation but it does not matter since we do not use this any way
    Str = 'BitmapNode'
    ASSOCIATE (Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION BitmapNode_ToString

!-------------------------------------------------------------------------------
!                               BitMapNode Procedures
!-------------------------------------------------------------------------------

FUNCTION BitMapNode_IsKeyEqual(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether *key* components of the two specified objects are equal to one another or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitMapNode),   INTENT(INOUT)  :: LhsObj   !! an object
    CLASS(HashListNode), INTENT(INOUT)  :: RhsObj   !! another object
    tLogical                            :: Flag     !! true if keys of both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    TYPE IS (BitMapNode)
        Flag = (LhsObj%Chr == RhsObj%Chr)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION BitMapNode_IsKeyEqual

!******************************************************************************

SUBROUTINE BitMapNode_FreeMemory(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free storage/memory of an object with pointer and/or allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitMapNode), INTENT(INOUT)    :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL Obj%State%Destruct()

    RETURN

END SUBROUTINE BitMapNode_FreeMemory

!******************************************************************************

FUNCTION BitMapNode_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code of the *key* component of the specified object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitMapNode), INTENT(IN)   :: Obj  !! object
    tHash                           :: Code !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: KeySize

! FLOW
    
    KeySize = C_SIZEOF(Obj%Chr)
    Code = HashFuncOpt(Obj%Chr, KeySize, 3131133)

    RETURN

END FUNCTION BitMapNode_HashCode

!******************************************************************************

SUBROUTINE BitMapNode_Construct(Node, Chr, Manager, State)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the BitMapNode object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BitMapNode),                       INTENT(OUT)    :: Node
    tChar,                                   INTENT(IN)     :: Chr
    TYPE(BitmapStateManager),                INTENT(IN)     :: Manager
    TYPE(BitmapStatePack), TARGET, OPTIONAL, INTENT(IN)     :: State

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (PRESENT(State)) THEN
        Node%State => State
        CALL Node%State%Construct(Manager)
    ELSE
        CALL Manager%NewEmptyPack(Node%State)
    END IF
    Node%Chr = Chr

    RETURN

END SUBROUTINE BitMapNode_Construct

!******************************************************************************

END MODULE MClass_BitmapState
    
!******************************************************************************
