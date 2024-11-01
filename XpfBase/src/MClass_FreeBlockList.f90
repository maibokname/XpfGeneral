
MODULE MClass_FreeBlockList

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *FreeBlockList* type, the *FreeBlockNode* type and its related
!   routines.  The *FreeBlockList* type is a linked-list type that can be used to keep track
!   of free memory blocks in a memory pool while the *FreeBlockNode* type is a linked-node
!   type to be used in conjunction with the *FreeBlockList* type.

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_MemHandlers
    USE MBase_SIntUtil,                 ONLY: ToChar => ToDecStrSigned
    USE MBase_ByteUtil,                 ONLY: AnyType_GetByteSize
#ifdef Indx32Bits
    USE MBase_SimpleHash32,             ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64,             ONLY: ComputeHash => Hash64_FNV1a
#endif
    USE MClass_Object,                  ONLY: Object
    USE MClass_BaseNodePool
    USE MClass_IntrusiveLinkedLists,    ONLY: DoublyLinkedNode, IntrusiveLinearList
    USE MClass_CharBuffer

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: FreeBlockList
    PUBLIC :: FreeBlockNode

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_FreeBlockList'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! initial block size
    tIndex,    PARAMETER    :: InitBlockSize = 1024_kIndex

!** DERIVED TYPE DEFINITIONS
    !> The *FreeBlockNode* type is a linked-node type to be used in conjunction with
    !  the *FreeBlockList* type.
    TYPE, EXTENDS(DoublyLinkedNode) :: FreeBlockNode
        PRIVATE
        !> position in the memory pool
        tIndex                      :: Position  = 0_kIndex
        !> block size in bytes
        tIndex                      :: BlockSize = 0_kIndex
        !> pointer to the node's pool
        TYPE(BaseNodePool), POINTER :: Pool => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: NextNode <br>
        !  **Purpose**:  To get a pointer to the next node of the current node. <br>
        !  **Usage**: <br>
        !   --->    NextNode => CurrNode%NextNode()
        PROCEDURE, PRIVATE  :: NextNode     => FreeBlockNode_GetNextNode
        !> **Type-Bound Function**: PrevNode <br>
        !  **Purpose**:  To get a pointer to the previous node of the current node. <br>
        !  **Usage**: <br>
        !   --->    PrevNode => CurrNode%PrevNode()
        PROCEDURE, PRIVATE  :: PrevNode     => FreeBlockNode_GetPreviousNode
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: SetNew <br>
        !  **Purpose**:  To construct a new node based on the optionally specified input. <br>
        !  **Usage**: <br>
        !   --->    CALL CurrNode%SetNew() <br>
        !   --->    CALL CurrNode%SetNew(BlockSize) <br>
        !   --->    CALL CurrNode%SetNew(Position=Pos) <br>
        !   --->    CALL CurrNode%SetNew(BckSize, Pos) <br>
        PROCEDURE   :: SetNew       => FreeBlockNode_SetNew
        !> **Type-Bound Function**: GetBlockSize <br>
        !  **Purpose**:  To get the block size of the current node. <br>
        !  **Usage**: <br>
        !   --->    BlockSize => CurrNode%GetBlockSize()
        PROCEDURE   :: GetBlockSize => FreeBlockNode_GetBlockSize
        !> **Type-Bound Function**: GetPosition <br>
        !  **Purpose**:  To get the position of the current node. <br>
        !  **Usage**: <br>
        !   --->    Position => CurrNode%GetPosition()
        PROCEDURE   :: GetPosition  => FreeBlockNode_GetPosition
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => FreeBlockNode_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => FreeBlockNode_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => FreeBlockNode_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => FreeBlockNode_ToString
        ! ---------------------------------------------------------------------
        ! -----                     Overridden Procedure                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => FreeBlockNode_HashCode
        !----------------------------------------------------------------------
    END TYPE FreeBlockNode
    !> The *FreeBlockList* type is a linked-list type that can be used to keep track
    !  of free memory blocks in a memory pool.
    TYPE FreeBlockList
        PRIVATE
        !> working list
        TYPE(IntrusiveLinearList)       :: WrkList
        !> pool of linked nodes 
        TYPE(BaseNodePool)              :: Pool
        !> a pointer to tail node used as a guard node
        TYPE(FreeBlockNode), POINTER    :: Tail
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: BestFit <br>
        !  **Purpose**:  To use the best fit method to find where to insert the data. <br>
        !  **Usage**: <br>
        !   --->    BestNode => List%BestFit(DataSize) <br>
        PROCEDURE, PRIVATE  :: BestFit      => FreeBlockList_BestFit
        !> **Type-Bound Subroutine**: Remove <br>
        !  **Purpose**:  To remove the specified node from the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Remove(DelNode) <br>
        PROCEDURE, PRIVATE  :: Remove       => FreeBlockList_Remove
        !> **Type-Bound Subroutine**: Merge <br>
        !  **Purpose**:  To merge the specified node. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Merge(Node) <br>
        PROCEDURE, PRIVATE  :: Merge        => FreeBlockList_Merge
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new list based on the specified input. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Construct(BlockSize) <br>
        PROCEDURE   :: Construct    => FreeBlockList_Construct
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To destruct the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Destruct()
        PROCEDURE   :: Destruct     => FreeBlockList_Destruct
        !> **Type-Bound Subroutine**: CloneTo <br>
        !  **Purpose**:  To perform cloning of the source list. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcList%CloneTo(DstList) <br>
        PROCEDURE   :: CloneTo      => FreeBlockList_Clone
        !> **Type-Bound Function**: GetNewNode <br>
        !  **Purpose**:  To get new node from the list's pool of nodes. <br>
        !  **Usage**: <br>
        !   --->    NewNode => List%GetNewNode() <br>
        PROCEDURE   :: GetNewNode   => FreeBlockList_GetNewNode
        !> **Type-Bound Function**: FindPosition <br>
        !  **Purpose**:  To find the position to insert the data. <br>
        !  **Usage**: <br>
        !   --->    Position = List%FindPosition(DataSize) <br>
        PROCEDURE   :: FindPosition => FreeBlockList_FindPosition
        !> **Type-Bound Function**: Update <br>
        !  **Purpose**:  To update the list.  When a best fit block is found, this block need to change
        !                the position and size info.  If best is not found, return position = 0. <br>
        !  **Usage**: <br>
        !   --->    Position = List%Update(DataSize) <br>
        PROCEDURE   :: Update       => FreeBlockList_Update
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert a free node, which has position and size reference. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Insert(NewNode) <br>
        PROCEDURE   :: Insert       => FreeBlockList_Insert
        ! ---------------------------------------------------------------------
        ! -----                     Final Procedure                       -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object
        FINAL               :: FreeBlockList_Finalize
        !----------------------------------------------------------------------
    END TYPE FreeBlockList

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!       Deferred/Overridden Procedures of the FreeBlockNode Type          -----
!------------------------------------------------------------------------------

SUBROUTINE FreeBlockNode_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the *FreeBlockNode* object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockNode), INTENT(IN)    :: SrcObj   !! source object
    CLASS(Object),        INTENT(OUT)   :: DstObj   !! destination object
    tLogical, OPTIONAL,   INTENT(IN)    :: IsDeep
    !^ Flag indicating whether to perform deep copy or shallow copy. <br>
    !  - If present and true, perform a deep copy. <br>
    !  - If present and false, perform a shallow copy. <br>
    !  - If not present, perform either a shallow or a deep copy that is naturally most suitable
    !    for the object's components.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE(DstObj)
    TYPE IS (FreeBlockNode)
        DstObj%Position  = SrcObj%Position
        DstObj%BlockSize = SrcObj%BlockSize
        BLOCK
            tLogical    :: DeepCopy
            DeepCopy = FalseVal
            IF (PRESENT(IsDeep)) DeepCopy = IsDeep
            IF (DeepCopy) THEN
                IF (ASSOCIATED(SrcObj%Pool)) CALL SrcObj%Pool%Copy(DstObj%Pool, IsDeep)
                IF (ASSOCIATED(DstObj%Pool)) THEN
                    ! get storages of nodes
                    BLOCK
                        CLASS(BaseNode),      POINTER   :: TmpNode
                        CLASS(FreeBlockNode), POINTER   :: SrcNode
                        CALL DstObj%Pool%GetNewNode(TmpNode)
                        SELECT TYPE (TmpNode)
                        TYPE IS (FreeBlockNode)
                            SrcNode => SrcObj%NextNode()
                            TmpNode = SrcNode
                            CALL DstObj%SetNext(TmpNode)
                        END SELECT
                        CALL DstObj%Pool%GetNewNode(TmpNode)
                        SELECT TYPE (TmpNode)
                        TYPE IS (FreeBlockNode)
                            SrcNode => SrcObj%PrevNode()
                            TmpNode = SrcNode
                            CALL DstObj%SetPrevious(TmpNode)
                        END SELECT
                        NULLIFY(TmpNode, SrcNode)
                    END BLOCK
                ELSE
                    CALL Handle_ErrLevel('FreeBlockNode_Copy', ModName, ErrWarning, &
                        'The "BaseNodePool" is NOT available for deep copy so perform shallow copy instead.')
                    CALL DstObj%SetNext(SrcObj%GetNext())
                    CALL DstObj%SetPrevious(SrcObj%GetPrevious())
                END IF
            ELSE
                DstObj%Pool => SrcObj%Pool
                CALL SrcObj%CopyDLLNode(DstObj)
            END IF
        END BLOCK
    CLASS DEFAULT
        CALL Handle_ErrLevel('FreeBlockNode_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE FreeBlockNode_Copy

!******************************************************************************

FUNCTION FreeBlockNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockNode), INTENT(IN)    :: LhsObj   !! an object
    CLASS(Object),        INTENT(IN)    :: RhsObj   !! another object
    tLogical                            :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (FreeBlockNode)
        Flag = FalseVal
        ! check equalities of position and block size
        IF (LhsObj%Position /= RhsObj%Position) RETURN
        IF (LhsObj%BlockSize /= RhsObj%BlockSize) RETURN
        ! check equalities of next and previous nodes
        IF (.NOT.LhsObj%IsDLLNodeEqual(RhsObj)) RETURN
        Flag = TrueVal
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION FreeBlockNode_IsEqualTo

!******************************************************************************

SUBROUTINE FreeBlockNode_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of the *FreeBlockNode* object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockNode), INTENT(INOUT) :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%FreePointers()
    NULLIFY(Obj%Pool)

    RETURN

END SUBROUTINE FreeBlockNode_MemFree

!******************************************************************************

FUNCTION FreeBlockNode_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockNode), INTENT(IN)    :: Obj
    tCharAlloc                          :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc          :: BaseStr
    TYPE(CharBuffer)    :: ChrBuf
    tCharAlloc          :: ItemStr

! FLOW

    CALL ChrBuf%CreateEmpty(InitCap=256_kIndex)
    CALL ChrBuf%Append('[')
    ItemStr = '{Position : ' // ToChar(Obj%Position) // '}, '
    CALL ChrBuf%Append(ItemStr)
    ItemStr = '{BlockSize : ' // ToChar(Obj%BlockSize) // '}, '
    CALL ChrBuf%Append(ItemStr)
    IF (ASSOCIATED(Obj%GetNext())) THEN
        ItemStr = '{NextNode : NONNULL}, '
    ELSE
        ItemStr = '{NextNode : NULL}, '
    END IF
    CALL ChrBuf%Append(ItemStr)
    IF (ASSOCIATED(Obj%GetPrevious())) THEN
        ItemStr = '{PrevNode : NONNULL}'
    ELSE
        ItemStr = '{PrevNode : NULL}'
    END IF
    CALL ChrBuf%Append(ItemStr)
    CALL ChrBuf%Append(']')
    BaseStr = ChrBuf%AsString()
    Str = '{FreeBlockNode : ' // BaseStr // '}'

    RETURN

END FUNCTION FreeBlockNode_ToString

!******************************************************************************

FUNCTION FreeBlockNode_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.  This is an overridden procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockNode), INTENT(IN)    :: Obj
    tIndex                              :: Code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Code = ComputeHash(Obj%Position, AnyType_GetByteSize(Obj%Position)) + &
           ComputeHash(Obj%BlockSize, AnyType_GetByteSize(Obj%BlockSize))

    RETURN

END FUNCTION FreeBlockNode_HashCode

!------------------------------------------------------------------------------
!                           Routines for FreeBlockNode
!------------------------------------------------------------------------------

SUBROUTINE FreeBlockNode_SetNew(Node, BlockSize, Position, Pool)

!** PURPOSE OF THIS SUBROUTINE:
    !! To construct a new node based on the optionally specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockNode),                 INTENT(OUT)   :: Node         !! current node
    tIndex,             OPTIONAL,         INTENT(IN)    :: BlockSize    !! block size
    tIndex,             OPTIONAL,         INTENT(IN)    :: Position     !! position
    TYPE(BaseNodePool), OPTIONAL, TARGET, INTENT(IN)    :: Pool         !! node's pool

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (PRESENT(BlockSize)) THEN
        Node%BlockSize = BlockSize
    ELSE
        Node%BlockSize = 0_kIndex
    END IF
    IF (PRESENT(Position)) THEN
        Node%Position = Position
    ELSE
        Node%Position = 0_kIndex
    END IF
    IF (PRESENT(Pool)) THEN
        Node%Pool => Pool
    ELSE
        Node%Pool => NULL()
    END IF
    CALL Node%FreePointers()
       
    RETURN

END SUBROUTINE FreeBlockNode_SetNew

!******************************************************************************

FUNCTION FreeBlockNode_GetNextNode(CurrNode) RESULT(NextNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get the next node of the current node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockNode), INTENT(IN) :: CurrNode !! current node
    CLASS(FreeBlockNode), POINTER    :: NextNode !! next node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: NodeNext

! FLOW
    
    NodeNext => CurrNode%GetNext()
    SELECT TYPE (NodeNext)
    TYPE IS (FreeBlockNode)
        NextNode => NodeNext
    END SELECT
    NULLIFY(NodeNext)

    RETURN

END FUNCTION FreeBlockNode_GetNextNode

!******************************************************************************

FUNCTION FreeBlockNode_GetPreviousNode(CurrNode) RESULT(PrevNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get the previous node of the current node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockNode), INTENT(IN) :: CurrNode !! current node
    CLASS(FreeBlockNode), POINTER    :: PrevNode !! previous node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: NodePrev

! FLOW
    
    NodePrev => CurrNode%GetPrevious()
    SELECT TYPE (NodePrev)
    TYPE IS (FreeBlockNode)
        PrevNode => NodePrev
    END SELECT
    NULLIFY(NodePrev)

    RETURN

END FUNCTION FreeBlockNode_GetPreviousNode

!******************************************************************************

FUNCTION FreeBlockNode_GetPosition(Node) RESULT(Position)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get the block size.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockNode), INTENT(IN)    :: Node     !! node
    tIndex                              :: Position !! block size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Position = Node%Position
       
    RETURN

END FUNCTION FreeBlockNode_GetPosition

!******************************************************************************

FUNCTION FreeBlockNode_GetBlockSize(Node) RESULT(BlockSize)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get the block size.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockNode), INTENT(IN)    :: Node         !! node
    tIndex                              :: BlockSize    !! block size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    BlockSize = Node%BlockSize
       
    RETURN

END FUNCTION FreeBlockNode_GetBlockSize

!------------------------------------------------------------------------------
!                           Routines for FreeBlockList
!------------------------------------------------------------------------------

SUBROUTINE FreeBlockList_Construct(List, BlockSize)

!** PURPOSE OF THIS SUBROUTINE:
    !! To construct a new list based on the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockList), INTENT(OUT)   :: List         !! new list
    tIndex,               INTENT(IN)    :: BlockSize    !! block size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(FreeBlockNode), POINTER :: Head
    TYPE(FreeBlockNode), POINTER :: First

! FLOW

    ! explicitly construct the nodes' pool
    CALL List%Pool%Construct(First)

    ! get and set the head and tail nodes as guard nodes
    Head => List%GetNewNode()
    CALL Head%SetNew(Pool=List%Pool)
    List%Tail => List%GetNewNode()
    CALL List%Tail%SetNew(Pool=List%Pool)

    ! get and set the first node
    First => List%GetNewNode()
    CALL First%SetNew(BlockSize, 1_kIndex, List%Pool)
    
    ! insert the nodes into the working list
    CALL List%WrkList%AddLast(Head)
    CALL List%WrkList%AddLast(First)
    CALL List%WrkList%AddLast(List%Tail)

    ! free first node
    NULLIFY(Head, First)

    RETURN

END SUBROUTINE FreeBlockList_Construct

!******************************************************************************

FUNCTION FreeBlockList_GetNewNode(List) RESULT(NewNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get the new node from the list's pool.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockList), INTENT(INOUT) :: List     !! current list
    TYPE(FreeBlockNode),  POINTER       :: NewNode  !! new node from the list's pool

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BaseNode), POINTER    :: TmpNode

! FLOW

    CALL List%Pool%GetNewNode(TmpNode)
    SELECT TYPE (TmpNode)
    TYPE IS (FreeBlockNode)
        NewNode => TmpNode
    END SELECT
    NULLIFY(TmpNode)

    RETURN

END FUNCTION FreeBlockList_GetNewNode

!******************************************************************************

FUNCTION FreeBlockList_BestFit(List, DatSize) RESULT(BestNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To use the best fit method to find where to insert the data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockList), INTENT(OUT)   :: List     !! current list
    tIndex,               INTENT(IN)    :: DatSize  !! data size
    TYPE(FreeBlockNode),  POINTER       :: BestNode !! node where to insert the data

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex, PARAMETER   :: MaxIndx = HUGE(1_kIndex)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: TmpNode
    TYPE(FreeBlockNode),     POINTER    :: CurrNode
    tLogical                            :: EndOfList
    tIndex                              :: FitBlock

! FLOW

    ! initialize
    FitBlock = MaxIndx
    BestNode => NULL()
    ! return if the list not yet constructed
    IF (List%WrkList%IsEmpty()) RETURN
    ! start from the node next to the head
    EndOfList = List%WrkList%StartFirst(TmpNode)
    EndOfList = List%WrkList%MoveForward(TmpNode)
    DO WHILE (.NOT.EndOfList)
        ! get current node as a free-block node
        SELECT TYPE (TmpNode)
        TYPE IS (FreeBlockNode)
            CurrNode => TmpNode
        END SELECT
        ! if the end of the list is reached, exit
        IF (ASSOCIATED(CurrNode, List%Tail)) EXIT
        ! check if the current block is the best one
        IF ((DatSize <= CurrNode%BlockSize).AND.(CurrNode%BlockSize < FitBlock)) THEN
            BestNode => CurrNode
            FitBlock = BestNode%BlockSize
        END IF
        EndOfList = List%WrkList%MoveForward(TmpNode)
    END DO
    NULLIFY(CurrNode)

    RETURN

END FUNCTION FreeBlockList_BestFit

!******************************************************************************

FUNCTION FreeBlockList_FindPosition(List, DatSize) RESULT(Position)

!** PURPOSE OF THIS SUBROUTINE:
    !! To find the position to insert the data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockList), INTENT(OUT)   :: List     !! current list
    tIndex,               INTENT(IN)    :: DatSize  !! data size
    tIndex                              :: Position !! position

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(FreeBlockNode), POINTER    :: BestNode

! FLOW

    BestNode => List%BestFit(DatSize)
    IF (ASSOCIATED(BestNode)) THEN
        Position = BestNode%Position
    ELSE
        Position = 0_kIndex
    END IF

    RETURN

END FUNCTION FreeBlockList_FindPosition

!******************************************************************************

FUNCTION FreeBlockList_Update(List, DatSize) RESULT(Position)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To update the list.  When a best fit block is found, this block need to change the position
    !  and size info.  If best is not found, return position = 0.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockList), INTENT(OUT)   :: List     !! current list
    tIndex,               INTENT(IN)    :: DatSize  !! data size
    tIndex                              :: Position !! position of the best fit block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(FreeBlockNode), POINTER    :: BestNode

! FLOW

    BestNode => List%BestFit(DatSize)
    IF (ASSOCIATED(BestNode)) THEN
        IF (DatSize == BestNode%BlockSize) THEN
            CALL List%Remove(BestNode)
        ELSE
            ! update the blockSize and position
            BestNode%BlockSize = BestNode%BlockSize - DatSize
            BestNode%Position  = BestNode%Position  + DatSize
        END IF
        Position = BestNode%Position
    ELSE
        Position = 0_kIndex
    END IF

    RETURN

END FUNCTION FreeBlockList_Update

!******************************************************************************

SUBROUTINE FreeBlockList_Remove(List, DelNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To remove the specified node from the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockList), INTENT(INOUT) :: List     !! current list
    TYPE(FreeBlockNode),  INTENT(INOUT) :: DelNode  !! node to be removed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! remove the node from the working list
    IF (.NOT.List%WrkList%RemoveNode(DelNode)) THEN
        CALL Handle_ErrLevel('FreeBlockList_Remove', ModName, ErrSevere, &
                'Unable to remove the specified node. Either the list is empty or the node is not in the list.')
        RETURN
    END IF

    ! return the unused node to the nodes' pool
    CALL List%Pool%ReturnNode(DelNode)

    RETURN

END SUBROUTINE FreeBlockList_Remove

!******************************************************************************

SUBROUTINE FreeBlockList_Insert(List, NewNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To insert a free node, which has position and size reference.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockList),        INTENT(INOUT)  :: List     !! current list
    TYPE(FreeBlockNode), TARGET, INTENT(INOUT)  :: NewNode  !! node to be inserted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    TYPE(FreeBlockNode),     POINTER    :: NextNode
    tLogical                            :: EndOfList
    tLogical                            :: Success
    tLogical                            :: Append

! FLOW

    ! check whether the list has yet been constructed or not
    IF (List%WrkList%IsEmpty()) CALL List%Construct(InitBlockSize)

    ! initialize
    Append = TrueVal

    ! check where to insert the new node
    EndOfList = List%WrkList%StartFirst(CurrNode)
    DO WHILE (.NOT.EndOfList)
        ! get next node
        SELECT TYPE (CurrNode)
        TYPE IS (FreeBlockNode)
            NextNode => CurrNode%NextNode()
        END SELECT
        ! if the end of the list is reached, exit
        IF (ASSOCIATED(NextNode, List%Tail)) EXIT
        ! if the place to insert is found, also exit
        ! (between the current node and the next node)
        IF (NextNode%Position > NewNode%Position) THEN
            Append = FalseVal
            EXIT
        END IF
        EndOfList = List%WrkList%MoveForward(CurrNode)
    END DO

    IF (Append) THEN
        ! insert before the tail node
        Success = List%WrkList%AddBefore(NewNode, List%Tail)
    ELSE
        ! insert before the next node
        Success = List%WrkList%AddBefore(NewNode, NextNode) 
    END IF

    ! check for any error
    IF (.NOT.Success) THEN
        CALL Handle_ErrLevel('FreeBlockList_Insert', ModName, ErrSevere, &
                'Unable to insert the specified node.  Check for a bug?')
        NULLIFY(CurrNode, NextNode)
        RETURN
    END IF

    ! merge the specified node if needed
    IF (IsMergeNeeded(NewNode)) CALL List%Merge(NewNode)

    ! free working pointers
    NULLIFY(CurrNode, NextNode)

    RETURN

END SUBROUTINE FreeBlockList_Insert

!******************************************************************************

SUBROUTINE FreeBlockList_Merge(List, Node)

!** PURPOSE OF THIS SUBROUTINE:
    !! To merge the specified node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockList), INTENT(INOUT)  :: List     !! current list
    TYPE(FreeBlockNode),  INTENT(INOUT)  :: Node     !! node to be merged

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(FreeBlockNode), POINTER    :: NextNode
    TYPE(FreeBlockNode), POINTER    :: PrevNode

! FLOW

    NextNode => Node%NextNode()
    PrevNode => Node%PrevNode()

    IF ((Node%Position == (PrevNode%Position + PrevNode%BlockSize)).AND. &
        (NextNode%Position /= (Node%Position + Node%BlockSize))) THEN
        ! only merge left
        PrevNode%BlockSize = PrevNode%BlockSize + Node%BlockSize
        CALL List%Remove(Node)
    ELSEIF ((NextNode%Position == (Node%Position + Node%BlockSize)).AND. &
            (Node%Position /= (PrevNode%Position + PrevNode%BlockSize))) THEN
        ! only merge right
        Node%BlockSize = Node%BlockSize + NextNode%BlockSize
        CALL List%Remove(NextNode)
    ELSEIF ((NextNode%Position == (Node%Position + Node%BlockSize)).AND. &
            (Node%Position == (PrevNode%Position + PrevNode%BlockSize))) THEN
        ! merge both left and right
        PrevNode%BlockSize = PrevNode%BlockSize + Node%BlockSize + NextNode%BlockSize
        CALL List%Remove(NextNode)
        CALL List%Remove(Node)
    END IF

    NULLIFY(PrevNode, NextNode)

    RETURN

END SUBROUTINE FreeBlockList_Merge

!******************************************************************************

FUNCTION IsMergeNeeded(Node) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether a merging of the specified node is needed or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FreeBlockNode), INTENT(IN) :: Node
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(FreeBlockNode), POINTER    :: NextNode
    TYPE(FreeBlockNode), POINTER    :: PrevNode

! FLOW

    NextNode => Node%NextNode()
    PrevNode => Node%PrevNode()

    IF ((Node%Position /= (PrevNode%Position + PrevNode%BlockSize)).AND. &
        (NextNode%Position /= (Node%Position + Node%BlockSize))) THEN
        Flag = FalseVal
    ELSE
        Flag = TrueVal
    END IF

    NULLIFY(PrevNode, NextNode)

    RETURN

END FUNCTION IsMergeNeeded

!******************************************************************************

SUBROUTINE FreeBlockList_Destruct(List)

!** PURPOSE OF THIS SUBROUTINE:
    !! To destruct the free list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockList), INTENT(INOUT) :: List !! current list

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL List%WrkList%Clear()
    NULLIFY(List%Tail)
    CALL List%Pool%MemFree()

    RETURN

END SUBROUTINE FreeBlockList_Destruct

!******************************************************************************

SUBROUTINE FreeBlockList_Clone(SrcList, DstList)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform cloning of the source list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FreeBlockList), INTENT(IN)    :: SrcList  !! source
    TYPE(FreeBlockList),  INTENT(OUT)   :: DstList  !! destination

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! NA

! FLOW

    CALL SrcList%WrkList%CloneTo(DstList%WrkList)
    CALL SrcList%Pool%Copy(DstList%Pool)
    DstList%Tail => SrcList%Tail

    RETURN

END SUBROUTINE FreeBlockList_Clone

!******************************************************************************

SUBROUTINE FreeBlockList_Finalize(List)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FreeBlockList), INTENT(INOUT)  :: List !! current list

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL List%Destruct()

    RETURN

END SUBROUTINE FreeBlockList_Finalize

!******************************************************************************

END MODULE MClass_FreeBlockList

!******************************************************************************
