
MODULE MClass_SyntaxNode

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SyntaxNode* type and its related routines.
!   The *SyntaxNode* type is a derived type representing a ... <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_SIntUtil,                 ONLY: ToChar => ToDecStrSigned
    USE MClass_IntrusiveLinkedLists,    ONLY: DoublyLinkedNode
    USE MClass_Object,                  ONLY: Object

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: SyntaxNode
    PUBLIC :: SyntaxNode_New
    PUBLIC :: SyntaxNode_Free

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER            :: ModName = 'Class_SyntaxNode'
    tSInt32,   PARAMETER            :: MsgLen  = 128
    tSInt32,   PARAMETER, PUBLIC    :: Node_Undefined = 0
    tSInt32,   PARAMETER, PUBLIC    :: LeafNode_Null = 1
    tSInt32,   PARAMETER, PUBLIC    :: LeafNode_Char = 2
    tSInt32,   PARAMETER, PUBLIC    :: LeafNode_Closure = 3
    tSInt32,   PARAMETER, PUBLIC    :: BranchNode_Or = 4
    tSInt32,   PARAMETER, PUBLIC    :: BranchNode_Many = 5
    tSInt32,   PARAMETER, PUBLIC    :: BranchNode_Concat = 6
    tSInt32,   PARAMETER, PUBLIC    :: BranchNode_LBracket = 7
    tSInt32,   PARAMETER, PUBLIC    :: BranchNode_RBracket = 8
    tCharStar, PARAMETER            :: NULCHR = ACHAR(0)

!** DERIVED TYPE DEFINITIONS
    !> The *SyntaxNode* type is a node type...
    TYPE, EXTENDS(DoublyLinkedNode) :: SyntaxNode
        tSInt32     :: Type = Node_Undefined    !! node type
        tSInt32     :: ID   = 0                 !! node identification (only used for branch nodes)
        tChar       :: C    = NULCHR            !! character used only for LeafNode_Char
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Object* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Object* object.
        PROCEDURE   :: Copy         => SyntaxNode_Copy
        !> *IsEqualTo* is a procedure deferred by the *Object* type. <br>
        !  **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => SyntaxNode_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Object* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Object* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: MemFree      => SyntaxNode_MemFree
        !> *ToString* is a deferred procedure to get the string representation of this object.
        PROCEDURE   :: ToString     => SyntaxNode_ToString
        ! ---------------------------------------------------------------------
        ! -----                     Specific Procedures                   -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetRight <br>
        !  **Purpose**:  To get a pointer to the right node of the current node. <br>
        !  **Usage**: <br>
        !   --->    RightNode => CurrNode%GetRight()
        PROCEDURE   :: GetRight => SyntaxNode_GetRightNode
        !> **Type-Bound Function**: GetLeft <br>
        !  **Purpose**:  To get a pointer to the left node of the current node. <br>
        !  **Usage**: <br>
        !   --->    LeftNode => CurrNode%GetLeft()
        PROCEDURE   :: GetLeft  => SyntaxNode_GetLeftNode
        !> **Type-Bound Subroutine**: SetRight <br>
        !  **Purpose**:  To set a pointer to the right node of the current node. <br>
        !  **Usage**: <br>
        !   --->    CALL CurrNode%SetRight(RightNode)
        PROCEDURE   :: SetRight => SyntaxNode_SetRightNode
        !> **Type-Bound Subroutine**: SetLeft <br>
        !  **Purpose**:  To set a pointer to the left node of the current node. <br>
        !  **Usage**: <br>
        !   --->    CALL CurrNode%SetLeft(LeftNode)
        PROCEDURE   :: SetLeft  => SyntaxNode_SetLeftNode
        !> **Type-Bound Function**: HasRight <br>
        !  **Purpose**:  To check whether the right node of the specified node is associated or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = CurrNode%HasRight() <br>
        !   --->    IF (.NOT.CurrNode%HasRight()) DoSomething <br>
        PROCEDURE   :: HasRight => SyntaxNode_HasRightNode
        !> **Type-Bound Function**: HasLeft <br>
        !  **Purpose**:  To check whether the left node of the specified node is associated or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = CurrNode%HasLeft() <br>
        !   --->    IF (.NOT.CurrNode%HasLeft()) DoSomething <br>
        PROCEDURE   :: HasLeft  => SyntaxNode_HasLeftNode
        !> **Type-Bound Subroutine**: Operate <br>
        !  **Purpose**:  To set a pointer to both the left and right nodes of the current node.
        !                This operation is applicable only for the "BranchNode_Or", "BranchNode_Many",
        !                or "BranchNode_Concat" node type. <br>
        !  **Usage**: <br>
        !   --->    CALL CurrNode%Operate(LeftNode, RightNode)
        PROCEDURE   :: Operate  => SyntaxNode_Operate
        !> **Type-Bound Subroutine**: SetChar <br>
        !  **Purpose**:  To set the specified character to the current node.  This operation is
        !                only applicable for the "LeafNode_Char" node type. <br>
        !  **Usage**: <br>
        !   --->    CALL CurrNode%SetChar(C)
        PROCEDURE   :: SetChar  => SyntaxNode_SetCharacter
    END TYPE SyntaxNode
    TYPE SyntaxNodeMemHandler
        tIndex                          :: ID = 0_kIndex
        TYPE(SyntaxNode), ALLOCATABLE   :: Node(:)
    END TYPE SyntaxNodeMemHandler

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    TYPE(SyntaxNodeMemHandler), TARGET  :: STNMemManger

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! -----------------------------------------------------------------------------
! -----                     SyntaxNode Procedures                         -----
! -----------------------------------------------------------------------------

SUBROUTINE SyntaxNode_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the SyntaxNode object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxNode),  INTENT(IN)  :: SrcObj   !! source object
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
    TYPE IS (SyntaxNode)
        DstObj = SrcObj
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('SyntaxNode_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE SyntaxNode_Copy

!******************************************************************************

FUNCTION SyntaxNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxNode), INTENT(IN)   :: LhsObj   !! an object
    CLASS(Object),     INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (SyntaxNode)
        Flag = FalseVal
        IF (LhsObj%Type /= RhsObj%Type) RETURN
        IF (LhsObj%ID /= RhsObj%ID) RETURN
        Flag = (LhsObj%C /= RhsObj%C)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION SyntaxNode_IsEqualTo

!******************************************************************************

SUBROUTINE SyntaxNode_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of the SyntaxNode object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxNode), INTENT(INOUT)    :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ASSOCIATE (Dummy => Obj); END ASSOCIATE

    RETURN

END SUBROUTINE SyntaxNode_MemFree

!******************************************************************************

FUNCTION SyntaxNode_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxNode), INTENT(IN)   :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Str = 'SyntaxNode: {Type: ' // ToChar(Obj%Type) // ', ID: ' // ToChar(Obj%ID) // ', C: ' // Obj%C // '}'

    RETURN

END FUNCTION SyntaxNode_ToString

! -----------------------------------------------------------------------------
! -----                     Specific Procedures                           -----
! -----------------------------------------------------------------------------

SUBROUTINE SyntaxNode_New(Node, NodeType)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate the node and specify its type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SyntaxNode), POINTER, INTENT(INOUT)    :: Node
    tSInt32,                   INTENT(IN)       :: NodeType

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL SyntaxNode_Allocate(Node)
    
    ! check allocation status and report error if necessary
    Node%Type = NodeType
    Node%C    = NULCHR
    SELECT CASE (NodeType)
    CASE (BranchNode_Or)
        Node%ID = 0
    CASE (BranchNode_Many)
        Node%ID = 2
    CASE (BranchNode_Concat)
        Node%ID = 1
    CASE (BranchNode_LBracket)
        Node%ID = -1
    CASE (BranchNode_RBracket)
        Node%ID = 3
    CASE DEFAULT
        Node%ID = -2
    END SELECT
    
    RETURN

END SUBROUTINE SyntaxNode_New

!******************************************************************************

SUBROUTINE SyntaxNode_Free()

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free all the nodes linked to the specified node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! na

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SyntaxNode), POINTER   :: Left, Right
    tIndex                      :: I

! FLOW

    IF (ALLOCATED(STNMemManger%Node)) THEN
        DO I = 1_kIndex, SIZE(STNMemManger%Node, KIND=kIndex)
            Left  => STNMemManger%Node(I)%GetLeft()
            IF (ASSOCIATED(Left)) NULLIFY(Left)
            Right => STNMemManger%Node(I)%GetRight()
            IF (ASSOCIATED(Left)) NULLIFY(Right)
        END DO
        DEALLOCATE(STNMemManger%Node)
    END IF
    STNMemManger%ID = 0_kIndex

    RETURN

END SUBROUTINE SyntaxNode_Free

!******************************************************************************

SUBROUTINE SyntaxNode_Allocate(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a storage of the specified node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SyntaxNode), POINTER, INTENT(INOUT)    :: Node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW
    
    IF (.NOT.ALLOCATED(STNMemManger%Node)) THEN
        ! need allocation
        ALLOCATE(STNMemManger%Node(64_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
        IF (AllocStat /= 0) THEN
            STNMemManger%ID = 0_kIndex
            CALL Handle_ErrAlloc('SyntaxNode_Allocate', ModName, AllocMsg, AllocStat)
            RETURN
        END IF
        STNMemManger%ID = 1_kIndex
    ELSEIF (STNMemManger%ID == SIZE(STNMemManger%Node, KIND=kIndex)) THEN
        ! need re-allocation
        BLOCK
            TYPE(SyntaxNode), ALLOCATABLE   :: NewNode(:)
            ALLOCATE(NewNode(SIZE(STNMemManger%Node)*2_kIndex), STAT=AllocStat, ERRMSG=AllocMsg)
            IF (AllocStat /= 0) THEN
                CALL Handle_ErrAlloc('SyntaxNode_Allocate', ModName, AllocMsg, AllocStat)
                RETURN
            END IF
            NewNode(1:SIZE(STNMemManger%Node)) = STNMemManger%Node
            CALL MOVE_ALLOC(NewNode, STNMemManger%Node)
            STNMemManger%ID = STNMemManger%ID + 1
        END BLOCK
    END IF
    
    ! set pointer to the storage
    Node => STNMemManger%Node(STNMemManger%ID)
    
    RETURN

END SUBROUTINE SyntaxNode_Allocate

!******************************************************************************

FUNCTION SyntaxNode_GetRightNode(CurrNode) RESULT(RightNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get the right node of the current node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxNode), INTENT(IN)   :: CurrNode   !! current node
    TYPE(SyntaxNode),  POINTER      :: RightNode  !! right node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: NextNode

! FLOW
    
    NextNode => CurrNode%GetNext()
    SELECT TYPE (NextNode)
    TYPE IS (SyntaxNode)
        RightNode => NextNode
    END SELECT
    NULLIFY(NextNode)

    RETURN

END FUNCTION SyntaxNode_GetRightNode

!******************************************************************************

FUNCTION SyntaxNode_GetLeftNode(CurrNode) RESULT(LeftNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get the left node of the current node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxNode), INTENT(IN)   :: CurrNode !! current node
    TYPE(SyntaxNode),  POINTER      :: LeftNode !! left node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: PrevNode

! FLOW
    
    PrevNode => CurrNode%GetPrevious()
    SELECT TYPE (PrevNode)
    TYPE IS (SyntaxNode)
        LeftNode => PrevNode
    END SELECT
    NULLIFY(PrevNode)

    RETURN

END FUNCTION SyntaxNode_GetLeftNode

!******************************************************************************

SUBROUTINE SyntaxNode_SetRightNode(CurrNode, RightNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To set the right node of the current node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxNode),        INTENT(INOUT) :: CurrNode !! current node
    TYPE(SyntaxNode), TARGET, INTENT(IN)    :: RightNode !! right node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL CurrNode%SetNext(RightNode)

    RETURN

END SUBROUTINE SyntaxNode_SetRightNode

!******************************************************************************

SUBROUTINE SyntaxNode_SetLeftNode(CurrNode, LeftNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To set the left node of the current node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxNode),        INTENT(INOUT) :: CurrNode !! current node
    TYPE(SyntaxNode), TARGET, INTENT(IN)    :: LeftNode !! left node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL CurrNode%SetPrevious(LeftNode)

    RETURN

END SUBROUTINE SyntaxNode_SetLeftNode

!******************************************************************************

FUNCTION SyntaxNode_HasRightNode(CurrNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether the right node of the specified node is associated or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxNode), INTENT(IN)   :: CurrNode !! current node
    tLogical                        :: Flag     !! true if right node is associated

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = ASSOCIATED(CurrNode%GetNext())

    RETURN

END FUNCTION SyntaxNode_HasRightNode

!******************************************************************************

FUNCTION SyntaxNode_HasLeftNode(CurrNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether the left node of the specified node is associated or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxNode), INTENT(IN)   :: CurrNode !! current node
    tLogical                        :: Flag     !! true if right node is associated

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = ASSOCIATED(CurrNode%GetPrevious())

    RETURN

END FUNCTION SyntaxNode_HasLeftNode

!******************************************************************************

SUBROUTINE SyntaxNode_Operate(CurrNode, LeftNode, RightNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set both the left and right nodes of the current node.  This operation is applicable
    !  only for the "BranchNode_Or", "BranchNode_Many", or "BranchNode_Concat" node type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxNode),       INTENT(INOUT)  :: CurrNode     !! current node
    CLASS(DoublyLinkedNode), INTENT(IN)     :: LeftNode     !! left node
    CLASS(DoublyLinkedNode), INTENT(IN)     :: RightNode    !! right node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (CurrNode%Type)
    CASE (BranchNode_Or, BranchNode_Many, BranchNode_Concat)
        CALL CurrNode%SetPrevious(LeftNode)
        CALL CurrNode%SetNext(RightNode)
    END SELECT

    RETURN

END SUBROUTINE SyntaxNode_Operate

!******************************************************************************

SUBROUTINE SyntaxNode_SetCharacter(CurrNode, C)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the specified character to the current node.  This operation is
    !  only applicable for the "LeafNode_Char" node type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxNode), INTENT(INOUT)    :: CurrNode !! current node
    tChar,             INTENT(IN)       :: C        !! character to be set

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (CurrNode%Type == LeafNode_Char) THEN
        CurrNode%C = C
    END IF

    RETURN

END SUBROUTINE SyntaxNode_SetCharacter

!******************************************************************************

END MODULE MClass_SyntaxNode
    
!******************************************************************************
