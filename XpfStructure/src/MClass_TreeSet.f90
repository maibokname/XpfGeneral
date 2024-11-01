
MODULE MClass_TreeSet

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *TreeSet* type and its supporting routines and data type.
!   The *TreeSet* type is a collection type that employs a balanced binary-search-tree
!   (BST) implementation to provide common operations for an ordered set.  Like any other
!   set collection types, the *TreeSet* type does not allow duplicated items. <br>
!   The *TreeSet* type uses the *KeyOrdered* type to store comparable items where allowed
!   item types include the *CHARACTER*, *INTEGER* and *REAL* intrinsic types as well as any
!   derived type that is in the *Comparable* class.  Like other collection types, it must
!   be employed to store items of only one particular data type.  To store items of another
!   data type, it must be destructed before inserting items of different data type. <br>
!   As an *ordered* set, the *TreeSet* type provides an ordered iteration over its stored
!   items, which are sorted according to the natural ordering of its items.  It can be
!   accessed and traversed in either ascending (using the *StartFirst* method) or descending
!   (using the *StartLast* method) order. <br>
!   Technically, the *TreeSet* type employs a balanced search tree implementation to provide
!   common operations for an ordered set.  The *TreeSet* type utilizes the *IntrusiveRBTree*
!   type as its component to store its tree nodes.  As an intrusive BST container, the
!   *IntrusiveRBTree* type provides common binary-search-tree operations without a memory
!   management task.  The memory management task of the inserted tree nodes is handled by
!   the *TreeSet* type.  <br>

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MClass_CharBuffer
    USE MBase_ByteUtil,     ONLY: AnyType_GetByteSize
    USE MBase_SIntUtil,           ToChar => ToDecStrSigned
#ifdef Indx32Bits
    USE MBase_SimpleHash32, ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64, ONLY: ComputeHash => Hash64_FNV1a
#endif
    USE MClass_IntrusiveBSTrees
    USE MClass_Object,          ONLY: Object
    USE MClass_Comparable,      ONLY: Comparable
    USE MClass_KeyOrdered
    USE MClass_GenData
    USE MClass_CompNodePool
    USE MClass_MemoryPool
    USE MClass_BaseCollection
    USE MClass_BaseIterable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: TreeSet

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar,   PARAMETER  :: ModName = 'MClass_TreeSet'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    !>  *SetNode* is a binary-search-tree node type containing an item as its components.  The
    !   *KeyOrdered* type is used as a storage for the item.  The *SetNode* type is a subtype
    !   of the *BSTNode* type and is intended to be used with the *TreeSet* type, which is a
    !   collection type that utilizes the *IntrusiveRBTree* type. <br>
    TYPE, EXTENDS(BSTNode)  :: SetNode
        TYPE(KeyOrdered)    :: Item     !! stored item
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----     Deferred/Overridden Procedures from Object Type       -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => SetNode_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => SetNode_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => SetNode_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => SetNode_ToString
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => SetNode_HashCode
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedure from Comparable Type           -----
        ! ---------------------------------------------------------------------
        !> Use a common logical expression to compare two *Comparable* objects.
        PROCEDURE   :: CompareTo    => SetNode_CompareItem
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedure for SetNode Type               -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: SetItem <br>
        !  **Purpose**:  To set new item. <br>
        !  **Usage**: <br>
        !   --->    Valid = Node%SetItem(Item, MemPool)
        PROCEDURE   :: SetItem       => SetNode_SetItem
        ! ---------------------------------------------------------------------
    END TYPE SetNode
    !>  The *TreeSet* type is a collection type that utilizes a balanced BST implementation
    !   to provide common operations for an ordered set.  The *TreeSet* type employs the
    !   *IntrusiveRBTree* type as its component to store *SetNode* objects.  As an intrusive
    !   BST container, the *IntrusiveRBTree* type provides common binary-search-tree operations
    !   without a memory management task.  The memory management task of the inserted *SetNode*
    !   objects is handled by the *TreeSet* type.  <br>
    !   The *TreeSet* type is a subtype of the *BaseIterable* type.  Thus, it implements all
    !   deferred procedures required by the *BaseIterable* type and all its super classes.  As
    !   a set container, the *TreeSet* type does not allow duplicated items.  Also, the *TreeSet*
    !   type, as an *ordered* set, provides an ordered iteration over its stored items, which are
    !   sorted according to the natural ordering of its items.  It can be traversed in either
    !   ascending or descending order. <br>
    !   Because the *IntrusiveRBTree* type is a subtype of the *IntrusiveAVLTree* type, the
    !   *WrkTree* component can be employed as a red-black tree or an AVL tree.  Therefore, the
    !   *TreeSet* type allows a user to specify which type of binary-search tree implementation
    !   to be used.  By default, the red-black tree implementation is used.  The user can call the
    !   *UseAVLTree* method to change to AVL tree implementation.  The *UseAVLTree* method must
    !   be called before inserting an object into the set (i.e when the set is empty).  Otherwise,
    !   the red-black tree implementation is employed.  <br>
    TYPE, EXTENDS(BaseIterable) :: TreeSet
        PRIVATE
        !> a flag indicating whether to use the red-black tree implementation
        !  or the AVL tree implementation
        tLogical                :: IsRBTree = TrueVal
        !% a working binary-search tree
        TYPE(IntrusiveRBTree)   :: WrkTree
        !> direction of the iteration <br>
        !  - positive -> forward iteration
        !  - negative -> backward iteration
        !  - zero     -> iteration not yet start
        tSInt32                 :: Dir = 0
        !> memory pool of tree nodes 
        TYPE(CompNodePool)      :: NodePool
        !> memory pool of stored items
        TYPE(MemoryPool)        :: ItemPool
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: FindItem <br>
        !  **Purpose**:  To find the specified item in the collection.  Return true if
        !                the specified item is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%FindItem(Item) <br>
        !   --->    IF (.NOT.Collection%FindItem(Item)) DoSomething
        PROCEDURE, PRIVATE  :: FindItem     => TreeSet_FindItem
        !> Use the *Construct* method to construct the collection from an array of items.
        PROCEDURE, PRIVATE  :: TreeSet_CreateByArray
        ! ---------------------------------------------------------------------
        ! -----  Deferred/Overridden Procedures from BaseCollection Type  -----
        ! ---------------------------------------------------------------------
        !> Use the *Construct* method to construct the collection from another collection.
        PROCEDURE   :: CopyCollection   => TreeSet_CopyCollection
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        PROCEDURE   :: Clear            => TreeSet_ClearItems
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        PROCEDURE   :: Destruct         => TreeSet_Destroy
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize          => TreeSet_GetSize
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseIterable Type        -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag indicating
        !                whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartFirst() <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstItem)
        PROCEDURE   :: StartFirst       => TreeSet_Move2FirstItem
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next iteration and return a flag indicating whether
        !                the cursor pointer has reached the end of the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextItem) <br>
        !  **Note**: See the <a href="../module/mclass_baseiterable.html">BaseIterable</a> type
        !            for illustrations of an iteration over a collection using the *StartFirst*
        !            and *MoveForward* methods.<br>
        PROCEDURE   :: MoveForward      => TreeSet_Move2NextItem
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start an iteration in a reversed order and return a flag
        !                indicating whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartLast() <br>
        !   --->    IsEmpty = Collection%StartLast(LastItem) <br>
        PROCEDURE   :: StartLast        => TreeSet_Move2LastItem
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move to the previous item and return a flag
        !                indicating whether the cursor pointer has reached the end of
        !                the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveBackward() <br>
        !   --->    IsTheEnd = Collection%MoveBackward(PrevItem) <br>
        PROCEDURE   :: MoveBackward     => TreeSet_Move2PrevItem
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified item to the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Insert(Item) <br>
        PROCEDURE   :: Insert           => TreeSet_Insert
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete the current item from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the *StartFirst* and
        !       *MoveForward* methods.  Therefore, after the call to one of those methods and then
        !       calling this one will result in a removal of the current item of the iteration
        !       (i.e. the same item that can be retrieved via the *StartFirst* and *MoveForward*
        !       methods). <br>
        PROCEDURE   :: Delete           => TreeSet_Delete
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the collection.  Also, return a flag
        !       indicating whether the items are successfully retrieved and removed or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Items) <br>
        !   --->    IF (.NOT.Collection%ToArray(Items)) DoSomething
        PROCEDURE   :: ToArray          => TreeSet_ToArray
        !> **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the collection. Also, return
        !                a flag indicating whether the items are successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Items) <br>
        !   --->    IF (.NOT.Collection%GetAll(Items)) DoSomething
        PROCEDURE   :: GetAll           => TreeSet_GetAll
        ! ---------------------------------------------------------------------
        ! -----                      Specific Procedures                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new collection from an array of items or from another
        !                collection. <br>
        !  **Usage**: <br>
        !   ! create a collection from an array of 25 items <br>
        !   --->    CALL Collection%Construct(25, Arr) <br>
        !   ! create a collection from another collection <br>
        !   --->    CALL Collection%Construct(OtherCollection)
        GENERIC     :: Construct    => TreeSet_CreateByArray
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified item in the collection.  Return true if
        !                the specified item is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Contain(Item) <br>
        !   --->    IF (.NOT.Collection%Contain(Item)) DoSomething
        PROCEDURE   :: Contain      => TreeSet_Contain
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified item from the collection.  Also, return a flag
        !                indicating whether the item is successfully removed or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Remove(Item) <br>
        !   --->    IF (.NOT.Collection%Remove(Item)) DoSomething
        PROCEDURE   :: Remove       => TreeSet_Remove
        !> **Type-Bound Subroutine**: UseAVLTree <br>
        !  **Purpose**:  To set the working tree component to work as an AVL tree.  <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%UseAVLTree() <br>
        !  *Note*: The collection must be empty when calling this method.
        PROCEDURE   :: UseAVLTree   => TreeSet_UseAVLTree
        !> **Type-Bound Subroutine**: UseRBTree <br>
        !  **Purpose**:  To set the working tree component to work as an red-black RB tree.  <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%UseRBTree() <br>
        !  *Note*: The collection must be empty when calling this method.
        PROCEDURE   :: UseRBTree   => TreeSet_UseRBTree
        ! ---------------------------------------------------------------------
        ! -----     Deferred/Overridden Procedures from Object Type       -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => TreeSet_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => TreeSet_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => TreeSet_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => TreeSet_ToString
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => TreeSet_HashCode
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the hash set.
        FINAL       :: TreeSet_Finalize
        ! ---------------------------------------------------------------------
    END TYPE TreeSet

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! ---------------------------------------------------------------------
! -----                 SetNode Procedures                        -----
! ---------------------------------------------------------------------

SUBROUTINE SetNode_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy the SetNode object.
    !  This is a deferred procedure by an *Object* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetNode),     INTENT(IN)  :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
    !^ Flag indicating whether to perform deep copy or shallow copy. <br>
    !  - If present and true, perform a deep copy. <br>
    !  - If present and false, perform a shallow copy. <br>
    !  - If not present, perform either a shallow or a deep copy that is naturally most
    !    suitable for the object's components.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! copy Key and Value components
    SELECT TYPE (DstObj)
    TYPE IS (SetNode)
        CALL SrcObj%CopyBSTNode(DstObj)
        CALL SrcObj%Item%Copy(DstObj%Item, IsDeep)
    CLASS DEFAULT
        CALL Handle_ErrLevel('SetNode_Copy', ModName, ErrSevere, &
                             'Type of the destination object must be "SetNode" only.')
        RETURN
    END SELECT

    RETURN

END SUBROUTINE SetNode_Copy

!******************************************************************************

FUNCTION SetNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.  This is a deferred procedure by
    !  an *Object* object. <br>
    !  It should be noted that this routine uses all components of the *SetNode* object to
    !  check equality. Therefore, although (A%CompareTo(B) == 0) returns true, (A%IsEqualTo(B))
    !  can return false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetNode), INTENT(IN)  :: LhsObj   !! an object
    CLASS(Object),  INTENT(IN)  :: RhsObj   !! another object
    tLogical                    :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check key and value equalities
    SELECT TYPE (RhsObj)
    TYPE IS (SetNode)
        Flag = FalseVal
        IF (.NOT.LhsObj%Item%IsEqualTo(RhsObj%Item)) RETURN
        Flag = LhsObj%IsBSTNodeEqual(RhsObj)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION SetNode_IsEqualTo

!******************************************************************************

SUBROUTINE SetNode_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the SetNode object.
    !  This is a deferred procedure by an *Object* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetNode), INTENT(INOUT)   :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! free the key and value components
    CALL Obj%Item%MemFree()
    
    ! reset the "BSTNode" components
    CALL Obj%ResetBSTNode()

    RETURN

END SUBROUTINE SetNode_MemFree

!******************************************************************************

FUNCTION SetNode_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.
    !  This is a deferred procedure by an *Object* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetNode), INTENT(IN)  :: Obj
    tCharAlloc                  :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Str = '{SetNode: ' // Obj%Item%ToString() // '}'

    RETURN

END FUNCTION SetNode_ToString

!******************************************************************************

FUNCTION SetNode_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetNode), INTENT(IN)  :: Obj
    tIndex                      :: Code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER     :: AdjNode

! FLOW

    ! get code from left node
    AdjNode => Obj%GetLeft()
    SELECT TYPE (Left => AdjNode)
    TYPE IS (SetNode)
        Code = Left%Item%HashCode()
    END SELECT

    ! add code from this node
    Code = Code + Obj%Item%HashCode()

    ! add code from right node
    AdjNode => Obj%GetRight()
    SELECT TYPE (Right => AdjNode)
    TYPE IS (SetNode)
        Code = Code + Right%Item%HashCode()
    END SELECT

    ! free pointer    
    NULLIFY(AdjNode)
    
    RETURN

END FUNCTION SetNode_HashCode

!******************************************************************************

FUNCTION SetNode_CompareItem(A, B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare *A* and *B* and return <br>
    !   1 if *A* is greater than *B*, <br>
    !   0 if *A* is equal to *B*, <br>
    !  -1 if *A* is less than *B*, <br>
    !  -999 if type of *B* is invalid. <br>
    !  Also, write an error message to the default log file if this happens. <br>
    !  This is a deferred procedure by an *Comparable* object. <br>
    !  It is important to note that this routine only uses the key component
    !  of the *SetNode* object.  Thus, even though (A%CompareTo(B) == 0)
    !  is true, A%IsEqualTo(B) may be false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetNode),    INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tSInt32                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (B)
    TYPE IS (SetNode)
        Flag = A%Item%CompareTo(B%Item)
    CLASS DEFAULT
        Flag = -999
        CALL Handle_ErrLevel('SetNode_CompareItem', ModName, ErrSevere, 'Type of B is valid.')
    END SELECT

    RETURN

END FUNCTION SetNode_CompareItem

!******************************************************************************

SUBROUTINE SetNode_SetItem(Node, Key, Pool)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the key and value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetNode),   INTENT(INOUT) :: Node     !! SetNode object
    CLASS(*),         INTENT(IN)    :: Key      !! the key
    TYPE(MemoryPool), INTENT(INOUT) :: Pool     !! memory pool for both key and value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL Node%Item%Set(Key, Pool)

    RETURN

END SUBROUTINE SetNode_SetItem

! ---------------------------------------------------------------------
! -----     Deferred/Overridden Procedures from Object Type       -----
! ---------------------------------------------------------------------

SUBROUTINE TreeSet_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet),     INTENT(IN)  :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
    !^ Flag indicating whether to perform deep copy or shallow copy. <br>
    !  - If present and true, perform a deep copy. <br>
    !  - If present and false, perform a shallow copy. <br>
    !  - If not present, perform either a shallow or a deep copy that is naturally most
    !    suitable for the object's components.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT TYPE (DstObj)
    TYPE IS (TreeSet)
        DstObj%Dir      = SrcObj%Dir
        DstObj%IsRBTree = SrcObj%IsRBTree
        CALL SrcObj%NodePool%Copy(DstObj%NodePool, IsDeep)
        CALL SrcObj%ItemPool%CloneTo(DstObj%ItemPool)
        CALL SrcObj%WrkTree%CloneTo(DstObj%WrkTree)
    CLASS DEFAULT
        CALL Handle_ErrLevel('TreeSet_Copy', ModName, ErrSevere, &
                             'Type of the DstObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE TreeSet_Copy

!******************************************************************************

FUNCTION TreeSet_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet), INTENT(IN)  :: LhsObj   !! an object
    CLASS(Object),  INTENT(IN)  :: RhsObj   !! another object
    tLogical                    :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    TYPE IS (TreeSet)
        Flag = FalseVal
        IF (LhsObj%GetSize() /= RhsObj%GetSize()) RETURN
        IF (.NOT.LhsObj%IsEmpty()) THEN
            ! implementation note:  we cannot use the iteration methods here
            !   due to the intent of the input data.
            BLOCK
                CLASS(BSTNode), POINTER :: LhsNode, LhsNext, LhsRoot
                CLASS(BSTNode), POINTER :: RhsNode, RhsNext, RhsRoot
                tLogical                :: ReturnNow
                ReturnNow = FalseVal
                ! start iteration
                LhsNode => LhsObj%WrkTree%GetMinNode()
                RhsNode => RhsObj%WrkTree%GetMinNode()
                LhsRoot => LhsObj%WrkTree%GetRoot()
                RhsRoot => RhsObj%WrkTree%GetRoot()
                Loop: DO WHILE (ASSOCIATED(LhsNode).AND.ASSOCIATED(RhsNode))
                    ! get successor nodes
                    CALL Find_Inorder_Successor(LhsRoot, LhsNode, LhsNext)
                    CALL Find_Inorder_Successor(RhsRoot, RhsNode, RhsNext)
                    ! check key and value equalities
                    SELECT TYPE (LhsNode)
                    TYPE IS (SetNode)
                        SELECT TYPE (RhsNode)
                        TYPE IS (SetNode)
                            IF (.NOT.LhsNode%IsEqualTo(RhsNode)) THEN
                                ReturnNow = TrueVal
                                EXIT Loop
                            END IF
                        END SELECT
                    END SELECT
                    ! move to the next iteration
                    LhsNode => LhsNext
                    RhsNode => RhsNext
                END DO Loop
                NULLIFY(LhsNode, RhsNode)
                NULLIFY(LhsNext, RhsNext)
                NULLIFY(LhsRoot, RhsRoot)
                IF (ReturnNow) RETURN
            END BLOCK
        ELSE
            RETURN
        END IF
        Flag = TrueVal
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION TreeSet_IsEqualTo

!******************************************************************************

SUBROUTINE TreeSet_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the TreeSet object.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet), INTENT(INOUT)  :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%WrkTree%Clear()
    CALL Obj%NodePool%MemFree()
    CALL Obj%ItemPool%Destruct()
    CALL Obj%FreeMold()

    RETURN

END SUBROUTINE TreeSet_MemFree

!******************************************************************************

FUNCTION TreeSet_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the TreeSet type.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet), INTENT(IN)  :: Obj
    tCharAlloc                  :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: BaseStr

! FLOW

    ! get base string
    IF (Obj%IsEmpty()) THEN
        BaseStr = '[NULL]'
    ELSE
        ! implementation note:  we cannot use the iteration methods here
        ! due to the intent of the input data.
        BLOCK
            CLASS(BSTNode), POINTER :: CurrNode, NextNode, RootNode
            TYPE(CharBuffer)        :: ChrBuf
            tCharAlloc              :: ItemStr
            ! initialize
            CALL ChrBuf%CreateEmpty(InitCap=Obj%GetSize()*60_kIndex)
            CALL ChrBuf%Append('[')
            ! start iteration
            CurrNode => Obj%WrkTree%GetMinNode()
            RootNode  => Obj%WrkTree%GetRoot()
            Loop: DO WHILE (ASSOCIATED(CurrNode))
                ! get successor nodes
                CALL Find_Inorder_Successor(RootNode, CurrNode, NextNode)
                ! get string representation of the stored item of the current node
                SELECT TYPE (CurrNode)
                TYPE IS (SetNode)
                    ItemStr = CurrNode%Item%ToString()
                    CALL ChrBuf%Append(ItemStr(15:LEN(ItemStr)-1))
                    IF (ASSOCIATED(NextNode)) CALL ChrBuf%Append(', ')
                END SELECT
                ! move to the next iteration
                CurrNode => NextNode
            END DO Loop
            NULLIFY(CurrNode, NextNode, RootNode)
            CALL ChrBuf%Append(']')
            Str = ChrBuf%AsString()
        END BLOCK
    END IF
    Str = '{TreeSet with ' // ToChar(Obj%GetSize()) // ' items : ' // BaseStr // '}'

    RETURN

END FUNCTION TreeSet_ToString

!******************************************************************************

FUNCTION TreeSet_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet), INTENT(IN)  :: Obj
    tIndex                      :: Code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Obj%IsEmpty()) THEN
        BLOCK
            tCharAlloc  :: BaseStr
            BaseStr = Obj%ToString()
            Code = ComputeHash(BaseStr, AnyType_GetByteSize(BaseStr))
        END BLOCK
    ELSE
        BLOCK
            CLASS(CompNode), POINTER    :: PoolNodes(:)
            tIndex                      :: I
            ! initialize
            CALL Obj%NodePool%GetAllNodes(PoolNodes)
            Code = 0_kIndex
            ! compute hash code
            SELECT TYPE (Nodes => PoolNodes)
            TYPE IS (SetNode)
                DO I = 1_kIndex, SIZE(Nodes, KIND=kIndex)
                    IF (Nodes(I)%Item%IsEmpty()) CYCLE
                    Code = Code + Nodes(I)%Item%HashCode()
                END DO
            END SELECT
        END BLOCK
    END IF

    RETURN

END FUNCTION TreeSet_HashCode

! ---------------------------------------------------------------------
! -----  Deferred/Overridden Procedures from BaseCollection Type  -----
! ---------------------------------------------------------------------

SUBROUTINE TreeSet_CopyCollection(This, Other, ItemCopy, ValCopy)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items as the given collection (Other).
    !  This is a deferred procedure by the *BaseCollection* class. <br>
    !  *Note*:  Other must be in the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(TreeSet),        INTENT(INOUT)    :: This
    !% collection object to be copied
    CLASS(BaseCollection), INTENT(INOUT)    :: Other
    !> a helper procedure to copy stored items for a derived type not in the *Object* class;
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL      :: ItemCopy
    !> a helper procedure to copy stored values for a derived type not in the *Object*
    !  class; required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL      :: ValCopy

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! return if Other is empty
    IF (Other%IsEmpty()) RETURN

    SELECT TYPE (Other)
    CLASS IS (TreeSet)
        ! same type of collection
        CALL Other%Copy(This)
    CLASS IS (BaseIterable)
        ! different types of collection
        BLOCK
            ! block variables
            tLogical                :: IsTheEnd
            CLASS(*), POINTER       :: MoldPtr
            CLASS(*), ALLOCATABLE   :: Item
            MoldPtr => Other%GetItemPtr()
            ALLOCATE(Item, MOLD=MoldPtr)
            ! loop through the other collection and get items along the way
            IsTheEnd = Other%StartFirst(Item, ItemCopy)
            DO WHILE (.NOT.IsTheEnd)
                ! add an item to this collection
                CALL This%Insert(Item)
                IsTheEnd = Other%MoveForward(Item, ItemCopy)
            END DO
            NULLIFY(MoldPtr)
            DEALLOCATE(Item)
        END BLOCK
    CLASS DEFAULT
        CALL Handle_ErrLevel('TreeSet_CopyCollection', ModName, ErrSevere, &
                             'Type of "Other" must be in the "BaseIterable" class.')
    END SELECT
    
    RETURN

END SUBROUTINE TreeSet_CopyCollection

!******************************************************************************

SUBROUTINE TreeSet_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To free components of the items from the set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet), INTENT(INOUT)   :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! simply return if the collection is empty
    IF (Collection%IsEmpty()) RETURN

    ! clear elements
    CALL Collection%WrkTree%Clear()

    ! reset components
    Collection%Dir = 0
    CALL Collection%NodePool%Reset()

    ! free mold
    CALL Collection%FreeMold()

    RETURN

END SUBROUTINE TreeSet_ClearItems

!******************************************************************************

SUBROUTINE TreeSet_Destroy(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet), INTENT(INOUT)   :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    CALL Collection%MemFree()

    RETURN

END SUBROUTINE TreeSet_Destroy

!******************************************************************************

FUNCTION TreeSet_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the number of items currently in the hash set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet), INTENT(IN)  :: Collection   !! collection
    tIndex                      :: Size         !! the number of items

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%WrkTree%GetSize()

    RETURN

END FUNCTION TreeSet_GetSize

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseIterable Type         -----
! ---------------------------------------------------------------------

FUNCTION TreeSet_Move2FirstItem(Collection, Item, ItemCopy) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the first (starting) item in a collection.   For the hash set, which is
    !  an unordered set, the starting item is the first item found in the non-empty bucket.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet),     INTENT(INOUT)   :: Collection    !! collection
    !% the first item as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Item
    !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
    !> a flag indicating whether the table contains no item or not <br>
    ! - true if the table is empty. <br>
    ! - otherwise the first item is available.
    tLogical                            :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (PRESENT(Item)) THEN
        BLOCK
            ! block variable
            CLASS(BSTNode), POINTER :: CurrNode
            ! start iteration
            IsEmpty = Collection%WrkTree%StartMin(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                SELECT TYPE (CurrNode)
                TYPE IS (SetNode)
                    ! get key if requested
                    IF (PRESENT(Item)) THEN
                        IF (.NOT.CurrNode%Item%Get(Item)) THEN
                            CALL Handle_ErrLevel('TreeSet_Move2FirstPair', ModName, ErrWarning, &
                                    'Type of the specified key is likely NOT the same as that of stored keys.')
                        END IF
                    END IF
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration
        IsEmpty = Collection%WrkTree%StartMin()
    END IF

    ! set direction
    IF (.NOT.IsEmpty) Collection%Dir = 1

    RETURN

END FUNCTION TreeSet_Move2FirstItem

!******************************************************************************

FUNCTION TreeSet_Move2NextItem(Collection, Item, ItemCopy) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next item in a collection.  For the hash set, which is an unordered set,
    !  the next item is a item inserted in the first non-empty bucket after the previous one.  <br>
    !  The routine will report an error if an alteration to stored item(s) (either by an insertion
    !  or a removal) has been occurred during current iteration.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet),     INTENT(INOUT)   :: Collection    !! collection
    !% the next item as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Item
    !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
    !> a flag indicating whether the move to the end of the table occurs or not <br>
    ! - true if next item is NOT available. <br>
    ! - otherwise next item is available.
    tLogical                            :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (PRESENT(Item)) THEN
        BLOCK
            ! block variable
            CLASS(BSTNode), POINTER :: CurrNode
            ! move to next iteration
            IsTheEnd = Collection%WrkTree%MoveForward(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (SetNode)
                    ! get key if requested
                    IF (PRESENT(Item)) THEN
                        IF (.NOT.CurrNode%Item%Get(Item)) THEN
                            CALL Handle_ErrLevel('TreeSet_Move2NextPair', ModName, ErrWarning, &
                                    'Type of the specified key is likely NOT the same as that of stored keys.')
                        END IF
                    END IF
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! move to next iteration
        IsTheEnd = Collection%WrkTree%MoveForward()
    END IF

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        Collection%Dir = 1
    ELSE
        Collection%Dir = 0
    END IF

    RETURN

END FUNCTION TreeSet_Move2NextItem

!******************************************************************************

FUNCTION TreeSet_Move2LastItem(Collection, Item) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the last (starting in a reversed order) item in a symbol table. For the
    !  *TreeSet*, which is an ordered symbol table, the starting item in a reversed order
    !  is the item with greatest key.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeSet),     INTENT(INOUT)   :: Collection
    !% the greatest key as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Item
    !> a flag indicating whether the collection contains no item or not <br>
    ! - true if the collection is empty. <br>
    ! - otherwise the first item is available.
    tLogical                            :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (PRESENT(Item)) THEN
        BLOCK
            ! block variable
            CLASS(BSTNode), POINTER :: CurrNode
            ! start iteration
            IsEmpty = Collection%WrkTree%StartMax(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (SetNode)
                    ! get key if requested
                    IF (PRESENT(Item)) THEN
                        IF (.NOT.CurrNode%Item%Get(Item)) THEN
                            CALL Handle_ErrLevel('TreeSet_Move2LastItem', ModName, ErrWarning, &
                                    'Type of the specified key is likely NOT the same as that of stored keys.')
                        END IF
                    END IF
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration
        IsEmpty = Collection%WrkTree%StartMax()
    END IF

    ! set direction
    IF (.NOT.IsEmpty) Collection%Dir = -1

    RETURN

END FUNCTION TreeSet_Move2LastItem

!******************************************************************************

FUNCTION TreeSet_Move2PrevItem(Collection, Item) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the previous item in a symbol table.  For the *TreeSet*, which is an
    !  ordered symbol table,  the previous item is the so-called predecessor item.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeSet),     INTENT(INOUT)   :: Collection
    !% the previous key as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Item
    !> a flag indicating whether the move to the end of the
    !  collection occurs or not <br>
    ! - true if next item is NOT available. <br>
    ! - otherwise next item is available.
    tLogical                            :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (PRESENT(Item)) THEN
        BLOCK
            ! block variable
            CLASS(BSTNode), POINTER :: CurrNode
            ! start iteration
            IsTheEnd = Collection%WrkTree%MoveBackward(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (SetNode)
                    ! get key if requested
                    IF (PRESENT(Item)) THEN
                        IF (.NOT.CurrNode%Item%Get(Item)) THEN
                            CALL Handle_ErrLevel('TreeSet_Move2PrevItem', ModName, ErrWarning, &
                                    'Type of the specified key is likely NOT the same as that of stored keys.')
                        END IF
                    END IF
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration
        IsTheEnd = Collection%WrkTree%MoveBackward()
    END IF

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        Collection%Dir = -1
    ELSE
        Collection%Dir = 0
    END IF

    RETURN

END FUNCTION TreeSet_Move2PrevItem

!******************************************************************************

SUBROUTINE TreeSet_Insert(Collection, Item)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add an item into the hash set.  If the specified item is already stored
    !  in the set, report severe error and return immediately.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet), INTENT(INOUT)   :: Collection   !! collection
    CLASS(*),       INTENT(IN)      :: Item         !! item to be inserted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical                    :: ItemFound
    TYPE(SetNode),   POINTER    :: ItemNode
    CLASS(CompNode), POINTER    :: NewNode

! FLOW

    ! check the specified item
    IF (.NOT.IsKeyOrdered(Item)) THEN
        CALL Handle_ErrLevel('TreeSet_Insert', ModName, ErrSevere, &
                'Type of the specified item is NOT valid.')
        RETURN
    ELSEIF (.NOT.Collection%IsItemValid(Item)) THEN
        CALL Handle_ErrLevel('HashSet_Insert', ModName, ErrSevere, &
                'Only items of the same type are allowed in a collection.')
        RETURN
    END IF

    ! check whether the key is already stored in the collection or not
    IF (Collection%IsEmpty()) THEN
        ItemFound = FalseVal
    ELSE
        ItemFound = Collection%FindItem(Item, ItemNode)
    END IF
    
    IF (ItemFound) THEN
        ! no duplicated items are allowed.
        CALL Handle_ErrLevel('TreeSet_Insert', ModName, ErrSevere, &
                'The specified item is already stored in the set.')
        RETURN
    ELSE
        ! +++ new item +++
        ! check for first-time insertion
        IF (.NOT.Collection%NodePool%IsReady()) THEN
            CALL Collection%NodePool%Construct(ItemNode)
            CALL Collection%ItemPool%Construct()
        END IF
        ! get new node from the node's pool
        CALL Collection%NodePool%GetNewNode(NewNode)
        SELECT TYPE (NewNode)
        TYPE IS (SetNode)
            ItemNode => NewNode
        END SELECT
        ! set key and value to the node
        ! (no need to check the valid flag since we have already done that in the beginning)
        CALL ItemNode%SetItem(Item, Collection%ItemPool)
        ! insert the new node to the working tree
        IF (Collection%IsRBTree) THEN
            ! WrkTree is a red-black tree.
            CALL Collection%WrkTree%Insert(ItemNode)
        ELSE
            ! WrkTree is an AVL tree.
            CALL Collection%WrkTree%IntrusiveAVLTree%Insert(ItemNode)
        END IF
    END IF
    
    ! free working pointer
    NULLIFY(ItemNode, NewNode)

    RETURN

END SUBROUTINE TreeSet_Insert

!******************************************************************************

SUBROUTINE TreeSet_Delete(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete a item of the current iteration from a collection.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeSet), INTENT(INOUT)   :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: CurrNode
    tLogical                :: Success
    tLogical                :: IsTheEnd

! FLOW

    ! get the cursor node
    CurrNode => Collection%WrkTree%GetCursor()
    
    ! check if the node is associated
    IF (ASSOCIATED(CurrNode)) THEN
        ! reset cursor
        IF (Collection%Dir == 1) THEN
            ! forward iteration so move cursor backward
            IsTheEnd = Collection%WrkTree%MoveBackward()
        ELSE
            ! backward iteration so move cursor forward
            IsTheEnd = Collection%WrkTree%MoveForward()
        END IF
        IF (Collection%IsRBTree) THEN
            ! WrkTree is a red-black tree.
            Success = Collection%WrkTree%Remove(CurrNode)
        ELSE
            ! WrkTree is an AVL tree.
            Success = Collection%WrkTree%IntrusiveAVLTree%Remove(CurrNode)
        END IF
        ! check if remove the node successfully or not
        IF (Success) THEN
            ! return the node to the node's pool
            SELECT TYPE (DelNode => CurrNode)
            TYPE IS (SetNode)
                CALL Collection%NodePool%ReturnNode(DelNode)
            END SELECT
        ELSE
            CALL Handle_ErrLevel('TreeSet_Delete', ModName, ErrWarning, &
                       'Check the Remove method of the intrusive tree for possible bug(s).')
        END IF
    END IF
    
    ! free working pointer
    NULLIFY(CurrNode)

    RETURN

END SUBROUTINE TreeSet_Delete

!**************************************************************************************

FUNCTION TreeSet_ToArray(Collection, Items, ItemCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove all items from the collection.  Also, return
    !  a flag indicating whether the items are successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TreeSet object
    CLASS(TreeSet),      INTENT(INOUT)  :: Collection
    !% the item to be removed from the collection
    CLASS(*),            INTENT(INOUT)  :: Items(:)
    !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
    !> flag indicating whether the items are successfully retrieved and removed. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! retrieve all items
    Success = Collection%GetAll(Items, ItemCopy)
    
    ! remove all items
    IF (Success) CALL Collection%Clear()

    RETURN

END FUNCTION TreeSet_ToArray

!**************************************************************************************

FUNCTION TreeSet_GetAll(Collection, Items, ItemCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all items (without removing them) from the collection.  Also,
    !  return a flag indicating whether the items are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TreeSet object
    CLASS(TreeSet),      INTENT(INOUT)  :: Collection
    !% the item to be removed from the collection
    CLASS(*),            INTENT(INOUT)  :: Items(1:)
    !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
    !> flag indicating whether the items are successfully retrieved. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        Success = FalseVal
    ELSEIF (.NOT.Collection%IsItemValid(Items(1))) THEN
        Success = FalseVal
        CALL Handle_ErrLevel('TreeSet_GetAll', ModName, ErrSevere, &
                                'Type of the specified keys is NOT the same as that of stored keys.')
    ELSE
        Success = TrueVal
    END IF
    IF (.NOT.Success) RETURN

    BLOCK
        tIndex      :: I, ArrSize
        tLogical    :: IsTheEnd
        ! initialize local variables
        ArrSize = SIZE(Items, KIND=kIndex)
        IF (ArrSize < 1_kindex) RETURN
        I = 1_kindex
        ! loop through the collection and get the keys along the way
        IsTheEnd = Collection%StartFirst(Items(I))
        DO WHILE ((.NOT.IsTheEnd).AND.(I < ArrSize))
            I = I + 1_kIndex 
            IsTheEnd = Collection%MoveForward(Items(I))
        END DO
    END BLOCK

    RETURN

END FUNCTION TreeSet_GetAll

! -----------------------------------------------------------------------------
! -----                        Common Procedures                          -----
! -----------------------------------------------------------------------------

FUNCTION TreeSet_FindItem(Collection, Item, ItemNode) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified item is currently stored in a hash set or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet), INTENT(INOUT)   :: Collection   !! collection
    CLASS(*),       INTENT(IN)      :: Item         !! item to be looked for
    !% the node containing the specified item; null pointer if the item is not found
    TYPE(SetNode), OPTIONAL, POINTER, INTENT(OUT)   :: ItemNode
    !> flag indicating whether the specified item is found or not.
    tLogical                        :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SetNode)           :: InNode
    CLASS(BSTNode), POINTER :: StoredNode

! FLOW

    ! initialize
    Found = Falseval
    IF (PRESENT(ItemNode)) ItemNode => NULL()

    ! check the specified key and return quickly if not valid
    IF (.NOT.IsKeyOrdered(Item)) RETURN
    IF (.NOT.Collection%IsItemValid(Item)) RETURN

    ! set key for search node
    CALL InNode%SetItem(Item, Collection%ItemPool)

    ! find the stored node equal to input node
    Found = Collection%WrkTree%Contain(InNode, StoredNode)
    IF (PRESENT(ItemNode).AND.Found) THEN
        SELECT TYPE (StoredNode)
        TYPE IS (SetNode)
            ItemNode => StoredNode
        END SELECT
    END IF

    ! free working pointer
    NULLIFY(StoredNode)

    RETURN

END FUNCTION TreeSet_FindItem

!******************************************************************************

SUBROUTINE TreeSet_CreateByArray(Collection, N, Items)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a table from an array of items.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet),      INTENT(INOUT)  :: Collection   !! collection
    tIndex,              INTENT(IN)     :: N            !! number of items
    CLASS(*),            INTENT(IN)     :: Items(N)     !! the items to be added to the set

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! add items to the set
    DO I = 1_kIndex, N
        CALL Collection%Insert(Items(I))
    END DO

    RETURN

END SUBROUTINE TreeSet_CreateByArray

!******************************************************************************

SUBROUTINE TreeSet_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To perform finalization of the collection.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(TreeSet), INTENT(INOUT)    :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE TreeSet_Finalize

!******************************************************************************

FUNCTION TreeSet_Remove(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the specified item (and its associated value) from a collection.  Also,
    !  return a flag indicating whether the item is successfully removed or not. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet), INTENT(INOUT)   :: Collection   !! collection
    CLASS(*),       INTENT(IN)      :: Item         !! item to be removed
    !> flag indicating whether the specified item and its associated
    !  value are successfully removed or not.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SetNode), POINTER  :: ItemNode
    tLogical                :: Success

! FLOW
    
    ! check whether the key is stored in the collection or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
    ELSE
        Flag = Collection%FindItem(Item, ItemNode)
    END IF
    
    IF (Flag) THEN
        IF (Collection%IsRBTree) THEN
            ! WrkTree is a red-black tree.
            Success = Collection%WrkTree%Remove(ItemNode)
        ELSE
            ! WrkTree is an AVL tree.
            Success = Collection%WrkTree%IntrusiveAVLTree%Remove(ItemNode)
        END IF
        ! check if remove the node successfully or not
        IF (Success) THEN
            ! return the node to the node's pool
            CALL Collection%NodePool%ReturnNode(ItemNode)
        ELSE
            CALL Handle_ErrLevel('TreeSet_Remove', ModName, ErrWarning, &
                       'Check the Remove method of the intrusive tree for possible bug(s).')
        END IF
    END IF
    
    ! free working pointer
    NULLIFY(ItemNode)

    RETURN

END FUNCTION TreeSet_Remove

!******************************************************************************

FUNCTION TreeSet_Contain(Collection, Item) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified item is currently stored in a collection.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeSet), INTENT(INOUT)   :: Collection   !! collection
    CLASS(*),       INTENT(IN)      :: Item         !! item to be looked for
    !% flag indicating whether the specified item is found or not.
    tLogical                        :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Collection%IsEmpty()) THEN
        Found = FalseVal
    ELSE
        Found = Collection%FindItem(Item)
    END IF

    RETURN

END FUNCTION TreeSet_Contain

!******************************************************************************

SUBROUTINE TreeSet_UseAVLTree(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set the *IsRBTree* component to false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object
    CLASS(TreeSet), INTENT(INOUT)   :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Collection%IsRBTree = FalseVal

    RETURN

END SUBROUTINE TreeSet_UseAVLTree

!******************************************************************************

SUBROUTINE TreeSet_UseRBTree(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set the *IsRBTree* component to true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object
    CLASS(TreeSet), INTENT(INOUT)   :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Collection%IsRBTree = TrueVal

    RETURN

END SUBROUTINE TreeSet_UseRBTree

!******************************************************************************

END MODULE MClass_TreeSet

!******************************************************************************
