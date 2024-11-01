
MODULE MClass_TreeComparable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *TreeComparable* type and its related helper type and routines.
!   The *TreeComparable* type is a container type representing an ordered symbol table, which
!   is a container that associates a *value* with a *key* where keys are stored in a sorted
!   order.  It employs a balanced binary-search-tree (BST) implementation to provide common
!   operations for an ordered symbol table.  As an ordered symbol table, the *TreeComparable*
!   type uses the *Comparable* derived type to store both the keys and their associated values.
!   Unlike other tree-based containers, the *TreeComparable* type uses a user-defined type in
!   the *Comparable* class to represent a key-value pair and requires only one argument (instead
!   of two) when inserting or retrieving the key and its associated value.  <br>
!   It is important to note that a user must be careful when implementing a *user-defined concrete*
!   subtype of the *Comparable* type.  The *TreeComparable* type employs the assignment statement
!   copy data of the key-value object and it utilizes the relational operators (e.g. ==, > and <)
!   to compare keys of the key-value objects.  This implies that the user should implement the 
!   deferred *CopyAssign* procedure where both key and value components are copied from the source
!   object to the destination object whereas, when implemented, the deferred *CompareTo* procedure
!   should be dependent on its key component only.  It is also worth mentioning that only one
!   user-defined type in the *Comparable* class should be used for one container instance.  If
!   more than one types are inserted into the container, they should all be able to compare to
!   other types.  In this case, additionally, the user must implement the *CopyAssign* procedure
!   in a way that allows one type to make a copy of another type.  This indicates that all these
!   inserted types must be subtypes of one parent type.  Otherwise, the *TreeComparable* type would
!   not operate properly and the user will not be able to correctly retrieve a key-value pair from
!   the container. <br>
!   As a symbol table, the *TreeComparable* type does not allow duplicated keys.  Therefore,
!   if an inserted key-value pair is equal to a key-value pair stored in the table, the stored
!   pair is replaced by the inserted one.  Technically, the *TreeComparable* type employs a
!   left-leaning red-black (RB) tree as the balanced BST. <br>
!   See the <a href="../module/mbase_balancedtrees.html">MBase_BalancedTrees</a> module
!   for an overview of a *balanced-tree-based* type.  A user may use the *MBase_BalancedTrees*
!   module instead of using this module directly. <br>
!   See the <a href="../module/mclass_treetable.html">MClass_TreeTable</a> module for a balanced
!   tree container type that is functionally similar to the *TreeComparable* type but utilizes
!   a different implementation.  Also, unlike the *TreeComparable* type, the *TreeTable* type
!   is designed as a generic ordered symbol table that allows keys with various types to be
!   inserted into the table. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MemHandlers
    USE MBase_ErrHandlers
    USE MBase_SIntUtil,       ONLY: ToChar => ToDecStrSigned
    USE MBase_DoublyLinkedLists
    USE MClass_Comparable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: TreeComparable

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define tComparable         CLASS(Comparable)
#define QueueKeyVal         ListObject

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_TreeComparable'

    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,  PARAMETER     :: MsgLen = 128
    ! traverse flags
    tSInt32,  PARAMETER     :: Inorder   = 1
    tSInt32,  PARAMETER     :: Preorder  = 2
    tSInt32,  PARAMETER     :: Postorder = 3
    ! color flags
    tLogical, PARAMETER     :: Red   = TrueVal
    tLogical, PARAMETER     :: Black = FalseVal

!** DERIVED TYPE DEFINITIONS
    !> *RBNode* is a red-black tree node type that consists of a key, a value and two
    !   pointers of the node type.  The  *RBNode* type is a private type.
   TYPE RBNode
        PRIVATE
        ! 'KeyVal' represents a key-value pair stored in the symbol table.  It is
        ! a derived type in the *Comparable* class that can be compared and sorted.
        tComparable, ALLOCATABLE    :: KeyVal
        ! pointer to the left node (or subtree)
        TYPE(RBNode),    POINTER    :: Left  => NULL()
        ! pointer to the right node (or subtree)
        TYPE(RBNode),    POINTER    :: Right => NULL()
        ! number of nodes in subtree rooted by this node
        tIndex                      :: Size = 0_kIndex
        ! color of parent node
        tLogical                    :: Color = Black
    CONTAINS
        PRIVATE
        PROCEDURE   :: SetNew       => BSTNode_SetNewKeyNValue
        PROCEDURE   :: Destruct     => BSTNode_Destructor
    END TYPE RBNode
    !> *TreeComparable* is a container type that employs a left-leaning red-black (RB) tree
    !   implementation to provide common operations for an ordered symbol table.
    TYPE TreeComparable
        PRIVATE
        ! pointer to the root node (topmost item) of the tree
        TYPE(RBNode), POINTER :: Root   => NULL()
        ! pointer to the current item (or node) used for iteration purpose
        TYPE(RBNode), POINTER :: Cursor => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: BSTree_Destructor_I
        PROCEDURE, PRIVATE  :: BSTree_Destructor_II
        PROCEDURE, PRIVATE  :: BSTree_Traverse_I
        PROCEDURE, PRIVATE  :: BSTree_Traverse_II
        PROCEDURE, PRIVATE  :: BSTree_GetKeys_Range
        PROCEDURE, PRIVATE  :: BSTree_GetKeys_All
        PROCEDURE, PRIVATE  :: IsRankConsistent => BSTree_IsRankConsistent
        PROCEDURE, PRIVATE  :: IsSizeConsistent => BSTree_IsSizeConsistent
        PROCEDURE, PRIVATE  :: IsBSTree         => BSTree_IsBinarySearchTree
        PROCEDURE, PRIVATE  :: Is23Tree         => BSTree_Is23Tree
        PROCEDURE, PRIVATE  :: IsBalanced       => BSTree_IsBalanced
        GENERIC,   PRIVATE  :: Traverse         => BSTree_Traverse_I, BSTree_Traverse_II
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        ! -----     constructor and destructor procedures   -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        ! **Purpose**:  To construct a tree from an array of key-value pairs.  <br>
        !  **Usage**: <br>
        !   --->    CALL Tree%Construct(10, KeyValArr)
        PROCEDURE   :: Construct        => BSTree_ConstructByArray
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all key-value pairs from the tree and optionally to
        !                retrieve stored key-value pairs. <br>
        !  **Usage**: <br>
        !   --->    CALL Tree%Destruct() <br>
        !   --->    CALL Tree%Destruct(KeyValQueue, ValQueue) <br>
        GENERIC     :: Destruct         => BSTree_Destructor_I,  BSTree_Destructor_II
        ! -------------------------------------------------------
        ! -----         adding and removing procedures      -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key-value pair to the tree. <br>
        !  **Usage**: <br>
        !   --->    CALL Tree%Insert(KeyVal) <br>
        PROCEDURE   :: Insert           => BSTree_Insert
        !> **Type-Bound Subroutine**: Remove <br>
        !  **Purpose**:  To remove the specified key (and its associated value) from the tree
        !       and return a flag indicating whether the key-value pair is successfully removed
        !       or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Remove(KeyVal) <br>
        !   --->    IF (.NOT.Tree%Remove(KeyVal)) DoSomething <br>
        PROCEDURE   :: Remove           => BSTree_Remove
        !> **Type-Bound Subroutine**: RemoveMin <br>
        !  **Purpose**:  To remove the smallest key (and its associated value) from the tree
        !       and return a flag indicating whether the key-value pair is successfully removed
        !       or not.  Optionally, the smallest key and its associated value can be retrieved. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%RemoveMin() <br>
        !   --->    IF (.NOT.Tree%RemoveMin(KeyVal)) DoSomething <br>
        PROCEDURE   :: RemoveMin        => BSTree_RemoveMin
        !> **Type-Bound Subroutine**: RemoveMax <br>
        !  **Purpose**:  To remove the largest key (and its associated value) from the tree
        !       and return a flag indicating whether the key-value pair is successfully removed
        !       or not.  Optionally, the largest key and its associated value can be retrieved. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%RemoveMax() <br>
        !   --->    IF (.NOT.Tree%RemoveMax(KeyVal)) DoSomething <br>
        PROCEDURE   :: RemoveMax        => BSTree_RemoveMax
        ! -------------------------------------------------------
        ! -----           tree-traversing procedures        -----
        ! -------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start an iteration (at a node with the smallest key) and return a flag
        !                indicating whether the tree is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Tree%StartFirst() <br>
        !   --->    IsEmpty = Tree%StartFirst(FirstKeyVal) <br>
        PROCEDURE   :: StartFirst       => BSTree_Move2First
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next key-value pair and return a flag indicating whether
        !                the cursor pointer has reached the end of the tree or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Tree%MoveForward() <br>
        !   --->    IsTheEnd = Tree%MoveForward(NextKeyVal) <br>
        PROCEDURE   :: MoveForward      => BSTree_Move2NextPair
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start an iteration in a reversed order (at a node with the largest key)
        !                and return a flag indicating whether the tree is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Tree%StartLast() <br>
        !   --->    IsEmpty = Tree%StartLast(LastKeyVal) <br>
        PROCEDURE   :: StartLast        => BSTree_Move2Last
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move to the previous key-value pair and return a flag indicating whether
        !                the cursor pointer has reached the end of the tree or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Tree%MoveBackward() <br>
        !   --->    IsTheEnd = Tree%MoveBackward(PrevKeyVal) <br>
        PROCEDURE   :: MoveBackward     => BSTree_Move2PrevPair
        ! -------------------------------------------------------
        ! -----               inquiry procedures            -----
        ! -------------------------------------------------------
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the tree is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%IsEmpty() <br>
        !   --->    IF (.NOT.Tree%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty          => BSTree_IsEmpty
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified key in the tree.  Return true if the specified key
        !                is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Contain(KeyVal) <br>
        !   --->    IF (.NOT.Tree%Contain(KeyVal)) DoSomething
        PROCEDURE   :: Contain          => BSTree_FindKey
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get size of the tree (the number of key-value pair stored in the tree). <br>
        !  **Usage**: <br>
        !   --->    TreeSize = Tree%GetSize()
        PROCEDURE   :: GetSize          => BSTree_GetSize
        !> **Type-Bound Function**: GetRangeSize <br>
        !  **Purpose**:  To return the number of keys between *KeyLo* (inclusive)
        !                and *KeyHi* (inclusive). <br>
        !  **Usage**: <br>
        !   --->    RangeSize = Tree%GetRangeSize(KeyValLo, KeyValHi)
        PROCEDURE   :: GetRangeSize     => BSTree_GetSize_Range
        !> **Type-Bound Function**: GetMinKey <br>
        !  **Purpose**:  To get the smallest key (and a value associated with it) in the tree.
        !                Also, return a flag indicating whether the key is successfully retrieved
        !                or not.  If the tree is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%GetMinKey(KeyVal) <br>
        !   --->    IF (.NOT.Tree%GetMinKey(KeyVal)) DoSomething
        PROCEDURE   :: GetMinKey        => BSTree_GetSmallestKey
        !> **Type-Bound Function**: GetMaxKey <br>
        !  **Purpose**:  To get the largest key (and a value associated with it) in the tree.
        !                Also, return a flag indicating whether the key is successfully retrieved
        !                or not.  If the tree is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%GetMaxKey(KeyVal) <br>
        !   --->    IF (.NOT.Tree%GetMaxKey(KeyVal)) DoSomething
        PROCEDURE   :: GetMaxKey        => BSTree_GetLargestKey
        !> **Type-Bound Subroutine**: GetKeys <br>
        !  **Purpose**:  To return all keys (in the tree or in the specified range) and
        !                their associated values. <br>
        !  **Usage**: <br>
        !   ! return all keys in the tree <br>
        !   --->    CALL Tree%GetKeys(KeyVals) <br>
        !   ! return all keys in the specified range <br>
        !   --->    CALL Tree%GetKeys(LoKeyVal, HiKeyVal, KeyVals) <br>
        GENERIC     :: GetKeys          => BSTree_GetKeys_Range, BSTree_GetKeys_All
        !> **Type-Bound Function**: Floor <br>
        !  **Purpose**:  To get the largest key (and a value associated with it) in the tree
        !                less than or equal to the given key.  Also, return a flag indicating
        !                whether the floor key is successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Floor(KeyValIn, KeyValOut) <br>
        !   --->    IF (.NOT.Tree%Floor(KeyValIn, KeyValOut)) DoSomething
        PROCEDURE   :: Floor            => BSTree_Floor
        !> **Type-Bound Function**: Ceiling <br>
        !  **Purpose**:  To get the smallest key (and a value associated with it) in the tree
        !                greater than or equal to the given key.  Also, return a flag indicating
        !                whether the ceiling key is successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Ceiling(KeyValIn, KeyValOut) <br>
        !   --->    IF (.NOT.Tree%Ceiling(KeyValIn, KeyValOut)) DoSomething
        PROCEDURE   :: Ceiling          => BSTree_Ceiling
        !> **Type-Bound Subroutine**: Select <br>
        !  **Purpose**:  To get the key (and its associated value) of the specified rank where the
        !                applicable range of rank is between 0 and TableSize-1. Also, return a flag
        !                indicating whether the ranked key is successfully retrieved or not. <br>
        !   --->    Flag = Tree%Select(Rank, KeyVal) <br>
        !   --->    IF (.NOT.Tree%Select(Rank, KeyVal)) DoSomething
        PROCEDURE   :: Select           => BSTree_Select
        !> **Type-Bound Function**: GetRank <br>
        !  **Purpose**:  To return the number of keys in the tree strictly less than the given key. <br>
        !  **Usage**: <br>
        !   --->    KeyRank = Tree%GetRank(KeyVal)
        PROCEDURE   :: GetRank          => BSTree_Rank
        !> **Type-Bound Function**: Check <br>
        !  **Purpose**:  To check integrity of the BST data structure. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Check() <br>
        !   --->    IF (.NOT.Tree%Check(ErrMsg)) DoSomething
        PROCEDURE   :: Check            => BSTree_CheckBST
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the tree.
        FINAL       :: BSTree_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE TreeComparable
    
!** INTERFACE DEFINITIONS:
    ! abstract interfaces
    ABSTRACT INTERFACE
        !> IteratureFunc is a user-suppled procedure used to traverse the tree
        !  in order to get the tree's keys and values.
        FUNCTION IteratorFunc(KeyVal,Done) RESULT(ErrStat)
            IMPORT
            tComparable, INTENT(IN)     :: KeyVal   !! key-value pair
            tLogical,    INTENT(INOUT)  :: Done
            !^ on input, Done is set to .FALSE. <br>
            !  on exit, set it to .TRUE. if user want to stop the tree traversing. <br>
            tLogical                    :: ErrStat  ! true if error occurred in the user routine
        END FUNCTION IteratorFunc
        !> IteratorLocal is a procedure used (locally in this module) to traverse
        !  the list in order to get the list's node.
        FUNCTION IteratorLocal(Node,Done) RESULT(ErrStat)
            IMPORT
            TYPE(RBNode), INTENT(IN)    :: Node     !! node
            tLogical,     INTENT(INOUT) :: Done
            !^ on input, Done is set to .FALSE. <br>
            !  on exit, set it to .TRUE. if user want to stop the tree traversing. <br>
            tLogical                    :: ErrStat  !! true if error occurred in the user routine
        END FUNCTION IteratorLocal
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   Type-Bound Procedures for RBNode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE BSTNode_SetNewKeyNValue(Node,KeyVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set new key-value pair.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RBNode), INTENT(INOUT)    :: Node     !! RBNode object
    tComparable,   INTENT(IN)       :: KeyVal   !! key-value pair

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(Node%KeyVal, SOURCE=KeyVal)
    Node%Size  = 1_kIndex
    Node%Color = Red
       
    RETURN

END SUBROUTINE BSTNode_SetNewKeyNValue

!******************************************************************************

SUBROUTINE BSTNode_Destructor(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct RBNode object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RBNode), INTENT(INOUT)    :: Node     !! RBNode object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Node%Size = 0
    Node%Color = Black
    CALL Node%KeyVal%MemFree()
    DEALLOCATE(Node%KeyVal)
    NULLIFY(Node%Left)
    NULLIFY(Node%Right)
       
    RETURN

END SUBROUTINE BSTNode_Destructor

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Auxiliary Procedures
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

RECURSIVE FUNCTION AddKeyNValue(InNode, KeyVal) RESULT(OutNode)
    
!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the given key-value pair into the tree.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(INOUT)    :: InNode   !! input node (root node of (sub)tree)
    tComparable,           INTENT(IN)       :: KeyVal   !! key-value pair
    TYPE(RBNode), POINTER                   :: OutNode  !! output node (new root node of (sub)tree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether the input node is null or not
    IF (.NOT.ASSOCIATED(InNode)) THEN
        ! create new node
        CALL CreateNewNode(KeyVal, OutNode)
        RETURN
    END IF
        
    ! compare the specified key with the node's key
    IF (KeyVal < InNode%KeyVal) THEN
                
        ! add the specified key-value pair to the left subtree
        InNode%Left => AddKeyNValue(InNode%Left, KeyVal)
            
    ELSEIF (KeyVal > InNode%KeyVal) THEN
                
        ! add the specified key-value pair to the right subtree
        InNode%Right => AddKeyNValue(InNode%Right, KeyVal)
            
    ELSE
                
        ! replace key-value pair
        InNode%KeyVal = KeyVal
        
    END IF
            
    ! fix-up any right-leaning links
    IF (IsRed(InNode%Right).AND..NOT.IsRed(InNode%Left)) InNode => LeftRotate(InNode)
    IF (IsRed(InNode%Left)) THEN
        IF (IsRed(InNode%Left%Left)) THEN
            InNode => RightRotate(InNode)
        END IF
    END IF
    IF (IsRed(InNode%Left).AND.IsRed(InNode%Right)) CALL FlipColor(InNode)
            
    ! update number of nodes in subtree rooted by the output node
    InNode%Size = NodeSize(InNode%Left) + NodeSize(InNode%Right) + 1
        
    ! set output node
    OutNode => InNode
        
    RETURN

END FUNCTION AddKeyNValue

!******************************************************************************

SUBROUTINE CreateNewNode(KeyVal, Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a new node and then set its key-value pair.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tComparable,           INTENT(IN)   :: KeyVal   !! key-value pair to be set to the node
    TYPE(RBNode), POINTER, INTENT(OUT)  :: Node     !! new node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW
        
    ! allocate new node
    ALLOCATE(Node, STAT=AllocStat, ERRMSG=AllocMsg)
        
    ! check allocation status and report error if necessary
    CALL Handle_ErrAlloc('CreateNewNode', ModName, AllocMsg, AllocStat)
        
    ! set the key-value pair
    CALL Node%SetNew(KeyVal)
        
    RETURN

END SUBROUTINE CreateNewNode

!******************************************************************************

RECURSIVE SUBROUTINE Find_Inorder_Predecessor(Root, KeyVal, PrvNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To search for the previous node in an inorder traversal of the tree
    !  of the specified node given by its key. <br>
    !  Note: The first call of this routine should provide the root of the
    !        tree as an input node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(IN)       :: Root     !! input node
    tComparable,           INTENT(IN)       :: KeyVal   !! key of the node
    TYPE(RBNode), POINTER, INTENT(INOUT)    :: PrvNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER :: Temp => NULL()

! FLOW
        
    ! base case
    IF (.NOT.ASSOCIATED(Root)) RETURN
        
    ! If key is equal to the key of the root
    IF (KeyVal == Root%KeyVal) THEN
        ! the maximum value in the left subtree is predecessor node
        IF (ASSOCIATED(Root%Left)) THEN
            ! iteratively search the rightmost node of the left subtree
            Temp => Root%Left
            DO WHILE (ASSOCIATED(Temp%Right))
                Temp => Temp%Right
            END DO
            PrvNode => Temp
            NULLIFY(Temp)
        END IF
        RETURN
    END IF
        
    IF (KeyVal < Root%KeyVal) THEN
        ! If key is smaller than the key of the root, go to left subtree
        CALL Find_Inorder_Predecessor(Root%Left, KeyVal, PrvNode)
    ELSE
        ! Otherwise, go to right subtree
        PrvNode => Root
        CALL Find_Inorder_Predecessor(Root%Right, KeyVal, PrvNode)
    END IF

    RETURN

END SUBROUTINE Find_Inorder_Predecessor

!******************************************************************************

RECURSIVE SUBROUTINE Find_Inorder_Successor(Root, KeyVal, NxtNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To search for the next node in an inorder traversal of the tree
    !  of the specified node given by its key.
    !  Note: The first call of this routine should provide the root of
    !        the tree as an input node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(IN)       :: Root     !! input node
    tComparable,           INTENT(IN)       :: KeyVal   !! key of the node
    TYPE(RBNode), POINTER, INTENT(INOUT)    :: NxtNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER :: Temp => NULL()

! FLOW
        
    ! base case
    IF (.NOT.ASSOCIATED(Root)) RETURN
        
    ! If key is equal to the key of the root
    IF (KeyVal == Root%KeyVal) THEN
        ! the minimum value in the right subtree is the successor node
        IF (ASSOCIATED(Root%Right)) THEN
            ! iteratively search the leftmost node of the right subtree
            Temp => Root%Right
            DO WHILE (ASSOCIATED(Temp%Left))
                Temp => Temp%Left
            END DO
            NxtNode => Temp
            NULLIFY(Temp)
        END IF
        RETURN
    END IF
        
    IF (KeyVal < Root%KeyVal) THEN
        ! If key is smaller than the key of the root, go to left subtree
        NxtNode => Root
        CALL Find_Inorder_Successor(Root%Left, KeyVal, NxtNode)
    ELSE
        ! Otherwise, go to right subtree
        CALL Find_Inorder_Successor(Root%Right, KeyVal, NxtNode)
    END IF

    RETURN

END SUBROUTINE Find_Inorder_Successor

!******************************************************************************

SUBROUTINE Find_Inorder_PrevNode(Node, Root, PrvNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To search for the previous node in an inorder traversal of the tree
    !  of the specified input node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(IN)   :: Node     !! input node
    TYPE(RBNode), POINTER, INTENT(IN)   :: Root     !! root node
    TYPE(RBNode), POINTER, INTENT(OUT)  :: PrvNode  !! previous node of input node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER :: CurRoot => NULL()

! FLOW
        
    ! If left subtree of node is not NULL, then PrvNode lies in left subtree.
    ! Thus, go to left subtree and return the node with maximum key value
    ! in the left subtree.
    IF (ASSOCIATED(Node%Left)) THEN
        CALL Find_MaxKeyNode(Node%Left, PrvNode)
        RETURN
    END IF
        
    ! start from root and iteratively search for the previous node down the tree
    PrvNode => NULL()
    CurRoot => Root
    DO WHILE (ASSOCIATED(CurRoot))
        IF (Node%KeyVal < CurRoot%KeyVal) THEN
            ! the previous node should be on the left subtree
            CurRoot => CurRoot%Left
        ELSEIF (Node%KeyVal > CurRoot%KeyVal) THEN
            ! the previous node should be on the right subtree
            PrvNode => CurRoot
            CurRoot => CurRoot%Right
        ELSE
            ! the previous node found
            EXIT
        END IF
    END DO

    RETURN

END SUBROUTINE Find_Inorder_PrevNode

!******************************************************************************

SUBROUTINE Find_Inorder_NextNode(Node, Root, NxtNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To search for the next node in an inorder traversal of the tree
    !  of the specified input node.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(IN)   :: Node     !! input node
    TYPE(RBNode), POINTER, INTENT(IN)   :: Root     !! root node
    TYPE(RBNode), POINTER, INTENT(OUT)  :: NxtNode  !! next node of input node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER :: CurRoot => NULL()

! FLOW
        
    ! If right subtree of node is not NULL, then NxtNode lies in right subtree.
    ! Thus, go to right subtree and return the node with minimum key value
    ! in the right subtree.
    IF (ASSOCIATED(Node%Right)) THEN
        CALL Find_MinKeyNode(Node%Right, NxtNode)
        RETURN
    END IF
        
    ! start from root and search for the next node down the tree
    NxtNode => NULL()
    CurRoot => Root
    DO WHILE (ASSOCIATED(CurRoot))
        IF (Node%KeyVal < CurRoot%KeyVal) THEN
            ! the next node should be on the left subtree
            NxtNode => CurRoot
            CurRoot => CurRoot%Left
        ELSEIF (Node%KeyVal > CurRoot%KeyVal) THEN
            ! the next node should be on the right subtree
            CurRoot => CurRoot%Right
        ELSE
            ! the next node found
            EXIT
        END IF
    END DO

    RETURN

END SUBROUTINE Find_Inorder_NextNode

!******************************************************************************

SUBROUTINE Find_MinKeyNode(Node, MinNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the leftmost leaf of the given node.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(IN)   :: Node     !! input node
    TYPE(RBNode), POINTER, INTENT(OUT)  :: MinNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! set pointer for working/output node
    MinNode => Node
        
    ! loop down to find the leftmost leaf
    DO WHILE (ASSOCIATED(MinNode%Left))
        MinNode => MinNode%Left
    END DO

    RETURN

END SUBROUTINE Find_MinKeyNode

!******************************************************************************

SUBROUTINE Find_MaxKeyNode(Node, MaxNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the rightmost leaf of the given node.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(IN)   :: Node     !! input node
    TYPE(RBNode), POINTER, INTENT(OUT)  :: MaxNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! set pointer for working/output node
    MaxNode => Node
        
    ! loop down to find the rightmost leaf
    DO WHILE (ASSOCIATED(MaxNode%Right))
        MaxNode => MaxNode%Right
    END DO

    RETURN

END SUBROUTINE Find_MaxKeyNode

!******************************************************************************

RECURSIVE SUBROUTINE FindNode(InNode, KeyVal, OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To search for the node having the same key as the specified one.
    !  Return null if the node does not exist.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(IN)   :: InNode   !! input node
    tComparable,           INTENT(IN)   :: KeyVal   !! key to be looked for
    TYPE(RBNode), POINTER, INTENT(OUT)  :: OutNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        ! the node we are looking for does not exist
        OutNode => NULL()
    ELSEIF (KeyVal == InNode%KeyVal) THEN
        ! the node found
        OutNode => InNode
    ELSEIF (KeyVal < InNode%KeyVal) THEN
        ! the node should be on the left subtree
        CALL FindNode(InNode%Left, KeyVal, OutNode)
    ELSE
        ! the node should be on the right subtree
        CALL FindNode(InNode%Right, KeyVal, OutNode)
    END IF

    RETURN

END SUBROUTINE FindNode

!******************************************************************************

RECURSIVE SUBROUTINE Find_FloorNode(InNode, KeyVal, OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the node containing largest key in the tree
    !  less than or equal to the specified key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(IN)   :: InNode   !! input node
    tComparable,           INTENT(IN)   :: KeyVal   !! key
    TYPE(RBNode), POINTER, INTENT(OUT)  :: OutNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        ! the node we are looking for does not exist
        OutNode => NULL()
    ELSEIF (KeyVal == InNode%KeyVal) THEN
        ! the node found
        OutNode => InNode
    ELSEIF (KeyVal < InNode%KeyVal) THEN
        ! the node should be on the left subtree
        CALL Find_FloorNode(InNode%Left, KeyVal, OutNode)
    ELSE
        ! the node should be on the right subtree
        CALL Find_FloorNode(InNode%Right, KeyVal, OutNode)
        ! if output node is null, return the input node instead
        IF (.NOT.ASSOCIATED(OutNode)) OutNode => InNode
    END IF

    RETURN

END SUBROUTINE Find_FloorNode

!******************************************************************************

RECURSIVE SUBROUTINE Find_CeilingNode(InNode, KeyVal, OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the node containing smallest key in the tree
    !  greater than or equal to the specified key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(IN)   :: InNode   !! input node
    tComparable,           INTENT(IN)   :: KeyVal   !! key
    TYPE(RBNode), POINTER, INTENT(OUT)  :: OutNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        ! the node we are looking for does not exist
        OutNode => NULL()
    ELSEIF (KeyVal == InNode%KeyVal) THEN
        ! the node found
        OutNode => InNode
    ELSEIF (KeyVal < InNode%KeyVal) THEN
        ! the node should be on the left subtree
        CALL Find_CeilingNode(InNode%Left, KeyVal, OutNode)
        ! if output node is null, return the input node instead
        IF (.NOT.ASSOCIATED(OutNode)) OutNode => InNode
    ELSE
        ! the node should be on the right subtree
        CALL Find_CeilingNode(InNode%Right, KeyVal, OutNode)
    END IF

    RETURN

END SUBROUTINE Find_CeilingNode

!******************************************************************************

RECURSIVE FUNCTION SelectKey(InNode, Rank, KeyVal) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return key in the subtree rooted at InNode of given rank. <br>
    !  Precondition: rank is in legal range.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(IN)   :: InNode   !! input node
    tIndex,                INTENT(IN)   :: Rank     !! rank of the key
    tComparable,           INTENT(OUT)  :: KeyVal   !! key of the given rank
    tLogical                            :: Found    !! true if the key found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: LeftSize

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        ! the node we are looking for does not exist
        Found = FalseVal
        RETURN
    END IF
        
    ! determine number of keys in the left subtree
    LeftSize = NodeSize(InNode%Left)
        
    ! find the node with a given rank
    IF (LeftSize > Rank) THEN
        ! the node should be on the left subtree
        Found = SelectKey(InNode%Left, Rank, KeyVal)
    ELSEIF (LeftSize < Rank) THEN
        ! the node should be on the right subtree
        Found = SelectKey(InNode%Right, Rank-LeftSize-1, KeyVal)
    ELSE
        ! the node is found
        Found  = TrueVal
        KeyVal = InNode%KeyVal
    END IF

    RETURN

END FUNCTION SelectKey

!******************************************************************************

RECURSIVE FUNCTION KeyRank(InNode, KeyVal) RESULT(Rank)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the rank of the given key (i.e. the number
    !  of keys in the tree that are less than the given key).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(IN)   :: InNode   !! input node
    tComparable,           INTENT(IN)   :: KeyVal   !! key-value pair
    tIndex                              :: Rank     !! rank of the key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        Rank = 0
        RETURN
    END IF
        
    IF (KeyVal < InNode%KeyVal) THEN
        Rank = KeyRank(InNode%Left, KeyVal)
    ELSEIF (KeyVal > InNode%KeyVal) THEN
        Rank = KeyRank(InNode%Right, KeyVal) + NodeSize(InNode%Left) + 1
    ELSE
        Rank = NodeSize(InNode%Left)
    END IF

    RETURN

END FUNCTION KeyRank

!******************************************************************************

RECURSIVE FUNCTION DeleteNode(InNode, KeyVal) RESULT(OutNode)
    
!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the specified key and its associated value from the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(INOUT)    :: InNode   !! input node (root node of (sub)tree)
    tComparable,           INTENT(IN)       :: KeyVal   !! key-value pair
    TYPE(RBNode), POINTER                   :: OutNode  !! output node (new root node of (sub)tree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER :: MinNode => NULL()

! FLOW
        
    ! return if the (sub)tree is empty
    IF (.NOT.ASSOCIATED(InNode)) THEN
        OutNode => NULL()
        RETURN
    END IF

    ! find the node to be deleted
    IF (KeyVal < InNode%KeyVal) THEN
            
        ! check whether to move red node to the left
        IF (.NOT.IsRed(InNode%Left)) THEN
            IF (ASSOCIATED(InNode%Left)) THEN
                IF (.NOT.IsRed(InNode%Left%Left)) InNode => MoveRedLeft(InNode)
            END IF
        END IF
            
        ! find the node on the left subtree
        InNode%Left => DeleteNode(InNode%Left, KeyVal)
            
    ELSE
            
        ! check whether to perform right rotation
        IF (IsRed(InNode%Left)) InNode => RightRotate(InNode)

        ! check whether the key is found with the null right child of the key node
        IF ((KeyVal == InNode%KeyVal).AND.(.NOT.ASSOCIATED(InNode%Right))) THEN

            ! delete node from the tree
            CALL InNode%Destruct()
            CALL FreeTreeNode(InNode)
                
            ! reset the input node
            InNode => NULL()
                
            ! set output node and return
            OutNode => InNode
            RETURN
                
        END IF
        
        ! check whether to move red node to the right
        IF (.NOT.IsRed(InNode%Right)) THEN
            IF (ASSOCIATED(InNode%Right)) THEN
                IF (.NOT.IsRed(InNode%Right%Left)) InNode => MoveRedRight(InNode)
            END IF
        END IF
            
        IF (KeyVal == InNode%KeyVal) THEN
                
            ! find inorder successor of InNode
            CALL Find_MinKeyNode(InNode%Right, MinNode)

            ! set the inorder successor data at the position of the node supposedly to be deleted
            InNode%KeyVal = MinNode%KeyVal
                
            ! actually delete the inorder successor node instead
            InNode%Right => DeleteMinKeyNode(InNode%Right)
                
            ! free working pointers
            NULLIFY(MinNode)
        
        ELSE
                
            ! find the node on the right subtree
            InNode%Right => DeleteNode(InNode%Right, KeyVal)
            
        END IF

    END IF

    ! rebalance
    InNode => Rebalance(InNode)
        
    ! set output node and return
    OutNode => InNode
                
    RETURN

END FUNCTION DeleteNode

!******************************************************************************

RECURSIVE FUNCTION DeleteMinKeyNode(InNode, KeyVal) RESULT(OutNode)
    
!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the smallest key and its associated value from the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(INOUT)    :: InNode   !! input node (root node of (sub)tree)
    tComparable, OPTIONAL, INTENT(OUT)      :: KeyVal   !! smallest key and its associated value
    TYPE(RBNode), POINTER                   :: OutNode  !! output node (new root node of (sub)tree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode%Left)) THEN
                
        ! smallest key found so get optional output if requested
        IF (PRESENT(KeyVal)) KeyVal = InNode%KeyVal
            
        ! delete node from the tree
        CALL InNode%Destruct()
        CALL FreeTreeNode(InNode)
                
        ! reset the input node
        InNode => NULL()
                
    ELSE
            
        ! check whether to move red node to the left
        IF (.NOT.IsRed(InNode%Left)) THEN
            IF (ASSOCIATED(InNode%Left)) THEN
                IF (.NOT.IsRed(InNode%Left%Left)) InNode => MoveRedLeft(InNode)
            END IF
        END IF

        ! find the node on the left subtree
        InNode%Left => DeleteMinKeyNode(InNode%Left, KeyVal)
            
        ! rebalance
        InNode => Rebalance(InNode)
                
    END IF
            
    ! set output node
    OutNode => InNode

    RETURN

END FUNCTION DeleteMinKeyNode

!******************************************************************************

RECURSIVE FUNCTION DeleteMaxKeyNode(InNode, KeyVal) RESULT(OutNode)
    
!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the largest key and its associated value from the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(INOUT)    :: InNode   !! input node (root node of (sub)tree)
    tComparable, OPTIONAL, INTENT(OUT)      :: KeyVal   !! largest key and its associated value
    TYPE(RBNode), POINTER                   :: OutNode  !! output node (new root node of (sub)tree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! check whether to perform right rotation
    IF (IsRed(InNode%Left)) InNode => RightRotate(InNode)

    IF (.NOT.ASSOCIATED(InNode%Right)) THEN
            
        ! largest key found so get optional output if requested
        IF (PRESENT(KeyVal)) KeyVal = InNode%KeyVal
            
        ! delete node from the tree
        CALL InNode%Destruct()
        CALL FreeTreeNode(InNode)
            
        ! reset the input node
        InNode => NULL()
                
    ELSE
            
        ! check whether to move red node to the right
        IF (.NOT.IsRed(InNode%Right)) THEN
            IF (ASSOCIATED(InNode%Right)) THEN
                IF (.NOT.IsRed(InNode%Right%Left)) InNode => MoveRedRight(InNode)
            END IF
        END IF

        ! find the node on the right subtree
        InNode%Right => DeleteMaxKeyNode(InNode%Right, KeyVal)
            
        ! rebalance
        InNode => Rebalance(InNode)

    END IF
        
    ! set output node
    OutNode => InNode

    RETURN

END FUNCTION DeleteMaxKeyNode

!******************************************************************************

FUNCTION NodeSize(Node) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get number of nodes in subtree rooted by this node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(IN)   :: Node !! RBNode object
    tIndex                              :: Size !! number of nodes in subtree rooted by this node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (ASSOCIATED(Node)) THEN
        Size = Node%Size
    ELSE
        Size = 0_kIndex
    END IF
       
    RETURN

END FUNCTION NodeSize

!******************************************************************************

FUNCTION IsRed(Node) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the color of this node is red.
    !  Return FalseVal if the node is not associated.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(IN)   :: Node !! RBNode object
    tLogical                            :: Flag !! true if the color is red

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (ASSOCIATED(Node)) THEN
        Flag = (Node%Color .EQV. Red)
    ELSE
        Flag = Black
    END IF
       
    RETURN

END FUNCTION IsRed

!******************************************************************************

FUNCTION RightRotate(InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make a left-leaning link lean to the right.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(INOUT)    :: InNode   !! input node (root of input subtree)
    TYPE(RBNode), POINTER                   :: OutNode  !! output node (root of output subtree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER   :: NodeA => NULL()
    TYPE(RBNode), POINTER   :: NodeB => NULL()
    TYPE(RBNode), POINTER   :: NodeC => NULL()

! FLOW
    
    ! for debugging purpose
    ASSERT(ASSOCIATED(InNode).AND.IsRed(InNode%Left), 'RotateRight')
        
    ! set working nodes
    NodeA => InNode
    NodeB => NodeA%Left
    NodeC => NodeB%Right
        
    ! perform right rotation
    NodeB%Right => NodeA
    NodeA%Left  => NodeC
        
    ! update the number of nodes in subtree rooted by NodeA and NodeB
    NodeB%Size = NodeA%Size
    NodeA%Size = NodeSize(NodeA%Left) + NodeSize(NodeA%Right) + 1
        
    ! update color
    NodeB%Color = NodeB%Right%Color
    NodeB%Right%Color = Red
        
    ! set output root
    OutNode => NodeB
        
    ! free pointers
    NULLIFY(NodeA)
    NULLIFY(NodeB)
    NULLIFY(NodeC)
        
    RETURN

END FUNCTION RightRotate

!******************************************************************************

FUNCTION LeftRotate(InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make a right-leaning link lean to the left.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(INOUT)    :: InNode   !! input node (root of input subtree)
    TYPE(RBNode), POINTER                   :: OutNode  !! output node (root of output subtree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER   :: NodeA => NULL()
    TYPE(RBNode), POINTER   :: NodeB => NULL()
    TYPE(RBNode), POINTER   :: NodeC => NULL()

! FLOW
    
    ! for debugging purpose
    ASSERT(ASSOCIATED(InNode).AND.IsRed(InNode%Right), 'RotateLeft')
        
    ! set working nodes
    NodeA => InNode
    NodeB => NodeA%Right
    NodeC => NodeB%Left
        
    ! perform left rotation
    NodeB%Left  => NodeA
    NodeA%Right => NodeC
        
    ! update the number of nodes in subtree rooted by NodeA and NodeB
    NodeB%Size = NodeA%Size
    NodeA%Size = NodeSize(NodeA%Left) + NodeSize(NodeA%Right) + 1
        
    ! update color
    NodeB%Color = NodeB%Left%Color
    NodeB%Left%Color = Red
        
    ! set output root
    OutNode => NodeB
        
    ! free pointers
    NULLIFY(NodeA)
    NULLIFY(NodeB)
    NULLIFY(NodeC)
        
    RETURN

END FUNCTION LeftRotate

!******************************************************************************

SUBROUTINE FlipColor(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To flip the colors of a node and its two children.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(INOUT)    :: Node   !! RBNode object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Node%Color       = .NOT.Node%Color
    Node%Left%Color  = .NOT.Node%Left%Color
    Node%Right%Color = .NOT.Node%Right%Color
       
    RETURN

END SUBROUTINE FlipColor

!******************************************************************************

FUNCTION MoveRedLeft(InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ Assuming that InNode is red and both InNode%Left and InNode%Left%Left
    !  are black, make InNode%Left or one of its children red.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(INOUT)    :: InNode   !! input node
    TYPE(RBNode), POINTER                   :: OutNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! for debugging purpose
    ASSERT(ASSOCIATED(InNode), 'MoveRedLeft')
    ASSERT(IsRed(InNode).AND..NOT.IsRed(InNode%Left), 'MoveRedLeft')
#ifdef DebugMode
    IF (ASSOCIATED(InNode%Left)) THEN
        ASSERT(.NOT.IsRed(InNode%Left%Left), 'MoveRedLeft')
    END IF
#endif
     
    ! move the input node to the left
    CALL FlipColor(InNode)
    IF (ASSOCIATED(InNode%Right)) THEN
        IF (IsRed(InNode%Right%Left)) THEN
            InNode%Right => RightRotate(InNode%Right)
            InNode => LeftRotate(InNode)
            CALL FlipColor(InNode)
        END IF
    END IF
        
    ! set output node
    OutNode => InNode
        
    RETURN

END FUNCTION MoveRedLeft

!******************************************************************************

FUNCTION MoveRedRight(InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ Assuming that InNode is red and both InNode%Right and InNode%Right%Left
    !  are black, make InNode%Right or one of its children red.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(INOUT)    :: InNode   !! input node
    TYPE(RBNode), POINTER                   :: OutNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! for debugging purpose
    ASSERT(ASSOCIATED(InNode), 'MoveRedRight')
    ASSERT(IsRed(InNode).AND..NOT.IsRed(InNode%Right), 'MoveRedRight')
#ifdef DebugMode
    IF (ASSOCIATED(InNode%Right)) THEN
        ASSERT(.NOT.IsRed(InNode%Right%Left), 'MoveRedRight')
    END IF
#endif
        
    ! move the input node to the right
    CALL FlipColor(InNode)
    IF (ASSOCIATED(InNode%Left)) THEN
        IF (IsRed(InNode%Left%Left)) THEN
            InNode => RightRotate(InNode)
            CALL FlipColor(InNode)
        END IF
    END IF

    ! set output node
    OutNode => InNode
        
    RETURN

END FUNCTION MoveRedRight

!******************************************************************************

FUNCTION Rebalance(InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To restore red-black tree invariant.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(INOUT)    :: InNode   !! input node
    TYPE(RBNode), POINTER                   :: OutNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! for debugging purpose
    ASSERT(ASSOCIATED(InNode), 'Rebalance')
        
    ! check whether to rotate left or not
    IF (IsRed(InNode%Right).AND..NOT.IsRed(InNode%Left)) InNode => LeftRotate(InNode)
        
    ! check whether to rotate right or not
    IF (IsRed(InNode%Left)) THEN
        ! note: if the left node is red, it must not be null
        IF (IsRed(InNode%Left%Left)) InNode => RightRotate(InNode)
    END IF
        
    ! check whether to flip color or not
    IF (IsRed(InNode%Left).AND.IsRed(InNode%Right)) CALL FlipColor(InNode)
        
    ! set output node size
    InNode%Size = NodeSize(InNode%Left) + NodeSize(InNode%Right) + 1
        
    ! set output node
    OutNode => InNode
        
    RETURN

END FUNCTION Rebalance

!******************************************************************************

SUBROUTINE FreeTreeNode(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To deallocate RBNode pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RBNode), POINTER, INTENT(INOUT)    :: Node   !! RBNode object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW
    
    IF (ASSOCIATED(Node)) THEN
        DEALLOCATE(Node, STAT=AllocStat, ERRMSG=AllocMsg)
        NULLIFY(Node)
        CALL Handle_ErrDealloc('FreeTreeNode', ModName, AllocMsg, AllocStat)
    END IF
       
    RETURN

END SUBROUTINE FreeTreeNode

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   Type-Bound Procedures for TreeComparable
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE BSTree_ConstructByArray(Tree, N, KeyVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a tree based on specified arrays.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree         !! tree
    tIndex,                INTENT(IN)       :: N            !! number of key-value pairs
    tComparable,           INTENT(IN)       :: KeyVal(N)    !! an array of key-value pairs

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW
        
    ! built tree from input arrays
    DO I = 1, N
        CALL Tree%Insert(KeyVal(I))
    END DO

    RETURN

END SUBROUTINE BSTree_ConstructByArray

!******************************************************************************

SUBROUTINE BSTree_Destructor_I(Tree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct a tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! traverse the tree and destroy all nodes
    CALL DestroyNode(Tree%Root)

    ! nullify pointers
    NULLIFY(Tree%Root)
    NULLIFY(Tree%Cursor)
        
    RETURN
        
CONTAINS

    RECURSIVE SUBROUTINE DestroyNode(Node)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To destroy the specified node.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RBNode), POINTER, INTENT(INOUT)    :: Node !! node
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! return if null
        IF (.NOT.ASSOCIATED(Node)) RETURN
            
        ! recursively remove left and right subtrees
        CALL DestroyNode(Node%Left)
        CALL DestroyNode(Node%Right)
            
        ! remove this node
        CALL Node%Destruct()
        CALL FreeTreeNode(Node)
            
        RETURN
            
    END SUBROUTINE DestroyNode
        
    !**************************************************************************

END SUBROUTINE BSTree_Destructor_I

!******************************************************************************

SUBROUTINE BSTree_Destructor_II(Tree, KeyValQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct a tree and get its pair data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    TYPE(QueueKeyVal),     INTENT(OUT)      :: KeyValQ  !! a queue of stored key-value pairs

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! traverse the tree to get keys and values
    CALL Tree%Traverse(Inorder, GetData)
        
    ! destroy all nodes
    CALL Tree%Destruct()
        
    RETURN
        
CONTAINS

    FUNCTION GetData(Node, Done) RESULT(ErrStat)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To get key-value pair of the specified node

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RBNode), INTENT(IN)    :: Node     !! node
        tLogical,     INTENT(INOUT) :: Done     !! on input, Done is set to FalseVal
                                                !! on exit, set it to TrueVal if user
                                                !!   want to stop the queue traversing.
        tLogical                    :: ErrStat  !! true if error occurred in iterator
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! set flag
        ErrStat = FalseVal
        Done = FalseVal
            
        ! add key-value pair to queues
        CALL KeyValQ%EnQueue(Node%KeyVal)

        RETURN
            
    END FUNCTION GetData
        
    !**************************************************************************
        
END SUBROUTINE BSTree_Destructor_II

!******************************************************************************

SUBROUTINE BSTree_Finalizer(Tree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(TreeComparable), INTENT(INOUT) :: Tree !! tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! destroy all nodes and free up memory
    CALL Tree%Destruct()
       
    RETURN

END SUBROUTINE BSTree_Finalizer

!******************************************************************************

SUBROUTINE BSTree_Insert(Tree, KeyVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert the given key-value pair into the tree.
    !  If the tree already contains the specified key, the
    !  old value is replaced with the new one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    tComparable,           INTENT(IN)       :: KeyVal   !! key-value pair to be inserted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! recursively search for place to insert the key-value pair
    Tree%Root => AddKeyNValue(Tree%Root, KeyVal)
    Tree%Root%Color = Black

    RETURN

END SUBROUTINE BSTree_Insert

!******************************************************************************

FUNCTION BSTree_Remove(Tree, KeyVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the specified key and its associated value from the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    tComparable,           INTENT(IN)       :: KeyVal   !! key-value pair
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_Remove', ModName, ErrWarning, &
                             'The binary search tree is empty.')
        RETURN
    END IF
        
    ! check if the tree contains the specified key
    IF (.NOT.Tree%Contain(KeyVal)) RETURN
        
    ! if both children of root are black, set root to red
    IF (.NOT.IsRed(Tree%Root%Left).AND..NOT.IsRed(Tree%Root%Right)) THEN
        Tree%Root%Color = Red
    END IF

    ! delete node with the given key
    Tree%Root => DeleteNode(Tree%Root, KeyVal)

    ! update color
    IF (.NOT.Tree%IsEmpty()) Tree%Root%Color = Black

    Flag = TrueVal

    RETURN

END FUNCTION BSTree_Remove

!******************************************************************************

FUNCTION BSTree_RemoveMin(Tree, KeyVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the smallest key and its associated value from the tree.
    !  Optionally, to retrieve the smallest key and its associated value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    tComparable, OPTIONAL, INTENT(OUT)      :: KeyVal   !! smallest key and its associated value
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_RemoveMin', ModName, ErrWarning, &
                            'The binary search tree is empty.')
        RETURN
    END IF
        
    ! if both children of root are black, set root to red
    IF (.NOT.IsRed(Tree%Root%Left).AND..NOT.IsRed(Tree%Root%Right)) THEN
        Tree%Root%Color = Red
    END IF

    ! delete node with smallest key
    Tree%Root => DeleteMinKeyNode(Tree%Root, KeyVal)
        
    ! update color
    IF (.NOT.Tree%IsEmpty()) Tree%Root%Color = Black

    Flag = TrueVal

    RETURN

END FUNCTION BSTree_RemoveMin

!******************************************************************************

FUNCTION BSTree_RemoveMax(Tree, KeyVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the largest key and its associated value from the tree.
    !  Optionally, to retrieve the largest key and its associated value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    tComparable, OPTIONAL, INTENT(OUT)      :: KeyVal   !! largest key and its associated value
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_RemoveMax', ModName, ErrWarning, &
                             'The binary search tree is empty.')
        RETURN
    END IF
        
    ! if both children of root are black, set root to red
    IF (.NOT.IsRed(Tree%Root%Left).AND..NOT.IsRed(Tree%Root%Right)) THEN
        Tree%Root%Color = Red
    END IF

    ! delete node with largest key
    Tree%Root => DeleteMaxKeyNode(Tree%Root, KeyVal)
        
    ! update color
    IF (.NOT.Tree%IsEmpty()) Tree%Root%Color = Black

    Flag = TrueVal

    RETURN

END FUNCTION BSTree_RemoveMax

!******************************************************************************

SUBROUTINE BSTree_Traverse_I(Tree, TraverseFlg, IterFunc)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To traverse the tree according to traversal flag.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree         !! tree
    tSInt32,               INTENT(IN)       :: TraverseFlg
    !^ traversal flag. <br>
    ! = 1 -> inorder. <br>
    ! = 2 -> preorder. <br>
    ! = 3 -> postorder. <br>
    PROCEDURE(IteratorLocal)                :: IterFunc     !! user-supplied routine

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Done

! FLOW
        
    Done = FalseVal
    SELECT CASE (TraverseFlg)
    CASE (Inorder)
        CALL Traverse_Inorder(Tree%Root, IterFunc, Done)
    CASE (Preorder)
        CALL Traverse_Preorder(Tree%Root, IterFunc, Done)
    CASE (Postorder)
        CALL Traverse_Postorder(Tree%Root, IterFunc, Done)
    END SELECT

    RETURN
        
CONTAINS

    RECURSIVE SUBROUTINE Traverse_Inorder(Root, IterFunc, Done)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform inorder traversal.
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RBNode), POINTER, INTENT(INOUT)    :: Root     !! root node of (sub)tree
        PROCEDURE(IteratorLocal)                :: IterFunc !! user-supplied routine
        tLogical,              INTENT(INOUT)    :: Done     !! true if want to stop the traversal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: ErrStat

    ! FLOW
        
        ! check whether the root is null or not
        IF (.NOT.ASSOCIATED(Root)) RETURN
        
        ! traverse the left subtree
        IF (.NOT.Done) CALL Traverse_Inorder(Root%Left, IterFunc, Done)
        
        ! call user-supplied routine to perform user-specified task for the current root
        IF (.NOT.Done) ErrStat = IterFunc(Root, Done)
        
        ! traverse the right subtree
        IF ((.NOT.Done).AND.(.NOT.ErrStat)) CALL Traverse_Inorder(Root%Right, IterFunc, Done)
        
        RETURN

    END SUBROUTINE Traverse_Inorder

    !**************************************************************************

    RECURSIVE SUBROUTINE Traverse_Preorder(Root, IterFunc, Done)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform preorder traversal.
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RBNode), POINTER, INTENT(INOUT)    :: Root     !! root node of (sub)tree
        PROCEDURE(IteratorLocal)                :: IterFunc !! user-supplied routine
        tLogical,              INTENT(INOUT)    :: Done     !! true if want to stop the traversal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: ErrStat

    ! FLOW
        
        ! check whether the root is null or not
        IF (.NOT.ASSOCIATED(Root)) RETURN
        
        ! call user-supplied routine to perform user-specified task for the current root
        ErrStat = IterFunc(Root, Done)
        
        IF ((.NOT.Done).AND.(.NOT.ErrStat)) THEN
        
            ! traverse the left subtree
            CALL Traverse_Preorder(Root%Left, IterFunc, Done)
        
            ! traverse the right subtree
            IF (.NOT.Done) CALL Traverse_Preorder(Root%Right, IterFunc, Done)

        END IF
        
        RETURN

    END SUBROUTINE Traverse_Preorder

    !**************************************************************************

    RECURSIVE SUBROUTINE Traverse_Postorder(Root, IterFunc, Done)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform postorder traversal.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RBNode), POINTER, INTENT(INOUT)    :: Root     !! root node of (sub)tree
        PROCEDURE(IteratorLocal)                :: IterFunc !! user-supplied routine
        tLogical,              INTENT(INOUT)    :: Done     !! true if want to stop the traversal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: ErrStat

    ! FLOW
        
        ! check whether the root is null or not
        IF (.NOT.ASSOCIATED(Root)) RETURN
        
        ! traverse the left subtree
        IF (.NOT.Done) CALL Traverse_Postorder(Root%Left, IterFunc, Done)
        
        ! traverse the right subtree
        IF (.NOT.Done) CALL Traverse_Postorder(Root%Right, IterFunc, Done)
        
        ! call user-supplied routine to perform user-specified task for the current root
        IF (.NOT.Done) ErrStat = IterFunc(Root, Done)
            
        ! set flag to quit if there is an error
        IF (ErrStat) Done = TrueVal
        
        RETURN

    END SUBROUTINE Traverse_Postorder

    !**************************************************************************

END SUBROUTINE BSTree_Traverse_I

!******************************************************************************

SUBROUTINE BSTree_Traverse_II(Tree, IterFunc)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform inorder traversal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    PROCEDURE(IteratorFunc)                 :: IterFunc !! iterator function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tComparable, ALLOCATABLE    :: KeyVal
    tLogical                    :: EndOfTree
    tLogical                    :: ErrStat
    tLogical                    :: Done

! FLOW
        
    ! check whether the tree is empty or not
    IF (Tree%IsEmpty()) RETURN
    
    ! allocate working variable
    ALLOCATE(KeyVal, MOLD=Tree%Root%KeyVal)
        
    ! set defaults
    Done    = FalseVal   ! traverse to all nodes
    ErrStat = FalseVal

    ! start iteration
    EndOfTree = Tree%StartFirst(KeyVal)
        
    ! loop over all nodes of the list
    DO WHILE (.NOT.EndOfTree)
            
        ! call iterator function
        ErrStat = IterFunc(KeyVal, Done)
            
        ! report error if necessary
        IF (ErrStat) THEN
            CALL Handle_ErrLevel('BSTree_Traverse_II', ModName, ErrSevere, &
                                 'An error occurred during call to iterator function.')
            RETURN
        END IF
            
        ! exit the loop if the user want to stop the traversing
        IF (Done) EXIT

        ! move to next node in the inorder traversal
        EndOfTree = Tree%MoveForward(KeyVal)

    END DO

    RETURN

END SUBROUTINE BSTree_Traverse_II

!******************************************************************************

FUNCTION BSTree_Move2First(Tree, KeyVal) RESULT(EmptyTree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To restart the iteration at the node with smallest key (and optionally to retrieve
    !  the key-value pair of that node if requested).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree         !! tree
    tComparable, OPTIONAL, INTENT(OUT)      :: KeyVal       !! smallest key and its associated value
    tLogical                                :: EmptyTree    !! true if the tree is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ASSOCIATED(Tree%Root)) THEN
            
        ! set iteration node to the node with the smallest key
        CALL Find_MinKeyNode(Tree%Root, Tree%Cursor)
            
        ! set flag
        EmptyTree = FalseVal

        ! get key-value pair if requested
        IF (PRESENT(KeyVal)) KeyVal = Tree%Cursor%KeyVal
            
    ELSE
            
        ! set iteration node to null
        Tree%Cursor => NULL()
            
        ! set flag
        EmptyTree = TrueVal
            
    END IF

    RETURN
        
END FUNCTION BSTree_Move2First

!******************************************************************************

FUNCTION BSTree_Move2NextPair(Tree, KeyVal) RESULT(EndOfTree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the next node in inorder traversal (and optionally to retrieve
    !  the key-value pair of the next node if requested).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    tComparable, OPTIONAL, INTENT(OUT)      :: KeyVal   !! key and its associated value of the next node
    tLogical                                :: EndOfTree
    !^ true if the current iteration node is  at the end of tree (i.e. the next node does not exist).

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER   :: NxtNode => NULL()

! FLOW

    IF (ASSOCIATED(Tree%Cursor)) THEN
            
        ! find next node
        CALL Find_Inorder_NextNode(Tree%Cursor, Tree%Root, NxtNode)
            
        ! check status of NxtNode
        IF (ASSOCIATED(NxtNode)) THEN
                
            ! next node exists so set iteration node to NxtNode
            Tree%Cursor => NxtNode
                
            ! set flag
            EndOfTree = FalseVal

            ! get key-value pair if requested
            IF (PRESENT(KeyVal)) KeyVal = Tree%Cursor%KeyVal
                
        ELSE
                
            ! next node does not exist so set iteration node to null
            Tree%Cursor => NULL()
            
            ! set flag
            EndOfTree = TrueVal
                
        END IF
            
    ELSE
            
        ! current iteration node is null so set flag to true
        EndOfTree = TrueVal
            
    END IF

    ! nullify working node
    NULLIFY(NxtNode)

    RETURN
        
END FUNCTION BSTree_Move2NextPair

!******************************************************************************

FUNCTION BSTree_Move2Last(Tree, KeyVal) RESULT(EmptyTree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To restart the iteration at the node with largest key (and optionally to retrieve
    !  the key-value pair of that node if requested).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree         !! tree
    tComparable, OPTIONAL, INTENT(OUT)      :: KeyVal       !! the largest key and its associated value
    tLogical                                :: EmptyTree    !! true if the tree is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ASSOCIATED(Tree%Root)) THEN
            
        ! set iteration node to the node with the largest key
        CALL Find_MaxKeyNode(Tree%Root, Tree%Cursor)
            
        ! set flag
        EmptyTree = FalseVal

        ! get key-value pair if requested
        IF (PRESENT(KeyVal)) KeyVal = Tree%Cursor%KeyVal
            
    ELSE
            
        ! set iteration node to null
        Tree%Cursor => NULL()
            
        ! set flag
        EmptyTree = TrueVal
            
    END IF

    RETURN
        
END FUNCTION BSTree_Move2Last

!******************************************************************************

FUNCTION BSTree_Move2PrevPair(Tree, KeyVal) RESULT(EndOfTree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the previous node in inorder traversal (and optionally to retrieve
    !  the key-value pair of the previous node if requested).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    tComparable, OPTIONAL, INTENT(OUT)      :: KeyVal   !! key and its associated value of the previous node
    tLogical                                :: EndOfTree
    !^ true if the current iteration node is  at the end of tree (i.e. the previous node does not exist).

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER   :: PrvNode => NULL()

! FLOW

    IF (ASSOCIATED(Tree%Cursor)) THEN
            
        ! find previous node
        CALL Find_Inorder_PrevNode(Tree%Cursor, Tree%Root, PrvNode)
            
        ! check status of PrvNode
        IF (ASSOCIATED(PrvNode)) THEN
                
            ! previous node exists so set iteration node to PrvNode
            Tree%Cursor => PrvNode
                
            ! set flag
            EndOfTree = FalseVal

            ! get key-value pair if requested
            IF (PRESENT(KeyVal)) KeyVal = Tree%Cursor%KeyVal

        ELSE
                
            ! previous node does not exist so set iteration node to null
            Tree%Cursor => NULL()
            
            ! set flag
            EndOfTree = TrueVal
                
        END IF
            
    ELSE
            
        ! current iteration node is null so set flag to true
        EndOfTree = TrueVal
            
    END IF

    ! nullify working node
    NULLIFY(PrvNode)

    RETURN
        
END FUNCTION BSTree_Move2PrevPair

!******************************************************************************

FUNCTION BSTree_IsEmpty(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the tree is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(IN)   :: Tree     !! tree
    tLogical                            :: Flag     !! true if the tree is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Flag = (Tree%GetSize() == 0_kIndex)

    RETURN

END FUNCTION BSTree_IsEmpty

!******************************************************************************

FUNCTION BSTree_FindKey(Tree, KeyVal, KeyNode) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the specified key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable),           INTENT(INOUT)  :: Tree     !! tree
    tComparable,                     INTENT(IN)     :: KeyVal   !! the key to be looked for
    TYPE(RBNode), OPTIONAL, POINTER, INTENT(OUT)    :: KeyNode  !! the node containing the specified key
    tLogical                                        :: Found    !! true if the key found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER :: OutNode => NULL()

! FLOW

    ! recursively search for the node that have the same key
    CALL FindNode(Tree%Root, KeyVal, OutNode)
        
    ! set flag
    Found = ASSOCIATED(OutNode)
        
    ! set optional output
    IF (PRESENT(KeyNode)) KeyNode => OutNode
        
    ! nullify working node
    NULLIFY(OutNode)

    RETURN
        
END FUNCTION BSTree_FindKey

!******************************************************************************

FUNCTION BSTree_GetSize(Tree) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get size of the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(IN)   :: Tree     !! tree
    tIndex                              :: Size     !! size of the tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Size = NodeSize(Tree%Root)
        
    RETURN
        
END FUNCTION BSTree_GetSize

!******************************************************************************

FUNCTION BSTree_GetSmallestKey(Tree, KeyVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the smallest key and its associated value in the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    tComparable ,          INTENT(OUT)      :: KeyVal   !! smallest key and its associated value
    !% flag indicating whether the key-value pair is found or not.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER   :: KeyNode => NULL()

! FLOW
    
    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_GetSmallestKey', ModName, ErrWarning, &
                             'The binary search tree is empty.')
        RETURN
    END IF
        
    ! find node with smallest key
    CALL Find_MinKeyNode(Tree%Root, KeyNode)
        
    ! get key-value pair
    KeyVal = KeyNode%KeyVal

    ! nullify working node
    NULLIFY(KeyNode)

    Flag = TrueVal

    RETURN
        
END FUNCTION BSTree_GetSmallestKey

!******************************************************************************

FUNCTION BSTree_GetLargestKey(Tree, KeyVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the largest key and its associated value in the tree.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    tComparable,           INTENT(OUT)      :: KeyVal   !! largest key and its associated value
    !% flag indicating whether the key-value pair is found or not.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER   :: KeyNode => NULL()

! FLOW
    
    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_GetLargestKey', ModName, ErrWarning, &
                             'The binary search tree is empty.')
        RETURN
    END IF
        
    ! find node with largest key
    CALL Find_MaxKeyNode(Tree%Root, KeyNode)
        
    ! get key-pair value
    KeyVal = KeyNode%KeyVal

    ! nullify working node
    NULLIFY(KeyNode)

    Flag = TrueVal

    RETURN
        
END FUNCTION BSTree_GetLargestKey

!******************************************************************************

FUNCTION BSTree_Floor(Tree, InKey, OutKey) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the largest key in the tree less than or equal to the given key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    tComparable,           INTENT(IN)       :: InKey    !! input key (and its associated value)
    tComparable,           INTENT(OUT)      :: OutKey   !! output key (and its associated value)
    !% flag indicating whether the key-value pair is found or not.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER   :: FloorNode => NULL()

! FLOW
    
    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_Floor', ModName, ErrWarning, &
                             'The binary search tree is empty.')
        RETURN
    END IF
        
    ! find the floor node
    CALL Find_FloorNode(Tree%Root, InKey, FloorNode)
        
    ! check whether the floor node exists or not
    IF (.NOT.ASSOCIATED(FloorNode)) THEN
        CALL Handle_ErrLevel('BSTree_Floor', ModName, ErrWarning, &
                             'The specified key is too small.')
        RETURN
    END IF
        
    ! get key-value pair
    OutKey = FloorNode%KeyVal

    ! nullify working node
    NULLIFY(FloorNode)

    Flag = TrueVal

    RETURN
        
END FUNCTION BSTree_Floor

!******************************************************************************

FUNCTION BSTree_Ceiling(Tree, InKey, OutKey) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the smallest key in the tree greater than or equal to the given key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    tComparable,           INTENT(IN)       :: InKey    !! input key (and its associated value)
    tComparable,           INTENT(OUT)      :: OutKey   !! output key (and its associated value)
    !% flag indicating whether the key-value pair is found or not.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RBNode), POINTER   :: CeilingNode => NULL()

! FLOW

    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_Ceiling', ModName, ErrWarning, &
                             'The binary search tree is empty.')
        RETURN
    END IF
        
    ! find the floor node
    CALL Find_CeilingNode(Tree%Root, InKey, CeilingNode)
        
    ! check whether the floor node exists or not
    IF (.NOT.ASSOCIATED(CeilingNode)) THEN
        CALL Handle_ErrLevel('BSTree_Ceiling', ModName, ErrWarning, &
                             'The specified key is too large.')
        RETURN
    END IF
        
    ! get key-value pair
    OutKey = CeilingNode%KeyVal

    ! nullify working node
    NULLIFY(CeilingNode)

    Flag = TrueVal

    RETURN
        
END FUNCTION BSTree_Ceiling

!******************************************************************************

FUNCTION BSTree_Select(Tree, Rank, KeyVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the key in the tree of a given rank.
    !  This key has the property that there are rank keys in
    !  the tree that are smaller. In other words, this key is the
    !  (rank+1)st smallest key in the tree. <br>
    !  Note: applicable range of rank is between 0 and tree_size-1.
    !       (this rank number is zero-based).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    tIndex,                INTENT(IN)       :: Rank     !! rank
    tComparable,           INTENT(OUT)      :: KeyVal   !! key-value pair of the given rank
    !% flag indicating whether the key-value pair is found or not.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Found

! FLOW

    Flag = FalseVal

    ! check input validity
    IF (NOT_IN_RANGE(Rank, 0, Tree%GetSize()-1)) THEN
        CALL Handle_ErrLevel('BSTree_Select', ModName, ErrSevere, &
                             'The specified rank is not in the applicable range.')
        RETURN
    END IF
        
    ! find the floor node
    Found = SelectKey(Tree%Root, Rank, KeyVal)

    ! check if the key is found
    IF (.NOT.Found) THEN
        CALL Handle_ErrLevel('BSTree_Select', ModName, ErrSevere, &
                             'There is a bug in the code.')
    ELSE
        Flag = TrueVal
    END IF
        
    RETURN
        
END FUNCTION BSTree_Select

!******************************************************************************

FUNCTION BSTree_Rank(Tree, Key) RESULT(Rank)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the number of keys in the tree strictly less than the given key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree !! tree
    tComparable,           INTENT(IN)       :: Key  !! key (and its associated value)
    tIndex                                  :: Rank !! rank of key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Rank = KeyRank(Tree%Root, Key)
        
    RETURN
        
END FUNCTION BSTree_Rank

!******************************************************************************

SUBROUTINE BSTree_GetKeys_Range(Tree, Low, High, KeyValQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return all keys and their associated values in the tree in the given range.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    tComparable,           INTENT(IN)       :: Low      !! low key
    tComparable,           INTENT(IN)       :: High     !! high key
    TYPE(QueueKeyVal),     INTENT(OUT)      :: KeyValQ  !! key-value queue

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! traverse the tree and get all keys in the range
    CALL GetKeys_Inorder(Tree%Root, Low, High, KeyValQ)
        
    RETURN
        
CONTAINS

    FUNCTION IsKeyInRange(Key, LowLimit, UppLimit) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To check if the key is in the specified range.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tComparable, INTENT(IN) :: Key
        tComparable, INTENT(IN) :: LowLimit
        tComparable, INTENT(IN) :: UppLimit
        tLogical                :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW
    
        IF ((Key < LowLimit).OR.(Key > UppLimit)) THEN
            Flag = FalseVal
        ELSE
            Flag = TrueVal
        END IF

        RETURN

    END FUNCTION IsKeyInRange

    !**************************************************************************

    RECURSIVE SUBROUTINE GetKeys_Inorder(Node, Lo, Hi, QueueKV)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To get inorder key.
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RBNode), POINTER,  INTENT(INOUT)   :: Node     !! root node of (sub)tree
        tComparable,            INTENT(IN)      :: Lo       !! low key
        tComparable,            INTENT(IN)      :: Hi       !! high key
        TYPE(QueueKeyVal),      INTENT(INOUT)   :: QueueKV  !! key-value queue

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: ErrStat

    ! FLOW
        
        ! check whether the root is null or not
        IF (.NOT.ASSOCIATED(Node)) RETURN
        
        ! traverse the left subtree
        IF (Lo < Node%KeyVal) CALL GetKeys_Inorder(Node%Left, Lo, Hi, QueueKV)
        
        ! call user-supplied routine to perform user-specified task for the current root
        IF (IsKeyInRange(Node%KeyVal, Lo, Hi)) THEN
            CALL QueueKV%EnQueue(Node%KeyVal)
        END IF
        
        ! traverse the right subtree
        IF (Hi > Node%KeyVal) CALL GetKeys_Inorder(Node%Right, Lo, Hi, QueueKV)
        
        RETURN

    END SUBROUTINE GetKeys_Inorder

    !**************************************************************************

END SUBROUTINE BSTree_GetKeys_Range

!******************************************************************************

SUBROUTINE BSTree_GetKeys_All(Tree, KeyValQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return all keys in the tree and optionally all associated values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! tree
    TYPE(QueueKeyVal),     INTENT(OUT)      :: KeyValQ  !! key-value queue

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! traverse the tree and get all keys in the range
    CALL Tree%Traverse(Inorder, GetKeys)
        
    RETURN
        
CONTAINS

    FUNCTION GetKeys(Node, Done) RESULT(ErrStat)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To get key of the specified node

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RBNode), INTENT(IN)    :: Node     !! node
        tLogical,     INTENT(INOUT) :: Done     !! on input, Done is set to FalseVal
                                                !! on exit, set it to TrueVal if user
                                                !!   want to stop the queue traversing.
        tLogical                    :: ErrStat  !! true if error occurred in iterator
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! set flag
        ErrStat = FalseVal
        Done = FalseVal
            
        ! add key to queues
        CALL KeyValQ%EnQueue(Node%KeyVal)

        RETURN
            
    END FUNCTION GetKeys
        
    !**************************************************************************

END SUBROUTINE BSTree_GetKeys_All

!******************************************************************************

FUNCTION BSTree_GetSize_Range(Tree, Low, High) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of keys in the tree in the given range.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree     !! TreeComparable object
    tComparable,           INTENT(IN)       :: Low      !! low key
    tComparable,           INTENT(IN)       :: High     !! high key
    tIndex                                  :: Size     !! size of the tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 0_kIndex
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_GetSize_Range', ModName, ErrWarning, &
                             'Tree is empty.')
        RETURN
    END IF

    IF (Low > High) THEN
        RETURN
    END IF
        
    IF (Tree%Contain(High)) THEN
        Size = Tree%GetRank(High) - Tree%GetRank(Low) + 1
    ELSE
        Size = Tree%GetRank(High) - Tree%GetRank(Low)
    END IF
        
    RETURN

END FUNCTION BSTree_GetSize_Range

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!               Routines for Checking Integrity of BST Data Structure
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION BSTree_IsRankConsistent(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check that ranks are consistent.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree !! tree
    tLogical                                :: Flag !! true if ranks are consistent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I
    tLogical                    :: Found
    tComparable, ALLOCATABLE    :: Key
    tComparable, ALLOCATABLE    :: NodeKey

! FLOW
    
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_IsRankConsistent', ModName, ErrWarning, 'Tree is empty.')
        Flag = TrueVal
        RETURN
    ELSE
        ALLOCATE(Key, MOLD=Tree%Root%KeyVal)
        ALLOCATE(NodeKey, MOLD=Tree%Root%KeyVal)
    END IF
        
    ! check rank of select of I
    DO I = 0, Tree%GetSize()-1
        Found = Tree%Select(I, Key)
        IF ((.NOT.Found).OR.(I /= Tree%GetRank(Key))) THEN
            Flag = FalseVal
            RETURN
        END IF
    END DO
        
    Flag = TrueVal
        
    ! check select of rank of key
    CALL Tree%Traverse(Inorder, CompareKey)
        
    RETURN
        
CONTAINS

    FUNCTION CompareKey(Node, Done) RESULT(ErrStat)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To compare key of the specified node.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RBNode), INTENT(IN)    :: Node     !! node
        tLogical,     INTENT(INOUT) :: Done     !! on input, Done is set to FalseVal
                                                !! on exit, set it to TrueVal if user
                                                !!   want to stop the queue traversing.
        tLogical                    :: ErrStat  !! true if error occurred in iterator
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: Found

    !** FLOW

        ! set flag
        ErrStat = FalseVal
        Done = FalseVal
            
        ! add key to queues
        Found = Tree%Select(Tree%GetRank(Node%KeyVal), NodeKey)
        IF ((.NOT.Found).OR.(Node%KeyVal /= NodeKey)) THEN
            Done = TrueVal
            Flag = FalseVal
        END IF

        RETURN
            
    END FUNCTION CompareKey

    !**************************************************************************

END FUNCTION BSTree_IsRankConsistent

!******************************************************************************

FUNCTION BSTree_IsSizeConsistent(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the size fields are correct or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree !! tree
    tLogical                                :: Flag !! true if the size fields are correct

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Flag = IsSizeConsistent(Tree%Root)
        
    RETURN
        
CONTAINS

    RECURSIVE FUNCTION IsSizeConsistent(Node) RESULT(Flag)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To check whether the size field is correct or not.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RBNode), POINTER, INTENT(IN)   :: Node     !! node
        tLogical                            :: Flag     !! true if the size fields are correct
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (.NOT.ASSOCIATED(Node)) THEN
            Flag = TrueVal
        ELSEIF (Node%Size /= (NodeSize(Node%Left)+NodeSize(Node%Right)+1)) THEN
            Flag = FalseVal
        ELSE
            Flag = (IsSizeConsistent(Node%Left).AND.IsSizeConsistent(Node%Right))
        END IF

        RETURN
            
    END FUNCTION IsSizeConsistent
        
!**************************************************************************

END FUNCTION BSTree_IsSizeConsistent

!******************************************************************************

FUNCTION BSTree_IsBinarySearchTree(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the inorder keys in the tree are in ascending order or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree !! tree
    tLogical                                :: Flag !! true if the inorder keys are sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(QueueKeyVal)           :: InorderKeyQ
    tComparable, ALLOCATABLE    :: KeyI, KeyIP1

! FLOW
        
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_IsBinarySearchTree', ModName, ErrWarning, 'Tree is empty.')
        Flag = TrueVal
        RETURN
    ELSE
        ALLOCATE(KeyI, MOLD=Tree%Root%KeyVal)
        ALLOCATE(KeyIP1, MOLD=Tree%Root%KeyVal)
    END IF
        
    ! get inorder keys
    CALL Tree%GetKeys(InorderKeyQ)
        
    ! verify that the inorder keys are sorted
    Flag = Are_Keys_In_Ascending_Order(InorderKeyQ)
        
    RETURN
        
CONTAINS

    FUNCTION Are_Keys_In_Ascending_Order(KeyQ) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE
        !^ To check whether the given keys are sorted in ascending order.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(QueueKeyVal), INTENT(INOUT)    :: KeyQ !! key queue
        tLogical                            :: Flag !! true if the queue is sorted
 
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: Success
 
    !** FLOW:

        ! initialize
        Flag = TrueVal
    
        ! check if the queue is empty or not
        IF (KeyQ%IsEmpty()) RETURN
    
        ! get KeyI
        Success = KeyQ%DeQueue(KeyI)

        DO WHILE (.NOT.KeyQ%IsEmpty())
            ! get KeyIP1
            Success = KeyQ%DeQueue(KeyIP1)
            ! compare keys
            IF (KeyIP1 <= KeyI) THEN
                Flag = FalseVal
                CALL KeyQ%Destruct()
                EXIT
            END IF
            ! reset KeyI
            KeyI = KeyIP1
        END DO
    
        RETURN
    
    END FUNCTION Are_Keys_In_Ascending_Order

    !**************************************************************************

END FUNCTION BSTree_IsBinarySearchTree

!******************************************************************************

FUNCTION BSTree_Is23Tree(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the tree is a 2-3 tree. <br>
    !  => Does the tree have no red right links, and at most one (left)
    !     red links in a row on any path?

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree !! tree
    tLogical                                :: Flag !! true if this is a 2-3 tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Flag = Is23Tree(Tree%Root)
        
    RETURN
        
CONTAINS

    RECURSIVE FUNCTION Is23Tree(Node) RESULT(Flag)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To check whether the subtree is a 2-3 tree.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RBNode), POINTER, INTENT(IN)   :: Node     !! node
        tLogical                            :: Flag     !! true if this is a 2-3 tree
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (.NOT.ASSOCIATED(Node)) THEN
            Flag = TrueVal
        ELSEIF (IsRed(Node%Right)) THEN
            Flag = FalseVal
        ELSEIF (.NOT.ASSOCIATED(Node,Tree%Root).AND.IsRed(Node).AND.IsRed(Node%Left)) THEN
            Flag = FalseVal
        ELSE
            Flag = (Is23Tree(Node%Left).AND.Is23Tree(Node%Right))
        END IF

        RETURN
            
    END FUNCTION Is23Tree
        
    !**************************************************************************

END FUNCTION BSTree_Is23Tree

!******************************************************************************

FUNCTION BSTree_IsBalanced(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the tree is balanced. <br>
    !  => Do all paths from root to leaf have
    !     same number of black edges?.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree !! tree
    tLogical                                :: Flag !! true if the tree is balanced

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: BlackCount   ! number of black links on path from root to min
    TYPE(RBNode), POINTER   :: XNode => NULL()

! FLOW
        
    BlackCount = 0
    XNode => Tree%Root
        
    DO WHILE (ASSOCIATED(XNode))
        IF (.NOT.IsRed(XNode)) BlackCount = BlackCount + 1
        XNode => XNode%Left
    END DO
    NULLIFY(XNode)
        
    Flag = IsBalanced(Tree%Root, BlackCount)
        
    RETURN
        
CONTAINS

    RECURSIVE FUNCTION IsBalanced(Node, BlackCount) RESULT(Flag)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To check whether the subtree is balanced.
        !  => Does every path from the root to a leaf
        !     have the given number of black links?

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RBNode), POINTER, INTENT(IN)   :: Node         !! node
        tIndex,                INTENT(IN)   :: BlackCount   !! number of black links on path from root to min
        tLogical                            :: Flag         !! true if the tree is balanced
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: CurrBlackCount

    !** FLOW

        CurrBlackCount = BlackCount
        IF (.NOT.ASSOCIATED(Node)) THEN
            Flag = (CurrBlackCount == 0)
        ELSE
            IF (.NOT.IsRed(Node)) CurrBlackCount = CurrBlackCount - 1
            Flag = (IsBalanced(Node%Left, CurrBlackCount).AND. &
                    IsBalanced(Node%Right, CurrBlackCount))
        END IF

        RETURN
            
    END FUNCTION IsBalanced
        
    !**************************************************************************

END FUNCTION BSTree_IsBalanced

!******************************************************************************

FUNCTION BSTree_CheckBST(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check integrity of BST data structure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeComparable), INTENT(INOUT)    :: Tree !! tree
    tLogical                                :: Flag !! flag for integrity

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: IsBSTree
    tLogical    :: Is23Tree
    tLogical    :: IsBalanced
    tLogical    :: IsRankConsistent
    tLogical    :: IsSizeConsistent

! FLOW
        
    IsBSTree = Tree%IsBSTree()
    Is23Tree = Tree%Is23Tree()
    IsBalanced = Tree%IsBalanced()
    IsRankConsistent = Tree%IsRankConsistent()
    IsSizeConsistent = Tree%IsSizeConsistent()
    IF (.NOT.IsBSTree) THEN
        CALL Handle_ErrLevel('BSTree_CheckBST', ModName, ErrWarning, &
                    'Not in symmetric order.')
    END IF
    IF (.NOT.Is23Tree) THEN
        CALL Handle_ErrLevel('BSTree_CheckBST', ModName, ErrWarning, &
                    'Not a 2-3 tree.')
    END IF
    IF (.NOT.IsBalanced) THEN
        CALL Handle_ErrLevel('BSTree_CheckBST', ModName, ErrWarning, &
                    'Not balanced tree.')
    END IF
    IF (.NOT.IsRankConsistent) THEN
        CALL Handle_ErrLevel('BSTree_CheckBST', ModName, ErrWarning, &
                    'Ranks not consistent.')
    END IF
    IF (.NOT.IsSizeConsistent) THEN
        CALL Handle_ErrLevel('BSTree_CheckBST', ModName, ErrWarning, &
                    'Subtree counts not consistent.')
    END IF
    Flag = (IsBSTree.AND.Is23Tree.AND.IsRankConsistent.AND.IsSizeConsistent)
        
    RETURN
        
END FUNCTION BSTree_CheckBST

!******************************************************************************

!** UNDEFINE MACROS **
#undef TreeComparable
#undef tComparable
#undef tComparable
#undef tComparable
#undef QueueKeyVal

END MODULE MClass_TreeComparable

!******************************************************************************
