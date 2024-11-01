
MODULE MClass_IntrusiveBSTrees

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains *intrusive-based* balanced-tree container types and their
!   related routines.  The module provides application programming interfaces (APIs)
!   for two balanced-tree containers: the AVL tree (by the *IntrusiveAVLTree* type)
!   and the red-black tree (by the *IntrusiveRBTree* type).  The module also provides
!   a binary-search tree node type (*BSTNode*) to be used with the containers. <br>
!   Both balanced-tree container types employ the so-called **intrusive** technique.
!   See the <a href="../module/MClass_IntrusiveLinkedLists.html">MClass_IntrusiveLinkedLists</a>
!   module for detailed explanation of the intrusive technique. <br>
!   Similar to the <a href="../module/MClass_IntrusiveLinkedLists.html#type-doublylinkednode">DoublyLinkedNode</a>
!   type, a user must define a new node type that extends the *BSTNode* type in order to
!   use an intrusive tree container provided.  This new type typically contains user data
!   as its additional component(s).  However, unlike a user type that extends the
!   *DoublyLinkedNode* type, this new type must be able to compare to itself and provides
!   a total ordering on the user objects.  This is due to the fact that the new user type
!   is in the *Comparable* class (as the *BSTNode* type is a subtype of the *Comparable*
!   type).  Therefore, the user must implement deferred procedures required by a
!   *Comparable* object.  This means that user data of the new user type typically contain
!   a key and other data.  The key then is commonly used to provide a comparison of two
!   *Comparable* objects. <br>
!   The *IntrusiveAVLTree* type is a balanced-tree container type that uses the AVL
!   (Adelson-Velsky and Landis) tree whereas the *IntrusiveRBTree* type is also a
!   balanced-tree container type but employs a left-leaning red-black (RB) tree.
!   Both tree container types stores objects (or nodes) in the *BSTNode* class.  Thus,
!   a user type that extends the *BSTNode* type can be used in both tree containers.

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_CompNodePool,    ONLY: CompNode

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived types
    PUBLIC :: BSTNode
    PUBLIC :: IntrusiveAVLTree
    PUBLIC :: IntrusiveRBTree
    ! helper procedures
    PUBLIC :: Find_Inorder_Predecessor
    PUBLIC :: Find_Inorder_Successor

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_IntrusiveBSTrees'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! traverse flags
    tSInt32,   PARAMETER    :: Inorder   = 1
    tSInt32,   PARAMETER    :: Preorder  = 2
    tSInt32,   PARAMETER    :: Postorder = 3
    ! color flags
    tIndex,    PARAMETER    :: Red   =  1_kIndex
    tIndex,    PARAMETER    :: Black = -1_kIndex

!** DERIVED TYPE DEFINITIONS
    !>  *BSTNode* is a binary-search tree node type provided to be used with both
    !   the *IntrusiveAVLTree* and *IntrusiveRBTree* container types.   Like the
    !   *DoublyLinkedNode* type, the *BSTNode*  contains no data content.  Therefore,
    !   an intrusive container working with objects (or nodes) in the *BSTNode* class
    !   can operate without having to manage the data content. <br>
    !   However, to use the intrusive container properly, for example, a user would
    !   typically define a new node type that extends the *BSTNode* type as follows:
    !   <Pre><Code style="color:MidnightBlue;">
    !   ! a tree-node object with default integer as type of its key and
    !     default real as type of its content
    !   TYPE, EXTENDS(BSTNode) :: IntegerRealNode
    !       INTEGER     :: key          ! stored key
    !       REAL        :: content      ! stored content
    !   END TYPE
    !   </Code></Pre>
    !   Because this new node type is a subtype of the *BSTNode* type, it can be used
    !   with any intrusive tree containers for the available operations.  It should be
    !   noted that this new type is in both the *Object* and *Comparable* classes.
    !   Therefore, a key component of user data would commonly be used to implement
    !   the *CompareTo* deferred procedure required by a *Comparable* object whereas
    !   the key and/or other components of the user data may be used to implement other
    !   deferred procedures required by an *Object* object. <br>
    !   Similar to an intrusive list container, an intrusive tree container only provides
    !   linking and unlinking capabilities and thus operates without the memory management
    !   of the user data.  This means that the user must manage the lifetime of inserted
    !   objects (i.e. the new node type) and be careful that these objects are not destroyed
    !   before they get removed from the container.
    TYPE, ABSTRACT, EXTENDS(CompNode)   :: BSTNode
        PRIVATE
        ! Pointer to the left node (or subtree).
        CLASS(BSTNode), POINTER :: Left  => NULL()
        ! Pointer to the right node (or subtree).
        CLASS(BSTNode), POINTER :: Right => NULL()
        ! Number of nodes in subtree rooted by this node.
        tIndex                  :: Size = 0_kIndex
        ! Height (or color) of this node.  When the node is used with
        ! the *IntrusiveAVLTree* container type, it is interpreted as
        ! a height.  When it is used with *IntrusiveRBTree* container
        ! type, it is interpreted as a color.
        tIndex                  :: HC = Black
    CONTAINS
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetLeft <br>
        !  **Purpose**:  To get a pointer to the left node of the current node. <br>
        !  **Usage**: <br>
        !   --->    LeftNode => CurrNode%GetLeft()
        PROCEDURE   :: GetLeft          => BSTNode_GetLeftNode
        !> **Type-Bound Function**: GetRight <br>
        !  **Purpose**:  To get a pointer to the right node of the current node. <br>
        !  **Usage**: <br>
        !   --->    RightNode => CurrNode%GetRight()
        PROCEDURE   :: GetRight         => BSTNode_GetRightNode
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get number of nodes in subtree rooted by this node. <br>
        !  **Usage**: <br>
        !   --->    Size = CurrNode%GetSize()
        PROCEDURE   :: GetSize          => BSTNode_GetSize
        !> **Type-Bound Subroutine**: FreePointers <br>
        !  **Purpose**:  To nullify the pointer components of the node. <br>
        !  **Usage**: <br>
        !   --->    CALL CurrNode%FreePointers()
        PROCEDURE   :: FreePointers     => BSTNode_FreePointers
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: SetNew <br>
        !  **Purpose**:  To set the node as a new node in the tree container. <br>
        !  **Usage**: <br>
        !   --->    CALL Node%SetNew()
        PROCEDURE   :: SetNew           => BSTNode_SetNew
        !> **Type-Bound Subroutine**: ResetBSTNode <br>
        !  **Purpose**:  To reset the node to its initialized state. <br>
        !  **Usage**: <br>
        !   --->    CALL Node%ResetBSTNode()
        PROCEDURE   :: ResetBSTNode     => BSTNode_Reset
        !> **Type-Bound Subroutine**: CopyBSTNode <br>
        !  **Purpose**:  To copy members of the source to the destination.  This method
        !                is provided to help user implement deferred procedure(s) required
        !                by an object in the *Object* class. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcNode%CopyBSTNode(DstNode)
        PROCEDURE   :: CopyBSTNode      => BSTNode_CopyBSTNode
        !> **Type-Bound Function**: IsBSTNodeEqual <br>
        !  **Purpose**:  To compare whether all members of both objects are equal or not.
        !                This method is provided to help user implement deferred procedure(s)
        !                required by an object in the *Object* class. <br>
        !  **Usage**: <br>
        !   --->    Flag = LhsNode%IsBSTNodeEqual(RhsNode) <br>
        !   --->    IF (.NOT.LhsNode%IsBSTNodeEqual(RhsNode)) DoSomething
        PROCEDURE   :: IsBSTNodeEqual   => BSTNode_EqualTo
        ! ---------------------------------------------------------------------
    END TYPE BSTNode
    !>  *IntrusiveAVLTree* is a binary-search tree container type that represents an
    !   ordered symbol table of objects in the *BSTNode* class.  It supports various
    !   common operations including the *Insert*, *Remove*, *Contain*, *GetSize*, and
    !   *IsEmpty* methods.  It also provides ordered methods for finding the minimum,
    !   maximum, floor, and ceiling objects.  In addition, it provides methods to
    !   perform an iteration over all nodes (or objects) in two directions (from minimum
    !   node to maximum node, and vice versa).  <br>
    !   The *IntrusiveAVLTree* type internally employs the AVL tree (Georgy Adelson-
    !   Velsky and Evgenii Landis' tree), which is a self-balancing tree.  In an AVL
    !   tree, the heights of the two child subtrees of any node differ by at most one;
    !   if at any time they differ by more than one, re-balancing is done to restore
    !   this property.  <br>
    !   As previously discussed, the *IntrusiveAVLTree* type provides methods that
    !   allow an iteration over all objects in two directions.
    !   The following code snippet illustrates how to perform an inorder traversal over
    !   the tree from the minimum object to the maximum object.
    !   <Pre><Code style="color:MidnightBlue;">
    !   ! start (forward) inorder traversal (from the minimum object)
    !   IsEmpty = Tree%StartMin()
    !   IF (.NOT.IsEmpty) DoSomeThing...
    !   DO
    !       ! move to the next (successor) object
    !       IsTheEnd = Tree%MoveForward()
    !       ! check whether we reach the end of the tree or not
    !       IF (IsTheEnd) EXIT
    !       ! if not, do the task we need
    !       DoSomeThing...
    !   END DO
    !   </Code></Pre>
    !   The following code snippet shows another way to perform an inorder traversal.
    !   <Pre><Code style="color:MidnightBlue;">
    !   ! start (forward) inorder traversal (from the minimum object)
    !   IsTheEnd = Tree%StartMin(CurrObj)
    !   DO WHILE (.NOT.IsTheEnd)
    !       DoSomeThing_With_CurrObj...
    !       ! move to the next (successor) object
    !       IsTheEnd = Tree%MoveForward(CurrObj)
    !   END DO
    !   </Code></Pre>
    !   In addition, the following code snippet shows how to iterate over the tree
    !   in reverse order (from the maximum object to the minimum object).
    !   <Pre><Code style="color:MidnightBlue;">
    !   ! start (backward) inorder traversal (from the maximum object)
    !   IsTheEnd = Tree%StartMax(CurrObj)
    !   DO WHILE (.NOT.IsTheEnd)
    !       DoSomeThing_With_CurrObj...
    !       ! move to the next iteration
    !       IsTheEnd = Tree%MoveBackward(CurrObj)
    !   END DO
    !   </Code></Pre>
    !   It should be note that, as a symbol table, the *IntrusiveAVLTree* type does
    !   not allow insertion of duplicated objects  (i.e. objects that are equal to
    !   one another).  When inserting a new object, if the tree container already
    !   contains an object that is equal to the new object, it replaces that object
    !   with the new one.  However, it does not check (or know) whether those objects
    !   are actually the same object or not.
    TYPE IntrusiveAVLTree
        PRIVATE
        ! pointer to the root node (topmost item) of the tree
        CLASS(BSTNode), POINTER :: Root   => NULL()
        ! pointer to the current item (or node) used for iteration purpose
        CLASS(BSTNode), POINTER :: Cursor => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----             Cloning Procedure                             -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Clone <br>
        !  **Purpose**:  To perform cloning of the source tree. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcTree%CloneTo(DstTree) <br>
        PROCEDURE   :: CloneTo      => BSTree_Clone
        ! ---------------------------------------------------------------------
        ! -----                 Adding/Removing procedures                -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all nodes from the tree. <br>
        !  **Usage**: <br>
        !   --->    CALL Tree%Clear()
        PROCEDURE   :: Clear        => BSTree_RemoveAll
        ! ---------------------------------------------------------------------
        ! -----                 Iteration procedures                      -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartMin <br>
        !  **Purpose**:  To start an iteration at minimum node and return a flag
        !                indicating whether the tree is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Tree%StartMin() <br>
        !   --->    IsEmpty = Tree%StartMin(FirstNode)
        PROCEDURE   :: StartMin     => BSTree_StartMin
        !> **Type-Bound Function**: StartMax <br>
        !  **Purpose**:  To start an iteration at maximum node and return a flag
        !                indicating whether the tree is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Tree%StartMax() <br>
        !   --->    IsEmpty = Tree%StartMax(FirstNode)
        PROCEDURE   :: StartMax     => BSTree_StartMax
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move *forward* to the next (successor) node and return
        !                a flag indicating whether the cursor has reached the end
        !                of the tree or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Tree%MoveForward() <br>
        !   --->    IsTheEnd = Tree%MoveForward(NextNode) <br>
        PROCEDURE   :: MoveForward  => BSTree_Move2Next
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move *backward* to the previous (predecessor) node and
        !                return a flag indicating whether the cursor has reached
        !                the end of the tree or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Tree%MoveBackward() <br>
        !   --->    IsTheEnd = Tree%MoveBackward(PrevNode) <br>
        PROCEDURE   :: MoveBackward => BSTree_Move2Prev
        ! ---------------------------------------------------------------------
        ! -----                 Inquiry procedures                        -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the container is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%IsEmpty() <br>
        !   --->    IF (.NOT.Tree%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty      => BSTree_IsEmpty
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To check whether the specified node is currently stored in
        !                the container or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Contain(NodeA) <br>
        !   --->    IF (.NOT.Tree%Contain(NodeB)) DoSomething
        PROCEDURE   :: Contain      => BSTree_Contain
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get size of the container (number of nodes). <br>
        !  **Usage**: <br>
        !   --->    TreeSize = Tree%GetSize()
        PROCEDURE   :: GetSize      => BSTree_GetSize
        !> **Type-Bound Function**: GetRangeSize <br>
        !  **Purpose**:  To get the number of nodes in the tree in the given range.
        !                It is the number of nodes between LowNode (inclusive) and
        !                HighNode (inclusive). <br>
        !  **Usage**: <br>
        !   --->    RangeSize = Tree%GetRangeSize(LowNode, HighNode)
        PROCEDURE   :: GetRangeSize => BSTree_GetRangeSize
        !> **Type-Bound Function**: GetMinNode <br>
        !  **Purpose**:  To get a pointer to the node with the smallest value.
        !                If the tree is empty, return null pointer. <br>
        !  **Usage**: <br>
        !   --->    MinNode => Tree%GetMinNode()
        PROCEDURE   :: GetMinNode   => BSTree_GetSmallestNode
        !> **Type-Bound Function**: GetMaxNode <br>
        !  **Purpose**:  To get a pointer to the node with the largest value.
        !                If the tree is empty, return null pointer. <br>
        !  **Usage**: <br>
        !   --->    MaxNode => Tree%GetMaxNode()
        PROCEDURE   :: GetMaxNode   => BSTree_GetLargestNode
        !> **Type-Bound Function**: GetCursor <br>
        !  **Purpose**:  To get a pointer to the node the cursor points to.  
        !       This usually points to the current node when performing an
        !       iteration over the tree. <br>
        !  **Usage**: <br>
        !   --->    CurrNode => Tree%GetCursor()
        PROCEDURE   :: GetCursor    => BSTree_GetCursor
        !> **Type-Bound Function**: GetRoot <br>
        !  **Purpose**:  To get a pointer to the root node of the tree. <br>
        !  **Usage**: <br>
        !   --->    CurrNode => Tree%GetRoot()
        PROCEDURE   :: GetRoot      => BSTree_GetRoot
        ! ---------------------------------------------------------------------
        ! -----                 Ranking procedures                        -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Floor <br>
        !  **Purpose**:  To return a pointer to the largest node in the tree less
        !                than or equal to the given node.  Return null pointer if
        !                the tree is empty or such node does not exist. <br>
        !  **Usage**: <br>
        !   --->    FloorNode => Tree%Floor(InNode)
        PROCEDURE   :: Floor        => BSTree_Floor
        !> **Type-Bound Function**: Ceiling <br>
        !  **Purpose**:  To return a pointer to the smallest node in the tree greater
        !                than or equal to the given node.  Return null pointer if the
        !                tree is empty or such node does not exist. <br>
        !  **Usage**: <br>
        !   --->    CeilingNode => Tree%Ceiling(InNode)
        PROCEDURE   :: Ceiling      => BSTree_Ceiling
        !> **Type-Bound Function**: Select <br>
        !  **Purpose**:  To return a pointer to the node in the tree of a given rank.
        !                This node has the property that there are rank nodes in the
        !                tree that are smaller.  Applicable range of rank is between
        !                0 and tree_size-1 where this rank number is zero-based. <br>
        !                Return null pointer if the tree is empty, such node does not
        !                exist or the input is invalid. <br>
        !  **Usage**: <br>
        !   --->    OutNode => Tree%Select(Rank)
        PROCEDURE   :: Select       => BSTree_Select
        !> **Type-Bound Function**: Rank <br>
        !  **Purpose**:  To return the number of nodes in the tree strictly less than
        !                the given node. <br>
        !  **Usage**: <br>
        !   --->    Rank = Tree%Rank(InNode)
        PROCEDURE   :: Rank         => BSTree_Rank
        ! ---------------------------------------------------------------------
        ! -----     Specific AVL tree Procedures                          -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert a new node into the tree.  If the tree already
        !                contains a node that is equal to the new node, replace
        !                that node with the new one. <br>
        !  **Usage**: <br>
        !   --->    CALL Tree%Insert(NewNode) <br>
        PROCEDURE   :: Insert       => AVLTree_Insert
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified node from the tree.  Return the flag
        !                indicating whether the node is removed successfully or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Tree%Remove(NodeA) <br>
        !   --->    IF (.NOT.Tree%Remove(NodeB)) DoSomething
        PROCEDURE   :: Remove       => AVLTree_Remove
        !> **Type-Bound Function**: RemoveMin <br>
        !  **Purpose**:  To remove the node with the smallest value from the tree.
        !                Return the flag indicating whether the node is removed
        !                successfully or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Tree%RemoveMin(MinNode) <br>
        !   --->    IF (.NOT.Tree%RemoveMin()) DoSomething
        PROCEDURE   :: RemoveMin    => AVLTree_RemoveMin
        !> **Type-Bound Function**: RemoveMax <br>
        !  **Purpose**:  To remove the node with the largest value from the tree.
        !                Return the flag indicating whether the node is removed
        !                successfully or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Tree%RemoveMax(MaxNode) <br>
        !   --->    IF (.NOT.Tree%RemoveMax()) DoSomething
        PROCEDURE   :: RemoveMax    => AVLTree_RemoveMax
        !> **Type-Bound Function**: Check <br>
        !  **Purpose**:  To check integrity of the BST data structure. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Check() <br>
        !   --->    IF (.NOT.Tree%Check(ErrMsg)) DoSomething
        PROCEDURE   :: Check        => AVLTree_CheckIntegrity
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the list.
        FINAL       :: AVLTree_Finalizer
    END TYPE IntrusiveAVLTree
    !> *IntrusiveRBTree* is a binary-search tree container type that represents an
    !  ordered symbol table of objects in the *BSTNode* class.  It is a subtype of
    !  the *IntrusiveAVLTree* type and thus inherits all methods from its super class.
    !  Internally, the *IntrusiveRBTree* type utilizes a left-leaning red-black (RB)
    !  tree, which is also a self-balancing tree.  It only overrides those methods
    !  that require a different implementation.
    TYPE, EXTENDS(IntrusiveAVLTree) :: IntrusiveRBTree
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----     Specific RB tree (Overridden) Procedures              -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert a new node into the tree.  If the tree already
        !                contains a node that is equal to the new node, replace
        !                that node with the new one. <br>
        !  **Usage**: <br>
        !   --->    CALL Tree%Insert(NewNode) <br>
        PROCEDURE   :: Insert       => RBTree_Insert
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified node from the tree.  Return the flag
        !                indicating whether the node is removed successfully or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Tree%Remove(NodeA) <br>
        !   --->    IF (.NOT.Tree%Remove(NodeB)) DoSomething
        PROCEDURE   :: Remove       => RBTree_Remove
        !> **Type-Bound Function**: RemoveMin <br>
        !  **Purpose**:  To remove the node with the smallest value from the tree.
        !                Return the flag indicating whether the node is removed
        !                successfully or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Tree%RemoveMin(MinNode) <br>
        !   --->    IF (.NOT.Tree%RemoveMin()) DoSomething
        PROCEDURE   :: RemoveMin    => RBTree_RemoveMin
        !> **Type-Bound Function**: RemoveMax <br>
        !  **Purpose**:  To remove the node with the largest value from the tree.
        !                Return the flag indicating whether the node is removed
        !                successfully or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Tree%RemoveMax(MaxNode) <br>
        !   --->    IF (.NOT.Tree%RemoveMax()) DoSomething
        PROCEDURE   :: RemoveMax    => RBTree_RemoveMax
        !> **Type-Bound Function**: Check <br>
        !  **Purpose**:  To check integrity of the BST data structure. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Check() <br>
        !   --->    IF (.NOT.Tree%Check(ErrMsg)) DoSomething
        PROCEDURE   :: Check        => RBTree_CheckIntegrity
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the list.
        FINAL       :: RBTree_Finalizer
    END TYPE IntrusiveRBTree

!** INTERFACE DEFINITIONS:
    ! interfaces for common procedures
    INTERFACE
        !----------------------------------------------------------------------
        !> To perform cloning of the source tree.
        MODULE SUBROUTINE BSTree_Clone(SrcTree, DstTree)
            !% source IntrusiveAVLTree object
            CLASS(IntrusiveAVLTree), INTENT(IN)     :: SrcTree
            !% destination IntrusiveAVLTree object
            CLASS(IntrusiveAVLTree), INTENT(OUT)    :: DstTree
        END SUBROUTINE BSTree_Clone
        !----------------------------------------------------------------------
        !> To remove all nodes from the tree.
        MODULE SUBROUTINE BSTree_RemoveAll(Tree)
            !% IntrusiveAVLTree object
            CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree
        END SUBROUTINE BSTree_RemoveAll
        !----------------------------------------------------------------------
        !> To start an iteration by setting the cursor pointer to the minimum node of
        !  the tree and return a flag indicating whether the tree is empty or not.
        MODULE FUNCTION BSTree_StartMin(Tree, NodeOut) RESULT(IsEmpty)
            !% IntrusiveAVLTree object
            CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
            !% pointer to the starting node
            CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: NodeOut
            !% true if the tree is empty
            tLogical                                            :: IsEmpty
        END FUNCTION BSTree_StartMin
        !----------------------------------------------------------------------
        !> To start an iteration by setting the cursor pointer to the maximum node of
        !  the tree and return a flag indicating whether the tree is empty or not.
        MODULE FUNCTION BSTree_StartMax(Tree, NodeOut) RESULT(IsEmpty)
            !% IntrusiveAVLTree object
            CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
            !% pointer to the starting node
            CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: NodeOut
            !% true if the tree is empty
            tLogical                                            :: IsEmpty
        END FUNCTION BSTree_StartMax
        !----------------------------------------------------------------------
        !> To move to the next (successor) node and return a flag indicating
        !  whether the cursor has reached the end of the tree or not.
        MODULE FUNCTION BSTree_Move2Next(Tree, NodeOut) RESULT(IsTheEnd)
            !% IntrusiveAVLTree object
            CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
            !% pointer to the next node
            CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: NodeOut
            !% true if the cursor pointer has reached the end of the tree
            tLogical                                            :: IsTheEnd
        END FUNCTION BSTree_Move2Next
        !----------------------------------------------------------------------
        !> To move to the previous (predecessor) node and return a flag indicating
        !  whether the cursor has reached the end of the tree or not.
        MODULE FUNCTION BSTree_Move2Prev(Tree, NodeOut) RESULT(IsTheEnd)
            !% IntrusiveAVLTree object
            CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
            !% pointer to the next node
            CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: NodeOut
            !% true if the cursor pointer has reached the end of the tree
            tLogical                                            :: IsTheEnd
        END FUNCTION BSTree_Move2Prev
        !----------------------------------------------------------------------
        !> To check whether the tree is empty or not.
        MODULE FUNCTION BSTree_IsEmpty(Tree) RESULT(Flag)
            CLASS(IntrusiveAVLTree), INTENT(IN) :: Tree !! tree
            tLogical                            :: Flag !! true if the tree is empty
        END FUNCTION BSTree_IsEmpty
        !----------------------------------------------------------------------
        !> To check whether there is a node stored in the tree that is equal
        !  to the specified node or not.  Optionally, return a pointer to the
        !  stored node if found (or return null pointer if not found).
        MODULE FUNCTION BSTree_Contain(Tree, InNode, OutNode) RESULT(Found)
            !% IntrusiveAVLTree object
            CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
            !% node to be looked for
            CLASS(BSTNode),                    INTENT(IN)       :: InNode
            !% a pointer to a stored node equal to the specified one
            CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: OutNode
            !% true if the stored node found
            tLogical                                            :: Found
        END FUNCTION BSTree_Contain
        !----------------------------------------------------------------------
        !> To get the tree size (i.e. the number of nodes).
        MODULE FUNCTION BSTree_GetSize(Tree) RESULT(Size)
            CLASS(IntrusiveAVLTree), INTENT(IN) :: Tree !! IntrusiveAVLTree object
            tIndex                              :: Size !! size of the tree
        END FUNCTION BSTree_GetSize
        !----------------------------------------------------------------------
        !> To get the number of nodes in the tree in the given range.
        MODULE FUNCTION BSTree_GetRangeSize(Tree, Low, High) RESULT(Size)
            CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree     !! tree
            CLASS(BSTNode),          INTENT(IN)     :: Low      !! low (minimum endpoint) node
            CLASS(BSTNode),          INTENT(IN)     :: High     !! high (maximum endpoint) node
            tIndex                                  :: Size     !! range size
        END FUNCTION BSTree_GetRangeSize
        !----------------------------------------------------------------------
        !> To get the node with the smallest value (key).  If the tree is empty
        !  return null pointer.
        MODULE FUNCTION BSTree_GetSmallestNode(Tree) RESULT(OutNode)
            CLASS(IntrusiveAVLTree), INTENT(IN) :: Tree     !! tree
            CLASS(BSTNode),          POINTER    :: OutNode  !! pointer to smallest node (or null)
        END FUNCTION BSTree_GetSmallestNode
        !----------------------------------------------------------------------
        !> To get the node with the largest value (key).  If the tree is empty
        !  return null pointer.
        MODULE FUNCTION BSTree_GetLargestNode(Tree) RESULT(OutNode)
            CLASS(IntrusiveAVLTree), INTENT(IN) :: Tree     !! tree
            CLASS(BSTNode),          POINTER    :: OutNode  !! pointer to largest node (or null)
        END FUNCTION BSTree_GetLargestNode
        !----------------------------------------------------------------------
        !> To get a pointer to the cursor node of the tree
        MODULE FUNCTION BSTree_GetCursor(Tree) RESULT(Cursor)
            CLASS(IntrusiveAVLTree), INTENT(IN) :: Tree     !! IntrusiveAVLTree object
            CLASS(BSTNode),          POINTER    :: Cursor   !! pointer to the cursor node
        END FUNCTION BSTree_GetCursor
        !----------------------------------------------------------------------
        !> To get a pointer to the root node of the tree
        MODULE FUNCTION BSTree_GetRoot(Tree) RESULT(Root)
            CLASS(IntrusiveAVLTree), INTENT(IN) :: Tree     !! IntrusiveAVLTree object
            CLASS(BSTNode),          POINTER    :: Root   !! pointer to the root node
        END FUNCTION BSTree_GetRoot
        !----------------------------------------------------------------------
        !> To return the largest node in the tree less than or equal to the given node.
        !  Return null pointer if the tree is empty or such node does not exist.
        MODULE FUNCTION BSTree_Floor(Tree, InNode) RESULT(OutNode)
            CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree     !! tree
            CLASS(BSTNode),          INTENT(IN)     :: InNode   !! input node
            CLASS(BSTNode),          POINTER        :: OutNode  !! pointer to floor node
        END FUNCTION BSTree_Floor
        !----------------------------------------------------------------------
        !> To return the smallest node in the tree greater than or equal to the given node.
        !  Return null pointer if the tree is empty or such node does not exist.
        MODULE FUNCTION BSTree_Ceiling(Tree, InNode) RESULT(OutNode)
            CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree     !! tree
            CLASS(BSTNode),          INTENT(IN)     :: InNode   !! input node
            CLASS(BSTNode),          POINTER        :: OutNode  !! pointer to ceiling node
        END FUNCTION BSTree_Ceiling
        !----------------------------------------------------------------------
        !> To return the node in the tree of a given rank.  This node has the property
        !  that there are rank nodes in the tree that are smaller.  In other words,
        !  this node is the (rank+1)st smallest node in the tree. <br>
        !  Applicable range of rank is between 0 and tree_size-1 where this rank number
        !  is zero-based. <br>
        !  Return null pointer if the tree is empty, such node does not exist or the 
        !  input is invalid.
        MODULE FUNCTION BSTree_Select(Tree, Rank) RESULT(OutNode)
            CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree     !! tree
            tIndex,                  INTENT(IN)     :: Rank     !! rank
            CLASS(BSTNode),          POINTER        :: OutNode  !! pointer to output node
        END FUNCTION BSTree_Select
        !----------------------------------------------------------------------
        !> To return the number of nodes in the tree strictly less than the given node.
        MODULE FUNCTION BSTree_Rank(Tree, InNode) RESULT(Rank)
            CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree     !! tree
            CLASS(BSTNode),          INTENT(IN)     :: InNode   !! input node
            tIndex                                  :: Rank     !! rank of key
        END FUNCTION BSTree_Rank
        !----------------------------------------------------------------------
    END INTERFACE
    ! interfaces for specific procedures of the AVLTree type
    INTERFACE
        !----------------------------------------------------------------------
        !> To insert a new node into the tree.  If the tree already contains a node
        !  that is equal to the new node, replace that node with the new one.
        MODULE SUBROUTINE AVLTree_Insert(Tree, NewNode)
            !% IntrusiveAVLTree object
            CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree
            !% a new node to be added to the tree
            CLASS(BSTNode), TARGET,  INTENT(IN)     :: NewNode
        END SUBROUTINE AVLTree_Insert
        !----------------------------------------------------------------------
        !> To remove the specified node from the tree.  Return the flag indicating
        !  whether the node is removed successfully or not.
        MODULE FUNCTION AVLTree_Remove(Tree, DelNode) RESULT(Flag)
            !% IntrusiveAVLTree object
            CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree
            !% the node to be removed
            CLASS(BSTNode),          INTENT(IN)     :: DelNode
            !% true if the specified node is removed successfully
            tLogical                                :: Flag
        END FUNCTION AVLTree_Remove
        !----------------------------------------------------------------------
        !> To remove the node with the smallest value from the tree.  Return
        !  the flag indicating whether the node is removed successfully or not.
        MODULE FUNCTION AVLTree_RemoveMin(Tree, OutNode) RESULT(Flag)
            !% IntrusiveAVLTree object
            CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
            !% the smallest node to be removed
            CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: OutNode
            !% true if the specified node is removed successfully
            tLogical                                            :: Flag
        END FUNCTION AVLTree_RemoveMin
        !----------------------------------------------------------------------
        !> To remove the node with the largest value from the tree.  Return
        !  the flag indicating whether the node is removed successfully or not.
        MODULE FUNCTION AVLTree_RemoveMax(Tree, OutNode) RESULT(Flag)
            !% IntrusiveAVLTree object
            CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
            !% the largest node to be removed
            CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: OutNode
            !% true if the specified node is removed successfully
            tLogical                                            :: Flag
        END FUNCTION AVLTree_RemoveMax
        !----------------------------------------------------------------------
        !> To check integrity of the BST data structure.
        MODULE FUNCTION AVLTree_CheckIntegrity(Tree, Message) RESULT(Flag)
            !% IntrusiveAVLTree object
            CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree
            !> message indicating the reason why the tree did not pass the
            !  integrity test
            tCharAlloc, OPTIONAL,    INTENT(OUT)    :: Message
            !> flag for integrity <br>
            ! - true if the tree passed the integrity test.
            ! - false if the tree did not.
            tLogical                                :: Flag
        END FUNCTION AVLTree_CheckIntegrity
        !----------------------------------------------------------------------
        !> To perform finalization of the tree.
        MODULE SUBROUTINE AVLTree_Finalizer(Tree)
            !% IntrusiveAVLTree object
            TYPE(IntrusiveAVLTree), INTENT(INOUT)   :: Tree
        END SUBROUTINE AVLTree_Finalizer
        !----------------------------------------------------------------------
    END INTERFACE
    ! interfaces for specific procedures of the RBTree type
    INTERFACE
        !----------------------------------------------------------------------
        !> To insert a new node into the tree.  If the tree already contains a node
        !  that is equal to the new node, replace that node with the new one.
        MODULE SUBROUTINE RBTree_Insert(Tree, NewNode)
            !% IntrusiveRBTree object
            CLASS(IntrusiveRBTree), INTENT(INOUT)   :: Tree
            !% a new node to be added to the tree
            CLASS(BSTNode), TARGET, INTENT(IN)      :: NewNode
        END SUBROUTINE RBTree_Insert
        !----------------------------------------------------------------------
        !> To remove the specified node from the tree.  Return the flag indicating
        !  whether the node is removed successfully or not.
        MODULE FUNCTION RBTree_Remove(Tree, DelNode) RESULT(Flag)
            !% IntrusiveRBTree object
            CLASS(IntrusiveRBTree), INTENT(INOUT)   :: Tree
            !% the node to be removed
            CLASS(BSTNode),         INTENT(IN)      :: DelNode
            !% true if the specified node is removed successfully
            tLogical                                :: Flag
        END FUNCTION RBTree_Remove
        !----------------------------------------------------------------------
        !> To remove the node with the smallest value from the tree.  Return
        !  the flag indicating whether the node is removed successfully or not.
        MODULE FUNCTION RBTree_RemoveMin(Tree, OutNode) RESULT(Flag)
            !% IntrusiveRBTree object
            CLASS(IntrusiveRBTree),            INTENT(INOUT)    :: Tree
            !% the smallest node to be removed
            CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: OutNode
            !% true if the specified node is removed successfully
            tLogical                                            :: Flag
        END FUNCTION RBTree_RemoveMin
        !----------------------------------------------------------------------
        !> To remove the node with the largest value from the tree.  Return
        !  the flag indicating whether the node is removed successfully or not.
        MODULE FUNCTION RBTree_RemoveMax(Tree, OutNode) RESULT(Flag)
            !% IntrusiveRBTree object
            CLASS(IntrusiveRBTree),            INTENT(INOUT)    :: Tree
            !% the largest node to be removed
            CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: OutNode
            !% true if the specified node is removed successfully
            tLogical                                            :: Flag
        END FUNCTION RBTree_RemoveMax
        !----------------------------------------------------------------------
        !> To check integrity of the BST data structure.
        MODULE FUNCTION RBTree_CheckIntegrity(Tree, Message) RESULT(Flag)
            !% IntrusiveRBTree object
            CLASS(IntrusiveRBTree), INTENT(INOUT)   :: Tree
            !> message indicating the reason why the tree did not pass the
            !  integrity test
            tCharAlloc, OPTIONAL,   INTENT(OUT)     :: Message
            !> flag for integrity <br>
            ! - true if the tree passed the integrity test.
            ! - false if the tree did not.
            tLogical                                :: Flag
        END FUNCTION RBTree_CheckIntegrity
        !----------------------------------------------------------------------
        !> To perform finalization of the tree.
        MODULE SUBROUTINE RBTree_Finalizer(Tree)
            !% IntrusiveRBTree object
            TYPE(IntrusiveRBTree), INTENT(INOUT)   :: Tree
        END SUBROUTINE RBTree_Finalizer
        !----------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Routines for BSTNode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION BSTNode_GetLeftNode(CurrNode) RESULT(LeftNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get the left node of the current node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), INTENT(IN)  :: CurrNode !! current node
    CLASS(BSTNode), POINTER     :: LeftNode !! left node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    LeftNode => CurrNode%Left

    RETURN

END FUNCTION BSTNode_GetLeftNode

!******************************************************************************

FUNCTION BSTNode_GetRightNode(CurrNode) RESULT(RightNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get the right node of the current node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), INTENT(IN)  :: CurrNode     !! current node
    CLASS(BSTNode), POINTER     :: RightNode    !! right node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    RightNode => CurrNode%Right

    RETURN

END FUNCTION BSTNode_GetRightNode

!******************************************************************************

FUNCTION BSTNode_GetSize(CurrNode) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get number of nodes in subtree rooted by the current node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), INTENT(IN)  :: CurrNode !! current node
    tIndex                      :: Size     !! number of nodes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = CurrNode%Size

    RETURN

END FUNCTION BSTNode_GetSize

!******************************************************************************

SUBROUTINE BSTNode_FreePointers(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !! To nullify the pointer components of the node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), INTENT(INOUT)   :: Node     !! current node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    NULLIFY(Node%Left)
    NULLIFY(Node%Right)
       
    RETURN

END SUBROUTINE BSTNode_FreePointers

!******************************************************************************

SUBROUTINE BSTNode_SetNew(Node, IsRBTree)

!** PURPOSE OF THIS SUBROUTINE:
    !! To set the node as a new node in the tree container.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% a node to be set as a new node in the tree
    CLASS(BSTNode), INTENT(INOUT)   :: Node
    !% flag indicating whether the node is used with a red-black tree
    tLogical,       INTENT(IN)      :: IsRBTree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Node%Size   = 1_kIndex
    IF (IsRBTree) THEN
        Node%HC = Red
    ELSE
        Node%HC = 0_kIndex
    END IF
    RETURN

END SUBROUTINE BSTNode_SetNew

!******************************************************************************

SUBROUTINE BSTNode_Reset(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !! To reset the node to its initialized state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), INTENT(INOUT)   :: Node !! node to be reset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Node%Size = 0_kIndex
    Node%HC   = Black
    CALL Node%FreePointers()
       
    RETURN

END SUBROUTINE BSTNode_Reset

!******************************************************************************

SUBROUTINE BSTNode_CopyBSTNode(SrcNode, DstNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy members of the source to the destination.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), INTENT(IN)      :: SrcNode  !! source
    CLASS(BSTNode), INTENT(INOUT)   :: DstNode  !! destination

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL SrcNode%CopyCompNode(DstNode)
    DstNode%Size = SrcNode%Size
    DstNode%HC   = SrcNode%HC
    DstNode%Left  => SrcNode%Left
    DstNode%Right => SrcNode%Right
       
    RETURN

END SUBROUTINE BSTNode_CopyBSTNode

!******************************************************************************

FUNCTION BSTNode_EqualTo(LhsNode, RhsNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether all members of both objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), INTENT(IN)  :: LhsNode  !! a node
    CLASS(BSTNode), INTENT(IN)  :: RhsNode  !! another node
    tLogical                    :: Flag     !! true if both nodes are equal to one another

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! set default value
    Flag = FalseVal
    IF (.NOT.LhsNode%IsPIndxEqual(RhsNode)) RETURN
    IF (LhsNode%Size /= RhsNode%Size) RETURN
    IF (LhsNode%HC /= RhsNode%HC) RETURN
    IF (.NOT.ASSOCIATED(LhsNode%Left,  RhsNode%Left)) RETURN
    IF (.NOT.ASSOCIATED(LhsNode%Right, RhsNode%Right)) RETURN
    Flag = TrueVal
       
    RETURN

END FUNCTION BSTNode_EqualTo

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Auxiliary Procedures
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION NodeSize(Node) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get number of nodes in subtree rooted by this node

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN) :: Node     ! BSTNode object
    tIndex                              :: Size     ! number of nodes in subtree rooted by this node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (.NOT.ASSOCIATED(Node)) THEN
        Size = 0
    ELSE
        Size = Node%Size
    END IF
       
    RETURN

END FUNCTION NodeSize

!******************************************************************************

SUBROUTINE Find_MinNode(Node, MinNode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To find the leftmost leaf of the given node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN)     :: Node     ! input node
    CLASS(BSTNode), POINTER, INTENT(OUT)    :: MinNode  ! output node

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

END SUBROUTINE Find_MinNode

!******************************************************************************

RECURSIVE SUBROUTINE Find_Inorder_Predecessor(Root, InNode, PrvNode)

!** PURPOSE OF THIS SUBROUTINE:
    !> To search for the previous node in inorder traversal of the tree
    !   of the specified node. <br>
    ! Note: The first call of this routine should provide the root
    !   of the tree as an input node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN)     :: Root     !! input node
    CLASS(BSTNode),          INTENT(IN)     :: InNode   !! the specified node
    CLASS(BSTNode), POINTER, INTENT(INOUT)  :: PrvNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: Temp => NULL()

! FLOW
        
    ! base case
    IF (.NOT.ASSOCIATED(Root)) RETURN
        
    ! If key is equal to the key of the root
    IF (InNode == Root) THEN
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
        
    IF (InNode < Root) THEN
        ! If node is smaller than the root, go to left subtree
        CALL Find_Inorder_Predecessor(Root%Left, InNode, PrvNode)
    ELSE
        ! Otherwise, go to right subtree
        PrvNode => Root
        CALL Find_Inorder_Predecessor(Root%Right, InNode, PrvNode)
    END IF

    RETURN

END SUBROUTINE Find_Inorder_Predecessor

!******************************************************************************

RECURSIVE SUBROUTINE Find_Inorder_Successor(Root, InNode, NxtNode)

!** PURPOSE OF THIS SUBROUTINE:
    !> To search for the next node in inorder traversal of the tree
    !   of the specified node. <br>
    ! Note: The first call of this routine should provide the root
    !   of the tree as an input node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN)     :: Root     !! input node
    CLASS(BSTNode),          INTENT(IN)     :: InNode   !! the specified node
    CLASS(BSTNode), POINTER, INTENT(INOUT)  :: NxtNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: Temp => NULL()

! FLOW
        
    ! base case
    IF (.NOT.ASSOCIATED(Root)) RETURN
        
    ! If key is equal to the key of the root
    IF (InNode == Root) THEN
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
        
    IF (InNode < Root) THEN
        ! If node is smaller than the root, go to left subtree
        NxtNode => Root
        CALL Find_Inorder_Successor(Root%Left, InNode, NxtNode)
    ELSE
        ! Otherwise, go to right subtree
        CALL Find_Inorder_Successor(Root%Right, InNode, NxtNode)
    END IF

    RETURN

END SUBROUTINE Find_Inorder_Successor

!******************************************************************************

END MODULE MClass_IntrusiveBSTrees

!******************************************************************************
