
MODULE MClass_IntrusiveLinkedLists

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains *intrusive-based* linked-list container types and their
!   related routines.  The module provides application programming interfaces (APIs)
!   for two linked-list containers: the linear list (by the *IntrusiveLinearList*
!   type) and the ring list (by *IntrusiveRingList* type).  The module also provides
!   a base linked node (*DoublyLinkedNode*) type to be used with both containers. <br>
!   The *DoublyLinkedNode* type is a base linked node type that can connect to
!   its adjacent nodes in two directions.  The node allows a doubly-linked list to
!   be formed.  This indicates that both containers are doubly-linked list containers. 
!   To use the list containers provided, a user must define a new node type that extends
!   the *DoublyLinkedNode* type.  This new type typically contains user data as its
!   additional component(s). <br>
!   The *IntrusiveLinearList* type is an *intrusive linearly-linked-list* container
!   while the *IntrusiveRingList* type is an *intrusive circularly-linked-list* container.
!   Unlike the *IntrusiveLinearList* type, the next pointer of the last node of the
!   *IntrusiveRingList* list points to its first node, rather than pointing to NULL.
!   Similarly, the previous pointer of the first node of the *IntrusiveRingList* list
!   points to its last node allowing the list to be formed as a ring.  Both containers
!   stores objects (or nodes) in the *DoublyLinkedNode* class (i.e. objects/nodes that
!   are subtypes of the *DoublyLinkedNode* type). <br>
!  <br>
!  **Intrusive Technique**: <br>
!   Both intrusive list container types employ the so-called **intrusive** technique.
!   To illustrate the intrusive technique and highlight its distinction from the
!   non-intrusive (i.e. tradition) one, let consider a derived type that defines
!   a point in a 2-dimensional coordinate system.
!   <Pre><Code style="color:MidnightBlue;">
!   TYPE Point2D
!       REAL   :: X, Y
!   END TYPE
!   </Code></Pre>
!   In a traditional non-intrusive implementation of a doubly-linked list for
!   a 2-D point data, a linked node is then usually defined as:
!   <Pre><Code style="color:MidnightBlue;">
!   TYPE LinkedNode
!       TYPE(Point2D)               :: Data
!       TYPE(LinkedNode), POINTER   :: Next => NULL()
!       TYPE(LinkedNode), POINTER   :: Prev => NULL()
!   END TYPE
!   </Code></Pre>
!   To add a 2-D point data to the non-intrusive doubly-linked list, the conventional
!   implementation commonly performs the following steps: <br>
!   1. Allocating a *LinkedNode*. <br>
!   2. Copying the 2-D point input data into the *Data* component of the *LinkedNode*. <br>
!   3. Linking the *Next* and *Prev* pointers of the *LinkedNode* into the list. <br>
!   <br>
!   Removing a 2-D point data requires similar steps in reverse order: <br>
!   1. Unlinking the two pointers. <br>
!   2. Copying the *Data* component to a 2-D point output data. <br>
!   3. Deallocating the *LinkedNode*. <br>
!  <br>
!   In an implementation of an intrusive doubly-linked list, a base linked node
!   can be defined as:
!   <Pre><Code style="color:MidnightBlue;">
!   TYPE BaseNode
!       CLASS(BaseNode), POINTER   :: Next => NULL()
!       CLASS(BaseNode), POINTER   :: Prev => NULL()
!   END TYPE
!   </Code></Pre>
!   Then, when using the intrusive list, an object for a 2-D point may be defined
!   as:
!   <Pre><Code style="color:MidnightBlue;">
!   TYPE, EXTENDS(BaseNode) :: Point2DNode
!       TYPE(Point2D)   :: Data
!   END TYPE
!   </Code></Pre>
!   or it can also be defined as:
!   <Pre><Code style="color:MidnightBlue;">
!   TYPE, EXTENDS(BaseNode) Point2DObj
!       REAL   :: X, Y
!   END TYPE
!   </Code></Pre>
!   Both 2-D point objects are in the *BaseNode* class; hence, they both can
!   be used with the intrusive list.  <br>
!   At first glance, both 2-D point objects shown for the intrusive list appear to
!   be very similar to the *LinkedNode* object shown for the non-intrusive list.
!   However, unlike the non-intrusive implementation, only the *linking/unlinking*
!   task is done by the intrusive list when adding/removing a 2-D point object to/from
!   the list.  The memory management (i.e. allocation and/or deallocation) task of the
!   2-D point object is external to the intrusive list and thus must be managed by the
!   user who is the owner of the 2-D point object.  The *Copying* task is typically not
!   necessary for the intrusive list case because the 2-D point object already contains
!   the 2-D point data where the user (as the owner of the object) can manipulate the
!   data directly. <br>
!  <br>
!   **Pros:**  <br>
!   As demonstrated above, the intrusive implementation has some advantages over
!   a traditional implementation including:  <br>
!   - Both intrusive list types provided here can be used as generic containers.
!     To use an intrusive container, a user must define an object that extends the
!     *DoublyLinkedNode* type and contains user data as additional component(s) where
!     the data can be defined by any data types (i.e. intrinsic or derived types). <br>
!   - An intrusive container requires fewer memory allocations/deallocations than
!     a generic non-intrusive container using unlimited polymorphism.  In Fortran,
!     a generic linked list employing unlimited polymorphism requires two memory
!     allocations when adding data to the list: one for the data and one for the
!     linked node.  On the other hand, the intrusive list requires only one memory
!     allocation for the user object as the data is already embedded in the object. <br>
!   - Because the user is the owner of the object, the *Copying* task is usually not
!     required for an intrusive container whereas it is essential for a non-intrusive
!     container as previously discussed. <br>
!  <br>
!   **Cons:**  <br>
!   The intrusive technique also has some disadvantages as follows: <br>
!   - As discussed above, a user must always define a user object as a subtype of the
!     *DoublyLinkedNode* type in order to use either the *IntrusiveLinearList* type
!     or the *IntrusiveRingList* type as an intrusive list container. <br>
!   - As the owner of objects inserted into an intrusive container, the user is also
!     responsible for their memory management.  This means that the user must manage
!     the lifetime of all inserted objects and be careful that these objects are not
!     destroyed (e.g. deallocated) before they get removed from the container. <br>
!   - Rather than storing a copy of an inserted object, an intrusive container just
!     links the inserted object with other previously inserted objects in the container.
!     Therefore, the user must be aware of the possible side effects whenever he/she
!     changes the data content of those inserted objects.  This is especially important
!     if the intrusive container is an ordered one.  <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseNodePool,    ONLY: BaseNode

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: DoublyLinkedNode
    PUBLIC :: IntrusiveLinearList
    PUBLIC :: IntrusiveRingList
    PUBLIC :: IfaceNodeEqual
    PUBLIC :: IfaceNodeCompare

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_IntrusiveLinkedLists'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,  PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    !> *DoublyLinkedNode* is a doubly-linked node type that contains two pointer
    !   components that can point to its adjacent nodes (next node and previous
    !   node) in two directions, which allow a doubly-linked list to be formed.
    !   Unlike a traditional (non-intrusive) list node, the *DoublyLinkedNode* type
    !   contains no data content.  Therefore, an intrusive container working with
    !   nodes in the *DoublyLinkedNode* class can operate without having to manage
    !   the data content. <br>
    !   However, to use the intrusive container properly, for example, a user must
    !   define a new node type that extends the *DoublyLinkedNode* type as follows:
    !   <Pre><Code style="color:MidnightBlue;">
    !   ! a list-node object with default integer as type of its data content
    !   TYPE, EXTENDS(DoublyLinkedNode) :: IntegerListNode
    !       INTEGER     :: content      ! stored content
    !   END TYPE
    !   </Code></Pre>
    !   Because this new node type is a subtype of the *DoublyLinkedNode* type, it
    !   can be used with any intrusive list containers for the available operations.
    !   As previously discussed, an intrusive container only provides linking and
    !   unlinking capabilities and thus operates without the memory management of
    !   the data content.  This means that the user must manage the lifetime of
    !   inserted objects (i.e. the new node type) and be careful that these objects
    !   are not destroyed before they get removed from the container.
    TYPE, ABSTRACT, EXTENDS(BaseNode) :: DoublyLinkedNode
        PRIVATE
        CLASS(DoublyLinkedNode), POINTER   :: Next => NULL()   ! pointer to next node
        CLASS(DoublyLinkedNode), POINTER   :: Prev => NULL()   ! pointer to previous node
    CONTAINS
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetNext <br>
        !  **Purpose**:  To get a pointer to the next node of the current node. <br>
        !  **Usage**: <br>
        !   --->    NextNode => CurrNode%GetNext()
        PROCEDURE   :: GetNext          => ListNode_GetNextNode
        !> **Type-Bound Function**: GetPrevious <br>
        !  **Purpose**:  To get a pointer to the previous node of the current node. <br>
        !  **Usage**: <br>
        !   --->    PrevNode => CurrNode%GetPrevious()
        PROCEDURE   :: GetPrevious      => ListNode_GetPreviousNode
        !> **Type-Bound Subroutine**: SetNext <br>
        !  **Purpose**:  To set a pointer to the next node of the current node. <br>
        !  **Usage**: <br>
        !   --->    CALL CurrNode%SetNext(NextNode)
        PROCEDURE   :: SetNext          => ListNode_SetNextNode
        !> **Type-Bound Subroutine**: SetPrevious <br>
        !  **Purpose**:  To set a pointer to the previous node of the current node. <br>
        !  **Usage**: <br>
        !   --->    CALL CurrNode%SetPrevious(PrevNode)
        PROCEDURE   :: SetPrevious      => ListNode_SetPreviousNode
        !> **Type-Bound Subroutine**: FreePointers <br>
        !  **Purpose**:  To nullify the pointer components of the node. <br>
        !  **Usage**: <br>
        !   --->    CALL CurrNode%FreePointers()
        PROCEDURE   :: FreePointers     => ListNode_FreePointers
        !> **Type-Bound Subroutine**: CopyDLLNode <br>
        !  **Purpose**:  To copy members of the source to the destination.  This method
        !                is provided to help user implement deferred procedure(s) required
        !                by an object in the *Object* class. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcNode%CopyDLLNode(DstNode)
        PROCEDURE   :: CopyDLLNode      => ListNode_CopyDLLNode
        !> **Type-Bound Function**: IsDLLNodeEqual <br>
        !  **Purpose**:  To compare whether all members of both objects are equal or not.
        !                This method is provided to help user implement deferred procedure(s)
        !                required by an object in the *Object* class. <br>
        !  **Usage**: <br>
        !   --->    Flag = LhsNode%IsDLLNodeEqual(RhsNode) <br>
        !   --->    IF (.NOT.LhsNode%IsDLLNodeEqual(RhsNode)) DoSomething
        PROCEDURE   :: IsDLLNodeEqual  => ListNode_EqualTo
        ! ---------------------------------------------------------------------
    END TYPE DoublyLinkedNode
    !> *IntrusiveRingList* is an intrusive circularly-linked list container type
    !   that provides common linked-list operations without a memory management
    !   task.  The management of objects inserted in the list must be done by
    !   a user, which is the owner of those objects. <br>
    !   See the <a href="../module/MClass_IntrusiveLinkedLists.html#type-doublylinkednode">DoublyLinkedNode</a>
    !   type for how to define an object to be inserted into an intrusive container. <br>
    !   Similar to the *IntrusiveLinearList* type, the *IntrusiveRingList* type provides
    !   four methods that allow iterations over its objects in two directions.  See the
    !   <a href="../module/MClass_IntrusiveLinkedLists.html#type-intrusivelinearlist">IntrusiveLinearList</a>
    !   for various illustrations of an iteration over an intrusive list. <br>
    !   Nonetheless, unlike the *IntrusiveLinearList* type, the meanings of the returned
    !   *EndOfList* flag from the *MoveForward* and *MoveBackward* methods are NOT the same
    !   for the two intrusive list types.  For the linear list, the end of list means that
    !   the next pointer of the cursor node points to NULL (i.e. its association status is
    !   disassociated).  However, for the circular list, the end of list means that the
    !   next pointer of the cursor node points back to the starting node.  Therefore, for
    !   the circular list, the user can ignore the *EndOfList* flag and loop over the list
    !   once more if necessary without having to call the *StartFirst* or *StartLast*
    !   method again. <br>
    !   It is important to note that the intrusive circular list allows insertion of
    !   duplicated objects (i.e. objects that are equal to one another).  It even allows
    !   the same object to be inserted twice (i.e. it does not check  whether the same
    !   object is actually inserted more than once).  Therefore, it is the user as the
    !   owner of inserted objects who must be responsible for how to handle those inserted
    !   objects properly.
    TYPE IntrusiveRingList
        PRIVATE
        ! size of the list container (i.e. number of nodes in the list)
        tIndex                              :: Size = 0
        ! pointer to the first node (or the head node) of the list
        CLASS(DoublyLinkedNode), POINTER    :: Head    => NULL()
        ! pointer to the current node used for iteration purpose
        CLASS(DoublyLinkedNode), POINTER    :: Cursor  => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----             Cloning Procedure                             -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CloneTo <br>
        !  **Purpose**:  To perform cloning of the source list. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcList%CloneTo(DstList) <br>
        PROCEDURE   :: CloneTo          => RingList_Clone
        ! ---------------------------------------------------------------------
        ! -----             Adding and Removing Procedures                -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: AddFirst <br>
        !  **Purpose**:  To insert a new node to the front of the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%AddFirst(NewNode) <br>
        !  **Note**:  For the circular list, the *AddFirst* and *AddLast* methods are
        !             mostly the same since the new node is inserted between the head
        !             (the first node inserted) and the tail (the last one inserted),
        !             except that the *AddFirst* method set the *Head* pointer to 
        !             the new node whereas the *AddLast* method does not.
        PROCEDURE   :: AddFirst         => RingList_AddFirst
        !> **Type-Bound Subroutine**: AddLast <br>
        !  **Purpose**:  To insert a new node to the back of the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%AddLast(NewNode)
        PROCEDURE   :: AddLast          => RingList_AddLast
        !> **Type-Bound Function**: AddAt <br>
        !  **Purpose**:  To add a new node at the specified position of the list where
        !                the position must be between 1 and the list size.  Also, return
        !                a flag indicating whether the node is successfully added. <br>
        !  **Usage**: <br>
        !   --->    Success = List%AddAt(NewNode, 3) <br>
        !   --->    IF (.NOT.List%AddAt(NewNode, 5)) DoSomething
        PROCEDURE   :: AddAt            => RingList_AddAt
        !> **Type-Bound Function**: AddBefore <br>
        !  **Purpose**:  To add a new node into the list before the specified node.  Also,
        !                return a flag indicating whether the node is successfully added.
        !                If the list is empty or the specified node does not exist in the
        !                list, return false.  Otherwise, return true. <br>
        !  **Usage**: <br>
        !   --->    Success = List%AddBefore(NewNode, ThisNode) <br>
        !   --->    IF (.NOT.List%AddBefore(NewNode, ThisNoode)) DoSomething
        PROCEDURE   :: AddBefore        => RingList_AddBefore
        !> **Type-Bound Function**: AddAfter <br>
        !  **Purpose**:  To add a new node into the list after the specified node.  Also,
        !                return a flag indicating whether the node is successfully added.
        !                If the list is empty or the specified node does not exist in the
        !                list, return false.  Otherwise, return true. <br>
        !  **Usage**: <br>
        !   --->    Success = List%AddAfter(NewNode, ThisNode) <br>
        !   --->    IF (.NOT.List%AddAfter(NewNode, ThisNoode)) DoSomething
        PROCEDURE   :: AddAfter         => RingList_AddAfter
        !> **Type-Bound Function**: RemoveFirst <br>
        !  **Purpose**:  To remove the first (head) node from the list.  Also, return
        !                a flag indicating whether the node is successfully removed.<br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveFirst() <br>
        !   --->    IF (.NOT.List%RemoveFirst(HeadNode)) DoSomething
        PROCEDURE   :: RemoveFirst      => RingList_RemoveFirst
        !> **Type-Bound Function**: RemoveLast <br>
        !  **Purpose**:  To remove the last (tail) node from the list.  Also, return
        !                a flag indicating whether the node is successfully removed.<br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveLast() <br>
        !   --->    IF (.NOT.List%RemoveLast(TailNode)) DoSomething
        PROCEDURE   :: RemoveLast       => RingList_RemoveLast
        !> **Type-Bound Function**: RemoveAt <br>
        !  **Purpose**:  To remove the Nth node from the list.  The index N is one-based
        !                where N <= the size of the list.   Also, return a flag indicating
        !                whether the node is successfully removed.<br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveAt(5) <br>
        !   --->    Success = List%RemoveAt(7, Node7) <br>
        !   --->    IF (.NOT.List%RemoveAt(3, Node3)) DoSomething
        PROCEDURE   :: RemoveAt         => RingList_RemoveAt
        !> **Type-Bound Function**: RemoveNode <br>
        !  **Purpose**:  To remove a node associated with the specified node from the list.
        !                Also, return a flag indicating whether the node is successfully
        !                removed. <br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveNode(NodeA) <br>
        !   --->    IF (.NOT.List%RemoveNode(NodeB)) DoSomething
        PROCEDURE   :: RemoveNode       => RingList_RemoveNode
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all nodes from the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Clear()
        PROCEDURE   :: Clear            => RingList_Clear
        ! ---------------------------------------------------------------------
        ! -----                 Iteration Procedures                      -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag
        !                indicating whether the list is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = List%StartFirst() <br>
        !   --->    IsEmpty = List%StartFirst(FirstNode)
        PROCEDURE   :: StartFirst       => RingList_StartFirst
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start the *backward* iteration and return a flag
        !                indicating whether the list is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = List%StartLast() <br>
        !   --->    IsEmpty = List%StartLast(FirstNode)
        PROCEDURE   :: StartLast        => RingList_StartLast
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move *forward* to the next iteration and return a flag
        !                indicating whether the cursor has reached the end of the
        !                list or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = List%MoveForward() <br>
        !   --->    IsTheEnd = List%MoveForward(NextNode) <br>
        PROCEDURE   :: MoveForward      => RingList_Move2Next
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move *backward* to the next iteration and return a flag
        !                indicating whether the cursor has reached the end of the
        !                list or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = List%MoveBackward() <br>
        !   --->    IsTheEnd = List%MoveBackward(PrevNode) <br>
        PROCEDURE   :: MoveBackward     => RingList_Move2Prev
        ! ---------------------------------------------------------------------
        ! -----               Retrieving Procedures                      ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetHead <br>
        !  **Purpose**:  To get a pointer to the first node. <br>
        !  **Usage**: <br>
        !   --->    FirstNode => List%GetHead()
        PROCEDURE   :: GetHead          => RingList_GetHead
        !> **Type-Bound Function**: GetTail <br>
        !  **Purpose**:  To get a pointer to the last node. <br>
        !  **Usage**: <br>
        !   --->    LastNode => List%GetTail()
        PROCEDURE   :: GetTail          => RingList_GetTail
        !> **Type-Bound Function**: GetCursor <br>
        !  **Purpose**:  To get a pointer to the node the cursor points to.  
        !       This usually points to the current node when performing an
        !       iteration over the list. <br>
        !  **Usage**: <br>
        !   --->    CurrNode => List%GetCursor()
        PROCEDURE   :: GetCursor        => RingList_GetCursor
        !> **Type-Bound Function**: GetAt <br>
        !  **Purpose**:  To get a pointer to the Nth node where N must be
        !                between 1 and the list size.  If the list is empty
        !                or N is not in a valid range, return Null pointer.<br>
        !  **Usage**: <br>
        !   --->    CurrNode => List%GetAt(5)
        PROCEDURE   :: GetAt            => RingList_GetAt
        ! ---------------------------------------------------------------------
        ! -----               Inquiry Procedures                         ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get size of the container. <br>
        !  **Usage**: <br>
        !   --->    ListSize = List%GetSize()
        PROCEDURE   :: GetSize          => RingList_GetSize
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the container is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = List%IsEmpty() <br>
        !   --->    IF (.NOT.List%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty          => RingList_IsEmpty
        ! ---------------------------------------------------------------------
        ! -----         Searching and Sorting Procedures                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To check whether the specified node is currently stored in
        !                the container or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = List%Contain(NodeA) <br>
        !   --->    IF (.NOT.List%Contain(NodeB)) DoSomething
        PROCEDURE   :: Contain          => RingList_Contain
        !> **Type-Bound Function**: IndexOf <br>
        !  **Purpose**:  To determine the index indicating the position where the
        !                specified node is stored in the list.  Return the one-based
        !                index of the first node found or return zero if the given
        !                node is not stored in the list. <br>
        !  **Usage**: <br>
        !   --->    Index = List%IndexOf(NodeA)
        PROCEDURE   :: IndexOf          => RingList_IndexOf
        !> **Type-Bound Function**: LastIndexOf <br>
        !  **Purpose**:  To determine the index indicating the position where the
        !                given node is stored in the list.  Return the one-based
        !                index of the last node found (i.e. the first node when
        !                searching backward from the tail node) or return the
        !                list size plus one if the given node is not stored in
        !                the list. <br>
        !  **Usage**: <br>
        !   --->    Index = List%LastIndexOf(NodeA)
        PROCEDURE   :: LastIndexOf      => RingList_LastIndexOf
        !> **Type-Bound Function**: FindFirstEqual <br>
        !  **Purpose**:  To determine whether there is a node in the list that is
        !                equal to the specified node.  Return the one-based index
        !                indicating the position of the first node found or return
        !                zero if none of the nodes is equal to the given node. <br>
        !  **Usage**: <br>
        !   --->    Index = List%FindFirstEqual(NodeA, IsEqualTo) <br>
        !  **Note**:  User must supply the *IsEqualTo* function to check whether two
        !             objects (nodes) in the *DoublyLinkedNode* class are equal to
        !             one another or not.
        PROCEDURE   :: FindFirstEqual        => RingList_FindFirstEqual
        !> **Type-Bound Function**: FindLastEqual <br>
        !  **Purpose**:  To determine whether there is a node in the list that is
        !                equal to the specified node.  Return the one-based index
        !                indicating the position of the last node found (i.e. the
        !                first node when searching backward from the tail node) or
        !                return the list size plus one if none of the nodes is equal
        !                to the given node. <br>
        !  **Usage**: <br>
        !   --->    Index = List%FindLastEqual(NodeA, IsEqualTo) <br>
        !  **Note**:  User must supply the *IsEqualTo* function to check whether two
        !             objects (nodes) in the *DoublyLinkedNode* class are equal to
        !             one another or not.
        PROCEDURE   :: FindLastEqual        => RingList_FindLastEqual
        !> **Type-Bound Subroutine**: SortAscend <br>
        !  **Purpose**:  To sort nodes in the list in ascending order. <br>
        !  **Usage**: <br>
        !   --->    CALL List%SortAscend(Compare) <br>
        !  **Note**:  User must supply the *Compare* function to compare two objects
        !             (nodes) in the *DoublyLinkedNode* class.
        PROCEDURE   :: SortAscend       => RingList_SortAscend
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the list.
        FINAL       :: RingList_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE IntrusiveRingList
    !> *IntrusiveLinearList* is an intrusive doubly-linked list container type
    !   that provides common linked-list operations without a memory management
    !   task.  The management of objects inserted in the list must be done by
    !   a user, which is the owner of those objects. <br>
    !   See the <a href="../module/MClass_IntrusiveLinkedLists.html#type-doublylinkednode">DoublyLinkedNode</a>
    !   type for how to define an object to be inserted into the intrusive list. <br>
    !   As a doubly-linked list type, the *IntrusiveLinearList* type provides
    !   four methods that allow iterations over its objects in two directions.
    !   The following code snippet illustrates how to typically traverse the list.
    !   <Pre><Code style="color:MidnightBlue;">
    !   ! start forward iteration (from the first or head node)
    !   IsEmpty = List%StartFirst()
    !   IF (.NOT.IsEmpty) DoSomeThing...
    !   DO
    !       ! move to the next iteration
    !       IsTheEnd = List%MoveForward()
    !       ! check whether we reach the end of the list or not
    !       IF (IsTheEnd) EXIT
    !       ! if not, do the task we need
    !       DoSomeThing...
    !   END DO
    !   </Code></Pre>
    !   The following code snippet shows another way to iterate over the list.
    !   <Pre><Code style="color:MidnightBlue;">
    !   ! start forward iteration (from the first or head node)
    !   IsTheEnd = List%StartFirst(CurrNode)
    !   DO WHILE (.NOT.IsTheEnd)
    !       DoSomeThing_With_CurrNode...
    !       ! move to the next iteration
    !       IsTheEnd = List%MoveForward(CurrNode)
    !   END DO
    !   </Code></Pre>
    !   In addition, the following code snippet shows how to iterate over the list
    !   in reverse order of insertion.
    !   <Pre><Code style="color:MidnightBlue;">
    !   ! start backward iteration (from the last or tail node)
    !   IsTheEnd = List%StartLast(CurrItem)
    !   DO WHILE (.NOT.IsTheEnd)
    !       DoSomeThing_With_CurrItem...
    !       ! move to the next iteration
    !       IsTheEnd = List%MoveBackward(CurrItem)
    !   END DO
    !   </Code></Pre>
    !   Like the *IntrusiveRingList* type, the linear list also allows insertion of
    !   duplicated objects (i.e. objects that are equal to one another) and it does
    !   not check whether the same object is inserted twice or not.  Therefore, it
    !   is the user as the owner of inserted objects who must be responsible for how
    !   to handle those inserted objects appropriately.
    TYPE, EXTENDS(IntrusiveRingList) :: IntrusiveLinearList
        PRIVATE
        ! pointer to the last node (or the tail node) of the list
        CLASS(DoublyLinkedNode), POINTER    :: Tail    => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! ++++++++++++++++++    Overriding Procedures    ++++++++++++++++++++++
        ! ---------------------------------------------------------------------
        ! -----             Cloning Procedure                             -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CloneTo <br>
        !  **Purpose**:  To perform cloning of the source list. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcList%CloneTo(DstList) <br>
        PROCEDURE   :: CloneTo          => LinearList_Clone
        ! ---------------------------------------------------------------------
        ! -----             adding and removing procedures                -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: AddFirst <br>
        !  **Purpose**:  To insert a new node to the front of the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%AddFirst(NewNode)
        PROCEDURE   :: AddFirst         => LinearList_AddFirst
        !> **Type-Bound Subroutine**: AddLast <br>
        !  **Purpose**:  To insert a new node to the back of the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%AddLast(NewNode)
        PROCEDURE   :: AddLast          => LinearList_AddLast
        !> **Type-Bound Function**: AddAt <br>
        !  **Purpose**:  To add a new node at the specified position of the list where
        !                the position must be between 1 and the list size.  Also, return
        !                a flag indicating whether the node is successfully added. <br>
        !  **Usage**: <br>
        !   --->    Success = List%AddAt(NewNode, 3) <br>
        !   --->    IF (.NOT.List%AddAt(NewNode, 5)) DoSomething
        PROCEDURE   :: AddAt            => LinearList_AddAt
        !> **Type-Bound Function**: AddAfter <br>
        !  **Purpose**:  To add a new node into the list after the specified node.  Also,
        !                return a flag indicating whether the node is successfully added.
        !                If the list is empty or the specified node does not exist in the
        !                list, return false.  Otherwise, return true. <br>
        !  **Usage**: <br>
        !   --->    Success = List%AddAfter(NewNode, ThisNode) <br>
        !   --->    IF (.NOT.List%AddAfter(NewNode, ThisNoode)) DoSomething
        PROCEDURE   :: AddAfter         => LinearList_AddAfter
        !> **Type-Bound Function**: RemoveFirst <br>
        !  **Purpose**:  To remove the first (head) node from the list.  Also, return
        !                a flag indicating whether the node is successfully removed.<br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveFirst() <br>
        !   --->    IF (.NOT.List%RemoveFirst(HeadNode)) DoSomething
        PROCEDURE   :: RemoveFirst      => LinearList_RemoveFirst
        !> **Type-Bound Function**: RemoveLast <br>
        !  **Purpose**:  To remove the last (tail) node from the list.  Also, return
        !                a flag indicating whether the node is successfully removed.<br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveLast() <br>
        !   --->    IF (.NOT.List%RemoveLast(TailNode)) DoSomething
        PROCEDURE   :: RemoveLast       => LinearList_RemoveLast
        !> **Type-Bound Function**: RemoveAt <br>
        !  **Purpose**:  To remove the Nth node from the list.  The index N is one-based
        !                where N <= the size of the list.   Also, return a flag indicating
        !                whether the node is successfully removed.<br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveAt(5) <br>
        !   --->    Success = List%RemoveAt(7, Node7) <br>
        !   --->    IF (.NOT.List%RemoveAt(3, Node3)) DoSomething
        PROCEDURE   :: RemoveAt         => LinearList_RemoveAt
        !> **Type-Bound Function**: RemoveNode <br>
        !  **Purpose**:  To remove a node associated with the specified node from the list.
        !                Also, return a flag indicating whether the node is successfully
        !                removed. <br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveNode(NodeA) <br>
        !   --->    IF (.NOT.List%RemoveNode(NodeB)) DoSomething
        PROCEDURE   :: RemoveNode       => LinearList_RemoveNode
        !> **Type-Bound Function**: ReplaceNode <br>
        !  **Purpose**:  To replace a node associated with the specified old node with the specified
        !                new node.  Also, return a flag indicating whether the node is successfully
        !                replaced. <br>
        !  **Usage**: <br>
        !   --->    Success = List%ReplaceNode(OldNode, NewNode) <br>
        !   --->    IF (.NOT.List%ReplaceNode(OldNode, NewNode)) DoSomething
        PROCEDURE   :: ReplaceNode      => LinearList_ReplaceNode
        ! ---------------------------------------------------------------------
        ! -----                 Iteration procedures                      -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start the *backward* iteration and return a flag
        !                indicating whether the list is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = List%StartLast() <br>
        !   --->    IsEmpty = List%StartLast(FirstNode)
        PROCEDURE   :: StartLast        => LinearList_StartLast
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move *forward* to the next iteration and return a flag
        !                indicating whether the cursor has reached the end of the
        !                list or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = List%MoveForward() <br>
        !   --->    IsTheEnd = List%MoveForward(NextNode) <br>
        PROCEDURE   :: MoveForward      => LinearList_Move2Next
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move *backward* to the next iteration and return a flag
        !                indicating whether the cursor has reached the end of the
        !                list or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = List%MoveBackward() <br>
        !   --->    IsTheEnd = List%MoveBackward(PrevNode) <br>
        PROCEDURE   :: MoveBackward     => LinearList_Move2Prev
        ! ---------------------------------------------------------------------
        ! -----               Retrieving Procedures                      ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetTail <br>
        !  **Purpose**:  To get a pointer to the last node. <br>
        !  **Usage**: <br>
        !   --->    LastNode => List%GetTail()
        PROCEDURE   :: GetTail          => LinearList_GetTail
        ! ---------------------------------------------------------------------
        ! -----                 Queue Procedures                         ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: EnQueue <br>
        ! **Purpose**:  To add a new item to the end of the queue. <br>
        !  **Usage**: <br>
        !   --->    CALL Queue%EnQueue(NewItem) <br>
        !  **Note**: *EnQueue* is an alias of *AddLast*.
        PROCEDURE   :: EnQueue  => LinearList_AddLast
        !> **Type-Bound Function**: DeQueue <br>
        !  **Purpose**:  To get and remove the front (first) item of the queue.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Queue%DeQueue(Item) <br>
        !   --->    IF (.NOT.Queue%DeQueue(Item)) DoSomething <br>
        !  **Note**: *DeQueue* is an alias of *RemoveFirst*.
        PROCEDURE   :: DeQueue  => LinearList_RemoveFirst
        ! ---------------------------------------------------------------------
        ! -----                 Stack Procedures                         ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Push <br>
        ! **Purpose**:  To add a new item to the top of the stack. <br>
        !  **Usage**: <br>
        !   --->    CALL Stack%Push(NewItem) <br>
        !  **Note**: *Push* is an alias of *AddLast*.
        PROCEDURE   :: Push     => LinearList_AddLast
        !> **Type-Bound Function**: Pop <br>
        !  **Purpose**:  To get and remove the top item of the stack.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Stack%Pop(Item) <br>
        !   --->    IF (.NOT.Stack%Pop(Item)) DoSomething <br>
        !  **Note**: *Pop* is an alias of *RemoveLast*.
        PROCEDURE   :: Pop      => LinearList_RemoveLast
        !> **Type-Bound Function**: PeekTop <br>
        !  **Purpose**:  To get the last item (without removing it from the stack).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    TopNode => Stack%PeekTop() <br>
        !  **Note**: *PeekTop* is an alias of *GetTail*.
        PROCEDURE   :: PeekTop  => LinearList_GetTail
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the list.
        FINAL       :: LinearList_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE IntrusiveLinearList

!** INTERFACE DEFINITIONS:
    ! abstract interfaces for user-supplied procedures
    ABSTRACT INTERFACE
        !> *IfaceNodeEqual* is an interface for a procedure to check whether two objects in the
        !  *DoublyLinkedNode* class are equal to one another or not.  The output flag should be
        !  set to true if they are equal. Otherwise, the flag should be set to false.
        FUNCTION IfaceNodeEqual(A, B) RESULT(Flag)
            IMPORT
            CLASS(DoublyLinkedNode), INTENT(IN) :: A
            CLASS(DoublyLinkedNode), INTENT(IN) :: B
            tLogical                            :: Flag
        END FUNCTION
        !> *IfaceNodeCompare* is an interface for a procedure to compare two objects in the 
        !  *DoublyLinkedNode* class where the output flag should be set to the following value: <br>
        !   1 (or positive value) if A is greater than B, <br>
        !   0 if A is equal to B, <br>
        !  -1 (or negative value) if A is less than B.
        FUNCTION IfaceNodeCompare(A,B) RESULT(Flag)
            IMPORT
            CLASS(DoublyLinkedNode), INTENT(IN) :: A
            CLASS(DoublyLinkedNode), INTENT(IN) :: B
            tSInt32                            :: Flag
        END FUNCTION
    END INTERFACE
    ! interfaces for the IntrusiveRingList type-bound procedures
    INTERFACE
        !----------------------------------------------------------------------
        !> To perform cloning of the source list.
        MODULE SUBROUTINE RingList_Clone(SrcList, DstList)
            !% source IntrusiveRingList object
            CLASS(IntrusiveRingList), INTENT(IN)    :: SrcList
            !% destination IntrusiveRingList object
            CLASS(IntrusiveRingList), INTENT(OUT)   :: DstList
        END SUBROUTINE RingList_Clone
        !----------------------------------------------------------------------
        !> To remove all nodes from the list.
        MODULE SUBROUTINE RingList_Clear(List, DelinkOnly)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList), INTENT(INOUT) :: List
            tLogical, OPTIONAL,       INTENT(IN)    :: DelinkOnly
        END SUBROUTINE RingList_Clear
        !----------------------------------------------------------------------
        !> To perform finalization of the object
        MODULE SUBROUTINE RingList_Finalizer(List)
            !% IntrusiveRingList object
            TYPE(IntrusiveRingList), INTENT(INOUT)  :: List
        END SUBROUTINE RingList_Finalizer
        !----------------------------------------------------------------------
        !> To add a new node to the front of the list.
        MODULE SUBROUTINE RingList_AddFirst(List, NewNode)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
            !% node to be added to the list
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
        END SUBROUTINE RingList_AddFirst
        !----------------------------------------------------------------------
        !> To add a new node to the back of the list.
        MODULE SUBROUTINE RingList_AddLast(List, NewNode)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
            !% node to be added to the list
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
        END SUBROUTINE RingList_AddLast
        !----------------------------------------------------------------------
        !> To add a new node at the Nth position of the list where N must be
        !  between 1 and the list size.  Also, return a flag indicating whether
        !  the node is successfully added.  If the list is empty, just add the
        !  new node to the list where N is simply ignored.
        MODULE FUNCTION RingList_AddAt(List, NewNode, N) RESULT(Flag)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
            !% node to be added to the list
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
            !% one-based index indicating position to add the node
            tIndex,                          INTENT(IN)     :: N
            !> flag indicating whether the node is successfully added. <br>
            ! - true if the node is successfully added. <br>
            ! - false if the node is NOT successfully added.
            tLogical                                        :: Flag
        END FUNCTION RingList_AddAt
        !----------------------------------------------------------------------
        !> To add a new node into the list before the specified node (*ThisNode*).
        !  Also, return a flag indicating whether the node is successfully added.
        !  If the list is empty or the *ThisNode* node does not exist in the list,
        !  return false.  Otherwise, return true.
        MODULE FUNCTION RingList_AddBefore(List, NewNode, ThisNode) RESULT(Flag)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
            !% node to be added to the list
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
            !% node that the NewNode is inserted just before it
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: ThisNode
            !> flag indicating whether the node is successfully added. <br>
            ! - true if the node is successfully added. <br>
            ! - false if the node is NOT successfully added.
            tLogical                                        :: Flag
        END FUNCTION RingList_AddBefore
        !----------------------------------------------------------------------
        !> To add a new node into the list after the specified node (*ThisNode*).
        !  Also, return a flag indicating whether the node is successfully added.
        !  If the list is empty or the *ThisNode* node does not exist in the list,
        !  return false.  Otherwise, return true.
        MODULE FUNCTION RingList_AddAfter(List, NewNode, ThisNode) RESULT(Flag)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
            !% node to be added to the list
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
            !% node that the NewNode is inserted just after it
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: ThisNode
            !> flag indicating whether the node is successfully added. <br>
            ! - true if the node is successfully added. <br>
            ! - false if the node is NOT successfully added.
            tLogical                                        :: Flag
        END FUNCTION RingList_AddAfter
        !----------------------------------------------------------------------
        !> To remove the first (head) node from the list. Also, return
        !  a flag indicating whether the node is successfully removed.
        MODULE FUNCTION RingList_RemoveFirst(List, NodeOut) RESULT(Flag)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
            !% pointer to the first node
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !> flag indicating whether the node is successfully removed. <br>
            ! - true if the node is successfully removed. <br>
            ! - false if the node is NOT successfully removed.
            tLogical                                                    :: Flag
        END FUNCTION RingList_RemoveFirst
        !----------------------------------------------------------------------
        !> To remove the last (tail) node from the list. Also, return
        !  a flag indicating whether the node is successfully removed.
        MODULE FUNCTION RingList_RemoveLast(List, NodeOut) RESULT(Flag)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
            !% pointer to the last node
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !> flag indicating whether the node is successfully removed. <br>
            ! - true if the node is successfully removed. <br>
            ! - false if the node is NOT successfully removed.
            tLogical                                                    :: Flag
        END FUNCTION RingList_RemoveLast
        !----------------------------------------------------------------------
        !> To remove a node associated with the specified node from the list.
        ! Also, return a flag indicating whether the node is successfully removed.
        MODULE FUNCTION RingList_RemoveNode(List, CurrNode) RESULT(Flag)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
            !% the node to be removed
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: CurrNode
            !> flag indicating whether the node is successfully removed. <br>
            ! - true if the node is successfully removed. <br>
            ! - false if the node is NOT successfully removed.
            tLogical                                        :: Flag
        END FUNCTION RingList_RemoveNode
        !----------------------------------------------------------------------
        !> To remove the Nth node from the list where N must be between 1 and the list size.
        !  Also, return a flag indicating whether the node is successfully removed.
        MODULE FUNCTION RingList_RemoveAt(List, N, NodeOut) RESULT(Flag)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
            !% (one-based) index indicating the node to be removed
            tIndex,                                     INTENT(IN)      :: N
            !% pointer to the Nth node (null pointer if N is not valid)
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !> flag indicating whether the node is successfully removed. <br>
            ! - true if the node is successfully removed. <br>
            ! - false if the node is NOT successfully removed.
            tLogical                                                    :: Flag
        END FUNCTION RingList_RemoveAt
        !----------------------------------------------------------------------
        !> To start a forward iteration by setting the cursor pointer to the head node
        !  of the list and return a flag indicating whether the list is empty or not.
        MODULE FUNCTION RingList_StartFirst(List, NodeOut) RESULT(IsEmpty)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
            !% pointer to the starting node
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !% true if the list is empty
            tLogical                                                    :: IsEmpty
        END FUNCTION RingList_StartFirst
        !----------------------------------------------------------------------
        !> To start a backward iteration by setting the cursor pointer to the tail node
        !  of the list and return a flag indicating whether the list is empty or not.
        MODULE FUNCTION RingList_StartLast(List, NodeOut) RESULT(IsEmpty)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
            !% pointer to the starting node
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !% true if the list is empty
            tLogical                                                    :: IsEmpty
        END FUNCTION RingList_StartLast
        !----------------------------------------------------------------------
        !> To move to the next node of the forward iteration and return a flag
        !  indicating whether the cursor has reached the end of the list or not.
        MODULE FUNCTION RingList_Move2Next(List, NodeOut) RESULT(IsTheEnd)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
            !% pointer to the next node
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !% true if the cursor pointer has reached the end of the list
            tLogical                                                    :: IsTheEnd
        END FUNCTION RingList_Move2Next
        !----------------------------------------------------------------------
        !> To move to the next node of the backward iteration and return a flag
        !  indicating whether the cursor has reached the end of the list or not.
        MODULE FUNCTION RingList_Move2Prev(List, NodeOut) RESULT(IsTheEnd)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
            !% pointer to the next node
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !% true if the cursor pointer has reached the end of the list
            tLogical                                                    :: IsTheEnd
        END FUNCTION RingList_Move2Prev
        !----------------------------------------------------------------------
        !> To check whether the list is empty or not
        MODULE FUNCTION RingList_IsEmpty(List) RESULT(Flag)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList), INTENT(IN)    :: List
            !% true if the list is empty
            tLogical                                :: Flag
        END FUNCTION RingList_IsEmpty
        !----------------------------------------------------------------------
        !> To get size of the list (a number of nodes).
        MODULE FUNCTION RingList_GetSize(List) RESULT(Size)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList), INTENT(IN)    :: List
            !% list size (number of nodes)
            tIndex                                  :: Size
        END FUNCTION RingList_GetSize
        !----------------------------------------------------------------------
        !> To get a pointer to the head (first node) of the list
        MODULE FUNCTION RingList_GetHead(List) RESULT(Head)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList), INTENT(IN)    :: List
            !% pointer to the head
            CLASS(DoublyLinkedNode),  POINTER       :: Head
        END FUNCTION RingList_GetHead
        !----------------------------------------------------------------------
        !> To get a pointer to the tail (last node) of the list
        MODULE FUNCTION RingList_GetTail(List) RESULT(Tail)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList), INTENT(IN)    :: List
            !% pointer to the tail
            CLASS(DoublyLinkedNode),  POINTER       :: Tail
        END FUNCTION RingList_GetTail
        !----------------------------------------------------------------------
        !> To get a pointer to the cursor node of the list
        MODULE FUNCTION RingList_GetCursor(List) RESULT(Cursor)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList), INTENT(IN)    :: List
            !% pointer to the cursor node
            CLASS(DoublyLinkedNode),  POINTER       :: Cursor
        END FUNCTION RingList_GetCursor
        !----------------------------------------------------------------------
        !> To get the Nth node from the list where N is between 1 and the list size.
        !  If the list is empty or N is not in a valid range, return Null pointer.
        MODULE FUNCTION RingList_GetAt(List, N) RESULT(NodeOut)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList), INTENT(INOUT) :: List
            !% (one-based) index indicating the node
            tIndex,                   INTENT(IN)    :: N
            !% pointer to the Nth node
            CLASS(DoublyLinkedNode),  POINTER       :: NodeOut
        END FUNCTION RingList_GetAt
        !----------------------------------------------------------------------
        !^ To check whether the specified node is currently stored in the list.
        MODULE FUNCTION RingList_Contain(List, NodeIn, NodeOut) RESULT(Flag)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
            !% node to be looked for
            CLASS(DoublyLinkedNode),           TARGET,  INTENT(IN)      :: NodeIn
            !% a pointer to a stored node equal to the specified one
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !% true if the node is stored in the list
            tLogical                                                    :: Flag
        END FUNCTION RingList_Contain
        !----------------------------------------------------------------------
        !> To determine the index indicating the position where the specified node
        !  is stored in the list.  Return the one-based index of the first node found
        !  or return zero if the given node is not stored in the list.
        MODULE FUNCTION RingList_IndexOf(List, NodeIn) RESULT(Index)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
            !% input node
            CLASS(DoublyLinkedNode), TARGET, INTENT(IN)     :: NodeIn
            !% one-based index indicating the position of the first node found
            tSInt32                                        :: Index
        END FUNCTION RingList_IndexOf
        !----------------------------------------------------------------------
        !> To determine the index indicating the position where the specified node
        !  is stored in the list.  Return the one-based index of the last node found
        !  (i.e. the first node when searching backward from the tail node) or return
        !  the list size plus one if the given node is not stored in the list.
        MODULE FUNCTION RingList_LastIndexOf(List, NodeIn) RESULT(Index)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
            !% input node
            CLASS(DoublyLinkedNode), TARGET, INTENT(IN)     :: NodeIn
            !% one-based index indicating the position of the last node found
            tSInt32                                        :: Index
        END FUNCTION RingList_LastIndexOf
        !----------------------------------------------------------------------
        !> To determine whether there is a node in the list that is equal to the
        !  specified node.  Return the one-based index indicating the position of
        !  the first node found or return zero if none of the nodes is equal to
        !  the given node.
        MODULE FUNCTION RingList_FindFirstEqual(List, NodeIn, IsEqualTo) RESULT(Index)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList), INTENT(INOUT) :: List
            !% the node to be found
            CLASS(DoublyLinkedNode),  INTENT(IN)    :: NodeIn
            !% procedure to check whether two objects (nodes) are equal to one another or not
            PROCEDURE(IfaceNodeEqual)               :: IsEqualTo
            !% one-based index indicating the position of the first node found
            tSInt32                                 :: Index
        END FUNCTION RingList_FindFirstEqual
        !----------------------------------------------------------------------
        !> To determine whether there is a node in the list that is equal to the given
        !  node.  Return the one-based index indicating the position of the last node
        !  found (i.e. the first node when searching backward from the tail node) or
        !  return the list size plus one if none of the nodes is equal to the given node.
        MODULE FUNCTION RingList_FindLastEqual(List, NodeIn, IsEqualTo) RESULT(Index)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList), INTENT(INOUT) :: List
            !% the node to be found
            CLASS(DoublyLinkedNode),  INTENT(IN)    :: NodeIn
            !% procedure to check whether two objects (nodes) are equal to one another or not
            PROCEDURE(IfaceNodeEqual)               :: IsEqualTo
            !% one-based index indicating the position of the last node found
            tSInt32                                 :: Index
        END FUNCTION RingList_FindLastEqual
        !----------------------------------------------------------------------
        !> To sort nodes in the list in ascending order.  The top-down
        !  merge sort algorithm is employed here.
        MODULE RECURSIVE SUBROUTINE RingList_SortAscend(List, Compare)
            !% IntrusiveRingList object
            CLASS(IntrusiveRingList), INTENT(INOUT) :: List
            !% procedure to compare two objects (nodes)
            PROCEDURE(IfaceNodeCompare)             :: Compare
        END SUBROUTINE RingList_SortAscend
        !----------------------------------------------------------------------
    END INTERFACE
    ! interfaces for the IntrusiveLinearList type-bound procedures
    INTERFACE
        !----------------------------------------------------------------------
        !> To perform cloning of the source list.
        MODULE SUBROUTINE LinearList_Clone(SrcList, DstList)
            !% source IntrusiveLinearList object
            CLASS(IntrusiveLinearList), INTENT(IN)  :: SrcList
            !% destination IntrusiveRingList object
            CLASS(IntrusiveRingList),   INTENT(OUT) :: DstList
        END SUBROUTINE LinearList_Clone
        !----------------------------------------------------------------------
        !> To perform finalization of the object
        MODULE SUBROUTINE LinearList_Finalizer(List)
            !% IntrusiveLinearList object
            TYPE(IntrusiveLinearList), INTENT(INOUT)    :: List
        END SUBROUTINE LinearList_Finalizer
        !----------------------------------------------------------------------
        !> To add a new node to the front of the list.
        MODULE SUBROUTINE LinearList_AddFirst(List, NewNode)
            !% IntrusiveLinearList object
            CLASS(IntrusiveLinearList),      INTENT(INOUT)  :: List
            !% node to be added to the list
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
        END SUBROUTINE LinearList_AddFirst
        !----------------------------------------------------------------------
        !> To add a new node to the back of the list.
        MODULE SUBROUTINE LinearList_AddLast(List, NewNode)
            !% IntrusiveLinearList object
            CLASS(IntrusiveLinearList),      INTENT(INOUT)  :: List
            !% node to be added to the list
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
        END SUBROUTINE LinearList_AddLast
        !----------------------------------------------------------------------
        !> To add a new node at the Nth position of the list where N must be
        !  between 1 and the list size.  Also, return a flag indicating whether
        !  the node is successfully added.  If the list is empty, just add the
        !  new node to the list where N is simply ignored.
        MODULE FUNCTION LinearList_AddAt(List, NewNode, N) RESULT(Flag)
            !% IntrusiveLinearList object
            CLASS(IntrusiveLinearList),      INTENT(INOUT)  :: List
            !% node to be added to the list
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
            !% one-based index indicating position to add the node
            tIndex,                          INTENT(IN)     :: N
            !> flag indicating whether the node is successfully added. <br>
            ! - true if the node is successfully added. <br>
            ! - false if the node is NOT successfully added.
            tLogical                                        :: Flag
        END FUNCTION LinearList_AddAt
        !----------------------------------------------------------------------
        !> To add a new node into the list after the specified node (*ThisNode*).
        !  Also, return a flag indicating whether the node is successfully added.
        !  If the list is empty or the *ThisNode* node does not exist in the list,
        !  return false.  Otherwise, return true.
        MODULE FUNCTION LinearList_AddAfter(List, NewNode, ThisNode) RESULT(Flag)
            !% IntrusiveLinearList object
            CLASS(IntrusiveLinearList),      INTENT(INOUT)  :: List
            !% node to be added to the list
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
            !% node that the NewNode is inserted just after it
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: ThisNode
            !> flag indicating whether the node is successfully added. <br>
            ! - true if the node is successfully added. <br>
            ! - false if the node is NOT successfully added.
            tLogical                                        :: Flag
        END FUNCTION LinearList_AddAfter
        !----------------------------------------------------------------------
        !> To remove the first (head) node from the list. Also, return
        !  a flag indicating whether the node is successfully removed.
        MODULE FUNCTION LinearList_RemoveFirst(List, NodeOut) RESULT(Flag)
            !% IntrusiveLinearList object
            CLASS(IntrusiveLinearList),                 INTENT(INOUT)   :: List
            !% pointer to the first node
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !> flag indicating whether the node is successfully removed. <br>
            ! - true if the node is successfully removed. <br>
            ! - false if the node is NOT successfully removed.
            tLogical                                                    :: Flag
        END FUNCTION LinearList_RemoveFirst
        !----------------------------------------------------------------------
        !> To remove the last (tail) node from the list. Also, return
        !  a flag indicating whether the node is successfully removed.
        MODULE FUNCTION LinearList_RemoveLast(List, NodeOut) RESULT(Flag)
            !% IntrusiveLinearList object
            CLASS(IntrusiveLinearList),                 INTENT(INOUT)   :: List
            !% pointer to the last node
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !> flag indicating whether the node is successfully removed. <br>
            ! - true if the node is successfully removed. <br>
            ! - false if the node is NOT successfully removed.
            tLogical                                                    :: Flag
        END FUNCTION LinearList_RemoveLast
        !----------------------------------------------------------------------
        !> To remove a node associated with the specified node from the list.
        ! Also, return a flag indicating whether the node is successfully removed.
        MODULE FUNCTION LinearList_RemoveNode(List, CurrNode) RESULT(Flag)
            !% IntrusiveLinearList object
            CLASS(IntrusiveLinearList),      INTENT(INOUT)  :: List
            !% the node to be removed
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: CurrNode
            !> flag indicating whether the node is successfully removed. <br>
            ! - true if the node is successfully removed. <br>
            ! - false if the node is NOT successfully removed.
            tLogical                                        :: Flag
        END FUNCTION LinearList_RemoveNode
        !----------------------------------------------------------------------
        !> To remove the Nth node from the list where N must be between 1 and the list size.
        !  Also, return a flag indicating whether the node is successfully removed.
        MODULE FUNCTION LinearList_RemoveAt(List, N, NodeOut) RESULT(Flag)
            !% IntrusiveLinearList object
            CLASS(IntrusiveLinearList),                 INTENT(INOUT)   :: List
            !% (one-based) index indicating the node to be removed
            tIndex,                                     INTENT(IN)      :: N
            !% pointer to the Nth node (null pointer if N is not valid)
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !> flag indicating whether the node is successfully removed. <br>
            ! - true if the node is successfully removed. <br>
            ! - false if the node is NOT successfully removed.
            tLogical                                                    :: Flag
        END FUNCTION LinearList_RemoveAt
        !----------------------------------------------------------------------
        !> To replace a node associated with the specified old node from the list
        !  with the specified new node. Also, return a flag indicating whether
        !  the old node is successfully replaced.
        MODULE FUNCTION LinearList_ReplaceNode(List, OldNode, NewNode) RESULT(Flag)
            !% IntrusiveLinearList object
            CLASS(IntrusiveLinearList),      INTENT(INOUT)  :: List
            !% the old node to be replaced
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: OldNode
            !% the node to be removed
            CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
            !> flag indicating whether the old node is successfully replaced. <br>
            ! - true if the node is successfully replaced. <br>
            ! - false if the node is NOT successfully replaced.
            tLogical                                        :: Flag
        END FUNCTION LinearList_ReplaceNode
        !----------------------------------------------------------------------
        !> To start a backward iteration by setting the cursor pointer to the tail node
        !  of the list and return a flag indicating whether the list is empty or not.
        MODULE FUNCTION LinearList_StartLast(List, NodeOut) RESULT(IsEmpty)
            !% IntrusiveLinearList object
            CLASS(IntrusiveLinearList),                 INTENT(INOUT)   :: List
            !% pointer to the starting node
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !% true if the list is empty
            tLogical                                                    :: IsEmpty
        END FUNCTION LinearList_StartLast
        !----------------------------------------------------------------------
        !> To move to the next node of the forward iteration and return a flag
        !  indicating whether the cursor has reached the end of the list or not.
        MODULE FUNCTION LinearList_Move2Next(List, NodeOut) RESULT(IsTheEnd)
            !% IntrusiveLinearList object
            CLASS(IntrusiveLinearList),                 INTENT(INOUT)   :: List
            !% pointer to the next node
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !% true if the cursor pointer has reached the end of the list
            tLogical                                                    :: IsTheEnd
        END FUNCTION LinearList_Move2Next
        !----------------------------------------------------------------------
        !> To move to the next node of the backward iteration and return a flag
        !  indicating whether the cursor has reached the end of the list or not.
        MODULE FUNCTION LinearList_Move2Prev(List, NodeOut) RESULT(IsTheEnd)
            !% IntrusiveLinearList object
            CLASS(IntrusiveLinearList),                 INTENT(INOUT)   :: List
            !% pointer to the next node
            CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
            !% true if the cursor pointer has reached the end of the list
            tLogical                                                    :: IsTheEnd
        END FUNCTION LinearList_Move2Prev
        !----------------------------------------------------------------------
        !> To get a pointer to the tail (last node) of the list
        MODULE FUNCTION LinearList_GetTail(List) RESULT(Tail)
            !% IntrusiveLinearList object
            CLASS(IntrusiveLinearList), INTENT(IN)  :: List
            !% pointer to the tail
            CLASS(DoublyLinkedNode),    POINTER     :: Tail
        END FUNCTION LinearList_GetTail
        !----------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Routines for DoublyLinkedNode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION ListNode_GetNextNode(CurrNode) RESULT(NextNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get the next node of the current node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DoublyLinkedNode), INTENT(IN) :: CurrNode !! current node
    CLASS(DoublyLinkedNode), POINTER    :: NextNode !! next node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    NextNode => CurrNode%Next

    RETURN

END FUNCTION ListNode_GetNextNode

!******************************************************************************

FUNCTION ListNode_GetPreviousNode(CurrNode) RESULT(PrevNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get the previous node of the current node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DoublyLinkedNode), INTENT(IN) :: CurrNode !! current node
    CLASS(DoublyLinkedNode), POINTER    :: PrevNode !! previous node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    PrevNode => CurrNode%Prev

    RETURN

END FUNCTION ListNode_GetPreviousNode

!******************************************************************************

SUBROUTINE ListNode_SetNextNode(CurrNode, NextNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To set the next node of the current node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DoublyLinkedNode),         INTENT(INOUT)  :: CurrNode !! current node
    CLASS(DoublyLinkedNode), TARGET, INTENT(IN)     :: NextNode !! next node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CurrNode%Next => NextNode

    RETURN

END SUBROUTINE ListNode_SetNextNode

!******************************************************************************

SUBROUTINE ListNode_SetPreviousNode(CurrNode, PrevNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To set the previous node of the current node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DoublyLinkedNode),         INTENT(INOUT)  :: CurrNode !! current node
    CLASS(DoublyLinkedNode), TARGET, INTENT(IN)     :: PrevNode !! previous node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CurrNode%Prev => PrevNode

    RETURN

END SUBROUTINE ListNode_SetPreviousNode

!******************************************************************************

SUBROUTINE ListNode_FreePointers(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !! To nullify the pointer components of the node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DoublyLinkedNode), INTENT(INOUT) :: Node     !! current node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    NULLIFY(Node%Next)
    NULLIFY(Node%Prev)
       
    RETURN

END SUBROUTINE ListNode_FreePointers

!******************************************************************************

SUBROUTINE ListNode_CopyDLLNode(SrcNode, DstNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy members of the source to the destination.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DoublyLinkedNode), INTENT(IN)     :: SrcNode  !! source
    CLASS(DoublyLinkedNode), INTENT(INOUT)  :: DstNode  !! destination

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL SrcNode%CopyBaseNode(DstNode)
    DstNode%Next => SrcNode%Next
    DstNode%Prev => SrcNode%Prev
       
    RETURN

END SUBROUTINE ListNode_CopyDLLNode

!******************************************************************************

FUNCTION ListNode_EqualTo(LhsNode, RhsNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether all members of both objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DoublyLinkedNode), INTENT(IN) :: LhsNode  !! a node
    CLASS(DoublyLinkedNode), INTENT(IN) :: RhsNode  !! another node
    tLogical                            :: Flag     !! true if both nodes are equal to one another

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! set default value
    Flag = FalseVal
    IF (.NOT.LhsNode%IsPIndxEqual(RhsNode)) RETURN
    IF (.NOT.ASSOCIATED(LhsNode%Next, RhsNode%Next)) RETURN
    IF (.NOT.ASSOCIATED(LhsNode%Prev, RhsNode%Prev)) RETURN
    Flag = TrueVal
       
    RETURN

END FUNCTION ListNode_EqualTo

! ---------------------------------------------------------------------
! -----                 Auxiliary Procedures                      -----
! ---------------------------------------------------------------------

FUNCTION IsIndexValid(Size, Index) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To check whether the specified index is between 1
    !  and the collection size or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: Size     !! the collection size
    tIndex, INTENT(IN)  :: Index    !! the specified index
    tLogical            :: Flag     !! true if the index is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal
    IF (Index < 1) RETURN
    IF (Index > Size) RETURN
    Flag = TrueVal

    RETURN

END FUNCTION IsIndexValid

!******************************************************************************

END MODULE MClass_IntrusiveLinkedLists

!******************************************************************************
