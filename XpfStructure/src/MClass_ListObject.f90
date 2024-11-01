
MODULE MClass_ListObject

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *ListObject* type and related routines.
!   The *ListObject* type is a container with *CLASS(Object)* as the type
!   of its stored items.  It employs a conventional doubly-linked list
!   implementation.  All derived types extending from the *Object* type can be
!   used with this container. <br>
!   It should be noted that although the *ListObject* type allows items with
!   different types (any derived types that are in the *Object* class) to be
!   stored in different nodes of the same container, a user must be extremely
!   careful when retrieving the items with different types from the container.
!   The type specified for an output argument must match the type of an item
!   stored in a particular node of the container.  Otherwise, the user would
!   not be able to retrieve the item for that specific routine. <br>
!   See the <a href="../module/mbase_doublylinkedlists.html">MBase_DoublyLinkedLists</a>
!   module for an overview and usage notes of a *doubly-linked-list-based* type.
!   A user may use the *MBase_DoublyLinkedLists* module instead of using this
!   module directly. <br>
!   See the <a href="../module/mclass_linkedlists.html">MClass_LinkedLists</a>
!   module for doubly-linked-list-based types of containers that are functionally
!   similar to the *ListObject* type but utilizes a different implementation.
!   Also, unlike the *ListObject* type, these container types are designed as
!   generic containers that can be used to store various data types providing that
!   the size (in bytes) of the data to be stored is known at compile time. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil,           ONLY: ToChar => ToDecStrSigned
    USE MBase_ErrHandlers
    USE MBase_MemHandlers
    USE MClass_Object

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: ListObject

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_ListObject'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    !> *DLLNode* is a node type that consists of an item and two pointers of the node type.
    !   As such, the node can point to its adjacent nodes in two directions (next node and
    !   previous node) allowing a doubly-linked list to be formed. <br>
    !   The type of the item stored in this node is a derived type in the *Object*
    !   class (i.e. the *Object* type or its subtypes).  The *DLLNode* type is a
    !   private type.
    TYPE DLLNode
        PRIVATE
        !> *Item* is an item (or value) stored in the node where its type can
        !   be any derived type that extends from the *Object* type.
        CLASS(Object), ALLOCATABLE  :: Item
        !% pointer to the next node
        TYPE(DLLNode),     POINTER      :: Next   => NULL()
        !% pointer to previous node
        TYPE(DLLNode),     POINTER      :: Prev   => NULL()
    CONTAINS
        ! destructor procedure
        PROCEDURE, PRIVATE  :: Destruct => LinkedNode_Destructor
    END TYPE DLLNode
    !> *ListObject* is a container type that employs a doubly-linked list implementation to
    !   provide common operations for a list container.  It can also represent other forms
    !   of containers including a LIFO stack, a FIFO queue and a double-ended queue (deque).
    !   The type of items stored in this container is a derived type in the *Object*
    !   class (i.e. the *Object* type or its subtypes).
    TYPE ListObject
        PRIVATE
        !% size of the list container (i.e. number of nodes in the list)
        tIndex                  :: Size = 0
        !% pointer to the first node (or the head node) of the list
        TYPE(DLLNode), POINTER  :: Head   => NULL()
        !% pointer to the last node (or the tail node) of the list
        TYPE(DLLNode), POINTER  :: Tail   => NULL()
        !% pointer to the current node used for iteration purpose
        TYPE(DLLNode), POINTER  :: Cursor => NULL()
        !> direction of the iteration <br>
        !  - positive -> forward iteration
        !  - negative -> backward iteration
        !  - zero     -> iteration not yet start
        tSInt32               :: Dir = 0
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----             PRIVATE PROCEDURES                            -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: SetNewNode   => LinkedList_SetNewNode
        PROCEDURE, PRIVATE  :: RemoveNode   => LinkedList_RemoveNode
        PROCEDURE, PRIVATE  :: GetNodeAt    => LinkedList_GetNodeAt
        PROCEDURE, PRIVATE  :: Traverse     => LinkedList_Traverse
        ! ---------------------------------------------------------------------
        ! -----             PUBLIC PROCEDURES                             -----
        ! ---------------------------------------------------------------------
        ! -----             Constructor and Destructor Procedures         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        ! **Purpose**:  To construct a list from an array of items <br>
        !  **Usage**: <br>
        !   --->    CALL List%Construct(10, Arr)    ! create a list from an array of 10 items
        PROCEDURE   :: Construct    => LinkedList_CreateByArray
        !> **Type-Bound Subroutine**: Destruct <br>
        ! **Purpose**:  To destruct a list and get its items if requested <br>
        !  **Usage**: <br>
        !   --->    CALL List%Destruct()    ! destruct the list <br>
        !  **Note**: This method is equivalent to the *Clear* method.
        PROCEDURE   :: Destruct     => LinkedList_Destructor
        ! ---------------------------------------------------------------------
        ! -----             Insertion and Removal Procedures              -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: AddFirst <br>
        !  **Purpose**:  To insert the specified item at the front of the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%AddFirst(Item)
        PROCEDURE   :: AddFirst         => LinkedList_AddFirst
        !> **Type-Bound Subroutine**: AddLast <br>
        !  **Purpose**:  To insert the specified item at the end of the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%AddLast(Item)
        PROCEDURE   :: AddLast          => LinkedList_AddLast
        !> **Type-Bound Function**: AddAt <br>
        !  **Purpose**:  To insert the specified item at the specified index where
        !                the index must be between 1 and the list size.
        !                Also, return a flag indicating whether the item is
        !                successfully added. <br>
        !  **Usage**: <br>
        !   --->    Success = List%AddAt(Index, Item) <br>
        !   --->    IF (.NOT.List%AddAt(Index, Item)) DoSomething
        PROCEDURE   :: AddAt            => LinkedList_AddAt
        !> **Type-Bound Subroutine**: Remove <br>
        ! **Purpose**:  To remove an item from the list.  The first item is removed
        !               by default.  If specified, the last item can be removed instead. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Remove()          ! remove the first item <br>
        !   --->    CALL List%Remove(.FALSE.)   ! remove the last item <br>
        !   --->    CALL List%Remove(Item=Item) ! retrieve and the remove the first item <br>
        !  **Important Note**: The specified type of item to be retrieved must match that
        !   of an item stored in the list.
        PROCEDURE   :: Remove           => LinkedList_Remove
        !> **Type-Bound Function**: RemoveFirst <br>
        !  **Purpose**:  To get and remove the front (first) item of the list.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveFirst(Item) <br>
        !   --->    IF (.NOT.List%RemoveFirst(Item)) DoSomething <br>
        !  **Important Note**: The specified type of item to be retrieved must match that
        !   of an item stored in the list.
        PROCEDURE   :: RemoveFirst      => LinkedList_RemoveFirst
        !> **Type-Bound Function**: RemoveLast <br>
        !  **Purpose**:  To get and remove the last item of the list.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveLast(Item) <br>
        !   --->    IF (.NOT.List%RemoveLast(Item)) DoSomething <br>
        !  **Important Note**: The specified type of item to be retrieved must match that
        !   of an item stored in the list.
        PROCEDURE   :: RemoveLast       => LinkedList_RemoveLast
        !> **Type-Bound Function**: RemoveAt <br>
        !  **Purpose**:  To get and remove the item at the specified index where
        !                the index must be between 1 and the list size.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveAt(Index, Item) <br>
        !   --->    IF (.NOT.List%RemoveAt(Index, Item)) DoSomething <br>
        !  **Important Note**: The specified type of item to be retrieved must match that
        !   of an item stored in the list.
        PROCEDURE   :: RemoveAt         => LinkedList_RemoveAt
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete an item from the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the
        !   *StartFirst* and *MoveForward* methods (or the *StartLast* and
        !   *MoveBackward* methods).  Therefore, after the call to one of those
        !   methods and then calling this one will result in a removal of the
        !   current item of the iteration (i.e. the same item that can be retrieved
        !   via those iteration methods).
        PROCEDURE   :: Delete           => LinkedList_Delete
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Clear()
        PROCEDURE   :: Clear            => LinkedList_ClearItems
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the list.  Also, return a flag
        !                indicating whether the items are successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = List%ToArray(Items) <br>
        !   --->    IF (.NOT.List%ToArray(Items)) DoSomething <br>
        !  **Important Note**: This operation will not be successful if items stored
        !   in different nodes have different types.
        PROCEDURE   :: ToArray          => LinkedList_ToArray
        !> **Type-Bound Subroutine**: RemoveDuplicates <br>
        !  **Purpose**:  To remove nodes with duplicated items from the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%RemoveDuplicates()
        PROCEDURE   :: RemoveDuplicates => LinkedList_RemoveDuplicates
        ! ---------------------------------------------------------------------
        ! -----                 Iteration Procedures                      -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag indicating
        !                whether the list is empty or not. <br>
        !  **Usage**: see *MoveForward* procedure.
        PROCEDURE   :: StartFirst       => LinkedList_Move2FirstElm
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move *forward* to the next iteration and return a flag
        !                indicating whether the cursor pointer has reached the end
        !                of the list or not. <br>
        !  **Usage**: <br>
        !   The following code snippet illustrates how to typically traverse the list.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start forward iteration (from the first item)
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
        !   </Code></Pre> <br>
        !   The following code snippet shows another way to iterate over the list.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start forward iteration (from the first item)
        !   IsTheEnd = List%StartFirst(CurrItem)
        !   DO WHILE (.NOT.IsTheEnd)
        !       DoSomeThing_With_CurrItem...
        !       ! move to the next iteration
        !       IsTheEnd = List%MoveForward(CurrItem)
        !   END DO
        !   </Code></Pre>
        !  **Important Note**: The specified type of item to be retrieved must match that
        !   of an item stored in the list.
        PROCEDURE   :: MoveForward      => LinkedList_Move2NextElm
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start the *backward* iteration and return a flag indicating
        !                whether the list is empty or not. <br>
        !  **Usage**: see *MoveBackward* procedure.
        PROCEDURE   :: StartLast        => LinkedList_Move2LastElm
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move *backward* to the next iteration and return a flag
        !                indicating whether the cursor pointer has reached the end
        !                of the list or not. <br>
        !  **Usage**: <br>
        !   The following code snippet illustrates how to typically traverse the list in reverse order.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start backward iteration (from the last item)
        !   IsEmpty = List%StartLast()
        !   IF (.NOT.IsEmpty) DoSomeThing...
        !   DO
        !       ! move to the next iteration
        !       IsTheEnd = List%MoveBackward()
        !       ! check whether we reach the end of the list or not
        !       IF (IsTheEnd) EXIT
        !       ! if not, do the task we need
        !       DoSomeThing...
        !   END DO
        !   </Code></Pre> <br>
        !   The following code snippet shows another way to iterate over the list in reverse order.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start backward iteration (from the last item)
        !   IsTheEnd = List%StartLast(CurrItem)
        !   DO WHILE (.NOT.IsTheEnd)
        !       DoSomeThing_With_CurrItem...
        !       ! move to the next iteration
        !       IsTheEnd = List%MoveBackward(CurrItem)
        !   END DO
        !   </Code></Pre>
        !  **Important Note**: The specified type of item to be retrieved must match that
        !   of an item stored in the list.
        PROCEDURE   :: MoveBackward     => LinkedList_Move2PrevElm
        ! ---------------------------------------------------------------------
        ! -----                 Inquiry Procedures                       ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the list is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = List%IsEmpty() <br>
        !   --->    IF (.NOT.List%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty          => LinkedList_IsEmpty
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get size of the list. <br>
        !  **Usage**: <br>
        !   --->    ListSize = List%GetSize()
        PROCEDURE   :: GetSize          => LinkedList_GetSize
        !> **Type-Bound Function**: SameType <br>
        !  **Purpose**:  To check whether all stored items have the same type or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = List%SameType() <br>
        !   --->    IF (.NOT.List%SameType()) DoSomeThing
        PROCEDURE   :: SameType         => LinkedList_SameType
        ! ---------------------------------------------------------------------
        ! -----                 Retrieval Procedures                     ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: PeekFirst <br>
        !  **Purpose**:  To get the first item (without removing it from the list).
        !                Also, return a flag indicating whether the item is available
        !                or not. <br>
        !  **Usage**: <br>
        !   --->    Success = List%PeekFirst(Item) <br>
        !   --->    IF (.NOT.List%PeekFirst(Item)) DoSomething <br>
        !  **Important Note**: The specified type of item to be retrieved must match that
        !   of an item stored in the list.
        PROCEDURE   :: PeekFirst        => LinkedList_PeekFirst
        !> **Type-Bound Function**: PeekLast <br>
        !  **Purpose**:  To get the last item (without removing it from the list).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = List%PeekLast(Item) <br>
        !   --->    IF (.NOT.List%PeekLast(Item)) DoSomething <br>
        !  **Important Note**: The specified type of item to be retrieved must match that
        !   of an item stored in the list.
        PROCEDURE   :: PeekLast        => LinkedList_PeekLast
        !> **Type-Bound Function**: PeekAt <br>
        !  **Purpose**:  To get the item (without removing it from the list) at
        !                the specified index where the index must be between 1 and the
        !                list size.  Also, return a flag indicating whether the
        !                item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = List%PeekAt(Index, Item) <br>
        !   --->    IF (.NOT.List%PeekAt(Index, Item)) DoSomething <br>
        !  **Important Note**: The specified type of item to be retrieved must match that
        !   of an item stored in the list.
        PROCEDURE   :: PeekAt           => LinkedList_PeekAt
        !> **Type-Bound Subroutine**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the list. Also,
        !                return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = List%GetAll(Items) <br>
        !   --->    IF (.NOT.List%GetAll(Items)) DoSomething <br>
        !  **Important Note**: This operation will not be successful if items stored
        !   in different nodes have different types.
        PROCEDURE   :: GetAll           => LinkedList_GetAllItems
        ! ---------------------------------------------------------------------
        ! -----                 Queue Procedures                         ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: EnQueue <br>
        ! **Purpose**:  To add a new item to the end of the queue. <br>
        !  **Usage**: <br>
        !   --->    CALL Queue%EnQueue(NewItem) <br>
        !  **Note**: *EnQueue* is an alias of *AddLast*.
        PROCEDURE   :: EnQueue  => LinkedList_AddLast
        !> **Type-Bound Function**: DeQueue <br>
        !  **Purpose**:  To get and remove the front (first) item of the queue.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Queue%DeQueue(Item) <br>
        !   --->    IF (.NOT.Queue%DeQueue(Item)) DoSomething <br>
        !  **Note**: *DeQueue* is an alias of *RemoveFirst*.
        PROCEDURE   :: DeQueue  => LinkedList_RemoveFirst
        ! ---------------------------------------------------------------------
        ! -----                 Stack Procedures                         ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Push <br>
        ! **Purpose**:  To add a new item to the top of the stack. <br>
        !  **Usage**: <br>
        !   --->    CALL Stack%Push(NewItem) <br>
        !  **Note**: *Push* is an alias of *AddLast*.
        PROCEDURE   :: Push     => LinkedList_AddLast
        !> **Type-Bound Function**: Pop <br>
        !  **Purpose**:  To get and remove the top item of the stack.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Stack%Pop(Item) <br>
        !   --->    IF (.NOT.Stack%Pop(Item)) DoSomething <br>
        !  **Note**: *Pop* is an alias of *RemoveLast*.
        PROCEDURE   :: Pop      => LinkedList_RemoveLast
        !> **Type-Bound Function**: PeekTop <br>
        !  **Purpose**:  To get the last item (without removing it from the container).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%PeekTop(Item) <br>
        !   --->    IF (.NOT.Container%PeekTop(Item)) DoSomething <br>
        !  **Note**: *PeekTop* is an alias of *PeekLast*.
        PROCEDURE   :: PeekTop  => LinkedList_PeekLast
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the container.
        FINAL       :: LinkedList_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE ListObject

!** INTERFACE DEFINITIONS:
    ! abstract interfaces
    ABSTRACT INTERFACE
        ! IterFuncItem is a procedure supplied to a *Traverse* procedure that
        ! can be used to get the list's item.
        FUNCTION IterFuncItem(Item,Done) RESULT(ErrStat)
            IMPORT
            CLASS(Object), INTENT(IN)       :: Item     ! item
            tLogical,          INTENT(INOUT)    :: Done     ! on input, Done is set to .FALSE.
                                                            ! on exit, set it to .TRUE. if user
                                                            !   want to stop the queue traversing.
            tLogical                            :: ErrStat  ! true if error occurred in the user routine
        END FUNCTION IterFuncItem
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! ---------------------------------------------------------------------
! -----             DLLNode PROCEDURES                            -----
! ---------------------------------------------------------------------

SUBROUTINE LinkedNode_Deallocate(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To deallocate DLLNode pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(DLLNode), POINTER, INTENT(INOUT)   :: Node !! DLLNode object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32           :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW

    IF (ASSOCIATED(Node)) THEN
        DEALLOCATE(Node, STAT=AllocStat, ERRMSG=AllocMsg)
        CALL Handle_ErrDealloc('LinkedNode_Deallocate', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE LinkedNode_Deallocate

!******************************************************************************

SUBROUTINE LinkedNode_Destructor(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct DLLNode object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DLLNode), INTENT(INOUT)   :: Node !! DLLNode object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Node%Item%MemFree()
    DEALLOCATE(Node%Item)
    NULLIFY(Node%Next)
    NULLIFY(Node%Prev)

    RETURN

END SUBROUTINE LinkedNode_Destructor

! ---------------------------------------------------------------------
! -----             ListObject PROCEDURES                         -----
! ---------------------------------------------------------------------
! -----             PRIVATE PROCEDURES                            -----
! ---------------------------------------------------------------------

SUBROUTINE LinkedList_SetNewNode(List, NewNode, AppendAtTail)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set pointers for newly created node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject),  INTENT(INOUT)   :: List         !! ListObject object
    TYPE(DLLNode), POINTER, INTENT(IN)      :: NewNode      !! new node to be added to the list
    tLogical,               INTENT(IN)      :: AppendAtTail !! true if append to the tail

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if list is empty or not
    IF (List%IsEmpty()) THEN
        ! the list is EMPTY so the new node is added to the beginning of the list
        List%Head => NewNode
        List%Tail => NewNode
    ELSE
        ! the list is NOT empty so add NewNode according to AppendAtTail flag
        IF (AppendAtTail) THEN
            !++++++++++++++++++++++++++++++++++++++++++++++++++
            !+++ Append the NewNode to the list at the tail +++
            !++++++++++++++++++++++++++++++++++++++++++++++++++
            NewNode%Prev   => List%Tail
            List%Tail%Next => NewNode
            List%Tail      => NewNode
        ELSE
            !++++++++++++++++++++++++++++++++++++++++++++++++++
            !+++ Append the NewNode to the list at the head +++
            !++++++++++++++++++++++++++++++++++++++++++++++++++
            NewNode%Next   => List%Head
            List%Head%Prev => NewNode
            List%Head      => NewNode
        END IF
    END IF

    ! set list length
    List%Size =  List%Size + 1

    RETURN

END SUBROUTINE LinkedList_SetNewNode

!******************************************************************************

FUNCTION LinkedList_RemoveNode(List, CurrNode, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the specified node from the list.  Also, return a flag indicating
    !  whether the node is removed successfully or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject),       INTENT(INOUT)  :: List     !! ListObject object
    TYPE(DLLNode), POINTER,      INTENT(INOUT)  :: CurrNode !! the node to be removed
    CLASS(Object), OPTIONAL, INTENT(OUT)    :: Item     !! item of the removed node if requested
    !> flag indicating whether the node is successfully removed. <br>
    ! - true if the node is successfully removed.
    ! - false if the node is NOT successfully removed.
    tLogical                                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check input data and their possibly-related errors
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSEIF (.NOT.ASSOCIATED(CurrNode)) THEN
        Flag = FalseVal
        RETURN
    ELSE
        ! set flag
        Flag = TrueVal
        ! +++ no error in required input arguments +++
        ! check whether requesting the item of the node to be removed
        IF (PRESENT(Item)) THEN
            ! check whether the types of items are the same or not
            IF (SAME_TYPE_AS(Item, CurrNode%Item)) THEN
                ! copy item
                Item = CurrNode%Item
            ELSE
                ! report error
                CALL Handle_ErrLevel('LinkedList_RemoveNode', ModName, ErrWarning, &
                           'Type of output item is not compatible with those in the list.')
            END IF
        END IF
        ! check whether there is only one node or not
        IF (List%Size == 1) THEN
            ! ++ the list has only one node ++
            ! check to make sure that the supplied node is really the only one
            IF (ASSOCIATED(CurrNode, List%Head).AND.ASSOCIATED(CurrNode, List%Tail)) THEN
                ! reset the list
                List%Head => NULL()
                List%Tail => NULL()
                List%Cursor => NULL()
                List%Size = 0
            ELSE
                ! The list contains only one node but it is NOT associated with
                ! the specified node so no node is removed.
                Flag = FalseVal
                RETURN
            END IF
        ELSE
            ! ++ the list has two or more nodes ++
            ! check where the supplied node is
            IF (ASSOCIATED(CurrNode,List%Head)) THEN
                ! the node is the head
                List%Head      => CurrNode%Next
                List%Head%Prev => NULL()
            ELSEIF (ASSOCIATED(CurrNode,List%Tail)) THEN
                ! the node is the tail
                List%Tail      => CurrNode%Prev
                List%Tail%Next => NULL()
            ELSE
                ! the node is in the middle
                CurrNode%Prev%Next => CurrNode%Next
                CurrNode%Next%Prev => CurrNode%Prev
            END IF
            ! set length
            List%Size = List%Size - 1
        END IF
        ! destroy the current node
        CALL CurrNode%Destruct()
        CALL LinkedNode_Deallocate(CurrNode)
    END IF

    RETURN

END FUNCTION LinkedList_RemoveNode

!******************************************************************************

SUBROUTINE LinkedList_Traverse(List, IterFunc)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To traverse the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject), INTENT(INOUT)    :: List     !! ListObject object
    PROCEDURE(IterFuncItem)                 :: IterFunc !! iterator function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode),     POINTER  :: CurrNode
    CLASS(Object), POINTER  :: CurrItem
    tLogical                    :: ErrStat
    tLogical                    :: Done

! FLOW

    ! set defaults
    Done    = FalseVal   ! traverse to all nodes
    ErrStat = FalseVal

    ! initialize current node
    CurrNode => List%Head

    ! loop over all nodes of the list
    DO WHILE (ASSOCIATED(CurrNode))

        ! get current item
        CurrItem => CurrNode%Item

        ! call iterator function
        ErrStat = IterFunc(CurrItem, Done)

        ! report error if necessary
        IF (ErrStat) THEN
            CALL Handle_ErrLevel('LinkedList_Traverse', ModName, ErrSevere, &
                                 'An error occurred during call to iterator function.')
            RETURN
        END IF

        ! exit the loop if the user want to stop the traversing
        IF (Done) EXIT

        ! set current node
        CurrNode => CurrNode%Next

    END DO

    NULLIFY(CurrNode)
    NULLIFY(CurrItem)

    RETURN

END SUBROUTINE LinkedList_Traverse

!******************************************************************************

FUNCTION IsIndexValid(Size, Index) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To check whether the specified index is between 1
    !  and the list size or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: Size     !! the list size
    tIndex, INTENT(IN)  :: Index    !! the specified index
    tLogical            :: Flag     !! true if the index is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal
    IF (Index < 1_kIndex) RETURN
    IF (Index > Size) RETURN
    Flag = TrueVal

    RETURN

END FUNCTION IsIndexValid

!******************************************************************************

FUNCTION LinkedList_GetNodeAt(List, N) RESULT(NodeOut)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the Nth node from the list where N is between 1 and the list size.
    !  If the list is empty or N is not in a valid range, return Null pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject), INTENT(INOUT)    :: List     !! ListObject object
    tIndex,                INTENT(IN)       :: N        !! (one-based) index indicating the node
    TYPE(DLLNode),         POINTER          :: NodeOut  !! pointer to the Nth node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CurrIndex
    tLogical    :: EndOfList

!** FLOW
    
    ! check and return quickly if possible
    IF (List%IsEmpty()) THEN
        NodeOut => NULL()
        RETURN
    ELSEIF (.NOT.IsIndexValid(List%Size, N)) THEN
        NodeOut => NULL()
        RETURN
    END IF

    ! check which direction to traverse the list
    IF (N <= (List%Size-N+1)) THEN
        ! perform forward iteration
        CurrIndex = 1
        ! restart the iteration
        EndOfList = List%StartFirst()
        DO WHILE (.NOT.EndOfList)
            ! check if the node is found
            IF (CurrIndex == N) EXIT
            ! move to the next iteration
            EndOfList = List%MoveForward()
            ! set index
            CurrIndex = CurrIndex + 1
        END DO
    ELSE
        ! perform backward iteration
        CurrIndex = List%Size
        ! restart the iteration
        EndOfList = List%StartLast()
        DO WHILE (.NOT.EndOfList)
            ! check if the node is found
            IF (CurrIndex == N) EXIT
            ! move to the next iteration
            EndOfList = List%MoveBackward()
            ! set index
            CurrIndex = CurrIndex - 1
        END DO
    END IF
    
    ! set pointer to the Nth node
    NodeOut => List%Cursor
    
    RETURN

END FUNCTION LinkedList_GetNodeAt

! ---------------------------------------------------------------------
! -----             PUBLIC PROCEDURES                             -----
! ---------------------------------------------------------------------
! -----             Constructor and Destructor Procedures         -----
! ---------------------------------------------------------------------

SUBROUTINE LinkedList_CreateByArray(List, N, Items)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a list from an array of item.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject), INTENT(INOUT)    :: List     !! ListObject object
    tIndex,                INTENT(IN)       :: N        !! number of items
    CLASS(Object),     INTENT(IN)       :: Items(N) !! an array of items

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! simply return if N is less than 1
    IF (N <= 0) RETURN

    ! built list of input items
    DO I = 1, N
        CALL List%AddLast(Items(I))
    END DO

    RETURN

END SUBROUTINE LinkedList_CreateByArray

!******************************************************************************

SUBROUTINE LinkedList_Destructor(List)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct ListObject object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject), INTENT(INOUT)    :: List !! ListObject object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! destroy all nodes and free up memory
    CALL List%Clear()

    RETURN

END SUBROUTINE LinkedList_Destructor

! ---------------------------------------------------------------------
! -----             Insertion and Removal Procedures              -----
! ---------------------------------------------------------------------

SUBROUTINE LinkedList_AddFirst(List, Item)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a item at the head of the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject), INTENT(INOUT)    :: List     !! ListObject object
    CLASS(Object),     INTENT(IN)       :: Item     !! item to be added to the list

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLogical, PARAMETER :: AppendAtTail = FalseVal
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: NewNode
    tSInt32               :: AllocStat
    tCharLen(MsgLen)        :: AllocMsg

! FLOW

    ! allocate new node and 
    ALLOCATE(NewNode, STAT=AllocStat, ERRMSG=AllocMsg)
    CALL Handle_ErrAlloc('LinkedList_AddFirst', ModName, AllocMsg, AllocStat)

    ! allocate the new node's item and copy its data
    ALLOCATE(NewNode%Item, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Item)
    CALL Handle_ErrAlloc('LinkedList_AddFirst', ModName, AllocMsg, AllocStat)

    ! set pointers to new node
    CALL List%SetNewNode(NewNode, AppendAtTail)

    ! free up memory
    NULLIFY(NewNode)

    RETURN

END SUBROUTINE LinkedList_AddFirst

!******************************************************************************

SUBROUTINE LinkedList_AddLast(List, Item)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a item at the tail of the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject), INTENT(INOUT)    :: List     !! ListObject object
    CLASS(Object),     INTENT(IN)       :: Item     !! item to be added to the list

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLogical, PARAMETER :: AppendAtTail = TrueVal
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: NewNode
    tSInt32               :: AllocStat
    tCharLen(MsgLen)        :: AllocMsg

! FLOW

    ! allocate new node
    ALLOCATE(NewNode, STAT=AllocStat, ERRMSG=AllocMsg)
    CALL Handle_ErrAlloc('LinkedList_AddLast', ModName, AllocMsg, AllocStat)

    ! allocate the new node's item and copy its data
    ALLOCATE(NewNode%Item, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Item)
    CALL Handle_ErrAlloc('LinkedList_AddLast', ModName, AllocMsg, AllocStat)

    ! set pointers to new node
    CALL List%SetNewNode(NewNode, AppendAtTail)

    ! free up memory
    NULLIFY(NewNode)

    RETURN

END SUBROUTINE LinkedList_AddLast

!******************************************************************************

FUNCTION LinkedList_AddAt(List, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To insert the given item at the specified index where the index must be
    !  between 1 and the list size.  Also, return a flag indicating whether the
    !  item is successfully added.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListObject object
    CLASS(ListObject), INTENT(INOUT)    :: List
    !% the one-based index into the list's items
    tIndex,                INTENT(IN)       :: Index
    !% the item to be added to the list
    CLASS(Object),     INTENT(IN)       :: Item
    !> flag indicating whether the item is successfully added. <br>
    ! - true if the item is successfully added.
    ! - false if the item is NOT successfully added.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: NewNode
    tSInt32               :: AllocStat
    tCharLen(MsgLen)        :: AllocMsg

! FLOW
    
    ! check the validity of the specified index
    Flag = FalseVal
    IF (.NOT.IsIndexValid(List%Size, Index)) RETURN
 
    ! allocate new node and set the new node's item
    ALLOCATE(NewNode, STAT=AllocStat, ERRMSG=AllocMsg)
    CALL Handle_ErrAlloc('LinkedList_AddAt', ModName, AllocMsg, AllocStat)

    ! allocate the new node's item and copy its data
    ALLOCATE(NewNode%Item, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=Item)
    CALL Handle_ErrAlloc('LinkedList_AddAt', ModName, AllocMsg, AllocStat)

    ! set flag
    Flag = TrueVal
    
    IF (Index == 1_kIndex) THEN
        ! add node to the front
        CALL List%SetNewNode(NewNode, AppendAtTail=FalseVal)
    ELSE
        BLOCK
            ! block variables
            TYPE(DLLNode), POINTER  :: NthNode
            TYPE(DLLNode), POINTER  :: PrvNode
            ! get node at the index
            IF (Index == List%Size) THEN
                NthNode => List%Tail
            ELSE
                NthNode => List%GetNodeAt(Index)
            END IF
            ! get previous node
            PrvNode => NthNode%Prev
            ! add new node
            PrvNode%Next => NewNode
            NthNode%Prev => NewNode
            NewNode%Prev => PrvNode
            NewNode%Next => NthNode
            ! free block variables
            NULLIFY(NthNode, PrvNode)
        END BLOCK            
        ! set list length
        List%Size =  List%Size + 1
    END IF
        
    ! free up pointer
    NULLIFY(NewNode)
    
    RETURN

END FUNCTION LinkedList_AddAt

!******************************************************************************

SUBROUTINE LinkedList_Remove(List, First, Item)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove a node from the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject),       INTENT(INOUT)  :: List     !! ListObject object
    tLogical,          OPTIONAL, INTENT(IN)     :: First
    !^ location flag where the node is removed <br>
    ! - true (by default) if want to remove the first node <br>
    ! - false if want to remove the last node
    CLASS(Object), OPTIONAL, INTENT(OUT)    :: Item     !! item of the removed node if requested

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: RemoveFirstNode
    tLogical    :: Success

! FLOW

    ! first, check whether the list is empty or not
    IF (List%IsEmpty()) THEN
        ! set routine name
        SubName = 'LinkedList_Remove'
        ! set error message
        ErrMsg = 'The list is EMPTY.'
        ! report error
        CALL Handle_ErrLevel(SubName, ModName, ErrWarning, ErrMsg)
        RETURN
    END IF

    ! set default and check optional input
    SET_OPTION(RemoveFirstNode, TrueVal, First)

    ! check whether requesting the item of the node to be removed
    IF (PRESENT(Item)) THEN
        IF (RemoveFirstNode) THEN
            Success = List%PeekFirst(Item)
        ELSE
            Success = List%PeekLast(Item)
        END IF
    END IF

    ! check whether there is only one node or not
    IF (ASSOCIATED(List%Head, List%Tail)) THEN
        ! the list has only one node so reset the list
        CALL List%Head%Destruct()
        CALL LinkedNode_Deallocate(List%Head)
        NULLIFY(List%Tail)
        NULLIFY(List%Cursor)
        List%Size = 0
    ELSE
        ! the list has two or more nodes so check which node to be removed
        IF (RemoveFirstNode) THEN
            ! reset the head
            List%Cursor => List%Head
            List%Head => List%Cursor%Next
            List%Head%Prev => NULL()
        ELSE
            ! reset the tail
            List%Cursor => List%Tail
            List%Tail => List%Cursor%Prev
            List%Tail%Next => NULL()
        END IF
        ! remove the node
        CALL List%Cursor%Destruct()
        CALL LinkedNode_Deallocate(List%Cursor)
        ! set length
        List%Size = List%Size - 1
    END IF

    RETURN

END SUBROUTINE LinkedList_Remove

!******************************************************************************

FUNCTION LinkedList_RemoveFirst(List, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get and remove the first item of the list.  Also, return
    !  a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListObject object
    CLASS(ListObject), INTENT(INOUT)    :: List
    !% the item to be removed from the list
    CLASS(Object),     INTENT(OUT)      :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the list is NOT empty.
    ! - false if the list is empty.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! return quickly if the list is empty
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    END IF

    ! get the item of the node to be removed
    Flag = List%PeekFirst(Item)

    ! check whether there is only one node or not
    IF (ASSOCIATED(List%Head, List%Tail)) THEN
        ! the list has only one node so reset the list
        CALL List%Head%Destruct()
        CALL LinkedNode_Deallocate(List%Head)
        NULLIFY(List%Tail)
        NULLIFY(List%Cursor)
        List%Size = 0
    ELSE
        BLOCK
            ! block variable
            TYPE(DLLNode), POINTER  :: DelNode => NULL()
            ! reset the head
            DelNode   => List%Head
            List%Head => DelNode%Next
            List%Head%Prev => NULL()
            ! remove the node
            CALL DelNode%Destruct()
            CALL LinkedNode_Deallocate(DelNode)
            ! free pointer
            NULLIFY(DelNode)
        END BLOCK
        ! set length
        List%Size = List%Size - 1
    END IF

    RETURN

END FUNCTION LinkedList_RemoveFirst

!******************************************************************************

FUNCTION LinkedList_RemoveLast(List, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get and remove the last item of the list.  Also, return
    !  a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListObject object
    CLASS(ListObject), INTENT(INOUT)    :: List
    !% the item to be removed from the list
    CLASS(Object),     INTENT(OUT)      :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the list is NOT empty.
    ! - false if the list is empty.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! return quickly if the list is empty
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    END IF

    ! get the item of the node to be removed
    Flag = List%PeekLast(Item)

    ! check whether there is only one node or not
    IF (ASSOCIATED(List%Head, List%Tail)) THEN
        ! the list has only one node so reset the list
        CALL List%Head%Destruct()
        CALL LinkedNode_Deallocate(List%Head)
        NULLIFY(List%Tail)
        NULLIFY(List%Cursor)
        List%Size = 0
    ELSE
        BLOCK
            ! block variable
            TYPE(DLLNode), POINTER  :: DelNode
            ! reset the tail
            DelNode => List%Tail
            List%Tail => DelNode%Prev
            List%Tail%Next => NULL()
            ! remove the node
            CALL DelNode%Destruct()
            CALL LinkedNode_Deallocate(DelNode)
            ! free pointer
            NULLIFY(DelNode)
        END BLOCK
        ! set length
        List%Size = List%Size - 1
    END IF

    RETURN

END FUNCTION LinkedList_RemoveLast

!******************************************************************************

FUNCTION LinkedList_RemoveAt(List, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the item at the specified index where the index must be
    !  between 1 and the list size.   Also, return a flag indicating whether
    !  the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListObject object
    CLASS(ListObject), INTENT(INOUT)    :: List
    !% the one-based index into the list's items
    tIndex,                INTENT(IN)       :: Index
    !% the item to be removed from the list
    CLASS(Object),     INTENT(OUT)      :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the item is successfully removed.
    ! - false if the item is NOT successfully removed.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: DelNode

! FLOW
    
    ! get node at the index
    DelNode => List%GetNodeAt(Index)

    IF (ASSOCIATED(DelNode)) THEN
        ! get item and remove node
        Flag = List%RemoveNode(DelNode, Item)
    ELSE
        Flag = FalseVal
    END IF

    NULLIFY(DelNode)

    RETURN

END FUNCTION LinkedList_RemoveAt

!******************************************************************************

SUBROUTINE LinkedList_Delete(List)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete an item from the list.  This procedure is intended to be used
    !  in conjunction with the *StartFirst* and *MoveForward* methods (or the
    !  *StartLast* and *MoveBackward*).  Therefore, after the call to one of
    !  these methods and then calling this procedure will result in a removal
    !  of the current item of the iteration (i.e. the same item that can be
    !  retrieved via those methods). <br>
    !  If the cursor pointer is not associated, nothing happens.  This usually
    !  means that the list is empty or this procedure is called before those
    !  iteration methods. <br>
    !  This procedure provides a way to remove items in the middle of the list
    !  without knowing specific locations of the items.  The user would perform
    !  an iteration over the list by calling those iteration methods.  While in
    !  the middle of the iteration, if the interested items are found, they can
    !  be removed from the list by this procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject), INTENT(INOUT)    :: List !! ListObject object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: DelNode
    tLogical                :: Flag

! FLOW

    ! get the node to be deleted
    DelNode => List%Cursor
    
    IF (ASSOCIATED(DelNode)) THEN
        ! reset cursor
        IF (List%Dir == 1) THEN
            ! forward iteration so move cursor backward
            List%Cursor => List%Cursor%Prev
        ELSE
            ! backward iteration so move cursor forward
            List%Cursor => List%Cursor%Next
        END IF
        Flag = List%RemoveNode(DelNode)
    END IF

    NULLIFY(DelNode)

    RETURN

END SUBROUTINE LinkedList_Delete

!**************************************************************************************

SUBROUTINE LinkedList_ClearItems(List)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free up memory of the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject), INTENT(INOUT)    :: List !! ListObject object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! traverse the list and destroy all nodes
    DO WHILE (.NOT.List%IsEmpty())
        CALL List%Remove()
    END DO

    ! free up pointers
    NULLIFY(List%Head)
    NULLIFY(List%Tail)
    NULLIFY(List%Cursor)

    ! reset components
    List%Size = 0
    List%Dir = 0

    RETURN

END SUBROUTINE LinkedList_ClearItems

!******************************************************************************

FUNCTION LinkedList_ToArray(List, Items) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove all items from the list.  Also, return
    !  a flag indicating whether the items are successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListObject object
    CLASS(ListObject),          INTENT(INOUT)   :: List
    !% the item to be removed from the list
    CLASS(Object), ALLOCATABLE, INTENT(INOUT)   :: Items(:)
    !> flag indicating whether the items are successfully removed. <br>
    ! - true if the items are successfully removed.
    ! - false if the items are NOT successfully removed.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (.NOT.List%SameType()) THEN
        Flag = FalseVal
        ! report error
        CALL Handle_ErrLevel('LinkedList_ToArray', ModName, ErrSevere, &
                             'Items does not have the same type.')
        RETURN
    END IF
    
    ! get items
    Flag = List%GetAll(Items)

    ! destroy all nodes and free up memory
    CALL List%Clear()

END FUNCTION LinkedList_ToArray

!******************************************************************************

SUBROUTINE LinkedList_RemoveDuplicates(List)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove nodes with duplicated items from the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject), INTENT(INOUT)    :: List    !! ListObject object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode),     POINTER      :: CurrNode
    TYPE(DLLNode),     POINTER      :: NextNode
    CLASS(Object), POINTER      :: CurrItem
    CLASS(Object), ALLOCATABLE  :: IterItem
    tLogical                        :: Repeat
    tLogical                        :: IsTheEnd
    tLogical                        :: Success
    tSInt32                       :: AllocStat
    tCharLen(MsgLen)                :: AllocMsg

!** FLOW

    ! start the list
    CurrNode => List%Head
    NextNode => CurrNode%Next
    IF (ASSOCIATED(CurrNode)) THEN
        ALLOCATE(IterItem, STAT=AllocStat, ERRMSG=AllocMsg, SOURCE=CurrNode%Item)
        CALL Handle_ErrAlloc('LinkedList_RemoveDuplicates', ModName, AllocMsg, AllocStat)
    END IF

    ! traverse the list
    DO WHILE (ASSOCIATED(NextNode))

        ! set flag
        Repeat = FalseVal

        ! get current item
        CurrItem => CurrNode%Item

        ! start the iteration at the next node
        List%Cursor => NextNode
        IsTheEnd = FalseVal
        IterItem = List%Cursor%Item

        DO WHILE (.NOT.IsTheEnd)

            ! check whether the items are the same
            IF (CurrItem%IsEqualTo(IterItem)) THEN
                Repeat = TrueVal
                EXIT
            END IF

            ! move to next iteration and get its item
            IsTheEnd = List%MoveForward(IterItem)

        END DO

        ! remove node if the current item has a duplicated one
        IF (Repeat) Success = List%RemoveNode(CurrNode)

        ! move to next node
        CurrNode => NextNode

        ! set NextNode
        NextNode => CurrNode%Next

    END DO

    ! free up memory
    NULLIFY(CurrNode)
    NULLIFY(NextNode)
    NULLIFY(CurrItem)
    DEALLOCATE(IterItem)

    RETURN

END SUBROUTINE LinkedList_RemoveDuplicates

! ---------------------------------------------------------------------
! -----                 Iteration Procedures                      -----
! ---------------------------------------------------------------------

FUNCTION LinkedList_Move2FirstElm(List, Item) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the head node of the list and return
    !  a flag indicating whether the list is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject),       INTENT(INOUT)  :: List     !! ListObject object
    CLASS(Object), OPTIONAL, INTENT(OUT)    :: Item     !! first item
    tLogical                                    :: IsEmpty  !! true if the list is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set cursor
    List%Cursor => List%Head

    ! set flag
    IsEmpty = .NOT.ASSOCIATED(List%Cursor)
    
    ! set direction
    IF (.NOT.IsEmpty) List%Dir = 1

    IF (PRESENT(Item).AND.(.NOT.IsEmpty)) THEN
        ! check whether the types of items are the same or not
        IF (SAME_TYPE_AS(Item, List%Cursor%Item)) THEN
            ! copy item
            Item = List%Cursor%Item
        ELSE
            ! report error
            CALL Handle_ErrLevel('LinkedList_Move2FirstElm', ModName, ErrWarning, &
                       'Type of output item is not compatible with those in the list.')
        END IF 
    END IF

    RETURN

END FUNCTION LinkedList_Move2FirstElm

!******************************************************************************

FUNCTION LinkedList_Move2LastElm(List, Item) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the tail node of the list and return
    !  a flag indicating whether the list is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject),       INTENT(INOUT)  :: List     !! ListObject object
    CLASS(Object), OPTIONAL, INTENT(OUT)    :: Item     !! last item
    tLogical                                    :: IsEmpty  !! true if the list is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set cursor
    List%Cursor => List%Tail

    ! set flag
    IsEmpty = .NOT.ASSOCIATED(List%Cursor)
    
    ! set direction
    IF (.NOT.IsEmpty) List%Dir = -1

    IF (PRESENT(Item).AND.(.NOT.IsEmpty)) THEN
        ! check whether the types of items are the same or not
        IF (SAME_TYPE_AS(Item, List%Cursor%Item)) THEN
            ! copy item
            Item = List%Cursor%Item
        ELSE
            ! report error
            CALL Handle_ErrLevel('LinkedList_Move2LastElm', ModName, ErrWarning, &
                       'Type of output item is not compatible with those in the list.')
        END IF 
    END IF

    RETURN

END FUNCTION LinkedList_Move2LastElm

!******************************************************************************

FUNCTION LinkedList_Move2NextElm(List, Item) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move (forward) to the next node in the list and return a flag indicating
    !  whether the cursor pointer has reached the end of the list or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject),       INTENT(INOUT)  :: List     !! ListObject object
    CLASS(Object), OPTIONAL, INTENT(OUT)    :: Item     !! item of the next node
    tLogical                                    :: IsTheEnd !! true if the cursor has reached the end of the list
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set cursor
    List%Cursor => List%Cursor%Next

    ! set flag
    IsTheEnd = .NOT.ASSOCIATED(List%Cursor)

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        List%Dir = 1
    ELSE
        List%Dir = 0
    END IF

    IF (PRESENT(Item).AND.(.NOT.IsTheEnd)) THEN
        ! check whether the types of items are the same or not
        IF (SAME_TYPE_AS(Item, List%Cursor%Item)) THEN
            ! copy item
            Item = List%Cursor%Item
        ELSE
            ! report error
            CALL Handle_ErrLevel('LinkedList_Move2NextElm', ModName, ErrWarning, &
                       'Type of output item is not compatible with those in the list.')
        END IF 
    END IF

    RETURN

END FUNCTION LinkedList_Move2NextElm

!******************************************************************************

FUNCTION LinkedList_Move2PrevElm(List, Item) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move backward to the next node (i.e. to the so-called previous node)
    !  in the list and return a flag indicating whether the cursor pointer has
    !  reached the end of the list or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject),       INTENT(INOUT)  :: List     !! ListObject object
    CLASS(Object), OPTIONAL, INTENT(OUT)    :: Item     !! item of the previous node
    tLogical                                    :: IsTheEnd !! true if the cursor has reached the end of the list
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set cursor
    List%Cursor => List%Cursor%Prev

    ! set flag
    IsTheEnd = .NOT.ASSOCIATED(List%Cursor)

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        List%Dir = -1
    ELSE
        List%Dir = 0
    END IF

    IF (PRESENT(Item).AND.(.NOT.IsTheEnd)) THEN
        ! check whether the types of items are the same or not
        IF (SAME_TYPE_AS(Item, List%Cursor%Item)) THEN
            ! copy item
            Item = List%Cursor%Item
        ELSE
            ! report error
            CALL Handle_ErrLevel('LinkedList_Move2PrevElm', ModName, ErrWarning, &
                       'Type of output item is not compatible with those in the list.')
        END IF 
    END IF

    RETURN

END FUNCTION LinkedList_Move2PrevElm

! ---------------------------------------------------------------------
! -----                 Inquiry Procedures                       ------
! ---------------------------------------------------------------------

FUNCTION LinkedList_IsEmpty(List) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether the list is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject), INTENT(IN)   :: List !! ListObject object
    tLogical                            :: Flag !! true if the list is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = .NOT.ASSOCIATED(List%Head)

    RETURN

END FUNCTION LinkedList_IsEmpty

!******************************************************************************

FUNCTION LinkedList_GetSize(List) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get size of the list (a number of nodes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject), INTENT(IN)   :: List     !! ListObject object
    tIndex                              :: Size     !! list size (number of nodes)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = List%Size

    RETURN

END FUNCTION LinkedList_GetSize

!******************************************************************************

FUNCTION LinkedList_SameType(List) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether all stored items have same type or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject), INTENT(IN)   :: List !! ListObject object
    tLogical                            :: Flag !! true if all items have same type

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: CurrNode

! FLOW

    Flag = TrueVal
    
    CurrNode => List%Head%Next
    DO WHILE (ASSOCIATED(CurrNode))
        IF (SAME_TYPE_AS(List%Head%Item, CurrNode%Item)) THEN
            CurrNode => CurrNode%Next
        ELSE
            Flag = FalseVal
            EXIT
        END IF
    END DO
    
    NULLIFY(CurrNode)

    RETURN

END FUNCTION LinkedList_SameType

! ---------------------------------------------------------------------
! -----                 Retrieval Procedures                     ------
! ---------------------------------------------------------------------

FUNCTION LinkedList_PeekFirst(List, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the item stored at the first node without removing it from the list.
    !  Also, return a flag indicating whether the item is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListObject object
    CLASS(ListObject), INTENT(IN)   :: List
    !% the item to be retrieved from the list
    CLASS(Object),     INTENT(OUT)  :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the list is NOT empty.
    ! - false if the list is empty.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if the list is empty or not
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    END IF

    ! check whether the types of items are the same or not
    IF (SAME_TYPE_AS(Item, List%Head%Item)) THEN
        Flag = TrueVal
        ! copy item
        Item = List%Head%Item
    ELSE
        Flag = FalseVal
        ! report error
        CALL Handle_ErrLevel('LinkedList_PeekFirst', ModName, ErrWarning, &
                   'Type of output item is not compatible with those in the list.')
    END IF 

    RETURN

END FUNCTION LinkedList_PeekFirst

!******************************************************************************

FUNCTION LinkedList_PeekLast(List, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the item stored at the last node without removing it from the list.
    !  Also, return a flag indicating whether the item is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListObject object
    CLASS(ListObject), INTENT(IN)   :: List
    !% the item to be retrieved from the list
    CLASS(Object),     INTENT(OUT)  :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the list is NOT empty.
    ! - false if the list is empty.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if the list is empty or not
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    END IF

    ! check whether the types of items are the same or not
    IF (SAME_TYPE_AS(Item, List%Tail%Item)) THEN
        Flag = TrueVal
        ! copy item
        Item = List%Tail%Item
    ELSE
        Flag = FalseVal
        ! report error
        CALL Handle_ErrLevel('LinkedList_PeekLast', ModName, ErrWarning, &
                   'Type of output item is not compatible with those in the list.')
    END IF 

    RETURN

END FUNCTION LinkedList_PeekLast

!******************************************************************************

FUNCTION LinkedList_PeekAt(List, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get the item (without removing it from the list) at the specified index
    !  where the index must be between 1 and the list size.  Also, return
    !  a flag indicating whether the item is available or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListObject object
    CLASS(ListObject), INTENT(INOUT)    :: List
    !% the one-based index into the list's items
    tIndex,                INTENT(IN)       :: Index
    !% the item to be retrieved from the list
    CLASS(Object),     INTENT(OUT)      :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the item is available.
    ! - false if the item is NOT available.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: CurrNode

! FLOW

    ! get the specified node
    CurrNode => List%GetNodeAt(Index)

    IF (ASSOCIATED(CurrNode)) THEN
        ! check whether the types of items are the same or not
        IF (SAME_TYPE_AS(Item, CurrNode%Item)) THEN
            Flag = TrueVal
            ! copy item
            Item = CurrNode%Item
        ELSE
            Flag = FalseVal
            ! report error
            CALL Handle_ErrLevel('LinkedList_PeekAt', ModName, ErrWarning, &
                       'Type of output item is not compatible with those in the list.')
        END IF 
    ELSE
        ! set output flag
        Flag = FalseVal
    END IF
    
    NULLIFY(CurrNode)

    RETURN

END FUNCTION LinkedList_PeekAt

!**************************************************************************************

FUNCTION LinkedList_GetAllItems(List, Items) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get all items (without removing them) from the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListObject),          INTENT(INOUT)   :: List     !! ListObject object
    CLASS(Object), ALLOCATABLE, INTENT(OUT)     :: Items(:) !! an allocatable array of items
    !> flag indicating whether the items are successfully retrieved. <br>
    ! - true if the items are available.
    ! - false if the items are NOT available.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, N

! FLOW

    ! check if the list is empty or not
    IF (List%IsEmpty()) THEN
        ! set output
        Flag = FalseVal
        ! report error
        CALL Handle_ErrLevel('LinkedList_GetAllItems', ModName, ErrWarning, 'The list is EMPTY.')
        RETURN
    END IF

    ! get number of items
    N = List%Size

    ! allocate storage for output
    IF (List%SameType()) THEN
        ALLOCATE(Items(N), MOLD=List%Head%Item)
    ELSE
        ! set output
        Flag = FalseVal
        ! report error
        CALL Handle_ErrLevel('LinkedList_GetAllItems', ModName, ErrSevere, &
                             'Items does not have the same type.')
        RETURN
    END IF

    ! get items
    I = 0
    CALL List%Traverse(ItemIterator)

    ! set flag
    Flag = TrueVal

    RETURN

CONTAINS

    FUNCTION ItemIterator(Item,Done) RESULT(ErrStat)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(Object), INTENT(IN)       :: Item     ! item
        tLogical,          INTENT(INOUT)    :: Done     ! on input, Done is set to FalseVal
                                                        ! on exit, set it to TrueVal if user
                                                        !   want to stop the stack traversing.
        tLogical                            :: ErrStat  ! true if error occurred in the user routine

    !** FLOW:

        ! set flag
        ErrStat = FalseVal
        Done = FalseVal
        ! get item
        I = I + 1
        Items(I) = Item
        RETURN
    END FUNCTION ItemIterator

    !**************************************************************************

END FUNCTION LinkedList_GetAllItems

! ---------------------------------------------------------------------
! -----             Final Procedure                               -----
! ---------------------------------------------------------------------

SUBROUTINE LinkedList_Finalizer(List)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ListObject), INTENT(INOUT) :: List  !! ListObject object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! destroy all nodes and free up memory
    CALL List%Clear()

    RETURN

END SUBROUTINE LinkedList_Finalizer

!******************************************************************************

END MODULE MClass_ListObject

!******************************************************************************
