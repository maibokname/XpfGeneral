
MODULE MClass_LinkedLists

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains *linked-list-based* collection types and their related routines.
!   A *linked-list-based* type is a collection type that employs a linked list implementation. <br>
!   Available collection types based on the linked list concept include:  <br>
!   - the *StackLinked* type that represents a last-in-first-out (LIFO) stack,  <br>
!   - the *QueueLinked* type that represents a first-in-first-out (FIFO) queue,  <br>
!   - the *DequeLinked* type that represents a double-ended queue (deque).  <br>
!   - the *ListLinked* type that represents a list where an item can be added, removed or
!     retrieved at the (valid) specified index.  <br>
!   <br>
!   **Usage Notes**:  <br>
!   - Unlike the *dynamic-array-based* types, the *linked-list-based* types commonly do not
!     require an explicit construction.  Items can be added via the *Construction* method
!     or an insertion method.  Therefore, the *CreateEmpty* method used to construct an empty
!     collection is deemed unnecessary and thus NOT provided.  <br>
!   - Other than that, all operations provided are the same for both groups of collections.  <br>
!   <br>
!   **Important Notes**:  <br>
!   (1) By design, similar to the *dynamic-array-based* types, although all *linked-list-based*
!       types are generic containers that can be used to store items of any data types, they
!       must be employed to store items of only one particular data type.  To store items of
!       another data type, they must be cleared and/or destructed before inserting items of
!       different data type. <br>
!   (2) To be able to successfully retrieve stored items, the specified output item must have
!       the same (concrete) type as that of stored items. <br>
!   (3) Additionally, if type of the stored items is a derived one that is NOT in the *Object*
!       class and this type has allocatable and/or pointer components, a user must specify the
!       optional *ItemCopy* argument when trying to retrieve the stored items.  The *ItemCopy*
!       argument is a user-defined procedure to copy an unlimited polymorphic entity.  It must
!       must provide a type-guard statement to check the compatibility of concrete types of its
!       arguments.  It must also handle the storage allocation (and/or the assignment) of those
!       allocatable (and/or pointer) components as appropriate. <br>
!   <br>
!   **Implementation Notes**:  <br>
!   - Unlike conventional implementation, all *linked-list-based* types provided in this module
!     employ the <a href="../../xpfbase/module/mclass_intrusivelinkedlists.html#type-intrusivelinearlist">
!     IntrusiveLinearList</a> type, which is an intrusive doubly-linked list container type that
!     performs common linked-list operations without a memory management task.  <br>
!   - As a result, the *linked-list-based* types mostly handle the memory management task while
!     common operations of a linked list are relegated to the *IntrusiveLinearList* type, which
!     is declared as a private component of the *linked-list-based* collection types.  <br>
!   - The implementation here in this module (and its submodules) is intended to illustrate the
!     usage of an intrusive container type. <br>

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MemHandlers
    USE MBase_ErrHandlers
    USE MBase_ByteUtil,                 ONLY: AnyType_GetByteSize
    USE MBase_SIntUtil,                 ONLY: ToChar => ToDecStrSigned
#ifdef Indx32Bits
    USE MBase_SimpleHash32,             ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64,             ONLY: ComputeHash => Hash64_FNV1a
#endif
    USE MClass_IntrusiveLinkedLists,    ONLY: DoublyLinkedNode, IntrusiveLinearList
    USE MClass_Object,                  ONLY: Object
    USE MClass_BaseNodePool
    USE MClass_MemoryPool
    USE MClass_GenData
    USE MClass_BaseCollection
    USE MClass_BaseIterable
    USE MClass_CharBuffer

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: DequeLinked
    PUBLIC :: ListLinked
    PUBLIC :: QueueLinked
    PUBLIC :: StackLinked

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_LinkedLists'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    !> *LinkedNode* is a linked node type that uses the *GenData* type to store its item.
    !  The *LinkedNode* type is a subtype of the *DoublyLinkedNode* type, which is intended
    !  to be used with a collection type that utilizes the *IntrusiveLinearList* type. <br>
    TYPE, EXTENDS(DoublyLinkedNode) :: LinkedNode
        TYPE(GenData)   :: Store    !! storage of item (or value)
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----     Deferred/Overridden Procedures from Object Type       -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => LinkedNode_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => LinkedNode_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => LinkedNode_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => LinkedNode_ToString
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => LinkedNode_HashCode
        ! ---------------------------------------------------------------------
    END TYPE LinkedNode
    !> **Description**: <br>
    !   The *QueueLinked* type is a collection type that employs a linked-list implementation
    !   to provide common operations for a FIFO queue. <br>
    !  **Usage Overview**: <br>
    !   The *QueueLinked* type is a *queue* collection type that provides common operations of
    !   a FIFO queue.  Their operations can be categorized as follows: <br>
    !   (1) Construction and destruction.  Methods for these operations include <br>
    !   (1.1) *Construct* method - method to construct the collection either from an array of
    !          items or from another collection, <br>
    !   (1.2) *Destruct* method - method to destruct the collection. <br>
    !   (2) Insert and remove.  Methods for these operations include <br>
    !   (2.1) *Enqueue* method - method to insert an item to the end of the collection, <br>
    !   (2.2) *Dequeue* method - method to get and remove the first item of the collection, <br>
    !   (2.3) *Clear* method - method to remove all items from the collection. <br>
    !   (2.4) *ToArray* method - method to remove and retrieve all items from the collection. <br>
    !   (3) Inquiry.  Methods for these operations include <br>
    !   (3.1) *PeekFirst* method - method to retrieve the first item of the collection, <br>
    !   (3.2) *GetSize* method - method to get the collection size (number of items stored), <br>
    !   (3.3) *IsEmpty* method - method to check whether the collection is empty or not. <br>
    !   (3.4) *GetAll* method - method to retrieve all items from the collection. <br>
    !   (4) Iteration.  Methods for these operations include <br>
    !   (4.1) *StartFirst* method - method to start a forward iteration over items, <br>
    !   (4.2) *MoveForward* method - method to move forward to the next item. <br>
    TYPE, EXTENDS(BaseIterable) :: QueueLinked
        PRIVATE
        !> a working doubly-linked list
        TYPE(IntrusiveLinearList)   :: WrkLst
        !> direction of the iteration <br>
        !  - positive -> forward iteration
        !  - negative -> backward iteration (only applicable for deque and list)
        !  - zero     -> iteration not yet start
        tSInt32                     :: Dir = 0
        !> memory pool of linked nodes 
        TYPE(BaseNodePool)          :: NodePool
        !> memory pool of stored items
        TYPE(MemoryPool)            :: ItemPool
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: QueueLinked_CreateByArray
        !> *ToString* is a deferred procedure to get the string representation of this object.
        PROCEDURE, PRIVATE  :: BaseString   => QueueLinked_BaseString
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseCollection Type      -----
        ! ---------------------------------------------------------------------
        !> Use the *Construct* method in place of the *CopyCollection* method to
        !  create a collection from another collection.
        PROCEDURE   :: CopyCollection   => QueueLinked_CopyCollection
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory of items stored
        !                in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        PROCEDURE   :: Destruct         => QueueLinked_Destroy
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize          => QueueLinked_GetSize
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseIterable Type        -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag indicating whether
        !                the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartFirst() <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstItem)
        PROCEDURE   :: StartFirst   => QueueLinked_Move2FirstElm
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next iteration and return a flag indicating whether the
        !                cursor pointer has reached the end of the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextItem) <br>
        !  **Note**: See the <a href="../module/mclass_baseiterable.html">BaseIterable</a> type
        !            for illustrations of an iteration over a collection using the *StartFirst*
        !            and *MoveForward* methods.<br>
        PROCEDURE   :: MoveForward  => QueueLinked_Move2NextElm
        !> Use the *Enqueue* method in place of the *Insert* method to add an item to the queue.
        PROCEDURE   :: Insert       => QueueLinked_AddElm
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete an item from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the *StartFirst* and
        !       *MoveForward* methods.  Therefore, after the call to one of those methods and then
        !       calling this one will result in a removal of the current item of the iteration
        !       (i.e. the same item that can be retrieved via the *StartFirst* and *MoveForward*
        !       methods). <br>
        PROCEDURE   :: Delete       => QueueLinked_DelElm
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the collection.  Also, return a flag
        !                indicating whether the items are successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Items) <br>
        !   --->    IF (.NOT.Collection%ToArray(Items)) DoSomething
        PROCEDURE   :: ToArray      => QueueLinked_ToArray
        !> **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the collection. Also,
        !                return a flag indicating whether the items are available. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Items) <br>
        !   --->    IF (.NOT.Collection%GetAll(Items)) DoSomething
        PROCEDURE   :: GetAll       => QueueLinked_GetAll
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by QueueLinked Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new collection from an array of items or from another
        !                collection. <br>
        !  **Usage**: <br>
        !   ! create a collection from an array of 25 items <br>
        !   --->    CALL Collection%Construct(25, Arr) <br>
        !   ! create a collection from another collection <br>
        !   --->    CALL Collection%Construct(OtherCollection)
        GENERIC     :: Construct    => QueueLinked_CreateByArray
        !> **Type-Bound Subroutine**: Enqueue <br>
        !  **Purpose**:  To insert the specified item at the end of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Enqueue(Item)
        GENERIC     :: Enqueue      => Insert
        !> **Type-Bound Function**: Dequeue <br>
        !  **Purpose**:  To get and remove the front (first) item of the collection. Also, return
        !                a flag indicating whether the item is successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%Dequeue(Item) <br>
        !   --->    IF (.NOT.Collection%Dequeue(Item)) DoSomething
        PROCEDURE   :: Dequeue      => QueueLinked_Dequeue
        !> **Type-Bound Function**: PeekFirst <br>
        !  **Purpose**:  To get the front (first) item (without removing it from the collection).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekFirst(Item) <br>
        !   --->    IF (.NOT.Collection%PeekFirst(Item)) DoSomething
        PROCEDURE   :: PeekFirst    => QueueLinked_PeekFirst
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => QueueLinked_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => QueueLinked_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => QueueLinked_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => QueueLinked_ToString
        ! ---------------------------------------------------------------------
        ! -----                     Overridden Procedure                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => QueueLinked_HashCode
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: QueueLinked_Finalize
        ! ---------------------------------------------------------------------
    END TYPE QueueLinked
    !> **Description**: <br>
    !   The *StackLinked* type is a collection type that employs a linked-list implementation to
    !   provide common operations for a LIFO stack. <br>
    !  **Usage Overview**: <br>
    !   The *StackLinked* type is a *stack* collection type that provides common operations of
    !   a LIFO stack.  Their operations can be categorized as follows: <br>
    !   (1) Construction and destruction.  Methods for these operations include <br>
    !   (1.1) *Construct* method - method to construct the collection either from an array of
    !          items another or from collection, <br>
    !   (1.2) *Destruct* method - method to destruct the collection. <br>
    !   (2) Insert and remove.  Methods for these operations include <br>
    !   (2.1) *Push* method - method to insert an item at the top of the collection, <br>
    !   (2.2) *Pop* method - method to get and remove the top (last) item of the collection, <br>
    !   (2.3) *Clear* method - method to remove all items from the collection. <br>
    !   (2.4) *ToArray* method - method to remove and retrieve all items from the collection. <br>
    !   (3) Inquiry.  Methods for these operations include <br>
    !   (3.1) *PeekTop* method - method to retrieve the top (last) item of the collection, <br>
    !   (3.2) *GetSize* method - method to get the collection size (number of items stored), <br>
    !   (3.3) *IsEmpty* method - method to check whether the collection is empty or not. <br>
    !   (3.4) *GetAll* method - method to retrieve all items from the collection. <br>
    !   (4) Iteration.  Methods for these operations include <br>
    !   (4.1) *StartFirst* method - method to start a forward iteration over items, <br>
    !   (4.2) *MoveForward* method - method to move forward to the next item. <br>
    !  **Note**: Since the *StackLinked* type is a subtype of the *QueueLinked* type, it can also
    !            be used as a FIFO queue.
    TYPE, EXTENDS(QueueLinked) :: StackLinked
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by StackLinked Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Push <br>
        !  **Purpose**:  To insert the specified item at the end (top) of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Push(Item)
        GENERIC     :: Push         => Insert
        !> **Type-Bound Function**: Pop <br>
        !  **Purpose**:  To get and remove the last (top) item of the collection. Also, return
        !                a flag indicating whether the item is successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%Pop(Item) <br>
        !   --->    IF (.NOT.Collection%Pop(Item)) DoSomething
        PROCEDURE   :: Pop          => StackLinked_Pop
        !> **Type-Bound Function**: PeekTop <br>
        !  **Purpose**:  To get the last (top) item (without removing it from the collection).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekTop(Item) <br>
        !   --->    IF (.NOT.Collection%PeekTop(Item)) DoSomething
        PROCEDURE   :: PeekTop      => StackLinked_PeekTop
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => StackLinked_ToString
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: StackLinked_Finalize
        ! ---------------------------------------------------------------------
    END TYPE StackLinked
    !> **Description**: <br>
    !   The *DequeLinked* type is a collection type that employs a linked-list implementation to
    !   provide common operations for a double-ended queue (deque).  It can be used as a FIFO
    !   queue or a LIFO stack as well. <br>
    !  **Usage Overview**: <br>
    !   The *DequeLinked* type is a *deque* collection type that provides common operations of
    !   a double-ended queue (deque).  Their operations can be categorized as follows: <br>
    !   (1) Construction and destruction.  Methods for these operations include <br>
    !   (1.1) *Construct* method - method to construct the collection either from an array of
    !          items or from another collection, <br>
    !   (1.2) *Destruct* method - method to destruct the collection. <br>
    !   (2) Insert and remove.  Methods for these operations include <br>
    !   (2.1) *AddFirst* method - method to insert an item to the front of the collection, <br>
    !   (2.2) *AddLast* method - method to insert an item to the end of the collection, <br>
    !   (2.3) *RemoveFirst* method - method to get and remove the first item of the collection, <br>
    !   (2.4) *RemoveLast* method - method to get and remove the last item of the collection, <br>
    !   (2.5) *Clear* method - method to remove all items from the collection. <br>
    !   (2.6) *ToArray* method - method to remove and retrieve all items from the collection. <br>
    !   (3) Inquiry.  Methods for these operations include <br>
    !   (3.1) *PeekFirst* method - method to retrieve the first item of the collection, <br>
    !   (3.2) *PeekLast* method - method to retrieve the last item of the collection, <br>
    !   (3.3) *GetSize* method - method to get the collection size (number of items stored), <br>
    !   (3.4) *IsEmpty* method - method to check whether the collection is empty or not. <br>
    !   (3.5) *GetAll* method - method to retrieve all items from the collection. <br>
    !   (4) Iteration.  Methods for these operations include <br>
    !   (4.1) *StartFirst* method - method to start a forward iteration over items, <br>
    !   (4.2) *MoveForward* method - method to move forward to the next item, <br>
    !   (4.3) *StartLast* method - method to start a backward iteration over items, <br>
    !   (4.4) *MoveBackward* method - method to move backward to the previous item. <br>
    !  **Note**: The *DequeArray* type also supports the usual *Enqueue*, *Dequeue* and *PeekFirst*
    !       operations of a FIFO queue as well as the usual *Push*, *Pop* and *PeekTop* operations
    !       of a LIFO stack.  Therefore, when using as a queue or a stack, these operations can be
    !       used in place of *insert*, *remove*, *peek* operations of a deque as desired. <br>
    TYPE, EXTENDS(QueueLinked) :: DequeLinked
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by DequeLinked Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start the *backward* iteration and return a flag indicating whether
        !                the collection is empty or not. <br>
        !  **Usage**:  See the *MoveBackward* method.
        PROCEDURE   :: StartLast    => DequeLinked_Move2LastElm
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move to the next iteration (in reverse order) and return a flag
        !                indicating whether the cursor pointer has reached the end of the
        !                collection or not. <br>
        !  **Usage**: <br>
        !   The following code snippet illustrates how to typically traverse across the collection
        !   in reverse order.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start backward iteration (from the last item)
        !   IsEmpty = Collection%StartLast()
        !   IF (.NOT.IsEmpty) DoSomeThing...
        !   DO
        !       ! move to the next iteration
        !       IsTheEnd = Collection%MoveBackward()
        !       ! check whether we reach the end of the collection or not
        !       IF (IsTheEnd) EXIT
        !       ! if not, do the task we need
        !       DoSomeThing...
        !   END DO
        !   </Code></Pre> <br>
        !   The following code snippet shows another way to iterate over the collection in reverse
        !   order.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start backward iteration (from the last item)
        !   IsTheEnd = Collection%StartLast(CurrItem)
        !   DO WHILE (.NOT.IsTheEnd)
        !       DoSomeThing_With_CurrItem...
        !       ! move to the next iteration
        !       IsTheEnd = Collection%MoveBackward(CurrItem)
        !   END DO
        !   </Code></Pre>
        PROCEDURE   :: MoveBackward => DequeLinked_Move2PrevElm
        !> **Type-Bound Subroutine**: AddFirst <br>
        !  **Purpose**:  To insert the specified item at the front of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%AddFirst(Item)
        PROCEDURE   :: AddFirst     => DequeLinked_AddFirst
        !> **Type-Bound Subroutine**: AddLast <br>
        !  **Purpose**:  To insert the specified item at the end of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%AddLast(Item)
        GENERIC     :: AddLast      => Insert
        !> **Type-Bound Function**: RemoveFirst <br>
        !  **Purpose**:  To get and remove the front (first) item of the collection.  Also, return
        !                a flag indicating whether the item is successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%RemoveFirst(Item) <br>
        !   --->    IF (.NOT.Collection%RemoveFirst(Item)) DoSomething
        GENERIC     :: RemoveFirst  => Dequeue
        !> **Type-Bound Function**: RemoveLast <br>
        !  **Purpose**:  To get and remove the last item of the collection.  Also, return a flag
        !                indicating whether the item is successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%RemoveLast(Item) <br>
        !   --->    IF (.NOT.Collection%RemoveLast(Item)) DoSomething
        PROCEDURE   :: RemoveLast   => DequeLinked_RemoveLast
        !> **Type-Bound Function**: PeekLast <br>
        !  **Purpose**:  To get the last item (without removing it from the collection).  Also,
        !                return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekLast(Item) <br>
        !   --->    IF (.NOT.Collection%PeekLast(Item)) DoSomething
        PROCEDURE   :: PeekLast     => DequeLinked_PeekLast
        ! ---------------------------------------------------------------------
        ! -----             Redefined Stack Operations                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Push <br>
        !  **Purpose**:  To insert the specified item at the end (top) of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Push(Item)
        GENERIC     :: Push         => Insert
        !> **Type-Bound Function**: Pop <br>
        !  **Purpose**:  To get and remove the last (top) item of the collection.  Also, return
        !                a flag indicating whether the item is successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%Pop(Item) <br>
        !   --->    IF (.NOT.Collection%Pop(Item)) DoSomething
        GENERIC     :: Pop          => RemoveLast
        !> **Type-Bound Function**: PeekTop <br>
        !  **Purpose**:  To get the last (top) item (without removing it from the collection).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekTop(Item) <br>
        !   --->    IF (.NOT.Collection%PeekTop(Item)) DoSomething
        GENERIC     :: PeekTop      => PeekLast
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => DequeLinked_ToString
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: DequeLinked_Finalize
        ! ---------------------------------------------------------------------
    END TYPE DequeLinked
    !> **Description**: <br>
    !   The *ListLinked* type is a collection type that employs a linked-list implementation to
    !   provide common operations for a list. <br>
    !  **Usage Overview**: <br>
    !   The *ListLinked* type provides insert, remove and peek operations at a specified index
    !   where the index must be between 1 and the collection size.  The *ListLinked* type is a
    !   subtype of the *DequeLinked* type; therefore, all operations available for the
    !   *DequeLinked* type are also available for the *ListLinked* type.  Furthermore, it thus
    !   can be used as a deque, a FIFO queue or a LIFO stack.  <br>
    TYPE, EXTENDS(DequeLinked) :: ListLinked
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by ListLinked Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: AddAt <br>
        !  **Purpose**:  To insert the specified item at the specified index where the index must
        !                be between 1 and the collection size.  Also, return a flag indicating
        !                whether the item is successfully added. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%AddAt(Index, Item) <br>
        !   --->    IF (.NOT.Collection%AddAt(Index, Item)) DoSomething
        PROCEDURE   :: AddAt      => ListLinked_AddAt
        !> **Type-Bound Function**: RemoveAt <br>
        !  **Purpose**:  To get and remove the item at the specified index where the index must be
        !                between 1 and the collection size.  Also, return a flag indicating whether
        !                the item is successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%RemoveAt(Index, Item) <br>
        !   --->    IF (.NOT.Collection%RemoveAt(Index, Item)) DoSomething
        PROCEDURE   :: RemoveAt   => ListLinked_RemoveAt
        !> **Type-Bound Function**: PeekAt <br>
        !  **Purpose**:  To get the item (without removing it from the collection) at the specified
        !                index where the index must be between 1 and the collection size.  Also,
        !                return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekAt(Index, Item) <br>
        !   --->    IF (.NOT.Collection%PeekAt(Index, Item)) DoSomething
        PROCEDURE   :: PeekAt     => ListLinked_PeekAt
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => ListLinked_ToString
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: ListLinked_Finalize
        ! ---------------------------------------------------------------------
    END TYPE ListLinked

!** INTERFACE DEFINITIONS:
    ! interfaces for QueueLinked
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_Copy(SrcObj, DstObj, IsDeep)
            !^ To copy the source object to the destination object. <br>
            CLASS(QueueLinked), INTENT(IN)  :: SrcObj   !! source object
            CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
            tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
            !^ Flag indicating whether to perform deep copy or shallow copy. <br>
            !  - If present and true, perform a deep copy. <br>
            !  - If present and false, perform a shallow copy. <br>
            !  - If not present, perform either a shallow or a deep copy that is naturally most
            !    suitable for the object's components.
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)
            !^ To check whether LhsObj and RhsObj are equal or not. <br>
            CLASS(QueueLinked), INTENT(IN)  :: LhsObj   !! an object
            CLASS(Object),      INTENT(IN)  :: RhsObj   !! another object
            tLogical                        :: Flag     !! true if both objects are equal
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_MemFree(Obj)
            !^ To free memory of the object. <br>
            CLASS(QueueLinked), INTENT(INOUT)   :: Obj
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_ToString(Obj) RESULT(Str)
            !^ To get the string representation of the object. <br>
            CLASS(QueueLinked), INTENT(IN)  :: Obj
            tCharAlloc                      :: Str
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_HashCode(Obj) RESULT(Code)
            !^ To compute hash code for this object.
            CLASS(QueueLinked), INTENT(IN)  :: Obj
            tIndex                          :: Code
        END FUNCTION QueueLinked_HashCode
        ! ---------------------------------------------------------------------
        ! -----      Deferred Procedures from BaseCollection Type         -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_CopyCollection(This, Other, ItemCopy, ValCopy)
	        !^ To creates a new collection (This) from the given collection (Other). <br>
            CLASS(QueueLinked),    INTENT(INOUT)    :: This     !! collection object to be created
            CLASS(BaseCollection), INTENT(INOUT)    :: Other    !! collection object to be copied
            !> a helper procedure to copy stored items for a derived type not in the *Object* class;
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ItemCopy
            !> a helper procedure to copy stored values for a derived type not in the *Object*
            !  class; required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ValCopy
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_ClearItems(Collection)
	        !^ To remove all of the items from the collection. <br>
            CLASS(QueueLinked), INTENT(INOUT)   :: Collection
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_Destroy(Collection)
	        !^ To destruct the collection. <br>
            CLASS(QueueLinked), INTENT(INOUT)   :: Collection
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_GetSize(Collection) RESULT(Size)
	        !^ To get the number of items stored in the collection. <br>
            CLASS(QueueLinked), INTENT(IN)  :: Collection   !! collection object
            tIndex                          :: Size         !! number of items
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----        Deferred Procedures from BaseIterable Type         -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_Move2FirstElm(Collection, Item, ItemCopy) RESULT(IsEmpty)
	        !^ To move to the first element in the collection. <br>
            CLASS(QueueLinked), INTENT(INOUT)   :: Collection   !! collection object
            !> the first element as output if requested (and available)
            CLASS(*), OPTIONAL, INTENT(INOUT)   :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> a flag indicating whether the collection contains no element or not <br>
            ! - true if the collection is empty. <br>
            ! - otherwise the first element is available.
            tLogical                            :: IsEmpty
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_Move2NextElm(Collection, Item, ItemCopy) RESULT(IsTheEnd)
	        !^ To move to the next element in the collection. <br>
            CLASS(QueueLinked), INTENT(INOUT)   :: Collection   !! collection object
            !> the next element as output if requested (and available)
            CLASS(*), OPTIONAL, INTENT(INOUT)   :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> a flag indicating whether the move to the end of the collection occurs or not <br>
            ! - true if next element is NOT available. <br>
            ! - otherwise next element is available.
            tLogical                            :: IsTheEnd
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_AddElm(Collection, Item)
	        !^ To insert the specified item at the end of the collection. <br>
            CLASS(QueueLinked), INTENT(INOUT)   :: Collection   !! collection object
            !> the item to be added to the collection
            CLASS(*),           INTENT(IN)      :: Item
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_DelElm(Collection)
	        !^ To delete the current item from a collection. <br>
            CLASS(QueueLinked), INTENT(INOUT)   :: Collection
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by QueueLinked Type            -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_CreateByArray(Collection, N, Items)
	        !^ To construct the collection from an array of items. <br>
            CLASS(QueueLinked), INTENT(INOUT)   :: Collection   !! collection object
            tIndex,             INTENT(IN)      :: N            !! number of items
            CLASS(*),           INTENT(IN)      :: Items(:)     !! array of items
        END SUBROUTINE QueueLinked_CreateByArray
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_Dequeue(Collection, Item, ItemCopy) RESULT(Success)
	        !^ To get and remove the first item of the collection.  Also, return a flag
            !  indicating whether the item is successfully removed.
            CLASS(QueueLinked), INTENT(INOUT)   :: Collection   !! collection object
            !> the item to be removed from the collection
            CLASS(*),           INTENT(INOUT)   :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully removed. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_PeekFirst(Collection, Item, ItemCopy) RESULT(Success)
	        !^ To get the first item (without removing it from the collection). Also, return
            !  a flag indicating whether the item is successfully retrieved or not.
            CLASS(QueueLinked), INTENT(IN)      :: Collection   !! collection object
            !> the item to be retrieved from the collection
            CLASS(*),           INTENT(INOUT)   :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully retrieved or not. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_ToArray(Collection, Items, ItemCopy) RESULT(Success)
	        !^ To get and remove all items from the collection.  Also, return
            !  a flag indicating whether the items are successfully removed.
            CLASS(QueueLinked), INTENT(INOUT)   :: Collection   !! collection object
            !> the items to be retrieved and removed from the collection
            CLASS(*),           INTENT(INOUT)   :: Items(:)
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the items are successfully retrieved and removed. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_GetAll(Collection, Items, ItemCopy) RESULT(Success)
	        !^ To get all items (without removing them) from the collection.  Also,
            !  return a flag indicating whether the items are available.
            CLASS(QueueLinked), INTENT(INOUT)   :: Collection   !! collection object
            !> the items to be retrieved from the collection
            CLASS(*),           INTENT(INOUT)   :: Items(1:)
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the items are successfully retrieved. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_BaseString(Obj) RESULT(Str)
            !^ To return the base-string representation of the *QueueLinked* class.
            CLASS(QueueLinked), INTENT(IN)  :: Obj  !! collection object
            tCharAlloc                      :: Str  !! base string
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for StackLinked
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackLinked_ToString(Obj) RESULT(Str)
            !^ To get the string representation of the object. <br>
            CLASS(StackLinked), INTENT(IN)  :: Obj
            tCharAlloc                      :: Str
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by StackLinked Type            -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackLinked_Pop(Collection, Item, ItemCopy) RESULT(Success)
	        !^ To get and remove the top (last) item of the collection.  Also, return
            !  a flag indicating whether the item is successfully removed.
            CLASS(StackLinked), INTENT(INOUT)   :: Collection   !! collection object
            !> the item to be removed from the collection
            CLASS(*),           INTENT(INOUT)   :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully removed. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackLinked_PeekTop(Collection, Item, ItemCopy) RESULT(Success)
	        !^ To get the top (last) item (without removing it from the collection). Also,
            !  return a flag indicating whether the item is successfully retrieved or not.
            CLASS(StackLinked), INTENT(IN)      :: Collection   !! collection object
            !> the item to be retrieved from the collection
            CLASS(*),           INTENT(INOUT)   :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully retrieved or not. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for DequeLinked
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeLinked_ToString(Obj) RESULT(Str)
            !^ To get the string representation of the object. <br>
            CLASS(DequeLinked), INTENT(IN)  :: Obj
            tCharAlloc                      :: Str
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by DequeLinked Type            -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeLinked_Move2LastElm(Collection, Item, ItemCopy) RESULT(IsEmpty)
	        !^ To move to the last element in the collection. <br>
            CLASS(DequeLinked), INTENT(INOUT)   :: Collection   !! collection object
            !> the last element as output if requested (and available)
            CLASS(*), OPTIONAL, INTENT(INOUT)   :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> a flag indicating whether the collection contains no element or not <br>
            ! - true if the collection is empty. <br>
            ! - otherwise the last element is available.
            tLogical                            :: IsEmpty
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeLinked_Move2PrevElm(Collection, Item, ItemCopy) RESULT(IsTheEnd)
	        !^ To move to the previous element in the collection. <br>
            CLASS(DequeLinked), INTENT(INOUT)   :: Collection   !! collection object
            !> the previous element as output if requested (and available)
            CLASS(*), OPTIONAL, INTENT(INOUT)   :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> a flag indicating whether the move to the end of the collection occurs or not <br>
            ! - true if previous element is NOT available. <br>
            ! - otherwise previous element is available.
            tLogical                            :: IsTheEnd
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE DequeLinked_AddFirst(Collection, Item)
	        !^ To insert the specified item at the start of the collection. <br>
            CLASS(DequeLinked), INTENT(INOUT)   :: Collection   !! collection object
            !> the item to be added to the collection
            CLASS(*),           INTENT(IN)      :: Item
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeLinked_RemoveLast(Collection, Item, ItemCopy) RESULT(Success)
	        !^ To get and remove the last item of the collection.  Also, return a flag
            !  indicating whether the item is successfully removed.
            CLASS(DequeLinked), INTENT(INOUT)   :: Collection   !! collection object
            !> the item to be removed from the collection
            CLASS(*),           INTENT(INOUT)   :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully retrieved and removed.
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeLinked_PeekLast(Collection, Item, ItemCopy) RESULT(Success)
	        !^ To get the last item (without removing it from the collection). Also, return
            !  a flag indicating whether the item is successfully retrieved or not.
            CLASS(DequeLinked), INTENT(IN)      :: Collection   !! collection object
            !> the item to be retrieved from the collection
            CLASS(*),           INTENT(INOUT)   :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully retrieved or not.
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for ListLinked
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListLinked_ToString(Obj) RESULT(Str)
            !^ To get the string representation of the object. <br>
            CLASS(ListLinked), INTENT(IN)   :: Obj
            tCharAlloc                      :: Str
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by ListLinked Type            -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListLinked_AddAt(Collection, Index, Item) RESULT(Success)
	        !^ To insert the specified item at the specified position in the collection.
            CLASS(ListLinked), INTENT(INOUT)    :: Collection   !! collection object
            !> index indicating the position in the collection to add the item
            tIndex,            INTENT(IN)       :: Index
            !> the item to be added to the collection
            CLASS(*),          INTENT(IN)       :: Item
            !> flag indicating whether the item is successfully added or not.
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListLinked_RemoveAt(Collection, Index, Item, ItemCopy) RESULT(Success)
	        !^ To get and remove the item at the specified position.  Also, return a flag
            !  indicating whether the item is successfully removed.
            CLASS(ListLinked), INTENT(INOUT)    :: Collection   !! collection object
            !> index indicating the position in the collection to retrieve and remove the item
            tIndex,            INTENT(IN)       :: Index
            !> the item to be removed from the collection
            CLASS(*),          INTENT(INOUT)    :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully retrieved and removed.
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListLinked_PeekAt(Collection, Index, Item, ItemCopy) RESULT(Success)
	        !^ To retrieve the item at the specified position (without removing it from the
            !  collection). Also, return a flag indicating whether the item is successfully
            !  retrieved or not.
            CLASS(ListLinked), INTENT(INOUT)    :: Collection   !! collection object
            !> index indicating the position in the collection to retrieve the item
            tIndex,            INTENT(IN)       :: Index
            !> the item to be retrieved from the collection
            CLASS(*),          INTENT(INOUT)    :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully retrieved or not.
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! -----------------------------------------------------------------------------
! -----                     LinkedNode Procedures                         -----
! -----------------------------------------------------------------------------

SUBROUTINE LinkedNode_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the LinkedNode object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LinkedNode),  INTENT(IN)  :: SrcObj   !! source object
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

    SELECT TYPE(DstObj)
    TYPE IS (LinkedNode)
        CALL SrcObj%CopyDLLNode(DstObj)
        CALL SrcObj%Store%Copy(DstObj%Store, IsDeep)
    CLASS DEFAULT
        CALL Handle_ErrLevel('LinkedNode_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE LinkedNode_Copy

!******************************************************************************

FUNCTION LinkedNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LinkedNode), INTENT(IN)   :: LhsObj   !! an object
    CLASS(Object),     INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (LinkedNode)
        Flag = FalseVal
        IF (.NOT.LhsObj%Store%IsEqualTo(RhsObj%Store)) RETURN
        Flag = LhsObj%IsDLLNodeEqual(RhsObj)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION LinkedNode_IsEqualTo

!******************************************************************************

SUBROUTINE LinkedNode_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of the LinkedNode object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LinkedNode), INTENT(INOUT)    :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%Store%MemFree()
    CALL Obj%FreePointers()

    RETURN

END SUBROUTINE LinkedNode_MemFree

!******************************************************************************

FUNCTION LinkedNode_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LinkedNode), INTENT(IN)   :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Str = '{LinkedNode : ' // Obj%Store%ToString() // '}'

    RETURN

END FUNCTION LinkedNode_ToString

!******************************************************************************

FUNCTION LinkedNode_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LinkedNode), INTENT(IN)   :: Obj
    tIndex                          :: Code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: AdjNode

! FLOW

    ! get code from previous node
    AdjNode => Obj%GetPrevious()
    SELECT TYPE (Prev => AdjNode)
    TYPE IS (LinkedNode)
        Code = Prev%Store%HashCode()
    END SELECT

    ! add code from this node
    Code = Code + Obj%Store%HashCode()

    ! add code from next node
    AdjNode => Obj%GetNext()
    SELECT TYPE (Next => AdjNode)
    TYPE IS (LinkedNode)
        Code = Code + Next%Store%HashCode()
    END SELECT

    ! free pointer    
    NULLIFY(AdjNode)
    
    RETURN

END FUNCTION LinkedNode_HashCode

! ------------------------------------------------------------------------------
! -----                     Final Procedures                               -----
! ------------------------------------------------------------------------------

SUBROUTINE DequeLinked_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(DequeLinked), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE DequeLinked_Finalize

!******************************************************************************

SUBROUTINE ListLinked_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ListLinked), INTENT(INOUT)   :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE ListLinked_Finalize

!******************************************************************************

SUBROUTINE QueueLinked_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(QueueLinked), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE QueueLinked_Finalize

!******************************************************************************

SUBROUTINE StackLinked_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(StackLinked), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE StackLinked_Finalize

!******************************************************************************

END MODULE MClass_LinkedLists

!******************************************************************************
