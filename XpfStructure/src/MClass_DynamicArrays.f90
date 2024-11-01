
MODULE MClass_DynamicArrays

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains *dynamic-array-based* collection types and their related routines.
!   A *dynamic-array-based* type is a collection type employing a resizable array (or the
!   so-called dynamic array) that can increase or decrease its size depending on the current
!   collection size (number of items contained in the collection) relative to the current
!   collection capacity.  <br>
!   Available collection types based on the dynamic-array concept include:  <br>
!   - the *StackArray* type that represents a last-in-first-out (LIFO) stack,  <br>
!   - the *QueueArray* type that represents a first-in-first-out (FIFO) queue,  <br>
!   - the *DequeArray* type that represents a double-ended queue (deque), and  <br>
!   - the *ListArray* type that represents a list where an item can be added, removed or
!     retrieved at the (valid) specified index.  <br>
!   <br>
!   **Usage Notes**:  <br>
!   - All *dynamic-array-based* collection types commonly require an explicit construction
!     before using other provided operations.  There are two methods provided to create a
!     collection.  First, the *CreateEmpty* method constructs an empty collection with the
!     specified initial capacity.  Second, the *Construction* method constructs a collection
!     based on the given input (either from an array of items or from another collection). <br>
!   - All available collection types are subtypes of the *BaseDynArr* abstract type, which is
!     a private type that is only available in this module.  The *BaseDynArr* type provides
!     the *Growing* (private) method to expand the collection's capacity and the *Shrinking*
!     (also private) method to reduce the capacity. <br>
!     When the collection is full (its current size is equal to its current capacity), its
!     capacity is doubled by default.  However, if the *IncSize* argument is specified during
!     a construction of the collection, the collection's capacity increases by the amount
!     specified (the specified value must be positive) instead of doubling.  <br>
!     When the collection's size is reducing due to removing items from the collection, its
!     capacity stays the same by default.  Nevertheless, if the *Shrink* flag is specified
!     and its value is true during a construction of the collection, the collection's capacity
!     is halved when its current size is one quarter of its capacity.  <br>
!   - Instead of using as a deque, the *DequeArray* type can also be used as a FIFO queue or
!     a LIFO stack.  <br>
!   - Instead of using as a list, the *ListArray* type can also be used as a deque, a FIFO
!     queue or a LIFO stack. <br>
!   <br>
!   **Important Notes**:  <br>
!   (1) By design, although all *dynamic-array-based* types are generic containers meaning
!       that they can be used to store items of any data types, they must be employed to store
!       items of only one specific data type.  To store items of another data type, they must
!       be cleared and/or destructed before inserting items of different data type. <br>
!   (2) To be able to successfully retrieve stored items, the specified output item must have
!       the same (concrete) type as that of stored items. <br>
!   (3) Additionally, if type of the stored items is a derived one that is NOT in the *Object*
!       class and this type has allocatable and/or pointer components, a user must specify the
!       optional *ItemCopy* argument when trying to retrieve the stored items.  The *ItemCopy*
!       argument is a user-defined procedure to copy an unlimited polymorphic entity.  It must
!       must provide a type-guard statement to check the compatibility of concrete types of its
!       arguments.  It must also handle the storage allocation (and/or the assignment) of those
!       allocatable (and/or pointer) components as appropriate. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_SIntUtil,     ONLY: MAX_I32, MAX_I64, ToChar => ToDecStrSigned
    USE MBase_ByteUtil,     ONLY: AnyType_GetByteSize
    USE MClass_Object,      ONLY: Object, ASSIGNMENT(=)
    USE MClass_MemoryPool
    USE MClass_BaseCollection
    USE MClass_BaseIterable
    USE MClass_GenData
    USE MClass_CharBuffer

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: DequeArray
    PUBLIC :: ListArray
    PUBLIC :: QueueArray
    PUBLIC :: StackArray

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_DynamicArrays'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! maximum capacity of a dynamic-array-based collection
#ifdef Indx64Bits
    tIndex,    PARAMETER    :: MaxCapacity = MAX_I64
#else
    tIndex,    PARAMETER    :: MaxCapacity = MAX_I32
#endif

!** DERIVED TYPE DEFINITIONS
    !> The *BaseDynArr* type is an abstract dynamic-array type that provides resizing operations
    !  to grow or shrink the collection capacity depending on its current size (number of items
    !  contained).  This is a private type. <br>
    TYPE, ABSTRACT, EXTENDS(BaseIterable) :: BaseDynArr
        PRIVATE
        !> incremental size of the collection when the collection is full.
        !  Its value will be reset to 0 if the optional input is NOT
        !  specified during construction
        tIndex                      :: IncSize = 16_kIndex
        !> flag to shrink the collection capacity
        tLogical                    :: Shrink = FalseVal
        !> items stored in the collection.
        TYPE(GenData), ALLOCATABLE  :: Items(:)
        !> memory pool of stored items
        TYPE(MemoryPool)            :: ItemPool
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *Offset* is a deferred procedure to get an offset.  This procedure is intended for
        !   internal uses only.
        PROCEDURE(IfaceOffset), DEFERRED    :: Offset
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: BaseDynArr_CreateByArray
        PROCEDURE, PRIVATE  :: Resize       => BaseDynArr_MemResize
        PROCEDURE, PRIVATE  :: Growing      => BaseDynArr_Growing
        PROCEDURE, PRIVATE  :: Shrinking    => BaseDynArr_Shrinking
        PROCEDURE, PRIVATE  :: BaseCopy     => BaseDynArr_BaseCopy
        PROCEDURE, PRIVATE  :: BaseFree     => BaseDynArr_BaseFree
        PROCEDURE, PRIVATE  :: BaseReset    => BaseDynArr_BaseReset
        PROCEDURE, PRIVATE  :: BaseString   => BaseDynArr_ToString
        PROCEDURE, PRIVATE  :: BaseHashCode => BaseDynArr_HashCode
        ! ---------------------------------------------------------------------
        ! -----                     public procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty collection. <br>
        !  **Usage**: <br>
        !           ! create an empty collection with specified initial capacity <br>
        !   --->    CALL Collection%CreateEmpty(25) <br>
        !           ! create a collection and specify the optional incremental size <br>
        !   --->    CALL Collection%CreateEmpty(25, IncSize=16) <br>
        !           ! create a collection and specify the optional shrink flag <br>
        !   --->    CALL Collection%CreateEmpty(25, Shrink=.TRUE.)
        PROCEDURE   :: CreateEmpty  => BaseDynArr_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new collection from an array of items or from another
        !                collection. <br>
        !  **Usage**: <br>
        !   ! create a collection from an array of 25 items <br>
        !   --->    CALL Collection%Construct(25, Arr) <br>
        !   ! create a collection and specify the optional incremental size <br>
        !   --->    CALL Collection%Construct(25, Arr, IncSize=16) <br>
        !   ! create a collection and specify the optional shrink flag <br>
        !   --->    CALL Collection%Construct(25, Arr, Shrink=.TRUE.) <br>
        !   ! create a collection from another collection <br>
        !   --->    CALL Collection%Construct(OtherCollection)
        GENERIC     :: Construct    => BaseDynArr_CreateByArray
    END TYPE BaseDynArr
    !> **Description**: <br>
    !   The *StackArray* type is a collection type that employs a resizable-array implementation
    !   to provide common operations for a LIFO stack. <br>
    !  **Usage Overview**: <br>
    !   The *StackArray* type is a *stack* collection type that provides common operations of a
    !   LIFO stack.  Their operations can be categorized as follows: <br>
    !   (1) Construction and destruction.  Methods for these operations include <br>
    !   (1.1) *Construct* method - method to construct the collection either from an array of items
    !         or from another collection, <br>
    !   (1.2) *CreateEmpty* method - method to construct an empty collection, <br>
    !   (1.3) *Destruct* method - method to destruct the collection. <br>
    !   (2) Insert and remove.  Methods for these operations include <br>
    !   (2.1) *Push* method - method to insert an item at the top (end) of the collection, <br>
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
    !   (4.2) *MoveForward* method - method to move forward to the next item.
    TYPE, EXTENDS(BaseDynArr) :: StackArray
        PRIVATE
        !> pointer to top (last) item of the stack
        tIndex      :: Top    = 0_kIndex
        !> pointer to current item of the iteration
        tIndex      :: Cursor = 0_kIndex
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseCollection Type      -----
        ! ---------------------------------------------------------------------
        !> Use the *Construct* method to create a collection from another collection.
        PROCEDURE   :: CopyCollection   => StackArray_CopyCollection
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        PROCEDURE   :: Clear            => StackArray_ClearItems
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        PROCEDURE   :: Destruct         => StackArray_Destroy
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize          => StackArray_GetSize
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseIterable Type        -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag indicating
        !                whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartFirst() <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstItem)
        PROCEDURE   :: StartFirst       => StackArray_Move2FirstElm
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next iteration and return a flag indicating whether
        !                the cursor pointer has reached the end of the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextItem) <br>
        !  **Note**: See the <a href="../module/mclass_baseiterable.html">BaseIterable</a> type
        !            for illustrations of an iteration over a collection using the *StartFirst*
        !            and *MoveForward* methods.<br>
        PROCEDURE   :: MoveForward      => StackArray_Move2NextElm
        !> Use the *Push* method in place of the *Insert* method to add an item to the stack.
        PROCEDURE   :: Insert           => StackArray_AddElm
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete the current item from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the *StartFirst* and
        !       *MoveForward* methods.  Therefore, after the call to one of those methods and then
        !       calling this one will result in a removal of the current item of the iteration
        !       (i.e. the same item that can be retrieved via the *StartFirst* and *MoveForward*
        !       methods). <br>
        PROCEDURE   :: Delete           => StackArray_DelElm
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the collection.  Also, return a flag
        !                indicating whether the items are successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Items) <br>
        !   --->    IF (.NOT.Collection%ToArray(Items)) DoSomething
        PROCEDURE   :: ToArray          => StackArray_ToArray
        !> **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the collection. Also, return
        !                a flag indicating whether the items are available. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Items) <br>
        !   --->    IF (.NOT.Collection%GetAll(Items)) DoSomething
        PROCEDURE   :: GetAll           => StackArray_GetAll
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by StackArray Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Push <br>
        !  **Purpose**:  To insert the specified item at the top (end) of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Push(Item)
        GENERIC     :: Push             => Insert
        !> **Type-Bound Function**: Pop <br>
        !  **Purpose**:  To get and remove the top (last) item of the collection. Also, return a
        !                flag indicating whether the item is successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%Pop(Item) <br>
        !   --->    IF (.NOT.Collection%Pop(Item)) DoSomething
        PROCEDURE   :: Pop              => StackArray_Pop
        !> **Type-Bound Function**: PeekTop <br>
        !  **Purpose**:  To get the top (last) item (without removing it from the collection).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekTop(Item) <br>
        !   --->    IF (.NOT.Collection%PeekTop(Item)) DoSomething
        PROCEDURE   :: PeekTop          => StackArray_PeekTop
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => StackArray_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => StackArray_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => StackArray_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => StackArray_ToString
        ! ---------------------------------------------------------------------
        ! -----                     Overridden Procedure                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => StackArray_HashCode
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseDynArr Type          -----
        ! ---------------------------------------------------------------------
        !> This procedure is intended for internal uses only.
        PROCEDURE   :: Offset       => StackArray_Offset
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: StackArray_Finalize
        ! ---------------------------------------------------------------------
    END TYPE StackArray
    !> **Description**: <br>
    !   The *QueueArray* type is a collection type that employs a resizable-array implementation
    !   to provide common operations for a FIFO queue. <br>
    !  **Usage Overview**: <br>
    !   The *QueueArray* type is a *queue* collection type that provides common operations of a
    !   FIFO queue.  Their operations can be categorized as follows: <br>
    !   (1) Construction and destruction.  Methods for these operations include <br>
    !   (1.1) *Construct* method - method to construct the collection either from an array of items
    !          or from another collection, <br>
    !   (1.2) *CreateEmpty* method - method to construct an empty collection, <br>
    !   (1.3) *Destruct* method - method to destruct the collection. <br>
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
    !   (4.2) *MoveForward* method - method to move forward to the next item.
    TYPE, EXTENDS(BaseDynArr) :: QueueArray
        PRIVATE
        !> pointer to first item of the queue
        tIndex      :: First  = 1_kIndex
        !> pointer to next to last item of the queue (i.e. the next available slot)
        tIndex      :: Last   = 1_kIndex
        !> size of the collection (number of items)
        tIndex      :: Size   = 0_kIndex
        !> pointer to current item of the iteration
        tIndex      :: Cursor = 0_kIndex
        !> direction of the iteration <br>
        !  - positive -> forward iteration
        !  - negative -> backward iteration (only applicable for deque and list)
        !  - zero     -> iteration not yet start
        tSInt32     :: Dir = 0
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseCollection Type      -----
        ! ---------------------------------------------------------------------
        !> Use the *Construct* method to create a collection from another collection.
        PROCEDURE   :: CopyCollection   => QueueArray_CopyCollection
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        PROCEDURE   :: Clear            => QueueArray_ClearItems
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        PROCEDURE   :: Destruct         => QueueArray_Destroy
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize          => QueueArray_GetSize
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseIterable Type        -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag indicating whether
        !                the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartFirst() <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstItem)
        PROCEDURE   :: StartFirst       => QueueArray_Move2FirstElm
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next iteration and return a flag indicating whether
        !                the cursor pointer has reached the end of the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextItem) <br>
        !  **Note**: See the <a href="../module/mclass_baseiterable.html">BaseIterable</a> type
        !            for illustrations of an iteration over a collection using the *StartFirst*
        !            and *MoveForward* methods.<br>
        PROCEDURE   :: MoveForward      => QueueArray_Move2NextElm
        !> Use the *Enqueue* method in place of the *Insert* method to add an item to the queue.
        PROCEDURE   :: Insert           => QueueArray_AddElm
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete the current item from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the *StartFirst* and
        !       *MoveForward* methods.  Therefore, after the call to one of those methods and then
        !       calling this one will result in a removal of the current item of the iteration
        !       (i.e. the same item that can be retrieved via the *StartFirst* and *MoveForward*
        !       methods). <br>
        PROCEDURE   :: Delete           => QueueArray_DelElm
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the collection.  Also, return a flag
        !                indicating whether the items are successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Items) <br>
        !   --->    IF (.NOT.Collection%ToArray(Items)) DoSomething
        PROCEDURE   :: ToArray          => QueueArray_ToArray
        !> **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the collection. Also, return
        !                a flag indicating whether the items are available. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Items) <br>
        !   --->    IF (.NOT.Collection%GetAll(Items)) DoSomething
        PROCEDURE   :: GetAll           => QueueArray_GetAll
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by QueueArray Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Enqueue <br>
        !  **Purpose**:  To insert the specified item at the end of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Enqueue(Item)
        GENERIC     :: Enqueue          => Insert
        !> **Type-Bound Function**: Dequeue <br>
        !  **Purpose**:  To get and remove the front (first) item of the collection. Also, return
        !                a flag indicating whether the item is successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%Dequeue(Item) <br>
        !   --->    IF (.NOT.Collection%Dequeue(Item)) DoSomething
        PROCEDURE   :: Dequeue          => QueueArray_Dequeue
        !> **Type-Bound Function**: PeekFirst <br>
        !  **Purpose**:  To get the front (first) item (without removing it from the collection).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekFirst(Item) <br>
        !   --->    IF (.NOT.Collection%PeekFirst(Item)) DoSomething
        PROCEDURE   :: PeekFirst        => QueueArray_PeekFirst
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => QueueArray_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => QueueArray_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => QueueArray_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => QueueArray_ToString
        ! ---------------------------------------------------------------------
        ! -----                     Overridden Procedure                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => QueueArray_HashCode
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseDynArr Type          -----
        ! ---------------------------------------------------------------------
        !> This procedure is intended for internal uses only.
        PROCEDURE   :: Offset       => QueueArray_Offset
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: QueueArray_Finalize
        ! ---------------------------------------------------------------------
    END TYPE QueueArray
    !> **Description**: <br>
    !   The *DequeArray* type is a collection type that employs a resizable-array implementation
    !   to provide common operations for a double-ended queue (deque).   It can be used as a
    !   FIFO queue or a LIFO stack as well. <br>
    !  **Usage Overview**: <br>
    !   The *DequeArray* type is a *deque* collection type that provides common operations of a
    !   double-ended queue (deque).  Their operations can be categorized as follows: <br>
    !   (1) Construction and destruction.  Methods for these operations include <br>
    !   (1.1) *Construct* method - method to construct the collection either from an array of items
    !          or from another collection, <br>
    !   (1.2) *CreateEmpty* method - method to construct an empty collection, <br>
    !   (1.3) *Destruct* method - method to destruct the collection. <br>
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
    !   The *DequeArray* type also supports the usual *Enqueue*, *Dequeue* and *PeekFirst*
    !   operations of a FIFO queue as well as the usual *Push*, *Pop* and *PeekTop* operations
    !   of a LIFO stack.  Therefore, when using as a queue or a stack, these operations can be
    !   used in place of *insert*, *remove*, *peek* operations of a deque as desired.
    TYPE, EXTENDS(QueueArray) :: DequeArray
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by DequeArray Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start the *backward* iteration and return a flag indicating
        !                whether the collection is empty or not. <br>
        !  **Usage**: see *MoveBackward* procedure.
        PROCEDURE   :: StartLast    => DequeArray_Move2LastElm
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move to the next iteration (in reverse order) and return
        !                a flag indicating whether the cursor pointer has reached the
        !                end of the collection or not. <br>
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
        PROCEDURE   :: MoveBackward => DequeArray_Move2PrevElm
        !> **Type-Bound Subroutine**: AddFirst <br>
        !  **Purpose**:  To insert the specified item at the front of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%AddFirst(Item)
        PROCEDURE   :: AddFirst     => DequeArray_AddFirst
        !> **Type-Bound Subroutine**: AddLast <br>
        !  **Purpose**:  To insert the specified item at the end of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%AddLast(Item)
        GENERIC     :: AddLast      => Insert
        !> **Type-Bound Function**: RemoveFirst <br>
        !  **Purpose**:  To get and remove the front (first) item of the collection. Also, return
        !                a flag indicating whether the item is successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%RemoveFirst(Item) <br>
        !   --->    IF (.NOT.Collection%RemoveFirst(Item)) DoSomething
        GENERIC     :: RemoveFirst  => Dequeue
        !> **Type-Bound Function**: RemoveLast <br>
        !  **Purpose**:  To get and remove the last item of the collection. Also, return a flag
        !                indicating whether the item is successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%RemoveLast(Item) <br>
        !   --->    IF (.NOT.Collection%RemoveLast(Item)) DoSomething
        PROCEDURE   :: RemoveLast   => DequeArray_RemoveLast
        !> **Type-Bound Function**: PeekLast <br>
        !  **Purpose**:  To get the last item (without removing it from the collection). Also,
        !                return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekLast(Item) <br>
        !   --->    IF (.NOT.Collection%PeekLast(Item)) DoSomething
        PROCEDURE   :: PeekLast     => DequeArray_PeekLast
        ! ---------------------------------------------------------------------
        ! -----             Redefined Stack Operations                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Push <br>
        !  **Purpose**:  To insert the specified item at the end (top) of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Push(Item)
        GENERIC     :: Push         => Insert
        !> **Type-Bound Function**: Pop <br>
        !  **Purpose**:  To get and remove the last (top) item of the collection. Also, return a
        !                flag indicating whether the item is successfully removed. <br>
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
        PROCEDURE   :: ToString     => DequeArray_ToString
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: DequeArray_Finalize
        ! ---------------------------------------------------------------------
    END TYPE DequeArray
    !> **Description**: <br>
    !   The *ListArray* type is a collection type that employs a resizable-array implementation
    !   to provide common operations for a list. <br>
    !  **Usage Overview**: <br>
    !   The *ListArray* type provides insert, remove and peek operations at a specified index
    !   where the index must be between 1 and the collection size.  The *ListArray* type is
    !   a subtype of the *DequeArray* type; therefore, all operations available for the
    !   *DequeArray* type are also available for the *ListArray* type.  As a result, it can
    !   thus be used as a deque, a FIFO queue or a LIFO stack.  <br>
    TYPE, EXTENDS(DequeArray) :: ListArray
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by DequeArray Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: AddAt <br>
        !  **Purpose**:  To insert the specified item at the specified index where the index
        !       must be between 1 and the collection size. Also, return a flag indicating
        !       whether the item is successfully added. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%AddAt(Index, Item) <br>
        !   --->    IF (.NOT.Collection%AddAt(Index, Item)) DoSomething
        PROCEDURE   :: AddAt      => ListArray_AddAt
        !> **Type-Bound Function**: RemoveAt <br>
        !  **Purpose**:  To get and remove the item at the specified index where the index
        !       must be between 1 and the collection size. Also, return a flag indicating
        !       whether the item is successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%RemoveAt(Index, Item) <br>
        !   --->    IF (.NOT.Collection%RemoveAt(Index, Item)) DoSomething
        PROCEDURE   :: RemoveAt   => ListArray_RemoveAt
        !> **Type-Bound Function**: PeekAt <br>
        !  **Purpose**:  To get the item (without removing it from the collection) at the
        !       specified index where the index must be between 1 and the collection size.
        !       Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekAt(Index, Item) <br>
        !   --->    IF (.NOT.Collection%PeekAt(Index, Item)) DoSomething
        PROCEDURE   :: PeekAt     => ListArray_PeekAt
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => ListArray_ToString
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: ListArray_Finalize
        ! ---------------------------------------------------------------------
    END TYPE ListArray

!** INTERFACE DEFINITIONS:
    ! abstract interface for BaseDynArr
    ABSTRACT INTERFACE
        !> IfaceOffset is an interface for a procedure to get an index of the first item
        !  in the collection.
        FUNCTION IfaceOffset(Collection) RESULT(First)
            IMPORT
            CLASS(BaseDynArr), INTENT(INOUT)    :: Collection   !! collection object
            tIndex                              :: First        !! index of the first item
        END FUNCTION
    END INTERFACE
    ! interfaces for StackArray
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_Copy(SrcObj, DstObj, IsDeep)
            !^ To copy the source object to the destination object. <br>
            CLASS(StackArray),  INTENT(IN)  :: SrcObj   !! source object
            CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
            tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
            !^ Flag indicating whether to perform deep copy or shallow copy. <br>
            !  - If present and true, perform a deep copy. <br>
            !  - If present and false, perform a shallow copy. <br>
            !  - If not present, perform either a shallow or a deep copy that is naturally most
            !    suitable for the object's components.
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)
            !^ To check whether LhsObj and RhsObj are equal or not. <br>
            CLASS(StackArray), INTENT(IN)   :: LhsObj   !! an object
            CLASS(Object),     INTENT(IN)   :: RhsObj   !! another object
            tLogical                        :: Flag     !! true if both objects are equal
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_MemFree(Obj)
            !^ To free memory of the object. <br>
            CLASS(StackArray), INTENT(INOUT)    :: Obj
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_ToString(Obj) RESULT(Str)
            !^ To get a string representation of the object. <br>
            CLASS(StackArray), INTENT(IN)   :: Obj
            tCharAlloc                      :: Str
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_HashCode(Obj) RESULT(Code)
            !^ To compute hash code for this object.
            CLASS(StackArray), INTENT(IN)   :: Obj
            tIndex                          :: Code
        END FUNCTION StackArray_HashCode
        ! ---------------------------------------------------------------------
        ! -----      Deferred Procedures from BaseCollection Type         -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_CopyCollection(This, Other, ItemCopy, ValCopy)
	        !^ To creates a new collection (This) from the given collection (Other). <br>
            CLASS(StackArray),     INTENT(INOUT)    :: This     !! collection object to be created
            CLASS(BaseCollection), INTENT(INOUT)    :: Other    !! collection object to be copied
            !> a helper procedure to copy stored items for a derived type not in the *Object* class;
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ItemCopy
            !> a helper procedure to copy stored values for a derived type not in the *Object*
            !  class; required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ValCopy
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_ClearItems(Collection)
	        !^ To remove all of the items from the collection. <br>
            CLASS(StackArray), INTENT(INOUT)    :: Collection   !! collection object
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_Destroy(Collection)
	        !^ To destruct the collection. <br>
            CLASS(StackArray), INTENT(INOUT)    :: Collection   !! collection object
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_GetSize(Collection) RESULT(Size)
            !^ To get the collection size (number of items in the collection). <br>
            CLASS(StackArray), INTENT(IN)   :: Collection   !! collection object
            tIndex                          :: Size         !! number of items
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----        Deferred Procedures from BaseIterable Type         -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_Move2FirstElm(Collection, Item, ItemCopy) RESULT(IsEmpty)
	        !^ To move to the first element in the collection. <br>
            CLASS(StackArray),  INTENT(INOUT)   :: Collection   !! collection object
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
        MODULE FUNCTION StackArray_Move2NextElm(Collection, Item, ItemCopy) RESULT(IsTheEnd)
	        !^ To move to the next element in the collection. <br>
            CLASS(StackArray),  INTENT(INOUT)   :: Collection   !! collection object
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
        MODULE SUBROUTINE StackArray_AddElm(Collection, Item)
	        !^ To insert the specified item at the top (end) of the collection. <br>
            CLASS(StackArray), INTENT(INOUT)    :: Collection   !! collection object
            !> the item to be added to the collection
            CLASS(*),          INTENT(IN)       :: Item
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_DelElm(Collection)
	        !^ To delete the current item from a collection. <br>
            CLASS(StackArray), INTENT(INOUT)    :: Collection   !! collection object
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        ! -----        Deferred Procedures from BaseDynArr Type         -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_Offset(Collection) RESULT(First)
            !^ To get an index pointing to the first item. <br>
            CLASS(StackArray), INTENT(INOUT)    :: Collection   !! collection object
            tIndex                              :: First        !! first index
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by StackArray Type            -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_Pop(Collection, Item, ItemCopy) RESULT(Success)
	        !^ To get and remove the top (last) item of the collection.  Also, return
            !  a flag indicating whether the item is successfully removed.
            CLASS(StackArray), INTENT(INOUT)    :: Collection   !! collection object
            !> the item to be removed from the collection
            CLASS(*),          INTENT(INOUT)    :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully removed. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_PeekTop(Collection, Item, ItemCopy) RESULT(Success)
	        !^ To get the top (last) item (without removing it from the collection). Also,
            !  return a flag indicating whether the item is successfully retrieved or not.
            CLASS(StackArray), INTENT(IN)       :: Collection   !! collection object
            !> the item to be retrieved from the collection
            CLASS(*),          INTENT(INOUT)    :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully retrieved or not. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_ToArray(Collection, Items, ItemCopy) RESULT(Success)
	        !^ To get and remove all items from the collection.  Also, return
            !  a flag indicating whether the items are successfully removed.
            CLASS(StackArray), INTENT(INOUT)    :: Collection   !! collection object
            !> the items to be retrieved and removed from the collection
            CLASS(*),          INTENT(INOUT)    :: Items(:)
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the items are successfully retrieved and removed. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_GetAll(Collection, Items, ItemCopy) RESULT(Success)
	        !^ To get all items (without removing them) from the collection.  Also,
            !  return a flag indicating whether the items are available.
            CLASS(StackArray), INTENT(INOUT)    :: Collection   !! collection object
            !> the items to be retrieved from the collection
            CLASS(*),          INTENT(INOUT)    :: Items(1:)
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the items are successfully retrieved. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for QueueArray
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_Copy(SrcObj, DstObj, IsDeep)
            !^ To copy the source object to the destination object. <br>
            CLASS(QueueArray),  INTENT(IN)  :: SrcObj   !! source object
            CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
            tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
            !^ Flag indicating whether to perform deep copy or shallow copy. <br>
            !  - If present and true, perform a deep copy. <br>
            !  - If present and false, perform a shallow copy. <br>
            !  - If not present, perform either a shallow or a deep copy that is naturally most
            !    suitable for the object's components.
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)
            !^ To check whether LhsObj and RhsObj are equal or not. <br>
            CLASS(QueueArray), INTENT(IN)   :: LhsObj   !! an object
            CLASS(Object),     INTENT(IN)   :: RhsObj   !! another object
            tLogical                        :: Flag     !! true if both objects are equal
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_MemFree(Obj)
            !^ To free memory of the object. <br>
            CLASS(QueueArray), INTENT(INOUT)    :: Obj
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_ToString(Obj) RESULT(Str)
            !^ To get the string representation of the object. <br>
            CLASS(QueueArray), INTENT(IN)   :: Obj
            tCharAlloc                      :: Str
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_HashCode(Obj) RESULT(Code)
            !^ To compute hash code for this object.
            CLASS(QueueArray), INTENT(IN)   :: Obj
            tIndex                          :: Code
        END FUNCTION QueueArray_HashCode
        ! ---------------------------------------------------------------------
        ! -----      Deferred Procedures from BaseCollection Type         -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_CopyCollection(This, Other, ItemCopy, ValCopy)
	        !^ To creates a new collection (This) from the given collection (Other). <br>
            CLASS(QueueArray),     INTENT(INOUT)    :: This     !! collection object to be created
            CLASS(BaseCollection), INTENT(INOUT)    :: Other    !! collection object to be copied
            !> a helper procedure to copy stored items for a derived type not in the *Object* class;
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ItemCopy
            !> a helper procedure to copy stored values for a derived type not in the *Object*
            !  class; required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ValCopy
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_ClearItems(Collection)
	        !^ To remove all of the items from the collection. <br>
            CLASS(QueueArray), INTENT(INOUT) :: Collection   !! collection object
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_Destroy(Collection)
	        !^ To destruct the collection. <br>
            CLASS(QueueArray), INTENT(INOUT) :: Collection   !! collection object
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_GetSize(Collection) RESULT(Size)
	        !^ To get the number of items stored in the collection. <br>
            CLASS(QueueArray), INTENT(IN)   :: Collection   !! collection object
            tIndex                          :: Size         !! number of items
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----        Deferred Procedures from BaseIterable Type         -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_Move2FirstElm(Collection, Item, ItemCopy) RESULT(IsEmpty)
	        !^ To move to the first element in the collection. <br>
            CLASS(QueueArray),  INTENT(INOUT)   :: Collection   !! collection object
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
        MODULE FUNCTION QueueArray_Move2NextElm(Collection, Item, ItemCopy) RESULT(IsTheEnd)
	        !^ To move to the next element in the collection. <br>
            CLASS(QueueArray),  INTENT(INOUT)   :: Collection   !! collection object
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
        MODULE SUBROUTINE QueueArray_AddElm(Collection, Item)
	        !^ To insert the specified item at the end of the collection. <br>
            CLASS(QueueArray), INTENT(INOUT)    :: Collection   !! collection object
            !> the item to be added to the collection
            CLASS(*),          INTENT(IN)       :: Item
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_DelElm(Collection)
	        !^ To delete the current item from a collection. <br>
            CLASS(QueueArray), INTENT(INOUT)    :: Collection   !! collection object
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        ! -----        Deferred Procedures from BaseDynArr Type         -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_Offset(Collection) RESULT(First)
            !^ To get an index pointing to the first item. <br>
            CLASS(QueueArray), INTENT(INOUT)    :: Collection   !! collection object
            tIndex                              :: First        !! first index
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by QueueArray Type            -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_Dequeue(Collection, Item, ItemCopy) RESULT(Success)
	        !^ To get and remove the first item of the collection.  Also, return a flag
            !  indicating whether the item is successfully removed.
            CLASS(QueueArray), INTENT(INOUT)    :: Collection   !! collection object
            !> the item to be removed from the collection
            CLASS(*),          INTENT(INOUT)    :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully removed. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_PeekFirst(Collection, Item, ItemCopy) RESULT(Success)
	        !^ To get the first item (without removing it from the collection). Also, return
            !  a flag indicating whether the item is successfully retrieved or not.
            CLASS(QueueArray), INTENT(IN)       :: Collection   !! collection object
            !> the item to be retrieved from the collection
            CLASS(*),          INTENT(INOUT)    :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully retrieved or not. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_ToArray(Collection, Items, ItemCopy) RESULT(Success)
	        !^ To get and remove all items from the collection.  Also, return
            !  a flag indicating whether the items are successfully removed.
            CLASS(QueueArray), INTENT(INOUT)    :: Collection   !! collection object
            !> the items to be retrieved and removed from the collection
            CLASS(*),          INTENT(INOUT)    :: Items(:)
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the items are successfully retrieved and removed. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_GetAll(Collection, Items, ItemCopy) RESULT(Success)
	        !^ To get all items (without removing them) from the collection.  Also,
            !  return a flag indicating whether the items are available.
            CLASS(QueueArray), INTENT(INOUT)    :: Collection   !! collection object
            !> the items to be retrieved from the collection
            CLASS(*),          INTENT(INOUT)    :: Items(1:)
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the items are successfully retrieved. <br>
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for DequeArray
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeArray_ToString(Obj) RESULT(Str)
            !^ To get the string representation of the object. <br>
            CLASS(DequeArray), INTENT(IN)   :: Obj
            tCharAlloc                      :: Str
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by DequeArray Type            -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeArray_Move2LastElm(Collection, Item, ItemCopy) RESULT(IsEmpty)
	        !^ To move to the last element in the collection. <br>
            CLASS(DequeArray),  INTENT(INOUT)   :: Collection   !! collection object
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
        MODULE FUNCTION DequeArray_Move2PrevElm(Collection, Item, ItemCopy) RESULT(IsTheEnd)
	        !^ To move to the previous element in the collection. <br>
            CLASS(DequeArray),  INTENT(INOUT)   :: Collection   !! collection object
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
        MODULE SUBROUTINE DequeArray_AddFirst(Collection, Item)
	        !^ To insert the specified item at the start of the collection. <br>
            CLASS(DequeArray), INTENT(INOUT)    :: Collection   !! collection object
            !> the item to be added to the collection
            CLASS(*),          INTENT(IN)       :: Item
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeArray_RemoveLast(Collection, Item, ItemCopy) RESULT(Success)
	        !^ To get and remove the last item of the collection.  Also, return a flag
            !  indicating whether the item is successfully removed.
            CLASS(DequeArray), INTENT(INOUT)    :: Collection   !! collection object
            !> the item to be removed from the collection
            CLASS(*),          INTENT(INOUT)    :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully retrieved and removed.
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeArray_PeekLast(Collection, Item, ItemCopy) RESULT(Success)
	        !^ To get the last item (without removing it from the collection). Also, return
            !  a flag indicating whether the item is successfully retrieved or not.
            CLASS(DequeArray), INTENT(IN)       :: Collection   !! collection object
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
    ! interfaces for ListArray
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type          -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListArray_ToString(Obj) RESULT(Str)
            !^ To get the string representation of the object. <br>
            CLASS(ListArray), INTENT(IN)    :: Obj
            tCharAlloc                      :: Str
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by ListArray Type            -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListArray_AddAt(Collection, Index, Item) RESULT(Success)
	        !^ To insert the specified item at the specified position in the collection.
            CLASS(ListArray), INTENT(INOUT) :: Collection   !! collection object
            !> index indicating the position in the collection to add the item
            tIndex,           INTENT(IN)    :: Index
            !> the item to be added to the collection
            CLASS(*),         INTENT(IN)    :: Item
            !> flag indicating whether the item is successfully added or not.
            tLogical                        :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListArray_RemoveAt(Collection, Index, Item, ItemCopy) RESULT(Success)
	        !^ To get and remove the item at the specified position.  Also, return a flag
            !  indicating whether the item is successfully removed.
            CLASS(ListArray), INTENT(INOUT)     :: Collection   !! collection object
            !> index indicating the position in the collection to retrieve and remove the item
            tIndex,           INTENT(IN)        :: Index
            !> the item to be removed from the collection
            CLASS(*),         INTENT(INOUT)     :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully retrieved and removed.
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListArray_PeekAt(Collection, Index, Item, ItemCopy) RESULT(Success)
	        !^ To retrieve the item at the specified position (without removing it from the
            !  collection). Also, return a flag indicating whether the item is successfully
            !  retrieved or not.
            CLASS(ListArray), INTENT(IN)        :: Collection   !! collection object
            !> index indicating the position in the collection to retrieve the item
            tIndex,           INTENT(IN)        :: Index
            !> the item to be retrieved from the collection
            CLASS(*),         INTENT(INOUT)     :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the item is successfully retrieved or not.
            tLogical                            :: Success
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE BaseDynArr_CreateEmpty(Collection, InitCap, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !! To create an empty collection.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr),  INTENT(INOUT)   :: Collection   !! BaseDynArr object
    tIndex,             INTENT(IN)      :: InitCap      !! initial size of the collection
    tIndex,   OPTIONAL, INTENT(IN)      :: IncSize      !! incremental size of the collection when it is full
    tLogical, OPTIONAL, INTENT(IN)      :: Shrink
    !^ flag to shrink the collection capacity <br>
    ! - true if want to reduce capacity when the size is less than a quarter of the capacity. <br>
    ! - otherwise, the capacity stays the same. <br>
    ! - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

! FLOW

    ! first, check required input data
    IF (InitCap < 1_kIndex) THEN
        CALL Handle_ErrLevel('BaseDynArr_CreateEmpty', ModName, ErrWarning, &
                             'Invalid InitCap (< 1).  Set the initial capacity to 16.')
        Capacity = Collection%IncSize
    ELSE
        Capacity = InitCap
    END IF

    ! then, allocate space for the items in the collection
    CALL MemAlloc(Collection%Items, Capacity)
    CALL Collection%ItemPool%Construct()
    
    ! finally, check optional input data
    Collection%IncSize = 0_kIndex       ! reset it to zero
    IF (PRESENT(IncSize)) THEN
        IF (IncSize > 0_kIndex) Collection%IncSize = IncSize
    END IF
    IF (PRESENT(Shrink)) Collection%Shrink  =  Shrink

    RETURN

END SUBROUTINE BaseDynArr_CreateEmpty

!******************************************************************************

SUBROUTINE BaseDynArr_CreateByArray(Collection, N, Items, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a collection from an array of items.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr),  INTENT(INOUT) :: Collection   !! BaseDynArr object
    tIndex,             INTENT(IN)    :: N            !! number of items
    CLASS(*),           INTENT(IN)    :: Items(:)     !! the items to be added to the collection
    tIndex,   OPTIONAL, INTENT(IN)    :: IncSize      !! incremental size of the collection when it is full
    tLogical, OPTIONAL, INTENT(IN)    :: Shrink
    !^ flag to shrink the collection capacity <br>
    ! - true if want to reduce capacity when the size is less than a quarter of the capacity. <br>
    ! - otherwise, the capacity stays the same. <br>
    ! - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, InitCap

! FLOW

    ! create empty collection
    InitCap = N*2_kIndex    ! by default, doubling its capacity
    IF (PRESENT(IncSize)) THEN
        IF (IncSize > 0_kIndex) InitCap = N + IncSize
    END IF
    CALL Collection%CreateEmpty(InitCap, IncSize, Shrink)

    ! add items to the collection
    CALL Collection%SetMold(Items(1))
    DO I = 1_kIndex, N
        CALL Collection%Insert(Items(I))
    END DO

    RETURN

END SUBROUTINE BaseDynArr_CreateByArray

!******************************************************************************

SUBROUTINE BaseDynArr_MemResize(Collection, NewSize)

!** PURPOSE OF THIS SUBROUTINE:
	!! To re-allocate the array of items of the collection and preserve its data

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr), INTENT(INOUT) :: Collection   !! BaseDynArr object
    tIndex,            INTENT(IN)    :: NewSize      !! new size of array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: Offset
    tIndex                      :: OldSize  ! original size of array
    tIndex                      :: PSize    ! size of preserved data
    TYPE(GenData), ALLOCATABLE  :: Temp(:)  ! temporary buffer

!** FLOW:

    ! determine the original size
    OldSize = SIZE(Collection%Items)

    ! first, allocate the temporary array
    CALL MemAlloc(Temp, NewSize)

    ! determine the preserving size
    IF (NewSize >= OldSize) THEN
        PSize = OldSize
    ELSE
        PSize = NewSize
    END IF

    ! get offset
    Offset = Collection%Offset()

    ! *** copy items to the temporary buffer ***
    IF (Offset == 1_kIndex) THEN
        ! use whole array expression (typical for a stack)
        Temp(1:PSize) = Collection%Items(1:PSize)
    ELSE
        ! use do loop (typical for a deque or a queue)
        BLOCK
            tIndex  :: I, J
            ! get offset to the first item
            J = Offset
            DO I = 1_kIndex, PSize
                ! copy an item to the buffer
                Temp(I) = Collection%Items(J)
                ! update J and wrap around if necessary
                J = J + 1_kIndex
                IF (J > OldSize) J = 1_kIndex
            END DO
        END BLOCK
    END IF

    ! move data from the temporary array back to the array
    ! (this operation includes deallocate the array, reallocate it to
    !  the new size and copy data back)
    CALL MOVE_ALLOC(Temp, Collection%Items)

    RETURN

END SUBROUTINE BaseDynArr_MemResize

!**************************************************************************************

SUBROUTINE BaseDynArr_Growing(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!! To increase the collection's capacity if needed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr), INTENT(INOUT)    :: Collection   !! BaseDynArr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

!** FLOW:

    IF (.NOT.ALLOCATED(Collection%Items)) THEN
        ! the collection has not yet been constructed.
        Capacity = 16_kIndex
        ! allocate storage for the collections' items
        CALL MemAlloc(Collection%Items, Capacity)
    ELSE
        Capacity = SIZE(Collection%Items)
        IF (Collection%GetSize() == Capacity) THEN
            ! increase the collection's capacity
            IF (Collection%IncSize > 0_kIndex) THEN
                Capacity = Capacity + Collection%IncSize
            ELSE
                Capacity = Capacity*2_kIndex
            END IF
            ! check integer overflow
            IF (Capacity <= 0_kIndex) Capacity = MaxCapacity
            ! resize the collections' items
            CALL Collection%Resize(Capacity)
        END IF
    END IF

    RETURN

END SUBROUTINE BaseDynArr_Growing

!**************************************************************************************

SUBROUTINE BaseDynArr_Shrinking(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!! To decrease the collection's capacity if needed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr), INTENT(INOUT)    :: Collection   !! BaseDynArr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CurCap, CurSize

!** FLOW:

    IF (.NOT.ALLOCATED(Collection%Items)) THEN
        ! the collection has not yet been constructed so simply return.
        RETURN
    END IF
    IF (Collection%Shrink) THEN
        CurCap  = SIZE(Collection%Items)
        CurSize = Collection%GetSize()
        IF ((CurSize >= 0_kIndex).AND.(CurSize <= CurCap/4_kIndex)) THEN
            ! halves the collection's capacity
            CurCap = CurCap/2_kIndex
            ! check if the capacity is zero or not
            IF (CurCap <= 0_kIndex) CurCap = 1_kIndex
            ! resize the collections' items
            CALL Collection%Resize(CurCap)
        END IF
    END IF

    RETURN

END SUBROUTINE BaseDynArr_Shrinking

!**************************************************************************************

SUBROUTINE BaseDynArr_BaseCopy(SrcObj, DstObj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy components from SrcObj to DstObj.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr), INTENT(IN)       :: SrcObj   !! a source
    CLASS(BaseDynArr), INTENT(INOUT)    :: DstObj   !! a destination

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: Cap, I
    CLASS(*), POINTER   :: MoldPtr

! FLOW

    DstObj%IncSize = SrcObj%IncSize
    DstObj%Shrink  = SrcObj%Shrink
    IF (ALLOCATED(SrcObj%Items)) THEN
        Cap = SIZE(SrcObj%Items)
        CALL MemAlloc(DstObj%Items, Cap)
        DO I = 1_kIndex, Cap
            CALL SrcObj%Items(I)%Copy(DstObj%Items(I))
        END DO
    END IF
    MoldPtr => SrcObj%GetItemPtr()
    IF (ASSOCIATED(MoldPtr)) CALL DstObj%SetMold(MoldPtr)

    RETURN

END SUBROUTINE BaseDynArr_BaseCopy

!******************************************************************************

SUBROUTINE BaseDynArr_BaseFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of components of the BaseDynArr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr), INTENT(INOUT) :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I

! FLOW

    IF (ALLOCATED(Obj%Items)) THEN
        DO I = 1_kIndex, SIZE(Obj%Items, KIND=kIndex)
            CALL Obj%Items(I)%MemFree()
        END DO
        CALL MemFree(Obj%Items)
    END IF
    CALL Obj%ItemPool%Destruct()
    CALL Obj%FreeMold()

    RETURN

END SUBROUTINE BaseDynArr_BaseFree

!******************************************************************************

SUBROUTINE BaseDynArr_BaseReset(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the BaseDynArr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr), INTENT(INOUT) :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I

! FLOW

    IF (ALLOCATED(Obj%Items)) THEN
        DO I = 1_kIndex, SIZE(Obj%Items, KIND=kIndex)
            CALL Obj%Items(I)%MemFree()
        END DO
    END IF
    CALL Obj%FreeMold()

    RETURN

END SUBROUTINE BaseDynArr_BaseReset

!******************************************************************************

FUNCTION BaseDynArr_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the base-string representation of the *BaseDynArr* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr), INTENT(IN)   :: Obj
    tCharAlloc                      :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (.NOT.Obj%IsEmpty()) THEN
        BLOCK
            TYPE(CharBuffer)    :: ChrBuf
            tIndex              :: I, Count
            tCharAlloc          :: ItemStr
            ! initialize
            CALL ChrBuf%CreateEmpty(InitCap=Obj%GetSize()*40_kIndex)
            CALL ChrBuf%Append('[')
            Count = 0_kIndex
            DO I = 1_kIndex, SIZE(Obj%Items, KIND=kIndex)
                ! skip if the item is empty
                IF (Obj%Items(I)%IsEmpty()) CYCLE
                ! add the string representation of the current item
                ItemStr = Obj%Items(I)%ToString()
                CALL ChrBuf%Append(ItemStr(12:LEN(ItemStr)-1))
                ! update Count and add comma between items if needed
                Count = Count + 1_kIndex
                IF (Count < Obj%GetSize()) THEN
                    CALL ChrBuf%Append(', ')
                ELSEIF (Count > Obj%GetSize()) THEN
                    EXIT
                END IF
            END DO
            ! add the closing character and get the base-string representation of this object
            CALL ChrBuf%Append(']')
            Str = ChrBuf%AsString()
        END BLOCK
    ELSE
        Str = '[NULL]'
    END IF

    RETURN

END FUNCTION BaseDynArr_ToString

!******************************************************************************

FUNCTION BaseDynArr_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the base hash code for the *BaseDynArr* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr), INTENT(IN)   :: Obj
    tIndex                          :: Code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (.NOT.Obj%IsEmpty()) THEN
        BLOCK
            tIndex      :: I, Count
            ! initialize
            Count = 0_kIndex
            Code = 0_kIndex
            DO I = 1_kIndex, SIZE(Obj%Items, KIND=kIndex)
                ! skip if the item is empty
                IF (Obj%Items(I)%IsEmpty()) CYCLE
                ! add the hash code of the current item
                Code = Code + Obj%Items(I)%HashCode()
                ! update Count and add comma between items if needed
                Count = Count + 1_kIndex
                IF (Count > Obj%GetSize()) EXIT
            END DO
        END BLOCK
    ELSE
        Code = 0_kIndex
    END IF

    RETURN

    RETURN

END FUNCTION BaseDynArr_HashCode

! ---------------------------------------------------------------------
! -----             Final Procedures                              -----
! ---------------------------------------------------------------------

SUBROUTINE DequeArray_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(DequeArray), INTENT(INOUT) :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE DequeArray_Finalize

!******************************************************************************
SUBROUTINE ListArray_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ListArray), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE ListArray_Finalize

!******************************************************************************

SUBROUTINE QueueArray_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(QueueArray), INTENT(INOUT) :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE QueueArray_Finalize

!******************************************************************************

SUBROUTINE StackArray_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(StackArray), INTENT(INOUT) :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE StackArray_Finalize

!******************************************************************************

END MODULE MClass_DynamicArrays

!******************************************************************************
