
MODULE MBase_DynamicArrays

!^  **PURPOSE OF THIS MODULE**: <br>
!   This module collectively contains *dynamic-array-based* types from other modules.
!   It is provided so that a user can refer to this module instead of referring to several
!   individual modules when various types are needed concurrently. <br>
!   <br>
!   **Overview**: <br>
!   These dynamic-array-based types are container types that employ dynamic-array (or
!   resizable-array) implementation.  Each individual type can be used to stored items
!   for a specific type where the type of stored items is one of Fortran intrinsic types
!   or a derived type in the *Object* class (i.e. the *Object* type or its subtypes). <br>
!   Available container types are: <br>
!   - the <a href="../module/mclass_darrcharacter.html#type-darrcharacter">DArrCharacter</a> type
!     for character string type, <br>
!   - the <a href="../module/mclass_darrcmpxsp.html#type-darrcmpxsp">DArrCmpxSP</a> type
!     for single-precision complex type, <br>
!   - the <a href="../module/mclass_darrcmpxdp.html#type-darrcmpxdp">DArrCmpxDP</a> type
!     for double-precision complex type, <br>
!   - the <a href="../module/mclass_darrcmpxqp.html#type-darrcmpxqp">DArrCmpxQP</a> type
!     for quadruple-precision complex type, <br>
!   - the <a href="../module/mclass_darrinteger1b.html#type-darrinteger1b">DArrInteger1B</a> type
!     for 1-byte (or 8-bit) integer type, <br>
!   - the <a href="../module/mclass_darrinteger2b.html#type-darrinteger2b">DArrInteger2B</a> type
!     for 2-byte (or 16-bit) integer type, <br>
!   - the <a href="../module/mclass_darrinteger4b.html#type-darrinteger4b">DArrInteger4B</a> type
!     for 4-byte (or 32-bit) integer type, <br>
!   - the <a href="../module/mclass_darrinteger8b.html#type-darrinteger8b">DArrInteger8B</a> type
!     for 8-byte (or 64-bit) integer type, <br>
!   - the <a href="../module/mclass_darrlogical.html#type-darrlogical">DArrLogical</a> type
!     for default logical type, <br>
!   - the <a href="../module/mclass_darrrealsp.html#type-darrrealsp">DArrRealSP</a> type
!     for single-precision real type, <br>
!   - the <a href="../module/mclass_darrrealdp.html#type-darrrealdp">DArrRealDP</a> type
!     for double-precision real type, <br>
!   - the <a href="../module/mclass_darrrealqp.html#type-darrrealqp">DArrRealQP</a> type
!     for quadruple-precision real type, and <br>
!   - the <a href="../module/mclass_darrobject.html#type-darrobject">DArrObject</a> type
!     for a derived type in the *Object* class. <br>
!   Each dynamic-array-based type can represent various forms of containers including: <br>
!   - the last-in-first-out (LIFO) stack container, <br>
!   - the first-in-first-out (FIFO) queue container, <br>
!   - the double-ended queue (deque) container, and <br>
!   - the list container where an item can be added, removed or retrieved
!     at the (valid) specified index. <br>
!   Each individual type provides various common operations that can be
!   categorized as follows. <br>
!   (1) Construction and Destruction.  Methods for these operations include: <br>
!   - *CreateEmpty* method - method to construct an empty container, <br>
!   - *Construct* method - method to construct the container from
!     an array of items, and <br>
!   - *Destruct* method - method to destruct the container by removing all
!     items from the container as well as freeing memory of its component used
!     to store items. <br>
!   (2) Insertion and Removal.  Methods for these operations include: <br>
!   - *AddFirst* method - method to insert an item to the front of the container, <br>
!   - *AddLast* method - method to insert an item to the end of the container, <br>
!   - *AddAt* method - method to insert an item at the specified index, <br>
!   - *RemoveFirst* method - method to get and remove the first item of the container, <br>
!   - *RemoveLast* method - method to get and remove the last item of the container, <br>
!   - *RemoveAt* method - method to get and remove an item at the specified index, <br>
!   - *Delete* method - method to remove an item at the current iteration while
!     performing an iteration over the container, <br>
!   - *Clear* method - method to remove all items from the container. <br>
!   - *ToArray* method - method to retrieve and then remove all items from the container. <br>
!   - *Enqueue* method - same as the *AddLast* method provided when used as a queue, <br>
!   - *Dequeue* method - same as the *RemoveFirst* method provided when used as a queue, <br>
!   - *Push* method - same as the *AddLast* method provided when used as a stack, and <br>
!   - *Pop* method - same as the *RemoveLast* method provided when used as a stack. <br>
!   (3) Retrieval.  Methods for this operation include: <br>
!   - *PeekFirst* method - method to retrieve the first item of the container, <br>
!   - *PeekLast* method - method to retrieve the last item of the container, <br>
!   - *PeekAt* method - method to retrieve the item at the specified index, <br>
!   - *PeekTop* method - same as the *PeekLast* method provided when used as a stack, and <br>
!   - *GetAll* method - method to retrieve all items from the container. <br>
!   (4) Inquiry.  Methods for this operation include: <br>
!   - *IsEmpty* method - method to check whether the container is empty or not, and <br>
!   - *GetSize* method - method to get the container size (number of items stored). <br>
!   (5) Iteration.  Methods for this operation include: <br>
!   - *StartFirst* method - method to start a forward iteration over items, <br>
!   - *MoveForward* method - method to move forward to the next item, <br>
!   - *StartLast* method - method to start a backward iteration over items, and <br>
!   - *MoveBackward* method - method to move backward to the next item. <br>
!   <br>
!   **Usage Notes**:  <br>
!   The *dynamic-array-based* types provided in this module can be considered
!   to be the same as those *doubly-linked-list-based* types provided in the
!   <a href="../module/mbase_doublylinkedlists.html">MBase_DoublyLinkedLists</a> module
!   although they employ different implementation.  However, the two groups of
!   containers have some subtle differences as discussed below. <br>
!   - Unlike the *doubly-linked-list-based* types, all *dynamic-array-based* types
!     commonly require an explicit construction before using other provided operations.
!     As shown above, there are two methods provided to create a container.  First,
!     the *CreateEmpty* method constructs an empty container with the specified initial
!     capacity.  Second, the *Construction* method constructs a container from an array
!     of items.  <br>
!   - Also, unlike *doubly-linked-list-based* types where the *Clear* method and the
!     *Destruct* method are considered to be equivalent, the *Destruct* method of a
!     *dynamic-array-based* type frees memory of its component used to store items
!     in addition to removing all items from the container (which is the operation
!     of the *Clear* method).  Therefore, after calling the *Destruct* method, a user
!     must reconstruct the container (by calling either the *Construction* or the
!     *CreateEmpty* method again) before using other operations once more.  Otherwise,
!     the container's behavior may not be as expected (or the program may even crash). <br>
!   Besides methods just discussed above, all other operations provided by both groups
!   of containers can be considered to be the same. <br>
!   See the <a href="../module/mclass_dynamicarrays.html">MClass_DynamicArrays</a>
!   module for discussions about the *Dynamic-Array* concept and its strategy used
!   for growing and shrinking a resizable array, which is similar to the strategy
!   employed by all *dynamic-array-based* types provided in this module.  It should
!   be noted that all *dynamic-array-based* types provided in the *MClass_DynamicArrays*
!   module are functionally similar to those provided in this module.  However, each
!   individual type provided in this module can only be used for a specific type of
!   items to be stored whereas each individual type in the *MClass_DynamicArrays*
!   is a generic container that can be used for various types of items to be stored
!   providing that the size (in bytes) of the data item to be stored is known at
!   compile time.

!** USE STATEMENTS:
    USE MClass_DArrCharacter
    USE MClass_DArrCmpxSP
    USE MClass_DArrCmpxDP
    USE MClass_DArrCmpxQP
    USE MClass_DArrInteger1B
    USE MClass_DArrInteger2B
    USE MClass_DArrInteger4B
    USE MClass_DArrInteger8B
    USE MClass_DArrLogical
    USE MClass_DArrRealSP
    USE MClass_DArrRealDP
    USE MClass_DArrRealQP
    USE MClass_DArrObject

END MODULE MBase_DynamicArrays
