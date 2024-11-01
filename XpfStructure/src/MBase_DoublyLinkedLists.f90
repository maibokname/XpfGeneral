
MODULE MBase_DoublyLinkedLists

!^  **PURPOSE OF THIS MODULE**: <br>
!   This module collectively contains *doubly-linked-list-based* types from other modules.
!   It is provided so that a user can refer to this module instead of referring to several
!   individual modules when various types are needed concurrently. <br>
!   <br>
!   **Overview**: <br>
!   These doubly-linked-list-based types are container types that employ doubly-linked-list
!   implementation.  Each individual type can be used to store items for a specific type
!   where the type of stored items is one of Fortran intrinsic types or a derived type in
!   the *Object* class (i.e. the *Object* type or its subtypes). <br>
!   Available container types are: <br>
!   - the <a href="../module/mclass_listcharacter.html#type-listcharacter">ListCharacter</a> type
!     for character string type, <br>
!   - the <a href="../module/mclass_listcmpxsp.html#type-listcmpxsp">ListCmpxSP</a> type
!     for single-precision complex type, <br>
!   - the <a href="../module/mclass_listcmpxdp.html#type-listcmpxdp">ListCmpxDP</a> type
!     for double-precision complex type, <br>
!   - the <a href="../module/mclass_listcmpxqp.html#type-listcmpxqp">ListCmpxQP</a> type
!     for quadruple-precision complex type, <br>
!   - the <a href="../module/mclass_listinteger1b.html#type-listinteger1b">ListInteger1B</a> type
!     for 1-byte (or 8-bit) integer type, <br>
!   - the <a href="../module/mclass_listinteger2b.html#type-listinteger2b">ListInteger2B</a> type
!     for 2-byte (or 16-bit) integer type, <br>
!   - the <a href="../module/mclass_listinteger4b.html#type-listinteger4b">ListInteger4B</a> type
!     for 4-byte (or 32-bit) integer type, <br>
!   - the <a href="../module/mclass_listinteger8b.html#type-listinteger8b">ListInteger8B</a> type
!     for 8-byte (or 64-bit) integer type, <br>
!   - the <a href="../module/mclass_listlogical.html#type-listlogical">ListLogical</a> type
!     for default logical type, <br>
!   - the <a href="../module/mclass_listrealsp.html#type-listrealsp">ListRealSP</a> type
!     for single-precision real type, <br>
!   - the <a href="../module/mclass_listrealdp.html#type-listrealdp">ListRealDP</a> type
!     for double-precision real type, <br>
!   - the <a href="../module/mclass_listrealqp.html#type-listrealqp">ListRealQP</a> type
!     for quadruple-precision real type, and <br>
!   - the <a href="../module/mclass_listobject.html#type-listobject">ListObject</a> type
!     for a derived type in the *Object* class, and <br>
!   - the <a href="../module/mclass_listanytype.html#type-listanytype">ListAnyType</a> type
!     for any data type. <br>
!   Each doubly-linked-list-based type can represent various forms of containers including: <br>
!   - the last-in-first-out (LIFO) stack container, <br>
!   - the first-in-first-out (FIFO) queue container, <br>
!   - the double-ended queue (deque) container, and <br>
!   - the list container where an item can be added, removed or retrieved
!     at the (valid) specified index. <br>
!   Each individual type provides various common operations that can be
!   categorized as follows. <br>
!   (1) Construction and Destruction.  Methods for these operations include: <br>
!   - *Construct* method - method to construct the container from
!     an array of items, and <br>
!   - *Destruct* method - method to destruct the container by removing
!     all items from the container. <br>
!   (2) Insertion and Removal.  Methods for these operations include: <br>
!   - *AddFirst* method - method to insert an item to the front of the container, <br>
!   - *AddLast* method - method to insert an item to the end of the container, <br>
!   - *AddAt* method - method to insert an item at the specified index, <br>
!   - *RemoveFirst* method - method to get and remove the first item of the container, <br>
!   - *RemoveLast* method - method to get and remove the last item of the container, <br>
!   - *RemoveAt* method - method to get and remove an item at the specified index, <br>
!   - *Remove* method - method to remove (and optionally get) either the last or
!     the first item, <br>
!   - *Delete* method - method to remove an item at the current iteration while
!     performing an iteration over the container, <br>
!   - *Clear* method - method to remove all items from the container. <br>
!   - *ToArray* method - method to retrieve and then remove all items from the container. <br>
!   - *RemoveDuplicates* method - method to remove duplicated items from the container, <br>
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
!   The *doubly-linked-list-based* types provided in this module can be considered
!   to be the same as those *dynamic-array-based* types provided in the
!   <a href="../module/mbase_dynamicarrays.html">MBase_DynamicArrays</a> module
!   although they employ different implementation.  However, the two groups of
!   containers have some subtle differences as discussed below. <br>
!   - Unlike the *dynamic-array-based* types, all *doubly-linked-list-based* types
!     commonly do not require an explicit construction.  Items can be added via the
!     *Construction* method (for an array of items) or they can be added by using
!     one of the insertion methods provided.  Therefore, the *CreateEmpty* method used
!     to construct an empty container is deemed unnecessary and thus NOT provided.  <br>
!   - For *doubly-linked-list-based* types, the *Clear* method (which is used to remove
!     all items from the container) is equivalent to the *Destruct* method (also used
!     to remove all items from the container.  On the other hand, the two methods for
!     the *dynamic-array-based* types are not equivalent because the *Destruct* method
!     not only removes all items from the container but also performs deallocation of
!     the *Items* component used to store items whereas the *Clear* method only removes
!     all items from the container. <br>
!   Besides methods just discussed above, all other operations provided by both groups
!   of containers can be considered to be the same.

!** USE STATEMENTS:
    USE MClass_ListCharacter
    USE MClass_ListCmpxSP
    USE MClass_ListCmpxDP
    USE MClass_ListCmpxQP
    USE MClass_ListInteger1B
    USE MClass_ListInteger2B
    USE MClass_ListInteger4B
    USE MClass_ListInteger8B
    USE MClass_ListLogical
    USE MClass_ListRealSP
    USE MClass_ListRealDP
    USE MClass_ListRealQP
    USE MClass_ListObject
    USE MClass_ListAnyType

END MODULE MBase_DoublyLinkedLists
