
MODULE MClass_DArrObject

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *DArrObject* type and related routines.
!   The *DArrObject* type is a container with *CLASS(Object)* as the type
!   of its stored items.  It employs a dynamic-array implementation where
!   items are stored as a resizable array.  All derived types extending from the
!   *Object* type can be used with this container. <br>
!   Unlike the <a href="../module/mclass_listassignable.html">ListObject</a> type,
!   the *DArrObject* type does not allow items with different types to be stored
!   in the same container.  The *DArrObject* type requires a user to specify the
!   type of items to be stored via the *Mold* argument when an empty container is
!   created.  Alternatively, the user may implicitly specify the type of items to be
!   stored via the *Items* argument when a container is constructed from an array
!   of items.  Also, it is important to note that the type of an item specified in
!   all other routines must be the same as the type of stored items of the container.
!   Otherwise, the container may not behave as expected. <br>
!   See the <a href="../module/mclass_dynamicarrays.html">MClass_DynamicArrays</a>
!   module for discussions about the *Dynamic-Array* concept and its strategy
!   used for growing and shrinking the array, which is similar to the strategy
!   employed by the *DArrObject* type. <br>
!   See the <a href="../module/mbase_dynamicarrays.html">MBase_DynamicArrays</a>
!   module for an overview and usage notes of a *dynamic-array-based* type.   A user
!   may use the *ModBase_DynamicArrays* module instead of using this module directly.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil,       ONLY: MAX_I32, MAX_I64
    USE MBase_ErrHandlers
    USE MBase_MemHandlers
    USE MClass_Object

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: DArrObject

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_DArrObject'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! maximum capacity of a dynamic-array-based container
#ifdef Indx64Bits
    tIndex,    PARAMETER    :: MaxCapacity = MAX_I64
#else
    tIndex,    PARAMETER    :: MaxCapacity = MAX_I32
#endif

!** DERIVED TYPE DEFINITIONS:
    !> *DArrObject* is a container type that employs a dynamic-array implementation
    !  to provide common operations for a list container.  It can also represent other
    !  forms of containers including a LIFO stack, a FIFO queue and a double-ended queue
    !  (deque).  The type of items stored in this container is a derived type in the
    !  *Object* class (i.e. the *Object* type or its subtypes).
    TYPE DArrObject
        PRIVATE
        !> incremental size of the container when the container is full.
        !  Its value will be reset to 0 if the optional input is NOT
        !  specified during construction
        tIndex      :: IncSize = 16
        !% flag to shrink the container capacity
        tLogical    :: Shrink = .FALSE.
        !% pointer to first item of the queue
        tIndex      :: First  = 1_kIndex
        !% pointer to next to last item of the queue (i.e. the next available slot)
        tIndex      :: Last   = 1_kIndex
        !% size of the container (number of items)
        tIndex      :: Size   = 0_kIndex
        !% pointer to current item of the iteration
        tIndex      :: Cursor = 0_kIndex
        !> direction of the iteration <br>
        !  - positive -> forward iteration
        !  - negative -> backward iteration (only applicable for deque and list)
        !  - zero     -> iteration not yet start
        tSInt32     :: Dir = 0
        !% items stored in the container.
        CLASS(Object), ALLOCATABLE   :: Items(:)
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----             PRIVATE PROCEDURES                            -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: Offset       => DynArr_GetFirst
        PROCEDURE, PRIVATE  :: Resize       => DynArr_MemResize
        PROCEDURE, PRIVATE  :: Growing      => DynArr_Growing
        PROCEDURE, PRIVATE  :: Shrinking    => DynArr_Shrinking
        PROCEDURE, PRIVATE  :: DynArr_CreateByArray
        ! ---------------------------------------------------------------------
        ! -----             PUBLIC PROCEDURES                             -----
        ! ---------------------------------------------------------------------
        ! -----             Constructor and Destructor Procedures         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty container. <br>
        !  **Usage**: <br>
        !   ! create an empty container with specified initial capacity and type of items to be stored <br>
        !   --->    CALL Container%CreateEmpty(25, Mold) <br>
        !   ! create a container and specify the optional incremental size <br>
        !   --->    CALL Container%CreateEmpty(25, Mold, IncSize=16) <br>
        !   ! create a container and specify the optional shrink flag <br>
        !   --->    CALL Container%CreateEmpty(25, Mold, Shrink=.TRUE.)
        PROCEDURE   :: CreateEmpty  => DynArr_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new container from an array of items. <br>
        !  **Usage**: <br>
        !           ! create a container from an array of 25 items <br>
        !   --->    CALL Container%Construct(25, Arr) <br>
        !           ! create a container and specify the optional incremental size <br>
        !   --->    CALL Container%Construct(25, Arr, IncSize=16) <br>
        !           ! create a container and specify the optional shrink flag <br>
        !   --->    CALL Container%Construct(25, Arr, Shrink=.TRUE.)
        GENERIC     :: Construct    => DynArr_CreateByArray
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the container and free memory
        !                of items stored in the container.<br>
        !  **Usage**: <br>
        !   --->    CALL Container%Destruct() <br>
        !  **Important Note**:  For the *DynArr* class, this method is not equivalent
        !   to the *Clear* method.  Therefore, after calling the *Destruct* method,
        !   the user should reconstruct the container (by calling a *Construction*
        !   method again) before using other operations once more.  Otherwise, the
        !   container's behavior may not be as expected.
        PROCEDURE   :: Destruct     => DynArr_Destroy
        ! ---------------------------------------------------------------------
        ! -----             Insertion and Removal Procedures              -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: AddFirst <br>
        !  **Purpose**:  To insert the specified item at the front of the container. <br>
        !  **Usage**: <br>
        !   --->    CALL Container%AddFirst(Item)
        PROCEDURE   :: AddFirst     => DynArr_AddFirst
        !> **Type-Bound Subroutine**: AddLast <br>
        !  **Purpose**:  To insert the specified item at the end of the container. <br>
        !  **Usage**: <br>
        !   --->    CALL Container%AddLast(Item)
        PROCEDURE   :: AddLast      => DynArr_AddLast
        !> **Type-Bound Function**: AddAt <br>
        !  **Purpose**:  To insert the specified item at the specified index where
        !                the index must be between 1 and the container size.
        !                Also, return a flag indicating whether the item is
        !                successfully added. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%AddAt(Index, Item) <br>
        !   --->    IF (.NOT.Container%AddAt(Index, Item)) DoSomething
        PROCEDURE   :: AddAt      => DynArr_AddAt
        !> **Type-Bound Function**: RemoveFirst <br>
        !  **Purpose**:  To get and remove the front (first) item of the container.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%RemoveFirst(Item) <br>
        !   --->    IF (.NOT.Container%RemoveFirst(Item)) DoSomething
        PROCEDURE   :: RemoveFirst  => DynArr_RemoveFirst
        !> **Type-Bound Function**: RemoveLast <br>
        !  **Purpose**:  To get and remove the last item of the container.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%RemoveLast(Item) <br>
        !   --->    IF (.NOT.Container%RemoveLast(Item)) DoSomething
        PROCEDURE   :: RemoveLast   => DynArr_RemoveLast
        !> **Type-Bound Function**: RemoveAt <br>
        !  **Purpose**:  To get and remove the item at the specified index where
        !                the index must be between 1 and the container size.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%RemoveAt(Index, Item) <br>
        !   --->    IF (.NOT.Container%RemoveAt(Index, Item)) DoSomething
        PROCEDURE   :: RemoveAt   => DynArr_RemoveAt
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete an item from the container. <br>
        !  **Usage**: <br>
        !   --->    CALL Container%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with
        !   the *StartFirst* and *MoveForward* methods.  Therefore, after
        !   the call to one of those methods and then calling this one
        !   will result in a removal of the current item of the iteration
        !   (i.e. the same item that can be retrieved via the *StartFirst*
        !   and *MoveForward* methods).
        PROCEDURE   :: Delete       => DynArr_Delete
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the container. <br>
        !  **Usage**: <br>
        !   --->    CALL Container%Clear()
        PROCEDURE   :: Clear        => DynArr_ClearItems
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the container.  Also, return
        !                a flag indicating whether the items are successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%ToArray(Items) <br>
        !   --->    IF (.NOT.Container%ToArray(Items)) DoSomething
        PROCEDURE   :: ToArray      => DynArr_ToArray
        ! ---------------------------------------------------------------------
        ! -----                 Iteration Procedures                      -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag indicating
        !                whether the container is empty or not. <br>
        !  **Usage**: see *MoveForward* procedure.
        PROCEDURE   :: StartFirst   => DynArr_Move2FirstElm
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next iteration and return a flag indicating whether
        !                the cursor pointer has reached the end of the container or not. <br>
        !  **Usage**: <br>
        !   The following code snippet illustrates how to typically traverse the container.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start forward iteration (from the first item)
        !   IsEmpty = Container%StartFirst()
        !   IF (.NOT.IsEmpty) DoSomeThing...
        !   DO
        !       ! move to the next iteration
        !       IsTheEnd = Container%MoveForward()
        !       ! check whether we reach the end of the container or not
        !       IF (IsTheEnd) EXIT
        !       ! if not, do the task we need
        !       DoSomeThing...
        !   END DO
        !   </Code></Pre> <br>
        !   The following code snippet shows another way to iterate over the container.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start forward iteration (from the first item)
        !   IsTheEnd = Container%StartFirst(CurrItem)
        !   DO WHILE (.NOT.IsTheEnd)
        !       DoSomeThing_With_CurrItem...
        !       ! move to the next iteration
        !       IsTheEnd = Container%MoveForward(CurrItem)
        !   END DO
        !   </Code></Pre>
        PROCEDURE   :: MoveForward  => DynArr_Move2NextElm
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start the *backward* iteration and return a flag indicating
        !                whether the container is empty or not. <br>
        !  **Usage**: see *MoveBackward* procedure.
        PROCEDURE   :: StartLast    => DynArr_Move2LastElm
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move to the next iteration (in reverse order) and return
        !                a flag indicating whether the cursor pointer has reached the
        !                end of the container or not. <br>
        !  **Usage**: <br>
        !   The following code snippet illustrates how to typically traverse the container in reverse order.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start backward iteration (from the last item)
        !   IsEmpty = Container%StartLast()
        !   IF (.NOT.IsEmpty) DoSomeThing...
        !   DO
        !       ! move to the next iteration
        !       IsTheEnd = Container%MoveBackward()
        !       ! check whether we reach the end of the container or not
        !       IF (IsTheEnd) EXIT
        !       ! if not, do the task we need
        !       DoSomeThing...
        !   END DO
        !   </Code></Pre> <br>
        !   The following code snippet shows another way to iterate over the container in reverse order.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start backward iteration (from the last item)
        !   IsTheEnd = Container%StartLast(CurrItem)
        !   DO WHILE (.NOT.IsTheEnd)
        !       DoSomeThing_With_CurrItem...
        !       ! move to the next iteration
        !       IsTheEnd = Container%MoveBackward(CurrItem)
        !   END DO
        !   </Code></Pre>
        PROCEDURE   :: MoveBackward => DynArr_Move2PrevElm
        ! ---------------------------------------------------------------------
        ! -----                 Inquiry Procedures                       ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the container. <br>
        !  **Usage**: <br>
        !   --->    Size = Container%GetSize()
        PROCEDURE   :: GetSize      => DynArr_GetSize
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the container is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Container%IsEmpty() <br>
        !   --->    IF (.NOT.Container%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty      => DynArr_IsEmpty
        ! ---------------------------------------------------------------------
        ! -----                 Retrieval Procedures                     ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: PeekFirst <br>
        !  **Purpose**:  To get the front (first) item (without removing it from the container).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%PeekFirst(Item) <br>
        !   --->    IF (.NOT.Container%PeekFirst(Item)) DoSomething
        PROCEDURE   :: PeekFirst    => DynArr_PeekFirst
        !> **Type-Bound Function**: PeekLast <br>
        !  **Purpose**:  To get the last item (without removing it from the container).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%PeekLast(Item) <br>
        !   --->    IF (.NOT.Container%PeekLast(Item)) DoSomething
        PROCEDURE   :: PeekLast     => DynArr_PeekLast
        !> **Type-Bound Function**: PeekAt <br>
        !  **Purpose**:  To get the item (without removing it from the container) at
        !                the specified index where the index must be between 1 and the
        !                container size.  Also, return a flag indicating whether the
        !                item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%PeekAt(Index, Item) <br>
        !   --->    IF (.NOT.Container%PeekAt(Index, Item)) DoSomething
        PROCEDURE   :: PeekAt     => DynArr_PeekAt
        !> *GetAll* is a procedure deferred by the *BaseIterable* type. <br>
        !  **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the container.
        !                Also, return a flag indicating whether the items are available. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%GetAll(Items) <br>
        !   --->    IF (.NOT.Container%GetAll(Items)) DoSomething
        PROCEDURE   :: GetAll       => DynArr_GetAll
        ! ---------------------------------------------------------------------
        ! -----                 Queue Procedures                         ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: EnQueue <br>
        ! **Purpose**:  To add a new item to the end of the queue. <br>
        !  **Usage**: <br>
        !   --->    CALL Queue%EnQueue(NewItem) <br>
        !  **Note**: *EnQueue* is an alias of *AddLast*.
        PROCEDURE   :: EnQueue      => DynArr_AddLast
        !> **Type-Bound Function**: DeQueue <br>
        !  **Purpose**:  To get and remove the front (first) item of the queue.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Queue%DeQueue(Item) <br>
        !   --->    IF (.NOT.Queue%DeQueue(Item)) DoSomething <br>
        !  **Note**: *DeQueue* is an alias of *RemoveFirst*.
        PROCEDURE   :: DeQueue      => DynArr_RemoveFirst
        ! ---------------------------------------------------------------------
        ! -----                 Stack Procedures                         ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Push <br>
        ! **Purpose**:  To add a new item to the top of the stack. <br>
        !  **Usage**: <br>
        !   --->    CALL Stack%Push(NewItem) <br>
        !  **Note**: *Push* is an alias of *AddLast*.
        PROCEDURE   :: Push         => DynArr_AddLast
        !> **Type-Bound Function**: Pop <br>
        !  **Purpose**:  To get and remove the top item of the stack.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Stack%Pop(Item) <br>
        !   --->    IF (.NOT.Stack%Pop(Item)) DoSomething <br>
        !  **Note**: *Pop* is an alias of *RemoveLast*.
        PROCEDURE   :: Pop          => DynArr_RemoveLast
        !> **Type-Bound Function**: PeekTop <br>
        !  **Purpose**:  To get the last item (without removing it from the container).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%PeekTop(Item) <br>
        !   --->    IF (.NOT.Container%PeekTop(Item)) DoSomething <br>
        !  **Note**: *PeekTop* is an alias of *PeekLast*.
        PROCEDURE   :: PeekTop      => DynArr_PeekLast
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the container.
        FINAL       :: DynArr_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE DArrObject

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):


! ---------------------------------------------------------------------
! -----             PRIVATE PROCEDURES                            -----
! ---------------------------------------------------------------------

SUBROUTINE DynArr_MemResize(Container, NewSize)

!** PURPOSE OF THIS SUBROUTINE:
	!! To re-allocate the array of items of the container and preserve its data

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DArrObject), INTENT(INOUT)    :: Container    !! DynArr object
    tIndex,                INTENT(IN)       :: NewSize      !! new size of array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Offset
    tIndex      :: OldSize  ! original size of array
    tIndex      :: PSize    ! size of preserved data
    CLASS(Object), ALLOCATABLE   :: Temp(:)  ! temporary buffer

!** FLOW:

    ! determine the original size and length of character string
    OldSize = SIZE(Container%Items)
    ! first, allocate the temporary array
    ALLOCATE(Temp(NewSize), MOLD=Container%Items(1))

    ! determine the preserving size
    IF (NewSize >= OldSize) THEN
        PSize = OldSize
    ELSE
        PSize = NewSize
    END IF

    ! get offset
    Offset = Container%Offset()

    ! *** copy items to the temporary buffer ***
    IF (Offset == 1_kIndex) THEN
        ! use whole array expression (typical for a stack)
        Temp(1:PSize) = Container%Items(1:PSize)
    ELSE
        ! use do loop (typical for a deque or a queue)
        BLOCK
            tIndex  :: I, J
            ! get offset to the first item
            J = Offset
            DO I = 1_kIndex, PSize
                ! copy an item to the buffer
                Temp(I) = Container%Items(J)
                ! update J and wrap around if necessary
                J = J + 1_kIndex
                IF (J > OldSize) J = 1_kIndex
            END DO
        END BLOCK
    END IF

    ! move data from the temporary array back to the array
    ! (this operation includes deallocate the array, reallocate it to
    !  the new size and copy data back)
    CALL MOVE_ALLOC(Temp, Container%Items)

    RETURN

END SUBROUTINE DynArr_MemResize

!**************************************************************************************

SUBROUTINE DynArr_Growing(Container)

!** PURPOSE OF THIS SUBROUTINE:
	!! To increase the container's capacity if needed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DArrObject), INTENT(INOUT)    :: Container    !! DynArr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

!** FLOW:

    IF (.NOT.ALLOCATED(Container%Items)) THEN
        ! the container has not yet been constructed so report error
        CALL Handle_ErrLevel('DynArr_Growing', ModName, ErrSevere, &
                             'The container has not yet been constructed.')
    ELSE
        Capacity = SIZE(Container%Items)
        IF (Container%GetSize() == Capacity) THEN
            ! increase the container's capacity
            IF (Container%IncSize > 0) THEN
                Capacity = Capacity + Container%IncSize
            ELSE
                Capacity = Capacity*2
            END IF
            ! check integer overflow
            IF (Capacity <= 0) Capacity = MaxCapacity
            ! resize the containers' items
            CALL Container%Resize(Capacity)
        END IF
    END IF

    RETURN

END SUBROUTINE DynArr_Growing

!**************************************************************************************

SUBROUTINE DynArr_Shrinking(Container)

!** PURPOSE OF THIS SUBROUTINE:
	!! To decrease the container's capacity if needed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DArrObject), INTENT(INOUT)    :: Container    !! DynArr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CurCap, CurSize

!** FLOW:

    IF (.NOT.ALLOCATED(Container%Items)) THEN
        ! the container has not yet been constructed so simply return.
        RETURN
    END IF
    IF (Container%Shrink) THEN
        CurCap  = SIZE(Container%Items)
        CurSize = Container%GetSize()
        IF ((CurSize >= 0).AND.(CurSize <= CurCap/4)) THEN
            ! halves the container's capacity
            CurCap = CurCap/2
            ! check if the capacity is zero or not
            IF (CurCap <= 0) CurCap = 1
            ! resize the containers' items
            CALL Container%Resize(CurCap)
        END IF
    END IF

    RETURN

END SUBROUTINE DynArr_Shrinking

!**************************************************************************************

FUNCTION DynArr_GetFirst(Container) RESULT(First)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get an index pointing to the first item.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DArrObject), INTENT(INOUT)    :: Container
    tIndex                                  :: First

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set index to the first item
    First = Container%First

    ! reset index pointers of the container
    Container%First = 1_kIndex
    Container%Last  = Container%Size + 1_kIndex

    RETURN

END FUNCTION DynArr_GetFirst

!******************************************************************************

FUNCTION IsIndexValid(Size, Index) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To check whether the specified index is between 1 and the container size or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: Size     !! the container size
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

!**************************************************************************************

FUNCTION ComputeTrueIndex(Container, Index) RESULT(ID)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To compute the actual index of the container's items based on the specified index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(IN)   :: Container
    !% the specified index
    tIndex,                INTENT(IN)   :: Index
    !% the actual index of the container's items
    tIndex                              :: ID

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ID = Index + Container%First - 1_kIndex
    IF (Container%First >= Container%Last) THEN
        BLOCK
            tIndex  :: Capacity
            Capacity = SIZE(Container%Items, KIND=kIndex)
            IF (ID > Capacity) THEN
                ID = ID - Capacity
            END IF
        END BLOCK
    END IF

    RETURN

END FUNCTION ComputeTrueIndex

!**************************************************************************************

SUBROUTINE MoveItemsRight(Container, Index)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move the container's items to the right of the specified index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(INOUT)    :: Container
    !% the one-based index into the container's items
    tIndex,                INTENT(IN)       :: Index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I, Cap

! FLOW

    ! initialize local variables
    I = Container%Last - 1_kIndex
    Cap = SIZE(Container%Items, KIND=kIndex)

    ! move items as necessary
    DO
        IF (I < Index) EXIT
        IF (I == 0_kIndex) THEN
            Container%Items(1_kIndex) = Container%Items(Cap)
            I = Cap - 1_kIndex
        ELSE
            Container%Items(I+1_kIndex) = Container%Items(I)
            I = I - 1_kIndex
        END IF
    END DO

    RETURN

END SUBROUTINE MoveItemsRight

!**************************************************************************************

SUBROUTINE MoveItemsLeft(Container, Index)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move the container's items to the left of the specified index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(INOUT)    :: Container
    !% the one-based index into the container's items
    tIndex,                INTENT(IN)       :: Index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I, Cap, Last

! FLOW

    ! initialize local variables
    I    = Index
    Cap  = SIZE(Container%Items, KIND=kIndex)
    Last = Container%Last - 1_kIndex
    IF (Last == 0_kIndex) Last = Cap

    ! move items as necessary
    DO
        IF (I == Last) EXIT
        IF (I == Cap) THEN
            Container%Items(Cap) = Container%Items(1_kIndex)
            I = 1_kIndex
        ELSE
            Container%Items(I) = Container%Items(I+1_kIndex)
            I = I + 1_kIndex
        END IF
    END DO

    RETURN

END SUBROUTINE MoveItemsLeft

! ---------------------------------------------------------------------
! -----             PUBLIC PROCEDURES                             -----
! ---------------------------------------------------------------------
! -----             Constructor and Destructor Procedures         -----
! ---------------------------------------------------------------------

SUBROUTINE DynArr_CreateEmpty(Container, InitCap, Mold, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !! To create an empty container.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DArrObject), INTENT(INOUT)    :: Container    !! DynArr object
    tIndex,                INTENT(IN)       :: InitCap      !! initial size of the container
    CLASS(Object),     INTENT(IN)       :: Mold         !! mold providing the type of items to be stored
    tIndex,   OPTIONAL,    INTENT(IN)       :: IncSize      !! incremental size of the container when it is full
    tLogical, OPTIONAL,    INTENT(IN)       :: Shrink
    !^ flag to shrink the container capacity <br>
    ! - true if want to reduce capacity when the size is less than a quarter of the capacity. <br>
    ! - otherwise, the capacity stays the same. <br>
    ! - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

! FLOW

    ! first, check required input data
    IF (InitCap < 1) THEN
        CALL Handle_ErrLevel('DynArr_CreateEmpty', ModName, ErrWarning, &
                             'Invalid InitCap (< 1).  Set the initial capacity to 16.')
        Capacity = Container%IncSize
    ELSE
        Capacity = InitCap
    END IF

    ! then, allocate space for the items in the container
    ALLOCATE(Container%Items(Capacity), MOLD=Mold)

    ! finally, check optional input data
    Container%IncSize = 0
    IF (PRESENT(IncSize)) THEN
        IF (IncSize > 0) Container%IncSize = IncSize
    END IF
    IF (PRESENT(Shrink)) Container%Shrink  =  Shrink

    RETURN

END SUBROUTINE DynArr_CreateEmpty

!******************************************************************************

SUBROUTINE DynArr_CreateByArray(Container, N, Items, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a container from an array of items.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DArrObject), INTENT(INOUT)    :: Container    !! DynArr object
    tIndex,                INTENT(IN)       :: N            !! number of items
    CLASS(Object),     INTENT(IN)       :: Items(:)     !! the items to be added to the container
    tIndex,   OPTIONAL,    INTENT(IN)       :: IncSize      !! incremental size of the container when it is full
    tLogical, OPTIONAL,    INTENT(IN)       :: Shrink
    !^ flag to shrink the container capacity <br>
    ! - true if want to reduce capacity when the size is less than a quarter of the capacity. <br>
    ! - otherwise, the capacity stays the same. <br>
    ! - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, InitCap

! FLOW

    ! create empty stack
    InitCap = N*2   ! by default, doubling its capacity
    IF (PRESENT(IncSize)) THEN
        IF (IncSize > 0) InitCap = N + IncSize
    END IF
    CALL Container%CreateEmpty(InitCap, Items(1), IncSize, Shrink)

    ! add items to the container
    DO I = 1, N
        CALL Container%AddLast(Items(I))
    END DO

    RETURN

END SUBROUTINE DynArr_CreateByArray

!******************************************************************************

SUBROUTINE DynArr_Destroy(Container)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the container.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(INOUT)    :: Container

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I

!** FLOW:

    ! remove all items
    CALL Container%Clear()

    ! free storage memory
    DO I = 1_kIndex, SIZE(Container%Items, KIND=kIndex)
        CALL Container%Items(I)%MemFree()
    END DO
    DEALLOCATE(Container%Items)

    RETURN

END SUBROUTINE DynArr_Destroy

! ---------------------------------------------------------------------
! -----             Insertion and Removal Procedures              -----
! ---------------------------------------------------------------------

SUBROUTINE DynArr_AddLast(Container, Item)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To insert the specified item at the end of the container.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(INOUT)    :: Container
    !% the item to be added to the container
    CLASS(Object),     INTENT(IN)       :: Item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (.NOT.SAME_TYPE_AS(Item, Container%Items(1))) THEN
        ! report error
        CALL Handle_ErrLevel('DynArr_AddLast', ModName, ErrSevere, &
                             'Type of input item is not valid.')
        RETURN
    END IF

    ! first, grow the container capacity if necessary
    CALL Container%Growing()

    ! then, add new item to the container
    Container%Items(Container%Last) = Item

    ! next, update pointer (wrap around if necessary) and size
    Container%Last = Container%Last + 1_kIndex
    IF (Container%Last > SIZE(Container%Items, KIND=kIndex)) Container%Last = 1_kIndex
    Container%Size = Container%Size + 1_kIndex

    RETURN

END SUBROUTINE DynArr_AddLast

!**************************************************************************************

SUBROUTINE DynArr_AddFirst(Container, Item)

!** PURPOSE OF THIS SUBROUTINE:
	!! To insert the specified item at the front of the container.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(INOUT)    :: Container
    !% the item to be added to the container
    CLASS(Object),     INTENT(IN)       :: Item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (.NOT.SAME_TYPE_AS(Item, Container%Items(1))) THEN
        ! report error
        CALL Handle_ErrLevel('DynArr_AddFirst', ModName, ErrSevere, &
                             'Type of input item is not valid.')
        RETURN
    END IF

    ! first, grow the container capacity if necessary
    CALL Container%Growing()

    ! then, update pointer (wrap around if necessary) and size
    Container%First = Container%First - 1_kIndex
    IF (Container%First == 0_kIndex) Container%First = SIZE(Container%Items, KIND=kIndex)
    Container%Size = Container%Size + 1_kIndex

    ! finally, add new item to the container
    Container%Items(Container%First) = Item

    RETURN

END SUBROUTINE DynArr_AddFirst

!**************************************************************************************

FUNCTION DynArr_AddAt(Container, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To insert the specified item at the specified index where the index must be
    !  between 1 and the container size.  Also, return a flag indicating whether
    !  the item is successfully added.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(INOUT)    :: Container
    !% the one-based index into the container's items
    tIndex,                INTENT(IN)       :: Index
    !% the item to be added to the container
    CLASS(Object),     INTENT(IN)       :: Item
    !> flag indicating whether the item is successfully added. <br>
    ! - true if the item is successfully added.
    ! - false if the item is NOT successfully added.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: ID

! FLOW

    IF (.NOT.SAME_TYPE_AS(Item, Container%Items(1))) THEN
        ! report error
        CALL Handle_ErrLevel('DynArr_AddAt', ModName, ErrSevere, &
                             'Type of input item is not valid.')
        RETURN
    END IF

    ! check the validity of the specified index
    Flag = FalseVal
    IF (.NOT.IsIndexValid(Container%Size, Index)) RETURN

    ! first, grow the container capacity if necessary
    CALL Container%Growing()

    ! compute the actual index of the container's items
    ID = ComputeTrueIndex(Container, Index)

    ! move the container's items to the right of ID
    CALL MoveItemsRight(Container, ID)

    ! then, add new item to the container
    Container%Items(ID) = Item

    ! next, update pointer (wrap around if necessary) and size
    Container%Last = Container%Last + 1_kIndex
    IF (Container%Last > SIZE(Container%Items, KIND=kIndex)) Container%Last = 1_kIndex
    Container%Size = Container%Size + 1_kIndex

    ! set output flag
    Flag = TrueVal

    RETURN

END FUNCTION DynArr_AddAt

!**************************************************************************************

FUNCTION DynArr_RemoveLast(Container, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the last item of the container.  Also, return
    ! a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(INOUT)    :: Container
    !% the item to be removed from the container
    CLASS(Object),     INTENT(INOUT)    :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the container is NOT empty.
    ! - false if the container is empty.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Last

! FLOW

    ! check whether the container is empty or not
    IF (Container%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        IF (.NOT.SAME_TYPE_AS(Item, Container%Items(1))) THEN
            Flag = FalseVal
            ! report error
            CALL Handle_ErrLevel('DynArr_RemoveLast', ModName, ErrSevere, &
                                 'Type of output item is not valid.')
            RETURN
        END IF
        Flag = TrueVal
    END IF

    ! set index pointer to the last item (wrap around if necessary)
    Last = Container%Last - 1_kIndex
    IF (Last == 0_kIndex) Last = SIZE(Container%Items, KIND=kIndex)

    ! get the last item
    Item = Container%Items(Last)

    ! update the last pointer (wrap around if necessary) and size
    Container%Last = Container%Last - 1_kIndex
    IF (Container%Last == 0_kIndex) Container%Last = SIZE(Container%Items, KIND=kIndex)
    Container%Size = Container%Size - 1_kIndex

    ! shrink the container if necessary
    CALL Container%Shrinking()

    RETURN

END FUNCTION DynArr_RemoveLast

!**************************************************************************************

FUNCTION DynArr_RemoveFirst(Container, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the first item of the container.  Also, return
    ! a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(INOUT)    :: Container
    !% the item to be removed from the container
    CLASS(Object),     INTENT(INOUT)    :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the container is NOT empty.
    ! - false if the container is empty.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether the container is empty or not
    IF (Container%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        IF (.NOT.SAME_TYPE_AS(Item, Container%Items(1))) THEN
            Flag = FalseVal
            ! report error
            CALL Handle_ErrLevel('DynArr_RemoveFirst', ModName, ErrSevere, &
                              'Type of output item is not valid.')
            RETURN
        END IF
        Flag = TrueVal
    END IF

    ! get the first item
    Item = Container%Items(Container%First)

    ! update the first pointer (wrap around if necessary) and size
    Container%First = Container%First + 1_kIndex
    IF (Container%First > SIZE(Container%Items, KIND=kIndex)) Container%First = 1_kIndex
    Container%Size = Container%Size - 1_kIndex

    ! shrink the container if necessary
    CALL Container%Shrinking()

    RETURN

END FUNCTION DynArr_RemoveFirst

!**************************************************************************************

FUNCTION DynArr_RemoveAt(Container, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the item at the specified index where the index must be
    !  between 1 and the container size.  Also, return a flag indicating whether
    !  the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(INOUT)    :: Container
    !% the one-based index into the container's items
    tIndex,                INTENT(IN)       :: Index
    !% the item to be removed from the container
    CLASS(Object),     INTENT(INOUT)    :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the item is successfully removed.
    ! - false if the item is NOT successfully removed.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: ID

! FLOW

    ! check whether the item is available or not
    Flag = FalseVal
    IF (Container%IsEmpty()) RETURN
    IF (.NOT.IsIndexValid(Container%Size, Index)) RETURN

    IF (.NOT.SAME_TYPE_AS(Item, Container%Items(1))) THEN
        ! report error
        CALL Handle_ErrLevel('DynArr_RemoveAt', ModName, ErrSevere, &
                             'Type of output item is not valid.')
        RETURN
    END IF

    ! compute the actual index of the container's items
    ID = ComputeTrueIndex(Container, Index)

    ! get the item
    Item = Container%Items(ID)

    ! move the container's items to the left of ID
    CALL MoveItemsLeft(Container, ID)

    ! update pointer (wrap around if necessary) and size
    Container%Last = Container%Last - 1_kIndex
    IF (Container%Last == 0_kIndex) Container%Last = SIZE(Container%Items, KIND=kIndex)
    Container%Size = Container%Size - 1_kIndex

    ! shrink the container if necessary
    CALL Container%Shrinking()

    ! set output flag
    Flag = TrueVal

    RETURN

END FUNCTION DynArr_RemoveAt

!**************************************************************************************

SUBROUTINE DynArr_ClearItems(Container)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the items from the container.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(INOUT)    :: Container

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! reset pointer indices
    Container%First  = 1_kIndex
    Container%Last   = 1_kIndex
    Container%Cursor = 0_kIndex
    Container%Size   = 0_kIndex
    Container%Dir    = 0

    ! shrink the container
    CALL Container%Shrinking()

    RETURN

END SUBROUTINE DynArr_ClearItems

!**************************************************************************************

SUBROUTINE DynArr_Delete(Container)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete an item from a container.  This procedure is intended
    !  to be used in conjunction with the *StartFirst* and *MoveForward*
    !  methods.  Therefore, after the call to one of the methods and then
    !  calling this procedure will result in a removal of the current item
    !  of the iteration (i.e. the same item that can be retrieved via those
    !  methods).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(INOUT)    :: Container

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! return immediately if the container is empty
    IF (Container%IsEmpty()) RETURN

    ! return immediately if the cursor is not in a valid position
    IF (Container%Cursor == 0_kIndex) RETURN
    IF (Container%First < Container%Last) THEN
        IF ((Container%Cursor < Container%First).OR. &
            (Container%Cursor >= Container%Last)) RETURN
    ELSE
        IF ((Container%Cursor < Container%First).AND. &
            (Container%Cursor >= Container%Last)) RETURN
    END IF

    ! the cursor is in a valid position
    Delete_Block: BLOCK
        ! block variables
        tIndex  :: I, Cap, Last
        ! initialize block variables
        I = Container%Cursor
        Cap = SIZE(Container%Items, KIND=kIndex)
        Last = Container%Last - 1_kIndex
        IF (Last == 0_kIndex) Last = Cap
        ! move items as necessary
        DO
            IF (I == Last) EXIT
            IF (I == Cap) THEN
                Container%Items(Cap) = Container%Items(1_kIndex)
                I = 1_kIndex
            ELSE
                Container%Items(I) = Container%Items(I+1_kIndex)
                I = I + 1_kIndex
            END IF
        END DO
        ! update the last pointer and size
        Container%Last = Last
        Container%Size = Container%Size - 1_kIndex
        ! shrink the container if necessary
        CALL Container%Shrinking()
        ! reset cursor
        Cap = SIZE(Container%Items, KIND=kIndex)
        ! reset cursor
        IF (Container%Dir == 1) THEN
            ! forward iteration so move cursor backward
            Container%Cursor = Container%Cursor - 1_kIndex
            IF (Container%Cursor == 0_kIndex) Container%Cursor = Cap
        ELSE
            ! backward iteration so move cursor forward
            Container%Cursor = Container%Cursor + 1_kIndex
            IF (Container%Cursor == Cap) Container%Cursor = 0_kIndex
        END IF
    END BLOCK Delete_Block

    RETURN

END SUBROUTINE DynArr_Delete

!**************************************************************************************

FUNCTION DynArr_ToArray(Container, Items) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove all items from the container.  Also, return
    !  a flag indicating whether the items are successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject),          INTENT(INOUT)   :: Container
    !% the item to be removed from the container
    CLASS(Object), ALLOCATABLE, INTENT(OUT)     :: Items(:)
    !> flag indicating whether the items are successfully removed. <br>
    ! - true if the items are successfully removed.
    ! - false if the items are NOT successfully removed.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! retrieve all items
    Flag = Container%GetAll(Items)

    ! remove all items
    CALL Container%Clear()

END FUNCTION DynArr_ToArray

! ---------------------------------------------------------------------
! -----                 Iteration Procedures                      -----
! ---------------------------------------------------------------------

FUNCTION DynArr_Move2FirstElm(Container, Item) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the front (first) element in the container.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject),       INTENT(INOUT)  :: Container
    !% the first element as output if requested (and available)
    CLASS(Object), OPTIONAL, INTENT(INOUT)  :: Item
    !> a flag indicating whether the container contains no element or not <br>
    ! - true if the container is empty. <br>
    ! - otherwise the first element is available.
    tLogical                                    :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set cursor pointer
    Container%Cursor = Container%First

    ! set return flag
    IsEmpty = (Container%Size == 0_kIndex)

    ! set direction
    IF (.NOT.IsEmpty) Container%Dir = 1

    IF ((.NOT.IsEmpty).AND.PRESENT(Item)) THEN
        ! check whether the types of items are the same or not
        IF (SAME_TYPE_AS(Item, Container%Items(Container%Cursor))) THEN
            ! get item
            Item = Container%Items(Container%Cursor)
        ELSE
            ! report error
            CALL Handle_ErrLevel('DynArr_Move2FirstElm', ModName, ErrWarning, &
                                 'Type of output item is not valid.')
        END IF
    END IF

    RETURN

END FUNCTION DynArr_Move2FirstElm

!**************************************************************************************

FUNCTION DynArr_Move2NextElm(Container, Item) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move (backward) to the next element in the container.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject),       INTENT(INOUT)  :: Container
    !% the next element as output if requested (and available)
    CLASS(Object), OPTIONAL, INTENT(INOUT)  :: Item
    !> a flag indicating whether the move to the end of the container occurs or not <br>
    ! - true if next element is NOT available. <br>
    ! - otherwise next element is available.
    tLogical                                    :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! simply return if empty
    IF (Container%IsEmpty()) THEN
        IsTheEnd = TrueVal
        RETURN
    END IF

    ! set cursor pointer and wrap around if necessary
    Container%Cursor = Container%Cursor + 1_kIndex
    IF (Container%Cursor > SIZE(Container%Items, KIND=kIndex)) Container%Cursor = 1_kIndex

    ! set return flag
    IsTheEnd = (Container%Cursor == Container%Last)

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        Container%Dir = 1
    ELSE
        Container%Dir = 0
    END IF

    IF ((.NOT.IsTheEnd).AND.PRESENT(Item)) THEN
        ! check whether the types of items are the same or not
        IF (SAME_TYPE_AS(Item, Container%Items(Container%Cursor))) THEN
            ! get item
            Item = Container%Items(Container%Cursor)
        ELSE
            ! report error
            CALL Handle_ErrLevel('DynArr_Move2NextElm', ModName, ErrWarning, &
                                 'Type of output item is not valid.')
        END IF
    END IF

    RETURN

END FUNCTION DynArr_Move2NextElm

!**************************************************************************************

FUNCTION DynArr_Move2LastElm(Container, Item) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the last element in the container. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject),       INTENT(INOUT)  :: Container
    !% the last element as output if requested (and available)
    CLASS(Object), OPTIONAL, INTENT(INOUT)  :: Item
    !> a flag indicating whether the container contains no element or not <br>
    ! - true if the container is empty. <br>
    ! - otherwise the last element is available.
    tLogical                                    :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set return flag
    IsEmpty = (Container%Size == 0_kIndex)

    ! simply return if empty
    IF (IsEmpty) RETURN

    ! set cursor pointer (note: Container%Last points to the next to last item)
    ! and wrap around if necessary
    Container%Cursor = Container%Last - 1_kIndex
    IF (Container%Cursor == 0_kIndex) Container%Cursor = SIZE(Container%Items, KIND=kIndex)

    ! set direction
    IF (.NOT.IsEmpty) Container%Dir = -1

    IF ((.NOT.IsEmpty).AND.PRESENT(Item)) THEN
        ! check whether the types of items are the same or not
        IF (SAME_TYPE_AS(Item, Container%Items(Container%Cursor))) THEN
            ! get item
            Item = Container%Items(Container%Cursor)
        ELSE
            ! report error
            CALL Handle_ErrLevel('DynArr_Move2LastElm', ModName, ErrWarning, &
                                 'Type of output item is not valid.')
        END IF
    END IF

    RETURN

END FUNCTION DynArr_Move2LastElm

!**************************************************************************************

FUNCTION DynArr_Move2PrevElm(Container, Item) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the previous element in the container. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject),       INTENT(INOUT)  :: Container
    !% the previous element as output if requested (and available)
    CLASS(Object), OPTIONAL, INTENT(INOUT)  :: Item
    !> a flag indicating whether the move to the end of the container occurs or not <br>
    ! - true if previous element is NOT available. <br>
    ! - otherwise previous element is available.
    tLogical                                    :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! simply return if empty
    IF (Container%IsEmpty()) THEN
        IsTheEnd = TrueVal
        RETURN
    END IF

    ! set return flag
    IsTheEnd = (Container%Cursor == Container%First)

    ! set cursor pointer and wrap around if necessary
    Container%Cursor = Container%Cursor - 1_kIndex
    IF (Container%Cursor == 0_kIndex) Container%Cursor = SIZE(Container%Items, KIND=kIndex)

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        Container%Dir = -1
    ELSE
        Container%Dir = 0
    END IF

    IF ((.NOT.IsTheEnd).AND.PRESENT(Item)) THEN
        ! check whether the types of items are the same or not
        IF (SAME_TYPE_AS(Item, Container%Items(Container%Cursor))) THEN
            ! get item
            Item = Container%Items(Container%Cursor)
        ELSE
            ! report error
            CALL Handle_ErrLevel('DynArr_Move2PrevElm', ModName, ErrWarning, &
                                 'Type of output item is not valid.')
        END IF
    END IF

    RETURN

END FUNCTION DynArr_Move2PrevElm

! ---------------------------------------------------------------------
! -----                 Inquiry Procedures                       ------
! ---------------------------------------------------------------------

FUNCTION DynArr_IsEmpty(Container) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether the container is currently empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DArrObject), INTENT(IN)   :: Container    !! DynArr object
    tLogical                            :: Flag         !! true if the container currently contains no item.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = (Container%GetSize() == 0)

    RETURN

END FUNCTION DynArr_IsEmpty

!******************************************************************************

FUNCTION DynArr_GetSize(Container) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of items in the container.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DArrObject), INTENT(IN)   :: Container
    tIndex                              :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Container%Size

    RETURN

END FUNCTION DynArr_GetSize

! ---------------------------------------------------------------------
! -----                 Retrieval Procedures                     ------
! ---------------------------------------------------------------------

FUNCTION DynArr_PeekFirst(Container, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get the front (first) item (without removing it from the container).
    !  Also, return a flag indicating whether the item is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(IN)       :: Container
    !% the item to be retrieved from the container
    CLASS(Object),     INTENT(INOUT)    :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the container is NOT empty.
    ! - false if the container is empty.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether the container is empty or not
    IF (Container%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF

    ! check whether the types of items are the same or not
    IF (SAME_TYPE_AS(Item, Container%Items(Container%First))) THEN
        ! get the top item
        Item = Container%Items(Container%First)
    ELSE
        ! report error
        CALL Handle_ErrLevel('DynArr_PeekFirst', ModName, ErrWarning, &
                             'Type of output item is not valid.')
    END IF

    RETURN

END FUNCTION DynArr_PeekFirst

!**************************************************************************************

FUNCTION DynArr_PeekLast(Container, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get the last item (without removing it from the container).
    !  Also, return a flag indicating whether the item is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(IN)       :: Container
    !% the item to be retrieved from the container
    CLASS(Object),     INTENT(INOUT)    :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the container is NOT empty.
    ! - false if the container is empty.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Last

! FLOW

    ! check whether the container is empty or not
    IF (Container%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF

    ! set index pointer to the last item (wrap around if necessary)
    Last = Container%Last - 1_kIndex
    IF (Last == 0_kIndex) Last = SIZE(Container%Items, KIND=kIndex)

    ! check whether the types of items are the same or not
    IF (SAME_TYPE_AS(Item, Container%Items(Last))) THEN
        ! get the last item
        Item = Container%Items(Last)
    ELSE
        ! report error
        CALL Handle_ErrLevel('DynArr_PeekFirst', ModName, ErrWarning, &
                             'Type of output item is not valid.')
    END IF

    RETURN

END FUNCTION DynArr_PeekLast

!**************************************************************************************

FUNCTION DynArr_PeekAt(Container, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get the item (without removing it from the container) at the specified index
    !  where the index must be between 1 and the container size.  Also, return a flag
    !  indicating whether the item is available or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject), INTENT(IN)       :: Container
    !% the one-based index into the container's items
    tIndex,                INTENT(IN)       :: Index
    !% the item to be retrieved from the container
    CLASS(Object),     INTENT(INOUT)    :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the item is available.
    ! - false if the item is NOT available.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: ID

! FLOW

    ! check whether the item is available or not
    Flag = FalseVal
    IF (Container%IsEmpty()) RETURN
    IF (.NOT.IsIndexValid(Container%Size, Index)) RETURN

    ! compute the actual index of the container's items
    ID = ComputeTrueIndex(Container, Index)

    ! check whether the types of items are the same or not
    IF (SAME_TYPE_AS(Item, Container%Items(ID))) THEN
        ! get the item
        Item = Container%Items(ID)
    ELSE
        ! report error
        CALL Handle_ErrLevel('DynArr_PeekFirst', ModName, ErrWarning, &
                             'Type of output item is not valid.')
    END IF

    ! set output flag
    Flag = TrueVal

    RETURN

END FUNCTION DynArr_PeekAt

!**************************************************************************************

FUNCTION DynArr_GetAll(Container, Items) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all items (without removing them) from the container.  Also,
    !  return a flag indicating whether the items are available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DynArr object
    CLASS(DArrObject),          INTENT(INOUT)   :: Container
    !% the item to be removed from the container
    CLASS(Object), ALLOCATABLE, INTENT(OUT)     :: Items(:)
    !> flag indicating whether the items are successfully removed. <br>
    ! - true if the items are available.
    ! - false if the items are NOT available.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, N

! FLOW

    ! check whether the container is empty or not
    IF (Container%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF

    ! get items from the container
    N = Container%Size

    ! allocate storage for output
    ALLOCATE(Items(N), MOLD=Container%Items(1))

    ! get items from the container
    J = Container%First
    DO I = 1, N
        Items(I) = Container%Items(J)
        J = J + 1_kIndex
        IF (J > SIZE(Container%Items, KIND=kIndex)) J = 1_kIndex
    END DO

    RETURN

END FUNCTION DynArr_GetAll

! ---------------------------------------------------------------------
! -----             Final Procedure                               -----
! ---------------------------------------------------------------------

SUBROUTINE DynArr_Finalizer(Container)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(DArrObject), INTENT(INOUT) :: Container    !! DynArr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! remove all items and free up memory
    CALL Container%Destruct()

    RETURN

END SUBROUTINE DynArr_Finalizer

!******************************************************************************

END MODULE MClass_DArrObject

!******************************************************************************
