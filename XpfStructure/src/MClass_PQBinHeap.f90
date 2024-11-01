
MODULE MClass_PQBinHeap

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *PQBinHeap* type and its related routines.  The *PQBinHeap* type
!   is a collection type that employs the binary heap implementation to provide common operations
!   for a priority queue.  The *PQBinHeap* type can be used as either a max-priority queue or a
!   min-priority queue.  By default, it represents the max-priority queue.  However, a user may
!   specify the optional *MinPQ* argument to true when constructing the collection so that the
!   *PQBinHeap* type represents the min-priority queue instead. <br>
!   The *PQBinHeap* type employs the *KeyOrdered* type to store comparable keys where allowed
!   key types include the *CHARACTER*, *INTEGER* and *REAL* intrinsic types as well as any
!   derived type that is in the *Comparable* class.  Like other collection types, it must be
!   employed to store keys of only one particular data type.  To store keys of another data
!   type, it must be destructed before inserting keys of different data type. <br>
!   The *PQBinHeap* type is NOT an *ordered* collection type because its stored keys are only
!   partially sorted in the so-called heap order, which also depends on the type of the
!   priority queue it represents (i.e. max-priority or min-priority queue).  Therefore, the
!   *PQBinHeap* type makes no guarantees as to the iteration order of its keys.  In particular,
!   it does not guarantee that the order will remain the same over time. <br>

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MClass_CharBuffer
    USE MBase_ByteUtil,     ONLY: AnyType_GetByteSize
    USE MBase_SIntUtil,     ONLY: MAX_I32, MAX_I64, ToChar => ToDecStrSigned
#ifdef Indx32Bits
    USE MBase_SimpleHash32, ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64, ONLY: ComputeHash => Hash64_FNV1a
#endif
    USE MClass_Object,      ONLY: Object
    USE MClass_Comparable,  ONLY: Comparable
    USE MClass_GenData,     ONLY: IfacePolyCopy
    USE MClass_KeyOrdered
    USE MClass_CompNodePool
    USE MClass_MemoryPool
    USE MClass_BaseCollection
    USE MClass_BaseIterable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: PQBinHeap

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'MClass_PQBinHeap'
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
    !> The *PQBinHeap* type is a collection type that employs a binary heap implementation
    !  to provide common operations for a priority queue.
    TYPE, EXTENDS(BaseCollection)   :: PQBinHeap
        PRIVATE
        !> pointer to the last item of the priority queue
        tIndex                          :: Last = 0_kIndex
        !> flag indicating whether the priority queue is implemented as a maximum or a minimum PQ
        tLogical                        :: Min = FalseVal
        !> incremental size of the collection when the collection is full.
        !  Its value will be reset to 0 if the optional input is NOT
        !  specified during construction
        tIndex                          :: IncSize = 16_kIndex
        !> flag to shrink priority queue capacity
        tLogical                        :: Shrink = FalseVal
        !> stored keys in the priority queue.
        TYPE(KeyOrdered), ALLOCATABLE   :: Keys(:)
        !> memory pool of stored items
        TYPE(MemoryPool)                :: KeyPool
        !> key mold providing the type of stored keys
        CLASS(*),         ALLOCATABLE   :: Mold
        !> pointer to current item of the iteration
        tIndex                          :: Cursor = 0_kIndex
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                      Private Procedures                   -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: Growing      => PQBinHeap_Growing
        PROCEDURE, PRIVATE  :: Shrinking    => PQBinHeap_Shrinking
        PROCEDURE, PRIVATE  :: SinkDown     => PQBinHeap_ReHeapify_TopDown
        PROCEDURE, PRIVATE  :: SwimUp       => PQBinHeap_ReHeapify_BottomUp
        PROCEDURE, PRIVATE  :: IsKeyValid   => PQBinHeap_IsKeyValid
        PROCEDURE, PRIVATE  :: SwapKeys     => PQBinHeap_SwapKeys
        PROCEDURE, PRIVATE  :: PQBinHeap_ConstructorByArray
        ! ---------------------------------------------------------------------
        ! -----  Deferred/Overridden Procedures from BaseCollection Type  -----
        ! ---------------------------------------------------------------------
        !> Use the *Construct* method to construct the collection from another collection.
        PROCEDURE   :: CopyCollection   => PQBinHeap_CopyCollection
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        PROCEDURE   :: Clear            => PQBinHeap_Clear
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        !  **Note**:  This method is NOT equivalent to the *Clear* method. <br>
        PROCEDURE   :: Destruct         => PQBinHeap_Destruct
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize          => PQBinHeap_GetSize
        ! ---------------------------------------------------------------------
        ! -----                      Specific Procedures                  -----
        ! ---------------------------------------------------------------------
        ! -----     constructor and destructor procedures   -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty priority queue. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%CreateEmpty(InitCap)                         ! use default options  <br>
        !   --->    CALL Table%CreateEmpty(32, Mold=KeyMold)                ! specify key mold <br>
        !   --->    CALL Table%CreateEmpty(32, MinPQ=.TRUE.)                ! use min-priority queue <br>
        !   --->    CALL Table%CreateEmpty(32, IncSize=16)                  ! specify incremental size <br>
        !   --->    CALL Table%CreateEmpty(32, Shrink=.TRUE.)               ! specify shrinking <br>
        !   --->    CALL Table%CreateEmpty(32, KeyMold, .TRUE., 16, .TRUE.) ! specify all options <br>
        PROCEDURE   :: CreateEmpty  => PQBinHeap_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a priority queue from the specified key arrays. <br>
        !  **Usage**: <br>
        !   ! use default options  <br>
        !   --->    CALL Collection%Construct(40, KeyArr) <br>
        !   ! specify all options (initial capacity is array size plus incremental size) <br>
        !   --->    CALL Collection%Construct(20, KeyArr, MinPQ, IncSize, Shrink) <br>
        GENERIC     :: Construct    => PQBinHeap_ConstructorByArray
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key to the priority queue. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Insert(Key) <br>
        PROCEDURE   :: Insert       => PQBinHeap_InsertKey
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To retrieve and remove the highest-priority key from the priority queue.  Also,
        !       return a flag indicating whether the key is successfully removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Remove(Key) <br>
        !   --->    IF (.NOT.Collection%Remove(Key)) DoSomething
        PROCEDURE   :: Remove       => PQBinHeap_RemoveKey
        !> **Type-Bound Function**: Peek <br>
        !  **Purpose**:  To retrieve the highest-priority key from the priority queue.  Also, return
        !       a flag indicating whether the key is successfully removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Peek(Key) <br>
        !   --->    IF (.NOT.Collection%Peek(Key)) DoSomething
        PROCEDURE   :: Peek         => PQBinHeap_PeekKey
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the collection.  Also, return a flag
        !                indicating whether the items are successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Items) <br>
        !   --->    IF (.NOT.Collection%ToArray(Items)) DoSomething
        PROCEDURE   :: ToArray      => PQBinHeap_ToArray
        !> **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the collection. Also, return
        !                a flag indicating whether the items are available. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Items) <br>
        !   --->    IF (.NOT.Collection%GetAll(Items)) DoSomething
        PROCEDURE   :: GetAll       => PQBinHeap_GetAll
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag indicating
        !                whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartFirst() <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstKey)
        PROCEDURE   :: StartFirst   => PQBinHeap_Move2FirstKey
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next iteration and return a flag indicating whether
        !                the cursor pointer has reached the end of the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey) <br>
        !  **Note**: See the <a href="../module/mclass_baseiterable.html">BaseIterable</a> type
        !            for illustrations of an iteration over a collection using the *StartFirst*
        !            and *MoveForward* methods.<br>
        PROCEDURE   :: MoveForward  => PQBinHeap_Move2NextKey
        ! ---------------------------------------------------------------------
        ! -----     Deferred/Overridden Procedures from Object Type       -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => PQBinHeap_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => PQBinHeap_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => PQBinHeap_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => PQBinHeap_ToString
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => PQBinHeap_HashCode
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: PQBinHeap_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE PQBinHeap

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! ---------------------------------------------------------------------
! -----     Deferred/Overridden Procedures from Object Type       -----
! ---------------------------------------------------------------------

SUBROUTINE PQBinHeap_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap),   INTENT(IN)  :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
    !^ Flag indicating whether to perform deep copy or shallow copy. <br>
    !  - If present and true, perform a deep copy. <br>
    !  - If present and false, perform a shallow copy. <br>
    !  - If not present, perform either a shallow or a deep copy that is naturally most
    !    suitable for the object's components.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: Cap, I

!** FLOW:

    SELECT TYPE (DstObj)
    TYPE IS (PQBinHeap)
        DstObj%Last = SrcObj%Last
        DstObj%Min = SrcObj%Min
        DstObj%IncSize = SrcObj%IncSize
        DstObj%Shrink = SrcObj%Shrink
        IF (ALLOCATED(SrcObj%Keys)) THEN
            Cap = SIZE(SrcObj%Keys, KIND=kIndex)
            CALL MemAlloc(DstObj%Keys, Cap)
            DO I = 1_kIndex, Cap
                CALL SrcObj%Keys(I)%Copy(DstObj%Keys(I))
            END DO
        END IF
        CALL SrcObj%KeyPool%CloneTo(DstObj%KeyPool)
        IF (ALLOCATED(SrcObj%Mold)) ALLOCATE(DstObj%Mold, MOLD=SrcObj%Mold)
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('PQBinHeap_Copy', ModName, ErrSevere, &
                             'Type of the DstObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE PQBinHeap_Copy

!******************************************************************************

FUNCTION PQBinHeap_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(IN)    :: LhsObj   !! an object
    CLASS(Object),    INTENT(IN)    :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    TYPE IS (PQBinHeap)
        Flag = FalseVal
        IF (LhsObj%GetSize() /= RhsObj%GetSize()) RETURN
        IF (.NOT.LhsObj%IsEmpty()) THEN
            BLOCK
                tIndex  :: I
                DO I = 1, LhsObj%Last
                    IF (.NOT.LhsObj%Keys(I)%IsEqualTo(RhsObj%Keys(I))) RETURN
                END DO
            END BLOCK
        END IF
        Flag = TrueVal
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION PQBinHeap_IsEqualTo

!******************************************************************************

SUBROUTINE PQBinHeap_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the PQBinHeap object.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(INOUT) :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! reset components
    Obj%Last    = 0_kIndex
    Obj%IncSize = 16_kIndex
    Obj%Shrink  = FalseVal
    Obj%Min     = FalseVal
    Obj%Cursor  = 0_kIndex

    ! free memory
    IF (ALLOCATED(Obj%Keys)) THEN
        DO I = 1_kIndex, SIZE(Obj%Keys, KIND=kIndex)
            CALL Obj%Keys(I)%MemFree()
        END DO
        CALL MemFree(Obj%Keys)
    END IF
    IF (ALLOCATED(Obj%Mold)) DEALLOCATE(Obj%Mold)
    CALL Obj%KeyPool%Destruct()

    RETURN

END SUBROUTINE PQBinHeap_MemFree

!******************************************************************************

FUNCTION PQBinHeap_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the PQBinHeap type.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(IN)    :: Obj
    tCharAlloc                      :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: BaseStr

! FLOW

    ! get base string
    IF (Obj%IsEmpty()) THEN
        BaseStr = '[NULL]'
    ELSE
        BLOCK
            TYPE(CharBuffer)    :: ChrBuf
            tIndex              :: I, Count
            tCharAlloc          :: KeyStr
            ! initialize
            CALL ChrBuf%CreateEmpty(InitCap=Obj%GetSize()*40_kIndex)
            CALL ChrBuf%Append('[')
            Count = 0_kIndex
            DO I = 1_kIndex, SIZE(Obj%Keys, KIND=kIndex)
                ! skip if the item is empty
                IF (Obj%Keys(I)%IsEmpty()) CYCLE
                ! add the string representation of the current item
                KeyStr = Obj%Keys(I)%ToString()
                CALL ChrBuf%Append(KeyStr(15:LEN(KeyStr)-1))
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
    END IF
    Str = '{PQBinHeap with ' // ToChar(Obj%GetSize()) // ' items : ' // BaseStr // '}'

    RETURN

END FUNCTION PQBinHeap_ToString

!******************************************************************************

FUNCTION PQBinHeap_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(IN)    :: Obj
    tIndex                          :: Code

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
            tIndex  :: I
            Code = 0_kIndex
            DO I = 1_kIndex, SIZE(Obj%Keys)
                IF (Obj%Keys(I)%IsEmpty()) CYCLE
                Code = Code + Obj%Keys(I)%HashCode()
            END DO
        END BLOCK
    END IF

    RETURN

END FUNCTION PQBinHeap_HashCode

! ---------------------------------------------------------------------
! -----  Deferred/Overridden Procedures from BaseCollection Type  -----
! ---------------------------------------------------------------------

SUBROUTINE PQBinHeap_CopyCollection(This, Other, ItemCopy, ValCopy)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items as the given collection (Other).
    !  This is a deferred procedure by the *BaseCollection* class. <br>
    !  *Note*:  Other must be in the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(PQBinHeap),      INTENT(INOUT)    :: This
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
    CLASS IS (PQBinHeap)
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
            IF (IsKeyOrdered(MoldPtr)) THEN
                ALLOCATE(Item, MOLD=MoldPtr)
                ! loop through the other collection and get items along the way
                IsTheEnd = Other%StartFirst(Item, ItemCopy)
                DO WHILE (.NOT.IsTheEnd)
                    ! add an item to this collection
                    CALL This%Insert(Item)
                    IsTheEnd = Other%MoveForward(Item, ItemCopy)
                END DO
                DEALLOCATE(Item)
            ELSE
                CALL Handle_ErrLevel('PQBinHeap_CopyCollection', ModName, ErrSevere, &
                                     'Item type of "Other" is NOT a valid key type.')
            END IF
            NULLIFY(MoldPtr)
        END BLOCK
    CLASS DEFAULT
        CALL Handle_ErrLevel('PQBinHeap_CopyCollection', ModName, ErrSevere, &
                             'Type of "Other" must be in the "BaseIterable" class.')
    END SELECT
    
    RETURN

END SUBROUTINE PQBinHeap_CopyCollection

!******************************************************************************

SUBROUTINE PQBinHeap_CreateEmpty(Collection, InitCap, Mold, MinPQ, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create an empty priority queue.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap),   INTENT(OUT) :: Collection   !! collection
    tIndex,             INTENT(IN)  :: InitCap      !! initial size of priority queue
    CLASS(*), OPTIONAL, INTENT(IN)  :: Mold         !! key mold
    tLogical, OPTIONAL, INTENT(IN)  :: MinPQ
    !^ If present and true, the priority queue is a MinPQ. <br>
    !  Otherwise, the priority queue is a MaxPQ. <br>
    tIndex,   OPTIONAL, INTENT(IN)  :: IncSize      !! incremental size of priority queue if it is full
    tLogical, OPTIONAL, INTENT(IN)  :: Shrink
    !^ flag to shrink the collection capacity <br>
    ! - true if want to reduce capacity when the size is less than a quarter of the capacity. <br>
    ! - otherwise, the capacity stays the same. <br>
    ! - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

! FLOW

    ! first, check required input data
    IF (InitCap < 1_kIndex) THEN
        CALL Handle_ErrLevel('PQBinHeap_CreateEmpty', ModName, ErrWarning, &
                'Invalid InitCap (< 1).  Set the initial capacity of priority queue to 16.')
        Capacity = Collection%IncSize
    ELSE
        Capacity = InitCap
    END IF

    ! then, allocate space for the keys in the priority queue
    CALL MemAlloc(Collection%Keys, Capacity)
    CALL Collection%KeyPool%Construct()

    ! set key mold
    IF (PRESENT(Mold)) ALLOCATE(Collection%Mold, MOLD=Mold)

    ! finally, check optional input data
    IF (PRESENT(MinPQ))  Collection%Min = MinPQ
    Collection%IncSize = 0_kIndex       ! reset it to zero
    IF (PRESENT(IncSize)) THEN
        IF (IncSize > 0_kIndex) Collection%IncSize = IncSize
    END IF
    IF (PRESENT(Shrink)) Collection%Shrink  =  Shrink
    Collection%Cursor = 0_kIndex

    RETURN

END SUBROUTINE PQBinHeap_CreateEmpty

!******************************************************************************

SUBROUTINE PQBinHeap_ConstructorByArray(Collection, N, Keys, MinPQ, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a priority queue from an array of key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap),   INTENT(INOUT)   :: Collection   !! collection
    tIndex,             INTENT(IN)      :: N            !! number of keys
    CLASS(*),           INTENT(IN)      :: Keys(1:N)    !! key array
    tLogical, OPTIONAL, INTENT(IN)      :: MinPQ
    !^ true if the priority queue to be implemented as a MinPQ; default -> a MaxPQ
    tIndex,   OPTIONAL, INTENT(IN)      :: IncSize      !! incremental size of priority queue if it is full
    tLogical, OPTIONAL, INTENT(IN)      :: Shrink
    !^ true if want to reduce capacity when size is less than a quarter of the capacity;
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, InitCap

! FLOW

    ! create empty collection
    InitCap = N*2_kIndex    ! by default, doubling its capacity
    IF (PRESENT(IncSize)) THEN
        IF (IncSize > 0_kIndex) InitCap = N + IncSize
    END IF
    CALL Collection%CreateEmpty(InitCap, Keys(1), MinPQ, IncSize, Shrink)

    ! add input keys to the priority queue
    DO I = 1_kIndex, N
        CALL Collection%Insert(Keys(I))
    END DO

    RETURN

END SUBROUTINE PQBinHeap_ConstructorByArray

!******************************************************************************

SUBROUTINE PQBinHeap_Destruct(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct PQBinHeap object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(INOUT) :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%MemFree()

    RETURN

END SUBROUTINE PQBinHeap_Destruct

!******************************************************************************

SUBROUTINE PQBinHeap_Clear(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clear the PQBinHeap object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(INOUT) :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! reset components
    Collection%Last   = 0_kIndex
    Collection%Cursor = 0_kIndex

    ! free memory
    IF (ALLOCATED(Collection%Keys)) THEN
        DO I = 1_kIndex, SIZE(Collection%Keys, KIND=kIndex)
            CALL Collection%Keys(I)%MemFree()
        END DO
    END IF
    IF (ALLOCATED(Collection%Mold)) DEALLOCATE(Collection%Mold)

    RETURN

END SUBROUTINE PQBinHeap_Clear

!******************************************************************************

SUBROUTINE PQBinHeap_Finalizer(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(PQBinHeap), INTENT(INOUT)  :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! free up memory and reset components
    CALL Collection%Destruct()

    RETURN

END SUBROUTINE PQBinHeap_Finalizer

!******************************************************************************

SUBROUTINE PQBinHeap_InsertKey(Collection, NewKey)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new key to the top (or bottom) of the priority queue.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(INOUT) :: Collection   !! collection
    CLASS(*),         INTENT(IN)    :: NewKey       !! new key to be added to the priority queue

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether type of the specified item is valid or not
    IF (.NOT.Collection%IsKeyValid(NewKey)) THEN
        CALL Handle_ErrLevel('PQBinHeap_InsertKey', ModName, ErrSevere, &
                'Type of the specified key is either invalid or NOT the same as that of stored keys.')
        RETURN
    END IF

    ! first, grow the collection capacity if necessary
    CALL Collection%Growing()

    ! next, update pointer
    Collection%Last = Collection%Last + 1_kIndex

    ! then, add new key to the priority queue
    CALL Collection%Keys(Collection%Last)%Set(NewKey, Collection%KeyPool)

    ! restore heap order
    CALL Collection%SwimUp(Collection%Last)

#ifdef DebugMode
    ! for debugging purpose
    IF (.NOT.IsHeapOrdered(Collection, 1_kIndex)) THEN
        CALL Handle_ErrLevel('PQBinHeap_InsertKey', ModName, ErrWarning, &
                             'The heap is NOT in order.')
    END IF
#endif

    RETURN

END SUBROUTINE PQBinHeap_InsertKey

!******************************************************************************

FUNCTION PQBinHeap_RemoveKey(Collection, HPKey) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve and remove the highest-priority key from the priority queue.  Also, return
    !  a flag indicating whether the key is successfully retrieved and removed or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(INOUT) :: Collection   !! collection
    CLASS(*),         INTENT(INOUT) :: HPKey        !! the highest-priority key
    !> flag indicating whether the specified key is successfully retrieved and removed or not.
    tLogical                        :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! first, check whether the priority queue is empty or not
    IF (Collection%IsEmpty()) THEN
        Success = FalseVal
        RETURN
    END IF

    ! get the highest-priority key
    IF (Collection%Keys(1)%Get(HPKey)) THEN
        !--- remove the highest-priority key from the queue ---
        ! set flag
        Success = TrueVal
        ! swap the highest-priority key with the last one
        CALL Collection%SwapKeys(1_kIndex, Collection%Last)
        ! update pointer
        Collection%Last = Collection%Last - 1_kIndex
        ! restore heap order
        CALL Collection%SinkDown(1_kIndex, Collection%Last)
        ! shrink the collection if necessary
        CALL Collection%Shrinking()
#ifdef DebugMode
        ! for debugging purpose
        IF (.NOT.IsHeapOrdered(Collection, 1_kIndex)) THEN
            CALL Handle_ErrLevel('PQBinHeap_RemoveKey', ModName, ErrWarning, &
                                 'The heap is NOT in order.')
        END IF
#endif
    ELSE
        ! set flag
        Success = FalseVal
        ! warning of mismatch types
        CALL Handle_ErrLevel('PQBinHeap_RemoveKey', ModName, ErrSevere, &
                             'Type of the specified key is NOT the same as stored keys.')
    END IF

    RETURN

END FUNCTION PQBinHeap_RemoveKey

!******************************************************************************

FUNCTION PQBinHeap_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get size of the priority queue.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(IN)    :: Collection   !! collection
    tIndex                          :: Size         !! size (number of keys)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%Last

    RETURN

END FUNCTION PQBinHeap_GetSize

!******************************************************************************

FUNCTION PQBinHeap_PeekKey(Collection, HPKey) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the highest-priority key from the priority queue.  Also, return
    !  a flag indicating whether the key is successfully retrieved or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(INOUT) :: Collection   !! collection
    CLASS(*),         INTENT(OUT)   :: HPKey        !! the highest-priority key
    !> flag indicating whether the specified key is successfully retrieved or not.
    tLogical                        :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether the priority queue is empty or not
    IF (Collection%IsEmpty()) THEN
        Success = FalseVal
        RETURN
    END IF

    ! get the highest-priority key
    IF (Collection%Keys(1)%Get(HPKey)) THEN
        ! set flag
        Success = TrueVal
    ELSE
        ! set flag
        Success = FalseVal
        ! warning of mismatch types
        CALL Handle_ErrLevel('PQBinHeap_PeekKey', ModName, ErrWarning, &
                'Type of the specified key is NOT the same as that of stored keys.')
    END IF

    RETURN

END FUNCTION PQBinHeap_PeekKey

!******************************************************************************

FUNCTION PQBinHeap_Move2FirstKey(Collection, Key) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the first (starting) key in a collection.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap),   INTENT(INOUT)   :: Collection    !! collection
    !% the first item as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Key
    !> a flag indicating whether the table contains key or not <br>
    ! - true if the table is empty. <br>
    ! - otherwise the first key is available.
    tLogical                            :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if the collection is empty or not
    IsEmpty = Collection%IsEmpty()
    IF (IsEmpty) RETURN
    
    ! set cursor pointer
    Collection%Cursor = 1_kIndex

    IF ((.NOT.IsEmpty).AND.PRESENT(Key)) THEN
        ! get current key
        IF (.NOT.Collection%Keys(Collection%Cursor)%Get(Key)) THEN
            CALL Handle_ErrLevel('PQBinHeap_Move2FirstKey', ModName, ErrSevere, &
                                 'Type of the specified item is NOT the same as stored items.')
        END IF
    END IF

    RETURN

END FUNCTION PQBinHeap_Move2FirstKey

!******************************************************************************

FUNCTION PQBinHeap_Move2NextKey(Collection, Key) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next key in a collection. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap),   INTENT(INOUT)   :: Collection    !! collection
    !% the next item as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Key
    !> a flag indicating whether the move to the end of the table occurs or not <br>
    ! - true if next key is NOT available. <br>
    ! - otherwise next key is available.
    tLogical                            :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set cursor pointer
    Collection%Cursor = Collection%Cursor + 1_kIndex

    ! set return flag
    IsTheEnd = (Collection%Cursor > Collection%Last)

    IF ((.NOT.IsTheEnd).AND.PRESENT(Key)) THEN
        ! get current key
        IF (.NOT.Collection%Keys(Collection%Cursor)%Get(Key)) THEN
            CALL Handle_ErrLevel('PQBinHeap_Move2NextKey', ModName, ErrSevere, &
                                 'Type of the specified item is NOT the same as stored items.')
        END IF
    END IF


    RETURN

END FUNCTION PQBinHeap_Move2NextKey

!******************************************************************************

FUNCTION PQBinHeap_ToArray(Collection, Keys) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove all keys from the collection.  Also, return
    ! a flag indicating whether the keys are successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% PQBinHeap object
    CLASS(PQBinHeap), INTENT(INOUT) :: Collection
    !% the keys to be retrieved and removed from the collection
    CLASS(*),         INTENT(INOUT) :: Keys(:)
    !> flag indicating whether the keys are successfully retrieved and removed. <br>
    tLogical                        :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Success = Collection%GetAll(Keys)

    ! remove all keys from the collection
    IF (Success) CALL Collection%Clear()

    RETURN

END FUNCTION PQBinHeap_ToArray

!**************************************************************************************

FUNCTION PQBinHeap_GetAll(Collection, Keys) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all keys (without removing them) from the collection.  Also,
    ! return a flag indicating whether the keys are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% PQBinHeap object
    CLASS(PQBinHeap), INTENT(INOUT) :: Collection
    !% the keys to be retrieved from the collection
    CLASS(*),         INTENT(INOUT) :: Keys(1:)
    !> flag indicating whether the keys are successfully retrieved. <br>
    tLogical                        :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, MinSize

! FLOW

    ! check whether the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        Success = FalseVal
        RETURN
    ELSE
        ! check whether type of the specified keys is valid or not
        IF (.NOT.Collection%IsKeyValid(Keys(1))) THEN
            Success = FalseVal
            CALL Handle_ErrLevel('PQBinHeap_GetAll', ModName, ErrSevere, &
                                 'Type of the specified key is NOT the same as stored keys.')
            RETURN
        END IF
    END IF

    ! get keys from the collection
    MinSize = MIN(Collection%Last, SIZE(Keys, KIND=kIndex))
    DO I = 1, MinSize
        Success = Collection%Keys(I)%Get(Keys(I))
        IF (.NOT.Success) EXIT
    END DO

    RETURN

END FUNCTION PQBinHeap_GetAll

! -----------------------------------------------------------------------------
! -----                     Auxiliary Procedures                          -----
! -----------------------------------------------------------------------------

SUBROUTINE PQBinHeap_Growing(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!! To increase the collection's capacity if needed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(INOUT) :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

!** FLOW:

    IF (.NOT.ALLOCATED(Collection%Keys)) THEN
        ! the collection has not yet been constructed.
        Capacity = 16_kIndex
        ! allocate storage for the collections' items
        CALL MemAlloc(Collection%Keys, Capacity)
    ELSE
        Capacity = SIZE(Collection%Keys)
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
            CALL MemResize(Collection%Keys, [Capacity])
        END IF
    END IF

    RETURN

END SUBROUTINE PQBinHeap_Growing

!******************************************************************************

SUBROUTINE PQBinHeap_Shrinking(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!! To decrease the collection's capacity if needed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(INOUT) :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CurCap, CurSize

!** FLOW:

    IF (.NOT.ALLOCATED(Collection%Keys)) THEN
        ! the collection has not yet been constructed so simply return.
        RETURN
    END IF
    IF (Collection%Shrink) THEN
        CurCap  = SIZE(Collection%Keys)
        CurSize = Collection%GetSize()
        IF ((CurSize >= 0_kIndex).AND.(CurSize <= CurCap/4_kIndex)) THEN
            ! halves the collection's capacity
            CurCap = CurCap/2_kIndex
            ! check if the capacity is zero or not
            IF (CurCap <= 0_kIndex) CurCap = 1_kIndex
            ! resize the collections' items
            CALL MemResize(Collection%Keys, [CurCap])
        END IF
    END IF

    RETURN

END SUBROUTINE PQBinHeap_Shrinking

!******************************************************************************

SUBROUTINE PQBinHeap_ReHeapify_TopDown(Collection, Start, Bottom)

!** PURPOSE OF THIS SUBROUTINE
    !^ To restore heap order by sinking down

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(INOUT) :: Collection   !! collection
    tIndex,           INTENT(IN)    :: Start        !! starting index
    tIndex,           INTENT(IN)    :: Bottom       !! ending index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(KeyOrdered)    :: Temp
    tIndex              :: I, J

!** FLOW:

    ! initialize
    CALL Collection%Keys(Start)%Copy(Temp)  ! Temp = Collection%Keys(Start)
    I    = Start
    J    = I + I
    ! do while J <= Bottom
    IF (Collection%Min) THEN
        ! for MinPQ
        DO
            IF (J > Bottom) EXIT
            IF (J < Bottom) THEN
                ! compare to the better underling
                IF (Collection%Keys(J) > Collection%Keys(J+1)) THEN
                    J = J + 1
                END IF
            END IF
            ! found key's level. Terminate the sift-down.
            IF (Collection%Keys(J) >= Temp) EXIT
            ! otherwise, demote key and continue.
            CALL Collection%Keys(J)%Copy(Collection%Keys(I))    ! Collection%Keys(I) = Collection%Keys(J)
            I = J
            J = I + I
        END DO
    ELSE
        ! for MaxPQ
        DO
            IF (J > Bottom) EXIT
            IF (J < Bottom) THEN
                ! compare to the better underling
                IF (Collection%Keys(J) < Collection%Keys(J+1)) THEN
                    J = J + 1
                END IF
            END IF
            ! found key's level. Terminate the sift-down.
            IF (Collection%Keys(J) <= Temp) EXIT
            ! otherwise, demote key and continue.
            CALL Collection%Keys(J)%Copy(Collection%Keys(I))    ! Collection%Keys(I) = Collection%Keys(J)
            I = J
            J = I + I
        END DO
    END IF
    ! put key into its slot.
    CALL Temp%Copy(Collection%Keys(I))  ! Collection%Keys(I) = Temp

    RETURN

END SUBROUTINE PQBinHeap_ReHeapify_TopDown

!******************************************************************************

SUBROUTINE PQBinHeap_ReHeapify_BottomUp(Collection, Start)

!** PURPOSE OF THIS SUBROUTINE
    !^ To restore heap order by swimming up.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(INOUT) :: Collection   !! collection
    tIndex,           INTENT(IN)    :: Start        !! starting index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(KeyOrdered)    :: Temp
    tIndex              :: I, J

!** FLOW:

    CALL Collection%Keys(Start)%Copy(Temp)  ! Temp = Collection%Keys(Start)
    I    = Start
    J    = I/2
    ! do while k > 1 and key(k/2) < key(k)
    IF (Collection%Min) THEN
        ! for MinPQ
        DO WHILE ((I > 1).AND.(Collection%Keys(J) > Temp))
            ! promote key and continue.
            CALL Collection%Keys(J)%Copy(Collection%Keys(I))    ! Collection%Keys(I) = Collection%Keys(J)
            I = J
            J = I/2
        END DO
    ELSE
        ! for MaxPQ
        DO WHILE ((I > 1).AND.(Collection%Keys(J) < Temp))
            ! promote key and continue.
            CALL Collection%Keys(J)%Copy(Collection%Keys(I))    ! Collection%Keys(I) = Collection%Keys(J)
            I = J
            J = I/2
        END DO
    END IF
    ! put key into its slot.
    CALL Temp%Copy(Collection%Keys(I))  ! Collection%Keys(I) = Temp

    RETURN

END SUBROUTINE PQBinHeap_ReHeapify_BottomUp

!******************************************************************************

RECURSIVE FUNCTION IsHeapOrdered(Collection, Start) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE
    !^ To check whether the heap is in order or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(IN)    :: Collection   !! collection
    tIndex,           INTENT(IN)    :: Start        !! starting index
    tLogical                        :: Flag         !! true if the heap is in order

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Left, Right

!** FLOW:

    IF (Start > Collection%Last) THEN
        Flag = TrueVal
        RETURN
    END IF

    Left  = Start + Start
    Right = Left + 1

    IF (Collection%Min) THEN
        ! for MinPQ
        IF ((Left <= Collection%Last) .AND. (Collection%Keys(Start) > Collection%Keys(Left))) THEN
            Flag = FalseVal
            RETURN
        END IF
        IF ((Right <= Collection%Last) .AND. (Collection%Keys(Start) > Collection%Keys(Right))) THEN
            Flag = FalseVal
            RETURN
        END IF
    ELSE
        ! for MaxPQ
        IF ((Left <= Collection%Last) .AND. (Collection%Keys(Start) < Collection%Keys(Left))) THEN
            Flag = FalseVal
            RETURN
        END IF
        IF ((Right <= Collection%Last) .AND. (Collection%Keys(Start) < Collection%Keys(Right))) THEN
            Flag = FalseVal
            RETURN
        END IF
    END IF

    Flag = (IsHeapOrdered(Collection, Left) .AND. IsHeapOrdered(Collection, Right))

    RETURN

END FUNCTION IsHeapOrdered

!******************************************************************************

FUNCTION PQBinHeap_IsKeyValid(Collection, Key) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To check whether the type of specified key is valid or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(INOUT) :: Collection   !! collection
    CLASS(*),         INTENT(IN)    :: Key          !! the key to be checked
    tLogical                        :: Valid        !! true if type of the specified key is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether the mold and the specified key have the same type or not
    IF (ALLOCATED(Collection%Mold)) THEN
        Valid = IsSameKeyOrdered(Collection%Mold, Key)
    ELSE
        ! this is the first key inserted so set it as the mold
        Valid = IsKeyOrdered(Key)
        IF (Valid) CALL Collection%CreateEmpty(16_kIndex, Mold=Key)
    END IF

    RETURN

END FUNCTION PQBinHeap_IsKeyValid

!******************************************************************************

SUBROUTINE PQBinHeap_SwapKeys(Collection, I, J)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To swap key I and key J.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBinHeap), INTENT(INOUT) :: Collection   !! collection
    tIndex,           INTENT(IN)    :: I, J

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(KeyOrdered)    :: Temp

! FLOW

    CALL Collection%Keys(I)%Copy(Temp)                  ! Temp = Collection%Keys(I)
    CALL Collection%Keys(J)%Copy(Collection%Keys(I))    ! Collection%Keys(I) = Collection%Keys(J)
    CALL Temp%Copy(Collection%Keys(J))                  ! Collection%Keys(J) = Temp

    RETURN

END SUBROUTINE PQBinHeap_SwapKeys

!******************************************************************************

END MODULE MClass_PQBinHeap

!******************************************************************************
