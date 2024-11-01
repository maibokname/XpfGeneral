
MODULE MClass_PQMinMax

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *PQMinMax* type and its related routines.  The *PQMinMax* type is
!   a collection type that employs an elementary implementation (where its array representation
!   is ordered using a sorting algorithm) to provide common operations for a priority queue.
!   The *PQMinMax* type is a generalized priority queue that offers combined operations of a
!   max-priority queue and a min-priority queue. <br>
!   The *PQMinMax* type employs the *KeyOrdered* type to store comparable keys where allowed key
!   types include the *CHARACTER*, *INTEGER* and *REAL* intrinsic types as well as any derived
!   type that is in the *Comparable* class.  Like other collection types, it must be employed to
!   store keys of only one particular data type.  To store keys of another data type, it must be
!   destructed before inserting keys of different data type. <br>
!   As an *ordered* collection type, the *PQMinMax* type provides an ordered iteration over its
!   stored keys, which are sorted according to the natural ordering of its keys.  The *StartMin*
!   method (or the *StartMax* method) must first be called to start an iteration for an ascending
!   order (or for an descending order).  The *MoveUp* method (or the *MoveDown* method) can then
!   be called repeatedly to move to the next keys with lower priorities in the desired order. <br>

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
    USE MClass_Object,      ONLY: Object, ASSIGNMENT(=)
    USE MClass_Comparable,  ONLY: Comparable
    USE MClass_GenData,     ONLY: IfacePolyCopy
    USE MClass_KeyOrdered
    USE MClass_CompNodePool
    USE MClass_MemoryPool
    USE MClass_BaseCollection
    USE MClass_BaseIterable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: PQMinMax

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'MClass_PQMinMax'
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
    !> The *PQMinMax* type is a collection type that employs an ordered array implementation
    !  to provide common operations for a generalized priority queue.
    TYPE, EXTENDS(BaseCollection)   :: PQMinMax
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
        PROCEDURE, PRIVATE  :: Growing      => PQMinMax_Growing
        PROCEDURE, PRIVATE  :: Shrinking    => PQMinMax_Shrinking
        PROCEDURE, PRIVATE  :: IsKeyValid   => PQMinMax_IsKeyValid
        PROCEDURE, PRIVATE  :: SwapKeys     => PQMinMax_SwapKeys
        PROCEDURE, PRIVATE  :: PQMinMax_ConstructorByArray
        ! ---------------------------------------------------------------------
        ! -----  Deferred/Overridden Procedures from BaseCollection Type  -----
        ! ---------------------------------------------------------------------
        !> Use the *Construct* method to construct the collection from another collection.
        PROCEDURE   :: CopyCollection   => PQMinMax_CopyCollection
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        PROCEDURE   :: Clear            => PQMinMax_Clear
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        !  **Note**:  This method is NOT equivalent to the *Clear* method. <br>
        PROCEDURE   :: Destruct         => PQMinMax_Destruct
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize          => PQMinMax_GetSize
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
        PROCEDURE   :: CreateEmpty  => PQMinMax_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a priority queue from the specified key arrays. <br>
        !  **Usage**: <br>
        !   ! use default options  <br>
        !   --->    CALL Collection%Construct(40, KeyArr) <br>
        !   ! specify all options (initial capacity is array size plus incremental size) <br>
        !   --->    CALL Collection%Construct(20, KeyArr, MinPQ, IncSize, Shrink) <br>
        GENERIC     :: Construct    => PQMinMax_ConstructorByArray
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key to the priority queue. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Insert(Key) <br>
        PROCEDURE   :: Insert       => PQMinMax_InsertKey
        !> **Type-Bound Function**: RemoveMin <br>
        !  **Purpose**:  To retrieve and remove the minimum key from the priority queue.  Also,
        !       return a flag indicating whether the key is successfully removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%RemoveMin(MinKey) <br>
        !   --->    IF (.NOT.Collection%RemoveMin(MinKey)) DoSomething
        PROCEDURE   :: RemoveMin    => PQMinMax_RemoveMinKey
        !> **Type-Bound Function**: RemoveMax <br>
        !  **Purpose**:  To retrieve and remove the maximum key from the priority queue.  Also,
        !       return a flag indicating whether the key is successfully removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%RemoveMax(MaxKey) <br>
        !   --->    IF (.NOT.Collection%RemoveMax(MaxKey)) DoSomething
        PROCEDURE   :: RemoveMax    => PQMinMax_RemoveMaxKey
        !> **Type-Bound Function**: PeekMin <br>
        !  **Purpose**:  To retrieve the minimum key from the priority queue.  Also, return
        !       a flag indicating whether the key is successfully removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%PeekMin(MinKey) <br>
        !   --->    IF (.NOT.Collection%PeekMin(MinKey)) DoSomething
        PROCEDURE   :: PeekMin      => PQMinMax_PeekMinKey
        !> **Type-Bound Function**: PeekMax <br>
        !  **Purpose**:  To retrieve the maximum key from the priority queue.  Also, return
        !       a flag indicating whether the key is successfully removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%PeekMax(MaxKey) <br>
        !   --->    IF (.NOT.Collection%PeekMax(MaxKey)) DoSomething
        PROCEDURE   :: PeekMax      => PQMinMax_PeekMaxKey
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the collection.  Also, return a flag
        !                indicating whether the items are successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Items) <br>
        !   --->    IF (.NOT.Collection%ToArray(Items, IsDescend=.TRUE.)) DoSomething
        PROCEDURE   :: ToArray      => PQMinMax_ToArray
        !> **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the collection. Also, return
        !                a flag indicating whether the items are available. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Items) <br>
        !   --->    IF (.NOT.Collection%GetAll(Items, IsDescend=.TRUE.)) DoSomething
        PROCEDURE   :: GetAll       => PQMinMax_GetAll
        !> **Type-Bound Function**: StartMin <br>
        !  **Purpose**:  To start the *ascending* iteration and return a flag indicating
        !                whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartMin() <br>
        !   --->    IsEmpty = Collection%StartMin(MinKey)
        PROCEDURE   :: StartMin   => PQMinMax_Move2MinKey
        !> **Type-Bound Function**: StartMax <br>
        !  **Purpose**:  To start the *descending* iteration and return a flag indicating
        !                whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartMax() <br>
        !   --->    IsEmpty = Collection%StartMax(MaxKey)
        PROCEDURE   :: StartMax   => PQMinMax_Move2MaxKey
        !> **Type-Bound Function**: MoveUp <br>
        !  **Purpose**:  To move to the next iteration in ascending order and return a flag
        !                indicating whether the cursor pointer has reached the end of the
        !                collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveUp() <br>
        !   --->    IsTheEnd = Collection%MoveUp(NextKey) <br>
        PROCEDURE   :: MoveUp       => PQMinMax_MoveUpNext
        !> **Type-Bound Function**: MoveDown <br>
        !  **Purpose**:  To move to the next iteration in descending order and return a flag
        !                indicating whether the cursor pointer has reached the end of the
        !                collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveDown() <br>
        !   --->    IsTheEnd = Collection%MoveDown(NextKey) <br>
        PROCEDURE   :: MoveDown     => PQMinMax_MoveDownNext
        ! ---------------------------------------------------------------------
        ! -----     Deferred/Overridden Procedures from Object Type       -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => PQMinMax_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => PQMinMax_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => PQMinMax_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => PQMinMax_ToString
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => PQMinMax_HashCode
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: PQMinMax_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE PQMinMax

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! ---------------------------------------------------------------------
! -----     Deferred/Overridden Procedures from Object Type       -----
! ---------------------------------------------------------------------

SUBROUTINE PQMinMax_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax),    INTENT(IN)  :: SrcObj   !! source object
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
    TYPE IS (PQMinMax)
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
        CALL Handle_ErrLevel('PQMinMax_Copy', ModName, ErrSevere, &
                             'Type of the DstObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE PQMinMax_Copy

!******************************************************************************

FUNCTION PQMinMax_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(IN) :: LhsObj   !! an object
    CLASS(Object),   INTENT(IN) :: RhsObj   !! another object
    tLogical                    :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    TYPE IS (PQMinMax)
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

END FUNCTION PQMinMax_IsEqualTo

!******************************************************************************

SUBROUTINE PQMinMax_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the PQMinMax object.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(INOUT)  :: Obj

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

END SUBROUTINE PQMinMax_MemFree

!******************************************************************************

FUNCTION PQMinMax_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the PQMinMax type.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(IN) :: Obj
    tCharAlloc                  :: Str

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
    Str = '{PQMinMax with ' // ToChar(Obj%GetSize()) // ' items : ' // BaseStr // '}'

    RETURN

END FUNCTION PQMinMax_ToString

!******************************************************************************

FUNCTION PQMinMax_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(IN) :: Obj
    tIndex                      :: Code

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

END FUNCTION PQMinMax_HashCode

! ---------------------------------------------------------------------
! -----  Deferred/Overridden Procedures from BaseCollection Type  -----
! ---------------------------------------------------------------------

SUBROUTINE PQMinMax_CopyCollection(This, Other, ItemCopy, ValCopy)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items as the given collection (Other).
    !  This is a deferred procedure by the *BaseCollection* class. <br>
    !  *Note*:  Other must be in the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(PQMinMax),       INTENT(INOUT)    :: This
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
    CLASS IS (PQMinMax)
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
                CALL Handle_ErrLevel('PQMinMax_CopyCollection', ModName, ErrSevere, &
                                     'Item type of "Other" is NOT a valid key type.')
            END IF
            NULLIFY(MoldPtr)
        END BLOCK
    CLASS DEFAULT
        CALL Handle_ErrLevel('PQMinMax_CopyCollection', ModName, ErrSevere, &
                             'Type of "Other" must be in the "BaseIterable" class.')
    END SELECT
    
    RETURN

END SUBROUTINE PQMinMax_CopyCollection

!******************************************************************************

SUBROUTINE PQMinMax_CreateEmpty(Collection, InitCap, Mold, MinPQ, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create an empty priority queue.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax),    INTENT(OUT) :: Collection   !! collection
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
        CALL Handle_ErrLevel('PQMinMax_CreateEmpty', ModName, ErrWarning, &
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

END SUBROUTINE PQMinMax_CreateEmpty

!******************************************************************************

SUBROUTINE PQMinMax_ConstructorByArray(Collection, N, Keys, MinPQ, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a priority queue from an array of key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax),    INTENT(INOUT)   :: Collection   !! collection
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

END SUBROUTINE PQMinMax_ConstructorByArray

!******************************************************************************

SUBROUTINE PQMinMax_Destruct(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct PQMinMax object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(INOUT)  :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%MemFree()

    RETURN

END SUBROUTINE PQMinMax_Destruct

!******************************************************************************

SUBROUTINE PQMinMax_Clear(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clear the PQMinMax object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(INOUT)  :: Collection   !! collection

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

END SUBROUTINE PQMinMax_Clear

!******************************************************************************

SUBROUTINE PQMinMax_Finalizer(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(PQMinMax), INTENT(INOUT)   :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! free up memory and reset components
    CALL Collection%Destruct()

    RETURN

END SUBROUTINE PQMinMax_Finalizer

!******************************************************************************

SUBROUTINE PQMinMax_InsertKey(Collection, NewKey)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new key to the top (or bottom) of the priority queue.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(INOUT)  :: Collection   !! collection
    CLASS(*),        INTENT(IN)     :: NewKey       !! new key to be added to the priority queue

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether type of the specified item is valid or not
    IF (.NOT.Collection%IsKeyValid(NewKey)) THEN
        CALL Handle_ErrLevel('PQMinMax_InsertKey', ModName, ErrSevere, &
                'Type of the specified key is either invalid or NOT the same as that of stored keys.')
        RETURN
    END IF

    ! first, grow the collection capacity if necessary
    CALL Collection%Growing()

    ! next, update pointer
    Collection%Last = Collection%Last + 1_kIndex

    ! then, add new key to the priority queue
    CALL Collection%Keys(Collection%Last)%Set(NewKey, Collection%KeyPool)

    ! sort the keys
    CALL SortAscend(Collection%Keys(1:Collection%Last))

    RETURN

END SUBROUTINE PQMinMax_InsertKey

!******************************************************************************

FUNCTION PQMinMax_RemoveMinKey(Collection, HPKey) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve and remove the minimum key from the priority queue.  Also, return
    !  a flag indicating whether the key is successfully retrieved and removed or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(INOUT)  :: Collection   !! collection
    CLASS(*),        INTENT(INOUT)  :: HPKey        !! the minimum key
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

    ! get the minimum key
    IF (Collection%Keys(1)%Get(HPKey)) THEN
        !--- remove the minimum key from the queue ---
        ! set flag
        Success = TrueVal
        ! swap the minimum key with the last one
        CALL Collection%SwapKeys(1_kIndex, Collection%Last)
        ! update pointer
        Collection%Last = Collection%Last - 1_kIndex
        ! sort the keys
        CALL SortAscend(Collection%Keys(1:Collection%Last))
        ! shrink the collection if necessary
        CALL Collection%Shrinking()
    ELSE
        ! set flag
        Success = FalseVal
        ! warning of mismatch types
        CALL Handle_ErrLevel('PQMinMax_RemoveMinKey', ModName, ErrSevere, &
                             'Type of the specified key is NOT the same as stored keys.')
    END IF

    RETURN

END FUNCTION PQMinMax_RemoveMinKey

!******************************************************************************

FUNCTION PQMinMax_RemoveMaxKey(Collection, HPKey) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve and remove the maximum key from the priority queue.  Also, return
    !  a flag indicating whether the key is successfully retrieved and removed or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(INOUT)  :: Collection   !! collection
    CLASS(*),        INTENT(INOUT)  :: HPKey        !! the maximum key
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

    ! get the maximum key
    IF (Collection%Keys(Collection%Last)%Get(HPKey)) THEN
        !--- remove the maximum key from the queue ---
        ! set flag
        Success = TrueVal
        ! update pointer
        Collection%Last = Collection%Last - 1_kIndex
        ! shrink the collection if necessary
        CALL Collection%Shrinking()
    ELSE
        ! set flag
        Success = FalseVal
        ! warning of mismatch types
        CALL Handle_ErrLevel('PQMinMax_RemoveMaxKey', ModName, ErrSevere, &
                             'Type of the specified key is NOT the same as stored keys.')
    END IF

    RETURN

END FUNCTION PQMinMax_RemoveMaxKey

!******************************************************************************

FUNCTION PQMinMax_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get size of the priority queue.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(IN) :: Collection   !! collection
    tIndex                      :: Size         !! size (number of keys)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%Last

    RETURN

END FUNCTION PQMinMax_GetSize

!******************************************************************************

FUNCTION PQMinMax_PeekMinKey(Collection, HPKey) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the minimum key from the priority queue.  Also, return
    !  a flag indicating whether the key is successfully retrieved or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(INOUT)  :: Collection   !! collection
    CLASS(*),        INTENT(OUT)    :: HPKey        !! the minimum key
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

    ! get the minimum key
    IF (Collection%Keys(1)%Get(HPKey)) THEN
        ! set flag
        Success = TrueVal
    ELSE
        ! set flag
        Success = FalseVal
        ! warning of mismatch types
        CALL Handle_ErrLevel('PQMinMax_PeekMinKey', ModName, ErrWarning, &
                'Type of the specified key is NOT the same as that of stored keys.')
    END IF

    RETURN

END FUNCTION PQMinMax_PeekMinKey

!******************************************************************************

FUNCTION PQMinMax_PeekMaxKey(Collection, HPKey) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the maximum key from the priority queue.  Also, return
    !  a flag indicating whether the key is successfully retrieved or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(INOUT)  :: Collection   !! collection
    CLASS(*),        INTENT(OUT)    :: HPKey        !! the maximum key
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

    ! get the maximum key
    IF (Collection%Keys(Collection%Last)%Get(HPKey)) THEN
        ! set flag
        Success = TrueVal
    ELSE
        ! set flag
        Success = FalseVal
        ! warning of mismatch types
        CALL Handle_ErrLevel('PQMinMax_PeekMaxKey', ModName, ErrWarning, &
                'Type of the specified key is NOT the same as that of stored keys.')
    END IF

    RETURN

END FUNCTION PQMinMax_PeekMaxKey

!******************************************************************************

FUNCTION PQMinMax_Move2MinKey(Collection, Key) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the minimum (starting) key in a collection.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax),    INTENT(INOUT)   :: Collection    !! collection
    !% the minimum key as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Key
    !> a flag indicating whether the table contains key or not <br>
    ! - true if the table is empty. <br>
    ! - otherwise the minimu key is available.
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
            CALL Handle_ErrLevel('PQMinMax_Move2MinKey', ModName, ErrSevere, &
                                 'Type of the specified item is NOT the same as stored items.')
        END IF
    END IF

    RETURN

END FUNCTION PQMinMax_Move2MinKey

!******************************************************************************

FUNCTION PQMinMax_Move2MaxKey(Collection, Key) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the maximum (starting) key in a collection.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax),    INTENT(INOUT)   :: Collection    !! collection
    !% the maximum key as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Key
    !> a flag indicating whether the table contains key or not <br>
    ! - true if the table is empty. <br>
    ! - otherwise the maximum key is available.
    tLogical                            :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if the collection is empty or not
    IsEmpty = Collection%IsEmpty()
    IF (IsEmpty) RETURN
    
    ! set cursor pointer
    Collection%Cursor = Collection%Last

    IF ((.NOT.IsEmpty).AND.PRESENT(Key)) THEN
        ! get current key
        IF (.NOT.Collection%Keys(Collection%Cursor)%Get(Key)) THEN
            CALL Handle_ErrLevel('PQMinMax_Move2MaxKey', ModName, ErrSevere, &
                                 'Type of the specified item is NOT the same as stored items.')
        END IF
    END IF

    RETURN

END FUNCTION PQMinMax_Move2MaxKey

!******************************************************************************

FUNCTION PQMinMax_MoveUpNext(Collection, Key) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next key in an ascending order in a collection. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax),    INTENT(INOUT)   :: Collection    !! collection
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
            CALL Handle_ErrLevel('PQMinMax_MoveUpNext', ModName, ErrSevere, &
                                 'Type of the specified item is NOT the same as stored items.')
        END IF
    END IF


    RETURN

END FUNCTION PQMinMax_MoveUpNext

!******************************************************************************

FUNCTION PQMinMax_MoveDownNext(Collection, Key) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next key in an descending order in a collection. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax),    INTENT(INOUT)   :: Collection    !! collection
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
    Collection%Cursor = Collection%Cursor - 1_kIndex

    ! set return flag
    IsTheEnd = (Collection%Cursor < 1_kIndex)

    IF ((.NOT.IsTheEnd).AND.PRESENT(Key)) THEN
        ! get current key
        IF (.NOT.Collection%Keys(Collection%Cursor)%Get(Key)) THEN
            CALL Handle_ErrLevel('PQMinMax_MoveDownNext', ModName, ErrSevere, &
                                 'Type of the specified item is NOT the same as stored items.')
        END IF
    END IF


    RETURN

END FUNCTION PQMinMax_MoveDownNext

!******************************************************************************

FUNCTION PQMinMax_ToArray(Collection, Keys, IsDescend) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove all keys from the collection.  Also, return
    ! a flag indicating whether the keys are successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% PQMinMax object
    CLASS(PQMinMax),    INTENT(INOUT)   :: Collection
    !% the keys to be retrieved and removed from the collection
    CLASS(*),           INTENT(INOUT)   :: Keys(:)
    !> If present and true, return the keys in descending order. <br>
    !  Otherwise, return the keys in ascending order. <br>
    tLogical, OPTIONAL, INTENT(IN)      :: IsDescend
    !> flag indicating whether the keys are successfully retrieved and removed. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Success = Collection%GetAll(Keys, IsDescend)

    ! remove all keys from the collection
    IF (Success) CALL Collection%Clear()

    RETURN

END FUNCTION PQMinMax_ToArray

!**************************************************************************************

FUNCTION PQMinMax_GetAll(Collection, Keys, IsDescend) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all keys (without removing them) from the collection.  Also,
    ! return a flag indicating whether the keys are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% PQMinMax object
    CLASS(PQMinMax),    INTENT(INOUT)   :: Collection
    !% the keys to be retrieved from the collection
    CLASS(*),           INTENT(INOUT)   :: Keys(1:)
    !> If present and true, return the keys in descending order. <br>
    !  Otherwise, return the keys in ascending order. <br>
    tLogical, OPTIONAL, INTENT(IN)      :: IsDescend
    !> flag indicating whether the keys are successfully retrieved. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, MinSize
    tLogical    :: DescendOrder

! FLOW

    ! check whether the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        Success = FalseVal
        RETURN
    ELSE
        ! check whether type of the specified keys is valid or not
        IF (.NOT.Collection%IsKeyValid(Keys(1))) THEN
            Success = FalseVal
            CALL Handle_ErrLevel('PQMinMax_GetAll', ModName, ErrSevere, &
                                 'Type of the specified key is NOT the same as stored keys.')
            RETURN
        END IF
    END IF
    
    DescendOrder = FalseVal
    IF (PRESENT(IsDescend)) DescendOrder = IsDescend

    ! get keys from the collection
    MinSize = MIN(Collection%Last, SIZE(Keys, KIND=kIndex))
    IF (DescendOrder) THEN
        ! get keys in descending order
        J = Collection%Last
        DO I = 1, MinSize
            Success = Collection%Keys(J)%Get(Keys(I))
            IF (.NOT.Success) EXIT
            J = J - 1_kIndex
        END DO
    ELSE
        ! get keys in ascending order
        DO I = 1, MinSize
            Success = Collection%Keys(I)%Get(Keys(I))
            IF (.NOT.Success) EXIT
        END DO
    END IF

    RETURN

END FUNCTION PQMinMax_GetAll

! -----------------------------------------------------------------------------
! -----                     Auxiliary Procedures                          -----
! -----------------------------------------------------------------------------

SUBROUTINE PQMinMax_Growing(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!! To increase the collection's capacity if needed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(INOUT)  :: Collection   !! collection

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

END SUBROUTINE PQMinMax_Growing

!******************************************************************************

SUBROUTINE PQMinMax_Shrinking(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!! To decrease the collection's capacity if needed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(INOUT)  :: Collection   !! collection

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

END SUBROUTINE PQMinMax_Shrinking

!******************************************************************************

FUNCTION PQMinMax_IsKeyValid(Collection, Key) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To check whether the type of specified key is valid or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(INOUT)  :: Collection   !! collection
    CLASS(*),        INTENT(IN)     :: Key          !! the key to be checked
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

END FUNCTION PQMinMax_IsKeyValid

!******************************************************************************

SUBROUTINE PQMinMax_SwapKeys(Collection, I, J)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To swap key I and key J.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQMinMax), INTENT(INOUT)  :: Collection   !! collection
    tIndex,          INTENT(IN)     :: I, J

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(KeyOrdered)    :: Temp

! FLOW

    CALL Collection%Keys(I)%Copy(Temp)                  ! Temp = Collection%Keys(I)
    CALL Collection%Keys(J)%Copy(Collection%Keys(I))    ! Collection%Keys(I) = Collection%Keys(J)
    CALL Temp%Copy(Collection%Keys(J))                  ! Collection%Keys(J) = Temp

    RETURN

END SUBROUTINE PQMinMax_SwapKeys

!******************************************************************************
! define comparison macro for sorting in ascending order
#define COMPARE_GLT(A, B)       (A < B)
#define COMPARE_GLE(A, B)       (A <= B)

SUBROUTINE SortAscend(A)

!** PURPOSE OF THIS SUBROUTINE
    ! To perform sorting of the specified array in an ascending order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(KeyOrdered), INTENT(INOUT) :: A(0:)    ! array to be sorted
 
!** SUBROUTINE PARAMETER DECLARATIONS:
    ! initial cutoff to insertion sort
    tIndex,  PARAMETER  :: Insertion_CutOff = 32_kIndex
    ! The maximum number of entries in a run stack, good for an array of 2**64 elements.
    tSInt32, PARAMETER  :: Max_Merge_Stack = INT(CEILING(LOG(2.0_kFloat**64) / &
                                                 LOG(1.6180339887_kFloat)))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(KeyOrdered), ALLOCATABLE   :: Buffer(:)
    tIndex                          :: NA       ! size of the arrays

!** FLOW:
    
    ! check whether to use an insertion sort instead
    NA = SIZE(A, KIND=kIndex)
    IF (NA <= Insertion_CutOff) THEN
        CALL Insert_Guarded(A, 1, NA)
        RETURN
    END IF

    ! Allocate a buffer to use as scratch memory.
    CALL MemAlloc(Buffer, NA/2_kIndex)
    
    ! perform merge sort
    CALL Rust_MergeSort(A, NA, Buffer)

    RETURN

CONTAINS

    SUBROUTINE Rust_MergeSort(A, ALen, B)

    !** PURPOSE OF THIS SUBROUTINE
        ! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
        ! which is described in detail at
        ! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
        !
        ! The algorithm identifies strictly descending and non-descending
        ! subsequences, which are called natural runs.  Where these runs are less
        ! than a minimum run size they are padded by adding additional samples
        ! using a insertion sort.  The merge process is driven by a stack of
        ! pending un-merged runs.  Each newly found run is pushed onto the stack,
        ! and then pairs of adjacent runs are merged until these two invariants
        ! are satisfied:
        !
        ! 1. for every 'i' in '1..size(runs)-1': 'runlen(i - 1) > runlen(i)'
        ! 2. for every 'i' in '2..size(runs)-1': 'runlen(i - 2) > runlen(i - 1) + runlen(i)'
        !
        ! The invariants ensure that the total running time is 'O(n log n)' worst-case.
   
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(KeyOrdered), INTENT(INOUT) :: A(0:)
        tIndex,           INTENT(IN)    :: ALen 
        TYPE(KeyOrdered), INTENT(INOUT) :: B(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Finish, Min_Run, nRun, R, RCount, Start
        tIndex      :: RunLen(0:Max_Merge_Stack-1)
        tIndex      :: RunBase(0:Max_Merge_Stack-1)
        tIndex      :: LeftLen, LeftBase
        tIndex      :: RightLen, RightBase

    !** FLOW:

        ! Very short runs are extended using insertion sort to span at least Min_Run elements.
        ! Slices of up to this length are sorted using pair-insertion sort.
        Min_Run = Calc_Min_Run(ALen)

        ! Following Rust sort, natural runs in 'A' are identified by traversing
        ! it backwards.  By traversing it backward, merges more often go in the
        ! opposite direction (forwards).  According to developers of Rust sort,
        ! merging forwards is slightly faster than merging backwards.  Therefore
        ! identifying runs by traversing backwards should improve performance.
        RCount = 0_kIndex
        Finish = ALen - 1_kIndex
        DO WHILE (Finish >= 0_kIndex)
            
            ! Find the next natural run, and reverse it if it's strictly descending.
            Start = Finish
            IF (Start > 0_kIndex) THEN
                Start = Start - 1_kIndex
                IF (COMPARE_GLT(A(Start+1), A(Start))) THEN
                    Descending: DO WHILE (Start > 0_kIndex)
                        IF (COMPARE_GLE(A(Start-1), A(Start))) EXIT Descending
                        Start = Start - 1_kIndex
                    END DO Descending
                    CALL Reverse_Order_Base0(A, Start, Finish)
                ELSE
                    Ascending: DO WHILE(Start > 0_kIndex)
                        IF (COMPARE_GLT(A(Start), A(Start-1))) EXIT Ascending
                        Start = Start - 1_kIndex
                    END DO Ascending
                END IF
            END IF

            ! compute run length
            nRun = Finish - Start
        
            IF (nRun < Min_Run) THEN
                ! If run is short, extend to MinRun
                IF (nRun < Min_Run/8_kIndex) THEN
                    ! If nRun is too short, use insertion sort
                    Start = Finish - Min_Run + 1_kIndex
                    IF (Start < 0_kIndex) Start = 0_kIndex
                    CALL Insert_Guarded(A, Start+1, Finish+1)
                ELSE
                    ! nRun is long enough, use insertion sort
                    Insert_Loop: DO WHILE (Start > 0_kIndex)
                        IF ((Finish-Start) >= (Min_Run-1_kIndex)) EXIT Insert_Loop
                        Start = Start - 1_kIndex
                        CALL Insert_Head(A(Start:Finish))
                    END DO Insert_Loop
                    IF ((Start == 0_kIndex).AND.(Finish == ALen-1_kIndex)) RETURN
                END IF
            END IF
        
            ! initialize the stack
            RunBase(RCount) = Start
            RunLen(RCount)  = Finish - Start + 1_kIndex
            
            Finish = Start - 1_kIndex
            RCount = RCount + 1_kIndex

            ! Determine whether pairs of adjacent runs need to be merged to satisfy
            ! the invariants, and, if so, merge them.
            Merge_Loop: DO
                
                ! examine the stack of runs waiting to be merged
                R = Collapse(RunBase(0:RCount-1), RunLen(0:RCount-1))
                
                IF ((R < 0_kIndex).OR.(RCount <= 1_kIndex)) EXIT Merge_Loop
                LeftLen   = RunLen(R+1)
                LeftBase  = RunBase(R+1)
                RightLen  = RunLen(R)
                RightBase = RunBase(R)
                
                ! merge adjacent runs
                CALL Merging(A(LeftBase:RightBase+RightLen-1), LeftLen, B)

                ! set the stack
                RunBase(R) = LeftBase
                RunLen(R)  = LeftLen + RightLen
                IF (R == RCount-3_kIndex) THEN
                    RunBase(R+1) = RunBase(R+2)
                    RunLen(R+1)  = RunLen(R+2)
                END IF
                RCount = RCount - 1_kIndex
            END DO Merge_Loop
        END DO
        
        RETURN

    END SUBROUTINE Rust_MergeSort

    !******************************************************************

    FUNCTION Calc_Min_Run(N) RESULT(Min_Run)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To determine the minimum length of a run from 32-63 so that N/MIN_RUN is
        ! less than or equal to a power of two. See
        ! https://svn.python.org/projects/python/trunk/Objects/listsort.txt.
   
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: N
        tIndex              :: Min_Run
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Num
        tIndex      :: R
 
    !** FLOW:
        
        Num = N
        R = 0_kIndex

        DO WHILE(Num >= 64_kIndex)
            R = IOR(R, IAND(Num, 1))
            Num = SHIFTA(Num, 1)
        END DO
        Min_Run = Num + R

        RETURN

    END FUNCTION Calc_Min_Run

    !******************************************************************

    FUNCTION Collapse(Base, Length) RESULT (R)
    
    !** PURPOSE OF THIS SUBROUTINE
        ! To examine the stack of runs waiting to be merged, identifying adjacent runs
        ! to be merged until the stack invariants are restablished:
        ! 1. len(-3) > len(-2) + len(-1)
        ! 2. len(-2) > len(-1)
   
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: Base(0:)
        tIndex, INTENT(IN)  :: Length(0:)
        tIndex              :: R
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: N
        tLogical    :: Test
 
    !** FLOW:

        N = MIN(SIZE(Base),SIZE(Length))
        Test = FalseVal
        IF (N >= 2_kIndex) THEN
            IF ((Base(N-1) == 0_kIndex).OR.(Length(N-2) <= Length(N-1))) THEN
                Test = TrueVal
            ELSEIF (N >= 3_kIndex) THEN
                ! X exists
                IF (Length(N-3) <= (Length(N-2)+Length(N-1))) THEN
                    Test = TrueVal
                    ! |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                ELSEIF(N >= 4_kIndex) THEN
                    IF (Length(N-4) <= (Length(N-3)+Length(N-2))) THEN
                        Test = TrueVal
                        ! |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                    END IF
                END IF
            END IF
        END IF
        IF (Test) THEN
            ! By default merge Y & Z, rho2 or rho3
            IF (N >= 3_kIndex) THEN
                IF (Length(N-3) < Length(N-1)) THEN
                    R = N - 3_kIndex
                    ! |X| < |Z| => merge X & Y, rho1
                    RETURN
                END IF
            END IF
            R = N - 2_kIndex
            ! |Y| <= |Z| => merge Y & Z, rho4
            RETURN
        ELSE
            R = -1_kIndex
        END IF

        RETURN

    END FUNCTION Collapse

    !******************************************************************

    SUBROUTINE Insert_Head(A)

    !** PURPOSE OF THIS SUBROUTINE
        ! To inserts 'A(0)' into the pre-sorted sequence 'A(1:)' so that the
        ! whole 'A(0:)' becomes sorted, copying the first element into
        ! a temporary variable, iterating until the right place for it is found.
        ! copying every traversed element into the slot preceding it, and finally,
        ! copying data from the temporary variable into the resulting hole.
   
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(KeyOrdered), INTENT(INOUT) :: A(0:)
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex              :: I
        TYPE(KeyOrdered)    :: Temp
 
    !** FLOW:

        Temp = A(0)
        Find_Hole: DO I = 1_kIndex, SIZE(A, KIND=kIndex)-1_kIndex
            IF (COMPARE_GLE(Temp, A(I))) EXIT Find_Hole
            A(I-1) = A(I)
        END DO Find_Hole
        A(I-1) = Temp

        RETURN

    END SUBROUTINE Insert_Head

    !******************************************************************

    SUBROUTINE Merging(A, Mid, B)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge the two non-decreasing runs 'A(0:Mid-1)' and 'A(Mid:)'
        ! using 'B' as a temporary storage, and stores the merged runs into
        ! 'A(0:)'.  'Mid' must be > 0, and < 'SIZE(A)-1'. The buffer 'B'
        ! must be long enough to hold the shorter of the two runs.
   
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(KeyOrdered), INTENT(INOUT)    :: A(0:)
        tIndex,           INTENT(IN)       :: Mid
        TYPE(KeyOrdered), INTENT(INOUT)    :: B(0:)
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex              :: ALen, I, J, K
        TYPE(KeyOrdered)    :: Temp
 
    !** FLOW:

        ALen = SIZE(A, KIND=kIndex)

        ! Merge first copies the shorter run into 'B'. Then, depending on which
        ! run was shorter, it traces the copied run and the longer run forwards
        ! (or backwards), comparing their next unprocessed elements and then
        ! copying the lesser (or greater) one into 'A'.

        IF (Mid <= (ALen - Mid)) THEN
            ! The left run is shorter.
            B(0:Mid-1) = A(0:Mid-1)
            I = 0_kIndex
            J = Mid
            Merge_Lower: DO K = 0_kIndex, ALen-1_kIndex
                IF (COMPARE_GLE(B(I), A(J))) THEN
                    A(K) = B(I)
                    I = I + 1_kIndex
                    IF (I >= Mid) EXIT Merge_Lower
                ELSE
                    A(K) = A(J)
                    J = J + 1_kIndex
                    IF (J >= ALen) THEN
                        A(K+1:) = B(I:Mid-1)
                        EXIT Merge_Lower
                    END IF
                END IF
            END DO Merge_Lower
        ELSE
            ! The right run is shorter so check that it is stable
            B(0:ALen-Mid-1) = A(Mid:ALen-1)
            I = Mid - 1_kIndex
            J = ALen - Mid -1_kIndex
            Merge_Upper: DO K = ALen-1_kIndex, 0_kIndex, -1_kIndex
                IF (COMPARE_GLE(A(I), B(J))) THEN
                    A(K) = B(J)
                    J = J - 1_kIndex
                    IF (J < 0_kIndex) EXIT Merge_Upper
                ELSE
                    A(K) = A(I)
                    I = I - 1_kIndex
                    IF (I < 0_kIndex) THEN
                        A(0:K-1) = B(0:J)
                        EXIT Merge_Upper
                    END IF
                END IF
            END DO Merge_Upper
        END IF

        RETURN

    END SUBROUTINE Merging

    !******************************************************************

    SUBROUTINE Insert_Guarded(A, Lo, Hi)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort array using insertion sort algorithm
   
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(KeyOrdered), INTENT(INOUT) :: A(:)     ! array to be sorted
        tIndex,           INTENT(IN)    :: Lo       ! starting index (inclusive)
        tIndex,           INTENT(IN)    :: Hi       ! ending index (inclusive)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex              :: I, J, Nxch
        TYPE(KeyOrdered)    :: Temp
 
    !** FLOW:

        ! initialize
        Nxch = 0_kIndex
    
        ! put smallest element in position to serve as sentinel
        DO I = Hi, Lo+1_kIndex, -1_kIndex
            J = I - 1_kIndex
            IF (COMPARE_GLT(A(I), A(J))) THEN
                EXCHANGE(A, I, J)
                Nxch = Nxch + 1_kIndex
            END IF
        END DO
        IF (Nxch == 0_kIndex) RETURN

        ! insertion sort with half exchanges
        DO I = Lo+2_kIndex, Hi
            Temp = A(I)
            J = I - 1_kIndex
            DO WHILE (COMPARE_GLT(Temp, A(J)))
                A(J+1)  = A(J)
                J = J - 1_kIndex
            END DO
            A(J+1) = Temp
        END DO

        RETURN

    END SUBROUTINE Insert_Guarded

    !******************************************************************

    SUBROUTINE Reverse_Order_Base0(A, IStart, IEnd)

    !** PURPOSE OF THIS SUBROUTINE
        ! To reverse order of a segment of an array in place
   
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(KeyOrdered), INTENT(INOUT) :: A(0:)    ! array to be reverse-ordered
        tIndex,           INTENT(IN)    :: IStart   ! starting index (inclusive)
        tIndex,           INTENT(IN)    :: IEnd     ! ending index (inclusive)
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex              :: Lo
        tIndex              :: Hi
        TYPE(KeyOrdered)    :: Temp
 
    !** FLOW:

        Lo = IStart
        Hi = IEnd
        DO WHILE (Lo < Hi)
            EXCHANGE(A, Lo, Hi)
            Lo = Lo + 1_kIndex
            Hi = Hi - 1_kIndex
        END DO

        RETURN

    END SUBROUTINE Reverse_Order_Base0

    !******************************************************************

END SUBROUTINE

#undef COMPARE_GLT
#undef COMPARE_GLE

!******************************************************************************

END MODULE MClass_PQMinMax

!******************************************************************************
