
MODULE MClass_HashSet

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *HashSet* type and its supporting routines and data type.
!   The *HashSet* type is a collection type that employs an open-addressing hash table
!   implementation to provide common operations for an unordered set.  Like any other
!   set collection types, the *HashSet* type does not allow duplicated items. <br>
!   The *HashSet* type requires an explicit construction before using other provided
!   operations.  There are two methods provided to create a collection.  The *CreateEmpty*
!   method constructs an empty set with optional multiple arguments (including an initial
!   capacity, a load factor, a probing algorithm, and a hash function used to compute
!   a hash code of an item) whereas the *Construct* method constructs a set from an array
!   items.  As an unordered set, the *HashSet* type makes no guarantees as to the iteration
!   order of the set.  In particular, it does not guarantee that the order will remain
!   the same over time. <br>
!   The *HashSet* type uses the *KeyUnordered* type to store its set items.  Therefore, it
!   can be used to store items of any data type, except the *LOGICAL* type.  Like other
!   collection types, however, it must be employed to store items of only one particular
!   data type.  To store items of another data type, it must be destructed before inserting
!   items of different data type. <br>
!   Technically, the *HashSet* type employs the open-addressing as a collision resolution
!   technique where the hash resolution is performed through probing.  It provides three
!   probing algorithms: linear probing, quadratic probing and double hashing.  By default,
!   the linear probing algorithm is used.  However, a user can specify other probing
!   algorithm during the construction of the hash set. <br>

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define     tHash           tIndex

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MClass_CharBuffer
    USE MBase_ByteUtil,     ONLY: AnyType_GetByteSize
    USE MBase_SIntUtil,           ToChar => ToDecStrSigned
    USE MBase_MathUtil
#ifdef Indx32Bits
    USE MBase_SimpleHash32, ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64, ONLY: ComputeHash => Hash64_FNV1a
#endif
    USE MClass_Object,      ONLY: Object
    USE MClass_GenData
    USE MClass_MemoryPool
    USE MClass_KeyUnordered
    USE MClass_BaseCollection
    USE MClass_BaseIterable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: HashSet

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar,   PARAMETER  :: ModName = 'MClass_HashSet'
    ! status of the SetItem
    tCharLen(3), PARAMETER  :: STAT_DEL = 'DEL'
    tCharLen(3), PARAMETER  :: STAT_NUL = 'NUL'
    tCharLen(3), PARAMETER  :: STAT_GEN = 'GEN'
    ! default capacity
    tIndex,      PARAMETER  :: DefaultCapacity   = 7
    ! default load factor
    tRealDP,     PARAMETER  :: DefaultLoadFactor = 0.65_kDouble
    tSInt32,     PARAMETER  :: LinearProbing     = 1
    tSInt32,     PARAMETER  :: QuadraticProbing  = 2
    tSInt32,     PARAMETER  :: DoubleHashing     = 3
    ! This is the linear constant used in the linear probing, it can be
    ! any positive number. The table capacity will be adjusted so that
    ! the GCD(capacity, LinearConstant) = 1 so that all buckets can be probed.
    tIndex,      PARAMETER  :: LinearConstant    = 17_kIndex
    ! seed for computation of hash code
#ifdef Indx32Bits
    tSInt32,     PARAMETER  :: HashSeed = 313131_kInt32
#else
    tSInt64,     PARAMETER  :: HashSeed = 313131_kInt64
#endif
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,     PARAMETER  :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    !> *SetItem* is a data type containing the stored item as its component.
    TYPE, EXTENDS(Object)   :: SetItem
        tCharLen(3)         :: Stat = STAT_NUL  !! current status of the object
        TYPE(KeyUnordered)  :: Store            !! storage of item (or value)
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures for SetItem Type              -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: SetItem_SetData
        PROCEDURE, PRIVATE  :: SetItem_SetStore
        GENERIC             :: Set          => SetItem_SetData, SetItem_SetStore
        PROCEDURE           :: IsItemEqual  => SetItem_IsItemEqual
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type              -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => SetItem_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => SetItem_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => SetItem_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => SetItem_ToString
        ! ---------------------------------------------------------------------
        ! -----                     Overridden Procedure                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => SetItem_HashCode
        ! ---------------------------------------------------------------------
    END TYPE SetItem
    !> The *HashSet* type is a collection type that employs an open-addressing hash table
    !  implementation to provide common operations for an unordered set.  It makes no
    !  guarantees as to the iteration order of the set; in particular, it does not guarantee
    !  that the order will remain constant over time.
    TYPE, EXTENDS(BaseIterable) :: HashSet
        PRIVATE
        !% current capacity of the hash set
        tIndex                      :: Capacity     = DefaultCapacity
        !% working table items used to store items
        TYPE(SetItem), ALLOCATABLE  :: Items(:)
        !% current index into the working items (used for iteration purpose)
        tIndex                      :: Indx         = 0_kIndex
        !% the number of items not yet visited (used for iteration purpose)
        tIndex                      :: ItemLeft      = 0_kIndex
        !% current modification count (used for iteration purpose)
        tIndex                      :: IterModCount = 0_kIndex
        !% load factor
        tRealDP                     :: LoadFactor   = DefaultLoadFactor
        !% threshold for resizing
        tIndex                      :: Threshold    = 0_kIndex
        !% modification count
        tIndex                      :: ModCount     = 0_kIndex
        !% the total number of used buckets inside the hash set (including cells marked as deleted).
        tIndex                      :: UsedBuckets  = 0_kIndex
        !% the total number of unique items currently inside the hash set.
        tIndex                      :: ItemCount    = 0_kIndex
        !% probing algorithm
        tSInt32                     :: ProbAlgo     = LinearProbing
        !% index for double hashing
        tHash                       :: HashIndx     = 0_kIndex
        !> memory pool of stored items
        TYPE(MemoryPool)            :: ItemPool
        !> pointer to a hash function
        PROCEDURE(HashFunc),      NOPASS, POINTER   :: HashCalc => NULL()
        !> pointer to a procedure to copy stored data for a derived type not in the Object class;
        !  required if this type has allocatable/pointer component(s).
        PROCEDURE(IfacePolyCopy), NOPASS, POINTER   :: ItemCopy => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: FindItem <br>
        !  **Purpose**:  To find the specified item in the collection.  Return true if
        !                the specified item is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%FindItem(Item) <br>
        !   --->    IF (.NOT.Collection%FindItem(Item)) DoSomething
        PROCEDURE, PRIVATE  :: FindItem     => HashSet_FindItem
        !> **Type-Bound Subroutine**: Resize <br>
        !  **Purpose**:  To resize the collection to the specified capacity. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Resize(64)
        PROCEDURE, PRIVATE  :: Resize       => HashSet_Resize
        !> Use the *Construct* method to construct the collection from an array of items.
        PROCEDURE, PRIVATE  :: HashSet_CreateByArray
        ! ---------------------------------------------------------------------
        ! -----  Deferred/Overridden Procedures from BaseCollection Type  -----
        ! ---------------------------------------------------------------------
        !> Use the *Construct* method to construct the collection from another collection.
        PROCEDURE   :: CopyCollection   => HashSet_CopyCollection
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        PROCEDURE   :: Clear            => HashSet_ClearItems
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        PROCEDURE   :: Destruct         => HashSet_Destroy
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize          => HashSet_GetSize
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%IsEmpty() <br>
        !   --->    IF (.NOT.Collection%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty          => HashSet_IsEmpty
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseIterable Type        -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag indicating
        !                whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartFirst() <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstItem)
        PROCEDURE   :: StartFirst       => HashSet_Move2FirstItem
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next iteration and return a flag indicating whether
        !                the cursor pointer has reached the end of the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextItem) <br>
        !  **Note**: See the <a href="../module/mclass_baseiterable.html">BaseIterable</a> type
        !            for illustrations of an iteration over a collection using the *StartFirst*
        !            and *MoveForward* methods.<br>
        PROCEDURE   :: MoveForward      => HashSet_Move2NextItem
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified item to the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Insert(Item) <br>
        PROCEDURE   :: Insert           => HashSet_Insert
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete the current item from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the *StartFirst* and
        !       *MoveForward* methods.  Therefore, after the call to one of those methods and then
        !       calling this one will result in a removal of the current item of the iteration
        !       (i.e. the same item that can be retrieved via the *StartFirst* and *MoveForward*
        !       methods). <br>
        PROCEDURE   :: Delete           => HashSet_Delete
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the collection.  Also, return a flag
        !       indicating whether the items are successfully retrieved and removed or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Items) <br>
        !   --->    IF (.NOT.Collection%ToArray(Items)) DoSomething
        PROCEDURE   :: ToArray          => HashSet_ToArray
        !> **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the collection. Also, return
        !                a flag indicating whether the items are successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Items) <br>
        !   --->    IF (.NOT.Collection%GetAll(Items)) DoSomething
        PROCEDURE   :: GetAll           => HashSet_GetAll
        ! ---------------------------------------------------------------------
        ! -----                      Specific Procedures                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%CreateEmpty()                                     ! use default options  <br>
        !   --->    CALL Table%CreateEmpty(InitCap=25)                           ! specify initial capacity <br>
        !   --->    CALL Table%CreateEmpty(LoadFactor=0.5)                       ! specify load factor <br>
        !   --->    CALL Table%CreateEmpty(ProbAlgo=2)                           ! specify probing algorithm <br>
        !   --->    CALL Table%CreateEmpty(HashCal=Murmur3_Hash32_Opt)           ! specify hash function <br>
        !   --->    CALL Table%CreateEmpty(ItemCopy=CopyProc)                    ! specify copy procedure <br>
        !   --->    CALL Table%CreateEmpty(30, 0.75, 3, XX_Hash64_Opt, CopyProc) ! specify all options <br>
        !  **Note1**: Any suitable hash function routine from the *ModBase_SimpleHash32*, 
        !       *ModBase_SimpleHash64*, *ModBase_ReferenceHash32*, *ModBase_ReferenceHash64*
        !       *ModBase_OptimalHash32*, and *ModBase_OptimalHash64* modules can be used to
        !       specify the *HashCal* argument.  The term *suitable* means that any routine
        !       that has exactly the same interface as the *HashFunc* abstract function
        !       is the suitable one.  <br>
        !  **Note2**: Depending on a type of indices defined in the '*Macro - Basic Definitions.f90*'
        !       file, a 32-bit hash-function routine is a suitable one for 32-bit integer indices
        !       while a 64-bit hash-function routine is a suitable one for 64-bit integer indices.
        !       This is a compile-time choice.  <br>
        PROCEDURE   :: CreateEmpty  => HashSet_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new collection from an array of items or from another
        !                collection. <br>
        !  **Usage**: <br>
        !   ! create a collection from an array of 25 items <br>
        !   --->    CALL Collection%Construct(25, Arr) <br>
        !   ! create a collection and specify the optional arguments (see the *CreateEmpy* method) <br>
        !   --->    CALL Collection%Construct(25, Arr, LoadFactor, ProbAlgo, HashCalc, ItemCopy) <br>
        !   ! create a collection from another collection <br>
        !   --->    CALL Collection%Construct(OtherCollection)
        GENERIC     :: Construct    => HashSet_CreateByArray
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified item in the collection.  Return true if
        !                the specified item is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Contain(Item) <br>
        !   --->    IF (.NOT.Collection%Contain(Item)) DoSomething
        PROCEDURE   :: Contain      => HashSet_Contain
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified item from the collection.  Also, return a flag
        !                indicating whether the item is successfully removed or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Remove(Item) <br>
        !   --->    IF (.NOT.Collection%Remove(Item)) DoSomething
        PROCEDURE   :: Remove       => HashSet_Remove
        ! ---------------------------------------------------------------------
        ! -----     Deferred/Overridden Procedures from Object Type       -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => HashSet_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => HashSet_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => HashSet_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => HashSet_ToString
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => HashSet_HashCode
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the hash set.
        FINAL       :: HashSet_Finalize
        ! ---------------------------------------------------------------------
    END TYPE HashSet

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !----------------------------------------------------------------------
        !^ *HashFunc* is a generic interface for a procedure to compute the hash value.
        FUNCTION HashFunc(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            IMPORT
            TYPE(*),  CONTIGUOUS, INTENT(IN)    :: Input(..)    !! input (any type and rank)
            tIndex,               INTENT(IN)    :: InpSize      !! size of the input (in bytes)
            tHash,    OPTIONAL,   INTENT(IN)    :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,   INTENT(IN)    :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tHash                               :: HashCode     !! hash code
        END FUNCTION HashFunc
        !----------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! -----------------------------------------------------------------------------
! -----                       SetItem Procedures                          -----
! -----------------------------------------------------------------------------

SUBROUTINE SetItem_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the SetItem object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetItem),     INTENT(IN)  :: SrcObj   !! source object
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
    TYPE IS (SetItem)
        DstObj%Stat = SrcObj%Stat
        CALL SrcObj%Store%Copy(DstObj%Store, IsDeep)
    CLASS DEFAULT
        CALL Handle_ErrLevel('SetItem_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE SetItem_Copy

!******************************************************************************

FUNCTION SetItem_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetItem), INTENT(IN)  :: LhsObj   !! an object
    CLASS(Object),  INTENT(IN)  :: RhsObj   !! another object
    tLogical                    :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (SetItem)
        Flag = FalseVal
        IF (LhsObj%Stat /= RhsObj%Stat) RETURN
        Flag = LhsObj%Store%IsEqualTo(RhsObj%Store)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION SetItem_IsEqualTo

!******************************************************************************

SUBROUTINE SetItem_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of the SetItem object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetItem), INTENT(INOUT)   :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Obj%Stat = STAT_NUL
    CALL Obj%Store%MemFree()

    RETURN

END SUBROUTINE SetItem_MemFree

!******************************************************************************

FUNCTION SetItem_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetItem), INTENT(IN)  :: Obj
    tCharAlloc                  :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Str = '{SetItem : ' // Obj%Store%ToString() // '}'

    RETURN

END FUNCTION SetItem_ToString

!******************************************************************************

FUNCTION SetItem_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetItem), INTENT(IN)  :: Obj
    tIndex                      :: Code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Code = Obj%Store%HashCode()
    
    RETURN

END FUNCTION SetItem_HashCode

!******************************************************************************

SUBROUTINE SetItem_SetData(This, Status, Pool, Item)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set data of the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetItem),             INTENT(INOUT)   :: This
    tCharLen(3),                INTENT(IN)      :: Status
    TYPE(MemoryPool), OPTIONAL, INTENT(INOUT)   :: Pool
    CLASS(*),         OPTIONAL, INTENT(IN)      :: Item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    This%Stat = Status
    IF (PRESENT(Item).AND.PRESENT(Pool)) CALL This%Store%Set(Item, Pool)

    RETURN

END SUBROUTINE SetItem_SetData

!******************************************************************************

SUBROUTINE SetItem_SetStore(This, Status, Other)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set data of the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetItem), INTENT(INOUT)   :: This
    tCharLen(3),    INTENT(IN)      :: Status
    TYPE(SetItem),  INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    This%Stat = Status
    CALL Other%Store%Copy(This%Store)

    RETURN

END SUBROUTINE SetItem_SetStore

!******************************************************************************

FUNCTION SetItem_IsItemEqual(This, Item, Pool) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To check whether the specified item is equal to the stored item.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SetItem),   INTENT(IN)    :: This
    CLASS(*),         INTENT(IN)    :: Item
    TYPE(MemoryPool), INTENT(INOUT) :: Pool
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(KeyUnordered)  :: Other

!** FLOW:

    CALL Other%Set(Item, Pool)
    Flag = This%Store%IsEqualTo(Other)
    CALL Other%MemFree()

    RETURN

END FUNCTION SetItem_IsItemEqual

! ---------------------------------------------------------------------
! -----     Deferred/Overridden Procedures from Object Type       -----
! ---------------------------------------------------------------------

SUBROUTINE HashSet_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet),     INTENT(IN)  :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
    !^ Flag indicating whether to perform deep copy or shallow copy. <br>
    !  - If present and true, perform a deep copy. <br>
    !  - If present and false, perform a shallow copy. <br>
    !  - If not present, perform either a shallow or a deep copy that is naturally most
    !    suitable for the object's components.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT TYPE (DstObj)
    TYPE IS (HashSet)
        DstObj%Capacity     =  SrcObj%Capacity
        DstObj%Indx         =  SrcObj%Indx
        DstObj%ItemLeft     =  SrcObj%ItemLeft
        DstObj%IterModCount =  SrcObj%IterModCount
        DstObj%LoadFactor   =  SrcObj%LoadFactor
        DstObj%Threshold    =  SrcObj%Threshold
        DstObj%ModCount     =  SrcObj%ModCount
        DstObj%UsedBuckets  =  SrcObj%UsedBuckets
        DstObj%ItemCount    =  SrcObj%ItemCount
        DstObj%ProbAlgo     =  SrcObj%ProbAlgo
        DstObj%HashIndx     =  SrcObj%HashIndx
        DstObj%HashCalc     => SrcObj%HashCalc
        DstObj%ItemCopy     => SrcObj%ItemCopy
        ALLOCATE(DstObj%Items, SOURCE=SrcObj%Items)
        CALL SrcObj%ItemPool%CloneTo(DstObj%ItemPool)
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('HashSet_Copy', ModName, ErrSevere, &
                             'Type of the DstObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE HashSet_Copy

!******************************************************************************

FUNCTION HashSet_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(IN)  :: LhsObj   !! an object
    CLASS(Object),  INTENT(IN)  :: RhsObj   !! another object
    tLogical                    :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    TYPE IS (HashSet)
        Flag = FalseVal
        IF (LhsObj%Capacity /= RhsObj%Capacity) RETURN
        IF (LhsObj%ItemCount /= RhsObj%ItemCount) RETURN
        IF (.NOT.LhsObj%IsEmpty()) THEN
            BLOCK
                tIndex  :: I
                DO I = 1_kIndex, SIZE(LhsObj%Items)
                    IF (.NOT.LhsObj%Items(I)%IsEqualTo(RhsObj%Items(I))) RETURN
                END DO
            END BLOCK
        END IF
        Flag = TrueVal
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION HashSet_IsEqualTo

!******************************************************************************

SUBROUTINE HashSet_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the HashSet object.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(INOUT)  :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! first free items' components and then free of the items themselves
    IF (ALLOCATED(Obj%Items)) THEN
        DO I = 0_kIndex, Obj%Capacity-1_kIndex
            CALL Obj%Items(I)%MemFree()
        END DO
        DEALLOCATE(Obj%Items)
    END IF

    ! reset all components
    Obj%Capacity = DefaultCapacity
    Obj%Indx = 0_kIndex
    Obj%ItemLeft = 0_kIndex
    Obj%IterModCount = 0_kIndex
    Obj%LoadFactor = DefaultLoadFactor
    Obj%Threshold = 0_kIndex
    Obj%ModCount = 0_kIndex
    Obj%UsedBuckets = 0_kIndex
    Obj%ItemCount = 0_kIndex
    Obj%ProbAlgo = LinearProbing
    Obj%HashIndx = 0_kIndex
    NULLIFY(Obj%HashCalc)
    NULLIFY(Obj%ItemCopy)
    CALL Obj%FreeMold()
    CALL Obj%ItemPool%Destruct()

    RETURN

END SUBROUTINE HashSet_MemFree

!******************************************************************************

FUNCTION HashSet_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the HashSet type.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(IN)  :: Obj
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
            tCharAlloc          :: ItemStr
            ! initialize
            CALL ChrBuf%CreateEmpty(InitCap=Obj%GetSize()*40_kIndex)
            CALL ChrBuf%Append('[')
            Count = 0_kIndex
            DO I = 1_kIndex, SIZE(Obj%Items)
                ! skip if the item is empty
                IF (Obj%Items(I)%Stat /= STAT_GEN) CYCLE
                ! add the string representation of the current item
                ItemStr = Obj%Items(I)%Store%ToString()
                CALL ChrBuf%Append(ItemStr(17:LEN(ItemStr)-1))
                ! update Count and add comma between items if needed
                Count = Count + 1_kIndex
                IF (Count < Obj%GetSize()) THEN
                    CALL ChrBuf%Append(', ')
                ELSEIF (Count > Obj%GetSize()) THEN
                    EXIT
                END IF
            END DO
            CALL ChrBuf%Append(']')
            BaseStr = ChrBuf%AsString()
        END BLOCK
    END IF
    Str = '{HashSet with ' // ToChar(Obj%GetSize()) // ' items : ' // BaseStr // '}'

    RETURN

END FUNCTION HashSet_ToString

!******************************************************************************

FUNCTION HashSet_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(IN)  :: Obj
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
            DO I = 1_kIndex, SIZE(Obj%Items)
                IF (Obj%Items(I)%Stat /= STAT_GEN) CYCLE
                Code = Code + Obj%Items(I)%Store%HashCode()
            END DO
        END BLOCK
    END IF

    RETURN

END FUNCTION HashSet_HashCode

! ---------------------------------------------------------------------
! -----  Deferred/Overridden Procedures from BaseCollection Type  -----
! ---------------------------------------------------------------------

SUBROUTINE HashSet_CopyCollection(This, Other, ItemCopy, ValCopy)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items as the given collection (Other).
    !  This is a deferred procedure by the *BaseCollection* class. <br>
    !  *Note*:  Other must be in the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(HashSet),        INTENT(INOUT)    :: This
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
    CLASS IS (HashSet)
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
            ALLOCATE(Item, MOLD=MoldPtr)
            ! loop through the other collection and get items along the way
            IsTheEnd = Other%StartFirst(Item, ItemCopy)
            DO WHILE (.NOT.IsTheEnd)
                ! add an item to this collection
                CALL This%Insert(Item)
                IsTheEnd = Other%MoveForward(Item, ItemCopy)
            END DO
            NULLIFY(MoldPtr)
            DEALLOCATE(Item)
        END BLOCK
    CLASS DEFAULT
        CALL Handle_ErrLevel('HashSet_CopyCollection', ModName, ErrSevere, &
                             'Type of "Other" must be in the "BaseIterable" class.')
    END SELECT
    
    RETURN

END SUBROUTINE HashSet_CopyCollection

!******************************************************************************

SUBROUTINE HashSet_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To free components of the items from the set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(INOUT)   :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

!** FLOW:

    IF (ALLOCATED(Collection%Items)) THEN
        DO I = 0_kIndex, Collection%Capacity-1_kIndex
            CALL Collection%Items(I)%MemFree()
        END DO
    END IF
    Collection%Indx = 0_kIndex
    Collection%ItemLeft = 0_kIndex
    Collection%IterModCount = 0_kIndex
    Collection%Threshold = 0_kIndex
    Collection%ItemCount = 0_kIndex
    Collection%UsedBuckets = 0_kIndex
    Collection%ModCount = Collection%ModCount + 1_kIndex
    CALL Collection%FreeMold()
    NULLIFY(Collection%ItemCopy)

    RETURN

END SUBROUTINE HashSet_ClearItems

!******************************************************************************

SUBROUTINE HashSet_Destroy(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(INOUT)   :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    CALL Collection%MemFree()

    RETURN

END SUBROUTINE HashSet_Destroy

!******************************************************************************

FUNCTION HashSet_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the number of items currently in the hash set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(IN)  :: Collection   !! collection
    tIndex                      :: Size         !! the number of items

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%ItemCount

    RETURN

END FUNCTION HashSet_GetSize

!******************************************************************************

FUNCTION HashSet_IsEmpty(Collection) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the hash set is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(IN)  :: Collection   !! collection
    tLogical                    :: Flag         !! true if the set is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = (Collection%ItemCount == 0_kIndex)

    RETURN

END FUNCTION HashSet_IsEmpty

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseIterable Type         -----
! ---------------------------------------------------------------------

FUNCTION HashSet_Move2FirstItem(Collection, Item, ItemCopy) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the first (starting) pair data in a collection.   For the hash set, which is
    !  an unordered set, the starting pair is the first pair found in the non-empty bucket.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet),     INTENT(INOUT)   :: Collection    !! collection
    !% the first item as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Item
    !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
    !> a flag indicating whether the table contains no pair data or not <br>
    ! - true if the table is empty. <br>
    ! - otherwise the first pair data is available.
    tLogical                            :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Collection%IsEmpty()) THEN
        IsEmpty = TrueVal
        RETURN
    ELSE
        IsEmpty = FalseVal
    END IF

    ! initialize iteration-related components
    Collection%Indx = 0_kIndex
    Collection%ItemLeft = Collection%ItemCount
    Collection%IterModCount = Collection%ModCount

    ! start iteration by looking for the first non-empty slot
    DO WHILE (Collection%Items(Collection%Indx)%Stat /= STAT_GEN)
        Collection%Indx = Collection%Indx + 1_kIndex
    END DO

    ! update ItemLelf
    Collection%ItemLeft = Collection%ItemLeft - 1_kIndex

    ! get item if requested
    IF (PRESENT(Item).AND.PRESENT(ItemCopy)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%Store%Get(Item, ItemCopy)) THEN
            CALL Handle_ErrLevel('HashSet_Move2FirstItem', ModName, ErrWarning, &
                    'Type of the specified item is likely NOT the same as that of stored items.')
        END IF
    ELSEIF (PRESENT(Item)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%Store%Get(Item, Collection%ItemCopy)) THEN
            CALL Handle_ErrLevel('HashSet_Move2FirstItem', ModName, ErrWarning, &
                    'Type of the specified item is likely NOT the same as that of stored items.')
        END IF
    END IF

    RETURN

END FUNCTION HashSet_Move2FirstItem

!******************************************************************************

FUNCTION HashSet_Move2NextItem(Collection, Item, ItemCopy) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next pair data in a collection.  For the hash set, which is an unordered set,
    !  the next pair is a pair inserted in the first non-empty bucket after the previous one.  <br>
    !  The routine will report an error if an alteration to stored item(s) (either by an insertion
    !  or a removal) has been occurred during current iteration.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet),     INTENT(INOUT)   :: Collection    !! collection
    !% the next item as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Item
    !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
    !> a flag indicating whether the move to the end of the table occurs or not <br>
    ! - true if next pair data is NOT available. <br>
    ! - otherwise next pair data is available.
    tLogical                            :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Collection%IterModCount /= Collection%ModCount) THEN
        CALL Handle_ErrLevel('HashSet_Move2NextItem', ModName, ErrWarning, &
                 "Must re-start the iteration because the stored items have been altered.")
        RETURN
    END IF

    ! check for empty table
    IF (Collection%IsEmpty()) THEN
        IsTheEnd = TrueVal
        RETURN
    ELSEIF (Collection%ItemLeft == 0_kIndex) THEN
        IsTheEnd = TrueVal
        RETURN
    END IF

    ! update Indx and set flag
    Collection%Indx = Collection%Indx + 1
    IsTheEnd = FalseVal

    ! start iteration by looking for the next non-empty slot
    DO WHILE (Collection%Items(Collection%Indx)%Stat /= STAT_GEN)
        Collection%Indx = Collection%Indx + 1_kIndex
    END DO

    ! update ItemLelf
    Collection%ItemLeft = Collection%ItemLeft - 1_kIndex

    ! get item if requested
    IF (PRESENT(Item).AND.PRESENT(ItemCopy)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%Store%Get(Item, ItemCopy)) THEN
            CALL Handle_ErrLevel('HashSet_Move2NextItem', ModName, ErrWarning, &
                    'Type of the specified item is likely NOT the same as that of stored items.')
        END IF
    ELSEIF (PRESENT(Item)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%Store%Get(Item, Collection%ItemCopy)) THEN
            CALL Handle_ErrLevel('HashSet_Move2NextItem', ModName, ErrWarning, &
                    'Type of the specified item is likely NOT the same as that of stored items.')
        END IF
    END IF

    RETURN

END FUNCTION HashSet_Move2NextItem

!******************************************************************************

SUBROUTINE HashSet_Insert(Collection, Item)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add an item into the hash set.  If the specified item is already stored
    !  in the set, report severe error and return immediately.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(INOUT)   :: Collection   !! collection
    CLASS(*),       INTENT(IN)      :: Item         !! item to be inserted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tHash       :: HashCode
    tIndex      :: Indx, Offset, I, J
    tIndex      :: ItemSize

! FLOW

    ! check the specified item
    IF (.NOT.IsKeyUnordered(Item)) THEN
        CALL Handle_ErrLevel('HashSet_Insert', ModName, ErrSevere, &
                'Type of the specified item is NOT valid.')
        RETURN
    ELSEIF (.NOT.Collection%IsItemValid(Item)) THEN
        CALL Handle_ErrLevel('HashSet_Insert', ModName, ErrSevere, &
                'Only items of the same type are allowed in a collection.')
        RETURN
    END IF

    IF (.NOT.ALLOCATED(Collection%Items)) THEN
        ! not constructed yet so check type of the specified item
        IF (GetDataType(Item) == TYPE_DERIVED) THEN
            CALL Collection%FreeMold()
            CALL Handle_ErrLevel('HashSet_Insert', ModName, ErrSevere, &
                    'The collection must be explicitly constructed before using any other method(s).')
            RETURN
        END IF
        ! construct empty collection with default options
        CALL Collection%CreateEmpty()
    ELSEIF (Collection%FindItem(Item)) THEN
        ! no duplicated items are allowed.
        CALL Handle_ErrLevel('HashSet_Insert', ModName, ErrSevere, &
                'The specified item is already stored in the set.')
        RETURN
    END IF

    ! resize the capacity if needed
    IF (Collection%UsedBuckets >= Collection%Threshold) CALL Collection%Resize(MoreCap=TrueVal)

    ! set up the probing if needed
    CALL SetupProbing(Collection, Item)

    ! compute the hash code and offset
    ItemSize = AnyType_GetByteSize(Item)
    HashCode = Collection%HashCalc(Item, ItemSize, HashSeed, RemoveSign=TrueVal)
    Offset   = ComputeIndex(HashCode, Collection%Capacity)

    ! initialize indices
    I = Offset
    J = -1_kIndex
    Indx = 1_kIndex
    DO
        IF (Collection%Items(I)%Stat == STAT_DEL) THEN
            ! The current bucket was previously deleted
            IF (J == -1_kIndex) J = I
        ELSEIF (Collection%Items(I)%Stat == STAT_NUL) THEN
            ! The current bucket is null so an insertion/update can occur
            IF (J == -1_kIndex) THEN
                ! No previously encountered deleted buckets
                Collection%UsedBuckets = Collection%UsedBuckets + 1_kIndex
                CALL Collection%Items(I)%Set(STAT_GEN, Collection%ItemPool, Item)
            ELSE
                ! Previously seen deleted bucket. Instead of inserting
                ! the new element at i where the null element is insert
                ! it where the deleted token was found.
                CALL Collection%Items(J)%Set(STAT_GEN, Collection%ItemPool, Item)
            END IF
            Collection%ItemCount = Collection%ItemCount + 1_kIndex
            Collection%ModCount = Collection%ModCount + 1_kIndex
            EXIT
        END IF
        I = ComputeIndex(Offset + Probe(Collection, Indx), Collection%Capacity)
        Indx = Indx + 1_kIndex
    END DO

    RETURN

END SUBROUTINE HashSet_Insert

!******************************************************************************

SUBROUTINE HashSet_Delete(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete a item of the current iteration from a collection.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashSet), INTENT(INOUT)   :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Collection%ItemCount = Collection%ItemCount - 1_kIndex
    Collection%ModCount  = Collection%ModCount + 1_kIndex
    CALL Collection%Items(Collection%Indx)%Set(STAT_DEL)

    RETURN

END SUBROUTINE HashSet_Delete

!**************************************************************************************

FUNCTION HashSet_ToArray(Collection, Items, ItemCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove all items from the collection.  Also, return
    !  a flag indicating whether the items are successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashSet object
    CLASS(HashSet),      INTENT(INOUT)  :: Collection
    !% the item to be removed from the collection
    CLASS(*),            INTENT(INOUT)  :: Items(:)
    !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
    !> flag indicating whether the items are successfully retrieved and removed. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! retrieve all items
    Success = Collection%GetAll(Items, ItemCopy)
    
    ! remove all items
    IF (Success) CALL Collection%Clear()

    RETURN

END FUNCTION HashSet_ToArray

!**************************************************************************************

FUNCTION HashSet_GetAll(Collection, Items, ItemCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all items (without removing them) from the collection.  Also,
    !  return a flag indicating whether the items are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashSet object
    CLASS(HashSet),      INTENT(INOUT)  :: Collection
    !% the item to be removed from the collection
    CLASS(*),            INTENT(INOUT)  :: Items(1:)
    !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
    !> flag indicating whether the items are successfully retrieved. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                              :: I, J, ArrSize
    PROCEDURE(IfacePolyCopy), POINTER   :: CopyProc

! FLOW

    ! check whether the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        Success = FalseVal
        RETURN
    ELSE
        ! check whether type of the specified items is valid or not
        IF (Collection%IsItemValid(Items(1))) THEN
            Success = TrueVal
        ELSE
            Success = FalseVal
            CALL Handle_ErrLevel('HashSet_GetAll', ModName, ErrSevere, &
                                 'Type of the specified items is NOT the same as that of stored items.')
            RETURN
        END IF
    END IF

    ! set a pointer to copy procedure
    IF (PRESENT(ItemCopy)) THEN
        CopyProc => ItemCopy
    ELSE
        CopyProc => Collection%ItemCopy
    END IF

    ! initialize local variables
    ArrSize = SIZE(Items, KIND=kIndex)
    I = 1_kindex

    ! loop through all buckets
    DO J = 1_kIndex, SIZE(Collection%Items, KIND=kIndex)
        ! skip the rest if the bucket is empty
        IF (Collection%Items(J)%Stat /= STAT_GEN) CYCLE
        IF (.NOT.Collection%Items(J)%Store%Get(Items(I), CopyProc)) THEN
            CALL Handle_ErrLevel('HashSet_GetAll', ModName, ErrSevere, &
                    'Unable to retrieve an item.  This is likely a bug.')
            Success = FalseVal
            EXIT
        END IF
        ! update I
        I = I + 1_kIndex
        ! if we got all items requested, quit the loop
        IF (I > ArrSize) EXIT
    END DO

    ! free pointer
    NULLIFY(CopyProc)

    RETURN

END FUNCTION HashSet_GetAll

! -----------------------------------------------------------------------------
! -----                        Common Procedures                          -----
! -----------------------------------------------------------------------------

SUBROUTINE HashSet_Resize(Collection, MoreCap)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To resize the capacity of the hash set according the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(INOUT)   :: Collection   !! collection
    tLogical,       INTENT(IN)      :: MoreCap
    !^ true if increasing the capacity; otherwise, decreasing the capacity

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SetItem), ALLOCATABLE  :: OldItems(:)
    tIndex                      :: OldCap, I
    tLogical                    :: Success
    CLASS(*), POINTER           :: ItemMold
    CLASS(*), ALLOCATABLE       :: Item

!** FLOW:

    OldCap = Collection%Capacity
    IF (MoreCap) THEN
        CALL IncreaseCapacity(Collection)
    ELSE
        ! halving the capacity
        Collection%Capacity = Collection%Capacity/2_kIndex
    END IF
    CALL AdjustCapacity(Collection)

    ! update threshold
    Collection%Threshold = ToIndex(Collection%Capacity*Collection%LoadFactor)

    ! move currently stored objects to temporary variable
    CALL MOVE_ALLOC(Collection%Items, OldItems)

    ! allocate working items to new capacity
    ALLOCATE(Collection%Items(0:Collection%Capacity-1_kIndex))

    ! set status to null
    Collection%Items(:)%Stat = STAT_NUL

    ! Reset the item count and buckets used since we are about to
    ! re-insert all the items into the hash-table.
    Collection%ItemCount = 0_kIndex
    Collection%UsedBuckets = 0_kIndex

    ! loop over the temporary variable (OldItems) to move stored objects (Items)
    ! back to the hash set
    ItemMold => Collection%GetItemPtr()
    ALLOCATE(Item, MOLD=ItemMold)
    DO I = 0_kIndex, OldCap-1_kIndex
        IF (OldItems(I)%Stat == STAT_GEN) THEN
            ! get an item back
            Success = OldItems(I)%Store%Get(Item, Collection%ItemCopy)
            ! re-insert the item
            IF (Success) THEN
                CALL Collection%Insert(Item)
            ELSE
                CALL Handle_ErrLevel('HashSet_Resize', ModName, ErrSevere, &
                    'Unable to get an old item.  This is likely due to invalid copy procedure.')
                EXIT
            END IF
        END IF
        CALL OldItems(I)%MemFree()
    END DO

    ! free local variables
    DEALLOCATE(OldItems)
    NULLIFY(ItemMold)
    DEALLOCATE(Item)

    RETURN

END SUBROUTINE HashSet_Resize

!******************************************************************************

FUNCTION HashSet_FindItem(Collection, Item) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified item is currently stored in a hash set or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet),     INTENT(INOUT)   :: Collection   !! collection
    CLASS(*),           INTENT(IN)      :: Item         !! item to be looked for
    !> flag indicating whether the specified item is found or not.
    tLogical                            :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tHash       :: HashCode
    tIndex      :: Indx, Offset, I, J
    tIndex      :: ItemSize

! FLOW

    ! set up the probing if needed
    CALL SetupProbing(Collection, Item)

    ! compute the hash code and offset
    ItemSize  = AnyType_GetByteSize(Item)
    HashCode = Collection%HashCalc(Item, ItemSize, HashSeed, RemoveSign=TrueVal)
    Offset   = ComputeIndex(HashCode, Collection%Capacity)

    ! initialize indices
    I = Offset
    J = -1_kIndex
    Indx = 1_kIndex
    ! Start at the original hash value and probe until we find a spot where our item
    ! is or hit a null element in which case our element does not exist.
    DO
        IF (Collection%Items(I)%Stat == STAT_DEL) THEN
            ! Ignore deleted buckets, but record where the first index
            ! of a deleted bucket is found to perform lazy relocation later.
            IF (J == -1_kIndex) J = I
        ELSEIF (Collection%Items(I)%Stat /= STAT_NUL) THEN
            ! found the item we want to remove
            IF (J /= -1_kIndex) THEN
                ! If J /= -1 this means we previously encountered a deleted cell.
                ! We can perform an optimization by swapping the entries in cells
                ! I and J so that the next time we search for this item it will be
                ! found faster. This is called lazy deletion/relocation.
                CALL Collection%Items(J)%Set(STAT_GEN, Collection%Items(I))
                CALL Collection%Items(I)%Set(STAT_DEL)
            END IF
            Found = TrueVal
            EXIT
        ELSE
            ! the item was not found
            Found = FalseVal
            EXIT
        END IF
        I = ComputeIndex(Offset + Probe(Collection, Indx), Collection%Capacity)
        Indx = Indx + 1_kIndex
    END DO

    RETURN

END FUNCTION HashSet_FindItem

!******************************************************************************

SUBROUTINE HashSet_CreateEmpty(Collection, InitCap, LoadFactor, ProbAlgo, HashCalc, ItemCopy)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To create an empty hash set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet),      INTENT(INOUT)  :: Collection   !! collection
    tIndex,  OPTIONAL,   INTENT(IN)     :: InitCap      !! initial capacity of the hash set
    tRealDP, OPTIONAL,   INTENT(IN)     :: LoadFactor   !! load factor
    !% probing algorithm (1 = Linear, 2 = Quadratic, 3 = Double Hashing)
    tSInt32, OPTIONAL,   INTENT(IN)     :: ProbAlgo
    !> hash function to compute the hash value of the item; if not present, use default one.
    PROCEDURE(HashFunc),      OPTIONAL  :: HashCalc
    !> a procedure to copy stored data for a derived type not in the Object class;
    !  required if the type of items to be stored has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! determine initial capacity
    IF (PRESENT(InitCap)) THEN
        IF (InitCap < 0_kIndex) THEN
            Collection%Capacity = DefaultCapacity
        ELSE
            Collection%Capacity = MAX(DefaultCapacity, InitCap)
        END IF
    ELSE
        Collection%Capacity = DefaultCapacity
    END IF

    ! determine load factor
    IF (PRESENT(LoadFactor)) THEN
        IF (InitCap < 0.0_kDouble) THEN
            Collection%LoadFactor = DefaultLoadFactor
        ELSE
            Collection%LoadFactor = LoadFactor
        END IF
    ELSE
        Collection%LoadFactor = DefaultLoadFactor
    END IF

    ! determine probing algorithm
    IF (PRESENT(ProbAlgo)) THEN
        SELECT CASE (ProbAlgo)
        CASE (1:3)
            Collection%ProbAlgo = ProbAlgo
        CASE DEFAULT
            Collection%ProbAlgo = LinearProbing
        END SELECT
    ELSE
        Collection%ProbAlgo = LinearProbing
    END IF

    ! set hash function pointer
    IF (PRESENT(HashCalc)) THEN
        ! use supplied function
        Collection%HashCalc => HashCalc
    ELSE
        ! use default algorithm
        Collection%HashCalc => ComputeHash
    END IF

    ! set pointer to copy procedure
    IF (PRESENT(ItemCopy)) THEN
        Collection%ItemCopy => ItemCopy
    ELSE
        Collection%ItemCopy => NULL()
    END IF

    ! adjust the capacity according to the probing algorithm
    CALL AdjustCapacity(Collection)

    ! compute threshold
    Collection%Threshold = ToIndex(Collection%Capacity*Collection%LoadFactor)

    ! allocate memory of item storages
    ALLOCATE(Collection%Items(0:Collection%Capacity-1))

    ! set status to null
    Collection%Items(:)%Stat = STAT_NUL

    ! construct items' pool
    CALL Collection%ItemPool%Construct()

    RETURN

END SUBROUTINE HashSet_CreateEmpty

!******************************************************************************

SUBROUTINE HashSet_CreateByArray(Collection, N, Items, LoadFactor, ProbAlgo, HashCalc, ItemCopy)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a table from an array of items.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet),      INTENT(INOUT)  :: Collection   !! collection
    tIndex,              INTENT(IN)     :: N            !! number of items
    CLASS(*),            INTENT(IN)     :: Items(N)     !! the items to be added to the set
    tRealDP, OPTIONAL,   INTENT(IN)     :: LoadFactor   !! load factor
    !% probing algorithm (1 = Linear, 2 = Quadratic, 3 = Double Hashing)
    tSInt32, OPTIONAL,   INTENT(IN)     :: ProbAlgo
    !> hash function to compute the hash value of the item; if not present, use default one.
    PROCEDURE(HashFunc),      OPTIONAL  :: HashCalc
    !> a procedure to copy stored data for a derived type not in the Object class; required if
    !  the type of specified items to be stored has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! create empty collection with capacity twice of the item size
    CALL Collection%CreateEmpty(N*2_kIndex, LoadFactor, ProbAlgo, HashCalc, ItemCopy)

    ! add items to the set
    DO I = 1_kIndex, N
        CALL Collection%Insert(Items(I))
    END DO

    RETURN

END SUBROUTINE HashSet_CreateByArray

!******************************************************************************

SUBROUTINE HashSet_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To perform finalization of the collection.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(HashSet), INTENT(INOUT)    :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE HashSet_Finalize

!******************************************************************************

FUNCTION HashSet_Remove(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the specified item (and its associated value) from a collection.  Also,
    !  return a flag indicating whether the item is successfully removed or not. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(INOUT)   :: Collection   !! collection
    CLASS(*),       INTENT(IN)      :: Item         !! item to be removed
    !> flag indicating whether the specified item and its associated
    !  value are successfully removed or not.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tHash       :: HashCode
    tIndex      :: Indx, Offset, I
    tIndex      :: ItemSize

! FLOW

    ! return quickly if possible
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSEIF (.NOT.Collection%IsItemValid(Item)) THEN
        Flag = FalseVal
        CALL Handle_ErrLevel('HashSet_Remove', ModName, ErrSevere, &
                'Type of the specified item is NOT the same as that of stored items.')
        RETURN
    END IF

    ! set up the probing if needed
    CALL SetupProbing(Collection, Item)

    ! compute the hash code and offset
    ItemSize = AnyType_GetByteSize(Item)
    HashCode = Collection%HashCalc(Item, ItemSize, HashSeed, RemoveSign=TrueVal)
    Offset   = ComputeIndex(HashCode, Collection%Capacity)

    ! initialize indices
    I = Offset
    Indx = 1_kIndex
    ! Starting at the original hash probe until we find a spot where our item is
    ! or we hit a null element in which case our element does not exist.
    DO
        IF (Collection%Items(I)%Stat == STAT_DEL) THEN
            ! ignore deleted bucket so do nothing here
        ELSEIF (Collection%Items(I)%Stat == STAT_NUL) THEN
            ! the item was not found
            Flag = FalseVal
            EXIT
        ELSEIF (Collection%Items(I)%IsItemEqual(Item, Collection%ItemPool)) THEN
            ! found the item we want to remove
            Collection%ItemCount = Collection%ItemCount - 1_kIndex
            Collection%ModCount  = Collection%ModCount + 1_kIndex
            CALL Collection%Items(I)%Set(STAT_DEL)
            Flag = TrueVal
            EXIT
        END IF
        I = ComputeIndex(Offset + Probe(Collection, Indx), Collection%Capacity)
        Indx = Indx + 1_kIndex
    END DO

    ! halve the hash set capacity if it is 12.5% full or less
    IF ((Collection%ItemCount > 0_kIndex).AND.(Collection%ItemCount <= Collection%Capacity/8)) THEN
        CALL Collection%Resize(MoreCap=FalseVal)
    END IF

    RETURN

END FUNCTION HashSet_Remove

!******************************************************************************

FUNCTION HashSet_Contain(Collection, Item) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified item is currently stored in a collection.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(INOUT)   :: Collection   !! collection
    CLASS(*),       INTENT(IN)      :: Item         !! item to be looked for
    !% flag indicating whether the specified item is found or not.
    tLogical                        :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Collection%IsEmpty()) THEN
        Found = FalseVal
    ELSEIF (.NOT.Collection%IsItemValid(Item)) THEN
        Found = FalseVal
        CALL Handle_ErrLevel('HashSet_Contain', ModName, ErrSevere, &
                'Type of the specified item is NOT the same as that of stored items.')
        RETURN
    ELSE
        ! find the item
        Found = Collection%FindItem(Item)
    END IF

    RETURN

END FUNCTION HashSet_Contain

! -----------------------------------------------------------------------------
! -----             Procedures Dependent On Probing Algorithm             -----
! -----------------------------------------------------------------------------

SUBROUTINE SetupProbing(Collection, Key)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set up the probing according to the probing algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(INOUT)   :: Collection   !! collection
    CLASS(*),       INTENT(IN)      :: Key          !! key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Collection%ProbAlgo)
    CASE (LinearProbing)
        ! no setup required
    CASE (QuadraticProbing)
        ! no setup required
    CASE (DoubleHashing)
        BLOCK
            tIndex  :: HashCode
            tIndex  :: KeySize
            ! Cache second hash value.
            KeySize  = AnyType_GetByteSize(Key)
            HashCode = Collection%HashCalc(Key, KeySize, HashSeed, RemoveSign=TrueVal)
            Collection%HashIndx = ComputeIndex(HashCode, Collection%Capacity)
            ! Fail safe to avoid infinite loop
            IF (Collection%HashIndx == 0_kIndex) Collection%HashIndx = 1_kIndex
        END BLOCK
    END SELECT

    RETURN

END SUBROUTINE SetupProbing

!******************************************************************************

FUNCTION Probe(Collection, IdIn) RESULT(IdOut)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To look for the next available bucket according to the probing algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(INOUT)   :: Collection    !! collection
    tIndex,         INTENT(IN)      :: IdIn     !! starting index for the probing
    tIndex                          :: IdOut    !! index of available bucket

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Collection%ProbAlgo)
    CASE (LinearProbing)
        IdOut = LinearConstant*IdIn
    CASE (QuadraticProbing)
        ! Quadratic probing function (x**2 + x) / 2
        IdOut = SHIFTR(IdIn*IdIn + IdIn, 1)
    CASE (DoubleHashing)
        IdOut = Collection%HashIndx*IdIn
    END SELECT

    RETURN

END FUNCTION Probe

!******************************************************************************

SUBROUTINE AdjustCapacity(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To adjust capacity according to the probing algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(INOUT)   :: Collection    !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Collection%ProbAlgo)
    CASE (LinearProbing)
        ! Adjust the capacity so that the linear constant and
        ! the set capacity are relatively prime.
        DO WHILE (ComputeGCD(LinearConstant, Collection%Capacity) /= 1_kIndex)
            Collection%Capacity = Collection%Capacity + 1_kIndex
        END DO
    CASE (QuadraticProbing)
        ! Adjust the capacity of the hash set to be a power of two.
        BLOCK
            tIndex      :: Pow2
            Pow2 = HighestOneBit(Collection%Capacity)
            IF (Collection%Capacity /= Pow2 ) THEN
                CALL IncreaseCapacity(Collection)
            END IF
        END BLOCK
    CASE (DoubleHashing)
        ! Adjust the capacity until it is a prime number. The reason for
        ! doing this is to help ensure that the GCD(hash, capacity) = 1 when
        ! probing so that all the cells can be reached.
        IF (.NOT.IsPrime(Collection%Capacity)) THEN
            Collection%Capacity = NextPrime(Collection%Capacity)
        END IF
    END SELECT

    RETURN

END SUBROUTINE AdjustCapacity

!******************************************************************************

SUBROUTINE IncreaseCapacity(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To increase capacity according to the probing algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashSet), INTENT(INOUT)   :: Collection    !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Collection%ProbAlgo)
    CASE (LinearProbing)
        ! doubling the capacity
        Collection%Capacity = Collection%Capacity*2_kIndex + 1_kIndex
    CASE (QuadraticProbing)
        ! increase the capacity of the hash set to the next power of two.
        Collection%Capacity = SHIFTL(HighestOneBit(Collection%Capacity), 1)
    CASE (DoubleHashing)
        ! doubling the capacity
        Collection%Capacity = Collection%Capacity*2_kIndex + 1_kIndex
    END SELECT

    RETURN

END SUBROUTINE IncreaseCapacity

! -----------------------------------------------------------------------------
! -----                     Auxiliary Procedures                          -----
! -----------------------------------------------------------------------------

FUNCTION ComputeIndex(HashCode, Capacity) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the index of working items of the hash set for
    !  the specified hash code.  Returns value between 0 and
    !  Capacity-1 (assumes Capacity is a power of 2).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tHash,  INTENT(IN)  :: HashCode
    tIndex, INTENT(IN)  :: Capacity
    tIndex              :: Indx

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! Note: the sign of the hash code must have already been removed.
    Indx = IAND(HashCode, Capacity-1_kIndex)

    RETURN

END FUNCTION ComputeIndex

!******************************************************************************

RECURSIVE FUNCTION ComputeGCD(A, B) RESULT(C)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the greatest common denominator of A and B.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: A, B
    tIndex              :: C

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (B == 0_kIndex) THEN
        C = A
    ELSE
        C = ComputeGCD(B, MOD(A, B))
    END IF

    RETURN

END FUNCTION ComputeGCD

!******************************************************************************

END MODULE MClass_HashSet

!******************************************************************************
