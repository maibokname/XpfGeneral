
MODULE MClass_HashMap

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *HashMap* type and its supporting routines and data type.  The
!   *HashMap* type is a collection type that employs an open-addressing hash table implementation
!   to provide common operations for an unordered symbol table. <br>
!   Unlike the *list-based* and *tree-based* symbol table types, which can be used instantly by
!   inserting objects into a collection, the *HashMap* type requires an explicit construction
!   before using other provided operations.  There are two methods provided to create the
!   collection.  The *CreateEmpty* method constructs an empty table with optional multiple
!   arguments (including an initial capacity, a load factor, a probing algorithm, and a hash
!   function used to compute a hash code of a key) whereas the *Construct* method constructs
!   a table from arrays of keys and values. <br>
!   The *HashMap* type uses the *KeyUnordered* type to store its keys and the *GenData* type
!   to store its values.  Therefore, it can be used to store key-value pairs of any data types
!   (except the *LOGICAL* type for the keys).  Like other collection types, however, it must
!   be employed to store key-value pairs of only specific key type and one specific value type.
!   To store key-value pairs of another key type (or another value type), it must be destructed
!   before inserting items of different key type (or different value type). <br>
!   As a symbol table, the *HashMap* type does not allow duplicated keys.  Therefore, if an
!   inserted key is equal to a key stored in the table, an associated value of the stored key
!   is replaced by an associated value of the inserted key.  As an *unordered* symbol table, the
!   *HashMap* type makes no guarantees as to the iteration order of the table.  In particular,
!   it does not guarantee that the order will remain the same over time. <br>
!   Technically, the *HashMap* type employs the open-addressing as a collision resolution
!   technique where the hash resolution is performed through probing.  It provides three probing
!   algorithms: linear probing, quadratic probing and double hashing.  By default, the linear
!   probing algorithm is used.  However, a user can specify other probing algorithm during the
!   construction of the table. <br>

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define     tHash           tIndex

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MClass_CharBuffer
    USE MBase_ByteUtil,         ONLY: AnyType_GetByteSize
    USE MBase_SIntUtil,               ToChar => ToDecStrSigned
    USE MBase_MathUtil
#ifdef Indx32Bits
    USE MBase_SimpleHash32,     ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64,     ONLY: ComputeHash => Hash64_FNV1a
#endif
    USE MClass_Object,          ONLY: Object
    USE MClass_GenData
    USE MClass_MemoryPool
    USE MClass_KeyUnordered
    USE MClass_BaseCollection
    USE MClass_BaseSymTable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: HashMap

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar,   PARAMETER  :: ModName = 'MClass_HashMap'
    ! status of the TabItem
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
    !> *TabItem* is a key-value pair data type containing key and value as
    !  its components.
    TYPE, EXTENDS(Object)   :: TabItem
        tCharLen(3)         :: Stat = STAT_NUL  !! current status of the object
        TYPE(KeyUnordered)  :: Key              !! stored key
        TYPE(GenData)       :: Value            !! stored value
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures for TabItem Type              -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: TabItem_SetKeyValue
        PROCEDURE, PRIVATE  :: TabItem_SetKeyOnly
        GENERIC             :: Set          => TabItem_SetKeyValue, TabItem_SetKeyOnly
        PROCEDURE           :: IsKeyEqual   => TabItem_IsKeyEqual
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Object Type              -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => TabItem_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => TabItem_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => TabItem_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => TabItem_ToString
        ! ---------------------------------------------------------------------
        ! -----                     Overridden Procedure                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => TabItem_HashCode
        ! ---------------------------------------------------------------------
    END TYPE TabItem
    !> The *HashMap* type is a collection type that employs an open-addressing hash table
    !  implementation to provide common operations for an unordered symbol table.  It makes
    !  no guarantees as to the iteration order of the symbol table; in particular, it does
    !  not guarantee that the order will remain constant over time.
    TYPE, EXTENDS(BaseSymTable) :: HashMap
        PRIVATE
        !% current capacity of the hash table
        tIndex                      :: Capacity     = DefaultCapacity
        !% working table items used to store key-value pairs
        TYPE(TabItem), ALLOCATABLE  :: Items(:)
        !% current index into the working items (used for iteration purpose)
        tIndex                      :: Indx         = 0_kIndex
        !% the number of keys not yet visited (used for iteration purpose)
        tIndex                      :: KeyLeft      = 0_kIndex
        !% current modification count (used for iteration purpose)
        tIndex                      :: IterModCount = 0_kIndex
        !% load factor
        tRealDP                     :: LoadFactor   = DefaultLoadFactor
        !% threshold for resizing
        tIndex                      :: Threshold    = 0_kIndex
        !% modification count
        tIndex                      :: ModCount     = 0_kIndex
        !% the total number of used buckets inside the hash table (including cells marked as deleted).
        tIndex                      :: UsedBuckets  = 0_kIndex
        !% the total number of unique keys currently inside the hash table.
        tIndex                      :: KeyCount     = 0_kIndex
        !% probing algorithm
        tSInt32                     :: ProbAlgo     = LinearProbing
        !% index for double hashing
        tHash                       :: HashIndx     = 0_kIndex
        !> memory pool of stored items
        TYPE(MemoryPool)            :: ItemPool
        !% pointer to a hash function
        PROCEDURE(HashFunc),      NOPASS, POINTER   :: HashCalc => NULL()
        !> pointer to a procedure to copy stored data for a derived type not in the Object class;
        !  required if this type has allocatable/pointer component(s).
        PROCEDURE(IfacePolyCopy), NOPASS, POINTER   :: KeyCopy => NULL()
        !> pointer to a procedure to copy stored data for a derived type not in the Object class;
        !  required if this type has allocatable/pointer component(s).
        PROCEDURE(IfacePolyCopy), NOPASS, POINTER   :: ValCopy => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: FindKey <br>
        !  **Purpose**:  To find the specified key in the collection.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%FindKey(Key, KeyItem) <br>
        !   --->    IF (.NOT.Collection%FindKey(Key)) DoSomething
        PROCEDURE, PRIVATE  :: FindKey      => HashMap_FindKey
        !> **Type-Bound Subroutine**: Resize <br>
        !  **Purpose**:  To resize the collection to the specified capacity. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Resize(64)
        PROCEDURE, PRIVATE  :: Resize       => HashMap_Resize
        !> Use the *Construct* method to construct the collection from an array of items.
        PROCEDURE, PRIVATE  :: HashMap_CreateByArray
        !> To retrieve all stored keys
        PROCEDURE, PRIVATE  :: GetAllKeys   => HashMap_GetAllKeys
        !> To retrieve all stored values
        PROCEDURE, PRIVATE  :: GetAllVals   => HashMap_GetAllVals
        ! ---------------------------------------------------------------------
        ! -----  Deferred/Overridden Procedures from BaseCollection Type  -----
        ! ---------------------------------------------------------------------
        !> Use the *Construct* method to construct the collection from another collection.
        PROCEDURE   :: CopyCollection   => HashMap_CopyCollection
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all key-value pairs from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        PROCEDURE   :: Clear            => HashMap_ClearItems
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        !  **Note**:  This method is NOT equivalent to the *Clear* method. <br>
        PROCEDURE   :: Destruct         => HashMap_Destroy
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize          => HashMap_GetSize
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%IsEmpty() <br>
        !   --->    IF (.NOT.Collection%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty          => HashMap_IsEmpty
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseSymTable Type        -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start an iteration and return a flag indicating whether
        !                the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartFirst() <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstKey) <br>
        !   --->    IsEmpty = Collection%StartFirst(Value=FirstVal) <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstKey, FirstVal)
        PROCEDURE   :: StartFirst   => HashMap_Move2FirstPair
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next key-value pair and return a flag
        !                indicating whether the cursor pointer has reached the end of
        !                the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey) <br>
        !   --->    IsTheEnd = Collection%MoveForward(Value=NextVal) <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey, NextVal)
        PROCEDURE   :: MoveForward  => HashMap_Move2NextPair
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key-value pair to the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Insert(Key, Value) <br>
        PROCEDURE   :: Insert       => HashMap_Insert
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete the current key-value pair from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the *StartFirst* and
        !       *MoveForward* methods.  Therefore, after the call to one of those methods and then
        !       calling this one will result in a removal of the current item of the iteration
        !       (i.e. the same item that can be retrieved via the *StartFirst* and *MoveForward*
        !       methods). <br>
        PROCEDURE   :: Delete       => HashMap_Delete
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified key (and its associated value) from
        !                the collection.  Also, return a flag indicating whether the
        !                key-value pair is successfully removed or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Remove(Key) <br>
        !   --->    IF (.NOT.Collection%Remove(Key)) DoSomething
        PROCEDURE   :: Remove       => HashMap_Remove
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified key in the collection.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Contain(Key) <br>
        !   --->    IF (.NOT.Collection%Contain(Key)) DoSomething
        PROCEDURE   :: Contain      => HashMap_Contain
        !> **Type-Bound Function**: GetValue <br>
        !  **Purpose**:  To get a value associated with the specified key in the collection. Also,
        !       return a flag indicating whether the key-value pair is successfully found or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetValue(Key, Value) <br>
        !   --->    IF (.NOT.Collection%GetValue(Key, Value)) DoSomething
        PROCEDURE   :: GetValue     => HashMap_GetValue
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all key-value pairs from the collection.  Also, return
        !       a flag indicating whether the pairs are successfully retrieved and removed or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Keys, Values) <br>
        !   --->    IF (.NOT.Collection%ToArray(Keys, Values)) DoSomething
        PROCEDURE   :: ToArray      => HashMap_ToArray
        !> **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all keys and/or all values (without removing them) from the collection.
        !       Also, return a flag indicating whether the keys and/or the values are successfully
        !       retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Keys, Values) <br>
        !   --->    IF (.NOT.Collection%GetAll(Keys, Values)) DoSomething
        PROCEDURE   :: GetAll       => HashMap_GetAll
        ! ---------------------------------------------------------------------
        ! -----                      Specific Procedures                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%CreateEmpty()                           ! use default options  <br>
        !   --->    CALL Table%CreateEmpty(InitCap=25)                 ! specify initial capacity <br>
        !   --->    CALL Table%CreateEmpty(LoadFactor=0.5)             ! specify load factor <br>
        !   --->    CALL Table%CreateEmpty(ProbAlgo=2)                 ! specify probing algorithm <br>
        !   --->    CALL Table%CreateEmpty(HashCal=Murmur3_Hash32_Opt) ! specify hash function <br>
        !   --->    CALL Table%CreateEmpty(30, 0.75, 3, XX_Hash64_Opt) ! specify all options <br>
        !  **Note**: Any suitable hash function routine from the *ModBase_SimpleHash32*, 
        !       *ModBase_SimpleHash64*, *ModBase_ReferenceHash32*, *ModBase_ReferenceHash64*
        !       *ModBase_OptimalHash32*, and *ModBase_OptimalHash64* modules can be used to
        !       specify the *HashCal* argument.  The term *suitable* means that any routine
        !       that has exactly the same interface as the *HashFunc* abstract function
        !       is the suitable one.  <br>
        !  **Note2**: Depending on a type of indices defined in the '*Macro - Basic Definitions.f90*'
        !       file, a 32-bit hash-function routine is a suitable one for 32-bit integer indices
        !       while a 64-bit hash-function routine is a suitable one for 64-bit integer indices.
        !       This is a compile-time choice.  <br>
        PROCEDURE   :: CreateEmpty  => HashMap_CreateEmpty
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
        GENERIC     :: Construct    => HashMap_CreateByArray
        ! ---------------------------------------------------------------------
        ! -----     Deferred/Overridden Procedures from Object Type       -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => HashMap_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => HashMap_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => HashMap_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => HashMap_ToString
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => HashMap_HashCode
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the hash table.
        FINAL       :: HashMap_Finalize
        ! ---------------------------------------------------------------------
    END TYPE HashMap

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
! -----                       TabItem Procedures                          -----
! -----------------------------------------------------------------------------

SUBROUTINE TabItem_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the TabItem object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabItem),     INTENT(IN)  :: SrcObj   !! source object
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
    TYPE IS (TabItem)
        DstObj%Stat = SrcObj%Stat
        CALL SrcObj%Key%Copy(DstObj%Key, IsDeep)
        CALL SrcObj%Value%Copy(DstObj%Value, IsDeep)
    CLASS DEFAULT
        CALL Handle_ErrLevel('TabItem_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE TabItem_Copy

!******************************************************************************

FUNCTION TabItem_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabItem), INTENT(IN)  :: LhsObj   !! an object
    CLASS(Object),  INTENT(IN)  :: RhsObj   !! another object
    tLogical                    :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (TabItem)
        Flag = FalseVal
        IF (LhsObj%Stat /= RhsObj%Stat) RETURN
        IF (.NOT.LhsObj%Key%IsEqualTo(RhsObj%Key)) RETURN
        Flag = LhsObj%Value%IsEqualTo(RhsObj%Value)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION TabItem_IsEqualTo

!******************************************************************************

SUBROUTINE TabItem_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of the TabItem object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabItem), INTENT(INOUT)   :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Obj%Stat = STAT_NUL
    CALL Obj%Key%MemFree()
    CALL Obj%Value%MemFree()

    RETURN

END SUBROUTINE TabItem_MemFree

!******************************************************************************

FUNCTION TabItem_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabItem), INTENT(IN)  :: Obj
    tCharAlloc                  :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Str = '{TabItem : {' // Obj%Key%ToString() // ' : ' // Obj%Value%ToString() // '}}'

    RETURN

END FUNCTION TabItem_ToString

!******************************************************************************

FUNCTION TabItem_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabItem), INTENT(IN)  :: Obj
    tIndex                      :: Code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Code = Obj%Key%HashCode() + Obj%Value%HashCode()
    
    RETURN

END FUNCTION TabItem_HashCode

!******************************************************************************

SUBROUTINE TabItem_SetKeyValue(Obj, Status, Pool, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set value of the specified item.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabItem),             INTENT(INOUT)   :: Obj
    tCharLen(3),                INTENT(IN)      :: Status
    TYPE(MemoryPool), OPTIONAL, INTENT(INOUT)   :: Pool
    CLASS(*),         OPTIONAL, INTENT(IN)      :: Key
    CLASS(*),         OPTIONAL, INTENT(IN)      :: Value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Obj%Stat = Status
    IF (PRESENT(Key).AND.PRESENT(Pool))   CALL Obj%Key%Set(Key, Pool)
    IF (PRESENT(Value).AND.PRESENT(Pool)) CALL Obj%Value%Set(Value, Pool)

    RETURN

END SUBROUTINE TabItem_SetKeyValue

!******************************************************************************

SUBROUTINE TabItem_SetKeyOnly(This, Status, Other)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set data of the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabItem), INTENT(INOUT)   :: This
    tCharLen(3),    INTENT(IN)      :: Status
    TYPE(TabItem),  INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    This%Stat = Status
    CALL Other%Key%Copy(This%Key)

    RETURN

END SUBROUTINE TabItem_SetKeyOnly

!******************************************************************************

FUNCTION TabItem_IsKeyEqual(Obj, Key, Pool) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To check whether the specified key is equal to the item's key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabItem),   INTENT(IN)    :: Obj
    CLASS(*),         INTENT(IN)    :: Key
    TYPE(MemoryPool), INTENT(INOUT) :: Pool
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(GenData)   :: KeyInp

!** FLOW:

    CALL KeyInp%Set(Key, Pool)
    Flag = Obj%Key%IsEqualTo(KeyInp)
    CALL KeyInp%MemFree()

    RETURN

END FUNCTION TabItem_IsKeyEqual

! ---------------------------------------------------------------------
! -----     Deferred/Overridden Procedures from Object Type       -----
! ---------------------------------------------------------------------

SUBROUTINE HashMap_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.
    !  This is a deferred procedure inherited from the *Object* type.
    !  *Note*:  SrcObj must be in the *HashMap* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap),     INTENT(IN)  :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
    !^ flag indicating whether to perform deep copy or shallow copy; <br>
    !  - if true, perform shallow copy; <br>
    !  - if false, perform deep copy; <br>
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT TYPE (DstObj)
    TYPE IS (HashMap)
        DstObj%Capacity     =  SrcObj%Capacity
        DstObj%Indx         =  SrcObj%Indx
        DstObj%KeyLeft      =  SrcObj%KeyLeft
        DstObj%IterModCount =  SrcObj%IterModCount
        DstObj%LoadFactor   =  SrcObj%LoadFactor
        DstObj%Threshold    =  SrcObj%Threshold
        DstObj%ModCount     =  SrcObj%ModCount
        DstObj%UsedBuckets  =  SrcObj%UsedBuckets
        DstObj%KeyCount     =  SrcObj%KeyCount
        DstObj%ProbAlgo     =  SrcObj%ProbAlgo
        DstObj%HashIndx     =  SrcObj%HashIndx
        DstObj%HashCalc     => SrcObj%HashCalc
        DstObj%KeyCopy      => SrcObj%KeyCopy
        DstObj%ValCopy      => SrcObj%ValCopy
        ALLOCATE(DstObj%Items, SOURCE=SrcObj%Items)
        CALL SrcObj%ItemPool%CloneTo(DstObj%ItemPool)
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('HashMap_Copy', ModName, ErrSevere, &
                             'Type of the DstObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE HashMap_Copy

!******************************************************************************

FUNCTION HashMap_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(IN)  :: LhsObj   !! an object
    CLASS(Object),  INTENT(IN)  :: RhsObj   !! another object
    tLogical                    :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (HashMap)
        Flag = FalseVal
        IF (LhsObj%Capacity /= RhsObj%Capacity) RETURN
        IF (LhsObj%KeyCount /= RhsObj%KeyCount) RETURN
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

END FUNCTION HashMap_IsEqualTo

!******************************************************************************

SUBROUTINE HashMap_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the HashMap object.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(INOUT)  :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! first free items' components and then free of the items themselves
    IF (ALLOCATED(Obj%Items)) THEN
        DO I = 1_kIndex, SIZE(Obj%Items)
            CALL Obj%Items(I)%MemFree()
        END DO
        DEALLOCATE(Obj%Items)
    END IF

    ! reset all components
    Obj%Capacity = DefaultCapacity
    Obj%Indx = 0_kIndex
    Obj%KeyLeft = 0_kIndex
    Obj%IterModCount = 0_kIndex
    Obj%LoadFactor = DefaultLoadFactor
    Obj%Threshold = 0_kIndex
    Obj%ModCount = 0_kIndex
    Obj%UsedBuckets = 0_kIndex
    Obj%KeyCount = 0_kIndex
    Obj%ProbAlgo = LinearProbing
    Obj%HashIndx = 0_kIndex
    NULLIFY(Obj%HashCalc)
    NULLIFY(Obj%KeyCopy)
    NULLIFY(Obj%ValCopy)
    CALL Obj%FreeMolds()
    CALL Obj%ItemPool%Destruct()

    RETURN

    RETURN

END SUBROUTINE HashMap_MemFree

!******************************************************************************

FUNCTION HashMap_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the HashMap type.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(IN)  :: Obj
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
            tCharAlloc          :: KeyStr, ValStr, ItemStr
            ! initialize
            CALL ChrBuf%CreateEmpty(InitCap=Obj%GetSize()*60_kIndex)
            CALL ChrBuf%Append('[')
            Count = 0_kIndex
            DO I = 1_kIndex, SIZE(Obj%Items)
                ! skip if the item is empty
                IF (Obj%Items(I)%Stat /= STAT_GEN) CYCLE
                ! add the string representation of the current item
                KeyStr = Obj%Items(I)%Key%ToString()
                ValStr = Obj%Items(I)%Value%ToString()
                ItemStr = '{' // KeyStr(17:LEN(KeyStr)-1) // ' : ' // ValStr(12:LEN(ValStr)-1) // '}'
                CALL ChrBuf%Append(ItemStr)
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
    Str = '{HashMap with ' // ToChar(Obj%GetSize()) // ' items : ' // BaseStr // '}'

    RETURN

END FUNCTION HashMap_ToString

!******************************************************************************

FUNCTION HashMap_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(IN)  :: Obj
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
                Code = Code + Obj%Items(I)%Key%HashCode()
            END DO
        END BLOCK
    END IF

    RETURN

END FUNCTION HashMap_HashCode

! ---------------------------------------------------------------------
! -----  Deferred/Overridden Procedures from BaseCollection Type  -----
! ---------------------------------------------------------------------

SUBROUTINE HashMap_CopyCollection(This, Other, ItemCopy, ValCopy)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items as the given collection (Other). <br>
    !  This is a deferred procedure by the *BaseCollection* class. <br>
    !  *Note*:  Other must be in the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(HashMap),        INTENT(INOUT)    :: This
    !% collection object to be copied
    CLASS(BaseCollection), INTENT(INOUT)    :: Other
    !> a helper procedure to copy stored items (or keys) for a derived type not in the
    !  *Object* class; required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL      :: ItemCopy
    !> a helper procedure to copy stored values for a derived type not in the *Object*
    !  class; required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL      :: ValCopy

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (Other)
    TYPE IS (HashMap)
        ! same type of collection
        CALL Other%Copy(This)
    CLASS IS (BaseSymTable)
        ! different types of collection
        BLOCK
            ! block variables
            tLogical                :: IsTheEnd
            CLASS(*), POINTER       :: MoldPtr
            CLASS(*), ALLOCATABLE   :: KeyItem
            CLASS(*), ALLOCATABLE   :: ValItem
            ! get key and value molds
            MoldPtr => Other%GetKeyPtr()
            ALLOCATE(KeyItem, MOLD=MoldPtr)
            MoldPtr => Other%GetValPtr()
            ALLOCATE(ValItem, MOLD=MoldPtr)
            ! loop through the other collection and get key-value pairs along the way
            IsTheEnd = Other%StartFirst(KeyItem, ValItem, ItemCopy, ValCopy)
            DO WHILE (.NOT.IsTheEnd)
                ! add an item to this collection
                CALL This%Insert(KeyItem, ValItem)
                IsTheEnd = Other%MoveForward(KeyItem, ValItem, ItemCopy, ValCopy)
            END DO
            NULLIFY(MoldPtr)
            DEALLOCATE(KeyItem, ValItem)
        END BLOCK
    CLASS DEFAULT
        CALL Handle_ErrLevel('HashMap_CopyCollection', ModName, ErrSevere, &
                             'Type of "Other" must be in the "BaseSymTable" class.')
    END SELECT
    
    RETURN

END SUBROUTINE HashMap_CopyCollection

!******************************************************************************

SUBROUTINE HashMap_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To free components of the items from the table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(INOUT)   :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

!** FLOW:

    IF (ALLOCATED(Collection%Items)) THEN
        DO I = 0_kIndex, Collection%Capacity-1_kIndex
            CALL Collection%Items(I)%MemFree()
        END DO
    END IF
    Collection%Indx = 0_kIndex
    Collection%KeyLeft = 0_kIndex
    Collection%IterModCount = 0_kIndex
    Collection%Threshold = 0_kIndex
    Collection%KeyCount = 0_kIndex
    Collection%UsedBuckets = 0_kIndex
    Collection%ModCount = Collection%ModCount + 1_kIndex
    CALL Collection%FreeMolds()
    NULLIFY(Collection%KeyCopy)
    NULLIFY(Collection%ValCopy)

    RETURN

END SUBROUTINE HashMap_ClearItems

!******************************************************************************

SUBROUTINE HashMap_Destroy(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(INOUT)   :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    CALL Collection%MemFree()

    RETURN

END SUBROUTINE HashMap_Destroy

!******************************************************************************

FUNCTION HashMap_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the number of keys currently in the hash table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(IN)  :: Collection   !! collection
    tIndex                      :: Size         !! the number of keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%KeyCount

    RETURN

END FUNCTION HashMap_GetSize

!******************************************************************************

FUNCTION HashMap_IsEmpty(Collection) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the hash table is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(IN)  :: Collection   !! collection
    tLogical                    :: Flag         !! true if the table is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = (Collection%KeyCount == 0_kIndex)

    RETURN

END FUNCTION HashMap_IsEmpty

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseSymTable Type         -----
! ---------------------------------------------------------------------

FUNCTION HashMap_Move2FirstPair(Collection, Key, Value, KeyCopy, ValCopy) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the first (starting) pair data in a hash table.   For the hash table,
    !  which is an unordered symbol table, the starting pair is the first pair found
    !  in the non-empty bucket.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap),     INTENT(INOUT)   :: Collection    !! collection
    !% the first key as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Key
    !% the first value as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Value
    !> a procedure to copy stored keys for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: KeyCopy
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
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
    Collection%KeyLeft = Collection%KeyCount
    Collection%IterModCount = Collection%ModCount

    ! start iteration by looking for the first non-empty slot
    DO WHILE (Collection%Items(Collection%Indx)%Stat /= STAT_GEN)
        Collection%Indx = Collection%Indx + 1_kIndex
    END DO

    ! update KeyLelf
    Collection%KeyLeft = Collection%KeyLeft - 1_kIndex

    ! get key if requested
    IF (PRESENT(Key).AND.PRESENT(KeyCopy)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%Key%Get(Key, KeyCopy)) THEN
            CALL Handle_ErrLevel('HashMap_Move2FirstPair', ModName, ErrWarning, &
                    'Type of the specified key is likely NOT the same as that of stored keys.')
        END IF
    ELSEIF (PRESENT(Key)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%Key%Get(Key, Collection%KeyCopy)) THEN
            CALL Handle_ErrLevel('HashMap_Move2FirstPair', ModName, ErrWarning, &
                    'Type of the specified key is likely NOT the same as that of stored keys.')
        END IF
    END IF

    ! get value if requested
    IF (PRESENT(Value).AND.PRESENT(ValCopy)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%Value%Get(Value, ValCopy)) THEN
            CALL Handle_ErrLevel('HashMap_Move2FirstPair', ModName, ErrWarning, &
                    'Type of the specified key is likely NOT the same as that of stored keys.')
        END IF
    ELSEIF (PRESENT(Value)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%Value%Get(Value, Collection%ValCopy)) THEN
            CALL Handle_ErrLevel('HashMap_Move2FirstPair', ModName, ErrWarning, &
                    'Type of the specified value is likely NOT the same as that of stored values.')
        END IF
    END IF

    RETURN

END FUNCTION HashMap_Move2FirstPair

!******************************************************************************

FUNCTION HashMap_Move2NextPair(Collection, Key, Value, KeyCopy, ValCopy) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next pair data in a symbol table.  For the *HashMap*, which
    !  is an unordered symbol table,  the next pair is a pair inserted in the first
    !  non-empty bucket after the previous one.  <br>
    !  The routine will report an error if an alteration to stored item(s) (either
    !  by an insertion or a removal) has been occurred during current iteration.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap),     INTENT(INOUT)   :: Collection    !! collection
    !% the next key as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Key
    !% the next value as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Value
    !> a procedure to copy stored keys for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: KeyCopy
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !> a flag indicating whether the move to the end of the table occurs or not <br>
    ! - true if next pair data is NOT available. <br>
    ! - otherwise next pair data is available.
    tLogical                            :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Collection%IterModCount /= Collection%ModCount) THEN
        CALL Handle_ErrLevel('HashMap_Move2NextPair', ModName, ErrWarning, &
                 "Must re-start the iteration because the stored items have been altered.")
        RETURN
    END IF

    ! check for empty table
    IF (Collection%IsEmpty()) THEN
        IsTheEnd = TrueVal
        RETURN
    ELSEIF (Collection%KeyLeft == 0_kIndex) THEN
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

    ! update KeyLelf
    Collection%KeyLeft = Collection%KeyLeft - 1_kIndex

    ! get key if requested
    IF (PRESENT(Key).AND.PRESENT(KeyCopy)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%Key%Get(Key, KeyCopy)) THEN
            CALL Handle_ErrLevel('HashMap_Move2NextPair', ModName, ErrWarning, &
                    'Type of the specified key is likely NOT the same as that of stored keys.')
        END IF
    ELSEIF (PRESENT(Key)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%Key%Get(Key, Collection%KeyCopy)) THEN
            CALL Handle_ErrLevel('HashMap_Move2NextPair', ModName, ErrWarning, &
                    'Type of the specified key is likely NOT the same as that of stored keys.')
        END IF
    END IF

    ! get value if requested
    IF (PRESENT(Value).AND.PRESENT(ValCopy)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%Value%Get(Value, ValCopy)) THEN
            CALL Handle_ErrLevel('HashMap_Move2NextPair', ModName, ErrWarning, &
                    'Type of the specified key is likely NOT the same as that of stored keys.')
        END IF
    ELSEIF (PRESENT(Value)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%Value%Get(Value, Collection%ValCopy)) THEN
            CALL Handle_ErrLevel('HashMap_Move2NextPair', ModName, ErrWarning, &
                    'Type of the specified value is likely NOT the same as that of stored values.')
        END IF
    END IF

    RETURN

    RETURN

END FUNCTION HashMap_Move2NextPair

!******************************************************************************

SUBROUTINE HashMap_Insert(Collection, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a key-value pair into the hash table.  If the specified key is already stored
    !  in the table, replace the old value with the new one.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(INOUT)   :: Collection   !! collection
    CLASS(*),       INTENT(IN)      :: Key          !! key to be inserted
    CLASS(*),       INTENT(IN)      :: Value        !! associated value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tHash       :: HashCode
    tIndex      :: Indx, Offset, I, J
    tIndex      :: KeySize

! FLOW

    ! check the specified key and value
    IF (.NOT.IsKeyUnordered(Key)) THEN
        CALL Handle_ErrLevel('HashMap_Insert', ModName, ErrSevere, &
                'Type of the specified key is NOT valid.')
        RETURN
    ELSEIF (.NOT.Collection%IsKeyValid(Key, IsOrderedKey=FalseVal)) THEN
        CALL Handle_ErrLevel('HashMap_Insert', ModName, ErrSevere, &
                'Only keys of the same type are allowed in a collection.')
        RETURN
    ELSEIF (.NOT.Collection%IsValValid(Value)) THEN
        CALL Handle_ErrLevel('HashMap_Insert', ModName, ErrSevere, &
                'Only values of the same type are allowed in a collection.')
        RETURN
    END IF

    IF (.NOT.ALLOCATED(Collection%Items)) THEN
        ! not constructed yet so check type of the specified key
        IF (GetDataType(Key) == TYPE_DERIVED) THEN
            CALL Collection%FreeMolds()
            CALL Handle_ErrLevel('HashMap_Insert', ModName, ErrSevere, &
                    'The collection must be explicitly constructed before using any other method(s).')
            RETURN
        END IF
        ! construct empty collection with default options
        CALL Collection%CreateEmpty()
    END IF

    ! resize the capacity if needed
    IF (Collection%UsedBuckets >= Collection%Threshold) CALL Collection%Resize(MoreCap=TrueVal)

    ! set up the probing if needed
    CALL SetupProbing(Collection, Key)

    ! compute the hash code and offset
    KeySize  = AnyType_GetByteSize(Key)
    HashCode = Collection%HashCalc(Key, KeySize, HashSeed, RemoveSign=TrueVal)
    Offset   = ComputeIndex(HashCode, Collection%Capacity)

    ! initialize indices
    I = Offset
    J = -1_kIndex
    Indx = 1_kIndex
    DO
        IF (Collection%Items(I)%Stat == STAT_DEL) THEN
            ! The current bucket was previously deleted
            IF (J == -1_kIndex) J = I
        ELSEIF (Collection%Items(I)%Stat /= STAT_NUL) THEN
            ! The current bucket already contains a key
            IF (Collection%Items(I)%IsKeyEqual(Key, Collection%ItemPool)) THEN
                ! The key we're trying to insert already exists in the hash-table,
                ! so update its value with the most recent value
                IF (J == -1_kIndex) THEN
                    CALL Collection%Items(I)%Set(STAT_GEN, Collection%ItemPool, Value=Value)
                ELSE
                    CALL Collection%Items(I)%Set(STAT_DEL)
                    CALL Collection%Items(J)%Set(STAT_GEN, Collection%ItemPool, Key, Value)
                END IF
                Collection%ModCount = Collection%ModCount + 1_kIndex
                EXIT
            END IF
        ELSE
            ! The current bucket is null so an insertion/update can occur
            IF (J == -1_kIndex) THEN
                ! No previously encountered deleted buckets
                Collection%UsedBuckets = Collection%UsedBuckets + 1_kIndex
                CALL Collection%Items(I)%Set(STAT_GEN, Collection%ItemPool, Key, Value)
            ELSE
                ! Previously seen deleted bucket. Instead of inserting
                ! the new element at i where the null element is insert
                ! it where the deleted token was found.
                CALL Collection%Items(J)%Set(STAT_GEN, Collection%ItemPool, Key, Value)
            END IF
            Collection%KeyCount = Collection%KeyCount + 1_kIndex
            Collection%ModCount = Collection%ModCount + 1_kIndex
            EXIT
        END IF
        I = ComputeIndex(Offset + Probe(Collection, Indx), Collection%Capacity)
        Indx = Indx + 1_kIndex
    END DO

    RETURN

END SUBROUTINE HashMap_Insert

!******************************************************************************

SUBROUTINE HashMap_Delete(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete a key-value pair of the current iteration from a symbol table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashMap), INTENT(INOUT)   :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Collection%KeyCount = Collection%KeyCount - 1_kIndex
    Collection%ModCount = Collection%ModCount + 1_kIndex
    CALL Collection%Items(Collection%Indx)%Set(STAT_DEL)

    RETURN

END SUBROUTINE HashMap_Delete

!******************************************************************************

FUNCTION HashMap_Remove(Collection, Key) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the specified key (and its associated value) from a symbol
    !  table.  Also, return a flag indicating whether the key-value pair is
    !  successfully removed or not. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap),     INTENT(INOUT)   :: Collection   !! collection
    CLASS(*),           INTENT(IN)      :: Key          !! key to be removed
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tHash       :: HashCode
    tIndex      :: Indx, Offset, I
    tIndex      :: KeySize

! FLOW

    ! return quickly if possible
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSEIF (.NOT.Collection%IsKeyValid(Key, IsOrderedKey=FalseVal)) THEN
        Flag = FalseVal
        CALL Handle_ErrLevel('HashMap_Remove', ModName, ErrSevere, &
                'Type of the specified key is NOT the same as that of stored keys.')
        RETURN
    END IF

    ! set up the probing if needed
    CALL SetupProbing(Collection, Key)

    ! compute the hash code and offset
    KeySize  = AnyType_GetByteSize(Key)
    HashCode = Collection%HashCalc(Key, KeySize, HashSeed, RemoveSign=TrueVal)
    Offset   = ComputeIndex(HashCode, Collection%Capacity)

    ! initialize indices
    I = Offset
    Indx = 1_kIndex
    ! Starting at the original hash probe until we find a spot where our key is
    ! or we hit a null element in which case our element does not exist.
    DO
        IF (Collection%Items(I)%Stat == STAT_DEL) THEN
            ! ignore deleted bucket so do nothing here
        ELSEIF (Collection%Items(I)%Stat == STAT_NUL) THEN
            ! the key was not found
            Flag = FalseVal
            EXIT
        ELSEIF (Collection%Items(I)%IsKeyEqual(Key, Collection%ItemPool)) THEN
            ! found the key we want to remove
            Collection%KeyCount = Collection%KeyCount - 1_kIndex
            Collection%ModCount = Collection%ModCount + 1_kIndex
            CALL Collection%Items(I)%Set(STAT_DEL)
            Flag = TrueVal
            EXIT
        END IF
        I = ComputeIndex(Offset + Probe(Collection, Indx), Collection%Capacity)
        Indx = Indx + 1_kIndex
    END DO

    ! halve the hash table capacity if it is 12.5% full or less
    IF ((Collection%KeyCount > 0_kIndex).AND.(Collection%KeyCount <= Collection%Capacity/8)) THEN
        CALL Collection%Resize(MoreCap=FalseVal)
    END IF

    RETURN

END FUNCTION HashMap_Remove

!******************************************************************************

FUNCTION HashMap_Contain(Collection, Key) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is currently stored in a symbol table.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(INOUT)   :: Collection   !! collection
    CLASS(*),       INTENT(IN)      :: Key          !! key to be looked for
    !% flag indicating whether the specified key is found or not.
    tLogical                        :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Collection%IsEmpty()) THEN
        Found = FalseVal
    ELSEIF (.NOT.Collection%IsKeyValid(Key, IsOrderedKey=FalseVal)) THEN
        Found = FalseVal
        CALL Handle_ErrLevel('HashMap_Contain', ModName, ErrSevere, &
                'Type of the specified key is NOT the same as that of stored keys.')
        RETURN
    ELSE
        ! find the key
        Found = Collection%FindKey(Key)
    END IF

    RETURN

END FUNCTION HashMap_Contain

!******************************************************************************

FUNCTION HashMap_GetValue(Collection, Key, Value, ValCopy) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the associated value of the specified key.  Also, return
    !  a flag indicating whether the value is successfully retrieved or not. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(INOUT)       :: Collection   !! collection
    CLASS(*),       INTENT(IN)          :: Key          !! key to be looked for
    CLASS(*),       INTENT(INOUT)       :: Value        !! associated value
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !% flag indicating whether the value is successfully retrieved or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
    ELSEIF (.NOT.Collection%IsKeyValid(Key, IsOrderedKey=FalseVal)) THEN
        Flag = FalseVal
        CALL Handle_ErrLevel('HashMap_GetValue', ModName, ErrSevere, &
                'Type of the specified key is NOT the same as that of stored keys.')
        RETURN
    ELSEIF (.NOT.Collection%IsValValid(Value)) THEN
        Flag = FalseVal
        CALL Handle_ErrLevel('HashMap_GetValue', ModName, ErrSevere, &
                'Type of the specified value is NOT the same as that of stored values.')
        RETURN
    ELSE
        ! find the key and get its associated value
        Flag = Collection%FindKey(Key, Value, ValCopy)
    END IF

    RETURN

END FUNCTION HashMap_GetValue

!**************************************************************************************

FUNCTION HashMap_ToArray(Collection, Keys, Values, KeyCopy, ValCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove all key-value pairs from the collection.  Also, return a flag
    !  indicating whether the pairs are successfully retrieved and removed or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashMap object
    CLASS(HashMap),      INTENT(INOUT)  :: Collection
    !% the keys to be retrieved and removed from the collection
    CLASS(*),            INTENT(INOUT)  :: Keys(:)
    !% the values associated with the keys
    CLASS(*),            INTENT(INOUT)  :: Values(:)
    !> a procedure to copy stored keys for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: KeyCopy
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !> flag indicating whether the items are successfully retrieved and removed. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! retrieve all items
    Success = Collection%GetAll(Keys, Values, KeyCopy, ValCopy)
    
    ! remove all items
    IF (Success) CALL Collection%Clear()

    RETURN

END FUNCTION HashMap_ToArray

!**************************************************************************************

FUNCTION HashMap_GetAll(Collection, Keys, Values, KeyCopy, ValCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all keys and/or all values (without removing them) from the collection.  Also,
    !  return a flag indicating whether the keys and/or values are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashMap object
    CLASS(HashMap),      INTENT(INOUT)  :: Collection
    !% the keys to be retrieved and removed from the collection
    CLASS(*), OPTIONAL,  INTENT(INOUT)  :: Keys(1:)
    !% the values associated with the keys
    CLASS(*), OPTIONAL,  INTENT(INOUT)  :: Values(1:)
    !> a procedure to copy stored keys for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: KeyCopy
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !> flag indicating whether the items are successfully retrieved. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: SameKeyType, SameValType

! FLOW

    ! check whether the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        Success = FalseVal
    ELSEIF (PRESENT(Keys).AND.PRESENT(Values)) THEN
        SameKeyType = Collection%IsKeyValid(Keys(1), IsOrderedKey=FalseVal)
        SameValType = Collection%IsValValid(Values(1))
        ! check whether types of the specified keys and values are valid or not
        IF (SameKeyType.AND.SameValType) THEN
            Success = Collection%GetAllKeys(Keys, KeyCopy)
            IF (Success) Success = Collection%GetAllVals(Values, ValCopy)
        ELSEIF (SameKeyType) THEN
            Success = FalseVal
            CALL Handle_ErrLevel('HashMap_GetAll', ModName, ErrSevere, &
                                 'Type of the specified values is NOT the same as that of stored values.')
        ELSEIF (SameValType) THEN
            Success = FalseVal
            CALL Handle_ErrLevel('HashMap_GetAll', ModName, ErrSevere, &
                                 'Type of the specified keys is NOT the same as that of stored keys.')
        ELSE
            Success = FalseVal
            CALL Handle_ErrLevel('HashMap_GetAll', ModName, ErrSevere, &
                                 'Types of both keys and values are NOT the same as those of stored pairs.')
        END IF
    ELSEIF (PRESENT(Keys)) THEN
        ! check whether type of the specified keys is valid or not
        IF (Collection%IsKeyValid(Keys(1), IsOrderedKey=FalseVal)) THEN
            Success = Collection%GetAllKeys(Keys, KeyCopy)
        ELSE
            Success = FalseVal
            CALL Handle_ErrLevel('HashMap_GetAll', ModName, ErrSevere, &
                                 'Type of the specified keys is NOT the same as that of stored keys.')
        END IF
    ELSEIF (PRESENT(Values)) THEN
        ! check whether type of the specified values is valid or not
        IF (Collection%IsValValid(Values(1))) THEN
            Success = Collection%GetAllVals(Values, ValCopy)
        ELSE
            Success = FalseVal
            CALL Handle_ErrLevel('HashMap_GetAll', ModName, ErrSevere, &
                                 'Type of the specified values is NOT the same as that of stored values.')
        END IF
    END IF

    RETURN

END FUNCTION HashMap_GetAll

!**************************************************************************************

FUNCTION HashMap_GetAllKeys(Collection, Keys, KeyCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all keys (without removing them) from the collection.  Also,
    !  return a flag indicating whether the keys are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashMap object
    CLASS(HashMap),      INTENT(INOUT)  :: Collection
    !% the keys to be retrieved and removed from the collection
    CLASS(*),            INTENT(INOUT)  :: Keys(:)
    !> a procedure to copy stored keys for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: KeyCopy
    !> flag indicating whether the keys are successfully retrieved. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                              :: I, J, ArrSize
    PROCEDURE(IfacePolyCopy), POINTER   :: CopyProc

! FLOW

    ! set a pointer to copy procedure
    IF (PRESENT(KeyCopy)) THEN
        CopyProc => KeyCopy
    ELSE
        CopyProc => Collection%KeyCopy
    END IF

    ! initialize local variables
    ArrSize = SIZE(Keys, KIND=kIndex)
    I = 1_kindex

    ! loop through all buckets
    DO J = 1_kIndex, SIZE(Collection%Items, KIND=kIndex)
        ! skip the rest if the bucket is empty
        IF (Collection%Items(J)%Stat /= STAT_GEN) CYCLE
        Success = Collection%Items(J)%Key%Get(Keys(I), CopyProc) 
        IF (.NOT.Success) THEN
            CALL Handle_ErrLevel('HashMap_GetAllKeys', ModName, ErrSevere, &
                        'Unable to retrieve a key.  This is likely a bug.')
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

END FUNCTION HashMap_GetAllKeys

!**************************************************************************************

FUNCTION HashMap_GetAllVals(Collection, Values, ValCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all values (without removing them) from the collection.  Also,
    !  return a flag indicating whether the values are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashMap object
    CLASS(HashMap),      INTENT(INOUT)  :: Collection
    !% the values to be retrieved and removed from the collection
    CLASS(*),            INTENT(INOUT)  :: Values(:)
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !> flag indicating whether the values are successfully retrieved. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                              :: I, J, ArrSize
    PROCEDURE(IfacePolyCopy), POINTER   :: CopyProc

! FLOW

    ! set a pointer to copy procedure
    IF (PRESENT(ValCopy)) THEN
        CopyProc => ValCopy
    ELSE
        CopyProc => Collection%ValCopy
    END IF

    ! initialize local variables
    ArrSize = SIZE(Values, KIND=kIndex)
    I = 1_kindex

    ! loop through all buckets
    DO J = 1_kIndex, SIZE(Collection%Items, KIND=kIndex)
        ! skip the rest if the bucket is empty
        IF (Collection%Items(J)%Stat /= STAT_GEN) CYCLE
        Success = Collection%Items(J)%Value%Get(Values(I), CopyProc) 
        IF (.NOT.Success) THEN
            CALL Handle_ErrLevel('HashMap_GetAllVals', ModName, ErrSevere, &
                        'Unable to retrieve a value.  This is likely a bug.')
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

END FUNCTION HashMap_GetAllVals

! -----------------------------------------------------------------------------
! -----                        Common Procedures                          -----
! -----------------------------------------------------------------------------

SUBROUTINE HashMap_Resize(Collection, MoreCap)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To resize the capacity of the hash table according the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(INOUT)   :: Collection   !! collection
    tLogical,       INTENT(IN)      :: MoreCap
    !^ true if increasing the capacity; otherwise, decreasing the capacity

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabItem), ALLOCATABLE  :: OldItems(:)
    tIndex                      :: OldCap, I
    tLogical                    :: Success
    CLASS(*), POINTER           :: KeyMold
    CLASS(*), POINTER           :: ValMold
    CLASS(*), ALLOCATABLE       :: Key
    CLASS(*), ALLOCATABLE       :: Value

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

    ! Reset the key count and buckets used since we are about to
    ! re-insert all the keys into the hash-table.
    Collection%KeyCount = 0_kIndex
    Collection%UsedBuckets = 0_kIndex

    ! loop over the temporary lists to move stored objects (Items)
    ! back to the working lists of the hash table
    KeyMold => Collection%GetKeyPtr()
    ValMold => Collection%GetValPtr()
    ALLOCATE(Key,   MOLD=KeyMold)
    ALLOCATE(Value, MOLD=ValMold)
    DO I = 0_kIndex, OldCap-1_kIndex
        IF (OldItems(I)%Stat == STAT_GEN) THEN
            ! get old key and value back
            Success = OldItems(I)%Key%Get(Key, Collection%ValCopy)
            IF (Success) Success = OldItems(I)%Value%Get(Value, Collection%ValCopy)
            ! re-insert the key and its associated value
            IF (Success) THEN
                CALL Collection%Insert(Key, Value)
            ELSE
                CALL Handle_ErrLevel('HashMap_Resize', ModName, ErrSevere, &
                    'Unable to get old key and/or value.  This is likely due to invalid copy procedure(s).')
                EXIT
            END IF
        END IF
        CALL OldItems(I)%MemFree()
    END DO

    ! free local variables
    DEALLOCATE(OldItems)
    NULLIFY(KeyMold, ValMold)
    DEALLOCATE(Key)
    DEALLOCATE(Value)

    RETURN

END SUBROUTINE HashMap_Resize

!******************************************************************************

FUNCTION HashMap_FindKey(Collection, Key, Value, ValCopy) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is currently stored in a symbol table.
    !  Optionally, retrieve the associated value if the key is found. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap),     INTENT(INOUT)   :: Collection   !! collection
    CLASS(*),           INTENT(IN)      :: Key          !! key to be looked for
    CLASS(*), OPTIONAL, INTENT(OUT)     :: Value        !! associated value
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !% flag indicating whether the specified key is found or not.
    tLogical                            :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tHash       :: HashCode
    tIndex      :: Indx, Offset, I, J
    tIndex      :: KeySize
    tLogical    :: Success

! FLOW

    ! set up the probing if needed
    CALL SetupProbing(Collection, Key)

    ! compute the hash code and offset
    KeySize  = AnyType_GetByteSize(Key)
    HashCode = Collection%HashCalc(Key, KeySize, HashSeed, RemoveSign=TrueVal)
    Offset   = ComputeIndex(HashCode, Collection%Capacity)

    ! initialize indices
    I = Offset
    J = -1_kIndex
    Indx = 1_kIndex
    ! Start at the original hash value and probe until we find a spot where our key
    ! is or hit a null element in which case our element does not exist.
    DO
        IF (Collection%Items(I)%Stat == STAT_DEL) THEN
            ! Ignore deleted buckets, but record where the first index
            ! of a deleted bucket is found to perform lazy relocation later.
            IF (J == -1_kIndex) J = I
        ELSEIF (Collection%Items(I)%Stat /= STAT_NUL) THEN
            ! found the key we want to remove
            IF (J /= -1_kIndex) THEN
                ! If J /= -1 this means we previously encountered a deleted cell.
                ! We can perform an optimization by swapping the entries in cells
                ! I and J so that the next time we search for this key it will be
                ! found faster. This is called lazy deletion/relocation.
                CALL Collection%Items(J)%Set(STAT_GEN, Collection%Items(I))
                CALL Collection%Items(I)%Set(STAT_DEL)
                IF (PRESENT(Value)) Success = Collection%Items(J)%Value%Get(Value, ValCopy)
            ELSE
                IF (PRESENT(Value)) Success = Collection%Items(I)%Value%Get(Value, ValCopy)
            END IF
            Found = TrueVal
            EXIT
        ELSE
            ! the key was not found
            Found = FalseVal
            EXIT
        END IF
        I = ComputeIndex(Offset + Probe(Collection, Indx), Collection%Capacity)
        Indx = Indx + 1_kIndex
    END DO

    RETURN

END FUNCTION HashMap_FindKey

!******************************************************************************

SUBROUTINE HashMap_CreateEmpty(Collection, InitCap, LoadFactor, ProbAlgo, HashCalc, KeyCopy, ValCopy)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To create an empty hash table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap),      INTENT(INOUT)  :: Collection   !! collection
    tIndex,  OPTIONAL,   INTENT(IN)     :: InitCap      !! initial capacity of the hash table
    tRealDP, OPTIONAL,   INTENT(IN)     :: LoadFactor   !! load factor
    !% probing algorithm (1 = Linear, 2 = Quadratic, 3 = Double Hashing)
    tSInt32, OPTIONAL,   INTENT(IN)     :: ProbAlgo
    !> hash function to compute the hash value of the key; if not present, use default one.
    PROCEDURE(HashFunc), OPTIONAL       :: HashCalc
    !> a procedure to copy stored keys for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: KeyCopy
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy

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

    ! set pointer to copy procedures
    IF (PRESENT(KeyCopy)) THEN
        Collection%KeyCopy => KeyCopy
    ELSE
        Collection%KeyCopy => NULL()
    END IF
    IF (PRESENT(ValCopy)) THEN
        Collection%ValCopy => ValCopy
    ELSE
        Collection%ValCopy => NULL()
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

END SUBROUTINE HashMap_CreateEmpty

!******************************************************************************

SUBROUTINE HashMap_CreateByArray(Collection, N, Keys, Values, LoadFactor, ProbAlgo, &
                                 HashCalc, KeyCopy, ValCopy)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a table from an array of key-value pairs.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(INOUT)       :: Collection   !! collection
    tIndex,         INTENT(IN)          :: N            !! number of key-value pairs
    !% the keys to be added to the table
    CLASS(*),       INTENT(IN)          :: Keys(N)
    !% the associated values to be added to the table
    CLASS(*),       INTENT(IN)          :: Values(N)
    tRealDP, OPTIONAL,   INTENT(IN)     :: LoadFactor   !! load factor
    !% probing algorithm (1 = Linear, 2 = Quadratic, 3 = Double Hashing)
    tSInt32, OPTIONAL,   INTENT(IN)     :: ProbAlgo
    !> hash function to compute the hash value of the key; if not present, use default one.
    PROCEDURE(HashFunc), OPTIONAL       :: HashCalc
    !> a procedure to copy stored keys for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: KeyCopy
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! create empty symbol table with capacity twice of the key size
    CALL Collection%CreateEmpty(N*2_kIndex, LoadFactor, ProbAlgo, HashCalc, KeyCopy, ValCopy)

    ! add key-value pairs to the table
    DO I = 1_kIndex, N
        CALL Collection%Insert(Keys(I), Values(I))
    END DO

    RETURN

END SUBROUTINE HashMap_CreateByArray

!******************************************************************************

SUBROUTINE HashMap_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To perform finalization of the collection.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(HashMap), INTENT(INOUT)    :: Collection   !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE HashMap_Finalize

! -----------------------------------------------------------------------------
! -----             Procedures Dependent On Probing Algorithm             -----
! -----------------------------------------------------------------------------

SUBROUTINE SetupProbing(Collection, Key)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set up the probing according to the probing algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap), INTENT(INOUT)   :: Collection    !! collection
    CLASS(*),       INTENT(IN)      :: Key      !! key

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
    CLASS(HashMap), INTENT(INOUT)   :: Collection    !! collection
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
    CLASS(HashMap), INTENT(INOUT)   :: Collection    !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Collection%ProbAlgo)
    CASE (LinearProbing)
        ! Adjust the capacity so that the linear constant and
        ! the table capacity are relatively prime.
        DO WHILE (ComputeGCD(LinearConstant, Collection%Capacity) /= 1_kIndex)
            Collection%Capacity = Collection%Capacity + 1_kIndex
        END DO
    CASE (QuadraticProbing)
        ! Adjust the capacity of the hash table to be a power of two.
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
    CLASS(HashMap), INTENT(INOUT)   :: Collection    !! collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Collection%ProbAlgo)
    CASE (LinearProbing)
        ! doubling the capacity
        Collection%Capacity = Collection%Capacity*2_kIndex + 1_kIndex
    CASE (QuadraticProbing)
        ! increase the capacity of the hash table to the next power of two.
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
    !^ To compute the index of working items of the hash table for
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

!** UNDEFINE MACROS **
#undef tHash

END MODULE MClass_HashMap

!******************************************************************************
