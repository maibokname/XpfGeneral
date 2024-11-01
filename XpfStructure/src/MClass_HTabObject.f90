
MODULE MClass_HTabObject

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *HTabObject* type, and its related routines.   The
!   *HTabObject* type is a container type that employs an open-addressing hash table
!   implementation to provide common operations for an unordered symbol table.  As a
!   symbol table, the *HTabObject* type uses the *Object* derived type to store the
!   keys and their associated values.  Unlike other hash-table containers, the
!  *HTabObject* type uses a user-defined type in the *Object* class to represent
!   a key-value pair and requires only one argument (instead of two) when inserting
!   or retrieving the key and its associated value. <br>
!   It should be noted that a user must be careful when implementing a *user-defined
!   concrete* subtype of the *Object* type.  The *HTabObject* type employs the
!   assignment statement copy data of the key-value object.  It also utilizes the
!   relational operators (e.g. ==) to compare keys of the key-value objects.  Moreover,
!   in order to compute indices of buckets used to store the specified key-value object,
!   it uses the *HashCode* method to compute the hash value of the specified key.  These
!   imply that the user should implement the deferred *CopyAssign* procedure where both
!   key and value components are copied from the source object to the destination object
!   whereas, when implemented, the deferred *CompareTo* and *ComputeHashValue* procedures
!   should be dependent on its key component only. <br>
!   It is also worth mentioning that the *HTabObject* type does not allow key-value objects
!   of different types to be stored in the same container.  The *HTabObject* type requires
!   a user to specify the type of user-defined key-value objects to be stored via arguments
!   specified during the creation of a container.  Also, the type of a user-define key-value
!   object specified in all other routines must be the same as the type of stored objects of
!   the container. Otherwise, the container may not behave as expected. <br>
!   Unlike the *list-based* and *tree-based* types, which can be used instantly by inserting
!   objects into a container, the *HTabObject* type requires an explicit construction before
!   using other provided operations.  There are two methods provided to create the container.
!   The *CreateEmpty* method constructs an empty table with optional multiple arguments
!   (including an initial capacity, a load factor, a probing algorithm, and a hash function
!   used to compute a hash code of a key) whereas the *Construct* method constructs a table
!   from an array of key-value pairs. <br>
!   As a symbol table, the *HTabObject* type does not allow duplicated keys.  Therefore, if
!   an inserted key is equal to a key stored in the table, an associated value of the stored
!   key is replaced by an associated value of the inserted key. <br>
!   Technically, the *HTabObject* type employs the open-addressing as a collision resolution
!   technique where the hash resolution is performed through probing.  It provides three probing
!   algorithms: linear probing, quadratic probing and double hashing.  By default, the linear
!   probing algorithm is used.  However, a user can specify other probing algorithm during the
!   construction of the container. <br>

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define     tHash           tIndex
!----------------------------------------------------------
#define     HashTable       HTabObject
#define     QueueKeyVal     ListObject
!----------------------------------------------------------

!** USE STATEMENTS:
    USE MBase_Common
    USE ISO_C_BINDING,          ONLY: C_SIZEOF
    USE MBase_ErrHandlers
    USE MBase_SIntUtil
    USE MBase_MathUtil
    USE MBase_DoublyLinkedLists
    USE MClass_Object
#ifdef Indx32Bits
    USE MBase_SimpleHash32, ONLY: HashFuncDefault => Hash32_FNV1a
#else
    USE MBase_SimpleHash64, ONLY: HashFuncDefault => Hash64_FNV1a
#endif

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: HTabObject

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_HTabObject'
    ! default capacity
    tIndex,    PARAMETER    :: DefaultCapacity   = 7
    ! default load factor
    tRealDP,   PARAMETER    :: DefaultLoadFactor = 0.65_kDouble
    tSInt32,   PARAMETER    :: LinearProbing     = 1
    tSInt32,   PARAMETER    :: QuadraticProbing  = 2
    tSInt32,   PARAMETER    :: DoubleHashing     = 3
    ! This is the linear constant used in the linear probing, it can be
    ! any positive number. The table capacity will be adjusted so that
    ! the GCD(capacity, LinearConstant) = 1 so that all buckets can be probed.
    tIndex,    PARAMETER    :: LinearConstant    = 17_kIndex
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
#ifdef Indx32Bits
    tSInt32,   PARAMETER    :: MaxHash = ToInt32(Z'7FFFFFFF')
#else
    tSInt64,   PARAMETER    :: MaxHash = ToInt64(Z'7FFFFFFFFFFFFFFF')
#endif

!** DERIVED TYPE DEFINITIONS
    !> The *HashTable* type is a table type that employs an open-addressing hash table
    !  implementation to provide common operations for an unordered symbol table.
    TYPE HashTable
        PRIVATE
        !% current capacity of the hash table
        tIndex                      :: Capacity = DefaultCapacity
        !% working table buckets used to store key-value pairs
        CLASS(Object), ALLOCATABLE  :: Buckets(:)
        !% Special marker token used to indicate the deletion of a key-value pair
        CLASS(Object), ALLOCATABLE  :: DELOBJ
        !% Special marker token used to indicate the empty of a key-value pair
        CLASS(Object), ALLOCATABLE  :: NULOBJ
        !% current index into the working buckets (used for iteration purpose)
        tIndex                      :: Indx = 0_kIndex
        !% the number of keys not yet visited (used for iteration purpose)
        tIndex                      :: KeyLeft = 0_kIndex
        !% current modification count (used for iteration purpose)
        tIndex                      :: IterModCount = 0_kIndex
        !% load factor
        tRealDP                     :: LoadFactor = DefaultLoadFactor
        !% threshold for resizing
        tIndex                      :: Threshold = 0_kIndex
        !% modification count
        tIndex                      :: ModCount = 0_kIndex
        !% the total number of used buckets inside the hash table (including cells marked as deleted).
        tIndex                      :: UsedBuckets = 0_kIndex
        !% the total number of unique keys currently inside the hash table.
        tIndex                      :: KeyCount = 0_kIndex
        !% probing algorithm
        tSInt32                     :: ProbAlgo = LinearProbing
        !% index for double hashing
        tHash                       :: HashIndx = 0_kIndex
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: FindKey <br>
        !  **Purpose**:  To find the specified key in the table.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%FindKey(KeyVal) <br>
        !   --->    IF (.NOT.Table%FindKey(KeyVal)) DoSomething
        PROCEDURE, PRIVATE  :: FindKey      => HashTable_FindKey
        !> **Type-Bound Subroutine**: Resize <br>
        !  **Purpose**:  To resize the table to the specified capacity. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Resize(64)
        PROCEDURE, PRIVATE  :: Resize       => HashTable_Resize
        ! ---------------------------------------------------------------------
        ! -----                      Public Procedures                    -----
        ! ---------------------------------------------------------------------
        ! -----     constructor and destructor procedures   -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%CreateEmpty(NulObj, DelObj)                  ! use default options  <br>
        !   --->    CALL Table%CreateEmpty(NulObj, DelObj, InitCap=25)      ! specify initial capacity <br>
        !   --->    CALL Table%CreateEmpty(NulObj, DelObj, LoadFactor=0.5)  ! specify load factor <br>
        !   --->    CALL Table%CreateEmpty(NulObj, DelObj, ProbAlgo=2)      ! specify probing algorithm <br>
        !   --->    CALL Table%CreateEmpty(NulObj, DelObj, 30, 0.75, 3)     ! specify all options <br>
        !  **Important Note**: <br>
        !  A user is required to specify two user-defined key-value pair objects to represent a null
        !  object and a deleted object where these two objects are not the same one when compared using
        !  the *operator ==*.  Also, these two objects should never be specified in any operations other
        !  than the construction operations. <br>
        PROCEDURE   :: CreateEmpty  => HashTable_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a hash table from the specified key-value array. <br>
        !  **Usage**: <br>
        !   ! use default options  <br>
        !   --->    CALL Table%Construct(40, KeyValArr, NulObj, DelObj) <br>
        !   ! specify all options (initial capacity is twice of the size of the given arrays) <br>
        !   --->    CALL Table%Construct(20, KeyValArr, NulObj, DelObj, LoadFactor, ProbAlgo) <br>
        !  **Important Note**: <br>
        !  A user is required to specify two user-defined key-value pair objects to represent a null
        !  object and a deleted object where these two objects are not the same one when compared using
        !  the *operator ==*.  Also, these two objects should never be specified in any operations other
        !  than the construction operations. <br>
        PROCEDURE   :: Construct    => HashTable_CreateByArray
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To free components of the buckets from the table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Clear()
        PROCEDURE   :: Clear        => HashTable_ClearBuckets
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To free all memory currently used by the table.<br>
        !  **Usage**: <br>
        !   --->    CALL Table%Destruct() <br>
        !  **Note**:  This method is NOT equivalent to the *Clear* method.  After the *Clear* method
        !       is called, other methods (the *Insert* method in particular) can be immediately used.
        !       However, after the *Destruct* method is called, the *Construct* method must be called
        !       again before other methods can be used. <br>
        PROCEDURE   :: Destruct     => HashTable_Destroy
        ! -------------------------------------------------------
        ! -----         adding and removing procedures      -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key-value pair to the table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Insert(KeyVal) <br>
        PROCEDURE   :: Insert       => HashTable_Insert
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified key (and its associated value) from the table.
        !                Also, return a flag indicating whether the key-value pair is successfully
        !                removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%Remove(KeyVal) <br>
        !   --->    IF (.NOT.Table%Remove(KeyVal)) DoSomething
        PROCEDURE   :: Remove       => HashTable_Remove
        ! -------------------------------------------------------
        ! -----           tree-traversing procedures        -----
        ! -------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start an iteration and return a flag indicating whether
        !                the table is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Table%StartFirst() <br>
        !   --->    IsEmpty = Table%StartFirst(FirstKeyVal) <br>
        PROCEDURE   :: StartFirst   => HashTable_Move2FirstPair
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next key-value pair and return a flag
        !                indicating whether the cursor pointer has reached the end of
        !                the table or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Table%MoveForward() <br>
        !   --->    IsTheEnd = Table%MoveForward(NextKeyVal) <br>
        !  **Important Note**: <br>
        !   After the start of the current iteration (i.e. a call to the *StartFirst* method),
        !   a user should not insert or remove any key-value pair.  Otherwise, the *MoveForward*
        !   method is not valid for the current iteration and the user must re-start the iteration
        !   in order to use the *MoveForward* method.
        PROCEDURE   :: MoveForward  => HashTable_Move2NextPair
        ! -------------------------------------------------------
        ! -----               inquiry procedures            -----
        ! -------------------------------------------------------
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the table is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%IsEmpty() <br>
        !   --->    IF (.NOT.Table%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty      => HashTable_IsEmpty
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size (number of keys and their associated values) of the table. <br>
        !  **Usage**: <br>
        !   --->    Size = Table%GetSize()
        PROCEDURE   :: GetSize      => HashTable_GetSize
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified key in the table.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%Contain(KeyVal) <br>
        !   --->    IF (.NOT.Table%Contain(KeyVal)) DoSomething
        PROCEDURE   :: Contain      => HashTable_Contain
        !> **Type-Bound Subroutine**: GetKeys <br>
        !  **Purpose**:  To return a queue of all key-value pairs. <br>
        !  **Usage**: <br>
        !   --->    CALL Tree%GetKeys(KeyValQueue) <br>
        PROCEDURE   :: GetKeys      => HashTable_GetAllKeys
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the hash table.
        FINAL       :: HashTable_Finalize
        ! ---------------------------------------------------------------------
    END TYPE HashTable

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! -----------------------------------------------------------------------------
! -----                        Common Procedures                          -----
! -----------------------------------------------------------------------------

SUBROUTINE HashTable_CreateEmpty(Table, NulObj, DelObj, InitCap, LoadFactor, ProbAlgo)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To create an empty hash table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTable object
    CLASS(HashTable),  INTENT(INOUT)    :: Table
    !> a user-defined object representing a null object; the user should never specify this
    !  object in any operations other than the construction operations.
    CLASS(Object),     INTENT(IN)       :: NulObj
    !> a user-defined object representing a deleted object (different from the null object); the user
    !  should never specify this object in any operations other than the construction operations.
    CLASS(Object),     INTENT(IN)       :: DelObj
    !% initial capacity of the hash table
    tIndex,  OPTIONAL, INTENT(IN)       :: InitCap
    !% load factor
    tRealDP, OPTIONAL, INTENT(IN)       :: LoadFactor
    !% probing algorithm (1 = Linear, 2 = Quadratic, 3 = Double Hashing)
    tSInt32, OPTIONAL, INTENT(IN)       :: ProbAlgo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! check and return quickly if needed
    IF (NulObj%IsEqualTo(DelObj)) THEN
        CALL Handle_ErrLevel('HashTable_CreateEmpty', ModName, ErrWarning, &
                'The null object must be different from the deleted object.')
        RETURN
    ELSEIF (.NOT.SAME_TYPE_AS(NulObj, DelObj)) THEN
        CALL Handle_ErrLevel('HashTable_CreateEmpty', ModName, ErrWarning, &
                'Types of the null and deleted objects must be the same.')
        RETURN
    END IF

    ! determine initial capacity
    IF (PRESENT(InitCap)) THEN
        IF (InitCap < 0_kIndex) THEN
            Table%Capacity = DefaultCapacity
        ELSE
            Table%Capacity = MAX(DefaultCapacity, InitCap)
        END IF
    ELSE
        Table%Capacity = DefaultCapacity
    END IF

    ! determine load factor
    IF (PRESENT(LoadFactor)) THEN
        IF (InitCap < 0.0_kDouble) THEN
            Table%LoadFactor = DefaultLoadFactor
        ELSE
            Table%LoadFactor = LoadFactor
        END IF
    ELSE
        Table%LoadFactor = DefaultLoadFactor
    END IF

    ! determine probing algorithm
    IF (PRESENT(ProbAlgo)) THEN
        SELECT CASE (ProbAlgo)
        CASE (1:3)
            Table%ProbAlgo = ProbAlgo
        CASE DEFAULT
            Table%ProbAlgo = LinearProbing
        END SELECT
    ELSE
        Table%ProbAlgo = LinearProbing
    END IF

    ! adjust the capacity according to the probing algorithm
    CALL AdjustCapacity(Table)

    ! compute threshold
    Table%Threshold = ToIndex(Table%Capacity*Table%LoadFactor)

    ! allocate module variables
    ALLOCATE(Table%NULOBJ, SOURCE=NulObj)
    ALLOCATE(Table%DELOBJ, SOURCE=DelObj)

    ! allocate memory of bucket storages
    ALLOCATE(Table%Buckets(0:Table%Capacity-1), SOURCE=Table%NULOBJ)

    RETURN

END SUBROUTINE HashTable_CreateEmpty

!******************************************************************************

SUBROUTINE HashTable_CreateByArray(Table, N, KeyVals, NulObj, DelObj, LoadFactor, ProbAlgo)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a table from an array of key-value pairs.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTable object
    CLASS(HashTable),  INTENT(INOUT)    :: Table
    !% number of key-value pairs
    tIndex,            INTENT(IN)       :: N
    !% the key-value pairs to be added to the table
    CLASS(Object),     INTENT(IN)       :: KeyVals(:)
    !> a user-defined object representing a null object; the user should never specify this
    !  object in any operations other than the construction operations.
    CLASS(Object),     INTENT(IN)       :: NulObj
    !> a user-defined object representing a deleted object (different from the null object); the user
    !  should never specify this object in any operations other than the construction operations.
    CLASS(Object),     INTENT(IN)       :: DelObj
    !% load factor
    tRealDP, OPTIONAL, INTENT(IN)       :: LoadFactor
    !% probing algorithm (1 = Linear, 2 = Quadratic, 3 = Double Hashing)
    tSInt32, OPTIONAL, INTENT(IN)       :: ProbAlgo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! check and return quickly if needed
    IF (NulObj%IsEqualTo(DelObj)) THEN
        CALL Handle_ErrLevel('HashTable_CreateByArray', ModName, ErrWarning, &
                'The null object must be different from the deleted object')
        RETURN
    ELSEIF (.NOT.SAME_TYPE_AS(NulObj, DelObj).OR..NOT.SAME_TYPE_AS(NulObj, KeyVals)) THEN
        CALL Handle_ErrLevel('HashTable_CreateByArray', ModName, ErrWarning, &
                'Types of the null and deleted objects must be the same.')
        RETURN
    END IF

    ! create empty symbol table with capacity twice of the key size
    CALL Table%CreateEmpty(NulObj, DelObj, N*2_kIndex, LoadFactor, ProbAlgo)

    ! add key-value pairs to the table
    DO I = 1_kIndex, N
        CALL Table%Insert(KeyVals(I))
    END DO

    RETURN

END SUBROUTINE HashTable_CreateByArray

!******************************************************************************

SUBROUTINE HashTable_ClearBuckets(Table)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To free components of the buckets from the table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

!** FLOW:

    DO I = 0_kIndex, Table%Capacity-1_kIndex
        Table%Buckets(I) = Table%NULOBJ
    END DO
    Table%KeyCount = 0_kIndex
    Table%UsedBuckets = 0_kIndex
    Table%ModCount = Table%ModCount + 1_kIndex

    RETURN

END SUBROUTINE HashTable_ClearBuckets

!******************************************************************************

SUBROUTINE HashTable_Destroy(Table)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! first free memory storages
    IF (ALLOCATED(Table%Buckets)) THEN
        BLOCK
            tIndex      :: I
            DO I = 0_kIndex, Table%Capacity-1_kIndex
                CALL Table%Buckets(I)%MemFree()
            END DO
        END BLOCK
        DEALLOCATE(Table%Buckets)
    END IF
    IF (ALLOCATED(Table%NULOBJ)) DEALLOCATE(Table%NULOBJ)
    IF (ALLOCATED(Table%DELOBJ)) DEALLOCATE(Table%DELOBJ)

    ! reset all components
    Table%Capacity = DefaultCapacity
    Table%Indx = 0_kIndex
    Table%KeyLeft = 0_kIndex
    Table%IterModCount = 0_kIndex
    Table%LoadFactor = DefaultLoadFactor
    Table%Threshold = 0_kIndex
    Table%ModCount = 0_kIndex
    Table%UsedBuckets = 0_kIndex
    Table%KeyCount = 0_kIndex
    Table%ProbAlgo = LinearProbing
    Table%HashIndx = 0_kIndex

    RETURN

END SUBROUTINE HashTable_Destroy

!******************************************************************************

SUBROUTINE HashTable_Finalize(Table)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To perform finalization of the HashTable object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(HashTable), INTENT(INOUT) :: Table    !! HashTable object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    CALL Table%Destruct()

    RETURN

END SUBROUTINE HashTable_Finalize

!******************************************************************************

FUNCTION HashTable_GetSize(Table) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the number of keys currently in the hash table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(IN)    :: Table    !! HashTable object
    tIndex                          :: Size     !! the number of keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Table%KeyCount

    RETURN

END FUNCTION HashTable_GetSize

!******************************************************************************

FUNCTION HashTable_IsEmpty(Table) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the hash table is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(IN)    :: Table    !! HashTable object
    tLogical                        :: Flag     !! true if the table is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = (Table%KeyCount == 0_kIndex)

    RETURN

END FUNCTION HashTable_IsEmpty

!******************************************************************************

SUBROUTINE HashTable_Resize(Table, MoreCap)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To resize the capacity of the hash table according the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object
    tLogical,         INTENT(IN)    :: MoreCap
    !^ true if increasing the capacity; otherwise, decreasing the capacity

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(Object), ALLOCATABLE    :: OldBuckets(:)
    tIndex                          :: OldCap, I

!** FLOW:

    OldCap = Table%Capacity
    IF (MoreCap) THEN
        CALL IncreaseCapacity(Table)
    ELSE
        ! halving the capacity
        Table%Capacity = Table%Capacity/2_kIndex
    END IF
    CALL AdjustCapacity(Table)

    ! update threshold
    Table%Threshold = ToIndex(Table%Capacity*Table%LoadFactor)

    ! move currently stored objects to temporary variable
    CALL MOVE_ALLOC(Table%Buckets, OldBuckets)

    ! allocate working buckets to new capacity
    ALLOCATE(Table%Buckets(0:Table%Capacity-1_kIndex), SOURCE=Table%NULOBJ)

    ! Reset the key count and buckets used since we are about to
    ! re-insert all the keys into the hash-table.
    Table%KeyCount = 0_kIndex
    Table%UsedBuckets = 0_kIndex

    ! loop over the temporary lists to move stored objects (Buckets)
    ! back to the working lists of the hash table
    DO I = 0_kIndex, OldCap-1_kIndex
        IF ((.NOT.OldBuckets(I)%IsEqualTo(Table%NULOBJ)).AND.(.NOT.OldBuckets(I)%IsEqualTo(Table%DELOBJ))) THEN
            ! re-insert the key and its associated value
            CALL Table%Insert(OldBuckets(I))
        END IF
        CALL OldBuckets(I)%MemFree()
    END DO

    ! free temporary variable
    DEALLOCATE(OldBuckets)

    RETURN

END SUBROUTINE HashTable_Resize

!******************************************************************************

SUBROUTINE HashTable_Insert(Table, KeyVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a key-value pair into the hash table.  If the specified key
    !  is already stored in the table, replace the old value with the
    !  new one.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object
    CLASS(Object),    INTENT(IN)    :: KeyVal   !! key to be inserted and its associated value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tHash       :: HashCode
    tIndex      :: Indx, Offset, I, J

! FLOW

    ! check and return quickly if needed
    IF ((KeyVal%IsEqualTo(Table%NULOBJ)).OR.(KeyVal%IsEqualTo(Table%DELOBJ))) THEN
        CALL Handle_ErrLevel('HashTable_Insert', ModName, ErrWarning, &
                'The inserted object must not be a null or a deleted one.')
        RETURN
    ELSEIF (.NOT.SAME_TYPE_AS(KeyVal, Table%NULOBJ)) THEN
        CALL Handle_ErrLevel('HashTable_Insert', ModName, ErrWarning, &
                'Type of the inserted object must be the same as that of stored objects.')
        RETURN
    END IF

    ! resize the capacity if needed
    IF (Table%UsedBuckets >= Table%Threshold) THEN
        CALL Table%Resize(MoreCap=TrueVal)
    END IF

    ! set up the probing if needed
    CALL SetupProbing(Table, KeyVal)

    ! compute the hash code and offset
    HashCode = KeyVal%HashCode()
    Offset   = ComputeIndex(HashCode, Table%Capacity)

    ! initialize indices
    I = Offset
    J = -1_kIndex
    Indx = 1_kIndex
    DO
        IF (Table%Buckets(I)%IsEqualTo(Table%DELOBJ)) THEN
            ! The current bucket was previously deleted
            IF (J == -1_kIndex) J = I
        ELSEIF (.NOT.Table%Buckets(I)%IsEqualTo(Table%NULOBJ)) THEN
            ! The current bucket already contains a key
            IF (Table%Buckets(I)%IsEqualTo(KeyVal)) THEN
                ! The key we're trying to insert already exists in the hash-table,
                ! so update its value with the most recent value
                IF (J == -1_kIndex) THEN
                    Table%Buckets(I) = KeyVal
                ELSE
                    Table%Buckets(I) = Table%DELOBJ
                    Table%Buckets(J) = KeyVal
                END IF
                Table%ModCount = Table%ModCount + 1_kIndex
                EXIT
            END IF
        ELSE
            ! The current bucket is null so an insertion/update can occur
            IF (J == -1_kIndex) THEN
                ! No previously encountered deleted buckets
                Table%UsedBuckets = Table%UsedBuckets + 1_kIndex
                Table%Buckets(I) = KeyVal
            ELSE
                ! Previously seen deleted bucket. Instead of inserting
                ! the new element at i where the null element is insert
                ! it where the deleted token was found.
                Table%Buckets(J) = KeyVal
            END IF
            Table%KeyCount = Table%KeyCount + 1_kIndex
            Table%ModCount = Table%ModCount + 1_kIndex
            EXIT
        END IF
        I = ComputeIndex(Offset + Probe(Table, Indx), Table%Capacity)
        Indx = Indx + 1_kIndex
    END DO

    RETURN

END SUBROUTINE HashTable_Insert

!******************************************************************************

FUNCTION HashTable_Remove(Table, KeyVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the specified key (and its associated value) from a symbol
    !  table.  Also, return a flag indicating whether the key-value pair is
    !  successfully removed or not.  Optionally, retrieve the associated
    !  value if the key exists in the table.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object
    CLASS(Object),    INTENT(IN)    :: KeyVal   !! key to be removed and its associated value
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tHash       :: HashCode
    tIndex      :: Indx, Offset, I

! FLOW

    ! check and return quickly if needed
    IF ((KeyVal%IsEqualTo(Table%NULOBJ)).OR.(KeyVal%IsEqualTo(Table%DELOBJ))) THEN
        Flag = FalseVal
        CALL Handle_ErrLevel('HashTable_Remove', ModName, ErrWarning, &
                'The removed object must not be a null or a deleted one.')
        RETURN
    ELSEIF (.NOT.SAME_TYPE_AS(KeyVal, Table%NULOBJ)) THEN
        Flag = FalseVal
        CALL Handle_ErrLevel('HashTable_Remove', ModName, ErrWarning, &
                'Type of the removed object must be the same as that of stored objects.')
        RETURN
    END IF

    IF (Table%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    END IF

    ! set up the probing if needed
    CALL SetupProbing(Table, KeyVal)

    ! compute the hash code and offset
    HashCode = KeyVal%HashCode()
    Offset   = ComputeIndex(HashCode, Table%Capacity)

    ! initialize indices
    I = Offset
    Indx = 1_kIndex
    ! Starting at the original hash probe until we find a spot where our key is
    ! or we hit a null element in which case our element does not exist.
    DO
        IF (Table%Buckets(I)%IsEqualTo(Table%DELOBJ)) THEN
            ! ignore deleted bucket so do nothing here
        ELSEIF (Table%Buckets(I)%IsEqualTo(Table%NULOBJ)) THEN
            ! the key was not found
            Flag = FalseVal
            EXIT
        ELSEIF (Table%Buckets(I)%IsEqualTo(KeyVal)) THEN
            ! found the key we want to remove
            Table%KeyCount = Table%KeyCount - 1_kIndex
            Table%ModCount = Table%ModCount + 1_kIndex
            Table%Buckets(I) = Table%DELOBJ
            Flag = TrueVal
            EXIT
        END IF
        I = ComputeIndex(Offset + Probe(Table, Indx), Table%Capacity)
        Indx = Indx + 1_kIndex
    END DO

    ! halve the hash table capacity if it is 12.5% full or less
    IF ((Table%KeyCount > 0_kIndex).AND.(Table%KeyCount <= Table%Capacity/8)) THEN
        CALL Table%Resize(MoreCap=FalseVal)
    END IF

    RETURN

END FUNCTION HashTable_Remove

!******************************************************************************

FUNCTION HashTable_Contain(Table, Key) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is currently stored in a symbol table.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object
    CLASS(Object),    INTENT(IN)    :: Key      !! key to be looked for
    !% flag indicating whether the specified key is found or not.
    tLogical                        :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check and return quickly if needed
    IF ((Key%IsEqualTo(Table%NULOBJ)).OR.(Key%IsEqualTo(Table%DELOBJ))) THEN
        Found = FalseVal
        CALL Handle_ErrLevel('HashTable_Contain', ModName, ErrWarning, &
                'The key to be looked for must not be a null or a deleted one.')
        RETURN
    ELSEIF (.NOT.SAME_TYPE_AS(Key, Table%NULOBJ)) THEN
        Found = FalseVal
        CALL Handle_ErrLevel('HashTable_Contain', ModName, ErrWarning, &
                'Type of the specified key must be the same as that of stored objects.')
        RETURN
    END IF

    IF (Table%IsEmpty()) THEN
        Found = FalseVal
    ELSE
        ! find the key
        Found = Table%FindKey(Key)
    END IF

    RETURN

END FUNCTION HashTable_Contain

!******************************************************************************

FUNCTION HashTable_FindKey(Table, KeyVal) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is currently stored in a symbol table.
    !  Optionally, retrieve the associated value if the key is found. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object
    CLASS(Object),    INTENT(IN)    :: KeyVal   !! key to be looked for and its associated value
    !% flag indicating whether the specified key is found or not.
    tLogical                        :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tHash       :: HashCode
    tIndex      :: Indx, Offset, I, J

! FLOW

    ! set up the probing if needed
    CALL SetupProbing(Table, KeyVal)

    ! compute the hash code and offset
    HashCode = KeyVal%HashCode()
    Offset   = ComputeIndex(HashCode, Table%Capacity)

    ! initialize indices
    I = Offset
    J = -1_kIndex
    Indx = 1_kIndex
    ! Start at the original hash value and probe until we find a spot where our key
    ! is or hit a null element in which case our element does not exist.
    DO
        IF (Table%Buckets(I)%IsEqualTo(Table%DELOBJ)) THEN
            ! Ignore deleted buckets, but record where the first index
            ! of a deleted bucket is found to perform lazy relocation later.
            IF (J == -1_kIndex) J = I
        ELSEIF (.NOT.Table%Buckets(I)%IsEqualTo(Table%NULOBJ)) THEN
            ! found the key we want to remove
            IF (J /= -1_kIndex) THEN
                ! If J /= -1 this means we previously encountered a deleted cell.
                ! We can perform an optimization by swapping the entries in cells
                ! I and J so that the next time we search for this key it will be
                ! found faster. This is called lazy deletion/relocation.
                Table%Buckets(J) = Table%Buckets(I)
                Table%Buckets(I) = Table%DELOBJ
            END IF
            Found = TrueVal
            EXIT
        ELSE
            ! the key was not found
            Found = FalseVal
            EXIT
        END IF
        I = ComputeIndex(Offset + Probe(Table, Indx), Table%Capacity)
        Indx = Indx + 1_kIndex
    END DO

    RETURN

END FUNCTION HashTable_FindKey

!******************************************************************************

FUNCTION HashTable_Move2FirstPair(Table, KeyVal) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the first (starting) pair data in a hash table.   For the hash table,
    !  which is an unordered symbol table, the starting pair is the first pair found
    !  in the non-empty bucket.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable),        INTENT(INOUT)  :: Table    !! HashTable object
    !% the first key and its associated value as output if requested (and available)
    CLASS(Object), OPTIONAL, INTENT(OUT)    :: KeyVal
    !> a flag indicating whether the table contains no pair data or not <br>
    ! - true if the table is empty. <br>
    ! - otherwise the first pair data is available.
    tLogical                                :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Table%IsEmpty()) THEN
        IsEmpty = TrueVal
        RETURN
    ELSE
        IsEmpty = FalseVal
    END IF

    ! initialize iteration-related components
    Table%Indx = 0_kIndex
    Table%KeyLeft = Table%KeyCount
    Table%IterModCount = Table%ModCount

    ! start iteration by looking for the first non-empty slot
    DO WHILE ((Table%Buckets(Table%Indx)%IsEqualTo(Table%NULOBJ)).OR. &
              (Table%Buckets(Table%Indx)%IsEqualTo(Table%DELOBJ)))
        Table%Indx = Table%Indx + 1_kIndex
    END DO

    ! update KeyLelf
    Table%KeyLeft = Table%KeyLeft - 1_kIndex

    ! get key if requested
    IF (PRESENT(KeyVal)) THEN
        IF (SAME_TYPE_AS(KeyVal, Table%NULOBJ)) THEN
            KeyVal = Table%Buckets(Table%Indx)
        ELSE
            CALL Handle_ErrLevel('HashTable_Move2FirstPair', ModName, ErrWarning, &
                    'Type of the specified key-value must be the same as that of stored objects.')
        END IF
    END IF

    RETURN

END FUNCTION HashTable_Move2FirstPair

!******************************************************************************

FUNCTION HashTable_Move2NextPair(Table, KeyVal) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next pair data in a symbol table.  For the *HashTable*, which
    !  is an unordered symbol table,  the next pair is a pair inserted in the first
    !  non-empty bucket after the previous one.  <br>
    !  The routine will report an error if an alteration to stored bucket(s) (either
    !  by an insertion or a removal) has been occurred during current iteration.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable),        INTENT(INOUT)  :: Table    !! HashTable object
    !% the next key and its associated value as output if requested (and available)
    CLASS(Object), OPTIONAL, INTENT(OUT)    :: KeyVal
    !> a flag indicating whether the move to the end of the table occurs or not <br>
    ! - true if next pair data is NOT available. <br>
    ! - otherwise next pair data is available.
    tLogical                                :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Table%IterModCount /= Table%ModCount) THEN
        CALL Handle_ErrLevel('HashTable_Move2NextPair', ModName, ErrWarning, &
                 "Must re-start the iteration because the stored buckets have been altered.")
        RETURN
    END IF

    ! check for empty table
    IF (Table%IsEmpty()) THEN
        IsTheEnd = TrueVal
        RETURN
    ELSEIF (Table%KeyLeft == 0_kIndex) THEN
        IsTheEnd = TrueVal
        RETURN
    END IF

    ! update Indx and set flag
    Table%Indx = Table%Indx + 1
    IsTheEnd = FalseVal

    ! start iteration by looking for the next non-empty slot
    DO WHILE ((Table%Buckets(Table%Indx)%IsEqualTo(Table%NULOBJ)).OR. &
              (Table%Buckets(Table%Indx)%IsEqualTo(Table%DELOBJ)))
        Table%Indx = Table%Indx + 1_kIndex
    END DO

    ! update KeyLelf
    Table%KeyLeft = Table%KeyLeft - 1_kIndex

    ! get key if requested
    IF (PRESENT(KeyVal)) THEN
        IF (SAME_TYPE_AS(KeyVal, Table%NULOBJ)) THEN
            KeyVal = Table%Buckets(Table%Indx)
        ELSE
            CALL Handle_ErrLevel('HashTable_Move2NextPair', ModName, ErrWarning, &
                    'Type of the specified key-value must be the same as that of stored objects.')
        END IF
    END IF

    RETURN

END FUNCTION HashTable_Move2NextPair

!******************************************************************************

SUBROUTINE HashTable_GetAllKeys(Table, KeyValQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return all keys in the table (and all associated values).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable),  INTENT(INOUT)    :: Table    !! HashTable object
    TYPE(QueueKeyVal), INTENT(OUT)      :: KeyValQ  !! key-value queue

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    DO I = 0_kIndex, Table%Capacity-1_kIndex
        IF ((.NOT.Table%Buckets(I)%IsEqualTo(Table%DELOBJ)).AND. &
            (.NOT.Table%Buckets(I)%IsEqualTo(Table%NULOBJ))) THEN
            CALL KeyValQ%EnQueue(Table%Buckets(I))
        END IF
    END DO

    RETURN

END SUBROUTINE HashTable_GetAllKeys

! -----------------------------------------------------------------------------
! -----             Procedures Dependent On Probing Algorithm             -----
! -----------------------------------------------------------------------------

SUBROUTINE SetupProbing(Table, Key)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set up the probing according to the probing algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object
    CLASS(Object),    INTENT(IN)    :: Key      !! key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Table%ProbAlgo)
    CASE (LinearProbing)
        ! no setup required
    CASE (QuadraticProbing)
        ! no setup required
    CASE (DoubleHashing)
        BLOCK
            tIndex  :: HashCode
            ! Cache second hash value.
            HashCode = Key%HashCode()
            Table%HashIndx = ComputeIndex(HashCode, Table%Capacity)
            ! Fail safe to avoid infinite loop
            IF (Table%HashIndx == 0_kIndex) Table%HashIndx = 1_kIndex
        END BLOCK
    END SELECT

    RETURN

END SUBROUTINE SetupProbing

!******************************************************************************

FUNCTION Probe(Table, IdIn) RESULT(IdOut)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To look for the next available bucket according to the probing algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object
    tIndex,           INTENT(IN)    :: IdIn     !! starting index for the probing
    tIndex                          :: IdOut    !! index of available bucket

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Table%ProbAlgo)
    CASE (LinearProbing)
        IdOut = LinearConstant*IdIn
    CASE (QuadraticProbing)
        ! Quadratic probing function (x**2 + x) / 2
        IdOut = SHIFTR(IdIn*IdIn + IdIn, 1)
    CASE (DoubleHashing)
        IdOut = Table%HashIndx*IdIn
    END SELECT

    RETURN

END FUNCTION Probe

!******************************************************************************

SUBROUTINE AdjustCapacity(Table)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To adjust capacity according to the probing algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Table%ProbAlgo)
    CASE (LinearProbing)
        ! Adjust the capacity so that the linear constant and
        ! the table capacity are relatively prime.
        DO WHILE (ComputeGCD(LinearConstant, Table%Capacity) /= 1_kIndex)
            Table%Capacity = Table%Capacity + 1_kIndex
        END DO
    CASE (QuadraticProbing)
        ! Adjust the capacity of the hash table to be a power of two.
        BLOCK
            tIndex      :: Pow2
            Pow2 = HighestOneBit(Table%Capacity)
            IF (Table%Capacity /= Pow2 ) THEN
                CALL IncreaseCapacity(Table)
            END IF
        END BLOCK
    CASE (DoubleHashing)
        ! Adjust the capacity until it is a prime number. The reason for
        ! doing this is to help ensure that the GCD(hash, capacity) = 1 when
        ! probing so that all the cells can be reached.
        IF (.NOT.IsPrime(Table%Capacity)) THEN
            Table%Capacity = NextPrime(Table%Capacity)
        END IF
    END SELECT

    RETURN

END SUBROUTINE AdjustCapacity

!******************************************************************************

SUBROUTINE IncreaseCapacity(Table)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To increase capacity according to the probing algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable), INTENT(INOUT) :: Table    !! HashTable object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (Table%ProbAlgo)
    CASE (LinearProbing)
        ! doubling the capacity
        Table%Capacity = Table%Capacity*2_kIndex + 1_kIndex
    CASE (QuadraticProbing)
        ! increase the capacity of the hash table to the next power of two.
        Table%Capacity = SHIFTL(HighestOneBit(Table%Capacity), 1)
    CASE (DoubleHashing)
        ! doubling the capacity
        Table%Capacity = Table%Capacity*2_kIndex + 1_kIndex
    END SELECT

    RETURN

END SUBROUTINE IncreaseCapacity

! -----------------------------------------------------------------------------
! -----                     Auxiliary Procedures                          -----
! -----------------------------------------------------------------------------

FUNCTION ComputeIndex(HashCode, Capacity) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the index of working buckets of the hash table for
    !  the specified hash code.  Returns value between 0 and
    !  Capacity-1 (assumes Capacity is a power of 2).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tHash,  INTENT(IN)  :: HashCode
    tIndex, INTENT(IN)  :: Capacity
    tIndex              :: Indx

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! remove sign and set the index in the applicable range
    Indx = IAND(IAND(HashCode, MaxHash), Capacity-1_kIndex)

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
#undef HashTable
#undef QueueKeyVal

END MODULE MClass_HTabObject

!******************************************************************************
