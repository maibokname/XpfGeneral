
MODULE MClass_HashTree

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *HashTree* type and its supporting routines and data type.
!   The *HashTree* type is a collection type that employs a separate-chaining hash table
!   implementation to provide common operations for an unordered symbol table. <br>
!   The *HashTree* type uses the *KeyOrdered* type to store its keys and the *GenData*
!   type to store its values.  Therefore, it can be used to store comparable keys and
!   values of any data types.  Allowed types of comparable keys include the *CHARACTER*,
!   *INTEGER* and *REAL* intrinsic types as well as any derived type that is in the
!   *Comparable* class.  Like other collection types, however, it must be employed to store
!   key-value pairs of only specific key type and one specific value type.  To store pairs
!   of another key type (and/or another value type), it must be destructed before inserting
!   items of different key type (or different value type). <br>
!   As a symbol table, the *HashTree* type does not allow duplicated keys.  Therefore, if
!   an inserted key is equal to a key stored in the table, an associated value of the stored
!   key is replaced by an associated value of the inserted key.  As an *unordered* symbol
!   table, the *HashTree* type makes no guarantees as to the iteration order of the table.
!   In particular, it does not guarantee that the order will remain the same over time. <br>
!   Technically, the *HashTree* type utilizes the *IntrusiveHashTree* type as its component
!   to store its hash-tree nodes.  As an *intrusive* container, the *IntrusiveHashTree* type,
!   which is based on a separate-chaining hash table implementation, provides common hash-table
!   operations without a memory management task.  The memory management task of the inserted
!   hash-tree nodes is handled by the *HashTree* type.  <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_MemHandlers
    USE MClass_CharBuffer
    USE MBase_ByteUtil,             ONLY: AnyType_GetByteSize
    USE MBase_SIntUtil,             ONLY: ToChar => ToDecStrSigned
#ifdef Indx32Bits
    USE MBase_SimpleHash32,         ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64,         ONLY: ComputeHash => Hash64_FNV1a
#endif
    USE MClass_IntrusiveBSTrees,    ONLY: BSTNode
    USE MClass_IntrusiveHashTree
    USE MClass_Object,              ONLY: Object
    USE MClass_Comparable,          ONLY: Comparable
    USE MClass_CompNodePool
    USE MClass_MemoryPool
    USE MClass_GenData
    USE MClass_KeyOrdered
    USE MClass_BaseCollection
    USE MClass_BaseSymTable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: HashTree

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_HashTree'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! default initial capacity
    tIndex,    PARAMETER    :: DfltInitCap = 64_kIndex

!** DERIVED TYPE DEFINITIONS
    !> *TabNode* is a binary-search-tree node type containing key and value as its components.
    !   The *KeyOrdered* type is used as a storage for the key and the *GenData* type is used
    !   as a storage for the value.  The *TabNode* type is a subtype of the *HashTreeNode* type
    !   and is intended to be used with the *HashTree* type, which is a collection type that
    !   utilizes the *IntrusiveHashTree* type. <br>
    TYPE, EXTENDS(HashTreeNode) :: TabNode
        TYPE(KeyOrdered)    :: Key          !! stored key
        TYPE(GenData)       :: Value        !! stored value
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----     Deferred/Overridden Procedures from Object Type       -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => TabNode_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => TabNode_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => TabNode_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => TabNode_ToString
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => TabNode_HashCode
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedure from Comparable Type           -----
        ! ---------------------------------------------------------------------
        !> Use a common logical expression to compare two *Comparable* objects.
        PROCEDURE   :: CompareTo    => TabNode_CompareTo
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedure for TabNode Type               -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: SetKeyNVal <br>
        !  **Purpose**:  To set new key and value. <br>
        !  **Usage**: <br>
        !   --->    Valid = Node%SetKeyNVal(Key, Value, MemPool)
        PROCEDURE   :: SetKeyNVal   => TabNode_SetKeyNVal
        !> **Type-Bound Function**: IsEqualKey <br>
        !  **Purpose**:  To check whether the specified key is equal to the node's key. <br>
        !  **Usage**: <br>
        !   --->    Flag = Node%IsEqualKey(Key)
        PROCEDURE   :: IsEqualKey   => TabNode_IsEqualKey
        ! ---------------------------------------------------------------------
    END TYPE TabNode
    !> The *HashTree* type is a collection type that employs a separate-chaining hash table
    !  implementation to provide common operations for an unordered symbol table.  The *HashTree*
    !  type utilizes the *IntrusiveHashTree* type as its component to store *TabNode* objects.
    !  As an intrusive container, the *IntrusiveHashTree* type provides common operations for
    !  hash table without a memory management task.  The memory management task of the inserted
    !  *TabNode* objects is handled by the *HashTree* type. <br>
    !  As an unordered symbol table, the *HashTree* type directly extends the *BaseSymTable*
    !  type and implements all deferred procedures required by the *BaseSymTable* type and all
    !  its super classes.  As a symbol table, the *HashTree* type does not allow duplicated keys;
    !  therefore, if an inserted key is equal to a key stored in the table, an associated value
    !  of the stored key is replaced by an associated value of the inserted key. <br>
    TYPE, EXTENDS(BaseSymTable) :: HashTree
        PRIVATE
        ! a working hash table
        TYPE(IntrusiveHashTree)     :: WrkTab
        !> memory pool of hash-tree nodes
        TYPE(CompNodePool)          :: NodePool
        !> memory pool of stored items
        TYPE(MemoryPool)            :: ItemPool
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: FindKey <br>
        !  **Purpose**:  To find the specified key in the collection.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%FindKey(Key, KeyNode) <br>
        !   --->    IF (.NOT.Collection%FindKey(Key)) DoSomething
        PROCEDURE, PRIVATE  :: FindKey      => HashTree_FindKey
        !> To retrieve all stored keys
        PROCEDURE, PRIVATE  :: GetAllKeys   => HashTree_GetAllKeys
        !> To retrieve all stored values
        PROCEDURE, PRIVATE  :: GetAllVals   => HashTree_GetAllVals
        !> To retrieve all stored keys and values
        PROCEDURE, PRIVATE  :: GetAllPairs  => HashTree_GetAllPairs
        !> Use the *Construct* method to construct the collection from an array of key-value pairs.
        PROCEDURE, PRIVATE  :: HashTree_CreateByArray
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseCollection Type      -----
        ! ---------------------------------------------------------------------
        !> Use the *Construct* method to construct the collection from another collection.
        PROCEDURE   :: CopyCollection   => HashTree_CopyCollection
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        PROCEDURE   :: Destruct         => HashTree_Destroy
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize          => HashTree_GetSize
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
        PROCEDURE   :: StartFirst   => HashTree_Move2FirstPair
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next key-value pair and return a flag
        !                indicating whether the cursor pointer has reached the end of
        !                the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey) <br>
        !   --->    IsTheEnd = Collection%MoveForward(Value=NextVal) <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey, NextVal)
        PROCEDURE   :: MoveForward  => HashTree_Move2NextPair
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key-value pair to the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Insert(Key, Value) <br>
        PROCEDURE   :: Insert       => HashTree_Insert
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete the current key-value pair from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the
        !  *StartFirst* and *MoveForward* methods.  Therefore, after the call to
        !  one of those methods and then calling this one will result in a removal
        !  of the current pair data of the iteration (i.e. the same key-value pair
        !  that can be retrieved via the *StartFirst* and *MoveForward* methods).
        PROCEDURE   :: Delete       => HashTree_Delete
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified key (and its associated value) from
        !                the collection.  Also, return a flag indicating whether the
        !                key-value pair is successfully removed or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Remove(Key) <br>
        !   --->    IF (.NOT.Collection%Remove(Key)) DoSomething
        PROCEDURE   :: Remove       => HashTree_Remove
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified key in the collection.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Contain(Key) <br>
        !   --->    IF (.NOT.Collection%Contain(Key)) DoSomething
        PROCEDURE   :: Contain      => HashTree_Contain
        !> **Type-Bound Function**: GetValue <br>
        !  **Purpose**:  To get a value associated with the specified key in the collection.
        !                Also, return a flag indicating whether the key-value pair is
        !                successfully found or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetValue(Key, Value) <br>
        !   --->    IF (.NOT.Collection%GetValue(Key, Value)) DoSomething
        PROCEDURE   :: GetValue     => HashTree_GetValue
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all key-value pairs from the collection.  Also, return
        !       a flag indicating whether the pairs are successfully retrieved and removed or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Keys, Values) <br>
        !   --->    IF (.NOT.Collection%ToArray(Keys, Values)) DoSomething
        PROCEDURE   :: ToArray      => HashTree_ToArray
        !> **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all keys and/or all values (without removing them) from the collection.
        !       Also, return a flag indicating whether the keys and/or the values are successfully
        !       retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Keys, Values) <br>
        !   --->    IF (.NOT.Collection%GetAll(Keys, Values)) DoSomething
        PROCEDURE   :: GetAll       => HashTree_GetAll
        ! ---------------------------------------------------------------------
        ! -----                      Specific Procedures                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%CreateEmpty()            ! use default options  <br>
        !   --->    CALL Table%CreateEmpty(InitCap=25)  ! specify initial capacity <br>
        PROCEDURE   :: CreateEmpty  => HashTree_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new collection from an array of items or from another
        !                collection. <br>
        !  **Usage**: <br>
        !   ! create a collection from an array of 25 items <br>
        !   --->    CALL Collection%Construct(25, Arr) <br>
        !   ! create a collection from another collection <br>
        !   --->    CALL Collection%Construct(OtherCollection) <br>
        GENERIC     :: Construct    => HashTree_CreateByArray
        ! ---------------------------------------------------------------------
        ! -----     Deferred/Overridden Procedures from Object Type       -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => HashTree_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => HashTree_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => HashTree_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => HashTree_ToString
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => HashTree_HashCode
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: HashTree_Finalize
        ! ---------------------------------------------------------------------
    END TYPE HashTree

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! ---------------------------------------------------------------------
! -----                 TabNode Procedures                        -----
! ---------------------------------------------------------------------

SUBROUTINE TabNode_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy the TabNode object.
    !  This is a deferred procedure by an *Object* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode),     INTENT(IN)  :: SrcObj   !! source object
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

    ! copy Key and Value components
    SELECT TYPE (DstObj)
    TYPE IS (TabNode)
        CALL SrcObj%CopyHashNode(DstObj)
        CALL SrcObj%Key%Copy(DstObj%Key, IsDeep)
        CALL SrcObj%Value%Copy(DstObj%Value, IsDeep)
    CLASS DEFAULT
        CALL Handle_ErrLevel('TabNode_Copy', ModName, ErrSevere, &
                             'Type of the destination object must be "TabNode" only.')
        RETURN
    END SELECT

    RETURN

END SUBROUTINE TabNode_Copy

!******************************************************************************

FUNCTION TabNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure by an *Object* object. <br>
    !  It should be noted that this routine uses all components of
    !  the *TabNode* object to check equality. Therefore, although
    !  (A%CompareTo(B) == 0) returns true, (A%IsEqualTo(B)) can return
    !  false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode), INTENT(IN)  :: LhsObj   !! an object
    CLASS(Object),  INTENT(IN)  :: RhsObj   !! another object
    tLogical                    :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check key and value equalities
    SELECT TYPE (RhsObj)
    TYPE IS (TabNode)
        Flag = FalseVal
        IF (.NOT.LhsObj%Key%IsEqualTo(RhsObj%Key)) RETURN
        IF (.NOT.LhsObj%Value%IsEqualTo(RhsObj%Value)) RETURN
        Flag = LhsObj%IsHashNodeEqual(RhsObj)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION TabNode_IsEqualTo

!******************************************************************************

SUBROUTINE TabNode_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the TabNode object.
    !  This is a deferred procedure by an *Object* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode), INTENT(INOUT)   :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! free the key and value components
    CALL Obj%Key%MemFree()
    CALL Obj%Value%MemFree()

    ! reset the node's components
    CALL Obj%ResetHashNode()

    RETURN

END SUBROUTINE TabNode_MemFree

!******************************************************************************

FUNCTION TabNode_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.
    !  This is a deferred procedure by an *Object* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode), INTENT(IN)  :: Obj
    tCharAlloc                  :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Str = '{TabNode: {' // Obj%Key%ToString() // ' : ' // Obj%Value%ToString() // '}}'

    RETURN

END FUNCTION TabNode_ToString

!******************************************************************************

FUNCTION TabNode_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode), INTENT(IN)  :: Obj
    tIndex                      :: Code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER     :: AdjNode

! FLOW

    ! get code from left node
    AdjNode => Obj%GetLeft()
    SELECT TYPE (Left => AdjNode)
    TYPE IS (TabNode)
        Code = Left%Key%HashCode()
    END SELECT

    ! add code from this node
    Code = Code + Obj%Key%HashCode()

    ! add code from right node
    AdjNode => Obj%GetRight()
    SELECT TYPE (Right => AdjNode)
    TYPE IS (TabNode)
        Code = Code + Right%Key%HashCode()
    END SELECT

    ! free pointer    
    NULLIFY(AdjNode)

    RETURN

END FUNCTION TabNode_HashCode

!******************************************************************************

FUNCTION TabNode_CompareTo(A, B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare *A* and *B* and return <br>
    !   1 if *A* is greater than *B*, <br>
    !   0 if *A* is equal to *B*, <br>
    !  -1 if *A* is less than *B*, <br>
    !  -999 if types of input keys are not the same or either key is invalid. <br>
    ! Also, write an error message to the default log file if this happens.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode),    INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tSInt32                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (B)
    TYPE IS (TabNode)
        Flag = A%Key%CompareTo(B%Key)
    CLASS DEFAULT
        Flag = -999
        CALL Handle_ErrLevel('TabNode_CompareTo', ModName, ErrSevere, 'Type of B is valid.')
    END SELECT

    RETURN

END FUNCTION TabNode_CompareTo

!******************************************************************************

SUBROUTINE TabNode_SetKeyNVal(Node, Key, Value, Pool)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the key and value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode),   INTENT(INOUT) :: Node     !! TabNode object
    CLASS(*),         INTENT(IN)    :: Key      !! the key
    CLASS(*),         INTENT(IN)    :: Value    !! the associated value
    TYPE(MemoryPool), INTENT(INOUT) :: Pool     !! memory pool for both key and value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Node%Key%Set(Key, Pool)
    CALL Node%Value%Set(Value, Pool)

    RETURN

END SUBROUTINE TabNode_SetKeyNVal

!******************************************************************************

FUNCTION TabNode_IsEqualKey(Node, Key, Pool) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is equal to the node's key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TabNode object
    CLASS(TabNode),   INTENT(IN)    :: Node
    !% the key to be retrieved
    CLASS(*),         INTENT(IN)    :: Key
    !% memory pool
    TYPE(MemoryPool), INTENT(INOUT) :: Pool
    !% true if the keys are equal; otherwise, false.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(GenData)   :: KeyInp

!** FLOW:

    CALL KeyInp%Set(Key, Pool)
    Flag = Node%Key%IsEqualTo(KeyInp)
    CALL KeyInp%MemFree()

    RETURN

END FUNCTION TabNode_IsEqualKey

! ---------------------------------------------------------------------
! -----         Deferred Procedures from Object Type          -----
! ---------------------------------------------------------------------

SUBROUTINE HashTree_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.
    !  This is a deferred procedure inherited from the *Object* type.
    !  *Note*:  SrcObj must be in the *HashTree* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTree),    INTENT(IN)  :: SrcObj   !! source object
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
    TYPE IS (HashTree)
        CALL SrcObj%NodePool%Copy(DstObj%NodePool, IsDeep)
        CALL SrcObj%ItemPool%CloneTo(DstObj%ItemPool)
        CALL SrcObj%WrkTab%CloneTo(DstObj%WrkTab)
    CLASS DEFAULT
        CALL Handle_ErrLevel('HashTree_Copy', ModName, ErrSevere, &
                             'Type of the DstObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE HashTree_Copy

!******************************************************************************

FUNCTION HashTree_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTree), INTENT(IN) :: LhsObj   !! an object
    CLASS(Object),   INTENT(IN) :: RhsObj   !! another object
    tLogical                    :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (HashTree)
        Flag = FalseVal
        IF (LhsObj%GetSize() /= RhsObj%GetSize()) RETURN
        IF (.NOT.LhsObj%IsEmpty()) THEN
            ! implementation note:  we cannot use the iteration methods here
            !   due to the intent of the input data.
            BLOCK
                tIndex                      :: I
                CLASS(CompNode), POINTER    :: LhsNodes(:), RhsNodes(:)
                ! initialize
                CALL LhsObj%NodePool%GetAllNodes(LhsNodes)
                CALL RhsObj%NodePool%GetAllNodes(RhsNodes)
                IF (SIZE(LhsNodes, KIND=kIndex) == SIZE(RhsNodes, KIND=kIndex)) THEN
                    SELECT TYPE (LhsNodes)
                    TYPE IS (TabNode)
                        SELECT TYPE (RhsNodes)
                        TYPE IS (TabNode)
                            DO I = 1_kIndex, SIZE(LhsNodes, KIND=kIndex)
                                Flag = LhsNodes(I)%IsEqualTo(RhsNodes(I))
                                IF (.NOT.Flag) EXIT
                            END DO
                        END SELECT
                    END SELECT
                END IF
                NULLIFY(LhsNodes, RhsNodes)
            END BLOCK
        END IF
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION HashTree_IsEqualTo

!******************************************************************************

SUBROUTINE HashTree_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the HashTree object.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTree), INTENT(INOUT)  :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%WrkTab%Destruct()
    CALL Obj%NodePool%MemFree()
    CALL Obj%ItemPool%Destruct()
    CALL Obj%FreeMolds()

    RETURN

END SUBROUTINE HashTree_MemFree

!******************************************************************************

FUNCTION HashTree_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the HashTree type.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTree), INTENT(IN) :: Obj
    tCharAlloc                  :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: BaseStr

! FLOW

    ! get base string
    IF (Obj%IsEmpty()) THEN
        BaseStr = '[NULL]'
    ELSE
        BLOCK
            TYPE(CharBuffer)            :: ChrBuf
            tIndex                      :: I, Count
            tCharAlloc                  :: KeyStr, ValStr, ItemStr
            CLASS(CompNode), POINTER    :: PoolNodes(:)
            ! initialize
            Count = 0_kIndex
            CALL Obj%NodePool%GetAllNodes(PoolNodes)
            CALL ChrBuf%CreateEmpty(InitCap=Obj%GetSize()*60_kIndex)
            CALL ChrBuf%Append('[')
            SELECT TYPE (Nodes => PoolNodes)
            TYPE IS (TabNode)
                DO I = 1_kIndex, SIZE(Nodes, KIND=kIndex)
                    IF (Nodes(I)%Key%IsEmpty()) CYCLE
                    IF (Nodes(I)%Value%IsEmpty()) CYCLE
                    KeyStr = Nodes(I)%Key%ToString()
                    ValStr = Nodes(I)%Value%ToString()
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
            END SELECT
            NULLIFY(PoolNodes)
            CALL ChrBuf%Append(']')
            BaseStr = ChrBuf%AsString()
        END BLOCK
    END IF
    Str = '{HashTree with ' // ToChar(Obj%GetSize()) // ' items : ' // BaseStr // '}'

    RETURN

END FUNCTION HashTree_ToString

!******************************************************************************

FUNCTION HashTree_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTree), INTENT(IN) :: Obj
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
            CLASS(CompNode), POINTER    :: PoolNodes(:)
            tIndex                      :: I
            ! initialize
            CALL Obj%NodePool%GetAllNodes(PoolNodes)
            Code = 0_kIndex
            ! compute hash code
            SELECT TYPE (Nodes => PoolNodes)
            TYPE IS (TabNode)
                DO I = 1_kIndex, SIZE(Nodes, KIND=kIndex)
                    IF (Nodes(I)%Key%IsEmpty()) CYCLE
                    Code = Code + Nodes(I)%Key%HashCode()
                END DO
            END SELECT
        END BLOCK
    END IF

    RETURN

END FUNCTION HashTree_HashCode

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseCollection Type       -----
! ---------------------------------------------------------------------

SUBROUTINE HashTree_CopyCollection(This, Other, ItemCopy, ValCopy)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items as the given collection (Other). <br>
    !  This is a deferred procedure by the *BaseCollection* class. <br>
    !  *Note*:  Other must be in the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(HashTree),       INTENT(INOUT)    :: This
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
    TYPE IS (HashTree)
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
        CALL Handle_ErrLevel('HashTree_CopyCollection', ModName, ErrSevere, &
                             'Type of "Other" must be in the "BaseSymTable" class.')
    END SELECT

    RETURN

END SUBROUTINE HashTree_CopyCollection

!******************************************************************************

SUBROUTINE HashTree_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the items from the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTree object
    CLASS(HashTree), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! simply return if the collection is empty
    IF (Collection%IsEmpty()) RETURN

    ! clear elements
    CALL Collection%WrkTab%Clear()

    ! reset components
    CALL Collection%NodePool%Reset()

    ! free mold
    CALL Collection%FreeMolds()

    RETURN

END SUBROUTINE HashTree_ClearItems

!******************************************************************************

SUBROUTINE HashTree_Destroy(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTree object
    CLASS(HashTree), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    CALL Collection%MemFree()

    RETURN

END SUBROUTINE HashTree_Destroy

!******************************************************************************

FUNCTION HashTree_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of items in the collection.
    !  This is a deferred procedure inherited from the *BaseCollection* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTree), INTENT(IN) :: Collection
    tIndex                      :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%WrkTab%GetSize()

    RETURN

END FUNCTION HashTree_GetSize

! ---------------------------------------------------------------------
! -----         Deferred Procedures from BaseSymTable Type        -----
! ---------------------------------------------------------------------

FUNCTION HashTree_Move2FirstPair(Collection, Key, Value, KeyCopy, ValCopy) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the first (starting) pair data in a symbol table.
    !  For the *HashTree*, which is an unordered symbol table,
    !  the starting pair is the first pair inserted.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTree),    INTENT(INOUT)   :: Collection
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
    !> a flag indicating whether the collection contains no pair data or not <br>
    ! - true if the collection is empty. <br>
    ! - otherwise the first pair data is available.
    tLogical                            :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (PRESENT(Key).OR.PRESENT(Value)) THEN
        BLOCK
            ! block variable
            CLASS(HashTreeNode), POINTER   :: CurrNode
            ! start iteration
            IsEmpty = Collection%WrkTab%StartFirst(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode)
                    IF (PRESENT(Key)) THEN
                        IF (.NOT.CurrNode%Key%Get(Key)) THEN
                            CALL Handle_ErrLevel('HashTree_Move2FirstPair', ModName, ErrWarning, &
                                'Type of the specified key is invalid or is NOT the same as that of stored keys.')
                        END IF
                    END IF
                    IF (PRESENT(Value)) THEN
                        IF (.NOT.CurrNode%Value%Get(Value, ValCopy)) THEN
                            CALL Handle_ErrLevel('HashTree_Move2FirstPair', ModName, ErrWarning, &
                                'Type of the specified value is NOT the same as that of stored values.')
                        END IF
                    END IF
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration
        IsEmpty = Collection%WrkTab%StartFirst()
    END IF

    RETURN

END FUNCTION HashTree_Move2FirstPair

!******************************************************************************

FUNCTION HashTree_Move2NextPair(Collection, Key, Value, KeyCopy, ValCopy) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next pair data in a symbol table.
    !  For the *HashTree*, which is an unordered symbol table,
    !  the next pair is the pair inserted after the previous one.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTree),    INTENT(INOUT)   :: Collection
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
    !> a flag indicating whether the move to the end of the
    !  collection occurs or not <br>
    ! - true if next pair data is NOT available. <br>
    ! - otherwise next pair data is available.
    tLogical                            :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (PRESENT(Key).OR.PRESENT(Value)) THEN
        BLOCK
            ! block variable
            CLASS(HashTreeNode), POINTER   :: CurrNode
            ! move to next iteration
            IsTheEnd = Collection%WrkTab%MoveForward(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode)
                    IF (PRESENT(Key)) THEN
                        IF (.NOT.CurrNode%Key%Get(Key)) THEN
                            CALL Handle_ErrLevel('HashTree_Move2NextPair', ModName, ErrWarning, &
                                'Type of the specified key is invalid or is NOT the same as that of stored keys.')
                        END IF
                    END IF
                    IF (PRESENT(Value)) THEN
                        IF (.NOT.CurrNode%Value%Get(Value, ValCopy)) THEN
                            CALL Handle_ErrLevel('HashTree_Move2NextPair', ModName, ErrWarning, &
                                'Type of the specified value is NOT the same as that of stored values.')
                        END IF
                    END IF
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! move to next iteration
        IsTheEnd = Collection%WrkTab%MoveForward()
    END IF

    RETURN

END FUNCTION HashTree_Move2NextPair

!******************************************************************************

SUBROUTINE HashTree_Insert(Collection, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a key-value pair into a symbol table.  If the specified key is already
    !  stored in the table, replace the old value with the new one.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTree), INTENT(INOUT)  :: Collection
    !% the key to be added to the collection
    CLASS(*),        INTENT(IN)     :: Key
    !% the associated value to be added to the collection
    CLASS(*),        INTENT(IN)     :: Value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical                    :: KeyFound
    TYPE(TabNode),   POINTER    :: KeyNode
    CLASS(CompNode), POINTER    :: NewNode

! FLOW

    ! check the specified key and value
    IF (.NOT.Collection%IsKeyValid(Key, IsOrderedKey=TrueVal)) THEN
        CALL Handle_ErrLevel('HashTree_Insert', ModName, ErrSevere, &
                'Type of the specified key is invalid or is NOT the same as that of stored keys.')
        RETURN
    ELSEIF (.NOT.Collection%IsValValid(Value)) THEN
        CALL Handle_ErrLevel('HashTree_Insert', ModName, ErrSevere, &
                'Only values of the same type are allowed in a collection.')
        RETURN
    END IF

    ! check whether the key is already stored in the collection or not
    IF (Collection%IsEmpty()) THEN
        KeyFound = FalseVal
    ELSE
        KeyFound = Collection%FindKey(Key, KeyNode)
    END IF

    IF (KeyFound) THEN
        ! replace the current value with the new one
        CALL KeyNode%Value%Set(Value, Collection%ItemPool)
    ELSE
        ! +++ new key-value pair +++
        ! check for first-time insertion
        IF (.NOT.Collection%NodePool%IsReady()) THEN
            CALL Collection%CreateEmpty(DfltInitCap)
        END IF
        ! get new node from the node's pool
        CALL Collection%NodePool%GetNewNode(NewNode)
        SELECT TYPE (NewNode)
        TYPE IS (TabNode)
            KeyNode => NewNode
        END SELECT
        ! set key and value to the node
        ! (no need to check the valid flag since we have already done that in the beginning)
        CALL KeyNode%SetKeyNVal(Key, Value, Collection%ItemPool)
        ! append the new node to the working hash table
        CALL Collection%WrkTab%Insert(KeyNode)
    END IF

    ! free working pointer
    NULLIFY(KeyNode, NewNode)

    RETURN

END SUBROUTINE HashTree_Insert

!******************************************************************************

SUBROUTINE HashTree_Delete(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete a key-value pair from a symbol table.  This procedure is intended
    !  to be used in conjunction with the *StartFirst* and *MoveForward* procedures.
    !  Therefore, after the call to either procedure and then calling this procedure
    !  will result in a removal of the current key-value pair of the iteration (i.e.
    !  the same key-value pair that can be retrieved via those *Move* procedures). <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTree), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(HashTreeNode), POINTER    :: DelNode

! FLOW

    ! remove node of the current iteration
    CALL Collection%WrkTab%Delete(DelNode)

    ! return the node to the nodes' pool
    IF (ASSOCIATED(DelNode)) THEN
        SELECT TYPE (DelNode)
        TYPE IS (TabNode)
            CALL Collection%NodePool%ReturnNode(DelNode)
        END SELECT
    END IF
    
    NULLIFY(DelNode)

    RETURN

END SUBROUTINE HashTree_Delete

!******************************************************************************

FUNCTION HashTree_Remove(Collection, Key) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To delete the specified key (and its associated value) from a symbol
    !  table.  Also, return a flag indicating whether the key-value pair is
    !  successfully removed or not.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTree), INTENT(INOUT)  :: Collection
    !% the key to be removed from the collection
    CLASS(*),        INTENT(IN)     :: Key
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode), POINTER  :: KeyNode

! FLOW

    ! check whether the key is stored in the collection or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
    ELSE
        Flag = Collection%FindKey(Key, KeyNode)
    END IF

    IF (Flag) THEN
        ! check if remove the node successfully or not
        IF (Collection%WrkTab%Remove(KeyNode)) THEN
            ! return the node to the node's pool
            CALL Collection%NodePool%ReturnNode(KeyNode)
        ELSE
            CALL Handle_ErrLevel('HashTree_Remove', ModName, ErrWarning, &
                       'Check the Remove method of the intrusive list for possible bug(s).')
        END IF
    END IF

    ! free working pointer
    NULLIFY(KeyNode)

    RETURN

END FUNCTION HashTree_Remove

!******************************************************************************

FUNCTION HashTree_Contain(Collection, Key) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is currently stored in a symbol table.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTree), INTENT(INOUT)  :: Collection
    !% the key to be looked for in the collection
    CLASS(*),        INTENT(IN)     :: Key
    !% flag indicating whether the specified key is found or not.
    tLogical                        :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Collection%IsEmpty()) THEN
        Found = FalseVal
    ELSE
        Found = Collection%FindKey(Key)
    END IF

    RETURN

END FUNCTION HashTree_Contain

!******************************************************************************

FUNCTION HashTree_GetValue(Collection, Key, Value, ValCopy) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a value associated with the specified key in a symbol table.
    !  Also, return a flag indicating whether the key-value pair is successfully
    !  found or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTree),     INTENT(INOUT)  :: Collection
    !% the key to be looked for in the collection
    CLASS(*),            INTENT(IN)     :: Key
    !% the value associated with the specified key
    CLASS(*),            INTENT(INOUT)  :: Value
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !% flag indicating whether the key-value pair is found or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode), POINTER  :: KeyNode

! FLOW

    ! check whether the key is stored in the collection or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
    ELSE
        Flag = Collection%FindKey(Key, KeyNode)
    END IF

    ! get value if key is found
    IF (Flag) THEN
        Flag = KeyNode%Value%Get(Value, ValCopy)
    END IF

    ! free working pointer
    NULLIFY(KeyNode)

    RETURN

END FUNCTION HashTree_GetValue

!**************************************************************************************

FUNCTION HashTree_ToArray(Collection, Keys, Values, KeyCopy, ValCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove all key-value pairs from the collection.  Also, return a flag
    !  indicating whether the pairs are successfully retrieved and removed or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTree object
    CLASS(HashTree),     INTENT(INOUT)  :: Collection
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

END FUNCTION HashTree_ToArray

!**************************************************************************************

FUNCTION HashTree_GetAll(Collection, Keys, Values, KeyCopy, ValCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all keys and/or all values (without removing them) from the collection.  Also,
    !  return a flag indicating whether the keys and/or values are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTree object
    CLASS(HashTree),     INTENT(INOUT)  :: Collection
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
        SameKeyType = Collection%IsKeyValid(Keys(1), IsOrderedKey=TrueVal)
        SameValType = Collection%IsValValid(Values(1))
        ! check whether types of the specified keys and values are valid or not
        IF (SameKeyType.AND.SameValType) THEN
            Success = Collection%GetAllPairs(Keys, Values, ValCopy)
        ELSEIF (SameKeyType) THEN
            Success = FalseVal
            CALL Handle_ErrLevel('HashTree_GetAll', ModName, ErrSevere, &
                                 'Type of the specified values is NOT the same as that of stored values.')
        ELSEIF (SameValType) THEN
            Success = FalseVal
            CALL Handle_ErrLevel('HashTree_GetAll', ModName, ErrSevere, &
                                 'Type of the specified keys is NOT the same as that of stored keys.')
        ELSE
            Success = FalseVal
            CALL Handle_ErrLevel('HashTree_GetAll', ModName, ErrSevere, &
                                 'Types of both keys and values are NOT the same as those of stored pairs.')
        END IF
    ELSEIF (PRESENT(Keys)) THEN
        ! check whether type of the specified keys is valid or not
        IF (Collection%IsKeyValid(Keys(1), IsOrderedKey=TrueVal)) THEN
            Success = Collection%GetAllKeys(Keys)
        ELSE
            Success = FalseVal
            CALL Handle_ErrLevel('HashTree_GetAll', ModName, ErrSevere, &
                                 'Type of the specified keys is NOT the same as that of stored keys.')
        END IF
    ELSEIF (PRESENT(Values)) THEN
        ! check whether type of the specified values is valid or not
        IF (Collection%IsValValid(Values(1))) THEN
            Success = Collection%GetAllVals(Values, ValCopy)
        ELSE
            Success = FalseVal
            CALL Handle_ErrLevel('HashTree_GetAll', ModName, ErrSevere, &
                                 'Type of the specified values is NOT the same as that of stored values.')
        END IF
    END IF

    RETURN

END FUNCTION HashTree_GetAll

!**************************************************************************************

FUNCTION HashTree_GetAllPairs(Collection, Keys, Values, ValCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all key-value pairs (without removing them) from the collection.  Also,
    !  return a flag indicating whether the keys are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTree object
    CLASS(HashTree),     INTENT(INOUT)  :: Collection
    !% the keys to be retrieved and removed from the collection
    CLASS(*),            INTENT(INOUT)  :: Keys(:)
    !% the values to be retrieved and removed from the collection
    CLASS(*),            INTENT(INOUT)  :: Values(:)
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !> flag indicating whether the keys are successfully retrieved. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                              :: I, KeySize, ValSize
    tLogical                            :: IsTheEnd
    CLASS(HashTreeNode), POINTER    :: CurrNode

! FLOW

    ! initialize local variables
    KeySize = SIZE(Keys, KIND=kIndex)
    ValSize = SIZE(Values, KIND=kIndex)
    IF ((KeySize < 1_kindex).AND.(ValSize < 1_kindex)) RETURN
    I = 1_kindex

    ! loop through the collection and get the keys along the way
    IsTheEnd = Collection%WrkTab%StartFirst(CurrNode)
    DO WHILE ((.NOT.IsTheEnd).AND.ASSOCIATED(CurrNode))
        SELECT TYPE (CurrNode)
        TYPE IS (TabNode)
            IF (I <= KeySize)               Success = CurrNode%Key%Get(Keys(I))
            IF (Success.AND.(I <= ValSize)) Success = CurrNode%Value%Get(Values(I), ValCopy)
            IF (.NOT.Success) EXIT
            I = I + 1_kIndex
        END SELECT
        IsTheEnd = Collection%WrkTab%MoveForward(CurrNode)
    END DO

    RETURN

END FUNCTION HashTree_GetAllPairs

!**************************************************************************************

FUNCTION HashTree_GetAllKeys(Collection, Keys) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all keys (without removing them) from the collection.  Also,
    !  return a flag indicating whether the keys are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTree object
    CLASS(HashTree),     INTENT(INOUT)  :: Collection
    !% the keys to be retrieved and removed from the collection
    CLASS(*),            INTENT(INOUT)  :: Keys(:)
    !> flag indicating whether the keys are successfully retrieved. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                              :: I, ArrSize
    tLogical                            :: IsTheEnd
    CLASS(HashTreeNode), POINTER    :: CurrNode

! FLOW

    ! initialize local variables
    ArrSize = SIZE(Keys, KIND=kIndex)
    IF (ArrSize < 1_kindex) RETURN
    I = 1_kindex

    ! loop through the collection and get the keys along the way
    IsTheEnd = Collection%WrkTab%StartFirst(CurrNode)
    DO WHILE ((.NOT.IsTheEnd).AND.ASSOCIATED(CurrNode))
        SELECT TYPE (CurrNode)
        TYPE IS (TabNode)
            Success = CurrNode%Key%Get(Keys(I))
            IF (.NOT.Success) EXIT
            I = I + 1_kIndex
            IF (I > ArrSize) EXIT
        END SELECT
        IsTheEnd = Collection%WrkTab%MoveForward(CurrNode)
    END DO

    RETURN

END FUNCTION HashTree_GetAllKeys

!**************************************************************************************

FUNCTION HashTree_GetAllVals(Collection, Values, ValCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all values (without removing them) from the collection.  Also,
    !  return a flag indicating whether the values are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTree object
    CLASS(HashTree),     INTENT(INOUT)  :: Collection
    !% the values to be retrieved and removed from the collection
    CLASS(*),            INTENT(INOUT)  :: Values(:)
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !> flag indicating whether the values are successfully retrieved. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                              :: I, ArrSize
    tLogical                            :: IsTheEnd
    CLASS(HashTreeNode), POINTER    :: CurrNode

! FLOW

    ! initialize local variables
    ArrSize = SIZE(Values, KIND=kIndex)
    IF (ArrSize < 1_kindex) RETURN
    I = 1_kindex

    ! loop through the collection and get the values along the way
    IsTheEnd = Collection%WrkTab%StartFirst(CurrNode)
    DO WHILE ((.NOT.IsTheEnd).AND.ASSOCIATED(CurrNode))
        SELECT TYPE (CurrNode)
        TYPE IS (TabNode)
            Success = CurrNode%Value%Get(Values(I), ValCopy)
            IF (.NOT.Success) EXIT
            I = I + 1_kIndex
            IF (I > ArrSize) EXIT
        END SELECT
        IsTheEnd = Collection%WrkTab%MoveForward(CurrNode)
    END DO

    RETURN

END FUNCTION HashTree_GetAllVals

! ---------------------------------------------------------------------
! -----                 Specific Procedures                       -----
! ---------------------------------------------------------------------

SUBROUTINE HashTree_CreateEmpty(Collection, InitCap)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To create an empty hash table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTree),   INTENT(INOUT)    :: Collection   !! collection
    tIndex,  OPTIONAL, INTENT(IN)       :: InitCap      !! initial capacity of the hash table

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode)   :: NodeMold

!** FLOW:

    ! construct working hash table
    CALL Collection%WrkTab%Construct(InitCap)

    ! construct nodes' pool
    CALL Collection%NodePool%Construct(NodeMold)

    ! construct items' pool
    CALL Collection%ItemPool%Construct()

    RETURN

END SUBROUTINE HashTree_CreateEmpty

!******************************************************************************

SUBROUTINE HashTree_CreateByArray(Collection, N, Keys, Values)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a table from an array of key-value pairs.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTree), INTENT(INOUT)  :: Collection   !! collection
    tIndex,          INTENT(IN)     :: N            !! number of key-value pairs
    !% the keys to be added to the table
    CLASS(*),        INTENT(IN)     :: Keys(N)
    !% the associated values to be added to the table
    CLASS(*),        INTENT(IN)     :: Values(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! create empty symbol table with capacity twice of the key size
    CALL Collection%CreateEmpty(N*2_kIndex)

    ! add key-value pairs to the table
    DO I = 1_kIndex, N
        CALL Collection%Insert(Keys(I), Values(I))
    END DO

    RETURN

END SUBROUTINE HashTree_CreateByArray

!******************************************************************************

FUNCTION HashTree_FindKey(Collection, Key, KeyNode) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the specified key in a symbol table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTree),                  INTENT(INOUT) :: Collection
    !% the key to be looked for in the collection
    CLASS(*),                         INTENT(IN)    :: Key
    !% the node containing the specified key; null pointer if the key is not found
    TYPE(TabNode), OPTIONAL, POINTER, INTENT(OUT)   :: KeyNode
    !% flag indicating whether the specified key is found or not.
    tLogical                                        :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode)                       :: InNode
    CLASS(HashTreeNode), POINTER    :: StoredNode

! FLOW

    ! initialize
    Found = Falseval
    IF (PRESENT(KeyNode)) KeyNode => NULL()

    ! check the specified key and return quickly if not valid
    IF (.NOT.Collection%IsKeyValid(Key, IsOrderedKey=TrueVal)) RETURN

    ! set key for search node
    CALL InNode%Key%Set(Key, Collection%ItemPool)

    ! find the stored node equal to input node
    Found = Collection%WrkTab%FindNode(InNode, StoredNode)
    IF (PRESENT(KeyNode).AND.Found) THEN
        SELECT TYPE (StoredNode)
        TYPE IS (TabNode)
            KeyNode => StoredNode
        END SELECT
    END IF

    ! free working pointer
    NULLIFY(StoredNode)

    RETURN

END FUNCTION HashTree_FindKey

! ---------------------------------------------------------------------
! -----             Final Procedures                              -----
! ---------------------------------------------------------------------

SUBROUTINE HashTree_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(HashTree), INTENT(INOUT)   :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE HashTree_Finalize

!******************************************************************************

END MODULE MClass_HashTree

!******************************************************************************
