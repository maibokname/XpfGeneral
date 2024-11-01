
MODULE MClass_TreeTable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *TreeTable* type and its supporting routines and data type.  The
!   The *TreeTable* type is a collection type that employs a balanced binary-search-tree (BST)
!   implementation to provide common operations for an ordered symbol table. <br>
!   The *TreeTable* type uses the *KeyOrdered* type to store its keys and the *GenData* type
!   to store its values.  Therefore, it can be used to store comparable keys and values of any
!   data types.  Allowed types of comparable keys include the *CHARACTER*, *INTEGER* and *REAL*
!   intrinsic types as well as any derived type that is in the *Comparable* class.  Like other
!   collection types, however, it must be employed to store key-value pairs of only specific
!   key type and one specific value type.  To store key-value pairs of another key type (or
!   another value type), it must be destructed before inserting items of different key type
!   (or different value type). <br>
!   As a symbol table, the *TreeTable* type does not allow duplicated keys.  Therefore, if an
!   inserted key is equal to a key stored in the table, an associated value of the stored key
!   is replaced by an associated value of the inserted key.  As an *ordered* symbol table, the
!   *TreeTable* type provides an ordered iteration over its stored key-value items, which are
!   sorted according to the natural ordering of its keys.  It can be accessed and traversed in
!   either ascending (using the *StartFirst* method) or descending (using the *StartLast* method)
!   key order. <br>
!   Technically, the *TreeTable* type employs a balanced binary search tree (BST) implementation
!   to provide common operations for an ordered symbol table.  The *TreeTable* type utilizes the
!   *IntrusiveRBTree* type as its component to store its tree nodes.  The *IntrusiveRBTree* type,
!   as an intrusive BST container, provides common binary-search-tree operations without a memory
!   management task.  The memory management task of the inserted tree nodes is handled by the
!   *TreeTable* type.  <br>

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_MemHandlers
    USE MClass_CharBuffer
    USE MBase_ByteUtil,         ONLY: AnyType_GetByteSize
    USE MBase_SIntUtil,         ONLY: ToChar => ToDecStrSigned
#ifdef Indx32Bits
    USE MBase_SimpleHash32,     ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64,     ONLY: ComputeHash => Hash64_FNV1a
#endif
    USE MClass_IntrusiveBSTrees
    USE MClass_Object,          ONLY: Object
    USE MClass_Comparable,      ONLY: Comparable
    USE MClass_KeyOrdered
    USE MClass_GenData
    USE MClass_CompNodePool
    USE MClass_MemoryPool
    USE MClass_BaseCollection
    USE MClass_BaseSymTable
    USE MClass_OrderedSymTable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: TreeTable

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_TreeTable'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    !> *TabNode* is a binary-search-tree node type containing key and value as its components.
    !   The *KeyOrdered* type is used as a storage for the key and the *GenData* type is used
    !   as a storage for the value.  The *TabNode* type is a subtype of the *BSTNode* type and
    !   is intended to be used with the *TreeTable* type, which is a collection type that utilizes
    !   the *IntrusiveRBTree* type. <br>
    TYPE, EXTENDS(BSTNode)  :: TabNode
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
        PROCEDURE   :: CompareTo    => TabNode_CompareKey
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedure for TabNode Type               -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: SetKeyNVal <br>
        !  **Purpose**:  To set new key and value. <br>
        !  **Usage**: <br>
        !   --->    Valid = Node%SetKeyNVal(Key, Value, MemPool)
        PROCEDURE   :: SetKeyNVal   => TabNode_SetKeyNVal
        ! ---------------------------------------------------------------------
    END TYPE TabNode
    !> The *TreeTable* type is a collection type that utilizes a balanced BST implementation to
    !   provide common operations for an ordered symbol table.  The *TreeTable* type uses the
    !   *IntrusiveRBTree* type as its component to store *TabNode* objects.  As an intrusive
    !   BST container, the *IntrusiveRBTree* type provides common binary-search-tree operations
    !   without a memory management task.  The memory management task of the inserted *TabNode*
    !   objects is handled by the *TreeTable* type.  <br>
    !   As an ordered symbol table, the *TreeTable* type is a subtype of the *OrderedSymTable*
    !   type.  Thus, it implements all deferred procedures required by the *OrderedSymTable*
    !   type and all its super classes.  As a symbol table, the *TreeTable* type does not allow
    !   duplicated keys; therefore, if an inserted key is equal to a key stored in the table, an
    !   associated value of the stored key is replaced by an associated value of the inserted key. <br>
    !   Because the *IntrusiveRBTree* type is a subtype of the *IntrusiveAVLTree* type, the
    !   *WrkTree* component can be employed as a red-black tree or an AVL tree.  Therefore, the
    !   *TreeTable* type allows a user to specify which type of binary-search tree implementation
    !   to be used.  By default, the red-black tree implementation is used.  The user can call the
    !   *UseAVLTree* method to change to AVL tree implementation.  The *UseAVLTree* method must
    !   be called before inserting an object into the symbol table (i.e when the table is empty).
    !   Otherwise, the red-black tree implementation is employed.  <br>
    TYPE, EXTENDS(OrderedSymTable) :: TreeTable
        PRIVATE
        !> a flag indicating whether to use the red-black tree implementation
        !  or the AVL tree implementation
        tLogical                :: IsRBTree = TrueVal
        !% a working binary-search tree
        TYPE(IntrusiveRBTree)   :: WrkTree
        !> direction of the iteration <br>
        !  - positive -> forward iteration
        !  - negative -> backward iteration
        !  - zero     -> iteration not yet start
        tSInt32                 :: Dir = 0
        !> memory pool of tree nodes 
        TYPE(CompNodePool)      :: NodePool
        !> memory pool of stored items
        TYPE(MemoryPool)        :: ItemPool
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: FindKey <br>
        !  **Purpose**:  To check whether the specified key is stored in the
        !                collection or not.  Optionally, return a stored node
        !                containing a key equal to the specified key. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%FindKey(Key, KeyNode) <br>
        !   --->    IF (.NOT.Collection%FindKey(Key)) DoSomething
        PROCEDURE, PRIVATE  :: FindKey      => TreeTable_FindKey
        !> To retrieve all stored keys
        PROCEDURE, PRIVATE  :: GetAllKeys   => TreeTable_GetAllKeys
        !> To retrieve all stored values
        PROCEDURE, PRIVATE  :: GetAllVals   => TreeTable_GetAllVals
        !> Use the *Construct* method to construct the collection from an array of key-value pairs.
        PROCEDURE, PRIVATE  :: TreeTable_CreateByArray
        ! ---------------------------------------------------------------------
        ! -----  Deferred/Overridden Procedures from BaseCollection Type  -----
        ! ---------------------------------------------------------------------
        !> Use the *Construct* method to construct the collection from another collection.
        PROCEDURE   :: CopyCollection   => TreeTable_CopyCollection
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all key-value pairs from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        PROCEDURE   :: Clear            => TreeTable_ClearItems
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        PROCEDURE   :: Destruct         => TreeTable_Destroy
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize          => TreeTable_GetSize
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
        PROCEDURE   :: StartFirst   => TreeTable_Move2FirstPair
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next key-value pair and return a flag
        !                indicating whether the cursor pointer has reached the end of
        !                the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey) <br>
        !   --->    IsTheEnd = Collection%MoveForward(Value=NextVal) <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey, NextVal)
        PROCEDURE   :: MoveForward  => TreeTable_Move2NextPair
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start an iteration in a reversed order and return a flag
        !                indicating whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartLast() <br>
        !   --->    IsEmpty = Collection%StartLast(LastKey) <br>
        !   --->    IsEmpty = Collection%StartLast(Value=LastVal) <br>
        !   --->    IsEmpty = Collection%StartLast(LastKey, LastVal)
        PROCEDURE   :: StartLast    => TreeTable_Move2LastPair
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move to the previous key-value pair and return a flag
        !                indicating whether the cursor pointer has reached the end of
        !                the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveBackward() <br>
        !   --->    IsTheEnd = Collection%MoveBackward(PrevKey) <br>
        !   --->    IsTheEnd = Collection%MoveBackward(Value=PrevVal) <br>
        !   --->    IsTheEnd = Collection%MoveBackward(PrevKey, PrevVal)
        PROCEDURE   :: MoveBackward => TreeTable_Move2PrevPair
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key-value pair to the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Insert(Key, Value) <br>
        PROCEDURE   :: Insert       => TreeTable_Insert
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete the current key-value pair from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the *StartFirst* and
        !       *MoveForward* methods.  Therefore, after the call to one of those methods and then
        !       calling this one will result in a removal of the current item of the iteration
        !       (i.e. the same item that can be retrieved via the *StartFirst* and *MoveForward*
        !       methods). <br>
        PROCEDURE   :: Delete       => TreeTable_Delete
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified key (and its associated value) from
        !                the collection.  Also, return a flag indicating whether the
        !                key-value pair is successfully removed or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Remove(Key) <br>
        !   --->    IF (.NOT.Collection%Remove(Key)) DoSomething
        PROCEDURE   :: Remove       => TreeTable_Remove
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified key in the collection.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Contain(Key) <br>
        !   --->    IF (.NOT.Collection%Contain(Key)) DoSomething
        PROCEDURE   :: Contain      => TreeTable_Contain
        !> **Type-Bound Function**: GetValue <br>
        !  **Purpose**:  To get a value associated with the specified key in the collection. Also,
        !       return a flag indicating whether the key-value pair is successfully found or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetValue(Key, Value) <br>
        !   --->    IF (.NOT.Collection%GetValue(Key, Value)) DoSomething
        PROCEDURE   :: GetValue     => TreeTable_GetValue
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all key-value pairs from the collection.  Also, return
        !       a flag indicating whether the pairs are successfully retrieved and removed or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Keys, Values) <br>
        !   --->    IF (.NOT.Collection%ToArray(Keys, Values)) DoSomething
        PROCEDURE   :: ToArray      => TreeTable_ToArray
        !> **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all keys and/or all values (without removing them) from the collection.
        !       Also, return a flag indicating whether the keys and/or the values are successfully
        !       retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Keys, Values) <br>
        !   --->    IF (.NOT.Collection%GetAll(Keys, Values)) DoSomething
        PROCEDURE   :: GetAll       => TreeTable_GetAll
        ! ---------------------------------------------------------------------
        ! -----     Deferred Procedures from OrderedSymTable Type         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetMinKey <br>
        !  **Purpose**:  To get the smallest key (and optionally a value associated
        !                with it) in a symbol table.  Also, return a flag indicating
        !                whether the key is successfully retrieved or not.  If the
        !                table is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetMinKey(Key) <br>
        !   --->    IF (.NOT.Collection%GetMinKey(Key, Value)) DoSomething
        PROCEDURE   :: GetMinKey    => TreeTable_GetMinKey
        !> **Type-Bound Function**: GetMaxKey <br>
        !  **Purpose**:  To get the largest key (and optionally a value associated
        !                with it) in a symbol table.  Also, return a flag indicating
        !                whether the key is successfully retrieved or not.  If the
        !                table is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetMaxKey(Key) <br>
        !   --->    IF (.NOT.Collection%GetMaxKey(Key, Value)) DoSomething
        PROCEDURE   :: GetMaxKey    => TreeTable_GetMaxKey
        !> **Type-Bound Function**: Floor <br>
        !  **Purpose**:  To get the largest key (and optionally a value associated
        !                with it) in a symbol table less than or equal to the given
        !                key.  Also, return a flag indicating whether the floor key
        !                is successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Floor(KeyIn, KeyOut) <br>
        !   --->    IF (.NOT.Collection%Floor(KeyIn, KeyOut, Value)) DoSomething
        PROCEDURE   :: Floor        => TreeTable_Floor
        !> **Type-Bound Function**: Ceiling <br>
        !  **Purpose**:  To get the smallest key (and optionally a value associated
        !                with it) in a symbol table greater than or equal to the given
        !                key.  Also, return a flag indicating whether the ceiling key
        !                is successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Ceiling(KeyIn, KeyOut) <br>
        !   --->    IF (.NOT.Collection%Ceiling(KeyIn, KeyOut, Value)) DoSomething
        PROCEDURE   :: Ceiling      => TreeTable_Ceiling
        !> **Type-Bound Function**: GetRank <br>
        !  **Purpose**:  To return the number of keys in the symbol table strictly
        !                less than the given key. <br>
        !  **Usage**: <br>
        !   --->    KeyRank = Collection%GetRank(Key)
        PROCEDURE   :: GetRank      => TreeTable_GetRank
        !> **Type-Bound Subroutine**: Select <br>
        !  **Purpose**:  To get the key (and optionally its associated value) of the
        !                specified rank where the applicable range of rank is between
        !                0 and TableSize-1. Also, return a flag indicating whether the
        !                ranked key is successfully retrieved or not. <br>
        !   --->    Flag = Collection%Select(Rank, Key) <br>
        !   --->    IF (.NOT.Collection%Select(Rank, Key, Value)) DoSomething
        PROCEDURE   :: Select       => TreeTable_Select
        !> **Type-Bound Function**: RemoveMin <br>
        !  **Purpose**:  To remove the smallest key (and a value associated with it)
        !                from a symbol table.  Also, return a flag indicating
        !                whether the key is successfully removed or not.  If the
        !                table is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%RemoveMin() <br>
        !   --->    Flag = Collection%RemoveMin(MinKey) <br>
        !   --->    Flag = Collection%RemoveMin(Value=MinVal) <br>
        !   --->    IF (.NOT.Collection%RemoveMin(MinKey, MinVal)) DoSomething
        PROCEDURE   :: RemoveMin    => TreeTable_RemoveMin
        !> **Type-Bound Function**: RemoveMax <br>
        !  **Purpose**:  To remove the largest key (and a value associated with it)
        !                from a symbol table.  Also, return a flag indicating
        !                whether the key is successfully removed or not.  If the
        !                table is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%RemoveMax() <br>
        !   --->    Flag = Collection%RemoveMax(MaxKey) <br>
        !   --->    Flag = Collection%RemoveMax(Value=MaxVal) <br>
        !   --->    IF (.NOT.Collection%RemoveMax(MaxKey, MaxVal)) DoSomething
        PROCEDURE   :: RemoveMax    => TreeTable_RemoveMax
        !> **Type-Bound Function**: GetRangeSize <br>
        !  **Purpose**:  To return the number of keys between *KeyLo* (inclusive)
        !                and *KeyHi* (inclusive). <br>
        !  **Usage**: <br>
        !   --->    RangeSize = Collection%GetRangeSize(KeyLo, KeyHi)
        PROCEDURE   :: GetRangeSize => TreeTable_RangeSize
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by TreeTable Type             -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new collection from an array of items or from another
        !                collection. <br>
        !  **Usage**: <br>
        !   ! create a collection from an array of 25 items <br>
        !   --->    CALL Collection%Construct(25, Arr) <br>
        !   ! create a collection from another collection <br>
        !   --->    CALL Collection%Construct(OtherCollection)
        GENERIC     :: Construct    => TreeTable_CreateByArray
        !> **Type-Bound Subroutine**: UseAVLTree <br>
        !  **Purpose**:  To set the working tree component to work as an AVL tree.  <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%UseAVLTree() <br>
        !  *Note*: The collection must be empty when calling this method.
        PROCEDURE   :: UseAVLTree   => TreeTable_UseAVLTree
        !> **Type-Bound Subroutine**: UseRBTree <br>
        !  **Purpose**:  To set the working tree component to work as an red-black RB tree.  <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%UseRBTree() <br>
        !  *Note*: The collection must be empty when calling this method.
        PROCEDURE   :: UseRBTree   => TreeTable_UseRBTree
        !> **Type-Bound Function**: Check <br>
        !  **Purpose**:  To check integrity of the BST data structure. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Check() <br>
        !   --->    IF (.NOT.Tree%Check(ErrMsg)) DoSomething
        PROCEDURE   :: Check        => TreeTable_CheckIntegrity
        ! ---------------------------------------------------------------------
        ! -----     Deferred/Overridden Procedures from Object Type       -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => TreeTable_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => TreeTable_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => TreeTable_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => TreeTable_ToString
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => TreeTable_HashCode
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: TreeTable_Finalize
        ! ---------------------------------------------------------------------
    END TYPE TreeTable

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
        CALL SrcObj%CopyBSTNode(DstObj)
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
    !^ To check whether LhsObj and RhsObj are equal or not.  This is a deferred procedure by
    !  an *Object* object. <br>
    !  It should be noted that this routine uses all components of the *TabNode* object to
    !  check equality. Therefore, although (A%CompareTo(B) == 0) returns true, (A%IsEqualTo(B))
    !  can return false.

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
        Flag = LhsObj%IsBSTNodeEqual(RhsObj)
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
    
    ! reset the "BSTNode" components
    CALL Obj%ResetBSTNode()

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

FUNCTION TabNode_CompareKey(A, B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare *A* and *B* and return <br>
    !   1 if *A* is greater than *B*, <br>
    !   0 if *A* is equal to *B*, <br>
    !  -1 if *A* is less than *B*, <br>
    !  -999 if type of *B* is invalid. <br>
    !  Also, write an error message to the default log file if this happens. <br>
    !  This is a deferred procedure by an *Comparable* object. <br>
    !  It is important to note that this routine only uses the key component
    !  of the *TabNode* object.  Thus, even though (A%CompareTo(B) == 0)
    !  is true, A%IsEqualTo(B) may be false.

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
        CALL Handle_ErrLevel('TabNode_CompareKey', ModName, ErrSevere, 'Type of B is valid.')
    END SELECT

    RETURN

END FUNCTION TabNode_CompareKey

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

! ---------------------------------------------------------------------
! -----         Deferred Procedures from Object Type          -----
! ---------------------------------------------------------------------

SUBROUTINE TreeTable_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.  This is a deferred procedure
    !  inherited from the *Object* type. <br>
    !  *Note*:  SrcObj must be in the *TreeTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeTable),   INTENT(IN)     :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT)    :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)     :: IsDeep
    !^ Flag indicating whether to perform deep copy or shallow copy. <br>
    !  - If present and true, perform a deep copy. <br>
    !  - If present and false, perform a shallow copy. <br>
    !  - If not present, perform either a shallow or a deep copy that is naturally most
    !    suitable for the object's components.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (DstObj)
    TYPE IS (TreeTable)
        DstObj%Dir      = SrcObj%Dir
        DstObj%IsRBTree = SrcObj%IsRBTree
        CALL SrcObj%NodePool%Copy(DstObj%NodePool, IsDeep)
        CALL SrcObj%ItemPool%CloneTo(DstObj%ItemPool)
        CALL SrcObj%WrkTree%CloneTo(DstObj%WrkTree)
    CLASS DEFAULT
        CALL Handle_ErrLevel('TreeTable_Copy', ModName, ErrSevere, &
                             'Type of the DstObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE TreeTable_Copy

!******************************************************************************

FUNCTION TreeTable_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeTable), INTENT(IN)    :: LhsObj   !! an object
    CLASS(Object),    INTENT(IN)    :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (TreeTable)
        Flag = FalseVal
        IF (LhsObj%GetSize() /= RhsObj%GetSize()) RETURN
        IF (.NOT.LhsObj%IsEmpty()) THEN
            ! implementation note:  we cannot use the iteration methods here
            !   due to the intent of the input data.
            BLOCK
                CLASS(BSTNode), POINTER :: LhsNode, LhsNext, LhsRoot
                CLASS(BSTNode), POINTER :: RhsNode, RhsNext, RhsRoot
                tLogical                :: ReturnNow
                ReturnNow = FalseVal
                ! start iteration
                LhsNode => LhsObj%WrkTree%GetMinNode()
                RhsNode => RhsObj%WrkTree%GetMinNode()
                LhsRoot => LhsObj%WrkTree%GetRoot()
                RhsRoot => RhsObj%WrkTree%GetRoot()
                Loop: DO WHILE (ASSOCIATED(LhsNode).AND.ASSOCIATED(RhsNode))
                    ! get successor nodes
                    CALL Find_Inorder_Successor(LhsRoot, LhsNode, LhsNext)
                    CALL Find_Inorder_Successor(RhsRoot, RhsNode, RhsNext)
                    ! check key and value equalities
                    SELECT TYPE (LhsNode)
                    TYPE IS (TabNode)
                        SELECT TYPE (RhsNode)
                        TYPE IS (TabNode)
                            IF (.NOT.LhsNode%IsEqualTo(RhsNode)) THEN
                                ReturnNow = TrueVal
                                EXIT Loop
                            END IF
                        END SELECT
                    END SELECT
                    ! move to the next iteration
                    LhsNode => LhsNext
                    RhsNode => RhsNext
                END DO Loop
                NULLIFY(LhsNode, RhsNode)
                NULLIFY(LhsNext, RhsNext)
                NULLIFY(LhsRoot, RhsRoot)
                IF (ReturnNow) RETURN
            END BLOCK
        ELSE
            RETURN
        END IF
        Flag = TrueVal
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION TreeTable_IsEqualTo

!******************************************************************************

SUBROUTINE TreeTable_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the TreeTable object.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeTable), INTENT(INOUT)  :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%WrkTree%Clear()
    CALL Obj%NodePool%MemFree()
    CALL Obj%ItemPool%Destruct()
    CALL Obj%FreeMolds()

    RETURN

END SUBROUTINE TreeTable_MemFree

!******************************************************************************

FUNCTION TreeTable_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the TreeTable type.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeTable), INTENT(IN)    :: Obj
    tCharAlloc                      :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: BaseStr

! FLOW

    ! get base string
    IF (Obj%IsEmpty()) THEN
        BaseStr = '[NULL]'
    ELSE
        ! implementation note:  we cannot use the iteration methods here
        ! due to the intent of the input data.
        BLOCK
            CLASS(BSTNode), POINTER :: CurrNode, NextNode, RootNode
            TYPE(CharBuffer)        :: ChrBuf
            tCharAlloc              :: KeyStr, ValStr, ItemStr
            ! initialize
            CALL ChrBuf%CreateEmpty(InitCap=Obj%GetSize()*60_kIndex)
            CALL ChrBuf%Append('[')
            ! start iteration
            CurrNode => Obj%WrkTree%GetMinNode()
            RootNode  => Obj%WrkTree%GetRoot()
            Loop: DO WHILE (ASSOCIATED(CurrNode))
                ! get successor nodes
                CALL Find_Inorder_Successor(RootNode, CurrNode, NextNode)
                ! get string representation of the stored item of the current node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode)
                    KeyStr = CurrNode%Key%ToString()
                    ValStr = CurrNode%Value%ToString()
                    ItemStr = '{' // KeyStr(15:LEN(KeyStr)-1) // ' : ' // ValStr(12:LEN(ValStr)-1) // '}'
                    CALL ChrBuf%Append(ItemStr)
                    IF (ASSOCIATED(NextNode)) CALL ChrBuf%Append(', ')
                END SELECT
                ! move to the next iteration
                CurrNode => NextNode
            END DO Loop
            NULLIFY(CurrNode, NextNode, RootNode)
            CALL ChrBuf%Append(']')
            Str = ChrBuf%AsString()
        END BLOCK
    END IF
    Str = '{TreeTable with ' // ToChar(Obj%GetSize()) // ' items : ' // BaseStr // '}'

    RETURN

END FUNCTION TreeTable_ToString

!******************************************************************************

FUNCTION TreeTable_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeTable), INTENT(IN)    :: Obj
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

END FUNCTION TreeTable_HashCode

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseCollection Type       -----
! ---------------------------------------------------------------------

SUBROUTINE TreeTable_CopyCollection(This, Other, ItemCopy, ValCopy)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items as the given collection (Other).
    !  This is a deferred procedure by the *BaseCollection* class. <br>
    !  *Note*:  Other must be in the *OrderedSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(TreeTable),      INTENT(INOUT)    :: This
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

    ! return if Other is empty
    IF (Other%IsEmpty()) RETURN

    SELECT TYPE (Other)
    TYPE IS (TreeTable)
        ! same type of collection
        CALL Other%Copy(This)
    CLASS IS (OrderedSymTable)
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
        CALL Handle_ErrLevel('TreeTable_CopyCollection', ModName, ErrSevere, &
                             'Type of "Other" must be in the "OrderedSymTable" class.')
    END SELECT
    
    RETURN

END SUBROUTINE TreeTable_CopyCollection

!******************************************************************************

SUBROUTINE TreeTable_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the items from the collection.  This is a deferred procedure
    !  by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TreeTable object
    CLASS(TreeTable), INTENT(INOUT) :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! simply return if the collection is empty
    IF (Collection%IsEmpty()) RETURN

    ! clear elements
    CALL Collection%WrkTree%Clear()

    ! reset components
    Collection%Dir = 0
    CALL Collection%NodePool%Reset()

    ! free mold
    CALL Collection%FreeMolds()

    RETURN

END SUBROUTINE TreeTable_ClearItems

!******************************************************************************

SUBROUTINE TreeTable_Destroy(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the collection.  This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TreeTable object
    CLASS(TreeTable), INTENT(INOUT) :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    CALL Collection%MemFree()

    RETURN

END SUBROUTINE TreeTable_Destroy

!******************************************************************************

FUNCTION TreeTable_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of items in the collection.  This is a deferred procedure
    !  by the *BaseCollection* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeTable), INTENT(IN)    :: Collection
    tIndex                          :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%WrkTree%GetSize()

    RETURN

END FUNCTION TreeTable_GetSize

! ---------------------------------------------------------------------
! -----         Deferred Procedures from BaseSymTable Type        -----
! ---------------------------------------------------------------------

FUNCTION TreeTable_Move2FirstPair(Collection, Key, Value, KeyCopy, ValCopy) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the first (starting) pair data in a symbol table.  For the *TreeTable*, which
    !  is an ordered symbol table, the starting pair is the pair with smallest key.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),   INTENT(INOUT)   :: Collection
    !% the smallest key as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Key
    !% the associated value as output if requested (and available)
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
            CLASS(BSTNode), POINTER :: CurrNode
            ! start iteration
            IsEmpty = Collection%WrkTree%StartMin(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode)
                    ! get key if requested
                    IF (PRESENT(Key)) THEN
                        IF (.NOT.CurrNode%Key%Get(Key)) THEN
                            CALL Handle_ErrLevel('TreeTable_Move2FirstPair', ModName, ErrWarning, &
                                    'Type of the specified key is likely NOT the same as that of stored keys.')
                        END IF
                    END IF
                    ! get value if requested
                    IF (PRESENT(Value)) THEN
                        IF (.NOT.CurrNode%Value%Get(Value, ValCopy)) THEN
                            CALL Handle_ErrLevel('TreeTable_Move2FirstPair', ModName, ErrWarning, &
                                    'Type of the specified key is likely NOT the same as that of stored keys.')
                        END IF
                    END IF
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration
        IsEmpty = Collection%WrkTree%StartMin()
    END IF

    ! set direction
    IF (.NOT.IsEmpty) Collection%Dir = 1

    RETURN

END FUNCTION TreeTable_Move2FirstPair

!******************************************************************************

FUNCTION TreeTable_Move2NextPair(Collection, Key, Value, KeyCopy, ValCopy) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next pair data in a symbol table.  For the *TreeTable*, which is an
    !  ordered symbol table, the next pair is the so-called successor pair.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),   INTENT(INOUT)   :: Collection
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
            CLASS(BSTNode), POINTER :: CurrNode
            ! move to next iteration
            IsTheEnd = Collection%WrkTree%MoveForward(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode)
                    ! get key if requested
                    IF (PRESENT(Key)) THEN
                        IF (.NOT.CurrNode%Key%Get(Key)) THEN
                            CALL Handle_ErrLevel('TreeTable_Move2NextPair', ModName, ErrWarning, &
                                    'Type of the specified key is likely NOT the same as that of stored keys.')
                        END IF
                    END IF
                    ! get value if requested
                    IF (PRESENT(Value)) THEN
                        IF (.NOT.CurrNode%Value%Get(Value, ValCopy)) THEN
                            CALL Handle_ErrLevel('TreeTable_Move2NextPair', ModName, ErrWarning, &
                                    'Type of the specified key is likely NOT the same as that of stored keys.')
                        END IF
                    END IF
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! move to next iteration
        IsTheEnd = Collection%WrkTree%MoveForward()
    END IF

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        Collection%Dir = 1
    ELSE
        Collection%Dir = 0
    END IF

    RETURN

END FUNCTION TreeTable_Move2NextPair

!******************************************************************************

FUNCTION TreeTable_Move2LastPair(Collection, Key, Value, ValCopy) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the last (starting in a reversed order) pair data in a symbol table. For the
    !  *TreeTable*, which is an ordered symbol table,  the starting pair in a reversed order
    !  is the pair with greatest key.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),   INTENT(INOUT)   :: Collection
    !% the greatest key as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Key
    !% the associated value as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Value
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
            CLASS(BSTNode), POINTER :: CurrNode
            ! start iteration
            IsEmpty = Collection%WrkTree%StartMax(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode)
                    ! get key if requested
                    IF (PRESENT(Key)) THEN
                        IF (.NOT.CurrNode%Key%Get(Key)) THEN
                            CALL Handle_ErrLevel('TreeTable_Move2LastPair', ModName, ErrWarning, &
                                    'Type of the specified key is likely NOT the same as that of stored keys.')
                        END IF
                    END IF
                    ! get value if requested
                    IF (PRESENT(Value)) THEN
                        IF (.NOT.CurrNode%Value%Get(Value, ValCopy)) THEN
                            CALL Handle_ErrLevel('TreeTable_Move2LastPair', ModName, ErrWarning, &
                                    'Type of the specified key is likely NOT the same as that of stored keys.')
                        END IF
                    END IF
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration
        IsEmpty = Collection%WrkTree%StartMax()
    END IF

    ! set direction
    IF (.NOT.IsEmpty) Collection%Dir = -1

    RETURN

END FUNCTION TreeTable_Move2LastPair

!******************************************************************************

FUNCTION TreeTable_Move2PrevPair(Collection, Key, Value, ValCopy) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the previous pair data in a symbol table.  For the *TreeTable*, which is an
    !  ordered symbol table,  the previous pair is the so-called predecessor pair.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),   INTENT(INOUT)   :: Collection
    !% the previous key as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Key
    !% the previous value as output if requested (and available)
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Value
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
            CLASS(BSTNode), POINTER :: CurrNode
            ! start iteration
            IsTheEnd = Collection%WrkTree%MoveBackward(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode)
                    ! get key if requested
                    IF (PRESENT(Key)) THEN
                        IF (.NOT.CurrNode%Key%Get(Key)) THEN
                            CALL Handle_ErrLevel('TreeTable_Move2PrevPair', ModName, ErrWarning, &
                                    'Type of the specified key is likely NOT the same as that of stored keys.')
                        END IF
                    END IF
                    ! get value if requested
                    IF (PRESENT(Value)) THEN
                        IF (.NOT.CurrNode%Value%Get(Value, ValCopy)) THEN
                            CALL Handle_ErrLevel('TreeTable_Move2PrevPair', ModName, ErrWarning, &
                                    'Type of the specified key is likely NOT the same as that of stored keys.')
                        END IF
                    END IF
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration
        IsTheEnd = Collection%WrkTree%MoveBackward()
    END IF

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        Collection%Dir = -1
    ELSE
        Collection%Dir = 0
    END IF

    RETURN

END FUNCTION TreeTable_Move2PrevPair

!******************************************************************************

SUBROUTINE TreeTable_Insert(Collection, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a key-value pair into a symbol table.  If the specified key is already stored
    !  in the table, replace the old value with the new one.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable), INTENT(INOUT) :: Collection
    !% the key to be added to the collection
    CLASS(*),         INTENT(IN)    :: Key
    !% the associated value to be added to the collection
    CLASS(*),         INTENT(IN)    :: Value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical                    :: KeyFound
    TYPE(TabNode),   POINTER    :: KeyNode
    CLASS(CompNode), POINTER    :: NewNode

! FLOW

    ! check the specified key and value
    IF (.NOT.Collection%IsKeyValid(Key, IsOrderedKey=TrueVal)) THEN
        CALL Handle_ErrLevel('TreeTable_Insert', ModName, ErrSevere, &
                'Type of the specified key is either invalid or NOT the same as that of stored keys.')
        RETURN
    ELSEIF (.NOT.Collection%IsValValid(Value)) THEN
        CALL Handle_ErrLevel('TreeTable_Insert', ModName, ErrSevere, &
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
            CALL Collection%NodePool%Construct(KeyNode)
            CALL Collection%ItemPool%Construct()
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
        ! insert the new node to the working tree
        IF (Collection%IsRBTree) THEN
            ! WrkTree is a red-black tree.
            CALL Collection%WrkTree%Insert(KeyNode)
        ELSE
            ! WrkTree is an AVL tree.
            CALL Collection%WrkTree%IntrusiveAVLTree%Insert(KeyNode)
        END IF
    END IF
    
    ! free working pointer
    NULLIFY(KeyNode, NewNode)

    RETURN

END SUBROUTINE TreeTable_Insert

!******************************************************************************

SUBROUTINE TreeTable_Delete(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete a key-value pair from a symbol table.  This procedure is intended
    !  to be used in conjunction with the *StartFirst* and *MoveForward* procedures.
    !  Therefore, after the call to either procedure and then calling this procedure
    !  will result in a removal of the current key-value pair of the iteration (i.e.
    !  the same key-value pair that can be retrieved via those *Move* procedures). <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable), INTENT(INOUT) :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: CurrNode
    tLogical                :: Success
    tLogical                :: IsTheEnd

! FLOW

    ! get the cursor node
    CurrNode => Collection%WrkTree%GetCursor()
    
    ! check if the node is associated
    IF (ASSOCIATED(CurrNode)) THEN
        ! reset cursor
        IF (Collection%Dir == 1) THEN
            ! forward iteration so move cursor backward
            IsTheEnd = Collection%WrkTree%MoveBackward()
        ELSE
            ! backward iteration so move cursor forward
            IsTheEnd = Collection%WrkTree%MoveForward()
        END IF
        IF (Collection%IsRBTree) THEN
            ! WrkTree is a red-black tree.
            Success = Collection%WrkTree%Remove(CurrNode)
        ELSE
            ! WrkTree is an AVL tree.
            Success = Collection%WrkTree%IntrusiveAVLTree%Remove(CurrNode)
        END IF
        ! check if remove the node successfully or not
        IF (Success) THEN
            ! return the node to the node's pool
            SELECT TYPE (DelNode => CurrNode)
            TYPE IS (TabNode)
                CALL Collection%NodePool%ReturnNode(DelNode)
            END SELECT
        ELSE
            CALL Handle_ErrLevel('TreeTable_Delete', ModName, ErrWarning, &
                       'Check the Remove method of the intrusive tree for possible bug(s).')
        END IF
    END IF
    
    ! free working pointer
    NULLIFY(CurrNode)

    RETURN

END SUBROUTINE TreeTable_Delete

!******************************************************************************

FUNCTION TreeTable_Remove(Collection, Key) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To delete the specified key (and its associated value) from a symbol table.  Also, return
    !  a flag indicating whether the key-value pair is successfully removed or not.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable), INTENT(INOUT) :: Collection
    !% the key to be removed from the collection
    CLASS(*),         INTENT(IN)    :: Key
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode), POINTER  :: KeyNode
    tLogical                :: Success

! FLOW
    
    ! check whether the key is stored in the collection or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
    ELSE
        Flag = Collection%FindKey(Key, KeyNode)
    END IF
    
    IF (Flag) THEN
        IF (Collection%IsRBTree) THEN
            ! WrkTree is a red-black tree.
            Success = Collection%WrkTree%Remove(KeyNode)
        ELSE
            ! WrkTree is an AVL tree.
            Success = Collection%WrkTree%IntrusiveAVLTree%Remove(KeyNode)
        END IF
        ! check if remove the node successfully or not
        IF (Success) THEN
            ! return the node to the node's pool
            CALL Collection%NodePool%ReturnNode(KeyNode)
        ELSE
            CALL Handle_ErrLevel('TreeTable_Remove', ModName, ErrWarning, &
                       'Check the Remove method of the intrusive tree for possible bug(s).')
        END IF
    END IF
    
    ! free working pointer
    NULLIFY(KeyNode)

    RETURN

END FUNCTION TreeTable_Remove

!******************************************************************************

FUNCTION TreeTable_Contain(Collection, Key) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is currently stored in a symbol table.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable), INTENT(INOUT) :: Collection
    !% the key to be looked for in the collection
    CLASS(*),         INTENT(IN)    :: Key
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

END FUNCTION TreeTable_Contain

!******************************************************************************

FUNCTION TreeTable_GetValue(Collection, Key, Value, ValCopy) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a value associated with the specified key in a symbol table.
    !  Also, return a flag indicating whether the key-value pair is successfully
    !  found or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),    INTENT(INOUT)  :: Collection
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
    TYPE(TabNode), POINTER :: KeyNode

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

END FUNCTION TreeTable_GetValue

!**************************************************************************************

FUNCTION TreeTable_ToArray(Collection, Keys, Values, KeyCopy, ValCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove all key-value pairs from the collection.  Also, return a flag
    !  indicating whether the pairs are successfully retrieved and removed or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TreeTable object
    CLASS(TreeTable),    INTENT(INOUT)  :: Collection
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

END FUNCTION TreeTable_ToArray

!**************************************************************************************

FUNCTION TreeTable_GetAll(Collection, Keys, Values, KeyCopy, ValCopy) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all keys and/or all values (without removing them) from the collection.  Also,
    !  return a flag indicating whether the keys and/or values are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TreeTable object
    CLASS(TreeTable),    INTENT(INOUT)  :: Collection
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
            Success = Collection%GetAllKeys(Keys)
            IF (Success) Success = Collection%GetAllVals(Values, ValCopy)
        ELSEIF (SameKeyType) THEN
            Success = FalseVal
            CALL Handle_ErrLevel('TreeTable_GetAll', ModName, ErrSevere, &
                                 'Type of the specified values is NOT the same as that of stored values.')
        ELSEIF (SameValType) THEN
            Success = FalseVal
            CALL Handle_ErrLevel('TreeTable_GetAll', ModName, ErrSevere, &
                                 'Type of the specified keys is NOT the same as that of stored keys.')
        ELSE
            Success = FalseVal
            CALL Handle_ErrLevel('TreeTable_GetAll', ModName, ErrSevere, &
                                 'Types of both keys and values are NOT the same as those of stored pairs.')
        END IF
    ELSEIF (PRESENT(Keys)) THEN
        ! check whether type of the specified keys is valid or not
        IF (Collection%IsKeyValid(Keys(1), IsOrderedKey=FalseVal)) THEN
            Success = Collection%GetAllKeys(Keys)
        ELSE
            Success = FalseVal
            CALL Handle_ErrLevel('TreeTable_GetAll', ModName, ErrSevere, &
                                 'Type of the specified keys is NOT the same as that of stored keys.')
        END IF
    ELSEIF (PRESENT(Values)) THEN
        ! check whether type of the specified values is valid or not
        IF (Collection%IsValValid(Values(1))) THEN
            Success = Collection%GetAllVals(Values, ValCopy)
        ELSE
            Success = FalseVal
            CALL Handle_ErrLevel('TreeTable_GetAll', ModName, ErrSevere, &
                                 'Type of the specified values is NOT the same as that of stored values.')
        END IF
    END IF

    RETURN

END FUNCTION TreeTable_GetAll

!**************************************************************************************

FUNCTION TreeTable_GetAllKeys(Collection, Keys)  RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all keys (without removing them) from the collection.  Also,
    !  return a flag indicating whether the values are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TreeTable object
    CLASS(TreeTable), INTENT(INOUT) :: Collection
    !% the keys to be retrieved and removed from the collection
    CLASS(*),         INTENT(INOUT) :: Keys(:)
    !> flag indicating whether the values are successfully retrieved. <br>
    tLogical                        :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: I, ArrSize
    tLogical                :: IsTheEnd
    CLASS(BSTNode), POINTER :: CurrNode

! FLOW

    ! initialize local variables
    ArrSize = SIZE(Keys, KIND=kIndex)
    IF (ArrSize < 1_kindex) RETURN
    I = 1_kindex

    ! loop through the collection and get the keys along the way
    IsTheEnd = Collection%WrkTree%StartMin(CurrNode)
    DO WHILE ((.NOT.IsTheEnd).AND.ASSOCIATED(CurrNode))
        SELECT TYPE (CurrNode)
        TYPE IS (TabNode)
            Success = CurrNode%Key%Get(Keys(I))
            IF (.NOT.Success) EXIT
            I = I + 1_kIndex
            IF (I > ArrSize) EXIT
        END SELECT
        IsTheEnd = Collection%WrkTree%MoveForward(CurrNode)
    END DO

    RETURN

END FUNCTION TreeTable_GetAllKeys

!**************************************************************************************

FUNCTION TreeTable_GetAllVals(Collection, Values, ValCopy)  RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all values (without removing them) from the collection.  Also,
    !  return a flag indicating whether the values are successfully retrieved.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TreeTable object
    CLASS(TreeTable),    INTENT(INOUT)  :: Collection
    !% the values to be retrieved and removed from the collection
    CLASS(*),            INTENT(INOUT)  :: Values(:)
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !> flag indicating whether the values are successfully retrieved. <br>
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: I, ArrSize
    tLogical                :: IsTheEnd
    CLASS(BSTNode), POINTER :: CurrNode

! FLOW

    ! initialize local variables
    ArrSize = SIZE(Values, KIND=kIndex)
    IF (ArrSize < 1_kindex) RETURN
    I = 1_kindex

    ! loop through the collection and get the values along the way
    IsTheEnd = Collection%WrkTree%StartMin(CurrNode)
    DO WHILE ((.NOT.IsTheEnd).AND.ASSOCIATED(CurrNode))
        SELECT TYPE (CurrNode)
        TYPE IS (TabNode)
            Success = CurrNode%Value%Get(Values(I), ValCopy)
            IF (.NOT.Success) EXIT
            I = I + 1_kIndex
            IF (I > ArrSize) EXIT
        END SELECT
        IsTheEnd = Collection%WrkTree%MoveForward(CurrNode)
    END DO

    RETURN

END FUNCTION TreeTable_GetAllVals

! ---------------------------------------------------------------------
! -----         Deferred Procedures from OrderedSymTable Type     -----
! ---------------------------------------------------------------------

FUNCTION TreeTable_GetMinKey(Collection, Key, Value, ValCopy) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the smallest key (and optionally a value associated with it) in a symbol table.
    !  Also, return a flag indicating whether the key is successfully retrieved or not.  If
    !  the table is empty, the flag is typically false.  Otherwise, the flag is always true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),   INTENT(INOUT)   :: Collection
    !% the smallest key to be retrieved from the collection
    CLASS(*),           INTENT(INOUT)   :: Key
    !% the value associated with the smallest key
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Value
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !% flag indicating whether the key-value pair is found or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: MinNode

! FLOW
    
    ! get node with smallest key
    MinNode => Collection%WrkTree%GetMinNode()
    IF (ASSOCIATED(MinNode)) THEN
        SELECT TYPE (MinNode)
        TYPE IS (TabNode)
            Flag = MinNode%Key%Get(Key)
            IF (PRESENT(Value).AND.Flag) Flag = MinNode%Value%Get(Value, ValCopy)
        END SELECT
    ELSE
        Flag = FalseVal
    END IF
    
    ! free working pointer
    NULLIFY(MinNode)

    RETURN

END FUNCTION TreeTable_GetMinKey

!******************************************************************************

FUNCTION TreeTable_GetMaxKey(Collection, Key, Value, ValCopy) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the largest key (and optionally a value associated with it)
    !  in a symbol table.  Also, return a flag indicating whether the key
    !  is successfully retrieved or not.  If the table is empty, the flag
    !  is typically false.  Otherwise, the flag is always true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),   INTENT(INOUT)   :: Collection
    !% the largest key to be retrieved from the collection
    CLASS(*),           INTENT(INOUT)   :: Key
    !% the value associated with the largest key
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Value
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !% flag indicating whether the key-value pair is found or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: MaxNode

! FLOW
    
    ! get node with largest key
    MaxNode => Collection%WrkTree%GetMaxNode()
    IF (ASSOCIATED(MaxNode)) THEN
        SELECT TYPE (MaxNode)
        TYPE IS (TabNode)
            Flag = MaxNode%Key%Get(Key)
            IF (PRESENT(Value).AND.Flag) Flag = MaxNode%Value%Get(Value, ValCopy)
        END SELECT
    ELSE
        Flag = FalseVal
    END IF
    
    ! free working pointer
    NULLIFY(MaxNode)

    RETURN

END FUNCTION TreeTable_GetMaxKey

!******************************************************************************

FUNCTION TreeTable_Floor(Collection, KeyIn, KeyOut, ValOut, ValCopy) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To the largest key (and optionally a value associated with the key) in
    !  a collection less than or equal to the given key.  Also, return a flag
    !  indicating whether the floor key is successfully retrieved or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),   INTENT(INOUT)   :: Collection
    !% the specified key
    CLASS(*),           INTENT(IN)      :: KeyIn
    !% the largest key in the table less than or equal to the given key
    CLASS(*),           INTENT(INOUT)   :: KeyOut
    !% the value associated with the largest key
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: ValOut
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !% flag indicating whether the key-value pair is found or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode)           :: KeyNode
    CLASS(BSTNode), POINTER :: FloorNode

! FLOW
    
    ! set key
    CALL KeyNode%Key%Set(KeyIn, Collection%ItemPool)
    
    ! get floor node
    FloorNode => Collection%WrkTree%Floor(KeyNode)
    IF (ASSOCIATED(FloorNode)) THEN
        SELECT TYPE (FloorNode)
        TYPE IS (TabNode)
            Flag = FloorNode%Key%Get(KeyOut)
            IF (PRESENT(ValOut).AND.Flag) Flag = FloorNode%Value%Get(ValOut, ValCopy)
        END SELECT
    ELSE
        Flag = FalseVal
    END IF
    
    ! free working pointer
    NULLIFY(FloorNode)

    RETURN

END FUNCTION TreeTable_Floor

!******************************************************************************

FUNCTION TreeTable_Ceiling(Collection, KeyIn, KeyOut, ValOut, ValCopy) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To the smallest key (and optionally a value associated with the key) in
    !  a collection greater than or equal to the given key.  Also, return a flag
    !  indicating whether the floor key is successfully retrieved or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),   INTENT(INOUT)   :: Collection
    !% the specified key
    CLASS(*),           INTENT(IN)      :: KeyIn
    !% the smallest key in the table greater than or equal to the given key
    CLASS(*),           INTENT(INOUT)   :: KeyOut
    !% the value associated with the smallest key
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: ValOut
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !% flag indicating whether the key-value pair is found or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode)           :: KeyNode
    CLASS(BSTNode), POINTER :: CeilingNode

! FLOW
    
    ! set key
    CALL KeyNode%Key%Set(KeyIn, Collection%ItemPool)
    
    ! get floor node
    CeilingNode => Collection%WrkTree%Ceiling(KeyNode)
    IF (ASSOCIATED(CeilingNode)) THEN
        SELECT TYPE (CeilingNode)
        TYPE IS (TabNode)
            Flag = CeilingNode%Key%Get(KeyOut)
            IF (PRESENT(ValOut).AND.Flag) Flag = CeilingNode%Value%Get(ValOut, ValCopy)
        END SELECT
    ELSE
        Flag = FalseVal
    END IF
    
    ! free working pointer
    NULLIFY(CeilingNode)

    RETURN

END FUNCTION TreeTable_Ceiling

!******************************************************************************

FUNCTION TreeTable_GetRank(Collection, Key) RESULT(Rank)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of keys in the symbol table strictly less than the given key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable), INTENT(INOUT) :: Collection
    !% the specified key
    CLASS(*),         INTENT(IN)    :: Key
    !% the number of keys less than the given key.
    tIndex                          :: Rank

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode)   :: KeyNode

! FLOW
    
    ! set key
    CALL KeyNode%Key%Set(Key, Collection%ItemPool)
    
    ! get rank
    Rank = Collection%WrkTree%Rank(KeyNode)

    RETURN

END FUNCTION TreeTable_GetRank

!******************************************************************************

FUNCTION TreeTable_Select(Collection, Rank, Key, Value, ValCopy) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the key (and optionally its associated value) of the given rank.
    !  Also, return a flag indicating whether the ranked key is successfully
    !  retrieved or not. <br>
    !  This ranked key has the property such that there are keys in the symbol
    !  table that are smaller.  In other words, this key is the (rank+1)st smallest
    !  key in the table. <br>
    !  The applicable range of rank is between 0 and TableSize-1 where the rank number
    !  is zero-based.  If the specified rank is out of range or the table is empty,
    !  the returned flag is false.  Otherwise, the returned flag is true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),   INTENT(INOUT)   :: Collection
    !% the specified rank.
    tIndex,             INTENT(IN)      :: Rank
    !% the key of the specified rank
    CLASS(*),           INTENT(INOUT)   :: Key
    !% the value associated with the ranked key
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Value
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !% flag indicating whether the key-value pair is found or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: RankNode

! FLOW
    
    ! get rank node
    RankNode => Collection%WrkTree%Select(Rank)

    ! get key and optionally its associated value
    IF (ASSOCIATED(RankNode)) THEN
        SELECT TYPE (RankNode)
        TYPE IS (TabNode)
            Flag = RankNode%Key%Get(Key)
            IF (PRESENT(Value).AND.Flag) Flag = RankNode%Value%Get(Value, ValCopy)
        END SELECT
    ELSE
        Flag = FalseVal
    END IF
    
    ! free working pointer
    NULLIFY(RankNode)

    RETURN

END FUNCTION TreeTable_Select

!******************************************************************************

FUNCTION TreeTable_RemoveMin(Collection, Key, Value, ValCopy) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the smallest key and a value associated with the key from
    !  a symbol table.  Also, return a flag indicating whether the key is
    !  successfully removed or not.  If the table is empty, the flag is
    !  typically false.  Otherwise, the flag is always true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),   INTENT(INOUT)   :: Collection
    !% the smallest key
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Key
    !% the value associated with the smallest key
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Value
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !% flag indicating whether the key is successfully removed or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF ((PRESENT(Key)).OR.(PRESENT(Value))) THEN
        BLOCK
            ! local variable
            CLASS(BSTNode), POINTER :: MinNode
            ! remove node with smallest key
            IF (Collection%IsRBTree) THEN
                ! WrkTree is a red-black tree
                Flag = Collection%WrkTree%RemoveMin(MinNode)
            ELSE
                ! WrkTree is an AVL tree
                Flag = Collection%WrkTree%IntrusiveAVLTree%RemoveMin(MinNode)
            END IF
            ! optionally get key and its associated value
            IF (ASSOCIATED(MinNode)) THEN
                SELECT TYPE (MinNode)
                TYPE IS (TabNode)
                    IF (PRESENT(Key)) THEN
                        IF (.NOT.MinNode%Key%Get(Key)) THEN
                            CALL Handle_ErrLevel('TreeTable_RemoveMin', ModName, ErrSevere, &
                                'Type of the specified key is invalid or is NOT the same as that of stored keys.')
                        END IF
                    END IF
                    IF (PRESENT(Value)) THEN
                        IF (.NOT.MinNode%Value%Get(Value, ValCopy)) THEN
                            CALL Handle_ErrLevel('TreeTable_RemoveMin', ModName, ErrSevere, &
                                'Type of the specified value is NOT the same as that of stored values.')
                        END IF
                    END IF
                END SELECT
            END IF
            ! free working pointer
            NULLIFY(MinNode)
        END BLOCK
    ELSE
        ! remove node with smallest key
        IF (Collection%IsRBTree) THEN
            ! WrkTree is a red-black tree
            Flag = Collection%WrkTree%RemoveMin()
        ELSE
            ! WrkTree is an AVL tree
            Flag = Collection%WrkTree%IntrusiveAVLTree%RemoveMin()
        END IF
    END IF

    RETURN

END FUNCTION TreeTable_RemoveMin

!******************************************************************************

FUNCTION TreeTable_RemoveMax(Collection, Key, Value, ValCopy) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the largest key and a value associated with the key from
    !  a symbol table.  Also, return a flag indicating whether the key is
    !  successfully removed or not.  If the table is empty, the flag is
    !  typically false.  Otherwise, the flag is always true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),   INTENT(INOUT)   :: Collection
    !% the largest key
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Key
    !% the value associated with the largest key
    CLASS(*), OPTIONAL, INTENT(INOUT)   :: Value
    !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
    !  required if the derived type has allocatable/pointer component(s).
    PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
    !% flag indicating whether the key is successfully removed or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF ((PRESENT(Key)).OR.(PRESENT(Value))) THEN
        BLOCK
            ! local variable
            CLASS(BSTNode), POINTER :: MaxNode
            ! remove node with largest key
            IF (Collection%IsRBTree) THEN
                ! WrkTree is a red-black tree
                Flag = Collection%WrkTree%RemoveMax(MaxNode)
            ELSE
                ! WrkTree is an AVL tree
                Flag = Collection%WrkTree%IntrusiveAVLTree%RemoveMax(MaxNode)
            END IF
            ! optionally get key and its associated value
            IF (ASSOCIATED(MaxNode)) THEN
                SELECT TYPE (MaxNode)
                TYPE IS (TabNode)
                    IF (PRESENT(Key)) THEN
                        IF (.NOT.MaxNode%Key%Get(Key)) THEN
                            CALL Handle_ErrLevel('TreeTable_RemoveMax', ModName, ErrSevere, &
                                'Type of the specified key is invalid or is NOT the same as that of stored keys.')
                        END IF
                    END IF
                    IF (PRESENT(Value)) THEN
                        IF (.NOT.MaxNode%Value%Get(Value, ValCopy)) THEN
                            CALL Handle_ErrLevel('TreeTable_RemoveMax', ModName, ErrSevere, &
                                'Type of the specified value is NOT the same as that of stored values.')
                        END IF
                    END IF
                END SELECT
            END IF
            ! free working pointer
            NULLIFY(MaxNode)
        END BLOCK
    ELSE
        ! remove node with largest key
        IF (Collection%IsRBTree) THEN
            ! WrkTree is a red-black tree
            Flag = Collection%WrkTree%RemoveMax()
        ELSE
            ! WrkTree is an AVL tree
            Flag = Collection%WrkTree%IntrusiveAVLTree%RemoveMax()
        END IF
    END IF

    RETURN

END FUNCTION TreeTable_RemoveMax

!******************************************************************************

FUNCTION TreeTable_RangeSize(Collection, KeyLo, KeyHi) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of keys in the given range (between KeyLo and KeyHi).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable), INTENT(INOUT) :: Collection
    !% the minimum key (inclusive)
    CLASS(*),         INTENT(IN)    :: KeyLo
    !% the maximum key (inclusive)
    CLASS(*),         INTENT(IN)    :: KeyHi
    !% the number of keys in the given range.
    tIndex                          :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode)   :: NodeLo
    TYPE(TabNode)   :: NodeHi

! FLOW
    
    ! set keys
    CALL NodeLo%Key%Set(KeyLo, Collection%ItemPool)
    CALL NodeHi%Key%Set(KeyHi, Collection%ItemPool)
    
    ! get range size
    Size = Collection%WrkTree%GetRangeSize(NodeLo, NodeHi)

    RETURN

END FUNCTION TreeTable_RangeSize

! ---------------------------------------------------------------------
! -----         Specific Procedures for TreeTable Type            -----
! ---------------------------------------------------------------------

SUBROUTINE TreeTable_CreateByArray(Collection, N, Keys, Values)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a table from an array of key-value pairs.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeTable), INTENT(INOUT) :: Collection   !! collection
    tIndex,           INTENT(IN)    :: N            !! number of key-value pairs
    !% the keys to be added to the table
    CLASS(*),         INTENT(IN)    :: Keys(N)
    !% the associated values to be added to the table
    CLASS(*),         INTENT(IN)    :: Values(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! add key-value pairs to the table
    DO I = 1_kIndex, N
        CALL Collection%Insert(Keys(I), Values(I))
    END DO

    RETURN

END SUBROUTINE TreeTable_CreateByArray

!******************************************************************************

SUBROUTINE TreeTable_UseAVLTree(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set the *IsRBTree* component to false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object
    CLASS(TreeTable), INTENT(INOUT) :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Collection%IsRBTree = FalseVal

    RETURN

END SUBROUTINE TreeTable_UseAVLTree

!******************************************************************************

SUBROUTINE TreeTable_UseRBTree(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set the *IsRBTree* component to true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object
    CLASS(TreeTable), INTENT(INOUT) :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Collection%IsRBTree = TrueVal

    RETURN

END SUBROUTINE TreeTable_UseRBTree

!******************************************************************************

FUNCTION TreeTable_CheckIntegrity(Collection, Message) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check integrity of the binary-search-tree (BST) data structure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),     INTENT(INOUT) :: Collection
    !> message indicating the reason why the tree did not pass the
    !  integrity test
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: Message
    !> flag for integrity <br>
    ! - true if the tree passed the integrity test.
    ! - false if the tree did not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (Collection%IsRBTree) THEN
        ! WrkTree is a red-black tree
        Flag = Collection%WrkTree%Check(Message)
    ELSE
        ! WrkTree is an AVL tree
        Flag = Collection%WrkTree%IntrusiveAVLTree%Check(Message)
    END IF

    RETURN

END FUNCTION TreeTable_CheckIntegrity

! ---------------------------------------------------------------------
! -----                 Auxiliary Procedures                      -----
! ---------------------------------------------------------------------

FUNCTION TreeTable_FindKey(Collection, Key, KeyNode) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the specified key in a symbol table.  Optionally, return
    !  a stored node containing a key equal to the specified key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable),                 INTENT(INOUT) :: Collection
    !% the key to be looked for in the collection
    CLASS(*),                         INTENT(IN)    :: Key
    !% the node containing the specified key; null pointer if the key is not found
    TYPE(TabNode), OPTIONAL, POINTER, INTENT(OUT)   :: KeyNode
    !% flag indicating whether the specified key is found or not.
    tLogical                                        :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode)           :: InNode
    CLASS(BSTNode), POINTER :: StoredNode

! FLOW

    ! initialize
    Found = Falseval
    IF (PRESENT(KeyNode)) KeyNode => NULL()

    ! check the specified key and return quickly if not valid
    IF (.NOT.Collection%IsKeyValid(Key, IsOrderedKey=TrueVal)) RETURN

    ! set key for search node
    CALL InNode%Key%Set(Key, Collection%ItemPool)

    ! find the stored node equal to input node
    Found = Collection%WrkTree%Contain(InNode, StoredNode)
    IF (PRESENT(KeyNode).AND.Found) THEN
        SELECT TYPE (StoredNode)
        TYPE IS (TabNode)
            KeyNode => StoredNode
        END SELECT
    END IF

    ! free working pointer
    NULLIFY(StoredNode)

    RETURN

END FUNCTION TreeTable_FindKey

! ---------------------------------------------------------------------
! -----             Final Procedures                              -----
! ---------------------------------------------------------------------

SUBROUTINE TreeTable_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(TreeTable), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE TreeTable_Finalize

!******************************************************************************

END MODULE MClass_TreeTable

!******************************************************************************
