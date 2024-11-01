
MODULE MClass_IntrusiveHashTree

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *IntrusiveHashTree* type, the *HashTreeNode* type and
!   their related routines.  The *IntrusiveHashTree* type is a container type that
!   employs a separate-chaining hash table implementation to provide common operations
!   for an unordered symbol table while the *HashTreeNode* type is a node type intended
!   to be used in conjunction with the *IntrusiveHashTree* type. <br>
!   As an intrusive container, the *IntrusiveHashTree* type requires a user to define
!   a new node type that extends the *HashTreeNode* type, which is a subtype of the
!   <a href="../module/MClass_IntrusiveBSTrees.html#type-bstode">BSTNode</a> type.  The
!   new node type must contain a key (or more than one) as its additional component(s).
!   The new node type may contain one or more associated values as its other component(s).
!   The user is also required to implement all procedures deferred by the *HashTreeNode*
!   type and its parent types.  In particular, the *CompareTo* and *ComputeHashValue*
!   deferred procedures should be dependent on the key component(s) only. <br>
!   Unlike intrusive list-based and tree-based types, which can be used instantly by
!   inserting objects into a container, the *IntrusiveHashTree* type requires an explicit
!   construction via the *Construct* method before using other provided operations. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE ISO_C_BINDING,                  ONLY: C_SIZEOF, C_LOC, C_NULL_PTR, C_PTR, C_F_POINTER
    USE, INTRINSIC  :: ISO_FORTRAN_ENV, ONLY: CHARACTER_STORAGE_SIZE
    USE MBase_SIntUtil,                 ONLY: ToChar => ToDecStrSigned
    USE MClass_IntrusiveBSTrees,        ONLY: BSTNode, IntrusiveRBTree
    USE MBase_ErrHandlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: HashTreeNode
    PUBLIC :: IntrusiveHashTree

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define     tHash               tIndex

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_IntrusiveHashTree'
    ! The maximum (positive) number of hash code
#ifdef Indx32Bits
    tSInt32,   PARAMETER    :: MaxHash = ToInt32(Z'7FFFFFFF')
#else
    tSInt64,   PARAMETER    :: MaxHash  = ToInt64(Z'7FFFFFFFFFFFFFFF')
#endif

!** DERIVED TYPE DEFINITIONS
    !> The *HashTreeNode* type is a binary-search-tree node type intended to be
    !  used in conjunction with the *IntrusiveHashTree* type.  Similar to other
    !  intrusive containers, the *IntrusiveHashTree* type requires a user to
    !  define a new (concrete) node type that extends the *HashTreeNode* type
    !  where the new node type contains a key and its associated value as its
    !  components. <br>
    !  The following code snippet illustrates how to define a new node type.
    !   <Pre><Code style="color:MidnightBlue;">
    !   TYPE, EXTENDS(HashTreeNode) :: AssociativeNode
    !       CHARACTER(LEN=:), ALLOCATABLE   :: Key      ! stored string key
    !       INTEGER                         :: Value1   ! stored integer value
    !       REAL                            :: Value2   ! stored real value
    !   END TYPE
    !   </Code></Pre>
    !  The *HashTreeNode* type is a subtype of the *BSTNode* type and it defines the
    !  *ComputeHashValue* method required by the *IntrusiveHashTree* type.  Therefore,
    !  a user must implement this deferred procedure in addition to other deferred
    !  procedures required by all its parent types.
    TYPE, ABSTRACT, EXTENDS(BSTNode) :: HashTreeNode
        PRIVATE
        !% hash code of the stored key
        tHash       :: KeyHash = 0_kIndex
    CONTAINS
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: ResetHashNode <br>
        !  **Purpose**:  To reset the node to its initialized state. <br>
        !  **Usage**: <br>
        !   --->    CALL Node%ResetHashNode()
        PROCEDURE   :: ResetHashNode    => HashTreeNode_Reset
        !> **Type-Bound Subroutine**: CopyHashNode <br>
        !  **Purpose**:  To copy members of the source to the destination.  This method
        !                is provided to help user implement deferred procedure(s) required
        !                by an object in the *Object* class. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcNode%CopyHashNode(DstNode)
        PROCEDURE   :: CopyHashNode     => HashTreeNode_CopyHashNode
        !> **Type-Bound Function**: EqualTo <br>
        !  **Purpose**:  To compare whether all members of both objects are equal or not.
        !                This method is provided to help user implement deferred procedure(s)
        !                required by an object in the *Object* class. <br>
        !  **Usage**: <br>
        !   --->    Flag = LhsNode%EqualTo(RhsNode) <br>
        !   --->    IF (.NOT.LhsNode%EqualTo(RhsNode)) DoSomething
        PROCEDURE   :: IsHashNodeEqual  => HashTreeNode_EqualTo
        ! ---------------------------------------------------------------------
    END TYPE HashTreeNode
    !> The *IntrusiveHashTree* type is an intrusive container type that employs a
    !  separate-chaining hash table implementation to provide common operations for
    !  an unordered symbol table.  The separate-chaining technique combines a balanced
    !  binary-search tree with a hash table to solve the collision problem.  <br>
    !  The *IntrusiveHashTree* type employs an array of the *IntrusiveRBTree* type
    !  as its component (i.e. using an array of balanced trees as its buckets) to store
    !  *HashTreeNode* objects.  Although the *IntrusiveHashTree* type handles a memory
    !  management of its working trees, these working trees (as intrusive containers)
    !  only provide a linking/unlinking task for common balanced-tree operations.  This
    !  means that the memory management task of the inserted *HashTreeNode* objects
    !  is essentially handled by a user who is an owner of those inserted objects. <br>
    !  As a symbol table, the *IntrusiveHashTree* type does not allow duplicated keys;
    !  therefore, if an inserted key node has a key equal to that of a key node stored
    !  in the table, the stored key node is replaced by the inserted key node.  <br>
    !  In order to avoid large balanced trees (due to many keys hashed to the same indices),
    !  the *IntrusiveHashTree* type employs the dynamic-array technique to resize the
    !  balanced-tree array.  As a result, the *IntrusiveHashTree* type requires an explicit
    !  construction via the *Construct* method before using other provided operations.
    !  When the *Destruct* method is called, the hash table also needs to be re-construct
    !  because this method essentially deallocates the working array.  <br>
    TYPE :: IntrusiveHashTree
        PRIVATE
        !% initial capacity of the hash table
        tIndex                              :: InitCap = 16_kIndex
        !% current size (number of stored nodes) of the hash table
        tIndex                              :: Size = 0_kIndex
        !% current capacity of the hash table
        tIndex                              :: Capacity = 0_kIndex
        !% working balanced tree
        TYPE(IntrusiveRBTree), ALLOCATABLE  :: WrkTree(:)
        !% current index into the working lists (used for iteration purpose)
        tIndex                              :: Indx = 0_kIndex
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Resize <br>
        !  **Purpose**:  To resize the table to the specified capacity. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Resize(64)
        PROCEDURE, PRIVATE  :: Resize           => HashTable_Resize
        !> To create an empty table; use the *Construct* method instead.
        PROCEDURE, PRIVATE  :: CreateEmpty      => HashTable_CreateEmpty
        !> To create a non-empty table; use the *Construct* method instead.
        PROCEDURE, PRIVATE  :: CreateByArray    => HashTable_CreateByArray
        ! ---------------------------------------------------------------------
        ! -----             Cloning Procedure                             -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CloneTo <br>
        !  **Purpose**:  To perform cloning of the source hash table. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcTab%CloneTo(DstTab) <br>
        PROCEDURE   :: CloneTo      => HashTable_Clone
        ! ---------------------------------------------------------------------
        ! -----         Constructor and Destructor Procedures             -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new empty table with default or specified capacity, or
        !                a new non-empty table from an array of key nodes. <br>
        !  **Usage**: <br>
        !   ! construct an empty table <br>
        !   --->    CALL Table%Construct()          ! use default initial capacity <br>
        !   --->    CALL Table%Construct(InitCap)   ! specify initial capacity <br>
        !   ! construct a non-empty table from an array of 25 key nodes <br>
        !   --->    CALL Table%Construct(25, KeyNodes) <br>
        GENERIC     :: Construct    => CreateEmpty, CreateByArray
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all key nodes from the table, free memory of the allocatable
        !                component, and reset all other components.<br>
        !  **Usage**: <br>
        !   --->    CALL Table%Destruct() <br>
        !  **Note**:  This method is NOT equivalent to the *Clear* method where the allocatable
        !             component is NOT deallocated.
        PROCEDURE   :: Destruct     => HashTable_Destroy
        ! ---------------------------------------------------------------------
        ! -----             Adding and Removing procedures                -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key node into the table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Insert(KeyNode) <br>
        PROCEDURE   :: Insert       => HashTable_Insert
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove a stored key node that has the key component equal to that
        !                of the specified key node.  Also, return a flag indicating whether
        !                a stored key node is successfully removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%Remove(KeyNode) <br>
        !   --->    IF (.NOT.Table%Remove(KeyNode)) DoSomething
        PROCEDURE   :: Remove       => HashTable_Remove
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete the current node from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !   --->    CALL Collection%Delete(DelNode) <br>
        !  **Note**: This method is intended to be used in conjunction with the
        !  *StartFirst* and *MoveForward* methods.  Therefore, after the call to
        !  one of those methods and then calling this one will result in a removal
        !  of the current node of the iteration.
        PROCEDURE   :: Delete       => HashTable_Delete
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all stored key nodes from the table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Clear()
        PROCEDURE   :: Clear        => HashTable_Clear
        ! ---------------------------------------------------------------------
        ! -----                 Iteration Procedures                      -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag
        !                indicating whether the table is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Table%StartFirst() <br>
        !   --->    IsEmpty = Table%StartFirst(FirstNode)
        PROCEDURE   :: StartFirst   => HashTable_Move2FirstNode
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move *forward* to the next iteration and return a flag
        !                indicating whether the cursor has reached the end of the
        !                table or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Table%MoveForward() <br>
        !   --->    IsTheEnd = Table%MoveForward(NextNode) <br>
        PROCEDURE   :: MoveForward  => HashTable_Move2NextNode
        ! ---------------------------------------------------------------------
        ! -----               Inquiry Procedures                         ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get size of the table (number of total nodes stored). <br>
        !  **Usage**: <br>
        !   --->    TableSize = Table%GetSize()
        PROCEDURE   :: GetSize      => HashTable_GetSize
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the table is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%IsEmpty() <br>
        !   --->    IF (.NOT.Table%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty      => HashTable_IsEmpty
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find a stored key node that has the key component equal to
        !                that of specified key node.  Return a flag indicating whether
        !                such node is found or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%Contain(KeyNode) <br>
        !   --->    IF (.NOT.Table%Contain(KeyNode)) DoSomething
        PROCEDURE   :: Contain      => HashTable_Contain
        !> **Type-Bound Function**: FindNode <br>
        !  **Purpose**:  To find the specified key node in the table.  Return true if
        !                the specified key node is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%FindNode(KeyNode, StoredNode) <br>
        !   --->    IF (.NOT.Table%FindNode(KeyNode)) DoSomething
        PROCEDURE   :: FindNode     => HashTable_FindNode
        !> **Type-Bound Function**: GetCursor <br>
        !  **Purpose**:  To get a pointer to the current node of the iteration. <br>
        !  **Usage**: <br>
        !   --->    CursorNode => Table%GetCursor()
        PROCEDURE   :: GetCursor    => HashTable_GetCursor
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: HashTable_Finalize
        ! ---------------------------------------------------------------------
    END TYPE IntrusiveHashTree

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Routines for HashTreeNode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE HashTreeNode_Reset(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !! To reset components of the node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTreeNode), INTENT(INOUT)  :: Node     !! current node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL Node%ResetBSTNode()
    Node%KeyHash = 0_kIndex

    RETURN

END SUBROUTINE HashTreeNode_Reset

!******************************************************************************

SUBROUTINE HashTreeNode_CopyHashNode(SrcNode, DstNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy members of the source to the destination.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTreeNode), INTENT(IN)     :: SrcNode  !! source
    CLASS(HashTreeNode), INTENT(INOUT)  :: DstNode  !! destination

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL SrcNode%CopyBSTNode(DstNode)
    DstNode%KeyHash = SrcNode%KeyHash
       
    RETURN

END SUBROUTINE HashTreeNode_CopyHashNode

!******************************************************************************

FUNCTION HashTreeNode_EqualTo(LhsNode, RhsNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether all members of both objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTreeNode), INTENT(IN) :: LhsNode  !! a node
    CLASS(HashTreeNode), INTENT(IN) :: RhsNode  !! another node
    tLogical                        :: Flag     !! true if both nodes are equal to one another

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! set default value
    Flag = FalseVal
    IF (LhsNode%KeyHash /= RhsNode%KeyHash) RETURN
    Flag = LhsNode%IsBSTNodeEqual(RhsNode)
       
    RETURN

END FUNCTION HashTreeNode_EqualTo

! ---------------------------------------------------------------------
! -----             Cloning Procedure                             -----
! ---------------------------------------------------------------------

SUBROUTINE HashTable_Clone(SrcTab, DstTab)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform cloning of the source list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveHashTree), INTENT(IN)    :: SrcTab   !! source
    CLASS(IntrusiveHashTree), INTENT(OUT)   :: DstTab   !! destination

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I

! FLOW

    DstTab%InitCap  = SrcTab%InitCap
    DstTab%Size     = SrcTab%Size
    DstTab%Capacity = SrcTab%Capacity
    DstTab%Indx     = SrcTab%Indx
    ALLOCATE(DstTab%WrkTree(SIZE(SrcTab%WrkTree)))
    DO I = 1_kIndex, SIZE(SrcTab%WrkTree)
        IF (.NOT.SrcTab%WrkTree(I)%IsEmpty()) CALL SrcTab%WrkTree(I)%CloneTo(DstTab%WrkTree(I))
    END DO

    RETURN

END SUBROUTINE HashTable_Clone

! ---------------------------------------------------------------------
! -----         Constructor and Destructor Procedures             -----
! ---------------------------------------------------------------------

SUBROUTINE HashTable_CreateEmpty(Table, InitCap)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To create an empty hash table with a *power of 2* capacity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table object
    CLASS(IntrusiveHashTree), INTENT(INOUT) :: Table
    !% initial capacity of the hash table (must be a power of 2)
    tIndex, OPTIONAL,         INTENT(IN)    :: InitCap

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (PRESENT(InitCap)) THEN
        IF (InitCap < 4_kIndex) THEN
            Table%InitCap = 4_kIndex
        ELSE
            Table%InitCap = PowerOfTwo(InitCap)
        END IF
    END IF
    Table%Capacity = Table%InitCap
    ALLOCATE(Table%WrkTree(0:Table%Capacity-1_kIndex))

    RETURN

CONTAINS

    FUNCTION PowerOfTwo(M) RESULT(N)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To return the smallest power of two greater than or equal to M.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: M    ! specified input
        tIndex              :: N    ! smallest power of two

    !** SUBROUTINE PARAMETER DECLARATIONS:
#ifdef Indx32Bits
        tSInt32, PARAMETER :: MaxN = SHIFTL(1_kInt32, 30)
#else
        tSInt64,    PARAMETER :: MaxN = SHIFTL(1_kInt64, 62)
#endif

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        N = SHIFTR(-1_kIndex, LEADZ(M-1_kIndex))
        IF (N >= 0) THEN
            IF (N < MaxN) THEN
                N = N + 1
            ELSE
                N = MaxN
            END IF
        ELSE
            N = 1
        END IF

        RETURN

    END FUNCTION PowerOfTwo

    !**************************************************************************

END SUBROUTINE HashTable_CreateEmpty

!******************************************************************************

SUBROUTINE HashTable_CreateByArray(Table, N, KeyNodes)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a table from an array of key nodes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table object
    CLASS(IntrusiveHashTree), INTENT(INOUT) :: Table
    !% number of key nodes
    tIndex,                   INTENT(IN)    :: N
    !% the key nodes to be added to the table
    CLASS(HashTreeNode),      INTENT(INOUT) :: KeyNodes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! create empty symbol table with capacity twice of the key size
    CALL Table%Construct(N*2_kIndex)

    ! add key-value pairs to the table
    DO I = 1_kIndex, N
        CALL Table%Insert(KeyNodes(I))
    END DO

    RETURN

END SUBROUTINE HashTable_CreateByArray

!******************************************************************************

SUBROUTINE HashTable_Destroy(Table)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the table. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table object
    CLASS(IntrusiveHashTree), INTENT(INOUT) :: Table

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! remove all stored objects
    CALL Table%Clear()

    ! set all components to initial states
    Table%InitCap = 16_kIndex
    Table%Size = 0_kIndex
    Table%Capacity = 0_kIndex
    Table%Indx = 0_kIndex
    DEALLOCATE(Table%WrkTree)

    RETURN

END SUBROUTINE HashTable_Destroy

! ---------------------------------------------------------------------
! -----             Insert and Remove Procedures                  -----
! ---------------------------------------------------------------------

SUBROUTINE HashTable_Insert(Table, KeyNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a key node into a symbol table.  If the specified key node has the key component
    !  equal to that of a key node stored in the table, replace the stored key node by the
    !  specified one.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table object
    CLASS(IntrusiveHashTree), INTENT(INOUT) :: Table
    !% the key node to be added to the table
    CLASS(HashTreeNode),      INTENT(INOUT) :: KeyNode

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical                        :: KeyFound
    CLASS(HashTreeNode), POINTER    :: StoredNode
    tHash                           :: HashCode
    tIndex                          :: Indx
    tLogical                        :: Success

! FLOW

    ! check if the table has been created yet
    IF (.NOT.ALLOCATED(Table%WrkTree)) CALL Table%CreateEmpty()

    ! initialize
    StoredNode => NULL()

    ! check if the table is empty or not
    IF (Table%IsEmpty()) THEN
        ! no key to be found
        KeyFound = FalseVal
        ! compute hash code
        HashCode = KeyNode%HashCode()
        ! compute the index
        Indx = ComputeIndex(HashCode, Table%Capacity)
    ELSE
        ! find a stored key node where its key component is equal to that of the specified key node
        ! also, get the stored node and compute key's hash code and index
        KeyFound = Table%FindNode(KeyNode, StoredNode, HashCode, Indx)
    END IF

    IF (KeyFound.AND.ASSOCIATED(StoredNode)) THEN
        ! key of the new key node is equal to key of a stored one so
        ! replace the stored key node with the specified one.
        Success = Table%WrkTree(Indx)%Remove(StoredNode)
        CALL Table%WrkTree(Indx)%Insert(KeyNode)
    ELSE
        ! double the hash table capacity if average size of the trees >= 128
        IF (Table%Size >= 128_kIndex*Table%Capacity) THEN
            CALL Table%Resize(2_kIndex*Table%Capacity)
        END IF
        ! store the computed hash code in the node
        KeyNode%KeyHash = HashCode
        ! append the new node
        CALL Table%WrkTree(Indx)%Insert(KeyNode)
        ! update table size
        Table%Size = Table%Size + 1_kIndex
    END IF

    ! free working pointer
    NULLIFY(StoredNode)

    RETURN

END SUBROUTINE HashTable_Insert

!******************************************************************************

FUNCTION HashTable_Remove(Table, KeyNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove a stored key node that has the key component equal to that
    !  of the specified key node.  Also, return a flag indicating whether
    !  a stored key node is successfully removed or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table object
    CLASS(IntrusiveHashTree), INTENT(INOUT) :: Table
    !% the key node where its key is the same as that of a stored node to be removed
    CLASS(HashTreeNode),      INTENT(IN)    :: KeyNode
    !> flag indicating whether a stored key node is successfully removed or not.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(HashTreeNode), POINTER    :: StoredNode
    tIndex                          :: Indx

! FLOW

    ! check whether the key is stored in the table or not
    IF (Table%IsEmpty()) THEN
        Flag = FalseVal
    ELSE
        Flag = Table%FindNode(KeyNode, StoredNode, TabIndx=Indx)
    END IF

    IF (Flag) THEN
        ! check if remove the node successfully or not
        Flag = Table%WrkTree(Indx)%Remove(StoredNode)
        IF (Flag) THEN
            ! update table size
            Table%Size = Table%Size - 1_kIndex
            ! halve the hash table capacity if average size of the lists <= 4
            IF ((Table%Capacity > Table%InitCap).AND.(Table%Size <= 4_kIndex*Table%Capacity)) THEN
                CALL Table%Resize(Table%Capacity/2_kIndex)
            END IF
        END IF
    END IF

    ! free working pointer
    NULLIFY(StoredNode)

    RETURN

END FUNCTION HashTable_Remove

!******************************************************************************

SUBROUTINE HashTable_Delete(Table, DelNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove a stored key node at the current iteration.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table object
    CLASS(IntrusiveHashTree),               INTENT(INOUT)   :: Table
    CLASS(HashTreeNode), POINTER, OPTIONAL, INTENT(OUT)     :: DelNode

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: StoredNode
    tLogical                :: Success

! FLOW

    ! check whether we are iterating; if not simply return
    IF ((Table%Indx < 1_kIndex).OR.(Table%Indx > SIZE(Table%WrkTree))) RETURN

    IF (.NOT.Table%WrkTree(Table%Indx)%IsEmpty()) THEN
        StoredNode => Table%WrkTree(Table%Indx)%GetCursor()
        Success = Table%WrkTree(Table%Indx)%Remove(StoredNode)
    ELSE
        Success = FalseVal
    END IF
    IF (Success) THEN
        ! update table size
        Table%Size = Table%Size - 1_kIndex
        ! halve the hash table capacity if average size of the lists <= 2
        IF ((Table%Capacity > Table%InitCap).AND.(Table%Size <= 2_kIndex*Table%Capacity)) THEN
            CALL Table%Resize(Table%Capacity/2_kIndex)
        END IF
        IF (PRESENT(DelNode)) THEN
            SELECT TYPE (StoredNode)
            CLASS IS (HashTreeNode)
                DelNode => StoredNode
            END SELECT
        END IF
    END IF

    ! free working pointer
    NULLIFY(StoredNode)

    RETURN

END SUBROUTINE HashTable_Delete

!******************************************************************************

SUBROUTINE HashTable_Clear(Table)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the stored key nodes from the table. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table object
    CLASS(IntrusiveHashTree), INTENT(INOUT) :: Table

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! simply return if the table is empty
    IF (Table%IsEmpty()) RETURN

    ! loop over all working lists
    DO I = 0_kIndex, Table%Capacity-1_kIndex
        ! clear each work list
        CALL Table%WrkTree(I)%Clear()
    END DO

    RETURN

END SUBROUTINE HashTable_Clear

! ---------------------------------------------------------------------
! -----                 Iteration Procedures                      -----
! ---------------------------------------------------------------------

FUNCTION HashTable_Move2FirstNode(Table, KeyNode) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the first (starting) key node in a symbol table.  For this hash table,
    !  which is an unordered symbol table, the starting key node is the first key node
    !  inserted into the first non-empty work list (the first work list is the list with
    !  index = 0) .  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table object
    CLASS(IntrusiveHashTree),                INTENT(INOUT)  :: Table
    !% the first key node as output if requested (and available)
    CLASS(HashTreeNode), OPTIONAL,  POINTER, INTENT(OUT)    :: KeyNode
    !> a flag indicating whether the table contains no key node or not <br>
    ! - true if the table is empty. <br>
    ! - otherwise the first key node is available.
    tLogical                                                :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Table%IsEmpty()) THEN
        IsEmpty = TrueVal
        RETURN
    ELSE
        IsEmpty = FalseVal
    END IF

    Table%Indx = 0_kIndex
    IF (PRESENT(KeyNode)) THEN
        BLOCK
            ! block variable
            CLASS(BSTNode), POINTER   :: CurrNode
            ! start iteration by looking for the first non-empty working list
            DO WHILE (Table%WrkTree(Table%Indx)%StartMin(CurrNode))
                Table%Indx = Table%Indx + 1_kIndex
            END DO
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                CLASS IS (HashTreeNode)
                    IF (PRESENT(KeyNode)) THEN
                        KeyNode => CurrNode
                    END IF
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration by looking for the first non-empty working list
        DO WHILE (Table%WrkTree(Table%Indx)%StartMin())
            Table%Indx = Table%Indx + 1_kIndex
        END DO
    END IF

    RETURN

END FUNCTION HashTable_Move2FirstNode

!******************************************************************************

FUNCTION HashTable_Move2NextNode(Table, KeyNode) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next key node in a symbol table.  For this hash table,
    !  which is an unordered symbol table, the key node is the node inserted
    !  after the previous one if it is in the same list.  Or it is the first
    !  node in non-empty next list. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table object
    CLASS(IntrusiveHashTree),                INTENT(INOUT)  :: Table
    !% the next key node as output if requested (and available)
    CLASS(HashTreeNode), OPTIONAL,  POINTER, INTENT(OUT)    :: KeyNode
    !> a flag indicating whether the move to the end of the table occurs or not <br>
    ! - true if next node is NOT available. <br>
    ! - otherwise next node is available.
    tLogical                                                :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Table%IsEmpty()) THEN
        IsTheEnd = TrueVal
        RETURN
    END IF

    IF (PRESENT(KeyNode)) THEN
        BLOCK
            ! block variable
            CLASS(BSTNode), POINTER   :: CurrNode
            ! move to next iteration
            IF (Table%Indx == (Table%Capacity-1_kIndex)) THEN
                ! the last working list
                IsTheEnd = Table%WrkTree(Table%Indx)%MoveForward(CurrNode)
            ELSE
                ! check if the end of the current working list
                IF (Table%WrkTree(Table%Indx)%MoveForward(CurrNode)) THEN
                    ! move to next working list
                    Table%Indx = Table%Indx + 1_kIndex
                    IF (Table%Indx == (Table%Capacity-1_kIndex)) THEN
                        ! the last working list
                        IsTheEnd = Table%WrkTree(Table%Indx)%StartMin(CurrNode)
                    ELSE
                        ! look for the next non-empty working list
                        IsTheEnd = FalseVal
                        DO WHILE (Table%WrkTree(Table%Indx)%StartMin(CurrNode))
                            Table%Indx = Table%Indx + 1_kIndex
                            IF (Table%Indx == (Table%Capacity-1_kIndex)) THEN
                                ! the last working list
                                IsTheEnd = Table%WrkTree(Table%Indx)%StartMin(CurrNode)
                                EXIT
                            END IF
                        END DO
                    END IF
                ELSE
                    IsTheEnd = FalseVal
                END IF
            END IF
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                CLASS IS (HashTreeNode)
                    IF (PRESENT(KeyNode)) THEN
                        KeyNode => CurrNode
                    END IF
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! move to next iteration
        IF (Table%Indx == (Table%Capacity-1_kIndex)) THEN
            ! the last working list
            IsTheEnd = Table%WrkTree(Table%Indx)%MoveForward()
        ELSE
            ! check if the end of the current working list
            IF (Table%WrkTree(Table%Indx)%MoveForward()) THEN
                ! move to next working list
                Table%Indx = Table%Indx + 1_kIndex
                IF (Table%Indx == (Table%Capacity-1_kIndex)) THEN
                    ! the last working list
                    IsTheEnd = Table%WrkTree(Table%Indx)%StartMin()
                ELSE
                    ! look for the next non-empty working list
                    IsTheEnd = FalseVal
                    DO WHILE (Table%WrkTree(Table%Indx)%StartMin())
                        Table%Indx = Table%Indx + 1_kIndex
                        IF (Table%Indx == (Table%Capacity-1_kIndex)) THEN
                            ! the last working list
                            IsTheEnd = Table%WrkTree(Table%Indx)%StartMin()
                            EXIT
                        END IF
                    END DO
                END IF
            ELSE
                IsTheEnd = FalseVal
            END IF
        END IF
    END IF

    RETURN

END FUNCTION HashTable_Move2NextNode

! ---------------------------------------------------------------------
! -----                     Inquiry Procedures                    -----
! ---------------------------------------------------------------------

FUNCTION HashTable_IsEmpty(Table) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the table is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveHashTree), INTENT(IN)    :: Table    !! symbol table object
    tLogical                                :: Flag     !! true if the table is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = (Table%Size == 0_kIndex)

    RETURN

END FUNCTION HashTable_IsEmpty

!******************************************************************************

FUNCTION HashTable_GetSize(Table) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get size of the table (a number of total stored nodes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveHashTree), INTENT(IN)    :: Table    !! symbol table object
    tIndex                                  :: Size     !! current size (number of nodes stored)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Table%Size

    RETURN

END FUNCTION HashTable_GetSize

!******************************************************************************

FUNCTION HashTable_Contain(Table, KeyNode) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find a stored key node that has the key component equal to that of
    !  specified key node.  Return a flag indicating whether such node is found
    !  or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table object
    CLASS(IntrusiveHashTree), INTENT(INOUT) :: Table
    !% the key node
    CLASS(HashTreeNode),      INTENT(IN)    :: KeyNode
    !> flag indicating whether a stored key node with key equal to that of the
    !  specified key node exists in the table or not.
    tLogical                                :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Table%IsEmpty()) THEN
        Found = FalseVal
    ELSE
        Found = Table%FindNode(KeyNode)
    END IF

    RETURN

END FUNCTION HashTable_Contain

! ---------------------------------------------------------------------
! -----                     Private Procedures                    -----
! ---------------------------------------------------------------------

SUBROUTINE HashTable_Resize(Table, NewCap)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To resize the capacity of the hash table to the *NewCap* value.  Also, redistribute
    !  the stored objects to new working lists according to their new indices. <br>
    !  Unlike conventional implementation, the key's hash code is only computed once and
    !  stored in the object.  When resizing, an index into the working lists where the
    !  object will be stored is re-computed based on the stored hash code and new capacity
    !  of the symbol table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol table object
    CLASS(IntrusiveHashTree), INTENT(INOUT) :: Table
    !% new capacity of the hash table
    tIndex,                   INTENT(IN)    :: NewCap

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(IntrusiveRBTree), TARGET, ALLOCATABLE  :: TmpTree(:)    ! working variable
    CLASS(BSTNode),        POINTER              :: CurrNode
    tIndex                                      :: OldCap, I, J

!** FLOW:

    ! move currently stored objects to temporary lists
    OldCap = Table%Capacity
    CALL MOVE_ALLOC(Table%WrkTree, TmpTree)

    ! allocate working lists to new capacity
    Table%Capacity = NewCap
    ALLOCATE(Table%WrkTree(0:Table%Capacity-1_kIndex))

    ! loop over the temporary lists to move stored objects (nodes)
    ! back to the working lists of the hash table
    DO I = 0_kIndex, OldCap-1_kIndex
        ! remove nodes from the current temporary list
        DO WHILE (.NOT.TmpTree(I)%IsEmpty())
            IF (.NOT.TmpTree(I)%RemoveMin(CurrNode)) EXIT
            ! recompute the index into the working lists
            SELECT TYPE (CurrNode)
            CLASS IS (HashTreeNode)
                J = ComputeIndex(CurrNode%KeyHash, Table%Capacity)
            END SELECT
            ! add the current node to a working list
            CALL Table%WrkTree(J)%Insert(CurrNode)
        END DO
    END DO

    ! free working variables
    NULLIFY(CurrNode)
    DEALLOCATE(TmpTree)

    RETURN

END SUBROUTINE HashTable_Resize

!******************************************************************************

FUNCTION HashTable_FindNode(Table, KeyNode, StoredNode, KeyHash, TabIndx) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the specified KeyNode in a symbol table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table object
    CLASS(IntrusiveHashTree),               INTENT(INOUT)   :: Table
    !% the key node to be looked for in the table
    CLASS(HashTreeNode),                    INTENT(IN)      :: KeyNode
    !> the stored node where the key is equal to that of the specified key node;
    !  null pointer if the key is not found
    CLASS(HashTreeNode), OPTIONAL, POINTER, INTENT(OUT)     :: StoredNode
    !% hash code of the specified key node
    tHash,               OPTIONAL,          INTENT(OUT)     :: KeyHash
    !% index of the working list
    tIndex,              OPTIONAL,          INTENT(OUT)     :: TabIndx
    !% flag indicating whether the specified key is found or not.
    tLogical                                                :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER    :: CurrNode
    tLogical                            :: IsTheEnd
    tHash                               :: HashCode
    tIndex                              :: Indx

! FLOW

    ! initialize
    Found = FalseVal
    IF (PRESENT(StoredNode)) StoredNode => NULL()
    IF (PRESENT(KeyHash))    KeyHash = 0_kIndex
    IF (PRESENT(TabIndx))    TabIndx = -1_kIndex

    ! compute the hash code of the specified key node
    HashCode = KeyNode%HashCode()

    ! compute index into the working lists
    Indx = ComputeIndex(HashCode, Table%Capacity)

    ! get optional output
    IF (PRESENT(KeyHash)) KeyHash = HashCode
    IF (PRESENT(TabIndx)) TabIndx = Indx

    IsTheEnd = Table%WrkTree(Indx)%StartMin(CurrNode)

    ! iterate over the table to find key
    DO WHILE (.NOT.IsTheEnd)
        SELECT TYPE (CurrNode)
        CLASS IS (HashTreeNode)
            IF (CurrNode == KeyNode) THEN
                ! key found
                Found = TrueVal
                IF (PRESENT(StoredNode)) StoredNode => CurrNode
                EXIT
            END IF
        END SELECT
        IsTheEnd = Table%WrkTree(Indx)%MoveForward(CurrNode)
    END DO

    ! free working pointer
    NULLIFY(CurrNode)

    RETURN

END FUNCTION HashTable_FindNode

!******************************************************************************

FUNCTION HashTable_GetCursor(Table) RESULT(Cursor)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get a pointer to the cursor node of the current tree of the hash table

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveHashTree), INTENT(IN)    :: Table    !! hash table
    CLASS(HashTreeNode),      POINTER       :: Cursor   !! pointer to the cursor node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: StoredNode

! FLOW

    ! initialize
    Cursor => NULL()

    ! check whether we are iterating; if not simply return
    IF ((Table%Indx < 1_kIndex).OR.(Table%Indx > SIZE(Table%WrkTree))) RETURN

    StoredNode => Table%WrkTree(Table%Indx)%GetCursor()
    SELECT TYPE (StoredNode)
    CLASS IS (HashTreeNode)
        Cursor => StoredNode
    END SELECT

    RETURN

END FUNCTION HashTable_GetCursor

! ---------------------------------------------------------------------
! -----                 Auxiliary Procedures                      -----
! ---------------------------------------------------------------------

FUNCTION ComputeIndex(HashCode, Capacity) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the index of working lists of the hash table for
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

! ---------------------------------------------------------------------
! -----             Final Procedures                              -----
! ---------------------------------------------------------------------

SUBROUTINE HashTable_Finalize(Table)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(IntrusiveHashTree), INTENT(INOUT)  :: Table

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Table%Destruct()

    RETURN

END SUBROUTINE HashTable_Finalize

!******************************************************************************

END MODULE MClass_IntrusiveHashTree

!******************************************************************************
