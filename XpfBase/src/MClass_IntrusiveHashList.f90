
MODULE MClass_IntrusiveHashList

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *IntrusiveHashList* type, the *HashListNode* type and
!   their related routines.  The *IntrusiveHashList* type is a container type that
!   employs a separate-chaining hash table implementation to provide common operations
!   for an unordered symbol table while the *HashListNode* type is a node type intended
!   to be used in conjunction with the *IntrusiveHashList* type. <br>
!   As an intrusive container, the *IntrusiveHashList* type requires a user to define
!   a new node type that extends the *HashListNode* type, which is a subtype of the
!   <a href="../module/MClass_IntrusiveLinkedLists.html#type-doublylinkednode">DoublyLinkedNode</a>
!   type.  The new node type must contain a key (or more than one) as its additional
!   component(s).  The new node type may contain one or more associated values as its
!   other component(s).  The user is also required to implement all procedures deferred
!   by the *HashListNode* type.  In particular, the *IsKeyEqual* and *ComputeHashValue*
!   deferred procedures should be dependent on the key component(s) only. <br>
!   Unlike intrusive list-based and tree-based types, which can be used instantly
!   by inserting objects into a container, the *IntrusiveHashList* type requires an
!   explicit construction via the *Construct* method before using other provided
!   operations. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE ISO_C_BINDING,                  ONLY: C_SIZEOF, C_LOC, C_NULL_PTR, C_PTR, C_F_POINTER
    USE, INTRINSIC  :: ISO_FORTRAN_ENV, ONLY: CHARACTER_STORAGE_SIZE
    USE MBase_SIntUtil,                 ONLY: ToChar => ToDecStrSigned
    USE MClass_IntrusiveLinkedLists,    ONLY: DoublyLinkedNode, IntrusiveLinearList
    USE MBase_ErrHandlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: HashListNode
    PUBLIC :: IntrusiveHashList

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define     tHash               tIndex

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_IntrusiveHashList'
    ! The maximum (positive) number of hash code
#ifdef Indx32Bits
    tSInt32,   PARAMETER    :: MaxHash = ToInt32(Z'7FFFFFFF')
#else
    tSInt64,   PARAMETER    :: MaxHash = ToInt64(Z'7FFFFFFFFFFFFFFF')
#endif

!** DERIVED TYPE DEFINITIONS
    !> The *HashListNode* type is a doubly-linked-list node type intended to be
    !  used in conjunction with the *IntrusiveHashList* type.  Similar to other
    !  intrusive containers, the *IntrusiveHashList* type requires a user to
    !  define a new (concrete) node type that extends the *HashListNode* type
    !  where the new node type contains a key and its associated value as its
    !  components. <br>
    !  The following code snippet illustrates how to define a new node type.
    !   <Pre><Code style="color:MidnightBlue;">
    !   TYPE, EXTENDS(HashListNode) :: AssociativeNode
    !       CHARACTER(LEN=:), ALLOCATABLE   :: Key      ! stored string key
    !       INTEGER                         :: Value1   ! stored integer value
    !       REAL                            :: Value2   ! stored real value
    !   END TYPE
    !   </Code></Pre>
    !  The *HashListNode* type is a subtype of the *DoublyLinkedNode* type and it defines
    !  one additional method (the *IsKeyEqual* method) required by the *IntrusiveHashList*
    !  type.  Therefore, a user must implement this deferred procedure in addition to other
    !  deferred procedures required by all its parent types. <br>
    !  It is necessary to note that the implementations of the *IsEqualTo* method required by
    !  an Object class and the *IsKeyEqual* method do not needed to be the same because the
    !  *IsKeyEqual* method should rely only on the key component whereas the *IsEqualTo* method
    !  should rely on all components of the object. <br>
    TYPE, ABSTRACT, EXTENDS(DoublyLinkedNode) :: HashListNode
        PRIVATE
        !% hash code of the stored key
        tHash       :: KeyHash = 0_kIndex
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: IsKeyEqual <br>
        !  **Purpose**:  To check whether the key components of the specified objects are equal or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = LhsObj%IsKeyEqual(RhsObj) <br>
        !   --->    IF (.NOT.LhsObj%IsKeyEqual(RhsObj)) DoSomething <br>
        PROCEDURE(IfaceKeyEqual), DEFERRED  :: IsKeyEqual
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: ResetHashNode <br>
        !  **Purpose**:  To reset the node to its initialized state. <br>
        !  **Usage**: <br>
        !   --->    CALL Node%ResetHashNode()
        PROCEDURE   :: ResetHashNode    => HashListNode_Reset
        !> **Type-Bound Subroutine**: CopyHashNode <br>
        !  **Purpose**:  To copy members of the source to the destination.  This method
        !                is provided to help user implement deferred procedure(s) required
        !                by an object in the *Object* class. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcNode%CopyHashNode(DstNode)
        PROCEDURE   :: CopyHashNode     => HashListNode_CopyHashNode
        !> **Type-Bound Function**: EqualTo <br>
        !  **Purpose**:  To compare whether all members of both objects are equal or not.
        !                This method is provided to help user implement deferred procedure(s)
        !                required by an object in the *Object* class. <br>
        !  **Usage**: <br>
        !   --->    Flag = LhsNode%EqualTo(RhsNode) <br>
        !   --->    IF (.NOT.LhsNode%EqualTo(RhsNode)) DoSomething
        PROCEDURE   :: IsHashNodeEqual  => HashListNode_EqualTo
        ! ---------------------------------------------------------------------
    END TYPE HashListNode
    !> The *IntrusiveHashList* type is an intrusive container type that employs a
    !  separate-chaining hash table implementation to provide common operations for
    !  an unordered symbol table.  The separate-chaining technique combines a linked
    !  list with a hash table to solve the collision problem.  <br>
    !  The *IntrusiveHashList* type employs an array of the *IntrusiveLinearList* type
    !  as its component (i.e. using an array of linked lists as its buckets) to store
    !  *HashListNode* objects.  Although the *IntrusiveHashList* type handles a memory
    !  management of its working lists, these working lists (as intrusive containers)
    !  only provide a linking/unlinking task for common linked-list operations.  This
    !  means that the memory management task of the inserted *HashListNode* objects
    !  is essentially handled by a user who is an owner of those inserted objects. <br>
    !  As a symbol table, the *IntrusiveHashList* type does not allow duplicated keys;
    !  therefore, if an inserted key node has a key equal to that of a key node stored
    !  in the table, the stored key node is replaced by the inserted key node.  <br>
    !  In order to avoid long linked lists (due to many keys hashed to the same indices),
    !  the *IntrusiveHashList* type employs the dynamic-array technique to resize the
    !  linked-list array.  As a result, the *IntrusiveHashList* type requires an explicit
    !  construction via the *Construct* method before using other provided operations.
    !  When the *Destruct* method is called, the hash table also needs to be re-construct
    !  because this method essentially deallocates the working array.  <br>
    TYPE :: IntrusiveHashList
        PRIVATE
        !% initial capacity of the hash table
        tIndex                                  :: InitCap = 16_kIndex
        !% current size (number of stored nodes) of the hash table
        tIndex                                  :: Size = 0_kIndex
        !% current capacity of the hash table
        tIndex                                  :: Capacity = 0_kIndex
        !% working doubly-linked lists
        TYPE(IntrusiveLinearList), ALLOCATABLE  :: WrkLst(:)
        !% current index into the working lists (used for iteration purpose)
        tIndex                                  :: Indx = 0_kIndex
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
    END TYPE IntrusiveHashList

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> *IfaceKeyEqual* is an interface for a procedure to check whether *key* components of
        !  the two specified objects are equal to one another or not.
        FUNCTION IfaceKeyEqual(LhsObj, RhsObj) RESULT(Flag)
            IMPORT
            CLASS(HashListNode), INTENT(INOUT)  :: LhsObj   !! an object
            CLASS(HashListNode), INTENT(INOUT)  :: RhsObj   !! another object
            tLogical                            :: Flag     !! true if keys of both objects are equal
        END FUNCTION IfaceKeyEqual
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Routines for HashListNode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE HashListNode_Reset(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !! To reset components of the node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashListNode), INTENT(INOUT)  :: Node     !! current node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL Node%FreePointers()
    Node%KeyHash = 0_kIndex

    RETURN

END SUBROUTINE HashListNode_Reset

!******************************************************************************

SUBROUTINE HashListNode_CopyHashNode(SrcNode, DstNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy members of the source to the destination.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashListNode), INTENT(IN)     :: SrcNode  !! source
    CLASS(HashListNode), INTENT(INOUT)  :: DstNode  !! destination

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL SrcNode%CopyDLLNode(DstNode)
    DstNode%KeyHash = SrcNode%KeyHash
       
    RETURN

END SUBROUTINE HashListNode_CopyHashNode

!******************************************************************************

FUNCTION HashListNode_EqualTo(LhsNode, RhsNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether all members of both objects are equal or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashListNode), INTENT(IN) :: LhsNode  !! a node
    CLASS(HashListNode), INTENT(IN) :: RhsNode  !! another node
    tLogical                        :: Flag     !! true if both nodes are equal to one another

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! set default value
    Flag = FalseVal
    IF (LhsNode%KeyHash /= RhsNode%KeyHash) RETURN
    Flag = LhsNode%IsDLLNodeEqual(RhsNode)
       
    RETURN

END FUNCTION HashListNode_EqualTo

! ---------------------------------------------------------------------
! -----             Cloning Procedure                             -----
! ---------------------------------------------------------------------

SUBROUTINE HashTable_Clone(SrcTab, DstTab)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform cloning of the source list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveHashList), INTENT(IN)    :: SrcTab   !! source
    CLASS(IntrusiveHashList), INTENT(OUT)   :: DstTab   !! destination

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I

! FLOW

    DstTab%InitCap  = SrcTab%InitCap
    DstTab%Size     = SrcTab%Size
    DstTab%Capacity = SrcTab%Capacity
    DstTab%Indx     = SrcTab%Indx
    ALLOCATE(DstTab%WrkLst(SIZE(SrcTab%WrkLst)))
    DO I = 1_kIndex, SIZE(SrcTab%WrkLst)
        IF (.NOT.SrcTab%WrkLst(I)%IsEmpty()) CALL SrcTab%WrkLst(I)%CloneTo(DstTab%WrkLst(I))
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
    CLASS(IntrusiveHashList), INTENT(INOUT) :: Table
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
    ALLOCATE(Table%WrkLst(0:Table%Capacity-1_kIndex))

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
        tSInt64, PARAMETER :: MaxN = SHIFTL(1_kInt64, 62)
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
    CLASS(IntrusiveHashList), INTENT(INOUT) :: Table
    !% number of key nodes
    tIndex,                   INTENT(IN)    :: N
    !% the key nodes to be added to the table
    CLASS(HashListNode),      INTENT(INOUT) :: KeyNodes(:)

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
    CLASS(IntrusiveHashList), INTENT(INOUT) :: Table

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
    DEALLOCATE(Table%WrkLst)

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
    CLASS(IntrusiveHashList), INTENT(INOUT) :: Table
    !% the key node to be added to the table
    CLASS(HashListNode),      INTENT(INOUT) :: KeyNode

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical                        :: KeyFound
    CLASS(HashListNode), POINTER    :: StoredNode
    tHash                           :: HashCode
    tIndex                          :: Indx
    tLogical                        :: Success

! FLOW

    ! check if the table has been created yet
    IF (.NOT.ALLOCATED(Table%WrkLst)) CALL Table%CreateEmpty()

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
        Success = Table%WrkLst(Indx)%ReplaceNode(StoredNode, KeyNode)
    ELSE
        ! double the hash table capacity if average size of the lists >= 16
        IF (Table%Size >= 16_kIndex*Table%Capacity) THEN
            CALL Table%Resize(2_kIndex*Table%Capacity)
        END IF
        ! store the computed hash code in the node
        KeyNode%KeyHash = HashCode
        ! append the new node
        CALL Table%WrkLst(Indx)%AddLast(KeyNode)
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
    CLASS(IntrusiveHashList), INTENT(INOUT) :: Table
    !% the key node where its key is the same as that of a stored node to be removed
    CLASS(HashListNode),      INTENT(INOUT) :: KeyNode
    !> flag indicating whether a stored key node is successfully removed or not.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(HashListNode), POINTER    :: StoredNode
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
        Flag = Table%WrkLst(Indx)%RemoveNode(StoredNode)
        IF (Flag) THEN
            ! update table size
            Table%Size = Table%Size - 1_kIndex
            ! halve the hash table capacity if average size of the lists <= 2
            IF ((Table%Capacity > Table%InitCap).AND.(Table%Size <= 2_kIndex*Table%Capacity)) THEN
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
    CLASS(IntrusiveHashList),               INTENT(INOUT)   :: Table
    CLASS(HashListNode), POINTER, OPTIONAL, INTENT(OUT)     :: DelNode

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: StoredNode
    tLogical                            :: Success

! FLOW
    
    IF (PRESENT(DelNode)) DelNode => NULL()

    ! check whether we are iterating; if not simply return
    IF ((Table%Indx < 1_kIndex).OR.(Table%Indx > SIZE(Table%WrkLst))) RETURN

    IF (.NOT.Table%WrkLst(Table%Indx)%IsEmpty()) THEN
        StoredNode => Table%WrkLst(Table%Indx)%GetCursor()
        Success = Table%WrkLst(Table%Indx)%RemoveNode(StoredNode)
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
            CLASS IS (HashListNode)
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
    CLASS(IntrusiveHashList), INTENT(INOUT) :: Table

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! simply return if the table is empty
    IF (Table%IsEmpty()) RETURN

    ! loop over all working lists
    DO I = 0_kIndex, Table%Capacity-1_kIndex
        ! clear each work list
        CALL Table%WrkLst(I)%Clear()
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
    CLASS(IntrusiveHashList),                INTENT(INOUT)  :: Table
    !% the first key node as output if requested (and available)
    CLASS(HashListNode), OPTIONAL,  POINTER, INTENT(OUT)    :: KeyNode
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
            CLASS(DoublyLinkedNode), POINTER   :: CurrNode
            ! start iteration by looking for the first non-empty working list
            DO WHILE (Table%WrkLst(Table%Indx)%StartFirst(CurrNode))
                Table%Indx = Table%Indx + 1_kIndex
            END DO
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                CLASS IS (HashListNode)
                    IF (PRESENT(KeyNode)) THEN
                        KeyNode => CurrNode
                    END IF
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration by looking for the first non-empty working list
        DO WHILE (Table%WrkLst(Table%Indx)%StartFirst())
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
    CLASS(IntrusiveHashList),                INTENT(INOUT)  :: Table
    !% the next key node as output if requested (and available)
    CLASS(HashListNode), OPTIONAL,  POINTER, INTENT(OUT)    :: KeyNode
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
            CLASS(DoublyLinkedNode), POINTER   :: CurrNode
            ! move to next iteration
            IF (Table%Indx == (Table%Capacity-1_kIndex)) THEN
                ! the last working list
                IsTheEnd = Table%WrkLst(Table%Indx)%MoveForward(CurrNode)
            ELSE
                ! check if the end of the current working list
                IF (Table%WrkLst(Table%Indx)%MoveForward(CurrNode)) THEN
                    ! move to next working list
                    Table%Indx = Table%Indx + 1_kIndex
                    IF (Table%Indx == (Table%Capacity-1_kIndex)) THEN
                        ! the last working list
                        IsTheEnd = Table%WrkLst(Table%Indx)%StartFirst(CurrNode)
                    ELSE
                        ! look for the next non-empty working list
                        IsTheEnd = FalseVal
                        DO WHILE (Table%WrkLst(Table%Indx)%StartFirst(CurrNode))
                            Table%Indx = Table%Indx + 1_kIndex
                            IF (Table%Indx == (Table%Capacity-1_kIndex)) THEN
                                ! the last working list
                                IsTheEnd = Table%WrkLst(Table%Indx)%StartFirst(CurrNode)
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
                CLASS IS (HashListNode)
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
            IsTheEnd = Table%WrkLst(Table%Indx)%MoveForward()
        ELSE
            ! check if the end of the current working list
            IF (Table%WrkLst(Table%Indx)%MoveForward()) THEN
                ! move to next working list
                Table%Indx = Table%Indx + 1_kIndex
                IF (Table%Indx == (Table%Capacity-1_kIndex)) THEN
                    ! the last working list
                    IsTheEnd = Table%WrkLst(Table%Indx)%StartFirst()
                ELSE
                    ! look for the next non-empty working list
                    IsTheEnd = FalseVal
                    DO WHILE (Table%WrkLst(Table%Indx)%StartFirst())
                        Table%Indx = Table%Indx + 1_kIndex
                        IF (Table%Indx == (Table%Capacity-1_kIndex)) THEN
                            ! the last working list
                            IsTheEnd = Table%WrkLst(Table%Indx)%StartFirst()
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
    CLASS(IntrusiveHashList), INTENT(IN)    :: Table    !! symbol table object
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
    CLASS(IntrusiveHashList), INTENT(IN)    :: Table    !! symbol table object
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
    CLASS(IntrusiveHashList), INTENT(INOUT) :: Table
    !% the key node
    CLASS(HashListNode),      INTENT(INOUT) :: KeyNode
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
    CLASS(IntrusiveHashList), INTENT(INOUT) :: Table
    !% new capacity of the hash table
    tIndex,                   INTENT(IN)    :: NewCap

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(IntrusiveLinearList), TARGET, ALLOCATABLE  :: TmpLst(:)    ! working variable
    CLASS(DoublyLinkedNode),   POINTER              :: CurrNode
    tIndex                                          :: OldCap, I, J

!** FLOW:

    ! move currently stored objects to temporary lists
    OldCap = Table%Capacity
    CALL MOVE_ALLOC(Table%WrkLst, TmpLst)

    ! allocate working lists to new capacity
    Table%Capacity = NewCap
    ALLOCATE(Table%WrkLst(0:Table%Capacity-1_kIndex))

    ! loop over the temporary lists to move stored objects (nodes)
    ! back to the working lists of the hash table
    DO I = 0_kIndex, OldCap-1_kIndex
        ! remove nodes from the current temporary list
        DO WHILE (.NOT.TmpLst(I)%IsEmpty())
            IF (.NOT.TmpLst(I)%RemoveFirst(CurrNode)) EXIT
            ! recompute the index into the working lists
            SELECT TYPE (CurrNode)
            CLASS IS (HashListNode)
                J = ComputeIndex(CurrNode%KeyHash, Table%Capacity)
            END SELECT
            ! add the current node to a working list
            CALL Table%WrkLst(J)%AddLast(CurrNode)
        END DO
    END DO

    ! free working variables
    NULLIFY(CurrNode)
    DEALLOCATE(TmpLst)

    RETURN

END SUBROUTINE HashTable_Resize

!******************************************************************************

FUNCTION HashTable_FindNode(Table, KeyNode, StoredNode, KeyHash, TabIndx) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the specified KeyNode in a symbol table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table object
    CLASS(IntrusiveHashList),               INTENT(INOUT)   :: Table
    !% the key node to be looked for in the table
    CLASS(HashListNode),                    INTENT(INOUT)   :: KeyNode
    !> the stored node where the key is equal to that of the specified key node;
    !  null pointer if the key is not found
    CLASS(HashListNode), OPTIONAL, POINTER, INTENT(OUT)     :: StoredNode
    !% hash code of the specified key node
    tHash,               OPTIONAL,          INTENT(OUT)     :: KeyHash
    !% index of the working list
    tIndex,              OPTIONAL,          INTENT(OUT)     :: TabIndx
    !% flag indicating whether the specified key is found or not.
    tLogical                                                :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
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

    IsTheEnd = Table%WrkLst(Indx)%StartFirst(CurrNode)

    ! iterate over the table to find key
    DO WHILE (.NOT.IsTheEnd)
        SELECT TYPE (CurrNode)
        CLASS IS (HashListNode)
            IF (CurrNode%IsKeyEqual(KeyNode)) THEN
                ! key found
                Found = TrueVal
                IF (PRESENT(StoredNode)) StoredNode => CurrNode
                EXIT
            END IF
        END SELECT
        IsTheEnd = Table%WrkLst(Indx)%MoveForward(CurrNode)
    END DO

    ! free working pointer
    NULLIFY(CurrNode)

    RETURN

END FUNCTION HashTable_FindNode

!******************************************************************************

FUNCTION HashTable_GetCursor(Table) RESULT(Cursor)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get a pointer to the cursor node of the current list of the hash table

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveHashList), INTENT(IN)    :: Table    !! hash table
    CLASS(HashListNode),      POINTER       :: Cursor   !! pointer to the cursor node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: StoredNode

! FLOW

    ! initialize
    Cursor => NULL()

    ! check whether we are iterating; if not simply return
    IF ((Table%Indx < 1_kIndex).OR.(Table%Indx > SIZE(Table%WrkLst))) RETURN

    StoredNode => Table%WrkLst(Table%Indx)%GetCursor()
    SELECT TYPE (StoredNode)
    CLASS IS (HashListNode)
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
    TYPE(IntrusiveHashList), INTENT(INOUT)  :: Table

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Table%Destruct()

    RETURN

END SUBROUTINE HashTable_Finalize

!******************************************************************************

END MODULE MClass_IntrusiveHashList

!******************************************************************************
