
MODULE MClass_PQBasic

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *PQBasic* type and its related routines.  The *PQBasic*
!   type is a priority-queue container where the type of its stored keys is in the
!   *Comparable* class (i.e. a concrete subtype of the *Comparable* type).  It uses
!   an elementary implementation where its array representation can be unordered or
!   ordered.  By default, its array representation is ordered where a sorting
!   algorithm is employed to order its stored keys.  <br>
!   Functionally, the *PQBasic* type is exactly the same as the *PQHeap* type.  They
!   only differs in their internal implementations where the *PQBasic* type can use
!   any sorting algorithm from the *MBase_SortAscend* and *MBase_SortDescend*
!   modules while the *PQHeap* type uses a binary heap (i.e. essentially heap sort)
!   to order its stored keys.  The sorting algorithms for the *PQBasic* type are
!   optionally specified when the container is created. <br>
!   See the <a href="../module/mclass_pqheap.html">MClass_PQHeap</a> module for more
!   explanations about its functionalities and limitations, which are the same as
!   those of the *PQBasic* type. <br>
!   See the <a href="../module/mbase_priorityqueues.html">MBase_PriorityQueues</a>
!   module for an overview of a *priority-queue-based* type. A user may use the
!   *MBase_PriorityQueues* module instead of using this module directly. <br>
!   See the <a href="../../xpfranknsort/module/mbase_sortdescend.html">MBase_SortDescend</a>
!   module for routines that perform sorting of an array in a *descending* order, and see the
!   <a href="../../xpfranknsort/module/mbase_sortascend.html">MBase_SortAscend</a> module for
!   routines that perform sorting of an array in an *ascending* order.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MClass_Comparable
    USE MBase_SortAscend,     ONLY: RustSortAscend  => RustSort
    USE MBase_SortDescend,    ONLY: RustSortDescend => RustSort

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: PQ_Basic

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     KeyType     CLASS(Comparable)
#define     PQBasic     PQ_Basic

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'MClass_PQBasic'

!** DERIVED TYPE DEFINITIONS
    !> The *PQBasic* type is a container type that employs a binary heap implementation
    !  to provide common operations for a priority queue.
    TYPE PQBasic
        PRIVATE
        !% pointer to last item of the priority queue
        tIndex                  :: Last = 0_kIndex
        !% incremental size of priority queue if it is full
        tIndex                  :: IncSize = 16_kIndex
        !% flag to shrink priority queue capacity
        tLogical                :: Shrink = FalseVal
        !> flag indicating whether the priority queue is implemented as
        !  a maximum PQ or a minimum PQ. <br>
        !  default -> a maximum PQ.
        tLogical                :: Min = FalseVal
        !> flag indicating whether the priority queue is implemented as
        !  an ordered or unordered array representation. <br>
        !  default -> an ordered array representation.
        tLogical                :: Unordered = FalseVal
        !% stored keys in the priority queue.
        KeyType, ALLOCATABLE    :: Keys(:)
        !% temporary key.
        KeyType, ALLOCATABLE    :: Temp
        !% pointer to sorting algorithm
        PROCEDURE(Sort), NOPASS, POINTER    :: SortAscend => NULL()
        !% pointer to sorting algorithm
        PROCEDURE(Sort), NOPASS, POINTER    :: SortDescend => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                      Public Procedures                    -----
        ! ---------------------------------------------------------------------
        ! -----     constructor and destructor procedures   -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty priority queue. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%CreateEmpty(InitCap, Mold)                 ! use default options  <br>
        !   --->    CALL Table%CreateEmpty(32, Mold, MinPQ=.TRUE.)        ! use min-priority queue <br>
        !   --->    CALL Table%CreateEmpty(32, Mold, IncSize=16)          ! specify incremental size <br>
        !   --->    CALL Table%CreateEmpty(32, Mold, Shrink=.TRUE.)       ! specify shrinking <br>
        !   --->    CALL Table%CreateEmpty(32, Mold, Unordered=.TRUE.)    ! specify unordered PQ <br>
        !   --->    CALL Table%CreateEmpty(32, Mold, SortAscend=WiseSort) ! specify ascending sorting algorithm <br>
        !   --->    CALL Table%CreateEmpty(32, Mold, SortDescend=TimSort) ! specify descending sorting algorithm <br>
        !           ! specify all options <br>
        !   --->    CALL Table%CreateEmpty(32, Mold, .TRUE., 16, .TRUE., .TRUE., RustSort, IntroSort) <br>
        PROCEDURE   :: CreateEmpty  => PQBasic_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a priority queue from the specified key arrays. <br>
        !  **Usage**: <br>
        !   ! use default options  <br>
        !   --->    CALL PQ%Construct(40, KeyArr) <br>
        !   ! specify all options (initial capacity is array size plus incremental size) <br>
        !   --->    CALL PQ%Construct(20, KeyArr, MinPQ, IncSize, Shrink, Unordered, SortAscend, SortDescend) <br>
        PROCEDURE   :: Construct    => PQBasic_ConstructorByArray
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all stored keys and free all memory currently used by the priority
        !       queue.  Optionally, stored keys can be retrieved. <br>
        !  **Usage**: <br>
        !   --->    CALL PQ%Destruct() <br>
        !   --->    CALL PQ%Destruct(StoredKeys) <br>
        PROCEDURE   :: Destruct     => PQBasic_Destructor
        ! -------------------------------------------------------
        ! -----         adding and removing procedures      -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key to the priority queue. <br>
        !  **Usage**: <br>
        !   --->    CALL PQ%Insert(Key) <br>
        PROCEDURE   :: Insert       => PQBasic_InsertKey
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To retrieve and remove the highest-priority key from the priority queue.  Also,
        !       return a flag indicating whether the key-value pair is successfully removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = PQ%Remove(Key) <br>
        !   --->    IF (.NOT.PQ%Remove(Key)) DoSomething
        PROCEDURE   :: Remove       => PQBasic_RemoveKey
        ! -------------------------------------------------------
        ! -----               inquiry procedures            -----
        ! -------------------------------------------------------
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the priority queue is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = PQ%IsEmpty() <br>
        !   --->    IF (.NOT.PQ%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty      => PQBasic_IsEmpty
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size (number of stored keys) of the priority queue. <br>
        !  **Usage**: <br>
        !   --->    Size = PQ%GetSize()
        PROCEDURE   :: GetSize      => PQBasic_GetSize
        !> **Type-Bound Function**: Peek <br>
        !  **Purpose**:  To retrieve the highest-priority key from the priority queue.  Also, return
        !       a flag indicating whether the key-value pair is successfully removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = PQ%Peek(Key) <br>
        !   --->    IF (.NOT.PQ%Peek(Key)) DoSomething
        PROCEDURE   :: Peek         => PQBasic_PeekKey
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: PQBasic_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE PQBasic

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        SUBROUTINE Sort(A)
            IMPORT
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Routines for PQBasic
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE PQBasic_CreateEmpty(PQ, InitCap, Mold, MinPQ, IncSize, Shrink, Unordered, &
                               SortAscend, SortDescend)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create an empty priority queue.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBasic),     INTENT(INOUT)   :: PQ       !! PQBasic object
    tIndex,             INTENT(IN)      :: InitCap  !! initial size of priority queue
    KeyType,            INTENT(IN)      :: Mold     !! mold
    tLogical, OPTIONAL, INTENT(IN)      :: MinPQ
    !^ true if the priority queue to be implemented as a MinPQ; default -> a MaxPQ
    tIndex,   OPTIONAL, INTENT(IN)      :: IncSize  !! incremental size of priority queue if it is full
    tLogical, OPTIONAL, INTENT(IN)      :: Shrink
    !^ true if want to reduce capacity when size is less than a quarter of the capacity;
    !  default is false.
    tLogical, OPTIONAL, INTENT(IN)      :: Unordered
    !^ true if the priority queue to be internally implemented as an unordered array PQ;
    !  default is an ordered array PQ.
    PROCEDURE(Sort), OPTIONAL           :: SortAscend   !! procedure to sort in ascending order
    PROCEDURE(Sort), OPTIONAL           :: SortDescend  !! procedure to sort in descending order

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

! FLOW

    ! first, check required input data
    IF (InitCap < 1_kIndex) THEN
        CALL Handle_ErrLevel('PQBasic_CreateEmpty', ModName, ErrWarning, &
                'Invalid InitCap (< 1).  Set the initial capacity of priority queue to 16.')
        Capacity = PQ%IncSize
    ELSE
        Capacity = InitCap
    END IF

    ! then, allocate space for the keys in the priority queue
    CALL MemAlloc(PQ%Keys, Capacity, Mold)

    ! finally, check optional input data
    IF (PRESENT(MinPQ))  PQ%Min = MinPQ
    IF (PRESENT(IncSize)) THEN
        IF (IncSize > 0_kIndex) PQ%IncSize = IncSize
    END IF
    IF (PRESENT(Shrink)) PQ%Shrink  =  Shrink
    IF (PRESENT(Unordered)) PQ%Unordered = Unordered
    IF (PRESENT(SortAscend)) PQ%SortAscend => SortAscend
    IF (PRESENT(SortDescend)) PQ%SortDescend => SortDescend

    RETURN

END SUBROUTINE PQBasic_CreateEmpty

!******************************************************************************

SUBROUTINE PQBasic_ConstructorByArray(PQ, N, Keys, MinPQ, IncSize, Shrink, Unordered, &
                                      SortAscend, SortDescend)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a priority queue from an array of key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBasic),     INTENT(INOUT)   :: PQ       !! PQBasic object
    tIndex,             INTENT(IN)      :: N        !! number of keys
    KeyType,            INTENT(IN)      :: Keys(N)  !! key array
    tLogical, OPTIONAL, INTENT(IN)      :: MinPQ
    !^ true if the priority queue to be implemented as a MinPQ; default -> a MaxPQ
    tIndex,   OPTIONAL, INTENT(IN)      :: IncSize  !! incremental size of priority queue if it is full
    tLogical, OPTIONAL, INTENT(IN)      :: Shrink
    !^ true if want to reduce capacity when size is less than a quarter of the capacity;
    !  default is false.
    tLogical, OPTIONAL, INTENT(IN)      :: Unordered
    !^ true if the priority queue to be internally implemented as an unordered array PQ;
    !  default is an ordered array PQ.
    PROCEDURE(Sort), OPTIONAL           :: SortAscend   !! procedure to sort in ascending order
    PROCEDURE(Sort), OPTIONAL           :: SortDescend  !! procedure to sort in descending order

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, InitCap

! FLOW

    ! simply return if N is less than 1
    IF (N <= 0_kIndex) RETURN

    ! create empty priority queue
    IF (PRESENT(IncSize)) THEN
        InitCap = N + IncSize
    ELSE
        InitCap = N + PQ%IncSize
    END IF
    CALL PQ%CreateEmpty(InitCap, Keys(1), MinPQ, IncSize, Shrink, Unordered, SortAscend, SortDescend)

    ! add input keys to the priority queue
    DO I = 1_kIndex, N
        CALL PQ%Insert(Keys(I))
    END DO

    RETURN

END SUBROUTINE PQBasic_ConstructorByArray

!******************************************************************************

SUBROUTINE PQBasic_Destructor(PQ, Keys)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct PQBasic object and get its keys if requested.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBasic),                 INTENT(INOUT)   :: PQ       !! PQBasic object
    KeyType, ALLOCATABLE, OPTIONAL, INTENT(OUT)     :: Keys(:)  !! array of keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: N, ID
    tLogical    :: Success

! FLOW

    IF (.NOT.PQ%IsEmpty()) THEN
        IF (PRESENT(Keys)) THEN
            ! get keys (and free memory of components of keys if applicable)
            N = PQ%Last
            CALL MemAlloc(Keys, N, PQ%Keys(1))
            DO ID = 1_kIndex, N
                Success = PQ%Remove(Keys(ID))
            END DO
        END IF
    END IF

    ! reset components
    PQ%Last    = 0_kIndex
    PQ%IncSize = 10_kIndex
    PQ%Shrink  = FalseVal
    PQ%Min     = FalseVal
    PQ%Unordered = FalseVal

    ! free memory of priority queue keys
    CALL MemFree(PQ%Keys)

    ! free pointers
    NULLIFY(PQ%SortAscend)
    NULLIFY(PQ%SortDescend)

    RETURN

END SUBROUTINE PQBasic_Destructor

!******************************************************************************

SUBROUTINE PQBasic_Finalizer(PQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(PQBasic), INTENT(INOUT)    :: PQ   !! PQBasic object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! free up memory and reset components
    CALL PQ%Destruct()

    RETURN

END SUBROUTINE PQBasic_Finalizer

!******************************************************************************

SUBROUTINE PQBasic_InsertKey(PQ, NewKey)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new key to the top (or bottom) of the priority queue.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBasic), INTENT(INOUT)   :: PQ       !! PQBasic object
    KeyType,        INTENT(IN)      :: NewKey   !! new key to be added to the priority queue

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

! FLOW

    IF (.NOT.SAME_TYPE_AS(NewKey, PQ%Keys)) THEN
        CALL Handle_ErrLevel('PQBasic_InsertKey', ModName, ErrWarning, &
                'Type of the inserted key must be the same as that of stored keys.')
        RETURN
    END IF

    ! check capacity of the priority queue
    Capacity = SIZE(PQ%Keys)
    IF (PQ%Last == Capacity) THEN
        ! increase the priority queue capacity
        Capacity = Capacity + PQ%IncSize
        ! resize the priority queue
        CALL MemResize(PQ%Keys, Capacity)
    END IF

    ! increment the pointer
    PQ%Last = PQ%Last + 1_kIndex

    ! then, add new key to the priority queue
    PQ%Keys(PQ%Last) = NewKey

    ! sort the keys if necessary
    IF (.NOT.PQ%Unordered) THEN
        ! the array elements 1 to PQ%Last-1 are already in order (total sorted)
        ! so perform sorting using sorting algorithm (algorithm can be changed
        ! via the use statements)
        IF (PQ%Min) THEN
            ! descending order
            IF (ASSOCIATED(PQ%SortDescend)) THEN
                CALL PQ%SortDescend(PQ%Keys(1:PQ%Last))
            ELSE
                CALL RustSortDescend(PQ%Keys(1:PQ%Last))
            END IF
        ELSE
            ! ascending order
            IF (ASSOCIATED(PQ%SortAscend)) THEN
                CALL PQ%SortAscend(PQ%Keys(1:PQ%Last))
            ELSE
                CALL RustSortAscend(PQ%Keys(1:PQ%Last))
            END IF
        END IF
    END IF

    RETURN

END SUBROUTINE PQBasic_InsertKey

!******************************************************************************

FUNCTION PQBasic_RemoveKey(PQ, HPKey) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve and remove the highest-priority key from the priority queue.  Also, return
    !  a flag indicating whether the key-value pair is successfully removed or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBasic), INTENT(INOUT)   :: PQ       !! PQBasic object
    KeyType,        INTENT(OUT)     :: HPKey    !! the highest-priority key
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity, HPIndex, I

! FLOW

    IF (.NOT.SAME_TYPE_AS(HPKey, PQ%Keys)) THEN
        CALL Handle_ErrLevel('PQBasic_RemoveKey', ModName, ErrWarning, &
                'Type of the retrieved key must be the same as that of stored keys.')
        Flag = FalseVal
        RETURN
    END IF

    ! first, check whether the priority queue is empty or not
    IF (PQ%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF

    ! get the highest-priority key
    IF (PQ%UnOrdered) THEN
        ! for an unordered-array PQ, get the index of the highest-priority key
        HPIndex = 1_kIndex
        IF (PQ%Min) THEN
            ! for MinPQ
            DO I = 2_kIndex, PQ%Last
                IF (PQ%Keys(HPIndex) > PQ%Keys(I)) HPIndex = I
            END DO
        ELSE
            ! for MaxPQ
            DO I = 2_kIndex, PQ%Last
                IF (PQ%Keys(HPIndex) < PQ%Keys(I)) HPIndex = I
            END DO
        END IF
        ! get the highest-priority key
        HPKey = PQ%Keys(HPIndex)
    ELSE
        ! for an ordered-array PQ, get the last key as the highest-priority one
        HPKey = PQ%Keys(PQ%Last)
    END IF

    ! remove the highest-priority key from the queue
    IF (PQ%UnOrdered) THEN
        ! swap the highest-priority key with the last one
        ASSOCIATE (Temp => PQ%Temp)
            Temp = PQ%Keys(HPIndex)
            EXCHANGE(PQ%Keys, HPIndex, PQ%Last)
        END ASSOCIATE
    END IF

    ! update pointer
    PQ%Last = PQ%Last - 1_kIndex

    ! shrink capacity of the priority queue if necessary
    IF (PQ%Shrink) THEN
        Capacity = SIZE(PQ%Keys, KIND=kIndex)
        IF ((PQ%Last > 0_kIndex).AND.(PQ%Last == Capacity/4_kIndex)) THEN
            ! reduce the priority queue capacity
            Capacity = Capacity/2_kIndex
            ! resize the priority queue
            CALL MemResize(PQ%Keys, Capacity)
        END IF
    END IF

    RETURN

END FUNCTION PQBasic_RemoveKey

!******************************************************************************

FUNCTION PQBasic_IsEmpty(PQ) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the priority queue is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBasic), INTENT(IN)  :: PQ   !! PQBasic object
    tLogical                    :: Flag !! true if the priority queue is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = (PQ%Last == 0_kIndex)

    RETURN

END FUNCTION PQBasic_IsEmpty

!******************************************************************************

FUNCTION PQBasic_GetSize(PQ) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get size of the priority queue.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBasic), INTENT(IN)  :: PQ   !! PQBasic object
    tIndex                      :: Size !! size (number of keys)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = PQ%Last

    RETURN

END FUNCTION PQBasic_GetSize

!******************************************************************************

FUNCTION PQBasic_PeekKey(PQ, HPKey) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the highest-priority key from the priority queue.  Also, return
    !  a flag indicating whether the key-value pair is successfully removed or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQBasic), INTENT(INOUT)   :: PQ      !! PQBasic object
    KeyType,        INTENT(OUT)     :: HPKey   !! the highest-priority key
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: HPIndex, I

! FLOW

    IF (.NOT.SAME_TYPE_AS(HPKey, PQ%Keys)) THEN
        CALL Handle_ErrLevel('PQBasic_PeekKey', ModName, ErrWarning, &
                'Type of the retrieved key must be the same as that of stored keys.')
        Flag = FalseVal
        RETURN
    END IF

    ! check whether the priority queue is empty or not
    IF (PQ%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF

    ! get the highest-priority key
    IF (PQ%UnOrdered) THEN
        ! for an unordered-array PQ, get the index of the highest-priority key
        HPIndex = 1_kIndex
        IF (PQ%Min) THEN
            ! for MinPQ
            DO I = 2_kIndex, PQ%Last
                IF (PQ%Keys(HPIndex) > PQ%Keys(I)) HPIndex = I
            END DO
        ELSE
            ! for MaxPQ
            DO I = 2_kIndex, PQ%Last
                IF (PQ%Keys(HPIndex) < PQ%Keys(I)) HPIndex = I
            END DO
        END IF
        ! get the highest-priority key
        HPKey = PQ%Keys(HPIndex)
    ELSE
        ! for an ordered-array PQ, get the last key as the highest-priority one
        HPKey = PQ%Keys(PQ%Last)
    END IF

    RETURN

END FUNCTION PQBasic_PeekKey

!******************************************************************************

END MODULE MClass_PQBasic

!******************************************************************************
