
MODULE MClass_PQHeap

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *PQHeap* type and its related routines.  The *PQHeap*
!   type is a priority-queue container where the type of its stored keys is in the
!   *Comparable* class (i.e. a concrete subtype of the *Comparable* type).  It uses
!   a binary heap implementation to order its stored keys. <br>
!   The *PQHeap* type can represent either a max-priority queue or a min-priority
!   queue.  By default, it represents the max-priority queue but a user can specify
!   the *MinPQ* argument to true so that it represents the min-priority queue instead. <br>
!   It should be noted that the *PQHeap* type does not allow keys with different types
!   to be stored in the same container.  The *PQHeap* type requires a user to specify
!   the type of keys to be stored via the *Mold* argument when an empty container is
!   created.  Alternatively, the user may implicitly specify the type of keys to be
!   stored via the *Keys* argument when a container is constructed from an array of
!   keys.  Also, it is important to note that the type of a key specified in all other
!   routines must be the same as the type of stored keys of the container.  Otherwise,
!   the container may not behave as expected. <br>
!   See the <a href="../module/mbase_priorityqueues.html">MBase_PriorityQueues</a>
!   module for an overview of a *priority-queue-based* type. A user may use the
!   *MBase_PriorityQueues* module instead of using this module directly. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MClass_Comparable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: PQ_Heap

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     KeyType     CLASS(Comparable)
#define     PQHeap      PQ_Heap

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'MClass_PQHeap'

!** DERIVED TYPE DEFINITIONS
    !> The *PQHeap* type is a container type that employs a binary heap implementation
    !  to provide common operations for a priority queue.
    TYPE PQHeap
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
        !% stored keys in the priority queue.
        KeyType, ALLOCATABLE    :: Keys(:)
        !% temporary key.
        KeyType, ALLOCATABLE    :: Temp
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                      Public Procedures                    -----
        ! ---------------------------------------------------------------------
        ! -----     constructor and destructor procedures   -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty priority queue. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%CreateEmpty(InitCap, Mold)                   ! use default options  <br>
        !   --->    CALL Table%CreateEmpty(32, Mold, MinPQ=.TRUE.)          ! use min-priority queue <br>
        !   --->    CALL Table%CreateEmpty(32, Mold, IncSize=16)            ! specify incremental size <br>
        !   --->    CALL Table%CreateEmpty(32, Mold, Shrink=.TRUE.)         ! specify shrinking <br>
        !   --->    CALL Table%CreateEmpty(32, Mold, .TRUE., 16, .TRUE.)    ! specify all options <br>
        PROCEDURE   :: CreateEmpty  => PQHeap_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a priority queue from the specified key arrays. <br>
        !  **Usage**: <br>
        !   ! use default options  <br>
        !   --->    CALL PQ%Construct(40, KeyArr) <br>
        !   ! specify all options (initial capacity is array size plus incremental size) <br>
        !   --->    CALL PQ%Construct(20, KeyArr, MinPQ, IncSize, Shrink) <br>
        PROCEDURE   :: Construct    => PQHeap_ConstructorByArray
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all stored keys and free all memory currently used by the priority
        !       queue.  Optionally, stored keys can be retrieved. <br>
        !  **Usage**: <br>
        !   --->    CALL PQ%Destruct() <br>
        !   --->    CALL PQ%Destruct(StoredKeys) <br>
        PROCEDURE   :: Destruct     => PQHeap_Destructor
        ! -------------------------------------------------------
        ! -----         adding and removing procedures      -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key to the priority queue. <br>
        !  **Usage**: <br>
        !   --->    CALL PQ%Insert(Key) <br>
        PROCEDURE   :: Insert       => PQHeap_InsertKey
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To retrieve and remove the highest-priority key from the priority queue.  Also,
        !       return a flag indicating whether the key-value pair is successfully removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = PQ%Remove(Key) <br>
        !   --->    IF (.NOT.PQ%Remove(Key)) DoSomething
        PROCEDURE   :: Remove       => PQHeap_RemoveKey
        ! -------------------------------------------------------
        ! -----               inquiry procedures            -----
        ! -------------------------------------------------------
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the priority queue is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = PQ%IsEmpty() <br>
        !   --->    IF (.NOT.PQ%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty      => PQHeap_IsEmpty
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size (number of stored keys) of the priority queue. <br>
        !  **Usage**: <br>
        !   --->    Size = PQ%GetSize()
        PROCEDURE   :: GetSize      => PQHeap_GetSize
        !> **Type-Bound Function**: Peek <br>
        !  **Purpose**:  To retrieve the highest-priority key from the priority queue.  Also, return
        !       a flag indicating whether the key-value pair is successfully removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = PQ%Peek(Key) <br>
        !   --->    IF (.NOT.PQ%Peek(Key)) DoSomething
        PROCEDURE   :: Peek         => PQHeap_PeekKey
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: PQHeap_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE PQHeap

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Routines for PQHeap
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE PQHeap_CreateEmpty(PQ, InitCap, Mold, MinPQ, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create an empty priority queue.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap),      INTENT(INOUT)   :: PQ       !! PQHeap object
    tIndex,             INTENT(IN)      :: InitCap  !! initial size of priority queue
    KeyType,            INTENT(IN)      :: Mold     !! mold
    tLogical, OPTIONAL, INTENT(IN)      :: MinPQ
    !^ true if the priority queue to be implemented as a MinPQ; default -> a MaxPQ
    tIndex,   OPTIONAL, INTENT(IN)      :: IncSize  !! incremental size of priority queue if it is full
    tLogical, OPTIONAL, INTENT(IN)      :: Shrink
    !^ true if want to reduce capacity when size is less than a quarter of the capacity;
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

! FLOW

    ! first, check required input data
    IF (InitCap < 1_kIndex) THEN
        CALL Handle_ErrLevel('PQHeap_CreateEmpty', ModName, ErrWarning, &
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

    RETURN

END SUBROUTINE PQHeap_CreateEmpty

!******************************************************************************

SUBROUTINE PQHeap_ConstructorByArray(PQ, N, Keys, MinPQ, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a priority queue from an array of key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap),      INTENT(INOUT)   :: PQ       !! PQHeap object
    tIndex,             INTENT(IN)      :: N        !! number of keys
    KeyType,            INTENT(IN)      :: Keys(N)  !! key array
    tLogical, OPTIONAL, INTENT(IN)      :: MinPQ
    !^ true if the priority queue to be implemented as a MinPQ; default -> a MaxPQ
    tIndex,   OPTIONAL, INTENT(IN)      :: IncSize  !! incremental size of priority queue if it is full
    tLogical, OPTIONAL, INTENT(IN)      :: Shrink
    !^ true if want to reduce capacity when size is less than a quarter of the capacity;
    !  default is false.

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
    CALL PQ%CreateEmpty(InitCap, Keys(1), MinPQ, IncSize, Shrink)

    ! add input keys to the priority queue
    DO I = 1_kIndex, N
        CALL PQ%Insert(Keys(I))
    END DO

    RETURN

END SUBROUTINE PQHeap_ConstructorByArray

!******************************************************************************

SUBROUTINE PQHeap_Destructor(PQ, Keys)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct PQHeap object and get its keys if requested.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap),                  INTENT(INOUT)   :: PQ       !! PQHeap object
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

    ! free memory of priority queue keys
    CALL MemFree(PQ%Keys)

    RETURN

END SUBROUTINE PQHeap_Destructor

!******************************************************************************

SUBROUTINE PQHeap_Finalizer(PQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(PQHeap), INTENT(INOUT) :: PQ   !! PQHeap object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! free up memory and reset components
    CALL PQ%Destruct()

    RETURN

END SUBROUTINE PQHeap_Finalizer

!******************************************************************************

SUBROUTINE PQHeap_InsertKey(PQ, NewKey)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new key to the top (or bottom) of the priority queue.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(INOUT)    :: PQ       !! PQHeap object
    KeyType,       INTENT(IN)       :: NewKey   !! new key to be added to the priority queue

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

! FLOW

    IF (.NOT.SAME_TYPE_AS(NewKey, PQ%Keys)) THEN
        CALL Handle_ErrLevel('PQHeap_InsertKey', ModName, ErrWarning, &
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

    ! restore heap order
    CALL ReHeapify_BottomUp(PQ, PQ%Last)

#ifdef DebugMode
    ! for debugging purpose
    IF (.NOT.IsHeapOrdered(PQ, 1_kIndex)) THEN
        CALL Handle_ErrLevel('PQHeap_InsertKey', ModName, ErrWarning, &
                             'The heap is NOT in order.')
    END IF
#endif

    RETURN

END SUBROUTINE PQHeap_InsertKey

!******************************************************************************

FUNCTION PQHeap_RemoveKey(PQ, HPKey) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve and remove the highest-priority key from the priority queue.  Also, return
    !  a flag indicating whether the key-value pair is successfully removed or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(INOUT)    :: PQ       !! PQHeap object
    KeyType,       INTENT(OUT)      :: HPKey    !! the highest-priority key
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

! FLOW

    IF (.NOT.SAME_TYPE_AS(HPKey, PQ%Keys)) THEN
        CALL Handle_ErrLevel('PQHeap_RemoveKey', ModName, ErrWarning, &
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
    HPKey = PQ%Keys(1)

    !--- remove the highest-priority key from the queue ---

    ! swap the highest-priority key with the last one
    ASSOCIATE (Temp => PQ%Temp)
        Temp = PQ%Keys(1)
        EXCHANGE(PQ%Keys, 1_kIndex, PQ%Last)
    END ASSOCIATE

    ! update pointer
    PQ%Last = PQ%Last - 1_kIndex

    ! restore heap order
    CALL ReHeapify_TopDown(PQ, 1_kIndex, PQ%Last)

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

#ifdef DebugMode
    ! for debugging purpose
    IF (.NOT.IsHeapOrdered(PQ, 1_kIndex)) THEN
        CALL Handle_ErrLevel('PQHeap_RemoveKey', ModName, ErrWarning, &
                             'The heap is NOT in order.')
    END IF
#endif

    RETURN

END FUNCTION PQHeap_RemoveKey

!******************************************************************************

FUNCTION PQHeap_IsEmpty(PQ) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the priority queue is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(IN)   :: PQ   !! PQHeap object
    tLogical                    :: Flag !! true if the priority queue is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = (PQ%Last == 0_kIndex)

    RETURN

END FUNCTION PQHeap_IsEmpty

!******************************************************************************

FUNCTION PQHeap_GetSize(PQ) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get size of the priority queue.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(IN)   :: PQ   !! PQHeap object
    tIndex                      :: Size !! size (number of keys)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = PQ%Last

    RETURN

END FUNCTION PQHeap_GetSize

!******************************************************************************

FUNCTION PQHeap_PeekKey(PQ, HPKey) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the highest-priority key from the priority queue.  Also, return
    !  a flag indicating whether the key-value pair is successfully removed or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(INOUT)    :: PQ      !! PQHeap object
    KeyType,       INTENT(OUT)      :: HPKey   !! the highest-priority key
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (.NOT.SAME_TYPE_AS(HPKey, PQ%Keys)) THEN
        CALL Handle_ErrLevel('PQHeap_PeekKey', ModName, ErrWarning, &
                'Type of the retrieved key must be the same as that of stored keys.')
        Flag = FalseVal
        RETURN
    END IF

    ! check whether the priority queue is empty or not
    IF (PQ%IsEmpty()) THEN
        Flag = FalseVal
    ELSE
        Flag = TrueVal
        ! get the highest-priority key
        HPKey = PQ%Keys(1)
    END IF

    RETURN

END FUNCTION PQHeap_PeekKey

!******************************************************************************

SUBROUTINE ReHeapify_TopDown(PQ, Start, Bottom)

!** PURPOSE OF THIS SUBROUTINE
    !^ To restore heap order by sinking down

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(INOUT)    :: PQ       !! PQHeap object
    tIndex,        INTENT(IN)       :: Start    !! starting index
    tIndex,        INTENT(IN)       :: Bottom   !! ending index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J

!** FLOW:

    ASSOCIATE (Temp => PQ%Temp)
        Temp = PQ%Keys(Start)
        I    = Start
        J    = I + I
        ! do while j <= bottom
        IF (PQ%Min) THEN
            ! for MinPQ
            DO
                IF (J > Bottom) EXIT
                IF (J < Bottom) THEN
                    ! compare to the better underling
                    IF (PQ%Keys(J) > PQ%Keys(J+1)) THEN
                        J = J + 1
                    END IF
                END IF
                ! found key's level. Terminate the sift-down.
                IF (PQ%Keys(J) >= Temp) EXIT
                ! otherwise, demote key and continue.
                PQ%Keys(I) = PQ%Keys(J)
                I = J
                J = I + I
            END DO
        ELSE
            ! for MaxPQ
            DO
                IF (J > Bottom) EXIT
                IF (J < Bottom) THEN
                    ! compare to the better underling
                    IF (PQ%Keys(J) < PQ%Keys(J+1)) THEN
                        J = J + 1
                    END IF
                END IF
                ! found key's level. Terminate the sift-down.
                IF (PQ%Keys(J) <= Temp) EXIT
                ! otherwise, demote key and continue.
                PQ%Keys(I) = PQ%Keys(J)
                I = J
                J = I + I
            END DO
        END IF
        ! put key into its slot.
        PQ%Keys(I) = Temp
    END ASSOCIATE

    RETURN

END SUBROUTINE ReHeapify_TopDown

!******************************************************************************

SUBROUTINE ReHeapify_BottomUp(PQ, Start)

!** PURPOSE OF THIS SUBROUTINE
    !^ To restore heap order by swimming up.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(INOUT)    :: PQ       !! PQHeap object
    tIndex,        INTENT(IN)       :: Start    !! starting index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J

!** FLOW:

    ASSOCIATE (Temp => PQ%Temp)
        Temp = PQ%Keys(Start)
        I    = Start
        J    = I/2
        ! do while k > 1 and key(k/2) < key(k)
        IF (PQ%Min) THEN
            ! for MinPQ
            DO WHILE ((I > 1).AND.(PQ%Keys(J) > Temp))
                ! promote key and continue.
                PQ%Keys(I) = PQ%Keys(J)
                I = J
                J = I/2
            END DO
        ELSE
            ! for MaxPQ
            DO WHILE ((I > 1).AND.(PQ%Keys(J) < Temp))
                ! promote key and continue.
                PQ%Keys(I) = PQ%Keys(J)
                I = J
                J = I/2
            END DO
        END IF
        ! put key into its slot.
        PQ%Keys(I) = Temp
    END ASSOCIATE

    RETURN

END SUBROUTINE ReHeapify_BottomUp

!******************************************************************************

RECURSIVE FUNCTION IsHeapOrdered(PQ, Start) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE
    !^ To check whether the heap is in order or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(IN)   :: PQ       !! PQHeap object
    tIndex,        INTENT(IN)   :: Start    !! starting index
    tLogical                    :: Flag     !! true if the heap is in order

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Left, Right

!** FLOW:

    IF (Start > PQ%Last) THEN
        Flag = TrueVal
        RETURN
    END IF

    Left  = Start + Start
    Right = Left + 1

    IF (PQ%Min) THEN
        ! for MinPQ
        IF ((Left <= PQ%Last) .AND. (PQ%Keys(Start) > PQ%Keys(Left))) THEN
            Flag = FalseVal
            RETURN
        END IF
        IF ((Right <= PQ%Last) .AND. (PQ%Keys(Start) > PQ%Keys(Right))) THEN
            Flag = FalseVal
            RETURN
        END IF
    ELSE
        ! for MaxPQ
        IF ((Left <= PQ%Last) .AND. (PQ%Keys(Start) < PQ%Keys(Left))) THEN
            Flag = FalseVal
            RETURN
        END IF
        IF ((Right <= PQ%Last) .AND. (PQ%Keys(Start) < PQ%Keys(Right))) THEN
            Flag = FalseVal
            RETURN
        END IF
    END IF

    Flag = (IsHeapOrdered(PQ, Left) .AND. IsHeapOrdered(PQ, Right))

    RETURN

END FUNCTION IsHeapOrdered

!******************************************************************************

END MODULE MClass_PQHeap

!******************************************************************************
