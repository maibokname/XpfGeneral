
MODULE MBase_AnyRankNType

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains common routines for unlimited polymorphic and assumed-type entities.
!   For most procedures, the specified polymorphic arguments can have any rank between 1 and 7. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_ByteUtil

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: AnyType_CopyData
    PUBLIC  :: AnyType_SwapData
    PUBLIC  :: MemFree
    PUBLIC  :: MemAlloc
    PUBLIC  :: MemResize
    PUBLIC  :: MemFreePtr
    PUBLIC  :: MemAllocPtr
    PUBLIC  :: MemResizePtr

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MBase_AnyRankNType'
    ! Size of character variable for storing error messages returned from ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! storage size of 8-bit integer
    tIndex,    PARAMETER    :: ByteStoreSize = STORAGE_SIZE(1_kInt8, KIND=kIndex)

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE MemFree
        !^ **Subroutine Interface**: MemFree <br>
        !  **Purpose**:  To free memory of the specified argument, which is declared as an
        !       unlimited polymorphic entity with *ALLOCATABLE* attribute. The specified
        !       argument can be a scalar argument or an array argument with any rank. <br>
        !  **Usage**: <br>
        !   --->    CALL MemFree(A) <br>
        MODULE PROCEDURE AnyType_MemFree_Allocatable
    END INTERFACE
    INTERFACE MemAlloc
        !^ **Subroutine Interface**: MemAlloc <br>
        !  **Purpose**:  To allocate memory of the specified argument, which is declared as an
        !       unlimited polymorphic entity with *ALLOCATABLE*  attribute.  The specified
        !       argument can be a scalar argument or an array argument with the rank between
        !       1 and 7.  The procedure requires the *Prototype* argument to be used as a mold. <br>
        !  **Usage**: <br>
        !   ! allocate A with starting indices of 1 <br>
        !   --->    CALL MemAlloc(A, Prototype, ASize) <br>
        !   ! allocate A with specified starting indices <br>
        !   --->    CALL MemAlloc(A, Prototype, ASize, StartID) <br>
        !  **Note**: The "ASize" and "StartID" arguments must be arrays where their size are equal
        !            to the rank of the specified array argument. <br>
        MODULE PROCEDURE AnyType_MemAlloc_Allocatable
    END INTERFACE
    INTERFACE MemResize
        !^ **Subroutine Interface**: MemResize <br>
        !  **Purpose**:  To re-allocate memory of the specified argument (and preserve its data),
        !       which is declared as an unlimited polymorphic entity with *ALLOCATABLE* attribute.
        !       The specified argument must have already been allocated and it must be an array
        !       argument with the rank between 1 and 7.  The procedure requires the *NewSize*
        !       argument to specify sizes in each dimension of the entity. <br>
        !  **Usage**: <br>
        !   --->    CALL MemResize(A, NewSize) <br>
        !  **Note**: The "NewSize" argument must be an array where its size is equal to the rank
        !            of the specified array argument. <br>
        MODULE PROCEDURE AnyType_MemResize_Allocatable
    END INTERFACE
    INTERFACE MemFreePtr
        !^ **Subroutine Interface**: MemFreePtr <br>
        !  **Purpose**:  To free memory of the specified argument, which is declared as an
        !       unlimited polymorphic entity with *POINTER* attribute. The specified argument
        !       can be a scalar argument or an array argument with any rank. <br>
        !  **Usage**: <br>
        !   --->    CALL MemFreePtr(A) <br>
        MODULE PROCEDURE AnyType_MemFree_Pointer
    END INTERFACE
    INTERFACE MemAllocPtr
        !^ **Subroutine Interface**: MemAllocPtr <br>
        !  **Purpose**:  To allocate memory of the specified argument, which is declared as an
        !       unlimited polymorphic entity with *POINTER*  attribute.  The specified argument
        !       can be a scalar argument or an array argument with the rank between 1 and 7.
        !       The procedure requires the *Prototype* argument to be used as a mold. <br>
        !  **Usage**: <br>
        !   ! allocate A with starting indices of 1 <br>
        !   --->    CALL MemAllocPtr(A, Prototype, ASize) <br>
        !   ! allocate A with specified starting indices <br>
        !   --->    CALL MemAllocPtr(A, Prototype, ASize, StartID) <br>
        !  **Note**: The "ASize" and "StartID" arguments must be arrays where their size are equal
        !            to the rank of the specified array argument. <br>
        MODULE PROCEDURE AnyType_MemAlloc_Pointer
    END INTERFACE
    INTERFACE MemResizePtr
        !^ **Subroutine Interface**: MemResize <br>
        !  **Purpose**:  To re-allocate memory of the specified argument (and preserve its data),
        !       which is declared as an unlimited polymorphic entity with *POINTER* attribute.
        !       The specified argument must have already been allocated and it must be an array
        !       argument with the rank between 1 and 7.  The procedure requires the *NewSize*
        !       argument to specify sizes in each dimension of the entity. <br>
        !  **Usage**: <br>
        !   --->    CALL MemResizePtr(A, NewSize) <br>
        !  **Note**: The "NewSize" argument must be an array where its size is equal to the rank
        !            of the specified array argument. <br>
        MODULE PROCEDURE AnyType_MemResize_Pointer
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION AnyType_CopyData(InData, OutData) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make a copy of the input data.  The *OutData* and *InData* arguments must have the
    !  same types, sizes, and ranks.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), CONTIGUOUS, INTENT(IN)    :: InData(..)   !! input
    CLASS(*), CONTIGUOUS, INTENT(INOUT) :: OutData(..)  !! output
    tLogical                            :: Success      !! true if no error occurred while copying data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: StoreSize

!** FLOW:

    ! set initial flag result
    Success = FalseVal

    ! check for mismatch data
    IF (RANK(OutData) /= RANK(InData)) THEN
        CALL Handle_ErrLevel('AnyType_CopyData', ModName, ErrSevere, &
            'Ranks of input and output are NOT the same.')
        RETURN
    ELSEIF (SIZE(OutData, KIND=kIndex) /= SIZE(InData, KIND=kIndex)) THEN
        CALL Handle_ErrLevel('AnyType_CopyData', ModName, ErrSevere, &
            'Sizes of input and output are NOT the same.')
        RETURN
    ELSE
        StoreSize = STORAGE_SIZE(InData, KIND=kIndex)
        IF (STORAGE_SIZE(OutData, KIND=kIndex) /= StoreSize) THEN
            CALL Handle_ErrLevel('AnyType_CopyData', ModName, ErrSevere, &
                'Storage sizes of input and output are NOT the same.')
            RETURN
        END IF
    END IF
    SELECT RANK(OutPtr => OutData)
    RANK (0)
#define     OutRank        0
#include    "Includes/CopyData_AnyType_CodeFracment.f90"
#undef      OutRank
    RANK (1)
#define     OutRank        1
#include    "Includes/CopyData_AnyType_CodeFracment.f90"
#undef      OutRank
    RANK (2)
#define     OutRank        2
#include    "Includes/CopyData_AnyType_CodeFracment.f90"
#undef      OutRank
    RANK (3)
#define     OutRank        3
#include    "Includes/CopyData_AnyType_CodeFracment.f90"
#undef      OutRank
    RANK (4)
#define     OutRank        4
#include    "Includes/CopyData_AnyType_CodeFracment.f90"
#undef      OutRank
    RANK (5)
#define     OutRank        5
#include    "Includes/CopyData_AnyType_CodeFracment.f90"
#undef      OutRank
    RANK (6)
#define     OutRank        6
#include    "Includes/CopyData_AnyType_CodeFracment.f90"
#undef      OutRank
    RANK (7)
#define     OutRank        7
#include    "Includes/CopyData_AnyType_CodeFracment.f90"
#undef      OutRank
    RANK DEFAULT
        CALL Handle_ErrLevel('AnyType_CopyData', ModName, ErrSevere, &
            'Currently, only handle an entity with the rank between 0 and 7.')
        RETURN
    END SELECT

    RETURN

END FUNCTION AnyType_CopyData

!******************************************************************************

FUNCTION AnyType_SwapData(Data1, Data2) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To swap the specified data.  Both arguments must have the same types, sizes, and ranks.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), CONTIGUOUS, INTENT(INOUT) :: Data1(..)   !! input
    CLASS(*), CONTIGUOUS, INTENT(INOUT) :: Data2(..)  !! output
    tLogical                            :: Success      !! true if no error occurred while copying data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: StoreSize

!** FLOW:

    ! set initial flag result
    Success = FalseVal

    ! check for mismatch data
    IF (RANK(Data2) /= RANK(Data1)) THEN
        CALL Handle_ErrLevel('AnyType_SwapData', ModName, ErrSevere, &
            'Ranks of input and output are NOT the same.')
        RETURN
    ELSEIF (SIZE(Data2, KIND=kIndex) /= SIZE(Data1, KIND=kIndex)) THEN
        CALL Handle_ErrLevel('AnyType_SwapData', ModName, ErrSevere, &
            'Sizes of input and output are NOT the same.')
        RETURN
    ELSE
        StoreSize = STORAGE_SIZE(Data1, KIND=kIndex)
        IF (STORAGE_SIZE(Data2, KIND=kIndex) /= StoreSize) THEN
            CALL Handle_ErrLevel('AnyType_SwapData', ModName, ErrSevere, &
                'Storage sizes of input and output are NOT the same.')
            RETURN
        END IF
    END IF
    SELECT RANK(Data2Ptr => Data2)
    RANK (0)
#define     ThisRank        0
        BLOCK
            CLASS(*), ALLOCATABLE   :: TmpDat
#include    "Includes/SwapData_AnyType_CodeFracment.f90"
        END BLOCK
#undef      ThisRank
    RANK (1)
#define     ThisRank        1
        BLOCK
            CLASS(*), ALLOCATABLE   :: TmpDat(:)
#include    "Includes/SwapData_AnyType_CodeFracment.f90"
        END BLOCK
#undef      ThisRank
    RANK (2)
#define     ThisRank        2
        BLOCK
            CLASS(*), ALLOCATABLE   :: TmpDat(:,:)
#include    "Includes/SwapData_AnyType_CodeFracment.f90"
        END BLOCK
#undef      ThisRank
    RANK (3)
#define     ThisRank        3
        BLOCK
            CLASS(*), ALLOCATABLE   :: TmpDat(:,:,:)
#include    "Includes/SwapData_AnyType_CodeFracment.f90"
        END BLOCK
#undef      ThisRank
    RANK (4)
#define     ThisRank        4
        BLOCK
            CLASS(*), ALLOCATABLE   :: TmpDat(:,:,:,:)
#include    "Includes/SwapData_AnyType_CodeFracment.f90"
        END BLOCK
#undef      ThisRank
    RANK (5)
#define     ThisRank        5
        BLOCK
            CLASS(*), ALLOCATABLE   :: TmpDat(:,:,:,:,:)
#include    "Includes/SwapData_AnyType_CodeFracment.f90"
        END BLOCK
#undef      ThisRank
    RANK (6)
#define     ThisRank        6
        BLOCK
            CLASS(*), ALLOCATABLE   :: TmpDat(:,:,:,:,:,:)
#include    "Includes/SwapData_AnyType_CodeFracment.f90"
        END BLOCK
#undef      ThisRank
    RANK (7)
#define     ThisRank        7
        BLOCK
            CLASS(*), ALLOCATABLE   :: TmpDat(:,:,:,:,:,:,:)
#include    "Includes/SwapData_AnyType_CodeFracment.f90"
        END BLOCK
#undef      ThisRank
    RANK DEFAULT
        CALL Handle_ErrLevel('AnyType_SwapData', ModName, ErrSevere, &
            'Currently, only handle an entity with the rank between 0 and 7.')
        RETURN
    END SELECT

    RETURN

END FUNCTION AnyType_SwapData

!******************************************************************************

SUBROUTINE AnyType_MemFree_Allocatable(A)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of an allocatable entity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(..)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:

    IF (ALLOCATED(A)) THEN
        SELECT RANK(APtr => A)
        RANK (0)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (1)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (2)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (3)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (4)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (5)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (6)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (7)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK DEFAULT
            CALL Handle_ErrLevel('AnyType_MemFree_Allocatable', ModName, ErrSevere, &
                'Currently, only handle an entity with the rank between 0 and 7.')
            RETURN
        END SELECT
        CALL Handle_ErrDealloc('AnyType_MemFree_Allocatable', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE AnyType_MemFree_Allocatable

!******************************************************************************

SUBROUTINE AnyType_MemAlloc_Allocatable(A, Prototype, ASize, StartID)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate memory of an allocatable entity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A(..)
    !^ An unlimited polymorphic allocatable entity with an assumed rank.  Currently,
    !  the procedure can handle the entity with the rank between 0 and 7.
    CLASS(*),              INTENT(IN)       :: Prototype
    !^ A copy providing the concrete type of *A*. <br>
    tIndex,                INTENT(IN)       :: ASize(RANK(A))
    !^ An array specifying sizes of A in each dimension.
    !  All its element values must be non-negative.
    tIndex,   OPTIONAL,    INTENT(IN)       :: StartID(RANK(A))
    !^ An array specifying starting indices of A in each dimension.
    !  If *ASize* is NOT present, the *StartID* is ignored.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tLogical            :: Handled

!** FLOW:

    ! free memory if allocated
    CALL MemFree(A)

    BLOCK
        tIndex      :: I, ARank
        tIndex      :: BeginID(RANK(A)), EndID(RANK(A))
        tLogical    :: ValidSize
        ! first check whether the specified ASize is valid
        ! and if so, compute start and ending indices
        ARank = RANK(A)
        IF (ARank > 0_kIndex) THEN
            ValidSize = TrueVal
            DO I = 1_kIndex, ARank
                IF (ASize(I) < 0_kIndex) THEN
                    ValidSize = FalseVal
                    EXIT
                END IF
            END DO
            IF (.NOT.ValidSize) THEN
                CALL Handle_ErrLevel('AnyType_MemAlloc_Allocatable', ModName, ErrSevere, &
                    'The specified "ASize" is NOT valid.  All its values must be non-negative.')
                RETURN
            END IF
            IF (PRESENT(StartID)) THEN
                BeginID = StartID
                EndID = BeginID + ASize - 1_kIndex
            ELSE
                BeginID = 1_kIndex
                EndID = ASize
            END IF
        END IF
        ! initialize error flags
        Handled = TrueVal
        SELECT RANK(A)
        RANK(0)
            ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(1)
            ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(2)
            ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, &
                     ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(3)
            ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                     STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(4)
            ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                     BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(5)
            ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                     BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                     STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(6)
            ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                     BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                     STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(7)
            ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                     BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                     BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK DEFAULT
            Handled = FalseVal
        END SELECT
    END BLOCK
    IF (Handled) THEN
        CALL Handle_ErrAlloc('AnyType_MemAlloc_Allocatable', ModName, AllocMsg, AllocStat)
    ELSE
        CALL Handle_ErrLevel('AnyType_MemAlloc_Allocatable', ModName, ErrSevere, &
            'Currently, only handle entities with the rank between 0 and 7.')
    END IF

    RETURN

END SUBROUTINE AnyType_MemAlloc_Allocatable

!******************************************************************************

SUBROUTINE AnyType_MemResize_Allocatable(A, NewSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To re-allocate memory of an allocatable entity and preserve its data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), TARGET, ALLOCATABLE, INTENT(INOUT)    :: A(..)
    !^ An unlimited polymorphic allocatable entity with an assumed rank.  Currently,
    !  the procedure can handle the entity with the rank between 1 and 7.
    tIndex,                        INTENT(IN)       :: NewSize(RANK(A))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical            :: Handled
    tLogical            :: ZeroSize
    tIndex              :: I, ARank
    tIndex              :: BeginID(RANK(A)), EndID(RANK(A))
    tIndex              :: OldSize(RANK(A)), PSize(RANK(A))
    CLASS(*), POINTER   :: AMold

!** FLOW:

    ! check whether A has been allocated or not.
    ! if not, it cannot be used as a mold to re-allocate its self
    IF (.NOT.ALLOCATED(A)) THEN
        CALL Handle_ErrLevel('AnyType_MemResize_Allocatable', ModName, ErrSevere, &
            '"A" has not yet been allocated.  Use the "MemAlloc" procedure instead.')
        RETURN
    END IF

    ! first check validity of A's rank and values of elements of NewSize
    ARank = RANK(A)
    IF (ARank > 0_kIndex) THEN
        BLOCK
            tLogical    :: ValidSize
            ValidSize = TrueVal
            DO I = 1_kIndex, ARank
                IF (NewSize(I) < 0_kIndex) THEN
                    ValidSize = FalseVal
                    EXIT
                END IF
            END DO
            IF (.NOT.ValidSize) THEN
                CALL Handle_ErrLevel('AnyType_MemResize_Allocatable', ModName, ErrSevere, &
                    'The specified "NewSize" is NOT valid.  All its values must be non-negative.')
                RETURN
            END IF
        END BLOCK
    ELSE
        CALL Handle_ErrLevel('AnyType_MemResize_Allocatable', ModName, ErrSevere, &
            'An entity with rank of 0 cannot be resized.')
        RETURN
    END IF

    ZeroSize = FalseVal
    SELECT RANK (A)
    RANK (1)
        OldSize(1) = SIZE(A, DIM=I, KIND=kIndex)
        BeginID(1) = LBOUND(A, DIM=I, KIND=kIndex)
        IF (OldSize(1) < 1_kIndex) ZeroSize = TrueVal
        AMold => A(BeginID(1))
    RANK (2)
        DO I = 1_kIndex, 2_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
        AMold => A(BeginID(1), BeginID(2))
    RANK (3)
        DO I = 1_kIndex, 3_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
        AMold => A(BeginID(1), BeginID(2), BeginID(3))
    RANK (4)
        DO I = 1_kIndex, 4_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
        AMold => A(BeginID(1), BeginID(2), BeginID(3), BeginID(4))
    RANK (5)
        DO I = 1_kIndex, 5_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
        AMold => A(BeginID(1), BeginID(2), BeginID(3), BeginID(4), BeginID(5))
    RANK (6)
        DO I = 1_kIndex, 6_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
        AMold => A(BeginID(1), BeginID(2), BeginID(3), BeginID(4), BeginID(5), BeginID(6))
    RANK (7)
        DO I = 1_kIndex, 4_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
        AMold => A(BeginID(1), BeginID(2), BeginID(3), BeginID(4), BeginID(5), BeginID(6), BeginID(7))
    RANK DEFAULT
        Handled = FalseVal
    END SELECT

    IF (ZeroSize) THEN
        ! one or more of A's dimensions has zero size; therefore, simply allocate A to new sizes
        CALL MemAlloc(A, AMold, NewSize, BeginID)
    ELSE
        Handled = TrueVal
        SELECT RANK (A)
        RANK (1)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:)
                tLogical                :: Success
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, AMold, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(A(BeginID(1):EndID(1)), Temp(BeginID(1):EndID(1)))
                IF (.NOT.Success) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK (2)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:,:)
                tLogical                :: Success
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, AMold, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(A(BeginID(1):EndID(1),BeginID(2):EndID(2)), &
                                           Temp(BeginID(1):EndID(1),BeginID(2):EndID(2)))
                IF (.NOT.Success) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK (3)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:,:,:)
                tLogical                :: Success
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, AMold, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(A(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3)), &
                                           Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3)))
                IF (.NOT.Success) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK (4)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:,:,:,:)
                tLogical                :: Success
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, AMold, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(A(BeginID(1):EndID(1),BeginID(2):EndID(2), &
                                             BeginID(3):EndID(3),BeginID(4):EndID(4)), &
                                        Temp(BeginID(1):EndID(1),BeginID(2):EndID(2), &
                                             BeginID(3):EndID(3),BeginID(4):EndID(4)))
                IF (.NOT.Success) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK (5)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:,:,:,:,:)
                tLogical                :: Success
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, AMold, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(A(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                             BeginID(4):EndID(4),BeginID(5):EndID(5)), &
                                        Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                             BeginID(4):EndID(4),BeginID(5):EndID(5)))
                IF (.NOT.Success) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK (6)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:,:,:,:,:,:)
                tLogical                :: Success
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, AMold, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(A(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                             BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6)), &
                                        Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                             BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6)))
                IF (.NOT.Success) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK (7)
            BLOCK
                CLASS(*), ALLOCATABLE   :: Temp(:,:,:,:,:,:,:)
                tLogical                :: Success
                ! first, allocate the temporary array
                CALL MemAlloc(Temp, AMold, NewSize, BeginID)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(A(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                             BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6), &
                                             BeginID(7):EndID(7)), &
                                        Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                             BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6), &
                                             BeginID(7):EndID(7)))
                IF (.NOT.Success) THEN
                    ! free up memory
                    CALL MemFree(Temp)
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Allocatable', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                    RETURN
                END IF
                ! move data from the temporary array back to the array
                CALL MemFree(A)
                CALL MOVE_ALLOC(Temp, A)
            END BLOCK
        RANK DEFAULT
            Handled = FalseVal
        END SELECT
        IF (.NOT.Handled) THEN
            CALL Handle_ErrLevel('AnyType_MemResize_Allocatable', ModName, ErrSevere, &
                'Currently, only handle an entity with the rank between 1 and 7.')
        END IF
    END IF

    ! free local pointer
    NULLIFY(AMold)

    RETURN

END SUBROUTINE AnyType_MemResize_Allocatable

!******************************************************************************

SUBROUTINE AnyType_MemFree_Pointer(A)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of a pointer entity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: A(..)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:

    IF (ASSOCIATED(A)) THEN
        SELECT RANK(APtr => A)
        RANK (0)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (1)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (2)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (3)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (4)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (5)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (6)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK (7)
            DEALLOCATE(APtr, STAT=AllocStat, ERRMSG=AllocMsg)
        RANK DEFAULT
            CALL Handle_ErrLevel('AnyType_MemFree_Pointer', ModName, ErrSevere, &
                'Currently, only handle an entity with the rank between 0 and 7.')
            RETURN
        END SELECT
        CALL Handle_ErrDealloc('AnyType_MemFree_Pointer', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE AnyType_MemFree_Pointer

!******************************************************************************

SUBROUTINE AnyType_MemAlloc_Pointer(A, Prototype, ASize, StartID, NoDeAlloc)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate memory of a pointer entity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),  POINTER, INTENT(INOUT)   :: A(..)
    !^ An unlimited polymorphic pointer entity with an assumed rank.  Currently,
    !  the procedure can handle the entity with the rank between 0 and 7.
    CLASS(*),           INTENT(IN)      :: Prototype
    !^ A copy providing the concrete type of *A*. <br>
    tIndex,             INTENT(IN)      :: ASize(RANK(A))
    !^ An array specifying sizes of A in each dimension.
    !  All its element values must be non-negative.
    tIndex,   OPTIONAL, INTENT(IN)      :: StartID(RANK(A))
    !^ An array specifying starting indices of A in each dimension.
    !  If *ASize* is NOT present, the *StartID* is ignored.
    tLogical, OPTIONAL, INTENT(IN)      :: NoDeAlloc
    !^ If present and true, only perform memory allocation.  Otherwise, first perform
    !  memory deallocation and then memory allocation.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tLogical            :: Handled
    tLogical            :: FreePtr

!** FLOW:

    ! free memory if needed
    FreePtr = TrueVal
    IF (PRESENT(NoDeAlloc)) FreePtr = .NOT.NoDeAlloc
    IF (FreePtr) CALL MemFreePtr(A)

    BLOCK
        tIndex      :: I, ARank
        tIndex      :: BeginID(RANK(A)), EndID(RANK(A))
        tLogical    :: ValidSize
        ! first check whether the specified ASize is valid
        ! and if so, compute start and ending indices
        ARank = RANK(A)
        IF (ARank > 0_kIndex) THEN
            ValidSize = TrueVal
            DO I = 1_kIndex, ARank
                IF (ASize(I) < 0_kIndex) THEN
                    ValidSize = FalseVal
                    EXIT
                END IF
            END DO
            IF (.NOT.ValidSize) THEN
                CALL Handle_ErrLevel('AnyType_MemAlloc_Pointer', ModName, ErrSevere, &
                    'The specified "ASize" is NOT valid.  All its values must be non-negative.')
                RETURN
            END IF
            IF (PRESENT(StartID)) THEN
                BeginID = StartID
                EndID = BeginID + ASize - 1_kIndex
            ELSE
                BeginID = 1_kIndex
                EndID = ASize
            END IF
        END IF
        ! initialize error flags
        Handled = TrueVal
        SELECT RANK(A)
        RANK(0)
            ALLOCATE(A, STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(1)
            ALLOCATE(A(BeginID(1):EndID(1)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(2)
            ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2)), STAT=AllocStat, &
                     ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(3)
            ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3)), &
                     STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(4)
            ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                     BeginID(4):EndID(4)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(5)
            ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                     BeginID(4):EndID(4), BeginID(5):EndID(5)), &
                     STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(6)
            ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                     BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6)), &
                     STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK(7)
            ALLOCATE(A(BeginID(1):EndID(1), BeginID(2):EndID(2), BeginID(3):EndID(3), &
                     BeginID(4):EndID(4), BeginID(5):EndID(5), BeginID(6):EndID(6), &
                     BeginID(7):EndID(7)), STAT=AllocStat, ERRMSG=AllocMsg, MOLD=Prototype)
        RANK DEFAULT
            Handled = FalseVal
        END SELECT
    END BLOCK
    IF (Handled) THEN
        CALL Handle_ErrAlloc('AnyType_MemAlloc_Pointer', ModName, AllocMsg, AllocStat)
    ELSE
        CALL Handle_ErrLevel('AnyType_MemAlloc_Pointer', ModName, ErrSevere, &
            'Currently, only handle entities with the rank between 0 and 7.')
    END IF

    RETURN

END SUBROUTINE AnyType_MemAlloc_Pointer

!******************************************************************************

SUBROUTINE AnyType_MemResize_Pointer(A, NewSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To re-allocate memory of an pointer entity and preserve its data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: A(..)
    !^ An unlimited polymorphic pointer entity with an assumed rank.  Currently,
    !  the procedure can handle the entity with the rank between 1 and 7.
    tIndex,            INTENT(IN)       :: NewSize(RANK(A))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical            :: Handled
    tLogical            :: ZeroSize
    tIndex              :: I, ARank
    tIndex              :: BeginID(RANK(A)), EndID(RANK(A))
    tIndex              :: OldSize(RANK(A)), PSize(RANK(A))
    CLASS(*), POINTER   :: AMold

!** FLOW:

    ! check whether A has been allocated or not.
    ! if not, it cannot be used as a mold to re-allocate its self
    IF (.NOT.ASSOCIATED(A)) THEN
        CALL Handle_ErrLevel('AnyType_MemResize_Pointer', ModName, ErrSevere, &
            '"A" has not yet been allocated.  Use the "MemAllocPtr" procedure instead.')
        RETURN
    END IF

    ! first check validity of A's rank and values of elements of NewSize
    ARank = RANK(A)
    IF (ARank > 0_kIndex) THEN
        BLOCK
            tLogical    :: ValidSize
            ValidSize = TrueVal
            DO I = 1_kIndex, ARank
                IF (NewSize(I) < 0_kIndex) THEN
                    ValidSize = FalseVal
                    EXIT
                END IF
            END DO
            IF (.NOT.ValidSize) THEN
                CALL Handle_ErrLevel('AnyType_MemResize_Pointer', ModName, ErrSevere, &
                    'The specified "NewSize" is NOT valid.  All its values must be non-negative.')
                RETURN
            END IF
        END BLOCK
    ELSE
        CALL Handle_ErrLevel('AnyType_MemResize_Pointer', ModName, ErrSevere, &
            'An entity with rank of 0 cannot be resized.')
        RETURN
    END IF

    ZeroSize = FalseVal
    SELECT RANK (A)
    RANK (1)
        OldSize(1) = SIZE(A, DIM=I, KIND=kIndex)
        BeginID(1) = LBOUND(A, DIM=I, KIND=kIndex)
        IF (OldSize(1) < 1_kIndex) ZeroSize = TrueVal
        AMold => A(BeginID(1))
    RANK (2)
        DO I = 1_kIndex, 2_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
        AMold => A(BeginID(1), BeginID(2))
    RANK (3)
        DO I = 1_kIndex, 3_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
        AMold => A(BeginID(1), BeginID(2), BeginID(3))
    RANK (4)
        DO I = 1_kIndex, 4_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
        AMold => A(BeginID(1), BeginID(2), BeginID(3), BeginID(4))
    RANK (5)
        DO I = 1_kIndex, 5_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
        AMold => A(BeginID(1), BeginID(2), BeginID(3), BeginID(4), BeginID(5))
    RANK (6)
        DO I = 1_kIndex, 6_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
        AMold => A(BeginID(1), BeginID(2), BeginID(3), BeginID(4), BeginID(5), BeginID(6))
    RANK (7)
        DO I = 1_kIndex, 4_kIndex
            OldSize(I) = SIZE(A, DIM=I, KIND=kIndex)
            BeginID(I) = LBOUND(A, DIM=I, KIND=kIndex)
            IF (OldSize(I) < 1_kIndex) ZeroSize = TrueVal
        END DO
        AMold => A(BeginID(1), BeginID(2), BeginID(3), BeginID(4), BeginID(5), BeginID(6), BeginID(7))
    RANK DEFAULT
        Handled = FalseVal
    END SELECT

    IF (ZeroSize) THEN
        ! one or more of A's dimensions has zero size; therefore, simply allocate A to new sizes
        CALL MemAllocPtr(A, AMold, NewSize, BeginID, NoDeAlloc=TrueVal)
    ELSE
        Handled = TrueVal
        EndID = BeginID + NewSize - 1_kIndex
        SELECT RANK (Asc => A)
        RANK (1)
            BLOCK
                CLASS(*), POINTER   :: Temp(:)
                tLogical            :: Success
                ! first, set pointer to input array
                Temp => Asc
                ! allocate the input array to the new size
                CALL MemAllocPtr(A, AMold, NewSize, BeginID, NoDeAlloc=TrueVal)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(Temp(BeginID(1):EndID(1)), Asc(BeginID(1):EndID(1)))
                ! free up memory
                DEALLOCATE(Temp)
                IF (.NOT.Success) THEN
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                END IF
            END BLOCK
        RANK (2)
            BLOCK
                CLASS(*), POINTER   :: Temp(:,:)
                tLogical            :: Success
                ! first, set pointer to input array
                ! first, set pointer to input array
                Temp => Asc
                ! allocate the input array to the new size
                CALL MemAllocPtr(A, AMold, NewSize, BeginID, NoDeAlloc=TrueVal)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2)), &
                                           Asc(BeginID(1):EndID(1),BeginID(2):EndID(2)))
                ! free up memory
                DEALLOCATE(Temp)
                IF (.NOT.Success) THEN
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                END IF
            END BLOCK
        RANK (3)
            BLOCK
                CLASS(*), POINTER   :: Temp(:,:,:)
                tLogical            :: Success
                ! first, set pointer to input array
                Temp => Asc
                ! allocate the input array to the new size
                CALL MemAllocPtr(A, AMold, NewSize, BeginID, NoDeAlloc=TrueVal)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3)), &
                                           Asc(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3)))
                ! free up memory
                DEALLOCATE(Temp)
                IF (.NOT.Success) THEN
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                END IF
            END BLOCK
        RANK (4)
            BLOCK
                CLASS(*), POINTER   :: Temp(:,:,:,:)
                tLogical            :: Success
                ! first, set pointer to input array
                Temp => Asc
                ! allocate the input array to the new size
                CALL MemAllocPtr(A, AMold, NewSize, BeginID, NoDeAlloc=TrueVal)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2), &
                                                BeginID(3):EndID(3),BeginID(4):EndID(4)), &
                                            Asc(BeginID(1):EndID(1),BeginID(2):EndID(2), &
                                                BeginID(3):EndID(3),BeginID(4):EndID(4)))
                ! free up memory
                DEALLOCATE(Temp)
                IF (.NOT.Success) THEN
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                END IF
            END BLOCK
        RANK (5)
            BLOCK
                CLASS(*), POINTER   :: Temp(:,:,:,:,:)
                tLogical            :: Success
                ! first, set pointer to input array
                Temp => Asc
                ! allocate the input array to the new size
                CALL MemAllocPtr(A, AMold, NewSize, BeginID, NoDeAlloc=TrueVal)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                                BeginID(4):EndID(4),BeginID(5):EndID(5)), &
                                            Asc(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                                BeginID(4):EndID(4),BeginID(5):EndID(5)))
                ! free up memory
                DEALLOCATE(Temp)
                IF (.NOT.Success) THEN
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                END IF
            END BLOCK
        RANK (6)
            BLOCK
                CLASS(*), POINTER   :: Temp(:,:,:,:,:,:)
                tLogical            :: Success
                ! first, set pointer to input array
                Temp => Asc
                ! allocate the input array to the new size
                CALL MemAllocPtr(A, AMold, NewSize, BeginID, NoDeAlloc=TrueVal)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                                BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6)), &
                                            Asc(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                                BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6)))
                ! free up memory
                DEALLOCATE(Temp)
                IF (.NOT.Success) THEN
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                END IF
            END BLOCK
        RANK (7)
            BLOCK
                CLASS(*), POINTER   :: Temp(:,:,:,:,:,:,:)
                tLogical            :: Success
                ! first, set pointer to input array
                Temp => Asc
                ! allocate the input array to the new size
                CALL MemAllocPtr(A, AMold, NewSize, BeginID, NoDeAlloc=TrueVal)
                ! determine the preserving sizes
                DO I = 1_kIndex, ARank
                    IF (NewSize(I) >= OldSize(I)) THEN
                        PSize(I) = OldSize(I)
                    ELSE
                        PSize(I) = NewSize(I)
                    END IF
                END DO
                ! compute ending indices
                EndID = PSize + BeginID - 1_kIndex
                ! copy data to the temporary array
                Success = AnyType_CopyData(Temp(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                                BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6), &
                                                BeginID(7):EndID(7)), &
                                            Asc(BeginID(1):EndID(1),BeginID(2):EndID(2),BeginID(3):EndID(3), &
                                                BeginID(4):EndID(4),BeginID(5):EndID(5),BeginID(6):EndID(6), &
                                                BeginID(7):EndID(7)))
                ! free up memory
                DEALLOCATE(Temp)
                IF (.NOT.Success) THEN
                    ! report error
                    CALL Handle_ErrLevel('AnyType_MemResize_Pointer', ModName, ErrSevere, &
                        'Cannot make a copy of data.')
                END IF
            END BLOCK
        RANK DEFAULT
            Handled = FalseVal
        END SELECT
        IF (.NOT.Handled) THEN
            CALL Handle_ErrLevel('AnyType_MemResize_Pointer', ModName, ErrSevere, &
                'Currently, only handle an entity with the rank between 1 and 7.')
        END IF
    END IF

    ! free local pointer
    NULLIFY(AMold)

    RETURN

END SUBROUTINE AnyType_MemResize_Pointer

!******************************************************************************

END MODULE MBase_AnyRankNType

!******************************************************************************
