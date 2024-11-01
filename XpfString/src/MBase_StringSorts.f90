
MODULE MBase_StringSorts

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains sorting and ranking routines specialized for strings.
!   The specified array can be of a *CHARACTER* or a *FvlStr* type. <br>

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ChrStr,               ONLY: ChangeCase
    USE MClass_Comparable
    USE MClass_FvlStr
    USE MClass_Alphabets,           ONLY: Alphabet => BaseAlphabet
#ifdef Indx64Bits
    USE MBase_DoublyLinkedLists,    ONLY: StackInteger => ListInteger8B
#else
    USE MBase_DoublyLinkedLists,    ONLY: StackInteger => ListInteger4B
#endif

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! sorting and ranking of case-insensitive strings
    PUBLIC :: Rank_Insensitive
    PUBLIC :: SortAscend_Insensitive
    PUBLIC :: SortDescend_Insensitive
    ! specialized sorting in an ascending order
    PUBLIC :: LSDSort
    PUBLIC :: MSDSort
    PUBLIC :: MSDInplaceSort
    PUBLIC :: QuickMultiKeySort
    PUBLIC :: AmericanFlagSort

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! number of total alphabets for extended ASCII character set
    tIndex, PARAMETER   :: Radix = 256_kIndex

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! public interfaces
    INTERFACE Rank_Insensitive
        !^ **Subroutine Interface**: Rank_Insensitive <br>
        !  **Purpose**:  To rank an array of strings in an ascending order using the specified
        !       ranking procedure where the strings are treated as case-insensitive strings.
        !       An applicable user-supplied procedure or any applicable ranking procedure (e.g.
        !       Wise_RankChar, PDQ_RankChar, QuickMo3_RankComp, MergeHalfCopy_RankComp, etc.) in
        !       the <a href="../module/mbase_ranking.html">MBase_Ranking</a> module can be
        !       used as an argument. <br>
        !  **Usage**: <br>
        !   ! rank an array of Fortran intrinsic character strings <br>
        !   --->    CALL Rank_Insensitive(cStrArr, cStrInd, Wise_RankChar) <br>
        !   ! rank an array of FvlStr objects <br>
        !   --->    CALL Rank_Insensitive(vStrArr, vStrInd, Wise_RankComp) <br>
        !  **Note**: <br>
        !   The *Wise_RankChar* and *Wise_RankComp* are examples of applicable ranking procedures
        !   for Fortran intrinsic character strings and FvlStr objects, respectively. <br>
        MODULE PROCEDURE ChrStr_RankInsensitive
        MODULE PROCEDURE FvlStr_RankInsensitive
    END INTERFACE
    INTERFACE SortAscend_Insensitive
        !^ **Subroutine Interface**: SortAscend_Insensitive <br>
        !  **Purpose**:  To sort an array of strings in an ascending order using the specified
        !       ranking procedure where the strings are treated as case-insensitive strings.
        !       An applicable user-supplied procedure or any applicable ranking procedure (e.g.
        !       Wise_RankChar, PDQ_RankChar, QuickMo3_RankComp, MergeHalfCopy_RankComp, etc.) in
        !       the <a href="../module/mbase_ranking.html">MBase_Ranking</a> module can be
        !       used as an argument. <br>
        !  **Usage**: <br>
        !   ! sort an array of Fortran intrinsic character strings <br>
        !   --->    CALL SortAscend_Insensitive(cStrArr, cStrInd, Wise_RankChar) <br>
        !   ! sort an array of FvlStr objects <br>
        !   --->    CALL SortAscend_Insensitive(vStrArr, vStrInd, Wise_RankComp) <br>
        !  **Note**: <br>
        !   The *Wise_RankChar* and *Wise_RankComp* are examples of applicable ranking procedures
        !   for Fortran intrinsic character strings and FvlStr objects, respectively. <br>
        MODULE PROCEDURE ChrStr_SortInsensitive_Ascend
        MODULE PROCEDURE FvlStr_SortInsensitive_Ascend
    END INTERFACE
    INTERFACE SortDescend_Insensitive
        !^ **Subroutine Interface**: SortDescend_Insensitive <br>
        !  **Purpose**:  To sort an array of strings in a descending order using the specified
        !       ranking procedure where the strings are treated as case-insensitive strings.
        !       An applicable user-supplied procedure or any applicable ranking procedure (e.g.
        !       Wise_RankChar, PDQ_RankChar, QuickMo3_RankComp, MergeHalfCopy_RankComp, etc.) in
        !       the <a href="../module/mbase_ranking.html">MBase_Ranking</a> module can be
        !       used as an argument. <br>
        !  **Usage**: <br>
        !   ! sort an array of Fortran intrinsic character strings <br>
        !   --->    CALL SortDescend_Insensitive(cStrArr, cStrInd, Wise_RankChar) <br>
        !   ! sort an array of FvlStr objects <br>
        !   --->    CALL SortDescend_Insensitive(vStrArr, vStrInd, Wise_RankComp) <br>
        !  **Note**: <br>
        !   The *Wise_RankChar* and *Wise_RankComp* are examples of applicable ranking procedures
        !   for Fortran intrinsic character strings and FvlStr objects, respectively. <br>
        MODULE PROCEDURE ChrStr_SortInsensitive_Descend
        MODULE PROCEDURE FvlStr_SortInsensitive_Descend
    END INTERFACE
    INTERFACE LSDSort
        !^ **Subroutine Interface**: LSDSort <br>
        !  **Purpose**:  To sort an array of strings in an ascending order using the LSD
        !       string sorting algorithm. <br>
        !  **Usage**: <br>
        !   --->    CALL LSDSort(cStrArr) <br>
        !  **Note**: <br>
        !   This procedure is only applicable for Fortran character strings. <br>
        MODULE PROCEDURE ChrStr_LSDSort
    END INTERFACE
    INTERFACE MSDSort
        !^ **Subroutine Interface**: MSDSort <br>
        !  **Purpose**:  To sort an array of strings in an ascending order using the MSD
        !       string sorting algorithm. <br>
        !  **Usage**: <br>
        !   --->    CALL MSDSort(Arr) <br>
        MODULE PROCEDURE ChrStr_MSDSort
        MODULE PROCEDURE FvlStr_MSDSort
    END INTERFACE
    INTERFACE MSDInplaceSort
        !^ **Subroutine Interface**: MSDInplaceSort <br>
        !  **Purpose**:  To sort an array of strings in an ascending order using the
        !       in-place MSD string sorting algorithm.  Optionally, if all characters
        !       of the strings are in a particular alphabet set, a user can specify an
        !       *Alphabet* data type of that particular alphabet set. <br>
        !  **Usage**: <br>
        !   --->    CALL MSDInplaceSort(Arr) <br>
        !   --->    CALL MSDInplaceSort(Arr, Alphabet) <br>
        !  **Usage**: See the <a href="../module/mclass_alphabets.html">Class_Alphabets</a>
        !       module for various *alphabet* data types available to be used with the
        !       *MSDInplaceSort* procedure.
        MODULE PROCEDURE ChrStr_MSDSort_Inplace
        MODULE PROCEDURE FvlStr_MSDSort_Inplace
        MODULE PROCEDURE ChrStr_MSDSort_Alphabet
        MODULE PROCEDURE FvlStr_MSDSort_Alphabet
    END INTERFACE
    INTERFACE QuickMultiKeySort
        !^ **Subroutine Interface**: QuickMultiKeySort <br>
        !  **Purpose**:  To sort an array of strings in an ascending order using the
        !       multi-key quicksort (or 3-way radix quicksort) algorithm. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickMultiKeySort(Arr) <br>
        MODULE PROCEDURE ChrStr_QuickMultiKeySort
        MODULE PROCEDURE FvlStr_QuickMultiKeySort
    END INTERFACE
    INTERFACE AmericanFlagSort
        !^ **Subroutine Interface**: AmericanFlagSort <br>
        !  **Purpose**:  To sort an array of strings in an ascending order using the
        !       American-flag algorithm. <br>
        !  **Usage**: <br>
        !   --->    CALL AmericanFlagSort(Arr) <br>
        MODULE PROCEDURE ChrStr_AmericanFlagSort
        MODULE PROCEDURE FvlStr_AmericanFlagSort
    END INTERFACE

    ! private interfaces
    ABSTRACT INTERFACE
        SUBROUTINE ChrStrRank(AVal, AInd)
            IMPORT
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        SUBROUTINE ComparableRank(AVal, AInd)
            IMPORT
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE InsertionSort
        MODULE PROCEDURE ChrStr_InsertionSort
        MODULE PROCEDURE FvlStr_InsertionSort
    END INTERFACE
    INTERFACE IsLessThan
        MODULE PROCEDURE ChrStr_IsLessThan
        MODULE PROCEDURE FvlStr_IsLessThan
    END INTERFACE
    INTERFACE CharAt
        MODULE PROCEDURE ChrStr_CharAt
        MODULE PROCEDURE FvlStr_CharAt
        MODULE PROCEDURE ChrStr_CharAt_Alphabet
        MODULE PROCEDURE FvlStr_CharAt_Alphabet
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!-----          CASE-INSENSITIVE RANKING AND SORTING PROCEDURES           -----
!------------------------------------------------------------------------------

SUBROUTINE ChrStr_RankInsensitive(AVal, AInd, RankArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform data ranking for an array of character strings where
    !  the character case is ignored.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)       :: AVal(:)          !! array values to be ranked
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) !! indices indicating the ranking
    PROCEDURE(ChrStrRank)       :: RankArray        !! procedure to perform ranking

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: UCVal(:)     ! upper-case array values
    tIndex      :: NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(AVal, KIND=kIndex)

    ! check input validity
    IF (NA <= 1_kIndex) THEN
        ! set indices and return
        AInd = 1_kIndex
        RETURN
    END IF

    ! allocate and make a copy of the specified array
    ALLOCATE(UCVal, SOURCE=AVal)

    ! change working array to upper-case values
    CALL ChangeCase(UCVal, TrueVal)

    ! determine indices
    CALL RankArray(UCVal, AInd)

    ! free memory
    DEALLOCATE(UCVal)

    RETURN

END SUBROUTINE ChrStr_RankInsensitive

!******************************************************************************

SUBROUTINE ChrStr_SortInsensitive_Ascend(AVal, RankArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform data sorting in ascending order for an array of
    !  character strings where the character case is ignored.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(INOUT)    :: AVal(:)      !! array values to be ranked
    PROCEDURE(ChrStrRank)       :: RankArray    !! procedure to perform ranking

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: UCVal(:)         ! upper-case array values
    tIndex      :: AInd(SIZE(AVal)) ! indices indicating the ranking
    tIndex      :: NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(AVal, KIND=kIndex)

    ! check input validity
    IF (NA <= 1_kIndex) RETURN

    ! determine the ranking indices
    CALL ChrStr_RankInsensitive(AVal, AInd, RankArray)

    ! allocate and make a copy of the given array
    ALLOCATE(UCVal, SOURCE=AVal)

    ! sort the array in ascending order using the indices
    AVal = UCVal(AInd(:))

    ! free memory
    DEALLOCATE(UCVal)

    RETURN

END SUBROUTINE ChrStr_SortInsensitive_Ascend

!******************************************************************************

SUBROUTINE ChrStr_SortInsensitive_Descend(AVal, RankArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform data sorting in descending order for an array of
    !  character strings where the character case is ignored.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(INOUT)    :: AVal(:)      !! array values to be ranked
    PROCEDURE(ChrStrRank)       :: RankArray    !! procedure to perform ranking

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: UCVal(:)         ! upper-case array values
    tIndex      :: AInd(SIZE(AVal)) ! indices indicating the ranking
    tIndex      :: I, NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(AVal, KIND=kIndex)

    ! check input validity
    IF (NA <= 1_kIndex) RETURN

    ! determine the ranking indices
    CALL ChrStr_RankInsensitive(AVal, AInd, RankArray)

    ! allocate and make a copy of the given array
    ALLOCATE(UCVal, SOURCE=AVal)

    ! sort the array in descending order using the indices
    DO I = 1_kIndex, NA
        AVal(I) = UCVal(AInd(NA-I+1))
    END DO

    ! free memory
    DEALLOCATE(UCVal)

    RETURN

END SUBROUTINE ChrStr_SortInsensitive_Descend

!******************************************************************************

SUBROUTINE FvlStr_RankInsensitive(AVal, AInd, RankArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform data ranking for an array of FvlStr objects where
    !  the character case is ignored.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: AVal(:)          !! array values to be ranked
    tIndex,       INTENT(INOUT) :: AInd(SIZE(AVal)) !! indices indicating the ranking
    PROCEDURE(ComparableRank)   :: RankArray        !! procedure to perform ranking

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(Comparable), ALLOCATABLE  :: UCVal(:)     ! upper-case array values
    tIndex                          :: NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(AVal, KIND=kIndex)

    ! check input validity
    IF (NA <= 1_kIndex) THEN
        ! set indices and return
        AInd = 1_kIndex
        RETURN
    END IF

    ! allocate and make a copy of the specified array
    ALLOCATE(UCVal, SOURCE=AVal)

    ! change working array to upper-case values
    SELECT TYPE (UCVal)
    TYPE IS (FvlStr)
        CALL UCVal%ChangeCase(TrueVal)
    END SELECT

    ! determine indices
    CALL RankArray(UCVal, AInd)

    ! free memory
    DEALLOCATE(UCVal)

    RETURN

END SUBROUTINE FvlStr_RankInsensitive

!******************************************************************************

SUBROUTINE FvlStr_SortInsensitive_Ascend(AVal, RankArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform data sorting in ascending order for an array of
    !  FvlStr objects where the character case is ignored.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(INOUT) :: AVal(:)      !! array values to be ranked
    PROCEDURE(ComparableRank)   :: RankArray    !! procedure to perform ranking

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(FvlStr)    :: UCVal(SIZE(AVal))    ! upper-case array values
    tIndex          :: AInd(SIZE(AVal))     ! indices indicating the ranking
    tIndex          :: NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(AVal, KIND=kIndex)

    ! check input validity
    IF (NA <= 1_kIndex) RETURN

    ! determine the ranking indices
    CALL FvlStr_RankInsensitive(AVal, AInd, RankArray)

    ! copy the given array
    UCVal = AVal

    ! sort the array in ascending order using the indices
    AVal = UCVal(AInd(:))

    RETURN

END SUBROUTINE FvlStr_SortInsensitive_Ascend

!******************************************************************************

SUBROUTINE FvlStr_SortInsensitive_Descend(AVal, RankArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform data sorting in descending order for an array of
    !  FvlStr objects where the character case is ignored.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(INOUT) :: AVal(:)      !! array values to be ranked
    PROCEDURE(ComparableRank)   :: RankArray    !! procedure to perform ranking

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(FvlStr)    :: UCVal(SIZE(AVal))    ! upper-case array values
    tIndex          :: AInd(SIZE(AVal))     ! indices indicating the ranking
    tIndex          :: I, NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(AVal, KIND=kIndex)

    ! check input validity
    IF (NA <= 1_kIndex) RETURN

    ! determine the ranking indices
    CALL FvlStr_RankInsensitive(AVal, AInd, RankArray)

    ! copy the given array
    UCVal = AVal

    ! sort the array in descending order using the indices
    DO I = 1_kIndex, NA
        AVal(I) = UCVal(AInd(NA-I+1))
    END DO

    RETURN

END SUBROUTINE FvlStr_SortInsensitive_Descend

!------------------------------------------------------------------------------
!-----          SPECIALIZED SORTING PROCEDURES FOR STRINGS                -----
!------------------------------------------------------------------------------

SUBROUTINE ChrStr_LSDSort(A)

!** PURPOSE OF THIS SUBROUTINE:
!^  To sort an array of character strings in an ascending order using
!   the least-significant-digit (LSD) string sorting algorithm, which
!   is preferable for *same-length* strings. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] R. Sedgewick and K. Wayne. 2011.  Algorithms, 4th Edition.  Addison-Wesley. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(INOUT)    :: A(:) !! the array to be sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: NA, StrLen

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(A, KIND=kIndex)

    IF (NA <= 1_kIndex) RETURN

    ! get the fixed length of strings
    StrLen = LEN(A(1))

    ! perform sorting
    CALL LSD_Sort(A, NA, StrLen)

    RETURN

    CONTAINS

    SUBROUTINE LSD_Sort(AVal, N, W)

    !** PURPOSE OF THIS SUBROUTINE
	    !^ To sort an array of character strings using the LSD sorting algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(INOUT)    :: AVal(0:) !! the array of strings to be sorted
        tIndex,    INTENT(IN)       :: N        !! size of the array
        tIndex,    INTENT(IN)       :: W        !! width or length of each string element

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I, K, L          ! zero-based indices
        tIndex      :: J                ! one-based index
        tIndex      :: ICount(0:Radix)  ! counting array
        tCharLen(W) :: Buf(0:N-1)       ! auxiliary array

    !** FLOW

        ! initialize the counting array
        ICount = 0_kIndex

        DO J = W, 1_kIndex, -1_kIndex

            !------------------------------------------------------
            ! --- sort by key-indexed counting on Jth character ---
            !------------------------------------------------------

            ! compute frequency counts
            DO I = 0_kIndex, N-1_kIndex
                K = IACHAR(AVal(I)(J:J)) + 1_kIndex
                ICount(K) = ICount(K) + 1_kIndex
            END DO

            ! transform counts to indices
            DO L = 0_kIndex, Radix-1_kIndex
                ICount(L+1) = ICount(L+1) + ICount(L)
            END DO

            ! distribute the data
            DO I = 0_kIndex, N-1_kIndex
                K = IACHAR(AVal(I)(J:J))
                L = ICount(K)
                Buf(L) = AVal(I)
                ICount(K) = L + 1_kIndex
            END DO

            ! copy the data back
            AVal = Buf
        END DO

        RETURN

    END SUBROUTINE LSD_Sort

    !**************************************************************************

END SUBROUTINE ChrStr_LSDSort

!******************************************************************************

SUBROUTINE ChrStr_MSDSort(A)

!** PURPOSE OF THIS SUBROUTINE:
!^  To sort an array of character strings in an ascending order using
!   the most-significant-digit (MSD) string sorting algorithm. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] R. Sedgewick and K. Wayne. 2011.  Algorithms, 4th Edition.  Addison-Wesley. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(INOUT)    :: A(:) !! the array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex, PARAMETER   :: Insertion_CutOff = 15_kIndex

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: NA
    tCharLen(LEN(A(1))) :: Aux(SIZE(A)) ! auxiliary array

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(A, KIND=kIndex)

    IF (NA <= 1_kIndex) RETURN

    ! perform sorting
    CALL MSD_Sort(A, 0_kIndex, NA-1_kIndex, 1_kIndex, Aux)

    RETURN

    CONTAINS

    RECURSIVE SUBROUTINE MSD_Sort(AVal, Lo, Hi, D, Buf)

    !** PURPOSE OF THIS SUBROUTINE
	    !^ To sort an array of character strings using the MSD sorting algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(INOUT)    :: AVal(0:) !! the array of strings to be sorted
        tIndex,    INTENT(IN)       :: Lo       !! starting zero-based index, inclusive
        tIndex,    INTENT(IN)       :: Hi       !! ending zero-based index, inclusive
        tIndex,    INTENT(IN)       :: D        !! starting one-based position in a string element
        tCharStar, INTENT(INOUT)    :: Buf(0:)  !! the auxiliary array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! cutoff to insertion sort for small sub-arrays
        IF (Hi <= Lo + Insertion_CutOff) THEN
            CALL InsertionSort(AVal, Lo, Hi, D)
            RETURN
        END IF

        BLOCK
            tIndex      :: I, K, L              ! zero-based indices
            tIndex      :: J                    ! one-based index
            tIndex      :: ICount(0:Radix+1)    ! counting array

            ! initialize the counting array
            ICount = 0_kIndex
            ! compute frequency counts
            DO I = Lo, Hi
                K = CharAt(AVal(I), D)
                ICount(K+2) = ICount(K+2) + 1_kIndex
            END DO
            ! transform counts to indices
            DO L = 0_kIndex, Radix
                ICount(L+1) = ICount(L+1) + ICount(L)
            END DO
            ! distribute the data
            DO I = Lo, Hi
                K = CharAt(AVal(I), D)
                L = ICount(K+1)
                Buf(L) = AVal(I)
                ICount(K+1) = L + 1_kIndex
            END DO
            ! copy the data back
            DO I = Lo, HI
                AVal(I) = Buf(I - Lo)
            END DO

            ! recursively sort for each character (excludes sentinel -1)
            DO L = 0_kIndex, Radix - 1_kIndex
                CALL MSD_Sort(AVal, Lo+ICount(L), Lo+ICount(L+1)-1_kIndex, D+1_kIndex, Buf)
            END DO
        END BLOCK

        RETURN

    END SUBROUTINE MSD_Sort

    !**************************************************************************

END SUBROUTINE ChrStr_MSDSort

!******************************************************************************

SUBROUTINE FvlStr_MSDSort(A)

!** PURPOSE OF THIS SUBROUTINE:
!^  To sort an array of FvlStr objects in an ascending order using
!   the most-significant-digit (MSD) string sorting algorithm. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] R. Sedgewick and K. Wayne. 2011.  Algorithms, 4th Edition.  Addison-Wesley. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(INOUT) :: A(:) !! the array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex, PARAMETER   :: Insertion_CutOff = 15_kIndex

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: NA
    TYPE(FvlStr)    :: Aux(SIZE(A)) ! auxiliary array

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(A, KIND=kIndex)

    IF (NA <= 1_kIndex) RETURN

    ! perform sorting
    CALL MSD_Sort(A, 0_kIndex, NA-1_kIndex, 1_kIndex, Aux)

    RETURN

    CONTAINS

    RECURSIVE SUBROUTINE MSD_Sort(AVal, Lo, Hi, D, Buf)

    !** PURPOSE OF THIS SUBROUTINE
	    !^ To sort an array of character strings using the MSD sorting algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(FvlStr), INTENT(INOUT) :: AVal(0:) !! the array of strings to be sorted
        tIndex,       INTENT(IN)    :: Lo       !! starting zero-based index, inclusive
        tIndex,       INTENT(IN)    :: Hi       !! ending zero-based index, inclusive
        tIndex,       INTENT(IN)    :: D        !! starting one-based position in a string element
        TYPE(FvlStr), INTENT(INOUT) :: Buf(0:)  !! the auxiliary array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! cutoff to insertion sort for small sub-arrays
        IF (Hi <= Lo + Insertion_CutOff) THEN
            CALL InsertionSort(AVal, Lo, Hi, D)
            RETURN
        END IF

        BLOCK
            tIndex      :: I, K, L              ! zero-based indices
            tIndex      :: J                    ! one-based index
            tIndex      :: ICount(0:Radix+1)    ! counting array

            ! initialize the counting array
            ICount = 0_kIndex
            ! compute frequency counts
            DO I = Lo, Hi
                K = CharAt(AVal(I), D)
                ICount(K+2) = ICount(K+2) + 1_kIndex
            END DO
            ! transform counts to indices
            DO L = 0_kIndex, Radix
                ICount(L+1) = ICount(L+1) + ICount(L)
            END DO
            ! distribute the data
            DO I = Lo, Hi
                K = CharAt(AVal(I), D)
                L = ICount(K+1)
                Buf(L) = AVal(I)
                ICount(K+1) = L + 1_kIndex
            END DO
            ! copy the data back
            DO I = Lo, HI
                AVal(I) = Buf(I - Lo)
            END DO

            ! recursively sort for each character (excludes sentinel -1)
            DO L = 0_kIndex, Radix - 1_kIndex
                CALL MSD_Sort(AVal, Lo+ICount(L), Lo+ICount(L+1)-1_kIndex, D+1_kIndex, Buf)
            END DO
        END BLOCK

        RETURN

    END SUBROUTINE MSD_Sort

    !**************************************************************************

END SUBROUTINE FvlStr_MSDSort

!******************************************************************************

SUBROUTINE ChrStr_MSDSort_Inplace(A)

!** PURPOSE OF THIS SUBROUTINE:
!^  To sort an array of character strings in an ascending order using
!   the in-place MSD radix sorting algorithm. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] R. Sedgewick and K. Wayne. 2011.  Algorithms, 4th Edition.  Addison-Wesley. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(INOUT)    :: A(:) !! the array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex, PARAMETER   :: Insertion_CutOff = 15_kIndex

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(A, KIND=kIndex)

    IF (NA <= 1_kIndex) RETURN

    ! perform sorting
    CALL MSD_Sort(A, 0_kIndex, NA-1_kIndex, 1_kIndex)

    RETURN

    CONTAINS

    RECURSIVE SUBROUTINE MSD_Sort(AVal, Lo, Hi, D)

    !** PURPOSE OF THIS SUBROUTINE
	    !^ To sort an array of character strings using the MSD sorting algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(INOUT)    :: AVal(0:) !! the array of strings to be sorted
        tIndex,    INTENT(IN)       :: Lo       !! starting zero-based index, inclusive
        tIndex,    INTENT(IN)       :: Hi       !! ending zero-based index, inclusive
        tIndex,    INTENT(IN)       :: D        !! starting one-based position in a string element

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! cutoff to insertion sort for small sub-arrays
        IF (Hi <= Lo + Insertion_CutOff) THEN
            CALL InsertionSort(AVal, Lo, Hi, D)
            RETURN
        END IF

        BLOCK
            tIndex                  :: I, K, L          ! zero-based indices
            tIndex                  :: J                ! one-based index
            tIndex                  :: IHead(0:Radix+1) ! index array
            tIndex                  :: ITail(0:Radix)   ! index array
            tCharLen(LEN(AVal(0)))  :: Temp             ! temporary variable

            ! initialize the counting array
            IHead = 0_kIndex
            ITail = 0_kIndex
            ! compute frequency counts
            DO I = Lo, Hi
                K = CharAt(AVal(I), D)
                IHead(K+2) = IHead(K+2) + 1_kIndex
            END DO
            ! transform counts to indices
            IHead(0) = Lo
            DO L = 0_kIndex, Radix
                IHead(L+1) = IHead(L+1) + IHead(L)
                ITail(L)   = IHead(L+1)
            END DO
            ! sort by d-th character in-place
            DO I = 0_kIndex, Radix
                DO WHILE (IHead(I) < ITail(I))
                    K = CharAt(AVal(IHead(I)), D)
                    DO WHILE ((K+1_kIndex) /= I)
                        EXCHANGE(AVal, IHead(I), IHead(K+1))
                        IHead(K+1) = IHead(K+1) + 1_kIndex
                        K = CharAt(AVal(IHead(I)), D)
                    END DO
                    IHead(I) = IHead(I) + 1_kIndex
                END DO
            END DO
            ! recursively sort for each character (excludes sentinel -1)
            DO L = 0_kIndex, Radix - 1_kIndex
                CALL MSD_Sort(AVal, ITail(L), ITail(L+1)-1_kIndex, D+1_kIndex)
            END DO
        END BLOCK

        RETURN

    END SUBROUTINE MSD_Sort

    !**************************************************************************

END SUBROUTINE ChrStr_MSDSort_Inplace

!******************************************************************************

SUBROUTINE FvlStr_MSDSort_Inplace(A)

!** PURPOSE OF THIS SUBROUTINE:
!^  To sort an array of FvlStr objects in an ascending order using
!   the in-place MSD radix sorting algorithm. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] R. Sedgewick and K. Wayne. 2011.  Algorithms, 4th Edition.  Addison-Wesley. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(INOUT) :: A(:) !! the array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex, PARAMETER   :: Insertion_CutOff = 15_kIndex

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(A, KIND=kIndex)

    IF (NA <= 1_kIndex) RETURN

    ! perform sorting
    CALL MSD_Sort(A, 0_kIndex, NA-1_kIndex, 1_kIndex)

    RETURN

    CONTAINS

    RECURSIVE SUBROUTINE MSD_Sort(AVal, Lo, Hi, D)

    !** PURPOSE OF THIS SUBROUTINE
	    !^ To sort an array of character strings using the MSD sorting algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(FvlStr), INTENT(INOUT) :: AVal(0:) !! the array of strings to be sorted
        tIndex,       INTENT(IN)    :: Lo       !! starting zero-based index, inclusive
        tIndex,       INTENT(IN)    :: Hi       !! ending zero-based index, inclusive
        tIndex,       INTENT(IN)    :: D        !! starting one-based position in a string element

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! cutoff to insertion sort for small sub-arrays
        IF (Hi <= Lo + Insertion_CutOff) THEN
            CALL InsertionSort(AVal, Lo, Hi, D)
            RETURN
        END IF

        BLOCK
            tIndex      :: I, K, L          ! zero-based indices
            tIndex      :: J                ! one-based index
            tIndex      :: IHead(0:Radix+1) ! index array
            tIndex      :: ITail(0:Radix)   ! index array

            ! initialize the counting array
            IHead = 0_kIndex
            ITail = 0_kIndex
            ! compute frequency counts
            DO I = Lo, Hi
                K = CharAt(AVal(I), D)
                IHead(K+2) = IHead(K+2) + 1_kIndex
            END DO
            ! transform counts to indices
            IHead(0) = Lo
            DO L = 0_kIndex, Radix
                IHead(L+1) = IHead(L+1) + IHead(L)
                ITail(L)   = IHead(L+1)
            END DO
            ! sort by d-th character in-place
            DO I = 0_kIndex, Radix
                DO WHILE (IHead(I) < ITail(I))
                    K = CharAt(AVal(IHead(I)), D)
                    DO WHILE ((K+1_kIndex) /= I)
                        CALL Swap(AVal(IHead(I)), AVal(IHead(K+1)))
                        IHead(K+1) = IHead(K+1) + 1_kIndex
                        K = CharAt(AVal(IHead(I)), D)
                    END DO
                    IHead(I) = IHead(I) + 1_kIndex
                END DO
            END DO
            ! recursively sort for each character (excludes sentinel -1)
            DO L = 0_kIndex, Radix - 1_kIndex
                CALL MSD_Sort(AVal, ITail(L), ITail(L+1)-1_kIndex, D+1_kIndex)
            END DO
        END BLOCK

        RETURN

    END SUBROUTINE MSD_Sort

    !**************************************************************************

END SUBROUTINE FvlStr_MSDSort_Inplace

!******************************************************************************

SUBROUTINE ChrStr_MSDSort_Alphabet(A, Alpha)

!** PURPOSE OF THIS SUBROUTINE:
!^  To sort an array of character strings (where all characters of the strings are
!   in a particular alphabet set of characters) in an ascending order using the
!   in-place MSD radix sorting algorithm.  <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] R. Sedgewick and K. Wayne. 2011.  Algorithms, 4th Edition.  Addison-Wesley. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,       INTENT(INOUT)  :: A(:)     !! the array to be sorted
    CLASS(Alphabet), INTENT(INOUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex, PARAMETER   :: Insertion_CutOff = 15_kIndex

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(A, KIND=kIndex)

    IF (NA <= 1_kIndex) RETURN

    ! perform sorting
    CALL MSD_Sort(A, 0_kIndex, NA-1_kIndex, 1_kIndex)

    RETURN

    CONTAINS

    RECURSIVE SUBROUTINE MSD_Sort(AVal, Lo, Hi, D)

    !** PURPOSE OF THIS SUBROUTINE
	    !^ To sort an array of character strings using the MSD sorting algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(INOUT)    :: AVal(0:) !! the array of strings to be sorted
        tIndex,    INTENT(IN)       :: Lo       !! starting zero-based index, inclusive
        tIndex,    INTENT(IN)       :: Hi       !! ending zero-based index, inclusive
        tIndex,    INTENT(IN)       :: D        !! starting one-based position in a string element

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! cutoff to insertion sort for small sub-arrays
        IF (Hi <= Lo + Insertion_CutOff) THEN
            CALL InsertionSort(AVal, Lo, Hi, D)
            RETURN
        END IF

        BLOCK
            tIndex                  :: I, K, L                      ! zero-based indices
            tIndex                  :: J                            ! one-based index
            tIndex                  :: IHead(0:Alpha%GetRadix()+1)  ! index array
            tIndex                  :: ITail(0:Alpha%GetRadix())    ! index array
            tCharLen(LEN(AVal(0)))  :: Temp                         ! temporary variable

            ! initialize the counting array
            IHead = 0_kIndex
            ITail = 0_kIndex
            ! compute frequency counts
            DO I = Lo, Hi
                K = CharAt(AVal(I), D, Alpha)
                IHead(K+2) = IHead(K+2) + 1_kIndex
            END DO
            ! transform counts to indices
            IHead(0) = Lo
            DO L = 0_kIndex, Alpha%GetRadix()
                IHead(L+1) = IHead(L+1) + IHead(L)
                ITail(L)   = IHead(L+1)
            END DO
            ! sort by d-th character in-place
            DO I = 0_kIndex, Alpha%GetRadix()
                DO WHILE (IHead(I) < ITail(I))
                    K = CharAt(AVal(IHead(I)), D, Alpha)
                    DO WHILE ((K+1_kIndex) /= I)
                        EXCHANGE(AVal, IHead(I), IHead(K+1))
                        IHead(K+1) = IHead(K+1) + 1_kIndex
                        K = CharAt(AVal(IHead(I)), D, Alpha)
                    END DO
                    IHead(I) = IHead(I) + 1_kIndex
                END DO
            END DO
            ! recursively sort for each character (excludes sentinel -1)
            DO L = 0_kIndex, Alpha%GetRadix() - 1_kIndex
                CALL MSD_Sort(AVal, ITail(L), ITail(L+1)-1_kIndex, D+1_kIndex)
            END DO
        END BLOCK

        RETURN

    END SUBROUTINE MSD_Sort

    !**************************************************************************

END SUBROUTINE ChrStr_MSDSort_Alphabet

!******************************************************************************

SUBROUTINE FvlStr_MSDSort_Alphabet(A, Alpha)

!** PURPOSE OF THIS SUBROUTINE:
!^  To sort an array of FvlStr objects (where all characters of their strings are
!   in a particular alphabet set of characters) in an ascending order using the
!   in-place MSD radix sorting algorithm.  <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] R. Sedgewick and K. Wayne. 2011.  Algorithms, 4th Edition.  Addison-Wesley. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr),    INTENT(INOUT)  :: A(:)     !! the array to be sorted
    CLASS(Alphabet), INTENT(INOUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex, PARAMETER   :: Insertion_CutOff = 15_kIndex

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(A, KIND=kIndex)

    IF (NA <= 1_kIndex) RETURN

    ! perform sorting
    CALL MSD_Sort(A, 0_kIndex, NA-1_kIndex, 1_kIndex)

    RETURN

    CONTAINS

    RECURSIVE SUBROUTINE MSD_Sort(AVal, Lo, Hi, D)

    !** PURPOSE OF THIS SUBROUTINE
	    !^ To sort an array of character strings using the MSD sorting algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(FvlStr), INTENT(INOUT) :: AVal(0:) !! the array of strings to be sorted
        tIndex,       INTENT(IN)    :: Lo       !! starting zero-based index, inclusive
        tIndex,       INTENT(IN)    :: Hi       !! ending zero-based index, inclusive
        tIndex,       INTENT(IN)    :: D        !! starting one-based position in a string element

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! cutoff to insertion sort for small sub-arrays
        IF (Hi <= Lo + Insertion_CutOff) THEN
            CALL InsertionSort(AVal, Lo, Hi, D)
            RETURN
        END IF

        BLOCK
            tIndex                  :: I, K, L                      ! zero-based indices
            tIndex                  :: J                            ! one-based index
            tIndex                  :: IHead(0:Alpha%GetRadix()+1)  ! index array
            tIndex                  :: ITail(0:Alpha%GetRadix())    ! index array

            ! initialize the counting array
            IHead = 0_kIndex
            ITail = 0_kIndex
            ! compute frequency counts
            DO I = Lo, Hi
                K = CharAt(AVal(I), D, Alpha)
                IHead(K+2) = IHead(K+2) + 1_kIndex
            END DO
            ! transform counts to indices
            IHead(0) = Lo
            DO L = 0_kIndex, Alpha%GetRadix()
                IHead(L+1) = IHead(L+1) + IHead(L)
                ITail(L)   = IHead(L+1)
            END DO
            ! sort by d-th character in-place
            DO I = 0_kIndex, Alpha%GetRadix()
                DO WHILE (IHead(I) < ITail(I))
                    K = CharAt(AVal(IHead(I)), D, Alpha)
                    DO WHILE ((K+1_kIndex) /= I)
                        CALL Swap(AVal(IHead(I)), AVal(IHead(K+1)))
                        IHead(K+1) = IHead(K+1) + 1_kIndex
                        K = CharAt(AVal(IHead(I)), D, Alpha)
                    END DO
                    IHead(I) = IHead(I) + 1_kIndex
                END DO
            END DO
            ! recursively sort for each character (excludes sentinel -1)
            DO L = 0_kIndex, Alpha%GetRadix() - 1_kIndex
                CALL MSD_Sort(AVal, ITail(L), ITail(L+1)-1_kIndex, D+1_kIndex)
            END DO
        END BLOCK

        RETURN

    END SUBROUTINE MSD_Sort

    !**************************************************************************

END SUBROUTINE FvlStr_MSDSort_Alphabet

!******************************************************************************

SUBROUTINE ChrStr_QuickMultiKeySort(A)

!** PURPOSE OF THIS SUBROUTINE:
!^  To sort an array of character strings in an ascending order using
!   the three-way radix quicksort (or multi-key quicksort) algorithm. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] R. Sedgewick and K. Wayne. 2011.  Algorithms, 4th Edition.  Addison-Wesley. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(INOUT)    :: A(:) !! the array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex, PARAMETER   :: Insertion_CutOff = 15_kIndex

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(A, KIND=kIndex)

    IF (NA <= 1_kIndex) RETURN

    ! perform sorting
    CALL Quick_MultiKey_Sort(A, 0_kIndex, NA-1_kIndex, 1_kIndex)

    RETURN

    CONTAINS

    RECURSIVE SUBROUTINE Quick_MultiKey_Sort(AVal, Lo, Hi, D)

    !** PURPOSE OF THIS SUBROUTINE
	    !^ To sort an array of character strings using the multi-key quicksort algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(INOUT)    :: AVal(0:) !! the array of strings to be sorted
        tIndex,    INTENT(IN)       :: Lo       !! starting zero-based index, inclusive
        tIndex,    INTENT(IN)       :: Hi       !! ending zero-based index, inclusive
        tIndex,    INTENT(IN)       :: D        !! starting one-based position in a string element

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! cutoff to insertion sort for small sub-arrays
        IF (Hi <= Lo + Insertion_CutOff) THEN
            CALL InsertionSort(AVal, Lo, Hi, D)
            RETURN
        END IF

        BLOCK
            tIndex                  :: LT, GT, I, J, K
            tCharLen(LEN(AVal(0)))  :: Temp

            ! initialize
            LT = Lo
            Gt = Hi
            I = Lo + 1_kIndex
            J = CharAt(AVal(Lo), D)

            ! perform 3-way partitioning
            DO WHILE (I <= GT)
                K = CharAt(AVal(I), D)
                IF (K < J) THEN
                    EXCHANGE(AVal, LT, I)
                    LT = LT + 1_kIndex
                    I = I + 1_kIndex
                ELSEIF (K > J) THEN
                    EXCHANGE(AVal, I, GT)
                    GT = GT - 1_kIndex
                ELSE
                    I = I + 1_kIndex
                END IF
            END DO

            ! AVal[Lo..LT-1] < J = AVal[LT..GT] < AVal[GT+1..Hi]
            CALL Quick_MultiKey_Sort(AVal, Lo, LT-1_kIndex, D)
            IF (J >= 0_kIndex) CALL Quick_MultiKey_Sort(AVal, LT, GT, D+1_kIndex)
            CALL Quick_MultiKey_Sort(AVal, GT+1_kIndex, Hi, D)
        END BLOCK

        RETURN

    END SUBROUTINE Quick_MultiKey_Sort

    !**************************************************************************

END SUBROUTINE ChrStr_QuickMultiKeySort

!******************************************************************************

SUBROUTINE FvlStr_QuickMultiKeySort(A)

!** PURPOSE OF THIS SUBROUTINE:
!^  To sort an array of FvlStr objects in an ascending order using
!   the three-way radix quicksort (or multi-key quicksort) algorithm. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] R. Sedgewick and K. Wayne. 2011.  Algorithms, 4th Edition.  Addison-Wesley. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(INOUT) :: A(:) !! the array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex, PARAMETER   :: Insertion_CutOff = 15_kIndex

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(A, KIND=kIndex)

    IF (NA <= 1_kIndex) RETURN

    ! perform sorting
    CALL Quick_MultiKey_Sort(A, 0_kIndex, NA-1_kIndex, 1_kIndex)

    RETURN

    CONTAINS

    RECURSIVE SUBROUTINE Quick_MultiKey_Sort(AVal, Lo, Hi, D)

    !** PURPOSE OF THIS SUBROUTINE
	    !^ To sort an array of character strings using the multi-key quicksort algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(FvlStr), INTENT(INOUT) :: AVal(0:) !! the array of strings to be sorted
        tIndex,       INTENT(IN)    :: Lo       !! starting zero-based index, inclusive
        tIndex,       INTENT(IN)    :: Hi       !! ending zero-based index, inclusive
        tIndex,       INTENT(IN)    :: D        !! starting one-based position in a string element

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! cutoff to insertion sort for small sub-arrays
        IF (Hi <= Lo + Insertion_CutOff) THEN
            CALL InsertionSort(AVal, Lo, Hi, D)
            RETURN
        END IF

        BLOCK
            tIndex      :: LT, GT, I, J, K

            ! initialize
            LT = Lo
            Gt = Hi
            I = Lo + 1_kIndex
            J = CharAt(AVal(Lo), D)

            ! perform 3-way partitioning
            DO WHILE (I <= GT)
                K = CharAt(AVal(I), D)
                IF (K < J) THEN
                    CALL Swap(AVal(LT), AVal(I))
                    LT = LT + 1_kIndex
                    I = I + 1_kIndex
                ELSEIF (K > J) THEN
                    CALL Swap(AVal(I), AVal(GT))
                    GT = GT - 1_kIndex
                ELSE
                    I = I + 1_kIndex
                END IF
            END DO

            ! AVal[Lo..LT-1] < J = AVal[LT..GT] < AVal[GT+1..Hi]
            CALL Quick_MultiKey_Sort(AVal, Lo, (LT-1_kIndex), D)
            IF (J >= 0_kIndex) CALL Quick_MultiKey_Sort(AVal, LT, GT, (D+1_kIndex))
            CALL Quick_MultiKey_Sort(AVal, (GT+1_kIndex), Hi, D)
        END BLOCK

        RETURN

    END SUBROUTINE Quick_MultiKey_Sort

    !**************************************************************************

END SUBROUTINE FvlStr_QuickMultiKeySort

!******************************************************************************

SUBROUTINE ChrStr_AmericanFlagSort(A)

!** PURPOSE OF THIS SUBROUTINE:
!^  To sort an array of character strings in an ascending order using the American
!   flag algorithm. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://www.usenix.org/publications/compsystems/1993/win_mcilroy.pdf">
!       P.M. McIlroy and K. Bostic. 1993.  Engineering Radix Sort. Computer Systems,
!       Vol. 6, No. 1. </a> <br>
!   [2] <a href="https://algs4.cs.princeton.edu/code/edu/princeton/cs/algs4/AmericanFlagX.java.html">
!       Java Code: Non-recursive Americal Flag Sort. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(INOUT)    :: A(:) !! the array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex, PARAMETER   :: Insertion_CutOff = 15_kIndex

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(A, KIND=kIndex)

    IF (NA <= 1_kIndex) RETURN

    ! perform sorting
    CALL American_Flag_Sort(A, 0_kIndex, NA-1_kIndex)

    RETURN

    CONTAINS

    SUBROUTINE American_Flag_Sort(AVal, StartID, EndID)

    !** PURPOSE OF THIS SUBROUTINE
	    !^ To sort an array of character strings using the American flag algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(INOUT)    :: AVal(0:) !! the array of strings to be sorted
        tIndex,    INTENT(IN)       :: StartID  !! starting zero-based index, inclusive
        tIndex,    INTENT(IN)       :: EndID    !! ending zero-based index, inclusive

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(StackInteger)      :: Stack
        tIndex                  :: ICount(0:Radix)
        tIndex                  :: D        ! character index to sort by
        tIndex                  :: Lo       ! starting index
        tIndex                  :: Hi       ! ending index
        tIndex                  :: I        ! working index
        tLogical                :: Success
        tIndex                  :: C
        tCharLen(LEN(AVal(0)))  :: Temp

    !** FLOW

        ! initialize
        D = 0_kIndex
        Lo = StartID
        Hi = EndID
        ICount = 0_kIndex
        CALL Stack%Push(Lo)
        CALL Stack%Push(Hi)
        CALL Stack%Push(D)

        DO WHILE (.NOT.Stack%IsEmpty())
            Success = Stack%Pop(D)
            Success = Stack%Pop(Hi)
            Success = Stack%Pop(Lo)
            ! cutoff to insertion sort for small sub-arrays
            IF (Hi <= Lo + Insertion_CutOff) THEN
                CALL InsertionSort(AVal, Lo, Hi, D)
                CYCLE
            END IF
            ! compute frequency counts
            DO I = Lo, Hi
                C = CharAt(AVal(I), D) + 1_kIndex   ! account for -1 representing end-of-string
                ICount(C) = ICount(C) + 1_kIndex
            END DO
            ! accumulate counts relative to AVal(0), so that ICount(C) is the number of keys <= C
            ICount(0) = ICount(0) + Lo
            DO C = 0_kIndex, Radix-1_kIndex
                ICount(C+1) = ICount(C+1) + ICount(C)
                IF ((C > 0_kIndex).AND.((ICount(C+1)-1_kIndex) > ICount(C))) THEN
                    ! add subproblem for character C (excludes sentinel C == 0)
                    CALL Stack%Push(ICount(C))
                    CALL Stack%Push(ICount(C+1)-1_kIndex)
                    CALL Stack%Push(D+1_kIndex)
                END IF
            END DO
            ! permute data in place
            I = Hi
            DO
                ! locate element that must be shifted right of I
                C = CharAt(AVal(I), D) + 1_kIndex
                DO WHILE ((I >= Lo).AND.((ICount(C)-1_kIndex) <= I))
                    IF ((ICount(C)-1_kIndex) == I) ICount(C) = ICount(C) - 1_kIndex
                    I = I - 1_kIndex
                    IF (I >= Lo) C = CharAt(AVal(I), D) + 1_kIndex
                END DO
                ! if I < Lo the sub-array is sorted.
                IF (I < Lo) EXIT
                ! permute AVal(I) until correct element is in place
                ICount(C) = ICount(C) - 1_kIndex
                DO WHILE (ICount(C) /= I)
                    EXCHANGE(AVal, I, ICount(C))
                    C = CharAt(AVal(I), D) + 1_kIndex
                    ICount(C) = ICount(C) - 1_kIndex
                END DO
                I = I - 1_kIndex
                IF (I < Lo) EXIT
            END DO
            ! clear ICount array
            ICount = 0_kIndex
        END DO

        RETURN

    END SUBROUTINE American_Flag_Sort

    !**************************************************************************

END SUBROUTINE ChrStr_AmericanFlagSort

!******************************************************************************

SUBROUTINE FvlStr_AmericanFlagSort(A)

!** PURPOSE OF THIS SUBROUTINE:
!^  To sort an array of character strings in an ascending order using the American
!   flag algorithm. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://www.usenix.org/publications/compsystems/1993/win_mcilroy.pdf">
!       P.M. McIlroy and K. Bostic. 1993.  Engineering Radix Sort. Computer Systems,
!       Vol. 6, No. 1. </a> <br>
!   [2] <a href="https://algs4.cs.princeton.edu/code/edu/princeton/cs/algs4/AmericanFlagX.java.html">
!       Java Code: Non-recursive Americal Flag Sort. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(INOUT) :: A(:) !! the array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex, PARAMETER   :: Insertion_CutOff = 15_kIndex

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: NA

!** FLOW

    ! check and return quickly if we can
    NA = SIZE(A, KIND=kIndex)

    IF (NA <= 1_kIndex) RETURN

    ! perform sorting
    CALL American_Flag_Sort(A, 0_kIndex, NA-1_kIndex)

    RETURN

    CONTAINS

    SUBROUTINE American_Flag_Sort(AVal, StartID, EndID)

    !** PURPOSE OF THIS SUBROUTINE
	    !^ To sort an array of character strings using the American flag algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(FvlStr), INTENT(INOUT) :: AVal(0:) !! the array of strings to be sorted
        tIndex,       INTENT(IN)    :: StartID  !! starting zero-based index, inclusive
        tIndex,       INTENT(IN)    :: EndID    !! ending zero-based index, inclusive

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(StackInteger)      :: Stack
        tIndex                  :: ICount(0:Radix)
        tIndex                  :: D        ! character index to sort by
        tIndex                  :: Lo       ! starting index
        tIndex                  :: Hi       ! ending index
        tIndex                  :: I        ! working index
        tLogical                :: Success
        tIndex                  :: C

    !** FLOW

        ! initialize
        D = 0_kIndex
        Lo = StartID
        Hi = EndID
        ICount = 0_kIndex
        CALL Stack%Push(Lo)
        CALL Stack%Push(Hi)
        CALL Stack%Push(D)

        DO WHILE (.NOT.Stack%IsEmpty())
            Success = Stack%Pop(D)
            Success = Stack%Pop(Hi)
            Success = Stack%Pop(Lo)
            ! cutoff to insertion sort for small sub-arrays
            IF (Hi <= Lo + Insertion_CutOff) THEN
                CALL InsertionSort(AVal, Lo, Hi, D)
                CYCLE
            END IF
            ! compute frequency counts
            DO I = Lo, Hi
                C = CharAt(AVal(I), D) + 1_kIndex   ! account for -1 representing end-of-string
                ICount(C) = ICount(C) + 1_kIndex
            END DO
            ! accumulate counts relative to AVal(0), so that ICount(C) is the number of keys <= C
            ICount(0) = ICount(0) + Lo
            DO C = 0_kIndex, Radix-1_kIndex
                ICount(C+1) = ICount(C+1) + ICount(C)
                IF ((C > 0_kIndex).AND.((ICount(C+1)-1_kIndex) > ICount(C))) THEN
                    ! add subproblem for character C (excludes sentinel C == 0)
                    CALL Stack%Push(ICount(C))
                    CALL Stack%Push(ICount(C+1)-1_kIndex)
                    CALL Stack%Push(D+1_kIndex)
                END IF
            END DO
            ! permute data in place
            I = Hi
            DO
                ! locate element that must be shifted right of I
                C = CharAt(AVal(I), D) + 1_kIndex
                DO WHILE ((I >= Lo).AND.((ICount(C)-1_kIndex) <= I))
                    IF ((ICount(C)-1_kIndex) == I) ICount(C) = ICount(C) - 1_kIndex
                    I = I - 1_kIndex
                    IF (I >= Lo) C = CharAt(AVal(I), D) + 1_kIndex
                END DO
                ! if I < Lo the sub-array is sorted.
                IF (I < Lo) EXIT
                ! permute AVal(I) until correct element is in place
                ICount(C) = ICount(C) - 1_kIndex
                DO WHILE (ICount(C) /= I)
                    CALL Swap(AVal(I), AVal(ICount(C)))
                    C = CharAt(AVal(I), D) + 1_kIndex
                    ICount(C) = ICount(C) - 1_kIndex
                END DO
                I = I - 1_kIndex
                IF (I < Lo) EXIT
            END DO
            ! clear ICount array
            ICount = 0_kIndex
        END DO

        RETURN

    END SUBROUTINE American_Flag_Sort

    !**************************************************************************

END SUBROUTINE FvlStr_AmericanFlagSort

!------------------------------------------------------------------------------
!-----                      AUXILIARY PROCEDURES                          -----
!------------------------------------------------------------------------------

SUBROUTINE ChrStr_InsertionSort(AVal, Lo, Hi, D)

!** PURPOSE OF THIS SUBROUTINE
	!^ To sort an array of character strings using the insertion sorting algorithm
    !  whose first D+1 characters of strings are equal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(INOUT)    :: AVal(0:) !! the array of strings to be sorted
    tIndex,    INTENT(IN)       :: Lo       !! starting zero-based index, inclusive
    tIndex,    INTENT(IN)       :: Hi       !! ending zero-based index, inclusive
    tIndex,    INTENT(IN)       :: D        !! starting one-based position in a string element

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: I, J     ! zero-based indices
    tCharLen(LEN(AVal(0)))  :: Temp

!** FLOW

    DO I = Lo, Hi
        J = I
        DO WHILE (J > Lo)
            IF (.NOT.IsLessThan(AVal(J), AVal(J-1), D)) EXIT
            EXCHANGE(AVal, J, J-1)
            J = J - 1_kIndex
        END DO
    END DO

    RETURN

END SUBROUTINE ChrStr_InsertionSort

!******************************************************************************

SUBROUTINE FvlStr_InsertionSort(AVal, Lo, Hi, D)

!** PURPOSE OF THIS SUBROUTINE
	!^ To sort an array of FvlStr objects using the insertion sorting algorithm
    !  whose first D+1 characters of strings are equal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(INOUT) :: AVal(0:) !! the array of strings to be sorted
    tIndex,       INTENT(IN)    :: Lo       !! starting zero-based index, inclusive
    tIndex,       INTENT(IN)    :: Hi       !! ending zero-based index, inclusive
    tIndex,       INTENT(IN)    :: D        !! starting one-based position in a string element

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J     ! zero-based indices

!** FLOW

    DO I = Lo, Hi
        J = I
        DO WHILE (J > Lo)
            IF (.NOT.IsLessThan(AVal(J), AVal(J-1), D)) EXIT
            CALL Swap(AVal(J), AVal(J-1))
        END DO
    END DO

    RETURN

END SUBROUTINE FvlStr_InsertionSort

!******************************************************************************

FUNCTION ChrStr_IsLessThan(V, W, D) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE
	!^ To check whether V is less than W, starting at Dth character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,        INTENT(IN)    :: V
    tCharLen(LEN(V)), INTENT(IN)    :: W
    tIndex,           INTENT(IN)    :: D
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

!** FLOW

    DO I = D, LEN(V)
        IF (V(I:I) < W(I:I)) THEN
            Flag = TrueVal
            RETURN
        END IF
        IF (V(I:I) > W(I:I)) THEN
            Flag = FalseVal
            RETURN
        END IF
    END DO
    Flag = FalseVal

    RETURN

END FUNCTION ChrStr_IsLessThan

!******************************************************************************

FUNCTION FvlStr_IsLessThan(V, W, D) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE
	!^ To check whether V is less than W, starting at Dth character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: V
    TYPE(FvlStr), INTENT(IN)    :: W
    tIndex,       INTENT(IN)    :: D
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I
    tChar       :: vChr, wChr
    tIndex      :: vIndx, wIndx

!** FLOW

    DO I = D, MIN(V%Length(), W%Length())
        vChr = V%Char(I)
        wChr = W%Char(I)
        vIndx = IACHAR(vChr, KIND=kIndex)
        wIndx = IACHAR(wChr, KIND=kIndex)
        IF (vIndx < wIndx) THEN
            Flag = TrueVal
            RETURN
        END IF
        IF (vIndx > wIndx) THEN
            Flag = FalseVal
            RETURN
        END IF
    END DO
    Flag = (V%Length() < W%Length())

    RETURN

END FUNCTION FvlStr_IsLessThan

!******************************************************************************

FUNCTION ChrStr_CharAt(V, D) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE
	!^ To return the character code of the Dth character of V.
    !  Return -1 if D > LEN(V).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: V
    tIndex,    INTENT(IN)   :: D
    tIndex                  :: Indx

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (D <= LEN(V, KIND=kIndex)) THEN
        Indx = IACHAR(V(D:D), KIND=kIndex)
    ELSE
        Indx = -1_kIndex
    END IF

    RETURN

END FUNCTION ChrStr_CharAt

!******************************************************************************

FUNCTION FvlStr_CharAt(V, D) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE
	!^ To return the character code of the Dth character of V.
    !  Return -1 if D > LEN(V).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: V
    tIndex,       INTENT(IN)    :: D
    tIndex                      :: Indx

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (D <= V%Length()) THEN
        BLOCK
            tChar   :: vChr
            vChr = V%Char(D)
            Indx = IACHAR(vChr, KIND=kIndex)
        END BLOCK
    ELSE
        Indx = -1_kIndex
    END IF

    RETURN

END FUNCTION FvlStr_CharAt

!******************************************************************************

FUNCTION ChrStr_CharAt_Alphabet(V, D, Alpha) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE
	!^ To return the character code of the Dth character of V.
    !  Return -1 if D > LEN(V).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,       INTENT(IN)     :: V
    tIndex,          INTENT(IN)     :: D
    CLASS(Alphabet), INTENT(INOUT)  :: Alpha    !! Alphabet object
    tIndex                          :: Indx

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (D <= LEN(V, KIND=kIndex)) THEN
        Indx = Alpha%GetIndex(V(D:D))
    ELSE
        Indx = -1_kIndex
    END IF

    RETURN

END FUNCTION ChrStr_CharAt_Alphabet

!******************************************************************************

FUNCTION FvlStr_CharAt_Alphabet(V, D, Alpha) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE
	!^ To return the character code of the Dth character of V.
    !  Return -1 if D > LEN(V).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr),    INTENT(IN)     :: V
    tIndex,          INTENT(IN)     :: D
    CLASS(Alphabet), INTENT(INOUT)  :: Alpha    !! Alphabet object
    tIndex                          :: Indx

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (D <= V%Length()) THEN
        Indx = Alpha%GetIndex(V%Char(D))
    ELSE
        Indx = -1_kIndex
    END IF

    RETURN

END FUNCTION FvlStr_CharAt_Alphabet

!******************************************************************************

END MODULE MBase_StringSorts

!******************************************************************************
