
MODULE MBase_Ranking

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains routines that can be used to rank an array in an *ascending*
!   order.  The specified array can be of any type that is comparable.  For *Fortran
!   intrinsic* types, *CHARACTER*, *INTEGER* and *REAL* types are considered valid
!   comparable types.  For derived types, any types that extend from the *Comparable*
!   type are valid comparable types. <br>
!   After the ranking, a user can use the returned indices to sort the array and its
!   associated variable(s) in a desired order.  The following code snippet illustrates
!   how to typically sort the array and its associated variable(s).
!   <Pre><Code style="color:MidnightBlue;">
!   ! sort AVal and BVal in an ascending order using AInd indices
!   XVal = AVal(AInd(:))
!   YVal = BVal(AInd(:))
!
!   ! sort AVal and BVal in a descending order using AInd indices
!   DO I = 1, ASize
!       XVal(I) = AVal(AInd(ASize-I+1))
!       YVal(I) = BVal(AInd(ASize-I+1))
!   END DO
!   </Code></Pre>
!   See the <a href="../module/mbase_sortascend.html">MBase_SortAscend</a> and the
!   <a href="../module/mbase_sortdescend.html">MBase_SortDescend</a> modules for
!   routines that perform sorting of an array in an *ascending* and *descending* order,
!   respectively.  The *MBase_SortAscend* module also gives an overview of available
!   sorting algorithms, which are mostly the same as the ranking algorithms available
!   in this module. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_Comparable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! hybrid
    PUBLIC :: WiseRank
    PUBLIC :: Wise_RankChar
    PUBLIC :: Wise_RankComp
    PUBLIC :: WiseRankStable
    PUBLIC :: WiseStable_RankChar
    PUBLIC :: WiseStable_RankComp
    PUBLIC :: IntroRank
    PUBLIC :: Intro_RankChar
    PUBLIC :: Intro_RankComp
    PUBLIC :: JavaRank
    PUBLIC :: Java_RankChar
    PUBLIC :: Java_RankComp
    PUBLIC :: PDQRank
    PUBLIC :: PDQ_RankChar
    PUBLIC :: PDQ_RankComp
    PUBLIC :: TimRank
    PUBLIC :: Tim_RankChar
    PUBLIC :: Tim_RankComp
    PUBLIC :: RustRank
    PUBLIC :: Rust_RankChar
    PUBLIC :: Rust_RankComp
    ! quick
    PUBLIC :: QuickRankHoare
    PUBLIC :: QuickHoare_RankChar
    PUBLIC :: QuickHoare_RankComp
    PUBLIC :: QuickRankLomuto
    PUBLIC :: QuickLomuto_RankChar
    PUBLIC :: QuickLomuto_RankComp
    PUBLIC :: QuickRankMo3
    PUBLIC :: QuickMo3_RankChar
    PUBLIC :: QuickMo3_RankComp
    PUBLIC :: QuickRank3Way
    PUBLIC :: Quick3Way_RankChar
    PUBLIC :: Quick3Way_RankComp
    PUBLIC :: QuickRankVowels
    PUBLIC :: QuickVowels_RankChar
    PUBLIC :: QuickVowels_RankComp
    PUBLIC :: QuickRankStable
    PUBLIC :: QuickStable_RankChar
    PUBLIC :: QuickStable_RankComp
    PUBLIC :: QuickRankIterative
    PUBLIC :: QuickIterative_RankChar
    PUBLIC :: QuickIterative_RankComp
    PUBLIC :: QuickRankJava
    PUBLIC :: QuickJava_RankChar
    PUBLIC :: QuickJava_RankComp
    ! merge
    PUBLIC :: MergeRankTopDown
    PUBLIC :: MergeTopDown_RankChar
    PUBLIC :: MergeTopDown_RankComp
    PUBLIC :: MergeRankBottomUp
    PUBLIC :: MergeBottomUp_RankChar
    PUBLIC :: MergeBottomUp_RankComp
    PUBLIC :: MergeRankQuadSplit
    PUBLIC :: MergeQuadSplit_RankChar
    PUBLIC :: MergeQuadSplit_RankComp
    PUBLIC :: MergeRankHalfCopy
    PUBLIC :: MergeHalfCopy_RankChar
    PUBLIC :: MergeHalfCopy_RankComp
    PUBLIC :: MergeRankOrderPack
    PUBLIC :: MergeOrderPack_RankChar
    PUBLIC :: MergeOrderPack_RankComp
    ! auxiliary
    PUBLIC :: IsRanked

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tCharParam  :: ModName = 'MBase_Ranking'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! hybrid
    INTERFACE WiseRank
        !^ **Subroutine Interface**: WiseRank <br>
        !  **Purpose**:  To rank an array in an ascending order using the *WiseSort*
        !       algorithm, which is a hybrid (unstable) algorithm that employs
        !       various sorting algorithms including the *pair-insertion sort*,
        !       *mergesort* and *quicksort* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL WiseRank(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/wisesort.html">WiseSort</a> interface
        !   for the technical information of the *WiseSort* algorithm. <br>
        MODULE SUBROUTINE Wise_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Wise_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Wise_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Wise_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Wise_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Wise_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Wise_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Wise_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Wise_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE WiseRankStable
        !^ **Subroutine Interface**: WiseRankStable <br>
        !  **Purpose**:  To rank an array in an ascending order using the *WiseSort-Stable*
        !       algorithm, which is a hybrid *stable* algorithm that employs various
        !       sorting algorithms including the *insertion sort*, *mergesort* and
        !       *quicksort* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL WiseRankStable(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/wisesortstable.html">WiseSortStable</a> interface
        !   for the technical information of the *WiseSort-Stable* algorithm. <br>
        MODULE SUBROUTINE WiseStable_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE IntroRank
        !^ **Subroutine Interface**: IntroRank <br>
        !  **Purpose**:  To rank an array in an ascending order using the *IntroSort*
        !       algorithm, which is a hybrid (unstable) algorithm that employs
        !       various sorting algorithms including the *pair-insertion sort*,
        !       *quicksort* and *heapsort* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL IntroRank(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/introsort.html">IntroSort</a> interface
        !   for the technical information of the *IntroSort* algorithm. <br>
        MODULE SUBROUTINE Intro_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Intro_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Intro_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Intro_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Intro_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Intro_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Intro_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Intro_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Intro_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE JavaRank
        !^ **Subroutine Interface**: JavaRank <br>
        !  **Purpose**:  To rank an array in an ascending order using the *JavaSort*
        !       algorithm, which is a hybrid (unstable) algorithm that employs
        !       various sorting algorithms including the *mixed-insertion sort*,
        !       *quicksort*, *heapsort*, and *merging* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL JavaRank(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/javasort.html">JavaSort</a> interface
        !   for the technical information of the *JavaSort* algorithm. <br>
        MODULE SUBROUTINE Java_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Java_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Java_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Java_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Java_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Java_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Java_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Java_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Java_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE PDQRank
        !^ **Subroutine Interface**: PDQRank <br>
        !  **Purpose**:  To rank an array in an ascending order using the *PDQSort*
        !       algorithm, which is a hybrid (unstable) algorithm that employs
        !       various sorting algorithms including the *insertion sort*,
        !       *quicksort*, and *heapsort* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL PDQRank(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/pdqsort.html">PDQSort</a> interface
        !   for the technical information of the *PDQSort* algorithm. <br>
        MODULE SUBROUTINE PDQ_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE TimRank
        !^ **Subroutine Interface**: TimRank <br>
        !  **Purpose**:  To rank an array in an ascending order using the *TimSort*
        !       algorithm, which is a hybrid *stable* algorithm that employs an
        !       *insertion sort* and an *adaptive mergesort* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL TimRank(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/timsort.html">TimSort</a> interface
        !   for the technical information of the *TimSort* algorithm. <br>
        MODULE SUBROUTINE Tim_RankChar(AVal, AInd)
            tCharStar,      INTENT(IN)      :: AVal(:)
            tIndex, TARGET, INTENT(INOUT)   :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Tim_RankI8(AVal, AInd)
            tSInt8,         INTENT(IN)      :: AVal(:)
            tIndex, TARGET, INTENT(INOUT)   :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Tim_RankI16(AVal, AInd)
            tSInt16,        INTENT(IN)      :: AVal(:)
            tIndex, TARGET, INTENT(INOUT)   :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Tim_RankI32(AVal, AInd)
            tSInt32,        INTENT(IN)      :: AVal(:)
            tIndex, TARGET, INTENT(INOUT)   :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Tim_RankI64(AVal, AInd)
            tSInt64,        INTENT(IN)      :: AVal(:)
            tIndex, TARGET, INTENT(INOUT)   :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Tim_RankR32(AVal, AInd)
            tRealSP,        INTENT(IN)      :: AVal(:)
            tIndex, TARGET, INTENT(INOUT)   :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Tim_RankR64(AVal, AInd)
            tRealDP,        INTENT(IN)      :: AVal(:)
            tIndex, TARGET, INTENT(INOUT)   :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Tim_RankR128(AVal, AInd)
            tRealQP,        INTENT(IN)      :: AVal(:)
            tIndex, TARGET, INTENT(INOUT)   :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Tim_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex, TARGET,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE RustRank
        !^ **Subroutine Interface**: RustRank <br>
        !  **Purpose**:  To rank an array in an ascending order using the *RustSort*
        !       algorithm, which is a hybrid (unstable) algorithm that employs
        !       various sorting algorithms including the *mixed-insertion sort*,
        !       *quicksort*, *heapsort*, and *merging* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL RustRank(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/rustsort.html">RustSort</a> interface
        !   for the technical information of the *RustSort* algorithm. <br>
        MODULE SUBROUTINE Rust_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Rust_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Rust_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Rust_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Rust_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Rust_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Rust_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Rust_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Rust_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    ! quick
    INTERFACE QuickRankHoare
        !^ **Subroutine Interface**: QuickRankHoare <br>
        !  **Purpose**:  To rank an array in an ascending order using the
        !       *QuickSort* algorithm with Hoare's partitioning scheme. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickRankHoare(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/quicksorthoare.html">QuickSortHoare</a>
        !   interface for the technical information of the *QuickSort* algorithm
        !   with Hoare's partitioning scheme. <br>
        MODULE SUBROUTINE QuickHoare_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickRankLomuto
        !^ **Subroutine Interface**: QuickRankLomuto <br>
        !  **Purpose**:  To rank an array in an ascending order using the
        !       *QuickSort* algorithm with Lomuto's partitioning scheme. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickRankLomuto(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/quicksortlomuto.html">QuickSortLomuto</a>
        !   interface for the technical information of the *QuickSort* algorithm
        !   with Lomuto's partitioning scheme. <br>
        MODULE SUBROUTINE QuickLomuto_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickRankMo3
        !^ **Subroutine Interface**: QuickRankMo3 <br>
        !  **Purpose**:  To rank an array in an ascending order using the
        !       *QuickSort* algorithm with the median-of-three (Mo3)
        !       partitioning scheme. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickRankMo3(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/quicksortmo3.html">QuickSortMo3</a>
        !   interface for the technical information of the *QuickSort* algorithm
        !   with the median-of-three partitioning scheme. <br>
        MODULE SUBROUTINE QuickMo3_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickRank3Way
        !^ **Subroutine Interface**: QuickRank3Way <br>
        !  **Purpose**:  To rank an array in an ascending order using the
        !       *QuickSort* algorithm with the three-way partitioning scheme. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickRank3Way(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/quicksort3way.html">QuickSort3Way</a>
        !   interface for the technical information of the *QuickSort* algorithm
        !   with the three-way partitioning scheme. <br>
        MODULE SUBROUTINE Quick3Way_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickRankVowels
        !^ **Subroutine Interface**: QuickRankVowels <br>
        !  **Purpose**:  To rank an array in an ascending order using the
        !       *QuickSort* algorithm based on the *QuickSort Version 3*
        !       (the professional version) by R.A. Vowels. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickRankVowels(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/quicksortvowels.html">QuickSortVowels</a>
        !   interface for the technical information of the *Vowels' QuickSort*
        !   algorithm. <br>
        MODULE SUBROUTINE QuickVowels_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickRankStable
        !^ **Subroutine Interface**: QuickRankStable <br>
        !  **Purpose**:  To rank an array in an ascending order using the
        !       *QuickSort* algorithm with the *stable* partitioning scheme. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickRankStable(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/quicksortstable.html">QuickSortStable</a>
        !   interface for the technical information of the *QuickSort* algorithm
        !   with the stable partitioning scheme. <br>
        MODULE SUBROUTINE QuickStable_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickRankIterative
        !^ **Subroutine Interface**: QuickRankIterative <br>
        !  **Purpose**:  To rank an array in an ascending order using an *iterative
        !       Quicksort* algorithm with median-of-three (Mo3) partitioning scheme. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickRankIterative(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/quicksortiterative.html">QuickSortIterative</a>
        !   interface for the technical information of the *iterative Quicksort*
        !   algorithm with median-of-three partitioning scheme. <br>
        MODULE SUBROUTINE QuickIterative_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickRankJava
        !^ **Subroutine Interface**: QuickRankJava <br>
        !  **Purpose**:  To rank an array in an ascending order using a *dual-pivot
        !       Quicksort* algorithm based on Java's sorting algorithm. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickRankJava(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/quicksortjava.html">QuickSortJava</a>
        !   interface for the technical information of the Java's *dual-pivot
        !   QuickSort* algorithm. <br>
        MODULE SUBROUTINE QuickJava_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    ! merge
    INTERFACE MergeRankTopDown
        !^ **Subroutine Interface**: MergeRankTopDown <br>
        !  **Purpose**:  To rank an array in an ascending order using a *top-down
        !       merge sort* algorithm. <br>
        !  **Usage**: <br>
        !   --->    CALL MergeRankTopDown(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/mergesorttopdown.html">MergeSortTopDown</a>
        !   interface for the technical information of the *top-down merge sort*
        !   algorithm. <br>
        MODULE SUBROUTINE MergeTopDown_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE MergeRankBottomUp
        !^ **Subroutine Interface**: MergeRankBottomUp <br>
        !  **Purpose**:  To rank an array in an ascending order using a *bottom-up
        !       merge sort* algorithm. <br>
        !  **Usage**: <br>
        !   --->    CALL MergeRankBottomUp(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/mergesortbottomup.html">MergeSortBottomUp</a>
        !   interface for the technical information of the *bottom-up merge sort*
        !   algorithm. <br>
        MODULE SUBROUTINE MergeBottomUp_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE MergeRankQuadSplit
        !^ **Subroutine Interface**: MergeRankQuadSplit <br>
        !  **Purpose**:  To rank an array in an ascending order using a *top-down
        !       merge sort* algorithm where the given array is split into four
        !       sub-arrays instead of two sub-arrays. <br>
        !  **Usage**: <br>
        !   --->    CALL MergeRankQuadSplit(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/mergesortquadsplit.html">MergeSortRealQPSplit</a>
        !   interface for the technical information of the *top-down merge sort*
        !   algorithm with quad split. <br>
        MODULE SUBROUTINE MergeQuadSplit_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE MergeRankHalfCopy
        !^ **Subroutine Interface**: MergeRankHalfCopy <br>
        !  **Purpose**:  To rank an array in an ascending order using a *top-down
        !       merge sort* algorithm with *half-copying merge* algorithm. <br>
        !  **Usage**: <br>
        !   --->    CALL MergeRankHalfCopy(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   See the <a href="../interface/mergesorthalfcopy.html">MergeSortHalfCopy</a>
        !   interface for the technical information of the *top-down merge sort*
        !   algorithm with *half-copying merge* algorithm. <br>
        MODULE SUBROUTINE MergeHalfCopy_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    INTERFACE MergeRankOrderPack
        !^ **Subroutine Interface**: MergeRankOrderPack <br>
        !  **Purpose**:  To rank an array in an ascending order using a *merge sort*
        !       algorithm based on the '*MrgRnk*' routine in the *OrderPack* 2.0
        !       library by Olagnon [1]. <br>
        !  **Usage**: <br>
        !   --->    CALL MergeRankOrderPack(AVal, AInd) <br>
        !  **Technical Information**: <br>
        !   The algorithm used here is actually a hybrid stable algorithm where
        !   the insertion sort algorithm is utilized for small arrays and the
        !   Orderpack's merge sort algorithm [1] is employed for large arrays. <br>
        !  **References**: <br>
        !   [1] <a href="http://www.fortran-2000.com/rank/">ORDERPACK 2.0: Unconditional,
        !       Unique, and Partial Ranking, Sorting, and Permutation Downloadable Fortran90
        !       source code. </a> <br>
        MODULE SUBROUTINE MergeOrderPack_RankChar(AVal, AInd)
            tCharStar, INTENT(IN)       :: AVal(:)
            tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeOrderPack_RankI8(AVal, AInd)
            tSInt8,  INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeOrderPack_RankI16(AVal, AInd)
            tSInt16, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeOrderPack_RankI32(AVal, AInd)
            tSInt32, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeOrderPack_RankI64(AVal, AInd)
            tSInt64, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeOrderPack_RankR32(AVal, AInd)
            tRealSP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeOrderPack_RankR64(AVal, AInd)
            tRealDP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeOrderPack_RankR128(AVal, AInd)
            tRealQP, INTENT(IN)     :: AVal(:)
            tIndex,  INTENT(INOUT)  :: AInd(SIZE(AVal))
        END SUBROUTINE
        MODULE SUBROUTINE MergeOrderPack_RankComp(AVal, AInd)
            CLASS(Comparable), INTENT(IN)       :: AVal(:)
            tIndex,            INTENT(INOUT)    :: AInd(SIZE(AVal))
        END SUBROUTINE
    END INTERFACE
    ! auxiliary
    INTERFACE IsRanked
        !^ **Function Interface**: IsRanked <br>
        !  **Purpose**:  To check whether the specified array is ranked in
        !       the ascending order. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsRanked(AVal, AInd) <br>
        !   --->    IF (.NOT.IsRanked(AVal, AInd)) DoSomething <br>
        MODULE FUNCTION IsRanked_Char(AVal, AInd) RESULT(Flag)
            tCharStar, INTENT(IN)   :: AVal(:)
            tIndex,    INTENT(IN)   :: AInd(SIZE(AVal))
            tLogical                :: Flag
        END FUNCTION
        MODULE FUNCTION IsRanked_I8(AVal, AInd) RESULT(Flag)
            tSInt8,  INTENT(IN) :: AVal(:)
            tIndex,  INTENT(IN) :: AInd(SIZE(AVal))
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsRanked_I16(AVal, AInd) RESULT(Flag)
            tSInt16, INTENT(IN) :: AVal(:)
            tIndex,  INTENT(IN) :: AInd(SIZE(AVal))
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsRanked_I32(AVal, AInd) RESULT(Flag)
            tSInt32, INTENT(IN) :: AVal(:)
            tIndex,  INTENT(IN) :: AInd(SIZE(AVal))
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsRanked_I64(AVal, AInd) RESULT(Flag)
            tSInt64, INTENT(IN) :: AVal(:)
            tIndex,  INTENT(IN) :: AInd(SIZE(AVal))
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsRanked_R32(AVal, AInd) RESULT(Flag)
            tRealSP, INTENT(IN) :: AVal(:)
            tIndex,  INTENT(IN) :: AInd(SIZE(AVal))
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsRanked_R64(AVal, AInd) RESULT(Flag)
            tRealDP, INTENT(IN) :: AVal(:)
            tIndex,  INTENT(IN) :: AInd(SIZE(AVal))
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsRanked_R128(AVal, AInd) RESULT(Flag)
            tRealQP, INTENT(IN) :: AVal(:)
            tIndex,  INTENT(IN) :: AInd(SIZE(AVal))
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsRanked_Comp(AVal, AInd) RESULT(Flag)
            CLASS(Comparable), INTENT(IN)   :: AVal(:)
            tIndex,            INTENT(IN)   :: AInd(SIZE(AVal))
            tLogical                        :: Flag
        END FUNCTION
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

END MODULE MBase_Ranking

!******************************************************************************
