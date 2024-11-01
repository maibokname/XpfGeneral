
MODULE MBase_SortAscend

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains routines that can be used to sort an array in an *ascending*
!   order.  The specified array can be of any type that is comparable.  For *Fortran
!   intrinsic* types, *CHARACTER*, *INTEGER* and *REAL* types are considered valid
!   comparable types.  For derived types, any types that extend from the *Comparable*
!   type are valid comparable types. <br>
!   Various sorting algorithms are available and are summarized as follows. <br>
!   1.  *WiseSort* and *WiseSort-Stable* algorithms are hybrid sorting algorithms that
!       employ multiple sorting algorithms.  As the names suggested, the *WiseSort-Stable*
!       algorithm can be used for stable sorting whereas the *WiseSort* algorithm cannot.
!       Both algorithms are originated in this library (XpfLib) and they sort the given
!       array using a variety of sorting algorithms based on the pattern of the data.
!       First, they carefully and *wisely* inspect the specified array by checking if there
!       is any pattern in the given array.  Next, they decide which sorting algorithm is
!       most suitable for the array.  They then sort the array using the chosen algorithm.
!       For more information regarding the selection of a preferred algorithm and the various
!       algorithms employed by either the *WiseSort* or the *WiseSort-Stable* algorithm, see
!       the *Technical Information* section of each algorithm. <br>
!   2.  *Hybrid* sorting algorithms include the *IntroSort*, *JavaSort*, *PDQSort*, *TimSort*,
!       and *RustSort* algorithms.  These hybrid algorithms commonly employ multiple algorithms
!       and can be classified into two groups: stable and unstable sorting algorithms.  The
!       *stable* algorithms include the *TimSort* and *RustSort* algorithms whereas the other
!       three algorithms are unstable.  For more information, see the *Technical Information*
!       section of each algorithm. <br>
!   3.  *Quicksort-based* sorting algorithms are actually hybrid algorithms that utilize a
!       quicksort algorithm as its main algorithm and employ an insertion sort algorithm
!       (mostly the pair-insertion sort) to deal with small (sub)array(s).  All of these
!       algorithms, with the exception of the *QuickSortStable* algorithm, are unstable.
!       For more information, see the *Technical Information* section of each algorithm. <br>
!   4.  *Mergesort-based* sorting algorithms are actually hybrid algorithms that employ a
!       mergesort algorithm as its main algorithm and use an insertion sort algorithm to
!       deal with small (sub)array(s).  All of these algorithms are considered stable.
!       For more information, see the *Technical Information* section of each algorithm. <br>
!   See the <a href="../module/mbase_sortdescend.html">MBase_SortDescend</a> module
!   for routines that perform sorting of an array in a *descending* order.  Also, see
!   the <a href="../module/mbase_ranking.html">MBase_Ranking</a> module for routines
!   that perform ranking of an array in an *ascending* order.

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_Comparable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! hybrid
    PUBLIC :: WiseSort
    PUBLIC :: WiseSortStable
    PUBLIC :: IntroSort
    PUBLIC :: JavaSort
    PUBLIC :: PDQSort
    PUBLIC :: TimSort
    PUBLIC :: RustSort
    ! quick
    PUBLIC :: QuickSortHoare
    PUBLIC :: QuickSortLomuto
    PUBLIC :: QuickSortMo3
    PUBLIC :: QuickSort3Way
    PUBLIC :: QuickSortVowels
    PUBLIC :: QuickSortStable
    PUBLIC :: QuickSortIterative
    PUBLIC :: QuickSortJava
    ! merge
    PUBLIC :: MergeSortTopDown
    PUBLIC :: MergeSortBottomUp
    PUBLIC :: MergeSortRealQPSplit
    PUBLIC :: MergeSortHalfCopy
    ! auxiliary
    PUBLIC :: IsSortedAscend

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tCharParam  :: ModName = 'MBase_SortAscend'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! hybrid
    INTERFACE WiseSort
        !^ **Subroutine Interface**: WiseSort <br>
        !  **Purpose**:  To sort an array in a desired order using the *WiseSort*
        !       algorithm, which is a hybrid (unstable) algorithm that employs
        !       various sorting algorithms including the *pair-insertion sort*,
        !       *mergesort* and *quicksort* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL WiseSort(A) <br>
        !  **Technical Information**: <br>
        !   The *WiseSort* algorithm is a hybrid algorithm originated in this library
        !   (*XpfLib*).  The algorithm sorts the given array using a variety of sorting
        !   algorithms based on the pattern of the data.  First, it carefully and wisely
        !   inspects the specified array by checking if there is any pattern in the given
        !   array.  Next, it decides which sorting algorithm is most suitable for the array.
        !   It then sorts the array using the chosen algorithm. <br>
        !   The selection of a sorting algorithm is based on the following facts. <br>
        !   - An *Insertion sort* algorithm usually performs well for an array with
        !     small size. <br>
        !   - If the data is already sorted, no sorting algorithm is needed. <br>
        !   - If the data is already sorted but in an order opposite to the desired one,
        !     a reversion of the array is the most optimal sorting algorithm. <br>
        !   - A quicksort algorithm typically performs well for a totally randomized array. <br>
        !   - A mergesort algorithm generally performs well for a mostly ordered array. <br>
        !   Based on the aforementioned facts, the *WiseSort* algorithm utilizes various
        !   sorting algorithms including: <br>
        !   1. *Pair-insertion-sort-based* algorithms consisting of <br>
        !     - guarded version for a left-most sub-array or the whole array with small size, <br>
        !     - unguarded version for a non-left-most sub-array with small size. <br>
        !   2. *Merge-sort-based* algorithms consisting of <br>
        !     - Java's merging runs for a highly structured array (an array with a certain
        !       recognized pattern, e.g. saw- or wave-like pattern), <br>
        !     - Rust's mergesort (or the so-called simplified TimSort) for a mostly ordered
        !       array (i.e. an array having a very long ordered run, either in ascending or
        !       descending order). <br>
        !       For this particular implementation, it is found that Rust's mergesort performs
        !       very well for an array when more than 50% of the first or last elements of the
        !       array are already sorted. <br>
        !   3. *Quick-sort-based* algorithms where various partitioning schemes are employed
        !       including: <br>
        !     - Hoare's partitioning scheme for a NOT highly structured array, i.e. a
        !       partly-ordered and partly-randomized array (an array having not very long
        !       and not very short ordered runs), <br>
        !     - Median-of-three partitioning scheme for a totally-randomized array
        !       (an array having VERY SHORT ordered runs, e.g. an ordered run with less than
        !       4 elements), <br>
        !     - Ninther (median of medians or pseudo-median of nine) partitioning scheme for
        !       worst-case situations (when the quicksort algorithm converges too slowly, it
        !       switches from Hoare's or median-of-three partitioning scheme to the ninther
        !       partitioning scheme), <br>
        !     - Three-way (or Dutch's national flag) partitioning scheme for an array with
        !       many equal elements (when the quick sort algorithm detects that many elements
        !       are equal, it switches from Hoare's or median-of-three partitioning scheme to
        !       the three-way partitioning scheme). <br>
        !   As previously mentioned, the selection of an appropriate algorithm is based on
        !   a careful and wise inspection of the given array.  The routine responsible for
        !   this task will try to predict a suitable algorithm for any given array according
        !   to its pattern found.  The following list provides an overview of the routine. <br>
        !     - The routine will mostly try to scan just a very few runs and quit very quickly.
        !       Only, the highly structured array (the one sorted by Java's merge runs) is
        !       entirely scanned since all the runs are needed by the selected algorithm. <br>
        !     - If the routine detects that the current run is in an order opposite to the
        !       desired one, it will only reverse the run if necessary (i.e. the chosen
        !       algorithm will benefit from the reversion of the run). <br>
        !     - The prediction of a proper algorithm is quite accurate for most known cases.
        !       However, there are certain known cases that the routine may not choose
        !       the *BEST* algorithm.  For example, if highly-order parts of the given array
        !       are in the middle while randomized parts are in the beginning and in the end,
        !       the routine will typically select a quick sort algorithm.  However, if the
        !       highly-ordered parts are long enough (e.g. more than 50% of the array size),
        !       Rust/TimSort algorithms might be better algorithms.  For these certain cases,
        !       nonetheless, it is not worth scanning the whole array just so we can choose
        !       the *BEST* algorithm because the overhead will surely be very expensive. <br>
        MODULE SUBROUTINE Wise_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Wise_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Wise_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Wise_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Wise_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Wise_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Wise_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Wise_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Wise_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE WiseSortStable
        !^ **Subroutine Interface**: WiseSortStable <br>
        !  **Purpose**:  To sort an array in a desired order using the *WiseSort-Stable*
        !       algorithm, which is a hybrid *stable* algorithm that employs various
        !       sorting algorithms including the *insertion sort*, *mergesort* and
        !       *quicksort* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL WiseSortStable(A) <br>
        !  **Technical Information**: <br>
        !   Similar to the *WiseSort* algorithm, the *WiseSort-Stable* algorithm is
        !   a hybrid algorithm originated in this library(*XpfLib*).  Unlike the
        !   *WiseSort* algorithm, which is an unstable algorithm, the *WiseSort-Stable*
        !   algorithm (as the name implied) is a stable algorithm that sorts the given
        !   array using a variety of *stable* sorting algorithms.  Similar to the *WiseSort*
        !   algorithm, the *WiseSort-Stable* algorithm first inspects the specified
        !   array by checking if there is any pattern in the given array and then decides
        !   which stable sorting algorithm is most suitable for the array.  It then sorts
        !   the array using the chosen algorithm. <br>
        !   The selection of a stable sorting algorithm is based on the same facts given
        !   in the *WiseSort* section; hence, the *WiseSort-Stable* algorithm employs
        !   various stable sorting algorithms including: <br>
        !   1. *Insertion sort* algorithm (guarded version only) for a small-size (sub)array. <br>
        !   2. *Merge-sort-based* algorithms consisting of <br>
        !     - Java's merging runs for a highly structured array (an array with a certain
        !       recognized pattern, e.g. saw- or wave-like pattern), <br>
        !     - Rust's merge sort (or the so-called simplified TimSort) for a mostly ordered
        !       array (an array having a very long ordered run, either in ascending or
        !       descending order), <br>
        !     - Merge sort with half-copying for a small-size array or a partially randomized
        !       array (an array with one not-too-short and not-too-long run and many very-very
        !       short runs). <br>
        !   3. Quicksort algorithm with stable partitioning scheme for a totally-randomized array
        !       (an array having VERY SHORT ordered runs, e.g. an ordered run with less than
        !       4 elements). <br>
        !   4. Hybrid quick-merge sorting algorithm for a partially randomized array (an array
        !       with many not-too-short and not-too-long runs). <br>
        !   Note: the hybrid quick-merge sorting algorithm is a quick-sort-based algorithm
        !       that switches to the mergesort with half-copying algorithm for the smaller
        !       sub-array after the stable partitioning process while it recurs on the larger
        !       sub-array. <br>
        MODULE SUBROUTINE WiseStable_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE WiseStable_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE IntroSort
        !^ **Subroutine Interface**: IntroSort <br>
        !  **Purpose**:  To sort an array in a desired order using the *IntroSort*
        !       algorithm, which is a hybrid (unstable) algorithm that employs
        !       various sorting algorithms including the *pair-insertion sort*,
        !       *quicksort* and *heapsort* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL IntroSort(A) <br>
        !  **Technical Information**: <br>
        !   The *IntroSort* algorithm is a hybrid algorithm developed by David Musser [1].
        !   The algorithm starts sorting the given array using the quicksort algorithm
        !   with the median-of-three partitioning scheme.  It then switches to the heapsort
        !   algorithm when the recursion depth exceeds its limit (which is dependent on the
        !   array size).  It also switches to the pair-insertion sort algorithm if the
        !   (sub)array size falls below a certain threshold (called insertion cutoff). <br>
        !  **References**: <br>
        !   [1] <a href="https://web.archive.org/web/20230307185457/http://www.cs.rpi.edu/~musser/gp/introsort.ps">
        !       Musser, D.R. 1997.  Introspective Sorting and Selection Algorithms.
        !       Software: Practice and Experience. 27(8):983-993. </a> <br>
        MODULE SUBROUTINE Intro_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Intro_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Intro_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Intro_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Intro_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Intro_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Intro_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Intro_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Intro_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE JavaSort
        !^ **Subroutine Interface**: JavaSort <br>
        !  **Purpose**:  To sort an array in a desired order using the *JavaSort*
        !       algorithm, which is a hybrid (unstable) algorithm that employs
        !       various sorting algorithms including the *mixed-insertion sort*,
        !       *quicksort*, *heapsort*, and *merging* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL JavaSort(A) <br>
        !  **Technical Information**: <br>
        !   The *JavaSort* algorithm is a hybrid algorithm based on Java's sorting
        !   algorithm [1].  The algorithm employs the dual-pivot quicksort as the
        !   main algorithm.  The algorithm is a variant of the *IntroSort* algorithm
        !   where the heapsort algorithm is used when the recursion depth exceeds its
        !   limit and the mixed insertion sort algorithm is utilized when the number
        !   of elements of the (sub)array(s) is below the insertion cutoff.  In addition,
        !   the *JavaSort* algorithm (with a routine that inspects the pattern of the
        !   specified array) employs a variant of the mergesort algorithm (a merging of
        !   runs) if the given array is found to be highly structured. <br>
        !  **References**: <br>
        !   [1] <a href="https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/DualPivotQuicksort.java">
        !       Java's DualPivotQuicksort class. </a> <br>
        MODULE SUBROUTINE Java_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Java_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Java_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Java_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Java_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Java_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Java_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Java_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Java_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE PDQSort
        !^ **Subroutine Interface**: PDQSort <br>
        !  **Purpose**:  To sort an array in a desired order using the *PDQSort*
        !       algorithm, which is a hybrid (unstable) algorithm that employs
        !       various sorting algorithms including the *insertion sort*,
        !       *quicksort*, and *heapsort* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL PDQSort(A) <br>
        !  **Technical Information**: <br>
        !   The *PDQSort* (Pattern-Defeating Quicksort) algorithm is a hybrid algorithm
        !   developed by Orson Peters [1].  The algorithm is a variant of the *IntroSort*
        !   algorithm with various improvements including median-of-three pivoting scheme,
        !   *BlockQuickSort* partitioning scheme to lesson the branch mis-prediction penalties,
        !   an adaptive sort to deals with an array with certain patterns, and a shuffling
        !   of array elements to help the heapsort works better. <br>
        !  **References**: <br>
        !   [1] <a href="https://arxiv.org/abs/2106.05123">Peters, O.R.L. 2021.
        !       Pattern-defeating Quicksort. </a> <br>
        MODULE SUBROUTINE PDQ_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE PDQ_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE TimSort
        !^ **Subroutine Interface**: TimSort <br>
        !  **Purpose**:  To sort an array in a desired order using the *TimSort*
        !       algorithm, which is a hybrid *stable* algorithm that employs an
        !       *insertion sort* and an *adaptive mergesort* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL TimSort(A) <br>
        !  **Technical Information**: <br>
        !   The *TimSort* algorithm is a hybrid stable algorithm developed by Tim
        !   Peters [1].  The algorithm is an adaptive, natural mergesort that works
        !   well for many kinds of partially ordered arrays.  As implemented here,
        !   the *Timsort* algorithm employs the insertion sort for small arrays and
        !   the adaptive mergesort for large arrays. <br>
        !  **References**: <br>
        !   [1] <a href="https://bugs.python.org/file4451/timsort.txt">Timsort by
        !       Tim Peters. </a> <br>
        MODULE SUBROUTINE Tim_SortChar(A)
            tCharStar, INTENT(INOUT), TARGET    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Tim_SortI8(A)
            tSInt8,  INTENT(INOUT), TARGET  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Tim_SortI16(A)
            tSInt16, INTENT(INOUT), TARGET  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Tim_SortI32(A)
            tSInt32, INTENT(INOUT), TARGET  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Tim_SortI64(A)
            tSInt64, INTENT(INOUT), TARGET  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Tim_SortR32(A)
            tRealSP, INTENT(INOUT), TARGET  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Tim_SortR64(A)
            tRealDP, INTENT(INOUT), TARGET  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Tim_SortR128(A)
            tRealQP, INTENT(INOUT), TARGET  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Tim_SortComp(A)
            CLASS(Comparable), INTENT(INOUT), TARGET    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE RustSort
        !^ **Subroutine Interface**: RustSort <br>
        !  **Purpose**:  To sort an array in a desired order using the *RustSort*
        !       algorithm, which is a hybrid *stable* algorithm that employs an
        !       *insertion sort* and an *adaptive mergesort* algorithms. <br>
        !  **Usage**: <br>
        !   --->    CALL RustSort(A) <br>
        !  **Technical Information**: <br>
        !   The *RustSort* algorithm is a hybrid stable algorithm based on the Rust's
        !   mergesort algorithm [1].  The algorithm can be considered as a simplified
        !   *TimSort* algorithm where the galloping mode is not utilized.  Similar to
        !   the *Timsort* algorithm, the *RustSort* algorithm employs the insertion sort
        !   for small arrays and the adaptive mergesort for large arrays. <br>
        !  **References**: <br>
        !   [1] <a href="https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159">
        !       Rust's mergesort. </a> <br>
        MODULE SUBROUTINE Rust_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Rust_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Rust_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Rust_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Rust_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Rust_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Rust_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Rust_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Rust_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    ! quick
    INTERFACE QuickSortHoare
        !^ **Subroutine Interface**: QuickSortHoare <br>
        !  **Purpose**:  To sort an array in a desired order using the *QuickSort*
        !       algorithm with Hoare's partitioning scheme. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickSortHoare(A) <br>
        !  **Technical Information**: <br>
        !   The algorithm used here is actually a hybrid (unstable) algorithm where
        !   the pair-insertion sort algorithm is utilized for small (sub)array(s) and
        !   the quicksort algorithm with Hoare's partitioning scheme [1] is employed
        !   for large (sub)array(s).  Also, the algorithm uses a tail recursion (instead
        !   of a pure recursion) to minimize the recursive depth and make sure at most
        !   O(log(n)) space is used [2]. <br>
        !  **References**: <br>
        !   [1] <a href="https://en.wikipedia.org/wiki/Quicksort">Quicksort. </a> <br>
        !   [2] <a href="https://www.techiedelight.com/boost-quicksort-performance/">
        !       How to Boost QuickSort Performance? </a> <br>
        MODULE SUBROUTINE QuickHoare_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickHoare_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickSortLomuto
        !^ **Subroutine Interface**: QuickSortLomuto <br>
        !  **Purpose**:  To sort an array in a desired order using the *QuickSort*
        !       algorithm with Lomuto's partitioning scheme. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickSortLomuto(A) <br>
        !  **Technical Information**: <br>
        !   The algorithm used here is actually a hybrid (unstable) algorithm where
        !   the pair-insertion sort algorithm is utilized for small (sub)array(s) and
        !   the quicksort algorithm with Lomuto's partitioning scheme [1] is employed
        !   for large (sub)array(s).  Also, the algorithm uses a tail recursion (instead
        !   of a pure recursion) to minimize the recursive depth and make sure at most
        !   O(log(n)) space is used [2]. <br>
        !  **References**: <br>
        !   [1] <a href="https://en.wikipedia.org/wiki/Quicksort">Quicksort. </a> <br>
        !   [2] <a href="https://www.techiedelight.com/boost-quicksort-performance/">
        !       How to Boost QuickSort Performance? </a> <br>
        MODULE SUBROUTINE QuickLomuto_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickLomuto_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickSortMo3
        !^ **Subroutine Interface**: QuickSortMo3 <br>
        !  **Purpose**:  To sort an array in a desired order using the *QuickSort*
        !       algorithm with the median-of-three (Mo3) partitioning scheme. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickSortMo3(A) <br>
        !  **Technical Information**: <br>
        !   The algorithm used here is actually a hybrid (unstable) algorithm where
        !   the pair-insertion sort algorithm is utilized for small (sub)array(s) and
        !   the quicksort algorithm with median-of-three partitioning scheme [1] is
        !   employed for large (sub)array(s).  Unlike other *QuickSort* procedures,
        !   the algorithm for *QuickSortMo3* procedures uses a pure recursion instead
        !   of a tail recursion since it appears experimentally that the pure recursion
        !   provides a better performance for this particular implementation of the
        !   median-of-three partitioning scheme. <br>
        !  **References**: <br>
        !   [1] <a href="https://en.wikipedia.org/wiki/Quicksort">Quicksort. </a> <br>
        MODULE SUBROUTINE QuickMo3_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickMo3_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickSort3Way
        !^ **Subroutine Interface**: QuickSort3Way <br>
        !  **Purpose**:  To sort an array in a desired order using the *QuickSort*
        !       algorithm with the three-way partitioning scheme. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickSort3Way(A) <br>
        !  **Technical Information**: <br>
        !   The algorithm used here is actually a hybrid (unstable) algorithm where
        !   the pair-insertion sort algorithm is utilized for small (sub)array(s) and
        !   the quicksort algorithm with Bentley and McIlroy's three-way partitioning
        !   scheme [1] is employed for large (sub)array(s).  Also, the algorithm uses
        !   a tail recursion (instead of a pure recursion) to minimize the recursive
        !   depth and make sure at most O(log(n)) space is used [2]. <br>
        !  **References**: <br>
        !   [1] <a href="https://en.wikipedia.org/wiki/Quicksort">Quicksort. </a> <br>
        !   [2] <a href="https://www.techiedelight.com/boost-quicksort-performance/">
        !       How to Boost QuickSort Performance? </a> <br>
        MODULE SUBROUTINE Quick3Way_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE Quick3Way_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickSortVowels
        !^ **Subroutine Interface**: QuickSortVowels <br>
        !  **Purpose**:  To sort an array in a desired order using the *QuickSort*
        !       algorithm based on the *QuickSort Version 3* (the professional version)
        !       by R.A. Vowels [1]. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickSortVowels(A) <br>
        !  **Technical Information**: <br>
        !   The algorithm used here is actually a hybrid (unstable) algorithm where
        !   the pair-insertion sort algorithm is utilized for small (sub)array(s) and
        !   the quicksort algorithm based on the *QuickSort Version 3* (the professional
        !   version) by R.A. Vowels [1] is employed for large (sub)array(s). <br>
        !  **References**: <br>
        !   [1] <a href="http://pages.swcp.com/~walt/fortran_store/Html/Info/books/adsff.html">
        !       Robin A. Vowels. 1998. Algorithms and Data Structures in F and Fortran,
        !       Unicomp. </a> <br>
        MODULE SUBROUTINE QuickVowels_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickVowels_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickSortStable
        !^ **Subroutine Interface**: QuickSortStable <br>
        !  **Purpose**:  To sort an array in a desired order using the *QuickSort*
        !       algorithm with a stable partitioning scheme. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickSortStable(A) <br>
        !  **Technical Information**: <br>
        !   The algorithm used here is actually a hybrid *stable* algorithm where
        !   the insertion sort algorithm is utilized for small (sub)array(s) and
        !   the quicksort algorithm with a stable partitioning scheme is employed
        !   for large (sub)array(s).  Also, the algorithm uses a tail recursion
        !   (instead of a pure recursion) to minimize the recursive depth and make
        !   sure at most O(log(n)) space is used [1]. <br>
        !   It is important to note that although the pair-insertion sort algorithm
        !   should conceptually be a stable algorithm, its implementation in this
        !   library seems to be *unstable*.  Therefore, for all *hybrid stable*
        !   algorithms, the insertion sort is used in place of the pair-insertion
        !   sort although the pair-insertion sort typically provides better overall
        !   performance. <br>
        !  **References**: <br>
        !   [1] <a href="https://www.techiedelight.com/boost-quicksort-performance/">
        !       How to Boost QuickSort Performance? </a> <br>
        MODULE SUBROUTINE QuickStable_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickStable_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickSortIterative
        !^ **Subroutine Interface**: QuickSortIterative <br>
        !  **Purpose**:  To sort an array in a desired order using an *iterative
        !       Quicksort* algorithm with median-of-three (Mo3) partitioning scheme. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickSortIterative(A) <br>
        !  **Technical Information**: <br>
        !   The algorithm used here is actually a hybrid (unstable) algorithm where
        !   the pair-insertion sort algorithm is utilized for small (sub)array(s) and
        !   the *iterative* quicksort algorithm with median-of-three partitioning
        !   scheme is employed for large (sub)array(s).  The implementation of the
        !   iterative quicksort algorithm is based mainly on the *Sort* subroutine
        !   of Numerical Recipes in Fortran 90 [1]. <br>
        !  **References**: <br>
        !   [1] <a href="http://numerical.recipes/oldverswitcher.html">Numerical
        !       Recipes Books Online. </a> <br>
        MODULE SUBROUTINE QuickIterative_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickIterative_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE QuickSortJava
        !^ **Subroutine Interface**: QuickSortJava <br>
        !  **Purpose**:  To sort an array in a desired order using a *dual-pivot
        !       Quicksort* algorithm based on Java's sorting algorithm [1]. <br>
        !  **Usage**: <br>
        !   --->    CALL QuickSortJava(A) <br>
        !  **Technical Information**: <br>
        !   The algorithm used here is actually a hybrid (unstable) algorithm where
        !   the pair-insertion sort algorithm is utilized for small (sub)array(s) and
        !   the *dual-pivot* quicksort algorithm is employed for large (sub)array(s).
        !   The implementation of the dual-pivot quicksort algorithm is based mainly
        !   on Java's sorting algorithm [1]. <br>
        !  **References**: <br>
        !   [1] <a href="https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/DualPivotQuicksort.java">
        !       Java's DualPivotQuicksort class. </a> <br>
        MODULE SUBROUTINE QuickJava_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE QuickJava_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    ! merge
    INTERFACE MergeSortTopDown
        !^ **Subroutine Interface**: MergeSortTopDown <br>
        !  **Purpose**:  To sort an array in a desired order using a *top-down
        !       merge sort* algorithm. <br>
        !  **Usage**: <br>
        !   --->    CALL MergeSortTopDown(A) <br>
        !  **Technical Information**: <br>
        !   The algorithm used here is actually a hybrid stable algorithm where
        !   the insertion sort algorithm is utilized for small (sub)array(s) and
        !   the top-down merge sort algorithm [1] is employed for large (sub)array(s).
        !   The algorithm also considers other improvements as suggested in [2]. <br>
        !  **References**: <br>
        !   [1] <a href="https://en.wikipedia.org/wiki/Merge_sort">Merge sort. </a> <br>
        !   [2] <a href="https://algs4.cs.princeton.edu/22mergesort/">Section 2.2 Merge sort
        !       of Algorithms, 4th Edition by Robert Sedgewick and Kevin Wayne. </a> <br>
        MODULE SUBROUTINE MergeTopDown_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeTopDown_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE MergeSortBottomUp
        !^ **Subroutine Interface**: MergeSortBottomUp <br>
        !  **Purpose**:  To sort an array in a desired order using a *bottom-up
        !       merge sort* algorithm. <br>
        !  **Usage**: <br>
        !   --->    CALL MergeSortBottomUp(A) <br>
        !  **Technical Information**: <br>
        !   The algorithm used here is actually a hybrid stable algorithm where
        !   the insertion sort algorithm is utilized for small (sub)array(s) and
        !   the bottom-up merge sort algorithm [1] is employed for large (sub)array(s). <br>
        !  **References**: <br>
        !   [1] <a href="https://en.wikipedia.org/wiki/Merge_sort">Merge sort. </a> <br>
        MODULE SUBROUTINE MergeBottomUp_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeBottomUp_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE MergeSortRealQPSplit
        !^ **Subroutine Interface**: MergeSortRealQPSplit <br>
        !  **Purpose**:  To sort an array in a desired order using a *top-down
        !       merge sort* algorithm where the given array is split into four
        !       sub-arrays instead of two sub-arrays. <br>
        !  **Usage**: <br>
        !   --->    CALL MergeSortRealQPSplit(A) <br>
        !  **Technical Information**: <br>
        !   The algorithm used here is actually a hybrid stable algorithm where
        !   the insertion sort algorithm is utilized for small (sub)array(s) and
        !   the top-down merge sort algorithm [1] with quad split.  The algorithm
        !   also considers other improvements as suggested in [2]. <br>
        !  **References**: <br>
        !   [1] <a href="https://en.wikipedia.org/wiki/Merge_sort">Merge sort. </a> <br>
        !   [2] <a href="https://algs4.cs.princeton.edu/22mergesort/">Section 2.2 Merge sort
        !       of Algorithms, 4th Edition by Robert Sedgewick and Kevin Wayne. </a> <br>
        MODULE SUBROUTINE MergeQuadSplit_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeQuadSplit_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    INTERFACE MergeSortHalfCopy
        !^ **Subroutine Interface**: MergeSortHalfCopy <br>
        !  **Purpose**:  To sort an array in a desired order using a fast
        !       *mergesort* algorithm based on *half-copying merge* algorithm
        !       by C. Juszczak [1]. <br>
        !  **Usage**: <br>
        !   --->    CALL MergeSortHalfCopy(A) <br>
        !  **Technical Information**: <br>
        !   The algorithm used here is actually a hybrid stable algorithm where
        !   the insertion sort algorithm is utilized for small (sub)array(s) and
        !   the top-down merge sort algorithm with a *half-copying* merging [1]
        !   is employed for large (sub)array(s).  The algorithm also considers
        !   other improvements as suggested in [2]. <br>
        !  **References**: <br>
        !   [1] <a href="http://kicia.ift.uni.wroc.pl/algorytmy/mergesortpaper.pdf">
        !       Juszczak, C. 2007.  Fast mergesort implementation based on half-copying
        !       merge algorithm. </a> <br>
        !   [2] <a href="https://algs4.cs.princeton.edu/22mergesort/">Section 2.2 Merge sort
        !       of Algorithms, 4th Edition by Robert Sedgewick and Kevin Wayne. </a> <br>
        MODULE SUBROUTINE MergeHalfCopy_SortChar(A)
            tCharStar, INTENT(INOUT)    :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_SortI8(A)
            tSInt8,  INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_SortI16(A)
            tSInt16, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_SortI32(A)
            tSInt32, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_SortI64(A)
            tSInt64, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_SortR32(A)
            tRealSP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_SortR64(A)
            tRealDP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_SortR128(A)
            tRealQP, INTENT(INOUT)  :: A(:)
        END SUBROUTINE
        MODULE SUBROUTINE MergeHalfCopy_SortComp(A)
            CLASS(Comparable), INTENT(INOUT)    :: A(:)
        END SUBROUTINE
    END INTERFACE
    ! auxiliary
    INTERFACE IsSortedAscend
        !^ **Function Interface**: IsSortedAscend <br>
        !  **Purpose**:  To check whether the specified array is sorted in
        !       the desired order. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsSortedAscend(A) <br>
        !   --->    IF (.NOT.IsSortedAscend(A)) DoSomething <br>
        MODULE FUNCTION IsSortedAscend_Char(A) RESULT(Flag)
            tCharStar, INTENT(IN)   :: A(:)
            tLogical                :: Flag
        END FUNCTION
        MODULE FUNCTION IsSortedAscend_I8(A) RESULT(Flag)
            tSInt8,  INTENT(IN) :: A(:)
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsSortedAscend_I16(A) RESULT(Flag)
            tSInt16, INTENT(IN) :: A(:)
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsSortedAscend_I32(A) RESULT(Flag)
            tSInt32, INTENT(IN) :: A(:)
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsSortedAscend_I64(A) RESULT(Flag)
            tSInt64, INTENT(IN) :: A(:)
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsSortedAscend_R32(A) RESULT(Flag)
            tRealSP, INTENT(IN) :: A(:)
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsSortedAscend_R64(A) RESULT(Flag)
            tRealDP, INTENT(IN) :: A(:)
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsSortedAscend_R128(A) RESULT(Flag)
            tRealQP, INTENT(IN) :: A(:)
            tLogical            :: Flag
        END FUNCTION
        MODULE FUNCTION IsSortedAscend_Comp(A) RESULT(Flag)
            CLASS(Comparable), INTENT(IN)   :: A(:)
            tLogical                        :: Flag
        END FUNCTION
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

END MODULE MBase_SortAscend

!******************************************************************************
