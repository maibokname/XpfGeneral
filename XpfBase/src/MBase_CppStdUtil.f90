#define HAVE_FLIBCPP

#ifdef  HAVE_FLIBCPP
MODULE MBase_CppStdUtil

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module provides interfaces/wrappers to the C++ standard library through
!   the *FLibCpp* library.

!** USE STATEMENTS:
    USE, INTRINSIC :: ISO_C_BINDING
    USE MBase_Common
    USE MLib_FLC_Algorithm
    USE MLib_FLC_Random
    USE MLib_FLC_String,  ONLY: CStd_String => String, CStd_S2F => stof, CStd_S2D => stod, &
                                CStd_S2I => stoi, CStd_S2L => stoll
    USE MLib_FLC_Map,     ONLY: CStd_MapII => MapIntInt, CStd_MapSI => MapStringInt, &
                                CStd_MapSS => MapStringString
    USE MLib_FLC_Set,     ONLY: CStd_SetInt => SetInt, CStd_SetStr => SetString
    USE MLib_FLC_Vector,  ONLY: CStd_VecI32 => VectorInt4, CStd_VecI64 => VectorInt8, &
                                CStd_VecStr => VectorString, CStd_VecCmpx => VectorComplex8, &
                                CStd_VecReal => VectorReal8

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! algorithms
    PUBLIC :: CStd_BinarySearch
    PUBLIC :: CStd_Shuffle
    PUBLIC :: CStd_Sort
    PUBLIC :: CStd_ArgSort
    PUBLIC :: CStd_IsSorted
    ! random generators
    PUBLIC :: MersenneEngine4
    PUBLIC :: MersenneEngine8
    ! strings
    PUBLIC :: CStd_String
    PUBLIC :: CStd_S2F
    PUBLIC :: CStd_S2D
    PUBLIC :: CStd_S2I
    PUBLIC :: CStd_S2L
    ! maps
    PUBLIC :: CStd_MapII
    PUBLIC :: CStd_MapSI
    PUBLIC :: CStd_MapSS
    ! sets
    PUBLIC :: CStd_SetInt
    PUBLIC :: CStd_SetStr
    ! vectors
    PUBLIC :: CStd_VecI32
    PUBLIC :: CStd_VecI64
    PUBLIC :: CStd_VecStr
    PUBLIC :: CStd_VecCmpx
    PUBLIC :: CStd_VecReal

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'MBase_CppStdUtil'   ! module name

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE CStd_BinarySearch
        MODULE PROCEDURE CStd_BinarySearch_I32
        MODULE PROCEDURE CStd_BinarySearch_I64
        MODULE PROCEDURE CStd_BinarySearch_Double
    END INTERFACE
    INTERFACE CStd_Shuffle
        MODULE PROCEDURE CStd_Shuffle_I32
        MODULE PROCEDURE CStd_Shuffle_I64
        MODULE PROCEDURE CStd_Shuffle_Double
    END INTERFACE
    INTERFACE CStd_Sort
        MODULE PROCEDURE CStd_Sort_I32
        MODULE PROCEDURE CStd_Sort_I64
        MODULE PROCEDURE CStd_Sort_Double
    END INTERFACE
    INTERFACE CStd_ArgSort
        MODULE PROCEDURE CStd_ArgSort_I32
        MODULE PROCEDURE CStd_ArgSort_I64
        MODULE PROCEDURE CStd_ArgSort_Double
    END INTERFACE
    INTERFACE CStd_IsSorted
        MODULE PROCEDURE CStd_IsSorted_I32
        MODULE PROCEDURE CStd_IsSorted_I64
        MODULE PROCEDURE CStd_IsSorted_Double
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION CStd_BinarySearch_I32(Arr, Val) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform binary searching.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: Arr(:), Val
    tIndex              :: Index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Index = Binary_Search(Arr, Val)

    RETURN

END FUNCTION CStd_BinarySearch_I32

!******************************************************************************

FUNCTION CStd_BinarySearch_I64(Arr, Val) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform binary searching.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Arr(:), Val
    tIndex              :: Index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Index = Binary_Search(Arr, Val)

    RETURN

END FUNCTION CStd_BinarySearch_I64

!******************************************************************************

FUNCTION CStd_BinarySearch_Double(Arr, Val) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform binary searching.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealDP,  INTENT(IN) :: Arr(:), Val
    tIndex              :: Index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Index = Binary_Search(Arr, Val)

    RETURN

END FUNCTION CStd_BinarySearch_Double

!******************************************************************************

SUBROUTINE CStd_Shuffle_I32(Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform shuffling.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(INOUT)  :: Arr(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(MersenneEngine4)   :: Rng

!** FLOW

    CALL Rng%Seed(31131313)
    CALL Shuffle(Rng, Arr)
    CALL Rng%Release()

    RETURN

END SUBROUTINE CStd_Shuffle_I32

!******************************************************************************

SUBROUTINE CStd_Shuffle_I64(Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform shuffling.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(INOUT)  :: Arr(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(MersenneEngine4)   :: Rng

!** FLOW

    CALL Rng%Seed(31131313)
    CALL Shuffle(Rng, Arr)
    CALL Rng%Release()

    RETURN

END SUBROUTINE CStd_Shuffle_I64

!******************************************************************************

SUBROUTINE CStd_Shuffle_Double(Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform shuffling.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealDP,  INTENT(INOUT)  :: Arr(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(MersenneEngine4)   :: Rng

!** FLOW

    CALL Rng%Seed(31131313)
    CALL Shuffle(Rng, Arr)
    CALL Rng%Release()

    RETURN

END SUBROUTINE CStd_Shuffle_Double

!******************************************************************************

SUBROUTINE CStd_Sort_I32(Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform sorting.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(INOUT)  :: Arr(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL Sort(Arr)

    RETURN

END SUBROUTINE CStd_Sort_I32

!******************************************************************************

SUBROUTINE CStd_Sort_I64(Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform sorting.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(INOUT)  :: Arr(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL Sort(Arr)

    RETURN

END SUBROUTINE CStd_Sort_I64

!******************************************************************************

SUBROUTINE CStd_Sort_Double(Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform sorting.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealDP,  INTENT(INOUT)  :: Arr(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL Sort(Arr)

    RETURN

END SUBROUTINE CStd_Sort_Double

!******************************************************************************

SUBROUTINE CStd_ArgSort_I32(Arr, Ind)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform sorting.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN)     :: Arr(:)
    tSInt32, INTENT(INOUT)  :: Ind(SIZE(Arr))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL ArgSort(Arr, Ind)

    RETURN

END SUBROUTINE CStd_ArgSort_I32

!******************************************************************************

SUBROUTINE CStd_ArgSort_I64(Arr, Ind)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform sorting.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN)     :: Arr(:)
    tSInt32, INTENT(INOUT)  :: Ind(SIZE(Arr))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL ArgSort(Arr, Ind)

    RETURN

END SUBROUTINE CStd_ArgSort_I64

!******************************************************************************

SUBROUTINE CStd_ArgSort_Double(Arr, Ind)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform sorting.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealDP,  INTENT(IN)     :: Arr(:)
    tSInt32,  INTENT(INOUT)  :: Ind(SIZE(Arr))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL ArgSort(Arr, Ind)

    RETURN

END SUBROUTINE CStd_ArgSort_Double

!******************************************************************************

FUNCTION CStd_IsSorted_I32(Arr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given array is sorted or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: Arr(:)
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = Is_Sorted(Arr)

    RETURN

END FUNCTION CStd_IsSorted_I32

!******************************************************************************

FUNCTION CStd_IsSorted_I64(Arr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given array is sorted or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Arr(:)
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = Is_Sorted(Arr)

    RETURN

END FUNCTION CStd_IsSorted_I64

!******************************************************************************

FUNCTION CStd_IsSorted_Double(Arr) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the given array is sorted or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealDP,  INTENT(IN) :: Arr(:)
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = Is_Sorted(Arr)

    RETURN

END FUNCTION CStd_IsSorted_Double

!******************************************************************************

END MODULE MBase_CppStdUtil

!******************************************************************************
#endif
