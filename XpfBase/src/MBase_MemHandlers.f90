
MODULE MBase_MemHandlers

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains memory-handling routines for all intrinsic types.  Currently, the specified
!   arguments can have either *ALLOCATABLE* or *POINTER* attribute and its rank can be any number
!   between 1 and 7. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_SIntUtil,         ONLY: ToChar => ToDecStrSigned

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: MemFree
    PUBLIC :: MemAlloc
    PUBLIC :: MemResize

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MBase_MemHandlers'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE MemFree
        !^ **Subroutine Interface**: MemFree <br>
        !  **Purpose**:  To free memory of the specified argument. <br>
        !  **Usage**: <br>
        !   --->    CALL MemFree(A) <br>
        MODULE PROCEDURE INT8_MemFree_Alloc
        MODULE PROCEDURE INT8_MemFree_Ptr
        MODULE PROCEDURE INT16_MemFree_Alloc
        MODULE PROCEDURE INT16_MemFree_Ptr
        MODULE PROCEDURE INT32_MemFree_Alloc
        MODULE PROCEDURE INT32_MemFree_Ptr
        MODULE PROCEDURE INT64_MemFree_Alloc
        MODULE PROCEDURE INT64_MemFree_Ptr
        MODULE PROCEDURE REAL32_MemFree_Alloc
        MODULE PROCEDURE REAL32_MemFree_Ptr
        MODULE PROCEDURE REAL64_MemFree_Alloc
        MODULE PROCEDURE REAL64_MemFree_Ptr
        MODULE PROCEDURE REAL128_MemFree_Alloc
        MODULE PROCEDURE REAL128_MemFree_Ptr
        MODULE PROCEDURE CMPX32_MemFree_Alloc
        MODULE PROCEDURE CMPX32_MemFree_Ptr
        MODULE PROCEDURE CMPX64_MemFree_Alloc
        MODULE PROCEDURE CMPX64_MemFree_Ptr
        MODULE PROCEDURE CMPX128_MemFree_Alloc
        MODULE PROCEDURE CMPX128_MemFree_Ptr
        MODULE PROCEDURE LOGICAL_MemFree_Alloc
        MODULE PROCEDURE LOGICAL_MemFree_Ptr
        MODULE PROCEDURE CHARACTER_MemFree_Alloc
        MODULE PROCEDURE CHARACTER_MemFree_Ptr
    END INTERFACE
    INTERFACE MemAlloc
        !^ **Subroutine Interface**: MemAlloc <br>
        !  **Purpose**:  To allocate memory of the specified argument. <br>
        !  **Usage**: <br>
        !   ! allocate a character array of any rank <br>
        !   --->    CALL MemAlloc(Arr1D, 64, [25])                      ! character length of 64 <br>
        !   --->    CALL MemAlloc(Arr1D, 128, [50], StartID=[-20])      ! character length of 128 <br>
        !   --->    CALL MemAlloc(Arr2D, 64, [30, 30])                  ! character length of 64 <br>
        !   --->    CALL MemAlloc(Arr2D, 32, [30, 30], StartID=[-5, 5]) ! character length of 32 <br>
        !   --->    CALL MemAlloc(Arr7D, CharLen, ArrSizes) <br>
        !   --->    CALL MemAlloc(Arr7D, CharLen, ArrSizes, StartIDs) <br>
        !   ! allocate a 1-D array of intrinsic types (except CHARACTER) <br>
        !   --->    CALL MemAlloc(Arr1D, 25) <br>
        !   --->    CALL MemAlloc(Arr1D, 50, StartID=-20) <br>
        !   --->    CALL MemAlloc(Arr1D, [25]) <br>
        !   --->    CALL MemAlloc(Arr1D, [50], StartID=[-20]) <br>
        !   ! allocate an array of any rank of intrinsic types (except CHARACTER) <br>
        !   --->    CALL MemAlloc(Arr2D, [30, 30]) <br>
        !   --->    CALL MemAlloc(Arr2D, [30, 30], StartID=[-10, 10]) <br>
        !   --->    CALL MemAlloc(Arr3D, [30, 40, 50]) <br>
        !   --->    CALL MemAlloc(Arr3D, [30, 40, 50], StartID=[0, 0, 0]) <br>
        !   --->    CALL MemAlloc(Arr7D, ASize) <br>
        !   --->    CALL MemAlloc(Arr7D, ASize, StartID) <br>
        !  **Note**: For an array of rank greater than 1, the "ASize" and "StartID" arguments must be
        !       arrays where their size are equal to the rank of the specified array argument. <br>
        MODULE PROCEDURE INT8_MemAlloc_Alloc1D
        MODULE PROCEDURE INT8_MemAlloc_Ptr1D
        MODULE PROCEDURE INT16_MemAlloc_Alloc1D
        MODULE PROCEDURE INT16_MemAlloc_Ptr1D
        MODULE PROCEDURE INT32_MemAlloc_Alloc1D
        MODULE PROCEDURE INT32_MemAlloc_Ptr1D
        MODULE PROCEDURE INT64_MemAlloc_Alloc1D
        MODULE PROCEDURE INT64_MemAlloc_Ptr1D
        MODULE PROCEDURE REAL32_MemAlloc_Alloc1D
        MODULE PROCEDURE REAL32_MemAlloc_Ptr1D
        MODULE PROCEDURE REAL64_MemAlloc_Alloc1D
        MODULE PROCEDURE REAL64_MemAlloc_Ptr1D
        MODULE PROCEDURE REAL128_MemAlloc_Alloc1D
        MODULE PROCEDURE REAL128_MemAlloc_Ptr1D
        MODULE PROCEDURE CMPX32_MemAlloc_Alloc1D
        MODULE PROCEDURE CMPX32_MemAlloc_Ptr1D
        MODULE PROCEDURE CMPX64_MemAlloc_Alloc1D
        MODULE PROCEDURE CMPX64_MemAlloc_Ptr1D
        MODULE PROCEDURE CMPX128_MemAlloc_Alloc1D
        MODULE PROCEDURE CMPX128_MemAlloc_Ptr1D
        MODULE PROCEDURE LOGICAL_MemAlloc_Alloc1D
        MODULE PROCEDURE LOGICAL_MemAlloc_Ptr1D
        !-----------------------------------------
        MODULE PROCEDURE INT8_MemAlloc_Alloc
        MODULE PROCEDURE INT8_MemAlloc_Ptr
        MODULE PROCEDURE INT16_MemAlloc_Alloc
        MODULE PROCEDURE INT16_MemAlloc_Ptr
        MODULE PROCEDURE INT32_MemAlloc_Alloc
        MODULE PROCEDURE INT32_MemAlloc_Ptr
        MODULE PROCEDURE INT64_MemAlloc_Alloc
        MODULE PROCEDURE INT64_MemAlloc_Ptr
        MODULE PROCEDURE REAL32_MemAlloc_Alloc
        MODULE PROCEDURE REAL32_MemAlloc_Ptr
        MODULE PROCEDURE REAL64_MemAlloc_Alloc
        MODULE PROCEDURE REAL64_MemAlloc_Ptr
        MODULE PROCEDURE REAL128_MemAlloc_Alloc
        MODULE PROCEDURE REAL128_MemAlloc_Ptr
        MODULE PROCEDURE CMPX32_MemAlloc_Alloc
        MODULE PROCEDURE CMPX32_MemAlloc_Ptr
        MODULE PROCEDURE CMPX64_MemAlloc_Alloc
        MODULE PROCEDURE CMPX64_MemAlloc_Ptr
        MODULE PROCEDURE CMPX128_MemAlloc_Alloc
        MODULE PROCEDURE CMPX128_MemAlloc_Ptr
        MODULE PROCEDURE LOGICAL_MemAlloc_Alloc
        MODULE PROCEDURE LOGICAL_MemAlloc_Ptr
        MODULE PROCEDURE CHARACTER_MemAlloc_Alloc
        MODULE PROCEDURE CHARACTER_MemAlloc_Ptr
    END INTERFACE
    INTERFACE MemResize
        !^ **Subroutine Interface**: MemResize <br>
        !  **Purpose**:  To re-allocate memory of the specified argument (and preserve its data). <br>
        !  **Usage**: <br>
        !   --->    CALL MemResize(Arr1D, [25]) <br>
        !   --->    CALL MemResize(Arr2D, [30, 30]) <br>
        !   --->    CALL MemResize(Arr3D, [30, 40, 50]) <br>
        !   --->    CALL MemResize(Arr7D, NewSize) <br>
        !  **Note**: The "NewSize" argument must be an array where its size is equal to the rank of
        !       the specified array argument.  <br>
        MODULE PROCEDURE INT8_MemResize_Alloc
        MODULE PROCEDURE INT8_MemResize_Ptr
        MODULE PROCEDURE INT16_MemResize_Alloc
        MODULE PROCEDURE INT16_MemResize_Ptr
        MODULE PROCEDURE INT32_MemResize_Alloc
        MODULE PROCEDURE INT32_MemResize_Ptr
        MODULE PROCEDURE INT64_MemResize_Alloc
        MODULE PROCEDURE INT64_MemResize_Ptr
        MODULE PROCEDURE REAL32_MemResize_Alloc
        MODULE PROCEDURE REAL32_MemResize_Ptr
        MODULE PROCEDURE REAL64_MemResize_Alloc
        MODULE PROCEDURE REAL64_MemResize_Ptr
        MODULE PROCEDURE REAL128_MemResize_Alloc
        MODULE PROCEDURE REAL128_MemResize_Ptr
        MODULE PROCEDURE CMPX32_MemResize_Alloc
        MODULE PROCEDURE CMPX32_MemResize_Ptr
        MODULE PROCEDURE CMPX64_MemResize_Alloc
        MODULE PROCEDURE CMPX64_MemResize_Ptr
        MODULE PROCEDURE CMPX128_MemResize_Alloc
        MODULE PROCEDURE CMPX128_MemResize_Ptr
        MODULE PROCEDURE LOGICAL_MemResize_Alloc
        MODULE PROCEDURE LOGICAL_MemResize_Ptr
        MODULE PROCEDURE CHARACTER_MemResize_Alloc
        MODULE PROCEDURE CHARACTER_MemResize_Ptr
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!+++++ INTEGER 8 BITS +++++
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            INTEGER(KIND=kInt8)
#define MemFree_Generic     INT8_MemFree_Alloc
#define MemAlloc_Generic    INT8_MemAlloc_Alloc
#define MemAlloc_OneD       INT8_MemAlloc_Alloc1D
#define MemResize_Generic   INT8_MemResize_Alloc
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic
!-----
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define TypeName            INTEGER(KIND=kInt8)
#define MemFree_Generic     INT8_MemFree_Ptr
#define MemAlloc_Generic    INT8_MemAlloc_Ptr
#define MemAlloc_OneD       INT8_MemAlloc_Ptr1D
#define MemResize_Generic   INT8_MemResize_Ptr
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic

!******************************************************************************

!+++++ INTEGER 16 BITS +++++
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            INTEGER(KIND=kInt16)
#define MemFree_Generic     INT16_MemFree_Alloc
#define MemAlloc_Generic    INT16_MemAlloc_Alloc
#define MemAlloc_OneD       INT16_MemAlloc_Alloc1D
#define MemResize_Generic   INT16_MemResize_Alloc
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic
!-----
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define TypeName            INTEGER(KIND=kInt16)
#define MemFree_Generic     INT16_MemFree_Ptr
#define MemAlloc_Generic    INT16_MemAlloc_Ptr
#define MemAlloc_OneD       INT16_MemAlloc_Ptr1D
#define MemResize_Generic   INT16_MemResize_Ptr
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic

!******************************************************************************

!+++++ INTEGER 32 BITS +++++
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            INTEGER(KIND=kInt32)
#define MemFree_Generic     INT32_MemFree_Alloc
#define MemAlloc_Generic    INT32_MemAlloc_Alloc
#define MemAlloc_OneD       INT32_MemAlloc_Alloc1D
#define MemResize_Generic   INT32_MemResize_Alloc
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic
!-----
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define TypeName            INTEGER(KIND=kInt32)
#define MemFree_Generic     INT32_MemFree_Ptr
#define MemAlloc_Generic    INT32_MemAlloc_Ptr
#define MemAlloc_OneD       INT32_MemAlloc_Ptr1D
#define MemResize_Generic   INT32_MemResize_Ptr
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic

!******************************************************************************

!+++++ INTEGER 64 BITS +++++
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            INTEGER(KIND=kInt64)
#define MemFree_Generic     INT64_MemFree_Alloc
#define MemAlloc_Generic    INT64_MemAlloc_Alloc
#define MemAlloc_OneD       INT64_MemAlloc_Alloc1D
#define MemResize_Generic   INT64_MemResize_Alloc
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic
!-----
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define TypeName            INTEGER(KIND=kInt64)
#define MemFree_Generic     INT64_MemFree_Ptr
#define MemAlloc_Generic    INT64_MemAlloc_Ptr
#define MemAlloc_OneD       INT64_MemAlloc_Ptr1D
#define MemResize_Generic   INT64_MemResize_Ptr
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic

!******************************************************************************

!+++++ REAL 32 BITS +++++
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            REAL(KIND=kSingle)
#define MemFree_Generic     REAL32_MemFree_Alloc
#define MemAlloc_Generic    REAL32_MemAlloc_Alloc
#define MemAlloc_OneD       REAL32_MemAlloc_Alloc1D
#define MemResize_Generic   REAL32_MemResize_Alloc
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic
!-----
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define TypeName            REAL(KIND=kSingle)
#define MemFree_Generic     REAL32_MemFree_Ptr
#define MemAlloc_Generic    REAL32_MemAlloc_Ptr
#define MemAlloc_OneD       REAL32_MemAlloc_Ptr1D
#define MemResize_Generic   REAL32_MemResize_Ptr
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic

!******************************************************************************

!+++++ REAL 64 BITS +++++
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            REAL(KIND=kDouble)
#define MemFree_Generic     REAL64_MemFree_Alloc
#define MemAlloc_Generic    REAL64_MemAlloc_Alloc
#define MemAlloc_OneD       REAL64_MemAlloc_Alloc1D
#define MemResize_Generic   REAL64_MemResize_Alloc
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic
!-----
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define TypeName            REAL(KIND=kDouble)
#define MemFree_Generic     REAL64_MemFree_Ptr
#define MemAlloc_Generic    REAL64_MemAlloc_Ptr
#define MemAlloc_OneD       REAL64_MemAlloc_Ptr1D
#define MemResize_Generic   REAL64_MemResize_Ptr
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic

!******************************************************************************

!+++++ REAL 128 BITS +++++
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            REAL(KIND=kQuad)
#define MemFree_Generic     REAL128_MemFree_Alloc
#define MemAlloc_Generic    REAL128_MemAlloc_Alloc
#define MemAlloc_OneD       REAL128_MemAlloc_Alloc1D
#define MemResize_Generic   REAL128_MemResize_Alloc
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic
!-----
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define TypeName            REAL(KIND=kQuad)
#define MemFree_Generic     REAL128_MemFree_Ptr
#define MemAlloc_Generic    REAL128_MemAlloc_Ptr
#define MemAlloc_OneD       REAL128_MemAlloc_Ptr1D
#define MemResize_Generic   REAL128_MemResize_Ptr
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic

!******************************************************************************

!+++++ COMPLEX 32 BITS +++++
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            COMPLEX(KIND=kSingle)
#define MemFree_Generic     CMPX32_MemFree_Alloc
#define MemAlloc_Generic    CMPX32_MemAlloc_Alloc
#define MemAlloc_OneD       CMPX32_MemAlloc_Alloc1D
#define MemResize_Generic   CMPX32_MemResize_Alloc
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic
!-----
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define TypeName            COMPLEX(KIND=kSingle)
#define MemFree_Generic     CMPX32_MemFree_Ptr
#define MemAlloc_Generic    CMPX32_MemAlloc_Ptr
#define MemAlloc_OneD       CMPX32_MemAlloc_Ptr1D
#define MemResize_Generic   CMPX32_MemResize_Ptr
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic

!******************************************************************************

!+++++ COMPLEX 64 BITS +++++
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            COMPLEX(KIND=kDouble)
#define MemFree_Generic     CMPX64_MemFree_Alloc
#define MemAlloc_Generic    CMPX64_MemAlloc_Alloc
#define MemAlloc_OneD       CMPX64_MemAlloc_Alloc1D
#define MemResize_Generic   CMPX64_MemResize_Alloc
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic
!-----
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define TypeName            COMPLEX(KIND=kDouble)
#define MemFree_Generic     CMPX64_MemFree_Ptr
#define MemAlloc_Generic    CMPX64_MemAlloc_Ptr
#define MemAlloc_OneD       CMPX64_MemAlloc_Ptr1D
#define MemResize_Generic   CMPX64_MemResize_Ptr
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic

!******************************************************************************

!+++++ COMPLEX 128 BITS +++++
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            COMPLEX(KIND=kQuad)
#define MemFree_Generic     CMPX128_MemFree_Alloc
#define MemAlloc_Generic    CMPX128_MemAlloc_Alloc
#define MemAlloc_OneD       CMPX128_MemAlloc_Alloc1D
#define MemResize_Generic   CMPX128_MemResize_Alloc
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic
!-----
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define TypeName            COMPLEX(KIND=kQuad)
#define MemFree_Generic     CMPX128_MemFree_Ptr
#define MemAlloc_Generic    CMPX128_MemAlloc_Ptr
#define MemAlloc_OneD       CMPX128_MemAlloc_Ptr1D
#define MemResize_Generic   CMPX128_MemResize_Ptr
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic

!******************************************************************************

!+++++ DEFAULT LOGICAL +++++
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            LOGICAL
#define MemFree_Generic     LOGICAL_MemFree_Alloc
#define MemAlloc_Generic    LOGICAL_MemAlloc_Alloc
#define MemAlloc_OneD       LOGICAL_MemAlloc_Alloc1D
#define MemResize_Generic   LOGICAL_MemResize_Alloc
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic
!-----
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define TypeName            LOGICAL
#define MemFree_Generic     LOGICAL_MemFree_Ptr
#define MemAlloc_Generic    LOGICAL_MemAlloc_Ptr
#define MemAlloc_OneD       LOGICAL_MemAlloc_Ptr1D
#define MemResize_Generic   LOGICAL_MemResize_Ptr
#include    "Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic

!******************************************************************************

!+++++ CHARACTER +++++
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define MemFree_Generic     CHARACTER_MemFree_Alloc
#define MemAlloc_Generic    CHARACTER_MemAlloc_Alloc
#define MemResize_Generic   CHARACTER_MemResize_Alloc
#include    "Includes/Character_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemResize_Generic
!-----
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define MemFree_Generic     CHARACTER_MemFree_Ptr
#define MemAlloc_Generic    CHARACTER_MemAlloc_Ptr
#define MemResize_Generic   CHARACTER_MemResize_Ptr
#include    "Includes/Character_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemResize_Generic

!******************************************************************************

END MODULE MBase_MemHandlers

!******************************************************************************
