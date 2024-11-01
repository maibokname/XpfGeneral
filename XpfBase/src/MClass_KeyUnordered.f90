
MODULE MClass_KeyUnordered

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *KeyUnordered* type and its related routines.  The *KeyUnordered*
!   type is a data type that provides a storage for an unordered key.  It can be used to store
!   any data types, except the *LOGICAL* type.  The *KeyUnordered* type is a subtype extended
!   from the *GenData* type. <br>
!   *Note*: By design, the *GenData* type requires the user to supply the *MemoryPool* type
!   when the *Set* method is called.  This means that the user is an owner of the specified
!   *MemoryPool* type and must be responsible for managing the memory pool.  If the type of
!   the data to be stored is a derived one, the user may specify a null pointer as an argument
!   to the *Set* method. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MClass_MemoryPool
    USE MClass_GenData

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: KeyUnordered
    ! auxiliary procedure
    PUBLIC :: IsKeyUnordered
    ! memory-handling procedures
    PUBLIC :: MemFree
    PUBLIC :: MemAlloc
    PUBLIC :: MemResize

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_KeyUnordered'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    !> The *KeyUnordered* type is a storage type that can hold any data types,
    !  except the *LOGICAL* type.
    TYPE, EXTENDS(GenData)  :: KeyUnordered
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Overridden Procedure                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => KeyUnordered_ToString
        !> **Type-Bound Subroutine**: Set <br>
        ! **Purpose**:  To store the specified data content in the storage. <br>
        !  **Usage**: <br>
        !   --->    CALL Store%Set(Data, Pool)
        PROCEDURE   :: Set          => KeyUnordered_SetData
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: KeyUnordered_Finalize
        ! ---------------------------------------------------------------------
    END TYPE KeyUnordered

!** INTERFACE DEFINITIONS:
    INTERFACE MemFree
        !^ **Subroutine Interface**: MemFree <br>
        !  **Purpose**:  To free memory of the specified argument. <br>
        !  **Usage**: <br>
        !   --->    CALL MemFree(A) <br>
        MODULE PROCEDURE KeyUnordered_MemFree_Alloc
        MODULE PROCEDURE KeyUnordered_MemFree_Ptr
    END INTERFACE
    INTERFACE MemAlloc
        !^ **Subroutine Interface**: MemAlloc <br>
        !  **Purpose**:  To allocate memory of the specified argument. <br>
        !  **Usage**: <br>
        !   ! allocate a 1-D array <br>
        !   --->    CALL MemAlloc(Arr1D, 25) <br>
        !   --->    CALL MemAlloc(Arr1D, 50, StartID=-20) <br>
        !   --->    CALL MemAlloc(Arr1D, [25]) <br>
        !   --->    CALL MemAlloc(Arr1D, [50], StartID=[-20]) <br>
        !   ! allocate an array of any rank <br>
        !   --->    CALL MemAlloc(Arr2D, [30, 30]) <br>
        !   --->    CALL MemAlloc(Arr2D, [30, 30], StartID=[-10, 10]) <br>
        !   --->    CALL MemAlloc(Arr3D, [30, 40, 50]) <br>
        !   --->    CALL MemAlloc(Arr3D, [30, 40, 50], StartID=[0, 0, 0]) <br>
        !   --->    CALL MemAlloc(Arr7D, ASize) <br>
        !   --->    CALL MemAlloc(Arr7D, ASize, StartID) <br>
        !  **Note**: For an array of rank greater than 1, the "ASize" and "StartID" arguments must be
        !       arrays where their size are equal to the rank of the specified array argument. <br>
        MODULE PROCEDURE KeyUnordered_MemAlloc_Alloc
        MODULE PROCEDURE KeyUnordered_MemAlloc_Ptr
        MODULE PROCEDURE KeyUnordered_MemAlloc_Alloc1D
        MODULE PROCEDURE KeyUnordered_MemAlloc_Ptr1D
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
        MODULE PROCEDURE KeyUnordered_MemResize_Alloc
        MODULE PROCEDURE KeyUnordered_MemResize_Ptr
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE KeyUnordered_Finalize(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform finalization of the KeyUnordered object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(KeyUnordered), INTENT(INOUT)    :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%MemFree()

    RETURN

END SUBROUTINE KeyUnordered_Finalize

!******************************************************************************

FUNCTION KeyUnordered_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyUnordered), INTENT(IN) :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: BaseStr

! FLOW
    
    IF (Obj%IsEmpty()) THEN
        Str = '{KeyUnordered: NULL}'
        RETURN
    END IF

    ! get base string
    BaseStr = Obj%GenData%ToString()

    ! set string
    Str = '{KeyUnordered : ' // BaseStr(12:) // '}'

    RETURN

END FUNCTION KeyUnordered_ToString

!******************************************************************************

SUBROUTINE KeyUnordered_SetData(Store, Content, Pool)

!** PURPOSE OF THIS SUBROUTINE:
    !! To store the data content in the *KeyUnordered* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyUnordered),      INTENT(INOUT) :: Store    !! *KeyUnordered* object
    CLASS(*),                 INTENT(IN)    :: Content  !! data content
    TYPE(MemoryPool), TARGET, INTENT(INOUT) :: Pool     !! memory pool that actually stores the data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (IsKeyUnordered(Content)) THEN
        CALL Store%GenData%Set(Content, Pool)
    ELSE
        CALL Handle_ErrLevel('KeyUnordered_SetData', ModName, ErrSevere, &
            'Type of the specified input is NOT a valid unordered key type.')
    END IF

    RETURN

END SUBROUTINE KeyUnordered_SetData

!******************************************************************************

FUNCTION IsKeyUnordered(Content) RESULT(Flag)

!** PURPOSE OF THIS FUNCTION:
    !^ To check whether type of the specified input is a valid *KeyUnordered* type.

!** FUNCTION ARGUMENT DEFINITIONS:
    CLASS(*), INTENT(IN)    :: Content
    tLogical                :: Flag

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (GetDataType(Content))
    CASE (TYPE_INT8:TYPE_CMPX128, TYPE_CHARACTER:TYPE_DERIVED)
        Flag = TrueVal
    CASE DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION IsKeyUnordered

!******************************************************************************

!+++++ Macro Definitions for Memory-Handling Procedures +++++
! allocatable procedures
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            TYPE(KeyUnordered)
#define MemFree_Generic     KeyUnordered_MemFree_Alloc
#define MemAlloc_Generic    KeyUnordered_MemAlloc_Alloc
#define MemAlloc_OneD       KeyUnordered_MemAlloc_Alloc1D
#define MemResize_Generic   KeyUnordered_MemResize_Alloc
#include    "../Core/Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic
!-----
! pointer procedures
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define TypeName            TYPE(KeyUnordered)
#define MemFree_Generic     KeyUnordered_MemFree_Ptr
#define MemAlloc_Generic    KeyUnordered_MemAlloc_Ptr
#define MemAlloc_OneD       KeyUnordered_MemAlloc_Ptr1D
#define MemResize_Generic   KeyUnordered_MemResize_Ptr
#include    "../Core/Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic

!******************************************************************************

END MODULE MClass_KeyUnordered

!******************************************************************************
