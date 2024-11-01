
MODULE MClass_Object

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Object* type and its related routines.  The *Object* type is an
!   abstract data type (ADT) representing an object that can make a copy of itself.  It defines
!   an application programming interface (API) for various common operations shared by most
!   objects.  By design, the *Object* type is intended to be a base type (superclass) for all
!   other derived types with object-oriented programming (OOP) implementation.  Hence, all other
!   derived types should extend from this base type (i.e being its subclasses). <br>

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#include    "Includes/Macro - MemHandling.f90"
#define     tHash       tIndex

!** USE STATEMENTS:
    USE, INTRINSIC :: ISO_FORTRAN_ENV,  ONLY: CHARACTER_STORAGE_SIZE
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_ByteUtil,                 ONLY: AnyType_2_ByteArrPtr
#ifdef Indx32Bits
    USE MBase_SimpleHash32,             ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64,             ONLY: ComputeHash => Hash64_FNV1a
#endif

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Object
    PUBLIC :: ASSIGNMENT(=)
    PUBLIC :: MemFree
    PUBLIC :: MemAlloc
    PUBLIC :: MemResize

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'MClass_Object'
    tSInt32,   PARAMETER    :: MsgLen = 128
    tSInt32,   PARAMETER    :: kHash = kIndex
    tHash,     PARAMETER    :: HashSeed = 313131_kHash

!** DERIVED TYPE 
    !>  The *Object* type is an abstract data type that must be able to make a copy of itself.
    !   It defines an API for various common operations shared by most objects.  <br>
    TYPE, ABSTRACT :: Object
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *Copy* is a deferred procedure to perform a copy of this (source) object.
        PROCEDURE(IfaceCopy),    DEFERRED   :: Copy
        !> *IsEqualTo* is a deferred procedure to compare whether this object is equal to the
        !  other object or not.
        PROCEDURE(IfaceEqual),   DEFERRED   :: IsEqualTo
        !> *MemFree* is a deferred procedure to free storage currently occupied by this object.
        PROCEDURE(IfaceFree),    DEFERRED   :: MemFree
        !> *ToString* is a deferred procedure to get the string representation of this object.
        PROCEDURE(Iface2String), DEFERRED   :: ToString
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedure                      -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        !  **Note**: The method provided here as a default implementation compute the hash code
        !       based on the string representation of this object and a simple hash algorithm.
        !       It should be overridden by a better and more efficient implementation if possible. <br>
        PROCEDURE   :: HashCode => Object_HashCode
        ! ---------------------------------------------------------------------
    END TYPE Object

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> *IfaceCopy* is an interface for a procedure to copy the source object to the destination
        !  object.  The procedure typically provides a value-copying task for the object's components.
        !  However, for objects with allocatable and/or pointer components, the procedure may also
        !  provide a storage allocation task of those allocatable and/or pointer components for the
        !  destination object.  The procedure should also provide a type-guard statement to check the
        !  compatibility of the "*concrete*" types of the source and destination objects.  If their
        !  concrete types are not compatible, the procedure should report an error.
        SUBROUTINE IfaceCopy(SrcObj, DstObj, IsDeep)
            IMPORT
            CLASS(Object),      INTENT(IN)  :: SrcObj   !! Source object
            CLASS(Object),      INTENT(OUT) :: DstObj   !! Destination object
            tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
            !^ Flag indicating whether to perform deep copy or shallow copy. <br>
            !  - If present and true, perform a deep copy. <br>
            !  - If present and false, perform a shallow copy. <br>
            !  - If not present, perform either a shallow or a deep copy that is naturally most suitable
            !    for the object's components.
        END SUBROUTINE
        !> *IfaceEqual* is an interface for a procedure to compare whether two objects are equal to one
        !  another or not.
        FUNCTION IfaceEqual(LhsObj, RhsObj) RESULT(Flag)
            IMPORT
            CLASS(Object), INTENT(IN)   :: LhsObj   !! an object
            CLASS(Object), INTENT(IN)   :: RhsObj   !! another object
            tLogical                    :: Flag     !! true if both objects are equal
        END FUNCTION
        !> *IfaceFree* is an interface for a procedure to free storage/memory of an object with pointer
        !  and/or allocatable components.  For an object without any pointer or allocatable components,
        !  the routine may perform a reset of the object's components or it may do nothing (i.e. it is
        !  a dummy routine).
        SUBROUTINE IfaceFree(Obj)
            IMPORT
            CLASS(Object), INTENT(INOUT)    :: Obj
        END SUBROUTINE
        !> *Iface2String* is an interface for a procedure to return a string representation of the
        !  object.  The string should represent both the name of its concrete type and value(s) of
        !  its component(s).
        FUNCTION Iface2String(Obj) RESULT(Str)
            IMPORT
            CLASS(Object), INTENT(IN)   :: Obj  !! an object
            tCharAlloc                  :: Str  !! string representation of the object
        END FUNCTION
    END INTERFACE
    INTERFACE ASSIGNMENT(=)
        !^ **Operator Overload**: ASSIGNMENT(=) <br>
        !  **Purpose**:  To make a copy of an object using an assignment statement. <br>
        !  **Usage**: <br>
        !   --->    Out = Inp
        MODULE PROCEDURE    :: Object_MakeCopy
        MODULE PROCEDURE    :: Object_MakeCopy1D
        MODULE PROCEDURE    :: Object_MakeCopy2D
        MODULE PROCEDURE    :: Object_MakeCopy3D
    END INTERFACE
    INTERFACE MakeCopy
        !^ **Subroutine Interface**: MakeCopy <br>
        !  **Purpose**:  To make a copy of an object. <br>
        !  **Usage**: <br>
        !   --->    CALL MakeCopy(Out, Inp)
        MODULE PROCEDURE    :: Object_MakeCopy
        MODULE PROCEDURE    :: Object_MakeCopy1D
        MODULE PROCEDURE    :: Object_MakeCopy2D
        MODULE PROCEDURE    :: Object_MakeCopy3D
    END INTERFACE
    INTERFACE MemFree
        !^ **Subroutine Interface**: MemFree <br>
        !  **Purpose**:  To free memory of an array. <br>
        !  **Usage**: <br>
        !   --->    CALL MemFree(Arr1D) <br>
        !   --->    CALL MemFree(Arr2D) <br>
        !   --->    CALL MemFree(Arr3D)
        MODULE PROCEDURE Allocatable_Object_MemFree_1D
        MODULE PROCEDURE Allocatable_Object_MemFree_2D
        MODULE PROCEDURE Allocatable_Object_MemFree_3D
        MODULE PROCEDURE Pointer_Object_MemFree_1D
        MODULE PROCEDURE Pointer_Object_MemFree_2D
        MODULE PROCEDURE Pointer_Object_MemFree_3D
    END INTERFACE
    INTERFACE MemAlloc
        !^ **Subroutine Interface**: MemAlloc <br>
        !  **Purpose**:  To allocate memory of an array. <br>
        !  **Note**:  For optional starting ID(s), the default value is 1. <br>
        !  **Usage**: <br>
        !   --->    CALL MemAlloc(Arr1D, 10, ArrMold) <br>
        !   --->    CALL MemAlloc(Arr1D, 100, ArrMold, StartID=11) <br>
        !   --->    CALL MemAlloc(Arr2D, 20, 10, ArrMold) <br>
        !   --->    CALL MemAlloc(Arr2D, 100, 50, ArrMold, StartID1=21, StartID2=-10) <br>
        !   --->    CALL MemAlloc(Arr3D, 50, 50, 50, ArrMold) <br>
        !   --->    CALL MemAlloc(Arr3D, 20, 30, 40, ArrMold, StartID2=-10) <br>
        !   --->    CALL MemAlloc(Arr3D, 10, 10, 10, ArrMold, 0, 0, 0)
        MODULE PROCEDURE Allocatable_Object_MemAlloc_1D
        MODULE PROCEDURE Allocatable_Object_MemAlloc_2D
        MODULE PROCEDURE Allocatable_Object_MemAlloc_3D
        MODULE PROCEDURE Pointer_Object_MemAlloc_1D
        MODULE PROCEDURE Pointer_Object_MemAlloc_2D
        MODULE PROCEDURE Pointer_Object_MemAlloc_3D
    END INTERFACE
    INTERFACE MemResize
        !^ **Subroutine Interface**: MemResize <br>
        !  **Purpose**:  To reallocate memory for an array and preserve its data. <br>
        !  **Usage**: <br>
        !   --->    CALL MemResize(Arr1D, 100) <br>
        !   --->    CALL MemResize(Arr2D, 20, 30) <br>
        !   --->    CALL MemResize(Arr3D, 50, 20, 100)
        MODULE PROCEDURE Allocatable_Object_MemResize_1D
        MODULE PROCEDURE Allocatable_Object_MemResize_2D
        MODULE PROCEDURE Allocatable_Object_MemResize_3D
        MODULE PROCEDURE Pointer_Object_MemResize_1D
        MODULE PROCEDURE Pointer_Object_MemResize_2D
        MODULE PROCEDURE Pointer_Object_MemResize_3D
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Object_MakeCopy(OutObj, InObj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform a copy of an object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Object), INTENT(OUT)  :: OutObj
    CLASS(Object), INTENT(IN)   :: InObj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL InObj%Copy(OutObj)

    RETURN

END SUBROUTINE Object_MakeCopy

!******************************************************************************

SUBROUTINE Object_MakeCopy1D(OutObj, InObj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform a copy of an array of objects.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Object), INTENT(OUT)  :: OutObj(:)
    CLASS(Object), INTENT(IN)   :: InObj(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32 :: I, MinSize

! FLOW
    
    ! first, check input data
    IF (.NOT.SAME_TYPE_AS(OutObj, InObj)) THEN
        ! report error
        CALL Handle_ErrLevel('Object_MakeCopy1D', ModName, ErrSevere, &
                             'Types of input and output arrays are NOT compatible.')
        RETURN
    END IF
        
    MinSize = MIN(SIZE(InObj, KIND=kIndex), SIZE(OutObj, KIND=kIndex))
    DO I = 1_kIndex, MinSize
        CALL MakeCopy(OutObj(I), InObj(I))
    END DO

    RETURN

END SUBROUTINE Object_MakeCopy1D

!******************************************************************************

SUBROUTINE Object_MakeCopy2D(OutObj, InObj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform a copy of an array of objects.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Object), INTENT(OUT)  :: OutObj(:,:)
    CLASS(Object), INTENT(IN)   :: InObj(:,:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32 :: I, MinSize

! FLOW
    
    ! first, check input data
    IF (.NOT.SAME_TYPE_AS(OutObj, InObj)) THEN
        ! report error
        CALL Handle_ErrLevel('Object_MakeCopy2D', ModName, ErrSevere, &
                             'Types of input and output arrays are NOT compatible.')
        RETURN
    END IF
        
    MinSize = MIN(SIZE(InObj, DIM=2, KIND=kIndex), SIZE(OutObj, DIM=2, KIND=kIndex))
    DO I = 1_kIndex, MinSize
        CALL MakeCopy(OutObj(:,I), InObj(:,I))
    END DO

    RETURN

END SUBROUTINE Object_MakeCopy2D

!******************************************************************************

SUBROUTINE Object_MakeCopy3D(OutObj, InObj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform a copy of an array of objects.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Object), INTENT(OUT)  :: OutObj(:,:,:)
    CLASS(Object), INTENT(IN)   :: InObj(:,:,:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32 :: I, MinSize

! FLOW
    
    ! first, check input data
    IF (.NOT.SAME_TYPE_AS(OutObj, InObj)) THEN
        ! report error
        CALL Handle_ErrLevel('Object_MakeCopy3D', ModName, ErrSevere, &
                             'Types of input and output arrays are NOT compatible.')
        RETURN
    END IF
        
    MinSize = MIN(SIZE(InObj, DIM=3, KIND=kIndex), SIZE(OutObj, DIM=3, KIND=kIndex))
    DO I = 1_kIndex, MinSize
        CALL MakeCopy(OutObj(:,:,I), InObj(:,:,I))
    END DO

    RETURN

END SUBROUTINE Object_MakeCopy3D

!******************************************************************************

FUNCTION Object_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS ROUTINE:
    !^ To provide default implementation of the *HashCode* method.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Object), INTENT(IN)   :: Obj
    tHash                       :: Code

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: Bits_kByte = BIT_SIZE(0_kInt8)       ! should be  8 bits
    tSInt32, PARAMETER  :: Bits_Char  = CHARACTER_STORAGE_SIZE
    tSInt32, PARAMETER  :: Bytes_Char = Bits_Char/Bits_kByte

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: KeySize
    tCharAlloc  :: Key

!** FLOW:

    Key = Obj%ToString()
    KeySize = ToIndex(Bytes_Char*LEN(Key))
    Code = ComputeHash(Key, KeySize, HashSeed)

    RETURN

END FUNCTION Object_HashCode

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           FREE-UP MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ----------------- ALLOCATABLE ARRAYS -------------------
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
! Object
#define TypeName            CLASS(Object)
#define Generic_MemFree_1D  Allocatable_Object_MemFree_1D
#define Generic_MemFree_2D  Allocatable_Object_MemFree_2D
#define Generic_MemFree_3D  Allocatable_Object_MemFree_3D
#include    "Includes/Object MemFree - Implementation.f90"
#undef TypeName
#undef Generic_MemFree_1D
#undef Generic_MemFree_2D
#undef Generic_MemFree_3D
! undefine for allocatable arrays
#undef AttributeName
#undef ArrayStatusTest(A)
!
! ----------------- POINTER ARRAYS -------------------
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
! Object
#define TypeName            CLASS(Object)
#define Generic_MemFree_1D  Pointer_Object_MemFree_1D
#define Generic_MemFree_2D  Pointer_Object_MemFree_2D
#define Generic_MemFree_3D  Pointer_Object_MemFree_3D
#include    "Includes/Object MemFree - Implementation.f90"
#undef TypeName
#undef Generic_MemFree_1D
#undef Generic_MemFree_2D
#undef Generic_MemFree_3D
! undefine for pointer arrays
#undef AttributeName
#undef ArrayStatusTest

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           ALLOCATE MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ----------------- ALLOCATABLE ARRAYS -------------------
#define AttributeName       ALLOCATABLE
! Object
#define TypeName            CLASS(Object)
#define Generic_MemAlloc_1D Allocatable_Object_MemAlloc_1D
#define Generic_MemAlloc_2D Allocatable_Object_MemAlloc_2D
#define Generic_MemAlloc_3D Allocatable_Object_MemAlloc_3D
#include    "Includes/Object MemAlloc - Implementation.f90"
#undef TypeName
#undef Generic_MemAlloc_1D
#undef Generic_MemAlloc_2D
#undef Generic_MemAlloc_3D
! undefine for allocatable arrays
#undef AttributeName
!
! ----------------- POINTER ARRAYS -------------------
#define AttributeName       POINTER
! Object
#define TypeName            CLASS(Object)
#define Generic_MemAlloc_1D Pointer_Object_MemAlloc_1D
#define Generic_MemAlloc_2D Pointer_Object_MemAlloc_2D
#define Generic_MemAlloc_3D Pointer_Object_MemAlloc_3D
#include    "Includes/Object MemAlloc - Implementation.f90"
#undef TypeName
#undef Generic_MemAlloc_1D
#undef Generic_MemAlloc_2D
#undef Generic_MemAlloc_3D
! undefine for pointer arrays
#undef AttributeName

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                       RE-ALLOCATE MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ----------------- ALLOCATABLE ARRAYS -------------------
#define AttributeName           ALLOCATABLE
#define ArrayStatusTest(A)      ALLOCATED(A)
#define MEM_MOVE(A,B)           MOVE_ALLOCATABLE_ARRAY(A, B) 
! Object
#define TypeName                CLASS(Object)
#define Generic_MemResize_1D    Allocatable_Object_MemResize_1D
#define Generic_MemResize_2D    Allocatable_Object_MemResize_2D
#define Generic_MemResize_3D    Allocatable_Object_MemResize_3D
#include    "Includes/Object MemResize - Implementation.f90"
#undef TypeName
#undef Generic_MemResize_1D
#undef Generic_MemResize_2D
#undef Generic_MemResize_3D
! undefine for allocatable arrays
#undef AttributeName
#undef ArrayStatusTest
#undef MEM_MOVE
!
! ----------------- POINTER ARRAYS -------------------
#define AttributeName           POINTER
#define ArrayStatusTest(A)      ASSOCIATED(A)
#define MEM_MOVE(A,B)           MOVE_POINTER_ARRAY(A, B) 
! Object
#define TypeName                CLASS(Object)
#define Generic_MemResize_1D    Pointer_Object_MemResize_1D
#define Generic_MemResize_2D    Pointer_Object_MemResize_2D
#define Generic_MemResize_3D    Pointer_Object_MemResize_3D
#include    "Includes/Object MemResize - Implementation.f90"
#undef TypeName
#undef Generic_MemResize_1D
#undef Generic_MemResize_2D
#undef Generic_MemResize_3D
! undefine for pointer arrays
#undef AttributeName
#undef ArrayStatusTest
#undef MEM_MOVE

!**************************************************************************************

END MODULE MClass_Object

!******************************************************************************
