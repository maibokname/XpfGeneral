
MODULE MClass_Comparable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Comparable* type and its related routines.  The *Comparable* type is
!   an abstract data type (ADT) representing an object that can be compared to itself and/or other
!   object in the *Comparable* class.  The comparison provides a total ordering on the *Comparable*
!   objects.  This ordering is commonly referred to as the class's natural ordering, and the class's
!   *CompareTo* method is referred to as its natural comparison method. <br>
!   The *Comparable* type defines an application programming interface (API) for typical relational
!   comparisons.  A *concrete* derived type that extends the *Comparable* type must implement the
!   *CompareTo* deferred procedure.  Because the *Comparable* type is a subtype of the
!   <a href="../module/mclass_object.html#type-object">Object</a> type, all extended types must also
!   implement those deferred procedures required by a subtype of the *Object* type.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MClass_Object,      ONLY: Object, ASSIGNMENT(=)

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Comparable
    PUBLIC :: ASSIGNMENT(=) ! inherited from 'MClass_Object' module
    PUBLIC :: MemFree
    PUBLIC :: MemAlloc
    PUBLIC :: MemResize

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#include    "Includes/Macro - MemHandling.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'MClass_Comparable'
    tSInt32,   PARAMETER    :: MsgLen = 128

!** DERIVED TYPE 
    !>  A *Comparable* type is an abstract data type that can be compared to itself.  When two
    !   *Comparable* objects are compared, we can specify whether they are less than, greater
    !   than or equal to one another or not.  This data type can also be called a *Sortable*
    !   type because it can be used with a sorting algorithm.
    TYPE, ABSTRACT, EXTENDS(Object) :: Comparable
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *CompareTo* is a deferred procedure to provide a natural comparison of two
        !  *Comparable* objects.
        PROCEDURE(IfaceCompare), DEFERRED   :: CompareTo
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: Comparable_EqualTo
        PROCEDURE, PRIVATE  :: Comparable_NotEqualTo
        PROCEDURE, PRIVATE  :: Comparable_LessThan
        PROCEDURE, PRIVATE  :: Comparable_GreaterThan
        PROCEDURE, PRIVATE  :: Comparable_LessEqual
        PROCEDURE, PRIVATE  :: Comparable_GreaterEqual
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Operator Overload**: OPERATOR(==) <br>
        !  **Purpose**:  To check if the LHS value is equal to the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS == RHS <br>
        !   --->    IF (LHS == RHS) DoSomething
        GENERIC     :: OPERATOR(==) => Comparable_EqualTo
        !> **Operator Overload**: OPERATOR(/=) <br>
        !  **Purpose**:  To check if the LHS value is NOT equal to the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS /= RHS <br>
        !   --->    IF (LHS /= RHS) DoSomething
        GENERIC     :: OPERATOR(/=) => Comparable_NotEqualTo
        !> **Operator Overload**: OPERATOR(<) <br>
        !  **Purpose**:  To check if the LHS value is less than the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS < RHS <br>
        !   --->    IF (LHS < RHS) DoSomething
        GENERIC     :: OPERATOR(<)  => Comparable_LessThan
        !> **Operator Overload**: OPERATOR(>) <br>
        !  **Purpose**:  To check if the LHS value is greater than the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS > RHS <br>
        !   --->    IF (LHS > RHS) DoSomething
        GENERIC     :: OPERATOR(>)  => Comparable_GreaterThan
        !> **Operator Overload**: OPERATOR(<=) <br>
        !  **Purpose**:  To check if the LHS value is less than or equal to the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS <= RHS <br>
        !   --->    IF (LHS <= RHS) DoSomething
        GENERIC     :: OPERATOR(<=) => Comparable_LessEqual
        !> **Operator Overload**: OPERATOR(>=) <br>
        !  **Purpose**:  To check if the LHS value is greater than or equal to the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS >= RHS <br>
        !   --->    IF (LHS >= RHS) DoSomething
        GENERIC     :: OPERATOR(>=) => Comparable_GreaterEqual
        ! ---------------------------------------------------------------------
    END TYPE Comparable

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> *IfaceCompare* is an interface for a procedure to compare two *Comparable* objects where
        !  the output flag should be set to the following value: <br>
        !  - positive value (or 1) if A is greater than B, <br>
        !  - 0 if A is equal to B, <br>
        !  - negative value (or -1) if A is less than B. <br>
        !  It should be noted that the implementation of the *IfaceCompare* procedure (i.e. the
        !  *CompareTo* method) can sometimes be inconsistent with the *IsEqualTo* method such that
        !  the expressions (A%IsEqualTo(B)) and (A%CompareTo(B) == 0) may return different values.
        !  For example, an object with key and value as its components may implement the *IsEqualTo*
        !  method based on equalities of both key and value components whereas it may implement the
        !  *CompareTo* method based only on the equality of the key component.
        FUNCTION IfaceCompare(A,B) RESULT(Flag)
            IMPORT
            CLASS(Comparable), INTENT(IN)   :: A
            CLASS(Comparable), INTENT(IN)   :: B
            tSInt32                         :: Flag
        END FUNCTION
    END INTERFACE
    INTERFACE MemFree
        !^ **Subroutine Interface**: MemFree <br>
        !  **Purpose**:  To free memory of an array. <br>
        !  **Usage**: <br>
        !   --->    CALL MemFree(Arr1D) <br>
        !   --->    CALL MemFree(Arr2D) <br>
        !   --->    CALL MemFree(Arr3D)
        MODULE PROCEDURE Allocatable_Comparable_MemFree_1D
        MODULE PROCEDURE Allocatable_Comparable_MemFree_2D
        MODULE PROCEDURE Allocatable_Comparable_MemFree_3D
        MODULE PROCEDURE Pointer_Comparable_MemFree_1D
        MODULE PROCEDURE Pointer_Comparable_MemFree_2D
        MODULE PROCEDURE Pointer_Comparable_MemFree_3D
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
        MODULE PROCEDURE Allocatable_Comparable_MemAlloc_1D
        MODULE PROCEDURE Allocatable_Comparable_MemAlloc_2D
        MODULE PROCEDURE Allocatable_Comparable_MemAlloc_3D
        MODULE PROCEDURE Pointer_Comparable_MemAlloc_1D
        MODULE PROCEDURE Pointer_Comparable_MemAlloc_2D
        MODULE PROCEDURE Pointer_Comparable_MemAlloc_3D
    END INTERFACE
    INTERFACE MemResize
        !^ **Subroutine Interface**: MemResize <br>
        !  **Purpose**:  To reallocate memory for an array and preserve its data. <br>
        !  **Usage**: <br>
        !   --->    CALL MemResize(Arr1D, 100) <br>
        !   --->    CALL MemResize(Arr2D, 20, 30) <br>
        !   --->    CALL MemResize(Arr3D, 50, 20, 100)
        MODULE PROCEDURE Allocatable_Comparable_MemResize_1D
        MODULE PROCEDURE Allocatable_Comparable_MemResize_2D
        MODULE PROCEDURE Allocatable_Comparable_MemResize_3D
        MODULE PROCEDURE Pointer_Comparable_MemResize_1D
        MODULE PROCEDURE Pointer_Comparable_MemResize_2D
        MODULE PROCEDURE Pointer_Comparable_MemResize_3D
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION Comparable_EqualTo(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is equal to B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Comparable), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) == 0)

    RETURN

END FUNCTION Comparable_EqualTo

!******************************************************************************

FUNCTION Comparable_NotEqualTo(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is NOT equal to B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Comparable), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) /= 0)

    RETURN

END FUNCTION Comparable_NotEqualTo

!******************************************************************************

FUNCTION Comparable_LessThan(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is less than B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Comparable), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) < 0)

    RETURN

END FUNCTION Comparable_LessThan

!******************************************************************************

FUNCTION Comparable_GreaterThan(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is greater than B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Comparable), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) > 0)

    RETURN

END FUNCTION Comparable_GreaterThan

!******************************************************************************

FUNCTION Comparable_LessEqual(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is less than or equal to B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Comparable), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) <= 0)

    RETURN

END FUNCTION Comparable_LessEqual

!******************************************************************************

FUNCTION Comparable_GreaterEqual(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is greater than or equal to B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Comparable), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) >= 0)

    RETURN

END FUNCTION Comparable_GreaterEqual

!******************************************************************************

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           FREE-UP MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ----------------- ALLOCATABLE ARRAYS -------------------
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
! Comparable
#define TypeName            CLASS(Comparable)
#define Generic_MemFree_1D  Allocatable_Comparable_MemFree_1D
#define Generic_MemFree_2D  Allocatable_Comparable_MemFree_2D
#define Generic_MemFree_3D  Allocatable_Comparable_MemFree_3D
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
! Comparable
#define TypeName            CLASS(Comparable)
#define Generic_MemFree_1D  Pointer_Comparable_MemFree_1D
#define Generic_MemFree_2D  Pointer_Comparable_MemFree_2D
#define Generic_MemFree_3D  Pointer_Comparable_MemFree_3D
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
! Comparable
#define TypeName            CLASS(Comparable)
#define Generic_MemAlloc_1D Allocatable_Comparable_MemAlloc_1D
#define Generic_MemAlloc_2D Allocatable_Comparable_MemAlloc_2D
#define Generic_MemAlloc_3D Allocatable_Comparable_MemAlloc_3D
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
! Comparable
#define TypeName            CLASS(Comparable)
#define Generic_MemAlloc_1D Pointer_Comparable_MemAlloc_1D
#define Generic_MemAlloc_2D Pointer_Comparable_MemAlloc_2D
#define Generic_MemAlloc_3D Pointer_Comparable_MemAlloc_3D
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
! Comparable
#define TypeName                CLASS(Comparable)
#define Generic_MemResize_1D    Allocatable_Comparable_MemResize_1D
#define Generic_MemResize_2D    Allocatable_Comparable_MemResize_2D
#define Generic_MemResize_3D    Allocatable_Comparable_MemResize_3D
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
! Comparable
#define TypeName                CLASS(Comparable)
#define Generic_MemResize_1D    Pointer_Comparable_MemResize_1D
#define Generic_MemResize_2D    Pointer_Comparable_MemResize_2D
#define Generic_MemResize_3D    Pointer_Comparable_MemResize_3D
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

END MODULE MClass_Comparable

!******************************************************************************
