
MODULE MClass_DArrLogical

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *DArrLogical* type and related routines.
!   The *DArrLogical* type is a container with *LOGICAL* as the type
!   of its stored items.  It employs a dynamic-array implementation where
!   items are stored as a resizable array. <br>
!   See the <a href="../module/mclass_dynamicarrays.html">MClass_DynamicArrays</a>
!   module for discussions about the *Dynamic-Array* concept and its strategy
!   used for growing and shrinking the array, which is similar to the strategy
!   employed by the *DArrLogical* type. <br>
!   See the <a href="../module/mbase_dynamicarrays.html">MBase_DynamicArrays</a>
!   module for an overview and usage notes of a *dynamic-array-based* type.   A user
!   may use the *MBase_DynamicArrays* module instead of using this module directly.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil,       ONLY: MAX_I32, MAX_I64
    USE MBase_ErrHandlers
    USE MBase_MemHandlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: DArrLogical

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define DynArr          DArrLogical
#define TypeAlloc       LOGICAL, ALLOCATABLE
#define TypeArgmt       LOGICAL
#define TypeOfItem      LOGICAL

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_DArrLogical'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! maximum capacity of a dynamic-array-based container
#ifdef Indx64Bits
    tIndex,    PARAMETER    :: MaxCapacity = MAX_I64
#else
    tIndex,    PARAMETER    :: MaxCapacity = MAX_I32
#endif

!** DERIVED TYPE DEFINITIONS:
!** INCLUDE FILE FOR DECLARATION PART**
#include "Includes/Intrinsic DynArr - Declaraction.f90"

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!** INCLUDE FILE FOR IMPLEMENTATION PART **
#include "Includes/Intrinsic DynArr - Implementation.f90"

!** UNDEFINE MACROS **
#undef DynArr
#undef TypeAlloc
#undef TypeArgmt
#undef TypeOfItem

END MODULE MClass_DArrLogical

!******************************************************************************
