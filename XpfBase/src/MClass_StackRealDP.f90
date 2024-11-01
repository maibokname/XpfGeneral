
MODULE MClass_StackRealDP

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *StackRealDP* type and its related/supporting routines and derived type(s).
!   The *StackRealDP* type is a stack container with *REAL(KIND=kDouble)* as the type of its stored
!   items.  It employs a conventional singly-linked list implementation. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MemHandlers
    USE MBase_ErrHandlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: StackRealDP

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tContent    tRealDP
#define     StackList   StackRealDP

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_StackRealDP'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128

#include    "Includes/Intrinsic Stack - Declaraction.f90"

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

#include    "Includes/Intrinsic Stack - Implementation.f90"

!******************************************************************************

END MODULE MClass_StackRealDP

!******************************************************************************
