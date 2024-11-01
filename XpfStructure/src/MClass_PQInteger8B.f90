
MODULE MClass_PQInteger8B

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *PQInteger8B* type and its related routines.
!   The *PQInteger8B* type is a priority-queue container with *INTEGER(KIND=kInt64)*
!   as the type of its stored keys.  It employs a binary heap implementation
!   to order its stored keys. <br>
!   The *PQInteger8B* type can represent either the max-priority queue or the
!   min-priority queue.  By default, it represents the max-priority queue but
!   a user can specify the *MinPQ* argument to true so that it represents
!   the min-priority queue instead. <br>
!   See the <a href="../module/mbase_priorityqueues.html">MBase_PriorityQueues</a>
!   module for an overview of a *priority-queue-based* type. A user may use the
!   *MBase_PriorityQueues* module instead of using this module directly. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_MemHandlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: PQInteger8B

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     KeyType     INTEGER(KIND=kInt64)
#define     PQHeap      PQInteger8B

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'MClass_PQInteger8B'

!** INCLUDE FILE FOR DECLARATION PART**
#include "Includes/Intrinsic PQHeap - Declaraction.f90"

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!** INCLUDE FILE FOR IMPLEMENTATION PART **
#include "Includes/Intrinsic PQHeap - Implementation.f90"

END MODULE MClass_PQInteger8B

!******************************************************************************
