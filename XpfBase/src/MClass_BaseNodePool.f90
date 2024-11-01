
MODULE MClass_BaseNodePool

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *BaseNodePool* type, the *BaseNode* type and its related routines.
!   The *BaseNodePool* type is a memory-pool type that can be used to manage memory allocation
!   of nodes stored in a linked-based container while the *BaseNode* type is an abstract type
!   provided as a base type for a linked node.  Any linked-based container type intended to
!   utilize the *BaseNodePool* type must implement its linked node type as an extended type of
!   the *BaseNode* type.

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_MemHandlers
    USE MBase_ByteUtil,     ONLY: AnyType_GetByteSize
#ifdef Indx32Bits
    USE MBase_SimpleHash32, ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64, ONLY: ComputeHash => Hash64_FNV1a
#endif
    USE MClass_Object,      ONLY: Object, ASSIGNMENT(=)
    USE MClass_CharBuffer

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: BaseNodePool
    PUBLIC :: BaseNode

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_BaseNodePool'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! size of the initial pool
    tIndex,    PARAMETER    :: DfltInitSize = 128_kIndex

!** DERIVED TYPE DEFINITIONS
#define     ThisNode    BaseNode
#define     BaseType    Object
#define     basic       common
#define     NodePool    BaseNodePool
#define     CopyPIndx   CopyBaseNode
#include    "Includes/NodePool - Declaration.f90"

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

#include    "Includes/NodePool - Implementation.f90"
#undef      ThisNode
#undef      NodePool

!******************************************************************************

END MODULE MClass_BaseNodePool

!******************************************************************************
