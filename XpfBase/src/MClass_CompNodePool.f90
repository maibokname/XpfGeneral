
MODULE MClass_CompNodePool

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *CompNodePool* type, the *CompNode* type and its related routines.
!   The *CompNodePool* type is a memory-pool type that can be used to manage memory allocation of
!   comparable nodes stored in a linked-based container while the *CompNode* type is an abstract
!   type provided as a base type for a comparable linked node.  Any linked-based container type
!   intended to utilize the *CompNodePool* type must implement its linked node type as an extended
!   type of the *CompNode* type.

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
    USE MClass_Comparable,  ONLY: Comparable
    USE MClass_CharBuffer

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: CompNodePool
    PUBLIC :: CompNode

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_CompNodePool'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! size of the initial pool
    tIndex,    PARAMETER    :: DfltInitSize = 128_kIndex

!** DERIVED TYPE DEFINITIONS
#define     ThisNode    CompNode
#define     BaseType    Comparable
#define     basic       comparable
#define     NodePool    CompNodePool
#define     CopyPIndx   CopyCompNode
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

END MODULE MClass_CompNodePool

!******************************************************************************
