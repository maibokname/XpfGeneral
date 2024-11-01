
MODULE MClass_TreeRealDP

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *TreeRealDP* type and its related helper type and routines.
!   The *TreeRealDP* type is a container type representing an ordered symbol table, which
!   is a container that associates a *value* with a *key* where keys are stored in a sorted
!   order.  It employs a balanced binary-search-tree (BST) implementation to provide common
!   operations for an ordered symbol table.  As an ordered symbol table, the *TreeRealDP*
!   type uses the Fortran intrinsic *REAL(KIND=kDouble)* type as the type of its stored keys
!   and an unlimited polymorphic type as the type of its stored values. <br>
!   As a symbol table, the *TreeRealDP* type does not allow duplicated keys.  Therefore,
!   if an inserted key is equal to a key stored in the table, an associated value of the
!   stored key is replaced by an associated value of the inserted key.  Technically, the
!   *TreeRealDP* type employs a left-leaning red-black (RB) tree as the balanced BST. <br>
!   See the <a href="../module/mbase_balancedtrees.html">MBase_BalancedTrees</a> module
!   for an overview of a *balanced-tree-based* type.  A user may use the *MBase_BalancedTrees*
!   module instead of using this module directly. <br>
!   See the <a href="../module/mclass_treetable.html">MClass_TreeTable</a> module for a balanced
!   tree container type that is functionally similar to the *TreeRealDP* type but utilizes
!   a different implementation.  Also, unlike the *TreeRealDP* type, the *TreeTable* type
!   is designed as a generic ordered symbol table that allows keys with various types to be
!   inserted into the table. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MemHandlers
    USE MBase_ErrHandlers
    USE MBase_SIntUtil,       ONLY: ToChar => ToDecStrSigned
    USE MBase_DoublyLinkedLists
    USE MClass_Object

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: TreeRealDP

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define KeyTypeA        REAL(KIND=kDouble)
#define KeyTypeB        REAL(KIND=kDouble)
#define KeyTypeC        REAL(KIND=kDouble)
#define QueueKey        ListRealDP
#define QueueVal        ListAnyType
#define RedBlackTree    TreeRealDP

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_TreeRealDP'

!** INCLUDE FILE FOR DECLARATION PART**
#include "Includes/Intrinsic Tree - Declaraction.f90"

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!** INCLUDE FILE FOR IMPLEMENTATION PART **
#include "Includes/Intrinsic Tree - Implementation.f90"

!** UNDEFINE MACROS **
#undef RedBlackTree
#undef KeyTypeA
#undef KeyTypeB
#undef KeyTypeC
#undef QueueKey
#undef QueueVal

END MODULE MClass_TreeRealDP

!******************************************************************************
