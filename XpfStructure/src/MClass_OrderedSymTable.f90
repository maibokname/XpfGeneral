
MODULE MClass_OrderedSymTable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *OrderedSymTable* type and its related routines.  The *OrderedSymTable*
!   type is an abstract type representing an ordered symbol table, which is a collection containing
!   key-value pairs that keeps the keys in order. <br>
!   The *OrderedSymTable* type is a subtype of the *BaseSymTable* type and thus inherits all methods
!   of the *BaseSymTable* type and all its super classes.  The *OrderedSymTable* type provides an
!   expanded API that defines numerous natural and useful operations involving relative key order. <br>
!   It is important to note that checking the key equality is usually sufficient for an unordered
!   symbol table.  However, for an ordered symbol table, the comparison between keys should provide
!   a total ordering on all the keys.  This means that although all Fortran intrinsic types (with
!   the exception of the *LOGICAL* type) can be used as a type of the key in an unordered symbol
!   table, only *CHARACTER*, *INTEGER* and *REAL* types can be used as a type of the key in an
!   ordered symbol table.  To use a derived type as a type of the key, any derived types are allowed
!   for an unordered symbol table.  However, for an ordered symbol table, only derived types that
!   are in the *Comparable* class are allowed. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseSymTable
    USE MClass_GenData,     ONLY: IfacePolyCopy

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: OrderedSymTable

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_OrderedSymTable'

!** DERIVED TYPE 
    !> The *OrderedSymTable* type is an abstract collection type that defines an API for an
    !  ordered symbol table.
    TYPE, ABSTRACT, EXTENDS(BaseSymTable) :: OrderedSymTable
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *GetMinKey* is a deferred procedure to get the smallest key (and optionally a value
        !   associated with it) in a symbol table. <br>
        PROCEDURE(IfaceMinKey),    DEFERRED :: GetMinKey
        !> *GetMaxKey* is a deferred procedure to get the largest key (and optionally a value
        !   associated with it) in a symbol table. <br>
        PROCEDURE(IfaceMaxKey),    DEFERRED :: GetMaxKey
        !> *Floor* is a deferred procedure to get the largest key (and optionally a value
        !   associated with it) in a symbol table less than or equal to the given key. <br>
        PROCEDURE(IfaceFloor),     DEFERRED :: Floor
        !> *Ceiling* is a deferred procedure to get the smallest key (and optionally a value
        !   associated with it) in a symbol table greater than or equal to the given key. <br>
        PROCEDURE(IfaceCeiling),   DEFERRED :: Ceiling
        !> *GetRank* is a deferred procedure to return the number of keys in the symbol table
        !   strictly less than the given key. <br>
        PROCEDURE(IfaceRank),      DEFERRED :: GetRank
        !> *Select* is a deferred procedure to get the key (and optionally its associated value) of
        !   the specified rank where the applicable range of rank is between 0 and TableSize-1. <br>
        PROCEDURE(IfaceSelect),    DEFERRED :: Select
        !> *RemoveMin* is a deferred procedure to remove the smallest key (and a value associated
        !   with it) from a symbol table. <br>
        PROCEDURE(IfaceDelMin),    DEFERRED :: RemoveMin
        !> *RemoveMax* is a deferred procedure to remove the largest key (and a value associated
        !   with it) from a symbol table. <br>
        PROCEDURE(IfaceDelMax),    DEFERRED :: RemoveMax
        !> *GetRangeSize* is a deferred procedure to return the number of keys between *KeyLo*
        !   (inclusive) and *KeyHi* (inclusive). <br>
        PROCEDURE(IfaceRangeSize), DEFERRED :: GetRangeSize
        ! ---------------------------------------------------------------------
    END TYPE OrderedSymTable

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> *IfaceMinKey* is an interface for a procedure to get the smallest key (and optionally
        !  a value associated with the key) in a symbol table.  Also, return a flag indicating
        !  whether the key is successfully retrieved or not.  If the table is empty, the flag is
        !  typically false.  Otherwise, the flag is always true. <br>
        FUNCTION IfaceMinKey(Collection, Key, Value, ValCopy) RESULT(Flag)
            IMPORT
            CLASS(OrderedSymTable), INTENT(INOUT)   :: Collection   !! symbol table
            !> the smallest key to be retrieved from the collection
            CLASS(*),               INTENT(INOUT)   :: Key
            !> the value associated with the smallest key
            CLASS(*), OPTIONAL,     INTENT(INOUT)   :: Value
            !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ValCopy
            !> flag indicating whether the key-value pair is found or not.
            tLogical                                :: Flag
        END FUNCTION IfaceMinKey
        !> *IfaceMaxKey* is an interface for a procedure to get the largest key (and optionally
        !  a value associated with the key) in a symbol table.  Also, return a flag indicating
        !  whether the key is successfully retrieved or not.  If the table is empty, the flag 
        ! is typically false.  Otherwise, the flag is always true.
        FUNCTION IfaceMaxKey(Collection, Key, Value, ValCopy) RESULT(Flag)
            IMPORT
            CLASS(OrderedSymTable), INTENT(INOUT)   :: Collection   !! symbol table
            !> the largest key to be retrieved from the collection
            CLASS(*),               INTENT(INOUT)   :: Key
            !> the value associated with the largest key
            CLASS(*), OPTIONAL,     INTENT(INOUT)   :: Value
            !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ValCopy
            !> flag indicating whether the key-value pair is found or not.
            tLogical                                :: Flag
        END FUNCTION IfaceMaxKey
        !> *GetFloor* is an interface for a procedure to get the largest key (and optionally
        !  a value associated with the key) in a symbol table less than or equal to the given
        !  key.  Also, return a flag indicating whether the floor key is successfully obtained
        !  or not.
        FUNCTION IfaceFloor(Collection, KeyIn, KeyOut, ValOut, ValCopy) RESULT(Flag)
            IMPORT
            CLASS(OrderedSymTable), INTENT(INOUT)   :: Collection   !! symbol table
            CLASS(*),               INTENT(IN)      :: KeyIn        !! the specified key
            !> the largest key in the table less than or equal to the given key
            CLASS(*),               INTENT(INOUT)   :: KeyOut
            !> the value associated with the largest key
            CLASS(*), OPTIONAL,     INTENT(INOUT)   :: ValOut
            !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ValCopy
            !> flag indicating whether the key-value pair is found or not.
            tLogical                                :: Flag
        END FUNCTION IfaceFloor
        !> *IfaceCeiling* is an interface for a procedure to get the smallest key (and optionally
        !  a value associated with the key) in a symbol table greater than or equal to the given
        !  key.  Also, return a flag indicating whether the ceiling key is successfully retrieved
        !  or not.
        FUNCTION IfaceCeiling(Collection, KeyIn, KeyOut, ValOut, ValCopy) RESULT(Flag)
            IMPORT
            CLASS(OrderedSymTable), INTENT(INOUT)   :: Collection   !! symbol table
            CLASS(*),               INTENT(IN)      :: KeyIn        !! the specified key
            !> the smallest key in the table less than or equal to the given key
            CLASS(*),               INTENT(INOUT)   :: KeyOut
            !> the value associated with the smallest key
            CLASS(*), OPTIONAL,     INTENT(INOUT)   :: ValOut
            !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ValCopy
            !> flag indicating whether the key-value pair is found or not.
            tLogical                                :: Flag
        END FUNCTION IfaceCeiling
        !> *IfaceRank* is an interface for a procedure to get the number of keys in the symbol
        !  table strictly less than the given key.
        FUNCTION IfaceRank(Collection, Key) RESULT(Rank)
            IMPORT
            CLASS(OrderedSymTable), INTENT(INOUT)   :: Collection   !! symbol table
            CLASS(*),               INTENT(IN)      :: Key          !! the specified key
            !> the number of keys less than the given key.
            tIndex                                  :: Rank
        END FUNCTION IfaceRank
        !> *KeySelect* is an interface for a procedure to get the key (and optionally its
        !  associated value) of the given rank.  Also, return a flag indicating whether the
        !  ranked key is successfully retrieved or not. <br>
        !  This ranked key has the property such that there are keys in the symbol table that
        !  are smaller.  In other words, this key is the (rank+1)st smallest key in the table. <br>
        !  The applicable range of rank is between 0 and TableSize-1 where the rank number is
        !  zero-based.  If the specified rank is out of range or the table is empty, set the
        !  returned flag to false.  Otherwise, set the returned flag to true. <br>
        FUNCTION IfaceSelect(Collection, Rank, Key, Value, ValCopy) RESULT(Flag)
            IMPORT
            CLASS(OrderedSymTable), INTENT(INOUT)   :: Collection   !! symbol table
            tIndex,                 INTENT(IN)      :: Rank         !! the specified rank
            !> the key of the specified rank
            CLASS(*),               INTENT(INOUT)   :: Key
            !> the value associated with the ranked key
            CLASS(*), OPTIONAL,     INTENT(INOUT)   :: Value
            !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ValCopy
            !> flag indicating whether the key-value pair is found or not.
            tLogical                                :: Flag
        END FUNCTION IfaceSelect
        !> *IfaceDelMin* is an interface for a procedure to remove the smallest key and a value
        !  associated with the key from a symbol table.  Also, return a flag indicating whether
        !  the key is successfully removed or not.  If the table is empty, the flag is typically
        !  false.  Otherwise, the flag is always true.
        FUNCTION IfaceDelMin(Collection, Key, Value, ValCopy) RESULT(Flag)
            IMPORT
            CLASS(OrderedSymTable), INTENT(INOUT)   :: Collection   !! symbol table
            CLASS(*), OPTIONAL,     INTENT(INOUT)   :: Key          !! the smallest key
            !> the value associated with the smallest key
            CLASS(*), OPTIONAL,     INTENT(INOUT)   :: Value
            !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ValCopy
            !> flag indicating whether the key is successfully removed or not
            tLogical                                :: Flag
        END FUNCTION IfaceDelMin
        !> *IfaceDelMax* is an interface for a procedure to remove the largest key and a value
        !  associated with the key from a symbol table.  Also, return a flag indicating whether
        !  the key is successfully removed or not.  If the table is empty, the flag is typically
        !  false.  Otherwise, the flag is always true.
        FUNCTION IfaceDelMax(Collection, Key, Value, ValCopy) RESULT(Flag)
            IMPORT
            CLASS(OrderedSymTable), INTENT(INOUT)   :: Collection   !! symbol table
            CLASS(*), OPTIONAL,     INTENT(INOUT)   :: Key          !! the largest key
            !> the value associated with the largest key
            CLASS(*), OPTIONAL,     INTENT(INOUT)   :: Value
            !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ValCopy
            !> flag indicating whether the key is successfully removed or not
            tLogical                                :: Flag
        END FUNCTION IfaceDelMax
        !> *IfaceRangeSize* is an interface for a procedure to get the number of keys in the given
        !   range (between KeyLo and KeyHi).
        FUNCTION IfaceRangeSize(Collection, KeyLo, KeyHi) RESULT(Size)
            IMPORT
            CLASS(OrderedSymTable), INTENT(INOUT)   :: Collection   !! symbol table
            CLASS(*),               INTENT(IN)      :: KeyLo        !! the minimum key (inclusive)
            CLASS(*),               INTENT(IN)      :: KeyHi        !! the maximum key (inclusive)
            !> the number of keys in the given range
            tIndex                                  :: Size
        END FUNCTION IfaceRangeSize
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    !  na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!**************************************************************************************

END MODULE MClass_OrderedSymTable

!******************************************************************************
