
MODULE MClass_BaseSymTable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *BaseSymTable* type and its related routines.  The *BaseSymTable*
!   type is an abstract data type representing a symbol table, which is a collection that
!   associates a *value* with a *key*.  The user can insert key-value pairs into the symbol
!   table with the expectation of later being able to search for the value associated with
!   a given key. <br>
!   The *BaseSymTable* type extends the *BaseCollection* type to define additional methods for
!   various common operations of a symbol table.  All other symbol table types (unordered or
!   ordered ones) should extend from this base type. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MClass_BaseCollection
    USE MClass_GenData,         ONLY: IsSameDataType,   IfacePolyCopy
    USE MClass_KeyOrdered,      ONLY: IsSameKeyOrdered, IsKeyOrdered
    USE MClass_KeyUnordered,    ONLY: IsKeyUnordered

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: BaseSymTable

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_BaseSymTable'

!** DERIVED TYPE 
    !> The *BaseSymTable* type is an abstract collection type that defines an API for
    !  a symbol table, which is a collection that associates a *value* with a *key*. <br>
    !  It is important to note that, by design, a concrete collection type that extends
    !  from the *BaseSymTable* type only stores entries of one specific key type and one
    !  specific value type.  To store entries of another key and/or value types, the
    !  collection must be cleared and/or destructed. <br>
    TYPE, ABSTRACT, EXTENDS(BaseCollection) :: BaseSymTable
        PRIVATE
        CLASS(*), ALLOCATABLE   :: KeyMold
        CLASS(*), ALLOCATABLE   :: ValMold
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *StartFirst* is a deferred procedure to start the *forward* iteration. <br>
        PROCEDURE(IfaceStartFirst), DEFERRED    :: StartFirst
        !> *MoveForward* is a deferred procedure to move to the next iteration. <br>
        PROCEDURE(IfaceMoveNext),   DEFERRED    :: MoveForward
        !> *Insert* is a deferred procedure to insert the specified key-value pair to the
        !   collection. <br>
        PROCEDURE(IfaceInsert),     DEFERRED    :: Insert
        !> *Delete* is a deferred procedure to delete the current key-value pair from the
        !   collection.  This method is intended to be employed in conjunction with the
        !   *StartFirst* and *MoveForward* methods.  Therefore, after the call to one of
        !   those methods and then calling this one will result in a removal of the current
        !   key-value pair of the iteration (i.e. the same key-value pair that can be obtained
        !   via the *StartFirst* and *MoveForward* methods). <br>
        PROCEDURE(IfaceDelete),     DEFERRED    :: Delete
        !> *Remove* is a deferred procedure to remove the specified key (and its associated
        !   value) from the collection. <br>
        PROCEDURE(IfaceRemove),     DEFERRED    :: Remove
        !> *Contain* is a deferred procedure to check whether the specified key is in the
        !   collection or not. <br>
        PROCEDURE(IfaceContain),    DEFERRED    :: Contain
        !> *GetValue* is a deferred procedure to get a value associated with the specified key
        !   in the collection. <br>
        PROCEDURE(IfaceGetVal),     DEFERRED    :: GetValue
        !> *ToArray* is a deferred procedure to retrieve and remove all key-value pairs from
        !   the collection. <br>
        PROCEDURE(IfaceToArray),    DEFERRED    :: ToArray
        !> *GetAll* is a deferred procedure to retrieve all keys and/or all values (without
        !   removing them) from the collection. <br>
        PROCEDURE(IfaceGetAll),     DEFERRED    :: GetAll
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all key-value pairs from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        !  **Note**: The *Clear* method provided by the *BaseSymTable* type is
        !       a simple implementation.   It can be overridden by a better and
        !       more efficient implementation.
        PROCEDURE   :: Clear        => BaseSymTable_ClearEntries
        !> **Type-Bound Function**: GetKeyPtr <br>
        !  **Purpose**:  To get a pointer to a key stored in a symbol table.  The pointer is
        !       intended to be used as a mold for the key (i.e. to provide type of the stored
        !       keys).  Return null pointer if the table is empty. <br>
        !  **Usage**: <br>
        !   --->    KeyPtr => Collection%GetKeyPtr()
        PROCEDURE   :: GetKeyPtr    => BaseSymTable_GetKeyPtr
        !> **Type-Bound Function**: GetValPtr <br>
        !  **Purpose**:  To get a pointer to a value stored in a symbol table.  The pointer is
        !       intended to be used as a mold for the value (i.e. to provide type of the stored
        !       values).  Return null pointer if the table is empty. <br>
        !  **Usage**: <br>
        !   --->    ValPtr => Collection%GetValPtr()
        PROCEDURE   :: GetValPtr    => BaseSymTable_GetValPtr
        ! ---------------------------------------------------------------------
        ! -----       Public Procedures To Be Used Internally             -----
        ! ---------------------------------------------------------------------
        !> *SetMolds* is a procedure to set molds of stored keys and values.  This procedure is
        !   intended to be used only by a subtype of the *BaseSymTable* type, not by a user.
        PROCEDURE   :: SetMolds     => BaseSymTable_SetMolds
        !> *SetMolds* is a procedure to free molds of stored keys and values.  This procedure is
        !   intended to be used only by a subtype of the *BaseSymTable* type, not by a user.
        PROCEDURE   :: FreeMolds    => BaseSymTable_FreeMolds
        !> *IsKeyOrdered* is a procedure to check whether the specified key is valid or not.  This
        !   procedure is intended to be used only by a subtype of the *BaseSymTable* type, not by
        !   a user.
        PROCEDURE   :: IsKeyValid   => BaseSymTable_IsKeyValid
        !> *IsValOrdered* is a procedure to check whether the specified value is valid or not.  This
        !   procedure is intended to be used only by a subtype of the *BaseSymTable* type, not by
        !   a user.
        PROCEDURE   :: IsValValid   => BaseSymTable_IsValValid
        ! ---------------------------------------------------------------------
    END TYPE BaseSymTable

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> IfaceStartFirst is an interface for a procedure to move to the first (starting)
        !  pair data in a symbol table.  It is provided for an iteration over all key-value
        !  pairs in a symbol table. <br>
        !  For an ordered symbol table, the first pair data normally means the key-value pair
        !  with the smallest key.  For an unordered symbol table, the first pair data can be
        !  any key-value pair. <br>
        FUNCTION IfaceStartFirst(Collection, Key, Value, KeyCopy, ValCopy) RESULT(IsEmpty)
            IMPORT
            CLASS(BaseSymTable), INTENT(INOUT)  :: Collection   !! symbol-table object
            !> the first key as output if requested (and available)
            CLASS(*), OPTIONAL,  INTENT(INOUT)  :: Key
            !> the first value as output if requested (and available)
            CLASS(*), OPTIONAL,  INTENT(INOUT)  :: Value
            !> a procedure to copy stored keys for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: KeyCopy
            !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
            !> a flag indicating whether the collection is empty or not <br>
            tLogical                            :: IsEmpty
        END FUNCTION
        !> IfaceMoveNext is an interface for a procedure to move to the next pair data in a symbol
        !  table.  It is provided for an iteration over all key-value pairs in a symbol table. <br>
        !  For an ordered symbol table, the next pair data normally indicates the key-value pair
        !  with the so-called successor key.  For an unordered symbol table, the next pair data may
        !  be any key-value pair. <br>
        FUNCTION IfaceMoveNext(Collection, Key, Value, KeyCopy, ValCopy) RESULT(IsTheEnd)
            IMPORT
            CLASS(BaseSymTable), INTENT(INOUT)  :: Collection   !! symbol-table object
            !> the next key as output if requested (and available)
            CLASS(*), OPTIONAL,  INTENT(INOUT)  :: Key
            !> the next value as output if requested (and available)
            CLASS(*), OPTIONAL,  INTENT(INOUT)  :: Value
            !> a procedure to copy stored keys for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: KeyCopy
            !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
            !> a flag indicating whether the end of the collection is encountered or not <br>
            tLogical                            :: IsTheEnd
        END FUNCTION
        !> *IfaceInsert* is an interface for a procedure to add a key-value pair into a symbol
        !  table.  When implementing this procedure, a symbol table should only allow one value
        !  being associated with each key (i.e. no duplicate keys in the table).  This means that
        !  when a user puts a key-value pair into a table already containing that key (and its
        !  associated value), the new value should then replace the old one.  These conventions
        !  define the associative array abstraction, where we can think of a symbol table as being
        !  just like an array, where keys are indices and values are array entries. <br>
        SUBROUTINE IfaceInsert(Collection, Key, Value)
            IMPORT
            CLASS(BaseSymTable), INTENT(INOUT)  :: Collection   !! symbol-table object
            !> the key to be added to the collection
            CLASS(*),            INTENT(IN)     :: Key
            !> the value to be added to the collection
            CLASS(*),            INTENT(IN)     :: Value
        END SUBROUTINE
        !> *IfaceDelete* is an interface for a procedure to delete the current key-value pair
        !  from a symbol table.  This procedure is intended to be used in conjunction with the
        !  *StartFirst* and *MoveForward* methods.  Therefore, after the call to either method
        !  and then calling this procedure will result in a removal of the current key-value
        !  pair of the iteration (i.e. the same key-value pair that can be retrieved via those
        !  iteration methods). <br>
        SUBROUTINE IfaceDelete(Collection)
            IMPORT
            CLASS(BaseSymTable), INTENT(INOUT)  :: Collection   !! symbol-table object
        END SUBROUTINE
        !> *IfaceRemove* is an interface for a procedure to delete the specified key (and its
        !  associated value) from a symbol table.  Also, return a flag indicating whether the
        !  key-value pair is successfully removed or not. <br>
        FUNCTION IfaceRemove(Collection, Key) RESULT(Success)
            IMPORT
            CLASS(BaseSymTable), INTENT(INOUT)  :: Collection   !! symbol-table object
            !> the key to be removed from the collection
            CLASS(*),            INTENT(IN)     :: Key
            !> flag indicating whether the specified key and its associated
            !  value are successfully removed or not.
            tLogical                            :: Success
        END FUNCTION IfaceRemove
        !> *IfaceContain* is an interface for a procedure to check whether the specified key is in
        !  a symbol table or not.  Return true if the key is found.  Otherwise, return false. <br>
        FUNCTION IfaceContain(Collection, Key) RESULT(Found)
            IMPORT
            CLASS(BaseSymTable), INTENT(INOUT)  :: Collection   !! symbol-table object
            !> the key to be looked for in the collection
            CLASS(*),            INTENT(IN)     :: Key
            !> flag indicating whether the specified key is found or not.
            tLogical                            :: Found
        END FUNCTION IfaceContain
        !> *IfaceGetVal* is an interface for a procedure to obtain a value associated with the
        !  specified key in a symbol table.  Also, return a flag indicating whether the value
        !  is successfully retrieved or not. <br>
        FUNCTION IfaceGetVal(Collection, Key, Value, ValCopy) RESULT(Success)
            IMPORT
            CLASS(BaseSymTable), INTENT(INOUT)  :: Collection   !! symbol-table object
            !> the key to be looked for in the collection
            CLASS(*),            INTENT(IN)     :: Key
            !> the value associated with the specified key
            CLASS(*),            INTENT(INOUT)  :: Value
            !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
            !> flag indicating whether the value is successfully retrieved or not.
            tLogical                            :: Success
        END FUNCTION IfaceGetVal
        !> *IfaceToArray* is an interface for a procedure to get and remove all key-value pairs
        !   from the collection.  Also, return a flag indicating whether the items are successfully
        !   removed or not.
        FUNCTION IfaceToArray(Collection, Keys, Values, KeyCopy, ValCopy) RESULT(Success)
            IMPORT
            CLASS(BaseSymTable), INTENT(INOUT)  :: Collection   !! iterable collection object
            !% the keys to be retrieved and removed from the collection
            CLASS(*),            INTENT(INOUT)  :: Keys(:)
            !% the values associated with the keys
            CLASS(*),            INTENT(INOUT)  :: Values(:)
            !> a procedure to copy stored keys for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: KeyCopy
            !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
            !> flag indicating whether the items are successfully retrieved and removed. <br>
            tLogical                            :: Success
        END FUNCTION
        !> *IfaceGetAll* is an interface for a procedure to get all keys and/or all values from
        !   the collection.  Also, return a flag indicating whether the keys (and/or values) are
        !   successfully retrieved or not.
        FUNCTION IfaceGetAll(Collection, Keys, Values, KeyCopy, ValCopy) RESULT(Success)
            IMPORT
	        !% iterable collection object
            CLASS(BaseSymTable), INTENT(INOUT)  :: Collection
            !% the keys to be retrieved and removed from the collection
            CLASS(*), OPTIONAL,  INTENT(INOUT)  :: Keys(1:)
            !% the values associated with the keys
            CLASS(*), OPTIONAL,  INTENT(INOUT)  :: Values(1:)
            !> a procedure to copy stored keys for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: KeyCopy
            !> a procedure to copy stored values for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ValCopy
            !> flag indicating whether the items are successfully retrieved. <br>
            tLogical                            :: Success
        END FUNCTION
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    !  na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE BaseSymTable_ClearEntries(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the pair data items from the collection. <br>
    !  This routine provides a basic implementation of the *Clear*
    !  deferred procedure required by the *BaseCollection* class.
    !  This routine should be overridden if a better implementation
    !  is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseSymTable), INTENT(INOUT)  :: Collection   !! symbol table
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: IsTheEnd

!** FLOW:
    
    IsTheEnd = Collection%StartFirst()
    DO WHILE (.NOT.IsTheEnd)
        CALL Collection%Delete()
        IsTheEnd = Collection%MoveForward()
    END DO

    RETURN

END SUBROUTINE BaseSymTable_ClearEntries

!******************************************************************************

FUNCTION BaseSymTable_GetKeyPtr(Collection) RESULT(Key)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a pointer to a key stored in a symbol table.  The pointer is intended to be
    !  used as a mold for the key (i.e. provides type of the stored keys).  Return null
    !  pointer if the table is empty.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseSymTable), TARGET, INTENT(IN) :: Collection   !! symbol table
    CLASS(*),            POINTER            :: Key          !! pointer to a stored key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(Collection%KeyMold)) THEN
        Key => Collection%KeyMold
    ELSE
        Key => NULL()
    END IF


    RETURN

END FUNCTION BaseSymTable_GetKeyPtr

!**************************************************************************************

FUNCTION BaseSymTable_GetValPtr(Collection) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a pointer to a value stored in a symbol table.  The pointer is intended to be
    !  used as a mold for the value (i.e. provides type of the stored values).  Return null
    !  pointer if the table is empty.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseSymTable), TARGET, INTENT(IN) :: Collection   !! symbol table
    CLASS(*),            POINTER            :: Val          !! pointer to a stored value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(Collection%ValMold)) THEN
        Val => Collection%ValMold
    ELSE
        Val => NULL()
    END IF


    RETURN

END FUNCTION BaseSymTable_GetValPtr

!**************************************************************************************

SUBROUTINE BaseSymTable_SetMolds(Collection, KeyMold, ValMold)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set the "KeyMold" and "ValMold" components. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseSymTable), INTENT(INOUT)  :: Collection   !! symbol table
    CLASS(*), OPTIONAL,  INTENT(IN)     :: KeyMold      !! mold for stored keys
    CLASS(*), OPTIONAL,  INTENT(IN)     :: ValMold      !! mold for stored values
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (PRESENT(KeyMold)) THEN
        IF (.NOT.ALLOCATED(Collection%KeyMold)) ALLOCATE(Collection%KeyMold, MOLD=KeyMold)
    END IF
    IF (PRESENT(ValMold)) THEN
        IF (.NOT.ALLOCATED(Collection%ValMold)) ALLOCATE(Collection%ValMold, MOLD=ValMold)
    END IF

    RETURN

ENDSUBROUTINE BaseSymTable_SetMolds

!**************************************************************************************

SUBROUTINE BaseSymTable_FreeMolds(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To free the "KeyMold" and "ValMold" components. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseSymTable), INTENT(INOUT)  :: Collection   !! symbol table
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (ALLOCATED(Collection%KeyMold)) DEALLOCATE(Collection%KeyMold)
    IF (ALLOCATED(Collection%ValMold)) DEALLOCATE(Collection%ValMold)

    RETURN

END SUBROUTINE BaseSymTable_FreeMolds

!**************************************************************************************

FUNCTION BaseSymTable_IsKeyValid(Collection, Key, IsOrderedKey) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To check whether the type of specified key is valid or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseSymTable), INTENT(INOUT)  :: Collection   !! symbol table
    CLASS(*),            INTENT(IN)     :: Key          !! the key to be checked
    tLogical,            INTENT(IN)     :: IsOrderedKey
    !^ true if the specified key must be an ordered key; false if the specified key is an unordered key.
    tLogical                            :: Valid        !! true if type of the specified key is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether the mold and the specified key have the same type or not
    IF (ALLOCATED(Collection%KeyMold)) THEN
        IF (IsKeyOrdered(Collection%KeyMold)) THEN
            ! ordered keys are stored
            Valid = IsSameKeyOrdered(Collection%KeyMold, Key)
        ELSE
            ! unordered keys are stored
            Valid = IsSameDataType(Collection%KeyMold, Key)
        END IF
    ELSE
        ! this is the first key inserted so set it as the mold
        IF (IsOrderedKey) THEN
            ! ordered keys to be stored
            Valid = IsKeyOrdered(Key)
        ELSE
            ! unordered keys to be stored
            Valid = IsKeyUnordered(Key)
        END IF
        IF (Valid) CALL Collection%SetMolds(KeyMold=Key)
    END IF

    RETURN

END FUNCTION BaseSymTable_IsKeyValid

!**************************************************************************************

FUNCTION BaseSymTable_IsValValid(Collection, Val) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To check whether the type of specified value is valid or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseSymTable), INTENT(INOUT)  :: Collection   !! symbol table
    CLASS(*),            INTENT(IN)     :: Val          !! value to be checked
    tLogical                            :: Valid        !! true if type of the specified value is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether the mold and the specified value have the same type or not
    IF (ALLOCATED(Collection%ValMold)) THEN
        Valid = IsSameDataType(Collection%ValMold, Val)
    ELSE
        ! this is the first value so set it as the mold
        CALL Collection%SetMolds(ValMold=Val)
        Valid = TrueVal
    END IF

    RETURN

END FUNCTION BaseSymTable_IsValValid

!**************************************************************************************

END MODULE MClass_BaseSymTable

!******************************************************************************
