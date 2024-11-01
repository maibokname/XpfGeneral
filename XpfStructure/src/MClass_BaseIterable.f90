
MODULE MClass_BaseIterable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *BaseIterable* type and its related routines.  The *BaseIterable*
!   type is an abstract data type extending the *BaseCollection* type.  It defines two additional
!   methods for an iteration over items stored in a collection.  The *StartFirst* method is used
!   to start an iteration and the *MoveForward* method is used to move to the next iteration. <br>
!   <br>
!   **Usage**:  <br>
!   The following code snippet illustrates how to iterate over the collection.
!   <Pre><Code style="color:MidnightBlue;">
!   ! start forward iteration (from the first item)
!   IsEmpty = Collection%StartFirst()
!   IF (.NOT.IsEmpty) DoSomeThing...
!   DO
!       ! move to the next iteration
!       IsTheEnd = Collection%MoveForward()
!       ! check whether we reach the end of the collection or not
!       IF (IsTheEnd) EXIT
!       ! if not, do the task we need
!       DoSomeThing...
!   END DO
!   </Code></Pre>
!   The following code snippet shows another way to traverse across the collection.
!   <Pre><Code style="color:MidnightBlue;">
!   ! start forward iteration (from the first item)
!   IsTheEnd = Collection%StartFirst(CurrItem)
!   DO WHILE (.NOT.IsTheEnd)
!       DoSomeThing_With_CurrItem...
!       ! move to the next iteration
!       IsTheEnd = Collection%MoveForward(CurrItem)
!   END DO
!   </Code></Pre>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseCollection
    USE MClass_GenData,     ONLY: IsSameDataType, IfacePolyCopy

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: BaseIterable

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_BaseIterable'

!** DERIVED TYPE 
    !> The *BaseIterable* type is an abstract collection type that defines an API for
    !  iterable collections, which are collections that can iterate over their items. <br>
    !  It is important to note that, by design, a concrete collection type that extends
    !  from the *BaseIterable* type only stores items of one specific type.  To store
    !  items of another type, the collection must be cleared and/or destructed. <br>
    TYPE, ABSTRACT, EXTENDS(BaseCollection) :: BaseIterable
        PRIVATE
        CLASS(*), ALLOCATABLE   :: Mold
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *StartFirst* is a deferred procedure to start the *forward* iteration. <br>
        PROCEDURE(IfaceStartFirst), DEFERRED    :: StartFirst
        !> *MoveForward* is a deferred procedure to move to the next iteration. <br>
        PROCEDURE(IfaceMoveNext),   DEFERRED    :: MoveForward
        !> *Insert* is a deferred procedure to insert the specified item to the collection.  This
        !   is a default procedure for adding an item to the collection.  For a particular collection,
        !   an alias name (e.g. the *Push* method for a stack or the *Enqueue* method for a queue)
        !   may be used in place of the *Insert* method. <br>
        PROCEDURE(IfaceInsert),     DEFERRED    :: Insert
        !> *Delete* is a deferred procedure to delete the current item from the collection.  This
        !   method is intended to be used in conjunction with the *StartFirst* and *MoveForward*
        !   methods.  Therefore, after the call to one of those methods and then calling this one
        !   will result in a removal of the current item of the iteration (i.e. the same item that
        !   can be retrieved via the *StartFirst* and *MoveForward* methods). <br>
        PROCEDURE(IfaceDelete),     DEFERRED    :: Delete
        !> *ToArray* is a deferred procedure to retrieve and remove all items from the collection. <br>
        PROCEDURE(IfaceToArray),    DEFERRED    :: ToArray
        !> *GetAll* is a deferred procedure to retrieve all items (without removing them) from the
        !   collection. <br>
        PROCEDURE(IfaceGetAll),     DEFERRED    :: GetAll
        ! ---------------------------------------------------------------------
        ! -----      Deferred Procedures from BaseCollection Type         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear() <br>
        !  **Note**: The *Clear* method provided by the *BaseIterable* type is
        !       a simple implementation.   It can be overridden by a better and
        !       more efficient implementation.
        PROCEDURE   :: Clear        => BaseIterable_ClearItems
        !> **Type-Bound Function**: GetItemPtr <br>
        !  **Purpose**:  To get a pointer to an item stored in a collection.  The pointer is
        !       intended to be used as a mold for the item (i.e. to provide type of the stored
        !       items).  Return null pointer if the table is empty. <br>
        !  **Usage**: <br>
        !   --->    ValPtr => Collection%GetItemPtr()
        PROCEDURE   :: GetItemPtr    => BaseIterable_GetItemPtr
        ! ---------------------------------------------------------------------
        ! -----       Public Procedures To Be Used Internally             -----
        ! ---------------------------------------------------------------------
        !> *SetMold* is a procedure to set a mold of stored items.  This procedure is intended
        !   to be used only by a subtype of the *BaseIterable* type, not by a user.
        PROCEDURE   :: SetMold      => BaseIterable_SetMold
        !> *FreeMold* is a procedure to free the mold of stored items.  This procedure is intended
        !   to be used only by a subtype of the *BaseIterable* type, not by a user.
        PROCEDURE   :: FreeMold     => BaseIterable_FreeMold
        !> *IsItemValid* is a procedure to check whether the specified item is valid or not.  This
        !   procedure is intended to be used only by a subtype of the *BaseIterable* type, not by
        !   a user.
        PROCEDURE   :: IsItemValid  => BaseIterable_IsItemValid
        ! ---------------------------------------------------------------------
    END TYPE BaseIterable

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> *IfaceStartFirst* is an interface for a procedure to move to the first element in an
        !   iterable collection.
        FUNCTION IfaceStartFirst(Collection, Item, ItemCopy) RESULT(IsEmpty)
            IMPORT
            CLASS(BaseIterable), INTENT(INOUT)  :: Collection   !! iterable collection object
            !% the first element as output if requested (and available)
            CLASS(*), OPTIONAL,  INTENT(INOUT)  :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> a flag indicating whether the collection is empty or not <br>
            tLogical                            :: IsEmpty
        END FUNCTION
        !> *IfaceMoveNext* is an interface for a procedure to move to the next element in an
        !   iterable collection.
        FUNCTION IfaceMoveNext(Collection, Item, ItemCopy) RESULT(IsTheEnd)
            IMPORT
            CLASS(BaseIterable), INTENT(INOUT)  :: Collection   !! iterable collection object
            !% the next element as output if requested (and available)
            CLASS(*), OPTIONAL,  INTENT(INOUT)  :: Item
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> a flag indicating whether the end of the collection is encountered or not <br>
            tLogical                            :: IsTheEnd
        END FUNCTION
        !> *IfaceInsert* is an interface for a procedure to add an item to a collection.  For a
        !  collection with various *add* procedures.  This procedure should be the default one.
        SUBROUTINE IfaceInsert(Collection, Item)
            IMPORT
            CLASS(BaseIterable), INTENT(INOUT)  :: Collection   !! iterable collection object
            !% the item to be added to the collection
            CLASS(*),            INTENT(IN)     :: Item
        END SUBROUTINE
        !> *IfaceDelete* is an interface for a procedure to delete an item from a collection.
        !   This procedure is intended to be used in conjunction with the *StartFirst* and
        !   *MoveForward* methods.  Therefore, after the call to either method and then calling
        !   this procedure will result in a removal of the current item of the iteration (i.e.
        !   the same item that can be retrieved via those iteration methods).
        SUBROUTINE IfaceDelete(Collection)
            IMPORT
            CLASS(BaseIterable), INTENT(INOUT)  :: Collection   !! iterable collection object
        END SUBROUTINE
        !> *IfaceToArray* is an interface for a procedure to get and remove all items from the
        !   collection.  Also, return a flag indicating whether the items are successfully
        !   removed or not.
        FUNCTION IfaceToArray(Collection, Items, ItemCopy) RESULT(Success)
            IMPORT
            CLASS(BaseIterable), INTENT(INOUT)  :: Collection   !! iterable collection object
            !% the items to be retrieved and removed from the collection
            CLASS(*),            INTENT(INOUT)  :: Items(:)
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the items are successfully removed. <br>
            tLogical                            :: Success
        END FUNCTION
        !> *IfaceGetAll* is an interface for a procedure to get all items (without removing them)
        !   from the collection.  Also, return a flag indicating whether the items are successfully
        !   retrieved or not.
        FUNCTION IfaceGetAll(Collection, Items, ItemCopy) RESULT(Success)
            IMPORT
	        !% iterable collection object
            CLASS(BaseIterable), INTENT(INOUT)  :: Collection
            !% the items to be retrieved from the collection
            CLASS(*),            INTENT(INOUT)  :: Items(1:)
            !> a procedure to copy stored items for a derived type not in the *Object* class; <br>
            !  required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL  :: ItemCopy
            !> flag indicating whether the items are successfully retrieved. <br>
            tLogical                            :: Success
        END FUNCTION
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    !  na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE BaseIterable_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the items from the collection.  This routine provides a basic implementation
    !  of the *Clear* deferred procedure required by the *BaseCollection* class. <br>
    !  This routine should be overridden if a better implementation is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseIterable), INTENT(INOUT)  :: Collection   !! collection object
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: TheEnd

!** FLOW:

    TheEnd = Collection%StartFirst()
    DO WHILE (.NOT.TheEnd)
        CALL Collection%Delete()
        TheEnd = Collection%MoveForward()
    END DO

    RETURN

END SUBROUTINE BaseIterable_ClearItems

!**************************************************************************************

FUNCTION BaseIterable_GetItemPtr(Collection) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a pointer to an item stored in a collection.  The pointer is intended to be
    !  used as a mold for the item (i.e. provides type of the stored items).  Return null
    !  pointer if the table is empty.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseIterable), TARGET, INTENT(IN) :: Collection   !! collection
    CLASS(*),            POINTER            :: Val          !! pointer to a stored item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(Collection%Mold)) THEN
        Val => Collection%Mold
    ELSE
        Val => NULL()
    END IF


    RETURN

END FUNCTION BaseIterable_GetItemPtr

!**************************************************************************************

SUBROUTINE BaseIterable_SetMold(Collection, Mold)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set the "Mold" component. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseIterable), INTENT(INOUT)  :: Collection   !! collection object
    CLASS(*),            INTENT(IN)     :: Mold         !! mold for stored items
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (.NOT.ALLOCATED(Collection%Mold)) ALLOCATE(Collection%Mold, MOLD=Mold)

    RETURN

ENDSUBROUTINE BaseIterable_SetMold

!**************************************************************************************

SUBROUTINE BaseIterable_FreeMold(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To free the "Mold" component. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseIterable), INTENT(INOUT)  :: Collection   !! collection object
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (ALLOCATED(Collection%Mold)) DEALLOCATE(Collection%Mold)

    RETURN

END SUBROUTINE BaseIterable_FreeMold

!**************************************************************************************

FUNCTION BaseIterable_IsItemValid(Collection, Item) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To check whether the type of specified item is valid or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseIterable), INTENT(INOUT)  :: Collection   !! collection object
    CLASS(*),            INTENT(IN)     :: Item         !! the item to be added to the collection
    tLogical                            :: Valid        !! true if type of the specified item is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether the mold and the specified item have the same type or not
    IF (ALLOCATED(Collection%Mold)) THEN
        Valid = IsSameDataType(Collection%Mold, Item)
    ELSE
        ! this is the first item so set it as the mold
        CALL Collection%SetMold(Item)
        Valid = TrueVal
    END IF

    RETURN

END FUNCTION BaseIterable_IsItemValid

!**************************************************************************************

END MODULE MClass_BaseIterable

!******************************************************************************
