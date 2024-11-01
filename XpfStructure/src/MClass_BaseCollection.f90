
MODULE MClass_BaseCollection

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *BaseCollection* type and its related routines.  The *BaseCollection*
!   type is an abstract data type (ADT) representing a collection, which is a group of objects,
!   known as its items or elements.  Some collections allow duplicate elements while others do not.
!   Some are ordered collections and others are unordered collections. <br>
!   The *BaseCollection* type defines an application programming interface (API) for various common
!   operations.  All other collection types should extend from this base type. <br>
!   It is important to note that the *BaseCollection* type is a subtype of the *Object* type.
!   Therefore, it inherits all deferred procedures required by a subtype of the *Object* type. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_Object
    USE MClass_GenData, ONLY: IfacePolyCopy

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: BaseCollection

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_BaseCollection'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128

!** DERIVED TYPE 
    !> The *BaseCollection* type is an abstract collection type that defines
    !  an API for various common operations.  Some operations are deferred
    !  while others (with default implementation) can be overridden.
    TYPE, ABSTRACT, EXTENDS(Object) :: BaseCollection
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *CopyCollection* is a deferred procedure to construct a new collection from another
        !   collection.  Use the *Construct* method in place of this method. <br>
        PROCEDURE(IfaceCreate),  DEFERRED   :: CopyCollection
        !> *Clear* is a deferred procedure to remove all items from the collection. <br>
        PROCEDURE(IfaceClear),   DEFERRED   :: Clear
        !> *Destruct* is a deferred procedure to remove all items from the collection and free
        !   memory storage of items stored in the collection. <br>
        PROCEDURE(IfaceDestroy), DEFERRED   :: Destruct
        !> *GetSize* is a deferred procedure to get the current size of the collection.
        PROCEDURE(IfaceSize),    DEFERRED   :: GetSize
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new collection. <br>
        !  **Usage**: <br>
        !   ! create a collection from another collection <br>
        !   --->    CALL Collection%Construct(OtherCollection)
        GENERIC     :: Construct    => CopyCollection
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%IsEmpty() <br>
        !   --->    IF (.NOT.Collection%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty      => BaseCollection_IsEmpty
        ! ---------------------------------------------------------------------
    END TYPE BaseCollection

!** INTERFACE DEFINITIONS:
    ! abstract interface for Collection
    ABSTRACT INTERFACE
        !> *IfaceCreate* is an interface for a procedure to creates a new collection (This) with
        !  the same items as the given collection (Other).  In essence, this is a constructor
        !  that allows the user to copy items from any collection. <br>
        !  It should be noted that this procedure is slightly different from the *Copy* method
        !  inherited from the *Object* type such that types of *This* and *Other* collections
        !  can be different whereas types of *SrcObj* and *DstObj* objects must be the same. <br>
        SUBROUTINE IfaceCreate(This, Other, ItemCopy, ValCopy)
            IMPORT
            CLASS(BaseCollection), INTENT(INOUT)    :: This     !! collection to be created
            CLASS(BaseCollection), INTENT(INOUT)    :: Other    !! collection to be copied from
            !> a helper procedure to copy stored items (or keys) for a derived type not in the
            !  *Object* class; required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ItemCopy
            !> a helper procedure to copy stored values for a derived type not in the *Object*
            !  class; required if the derived type has allocatable/pointer component(s).
            PROCEDURE(IfacePolyCopy), OPTIONAL      :: ValCopy
        END SUBROUTINE
        !> *IfaceClear* is an interface for a procedure to remove all items from the collection.
        SUBROUTINE IfaceClear(Collection)
            IMPORT
            CLASS(BaseCollection), INTENT(INOUT)    :: Collection   !! collection object
        END SUBROUTINE
        !> *IfaceDestroy* is an interface for a procedure to destruct the collection where items
        !  are all removed first (this operation is essentially the same as that of the *Clear*
        !  method) and the storage of those items are then freed.   For the second operation,
        !  this may also be done by the *Clear* method for some collections.  However, for others
        !  (such as dynamic-array-based collections), this must only be done by this procedure.
        SUBROUTINE IfaceDestroy(Collection)
            IMPORT
            CLASS(BaseCollection), INTENT(INOUT)    :: Collection   !! collection object
        END SUBROUTINE
        !> *IfaceSize* is an interface for a procedure to get the current size of the collection,
        !   which represents the number of items currently stored in the collection.
        FUNCTION IfaceSize(Collection) RESULT(Size)
            IMPORT
            CLASS(BaseCollection), INTENT(IN)   :: Collection   !! collection object
            tIndex                              :: Size         !! collection size
        END FUNCTION
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION BaseCollection_IsEmpty(Collection) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether the collection is currently empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseCollection), INTENT(IN)   :: Collection   !! collection object
    tLogical                            :: Flag         !! true if the collection currently contains no item.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Flag = (Collection%GetSize() == 0)

    RETURN

END FUNCTION BaseCollection_IsEmpty

!******************************************************************************

END MODULE MClass_BaseCollection

!******************************************************************************
