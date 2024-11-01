
MODULE MClass_KeyOrdered

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *KeyOrdered* type and its related routines.  The *KeyOrdered*
!   type is a data type that provides a storage for an ordered key.  It can be used to store
!   any valid ordered key types.  This data type is intended to be used in conjunction with
!   an ordered associative (key-value) container. <br>
!   The comparison between ordered keys provides a total ordering on the key objects.  This
!   ordering is commonly referred to as the key's natural ordering, and the *CompareTo* method
!   provided for the *KeyOrdered* type is referred to as its natural comparison method. <br>
!   For the *KeyOrdered* type implemented here, the valid ordered key types include any Fortran
!   intrinsic types that have a *natural ordering* and any concrete derived type that is a subtype
!   of the *Comparable* type.  The valid Fortran intrinsic key types include the *CHARACTER* type,
!   *INTEGER* types and *REAL* types. <br>
!   The *KeyOrdered* type is a client of the *MemoryPool* type where, internally, it uses the
!   storage of the *MemoryPool* type to store key data of intrinsic types.  For key data of derived
!   types, it uses an unlimited polymorphic entity to store the specified data. <br>
!   *Note*: By design, the *GenData* type requires the user to supply the *MemoryPool* type
!   when the *Set* method is called.  This means that the user is an owner of the specified
!   *MemoryPool* type and must be responsible for managing the memory pool.  If the type of
!   the data to be stored is a derived one, the user may specify a null pointer as an argument
!   to the *Set* method. <br>

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_MemHandlers
    USE MBase_ByteUtil
    USE MBase_WriteUtil
#ifdef Indx32Bits
    USE MBase_SimpleHash32, ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64, ONLY: ComputeHash => Hash64_FNV1a
#endif
    USE MClass_Object,      ONLY: Object
    USE MClass_Comparable,  ONLY: Comparable
    USE MClass_MemoryPool

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: KeyOrdered
    ! auxiliary procedures
    PUBLIC :: GetKeyOrderedType
    PUBLIC :: IsKeyOrdered
    PUBLIC :: IsSameKeyOrdered
    ! memory-handling procedures
    PUBLIC :: MemFree
    PUBLIC :: MemAlloc
    PUBLIC :: MemResize

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_KeyOrdered'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! parameters for key types
    tSInt32, PUBLIC, PARAMETER  :: KEY_INVALID    = -1
    tSInt32, PUBLIC, PARAMETER  :: KEY_UNKNOWN    = 0
    tSInt32, PUBLIC, PARAMETER  :: KEY_INT8       = 1
    tSInt32, PUBLIC, PARAMETER  :: KEY_INT16      = 2
    tSInt32, PUBLIC, PARAMETER  :: KEY_INT32      = 3
    tSInt32, PUBLIC, PARAMETER  :: KEY_INT64      = 4
    tSInt32, PUBLIC, PARAMETER  :: KEY_REAL32     = 5
    tSInt32, PUBLIC, PARAMETER  :: KEY_REAL64     = 6
    tSInt32, PUBLIC, PARAMETER  :: KEY_REAL128    = 7
    tSInt32, PUBLIC, PARAMETER  :: KEY_CHARACTER  = 8
    tSInt32, PUBLIC, PARAMETER  :: KEY_COMPARABLE = 9

!** DERIVED TYPE DEFINITIONS
    !> The *KeyOrdered* type is a generic storage type that can hold any data types.
    TYPE, EXTENDS(Comparable)   :: KeyOrdered
        PRIVATE
        !> *Type* is the type of the stored item
        tSInt32                     :: Type = KEY_UNKNOWN
        !> memory handle
        TYPE(MemHandle)             :: Handle
        !> a pointer to the memory pool storage of a data of any intrinsic type
        TYPE(MemoryPool),  POINTER  :: Pool => NULL()
        !> *Drvt* is a storage of a data of any derived type in the *Comparable* class.
        CLASS(*),      ALLOCATABLE  :: Drvt
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Set <br>
        ! **Purpose**:  To store the specified key content in the storage. <br>
        !  **Usage**: <br>
        !   --->    CALL Store%Set(Data, Pool)
        PROCEDURE   :: Set          => KeyOrdered_SetData
        !> **Type-Bound Function**: Get <br>
        ! **Purpose**:  To retrieve the key content from the storage.  If type of the specified
        !               output content does not match that of stored content, return false and the
        !               output content is not modified.  Otherwise, return true and copy the stored
        !               content to the output content. <br>
        !  **Usage**: <br>
        !   ! to get data of an intrinsic type or a derived type in the Comparable class <br>
        !   --->    Valid = Store%Get(Data) <br>
        !  **Important Note**: To use this method properly, the following conditions are applied. <br>
        !   1. The type of stored key must be known.  The *GetType* method can be used to inquire
        !      the type of stored key. <br>
        !   2. If the type of stored key is *CHARACTER*.  Length of the specified output data should
        !      be equal to or greater than that of the stored key. Otherwise, the output data would
        !      contain a truncated string.  The *GetSize* method can be used to inquire this required
        !      length. <br>
        !   3. If the type of stored key is a derived type in the *Comparable* class, the specified
        !      output data must have the same concrete type as that of the stored key. <br>
        PROCEDURE   :: Get          => KeyOrdered_GetData
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the storage is currently empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Store%IsEmpty() <br>
        !   --->    IF (.NOT.Store%IsEmpty()) DoSomething <br>
        PROCEDURE   :: IsEmpty      => KeyOrdered_IsEmpty
        !> **Type-Bound Function**: GetType <br>
        !  **Purpose**:  To get type of the stored content. <br>
        !  **Usage**: <br>
        !   --->    DatType = Store%GetType() <br>
        PROCEDURE   :: GetType      => KeyOrdered_GetType
        !> **Size-Bound Function**: GetSize <br>
        !  **Purpose**:  To get size of the stored content in bytes. <br>
        !  **Usage**: <br>
        !   --->    DatSize = Store%GetSize() <br>
        !  **Note**: This method is only useful if the data type is *CHARACTER*. <br>
        PROCEDURE   :: GetSize      => KeyOrdered_GetSize
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => KeyOrdered_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => KeyOrdered_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => KeyOrdered_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => KeyOrdered_ToString
        !> **Type-Bound Function**: CompareTo <br>
        ! **Purpose**: To compare *Key1* and *Key2* and return <br>
        !              1 if *Key1* is greater than *Key2*, <br>
        !              0 if *Key1* is equal to *Key2*, <br>
        !             -1 if *Key1* is less than *Key2*, <br>
        !           -999 if types of input keys are not the same or either key is invalid. <br>
        !  **Usage**: <br>
        !   --->    Flag = KeyA%CompareTo(KeyB)
        PROCEDURE   :: CompareTo    => KeyOrdered_CompareTo
        ! ---------------------------------------------------------------------
        ! -----                     Overridden Procedure                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => KeyOrdered_HashCode
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: KeyOrdered_Finalize
        ! ---------------------------------------------------------------------
    END TYPE KeyOrdered

!** INTERFACE DEFINITIONS:
    INTERFACE MemFree
        !^ **Subroutine Interface**: MemFree <br>
        !  **Purpose**:  To free memory of the specified argument. <br>
        !  **Usage**: <br>
        !   --->    CALL MemFree(A) <br>
        MODULE PROCEDURE KeyOrdered_MemFree_Alloc
        MODULE PROCEDURE KeyOrdered_MemFree_Ptr
    END INTERFACE
    INTERFACE MemAlloc
        !^ **Subroutine Interface**: MemAlloc <br>
        !  **Purpose**:  To allocate memory of the specified argument. <br>
        !  **Usage**: <br>
        !   ! allocate a 1-D array <br>
        !   --->    CALL MemAlloc(Arr1D, 25) <br>
        !   --->    CALL MemAlloc(Arr1D, 50, StartID=-20) <br>
        !   --->    CALL MemAlloc(Arr1D, [25]) <br>
        !   --->    CALL MemAlloc(Arr1D, [50], StartID=[-20]) <br>
        !   ! allocate an array of any rank <br>
        !   --->    CALL MemAlloc(Arr2D, [30, 30]) <br>
        !   --->    CALL MemAlloc(Arr2D, [30, 30], StartID=[-10, 10]) <br>
        !   --->    CALL MemAlloc(Arr3D, [30, 40, 50]) <br>
        !   --->    CALL MemAlloc(Arr3D, [30, 40, 50], StartID=[0, 0, 0]) <br>
        !   --->    CALL MemAlloc(Arr7D, ASize) <br>
        !   --->    CALL MemAlloc(Arr7D, ASize, StartID) <br>
        !  **Note**: For an array of rank greater than 1, the "ASize" and "StartID" arguments must be
        !       arrays where their size are equal to the rank of the specified array argument. <br>
        MODULE PROCEDURE KeyOrdered_MemAlloc_Alloc
        MODULE PROCEDURE KeyOrdered_MemAlloc_Ptr
        MODULE PROCEDURE KeyOrdered_MemAlloc_Alloc1D
        MODULE PROCEDURE KeyOrdered_MemAlloc_Ptr1D
    END INTERFACE
    INTERFACE MemResize
        !^ **Subroutine Interface**: MemResize <br>
        !  **Purpose**:  To re-allocate memory of the specified argument (and preserve its data). <br>
        !  **Usage**: <br>
        !   --->    CALL MemResize(Arr1D, [25]) <br>
        !   --->    CALL MemResize(Arr2D, [30, 30]) <br>
        !   --->    CALL MemResize(Arr3D, [30, 40, 50]) <br>
        !   --->    CALL MemResize(Arr7D, NewSize) <br>
        !  **Note**: The "NewSize" argument must be an array where its size is equal to the rank of
        !       the specified array argument.  <br>
        MODULE PROCEDURE KeyOrdered_MemResize_Alloc
        MODULE PROCEDURE KeyOrdered_MemResize_Ptr
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE KeyOrdered_Finalize(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform finalization of the KeyOrdered object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(KeyOrdered), INTENT(INOUT)    :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%MemFree()

    RETURN

END SUBROUTINE KeyOrdered_Finalize

!******************************************************************************

SUBROUTINE KeyOrdered_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the KeyOrdered object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyOrdered),  INTENT(IN)  :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
    !^ Flag indicating whether to perform deep copy or shallow copy. <br>
    !  - If present and true, perform a deep copy. <br>
    !  - If present and false, perform a shallow copy. <br>
    !  - If not present, perform either a shallow or a deep copy that is naturally most suitable
    !    for the object's components.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE(DstObj)
    TYPE IS (KeyOrdered)
        DstObj%Type   =  SrcObj%Type
        DstObj%Handle =  SrcObj%Handle
        DstObj%Pool   => SrcObj%Pool
        IF (ALLOCATED(SrcObj%Drvt)) ALLOCATE(DstObj%Drvt, SOURCE=SrcObj%Drvt)
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('KeyOrdered_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE KeyOrdered_Copy

!******************************************************************************

FUNCTION KeyOrdered_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyOrdered), INTENT(IN)   :: LhsObj   !! an object
    CLASS(Object),     INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (KeyOrdered)
        Flag = FalseVal
        IF (LhsObj%Type /= RhsObj%Type) RETURN
        SELECT CASE (LhsObj%Type)
        CASE (KEY_INT8:KEY_CHARACTER)
            IF (LhsObj%Handle%GetSize() /= RhsObj%Handle%GetSize()) RETURN
            BLOCK
                tSInt8, POINTER :: LhsDatPtr(:)
                tSInt8, POINTER :: RhsDatPtr(:)
                tIndex          :: I
                LhsDatPtr => LhsObj%Pool%Retrieve(LhsObj%Handle)
                RhsDatPtr => RhsObj%Pool%Retrieve(RhsObj%Handle)
                DO I = 1_kIndex, SIZE(LhsDatPtr, KIND=kIndex)
                    IF (LhsDatPtr(I) /= RhsDatPtr(I)) THEN
                        NULLIFY(LhsDatPtr, RhsDatPtr)
                        RETURN
                    END IF
                END DO
                NULLIFY(LhsDatPtr, RhsDatPtr)
            END BLOCK
            Flag = TrueVal
        CASE (KEY_COMPARABLE)
            ! derived type in the Object class
            SELECT TYPE (LhsDat => LhsObj%Drvt)
            CLASS IS (Comparable)
                SELECT TYPE (RhsDat => RhsObj%Drvt)
                CLASS IS (Comparable)
                    Flag = LhsDat%IsEqualTo(RhsDat)
                END SELECT
            END SELECT
        END SELECT
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION KeyOrdered_IsEqualTo

!******************************************************************************

SUBROUTINE KeyOrdered_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of the KeyOrdered object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyOrdered), INTENT(INOUT)    :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Obj%Type = KEY_UNKNOWN
    IF (ASSOCIATED(Obj%Pool)) CALL Obj%Pool%Release(Obj%Handle)
    NULLIFY(Obj%Pool)
    IF (ALLOCATED(Obj%Drvt)) DEALLOCATE(Obj%Drvt)

    RETURN

END SUBROUTINE KeyOrdered_MemFree

!******************************************************************************

FUNCTION KeyOrdered_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyOrdered), INTENT(IN)   :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:),   POINTER  :: ValStr
    tSInt8,        POINTER  :: DatPtr(:)
    tCharLen(128), TARGET   :: TmpStr
    tCharAlloc,    TARGET   :: AllocStr
    tSInt32                 :: NumLen

! FLOW
    
    IF (Obj%IsEmpty()) THEN
        Str = '{KeyOrdered : NULL}'
        RETURN
    END IF

    ! retrieve stored key
    IF (Obj%Type < KEY_COMPARABLE) DatPtr => Obj%Pool%Retrieve(Obj%Handle)

    ! get string representing the content
    SELECT CASE (Obj%Type)
    CASE (KEY_CHARACTER)
        BLOCK
            tCharLen(Obj%Handle%GetSize()), TARGET  :: DatStr
            CALL ByteArray_2_AnyType(DatPtr, DatStr)
            ValStr => DatStr
        END BLOCK
    CASE (KEY_INT32)
        BLOCK
            tSInt32     :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (KEY_INT64)
        BLOCK
            tSInt64     :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (KEY_REAL32)
        BLOCK
            tRealSP     :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (KEY_REAL64)
        BLOCK
            tRealDP     :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (KEY_INT8)
        BLOCK
            tSInt8      :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (KEY_INT16)
        BLOCK
            tSInt16         :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (KEY_REAL128)
        BLOCK
            tRealQP     :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (KEY_COMPARABLE)
        SELECT TYPE (ObjDat => Obj%Drvt)
        CLASS IS (Comparable)
            AllocStr = ObjDat%ToString()
        END SELECT
        ValStr => AllocStr
    END SELECT

    Str = '{KeyOrdered : ' // ValStr // '}'
    NULLIFY(ValStr, DatPtr)

    RETURN

END FUNCTION KeyOrdered_ToString

!******************************************************************************

FUNCTION KeyOrdered_CompareTo(A, B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare *A* and *B* and return <br>
    !   1 if *A* is greater than *B*, <br>
    !   0 if *A* is equal to *B*, <br>
    !  -1 if *A* is less than *B*, <br>
    !  -999 if types of input keys are not the same or either key is invalid. <br>
    ! Also, write an error message to the default log file if this happens.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyOrdered), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tSInt32                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (B)
    TYPE IS (KeyOrdered)
        IF (A%Type == B%Type) THEN
            IF (ALLOCATED(A%Drvt)) THEN
                SELECT TYPE (AKey => A%Drvt)
                CLASS IS (Comparable)
                    SELECT TYPE (BKey => B%Drvt)
                    CLASS IS (Comparable)
                        Flag = AKey%CompareTo(BKey)
                    END SELECT
                END SELECT
            ELSE
                OuterBlock: BLOCK
                    tSInt8, POINTER  :: APtr(:)
                    tSInt8, POINTER  :: BPtr(:)
                    APtr => A%Pool%Retrieve(A%Handle)
                    BPtr => B%Pool%Retrieve(B%Handle)
                    SELECT CASE (A%Type)
                    CASE (KEY_CHARACTER)
                        BLOCK
                            tCharLen(A%Handle%GetSize()), TARGET  :: AStr
                            tCharLen(B%Handle%GetSize()), TARGET  :: BStr
                            CALL ByteArray_2_AnyType(APtr, AStr)
                            CALL ByteArray_2_AnyType(BPtr, BStr)
                            IF (LGT(AStr, BStr)) THEN
                                Flag = 1
                            ELSEIF (LLT(AStr, BStr)) THEN
                                Flag = -1
                            ELSE
                                Flag = 0
                            END IF
                        END BLOCK
                    CASE (KEY_INT32)
                        BLOCK
                            tSInt32     :: AVal, BVal
#include                    "Includes/KeyOrdered_Compare.f90"
                        END BLOCK
                    CASE (KEY_INT64)
                        BLOCK
                            tSInt64     :: AVal, BVal
#include                    "Includes/KeyOrdered_Compare.f90"
                        END BLOCK
                    CASE (KEY_REAL32)
                        BLOCK
                            tRealSP     :: AVal, BVal
#include                    "Includes/KeyOrdered_Compare.f90"
                        END BLOCK
                    CASE (KEY_REAL64)
                        BLOCK
                            tRealDP     :: AVal, BVal
#include                    "Includes/KeyOrdered_Compare.f90"
                        END BLOCK
                    CASE (KEY_INT8)
                        BLOCK
                            tSInt8      :: AVal, BVal
#include                    "Includes/KeyOrdered_Compare.f90"
                        END BLOCK
                    CASE (KEY_INT16)
                        BLOCK
                            tSInt16         :: AVal, BVal
#include                    "Includes/KeyOrdered_Compare.f90"
                        END BLOCK
                    CASE (KEY_REAL128)
                        BLOCK
                            tRealQP     :: AVal, BVal
#include                    "Includes/KeyOrdered_Compare.f90"
                        END BLOCK
                    END SELECT
                END BLOCK OuterBlock
            END IF
        ELSE
            CALL Handle_ErrLevel('KeyOrdered_CompareTo', ModName, ErrSevere, &
                                 'Types of the specified keys are NOT the same.')
        END IF
    CLASS DEFAULT
        Flag = -999
        CALL Handle_ErrLevel('KeyOrdered_CompareTo', ModName, ErrSevere, &
                             'Type of "B" is NOT a valid "KeyOrdered" type.')
        RETURN
    END SELECT

    RETURN

END FUNCTION KeyOrdered_CompareTo

!******************************************************************************

FUNCTION KeyOrdered_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyOrdered), INTENT(IN)   :: Obj
    tIndex                          :: Code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt8, POINTER :: DatPtr(:)

! FLOW

    SELECT CASE (Obj%Type)
    CASE (KEY_INT8:KEY_CHARACTER)
        ! retrieve stored key
        DatPtr => Obj%Pool%Retrieve(Obj%Handle)
        Code = ComputeHash(DatPtr, SIZE(DatPtr, KIND=kIndex))
    CASE (KEY_COMPARABLE)
        Code = ComputeHash(Obj%Drvt, AnyType_GetByteSize(Obj%Drvt))
    CASE DEFAULT
        Code = 0_kIndex
    END SELECT
    
    RETURN

END FUNCTION KeyOrdered_HashCode

!******************************************************************************

SUBROUTINE KeyOrdered_SetData(Store, Content, Pool)

!** PURPOSE OF THIS SUBROUTINE:
    !! To store the key content in the *KeyOrdered* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyOrdered),        INTENT(INOUT) :: Store    !! *KeyOrdered* object
    CLASS(*),                 INTENT(IN)    :: Content  !! key content
    TYPE(MemoryPool), TARGET, INTENT(INOUT) :: Pool     !! memory pool that actually stores the data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: KeyType

! FLOW

    KeyType = GetKeyOrderedType(Content)

    IF (KeyType /= KEY_INVALID) THEN
        Store%Type = GetKeyOrderedType(Content)
        SELECT CASE (Store%Type)
        CASE (KEY_INT8:KEY_CHARACTER)
            ! intrinsic types
            Store%Pool => Pool
            Store%Handle = Pool%Insert(Content)
        CASE (KEY_COMPARABLE)
            ! derived types in the Comparable class
            IF (ALLOCATED(Store%Drvt)) DEALLOCATE(Store%Drvt)
            ALLOCATE(Store%Drvt, SOURCE=Content)
        END SELECT
    ELSE
        CALL Handle_ErrLevel('KeyOrdered_SetData', ModName, ErrSevere, &
                             'Type of the specified input is NOT a valid "KeyOrdered" type.')
    END IF

    RETURN

END SUBROUTINE KeyOrdered_SetData

!******************************************************************************

FUNCTION KeyOrdered_GetData(Store, Content) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the key content from the *KeyOrdered* object.  If type of the specified key content
    !  does not match that of stored key content, return false and the content is not modified.
    !  Otherwise, return true and copy the stored content to the output content. <br>
    !  **Important Note**: <br>
    !  1. If the data type is CHARACTER, length of the specified character string (content) must
    !     be greater than or equal to that of stored key.  Otherwise, the returned key content
    !     is a truncated string. <br>
    !  2. If the data type is a derived type in the *Comparable* class, its concrete type must be
    !     the same as that of stored data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyOrdered), INTENT(IN)       :: Store    !! *KeyOrdered* object
    CLASS(*),          INTENT(INOUT)    :: Content  !! key content
    tLogical                            :: Valid
    !^ true if the type of the specified item matches that of the stored content

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt8, POINTER :: ByteStore(:)

! FLOW
    
    IF (Store%Type == GetKeyOrderedType(Content)) THEN
        SELECT CASE (Store%Type)
        CASE (KEY_INT8:KEY_REAL128)
            ! set flag
            Valid = TrueVal
            ! retrieve stored key
            ByteStore => Store%Pool%Retrieve(Store%Handle)
            CALL ByteArray_2_AnyType(ByteStore, Content)
        CASE (KEY_CHARACTER)
            ! set flag
            Valid = TrueVal
            ! retrieve stored key
            ByteStore => Store%Pool%Retrieve(Store%Handle)
            BLOCK
                tIndex  :: OutSize, CopySize
                OutSize = AnyType_GetByteSize(Content)
                CopySize = MIN(OutSize, Store%Handle%GetSize())
                CALL ByteArray_2_AnyType(ByteStore(1:CopySize), Content)
            END BLOCK
        CASE (KEY_COMPARABLE)
            ! derived type so must check if its dynamic type is the same as that of the stored key
            IF (SAME_TYPE_AS(Store%Drvt, Content)) THEN
                Valid = TrueVal
                SELECT TYPE (Drvt => Store%Drvt)
                CLASS IS (Comparable)
                    SELECT TYPE (Content)
                    CLASS IS (Comparable)
                        ! get the content
                        CALL Drvt%Copy(Content)
                    END SELECT
                END SELECT
            ELSE
                ! set flag
                Valid = FalseVal
            END IF
        END SELECT
        NULLIFY(ByteStore)
    ELSE
        Valid = FalseVal
    END IF

    RETURN

END FUNCTION KeyOrdered_GetData

!******************************************************************************

FUNCTION KeyOrdered_IsEmpty(Obj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the storage is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyOrdered), INTENT(IN)   :: Obj
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = (Obj%Type == KEY_UNKNOWN)

    RETURN

END FUNCTION KeyOrdered_IsEmpty

!******************************************************************************

FUNCTION KeyOrdered_GetType(Obj) RESULT(Type)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the type of stored key content.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyOrdered), INTENT(IN)   :: Obj
    tSInt32                         :: Type

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Type = Obj%Type

    RETURN

END FUNCTION KeyOrdered_GetType

!******************************************************************************

FUNCTION KeyOrdered_GetSize(Obj) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the size of stored key content in bytes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyOrdered), INTENT(IN)   :: Obj
    tIndex                          :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(Obj%Drvt)) THEN
        Size = AnyType_GetByteSize(Obj%Drvt)
    ELSE
        Size = Obj%Handle%GetSize()
    END IF

    RETURN

END FUNCTION KeyOrdered_GetSize

! -----------------------------------------------------------------------------
! -----                     Auxiliary Procedures                          -----
! -----------------------------------------------------------------------------

FUNCTION GetKeyOrderedType(Input) RESULT(KeyType)

!** PURPOSE OF THIS FUNCTION:
    !^ To return the key type of the specified input.

!** FUNCTION ARGUMENT DEFINITIONS:
    CLASS(*), INTENT(IN)    :: Input
    tSInt32                 :: KeyType

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! get size of the specified value in bytes
    SELECT TYPE (Input)
    TYPE IS (tCharStar)
        KeyType = KEY_CHARACTER
    TYPE IS (tSInt32)
        KeyType = KEY_INT32
    TYPE IS (tSInt64)
        KeyType = KEY_INT64
    TYPE IS (tRealSP)
        KeyType = KEY_REAL32
    TYPE IS (tRealDP)
        KeyType = KEY_REAL64
    CLASS IS (Comparable)
        KeyType = KEY_COMPARABLE
    TYPE IS (tSInt8)
        KeyType = KEY_INT8
    TYPE IS (tSInt16)
        KeyType = KEY_INT16
    TYPE IS (tRealQP)
        KeyType = KEY_REAL128
    CLASS DEFAULT
        KeyType = KEY_INVALID
    END SELECT

    RETURN

END FUNCTION GetKeyOrderedType

!******************************************************************************

FUNCTION IsKeyOrdered(Content) RESULT(Flag)

!** PURPOSE OF THIS FUNCTION:
    !^ To check whether type of the specified input is a valid *KeyOrdered* type.

!** FUNCTION ARGUMENT DEFINITIONS:
    CLASS(*), INTENT(IN)    :: Content
    tLogical                :: Flag

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (GetKeyOrderedType(Content) /= KEY_INVALID)

    RETURN

END FUNCTION IsKeyOrdered

!******************************************************************************

FUNCTION IsSameKeyOrdered(A, B) RESULT(Flag)

!** PURPOSE OF THIS FUNCTION:
    !^ To check whether A and B have the same (dynamic) type or not.

!** FUNCTION ARGUMENT DEFINITIONS:
    CLASS(*), INTENT(IN)    :: A, B
    tLogical                :: Flag

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: AType

!** FLOW

    ! set default value
    Flag = FalseVal

    AType = GetKeyOrderedType(A)
    IF (AType == KEY_INVALID) RETURN
    Flag = (AType == GetKeyOrderedType(B))

    RETURN

END FUNCTION IsSameKeyOrdered

!******************************************************************************

!+++++ Macro Definitions for Memory-Handling Procedures +++++
! allocatable procedures
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            TYPE(KeyOrdered)
#define MemFree_Generic     KeyOrdered_MemFree_Alloc
#define MemAlloc_Generic    KeyOrdered_MemAlloc_Alloc
#define MemAlloc_OneD       KeyOrdered_MemAlloc_Alloc1D
#define MemResize_Generic   KeyOrdered_MemResize_Alloc
#include    "../Core/Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic
!-----
! pointer procedures
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
#define MEM_MOVE(A,B)       MOVE_POINTER_ARRAY(A, B)
#define TypeName            TYPE(KeyOrdered)
#define MemFree_Generic     KeyOrdered_MemFree_Ptr
#define MemAlloc_Generic    KeyOrdered_MemAlloc_Ptr
#define MemAlloc_OneD       KeyOrdered_MemAlloc_Ptr1D
#define MemResize_Generic   KeyOrdered_MemResize_Ptr
#include    "../Core/Includes/Generic_MemHandlers.f90"
#undef  AttributeName
#undef  ArrayStatusTest
#undef  MEM_MOVE
#undef  TypeName
#undef  MemFree_Generic
#undef  MemAlloc_Generic
#undef  MemAlloc_OneD
#undef  MemResize_Generic

!******************************************************************************

END MODULE MClass_KeyOrdered

!******************************************************************************
