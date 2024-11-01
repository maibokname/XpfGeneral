
MODULE MClass_GenData

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *GenData* type and its related routines.  The *GenData* type is
!   a data type that provides a generic data storage.  It can be used to store any data types.
!   The *GenData* type is a client of the *MemoryPool* type where, internally, it uses the
!   storage of the *MemoryPool* type to store data of intrinsic types.  For data of derived
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
    USE MBase_ByteUtil
    USE MBase_WriteUtil
#ifdef Indx32Bits
    USE MBase_SimpleHash32, ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64, ONLY: ComputeHash => Hash64_FNV1a
#endif
    USE MClass_Object,      ONLY: Object
    USE MClass_MemoryPool

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: GenData
    ! auxiliary procedures
    PUBLIC :: GetDataType
    PUBLIC :: IsSameDataType
    ! memory-handling procedures
    PUBLIC :: MemFree
    PUBLIC :: MemAlloc
    PUBLIC :: MemResize
    ! abstract interface
    PUBLIC :: IfacePolyCopy

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_GenData'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! parameters for data types
    tSInt32, PUBLIC, PARAMETER  :: TYPE_UNKNOWN   = 0
    tSInt32, PUBLIC, PARAMETER  :: TYPE_INT8      = 1
    tSInt32, PUBLIC, PARAMETER  :: TYPE_INT16     = 2
    tSInt32, PUBLIC, PARAMETER  :: TYPE_INT32     = 3
    tSInt32, PUBLIC, PARAMETER  :: TYPE_INT64     = 4
    tSInt32, PUBLIC, PARAMETER  :: TYPE_REAL32    = 5
    tSInt32, PUBLIC, PARAMETER  :: TYPE_REAL64    = 6
    tSInt32, PUBLIC, PARAMETER  :: TYPE_REAL128   = 7
    tSInt32, PUBLIC, PARAMETER  :: TYPE_CMPX32    = 8
    tSInt32, PUBLIC, PARAMETER  :: TYPE_CMPX64    = 9
    tSInt32, PUBLIC, PARAMETER  :: TYPE_CMPX128   = 10
    tSInt32, PUBLIC, PARAMETER  :: TYPE_LOGICAL   = 11
    tSInt32, PUBLIC, PARAMETER  :: TYPE_BOOLEAN8  = 12
    tSInt32, PUBLIC, PARAMETER  :: TYPE_BOOLEAN16 = 13
    tSInt32, PUBLIC, PARAMETER  :: TYPE_BOOLEAN64 = 14
    tSInt32, PUBLIC, PARAMETER  :: TYPE_CHARACTER = 15
    tSInt32, PUBLIC, PARAMETER  :: TYPE_OBJECT    = 16
    tSInt32, PUBLIC, PARAMETER  :: TYPE_DERIVED   = 17

!** DERIVED TYPE DEFINITIONS
    !> The *GenData* type is a generic storage type that can hold any data types.
    TYPE, EXTENDS(Object)   :: GenData
        PRIVATE
        !> *Type* is the type of the stored item
        tSInt32                     :: Type = TYPE_UNKNOWN
        !> memory handle
        TYPE(MemHandle)             :: Handle
        !> a pointer to the memory pool storage of a data of any intrinsic type
        TYPE(MemoryPool), POINTER   :: Pool => NULL()
        !> *Drvt* is a storage of a data of any derived type.
        CLASS(*),     ALLOCATABLE   :: Drvt
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Set <br>
        ! **Purpose**:  To store the specified data content in the storage. <br>
        !  **Usage**: <br>
        !   --->    CALL Store%Set(Data, Pool)
        PROCEDURE   :: Set          => GenData_SetData
        !> **Type-Bound Function**: Get <br>
        ! **Purpose**:  To retrieve the data content from the storage.  If type of the specified
        !               output content does not match that of stored content, return false and the
        !               output content is not modified.  Otherwise, return true and copy the stored
        !               content to the output content. <br>
        !  **Usage**: <br>
        !   ! to get data of an intrinsic type or a derived type in the Object class <br>
        !   --->    Valid = Store%Get(Data) <br>
        !   ! to get data of a derived type NOT in the Object class <br>
        !   --->    Valid = Store%Get(Data, CopyProc) <br>
        !  **Important Note**: To use this method properly, the following conditions are applied. <br>
        !   1. The type of stored data must be known.  The *GetType* method can be used to inquire
        !      the type of stored data. <br>
        !   2. If the type of stored data is *CHARACTER*.  Length of the specified output data should
        !      be equal to or greater than that of the stored data. Otherwise, the output data would
        !      contain a truncated string.  The *GetSize* method can be used to inquire this required
        !      length. <br>
        !   3. If the type of stored data is a derived type, the specified output data must have the
        !      same concrete type as that of the stored data. <br>
        !   4. If the derived type NOT in the *Object* class has allocatable and/or pointer components,
        !      a procedure to copy the data must be specified.  Alternative, the specified output data
        !      and the stored data must have the same storage sizes (i.e. those allocatable/pointer
        !      components are allocated to the same sizes).  Otherwise, the routine will set the Valid
        !      flag to false and return without modifying the specified output data content. <br>
        PROCEDURE   :: Get          => GenData_GetData
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the storage is currently empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Store%IsEmpty() <br>
        !   --->    IF (.NOT.Store%IsEmpty()) DoSomething <br>
        PROCEDURE   :: IsEmpty      => GenData_IsEmpty
        !> **Type-Bound Function**: GetType <br>
        !  **Purpose**:  To get type of the stored content. <br>
        !  **Usage**: <br>
        !   --->    DatType = Store%GetType() <br>
        PROCEDURE   :: GetType      => GenData_GetType
        !> **Size-Bound Function**: GetSize <br>
        !  **Purpose**:  To get size of the stored content in bytes. <br>
        !  **Usage**: <br>
        !   --->    DatSize = Store%GetSize() <br>
        !  **Note**: This method is only useful if the data type is *CHARACTER*. <br>
        PROCEDURE   :: GetSize      => GenData_GetSize
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => GenData_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => GenData_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => GenData_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => GenData_ToString
        ! ---------------------------------------------------------------------
        ! -----                     Overridden Procedure                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => GenData_HashCode
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: GenData_Finalize
        ! ---------------------------------------------------------------------
    END TYPE GenData

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !^ *IfacePolyCopy* is an interface for a procedure to copy an unlimited polymorphic entity
        !  where both the source and the destination must have the same concrete type.  If their
        !  concrete types are not the same, the routine should set the *Valid* flag to false and
        !  return immediately. <br>
        FUNCTION IfacePolyCopy(SrcData, DstData) RESULT(Valid)
            IMPORT
            CLASS(*), INTENT(IN)    :: SrcData  !! input (source)
            CLASS(*), INTENT(OUT)   :: DstData  !! output (destination)
            tLogical                :: Valid    !! true if no error while copying data
        END FUNCTION IfacePolyCopy
    END INTERFACE
    INTERFACE MemFree
        !^ **Subroutine Interface**: MemFree <br>
        !  **Purpose**:  To free memory of the specified argument. <br>
        !  **Usage**: <br>
        !   --->    CALL MemFree(A) <br>
        MODULE PROCEDURE GenData_MemFree_Alloc
        MODULE PROCEDURE GenData_MemFree_Ptr
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
        MODULE PROCEDURE GenData_MemAlloc_Alloc
        MODULE PROCEDURE GenData_MemAlloc_Ptr
        MODULE PROCEDURE GenData_MemAlloc_Alloc1D
        MODULE PROCEDURE GenData_MemAlloc_Ptr1D
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
        MODULE PROCEDURE GenData_MemResize_Alloc
        MODULE PROCEDURE GenData_MemResize_Ptr
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE GenData_Finalize(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform finalization of the GenData object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenData), INTENT(INOUT)    :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%MemFree()

    RETURN

END SUBROUTINE GenData_Finalize

!******************************************************************************

SUBROUTINE GenData_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the GenData object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenData),     INTENT(IN)  :: SrcObj   !! source object
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
    TYPE IS (GenData)
        DstObj%Type   =  SrcObj%Type
        DstObj%Handle =  SrcObj%Handle
        DstObj%Pool   => SrcObj%Pool
        IF (ALLOCATED(SrcObj%Drvt)) ALLOCATE(DstObj%Drvt, SOURCE=SrcObj%Drvt)
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('GenData_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE GenData_Copy

!******************************************************************************

FUNCTION GenData_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenData), INTENT(IN)  :: LhsObj   !! an object
    CLASS(Object),  INTENT(IN)  :: RhsObj   !! another object
    tLogical                    :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (GenData)
        Flag = FalseVal
        IF (LhsObj%Type /= RhsObj%Type) RETURN
        SELECT CASE (LhsObj%Type)
        CASE (TYPE_INT8:TYPE_CHARACTER)
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
        CASE (TYPE_OBJECT)
            ! derived type in the Object class
            SELECT TYPE (LhsDat => LhsObj%Drvt)
            CLASS IS (Object)
                SELECT TYPE (RhsDat => RhsObj%Drvt)
                CLASS IS (Object)
                    Flag = LhsDat%IsEqualTo(RhsDat)
                END SELECT
            END SELECT
        CASE (TYPE_DERIVED)
            ! other derived type so must check whether their dynamic type are the same or not
            IF (SAME_TYPE_AS(LhsObj%Drvt, RhsObj%Drvt)) THEN
                BLOCK
                    ! local variables
                    tIndex          :: LhsSize
                    tSInt8, POINTER :: LhsBytes(:)
                    tSInt8, POINTER :: RhsBytes(:)
                    tIndex          :: I
                    ! transform the specified content into 8-bit integers
                    LhsSize = AnyType_GetByteSize(LhsObj%Drvt)
                    IF (LhsSize /= AnyType_GetByteSize(RhsObj%Drvt)) RETURN
                    CALL AnyType_2_ByteArrPtr(LhsObj%Drvt, LhsSize, LhsBytes)
                    CALL AnyType_2_ByteArrPtr(RhsObj%Drvt, LhsSize, RhsBytes)
                    DO I = 1_kIndex, LhsSize
                        IF (LhsBytes(I) /= RhsBytes(I)) THEN
                            NULLIFY(LhsBytes, RhsBytes)
                            RETURN
                        END IF
                    END DO
                    NULLIFY(LhsBytes, RhsBytes)
                END BLOCK
                Flag = TrueVal
            END IF
        CASE DEFAULT
            ! both are empty
            Flag = TrueVal
        END SELECT
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION GenData_IsEqualTo

!******************************************************************************

SUBROUTINE GenData_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of the GenData object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenData), INTENT(INOUT)   :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Obj%Type = TYPE_UNKNOWN
    IF (ASSOCIATED(Obj%Pool)) CALL Obj%Pool%Release(Obj%Handle)
    NULLIFY(Obj%Pool)
    IF (ALLOCATED(Obj%Drvt)) DEALLOCATE(Obj%Drvt)

    RETURN

END SUBROUTINE GenData_MemFree

!******************************************************************************

FUNCTION GenData_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenData), INTENT(IN)  :: Obj
    tCharAlloc                  :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:),   POINTER  :: ValStr
    tSInt8,        POINTER  :: DatPtr(:)
    tCharLen(128), TARGET   :: TmpStr
    tCharAlloc,    TARGET   :: AllocStr
    tSInt32                 :: NumLen

! FLOW
    
    IF (Obj%IsEmpty()) THEN
        Str = '{GenData : NULL}'
        RETURN
    END IF

    ! retrieve stored data
    IF (Obj%Type < TYPE_OBJECT) DatPtr => Obj%Pool%Retrieve(Obj%Handle)

    ! get string representing the content
    SELECT CASE (Obj%Type)
    CASE (TYPE_CHARACTER)
        BLOCK
            tCharLen(Obj%Handle%GetSize()), TARGET  :: DatStr
            CALL ByteArray_2_AnyType(DatPtr, DatStr)
            ValStr => DatStr
        END BLOCK
    CASE (TYPE_INT32)
        BLOCK
            tSInt32     :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (TYPE_INT64)
        BLOCK
            tSInt64     :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (TYPE_REAL32)
        BLOCK
            tRealSP     :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (TYPE_REAL64)
        BLOCK
            tRealDP     :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (TYPE_CMPX32)
        BLOCK
            tCmpxSP     :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (TYPE_CMPX64)
        BLOCK
            tCmpxDP     :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (TYPE_INT8)
        BLOCK
            tSInt8      :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (TYPE_INT16)
        BLOCK
            tSInt16         :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (TYPE_REAL128)
        BLOCK
            tRealQP     :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (TYPE_CMPX128)
        BLOCK
            tCmpxQP     :: DatVal
            CALL ByteArray_2_AnyType(DatPtr, DatVal)
            NumLen = WriteNumber(DatVal, TmpStr)
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (TYPE_LOGICAL, TYPE_BOOLEAN8, TYPE_BOOLEAN16, TYPE_BOOLEAN64)
        BLOCK
            tLogical    :: DatVal
            SELECT CASE (Obj%Type)
            CASE (TYPE_LOGICAL)
                CALL ByteArray_2_AnyType(DatPtr, DatVal)
            CASE (TYPE_BOOLEAN8)
                BLOCK
                    LOGICAL(KIND=1) :: LogVal
                    CALL ByteArray_2_AnyType(DatPtr, LogVal)
                    DatVal = LogVal
                END BLOCK
            CASE (TYPE_BOOLEAN16)
                BLOCK
                    LOGICAL(KIND=2) :: LogVal
                    CALL ByteArray_2_AnyType(DatPtr, LogVal)
                    DatVal = LogVal
                END BLOCK
            CASE (TYPE_BOOLEAN64)
                BLOCK
                    LOGICAL(KIND=8) :: LogVal
                    CALL ByteArray_2_AnyType(DatPtr, LogVal)
                    DatVal = LogVal
                END BLOCK
            END SELECT
            IF (DatVal) THEN
                TmpStr(1:6) = '.TRUE.'
                ValStr => TmpStr(1:6)
            ELSE
                TmpStr(1:7) = '.FALSE.'
                ValStr => TmpStr(1:7)
            END IF
            ValStr => TmpStr(1:NumLen)
        END BLOCK
    CASE (TYPE_OBJECT)
        SELECT TYPE (ObjDat => Obj%Drvt)
        CLASS IS (Object)
            AllocStr = ObjDat%ToString()
        END SELECT
        ValStr => AllocStr
    CASE (TYPE_DERIVED)
        ! any other derived type
        BLOCK
            tIndex      :: ByteSize
            ! transform the specified content into 8-bit integers
            ByteSize = AnyType_GetByteSize(Obj%Drvt)
            CALL AnyType_2_ByteArrPtr(Obj%Drvt, ByteSize, DatPtr)
            ! get hexadecimal string representing the content
            IF (IsLittleEndian) THEN
                CALL ToHexStr_LE(DatPtr, AllocStr)
            ELSE
                CALL ToHexStr_BE(DatPtr, AllocStr)
            END IF
            ValStr => AllocStr
        END BLOCK
    END SELECT

    Str = '{GenData : ' // ValStr // '}'
    NULLIFY(ValStr, DatPtr)

    RETURN

END FUNCTION GenData_ToString

!******************************************************************************

FUNCTION GenData_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenData), INTENT(IN)  :: Obj
    tIndex                      :: Code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt8, POINTER :: DatPtr(:)

! FLOW

    SELECT CASE (Obj%Type)
    CASE (TYPE_INT8:TYPE_CHARACTER)
        ! retrieve stored data
        DatPtr => Obj%Pool%Retrieve(Obj%Handle)
        Code = ComputeHash(DatPtr, SIZE(DatPtr, KIND=kIndex))
    CASE (TYPE_OBJECT, TYPE_DERIVED)
        Code = ComputeHash(Obj%Drvt, AnyType_GetByteSize(Obj%Drvt))
    CASE DEFAULT
        Code = 0_kIndex
    END SELECT
    
    RETURN

END FUNCTION GenData_HashCode

!******************************************************************************

SUBROUTINE GenData_SetData(Store, Content, Pool)

!** PURPOSE OF THIS SUBROUTINE:
    !! To store the data content in the *GenData* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenData),           INTENT(INOUT) :: Store    !! *GenData* object
    CLASS(*),                 INTENT(IN)    :: Content  !! data content
    TYPE(MemoryPool), TARGET, INTENT(INOUT) :: Pool     !! memory pool that actually stores the data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Store%Type = GetDataType(Content)
    SELECT CASE (Store%Type)
    CASE (TYPE_INT8:TYPE_CHARACTER)
        ! intrinsic types
        Store%Pool => Pool
        Store%Handle = Pool%Insert(Content)
    CASE (TYPE_OBJECT, TYPE_DERIVED)
        ! derived types
        IF (ALLOCATED(Store%Drvt)) DEALLOCATE(Store%Drvt)
        ALLOCATE(Store%Drvt, SOURCE=Content)
    END SELECT

    RETURN

END SUBROUTINE GenData_SetData

!******************************************************************************

FUNCTION GenData_GetData(Store, Content, PolyCopy) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the data content from the *GenData* object.  If type of the specified data content
    !  does not match that of stored data content, return false and the content is not modified.
    !  Otherwise, return true and copy the stored content to the output content. <br>
    !  **Important Note**: <br>
    !  1. If the data type is CHARACTER, length of the specified character string (content) must
    !     be greater than or equal to that of stored data.  Otherwise, the returned data content
    !     is a truncated string. <br>
    !  2. If the data type is a derived one, its concrete type must be the same as that of stored
    !     data.
    !  3. If the derived data type has allocatable and/or pointer components and this type is NOT
    !     in the *Object* class, a procedure to copy the data must be specified.  Alternative, the
    !     specified data content may allocate those components to the same sizes as those allocated
    !     components of the stored data (i.e. the storage sizes of the specified data and the stored
    !     data must be the same).  Otherwise, the routine will return without modifying the specified
    !     data content (i.e. *Valid* is set to false). <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenData),           INTENT(IN)    :: Store    !! *GenData* object
    CLASS(*),                 INTENT(INOUT) :: Content  !! data content
    PROCEDURE(IfacePolyCopy), OPTIONAL      :: PolyCopy
    !^ a procedure to copy stored data for a derived type not in the Object class; <br>
    !  required if this type has allocatable/pointer component(s).
    tLogical                                :: Valid
    !^ true if the type of the specified item matches that of the stored content

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt8, POINTER :: ByteStore(:)

! FLOW
    
    IF (Store%Type == GetDataType(Content)) THEN
        SELECT CASE (Store%Type)
        CASE (TYPE_INT8:TYPE_BOOLEAN64)
            ! set flag
            Valid = TrueVal
            ! retrieve stored data
            ByteStore => Store%Pool%Retrieve(Store%Handle)
            CALL ByteArray_2_AnyType(ByteStore, Content)
        CASE (TYPE_CHARACTER)
            ! set flag
            Valid = TrueVal
            ! retrieve stored data
            ByteStore => Store%Pool%Retrieve(Store%Handle)
            BLOCK
                tIndex  :: OutSize, CopySize
                OutSize = AnyType_GetByteSize(Content)
                CopySize = MIN(OutSize, Store%Handle%GetSize())
                CALL ByteArray_2_AnyType(ByteStore(1:CopySize), Content)
            END BLOCK
        CASE (TYPE_OBJECT, TYPE_DERIVED)
            ! derived type so must check if its dynamic type is the same as that of the stored data
            IF (SAME_TYPE_AS(Store%Drvt, Content)) THEN
                IF (Store%Type == TYPE_OBJECT) THEN
                    ! a derived type in the Object class
                    Valid = TrueVal
                    SELECT TYPE (Drvt => Store%Drvt)
                    CLASS IS (Object)
                        SELECT TYPE (Content)
                        CLASS IS (Object)
                            ! get the content
                            CALL Drvt%Copy(Content)
                        END SELECT
                    END SELECT
                ELSE
                    ! other derive type
                    IF (PRESENT(PolyCopy)) THEN
                        ! use the specified procedure to copy data
                        Valid = PolyCopy(Store%Drvt, Content)
                    ELSE
                        IF (STORAGE_SIZE(Store%Drvt) == STORAGE_SIZE(Content)) THEN
                            ! set flag
                            Valid = TrueVal
                            BLOCK
                                tIndex          :: OutSize
                                tSInt8, POINTER :: ByteOut(:)
                                ! transform the specified content into 8-bit integers
                                OutSize = AnyType_GetByteSize(Content)
                                CALL AnyType_2_ByteArrPtr(Content,    OutSize, ByteOut)
                                CALL AnyType_2_ByteArrPtr(Store%Drvt, OutSize, ByteStore)
                                ! get the content
                                ByteOut(1:OutSize) = ByteStore(1:OutSize)
                                NULLIFY(ByteOut)
                            END BLOCK
                        ELSE
                            ! set flag
                            Valid = FalseVal
                        END IF
                    END IF
                END IF
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

END FUNCTION GenData_GetData

!******************************************************************************

FUNCTION GenData_IsEmpty(Obj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the storage is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenData), INTENT(IN)  :: Obj
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = (Obj%Type == TYPE_UNKNOWN)

    RETURN

END FUNCTION GenData_IsEmpty

!******************************************************************************

FUNCTION GenData_GetType(Obj) RESULT(Type)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the type of stored data content.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenData), INTENT(IN)  :: Obj
    tSInt32                     :: Type

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Type = Obj%Type

    RETURN

END FUNCTION GenData_GetType

!******************************************************************************

FUNCTION GenData_GetSize(Obj) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the size of stored data content in bytes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenData), INTENT(IN)  :: Obj
    tIndex                      :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(Obj%Drvt)) THEN
        Size = AnyType_GetByteSize(Obj%Drvt)
    ELSE
        Size = Obj%Handle%GetSize()
    END IF

    RETURN

END FUNCTION GenData_GetSize

! -----------------------------------------------------------------------------
! -----                     Auxiliary Procedures                          -----
! -----------------------------------------------------------------------------

FUNCTION GetDataType(Input) RESULT(DatType)

!** PURPOSE OF THIS FUNCTION:
    !^ To return the type of the specified input.

!** FUNCTION ARGUMENT DEFINITIONS:
    CLASS(*), INTENT(IN)    :: Input
    tSInt32                 :: DatType

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! get size of the specified value in bytes
    SELECT TYPE (Input)
    TYPE IS (tCharStar)
        DatType = TYPE_CHARACTER
    TYPE IS (tSInt32)
        DatType = TYPE_INT32
    TYPE IS (tSInt64)
        DatType = TYPE_INT64
    TYPE IS (tRealSP)
        DatType = TYPE_REAL32
    TYPE IS (tRealDP)
        DatType = TYPE_REAL64
    TYPE IS (tCmpxSP)
        DatType = TYPE_CMPX32
    TYPE IS (tCmpxDP)
        DatType = TYPE_CMPX64
    CLASS IS (Object)
        DatType = TYPE_OBJECT
    TYPE IS (tSInt8)
        DatType = TYPE_INT8
    TYPE IS (tLogical)
        DatType = TYPE_LOGICAL
    TYPE IS (tSInt16)
        DatType = TYPE_INT16
    TYPE IS (tRealQP)
        DatType = TYPE_REAL128
    TYPE IS (tCmpxQP)
        DatType = TYPE_CMPX128
    TYPE IS (LOGICAL(KIND=1))
        DatType = TYPE_BOOLEAN8
    TYPE IS (LOGICAL(KIND=2))
        DatType = TYPE_BOOLEAN16
    TYPE IS (LOGICAL(KIND=8))
        DatType = TYPE_BOOLEAN64
    CLASS DEFAULT
        DatType = TYPE_DERIVED
    END SELECT

    RETURN

END FUNCTION GetDataType

!******************************************************************************

FUNCTION IsSameDataType(A, B) RESULT(Flag)

!** PURPOSE OF THIS FUNCTION:
    !^ To check whether A and B have the same (dynamic) type or not.

!** FUNCTION ARGUMENT DEFINITIONS:
    CLASS(*), INTENT(IN)    :: A, B
    tLogical                :: Flag

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: AType

!** FLOW

    AType = GetDataType(A)
    IF (AType /= GetDataType(B)) THEN
        Flag = FalseVal
    ELSE
        IF (AType <= TYPE_CHARACTER) THEN
            ! intrinsic types
            Flag = TrueVal
        ELSE
            ! derived type
            Flag = SAME_TYPE_AS(A, B)
        END IF
    END IF

    RETURN

END FUNCTION IsSameDataType

!******************************************************************************

!+++++ Macro Definitions for Memory-Handling Procedures +++++
! allocatable procedures
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
#define MEM_MOVE(A,B)       MOVE_ALLOCATABLE_ARRAY(A, B)
#define TypeName            TYPE(GenData)
#define MemFree_Generic     GenData_MemFree_Alloc
#define MemAlloc_Generic    GenData_MemAlloc_Alloc
#define MemAlloc_OneD       GenData_MemAlloc_Alloc1D
#define MemResize_Generic   GenData_MemResize_Alloc
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
#define TypeName            TYPE(GenData)
#define MemFree_Generic     GenData_MemFree_Ptr
#define MemAlloc_Generic    GenData_MemAlloc_Ptr
#define MemAlloc_OneD       GenData_MemAlloc_Ptr1D
#define MemResize_Generic   GenData_MemResize_Ptr
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

END MODULE MClass_GenData

!******************************************************************************
