
MODULE MClass_CharBuffer

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *CharBuffer* type and its related routines.  The *CharBuffer* type
!   is a buffer type that provides a conveniently simple and efficient mechanism for concatenating
!   multiple character strings (i.e. building a string from several strings).  <br>
!   The *CharBuffer* type provides several methods that can be grouped into the following
!   categories. <br>
!   (1) Construction and Destruction.  Methods for these operations include: <br>
!   - *CreateEmpty* method - method to construct an empty buffer, <br>
!   - *Construct* method - method to construct the buffer from its first specified string, and <br>
!   - *Clear* method - method to remove all inserted characters from the buffer. <br>
!   (2) Insertion.  A method for this operation is: <br>
!   - *Append* method - method to insert a string into the end of the buffer's string, <br>
!   (3) Conversion-To-String.  Methods for these operations include: <br>
!   - *AsString* method - method to return the buffer's string as an allocatable character string, and <br>
!   - *ViewString* method - method to return a pointer to the buffer's string. <br>
!   (4) Inquiry.  A method for this operation is: <br>
!   - *Length* method - method to inquire the current length of the buffer's string. <br>
!   ***Note***: The *CharBuffer* type is a subtype in the *Object* class so it also provides those deferred
!   methods required by the *Object* class. <br>
!   ***Important Note***: The *CharBuffer* type provides two methods to return character strings stored in
!   the object.  The *AsString* method exactly returns the stored character string whereas the *ToString*
!   method (inherited from the *Object* type) returns a string representation of the object, which includes
!   the stored character string and a prefix specifying the type name of the object (i.e. "CharBuffer").
!   Therefore, the user should choose the method properly. <br>

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_ByteUtil,     ONLY: AnyType_GetByteSize
#ifdef Indx32Bits
    USE MBase_SimpleHash32, ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64, ONLY: ComputeHash => Hash64_FNV1a
#endif
    USE MClass_Object,      ONLY: Object

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: CharBuffer

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'MClass_CharBuffer'
    tIndex,    PARAMETER    :: DfltInitCap = 2048_kIndex

!** DERIVED TYPE DEFINITIONS
    !> The *CharBuffer* type is a buffer type that provides a convenient way
    !  to build a string from multiple strings.
    TYPE, EXTENDS(Object)   :: CharBuffer
        PRIVATE
        tIndex      :: Count    = 0_kIndex  !! the number of characters currently stored
        tIndex      :: Capacity = 0_kIndex  !! the maximum number of characters that currently can be stored
        tCharAlloc  :: Buffer               !! CharBuffer
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: EnsureCapacity   => CharBuffer_EnsureCapacity
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty *CharBuffer* object. <br>
        !  **Usage**: <br>
        !   ! use default initial capacity  <br>
        !   --->    CALL ChrBuf%CreateEmpty() <br>
        !   ! specify initial capacity <br>
        !   --->    CALL ChrBuf%CreateEmpty(InitCap=256) <br>
        PROCEDURE   :: CreateEmpty  => CharBuffer_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a *CharBuffer* object with the specified string.  This method
        !       provides a convenient way to create an empty *CharBuffer* object and then add the
        !       first string into the object. <br>
        !  **Usage**: <br>
        !   --->    CALL ChrBuf%Construct(String) <br>
        PROCEDURE   :: Construct    => CharBuffer_Construct
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To clear the buffer string of the *CharBuffer* object. <br>
        !  **Usage**: <br>
        !   --->    CALL ChrBuf%Clear() <br>
        PROCEDURE   :: Clear        => CharBuffer_Clear
        !> **Type-Bound Subroutine**: Append <br>
        !  **Purpose**:  To append the specified string at the end of the buffer string. <br>
        !  **Usage**: <br>
        !   --->    CALL ChrBuf%Append(String) <br>
        PROCEDURE   :: Append       => CharBuffer_Append
        !> **Type-Bound Function**: AsString <br>
        !  **Purpose**:  To return characters currently stored in the CharBuffer as an
        !       allocatable character string.  Optionally, a user can request to clear the
        !       buffer' buffer string. <br>
        !  **Usage**: <br>
        !   --->    cStr = ChrBuf%AsString() <br>
        !   --->    cStr = ChrBuf%AsString(ClearBuffer=.TRUE.) <br>
        PROCEDURE   :: AsString     => CharBuffer_AsString
        !> **Type-Bound Function**: ViewString <br>
        !  **Purpose**:  To return a pointer to a string representing characters currently stored
        !                in the *CharBuffer* object. <br>
        !  **Usage**: <br>
        !   --->    StrPtr => ChrBuf%ViewString() <br>
        PROCEDURE   :: ViewString   => CharBuffer_ViewString
        !> **Type-Bound Function**: Length <br>
        !  **Purpose**:  To return the current length of the string being built. <br>
        !  **Usage**: <br>
        !   --->    Length = ChrBuf%Length() <br>
        PROCEDURE   :: Length       => CharBuffer_GetLength
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => CharBuffer_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => CharBuffer_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => CharBuffer_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => CharBuffer_ToString
        ! ---------------------------------------------------------------------
        ! -----                     Overridden Procedure                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => CharBuffer_HashCode
        ! ---------------------------------------------------------------------
        ! -----                 Final Procedure                           -----
        ! ---------------------------------------------------------------------
        !% To perform finalization of the *CharBuffer* object.
        FINAL       :: CharBuffer_Finalize
        ! ---------------------------------------------------------------------
    END TYPE CharBuffer

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE CharBuffer_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the *CharBuffer* object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharBuffer),  INTENT(IN)  :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
    !^ Flag indicating whether to perform deep copy or shallow copy. <br>
    !  - If present and true, perform a deep copy. <br>
    !  - If present and false, perform a shallow copy. <br>
    !  - If not present, perform either a shallow or a deep copy that is naturally most
    !    suitable for the object's components.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE(DstObj)
    TYPE IS (CharBuffer)
        DstObj%Count    = SrcObj%Count
        DstObj%Capacity = SrcObj%Capacity
        IF (ALLOCATED(SrcObj%Buffer)) THEN
            ALLOCATE(DstObj%Buffer, SOURCE=SrcObj%Buffer)
        END IF
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('CharBuffer_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE CharBuffer_Copy

!******************************************************************************

FUNCTION CharBuffer_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharBuffer), INTENT(IN)   :: LhsObj   !! an object
    CLASS(Object),     INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (CharBuffer)
        Flag = FalseVal
        IF (LhsObj%Count /= RhsObj%Count) RETURN
        BLOCK
            tIndex  :: I
            DO I = 1_kIndex, LhsObj%Count
                IF (LhsObj%Buffer(I:I) /= RhsObj%Buffer(I:I)) RETURN
            END DO
        END BLOCK
        Flag = TrueVal
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION CharBuffer_IsEqualTo

!******************************************************************************

SUBROUTINE CharBuffer_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of the *CharBuffer* object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharBuffer), INTENT(INOUT)    :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Obj%Count    = 0_kIndex
    Obj%Capacity = 0_kIndex
    IF (ALLOCATED(Obj%Buffer)) DEALLOCATE(Obj%Buffer)

    RETURN

END SUBROUTINE CharBuffer_MemFree

!******************************************************************************

FUNCTION CharBuffer_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharBuffer), INTENT(IN)   :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Obj%Count > 0_kIndex) THEN
        Str = 'CharBuffer: ' // Obj%Buffer(1:Obj%Count)
    ELSE
        Str = 'CharBuffer: Empty_Buffer'
    END IF

    RETURN

END FUNCTION CharBuffer_ToString

!******************************************************************************

FUNCTION CharBuffer_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharBuffer), INTENT(IN)   :: Obj
    tIndex                          :: Code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Code = ComputeHash(Obj%Buffer, AnyType_GetByteSize(Obj%Buffer(1:Obj%Count)))
    
    RETURN

END FUNCTION CharBuffer_HashCode

!******************************************************************************

SUBROUTINE CharBuffer_CreateEmpty(ChrBuf, InitCap)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create an empty 'CharBuffer' object with optionally specified initial capacity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharBuffer), INTENT(INOUT)    :: ChrBuf   !! 'CharBuffer' object
    tIndex, OPTIONAL,  INTENT(IN)       :: InitCap  !! initial capacity

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

! FLOW

    ! set capacity
    SET_OPTION(Capacity, DfltInitCap, InitCap)

    ! allocate buffer
    IF (.NOT.ALLOCATED(ChrBuf%Buffer)) THEN
        ALLOCATE(CHARACTER(LEN=Capacity) :: ChrBuf%Buffer)
        ChrBuf%Capacity = Capacity
    ELSEIF (Capacity > ChrBuf%Capacity) THEN
        DEALLOCATE(ChrBuf%Buffer)
        ALLOCATE(CHARACTER(LEN=Capacity) :: ChrBuf%Buffer)
        ChrBuf%Capacity = Capacity
    END IF
    
    ! reset the count
    ChrBuf%Count = 0_kIndex

    RETURN

END SUBROUTINE CharBuffer_CreateEmpty

!******************************************************************************

SUBROUTINE CharBuffer_Construct(ChrBuf, String)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a 'CharBuffer' object with the specified string.  This routine
    !  provides a convenient way to create an empty *CharBuffer* object and then
    !  add the first string into the buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharBuffer), INTENT(INOUT) :: ChrBuf  !! 'CharBuffer' object
    tCharStar,         INTENT(IN)    :: String  !! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL ChrBuf%CreateEmpty()
    CALL ChrBuf%Append(String)

    RETURN

END SUBROUTINE CharBuffer_Construct

!******************************************************************************

SUBROUTINE CharBuffer_Clear(ChrBuf)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clear the buffer string of a 'CharBuffer' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharBuffer), INTENT(INOUT)    :: ChrBuf  !! 'CharBuffer' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ChrBuf%Count = 0_kIndex

    RETURN

END SUBROUTINE CharBuffer_Clear

!******************************************************************************

SUBROUTINE CharBuffer_Finalize(ChrBuf)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of a 'CharBuffer' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(CharBuffer), INTENT(INOUT) :: ChrBuf  !! 'CharBuffer' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL ChrBuf%MemFree()

    RETURN

END SUBROUTINE CharBuffer_Finalize

!******************************************************************************

SUBROUTINE CharBuffer_EnsureCapacity(ChrBuf, StrLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make sure that a 'CharBuffer' object has enough capacity to hold
    !  a character string with the specified length.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharBuffer), INTENT(INOUT)    :: ChrBuf   !! 'CharBuffer' object
    tIndex,            INTENT(IN)       :: StrLen   !! length of character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (ChrBuf%Capacity == 0_kIndex) CALL ChrBuf%CreateEmpty()
    IF (ChrBuf%Capacity < (StrLen + ChrBuf%Count)) THEN
        BLOCK
            tIndex      :: NewCap
            tCharAlloc  :: TmpBuf
            NewCap = MAX(ChrBuf%Capacity*2_kIndex, ChrBuf%Capacity + StrLen)
            ! allocate temporary buffer
            ALLOCATE(CHARACTER(LEN=NewCap) :: TmpBuf)
            ! copy stored characters
            IF (ChrBuf%Count > 0_kIndex) TmpBuf(1:ChrBuf%Count) = ChrBuf%Buffer(1:ChrBuf%Count)
            ! move allocation from TmpBuf to Buffer
            CALL MOVE_ALLOC(TmpBuf, ChrBuf%Buffer)
            ! set capacity
            ChrBuf%Capacity = NewCap
        END BLOCK
    END IF

    RETURN

END SUBROUTINE CharBuffer_EnsureCapacity

!******************************************************************************

SUBROUTINE CharBuffer_Append(ChrBuf, String)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To append the character string at the end of the stored characters (i.e. at Count+1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharBuffer), INTENT(INOUT) :: ChrBuf  !! 'CharBuffer' object
    tCharStar,         INTENT(IN)    :: String  !! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: StrLen, EndID

! FLOW
    
    ! get string length and ensure that there is enough capacity
    StrLen = LEN(String, KIND=kIndex)
    IF (StrLen > 0_kIndex) THEN
        CALL ChrBuf%EnsureCapacity(StrLen)
        ! insert characters
        EndID = ChrBuf%Count + StrLen
        ChrBuf%Buffer(ChrBuf%Count+1:EndID) = String(1:StrLen)
        ChrBuf%Count = EndID
    END IF

    RETURN

END SUBROUTINE CharBuffer_Append

!******************************************************************************

FUNCTION CharBuffer_AsString(ChrBuf, ClearBuffer) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To return characters currently stored in the 'CharBuffer' object as
    !  an allocatable character string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharBuffer),  INTENT(INOUT)   :: ChrBuf   !! 'CharBuffer' object
    tLogical, OPTIONAL, INTENT(IN)      :: ClearBuffer
    !^ true if requesting to clear the buffer's buffer after getting the string;
    !  default is false.
    tCharAlloc                          :: cStr     !! string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: ClearBuf

! FLOW
    
    SET_OPTION(ClearBuf, FalseVal, ClearBuffer)
    IF (ChrBuf%Count > 0_kIndex) THEN
        cStr = ChrBuf%Buffer(1:ChrBuf%Count)
    ELSE
        cStr = ''
    END IF
    IF (ClearBuf) CALL ChrBuf%Clear()

    RETURN

END FUNCTION CharBuffer_AsString

!******************************************************************************

FUNCTION CharBuffer_ViewString(ChrBuf) RESULT(StrPtr)

!** PURPOSE OF THIS ROUTINE:
    !^ To return a pointer to a string representing characters currently
    !  stored in the 'CharBuffer' object.  Return null pointer if the
    !  object is currently empty.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharBuffer), TARGET, INTENT(IN)   :: ChrBuf   !! 'CharBuffer' object
    tCharLen(:),       POINTER              :: StrPtr   !! string pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (ChrBuf%Count > 0_kIndex) THEN
        StrPtr => ChrBuf%Buffer(1:ChrBuf%Count)
    ELSE
        StrPtr => NULL()
    END IF

    RETURN

END FUNCTION CharBuffer_ViewString

!******************************************************************************

FUNCTION CharBuffer_GetLength(ChrBuf) RESULT(Length)

!** PURPOSE OF THIS ROUTINE:
    !^ To return the current length of the string being built (i.e. the number
    !  of characters currently stored in the buffer).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharBuffer), INTENT(INOUT)    :: ChrBuf   !! 'CharBuffer' object
    tIndex                              :: Length   !! length of the string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = ChrBuf%Count

    RETURN

END FUNCTION CharBuffer_GetLength

!******************************************************************************

END MODULE MClass_CharBuffer
    
!******************************************************************************
