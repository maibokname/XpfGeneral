
MODULE MClass_ByteBuffer

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *ByteBuffer* type and its related routines.  The *ByteBuffer* type
!   is a buffer type that provides a conveniently simple and efficient mechanism for adding
!   multiple bytes (i.e. building an array of 8-bit integers).  <br>
!   The *ByteBuffer* type provides several methods that can be grouped into the following
!   categories. <br>
!   (1) Construction and Destruction.  Methods for these operations include: <br>
!   - *CreateEmpty* method - method to construct an empty buffer, <br>
!   - *Construct* method - method to construct the buffer from its first specified bytes, and <br>
!   - *Clear* method - method to remove all inserted bytes from the buffer. <br>
!   (2) Insertion.  A method for this operation is: <br>
!   - *Append* method - method to insert bytes into the end of the buffer, <br>
!   (3) Conversion-To-Bytes.  Methods for these operations include: <br>
!   - *AsArray* method - method to return the stored bytes as an allocatable array of 8-bit integers, and <br>
!   - *ViewArray* method - method to return a pointer to the stored bytes. <br>
!   (4) Inquiry.  A method for this operation is: <br>
!   - *Size* method - method to inquire the current size of the stored bytes. <br>
!   *Note*: The *ByteBuffer* type is a subtype in the *Object* class so it also provides those deferred
!   methods required by the *Object* class. <br>

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_ByteUtil
#ifdef Indx32Bits
    USE MBase_SimpleHash32, ONLY: ComputeHash => Hash32_FNV1a
#else
    USE MBase_SimpleHash64, ONLY: ComputeHash => Hash64_FNV1a
#endif
    USE MClass_Object,      ONLY: Object

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: ByteBuffer

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'MClass_ByteBuffer'
    tIndex,    PARAMETER    :: DfltInitCap = 2048_kIndex

!** DERIVED TYPE DEFINITIONS
    !> The *ByteBuffer* type is a buffer type that provides a convenient way
    !  to build an array of 8-bit integers.
    TYPE, EXTENDS(Object)   :: ByteBuffer
        PRIVATE
        tIndex              :: Count    = 0_kIndex  !! the number of bytes currently stored
        tIndex              :: Capacity = 0_kIndex  !! the maximum number of bytes that currently can be stored
        tSInt8, ALLOCATABLE :: Buffer(:)            !! buffer
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: EnsureCapacity   => ByteBuffer_EnsureCapacity
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty *ByteBuffer* object. <br>
        !  **Usage**: <br>
        !   ! use default initial capacity  <br>
        !   --->    CALL ByteBuf%CreateEmpty() <br>
        !   ! specify initial capacity <br>
        !   --->    CALL ByteBuf%CreateEmpty(InitCap=256) <br>
        PROCEDURE   :: CreateEmpty  => ByteBuffer_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a *ByteBuffer* object with the specified bytes.  This method
        !       provides a convenient way to create an empty *ByteBuffer* object and then add the
        !       first array of bytes into the object. <br>
        !  **Usage**: <br>
        !   --->    CALL ByteBuf%Construct(ByteArr) <br>
        PROCEDURE   :: Construct    => ByteBuffer_Construct
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To clear the buffer of the *ByteBuffer* object. <br>
        !  **Usage**: <br>
        !   --->    CALL ByteBuf%Clear() <br>
        PROCEDURE   :: Clear        => ByteBuffer_Clear
        !> **Type-Bound Subroutine**: Append <br>
        !  **Purpose**:  To append the specified byte(s) at the end of the buffer. <br>
        !  **Usage**: <br>
        !   --->    CALL ByteBuf%Append(ByteArr, NElm) <br>
        PROCEDURE   :: Append       => ByteBuffer_Append
        !> **Type-Bound Function**: AsArray <br>
        !  **Purpose**:  To return bytes currently stored in the ByteBuffer as an
        !       allocatable array of bytes.  Optionally, a user can request to clear the
        !       buffer. <br>
        !  **Usage**: <br>
        !   --->    CALL ByteBuf%AsArray(ByteArr) <br>
        !   --->    CALL ByteBuf%AsArray(ByteArr, ClearBuffer=.TRUE.) <br>
        PROCEDURE   :: AsArray     => ByteBuffer_AsArray
        !> **Type-Bound Function**: ViewArray <br>
        !  **Purpose**:  To return a pointer to bytes currently stored in the *ByteBuffer* object. <br>
        !  **Usage**: <br>
        !   --->    BytePtr => ByteBuf%ViewArray() <br>
        PROCEDURE   :: ViewArray   => ByteBuffer_ViewArray
        !> **Type-Bound Function**: Size <br>
        !  **Purpose**:  To return the current size of the byte array being built. <br>
        !  **Usage**: <br>
        !   --->    Size = ByteBuf%Size() <br>
        PROCEDURE   :: Size         => ByteBuffer_GetSize
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => ByteBuffer_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => ByteBuffer_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => ByteBuffer_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => ByteBuffer_ToString
        ! ---------------------------------------------------------------------
        ! -----                     Overridden Procedure                  -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => ByteBuffer_HashCode
        ! ---------------------------------------------------------------------
        ! -----                 Final Procedure                           -----
        ! ---------------------------------------------------------------------
        !% To perform finalization of the *ByteBuffer* object.
        FINAL       :: ByteBuffer_Finalize
        ! ---------------------------------------------------------------------
    END TYPE ByteBuffer

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE ByteBuffer_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the *ByteBuffer* object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteBuffer),  INTENT(IN)  :: SrcObj   !! source object
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
    TYPE IS (ByteBuffer)
        DstObj%Count    = SrcObj%Count
        DstObj%Capacity = SrcObj%Capacity
        IF (ALLOCATED(SrcObj%Buffer)) THEN
            ALLOCATE(DstObj%Buffer, SOURCE=SrcObj%Buffer)
        END IF
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('ByteBuffer_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE ByteBuffer_Copy

!******************************************************************************

FUNCTION ByteBuffer_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteBuffer), INTENT(IN)   :: LhsObj   !! an object
    CLASS(Object),     INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (ByteBuffer)
        Flag = FalseVal
        IF (LhsObj%Count /= RhsObj%Count) RETURN
        BLOCK
            tIndex  :: I
            DO I = 1_kIndex, LhsObj%Count
                IF (LhsObj%Buffer(I) /= RhsObj%Buffer(I)) RETURN
            END DO
        END BLOCK
        Flag = TrueVal
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION ByteBuffer_IsEqualTo

!******************************************************************************

SUBROUTINE ByteBuffer_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of the *ByteBuffer* object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteBuffer), INTENT(INOUT)    :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Obj%Count    = 0_kIndex
    Obj%Capacity = 0_kIndex
    IF (ALLOCATED(Obj%Buffer)) DEALLOCATE(Obj%Buffer)

    RETURN

END SUBROUTINE ByteBuffer_MemFree

!******************************************************************************

FUNCTION ByteBuffer_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteBuffer), INTENT(IN)   :: Obj
    tCharAlloc                      :: Str  !! string representation of the object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: AllocStr

! FLOW

    IF (Obj%Count > 0_kIndex) THEN
        ! get hexadecimal string representing the stored content in the buffer
        IF (IsLittleEndian) THEN
            CALL ToHexStr_LE(Obj%Buffer(1:Obj%Count), AllocStr)
        ELSE
            CALL ToHexStr_BE(Obj%Buffer(1:Obj%Count), AllocStr)
        END IF
        Str = 'ByteBuffer: ' // AllocStr
    ELSE
        Str = 'ByteBuffer: Empty_Buffer'
    END IF

    RETURN

END FUNCTION ByteBuffer_ToString

!******************************************************************************

FUNCTION ByteBuffer_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code for this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteBuffer), INTENT(IN)   :: Obj
    tIndex                          :: Code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Code = ComputeHash(Obj%Buffer, Obj%Count)
    
    RETURN

END FUNCTION ByteBuffer_HashCode

!******************************************************************************

SUBROUTINE ByteBuffer_CreateEmpty(ByteBuf, InitCap)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create an empty 'ByteBuffer' object with optionally specified initial capacity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteBuffer), INTENT(INOUT)    :: ByteBuf  !! 'ByteBuffer' object
    tIndex, OPTIONAL,  INTENT(IN)       :: InitCap  !! initial capacity

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

! FLOW

    ! set capacity
    SET_OPTION(Capacity, DfltInitCap, InitCap)

    ! allocate buffer
    IF (.NOT.ALLOCATED(ByteBuf%Buffer)) THEN
        ALLOCATE(ByteBuf%Buffer(Capacity))
        ByteBuf%Capacity = Capacity
    ELSEIF (Capacity > ByteBuf%Capacity) THEN
        DEALLOCATE(ByteBuf%Buffer)
        ALLOCATE(ByteBuf%Buffer(Capacity))
        ByteBuf%Capacity = Capacity
    END IF
    
    ! reset the count
    ByteBuf%Count = 0_kIndex

    RETURN

END SUBROUTINE ByteBuffer_CreateEmpty

!******************************************************************************

SUBROUTINE ByteBuffer_Construct(ByteBuf, ByteArr, NElm)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a 'ByteBuffer' object with the specified bytes.  This routine
    !  provides a convenient way to create an empty *ByteBuffer* object and then
    !  add the first bytes into the buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteBuffer), INTENT(INOUT) :: ByteBuf     !! 'ByteBuffer' object
    tSInt8,            INTENT(IN)    :: ByteArr(*)  !! byte array (can be a single byte)
    tIndex,            INTENT(IN)    :: NElm        !! number of bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL ByteBuf%CreateEmpty()
    CALL ByteBuf%Append(ByteArr, NElm)

    RETURN

END SUBROUTINE ByteBuffer_Construct

!******************************************************************************

SUBROUTINE ByteBuffer_Clear(ByteBuf)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clear the buffer of a 'ByteBuffer' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteBuffer), INTENT(INOUT)    :: ByteBuf  !! 'ByteBuffer' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ByteBuf%Count = 0_kIndex

    RETURN

END SUBROUTINE ByteBuffer_Clear

!******************************************************************************

SUBROUTINE ByteBuffer_Finalize(ByteBuf)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of a 'ByteBuffer' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ByteBuffer), INTENT(INOUT) :: ByteBuf  !! 'ByteBuffer' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL ByteBuf%MemFree()

    RETURN

END SUBROUTINE ByteBuffer_Finalize

!******************************************************************************

SUBROUTINE ByteBuffer_EnsureCapacity(ByteBuf, NElm)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make sure that a 'ByteBuffer' object has enough capacity to hold
    !  an array of bytes with the specified size.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteBuffer), INTENT(INOUT)    :: ByteBuf  !! 'ByteBuffer' object
    tIndex,            INTENT(IN)       :: NElm     !! number of bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (ByteBuf%Capacity == 0_kIndex) CALL ByteBuf%CreateEmpty()
    IF (ByteBuf%Capacity < (NElm + ByteBuf%Count)) THEN
        BLOCK
            tIndex              :: NewCap
            tSInt8, ALLOCATABLE :: TmpBuf(:)
            NewCap = MAX(ByteBuf%Capacity*2_kIndex, ByteBuf%Capacity + NElm)
            ! allocate temporary buffer
            ALLOCATE(TmpBuf(NewCap))
            ! copy stored characters
            IF (ByteBuf%Count > 0_kIndex) TmpBuf(1:ByteBuf%Count) = ByteBuf%Buffer(1:ByteBuf%Count)
            ! move allocation from TmpBuf to Buffer
            CALL MOVE_ALLOC(TmpBuf, ByteBuf%Buffer)
            ! set capacity
            ByteBuf%Capacity = NewCap
        END BLOCK
    END IF

    RETURN

END SUBROUTINE ByteBuffer_EnsureCapacity

!******************************************************************************

SUBROUTINE ByteBuffer_Append(ByteBuf, ByteArr, NElm)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To append the character string at the end of the stored characters (i.e. at Count+1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteBuffer), INTENT(INOUT) :: ByteBuf     !! 'ByteBuffer' object
    tSInt8,            INTENT(IN)    :: ByteArr(*)  !! byte array (can be a single byte)
    tIndex,            INTENT(IN)    :: NElm        !! number of bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: EndID

! FLOW
    
    ! ensure that there is enough capacity
    IF (NElm > 0_kIndex) THEN
        CALL ByteBuf%EnsureCapacity(NElm)
        ! insert characters
        EndID = ByteBuf%Count + NElm
        ByteBuf%Buffer(ByteBuf%Count+1:EndID) = ByteArr(1:NElm)
        ByteBuf%Count = EndID
    END IF

    RETURN

END SUBROUTINE ByteBuffer_Append

!******************************************************************************

SUBROUTINE ByteBuffer_AsArray(ByteBuf, ByteArr, ClearBuffer)

!** PURPOSE OF THIS ROUTINE:
    !^ To return bytes currently stored in the 'ByteBuffer' object as
    !  an allocatable array of 8-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteBuffer),   INTENT(INOUT)  :: ByteBuf      !! 'ByteBuffer' object
    tSInt8, ALLOCATABLE, INTENT(OUT)    :: ByteArr(:)   !! an array of bytes
    tLogical, OPTIONAL,  INTENT(IN)     :: ClearBuffer
    !^ true if requesting to clear the buffer's buffer after getting the string;
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: ClearBuf

! FLOW
    
    SET_OPTION(ClearBuf, FalseVal, ClearBuffer)
    IF (ByteBuf%Count > 0_kIndex) THEN
        ALLOCATE(ByteArr(ByteBuf%Count))
        ByteArr(1:ByteBuf%Count) = ByteBuf%Buffer(1:ByteBuf%Count)
    END IF
    IF (ClearBuf) CALL ByteBuf%Clear()

    RETURN

END SUBROUTINE ByteBuffer_AsArray

!******************************************************************************

FUNCTION ByteBuffer_ViewArray(ByteBuf) RESULT(BytePtr)

!** PURPOSE OF THIS ROUTINE:
    !^ To return a pointer to an array representing bytes currently
    !  stored in the 'ByteBuffer' object.  Return null pointer if the
    !  object is currently empty.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteBuffer), TARGET, INTENT(IN)   :: ByteBuf      !! 'ByteBuffer' object
    tSInt8,            POINTER              :: BytePtr(:)   !! byte pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (ByteBuf%Count > 0_kIndex) THEN
        BytePtr => ByteBuf%Buffer(1:ByteBuf%Count)
    ELSE
        BytePtr => NULL()
    END IF

    RETURN

END FUNCTION ByteBuffer_ViewArray

!******************************************************************************

FUNCTION ByteBuffer_GetSize(ByteBuf) RESULT(Size)

!** PURPOSE OF THIS ROUTINE:
    !^ To return the current size of the array being built (i.e. the number
    !  of bytes currently stored in the buffer).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteBuffer), INTENT(INOUT)    :: ByteBuf  !! 'ByteBuffer' object
    tIndex                              :: Size     !! size of the array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = ByteBuf%Count

    RETURN

END FUNCTION ByteBuffer_GetSize

!******************************************************************************

END MODULE MClass_ByteBuffer
    
!******************************************************************************
