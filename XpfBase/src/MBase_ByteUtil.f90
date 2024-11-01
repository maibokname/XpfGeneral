
MODULE MBase_ByteUtil

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various byte (8-bit integer) utility routines.

!** USE STATEMENTS:
    USE ISO_C_BINDING,                  ONLY: C_LOC, C_F_POINTER, C_PTR, C_NULL_PTR, C_SIZEOF
    USE, INTRINSIC :: ISO_FORTRAN_ENV,  ONLY: CHARACTER_STORAGE_SIZE
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_CharUtil

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: ToHexStr_BE
    PUBLIC  :: ToHexStr_LE
    PUBLIC  :: HexToBytes_BE
    PUBLIC  :: HexToBytes_LE
    PUBLIC  :: AnyType_2_ByteArray
    PUBLIC  :: AnyType_2_ByteArrPtr
    PUBLIC  :: AnyType_GetByteSize
    PUBLIC  :: ByteArray_2_AnyType
    PUBLIC  :: SwapBytes
    PUBLIC  :: SwapTwoBytes
    PUBLIC  :: SwapByteArray

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tCharParam              :: ModName = 'MBase_ByteUtil'
    tCharParam              :: HexString = '0123456789ABCDEF'
    ! size of one byte in bits
    tFloat,    PARAMETER    :: ByteBits = REAL(STORAGE_SIZE(0_kInt8), KIND=kFloat)
    ! The number of bits used by each integer type
    tSInt32,   PARAMETER    :: Bits_kInt8 = BIT_SIZE(0_kInt8)           ! should be  8 bits
    tSInt32,   PARAMETER    :: Bits_kInt16 = BIT_SIZE(0_kInt16)         ! should be 16 bits
    tSInt32,   PARAMETER    :: Bits_kInt32 = BIT_SIZE(0_kInt32)         ! should be 32 bits
    tSInt32,   PARAMETER    :: Bits_kInt64 = BIT_SIZE(0_kInt64)         ! should be 64 bits
    ! The number of bytes used by each integer type
    tSInt32,   PARAMETER    :: Bytes_kInt16 = Bits_kInt16/Bits_kInt8    ! should be 2 bytes
    tSInt32,   PARAMETER    :: Bytes_kInt32 = Bits_kInt32/Bits_kInt8    ! should be 4 bytes
    tSInt32,   PARAMETER    :: Bytes_kInt64 = Bits_kInt64/Bits_kInt8    ! should be 8 bytes
    ! The numbers of bits/bytes used by a character
    tSInt32,   PARAMETER    :: Bits_Char  = CHARACTER_STORAGE_SIZE
    tSInt32,   PARAMETER    :: Bytes_Char = Bits_Char/Bits_kInt8

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE ToHexStr_BE
        !^ **Subroutine Interface**: ToHexStr_BE <br>
        !  **Purpose**:  To convert an array of 8-bit integer numbers (stored in
        !                big-endian order) to a hexadecimal string. <br>
        !  **Usage**: <br>
        !   --->    CALL ToHexStr_BE(ByteArr, HexStr)
        MODULE PROCEDURE BE_ByteArray_ToHexStr
    END INTERFACE
    INTERFACE ToHexStr_LE
        !^ **Subroutine Interface**: ToHexStr_LE <br>
        !  **Purpose**:  To convert an array of 8-bit integer numbers (stored in
        !                little-endian order) to a hexadecimal string. <br>
        !  **Usage**: <br>
        !   --->    CALL ToHexStr_LE(ByteArr, HexStr)
        MODULE PROCEDURE LE_ByteArray_ToHexStr
    END INTERFACE
    INTERFACE HexToBytes_BE
        !^ **Subroutine Interface**: HexToBytes_BE <br>
        !  **Purpose**:  To convert a hexadecimal string to an array of 8-bit
        !                integer numbers (stored in big-endian order). <br>
        !  **Usage**: <br>
        !   --->    CALL HexToBytes_BE(HexStr, ByteArr)
        MODULE PROCEDURE HexStr_2_ByteArray_BE
    END INTERFACE
    INTERFACE HexToBytes_LE
        !^ **Subroutine Interface**: HexToBytes_LE <br>
        !  **Purpose**:  To convert a hexadecimal string to an array of 8-bit
        !                integer numbers (stored in little-endian order). <br>
        !  **Usage**: <br>
        !   --->    CALL HexToBytes_LE(HexStr, ByteArr)
        MODULE PROCEDURE HexStr_2_ByteArray_LE
    END INTERFACE
    INTERFACE SwapBytes
        !^ **Function Interface**: SwapBytes <br>
        !  **Purpose**:  To perform swapping of bytes of an integer. <br>
        !  **Usage**: <br>
        !   --->    Output = SwapBytes(Input)
        MODULE PROCEDURE SwapBytes_I16
        MODULE PROCEDURE SwapBytes_I32
        MODULE PROCEDURE SwapBytes_I64
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE BE_ByteArray_ToHexStr(ByteArr, RetStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert an array of 8-bit integer numbers (stored in big-endian order)
    !  into a hexadecimal string. <br>
    !  *Note*: For a big-endian number, the most significant value is stored in
    !          the first array element and the least significant value is stored
    !          in the last array element.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt8,     INTENT(IN)  :: ByteArr(0:)  !! array of 8-bit integers
    tCharAlloc, INTENT(OUT) :: RetStr       !! returned hexadecimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J
    tSInt32     :: V, Indx1, Indx2

! FLOW
    
    ALLOCATE(CHARACTER(LEN=2*SIZE(ByteArr)) :: RetStr)
    J = 0
    DO I = 0, SIZE(ByteArr)-1
        V = ToInt32(IAND(ByteArr(I), ToInt8(Z'FF')))
        ! high order hexadecimal digit
        Indx1 = SHIFTR(V, 4) + 1
        J = J + 1
        RetStr(J:J) = HexString(Indx1:Indx1)
        ! low order hexadecimal digit
        Indx2 = IAND(V, ToInt32(Z'0000000F')) + 1
        J = J + 1
        RetStr(J:J) = HexString(Indx2:Indx2)
    END DO
        
    RETURN
        
END SUBROUTINE

!******************************************************************************

SUBROUTINE LE_ByteArray_ToHexStr(ByteArr, RetStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert an array of 8-bit integer numbers (stored in little-endian order)
    !  into a hexadecimal string. <br>
    !  *Note*: For a little-endian number, the most significant value is stored in
    !          the last array element and the least significant value is stored
    !          in the first array element.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt8,     INTENT(IN)  :: ByteArr(0:)  !! array of 8-bit integers
    tCharAlloc, INTENT(OUT) :: RetStr       !! returned hexadecimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J
    tSInt32     :: V, Indx1, Indx2

!** FLOW:

    ALLOCATE(CHARACTER(LEN=2*SIZE(ByteArr)) :: RetStr)
    J = SIZE(ByteArr)*2
    DO I = 0, SIZE(ByteArr)-1
        V = ToInt32(IAND(ByteArr(I), ToInt8(Z'FF')))
        Indx1 = IAND(V, ToInt32(Z'0000000F')) + 1
        J = J - 1
        RetStr(J:J) = HexString(Indx1:Indx1)
        Indx2 = SHIFTR(V, 4) + 1
        J = J - 1
        RetStr(J:J) = HexString(Indx2:Indx2)
    END DO

    RETURN

END SUBROUTINE

!******************************************************************************

SUBROUTINE HexStr_2_ByteArray_BE(Str, Bytes)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a hexadecimal string to an array of 8-bit integer numbers
    !  (stored in big-endian order). <br>
    !  *Note*: For a big-endian number, the most significant value is stored in
    !          the first array element and the least significant value is stored
    !          in the last array element.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,           INTENT(IN)     :: Str      !! input string
    tSInt8, ALLOCATABLE, INTENT(OUT)    :: Bytes(:) !! array of 8-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, ByteSize, Pos, StrLen
    tCharAlloc  :: CapStr

!** FLOW:

    ! verify that the input is a hexadecimal string
    CapStr = CropBlanks(Str)
    CALL ChangeCase(CapStr, TrueVal)
    Pos = VERIFY(CapStr, HexString, KIND=kIndex)
    IF (Pos /= 0) THEN
        ALLOCATE(Bytes(1))
        Bytes = 0
        CALL Handle_ErrLevel('HexStr_2_ByteArray_BE', ModName, ErrWarning, &
                             'Input string is NOT a hexadecimal string.')
        RETURN
    END IF

    ! check whether string length is an even number
    StrLen = LEN(CapStr, KIND=kIndex)
    IF (MOD(StrLen, 2_kIndex) /= 0_kIndex) THEN
        CapStr = '0' // CapStr
        StrLen = StrLen + 1
    END IF

    ! allocate output bytes
    ByteSize = SHIFTR(StrLen, 1)
    ALLOCATE(Bytes(ByteSize))

    ! convert hexadecimal string to bytes
    I   = 1
    Pos = 1
    DO WHILE (I <= ByteSize)
        Bytes(I) = GetHexVal(CapStr(Pos:Pos+1))
        I   = I + 1
        Pos = Pos + 2
    END DO

    RETURN

END SUBROUTINE

!******************************************************************************

SUBROUTINE HexStr_2_ByteArray_LE(Str, Bytes)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a hexadecimal string to an array of 8-bit integer numbers
    !  (stored in little-endian order). <br>
    !  *Note*: For a little-endian number, the most significant value is stored in
    !          the last array element and the least significant value is stored
    !          in the first array element.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,          INTENT(IN)  :: Str      !! input string
    tSInt8, ALLOCATABLE, INTENT(OUT) :: Bytes(:) !! array of 8-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, ByteSize, Pos, StrLen
    tCharAlloc  :: CapStr

!** FLOW:

    ! verify that the input is a hexadecimal string
    CapStr = CropBlanks(Str)
    CALL ChangeCase(CapStr, TrueVal)
    Pos = VERIFY(CapStr, HexString, KIND=kIndex)
    IF (Pos /= 0) THEN
        ALLOCATE(Bytes(1))
        Bytes = 0
        CALL Handle_ErrLevel('HexStr_2_ByteArray_LE', ModName, ErrWarning, &
                             'Input string is NOT a hexadecimal string.')
        RETURN
    END IF

    ! check whether string length is an even number
    StrLen = LEN(CapStr, KIND=kIndex)
    IF (MOD(StrLen, 2_kIndex) /= 0_kIndex) THEN
        CapStr = '0' // CapStr
        StrLen = StrLen + 1
    END IF

    ! allocate output bytes
    ByteSize = SHIFTR(StrLen, 1)
    ALLOCATE(Bytes(ByteSize))

    ! convert hexadecimal string to bytes
    I   = 1
    Pos = ByteSize*2
    DO WHILE (I <= ByteSize)
        Bytes(I) = GetHexVal(CapStr(Pos-1:Pos))
        I   = I + 1
        Pos = Pos - 2
    END DO

    RETURN

END SUBROUTINE

!******************************************************************************

FUNCTION GetHexVal(Str) RESULT(Val)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a hexadecimal string into an 8-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharLen(2), INTENT(IN) :: Str
    tSInt8                   :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt8      :: Num1, Num2

!** FLOW:

    Num1 = INDEX(HexString, Str(1:1), KIND=kInt8) - 1_kInt8
    Num2 = INDEX(HexString, Str(2:2), KIND=kInt8) - 1_kInt8
    Val = Num1*16 + Num2

    RETURN

END FUNCTION

!******************************************************************************

SUBROUTINE AnyType_2_ByteArray(Input, ByteSize, Output)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert the given input to an array of 8-bit integers where the input
    !  can be any type and its size (in bytes) must be known.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), TARGET, CONTIGUOUS, INTENT(IN)     :: Input(..)    !! input (can be any type and rank)
    tIndex,                      INTENT(IN)     :: ByteSize         !! size of the input (in bytes)
    tSInt8,                      INTENT(OUT)    :: Output(ByteSize) !! array of 8-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt8, POINTER :: fPtr(:)  ! Fortran pointer to data content
    TYPE(C_PTR)     :: cPtr     ! C pointer to data content

!** FLOW:

    ! get a C pointer to input
    cPtr = C_LOC(Input)
    
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [ByteSize])

    ! set output
    Output = fPtr

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE

!******************************************************************************

SUBROUTINE AnyType_2_ByteArrPtr(Input, ByteSize, Output)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert the given input to a pointer array of 8-bit integers where the input
    !  can be any type and its size (in bytes) must be known.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), TARGET, CONTIGUOUS, INTENT(IN)     :: Input(..)    !! input (can be any type and rank)
    tIndex,                      INTENT(IN)     :: ByteSize     !! size of the input (in bytes)
    tSInt8,  POINTER,            INTENT(INOUT)  :: Output(:)    !! Fortran data pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(C_PTR) :: cPtr     ! C pointer to data content

!** FLOW:

    ! get a C pointer to input
    cPtr = C_LOC(Input)
    
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, Output, [ByteSize])

    ! nullify pointers
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE

!******************************************************************************

SUBROUTINE ByteArray_2_AnyType(Input, Output)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert an array of 8-bit integers to the specified output.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt8,                      INTENT(IN)     :: Input(:)     !! array of 8-bit integers
    TYPE(*), TARGET, CONTIGUOUS, INTENT(INOUT)  :: Output(..)   !! output (can be any type)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt8, POINTER :: fPtr(:)  ! Fortran pointer to data content
    TYPE(C_PTR)     :: cPtr     ! C pointer to data content

!** FLOW:

    ! get a C pointer to input
    cPtr = C_LOC(Output)
    
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [SIZE(Input)])

    ! set output
    fPtr = Input

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE

!******************************************************************************

FUNCTION AnyType_GetByteSize(Input) RESULT(ByteSize)

!** PURPOSE OF THIS FUNCTION:
    !^ To return the size in bytes of the specified input.

!** FUNCTION ARGUMENT DEFINITIONS:
    CLASS(*), INTENT(IN)    :: Input
    tIndex                  :: ByteSize

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! get size of the specified value in bytes
    SELECT TYPE (S => Input)
    TYPE IS (tCharStar)
        BLOCK
            tChar   :: C
            ByteSize = C_SIZEOF(C)*LEN(S)
        END BLOCK
    TYPE IS (tSInt32)
        ByteSize = C_SIZEOF(S)
    TYPE IS (tSInt64)
        ByteSize = C_SIZEOF(S)
    TYPE IS (tRealSP)
        ByteSize = C_SIZEOF(S)
    TYPE IS (tRealDP)
        ByteSize = C_SIZEOF(S)
    TYPE IS (tCmpxSP)
        ByteSize = C_SIZEOF(S)
    TYPE IS (tCmpxDP)
        ByteSize = C_SIZEOF(S)
    TYPE IS (tSInt8)
        ByteSize = C_SIZEOF(S)
    TYPE IS (tSInt16)
        ByteSize = C_SIZEOF(S)
    TYPE IS (tRealQP)
        ByteSize = C_SIZEOF(S)
    TYPE IS (tCmpxQP)
        ByteSize = C_SIZEOF(S)
    TYPE IS (tLogical)
        ByteSize = C_SIZEOF(S)
    TYPE IS (LOGICAL(KIND=1))
        ByteSize = C_SIZEOF(S)
    TYPE IS (LOGICAL(KIND=2))
        ByteSize = C_SIZEOF(S)
    TYPE IS (LOGICAL(KIND=8))
        ByteSize = C_SIZEOF(S)
    CLASS DEFAULT
        ByteSize = SHIFTR(STORAGE_SIZE(S, KIND=kIndex), 3)   ! ByteSize = BitSize / 8
    END SELECT

    RETURN

END FUNCTION AnyType_GetByteSize

!******************************************************************************

ELEMENTAL FUNCTION SwapBytes_I16(Input) result(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform swapping of bytes of a 16-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt16, INTENT(IN) :: Input
    tSInt16             :: Output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! initialize because that argument of MVBITS is INTENT(INOUT)
    Output = 0
    CALL MVBITS(Input,  0, 8, Output, 8)
    CALL MVBITS(Input,  8, 8, Output, 0)

    RETURN

END FUNCTION SwapBytes_I16

!******************************************************************************

ELEMENTAL FUNCTION SwapBytes_I32(Input) result(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform swapping of bytes of a 32-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: Input
    tSInt32                 :: Output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! initialize because that argument of MVBITS is INTENT(INOUT)
    Output = 0
    CALL MVBITS(Input,  0, 8, Output, 24)
    CALL MVBITS(Input,  8, 8, Output, 16)
    CALL MVBITS(Input, 16, 8, Output, 8)
    CALL MVBITS(Input, 24, 8, Output, 0)

    RETURN

END FUNCTION SwapBytes_I32

!******************************************************************************

ELEMENTAL FUNCTION SwapBytes_I64(Input) result(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform swapping of bytes of a 64-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Input
    tSInt64             :: Output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! initialize because that argument of MVBITS is INTENT(INOUT)
    Output = 0
    CALL MVBITS(Input,  0, 8, Output, 56)
    CALL MVBITS(Input,  8, 8, Output, 48)
    CALL MVBITS(Input, 16, 8, Output, 40)
    CALL MVBITS(Input, 24, 8, Output, 32)
    CALL MVBITS(Input, 32, 8, Output, 24)
    CALL MVBITS(Input, 40, 8, Output, 16)
    CALL MVBITS(Input, 48, 8, Output,  8)
    CALL MVBITS(Input, 56, 8, Output,  0)

    RETURN

END FUNCTION SwapBytes_I64

!******************************************************************************

SUBROUTINE SwapTwoBytes(ByteArr, I, J)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To swap element I and element J of the specified byte array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt8,  INTENT(INOUT)   :: ByteArr(:)   !! byte array
    tIndex, INTENT(IN)      :: I            !! index of element I
    tIndex, INTENT(IN)      :: J            !! index of element J

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt8       :: Tmp

! FLOW
    
    Tmp = ByteArr(I)
    ByteArr(I) = ByteArr(J)
    ByteArr(J) = Tmp

    RETURN

END SUBROUTINE SwapTwoBytes

!******************************************************************************

SUBROUTINE SwapByteArray(ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To swap elements of the specified byte array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt8, INTENT(INOUT)    :: ByteArr(:)   !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (SIZE(ByteArr))
    CASE (2)
        CALL SwapTwoBytes(ByteArr, 1_kIndex, 2_kIndex)
    CASE (4)
        CALL SwapTwoBytes(ByteArr, 1_kIndex, 4_kIndex)
        CALL SwapTwoBytes(ByteArr, 2_kIndex, 3_kIndex)
    CASE (8)
        CALL SwapTwoBytes(ByteArr, 1_kIndex, 8_kIndex)
        CALL SwapTwoBytes(ByteArr, 2_kIndex, 7_kIndex)
        CALL SwapTwoBytes(ByteArr, 3_kIndex, 6_kIndex)
        CALL SwapTwoBytes(ByteArr, 4_kIndex, 5_kIndex)
    CASE DEFAULT
        CALL Handle_ErrLevel('SwapByteArray', ModName, ErrSevere, &
                             'Size of the array must be 2, 4 or 8 only.')
    END SELECT
    
    RETURN

END SUBROUTINE SwapByteArray

!******************************************************************************

FUNCTION CropBlanks(cStrIn) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To remove leading and trailing blanks from the character string

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStrIn       ! input character string
    tCharAlloc              :: cStrOut      ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

    ! FLOW:
    
    cStrOut = TRIM(ADJUSTL(cStrIn))

    RETURN

END FUNCTION CropBlanks

!******************************************************************************

SUBROUTINE ChangeCase(cStr, ToUpper)

    ! PURPOSE OF THIS ROUTINE:
    ! To change case of the given character string according to flag.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(INOUT)    :: cStr     ! character string
    tLogical,  INTENT(IN)       :: ToUpper  ! true if requesting an uppercase character
                                            ! false if requesting a lowercase character

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: I, Length

    ! FLOW:
    
    Length = LEN(cStr)
    IF (Length > 0) THEN
        IF (ToUpper) THEN
            DO I = 1, Length
                cStr(I:I) = ToUpperCase(cStr(I:I))
            END DO
        ELSE
            DO I = 1, Length
                cStr(I:I) = ToLowerCase(cStr(I:I))
            END DO
        END IF
    END IF
    
    RETURN

END SUBROUTINE ChangeCase

!******************************************************************************

END MODULE MBase_ByteUtil

!******************************************************************************
