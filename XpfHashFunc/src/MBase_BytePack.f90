
MODULE MBase_BytePack

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains utility routines for conversion of bit patterns between
!   a byte array (an array of 8-bit integers) and other integer types.  The module
!   provide six procedure interfaces: BytePack, ByteUnpack, BytePackBE, ByteUnpackBE,
!   BytePackLE, and ByteUnpackLE. <br>
!   The *BytePack* procedure interface performs a conversion from a byte array to
!   an (other) integer (or an array of other integers) using machine endianess. <br>
!   The *ByteUnpack* procedure interface performs a conversion from an (other) integer
!   (or an array of other integers) to a byte array using machine endianess. <br>
!   The *BytePackBE* procedure interface performs a conversion from a byte array in
!   big-endian order to an (other) integer (or an array of other integers). <br>
!   The *ByteUnpackBE* procedure interface performs a conversion from an (other) integer
!   (or an array of other integers) to a byte array in big-endian order. <br>
!   The *BytePackLE* procedure interface performs a conversion from a byte array in
!   little-endian order to an (other) integer (or an array of other integers). <br>
!   The *ByteUnpackLE* procedure interface performs a conversion from an (other) integer
!   (or an array of other integers) to a byte array in little-endian order. <br>

!** USE STATEMENTS:
    USE ISO_C_BINDING,      ONLY: C_LOC, C_F_POINTER, C_PTR, C_NULL_PTR
    USE MBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: BytePack
    PUBLIC  :: ByteUnpack
    PUBLIC  :: BytePackBE
    PUBLIC  :: ByteUnpackBE
    PUBLIC  :: BytePackLE
    PUBLIC  :: ByteUnpackLE

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#define tByte               tUInt8
#define tShort              tUInt16
#define tInteger            tUInt32
#define tLong               tUInt64
#define MaskShort(X)        IAND(ToInt16(X), ToInt16(Z'00FF'))
#define MaskInteger(X)      IAND(ToInt32(X), ToInt32(Z'000000FF'))
#define MaskLong(X)         IAND(ToInt64(X), ToInt64(Z'00000000000000FF'))

!** MODULE PARAMETERS:
    ! size of one byte in bits
    tFloat,    PARAMETER    :: ByteBits = REAL(STORAGE_SIZE(0_kInt8), KIND=kFloat)
    ! The number of bits used by each integer type
    tInteger,  PARAMETER    :: Bits_Int8  = BIT_SIZE(0_kInt8)       ! should be  8 bits
    tInteger,  PARAMETER    :: Bits_Int16 = BIT_SIZE(0_kInt16)      ! should be 16 bits
    tInteger,  PARAMETER    :: Bits_Int32 = BIT_SIZE(0_kInt32)      ! should be 32 bits
    tInteger,  PARAMETER    :: Bits_Int64 = BIT_SIZE(0_kInt64)      ! should be 64 bits
    ! The number of bytes used by each integer type
    tInteger,  PARAMETER    :: Bytes_Int16 = Bits_Int16/Bits_Int8   ! should be 2 bytes
    tInteger,  PARAMETER    :: Bytes_Int32 = Bits_Int32/Bits_Int8   ! should be 4 bytes
    tInteger,  PARAMETER    :: Bytes_Int64 = Bits_Int64/Bits_Int8   ! should be 8 bytes

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE BytePack
        !^ **Subroutine Interface**: BytePack <br>
        !  **Purpose**:  To convert a byte array stored in machine-endian order to an
        !                (16-bit, 32-bit or 64-bit) integer (or an array of integers). <br>
        !  **Usage**: <br>
        !   ! convert a byte array to an integer <br>
        !   --->    CALL BytePack(ByteArr, IntVal) <br>
        !   ! convert a byte array starting at the specified offset to an integer <br>
        !   --->    CALL BytePack(ByteArr, Offset, IntVal) <br>
        !   ! convert a byte array to an integer array <br>
        !   --->    CALL BytePack(ByteArr, IntArr) <br>
        !   ! convert a byte array starting at the specified offset to an integer array <br>
        !   --->    CALL BytePack(ByteArr, Offset, IntArr)
        MODULE PROCEDURE PackBytes_Short
        MODULE PROCEDURE PackBytesWOffset_Short
        MODULE PROCEDURE PackBytes_Shorts
        MODULE PROCEDURE PackBytesWOffset_Shorts
        MODULE PROCEDURE PackBytes_Integer
        MODULE PROCEDURE PackBytesWOffset_Integer
        MODULE PROCEDURE PackBytes_Integers
        MODULE PROCEDURE PackBytesWOffset_Integers
        MODULE PROCEDURE PackBytes_Long
        MODULE PROCEDURE PackBytesWOffset_Long
        MODULE PROCEDURE PackBytes_Longs
        MODULE PROCEDURE PackBytesWOffset_Longs
    END INTERFACE
    INTERFACE ByteUnpack
        !^ **Subroutine Interface**: ByteUnpack <br>
        !  **Purpose**:  To convert an (16-bit, 32-bit or 64-bit) integer (or an array
        !                of integers) to a byte array stored in machine-endian order. <br>
        !  **Usage**: <br>
        !   ! convert an integer to a byte array <br>
        !   --->    CALL ByteUnpack(IntVal, ByteArr) <br>
        !   ! convert an integer to a byte array starting at the specified offset <br>
        !   --->    CALL ByteUnpack(IntVal, ByteArr, Offset) <br>
        !   ! convert an integer array to a byte array <br>
        !   --->    CALL ByteUnpack(IntArr, ByteArr) <br>
        !   ! convert an integer array to a byte array starting at the specified offset <br>
        !   --->    CALL ByteUnpack(IntArr, ByteArr, Offset)
        MODULE PROCEDURE UnpackBytes_Short
        MODULE PROCEDURE UnpackBytesWOffset_Short
        MODULE PROCEDURE UnpackBytes_Shorts
        MODULE PROCEDURE UnpackBytesWOffset_Shorts
        MODULE PROCEDURE UnpackBytes_Integer
        MODULE PROCEDURE UnpackBytesWOffset_Integer
        MODULE PROCEDURE UnpackBytes_Integers
        MODULE PROCEDURE UnpackBytesWOffset_Integers
        MODULE PROCEDURE UnpackBytes_Long
        MODULE PROCEDURE UnpackBytesWOffset_Long
        MODULE PROCEDURE UnpackBytes_Longs
        MODULE PROCEDURE UnpackBytesWOffset_Longs
    END INTERFACE
    INTERFACE BytePackBE
        !^ **Subroutine Interface**: BytePackBE <br>
        !  **Purpose**:  To convert a byte array stored in big-endian order to an
        !                (16-bit, 32-bit or 64-bit) integer (or an array of integers). <br>
        !  **Usage**: <br>
        !   ! convert a byte array to an integer <br>
        !   --->    CALL BytePackBE(ByteArr, IntVal) <br>
        !   ! convert a byte array starting at the specified offset to an integer <br>
        !   --->    CALL BytePackBE(ByteArr, Offset, IntVal) <br>
        !   ! convert a byte array to an integer array <br>
        !   --->    CALL BytePackBE(ByteArr, IntArr) <br>
        !   ! convert a byte array starting at the specified offset to an integer array <br>
        !   --->    CALL BytePackBE(ByteArr, Offset, IntArr)
        MODULE PROCEDURE PackBytesBE_Short
        MODULE PROCEDURE PackBytesBEWOffset_Short
        MODULE PROCEDURE PackBytesBE_Shorts
        MODULE PROCEDURE PackBytesBEWOffset_Shorts
        MODULE PROCEDURE PackBytesBE_Integer
        MODULE PROCEDURE PackBytesBEWOffset_Integer
        MODULE PROCEDURE PackBytesBE_Integers
        MODULE PROCEDURE PackBytesBEWOffset_Integers
        MODULE PROCEDURE PackBytesBE_Long
        MODULE PROCEDURE PackBytesBEWOffset_Long
        MODULE PROCEDURE PackBytesBE_Longs
        MODULE PROCEDURE PackBytesBEWOffset_Longs
    END INTERFACE
    INTERFACE ByteUnpackBE
        !^ **Subroutine Interface**: ByteUnpackBE <br>
        !  **Purpose**:  To convert an (16-bit, 32-bit or 64-bit) integer (or an array
        !                of integers) to a byte array stored in big-endian order. <br>
        !  **Usage**: <br>
        !   ! convert an integer to a byte array <br>
        !   --->    CALL ByteUnpackBE(IntVal, ByteArr) <br>
        !   ! convert an integer to a byte array starting at the specified offset <br>
        !   --->    CALL ByteUnpackBE(IntVal, ByteArr, Offset) <br>
        !   ! convert an integer array to a byte array <br>
        !   --->    CALL ByteUnpackBE(IntArr, ByteArr) <br>
        !   ! convert an integer array to a byte array starting at the specified offset <br>
        !   --->    CALL ByteUnpackBE(IntArr, ByteArr, Offset)
        MODULE PROCEDURE UnpackBytesBE_Short
        MODULE PROCEDURE UnpackBytesBEWOffset_Short
        MODULE PROCEDURE UnpackBytesBE_Shorts
        MODULE PROCEDURE UnpackBytesBEWOffset_Shorts
        MODULE PROCEDURE UnpackBytesBE_Integer
        MODULE PROCEDURE UnpackBytesBEWOffset_Integer
        MODULE PROCEDURE UnpackBytesBE_Integers
        MODULE PROCEDURE UnpackBytesBEWOffset_Integers
        MODULE PROCEDURE UnpackBytesBE_Long
        MODULE PROCEDURE UnpackBytesBEWOffset_Long
        MODULE PROCEDURE UnpackBytesBE_Longs
        MODULE PROCEDURE UnpackBytesBEWOffset_Longs
    END INTERFACE
    INTERFACE BytePackLE
        !^ **Subroutine Interface**: BytePackLE <br>
        !  **Purpose**:  To convert a byte array stored in little-endian order to an
        !                (16-bit, 32-bit or 64-bit) integer (or an array of integers). <br>
        !  **Usage**: <br>
        !   ! convert a byte array to an integer <br>
        !   --->    CALL BytePackLE(ByteArr, IntVal) <br>
        !   ! convert a byte array starting at the specified offset to an integer <br>
        !   --->    CALL BytePackLE(ByteArr, Offset, IntVal) <br>
        !   ! convert a byte array to an integer array <br>
        !   --->    CALL BytePackLE(ByteArr, IntArr) <br>
        !   ! convert a byte array starting at the specified offset to an integer array <br>
        !   --->    CALL BytePackLE(ByteArr, Offset, IntArr)
        MODULE PROCEDURE PackBytesLE_Short
        MODULE PROCEDURE PackBytesLEWOffset_Short
        MODULE PROCEDURE PackBytesLE_Shorts
        MODULE PROCEDURE PackBytesLEWOffset_Shorts
        MODULE PROCEDURE PackBytesLE_Integer
        MODULE PROCEDURE PackBytesLEWOffset_Integer
        MODULE PROCEDURE PackBytesLE_Integers
        MODULE PROCEDURE PackBytesLEWOffset_Integers
        MODULE PROCEDURE PackBytesLE_Long
        MODULE PROCEDURE PackBytesLEWOffset_Long
        MODULE PROCEDURE PackBytesLE_Longs
        MODULE PROCEDURE PackBytesLEWOffset_Longs
    END INTERFACE
    INTERFACE ByteUnpackLE
        !^ **Subroutine Interface**: ByteUnpackLE <br>
        !  **Purpose**:  To convert an (16-bit, 32-bit or 64-bit) integer (or an array
        !                of integers) to a byte array stored in little-endian order. <br>
        !  **Usage**: <br>
        !   ! convert an integer to a byte array <br>
        !   --->    CALL ByteUnpackLE(IntVal, ByteArr) <br>
        !   ! convert an integer to a byte array starting at the specified offset <br>
        !   --->    CALL ByteUnpackLE(IntVal, ByteArr, Offset) <br>
        !   ! convert an integer array to a byte array <br>
        !   --->    CALL ByteUnpackLE(IntArr, ByteArr) <br>
        !   ! convert an integer array to a byte array starting at the specified offset <br>
        !   --->    CALL ByteUnpackLE(IntArr, ByteArr, Offset)
        MODULE PROCEDURE UnpackBytesLE_Short
        MODULE PROCEDURE UnpackBytesLEWOffset_Short
        MODULE PROCEDURE UnpackBytesLE_Shorts
        MODULE PROCEDURE UnpackBytesLEWOffset_Shorts
        MODULE PROCEDURE UnpackBytesLE_Integer
        MODULE PROCEDURE UnpackBytesLEWOffset_Integer
        MODULE PROCEDURE UnpackBytesLE_Integers
        MODULE PROCEDURE UnpackBytesLEWOffset_Integers
        MODULE PROCEDURE UnpackBytesLE_Long
        MODULE PROCEDURE UnpackBytesLEWOffset_Long
        MODULE PROCEDURE UnpackBytesLE_Longs
        MODULE PROCEDURE UnpackBytesLEWOffset_Longs
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!-----                          Pack Procedures                           -----
!------------------------------------------------------------------------------

SUBROUTINE PackBytes_Short(ByteArr, I16)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored according to the machine endianess)
    !  to a 16-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,          INTENT(IN)  :: ByteArr(Bytes_Int16)    !! byte array
    tShort, TARGET, INTENT(OUT) :: I16                      !! 16-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to the output
    TYPE(C_PTR)     :: CPtr     ! C pointer to the output

! FLOW

    ! get a C pointer to the output
    CPtr = C_LOC(I16)
    
    ! associate a Fortran pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [Bytes_Int16])

    ! copy bit patterns
    fPtr = ByteArr
        
    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE PackBytes_Short

!******************************************************************************

SUBROUTINE PackBytesWOffset_Short(ByteArr, Offset, I16)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored according to the machine endianess)
    !  starting at the offset to a 16-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array
    tShort, INTENT(OUT) :: I16          !! 16-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePack(ByteArr(Offset:), I16)

    RETURN

END SUBROUTINE PackBytesWOffset_Short

!******************************************************************************

SUBROUTINE PackBytes_Shorts(ByteArr, I16Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored according to the machine endianess)
    !  to an array of 16-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,          INTENT(IN)  :: ByteArr(:)   !! byte array
    tShort, TARGET, INTENT(OUT) :: I16Arr(:)    !! array of 16-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to the output
    TYPE(C_PTR)     :: CPtr     ! C pointer to the output
    tInteger        :: MinBytes ! minimum byte sizes to transfer data

! FLOW
    
    ! get minimum byte sizes
    MinBytes = MIN(SIZE(ByteArr)/Bytes_Int16 , SIZE(I16Arr))*Bytes_Int16

    ! get a C pointer to the output
    CPtr = C_LOC(I16Arr)
    
    ! associate a Fortran pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [MinBytes])

    ! copy bit patterns
    fPtr = ByteArr(1:MinBytes)
        
    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE PackBytes_Shorts

!******************************************************************************

SUBROUTINE PackBytesWOffset_Shorts(ByteArr, Offset, I16Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored according to the machine endianess)
    !  starting at the offset to an array of 16-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array
    tShort, INTENT(OUT) :: I16Arr(:)    !! array of 16-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePack(ByteArr(Offset:), I16Arr)

    RETURN

END SUBROUTINE PackBytesWOffset_Shorts

!******************************************************************************

SUBROUTINE PackBytes_Integer(ByteArr, I32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored according to the machine endianess)
    !  to a 32-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,            INTENT(IN)    :: ByteArr(Bytes_Int32)  !! byte array
    tInteger, TARGET, INTENT(OUT)   :: I32                      !! 32-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to the output
    TYPE(C_PTR)     :: CPtr     ! C pointer to the output

! FLOW

    ! get a C pointer to the output
    CPtr = C_LOC(I32)
    
    ! associate a Fortran pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [Bytes_Int32])

    ! copy bit patterns
    fPtr = ByteArr
        
    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE PackBytes_Integer

!******************************************************************************

SUBROUTINE PackBytesWOffset_Integer(ByteArr, Offset, I32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored according to the machine endianess)
    !  starting at the offset to a 32-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: ByteArr(0:)  !! byte array
    tIndex,   INTENT(IN)    :: Offset       !! starting index of the byte array
    tInteger, INTENT(OUT)   :: I32          !! 32-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePack(ByteArr(Offset:), I32)

    RETURN

END SUBROUTINE PackBytesWOffset_Integer

!******************************************************************************

SUBROUTINE PackBytes_Integers(ByteArr, I32Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored according to the machine endianess)
    !  to an array of 32-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,            INTENT(IN)    :: ByteArr(:)   !! byte array
    tInteger, TARGET, INTENT(OUT)   :: I32Arr(:)    !! array of 32-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to the output
    TYPE(C_PTR)     :: CPtr     ! C pointer to the output
    tInteger        :: MinBytes ! minimum byte sizes to transfer data

! FLOW
    
    ! get minimum byte sizes
    MinBytes = MIN(SIZE(ByteArr)/Bytes_Int32 , SIZE(I32Arr))*Bytes_Int32

    ! get a C pointer to the output
    CPtr = C_LOC(I32Arr)
    
    ! associate a Fortran pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [MinBytes])

    ! copy bit patterns
    fPtr = ByteArr(1:MinBytes)
        
    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE PackBytes_Integers

!******************************************************************************

SUBROUTINE PackBytesWOffset_Integers(ByteArr, Offset, I32Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored according to the machine endianess)
    !  starting at the offset to an array of 32-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: ByteArr(0:)  !! byte array
    tIndex,   INTENT(IN)    :: Offset       !! starting index of the byte array
    tInteger, INTENT(OUT)   :: I32Arr(:)    !! array of 32-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePack(ByteArr(Offset:), I32Arr)

    RETURN

END SUBROUTINE PackBytesWOffset_Integers

!******************************************************************************

SUBROUTINE PackBytes_Long(ByteArr, I64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored according to the machine endianess)
    !  to a 64-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,         INTENT(IN)   :: ByteArr(Bytes_Int64) !! byte array
    tLong, TARGET, INTENT(OUT)  :: I64                  !! 64-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to the output
    TYPE(C_PTR)     :: CPtr     ! C pointer to the output

! FLOW

    ! get a C pointer to the output
    CPtr = C_LOC(I64)
    
    ! associate a Fortran pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [Bytes_Int64])

    ! copy bit patterns
    fPtr = ByteArr
        
    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE PackBytes_Long

!******************************************************************************

SUBROUTINE PackBytesWOffset_Long(ByteArr, Offset, I64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored according to the machine endianess)
    !  starting at the offset to a 64-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array
    tLong,  INTENT(OUT) :: I64          !! 64-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePack(ByteArr(Offset:), I64)

    RETURN

END SUBROUTINE PackBytesWOffset_Long

!******************************************************************************

SUBROUTINE PackBytes_Longs(ByteArr, I64Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored according to the machine endianess)
    !  to an array of 64-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,         INTENT(IN)   :: ByteArr(:)   !! byte array
    tLong, TARGET, INTENT(OUT)  :: I64Arr(:)    !! array of 64-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to the output
    TYPE(C_PTR)     :: CPtr     ! C pointer to the output
    tInteger        :: MinBytes ! minimum byte sizes to transfer data

! FLOW
    
    ! get minimum byte sizes
    MinBytes = MIN(SIZE(ByteArr)/Bytes_Int64 , SIZE(I64Arr))*Bytes_Int64

    ! get a C pointer to the output
    CPtr = C_LOC(I64Arr)
    
    ! associate a Fortran pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [MinBytes])

    ! copy bit patterns
    fPtr = ByteArr(1:MinBytes)
        
    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE PackBytes_Longs

!******************************************************************************

SUBROUTINE PackBytesWOffset_Longs(ByteArr, Offset, I64Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored according to the machine endianess)
    !  starting at the offset to an array of 64-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array
    tLong,  INTENT(OUT) :: I64Arr(:)    !! array of 64-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePack(ByteArr(Offset:), I64Arr)

    RETURN

END SUBROUTINE PackBytesWOffset_Longs

!------------------------------------------------------------------------------
!-----                         Unpack Procedures                          -----
!------------------------------------------------------------------------------

SUBROUTINE UnpackBytes_Short(I16, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 16-bit integer to the byte array (stored according to
    !  the machine endianess).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tShort, TARGET, INTENT(IN)   :: I16                      !! 16-bit integer
    tByte,          INTENT(OUT)  :: ByteArr(Bytes_Int16)    !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to the input
    TYPE(C_PTR)     :: CPtr     ! C pointer to the input

! FLOW

    ! get a C pointer to the input
    CPtr = C_LOC(I16)
    
    ! associate a Fortran pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [Bytes_Int16])

    ! copy bit patterns
    ByteArr = fPtr
        
    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE UnpackBytes_Short

!******************************************************************************

SUBROUTINE UnpackBytesWOffset_Short(I16, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 16-bit integer to the byte array (stored according to
    !  the machine endianess) starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tShort, INTENT(IN)  :: I16          !! 16-bit integer
    tByte,  INTENT(OUT) :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpack(I16, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesWOffset_Short

!******************************************************************************

SUBROUTINE UnpackBytes_Shorts(I16Arr, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 16-bit integers to the byte array (stored
    !  according to the machine endianess).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tShort, TARGET, INTENT(IN)  :: I16Arr(:)    !! array of 16-bit integers
    tByte,          INTENT(OUT) :: ByteArr(:)   !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to the output
    TYPE(C_PTR)     :: CPtr     ! C pointer to the output
    tInteger        :: MinBytes ! minimum byte sizes to transfer data

! FLOW
    
    ! get minimum byte sizes
    MinBytes = MIN(SIZE(ByteArr)/Bytes_Int16 , SIZE(I16Arr))*Bytes_Int16

    ! get a C pointer to the output
    CPtr = C_LOC(I16Arr)
    
    ! associate a Fortran pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [MinBytes])

    ! copy bit patterns
    ByteArr(1:MinBytes) = fPtr
        
    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE UnpackBytes_Shorts

!******************************************************************************

SUBROUTINE UnpackBytesWOffset_Shorts(I16Arr, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 16-bit integers to the byte array (stored
    !  according to the machine endianess) starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tShort, INTENT(IN)  :: I16Arr(:)    !! array of 16-bit integers
    tByte,  INTENT(OUT) :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpack(I16Arr, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesWOffset_Shorts

!******************************************************************************

SUBROUTINE UnpackBytes_Integer(I32, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 32-bit integer to the byte array (stored according to
    !  the machine endianess).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, TARGET, INTENT(IN)    :: I32                      !! 32-bit integer
    tByte,            INTENT(OUT)   :: ByteArr(Bytes_Int32)  !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to the input
    TYPE(C_PTR)     :: CPtr     ! C pointer to the input

! FLOW

    ! get a C pointer to the input
    CPtr = C_LOC(I32)
    
    ! associate a Fortran pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [Bytes_Int32])

    ! copy bit patterns
    ByteArr = fPtr
        
    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE UnpackBytes_Integer

!******************************************************************************

SUBROUTINE UnpackBytesWOffset_Integer(I32, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 32-bit integer to the byte array (stored according to
    !  the machine endianess) starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I32          !! 32-bit integer
    tByte,    INTENT(OUT)   :: ByteArr(0:)  !! byte array
    tIndex,   INTENT(IN)    :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpack(I32, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesWOffset_Integer

!******************************************************************************

SUBROUTINE UnpackBytes_Integers(I32Arr, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 32-bit integers to the byte array (stored
    !  according to the machine endianess).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, TARGET, INTENT(IN)    :: I32Arr(:)    !! array of 32-bit integers
    tByte,            INTENT(OUT)   :: ByteArr(:)   !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to the output
    TYPE(C_PTR)     :: CPtr     ! C pointer to the output
    tInteger        :: MinBytes ! minimum byte sizes to transfer data

! FLOW
    
    ! get minimum byte sizes
    MinBytes = MIN(SIZE(ByteArr)/Bytes_Int32 , SIZE(I32Arr))*Bytes_Int32

    ! get a C pointer to the output
    CPtr = C_LOC(I32Arr)
    
    ! associate a Fortran pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [MinBytes])

    ! copy bit patterns
    ByteArr(1:MinBytes) = fPtr
        
    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE UnpackBytes_Integers

!******************************************************************************

SUBROUTINE UnpackBytesWOffset_Integers(I32Arr, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 32-bit integers to the byte array (stored
    !  according to the machine endianess) starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I32Arr(:)    !! array of 32-bit integers
    tByte,    INTENT(OUT)   :: ByteArr(0:)  !! byte array
    tIndex,   INTENT(IN)    :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpack(I32Arr, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesWOffset_Integers

!******************************************************************************

SUBROUTINE UnpackBytes_Long(I64, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 64-bit integer to the byte array (stored according to
    !  the machine endianess).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, TARGET, INTENT(IN)   :: I64                  !! 64-bit integer
    tByte,         INTENT(OUT)  :: ByteArr(Bytes_Int64) !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to the input
    TYPE(C_PTR)     :: CPtr     ! C pointer to the input

! FLOW

    ! get a C pointer to the input
    CPtr = C_LOC(I64)
    
    ! associate a Fortran pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [Bytes_Int64])

    ! copy bit patterns
    ByteArr = fPtr
        
    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE UnpackBytes_Long

!******************************************************************************

SUBROUTINE UnpackBytesWOffset_Long(I64, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 64-bit integer to the byte array (stored according to
    !  the machine endianess) starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,  INTENT(IN)  :: I64          !! 64-bit integer
    tByte,  INTENT(OUT) :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpack(I64, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesWOffset_Long

!******************************************************************************

SUBROUTINE UnpackBytes_Longs(I64Arr, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 64-bit integers to the byte array (stored
    !  according to the machine endianess).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, TARGET, INTENT(IN)   :: I64Arr(:)    !! array of 64-bit integers
    tByte,         INTENT(OUT)  :: ByteArr(:)   !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: fPtr(:)  ! Fortran pointer to the output
    TYPE(C_PTR)     :: CPtr     ! C pointer to the output
    tInteger        :: MinBytes ! minimum byte sizes to transfer data

! FLOW
    
    ! get minimum byte sizes
    MinBytes = MIN(SIZE(ByteArr)/Bytes_Int64 , SIZE(I64Arr))*Bytes_Int64

    ! get a C pointer to the output
    CPtr = C_LOC(I64Arr)
    
    ! associate a Fortran pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [MinBytes])

    ! copy bit patterns
    ByteArr(1:MinBytes) = fPtr
        
    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE UnpackBytes_Longs

!******************************************************************************

SUBROUTINE UnpackBytesWOffset_Longs(I64Arr, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 64-bit integers to the byte array (stored
    !  according to the machine endianess) starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,  INTENT(IN)  :: I64Arr(:)    !! array of 64-bit integers
    tByte,  INTENT(OUT) :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpack(I64Arr, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesWOffset_Longs

!------------------------------------------------------------------------------
!-----                         BytePackBE Procedures                      -----
!------------------------------------------------------------------------------

SUBROUTINE PackBytesBE_Short(ByteArr, I16)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in big-endian order)
    !  to a 16-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(Bytes_Int16)    !! byte array
    tShort, INTENT(OUT) :: I16                      !! 16-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! implementation algorithm #1
    I16 = IOR(SHIFTL(MaskShort(ByteArr(1)), 8), MaskShort(ByteArr(2)))

    ! implementation algorithm #2 (comparable to #1)
!    I16 = SHIFTL(MaskShort(ByteArr(1)), 8) + MaskShort(ByteArr(2))

    ! implementation algorithm #3 (comparable to #1)
!#define Byte2Integer(Val, Off)  ToInt32(Val(Off))
!#define UnsignedByte(Val, Off)  IAND(Byte2Integer(Val, Off), Z'000000FF')
!#define UnsignedShort(Val, Off) IOR(UnsignedByte(Val, Off+1), SHIFTL(UnsignedByte(Val, Off), 8))
!    I16 = ToInt16(UnsignedShort(ByteArr, 1))
!#undef Byte2Integer
!#undef UnsignedByte
!#undef UnsignedShort

    ! implementation algorithm #4 (second slowest)
    ! initialize because that argument of MVBITS is INTENT(INOUT)
!    I16 = 0
!    CALL MVBITS(MaskShort(ByteArr(1)), 0, 8, I16, 8)
!    CALL MVBITS(MaskShort(ByteArr(2)), 0, 8, I16, 0)

    ! implementation algorithm #5 (slowest)
!    I16 = TRANSFER([ByteArr(2), ByteArr(1)], 0_kInt16)

    RETURN

END SUBROUTINE PackBytesBE_Short

!******************************************************************************

SUBROUTINE PackBytesBEWOffset_Short(ByteArr, Offset, I16)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in big-endian order)
    !  starting at the offset to a 16-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array
    tShort, INTENT(OUT) :: I16          !! 16-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePackBE(ByteArr(Offset:), I16)

    RETURN

END SUBROUTINE PackBytesBEWOffset_Short

!******************************************************************************

SUBROUTINE PackBytesBE_Shorts(ByteArr, I16Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in big-endian order)
    !  to an array of 16-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(:)   !! byte array
    tShort, INTENT(OUT) :: I16Arr(:)    !! array of 16-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: MinSize, Offset, I
    

! FLOW
    
    ! get minimum size
    MinSize = MIN(SIZE(ByteArr)/Bytes_Int16 , SIZE(I16Arr))

    Offset = 0
    DO I = 1, MinSize
        CALL BytePackBE(ByteArr, Offset, I16Arr(I))
        Offset = Offset + 2
    END DO

    RETURN

END SUBROUTINE PackBytesBE_Shorts

!******************************************************************************

SUBROUTINE PackBytesBEWOffset_Shorts(ByteArr, Offset, I16Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in big-endian order)
    !  starting at the offset to an array of 16-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array
    tShort, INTENT(OUT) :: I16Arr(:)    !! array of 16-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePackBE(ByteArr(Offset:), I16Arr)

    RETURN

END SUBROUTINE PackBytesBEWOffset_Shorts

!******************************************************************************

SUBROUTINE PackBytesBE_Integer(ByteArr, I32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in big-endian order)
    !  to a 32-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: ByteArr(Bytes_Int32)  !! byte array
    tInteger, INTENT(OUT)   :: I32                      !! 32-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!    tIndex      :: I

! FLOW

    ! implementation algorithm #1
    I32 = IOR(IOR(IOR(SHIFTL(MaskInteger(ByteArr(1)), 24),  &
                      SHIFTL(MaskInteger(ByteArr(2)), 16)), &
                      SHIFTL(MaskInteger(ByteArr(3)), 8)), &
                             MaskInteger(ByteArr(4)))

    ! implementation algorithm #2 (comparable to #1)
!    I32 = SHIFTL(MaskInteger(ByteArr(1)), 24) + &
!          SHIFTL(MaskInteger(ByteArr(2)), 16) + &
!          SHIFTL(MaskInteger(ByteArr(3)),  8) + &
!                 MaskInteger(ByteArr(4))

    ! implementation algorithm #3 (comparable to #1)
!#define UnsignedByte(Val, Off)  IAND(ToInt32(Val(Off)), ToInt32(Z'000000FF'))
!#define UnsignedShort(Val, Off) IOR(UnsignedByte(Val, Off+1), SHIFTL(UnsignedByte(Val, Off), 8))
!    I32 = IOR(UnsignedShort(ByteArr, 3), SHIFTL(UnsignedShort(ByteArr, 1), 16))
!#undef UnsignedByte
!#undef UnsignedShort

    ! implementation algorithm #4 (second slowest)
    ! initialize because that argument of MVBITS is INTENT(INOUT)
!    I32 = 0
!    DO I = 0, 3
!        CALL MVBITS(MaskInteger(ByteArr(1+I)), 0, 8, I32, 24-I*8)
!    END DO

    ! implementation algorithm #5 (slowest)
!    I32 = TRANSFER([ByteArr(4), ByteArr(3), ByteArr(2), ByteArr(1)], 0_kInt32)

    RETURN

END SUBROUTINE PackBytesBE_Integer

!******************************************************************************

SUBROUTINE PackBytesBEWOffset_Integer(ByteArr, Offset, I32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in big-endian order)
    !  starting at the offset to a 32-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: ByteArr(0:)  !! byte array
    tIndex,   INTENT(IN)    :: Offset       !! starting index of the byte array
    tInteger, INTENT(OUT)   :: I32          !! 32-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePackBE(ByteArr(Offset:), I32)

    RETURN

END SUBROUTINE PackBytesBEWOffset_Integer

!******************************************************************************

SUBROUTINE PackBytesBE_Integers(ByteArr, I32Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in big-endian order)
    !  to an array of 32-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: ByteArr(:)   !! byte array
    tInteger, INTENT(OUT)   :: I32Arr(:)    !! array of 32-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinSize, Offset, I

! FLOW
    
    ! get minimum sizes
    MinSize = MIN(SIZE(ByteArr)/Bytes_Int32 , SIZE(I32Arr))

    Offset = 0
    DO I = 1, MinSize
        CALL BytePackBE(ByteArr, Offset, I32Arr(I))
        Offset = Offset + 4
    END DO

    RETURN

END SUBROUTINE PackBytesBE_Integers

!******************************************************************************

SUBROUTINE PackBytesBEWOffset_Integers(ByteArr, Offset, I32Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in big-endian order)
    !  starting at the offset to an array of 32-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: ByteArr(0:)  !! byte array
    tIndex,   INTENT(IN)    :: Offset       !! starting index of the byte array
    tInteger, INTENT(OUT)   :: I32Arr(:)    !! array of 32-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePackBE(ByteArr(Offset:), I32Arr)

    RETURN

END SUBROUTINE PackBytesBEWOffset_Integers

!******************************************************************************

SUBROUTINE PackBytesBE_Long(ByteArr, I64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in big-endian order)
    !  to a 64-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: ByteArr(Bytes_Int64) !! byte array
    tLong, INTENT(OUT)  :: I64                  !! 64-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!    tIndex      :: I

! FLOW
        
    ! implementation algorithm #1 (second fastest)
!    I64 = IOR(IOR(IOR(IOR(IOR(IOR(IOR(SHIFTL(MaskLong(ByteArr(1)), 56),   &
!                                      SHIFTL(MaskLong(ByteArr(2)), 48)),  &
!                                      SHIFTL(MaskLong(ByteArr(3)), 40)),  &
!                                      SHIFTL(MaskLong(ByteArr(4)), 32)),  &
!                                      SHIFTL(MaskLong(ByteArr(5)), 24)),  &
!                                      SHIFTL(MaskLong(ByteArr(6)), 16)),  &
!                                      SHIFTL(MaskLong(ByteArr(7)),  8)),  &
!                                             MaskLong(ByteArr(8)))

    ! implementation algorithm #2 (comparable to #1)
!    I64 = SHIFTL(MaskLong(ByteArr(1)), 56) + &
!          SHIFTL(MaskLong(ByteArr(2)), 48) + &
!          SHIFTL(MaskLong(ByteArr(3)), 40) + &
!          SHIFTL(MaskLong(ByteArr(4)), 32) + &
!          SHIFTL(MaskLong(ByteArr(5)), 24) + &
!          SHIFTL(MaskLong(ByteArr(6)), 16) + &
!          SHIFTL(MaskLong(ByteArr(7)),  8) + &
!                 MaskLong(ByteArr(8))

    ! implementation algorithm #3 (fastest)
#define Byte2Integer(Val, Off)      ToInt32(Val(Off))
#define UnsignedByte(Val, Off)      IAND(Byte2Integer(Val, Off), Z'000000FF')
#define UnsignedShort(Val, Off)     IOR(UnsignedByte(Val, Off+1), SHIFTL(UnsignedByte(Val, Off), 8))
#define SignedInteger(Val, Off)     IOR(UnsignedShort(Val, Off+2), SHIFTL(UnsignedShort(Val, Off), 16))
#define UnsignedInteger(Val, Off)   IAND(ToInt64(SignedInteger(Val, Off)), Z'00000000FFFFFFFF')
    I64 = IOR(UnsignedInteger(ByteArr, 5), SHIFTL(UnsignedInteger(ByteArr, 1), 32))
#undef Byte2Integer
#undef UnsignedByte
#undef UnsignedShort
#undef SignedInteger
#undef UnsignedInteger

    ! implementation algorithm #4 (second slowest)
    ! initialize because that argument of MVBITS is INTENT(INOUT)
!    I64 = 0
!    DO I = 0, 7
!        CALL MVBITS(MaskLong(ByteArr(1+I)), 0, 8, I64, 56-I*8)
!    END DO

    ! implementation algorithm #5 (slowest)
!    I64 = TRANSFER([ByteArr(8), ByteArr(7), ByteArr(6), ByteArr(5), &
!                   ByteArr(4), ByteArr(3), ByteArr(2), ByteArr(1)], 0_kInt64)

    RETURN

END SUBROUTINE PackBytesBE_Long

!******************************************************************************

SUBROUTINE PackBytesBEWOffset_Long(ByteArr, Offset, I64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in big-endian order)
    !  starting at the offset to a 64-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array
    tLong,  INTENT(OUT) :: I64          !! 64-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePackBE(ByteArr(Offset:), I64)

    RETURN

END SUBROUTINE PackBytesBEWOffset_Long

!******************************************************************************

SUBROUTINE PackBytesBE_Longs(ByteArr, I64Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in big-endian order)
    !  to an array of 64-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: ByteArr(:)   !! byte array
    tLong, INTENT(OUT)  :: I64Arr(:)    !! array of 64-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinSize, Offset, I

! FLOW
    
    ! get minimum sizes
    MinSize = MIN(SIZE(ByteArr)/Bytes_Int64 , SIZE(I64Arr))

    Offset = 0
    DO I = 1, MinSize
        CALL BytePackBE(ByteArr, Offset, I64Arr(I))
        Offset = Offset + 8
    END DO

    RETURN

END SUBROUTINE PackBytesBE_Longs

!******************************************************************************

SUBROUTINE PackBytesBEWOffset_Longs(ByteArr, Offset, I64Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in big-endian order)
    !  starting at the offset to an array of 64-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array
    tLong,  INTENT(OUT) :: I64Arr(:)    !! array of 64-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePackBE(ByteArr(Offset:), I64Arr)

    RETURN

END SUBROUTINE PackBytesBEWOffset_Longs

!------------------------------------------------------------------------------
!-----                        ByteUnpackBE Procedures                     -----
!------------------------------------------------------------------------------

SUBROUTINE UnpackBytesBE_Short(I16, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 16-bit integer to the byte array (stored in big-endian order).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tShort, INTENT(IN)   :: I16                      !! 16-bit integer
    tByte,  INTENT(OUT)  :: ByteArr(Bytes_Int16)    !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! implementation algorithm #1
    ByteArr(1) = ToInt8(SHIFTR(I16, 8))
    ByteArr(2) = ToInt8(I16)

    ! implementation algorithm #2 (comparable to #1)
!    ByteArr(1) = ToInt8(IBITS(I16, 8, 8))
!    ByteArr(2) = ToInt8(IBITS(I16, 0, 8))
        
    ! implementation algorithm #3 (slowest)
!    ByteArr = TRANSFER(I16, ByteArr)
!    IF (IsLittleEndian) CALL SwapByteArray(ByteArr)

    RETURN

END SUBROUTINE UnpackBytesBE_Short

!******************************************************************************

SUBROUTINE UnpackBytesBEWOffset_Short(I16, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 16-bit integer to the byte array (stored in big-endian order)
    !  starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tShort, INTENT(IN)  :: I16          !! 16-bit integer
    tByte,  INTENT(OUT) :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpackBE(I16, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesBEWOffset_Short

!******************************************************************************

SUBROUTINE UnpackBytesBE_Shorts(I16Arr, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 16-bit integers to the byte array (stored
    !  in big-endian order).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tShort, INTENT(IN)  :: I16Arr(:)    !! array of 16-bit integers
    tByte,  INTENT(OUT) :: ByteArr(:)   !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinSize, Offset, I

! FLOW
    
    ! get minimum byte sizes
    MinSize = MIN(SIZE(ByteArr)/Bytes_Int16 , SIZE(I16Arr))

    Offset = 0
    DO I = 1, MinSize
        CALL ByteUnpackBE(I16Arr(I), ByteArr, Offset)
        Offset = Offset + 2
    END DO

    RETURN

END SUBROUTINE UnpackBytesBE_Shorts

!******************************************************************************

SUBROUTINE UnpackBytesBEWOffset_Shorts(I16Arr, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 16-bit integers to the byte array (stored
    !  in big-endian order) starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tShort, INTENT(IN)  :: I16Arr(:)    !! array of 16-bit integers
    tByte,  INTENT(OUT) :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpackBE(I16Arr, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesBEWOffset_Shorts

!******************************************************************************

SUBROUTINE UnpackBytesBE_Integer(I32, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 32-bit integer to the byte array (stored in big-endian order).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I32                      !! 32-bit integer
    tByte,    INTENT(OUT)   :: ByteArr(Bytes_Int32)  !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! implementation algorithm #1
    ByteArr(1) = ToInt8(SHIFTR(I32, 24))
    ByteArr(2) = ToInt8(SHIFTR(I32, 16))
    ByteArr(3) = ToInt8(SHIFTR(I32, 8))
    ByteArr(4) = ToInt8(I32)

    ! implementation algorithm #2 (comparable to #1)
!    ByteArr(1) = ToInt8(IBITS(I32, 24, 8))
!    ByteArr(2) = ToInt8(IBITS(I32, 16, 8))
!    ByteArr(3) = ToInt8(IBITS(I32,  8, 8))
!    ByteArr(4) = ToInt8(IBITS(I32,  0, 8))
        
    ! implementation algorithm #3 (slowest)
!    ByteArr = TRANSFER(I32, ByteArr)
!    IF (IsLittleEndian) CALL SwapByteArray(ByteArr)

    RETURN

END SUBROUTINE UnpackBytesBE_Integer

!******************************************************************************

SUBROUTINE UnpackBytesBEWOffset_Integer(I32, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 32-bit integer to the byte array (stored in big-endian order)
    !  starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I32          !! 32-bit integer
    tByte,    INTENT(OUT)   :: ByteArr(0:)  !! byte array
    tIndex,   INTENT(IN)    :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpackBE(I32, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesBEWOffset_Integer

!******************************************************************************

SUBROUTINE UnpackBytesBE_Integers(I32Arr, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 32-bit integers to the byte array (stored
    !  in big-endian order).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I32Arr(:)    !! array of 32-bit integers
    tByte,    INTENT(OUT)   :: ByteArr(:)   !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinSize, Offset, I

! FLOW
    
    ! get minimum byte sizes
    MinSize = MIN(SIZE(ByteArr)/Bytes_Int32 , SIZE(I32Arr))

    Offset = 0
    DO I = 1, MinSize
        CALL ByteUnpackBE(I32Arr(I), ByteArr, Offset)
        Offset = Offset + 4
    END DO

    RETURN

END SUBROUTINE UnpackBytesBE_Integers

!******************************************************************************

SUBROUTINE UnpackBytesBEWOffset_Integers(I32Arr, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 32-bit integers to the byte array (stored
    !  in big-endian order) starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I32Arr(:)    !! array of 32-bit integers
    tByte,    INTENT(OUT)   :: ByteArr(0:)  !! byte array
    tIndex,   INTENT(IN)    :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpackBE(I32Arr, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesBEWOffset_Integers

!******************************************************************************

SUBROUTINE UnpackBytesBE_Long(I64, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 64-bit integer to the byte array (stored in big-endian order).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: I64                  !! 64-bit integer
    tByte, INTENT(OUT)  :: ByteArr(Bytes_Int64) !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! implementation algorithm #1
    ByteArr(1) = ToInt8(SHIFTR(I64, 56))
    ByteArr(2) = ToInt8(SHIFTR(I64, 48))
    ByteArr(3) = ToInt8(SHIFTR(I64, 40))
    ByteArr(4) = ToInt8(SHIFTR(I64, 32))
    ByteArr(5) = ToInt8(SHIFTR(I64, 24))
    ByteArr(6) = ToInt8(SHIFTR(I64, 16))
    ByteArr(7) = ToInt8(SHIFTR(I64, 8))
    ByteArr(8) = ToInt8(I64)

    ! implementation algorithm #2 (comparable to #1)
!    ByteArr(1) = ToInt8(IBITS(I64, 56, 8))
!    ByteArr(2) = ToInt8(IBITS(I64, 48, 8))
!    ByteArr(3) = ToInt8(IBITS(I64, 40, 8))
!    ByteArr(4) = ToInt8(IBITS(I64, 32, 8))
!    ByteArr(5) = ToInt8(IBITS(I64, 24, 8))
!    ByteArr(6) = ToInt8(IBITS(I64, 16, 8))
!    ByteArr(7) = ToInt8(IBITS(I64,  8, 8))
!    ByteArr(8) = ToInt8(IBITS(I64,  0, 8))
        
    ! implementation algorithm #3 (slowest)
!    ByteArr = TRANSFER(I64, ByteArr)
!    IF (IsLittleEndian) CALL SwapByteArray(ByteArr)

    RETURN

END SUBROUTINE UnpackBytesBE_Long

!******************************************************************************

SUBROUTINE UnpackBytesBEWOffset_Long(I64, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 64-bit integer to the byte array (stored in big-endian order)
    !  starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,  INTENT(IN)  :: I64          !! 64-bit integer
    tByte,  INTENT(OUT) :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpackBE(I64, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesBEWOffset_Long

!******************************************************************************

SUBROUTINE UnpackBytesBE_Longs(I64Arr, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 64-bit integers to the byte array (stored
    !  in big-endian order).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: I64Arr(:)    !! array of 64-bit integers
    tByte, INTENT(OUT)  :: ByteArr(:)   !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinSize, Offset, I

! FLOW
    
    ! get minimum byte sizes
    MinSize = MIN(SIZE(ByteArr)/Bytes_Int64 , SIZE(I64Arr))

    Offset = 0
    DO I = 1, MinSize
        CALL ByteUnpackBE(I64Arr(I), ByteArr, Offset)
        Offset = Offset + 8
    END DO

    RETURN

END SUBROUTINE UnpackBytesBE_Longs

!******************************************************************************

SUBROUTINE UnpackBytesBEWOffset_Longs(I64Arr, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 64-bit integers to the byte array (stored
    !  in big-endian order) starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,  INTENT(IN)  :: I64Arr(:)    !! array of 64-bit integers
    tByte,  INTENT(OUT) :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpackBE(I64Arr, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesBEWOffset_Longs

!------------------------------------------------------------------------------
!-----                         BytePackLE Procedures                      -----
!------------------------------------------------------------------------------

SUBROUTINE PackBytesLE_Short(ByteArr, I16)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in little-endian order)
    !  to a 16-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(Bytes_Int16)    !! byte array
    tShort, INTENT(OUT) :: I16                      !! 16-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! implementation algorithm #1
    I16 = IOR(MaskShort(ByteArr(1)), SHIFTL(MaskShort(ByteArr(2)), 8))

    ! implementation algorithm #2 (comparable to #1)
!    I16 = MaskShort(ByteArr(1)) + SHIFTL(MaskShort(ByteArr(2)), 8)

    ! implementation algorithm #3 (comparable to #1)
!#define Byte2Integer(Val, Off)  ToInt32(Val(Off))
!#define UnsignedByte(Val, Off)  IAND(Byte2Integer(Val, Off), Z'000000FF')
!#define UnsignedShort(Val, Off) IOR(UnsignedByte(Val, Off), SHIFTL(UnsignedByte(Val, Off+1), 8))
!    I16 = ToInt16(UnsignedShort(ByteArr, 1))
!#undef Byte2Integer
!#undef UnsignedByte
!#undef UnsignedShort

    ! implementation algorithm #4 (second slowest)
    ! initialize because that argument of MVBITS is INTENT(INOUT)
!    I16 = 0
!    CALL MVBITS(MaskShort(ByteArr(1)), 0, 8, I16, 0)
!    CALL MVBITS(MaskShort(ByteArr(2)), 0, 8, I16, 8)

    ! implementation algorithm #5 (slowest)
!    I16 = TRANSFER([ByteArr(1), ByteArr(2)], 0_kInt16)

    RETURN

END SUBROUTINE PackBytesLE_Short

!******************************************************************************

SUBROUTINE PackBytesLEWOffset_Short(ByteArr, Offset, I16)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in little-endian order)
    !  starting at the offset to a 16-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array
    tShort, INTENT(OUT) :: I16          !! 16-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePackLE(ByteArr(Offset:), I16)

    RETURN

END SUBROUTINE PackBytesLEWOffset_Short

!******************************************************************************

SUBROUTINE PackBytesLE_Shorts(ByteArr, I16Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in little-endian order)
    !  to an array of 16-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(:)   !! byte array
    tShort, INTENT(OUT) :: I16Arr(:)    !! array of 16-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinSize, Offset, I

! FLOW
    
    ! get minimum size
    MinSize = MIN(SIZE(ByteArr)/Bytes_Int16 , SIZE(I16Arr))

    Offset = 0
    DO I = 1, MinSize
        CALL BytePackLE(ByteArr, Offset, I16Arr(I))
        Offset = Offset + 2
    END DO

    RETURN

END SUBROUTINE PackBytesLE_Shorts

!******************************************************************************

SUBROUTINE PackBytesLEWOffset_Shorts(ByteArr, Offset, I16Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in little-endian order)
    !  starting at the offset to an array of 16-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array
    tShort, INTENT(OUT) :: I16Arr(:)    !! array of 16-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePackLE(ByteArr(Offset:), I16Arr)

    RETURN

END SUBROUTINE PackBytesLEWOffset_Shorts

!******************************************************************************

SUBROUTINE PackBytesLE_Integer(ByteArr, I32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in little-endian order)
    !  to a 32-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: ByteArr(Bytes_Int32)  !! byte array
    tInteger, INTENT(OUT)   :: I32                      !! 32-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!    tIndex      :: I

! FLOW

    ! implementation algorithm #1
    I32 = IOR(IOR(IOR(       MaskInteger(ByteArr(1)),         &
                      SHIFTL(MaskInteger(ByteArr(2)),  8)), &
                      SHIFTL(MaskInteger(ByteArr(3)), 16)), &
                      SHIFTL(MaskInteger(ByteArr(4)), 24))

    ! implementation algorithm #2 (comparable to #1)
!    I32 =        MaskInteger(ByteArr(1))        + &
!          SHIFTL(MaskInteger(ByteArr(2)),  8) + &
!          SHIFTL(MaskInteger(ByteArr(3)), 16) + &
!          SHIFTL(MaskInteger(ByteArr(4)), 24)

    ! implementation algorithm #3 (comparable to #1)
!#define Byte2Integer(Val, Off)  ToInt32(Val(Off))
!#define UnsignedByte(Val, Off)  IAND(Byte2Integer(Val, Off), ToInt32(Z'000000FF'))
!#define UnsignedShort(Val, Off) IOR(UnsignedByte(Val, Off), SHIFTL(UnsignedByte(Val, Off+1), 8))
!       I32 = IOR(UnsignedShort(ByteArr, 1), SHIFTL(UnsignedShort(ByteArr, 3), 16))
!#undef Byte2Integer
!#undef UnsignedByte
!#undef UnsignedShort

    ! implementation algorithm #4 (second slowest)
    ! initialize because that argument of MVBITS is INTENT(INOUT)
!    I32 = 0
!    DO I = 0, 3
!        CALL MVBITS(MaskInteger(ByteArr(1+I)), 0, 8, I32, I*8)
!    END DO

    ! implementation algorithm #5 (slowest)
!    I32 = TRANSFER([ByteArr(1), ByteArr(2), ByteArr(3), ByteArr(4)], 0_kInt32)

    RETURN

END SUBROUTINE PackBytesLE_Integer

!******************************************************************************

SUBROUTINE PackBytesLEWOffset_Integer(ByteArr, Offset, I32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in little-endian order)
    !  starting at the offset to a 32-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: ByteArr(0:)  !! byte array
    tIndex,   INTENT(IN)    :: Offset       !! starting index of the byte array
    tInteger, INTENT(OUT)   :: I32          !! 32-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePackLE(ByteArr(Offset:), I32)

    RETURN

END SUBROUTINE PackBytesLEWOffset_Integer

!******************************************************************************

SUBROUTINE PackBytesLE_Integers(ByteArr, I32Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in little-endian order)
    !  to an array of 32-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: ByteArr(:)   !! byte array
    tInteger, INTENT(OUT)   :: I32Arr(:)    !! array of 32-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinSize, Offset, I

! FLOW
    
    ! get minimum byte sizes
    MinSize = MIN(SIZE(ByteArr)/Bytes_Int32 , SIZE(I32Arr))

    Offset = 0
    DO I = 1, MinSize
        CALL BytePackLE(ByteArr, Offset, I32Arr(I))
        Offset = Offset + 4
    END DO

    RETURN

END SUBROUTINE PackBytesLE_Integers

!******************************************************************************

SUBROUTINE PackBytesLEWOffset_Integers(ByteArr, Offset, I32Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in little-endian order)
    !  starting at the offset to an array of 32-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: ByteArr(0:)  !! byte array
    tIndex,   INTENT(IN)    :: Offset       !! starting index of the byte array
    tInteger, INTENT(OUT)   :: I32Arr(:)    !! array of 32-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePackLE(ByteArr(Offset:), I32Arr)

    RETURN

END SUBROUTINE PackBytesLEWOffset_Integers

!******************************************************************************

SUBROUTINE PackBytesLE_Long(ByteArr, I64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in little-endian order)
    !  to a 64-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: ByteArr(Bytes_Int64) !! byte array
    tLong, INTENT(OUT)  :: I64                  !! 64-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!    tIndex      :: I

! FLOW

    ! implementation algorithm #1 (second fastest)
!    I64 = IOR(IOR(IOR(IOR(IOR(IOR(IOR(       MaskLong(ByteArr(1)),          &
!                                      SHIFTL(MaskLong(ByteArr(2)),  8)),  &
!                                      SHIFTL(MaskLong(ByteArr(3)), 16)),  &
!                                      SHIFTL(MaskLong(ByteArr(4)), 24)),  &
!                                      SHIFTL(MaskLong(ByteArr(5)), 32)),  &
!                                      SHIFTL(MaskLong(ByteArr(6)), 40)),  &
!                                      SHIFTL(MaskLong(ByteArr(7)), 48)),  &
!                                      SHIFTL(MaskLong(ByteArr(8)), 56))

    ! implementation algorithm #2 (comparable to #1)
!    I64 =        MaskLong(ByteArr(1))      + &
!          SHIFTL(MaskLong(ByteArr(2)),  8) + &
!          SHIFTL(MaskLong(ByteArr(3)), 16) + &
!          SHIFTL(MaskLong(ByteArr(4)), 24) + &
!          SHIFTL(MaskLong(ByteArr(5)), 32) + &
!          SHIFTL(MaskLong(ByteArr(6)), 40) + &
!          SHIFTL(MaskLong(ByteArr(7)), 48) + &
!          SHIFTL(MaskLong(ByteArr(8)), 56)
        
    ! implementation algorithm #3 (fastest)
#define UnsignedByte(Val, Off)      IAND(ToInt32(Val(Off)), ToInt32(Z'000000FF'))
#define UnsignedShort(Val, Off)     IOR(UnsignedByte(Val, Off), SHIFTL(UnsignedByte(Val, Off+1), 8))
#define SignedInteger(Val, Off)     IOR(UnsignedShort(Val, Off), SHIFTL(UnsignedShort(Val, Off+2), 16))
#define UnsignedInteger(Val, Off)   IAND(ToInt64(SignedInteger(Val, Off)), ToInt64(Z'00000000FFFFFFFF'))
    I64 = IOR(UnsignedInteger(ByteArr, 1), SHIFTL(UnsignedInteger(ByteArr, 5), 32))
#undef UnsignedByte
#undef UnsignedShort
#undef SignedInteger
#undef UnsignedInteger

    ! implementation algorithm #4 (second slowest)
    ! initialize because that argument of MVBITS is INTENT(INOUT)
!    I64 = 0_kInt64
!    DO I = 0, 7
!        CALL MVBITS(MaskLong(ByteArr(I+1)), 0, 8, I64, I*8)
!    END DO

    ! implementation algorithm #5 (slowest)
!    I64 = TRANSFER([ByteArr(1), ByteArr(2), ByteArr(3), ByteArr(4), &
!                   ByteArr(5), ByteArr(6), ByteArr(7), ByteArr(8)], 0_kInt64)

    RETURN

END SUBROUTINE PackBytesLE_Long

!******************************************************************************

SUBROUTINE PackBytesLEWOffset_Long(ByteArr, Offset, I64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in little-endian order)
    !  starting at the offset to a 64-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array
    tLong,  INTENT(OUT) :: I64          !! 64-bit integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePackLE(ByteArr(Offset:), I64)

    RETURN

END SUBROUTINE PackBytesLEWOffset_Long

!******************************************************************************

SUBROUTINE PackBytesLE_Longs(ByteArr, I64Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in little-endian order)
    !  to an array of 64-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: ByteArr(:)   !! byte array
    tLong, INTENT(OUT)  :: I64Arr(:)    !! array of 64-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinSize, Offset, I

! FLOW
    
    ! get minimum byte sizes
    MinSize = MIN(SIZE(ByteArr)/Bytes_Int64 , SIZE(I64Arr))

    Offset = 0
    DO I = 1, MinSize
        CALL BytePackLE(ByteArr, Offset, I64Arr(I))
        Offset = Offset + 8
    END DO

    RETURN

END SUBROUTINE PackBytesLE_Longs

!******************************************************************************

SUBROUTINE PackBytesLEWOffset_Longs(ByteArr, Offset, I64Arr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from the byte array (stored in little-endian order)
    !  starting at the offset to an array of 64-bit integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array
    tLong,  INTENT(OUT) :: I64Arr(:)    !! array of 64-bit integers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL BytePackLE(ByteArr(Offset:), I64Arr)

    RETURN

END SUBROUTINE PackBytesLEWOffset_Longs

!------------------------------------------------------------------------------
!-----                        ByteUnpackLE Procedures                     -----
!------------------------------------------------------------------------------

SUBROUTINE UnpackBytesLE_Short(I16, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 16-bit integer to the byte array (stored in little-endian order).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tShort, INTENT(IN)   :: I16                      !! 16-bit integer
    tByte,  INTENT(OUT)  :: ByteArr(Bytes_Int16)    !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! implementation algorithm #1
    ByteArr(1) = ToInt8(I16)
    ByteArr(2) = ToInt8(SHIFTR(I16, 8))

    ! implementation algorithm #2 (comparable to #1)
!    ByteArr(1) = ToInt8(IBITS(I16, 0, 8))
!    ByteArr(2) = ToInt8(IBITS(I16, 8, 8))
        
    ! implementation algorithm #3 (slowest)
!    ByteArr = TRANSFER(I16, ByteArr)
!    IF (.NOT.IsLittleEndian) CALL SwapByteArray(ByteArr)

    RETURN

END SUBROUTINE UnpackBytesLE_Short

!******************************************************************************

SUBROUTINE UnpackBytesLEWOffset_Short(I16, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 16-bit integer to the byte array (stored in little-endian order)
    !  starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tShort, INTENT(IN)  :: I16          !! 16-bit integer
    tByte,  INTENT(OUT) :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpackLE(I16, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesLEWOffset_Short

!******************************************************************************

SUBROUTINE UnpackBytesLE_Shorts(I16Arr, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 16-bit integers to the byte array (stored
    !  in little-endian order).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tShort, INTENT(IN)  :: I16Arr(:)    !! array of 16-bit integers
    tByte,  INTENT(OUT) :: ByteArr(:)   !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinSize, Offset, I

! FLOW
    
    ! get minimum size
    MinSize = MIN(SIZE(ByteArr)/Bytes_Int16 , SIZE(I16Arr))

    Offset = 0
    DO I = 1, MinSize
        CALL ByteUnpackLE(I16Arr(I), ByteArr, Offset)
        Offset = Offset + 2
    END DO

    RETURN

END SUBROUTINE UnpackBytesLE_Shorts

!******************************************************************************

SUBROUTINE UnpackBytesLEWOffset_Shorts(I16Arr, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 16-bit integers to the byte array (stored
    !  in little-endian order) starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tShort, INTENT(IN)  :: I16Arr(:)    !! array of 16-bit integers
    tByte,  INTENT(OUT) :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpackLE(I16Arr, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesLEWOffset_Shorts

!******************************************************************************

SUBROUTINE UnpackBytesLE_Integer(I32, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 32-bit integer to the byte array (stored in little-endian order).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I32                      !! 32-bit integer
    tByte,    INTENT(OUT)   :: ByteArr(Bytes_Int32)  !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! implementation algorithm #1
    ByteArr(1) = ToInt8(I32)
    ByteArr(2) = ToInt8(SHIFTR(I32, 8))
    ByteArr(3) = ToInt8(SHIFTR(I32, 16))
    ByteArr(4) = ToInt8(SHIFTR(I32, 24))

    ! implementation algorithm #2 (comparable to #1)
!    ByteArr(1) = ToInt8(IBITS(I32,  0, 8))
!    ByteArr(2) = ToInt8(IBITS(I32,  8, 8))
!    ByteArr(3) = ToInt8(IBITS(I32, 16, 8))
!    ByteArr(4) = ToInt8(IBITS(I32, 24, 8))
        
    ! implementation algorithm #3 (slowest)
!    ByteArr = TRANSFER(I32, ByteArr)
!    IF (.NOT.IsLittleEndian) CALL SwapByteArray(ByteArr)

    RETURN

END SUBROUTINE UnpackBytesLE_Integer

!******************************************************************************

SUBROUTINE UnpackBytesLEWOffset_Integer(I32, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 32-bit integer to the byte array (stored in little-endian order)
    !  starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I32          !! 32-bit integer
    tByte,    INTENT(OUT)   :: ByteArr(0:)  !! byte array
    tIndex,   INTENT(IN)    :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpackLE(I32, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesLEWOffset_Integer

!******************************************************************************

SUBROUTINE UnpackBytesLE_Integers(I32Arr, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 32-bit integers to the byte array (stored
    !  in little-endian order).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I32Arr(:)    !! array of 32-bit integers
    tByte,    INTENT(OUT)   :: ByteArr(:)   !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinSize, Offset, I

! FLOW
    
    ! get minimum size
    MinSize = MIN(SIZE(ByteArr)/Bytes_Int32 , SIZE(I32Arr))

    Offset = 0
    DO I = 1, MinSize
        CALL ByteUnpackLE(I32Arr(I), ByteArr, Offset)
        Offset = Offset + 4
    END DO

    RETURN

END SUBROUTINE UnpackBytesLE_Integers

!******************************************************************************

SUBROUTINE UnpackBytesLEWOffset_Integers(I32Arr, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 32-bit integers to the byte array (stored
    !  in little-endian order) starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I32Arr(:)    !! array of 32-bit integers
    tByte,    INTENT(OUT)   :: ByteArr(0:)  !! byte array
    tIndex,   INTENT(IN)    :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpackLE(I32Arr, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesLEWOffset_Integers

!******************************************************************************

SUBROUTINE UnpackBytesLE_Long(I64, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 64-bit integer to the byte array (stored in little-endian order).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: I64                  !! 64-bit integer
    tByte, INTENT(OUT)  :: ByteArr(Bytes_Int64) !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! implementation algorithm #1
    ByteArr(1) = ToInt8(I64)
    ByteArr(2) = ToInt8(SHIFTR(I64, 8))
    ByteArr(3) = ToInt8(SHIFTR(I64, 16))
    ByteArr(4) = ToInt8(SHIFTR(I64, 24))
    ByteArr(5) = ToInt8(SHIFTR(I64, 32))
    ByteArr(6) = ToInt8(SHIFTR(I64, 40))
    ByteArr(7) = ToInt8(SHIFTR(I64, 48))
    ByteArr(8) = ToInt8(SHIFTR(I64, 56))

    ! implementation algorithm #2 (comparable to #1)
!    ByteArr(1) = ToInt8(IBITS(I64,  0, 8))
!    ByteArr(2) = ToInt8(IBITS(I64,  8, 8))
!    ByteArr(3) = ToInt8(IBITS(I64, 16, 8))
!    ByteArr(4) = ToInt8(IBITS(I64, 24, 8))
!    ByteArr(5) = ToInt8(IBITS(I64, 32, 8))
!    ByteArr(6) = ToInt8(IBITS(I64, 40, 8))
!    ByteArr(7) = ToInt8(IBITS(I64, 48, 8))
!    ByteArr(8) = ToInt8(IBITS(I64, 56, 8))

    ! implementation algorithm #3 (slowest)
!    ByteArr = TRANSFER(I64, ByteArr)
!    IF (.NOT.IsLittleEndian) CALL SwapByteArray(ByteArr)

    RETURN

END SUBROUTINE UnpackBytesLE_Long

!******************************************************************************

SUBROUTINE UnpackBytesLEWOffset_Long(I64, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 64-bit integer to the byte array (stored in little-endian order)
    !  starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,  INTENT(IN)  :: I64          !! 64-bit integer
    tByte,  INTENT(OUT) :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpackLE(I64, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesLEWOffset_Long

!******************************************************************************

SUBROUTINE UnpackBytesLE_Longs(I64Arr, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 64-bit integers to the byte array (stored
    !  in little-endian order).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: I64Arr(:)    !! array of 64-bit integers
    tByte, INTENT(OUT)  :: ByteArr(:)   !! byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinSize, Offset, I

! FLOW
    
    ! get minimum size
    MinSize = MIN(SIZE(ByteArr)/Bytes_Int64 , SIZE(I64Arr))

    Offset = 0
    DO I = 1, MinSize
        CALL ByteUnpackLE(I64Arr(I), ByteArr, Offset)
        Offset = Offset + 8
    END DO

    RETURN

END SUBROUTINE UnpackBytesLE_Longs

!******************************************************************************

SUBROUTINE UnpackBytesLEWOffset_Longs(I64Arr, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an array of 64-bit integers to the byte array (stored
    !  in little-endian order) starting at the offset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,  INTENT(IN)  :: I64Arr(:)    !! array of 64-bit integers
    tByte,  INTENT(OUT) :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! starting index of the byte array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL ByteUnpackLE(I64Arr, ByteArr(Offset:))

    RETURN

END SUBROUTINE UnpackBytesLEWOffset_Longs

!******************************************************************************

#undef MaskInteger
#undef MaskLong

END MODULE MBase_BytePack
    
!******************************************************************************
