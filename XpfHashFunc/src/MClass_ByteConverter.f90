
MODULE MClass_ByteConverter

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *ByteConverter* type and its related routines.  The
!   *ByteConverter* type performs a conversion of bit patterns between a byte
!   array (an array of 8-bit integers) and an other integer type (i.e. a 16-bit,
!   32-bit or 64-bit integer).  The *ByteConverter* type is intended to be used
!   with a general-purpose (non-cryptographic) hash function. <br>
!   It is important to note that the *ByteConverter* type MUST be initialized
!   before being used.  Otherwise, unexpected behaviors or a crash may occur.
!   The initialization provides a way to specify how the byte values are stored
!   in a byte array.  The byte values can be stored in either the big-endian
!   or the little-endian order.  If the optional flag is not specified during
!   the initialization, it assumes that the byte values are stored according to
!   machine-endian order.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: ByteConverter

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'Class_ByteConverter'

!** DERIVED TYPE DEFINITIONS
    !> The *ByteConverter* type is a converter type that performs a conversion
    !  of bit patterns between a byte (an array of 8-bit integers) array and 
    !  an other integer type (i.e. a 16-bit, 32-bit or 64-bit integer).
    TYPE    :: ByteConverter
        !> **Function Pointer**: Pack_I16 <br>
        ! **Purpose**:  To convert a byte array (starting at the specified offset)
        !               to a 16-bit integer. <br>
        !  **Usage**: <br>
        !   --->    IntVal = Pack_I16(ByteArr, Offset)
        PROCEDURE(PackShort),   POINTER, NOPASS :: Pack_I16 => NULL()
        !> **Function Pointer**: Pack_I32 <br>
        ! **Purpose**:  To convert a byte array (starting at the specified offset)
        !               to a 32-bit integer. <br>
        !  **Usage**: <br>
        !   --->    IntVal = Pack_I32(ByteArr, Offset)
        PROCEDURE(PackInt),     POINTER, NOPASS :: Pack_I32 => NULL()
        !> **Function Pointer**: Pack_I64 <br>
        ! **Purpose**:  To convert a byte array (starting at the specified offset)
        !               to a 64-bit integer. <br>
        !  **Usage**: <br>
        !   --->    IntVal = Pack_I64(ByteArr, Offset)
        PROCEDURE(PackLong),    POINTER, NOPASS :: Pack_I64 => NULL()
        !> **Subroutine Pointer**: Unpack_I16 <br>
        ! **Purpose**:  To convert a 16-bit integer to a byte array (starting at the
        !                specified offset). <br>
        !  **Usage**: <br>
        !   --->    CALL Unpack_I16(IntVal, ByteArr, Offset)
        PROCEDURE(UnpackShort), POINTER, NOPASS :: Unpack_I16 => NULL()
        !> **Subroutine Pointer**: Unpack_I32 <br>
        ! **Purpose**:  To convert a 32-bit integer to a byte array (starting at the
        !                specified offset). <br>
        !  **Usage**: <br>
        !   --->    CALL Unpack_I32(IntVal, ByteArr, Offset)
        PROCEDURE(UnpackInt),   POINTER, NOPASS :: Unpack_I32 => NULL()
        !> **Subroutine Pointer**: Unpack_I64 <br>
        ! **Purpose**:  To convert a 64-bit integer to a byte array (starting at the
        !                specified offset). <br>
        !  **Usage**: <br>
        !   --->    CALL Unpack_I64(IntVal, ByteArr, Offset)
        PROCEDURE(UnpackLong),  POINTER, NOPASS :: Unpack_I64 => NULL()
    CONTAINS
        !> **Type-Bound Subroutine**: Initialize <br>
        ! **Purpose**:  To initialize the *ByteConverter* object according to the
        !               optionally specified endianess flag. <br>
        !  **Usage**: <br>
        !   --->    CALL ByteConv%Initialize()          ! machine endian <br>
        !   --->    CALL ByteConv%Initialize(.TRUE.)    ! big endian <br>
        !   --->    CALL ByteConv%Initialize(.FALSE.)   ! little endian
        PROCEDURE   :: Initialize       => ByteConverter_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        ! **Purpose**:  To reset the *ByteConverter* object. <br>
        !  **Usage**: <br>
        !   --->    CALL ByteConv%Reset()
        PROCEDURE   :: Reset            => ByteConverter_Reset
        !> **Type-Bound Function**: Get_I8 <br>
        ! **Purpose**:  To get a single byte at the specified offset of the given byte
        !               array and return the result widened to default integer type. <br>
        !  **Usage**: <br>
        !   --->    IntVal = ByteConv%Get_I8(ByteArr, Offset)
        PROCEDURE   :: Get_I8           => Get_Byte
        !> **Type-Bound Function**: Get_U8 <br>
        ! **Purpose**:  To get a single byte at the specified offset of the given byte
        !               array and return the result widened to default integer type with
        !               the signed of the requested byte removed. <br>
        !  **Usage**: <br>
        !   --->    IntVal = ByteConv%Get_U8(ByteArr, Offset)
        PROCEDURE   :: Get_U8           => Get_Unsigned_Byte
        !> **Type-Bound Function**: Pack_U16 <br>
        ! **Purpose**:  To get two bytes starting at the specified offset of the given byte
        !               array and return the result widened to default integer type with
        !               the signed of the requested byte removed. <br>
        !  **Usage**: <br>
        !   --->    IntVal = ByteConv%Pack_U16(ByteArr, Offset)
        PROCEDURE   :: Pack_U16         => Pack_Unsigned_Short
        !> **Type-Bound Function**: Pack_U32 <br>
        ! **Purpose**:  To get four bytes starting at the specified offset of the given byte
        !               array and return the result widened to 64-bit integer type with
        !               the signed of the requested byte removed. <br>
        !  **Usage**: <br>
        !   --->    IntVal = ByteConv%Pack_U32(ByteArr, Offset)
        PROCEDURE   :: Pack_U32         => Pack_Unsigned_Integer
        !> **Type-Bound Function**: Pack_I32_Partial <br>
        ! **Purpose**:  To get three or fewer bytes of the given byte array starting at
        !               the specified offset and return the result as 32-bit integer. <br>
        !  **Usage**: <br>
        !   --->    IntVal = ByteConv%Pack_I32_Partial(ByteArr, Offset, Length) <br>
        !  **Note**: Length must be between 1 and 3.
        PROCEDURE   :: Pack_I32_Partial => Pack_Integer_Partial
        !> **Type-Bound Function**: Pack_I64_Partial <br>
        ! **Purpose**:  To get seven or fewer bytes of the given byte array starting at
        !               the specified offset and return the result as 64-bit integer. <br>
        !  **Usage**: <br>
        !   --->    IntVal = ByteConv%Pack_I64_Partial(ByteArr, Offset, Length) <br>
        !  **Note**: Length must be between 1 and 7.
        PROCEDURE   :: Pack_I64_Partial => Pack_Long_Partial
    END TYPE ByteConverter

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !---------------------------------------------------------------------------
        FUNCTION PackShort(Buf, Off) RESULT(Res)
            !^ To pack (convert) the array 'Buf' at offset 'Off' into the 16-bit word 'Res'.
            IMPORT
            tUInt8, INTENT(IN)  :: Buf(0:)  !! byte sequence
            tIndex, INTENT(IN)  :: Off      !! starting index into the byte sequence
            tUInt16             :: Res      !! result
        END FUNCTION PackShort
        !---------------------------------------------------------------------------
        FUNCTION PackInt(Buf, Off) RESULT(Res)
            !^ To pack (convert) the array 'Buf' at offset 'Off' into the 32-bit word 'Res'.
            IMPORT
            tUInt8, INTENT(IN)  :: Buf(0:)  !! byte sequence
            tIndex, INTENT(IN)  :: Off      !! starting index into the byte sequence
            tUInt32             :: Res      !! result
        END FUNCTION PackInt
        !---------------------------------------------------------------------------
        FUNCTION PackLong(Buf, Off) RESULT(Res)
            !^ To pack (convert) the array 'Buf' at offset 'Off' into the 64-bit word 'Res'.
            IMPORT
            tUInt8, INTENT(IN)  :: Buf(0:)  !! byte sequence
            tIndex, INTENT(IN)  :: Off      !! starting index into the byte sequence
            tUInt64             :: Res      !! result
        END FUNCTION PackLong
        !---------------------------------------------------------------------------
        SUBROUTINE UnpackShort(Val, Buf, Off)
            !^ To unpack (convert) the 16-bit word 'Val' into the array 'Buf' at offset 'Off'.
            IMPORT
            tUInt16, INTENT(IN)     :: Val      !! the value to convert
            tUInt8, INTENT(INOUT)   :: Buf(0:)  !! the destination buffer
            tIndex, INTENT(IN)      :: Off      !! the destination offset
        END SUBROUTINE UnpackShort
        !---------------------------------------------------------------------------
        SUBROUTINE UnpackInt(Val, Buf, Off)
            !^ To unpack (convert) the 32-bit word 'Val' into the array 'Buf' at offset 'Off'.
            IMPORT
            tUInt32,  INTENT(IN)    :: Val      !! the value to convert
            tUInt8,   INTENT(INOUT) :: Buf(0:)  !! the destination buffer
            tIndex,   INTENT(IN)    :: Off      !! the destination offset
        END SUBROUTINE UnpackInt
        !---------------------------------------------------------------------------
        SUBROUTINE UnpackLong(Val, Buf, Off)
            !^ To unpack (convert) the 64-bit word 'Val' into the array 'Buf' at offset 'Off'.
            IMPORT
            tUInt64, INTENT(IN)     :: Val      !! the value to convert
            tUInt8, INTENT(INOUT)   :: Buf(0:)  !! the destination buffer
            tIndex, INTENT(IN)      :: Off      !! the destination offset
        END SUBROUTINE UnpackLong
        !---------------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName
    tCharAlloc      :: Message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                               +
!                       INSTANTIATION PROCEDURES                                +
!                                                                               +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE ByteConverter_Initialize(BC, IsBigEndian)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the *ByteConverter* object according to the optionally
    !  specified endianess flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteConverter), INTENT(INOUT) :: BC           !! byte converter
    tLogical, OPTIONAL,   INTENT(IN)    :: IsBigEndian
    !^ the endianess flag indicating whether the byte array is stored in big-endian
    !  order or not. <br>
    !  - True if the byte array is stored in big-endian order. <br>
    !  - False if the byte array is stored in little-endian order. <br>
    !  - If not present, the byte array is stored in machine-endian order.
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tLogical    :: IsBE

!** FLOW
        
    SET_OPTION(IsBE, .NOT.IsLittleEndian, IsBigEndian)
    IF (IsBE) THEN
        BC%Pack_I16     => Pack_I16_BE
        BC%Pack_I32     => Pack_I32_BE
        BC%Pack_I64     => Pack_I64_BE
        BC%Unpack_I16   => Unpack_I16_BE
        BC%Unpack_I32   => Unpack_I32_BE
        BC%Unpack_I64   => Unpack_I64_BE
    ELSE
        BC%Pack_I16     => Pack_I16_LE
        BC%Pack_I32     => Pack_I32_LE
        BC%Pack_I64     => Pack_I64_LE
        BC%Unpack_I16   => Unpack_I16_LE
        BC%Unpack_I32   => Unpack_I32_LE
        BC%Unpack_I64   => Unpack_I64_LE
    END IF
        
    RETURN

END SUBROUTINE ByteConverter_Initialize

!******************************************************************************

SUBROUTINE ByteConverter_Reset(BC)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the *ByteConverter* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteConverter), INTENT(INOUT) :: BC   !! byte converter

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
        
    BC%Pack_I16     => NULL()
    BC%Pack_I32     => NULL()
    BC%Pack_I64     => NULL()
    BC%Unpack_I16   => NULL()
    BC%Unpack_I32   => NULL()
    BC%Unpack_I64   => NULL()
        
    RETURN

END SUBROUTINE ByteConverter_Reset

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                               +
!                           COMMON PROCEDURES                                   +
!                                                                               +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION Get_Byte(BC, Buf, Offset) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a single byte at the given 'Offset' in the specified byte sequence 'Buf'
    !  and return the result widened to default integer type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteConverter), INTENT(IN)    :: BC       !! byte converter
    tUInt8,               INTENT(IN)    :: Buf(0:)  !! byte sequence
    tIndex,               INTENT(IN)    :: Offset   !! offset to the byte to read within the byte sequence
    tUInt32                             :: OutVal   !! the requested byte, widened to default integer type

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
        
    ASSERT_MSG(IN_RANGE(Offset, 0, SIZE(Buf)-1), 'Get_Byte', &
               'Offset is not in the valid range.')

    OutVal = ToInt32(Buf(Offset))

    ASSOCIATE(Dummy => BC); END ASSOCIATE

    RETURN

END FUNCTION Get_Byte

!******************************************************************************

FUNCTION Get_Unsigned_Byte(BC, Buf, Offset) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a single byte at the given 'Offset' in the specified byte sequence 'Buf'
    !  and return the result widened to default integer type with the signed of the
    !  requested byte removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteConverter), INTENT(IN)    :: BC       !! byte converter
    tUInt8,               INTENT(IN)    :: Buf(0:)  !! byte sequence
    tIndex,               INTENT(IN)    :: Offset   !! offset to the byte to read within the byte sequence
    tUInt32                             :: OutVal   !! the requested byte, widened to default integer type
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ASSERT_MSG(IN_RANGE(Offset, 0, SIZE(Buf)-1), 'Get_Unsigned_Byte', &
               'Offset is not in the valid range.')

    OutVal = IAND(BC%Get_I8(Buf, Offset), ToInt32(Z'000000FF'))

    RETURN

END FUNCTION Get_Unsigned_Byte

!******************************************************************************

FUNCTION Pack_Unsigned_Short(BC, Buf, Offset) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get two bytes at Offset and Offset+1 positions in the specified byte sequence
    !  'Buf' and return the result widened to default integer type with the signed of the
    !  requested result removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteConverter), INTENT(IN)    :: BC       !! byte converter
    tUInt8,               INTENT(IN)    :: Buf(0:)  !! byte sequence
    tIndex,               INTENT(IN)    :: Offset   !! offset to the byte to read within the byte sequence
    tUInt32                             :: OutVal   !! the requested result, widened to default integer type
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ASSERT_MSG(IN_RANGE(Offset, 0, SIZE(Buf)-1), 'Pack_Unsigned_Short', &
               'Offset is not in the valid range.')

    OutVal = IAND(ToInt32(BC%Pack_I16(Buf, Offset)), ToInt32(Z'0000FFFF'))

    RETURN

END FUNCTION Pack_Unsigned_Short

!******************************************************************************

FUNCTION Pack_Unsigned_Integer(BC, Buf, Offset) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get four bytes starting at Offset positions in the specified byte sequence
    !  'Buf' and return the result widened to long integer type with the signed of the
    !  requested result removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteConverter), INTENT(IN)    :: BC       !! byte converter
    tUInt8,               INTENT(IN)    :: Buf(0:)  !! byte sequence
    tIndex,               INTENT(IN)    :: Offset   !! offset to the byte to read within the byte sequence
    tUInt64                             :: OutVal   !! the requested result, widened to long integer type
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ASSERT_MSG(IN_RANGE(Offset, 0, SIZE(Buf)-1), 'Pack_Unsigned_Integer', &
               'Offset is not in the valid range.')

    OutVal = IAND(ToInt64(BC%Pack_I32(Buf, Offset)), ToInt64(Z'00000000FFFFFFFF'))

    RETURN

END FUNCTION Pack_Unsigned_Integer

!******************************************************************************

FUNCTION Pack_Integer_Partial(BC, Buf, Offset, Length) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pack three or fewer bytes of the array 'Buf' starting at Offset positions
    !  in the specified byte sequence 'Buf' into the 32-bit word 'OutVal'.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteConverter), INTENT(IN)    :: BC       !! byte converter
    tUInt8,               INTENT(IN)    :: Buf(0:)  !! buffer
    tIndex,               INTENT(IN)    :: Offset   !! offset
    tIndex,               INTENT(IN)    :: Length   !! the number of bytes to pack (between 1 to 3)
    tUInt32                             :: OutVal   !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: Wrk(0:3)

! FLOW
        
    ASSERT_MSG(IN_RANGE(Offset, 0, SIZE(Buf)-1), 'Pack_Integer_Partial', &
               'Offset is not in the valid range.')
    ASSERT_MSG(IN_RANGE(Length, 1, 3), 'Pack_Integer_Partial', &
               'Length is not in the valid range.')
    ASSERT_MSG(IN_RANGE(Offset+Length, 1, SIZE(Buf)), 'Pack_Integer_Partial', &
               'Length is not in the valid range.')

    ! initialize
    Wrk = 0_kInt8
        
    ! gather available bytes
    Wrk(0:Length-1) = Buf(Offset:Offset+Length-1)
        
    ! pack bytes into word
    OutVal = BC%Pack_I32(Wrk, 0_kIndex)

    RETURN

END FUNCTION Pack_Integer_Partial

!******************************************************************************

FUNCTION Pack_Long_Partial(BC, Buf, Offset, Length) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pack seven or fewer bytes of the array 'Buf' starting at Offset positions
    !  in the specified byte sequence 'Buf' into the 64-bit word 'OutVal'.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ByteConverter), INTENT(IN)    :: BC       !! byte converter
    tUInt8,               INTENT(IN)    :: Buf(0:)  !! buffer
    tIndex,               INTENT(IN)    :: Offset   !! offset
    tIndex,               INTENT(IN)    :: Length   !! the number of bytes to pack (between 1 to 7)
    tUInt64                             :: OutVal   !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: Wrk(0:7)

! FLOW
        
    ASSERT_MSG(IN_RANGE(Offset, 0, SIZE(Buf)-1), 'Pack_Long_Partial', &
               'Offset is not in the valid range.')
    ASSERT_MSG(IN_RANGE(Length, 1, 7), 'Pack_Long_Partial', &
               'Length is not in the valid range.')
    ASSERT_MSG(IN_RANGE(Offset+Length, 1, SIZE(Buf)), 'Pack_Long_Partial', &
               'Length is not in the valid range.')
    
    ! initialize
    Wrk = 0_kInt8
        
    ! gather available bytes
    Wrk(0:Length-1) = Buf(Offset:Offset+Length-1)
        
    ! pack bytes into word
    OutVal = BC%Pack_I64(Wrk, 0_kIndex)

    RETURN

END FUNCTION Pack_Long_Partial

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                               +
!                           BIG-ENDIAN PROCEDURES                               +
!                                                                               +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#define MaskInt16(X)    IAND(ToInt16(X), ToInt16(Z'00FF'))
#define MaskInt32(X)  IAND(ToInt32(X), Z'000000FF')
#define MaskInt64(X)     IAND(ToInt64(X), Z'00000000000000FF')

PURE FUNCTION Pack_I16_BE(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pack the array 'Buf' at offset 'Off' into the 16-bit word 'Res',
    !  in big-endian convention (most significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  !! buffer
    tIndex, INTENT(IN)  :: Off      !! offset
    tUInt16             :: Res      !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! implementation algorithm #1
    Res = IOR(SHIFTL(MaskInt16(Buf(Off)), 8), MaskInt16(Buf(Off+1)))

    ! implementation algorithm #2 (comparable to #1)
!    Res = SHIFTL(MaskInt16(Buf(Off)), 8) + MaskInt16(Buf(Off+1))

    ! implementation algorithm #3 (comparable to #1)
!#define Byte2Integer(Val, Off)  ToInt32(Val(Off))
!#define UnsignedByte(Val, Off)  IAND(Byte2Integer(Val, Off), Z'000000FF')
!#define UnsignedShort(Val, Off) IOR(UnsignedByte(Val, Off+1), SHIFTL(UnsignedByte(Val, Off), 8))
!    Res = ToInt16(UnsignedShort(Buf, Off))
!#undef Byte2Integer
!#undef UnsignedByte
!#undef UnsignedShort

    ! implementation algorithm #4 (second slowest)
    ! initialize because that argument of MVBITS is INTENT(INOUT)
!    Res = 0
!    CALL MVBITS(MaskInt16(Buf(Off)), 0, 8, Res, 8)
!    CALL MVBITS(MaskInt16(Buf(Off+1)), 0, 8, Res, 0)

    ! implementation algorithm #5 (slowest)
!    Res = TRANSFER([Buf(Off+1), Buf(Off)], 0_kInt16)

    RETURN

END FUNCTION Pack_I16_BE

!******************************************************************************

PURE FUNCTION Pack_I32_BE(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pack the array 'Buf' at offset 'Off' into the 32-bit word 'Res',
    !  in big-endian convention (most significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  !! buffer
    tIndex, INTENT(IN)  :: Off      !! offset
    tUInt32             :: Res      !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!        tIndex      :: I

! FLOW
        
    ! implementation algorithm #1
    Res = IOR(IOR(IOR(SHIFTL(MaskInt32(Buf(Off)),   24),  &
                      SHIFTL(MaskInt32(Buf(Off+1)), 16)), &
                      SHIFTL(MaskInt32(Buf(Off+2)),  8)), &
                             MaskInt32(Buf(Off+3)))

    ! implementation algorithm #2 (comparable to #1)
!    Res = SHIFTL(MaskInt32(Buf(Off)),   24) + &
!          SHIFTL(MaskInt32(Buf(Off+1)), 16) + &
!          SHIFTL(MaskInt32(Buf(Off+2)),  8) + &
!                 MaskInt32(Buf(Off+3))

    ! implementation algorithm #3 (comparable to #1)
!#define Byte2Integer(Val, Off)  ToInt32(Val(Off))
!#define UnsignedByte(Val, Off)  IAND(Byte2Integer(Val, Off), Z'000000FF')
!#define UnsignedShort(Val, Off) IOR(UnsignedByte(Val, Off+1), SHIFTL(UnsignedByte(Val, Off), 8))
!    Res = IOR(UnsignedShort(Buf, Off+2), SHIFTL(UnsignedShort(Buf, Off), 16))
!#undef Byte2Integer
!#undef UnsignedByte
!#undef UnsignedShort

    ! implementation algorithm #4 (second slowest)
    ! initialize because that argument of MVBITS is INTENT(INOUT)
!    Res = 0
!    DO I = 0, 3
!        CALL MVBITS(MaskInt32(Buf(Off+I)), 0, 8, Res, 24-I*8)
!    END DO

    ! implementation algorithm #5 (slowest)
!    Res = TRANSFER([Buf(Off+3), Buf(Off+2), Buf(Off+1), Buf(Off)], 0_kInt32)

    RETURN

END FUNCTION Pack_I32_BE

!******************************************************************************

PURE FUNCTION Pack_I64_BE(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pack the array 'Buf' at offset 'Off' into the 64-bit word 'Res',
    !  in big-endian convention (most significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  !! buffer
    tIndex, INTENT(IN)  :: Off      !! offset
    tUInt64             :: Res      !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!    tIndex      :: I

! FLOW
        
    ! implementation algorithm #1 (second fastest)
!    Res = IOR(IOR(IOR(IOR(IOR(IOR(IOR(SHIFTL(MaskInt64(Buf(Off)),   56),   &
!                                      SHIFTL(MaskInt64(Buf(Off+1)), 48)),  &
!                                      SHIFTL(MaskInt64(Buf(Off+2)), 40)),  &
!                                      SHIFTL(MaskInt64(Buf(Off+3)), 32)),  &
!                                      SHIFTL(MaskInt64(Buf(Off+4)), 24)),  &
!                                      SHIFTL(MaskInt64(Buf(Off+5)), 16)),  &
!                                      SHIFTL(MaskInt64(Buf(Off+6)),  8)),  &
!                                             MaskInt64(Buf(Off+7)))

    ! implementation algorithm #2 (comparable to #1)
!    Res = SHIFTL(MaskInt64(Buf(Off)),   56) + &
!          SHIFTL(MaskInt64(Buf(Off+1)), 48) + &
!          SHIFTL(MaskInt64(Buf(Off+2)), 40) + &
!          SHIFTL(MaskInt64(Buf(Off+3)), 32) + &
!          SHIFTL(MaskInt64(Buf(Off+4)), 24) + &
!          SHIFTL(MaskInt64(Buf(Off+5)), 16) + &
!          SHIFTL(MaskInt64(Buf(Off+6)),  8) + &
!                 MaskInt64(Buf(Off+7))
        
    ! implementation algorithm #3 (fastest)
#define Byte2Integer(Val, Off)      ToInt32(Val(Off))
#define UnsignedByte(Val, Off)      IAND(Byte2Integer(Val, Off), Z'000000FF')
#define UnsignedShort(Val, Off)     IOR(UnsignedByte(Val, Off+1), SHIFTL(UnsignedByte(Val, Off), 8))
#define SignedInteger(Val, Off)     IOR(UnsignedShort(Val, Off+2), SHIFTL(UnsignedShort(Val, Off), 16))
#define UnsignedInteger(Val, Off)   IAND(ToInt64(SignedInteger(Val, Off)), Z'00000000FFFFFFFF')
    Res = IOR(UnsignedInteger(Buf, Off+4), SHIFTL(UnsignedInteger(Buf, Off), 32))
#undef Byte2Integer
#undef UnsignedByte
#undef UnsignedShort
#undef SignedInteger
#undef UnsignedInteger

    ! implementation algorithm #4 (second slowest)
    ! initialize because that argument of MVBITS is INTENT(INOUT)
!    Res = 0
!    DO I = 0, 7
!        CALL MVBITS(MaskInt64(Buf(Off+I)), 0, 8, Res, 56-I*8)
!    END DO

    ! implementation algorithm #5 (slowest)
!    Res = TRANSFER([Buf(Off+7), Buf(Off+6), Buf(Off+5), Buf(Off+4), &
!                   Buf(Off+3), Buf(Off+2), Buf(Off+1), Buf(Off)], 0_kInt64)

    RETURN

END FUNCTION Pack_I64_BE

!******************************************************************************

SUBROUTINE Unpack_I16_BE(Val, Buf, Off)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To unpack the 16-bit word 'Val' into the array 'Buf' at offset 'Off',
    !  in big-endian convention (most significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt16, INTENT(IN)     :: Val      !! the value to encode
    tUInt8, INTENT(INOUT)   :: Buf(0:)  !! the destination buffer
    tIndex, INTENT(IN)      :: Off      !! the destination offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!    tUInt8      :: Tmp

! FLOW
    
    ! implementation algorithm #1
    Buf(Off)   = ToInt8(SHIFTR(Val, 8))
    Buf(Off+1) = ToInt8(Val)

    ! implementation algorithm #2 (comparable to #1)
!    Buf(Off)   = ToInt8(IBITS(Val, 8, 8))
!    Buf(Off+1) = ToInt8(IBITS(Val, 0, 8))
        
    ! implementation algorithm #3 (slowest)
!    Buf = TRANSFER(Val, Buf)
!    IF (IsLittleEndian) THEN
!        ! swap
!        Tmp = Buf(Off)
!        Buf(Off) = Buf(Off+1)
!        Buf(Off+1) = Tmp
!    END IF

    RETURN

END SUBROUTINE Unpack_I16_BE

!******************************************************************************

SUBROUTINE Unpack_I32_BE(Val, Buf, Off)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To unpack the 32-bit word 'Val' into the array 'Buf' at offset 'Off',
    !  in big-endian convention (most significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32,  INTENT(IN)    :: Val      !! the value to encode
    tUInt8,   INTENT(INOUT) :: Buf(0:)  !! the destination buffer
    tIndex,   INTENT(IN)    :: Off      !! the destination offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!    tUInt8      :: Tmp(0:3)

! FLOW
    
    ! implementation algorithm #1
    Buf(Off)   = ToInt8(SHIFTR(Val, 24))
    Buf(Off+1) = ToInt8(SHIFTR(Val, 16))
    Buf(Off+2) = ToInt8(SHIFTR(Val, 8))
    Buf(Off+3) = ToInt8(Val)

    ! implementation algorithm #2 (comparable to #1)
!    Buf(Off)   = ToInt8(IBITS(Val, 24, 8))
!    Buf(Off+1) = ToInt8(IBITS(Val, 16, 8))
!    Buf(Off+2) = ToInt8(IBITS(Val,  8, 8))
!    Buf(Off+3) = ToInt8(IBITS(Val,  0, 8))
        
    ! implementation algorithm #3 (slowest)
!    Tmp = TRANSFER(Val, Tmp)
!    IF (IsLittleEndian) THEN
!        Buf(Off)   = Tmp(3)
!        Buf(Off+1) = Tmp(2)
!        Buf(Off+2) = Tmp(1)
!        Buf(Off+3) = Tmp(0)
!    ELSE
!        Buf(Off:) = Tmp(0:)
!    END IF

    RETURN

END SUBROUTINE Unpack_I32_BE

!******************************************************************************

SUBROUTINE Unpack_I64_BE(Val, Buf, Off)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To unpack the 64-bit word 'Val' into the array 'Buf' at offset 'Off',
    !  in big-endian convention (most significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)      :: Val      !! the value to encode
    tUInt8, INTENT(INOUT)   :: Buf(0:)  !! the destination buffer
    tIndex, INTENT(IN)      :: Off      !! the destination offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!    tUInt8      :: Tmp(0:7)

! FLOW
    
    ! implementation algorithm #1
    Buf(Off)   = ToInt8(SHIFTR(Val, 56))
    Buf(Off+1) = ToInt8(SHIFTR(Val, 48))
    Buf(Off+2) = ToInt8(SHIFTR(Val, 40))
    Buf(Off+3) = ToInt8(SHIFTR(Val, 32))
    Buf(Off+4) = ToInt8(SHIFTR(Val, 24))
    Buf(Off+5) = ToInt8(SHIFTR(Val, 16))
    Buf(Off+6) = ToInt8(SHIFTR(Val, 8))
    Buf(Off+7) = ToInt8(Val)

    ! implementation algorithm #2 (comparable to #1)
!    Buf(Off)   = ToInt8(IBITS(Val, 56, 8))
!    Buf(Off+1) = ToInt8(IBITS(Val, 48, 8))
!    Buf(Off+2) = ToInt8(IBITS(Val, 40, 8))
!    Buf(Off+3) = ToInt8(IBITS(Val, 32, 8))
!    Buf(Off+4) = ToInt8(IBITS(Val, 24, 8))
!    Buf(Off+5) = ToInt8(IBITS(Val, 16, 8))
!    Buf(Off+6) = ToInt8(IBITS(Val,  8, 8))
!    Buf(Off+7) = ToInt8(IBITS(Val,  0, 8))
        
    ! implementation algorithm #3 (slowest)
!    Tmp = TRANSFER(Val, Tmp)
!    IF (IsLittleEndian) THEN
!        Buf(Off)   = Tmp(7)
!        Buf(Off+1) = Tmp(6)
!        Buf(Off+2) = Tmp(5)
!        Buf(Off+3) = Tmp(4)
!        Buf(Off+4) = Tmp(3)
!        Buf(Off+5) = Tmp(2)
!        Buf(Off+6) = Tmp(1)
!        Buf(Off+7) = Tmp(0)
!    ELSE
!        Buf(Off:) = Tmp(0:)
!    END IF

    RETURN

END SUBROUTINE Unpack_I64_BE

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                               +
!                          LITTLE-ENDIAN PROCEDURES                             +
!                                                                               +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION Pack_I16_LE(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pack the array 'Buf' at offset 'Off' into the 16-bit word 'Res',
    !  in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  !! buffer
    tIndex, INTENT(IN)  :: Off      !! offset
    tUInt16             :: Res      !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! implementation algorithm #1
    Res = IOR(MaskInt16(Buf(Off)), SHIFTL(MaskInt16(Buf(Off+1)), 8))

    ! implementation algorithm #2 (comparable to #1)
!    Res = MaskInt16(Buf(Off)) + SHIFTL(MaskInt16(Buf(Off+1)), 8)

    ! implementation algorithm #3 (comparable to #1)
!#define Byte2Integer(Val, Off)  ToInt32(Val(Off))
!#define UnsignedByte(Val, Off)  IAND(Byte2Integer(Val, Off), Z'000000FF')
!#define UnsignedShort(Val, Off) IOR(UnsignedByte(Val, Off), SHIFTL(UnsignedByte(Val, Off+1), 8))
!    Res = ToInt16(UnsignedShort(Buf, Off))
!#undef Byte2Integer
!#undef UnsignedByte
!#undef UnsignedShort

    ! implementation algorithm #4 (second slowest)
    ! initialize because that argument of MVBITS is INTENT(INOUT)
!    Res = 0
!    CALL MVBITS(MaskInt16(Buf(Off)), 0, 8, Res, 0)
!    CALL MVBITS(MaskInt16(Buf(Off+1)), 0, 8, Res, 8)

    ! implementation algorithm #5 (slowest)
!    Res = TRANSFER([Buf(Off), Buf(Off+1)], 0_kInt16)

    RETURN

END FUNCTION Pack_I16_LE

!******************************************************************************

PURE FUNCTION Pack_I32_LE(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pack the array 'Buf' at offset 'Off' into the 32-bit word 'Res',
    !  in little-endian convention (least significant byte first).
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  !! buffer
    tIndex, INTENT(IN)  :: Off      !! offset
    tUInt32             :: Res      !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!    tIndex      :: I

! FLOW
        
    ! implementation algorithm #1
    Res = IOR(IOR(IOR(       MaskInt32(Buf(Off)),         &
                        SHIFTL(MaskInt32(Buf(Off+1)),  8)), &
                        SHIFTL(MaskInt32(Buf(Off+2)), 16)), &
                        SHIFTL(MaskInt32(Buf(Off+3)), 24))

    ! implementation algorithm #2 (comparable to #1)
!    Res =        MaskInt32(Buf(Off))        + &
!          SHIFTL(MaskInt32(Buf(Off+1)),  8) + &
!          SHIFTL(MaskInt32(Buf(Off+2)), 16) + &
!          SHIFTL(MaskInt32(Buf(Off+3)), 24)

    ! implementation algorithm #3 (comparable to #1)
!#define Byte2Integer(Val, Off)  ToInt32(Val(Off))
!#define UnsignedByte(Val, Off)  IAND(Byte2Integer(Val, Off), Z'000000FF')
!#define UnsignedShort(Val, Off) IOR(UnsignedByte(Val, Off), SHIFTL(UnsignedByte(Val, Off+1), 8))
!    Res = IOR(UnsignedShort(Buf, Off), SHIFTL(UnsignedShort(Buf, Off+2), 16))
!#undef Byte2Integer
!#undef UnsignedByte
!#undef UnsignedShort

    ! implementation algorithm #4 (second slowest)
    ! initialize because that argument of MVBITS is INTENT(INOUT)
!    Res = 0
!    DO I = 0, 3
!        CALL MVBITS(MaskInt32(Buf(Off+I)), 0, 8, Res, I*8)
!    END DO

    ! implementation algorithm #5 (slowest)
!    Res = TRANSFER([Buf(Off), Buf(Off+1), Buf(Off+2), Buf(Off+3)], 0_kInt32)

    RETURN

END FUNCTION Pack_I32_LE

!******************************************************************************

FUNCTION Pack_I64_LE(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pack the array 'Buf' at offset 'Off' into the 64-bit word 'Res',
    !  in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8,              INTENT(IN) :: Buf(0:)  !! buffer
    tIndex,              INTENT(IN) :: Off      !! offset
    tUInt64                         :: Res      !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!    tIndex      :: I

! FLOW
        
    ! implementation algorithm #1 (second fastest)
!    Res = IOR(IOR(IOR(IOR(IOR(IOR(IOR(       MaskInt64(Buf(Off)),          &
!                                      SHIFTL(MaskInt64(Buf(Off+1)),  8)),  &
!                                      SHIFTL(MaskInt64(Buf(Off+2)), 16)),  &
!                                      SHIFTL(MaskInt64(Buf(Off+3)), 24)),  &
!                                      SHIFTL(MaskInt64(Buf(Off+4)), 32)),  &
!                                      SHIFTL(MaskInt64(Buf(Off+5)), 40)),  &
!                                      SHIFTL(MaskInt64(Buf(Off+6)), 48)),  &
!                                      SHIFTL(MaskInt64(Buf(Off+7)), 56))

    ! implementation algorithm #2 (comparable to #1)
!    Res =        MaskInt64(Buf(Off))        + &
!          SHIFTL(MaskInt64(Buf(Off+1)),  8) + &
!          SHIFTL(MaskInt64(Buf(Off+2)), 16) + &
!          SHIFTL(MaskInt64(Buf(Off+3)), 24) + &
!          SHIFTL(MaskInt64(Buf(Off+4)), 32) + &
!          SHIFTL(MaskInt64(Buf(Off+5)), 40) + &
!          SHIFTL(MaskInt64(Buf(Off+6)), 48) + &
!          SHIFTL(MaskInt64(Buf(Off+7)), 56)
        
    ! implementation algorithm #3 (fastest)
#define Byte2Integer(Val, Off)      ToInt32(Val(Off))
#define UnsignedByte(Val, Off)      IAND(Byte2Integer(Val, Off), Z'000000FF')
#define UnsignedShort(Val, Off)     IOR(UnsignedByte(Val, Off), SHIFTL(UnsignedByte(Val, Off+1), 8))
#define SignedInteger(Val, Off)     IOR(UnsignedShort(Val, Off), SHIFTL(UnsignedShort(Val, Off+2), 16))
#define UnsignedInteger(Val, Off)   IAND(ToInt64(SignedInteger(Val, Off)), Z'00000000FFFFFFFF')
    Res = IOR(UnsignedInteger(Buf, Off), SHIFTL(UnsignedInteger(Buf, Off+4), 32))
#undef Byte2Integer
#undef UnsignedByte
#undef UnsignedShort
#undef SignedInteger
#undef UnsignedInteger

    ! implementation algorithm #4 (second slowest)
    ! initialize because that argument of MVBITS is INTENT(INOUT)
!    Res = 0
!    DO I = 0, 7
!        CALL MVBITS(MaskInt64(Buf(Off+I)), 0, 8, Res, I*8)
!    END DO

    ! implementation algorithm #5 (slowest)
!    Res = TRANSFER([Buf(Off), Buf(Off+1), Buf(Off+2), Buf(Off+3), &
!                   Buf(Off+4), Buf(Off+5), Buf(Off+6), Buf(Off+7)], 0_kInt64)

    RETURN

END FUNCTION Pack_I64_LE

!******************************************************************************

SUBROUTINE Unpack_I16_LE(Val, Buf, Off)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To unpack the 16-bit word 'Val' into the array 'Buf' at offset 'Off',
    !  in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt16, INTENT(IN)     :: Val      !! the value to encode
    tUInt8, INTENT(INOUT)   :: Buf(0:)  !! the destination buffer
    tIndex, INTENT(IN)      :: Off      !! the destination offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! implementation algorithm #1
    Buf(Off)   = ToInt8(Val)
    Buf(Off+1) = ToInt8(SHIFTR(Val, 8))

    ! implementation algorithm #2 (comparable to #1)
!    Buf(Off)   = ToInt8(IBITS(Val,  0, 8))
!    Buf(Off+1) = ToInt8(IBITS(Val,  8, 8))
        
    ! implementation algorithm #3 (slowest)
!    Buf(Off:Off+1) = TRANSFER(Val, Buf)

    RETURN

END SUBROUTINE Unpack_I16_LE

!******************************************************************************

SUBROUTINE Unpack_I32_LE(Val, Buf, Off)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To unpack the 32-bit word 'Val' into the array 'Buf' at offset 'Off',
    !  in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32,  INTENT(IN)    :: Val      !! the value to encode
    tUInt8,   INTENT(INOUT) :: Buf(0:)  !! the destination buffer
    tIndex,   INTENT(IN)    :: Off      !! the destination offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! implementation algorithm #1
    Buf(Off)   = ToInt8(Val)
    Buf(Off+1) = ToInt8(SHIFTR(Val, 8))
    Buf(Off+2) = ToInt8(SHIFTR(Val, 16))
    Buf(Off+3) = ToInt8(SHIFTR(Val, 24))

    ! implementation algorithm #2 (comparable to #1)
!    Buf(Off)   = ToInt8(IBITS(Val,  0, 8))
!    Buf(Off+1) = ToInt8(IBITS(Val,  8, 8))
!    Buf(Off+2) = ToInt8(IBITS(Val, 16, 8))
!    Buf(Off+3) = ToInt8(IBITS(Val, 24, 8))
        
    ! implementation algorithm #3 (slowest)
!    Buf(Off:Off+3) = TRANSFER(Val, Buf)

    RETURN

END SUBROUTINE Unpack_I32_LE

!******************************************************************************

SUBROUTINE Unpack_I64_LE(Val, Buf, Off)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To unpack the 64-bit word 'Val' into the array 'Buf' at offset 'Off',
    !  in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)      :: Val      !! the value to encode
    tUInt8, INTENT(INOUT)   :: Buf(0:)  !! the destination buffer
    tIndex, INTENT(IN)      :: Off      !! the destination offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! implementation algorithm #1
    Buf(Off)   = ToInt8(Val)
    Buf(Off+1) = ToInt8(SHIFTR(Val, 8))
    Buf(Off+2) = ToInt8(SHIFTR(Val, 16))
    Buf(Off+3) = ToInt8(SHIFTR(Val, 24))
    Buf(Off+4) = ToInt8(SHIFTR(Val, 32))
    Buf(Off+5) = ToInt8(SHIFTR(Val, 40))
    Buf(Off+6) = ToInt8(SHIFTR(Val, 48))
    Buf(Off+7) = ToInt8(SHIFTR(Val, 56))

    ! implementation algorithm #2 (comparable to #1)
!    Buf(Off)   = ToInt8(IBITS(Val,  0, 8))
!    Buf(Off+1) = ToInt8(IBITS(Val,  8, 8))
!    Buf(Off+2) = ToInt8(IBITS(Val, 16, 8))
!    Buf(Off+3) = ToInt8(IBITS(Val, 24, 8))
!    Buf(Off+4) = ToInt8(IBITS(Val, 32, 8))
!    Buf(Off+5) = ToInt8(IBITS(Val, 40, 8))
!    Buf(Off+6) = ToInt8(IBITS(Val, 48, 8))
!    Buf(Off+7) = ToInt8(IBITS(Val, 56, 8))
        
    ! implementation algorithm #3 (slowest)
!    Buf(Off:Off+7) = TRANSFER(Val, Buf)

    RETURN

END SUBROUTINE Unpack_I64_LE

!******************************************************************************

#undef MaskInt16
#undef MaskInt32
#undef MaskInt64

END MODULE MClass_ByteConverter
    
!******************************************************************************
