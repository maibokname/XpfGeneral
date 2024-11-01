
MODULE MBase_ExperimentalHash32

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains an experimental implementation of various non-cryptographic
!   hash function routines that output a hash value as a 32-bit integer.  The available
!   hash functions are a subset of those reference hash functions implemented in the
!   <a href="../module/mbase_referencehash32.html">ModBase_ReferenceHash32</a> module.
!   The API of these experimental routines are the same as those reference routines with
!   the exception of an additional argument (*Algo*). <br>
!   The *Algo* argument is an algorithm flag used to indicate which algorithm is employed
!   to implement a *Pack_I32* procedure, which perform a conversion from an array of four
!   8-bit integers to a 32-bit integer.  There are a number of possible implementations
!   of the *Pack_I32* procedure.  In this module, seven basic implementations are provided.
!   A user can perform a benchmark of each specific hash function routine in order to know
!   which one of the *Pack_I32* algorithms is the best one for a particular system (i.e.
!   a combination of operating system, machine as well as compiler used).  The benchmark
!   can then be used to implement an optimal implementation of the hash function. <br>
!   <br>
!^ **REFERENCES**: <br>
!   See the <a href="../module/mbase_referencehash32.html">ModBase_ReferenceHash32</a>
!   module for references of the available hash functions in this module.

!** USE STATEMENTS:
    USE ISO_C_BINDING,      ONLY: C_LOC, C_F_POINTER, C_PTR, C_NULL_PTR
    USE MBase_Common
    USE MBase_ByteUtil,   ONLY: SwapBytes

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! experimental procedures
    PUBLIC :: City_Hash32_Exp
    PUBLIC :: FarmMk_Hash32_Exp
    PUBLIC :: Lookup3_Hash32_Exp
    PUBLIC :: Murmur3_Hash32_Exp
    PUBLIC :: NM_Hash32_Exp
    PUBLIC :: NMx_Hash32_Exp
    PUBLIC :: Water_Hash32_Exp
    PUBLIC :: XX_Hash32_Exp
    ! auxiliary procedures
    PUBLIC :: Pack_I16
    PUBLIC :: Pack_I16_A1
    PUBLIC :: Pack_I16_A2
    PUBLIC :: Pack_I16_A3
    PUBLIC :: Pack_I16_A4
    PUBLIC :: Pack_I16_A5
    PUBLIC :: Pack_I16_A6
    PUBLIC :: Pack_I16_A7
    PUBLIC :: Pack_I32
    PUBLIC :: Pack_I32_A1
    PUBLIC :: Pack_I32_A2
    PUBLIC :: Pack_I32_A3
    PUBLIC :: Pack_I32_A4
    PUBLIC :: Pack_I32_A5
    PUBLIC :: Pack_I32_A6
    PUBLIC :: Pack_I32_A7

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define MaskI32(X)      IAND(ToInt32(X), ToInt32(Z'000000FF'))
#define MaskI16(X)      IAND(ToInt16(X), ToInt16(Z'00FF'))

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'ModBase_ExperimentalHash32'
    ! The maximum (positive) number of hash code
    tUInt32,   PARAMETER    :: MaxHash = ToInt32(Z'7FFFFFFF')

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Pack_I16(ByteArr, Offset) RESULT(Res)
            !^ To convert an array of 8-bit integers starting at the offset to
            !  a 16-bit integer value.
            IMPORT
            tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
            tIndex,         INTENT(IN)  :: Offset       !! offset
            tUInt16                     :: Res          !! result
        END FUNCTION Pack_I16
        FUNCTION Pack_I32(ByteArr, Offset) RESULT(Res)
            !^ To convert an array of 8-bit integers starting at the offset to
            !  a 32-bit integer value.
            IMPORT
            tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
            tIndex,         INTENT(IN)  :: Offset       !! offset
            tUInt32                     :: Res          !! result
        END FUNCTION Pack_I32
    END INTERFACE
    INTERFACE
        !----------------------------------------------------------------------
        MODULE FUNCTION Lookup3_Hash32_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the Lookup3 hash algorithm by Bob Jenkins [2].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION Murmur3_Hash32_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the MurmurHash3 hash algorithm by Austin
            !  Appleby [1].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION City_Hash32_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the City hash algorithm by Google Inc [3].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-9)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION FarmMk_Hash32_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the FarmMk hash algorithm by Google Inc [4].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION NM_Hash32_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using version 2 of the NMHASH hash algorithm
            !  by James Z. M. Gao [7].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION NMx_Hash32_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using version 2 of the NMxHASH hash algorithm by
            !  James Z. M. Gao [7].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION XX_Hash32_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the XXHash hash algorithm by Yann Collet [8].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION Water_Hash32_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using a new version of the WaterHash hash
            !  algorithm by Tommy Ettinger [11].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION Pack_I32_A1(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 32-bit integer value using the *SHIFTL* and *IOR* intrinsic
    !  functions.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt32                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! little-endian order
    Res = IOR(IOR(IOR(       MaskI32(ByteArr(Offset)),         &
                      SHIFTL(MaskI32(ByteArr(Offset+1)),  8)), &
                      SHIFTL(MaskI32(ByteArr(Offset+2)), 16)), &
                      SHIFTL(MaskI32(ByteArr(Offset+3)), 24))

    ! big-endian order
!    Res = IOR(IOR(IOR(SHIFTL(MaskI32(ByteArr(Offset)),   24),  &
!                      SHIFTL(MaskI32(ByteArr(Offset+1)), 16)), &
!                      SHIFTL(MaskI32(ByteArr(Offset+2)),  8)), &
!                             MaskI32(ByteArr(Offset+3)))

    RETURN

END FUNCTION Pack_I32_A1

!**************************************************************************

FUNCTION Pack_I32_A2(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 32-bit integer value using the *SHIFTL* intrinsic function
    !  and an addition operator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt32                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! little-endian order
    Res =        MaskI32(ByteArr(Offset))        + &
          SHIFTL(MaskI32(ByteArr(Offset+1)),  8) + &
          SHIFTL(MaskI32(ByteArr(Offset+2)), 16) + &
          SHIFTL(MaskI32(ByteArr(Offset+3)), 24)

    ! big-endian order
!    Res = SHIFTL(MaskI32(ByteArr(Offset)),   24) + &
!          SHIFTL(MaskI32(ByteArr(Offset+1)), 16) + &
!          SHIFTL(MaskI32(ByteArr(Offset+2)),  8) + &
!                 MaskI32(ByteArr(Offset+3))

    RETURN

END FUNCTION Pack_I32_A2

!**************************************************************************

FUNCTION Pack_I32_A3(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 32-bit integer value using the *SHIFTL* and *IOR* intrinsic
    !  functions. <br>
    !  *Note*: Although this routine and the *Pack_I32_A3* routine employ
    !  the same intrinsic functions, orders of the functions being used
    !  are somewhat different.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt32                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! little-endian order
#define UnsignedShort(Val, Off)     IOR(MaskI32(Val(Off)), SHIFTL(MaskI32(Val(Off+1)), 8))
    Res = IOR(UnsignedShort(ByteArr, Offset), SHIFTL(UnsignedShort(ByteArr, Offset+2), 16))
#undef UnsignedShort

    ! big-endian order
!#define UnsignedByte(Val, Off)  IAND(ToInt32(Val(Off)), ToInt32(Z'000000FF'))
!#define UnsignedShort(Val, Off) IOR(UnsignedByte(Val, Off+1), SHIFTL(UnsignedByte(Val, Off), 8))
!    Res = IOR(UnsignedShort(ByteArr, Offset+2), SHIFTL(UnsignedShort(ByteArr, Offset), 16))
!#undef UnsignedByte
!#undef UnsignedShort

    RETURN

END FUNCTION Pack_I32_A3

!**************************************************************************

FUNCTION Pack_I32_A4(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 32-bit integer value using the *MVBITS* intrinsic subroutine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt32                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! little-endian order
    Res = 0
    CALL MVBITS(MaskI32(ByteArr(Offset)),   0, 8, Res,  0)
    CALL MVBITS(MaskI32(ByteArr(Offset+1)), 0, 8, Res,  8)
    CALL MVBITS(MaskI32(ByteArr(Offset+2)), 0, 8, Res, 16)
    CALL MVBITS(MaskI32(ByteArr(Offset+3)), 0, 8, Res, 24)

    ! big-endian order
!    Res = 0
!    CALL MVBITS(MaskI32(ByteArr(Offset)),   0, 8, Res, 24)
!    CALL MVBITS(MaskI32(ByteArr(Offset+1)), 0, 8, Res, 16)
!    CALL MVBITS(MaskI32(ByteArr(Offset+2)), 0, 8, Res,  8)
!    CALL MVBITS(MaskI32(ByteArr(Offset+3)), 0, 8, Res,  0)

    RETURN

END FUNCTION Pack_I32_A4

!**************************************************************************

FUNCTION Pack_I32_A5(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 32-bit integer value using the *TRANSFER* intrinsic function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt32                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! little-endian order
    Res = TRANSFER([ByteArr(Offset), ByteArr(Offset+1), ByteArr(Offset+2), &
                    ByteArr(Offset+3)], 0_kInt32)

    ! big-endian order
!    Res = TRANSFER([ByteArr(Offset+3), ByteArr(Offset+2), ByteArr(Offset+1), 
!                    ByteArr(Offset)], 0_kInt32)

    RETURN

END FUNCTION Pack_I32_A5

!**************************************************************************

FUNCTION Pack_I32_A6(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 32-bit integer value using the *C_LOC* and *C_F_POINTER*
    !  intrinsic module routines. <br>
    !  *Note*:  This routine works best for system-endian order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt32                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32,  POINTER   :: fPtr     ! Fortran pointer to the input
    TYPE(C_PTR)         :: CPtr     ! C pointer to the input

! FLOW

    !+++ get a C pointer to the input +++
    CPtr = C_LOC(ByteArr(Offset))
    
    !+++ associate a Fortran pointer with the C pointer +++
    CALL C_F_POINTER(cPtr, fPtr)

    !+++ copy bit patterns +++
    ! system (or machine) endian order
    Res = fPtr

    ! little endian order
!    IF (IsLittleEndian) THEN
!        Res = fPtr
!    ELSE
!        Res = SwapBytes(fPtr)
!    END IF

    ! big endian order
!    IF (IsLittleEndian) THEN
!        Res = SwapBytes(fPtr)
!    ELSE
!        Res = fPtr
!    END IF

    !+++ nullify pointers +++
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END FUNCTION Pack_I32_A6

!**************************************************************************

FUNCTION Pack_I32_A7(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 32-bit integer value using the *EQUIVALENCE* statement. <br>
    !  *Note*:  This routine works best for system-endian order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt32                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: Input(0:3)
    tUInt32     :: Output
    EQUIVALENCE (Input, Output)

! FLOW

    !+++ copy input +++
    Input(0:3) = ByteArr(Offset:Offset+3)
    
    !+++ copy output +++
    ! system (or machine) endian order
    Res = Output

    ! little endian order
!    IF (IsLittleEndian) THEN
!        Res = Output
!    ELSE
!        Res = SwapBytes(Output)
!    END IF
    
    ! big endian order
!    IF (IsLittleEndian) THEN
!        Res = SwapBytes(Output)
!    ELSE
!        Res = Output
!    END IF

    RETURN

END FUNCTION Pack_I32_A7

!**************************************************************************

FUNCTION PackPartial(Buf, Off, Length, PackFull) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack three or fewer bytes of the array 'Buf' at offset 'Off'
    ! into the 32-bit word 'Res'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  !! buffer
    tIndex, INTENT(IN)  :: Off      !! offset
    tIndex, INTENT(IN)  :: Length   !! the number of bytes to pack (between 1 to 3)
    PROCEDURE(Pack_I32) :: PackFull !! procedure to convert a byte array to 32-bit integer
    tUInt32             :: Res      !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: Wrk(0:3)

! FLOW

    ! initialize
    Wrk = 0_kInt8

    ! gather available bytes in little-endian order
    Wrk(0:Length-1) = Buf(Off:Off+Length-1)

    ! gather available bytes in big-endian order
!    Wrk(Length-1:0:-1) = Buf(Off:Off+Length-1)
    
    ! pack bytes into word
    Res = PackFull(Wrk, 0_kIndex)

    RETURN

END FUNCTION PackPartial

!******************************************************************************

FUNCTION Pack_I16_A1(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at offset 'Off' into the 16-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex,         INTENT(IN)  :: Off      ! offset
    tUInt16                     :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! implementation algorithm #1
    Res = IOR(MaskI16(Buf(Off)), SHIFTL(MaskI16(Buf(Off+1)), 8))

    RETURN

END FUNCTION Pack_I16_A1

!******************************************************************************

FUNCTION Pack_I16_A2(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at offset 'Off' into the 16-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex,         INTENT(IN)  :: Off      ! offset
    tUInt16                     :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! implementation algorithm #2 (comparable to #1)
    Res = MaskI16(Buf(Off)) + SHIFTL(MaskI16(Buf(Off+1)), 8)

    RETURN

END FUNCTION Pack_I16_A2

!******************************************************************************

FUNCTION Pack_I16_A3(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at offset 'Off' into the 16-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex,         INTENT(IN)  :: Off      ! offset
    tUInt16                     :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! implementation algorithm #3 (comparable to #1)
#define UnsignedByte(Val, Off)  IAND(ToInt32(Val(Off)), Z'000000FF')
#define UnsignedShort(Val, Off) IOR(UnsignedByte(Val, Off), SHIFTL(UnsignedByte(Val, Off+1), 8))
    Res = ToInt16(UnsignedShort(Buf, Off))
#undef UnsignedByte
#undef UnsignedShort

    RETURN

END FUNCTION Pack_I16_A3

!******************************************************************************

FUNCTION Pack_I16_A4(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at offset 'Off' into the 16-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex,         INTENT(IN)  :: Off      ! offset
    tUInt16                     :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! implementation algorithm #4 (second slowest)
    ! initialize because that argument of MVBITS is INTENT(INOUT)
    Res = 0
    CALL MVBITS(MaskI16(Buf(Off)), 0, 8, Res, 0)
    CALL MVBITS(MaskI16(Buf(Off+1)), 0, 8, Res, 8)

    RETURN

END FUNCTION Pack_I16_A4

!******************************************************************************

FUNCTION Pack_I16_A5(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at offset 'Off' into the 16-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex,         INTENT(IN)  :: Off      ! offset
    tUInt16                     :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! implementation algorithm #5 (slowest)
    Res = TRANSFER([Buf(Off), Buf(Off+1)], 0_kInt16)

    RETURN

END FUNCTION Pack_I16_A5

!******************************************************************************

FUNCTION Pack_I16_A6(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at offset 'Off' into the 16-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex,         INTENT(IN)  :: Off      ! offset
    tUInt16                     :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt16, POINTER    :: Val => NULL()
    TYPE(C_PTR)         :: CPtr     ! C pointer to the input

! FLOW

    ! implementation algorithm #6
    CPtr = C_LOC(Buf(Off))
    CALL C_F_POINTER(CPtr, Val)
    Res = Val
    NULLIFY(Val)
    cPtr = C_NULL_PTR

    RETURN

END FUNCTION Pack_I16_A6

!******************************************************************************

FUNCTION Pack_I16_A7(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at offset 'Off' into the 16-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex,         INTENT(IN)  :: Off      ! offset
    tUInt16                     :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: Input(0:1)
    tUInt16     :: Output
    EQUIVALENCE (Input, Output)

! FLOW

    ! implementation algorithm #7
    Input(0:1) = Buf(Off:Off+1)
    Res = Output

    RETURN

END FUNCTION Pack_I16_A7

!******************************************************************************

END MODULE MBase_ExperimentalHash32

!******************************************************************************
