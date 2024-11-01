
MODULE MBase_ExperimentalHash64

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains an experimental implementation of various non-cryptographic
!   hash function routines that output a hash value as a 64-bit integer.  The available
!   hash functions are a subset of those reference hash functions implemented in the
!   <a href="../module/mbase_referencehash64.html">ModBase_ReferenceHash64</a> module.
!   The API of these experimental routines are the same as those reference routines with
!   the exception of an additional argument (*Algo*). <br>
!   The *Algo* argument is an algorithm flag used to indicate which algorithm is employed
!   to implement a *Pack_I64* procedure, which perform a conversion from an array of eight
!   8-bit integers to a 64-bit integer.  There are a number of possible implementations
!   of the *Pack_I64* procedure.  In this module, seven basic implementations are provided.
!   A user can perform a benchmark of each specific hash function routine in order to know
!   which one of the *Pack_I64* algorithms is the best one for a particular system (i.e.
!   a combination of operating system, machine as well as compiler used).  The benchmark
!   can then be used to implement an optimal implementation of the hash function. <br>
!   <br>
!^ **REFERENCES**: <br>
!   See the <a href="../module/mbase_referencehash64.html">ModBase_ReferenceHash64</a>
!   module for references of the available hash functions in this module.

!** USE STATEMENTS:
    USE ISO_C_BINDING,      ONLY: C_LOC, C_F_POINTER, C_PTR, C_NULL_PTR
    USE MBase_Common
    USE MBase_ByteUtil,   ONLY: SwapBytes, SwapByteArray

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: City_Hash64_Exp
    PUBLIC :: FarmNa_Hash64_Exp
    PUBLIC :: FarmUo_Hash64_Exp
    PUBLIC :: Komi_Hash64_Exp
    PUBLIC :: Metro_Hash64_Exp
    PUBLIC :: Murmur3_Hash128_Exp
    PUBLIC :: PengyV02_Hash64_Exp
    PUBLIC :: PengyV03_Hash64_Exp
    PUBLIC :: Spooky_Hash128_Exp
    PUBLIC :: Wy_Hash64_Exp
    PUBLIC :: WyF3_Hash64_Exp
    PUBLIC :: XX_Hash64_Exp
    PUBLIC :: XX3_Hash64_Exp
    PUBLIC :: XX3_Hash128_Exp

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define MaskI64(X)      IAND(ToInt64(X), ToInt64(Z'00000000000000FF'))

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'ModBase_ExperimentalHash64'
    ! The maximum (positive) number of hash code
    tUInt64,   PARAMETER    :: MaxHash  = ToInt64(Z'7FFFFFFFFFFFFFFF')

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !----------------------------------------------------------------------
        FUNCTION Pack_I64(ByteArr, Offset) RESULT(Res)
            !^ To convert an array of 8-bit integers starting at the offset to
            !  a 64-bit integer value.
            IMPORT
            tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
            tIndex,         INTENT(IN)  :: Offset       !! offset
            tUInt64                     :: Res          !! result
        END FUNCTION Pack_I64
        !----------------------------------------------------------------------
        SUBROUTINE Unpack_I64(Val, ByteArr, Offset)
            !^ To convert a 64-bit integer value to an array of 8-bit integers
            !  starting at the offset to.
            IMPORT
            tUInt64, TARGET, INTENT(IN)     :: Val          !! 64-bit integer
            tUInt8,          INTENT(INOUT)  :: ByteArr(0:)  !! byte array
            tIndex,          INTENT(IN)     :: Offset       !! offset
        END SUBROUTINE Unpack_I64
        !----------------------------------------------------------------------
    END INTERFACE
    INTERFACE
        MODULE FUNCTION Metro_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the MetroHash64 hash algorithm by J. Andrew
            !  Rogers [2].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-9)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION Murmur3_Hash128_Exp(Input, InpSize, Algo, StartHash, RemoveSign, HashPair) RESULT(HashCode)
            !^ To compute hash code using the MurmurHash3 hash algorithm by Austin
            !  Appleby [1].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-14)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64,  OPTIONAL,     INTENT(OUT) :: HashPair(2)  !! dual (128-bit) hash codes
            tUInt64                             :: HashCode     !! single (64-bit) hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION Wy_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using version 3 (?) of the WyHash hash algorithm by
            !  Wang Yi [3, 4].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-11)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION WyF3_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the final version 3 of the WyHash hash algorithm
            !  by Wang Yi [3].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-10)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION XX_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the XXHash hash algorithm by Yann Collet [5].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-9)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION XX3_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign, &
                                    Secret) RESULT(HashCode)
            !^ To compute hash code using the XX3Hash64 hash algorithm by Yann Collet [5].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-10)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt8,   OPTIONAL,     INTENT(IN)  :: Secret(:)
            !^ a byte (8-bit integer) array (of at least 192 bytes) representing
            !  a custom secret <br>
            tUInt64                             :: HashCode             !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION XX3_Hash128_Exp(Input, InpSize, Algo, StartHash, RemoveSign, &
                                     Secret, HashPair) RESULT(HashCode)
            !^ To compute hash code using the XX3Hash128 hash algorithm by Yann Collet [5].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-9)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt8,   OPTIONAL,     INTENT(IN)  :: Secret(:)
            !^ a byte (8-bit integer) array (of at least 192 bytes) representing
            !  a custom secret <br>
            tUInt64,  OPTIONAL,     INTENT(OUT) :: HashPair(2)  !! dual (128-bit) hash codes
            tUInt64                             :: HashCode     !! single (64-bit) hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION City_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign, &
                                     Seed) RESULT(HashCode)
            !^ To compute hash code using the CityHash hash algorithm by Google Inc [6].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-10)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64,  OPTIONAL,     INTENT(IN)  :: Seed         !! optional seed
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION FarmNa_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign, &
                                       Seed) RESULT(HashCode)
            !^ To compute hash code using the FarmNaHash hash algorithm by Google Inc [7].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-9)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64,  OPTIONAL,     INTENT(IN)  :: Seed         !! optional seed
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION FarmUo_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign, &
                                       Seed) RESULT(HashCode)
            !^ To compute hash code using the FarmUoHash hash algorithm by Google Inc [7].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-9)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64,  OPTIONAL,     INTENT(IN)  :: Seed         !! optional seed
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION Spooky_Hash128_Exp(Input, InpSize, Algo, StartHash, RemoveSign, &
                                        Seed, HashPair) RESULT(HashCode)
            !^ To compute hash code using version 2 of the SpookyHash hash algorithm
            !  by Bob Jenkins [8].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-9)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64,  OPTIONAL,     INTENT(IN)  :: Seed         !! optional seed
            tUInt64,  OPTIONAL,     INTENT(OUT) :: HashPair(2)  !! dual (128-bit) hash codes
            tUInt64                             :: HashCode     !! single (64-bit) hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION PengyV03_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the PengyHash hash algorithm (v0.3) by
            !  Alberto Fajardo [15]. <br>
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION PengyV02_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the PengyHash hash algorithm (v0.2) by
            !  Alberto Fajardo [9, 10].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION Komi_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the KomiHash hash algorithm by Aleksey Vaneev [13].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-8)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

FUNCTION Pack_I64_A1(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 64-bit integer value using the *SHIFTL* and *IOR* intrinsic
    !  functions.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt64                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! little-endian order
    Res = IOR(IOR(IOR(IOR(IOR(IOR(IOR(       MaskI64(ByteArr(Offset)),          &
                                      SHIFTL(MaskI64(ByteArr(Offset+1)),  8)),  &
                                      SHIFTL(MaskI64(ByteArr(Offset+2)), 16)),  &
                                      SHIFTL(MaskI64(ByteArr(Offset+3)), 24)),  &
                                      SHIFTL(MaskI64(ByteArr(Offset+4)), 32)),  &
                                      SHIFTL(MaskI64(ByteArr(Offset+5)), 40)),  &
                                      SHIFTL(MaskI64(ByteArr(Offset+6)), 48)),  &
                                      SHIFTL(MaskI64(ByteArr(Offset+7)), 56))

    ! big-endian order
!    Res = IOR(IOR(IOR(IOR(IOR(IOR(IOR(SHIFTL(MaskI64(ByteArr(Offset)),   56),  &
!                                      SHIFTL(MaskI64(ByteArr(Offset+1)), 48)), &
!                                      SHIFTL(MaskI64(ByteArr(Offset+2)), 40)), &
!                                      SHIFTL(MaskI64(ByteArr(Offset+3)), 32)), &
!                                      SHIFTL(MaskI64(ByteArr(Offset+4)), 24)), &
!                                      SHIFTL(MaskI64(ByteArr(Offset+5)), 16)), &
!                                      SHIFTL(MaskI64(ByteArr(Offset+6)),  8)), &
!                                             MaskI64(ByteArr(Offset+7)))

    RETURN

END FUNCTION Pack_I64_A1

!**************************************************************************

FUNCTION Pack_I64_A2(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 64-bit integer value using the *SHIFTL* intrinsic function
    !  and an addition operator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt64                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! little-endian order
    Res =        MaskI64(ByteArr(Offset))        + &
          SHIFTL(MaskI64(ByteArr(Offset+1)),  8) + &
          SHIFTL(MaskI64(ByteArr(Offset+2)), 16) + &
          SHIFTL(MaskI64(ByteArr(Offset+3)), 24) + &
          SHIFTL(MaskI64(ByteArr(Offset+4)), 32) + &
          SHIFTL(MaskI64(ByteArr(Offset+5)), 40) + &
          SHIFTL(MaskI64(ByteArr(Offset+6)), 48) + &
          SHIFTL(MaskI64(ByteArr(Offset+7)), 56)

    ! big-endian order
!    Res = SHIFTL(MaskI64(ByteArr(Offset)),   56) + &
!          SHIFTL(MaskI64(ByteArr(Offset+1)), 48) + &
!          SHIFTL(MaskI64(ByteArr(Offset+2)), 40) + &
!          SHIFTL(MaskI64(ByteArr(Offset+3)), 32) + &
!          SHIFTL(MaskI64(ByteArr(Offset+4)), 24) + &
!          SHIFTL(MaskI64(ByteArr(Offset+5)), 16) + &
!          SHIFTL(MaskI64(ByteArr(Offset+6)),  8) + &
!                 MaskI64(ByteArr(Offset+7))

    RETURN

END FUNCTION Pack_I64_A2

!**************************************************************************

FUNCTION Pack_I64_A3(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 64-bit integer value using the *SHIFTL* and *IOR* intrinsic
    !  functions. <br>
    !  *Note*: Although this routine and the *Pack_I64_A3* routine employ
    !  the same intrinsic functions, orders of the functions being used
    !  are somewhat different.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt64                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! little-endian order
#define UnsignedByte(Val, Off)      IAND(ToInt32(Val(Off)), ToInt32(Z'000000FF'))
#define UnsignedShort(Val, Off)     IOR(UnsignedByte(Val, Off), SHIFTL(UnsignedByte(Val, Off+1), 8))
#define SignedInteger(Val, Off)     IOR(UnsignedShort(Val, Off), SHIFTL(UnsignedShort(Val, Off+2), 16))
#define UnsignedInteger(Val, Off)   IAND(ToInt64(SignedInteger(Val, Off)), ToInt64(Z'00000000FFFFFFFF'))
    Res = IOR(UnsignedInteger(ByteArr, Offset), SHIFTL(UnsignedInteger(ByteArr, Offset+4), 32))
#undef UnsignedByte
#undef UnsignedShort
#undef SignedInteger
#undef UnsignedInteger

    ! big-endian order
!#define UnsignedByte(Val, Off)      IAND(ToInt32(Val(Off)), Z'000000FF')
!#define UnsignedShort(Val, Off)     IOR(UnsignedByte(Val, Off+1), SHIFTL(UnsignedByte(Val, Off), 8))
!#define SignedInteger(Val, Off)     IOR(UnsignedShort(Val, Off+2), SHIFTL(UnsignedShort(Val, Off), 16))
!#define UnsignedInteger(Val, Off)   IAND(ToInt64(SignedInteger(Val, Off)), Z'00000000FFFFFFFF')
!    Res = IOR(UnsignedInteger(ByteArr, Offset+4), SHIFTL(UnsignedInteger(ByteArr, Offset), 32))
!#undef UnsignedByte
!#undef UnsignedShort
!#undef SignedInteger
!#undef UnsignedInteger

    RETURN

END FUNCTION Pack_I64_A3

!**************************************************************************

FUNCTION Pack_I64_A4(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 64-bit integer value using the *MVBITS* intrinsic subroutine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt64                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! little-endian order
    Res = 0
    CALL MVBITS(MaskI64(ByteArr(Offset)),   0, 8, Res,  0)
    CALL MVBITS(MaskI64(ByteArr(Offset+1)), 0, 8, Res,  8)
    CALL MVBITS(MaskI64(ByteArr(Offset+2)), 0, 8, Res, 16)
    CALL MVBITS(MaskI64(ByteArr(Offset+3)), 0, 8, Res, 24)
    CALL MVBITS(MaskI64(ByteArr(Offset+4)), 0, 8, Res, 32)
    CALL MVBITS(MaskI64(ByteArr(Offset+5)), 0, 8, Res, 40)
    CALL MVBITS(MaskI64(ByteArr(Offset+6)), 0, 8, Res, 48)
    CALL MVBITS(MaskI64(ByteArr(Offset+7)), 0, 8, Res, 56)

    ! big-endian order
!    Res = 0
!    CALL MVBITS(MaskI64(ByteArr(Offset)),   0, 8, Res, 56)
!    CALL MVBITS(MaskI64(ByteArr(Offset+1)), 0, 8, Res, 48)
!    CALL MVBITS(MaskI64(ByteArr(Offset+2)), 0, 8, Res, 40)
!    CALL MVBITS(MaskI64(ByteArr(Offset+3)), 0, 8, Res, 32)
!    CALL MVBITS(MaskI64(ByteArr(Offset+4)), 0, 8, Res, 24)
!    CALL MVBITS(MaskI64(ByteArr(Offset+5)), 0, 8, Res, 16)
!    CALL MVBITS(MaskI64(ByteArr(Offset+6)), 0, 8, Res,  8)
!    CALL MVBITS(MaskI64(ByteArr(Offset+7)), 0, 8, Res,  0)

    RETURN

END FUNCTION Pack_I64_A4

!**************************************************************************

FUNCTION Pack_I64_A5(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 64-bit integer value using the *TRANSFER* intrinsic function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt64                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! little-endian order
    Res = TRANSFER([ByteArr(Offset),   ByteArr(Offset+1), ByteArr(Offset+2), &
                    ByteArr(Offset+3), ByteArr(Offset+4), ByteArr(Offset+5), &
                    ByteArr(Offset+6), ByteArr(Offset+7)], 0_kInt64)

    ! big-endian order
!    Res = TRANSFER([ByteArr(Offset+7), ByteArr(Offset+6), ByteArr(Offset+5), 
!                    ByteArr(Offset+4), ByteArr(Offset+3), ByteArr(Offset+2), &
!                    ByteArr(Offset+1), ByteArr(Offset)], 0_kInt64)

    RETURN

END FUNCTION Pack_I64_A5

!**************************************************************************

FUNCTION Pack_I64_A6(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 64-bit integer value using the *C_LOC* and *C_F_POINTER*
    !  intrinsic module routines. <br>
    !  *Note*:  This routine works best for system-endian order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt64                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64, POINTER    :: fPtr     ! Fortran pointer to the input
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

END FUNCTION Pack_I64_A6

!**************************************************************************

FUNCTION Pack_I64_A7(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 64-bit integer value using the *EQUIVALENCE* statement. <br>
    !  *Note*:  This routine works best for system-endian order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, TARGET, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex,         INTENT(IN)  :: Offset       !! offset
    tUInt64                     :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: Input(0:7)
    tUInt64     :: Output
    EQUIVALENCE (Input, Output)

! FLOW

    !+++ copy input +++
    Input(0:7) = ByteArr(Offset:Offset+7)
    
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

END FUNCTION Pack_I64_A7

!**************************************************************************

FUNCTION PackPartial(Buf, Off, Length, PackFull) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack three or fewer bytes of the array 'Buf' at offset 'Off'
    ! into the 64-bit word 'Res'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  !! buffer
    tIndex, INTENT(IN)  :: Off      !! offset
    tIndex, INTENT(IN)  :: Length   !! the number of bytes to pack (between 1 to 7)
    PROCEDURE(Pack_I64) :: PackFull !! procedure to convert a byte array to 32-bit integer
    tUInt64             :: Res      !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: Wrk(0:7)

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

SUBROUTINE Unpack_I64_A1(Val, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a 64-bit integer value to an array of 8-bit integers
    !  starting at the offset using the *SHIFTR* intrinsic function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, TARGET, INTENT(IN)     :: Val          !! 64-bit integer
    tUInt8,          INTENT(INOUT)  :: ByteArr(0:)  !! byte array
    tIndex,          INTENT(IN)     :: Offset       !! offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! little-endian order
    ByteArr(Offset)   = ToInt8(Val)
    ByteArr(Offset+1) = ToInt8(SHIFTR(Val, 8))
    ByteArr(Offset+2) = ToInt8(SHIFTR(Val, 16))
    ByteArr(Offset+3) = ToInt8(SHIFTR(Val, 24))
    ByteArr(Offset+4) = ToInt8(SHIFTR(Val, 32))
    ByteArr(Offset+5) = ToInt8(SHIFTR(Val, 40))
    ByteArr(Offset+6) = ToInt8(SHIFTR(Val, 48))
    ByteArr(Offset+7) = ToInt8(SHIFTR(Val, 56))

    ! big-endian order
!    ByteArr(Offset)   = ToInt8(SHIFTR(Val, 56))
!    ByteArr(Offset+1) = ToInt8(SHIFTR(Val, 48))
!    ByteArr(Offset+2) = ToInt8(SHIFTR(Val, 40))
!    ByteArr(Offset+3) = ToInt8(SHIFTR(Val, 32))
!    ByteArr(Offset+4) = ToInt8(SHIFTR(Val, 24))
!    ByteArr(Offset+5) = ToInt8(SHIFTR(Val, 16))
!    ByteArr(Offset+6) = ToInt8(SHIFTR(Val, 8))
!    ByteArr(Offset+7) = ToInt8(Val)

    RETURN

END SUBROUTINE Unpack_I64_A1

!******************************************************************************

SUBROUTINE Unpack_I64_A2(Val, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a 64-bit integer value to an array of 8-bit integers
    !  starting at the offset using the *IBITS* intrinsic function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, TARGET, INTENT(IN)     :: Val          !! 64-bit integer
    tUInt8,          INTENT(INOUT)  :: ByteArr(0:)  !! byte array
    tIndex,          INTENT(IN)     :: Offset       !! offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! little-endian order
    ByteArr(Offset)   = ToInt8(IBITS(Val,  0, 8))
    ByteArr(Offset+1) = ToInt8(IBITS(Val,  8, 8))
    ByteArr(Offset+2) = ToInt8(IBITS(Val, 16, 8))
    ByteArr(Offset+3) = ToInt8(IBITS(Val, 24, 8))
    ByteArr(Offset+4) = ToInt8(IBITS(Val, 32, 8))
    ByteArr(Offset+5) = ToInt8(IBITS(Val, 40, 8))
    ByteArr(Offset+6) = ToInt8(IBITS(Val, 48, 8))
    ByteArr(Offset+7) = ToInt8(IBITS(Val, 56, 8))

    ! big-endian order
!    ByteArr(Offset)   = ToInt8(IBITS(Val, 56, 8))
!    ByteArr(Offset+1) = ToInt8(IBITS(Val, 48, 8))
!    ByteArr(Offset+2) = ToInt8(IBITS(Val, 40, 8))
!    ByteArr(Offset+3) = ToInt8(IBITS(Val, 32, 8))
!    ByteArr(Offset+4) = ToInt8(IBITS(Val, 24, 8))
!    ByteArr(Offset+5) = ToInt8(IBITS(Val, 16, 8))
!    ByteArr(Offset+6) = ToInt8(IBITS(Val,  8, 8))
!    ByteArr(Offset+7) = ToInt8(IBITS(Val,  0, 8))

    RETURN

END SUBROUTINE Unpack_I64_A2

!******************************************************************************

SUBROUTINE Unpack_I64_A3(Val, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a 64-bit integer value to an array of 8-bit integers
    !  starting at the offset using the *TRANSFER* intrinsic function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, TARGET, INTENT(IN)     :: Val          !! 64-bit integer
    tUInt8,          INTENT(INOUT)  :: ByteArr(0:)  !! byte array
    tIndex,          INTENT(IN)     :: Offset       !! offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! system (machine) endian order
    ByteArr(Offset:) = TRANSFER(Val, ByteArr)
    
    ! little-endian order
!    IF (.NOT.IsLittleEndian) CALL SwapByteArray(ByteArr)

    ! big-endian order
!    IF (IsLittleEndian) CALL SwapByteArray(ByteArr)

    RETURN

END SUBROUTINE Unpack_I64_A3

!******************************************************************************

SUBROUTINE Unpack_I64_A4(Val, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a 64-bit integer value to an array of 8-bit integers
    !  starting at the offset using the *C_LOC* and *C_F_POINTER*
    !  intrinsic module routines.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, TARGET, INTENT(IN)     :: Val          !! 64-bit integer
    tUInt8,          INTENT(INOUT)  :: ByteArr(0:)  !! byte array
    tIndex,          INTENT(IN)     :: Offset       !! offset
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt32,  PARAMETER :: ByteSize = BIT_SIZE(0_kInt64)/BIT_SIZE(0_kInt8)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8, POINTER :: fPtr(:)  ! Fortran pointer to the input
    TYPE(C_PTR)     :: CPtr     ! C pointer to the input

! FLOW

    ! get a C pointer to the input
    CPtr = C_LOC(Val)
    
    ! associate a Fortran pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr, [ByteSize])

    ! copy bit patterns (system-endian order)
    ByteArr(Offset:) = fPtr(1:)
        
    ! little-endian order
!    IF (.NOT.IsLittleEndian) CALL SwapByteArray(ByteArr)

    ! big-endian order
!    IF (IsLittleEndian) CALL SwapByteArray(ByteArr)

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE Unpack_I64_A4

!******************************************************************************

SUBROUTINE Unpack_I64_A5(Val, ByteArr, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a 64-bit integer value to an array of 8-bit integers
    !  starting at the offset using the *EQUIVALENCE* statement.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, TARGET, INTENT(IN)     :: Val          !! 64-bit integer
    tUInt8,          INTENT(INOUT)  :: ByteArr(0:)  !! byte array
    tIndex,          INTENT(IN)     :: Offset       !! offset

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt32,  PARAMETER :: ByteSize = BIT_SIZE(0_kInt64)/BIT_SIZE(0_kInt8)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: ByteVals(ByteSize)
    tUInt64     :: LongVal
    EQUIVALENCE(ByteVals, LongVal)

! FLOW
    
    ! system (machine) endian order
    LongVal = Val
    ByteArr(Offset:) = ByteVals(1:)
    
    ! little-endian order
!    IF (.NOT.IsLittleEndian) CALL SwapByteArray(ByteArr)

    ! big-endian order
!    IF (IsLittleEndian) CALL SwapByteArray(ByteArr)

    RETURN

END SUBROUTINE Unpack_I64_A5

!******************************************************************************

FUNCTION UnsignedLongMultiplyorFold(LHS, RHS) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply or fold.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)   :: LHS
    tUInt64, INTENT(IN)   :: RHS
    tUInt64             :: Res

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt64, PARAMETER  :: MaxU32 = ToInt64(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64 :: LHS_Lo, LHS_Hi, RHS_Lo, RHS_Hi
    tUInt64 :: Lo_Lo, Hi_Lo
    tUInt64 :: Cross

!** FLOW

    ! the Grade School method of multiplication.
    LHS_Lo = IAND(LHS, MaxU32)
    LHS_Hi = SHIFTR(LHS, 32)
    RHS_Lo = IAND(RHS, MaxU32)
    RHS_Hi = SHIFTR(RHS, 32)
    Lo_Lo = LHS_Lo*RHS_Lo
    Hi_Lo = LHS_Hi*RHS_Lo

    ! Add the products together. This will never overfLow.
    Cross = SHIFTR(Lo_Lo, 32) + IAND(Hi_Lo, MaxU32) + LHS_Lo*RHS_Hi
    Res   = IEOR(IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, MaxU32)), &
                 SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + LHS_Hi*RHS_Hi)

    RETURN

END FUNCTION UnsignedLongMultiplyorFold

!******************************************************************************

END MODULE MBase_ExperimentalHash64

!******************************************************************************
