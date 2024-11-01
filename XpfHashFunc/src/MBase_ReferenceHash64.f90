
MODULE MBase_ReferenceHash64

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains a reference implementation of various non-cryptographic
!   hash function routines that output a hash value as a 64-bit integer.  The API
!   of these hash function routines are mostly similar to that of routines in the
!   <a href="../../xpfbase/module/mbase_simplehash64.html">ModBase_SimpleHash64</a>
!   module.  The main difference between routines in these two modules is that routines
!   in the *ModBase_SimpleHash64* module process input data one byte at a time whereas
!   routines in this module commonly process input data several/many bytes at a time.
!   Also, routines in this module are somewhat more complicated than those in the
!   *ModBase_SimpleHash64* module. <br>
!   Similar to those routines in the *ModBase_SimpleHash64* module, all routines can
!   be used for an input (i.e. a key) of any type and rank providing that the size of
!   the input (in bytes) is known at compile time.  All routines can be used for a
!   continued hashing by providing the previously computed hash value as an (optional)
!   input argument (i.e. the *StartHash* argument).  Optionally, a user can specify
!   whether to return only positive value of the hash code.  This is particularly
!   useful when used in conjunction with a hash table. <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://github.com/aappleby/smhasher">SMHasher: a test suite designed to
!       test the distribution, collision, and performance properties of non-cryptographic
!       hash functions. </a> <br>
!   [2] <a href="https://github.com/jandrewrogers/MetroHash">MetroHash: Faster, Better
!       Hash Functions. </a> <br>
!   [3] <a href="https://github.com/wangyi-fudan/wyhash">WYHASH and WYRAND - The FASTEST
!       QUALITY hash function, random number generators (PRNG) and hash map. </a> <br>
!   [4] <a href="https://github.com/OpenHFT/Zero-Allocation-Hashing/tree/ea">
!       Zero-Allocation Hashing for Java. </a> <br>
!   [5] <a href="https://github.com/Cyan4973/xxHash">xxHash: Extremely fast hash algorithm. </a> <br>
!   [6] <a href="https://github.com/google/cityhash">CityHash: a family of hash functions
!       for strings. </a> <br>
!   [7] <a href="https://github.com/google/farmhash">FarmHash: a family of hash functions. </a> <br>
!   [8] <a href="https://burtleburtle.net/bob/hash/spooky.html">SpookyHash: a 128-bit
!       non-cryptographic hash. </a> <br>
!   [9] <a href="https://github.com/rurban/smhasher">SMHasher by Reini Urban. </a> <br>
!   [10] <a href="https://github.com/fortran-lang/stdlib">Fortran Standard Library. </a> <br>
!   [11] <a href="https://github.com/veorq/SipHash">SipHash: high-speed secure pseudorandom
!       function for short messages. </a> <br>
!   [12] <a href="https://github.com/jonmaiga/mx3">mx3: A bit mixer, pseudo random number
!       generator and a hash function. </a> <br>
!   [13] <a href="https://github.com/avaneev/komihash">KOMIHASH - Very fast, high-quality
!       hash function, discrete-incremental and streamed hashing-capable (non-cryptographic,
!       in C) + PRNG.</a> <br>
!   [14] <a href="https://github.com/vnmakarov/mir">MIR Project: A lightweight JIT compiler
!       based on MIR (Medium Internal Representation) and C11 JIT compiler and interpreter
!       based on MIR. </a> <br>
!   [15] <a href="https://github.com/vnmakarov/mum-hash">MumHash: Hashing functions and
!       PRNGs based on them. </a> <br>
!   [15] <a href="https://github.com/tinypeng/pengyhash">PengyHash: Fast 64-bit
!       non-cryptographic function. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_ByteConverter

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: CityHash_I64
    PUBLIC :: FarmNaHash_I64
    PUBLIC :: FarmUoHash_I64
    PUBLIC :: KomiHash_I64
    PUBLIC :: MetroHash_I64
    PUBLIC :: MirHash_I64
    PUBLIC :: MumHash_I64
    PUBLIC :: Murmur3Hash_I128
    PUBLIC :: Mx3Hash_I64
    PUBLIC :: PengyHash_V02_I64
    PUBLIC :: PengyHash_V03_I64
    PUBLIC :: SipHash24_I64
    PUBLIC :: SipHash_I128
    PUBLIC :: SpookyHash_I128
    PUBLIC :: TSipHash_I64
    PUBLIC :: WyHash_I64
    PUBLIC :: WyHash_F3_I64
    PUBLIC :: XXHash_I64
    PUBLIC :: XX3Hash_I64
    PUBLIC :: XX3Hash_I128

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'ModBase_ReferenceHash64'
    ! The maximum (positive) number of hash code
    tUInt64,   PARAMETER    :: MaxHash  = ToInt64(Z'7FFFFFFFFFFFFFFF')

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE
        MODULE FUNCTION MetroHash_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the MetroHash64 hash algorithm by J. Andrew
            !  Rogers [2, 4].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION MetroHash_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION Murmur3Hash_I128(Input, InpSize, StartHash, RemoveSign, HashPair) RESULT(HashCode)
            !^ To compute hash code using the Murmur3 hash algorithm by Austin Appleby [1, 4].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64,  OPTIONAL,     INTENT(OUT) :: HashPair(2)  !! dual (128-bit) hash codes
            tUInt64                             :: HashCode     !! single (64-bit) hash code
        END FUNCTION Murmur3Hash_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION WyHash_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using version 3 (?) of the WyHash hash algorithm by
            !  Wang Yi [3, 4].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION WyHash_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION WyHash_F3_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the final version 3 of the WyHash hash algorithm
            !  by Wang Yi [3].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION WyHash_F3_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION XXHash_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the XXHash hash algorithm by Yann Collet [4, 5].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION XXHash_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION XX3Hash_I64(Input, InpSize, StartHash, RemoveSign, &
                                    Secret) RESULT(HashCode)
            !^ To compute hash code using the XX3Hash64 hash algorithm by Yann Collet [4, 5].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
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
        END FUNCTION XX3Hash_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION XX3Hash_I128(Input, InpSize, StartHash, RemoveSign, &
                                     Secret, HashPair) RESULT(HashCode)
            !^ To compute hash code using the XX3Hash128 hash algorithm by Yann Collet [4, 5].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
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
        END FUNCTION XX3Hash_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION CityHash_I64(Input, InpSize, StartHash, RemoveSign, &
                                     Seed) RESULT(HashCode)
            !^ To compute hash code using the CityHash hash algorithm by Google Inc [4, 6].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64,  OPTIONAL,     INTENT(IN)  :: Seed         !! optional seed
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION CityHash_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION FarmNaHash_I64(Input, InpSize, StartHash, RemoveSign, &
                                       Seed) RESULT(HashCode)
            !^ To compute hash code using the FarmNaHash hash algorithm by Google Inc [4, 7].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64,  OPTIONAL,     INTENT(IN)  :: Seed         !! optional seed
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION FarmNaHash_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION FarmUoHash_I64(Input, InpSize, StartHash, RemoveSign, &
                                       Seed) RESULT(HashCode)
            !^ To compute hash code using the FarmUoHash hash algorithm by Google Inc [4, 7].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64,  OPTIONAL,     INTENT(IN)  :: Seed         !! optional seed
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION FarmUoHash_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION SpookyHash_I128(Input, InpSize, StartHash, RemoveSign, &
                                        Seed, HashPair) RESULT(HashCode)
            !^ To compute hash code using version 2 of the SpookyHash hash algorithm
            !  by Bob Jenkins [8].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64,  OPTIONAL,     INTENT(IN)  :: Seed         !! optional seed
            tUInt64,  OPTIONAL,     INTENT(OUT) :: HashPair(2)  !! dual (128-bit) hash codes
            tUInt64                             :: HashCode     !! single (64-bit) hash code
        END FUNCTION SpookyHash_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION PengyHash_V03_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the PengyHash hash algorithm (v0.3) by
            !  Alberto Fajardo [15]. <br>
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION PengyHash_V03_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION PengyHash_V02_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the PengyHash hash algorithm (v0.2) by
            !  Alberto Fajardo [9, 10].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION PengyHash_V02_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION SipHash24_I64(Input, InpSize, Key, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the SipHash24 hash algorithm by Jean-Philippe
            !  Aumasson [11].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt8,                 INTENT(IN)  :: Key(0:15)    !! key bytes
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION SipHash24_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION TSipHash_I64(Input, InpSize, Key, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the TSipHash hash algorithm (a variant of SipHash)
            !  from SMHasher [9].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt8,                 INTENT(IN)  :: Key(0:15)    !! key bytes
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION TSipHash_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION Mx3Hash_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the Mx3Hash hash algorithm by Jon Maiga [12].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION Mx3Hash_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION KomiHash_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the KomiHash hash algorithm by Aleksey Vaneev [13].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION KomiHash_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION MirHash_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the MirHash hash algorithm by Vladimir Makarov [9, 14].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION MirHash_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION MumHash_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the MumHash hash algorithm by Vladimir Makarov [9, 15].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION MumHash_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION SipHash_I128(Input, InpSize, Key, cRound, dRound, StartHash, &
                                     RemoveSign, HashPair) RESULT(HashCode)
            !^ To compute hash code using the SipHash24 hash algorithm by Jean-Philippe
            !  Aumasson [11] where the numbers of rounds are specified.
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt8,                 INTENT(IN)  :: Key(0:15)    !! key bytes
            tIndex,                 INTENT(IN)  :: cRound       ! number of C rounds
            tIndex,                 INTENT(IN)  :: dRound       ! number of D rounds
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64,  OPTIONAL,     INTENT(OUT) :: HashPair(2)  !! dual (128-bit) hash codes
            tUInt64                             :: HashCode     !! single (64-bit) hash code
        END FUNCTION SipHash_I128
        !----------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

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

END MODULE MBase_ReferenceHash64

!******************************************************************************
