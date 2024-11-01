
MODULE MBase_ReferenceHash32

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains a reference implementation of various non-cryptographic
!   hash function routines that output a hash value as a 32-bit integer.  The API
!   of these hash function routines are mostly similar to that of routines in the
!   <a href="../../xpfbase/module/mbase_simplehash32.html">ModBase_SimpleHash32</a>
!   module.  The main difference between routines in these two modules is that routines
!   in the *ModBase_SimpleHash32* module process input data one byte at a time whereas
!   routines in this module commonly process input data several/many bytes at a time.
!   Also, routines in this module are somewhat more complicated than those in the
!   *ModBase_SimpleHash32* module. <br>
!   Similar to those routines in the *ModBase_SimpleHash32* module, all routines can
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
!   [2] <a href="https://burtleburtle.net/bob/c/lookup3.c">Lookup 3 Hash Function: C Code. </a> <br>
!   [3] <a href="https://github.com/google/cityhash">CityHash: a family of hash functions
!       for strings. </a> <br>
!   [4] <a href="https://github.com/google/farmhash">FarmHash: a family of hash functions. </a> <br>
!   [5] <a href="http://www.azillionmonkeys.com/qed/hash.html">Hash functions by Paul Hsieh. </a> <br>
!   [6] <a href="https://github.com/fortran-lang/stdlib">Fortran Standard Library. </a> <br>
!   [7] <a href="https://github.com/gzm55/hash-garage">NMHash32 Hash Functions. </a> <br>
!   [8] <a href="https://github.com/Cyan4973/xxHash">xxHash: Extremely fast hash algorithm. </a> <br>
!   [9] <a href="https://github.com/tildeleb/hashland">HashLand: a collection of hash
!       functions in Go. </a> <br>
!   [10] <a href="https://github.com/rurban/fast-hash/tree/master">FastHash by Zilong Tan. </a> <br>
!   [11] <a href="https://github.com/tommyettinger/waterhash">WaterHash: A variant of WyHash. </a> <br>
!   [12] <a href="https://github.com/veorq/SipHash">SipHash: high-speed secure pseudorandom
!       function for short messages. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: CityHash_I32
    PUBLIC :: CrapWowHash_I32
    PUBLIC :: FarmMkHash_I32
    PUBLIC :: FarmMkHash_I32_Recur
    PUBLIC :: FastHash_I32
    PUBLIC :: HalfSipHash24_I32
    PUBLIC :: HalfSipHash_I32
    PUBLIC :: Lookup3Hash_I32
    PUBLIC :: Murmur3Hash_I32
    PUBLIC :: NMHash_V1_I32
    PUBLIC :: NMxHash_V1_I32
    PUBLIC :: NMHash_V2_I32
    PUBLIC :: NMxHash_V2_I32
    PUBLIC :: SuperFastHash_I32
    PUBLIC :: WaterHash_I32
    PUBLIC :: WaterHash_I32_New
    PUBLIC :: XXHash_I32

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'ModBase_ReferenceHash32'
    ! The maximum (positive) number of hash code
    tUInt32,   PARAMETER    :: MaxHash = ToInt32(Z'7FFFFFFF')

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE
        !----------------------------------------------------------------------
        MODULE FUNCTION Lookup3Hash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the Lookup3 hash algorithm by Bob Jenkins [2].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION Lookup3Hash_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION Murmur3Hash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the Murmur3 hash algorithm by Austin Appleby [1].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION Murmur3Hash_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION CityHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the City hash algorithm by Google Inc [3].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION CityHash_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION FarmMkHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the FarmMk hash algorithm by Google Inc [4]. <br>
            !  *Note*: The working routine of this procedure is a non-recursive one.
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION FarmMkHash_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION FarmMkHash_I32_Recur(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the FarmMk hash algorithm by Google Inc [4]. <br>
            !  *Note*: The working routine of this procedure is a recursive one.
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION FarmMkHash_I32_Recur
        !----------------------------------------------------------------------
        MODULE FUNCTION SuperFastHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the SuperFast hash algorithm by Paul
            !  Hsieh [5].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION SuperFastHash_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION NMHash_V1_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using version 1 of the NMHASH hash algorithm
            !  by James Z. M. Gao [6].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION NMHash_V1_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION NMxHash_V1_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using version 1 of the NMxHASH hash algorithm by
            !  James Z. M. Gao [6].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION NMxHash_V1_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION NMHash_V2_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using version 2 of the NMHASH hash algorithm
            !  by James Z. M. Gao [7].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION NMHash_V2_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION NMxHash_V2_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using version 2 of the NMxHASH hash algorithm by
            !  James Z. M. Gao [7].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION NMxHash_V2_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION XXHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the XXHash hash algorithm by Yann Collet [8].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION XXHash_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION CrapWowHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the CrapWow hash algorithm [9].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION CrapWowHash_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION FastHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the Fast hash algorithm by
            !  Zilong Tan [10].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION FastHash_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION WaterHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the WaterHash hash algorithm by Tommy
            !  Ettinger [6].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION WaterHash_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION WaterHash_I32_New(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using a new version of the WaterHash hash
            !  algorithm by Tommy Ettinger [11].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION WaterHash_I32_New
        !----------------------------------------------------------------------
        MODULE FUNCTION HalfSipHash24_I32(Input, InpSize, Key, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the HalfSipHash24 hash algorithm by
            !  Jean-Philippe Aumasson [12].
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt8,                 INTENT(IN)  :: Key(:)       !! key (at least 8 bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION HalfSipHash24_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION HalfSipHash_I32(Input, InpSize, Key, cRound, dRound, &
                                        StartHash, RemoveSign, HashLong) RESULT(HashCode)
            !^ To compute the hash value using the HalfSipHash hash algorithm by
            !  Jean-Philippe Aumasson [12] where the numbers of rounds are specified.
            TYPE(*), CONTIGUOUS,    INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt8,                 INTENT(IN)  :: Key(:)       !! key (at least 8 bytes)
            tIndex,                 INTENT(IN)  :: cRound       !! number of C rounds
            tIndex,                 INTENT(IN)  :: dRound       !! number of D rounds
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64,  OPTIONAL,     INTENT(OUT) :: HashLong     !! long integer hash code
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION HalfSipHash_I32
        !----------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

END MODULE MBase_ReferenceHash32

!******************************************************************************
