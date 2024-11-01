
MODULE MBase_OptimalHash64

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains an optimal implementation of various non-cryptographic hash
!   function routines that output a hash value as a 64-bit integer.  The available
!   hash functions are a subset of those reference hash functions implemented in the
!   <a href="../module/mbase_referencehash64.html">ModBase_ReferenceHash64</a> module.
!   The API of these optimal routines are exactly the same as those reference routines. <br>
!   It should be noted that each optimal hash function routine is based on a benchmark
!   that compares performances of various possible implementations of the hash function
!   as provided in the  <a href="../module/mbase_experimentalhash64.html">
!   ModBase_ExperimentalHash64</a> module.  It should also be noted that these so-called
!   optimal hash function routines may not actually be optimal for a particular user so
!   the user is highly encouraged to perform (by himself/herself) a benchmark of each
!   specific hash function routine in order to know which implementation is the best one
!   for a particular system (i.e. a combination of operating system, machine as well as
!   compiler used). <br>
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
    PUBLIC :: City_Hash64_Opt
    PUBLIC :: FarmNa_Hash64_Opt
    PUBLIC :: FarmUo_Hash64_Opt
    PUBLIC :: Komi_Hash64_Opt
    PUBLIC :: Metro_Hash64_Opt
    PUBLIC :: Murmur3_Hash128_Opt
    PUBLIC :: PengyV02_Hash64_Opt
    PUBLIC :: PengyV03_Hash64_Opt
    PUBLIC :: Spooky_Hash128_Opt
    PUBLIC :: Wy_Hash64_Opt
    PUBLIC :: WyF3_Hash64_Opt
    PUBLIC :: XX_Hash64_Opt
    PUBLIC :: XX3_Hash64_Opt
    PUBLIC :: XX3_Hash128_Opt

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'ModBase_OptimalHash64'
    ! The maximum (positive) number of hash code
    tUInt64,   PARAMETER    :: MaxHash  = ToInt64(Z'7FFFFFFFFFFFFFFF')

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE
        MODULE FUNCTION Metro_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the MetroHash64 hash algorithm by J. Andrew
            !  Rogers [2].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION Murmur3_Hash128_Opt(Input, InpSize, StartHash, RemoveSign, HashPair) RESULT(HashCode)
            !^ To compute hash code using the MurmurHash3 hash algorithm by Austin
            !  Appleby [1].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
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
        MODULE FUNCTION Wy_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using version 3 (?) of the WyHash hash algorithm by
            !  Wang Yi [3, 4].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION WyF3_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the final version 3 of the WyHash hash algorithm
            !  by Wang Yi [3].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION XX_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the XXHash hash algorithm by Yann Collet [5].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION XX3_Hash64_Opt(Input, InpSize, StartHash, RemoveSign, &
                                    Secret) RESULT(HashCode)
            !^ To compute hash code using the XX3Hash64 hash algorithm by Yann Collet [5].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
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
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION XX3_Hash128_Opt(Input, InpSize, StartHash, RemoveSign, &
                                     Secret, HashPair) RESULT(HashCode)
            !^ To compute hash code using the XX3Hash128 hash algorithm by Yann Collet [5].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
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
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION City_Hash64_Opt(Input, InpSize, StartHash, RemoveSign, &
                                     Seed) RESULT(HashCode)
            !^ To compute hash code using the CityHash hash algorithm by Google Inc [6].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
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
        MODULE FUNCTION FarmNa_Hash64_Opt(Input, InpSize, StartHash, RemoveSign, &
                                       Seed) RESULT(HashCode)
            !^ To compute hash code using the FarmNaHash hash algorithm by Google Inc [7].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
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
        MODULE FUNCTION FarmUo_Hash64_Opt(Input, InpSize, StartHash, RemoveSign, &
                                       Seed) RESULT(HashCode)
            !^ To compute hash code using the FarmUoHash hash algorithm by Google Inc [7].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
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
        MODULE FUNCTION Spooky_Hash128_Opt(Input, InpSize, StartHash, RemoveSign, &
                                        Seed, HashPair) RESULT(HashCode)
            !^ To compute hash code using version 2 of the SpookyHash hash algorithm
            !  by Bob Jenkins [8].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
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
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION PengyV03_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the PengyHash hash algorithm (v0.3) by
            !  Alberto Fajardo [15]. <br>
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION PengyV02_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the PengyHash hash algorithm (v0.2) by
            !  Alberto Fajardo [9, 10].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt64,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt64                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION Komi_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute hash code using the KomiHash hash algorithm by Aleksey Vaneev [13].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
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

END MODULE MBase_OptimalHash64

!******************************************************************************
