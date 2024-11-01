
MODULE MBase_OptimalHash32

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains an optimal implementation of various non-cryptographic hash
!   function routines that output a hash value as a 32-bit integer.  The available
!   hash functions are a subset of those reference hash functions implemented in the
!   <a href="../module/mbase_referencehash32.html">ModBase_ReferenceHash32</a> module.
!   The API of these optimal routines are exactly the same as those reference routines. <br>
!   It should be noted that each optimal hash function routine is based on a benchmark
!   that compares performances of various possible implementations of the hash function
!   as provided in the  <a href="../module/mbase_experimentalhash32.html">
!   ModBase_ExperimentalHash32</a> module.  It should also be noted that these so-called
!   optimal hash function routines may not actually be optimal for a particular user so
!   the user is highly encouraged to perform (by himself/herself) a benchmark of each
!   specific hash function routine in order to know which implementation is the best one
!   for a particular system (i.e. a combination of operating system, machine as well as
!   compiler used). <br>
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
    PUBLIC :: City_Hash32_Opt
    PUBLIC :: FarmMk_Hash32_Opt
    PUBLIC :: Lookup3_Hash32_Opt
    PUBLIC :: Murmur3_Hash32_Opt
    PUBLIC :: NM_Hash32_Opt
    PUBLIC :: NMx_Hash32_Opt
    PUBLIC :: Water_Hash32_Opt
    PUBLIC :: XX_Hash32_Opt

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'ModBase_OpterimentalHash32'
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
        MODULE FUNCTION Lookup3_Hash32_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the Lookup3 hash algorithm by Bob Jenkins [2].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION Murmur3_Hash32_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the MurmurHash3 hash algorithm by Austin
            !  Appleby [1].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION City_Hash32_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the City hash algorithm by Google Inc [3].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION FarmMk_Hash32_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the FarmMk hash algorithm by Google Inc [4].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION NM_Hash32_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using version 2 of the NMHASH hash algorithm
            !  by James Z. M. Gao [7].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION NMx_Hash32_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using version 2 of the NMxHASH hash algorithm by
            !  James Z. M. Gao [7].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION XX_Hash32_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using the XXHash hash algorithm by Yann Collet [8].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
        MODULE FUNCTION Water_Hash32_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            !^ To compute the hash value using a new version of the WaterHash hash
            !  algorithm by Tommy Ettinger [11].
            TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Input(..)    !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
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

!******************************************************************************

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

END MODULE MBase_OptimalHash32

!******************************************************************************
