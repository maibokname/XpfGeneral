
MODULE MBase_SimpleHash64

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains (non-cryptographic) simple hash function routines that
!   output a hash value as a 64-bit integer.  All routines can be used for an
!   input (i.e. a key) of any type and rank providing that the size of the input
!   (in bytes) is known at compile time.  All routines can be used for a continued
!   hashing by providing the previously computed hash value as an (optional) input
!   argument (i.e. the *StartHash* argument).  Optionally, a user can specify
!   whether to return only positive value of the hash code.  This is particularly
!   useful when used in conjunction with a hash table. <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="http://www.partow.net/programming/hashfunctions/index.html">
!       General Purpose Hash Function Algorithms</a> by Arash Partow. <br>
!   [2] <a href="https://github.com/zmiimz/fortran_notes">Fortran Notes Library. </a> <br>
!   [3] <a href="https://dl.acm.org/doi/10.5555/523106"> Sedgewick, R. 1997. Algorithms
!        in C: Parts 1-4, Fundamentals, Data Structures, Sorting, and Searching. </a> <br>
!   [4] <a href="https://en.wikipedia.org/wiki/PJW_hash_function">PJW hash function. </a> <br>
!   [5] <a href="http://www.cse.yorku.ca/~oz/hash.html">Hash Functions. </a> <br>
!   [6] <a href="http://stackoverflow.com/questions/664014/what-integer-hash-function-are-good-that-accepts-an-integer-hash-key">
!       What integer hash function are good that accepts an integer hash key? </a> <br>
!   [7] <a href="https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function">
!       Fowler-Noll-Vo hash function. </a> <br>
!   [8] <a href="https://en.wikipedia.org/wiki/Jenkins_hash_function">Jenkins hash function. </a> <br>

!** USE STATEMENTS:
    USE ISO_C_BINDING,      ONLY: C_SIZEOF
    USE MBase_Common
    USE MBase_ByteUtil

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! procedures
    PUBLIC :: Hash64_RS
    PUBLIC :: Hash64_JS
    PUBLIC :: Hash64_PJW
    PUBLIC :: Hash64_DJB
    PUBLIC :: Hash64_SDBM
    PUBLIC :: Hash64_DEK
    PUBLIC :: Hash64_AP
    PUBLIC :: Hash64_TM
    PUBLIC :: Hash64_FNV1
    PUBLIC :: Hash64_FNV1a
    PUBLIC :: Hash64_OAT

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#undef  tHash
#define tHash           tUInt64

!** MODULE PARAMETERS:
    ! module name
    tCharStar, PARAMETER    :: ModName = 'ModBase_SimpleHash64'
    ! kind of hash code
    tUInt32,   PARAMETER    :: kHash = kInt64
    ! The number of bits of hash code
    tHash,     PARAMETER    :: HashBits = BIT_SIZE(0_kHash)     ! should be 64 bits
    ! The maximum (positive) number of hash code
    tHash,     PARAMETER    :: MaxHash  = INT(Z'7FFFFFFFFFFFFFFF', kHash)

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        PURE FUNCTION GetKey(KeyByte) RESULT(KeyNum)
            !^ To convert an 8-bit integer to an integer number (kind=kHash)
            IMPORT
            tUInt8, INTENT(IN)  :: KeyByte  ! key as a byte
            tHash               :: KeyNum   ! key as an integer
        END FUNCTION GetKey
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                           HASH FUNCTIONS                                    +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION Hash64_RS(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the RS hash algorithm by Robert Sedgewick [1],
    !  which is based on the universal hash function in [3].

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Key(..)      !! key (any type and rank)
    tIndex,                 INTENT(IN)  :: KeySize      !! size of the key (in bytes)
    tHash,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tHash                               :: HashCode     !! hash code

!** SUBROUTINE PARAMETER DECLARATIONS:
    tHash, PARAMETER    :: B = 378551_kHash

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I
    tHash, SAVE                 :: A = 63689_kHash
    tUInt8,            POINTER  :: KeyPtr(:)
    PROCEDURE(GetKey), POINTER  :: GetKeyNum

!** FLOW

    ! initialize
    IF (IsLittleEndian) THEN
        GetKeyNum => Get_KeyNum_LE
    ELSE
        GetKeyNum => Get_KeyNum_BE
    END IF
    SET_OPTION(HashCode, 0_kHash, StartHash)
    IF (.NOT.PRESENT(StartHash)) A = 63689_kHash
    CALL AnyType_2_ByteArrPtr(Key, KeySize, KeyPtr)

    ! perform hashing
    DO I = 1, SIZE(KeyPtr, KIND=kIndex)
        HashCode = HashCode*A + GetKeyNum(KeyPtr(I))
        A = A*B
    END DO

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    NULLIFY(KeyPtr)
    NULLIFY(GetKeyNum)

    RETURN

END FUNCTION Hash64_RS

!******************************************************************************

FUNCTION Hash64_JS(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the JS hash algorithm by Justin Sobel [1].

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Key(..)      !! key (any type and rank)
    tIndex,                 INTENT(IN)  :: KeySize      !! size of the key (in bytes)
    tHash,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tHash                               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I
    tUInt8,            POINTER  :: KeyPtr(:)
    PROCEDURE(GetKey), POINTER  :: GetKeyNum

!** FLOW

    ! initialize
    IF (IsLittleEndian) THEN
        GetKeyNum => Get_KeyNum_LE
    ELSE
        GetKeyNum => Get_KeyNum_BE
    END IF
    SET_OPTION(HashCode, 1315423911_kHash, StartHash)
    CALL AnyType_2_ByteArrPtr(Key, KeySize, KeyPtr)

    ! perform hashing
    DO I = 1, SIZE(KeyPtr, KIND=kIndex)
        HashCode = IEOR(HashCode, SHIFTL(HashCode, 5) + GetKeyNum(KeyPtr(I)) + &
                                  SHIFTR(HashCode, 2))
    END DO

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    NULLIFY(KeyPtr)
    NULLIFY(GetKeyNum)

    RETURN

END FUNCTION Hash64_JS

!******************************************************************************

FUNCTION Hash64_PJW(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the PJW hash algorithm by Peter J. Weinberger [1, 4].

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Key(..)      !! key (any type and rank)
    tIndex,                 INTENT(IN)  :: KeySize      !! size of the key (in bytes)
    tHash,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tHash                               :: HashCode     !! hash code

!** SUBROUTINE PARAMETER DECLARATIONS:
    tHash, PARAMETER    :: OneEighthBits    = HashBits/8_kHash
    tHash, PARAMETER    :: ThreeQuatersBits = (HashBits*3_kHash)/4_kHash
    tHash, PARAMETER    :: HighBits         = SHIFTL(ToInt64(Z'FFFFFFFFFFFFFFFF'), (HashBits-OneEighthBits))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I
    tHash                       :: Test
    tUInt8,            POINTER  :: KeyPtr(:)
    PROCEDURE(GetKey), POINTER  :: GetKeyNum

!** FLOW

    ! initialize
    IF (IsLittleEndian) THEN
        GetKeyNum => Get_KeyNum_LE
    ELSE
        GetKeyNum => Get_KeyNum_BE
    END IF
    SET_OPTION(HashCode, 0_kHash, StartHash)
    CALL AnyType_2_ByteArrPtr(Key, KeySize, KeyPtr)

    ! perform hashing
    DO I = 1, SIZE(KeyPtr, KIND=kIndex)
        HashCode = SHIFTL(HashCode, OneEighthBits) + GetKeyNum(KeyPtr(I))
        Test = IAND(HashCode, HighBits)
        IF (Test /= 0_kHash) THEN
            HashCode = IAND(IEOR(HashCode, SHIFTR(Test, ThreeQuatersBits)), NOT(HighBits))
        END IF
    END DO

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    NULLIFY(KeyPtr)
    NULLIFY(GetKeyNum)

    RETURN

END FUNCTION Hash64_PJW

!******************************************************************************

FUNCTION Hash64_DJB(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the DJB2 hash algorithm by 
    !  Daniel J. Bernstein [1, 5].

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Key(..)      !! key (any type and rank)
    tIndex,                 INTENT(IN)  :: KeySize      !! size of the key (in bytes)
    tHash,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tHash                               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I
    tUInt8,            POINTER  :: KeyPtr(:)
    PROCEDURE(GetKey), POINTER  :: GetKeyNum

!** FLOW

    ! initialize
    IF (IsLittleEndian) THEN
        GetKeyNum => Get_KeyNum_LE
    ELSE
        GetKeyNum => Get_KeyNum_BE
    END IF
    SET_OPTION(HashCode, 5381_kHash, StartHash)
    CALL AnyType_2_ByteArrPtr(Key, KeySize, KeyPtr)

    ! perform hashing:  hash = ((hash << 5) + hash) ^ key
    DO I = 1, SIZE(KeyPtr, KIND=kIndex)
        HashCode = IEOR(SHIFTL(HashCode, 5) + HashCode, GetKeyNum(KeyPtr(I)))
    END DO

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    NULLIFY(KeyPtr)
    NULLIFY(GetKeyNum)

    RETURN

END FUNCTION Hash64_DJB

!******************************************************************************

FUNCTION Hash64_SDBM(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the SDBM hash algorithm [1, 5].

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Key(..)      !! key (any type and rank)
    tIndex,                 INTENT(IN)  :: KeySize      !! size of the key (in bytes)
    tHash,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tHash                               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I
    tUInt8,            POINTER  :: KeyPtr(:)
    PROCEDURE(GetKey), POINTER  :: GetKeyNum

!** FLOW

    ! initialize
    IF (IsLittleEndian) THEN
        GetKeyNum => Get_KeyNum_LE
    ELSE
        GetKeyNum => Get_KeyNum_BE
    END IF
    SET_OPTION(HashCode, 0_kHash, StartHash)
    CALL AnyType_2_ByteArrPtr(Key, KeySize, KeyPtr)

    ! perform hashing < hash = key + (hash << 6) + (hash << 16) - hash >
    DO I = 1, SIZE(KeyPtr, KIND=kIndex)
        HashCode = GetKeyNum(KeyPtr(I)) + SHIFTL(HashCode, 6) + &
                   SHIFTL(HashCode, 16) - HashCode
    END DO

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    NULLIFY(KeyPtr)
    NULLIFY(GetKeyNum)

    RETURN

END FUNCTION Hash64_SDBM

!******************************************************************************

FUNCTION Hash64_DEK(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the DEK hash algorithm by Donald E. Knuth [1].

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Key(..)      !! key (any type and rank)
    tIndex,                 INTENT(IN)  :: KeySize      !! size of the key (in bytes)
    tHash,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tHash                               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I
    tUInt8,            POINTER  :: KeyPtr(:)
    PROCEDURE(GetKey), POINTER  :: GetKeyNum

!** FLOW

    ! initialize
    IF (IsLittleEndian) THEN
        GetKeyNum => Get_KeyNum_LE
    ELSE
        GetKeyNum => Get_KeyNum_BE
    END IF
    CALL AnyType_2_ByteArrPtr(Key, KeySize, KeyPtr)
    SET_OPTION(HashCode, KeySize, StartHash)

    ! perform hashing
    DO I = 1, SIZE(KeyPtr, KIND=kIndex)
        HashCode = IEOR(IEOR(SHIFTL(HashCode, 5), SHIFTR(HashCode, 27)), GetKeyNum(KeyPtr(I)))
    END DO

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    NULLIFY(KeyPtr)
    NULLIFY(GetKeyNum)

    RETURN

END FUNCTION Hash64_DEK

!******************************************************************************

FUNCTION Hash64_AP(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the AP hash algorithm by Arash Partow [1].

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Key(..)      !! key (any type and rank)
    tIndex,                 INTENT(IN)  :: KeySize      !! size of the key (in bytes)
    tHash,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tHash                               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I
    tUInt8,            POINTER  :: KeyPtr(:)
    PROCEDURE(GetKey), POINTER  :: GetKeyNum

!** FLOW

    ! initialize
    IF (IsLittleEndian) THEN
        GetKeyNum => Get_KeyNum_LE
    ELSE
        GetKeyNum => Get_KeyNum_BE
    END IF
    SET_OPTION(HashCode, ToInt64(Z'00000000AAAAAAAA'), StartHash)
    CALL AnyType_2_ByteArrPtr(Key, KeySize, KeyPtr)

    ! perform hashing
    DO I = 1_kIndex, SIZE(KeyPtr, KIND=kIndex)
        IF (IAND(I-1_kIndex, 1_kIndex) == 0_kIndex) THEN
            HashCode = IEOR(HashCode, IEOR(SHIFTL(HashCode, 7), &
                                           GetKeyNum(KeyPtr(I))*SHIFTR(HashCode, 3)))
        ELSE
            HashCode = IEOR(HashCode, NOT(SHIFTL(HashCode, 11) + &
                                          IEOR(GetKeyNum(KeyPtr(I)), SHIFTR(HashCode, 5))))
        END IF
    END DO

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    NULLIFY(KeyPtr)
    NULLIFY(GetKeyNum)

    RETURN

END FUNCTION Hash64_AP

!******************************************************************************

FUNCTION Hash64_TM(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the TM hash algorithm by Thomas Mueller [6].

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Key(..)      !! key (any type and rank)
    tIndex,                 INTENT(IN)  :: KeySize      !! size of the key (in bytes)
    tHash,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tHash                               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I
    tUInt8,            POINTER  :: KeyPtr(:)
    PROCEDURE(GetKey), POINTER  :: GetKeyNum

!** FLOW

    ! initialize
    IF (IsLittleEndian) THEN
        GetKeyNum => Get_KeyNum_LE
    ELSE
        GetKeyNum => Get_KeyNum_BE
    END IF
    SET_OPTION(HashCode, 0_kHash, StartHash)
    CALL AnyType_2_ByteArrPtr(Key, KeySize, KeyPtr)

    ! perform hashing
    DO I = 1, SIZE(KeyPtr, KIND=kIndex)
        HashCode = HashCode + GetKeyNum(KeyPtr(I))
        HashCode = IEOR(SHIFTR(HashCode, 30), HashCode) * ToInt64(Z'BF58476D1CE4E5B9')
        HashCode = IEOR(SHIFTR(HashCode, 27), HashCode) * ToInt64(Z'94D049BB133111EB')
        HashCode = IEOR(SHIFTR(HashCode, 31), HashCode)
    END DO

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    NULLIFY(KeyPtr)
    NULLIFY(GetKeyNum)

    RETURN

END FUNCTION Hash64_TM

!******************************************************************************

FUNCTION Hash64_FNV1(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the FNV-1 hash algorithm by Glenn Fowler,
    !  Landon Curt Noll, and Phong Vo [7].

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Key(..)      !! key (any type and rank)
    tIndex,                 INTENT(IN)  :: KeySize      !! size of the key (in bytes)
    tHash,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tHash                               :: HashCode     !! hash code

!** SUBROUTINE PARAMETER DECLARATIONS:
    tHash, PARAMETER    :: Offset_Basic = INT(Z'CBF29CE484222325', kHash)
    tHash, PARAMETER    :: Prime        = INT(Z'00000100000001B3', kHash)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I
    tUInt8,            POINTER  :: KeyPtr(:)
    PROCEDURE(GetKey), POINTER  :: GetKeyNum

!** FLOW

    ! initialize
    IF (IsLittleEndian) THEN
        GetKeyNum => Get_KeyNum_LE
    ELSE
        GetKeyNum => Get_KeyNum_BE
    END IF
    SET_OPTION(HashCode, Offset_Basic, StartHash)
    CALL AnyType_2_ByteArrPtr(Key, KeySize, KeyPtr)

    ! perform hashing
    DO I = 1, SIZE(KeyPtr, KIND=kIndex)
        HashCode = IEOR(HashCode*Prime, GetKeyNum(KeyPtr(I)))
    END DO

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    NULLIFY(KeyPtr)
    NULLIFY(GetKeyNum)

    RETURN

END FUNCTION Hash64_FNV1

!******************************************************************************

FUNCTION Hash64_FNV1a(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the FNV-1a hash algorithm by Glenn Fowler,
    !  Landon Curt Noll, and Phong Vo [7].

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Key(..)      !! key (any type and rank)
    tIndex,                 INTENT(IN)  :: KeySize      !! size of the key (in bytes)
    tHash,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tHash                               :: HashCode     !! hash code

!** SUBROUTINE PARAMETER DECLARATIONS:
    tHash, PARAMETER    :: Offset_Basic = INT(Z'CBF29CE484222325', kHash)
    tHash, PARAMETER    :: Prime        = INT(Z'00000100000001B3', kHash)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I
    tUInt8,            POINTER  :: KeyPtr(:)
    PROCEDURE(GetKey), POINTER  :: GetKeyNum

!** FLOW

    ! initialize
    IF (IsLittleEndian) THEN
        GetKeyNum => Get_KeyNum_LE
    ELSE
        GetKeyNum => Get_KeyNum_BE
    END IF
    SET_OPTION(HashCode, Offset_Basic, StartHash)
    CALL AnyType_2_ByteArrPtr(Key, KeySize, KeyPtr)

    ! perform hashing
    DO I = 1, SIZE(KeyPtr, KIND=kIndex)
        HashCode = IEOR(HashCode, GetKeyNum(KeyPtr(I)))*Prime
    END DO

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    NULLIFY(KeyPtr)
    NULLIFY(GetKeyNum)

    RETURN

END FUNCTION Hash64_FNV1a

!******************************************************************************

FUNCTION Hash64_OAT(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the OAT (One-At-a-Time) hash algorithm by
    !  Bob Jenkins [8].

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*),  CONTIGUOUS,   INTENT(IN)  :: Key(..)      !! key (any type and rank)
    tIndex,                 INTENT(IN)  :: KeySize      !! size of the key (in bytes)
    tHash,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tHash                               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: I
    tUInt8,            POINTER  :: KeyPtr(:)
    PROCEDURE(GetKey), POINTER  :: GetKeyNum

!** FLOW

    ! initialize
    IF (IsLittleEndian) THEN
        GetKeyNum => Get_KeyNum_LE
    ELSE
        GetKeyNum => Get_KeyNum_BE
    END IF
    SET_OPTION(HashCode, 0_kHash, StartHash)
    CALL AnyType_2_ByteArrPtr(Key, KeySize, KeyPtr)

    ! perform hashing
    DO I = 1, SIZE(KeyPtr, KIND=kIndex)
        HashCode = HashCode + GetKeyNum(KeyPtr(I))
        HashCode = HashCode + SHIFTL(HashCode, 10)
        HashCode = IEOR(HashCode, SHIFTR(HashCode, 6))
    END DO
    HashCode = HashCode + SHIFTL(HashCode, 3)
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 11))
    HashCode = HashCode + SHIFTL(HashCode, 15)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    NULLIFY(KeyPtr)
    NULLIFY(GetKeyNum)

    RETURN

END FUNCTION Hash64_OAT

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                               AUXILIARY ROUTINES                            +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION Get_KeyNum_LE(KeyByte) RESULT(KeyNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an 8-bit integer to an integer number where
    !  system endianess is in little-endian order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: KeyByte  !! key as a byte
    tHash               :: KeyNum   !! key as an integer (kind=kHash)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    KeyNum = IAND(ToInt64(KeyByte), ToInt64(Z'00000000000000FF'))

    ! alternative implementation
    !KeyNum = TRANSFER([KeyByte, 0_kInt8, 0_kInt8, 0_kInt8,  &
    !                   0_kInt8, 0_kInt8, 0_kInt8, 0_kInt8], 0_kHash)

    RETURN

END FUNCTION Get_KeyNum_LE

!******************************************************************************

PURE FUNCTION Get_KeyNum_BE(KeyByte) RESULT(KeyNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an 8-bit integer to an integer number where
    !  system endianess is in big-endian order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: KeyByte  !! key as a byte
    tHash               :: KeyNum   !! key as an integer (kind=kHash)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    KeyNum = SHIFTL(IAND(ToInt64(KeyByte), ToInt64(Z'00000000000000FF')), 56)

    ! alternative implementation
    !KeyNum = TRANSFER([0_kInt8, 0_kInt8, 0_kInt8, KeyByte], 0_kHash)

    RETURN

END FUNCTION Get_KeyNum_BE

!******************************************************************************

PURE FUNCTION Get_KeyNum(KeyByte) RESULT(KeyNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an integer byte to an integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: KeyByte  !! key as a byte
    tHash               :: KeyNum   ! key number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check endianess
    IF (IsLittleEndian) THEN
        KeyNum = TRANSFER([KeyByte, 0_kInt8, 0_kInt8, 0_kInt8,  &
                            0_kInt8, 0_kInt8, 0_kInt8, 0_kInt8], 0_kHash)
    ELSE
        KeyNum = TRANSFER([0_kInt8, 0_kInt8, 0_kInt8, 0_kInt8,  &
                            0_kInt8, 0_kInt8, 0_kInt8, KeyByte], 0_kHash)
    END IF

    RETURN

END FUNCTION Get_KeyNum

!******************************************************************************

END MODULE MBase_SimpleHash64

#undef tHash
#undef kHash

!******************************************************************************
