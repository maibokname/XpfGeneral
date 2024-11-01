
MODULE MBase_SimpleHash32

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains (non-cryptographic) simple hash function routines that
!   output a hash value as a 32-bit integer.  All routines can be used for an
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
    PUBLIC :: Hash32_RS
    PUBLIC :: Hash32_JS
    PUBLIC :: Hash32_PJW
    PUBLIC :: Hash32_ELF
    PUBLIC :: Hash32_DJB
    PUBLIC :: Hash32_SDBM
    PUBLIC :: Hash32_DEK
    PUBLIC :: Hash32_AP
    PUBLIC :: Hash32_TM
    PUBLIC :: Hash32_FNV1
    PUBLIC :: Hash32_FNV1a
    PUBLIC :: Hash32_OAT

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#undef  tHash
#define tHash           tUInt32

!** MODULE PARAMETERS:
    ! module name
    tCharStar, PARAMETER    :: ModName = 'ModBase_SimpleHash32'
    ! kind of hash code
    tUInt32,   PARAMETER    :: kHash = kInt32
    ! The number of bits of hash code
    tHash,     PARAMETER    :: HashBits = BIT_SIZE(0_kHash)     ! should be 32 bits
    ! The maximum (positive) number of hash code
    tHash,     PARAMETER    :: MaxHash  = INT(Z'7FFFFFFF', kHash)

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
    TYPE DUMMY
        tUInt32         :: A
        tRealDP         :: B
        tCharLen(72)    :: C
        tLogical        :: D
        tCmpxQP         :: F
    END TYPE

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION Hash32_RS(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

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

END FUNCTION Hash32_RS

!******************************************************************************

FUNCTION Hash32_JS(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

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

END FUNCTION Hash32_JS

!******************************************************************************

FUNCTION Hash32_PJW(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

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
    tHash, PARAMETER    :: HighBits         = SHIFTL(ToInt32(Z'FFFFFFFF'), (HashBits-OneEighthBits))

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

END FUNCTION Hash32_PJW

!******************************************************************************

FUNCTION Hash32_ELF(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the PJW hash algorithm (tweaked for
    !  32-bit processors) as implemented on UNIX-based systems.

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
    tHash                       :: High
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
        HashCode = SHIFTL(HashCode, 4) + GetKeyNum(KeyPtr(I))
        High = IAND(HashCode, Z'F0000000')
        IF (High /= 0_kHash) HashCode = IEOR(HashCode, SHIFTR(High, 24))
        HashCode = IAND(HashCode, NOT(High))
    END DO

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    NULLIFY(KeyPtr)
    NULLIFY(GetKeyNum)

    RETURN

END FUNCTION Hash32_ELF

!******************************************************************************

FUNCTION Hash32_DJB(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

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

END FUNCTION Hash32_DJB

!******************************************************************************

FUNCTION Hash32_SDBM(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

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

END FUNCTION Hash32_SDBM

!******************************************************************************

FUNCTION Hash32_DEK(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

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

END FUNCTION Hash32_DEK

!******************************************************************************

FUNCTION Hash32_AP(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

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
    SET_OPTION(HashCode, ToInt32(Z'AAAAAAAA'), StartHash)
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

END FUNCTION Hash32_AP

!******************************************************************************

FUNCTION Hash32_TM(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

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
        HashCode = IEOR(SHIFTR(HashCode, 16), HashCode) * ToInt32(Z'45D9F3B')
        HashCode = IEOR(SHIFTR(HashCode, 16), HashCode) * ToInt32(Z'45D9F3B')
        HashCode = IEOR(SHIFTR(HashCode, 16), HashCode)
    END DO

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    NULLIFY(KeyPtr)
    NULLIFY(GetKeyNum)

    RETURN

END FUNCTION Hash32_TM

!******************************************************************************

FUNCTION Hash32_FNV1(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

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
    tHash, PARAMETER    :: Offset_Basic = INT(Z'811C9DC5', kHash)
    tHash, PARAMETER    :: Prime        = INT(Z'01000193', kHash)

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

END FUNCTION Hash32_FNV1

!******************************************************************************

FUNCTION Hash32_FNV1a(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

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
    tHash, PARAMETER    :: Offset_Basic = INT(Z'811C9DC5', kHash)
    tHash, PARAMETER    :: Prime        = INT(Z'01000193', kHash)

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

END FUNCTION Hash32_FNV1a

!******************************************************************************

FUNCTION Hash32_OAT(Key, KeySize, StartHash, RemoveSign) RESULT(HashCode)

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

END FUNCTION Hash32_OAT

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

    KeyNum = IAND(ToInt32(KeyByte), ToInt32(Z'000000FF'))

    ! alternative implementation
    !KeyNum = TRANSFER([KeyByte, 0_kInt8, 0_kInt8, 0_kInt8], 0_kHash)

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

    KeyNum = SHIFTL(IAND(ToInt32(KeyByte), ToInt32(Z'000000FF')), 24)

    ! alternative implementation
    !KeyNum = TRANSFER([0_kInt8, 0_kInt8, 0_kInt8, KeyByte], 0_kHash)

    RETURN

END FUNCTION Get_KeyNum_BE

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                           TESTING HASH FUNCTIONS                            +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION Test_Hash32_RS_1(Key, StartHash) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ The RS hash algorithm from "Algorithms in C" book by Robert Sedgwicks.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32,         INTENT(IN) :: Key          !! key
    tHash, OPTIONAL, INTENT(IN) :: StartHash    !! optional starting hash for continued hashing
    tHash                       :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: KeySize

!** FLOW

    KeySize  = C_SIZEOF(Key)
    HashCode = Hash32_RS(Key, KeySize, StartHash)

    RETURN

END FUNCTION Test_Hash32_RS_1

!******************************************************************************

FUNCTION Test_Hash32_RS_2(Key, StartHash) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ The RS hash algorithm from "Algorithms in C" book by Robert Sedgwicks.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32,         INTENT(IN) :: Key(:)       !! key
    tHash, OPTIONAL, INTENT(IN) :: StartHash    !! optional starting hash for continued hashing
    tHash                       :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: KeySize

!** FLOW

    KeySize  = C_SIZEOF(Key(1))*SIZE(Key)
    HashCode = Hash32_RS(Key, KeySize, StartHash)

    RETURN

END FUNCTION Test_Hash32_RS_2

!******************************************************************************

FUNCTION Test_Hash32_RS_3(Key, StartHash) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ The RS hash algorithm from "Algorithms in C" book by Robert Sedgwicks.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharLen(72),    INTENT(IN) :: Key(:,:)     !! key
    tHash, OPTIONAL, INTENT(IN) :: StartHash    !! optional starting hash for continued hashing
    tHash                       :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: KeySize

!** FLOW

    !KeySize  = C_SIZEOF(Key(1:SIZE(Key,DIM=1),1:SIZE(Key,DIM=2)))
    KeySize  = C_SIZEOF(Key(1,1)(1:1))*SIZE(Key,DIM=1)*SIZE(Key,DIM=2)*LEN(Key(1,1))
    HashCode = Hash32_RS(Key, KeySize, StartHash)

    RETURN

END FUNCTION Test_Hash32_RS_3

!******************************************************************************

FUNCTION Test_Hash32_RS_4(Key, Length, StartHash) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ The RS hash algorithm from "Algorithms in C" book by Robert Sedgwicks.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealQP,         INTENT(IN) :: Key(*)       !! key
    tIndex,          INTENT(IN) :: Length
    tHash, OPTIONAL, INTENT(IN) :: StartHash    !! optional starting hash for continued hashing
    tHash                       :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: KeySize

!** FLOW

    KeySize  = Length*8
    HashCode = Hash32_RS(Key, KeySize, StartHash)

    RETURN

END FUNCTION Test_Hash32_RS_4

!******************************************************************************

FUNCTION Test_Hash32_RS_5(Key, StartHash) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ The RS hash algorithm from "Algorithms in C" book by Robert Sedgwicks.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(DUMMY),     INTENT(IN) :: Key          !! key
    tHash, OPTIONAL, INTENT(IN) :: StartHash    !! optional starting hash for continued hashing
    tHash                       :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: KeySize

!** FLOW

!    KeySize  = SIZEOF(Key)
    KeySize  = SHIFTR(STORAGE_SIZE(Key), 3)
    HashCode = Hash32_RS(Key, KeySize, StartHash)

    RETURN

END FUNCTION Test_Hash32_RS_5

!******************************************************************************

#undef tHash
#undef kHash

END MODULE MBase_SimpleHash32

!******************************************************************************
