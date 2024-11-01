
MODULE MClass_FarmNaHasher64

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *FarmNaHasher64* type and its related routines.
!   The *FarmNaHasher64* type is a hasher type that extends directly from the
!   <a href="../module/mclass_hasher64.html#type-hasher64">Hasher64</a> type.
!   It provides all deferred procedures required by a *Hasher64* class and
!   outputs the hash value as a 64-bit integer. <br>
!   The *FarmNaHasher64* type employs the *FarmNa* hash algorithm for 64-bit integer
!   output by Google Inc [1, 2].  As a hasher, it can be used to compute the
!   hash value incrementally.  It also provides a method to compute the hash
!   value directly (i.e. non-incrementally).  The following code snippet shows
!   a typical usage of the hasher.
!   <Pre><Code style="color:MidnightBlue;">
!   ! first, initialize the hasher (once)
!   CALL Hasher%Initialize(Seed)
!   ! then, put data into the hasher (a number of times)
!   CALL Hasher%Update(Input, InpSize)
!               ...
!               ...
!               ...
!   ! finally, get the hash value from the hasher (once)
!   HashCode = Hasher%Finalize()
!   </Code></Pre>
!   However, if the *Update* method is to be called only one time, then the
!   *HashDirect* method should be employed as follows.
!   <Pre><Code style="color:MidnightBlue;">
!   ! directly compute the hash value
!   HashCode = Hasher%HashDirect(Input, InpSize, Seed)
!   </Code></Pre>
!^ **REFERENCES**: <br>
!   [1] <a href="https://github.com/google/farmhash">FarmHash: a family of hash functions. </a> <br>
!   [2] <a href="https://github.com/OpenHFT/Zero-Allocation-Hashing/tree/ea">
!       Zero-Allocation Hashing for Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_Hasher64

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: FarmNaHasher64

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define ShiftMix(V)             IEOR(V, SHIFTR(V, 47))
#define HashLen16_3(U,V,M)      ShiftMix(IEOR(V, ShiftMix(IEOR(U, V)*M))*M)*M
#define HashLen16_2(U,V)        HashLen16_3(U, V, K_MUL)
#define Mul(L)                  K2 + SHIFTL(ToInt64(L), 1)
#define MaskI8(X)               IAND(ToInt64(X), Z'00000000000000FF')
#define MaskI32(X)              IAND(ToInt64(X), Z'00000000FFFFFFFF')
#define Pack_U32(Buf,Index)     MaskI32(PackInteger(Buf, Index))

!** MODULE PARAMETERS:
    tUInt64, PARAMETER  :: K0    = ToInt64(Z'C3A5C85C97CB3127')
    tUInt64, PARAMETER  :: K1    = ToInt64(Z'B492B66FBE98F273')
    tUInt64, PARAMETER  :: K2    = ToInt64(Z'9AE16A3B2F90404F')
    tUInt64, PARAMETER  :: K_MUL = ToInt64(Z'9DDFEA08EB382D69')
    tIndex,  PARAMETER  :: BlockLen = 64_kIndex

!** DERIVED TYPE DEFINITIONS
    !> *FarmNaHasher64* is a hasher type that outputs the hash value as a 64-bit integer.
    !  It employs the *FarmNa* hash algorithm by Google Inc.
    TYPE, EXTENDS(Hasher64) :: FarmNaHasher64
        PRIVATE
        !% seeds
        tUInt64     :: Seed(2)              = 0_kInt64
        !% state
        tUInt64     :: State(7)             = 0_kInt64
        !% buffer array used to store input data
        tUInt8      :: BufArr(0:BlockLen-1) = 0_kInt8
        !% flag indicating whether to remove sign from the final hash value
        tLogical    :: RemoveSign   = FalseVal
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName          => FarmNa_GetName
        !> *GetBlockLength* is a procedure to return the block length of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockLength   => FarmNa_BlockLength
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: SetBufPtr        => FarmNa_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: ProcessBlock     => FarmNa_ProcessBlock
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the hasher with one seed. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Initialize(Seed)            ! hash value with sign <br>
        !   --->    CALL Hasher%Initialize(Seed, .TRUE.)    ! remove sign from hash value <br>
        PROCEDURE   :: Initialize       => FarmNa_Initialize
        !> **Type-Bound Subroutine**: InitializeWSeeds <br>
        !  **Purpose**:  To initialize the hasher with two seeds. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%InitializeWSeeds(Seed1, Seed2)          ! hash value with sign <br>
        !   --->    CALL Hasher%InitializeWSeeds(Seed1, Seed2, .TRUE.)  ! remove sign from hash value <br>
        PROCEDURE   :: InitializeWSeeds => FarmNa_Initialize_WithSeeds
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 64-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize         => FarmNa_Finalize
        !> **Type-Bound Function**: HashDirect <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally)
        !                without seed or with one seed. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect       => FarmNa_HashDirect
        !> **Type-Bound Function**: HashDirectWSeeds <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally)
        !                with two seeds. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirectWSeeds(Input, InpSize, Seed1, Seed2) <br>
        !   --->    HashCode = Hasher%HashDirectWSeeds(Input, InpSize, Seed1, Seed2, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirectWSeeds => FarmNa_HashDirect_WithSeeds
    END TYPE FarmNaHasher64

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION FarmNa_GetName(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FarmNaHasher64), INTENT(IN)   :: HS   !! a hasher (HS) object
    tCharAlloc                          :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Name = 'FarmNa_Hahser64'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION FarmNa_GetName

!******************************************************************************

FUNCTION FarmNa_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FarmNaHasher64), INTENT(IN)   :: HS       !! a hasher (HS) object
    tIndex                              :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Length = BlockLen
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION FarmNa_BlockLength

!******************************************************************************

SUBROUTINE FarmNa_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FarmNaHasher64), TARGET, INTENT(INOUT)    :: HS           !! a hasher (HS) object
    tUInt8,               POINTER, INTENT(INOUT)    :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => HS%BufArr

    RETURN

END SUBROUTINE FarmNa_SetBufPtr

!******************************************************************************

SUBROUTINE FarmNa_ProcessBlock(HS, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FarmNaHasher64), INTENT(INOUT)    :: HS           !! a hasher (HS) object
    tUInt8,                INTENT(IN)       :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64 :: Tmp
    tUInt64 :: A1, B1, C1, Z1
    tUInt64 :: A2, B2, C2, Z2

!** FLOW

    ASSOCIATE (X => HS%State(1), Y => HS%State(2), Z => HS%State(3), &
               V1 => HS%State(4), V2 => HS%State(5), &
               W1 => HS%State(6), W2 => HS%State(7))

        IF (HS%GetBlockCount() == 0_kIndex) X = X*K2 + PackLong(BytesIn, 0_kIndex)

        X = RotateRight(X + Y + V1 + PackLong(BytesIn, 8_kIndex), 37)*K1
        Y = RotateRight(Y + V2 + PackLong(BytesIn, 48_kIndex), 42)*K1
        X = IEOR(X, W2)
        Y = Y + V1 + PackLong(BytesIn, 40_kIndex)
        Z = RotateRight(Z + W1, 33)*K1
        ! WeakHashLen32WithSeeds
        A1 = V2*K1
        B1 = X + W1
        Z1 = PackLong(BytesIn, 24_kIndex)
        A1 = A1 + PackLong(BytesIn, 0_kIndex)
        B1 = RotateRight(B1 + A1 + Z1, 21)
        C1 = A1
        A1 = A1 + PackLong(BytesIn, 8_kIndex)
        A1 = A1 + PackLong(BytesIn, 16_kIndex)
        B1 = B1 + RotateRight(A1, 44)
        V1 = A1 + Z1
        V2 = B1 + C1
        ! WeakHashLen32WithSeeds
        A2 = Z + W2
        B2 = Y + PackLong(BytesIn, 16_kIndex)
        Z2 = PackLong(BytesIn, 56_kIndex)
        A2 = A2 + PackLong(BytesIn, 32_kIndex)
        B2 = RotateRight(B2 + A2 + Z2, 21)
        C2 = A2
        A2 = A2 + PackLong(BytesIn, 40_kIndex)
        A2 = A2 + PackLong(BytesIn, 48_kIndex)
        B2 = B2 + RotateRight(A2, 44)
        W1 = A2 + Z2
        W2 = B2 + C2
        ! swap Z and X
        Tmp = Z
        Z = X
        X = Tmp

    END ASSOCIATE

    RETURN

END SUBROUTINE FarmNa_ProcessBlock

!******************************************************************************

SUBROUTINE FarmNa_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FarmNaHasher64), TARGET, INTENT(INOUT)    :: HS   !! a hasher (HS) object
    tUInt64,                       INTENT(IN)       :: Seed !! seed
    tLogical,            OPTIONAL, INTENT(IN)       :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL HS%InitializeWSeeds(Seed, K2, RemoveSign)

    RETURN

END SUBROUTINE FarmNa_Initialize

!******************************************************************************

SUBROUTINE FarmNa_Initialize_WithSeeds(HS, Seed1, Seed2, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher with two seeds.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FarmNaHasher64), TARGET, INTENT(INOUT)    :: HS       !! a hasher (HS) object
    tUInt64,                       INTENT(IN)       :: Seed1    !! seed1
    tUInt64,                       INTENT(IN)       :: Seed2    !! seed2
    tLogical,            OPTIONAL, INTENT(IN)       :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    HS%Seed(1) = Seed1
    HS%Seed(2) = Seed2
    ASSOCIATE (X => HS%State(1), Y => HS%State(2), Z => HS%State(3), &
               V1 => HS%State(4), V2 => HS%State(5), &
               W1 => HS%State(6), W2 => HS%State(7))
        X = 81_kInt64
        Y = 81_kInt64*K1 + 113_kInt64
        Z = ShiftMix(Y*K2 + 113_kInt64)*K2
        V1 = 0_kInt64
        V2 = 0_kInt64
        W1 = 0_kInt64
        W2 = 0_kInt64
    END ASSOCIATE
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE FarmNa_Initialize_WithSeeds

!******************************************************************************

FUNCTION FarmNa_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 64-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FarmNaHasher64), INTENT(INOUT)    :: HS       !! a hasher (HS) object
    tUInt64                                 :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length

!** FLOW

    IF (HS%GetBlockCount() > 0_kIndex) THEN
        ! compute length
        Length = HS%GetBlockCount()*HS%GetBlockLength() + HS%GetBufLen()
        BLOCK
            ! block variables
            tUInt8      :: LastBuf(0:BlockLen-1)
            tIndex      :: I, J
            tUInt64     :: Tmp, M
            tUInt64     :: A1, B1, C1, Z1
            tUInt64     :: A2, B2, C2, Z2
            ! copy buffer data for last round
            J = HS%GetBufLen() + 1_kIndex
            DO I = 0_kIndex, BlockLen-1_kIndex
                LastBuf(I) = HS%BufArr(J)
                J = J + 1_kIndex
                IF (J >= BlockLen) J = 0_kIndex
            END DO
            ASSOCIATE (X => HS%State(1), Y => HS%State(2), Z => HS%State(3), &
                       V1 => HS%State(4), V2 => HS%State(5), &
                       W1 => HS%State(6), W2 => HS%State(7))
                M = K1 + SHIFTL(IAND(Z, ToInt64(Z'00000000000000FF')), 1)
                W1 = W1 + ToInt64(IAND((Length - 1), 63_kIndex))
                V1 = V1 + W1
                W1 = W1 + V1
                X = RotateRight(X + Y + V1 + PackLong(LastBuf, 8_kIndex), 37)*M
                Y = RotateRight(Y + V2 + PackLong(LastBuf, 48_kIndex), 42)*M
                X = IEOR(X, W2*9_kInt64)
                Y = Y + V1*9_kInt64 + PackLong(LastBuf, 40_kIndex)
                Z = RotateRight(Z + W1, 33)*M
                ! WeakHashLen32WithSeeds
                A1 = V2*M
                B1 = X + W1
                Z1 = PackLong(LastBuf, 24_kIndex)
                A1 = A1 + PackLong(LastBuf, 0_kIndex)
                B1 = RotateRight(B1 + A1 + Z1, 21)
                C1 = A1
                A1 = A1 + PackLong(LastBuf, 8_kIndex)
                A1 = A1 + PackLong(LastBuf, 16_kIndex)
                B1 = B1 + RotateRight(A1, 44)
                V1 = A1 + Z1
                V2 = B1 + C1
                ! WeakHashLen32WithSeeds
                A2 = Z + W2
                B2 = Y + PackLong(LastBuf, 16_kIndex)
                Z2 = PackLong(LastBuf, 56_kIndex)
                A2 = A2 + PackLong(LastBuf, 32_kIndex)
                B2 = RotateRight(B2 + A2 + Z2, 21)
                C2 = A2
                A2 = A2 + PackLong(LastBuf, 40_kIndex)
                A2 = A2 + PackLong(LastBuf, 48_kIndex)
                B2 = B2 + RotateRight(A2, 44)
                W1 = A2 + Z2
                W2 = B2 + C2
                ! swap Z and X
                Tmp = Z
                Z = X
                X = Tmp
                HashCode = HashLen16_3(HashLen16_3(V1, W1, M) + ShiftMix(Y)*K0 + Z, \
                                       HashLen16_3(V2, W2, M) + X, M)
            END ASSOCIATE
        END BLOCK
    ELSE
        Length = HS%GetBufLen()
        ASSOCIATE (Input => HS%BufArr)
            IF (Length <= 16) THEN
                ! FarmNaHash_Len0To16
                IF (Length >= 8) THEN
                    BLOCK
                        tUInt64 :: M, A, C, D
                        tUInt64 :: First8Bytes, Last8Bytes
                        First8Bytes = PackLong(Input, 0_kIndex)
                        Last8Bytes  = PackLong(Input, Length - 8)
                        ! Hash 8 To 16 Bytes
                        M = Mul(Length)
                        A = First8Bytes + K2
                        C = RotateRight(Last8Bytes, 37)*M + A
                        D = (RotateRight(A, 25) + Last8Bytes)*M
                        HashCode = HashLen16_3(C, D, M)
                    END BLOCK
                ELSEIF (Length >= 4) THEN
                    BLOCK
                        tUInt64 :: M, First4Bytes, Last4Bytes
                        First4Bytes = Pack_U32(Input, 0_kIndex)
                        Last4Bytes  = Pack_U32(Input, Length - 4)
                        ! Hash 4 To 7 Bytes
                        M = Mul(Length)
                        HashCode = HashLen16_3(ToInt64(Length) + SHIFTL(First4Bytes, 3), Last4Bytes, M)
                    END BLOCK
                ELSEIF (Length > 0) THEN
                    BLOCK
                        tUInt32     :: FirstByte, MidOrLastByte, LastByte
                        tUInt32     :: Y, Z
                        FirstByte     = MaskI8(Input(0))
                        MidOrLastByte = MaskI8(Input(SHIFTA(Length, 1)))
                        LastByte      = MaskI8(Input(Length - 1))
                        ! Hash 1 To 3 Bytes
                        Y = FirstByte + SHIFTL(MidOrLastByte, 8)
                        Z = ToInt32(Length) + SHIFTL(LastByte, 2)
                        HashCode = ShiftMix(IEOR(ToInt64(Y)*K2, ToInt64(Z)*K0))*K2
                    END BLOCK
                ELSE
                    HashCode = K2
                END IF
            ELSEIF (Length <= 32) THEN
                ! FarmNaHash_Len17To32
                BLOCK
                    tUInt64 :: M, A, B, C, D
                    ! perform hashing
                    M = Mul(Length)
                    A = PackLong(Input, 0_kIndex)*K1
                    B = PackLong(Input, 8_kIndex)
                    C = PackLong(Input, Length - 8)*M
                    D = PackLong(Input, Length - 16)*K2
                    HashCode = HashLen16_3(RotateRight(A+B, 43) + RotateRight(C, 30) + D, \
                                           A + RotateRight(B+K2, 18) + C, M)
                END BLOCK
            ELSE
                ! FarmNaHash_Len33To64
                BLOCK
                    tUInt64     :: M, A, B, C, D, E, F, G, H
                    tUInt64     :: Y, Z
                    ! perform hashing
                    M = Mul(Length)
                    A = PackLong(Input, 0_kIndex)*K2
                    B = PackLong(Input, 8_kIndex)
                    C = PackLong(Input, Length - 8)*M
                    D = PackLong(Input, Length - 16)*K2
                    Y = RotateRight(A + B, 43) + RotateRight(C, 30) + D
                    Z = HashLen16_3(Y, A + RotateRight(B + K2, 18) + C, M)
                    E = PackLong(Input, 16_kIndex)*M
                    F = PackLong(Input, 24_kIndex)
                    G = (Y + PackLong(Input, Length - 32))*M
                    H = (Z + PackLong(Input, Length - 24))*M
                    HashCode = HashLen16_3(RotateRight(E + F, 43) + RotateRight(G, 30) + H, \
                                           E + RotateRight(F + A, 18) + G, M)
                END BLOCK
            END IF
        END ASSOCIATE
    END IF

    ! finalize with seed(s)
    HashCode = HashLen16_2(HashCode - HS%Seed(2), HS%Seed(1))

    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInt64(Z'7FFFFFFFFFFFFFFF'))

    ! reset the hasher
    HS%Seed   = 0_kInt64
    HS%State  = 0_kInt64
    HS%BufArr = 0_kInt8
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

CONTAINS

    PURE FUNCTION PackInteger(Buf, Off) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To pack the array 'Buf' at Offset 'Off' into the 32-bit word 'Res',
        ! in little-endian convention (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt8, INTENT(IN)  :: Buf(0:)  ! buffer
        tIndex, INTENT(IN)  :: Off      ! Offset
        tUInt32             :: Res      ! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

#define MaskInt32(X)  IAND(ToInt32(X), Z'000000FF')

        ! implementation algorithm #1
        Res = IOR(IOR(IOR(       MaskInt32(Buf(Off)),         &
                          SHIFTL(MaskInt32(Buf(Off+1)),  8)), &
                          SHIFTL(MaskInt32(Buf(Off+2)), 16)), &
                          SHIFTL(MaskInt32(Buf(Off+3)), 24))

#undef MaskInt32

        RETURN

    END FUNCTION PackInteger

    !**************************************************************************

END FUNCTION FarmNa_Finalize

!******************************************************************************

FUNCTION FarmNa_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FarmNaHasher64),  INTENT(INOUT)   :: HS           !! a hasher (HS) object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tUInt64,  OPTIONAL,     INTENT(IN)      :: Seed         !! seed
    tLogical, OPTIONAL,     INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tUInt64                                 :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Seed0

!** FLOW

    SET_OPTION(Seed0, 0_kInt64, Seed)

    ! The following code illustrates simple use of the hasher.
    CALL HS%Initialize(Seed0, RemoveSign)
    CALL HS%Update(Input, InpSize)
    HashCode = HS%Finalize()

    RETURN

END FUNCTION FarmNa_HashDirect

!******************************************************************************

FUNCTION FarmNa_HashDirect_WithSeeds(HS, Input, InpSize, Seed1, Seed2, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) with two seeds.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FarmNaHasher64),  INTENT(INOUT)   :: HS           !! a hasher (HS) object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tUInt64,                INTENT(IN)      :: Seed1        !! seed1
    tUInt64,                INTENT(IN)      :: Seed2        !! seed2
    tLogical, OPTIONAL,     INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tUInt64                                 :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! The following code illustrates simple use of the hasher.
    CALL HS%InitializeWSeeds(Seed1, Seed2, RemoveSign)
    CALL HS%Update(Input, InpSize)
    HashCode = HS%Finalize()

    RETURN

END FUNCTION FarmNa_HashDirect_WithSeeds

!******************************************************************************

PURE FUNCTION PackLong(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at Offset 'Off' into the 64-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! Offset
    tUInt64             :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: Input(0:7)
    tUInt64     :: Output
    EQUIVALENCE (Input, Output)

! FLOW

    ! implementation algorithm #7
    Input(0:7) = Buf(Off:Off+7)
    Res = Output

    RETURN

END FUNCTION PackLong

!**************************************************************************

END MODULE MClass_FarmNaHasher64

!******************************************************************************
