
MODULE MClass_KomiHasher64

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *KomiHasher64* type and its related routines.
!   The *KomiHasher64* type is a hasher type that extends directly from the
!   <a href="../module/mclass_hasher64.html#type-hasher64">Hasher64</a> type.
!   It provides all deferred procedures required by a *Hasher64* class and
!   outputs the hash value as a 64-bit integer. <br>
!   The *KomiHasher64* type employs the *Komi* hash algorithm for 64-bit integer
!   output by Aleksey Vaneev [1].  As a hasher, it can be used to compute the
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
!   [1] <a href="https://github.com/avaneev/komihash">KOMIHASH - Very fast, high-quality
!       hash function, discrete-incremental and streamed hashing-capable (non-cryptographic,
!       in C) + PRNG.</a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_Hasher64
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: KomiHasher64

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI32(X)              IAND(ToInt64(X), Z'00000000FFFFFFFF')
#define KOMIHASH_HASHROUND() \
    CALL KH_M128(Seed1, Seed5, R2L, R2H); \
    Seed5 = Seed5 + R2H; \
    Seed1 = IEOR(Seed5, R2L);
#define KOMIHASH_HASHFINAL() \
    CALL KH_M128(R2L, R2H, R1L, R1H); \
    Seed5 = Seed5 + R1H; \
    Seed1 = IEOR(Seed5, R1L); \
    KOMIHASH_HASHROUND();

!** MODULE PARAMETERS:
    tIndex,  PARAMETER  :: BlockLen = 64_kIndex

!** DERIVED TYPE DEFINITIONS
    !> *KomiHasher64* is a hasher type that outputs the hash value as a 64-bit integer.
    !  It employs the *Komi* hash algorithm by Aleksey Vaneev.
    TYPE, EXTENDS(Hasher64) :: KomiHasher64
        PRIVATE
        !% state
        tUInt64     :: State(8)             = 0_kInt64
        !% buffer array used to store input data
        tUInt8      :: BufArr(0:BlockLen-1) = 0_kInt8
        !% flag indicating whether to remove sign from the final hash value
        tLogical    :: RemoveSign   = FalseVal
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName          => Komi_GetName
        !> *GetBlockLength* is a procedure to return the block length of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockLength   => Komi_BlockLength
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: SetBufPtr        => Komi_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: ProcessBlock     => Komi_ProcessBlock
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the hasher. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Initialize(Seed)            ! hash value with sign <br>
        !   --->    CALL Hasher%Initialize(Seed, .TRUE.)    ! remove sign from hash value <br>
        PROCEDURE   :: Initialize       => Komi_Initialize
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 64-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize         => Komi_Finalize
        !> **Type-Bound Function**: HashDirect <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect       => Komi_HashDirect
    END TYPE KomiHasher64

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION Komi_GetName(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KomiHasher64), INTENT(IN) :: HS   !! a hasher (HS) object
    tCharAlloc                      :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Name = 'Komi_Hahser64'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Komi_GetName

!******************************************************************************

FUNCTION Komi_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KomiHasher64), INTENT(IN) :: HS       !! a hasher (HS) object
    tIndex                          :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Komi_BlockLength

!******************************************************************************

SUBROUTINE Komi_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KomiHasher64), TARGET, INTENT(INOUT)  :: HS           !! a hasher (HS) object
    tUInt8,             POINTER, INTENT(INOUT)  :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => HS%BufArr

    RETURN

END SUBROUTINE Komi_SetBufPtr

!******************************************************************************

SUBROUTINE Komi_ProcessBlock(HS, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KomiHasher64), INTENT(INOUT)  :: HS           !! a hasher (HS) object
    tUInt8,              INTENT(IN)     :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: R1L, R1H, R2L, R2H
    tUInt64         :: R3L, R3H, R4L, R4H
    tUInt8          :: I64Bytes(0:63)
    tUInt64         :: I64Val(1:8)
    EQUIVALENCE(I64Bytes, I64Val)

!** FLOW

    ASSOCIATE (Seed1 => HS%State(1), Seed2 => HS%State(2), &
               Seed3 => HS%State(3), Seed4 => HS%State(4), &
               Seed5 => HS%State(5), Seed6 => HS%State(6), &
               Seed7 => HS%State(7), Seed8 => HS%State(8))

        IF (HS%GetBlockCount() == 0_kIndex) THEN
            Seed2 = IEOR(Z'13198A2E03707344', Seed1)
            Seed3 = IEOR(Z'A4093822299F31D0', Seed1)
            Seed4 = IEOR(Z'082EFA98EC4E6C89', Seed1)
            Seed6 = IEOR(Z'BE5466CF34E90C6C', Seed5)
            Seed7 = IEOR(Z'C0AC29B7C97C50DD', Seed5)
            Seed8 = IEOR(Z'3F84D5B5B5470917', Seed5)
        END IF
        
        ! convert data stored in BufArr to I64Val via EQUIVALENCE statement and assignment
        I64Bytes = BytesIn
        CALL KH_M128(IEOR(Seed1, I64Val(1)), IEOR(Seed5, I64Val(2)), R1L, R1H)
        CALL KH_M128(IEOR(Seed2, I64Val(3)), IEOR(Seed6, I64Val(4)), R2L, R2H)
        CALL KH_M128(IEOR(Seed3, I64Val(5)), IEOR(Seed7, I64Val(6)), R3L, R3H)
        CALL KH_M128(IEOR(Seed4, I64Val(7)), IEOR(Seed8, I64Val(8)), R4L, R4H)
                
        ! Such "shifting" arrangement (below) does not increase
        ! individual SeedN's PRNG period beyond 2^64, but reduces a
        ! chance of any occasional synchronization between PRNG lanes
        ! happening. Practically, Seed1-4 together become a single
        ! "fused" 256-bit PRNG value, having a summary PRNG period of
        ! 2^66.
        Seed5 = Seed5 + R1H
        Seed6 = Seed6 + R2H
        Seed7 = Seed7 + R3H
        Seed8 = Seed8 + R4H
        Seed2 = IEOR(Seed5, R2L)
        Seed3 = IEOR(Seed6, R3L)
        Seed4 = IEOR(Seed7, R4L)
        Seed1 = IEOR(Seed8, R1L)

    END ASSOCIATE

    RETURN

END SUBROUTINE Komi_ProcessBlock

!******************************************************************************

SUBROUTINE Komi_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KomiHasher64), TARGET,   INTENT(INOUT)    :: HS   !! a hasher (HS) object
    tUInt64,                       INTENT(IN)       :: Seed !! seed
    tLogical,            OPTIONAL, INTENT(IN)       :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64 :: R2L, R2H

!** FLOW

    ASSOCIATE (Seed1 => HS%State(1), Seed5 => HS%State(5))

        ! The seeds are initialized to the first mantissa bits of PI.
        Seed1 = IEOR(Z'243F6A8885A308D3', IAND(Seed, Z'5555555555555555'))
        Seed5 = IEOR(Z'452821E638D01377', IAND(Seed, Z'AAAAAAAAAAAAAAAA'))

        ! The three instructions in the "KOMIHASH_HASHROUND" macro represent the
        ! simplest constant-less PRNG, scalable to any even-sized state
        ! variables, with the `Seed1` being the PRNG output (2^64 PRNG period).
        ! It passes `PractRand` tests with rare non-systematic "unusual"
        ! evaluations.
        !
        ! To make this PRNG reliable, self-starting, and eliminate a risk of
        ! stopping, the following variant can be used, which is a "register
        ! checker-board", a source of raw entropy. The PRNG is available as the
        ! komirand() function. Not required for hashing (but works for it) since
        ! the input entropy is usually available in abundance during hashing.
        !
        ! Seed5 += R2H + 0xAAAAAAAAAAAAAAAA
        !
        ! (the `0xAAAA...` constant should match register's size essentially,
        ! it is a replication of the `10` bit-pair it is not an arbitrary
        ! constant).

        KOMIHASH_HASHROUND() ! Required for PerlinNoise.

    END ASSOCIATE

    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE Komi_Initialize

!******************************************************************************

FUNCTION Komi_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 64-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KomiHasher64), INTENT(INOUT)  :: HS       !! a hasher (HS) object
    tUInt64                             :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: R1L, R1H, R2L, R2H
    tUInt64         :: FByte, Inp64
    tIndex          :: Length, BLShift, Remaining, Offset
    ! variables used to quickly access input and secret bytes
    tUInt8          :: I64Bytes(0:63)
    tUInt64         :: I64Val(1:8)
    EQUIVALENCE(I64Bytes, I64Val)
    tUInt8          :: I32Bytes(0:7)
    tUInt32         :: I32Val(2)
    EQUIVALENCE(I32Bytes, I32Val)

!** FLOW
    
    Length = HS%GetBufLen()
    FByte  = SHIFTL(1, SHIFTR(HS%BufArr(Length-1), 7))
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        Length = HS%GetBlockCount()*HS%GetBlockLength() + Length
    END IF

    ASSOCIATE (Seed1 => HS%State(1), Seed5 => HS%State(5), Input => HS%BufArr)
        IF (Length < 16) THEN
            R2L = Seed1
            R2H = Seed5
            IF (Length > 7) THEN
                ! The following two XOR instructions are equivalent to mixing a
                ! message with a cryptographic one-time-pad (bitwise modulo 2
                ! addition). Message's statistics and distribution are thus
                ! unimportant.
                BLShift = SHIFTL(Length-8, 3)
                IF (Length < 12) THEN
                    I64Bytes(0:2) = Input(Length-3:Length-1)
                    I64Bytes(3:7) = 0_kInt8
                    Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(I64Val(1), (24 - BLShift)))
                ELSE
                    I32Bytes(0:3) = Input(8:11)
                    I32Bytes(4:7) = Input(Length-4:Length-1)
                    Inp64 = IOR(IOR(SHIFTL(FByte, BLShift), MaskI32(I32Val(1))), &
                                    SHIFTL(SHIFTR(MaskI32(I32Val(2)), (64 - BLShift)), 32))
                END IF
                R2H = IEOR(R2H, Inp64)
                I64Bytes(0:7) = Input(0:7)
                R2L = IEOR(R2L, I64Val(1))
            ELSEIF (Length /= 0) THEN
                BLShift = SHIFTL(Length, 3)
                IF (Length < 4) THEN
                    I64Bytes(0:Length-1) = Input(0:Length-1)
                    I64Bytes(Length:7)   = 0_kInt8
                    Inp64 = IOR(SHIFTL(FByte, BLShift), I64Val(1))
                ELSE
                    I32Bytes(0:3) = Input(0:3)
                    I32Bytes(4:7) = Input(Length-4:Length-1)
                    Inp64 = IOR(IOR(SHIFTL(FByte, BLShift), MaskI32(I32Val(1))), &
                                    SHIFTL(SHIFTR(MaskI32(I32Val(2)), (64 - BLShift)), 32))
                END IF
                R2L = IEOR(R2L, Inp64)
            END IF
            KOMIHASH_HASHFINAL()
            HashCode = Seed1
        ELSEIF (Length  < 32) THEN
            I64Bytes(0:15) = Input(0:15)
            CALL KH_M128(IEOR(Seed1, I64Val(1)), IEOR(Seed5, I64Val(2)), R1L, R1H)
            Seed5 = Seed5 + R1H
            Seed1 = IEOR(Seed5, R1L)
            IF (Length  > 23) THEN
                BLShift = SHIFTL(Length-24, 3)
                IF (Length < 29) THEN
                    I32Bytes(0:3) = Input(Length-4:Length-1)
                    Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(MaskI32(I32Val(1)), (32 - BLShift)))
                ELSE
                    I64Bytes(0:7) = Input(Length-8:Length-1)
                    Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(I64Val(1), (64 - BLShift)))
                END IF
                R2H = IEOR(Seed5, Inp64)
                I64Bytes(0:7) = Input(16:23)
                R2L = IEOR(Seed1, I64Val(1))
            ELSE
                BLShift = SHIFTL(Length-16, 3)
                IF (Length < 21) THEN
                    I32Bytes(0:3) = Input(Length-4:Length-1)
                    Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(MaskI32(I32Val(1)), (32 - BLShift)))
                ELSE
                    I64Bytes(0:7) = Input(Length-8:Length-1)
                    Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(I64Val(1), (64 - BLShift)))
                END IF
                R2L = IEOR(Seed1, Inp64)
                R2H = Seed5
            END IF
            KOMIHASH_HASHFINAL()
            HashCode = Seed1
        ELSE
            IF (Length >= 64) THEN
                ASSOCIATE (Seed2 => HS%State(2), Seed3 => HS%State(3), Seed4 => HS%State(4), &
                           Seed6 => HS%State(6), Seed7 => HS%State(7), Seed8 => HS%State(8))
                    Seed5 = IEOR(Seed5, IEOR(IEOR(Seed6, Seed7), Seed8))
                    Seed1 = IEOR(Seed1, IEOR(IEOR(Seed2, Seed3), Seed4))
                END ASSOCIATE
            END IF
            Remaining = HS%GetBufLen()
            Offset = 0_kIndex
            IF (Remaining > 31) THEN
                I64Bytes(0:15) = Input(Offset:Offset+15)
                CALL KH_M128(IEOR(Seed1, I64Val(1)), IEOR(Seed5, I64Val(2)), R1L, R1H)
                Seed5 = Seed5 + R1H
                Seed1 = IEOR(Seed5, R1L)
                I64Bytes(16:31) = Input(Offset+16:Offset+31)
                CALL KH_M128(IEOR(Seed1, I64Val(3)), IEOR(Seed5, I64Val(4)), R1L, R1H)
                Seed5 = Seed5 + R1H
                Seed1 = IEOR(Seed5, R1L)
                ! update indices
                Offset = Offset + 32
                Remaining = Remaining - 32
            END IF

            IF (Remaining > 15) THEN
                I64Bytes(0:15) = Input(Offset:Offset+15)
                CALL KH_M128(IEOR(Seed1, I64Val(1)), IEOR(Seed5, I64Val(2)), R1L, R1H)
                Seed5 = Seed5 + R1H
                Seed1 = IEOR(Seed5, R1L)
                ! update indices
                Offset = Offset + 16
                Remaining = Remaining - 16
            END IF

            IF (Remaining > 7) THEN
                BLShift = SHIFTL(Remaining-8, 3)
                IF (Remaining < 13) THEN
                    I32Bytes(0:3) = Input(Offset+Remaining-4:Offset+Remaining-1)
                    Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(MaskI32(I32Val(1)), (32 - BLShift)))
                ELSE
                    I64Bytes(0:7) = Input(Offset+Remaining-8:Offset+Remaining-1)
                    Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(I64Val(1), (64 - BLShift)))
                END IF
                R2H = IEOR(Seed5, Inp64)
                I64Bytes(0:7) = Input(Offset:Offset+7)
                R2L = IEOR(Seed1, I64Val(1))
            ELSE
                BLShift = SHIFTL(Remaining, 3)
                IF (Remaining < 5) THEN
                    I32Bytes(0:3) = Input(Offset+Remaining-4:Offset+Remaining-1)
                    Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(MaskI32(I32Val(1)), (32 - BLShift)))
                ELSE
                    I64Bytes(0:7) = Input(Offset+Remaining-8:Offset+Remaining-1)
                    Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(I64Val(1), (64 - BLShift)))
                END IF
                R2L = IEOR(Seed1, Inp64)
                R2H = Seed5
            END IF

            KOMIHASH_HASHFINAL()
            HashCode = Seed1
        END IF
    END ASSOCIATE
            
    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInt64(Z'7FFFFFFFFFFFFFFF'))

    ! reset the hasher
    HS%State  = 0_kInt64
    HS%BufArr = 0_kInt8
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

END FUNCTION Komi_Finalize

!******************************************************************************

FUNCTION Komi_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KomiHasher64),    INTENT(INOUT)   :: HS           !! a hasher (HS) object
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

END FUNCTION Komi_HashDirect

!******************************************************************************

SUBROUTINE KH_M128(LHS, RHS, ResLo, ResHi)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute 128-bit result of multiplication of two 64-bit unsigned integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)   :: LHS
    tUInt64, INTENT(IN)   :: RHS
    tUInt64, INTENT(OUT)  :: ResLo
    tUInt64, INTENT(OUT)  :: ResHi

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64 :: LHS_Lo, LHS_Hi, RHS_Lo, RHS_Hi
    tUInt64 :: Lo_Lo, Hi_Lo
    tUInt64 :: Cross

!** FLOW

    ! the Grade School method of multiplication.
    LHS_Lo = MaskI32(LHS)
    LHS_Hi = SHIFTR(LHS, 32)
    RHS_Lo = MaskI32(RHS)
    RHS_Hi = SHIFTR(RHS, 32)
    Lo_Lo = LHS_Lo*RHS_Lo
    Hi_Lo = LHS_Hi*RHS_Lo

    ! Add the products together. This will never overfLow.
    Cross = SHIFTR(Lo_Lo, 32) + MaskI32(Hi_Lo) + LHS_Lo*RHS_Hi
    ResLo = IOR(SHIFTL(Cross, 32), MaskI32(Lo_Lo))
    ResHi = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + LHS_Hi*RHS_Hi

    RETURN

END SUBROUTINE KH_M128

!******************************************************************************

END MODULE MClass_KomiHasher64

!******************************************************************************
