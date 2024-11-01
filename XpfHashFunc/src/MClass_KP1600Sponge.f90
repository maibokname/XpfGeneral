
MODULE MClass_KP1600Sponge

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *KP1600Sponge* type and its related routines.
!   The *KP1600Sponge* type is a derived type representing a sponge instance
!   for use with the so-called *Keccak Sponge* function.  It provides basic
!   operations of the *Keccak-p[1600, nRound]* message-digest algorithm, which
!   has the internal state size of 1600 bits.  The *Keccak Sponge* function,
!   the *Sponge* construction and other related information are described in
!   FIPS 202 [1].  The implementation here is mainly based on the *eXtended
!   Keccak Code Package* (XKCP) [2].  The *KP1600Sponge* type is provided
!   to help the development and implementation of the *Keccak-based* digest
!   types.  <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://doi.org/10.6028/NIST.FIPS.202">SHA-3 Standard:
!       Permutation-Based Hash and Extendable-Output Functions. </a> <br>
!   [2] <a href="https://github.com/XKCP/XKCP">The eXtended Keccak Code Package. </a> <br>

!** USE STATEMENTS:
    USE, INTRINSIC :: ISO_C_BINDING
    USE MBase_Common
    USE MBase_BytePack,           ONLY: BytePackLE, ByteUnpackLE
    USE MBase_UIntUtil,           ONLY: OPERATOR (.UGE.)

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: KP1600Sponge
    PUBLIC :: SUCCESS, FAILURE

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    ! parameters for the first padding byte
    tByte,    PARAMETER :: FByte00 = ToInt8(Z'00')
    tByte,    PARAMETER :: FByte01 = ToInt8(Z'01')
    tByte,    PARAMETER :: FByte80 = ToInt8(Z'80')
    tByte,    PARAMETER :: FByteFF = ToInt8(Z'FF')
    ! size parameters
    tInteger, PARAMETER :: KP1600SizeInBits  = 1600
    tInteger, PARAMETER :: KP1600SizeInBytes = (KP1600SizeInBits/8)     ! 200
    tInteger, PARAMETER :: KP1600SizeInWords = (KP1600SizeInBits/64)    ! 25
    ! maximum number of rounds of permutation
    tInteger, PARAMETER :: MaxNrRounds       = 24
    ! return flags
    tByte,    PARAMETER :: SUCCESS           = 0_kInt8
    tByte,    PARAMETER :: FAILURE           = 1_kInt8

!** DERIVED TYPE DEFINITIONS
    !> *KP1600Sponge* is a derived type representing a sponge instance for
    !  use with the *Keccak Sponge* functions.  It gathers the state processed
    !  by the permutation as well as the rate.
    TYPE KP1600Sponge
        PRIVATE
        !% The state in bytes processed by the permutation
        tByte           :: State(0:KP1600SizeInBytes-1) = 0_kInt8
        !% The state in words; an alias of the state
        tLong, POINTER  :: StateAsWords(:) => NULL()
        !% The number of permutation rounds
        tInteger        :: NRounds = 24
        !% The value of the rate in bits
        tInteger        :: Rate
        !% The position in the state of the next byte to be input/output
        tInteger        :: ByteIOIndex = 0
        !% The flag for squeezing
        tLogical        :: Squeezing = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: AddByte          => KP1600Sponge_AddByte
        PROCEDURE, PRIVATE  :: AddBytes         => KP1600Sponge_AddBytes
        PROCEDURE, PRIVATE  :: ExtractBytes     => KP1600Sponge_ExtractBytes
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the *Sponge* instance. <br>
        !  **Usage**: <br>
        !   --->    CALL Instance%Initialize(Capacity, nRounds)
        PROCEDURE       :: Initialize           => KP1600Sponge_Initialize
        !> **Type-Bound Function**: Absorb <br>
        !  **Purpose**:  To insert input data in bytes to be absorbed by *Sponge* instance. <br>
        !  **Usage**: <br>
        !   --->    Flag = Instance%Absorb(Input, InpSize) <br>
        !   --->    IF (.NOT.Instance%Absorb(Input, InpSize)) DoSomething <br>
        !  **Note**: Must call this method before calling either the *AbsorbLastFewBits*
        !            method or the *Squeeze* method. <br>
        PROCEDURE       :: Absorb               => KP1600Sponge_Absorb
        !> **Type-Bound Function**: AbsorbLastFewBits <br>
        !  **Purpose**:  To insert input data in bits to be absorbed by *Sponge* instance. <br>
        !  **Usage**: <br>
        !   --->    Flag = Instance%AbsorbLastFewBits(LastByte) <br>
        !   --->    IF (.NOT.Instance%AbsorbLastFewBits(LastByte)) DoSomething <br>
        !  **Note**: Must call this method only once and before calling the *Squeeze* method. <br>
        PROCEDURE       :: AbsorbLastFewBits    => KP1600Sponge_AbsorbLastFewBits
        !> **Type-Bound Subroutine**: Permute <br>
        !  **Purpose**:  To perform a permutation of the state of the *Sponge* instance. <br>
        !  **Usage**: <br>
        !   --->    CALL Instance%Permute()
        PROCEDURE       :: Permute              => KP1600Sponge_Permute
        !> **Type-Bound Function**: Squeeze <br>
        !  **Purpose**:  To retrieve output data from *Sponge* instance. <br>
        !  **Usage**: <br>
        !   --->    Flag = Instance%Squeeze(Output, OutSize) <br>
        !   --->    IF (.NOT.Instance%Squeeze(Output, OutSize)) DoSomething <br>
        PROCEDURE       :: Squeeze              => KP1600Sponge_Squeeze
        !> **Type-Bound Function**: GetByteIOIndex <br>
        !  **Purpose**:  To get value of the *ByteIOIndex* component of the *Sponge* instance. <br>
        !  **Usage**: <br>
        !   --->    Index = Instance%GetByteIOIndex()
        PROCEDURE       :: GetByteIOIndex       => KP1600Sponge_GetByteIOIndex
        !> **Type-Bound Subroutine**: SetByteIOIndex <br>
        !  **Purpose**:  To set value of the *ByteIOIndex* component of the *Sponge* instance. <br>
        !  **Usage**: <br>
        !   --->    CALL Instance%SetByteIOIndex(Index)
        PROCEDURE       :: SetByteIOIndex       => KP1600Sponge_SetByteIOIndex
        !> **Type-Bound Subroutine**: CopyState <br>
        !  **Purpose**:  To copy the current state of the source instance. <br>
        !  **Usage**: <br>
        !   --->    CALL Src%CopyState(Dst)
        PROCEDURE       :: CopyState            => KP1600Sponge_CopyState
        ! ---------------------------------------------------------------------
        FINAL           :: KP1600Sponge_Finalize
        ! ---------------------------------------------------------------------
    END TYPE KP1600Sponge

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS

!------------------------------------------------------------------------------
!               KP1600Sponge Procedures (i.e. Sponge Functions)
!------------------------------------------------------------------------------

SUBROUTINE KP1600Sponge_Initialize(Instance, Capacity, NRounds)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the state of the Keccak-p[1600, nRounds] sponge function.
    !  The phase of the sponge function is set to absorbing. <br>
    !  Note: the capacity 'c' is a double of the security strength of a particular
    !        cryptographic hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Sponge), TARGET, INTENT(INOUT)  :: Instance !! 'Sponge' instance
    tInteger,                    INTENT(IN)     :: Capacity !! value of the capacity c
    tInteger,                    INTENT(IN)     :: NRounds  !! the number of rounds to be permuted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(C_PTR)     :: StateLoc

! FLOW

    Instance%State = 0_kInt8
    ! map the StateAsWords to the State
    StateLoc = C_LOC(Instance%State)
    CALL C_F_POINTER(StateLoc, Instance%StateAsWords, SHAPE=[KP1600SizeInWords])
    Instance%Rate = KP1600SizeInBits - Capacity
    Instance%ByteIOIndex = 0
    Instance%Squeezing = FalseVal
    Instance%NRounds = NRounds

    RETURN

END SUBROUTINE KP1600Sponge_Initialize

!******************************************************************************

FUNCTION KP1600Sponge_Absorb(Instance, InpDat, InpByteLen) RESULT(RetFlag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data in bytes to be absorbed by the sponge function. <br>
    !  The sponge function must be in the absorbing phase, which means that
    !  the *Squeezing* flag is currently set to false.  Otherwise, the routine
    !  return a *FAILURE* value. <br>
    !  This also indicates that this function (i.e. the *Absorb* method) must be
    !  called before the *AbsorbLastFewBits* method or the *Squeeze* method is
    !  called.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Sponge), INTENT(INOUT) :: Instance      !! 'Sponge' instance
    tByte,               INTENT(IN)    :: InpDat(0:)    !! input data as an array of 8-bit integers
    tIndex,              INTENT(IN)    :: InpByteLen    !! size of input data in bytes
    tByte                              :: RetFlag
    !^ Returned flag indicating whether the operation is successful or not. <br>
    !  - Return the *FAILURE* value if the *Squeezing* flag is true. <br>
    !  - Otherwise, return the *SUCCESS* value. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J
    tInteger    :: PartialBlock, RateInBytes
    tIndex      :: CurIndx

! FLOW

    IF (Instance%Squeezing) THEN
        ! Too late for additional input
        RetFlag = FAILURE
        RETURN
    END IF

    ! initialize
    RateInBytes = Instance%Rate / 8
    I = 0
    CurIndx = 0_kIndex

    DO WHILE (I < InpByteLen)
        IF ((Instance%ByteIOIndex == 0).AND.(InpByteLen >= (I + RateInBytes))) THEN
            J = InpByteLen-I
            DO WHILE (J >= RateInBytes)
                CALL Instance%AddBytes(InpDat(CurIndx:), 0, RateInBytes)
                CALL Instance%Permute()
                CurIndx = CurIndx + RateInBytes
                J = J - RateInBytes
            END DO
            I = InpByteLen - J
        ELSE
            ! normal lane: using the message queue
            PartialBlock = ToInt32(InpByteLen - I)
            IF (PartialBlock+Instance%ByteIOIndex > RateInBytes) THEN
                PartialBlock = RateInBytes - Instance%ByteIOIndex
            END IF
            I = I + PartialBlock
            CALL Instance%AddBytes(InpDat(CurIndx:), Instance%ByteIOIndex, PartialBlock)
            CurIndx = CurIndx + PartialBlock
            Instance%ByteIOIndex = Instance%ByteIOIndex + PartialBlock
            IF (Instance%ByteIOIndex == RateInBytes) THEN
                CALL Instance%Permute()
                Instance%ByteIOIndex = 0
            END IF
        END IF
    END DO

    RetFlag = SUCCESS

    RETURN

END FUNCTION KP1600Sponge_Absorb

!******************************************************************************

FUNCTION KP1600Sponge_AbsorbLastFewBits(Instance, DelimitedData) RESULT(RetFlag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data in bits to be absorbed by the sponge function and
    !  then switch to the squeezing phase. <br>
    !  The sponge function must be in the absorbing phase, which means that
    !  the *Squeezing* flag is currently set to false.  Otherwise, the routine
    !  return a *FAILURE* value. <br>
    !  This also indicates that this function (i.e. the *AbsorbLastFewBits* method)
    !  must be called only once and before the *Squeeze* method is called.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Sponge), INTENT(INOUT)  :: Instance      !! 'Sponge' instance
    tByte,               INTENT(IN)     :: DelimitedData
    !^ A single byte containing from 0 to 7 trailing bits that must be absorbed.
    !  These *n* bits must be in the least significant bit positions. These bits
    !  must be delimited with a bit 1 at position *n* (counting from 0=LSB to 7=MSB)
    !  and followed by bits 0 from position *n*+1 to position 7. <br>
    !  Some examples: <br>
    !   - If no bits are to be absorbed, then *DelimitedData* must be Z'01'. <br>
    !   - If the 2-bit sequence 0,0 is to be absorbed, *DelimitedData* must be Z'04'. <br>
    !   - If the 5-bit sequence 0,1,0,0,1 is to be absorbed, *DelimitedData* must be Z'32'. <br>
    !   - If the 7-bit sequence 1,1,0,1,0,0,0 is to be absorbed, *DelimitedData* must be Z'8B'. <br>
    !  *DelimitedData* must NOT be zero.
    tByte                               :: RetFlag
    !^ Returned flag indicating whether the operation is successful or not. <br>
    !  - Return the *FAILURE* value if the *Squeezing* flag is true or *DelimitedData* is zero. <br>
    !  - Otherwise, return the *SUCCESS* value. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: RateInBytes

! FLOW

    IF (DelimitedData == 0_kInt8) THEN
        ! no data to be added
        RetFlag = FAILURE
        RETURN
    END IF

    IF (Instance%Squeezing) THEN
        ! Too late for additional input
        RetFlag = FAILURE
        RETURN
    END IF

    ! initialize
    RateInBytes = Instance%Rate / 8

    ! Last few bits, whose delimiter coincides with first bit of padding
    CALL Instance%AddByte(DelimitedData, Instance%ByteIOIndex)
    ! If the first bit of padding is at position rate-1,
    ! we need a whole new block for the second bit of padding
    IF ((DelimitedData .UGE. FByte80).AND.(Instance%ByteIOIndex == (RateInBytes-1))) THEN
        CALL Instance%Permute()
    END IF
    ! Second bit of padding
    CALL Instance%AddByte(FByte80, RateInBytes-1)
    CALL Instance%Permute()
    Instance%ByteIOIndex = 0
    Instance%Squeezing = TrueVal

    RetFlag = SUCCESS

    RETURN

END FUNCTION KP1600Sponge_AbsorbLastFewBits

!******************************************************************************

SUBROUTINE KP1600Sponge_Permute(Instance)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform a permutation of the state of the instance.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Sponge), INTENT(INOUT)  :: Instance !! 'Sponge' instance

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: NrLanes      = 25
    tLong,    PARAMETER :: KeccakRoundConstants(0:MaxNrRounds-1) = [  &
            ToInt64(Z'0000000000000001'), ToInt64(Z'0000000000008082'), &
            ToInt64(Z'800000000000808A'), ToInt64(Z'8000000080008000'), &
            ToInt64(Z'000000000000808B'), ToInt64(Z'0000000080000001'), &
            ToInt64(Z'8000000080008081'), ToInt64(Z'8000000000008009'), &
            ToInt64(Z'000000000000008A'), ToInt64(Z'0000000000000088'), &
            ToInt64(Z'0000000080008009'), ToInt64(Z'000000008000000A'), &
            ToInt64(Z'000000008000808B'), ToInt64(Z'800000000000008B'), &
            ToInt64(Z'8000000000008089'), ToInt64(Z'8000000000008003'), &
            ToInt64(Z'8000000000008002'), ToInt64(Z'8000000000000080'), &
            ToInt64(Z'000000000000800A'), ToInt64(Z'800000008000000A'), &
            ToInt64(Z'8000000080008081'), ToInt64(Z'8000000000008080'), &
            ToInt64(Z'0000000080000001'), ToInt64(Z'8000000080008008')]
    tInteger, PARAMETER :: KeccakRhoOffsets(0:NrLanes-1) = [          &
            0,  1, 62, 28, 27, 36, 44,  6, 55, 20,  3, 10, 43,        &
            25, 39, 41, 45, 15, 21,  8, 18,  2, 61, 56, 14]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: A0, A1, A2, A3, A4
    tLong       :: A5, A6, A7, A8, A9
    tLong       :: A10, A11, A12, A13, A14
    tLong       :: A15, A16, A17, A18, A19
    tLong       :: A20, A21, A22, A23, A24
    tLong       :: C0, C1, C2, C3, C4
    tLong       :: D0, D1, D2, D3, D4
    tInteger    :: IRound

!** SUBROUTINE MACRO DEFINITIONS:
#define IEOR_5(A, B, C, D, E)   IEOR(IEOR(IEOR(IEOR(A, B), C), D), E)
#define Lane(A, B)              IEOR(RotateLeft(A, 1), B)
#define IEOR_N_NOT(A, B, C)     IEOR(A, IAND(NOT(B), C))

! FLOW

    ASSOCIATE(State => Instance%StateAsWords, NRounds => Instance%NRounds)

        ! get input
        A0  = State(1)
        A1  = State(2)
        A2  = State(3)
        A3  = State(4)
        A4  = State(5)
        A5  = State(6)
        A6  = State(7)
        A7  = State(8)
        A8  = State(9)
        A9  = State(10)
        A10 = State(11)
        A11 = State(12)
        A12 = State(13)
        A13 = State(14)
        A14 = State(15)
        A15 = State(16)
        A16 = State(17)
        A17 = State(18)
        A18 = State(19)
        A19 = State(20)
        A20 = State(21)
        A21 = State(22)
        A22 = State(23)
        A23 = State(24)
        A24 = State(25)

        ! permute state
        DO IRound = MaxNrRounds-NRounds, MaxNrRounds-1

            ! theta step
            C0 = IEOR_5(A0, A5, A10, A15, A20)
            C1 = IEOR_5(A1, A6, A11, A16, A21)
            C2 = IEOR_5(A2, A7, A12, A17, A22)
            C3 = IEOR_5(A3, A8, A13, A18, A23)
            C4 = IEOR_5(A4, A9, A14, A19, A24)
            D1 = Lane(C1, C4)
            D2 = Lane(C2, C0)
            D3 = Lane(C3, C1)
            D4 = Lane(C4, C2)
            D0 = Lane(C0, C3)
            A0  = IEOR(A0, D1)
            A5  = IEOR(A5, D1)
            A10 = IEOR(A10, D1)
            A15 = IEOR(A15, D1)
            A20 = IEOR(A20, D1)
            A1  = IEOR(A1, D2)
            A6  = IEOR(A6, D2)
            A11 = IEOR(A11, D2)
            A16 = IEOR(A16, D2)
            A21 = IEOR(A21, D2)
            A2  = IEOR(A2, D3)
            A7  = IEOR(A7, D3)
            A12 = IEOR(A12, D3)
            A17 = IEOR(A17, D3)
            A22 = IEOR(A22, D3)
            A3  = IEOR(A3, D4)
            A8  = IEOR(A8, D4)
            A13 = IEOR(A13, D4)
            A18 = IEOR(A18, D4)
            A23 = IEOR(A23, D4)
            A4  = IEOR(A4, D0)
            A9  = IEOR(A9, D0)
            A14 = IEOR(A14, D0)
            A19 = IEOR(A19, D0)
            A24 = IEOR(A24, D0)

            ! rho and pi steps
            C0 = RotateLeft(A1,  1)
            A1  = RotateLeft(A6, 44)
            A6  = RotateLeft(A9, 20)
            A9  = RotateLeft(A22, 61)
            A22 = RotateLeft(A14, 39)
            A14 = RotateLeft(A20, 18)
            A20 = RotateLeft(A2, 62)
            A2  = RotateLeft(A12, 43)
            A12 = RotateLeft(A13, 25)
            A13 = RotateLeft(A19,  8)
            A19 = RotateLeft(A23, 56)
            A23 = RotateLeft(A15, 41)
            A15 = RotateLeft(A4, 27)
            A4  = RotateLeft(A24, 14)
            A24 = RotateLeft(A21,  2)
            A21 = RotateLeft(A8, 55)
            A8  = RotateLeft(A16, 45)
            A16 = RotateLeft(A5, 36)
            A5  = RotateLeft(A3, 28)
            A3  = RotateLeft(A18, 21)
            A18 = RotateLeft(A17, 15)
            A17 = RotateLeft(A11, 10)
            A11 = RotateLeft(A7,  6)
            A7  = RotateLeft(A10, 3)
            A10 = C0

            ! chi step
            C0 = IEOR_N_NOT(A0, A1, A2)
            C1 = IEOR_N_NOT(A1, A2, A3)
            A2 = IEOR_N_NOT(A2, A3, A4)
            A3 = IEOR_N_NOT(A3, A4, A0)
            A4 = IEOR_N_NOT(A4, A0, A1)
            A0 = C0
            A1 = C1

            C0 = IEOR_N_NOT(A5, A6, A7)
            C1 = IEOR_N_NOT(A6, A7, A8)
            A7 = IEOR_N_NOT(A7, A8, A9)
            A8 = IEOR_N_NOT(A8, A9, A5)
            A9 = IEOR_N_NOT(A9, A5, A6)
            A5 = C0
            A6 = C1

            C0  = IEOR_N_NOT(A10, A11, A12)
            C1  = IEOR_N_NOT(A11, A12, A13)
            A12 = IEOR_N_NOT(A12, A13, A14)
            A13 = IEOR_N_NOT(A13, A14, A10)
            A14 = IEOR_N_NOT(A14, A10, A11)
            A10 = C0
            A11 = C1

            C0  = IEOR_N_NOT(A15, A16, A17)
            C1  = IEOR_N_NOT(A16, A17, A18)
            A17 = IEOR_N_NOT(A17, A18, A19)
            A18 = IEOR_N_NOT(A18, A19, A15)
            A19 = IEOR_N_NOT(A19, A15, A16)
            A15 = C0
            A16 = C1

            C0  = IEOR_N_NOT(A20, A21, A22)
            C1  = IEOR_N_NOT(A21, A22, A23)
            A22 = IEOR_N_NOT(A22, A23, A24)
            A23 = IEOR_N_NOT(A23, A24, A20)
            A24 = IEOR_N_NOT(A24, A20, A21)
            A20 = C0
            A21 = C1

            ! iota step
            A0 = IEOR(A0, KeccakRoundConstants(IRound))
        END DO

        ! return output
        State(1)  = A0
        State(2)  = A1
        State(3)  = A2
        State(4)  = A3
        State(5)  = A4
        State(6)  = A5
        State(7)  = A6
        State(8)  = A7
        State(9)  = A8
        State(10)  = A9
        State(11) = A10
        State(12) = A11
        State(13) = A12
        State(14) = A13
        State(15) = A14
        State(16) = A15
        State(17) = A16
        State(18) = A17
        State(19) = A18
        State(20) = A19
        State(21) = A20
        State(22) = A21
        State(23) = A22
        State(24) = A23
        State(25) = A24

    END ASSOCIATE

    RETURN

#undef IEOR_N_NOT
#undef IEOR_5
#undef Lane

END SUBROUTINE KP1600Sponge_Permute

!******************************************************************************

FUNCTION KP1600Sponge_Squeeze(Instance, OutDat, OutByteLen) RESULT(RetFlag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To squeeze output data from the sponge function.  If the sponge function
    !  was in the absorbing phase, this function switches it to the squeezing
    !  phase as if the *AbsorbLastFewBits* method was called.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Sponge), INTENT(INOUT)  :: Instance     !! 'Sponge' instance
    tByte,               INTENT(OUT)    :: OutDat(0:)   !! a byte array to store the output data
    tIndex,              INTENT(IN)     :: OutByteLen   !! the number of output bytes desired
    tByte                               :: RetFlag
    !^ Returned flag indicating whether the operation is successful or not. <br>
    !  - Return the *FAILURE* value if the operation is NOT successful. <br>
    !  - Otherwise, return the *SUCCESS* value. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J
    tInteger    :: PartialBlock, RateInBytes
    tIndex      :: CurIndx

! FLOW

    IF (.NOT.Instance%Squeezing) THEN
        RetFlag = Instance%AbsorbLastFewBits(FByte01)
        IF (RetFlag == FAILURE) RETURN
    END IF

    ! initialize
    RateInBytes = Instance%Rate / 8
    I = 0_kIndex
    CurIndx = 0_kIndex

    DO WHILE (I < OutByteLen)
        IF ((Instance%ByteIOIndex == RateInBytes).AND.(OutByteLen >= (I + RateInBytes))) THEN
            J = OutByteLen-I
            DO WHILE (J >= RateInBytes)
                CALL Instance%Permute()
                CALL Instance%ExtractBytes(OutDat(CurIndx:), 0_kInt32, RateInBytes)
                CurIndx = CurIndx + RateInBytes
                J = J - RateInBytes
            END DO
            I = OutByteLen - J
        ELSE
            ! normal lane: using the message queue
            IF (Instance%ByteIOIndex == RateInBytes) THEN
                CALL Instance%Permute()
                Instance%ByteIOIndex = 0
            END IF
            PartialBlock = ToInt32(OutByteLen - I)
            IF (PartialBlock+Instance%ByteIOIndex > RateInBytes) THEN
                PartialBlock = RateInBytes - Instance%ByteIOIndex
            END IF
            I = I + PartialBlock
            CALL Instance%ExtractBytes(OutDat(CurIndx:), Instance%ByteIOIndex, PartialBlock)
            CurIndx = CurIndx + PartialBlock
            Instance%ByteIOIndex = Instance%ByteIOIndex + PartialBlock
        END IF
    END DO

    RetFlag = SUCCESS

    RETURN

END FUNCTION KP1600Sponge_Squeeze

!******************************************************************************

FUNCTION KP1600Sponge_GetByteIOIndex(Instance) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *ByteIOIndex* variable.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Sponge), INTENT(INOUT)  :: Instance
    tIndex                              :: Indx

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Indx = Instance%ByteIOIndex

    RETURN

END FUNCTION KP1600Sponge_GetByteIOIndex

!******************************************************************************

SUBROUTINE KP1600Sponge_SetByteIOIndex(Instance, Indx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the *ByteIOIndex* variable.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Sponge), INTENT(INOUT)  :: Instance
    tIndex,              INTENT(IN)     :: Indx

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Instance%ByteIOIndex = Indx

    RETURN

END SUBROUTINE KP1600Sponge_SetByteIOIndex

!******************************************************************************

SUBROUTINE KP1600Sponge_CopyState(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy the current states of the source instance to the destination instance. <br>
    !  Note: Only those that would be changed during various operations are copied.
    !        Those that would not be changed are expected to be copied to the
    !        destination during its construction.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Sponge), INTENT(INOUT)  :: Src  !! the source instance
    CLASS(KP1600Sponge), INTENT(INOUT)  :: Dst  !! the destination instance

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Dst%State       = Src%State
    Dst%ByteIOIndex = Src%ByteIOIndex
    Dst%Squeezing   = Src%Squeezing

    RETURN

END SUBROUTINE KP1600Sponge_CopyState

!------------------------------------------------------------------------------
!           State and Permutation Procedures (i.e. SnP Functions)
!------------------------------------------------------------------------------

SUBROUTINE KP1600Sponge_AddByte(Instance, InpByte, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a given byte into the state of the sponge instance.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Sponge), INTENT(INOUT)  :: Instance !! 'Sponge' instance
    tByte,               INTENT(IN)     :: InpByte  !! an input byte
    tInteger,            INTENT(IN)     :: Offset   !! offset in bytes within the state

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (IsLittleEndian) THEN
        Instance%State(Offset) = IEOR(Instance%State(Offset), InpByte)
    ELSE
        BLOCK
            tLong           :: Lane
            tIndex          :: Indx
            Lane = InpByte
            Lane = SHIFTL(Lane, MOD(Offset, 8)*8)
            Indx = SHIFTR(Offset, 3)
            Instance%StateAsWords(Indx) = IEOR(Instance%StateAsWords(Indx), Lane)
        END BLOCK
    END IF

    RETURN

END SUBROUTINE KP1600Sponge_AddByte

!******************************************************************************

SUBROUTINE KP1600Sponge_AddBytes(Instance, InpBytes, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the given bytes into the state of the sponge instance.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Sponge), INTENT(INOUT)  :: Instance     !! 'Sponge' instance
    tByte,               INTENT(IN)     :: InpBytes(0:) !! a byte array of input data
    tInteger,            INTENT(IN)     :: Offset       !! offset in bytes within the state
    tInteger,            INTENT(IN)     :: Length       !! length of the input bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex    :: I

! FLOW

    IF (IsLittleEndian) THEN
        DO I = 0, Length-1
            Instance%State(Offset+I) = IEOR(Instance%State(Offset+I), InpBytes(I))
        END DO
    ELSE
        BLOCK
            tLong           :: Lane
            tIndex          :: Indx
            I = 0
            DO WHILE (I < (Length-8))
                Indx = SHIFTR(Offset+I, 3)
                CALL BytePackLE(InpBytes, I, Lane)
                Instance%StateAsWords(Indx) = IEOR(Instance%StateAsWords(Indx), Lane)
                I = I + 8
            END DO
            DO WHILE (I < Length)
                Lane = InpBytes(I)
                Lane = SHIFTL(Lane, MOD(Offset+I, 8_kIndex)*8)
                Indx = SHIFTR(Offset+I, 3)
                Instance%StateAsWords(Indx) = IEOR(Instance%StateAsWords(Indx), Lane)
            END DO
        END BLOCK
    END IF

    RETURN

END SUBROUTINE KP1600Sponge_AddBytes

!******************************************************************************

SUBROUTINE KP1600Sponge_ExtractBytes(Instance, OutDat, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve output data from the state of the sponge instance.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Sponge), INTENT(INOUT)  :: Instance     !! 'Sponge' instance
    tByte,               INTENT(OUT)    :: OutDat(0:)   !! a byte array used to store output data
    tInteger,            INTENT(IN)     :: Offset       !! offset index of the state values
    tInteger,            INTENT(IN)     :: Length       !! a number of bytes extracted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (IsLittleEndian) THEN
        OutDat(0:Length-1) = Instance%State(Offset:Offset+Length-1)
    ELSE
        Outer: BLOCK
            tIndex  :: Len8, I, Indx
            Len8 = (Length/8)*8
            IF (Len8 == Length) THEN
                DO I = 0, Length-1, 8
                    Indx = SHIFTR(Offset+I, 3)
                    CALL ByteUnpackLE(Instance%StateAsWords(Indx), OutDat, I)
                END DO
            ELSE
                Len8 = Len8 + 8
                Inner: BLOCK
                    tByte   :: OutBuf(0:Length)
                DO I = 0, Len8-1, 8
                    Indx = SHIFTR(Offset+I, 3)
                    CALL ByteUnpackLE(Instance%StateAsWords(Indx), OutBuf, I)
                END DO
                OutDat(0:Length-1) = OutBuf(0:Length-1)
                END BLOCK Inner
            END IF
        END BLOCK Outer
    END IF

    RETURN

END SUBROUTINE KP1600Sponge_ExtractBytes

!******************************************************************************

SUBROUTINE KP1600Sponge_Finalize(Instance)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(KP1600Sponge), INTENT(INOUT)   :: Instance     !! 'Sponge' instance

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Instance%StateAsWords => NULL()

    RETURN

END SUBROUTINE KP1600Sponge_Finalize

!******************************************************************************

END MODULE MClass_KP1600Sponge

!******************************************************************************
