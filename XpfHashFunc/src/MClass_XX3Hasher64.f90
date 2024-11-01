
MODULE MClass_XX3Hasher64

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *XX3Hasher64* and *XX3Hasher128* types and their
!   related routines.  The *XX3Hasher64* type is a hasher type that extends directly
!   from the <a href="../module/mclass_hasher64.html#type-hasher64">Hasher64</a> type.
!   It provides all deferred procedures required by a *Hasher64* class and outputs the
!   hash value as a 64-bit integer.  The *XX3Hasher128* type is a hasher type that
!   extends from the *XX3Hasher64* type and provides additional methods to output
!   the hash value as a 128-bit integer. <br>
!   Both hashers employ the *XX3* hash algorithm by Yann Collet [1, 2].  As hashers,
!   they can be used to compute the hash value incrementally.  They also provide a
!   method to compute the hash value directly (i.e. non-incrementally).  The following
!   code snippet shows a typical usage of the hashers.
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
!   However, if the *Update* method is to be called only one time, then the *HashDirect*
!   method should be employed as follows.
!   <Pre><Code style="color:MidnightBlue;">
!   ! directly compute the hash value
!   HashCode = Hasher%HashDirect(Input, InpSize, Seed)
!   </Code></Pre>
!   To compute the hash value as a 128-bit integer, simply replace the *Finalize* and
!   *HashDirect* methods by the *Finalize128* and *HashDirect128* methods, respectively. <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://github.com/Cyan4973/xxHash">xxHash: Extremely fast hash algorithm. </a> <br>
!   [2] <a href="https://github.com/OpenHFT/Zero-Allocation-Hashing/tree/ea">
!       Zero-Allocation Hashing for Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil,   ONLY: LongReverseBytes    => ReverseBytes, &
                                  IntegerReverseBytes => ReverseBytes
    USE MBase_UIntUtil,   ONLY: UnsignedLongMultiplyHigh => UMul128_Upper64
    USE MBase_SInt128
    USE MClass_Hasher64

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: XX3Hasher64
    PUBLIC :: XX3Hasher128

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define     tSInt128        TYPE(SInt128)
#define     MaskI8(X)           IAND(ToInt64(X), ToInt64(Z'00000000000000FF'))
#define     MaskI32(X)          IAND(ToInt64(X), ToInt64(Z'00000000FFFFFFFF'))
#define     UnsignedInt(I)      IAND(ToInt64(I), ToInt64(Z'00000000FFFFFFFF'))
#define     XXH3_Mix16B(Seed,Input,OffIn,Secret,OffSec) \
    UnsignedLongMultiplyorFold(IEOR(Input(OffIn),   Secret(OffSec)   + Seed), \
                    IEOR(Input(OffIn+1), Secret(OffSec+1) - Seed))
#define     XXH3_Mix2Accs(Acc_LH,Acc_RH,Secret,OffSec) \
    UnsignedLongMultiplyorFold(IEOR(Acc_LH, Secret(OffSec)), IEOR(Acc_RH, Secret(OffSec+1)))
#define     XXH128_Mix32B_Once(Seed,Secret,OffSec,Acc,Input0,Input1,Input2,Input3) \
    IEOR(Acc + UnsignedLongMultiplyorFold(IEOR(Input0, (Secret(OffSec)   + Seed)),  \
                                          IEOR(Input1, (Secret(OffSec+1) - Seed))), \
                                              (Input2 + Input3))

!** MODULE PARAMETERS:
    tUInt64, PARAMETER  :: MaxU32 = ToInt64(Z'00000000FFFFFFFF')
    ! *****************************************************************
    ! *****     Pseudo-random secret taken directly from FARSH    *****
    ! *****************************************************************
    ! base secret parameters in bytes for non-optimized versions
    tUInt8,    PARAMETER    :: XXH3_Secret_Bytes(192) =                     &
               [ToInt8(Z'B8'), ToInt8(Z'FE'), ToInt8(Z'6C'), ToInt8(Z'39'), &
                ToInt8(Z'23'), ToInt8(Z'A4'), ToInt8(Z'4B'), ToInt8(Z'BE'), &
                ToInt8(Z'7C'), ToInt8(Z'01'), ToInt8(Z'81'), ToInt8(Z'2C'), &
                ToInt8(Z'F7'), ToInt8(Z'21'), ToInt8(Z'AD'), ToInt8(Z'1C'), &
                ToInt8(Z'DE'), ToInt8(Z'D4'), ToInt8(Z'6D'), ToInt8(Z'E9'), &
                ToInt8(Z'83'), ToInt8(Z'90'), ToInt8(Z'97'), ToInt8(Z'DB'), &
                ToInt8(Z'72'), ToInt8(Z'40'), ToInt8(Z'A4'), ToInt8(Z'A4'), &
                ToInt8(Z'B7'), ToInt8(Z'B3'), ToInt8(Z'67'), ToInt8(Z'1F'), &
                ToInt8(Z'CB'), ToInt8(Z'79'), ToInt8(Z'E6'), ToInt8(Z'4E'), &
                ToInt8(Z'CC'), ToInt8(Z'C0'), ToInt8(Z'E5'), ToInt8(Z'78'), &
                ToInt8(Z'82'), ToInt8(Z'5A'), ToInt8(Z'D0'), ToInt8(Z'7D'), &
                ToInt8(Z'CC'), ToInt8(Z'FF'), ToInt8(Z'72'), ToInt8(Z'21'), &
                ToInt8(Z'B8'), ToInt8(Z'08'), ToInt8(Z'46'), ToInt8(Z'74'), &
                ToInt8(Z'F7'), ToInt8(Z'43'), ToInt8(Z'24'), ToInt8(Z'8E'), &
                ToInt8(Z'E0'), ToInt8(Z'35'), ToInt8(Z'90'), ToInt8(Z'E6'), &
                ToInt8(Z'81'), ToInt8(Z'3A'), ToInt8(Z'26'), ToInt8(Z'4C'), &
                ToInt8(Z'3C'), ToInt8(Z'28'), ToInt8(Z'52'), ToInt8(Z'BB'), &
                ToInt8(Z'91'), ToInt8(Z'C3'), ToInt8(Z'00'), ToInt8(Z'CB'), &
                ToInt8(Z'88'), ToInt8(Z'D0'), ToInt8(Z'65'), ToInt8(Z'8B'), &
                ToInt8(Z'1B'), ToInt8(Z'53'), ToInt8(Z'2E'), ToInt8(Z'A3'), &
                ToInt8(Z'71'), ToInt8(Z'64'), ToInt8(Z'48'), ToInt8(Z'97'), &
                ToInt8(Z'A2'), ToInt8(Z'0D'), ToInt8(Z'F9'), ToInt8(Z'4E'), &
                ToInt8(Z'38'), ToInt8(Z'19'), ToInt8(Z'EF'), ToInt8(Z'46'), &
                ToInt8(Z'A9'), ToInt8(Z'DE'), ToInt8(Z'AC'), ToInt8(Z'D8'), &
                ToInt8(Z'A8'), ToInt8(Z'FA'), ToInt8(Z'76'), ToInt8(Z'3F'), &
                ToInt8(Z'E3'), ToInt8(Z'9C'), ToInt8(Z'34'), ToInt8(Z'3F'), &
                ToInt8(Z'F9'), ToInt8(Z'DC'), ToInt8(Z'BB'), ToInt8(Z'C7'), &
                ToInt8(Z'C7'), ToInt8(Z'0B'), ToInt8(Z'4F'), ToInt8(Z'1D'), &
                ToInt8(Z'8A'), ToInt8(Z'51'), ToInt8(Z'E0'), ToInt8(Z'4B'), &
                ToInt8(Z'CD'), ToInt8(Z'B4'), ToInt8(Z'59'), ToInt8(Z'31'), &
                ToInt8(Z'C8'), ToInt8(Z'9F'), ToInt8(Z'7E'), ToInt8(Z'C9'), &
                ToInt8(Z'D9'), ToInt8(Z'78'), ToInt8(Z'73'), ToInt8(Z'64'), &
                ToInt8(Z'EA'), ToInt8(Z'C5'), ToInt8(Z'AC'), ToInt8(Z'83'), &
                ToInt8(Z'34'), ToInt8(Z'D3'), ToInt8(Z'EB'), ToInt8(Z'C3'), &
                ToInt8(Z'C5'), ToInt8(Z'81'), ToInt8(Z'A0'), ToInt8(Z'FF'), &
                ToInt8(Z'FA'), ToInt8(Z'13'), ToInt8(Z'63'), ToInt8(Z'EB'), &
                ToInt8(Z'17'), ToInt8(Z'0D'), ToInt8(Z'DD'), ToInt8(Z'51'), &
                ToInt8(Z'B7'), ToInt8(Z'F0'), ToInt8(Z'DA'), ToInt8(Z'49'), &
                ToInt8(Z'D3'), ToInt8(Z'16'), ToInt8(Z'55'), ToInt8(Z'26'), &
                ToInt8(Z'29'), ToInt8(Z'D4'), ToInt8(Z'68'), ToInt8(Z'9E'), &
                ToInt8(Z'2B'), ToInt8(Z'16'), ToInt8(Z'BE'), ToInt8(Z'58'), &
                ToInt8(Z'7D'), ToInt8(Z'47'), ToInt8(Z'A1'), ToInt8(Z'FC'), &
                ToInt8(Z'8F'), ToInt8(Z'F8'), ToInt8(Z'B8'), ToInt8(Z'D1'), &
                ToInt8(Z'7A'), ToInt8(Z'D0'), ToInt8(Z'31'), ToInt8(Z'CE'), &
                ToInt8(Z'45'), ToInt8(Z'CB'), ToInt8(Z'3A'), ToInt8(Z'8F'), &
                ToInt8(Z'95'), ToInt8(Z'16'), ToInt8(Z'04'), ToInt8(Z'28'), &
                ToInt8(Z'AF'), ToInt8(Z'D7'), ToInt8(Z'FB'), ToInt8(Z'CA'), &
                ToInt8(Z'BB'), ToInt8(Z'4B'), ToInt8(Z'40'), ToInt8(Z'7E')]
    ! base secret parameters in longs used for general optimization
    tUInt64,   PARAMETER    :: XXH3_Secret_Long1(0:23) = &
               [ToInt64(Z'BE4BA423396CFEB8'), ToInt64(Z'1CAD21F72C81017C'), &
                ToInt64(Z'DB979083E96DD4DE'), ToInt64(Z'1F67B3B7A4A44072'), &
                ToInt64(Z'78E5C0CC4EE679CB'), ToInt64(Z'2172FFCC7DD05A82'), &
                ToInt64(Z'8E2443F7744608B8'), ToInt64(Z'4C263A81E69035E0'), &
                ToInt64(Z'CB00C391BB52283C'), ToInt64(Z'A32E531B8B65D088'), &
                ToInt64(Z'4EF90DA297486471'), ToInt64(Z'D8ACDEA946EF1938'), &
                ToInt64(Z'3F349CE33F76FAA8'), ToInt64(Z'1D4F0BC7C7BBDCF9'), &
                ToInt64(Z'3159B4CD4BE0518A'), ToInt64(Z'647378D9C97E9FC8'), &
                ToInt64(Z'C3EBD33483ACC5EA'), ToInt64(Z'EB6313FAFFA081C5'), &
                ToInt64(Z'49DAF0B751DD0D17'), ToInt64(Z'9E68D429265516D3'), &
                ToInt64(Z'FCA1477D58BE162B'), ToInt64(Z'CE31D07AD1B8F88F'), &
                ToInt64(Z'280416958F3ACB45'), ToInt64(Z'7E404BBBCAFBD7AF')]
    ! base secret parameters in longs used for medium-message (129-240) optimization
    tUInt64,   PARAMETER    :: XXH3_Secret_Long2(0:13) = &
               [ToInt64(Z'81017CBE4BA42339'), ToInt64(Z'6DD4DE1CAD21F72C'), &
                ToInt64(Z'A44072DB979083E9'), ToInt64(Z'E679CB1F67B3B7A4'), &
                ToInt64(Z'D05A8278E5C0CC4E'), ToInt64(Z'4608B82172FFCC7D'), &
                ToInt64(Z'9035E08E2443F774'), ToInt64(Z'52283C4C263A81E6'), &
                ToInt64(Z'65D088CB00C391BB'), ToInt64(Z'486471A32E531B8B'), &
                ToInt64(Z'EF19384EF90DA297'), ToInt64(Z'76FAA8D8ACDEA946'), &
                ToInt64(Z'BBDCF93F349CE33F'), ToInt64(Z'E0518A1D4F0BC7C7')]
    tUInt64,   PARAMETER    :: XXH3_Secret_Long3(0:3) = &
               [ToInt64(Z'4F0BC7C7BBDCF93F'), ToInt64(Z'59B4CD4BE0518A1D'), &
                ToInt64(Z'7378D9C97E9FC831'), ToInt64(Z'EBD33483ACC5EA64')]
    ! base secret parameters in longs used for long-message (240 up) optimization
    tUInt64,   PARAMETER    :: XXH3_Secret_Long4(0:7) = &
               [ToInt64(Z'EA647378D9C97E9F'), ToInt64(Z'C5C3EBD33483ACC5'), &
                ToInt64(Z'17EB6313FAFFA081'), ToInt64(Z'D349DAF0B751DD0D'), &
                ToInt64(Z'2B9E68D429265516'), ToInt64(Z'8FFCA1477D58BE16'), &
                ToInt64(Z'45CE31D07AD1B8F8'), ToInt64(Z'AF280416958F3ACB')]
    tUInt64,   PARAMETER    :: XXH3_Secret_Long5(0:7) = &
               [ToInt64(Z'6DD4DE1CAD21F72C'), ToInt64(Z'A44072DB979083E9'), &
                ToInt64(Z'E679CB1F67B3B7A4'), ToInt64(Z'D05A8278E5C0CC4E'), &
                ToInt64(Z'4608B82172FFCC7D'), ToInt64(Z'9035E08E2443F774'), &
                ToInt64(Z'52283C4C263A81E6'), ToInt64(Z'65D088CB00C391BB')]
    tUInt64,   PARAMETER    :: XXH3_Secret_Long6(0:7) = &
               [ToInt64(Z'D9C97E9FC83159B4'), ToInt64(Z'3483ACC5EA647378'), &
                ToInt64(Z'FAFFA081C5C3EBD3'), ToInt64(Z'B751DD0D17EB6313'), &
                ToInt64(Z'29265516D349DAF0'), ToInt64(Z'7D58BE162B9E68D4'), &
                ToInt64(Z'7AD1B8F88FFCA147'), ToInt64(Z'958F3ACB45CE31D0')]
    ! base secret parameters in integers used for short-message (1-16) optimization
    tUInt32,   PARAMETER    :: XXH3_Secret_Integer(0:3) = &
               [ToInt32(Z'396CFEB8'), ToInt32(Z'BE4BA423'), &
                ToInt32(Z'2C81017C'), ToInt32(Z'1CAD21F7')]
    ! Primes for 32 bits
    tUInt64, PARAMETER  :: XXH_PRIME32_1 = ToInt64(Z'000000009E3779B1')  ! < 0b10011110001101110111100110110001 >
    tUInt64, PARAMETER  :: XXH_PRIME32_2 = ToInt64(Z'0000000085EBCA77')  ! < 0b10000101111010111100101001110111 >
    tUInt64, PARAMETER  :: XXH_PRIME32_3 = ToInt64(Z'00000000C2B2AE3D')  ! < 0b11000010101100101010111000111101 >
    ! Primes for 64 bits
    tUInt64, PARAMETER  :: XXH_PRIME64_1 = ToInt64(Z'9E3779B185EBCA87')  ! < 0b1001111000110111011110011011000110000101111010111100101010000111 >
    tUInt64, PARAMETER  :: XXH_PRIME64_2 = ToInt64(Z'C2B2AE3D27D4EB4F')  ! < 0b1100001010110010101011100011110100100111110101001110101101001111 >
    tUInt64, PARAMETER  :: XXH_PRIME64_3 = ToInt64(Z'165667B19E3779F9')  ! < 0b0001011001010110011001111011000110011110001101110111100111111001 >
    tUInt64, PARAMETER  :: XXH_PRIME64_4 = ToInt64(Z'85EBCA77C2B2AE63')  ! < 0b1000010111101011110010100111011111000010101100101010111001100011 >
    tUInt64, PARAMETER  :: XXH_PRIME64_5 = ToInt64(Z'27D4EB2F165667C5')  ! < 0b0010011111010100111010110010111100010110010101100110011111000101 >
    tIndex,  PARAMETER  :: BlockLen = 256_kIndex

!** DERIVED TYPE DEFINITIONS
    !> *XX3Hasher64* is a hasher type that outputs the hash value as a 64-bit integer.
    !  It employs the *XX3* hash algorithm by Yann Collet.
    TYPE, EXTENDS(Hasher64) :: XX3Hasher64
        PRIVATE
        !% seed
        tUInt64         :: Seed                 = 0_kInt64
        !% state
        tUInt64         :: State(0:7)           = 0_kInt64
        !% buffer array used to store input data
        tUInt8          :: BufArr(0:BlockLen-1) = 0_kInt8
        !% pointer to custom secret
        tUInt8, POINTER :: Secret(:)            => NULL()
        !% number of stripes processed
        tIndex          :: nStripe              = 0_kIndex
        !% flag indicating whether to remove sign from the final hash value
        tLogical        :: RemoveSign           = FalseVal
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName              => XX3_GetName
        !> *GetBlockLength* is a procedure to return the block length of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockLength       => XX3_BlockLength
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: SetBufPtr            => XX3_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: ProcessBlock         => XX3_ProcessBlock
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the hasher. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Initialize(Seed)            ! hash value with sign <br>
        !   --->    CALL Hasher%Initialize(Seed, .TRUE.)    ! remove sign from hash value <br>
        PROCEDURE   :: Initialize           => XX3_Initialize
        !> **Type-Bound Subroutine**: InitializeWSecret <br>
        !  **Purpose**:  To initialize the hasher with specified secret. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%InitializeWSecret(Seed, Secret)         ! hash value with sign <br>
        !   --->    CALL Hasher%InitializeWSecret(Seed, Secret, .TRUE.) ! remove sign from hash value <br>
        PROCEDURE   :: InitializeWSecret    => XX3_Initialize_wSecret
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 64-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize             => XX3_Finalize
        !> **Type-Bound Function**: HashDirect <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect           => XX3_HashDirect
        !> **Type-Bound Function**: HashDirectWSecret <br>
        !  **Purpose**:  To compute the hash value directly with specified seed and secret. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirectWSecret(Input, InpSize, Seed, Secret) <br>
        !   --->    HashCode = Hasher%HashDirectWSecret(Input, InpSize, Seed, Secret, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirectWSecret    => XX3_HashDirect_wSecret
    END TYPE XX3Hasher64
    !> *XX3Hasher128* is a hasher type that outputs the hash value as a 128-bit integer.
    !  It is a subtype of the *XX3Hasher64* type.
    TYPE, EXTENDS(XX3Hasher64) :: XX3Hasher128
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName              => XX3_GetName128
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 64-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize             => XX3_Finalize64
        !> **Type-Bound Function**: Finalize128 <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 128-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize128()
        PROCEDURE   :: Finalize128          => XX3_Finalize128
        !> **Type-Bound Function**: HashDirect128 <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect128(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect128(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect128(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect128        => XX3_HashDirect128
        !> **Type-Bound Function**: HashDirect128WSecret <br>
        !  **Purpose**:  To compute the hash value directly with specified seed and secret. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect128WSecret(Input, InpSize, Seed, Secret) <br>
        !   --->    HashCode = Hasher%HashDirect128WSecret(Input, InpSize, Seed, Secret, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect128WSecret => XX3_HashDirect128_wSecret
    END TYPE XX3Hasher128

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    tUInt8, TARGET  :: CustomSecret(0:191)

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION XX3_GetName(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher64), INTENT(IN)  :: HS   !! a hasher (HS) object
    tCharAlloc                      :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Name = 'XX3_Hahser64'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION XX3_GetName

!******************************************************************************

FUNCTION XX3_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher64), INTENT(IN)  :: HS       !! a hasher (HS) object
    tIndex                          :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Length = BlockLen
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION XX3_BlockLength

!******************************************************************************

SUBROUTINE XX3_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher64), TARGET, INTENT(INOUT)   :: HS           !! a hasher (HS) object
    tUInt8,            POINTER, INTENT(INOUT)   :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => HS%BufArr

    RETURN

END SUBROUTINE XX3_SetBufPtr

!******************************************************************************

SUBROUTINE XX3_ProcessBlock(HS, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher64), INTENT(INOUT)   :: HS           !! a hasher (HS) object
    tUInt8,             INTENT(IN)      :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: DataVal0, DataVal1
    tUInt64         :: DataKey0, DataKey1
    tIndex          :: I, J, OffStripe, Offsec
    tUInt8          :: InpBytes(0:63)
    tUInt64         :: InpLongs(1:8)
    EQUIVALENCE(InpBytes, InpLongs)
    tUInt8          :: SecBytes(0:63)
    tUInt64         :: SecLongs(1:8)
    EQUIVALENCE(SecBytes, SecLongs)

!** FLOW

    IF (ASSOCIATED(HS%Secret)) THEN
        ASSOCIATE (nStripe => HS%nStripe, Acc => HS%State)
            ! XXH3_accumulate_256
            OffStripe = 0_kIndex
            OffSec    = nStripe*8_kIndex
            ! process 4 stripes at a time
            DO I = 1, 4
                InpBytes(0:63) = BytesIn(OffStripe:OffStripe+63)
                SecBytes(0:63) = HS%Secret(OffSec:OffSec+63)
                DO J = 0, 7, 2
                    DataVal0 = InpLongs(J+1)
                    DataVal1 = InpLongs(J+2)
                    DataKey0 = IEOR(DataVal0, SecLongs(J+1))
                    DataKey1 = IEOR(DataVal1, SecLongs(J+2))
                    ! swap adjacent lanes
                    Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                    Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
                END DO
                nStripe = nStripe + 1_kIndex
                OffStripe = OffStripe + 64_kIndex
            END DO
            IF (nStripe == 16_kIndex) THEN
                ! XXH3_scrambleAcc_scalar
                OffSec = 128    ! 192 - 64
                SecBytes(0:63) = HS%Secret(128:191)
                DO J = 0, 7
                    Acc(J) = (IEOR(IEOR(Acc(J), SHIFTR(Acc(J), 47)), SecLongs(J+1)))*XXH_PRIME32_1
                END DO
                ! reset
                nStripe = 0_kIndex
            END IF
        END ASSOCIATE
    ELSE
        ASSOCIATE (Secret1 => XXH3_Secret_Long1, nStripe => HS%nStripe, Acc => HS%State)
            ! XXH3_accumulate_256
            OffStripe = 0_kIndex
            ! process 4 stripes at a time
            DO I = 1, 4
                InpBytes(0:63) = BytesIn(OffStripe:OffStripe+63)
                DO J = 0, 7, 2
                    DataVal0 = InpLongs(J+1)
                    DataVal1 = InpLongs(J+2)
                    DataKey0 = IEOR(DataVal0, Secret1(nStripe+J))
                    DataKey1 = IEOR(DataVal1, Secret1(nStripe+J+1))
                    ! swap adjacent lanes
                    Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                    Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
                END DO
                nStripe = nStripe + 1_kIndex
                OffStripe = OffStripe + 64_kIndex
            END DO
            IF (nStripe == 16_kIndex) THEN
                ! XXH3_scrambleAcc_scalar
                DO J = 0, 7
                    Acc(J) = (IEOR(IEOR(Acc(J), SHIFTR(Acc(J), 47)), &
                                        Secret1(nStripe+J)))*XXH_PRIME32_1
                END DO
                ! reset
                nStripe = 0_kIndex
            END IF
        END ASSOCIATE
    END IF

    RETURN

END SUBROUTINE XX3_ProcessBlock

!******************************************************************************

SUBROUTINE XX3_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher64), TARGET, INTENT(INOUT)   :: HS   !! a hasher (HS) object
    tUInt64,                    INTENT(IN)      :: Seed !! seed
    tLogical,         OPTIONAL, INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    HS%Seed = Seed
    HS%State(0) = XXH_PRIME32_3
    HS%State(1) = XXH_PRIME64_1
    HS%State(2) = XXH_PRIME64_2
    HS%State(3) = XXH_PRIME64_3
    HS%State(4) = XXH_PRIME64_4
    HS%State(5) = XXH_PRIME32_2
    HS%State(6) = XXH_PRIME64_5
    HS%State(7) = XXH_PRIME32_1
    HS%Secret => NULL()
    HS%nStripe = 0_kIndex
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE XX3_Initialize

!******************************************************************************

SUBROUTINE XX3_Initialize_wSecret(HS, Seed, Secret, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher64), TARGET, INTENT(INOUT)   :: HS   !! a hasher (HS) object
    tUInt64,                    INTENT(IN)      :: Seed !! seed
    tUInt8,             TARGET, INTENT(IN)      :: Secret(0:)
    !^ a byte (8-bit integer) array (of at least 192 bytes) representing
    !  a custom secret <br>
    tLogical,         OPTIONAL, INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    HS%Seed = Seed
    HS%State(0) = XXH_PRIME32_3
    HS%State(1) = XXH_PRIME64_1
    HS%State(2) = XXH_PRIME64_2
    HS%State(3) = XXH_PRIME64_3
    HS%State(4) = XXH_PRIME64_4
    HS%State(5) = XXH_PRIME32_2
    HS%State(6) = XXH_PRIME64_5
    HS%State(7) = XXH_PRIME32_1
    IF (SIZE(Secret) >= 192) THEN
        HS%Secret => Secret
    ELSE
        BLOCK
            tUInt32     :: InSize
            InSize = SIZE(Secret)
            CustomSecret(1:InSize)  = Secret(1:InSize)
            CustomSecret(InSize+1:) = XXH3_Secret_Bytes(1:)
            HS%Secret => CustomSecret
        END BLOCK
    END IF
    HS%nStripe = 0_kIndex
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE XX3_Initialize_wSecret

!******************************************************************************

FUNCTION XX3_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 64-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher64), INTENT(INOUT)   :: HS       !! a hasher (HS) object
    tUInt64                             :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length, Remaining

!** FLOW

    Remaining = HS%GetBufLen()
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        Length = HS%GetBlockCount()*HS%GetBlockLength() + Remaining
    ELSE
        Length = Remaining
    END IF
    IF (Length > 240) THEN
        ASSOCIATE (Input => HS%BufArr, Acc => HS%State, Secret => HS%Secret)
            IF (ASSOCIATED(HS%Secret)) THEN
                CALL XX3Hash_Len241Up_Custom(Acc, Input, Remaining, Length, Secret, HashCode)
            ELSE
                CALL XX3Hash_Len241Up_Base(Acc, Input, Remaining, Length,        &
                                           XXH3_Secret_Long1, XXH3_Secret_Long4, &
                                           XXH3_Secret_Long5, XXH3_Secret_Long6, HashCode)
            END IF
        END ASSOCIATE
    ELSE
        ASSOCIATE (Input => HS%BufArr, Seed => HS%Seed)
            IF (Length <= 16) THEN
                HashCode = XX3Hash64_Len1To16(Input, Seed, Length, XXH3_Secret_Integer, &
                                              XXH3_Secret_Long1)
            ELSEIF (Length <= 128) THEN
                HashCode = XX3Hash64_Len17To128(Input, Seed, Length, XXH3_Secret_Long1)
            ELSEIF (Length <= 240) THEN
                HashCode = XX3Hash64_Len129To240(Input, Seed, Length, XXH3_Secret_Long1, &
                                                 XXH3_Secret_Long2, XXH3_Secret_Long3)
            END IF
        END ASSOCIATE
    END IF

    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInt64(Z'7FFFFFFFFFFFFFFF'))

    ! reset the hasher
    HS%Seed   = 0_kInt64
    HS%State  = 0_kInt64
    HS%BufArr = 0_kInt8
    HS%nStripe = 0_kIndex
    HS%RemoveSign = FalseVal
    CALL HS%Reset()
    NULLIFY(HS%Secret)

    RETURN

CONTAINS

    FUNCTION XX3Hash64_Len1To16(Input, Seed, Length, IntSecret, &
                                LongSecret) RESULT(HashCode)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt8,   INTENT(IN)    :: Input(0:)            ! input bytes
        tUInt64,  INTENT(IN)    :: Seed                 ! seed
        tIndex,   INTENT(IN)    :: Length               ! length of input bytes
        tUInt32,  INTENT(IN)    :: IntSecret(0:3)       ! base secret in integers
        tUInt64,  INTENT(IN)    :: LongSecret(0:23)     ! base secret in longs
        tUInt64                 :: HashCode             ! hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64         :: BitFlip1, BitFlip2, Input_Lo, Input_Hi, Acc
        tUInt64         :: S, Input1, Input2, BitFlip, Keyed
        tUInt32         :: C1, C2, C3
        tUInt64         :: Combined
        ! variables used to quickly access input and secret bytes
        tUInt8          :: InpBytes(0:127)
        tUInt64         :: InpLongs(1:16)
        EQUIVALENCE(InpBytes, InpLongs)
        tUInt8          :: Bytes(0:7)
        tUInt32         :: I32Val(2)
        EQUIVALENCE(Bytes, I32Val)

    !** FLOW

        ! XXH3_Len_0to16_64b
        IF (Length > 8) THEN
            ! XXH3_Len_9to16_64b
            BitFlip1 = IEOR(LongSecret(3), LongSecret(4)) + Seed
            BitFlip2 = IEOR(LongSecret(5), LongSecret(6)) - Seed
            InpBytes(0:7) = Input(0:7)
            InpBytes(8:15) = Input(Length-8:Length-1)
            Input_Lo = IEOR(InpLongs(1), BitFlip1)
            Input_Hi = IEOR(InpLongs(2), BitFlip2)
            Acc = ToInt64(Length) + LongReverseBytes(Input_Lo) + Input_Hi + &
                    UnsignedLongMultiplyorFold(Input_Lo, Input_Hi)
            HashCode = XXH3_Avalanche(Acc)
            RETURN
        END IF
        IF (Length >= 4) THEN
            ! XXH3_Len_4to8_64b
            S = IEOR(Seed, LongReverseBytes(IAND(Seed, MaxU32)))
            Bytes(0:3) = Input(0:3)
            Bytes(4:7) = Input(Length-4:Length-1)
            Input1 = ToInt64(I32Val(1))      ! high int will be shifted
            Input2 = MaskI32(I32Val(2))
            BitFlip = IEOR(LongSecret(1), LongSecret(2)) - S
            Keyed = IEOR((Input2 + SHIFTL(Input1, 32)), BitFlip)
            HashCode = XXH3_RRMxMx(Keyed, Length)
            RETURN
        END IF
        IF (Length /= 0) THEN
            ! XXH3_Len_1to3_64b
            C1 = MaskI8(Input(0))
            C2 = ToInt32(Input(SHIFTA(Length, 1)))   ! high 3 bytes will be shifted
            C3 = MaskI8(Input(Length - 1))
            Combined = UnsignedInt(ToInt32(IOR(ToIndex(IOR(IOR(SHIFTL(C1, 16), SHIFTL(C2, 24)), C3)), \
                                   SHIFTL(Length, 8))))
            BitFlip = UnsignedInt(IEOR(IntSecret(0), IntSecret(1))) + Seed
            HashCode = IEOR(Combined, BitFlip)
            HashCode = XXH64_Avalanche(HashCode)
            RETURN
        END IF
        HashCode = IEOR(IEOR(Seed, LongSecret(7)), LongSecret(8))
        HashCode = XXH64_Avalanche(HashCode)

        RETURN

    END FUNCTION XX3Hash64_Len1To16

    !***************************************************************************

    FUNCTION XX3Hash64_Len17To128(Input, Seed, Length, LongSecret) RESULT(HashCode)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt8,  INTENT(IN)  :: Input(0:)            ! input bytes
        tUInt64, INTENT(IN)  :: Seed                 ! seed
        tIndex,  INTENT(IN)  :: Length               ! length of input bytes
        tUInt64, INTENT(IN)  :: LongSecret(0:23)     ! base secret
        tUInt64              :: HashCode             ! hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64         :: Acc
        ! variables used to quickly access input and secret bytes
        tUInt8          :: InpBytes(0:127)
        tUInt64         :: InpLongs(1:16)
        EQUIVALENCE(InpBytes, InpLongs)

    !** FLOW

        ! XXH3_Len_17to128_64b
        Acc = ToInt64(Length)*XXH_PRIME64_1
        IF (Length > 32) THEN
            IF (Length > 64) THEN
                IF (Length > 96) THEN
                    InpBytes(0:63)   = Input(0:63)
                    InpBytes(64:127) = Input(Length-64:Length-1)
                    Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 7, LongSecret, 12)
                    Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 9, LongSecret, 14)
                ELSE
                    InpBytes(0:47)   = Input(0:47)
                    InpBytes(80:127) = Input(Length-48:Length-1)
                END IF
                Acc = Acc + XXH3_Mix16B(Seed, InpLongs,  5, LongSecret, 8)
                Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 11, LongSecret, 10)
            ELSE
                InpBytes(0:31)   = Input(0:31)
                InpBytes(96:127) = Input(Length-32:Length-1)
            END IF
            Acc = Acc + XXH3_Mix16B(Seed, InpLongs,  3, LongSecret, 4)
            Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 13, LongSecret, 6)
        ELSE
            InpBytes(0:15)    = Input(0:15)
            InpBytes(112:127) = Input(Length-16:Length-1)
        END IF
        Acc = Acc + XXH3_Mix16B(Seed, InpLongs,  1, LongSecret, 0)
        Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 15, LongSecret, 2)
        HashCode = XXH3_Avalanche(Acc)

        RETURN

    END FUNCTION XX3Hash64_Len17To128

    !***************************************************************************

    FUNCTION XX3Hash64_Len129To240(Input, Seed, Length, LongSecret1, &
                                   LongSecret2, LongSecret3) RESULT(HashCode)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt8,  INTENT(IN)  :: Input(0:)            ! input bytes
        tUInt64, INTENT(IN)  :: Seed                 ! seed
        tIndex,  INTENT(IN)  :: Length               ! length of input bytes
        tUInt64, INTENT(IN)  :: LongSecret1(0:23)    ! base secret
        tUInt64, INTENT(IN)  :: LongSecret2(0:13)    ! base secret
        tUInt64, INTENT(IN)  :: LongSecret3(0:3)     ! base secret
        tUInt64              :: HashCode             ! hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64         :: Acc
        tIndex          :: NbRounds, I
        ! variables used to quickly access input and secret bytes
        tUInt8          :: InpBytes(0:127)
        tUInt64         :: InpLongs(1:16)
        EQUIVALENCE(InpBytes, InpLongs)

    !** FLOW

        ! XXH3_Len_129to240_64b
        Acc = ToInt64(Length)*XXH_PRIME64_1
        NbRounds = Length / 16
        I = 0
        InpBytes(0:127) = Input(0:127)
        DO WHILE (I < 8)
            Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 2*I+1, LongSecret1, 2*I)
            I = I + 1
        END DO
        Acc = XXH3_Avalanche(Acc)
        InpBytes(0:(NbRounds*2-16)*8-1) = Input(128:128+(NbRounds*2-16)*8-1)
        DO WHILE (I < NbRounds)
            Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 2*(I-8)+1, LongSecret2, 2*(I-8))
            I = I + 1
        END DO
        ! last bytes
        InpBytes(0:15) = Input(Length-16:Length-1)
        Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 1, LongSecret3, 2)
        HashCode = XXH3_Avalanche(Acc)

        RETURN

    END FUNCTION XX3Hash64_Len129To240

    !******************************************************************************

END FUNCTION XX3_Finalize

!******************************************************************************

FUNCTION XX3_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher64),     INTENT(INOUT)   :: HS           !! a hasher (HS) object
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

END FUNCTION XX3_HashDirect

!******************************************************************************

FUNCTION XX3_HashDirect_wSecret(HS, Input, InpSize, Seed, Secret, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher64),     INTENT(INOUT)   :: HS           !! a hasher (HS) object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tUInt64,                INTENT(IN)      :: Seed         !! seed
    tUInt8,                 INTENT(IN)      :: Secret(:)
    !^ a byte (8-bit integer) array (of at least 192 bytes) representing
    !  a custom secret <br>
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
    CALL HS%InitializeWSecret(Seed, Secret, RemoveSign)
    CALL HS%Update(Input, InpSize)
    HashCode = HS%Finalize()

    RETURN

END FUNCTION XX3_HashDirect_wSecret

!******************************************************************************

FUNCTION XX3_GetName128(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher128), INTENT(IN) :: HS   !! a hasher (HS) object
    tCharAlloc                      :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Name = 'XX3_Hahser128'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION XX3_GetName128

!******************************************************************************

FUNCTION XX3_Finalize64(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 64-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher128), INTENT(INOUT)  :: HS       !! a hasher (HS) object
    tUInt64                             :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL XX3_Finalize_Hash128(HS, HashCode)

    RETURN

END FUNCTION XX3_Finalize64

!******************************************************************************

FUNCTION XX3_Finalize128(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 128-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher128), INTENT(INOUT)  :: HS       !! a hasher (HS) object
    tSInt128                            :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL XX3_Finalize_Hash128(HS, HashCode%Low, HashCode%High)

    RETURN

END FUNCTION XX3_Finalize128

!******************************************************************************

SUBROUTINE XX3_Finalize_Hash128(HS, HashLo, HashHi)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 128-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher128), INTENT(INOUT)  :: HS       !! a hasher (HS) object
    tUInt64,             INTENT(OUT)    :: HashLo   !! lower 64-bit hash code
    tUInt64, OPTIONAL,   INTENT(OUT)    :: HashHi   !! upper 64-bit hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length, Remaining

!** FLOW

    Remaining = HS%GetBufLen()
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        Length = HS%GetBlockCount()*HS%GetBlockLength() + Remaining
    ELSE
        Length = Remaining
    END IF
    IF (Length > 240) THEN
        ASSOCIATE (Input => HS%BufArr, Acc => HS%State, Secret => HS%Secret)
            IF (ASSOCIATED(HS%Secret)) THEN
                CALL XX3Hash_Len241Up_Custom(Acc, Input, Remaining, Length, Secret, HashLo, HashHi)
            ELSE
                CALL XX3Hash_Len241Up_Base(Acc, Input, Remaining, Length,        &
                                           XXH3_Secret_Long1, XXH3_Secret_Long4, &
                                           XXH3_Secret_Long5, XXH3_Secret_Long6, &
                                           HashLo, HashHi)
            END IF
        END ASSOCIATE
    ELSE
        ASSOCIATE (Input => HS%BufArr, Seed => HS%Seed)
            IF (Length <= 16) THEN
                CALL XX3Hash128_Len1To16(Input, Seed, Length, XXH3_Secret_Integer, &
                                         XXH3_Secret_Long1, HashLo, HashHi)
            ELSEIF (Length <= 128) THEN
                CALL XX3Hash128_Len17To128(Input, Seed, Length, XXH3_Secret_Long1, &
                                           HashLo, HashHi)
            ELSEIF (Length <= 240) THEN
                CALL XX3Hash128_Len129To240(Input, Seed, Length, XXH3_Secret_Long1, &
                                            XXH3_Secret_Long2, XXH3_Secret_Long3, &
                                            HashLo, HashHi)
            END IF
        END ASSOCIATE
    END IF

    ! remove sign if needed
    IF (HS%RemoveSign) HashHi = IAND(HashHi, ToInt64(Z'7FFFFFFFFFFFFFFF'))

    ! reset the hasher
    HS%Seed   = 0_kInt64
    HS%State  = 0_kInt64
    HS%BufArr = 0_kInt8
    HS%nStripe = 0_kIndex
    HS%RemoveSign = FalseVal
    CALL HS%Reset()
    NULLIFY(HS%Secret)

    RETURN

CONTAINS

    SUBROUTINE XX3Hash128_Len1To16(Input, Seed, Length, IntSecret, LongSecret, HashLo, HashHi)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code using the XX3Hash128 hash algorithm by Yann Collet.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt8,            INTENT(IN)   :: Input(0:)        ! input bytes
        tUInt64,           INTENT(IN)   :: Seed             ! seed
        tIndex,            INTENT(IN)   :: Length           ! length of input bytes
        tUInt32,           INTENT(IN)   :: IntSecret(0:3)   ! base secret in integers
        tUInt64,           INTENT(IN)   :: LongSecret(0:23) ! base secret in longs
        tUInt64,           INTENT(OUT)  :: HashLo           ! lower 64-bit hash code
        tUInt64, OPTIONAL, INTENT(OUT)  :: HashHi           ! upper 64-bit hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64         :: BitFlipL, BitFlipH, Input_Lo, Input_Hi
        tUInt64         :: S, PL, Bitflip, Keyed, M128_Lo, M128_Hi
        tUInt32         :: C1, C2, C3, CombinedL, CombinedH
        ! variables used to quickly access input bytes
        tUInt8          :: InpBytes(0:127)
        tUInt64         :: InpLongs(1:16)
        EQUIVALENCE(InpBytes, InpLongs)
        tUInt8          :: Bytes(0:7)
        tUInt32         :: I32Val(1:2)
        EQUIVALENCE(Bytes, I32Val)

    !** FLOW

        ! perform hashing
        ! XXH3_len_0to16_128b
        IF (Length > 8) THEN
            ! XXH3_len_9to16_128b
            BitflipL = IEOR(LongSecret(4), LongSecret(5)) - Seed
            BitflipH = IEOR(LongSecret(6), LongSecret(7)) + Seed
            InpBytes(0:7) = Input(Length-8:Length-1)
            InpBytes(8:15) = Input(0:7)
            Input_Hi = InpLongs(1)
            Input_Lo = IEOR(IEOR(InpLongs(2), Input_Hi), BitflipL)
            M128_Lo = Input_Lo*XXH_PRIME64_1
            M128_Hi = UnsignedLongMultiplyHigh(Input_Lo, XXH_PRIME64_1)
            M128_Lo = M128_Lo + SHIFTL(ToInt64(Length - 1), 54)
            Input_Hi = IEOR(Input_Hi, BitflipH)
            M128_Hi = M128_Hi + Input_Hi + &
                        UnsignedInt(ToInt32(Input_Hi))*(XXH_PRIME32_2 - 1_kInt64)
            M128_Lo = IEOR(M128_Lo, LongReverseBytes(M128_Hi))
            HashLo = XXH3_Avalanche(M128_Lo*XXH_PRIME64_2)
            IF (PRESENT(HashHi)) THEN
                HashHi = XXH3_Avalanche(UnsignedLongMultiplyHigh(M128_Lo, XXH_PRIME64_2) &
                            + M128_Hi*XXH_PRIME64_2)
            END IF
            RETURN
        END IF
        IF (Length >= 4) THEN
            ! XXH3_len_4to8_128b
            S = IEOR(Seed, LongReverseBytes(IAND(Seed, MaxU32)))
            ! high int will be shifted
            Bytes(0:3) = Input(0:3)
            Bytes(4:7) = Input(Length-4:Length-1)
            Input_Lo = MaskI32(I32Val(1))
            Input_Hi = ToInt64(I32Val(2))
            Bitflip = IEOR(LongSecret(2), LongSecret(3)) + S
            Keyed = IEOR(Input_Lo + SHIFTL(Input_Hi, 32), Bitflip)
            ! Shift len to the left to ensure it is even, this avoids even multiplies.
            PL = XXH_PRIME64_1 + ToInt64(SHIFTL(Length, 2))
            M128_Lo = Keyed*PL
            M128_Hi = UnsignedLongMultiplyHigh(Keyed, PL)
            M128_Hi = M128_Hi + SHIFTL(M128_Lo, 1)
            M128_Lo = IEOR(M128_Lo, SHIFTR(M128_Hi, 3))
            M128_Lo = IEOR(M128_Lo, SHIFTR(M128_Lo, 35))
            M128_Lo = M128_Lo*ToInt64(Z'9FB21C651E98DF25')
            M128_Lo = IEOR(M128_Lo, SHIFTR(M128_Lo, 28))
            HashLo = M128_Lo
            IF (PRESENT(HashHi)) HashHi = XXH3_Avalanche(M128_Hi)
            RETURN
        END IF
        IF (Length /= 0) THEN
            ! XXH3_len_1to3_128b
            C1 = MaskI8(Input(0))
            ! high 3 bytes will be shifted
            C2 = ToInt32(Input(0 + SHIFTA(Length, 1)))
            C3 = MaskI8(Input(0 + Length - 1))
            CombinedL = ToInt32(IOR(ToIndex(IOR(IOR(SHIFTL(C1, 16), SHIFTL(C2, 24)), C3)), SHIFTL(Length, 8)))
            CombinedH = RotateLeft(IntegerReverseBytes(CombinedL), 13)
            BitFlipL = UnsignedInt(IEOR(IntSecret(0), IntSecret(1))) + Seed
            BitFlipH = UnsignedInt(IEOR(IntSecret(2), IntSecret(3))) - Seed
            HashLo = IEOR(UnsignedInt(CombinedL), BitFlipL)
            HashLo = XXH64_Avalanche(HashLo)
            IF (PRESENT(HashHi)) THEN
                HashHi = IEOR(UnsignedInt(CombinedH), BitFlipH)
                HashHi = XXH64_Avalanche(HashHi)
            END IF
            RETURN
        END IF
        HashLo = XXH64_Avalanche(IEOR(IEOR(Seed, LongSecret(8)), LongSecret(9)))
        IF (PRESENT(HashHi)) THEN
            HashHi = XXH64_Avalanche(IEOR(IEOR(Seed, LongSecret(10)), LongSecret(11)))
        END IF

        RETURN

    END SUBROUTINE XX3Hash128_Len1To16

    !**************************************************************************

    SUBROUTINE XX3Hash128_Len17To128(Input, Seed, Length, LongSecret, HashLo, HashHi)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code using the XX3Hash128 hash algorithm by Yann Collet.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt8,            INTENT(IN)     :: Input(0:)        ! input bytes
        tUInt64,           INTENT(IN)     :: Seed             ! seed
        tIndex,            INTENT(IN)     :: Length           ! length of input bytes
        tUInt64,           INTENT(IN)     :: LongSecret(0:23) ! base secret in longs
        tUInt64,           INTENT(OUT)    :: HashLo           ! lower 64-bit hash code
        tUInt64, OPTIONAL, INTENT(OUT)    :: HashHi           ! upper 64-bit hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64         :: Acc0, Acc1, Input0, Input1, Input2, Input3
        ! variables used to quickly access input and secret bytes
        tUInt8          :: InpBytes(0:127)
        tUInt64         :: InpLongs(1:16)
        EQUIVALENCE(InpBytes, InpLongs)

    !** FLOW

        ! perform hashing
        ! XXH3_len_17to128_128b
        Acc0 = ToInt64(Length)*XXH_PRIME64_1
        Acc1 = 0
        IF (Length > 32) THEN
            IF (Length > 64) THEN
                IF (Length > 96) THEN
                    InpBytes(0:63)   = Input(0:63)
                    InpBytes(64:127) = Input(Length-64:Length-1)
                    Input0 = InpLongs(7)
                    Input1 = InpLongs(8)
                    Input2 = InpLongs(9)
                    Input3 = InpLongs(10)
                    Acc0 = XXH128_Mix32B_Once(Seed, LongSecret, 12, Acc0, \
                                                Input0, Input1, Input2, Input3)
                    Acc1 = XXH128_Mix32B_Once(Seed, LongSecret, 14, Acc1, \
                                                Input2, Input3, Input0, Input1)
                ELSE
                    InpBytes(0:47)   = Input(0:47)
                    InpBytes(80:127) = Input(Length-48:Length-1)
                END IF
                Input0 = InpLongs(5)
                Input1 = InpLongs(6)
                Input2 = InpLongs(11)
                Input3 = InpLongs(12)
                Acc0 = XXH128_Mix32B_Once(Seed, LongSecret, 8, Acc0, \
                                            Input0, Input1, Input2, Input3)
                Acc1 = XXH128_Mix32B_Once(Seed, LongSecret, 10, Acc1,  \
                                            Input2, Input3, Input0, Input1)
            ELSE
                InpBytes(0:31)   = Input(0:31)
                InpBytes(96:127) = Input(Length-32:Length-1)
            END IF
            Input0 = InpLongs(3)
            Input1 = InpLongs(4)
            Input2 = InpLongs(13)
            Input3 = InpLongs(14)
            Acc0 = XXH128_Mix32B_Once(Seed, LongSecret, 4, Acc0, Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, LongSecret, 6, Acc1, Input2, Input3, Input0, Input1)
        ELSE
            InpBytes(0:15)    = Input(0:15)
            InpBytes(112:127) = Input(Length-16:Length-1)
        END IF
        Input0 = InpLongs(1)
        Input1 = InpLongs(2)
        Input2 = InpLongs(15)
        Input3 = InpLongs(16)
        Acc0 = XXH128_Mix32B_Once(Seed, LongSecret, 0,  Acc0, Input0, Input1, Input2, Input3)
        Acc1 = XXH128_Mix32B_Once(Seed, LongSecret, 2, Acc1, Input2, Input3, Input0, Input1)
        HashLo = XXH3_Avalanche(Acc0 + Acc1)
        IF (PRESENT(HashHi)) THEN
            HashHi = -XXH3_Avalanche(Acc0*XXH_PRIME64_1 + Acc1*XXH_PRIME64_4 &
                     + (ToInt64(Length) - Seed)*XXH_PRIME64_2)
        END IF

        RETURN

    END SUBROUTINE XX3Hash128_Len17To128

    !**************************************************************************

    SUBROUTINE XX3Hash128_Len129To240(Input, Seed, Length, LongSecret1, LongSecret2, &
                                      LongSecret3, HashLo, HashHi)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code using the XX3Hash128 hash algorithm by Yann Collet.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt8,            INTENT(IN)     :: Input(0:)            ! input bytes
        tUInt64,           INTENT(IN)     :: Seed                 ! seed
        tIndex,            INTENT(IN)     :: Length               ! length of input bytes
        tUInt64,           INTENT(IN)     :: LongSecret1(0:23)    ! base secret
        tUInt64,           INTENT(IN)     :: LongSecret2(0:13)    ! base secret
        tUInt64,           INTENT(IN)     :: LongSecret3(0:3)     ! base secret
        tUInt64,           INTENT(OUT)    :: HashLo               ! lower 64-bit hash code
        tUInt64, OPTIONAL, INTENT(OUT)    :: HashHi               ! upper 64-bit hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: Offset
        tUInt64         :: Acc0, Acc1, Input0, Input1, Input2, Input3
        tIndex          :: I
        tUInt32         :: NbRounds
        ! variables used to quickly access input and secret bytes
        tUInt8          :: InpBytes(0:127)
        tUInt64         :: InpLongs(1:16)
        EQUIVALENCE(InpBytes, InpLongs)

    !** FLOW

        ! initialize
        Offset = 0

        ! XXH3_len_129to240_128b
        NbRounds = Length / 32
        Acc0 = ToInt64(Length)*XXH_PRIME64_1
        Acc1 = 0_kInt64
        I = 0
        InpBytes(0:127) = Input(Offset:Offset+127)
        DO WHILE (I < 4)
            Input0 = InpLongs(4*I+1)
            Input1 = InpLongs(4*I+2)
            Input2 = InpLongs(4*I+3)
            Input3 = InpLongs(4*I+4)
            Acc0 = XXH128_Mix32B_Once(Seed, LongSecret1, 4*I, Acc0, \
                                        Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, LongSecret1, 4*I+2, Acc1, \
                                        Input2, Input3, Input0, Input1)
            I = I + 1
        END DO
        Acc0 = XXH3_Avalanche(Acc0)
        Acc1 = XXH3_Avalanche(Acc1)
        Offset = Offset + 128
        InpBytes(0:(NbRounds*4-16)*8-1) = Input(Offset:Offset+(NbRounds*4-16)*8-1)
        DO WHILE (I < NbRounds)
            Input0 = InpLongs(4*(I-4)+1)
            Input1 = InpLongs(4*(I-4)+2)
            Input2 = InpLongs(4*(I-4)+3)
            Input3 = InpLongs(4*(I-4)+4)
            Acc0 = XXH128_Mix32B_Once(Seed, LongSecret2, 4*(I-4), Acc0, \
                                        Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, LongSecret2, 4*(I-4)+2, Acc1, \
                                        Input2, Input3, Input0, Input1)
            I = I + 1
        END DO

        ! last bytes
        Offset = Offset - 128
        InpBytes(0:31) = Input(Offset+Length-32:Length-1)
        Input0 = InpLongs(3)
        Input1 = InpLongs(4)
        Input2 = InpLongs(1)
        Input3 = InpLongs(2)
        Acc0 = XXH128_Mix32B_Once((-Seed), LongSecret3, 0, Acc0, Input0, Input1, Input2, Input3)
        Acc1 = XXH128_Mix32B_Once((-Seed), LongSecret3, 2, Acc1, Input2, Input3, Input0, Input1)

        HashLo = XXH3_Avalanche(Acc0 + Acc1)
        IF (PRESENT(HashHi)) THEN
            HashHi = -XXH3_Avalanche(Acc0*XXH_PRIME64_1 + Acc1*XXH_PRIME64_4 &
                     + (ToInt64(Length) - Seed)*XXH_PRIME64_2)
        END IF

        RETURN

    END SUBROUTINE XX3Hash128_Len129To240

    !**************************************************************************

END SUBROUTINE XX3_Finalize_Hash128

!******************************************************************************

FUNCTION XX3_HashDirect128(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher128), INTENT(INOUT)  :: HS           !! a hasher (HS) object
    TYPE(*), CONTIGUOUS, INTENT(IN)     :: Input(..)    !! input data (any type and rank)
    tIndex,              INTENT(IN)     :: InpSize      !! size of the input (in bytes)
    tUInt64,  OPTIONAL,  INTENT(IN)     :: Seed         !! seed
    tLogical, OPTIONAL,  INTENT(IN)     :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tSInt128                            :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Seed0

!** FLOW

    SET_OPTION(Seed0, 0_kInt64, Seed)

    ! The following code illustrates simple use of the hasher.
    CALL HS%Initialize(Seed0, RemoveSign)
    CALL HS%Update(Input, InpSize)
    HashCode = HS%Finalize128()

    RETURN

END FUNCTION XX3_HashDirect128

!******************************************************************************

FUNCTION XX3_HashDirect128_wSecret(HS, Input, InpSize, Seed, Secret, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) with seed and secret.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XX3Hasher128),    INTENT(INOUT)   :: HS           !! a hasher (HS) object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tUInt64,                INTENT(IN)      :: Seed         !! seed
    tUInt8,                 INTENT(IN)      :: Secret(:)
    !^ a byte (8-bit integer) array (of at least 192 bytes) representing
    !  a custom secret <br>
    tLogical, OPTIONAL,     INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tSInt128                                :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! The following code illustrates simple use of the hasher.
    CALL HS%InitializeWSecret(Seed, Secret, RemoveSign)
    CALL HS%Update(Input, InpSize)
    HashCode = HS%Finalize128()

    RETURN

END FUNCTION XX3_HashDirect128_wSecret

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                   SUPPORTING ROUTINES FOR XXHASH ALGORITHMS                 +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION XXH64_Avalanche(H64) RESULT(Res)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: H64
    tUInt64             :: Res

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Res = IEOR(H64, SHIFTR(H64, 33))*XXH_PRIME64_2
    Res = IEOR(Res, SHIFTR(Res, 29))*XXH_PRIME64_3
    Res = IEOR(Res, SHIFTR(Res, 32))

    RETURN

END FUNCTION XXH64_Avalanche

!******************************************************************************

FUNCTION XXH3_Avalanche(H64) RESULT(Res)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: H64
    tUInt64             :: Res

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Res = IEOR(H64, SHIFTR(H64, 37))*ToInt64(Z'165667919E3779F9')
    Res = IEOR(Res, SHIFTR(Res, 32))

    RETURN

END FUNCTION XXH3_Avalanche

!******************************************************************************

FUNCTION XXH3_RRMxMx(H64, Length) RESULT(Res)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: H64
    tIndex,  INTENT(IN) :: Length
    tUInt64             :: Res

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Res = IEOR(H64, IEOR(RotateLeft(H64, 49), RotateLeft(H64, 24)))*ToInt64(Z'9FB21C651E98DF25')
    Res = IEOR(Res, SHIFTR(Res, 35) + ToInt64(Length))*ToInt64(Z'9FB21C651E98DF25')
    Res = IEOR(Res, SHIFTR(Res, 28))

    RETURN

END FUNCTION XXH3_RRMxMx

!******************************************************************************

FUNCTION UnsignedLongMultiplyorFold(LHS, RHS) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply or fold.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: LHS
    tUInt64, INTENT(IN) :: RHS
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

SUBROUTINE XX3Hash_Len241Up_Base(Acc, Input, Remaining, Length, Secret1, Secret2, &
                                 Secret3, Secret4, HashLo, HashHi)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,           INTENT(INOUT)  :: Acc(0:7)         ! state
    tUInt8,            INTENT(IN)     :: Input(0:)        ! input bytes
    tIndex,            INTENT(IN)     :: Remaining        ! remaining length of input bytes
    tIndex,            INTENT(IN)     :: Length           ! total length of input bytes
    tUInt64,           INTENT(IN)     :: Secret1(0:23)    ! base secret
    tUInt64,           INTENT(IN)     :: Secret2(0:7)     ! base secret
    tUInt64,           INTENT(IN)     :: Secret3(0:7)     ! base secret
    tUInt64,           INTENT(IN)     :: Secret4(0:7)     ! base secret
    tUInt64,           INTENT(OUT)    :: HashLo           ! lower 64-bit hash code
    tUInt64, OPTIONAL, INTENT(OUT)    :: HashHi           ! upper 64-bit hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: OffStripe, OffSec, NbStripes
    tIndex          :: J
    tUInt64         :: DataVal0, DataVal1, DataKey0, DataKey1
    tUInt8          :: InpBytes(0:63)
    tUInt64         :: InpLongs(1:8)
    EQUIVALENCE(InpBytes, InpLongs)

!** FLOW

    ! last partial block
    NbStripes = (Remaining - 1) / 64_kIndex
    OffStripe = 0_kIndex
    DO OffSec = 0, NbStripes - 1
        ! XXH3_accumulate_512
        InpBytes(0:63) = Input(OffStripe:OffStripe+63)
        DO J = 0, 7, 2
            DataVal0 = InpLongs(J+1)
            DataVal1 = InpLongs(J+2)
            DataKey0 = IEOR(DataVal0, Secret1(Offsec+J))
            DataKey1 = IEOR(DataVal1, Secret1(Offsec+J+1))
            ! swap adjacent lanes
            Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
            Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
        END DO
        OffStripe = OffStripe + 64_kIndex
    END DO

    ! last stripe
    ! XXH3_accumulate_512
    OffStripe = Remaining - 64
    InpBytes(0:63) = Input(OffStripe:OffStripe+63)
    DO J = 0, 7, 2
        DataVal0 = InpLongs(J+1)
        DataVal1 = InpLongs(J+2)
        DataKey0 = IEOR(DataVal0, Secret2(J))
        DataKey1 = IEOR(DataVal1, Secret2(J+1))
        ! swap adjacent lanes
        Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
        Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
    END DO

    ! XXH3_mergeAccs
    HashLo = ToInt64(Length)*XXH_PRIME64_1                 &
                + XXH3_Mix2Accs(Acc(0), Acc(1), Secret3, 0) &
                + XXH3_Mix2Accs(Acc(2), Acc(3), Secret3, 2) &
                + XXH3_Mix2Accs(Acc(4), Acc(5), Secret3, 4) &
                + XXH3_Mix2Accs(Acc(6), Acc(7), Secret3, 6)
    HashLo = XXH3_Avalanche(HashLo)
    IF (PRESENT(HashHi)) THEN
        HashHi = NOT(ToInt64(Length)*XXH_PRIME64_2)              &
                    + XXH3_Mix2Accs(Acc(0), Acc(1), Secret4, 0) &
                    + XXH3_Mix2Accs(Acc(2), Acc(3), Secret4, 2) &
                    + XXH3_Mix2Accs(Acc(4), Acc(5), Secret4, 4) &
                    + XXH3_Mix2Accs(Acc(6), Acc(7), Secret4, 6)
        HashHi = XXH3_Avalanche(HashHi)
    END IF

    RETURN

END SUBROUTINE XX3Hash_Len241Up_Base

!******************************************************************************

SUBROUTINE XX3Hash_Len241Up_Custom(Acc, Input, Remaining, Length, Secret, HashLo, HashHi)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,           INTENT(INOUT)  :: Acc(0:7)         ! state
    tUInt8,            INTENT(IN)     :: Input(0:)        ! input bytes
    tIndex,            INTENT(IN)     :: Remaining        ! remaining length of input bytes
    tIndex,            INTENT(IN)     :: Length           ! total length of input bytes
    tUInt8,            INTENT(IN)     :: Secret(0:191)    ! custom secret
    tUInt64,           INTENT(OUT)    :: HashLo           ! lower 64-bit hash code
    tUInt64, OPTIONAL, INTENT(OUT)    :: HashHi           ! upper 64-bit hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: OffStripe, OffSec, NbStripes
    tIndex          :: S, J
    tUInt64         :: DataVal0, DataVal1, DataKey0, DataKey1
    tUInt8          :: InpBytes(0:63)
    tUInt64         :: InpLongs(1:8)
    EQUIVALENCE(InpBytes, InpLongs)
    tUInt8          :: SecBytes(0:127)
    tUInt64         :: SecLongs(1:16)
    EQUIVALENCE(SecBytes, SecLongs)

!** FLOW

    ! last partial block
    NbStripes = (Remaining - 1) / 64_kIndex
    OffStripe = 0_kIndex
    OffSec    = 0_kIndex
    DO S = 0, NbStripes - 1
        ! XXH3_accumulate_512
        InpBytes(0:63) = Input(OffStripe:OffStripe+63)
        SecBytes(0:63) = Secret(OffSec:OffSec+63)
        DO J = 0, 7, 2
            DataVal0 = InpLongs(J+1)
            DataVal1 = InpLongs(J+2)
            DataKey0 = IEOR(DataVal0, SecLongs(J+1))
            DataKey1 = IEOR(DataVal1, SecLongs(J+2))
            ! swap adjacent lanes
            Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
            Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
        END DO
        OffStripe = OffStripe + 64_kIndex
        OffSec    = OffSec + 8_kIndex
    END DO

    ! last stripe
    ! XXH3_accumulate_512
    OffStripe = Remaining - 64
    OffSec    = 121    ! 192 - 64 - 7
    InpBytes(0:63) = Input(OffStripe:OffStripe+63)
    SecBytes(0:63) = Secret(121:184)
    DO J = 0, 7, 2
        DataVal0 = InpLongs(J+1)
        DataVal1 = InpLongs(J+2)
        DataKey0 = IEOR(DataVal0, SecLongs(J+1))
        DataKey1 = IEOR(DataVal1, SecLongs(J+2))
        ! swap adjacent lanes
        Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
        Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
    END DO

    ! XXH3_mergeAccs
    SecBytes(0:63) = Secret(11:74)
    HashLo = ToInt64(Length)*XXH_PRIME64_1                  &
                + XXH3_Mix2Accs(Acc(0), Acc(1), SecLongs, 1) &
                + XXH3_Mix2Accs(Acc(2), Acc(3), SecLongs, 3) &
                + XXH3_Mix2Accs(Acc(4), Acc(5), SecLongs, 5) &
                + XXH3_Mix2Accs(Acc(6), Acc(7), SecLongs, 7)

    HashLo = XXH3_Avalanche(HashLo)
    IF (PRESENT(HashHi)) THEN
        SecBytes(0:63) = Secret(117:180)
        HashHi = NOT(ToInt64(Length)*XXH_PRIME64_2)               &
                    + XXH3_Mix2Accs(Acc(0), Acc(1), SecLongs, 1) &
                    + XXH3_Mix2Accs(Acc(2), Acc(3), SecLongs, 3) &
                    + XXH3_Mix2Accs(Acc(4), Acc(5), SecLongs, 5) &
                    + XXH3_Mix2Accs(Acc(6), Acc(7), SecLongs, 7)
        HashHi = XXH3_Avalanche(HashHi)
    END IF

    RETURN

END SUBROUTINE XX3Hash_Len241Up_Custom

!******************************************************************************

END MODULE MClass_XX3Hasher64

!******************************************************************************
