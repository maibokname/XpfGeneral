
MODULE MClass_SHA2B

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SHA2B* type and its related routines.
!   The *SHA2B* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdhelper.html#type-mdhelper">MDHelper</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *SHA2B* type implements an incremental cryptographic hash
!   function by employing the *SHA-384 or SHA-512 message-digest*
!   algorithm where both algorithms are described in FIPS 180-4 [1].
!   The implementation here is mainly based on the references [2, 3].  <br>
!   By default, the *SHA2B* type employs the *SHA-512 message-digest*
!   algorithm.  However, a user can specify the *IsSHA384* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *SHA-384 message-digest* algorithm
!   instead of the default one. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://doi.org/10.6028%2FNIST.FIPS.180-4">FIPS PUB 180-4:
!       Secure Hash Standard (SHS). </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>
!   [3] <a href="https://github.com/bcgit/bc-java">The Bouncy Castle Crypto
!       Package For Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_BytePack,           ONLY: BytePackBE, ByteUnpackBE
    USE MClass_BaseDigest
    USE MClass_MDHelper

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: SHA2B

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tIndex, PARAMETER   :: BlockLen = 128_kIndex
    tIndex, PARAMETER   :: DLen384  = 48_kIndex
    tIndex, PARAMETER   :: DLen512  = 64_kIndex
    tLong,  PARAMETER   :: IV384(0:7) = [                             &
            ToInt64(Z'CBBB9D5DC1059ED8'), ToInt64(Z'629A292A367CD507'), &
            ToInt64(Z'9159015A3070DD17'), ToInt64(Z'152FECD8F70E5939'), &
            ToInt64(Z'67332667FFC00B31'), ToInt64(Z'8EB44A8768581511'), &
            ToInt64(Z'DB0C2E0D64F98FA7'), ToInt64(Z'47B5481DBEFA4FA4')]
    tLong,  PARAMETER   :: IV512(0:7) = [                             &
            ToInt64(Z'6A09E667F3BCC908'), ToInt64(Z'BB67AE8584CAA73B'), &
            ToInt64(Z'3C6EF372FE94F82B'), ToInt64(Z'A54FF53A5F1D36F1'), &
            ToInt64(Z'510E527FADE682D1'), ToInt64(Z'9B05688C2B3E6C1F'), &
            ToInt64(Z'1F83D9ABFB41BD6B'), ToInt64(Z'5BE0CD19137E2179')]
    tLong,  PARAMETER   :: K(0:79) = [                                                         &
        ToInt64(Z'428A2F98D728AE22'), ToInt64(Z'7137449123EF65CD'), ToInt64(Z'B5C0FBCFEC4D3B2F'), &
        ToInt64(Z'E9B5DBA58189DBBC'), ToInt64(Z'3956C25BF348B538'), ToInt64(Z'59F111F1B605D019'), &
        ToInt64(Z'923F82A4AF194F9B'), ToInt64(Z'AB1C5ED5DA6D8118'), ToInt64(Z'D807AA98A3030242'), &
        ToInt64(Z'12835B0145706FBE'), ToInt64(Z'243185BE4EE4B28C'), ToInt64(Z'550C7DC3D5FFB4E2'), &
        ToInt64(Z'72BE5D74F27B896F'), ToInt64(Z'80DEB1FE3B1696B1'), ToInt64(Z'9BDC06A725C71235'), &
        ToInt64(Z'C19BF174CF692694'), ToInt64(Z'E49B69C19EF14AD2'), ToInt64(Z'EFBE4786384F25E3'), &
        ToInt64(Z'0FC19DC68B8CD5B5'), ToInt64(Z'240CA1CC77AC9C65'), ToInt64(Z'2DE92C6F592B0275'), &
        ToInt64(Z'4A7484AA6EA6E483'), ToInt64(Z'5CB0A9DCBD41FBD4'), ToInt64(Z'76F988DA831153B5'), &
        ToInt64(Z'983E5152EE66DFAB'), ToInt64(Z'A831C66D2DB43210'), ToInt64(Z'B00327C898FB213F'), &
        ToInt64(Z'BF597FC7BEEF0EE4'), ToInt64(Z'C6E00BF33DA88FC2'), ToInt64(Z'D5A79147930AA725'), &
        ToInt64(Z'06CA6351E003826F'), ToInt64(Z'142929670A0E6E70'), ToInt64(Z'27B70A8546D22FFC'), &
        ToInt64(Z'2E1B21385C26C926'), ToInt64(Z'4D2C6DFC5AC42AED'), ToInt64(Z'53380D139D95B3DF'), &
        ToInt64(Z'650A73548BAF63DE'), ToInt64(Z'766A0ABB3C77B2A8'), ToInt64(Z'81C2C92E47EDAEE6'), &
        ToInt64(Z'92722C851482353B'), ToInt64(Z'A2BFE8A14CF10364'), ToInt64(Z'A81A664BBC423001'), &
        ToInt64(Z'C24B8B70D0F89791'), ToInt64(Z'C76C51A30654BE30'), ToInt64(Z'D192E819D6EF5218'), &
        ToInt64(Z'D69906245565A910'), ToInt64(Z'F40E35855771202A'), ToInt64(Z'106AA07032BBD1B8'), &
        ToInt64(Z'19A4C116B8D2D0C8'), ToInt64(Z'1E376C085141AB53'), ToInt64(Z'2748774CDF8EEB99'), &
        ToInt64(Z'34B0BCB5E19B48A8'), ToInt64(Z'391C0CB3C5C95A63'), ToInt64(Z'4ED8AA4AE3418ACB'), &
        ToInt64(Z'5B9CCA4F7763E373'), ToInt64(Z'682E6FF3D6B2B8A3'), ToInt64(Z'748F82EE5DEFB2FC'), &
        ToInt64(Z'78A5636F43172F60'), ToInt64(Z'84C87814A1F0AB72'), ToInt64(Z'8CC702081A6439EC'), &
        ToInt64(Z'90BEFFFA23631E28'), ToInt64(Z'A4506CEBDE82BDE9'), ToInt64(Z'BEF9A3F7B2C67915'), &
        ToInt64(Z'C67178F2E372532B'), ToInt64(Z'CA273ECEEA26619C'), ToInt64(Z'D186B8C721C0C207'), &
        ToInt64(Z'EADA7DD6CDE0EB1E'), ToInt64(Z'F57D4F7FEE6ED178'), ToInt64(Z'06F067AA72176FBA'), &
        ToInt64(Z'0A637DC5A2C898A6'), ToInt64(Z'113F9804BEF90DAE'), ToInt64(Z'1B710B35131C471B'), &
        ToInt64(Z'28DB77F523047D84'), ToInt64(Z'32CAAB7B40C72493'), ToInt64(Z'3C9EBE0A15C9BEBC'), &
        ToInt64(Z'431D67C49C100D4C'), ToInt64(Z'4CC5D4BECB3E42B6'), ToInt64(Z'597F299CFC657E2A'), &
        ToInt64(Z'5FCB6FAB3AD6FAEC'), ToInt64(Z'6C44198C4A475817')]

!** DERIVED TYPE DEFINITIONS
    !> *SHA2B* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing either the
    !  *SHA-384* or the *SHA-512 message-digest* algorithm.
    TYPE, EXTENDS(MDHelper) :: SHA2B
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state
        tLong       :: State(0:7) = IV512(0:7)
        !% flag indicating whether the SHA-384 algorithm is employed or not.
        tLogical    :: IsSHA384   = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => SHA2B_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (SHA-512).
        PROCEDURE       :: Initialize   => SHA2B_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => SHA2B_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => SHA2B_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => SHA2B_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => SHA2B_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => SHA2B_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => SHA2B_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => SHA2B_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => SHA2B_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => SHA2B_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (SHA-512) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the SHA-384 algorithm <br>
        !   --->    CALL MD%Create(IsSHA384=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
    END TYPE SHA2B

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE SHA2B_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2B), INTENT(INOUT) :: MD    !! 'SHA2B' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the SHA-512 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE SHA2B_Initialize

!******************************************************************************

SUBROUTINE SHA2B_Initialize_wFlag(MD, IsSHA384)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2B), INTENT(INOUT) :: MD           !! 'SHA2B' object
    tLogical,     INTENT(IN)    :: IsSHA384
    !^ flag indicating whether the SHA-384 algorithm is employed or not. <br>
    !  - If true, use the SHA-384 algorithm. <br>
    !  - Otherwise, use the SHA-512 algorithm. <br>

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: LittleEndian = FalseVal
        
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsSHA384 = IsSHA384
    CALL MD%HelperInit(LittleEndian, 16_kIndex)
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE SHA2B_Initialize_wFlag

!******************************************************************************

SUBROUTINE SHA2B_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2B), INTENT(INOUT) :: MD   !! 'SHA2B' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    IF (MD%IsSHA384) THEN
        MD%State = IV384
    ELSE
        MD%State = IV512
    END IF
    CALL MD%HelperReset()

    RETURN

END SUBROUTINE SHA2B_Reset

!******************************************************************************

SUBROUTINE SHA2B_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2B),                   INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(SHA2B :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (SHA2B)
        CALL Dst%Create(Src%IsSHA384)
        Dst%State    = Src%State
        Dst%BufArr   = Src%BufArr
        CALL Src%HelperClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE SHA2B_GetClone

!******************************************************************************

FUNCTION SHA2B_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2B), INTENT(IN)    :: MD       !! 'SHA2B' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSHA384) THEN
        Name = 'SHA-384'
    ELSE
        Name = 'SHA-512'
    END IF

    RETURN

END FUNCTION SHA2B_GetName

!******************************************************************************

FUNCTION SHA2B_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2B), INTENT(IN)    :: MD       !! 'SHA2B' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSHA384) THEN
        Length = DLen384
    ELSE
        Length = DLen512
    END IF
    RETURN

END FUNCTION SHA2B_GetDigestLen

!******************************************************************************

FUNCTION SHA2B_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2B), INTENT(IN)    :: MD       !! 'SHA2B' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION SHA2B_GetBlockLen

!******************************************************************************

SUBROUTINE SHA2B_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2B), TARGET, INTENT(INOUT) :: MD           !! 'SHA2B' object
    tByte,       POINTER, INTENT(INOUT) :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE SHA2B_SetBufPtr

!******************************************************************************

SUBROUTINE SHA2B_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2B), INTENT(INOUT) :: MD           !! 'SHA2B' object
    tByte,        INTENT(IN)    :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: A, B, C, D, E, F, G, H
    tLong       :: T1, T2
    tLong       :: W(0:79)
    tIndex      :: I, J

!** SUBROUTINE MACRO DEFINITIONS:
#define BSigma0(X)      IEOR(IEOR(RotateLeft(X, 36), RotateLeft(X, 30)), RotateLeft(X, 25))
#define BSigma1(X)      IEOR(IEOR(RotateLeft(X, 50), RotateLeft(X, 46)), RotateLeft(X, 23))
#define SSigma0(X)      IEOR(IEOR(RotateLeft(X, 63), RotateLeft(X, 56)), SHIFTR(X, 7))
#define SSigma1(X)      IEOR(IEOR(RotateLeft(X, 45), RotateLeft(X,  3)), SHIFTR(X, 6))
#define CH(X, Y, Z)     IEOR(IAND(IEOR(Y, Z), X), Z)
#define MAJ(X, Y, Z)    IOR(IAND(Y, Z), IAND(IOR(Y, Z), X))

! FLOW

    A = MD%State(0)
    B = MD%State(1)
    C = MD%State(2)
    D = MD%State(3)
    E = MD%State(4)
    F = MD%State(5)
    G = MD%State(6)
    H = MD%State(7)

    CALL BytePackBE(BytesIn, 0_kIndex, W(0:15))

    DO J = 16, 79
        W(J) = SSigma1(W(J-2)) + W(J-7) + SSigma0(W(J-15)) + W(J-16)
    END DO

    DO I = 0, 79
        T1 = H + BSigma1(E) + CH(E, F, G) + K(I) + W(I)
        T2 = BSigma0(A) + MAJ(A, B, C)
        H = G
        G = F
        F = E
        E = D + T1
        D = C
        C = B
        B = A
        A = T1 + T2
    END DO

    MD%State(0) = MD%State(0) + A
    MD%State(1) = MD%State(1) + B
    MD%State(2) = MD%State(2) + C
    MD%State(3) = MD%State(3) + D
    MD%State(4) = MD%State(4) + E
    MD%State(5) = MD%State(5) + F
    MD%State(6) = MD%State(6) + G
    MD%State(7) = MD%State(7) + H

    RETURN

#undef BSigma0
#undef BSigma1
#undef SSigma0
#undef SSigma1
#undef CH
#undef MAJ

END SUBROUTINE SHA2B_ProcessBlock

!******************************************************************************

SUBROUTINE SHA2B_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2B), INTENT(INOUT) :: MD           !! 'SHA2B' object
    tByte,        INTENT(INOUT) :: BytesOut(0:) !! the output buffer
    tIndex,       INTENT(IN)    :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW
        
    CALL MD%HelperPadding()
    DO I = 0_kIndex, (MD%GetDigestLen()/8_kIndex - 1_kIndex)
        CALL ByteUnpackBE(MD%State(I), BytesOut, Offset + 8_kIndex*I)
    END DO

    RETURN

END SUBROUTINE SHA2B_DoPadding

!******************************************************************************

SUBROUTINE SHA2B_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2B), INTENT(INOUT) :: MD           !! 'SHA2B' object
    tByte,        INTENT(IN)    :: LastByte     !! the last byte
    tByte,        INTENT(IN)    :: NBits        !! number of bits in the last byte
    tByte,        INTENT(INOUT) :: BytesOut(0:) !! the output buffer
    tIndex,       INTENT(IN)    :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! SHA-2 algorithms do not handle partial bytes
    CALL MD%DoPadding(BytesOut, Offset)
    ASSOCIATE(Dummy => LastByte, Dummy2 => NBits); END ASSOCIATE

    RETURN

END SUBROUTINE SHA2B_AddBitsNPad

!******************************************************************************

END MODULE MClass_SHA2B
    
!******************************************************************************
