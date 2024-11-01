
MODULE MClass_Skein

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Skein* type and its related routines.
!   The *Skein* type is a *digest* type that extends directly from the
!   <a href="../module/mclass_basedigest.html#type-basedigest">BaseDigest</a>
!   type.  It implements all deferred procedures required by a digest type. <br>
!   The *Skein* type implements an incremental cryptographic hash
!   function by employing the *Skein message-digest* algorithm [1].
!   The implementation here is mainly based on the references [2]. <br>
!   The *Skein* type represents four cryptographic hash functions: the
!   *Skein-224*, *Skein-256*, *Skein-384*, and *Skein-512* hash functions.
!   By default, the *Skein* type represents the *Skein-256* hash function.
!   However, a user can specify the *Security* argument (to one of the
!   four applicable values: 224, 256, 384 and 512) when initializing the
!   digest object in order to use a different hash function and get a
!   different hash output size. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.schneier.com/academic/skein/">
!       The Skein Hash Function Family. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MemHandlers,  ONLY: MemAlloc
    USE MBase_SIntUtil,     ONLY: ToDecStrSigned
    USE MBase_BytePack,     ONLY: BytePackLE, ByteUnpackLE
    USE MBase_ByteUtil,     ONLY: AnyType_2_ByteArrPtr, ByteArr_2_HexStr => ToHexStr_BE
    USE MClass_BaseDigest

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: Skein

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tByte,  PARAMETER   :: FByte80 = ToInt8(Z'80')
    tByte,  PARAMETER   :: FByteFF = ToInt8(Z'FF')
    tIndex, PARAMETER   :: BlockLen = 64_kIndex
    tIndex, PARAMETER   :: DLen224  = 28_kIndex
    tIndex, PARAMETER   :: DLen256  = 32_kIndex
    tIndex, PARAMETER   :: DLen384  = 48_kIndex
    tIndex, PARAMETER   :: DLen512  = 64_kIndex
    tLong,  PARAMETER   :: IV224(0:7) = [                             &
            ToInt64(Z'CCD0616248677224'), ToInt64(Z'CBA65CF3A92339EF'), &
            ToInt64(Z'8CCD69D652FF4B64'), ToInt64(Z'398AED7B3AB890B4'), &
            ToInt64(Z'0F59D1B1457D2BD0'), ToInt64(Z'6776FE6575D4EB3D'), &
            ToInt64(Z'99FBC70E997413E9'), ToInt64(Z'9E2CFCCFE1C41EF7')]
    tLong,  PARAMETER   :: IV256(0:7) = [                             &
            ToInt64(Z'CCD044A12FDB3E13'), ToInt64(Z'E83590301A79A9EB'), &
            ToInt64(Z'55AEA0614F816E6F'), ToInt64(Z'2A2767A4AE9B94DB'), &
            ToInt64(Z'EC06025E74DD7683'), ToInt64(Z'E7A436CDC4746251'), &
            ToInt64(Z'C36FBAF9393AD185'), ToInt64(Z'3EEDBA1833EDFC13')]
    tLong,  PARAMETER   :: IV384(0:7) = [                             &
            ToInt64(Z'A3F6C6BF3A75EF5F'), ToInt64(Z'B0FEF9CCFD84FAA4'), &
            ToInt64(Z'9D77DD663D770CFE'), ToInt64(Z'D798CBF3B468FDDA'), &
            ToInt64(Z'1BC4A6668A0E4465'), ToInt64(Z'7ED7D434E5807407'), &
            ToInt64(Z'548FC1ACD4EC44D6'), ToInt64(Z'266E17546AA18FF8')]
    tLong,  PARAMETER   :: IV512(0:7) = [                             &
            ToInt64(Z'4903ADFF749C51CE'), ToInt64(Z'0D95DE399746DF03'), &
            ToInt64(Z'8FD1934127C79BCE'), ToInt64(Z'9A255629FF352CB1'), &
            ToInt64(Z'5DB62599DF6CA7B0'), ToInt64(Z'EABE394CA9D5C3F4'), &
            ToInt64(Z'991112C71A75B523'), ToInt64(Z'AE18A40B660FCC33')]

!** DERIVED TYPE DEFINITIONS
    !> *Skein* is a concrete *digest* type that implements an incremental
    !  cryptographic hash function based on the Skein hash algorithms.
    TYPE, EXTENDS(BaseDigest) :: Skein
        PRIVATE
        !% buffer array used to store input data
        tByte           :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state variables (State will point to the first 8 elements of H)
        tLong           :: H(0:26) = 0_kInt64
        tLong, POINTER  :: State(:) => NULL()
        !% security strength in bits
        tInteger        :: Security = 256
        !% length of hash output in bytes
        tIndex          :: DigestLen = DLen256
        !% the number of blocks of input processed
        tLong           :: BlockCount = 0_kInt64
        !% the number of bytes of input currently stored in the buffer
        tIndex          :: BufLen     = 0_kIndex
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: Skein_ByteDigest_AddBits
        PROCEDURE, PRIVATE  :: Skein_HexDigest_AddBits
        PROCEDURE, PRIVATE  :: ProcessBlock         => Skein_ProcessBlock
        PROCEDURE, PRIVATE  :: InitializeWSecurity  => Skein_Initialize_wSecurity
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (Skein-256).
        PROCEDURE       :: Initialize           => Skein_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset                => Skein_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone             => Skein_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName              => Skein_GetName
        !> Use the *Update* method in place of the *InsertBytes* method to insert input
        !  data as an array of bytes (8-bit integers) where the offset (zero-based) into
        !  the array and the length of the input in bytes are specified.
        PROCEDURE       :: InsertBytes          => Skein_InsertBytes
        !> Use the *Update* method in place of the *InsertGen* method to insert
        !  input data in a generic way where the *Input* argument can be any type and
        !  any rank and the *InpSize* argument specifies the size of input data in bytes.
        PROCEDURE       :: InsertGen            => Skein_InsertGen
        !> Use the *Digest* method in place of the *ByteDigest* method to finalize the
        !  current hash computation and return the hash value as an array of bytes
        !  in a newly-allocated array.
        PROCEDURE       :: ByteDigest           => Skein_ByteDigest
        !> Use the *Digest* method in place of the *ByteDigest_wInput* method to insert final
        !  input in a generic way and then finalize the current hash computation and return
        !  the hash value as an array of bytes in a newly-allocated array.
        PROCEDURE       :: ByteDigest_wInput    => Skein_ByteDigest_wInput
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen         => Skein_GetDigestLen
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (Skein-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the Skein-512 algorithm <br>
        !   --->    CALL MD%Create(512) <br>
        GENERIC         :: Create               => InitializeWSecurity
        !> **Type-Bound Subroutine**: AddBitsNDigest <br>
        !  **Purpose**:  To add the last byte and then finalize the current hash computation
        !                and return the hash output. The object is reset. <br>
        !  **Usage**: <br>
        !   ! insert final input and return hash output as a byte array <br>
        !   --->    CALL MD%AddBitsNDigest(Input, InpSize, ByteArr) <br>
        !   ! insert final input and return hash output as a hexadecimal string <br>
        !   --->    CALL MD%AddBitsNDigest(Input, InpSize, HexStr) <br>
        !  **Note**: <br>
        !  This method is only used for an input message whose bit length is not
        !  a multiple of eight (i.e. a message with partial bytes). <br>
        !  The method is intended to be used by a digest type that implements a hash function
        !  that is an entrant of the SHA-3 competition.  It is mainly used for a test purpose.
        GENERIC     :: AddBitsNDigest           => Skein_ByteDigest_AddBits, &
                                                   Skein_HexDigest_AddBits
        ! ---------------------------------------------------------------------
        FINAL       :: Skein_Finalize
        ! ---------------------------------------------------------------------
    END TYPE Skein

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Skein_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Skein), INTENT(INOUT) :: MD    !! 'Skein' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL MD%Create(256)
   
    RETURN

END SUBROUTINE Skein_Initialize

!******************************************************************************

SUBROUTINE Skein_Initialize_wSecurity(MD, Security)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified Security.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Skein), TARGET, INTENT(INOUT) :: MD           !! 'Skein' object
    tInteger,             INTENT(IN)    :: Security
    !^ Strength of security in bits with four possible values: 224, 256, 384 and 512.
    !  If the specified value is NOT valid, it is set to the default (256) value.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (Security)
    CASE (224, 256, 384, 512)
        MD%Security = Security
    CASE DEFAULT
        MD%Security = 256
    END SELECT
    
    SELECT CASE (MD%Security)
    CASE (224)
        MD%DigestLen = DLen224
    CASE (256)
        MD%DigestLen = DLen256
    CASE (384)
        MD%DigestLen = DLen384
    CASE (512)
        MD%DigestLen = DLen512
    END SELECT
    
    MD%State(0:7) => MD%H(0:7)
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE Skein_Initialize_wSecurity

!******************************************************************************

SUBROUTINE Skein_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Skein), INTENT(INOUT) :: MD   !! 'Skein' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    MD%H = 0_kInt64
    SELECT CASE (MD%Security)
    CASE (224)
        MD%H(0:7) = IV224
    CASE (256)
        MD%H(0:7) = IV256
    CASE (384)
        MD%H(0:7) = IV384
    CASE (512)
        MD%H(0:7) = IV512
    END SELECT
    MD%BufLen     = 0_kIndex
    MD%BlockCount = 0_kInt64

    RETURN

END SUBROUTINE Skein_Reset

!******************************************************************************

SUBROUTINE Skein_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Skein),                   INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(Skein :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (Skein)
        CALL Dst%Create(Src%Security)
        Dst%H          = Src%H
        Dst%BufArr     = Src%BufArr
        Dst%BufLen     = Src%BufLen
        Dst%BlockCount = Src%BlockCount
    END SELECT
        
    RETURN

END SUBROUTINE Skein_GetClone

!******************************************************************************

FUNCTION Skein_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Skein), INTENT(IN)    :: MD       !! 'Skein' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Skein-' // ToDecStrSigned(MD%Security)

    RETURN

END FUNCTION Skein_GetName

!******************************************************************************

FUNCTION Skein_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Skein), INTENT(IN)    :: MD       !! 'Skein' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Length = MD%DigestLen

    RETURN

END FUNCTION Skein_GetDigestLen

!******************************************************************************

SUBROUTINE Skein_ProcessBlock(MD, EType, Extra)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Skein), INTENT(INOUT) :: MD           !! 'Skein' object
    tInteger,         INTENT(IN)    :: EType
    tInteger,         INTENT(IN)    :: Extra

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: M(0:7), P(0:7)
    tLong       :: T0, T1, T2, Temp
    tInteger    :: I, S

!** SUBROUTINE MACRO DEFINITIONS:
#define TFBIG_MIX(X0, X1, RC) \
    X0 = X0 + X1; \
    X1 = IEOR(RotateLeft(X1, RC), X0);
#define TFBIG_MIX8(W0, W1, W2, W3, W4, W5, W6, W7, RC0, RC1, RC2, RC3) \
    TFBIG_MIX(W0, W1, RC0); \
    TFBIG_MIX(W2, W3, RC1); \
    TFBIG_MIX(W4, W5, RC2); \
    TFBIG_MIX(W6, W7, RC3);
#define TFBIG_ADDKEY(R, H, SS, TT0, TT1) \
        R(0) = R(0) + H(SS); \
        R(1) = R(1) + H(SS+1); \
        R(2) = R(2) + H(SS+2); \
        R(3) = R(3) + H(SS+3); \
        R(4) = R(4) + H(SS+4); \
        R(5) = R(5) + H(SS+5) + TT0; \
        R(6) = R(6) + H(SS+6) + TT1; \
        R(7) = R(7) + H(SS+7) + SS;
#define TFBIG_4E(S) \
        TFBIG_ADDKEY(P, MD%H, S, T0, T1); \
        TFBIG_MIX8(P(0), P(1), P(2), P(3), P(4), P(5), P(6), P(7), 46, 36, 19, 37); \
        TFBIG_MIX8(P(2), P(1), P(4), P(7), P(6), P(5), P(0), P(3), 33, 27, 14, 42); \
        TFBIG_MIX8(P(4), P(1), P(6), P(3), P(0), P(5), P(2), P(7), 17, 49, 36, 39); \
        TFBIG_MIX8(P(6), P(1), P(0), P(7), P(2), P(5), P(4), P(3), 44,  9, 54, 56);
#define TFBIG_4O(S) \
        TFBIG_ADDKEY(P, MD%H, S, T1, T2); \
        TFBIG_MIX8(P(0), P(1), P(2), P(3), P(4), P(5), P(6), P(7), 39, 30, 34, 24); \
        TFBIG_MIX8(P(2), P(1), P(4), P(7), P(6), P(5), P(0), P(3), 13, 50, 10, 17); \
        TFBIG_MIX8(P(4), P(1), P(6), P(3), P(0), P(5), P(2), P(7), 25, 29, 39, 43); \
        TFBIG_MIX8(P(6), P(1), P(0), P(7), P(2), P(5), P(4), P(3),  8, 35, 56, 22);

! FLOW

    ! get input
    CALL BytePackLE(MD%BufArr, 0_kIndex, M)
    
    ! initialize
    P(0:7) = M(0:7)
    MD%H(8) = IEOR(IEOR(IEOR(IEOR(MD%State(0), MD%State(1)), IEOR(MD%State(2), MD%State(3))),  &
                        IEOR(IEOR(MD%State(4), MD%State(5)), IEOR(MD%State(6), MD%State(7)))), &
                        ToInt64(Z'1BD11BDAA9FC1A22'))
    T0 = SHIFTL(MD%BlockCount, 6) + ToInt64(Extra)
    T1 = SHIFTR(MD%BlockCount, 58) + SHIFTL(ToInt64(EType), 55)
    T2 = IEOR(T0, T1)
    DO I = 0, 15, 3
        MD%H(I+9)  = MD%H(I)
        MD%H(I+10) = MD%H(I+1)
        MD%H(I+11) = MD%H(I+2)
    END DO
    
    ! transform
    DO I = 0, 8
        S = SHIFTL(I, 1)
        TFBIG_4E(S)
        TFBIG_4O(S+1)
        Temp = T2
        T2 = T1
        T1 = T0
        T0 = Temp
    END DO
    TFBIG_ADDKEY(P, MD%H, 18, T0, T1)
    
    ! get output
    DO I = 0, 7
        MD%State(I) = IEOR(M(I), P(I))
    END DO

    RETURN

#undef TFBIG_MIX
#undef TFBIG_MIX8
#undef TFBIG_ADDKEY
#undef TFBIG_4E
#undef TFBIG_4O

END SUBROUTINE Skein_ProcessBlock

!******************************************************************************

SUBROUTINE Skein_InsertBytes(MD, ByteArr, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data as a byte (8-bit integer) array where offset (zero-based)
    !  and length are specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Skein), INTENT(INOUT) :: MD             !! 'Skein' object
    tByte,        INTENT(IN)    :: ByteArr(0:)    !! a byte array of input data
    tIndex,       INTENT(IN)    :: Offset         !! the offset in input data
    tIndex,       INTENT(IN)    :: Length         !! the length of input data in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CLen, CurPos, CurLen
    tInteger    :: EType

! FLOW

    IF (Length == 0_kIndex) RETURN
    
    CLen = BlockLen - MD%BufLen
    IF (Length <= CLen) THEN
        MD%BufArr(MD%BufLen:MD%BufLen+Length-1) = ByteArr(Offset:Offset+Length-1)
        MD%BufLen = MD%BufLen + Length
        RETURN
    END IF
    CurPos = Offset
    CurLen = Length
    IF (CLen /= 0_kIndex) THEN
        MD%BufArr(MD%BufLen:MD%BufLen+CLen-1) = ByteArr(CurPos:CurPos+CLen-1)
        CurPos = CurPos + CLen
        CurLen = CurLen - CLen
    END IF

    DO
        IF (MD%BlockCount == 0_kInt64) THEN
            EType = 224
        ELSE
            EType = 96
        END IF
        MD%BlockCount = MD%BlockCount + 1_kInt64
        CALL MD%ProcessBlock(EType, 0)
        IF (CurLen <= BlockLen) EXIT
        MD%BufArr(0:BlockLen-1) = ByteArr(CurPos:CurPos+BlockLen-1)
        CurPos = CurPos + BlockLen
        CurLen = CurLen - BlockLen
    END DO
    MD%BufArr(0:CurLen-1) = ByteArr(CurPos:CurPos+CurLen-1)
    MD%BufLen = CurLen

    RETURN

END SUBROUTINE Skein_InsertBytes

!******************************************************************************

SUBROUTINE Skein_InsertGen(MD, Input, InpSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data in a generic way where the *Input* argument can be
    !  any type and any rank and the *InpSize* argument specifies the size of
    !  the input data in a number of bytes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Skein),           INTENT(INOUT)   :: MD       !! 'Skein' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)!! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize  !! size of the input (in bytes)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: InpPtr(:)

! FLOW
    
    ! set pointer to the input
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)
    
    ! insert the input data as a byte array
    CALL MD%Update(InpPtr, 0_kIndex, InpSize)

    NULLIFY(InpPtr)

    RETURN

END SUBROUTINE Skein_InsertGen

!******************************************************************************

SUBROUTINE Skein_ByteDigest(MD, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes in a newly-allocated array.
    !  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Skein),       INTENT(INOUT)   :: MD           !! 'Skein' object
    tByte, ALLOCATABLE, INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%AddBitsNDigest(0_kInt8, 0_kInt8, ByteArr)

    RETURN

END SUBROUTINE Skein_ByteDigest

!******************************************************************************

SUBROUTINE Skein_ByteDigest_wInput(MD, Input, InpSize, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash
    !  computation and return the hash value as an array of (8-bit integer) bytes
    !  in a newly-allocated array.  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Skein),           INTENT(INOUT)   :: MD           !! 'Skein' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tByte,   ALLOCATABLE,   INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%Update(Input, InpSize)
    CALL MD%Digest(ByteArr)
        
    RETURN

END SUBROUTINE Skein_ByteDigest_wInput

!******************************************************************************

SUBROUTINE Skein_ByteDigest_AddBits(MD, LastByte, NBits, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then finalize the current hash computation
    !  and return the hash output as a byte array. <br>
    !  This routine is only used for an input message whose bit length is not
    !  a multiple of eight (i.e. a message with partial bytes). <br>
    !  This procedure is intended to be used by a digest type that implements
    !  a hash function that is an entrant of the SHA-3 competition.  It is mainly
    !  used for a test purpose.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Skein),       INTENT(INOUT)   :: MD           !! 'Skein' object
    tByte,              INTENT(IN)      :: LastByte     !! the last byte
    tByte,              INTENT(IN)      :: NBits        !! number of bits in the last byte
    tByte, ALLOCATABLE, INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I
    tInteger    :: EType, Add
    tByte       :: TmpOut(0:BlockLen-1)
    tByte       :: X, Z

! FLOW

    ! Add bit padding if necessary
    IF (NBits /= 0_kInt8) THEN
        Z = SHIFTR(FByte80, NBits)
        X = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
        CALL MD%Update(X, 1_kIndex)
        Add = 1
    ELSE
        Add = 0
    END IF

    ! At that point, if BufLen == 0, then the message was empty;
    ! otherwise, there is between 1 and 64 bytes (inclusive) which
    ! are yet to be processed. Either way, we complete the buffer
    ! to a full block with zeros (the Skein specification mandates
    ! that an empty message is padded so that there is at least
    ! one block to process).
    !
    ! Once this block has been processed, we do it again, with
    ! a block full of zeros, for the output (that block contains
    ! the encoding of "0", over 8 bytes, then padded with zeros).

    ! pad zero to the end of the buffer
    MD%BufArr(MD%BufLen:BlockLen-1) = 0_kInt8

    ! process the buffer
    IF (MD%BlockCount == 0_kInt64) THEN
        EType = 480 + Add
    ELSE
        EType = 352 + Add
    END IF
    CALL MD%ProcessBlock(EType, ToInt32(MD%BufLen))
    
    ! reset Buffer and BCount
    MD%BufArr(0:BlockLen-1) = 0_kInt8
    MD%BlockCount = 0_kInt64
    
    ! process the buffer
    CALL MD%ProcessBlock(510, 8)
    
    ! get output
    DO I = 0, 7
        CALL ByteUnpackLE(MD%State(I), TmpOut, SHIFTL(I, 3))
    END DO
    CALL MemAlloc(ByteArr, MD%DigestLen, StartID=0_kIndex)
    ByteArr(0:MD%DigestLen-1) = TmpOut(0:MD%DigestLen-1)
    
    ! reset the states
    CALL MD%Reset()

    RETURN

END SUBROUTINE Skein_ByteDigest_AddBits

!******************************************************************************

SUBROUTINE Skein_HexDigest_AddBits(MD, LastByte, NBits, HexStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then finalize the current hash computation
    !  and return the hash output as a hexadecimal string. <br>
    !  This routine is only used for an input message whose bit length is not
    !  a multiple of eight (i.e. a message with partial bytes). <br>
    !  This procedure is intended to be used by a digest type that implements
    !  a hash function that is an entrant of the SHA-3 competition.  It is mainly
    !  used for a test purpose.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Skein), INTENT(INOUT) :: MD           !! 'Skein' object
    tByte,        INTENT(IN)    :: LastByte     !! the last byte
    tByte,        INTENT(IN)    :: NBits        !! number of bits in the last byte
    tCharAlloc,   INTENT(OUT)   :: HexStr       !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, ALLOCATABLE  :: ByteArr(:)

! FLOW

    CALL MD%AddBitsNDigest(LastByte, NBits, ByteArr)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)
        
    RETURN

END SUBROUTINE Skein_HexDigest_AddBits

!******************************************************************************

SUBROUTINE Skein_Finalize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free a pointer component of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Skein), INTENT(INOUT)  :: MD   !! 'Skein' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    NULLIFY(MD%State)
   
    RETURN

END SUBROUTINE Skein_Finalize

!******************************************************************************

END MODULE MClass_Skein
    
!******************************************************************************
