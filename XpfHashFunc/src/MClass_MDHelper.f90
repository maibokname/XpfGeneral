
MODULE MClass_MDHelper

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *MDHelper* type and its related routines.
!   The *MDHelper* type is an abstract *digest* type extending from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  It is a helper type that implements the padding common to
!   MD4, MD5, and the SHA family.  The implementation works as long as
!   the internal block length is a power of 2, which is the case for all
!   these algorithms. <br>
!   <br>
!   **REFERENCES**: <br>
!   [1] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_BytePack,           ONLY: ByteUnpackBE, ByteUnpackLE
    USE MClass_MDEngine

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: MDHelper

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'MClass_MDHelper'
    tIndex,    PARAMETER    :: MaxLen  = 16_kIndex

!** DERIVED TYPE DEFINITIONS
    !> *MDHelper* is an abstract *digest* type acting as a helper by
    !  implementing the padding common to MD4, MD5, and the SHA family.
    TYPE, ABSTRACT, EXTENDS(MDEngine)   :: MDHelper
        PRIVATE
        !% flag indicating whether the padding is in little-endian order or not 
        tLogical    :: LittleEndian = FalseVal
        !% the length encoding length, in bytes (must be at least 8 and less than MaxLen
        tIndex      :: LenLen       = 8_kIndex
        !% the first padding byte
        tUInt8      :: FByte        = FByte80
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> *HelperInit* is a procedure to initialize components of the *MDHelper* type. <br>
        !  This procedure is NOT intended to be used by a user but the *Create* method
        !  implemented by a *concrete* digest type that extends from this type should
        !  call this method.
        PROCEDURE   :: HelperInit       => MDHelper_Init
        !> *HelperPadding* is a procedure to perform padding required by the hash functions. <br>
        !  This procedure is NOT intended to be used by a user but the *DoPadding* method
        !  implemented by a *concrete* digest type that extends from this type should
        !  call this method.
        PROCEDURE   :: HelperPadding    => MDHelper_DoPadding
        !> *HelperReset* is a procedure to reset components of the digest to their initial values. <br>
        !  This procedure is NOT intended to be used by a user but the *Reset* method implemented
        !  by a *concrete* digest type should call this method.
        PROCEDURE   :: HelperReset      => MDHelper_Reset
        !> *HelperClone* is a procedure to copy components of the source object to the destination one. <br>
        !  This procedure is NOT intended to be used by a user but the *GetClone* method implemented
        !  by a *concrete* digest type should call this method.
        PROCEDURE   :: HelperClone      => MDHelper_Clone
        ! ---------------------------------------------------------------------
    END TYPE MDHelper

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE MDHelper_Init(MDH, LittleEndian, LenLen, FByte)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the object with the specified first padding byte.
    !  The padding byte is normally Z'80'.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDHelper),  INTENT(INOUT) :: MDH          !! 'MDHelper' object
    tLogical,         INTENT(IN)    :: LittleEndian !! true for little-endian padding
    tIndex,           INTENT(IN)    :: LenLen       !! the length encoding length, in bytes (must be at least 8)
    tUInt8, OPTIONAL, INTENT(IN)    :: FByte        !! the first padding byte

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MDH%LittleEndian = LittleEndian
    IF (LenLen > MaxLen) THEN
        CALL Handle_ErrLevel('MDHelper_Init_wFByte', ModName, ErrSevere, &
                             'The specified length must be less than or equal to 16.')
        RETURN
    END IF
    MDH%LenLen = LenLen
    IF (PRESENT(FByte)) THEN
        MDH%FByte = FByte
    ELSE
        MDH%FByte = FByte80
    END IF
        
    RETURN

END SUBROUTINE MDHelper_Init

!******************************************************************************

SUBROUTINE MDHelper_DoPadding(MDH)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the padding.  The padding data is input into the engine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDHelper), INTENT(INOUT)  :: MDH  !! 'MDHelper' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: CountBuf(0:MDH%LenLen-1)
    tIndex      :: DataLen, BlockLen, LenLen, EndLen, I
    tUInt64     :: CurrLen

! FLOW
    
    !+++ initialize +++
    DataLen = MDH%GetBufLen()
    BlockLen = MDH%GetBlockLen()
    CurrLen = MDH%GetBlockCount()*ToInt64(BlockLen)
    CurrLen = (CurrLen + ToInt64(DataLen))*8_kInt64
    LenLen = MDH%LenLen
    CountBuf = 0_kInt8
    
    !+++ unpack current length into a byte array +++
    IF (MDH%LittleEndian) THEN
        CALL ByteUnpackLE(CurrLen, CountBuf, LenLen-8_kIndex)
    ELSE
        CALL ByteUnpackBE(CurrLen, CountBuf, LenLen-8_kIndex)
    END IF
    
    !+++ perform padding +++
    EndLen = IAND((DataLen + LenLen + BlockLen), NOT(BlockLen - 1_kIndex))
    ! insert the first padding byte
    CALL MDH%Update(MDH%Fbyte, 1_kIndex)
    ! add zeros
    DO I = DataLen+1, (EndLen - LenLen)-1
        CALL MDH%Update(FByte00, 1_kIndex)
    END DO
    ! add the current length
    CALL MDH%Update(CountBuf, 0_kIndex, LenLen)

    RETURN

END SUBROUTINE MDHelper_DoPadding

!******************************************************************************

SUBROUTINE MDHelper_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset components of the digest.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDHelper), INTENT(INOUT)  :: MD   !! 'MDHelper' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    MD%LittleEndian = FalseVal
    MD%LenLen       = 8_kIndex
    MD%FByte        = FByte80
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE MDHelper_Reset

!******************************************************************************

SUBROUTINE MDHelper_Clone(SrcMD, DstMD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy components of the source digest.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDHelper), INTENT(IN)     :: SrcMD   !! source object
    CLASS(MDHelper), INTENT(INOUT)  :: DstMD   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    DstMD%LittleEndian = SrcMD%LittleEndian
    DstMD%LenLen       = SrcMD%LenLen
    DstMD%FByte        = SrcMD%FByte
    CALL SrcMD%EngineClone(DstMD)

    RETURN

END SUBROUTINE MDHelper_Clone

!******************************************************************************

END MODULE MClass_MDHelper
    
!******************************************************************************
