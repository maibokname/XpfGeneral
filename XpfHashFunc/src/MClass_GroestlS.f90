
MODULE MClass_GroestlS

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *GroestlS* type and its related routines.
!   The *GroestlS* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *GroestlS* type implements an incremental cryptographic hash function
!   by employing either the *Groestl-224* or the *Groestl-256 message-digest*
!   algorithm [1].  The implementation here is based mainly on the *SPHLIB*
!   implementation [2].  <br>
!   By default, the *GroestlS* type employs the *Groestl-256 message-digest*
!   algorithm.  However, a user can specify the *IsGroestl224* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *Groestl-224 message-digest* algorithm
!   instead of the default one. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="http://www.groestl.info/">Grostl - a SHA-3 candidate. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_BytePack,           ONLY: BytePackBE, ByteUnpackBE
    USE MClass_BaseDigest
    USE MClass_MDEngine

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: GroestlS

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64
#define BX64(X, Pos)    ToInt32(SHIFTR(X, Pos))
#define BY64(X, Pos)    IAND(ToInt32(SHIFTR(X, Pos)), ToInt32(Z'000000FF'))
#define RSTT(A, I0, I1, I2, I3, I4, I5, I6, I7) \
    IEOR(IEOR(IEOR(IEOR(IEOR(IEOR(IEOR(T0(BX64(A(I0), 56)),  \
                                       T1(BY64(A(I1), 48))), \
                                       T2(BY64(A(I2), 40))), \
                                       T3(BY64(A(I3), 32))), \
                                       T4(BY64(A(I4), 24))), \
                                       T5(BY64(A(I5), 16))), \
                                       T6(BY64(A(I6),  8))), \
                                       T7(BY64(A(I7),  0)))
#define PC64(A, J, R)   IEOR(A, SHIFTL(ToInt64(J + R), 56))
#define QC64(A, J, R)   IEOR(A, IEOR(-ToInt64(J), ToInt64(R)))

!** MODULE PARAMETERS:
    tIndex, PARAMETER           :: BlockLen = 64_kIndex
    tIndex, PARAMETER           :: DLen224  = 28_kIndex
    tIndex, PARAMETER           :: DLen256  = 32_kIndex
    tLong,  PARAMETER, PUBLIC   :: T0(0:255) = [                      &
            ToInt64(Z'C632F4A5F497A5C6'), ToInt64(Z'F86F978497EB84F8'), &
            ToInt64(Z'EE5EB099B0C799EE'), ToInt64(Z'F67A8C8D8CF78DF6'), &
            ToInt64(Z'FFE8170D17E50DFF'), ToInt64(Z'D60ADCBDDCB7BDD6'), &
            ToInt64(Z'DE16C8B1C8A7B1DE'), ToInt64(Z'916DFC54FC395491'), &
            ToInt64(Z'6090F050F0C05060'), ToInt64(Z'0207050305040302'), &
            ToInt64(Z'CE2EE0A9E087A9CE'), ToInt64(Z'56D1877D87AC7D56'), &
            ToInt64(Z'E7CC2B192BD519E7'), ToInt64(Z'B513A662A67162B5'), &
            ToInt64(Z'4D7C31E6319AE64D'), ToInt64(Z'EC59B59AB5C39AEC'), &
            ToInt64(Z'8F40CF45CF05458F'), ToInt64(Z'1FA3BC9DBC3E9D1F'), &
            ToInt64(Z'8949C040C0094089'), ToInt64(Z'FA68928792EF87FA'), &
            ToInt64(Z'EFD03F153FC515EF'), ToInt64(Z'B29426EB267FEBB2'), &
            ToInt64(Z'8ECE40C94007C98E'), ToInt64(Z'FBE61D0B1DED0BFB'), &
            ToInt64(Z'416E2FEC2F82EC41'), ToInt64(Z'B31AA967A97D67B3'), &
            ToInt64(Z'5F431CFD1CBEFD5F'), ToInt64(Z'456025EA258AEA45'), &
            ToInt64(Z'23F9DABFDA46BF23'), ToInt64(Z'535102F702A6F753'), &
            ToInt64(Z'E445A196A1D396E4'), ToInt64(Z'9B76ED5BED2D5B9B'), &
            ToInt64(Z'75285DC25DEAC275'), ToInt64(Z'E1C5241C24D91CE1'), &
            ToInt64(Z'3DD4E9AEE97AAE3D'), ToInt64(Z'4CF2BE6ABE986A4C'), &
            ToInt64(Z'6C82EE5AEED85A6C'), ToInt64(Z'7EBDC341C3FC417E'), &
            ToInt64(Z'F5F3060206F102F5'), ToInt64(Z'8352D14FD11D4F83'), &
            ToInt64(Z'688CE45CE4D05C68'), ToInt64(Z'515607F407A2F451'), &
            ToInt64(Z'D18D5C345CB934D1'), ToInt64(Z'F9E1180818E908F9'), &
            ToInt64(Z'E24CAE93AEDF93E2'), ToInt64(Z'AB3E9573954D73AB'), &
            ToInt64(Z'6297F553F5C45362'), ToInt64(Z'2A6B413F41543F2A'), &
            ToInt64(Z'081C140C14100C08'), ToInt64(Z'9563F652F6315295'), &
            ToInt64(Z'46E9AF65AF8C6546'), ToInt64(Z'9D7FE25EE2215E9D'), &
            ToInt64(Z'3048782878602830'), ToInt64(Z'37CFF8A1F86EA137'), &
            ToInt64(Z'0A1B110F11140F0A'), ToInt64(Z'2FEBC4B5C45EB52F'), &
            ToInt64(Z'0E151B091B1C090E'), ToInt64(Z'247E5A365A483624'), &
            ToInt64(Z'1BADB69BB6369B1B'), ToInt64(Z'DF98473D47A53DDF'), &
            ToInt64(Z'CDA76A266A8126CD'), ToInt64(Z'4EF5BB69BB9C694E'), &
            ToInt64(Z'7F334CCD4CFECD7F'), ToInt64(Z'EA50BA9FBACF9FEA'), &
            ToInt64(Z'123F2D1B2D241B12'), ToInt64(Z'1DA4B99EB93A9E1D'), &
            ToInt64(Z'58C49C749CB07458'), ToInt64(Z'3446722E72682E34'), &
            ToInt64(Z'3641772D776C2D36'), ToInt64(Z'DC11CDB2CDA3B2DC'), &
            ToInt64(Z'B49D29EE2973EEB4'), ToInt64(Z'5B4D16FB16B6FB5B'), &
            ToInt64(Z'A4A501F60153F6A4'), ToInt64(Z'76A1D74DD7EC4D76'), &
            ToInt64(Z'B714A361A37561B7'), ToInt64(Z'7D3449CE49FACE7D'), &
            ToInt64(Z'52DF8D7B8DA47B52'), ToInt64(Z'DD9F423E42A13EDD'), &
            ToInt64(Z'5ECD937193BC715E'), ToInt64(Z'13B1A297A2269713'), &
            ToInt64(Z'A6A204F50457F5A6'), ToInt64(Z'B901B868B86968B9'), &
            ToInt64(Z'0000000000000000'), ToInt64(Z'C1B5742C74992CC1'), &
            ToInt64(Z'40E0A060A0806040'), ToInt64(Z'E3C2211F21DD1FE3'), &
            ToInt64(Z'793A43C843F2C879'), ToInt64(Z'B69A2CED2C77EDB6'), &
            ToInt64(Z'D40DD9BED9B3BED4'), ToInt64(Z'8D47CA46CA01468D'), &
            ToInt64(Z'671770D970CED967'), ToInt64(Z'72AFDD4BDDE44B72'), &
            ToInt64(Z'94ED79DE7933DE94'), ToInt64(Z'98FF67D4672BD498'), &
            ToInt64(Z'B09323E8237BE8B0'), ToInt64(Z'855BDE4ADE114A85'), &
            ToInt64(Z'BB06BD6BBD6D6BBB'), ToInt64(Z'C5BB7E2A7E912AC5'), &
            ToInt64(Z'4F7B34E5349EE54F'), ToInt64(Z'EDD73A163AC116ED'), &
            ToInt64(Z'86D254C55417C586'), ToInt64(Z'9AF862D7622FD79A'), &
            ToInt64(Z'6699FF55FFCC5566'), ToInt64(Z'11B6A794A7229411'), &
            ToInt64(Z'8AC04ACF4A0FCF8A'), ToInt64(Z'E9D9301030C910E9'), &
            ToInt64(Z'040E0A060A080604'), ToInt64(Z'FE66988198E781FE'), &
            ToInt64(Z'A0AB0BF00B5BF0A0'), ToInt64(Z'78B4CC44CCF04478'), &
            ToInt64(Z'25F0D5BAD54ABA25'), ToInt64(Z'4B753EE33E96E34B'), &
            ToInt64(Z'A2AC0EF30E5FF3A2'), ToInt64(Z'5D4419FE19BAFE5D'), &
            ToInt64(Z'80DB5BC05B1BC080'), ToInt64(Z'0580858A850A8A05'), &
            ToInt64(Z'3FD3ECADEC7EAD3F'), ToInt64(Z'21FEDFBCDF42BC21'), &
            ToInt64(Z'70A8D848D8E04870'), ToInt64(Z'F1FD0C040CF904F1'), &
            ToInt64(Z'63197ADF7AC6DF63'), ToInt64(Z'772F58C158EEC177'), &
            ToInt64(Z'AF309F759F4575AF'), ToInt64(Z'42E7A563A5846342'), &
            ToInt64(Z'2070503050403020'), ToInt64(Z'E5CB2E1A2ED11AE5'), &
            ToInt64(Z'FDEF120E12E10EFD'), ToInt64(Z'BF08B76DB7656DBF'), &
            ToInt64(Z'8155D44CD4194C81'), ToInt64(Z'18243C143C301418'), &
            ToInt64(Z'26795F355F4C3526'), ToInt64(Z'C3B2712F719D2FC3'), &
            ToInt64(Z'BE8638E13867E1BE'), ToInt64(Z'35C8FDA2FD6AA235'), &
            ToInt64(Z'88C74FCC4F0BCC88'), ToInt64(Z'2E654B394B5C392E'), &
            ToInt64(Z'936AF957F93D5793'), ToInt64(Z'55580DF20DAAF255'), &
            ToInt64(Z'FC619D829DE382FC'), ToInt64(Z'7AB3C947C9F4477A'), &
            ToInt64(Z'C827EFACEF8BACC8'), ToInt64(Z'BA8832E7326FE7BA'), &
            ToInt64(Z'324F7D2B7D642B32'), ToInt64(Z'E642A495A4D795E6'), &
            ToInt64(Z'C03BFBA0FB9BA0C0'), ToInt64(Z'19AAB398B3329819'), &
            ToInt64(Z'9EF668D16827D19E'), ToInt64(Z'A322817F815D7FA3'), &
            ToInt64(Z'44EEAA66AA886644'), ToInt64(Z'54D6827E82A87E54'), &
            ToInt64(Z'3BDDE6ABE676AB3B'), ToInt64(Z'0B959E839E16830B'), &
            ToInt64(Z'8CC945CA4503CA8C'), ToInt64(Z'C7BC7B297B9529C7'), &
            ToInt64(Z'6B056ED36ED6D36B'), ToInt64(Z'286C443C44503C28'), &
            ToInt64(Z'A72C8B798B5579A7'), ToInt64(Z'BC813DE23D63E2BC'), &
            ToInt64(Z'1631271D272C1D16'), ToInt64(Z'AD379A769A4176AD'), &
            ToInt64(Z'DB964D3B4DAD3BDB'), ToInt64(Z'649EFA56FAC85664'), &
            ToInt64(Z'74A6D24ED2E84E74'), ToInt64(Z'1436221E22281E14'), &
            ToInt64(Z'92E476DB763FDB92'), ToInt64(Z'0C121E0A1E180A0C'), &
            ToInt64(Z'48FCB46CB4906C48'), ToInt64(Z'B88F37E4376BE4B8'), &
            ToInt64(Z'9F78E75DE7255D9F'), ToInt64(Z'BD0FB26EB2616EBD'), &
            ToInt64(Z'43692AEF2A86EF43'), ToInt64(Z'C435F1A6F193A6C4'), &
            ToInt64(Z'39DAE3A8E372A839'), ToInt64(Z'31C6F7A4F762A431'), &
            ToInt64(Z'D38A593759BD37D3'), ToInt64(Z'F274868B86FF8BF2'), &
            ToInt64(Z'D583563256B132D5'), ToInt64(Z'8B4EC543C50D438B'), &
            ToInt64(Z'6E85EB59EBDC596E'), ToInt64(Z'DA18C2B7C2AFB7DA'), &
            ToInt64(Z'018E8F8C8F028C01'), ToInt64(Z'B11DAC64AC7964B1'), &
            ToInt64(Z'9CF16DD26D23D29C'), ToInt64(Z'49723BE03B92E049'), &
            ToInt64(Z'D81FC7B4C7ABB4D8'), ToInt64(Z'ACB915FA1543FAAC'), &
            ToInt64(Z'F3FA090709FD07F3'), ToInt64(Z'CFA06F256F8525CF'), &
            ToInt64(Z'CA20EAAFEA8FAFCA'), ToInt64(Z'F47D898E89F38EF4'), &
            ToInt64(Z'476720E9208EE947'), ToInt64(Z'1038281828201810'), &
            ToInt64(Z'6F0B64D564DED56F'), ToInt64(Z'F073838883FB88F0'), &
            ToInt64(Z'4AFBB16FB1946F4A'), ToInt64(Z'5CCA967296B8725C'), &
            ToInt64(Z'38546C246C702438'), ToInt64(Z'575F08F108AEF157'), &
            ToInt64(Z'732152C752E6C773'), ToInt64(Z'9764F351F3355197'), &
            ToInt64(Z'CBAE6523658D23CB'), ToInt64(Z'A125847C84597CA1'), &
            ToInt64(Z'E857BF9CBFCB9CE8'), ToInt64(Z'3E5D6321637C213E'), &
            ToInt64(Z'96EA7CDD7C37DD96'), ToInt64(Z'611E7FDC7FC2DC61'), &
            ToInt64(Z'0D9C9186911A860D'), ToInt64(Z'0F9B9485941E850F'), &
            ToInt64(Z'E04BAB90ABDB90E0'), ToInt64(Z'7CBAC642C6F8427C'), &
            ToInt64(Z'712657C457E2C471'), ToInt64(Z'CC29E5AAE583AACC'), &
            ToInt64(Z'90E373D8733BD890'), ToInt64(Z'06090F050F0C0506'), &
            ToInt64(Z'F7F4030103F501F7'), ToInt64(Z'1C2A36123638121C'), &
            ToInt64(Z'C23CFEA3FE9FA3C2'), ToInt64(Z'6A8BE15FE1D45F6A'), &
            ToInt64(Z'AEBE10F91047F9AE'), ToInt64(Z'69026BD06BD2D069'), &
            ToInt64(Z'17BFA891A82E9117'), ToInt64(Z'9971E858E8295899'), &
            ToInt64(Z'3A5369276974273A'), ToInt64(Z'27F7D0B9D04EB927'), &
            ToInt64(Z'D991483848A938D9'), ToInt64(Z'EBDE351335CD13EB'), &
            ToInt64(Z'2BE5CEB3CE56B32B'), ToInt64(Z'2277553355443322'), &
            ToInt64(Z'D204D6BBD6BFBBD2'), ToInt64(Z'A9399070904970A9'), &
            ToInt64(Z'07878089800E8907'), ToInt64(Z'33C1F2A7F266A733'), &
            ToInt64(Z'2DECC1B6C15AB62D'), ToInt64(Z'3C5A66226678223C'), &
            ToInt64(Z'15B8AD92AD2A9215'), ToInt64(Z'C9A96020608920C9'), &
            ToInt64(Z'875CDB49DB154987'), ToInt64(Z'AAB01AFF1A4FFFAA'), &
            ToInt64(Z'50D8887888A07850'), ToInt64(Z'A52B8E7A8E517AA5'), &
            ToInt64(Z'03898A8F8A068F03'), ToInt64(Z'594A13F813B2F859'), &
            ToInt64(Z'09929B809B128009'), ToInt64(Z'1A2339173934171A'), &
            ToInt64(Z'651075DA75CADA65'), ToInt64(Z'D784533153B531D7'), &
            ToInt64(Z'84D551C65113C684'), ToInt64(Z'D003D3B8D3BBB8D0'), &
            ToInt64(Z'82DC5EC35E1FC382'), ToInt64(Z'29E2CBB0CB52B029'), &
            ToInt64(Z'5AC3997799B4775A'), ToInt64(Z'1E2D3311333C111E'), &
            ToInt64(Z'7B3D46CB46F6CB7B'), ToInt64(Z'A8B71FFC1F4BFCA8'), &
            ToInt64(Z'6D0C61D661DAD66D'), ToInt64(Z'2C624E3A4E583A2C')]
    tInteger                    :: Indx
    tLong,  PARAMETER, PUBLIC   :: T1(0:255) = [(RotateLeft(T0(Indx), 56), Indx = 0, 255)]
    tLong,  PARAMETER, PUBLIC   :: T2(0:255) = [(RotateLeft(T0(Indx), 48), Indx = 0, 255)]
    tLong,  PARAMETER, PUBLIC   :: T3(0:255) = [(RotateLeft(T0(Indx), 40), Indx = 0, 255)]
    tLong,  PARAMETER, PUBLIC   :: T4(0:255) = [(RotateLeft(T0(Indx), 32), Indx = 0, 255)]
    tLong,  PARAMETER, PUBLIC   :: T5(0:255) = [(RotateLeft(T0(Indx), 24), Indx = 0, 255)]
    tLong,  PARAMETER, PUBLIC   :: T6(0:255) = [(RotateLeft(T0(Indx), 16), Indx = 0, 255)]
    tLong,  PARAMETER, PUBLIC   :: T7(0:255) = [(RotateLeft(T0(Indx),  8), Indx = 0, 255)]

!** DERIVED TYPE DEFINITIONS
    !> *GroestlS* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing either
    !  the *Groestl-224* or the *Groestl-256 message-digest* algorithm.
    TYPE, EXTENDS(MDEngine) :: GroestlS
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state variable
        tLong       :: H(0:7) = 0_kInt64
        !% flag indicating whether the Groestl-224 algorithm is employed or not.
        tLogical    :: IsGroestl224 = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                    Private Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => GroestlS_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (Groestl-256).
        PROCEDURE       :: Initialize   => GroestlS_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => GroestlS_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => GroestlS_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => GroestlS_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => GroestlS_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => GroestlS_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => GroestlS_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => GroestlS_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => GroestlS_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => GroestlS_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (Groestl-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the Groestl-224 algorithm <br>
        !   --->    CALL MD%Create(IsGroestl224=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
        ! ---------------------------------------------------------------------
    END TYPE GroestlS

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE GroestlS_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(INOUT)  :: MD    !! 'GroestlS' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the Groestl-256 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE GroestlS_Initialize

!******************************************************************************

SUBROUTINE GroestlS_Initialize_wFlag(MD, IsGroestl224)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(INOUT)  :: MD           !! 'GroestlS' object
    tLogical,        INTENT(IN)     :: IsGroestl224
    !^ flag indicating whether the Groestl-224 algorithm is employed or not. <br>
    !  - If true, use the Groestl-224 algorithm. <br>
    !  - Otherwise, use the Groestl-256 algorithm. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsGroestl224 = IsGroestl224
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE GroestlS_Initialize_wFlag

!******************************************************************************

SUBROUTINE GroestlS_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(INOUT)  :: MD   !! 'GroestlS' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    MD%H(0:6) = 0_kInt64
    MD%H(7)   = SHIFTL(ToInt64(MD%GetDigestLen()), 3)
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE GroestlS_Reset

!******************************************************************************

SUBROUTINE GroestlS_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS),                INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(GroestlS :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (GroestlS)
        CALL Dst%Create(Src%IsGroestl224)
        Dst%H      = Src%H
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE GroestlS_GetClone

!******************************************************************************

FUNCTION GroestlS_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(IN) :: MD       !! 'GroestlS' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsGroestl224) THEN
        Name = 'Groestl-224'
    ELSE
        Name = 'Groestl-256'
    END IF

    RETURN

END FUNCTION GroestlS_GetName

!******************************************************************************

FUNCTION GroestlS_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(IN) :: MD       !! 'GroestlS' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsGroestl224) THEN
        Length = DLen224
    ELSE
        Length = DLen256
    END IF

    RETURN

END FUNCTION GroestlS_GetDigestLen

!******************************************************************************

FUNCTION GroestlS_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(IN) :: MD       !! 'GroestlS' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION GroestlS_GetBlockLen

!******************************************************************************

SUBROUTINE GroestlS_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), TARGET, INTENT(INOUT)  :: MD           !! 'GroestlS' object
    tByte,          POINTER, INTENT(INOUT)  :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE GroestlS_SetBufPtr

!******************************************************************************

SUBROUTINE GroestlS_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(INOUT)  :: MD           !! 'GroestlS' object
    tByte,           INTENT(IN)     :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: G(0:7)
    tLong       :: MS(0:7)
    tIndex      :: I

! FLOW

    ! input block
    CALL BytePackBE(BytesIn, 0_kIndex, MS)
    DO I = 0, 7
        G(I) = IEOR(MS(I), MD%H(I))
    END DO

    ! perform permutations
    CALL DoPermP(G)
    CALL DoPermQ(MS)

    ! get output states
    DO I = 0, 7
        MD%H(I) = IEOR(MD%H(I), IEOR(G(I), MS(I)))
    END DO

    RETURN

CONTAINS

    SUBROUTINE DoPermQ(X)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform permutation Q.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: X(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: T(0:7)
        tInteger    :: R, RP1

    ! FLOW

        DO R = 0, 9, 2
            X(0) = QC64(X(0), ToInt32(Z'00000001'), R)
            X(1) = QC64(X(1), ToInt32(Z'00000011'), R)
            X(2) = QC64(X(2), ToInt32(Z'00000021'), R)
            X(3) = QC64(X(3), ToInt32(Z'00000031'), R)
            X(4) = QC64(X(4), ToInt32(Z'00000041'), R)
            X(5) = QC64(X(5), ToInt32(Z'00000051'), R)
            X(6) = QC64(X(6), ToInt32(Z'00000061'), R)
            X(7) = QC64(X(7), ToInt32(Z'00000071'), R)
            T(0) = RSTT(X, 1, 3, 5, 7, 0, 2, 4, 6)
            T(1) = RSTT(X, 2, 4, 6, 0, 1, 3, 5, 7)
            T(2) = RSTT(X, 3, 5, 7, 1, 2, 4, 6, 0)
            T(3) = RSTT(X, 4, 6, 0, 2, 3, 5, 7, 1)
            T(4) = RSTT(X, 5, 7, 1, 3, 4, 6, 0, 2)
            T(5) = RSTT(X, 6, 0, 2, 4, 5, 7, 1, 3)
            T(6) = RSTT(X, 7, 1, 3, 5, 6, 0, 2, 4)
            T(7) = RSTT(X, 0, 2, 4, 6, 7, 1, 3, 5)
            RP1 = R + 1
            T(0) = QC64(T(0), ToInt32(Z'00000001'), RP1)
            T(1) = QC64(T(1), ToInt32(Z'00000011'), RP1)
            T(2) = QC64(T(2), ToInt32(Z'00000021'), RP1)
            T(3) = QC64(T(3), ToInt32(Z'00000031'), RP1)
            T(4) = QC64(T(4), ToInt32(Z'00000041'), RP1)
            T(5) = QC64(T(5), ToInt32(Z'00000051'), RP1)
            T(6) = QC64(T(6), ToInt32(Z'00000061'), RP1)
            T(7) = QC64(T(7), ToInt32(Z'00000071'), RP1)
            X(0) = RSTT(T, 1, 3, 5, 7, 0, 2, 4, 6)
            X(1) = RSTT(T, 2, 4, 6, 0, 1, 3, 5, 7)
            X(2) = RSTT(T, 3, 5, 7, 1, 2, 4, 6, 0)
            X(3) = RSTT(T, 4, 6, 0, 2, 3, 5, 7, 1)
            X(4) = RSTT(T, 5, 7, 1, 3, 4, 6, 0, 2)
            X(5) = RSTT(T, 6, 0, 2, 4, 5, 7, 1, 3)
            X(6) = RSTT(T, 7, 1, 3, 5, 6, 0, 2, 4)
            X(7) = RSTT(T, 0, 2, 4, 6, 7, 1, 3, 5)
        END DO

        RETURN

    END SUBROUTINE DoPermQ

    !**************************************************************************

END SUBROUTINE GroestlS_ProcessBlock

!******************************************************************************

SUBROUTINE DoPermP(X)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform permutation P.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT)    :: X(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: T(0:7)
    tInteger    :: R, RP1

! FLOW

    DO R = 0, 9, 2
        X(0) = PC64(X(0), ToInt32(Z'00000000'), R)
        X(1) = PC64(X(1), ToInt32(Z'00000010'), R)
        X(2) = PC64(X(2), ToInt32(Z'00000020'), R)
        X(3) = PC64(X(3), ToInt32(Z'00000030'), R)
        X(4) = PC64(X(4), ToInt32(Z'00000040'), R)
        X(5) = PC64(X(5), ToInt32(Z'00000050'), R)
        X(6) = PC64(X(6), ToInt32(Z'00000060'), R)
        X(7) = PC64(X(7), ToInt32(Z'00000070'), R)
        T(0) = RSTT(X, 0, 1, 2, 3, 4, 5, 6, 7)
        T(1) = RSTT(X, 1, 2, 3, 4, 5, 6, 7, 0)
        T(2) = RSTT(X, 2, 3, 4, 5, 6, 7, 0, 1)
        T(3) = RSTT(X, 3, 4, 5, 6, 7, 0, 1, 2)
        T(4) = RSTT(X, 4, 5, 6, 7, 0, 1, 2, 3)
        T(5) = RSTT(X, 5, 6, 7, 0, 1, 2, 3, 4)
        T(6) = RSTT(X, 6, 7, 0, 1, 2, 3, 4, 5)
        T(7) = RSTT(X, 7, 0, 1, 2, 3, 4, 5, 6)
        RP1 = R + 1
        T(0) = PC64(T(0), ToInt32(Z'00000000'), RP1)
        T(1) = PC64(T(1), ToInt32(Z'00000010'), RP1)
        T(2) = PC64(T(2), ToInt32(Z'00000020'), RP1)
        T(3) = PC64(T(3), ToInt32(Z'00000030'), RP1)
        T(4) = PC64(T(4), ToInt32(Z'00000040'), RP1)
        T(5) = PC64(T(5), ToInt32(Z'00000050'), RP1)
        T(6) = PC64(T(6), ToInt32(Z'00000060'), RP1)
        T(7) = PC64(T(7), ToInt32(Z'00000070'), RP1)
        X(0) = RSTT(T, 0, 1, 2, 3, 4, 5, 6, 7)
        X(1) = RSTT(T, 1, 2, 3, 4, 5, 6, 7, 0)
        X(2) = RSTT(T, 2, 3, 4, 5, 6, 7, 0, 1)
        X(3) = RSTT(T, 3, 4, 5, 6, 7, 0, 1, 2)
        X(4) = RSTT(T, 4, 5, 6, 7, 0, 1, 2, 3)
        X(5) = RSTT(T, 5, 6, 7, 0, 1, 2, 3, 4)
        X(6) = RSTT(T, 6, 7, 0, 1, 2, 3, 4, 5)
        X(7) = RSTT(T, 7, 0, 1, 2, 3, 4, 5, 6)
    END DO

    RETURN

END SUBROUTINE DoPermP

!******************************************************************************

SUBROUTINE GroestlS_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(INOUT)  :: MD           !! 'GroestlS' object
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE GroestlS_DoPadding

!******************************************************************************

SUBROUTINE GroestlS_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(INOUT)  :: MD           !! 'GroestlS' object
    tByte,           INTENT(IN)     :: LastByte     !! the last byte
    tByte,           INTENT(IN)     :: NBits        !! number of bits in the last byte
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: G(0:7)
    tIndex      :: I, Ptr, DLen
    tLong       :: Count
    tByte       :: Z

! FLOW

    ASSOCIATE(TmpBuf => MD%BufArr)
        Ptr = MD%GetBufLen()
        Z = SHIFTR(FByte80, NBits)
        TmpBuf(Ptr) = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
        Ptr = Ptr + 1_kIndex
        Count = MD%GetBlockCount()
        IF (Ptr <= 56_kIndex) THEN
            TmpBuf(Ptr:55) = FByte00
            Count = Count + 1_kInt64
        ELSE
            TmpBuf(Ptr:63) = FByte00
            CALL MD%ProcessBlock(TmpBuf)
            TmpBuf(0:55) = FByte00
            Count = Count + 2_kInt64
        END IF
        CALL ByteUnpackBE(Count, TmpBuf, 56_kIndex)
        CALL MD%ProcessBlock(TmpBuf)
        G = MD%H
        CALL DoPermP(G)
        DO I = 0_kIndex, 3_kIndex
            CALL ByteUnpackBE(IEOR(MD%H(I+4_kIndex), G(I+4_kIndex)), TmpBuf, I*8_kIndex)
        END DO
        DLen = MD%GetDigestLen()
        BytesOut(Offset:Offset+DLen-1) = TmpBuf(32-DLen:31)
    END ASSOCIATE
        
    RETURN

END SUBROUTINE GroestlS_AddBitsNPad

!******************************************************************************

END MODULE MClass_GroestlS
    
!******************************************************************************
