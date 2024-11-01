
MODULE MBase_PrgnBuilder
       
!^ **PURPOSE OF THIS MODULE**: <br>
!   This module provides access to random number generator classes and two groups of
!   general routines that perform the following tasks: <br>
!   - Construct a random-number generator class based on the specified algorithm. <br>
!   - Return a number of seeds required to construct the specified generator.

!** USE STATEMENTS:
    USE MBase_Common
    ! base classes
    USE MClass_BaseRNG
    USE MClass_IntegerRNG
    USE MClass_LongRNG
    ! integer classes
    USE MClass_Cmwc4096RNG
    USE MClass_IsaccRNG
    USE MClass_Jsf32RNG
    USE MClass_Kiss32RNG
    USE MClass_L32X64MRNG
    USE MClass_L32X64RNG
    USE MClass_Lfsr113RNG
    USE MClass_Mrg32k3aRNG
    USE MClass_Mt32RNG
    USE MClass_Mwc256RNG
    USE MClass_PcgLcg32RNG
    USE MClass_PcgMcg32RNG
    USE MClass_RanLuxRNG
    USE MClass_Sfc32RNG
    USE MClass_SuperKiss32RNG
    USE MClass_Taus88RNG
    USE MClass_Well32RNG
    USE MClass_XoRoShiRo64RNG
    USE MClass_XoShiRo128RNG
    ! long classes
    USE MClass_ChaChaRNG
    USE MClass_Jsf64RNG
    USE MClass_Kiss64RNG
    USE MClass_KomiRNG
    USE MClass_L64X128RNG
    USE MClass_L64X256RNG
    USE MClass_L64X1024RNG
    USE MClass_L64XMRNG
    USE MClass_L128X128RNG
    USE MClass_L128X256RNG
    USE MClass_L128X1024RNG
    USE MClass_L128XMRNG
    USE MClass_Lfsr258RNG
    USE MClass_Mrg63k3aRNG
    USE MClass_MswsRNG
    USE MClass_Mt64RNG
    USE MClass_PcgRxsMXs64RNG
    USE MClass_RanLuxPpRNG
    USE MClass_Sfc64RNG
    USE MClass_Sip24RNG
    USE MClass_SplitMixRNG
    USE MClass_SuperKiss64RNG
    USE MClass_WyRNG
    USE MClass_XoRoShiRo128RNG
    USE MClass_XoRoShiRo1024RNG
    USE MClass_XoShiRo256RNG
    USE MClass_XoShiRo512RNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! base classes
    PUBLIC :: BaseRNG, IntegerRNG, LongRNG
    ! integer classes
    PUBLIC :: Cmwc4096RNG, IsaccRNG, Jsf32RNG, Kiss32RNG
    PUBLIC :: L32X64MRNG, L32X64RNG, Lfsr113RNG, Mrg32k3aRNG
    PUBLIC :: Mt32RNG, Mwc256RNG, PcgLcg32RNG, PcgMcg32RNG
    PUBLIC :: RanLuxRNG, Sfc32RNG, SuperKiss32RNG, Taus88RNG
    PUBLIC :: Well32RNG, XoRoShiRo64RNG, XoShiRo128RNG
    ! long classes
    PUBLIC :: ChaChaRNG, Jsf64RNG, Kiss64RNG, KomiRNG
    PUBLIC :: L64X128RNG, L64X256RNG, L64X1024RNG, L64XMRNG
    PUBLIC :: L128X128RNG, L128X256RNG, L128X1024RNG, L128XMRNG
    PUBLIC :: Lfsr258RNG, Mrg63k3aRNG, MswsRNG, Mt64RNG
    PUBLIC :: PcgRxsMXs64RNG, RanLuxPpRNG, Sfc64RNG, Sip24RNG
    PUBLIC :: SplitMixRNG, SuperKiss64RNG, WyRNG, XoRoShiRo128RNG
    PUBLIC :: XoRoShiRo1024RNG, XoShiRo256RNG, XoShiRo512RNG
    ! builder procedures
    PUBLIC :: CreateIntegerRng
    PUBLIC :: CreateLongRng
    PUBLIC :: GetSeedSizeIntegerRng
    PUBLIC :: GetSeedSizeLongRng

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"
#define     PublicParam     tSInt32,  PARAMETER, PUBLIC 

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MBase_PrgnBuilder'
    ! -------------------------------------------------------------------------
    ! -----     Parameters for Integer Pseudo-Random Number Generators    -----
    ! -------------------------------------------------------------------------
    !% CMWC (Complimentary-Multiply-With-Carry)
    PublicParam :: I32_CMWC          =  1
    !% ISAAC (Indirection, Shift, Accumulate, Add, and Count)
    PublicParam :: I32_ISACC         =  2
    !% Jenkins's small fast non-cryptographic (32-bit variant)
    PublicParam :: I32_JSF           =  3
    !% Marsaglia's 32-bit KISS (Keep it Simple Stupid)
    PublicParam :: I32_KISS          =  4
    !% L32-X64-Mix based on Java's JDK.Random
    PublicParam :: I32_L32_X64_M1    =  5
    !% L32-X64-Mix based on Apache Commons RNG
    PublicParam :: I32_L32_X64_M2    =  6
    !% L'Ecuyer's 1999 32-bit Composite LFSR
    PublicParam :: I32_LFSR113       =  7
    !% L'Ecuyer's 32-bit combined Multiple Recursive Generator (cMRG)
    PublicParam :: I32_MRG32K3A      =  8
    !% 32-bit Mersenne Twister (MT)
    PublicParam :: I32_MT            =  9
    !% Marsaglia's Multiply-With-Carry (MWC)
    PublicParam :: I32_MWC           = 10
    !% 32-bit PCG's LCG-XorShift-RandomShift
    PublicParam :: I32_PCG_LCG_XRS   = 11
    !% 32-bit PCG's LCG-XorShift-RandomRotate
    PublicParam :: I32_PCG_LCG_XRR   = 12
    !% 32-bit PCG's MCG-XorShift-RandomShift
    PublicParam :: I32_PCG_MCG_XRS   = 13
    !% 32-bit PCG's MCG-XorShift-RandomRotate
    PublicParam :: I32_PCG_MCG_XRR   = 14
    !% 32-bit RANLUX
    PublicParam :: I32_RANLUX        = 15
    !% Doty-Humphrey's 32-bit Small, Fast, Counting (SFC)
    PublicParam :: I32_SFC           = 16
    !% Marsaglia's 32-bit SuperKISS
    PublicParam :: I32_SUPERKISS     = 17
    !% L'Ecuyer's 1996 Three-Component Tausworthe
    PublicParam :: I32_TUAS88        = 18
    !% Well Equidistributed Long-period Linear (WELL) 512a
    PublicParam :: I32_WELL512A      = 19
    !% Well Equidistributed Long-period Linear (WELL) 1024a
    PublicParam :: I32_WELL1024A     = 20
    !% Well Equidistributed Long-period Linear (WELL) 19937a
    PublicParam :: I32_WELL19937A    = 21
    !% Well Equidistributed Long-period Linear (WELL) 19937c
    PublicParam :: I32_WELL19937C    = 22
    !% Well Equidistributed Long-period Linear (WELL) 49937a
    PublicParam :: I32_WELL49937A    = 23
    !% Well Equidistributed Long-period Linear (WELL) 49937b
    PublicParam :: I32_WELL49937B    = 24
    !% XOR_Rotate_Shift_Rotate (XoRoShiRo) 64Star
    PublicParam :: I32_XOROSHIRO64S  = 25
    !% XOR_Rotate_Shift_Rotate (XoRoShiRo) 64StarStar
    PublicParam :: I32_XOROSHIRO64SS = 26
    !% XOR_Shift_Rotate (XoShiRo) 128StarStar
    PublicParam :: I32_XOSHIRO128SS  = 27
    !% XOR_Shift_Rotate (XoShiRo) 128Plus
    PublicParam :: I32_XOSHIRO128P   = 28
    !% XOR_Shift_Rotate (XoShiRo) 128PlusPlus
    PublicParam :: I32_XOSHIRO128PP  = 29
    ! -------------------------------------------------------------------------
    ! -----     Parameters for Long Pseudo-Random Number Generators       -----
    ! -------------------------------------------------------------------------
    !% Chacha cipher
    PublicParam :: I64_CHACHA          =  1
    !% Jenkins's small fast non-cryptographic (64-bit variant)
    PublicParam :: I64_JSF             =  2
    !% Marsaglia's 64-bit KISS (Keep it Simple Stupid)
    PublicParam :: I64_KISS            =  3
    !% Vaneev's KomiRand
    PublicParam :: I64_KOMI            =  4
    !% L64-X128-Mix based on Java's JDK.Random
    PublicParam :: I64_L64_X128_M1     =  5
    !% L64-X128-StarStar based on Java's JDK.Random
    PublicParam :: I64_L64_X128_SS1    =  6
    !% L64-X256-Mix based on Java's JDK.Random
    PublicParam :: I64_L64_X256_M1     =  7
    !% L64-X1024-Mix based on Java's JDK.Random
    PublicParam :: I64_L64_X1024_M1    =  8
    !% L128-X128-Mix based on Java's JDK.Random
    PublicParam :: I64_L128_X128_M1    =  9
    !% L128-X256-Mix based on Java's JDK.Random
    PublicParam :: I64_L128_X256_M1    = 10
    !% L128-X1024-Mix based on Java's JDK.Random
    PublicParam :: I64_L128_X1024_M1   = 11
    !% L64-X128-Mix based on Apache Commons RNG
    PublicParam :: I64_L64_X128_M2     = 12
    !% L64-X128-StarStar based on Apache Commons RNG
    PublicParam :: I64_L64_X128_SS2    = 13
    !% L64-X256-Mix based on Apache Commons RNG
    PublicParam :: I64_L64_X256_M2     = 14
    !% L64-X1024-Mix based on Apache Commons RNG
    PublicParam :: I64_L64_X1024_M2    = 15
    !% L128-X128-Mix based on Apache Commons RNG
    PublicParam :: I64_L128_X128_M2    = 16
    !% L128-X256-Mix based on Apache Commons RNG
    PublicParam :: I64_L128_X256_M2    = 17
    !% L128-X1024-Mix based on Apache Commons RNG
    PublicParam :: I64_L128_X1024_M2   = 18
    !% L'Ecuyer's 1999 64-bit Composite LFSR
    PublicParam :: I64_LFSR258         = 19
    !% L'Ecuyer's 63-bit combined Multiple Recursive Generator (cMRG)
    PublicParam :: I64_MRG63K3A        = 20
    !% Middle-Square Weyl Sequence (MSWS)
    PublicParam :: I64_MSWS            = 21
    !% 64-bit Mersenne Twister (MT)
    PublicParam :: I64_MT              = 22
    !% 64-bit PCG's LCG-RandomXorshift-Multiply-Xorshift
    PublicParam :: I64_PCG_RXS_M_XS    = 23
    !% 64-bit RANLUX++
    PublicParam :: I64_RANLUXPP        = 24
    !% Doty-Humphrey's 64-bit Small, Fast, Counting (SFC)
    PublicParam :: I64_SFC             = 25
    !% SipHash24
    PublicParam :: I64_SIP24           = 26
    !% SplitMix
    PublicParam :: I64_SPLITMIX        = 27
    !% Marsaglia's 64-bit SuperKISS
    PublicParam :: I64_SUPERKISS       = 28
    !% Wang Yi's WyRand
    PublicParam :: I64_WY              = 29
    !% XOR_Rotate_Shift_Rotate (XoRoShiRo) 128StarStar
    PublicParam :: I64_XOROSHIRO128SS  = 30
    !% XOR_Rotate_Shift_Rotate (XoRoShiRo) 128Plus
    PublicParam :: I64_XOROSHIRO128P   = 31
    !% XOR_Rotate_Shift_Rotate (XoRoShiRo) 128PlusPlus
    PublicParam :: I64_XOROSHIRO128PP  = 32
    !% XOR_Rotate_Shift_Rotate (XoRoShiRo) 1024Star
    PublicParam :: I64_XOROSHIRO1024S  = 33
    !% XOR_Rotate_Shift_Rotate (XoRoShiRo) 1024StarStar
    PublicParam :: I64_XOROSHIRO1024SS = 34
    !% XOR_Rotate_Shift_Rotate (XoRoShiRo) 1024PlusPlus
    PublicParam :: I64_XOROSHIRO1024PP = 35
    !% XOR_Shift_Rotate (XoShiRo) 256StarStar
    PublicParam :: I64_XOSHIRO256SS    = 36
    !% XOR_Shift_Rotate (XoShiRo) 256Plus
    PublicParam :: I64_XOSHIRO256P     = 37
    !% XOR_Shift_Rotate (XoShiRo) 256PlusPlus
    PublicParam :: I64_XOSHIRO256PP    = 38
    !% XOR_Shift_Rotate (XoShiRo) 512StarStar
    PublicParam :: I64_XOSHIRO512SS    = 39
    !% XOR_Shift_Rotate (XoShiRo) 512Plus
    PublicParam :: I64_XOSHIRO512P     = 40
    !% XOR_Shift_Rotate (XoShiRo) 512PlusPlus
    PublicParam :: I64_XOSHIRO512PP    = 41

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION CreateIntegerRng(RngAlgo, Seed) RESULT(NewRng)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create an instance of the specified *Integer* random number generator
    !  as well as to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,            INTENT(IN)  :: RngAlgo  !! flag indicating algorithm (1-29)
    tSInt32,  OPTIONAL, INTENT(IN)  :: Seed(:)  !! seed(s)
    CLASS(BaseRNG),     ALLOCATABLE :: NewRng   !! 'BaseRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (RngAlgo)
    CASE (I32_CMWC)
        ALLOCATE(Cmwc4096RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Cmwc4096RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_ISACC)
        ALLOCATE(IsaccRNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (IsaccRNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_JSF)
        ALLOCATE(Jsf32RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Jsf32RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_KISS)
        ALLOCATE(Kiss32RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Kiss32RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_L32_X64_M1)
        ALLOCATE(L32X64RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (L32X64RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_L32_X64_M2)
        ALLOCATE(L32X64MRNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (L32X64MRNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_LFSR113)
        ALLOCATE(Lfsr113RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Lfsr113RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_MRG32K3A)
        ALLOCATE(Mrg32k3aRNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Mrg32k3aRNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_MT)
        ALLOCATE(Mt32RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Mt32RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_MWC)
        ALLOCATE(Mwc256RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Mwc256RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_PCG_LCG_XRS, I32_PCG_LCG_XRR)
        ALLOCATE(PcgLcg32RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (PcgLcg32RNG)
            IF (RngAlgo == I32_PCG_LCG_XRS) THEN
                CALL NewRng%Initialize(TrueVal, Seed)
            ELSE
                CALL NewRng%Initialize(FalseVal, Seed)
            END IF
        END SELECT
    CASE (I32_PCG_MCG_XRS, I32_PCG_MCG_XRR)
        ALLOCATE(PcgMcg32RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (PcgMcg32RNG)
            IF (RngAlgo == I32_PCG_MCG_XRS) THEN
                CALL NewRng%Initialize(TrueVal, Seed)
            ELSE
                CALL NewRng%Initialize(FalseVal, Seed)
            END IF
        END SELECT
    CASE (I32_RANLUX)
        ALLOCATE(RanLuxRNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (RanLuxRNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_SFC)
        ALLOCATE(Sfc32RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Sfc32RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_SUPERKISS)
        ALLOCATE(SuperKiss32RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (SuperKiss32RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_TUAS88)
        ALLOCATE(Taus88RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Taus88RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I32_WELL512A, I32_WELL1024A, I32_WELL19937A, &
          I32_WELL19937C, I32_WELL49937A, I32_WELL49937B)
        ALLOCATE(Well32RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Well32RNG)
            SELECT CASE (RngAlgo)
            CASE (I32_WELL512A)
                CALL NewRng%Initialize(WELL512a, Seed)
            CASE (I32_WELL1024A)
                CALL NewRng%Initialize(WELL1024a, Seed)
            CASE (I32_WELL19937A)
                CALL NewRng%Initialize(WELL19937a, Seed)
            CASE (I32_WELL19937C)
                CALL NewRng%Initialize(WELL19937C, Seed)
            CASE (I32_WELL49937A)
                CALL NewRng%Initialize(WELL49937a, Seed)
            CASE (I32_WELL49937B)
                CALL NewRng%Initialize(WELL49937b, Seed)
            END SELECT
        END SELECT
    CASE (I32_XOROSHIRO64S, I32_XOROSHIRO64SS)
        ALLOCATE(XoRoShiRo64RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (XoRoShiRo64RNG)
            IF (RngAlgo == I32_XOROSHIRO64S) THEN
                CALL NewRng%Initialize(TrueVal, Seed)
            ELSE
                CALL NewRng%Initialize(FalseVal, Seed)
            END IF
        END SELECT
    CASE (I32_XOSHIRO128SS, I32_XOSHIRO128P, I32_XOSHIRO128PP)
        ALLOCATE(XoShiRo128RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (XoShiRo128RNG)
            SELECT CASE (RngAlgo)
            CASE (I32_XOSHIRO128SS)
                CALL NewRng%Initialize(XoShiRo128StarStar, Seed)
            CASE (I32_XOSHIRO128P)
                CALL NewRng%Initialize(XoShiRo128Plus, Seed)
            CASE (I32_XOSHIRO128PP)
                CALL NewRng%Initialize(XoShiRo128PlusPlus, Seed)
            END SELECT
        END SELECT
    CASE DEFAULT
        ALLOCATE(Mt32RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Mt32RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    END SELECT
    
    RETURN

END FUNCTION CreateIntegerRng

!******************************************************************************

FUNCTION CreateLongRng(RngAlgo, Seed) RESULT(NewRng)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create an instance of the specified *Long* random number generator
    !  as well as to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,           INTENT(IN)   :: RngAlgo  !! flag indicating algorithm (1-41)
    tSInt64, OPTIONAL, INTENT(IN)   :: Seed(:)  !! seed(s)
    CLASS(BaseRNG),  ALLOCATABLE    :: NewRng   !! 'BaseRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (RngAlgo)
    CASE (I64_CHACHA)
        ALLOCATE(ChaChaRNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (ChaChaRNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_JSF)
        ALLOCATE(Jsf64RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Jsf64RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_KISS)
        ALLOCATE(Kiss64RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Kiss64RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_KOMI)
        ALLOCATE(KomiRNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (KomiRNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_L64_X128_M1, I64_L64_X128_SS1)
        ALLOCATE(L64X128RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (L64X128RNG)
            IF (RngAlgo == I64_L64_X128_SS1) THEN
                CALL NewRng%Initialize(Seed, UseStarStar=TrueVal)
            ELSE
                CALL NewRng%Initialize(Seed, UseStarStar=FalseVal)
            END IF
        END SELECT
    CASE (I64_L64_X256_M1)
        ALLOCATE(L64X256RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (L64X256RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_L64_X1024_M1)
        ALLOCATE(L64X1024RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (L64X1024RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_L128_X128_M1)
        ALLOCATE(L128X128RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (L128X128RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_L128_X256_M1)
        ALLOCATE(L128X256RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (L128X256RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_L128_X1024_M1)
        ALLOCATE(L128X1024RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (L128X1024RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_L64_X128_M2, I64_L64_X128_SS2, I64_L64_X256_M2, I64_L64_X1024_M2)
        ALLOCATE(L64XMRNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (L64XMRNG)
            SELECT CASE (RngAlgo)
            CASE (I64_L64_X128_M2)
                CALL NewRng%Initialize(L64X128Mix, Seed)
            CASE (I64_L64_X128_SS2)
                CALL NewRng%Initialize(L64X128StarStar, Seed)
            CASE (I64_L64_X256_M2)
                CALL NewRng%Initialize(L64X256Mix, Seed)
            CASE (I64_L64_X1024_M2)
                CALL NewRng%Initialize(L64X1024Mix, Seed)
            END SELECT
        END SELECT
    CASE (I64_L128_X128_M2, I64_L128_X256_M2, I64_L128_X1024_M2)
        ALLOCATE(L128XMRNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (L128XMRNG)
            SELECT CASE (RngAlgo)
            CASE (I64_L128_X128_M2)
                CALL NewRng%Initialize(L128X128Mix, Seed)
            CASE (I64_L128_X256_M2)
                CALL NewRng%Initialize(L128X256Mix, Seed)
            CASE (I64_L128_X1024_M2)
                CALL NewRng%Initialize(L128X1024Mix, Seed)
            END SELECT
        END SELECT
    CASE (I64_LFSR258)
        ALLOCATE(Lfsr258RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Lfsr258RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_MRG63K3A)
        ALLOCATE(Mrg63k3aRNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Mrg63k3aRNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_MSWS)
        ALLOCATE(MswsRNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (MswsRNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_MT)
        ALLOCATE(Mt64RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Mt64RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_PCG_RXS_M_XS)
        ALLOCATE(PcgRxsMXs64RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (PcgRxsMXs64RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_RANLUXPP)
        ALLOCATE(RanLuxPpRNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (RanLuxPpRNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_SFC)
        ALLOCATE(Sfc64RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Sfc64RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_SIP24)
        ALLOCATE(Sip24RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Sip24RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_SPLITMIX)
        ALLOCATE(SplitMixRNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (SplitMixRNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_SUPERKISS)
        ALLOCATE(SuperKiss64RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (SuperKiss64RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_WY)
        ALLOCATE(WyRNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (WyRNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
    CASE (I64_XOROSHIRO128SS, I64_XOROSHIRO128P, I64_XOROSHIRO128PP)
        ALLOCATE(XoRoShiRo128RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (XoRoShiRo128RNG)
            SELECT CASE (RngAlgo)
            CASE (I64_XOROSHIRO128SS)
                CALL NewRng%Initialize(XoRoShiRo128StarStar, Seed)
            CASE (I64_XOROSHIRO128P)
                CALL NewRng%Initialize(XoRoShiRo128Plus, Seed)
            CASE (I64_XOROSHIRO128PP)
                CALL NewRng%Initialize(XoRoShiRo128PlusPlus, Seed)
            END SELECT
        END SELECT
    CASE (I64_XOROSHIRO1024S, I64_XOROSHIRO1024SS, I64_XOROSHIRO1024PP)
        ALLOCATE(XoRoShiRo1024RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (XoRoShiRo1024RNG)
            SELECT CASE (RngAlgo)
            CASE (I64_XOROSHIRO1024S)
                CALL NewRng%Initialize(XoRoShiRo1024Star, Seed)
            CASE (I64_XOROSHIRO1024SS)
                CALL NewRng%Initialize(XoRoShiRo1024StarStar, Seed)
            CASE (I64_XOROSHIRO1024PP)
                CALL NewRng%Initialize(XoRoShiRo1024PlusPlus, Seed)
            END SELECT
        END SELECT
    CASE (I64_XOSHIRO256SS, I64_XOSHIRO256P, I64_XOSHIRO256PP)
        ALLOCATE(XoShiRo256RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (XoShiRo256RNG)
            SELECT CASE (RngAlgo)
            CASE (I64_XOSHIRO256SS)
                CALL NewRng%Initialize(XoShiRo256StarStar, Seed)
            CASE (I64_XOSHIRO256P)
                CALL NewRng%Initialize(XoShiRo256Plus, Seed)
            CASE (I64_XOSHIRO256PP)
                CALL NewRng%Initialize(XoShiRo256PlusPlus, Seed)
            END SELECT
        END SELECT
    CASE (I64_XOSHIRO512SS, I64_XOSHIRO512P, I64_XOSHIRO512PP)
        ALLOCATE(XoShiRo512RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (XoShiRo512RNG)
            SELECT CASE (RngAlgo)
            CASE (I64_XOSHIRO512SS)
                CALL NewRng%Initialize(XoShiRo512StarStar, Seed)
            CASE (I64_XOSHIRO512P)
                CALL NewRng%Initialize(XoShiRo512Plus, Seed)
            CASE (I64_XOSHIRO512PP)
                CALL NewRng%Initialize(XoShiRo512PlusPlus, Seed)
            END SELECT
        END SELECT
    CASE DEFAULT
        ALLOCATE(Mt64RNG :: NewRng)
        SELECT TYPE (NewRng)
        TYPE IS (Mt64RNG)
            CALL NewRng%Initialize(Seed)
        END SELECT
   END SELECT
    
    RETURN

END FUNCTION CreateLongRng

!******************************************************************************

FUNCTION GetSeedSizeIntegerRng(RngAlgo) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the number of seeds required by the specified algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: RngAlgo  !! flag indicating algorithm (1-29)
    tIndex                  :: Number   !! number of seeds required by the specified algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (RngAlgo)
    CASE (I32_CMWC)
        Number = 4097_kIndex
    CASE (I32_ISACC)
        Number = SHIFTL(1_kIndex, 8)
    CASE (I32_JSF)
        Number = 1_kIndex
    CASE (I32_KISS)
        Number = 4_kIndex
    CASE (I32_L32_X64_M1)
        Number = 2_kIndex
    CASE (I32_L32_X64_M2)
        Number = 4_kIndex
    CASE (I32_LFSR113)
        Number = 4_kIndex
    CASE (I32_MRG32K3A)
        Number = 6_kIndex
    CASE (I32_MT)
        Number = 624_kIndex
    CASE (I32_MWC)
        Number = 257_kIndex
    CASE (I32_PCG_LCG_XRS, I32_PCG_LCG_XRR)
        Number = 4_kIndex
    CASE (I32_PCG_MCG_XRS, I32_PCG_MCG_XRR)
        Number = 2_kIndex
    CASE (I32_RANLUX)
        Number = 1_kIndex
    CASE (I32_SFC)
        Number = 3_kIndex
    CASE (I32_SUPERKISS)
        Number = 3_kIndex
    CASE (I32_TUAS88)
        Number = 3_kIndex
    CASE (I32_WELL512A)
        Number = CalculateBlockCount(512)
    CASE (I32_WELL1024A)
        Number = CalculateBlockCount(1024)
    CASE (I32_WELL19937A)
        Number = CalculateBlockCount(19937)
    CASE (I32_WELL19937C)
        Number = CalculateBlockCount(19937)
    CASE (I32_WELL49937A)
        Number = CalculateBlockCount(49937)
    CASE (I32_WELL49937B)
        Number = CalculateBlockCount(49937)
    CASE (I32_XOROSHIRO64S, I32_XOROSHIRO64SS)
        Number = 2_kIndex
    CASE (I32_XOSHIRO128SS, I32_XOSHIRO128P, I32_XOSHIRO128PP)
        Number = 4_kIndex
    CASE DEFAULT
        Number = 624_kIndex
    END SELECT
    
    RETURN

END FUNCTION GetSeedSizeIntegerRng

!******************************************************************************

FUNCTION GetSeedSizeLongRng(RngAlgo) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the number of seeds required by the specified algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: RngAlgo  !! flag indicating algorithm (1-41)
    tIndex                  :: Number   !! number of seeds required by the specified algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (RngAlgo)
    CASE (I64_CHACHA)
        Number = 5_kIndex
    CASE (I64_JSF)
        Number = 1_kIndex
    CASE (I64_KISS)
        Number = 4_kIndex
    CASE (I64_KOMI)
        Number = 1_kIndex
    CASE (I64_L64_X128_M1, I64_L64_X128_SS1, I64_L64_X256_M1, I64_L64_X1024_M1)
        Number = 1_kIndex
    CASE (I64_L128_X128_M1, I64_L128_X256_M1, I64_L128_X1024_M1)
        Number = 1_kIndex
    CASE (I64_L64_X128_M2, I64_L64_X128_SS2)
        Number = 4_kIndex
    CASE (I64_L64_X256_M2)
        Number = 6_kIndex
    CASE (I64_L64_X1024_M2)
        Number = 18_kIndex
    CASE (I64_L128_X128_M2)
        Number = 6_kIndex
    CASE (I64_L128_X256_M2)
        Number = 8_kIndex
    CASE (I64_L128_X1024_M2)
        Number = 20_kIndex
    CASE (I64_LFSR258)
        Number = 5_kIndex
    CASE (I64_MRG63K3A)
        Number = 6_kIndex
    CASE (I64_MSWS)
        Number = 6_kIndex
    CASE (I64_MT)
        Number = 312_kIndex
    CASE (I64_PCG_RXS_M_XS)
        Number = 2_kIndex
    CASE (I64_RANLUXPP)
        Number = 1_kIndex
    CASE (I64_SFC)
        Number = 3_kIndex
    CASE (I64_SIP24)
        Number = 4_kIndex
    CASE (I64_SPLITMIX)
        Number = 1_kIndex
    CASE (I64_SUPERKISS)
        Number = 3_kIndex
    CASE (I64_WY)
        Number = 1_kIndex
    CASE (I64_XOROSHIRO128SS, I64_XOROSHIRO128P, I64_XOROSHIRO128PP)
        Number = 2_kIndex
    CASE (I64_XOROSHIRO1024S, I64_XOROSHIRO1024SS, I64_XOROSHIRO1024PP)
        Number = 16_kIndex
    CASE (I64_XOSHIRO256SS, I64_XOSHIRO256P, I64_XOSHIRO256PP)
        Number = 4_kIndex
    CASE (I64_XOSHIRO512SS, I64_XOSHIRO512P, I64_XOSHIRO512PP)
        Number = 8_kIndex
    CASE DEFAULT
        Number = 312_kIndex
    END SELECT
    
    RETURN

END FUNCTION GetSeedSizeLongRng

!******************************************************************************

END MODULE MBase_PrgnBuilder
    
!******************************************************************************
