
MODULE MBase_MathUtil

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various routines relating to mathematic operations. <br>

!** USE STATEMENTS:
    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE MBase_Common
    USE MBase_UIntUtil,   ONLY: UMOD

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: IdentityMatrix
    PUBLIC :: TransposeMatrix
    PUBLIC :: RandomGen
    PUBLIC :: LOG
    PUBLIC :: GCD
    PUBLIC :: IsPrime
    PUBLIC :: NextPrime

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** DERIVED TYPE DEFINITIONS
#ifdef  __INTEL_COMPILER
    TYPE BaseSet(Len)
        tIndex, LEN :: Len
        tSInt64     :: Val(0:Len-1)
    END TYPE
#endif

!** MODULE PARAMETERS:
    tSInt64, PARAMETER  :: FLOOR_SQRT_MAX_LONG = 3037000499_kInt64
    tSInt64, PARAMETER  :: MaskU32 = ToInt64(Z'00000000FFFFFFFF')
#ifdef  __INTEL_COMPILER
    ! These parameters will only work for Intel compilers.
    TYPE(BaseSet(2)), PARAMETER :: BaseSet1 = BaseSet(2_kIndex) &
        ([291830_kInt64, 126401071349994536_kInt64])
    TYPE(BaseSet(3)), PARAMETER :: BaseSet2 = BaseSet(3_kIndex) &
        ([885594168_kInt64, 725270293939359937_kInt64, 3569819667048198375_kInt64])
    TYPE(BaseSet(4)), PARAMETER :: BaseSet3 = BaseSet(4_kIndex) &
        ([273919523040_kInt64, 15_kInt64, 7363882082_kInt64, 992620450144556_kInt64])
    TYPE(BaseSet(5)), PARAMETER :: BaseSet4 = BaseSet(5_kIndex) &
        ([47636622961200_kInt64, 2_kInt64, 2570940_kInt64, 211991001_kInt64, 3749873356_kInt64])
    TYPE(BaseSet(6)), PARAMETER :: BaseSet5 = BaseSet(6_kIndex) &
        ([7999252175582850_kInt64, 2_kInt64, 4130806001517_kInt64, 149795463772692060_kInt64, &
          186635894390467037_kInt64, 3967304179347715805_kInt64])
    TYPE(BaseSet(7)), PARAMETER :: BaseSet6 = BaseSet(7_kIndex) &
        ([585226005592931976_kInt64, 2_kInt64, 123635709730000_kInt64, 9233062284813009_kInt64, &
          43835965440333360_kInt64, 761179012939631437_kInt64, 1263739024124850375_kInt64])
    TYPE(BaseSet(8)), PARAMETER :: BaseSet7 = BaseSet(8_kIndex) &
        ([9223372036854775807_kInt64, 2_kInt64, 325_kInt64, 9375_kInt64, 28178_kInt64, &
          450775_kInt64, 9780504_kInt64, 1795265022_kInt64])
#else
    ! These parameters will work for both Intel and GFortran compilers.
    tSInt64, PARAMETER  :: BaseSet1(0:1) = [291830_kInt64, 126401071349994536_kInt64]
    tSInt64, PARAMETER  :: BaseSet2(0:2) = [885594168_kInt64, 725270293939359937_kInt64, 3569819667048198375_kInt64]
    tSInt64, PARAMETER  :: BaseSet3(0:3) = [273919523040_kInt64, 15_kInt64, 7363882082_kInt64, 992620450144556_kInt64]
    tSInt64, PARAMETER  :: BaseSet4(0:4) = [47636622961200_kInt64, 2_kInt64, 2570940_kInt64, 211991001_kInt64, 3749873356_kInt64]
    tSInt64, PARAMETER  :: BaseSet5(0:5) = [7999252175582850_kInt64, 2_kInt64, 4130806001517_kInt64, 149795463772692060_kInt64, &
          186635894390467037_kInt64, 3967304179347715805_kInt64]
    tSInt64, PARAMETER  :: BaseSet6(0:6) = [585226005592931976_kInt64, 2_kInt64, 123635709730000_kInt64, 9233062284813009_kInt64, &
          43835965440333360_kInt64, 761179012939631437_kInt64, 1263739024124850375_kInt64]
    tSInt64, PARAMETER  :: BaseSet7(0:7) = [9223372036854775807_kInt64, 2_kInt64, 325_kInt64, 9375_kInt64, 28178_kInt64, &
          450775_kInt64, 9780504_kInt64, 1795265022_kInt64]
#endif

!** INTERFACE DEFINITIONS:
    INTERFACE LOG
        !^ **Function Interface**: LOG <br>
        ! **Purpose**:  To compute the logarithm of the input for a specified base. <br>
        !  **Usage**: <br>
        !   --->    LogVal = LOG(InVal, Base) <br>
        MODULE PROCEDURE Logarithm
    END INTERFACE
    INTERFACE GCD
        !^ **Function Interface**: GCD <br>
        ! **Purpose**:  To return the greatest common divisor of A and B.  To return 0
        !               if A < 0 or B < 0.  Also, return 0 if A == 0 and B == 0. <br>
        !  **Usage**: <br>
        !   --->    GCDNum = GCD(A, B) <br>
        MODULE PROCEDURE GCD_I32
        MODULE PROCEDURE GCD_I64
    END INTERFACE
    INTERFACE IsPrime
        !^ **Function Interface**: IsPrime <br>
        ! **Purpose**:  To check whether the specified number is a prime number or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsPrime(Number) <br>
        !   --->    IF (.NOT.IsPrime(Number)) DoSomething <br>
        MODULE PROCEDURE IsPrime_I32
        MODULE PROCEDURE IsPrime_I64
    END INTERFACE
    INTERFACE NextPrime
        !^ **Function Interface**: NextPrime <br>
        ! **Purpose**:  To return the smallest prime greater than or equal to the specified number. <br>
        !  **Usage**: <br>
        !   --->    PrimeNum = IsPrime(Number) <br>
        MODULE PROCEDURE NextPrime_I32
        MODULE PROCEDURE NextPrime_I64
    END INTERFACE
    ABSTRACT INTERFACE
        FUNCTION Mul_Mod(A, B, M) RESULT(X)
            IMPORT
            tSInt64, INTENT(IN) :: A, B, M
            tSInt64             :: X
        END FUNCTION
        !**********************************************************************
        FUNCTION Sqr_Mod(A, M) RESULT(X)
            IMPORT
            tSInt64, INTENT(IN) :: A, M
            tSInt64             :: X
        END FUNCTION
        !**********************************************************************
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

PURE FUNCTION IdentityMatrix(M) RESULT(IDMat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create identity matrix.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: M                !! number of row (and column)
    tFloat              :: IDMat(M,M)       !! M by M array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I    ! index

!** FLOW:

    ! initialize all elements to zero
    IDMat = Zero

    ! set the diagonal elements to one
    FORALL (I=1:M) IDMat(I,I) = One

! alternative implementations
!    DO I = 1,M
!        IDMat(I,I) = One
!    END DO

    RETURN

END FUNCTION IdentityMatrix

!******************************************************************************

PURE FUNCTION TransposeMatrix(M,N,Matrix) RESULT(TransMat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To transpose matrix.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: M                !! number of row of input matrix
    tIndex, INTENT(IN)  :: N                !! number of column of input matrix
    tFloat, INTENT(IN)  :: Matrix(M,N)      !! M by N array, the input matrix
    tFloat              :: TransMat(N,M)    !! N by M array, the transpose matrix

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex     :: I, J

!** FLOW:

    FORALL (I=1:N, J=1:M) TransMat(I,J) = Matrix(J,I)

! alternative implementations
!    DO I = 1, N
!        DO J = 1, M
!            TransMat(I,J) = Matrix(J,I)
!        END DO
!    END DO
!    DO CONCURRENT (I=1:N, J=1:M)
!        TransMat(I,J) = Matrix(J,I)
!    END DO

    RETURN

END FUNCTION TransposeMatrix

!******************************************************************************

ELEMENTAL FUNCTION Logarithm(Input, Base) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the logarithm of the input for a specified base.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat, INTENT(IN)  :: Input    !! input value
    tFloat, INTENT(IN)  :: Base     !! base
    tFloat              :: Output   !! logarithmic value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Input > Zero) THEN
        Output = LOG(Input)/LOG(Base)
    ELSE
        Output = IEEE_VALUE(Zero, IEEE_SIGNALING_NAN)
    END IF

    RETURN

END FUNCTION Logarithm

!******************************************************************************

FUNCTION RandomGen() RESULT(RNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate random number.

!** METHODOLOGY:
!   'Minimal' random number generator of Park and Miller combined with a Marsaglia shift
!   sequence. Returns a uniform random deviate between 0.0 and 1.0 (exclusive of the endpoint
!   values). This fully portable, scalar generator has the 'traditional' (not Fortran 90) calling
!   sequence with a random deviate as the returned function value: call with idum a negative
!   integer to initialize; thereafter, do not alter idum except to reinitialize. The period of this
!   generator is about 3.1**1018.

!** REFERENCE:
!   Press et.al. 1992.  Numerical Recipe.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat      :: RNum     !! generated random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,  PARAMETER :: IA = 16807
    tSInt32,  PARAMETER :: IM = 2147483647
    tSInt32,  PARAMETER :: IQ = 127773
    tSInt32,  PARAMETER :: IR = 2836

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32         :: int_time
    tSInt32,  SAVE  :: idum
    tSInt32,  SAVE  :: ix=-1,iy=-1,k
    tFloat,   SAVE  :: am
    tLogical, SAVE  :: FirstTime = TrueVal

! FLOW

    IF (FirstTime) THEN
        FirstTime = FalseVal
        CALL SYSTEM_CLOCK(Count=int_time)
        idum      = -int_time
    END IF

    IF (idum <= 0 .OR. iy < 0) THEN                    ! Initialize.
        am   = NEAREST(1.0,-1.0)/IM
        iy   = IOR(IEOR(888889999,ABS(idum)),1)
        ix   = IEOR(777755555,ABS(idum))
        idum = ABS(idum)+1                            ! Set idum positive.
    END IF
    ix = IEOR(ix,ISHFT(ix,13))                        ! Marsaglia shift sequence with period 2**32 - 1.
    ix = IEOR(ix,ISHFT(ix,-17))
    ix = IEOR(ix,ISHFT(ix,5))
    k  = iy/IQ                                        ! Park-Miller sequence by Schrage's method,
                                                    ! period 2**31 - 2.
    iy = IA*(iy-k*IQ)-IR*k
    IF (iy < 0) THEN
        iy=iy+IM
    END IF
    RNum = am*IOR(IAND(IM,IEOR(ix,iy)),1)            ! Combine the two generators with masking to
                                                    ! ensure nonzero value.
    RETURN

END FUNCTION RandomGen

!******************************************************************************

FUNCTION GCD_I32(A, B) RESULT(R)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the greatest common divisor of A and B.
    !  To return 0 if A < 0 or B < 0.  Also, return 0 if
    !  A == 0 and B == 0.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: A, B
    tSInt32                 :: R

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,  PARAMETER :: BitSize = 32

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! set default and check input
    R = 0
    IF (A < 0) RETURN
    IF (B < 0) RETURN
    IF (A == 0) THEN
        ! MOD(0, B) == 0, so B divides A, but the converse doesn't hold.
        R = B
        RETURN
    ELSEIF (B == 0) THEN
        ! similar logic as above.
        R = A
        RETURN
    END IF

    ! use the binary GCD algorithm
    BLOCK
        tSInt32     :: ATZ, BTZ, MinTZ
        tSInt64     :: AA, BB, Delta, MinD
        ATZ = TRAILZ(A)
        BTZ = TRAILZ(B)
        MinTZ = MIN(ATZ, BTZ)
        ! divide out all 2s
        AA = SHIFTR(A, ATZ)
        BB = SHIFTR(B, BTZ)
        DO WHILE (AA /= BB)
            ! both AA and BB are odd
            Delta = AA - BB
            ! MinD = MIN(Delta, 0)
            MinD = IAND(Delta, SHIFTA(Delta, BitSize-1))
            ! set AA to ABS(AA - BB)
            AA = Delta - MinD - MinD    ! AA is now nonnegative and even
            ! set BB to MIN(old AA, B)
            BB = BB + MinD
            ! divide out all 2s
            ATZ = TRAILZ(AA)
            AA = SHIFTA(AA, ATZ)
        END DO
        R = SHIFTL(AA, MinTZ)
    END BLOCK

    RETURN

END FUNCTION GCD_I32

!******************************************************************************

FUNCTION GCD_I64(A, B) RESULT(R)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the greatest common divisor of A and B.
    !  To return 0 if A < 0 or B < 0.  Also, return 0 if
    !  A == 0 and B == 0.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: A, B
    tSInt64             :: R

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,  PARAMETER :: BitSize = 64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! set default and check input
    R = 0_kInt64
    IF (A < 0_kInt64) RETURN
    IF (B < 0_kInt64) RETURN
    IF (A == 0_kInt64) THEN
        ! MOD(0, B) == 0, so B divides A, but the converse doesn't hold.
        R = B
        RETURN
    ELSEIF (B == 0_kInt64) THEN
        ! similar logic as above.
        R = A
        RETURN
    END IF

    ! use the binary GCD algorithm
    BLOCK
        tSInt32     :: ATZ, BTZ, MinTZ
        tSInt64     :: AA, BB, Delta, MinD
        ATZ = TRAILZ(A)
        BTZ = TRAILZ(B)
        MinTZ = MIN(ATZ, BTZ)
        ! divide out all 2s
        AA = SHIFTR(A, ATZ)
        BB = SHIFTR(B, BTZ)
        DO WHILE (AA /= BB)
            ! both AA and BB are odd
            Delta = AA - BB
            ! MinD = MIN(Delta, 0)
            MinD = IAND(Delta, SHIFTA(Delta, BitSize-1))
            ! set AA to ABS(AA - BB)
            AA = Delta - MinD - MinD    ! AA is now nonnegative and even
            ! set BB to MIN(old AA, B)
            BB = BB + MinD
            ! divide out all 2s
            ATZ = TRAILZ(AA)
            AA = SHIFTA(AA, ATZ)
        END DO
        R = SHIFTL(AA, MinTZ)
    END BLOCK

    RETURN

END FUNCTION GCD_I64

!******************************************************************************

FUNCTION NextPrime_I32(N) RESULT(P)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the smallest prime greater than or equal to the specified number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: N    ! the specified number
    tSInt32                 :: P    ! the smallest prime number greater than N

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    P = ToInt32(NextPrime(ToInt64(N)))

    RETURN

END FUNCTION NextPrime_I32

!******************************************************************************

FUNCTION NextPrime_I64(N) RESULT(P)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the smallest prime greater than or equal to the specified number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: N    ! the specified number
    tSInt64             :: P    ! the smallest prime number greater than N

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64       :: M

!** FLOW:

    IF (N <= 2_kInt64) THEN
        P = 2_kInt64
        RETURN
    END IF

    ! make the number to be the odd one
    M = IOR(N, 1_kInt64)

    IF (IsPrime(M)) THEN
        P = M
        RETURN
    END IF

    ! prepare entry in the +2, +4 loop: M should not be a multiple of 3
    BLOCK
        tSInt64   :: Rem
        Rem = MOD(M, 3_kInt64)
        IF (Rem == 0_kInt64) THEN
            M = M + 2_kInt64     ! Now: MOD(M, 3_kInt64) = 2_kInt64
        ELSEIF (Rem == 1_kInt64) THEN
            M = M + 4_kInt64     ! Now: MOD(M, 3_kInt64) = 2_kInt64
        END IF
        ! this loop skips all multiple of 3
        DO
            IF (IsPrime(M)) THEN
                P = M
                RETURN
            END IF
            M = M + 2_kInt64     ! Now: MOD(M, 3_kInt64) = 1_kInt64
            IF (IsPrime(M)) THEN
                P = M
                RETURN
            END IF
            M = M + 4_kInt64     ! Now: MOD(M, 3_kInt64) = 2_kInt64
        END DO
    END BLOCK

    RETURN

END FUNCTION NextPrime_I64

!******************************************************************************

FUNCTION IsPrime_I32(N) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified number is a prime number or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: N
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Flag = IsPrime_I64(ToInt64(N))

    RETURN

END FUNCTION IsPrime_I32

!******************************************************************************

FUNCTION IsPrime_I64(N) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified number is a prime number or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: N
    tLogical            :: Flag

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Encode all primes less than 66 into mask without 0 and 1
    tSInt64, PARAMETER  :: Mask = IOR(IOR(IOR(IOR(IOR(IOR(IOR(IOR(      &
        IOR(IOR(IOR(IOR(IOR(IOR(IOR(IOR(IOR(SHIFTL(1_kInt64,  2 - 2),    &
                                            SHIFTL(1_kInt64,  3 - 2)),   &
                                            SHIFTL(1_kInt64,  5 - 2)),   &
                                            SHIFTL(1_kInt64,  7 - 2)),   &
                                            SHIFTL(1_kInt64, 11 - 2)),   &
                                            SHIFTL(1_kInt64, 13 - 2)),   &
                                            SHIFTL(1_kInt64, 17 - 2)),   &
                                            SHIFTL(1_kInt64, 19 - 2)),   &
                                            SHIFTL(1_kInt64, 23 - 2)),   &
                                            SHIFTL(1_kInt64, 29 - 2)),   &
                                            SHIFTL(1_kInt64, 31 - 2)),   &
                                            SHIFTL(1_kInt64, 37 - 2)),   &
                                            SHIFTL(1_kInt64, 41 - 2)),   &
                                            SHIFTL(1_kInt64, 43 - 2)),   &
                                            SHIFTL(1_kInt64, 47 - 2)),   &
                                            SHIFTL(1_kInt64, 53 - 2)),   &
                                            SHIFTL(1_kInt64, 59 - 2)),   &
                                            SHIFTL(1_kInt64, 61 - 2))
    ! This bitmask is used as an optimization for cheaply testing for divisiblity by 2, 3, or 5.
    ! Each bit is set to 1 for all remainders that indicate divisibility by 2, 3, or 5, so
    ! 1, 7, 11, 13, 17, 19, 23, 29 are set to 0. 30 and up don't matter because they won't be hit.
    tSInt32,  PARAMETER  :: SIEVE_30 = NOT(IOR(IOR(IOR(IOR(IOR(IOR(IOR(SHIFTL(1, 1),  &
                                                                       SHIFTL(1, 7)), &
                                                                       SHIFTL(1, 11)), &
                                                                       SHIFTL(1, 13)), &
                                                                       SHIFTL(1, 17)), &
                                                                       SHIFTL(1, 19)), &
                                                                       SHIFTL(1, 23)), &
                                                                       SHIFTL(1, 29)))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! set default value
    Flag = FalseVal

    IF (N < 2_kInt64) RETURN

    IF (N < 66_kInt64) THEN
        ! Look up n within the mask.
        Flag = (IAND(SHIFTA(Mask, N-2), 1_kInt64) /= 0_kInt64)
        RETURN
    END IF

    IF (IAND(SIEVE_30, SHIFTL(1, MOD(N, 30_kInt64))) /= 0) RETURN

    IF ((MOD(N,  7_kInt64) == 0_kInt64).OR. &
        (MOD(N, 11_kInt64) == 0_kInt64).OR. &
        (MOD(N, 13_kInt64) == 0_kInt64)) RETURN

    IF (N < 17_kInt64 * 17_kInt64) THEN
        Flag = TrueVal
        RETURN
    END IF

    ! perform primolity test using the Miller-Rabin algorithm
    BLOCK
        tSInt32                     :: I, J
#ifdef  __INTEL_COMPILER
        TYPE(BaseSet(:)), POINTER   :: Base
        OutLoop: DO J = 1, 7
            Base => GetMillerRabinBaseSet(J)
            IF (N <= Base%Val(0)) THEN
                InLoop: DO I = 1, Base%Len-1
                    IF (.NOT.MillerRabinTest(Base%Val(I), N)) EXIT OutLoop
                END DO InLoop
                Flag = TrueVal
                EXIT OutLoop
            END IF
        END DO OutLoop
        NULLIFY(Base)
#else
        tSInt64, POINTER    :: Base(:)
        OutLoop: DO J = 1, 7
            Base => GetMillerRabinBaseSet(J)
            IF (N <= Base(0)) THEN
                InLoop: DO I = 1, SIZE(Base)-1
                    IF (.NOT.MillerRabinTest(Base(I), N)) EXIT OutLoop
                END DO InLoop
                Flag = TrueVal
                EXIT OutLoop
            END IF
        END DO OutLoop
        NULLIFY(Base)
#endif
    END BLOCK

    RETURN

END FUNCTION IsPrime_I64

!******************************************************************************

FUNCTION IsPrime_I64_Alternative(N) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified number is a prime number or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: N
    tLogical            :: Flag

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! The first 512 prime numbers
    tSInt32,  PARAMETER :: PRIMES(512) = [     &
        2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, &
        109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, &
        233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, &
        367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, &
        499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, &
        643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, &
        797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, &
        947, 953, 967, 971, 977, 983, 991, 997, 1009, 1013, 1019, 1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069, &
        1087, 1091, 1093, 1097, 1103, 1109, 1117, 1123, 1129, 1151, 1153, 1163, 1171, 1181, 1187, 1193, 1201, 1213, &
        1217, 1223, 1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283, 1289, 1291, 1297, 1301, 1303, 1307, 1319, 1321, &
        1327, 1361, 1367, 1373, 1381, 1399, 1409, 1423, 1427, 1429, 1433, 1439, 1447, 1451, 1453, 1459, 1471, 1481, &
        1483, 1487, 1489, 1493, 1499, 1511, 1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571, 1579, 1583, 1597, 1601, &
        1607, 1609, 1613, 1619, 1621, 1627, 1637, 1657, 1663, 1667, 1669, 1693, 1697, 1699, 1709, 1721, 1723, 1733, &
        1741, 1747, 1753, 1759, 1777, 1783, 1787, 1789, 1801, 1811, 1823, 1831, 1847, 1861, 1867, 1871, 1873, 1877, &
        1879, 1889, 1901, 1907, 1913, 1931, 1933, 1949, 1951, 1973, 1979, 1987, 1993, 1997, 1999, 2003, 2011, 2017, &
        2027, 2029, 2039, 2053, 2063, 2069, 2081, 2083, 2087, 2089, 2099, 2111, 2113, 2129, 2131, 2137, 2141, 2143, &
        2153, 2161, 2179, 2203, 2207, 2213, 2221, 2237, 2239, 2243, 2251, 2267, 2269, 2273, 2281, 2287, 2293, 2297, &
        2309, 2311, 2333, 2339, 2341, 2347, 2351, 2357, 2371, 2377, 2381, 2383, 2389, 2393, 2399, 2411, 2417, 2423, &
        2437, 2441, 2447, 2459, 2467, 2473, 2477, 2503, 2521, 2531, 2539, 2543, 2549, 2551, 2557, 2579, 2591, 2593, &
        2609, 2617, 2621, 2633, 2647, 2657, 2659, 2663, 2671, 2677, 2683, 2687, 2689, 2693, 2699, 2707, 2711, 2713, &
        2719, 2729, 2731, 2741, 2749, 2753, 2767, 2777, 2789, 2791, 2797, 2801, 2803, 2819, 2833, 2837, 2843, 2851, &
        2857, 2861, 2879, 2887, 2897, 2903, 2909, 2917, 2927, 2939, 2953, 2957, 2963, 2969, 2971, 2999, 3001, 3011, &
        3019, 3023, 3037, 3041, 3049, 3061, 3067, 3079, 3083, 3089, 3109, 3119, 3121, 3137, 3163, 3167, 3169, 3181, &
        3187, 3191, 3203, 3209, 3217, 3221, 3229, 3251, 3253, 3257, 3259, 3271, 3299, 3301, 3307, 3313, 3319, 3323, &
        3329, 3331, 3343, 3347, 3359, 3361, 3371, 3373, 3389, 3391, 3407, 3413, 3433, 3449, 3457, 3461, 3463, 3467, &
        3469, 3491, 3499, 3511, 3517, 3527, 3529, 3533, 3539, 3541, 3547, 3557, 3559, 3571, 3581, 3583, 3593, 3607, &
        3613, 3617, 3623, 3631, 3637, 3643, 3659, 3671]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! set default value
    Flag = FalseVal

    IF (N < 2_kInt64) RETURN

    ! check for small primes
    BLOCK
        tSInt32     :: I
        tSInt64     :: P
        DO I = 1, SIZE(PRIMES)
            P = ToInt64(PRIMES(I))
            IF (MOD(N, P) == 0_kInt64) THEN
                Flag = (N == P)
                RETURN
            END IF
        END DO
    END BLOCK

    ! perform primolity test using the Miller-Rabin algorithm
    BLOCK
        tSInt32                     :: I, J
#ifdef  __INTEL_COMPILER
        TYPE(BaseSet(:)), POINTER   :: Base
        OutLoop: DO J = 1, 7
            Base => GetMillerRabinBaseSet(J)
            IF (N <= Base%Val(0)) THEN
                InLoop: DO I = 1, Base%Len-1
                    IF (.NOT.MillerRabinTest(Base%Val(I), N)) EXIT OutLoop
                END DO InLoop
                Flag = TrueVal
                EXIT OutLoop
            END IF
        END DO OutLoop
        NULLIFY(Base)
#else
        tSInt64, POINTER    :: Base(:)
        OutLoop: DO J = 1, 7
            Base => GetMillerRabinBaseSet(J)
            IF (N <= Base(0)) THEN
                InLoop: DO I = 1, SIZE(Base)-1
                    IF (.NOT.MillerRabinTest(Base(I), N)) EXIT OutLoop
                END DO InLoop
                Flag = TrueVal
                EXIT OutLoop
            END IF
        END DO OutLoop
        NULLIFY(Base)
#endif
    END BLOCK

    RETURN

END FUNCTION IsPrime_I64_Alternative

!******************************************************************************

#ifdef  __INTEL_COMPILER
FUNCTION GetMillerRabinBaseSet(I) RESULT(BSPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a pointer to the specified base set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,          INTENT(IN)    :: I
    TYPE(BaseSet(:)), POINTER       :: BSPtr

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (I)
    CASE (1)
        CALL SetPointer(BSPtr, BaseSet1)
    CASE (2)
        CALL SetPointer(BSPtr, BaseSet2)
    CASE (3)
        CALL SetPointer(BSPtr, BaseSet3)
    CASE (4)
        CALL SetPointer(BSPtr, BaseSet4)
    CASE (5)
        CALL SetPointer(BSPtr, BaseSet5)
    CASE (6)
        CALL SetPointer(BSPtr, BaseSet6)
    CASE (7)
        CALL SetPointer(BSPtr, BaseSet7)
    END SELECT

    RETURN

CONTAINS

    SUBROUTINE SetPointer(BSPtr, BSTgt)
        TYPE(BaseSet(:)), POINTER   :: BSPtr
        TYPE(BaseSet(*)), TARGET    :: BSTgt
        BSPtr => BSTgt
        RETURN
    END SUBROUTINE

    !**************************************************************************

END FUNCTION GetMillerRabinBaseSet

!******************************************************************************
#else
FUNCTION GetMillerRabinBaseSet(I) RESULT(BSPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a pointer to the specified base set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: I
    tSInt64, POINTER    :: BSPtr(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT CASE (I)
    CASE (1)
        CALL SetPointer(BSPtr, BaseSet1)
    CASE (2)
        CALL SetPointer(BSPtr, BaseSet2)
    CASE (3)
        CALL SetPointer(BSPtr, BaseSet3)
    CASE (4)
        CALL SetPointer(BSPtr, BaseSet4)
    CASE (5)
        CALL SetPointer(BSPtr, BaseSet5)
    CASE (6)
        CALL SetPointer(BSPtr, BaseSet6)
    CASE (7)
        CALL SetPointer(BSPtr, BaseSet7)
    END SELECT

    RETURN

CONTAINS

    SUBROUTINE SetPointer(BSPtr, BSTgt)
        tSInt64, POINTER  :: BSPtr(:)
        tSInt64, TARGET   :: BSTgt(:)
        BSPtr => BSTgt
        RETURN
    END SUBROUTINE

    !**************************************************************************

END FUNCTION GetMillerRabinBaseSet

!******************************************************************************
#endif

FUNCTION MillerRabinTest(Base, N) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform primolity test using the Miller-Rabin algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Base, N
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (N <= FLOOR_SQRT_MAX_LONG) THEN
        Flag = PrimeTest(Base, N, MulMod_Small, SquareMod_Small)
    ELSE
        Flag = PrimeTest(Base, N, MulMod_Large, SquareMod_Large)
    END IF

    RETURN

CONTAINS

    FUNCTION MulMod_Small(A, B, M) RESULT(X)
        ! To return X = (A*B) Mod M
        tSInt64, INTENT(IN) :: A, B, M
        tSInt64             :: X
        X = MOD(A*B, M)
        RETURN
    END FUNCTION

    !**************************************************************************

    FUNCTION SquareMod_Small(A, M) RESULT(X)
        ! To return X = (A**2) Mod M
        tSInt64, INTENT(IN) :: A, M
        tSInt64             :: X
        X = MOD(A*A, M)
        RETURN
    END FUNCTION

    !**************************************************************************

    FUNCTION MulMod_Large(A, B, M) RESULT(X)
        ! To return X = (A*B) Mod M
        tSInt64, INTENT(IN) :: A, B, M
        tSInt64             :: X
        ! local variables
        tSInt64     :: AHi, BHi, ALo, BLo
        tSInt64     :: ResVal
        AHi = SHIFTR(A, 32)
        BHi = SHIFTR(B, 32)
        ALo = IAND(A, MaskU32)
        BLo = IAND(B, MaskU32)
        ! perform multiplication in modular arithmetic
        ResVal = Times2ToThe32Mod(AHi * BHi, M)
        ResVal = ResVal + AHi * BLo
        IF (ResVal < 0_kInt64) ResVal = UMOD(ResVal, M)
        ResVal = ResVal + ALo * BHi
        ResVal = Times2ToThe32Mod(ResVal, M)
        X = PlusMod(ResVal, UMOD(ALo * BLo, M), M)
        RETURN
    END FUNCTION

    !**************************************************************************

    FUNCTION SquareMod_Large(A, M) RESULT(X)
        ! To return X = (A**2) Mod M
        tSInt64, INTENT(IN) :: A, M
        tSInt64             :: X
        ! local variables
        tSInt64     :: AHi, ALo, HiLo
        tSInt64     :: ResVal
        AHi = SHIFTR(A, 32)
        ALo = IAND(A, MaskU32)
        ! perform multiplication in modular arithmetic
        ResVal = Times2ToThe32Mod(AHi * AHi, M)
        HiLo = AHi * ALo * 2_kInt64
        IF (HiLo < 0_kInt64) HiLo = UMOD(HiLo, M)
        ResVal = ResVal + HiLo
        ResVal = Times2ToThe32Mod(ResVal, M)
        X = PlusMod(ResVal, UMOD(ALo * ALo, M), M)
        RETURN
    END FUNCTION

    !**************************************************************************

    FUNCTION PlusMod(A, B, M) RESULT(X)
        ! To return X = (A+B) Mod M
        tSInt64, INTENT(IN) :: A, B, M
        tSInt64             :: X
        IF (A >= M-B) THEN
            X = A + B - M
        ELSE
            X = A + B
        END IF
        RETURN
    END FUNCTION

    !**************************************************************************

    FUNCTION Times2ToThe32Mod(A, M) RESULT(X)
        ! To return X = (A*(2**32)) Mod M
        tSInt64, INTENT(IN) :: A, M
        tSInt64             :: X
        ! local variables
        tSInt32             :: RemPow2, Shift
        RemPow2 = 32
        X = A
        DO
            Shift = MIN(RemPow2, LEADZ(X))
            ! shift is either the number of powers of 2 left to multiply X by,
            ! or the biggest shift possible while keeping X in an unsigned long
            X = UMOD(SHIFTL(X, Shift), M)
            RemPow2 = RemPow2 - Shift
            IF (RemPow2 <= 0) EXIT
        END DO
        RETURN
    END FUNCTION

    !**************************************************************************

    FUNCTION PowMod(A, P, M, MulMod, SquareMod) RESULT(X)
        ! To return X = (A**P) Mod M
        tSInt64, INTENT(IN) :: A, P, M
        PROCEDURE(Mul_Mod)  :: MulMod
        PROCEDURE(Sqr_Mod)  :: SquareMod
        tSInt64             :: X
        ! local variables
        tSInt64     :: AA, PP
        ! initialize
        X  = 1_kInt64
        AA = A
        PP = P
        DO WHILE (PP /= 0_kInt64)
            IF (IAND(P, 1_kInt64) /= 0_kInt64) X = MulMod(X, AA, M)
            AA = SquareMod(AA, M)
            PP = SHIFTA(PP, 1)
        END DO
        RETURN
    END FUNCTION

    !**************************************************************************

    FUNCTION PrimeTest(Base, N, MulMod, SquareMod) RESULT(Flag)
        ! To return true if N is a strong probable prime relative to the specified base.
        tSInt64, INTENT(IN) :: Base, N
        PROCEDURE(Mul_Mod)  :: MulMod
        PROCEDURE(Sqr_Mod)  :: SquareMod
        tLogical            :: Flag
        ! local variables
        tSInt64     :: D, B, A, NM1
        tSInt32     :: R, J
        ! initialize
        Flag = TrueVal
        R = TRAILZ(N - 1_kInt64)
        D = SHIFTA(N - 1_kInt64, R)
        B = MOD(Base, N)
        IF (B == 0_kInt64) RETURN
        ! Calculate A := B**D mod N
        A = PowMod(B, D, N, MulMod, SquareMod)
        ! N passes this test if B**D = 1 (mod N)
        ! or B**(2**J * D) = -1 (mod N) for some 0 <= J < R
        IF (A == 1_kInt64) RETURN
        J = 0
        NM1 = N - 1_kInt64
        DO WHILE (A /= NM1)
            J = J + 1
            IF (J == R) THEN
                Flag = FalseVal
                RETURN
            END IF
            A = SquareMod(A, N)
        END DO
        RETURN
    END FUNCTION

    !**************************************************************************

END FUNCTION MillerRabinTest

!******************************************************************************

END MODULE MBase_MathUtil

!******************************************************************************
