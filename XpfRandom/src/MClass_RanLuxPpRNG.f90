
MODULE MClass_RanLuxPpRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *RanLuxPpRNG* type and its related routines.
!   The *RanLuxPpRNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *RanLuxPpRNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the *RANLUX++* algorithm by
!   Alexei Sibidanov. <br>
!   The *RANLUX++* algorithm is equivalent to the *RANLUX* algorithm but 
!   employs an equivalent Linear Congruential generator (LCG) in place of
!   the Subtract-With-Borrow (SWB) generator.  Unlike the *RANLUX* algorithm,
!   the *RANLUX++* algorithm destroys correlations of generated numbers by fast
!   skipping of numbers instead of throwing away some of generated number.
!   Therefore, it is faster and can perform the decimation at a higher level
!   than the highest standard level of the *RANLUX* algorithm.  <br>
!   It is important to note that the *RanLux++* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://arxiv.org/abs/1705.03123">
!       A. Sibidanov.  2017.  A revision of the subtract-with-borrow random
!       number generators.  arXiv:1705.03123 [physics.comp-ph]. </a> <br>
!   [2] <a href="https://arxiv.org/abs/1903.01247">
!       F. James and L. Moneta.  2019.  Review of High-Quality Random Number
!       Generators.  arXiv:1903.01247 [physics.comp-ph]. </a> <br>
!   [3] <a href="https://github.com/sibidanov/ranluxpp">
!       RANLUX++: The original implementation of Ranlux++ RNG. </a> <br>
!   [4] <a href="https://github.com/jirka-h/ranluxpp-portable">
!       RANLUX++: The portable version of Ranlux++ RNG. </a>

!** USE STATEMENTS:
    USE ISO_C_BINDING,      ONLY: C_LOC, C_F_POINTER, C_PTR, C_NULL_PTR
    USE MBase_Common
    USE MBase_SIntUtil,   ONLY: MIN_I64, MAX_I64
    USE MBase_UIntUtil,   ONLY: MAX_U64, ToUnsignedLong, UMulBasic
    USE MClass_BaseRNG
    USE MClass_IntegerRNG,   ONLY: MaskL
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: RanLuxPpRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! Size of the seed required
    tIndex,   PARAMETER :: STATE_SIZE = 9_kIndex
    tIndex,   PARAMETER :: CACHE_SIZE = 11_kIndex
    ! A = M - (M-1)/B = 2^576 - 2^552 - 2^240 + 2^216 + 1
    tUInt64,  PARAMETER :: InitA(STATE_SIZE) = [ &
        ToInt64(Z'0000000000000001'), ToInt64(Z'0000000000000000'), &
        ToInt64(Z'0000000000000000'), ToInt64(Z'FFFF000001000000'), &
        ToInt64(Z'FFFFFFFFFFFFFFFF'), ToInt64(Z'FFFFFFFFFFFFFFFF'), &
        ToInt64(Z'FFFFFFFFFFFFFFFF'), ToInt64(Z'FFFFFFFFFFFFFFFF'), &
        ToInt64(Z'FFFFFEFFFFFFFFFF')]

!** DERIVED TYPE DEFINITIONS
    !> The *RanLuxPpRNG* type is a *Long* PRNG type based on the *RANLUX++* algorithm
    !  by A. Sibidanov.
    TYPE, EXTENDS(LongRNG)  :: RanLuxPpRNG
        PRIVATE
        !% multiplier
        tUInt64     :: A(STATE_SIZE) = InitA
        !% current state
        tUInt64     :: X(STATE_SIZE) = 0_kInt64
        !% cached double-precision random numbers
        tRealDP     :: D(CACHE_SIZE) = 0.0_kDouble
        !% skipping parameter
        tUInt64     :: P = 2048_kInt64
        !% index into the cached output
        tIndex      :: Index = CACHE_SIZE + 1_kIndex
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: InitWithSkipping => RanLuxPpRNG_InitWithSkipping
        PROCEDURE, PRIVATE  :: Update           => RanLuxPpRNG_Update
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => RanLuxPpRNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl     => RanLuxPpRNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => RanLuxPpRNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => RanLuxPpRNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----               Generic Interface                           -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Initialize <br>
        !  **Purpose**:  To initialize the pseudo-random number generator. <br>
        !  **Usage**: <br>
        !   ! initialize without seed; use default skipping parameter (= 2048) <br>
        !   --->    CALL RNG%Initialize() <br>
        !   ! initialize with seed(s); default skipping parameter (= 2048) <br>
        !   --->    CALL RNG%Initialize(Seeds) <br>
        !   ! initialize without seed; use specified skipping parameter (= 389) <br>
        !   --->    CALL RNG%Initialize(389) <br>
        !   ! initialize with seed(s); use specified luxury level (= 1024) <br>
        !   --->    CALL RNG%Initialize(1024, Seeds) <br>
        !  **Usage**: If specified, the skipping parameter should be between 24 and 4096.
        GENERIC         :: Initialize           => InitWithSkipping
        ! ---------------------------------------------------------------------
        ! -----                 Overridden Procedures                     -----
        ! ---------------------------------------------------------------------
        !> *Default_NextDouble* is an overridden procedure. <br>
        !  Use the *NextDouble* method in place of the *Default_NextDouble*
        !  method to generate a 64-bit real number.
        PROCEDURE       :: Default_NextDouble   => RanLuxPpRNG_NextDouble
        ! ---------------------------------------------------------------------
    END TYPE RanLuxPpRNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE RanLuxPpRNG_InitWithSkipping(RNG, Skip, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified skipping (p) parameter.
    !  Optionally, initial Seeds can be specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'RanLuxPpRNG' object
    CLASS(RanLuxPpRNG), INTENT(INOUT)   :: RNG
    !> skipping (p) parameter with valid range of [24, 4096]. <br>
    !  If value is out of the valid range, use default value (2048).
    tSInt32,            INTENT(IN)      :: Skip
    !% optional 64-bit integer seed(s)
    tSInt64, OPTIONAL,  INTENT(IN)      :: Seed(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check and set skipping (p) parameter
    IF (IN_RANGE(Skip, 24, 4096)) THEN
        RNG%P = ToInt64(Skip)
    ELSE
        RNG%P = 2048_kInt64
    END IF

    IF (PRESENT(Seed)) THEN
        ! initialize the generator with seed(s)
        CALL RNG%Initialize(Seed)
    ELSE
        ! initialize the generator without seed
        CALL RNG%Initialize()
    END IF

    RETURN

END SUBROUTINE RanLuxPpRNG_InitWithSkipping

!******************************************************************************

SUBROUTINE RanLuxPpRNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'RanLuxPpRNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RanLuxPpRNG), INTENT(INOUT)   :: RNG      !! 'RanLuxPpRNG' object
    tSInt64,            INTENT(IN)      :: Seed(:)  !! seed(s)

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt64, PARAMETER  :: Two48 = SHIFTL(1_kInt64, 48)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Seed0
    tUInt64     :: A(STATE_SIZE)

! FLOW

    ! set initial seed
    Seed0 = Seed(1)
    
    ! set state and multiplier
    RNG%X(1)  = 1_kInt64
    RNG%X(2:) = 0_kInt64
    RNG%A     = InitA
    
    ! set A = MOD(InitA**P, M)
    CALL PowMod(RNG%A, RNG%P, RNG%A)
    
    ! initialize with the initial seed
    CALL PowMod(RNG%A, Two48, A)
    CALL PowMod(A, Two48, A)    ! skip 2**96 states
    CALL PowMod(A, Seed0, A)    ! skip (2**96)*Seed0 states
    CALL MulMod(A, RNG%X)
    
    ! set index
    RNG%Index = CACHE_SIZE + 1_kIndex

    RETURN

END SUBROUTINE RanLuxPpRNG_BaseInit

!******************************************************************************

FUNCTION RanLuxPpRNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RanLuxPpRNG), INTENT(INOUT)   :: RNG      !! 'RanLuxPpRNG' object
    tSInt64                             :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tRealDP, PARAMETER  :: Diff = ToRealDP(MAX_I64) - ToRealDP(MIN_I64)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandNum = ToInt64(RNG%NextDouble()*Diff) + MIN_I64
    
    RETURN

END FUNCTION RanLuxPpRNG_NextLong

!******************************************************************************

FUNCTION RanLuxPpRNG_NextDouble(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random real value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RanLuxPpRNG), INTENT(INOUT)   :: RNG      !! 'RanLuxPpRNG' object
    tRealDP                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! update current state if necessary
    IF (RNG%Index > CACHE_SIZE) CALL RNG%Update()
    
    ! get random number
    RandNum = RNG%D(RNG%Index)

    ! update index
    RNG%Index = RNG%Index + 1_kIndex

    RETURN

END FUNCTION RanLuxPpRNG_NextDouble

!******************************************************************************

SUBROUTINE RanLuxPpRNG_Update(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To update the current state as well as the cached values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RanLuxPpRNG), INTENT(INOUT)   :: RNG  !! 'RanLuxPpRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! compute next state: the core of LCG -- modular multiplication
    CALL MulMod(RNG%A, RNG%X)
    
    ! unpack state into cached values
    CALL UnpackState(RNG%X, RNG%D)
    
    ! reset index
    RNG%Index = 1_kIndex

    RETURN
    
CONTAINS

    SUBROUTINE UnpackState(X, D)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To unpack 64-bit integer state into double-precision real cached values.
        !  52 bits out of possible 53 bits are random.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64,         INTENT(IN)     :: X(0:)    ! state (9 elements)
        tRealDP, TARGET, INTENT(OUT)    :: D(0:)    ! cached values (11 elements)

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER  :: E = ToInt64(Z'3FF0000000000000') ! exponent
        tUInt64, PARAMETER  :: M = ToInt64(Z'000FFFFFFFFFFFFF') ! mantissa

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64, POINTER    :: ID(:)    ! Fortran pointer to cached values
        TYPE(C_PTR)         :: CPtr     ! C pointer to cached values

    !** FLOW
    
        ! get a C pointer to data content
        CPtr = C_LOC(D)
    
        ! associate a Fortran data pointer with the C pointer
        CALL C_F_POINTER(cPtr, ID, [CACHE_SIZE])

        ! unpack state into cached values
        ID( 0) = IOR(E,  IAND(M, X(0)))
        ID( 1) = IOR(E,  IAND(M, IOR(SHIFTR(X(0), 52), SHIFTL(X(1), 12))))
        ID( 2) = IOR(E,  IAND(M, IOR(SHIFTR(X(1), 40), SHIFTL(X(2), 24))))
        ID( 3) = IOR(E,  IAND(M, IOR(SHIFTR(X(2), 28), SHIFTL(X(3), 36))))
        ID( 4) = IOR(E,  IAND(M, IOR(SHIFTR(X(3), 16), SHIFTL(X(4), 48))))
        ID( 5) = IOR(E,  IAND(M, IOR(SHIFTR(X(4),  4), SHIFTL(X(5), 60))))
        ID( 6) = IOR(E,  IAND(M, IOR(SHIFTR(X(4), 56), SHIFTL(X(5),  8))))
        ID( 7) = IOR(E,  IAND(M, IOR(SHIFTR(X(5), 44), SHIFTL(X(6), 20))))
        ID( 8) = IOR(E,  IAND(M, IOR(SHIFTR(X(6), 32), SHIFTL(X(7), 32))))
        ID( 9) = IOR(E,  IAND(M, IOR(SHIFTR(X(7), 20), SHIFTL(X(8), 44))))
        ID(10) = IOR(E,  IAND(M, SHIFTR(X(8), 8)))

        ! subtract D by 1
        D = D - 1.0_kDouble

        ! nullify pointers
        NULLIFY(ID)
        cPtr = C_NULL_PTR

        RETURN

    END SUBROUTINE UnpackState

    !******************************************************************************

END SUBROUTINE RanLuxPpRNG_Update

!******************************************************************************

FUNCTION RanLuxPpRNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RanLuxPpRNG), INTENT(IN)  :: RNG      !! 'RanLuxPpRNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'RanLuxPpRNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION RanLuxPpRNG_GetName

!******************************************************************************

FUNCTION RanLuxPpRNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RanLuxPpRNG), INTENT(IN)  :: RNG      !! 'RanLuxPpRNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 1
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION RanLuxPpRNG_GetSeedSize

!******************************************************************************

FUNCTION AddOverflow(A, B, Overflow) RESULT(C)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute C = A + B and set Overflow accordingly.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: A, B
    tUInt64, INTENT(OUT)    :: Overflow
    tUInt64                 :: C

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    C = A + B
    IF (C < A) THEN
        Overflow = 1_kInt64
    ELSE
        Overflow = 0_kInt64
    END IF
    
    RETURN

END FUNCTION AddOverflow

!******************************************************************************

FUNCTION AddCarry(A, B, Carry) RESULT(C)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute C = A + B and increment Carry if there was an overflow.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: A, B
    tUInt64, INTENT(INOUT)  :: Carry
    tUInt64                 :: C

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Overflow

! FLOW
    
    C = AddOverflow(A, B, Overflow)
    Carry = Carry + Overflow
    
    RETURN

END FUNCTION AddCarry

!******************************************************************************

FUNCTION SubOverflow(A, B, Overflow) RESULT(C)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute C = A - B and set Overflow accordingly.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: A, B
    tUInt64, INTENT(OUT)    :: Overflow
    tUInt64                 :: C

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    C = A + B
    IF (C > A) THEN
        Overflow = 1_kInt64
    ELSE
        Overflow = 0_kInt64
    END IF
    
    RETURN

END FUNCTION SubOverflow

!******************************************************************************

FUNCTION SubCarry(A, B, Carry) RESULT(C)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute C = A + B and increment Carry if there was an overflow.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: A, B
    tUInt64, INTENT(INOUT)  :: Carry
    tUInt64                 :: C

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Overflow

! FLOW
    
    C = SubOverflow(A, B, Overflow)
    Carry = Carry + Overflow
    
    RETURN

END FUNCTION SubCarry

!******************************************************************************

FUNCTION ComputeR(Upper, R) RESULT(C)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To update the remainder: r = r - (t1 + t2) + (t3 + t2) * b ** 10.
    !  This function also yields cbar = floor(r / m) as its return value.
    !  With an initial value of r = t0, this can be used for computing
    !  the remainder after division by m (see the function ModM).
    !  The function to_ranlux passes r = 0 and uses only the return value
    !  to obtain the decimal expansion after division by m.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: Upper(0:)
    tUInt64, INTENT(INOUT)  :: R(0:)
    tSInt64                 :: C
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt64, PARAMETER  :: Mask = ToInt64(Z'0000FFFFFFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: RI, T1, T2, T3
    tUInt64     :: Carry, Overflow
    tIndex      :: I
    tLogical    :: GreaterM

! FLOW
    
    ! Subtract t1 (24 * 24 = 576 bits)
    Carry = 0_kInt64
    DO I = 0, 8
        RI = SubOverflow(R(I), Carry, Overflow)
        T1 = Upper(I)
        R(I) = SubCarry(RI, T1, Overflow)
        Carry = Overflow
    END DO
    C = -Carry

    ! Subtract t2 (only 240 bits, so need to extend)
    Carry = 0_kInt64
    DO I = 0, 8
        RI = SubOverflow(R(I), Carry, Overflow)
        T2 = 0_kInt64
        IF (I < 4) THEN
            T2 = T2 + SHIFTR(Upper(I+5), 16)
            IF (I < 3) THEN
                T2 = T2 + SHIFTR(Upper(I+6), 48)
            END IF
        END IF
        R(I) = SubCarry(RI, T2, Overflow)
        Carry = Overflow
    END DO
    C = C - Carry
    
    ! r = r + (t3 + t2) * 2 ** 240
    Carry = 0_kInt64
    T2 = SHIFTL(SHIFTR(Upper(5), 16), 48)
    T3 = SHIFTL(Upper(0), 48)
    RI   = AddCarry(R(3), T2, Carry)
    R(3) = AddCarry(RI,   T3, Carry)
    DO I = 0, 2
        RI = AddOverflow(R(I+4), Carry, Overflow)
        T2 = SHIFTR(Upper(I+5), 32) + SHIFTL(Upper(I+6), 32)
        T3 = SHIFTR(Upper(I), 16)   + SHIFTL(Upper(I+1), 48)
        R(I+4) = RI
        RI     = AddCarry(R(I+4), T2, Overflow)
        R(I+4) = AddCarry(RI,     T3, Overflow)
        Carry = Overflow
    END DO
    RI = AddOverflow(R(7), Carry, Overflow)
    T2 = SHIFTR(Upper(8), 32)
    T3 = SHIFTR(Upper(3), 16) + SHIFTL(Upper(4), 48)
    R(7) = RI
    RI   = AddCarry(R(7), T2, Overflow)
    R(7) = AddCarry(RI,   T3, Overflow)
    Carry = Overflow
    RI = AddOverflow(R(8), Carry, Overflow)
    T3 = SHIFTR(Upper(4), 16) + SHIFTL(Upper(5), 48)
    R(8) = AddCarry(RI, T3, Overflow)
    Carry = Overflow
    C = C + Carry

    ! c = floor(r / 2 ** 576) has been computed along the way via the carry
    ! flags. Now if c = 0 and the value currently stored in r is greater or
    ! equal to m, we need cbar = 1 and subtract m, otherwise cbar = c. The
    ! value currently in r is greater or equal to m, if and only if one of
    ! the last 240 bits is set and the upper bits are all set.
    GreaterM = (IOR(IOR(IOR(R(0), R(1)), R(2)), IAND(R(3), Mask)) > 0_kInt64)
    GreaterM = (GreaterM .AND. (SHIFTR(R(3), 48) == ToInt64(Z'000000000000FFFF')))
    DO I = 4, 8
        GreaterM = (GreaterM .AND. (R(I) == MAX_U64))
    END DO
    IF ((C == 0_kInt64).AND.GreaterM) C = C + 1_kInt64

    RETURN

END FUNCTION ComputeR

!******************************************************************************

SUBROUTINE ModM(Mul, Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute a value congruent to Mul modulo M less than 2 ** 576
    !  where M = 2**256 - 2**240 + 1.  The result is guaranteed to be
    !  smaller than the modulus.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: Mul(0:)  !! multiplication product of 18 elements
    tUInt64, INTENT(INOUT)  :: Res(0:)  !! result with 9 elements

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: R(0:8)
    tUInt64     :: RI, Carry, Overflow
    tSInt64     :: C, T0, T1, T2
    tIndex      :: I

! FLOW
    
    ! assign r = t0
    R(0:8) = Mul(0:8)
    
    ! compute remainder
    C = ComputeR(Mul(9:), R)
    
    ! To update r = r - c * m, it suffices to know c * (-2 ** 240 + 1)
    ! because the 2 ** 576 will cancel out.  Also note that c may be zero, but
    ! the operation is still performed to avoid branching.

    ! c * (-2 ** 240 + 1) in 576 bits looks as follows, depending on c:
    !  - if c = 0, the number is zero.
    !  - if c = 1: bits 576 to 240 are set,
    !              bits 239 to 1 are zero, and
    !              the last one is set
    !  - if c = -1, which corresponds to all bits set (signed int64_t):
    !              bits 576 to 240 are zero and the rest is set.
    ! Note that all bits except the last are exactly complimentary (unless c = 0)
    ! and the last byte is conveniently represented by c already.
    ! Now construct the three bit patterns from c.
    ! ---------------------------------------------------------------
    ! Note:  C, T0, T1 and T2 are all treated as signed integers in
    !        the following three statements but after that all Ts
    !        are treated as unsigned integers.
    ! ---------------------------------------------------------------
    T0 = SHIFTA(C, 1)
    T2 = T0 - SHIFTL(C, 48)
    T1 = SHIFTA(T2, 48)
    ! alternative implementation of the above three statements
    !IF (C = 0_kInt64) THEN
    !    T0 = 0_kInt64
    !    T1 = 0_kInt64
    !    T2 = 0_kInt64
    !ELSEIF (C = 1_kInt64) THEN
    !    T0 = 0_kInt64
    !    T1 = ToInt64(Z'FFFFFFFFFFFFFFFF')
    !    T2 = ToInt64(Z'FFFF000000000000')
    !ELSE
    !    T0 = ToInt64(Z'FFFFFFFFFFFFFFFF')
    !    T1 = 0_kInt64
    !    T2 = ToInt64(Z'0000FFFFFFFFFFFF')
    !END IF

    Carry = 0_kInt64
    Res(0) = SubCarry(R(0), C, Carry)
    DO I = 1, 2
        RI = SubOverflow(R(I), Carry, Overflow)
        Res(I) = SubCarry(RI, T0, Overflow)
        Carry = Overflow
    END DO
    RI = SubOverflow(R(3), Carry, Overflow)
    Res(3) = SubCarry(RI, T2, Overflow)
    Carry = Overflow
    DO I = 4, 8
        RI = SubOverflow(R(I), Carry, Overflow)
        Res(I) = SubCarry(RI, T1, Overflow)
        Carry = Overflow
    END DO

    RETURN

END SUBROUTINE ModM

!******************************************************************************

SUBROUTINE MulMod(Inp, InpOut)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication and then compute the product modulo M.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% first factor of multiplication with 9 elements
    tUInt64, INTENT(IN)     :: Inp(:)
    !> on entry, second factor with 9 elements <br>
    !  on exit, the result guaranteed to be smaller than the modulus
    tUInt64, INTENT(INOUT)  :: InpOut(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Mul(2*STATE_SIZE)  ! product of multiplication

! FLOW
    
    ! initialize to zero
    Mul = 0_kInt64
    
    ! perform multiplication
    CALL UMulBasic(Inp, STATE_SIZE, InpOut, STATE_SIZE, Mul)
    
    ! compute the product modulo M
    CALL ModM(Mul, InpOut)

    RETURN

END SUBROUTINE MulMod

!******************************************************************************

SUBROUTINE PowMod(Base, N, Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute Base to the N modulo M.
    !  (To perform modular exponentiation: Res = Base**N mod M where
    !   M = 2**576 - 2**240 + 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% the base with 9 elements
    tUInt64, INTENT(IN)     :: Base(:)
    !% the exponent
    tUInt64, INTENT(IN)     :: N
    !% the output with 9 elements
    tUInt64, INTENT(INOUT)  :: Res(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Factor(STATE_SIZE)   ! working array
    tUInt64     :: Mul(2*STATE_SIZE)    ! working array
    tUInt64     :: Exp

! FLOW
    
    ! initialize
    Factor(1:STATE_SIZE) = Base(1:STATE_SIZE)
    Res(1) = 1_kInt64
    Res(2:) = 0_kInt64
    Mul = 0_kInt64
    Exp = N
    
    ! perform modular exponentiation
    DO WHILE (Exp /= 0_kInt64)
        IF (IAND(Exp, 1_kInt64) /= 0_kInt64) THEN
            CALL UMulBasic(Res, STATE_SIZE, Factor, STATE_SIZE, Mul)
            CALL ModM(Mul, Res)
        END IF
        Exp = SHIFTR(Exp, 1)
        IF (Exp == 0_kInt64) EXIT
        CALL UMulBasic(Factor, STATE_SIZE, Factor, STATE_SIZE, Mul)
        CALL ModM(Mul, Factor)
    END DO

    RETURN

END SUBROUTINE PowMod

!******************************************************************************

END MODULE MClass_RanLuxPpRNG
    
!******************************************************************************
