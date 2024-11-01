
MODULE MClass_Mt64RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Mt64RNG* type and its related routines.
!   The *Mt64RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *Mt64RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the 64-bit Mersenne Twister
!   algorithm by Makoto Matsumoto and Takuji Nishimura.  The *Mt64* PRNG
!   features an extremely long period (2<sup>19937</sup> - 1) and
!   311-dimensional equidistribution up to 64 bits accuracy. <br>
!   It is important to note that the *Mt64* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://dl.acm.org/doi/10.1145/369534.369540">
!       T. Nishimura.  2000.  Tables of 64-bit Mersenne twisters.  ACM Transactions on
!       Modeling and Computer Simulation, Vol. 10, No. 4, October 2000, pp 348-357.</a> <br>
!   [2] <a href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/ARTICLES/mt.pdf">
!       M. Matsumoto and T. Nishimura.  1998.  Mersenne Twister: A 623-Dimensionally
!       Equidistributed Uniform Pseudo-Random Number Generator.  ACM Transactions on
!       Modeling and Computer Simulation, Vol. 8, No. 1, January 1998, pp 3-30.</a> <br>
!   [3] <a href="http://www.math.sci.hiroshima-u.ac.jp/m-mat/MT/emt64.html">
!       Mersenne Twister 64bit version.</a> <br>
!   [4] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/MersenneTwister64.html">
!       Apache Commons RNG: Class MersenneTwister64</a>

!** PURPOSE OF THIS MODULE:
    ! This module contains a random number generator class based on the Mersenne Twister
    ! algorithm by Makoto Matsumoto and Takuji Nishimura.

!** REFERENCES:
    ! [1] M. Matsumoto and T. Nishimura.  1998.  Mersenne Twister: A 623-Dimensionally
    !    Equidistributed Uniform Pseudo-Random Number Generator.  ACM Transactions on
    !    Modeling and Computer Simulation, Vol. 8, No. 1, January 1998, pp 3--30
    ! [2] http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt64.html

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Mt64RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! Most significant 33 bits.
    tSInt64,  PARAMETER :: UM = ToInt64(Z'FFFFFFFF80000000')
    ! Least significant 31 bits.
    tSInt64,  PARAMETER :: LM = ToInt64(Z'000000007FFFFFFF')
    ! Size of the bytes pool.
    tSInt32,  PARAMETER :: NN = 312
    ! Period second parameter.
    tSInt32,  PARAMETER :: MM = 156
    ! X * MATRIX_A for X = {0, 1}.
    tSInt64,  PARAMETER :: MAG01(0:1) = [0_kInt64, ToInt64(Z'B5026F5AA96619E9')]

!** DERIVED TYPE DEFINITIONS
    !> The *Mt64RNG* type is a *Long* PRNG type based on the Mersenne
    !  Twister algorithm by Makoto Matsumoto and Takuji Nishimura.
    TYPE, EXTENDS(LongRNG)  :: Mt64RNG
        PRIVATE
        ! Bytes pool
        tSInt64     :: MT(0:NN-1)
        ! Current index in the bytes pool
        tSInt32     :: MTI
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => Mt64RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl     => Mt64RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => Mt64RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => Mt64RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE Mt64RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Mt64RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Mt64RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt64RNG), INTENT(INOUT)   :: RNG      !! 'Mt64RNG' object
    tSInt64,        INTENT(IN)      :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set initial states
    IF (SIZE(Seed) < NN) THEN
        CALL FillStateVariable(Seed, RNG%MT)
    ELSE
        RNG%MT(0:NN-1) = Seed(1:NN)
    END IF

    ! Initial index
    RNG%MTI = NN

    RETURN
    
    CONTAINS

    SUBROUTINE FillStateVariable(Seed, State)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To fill State based on the given seed.

        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt64, INTENT(IN)     :: Seed(0:)
        tSInt64, INTENT(OUT)    :: State(0:)

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt64, PARAMETER  ::C1 = ToInt64(Z'369DEA0F31A53F85')
        tSInt64, PARAMETER  ::C2 = ToInt64(Z'27BB2EE687B0B0FD')
        tSInt64, PARAMETER  ::C3 = ToInt64(Z'8000000000000000')
        
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: I, J, K, SeedSize
        tSInt64     :: MM1

    ! FLOW
    
        CALL Initialize_State(19650218_kInt64, State)

        I = 1
        J = 0
        SeedSize = SIZE(Seed)

        DO K = MAX(NN, SeedSize), 1, -1
            MM1 = State(I - 1)
            ! non linear
            State(I) = IEOR(State(I), (IEOR(MM1, SHIFTR(MM1, 62)) * C1)) + Seed(J) + J
            I = I + 1
            J = J + 1
            IF (I >= NN) THEN
                State(0) = State(NN - 1)
                I = 1
            END IF
            IF (J >= SeedSize) THEN
                J = 0
            END IF
        END DO
        DO K = NN-1, 1, -1
            MM1 = State(I - 1)
            ! non linear
            State(I) = IEOR(State(I), (IEOR(MM1, SHIFTR(MM1, 62)) * C2)) - I
            I = I + 1
            IF (I >= NN) THEN
                State(0) = State(NN - 1)
                I = 1
            END IF
        END DO

        ! MSB is 1 assuring non-zero initial array
        State(0) = C3

        RETURN

    END SUBROUTINE FillStateVariable

    !******************************************************************************

    SUBROUTINE Initialize_State(Seed, State)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To fill the State using a defined pseudo-random sequence.

        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt64, INTENT(IN)     :: Seed
        tSInt64, INTENT(OUT)    :: State(0:)

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt64, PARAMETER  ::CPAR = ToInt64(Z'5851F42D4C957F2D')
        
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt64     :: MM1
        tSInt32     :: MTI

    ! FLOW
    
        State(0) = Seed
        DO MTI = 1, NN-1
            MM1 = State(MTI - 1)
            State(MTI) = CPAR * IEOR(MM1, SHIFTR(MM1, 62)) + MTI
        END DO

        RETURN

    END SUBROUTINE Initialize_State

    !******************************************************************************

END SUBROUTINE Mt64RNG_BaseInit

!******************************************************************************

FUNCTION Mt64RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt64RNG), INTENT(INOUT)   :: RNG      !! 'Mt64RNG' object
    tSInt64                         :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER  ::C1 = ToInt64(Z'5555555555555555')
    tSInt64, PARAMETER  ::C2 = ToInt64(Z'71d67fffeda60000')
    tSInt64, PARAMETER  ::C3 = ToInt64(Z'fff7eee000000000')
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: X
    tSInt32     :: I

! FLOW

    IF (RNG%MTI >= NN) THEN
        
        ! Generate N words at one time
        DO I = 0, NN - MM -1
            X = IOR(IAND(RNG%MT(I), UM), IAND(RNG%MT(I + 1), LM))
            RNG%MT(I) = IEOR(IEOR(RNG%MT(I + MM), SHIFTR(X, 1)), &
                             MAG01(ToInt32(IAND(X, 1_kInt64))))
        END DO
        DO I = NN - MM, NN - 2
            X = IOR(IAND(RNG%MT(I), UM), IAND(RNG%MT(I + 1), LM))
            RNG%MT(I) = IEOR(IEOR(RNG%MT( I + (MM - NN)), SHIFTR(X, 1)), &
                             MAG01(ToInt32(IAND(X, 1_kInt64))))
        END DO

        X = IOR(IAND(RNG%MT(NN - 1), UM), IAND(RNG%MT(0), LM))
        RNG%MT(NN - 1) = IEOR(IEOR(RNG%MT(MM - 1), SHIFTR(X, 1)), &
                              MAG01(ToInt32(IAND(X, 1_kInt64))))

        RNG%MTI = 0
    END IF

    X = RNG%MT(RNG%MTI)
    RNG%MTI = RNG%MTI + 1

    ! Tempering
    X = IEOR(X, IAND(SHIFTR(X, 29), C1))
    X = IEOR(X, IAND(SHIFTL(X, 17), C2))
    X = IEOR(X, IAND(SHIFTL(X, 37), C3))
    X = IEOR(X, SHIFTR(X, 43))

    RandNum = X

    RETURN

END FUNCTION Mt64RNG_NextLong

!******************************************************************************

FUNCTION Mt64RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt64RNG), INTENT(IN)  :: RNG      !! 'Mt64RNG' object
    tCharAlloc                  :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Mt64RNG'
    ! to prevent warning of unused variable(s)
    ASSOCIATE (Dummy => RNG); END ASSOCIATE
    
    RETURN

END FUNCTION Mt64RNG_GetName

!******************************************************************************

FUNCTION Mt64RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt64RNG), INTENT(IN)  :: RNG      !! 'Mt64RNG' object
    tIndex                      :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = NN
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Mt64RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_Mt64RNG
    
!******************************************************************************
