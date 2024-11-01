
MODULE MBase_PolyFloatNum

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains common routines for basic operations of floating-point numbers
!   using the unlimited polymorphic type as a type of the floating-point numbers.  These
!   routines can handle only the Fortran intrinsic real and complex types with a default
!   kind where the default kind for both real and complex types is defined in the
!   *MBase_Kinds* module.  <br>
!   <br>
!^ **Important Note**: <br>
!   By design, most routines in this module use the unlimited polymorphic type (i.e.
!   CLASS(*)) to handle either a real or a complex type and they do not check for any
!   other types.  This means that these routines will silently return (without reporting
!   any errors) if invalid types are specified as arguments.  It is a user responsibility
!   to use these routines properly.  It should also be mentioned that if two or more
!   arguments are required by a routine, the specified arguments must have the same type,
!   either the real or the complex type.  Otherwise, the routine will also silently return. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MiscUtil
    USE MBase_AnyRankNType
    USE MBase_ErrHandlers
    USE MBase_SIntUtil,         ONLY: ToChar => ToDecStrSigned

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! miscellaneous routines
    PUBLIC  :: PolyDot
    PUBLIC  :: PolyNorm2
    PUBLIC  :: PolyAbsolute
    PUBLIC  :: PolySquareRoot
    PUBLIC  :: PolyLogarithm
    PUBLIC  :: PolySwapFloat
    PUBLIC  :: PolyCompare
    ! arithmetic routines
    PUBLIC  :: PolyScaleSum
    PUBLIC  :: PolyScaleAdd
    PUBLIC  :: PolyScaleArray
    PUBLIC  :: PolyScaleSelf
    PUBLIC  :: PolyScaleOther
    PUBLIC  :: PolyDivision
    PUBLIC  :: PolyProduct
    PUBLIC  :: PolyInvSelf
    PUBLIC  :: PolyInvOther
    PUBLIC  :: PolyNegate
    ! assignment routines
    PUBLIC  :: PolyAssign
    PUBLIC  :: PolySetZero
    PUBLIC  :: PolySetOne
    ! type-checking routines
    PUBLIC  :: IsPolyTypeValid
    ! memory management for allocatable argument
    PUBLIC  :: PolyAllocate
    PUBLIC  :: PolyResize
    ! memory management for pointer argument
#ifdef  __INTEL_COMPILER
    PUBLIC  :: PolyAllocatePointer
    PUBLIC  :: PolyAllocatePointer1D
    PUBLIC  :: PolyAllocatePointer2D
    PUBLIC  :: PolyResizePointer1D
    PUBLIC  :: PolyResizePointer2D
#endif

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MBase_PolyFloatNum'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! typical constants
    tFloat,    PARAMETER, PUBLIC    :: ZEROR = 0.0_kFloat
    tFloat,    PARAMETER, PUBLIC    :: ONER  = 1.0_kFloat
    tFloat,    PARAMETER, PUBLIC    :: PI_R  = 3.141592653589793238462643383279502884197_kFloat
    tCmplx,    PARAMETER, PUBLIC    :: ZEROC = (ZEROR,ZEROR)
    tCmplx,    PARAMETER, PUBLIC    :: ONEC  = (ONER, ZEROR)
    tCmplx,    PARAMETER, PUBLIC    :: PI_C  = (PI_R, ZEROR)
    ! machine dependent parameters
    tFloat,    PARAMETER, PUBLIC    :: MachEpsR = EPSILON(ONER)     ! machine epsilon
    tFloat,    PARAMETER, PUBLIC    :: SmallR   = TINY(ONER)        ! the smallest positive number
    tFloat,    PARAMETER, PUBLIC    :: LargeR   = HUGE(ONER)        ! the largest positive number

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! arithmetic routines
    INTERFACE PolyScaleSum
        !^ **Subroutine Interface**: PolyScaleSum <br>
        !  **Purpose**:  To perform scaling and addition of one of the followings: <br>
        !       1. CVal = Alpha*(ASign)AVal + Beta*(BSign)BVal, or <br>
        !       2. CVal =       (ASign)AVal + Beta*(BSign)BVal, or <br>
        !       3. CVal = Alpha*(ASign)AVal +      (BSign)BVal, or <br>
        !       4. CVal =       (ASign)AVal +      (BSign)BVal. <br>
        MODULE PROCEDURE PolyScaleSumScalar
        MODULE PROCEDURE PolyScaleSumArray1D
        MODULE PROCEDURE PolyScaleSumArray2D
    END INTERFACE
    INTERFACE PolyScaleAdd
        !^ **Subroutine Interface**: PolyScaleAdd <br>
        !  **Purpose**:  To perform scaling and addition of one of the followings: <br>
        !       1. AVal = Alpha*(ASign)AVal + Beta*(BSign)BVal, or <br>
        !       2. AVal =       (ASign)AVal + Beta*(BSign)BVal, or <br>
        !       3. AVal = Alpha*(ASign)AVal +      (BSign)BVal, or <br>
        !       4. AVal =       (ASign)AVal +      (BSign)BVal. <br>
        MODULE PROCEDURE PolyScaleAddScalar
        MODULE PROCEDURE PolyScaleAddArray1D
        MODULE PROCEDURE PolyScaleAddArray2D
    END INTERFACE
    INTERFACE PolyScaleArray
        !^ **Subroutine Interface**: PolyScaleArray <br>
        !  **Purpose**:  To perform scaling of one of the followings: <br>
        !       1. AVal = Alpha*(Sign)AVal, or <br>
        !       2. BVal = Alpha*(Sign)AVal. <br>
        MODULE PROCEDURE PolyScaleSelfArray1D
        MODULE PROCEDURE PolyScaleSelfArray2D
        MODULE PROCEDURE PolyScaleArray1D
        MODULE PROCEDURE PolyScaleArray2D
    END INTERFACE
    INTERFACE PolyProduct
        !^ **Subroutine Interface**: PolyProduct <br>
        !  **Purpose**:  To perform multiplication: CVal = (Sign)(AVal*BVal). <br>
        MODULE PROCEDURE PolyProductArray1D
    END INTERFACE
    INTERFACE PolyInvSelf
        !^ **Subroutine Interface**: PolyInvSelf <br>
        !  **Purpose**:  To perform an inversion of the specified argument. <br>
        MODULE PROCEDURE PolyInverseSelf
    END INTERFACE
    INTERFACE PolyInvOther
        !^ **Subroutine Interface**: PolyInvOther <br>
        !  **Purpose**:  To perform an inversion of the specified argument. <br>
        MODULE PROCEDURE PolyInverseOther
    END INTERFACE
    INTERFACE PolyNegate
        !^ **Subroutine Interface**: PolyNegate <br>
        !  **Purpose**:  To perform negation of the specified argument. <br>
        MODULE PROCEDURE PolyNegateSelf
        MODULE PROCEDURE PolyNegateSelf1D
        MODULE PROCEDURE PolyNegateSelf2D
        MODULE PROCEDURE PolyNegateOther
        MODULE PROCEDURE PolyNegateOther1D
        MODULE PROCEDURE PolyNegateOther2D
    END INTERFACE
    ! assignment routines
    INTERFACE PolyAssign
        !^ **Subroutine Interface**: PolyAssign <br>
        !  **Purpose**:  To perform assignment of the specified argument. <br>
        MODULE PROCEDURE PolyAssignScalar
        MODULE PROCEDURE PolyAssignArray1D
        MODULE PROCEDURE PolyAssignArray2D
    END INTERFACE
    INTERFACE PolySetZero
        !^ **Subroutine Interface**: PolySetZero <br>
        !  **Purpose**:  To set value(s) of the specified argument to zero. <br>
        MODULE PROCEDURE PolySetZeroScalar
        MODULE PROCEDURE PolySetZeroArray1D
        MODULE PROCEDURE PolySetZeroArray2D
    END INTERFACE
    INTERFACE PolySetOne
        !^ **Subroutine Interface**: PolySetOne <br>
        !  **Purpose**:  To set value(s) of the specified argument to one. <br>
        MODULE PROCEDURE PolySetOneScalar
        MODULE PROCEDURE PolySetOneArray1D
        MODULE PROCEDURE PolySetOneArray2D
    END INTERFACE
    ! type-checking routines
    INTERFACE IsPolyTypeValid
        !^ **Subroutine Interface**: IsPolyTypeValid <br>
        !  **Purpose**:  To check whether the specified input has the same type and their type is valid. <br>
        MODULE PROCEDURE IsPolyTypeValidI
        MODULE PROCEDURE IsPolyTypeValidII
        MODULE PROCEDURE IsPolyTypeValidIII
        MODULE PROCEDURE IsPolyTypeValidIV
        MODULE PROCEDURE IsPolyTypeValidV
    END INTERFACE
    ! miscellaneous routines
    INTERFACE PolySwapFloat
        !^ **Subroutine Interface**: PolySwap <br>
        !  **Purpose**:  To perform swapping of the specified arguments. <br>
        MODULE PROCEDURE PolySwapScalar
        MODULE PROCEDURE PolySwapArray1D
    END INTERFACE
    ! memory-management routines
    INTERFACE PolyAllocate
        !^ **Subroutine Interface**: PolyAllocate <br>
        !  **Purpose**:  To allocate memory of the specified allocatable argument. <br>
        MODULE PROCEDURE AllocatePolyAllocatable
        MODULE PROCEDURE AllocatePolyArray1D
        MODULE PROCEDURE AllocatePolyArray2D
    END INTERFACE
    INTERFACE PolyResize
        !^ **Subroutine Interface**: PolyResize <br>
        !  **Purpose**:  To re-allocate memory of the specified allocatable argument and
        !       preserve its data. <br>
        MODULE PROCEDURE ResizePolyArray1D
        MODULE PROCEDURE ResizePolyArray2D
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           ARITHMETIC ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE PolyNegateSelf(AVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform negation of the specified argument.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(INOUT) :: AVal     !! value to be negated

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        AVal = -AVal
    TYPE IS (tCmplx)
        AVal = -AVal
    END SELECT

    RETURN

END SUBROUTINE PolyNegateSelf

!******************************************************************************

SUBROUTINE PolyNegateSelf1D(AVal,N)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform negation of the specified argument.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: N        !! size of AVal
    CLASS(*), INTENT(INOUT) :: AVal(N)  !! values to be negated

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        AVal(1:N) = -AVal(1:N)
    TYPE IS (tCmplx)
        AVal(1:N) = -AVal(1:N)
    END SELECT

    RETURN

END SUBROUTINE PolyNegateSelf1D

!******************************************************************************

SUBROUTINE PolyNegateSelf2D(AVal,M,N)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform negation of the specified argument.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: M            !! size of the first dimension of AVal
    tIndex,   INTENT(IN)    :: N            !! size of the second dimension of AVal
    CLASS(*), INTENT(INOUT) :: AVal(M,N)    !! values to be negated

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        AVal(1:M,1:N) = -AVal(1:M,1:N)
    TYPE IS (tCmplx)
        AVal(1:M,1:N) = -AVal(1:M,1:N)
    END SELECT

    RETURN

END SUBROUTINE PolyNegateSelf2D

!******************************************************************************

SUBROUTINE PolyNegateOther(BVal,AVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform negation of the specified argument.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(OUT)   :: BVal     !! negated value
    CLASS(*), INTENT(IN)    :: AVal     !! input value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (BVal)
    TYPE IS (tFloat)
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            BVal = -AVal
        TYPE IS (tCmplx)
            BVal = -REAL(AVal,KIND=kFloat)
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            BVal = -CMPLX(AVal,ZEROR,KIND=kFloat)
        TYPE IS (tCmplx)
            BVal = -AVal
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyNegateOther

!******************************************************************************

SUBROUTINE PolyNegateOther1D(BVal,AVal,N)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform negation of the specified argument.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: N        !! size of the arguments
    CLASS(*), INTENT(OUT)   :: BVal(N)  !! negated values
    CLASS(*), INTENT(IN)    :: AVal(N)  !! input values

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    SELECT TYPE (BVal)
    TYPE IS (tFloat)
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            BVal(1:N) = -AVal(1:N)
        TYPE IS (tCmplx)
            BVal(1:N) = -REAL(AVal(1:N),KIND=kFloat)
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            FORALL (I=1:N) BVal(I) = -CMPLX(AVal(I),ZEROR,KIND=kFloat)
        TYPE IS (tCmplx)
            BVal(1:N) = -AVal(1:N)
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyNegateOther1D

!******************************************************************************

SUBROUTINE PolyNegateOther2D(BVal,AVal,M,N)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform negation of the specified argument.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: M            !! size of the first dimension of the arguments
    tIndex,   INTENT(IN)    :: N            !! size of the second dimension of the arguments
    CLASS(*), INTENT(OUT)   :: BVal(M,N)    !! negated values
    CLASS(*), INTENT(IN)    :: AVal(M,N)    !! input values

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J

! FLOW

    SELECT TYPE (BVal)
    TYPE IS (tFloat)
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            BVal(1:M,1:N) = -AVal(1:M,1:N)
        TYPE IS (tCmplx)
            BVal(1:M,1:N) = -REAL(AVal(1:M,1:N),KIND=kFloat)
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            FORALL (I=1:M,J=1:N) BVal(I,J) = -CMPLX(AVal(I,J),ZEROR,KIND=kFloat)
        TYPE IS (tCmplx)
            BVal(1:M,1:N) = -AVal(1:M,1:N)
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyNegateOther2D

!******************************************************************************

SUBROUTINE PolyScaleSelf(AVal,Alpha,Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform scaling: AVal = Alpha*(Sign)AVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),           INTENT(INOUT)   :: AVal     !! value to be scaled
    CLASS(*),           INTENT(IN)      :: Alpha    !! scaling factor
    tSInt32,  OPTIONAL, INTENT(IN)      :: Sign     !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Positive

! FLOW

    ! set default and check sign
    Positive = TrueVal
    IF (PRESENT(Sign)) THEN
        IF (Sign < 0) Positive = FalseVal
    END IF

    IF (.NOT.Positive) THEN
        ! negate AVal
        CALL PolyNegate(AVal)
    END IF

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        SELECT TYPE (Alpha)
        TYPE IS (tFloat)
            AVal = Alpha*AVal
        TYPE IS (tCmplx)
            AVal = REAL(Alpha,KIND=kFloat)*AVal
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (Alpha)
            TYPE IS (tFloat)
            AVal = CMPLX(Alpha,ZEROR,KIND=kFloat)*AVal
        TYPE IS (tCmplx)
            AVal = Alpha*AVal
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyScaleSelf

!******************************************************************************

SUBROUTINE PolyScaleSelfArray1D(AVal,Alpha,N,Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform scaling: AVal = Alpha*(Sign)AVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)      :: N        !! size of AVal
    CLASS(*),           INTENT(INOUT)   :: AVal(N)  !! values to be scaled
    CLASS(*),           INTENT(IN)      :: Alpha    !! scaling factor
    tSInt32,  OPTIONAL, INTENT(IN)      :: Sign     !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Positive

! FLOW

    ! set default and check sign
    Positive = TrueVal
    IF (PRESENT(Sign)) THEN
        IF (Sign < 0) Positive = FalseVal
    END IF

    IF (.NOT.Positive) THEN
        ! negate AVal
        CALL PolyNegate(AVal,N)
    END IF

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        SELECT TYPE (Alpha)
        TYPE IS (tFloat)
            AVal(1:N) = Alpha*AVal(1:N)
        TYPE IS (tCmplx)
            AVal(1:N) = REAL(Alpha,KIND=kFloat)*AVal(1:N)
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (Alpha)
            TYPE IS (tFloat)
            AVal(1:N) = CMPLX(Alpha,ZEROR,KIND=kFloat)*AVal(1:N)
        TYPE IS (tCmplx)
            AVal(1:N) = Alpha*AVal(1:N)
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyScaleSelfArray1D

!******************************************************************************

SUBROUTINE PolyScaleSelfArray2D(AVal,Alpha,M,N,Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform scaling: AVal = Alpha*(Sign)AVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)      :: M            !! size of the first dimension of AVal
    tIndex,             INTENT(IN)      :: N            !! size of the second dimension of AVal
    CLASS(*),           INTENT(INOUT)   :: AVal(M,N)    !! values to be scaled
    CLASS(*),           INTENT(IN)      :: Alpha        !! scaling factor
    tSInt32,  OPTIONAL, INTENT(IN)      :: Sign         !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Positive

! FLOW

    ! set default and check sign
    Positive = TrueVal
    IF (PRESENT(Sign)) THEN
        IF (Sign < 0) Positive = FalseVal
    END IF

    IF (.NOT.Positive) THEN
        ! negate AVal
        CALL PolyNegate(AVal,M,N)
    END IF

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        SELECT TYPE (Alpha)
        TYPE IS (tFloat)
            AVal(1:M,1:N) = Alpha*AVal(1:M,1:N)
        TYPE IS (tCmplx)
            AVal(1:M,1:N) = REAL(Alpha,KIND=kFloat)*AVal(1:M,1:N)
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (Alpha)
            TYPE IS (tFloat)
            AVal(1:M,1:N) = CMPLX(Alpha,ZEROR,KIND=kFloat)*AVal(1:M,1:N)
        TYPE IS (tCmplx)
            AVal(1:M,1:N) = Alpha*AVal(1:M,1:N)
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyScaleSelfArray2D

!******************************************************************************

SUBROUTINE PolyScaleOther(BVal,AVal,Alpha,Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform scaling: BVal = Alpha*(Sign)AVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),           INTENT(OUT) :: BVal     !! scaled value
    CLASS(*),           INTENT(IN)  :: AVal     !! input value
    CLASS(*),           INTENT(IN)  :: Alpha    !! scaling factor
    tSInt32,  OPTIONAL, INTENT(IN)  :: Sign     !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set BVal = AVal
    CALL PolyAssign(BVal,AVal)

    ! scale BVal by Alpha
    IF (PRESENT(Sign)) THEN
        CALL PolyScaleSelf(BVal,Alpha,Sign=Sign)
    ELSE
        CALL PolyScaleSelf(BVal,Alpha)
    END IF

    RETURN

END SUBROUTINE PolyScaleOther

!******************************************************************************

SUBROUTINE PolyScaleArray1D(BVal,AVal,Alpha,N,Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform scaling: BVal = Alpha*(Sign)AVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)  :: N        !! size of the arguments
    CLASS(*),           INTENT(OUT) :: BVal(N)  !! scaled values
    CLASS(*),           INTENT(IN)  :: AVal(N)  !! input values
    CLASS(*),           INTENT(IN)  :: Alpha    !! scaling factor
    tSInt32,  OPTIONAL, INTENT(IN)  :: Sign     !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set BVal = AVal
    CALL PolyAssign(BVal,AVal,N)

    ! scale BVal by Alpha
    IF (PRESENT(Sign)) THEN
        CALL PolyScaleArray(BVal,Alpha,N,Sign=Sign)
    ELSE
        CALL PolyScaleArray(BVal,Alpha,N)
    END IF

    RETURN

END SUBROUTINE PolyScaleArray1D

!******************************************************************************

SUBROUTINE PolyScaleArray2D(BVal,AVal,Alpha,M,N,Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform scaling: BVal = Alpha*(Sign)AVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)  :: M            !! size of the first dimension of the arguments
    tIndex,             INTENT(IN)  :: N            !! size of the second dimension of the arguments
    CLASS(*),           INTENT(OUT) :: BVal(M,N)    !! scaled values
    CLASS(*),           INTENT(IN)  :: AVal(M,N)    !! input values
    CLASS(*),           INTENT(IN)  :: Alpha        !! scaling factor
    tSInt32,  OPTIONAL, INTENT(IN)  :: Sign         !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set BVal = AVal
    CALL PolyAssign(BVal,AVal,M,N)

    ! scale BVal by Alpha
    IF (PRESENT(Sign)) THEN
        CALL PolyScaleArray(BVal,Alpha,M,N,Sign=Sign)
    ELSE
        CALL PolyScaleArray(BVal,Alpha,M,N)
    END IF

    RETURN

END SUBROUTINE PolyScaleArray2D

!******************************************************************************

SUBROUTINE PolyProductArray1D(CVal,AVal,BVal,N,Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication: CVal = (Sign)(AVal*BVal).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)  :: N        !! size of the arguments
    CLASS(*),           INTENT(OUT) :: CVal(N)  !! values of the product
    CLASS(*),           INTENT(IN)  :: AVal(N)  !! values of the first factor
    CLASS(*),           INTENT(IN)  :: BVal(N)  !! values of the second factor
    tSInt32,  OPTIONAL, INTENT(IN)  :: Sign     !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Positive
    tIndex      :: I

! FLOW

    ! set CVal = AVal
    CALL PolyAssign(CVal,AVal,N)

    ! set default and check sign
    Positive = TrueVal
    IF (PRESENT(Sign)) THEN
        IF (Sign < 0) Positive = FalseVal
    END IF

    IF (.NOT.Positive) THEN
        ! negate CVal
        CALL PolyNegate(CVal,N)
    END IF

    SELECT TYPE (CVal)
    TYPE IS (tFloat)
        SELECT TYPE (BVal)
        TYPE IS (tFloat)
            CVal(1:N) = BVal(1:N)*CVal(1:N)
        TYPE IS (tCmplx)
            CVal(1:N) = REAL(BVal(1:N),KIND=kFloat)*CVal(1:N)
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (BVal)
            TYPE IS (tFloat)
            FORALL (I=1:N) CVal(I) = CMPLX(BVal(I),ZEROR,KIND=kFloat)*CVal(I)
        TYPE IS (tCmplx)
            CVal(1:N) = BVal(1:N)*CVal(1:N)
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyProductArray1D

!******************************************************************************

SUBROUTINE PolyScaleAddScalar(AVal,BVal,Alpha,Beta,ASign,BSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform scaling and addition of one of the followings: <br>
    !   1. AVal = Alpha*(ASign)AVal + Beta*(BSign)BVal, or <br>
    !   2. AVal =       (ASign)AVal + Beta*(BSign)BVal, or <br>
    !   3. AVal = Alpha*(ASign)AVal +      (BSign)BVal, or <br>
    !   4. AVal =       (ASign)AVal +      (BSign)BVal. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),           INTENT(INOUT)   :: AVal     !! on entry, input value; on exit, output value
    CLASS(*),           INTENT(IN)      :: BVal     !! input value
    CLASS(*), OPTIONAL, INTENT(IN)      :: Alpha    !! scaling factor of AVal
    CLASS(*), OPTIONAL, INTENT(IN)      :: Beta     !! scaling factor of BVal
    tSInt32,  OPTIONAL, INTENT(IN)      :: ASign    !! if < 0, negative; otherwise, positive (default)
    tSInt32,  OPTIONAL, INTENT(IN)      :: BSign    !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: APositive, BPositive

! FLOW

    ! set defaults and check signs
    APositive = TrueVal
    BPositive = TrueVal
    IF (PRESENT(ASign).AND.PRESENT(BSign)) THEN
        IF (ASign < 0) APositive = FalseVal
        IF (BSign < 0) BPositive = FalseVal
    ELSEIF (PRESENT(ASign)) THEN
        IF (ASign < 0) APositive = FalseVal
    ELSEIF (PRESENT(BSign)) THEN
        IF (BSign < 0) BPositive = FalseVal
    END IF

    IF (.NOT.APositive) THEN
        ! negate AVal
        CALL PolyNegate(AVal)
    END IF

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        SELECT TYPE (BVal)
        TYPE IS (tFloat)
            IF (PRESENT(Alpha).AND.PRESENT(Beta)) THEN
                SELECT TYPE (Alpha)
                TYPE IS (tFloat)
                    SELECT TYPE (Beta)
                    TYPE IS (tFloat)
                        IF (BPositive) THEN
                            AVal = Alpha*AVal + Beta*BVal
                        ELSE
                            AVal = Alpha*AVal - Beta*BVal
                        END IF
                    END SELECT
                END SELECT
            ELSEIF (PRESENT(Alpha)) THEN
                SELECT TYPE (Alpha)
                TYPE IS (tFloat)
                    IF (BPositive) THEN
                        AVal = Alpha*AVal + BVal
                    ELSE
                        AVal = Alpha*AVal - BVal
                    END IF
                END SELECT
            ELSEIF (PRESENT(Beta)) THEN
                SELECT TYPE (Beta)
                TYPE IS (tFloat)
                    IF (BPositive) THEN
                        AVal = AVal + Beta*BVal
                    ELSE
                        AVal = AVal - Beta*BVal
                    END IF
                END SELECT
            ELSE
                IF (BPositive) THEN
                    AVal = AVal + BVal
                ELSE
                    AVal = AVal - BVal
                END IF
            END IF
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (BVal)
        TYPE IS (tCmplx)
            IF (PRESENT(Alpha).AND.PRESENT(Beta)) THEN
                SELECT TYPE (Alpha)
                TYPE IS (tCmplx)
                    SELECT TYPE (Beta)
                    TYPE IS (tCmplx)
                        IF (BPositive) THEN
                            AVal = Alpha*AVal + Beta*BVal
                        ELSE
                            AVal = Alpha*AVal - Beta*BVal
                        END IF
                    END SELECT
                END SELECT
            ELSEIF (PRESENT(Alpha)) THEN
                SELECT TYPE (Alpha)
                TYPE IS (tCmplx)
                    IF (BPositive) THEN
                        AVal = Alpha*AVal + BVal
                    ELSE
                        AVal = Alpha*AVal - BVal
                    END IF
                END SELECT
            ELSEIF (PRESENT(Beta)) THEN
                SELECT TYPE (Beta)
                TYPE IS (tCmplx)
                    IF (BPositive) THEN
                        AVal = AVal + Beta*BVal
                    ELSE
                        AVal = AVal - Beta*BVal
                    END IF
                END SELECT
            ELSE
                IF (BPositive) THEN
                    AVal = AVal + BVal
                ELSE
                    AVal = AVal - BVal
                END IF
            END IF
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyScaleAddScalar

!******************************************************************************

SUBROUTINE PolyScaleAddArray1D(AVal,BVal,N,Alpha,Beta,ASign,BSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform scaling and addition of one of the followings: <br>
    !   1. AVal = Alpha*(ASign)AVal + Beta*(BSign)BVal, or <br>
    !   2. AVal =       (ASign)AVal + Beta*(BSign)BVal, or <br>
    !   3. AVal = Alpha*(ASign)AVal +      (BSign)BVal, or <br>
    !   4. AVal =       (ASign)AVal +      (BSign)BVal. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)      :: N        !! size of the arguments
    CLASS(*),           INTENT(INOUT)   :: AVal(N)  !! on entry, input values; on exit, output values
    CLASS(*),           INTENT(IN)      :: BVal(N)  !! input values
    CLASS(*), OPTIONAL, INTENT(IN)      :: Alpha    !! scaling factor of AVal
    CLASS(*), OPTIONAL, INTENT(IN)      :: Beta     !! scaling factor of BVal
    tSInt32,  OPTIONAL, INTENT(IN)      :: ASign    !! if < 0, negative; otherwise, positive (default)
    tSInt32,  OPTIONAL, INTENT(IN)      :: BSign    !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: APositive, BPositive

! FLOW

    ! set defaults and check signs
    APositive = TrueVal
    BPositive = TrueVal
    IF (PRESENT(ASign).AND.PRESENT(BSign)) THEN
        IF (ASign < 0) APositive = FalseVal
        IF (BSign < 0) BPositive = FalseVal
    ELSEIF (PRESENT(ASign)) THEN
        IF (ASign < 0) APositive = FalseVal
    ELSEIF (PRESENT(BSign)) THEN
        IF (BSign < 0) BPositive = FalseVal
    END IF

    IF (.NOT.APositive) THEN
        ! negate AVal
        CALL PolyNegate(AVal,N)
    END IF

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        SELECT TYPE (BVal)
        TYPE IS (tFloat)
            IF (PRESENT(Alpha).AND.PRESENT(Beta)) THEN
                SELECT TYPE (Alpha)
                TYPE IS (tFloat)
                    SELECT TYPE (Beta)
                    TYPE IS (tFloat)
                        IF (BPositive) THEN
                            AVal(1:N) = Alpha*AVal(1:N) + Beta*BVal(1:N)
                        ELSE
                            AVal(1:N) = Alpha*AVal(1:N) - Beta*BVal(1:N)
                        END IF
                    END SELECT
                END SELECT
            ELSEIF (PRESENT(Alpha)) THEN
                SELECT TYPE (Alpha)
                TYPE IS (tFloat)
                    IF (BPositive) THEN
                        AVal(1:N) = Alpha*AVal(1:N) + BVal(1:N)
                    ELSE
                        AVal(1:N) = Alpha*AVal(1:N) - BVal(1:N)
                    END IF
                END SELECT
            ELSEIF (PRESENT(Beta)) THEN
                SELECT TYPE (Beta)
                TYPE IS (tFloat)
                    IF (BPositive) THEN
                        AVal(1:N) = AVal(1:N) + Beta*BVal(1:N)
                    ELSE
                        AVal(1:N) = AVal(1:N) - Beta*BVal(1:N)
                    END IF
                END SELECT
            ELSE
                IF (BPositive) THEN
                    AVal(1:N) = AVal(1:N) + BVal(1:N)
                ELSE
                    AVal(1:N) = AVal(1:N) - BVal(1:N)
                END IF
            END IF
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (BVal)
        TYPE IS (tCmplx)
            IF (PRESENT(Alpha).AND.PRESENT(Beta)) THEN
                SELECT TYPE (Alpha)
                TYPE IS (tCmplx)
                    SELECT TYPE (Beta)
                    TYPE IS (tCmplx)
                        IF (BPositive) THEN
                            AVal(1:N) = Alpha*AVal(1:N) + Beta*BVal(1:N)
                        ELSE
                            AVal(1:N) = Alpha*AVal(1:N) - Beta*BVal(1:N)
                        END IF
                    END SELECT
                END SELECT
            ELSEIF (PRESENT(Alpha)) THEN
                SELECT TYPE (Alpha)
                TYPE IS (tCmplx)
                    IF (BPositive) THEN
                        AVal(1:N) = Alpha*AVal(1:N) + BVal(1:N)
                    ELSE
                        AVal(1:N) = Alpha*AVal(1:N) - BVal(1:N)
                    END IF
                END SELECT
            ELSEIF (PRESENT(Beta)) THEN
                SELECT TYPE (Beta)
                TYPE IS (tCmplx)
                    IF (BPositive) THEN
                        AVal(1:N) = AVal(1:N) + Beta*BVal(1:N)
                    ELSE
                        AVal(1:N) = AVal(1:N) - Beta*BVal(1:N)
                    END IF
                END SELECT
            ELSE
                IF (BPositive) THEN
                    AVal(1:N) = AVal(1:N) + BVal(1:N)
                ELSE
                    AVal(1:N) = AVal(1:N) - BVal(1:N)
                END IF
            END IF
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyScaleAddArray1D

!******************************************************************************

SUBROUTINE PolyScaleAddArray2D(AVal,BVal,M,N,Alpha,Beta,ASign,BSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform scaling and addition of one of the followings: <br>
    !   1. AVal = Alpha*(ASign)AVal + Beta*(BSign)BVal, or <br>
    !   2. AVal =       (ASign)AVal + Beta*(BSign)BVal, or <br>
    !   3. AVal = Alpha*(ASign)AVal +      (BSign)BVal, or <br>
    !   4. AVal =       (ASign)AVal +      (BSign)BVal. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)      :: M            !! size of the first dimension of the arguments
    tIndex,             INTENT(IN)      :: N            !! size of the second dimension of the arguments
    CLASS(*),           INTENT(INOUT)   :: AVal(M,N)    !! on entry, input values; on exit, output values
    CLASS(*),           INTENT(IN)      :: BVal(M,N)    !! input values
    CLASS(*), OPTIONAL, INTENT(IN)      :: Alpha        !! scaling factor of AVal
    CLASS(*), OPTIONAL, INTENT(IN)      :: Beta         !! scaling factor of BVal
    tSInt32,  OPTIONAL, INTENT(IN)      :: ASign        !! if < 0, negative; otherwise, positive (default)
    tSInt32,  OPTIONAL, INTENT(IN)      :: BSign        !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: APositive, BPositive

! FLOW

    ! set defaults and check signs
    APositive = TrueVal
    BPositive = TrueVal
    IF (PRESENT(ASign).AND.PRESENT(BSign)) THEN
        IF (ASign < 0) APositive = FalseVal
        IF (BSign < 0) BPositive = FalseVal
    ELSEIF (PRESENT(ASign)) THEN
        IF (ASign < 0) APositive = FalseVal
    ELSEIF (PRESENT(BSign)) THEN
        IF (BSign < 0) BPositive = FalseVal
    END IF

    IF (.NOT.APositive) THEN
        ! negate AVal
        CALL PolyNegate(AVal,M,N)
    END IF

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        SELECT TYPE (BVal)
        TYPE IS (tFloat)
            IF (PRESENT(Alpha).AND.PRESENT(Beta)) THEN
                SELECT TYPE (Alpha)
                TYPE IS (tFloat)
                    SELECT TYPE (Beta)
                    TYPE IS (tFloat)
                        IF (BPositive) THEN
                            AVal(1:M,1:N) = Alpha*AVal(1:M,1:N) + Beta*BVal(1:M,1:N)
                        ELSE
                            AVal(1:M,1:N) = Alpha*AVal(1:M,1:N) - Beta*BVal(1:M,1:N)
                        END IF
                    END SELECT
                END SELECT
            ELSEIF (PRESENT(Alpha)) THEN
                SELECT TYPE (Alpha)
                TYPE IS (tFloat)
                    IF (BPositive) THEN
                        AVal(1:M,1:N) = Alpha*AVal(1:M,1:N) + BVal(1:M,1:N)
                    ELSE
                        AVal(1:M,1:N) = Alpha*AVal(1:M,1:N) - BVal(1:M,1:N)
                    END IF
                END SELECT
            ELSEIF (PRESENT(Beta)) THEN
                SELECT TYPE (Beta)
                TYPE IS (tFloat)
                    IF (BPositive) THEN
                        AVal(1:M,1:N) = AVal(1:M,1:N) + Beta*BVal(1:M,1:N)
                    ELSE
                        AVal(1:M,1:N) = AVal(1:M,1:N) - Beta*BVal(1:M,1:N)
                    END IF
                END SELECT
            ELSE
                IF (BPositive) THEN
                    AVal(1:M,1:N) = AVal(1:M,1:N) + BVal(1:M,1:N)
                ELSE
                    AVal(1:M,1:N) = AVal(1:M,1:N) - BVal(1:M,1:N)
                END IF
            END IF
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (BVal)
        TYPE IS (tCmplx)
            IF (PRESENT(Alpha).AND.PRESENT(Beta)) THEN
                SELECT TYPE (Alpha)
                TYPE IS (tCmplx)
                    SELECT TYPE (Beta)
                    TYPE IS (tCmplx)
                        IF (BPositive) THEN
                            AVal(1:M,1:N) = Alpha*AVal(1:M,1:N) + Beta*BVal(1:M,1:N)
                        ELSE
                            AVal(1:M,1:N) = Alpha*AVal(1:M,1:N) - Beta*BVal(1:M,1:N)
                        END IF
                    END SELECT
                END SELECT
            ELSEIF (PRESENT(Alpha)) THEN
                SELECT TYPE (Alpha)
                TYPE IS (tCmplx)
                    IF (BPositive) THEN
                        AVal(1:M,1:N) = Alpha*AVal(1:M,1:N) + BVal(1:M,1:N)
                    ELSE
                        AVal(1:M,1:N) = Alpha*AVal(1:M,1:N) - BVal(1:M,1:N)
                    END IF
                END SELECT
            ELSEIF (PRESENT(Beta)) THEN
                SELECT TYPE (Beta)
                TYPE IS (tCmplx)
                    IF (BPositive) THEN
                        AVal(1:M,1:N) = AVal(1:M,1:N) + Beta*BVal(1:M,1:N)
                    ELSE
                        AVal(1:M,1:N) = AVal(1:M,1:N) - Beta*BVal(1:M,1:N)
                    END IF
                END SELECT
            ELSE
                IF (BPositive) THEN
                    AVal(1:M,1:N) = AVal(1:M,1:N) + BVal(1:M,1:N)
                ELSE
                    AVal(1:M,1:N) = AVal(1:M,1:N) - BVal(1:M,1:N)
                END IF
            END IF
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyScaleAddArray2D

!******************************************************************************

SUBROUTINE PolyScaleSumScalar(CVal,AVal,BVal,Alpha,Beta,ASign,BSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform scaling and addition of one of the followings: <br>
    !   1. CVal = Alpha*(ASign)AVal + Beta*(BSign)BVal, or <br>
    !   2. CVal =       (ASign)AVal + Beta*(BSign)BVal, or <br>
    !   3. CVal = Alpha*(ASign)AVal +      (BSign)BVal, or <br>
    !   4. CVal =       (ASign)AVal +      (BSign)BVal. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),           INTENT(OUT) :: CVal     !! output value
    CLASS(*),           INTENT(IN)  :: AVal     !! input value
    CLASS(*),           INTENT(IN)  :: BVal     !! input value
    CLASS(*), OPTIONAL, INTENT(IN)  :: Alpha    !! scaling factor of AVal
    CLASS(*), OPTIONAL, INTENT(IN)  :: Beta     !! scaling factor of BVal
    tSInt32,  OPTIONAL, INTENT(IN)  :: ASign    !! if < 0, negative; otherwise, positive (default)
    tSInt32,  OPTIONAL, INTENT(IN)  :: BSign    !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL PolyAssign(CVal,AVal)
    CALL PolyScaleAdd(CVal,BVal,Alpha,Beta,ASign,BSign)

    RETURN

END SUBROUTINE PolyScaleSumScalar

!******************************************************************************

SUBROUTINE PolyScaleSumArray1D(CVal,AVal,BVal,N,Alpha,Beta,ASign,BSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform scaling and addition of one of the followings: <br>
    !   1. CVal = Alpha*(ASign)AVal + Beta*(BSign)BVal, or <br>
    !   2. CVal =       (ASign)AVal + Beta*(BSign)BVal, or <br>
    !   3. CVal = Alpha*(ASign)AVal +      (BSign)BVal, or <br>
    !   4. CVal =       (ASign)AVal +      (BSign)BVal. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)  :: N        !! size of the arguments
    CLASS(*),           INTENT(OUT) :: CVal(N)  !! output values
    CLASS(*),           INTENT(IN)  :: AVal(N)  !! input values
    CLASS(*),           INTENT(IN)  :: BVal(N)  !! input values
    CLASS(*), OPTIONAL, INTENT(IN)  :: Alpha    !! scaling factor of AVal
    CLASS(*), OPTIONAL, INTENT(IN)  :: Beta     !! scaling factor of BVal
    tSInt32,  OPTIONAL, INTENT(IN)  :: ASign    !! if < 0, negative; otherwise, positive (default)
    tSInt32,  OPTIONAL, INTENT(IN)  :: BSign    !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL PolyAssign(CVal,AVal,N)
    CALL PolyScaleAdd(CVal,BVal,N,Alpha,Beta,ASign,BSign)

    RETURN

END SUBROUTINE PolyScaleSumArray1D

!******************************************************************************

SUBROUTINE PolyScaleSumArray2D(CVal,AVal,BVal,M,N,Alpha,Beta,ASign,BSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform scaling and addition of one of the followings: <br>
    !   1. CVal = Alpha*(ASign)AVal + Beta*(BSign)BVal, or <br>
    !   2. CVal =       (ASign)AVal + Beta*(BSign)BVal, or <br>
    !   3. CVal = Alpha*(ASign)AVal +      (BSign)BVal, or <br>
    !   4. CVal =       (ASign)AVal +      (BSign)BVal. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)  :: M            !! size of the first dimension of the arguments
    tIndex,             INTENT(IN)  :: N            !! size of the second dimension of the arguments
    CLASS(*),           INTENT(OUT) :: CVal(M,N)    !! output values
    CLASS(*),           INTENT(IN)  :: AVal(M,N)    !! input values
    CLASS(*),           INTENT(IN)  :: BVal(M,N)    !! input values
    CLASS(*), OPTIONAL, INTENT(IN)  :: Alpha        !! scaling factor of AVal
    CLASS(*), OPTIONAL, INTENT(IN)  :: Beta         !! scaling factor of BVal
    tSInt32,  OPTIONAL, INTENT(IN)  :: ASign        !! if < 0, negative; otherwise, positive (default)
    tSInt32,  OPTIONAL, INTENT(IN)  :: BSign        !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL PolyAssign(CVal,AVal,M,N)
    CALL PolyScaleAdd(CVal,BVal,M,N,Alpha,Beta,ASign,BSign)

    RETURN

END SUBROUTINE PolyScaleSumArray2D

!******************************************************************************

SUBROUTINE PolyDivision(CVal,AVal,BVal,Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division: CVal = (Sign)AVal/BVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),           INTENT(OUT) :: CVal     !! quotient
    CLASS(*),           INTENT(IN)  :: AVal     !! dividend
    CLASS(*),           INTENT(IN)  :: BVal     !! divisor
    tSInt32,  OPTIONAL, INTENT(IN)  :: Sign     !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! perform inversion: CVal = 1/BVal
    CALL PolyInvOther(CVal,AVal=BVal)

    ! perform scaling: CVal = AVal*CVal
    IF (PRESENT(Sign)) THEN
        CALL PolyScaleSelf(CVal,AVal,Sign=Sign)
    ELSE
        CALL PolyScaleSelf(CVal,AVal)
    END IF

    RETURN

END SUBROUTINE PolyDivision

!******************************************************************************

SUBROUTINE PolyInverseSelf(AVal,Sign)

!** PURPOSE OF THIS SUBROUTINE
    !^ To perform inversion of the specified argument.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),           INTENT(INOUT)   :: AVal !! value to be inversed
    tSInt32,  OPTIONAL, INTENT(IN)      :: Sign !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Positive

! FLOW

    ! set default and check sign
    Positive = TrueVal
    IF (PRESENT(Sign)) THEN
        IF (Sign < 0) Positive = FalseVal
    END IF

    ! check type validity
    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        IF (AVal == ZEROR) THEN
            IF (Positive) THEN
                AVal = LargeR
            ELSE
                AVal = -LargeR
            END IF
            ! report error
            CALL Handle_ErrLevel('PolyInverseSelf', ModName, ErrWarning, &
                        'AVal is zero. Set AVal to LargeR.')
        ELSE
            IF (Positive) THEN
                AVal = ONER/AVal
            ELSE
                AVal = -ONER/AVal
            END IF
        END IF
    TYPE IS (tCmplx)
        IF (AVal == ZEROC) THEN
            IF (Positive) THEN
                AVal = CMPLX(LargeR,LargeR)
            ELSE
                AVal = -CMPLX(LargeR,LargeR)
            END IF
            CALL Handle_ErrLevel('PolyInverseSelf', ModName, ErrWarning, &
                        'AVal is zero. Set AVal to LargeR.')
        ELSE
            IF (Positive) THEN
                AVal = ONEC/AVal
            ELSE
                AVal = -ONEC/AVal
            END IF
        END IF
    END SELECT

    RETURN

END SUBROUTINE PolyInverseSelf

!******************************************************************************

SUBROUTINE PolyInverseOther(BVal,AVal,Sign)

!** PURPOSE OF THIS SUBROUTINE
    !^ To perform inversion of the specified argument.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),           INTENT(OUT) :: BVal !! inversed value
    CLASS(*),           INTENT(IN)  :: AVal !! input value
    tSInt32,  OPTIONAL, INTENT(IN)  :: Sign !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! set BVal = AVal
    CALL PolyAssign(BVal,AVal)

    ! perform inversion
    IF (PRESENT(Sign)) THEN
        CALL PolyInvSelf(BVal,Sign=Sign)
    ELSE
        CALL PolyInvSelf(BVal)
    END IF

    RETURN

END SUBROUTINE PolyInverseOther

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           ASSIGNMENT ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE PolyAssignScalar(BVal,AVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform assignment: BVal = AVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(OUT)   :: BVal
    CLASS(*), INTENT(IN)    :: AVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (BVal)
    TYPE IS (tFloat)
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            BVal = AVal
        TYPE IS (tCmplx)
            BVal = REAL(AVal,KIND=kFloat)
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            BVal = CMPLX(AVal,ZEROR,KIND=kFloat)
        TYPE IS (tCmplx)
            BVal = AVal
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyAssignScalar

!******************************************************************************

SUBROUTINE PolyAssignArray1D(BVal,AVal,N)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform assignment: BVal = AVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: N        !! size of the arguments
    CLASS(*), INTENT(OUT)   :: BVal(N)
    CLASS(*), INTENT(IN)    :: AVal(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (BVal)
    TYPE IS (tFloat)
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            BVal(1:N) = AVal(1:N)
        TYPE IS (tCmplx)
            BVal(1:N) = REAL(AVal(1:N),KIND=kFloat)
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            BVal(1:N) = CMPLX(AVal(1:N),ZEROR,KIND=kFloat)
        TYPE IS (tCmplx)
            BVal(1:N) = AVal(1:N)
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyAssignArray1D

!******************************************************************************

SUBROUTINE PolyAssignArray2D(BVal,AVal,M,N)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform assignment: BVal = AVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: M    !! size of the first dimension of the arguments
    tIndex,   INTENT(IN)    :: N    !! size of the second dimension of the arguments
    CLASS(*), INTENT(OUT)   :: BVal(M,N)
    CLASS(*), INTENT(IN)    :: AVal(M,N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (BVal)
    TYPE IS (tFloat)
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            BVal(1:M,1:N) = AVal(1:M,1:N)
        TYPE IS (tCmplx)
            BVal(1:M,1:N) = REAL(AVal(1:M,1:N),KIND=kFloat)
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            BVal(1:M,1:N) = CMPLX(AVal(1:M,1:N),ZEROR,KIND=kFloat)
        TYPE IS (tCmplx)
            BVal(1:M,1:N) = AVal(1:M,1:N)
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyAssignArray2D

!******************************************************************************

SUBROUTINE PolySetZeroScalar(AVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set value of the specified argument to zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),   INTENT(INOUT)   :: AVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        AVal = ZEROR
    TYPE IS (tCmplx)
        AVal = ZEROC
    END SELECT

    RETURN

END SUBROUTINE PolySetZeroScalar

!******************************************************************************

SUBROUTINE PolySetZeroArray1D(AVal,N)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set values of the specified argument to zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,    INTENT(IN)       :: N        !! size of the argument
    CLASS(*),  INTENT(INOUT)    :: AVal(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        AVal(1:N) = ZEROR
    TYPE IS (tCmplx)
        AVal(1:N) = ZEROC
    END SELECT

    RETURN

END SUBROUTINE PolySetZeroArray1D

!******************************************************************************

SUBROUTINE PolySetZeroArray2D(AVal,M,N)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set values of the specified argument to zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,    INTENT(IN)       :: M    !! size of the first dimension of the argument
    tIndex,    INTENT(IN)       :: N    !! size of the second dimension of the argument
    CLASS(*),  INTENT(INOUT)    :: AVal(M,N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        AVal(1:M,1:N) = ZEROR
    TYPE IS (tCmplx)
        AVal(1:M,1:N) = ZEROC
    END SELECT

    RETURN

END SUBROUTINE PolySetZeroArray2D

!******************************************************************************

SUBROUTINE PolySetOneScalar(AVal,Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set value of the specified argument to one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),           INTENT(INOUT)   :: AVal
    tSInt32,  OPTIONAL, INTENT(IN)      :: Sign !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Positive

! FLOW

    ! set default and check sign
    Positive = TrueVal
    IF (PRESENT(Sign)) THEN
        IF (Sign < 0) Positive = FalseVal
    END IF

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        IF (Positive) THEN
            AVal = ONER
        ELSE
            AVal = -ONER
        END IF
    TYPE IS (tCmplx)
        IF (Positive) THEN
            AVal = ONEC
        ELSE
            AVal = -ONEC
        END IF
    END SELECT

    RETURN

END SUBROUTINE PolySetOneScalar

!******************************************************************************

SUBROUTINE PolySetOneArray1D(AVal,N,Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set values of the specified argument to one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)      :: N        !! size of the argument
    CLASS(*),           INTENT(INOUT)   :: AVal(N)
    tSInt32,  OPTIONAL, INTENT(IN)      :: Sign     !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Positive

! FLOW

    ! set default and check sign
    Positive = TrueVal
    IF (PRESENT(Sign)) THEN
        IF (Sign < 0) Positive = FalseVal
    END IF

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        IF (Positive) THEN
            AVal(1:N) = ONER
        ELSE
            AVal(1:N) = -ONER
        END IF
    TYPE IS (tCmplx)
        IF (Positive) THEN
            AVal(1:N) = ONEC
        ELSE
            AVal(1:N) = -ONEC
        END IF
    END SELECT

    RETURN

END SUBROUTINE PolySetOneArray1D

!******************************************************************************

SUBROUTINE PolySetOneArray2D(AVal,M,N,Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set values of the specified argument to one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)      :: M        !! size of the first dimension of the argument
    tIndex,             INTENT(IN)      :: N        !! size of the second dimension of the argument
    CLASS(*),           INTENT(INOUT)   :: AVal(M,N)
    tSInt32,  OPTIONAL, INTENT(IN)      :: Sign     !! if < 0, negative; otherwise, positive (default)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Positive

! FLOW

    ! set default and check sign
    Positive = TrueVal
    IF (PRESENT(Sign)) THEN
        IF (Sign < 0) Positive = FalseVal
    END IF

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        IF (Positive) THEN
            AVal(1:M,1:N) = ONER
        ELSE
            AVal(1:M,1:N) = -ONER
        END IF
    TYPE IS (tCmplx)
        IF (Positive) THEN
            AVal(1:M,1:N) = ONEC
        ELSE
            AVal(1:M,1:N) = -ONEC
        END IF
    END SELECT

    RETURN

END SUBROUTINE PolySetOneArray2D

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           MISCELLANEOUS ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE PolyDot(Dot,AVec,BVec,N)

!** PURPOSE OF THIS SUBROUTINE
    !^ To perform dot-product multiplication of the specified arguments.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(OUT)   :: Dot      !! the dot-product value
    tIndex,   INTENT(IN)    :: N        !! size of the arguments
    CLASS(*), INTENT(IN)    :: AVec(N)
    CLASS(*), INTENT(IN)    :: BVec(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT TYPE (AVec)
    TYPE IS (tFloat)
        SELECT TYPE (BVec)
        TYPE IS (tFloat)
            SELECT TYPE (Dot)
            TYPE IS (tFloat)
                Dot = DOT_PRODUCT(AVec,BVec)
            END SELECT
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (BVec)
        TYPE IS (tCmplx)
            SELECT TYPE (Dot)
            TYPE IS (tCmplx)
                Dot = DOT_PRODUCT(AVec,BVec)
            END SELECT
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolyDot

!******************************************************************************

FUNCTION PolyNorm2(AVec,N,PDot) RESULT(Norm)

!** PURPOSE OF THIS SUBROUTINE
    !^ To determine the Euclidean norm of the specified argument.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)  :: N        !! size of the vector
    CLASS(*),           INTENT(IN)  :: AVec(N)
    CLASS(*), OPTIONAL, INTENT(IN)  :: PDot
    !! If present and its type is the default real type, the norm is the square root of *Pdot*.
    tFloat                           :: Norm     !! the norm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (PRESENT(PDot)) THEN
        SELECT TYPE (PDot)
        TYPE IS (tFloat)
            Norm = SQRT(PDot)
        TYPE IS (tCmplx)
            SELECT TYPE (AVec)
            TYPE IS (tCmplx)
                Norm = Norm2_Complex(N,AVec, 1_kIndex)
            END SELECT
        END SELECT
    ELSE
        SELECT TYPE (AVec)
        TYPE IS (tFloat)
            Norm = Norm2_Real(N,AVec, 1_kIndex)
        TYPE IS (tCmplx)
            Norm = Norm2_Complex(N,AVec, 1_kIndex)
        END SELECT
    END IF

    RETURN

CONTAINS

    FUNCTION Norm2_Real(N,X,INCX) RESULT(Norm2)
        !  =====================================================================
        !  -- Reference BLAS level1 routine (version 3.8.0) --
        !  -- Reference BLAS is a software package provided by Univ. of Tennessee,--
        !  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
        !     November 2017
        !  =====================================================================
        !     .. Arguments ..
        tIndex, INTENT(IN)  :: N
        tIndex, INTENT(IN)  :: INCX
        tFloat, INTENT(IN)  :: X(:)
        tFloat              :: Norm2
        !  =====================================================================
        !     .. Local Variables ..
        tFloat      :: ABSXI, SCALE, SSQ
        tIndex      :: IX
        !  =====================================================================
        !     ..
        IF (N < 1 .OR. INCX < 1) THEN
            Norm2 = ZEROR
        ELSE IF (N == 1) THEN
            Norm2 = ABS(X(1))
        ELSE
            SCALE = ZEROR
            SSQ = ONER
            !        The following loop is equivalent to this call to the LAPACK
            !        auxiliary routine:
            !        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
            DO IX = 1,1 + (N-1)*INCX,INCX
                IF (X(IX) /= ZEROR) THEN
                    ABSXI = ABS(X(IX))
                    IF (SCALE < ABSXI) THEN
                        SSQ = ONER + SSQ* (SCALE/ABSXI)**2
                        SCALE = ABSXI
                    ELSE
                        SSQ = SSQ + (ABSXI/SCALE)**2
                    END IF
                END IF
            END DO
            Norm2 = SCALE*SQRT(SSQ)
        END IF

        RETURN
        !  =====================================================================
    END FUNCTION Norm2_Real

    !*************************************************************

    FUNCTION Norm2_Complex(N,X,INCX) RESULT(Norm2)
        !  =====================================================================
        !  -- Reference BLAS level1 routine (version 3.8.0) --
        !  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
        !  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
        !     November 2017
        !  =====================================================================
        !     .. Arguments ..
        tIndex, INTENT(IN)  :: N
        tIndex, INTENT(IN)  :: INCX
        tCmplx, INTENT(IN)  :: X(:)
        tFloat              :: Norm2
        !  =====================================================================
        !     .. Local Variables ..
        tFloat      :: SCALE, SSQ ,TEMP
        tIndex      :: IX
        !  =====================================================================
        !     ..
        IF (N < 1 .OR. INCX < 1) THEN
            Norm2 = ZEROR
        ELSE
            SCALE = ZEROR
            SSQ = ONER
            !        The following loop is equivalent to this call to the LAPACK
            !        auxiliary routine:
            !        CALL ZLASSQ( N, X, INCX, SCALE, SSQ )
            DO IX = 1,1 + (N-1)*INCX,INCX
                IF (REAL(X(IX),KIND=kFloat) /= ZEROR) THEN
                    TEMP = ABS(REAL(X(IX),KIND=kFloat))
                    IF (SCALE < TEMP) THEN
                        SSQ = ONER + SSQ* (SCALE/TEMP)**2
                        SCALE = TEMP
                    ELSE
                        SSQ = SSQ + (TEMP/SCALE)**2
                    END IF
                END IF
                IF (AIMAG(X(IX)) /= ZEROR) THEN
                    TEMP = ABS(AIMAG(X(IX)))
                    IF (SCALE < TEMP) THEN
                        SSQ = ONER + SSQ* (SCALE/TEMP)**2
                        SCALE = TEMP
                    ELSE
                        SSQ = SSQ + (TEMP/SCALE)**2
                    END IF
                END IF
            END DO
            Norm2 = SCALE*SQRT(SSQ)
        END IF

        RETURN
        !  =====================================================================
    END FUNCTION Norm2_Complex

    !*************************************************************

END FUNCTION PolyNorm2

!******************************************************************************

FUNCTION PolyAbsolute(A) RESULT(AbsA)

!** PURPOSE OF THIS SUBROUTINE
    !^ To determine the absolute value of A.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: A
    tFloat                   :: AbsA

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    !

!** FLOW:

    SELECT TYPE (A)
    TYPE IS (tFloat)
        AbsA = ABS(A)
    TYPE IS (tCmplx)
        AbsA = ABS(A)
    END SELECT

    RETURN

END FUNCTION PolyAbsolute

!******************************************************************************

SUBROUTINE PolySquareRoot(BVal,AVal)

!** PURPOSE OF THIS SUBROUTINE
    !^ To compute the square root of AVal and return the result in BVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(OUT)   :: BVal
    CLASS(*), INTENT(IN)    :: AVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        SELECT TYPE (BVal)
        TYPE IS (tFloat)
            IF (AVal >= ZEROR) THEN
                BVal = SQRT(AVal)
            ELSE
                BVal = ZEROR
                ! report error
                CALL Handle_ErrLevel('PolySquareRoot', ModName, ErrWarning, &
                            'AVal is negative. Set BVal to zero.')
            END IF
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (BVal)
        TYPE IS (tCmplx)
            BVal = SQRT(AVal)
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolySquareRoot

!******************************************************************************

SUBROUTINE PolyLogarithm(BVal,AVal,Base)

!** PURPOSE OF THIS SUBROUTINE
    !^ To determine the natural logarithm of AVAl or any logarithm if the base
    !  is specified.  Return the logarithmic value in BVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(OUT)           :: BVal
    CLASS(*), INTENT(IN)            :: AVal
    CLASS(*), INTENT(IN), OPTIONAL  :: Base

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (.NOT.PRESENT(Base)) THEN
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            SELECT TYPE (BVal)
            TYPE IS (tFloat)
                IF (AVal > ZEROR) THEN
                    BVal = LOG(AVal)
                ELSE
                    BVal = -1000.0_kFloat
                    ! report error
                    CALL Handle_ErrLevel('PolyLogarithm', ModName, ErrWarning, &
                                'AVal is less than zero. Set BVal to -1000.')
                END IF
            END SELECT
        TYPE IS (tCmplx)
            SELECT TYPE (BVal)
            TYPE IS (tCmplx)
                IF (AVal /= ZEROC) THEN
                    BVal = LOG(AVal)
                ELSE
                    BVal = -(1000.0_kFloat,1000.0_kFloat)
                    ! report error
                    CALL Handle_ErrLevel('PolyLogarithm', ModName, ErrWarning, &
                                'AVal is equal to zero. Set BVal to -1000.')
                END IF
            END SELECT
        END SELECT
    ELSE
        SELECT TYPE (AVal)
        TYPE IS (tFloat)
            SELECT TYPE (BVal)
            TYPE IS (tFloat)
                SELECT TYPE (Base)
                TYPE IS (tFloat)
                    IF (AVal > ZEROR) THEN
                        IF (Base > ZEROR) THEN
                            BVal = LOG(AVal)/LOG(Base)
                        ELSE
                            BVal = -1000.0_kFloat
                            ! report error
                            CALL Handle_ErrLevel('PolyLogarithm', ModName, ErrWarning, &
                                        'Base is less than zero. Set BVal to -1000.')
                        END IF
                    ELSE
                        BVal = -1000.0_kFloat
                        ! report error
                        CALL Handle_ErrLevel('PolyLogarithm', ModName, ErrWarning, &
                                    'AVal is less than zero. Set BVal to -1000.')
                    END IF
                END SELECT
            END SELECT
        TYPE IS (tCmplx)
            SELECT TYPE (BVal)
            TYPE IS (tCmplx)
                SELECT TYPE (Base)
                TYPE IS (tCmplx)
                    IF (AVal /= ZEROC) THEN
                        IF (Base /= ZEROC) THEN
                            BVal = LOG(AVal)/LOG(Base)
                        ELSE
                            BVal = -(1000.0_kFloat,1000.0_kFloat)
                            ! report error
                            CALL Handle_ErrLevel('PolyLogarithm', ModName, ErrWarning, &
                                        'Base is equal to zero. Set BVal to -1000.')
                        END IF
                    ELSE
                        BVal = -(1000.0_kFloat,1000.0_kFloat)
                        ! report error
                        CALL Handle_ErrLevel('PolyLogarithm', ModName, ErrWarning, &
                                    'AVal is equal to zero. Set BVal to -1000.')
                    END IF
                END SELECT
            END SELECT
        END SELECT
    END IF

    RETURN

END SUBROUTINE PolyLogarithm

!******************************************************************************

SUBROUTINE PolySwapScalar(AVal,BVal)

!** PURPOSE OF THIS SUBROUTINE
    !! To swap the specified values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(INOUT) :: AVal
    CLASS(*), INTENT(INOUT) :: BVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        SELECT TYPE (BVal)
        TYPE IS (tFloat)
            CALL Swap(AVal, BVal)
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (BVal)
        TYPE IS (tCmplx)
            CALL Swap(AVal, BVal)
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolySwapScalar

!******************************************************************************

SUBROUTINE PolySwapArray1D(AVal,BVal,N)

!** PURPOSE OF THIS SUBROUTINE
    !! To swap the specified values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: N
    CLASS(*), INTENT(INOUT) :: AVal(N)
    CLASS(*), INTENT(INOUT) :: BVal(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (AVal)
    TYPE IS (tFloat)
        SELECT TYPE (BVal)
        TYPE IS (tFloat)
            CALL Swap(AVal(1:N), BVal(1:N))
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (BVal)
        TYPE IS (tCmplx)
            CALL Swap(AVal(1:N), BVal(1:N))
        END SELECT
    END SELECT

    RETURN

END SUBROUTINE PolySwapArray1D

!******************************************************************************

FUNCTION PolyCompare(A,OP,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE
    !^ To compare A and B.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),    INTENT(IN) :: A
    tCharLen(2), INTENT(IN) :: OP
    CLASS(*),    INTENT(IN) :: B
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    SELECT TYPE (A)
    TYPE IS (tFloat)
        SELECT TYPE (B)
        TYPE IS (tFloat)
            SELECT CASE (OP)
            CASE ('EQ')
                Flag = (A == B)
            CASE ('NE')
                Flag = (A /= B)
            CASE ('GT')
                Flag = (A > B)
            CASE ('GE')
                Flag = (A >= B)
            CASE ('LT')
                Flag = (A < B)
            CASE ('LE')
                Flag = (A <= B)
            END SELECT
        END SELECT
    TYPE IS (tCmplx)
        SELECT TYPE (B)
        TYPE IS (tCmplx)
            SELECT CASE (OP)
            CASE ('EQ')
                Flag = (A == B)
            CASE ('NE')
                Flag = (A /= B)
            CASE ('GT')
                Flag = (ABS(A) > ABS(B))
            CASE ('GE')
                Flag = (ABS(A) >= ABS(B))
            CASE ('LT')
                Flag = (ABS(A) < ABS(B))
            CASE ('LE')
                Flag = (ABS(A) <= ABS(B))
            END SELECT
        END SELECT
    END SELECT

    RETURN

END FUNCTION PolyCompare

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           TYPE-CHECKING ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION IsPolyTypeValidI(A) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE
    !^ To check whether given polymorphic input data has valid type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: A        !! input value
    tLogical                :: Valid    !! true if type of the input is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! check whether A type is floating-point or not
    SELECT TYPE (A)
    TYPE IS (tFloat)
        Valid = TrueVal
    TYPE IS (tCmplx)
        Valid = TrueVal
    CLASS DEFAULT
        Valid = FalseVal
    END SELECT

    RETURN

END FUNCTION IsPolyTypeValidI

!******************************************************************************

FUNCTION IsPolyTypeValidII(A,B) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE
    !^ To check whether given polymorphic input data have the same type and valid.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: A        !! input value
    CLASS(*), INTENT(IN)    :: B        !! input value
    tLogical                :: Valid    !! true if the input have the same type and their type is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! check type validity
    IF (IsPolyTypeValidI(A).AND.IsPolyTypeValidI(B)) THEN
        Valid = SAME_TYPE_AS(A,B)
    ELSE
        Valid = FalseVal
    END IF

    RETURN

END FUNCTION IsPolyTypeValidII

!******************************************************************************

FUNCTION IsPolyTypeValidIII(A,B,C) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE
    !^ To check whether given polymorphic input data have the same type and valid.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: A        !! input value
    CLASS(*), INTENT(IN)    :: B        !! input value
    CLASS(*), INTENT(IN)    :: C        !! input value
    tLogical                :: Valid    !! true if all three input have the same type and their type is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! check type validity
    IF (IsPolyTypeValidI(A).AND.IsPolyTypeValidI(B).AND.IsPolyTypeValidI(C)) THEN
        Valid = SAME_TYPE_AS(A,B).AND.SAME_TYPE_AS(B,C)
    ELSE
        Valid = FalseVal
    END IF

    RETURN

END FUNCTION IsPolyTypeValidIII

!******************************************************************************

FUNCTION IsPolyTypeValidIV(A,B,C,D) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE
    !^ To check whether given polymorphic input data have the same type and valid.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: A        !! input value
    CLASS(*), INTENT(IN)    :: B        !! input value
    CLASS(*), INTENT(IN)    :: C        !! input value
    CLASS(*), INTENT(IN)    :: D        !! input value
    tLogical                :: Valid    !! true if all four input have the same type and their type is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! check type validity
    IF (IsPolyTypeValidI(A).AND.IsPolyTypeValidI(B).AND.IsPolyTypeValidI(C).AND.IsPolyTypeValidI(D)) THEN
        Valid = SAME_TYPE_AS(A,B).AND.SAME_TYPE_AS(B,C).AND.SAME_TYPE_AS(C,D)
    ELSE
        Valid = FalseVal
    END IF

    RETURN

END FUNCTION IsPolyTypeValidIV

!******************************************************************************

FUNCTION IsPolyTypeValidV(A,B,C,D,E) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE
    !^ To check whether given polymorphic input data have the same type and valid.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: A        !! input value
    CLASS(*), INTENT(IN)    :: B        !! input value
    CLASS(*), INTENT(IN)    :: C        !! input value
    CLASS(*), INTENT(IN)    :: D        !! input value
    CLASS(*), INTENT(IN)    :: E        !! input value
    tLogical                :: Valid    !! true if all five input have the same type and their type is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! check type validity
    IF (IsPolyTypeValidI(A).AND.IsPolyTypeValidI(B).AND.IsPolyTypeValidI(C).AND. &
        IsPolyTypeValidI(D).AND.IsPolyTypeValidI(E)) THEN
        Valid = SAME_TYPE_AS(A,B).AND.SAME_TYPE_AS(B,C).AND.SAME_TYPE_AS(C,D).AND.SAME_TYPE_AS(D,E)
    ELSE
        Valid = FalseVal
    END IF

    RETURN

END FUNCTION IsPolyTypeValidV

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!               MEMORY-HANDLING ROUTINES FOR ALLOCATABLE ENTITIES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE AllocatePolyAllocatable(A, Mold)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate memory of an allocatable object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: A
    CLASS(*),              INTENT(IN)       :: Mold !! mold

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ALLOCATE(A, MOLD=Mold)

    RETURN

END SUBROUTINE AllocatePolyAllocatable

!******************************************************************************

SUBROUTINE AllocatePolyArray1D(Array, Size, Mold)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate memory of array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: Array(:)     !! array
    tIndex,                INTENT(IN)       :: Size         !! size of array
    CLASS(*),              INTENT(IN)       :: Mold         !! mold

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: ASize(1)

!** FLOW:

    ASize = Size
    CALL MemAlloc(Array, Mold, ASize)

    RETURN

END SUBROUTINE AllocatePolyArray1D

!******************************************************************************

SUBROUTINE AllocatePolyArray2D(Array, Size1, Size2, Mold)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate memory of array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: Array(:,:)   !! array
    tIndex,                INTENT(IN)       :: Size1        !! size of dimension 1 of array
    tIndex,                INTENT(IN)       :: Size2        !! size of dimension 2 of array
    CLASS(*),              INTENT(IN)       :: Mold         !! mold

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: ASize(2)

!** FLOW:

    ASize(1) = Size1
    ASize(2) = Size2
    CALL MemAlloc(Array, Mold, ASize)

    RETURN

END SUBROUTINE AllocatePolyArray2D

!******************************************************************************

SUBROUTINE ResizePolyArray1D(Array, NewSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To re-allocate array and preserve data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: Array(:)     !! array
    tIndex,                INTENT(IN)       :: NewSize      !! new size of array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: ASize(1)

!** FLOW:

    ASize(1) = NewSize
    CALL MemResize(Array, ASize)

    RETURN

END SUBROUTINE ResizePolyArray1D

!******************************************************************************

SUBROUTINE ResizePolyArray2D(Array, NewSize1, NewSize2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To re-allocate array and preserve data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), ALLOCATABLE, INTENT(INOUT)    :: Array(:,:)   !! array
    tIndex,                INTENT(IN)       :: NewSize1     !! new size of first dimension of array
    tIndex,                INTENT(IN)       :: NewSize2     !! new size of second dimension of array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: ASize(2)

!** FLOW:

    ASize(1) = NewSize1
    ASize(2) = NewSize2
    CALL MemResize(Array, ASize)

    RETURN

END SUBROUTINE ResizePolyArray2D

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!               MEMORY-HANDLING ROUTINES FOR POINTER ENTITIES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#ifdef  __INTEL_COMPILER

SUBROUTINE PolyAllocatePointer(Ptr, Mold)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate memory of pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: Ptr          !! pointer
    CLASS(*),          INTENT(IN)       :: Mold         !! mold

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ALLOCATE(Ptr, MOLD=Mold)

    RETURN

END SUBROUTINE PolyAllocatePointer

!******************************************************************************

SUBROUTINE PolyAllocatePointer1D(Ptr, Size, Mold)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate memory of pointer array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: Ptr(:)       !! pointer
    tIndex,            INTENT(IN)       :: Size         !! size of pointer
    CLASS(*),          INTENT(IN)       :: Mold         !! mold

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: ASize(1)

!** FLOW:

    ASize = Size
    CALL MemAllocPtr(Ptr, Mold, ASize)

    RETURN

END SUBROUTINE PolyAllocatePointer1D

!******************************************************************************

SUBROUTINE PolyAllocatePointer2D(Ptr, Size1, Size2, Mold)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To allocate memory of pointer array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: Ptr(:,:)     !! pointer
    tIndex,            INTENT(IN)       :: Size1        !! size of dimension 1 of pointer
    tIndex,            INTENT(IN)       :: Size2        !! size of dimension 2 of pointer
    CLASS(*),          INTENT(IN)       :: Mold         !! mold

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: ASize(2)

!** FLOW:

    ASize(1) = Size1
    ASize(2) = Size2
    CALL MemAllocPtr(Ptr, Mold, ASize)

    RETURN

END SUBROUTINE PolyAllocatePointer2D

!******************************************************************************

SUBROUTINE PolyResizePointer1D(Ptr,NewSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To re-allocate pointer array and preserve data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: Ptr(:)       !! pointer
    tIndex,            INTENT(IN)       :: NewSize      !! new size of pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: ASize(1)

!** FLOW:

    ASize(1) = NewSize
    CALL MemResizePtr(Ptr, ASize)

    RETURN

END SUBROUTINE PolyResizePointer1D

!******************************************************************************

SUBROUTINE PolyResizePointer2D(Ptr,NewSize1,NewSize2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To re-allocate pointer array and preserve data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), POINTER, INTENT(INOUT)    :: Ptr(:,:)     !! pointer
    tIndex,            INTENT(IN)       :: NewSize1     !! new size of first dimension of pointer
    tIndex,            INTENT(IN)       :: NewSize2     !! new size of second dimension of pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: ASize(2)

!** FLOW:

    ASize(1) = NewSize1
    ASize(2) = NewSize2
    CALL MemResizePtr(Ptr, ASize)

    RETURN

END SUBROUTINE PolyResizePointer2D

#endif
!******************************************************************************

END MODULE MBase_PolyFloatNum

!******************************************************************************
