
MODULE MClass_MathExpParser

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *MathExpParser* derived type and its related routines.
!   The *MathExpParser* type is a parser type that can be used to parse and evaluate
!   mathematical expressions.  It is intended for applications where a set of
!   mathematical expressions is specified at runtime and is then evaluated for
!   a large number of variable values.  This is done by  compiling the set of
!   function strings into byte code, which is interpreted very efficiently for
!   the various variable values.  The evaluation is straightforward and no
!   recursions are done (uses stack arithmetic). <br>
!   This module is a modernized/re-engineered version of the *fParser* module
!   of the *FEATFLOW2* program [1].  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://wwwold.mathematik.tu-dortmund.de/~featflow/en/software/featflow2.html">
!       The software package FEATFLOW2</a> <br>

    !** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_MemHandlers
    USE MBase_SIntUtil,     ONLY: ToChar   => ToDecStrSigned
    USE MClass_StackInt32,  ONLY: StackI4B => StackInt32

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: MathExpParser
    ! procedure
    PUBLIC :: GetErrorMessage

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    !--------------------------------------------------------------------------
    !-----                  global constants for parser                   -----
    !--------------------------------------------------------------------------
    ! name of module
    tCharStar, PARAMETER        :: ModName = 'MClass_MathExpParser'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,  PARAMETER         :: MsgLen = 128
    ! constant for infinity
    tFloat,   PARAMETER         :: SYS_INFINITY_kFloat  = HUGE(1.0_kFloat)
    !% Length of string
    tSInt32,  PARAMETER, PUBLIC :: FPAR_STR_LEN    = 2048
    !% Maximum number of predefined/user-defined constants
    tSInt32,  PARAMETER, PUBLIC :: FPAR_MAX_CONSTS = 128
    !% Maximum number of predefined/user-defined expressions
    tSInt32,  PARAMETER, PUBLIC :: FPAR_MAX_EXPRS  = 128
    !% Length of constant name
    tSInt32,  PARAMETER, PUBLIC :: FPAR_CONST_LEN  = 32
    !% Length of expression name
    tSInt32,  PARAMETER, PUBLIC :: FPAR_EXPR_LEN   = 32
    !% Length of function name
    tSInt32,  PARAMETER, PUBLIC :: FPAR_FUNC_LEN   = 32
    !% Length of variable name
    tSInt32,  PARAMETER, PUBLIC :: FPAR_VAR_LEN    = 32
    !--------------------------------------------------------------------------
    !-----                  types for parser expressions                  -----
    !--------------------------------------------------------------------------
    !% Constant
    tSInt32,  PARAMETER, PUBLIC :: FPAR_CONSTANT   = 1
    !% Expression
    tSInt32,  PARAMETER, PUBLIC :: FPAR_EXPRESSION = 2
    !% Functions (including symbolic variables)
    tSInt32,  PARAMETER, PUBLIC :: FPAR_FUNCTION   = 3
    !--------------------------------------------------------------------------
    !-----                      keywords for parser                       -----
    !--------------------------------------------------------------------------
    tSInt8,   PARAMETER ::    cImmed       =  1, &
                              cJump        =  2, &
                              cNeg         =  3, &
                              cDeg         =  4, &
                              cRad         =  5, &
                              cAdd         =  6, & ! <-- first dyadic operator: A .OP. B
                              cSub         =  7, &
                              cMul         =  8, &
                              cDiv         =  9, &
                              cMod         = 10
    tSInt8,   PARAMETER ::    cPow         = 11, &
                              cNEqual      = 12, & ! NOTE: != must be prior to =
                              cEqual       = 13, &
                              cLessOrEq    = 14, & ! NOTE: <= must be prior to <
                              cLess        = 15, &
                              cGreaterOrEq = 16, & ! NOTE: >= must be prior to >
                              cGreater     = 17, &
                              cNot         = 18, &
                              cAnd         = 19, &
                              cOr          = 20    ! --> last dyadic operator: A.OP. B
    tSInt8,   PARAMETER ::    cIf          = 21, & ! <-- if-then-else
                              cMin         = 22, & ! <-- first dyadic operator: .OP.(A,B)
                              cMax         = 23, &
                              cRrand       = 24, &
                              cCmplx       = 25, &
                              cAtan2       = 26, & ! --> last dyadic operator: .OP.(A,B)
                              cAbs         = 27, & ! <-- monadic operator: .OP.(A)
                              cAnint       = 28, &
                              cAint        = 29, &
                              cExp         = 30, &
                              cLog10       = 31, &
                              cLog         = 32, &
                              cSqrt        = 33, &
                              cCeil        = 34, &
                              cFloor       = 35
    tSInt8,   PARAMETER ::    cAsinh       = 36, &
                              cAsin        = 37, &
                              cSinh        = 38, &
                              cSin         = 39, &
                              cACosh       = 40, &
                              cAcos        = 41, &
                              cCosh        = 42, &
                              cCos         = 43
    tSInt8,   PARAMETER ::    cAtanh       = 44, &
                              cAtan        = 45, &
                              cTanh        = 46, &
                              cTan         = 47, &
                              cAcoth       = 48, &
                              cAcot        = 49, &
                              cCoth        = 50, &
                              cCot         = 51
    tSInt8,   PARAMETER ::    cAsech       = 52, &
                              cAsec        = 53, &
                              cSech        = 54, &
                              cSec         = 55, &
                              cAcsch       = 56, &
                              cAcsc        = 57, &
                              cCsch        = 58, &
                              cCsc         = 59, &
                              cReal        = 60, &
                              cImag        = 61, &
                              cConj        = 62, &
                              cSign        = 63, & ! --> last monadic operator: .OP.(A)
                              VarBegin     = 64
    !--------------------------------------------------------------------------
    !-----                  symbols for parser operands                   -----
    !--------------------------------------------------------------------------
    tCharParam  :: Ops(cAdd:cOr) = ['+ ', &
                                    '- ', &
                                    '* ', &
                                    '/ ', &
                                    '% ', &
                                    '^ ', &
                                    '!=', &
                                    '= ', &
                                    '<=', &
                                    '< ', &
                                    '>=', &
                                    '> ', &
                                    '! ', &
                                    '& ', &
                                    '| ']
    !--------------------------------------------------------------------------
    !-----                  function names for parser                     -----
    !--------------------------------------------------------------------------
    tCharParam  :: Funcs(cIf:cSign) = ['if   ', &
                                       'min  ', &
                                       'max  ', &
                                       'rrand', &
                                       'cmplx', &
                                       'atan2', &
                                       'abs  ', &
                                       'anint', &
                                       'aint ', &
                                       'exp  ', &
                                       'log10', &
                                       'log  ', &
                                       'sqrt ', &
                                       'ceil ', &
                                       'floor', &
                                       'asinh', &
                                       'asin ', &
                                       'sinh ', &
                                       'sin  ', &
                                       'acosh', &
                                       'acos ', &
                                       'cosh ', &
                                       'cos  ', &
                                       'atanh', &
                                       'atan ', &
                                       'tanh ', &
                                       'tan  ', &
                                       'acoth', &
                                       'acot ', &
                                       'coth ', &
                                       'cot  ', &
                                       'asech', &
                                       'asec ', &
                                       'sech ', &
                                       'sec  ', &
                                       'acsch', &
                                       'acsc ', &
                                       'csch ', &
                                       'csc  ', &
                                       'real ', &
                                       'imag ', &
                                       'conj ', &
                                       'sign ']
    !--------------------------------------------------------------------------
    !-----              predefined constant names for parser              -----
    !--------------------------------------------------------------------------
    tCharLen(FPAR_CONST_LEN), PARAMETER :: PredefinedConsts(3) = ['pi        ', &
                                                                  'exp       ', &
                                                                  'infty     ']
    !--------------------------------------------------------------------------
    !-----      predefined real-valued constant values for parser         -----
    !--------------------------------------------------------------------------
    tFloat,                   PARAMETER :: PredefinedConstVals(3) = [      &
                3.141592653589793115997963468544185161590576171875_kFloat,    &
                2.718281828459045090795598298427648842334747314453125_kFloat, &
                SYS_INFINITY_kFloat]
    !--------------------------------------------------------------------------
    !-----              predefined expression names for parser            -----
    !--------------------------------------------------------------------------
    tCharLen(FPAR_CONST_LEN), PARAMETER :: PredefinedExpressions(1)    = ['null      ']
    !--------------------------------------------------------------------------
    !-----               predefined expressions for parser                -----
    !--------------------------------------------------------------------------
    tCharLen(FPAR_CONST_LEN), PARAMETER :: PredefinedExpressionVals(1) = ['0         ']
    !--------------------------------------------------------------------------
    !-----                        types of error                          -----
    !--------------------------------------------------------------------------
    tSInt32,  PARAMETER :: No_Error              = 0
    tSInt32,  PARAMETER :: DivisionByZero        = 1
    tSInt32,  PARAMETER :: IllegalArgument       = 2
    tSInt32,  PARAMETER :: ComplexValuedArgument = 3
    tSInt32,  PARAMETER :: IllegalOperation      = 4

!** DERIVED TYPE DEFINITIONS
    !> *ParserComponent* is a derived type used to store the bytecode of
    !   the *MathExpParser* type for one component.  It is used to handle
    !   one function string at a time.  This is a private type.
    TYPE ParserComponent
        PRIVATE
        !% Size of bytecode
        tSInt32         :: iByteCodeSize = 0
        !% Size of immediates
        tSInt32         :: iImmedSize = 0
        !% Stack size
        tSInt32         :: iStackSize = 0
        !% Stack pointer
        tSInt32         :: iStackPtr = 0
        !% Use degree conversion DEG <-> RAD for some functions
        tLogical        :: bUseDegreeConversion = FalseVal
        !% Is vectorizable
        tLogical        :: bIsVectorizable = TrueVal
        !% Is complex-valued
        tLogical        :: bIsComplex = FalseVal
        !% Bytecode
        tSInt8, POINTER :: iByteCode(:) => NULL()
        !% Immediates (real-valued)
        tFloat, POINTER :: dImmed(:)    => NULL()
        !% Immediates (complex-valued)
        tCmplx, POINTER :: zImmed(:)    => NULL()
    END TYPE ParserComponent
    !> *MathExpParser* is a parser type that can be used to parse and evaluate
    !   mathematical expressions.
    TYPE MathExpParser
        PRIVATE
        !> Array of function parser components.
        !  Each component is used to handle one function string at a time.
        TYPE(ParserComponent), POINTER  :: rComp(:)     => NULL()
        !% Array of function names corresponding to the individual components.
        tCharLen(:),           POINTER  :: SCompName(:) => NULL()
        !% Number of parser components
        tSInt32                         :: NComp = 0
        !% Maximum number of components
        tSInt32                         :: MaxNComp = 0
        ! Global number of predefined/user-defined constants
        tSInt32                         :: nConstants = 0
        ! Global number of predefined/user-defined Expressions
        tSInt32                         :: nExpressions = 0
        ! Global constant names for parser
        tCharLen(FPAR_CONST_LEN)        :: cConstantName(FPAR_MAX_CONSTS)  = ''
        ! Global constant values for parser (real-valued)
        tFloat                           :: dConstantValue(FPAR_MAX_CONSTS) = Zero
        ! Global constant values for parser (complex-valued)
        tCmplx                           :: zConstantValue(FPAR_MAX_CONSTS) = (Zero, Zero)
        ! Global expression name for parser
        tCharLen(FPAR_EXPR_LEN)         :: cExpressionName(FPAR_MAX_CONSTS) = ''
        ! Global expression string for parser
        tCharLen(FPAR_STR_LEN)          :: cExpressionString(FPAR_MAX_EXPRS)
    CONTAINS
        !----------------------------------------------------------------------
        !-----                  Private Procedures                        -----
        !----------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: Parser_DefineConstantDble
        PROCEDURE, PRIVATE  :: Parser_DefineConstantCmpl
        PROCEDURE, PRIVATE  :: Parser_DefineExpression
        PROCEDURE, PRIVATE  :: Parser_ParseFunctionByName
        PROCEDURE, PRIVATE  :: Parser_ParseFunctionByNumber
        PROCEDURE, PRIVATE  :: Parser_EvalFuncScDbleByName
        PROCEDURE, PRIVATE  :: Parser_EvalFuncScDbleByNumber
        PROCEDURE, PRIVATE  :: Parser_EvalFuncScCmplByName
        PROCEDURE, PRIVATE  :: Parser_EvalFuncScCmplByNumber
        PROCEDURE, PRIVATE  :: Parser_PrintByteCodeByName
        PROCEDURE, PRIVATE  :: Parser_PrintByteCodeByNumber
        !----------------------------------------------------------------------
        !-----                   Public Procedures                        -----
        !----------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        ! **Purpose**:  To initialize the function parser with the specified number of functions. <br>
        !  **Usage**: <br>
        !   --->    CALL Parser%Create(nFunc) <br>
        PROCEDURE   :: Create               => Parser_Create
        !> **Type-Bound Subroutine**: Destroy <br>
        ! **Purpose**:  To release memory occupied by the function parser and its components. <br>
        !  **Usage**: <br>
        !   --->    CALL Parser%Destroy() <br>
        PROCEDURE   :: Destroy              => Parser_Destroy
        !> **Type-Bound Subroutine**: DefineConstant <br>
        ! **Purpose**:  To define a new user-defined constant by specifying the name and value of
        !       the new constant. <br>
        !  **Usage**: <br>
        !   --->    CALL Parser%DefineConstant(ConstName, ConstVal) <br>
        GENERIC     :: DefineConstant       => Parser_DefineConstantDble,      &
                                               Parser_DefineConstantCmpl
        !> **Type-Bound Subroutine**: DefineExpression <br>
        ! **Purpose**:  To define a new user-defined expression by specifying the name and value
        !       (expression string) of the new expression. <br>
        !  **Usage**: <br>
        !   --->    CALL Parser%DefineExpression(ExpName, ExpString) <br>
        GENERIC     :: DefineExpression     => Parser_DefineExpression
        !> **Type-Bound Subroutine**: ParseFunction <br>
        ! **Purpose**:  To parse a string expressing mathematical expression and compile
        !       it into bytecode. <br>
        !  **Usage**: <br>
        !   ! parse function by a name as a function identifier <br>
        !   --->    CALL Parser%ParseFunction(FuncName, FuncString, VarArr) <br>
        !   --->    CALL Parser%ParseFunction(FuncName, FuncString, VarArr, bUseDegrees=.TRUE., iComp=FuncNum) <br>
        !   ! parse function by a number as a function identifier <br>
        !   --->    CALL Parser%ParseFunction(FuncNum, FuncString, VarArr) <br>
        !   --->    CALL Parser%ParseFunction(FuncNum, FuncString, VarArr, bUseDegrees=.TRUE.) <br>
        !  **Note**: The *VarArr* is an array of strings containing the variable names. <br>
        GENERIC     :: ParseFunction        => Parser_ParseFunctionByName,     &
                                               Parser_ParseFunctionByNumber
        !> **Type-Bound Subroutine**: EvalFunction <br>
        ! **Purpose**:  To evaluate the parsed function by specifying its identifier. <br>
        !  **Usage**: <br>
        !   ! evaluate function by a name as a function identifier <br>
        !   --->    CALL Parser%EvalFunction(FuncName, VarArr, ErrCode, ResVal) <br>
        !   ! evaluate function by a number as a function identifier <br>
        !   --->    CALL Parser%EvalFunction(FuncNum, FVarArr, ErrCode, ResVal) <br>
        !  **Note**: The *VarArr* is an array of real (or complex) numbers containing the variable
        !       values, which correspond to the variable names specified when parsing the function. <br>
        GENERIC     :: EvalFunction         => Parser_EvalFuncScDbleByName,    &
                                               Parser_EvalFuncScDbleByNumber,  &
                                               Parser_EvalFuncScCmplByName,    &
                                               Parser_EvalFuncScCmplByNumber
        !> **Type-Bound Subroutine**: PrintByteCode <br>
        ! **Purpose**:  To print the compiled bytecode stack of the parsed function by
        !       specifying its identifier and the output unit number. <br>
        !  **Usage**: <br>
        !   ! print the bytecode stack by a name as a function identifier <br>
        !   --->    CALL Parser%PrintByteCode(FuncName, OutUnit) <br>
        !   ! print the bytecode stack by a number as a function identifier <br>
        !   --->    CALL Parser%PrintByteCode(FuncNum, OutUnit) <br>
        !  **Note**: This method provides very technical information and is intended to be
        !       used for debugging purpose only.  <br>
        GENERIC     :: PrintByteCode        => Parser_PrintByteCodeByName,     &
                                               Parser_PrintByteCodeByNumber
        !> **Type-Bound Function**: GetFunctionNumber <br>
        ! **Purpose**:  To return a number used as a function identifier by
        !       specifying a name as a function identifier. <br>
        !  **Usage**: <br>
        !   --->    FuncNum = Parser%GetFunctionNumber(FuncName) <br>
        PROCEDURE   :: GetFunctionNumber    => Parser_GetFunctionNumber
        !% a final subroutine supposedly automatically called when the object is out of scope
        FINAL       :: Parser_Finalize
    END TYPE MathExpParser

!** INTERFACE DEFINITIONS:
    INTERFACE GetErrorMessage
        !^ **Function Interface**: GetErrorMessage <br>
        ! **Purpose**:  To return an error message corresponding to the specified error code. <br>
        !  **Usage**: <br>
        !   --->    ErrMsg = Parser_ErrorMsg(ErrCode) <br>
        MODULE PROCEDURE Parser_ErrorMsg
    END INTERFACE
    INTERFACE ToLowerCase
        MODULE PROCEDURE ToLower_Replace
        MODULE PROCEDURE ToLower_Copy
    END INTERFACE
    INTERFACE MemFree
        MODULE PROCEDURE FreeParseCompPtr1D
    END INTERFACE
    INTERFACE MemAlloc
        MODULE PROCEDURE AllocateParseCompPtr1D
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
!    tCharAlloc  :: SubName  ! routine name
    tCharAlloc  :: ErrMsg   ! error message

    CONTAINS

SUBROUTINE Module_Documentation()

    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !
    ! Note: the following documentation is from "fParser" module in "FeatFlow2" program
    !
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !!##############################################################################
    !!# ****************************************************************************
    !!# <name> fparser </name>
    !!# ****************************************************************************
    !!#
    !!# <purpose>
    !!#
    !!# This public domain function parser module is intended for applications
    !!# where a set of mathematical Expressions is specified at runtime and is
    !!# then evaluated for a large number of variable values. This is done by
    !!# compiling the set of function strings into byte code, which is interpreted
    !!# very efficiently for the various variable values. The evaluation is
    !!# straightforward and no recursions are done (uses stack arithmetic).
    !!#
    !!# ------------------------------------------------------------------------ \\
    !!# Copyright notice \\
    !!# ------------------------------------------------------------------------ \\
    !!# (a) This module is based on the "Fortran 90 function parser V1.1"
    !!#     written by Roland Schmehl < Roland.Schmehl@mach.uni-karlsruhe.de >
    !!#     The original Fortran90 source code is available from:
    !!#     http://itsextern.its.uni-karlsruhe.de/~schmehl/functionparser.html
    !!#
    !!#     However the "Fortran 90 function parser V1.1" only recognises the
    !!#     (single argument) Fortran 90 intrinsic functions abs, exp, log10,
    !!#     log, sqrt, sinh, cosh, tanh, sin, cos, tan, asin, acos, atan.
    !!#
    !!#     The function parser concept is based on a C++ class library written
    !!#     by Warp < warp@iki.fi > available from:
    !!#     http://warp.povusers.org/MathExpParser/
    !!#
    !!# (b) This FParser module is an extension of the `Fortran 90 function parser
    !!#     V1.1` which implements most of the features available in the `function
    !!#     parser library for C++ V2.8` written by Warp. The optimizer included
    !!#     in the C++ library and the recursive evaluation of functions by means
    !!#     of eval(...) is not implemented in this version.
    !!#
    !!# ------------------------------------------------------------------------ \\
    !!# Basic usage \\
    !!# ------------------------------------------------------------------------ \\
    !!#
    !!# Step 0 - Module Import \\
    !!# ---------------------- \\
    !!# In all program units where you want to use the function parser procedures
    !!# and variables you must import the module by:
    !!#
    !!# <code>
    !!#  use fparser
    !!# </code>
    !!#
    !!# This command imports only 6 public names: Parser_Create, Parser_Release,
    !!# Parser_ParseFunction, Parser_EvalFunction, Parser_ErrorMsg and EvalErrType
    !!# which are explained in the following. The remaInder of the
    !!# module is hidden to the calling program.
    !!#
    !!# Step 1 - Initialization \\
    !!# ----------------------- \\
    !!# The parser module has to be initialized for the simultaneous evaluation of
    !!# n functions by calling the module subroutine initp one time in your Fortran
    !!# code:
    !!#
    !!# <code>
    !!#  call Parser_Create(Parser, n)
    !!# </code>
    !!#
    !!# This ParseCompPtr1Ds i=1,...,n internal data structures used by the byte-compiler
    !!# and subsequently by the bytecode-interpreter in the bytecode object Comp.
    !!#
    !!# Step 2 - Function parsing \\
    !!# ------------------------- \\
    !!# The i-th function string FuncStr is parsed (checked and compiled) into the
    !!# i-th bytecode by calling the module subroutine parsef:
    !!#
    !!# <code>
    !!#  call Parser_ParseFunction(Parser, i, FuncStr, Var)
    !!# </code>
    !!#
    !!# The variable names as they appear in the string FuncStr have to be passed
    !!# in the one-dimensional string array Var (zero size of Var is acceptable).
    !!# The number of variables is implicitly passed by the dimension of this array.
    !!# For some notes on the syntax of the function string see below.
    !!#
    !!# Step 3 - Function evaluation \\
    !!# ---------------------------- \\
    !!# The i-th function value is evaluated for a specific set of variable values
    !!# by calling the module function evalf:
    !!#
    !!# <code>
    !!#  a = Parser_EvalFunction(Parser, i, Val)
    !!# </code>
    !!#
    !!# The variable values are passed in the one-dimensional array Val which must
    !!# have the same dimension as array Var.
    !!#
    !!# ------------------------------------------------------------------------ \\
    !!# Error handling \\
    !!# ------------------------------------------------------------------------ \\
    !!#
    !!# An error in the function parsing step leads to a detailed error message
    !!# (Type and position of error) and program termination.
    !!#
    !!# An error during function evaluation returns a function value of 0.0 and
    !!# sets the error flag EvalErrTYPE(part of the MathExpParser derived type) to
    !!# a value > 0 (EvalErrType = 0 Indicates no error). An error message from the
    !!# bytecode-interpreter can be obtained by calling the character function
    !!# Parser_ErrorMsg(Parser) with the parser object as an argument.
    !!#
    !!# ------------------------------------------------------------------------ \\
    !!# Function string syntax \\
    !!# ------------------------------------------------------------------------ \\
    !!#
    !!# Although they have to be passed as array elements of the same declared
    !!# length (Fortran 90 restriction), the variable names can be of arbitrary
    !!# actual length for the parser. Parsing for variables is case sensitive.
    !!#
    !!# The syntax of the function string is similar to the Fortran convention.
    !!# Mathematical Operators recognized are +, -, *, /, %, ** or alternatively
    !!# ^, whereas symbols for brackets must be (), [] or {}. Note that the
    !!# parser does not check if, e.g. ( is closed by ) or ]. At the moment,
    !!# different brackets may be used only to improve readability of the function
    !!# string.
    !!#
    !!# Operations are evaluated in the correct order:
    !!#
    !!# <verb>
    !!#  ()             Expressions in brackets first
    !!#  -A             unary minus (or plus)
    !!#  A**B A^B       exponentiation (A raised to the power B)
    !!#  A*B  A/B  A%B  multiplication, division and modulo
    !!#  A+B  A-B       addition and subtraction
    !!#  A=B  A!=B  A < B  A <= B  A > B  A >= B
    !!#                 comparison between A and B (result is either 0 or 1)
    !!#  A&B            result is 1 if int(A) and int(B) differ from 0, else 0.
    !!#  A|B            result is 1 if int(A) or int(B) differ from 0, else 0.
    !!# </verb>
    !!#
    !!# The function string can contain integer or real constants. To be recognized
    !!# as explicit constants these must conform to the format
    !!#
    !!# <verb>
    !!#  [+|-][nnn][.nnn][e|E|d|D[+|-]nnn]
    !!# </verb>
    !!#
    !!# where nnn means any number of digits. The mantissa must contain at least
    !!# one digit before or following an optional decimal point. Valid exponent
    !!# identifiers are 'e', 'E', 'd' or 'D'. If they appear they must be followed
    !!# by a valid exponent!
    !!#
    !!# Note that the function parser is case insensitive.
    !!# The following mathematical functions are supported
    !!#
    !!# <verb>
    !!# abs(A)    : Absolute value of A. If A is negative, returns -A otherwise
    !!#             returns A.
    !!# acos(A)   : Arc-cosine of A. Returns the angle, measured in radians,
    !!#             whose cosine is A.
    !!# acosh(A)  : Same as acos() but for hyperbolic cosine.
    !!# acot(A)   : Arc-cotangent of A (equivalent to 1/atan(A)).
    !!# acoth(A)  : Same as acot() but for hyperbolic cotangent.
    !!# acsc(A)   : Same as csc(A) but for hyperbolic cosecant.
    !!# aint(A)   : Truncate A to a whole number
    !!# anint(A)  : Rounds A to the closest integer. 0.5 is rounded to 1.
    !!# asec(A)   : Same as sec(A) but for hyperbolic secant.
    !!# asin(A)   : Arc-sine of A. Returns the angle, measured in radians, whose
    !!#             sine is A.
    !!# asinh(A)  : Same as asin() but for hyperbolic sine.
    !!# atan(A)   : Arc-tangent of (A). Returns the angle, measured in radians,
    !!#             whose tangent is (A).
    !!# atan2(A,B): Arc-tangent of A/B. The two main differences to atan() is
    !!#             that it will return the right angle depending on the signs of
    !!#             A and B (atan() can only return values betwen -pi/2 and pi/2),
    !!#             and that the return value of pi/2 and -pi/2 are possible.
    !!# atanh(A)  : Same as atan() but for hyperbolic tangent.
    !!# ceil(A)   : Ceiling of A. Returns the smallest integer greater than A.
    !!#             Rounds up to the next higher integer.
    !!# conj(A)   : Complex conjugate of complex number A.
    !!# cos(A)    : Cosine of A. Returns the cosine of the angle A, where A is
    !!#             measured in radians.
    !!# cosh(A)   : Same as cos() but for hyperbolic cosine.
    !!# cot(A)    : Cotangent of A (equivalent to 1/tan(A)).
    !!# coth(A)   : Same as cot() but for hyperbolic tangent.
    !!# csc(A)    : Cosecant of A (equivalent to 1/sin(A)).
    !!# exp(A)    : Exponential of A. Returns the value of e raised to the power
    !!#             A where e is the base of the natural logarithm, i.e. the
    !!#             non-repeating value approximately equal to 2.71828182846.
    !!# floor(A)  : Floor of A. Returns the largest integer less than A. Rounds
    !!#             down to the next lower integer.
    !!# if(A,B,C) : If int(A) differs from 0, the return value of this function is B,
    !!#             else C. Only the parameter which needs to be evaluated is
    !!#             evaluated, the other parameter is skipped.
    !!# imag(A)   : Imaginary part of complex number A.
    !!# log(A)    : Natural (base e) logarithm of A.
    !!# log10(A)  : Base 10 logarithm of A.
    !!# max(A,B)  : If A > B, the result is A, else B.
    !!# min(A,B)  : If A < B, the result is A, else B.
    !!# real(A)   : Real part of complex number A.
    !!# sec(A)    : Secant of A (equivalent to 1/cos(A)).
    !!# sin(A)    : Sine of A. Returns the sine of the angle A, where A is
    !!#             measured in radians.
    !!# sinh(A)   : Same as sin() but for hyperbolic sine.
    !!# sign(A)   : Sign of A.
    !!# sqrt(A)   : Square root of A. Returns the value whose square is A.
    !!# tan(A)    : Tangent of A. Returns the tangent of the angle A, where A
    !!#             is measured in radians.
    !!# tanh(A)   : Same as tan() but for hyperbolic tangent.
    !!# rrand(A,B): Reproducable pseudo-random number; B'th random number with
    !!#             Random-Seed A.
    !!# </verb>
    !!#
    !!# The parser also supports a number of standard constants in the function
    !!# string. All constants start with an underscore '_'. The following constants
    !!# are defined by default:
    !!#
    !!# <verb>
    !!# _PI       : Gives the number $pi$.
    !!# _EXP      : Gives the number $e$
    !!# _INFTY    : Gives the maximum possible number in double precision, defined
    !!#             in fsystem by SYS_INFINITY_kFloat.
    !!# </verb>
    !!#
    !!# In addition, the user can define his own global constant which are available
    !!# throughout the complete function parser as it is the case for the standard
    !!# constant defined above.
    !!#
    !!# The parser also supports user-defined Expressions which are globally
    !!# available throughout the complete function parser. All Expressions must
    !!# start with '@' to Indicate that the following Expression should be
    !!# looked-up from the list of predefined Expressions.
    !!#
    !!# The following routines can be found in this module:
    !!#
    !!# 1.) Parser_Init
    !!#     -> Initialize the sub-system for function parsers
    !!#
    !!# 2.) Parser_Done
    !!#     -> Release the sub-system for function parsers
    !!#
    !!# 3.) Parser_DefineConstant
    !!#     -> Define special constants which are available for all function parsers
    !!#
    !!# 4.) Parser_DefineExpression
    !!#     -> Define special Expressions which are available for all function parsers
    !!#
    !!# 5.) Parser_Create
    !!#     -> Create function parser
    !!#
    !!# 6.) Parser_Release
    !!#     -> Release function parser
    !!#
    !!# 7.) Parser_ParseFunction = Parser_ParseFunctionByName /
    !!#                             Parser_ParseFunctionByNumber
    !!#     -> Parse function string and compile it into bytecode
    !!#
    !!# 8.) Parser_EvalFunction = Parser_EvalFuncScDbleByName /
    !!#                            Parser_EvalFuncScDbleByNumber /
    !!#                            Parser_EvalFuncBlDbleByName /
    !!#                            Parser_EvalFuncBlDbleByNumber /
    !!#                            Parser_EvalFuncScCmplByName /
    !!#                            Parser_EvalFuncScCmplByNumber /
    !!#                            Parser_EvalFuncBlCmplByName /
    !!#                            Parser_EvalFuncBlCmplByNumber
    !!#     -> Evaluate precompiled bytecode
    !!#
    !!# 9.) Parser_ErrorMsg
    !!#     -> Get error message from function parser
    !!#
    !!# 10.) Parser_PrintByteCode = Parser_PrintByteCodeByName /
    !!#                              Parser_PrintByteCodeByNumber
    !!#      -> Print the bytecode stack (very technical!)
    !!#
    !!# 11.) Parser_ParseFileForKeyword
    !!#      -> Parse input file for keyword
    !!#
    !!# 12.) Parser_GetFunctionNumber
    !!#      -> Return the internal number of the function
    !!#
    !!# 13.) Parser_InitPerfConfig
    !!#      -> Initializes the global performance configuration
    !!#
    !!# The following internal routines can be found in this module:
    !!#
    !!# 1.) CheckSyntax
    !!#     -> Check syntax of function string before compiling bytecode
    !!#
    !!# 2.) IsOperator
    !!#     -> Return size of operator and 0 otherwise
    !!#
    !!# 3.) MathFunctionIndex
    !!#     -> Return Index of mathematical function and 0 otherwise
    !!#
    !!# 4.) MathFunctionParameters
    !!#     -> Return number of required function parameters
    !!#
    !!# 5.) ConstantIndex
    !!#     -> Return Index of predefined constant and 0 otherwise
    !!#
    !!# 6.) ExpressionIndex
    !!#     -> Return Index of predefined Expression and 0 otherwise
    !!#
    !!# 7.) VariableIndex
    !!#     -> Return Index of variable
    !!#
    !!# 8.) RemoveSpaces
    !!#     -> Remove spaces from string
    !!#
    !!# 9.) Replace
    !!#     -> Replace all appearances of one character set by another
    !!#        character set in a given string
    !!#
    !!# 10.) Compile
    !!#      -> Compile function string into bytecode
    !!#
    !!# 11.) IncStackPtr
    !!#     -> Increase stack pointer
    !!#
    !!# 12.) AddCompiledByte
    !!#     -> Add compiled byte to bytecode stack
    !!#
    !!# 13.) RemoveCompiledByte
    !!#     -> Remove last compiled byte from bytecode stack
    !!#
    !!# 14.) AddImmediate
    !!#     -> Add immediate to immediate stack
    !!#
    !!# 15.) AddFunctionOpcode
    !!#     -> Add function opcode to bytecode stack
    !!#
    !!# 16.) RealNum
    !!#     -> Get real number from string
    !!#
    !!# 17.) FunctionSize
    !!#     -> Get the total size of the function
    !!#
    !!# 18.) CompileExpression
    !!#      -> Compile ','
    !!#
    !!# 19.) CompileOr
    !!#      -> Compile '|'
    !!#
    !!# 20.) CompileAnd
    !!#      -> Compile '&'
    !!#
    !!# 21.) CompileComparison
    !!#      -> Compile '=', '<', and '>'
    !!#
    !!# 22.) CompileAddition
    !!#      -> Compile '+' and '-'
    !!#
    !!# 23.) CompileMult
    !!#      -> Compile '*', '/', and '%'
    !!#
    !!# 24.) CompileUnaryMinus
    !!#      -> Compile unary '-'
    !!#
    !!# 25.) CompilePow
    !!#      -> Compile '^'
    !!#
    !!# 26.) CompileElement
    !!#      -> Compile mathematical function, variable, constant and number
    !!#
    !!# 27.) CompileFunctionParameters
    !!#      -> Compile function parameters
    !!#
    !!# 28.) CompileIf
    !!#      -> Compile if-then-else
    !!#
    !!# 29.) EvalFunctionScDble /
    !!#      EvalFunctionScCmpl
    !!#      -> Evaluate function for scalar data (real-/complex valued)
    !!#
    !!# 30.) EvalFunctionBlDble /
    !!#      EvalFunctionBlCmpl
    !!#      -> Evaluate function for multi-component data (real-/complex valued)
    !!#
    !!# </purpose>
    !!##############################################################################

END SUBROUTINE Module_Documentation

!**************************************************************************************

SUBROUTINE Parser_Init(rfParser)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the function parser.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser object
    TYPE(MathExpParser), INTENT(INOUT)  :: rfParser
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: i

!** FLOW

    ! Initialize predefined constants
    DO i = LBOUND(PredefinedConsts, 1), UBOUND(PredefinedConsts, 1)
        CALL rfParser%DefineConstant(PredefinedConsts(i), PredefinedConstVals(i))
    END DO

    ! Initialize predefined Expressions
    DO i = LBOUND(PredefinedExpressions, 1), UBOUND(PredefinedExpressions, 1)
        CALL rfParser%DefineExpression(PredefinedExpressions(i), PredefinedExpressionVals(i))
    END DO

END SUBROUTINE Parser_Init

!**************************************************************************************

SUBROUTINE Parser_Done(rfParser)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To release the function parser.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser object
    TYPE(MathExpParser), INTENT(INOUT)  :: rfParser
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! Reset constants
    rfParser%nConstants     = 0
    rfParser%cConstantName  = ''
    rfParser%dConstantValue = 0.0_kFloat
    rfParser%zConstantValue = CMPLX(0.0_kFloat, 0.0_kFloat, KIND=kFloat)

    ! Reset Expressions
    rfParser%nExpressions      = 0
    rfParser%cExpressionName   = ''
    rfParser%cExpressionString = ''

    RETURN

END SUBROUTINE Parser_Done

!**************************************************************************************

SUBROUTINE Parser_DefineConstantDble(rfParser, sName, dValue)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To define a new real-valued constant.  The routine checks if the given constant
    !  is already defined.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser object
    CLASS(MathExpParser),     INTENT(INOUT) :: rfParser
    !% Name of the constant
    tCharLen(FPAR_CONST_LEN), INTENT(IN)    :: sName
    !% Value of the constant
    tFloat,                    INTENT(IN)    :: dValue

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'Parser_DefineConstantDble'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(LEN(sName))    :: sString
    tSInt32                 :: iconst

!** FLOW

    ASSOCIATE(nConstants     => rfParser%nConstants, &
              cConstantName  => rfParser%cConstantName, &
              dConstantValue => rfParser%dConstantValue, &
              zConstantValue => rfParser%zConstantValue)
        ! Check if there is enough space
        IF (nConstants < FPAR_MAX_CONSTS) THEN
            nConstants = nConstants + 1
        ELSE
            ErrMsg = 'No space left for definition of constant!'
            CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
            RETURN
        END IF

        ! Prepare constant
        CALL ToLowerCase(sName, sString)

        ! Check if constant is already defined
        DO iconst = 1, nConstants-1
            IF (cConstantName(iconst) == sString) THEN
                ! If it is already defined, then it must not have a different value
                IF (dConstantValue(iconst) /= dValue) THEN
                    ErrMsg = 'Constant is already defined with different value!'
                    CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                    RETURN
                ELSE
                    nConstants = nConstants - 1
                    RETURN
                END IF
            END IF
        END DO

        ! Apply constant value and constant name
        cConstantName(nConstants)  = sString
        dConstantValue(nConstants) = dValue
        zConstantValue(nConstants) = CMPLX(dValue, 0.0_kFloat, KIND=kFloat)
    END ASSOCIATE

    RETURN

END SUBROUTINE Parser_DefineConstantDble

!**************************************************************************************

SUBROUTINE Parser_DefineConstantCmpl(rfParser, sName, zValue)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To define a new complex-valued constant.  The routine checks if the given constant
    !  is already defined.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser object
    CLASS(MathExpParser),     INTENT(INOUT) :: rfParser
    !% Name of the constant
    tCharLen(FPAR_CONST_LEN), INTENT(IN)    :: sName
    !% Value of the constant
    tCmplx,                    INTENT(IN)    :: zValue

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'Parser_DefineConstantCmpl'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(LEN(sName))    :: sString
    tSInt32                 :: iconst

!** FLOW

    ASSOCIATE(nConstants     => rfParser%nConstants, &
              cConstantName  => rfParser%cConstantName, &
              dConstantValue => rfParser%dConstantValue, &
              zConstantValue => rfParser%zConstantValue)
        ! Check if there is enough space
        IF (nConstants < FPAR_MAX_CONSTS) THEN
            nConstants = nConstants + 1
        ELSE
            ErrMsg = 'No space left for definition of constant!'
            CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
            RETURN
        END IF

        ! Prepare constant
        CALL ToLowerCase(sName, sString)

        ! Check if constant is already defined
        DO iconst = 1, nConstants-1
            IF (cConstantName(iconst) == sString) THEN
                ! If it is already defined, then it must not have a different value
                IF (zConstantValue(iconst) /= zValue) THEN
                    ErrMsg = 'Constant is already defined with different value!'
                    CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                    RETURN
                ELSE
                    nConstants = nConstants - 1
                    RETURN
                END IF
            END IF
        END DO

        ! Apply constant value and constant name
        cConstantName(nConstants)  = sString
        dConstantValue(nConstants) = REAL(zValue)
        zConstantValue(nConstants) = zValue
    END ASSOCIATE

    RETURN

END SUBROUTINE Parser_DefineConstantCmpl

!**************************************************************************************

SUBROUTINE Parser_DefineExpression(rfParser, sName, sValue)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To define a new expression.  The routine checks if the given expression
    !  is already defined.

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'Parser_DefineExpression'

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser object
    CLASS(MathExpParser),    INTENT(INOUT)  :: rfParser
    !% Name of the Expression
    tCharLen(FPAR_EXPR_LEN), INTENT(IN)     :: sName
    !% String of the Expression
    tCharStar,               INTENT(IN)     :: sValue

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(LEN(sName))    :: sExpression
    tCharLen(LEN(sValue))   :: sString
    tSInt32                 :: iExpression

!** FLOW

    ASSOCIATE(nExpressions      => rfParser%nExpressions, &
              cExpressionName   => rfParser%cExpressionName, &
              cExpressionString => rfParser%cExpressionString)
        ! Check if there is enough space
        IF (nExpressions < FPAR_MAX_EXPRS) THEN
            nExpressions = nExpressions + 1
        ELSE
            ErrMsg = 'No space left for definition of expression!'
            CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
            RETURN
        END IF

        ! Prepare Expression string
        CALL ToLowerCase(sName, sExpression)
        CALL ToLowerCase(sValue, sString)

        ! Replace human readable function names by 1-Char. format
        CALL Replace('**','^ ', sString)
        CALL Replace('[','(',   sString)
        CALL Replace(']',')',   sString)
        CALL Replace('{','(',   sString)
        CALL Replace('}',')',   sString)

        ! Condense function string
        CALL RemoveSpaces(sString)

        ! Check if Expressions is already defined
        DO iExpression = 1, nExpressions-1
            IF (cExpressionName(iExpression) == sExpression) THEN
                ! If it is already defined, then it must not have a different value
                IF (cExpressionString(iExpression) /= sString) THEN
                    ErrMsg = 'Expression is already defined with different string!'
                    CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                    RETURN
                ELSE
                    nExpressions = nExpressions - 1
                    RETURN
                END IF
            END IF
        END DO

        ! Apply Expressions string and Expression name
        cExpressionName(nExpressions)   = sExpression
        cExpressionString(nExpressions) = sString
    END ASSOCIATE

    RETURN

END SUBROUTINE Parser_DefineExpression

!**************************************************************************************

SUBROUTINE Parser_Create(rfParser, MaxNComp)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the function parser for *MaxNComp* functions.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser object
    CLASS(MathExpParser), INTENT(OUT)   :: rfParser
    !% Number of functions
    tSInt32,              INTENT(IN)    :: MaxNComp
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! Set number of components
    rfParser%MaxNComp = MaxNComp
    rfParser%NComp  = 0

    ! Allocate arrays
    CALL MemAlloc(rfParser%rComp, MaxNComp)
    CALL MemAlloc(rfParser%SCompName, ToIndex(FPAR_FUNC_LEN), [ToIndex(MaxNComp)])
    
    ! initialize global variables
    CALL Parser_Init(rfParser)

END SUBROUTINE Parser_Create

!**************************************************************************************

SUBROUTINE Parser_Destroy(rfParser)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To release the function parser and all of its components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    CLASS(MathExpParser), INTENT(INOUT) :: rfParser

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32         :: iComp

!** FLOW

    ! Check that pointer is associated and return otherwise
    IF (.NOT.ASSOCIATED(rfParser%rComp)) RETURN

    ! Loop over all components and deallocate arrays
    DO iComp = 1, rfParser%MaxNComp
        CALL MemFree(rfParser%rComp(iComp)%iByteCode)
        CALL MemFree(rfParser%rComp(iComp)%dImmed)
        CALL MemFree(rfParser%rComp(iComp)%zImmed)
    END DO

    ! Deallocate memory
    CALL MemFree(rfParser%rComp)
    CALL MemFree(rfParser%SCompName)

    ! Reset scalar data
    rfParser%MaxNComp = 0
    rfParser%NComp  = 0

    ! reset global variables
    CALL Parser_Done(rfParser)

END SUBROUTINE Parser_Destroy

!**************************************************************************************

SUBROUTINE Parser_Finalize(rfParser)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the 'MathExpParser' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser), INTENT(INOUT)  :: rfParser
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL rfParser%Destroy()

END SUBROUTINE Parser_Finalize

!**************************************************************************************

SUBROUTINE Parser_ParseFunctionByName(rfParser, SCompName, sFunctionString, &
                                      sVariables, bUseDegrees, iComp)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To parse function string *sFunctionString* and compile it into bytecode.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    CLASS(MathExpParser), INTENT(INOUT) :: rfParser
    !% Function identifier
    tCharStar,            INTENT(IN)    :: SCompName
    !% Function string
    tCharStar,            INTENT(IN)    :: sFunctionString
    !% Array with variable names
    tCharStar,            INTENT(IN)    :: sVariables(:)
    !> Flag indicating whether to use degree unit. <br>
    !  - If true, use degree unit. <br>
    !  - If false, use radian unit. <br>
    !  Default is false. <br>
    tLogical, OPTIONAL,   INTENT(IN)    :: bUseDegrees
    !% Function identifier
    tSInt32,  OPTIONAL,   INTENT(OUT)   :: iComp

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'Parser_ParseFunctionByName'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! Check if there is space for a new component
    IF (rfParser%NComp == rfParser%MaxNComp) THEN
        ErrMsg = 'No free components left!'
        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
        RETURN
    END IF

    ! Increase component counter
    rfParser%NComp = rfParser%NComp + 1

    ! Store function name
    CALL ToLowerCase(TRIM(ADJUSTL(SCompName)), rfParser%SCompName(rfParser%NComp))

    ! Parse function
    CALL rfParser%ParseFunction(rfParser%NComp, sFunctionString, sVariables, bUseDegrees)

    ! Return function identifier
    IF (PRESENT(iComp)) iComp = rfParser%NComp

END SUBROUTINE Parser_ParseFunctionByName

!**************************************************************************************

SUBROUTINE Parser_ParseFunctionByNumber(rfParser, iComp, sFunctionString, &
                                        sVariables, bUseDegrees)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To parse the function string *sFunctionString* and compile it into bytecode.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    CLASS(MathExpParser), INTENT(INOUT) :: rfParser
    !% Function identifier
    tSInt32,              INTENT(IN)    :: iComp
    !% Function string
    tCharStar,            INTENT(IN)    :: sFunctionString
    !% Array with variable names
    tCharStar,            INTENT(IN)    :: sVariables(:)
    !> Flag indicating whether to use degree unit. <br>
    !  - If true, use degree unit. <br>
    !  - If false, use radian unit. <br>
    !  Default is false. <br>
    tLogical, OPTIONAL,   INTENT(IN)    :: bUseDegrees

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'Parser_ParseFunctionByNumber'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(LEN(sFunctionString))  :: sString

!** FLOW

    ! Check if component is valid
    IF (iComp < 1 .OR. iComp > rfParser%MaxNComp) THEN
        ErrMsg = 'Component number is out of range.'
        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
        RETURN
    END IF

    ! Local copy of function string
    sString = sFunctionString

    ! Replace human readable function names by 1-Char. format
    CALL Replace('**','^ ', sString)
    CALL Replace('[','(',   sString)
    CALL Replace(']',')',   sString)
    CALL Replace('{','(',   sString)
    CALL Replace('}',')',   sString)

    ! Condense function string
    CALL RemoveSpaces(sString)

    ! Check for valid syntax; this prevents the bytecode compiler
    ! from running into endless loops or other problems
    CALL CheckSyntax(rfParser, sString, sVariables)

    ! Check if conversion to degrees is required
    IF (PRESENT(bUseDegrees)) rfParser%rComp(iComp)%bUseDegreeConversion = bUseDegrees

    ! If syntax is correct, then compile into bytecode
    CALL Compile(rfParser, rfParser%rComp(iComp), sString, sVariables)

END SUBROUTINE Parser_ParseFunctionByNumber

!**************************************************************************************

SUBROUTINE Parser_EvalFuncScDbleByName(rfParser, SCompName, dValue, EvalErrType, dResult)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To evaluate bytecode of the function named *SCompName* for the
    !  real-valued values passed in the array *dValue*.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    CLASS(MathExpParser), INTENT(IN)    :: rfParser
    !% Function name
    tCharStar,            INTENT(IN)    :: SCompName
    !% Variable values
    tFloat,               INTENT(IN)    :: dValue(:)
    !% Error flag
    tSInt32,              INTENT(OUT)   :: EvalErrType
    !% Evaluated function
    tFloat,               INTENT(OUT)   :: dResult

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: iComp

!** FLOW

    ! Lookup function by name
    iComp = rfParser%GetFunctionNumber(SCompName)

    ! Evaluate function by number
    CALL rfParser%EvalFunction(iComp, dValue, EvalErrType, dResult)

END SUBROUTINE Parser_EvalFuncScDbleByName

!**************************************************************************************

SUBROUTINE Parser_EvalFuncScCmplByName(rfParser, SCompName, zValue, EvalErrType, zResult)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To evaluate bytecode of the function named *SCompName* for the
    !  complex-valued values passed in the array *zValues*.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    CLASS(MathExpParser), INTENT(IN)    :: rfParser
    !% Function name
    tCharStar,            INTENT(IN)    :: SCompName
    !% Variable values
    tCmplx,               INTENT(IN)    :: zValue(:)
    !% Error flag
    tSInt32,              INTENT(OUT)   :: EvalErrType
    !% Evaluated function
    tCmplx,               INTENT(OUT)   :: zResult
    
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: iComp

!** FLOW

    ! Lookup function by name
    iComp = rfParser%GetFunctionNumber(SCompName)

    ! Evaluate function by number
    CALL rfParser%EvalFunction(iComp, zValue, EvalErrType, zResult)

END SUBROUTINE Parser_EvalFuncScCmplByName

!**************************************************************************************

SUBROUTINE Parser_EvalFuncScDbleByNumber(rfParser, iComp, dValue, EvalErrType, dResult)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To evaluate bytecode of component *iComp* for the real-valued values
    !  passed in the array *dValue*. Note that this function is a wrapper
    !  for the working routine *EvalFunctionScDble*. It is used to adjust
    !  the dimensions of the global stack memory if required.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    CLASS(MathExpParser), INTENT(IN)    :: rfParser
    !% Function identifier
    tSInt32,              INTENT(IN)    :: iComp
    !% Variable values
    tFloat,               INTENT(IN)    :: dValue(:)
    !% Error flag
    tSInt32,              INTENT(OUT)   :: EvalErrType
    !% Evaluated function
    tFloat,               INTENT(OUT)   :: dResult

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'Parser_EvalFuncScDbleByNumber'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tFloat, ALLOCATABLE  :: Dstack(:)

!** FLOW

    ! Check if component is valid
    IF (iComp < 1 .OR. iComp > rfParser%MaxNComp) THEN
        ErrMsg = 'Component number is out of range.'
        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
        RETURN
    END IF

    ! Allocate temporal memory
    CALL MemAlloc(Dstack, [ToIndex(rfParser%rComp(iComp)%iStackSize+1)])

    ! Invoke working routine
    CALL EvalFunctionScDble(rfParser%rComp(iComp), Dstack, dValue, EvalErrType, dResult)

    ! Deallocate temporal memory
    CALL MemFree(Dstack)

    ! Check if evaluation was successful
    IF (EvalErrType /= 0) THEN
        ErrMsg = 'An error occurred during function evaluation!'
        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
    END IF

END SUBROUTINE Parser_EvalFuncScDbleByNumber

!**************************************************************************************

SUBROUTINE Parser_EvalFuncScCmplByNumber(rfParser, iComp, zValue, EvalErrType, zResult)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To evaluate bytecode of component *iComp* for the complex-valued values
    !  passed in the array *zValue*. Note that this function is a wrapper
    !  for the working routine *EvalFunctionScCmpl*. It is used to adjust
    !  the dimensions of the global stack memory if required.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    CLASS(MathExpParser), INTENT(IN)    :: rfParser
    !% Function identifier
    tSInt32,              INTENT(IN)    :: iComp
    !% Variable values
    tCmplx,               INTENT(IN)    :: zValue(:)
    !% Error flag
    tSInt32,              INTENT(OUT)   :: EvalErrType
    !% Evaluated function
    tCmplx,               INTENT(OUT)   :: zResult

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'Parser_EvalFuncScCmplByNumber'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCmplx, ALLOCATABLE   :: Zstack(:)
     
!** FLOW

    ! Check if component is valid
    IF (iComp < 1 .OR. iComp > rfParser%MaxNComp) THEN
        ErrMsg = 'Component number is out of range.'
        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
        RETURN
    END IF

    ! Allocate temporal memory
    CALL MemAlloc(Zstack, [ToIndex(rfParser%rComp(iComp)%iStackSize+1)])

    ! Invoke working routine
    CALL EvalFunctionScCmpl(rfParser%rComp(iComp), Zstack, zValue, EvalErrType, zResult)

    ! Deallocate temporal memory
    CALL MemFree(Zstack)

    ! Check if evaluation was successful
    IF (EvalErrType /= 0) THEN
        ErrMsg = 'An error occurred during function evaluation!'
        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
    END IF

END SUBROUTINE Parser_EvalFuncScCmplByNumber

!**************************************************************************************

FUNCTION Parser_ErrorMsg(EvalErrType) RESULT(sMessage)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return error message of function parser.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Error identifier
    tSInt32,  INTENT(IN)    :: EvalErrType
    !% Error messages
    tCharAlloc              :: sMessage

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: Msg(4) = ['Division by zero       ', &
                             'Illegal argument       ', &
                             'Complex-valued argument', &
                             'Illegal operation      ']

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (EvalErrType < 1 .OR. EvalErrType > SIZE(Msg)) THEN
        sMessage = ''
    ELSE
        sMessage = Msg(EvalErrType)
    END IF

END FUNCTION Parser_ErrorMsg

!**************************************************************************************

SUBROUTINE Parser_PrintByteCodeByName(rfParser, SCompName, iOutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To print the compiled bytecode stack.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    CLASS(MathExpParser), INTENT(IN)    :: rfParser
    !% Function name
    tCharStar,            INTENT(IN)    :: SCompName
    !% Output unit
    tSInt32,              INTENT(IN)    :: iOutUnit

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: iComp
    
!** FLOW
    
    ! Lookup function by name
    iComp = Parser_GetFunctionNumber(rfParser, SCompName)

    ! Print bytecode
    CALL Parser_PrintByteCodeByNumber(rfParser, iComp, iOutUnit)

END SUBROUTINE Parser_PrintByteCodeByName

!**************************************************************************************

SUBROUTINE Parser_PrintByteCodeByNumber(rfParser, iComp, iOutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To print the compiled bytecode stack

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    CLASS(MathExpParser), INTENT(IN)    :: rfParser
    !% Function identifier
    tSInt32,              INTENT(IN)    :: iComp
    !% Output unit
    tSInt32,              INTENT(IN)    :: iOutUnit

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    TYPE(ParserComponent), POINTER  :: p_Comp
    tCharLen(5)                     :: n
    tSInt32                         :: iinstPtr, idataPtr, iStackPtr, nparams
    tSInt8                           :: iOpcode
    
!** FLOW

    nparams   = 1
    idataPtr  = 1
    iStackPtr = 0
    iinstPtr  = 0
    p_Comp    => rfParser%rComp(iComp)

    DO WHILE(iinstPtr < p_Comp%iByteCodeSize)
        
        iinstPtr = iinstPtr + 1

        WRITE(iOutUnit,FMT='(I8.8,1X,":",1X)', ADVANCE="NO") iinstPtr

        iOpcode = p_Comp%iByteCode(iinstPtr)
        
        SELECT CASE (iOpcode)
        CASE (cIf)
            
            WRITE(iOutUnit,FMT='(A,1X,T10,I8.8)') "jz", p_Comp%iByteCode(iinstPtr+1)+1
            iinstPtr = iinstPtr + 2

        CASE (cJump)
            
            WRITE(iOutUnit,FMT='(A,1X,T10,I8.8)') "jump", p_Comp%iByteCode(iinstPtr+1)+1
            iinstPtr = iinstPtr + 2

        CASE (cImmed)
            
            IF (p_Comp%bIsComplex) THEN
                WRITE(iOutUnit,FMT='(A,1X,T10,A,G16.8,A,G16.8,A)') "push", "(",&
                            REAL(p_Comp%zImmed(idataPtr)), ",", &
                            AIMAG(p_Comp%zImmed(idataPtr)), ")"
            ELSE
                WRITE(iOutUnit,FMT='(A,1X,T10,G16.8)') "push", p_Comp%dImmed(idataPtr)
            END IF
            idataPtr = idataPtr + 1

        CASE DEFAULT
            IF (iOpcode < VarBegin) THEN
                SELECT CASE (iOpcode)
                CASE (cNEG);         n = "neg"
                CASE (cADD);         n = "add"
                CASE (cSUB);         n = "sub"
                CASE (cMUL);         n = "mul"
                CASE (cDIV);         n = "div"
                CASE (cMOD);         n = "mod"
                CASE (cPOW);         n = "pow"
                CASE (cEqual);       n = "eq"
                CASE (cNEqual);      n = "ne"
                CASE (cLess);        n = "lt"
                CASE (cLessOrEq);    n = "le"
                CASE (cGreater);     n = "gt"
                CASE (cGreaterOrEq); n = "ge"
                CASE (cAND);         n = "and"
                CASE (cOR);          n = "or"
                CASE (cNOT);         n = "not"
                CASE (cDEG);         n = "deg"
                CASE (cRAD);         n = "rad"
                CASE DEFAULT
                    n       = Funcs(iOpcode)
                    nparams = MathFunctionParameters(iOpcode)
                END SELECT
                WRITE(iOutUnit,FMT='(A,T10,A,"  (",I1,") ")') TRIM(n), "Par", nparams

            ELSE
                WRITE(iOutUnit,FMT='(A,T10,A,1X,I4.4)') "push", "Var", iOpcode-VarBegin+1
            END IF

        END SELECT
    END DO

END SUBROUTINE Parser_PrintByteCodeByNumber

!**************************************************************************************

FUNCTION Parser_GetFunctionNumber(rfParser, SCompName, bQuiet) RESULT(iComp)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the internal number of the component which
    !  corresponds to the function with name *SCompName*.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    CLASS(MathExpParser), INTENT(IN)    :: rfParser
    !% Function name
    tCharStar,            INTENT(IN)    :: SCompName
    !> OPTIONAL: Specifies whether a warning should be printed when released an
    !  empty vector (bQuiet = FalseVal) or whether to remain silent in this case.
    !  If not specified, bQuiet = FalseVal is used.
    tLogical, OPTIONAL,   INTENT(IN)    :: bQuiet
    !% Function identifier
    tSInt32                             :: iComp

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'Parser_GetFunctionNumber'

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLogical                    :: bwarn
    tCharLen(LEN(SCompName))    :: sName

!** FLOW

    ! Convert to lower case
    CALL ToLowerCase(SCompName, sName)

    ! Lookup function
    DO iComp = 1, rfParser%NComp
        IF (TRIM(ADJUSTL(sName)) == TRIM(rfParser%SCompName(iComp))) RETURN
    END DO

    ! If we end up here, then the function is not available
    iComp = 0

    ! Shout or shut up?
    bwarn = TrueVal
    IF (PRESENT(bQuiet)) bwarn = .NOT. bQuiet

    IF (bwarn) THEN
        ErrMsg = 'Function is not available.'
        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
    END IF

END FUNCTION Parser_GetFunctionNumber

!**************************************************************************************

RECURSIVE SUBROUTINE CheckSyntax(rfParser, sFunctionString, sVariables)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check syntax of function string, returns 0 if syntax is valid.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser), INTENT(IN) :: rfParser
    !% Function string without spaces
    tCharStar,           INTENT(IN) :: sFunctionString
    !% Array with variable names
    tCharStar,           INTENT(IN) :: sVariables(:)

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'CheckSyntax'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    TYPE(StackI4B)  :: rStack
    tSInt32         :: StackItem, idummy
    tSInt8          :: n, iopSize
    tCharLen(1)     :: c
    tFloat          :: dnumber
    tLogical        :: bError
    tSInt32         :: iFunctionIndex, iFunctionIndex2
    tSInt32         :: iparenthCount, ib, in, ifunctionLength

!** FLOW

    ASSOCIATE(nConstants     => rfParser%nConstants, &
              cConstantName  => rfParser%cConstantName, &
              dConstantValue => rfParser%dConstantValue, &
              zConstantValue => rfParser%zConstantValue, &
              nExpressions      => rfParser%nExpressions, &
              cExpressionName   => rfParser%cExpressionName, &
              cExpressionString => rfParser%cExpressionString)
        ! Initialization
        iFunctionIndex  = 1
        iparenthCount   = 0
        ifunctionLength = LEN_TRIM(sFunctionString)

        Outer_Loop: DO
            IF (iFunctionIndex > ifunctionLength) THEN
                ErrMsg = 'Invalid function string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF
            c = sFunctionString(iFunctionIndex:iFunctionIndex)

            ! Check for valid operand (must appear)

            ! Check for leading - or !
            IF (c == '-' .OR. c == '!') THEN
                iFunctionIndex = iFunctionIndex + 1
                IF (iFunctionIndex > ifunctionLength) THEN
                    ErrMsg = 'Premature end of string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                    CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                    RETURN
                END IF
                c = sFunctionString(iFunctionIndex:iFunctionIndex)
            END IF

            ! Check for math function
            n = MathFunctionIndex(sFunctionString(iFunctionIndex:))
        
            PossibleError: BLOCK
                IF (n > 0) THEN
                    ! Math function found
                    iFunctionIndex = iFunctionIndex + LEN_TRIM(Funcs(n))
                    IF (iFunctionIndex > ifunctionLength) THEN
                        ErrMsg = 'Premature end of string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                        RETURN
                    END IF
                    c = sFunctionString(iFunctionIndex:iFunctionIndex)
                    IF (c /= '(') THEN
                        ErrMsg = 'Expecting ( after function ' // sFunctionString(iFunctionIndex:) // '!'
                        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                        RETURN
                    END IF
                    iFunctionIndex2 = iFunctionIndex + 1
                    IF (iFunctionIndex2 > ifunctionLength) THEN
                        ErrMsg = 'Premature end of string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                        RETURN
                    END IF
                    IF (sFunctionString(iFunctionIndex2:iFunctionIndex2) == ')') THEN
                        iFunctionIndex = iFunctionIndex2 + 1
                        IF (iFunctionIndex > ifunctionLength) THEN
                            ErrMsg = 'Premature end of string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                            CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                            RETURN
                        END IF
                        c = sFunctionString(iFunctionIndex:iFunctionIndex)

                        ! immediately get out of the PossibleError block
                        EXIT PossibleError
                    END IF

                    ! Push counter for parentheses to stack
                    CALL rStack%Push(iparenthCount+1)
                END IF

                ! Check for opening parenthesis
                IF (c == '(') THEN
                    iparenthCount = iparenthCount + 1
                    iFunctionIndex = iFunctionIndex + 1
                    IF (iFunctionIndex > ifunctionLength) THEN
                        ErrMsg = 'Premature end of string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                        RETURN
                    END IF
                    IF (sFunctionString(iFunctionIndex:iFunctionIndex) == ')') THEN
                        ErrMsg = 'Empty parentheses in string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                        RETURN
                    END IF
                    CYCLE Outer_Loop
                END IF

                ! Check for number
                IF (SCAN(c,'0123456789.') > 0) THEN
                    dnumber = RealNum(sFunctionString(iFunctionIndex:), ib, in, bError)
                    IF (bError) THEN
                        ErrMsg = 'Invalid number format in string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                        RETURN
                    END IF
                    iFunctionIndex = iFunctionIndex + in - 1
                    IF (iFunctionIndex > ifunctionLength) EXIT Outer_Loop
                    c = sFunctionString(iFunctionIndex:iFunctionIndex)

                ELSE IF (c == '_') THEN
                    ! Check for constant
                    n = ConstantIndex(rfParser, sFunctionString(iFunctionIndex:))
                    IF (n == 0) THEN
                        ErrMsg = 'Invalid constant in string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                        RETURN
                    END IF
                    iFunctionIndex = iFunctionIndex + LEN_TRIM(cConstantName(n)) + 1
                    IF (iFunctionIndex > ifunctionLength) EXIT Outer_Loop
                    c = sFunctionString(iFunctionIndex:iFunctionIndex)
                ELSE IF (c == '@') THEN
                    ! Check for Expression
                    n = ExpressionIndex(rfParser, sFunctionString(iFunctionIndex:))
                    IF (n == 0) THEN
                        ErrMsg = 'Invalid Expression in string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                        RETURN
                    END IF
                    CALL CheckSyntax(rfParser, cExpressionString(n), sVariables)
                    iFunctionIndex = iFunctionIndex + LEN_TRIM(cExpressionName(n)) + 1
                    IF (iFunctionIndex > ifunctionLength) EXIT Outer_Loop
                    c = sFunctionString(iFunctionIndex:iFunctionIndex)
                ELSE
                    ! Check for variable
                    n = VariableIndex(sFunctionString(iFunctionIndex:), sVariables, ib, in)
                    IF (n == 0) THEN
                        ErrMsg = 'Invalid element in string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                        RETURN
                    END IF
                    iFunctionIndex = iFunctionIndex + in - 1
                    IF (iFunctionIndex > ifunctionLength) EXIT Outer_Loop
                    c = sFunctionString(iFunctionIndex:iFunctionIndex)
                END IF

                ! Check for closing parenthesis
                DO WHILE (c == ')')
                    IF (.NOT.rStack%IsEmpty()) THEN
                        CALL rStack%Pop(StackItem, Peek=TrueVal)
                        IF (StackItem == iparenthCount) THEN
                            CALL rStack%Pop(idummy)
                        END IF
                    END IF
                    iparenthCount = iparenthCount - 1
                    IF (iparenthCount < 0) THEN
                        ErrMsg = 'Mismatched parenthesis in string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                        RETURN
                    END IF
                    IF (sFunctionString(iFunctionIndex-1:iFunctionIndex-1) == '(') THEN
                        ErrMsg = 'Empty parentheses in string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                        RETURN
                    END IF
                    iFunctionIndex = iFunctionIndex + 1
                    IF (iFunctionIndex > ifunctionLength) EXIT Outer_Loop
                    c = sFunctionString(iFunctionIndex:iFunctionIndex)
                END DO
            END BLOCK PossibleError

            ! Now, we have a legal operand: A legal operator or the end of
            ! string must follow
            IF (iFunctionIndex > ifunctionLength) EXIT Outer_Loop

            ! Check operators
            iopSize = 0
            IF (.NOT.rStack%IsEmpty()) THEN
                CALL rStack%Pop(StackItem, Peek=TrueVal)
                IF (c == ',' .AND. StackItem == iparenthCount) THEN
                    iopSize = 1
                ELSE
                    iopSize = IsOperator(sFunctionString(iFunctionIndex:))
                END IF
            ELSE
                iopSize = IsOperator(sFunctionString(iFunctionIndex:))
            END IF

            IF (iopSize == 0) THEN
                ErrMsg = 'Operator expected in string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Now, we have an operand and an operator: the next loop will check for another
            ! operand (must appear)
            iFunctionIndex = iFunctionIndex + iopSize
        END DO Outer_Loop

        ! Sanity check if the number of opening and closing brackets is the same
        IF (iparenthCount /= 0) THEN
            ErrMsg = 'Missing ) in string ' // TRIM(ADJUSTL(sFunctionString)) // ' !'
            CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
            RETURN
        END IF
    END ASSOCIATE

    RETURN

END SUBROUTINE CheckSyntax

!**************************************************************************************

FUNCTION IsOperator(sFunctionString) RESULT(n)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return 0 if given string is not an operator, else the size of the
    !  operator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Operator string
    tCharStar, INTENT(IN)   :: sFunctionString
    !% Length of operator, 0 if string is no operator
    tSInt8                   :: n

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt8       :: j

!** FLOW

    ! Check all operators
    DO j = cAdd, cOr
        IF (INDEX(sFunctionString,TRIM(Ops(j))) == 1) THEN
            n = LEN_TRIM(Ops(j))
            RETURN
        END IF
    END DO
    n = 0

END FUNCTION IsOperator

!**************************************************************************************

FUNCTION MathFunctionIndex(sFunctionString) RESULT(n)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return Index of math function beginning at 1st position of string *sFunctionString*.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Math function string
    tCharStar, INTENT(IN)   :: sFunctionString
    !% Index of math function
    tSInt8                   :: n

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(LEN(sFunctionString))  :: sFunctionName

!** FLOW

    ! Check all math functions
    CALL ToLowerCase(sFunctionString, sFunctionName)
    DO n = cIf, cSign
        IF (INDEX(sFunctionName, TRIM(Funcs(n))) == 1) RETURN
    END DO
    n = 0

END FUNCTION MathFunctionIndex

!**************************************************************************************

FUNCTION MathFunctionParameters(iFunctionIndex) RESULT(nParameters)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return number of required parameters.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Index of function
    tSInt8, INTENT(IN)   :: iFunctionIndex
    !% Number if required parameters
    tSInt32             :: nParameters

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'MathFunctionParameters'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (iFunctionIndex)
    CASE (cIf)
        nParameters = 3

    CASE (cMin:cAtan2)
        nParameters = 2

    CASE (cAbs:cSign)
        nParameters = 1

    CASE DEFAULT
        nParameters = 0
        ErrMsg = 'Not a function'
        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
        RETURN
    END SELECT

END FUNCTION MathFunctionParameters

!**************************************************************************************

FUNCTION ConstantIndex(rfParser, sFunctionString) RESULT(n)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return Index of predefined constants beginning at 1st position of
    !  string *sFunctionString*.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser), INTENT(IN) :: rfParser
    !% Math function string
    tCharStar,           INTENT(IN) :: sFunctionString
    !% Index of math function
    tSInt8                           :: n

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(LEN(sFunctionString)-1)    :: sConstantName

!** FLOW

    ! Check all math functions
    CALL ToLowerCase(sFunctionString(2:), sConstantName)
    DO n = 1, rfParser%nConstants
        IF (INDEX(sConstantName, TRIM(rfParser%cConstantName(n))) == 1) RETURN
    END DO
    n = 0

END FUNCTION ConstantIndex

!**************************************************************************************

FUNCTION ExpressionIndex(rfParser, sFunctionString) RESULT(n)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return Index of predefined Expression beginning at 1st position
    !  of string *sFunctionString*.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser), INTENT(IN) :: rfParser
    !% Math function string
    tCharStar,           INTENT(IN) :: sFunctionString
    !% Index of math function
    tSInt8                           :: n

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(LEN(sFunctionString)-1)    :: sExpressionName

!** FLOW

    ! Check all math functions
    CALL ToLowerCase(sFunctionString(2:), sExpressionName)
    DO n = 1, rfParser%nExpressions
        IF (INDEX(sExpressionName, TRIM(rfParser%cExpressionName(n))) == 1) RETURN
    END DO
    n = 0

END FUNCTION ExpressionIndex

!**************************************************************************************
  
FUNCTION VariableIndex(sString, sVariables, iBegin, iNext) RESULT(n)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return Index of variable at begin of string *sString*
    ! (returns 0 if no variable found).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! String
    tCharStar,          INTENT(IN)  :: sString
    ! Array with variable names
    tCharStar,          INTENT(IN)  :: sVariables(:)
    ! OPTIONAL: Start position of variable name
    tSInt32,  OPTIONAL, INTENT(OUT) :: iBegin
    ! OPTIONAL: Position of character after name
    tSInt32,  OPTIONAL, INTENT(OUT) :: iNext
    ! Index of variable
    tSInt8                           :: n

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: j, ib, in, iStringLen

!** FLOW

    n = 0
    iStringLen = LEN_TRIM(sString)
    IF (iStringLen > 0) THEN
        ! Search for first character in sString
        DO ib = 1, iStringLen
            ! When iStringLen > 0 at least 1 char in sString
            IF (sString(ib:ib) /= ' ') EXIT
        END DO

        ! Search for name terminators
        DO in = ib, iStringLen
            IF (SCAN(sString(in:in),'*+-/%^),&|<>=! ') > 0) EXIT
        END DO
        DO j = 1, SIZE(sVariables)
            IF (sString(ib:in-1) == sVariables(j)) THEN
                ! Variable name found
                n = j
                EXIT
            END IF
        END DO
    END IF

    IF (PRESENT(iBegin)) iBegin = ib
    IF (PRESENT(iNext))  iNext  = in

END FUNCTION VariableIndex

!**************************************************************************************

SUBROUTINE RemoveSpaces(sFunctionString)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove Spaces from string, remember positions of characters in
    !  old string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% String from which spaces should be removed
    tCharStar, INTENT(INOUT)    :: sFunctionString

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: k,iStringLen

!** FLOW

    iStringLen = LEN_TRIM(sFunctionString)
    k = 1
    DO WHILE (sFunctionString(k:iStringLen) /= ' ')
        IF (sFunctionString(k:k) == ' ') THEN
            sFunctionString(k:iStringLen)  = sFunctionString(k+1:iStringLen)//' ' ! Move 1 character to left
            k = k - 1
        END IF
        k = k + 1
    END DO

END SUBROUTINE RemoveSpaces

!**************************************************************************************

SUBROUTINE Replace(ca, cb, sFunctionString)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To replace all appearances of character set *ca* in string *sFunctionString* by character set *cb*.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Source characters
    tCharStar,         INTENT(IN)       :: ca
    !% Destination characters
    tCharLen(LEN(ca)), INTENT(IN)       :: cb
    !% String
    tCharStar,         INTENT(INOUT)    :: sFunctionString

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: j,lca

!** FLOW

    lca = LEN(ca)
    DO j = 1, LEN_TRIM(sFunctionString)-lca+1
        IF (sFunctionString(j:j+lca-1) == ca) sFunctionString(j:j+lca-1) = cb
    END DO

END SUBROUTINE Replace

!**************************************************************************************

SUBROUTINE Compile(rfParser, rComp, sFunctionString, sVariables)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compile the i-th function string into bytecode.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser),   INTENT(IN)       :: rfParser
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Function string
    tCharStar,             INTENT(IN)       :: sFunctionString
    !% Array with variable names
    tCharStar,             INTENT(IN)       :: sVariables(:)

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: Ind,iSize

!** FLOW

    ! (Re-)initialize the bytecode structure (if required)
    CALL MemFree(rComp%iByteCode)
    CALL MemFree(rComp%dImmed)
    CALL MemFree(rComp%zImmed)
    rComp%iByteCodeSize = 0
    rComp%iImmedSize    = 0
    rComp%iStackSize    = 0
    rComp%iStackPtr     = 0
    rComp%bIsVectorizable = TrueVal

    ! Neither the stack for the bytecode nor the stack for the
    ! immediate Expressions can exceed the size of the function
    ! string. Hence, allocate some initial memory
    iSize = FunctionSize(rfParser, sFunctionString)
    CALL MemAlloc(rComp%iByteCode, [ToIndex(iSize)])
    CALL MemAlloc(rComp%dImmed, [ToIndex(iSize)])
    CALL MemAlloc(rComp%zImmed, [ToIndex(iSize)])

    ! Compile function string into bytecode
    Ind = CompileExpression(rfParser, rComp, sFunctionString, 1, sVariables)

    ! Adjust memory size of bytecode stack
    IF (rComp%iByteCodeSize == 0) THEN
        CALL MemFree(rComp%iByteCode)
    ELSE
        ! Resize iByteCode
        CALL MemResize(rComp%iByteCode, [ToIndex(rComp%iByteCodeSize)])
    END IF

    ! Adjust memory size of immediate stack
    IF (rComp%iImmedSize == 0) THEN
        CALL MemFree(rComp%dImmed)
        CALL MemFree(rComp%zImmed)
    ELSE
        ! Resize dImmed
        CALL MemResize(rComp%dImmed, [ToIndex(rComp%iImmedSize)])
        ! Resize zImmed
        CALL MemResize(rComp%zImmed, [ToIndex(rComp%iImmedSize)])
    END IF
    
    RETURN
    
END SUBROUTINE Compile

!**************************************************************************************

SUBROUTINE IncStackPtr(rComp)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To increase stack pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    rComp%iStackPtr = rComp%iStackPtr + 1
    IF (rComp%iStackPtr > rComp%iStackSize) rComp%iStackSize = rComp%iStackSize + 1

END SUBROUTINE IncStackPtr

!**************************************************************************************

SUBROUTINE AddCompiledByte(rComp, iByte)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add compiled byte to bytecode.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Value of byte to be added
    tSInt8,                INTENT(IN)       :: iByte

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'AddCompiledByte'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tFloat                   :: daux
    tSInt32,  ALLOCATABLE   :: p_Irandom1(:), p_Irandom2(:)
    tSInt32                 :: iaux

!** FLOW

    rComp%iByteCodeSize = rComp%iByteCodeSize + 1
    rComp%iByteCode(rComp%iByteCodeSize) = iByte

    ! Try to optimize the compiled bytecode. Check the bytecode
    ! instruction and compute some values on-the-fly of this is
    ! possible. If rComp%bIsComplex is false, we compute only the
    ! real-valued path and set the complex-valued path equal to the
    ! real-valued path. Some functions return complex-valued
    ! results. In this case rComp%bIsComplex is set to true and the
    ! real-valued path is overwritten by SYS_INFINITY_kFloat.
    SELECT CASE (iByte)
      !------------------------------------------------------------
      ! Functions
      !------------------------------------------------------------
    CASE (cAbs)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = ABS(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = ABS(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAcos)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Acos(A) gives complex result?
            IF (rComp%dImmed(rComp%iImmedSize) < -1.0_kFloat .OR. &
                rComp%dImmed(rComp%iImmedSize) >  1.0_kFloat) rComp%bIsComplex = TrueVal

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = ACOS(rComp%zImmed(rComp%iImmedSize))
            ELSE
                ! Real-valued path
                rComp%dImmed(rComp%iImmedSize) = ACOS(rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = ACOS(rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAsin)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Asin(A) gives complex result?
            IF (rComp%dImmed(rComp%iImmedSize) < -1.0_kFloat .OR. &
                rComp%dImmed(rComp%iImmedSize) >  1.0_kFloat) rComp%bIsComplex = TrueVal

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = ASIN(rComp%zImmed(rComp%iImmedSize))
            ELSE
                rComp%dImmed(rComp%iImmedSize) = ASIN(rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = ASIN(rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAtan)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = ATAN(rComp%zImmed(rComp%iImmedSize))
            ELSE
                rComp%dImmed(rComp%iImmedSize) = ATAN(rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = ATAN(rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAtan2)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex) THEN
                ErrMsg = 'Invalid complex argument for ATAN2!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize-1) = ATAN2(rComp%dImmed(rComp%iImmedSize),&
                                                     rComp%dImmed(rComp%iImmedSize-1))
            rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAcot)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = ATAN(1.0_kFloat/rComp%zImmed(rComp%iImmedSize))
            ELSE
                rComp%dImmed(rComp%iImmedSize) = ATAN(1.0_kFloat/rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = ATAN(1.0_kFloat/rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAcoth)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = ATANH(1.0_kFloat/rComp%zImmed(rComp%iImmedSize))
            ELSE
                rComp%dImmed(rComp%iImmedSize) = ATANH(1.0_kFloat/rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = ATANH(1.0_kFloat/rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAcosh)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Acosh(A) gives complex result?
            IF (rComp%dImmed(rComp%iImmedSize) < 1.0_kFloat) rComp%bIsComplex = TrueVal

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = ACOSH(rComp%zImmed(rComp%iImmedSize))
            ELSE
                rComp%dImmed(rComp%iImmedSize) = ACOSH(rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = ACOSH(rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAcsc)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Acsc(A) gives complex result?
            IF (rComp%dImmed(rComp%iImmedSize) > -Pi/2.0_kFloat .AND. &
                rComp%dImmed(rComp%iImmedSize) <  Pi/2.0_kFloat) rComp%bIsComplex = TrueVal

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = ASIN(1.0_kFloat/rComp%zImmed(rComp%iImmedSize))
            ELSE
                ! Real-valued path
                rComp%dImmed(rComp%iImmedSize) =  ASIN(1.0_kFloat/rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = ASIN(1.0_kFloat/rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAcsch)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = ASINH(1.0_kFloat/rComp%zImmed(rComp%iImmedSize))
            ELSE
                ! Real-valued path
                rComp%dImmed(rComp%iImmedSize) = ASINH(1.0_kFloat/rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = ASINH(1.0_kFloat/rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAsec)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Asec(A) gives complex result?
            IF (rComp%dImmed(rComp%iImmedSize) > -1.0_kFloat .AND. &
                rComp%dImmed(rComp%iImmedSize) <  1.0_kFloat) rComp%bIsComplex = TrueVal

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = ACOS(1.0_kFloat/rComp%zImmed(rComp%iImmedSize))
            ELSE
                ! Real-valued path
                rComp%dImmed(rComp%iImmedSize) =  ACOS(1.0_kFloat/rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = ACOS(1.0_kFloat/rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAsech)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Asech(A) gives complex result?
            IF (rComp%dImmed(rComp%iImmedSize) < 0.0_kFloat .OR. &
                rComp%dImmed(rComp%iImmedSize) > 1.0_kFloat) rComp%bIsComplex = TrueVal

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = ACOSH(1.0_kFloat/rComp%zImmed(rComp%iImmedSize))
            ELSE
                ! Real-valued path
                rComp%dImmed(rComp%iImmedSize) = ACOSH(1.0_kFloat/rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = ACOSH(1.0_kFloat/rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAsinh)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Asinh(A) gives complex result?
            IF (rComp%dImmed(rComp%iImmedSize) <= 0.0_kFloat) rComp%bIsComplex = TrueVal

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = ASINH(rComp%zImmed(rComp%iImmedSize))
            ELSE
                rComp%dImmed(rComp%iImmedSize) = ASINH(rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = ASINH(rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAtanh)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = ATANH(rComp%zImmed(rComp%iImmedSize))
            ELSE
                rComp%dImmed(rComp%iImmedSize) = ATANH(rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = ATANH(rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAnint)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex .AND. IsComplex(rComp%zImmed(rComp%iImmedSize))) THEN
                ErrMsg = 'Invalid complex argument for ANINT!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize) = ANINT(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = CMPLX(rComp%dImmed(rComp%iImmedSize),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAint)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex .AND. IsComplex(rComp%zImmed(rComp%iImmedSize))) THEN
                ErrMsg = 'Invalid complex argument for AINT!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize) = AINT(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = CMPLX(rComp%dImmed(rComp%iImmedSize),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cCeil)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = CMPLX(CEILING(REAL(rComp%zImmed(rComp%iImmedSize))),&
                                                       CEILING(AIMAG(rComp%zImmed(rComp%iImmedSize))),KIND=kFloat)
            ELSE
                rComp%dImmed(rComp%iImmedSize) = CEILING(rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = CMPLX(rComp%dImmed(rComp%iImmedSize),0.0_kFloat,KIND=kFloat)
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (Ccmplx)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN

                ! Not available for complex-valued case
                IF (IsComplex(rComp%zImmed(rComp%iImmedSize)) .OR.&
                    IsComplex(rComp%zImmed(rComp%iImmedSize-1))) THEN
                    ErrMsg = 'Invalid complex argument for CMPLX!'
                    CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                    RETURN
                END IF

                rComp%zImmed(rComp%iImmedSize-1) = CMPLX(REAL(rComp%zImmed(rComp%iImmedSize-1)),&
                                                         REAL(rComp%zImmed(rComp%iImmedSize)),KIND=kFloat)
                rComp%dImmed(rComp%iImmedSize-1) = SYS_INFINITY_kFloat

            ELSE
                rComp%bIsComplex =TrueVal
                rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),&
                                                         rComp%dImmed(rComp%iImmedSize),KIND=kFloat)
                rComp%dImmed(rComp%iImmedSize-1) = SYS_INFINITY_kFloat
            END IF
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (Cconj)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            ! Nothing needs to be done for the real-valued case
            rComp%zImmed(rComp%iImmedSize) = CONJG(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cCos)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = COS(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = COS(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cCosh)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = COSH(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = COSH(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cCot)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = 1.0_kFloat/TAN(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = 1.0_kFloat/TAN(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cCoth)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = 1.0_kFloat/TANH(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = 1.0_kFloat/TANH(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cCsc)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = 1.0_kFloat/SIN(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = 1.0_kFloat/SIN(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cCsch)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = 1.0_kFloat/SINH(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = 1.0_kFloat/SINH(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cExp)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = EXP(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = EXP(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cFloor)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = CMPLX(FLOOR(REAL(rComp%zImmed(rComp%iImmedSize))),&
                                                       FLOOR(AIMAG(rComp%zImmed(rComp%iImmedSize))),KIND=kFloat)
            ELSE
                rComp%dImmed(rComp%iImmedSize) = FLOOR(rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = CMPLX(rComp%dImmed(rComp%iImmedSize),0.0_kFloat,KIND=kFloat)
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cIf)
      ! No optimization possible

    CASE (cImag)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) =       AIMAG(rComp%zImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = CMPLX(AIMAG(rComp%zImmed(rComp%iImmedSize)),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cLog)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Log(A) gives complex result?
            IF (rComp%dImmed(rComp%iImmedSize) <= 0.0_kFloat) rComp%bIsComplex = TrueVal

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = LOG(rComp%zImmed(rComp%iImmedSize))
            ELSE
                rComp%dImmed(rComp%iImmedSize) = LOG(rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = LOG(rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cLog10)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Log10(A) gives complex result?
            IF (rComp%dImmed(rComp%iImmedSize) < 0.0_kFloat) rComp%bIsComplex = TrueVal

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = LOG(rComp%zImmed(rComp%iImmedSize))/LOG(10.0_kFloat)
            ELSE
                rComp%dImmed(rComp%iImmedSize) = LOG10(rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = LOG(rComp%zImmed(rComp%iImmedSize))/LOG(10.0_kFloat)
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cMax)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                IF (ABS(rComp%zImmed(rComp%iImmedSize)) >=&
                    ABS(rComp%zImmed(rComp%iImmedSize-1))) THEN
                    rComp%zImmed(rComp%iImmedSize-1) = rComp%zImmed(rComp%iImmedSize)
                END IF
            ELSE
                rComp%dImmed(rComp%iImmedSize-1) = MAX(rComp%dImmed(rComp%iImmedSize),&
                                                       rComp%dImmed(rComp%iImmedSize-1))
                rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            END IF
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cMin)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex) THEN
                ErrMsg = 'Invalid complex argument for MAX!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize-1) = MIN(rComp%dImmed(rComp%iImmedSize),&
                                                   rComp%dImmed(rComp%iImmedSize-1))
            rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cReal)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) =       REAL(rComp%zImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = CMPLX(REAL(rComp%zImmed(rComp%iImmedSize)),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cRrand)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex .AND. IsComplex(rComp%zImmed(rComp%iImmedSize))) THEN
                ErrMsg = 'Invalid complex argument for RRAND!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            CALL RANDOM_SEED(SIZE=iaux)
            CALL MemAlloc(p_Irandom1, [ToIndex(iaux)])
            CALL MemAlloc(p_Irandom2, [ToIndex(iaux)])
            CALL RANDOM_SEED(get=p_Irandom1)

            p_Irandom2(:) = 0
            p_Irandom2(1) = INT(rComp%dImmed(rComp%iImmedSize-1))
            CALL RANDOM_SEED(put=p_Irandom2)
            daux = 0.0_kFloat
            DO iaux=1,MAX(1,INT(rComp%dImmed(rComp%iImmedSize)))
                CALL RANDOM_NUMBER(daux)
            END DO

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize-1) = daux
            rComp%zImmed(rComp%iImmedSize-1) = CMPLX(daux,0.0_kFloat,KIND=kFloat)

            CALL RANDOM_SEED(put=p_Irandom1)
            CALL MemFree(p_Irandom1)
            CALL MemFree(p_Irandom2)

            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cSec)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = 1.0_kFloat/COS(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = 1.0_kFloat/COS(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cSech)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = 1.0_kFloat/COSH(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = 1.0_kFloat/COSH(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cSign)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
          rComp%dImmed(rComp%iImmedSize) = SIGN(1.0_kFloat,rComp%dImmed(rComp%iImmedSize))

            ! Compute sign(A) = A./abs(A) following the definition in MATLAB
            rComp%zImmed(rComp%iImmedSize) = rComp%zImmed(rComp%iImmedSize)/&
                                             ABS(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cSin)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = SIN(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = SIN(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cSinh)
      IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
        rComp%dImmed(rComp%iImmedSize) = SINH(rComp%dImmed(rComp%iImmedSize))
        rComp%zImmed(rComp%iImmedSize) = SINH(rComp%zImmed(rComp%iImmedSize))
        CALL RemoveCompiledByte(rComp)
      END IF

    CASE (cSqrt)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Sqrt(A) gives complex result?
            IF (rComp%dImmed(rComp%iImmedSize) < 0.0_kFloat) rComp%bIsComplex = TrueVal

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
                rComp%zImmed(rComp%iImmedSize) = SQRT(rComp%zImmed(rComp%iImmedSize))
            ELSE
                rComp%dImmed(rComp%iImmedSize) = SQRT(rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize) = SQRT(rComp%zImmed(rComp%iImmedSize))
            END IF
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cTan)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = TAN(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = TAN(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cTanh)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = TANH(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = TANH(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

      !------------------------------------------------------------
      ! Misc
      !------------------------------------------------------------
    CASE (cImmed, cJump)
      ! No optimization needed

      !------------------------------------------------------------
      ! Operators
      !------------------------------------------------------------
    CASE (cNeg)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = -(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = -(rComp%zImmed(rComp%iImmedSize))
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAdd)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
             rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize-1) = rComp%dImmed(rComp%iImmedSize-1)+&
                                               rComp%dImmed(rComp%iImmedSize)
            rComp%zImmed(rComp%iImmedSize-1) = rComp%zImmed(rComp%iImmedSize-1)+&
                                               rComp%zImmed(rComp%iImmedSize)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cSub)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize-1) = rComp%dImmed(rComp%iImmedSize-1)-&
                                               rComp%dImmed(rComp%iImmedSize)
            rComp%zImmed(rComp%iImmedSize-1) = rComp%zImmed(rComp%iImmedSize-1)-&
                                               rComp%zImmed(rComp%iImmedSize)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cMul)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize-1) = rComp%dImmed(rComp%iImmedSize-1)*&
                                               rComp%dImmed(rComp%iImmedSize)
            rComp%zImmed(rComp%iImmedSize-1) = rComp%zImmed(rComp%iImmedSize-1)*&
                                               rComp%zImmed(rComp%iImmedSize)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cDiv)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Division by zero?
            IF (rComp%dImmed(rComp%iImmedSize) == 0.0_kFloat) THEN
                ErrMsg = 'Division by zero!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF
            rComp%dImmed(rComp%iImmedSize-1) = rComp%dImmed(rComp%iImmedSize-1)/&
                                               rComp%dImmed(rComp%iImmedSize)

            ! Division by zero?
            IF (rComp%zImmed(rComp%iImmedSize) == CMPLX(0.0_kFloat,0.0_kFloat,KIND=kFloat)) THEN
                ErrMsg = 'Division by zero!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF
            rComp%zImmed(rComp%iImmedSize-1) = rComp%zImmed(rComp%iImmedSize-1)/&
                                               rComp%zImmed(rComp%iImmedSize)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cMod)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex .AND. IsComplex(rComp%zImmed(rComp%iImmedSize))) THEN
                ErrMsg = 'Invalid complex argument for MOD!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! MOD(A,0) not available
            IF (rComp%dImmed(rComp%iImmedSize) == 0.0_kFloat) THEN
                ErrMsg = 'Invalid argument for MOD!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize-1) = MOD(rComp%dImmed(rComp%iImmedSize-1),&
                                                   rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cPow)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize-1) = rComp%dImmed(rComp%iImmedSize-1)**&
                                               rComp%dImmed(rComp%iImmedSize)
            rComp%zImmed(rComp%iImmedSize-1) = rComp%zImmed(rComp%iImmedSize-1)**&
                                               rComp%zImmed(rComp%iImmedSize)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cEqual)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize-1) = LogcToDble((REAL(rComp%zImmed(rComp%iImmedSize-1)) ==&
                                                               REAL(rComp%zImmed(rComp%iImmedSize))) .AND.&
                                                              (AIMAG(rComp%zImmed(rComp%iImmedSize-1)) ==&
                                                               AIMAG(rComp%zImmed(rComp%iImmedSize))))
                rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            ELSE
                ! Copy data from real-valued case
                rComp%dImmed(rComp%iImmedSize-1) = LogcToDble(rComp%dImmed(rComp%iImmedSize-1) ==&
                                                              rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            END IF
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cNEqual)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Different treatment for real- and complex-valued case
            IF (rComp%bIsComplex) THEN
                rComp%dImmed(rComp%iImmedSize-1) = LogcToDble((REAL(rComp%zImmed(rComp%iImmedSize-1)) /=&
                                                               REAL(rComp%zImmed(rComp%iImmedSize)))  .OR.&
                                                              (AIMAG(rComp%zImmed(rComp%iImmedSize-1)) /=&
                                                               AIMAG(rComp%zImmed(rComp%iImmedSize))))
                rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            ELSE
                ! Copy data from real-valued case
                rComp%dImmed(rComp%iImmedSize-1) = LogcToDble(rComp%dImmed(rComp%iImmedSize-1) /=&
                                                              rComp%dImmed(rComp%iImmedSize))
                rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            END IF
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cLess)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex .AND. IsComplex(rComp%zImmed(rComp%iImmedSize))) THEN
                ErrMsg = 'Invalid complex argument for "<" Operator!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize-1) = LogcToDble(rComp%dImmed(rComp%iImmedSize-1) <&
                                                          rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cLessOrEq)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex .AND. IsComplex(rComp%zImmed(rComp%iImmedSize))) THEN
                ErrMsg = 'Invalid complex argument for "<=" Operator!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize-1) = LogcToDble(rComp%dImmed(rComp%iImmedSize-1) <=&
                                                          rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cGreater)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex .AND. IsComplex(rComp%zImmed(rComp%iImmedSize))) THEN
                ErrMsg = 'Invalid complex argument for ">" Operator!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize-1) = LogcToDble(rComp%dImmed(rComp%iImmedSize-1) >&
                                                          rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cGreaterOrEq)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex .AND. IsComplex(rComp%zImmed(rComp%iImmedSize))) THEN
                ErrMsg = 'Invalid complex argument for ">=" Operator!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize-1) = LogcToDble(rComp%dImmed(rComp%iImmedSize-1) >=&
                                                          rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cAnd)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex .AND. IsComplex(rComp%zImmed(rComp%iImmedSize))) THEN
                ErrMsg = 'Invalid complex argument for "AND" Operator!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize-1) = LogcToDble(DbleToLogc(rComp%dImmed(rComp%iImmedSize-1)) .AND.&
                                                          DbleToLogc(rComp%dImmed(rComp%iImmedSize)))
            rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cOr)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed .AND.&
            rComp%iByteCode(rComp%iByteCodeSize-2) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex .AND. IsComplex(rComp%zImmed(rComp%iImmedSize))) THEN
                ErrMsg = 'Invalid complex argument for "OR" Operator!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize-1) = LogcToDble(DbleToLogc(rComp%dImmed(rComp%iImmedSize-1)) .OR.&
                                                          DbleToLogc(rComp%dImmed(rComp%iImmedSize)))
            rComp%zImmed(rComp%iImmedSize-1) = CMPLX(rComp%dImmed(rComp%iImmedSize-1),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledImmediate(rComp)
            CALL RemoveCompiledByte(rComp)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cNot)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex .AND. IsComplex(rComp%zImmed(rComp%iImmedSize))) THEN
                ErrMsg = 'Invalid complex argument for "NOT" Operator!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize) = LogcToDble(.NOT.DbleToLogc(rComp%dImmed(rComp%iImmedSize)))
            rComp%zImmed(rComp%iImmedSize) = CMPLX(rComp%dImmed(rComp%iImmedSize),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledByte(rComp)
        END IF

      !------------------------------------------------------------
      ! Degrees-radians conversion
      !------------------------------------------------------------
    CASE (cDeg)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex .AND. IsComplex(rComp%zImmed(rComp%iImmedSize))) THEN
                ErrMsg = 'Invalid complex argument for Deg!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            ! Copy data from real-valued case
            rComp%dImmed(rComp%iImmedSize) = RadToDeg(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = CMPLX(rComp%dImmed(rComp%iImmedSize),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledByte(rComp)
        END IF

    CASE (cRad)
        IF (rComp%iByteCode(rComp%iByteCodeSize-1) == cImmed) THEN

            ! Not available for complex-valued case
            IF (rComp%bIsComplex .AND. IsComplex(rComp%zImmed(rComp%iImmedSize))) THEN
                ErrMsg = 'Invalid complex argument for Rad!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF

            rComp%dImmed(rComp%iImmedSize) = DegToRad(rComp%dImmed(rComp%iImmedSize))
            rComp%zImmed(rComp%iImmedSize) = CMPLX(rComp%dImmed(rComp%iImmedSize),0.0_kFloat,KIND=kFloat)
            CALL RemoveCompiledByte(rComp)
        END IF
    END SELECT

END SUBROUTINE AddCompiledByte

!**************************************************************************************

SUBROUTINE RemoveCompiledByte(rComp)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove last compiled byte from bytecode.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    rComp%iByteCode(rComp%iByteCodeSize) = 0
    rComp%iByteCodeSize = rComp%iByteCodeSize - 1

END SUBROUTINE RemoveCompiledByte

!**************************************************************************************

SUBROUTINE AddImmediateDouble(rComp, dImmediate)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add double-valued immediate.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Value of byte to be added
    tFloat,                 INTENT(IN)       :: dImmediate
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    rComp%iImmedSize               = rComp%iImmedSize + 1
    rComp%dImmed(rComp%iImmedSize) = dImmediate
    rComp%zImmed(rComp%iImmedSize) = CMPLX(dImmediate, 0.0_kFloat, KIND=kFloat)

END SUBROUTINE AddImmediateDouble

!**************************************************************************************

SUBROUTINE AddImmediateComplex(rComp, zImmediate)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add complex-valued immediate

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Value of byte to be added
    tCmplx,              INTENT(IN)       :: zImmediate
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    rComp%iImmedSize               = rComp%iImmedSize + 1
    rComp%zImmed(rComp%iImmedSize) = SYS_INFINITY_kFloat
    rComp%zImmed(rComp%iImmedSize) = zImmediate

    ! Complex-valued case
    rComp%bIsComplex = TrueVal

END SUBROUTINE AddImmediateComplex

!**************************************************************************************

SUBROUTINE RemoveCompiledImmediate(rComp)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove last compiled immediate from immediate stack

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    rComp%dImmed(rComp%iImmedSize) = 0.0_kFloat
    rComp%zImmed(rComp%iImmedSize) = CMPLX(0.0_kFloat, 0.0_kFloat, KIND=kFloat)
    rComp%iImmedSize               = rComp%iImmedSize - 1

END SUBROUTINE RemoveCompiledImmediate

!**************************************************************************************

SUBROUTINE AddFunctionOpcode(rComp, iOpcode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add compiled byte to bytecode

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Value of opcode to be added
    tSInt8,                INTENT(IN)       :: iOpcode
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (rComp%bUseDegreeConversion) THEN
        SELECT CASE (iOpcode)
        CASE (cCos, cCosh, cCot, cCoth,&
              cCsc, cCsch, cSec, cSech,&
              cSin, cSinh, cTan, cTanh)
            CALL AddCompiledByte(rComp, cRad)
        END SELECT
    END IF

    CALL AddCompiledByte(rComp, iOpcode)

    IF (rComp%bUseDegreeConversion) THEN
        SELECT CASE (iOpcode)
        CASE (cAcos, cAcosh, cAcot, cAcoth,&
              cAcsc, cACsch, cAsec, cAsech,&
              cAsin, cAsinh, cAtan, cAtanh, cAtan2)
            CALL AddCompiledByte(rComp, cDeg)
        END SELECT
    END IF

END SUBROUTINE AddFunctionOpcode

!**************************************************************************************

FUNCTION RealNum(sFunctionString, iBegin, iNext, bError) RESULT(dResult)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get real number from string. <br>
    ! Format: [blanks][+|-][nnn][.nnn][e|E|d|D[+|-]nnn]

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! String
    tCharStar,          INTENT(IN)  :: sFunctionString
    ! OPTIONAL: Start position of real number
    tSInt32,  OPTIONAL, INTENT(OUT) :: iBegin
    ! OPTIONAL: 1st character after real number
    tSInt32,  OPTIONAL, INTENT(OUT) :: iNext
    ! OPTIONAL: Error flag
    tLogical, OPTIONAL, INTENT(OUT) :: bError
    ! Real number
    tFloat                           :: dResult

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: ib, in, istat
    tLogical    :: Bflag,               & ! .T. at begin of number in string
                   InMan,               & ! .T. in mantissa of number
                   Pflag,               & ! .T. after 1st '.' encountered
                   Eflag,               & ! .T. at exponent identifier 'eEdD'
                   InExp,               & ! .T. in exponent of number
                   DInMan,              & ! .T. if at least 1 digit in mant.
                   DInExp,              & ! .T. if at least 1 digit in exp.
                   err                    ! Local error flag

!** FLOW

    Bflag=TrueVal; InMan=FalseVal; Pflag=FalseVal; Eflag=FalseVal; InExp=FalseVal
    DInMan=FalseVal; DInExp=FalseVal
    ib   = 1
    in   = 1
    DO WHILE (in <= LEN_TRIM(sFunctionString))
        SELECT CASE (sFunctionString(in:in))
        CASE (' ') ! Only leading blanks permitted
            ib = ib + 1
            IF (InMan .OR. Eflag .OR. InExp) EXIT
        CASE ('+','-') ! Permitted only
            IF     (Bflag) THEN
                InMan=TrueVal; Bflag=FalseVal ! - at beginning of mantissa
            ELSE IF (Eflag) THEN
                InExp=TrueVal; Eflag=FalseVal ! - at beginning of exponent
            ELSE
                EXIT ! - otherwise CALL sys_halt()
            END IF
        CASE ('0':'9') ! Mark
            IF     (Bflag) THEN
                InMan=TrueVal; Bflag=FalseVal ! - beginning of mantissa
            ELSE IF (Eflag) THEN
                InExp=TrueVal; Eflag=FalseVal ! - beginning of exponent
            END IF
            IF (InMan) DInMan=TrueVal ! Mantissa contains digit
            IF (InExp) DInExp=TrueVal ! Exponent contains digit
        CASE ('.')
            IF     (Bflag) THEN
                Pflag=TrueVal ! - mark 1st appearance of '.'
                InMan=TrueVal; Bflag=FalseVal !   mark beginning of mantissa
            ELSE IF (InMan .AND..NOT.Pflag) THEN
                Pflag=TrueVal ! - mark 1st appearance of '.'
            ELSE
                EXIT ! - otherwise CALL sys_halt()
            END IF
        CASE ('e','E','d','D') ! Permitted only
            IF (InMan) THEN
                Eflag=TrueVal; InMan=FalseVal ! - following mantissa
            ELSE
                EXIT ! - otherwise CALL sys_halt()
            END IF
        CASE DEFAULT
            EXIT ! CALL sys_halt() at all other characters
        END SELECT
        in = in+1
    END DO
    err = (ib > in-1) .OR. (.NOT.DInMan) .OR.&
          ((Eflag.OR.InExp).AND..NOT.DInExp)
    IF (err) THEN
        dResult = 0.0_kFloat
    ELSE
        READ(sFunctionString(ib:in-1),*, IOSTAT=istat) dResult
        err = istat /= 0
    END IF
    IF (PRESENT(iBegin)) iBegin = ib
    IF (PRESENT(iNext))  iNext  = in
    IF (PRESENT(bError)) bError = err

END FUNCTION RealNum

!**************************************************************************************

RECURSIVE FUNCTION FunctionSize(rfParser, sFunctionString) RESULT(iSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the size of the total function including external expressions

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    CLASS(MathExpParser), INTENT(IN)    :: rfParser
    !% Function string
    tCharStar,            INTENT(IN)    :: sFunctionString
    !% Size of function string
    tSInt32                             :: iSize

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'FunctionSize'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32         :: Ind, n
    tCharLen(1)     :: c

!** FLOW

    ! Determine size of given expression
    iSize = LEN_TRIM(sFunctionString)

    ! "Parse" string for externally defined expressions
    DO Ind = 1, iSize
        c = sFunctionString(Ind:Ind)
        IF (c == '@') THEN
            n = ExpressionIndex(rfParser, sFunctionString(Ind:))
            IF (n == 0) THEN
                ErrMsg = 'Invalid Expression!'
                CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
                RETURN
            END IF
            iSize = iSize+FunctionSize(rfParser, rfParser%cExpressionString(n))
        END IF
    END DO

END FUNCTION FunctionSize

!**************************************************************************************

RECURSIVE FUNCTION CompileExpression(rfParser, rComp, sFunctionString, Ind, sVariables,&
                                     bStopAtComma) RESULT(Ind2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compiles the expression string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser),   INTENT(IN)       :: rfParser
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Function substring representing the expression
    tCharStar,             INTENT(IN)       :: sFunctionString
    !% Begin position substring
    tSInt32,               INTENT(IN)       :: Ind
    !% Array with variable names
    tCharStar,             INTENT(IN)       :: sVariables(:)
    !% OPTIONAL: stop at comma
    tLogical, OPTIONAL,    INTENT(IN)       :: bStopAtComma
    !% result
    tSInt32                                 :: Ind2

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: ifunctionLength

!** FLOW

    ifunctionLength = LEN_TRIM(sFunctionString)

    Ind2 = CompileOr(rfParser, rComp, sFunctionString, Ind, sVariables)
    IF (Ind2 > ifunctionLength) RETURN

    IF (PRESENT(bStopAtComma)) THEN
        IF (bStopAtComma) RETURN
    END IF

    DO WHILE (sFunctionString(Ind2:Ind2) == ',')
        Ind2 = CompileOr(rfParser, rComp, sFunctionString, Ind2+1, sVariables)
        IF (Ind2 > ifunctionLength) RETURN
    END DO

END FUNCTION CompileExpression

!**************************************************************************************

RECURSIVE FUNCTION CompileOr(rfParser, rComp, sFunctionString, Ind, sVariables) RESULT(Ind2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compiles 'OR'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser),   INTENT(IN)       :: rfParser
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Function substring
    tCharStar,             INTENT(IN)       :: sFunctionString
    !% Begin position substring
    tSInt32,               INTENT(IN)       :: Ind
    !% Array with variable names
    tCharStar,             INTENT(IN)       :: sVariables(:)
    !% result
    tSInt32                                 :: Ind2

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: ifunctionLength

!** FLOW

    ifunctionLength = LEN_TRIM(sFunctionString)

    Ind2 = CompileAnd(rfParser, rComp, sFunctionString, Ind, sVariables)
    IF (Ind2 > ifunctionLength) RETURN

    DO WHILE(sFunctionString(Ind2:Ind2) == '|')
        Ind2 = CompileAnd(rfParser, rComp, sFunctionString, Ind2+1, sVariables)

        CALL AddCompiledByte(rComp, cOr)
        rComp%iStackPtr = rComp%iStackPtr - 1
        IF (Ind2 > ifunctionLength) RETURN
    END DO

END FUNCTION CompileOr

!**************************************************************************************

RECURSIVE FUNCTION CompileAnd(rfParser, rComp, sFunctionString, Ind, sVariables) RESULT(Ind2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compiles 'AND'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser),   INTENT(IN)       :: rfParser
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Function substring
    tCharStar,             INTENT(IN)       :: sFunctionString
    !% Begin position substring
    tSInt32,               INTENT(IN)       :: Ind
    !% Array with variable names
    tCharStar,             INTENT(IN)       :: sVariables(:)
    !% result
    tSInt32                                 :: Ind2

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: ifunctionLength

!** FLOW

    ifunctionLength = LEN_TRIM(sFunctionString)

    Ind2 = CompileComparison(rfParser, rComp, sFunctionString, Ind, sVariables)
    IF (Ind2 > ifunctionLength) RETURN

    DO WHILE(sFunctionString(Ind2:Ind2) == '&')
      Ind2 = CompileComparison(rfParser, rComp, sFunctionString, Ind2+1, sVariables)

      CALL AddCompiledByte(rComp, cAnd)
      rComp%iStackPtr = rComp%iStackPtr - 1
      IF (Ind2 > ifunctionLength) RETURN
    END DO

END FUNCTION CompileAnd

!**************************************************************************************

RECURSIVE FUNCTION CompileComparison(rfParser, rComp, sFunctionString, Ind, sVariables) RESULT(Ind2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compiles comparison operators.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser),   INTENT(IN)       :: rfParser
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Function substring
    tCharStar,             INTENT(IN)       :: sFunctionString
    !% Begin position substring
    tSInt32,               INTENT(IN)       :: Ind
    !% Array with variable names
    tCharStar,             INTENT(IN)       :: sVariables(:)
    !% result
    tSInt32                                 :: Ind2

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(1)     :: c
    tSInt8           :: iopSize
    tSInt32         :: ifunctionLength

!** FLOW

    ifunctionLength = LEN_TRIM(sFunctionString)

    Ind2 = CompileAddition(rfParser, rComp, sFunctionString, Ind, sVariables)
    IF (Ind2 > ifunctionLength) RETURN

    c=sFunctionString(Ind2:Ind2)
    DO WHILE(c == '=' .OR. c == '<' .OR. c == '>' .OR. c == '!')
        iopSize = MERGE(2, 1, sFunctionString(Ind2+1:Ind2+1) == '=')
        Ind2 = CompileAddition(rfParser, rComp, sFunctionString, Ind2+iopSize, sVariables)

        SELECT CASE (c)
        CASE ('=')
            CALL AddCompiledByte(rComp, cEqual)

        CASE ('<')
            CALL AddCompiledByte(rComp, MERGE(cLess, cLessOrEq, iopSize == 1))

        CASE ('>')
            CALL AddCompiledByte(rComp, MERGE(cGreater, cGreaterOrEq, iopSize == 1))

        CASE ('!')
            CALL AddCompiledByte(rComp, cNEqual)
        END SELECT
        rComp%iStackPtr = rComp%iStackPtr - 1

        IF (Ind2 > ifunctionLength) RETURN
        c = sFunctionString(Ind2:Ind2)
    END DO

END FUNCTION CompileComparison

!**************************************************************************************

RECURSIVE FUNCTION CompileAddition(rfParser, rComp, sFunctionString, Ind, sVariables) RESULT(Ind2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compiles '+' and '-'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser),   INTENT(IN)       :: rfParser
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Function substring
    tCharStar,             INTENT(IN)       :: sFunctionString
    !% Begin position substring
    tSInt32,               INTENT(IN)       :: Ind
    !% Array with variable names
    tCharStar,             INTENT(IN)       :: sVariables(:)
    !% result
    tSInt32                                 :: Ind2

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(1)     :: c
    tSInt32         :: ifunctionLength

!** FLOW

    ifunctionLength = LEN_TRIM(sFunctionString)

    Ind2 = CompileMult(rfParser, rComp, sFunctionString, Ind, sVariables)
    IF (Ind2 > ifunctionLength) RETURN

    c=sFunctionString(Ind2:Ind2)
    DO WHILE(c == '+' .OR. c == '-')
        Ind2 = CompileMult(rfParser, rComp, sFunctionString, Ind2+1, sVariables)

        CALL AddCompiledByte(rComp, MERGE(cAdd, cSub, c == '+'))
        rComp%iStackPtr = rComp%iStackPtr - 1

        IF (Ind2 > ifunctionLength) RETURN
        c=sFunctionString(Ind2:Ind2)
    END DO

END FUNCTION CompileAddition

!**************************************************************************************

RECURSIVE FUNCTION CompileMult(rfParser, rComp, sFunctionString, Ind, sVariables) RESULT(Ind2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compiles '*', '/' and '%'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser),   INTENT(IN)       :: rfParser
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Function substring
    tCharStar,             INTENT(IN)       :: sFunctionString
    !% Begin position substring
    tSInt32,               INTENT(IN)       :: Ind
    !% Array with variable names
    tCharStar,             INTENT(IN)       :: sVariables(:)
    !% result
    tSInt32                                 :: Ind2

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(1)     :: c
    tSInt32         :: ifunctionLength

!** FLOW

    ifunctionLength = LEN_TRIM(sFunctionString)

    Ind2 = CompileUnaryMinus(rfParser, rComp, sFunctionString, Ind, sVariables)
    IF (Ind2 > ifunctionLength) RETURN

    c=sFunctionString(Ind2:Ind2)
    DO WHILE(c == '*' .OR. c == '/' .OR. c == '%')
        Ind2 = CompileUnaryMinus(rfParser, rComp, sFunctionString, Ind2+1, sVariables)

        SELECT CASE (c)
        CASE ('*')
            CALL AddCompiledByte(rComp, cMul)

        CASE ('/')
            CALL AddCompiledByte(rComp, cDiv)

        CASE ('%')
            CALL AddCompiledByte(rComp, cMod)

        END SELECT
        rComp%iStackPtr = rComp%iStackPtr - 1

        IF (Ind2 > ifunctionLength) RETURN
        c=sFunctionString(Ind2:Ind2)
    END DO

END FUNCTION CompileMult

!**************************************************************************************

RECURSIVE FUNCTION CompileUnaryMinus(rfParser, rComp, sFunctionString, Ind, sVariables) RESULT(Ind2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compiles unary '-'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser),   INTENT(IN)       :: rfParser
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Function substring
    tCharStar,             INTENT(IN)       :: sFunctionString
    !% Begin position substring
    tSInt32,               INTENT(IN)       :: Ind
    !% Array with variable names
    tCharStar,             INTENT(IN)       :: sVariables(:)
    !% result
    tSInt32                                 :: Ind2

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(1)     :: c
    tSInt32         :: ifunctionLength

!** FLOW

    ifunctionLength = LEN_TRIM(sFunctionString)

    c=sFunctionString(Ind:Ind)
    IF (c == '-' .OR. c == '!') THEN
        Ind2 = Ind + 1
        IF (Ind2 > ifunctionLength) RETURN
        Ind2 = CompilePow(rfParser, rComp, sFunctionString, Ind2, sVariables)

        ! If we are negating a constant, negate the constant itself
        IF (c == '-' .AND. rComp%iByteCode(rComp%iByteCodeSize) == cImmed) THEN
            rComp%dImmed(rComp%iImmedSize) = -rComp%dImmed(rComp%iImmedSize)

        ! If we are negating a negation, we can remove both
        ELSE IF (c == '-' .AND. rComp%iByteCode(rComp%iByteCodeSize) == cNeg) THEN
            CALL RemoveCompiledByte(rComp)

        ELSE
            CALL AddCompiledByte(rComp, MERGE(cNeg, cNot, c == '-'))

        END IF
        RETURN
    END IF

    Ind2 = CompilePow(rfParser, rComp, sFunctionString, Ind, sVariables)

END FUNCTION CompileUnaryMinus

!**************************************************************************************

RECURSIVE FUNCTION CompilePow(rfParser, rComp, sFunctionString, Ind, sVariables) RESULT(Ind2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compiles '^'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser),   INTENT(IN)       :: rfParser
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Function substring
    tCharStar,             INTENT(IN)       :: sFunctionString
    !% Begin position substring
    tSInt32,               INTENT(IN)       :: Ind
    !% Array with variable names
    tCharStar,             INTENT(IN)       :: sVariables(:)
    !% result
    tSInt32                                 :: Ind2

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: ifunctionLength

!** FLOW

    ifunctionLength = LEN_TRIM(sFunctionString)

    Ind2 = CompileElement(rfParser, rComp, sFunctionString, Ind, sVariables)
    IF (Ind2 > ifunctionLength) RETURN

    DO WHILE(sFunctionString(Ind2:Ind2) == '^')
        Ind2 = CompileUnaryMinus(rfParser, rComp, sFunctionString, Ind2+1, sVariables)

        CALL AddCompiledByte(rComp, cPow)
        rComp%iStackPtr = rComp%iStackPtr - 1
        IF (Ind2 > ifunctionLength) RETURN
    END DO

END FUNCTION CompilePow

!**************************************************************************************

RECURSIVE FUNCTION CompileElement(rfParser, rComp, sFunctionString, Ind, sVariables) RESULT(Ind2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compiles element.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser),   INTENT(IN)       :: rfParser
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Function substring
    tCharStar,             INTENT(IN)       :: sFunctionString
    !% Begin position substring
    tSInt32,               INTENT(IN)       :: Ind
    !% Array with variable names
    tCharStar,             INTENT(IN)       :: sVariables(:)
    !% result
    tSInt32                                 :: Ind2

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'CompileElement'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(1)     :: c
    tFloat           :: dnumber
    tSInt8           :: n
    tSInt32         :: Ind1, Ind0, ib, in, nparams
    tLogical        :: bError

!** FLOW

    Ind1 = Ind
    c = sFunctionString(Ind1:Ind1)
    IF (c == '(') THEN
        Ind1 = CompileExpression(rfParser, rComp, sFunctionString, Ind1+1, sVariables, FalseVal)
        Ind2 = Ind1 + 1     ! sFunctionString(Ind1:Ind1) is ')'
        RETURN
    END IF

    ! Check for numbers
    IF (SCAN(c,'0123456789,') > 0) THEN
        dnumber = RealNum(sFunctionString(Ind1:), ib, in, bError)
        IF (bError) THEN
            ErrMsg = 'Invalid number format!'
            CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
            RETURN
        END IF
        Ind2 = Ind1 + in - 1

        CALL AddImmediateDouble(rComp, dnumber)
        CALL AddCompiledByte(rComp, cImmed)
        CALL IncStackPtr(rComp)
        RETURN

    ELSE
        ! Then string must be function, variable or constant

        ! Check for mathematical functions
        n = MathFunctionIndex(sFunctionString(Ind1:))
        IF (n > 0) THEN
            Ind2 = Ind1 + LEN_TRIM(Funcs(n))

            ! Check for IF-THEN-ELSE
            IF (n == cIf) THEN
                Ind2 = CompileIf(rfParser, rComp, sFunctionString, Ind2+1, sVariables)
                ! IF-THEN-ELSE cannot be vectorized, note that!
                rComp%bIsVectorizable = FalseVal
                RETURN
            END IF

            nparams = MathFunctionParameters(n)
            Ind2 = CompileFunctionParameters(rfParser, rComp, sFunctionString, Ind2+1, sVariables, nparams)
            CALL AddFunctionOpcode(rComp, n)
            RETURN
        END IF

        ! Check for predefined constant
        n = ConstantIndex(rfParser, sFunctionString(Ind1:))
        IF (n > 0) THEN
            Ind2 = Ind1 + LEN_TRIM(rfParser%cConstantName(n)) + 1
            CALL AddImmediateDouble(rComp, rfParser%dConstantValue(n))
            CALL AddCompiledByte(rComp, cImmed)
            CALL IncStackPtr(rComp)
            RETURN
        END IF

        ! Check for predefined Expressions
        n = ExpressionIndex(rfParser, sFunctionString(Ind1:))
        IF (n > 0) THEN
            Ind2 = Ind1 + LEN_TRIM(rfParser%cExpressionName(n)) + 1

            ! Recursively compile the given Expression
            Ind0 = CompileExpression(rfParser, rComp, rfParser%cExpressionString(n), 1, sVariables)

            ! Proceed with compilation of mathematical function Func afterwards
            RETURN
        END IF

        ! Check for variables
        n = VariableIndex(sFunctionString(Ind1:), sVariables, ib, in)
        IF (n > 0) n = VarBegin + n - 1
        CALL AddCompiledByte(rComp, n)
        CALL IncStackPtr(rComp)
        Ind2 = Ind1+in-1
        RETURN
    END IF
    
    ErrMsg = 'An unexpected error occurred!'
    CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
    RETURN

END FUNCTION CompileElement

!**************************************************************************************

RECURSIVE FUNCTION CompileFunctionParameters(rfParser, rComp, sFunctionString, Ind, sVariables,&
                                             nparams) RESULT(Ind2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compiles function parameters

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser),   INTENT(IN)       :: rfParser
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    ! Function substring
    tCharStar,             INTENT(IN)       :: sFunctionString
    !% Begin position substring
    tSInt32,               INTENT(IN)       :: Ind
    !% Array with variable names
    tCharStar,             INTENT(IN)       :: sVariables(:)
    !% Number of required parameters
    tSInt32,               INTENT(IN)       :: nparams
    !% result
    tSInt32                                 :: Ind2

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'CompileFunctionParameters'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: iStackPtr

!** FLOW

    Ind2 = Ind
    IF (nparams > 0) THEN

        iStackPtr = rComp%iStackPtr
        Ind2 = CompileExpression(rfParser, rComp, sFunctionString, Ind, sVariables, FalseVal)

        IF (rComp%iStackPtr /= iStackPtr+nparams) THEN
            ErrMsg = 'Illegal number of parameters to function!'
            CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
            RETURN
        END IF

        rComp%iStackPtr = rComp%iStackPtr - (nparams-1)

    ELSE

        CALL IncStackPtr(rComp)

    END IF
    Ind2 = Ind2 + 1

END FUNCTION CompileFunctionParameters

!**************************************************************************************

RECURSIVE FUNCTION CompileIf(rfParser, rComp, sFunctionString, Ind, sVariables) RESULT(Ind2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compiles if()

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% Function parser
    TYPE(MathExpParser),   INTENT(IN)       :: rfParser
    !% Function parser component
    TYPE(ParserComponent), INTENT(INOUT)    :: rComp
    !% Function substring
    tCharStar,             INTENT(IN)       :: sFunctionString
    !% Begin position substring
    tSInt32,               INTENT(IN)       :: Ind
    !% Array with variable names
    tCharStar,             INTENT(IN)       :: sVariables(:)
    !% result
    tSInt32                                 :: Ind2

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'CompileIf'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: ifunctionLength, curiByteCodeSize, curiByteCodeSize2, curiImmedSize2

!** FLOW

    ifunctionLength = LEN_TRIM(sFunctionString)

    Ind2 = CompileExpression(rfParser, rComp, sFunctionString, Ind, sVariables, TrueVal) ! Condition branch
    IF (Ind2 > ifunctionLength) RETURN

    IF (sFunctionString(Ind2:Ind2) /= ',') THEN
        ErrMsg = 'Illegal number of parameters to function!'
        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
        RETURN
    END IF
    CALL AddCompiledByte(rComp, cIf)
    curiByteCodeSize = rComp%iByteCodeSize
    CALL AddCompiledByte(rComp, 0_kInt8) ! Jump Index will be set below
    CALL AddCompiledByte(rComp, 0_kInt8) ! Immed jump Index will be set below
    rComp%iStackPtr = rComp%iStackPtr - 1

    Ind2 = CompileExpression(rfParser, rComp, sFunctionString, Ind2+1, sVariables, TrueVal) ! Then branch
    IF (Ind2 > ifunctionLength) RETURN

    IF (sFunctionString(Ind2:Ind2) /= ',') THEN
        ErrMsg = 'Illegal number of parameters to function!'
        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
        RETURN
    END IF
    CALL AddCompiledByte(rComp, cJump)
    curiByteCodeSize2 = rComp%iByteCodeSize
    curiImmedSize2 = rComp%iImmedSize
    CALL AddCompiledByte(rComp, 0_kInt8) ! Jump Index will be set below
    CALL AddCompiledByte(rComp, 0_kInt8) ! Immed jump Index will be set below
    rComp%iStackPtr = rComp%iStackPtr - 1

    Ind2 = CompileExpression(rfParser, rComp, sFunctionString, Ind2+1, sVariables, TrueVal) ! Else branch
    IF (Ind2 > ifunctionLength) RETURN

    IF (sFunctionString(Ind2:Ind2) /= ')') THEN
        ErrMsg = 'Illegal number of parameters to function!'
        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
        RETURN
    END IF

    ! Set jump Indices
    IF (ASSOCIATED(rComp%iByteCode)) THEN
        rComp%iByteCode(curiByteCodeSize+1)  = curiByteCodeSize2 + 2
        rComp%iByteCode(curiByteCodeSize+2)  = curiImmedSize2 + 1
        rComp%iByteCode(curiByteCodeSize2+1) = rComp%iByteCodeSize
        rComp%iByteCode(curiByteCodeSize2+2) = rComp%iImmedSize + 1
    END IF

    Ind2 = Ind2 + 1

END FUNCTION CompileIf

!**************************************************************************************

ELEMENTAL FUNCTION DbleToLogc(d) RESULT(l)

!** PURPOSE OF THIS SUBROUTINE:
    ! This function transforms a Double into a Logical

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! Double variable
    tFloat, INTENT(IN)   :: d
    ! Logical variable
    LOGICAL             :: l
    
!** FLOW

    ! Convert all nonzero double values to TrueVal and 0.0_kFloat to
    ! FalseVal This behavior is consistent with the MATLAB
    ! implementation.
    l = .NOT.(ABS(d) <= 1.0E-12)

    ! Previous implementation
    ! l = (abs(1-d) <= 1.0E-12)

END FUNCTION DbleToLogc

!**************************************************************************************

ELEMENTAL FUNCTION CmplToLogc(c) RESULT(l)

!** PURPOSE OF THIS SUBROUTINE:
    ! This function transforms a Complex into a Logical

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! Complex variable
    tCmplx, INTENT(IN)  :: c
    ! Logical variable
    tLogical            :: l

!** FLOW

    ! Convert all nonzero complex values to TrueVal and (0.0_kFloat,0.0_kFloat)
    ! to FalseVal This behavior is consistent with the MATLAB
    ! implementation.
    l = .NOT.(ABS(c) <= 1.0E-12)

    ! Previous implementation
    ! l = (abs(1-c) <= 1.0E-12)

END FUNCTION CmplToLogc

!**************************************************************************************

ELEMENTAL FUNCTION LogcToDble(l) RESULT(d)

!** PURPOSE OF THIS SUBROUTINE:
    ! This function transforms a Logical into a Double

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! Logical variable
    tLogical, INTENT(IN)    :: l
    ! Double variable
    tFloat                  :: d

!** FLOW

    d = MERGE(1.0_kFloat, 0.0_kFloat, l)

END FUNCTION LogcToDble

!**************************************************************************************

ELEMENTAL FUNCTION DegToRad(d) RESULT(r)

!** PURPOSE OF THIS SUBROUTINE:
    ! This function converts DEG to RAD

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! DEG
    tFloat, INTENT(IN)  :: d
    ! RAD
    tFloat              :: r

!** FLOW

    r = d * (3.141592653589793115997963468544185161590576171875_kFloat / 180._kFloat)

END FUNCTION DegToRad

!**************************************************************************************

ELEMENTAL FUNCTION RadToDeg(r) RESULT(d)

!** PURPOSE OF THIS SUBROUTINE:
    ! This function converts RAD to DEG

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! RAD
    tFloat, INTENT(IN)  :: r
    ! DEG
    tFloat              :: d

!** FLOW

    d = r * (180._kFloat / 3.141592653589793115997963468544185161590576171875_kFloat)

END FUNCTION RadToDeg

!**************************************************************************************

ELEMENTAL FUNCTION IsComplex(c) RESULT(l)

!** PURPOSE OF THIS SUBROUTINE:
    ! This function returns TrueVal if the imaginary part of the given
    ! value c is not zero. Otherwise it returns FalseVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! complex value
    tCmplx, INTENT(IN)  :: c
    ! TrueVal if the imaginary part of c is not zero
    tLogical        :: l

!** FLOW

    l = (AIMAG(c)/=0.0_kFloat)

END FUNCTION IsComplex

!**************************************************************************************

ELEMENTAL FUNCTION IsReal(c) RESULT(l)

!** PURPOSE OF THIS SUBROUTINE:
    ! This function returns TrueVal if the imaginary part of the given
    ! value c is zero. Otherwise it returns FalseVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! complex value
    tCmplx, INTENT(IN)  :: c
    ! TrueVal if the imaginary part of c is zero
    tLogical            :: l

!** FLOW

    l = (AIMAG(c)==0.0_kFloat)

END FUNCTION IsReal

!**************************************************************************************

SUBROUTINE EvalFunctionScDble(rComp, Dstack, dValue, EvalErrType, dResult)

!** PURPOSE OF THIS SUBROUTINE:
    ! Evaluate bytecode for the values passed in array Val(:).
    ! Assume that all intermediate values and the final result
    ! dResult is real-valued.
    !
    ! REMARK: If intermediate values become complex-valued then this
    ! subroutine exists with a non-zero error code EvalErrType.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! Component of function parser
    TYPE(ParserComponent), INTENT(IN)       :: rComp
    ! Variable values
    tFloat,                INTENT(IN)       :: dValue(:)
    ! Stack memory
    tFloat,                INTENT(INOUT)    :: Dstack(:)
    ! Error code for function evaluation
    tSInt32,               INTENT(OUT)      :: EvalErrType
    ! Evaluated function
    tFloat,                INTENT(OUT)      :: dResult

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32                 :: iinstPtr, iStackPtr, idataPtr
    tSInt32                 :: ijumpAddr, iimmedAddr
    tFloat                  :: daux
    tSInt32                 :: iaux
    tSInt32,  ALLOCATABLE   :: p_Irandom1(:), p_Irandom2(:)

!** FLOW

    ! Initialization
    idataPtr  = 1
    iStackPtr = 0
    iinstPtr  = 0

    ! Initialize error type
    EvalErrType = No_Error

    ! Repeat until complete bytecode has been processed
    DO WHILE(iinstPtr < rComp%iByteCodeSize)
        iinstPtr = iinstPtr + 1

        ! What KIND of bytecode are we?
        SELECT CASE (rComp%iByteCode(iinstPtr))
        !------------------------------------------------------------
        ! Functions
        !------------------------------------------------------------
        CASE (cAbs)
            Dstack(iStackPtr) = ABS(Dstack(iStackPtr))

        CASE (cAcos)
            IF ((Dstack(iStackPtr) < -1.0_kFloat) .OR. &
                (Dstack(iStackPtr) >  1.0_kFloat)) THEN
                EvalErrType = IllegalArgument
                dResult     = SYS_INFINITY_kFloat
            END IF
            Dstack(iStackPtr) = ACOS(Dstack(iStackPtr))

        CASE (cAsin)
            IF ((Dstack(iStackPtr) < -1.0_kFloat) .OR. &
                (Dstack(iStackPtr) >  1.0_kFloat)) THEN
                EvalErrType = IllegalArgument
                dResult     = SYS_INFINITY_kFloat
            END IF
            Dstack(iStackPtr) = ASIN(Dstack(iStackPtr))

        CASE (cAtan)
            Dstack(iStackPtr) = ATAN(Dstack(iStackPtr))

        CASE (cAtan2)
            Dstack(iStackPtr-1) = ATAN2(Dstack(iStackPtr-1), Dstack(iStackPtr))
            iStackPtr = iStackPtr - 1

        CASE (cAcot)
            Dstack(iStackPtr) = ATAN(1.0_kFloat/Dstack(iStackPtr))

        CASE (cAcoth)
            Dstack(iStackPtr) = ATANH(1.0_kFloat/Dstack(iStackPtr))

        CASE (cAcosh)
            IF (Dstack(iStackPtr) < 1.0_kFloat) THEN
                EvalErrType = IllegalArgument
                dResult     = SYS_INFINITY_kFloat
            END IF
            Dstack(iStackPtr) = ACOSH(Dstack(iStackPtr))

        CASE (cAcsc)
            IF (Dstack(iStackPtr) > -Pi/2.0_kFloat .AND.&
                Dstack(iStackPtr) <  Pi/2.0_kFloat) THEN
                EvalErrType = IllegalArgument
                dResult     = SYS_INFINITY_kFloat
            END IF
            Dstack(iStackPtr) = ASIN(1.0_kFloat/Dstack(iStackPtr))

        CASE (cAcsch)
            Dstack(iStackPtr) = ASINH(1.0_kFloat/Dstack(iStackPtr))

        CASE (cAsec)
            IF ((Dstack(iStackPtr) > -1.0_kFloat) .AND. &
                (Dstack(iStackPtr) <  1.0_kFloat)) THEN
                EvalErrType = IllegalArgument
                dResult     = SYS_INFINITY_kFloat
            END IF
            Dstack(iStackPtr) = ACOS(Dstack(iStackPtr))

        CASE (cAsech)
            IF ((Dstack(iStackPtr) < 0.0_kFloat) .OR. &
                (Dstack(iStackPtr) > 1.0_kFloat)) THEN
                EvalErrType = IllegalArgument
                dResult     = SYS_INFINITY_kFloat
            END IF
            Dstack(iStackPtr) = ACOSH(1.0_kFloat/Dstack(iStackPtr))

        CASE (cAsinh)
            IF ( Dstack(iStackPtr) <= 0._kFloat) THEN
                EvalErrType = IllegalArgument
                dResult     = SYS_INFINITY_kFloat
            END IF
            Dstack(iStackPtr) = ASINH(Dstack(iStackPtr))

        CASE (cAtanh)
            Dstack(iStackPtr) = ATANH(Dstack(iStackPtr))

        CASE (cAnint)
            Dstack(iStackPtr) = ANINT(Dstack(iStackPtr))

        CASE (cAint)
            Dstack(iStackPtr) = AINT(Dstack(iStackPtr))

        CASE (cCeil)
            Dstack(iStackPtr) = CEILING(Dstack(iStackPtr))

        CASE (Ccmplx)
            ! Illegal operation
            EvalErrType = IllegalArgument
            dResult     = SYS_INFINITY_kFloat

        CASE (Cconj)
            ! Nothing needs to be done for the real-valued case

        CASE (cCos)
            Dstack(iStackPtr) = COS(Dstack(iStackPtr))

        CASE (cCosh)
            Dstack(iStackPtr) = COSH(Dstack(iStackPtr))

        CASE (cCot)
            Dstack(iStackPtr) = 1.0_kFloat/TAN(Dstack(iStackPtr))

        CASE (cCoth)
            Dstack(iStackPtr) = 1.0_kFloat/TANH(Dstack(iStackPtr))

        CASE (cCsc)
            Dstack(iStackPtr) = 1.0_kFloat/SIN(Dstack(iStackPtr))

        CASE (cCsch)
            Dstack(iStackPtr) = 1.0_kFloat/SINH(Dstack(iStackPtr))

        CASE (cExp)
            Dstack(iStackPtr) = EXP(Dstack(iStackPtr))

        CASE (cFloor)
            Dstack(iStackPtr) = FLOOR(Dstack(iStackPtr))

        CASE (cIf)
            iinstPtr = iinstPtr + 1;   ijumpAddr  = rComp%iByteCode(iinstPtr)
            iinstPtr = iinstPtr + 1;   iimmedAddr = rComp%iByteCode(iinstPtr)
            IF (.NOT.DbleToLogc(Dstack(iStackPtr))) THEN
                iinstPtr = ijumpAddr
                idataPtr = iimmedAddr
            END IF
            iStackPtr = iStackPtr - 1

        CASE (cImag)
            Dstack(iStackPtr) = 0.0_kFloat

        CASE (cLog)
            IF (Dstack(iStackPtr) <= 0._kFloat) THEN
                EvalErrType = IllegalArgument
                dResult     = SYS_INFINITY_kFloat
            END IF
            Dstack(iStackPtr) = LOG(Dstack(iStackPtr))

        CASE (cLog10)
            IF (Dstack(iStackPtr) <= 0._kFloat) THEN
                EvalErrType = IllegalArgument
                dResult     = SYS_INFINITY_kFloat
            END IF
            Dstack(iStackPtr) = LOG10(Dstack(iStackPtr))

        CASE (cMax)
            Dstack(iStackPtr-1) = MAX(Dstack(iStackPtr-1), Dstack(iStackPtr))
            iStackPtr = iStackPtr - 1

        CASE (cMin)
            Dstack(iStackPtr-1) = MIN(Dstack(iStackPtr-1), Dstack(iStackPtr))
            iStackPtr = iStackPtr - 1

        CASE (cReal)
            ! Nothing needs to be done for the real-valued case

        CASE (cRrand)
            CALL RANDOM_SEED(SIZE=iaux)
            CALL MemAlloc(p_Irandom1, [ToIndex(iaux)])
            CALL MemAlloc(p_Irandom2, [ToIndex(iaux)])
            CALL RANDOM_SEED(get=p_Irandom1)

            p_Irandom2(:) = 0
            p_Irandom2(1) = INT(Dstack(iStackPtr-1))
            CALL RANDOM_SEED(put=p_Irandom2)

            daux = 0.0_kFloat
            DO iaux=1,MAX(1,INT(Dstack(iStackPtr)))
                CALL RANDOM_NUMBER(daux)
            END DO
            Dstack(iStackPtr-1) = daux

            CALL RANDOM_SEED(put=p_Irandom1)
            CALL MemFree(p_Irandom1)
            CALL MemFree(p_Irandom2)

            iStackPtr = iStackPtr - 1

        CASE (cSec)
            Dstack(iStackPtr) = 1.0_kFloat/COS(Dstack(iStackPtr))

        CASE (cSech)
            Dstack(iStackPtr) = 1.0_kFloat/COSH(Dstack(iStackPtr))

        CASE (cSign)
            Dstack(iStackPtr) = SIGN(1.0_kFloat,Dstack(iStackPtr))

        CASE (cSin)
            Dstack(iStackPtr) = SIN(Dstack(iStackPtr))

        CASE (cSinh)
            Dstack(iStackPtr) = SINH(Dstack(iStackPtr))

        CASE (cSqrt)
            IF (Dstack(iStackPtr) < 0.0_kFloat) THEN
                EvalErrType = IllegalArgument
                dResult     = SYS_INFINITY_kFloat
            END IF
            Dstack(iStackPtr) = SQRT(Dstack(iStackPtr))

        CASE (cTan)
            Dstack(iStackPtr) = TAN(Dstack(iStackPtr))

        CASE (cTanh)
            Dstack(iStackPtr) = TANH(Dstack(iStackPtr))

        !------------------------------------------------------------
        ! Misc
        !------------------------------------------------------------
        CASE (cImmed)
            iStackPtr         = iStackPtr+1
            Dstack(iStackPtr) = rComp%dImmed(idataPtr)
            idataPtr          = idataPtr + 1

        CASE (cJump)
            idataPtr = rComp%iByteCode(iinstPtr+2)
            iinstPtr = rComp%iByteCode(iinstPtr+1)

        !------------------------------------------------------------
        ! Operators
        !------------------------------------------------------------
        CASE (cNeg)
            Dstack(iStackPtr) = -Dstack(iStackPtr)

        CASE (cAdd)
            Dstack(iStackPtr-1) = Dstack(iStackPtr-1)+Dstack(iStackPtr)
            iStackPtr = iStackPtr - 1

        CASE (cSub)
            Dstack(iStackPtr-1) = Dstack(iStackPtr-1)-Dstack(iStackPtr)
            iStackPtr = iStackPtr - 1

        CASE (cMul)
            Dstack(iStackPtr-1) = Dstack(iStackPtr-1)*Dstack(iStackPtr)
            iStackPtr = iStackPtr - 1

        CASE (cDiv)
            IF (Dstack(iStackPtr) == 0.0_kFloat) THEN
                EvalErrType = DivisionByZero
                dResult     = SYS_INFINITY_kFloat
            END IF
            Dstack(iStackPtr-1) = Dstack(iStackPtr-1)/Dstack(iStackPtr)
            iStackPtr = iStackPtr - 1

        CASE (cMod)
            IF (Dstack(iStackPtr) == 0.0_kFloat) THEN
                EvalErrType = DivisionByZero
                dResult     = SYS_INFINITY_kFloat
            END IF
            Dstack(iStackPtr-1) = MOD(Dstack(iStackPtr-1), Dstack(iStackPtr))
            iStackPtr = iStackPtr - 1

        CASE (cPow)
            Dstack(iStackPtr-1) = Dstack(iStackPtr-1)**Dstack(iStackPtr)
            iStackPtr = iStackPtr - 1

        CASE (cEqual)
            Dstack(iStackPtr-1) = LogcToDble(Dstack(iStackPtr-1) == Dstack(iStackPtr))
            iStackPtr = iStackPtr - 1

        CASE (cNEqual)
            Dstack(iStackPtr-1) = LogcToDble(Dstack(iStackPtr-1) /= Dstack(iStackPtr))
            iStackPtr = iStackPtr - 1

        CASE (cLess)
            Dstack(iStackPtr-1) = LogcToDble(Dstack(iStackPtr-1) < Dstack(iStackPtr))
            iStackPtr = iStackPtr - 1

        CASE (cLessOrEq)
            Dstack(iStackPtr-1) = LogcToDble(Dstack(iStackPtr-1) <= Dstack(iStackPtr))
            iStackPtr = iStackPtr - 1

        CASE (cGreater)
            Dstack(iStackPtr-1) = LogcToDble(Dstack(iStackPtr-1) > Dstack(iStackPtr))
            iStackPtr = iStackPtr - 1

        CASE (cGreaterOrEq)
            Dstack(iStackPtr-1) = LogcToDble(Dstack(iStackPtr-1) >= Dstack(iStackPtr))
            iStackPtr = iStackPtr - 1

        CASE (cAnd)
            Dstack(iStackPtr-1) = LogcToDble(DbleToLogc(Dstack(iStackPtr-1)) .AND. &
                                             DbleToLogc(Dstack(iStackPtr)) )
            iStackPtr = iStackPtr - 1

        CASE (cOr)
            Dstack(iStackPtr-1) = LogcToDble(DbleToLogc(Dstack(iStackPtr-1)) .OR. &
                                             DbleToLogc(Dstack(iStackPtr)) )
            iStackPtr = iStackPtr - 1

        CASE (cNot)
            Dstack(iStackPtr) = LogcToDble( .NOT. DbleToLogc(Dstack(iStackPtr)) )

        !------------------------------------------------------------
        ! Degrees-radians conversion
        !------------------------------------------------------------
        CASE (cDeg)
            Dstack(iStackPtr) = RadToDeg(Dstack(iStackPtr))

        CASE (cRad)
            Dstack(iStackPtr) = DegToRad(Dstack(iStackPtr))

        CASE DEFAULT
            iStackPtr = iStackPtr + 1
            Dstack(iStackPtr) = dValue(rComp%iByteCode(iinstPtr)-VarBegin+1)
         END SELECT
        IF (EvalErrType /= No_Error) RETURN
    END DO

    EvalErrType = No_Error
    dResult = Dstack(iStackPtr)

END SUBROUTINE EvalFunctionScDble

!**************************************************************************************

SUBROUTINE EvalFunctionScCmpl(rComp, Zstack, zValue, EvalErrType, zResult)

!** PURPOSE OF THIS SUBROUTINE:
    ! Evaluate bytecode for the values passed in array Val(:).
    ! Assume that intermediate values and/or the final result
    ! zResult is complex-valued.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! Component of function parser
    TYPE(ParserComponent), INTENT(IN)       :: rComp
    ! Variable values
    tCmplx,                INTENT(IN)       :: zValue(:)
    ! Stack memory
    tCmplx,                INTENT(INOUT)    :: Zstack(:)
    ! Error code for function evaluation
    tSInt32,               INTENT(OUT)      :: EvalErrType
    ! Evaluated function
    tCmplx,                INTENT(OUT)      :: zResult

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: iinstPtr,iStackPtr,idataPtr
    tSInt32     :: ijumpAddr,iimmedAddr

!** FLOW

    ! Initialization
    idataPtr  = 1
    iStackPtr = 0
    iinstPtr  = 0

    ! Initialize error type
    EvalErrType = No_Error

    ! Repeat until complete bytecode has been processed
    DO WHILE(iinstPtr < rComp%iByteCodeSize)
        iinstPtr = iinstPtr + 1

        ! What KIND of bytecode are we?
        SELECT CASE (rComp%iByteCode(iinstPtr))
        !------------------------------------------------------------
        ! Functions
        !------------------------------------------------------------
        CASE (cAbs)
            Zstack(iStackPtr) = ABS(Zstack(iStackPtr))

        CASE (cAcos)
            Zstack(iStackPtr) = ACOS(Zstack(iStackPtr))

        CASE (cAsin)
            Zstack(iStackPtr) = ASIN(Zstack(iStackPtr))

        CASE (cAtan)
            Zstack(iStackPtr) = ATAN(Zstack(iStackPtr))

        CASE (cAtan2)
            IF (IsComplex(Zstack(iStackPtr-1)) .OR.&
                IsComplex(Zstack(iStackPtr))) THEN
                ! ATAN2 is not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr-1) = CMPLX(ATAN2(REAL(Zstack(iStackPtr-1)),&
                                              REAL(zstack(iStackPtr))),0.0_kFloat,KIND=kFloat)
            iStackPtr = iStackPtr - 1

        CASE (cAcot)
            Zstack(iStackPtr) = ATAN(1.0_kFloat/Zstack(iStackPtr))

        CASE (cAcoth)
            Zstack(iStackPtr) = ATANH(1.0_kFloat/Zstack(iStackPtr))

        CASE (cAcosh)
            Zstack(iStackPtr) = ACOSH(Zstack(iStackPtr))

        CASE (cAcsc)
            Zstack(iStackPtr) = ASIN(1.0_kFloat/Zstack(iStackPtr))

        CASE (cAcsch)
            Zstack(iStackPtr) = ASINH(1.0_kFloat/Zstack(iStackPtr))

        CASE (cAsec)
            Zstack(iStackPtr) = ACOS(Zstack(iStackPtr))

        CASE (cAsech)
            Zstack(iStackPtr) = ACOSH(1.0_kFloat/Zstack(iStackPtr))

        CASE (cAsinh)
            Zstack(iStackPtr) = ASINH(Zstack(iStackPtr))

        CASE (cAtanh)
            Zstack(iStackPtr) = ATANH(Zstack(iStackPtr))

        CASE (cAnint)
            IF (IsComplex(Zstack(iStackPtr))) THEN
              ! ANINT are not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr) = CMPLX(ANINT(AIMAG(zstack(iStackPtr))),0.0_kFloat,KIND=kFloat)

        CASE (cAint)
            IF (IsComplex(Zstack(iStackPtr))) THEN
              ! AINT are not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr) = CMPLX(AINT(AIMAG(zstack(iStackPtr))),0.0_kFloat,KIND=kFloat)

        CASE (cCeil)
            Zstack(iStackPtr) = CMPLX(CEILING(REAL(Zstack(iStackPtr))),&
                                      CEILING(AIMAG(Zstack(iStackPtr))),KIND=kFloat)

        CASE (Ccmplx)
            IF (IsComplex(Zstack(iStackPtr)) .OR.&
                IsComplex(Zstack(iStackPtr-1))) THEN
                ! CMPLX cannot be applied to a complex value
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr-1) = CMPLX(REAL(Zstack(iStackPtr-1)),&
                                        REAL(zstack(iStackPtr)),KIND=kFloat)
            iStackPtr = iStackPtr - 1

        CASE (Cconj)
            Zstack(iStackPtr) = CONJG(Zstack(iStackPtr))

        CASE (cCos)
            Zstack(iStackPtr) = COS(Zstack(iStackPtr))

        CASE (cCosh)
            Zstack(iStackPtr) = COSH(Zstack(iStackPtr))

        CASE (cCot)
            Zstack(iStackPtr) = 1.0_kFloat/TAN(Zstack(iStackPtr))

        CASE (cCoth)
            Zstack(iStackPtr) = 1.0_kFloat/TANH(Zstack(iStackPtr))

        CASE (cCsc)
            Zstack(iStackPtr) = 1.0_kFloat/SIN(Zstack(iStackPtr))

        CASE (cCsch)
            Zstack(iStackPtr) = 1.0_kFloat/SINH(Zstack(iStackPtr))

        CASE (cExp)
            Zstack(iStackPtr) = EXP(Zstack(iStackPtr))

        CASE (cFloor)
            Zstack(iStackPtr) = CMPLX(FLOOR(REAL(Zstack(iStackPtr))),&
                                      FLOOR(AIMAG(Zstack(iStackPtr))),KIND=kFloat)

        CASE (cIf)
            IF (IsComplex(Zstack(iStackPtr))) THEN
                ! IF is not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF

            iinstPtr = iinstPtr + 1;   ijumpAddr  = rComp%iByteCode(iinstPtr)
            iinstPtr = iinstPtr + 1;   iimmedAddr = rComp%iByteCode(iinstPtr)
            IF (.NOT.DbleToLogc(REAL(Zstack(iStackPtr)))) THEN
                iinstPtr = ijumpAddr
                idataPtr = iimmedAddr
            END IF
            iStackPtr = iStackPtr - 1

        CASE (cImag)
            Zstack(iStackPtr) = CMPLX(AIMAG(Zstack(iStackPtr)),0.0_kFloat,KIND=kFloat)

        CASE (cLog)
            Zstack(iStackPtr) = LOG(Zstack(iStackPtr))

        CASE (cLog10)
            Zstack(iStackPtr) = LOG(Zstack(iStackPtr))/LOG(10.0_kFloat)

        CASE (cMax)
            IF (ABS(Zstack(iStackPtr)) > ABS(Zstack(iStackPtr-1))) THEN
                Zstack(iStackPtr-1) = Zstack(iStackPtr)
            END IF
            iStackPtr = iStackPtr - 1

        CASE (cMin)
            IF (ABS(Zstack(iStackPtr)) < ABS(Zstack(iStackPtr-1))) THEN
                Zstack(iStackPtr-1) = Zstack(iStackPtr)
            END IF
            iStackPtr = iStackPtr - 1

        CASE (cReal)
            Zstack(iStackPtr) = CMPLX(REAL(Zstack(iStackPtr)),0.0_kFloat,KIND=kFloat)

        CASE (cRrand)
            ! RRAND is not supported in complex-valued case
            EvalErrType = ComplexValuedArgument
            zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)

        CASE (cSec)
            Zstack(iStackPtr) = 1.0_kFloat/COS(Zstack(iStackPtr))

        CASE (cSech)
            Zstack(iStackPtr) = 1.0_kFloat/COSH(Zstack(iStackPtr))

        CASE (cSign)
            Zstack(iStackPtr) = Zstack(iStackPtr)/ABS(Zstack(iStackPtr))

        CASE (cSin)
            Zstack(iStackPtr) = SIN(Zstack(iStackPtr))

        CASE (cSinh)
            Zstack(iStackPtr) = SINH(Zstack(iStackPtr))

        CASE (cSqrt)
            Zstack(iStackPtr) = SQRT(Zstack(iStackPtr))

        CASE (cTan)
            Zstack(iStackPtr) = TAN(Zstack(iStackPtr))

        CASE (cTanh)
            Zstack(iStackPtr) = TANH(Zstack(iStackPtr))

        !------------------------------------------------------------
        ! Misc
        !------------------------------------------------------------
        CASE (cImmed)
            iStackPtr         = iStackPtr + 1
            Zstack(iStackPtr) = rComp%zImmed(idataPtr)
            idataPtr          = idataPtr + 1

        CASE (cJump)
            idataPtr = rComp%iByteCode(iinstPtr+2)
            iinstPtr = rComp%iByteCode(iinstPtr+1)

        !------------------------------------------------------------
        ! Operators
        !------------------------------------------------------------
        CASE (cNeg)
            Zstack(iStackPtr) = -Zstack(iStackPtr)

        CASE (cAdd)
            Zstack(iStackPtr-1) = Zstack(iStackPtr-1)+Zstack(iStackPtr)
            iStackPtr = iStackPtr - 1

        CASE (cSub)
            Zstack(iStackPtr-1) = Zstack(iStackPtr-1)-Zstack(iStackPtr)
            iStackPtr = iStackPtr - 1

        CASE (cMul)
            Zstack(iStackPtr-1) = Zstack(iStackPtr-1)*Zstack(iStackPtr)
            iStackPtr = iStackPtr - 1

        CASE (cDiv)
            IF (Zstack(iStackPtr) == CMPLX(0.0_kFloat,0.0_kFloat,KIND=kFloat)) THEN
                EvalErrType = DivisionByZero
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr-1) = Zstack(iStackPtr-1)/Zstack(iStackPtr)
            iStackPtr = iStackPtr - 1

        CASE (cMod)
            IF (IsComplex(Zstack(iStackPtr)) .OR.&
                IsComplex(Zstack(iStackPtr-1))) THEN
                ! MOD is not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF

            IF (AIMAG(Zstack(iStackPtr)) == 0.0_kFloat) THEN
                EvalErrType = DivisionByZero
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr-1) = CMPLX(MOD(AIMAG(Zstack(iStackPtr-1)),&
                                            AIMAG(Zstack(iStackPtr))),0.0_kFloat,KIND=kFloat)
            iStackPtr = iStackPtr - 1

        CASE (cPow)
            Zstack(iStackPtr-1) = Zstack(iStackPtr-1)**Zstack(iStackPtr)
            iStackPtr = iStackPtr - 1

        CASE (cEqual)
            Zstack(iStackPtr-1) = CMPLX(LogcToDble((REAL(Zstack(iStackPtr-1)) ==&
                                                    REAL(Zstack(iStackPtr))) .AND.&
                                                  (AIMAG(Zstack(iStackPtr-1)) ==&
                                                   AIMAG(Zstack(iStackPtr)))),0.0_kFloat,KIND=kFloat)
            iStackPtr = iStackPtr - 1

        CASE (cNEqual)
            Zstack(iStackPtr-1) = CMPLX(LogcToDble((REAL(Zstack(iStackPtr-1)) /=&
                                                    REAL(Zstack(iStackPtr)))  .OR.&
                                                  (AIMAG(Zstack(iStackPtr-1)) /=&
                                                   AIMAG(Zstack(iStackPtr)))),0.0_kFloat,KIND=kFloat)
            iStackPtr = iStackPtr - 1

        CASE (cLess)
            IF (IsComplex(Zstack(iStackPtr))) THEN
                ! LESS is not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr-1) = CMPLX(LogcToDble(REAL(Zstack(iStackPtr-1)) <&
                                                   REAL(Zstack(iStackPtr))),0.0_kFloat,KIND=kFloat)
            iStackPtr = iStackPtr - 1

        CASE (cLessOrEq)
            IF (IsComplex(Zstack(iStackPtr))) THEN
                ! LESSOREQUAL is not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr-1) = CMPLX(LogcToDble(REAL(Zstack(iStackPtr-1)) <=&
                                                   REAL(Zstack(iStackPtr))),0.0_kFloat,KIND=kFloat)
            iStackPtr = iStackPtr - 1

        CASE (cGreater)
            IF (IsComplex(Zstack(iStackPtr))) THEN
                ! GREATER is not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr-1) = CMPLX(LogcToDble(REAL(Zstack(iStackPtr-1)) >&
                                                   REAL(Zstack(iStackPtr))),0.0_kFloat,KIND=kFloat)
            iStackPtr = iStackPtr - 1

        CASE (cGreaterOrEq)
            IF (IsComplex(Zstack(iStackPtr))) THEN
                ! GREATEROREQUAL is not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr-1) = CMPLX(LogcToDble(REAL(Zstack(iStackPtr-1)) >=&
                                                   REAL(Zstack(iStackPtr))),0.0_kFloat,KIND=kFloat)
            iStackPtr = iStackPtr - 1

        CASE (cAnd)
            IF (IsComplex(Zstack(iStackPtr)) .OR.&
                IsComplex(Zstack(iStackPtr-1))) THEN
                ! AND is not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr-1) = CMPLX(LogcToDble(CmplToLogc(Zstack(iStackPtr-1)) .AND. &
                                                   CmplToLogc(Zstack(iStackPtr)) ),0.0_kFloat,KIND=kFloat)
            iStackPtr = iStackPtr - 1

        CASE (cOr)
            IF (IsComplex(Zstack(iStackPtr)) .OR.&
                IsComplex(Zstack(iStackPtr-1))) THEN
                ! OR is not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr-1) = CMPLX(LogcToDble(CmplToLogc(Zstack(iStackPtr-1)) .OR. &
                                                   CmplToLogc(Zstack(iStackPtr)) ),0.0_kFloat,KIND=kFloat)
            iStackPtr = iStackPtr - 1

        CASE (cNot)
            IF (IsComplex(Zstack(iStackPtr))) THEN
                ! NOT is not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr) = CMPLX(LogcToDble( .NOT. CmplToLogc(Zstack(iStackPtr)) ),0.0_kFloat,KIND=kFloat)

        !------------------------------------------------------------
        ! Degrees-radians conversion
        !------------------------------------------------------------
        CASE (cDeg)
            IF (IsComplex(Zstack(iStackPtr))) THEN
                ! DEG is not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr) = CMPLX(RadToDeg(REAL(Zstack(iStackPtr))),0.0_kFloat,KIND=kFloat)

        CASE (cRad)
            IF (IsComplex(Zstack(iStackPtr))) THEN
                ! RAD is not supported in complex-valued case
                EvalErrType = ComplexValuedArgument
                zResult     = CMPLX(SYS_INFINITY_kFloat,SYS_INFINITY_kFloat,KIND=kFloat)
            END IF
            Zstack(iStackPtr) = CMPLX(DegToRad(REAL(Zstack(iStackPtr))),0.0_kFloat,KIND=kFloat)

        CASE DEFAULT
            iStackPtr = iStackPtr + 1
            Zstack(iStackPtr) = zValue(rComp%iByteCode(iinstPtr)-VarBegin+1)
        END SELECT
        IF (EvalErrType /= No_Error) RETURN
    END DO

    EvalErrType = No_Error
    zResult = Zstack(iStackPtr)

END SUBROUTINE EvalFunctionScCmpl

!**************************************************************************************

SUBROUTINE ToLower_Replace(str)

!** PURPOSE OF THIS SUBROUTINE:
    ! Convert a string to lower case.
    ! The given string is replaced by its lowercase version.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! The string that is to make lowercase
    tCharStar, INTENT(INOUT)    :: str

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,  PARAMETER :: up2low = IACHAR("a") - IACHAR("A")

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: i
    tChar       :: c

!** FLOW

    DO i=1,LEN(str)
        c = str(i:i)
        IF ((c >= "A") .AND. (c <= "Z")) THEN
            str(i:i) = ACHAR(IACHAR(c) + up2low)
        END IF
    END DO

END SUBROUTINE ToLower_Replace

!************************************************************************

SUBROUTINE ToLower_Copy(str,strLower)

!** PURPOSE OF THIS SUBROUTINE:
    ! Convert a string to lower case.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! The string that is to make lowercase
    tCharStar, INTENT(IN)   :: str
    ! Lowercase version of the given string
    tCharStar, INTENT(OUT)  :: strLower

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,  PARAMETER :: up2low = IACHAR("a") - IACHAR("A")
    tCharParam          :: SubName = 'ToLower_Copy'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: i
    tChar       :: c

!** FLOW

    IF (LEN(str) .gt. LEN(strLower)) THEN
        ErrMsg = 'Target string is too short'
        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
        RETURN
    END IF

    ! Initialize string
    strLower = ""

    DO i=1,LEN(str)
        c = str(i:i)
        IF ((c >= "A") .AND. (c <= "Z")) THEN
            strLower(i:i) = ACHAR(IACHAR(c) + up2low)
        ELSE
            strLower(i:i) = c
        END IF
    END DO

END SUBROUTINE ToLower_Copy

!******************************************************************************

SUBROUTINE FreeParseCompPtr1D(Array)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory of array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ParserComponent), POINTER, INTENT(INOUT)   :: Array(:) ! array

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'FreeParseCompPtr1D'

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:

    IF (ASSOCIATED(Array)) THEN
        DEALLOCATE(Array, STAT=AllocStat, ERRMSG=AllocMsg)
        CALL Handle_ErrDealloc(SubName, ModName, AllocMsg, AllocStat)
        NULLIFY(Array)
    END IF

    RETURN

END SUBROUTINE FreeParseCompPtr1D

!**************************************************************************************

SUBROUTINE AllocateParseCompPtr1D(Array,Size)

!** PURPOSE OF THIS SUBROUTINE:
    ! To allocate memory of array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ParserComponent), POINTER, INTENT(INOUT)   :: Array(:) ! array
    tSInt32,                        INTENT(IN)      :: Size     ! size of array

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: SubName = 'AllocateParseCompPtr1D'

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

!** FLOW:
    
    ! check input validity
    IF (Size<=0_kIndex) THEN
        ErrMsg = '"Size" must be positive integer.'
        CALL Handle_ErrLevel(SubName, ModName, ErrSevere, ErrMsg)
        RETURN
    END IF

    ! deallocate if already allocated
    CALL MemFree(Array)
    
    ! allocate memory
    ALLOCATE(Array(Size), STAT=AllocStat, ERRMSG=AllocMsg)
    CALL Handle_ErrAlloc(SubName, ModName, AllocMsg, AllocStat)

    RETURN

END SUBROUTINE AllocateParseCompPtr1D

!**************************************************************************************

END MODULE MClass_MathExpParser
