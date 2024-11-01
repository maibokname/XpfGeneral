#define HAVE_DISPMODULE

MODULE MBase_LegacyUtil

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains derived types used for legacy code.  It also contains
!   utility routines based on routines from SLATEC library.   These routines
!   have the same interface as their original ones but their implementation have
!   all been re-engineered.  These routines provide a simple refactoring of
!   legacy code.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_MemHandlers
    USE MBase_IOHandlers,   ONLY: GetNewIOUnit, IsFileOpen
    USE MBase_MiscUtil,     ONLY: Machine_Real_Parameter, Machine_Integer_Parameter
#ifdef  HAVE_DISPMODULE
    USE MLib_Display,       ONLY: DISP
#endif

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived types for used with legacy code
    PUBLIC :: Equation
    PUBLIC :: UserParam
    PUBLIC :: WorkSpace
    PUBLIC :: SharePar
    PUBLIC :: SaveVar
    ! legacy routines
#ifdef  HAVE_DISPMODULE
    PUBLIC :: XERRWD
    PUBLIC :: XERMSG
    PUBLIC :: I1MACH
    PUBLIC :: D1MACH
#endif
    PUBLIC :: DUMACH
    ! memory handling routines
    PUBLIC  :: MemFree
    PUBLIC  :: MemAlloc

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName        = 'MBase_SLATEC'      ! module name
    tCharStar, PARAMETER    :: SLATECFileName = 'SLATEC Report.Err' ! name of the error File

!** DERIVED TYPE DEFINITIONS
    !> an equation type that can be used to parse strings/texts expressing
    !  mathematical expressions to a 'Function Parser' so that a system of
    !  equations can be evaluated at runtime.
    TYPE Equation
        !> number of equations
        tIndex                      :: NEQ
        !> number of variables
        tIndex                      :: NVR
        !> texts expressing equations
        tCharLen(:), ALLOCATABLE    :: EQText(:)
        !> texts expressing variable names
        tCharLen(:), ALLOCATABLE    :: VarText(:)
        !> values of variables
        tFloat,      ALLOCATABLE    :: Values(:)
    END TYPE Equation
    !> a user parameter type used to modernize legacy code
    TYPE UserParam
        !> number of real parameters
        tIndex                  :: NR
        !> number of integer parameters
        tIndex                  :: NI
        !> real parameters
        tFloat,  ALLOCATABLE    :: RPar(:)
        !> integer parameters
        tSInt32, ALLOCATABLE    :: IPar(:)
    END TYPE UserParam
    !> a workspace type used to modernize legacy code
    TYPE WorkSpace
        !> number of real workspace variables
        tIndex                  :: LRW
        !> number of integer workspace variables
        tIndex                  :: LIW
        !> real workspace variables
        tFloat,  ALLOCATABLE    :: RVar(:)
        !> integer workspace variables
        tSInt32, ALLOCATABLE    :: IVar(:)
    END TYPE WorkSpace
    !> a share parameter type used to modernize common block of legacy code
    TYPE SharePar(NR,NI)
        !> number of real parameters
        tIndex, LEN     :: NR
        !> number integer parameters
        tIndex, LEN     :: NI
        !> real parameters
        tFloat          :: RPar(NR)
        !> integer parameters
        tIndex          :: IPar(NI)
    END TYPE SharePar
    !> a saved variable type used to modernize legacy code
    TYPE, EXTENDS(SharePar) :: SaveVar(NL)
        !> number of logical parameters
        tIndex, LEN     :: NL
        !> number of logical parameters
        tLogical        :: LPar(NL)
    END TYPE SaveVar

!** INTERFACE DEFINITIONS:
    ! MemFree and MemAlloc for Legacy Derived Types
    INTERFACE MemFree
        !^ **Subroutine Interface**: MemFree <br>
        !  **Purpose**:  To free memory of allocatable components of legacy derived types. <br>
        !  **Usage**: <br>
        !   --->    CALL MemFree(UPar) <br>
        !   --->    CALL MemFree(EqText) <br>
        !   --->    CALL MemFree(WkSpace)
        MODULE PROCEDURE DestroyUserParam
        MODULE PROCEDURE DestroyEquation
        MODULE PROCEDURE DestroyWorkSpace
    END INTERFACE
    INTERFACE MemAlloc
        !^ **Subroutine Interface**: MemAlloc <br>
        !  **Purpose**:  To allocate memory for allocatable components of legacy derived types. <br>
        !  **Usage**: <br>
        !   --->    CALL MemAlloc(UPar, 5, 2) <br>
        !   --->    CALL MemAlloc(EqText, 4, 5) <br>
        !   --->    CALL MemAlloc(WkSpace, 10, 10)
        MODULE PROCEDURE CreateUserParam
        MODULE PROCEDURE CreateEquation
        MODULE PROCEDURE CreateWorkSpace
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    tLogical        :: ErrFileOpended = FalseVal
    tSInt32         :: ErrFileUnit    = 7

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

#ifdef  HAVE_DISPMODULE

SUBROUTINE XERRWD(MSG, NMES, NERR, LEVEL, NI, I1, I2, NR, R1, R2)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process and display error information.

!** LEGACY DESCRIPTION:
    !***BEGIN PROLOGUE XERRWD
    !***SUBSIDIARY
    !***PURPOSE Write error message with values.
    !***CATEGORY R3C
    !***TYPE DOUBLE PRECISION (XERRWV-S, XERRWD-D)
    !***AUTHOR Hindmarsh, Alan C., (LLNL)
    !***DESCRIPTION
    !
    ! Subroutines XERRWD, XSETF, XSETUN, and the function routine IXSAV,
    ! as given here, constitute a simplified version of the SLATEC error
    ! handling package.
    !
    ! All arguments are input arguments.
    !
    ! MSG = The message (character array).
    ! NMES = The length of MSG (number of characters).
    ! NERR = The error number (not used).
    ! LEVEL = The error level..
    ! 0 or 1 means recoverable (control returns to caller).
    ! 2 means fatal (run is aborted--see note below).
    ! NI = Number of integers (0, 1, or 2) to be printed with message.
    ! I1,I2 = Integers to be printed, depending on NI.
    ! NR = Number of reals (0, 1, or 2) to be printed with message.
    ! R1,R2 = Reals to be printed, depending on NR.
    !
    ! Note.. this routine is machine-dependent and specialized for use
    ! in limited context, in the following ways..
    ! 1. The argument MSG is assumed to be of type CHARACTER, and
    ! the message is printed with a format of (1X,A).
    ! 2. The message is assumed to take only one line.
    ! Multi-line messages are generated by repeated calls.
    ! 3. If LEVEL = 2, control passes to the statement STOP
    ! to abort the run. This statement may be machine-dependent.
    ! 4. R1 and R2 are assumed to be in double precision and are printed
    ! in D21.13 format.
    !
    !***ROUTINES CALLED IXSAV
    !***REVISION HISTORY (YYMMDD)
    ! 920831 DATE WRITTEN
    ! 921118 Replaced MFLGSV/LUNSAV by IXSAV. (ACH)
    ! 930329 Modified prologue to SLATEC format. (FNF)
    ! 930407 Changed MSG from CHARACTER*1 array to variable. (FNF)
    ! 930922 Minor cosmetic change. (FNF)
    !***END PROLOGUE XERRWD
    !
    !*Internal Notes:
    !
    ! For a different default logical unit number, IXSAV (or a subsidiary
    ! routine that it calls) will need to be modified.
    ! For a different run-abort command, change the statement following
    ! statement 100 at the end.
    !-----------------------------------------------------------------------
    ! Subroutines called by XERRWD.. None
    ! Function routine called by XERRWD.. IXSAV
    !-----------------------------------------------------------------------
    !**End
    
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,        INTENT(IN)  :: NMES
    tSInt32,        INTENT(IN)  :: NERR
    tSInt32,        INTENT(IN)  :: LEVEL
    tSInt32,        INTENT(IN)  :: NI
    tSInt32,        INTENT(IN)  :: I1
    tSInt32,        INTENT(IN)  :: I2
    tSInt32,        INTENT(IN)  :: NR
    tCharLen(NMES), INTENT(IN)  :: MSG
    tFloat,         INTENT(IN)  :: R1
    tFloat,         INTENT(IN)  :: R2

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: IOStatus

!** FLOW:

    ! check flag
    IF (.NOT.ErrFileOpended) THEN
        ! get unit number
        ErrFileUnit=GetNewIOUnit()
        ! open file
        OPEN(ErrFileUnit,File=SLATECFileName,POSITION='APPEND',IOStat=IOStatus)
        IF ( IOStatus /= 0 ) THEN
            CALL Handle_ErrOpen('XERRWD', ModName, SLATECFileName, ErrSevere)
        END IF
        ! write heading
        WRITE(ErrFileUnit,'(A)') ''
        WRITE(ErrFileUnit,'(A)') '+++++ SLATEC ERROR REPORT +++++'
        WRITE(ErrFileUnit,'(A)') '-------------------------------'
        ! Write a blank line
        CALL DISP(X='', UNIT=ErrFileUnit)
        ! set flag
        ErrFileOpended = TrueVal
    ELSE
        ! double check to make sure that the error file is really opened
        IF (.NOT.IsFileOpen(SLATECFileName)) THEN
            ! get unit number
            ErrFileUnit=GetNewIOUnit()
            ! open file
            OPEN(ErrFileUnit,File=SLATECFileName,POSITION='APPEND',IOStat=IOStatus)
            IF ( IOStatus /= 0 ) THEN
                CALL Handle_ErrOpen('XERRWD', ModName, SLATECFileName, ErrSevere)
            END IF
            ! write heading
            WRITE(ErrFileUnit,'(A)') ''
            WRITE(ErrFileUnit,'(A)') '+++++ SLATEC ERROR REPORT +++++'
            WRITE(ErrFileUnit,'(A)') '-------------------------------'
            ! Write a blank line
            CALL DISP(X='', UNIT=ErrFileUnit)
            ! set flag
            ErrFileOpended = TrueVal
        END IF
    END IF

    !
    ! Write the message.
    !
    CALL DISP(X='*** ' // TRIM(MSG), UNIT=ErrFileUnit)
    
    SELECT CASE (NI)
    CASE (1)
        CALL DISP(TITLE='* IN ABOVE MESSAGE, I1 = ', X=I1, FMT='I10', &
                  UNIT=ErrFileUnit, STYLE='LEFT', ADVANCE='YES')
    CASE (2)
        CALL DISP(TITLE='* IN ABOVE MESSAGE, I1 = ', X=I1, FMT='I10', &
                  UNIT=ErrFileUnit, STYLE='LEFT', ADVANCE='NO')
        CALL DISP(TITLE='                AND I2 = ', X=I2, FMT='I10', &
                  UNIT=ErrFileUnit, STYLE='LEFT', ADVANCE='YES')
    END SELECT
    
    SELECT CASE (NR)
    CASE (1)
        CALL DISP(TITLE='* IN ABOVE MESSAGE, R1 = ', X=R1, FMT='D21.13', &
                  UNIT=ErrFileUnit, STYLE='LEFT', ADVANCE='YES')
    CASE (2)
        CALL DISP(TITLE='* IN ABOVE MESSAGE, R1 = ', X=R1, FMT='D21.13', &
                  UNIT=ErrFileUnit, STYLE='LEFT', ADVANCE='YES')
        CALL DISP(TITLE='                AND R2 = ', X=R2, FMT='D21.13', &
                  UNIT=ErrFileUnit, STYLE='LEFT', ADVANCE='YES')
    END SELECT
    
    IF ((NI.GT.0).OR.(NR.GT.0)) THEN
        
        IF (LEVEL.GT.0) THEN
        
            ! write error level
            CALL DISP(TITLE='* ERROR LEVEL = ', X=LEVEL, STYLE='LEFT', &
                      UNIT=ErrFileUnit, ADVANCE='YES')
        
            ! write error number
            CALL DISP(TITLE='* ERROR NUMBER = ', X=NERR, STYLE='LEFT', &
                      UNIT=ErrFileUnit, ADVANCE='YES')
        END IF

    END IF
    
    RETURN
!----------------------- End of Subroutine XERRWD ----------------------

END SUBROUTINE XERRWD

!******************************************************************************

SUBROUTINE XERMSG(LIBRAR, SUBROU, MESSG, NERR, LEVEL)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process and display error information.

!** LEGACY DESCRIPTION:
    !! XERMSG processes XERROR messages.
    !
    !***LIBRARY SLATEC (XERROR)
    !***CATEGORY R3C
    !***TYPE ALL (XERMSG-A)
    !***KEYWORDS ERROR MESSAGE, XERROR
    !***AUTHOR Fong, Kirby, (NMFECC at LLNL)
    !***DESCRIPTION
    !
    ! XERMSG processes a diagnostic message in a manner determined by the
    ! value of LEVEL and the current value of the library error control
    ! flag, KONTRL. See subroutine XSETF for details.
    !
    ! LIBRAR A character constant (or character variable) with the name
    ! of the library. This will be 'SLATEC' for the SLATEC
    ! Common Math Library. The error handling package is
    ! general enough to be used by many libraries
    ! simultaneously, so it is desirable for the routine that
    ! detects and reports an error to identify the library name
    ! as well as the routine name.
    !
    ! SUBROU A character constant (or character variable) with the name
    ! of the routine that detected the error. Usually it is the
    ! name of the routine that is calling XERMSG. There are
    ! some instances where a user callable library routine calls
    ! lower level subsidiary routines where the error is
    ! detected. In such cases it may be more informative to
    ! supply the name of the routine the user called rather than
    ! the name of the subsidiary routine that detected the
    ! error.
    !
    ! MESSG A character constant (or character variable) with the text
    ! of the error or warning message. In the example below,
    ! the message is a character constant that contains a
    ! generic message.
    !
    ! call XERMSG ('SLATEC', 'MMPY',
    ! *'THE ORDER OF THE MATRIX EXCEEDS THE ROW DIMENSION',
    ! *3, 1)
    !
    ! It is possible (and is sometimes desirable) to generate a
    ! specific message--e.g., one that contains actual numeric
    ! values. Specific numeric values can be converted into
    ! character strings using formatted WRITE statements into
    ! character variables. This is called standard Fortran
    ! internal file I/O and is exemplified in the first three
    ! lines of the following example. You can also catenate
    ! substrings of characters to construct the error message.
    ! Here is an example showing the use of both writing to
    ! an internal file and catenating character strings.
    !
    ! CHARACTER*5 CHARN, CHARL
    ! WRITE (CHARN,10) N
    ! WRITE (CHARL,10) LDA
    ! 10 FORMAT(I5)
    ! call XERMSG ('SLATEC', 'MMPY', 'THE ORDER'//CHARN//
    ! * ' OF THE MATRIX EXCEEDS ITS ROW DIMENSION OF'//
    ! * CHARL, 3, 1)
    !
    ! There are two subtleties worth mentioning. One is that
    ! the // for character catenation is used to construct the
    ! error message so that no single character constant is
    ! continued to the next line. This avoids confusion as to
    ! whether there are trailing blanks at the end of the line.
    ! The second is that by catenating the parts of the message
    ! as an actual argument rather than encoding the entire
    ! message into one large character variable, we avoid
    ! having to know how long the message will be in order to
    ! declare an adequate length for that large character
    ! variable. XERMSG calls XERPRN to print the message using
    ! multiple lines if necessary. If the message is very long,
    ! XERPRN will break it into pieces of 72 characters (as
    ! requested by XERMSG) for printing on multiple lines.
    ! Also, XERMSG asks XERPRN to prefix each line with ' * '
    ! so that the total line length could be 76 characters.
    ! Note also that XERPRN scans the error message backwards
    ! to ignore trailing blanks. Another feature is that
    ! the substring '$$' is treated as a new line sentinel
    ! by XERPRN. If you want to construct a multiline
    ! message without having to count out multiples of 72
    ! characters, just use '$$' as a separator. '$$'
    ! obviously must occur within 72 characters of the
    ! start of each line to have its intended effect since
    ! XERPRN is asked to wrap around at 72 characters in
    ! addition to looking for '$$'.
    !
    ! NERR An integer value that is chosen by the library routine's
    ! author. It must be in the range -99 to 999 (three
    ! printable digits). Each distinct error should have its
    ! own error number. These error numbers should be described
    ! in the machine readable documentation for the routine.
    ! The error numbers need be unique only within each routine,
    ! so it is reasonable for each routine to start enumerating
    ! errors from 1 and proceeding to the next integer.
    !
    ! LEVEL An integer value in the range 0 to 2 that indicates the
    ! level (severity) of the error. Their meanings are
    !
    ! -1 A warning message. This is used if it is not clear
    ! that there really is an error, but the user's attention
    ! may be needed. An attempt is made to only print this
    ! message once.
    !
    ! 0 A warning message. This is used if it is not clear
    ! that there really is an error, but the user's attention
    ! may be needed.
    !
    ! 1 A recoverable error. This is used even if the error is
    ! so serious that the routine cannot return any useful
    ! answer. If the user has told the error package to
    ! return after recoverable errors, then XERMSG will
    ! return to the Library routine which can then return to
    ! the user's routine. The user may also permit the error
    ! package to terminate the program upon encountering a
    ! recoverable error.
    !
    ! 2 A fatal error. XERMSG will not return to its caller
    ! after it receives a fatal error. This level should
    ! hardly ever be used; it is much better to allow the
    ! user a chance to recover. An example of one of the few
    ! cases in which it is permissible to declare a level 2
    ! error is a reverse communication Library routine that
    ! is likely to be called repeatedly until it integrates
    ! across some interval. If there is a serious error in
    ! the input such that another step cannot be taken and
    ! the Library routine is called again without the input
    ! error having been corrected by the caller, the Library
    ! routine will probably be called forever with improper
    ! input. In this case, it is reasonable to declare the
    ! error to be fatal.
    !
    ! Each of the arguments to XERMSG is input; none will be modified by
    ! XERMSG. A routine may make multiple calls to XERMSG with warning
    ! level messages; however, after a call to XERMSG with a recoverable
    ! error, the routine should return to the user. Do not try to call
    ! XERMSG with a second recoverable error after the first recoverable
    ! error because the error package saves the error number. The user
    ! can retrieve this error number by calling another entry point in
    ! the error handling package and then clear the error number when
    ! recovering from the error. Calling XERMSG in succession causes the
    ! old error number to be overwritten by the latest error number.
    ! This is considered harmless for error numbers associated with
    ! warning messages but must not be done for error numbers of serious
    ! errors. After a call to XERMSG with a recoverable error, the user
    ! must be given a chance to call NUMXER or XERCLR to retrieve or
    ! clear the error number.
    !***REFERENCES R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
    ! Error-handling Package, SAND82-0800, Sandia
    ! Laboratories, 1982.
    !***ROUTINES CALLED FDUMP, J4SAVE, XERCNT, XERHLT, XERPRN, XERSVE
    !***REVISION HISTORY (YYMMDD)
    ! 880101 DATE WRITTEN
    ! 880621 REVISED AS DIRECTED AT SLATEC CML MEETING OF FEBRUARY 1988.
    ! THERE ARE TWO BASIC CHANGES.
    ! 1. A NEW ROUTINE, XERPRN, IS USED INSTEAD OF XERPRT TO
    ! PRINT MESSAGES. THIS ROUTINE WILL BREAK LONG MESSAGES
    ! INTO PIECES FOR PRINTING ON MULTIPLE LINES. '$$' IS
    ! ACCEPTED AS A NEW LINE SENTINEL. A PREFIX CAN BE
    ! ADDED TO EACH LINE TO BE PRINTED. XERMSG USES EITHER
    ! ' ***' OR ' * ' AND LONG MESSAGES ARE BROKEN EVERY
    ! 72 CHARACTERS (AT MOST) SO THAT THE MAXIMUM LINE
    ! LENGTH OUTPUT CAN NOW BE AS GREAT AS 76.
    ! 2. THE TEXT OF ALL MESSAGES IS NOW IN UPPER CASE SINCE THE
    ! FORTRAN STANDARD DOCUMENT DOES NOT ADMIT THE EXISTENCE
    ! OF LOWER CASE.
    ! 880708 REVISED AFTER THE SLATEC CML MEETING OF JUNE 29 AND 30.
    ! THE PRINCIPAL CHANGES ARE
    ! 1. CLARIFY COMMENTS IN THE PROLOGUES
    ! 2. RENAME XRPRNT TO XERPRN
    ! 3. REWORK HANDLING OF '$$' IN XERPRN TO HANDLE BLANK LINES
    ! SIMILAR TO THE WAY FORMAT STATEMENTS HANDLE THE /
    ! CHARACTER FOR NEW RECORDS.
    ! 890706 REVISED WITH THE HELP OF FRED FRITSCH AND REG CLEMENS TO
    ! CLEAN UP THE CODING.
    ! 890721 REVISED TO USE NEW FEATURE IN XERPRN TO COUNT CHARACTERS IN
    ! PREFIX.
    ! 891013 REVISED TO CORRECT COMMENTS.
    ! 891214 Prologue converted to Version 4.0 format. (WRB)
    ! 900510 Changed test on NERR to be -9999999 < NERR < 99999999, but
    ! NERR .ne. 0, and on LEVEL to be -2 < LEVEL < 3. Added
    ! LEVEL=-1 logic, changed calls to XERSAV to XERSVE, and
    ! XERCTL to XERCNT. (RWC)
    ! 920501 Reformatted the REFERENCES section. (WRB)
    !***END PROLOGUE XERMSG
    
    !** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,   INTENT(IN)   :: NERR
    tSInt32,   INTENT(IN)   :: LEVEL
    tCharStar, INTENT(IN)   :: LIBRAR
    tCharStar, INTENT(IN)   :: SUBROU
    tCharStar, INTENT(IN)   :: MESSG

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(256)   :: MSG
    tSInt32         :: IOStatus

    !** FLOW:

    ! check flag
    IF (.NOT.ErrFileOpended) THEN
        ! get unit number
        ErrFileUnit=GetNewIOUnit()
        ! open file
        OPEN(ErrFileUnit,File=SLATECFileName,POSITION='APPEND',IOStat=IOStatus)
        IF ( IOStatus /= 0 ) THEN
            CALL Handle_ErrOpen('XERMSG', ModName, SLATECFileName, ErrSevere)
        END IF
        ! write heading
        WRITE(ErrFileUnit,'(A)') ''
        WRITE(ErrFileUnit,'(A)') '+++++ SLATEC ERROR REPORT +++++'
        WRITE(ErrFileUnit,'(A)') '-------------------------------'
        ! Write a blank line
        CALL DISP(X='', UNIT=ErrFileUnit)
        ! set flag
        ErrFileOpended = TrueVal
    ELSE
        ! double check to make sure that the error file is really opened
        IF (.NOT.IsFileOpen(SLATECFileName)) THEN
            ! get unit number
            ErrFileUnit=GetNewIOUnit()
            ! open file
            OPEN(ErrFileUnit,File=SLATECFileName,POSITION='APPEND',IOStat=IOStatus)
            IF ( IOStatus /= 0 ) THEN
                CALL Handle_ErrOpen('XERMSG', ModName, SLATECFileName, ErrSevere)
            END IF
            ! write heading
            WRITE(ErrFileUnit,'(A)') ''
            WRITE(ErrFileUnit,'(A)') '+++++ SLATEC ERROR REPORT +++++'
            WRITE(ErrFileUnit,'(A)') '-------------------------------'
            ! Write a blank line
            CALL DISP(X='', UNIT=ErrFileUnit)
            ! set flag
            ErrFileOpended = TrueVal
        END IF
    END IF

    ! set up the message for subroutine and library
    MSG = '*** MESSAGE FROM ROUTINE ' // TRIM(SUBROU) // &
          ' IN LIBRARY ' // TRIM(LIBRAR)
    !
    ! Write the message.
    !
    CALL DISP(X=TRIM(MSG), UNIT=ErrFileUnit)
    
    ! set up the message for error level
    SELECT CASE (LEVEL)
    CASE (:0)
        MSG = '* ERROR LEVEL = INFORMATIVE/WARNING MESSAGE'
    CASE (1)
        MSG = '* ERROR LEVEL = POTENTIALLY RECOVERABLE ERROR'
    CASE (2)
        MSG = '* ERROR LEVEL = FATAL ERROR'
    CASE DEFAULT
        MSG = '*** INVALID ERROR LEVEL SPECIFIED.  CHECK THE ROUTINE NAMED IN ABOVE MESSAGE. ***'
    END SELECT
    
    !
    ! Write the message.
    !
    CALL DISP(X=TRIM(MSG), UNIT=ErrFileUnit)
    
    !
    ! Write the supplied message.
    !
    MSG = '* ' // MESSG
    CALL DISP(X=TRIM(MSG), UNIT=ErrFileUnit)

    ! write the message for error code
    CALL DISP(TITLE='* ERROR NUMBER = ', X=NERR, STYLE='LEFT', &
              UNIT=ErrFileUnit, ADVANCE='YES')

    !
    ! Write the end of the message.
    !
    CALL DISP(X='*** END OF MESSAGE', UNIT=ErrFileUnit)

    !
    ! Write a blank line
    !
    CALL DISP(X='', UNIT=ErrFileUnit)

    RETURN

END SUBROUTINE XERMSG

!******************************************************************************

FUNCTION I1MACH (I) RESULT ( FUNCVAL )

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return integer machine dependent parameters.

!** LEGACY DESCRIPTION:
    !! I1MACH returns integer machine dependent constants.
    !
    !***LIBRARY SLATEC
    !***CATEGORY R1
    !***TYPE INTEGER (I1MACH-I)
    !***KEYWORDS MACHINE CONSTANTS
    !***AUTHOR Fox, P. A., (Bell Labs)
    ! Hall, A. D., (Bell Labs)
    ! Schryer, N. L., (Bell Labs)
    !***DESCRIPTION
    !
    ! I1MACH can be used to obtain machine-dependent parameters for the
    ! local machine environment. It is a function subprogram with one
    ! (input) argument and can be referenced as follows:
    !
    ! K = I1MACH(I)
    !
    ! where I=1,...,16. The (output) value of K above is determined by
    ! the (input) value of I. The results for various values of I are
    ! discussed below.
    !
    ! I/O unit numbers:
    ! I1MACH( 1) = the standard input unit.
    ! I1MACH( 2) = the standard output unit.
    ! I1MACH( 3) = the standard punch unit.
    ! I1MACH( 4) = the standard error message unit.
    !
    ! Words:
    ! I1MACH( 5) = the number of bits per integer storage unit.
    ! I1MACH( 6) = the number of characters per integer storage unit.
    !
    ! Integers:
    ! assume integers are represented in the S-digit, base-A form
    !
    ! sign ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
    !
    ! where 0 <= X(I) < A for I=0,...,S-1.
    ! I1MACH( 7) = A, the base.
    ! I1MACH( 8) = S, the number of base-A digits.
    ! I1MACH( 9) = A**S - 1, the largest magnitude.
    !
    ! Floating-Point Numbers:
    ! Assume floating-point numbers are represented in the T-digit,
    ! base-B form
    ! sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
    !
    ! where 0 <= X(I) < B for I=1,...,T,
    ! 0 < X(1), and EMIN <= E <= EMAX.
    ! I1MACH(10) = B, the base.
    !
    ! Single-Precision:
    ! I1MACH(11) = T, the number of base-B digits.
    ! I1MACH(12) = EMIN, the smallest exponent E.
    ! I1MACH(13) = EMAX, the largest exponent E.
    !
    ! Double-Precision:
    ! I1MACH(14) = T, the number of base-B digits.
    ! I1MACH(15) = EMIN, the smallest exponent E.
    ! I1MACH(16) = EMAX, the largest exponent E.
    !
    ! To alter this function for a particular environment, the desired
    ! set of DATA statements should be activated by removing the C from
    ! column 1. Also, the values of I1MACH(1) - I1MACH(4) should be
    ! checked for consistency with the local operating system.
    !
    !***REFERENCES P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
    ! a portable library, ACM Transactions on Mathematical
    ! Software 4, 2 (June 1978), pp. 177-188.
    !***ROUTINES CALLED (NONE)
    !***REVISION HISTORY (YYMMDD)
    ! 750101 DATE WRITTEN
    ! 891012 Added VAX G-floating constants. (WRB)
    ! 891012 REVISION DATE from Version 3.2
    ! 891214 Prologue converted to Version 4.0 format. (BAB)
    ! 900618 Added DEC RISC constants. (WRB)
    ! 900723 Added IBM RS 6000 constants. (WRB)
    ! 901009 Correct I1MACH(7) for IBM Mainframes. Should be 2 not 16.
    ! (RWC)
    ! 910710 Added HP 730 constants. (SMR)
    ! 911114 Added Convex IEEE constants. (WRB)
    ! 920121 Added SUN -r8 compiler option constants. (WRB)
    ! 920229 Added Touchstone Delta i860 constants. (WRB)
    ! 920501 Reformatted the REFERENCES section. (WRB)
    ! 920625 Added Convex -p8 and -pd8 compiler option constants.
    ! (BKS, WRB)
    ! 930201 Added DEC Alpha and SGI constants. (RWC and WRB)
    ! 930618 Corrected I1MACH(5) for Convex -p8 and -pd8 compiler
    ! options. (DWL, RWC and WRB).
    !***END PROLOGUE I1MACH
    
    !** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: I
    tSInt32                 :: FUNCVAL

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        !na

    !** FLOW:

    IF ( I < 1 .OR. I > 16 ) THEN
      CALL XERMSG ('SLATEC', 'I1MACH', 'I OUT OF BOUNDS', 1, 2)
    END IF

    FUNCVAL = Machine_Integer_Parameter(I)

    RETURN

END FUNCTION I1MACH

!******************************************************************************

FUNCTION D1MACH (I) RESULT ( FUNCVAL )

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return floating-point machine dependent parameters.

!** LEGACY DESCRIPTION:
    !! D1MACH returns floating point machine dependent constants.
    !
    !***LIBRARY SLATEC
    !***CATEGORY R1
    !***TYPE DOUBLE PRECISION (R1MACH-S, D1MACH-D)
    !***KEYWORDS MACHINE CONSTANTS
    !***AUTHOR Fox, P. A., (Bell Labs)
    ! Hall, A. D., (Bell Labs)
    ! Schryer, N. L., (Bell Labs)
    !***DESCRIPTION
    !
    ! D1MACH can be used to obtain machine-dependent parameters for the
    ! local machine environment. It is a function subprogram with one
    ! (input) argument, and can be referenced as follows:
    !
    ! D = D1MACH(I)
    !
    ! where I=1,...,5. The (output) value of D above is determined by
    ! the (input) value of I. The results for various values of I are
    ! discussed below.
    !
    ! D1MACH( 1) = B**(EMIN-1), the smallest positive magnitude.
    ! D1MACH( 2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
    ! D1MACH( 3) = B**(-T), the smallest relative spacing.
    ! D1MACH( 4) = B**(1-T), the largest relative spacing.
    ! D1MACH( 5) = LOG10(B)
    !
    ! Assume double precision numbers are represented in the T-digit,
    ! base-B form
    !
    ! sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
    !
    ! where 0 <= X(I) < B for I=1,...,T, 0 < X(1), and
    ! EMIN <= E <= EMAX.
    !
    ! The values of B, T, EMIN and EMAX are provided in I1MACH as
    ! follows:
    ! I1MACH(10) = B, the base.
    ! I1MACH(14) = T, the number of base-B digits.
    ! I1MACH(15) = EMIN, the smallest exponent E.
    ! I1MACH(16) = EMAX, the largest exponent E.
    !
    ! To alter this function for a particular environment, the desired
    ! set of DATA statements should be activated by removing the C from
    ! column 1. Also, the values of D1MACH(1) - D1MACH(4) should be
    ! checked for consistency with the local operating system.
    !
    !***REFERENCES P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
    ! a portable library, ACM Transactions on Mathematical
    ! Software 4, 2 (June 1978), pp. 177-188.
    !***ROUTINES CALLED XERMSG
    !***REVISION HISTORY (YYMMDD)
    ! 750101 DATE WRITTEN
    ! 890213 REVISION DATE from Version 3.2
    ! 891214 Prologue converted to Version 4.0 format. (BAB)
    ! 900315 CALLs to XERROR changed to CALLs to XERMSG. (THJ)
    ! 900618 Added DEC RISC constants. (WRB)
    ! 900723 Added IBM RS 6000 constants. (WRB)
    ! 900911 Added SUN 386i constants. (WRB)
    ! 910710 Added HP 730 constants. (SMR)
    ! 911114 Added Convex IEEE constants. (WRB)
    ! 920121 Added SUN -r8 compiler option constants. (WRB)
    ! 920229 Added Touchstone Delta i860 constants. (WRB)
    ! 920501 Reformatted the REFERENCES section. (WRB)
    ! 920625 Added CONVEX -p8 and -pd8 compiler option constants.
    ! (BKS, WRB)
    ! 930201 Added DEC Alpha and SGI constants. (RWC and WRB)
    !***END PROLOGUE D1MACH
    
    !** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: I
    tFloat                  :: FUNCVAL

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        !na

    !** FLOW:

    IF ( I < 1 .OR. I > 5 ) THEN
      CALL XERMSG ('SLATEC', 'D1MACH', 'I OUT OF BOUNDS', 1, 2)
    END IF

    FUNCVAL = Machine_Real_Parameter(I)

    RETURN

END FUNCTION D1MACH

#endif

!******************************************************************************

FUNCTION DUMACH() RESULT ( FUNCVAL )

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the unit roundoff of the machine.

!** LEGACY DESCRIPTION:
    !! DUMACH computes the unit roundoff of the machine.
    !
    ! The unit roundoff is defined as the smallest positive machine
    ! number u such that 1.0 + u .ne. 1.0. This is computed by DUMACH
    ! in a machine-independent manner.
    
    !** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat      :: FUNCVAL

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tFloat      :: U, COMP

    !** FLOW:

    U = One
    DO
        U = U*Half
        COMP = One + U
        IF (COMP == One) EXIT
    END DO
    FUNCVAL = U*Two
    RETURN

END FUNCTION DUMACH

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!       CONSTRUCTOR AND DESTRUCTOR ROUTINES FOR LEGACY DERIVED TYPES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE CreateUserParam(UPar,NR,NI)

!** PURPOSE OF THIS SUBROUTINE:
    !! To create UserParam object and allocate memory for allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UserParam), INTENT(INOUT)  :: UPar
    tIndex,          INTENT(IN)     :: NR
    tIndex,          INTENT(IN)     :: NI
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! check validity for input data
    IF ((NR <= 0).OR.(NI <= 0)) THEN
        CALL Handle_ErrLevel('CreateUserParam', ModName, ErrSevere, &
                             'Both "NR" and "NI" must be positive integer.')
    END IF

    ! create UserParam object
    UPar%NR = NR
    UPar%NI = NI
    CALL MemAlloc(UPar%RPar, [UPar%NR])
    CALL MemAlloc(UPar%IPar, [UPar%NI])

    RETURN

END SUBROUTINE CreateUserParam

!******************************************************************************

SUBROUTINE DestroyUserParam(UPar)

!** PURPOSE OF THIS SUBROUTINE:
    !! To reset components and free memory of allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UserParam), INTENT(INOUT)    :: UPar
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    UPar%NR = 0
    UPar%NI = 0
    CALL MemFree(UPar%IPar)
    CALL MemFree(UPar%RPar)

    RETURN

END SUBROUTINE DestroyUserParam

!******************************************************************************

SUBROUTINE CreateWorkSpace(Work,LRW,LIW)

!** PURPOSE OF THIS SUBROUTINE:
    !! To create WorkSpace object and allocate memory for allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(WorkSpace), INTENT(INOUT)  :: Work
    tIndex,          INTENT(IN)     :: LRW
    tIndex,          INTENT(IN)     :: LIW
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! check validity for input data
    IF ((LRW <= 0).OR.(LIW <= 0)) THEN
        CALL Handle_ErrLevel('CreateWorkSpace', ModName, ErrSevere, &
                             'Both "LRW" and "LIW" must be positive integer.')
    END IF

    ! create WorkSpace object
    Work%LRW = LRW
    Work%LIW = LIW
    CALL MemAlloc(Work%RVar, [Work%LRW])
    CALL MemAlloc(Work%IVar, [Work%LIW])

    RETURN

END SUBROUTINE CreateWorkSpace

!******************************************************************************

SUBROUTINE DestroyWorkSpace(Work)

!** PURPOSE OF THIS SUBROUTINE:
    !! To reset components and free memory of allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(WorkSpace), INTENT(INOUT)    :: Work
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Work%LRW = 0
    Work%LIW = 0
    CALL MemFree(Work%IVar)
    CALL MemFree(Work%RVar)

    RETURN

END SUBROUTINE DestroyWorkSpace

!******************************************************************************

SUBROUTINE CreateEquation(EQ,NEQ,NVR)

!** PURPOSE OF THIS SUBROUTINE:
    !! To create Equation object and allocate memory for allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Equation), INTENT(INOUT)   :: EQ
    tIndex,         INTENT(IN)      :: NEQ
    tIndex,         INTENT(IN)      :: NVR
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! check validity for input data
    IF ((NEQ <= 0).OR.(NVR <= 0)) THEN
        CALL Handle_ErrLevel('CreateEquation', ModName, ErrSevere, &
                             'Both "NEQ" and "NVR" must be positive integer.')
    END IF

    ! create Equation object
    EQ%NEQ = NEQ
    EQ%NVR = NVR
    CALL MemAlloc(EQ%EQText,  256_kIndex, [NEQ])
    CALL MemAlloc(EQ%VarText, 30_kIndex,  [NVR])
    CALL MemAlloc(EQ%Values,  [NVR])

    RETURN

END SUBROUTINE CreateEquation

!******************************************************************************

SUBROUTINE DestroyEquation(EQ)

!** PURPOSE OF THIS SUBROUTINE:
    !! To reset components and free memory of allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Equation), INTENT(INOUT)    :: EQ
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    EQ%NEQ = 0
    EQ%NVR = 0
    CALL MemFree(EQ%EQText)
    CALL MemFree(EQ%VarText)
    CALL MemFree(EQ%Values)

    RETURN

END SUBROUTINE DestroyEquation

!******************************************************************************

END MODULE MBase_LegacyUtil

!******************************************************************************
