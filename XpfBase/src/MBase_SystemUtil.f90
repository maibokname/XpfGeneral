
MODULE MBase_SystemUtil

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains utility routines relating to operating system and compiler currently in used.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
!#if (FORTRAN_COMPILER == COMPILER_IS_INTEL)
#ifdef  __INTEL_COMPILER
    USE IFPORT
#endif

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: GetOSName
    PUBLIC :: GetCompilerName
    PUBLIC :: DoesDirectoryExist
#if ((OPERATING_SYSTEM) && (FORTRAN_COMPILER))
    PUBLIC :: CreateDirectory
    PUBLIC :: ChangeDirectory
    PUBLIC :: GetCurrentDirectory
#endif

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tCharParam      :: ModName = 'MBase_SystemUtil'
#if (OPERATING_SYSTEM == OS_IS_WINDOWS)
    tCharParam      :: PathSep = '\'
#elif (OPERATING_SYSTEM == OS_IS_LINUX)
    tCharParam      :: PathSep = '/'
#endif

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

FUNCTION GetOSName() RESULT(OSName)

!** PURPOSE OF THIS FUNCTION:
    !^ To return the name of the current operating system in use.

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharAlloc  :: OSName   !! WINDOWS, LINUX or UNKNOWN

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

#if (OPERATING_SYSTEM == OS_IS_WINDOWS)
    OSName = 'WINDOWS'
#elif (OPERATING_SYSTEM == OS_IS_LINUX)
    OSName = 'LINUX'
#else
    OSName = 'UNKNOWN'
#endif

    RETURN

END FUNCTION GetOSName

!******************************************************************************

FUNCTION GetCompilerName() RESULT(CompilerName)

!** PURPOSE OF THIS FUNCTION:
    !^ To return the name of the current compiler in use.

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharAlloc  :: CompilerName     !! INTEL, GFORTRAN or UNKNOWN

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

#if (FORTRAN_COMPILER == COMPILER_IS_INTEL)
#ifdef  __INTEL_LLVM_COMPILER
    CompilerName = 'INTEL_IFX'
#else
    CompilerName = 'INTEL_IFORT'
#endif
#elif (FORTRAN_COMPILER == COMPILER_IS_GFORTRAN)
    CompilerName = 'GFORTRAN'
#else
    CompilerName = 'UNKNOWN'
#endif

    RETURN

END FUNCTION GetCompilerName

!******************************************************************************

FUNCTION DoesDirectoryExist(DirName) RESULT(Flag)

!** PURPOSE OF THIS FUNCTION:
    !^ To check directory if the given directory exists.

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: DirName  !! directory name
    tLogical                :: Flag     !! true if the directory exists

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: IStat, IOUnit, C
    tCharAlloc  :: TestFile

!** FLOW

    ! try to create a file in the specified directory name to see if the directory already exists
    C = LEN_TRIM(DirName)
    IF (DirName(C:C) == PathSep) THEN
        TestFile = TRIM(ADJUSTL(DirName)) // 'TestFile.txt'
    ELSE
        TestFile = TRIM(ADJUSTL(DirName)) // PathSep // 'TestFile.txt'
    END IF
    OPEN(NEWUNIT=IOUnit, FILE=TestFile, IOSTAT=IStat)

    Flag = (IStat == 0)

    ! directory exists so remove the file created.
    IF (Flag) CLOSE(UNIT=IOUnit, STATUS='DELETE')

    RETURN

END FUNCTION DoesDirectoryExist

!******************************************************************************

#if ((OPERATING_SYSTEM) && (FORTRAN_COMPILER))

SUBROUTINE CreateDirectory(DirName)

!** PURPOSE OF THIS FUNCTION:
    !^ To create new directory for the specified directory name.

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   ::  DirName     !! directory name

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tCharAlloc  :: Command

!** FLOW

    IF (LEN_TRIM(DirName) == 0) THEN
        CALL Handle_ErrLevel('CreateDirectory', ModName, ErrWarning, &
                             'The specified directory name is empty.')
        RETURN
    END IF

    IF (.NOT.DoesDirectoryExist(DirName)) THEN
        ! directory does not exist
        Command = 'mkdir ' // TRIM(ADJUSTL(DirName))
        CALL EXECUTE_COMMAND_LINE(Command)
    END IF

    RETURN

END SUBROUTINE CreateDirectory

!******************************************************************************

SUBROUTINE ChangeDirectory(DirName)

!** PURPOSE OF THIS FUNCTION:
    !^ To change directory to the specified directory name.

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   ::  DirName     !! directory name

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tCharAlloc  :: Command

!** FLOW

    IF (LEN_TRIM(DirName) == 0) THEN
        CALL Handle_ErrLevel('ChangeDirectory', ModName, ErrWarning, &
                             'The specified directory name is empty.')
        RETURN
    END IF

    IF (DoesDirectoryExist(DirName)) THEN
        ! change to the specified directory
        Command = 'chdir ' // TRIM(ADJUSTL(DirName))
        CALL EXECUTE_COMMAND_LINE(Command)
    ELSE
        CALL Handle_ErrLevel('ChangeDirectory', ModName, ErrWarning, &
                             'The specified directory name does not exist.')
    END IF

    RETURN

END SUBROUTINE ChangeDirectory

!******************************************************************************

FUNCTION GetCurrentDirectory() RESULT(DirName)

!** PURPOSE OF THIS FUNCTION:
    !^ To get the current directory name.  If error occurred,
    !  return unallocated character string.

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharAlloc  ::  DirName     !! directory name

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tCharLen(255)   :: FolderName

!** FLOW

    IF (GETCWD(FolderName) == 0) THEN
        DirName = TRIM(FolderName)
    END IF

    RETURN

END FUNCTION GetCurrentDirectory

!******************************************************************************

#endif

END MODULE MBase_SystemUtil

!******************************************************************************
