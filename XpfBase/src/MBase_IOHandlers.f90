
MODULE MBase_IOHandlers

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various routines for handling basic input/output (IO) operations.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil,   ONLY: ToChar => ToDecStrSigned

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! open and close procedures
    PUBLIC :: OpenInputFile
    PUBLIC :: OpenNewOutputFile
    PUBLIC :: OpenOutputFile
    PUBLIC :: OpenNewFile
    PUBLIC :: OpenFile
    PUBLIC :: CloseFile
    PUBLIC :: CloseOpenFiles
    ! inquiry procedures
    PUBLIC :: GetNewIOUnit
    PUBLIC :: GetFileIOUnit
    PUBLIC :: GetIOUnits_AllOpenFile
    PUBLIC :: IsFileOpen
    PUBLIC :: DoesFileExist
    PUBLIC :: GetFileName
    ! reading and writing procedures
    PUBLIC :: Output_Message
    PUBLIC :: ReadSqfrm_LineCharAlloc
    PUBLIC :: ReadSqfrm_LineCharStar
    PUBLIC :: CountSqfrm_Lines

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    !  Largest allowed Unit number (or a large number, if none)
    tSInt32,   PARAMETER    :: MaxUnitNumber = 1000
    ! name for audit file
    tCharStar, PARAMETER    :: AudFileName = LibName // '.Aud'
    ! Size of character variable for storing error messages.
    tSInt32,   PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE IsFileOpen
        !^ **Function Interface**: IsFileOpen <br>
        !  **Purpose**:  To check whether the specified file is open or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsFileOpen(FileIOUnit) <br>
        !   --->    IF (IsFileOpen(FileName)) DoSomething
        MODULE PROCEDURE IsFileOpen_UnitNumber
        MODULE PROCEDURE IsFileOpen_FileName
    END INTERFACE
    INTERFACE CloseFile
        !^ **Subroutine Interface**: CloseFile <br>
        !  **Purpose**:  To close the specified file. <br>
        !  **Usage**: <br>
        !   --->    CALL CloseFile(FileIOUnit) <br>
        !   --->    CALL CloseFile(FileName))
        MODULE PROCEDURE CloseFile_Unit
        MODULE PROCEDURE CloseFile_Name
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

FUNCTION OpenInputFile(Name) RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^  To open a file associated with the specified name for reading.
    !   A valid (positive) unit number is return if the specified file
    !   is opened successfully.  Otherwise, it is set to -1. 

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: Name     !! file name
    tSInt32                 :: IOUnit   !! unit number

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tSInt32             :: iStat
    tCharLen(MsgLen)    :: cMsg

!** FLOW

    ! first, check the input
    IF (LEN_TRIM(Name)==0) THEN
        CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
        CALL Output_Message('The specified name is an empty string.')
        CALL Output_Message('--------------------------------------')
        IOUnit = -1
        RETURN
    ELSEIF (.NOT.DoesFileExist(Name)) THEN
        CALL Output_Message('---             WARNING ERROR MESSAGE           ---')
        CALL Output_Message('The specified file (' // Name // ') does not exist.')
        CALL Output_Message('---------------------------------------------------')
        IOUnit = -1
        RETURN
    END IF
    
    IF (IsFileOpen(Name)) THEN
        ! the specified file is already open so get the unit number associated with it
        IOUnit = GetFileIOUnit(Name)
    ELSE
        ! open file for reading
        IOUnit = GetNewIOUnit()
        OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='OLD', ACTION='READ', &
             IOSTAT=iStat, IOMSG=cMsg)
    
        ! check I/O status
        IF (iStat /= 0) THEN
            CALL Output_Message('---       WARNING ERROR MESSAGE          ---')
            CALL Output_Message('Cannot open ' // Name // ' as an input file.')
            CALL Output_Message('IOSTAT = ' // ToChar(iStat))
            CALL Output_Message('IOMSG  = ' // cMsg) 
            CALL Output_Message('--------------------------------------------')
            IOUnit = -1
        END IF
    END IF

    RETURN
   
END FUNCTION OpenInputFile

!******************************************************************************

FUNCTION OpenNewOutputFile(Name,DirAcc,RecLen) RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^  To open a new file associated with the specified name for writing.
    !   A valid (positive) unit number is return if the specified file
    !   is opened successfully.  Otherwise, it is set to -1. 

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: Name     !! file name
    tLogical, OPTIONAL, INTENT(IN)  :: DirAcc
    !^  flag indicating access mode <br>
    ! - true if requesting direct access mode <br>
    ! - false if requesting sequential access mode <br>
    ! - default = FalseVal <br>
    ! For direct access mode, the file is opened for an unformatted file. <br>
    ! For sequential access mode, the file is opened for a formatted file.
    tSInt32,  OPTIONAL, INTENT(IN)  :: RecLen
    !^  (fixed) length of records for direct access mode. <br>
    !   If DirAcc is present and true, RecLen must be specified.
    tSInt32                         :: IOUnit   !! unit number
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tLogical            :: Sequential
    tSInt32             :: iStat
    tCharLen(MsgLen)    :: cMsg

!** FLOW

    ! set default, check the input
    Sequential = TrueVal
    IF (PRESENT(DirAcc)) Sequential = .NOT.DirAcc
    IF (LEN_TRIM(Name)==0) THEN
        CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
        CALL Output_Message('The specified name is an empty string.')
        CALL Output_Message('--------------------------------------')
        IOUnit = -1
        RETURN
    ELSEIF (DoesFileExist(Name)) THEN
        CALL Output_Message('---                     WARNING ERROR MESSAGE                   ---')
        CALL Output_Message('Cannot open an existing file (' // Name // ') as a new output file.')
        CALL Output_Message('-------------------------------------------------------------------')
        IOUnit = -1
        RETURN
    ELSEIF (.NOT.Sequential.AND..NOT.PRESENT(RecLen)) THEN
        CALL Output_Message('---               WARNING ERROR MESSAGE              ---')
        CALL Output_Message('DirAcc is present and true, but RecLen is not specified.')
        CALL Output_Message('--------------------------------------------------------')
        IOUnit = -1
        RETURN
    END IF

    ! open file for writing
    IF (Sequential) THEN
        ! open sequential and formatted file
        IOUnit = GetNewIOUnit()
        OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='NEW', ACTION='WRITE', &
             IOSTAT=iStat, IOMSG=cMsg)
    ELSE
        ! open direct access and unformatted file
        IF (PRESENT(RecLen)) THEN
            IOUnit = GetNewIOUnit()
            OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='NEW', ACTION='WRITE', &
                 ACCESS='DIRECT', RECL=RecLen, IOSTAT=iStat, IOMSG=cMsg)
        END IF
    END IF
    
    ! check I/O status
    IF (iStat /= 0) THEN
        CALL Output_Message('---          WARNING ERROR MESSAGE           ---')
        CALL Output_Message('Cannot open ' // Name // ' as a new output file.')
        CALL Output_Message('IOSTAT = ' // ToChar(iStat))
        CALL Output_Message('IOMSG  = ' // cMsg) 
        CALL Output_Message('------------------------------------------------')
        IOUnit = -1
    END IF

    RETURN
   
END FUNCTION OpenNewOutputFile

!******************************************************************************

FUNCTION OpenOutputFile(Name,Append,DirAcc,RecLen) RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^  To open a file associated with the specified name for writing.
    !   A valid (positive) unit number is return if the specified file
    !   is opened successfully.  Otherwise, it is set to -1. <br>
    ! ------------------------------------------------------------------------------ <br>
    ! **Important note**: <br>
    !   Using this routine requires a careful consideration
    !   according to the technical notes below. <br>
    ! ------------------------------------------------------------------------------ <br>
    ! **Technical Notes**: <br>
    ! (1) If the specified file does not exists, a new output file will be opened and
    !   the argument Append is ignored. <br>
    ! (2) If the specified file exists and currently is open, all optional arguments
    !   are ignored and the unit number associated with it will be returned. <br>
    ! (3) If the specified file exists (but is not open) and Append is not present or false,
    !   the existing file will be deleted and a file with the same name will be open as a
    !   new output file. That is the existing one is replaced by the new one. <br>
    ! (3.1) However, If DirAcc is present and true but RecLen is NOT specified, the existing
    !   file will be left as it is, and the unit number is set to -1. <br>
    ! (4) If the specified file exists (but is not open) and Append is present and true,
    !   the existing file will be opened for writing where the new data are written after
    !   existing data. Both DirAcc and RecLen arguments are ignored since, for an existing
    !   file, existing access mode and format will be retained.

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: Name     !! file name
    tLogical, OPTIONAL, INTENT(IN)  :: Append
    !^  flag indicating the file position <br>
    ! - true if the file is positioned at its terminal point <br>
    ! - false if the file is positioned at its initial point <br>
    ! - default = FalseVal
    tLogical, OPTIONAL, INTENT(IN)  :: DirAcc
    !^  flag indicating access mode <br>
    ! - true if requesting direct access mode <br>
    ! - false if requesting sequential access mode <br>
    ! - default = FalseVal <br>
    ! For direct access mode, the file is opened for an unformatted file. <br>
    ! For sequential access mode, the file is opened for a formatted file.
    tSInt32,  OPTIONAL, INTENT(IN)  :: RecLen
    !^  (fixed) length of records for direct access mode. <br>
    !   If DirAcc is present and true, RecLen must be specified.
    tSInt32                         :: IOUnit   !! unit number
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tLogical            :: Sequential
    tLogical            :: AsIs
    tSInt32             :: iStat
    tCharLen(MsgLen)    :: cMsg

!** FLOW

    ! set defaults, check the input
    AsIs       = TrueVal
    Sequential = TrueVal
    IF (PRESENT(Append)) AsIs = .NOT.Append
    IF (PRESENT(DirAcc)) Sequential = .NOT.DirAcc
    IF (LEN_TRIM(Name)==0) THEN
        CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
        CALL Output_Message('The specified name is an empty string.')
        CALL Output_Message('--------------------------------------')
        IOUnit = -1
        RETURN
    END IF
    
    IF (DoesFileExist(Name)) THEN
        IF (IsFileOpen(Name)) THEN
            ! technical note #2
            IOUnit = GetFileIOUnit(Name)
        ELSE
            IF (AsIs) THEN
                ! technical note #3
                IF (.NOT.Sequential.AND..NOT.PRESENT(RecLen)) THEN
                    ! technical note #3.1
                    CALL Output_Message('---               WARNING ERROR MESSAGE              ---')
                    CALL Output_Message('DirAcc is present and true, but RecLen is not specified.')
                    CALL Output_Message('--------------------------------------------------------')
                    IOUnit = -1
                    RETURN
                END IF
                IF (Sequential) THEN
                    ! replace existing file with new sequential and formatted file
                    IOUnit = GetNewIOUnit()
                    OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='REPLACE', ACTION='WRITE', &
                         IOSTAT=iStat, IOMSG=cMsg)
                ELSE
                    ! replace existing file with new direct access and unformatted file
                    IF (PRESENT(RecLen)) THEN
                        IOUnit = GetNewIOUnit()
                        OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='REPLACE', ACTION='WRITE', &
                             ACCESS='DIRECT', RECL=RecLen, IOSTAT=iStat, IOMSG=cMsg)
                    END IF
                END IF
            ELSE
                ! technical note #4
                IOUnit = GetNewIOUnit()
                OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='OLD', ACTION='WRITE', &
                     POSITION='APPEND', IOSTAT=iStat, IOMSG=cMsg)
            END IF

            ! check I/O status
            IF (iStat /= 0) THEN
                CALL Output_Message('---         WARNING ERROR MESSAGE         ---')
                CALL Output_Message('Cannot open ' // Name // ' as an output file.')
                CALL Output_Message('IOSTAT = ' // ToChar(iStat))
                CALL Output_Message('IOMSG  = ' // cMsg) 
                CALL Output_Message('---------------------------------------------')
                IOUnit = -1
            END IF
        END IF
    ELSE
        ! technical note #1
        IOUnit = OpenNewOutputFile(Name,DirAcc,RecLen)
    END IF
    
    RETURN
   
END FUNCTION OpenOutputFile

!******************************************************************************

FUNCTION OpenNewFile(Name,DirAcc,RecLen) RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^  To open a new file associated with the specified name for both reading and writing.
    !   A valid (positive) unit number is return if the specified file is opened successfully.
    !   Otherwise, it is set to -1. 

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: Name     !! file name
    tLogical, OPTIONAL, INTENT(IN)  :: DirAcc
    !^ flag indicating access mode <br>
    ! - true if requesting direct access mode <br>
    ! - false if requesting sequential access mode <br>
    ! - default = FalseVal <br>
    ! For direct access mode, the file is opened for an unformatted file. <br>
    ! For sequential access mode, the file is opened for a formatted file.
    tSInt32,  OPTIONAL, INTENT(IN)  :: RecLen
    !^  (fixed) length of records for direct access mode. <br>
    !   If DirAcc is present and true, RecLen must be specified.
    tSInt32                         :: IOUnit   !! unit number
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tLogical            :: Sequential
    tSInt32             :: iStat
    tCharLen(MsgLen)    :: cMsg

!** FLOW

    ! set default, check the input
    Sequential = TrueVal
    IF (PRESENT(DirAcc)) Sequential = .NOT.DirAcc
    IF (LEN_TRIM(Name)==0) THEN
        CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
        CALL Output_Message('The specified name is an empty string.')
        CALL Output_Message('--------------------------------------')
        IOUnit = -1
        RETURN
    ELSEIF (DoesFileExist(Name)) THEN
        CALL Output_Message('---                 WARNING ERROR MESSAGE                ---')
        CALL Output_Message('Cannot open an existing file (' // Name // ') as a new file.')
        CALL Output_Message('------------------------------------------------------------')
        IOUnit = -1
        RETURN
    ELSEIF (.NOT.Sequential.AND..NOT.PRESENT(RecLen)) THEN
        CALL Output_Message('---               WARNING ERROR MESSAGE              ---')
        CALL Output_Message('DirAcc is present and true, but RecLen is not specified.')
        CALL Output_Message('--------------------------------------------------------')
        IOUnit = -1
        RETURN
    END IF

    ! open file for reading and writing
    IF (Sequential) THEN
        ! open sequential and formatted file
        IOUnit = GetNewIOUnit()
        OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='NEW', ACTION='READWRITE', &
             IOSTAT=iStat, IOMSG=cMsg)
    ELSE
        ! open direct access and unformatted file
        IF (PRESENT(RecLen)) THEN
            IOUnit = GetNewIOUnit()
            OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='NEW', ACTION='READWRITE', &
                 ACCESS='DIRECT', RECL=RecLen, IOSTAT=iStat, IOMSG=cMsg)
        END IF
    END IF
    
    ! check I/O status
    IF (iStat /= 0) THEN
        CALL Output_Message('---       WARNING ERROR MESSAGE       ---')
        CALL Output_Message('Cannot open ' // Name // ' as a new file.')
        CALL Output_Message('IOSTAT = ' // ToChar(iStat))
        CALL Output_Message('IOMSG  = ' // cMsg) 
        CALL Output_Message('-----------------------------------------')
        IOUnit = -1
    END IF

    RETURN
   
END FUNCTION OpenNewFile

!******************************************************************************

FUNCTION OpenFile(Name,Append,DirAcc,RecLen) RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^  To open a file associated with the specified name for both reading and writing.
    !   A valid (positive) unit number is return if the specified file is opened successfully.
    !   Otherwise, it is set to -1. <br>
    ! ------------------------------------------------------------------------------ <br>
    ! **Important note**: <br>
    !   Using this routine requires a careful consideration
    !   according to the technical notes below. <br>
    ! ------------------------------------------------------------------------------ <br>
    ! **Technical Notes**: <br>
    ! (1) If the specified file does not exists, a new file will be opened and
    !   the argument Append is ignored. <br>
    ! (2) If the specified file exists and currently is open, all optional arguments
    !   are ignored and the unit number associated with it will be returned. <br>
    ! (3) If the specified file exists (but is not open) and Append is not present,
    !   the existing file will be open with "AsIs" position.  Both DirAcc and RecLen arguments
    !   are ignored since, for an existing file, existing access mode and format will be retained. <br>
    ! (4) If the specified file exists (but is not open) and Append is present and true,
    !   the existing file will be opened with "Append" position. Both DirAcc and RecLen arguments
    !   are ignored since, for an existing file, existing access mode and format will be retained. <br>
    ! (5) If the specified file exists (but is not open) and Append is present and false,
    !   the existing file will be deleted and a file with the same name will be open as a
    !   new file. That is the existing one is replaced by the new one. <br>
    ! (5.1) However, If DirAcc is present and true but RecLen is NOT specified, the existing
    !   file will be left as it is, and the unit number is set to -1.

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: Name     !! file name
    tLogical, OPTIONAL, INTENT(IN)  :: Append
    !^  flag indicating the file position <br>
    ! - true if the file is positioned at its terminal point <br>
    ! - false if the file is positioned at its initial point <br>
    ! - default = FalseVal
    tLogical, OPTIONAL, INTENT(IN)  :: DirAcc
    !^  flag indicating access mode <br>
    ! - true if requesting direct access mode <br>
    ! - false if requesting sequential access mode <br>
    ! - default = FalseVal <br>
    ! For direct access mode, the file is opened for an unformatted file. <br>
    ! For sequential access mode, the file is opened for a formatted file.
    tSInt32,  OPTIONAL, INTENT(IN)  :: RecLen
    !^  (fixed) length of records for direct access mode. <br>
    !   If DirAcc is present and true, RecLen must be specified.
    tSInt32                         :: IOUnit   !! unit number
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tLogical            :: Sequential
    tSInt32             :: iStat
    tCharLen(MsgLen)    :: cMsg

!** FLOW

    ! set defaults, check the input
    Sequential = TrueVal
    IF (PRESENT(DirAcc)) Sequential = .NOT.DirAcc
    IF (LEN_TRIM(Name)==0) THEN
        CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
        CALL Output_Message('The specified name is an empty string.')
        CALL Output_Message('--------------------------------------')
        IOUnit = -1
        RETURN
    END IF
    
    IF (DoesFileExist(Name)) THEN
        IF (IsFileOpen(Name)) THEN
            ! technical note #2
            IOUnit = GetFileIOUnit(Name)
        ELSE
            IF (PRESENT(Append)) THEN
                IF (Append) THEN
                    ! technical note #4
                    IOUnit = GetNewIOUnit()
                    OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='OLD', ACTION='READWRITE', &
                         POSITION='APPEND', IOSTAT=iStat, IOMSG=cMsg)
                ELSE
                    ! technical note #5
                    IF (.NOT.Sequential.AND..NOT.PRESENT(RecLen)) THEN
                        ! technical note #5.1
                        CALL Output_Message('---               WARNING ERROR MESSAGE              ---')
                        CALL Output_Message('DirAcc is present and true, but RecLen is not specified.')
                        CALL Output_Message('--------------------------------------------------------')
                        IOUnit = -1
                        RETURN
                    END IF
                    IF (Sequential) THEN
                        ! replace existing file with new sequential and formatted file
                        IOUnit = GetNewIOUnit()
                        OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='REPLACE', ACTION='READWRITE', &
                             IOSTAT=iStat, IOMSG=cMsg)
                    ELSE
                        ! replace existing file with new direct access and unformatted file
                        IF (PRESENT(RecLen)) THEN
                            IOUnit = GetNewIOUnit()
                            OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='REPLACE', ACTION='READWRITE', &
                                 ACCESS='DIRECT', RECL=RecLen, IOSTAT=iStat, IOMSG=cMsg)
                        END IF
                    END IF
                END IF
            ELSE
                ! technical note #3
                IOUnit = GetNewIOUnit()
                OPEN(UNIT=IOUnit, FILE=TRIM(Name), STATUS='OLD', ACTION='READWRITE', &
                     POSITION='ASIS', IOSTAT=iStat, IOMSG=cMsg)
            END IF
            
            ! check I/O status
            IF (iStat /= 0) THEN
                CALL Output_Message('---            WARNING ERROR MESSAGE            ---')
                CALL Output_Message('Cannot open ' // Name // ' for reading and writing.')
                CALL Output_Message('IOSTAT = ' // ToChar(iStat))
                CALL Output_Message('IOMSG  = ' // cMsg) 
                CALL Output_Message('---------------------------------------------------')
                IOUnit = -1
            END IF
        END IF
    ELSE
        ! technical note #1
        IOUnit = OpenNewFile(Name,DirAcc,RecLen)
    END IF
    
    RETURN
   
END FUNCTION OpenFile

!******************************************************************************

SUBROUTINE CloseFile_Unit(IOUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To close a file associated with the specified unit number.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tSInt32, INTENT(IN) :: IOUnit   !! I/O unit number
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na
   
!** FLOW

    CLOSE(UNIT = IOUnit)
   
    RETURN

END SUBROUTINE CloseFile_Unit

!******************************************************************************

SUBROUTINE CloseFile_Name(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To close a file associated with the specified unit number.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: Name     !! file name
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32     :: IOUnit
   
!** FLOW

    IOUnit = GetFileIOUnit(Name)
    CLOSE(UNIT = IOUnit)
   
    RETURN

END SUBROUTINE CloseFile_Name

!******************************************************************************

SUBROUTINE CloseOpenFiles()

!** PURPOSE OF THIS SUBROUTINE:
    !^ To scan potential unit numbers and closes any that are still open.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    ! na
          
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tLogical    :: EXISTS, OPENED
    tSInt32     :: IOUnit
    tSInt32     :: IOS
   
!** FLOW

    DO IOUnit = 1, MaxUnitNumber
        INQUIRE (UNIT = IOUnit, EXIST = EXISTS,  OPENED = OPENED, IOSTAT = IOS)
        IF (EXISTS .AND. OPENED .AND. IOS == 0) CLOSE(IOUnit)
    END DO
   
    RETURN

END SUBROUTINE CloseOpenFiles

!******************************************************************************

FUNCTION GetFileIOUnit(Name) RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^ To return a unit number of the file associated with the specified name.

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: Name     !! file name
    tSInt32                 :: IOUnit   !! unit number 
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    INQUIRE (FILE = Name, NUMBER = IOUnit)

    RETURN
   
END FUNCTION GetFileIOUnit

!******************************************************************************

FUNCTION GetNewIOUnit() RESULT(IOUnit)

!** PURPOSE OF THIS FUNCTION:
    !^ To return a input/output unit number that can exist and is not connected.

!** FUNCTION ARGUMENT DEFINITIONS:
    tSInt32             :: IOUnit   !! I/O unit number
     
!** FUNCTION PARAMETER DEFINITIONS:
    !  IO Status Values:
    tSInt32, PARAMETER  :: END_OF_RECORD = -2
    tSInt32, PARAMETER  :: END_OF_FILE = -1
    !  Indicate default input and output units:
    tSInt32, PARAMETER  :: DEFAULT_INPUT_UNIT = 5
    tSInt32, PARAMETER  :: DEFAULT_OUTPUT_UNIT = 6
    !  Indicate number and value of pre-connected units
    tSInt32, PARAMETER  :: NUMBER_OF_PRECONNECTED_UNITS = 2
    tSInt32, PARAMETER  :: PRECONNECTED_UNITS (NUMBER_OF_PRECONNECTED_UNITS) = (/ 5, 6 /)

!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tLogical    :: EXISTS, OPENED
    tSInt32     :: IOS

!** FLOW

    DO IOUnit = 11, MaxUnitNumber
        IF (IOUnit == DEFAULT_INPUT_UNIT .OR. IOUnit == DEFAULT_OUTPUT_UNIT) CYCLE
        IF (ANY (IOUnit == PRECONNECTED_UNITS)) CYCLE
        INQUIRE (UNIT = IOUnit, EXIST = EXISTS,  OPENED = OPENED, IOSTAT = IOS)
        IF (EXISTS .AND. .NOT. OPENED .AND. IOS == 0) RETURN      ! result is set in IOUnit
    END DO
   
    IOUnit = -1

    RETURN
   
END FUNCTION GetNewIOUnit

!******************************************************************************

FUNCTION GetIOUnits_AllOpenFile(IOUnits) RESULT(nUnits)

!** PURPOSE OF THIS FUNCTION:
    !^ To return a number of currently opened files and their associated unit numbers.

!** FUNCTION ARGUMENT DEFINITIONS:
    tSInt32, ALLOCATABLE, INTENT(OUT)   :: IOUnits(:)   !! unit numbers of opened files
    tSInt32                             :: nUnits       !! number of opened files
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tSInt32    :: Step, iUnit

!** FLOW

    DO Step = 1, 2
        nUnits = 0
        DO iUnit = 11, MaxUnitNumber
            IF (IsFileOpen(iUnit)) THEN
                IF (Step == 1) THEN
                    ! only count the opened file
                    nUnits = nUnits + 1
                ELSE
                    ! count and get the unit number of the opened file
                    nUnits = nUnits + 1
                    IOUnits(nUnits) = iUnit
                END IF
            END IF
        END DO
        IF (Step == 1) THEN
            ! allocate output data
            ALLOCATE(IOUnits(nUnits))
        END IF
    END DO

    RETURN
   
END FUNCTION GetIOUnits_AllOpenFile

!******************************************************************************

FUNCTION IsFileOpen_UnitNumber(IOUnit) RESULT(Status)

!** PURPOSE OF THIS FUNCTION:
    !^ To check whether the file associated with the specified unit number is open or not

!** FUNCTION ARGUMENT DEFINITIONS:
    tSInt32, INTENT(IN) :: IOUnit   !! I/O unit number
    tLogical            :: Status   !! status flag; true if the file is open
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    INQUIRE (UNIT = IOUnit, OPENED = Status)
    
    RETURN
   
END FUNCTION IsFileOpen_UnitNumber

!******************************************************************************

FUNCTION IsFileOpen_FileName(Name) RESULT(Status)

!** PURPOSE OF THIS FUNCTION:
    !^ To check whether the file associated with the specified name is open or not

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: Name     !! file name
    tLogical                :: Status   !! status flag; true if the file is open
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    INQUIRE (FILE = Name, OPENED = Status)
    
    RETURN
   
END FUNCTION IsFileOpen_FileName

!******************************************************************************

FUNCTION DoesFileExist(Name) RESULT(Status)

!** PURPOSE OF THIS FUNCTION:
    !^ To check whether the file associated with the specified name exists or not

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: Name     !! file name
    tLogical                :: Status   !! status flag; true if the file exists
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    INQUIRE (FILE = Name, EXIST = Status)
    
    RETURN
   
END FUNCTION DoesFileExist

!******************************************************************************

SUBROUTINE Output_Message(Message, IOUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To display the specified message on default output (an audit file) or the
    !   indicated file unit number if specified.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,         INTENT(IN)       :: Message
    tSInt32, OPTIONAL, INTENT(INOUT)    :: IOUnit
          
!** SUBROUTINE PARAMETER DEFINITIONS:
    tCharStar, PARAMETER    :: OutputFormat = '(2X,A)'
  
!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tSInt32    :: OutputUnit

!** FLOW
    
    IF (PRESENT(IOUnit)) THEN
        
        ! check whether the specified unit number is connected to an open file
        IF (.NOT.IsFileOpen(IOUnit)) THEN

            ! open file
            IOUnit = OpenOutputFile('UserOutput.Txt',Append=TrueVal)

        END IF
        
        ! write message to the specified file unit number
        IF (IOUnit /= -1) THEN
            WRITE(IOUnit,OutputFormat) TRIM(ADJUSTL(Message))
        ELSE
            ! error occurred while opening file
            WRITE(*,*) 'Error occurred while opening a user-specified file.'
            WRITE(*,*) 'The specified message is "' // TRIM(ADJUSTL(Message)) // '".'
        END IF

    ELSE
        
        ! write to the log file so check whether the log file is currently open or not
        IF (IsFileOpen(AudFileName)) THEN
            
            ! get existing unit number for the opened file
            OutputUnit = GetFileIOUnit(AudFileName)
            
            ! then write message to the audit file
            WRITE(OutputUnit,OutputFormat) TRIM(ADJUSTL(Message))
            
        ELSE
            
            ! open file
            OutputUnit = OpenOutputFile(AudFileName,Append=TrueVal)
        
            IF (OutputUnit /= -1) THEN
                WRITE(OutputUnit,OutputFormat) TRIM(ADJUSTL(Message))
            ELSE
                ! error occurred while opening file
                WRITE(*,*) 'Error occurred while opening an audit file.'
                WRITE(*,*) 'The specified message is "' // TRIM(ADJUSTL(Message)) // '".'
            END IF

            ! then write message to the audit file
            WRITE(OutputUnit,OutputFormat) TRIM(ADJUSTL(Message))
            
        END IF
        
    ENDIF

    RETURN

END SUBROUTINE Output_Message

!******************************************************************************

SUBROUTINE ReadSqfrm_LineCharAlloc(IOUnit, Str, IOStat, IOMsg)

!** PURPOSE OF THIS ROUTINE:
    !^ To read a character sequence from a connected sequential-formatted unit into the character string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,              INTENT(IN)    :: IOUnit   !! connected I/O unit number
    tCharAlloc,           INTENT(OUT)   :: Str      !! character string
    tSInt32,    OPTIONAL, INTENT(OUT)   :: IOStat   !! status of I/O operation
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: IOMsg    !! an I/O message

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(10)    :: Acc
    tCharLen(9)     :: Act
    tCharLen(11)    :: Frm
    tLogical        :: ValidAccess, ValidAction, ValidForm
    tCharAlloc      :: LineRecord
    tSInt32         :: IO_Stat  ! status of I/O operation
    tCharLen(256)   :: IO_Msg   ! an I/O message if is IOStat is non-zero

!** FLOW:
    
    ! write a warning message and simply return if the file has not yet been open
    IF (.NOT.IsFileOpen(IOUnit)) THEN
        IF (Verbose) THEN
            CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
            CALL Output_Message('ReadInput: The specified file has not yet been open.')
            CALL Output_Message('--------------------------------------')
        END IF
        RETURN
    END IF

    ! check validities of access, action and form of the specified file
    INQUIRE (UNIT = IOUnit, ACCESS=Acc, ACTION=Act, FORM=Frm)
    ValidAccess = (TRIM(Acc) == 'SEQUENTIAL')
    ValidAction = (TRIM(Act) == 'READ').OR.(TRIM(Act) == 'READWRITE')
    ValidForm   = (TRIM(Frm) == 'FORMATTED')
    IF (.NOT.ValidAccess) THEN
        IF (Verbose) THEN
            CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
            CALL Output_Message('ReadInput: The access of the specified file is NOT "sequential".')
            CALL Output_Message('--------------------------------------')
        END IF
        RETURN
    ELSEIF (.NOT.ValidAction) THEN
        IF (Verbose) THEN
            CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
            CALL Output_Message('ReadInput: The action of the specified file is NEITHER "read" NOR "readwrite".')
            CALL Output_Message('--------------------------------------')
        END IF
        RETURN
    ELSEIF (.NOT.ValidForm) THEN
        IF (Verbose) THEN
            CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
            CALL Output_Message('ReadInput: The form of the specified file is NOT "formatted".')
            CALL Output_Message('--------------------------------------')
        END IF
        RETURN
    END IF

    ! read the line record
    CALL ReadLine(LineRecord, IO_Stat, IO_Msg, IOUnit)

    ! transfer optional output
    IF (PRESENT(IOStat)) IOStat = IO_Stat
    IF (PRESENT(IOMsg))  IOMsg  = TRIM(IO_Msg)

    IF ((IO_Stat == 0).OR.IS_IOSTAT_END(IOStat)) THEN
        ! transfer output
        CALL MOVE_ALLOC(LineRecord, Str)
    ELSE
        Str = ''
        IF (Verbose) THEN
            CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
            CALL Output_Message('ReadInput: ' // TRIM(IO_Msg))
            CALL Output_Message('--------------------------------------')
        END IF
    END IF

    RETURN

    CONTAINS

    SUBROUTINE ReadLine(Line, IOStat, IOMsg, IOUnit)

    !** PURPOSE OF THIS ROUTINE:
        ! To read a whole line from a connected formatted unit.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharAlloc, INTENT(OUT)     :: Line     ! data record
        tSInt32,    INTENT(OUT)     :: IOStat   ! status of I/O operation
        tCharStar,  INTENT(INOUT)   :: IOMsg    ! an I/O message if is IOStat is non-zero
        tSInt32,    INTENT(IN)      :: IOUnit   ! connected I/O unit number

    !** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        tCharAlloc  :: Buffer
        tChar       :: C
        tIndex      :: Count, BufSize

    !** FLOW:

        ! initialize
        BufSize = 256_kIndex
        ALLOCATE(tCharLen(BufSize) :: Buffer)
        Count = 0_kIndex
        IOStat = 0

        ! loop thorough all characters in the current line (record)
        DO
            ! read a character
            READ(UNIT=IOUnit, FMT='(A)', IOSTAT=IOStat, IOMSG=IOMsg, ADVANCE='NO') C
            ! check I/O status
            IF (IOStat /= 0) EXIT
            ! increment character count
            Count = Count + 1
            IF (Count <= BufSize) THEN
                ! add the current character to the buffer
                Buffer(Count:Count) = C
            ELSE
                ! allocate new buffer, add the current character to the new buffer
                ! and move allocation of the new buffer to the working one
                BLOCK
                    tCharAlloc  :: NewBuf
                    BufSize = BufSize*2_kIndex
                    ALLOCATE(tCharLen(BufSize) :: NewBuf)
                    NewBuf(1:Count-1) = Buffer(1:Count-1)
                    NewBuf(Count:Count) = C
                    CALL MOVE_ALLOC(NewBuf, Buffer)
                END BLOCK
            END IF
        END DO

        IF (IS_IOSTAT_EOR(IOStat).OR.IS_IOSTAT_END(IOStat)) THEN
            ! reset IOStat for end of record status
            IF (IS_IOSTAT_EOR(IOStat)) IOStat = 0
            ALLOCATE(tCharLen(Count) :: Line)
            Line(1:Count) = Buffer(1:Count)
        END IF

        RETURN

    END SUBROUTINE ReadLine

    !******************************************************************************

END SUBROUTINE ReadSqfrm_LineCharAlloc

!******************************************************************************

SUBROUTINE ReadSqfrm_LineCharStar(IOUnit, Str, IOStat, IOMsg, EndPos)

!** PURPOSE OF THIS ROUTINE:
    !^ To read a character sequence from a connected sequential-formatted unit into the character string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,              INTENT(IN)    :: IOUnit   !! connected I/O unit number
    tCharStar,            INTENT(OUT)   :: Str      !! character string
    tSInt32,    OPTIONAL, INTENT(OUT)   :: EndPos   !! ending position
    tSInt32,    OPTIONAL, INTENT(OUT)   :: IOStat   !! status of I/O operation
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: IOMsg    !! an I/O message

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(10)    :: Acc
    tCharLen(9)     :: Act
    tCharLen(11)    :: Frm
    tLogical        :: ValidAccess, ValidAction, ValidForm
    tSInt32         :: IO_Stat  ! status of I/O operation
    tCharLen(256)   :: IO_Msg   ! an I/O message if is IOStat is non-zero

!** FLOW:
    
    ! write a warning message and simply return if the file has not yet been open
    IF (.NOT.IsFileOpen(IOUnit)) THEN
        IF (Verbose) THEN
            CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
            CALL Output_Message('ReadInput: The specified file has not yet been open.')
            CALL Output_Message('--------------------------------------')
        END IF
        RETURN
    END IF

    ! check validities of access, action and form of the specified file
    INQUIRE (UNIT = IOUnit, ACCESS=Acc, ACTION=Act, FORM=Frm)
    ValidAccess = (TRIM(Acc) == 'SEQUENTIAL')
    ValidAction = (TRIM(Act) == 'READ').OR.(TRIM(Act) == 'READWRITE')
    ValidForm   = (TRIM(Frm) == 'FORMATTED')
    IF (.NOT.ValidAccess) THEN
        IF (Verbose) THEN
            CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
            CALL Output_Message('ReadInput: The access of the specified file is NOT "sequential".')
            CALL Output_Message('--------------------------------------')
        END IF
        RETURN
    ELSEIF (.NOT.ValidAction) THEN
        IF (Verbose) THEN
            CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
            CALL Output_Message('ReadInput: The action of the specified file is NEITHER "read" NOR "readwrite".')
            CALL Output_Message('--------------------------------------')
        END IF
        RETURN
    ELSEIF (.NOT.ValidForm) THEN
        IF (Verbose) THEN
            CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
            CALL Output_Message('ReadInput: The form of the specified file is NOT "formatted".')
            CALL Output_Message('--------------------------------------')
        END IF
        RETURN
    END IF

    ! read the line record
    CALL ReadLine(Str, IO_Stat, IO_Msg, IOUnit, EndPos)

    ! transfer optional output
    IF (PRESENT(IOStat)) IOStat = IO_Stat
    IF (PRESENT(IOMsg))  IOMsg  = TRIM(IO_Msg)

    IF (IO_Stat /= 0) THEN
        IF (.NOT.IS_IOSTAT_END(IO_Stat).AND.Verbose) THEN
            CALL Output_Message('---      WARNING ERROR MESSAGE     ---')
            CALL Output_Message('ReadInput: ' // TRIM(IO_Msg))
            CALL Output_Message('--------------------------------------')
        END IF
    END IF

    RETURN

    CONTAINS

    SUBROUTINE ReadLine(Line, IOStat, IOMsg, IOUnit, EndPos)

    !** PURPOSE OF THIS ROUTINE:
        ! To read a whole line from a connected formatted unit.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(OUT)      :: Line     ! data record
        tSInt32,   INTENT(OUT)      :: IOStat   ! status of I/O operation
        tCharStar, INTENT(INOUT)    :: IOMsg    ! an I/O message if is IOStat is non-zero
        tSInt32,   INTENT(IN)       :: IOUnit   ! connected I/O unit number
        tSInt32,   INTENT(OUT)      :: EndPos   !! ending position
        OPTIONAL                    :: EndPos

    !** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        tChar       :: C
        tIndex      :: Count, StrLen

    !** FLOW:

        ! initialize
        StrLen = LEN(Line, KIND=kIndex)
        Count = 0_kIndex
        IOStat = 0

        ! loop thorough all characters in the current line (record)
        DO WHILE (Count <= StrLen)
            ! read a character
            READ(UNIT=IOUnit, FMT='(A)', IOSTAT=IOStat, IOMSG=IOMsg, ADVANCE='NO') C
            ! check I/O status
            IF (IOStat /= 0) EXIT
            ! increment character count
            Count = Count + 1
            ! add the current character to the line
            Line(Count:Count) = C
        END DO

        ! reset IOStat for end of record status
        IF (IS_IOSTAT_EOR(IOStat)) IOStat = 0
        IF (Count < StrLen) Line(Count+1:StrLen) = ' '
        IF (PRESENT(EndPos)) EndPos = Count

        RETURN

    END SUBROUTINE ReadLine

    !******************************************************************************

END SUBROUTINE ReadSqfrm_LineCharStar

!******************************************************************************

FUNCTION CountSqfrm_Lines(FileName, SkipBlank, Comment, ErrMsg) RESULT(LineTot)

!** PURPOSE OF THIS ROUTINE:
    !^ To count the total number of lines in the specified file name where the file
    !  can be opened as a sequential-formatted file.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: FileName     !! file name
    tLogical,   OPTIONAL, INTENT(IN)    :: SkipBlank
    !^ If present and true, do not count an empty line; otherwise, also count an empty line.
    tChar,      OPTIONAL, INTENT(IN)    :: Comment
    !^ comment character; if present, the line started with the specified character is not
    !  counted.
    tCharAlloc, OPTIONAL, INTENT(INOUT) :: ErrMsg       !! error message if error occurred
    tSInt32                             :: LineTot      !! total number of lines

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tChar           :: C
    tLogical        :: NoBlank
    tSInt32         :: IOUnit
    tSInt32         :: IOStat  ! status of I/O operation
    tCharLen(256)   :: IOMsg   ! an I/O message if is IOStat is non-zero

!** FLOW:
    
    ! initialize
    LineTot = 0
    IOStat  = 0
    IOMsg   = ' '
    SET_OPTION(NoBlank, FalseVal, SkipBlank)

    ! check whether the file exists or not
    IOUnit = OpenInputFile(FileName)
    IF (IOUnit == -1) THEN
        IF (PRESENT(ErrMsg)) ErrMsg = 'An error occurred while trying to open the specified file.'
        RETURN
    END IF
    
    IF (NoBlank.OR.PRESENT(Comment)) THEN
        BLOCK
            tCharLen(8192)  :: LineRecord
            tCharAlloc      :: IO_Msg
            tSInt32         :: EndPos
            ! do not count empty and/or comment lines
            DO
                ! read a line
                CALL ReadSqfrm_LineCharStar(IOUnit, LineRecord, IOStat, IO_Msg, EndPos)
                ! check I/O status
                IF (IOStat /= 0) THEN
                    IOMsg = IO_Msg
                    IF (.NOT.IS_IOSTAT_END(IOStat)) EXIT
                END IF
                ! exclude comment line
                IF (PRESENT(Comment)) THEN
                    LineRecord = ADJUSTL(LineRecord(1:EndPos))
                    IF (LineRecord(1:1) == Comment) CYCLE
                END IF
                ! exclude empty line
                IF (NoBlank) THEN
                    IF (TRIM(ADJUSTL(LineRecord(1:EndPos))) == '') CYCLE
                END IF
                ! increment line count
                LineTot = LineTot + 1
                IF (IS_IOSTAT_END(IOStat)) EXIT
            END DO
        END BLOCK
    ELSE
        ! count both empty and comment lines
        DO
            ! read without I/O list
            READ(UNIT=IOUnit, FMT='(A)', IOSTAT=IOStat, IOMSG=IOMsg, ADVANCE='YES') C
            ! check I/O status
            IF (IOStat /= 0) EXIT
            ! increment line count
            LineTot = LineTot + 1
        END DO
    END IF
    
    IF (.NOT.IS_IOSTAT_END(IOStat)) THEN
        IF (PRESENT(ErrMsg)) ErrMsg = TRIM(IOMsg)
    END IF

    RETURN

END FUNCTION CountSqfrm_Lines

!******************************************************************************

FUNCTION GetFileName(FullPath, PathName, ExtName, IncludeSep) RESULT(FileName)

!** PURPOSE OF THIS FUNCTION:
    !^ To extract and return the file name (which includes the extension name) from
    !  the specified full path name.  Optionally, also return the path name and/or
    !  the extension name if present and exists.

!** FUNCTION ARGUMENT DEFINITIONS:
    tCharStar,            INTENT(IN)    :: FullPath     !! full path name (i.e. path + file)
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: PathName     !! path name only if present and exists
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ExtName      !! extension name only if present and exists
    tLogical,   OPTIONAL, INTENT(IN)    :: IncludeSep
    !^ if present and true, include the separator in the path name; otherwise, exclude it
    tCharAlloc                          :: FileName     !! file (+ extension) name
     
!** FUNCTION LOCAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, K, SLen
    tChar       :: C

!** FLOW

    ! get length of the specified full path name
    SLen = LEN_TRIM(FullPath)
    
    ! scan from the back to find the last path separator ('/' or '\')
    I = SLen
    J = SLen + 1
    DO WHILE (I > 0_kIndex)
        C = FullPath(I:I)
        IF ((C == '/').OR.(C == '\')) EXIT
        IF (J > SLen) THEN
            ! if this is extension separator, record it position
            IF (C == '.') J = I
        END IF
        I = I - 1_kIndex
    END DO
    
    ! get file and path names
    IF (I > 1) THEN
        ! path name including the separator ends at I
        IF (PRESENT(PathName)) THEN
            ! set last index excluding the separator
            K = I - 1
            IF (PRESENT(IncludeSep)) THEN
                ! add 1 to the last index if including the separator
                IF (IncludeSep) K = K + 1
            END IF
            PathName = FullPath(1:K)
        END IF
        ! file name starts at I+1
        FileName = FullPath(I+1:SLen)
    ELSE
        ! no directory specified
        IF (PRESENT(PathName)) PathName = ''
        FileName = FullPath(1:SLen)
    END IF
    
    ! get extension name
    IF (PRESENT(ExtName)) THEN
        ! extension starts at J + 1
        IF (J < SLen-1) THEN
            ExtName = FullPath(J+1:SLen)
        ELSE
            ExtName = ''
        END IF
    END IF

    RETURN
   
END FUNCTION GetFileName

!******************************************************************************

END MODULE MBase_IOHandlers

!******************************************************************************
