
MODULE MBase_ChrStr

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains routines that handle and manipulate character strings.
!   These routines supplement the Fortran intrinsic procedures provided for
!   the Fortran's *CHARACTER* type.  The routines provided in this module
!   can be categorized into 5 groups as follows. <br>
!   (1) *Character* procedures are routines for a single character.  The following
!       list shows available (i.e. public) character procedures. <br>
!       - IsCharacterInClass, <br>
!       - ChangeCaseCharacter, and <br>
!       - CharacterDescription. <br>
!   (2) *Assignment* procedures are conversion routines between a character string
!       and an other (Fortran intrinsic) type.  These conversion routines are intended
!       to be used in an assignment expression.  <br>
!   (3) *Conversion* procedures are conversion routines between a character string
!       and an other (Fortran intrinsic) type.  These conversion routines are NOT
!       intended to be used in an assignment expression; therefore, they have
!       different interfaces from those intended for an assignment expression.
!       The following list shows available (i.e. public) conversion procedures. <br>
!       - CharString, <br>
!       - ToChrArrFixed, <br>
!       - ToChrArrAlloc, <br>
!       - ToCString, <br>
!       - ParseByte, <br>
!       - ParseShort, <br>
!       - ParseInteger, <br>
!       - ParseLong, <br>
!       - ParseRSingle, <br>
!       - ParseRDouble, <br>
!       - ParseRQuad, <br>
!       - ParseCSingle, <br>
!       - ParseCDouble, <br>
!       - ParseCQuad, and <br>
!       - ParseLogical. <br>
!   (4) *Inquiry* procedures are routines that inquire information relating to
!       a character string.  The following list shows available (i.e. public)
!       inquiry procedures. <br>
!       - IsStringNumber, <br>
!       - IsStringLogical, <br>
!       - IsStringInClass, <br>
!       - IsStringInCharacterSet, <br>
!       - CountSubString, <br>
!       - CountCharacters, <br>
!       - CountCharactersProtect, <br>
!       - CountWords, <br>
!       - FindProtectedRegions, <br>
!       - FindSubstring, <br>
!       - FindDelimiters, <br>
!       - FindSeparators, <br>
!       - FindSubstringProtect, <br>
!       - FindDelimitersProtect, <br>
!       - FindSeparatorsProtect, <br>
!       - StartWith, <br>
!       - EndWith, <br>
!       - GetSubstring, and <br>
!       - GetSlice. <br>
!   (5) *Manipulation* procedures are routines that perform a manipulation on
!       a character string.  The following list shows available (i.e. public)
!       manipulation procedures. <br>
!       - ChangeCase, <br>
!       - BlankCompressChangeCase, <br>
!       - CropBlanks, <br>
!       - CompactString, <br>
!       - CompressString, <br>
!       - InsertSubstring, <br>
!       - RemoveCharacters, <br>
!       - RemoveCharactersProtect, <br>
!       - RemoveSubstring, <br>
!       - RemoveSubstringProtect, <br>
!       - ReplaceSubstring, <br>
!       - ReplaceSubstringProtect, <br>
!       - ReplaceSubstringRecursive <br>
!       - Partition, <br>
!       - Split, and <br>
!       - SplitProtect. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_CharUtil
    USE MBase_DoublyLinkedLists,   ONLY: QueueChar => ListCharacter

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! conversion procedures
    PUBLIC :: CharString
    PUBLIC :: ToChrArrFixed, ToChrArrAlloc, ToCString
    PUBLIC :: ParseByte, ParseShort, ParseInteger, ParseLong
    PUBLIC :: ParseRSingle, ParseRDouble, ParseRQuad
    PUBLIC :: ParseCSingle, ParseCDouble, ParseCQuad
    PUBLIC :: ParseLogical
    PUBLIC :: ASSIGNMENT(=)
    ! inquiry procedures
    PUBLIC :: IsStringNumber, IsStringLogical, IsStringInClass, IsStringInCharacterSet
    PUBLIC :: CountSubString, CountCharacters, CountCharactersProtect, CountWords
    PUBLIC :: FindProtectedRegions, FindSubstring, FindDelimiters, FindSeparators
    PUBLIC :: FindSubstringProtect, FindDelimitersProtect, FindSeparatorsProtect
    PUBLIC :: StartWith, EndWith, GetSubstring, GetSlice
    ! case procedures
    PUBLIC :: ChangeCase, BlankCompressChangeCase
    ! editing procedures
    PUBLIC :: CropBlanks, CompactString, CompressString
    PUBLIC :: InsertSubstring,  Partition, Split, SplitProtect
    PUBLIC :: RemoveCharacters, RemoveCharactersProtect
    PUBLIC :: RemoveSubstring,  RemoveSubstringProtect
    PUBLIC :: ReplaceSubstring, ReplaceSubstringProtect, ReplaceSubstringRecursive
    ! character procedures
    PUBLIC :: IsCharacterInClass, ChangeCaseCharacter, CharacterDescription

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharParam          :: ModName = 'MBase_ChrStr'
    !------------------------------------------------------------------
    !-----                  FailIndex parameters                  -----
    !------------------------------------------------------------------
    !% unknown index
    tSInt32,  PARAMETER, PUBLIC     :: ID_UNKNOWN_INDEX     = -5
    !% character string not yet allocated
    tSInt32,  PARAMETER, PUBLIC     :: ID_NOT_ALLOCATED     = -4
    !% character string with zero length
    tSInt32,  PARAMETER, PUBLIC     :: ID_ZERO_LENGTH       = -3
    !% missing character
    tSInt32,  PARAMETER, PUBLIC     :: ID_MISSING_CHARACTER = -2
    !% empty string
    tSInt32,  PARAMETER, PUBLIC     :: ID_EMPTY_STRING      = -1
    !% fail index not applicable
    tSInt32,  PARAMETER, PUBLIC     :: ID_NO_FAILINDEX      =  0

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    !------------------------------------------------------------------
    !-----      Interfaces for 'Character' Procedures             -----
    !------------------------------------------------------------------
    INTERFACE IsCharacterInClass
        !^ **Function Interface**: IsCharacterInClass <br>
        !  **Purpose**:  To check whether a given character is in the specified class. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsCharacterInClass('A', 'ASCII', FailIndex) <br>
        !   --->    IF (.NOT.IsCharacterInClass('5', 'ALPHANUM')) DoSomething
        MODULE FUNCTION IsCharacterInClass(Chr, ClassType, FailIndex) RESULT(ClassFlag)
            !^ To check whether a given character is in the specified class where
            !  the following character classes are recognized. <br>
            !  - 'ALPHABET': The given character is a valid letter [a-zA-Z]. <br>
            !  - 'ALPHANUM': The given character is a valid letter or digit [a-zA-Z0-9]. <br>
            !  - 'ASCII': The given character is a valid ASCII character. <br>
            !  - 'BLANK': The given character is a valid blank character, that is blank
            !       space or tab. <br>
            !  - 'CONTROL': The given character is a valid control character where control
            !       characters are in the ranges 00..1F and 7F..9F, that is from ASCII #0
            !       to #31 and from #127 to #159. <br>
            !  - 'DIGIT': The given character is a valid digit [0-9]. <br>
            !  - 'GRAPHICAL': The given character is a valid graphical character not
            !       including space that is from ASCII #33 to #126. <br>
            !  - 'LOGICAL': The given character is a valid logical value, that is 't', 'T',
            !       'f', and 'F'. <br>
            !  - 'LOWERCASE': The given character is a valid lower-case letter, that is
            !       [a-z]. <br>
            !  - 'PUNCTUATION': The given character is a valid punctuation character, that
            !       is _,;:.?![](){}@"'. <br>
            !  - 'PRINTABLE': The given character is a valid printable character including 
            !       space that is from ASCII #32 to #126. <br>
            !  - 'UPPERCASE': The given character is a valid upper-case letter, that is
            !       [A-Z]. <br>
            !  - 'WHITESPACE': The given character is a valid white space, that is space,
            !       tab, vertical tab, form-feed, newline or carriage return. <br>
            !  - 'HEXDIGIT': The given character is a valid hexadecimal digit characters
            !       that is [0-9A-Fa-f]. <br>
            !  - 'OCTDIGIT': The given character is a valid octal digit characters
            !       that is [0-9A-Fa-f].
            tChar,            INTENT(IN)    :: Chr          !! the specified character
            tCharStar,        INTENT(IN)    :: ClassType    !! character class
            tIndex, OPTIONAL, INTENT(OUT)   :: FailIndex
            !^ return 1 if the character class is valid; otherwise, return 0.
            tLogical                        :: ClassFlag    !! true if the character is in the specified class
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ChangeCaseCharacter
        !^ **Subroutine Interface**: ChangeCaseCharacter <br>
        !  **Purpose**:  To change case of the given character according to flag. <br>
        !  **Usage**: <br>
        !   ! if *Char* is an alphabet character, change it to its upper case <br>
        !   --->    CALL ChangeCaseCharacter(Char, .TRUE.)
        !   ! if *Char* is an alphabet character, change it to its lower case <br>
        !   --->    CALL ChangeCaseCharacter(Char, .FALSE.)
        !   ! if *Char* is NOT an alphabet character, nothing done <br>
        !   --->    CALL ChangeCaseCharacter(Char, .TRUE.)
        MODULE ELEMENTAL SUBROUTINE ChangeCaseCharacter(Chr, ToUpper)
            !^ To change case of the given character according to flag.
            tChar,    INTENT(INOUT) :: Chr      !! the specified character
            tLogical, INTENT(IN)    :: ToUpper
            !^ flag indicating whether to change the character to upper case <br>
            ! - true if requesting an uppercase character <br>
            ! - false if requesting a lowercase character
        END SUBROUTINE ChangeCaseCharacter
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE CharacterDescription
        !^ **Function Interface**: CharacterDescription <br>
        !  **Purpose**:  To provide a description of the given character. <br>
        !  **Usage**: <br>
        !   --->    Description = CharacterDescription('~')
        MODULE FUNCTION CharacterDescription(Chr) RESULT(Description)
            !^ To provide a description of the given character.
            tChar, INTENT(IN)   :: Chr          !! the specified character
            tCharAlloc          :: Description  !! the character description
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    !------------------------------------------------------------------
    !-----      Interfaces for 'Assignment' Procedures            -----
    !------------------------------------------------------------------
    INTERFACE ASSIGNMENT(=)
        !^ **Operator Overload**: ASSIGNMENT(=) <br>
        !  **Purpose**:  To convert between a character string and an other
        !                (Fortran intrinsic) type via an assignment expression. <br>
        !  **Usage**: <br>
        !   ! convert a 64-bit integer number to a character string <br>
        !   --->    cStr = I64Num <br>
        !   ! convert a character string to a quadruple-precision real number <br>
        !   --->    RQPNum = cStr <br>
        !  **Important Note**: The *assignment* and *conversion* operations are
        !            functionally similar but have some subtle differences.  In
        !            particular for those procedures that convert from a decimal
        !            string to a number, the *assignment* procedures will silently
        !            convert to a proper representation if the input string is
        !            *INVALID* whereas the *conversion* procedures can optionally
        !            report the error occurred. <br>
        MODULE SUBROUTINE ChrStr_From_IByte(cStr,IntNum)
            !^ To convert from a 8-bit integer number to a character string
            !  via an assignment expression. <br>
            !  *Usage*: cStr = IntNum
            tCharAlloc, INTENT(OUT) :: cStr
            tSInt8,     INTENT(IN)  :: IntNum
        END SUBROUTINE ChrStr_From_IByte
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_From_IShort(cStr,IntNum)
            !^ To convert from a 16-bit integer number to a character string
            !  via an assignment expression. <br>
            !  *Usage*: cStr = IntNum
            tCharAlloc, INTENT(OUT) :: cStr
            tSInt16,    INTENT(IN)  :: IntNum
        END SUBROUTINE ChrStr_From_IShort
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_From_Integer(cStr,IntNum)
            !^ To convert from a 32-bit integer number to a character string
            !  via an assignment expression. <br>
            !  *Usage*: cStr = IntNum
            tCharAlloc, INTENT(OUT) :: cStr
            tSInt32,    INTENT(IN)  :: IntNum
        END SUBROUTINE ChrStr_From_Integer
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_From_ILong(cStr,IntNum)
            !^ To convert from a 64-bit integer number to a character string
            !  via an assignment expression. <br>
            !  *Usage*: cStr = IntNum
            tCharAlloc, INTENT(OUT) :: cStr
            tSInt64,    INTENT(IN)  :: IntNum
        END SUBROUTINE ChrStr_From_ILong
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_From_RSingle(cStr,RealNum)
            !^ To convert from a single-precision real number to a character string
            !  via an assignment expression. <br>
            !  *Usage*: cStr = RealNum
            tCharAlloc, INTENT(OUT) :: cStr
            tRealSP,    INTENT(IN)  :: RealNum
        END SUBROUTINE ChrStr_From_RSingle
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_From_RDouble(cStr,RealNum)
            !^ To convert from a double-precision real number to a character string
            !  via an assignment expression. <br>
            !  *Usage*: cStr = RealNum
            tCharAlloc, INTENT(OUT) :: cStr
            tRealDP,    INTENT(IN)  :: RealNum
        END SUBROUTINE ChrStr_From_RDouble
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_From_RQuad(cStr,RealNum)
            !^ To convert from a quadruple-precision real number to a character string
            !  via an assignment expression. <br>
            !  *Usage*: cStr = RealNum
            tCharAlloc, INTENT(OUT) :: cStr
            tRealQP,    INTENT(IN)  :: RealNum
        END SUBROUTINE ChrStr_From_RQuad
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_From_CSingle(cStr,CmpxNum)
            !^ To convert from a single-precision complex number to a character string
            !  via an assignment expression. <br>
            !  *Usage*: cStr = CmpxNum
            tCharAlloc,  INTENT(OUT)    :: cStr
            tCmpxSP,     INTENT(IN)     :: CmpxNum
        END SUBROUTINE ChrStr_From_CSingle
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_From_CDouble(cStr,CmpxNum)
            !^ To convert from a double-precision complex number to a character string
            !  via an assignment expression. <br>
            !  *Usage*: cStr = CmpxNum
            tCharAlloc,  INTENT(OUT)    :: cStr
            tCmpxDP,     INTENT(IN)     :: CmpxNum
        END SUBROUTINE ChrStr_From_CDouble
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_From_CQuad(cStr,CmpxNum)
            !^ To convert from a quadruple-precision complex number to a character string
            !  via an assignment expression. <br>
            !  *Usage*: cStr = CmpxNum
            tCharAlloc, INTENT(OUT) :: cStr
            tCmpxQP,    INTENT(IN)  :: CmpxNum
        END SUBROUTINE ChrStr_From_CQuad
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_From_Logical(cStr,Boolean)
            !^ To convert from a logical value to a character string
            !  via an assignment expression. <br>
            !  *Usage*: cStr = Boolean
            tCharAlloc, INTENT(OUT)  :: cStr
            tLogical,   INTENT(IN)   :: Boolean
        END SUBROUTINE ChrStr_From_Logical
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_To_IByte(IntNum,cStr)
            !^ To convert from a character string to a 8-bit integer number
            !  via an assignment expression. <br>
            !  *Usage*: IntNum = cStr
            tCharStar, INTENT(IN)   :: cStr
            tSInt8,    INTENT(OUT)  :: IntNum
        END SUBROUTINE ChrStr_To_IByte
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_To_IShort(IntNum,cStr)
            !^ To convert from a character string to a 16-bit integer number
            !  via an assignment expression. <br>
            !  *Usage*: IntNum = cStr
            tCharStar, INTENT(IN)   :: cStr
            tSInt16,   INTENT(OUT)  :: IntNum
        END SUBROUTINE ChrStr_To_IShort
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_To_Integer(IntNum,cStr)
            !^ To convert from a character string to a 32-bit integer number
            !  via an assignment expression. <br>
            !  *Usage*: IntNum = cStr
            tCharStar, INTENT(IN)   :: cStr
            tSInt32,   INTENT(OUT)  :: IntNum
        END SUBROUTINE ChrStr_To_Integer
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_To_ILong(IntNum,cStr)
            !^ To convert from a character string to a 64-bit integer number
            !  via an assignment expression. <br>
            !  *Usage*: IntNum = cStr
            tCharStar, INTENT(IN)   :: cStr
            tSInt64,   INTENT(OUT)  :: IntNum
        END SUBROUTINE ChrStr_To_ILong
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_To_RSingle(RealNum,cStr)
            !^ To convert from a character string to a single-precision real number
            !  via an assignment expression. <br>
            !  *Usage*: RealNum = cStr
            tCharStar, INTENT(IN)   :: cStr
            tRealSP,   INTENT(OUT)  :: RealNum
        END SUBROUTINE ChrStr_To_RSingle
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_To_RDouble(RealNum,cStr)
            !^ To convert from a character string to a double-precision real number
            !  via an assignment expression. <br>
            !  *Usage*: RealNum = cStr
            tCharStar, INTENT(IN)   :: cStr
            tRealDP,   INTENT(OUT)  :: RealNum
        END SUBROUTINE ChrStr_To_RDouble
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_To_RQuad(RealNum,cStr)
            !^ To convert from a character string to a quadruple-precision real number
            !  via an assignment expression. <br>
            !  *Usage*: RealNum = cStr
            tCharStar, INTENT(IN)   :: cStr
            tRealQP,   INTENT(OUT)  :: RealNum
        END SUBROUTINE ChrStr_To_RQuad
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_To_CSingle(CmpxNum,cStr)
            !^ To convert from a character string to a single-precision complex number
            !  via an assignment expression. <br>
            !  *Usage*: CmpxNum = cStr
            tCharStar,   INTENT(IN)     :: cStr
            tCmpxSP,     INTENT(OUT)    :: CmpxNum
        END SUBROUTINE ChrStr_To_CSingle
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_To_CDouble(CmpxNum,cStr)
            !^ To convert from a character string to a double-precision complex number
            !  via an assignment expression. <br>
            !  *Usage*: CmpxNum = cStr
            tCharStar,   INTENT(IN)     :: cStr
            tCmpxDP,     INTENT(OUT)    :: CmpxNum
        END SUBROUTINE ChrStr_To_CDouble
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_To_CQuad(CmpxNum,cStr)
            !^ To convert from a character string to a quadruple-precision complex number
            !  via an assignment expression. <br>
            !  *Usage*: CmpxNum = cStr
            tCharStar, INTENT(IN)   :: cStr
            tCmpxQP,   INTENT(OUT)  :: CmpxNum
        END SUBROUTINE ChrStr_To_CQuad
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChrStr_To_Logical(Boolean,cStr)
            !^ To convert from a character string to a logical value
            !  via an assignment expression. <br>
            !  *Usage*: Boolean = cStr
            tCharStar, INTENT(IN)   :: cStr
            tLogical,  INTENT(OUT)  :: Boolean
        END SUBROUTINE ChrStr_To_Logical
        ! ---------------------------------------------------------------------
    END INTERFACE
    !------------------------------------------------------------------
    !-----      Interfaces for 'Conversion' Procedures            -----
    !------------------------------------------------------------------
    INTERFACE CharString
        !^ **Function Interface**: CharString <br>
        !  **Purpose**:  To construct a character string based on specified input. <br>
        !  **Usage**: <br>
        !   ! construct a character string from an array of characters <br>
        !   --->    cStr = CharString(cArray) <br>
        !   ! construct a character string from double-precision real number <br>
        !   --->    cStr = CharString(R64, IsScientific=.TRUE.) <br>
        MODULE FUNCTION CharacterArray_To_ChrStr(cArr,IsCString) RESULT(cStr)
            !^ To convert a character array to a character string.
            tChar,              INTENT(IN)  :: cArr(:)  !! array of characters
            tLogical, OPTIONAL, INTENT(IN)  :: IsCString
            !^ flag indicating whether the array is a 'C' string or not. <br>
            !  If true, the array must contain a null character. <br>
            !  Default is FALSE.
            tCharAlloc                      :: cStr     !! character string
        END FUNCTION CharacterArray_To_ChrStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION IByte_To_ChrStr(IntNum) RESULT(cStr)
            !^ To convert an 8-bit integer value to a character string.
            tCharAlloc              :: cStr
            tSInt8,     INTENT(IN)  :: IntNum
        END FUNCTION IByte_To_ChrStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION IShort_To_ChrStr(IntNum) RESULT(cStr)
            !^ To convert an 16-bit integer value to a character string.
            tCharAlloc              :: cStr
            tSInt16,    INTENT(IN)  :: IntNum
        END FUNCTION IShort_To_ChrStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION Integer_To_ChrStr(IntNum) RESULT(cStr)
            !^ To convert an 32-bit integer value to a character string.
            tCharAlloc              :: cStr
            tSInt32,    INTENT(IN)  :: IntNum
        END FUNCTION Integer_To_ChrStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ILong_To_ChrStr(IntNum) RESULT(cStr)
            !^ To convert an 64-bit integer value to a character string.
            tCharAlloc              :: cStr
            tSInt64,    INTENT(IN)  :: IntNum
        END FUNCTION ILong_To_ChrStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION RSingle_To_ChrStr(RealNum,IsScientific) RESULT(cStr)
            !^ To convert a single-precision real value to a character string.
            tRealSP,            INTENT(IN)  :: RealNum
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ flag indicating whether the string is expressed in the scientific format. <br>
            ! Default is FALSE where the string is expressed in the general format.
            tCharAlloc                      :: cStr
        END FUNCTION RSingle_To_ChrStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION RDouble_To_ChrStr(RealNum,IsScientific) RESULT(cStr)
            !^ To convert a double-precision real value to a character string.
            tRealDP,            INTENT(IN)  :: RealNum
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ flag indicating whether the string is expressed in the scientific format. <br>
            ! Default is FALSE where the string is expressed in the general format.
            tCharAlloc                      :: cStr
        END FUNCTION RDouble_To_ChrStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION RQuad_To_ChrStr(RealNum,IsScientific) RESULT(cStr)
            !^ To convert a quadruple-precision real value to a character string.
            tRealQP,            INTENT(IN)  :: RealNum
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ flag indicating whether the string is expressed in the scientific format. <br>
            ! Default is FALSE where the string is expressed in the general format.
            tCharAlloc                      :: cStr
        END FUNCTION RQuad_To_ChrStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CSingle_To_ChrStr(CmpxNum,IsScientific) RESULT(cStr)
            !^ To convert a single-precision complex value to a character string.
            tCmpxSP,            INTENT(IN)  :: CmpxNum
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ flag indicating whether the string is expressed in the scientific format. <br>
            ! Default is FALSE where the string is expressed in the general format.
            tCharAlloc                      :: cStr
        END FUNCTION CSingle_To_ChrStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CDouble_To_ChrStr(CmpxNum,IsScientific) RESULT(cStr)
            !^ To convert a double-precision complex value to a character string.
            tCmpxDP,            INTENT(IN)  :: CmpxNum
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ flag indicating whether the string is expressed in the scientific format. <br>
            ! Default is FALSE where the string is expressed in the general format.
            tCharAlloc                      :: cStr
        END FUNCTION CDouble_To_ChrStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CQuad_To_ChrStr(CmpxNum,IsScientific) RESULT(cStr)
            !^ To convert a quadruple-precision complex value to a character string.
            tCmpxQP,            INTENT(IN)  :: CmpxNum
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ flag indicating whether the string is expressed in the scientific format. <br>
            ! Default is FALSE where the string is expressed in the general format.
            tCharAlloc                      :: cStr
        END FUNCTION CQuad_To_ChrStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION Logical_To_ChrStr(Boolean) RESULT(cStr)
            !^ To convert a logical value to a character string.
            tCharAlloc              :: cStr
            tLogical, INTENT(IN)    :: Boolean
        END FUNCTION Logical_To_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ToChrArrAlloc
        !^ **Function Interface**: ToChrArrAlloc <br>
        !  **Purpose**:  To convert a character string to an allocatable array
        !                of characters. <br>
        !  **Usage**: <br>
        !   ! convert a character string to a character array <br>
        !   --->    cArray = ToChrArrAlloc(cStr) <br>
        !   ! convert a character string to a character array with a null character <br>
        !   --->    cArray = ToChrArrAlloc(cStr, IsCString=.TRUE.) <br>
        MODULE FUNCTION CharArray_From_ChrStr_I(cStr,IsCString) RESULT(cArr)
            !^ To convert a character string to an allocatable array of characters.
            tCharStar,          INTENT(IN)  :: cStr     !! a character string
            tLogical, OPTIONAL, INTENT(IN)  :: IsCString
            !^ flag indicating whether the array is a 'C' string or not. <br>
            !  If true, the array will contain a null character. <br>
            !  Default is FALSE.
            tChar, ALLOCATABLE              :: cArr(:)  !! an array of characters
        END FUNCTION CharArray_From_ChrStr_I
       ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ToChrArrFixed
        !^ **Function Interface**: ToChrArrFixed <br>
        !  **Purpose**:  To convert a character string to an explicit-shape array
        !                of characters. <br>
        !  **Usage**: <br>
        !   --->    cArray = ToChrArrFixed(cStr)
        MODULE FUNCTION CharArray_From_ChrStr_II(cStr) RESULT(cArr)
            !^ To convert a character string to an explicit-shape array of characters.
            tCharStar, INTENT(IN)   :: cStr             !! a character string
            tChar                   :: cArr(LEN(cStr))  !! an array of characters
        END FUNCTION CharArray_From_ChrStr_II
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ToCString
        !^ **Function Interface**: ToCString <br>
        !  **Purpose**:  To convert a character string to a 'C' style string, which is an
        !                explicit-shape array of characters with a null character added. <br>
        !  **Usage**: <br>
        !   --->    cArray = ToCString(cStr)
        MODULE FUNCTION CString_From_ChrStr(cStr) RESULT(cArr)
            !^ To convert a character string to a 'C' style string.
            tCharStar, INTENT(IN)   :: cStr                 !! a character string
            tChar                   :: cArr(LEN(cStr)+1)    !! a 'C' string
        END FUNCTION CString_From_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ParseByte
        !^ **Function Interface**: ParseByte <br>
        !  **Purpose**:  To parse a character string as a 8-bit integer number. <br>
        !  **Usage**: <br>
        !   --->    IntNum = ParseByte(cStr) <br>
        !   --->    IntNum = ParseByte(cStr, ErrFlag, ErrMsg) <br>
        MODULE FUNCTION IByte_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(IntNum)
            !^ To convert a character string to a 8-bit integer number.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tSInt8                              :: IntNum   !! integer number
        END FUNCTION IByte_From_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ParseShort
        !^ **Function Interface**: ParseShort <br>
        !  **Purpose**:  To parse a character string as a 16-bit integer number. <br>
        !  **Usage**: <br>
        !   --->    IntNum = ParseShort(cStr) <br>
        !   --->    IntNum = ParseShort(cStr, ErrFlag, ErrMsg) <br>
        MODULE FUNCTION IShort_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(IntNum)
            !^ To convert a character string to a 16-bit integer number.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tSInt16                             :: IntNum   !! integer number
        END FUNCTION IShort_From_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ParseInteger
        !^ **Function Interface**: ParseInteger <br>
        !  **Purpose**:  To parse a character string as a 32-bit integer number. <br>
        !  **Usage**: <br>
        !   --->    IntNum = ParseInteger(cStr) <br>
        !   --->    IntNum = ParseInteger(cStr, ErrFlag, ErrMsg) <br>
        MODULE FUNCTION Integer_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(IntNum)
            !^ To convert a character string to a 32-bit integer number.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tSInt32                             :: IntNum   !! integer number
        END FUNCTION Integer_From_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ParseLong
        !^ **Function Interface**: ParseLong <br>
        !  **Purpose**:  To parse a character string as a 64-bit integer number. <br>
        !  **Usage**: <br>
        !   --->    IntNum = ParseLong(cStr) <br>
        !   --->    IntNum = ParseLong(cStr, ErrFlag, ErrMsg) <br>
        MODULE FUNCTION ILong_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(IntNum)
            !^ To convert a character string to a 64-bit integer number.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tSInt64                             :: IntNum   !! integer number
        END FUNCTION ILong_From_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ParseRSingle
        !^ **Function Interface**: ParseRSingle <br>
        !  **Purpose**:  To parse a character string as a single-precision real number. <br>
        !  **Usage**: <br>
        !   --->    RealNum = ParseRSingle(cStr) <br>
        !   --->    RealNum = ParseRSingle(cStr, ErrFlag, ErrMsg) <br>
        MODULE FUNCTION RSingle_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(RealNum)
            !^ To convert a character string to a single-precision real number.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealSP                             :: RealNum  !! real number
        END FUNCTION RSingle_From_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ParseRDouble
        !^ **Function Interface**: ParseRDouble <br>
        !  **Purpose**:  To parse a character string as a double-precision real number. <br>
        !  **Usage**: <br>
        !   --->    RealNum = ParseRDouble(cStr) <br>
        !   --->    RealNum = ParseRDouble(cStr, ErrFlag, ErrMsg) <br>
        MODULE FUNCTION RDouble_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(RealNum)
            !^ To convert a character string to a double-precision real number.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealDP                             :: RealNum  !! real number
        END FUNCTION RDouble_From_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ParseRQuad
        !^ **Function Interface**: ParseRQuad <br>
        !  **Purpose**:  To parse a character string as a quadruple-precision real number. <br>
        !  **Usage**: <br>
        !   --->    RealNum = ParseRQuad(cStr) <br>
        !   --->    RealNum = ParseRQuad(cStr, ErrFlag, ErrMsg) <br>
        MODULE FUNCTION RQuad_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(RealNum)
            !^ To convert a character string to a quadruple-precision real number.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tRealQP                             :: RealNum  !! real number
        END FUNCTION RQuad_From_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ParseCSingle
        !^ **Function Interface**: ParseCSingle <br>
        !  **Purpose**:  To parse a character string as a single-precision complex number. <br>
        !  **Usage**: <br>
        !   --->    CmpxNum = ParseCSingle(cStr) <br>
        !   --->    CmpxNum = ParseCSingle(cStr, ErrFlag, ErrMsg) <br>
        !  **Note**: A valid string representing a complex number consists of a pair of
        !            real-number (or integer-number) strings, separated by a comma, and
        !            enclosed in parentheses.
        MODULE FUNCTION CSingle_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(CmpxNum)
            !^ To convert a character string to a single-precision complex number.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tCmpxSP                             :: CmpxNum  !! complex number
        END FUNCTION CSingle_From_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ParseCDouble
        !^ **Function Interface**: ParseCDouble <br>
        !  **Purpose**:  To parse a character string as a double-precision complex number. <br>
        !  **Usage**: <br>
        !   --->    CmpxNum = ParseCDouble(cStr) <br>
        !   --->    CmpxNum = ParseCDouble(cStr, ErrFlag, ErrMsg) <br>
        !  **Note**: A valid string representing a complex number consists of a pair of
        !            real-number (or integer-number) strings, separated by a comma, and
        !            enclosed in parentheses.
        MODULE FUNCTION CDouble_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(CmpxNum)
            !^ To convert a character string to a double-precision complex number.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tCmpxDP                             :: CmpxNum  !! complex number
        END FUNCTION CDouble_From_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ParseCQuad
        !^ **Function Interface**: ParseCQuad <br>
        !  **Purpose**:  To parse a character string as a quadruple-precision complex number. <br>
        !  **Usage**: <br>
        !   --->    CmpxNum = ParseCQuad(cStr) <br>
        !   --->    CmpxNum = ParseCQuad(cStr, ErrFlag, ErrMsg) <br>
        !  **Note**: A valid string representing a complex number consists of a pair of
        !            real-number (or integer-number) strings, separated by a comma, and
        !            enclosed in parentheses.
        MODULE FUNCTION CQuad_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(CmpxNum)
            !^ To convert a character string to a quadruple-precision complex number.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tCmpxQP                             :: CmpxNum  !! complex number
        END FUNCTION CQuad_From_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ParseLogical
        !^ **Function Interface**: ParseLogical <br>
        !  **Purpose**:  To parse a character string as a logical value. <br>
        !  **Usage**: <br>
        !   --->    Boolean = ParseLogical(cStr) <br>
        MODULE FUNCTION Logical_From_ChrStr(cStr) RESULT(Boolean)
            !^ To convert a character string to a logical value.
            tCharStar, INTENT(IN)   :: cStr     !! character string
            LOGICAL                 :: Boolean  !! logical value
        END FUNCTION Logical_From_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    !------------------------------------------------------------------
    !-----          Interfaces for 'Inquiry' Procedures           -----
    !------------------------------------------------------------------
    INTERFACE IsStringNumber
        !^ **Function Interface**: IsStringNumber <br>
        !  **Purpose**:  To check whether a character string is a valid number and
        !                return a flag indicating what kind of number the string is. <br>
        !  **Usage**: <br>
        !   --->    NumFlag = IsStringNumber('123a31')                        ! return -1 <br>
        !   --->    NumFlag = IsStringNumber('123')                           ! return 0 <br>
        !   --->    NumFlag = IsStringNumber('1.23')                          ! return 0 <br>
        !   --->    NumFlag = IsStringNumber('123', Strict=.TRUE.)            ! return 1 <br>
        !   --->    NumFlag = IsStringNumber('1.23', Strict=.TRUE.)           ! return 2 <br>
        !   --->    NumFlag = IsStringNumber('(123, 456)')                    ! return 3 <br>
        !   --->    NumFlag = IsStringNumber('(12.3, 4.56)', NumVal=CmplxNum) ! return 3 and also get number <br>
        !  **Technical Notes**: <br>
        !   A (strict) integer number is a whole number with no decimal point.
        !   It can have a leading sign and is interpreted as a decimal number.
        !   It takes a general form of [s]n[n...] where <br>
        !   - s is a sign; required if negative (-), optional if positive (+). <br>
        !   - n is a decimal digit (0 through 9). <br>
        !   A (strict) real number is a number with decimal point or an exponent part.
        !   The general form of a real number with no exponent part is [s]n[n...] and
        !   a real number with an exponent part has a general form of [s]n[n...]E[s]nn...
        !   where  <br>
        !   - s is a sign; required if negative (-), optional if positive (+). <br>
        !   - n is a decimal digit (0 through 9). A decimal point must appear if
        !     the real number has no exponent part. <br>
        !   - E is an exponent indicator where it can be 'E', 'e', 'D', 'd'. <br>
        !   A complex number is a pair of real or integer numbers, separated by a comma,
        !   and enclosed in parentheses.  The first number represents the real part and
        !   the second number represents the imaginary part.
        MODULE FUNCTION IsStringNumber(cStr,Strict,NumVal) RESULT(NumFlag)
            !^ To check whether a character string is a valid number and
            !  if so, what kind of number it is. <br>
            tCharStar,             INTENT(IN)               :: cStr
            !^ character string
            tLogical,              OPTIONAL, INTENT(IN)     :: Strict
            !^ true if requesting strict integer/real number; default is false.
            CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: NumVal
            !^ (optional) value of number if it is valid
            tSInt32                                         :: NumFlag
            !^ flag indicating what kind of number the string represents. <br>
            ! Return -1 if the string is NOT a number. <br>
            ! Return  0 if the string is a valid integer or real number. <br>
            ! Return  1 if the string is strictly an integer number. <br>
            ! Return  2 if the string is strictly a real number. <br>
            ! Return  3 if the string is a valid complex number.
        END FUNCTION IsStringNumber
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE IsStringLogical
        !^ **Function Interface**: IsStringLogical <br>
        !  **Purpose**: To check whether a character string is a logical value where
        !               valid one include 'T', 'F', 't', 'f', 'TRUE', 'FALSE', 'true',
        !               'false'. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsStringLogical('a') <br>
        !   --->    Flag = IsStringLogical('f', LogVal)
        MODULE FUNCTION IsStringLogical(cStr,Boolean) RESULT(LogFlag)
            tCharStar,          INTENT(IN)  :: cStr     !! character string
            tLogical, OPTIONAL, INTENT(OUT) :: Boolean  !! (optional) logical value
            tLogical                        :: LogFlag  !! true if the string is a valid logical value
        END FUNCTION IsStringLogical
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE IsStringInClass
        !^ **Function Interface**: IsStringInClass <br>
        !  **Purpose**:  To check whether a given string is in the specified class. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsStringInClass('AbCd', 'ASCII', FailIndex) <br>
        !   --->    IF (.NOT.IsStringInClass('32.105e-32', 'REAL') DoSomething
        RECURSIVE MODULE FUNCTION IsStringInClass(cStr,ClassType,FailIndex) RESULT(ClassFlag)
            !^ To check whether the given character string is in the specified class.
            !  To be in the specified class, all characters in the string must be
            !  valid characters of that class. <br>
            !  The recognized character string classes include those of character classes
            !  (see the <a href="../module/mbase_chrstr.html#interface-ischaracterinclass">
            !  IsCharacterInClass</a> routine) and the following additional classes: <br>
            !  - 'COMPLEX': The character string is a valid complex constant in Fortran,
            !       with optional sign and surrounding white spaces. <br>
            !  - 'FNAME': The character string is a valid FORTRAN name that can contain
            !       letters, digits, and underscores. The first character must be a letter. <br>
            !  - 'INTEGER': The character string is a valid integer constant in Fortran,
            !       with optional surrounding white spaces. <br>
            !  - 'LOGICAL': The character string is considered a valid logical value, that
            !       is 't', 'T', 'true', 'TRUE', 'f', 'F', 'false' and 'FALSE', with
            !       optional surrounding white spaces. <br>
            !  - 'REAL': The character string is a valid real constant in Fortran, with
            !       optional surrounding white spaces. 
            tCharStar,        INTENT(IN)    :: cStr         !! specified character string
            tCharStar,        INTENT(IN)    :: ClassType    !! character string class
            tIndex, OPTIONAL, INTENT(OUT)   :: FailIndex    !! flag indicating position of the failed character
            tLogical                        :: ClassFlag    !! true if the string is in the specified class
        END FUNCTION IsStringInClass
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE IsStringInCharacterSet
        !^ **Function Interface**: IsStringInCharacterSet <br>
        !  **Purpose**:  To check whether all characters in the character string are
        !                in the specified character set. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsStringInCharacterSet('Ab23Cd', SET_ALPHANUM, FailIndex) <br>
        !   --->    IF (.NOT.IsStringInCharacterSet('32.105Q-32', SET_NUMERICS) DoSomething
        MODULE FUNCTION IsStringInCharacterSet(cStr,ChrSet,FailID) RESULT(Flag)
            !^ To check whether all characters in the character string are
            !  in the given character set. <br>
            !  Note: This routine is an alternative to the *VERIFY* intrinsic function.
            tCharStar, INTENT(IN)   :: cStr     !! character string
            tCharStar, INTENT(IN)   :: ChrSet   !! character set
            tIndex,    INTENT(OUT)  :: FailID   !! index indicating position of invalid character
            tLogical                :: Flag     !! true if all characters are in the set
        END FUNCTION IsStringInCharacterSet
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE CountSubstring
        !^ **Function Interface**: CountSubstring <br>
        !  **Purpose**:  To count the number of occurrences of the given
        !                substring in the specified character string. <br>
        !  **Usage**: <br>
        !   --->    Count = CountSubstring(String, SubStr) <br>
        !   --->    Count = CountSubstring(String, SubStr, Overlap=.TRUE.)
        MODULE FUNCTION CountSubstring(cStr,sStr,Overlap) RESULT(nCount)
            !^ To count the number of occurrences of the given substring in the
            !  specified character string.
            tCharStar,          INTENT(IN)  :: cStr     !! character string
            tCharStar,          INTENT(IN)  :: sStr     !! substring
            tLogical, OPTIONAL, INTENT(IN)  :: Overlap
            !^ flag indicating whether overlapping occurrences of the substring
            !  are allowed or not. <br>
            !  - If true, count the overlapping occurrences. <br>
            !  - If false, count the non-overlapping occurrences . <br>
            !  Default is false.
            tIndex                          :: nCount   !! number of occurrences
        END FUNCTION CountSubstring
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE CountCharacters
        !^ **Function Interface**: CountCharacters <br>
        !  **Purpose**:  To count the number of occurrences of character(s) in the
        !                specified character string for any character appearing in
        !                the given character set. <br>
        !  **Usage**: <br>
        !   --->    Count = CountCharacters(String, CharSet)
        MODULE FUNCTION CountCharacters(cStr,ChrSet) RESULT(nCount)
            !^ To count the number of occurrences of character(s) in the specified
            !  character string for any character appearing in the given character set.
            tCharStar, INTENT(IN)   :: cStr     !! character string
            tCharStar, INTENT(IN)   :: ChrSet   !! character set
            tIndex                  :: nCount   !! number of occurrences
        END FUNCTION CountCharacters
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE CountCharactersProtect
        !^ **Function Interface**: CountCharactersProtect <br>
        !  **Purpose**:  To count the number of occurrences of delimiter(s) in the
        !                unprotected region(s) of the specified character string
        !                where a delimiter is any character appearing in the
        !                given character set. <br>
        !  **Usage**: <br>
        !   --->    Count = CountCharactersProtect(String, CharSet) <br>
        !   --->    Count = CountCharactersProtect(String, CharSet, ExclMrk=.FALSE.) <br>
        MODULE FUNCTION CountCharactersProtect(cStr,ChrSet,ExclMrk) RESULT(nCount)
            !^ To count the number of occurrences of delimiter(s) in the unprotected
            !  region(s) of the specified character string where a delimiter is any
            !  character appearing in the given character set. <br>
            !  See the <a href="../module/mbase_chrstr.html#interface-findprotectedregions">
            !  FindProtectedRegions</a> routine for explanations regarding the protected
            !  region(s).
            tCharStar,          INTENT(IN)  :: cStr     !! character string
            tCharStar,          INTENT(IN)  :: ChrSet   !! character set
            tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tIndex                          :: nCount   !! number of occurrences
        END FUNCTION CountCharactersProtect
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE CountWords
        !^ **Function Interface**: CountWords <br>
        !  **Purpose**:  To count the number of words (separated by blanks) in the
        !                specified character string where blanks are characters in
        !                the <a href="../module/mbase_chrstr.html#variable-set_blanks">
        !                SET_BLANKS</a> character set. <br>
        !  **Usage**: <br>
        !   --->    Count = CountWords(String)
        MODULE FUNCTION CountWords(cStr) RESULT(nCount)
            !^ To count the number of words (separated by blanks) in the specified
            !  character string.
            tCharStar, INTENT(IN)   :: cStr     !! character string
            tIndex                  :: nCount   !! number of words
        END FUNCTION CountWords
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE FindProtectedRegions
        !^ **Function Interface**: FindProtectedRegions <br>
        !  **Purpose**:  To find the number of protected regions marked by two (single
        !                or double) quotes and/or by an exclamation mark.  Also, return
        !                positions of the first and last characters of each region. <br>
        !  **Usage**: <br>
        !   --->    nRegion = FindProtectedRegions(String, lPos, rPos) <br>
        !   --->    nRegion = FindProtectedRegions(String, lPos, rPos, ExclMrk=.FALSE.)
        MODULE FUNCTION FindProtectedRegions(cStr,lPos,rPos,ExclMrk) RESULT(nRegion)
            !^ To look for quotes (and/or an exclamation mark) to find regions
            !  that must be protected from character string editing.  Return
            !  the number of protected regions as well as positions of the
            !  first and last characters of each region. <br>
            !  **Technical Notes**: <br>
            !  - Single quote, double quote and optionally exclamation mark are used as
            !    delimiters to find protected regions. <br>
            !  - Two single quotes or two double quotes are used to define a protected
            !    region whereas an exclamation mark indicates that all characters
            !    following it are all protected. <br>
            !  - This routine is designed specifically for manipulating Fortran source code
            !    where an exclamation mark is used for a comment and two (single or double)
            !    quotes are used to specify a value to a character variable or literal.
            tCharStar,           INTENT(IN)     :: cStr     !! character string
            tIndex, ALLOCATABLE, INTENT(OUT)    :: lPos(:)
            !^ positions of the first character of protected regions
            tIndex, ALLOCATABLE, INTENT(OUT)    :: rPos(:)
            !^ positions of the last character of protected regions
            tLogical, OPTIONAL,  INTENT(IN)     :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tIndex                              :: nRegion  !! number of protected regions
        END FUNCTION FindProtectedRegions
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE FindSubstring
        !^ **Function Interface**: FindSubstring <br>
        !  **Purpose**:  To count the number of non-overlapping occurrences of the given
        !                substring in the specified character string and also return
        !                position(s) of the first character of substring found. <br>
        !  **Usage**: <br>
        !   --->    Count = FindSubstring(String, SubStr, FirstPos)
        MODULE FUNCTION FindSubstring(cStr,sStr,sPos) RESULT(nCount)
            !^ To count the number of non-overlapping occurrences of the given
            !  substring in the specified character string and also return
            !  position(s) of the first character of substring found.
            tCharStar,           INTENT(IN)     :: cStr     !! character string
            tCharStar,           INTENT(IN)     :: sStr     !! substring
            tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)  !! position(s) of the first character of substring found
            tIndex                              :: nCount   !! number of occurrences
        END FUNCTION FindSubstring
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE FindSubstringProtect
        !^ **Function Interface**: FindSubstringProtect <br>
        !  **Purpose**:  To count the number of non-overlapping occurrences of the given
        !                substring in unprotected regions of the specified character
        !                string and also return position(s) of the first character of
        !                substring found. <br>
        !  **Usage**: <br>
        !   --->    Count = FindSubstringProtect(String, SubStr, FirstPos) <br>
        !   --->    Count = FindSubstringProtect(String, SubStr, FirstPos, ExclMrk=.FALSE.)
        MODULE FUNCTION FindSubstringProtect(cStr,sStr,sPos,ExclMrk) RESULT(nCount)
            !^ To count the number of non-overlapping occurrences of the given substring
            !  in unprotected regions of the specified character string and also return
            !  position(s) of the first character of substring found. <br>
            !  See the <a href="../module/mbase_chrstr.html#interface-findprotectedregions">
            !  FindProtectedRegions</a> routine for explanations regarding the protected
            !  region(s).
            tCharStar,           INTENT(IN)     :: cStr     !! character string
            tCharStar,           INTENT(IN)     :: sStr     !! substring
            tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)  !! position(s) of the first character of substring found
            tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tIndex                              :: nCount   !! number of occurrences
        END FUNCTION FindSubstringProtect
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE FindDelimiters
        !^ **Function Interface**: FindDelimiters <br>
        !  **Purpose**:  To count the number of occurrences of delimiter(s) in the specified
        !                character string and also return position(s) of the delimiter(s) found.
        !                A delimiter is any character appearing in the given character set. <br>
        !  **Usage**: <br>
        !   --->    Count = FindDelimiters(String, CharSet, DPos)
        MODULE FUNCTION FindDelimiters(cStr,ChrSet,dPos) RESULT(nCount)
            !^ To count the number of occurrences of delimiter(s) in the specified
            !  character string and also return position(s) of the delimiter(s) found.
            !  A delimiter is any character appearing in the given character set.
            tCharStar,           INTENT(IN)     :: cStr     !! character string
            tCharStar,           INTENT(IN)     :: ChrSet   !! a set of characters
            tIndex, ALLOCATABLE, INTENT(OUT)    :: dPos(:)  !! position(s) of the delimiter(s) found
            tIndex                              :: nCount   !! number of occurrences
        END FUNCTION FindDelimiters
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE FindDelimitersProtect
        !^ **Function Interface**: FindDelimitersProtect <br>
        !  **Purpose**:  To count the number of occurrences of delimiter(s) in unprotected
        !                regions of the specified character string and also return position(s)
        !                of the delimiter(s) found.  A delimiter is any character appearing
        !                in the given character set. <br>
        !  **Usage**: <br>
        !   --->    Count = FindDelimitersProtect(String, CharSet, DPos) <br>
        !   --->    Count = FindDelimitersProtect(String, CharSet, DPos, ExclMrk=.FALSE.)
        MODULE FUNCTION FindDelimitersProtect(cStr,ChrSet,dPos,ExclMrk) RESULT(nCount)
            !^ To count the number of occurrences of delimiter(s) in unprotected regions
            !  of the specified character string and also return position(s) of the
            !  delimiter(s) found.  A delimiter is any character appearing in the given
            !  character set. <br>
            !  See the <a href="../module/mbase_chrstr.html#interface-findprotectedregions">
            !  FindProtectedRegions</a> routine for explanations regarding the protected
            !  region(s).
            tCharStar,           INTENT(IN)     :: cStr     !! character string
            tCharStar,           INTENT(IN)     :: ChrSet   !! a set of characters
            tIndex, ALLOCATABLE, INTENT(OUT)    :: dPos(:)  !! position(s) of the delimiter(s) found
            tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tIndex                              :: nCount   !! number of occurrences
        END FUNCTION FindDelimitersProtect
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE FindSeparators
        !^ **Function Interface**: FindSeparators <br>
        !  **Purpose**:  To count the number of occurrences of separator(s) in the
        !                specified character string and also return (the first)
        !                position(s) of the separator(s) found. <br>
        !  **Usage**: <br>
        !   ! a separator is any (single) character in the *Separator* argument <br>
        !   --->    Count = FindSeparators(String, Separator, .TRUE., Pos) <br>
        !   ! a separator is a character string specified by the *Separator* argument <br>
        !   --->    Count = FindSeparators(String, Separator, .FALSE., Pos)
        MODULE FUNCTION FindSeparators(cStr,Separator,CharSet,sPos) RESULT(nCount)
            !^ To count the number of occurrences of separator(s) in the specified
            !  character string and also return (the first) position(s) of the
            !  separator(s) found. <br>
            !  A separator can be a (single) character or a character string (multiple
            !  characters). <br>
            !  The *CharSet* argument is a flag used to specify whether the separator
            !  is a character or a character string. <br>
            !  If the *CharSet* argument is true, the *Separator* argument contains a
            !  set of characters where a separator is any character in the set. <br>
            !  If the *CharSet* argument is false, the *Separator* argument specifies
            !  the character-string separator.
            tCharStar,           INTENT(IN)     :: cStr         !! character string
            tCharStar,           INTENT(IN)     :: Separator    !! separator
            tLogical,            INTENT(IN)     :: CharSet      !! a flag indicating type of the separator
            tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)      !! (first) position(s) of the separator(s) found
            tIndex                              :: nCount       !! number of occurrences
        END FUNCTION FindSeparators
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE FindSeparatorsProtect
        !^ **Function Interface**: FindSeparatorsProtect <br>
        !  **Purpose**:  To count the number of occurrences of separator(s) in the
        !                specified character string and also return (the first)
        !                position(s) of the separator(s) found. <br>
        !  **Usage**: <br>
        !   ! both quotes and an exclamation mark used to define protected regions.  <br>
        !   --->    Count = FindSeparatorsProtect(String, Separator, .TRUE., Pos)  ! separator is a single character <br>
        !   --->    Count = FindSeparatorsProtect(String, Separator, .FALSE., Pos) ! separator is a character string <br>
        !   ! only quotes used to define protected regions.  <br>
        !   --->    Count = FindSeparatorsProtect(String, Separator, .TRUE., Pos, ExclMrk=.FALSE.) <br>
        !   --->    Count = FindSeparatorsProtect(String, Separator, .FALSE., Pos, ExclMrk=.FALSE.)
        MODULE FUNCTION FindSeparatorsProtect(cStr,Separator,CharSet,sPos,ExclMrk) RESULT(nCount)
            !^ To count the number of occurrences of separator(s) in unprotected
            !  regions of the specified character string and also return (the first)
            !  position(s) of the separator(s) found. <br>
            !  See the <a href="../module/mbase_chrstr.html#interface-findseparators">
            !  FindSeparators</a> routine for explanations regarding the separator and
            !  its types.
            !  See the <a href="../module/mbase_chrstr.html#interface-findprotectedregions">
            !  FindProtectedRegions</a> routine for explanations regarding the protected
            !  region(s).
            tCharStar,           INTENT(IN)     :: cStr         !! character string
            tCharStar,           INTENT(IN)     :: Separator    !! separator
            tLogical,            INTENT(IN)     :: CharSet      !! a flag indicating type of the separator
            tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)      !! (first) position(s) of the separator(s) found
            tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tIndex                              :: nCount       !! number of occurrences

        END FUNCTION FindSeparatorsProtect
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE GetSubstring
        !^ **Function Interface**: GetSubstring <br>
        !  **Purpose**:  To get a substring (specified by the leftmost and rightmost
        !                indices) of the given character string. <br>
        !  **Usage**: <br>
        !   --->    Substring = GetSubstring(String, LeftIndx, RightIndx)
        MODULE FUNCTION GetSubstring(cStr,lPos,rPos) RESULT(cSub)
            !^ To return a substring specified by the *lPos* and *rPos* arguments
            !  of the given character string. <br>
            !  If the *lPos* argument is less than 1, then 1 is used as a
            !  starting position of the substring. <br>
            !  Similarly, if the *rPos* argument is greater than the length of
            !  the string, then the length is used as an ending position. <br>
            !  Also, If the *rPos* argument is less than the *lPos* argument,
            !  a zero-length string is returned.
            tCharStar, INTENT(IN)   :: cStr !! character string
            tIndex,    INTENT(IN)   :: lPos !! leftmost index
            tIndex,    INTENT(IN)   :: rPos !! rightmost index
            tCharAlloc              :: cSub !! substring
        END FUNCTION GetSubstring
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE GetSlice
        !^ **Function Interface**: GetSlice <br>
        !  **Purpose**:  To extract characters from the region between the first and
        !                last indices (both inclusive) of the given string by taking
        !                strides of length *Stride*. <br>
        !  **Usage**: <br>
        !   --->    Slice = GetSlice(String, FirstIndx, LastIndx, Stride)
        MODULE FUNCTION GetSlice(cStr,First,Last,Stride) RESULT(Slice)
            !^ To extract characters from the region between the first and last indices
            !  (both inclusive) of the given string by taking strides of length *Stride*.
            tCharStar,        INTENT(IN)    :: cStr     !! character string
            tIndex, OPTIONAL, INTENT(IN)    :: First
            !^ the first index; can be greater than the last index if *Stride* is negative.
            tIndex, OPTIONAL, INTENT(IN)    :: Last
            !^ the last index; can be less than the first index if *Stride* is negative.
            tIndex, OPTIONAL, INTENT(IN)    :: Stride
            !^ a step (length) between characters extracted; can be negative.
            tCharAlloc                      :: Slice    !! slice of the string
        END FUNCTION GetSlice
    END INTERFACE
    INTERFACE StartWith
        !^ **Function Interface**: StartWith <br>
        !  **Purpose**:  To check whether the given character string starts with the
        !                specified substring or not.  Both the string and the substring
        !                must not have a zero length. <br>
        !  **Usage**: <br>
        !   --->    Flag = StartWith(String, Substring) <br>
        !   --->    IF (.NOT.StartWith(String, Substring)) DoSomething
        MODULE FUNCTION StartWithSubstring(cStr,sStr) RESULT(Flag)
            !^ To check whether the given character string starts with the specified
            !  substring or not.  Both the string and the substring must not have a
            !  zero length.
            tCharStar, INTENT(IN)   :: cStr     !! character string
            tCharStar, INTENT(IN)   :: sStr     !! substring
            tLogical                :: Flag     !! true if the string starts with the substring
        END FUNCTION StartWithSubstring
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE EndWith
        !^ **Function Interface**: EndWith <br>
        !  **Purpose**:  To check whether the given character string ends with the
        !                specified substring or not.  Both the string and the substring
        !                must not have a zero length. <br>
        !  **Usage**: <br>
        !   --->    Flag = EndWith(String, Substring) <br>
        !   --->    IF (.NOT.EndWith(String, Substring)) DoSomething
        MODULE FUNCTION EndWithSubstring(cStr,sStr) RESULT(Flag)
            !^ To check whether the given character string ends with the specified
            !  substring or not.  Both the string and the substring must not have a
            !  zero length.
            tCharStar, INTENT(IN)   :: cStr     !! character string
            tCharStar, INTENT(IN)   :: sStr     !! substring
            tLogical                :: Flag     !! true if the string ends with the substring
        END FUNCTION EndWithSubstring
        ! ---------------------------------------------------------------------
    END INTERFACE
    !------------------------------------------------------------------
    !-----      Interfaces for 'Manipulation' Procedures          -----
    !------------------------------------------------------------------
    INTERFACE CropBlanks
        !^ **Function Interface**: CropBlanks <br>
        !  **Purpose**:  To remove leading and trailing blanks from the character string. <br>
        !  **Usage**: <br>
        !   --->    OutStr = CropBlanks(InStr) <br>
        !   --->    OutStr = CropBlanks(InStr, SpaceOnly=.TRUE.) <br>
        !  **Note**: CropBlanks(InStr,.TRUE.) is the same as TRIM(ADJUSTL(InStr)).
        MODULE FUNCTION CropBlanks(cStrIn,SpaceOnly) RESULT(cStrOut)
            !^ To remove leading and trailing blanks from the character string.
            tCharStar,          INTENT(IN)  :: cStrIn       !! input string
            tLogical, OPTIONAL, INTENT(IN)  :: SpaceOnly
            !^ flag indicating whether to only remove the space character or not. <br>
            ! - True if requesting to remove only the space character. <br>
            ! - False if requesting to remove both the tab and the space characters. <br>
            ! Default is false.
            tCharAlloc                      :: cStrOut      !! output string
        END FUNCTION CropBlanks
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE CompactString
        !^ **Function Interface**: CompactString <br>
        !  **Purpose**:  To convert multiple spaces and tabs into a single space,
        !                delete control characters and removes initial (leading
        !                and trailing) spaces. <br>
        !  **Usage**: <br>
        !   --->    OutStr = CompactString(InStr)
        MODULE FUNCTION CompactString(cStrIn) RESULT(cStrOut)
            !^ To convert multiple spaces and tabs into a single space, delete control
            !  characters and removes initial (leading and trailing) spaces.
            tCharStar, INTENT(IN)   :: cStrIn       !! input string
            tCharAlloc              :: cStrOut      !! output string
        END FUNCTION CompactString
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE CompressString
        !^ **Function Interface**: CompressString <br>
        !  **Purpose**:  To remove spaces, tabs and control characters from the
        !                character string. <br>
        !  **Usage**: <br>
        !   --->    OutStr = CompressString(InStr) <br>
        !  **Note**: Unlike the *CompactString* procedure, the output string
        !            contains no space between its characters.
        MODULE FUNCTION CompressString(cStrIn) RESULT(cStrOut)
            !^ To remove spaces, tabs and control characters from the character string.
            tCharStar, INTENT(IN)   :: cStrIn       !! input string
            tCharAlloc              :: cStrOut      !! output string
        END FUNCTION CompressString
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ChangeCase
        !^ **Subroutine Interface**: ChangeCase <br>
        !  **Purpose**:  To change case of all alphabet characters of the specified
        !                character string according to the given flag.  If protected
        !                regions are specified, only characters in unprotected regions
        !                are changed. <br>
        !  **Usage**: <br>
        !   ! change all alphabet characters to upper cases <br>
        !   --->    CALL ChangeCase(String, .TRUE.) <br>
        !   ! change all alphabet characters only in unprotected regions to lower cases <br>
        !   --->    CALL ChangeCase(String, nRegion, lPos, rPos, .FALSE.)
        MODULE ELEMENTAL SUBROUTINE ChangeCaseString(cStr,ToUpper)
            !^ To change case of all alphabet characters of the specified character
            !  string according to the given flag.
            tCharStar, INTENT(INOUT)    :: cStr     !! character string
            tLogical,  INTENT(IN)       :: ToUpper
            !^ flag indicating whether to change the string's characters to
            !  upper-case characters or not. <br>
            !  - If true, the string contains upper-case characters on exit. <br>
            !  - If false, the string contains lower-case characters on exit. <br>
        END SUBROUTINE ChangeCaseString
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ChangeCaseProtect(cStr,nRegion,lPos,rPos,ToUpper)
            !^ To change case of all alphabet characters of the specified character
            !  string in *unprotected regions* according to the given flag.
            tCharStar, INTENT(INOUT)    :: cStr     !! character string
            tIndex,    INTENT(IN)       :: nRegion  !! number of protected regions
            tIndex,    INTENT(IN)       :: lPos(:)  !! positions of the first character of protected regions 
            tIndex,    INTENT(IN)       :: rPos(:)  !! positions of the last character of protected regions 
            tLogical,  INTENT(IN)       :: ToUpper
            !^ flag indicating whether to change the string's characters to
            !  upper-case characters or not. <br>
            !  - If true, the string contains upper-case characters on exit. <br>
            !  - If false, the string contains lower-case characters on exit. <br>
        END SUBROUTINE ChangeCaseProtect
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE BlankCompressChangeCase
        !^ **Function Interface**: BlankCompressChangeCase <br>
        !  **Purpose**:  To first remove all blank characters and then change case of all
        !                alphabet characters of the specified character string according
        !                to the given flag. <br>
        !  **Usage**: <br>
        !   --->    OutStr = BlankCompressChangeCase(InStr, .TRUE.)
        MODULE FUNCTION BlankCompressChangeCase(cStrIn,ToUpper) RESULT(cStrOut)
            !^ To first remove all blank characters and then change case of all alphabet
            !  characters of the specified character string according to the given flag.
            tCharStar, INTENT(IN)   :: cStrIn   !! input string
            tLogical,  INTENT(IN)   :: ToUpper
            !^ flag indicating whether to change the string's characters to
            !  upper-case characters or not. <br>
            !  - If true, the string contains upper-case characters on exit. <br>
            !  - If false, the string contains lower-case characters on exit. <br>
            tCharAlloc              :: cStrOut  !! output string
        END FUNCTION BlankCompressChangeCase
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE InsertSubstring
        !^ **Function Interface**: InsertSubstring <br>
        !  **Purpose**:  To insert a given substring into the character string at a
        !                specified position. <br>
        !  **Usage**: <br>
        !   --->    OutStr = InsertSubstring(InStr, Pos, SubStr)
        MODULE FUNCTION InsertSubstring(cStrIn,Pos,sStr) RESULT(cStrOut)
            !^ To insert a given substring into the character string at a
            !  specified position. <br>
            !  If the *Pos* argument is less than 1, then 1 is used as an insertion point. <br>
            !  If the *Pos* argument is greater than length of the character string, then
            !  the substring is inserted at the end of the character string.
            tCharStar, INTENT(IN)   :: cStrIn   !! input string
            tIndex,    INTENT(IN)   :: Pos      !! the insertion point
            tCharStar, INTENT(IN)   :: sStr     !! substring
            tCharAlloc              :: cStrOut  !! output string
        END FUNCTION InsertSubstring
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE RemoveCharacters
        !^ **Function Interface**: RemoveCharacters <br>
        !  **Purpose**:  To remove characters from the character string depending on
        !                specified input. <br>
        !  **Usage**: <br>
        !   --->    OutStr = RemoveCharacters(InStr, CharSet) <br>
        !   --->    OutStr = RemoveCharacters(InStr, CharSet, Option=2)
        MODULE FUNCTION RemoveCharacters(cStrIn,ChrSet,Option) RESULT(cStrOut)
            !^ To remove characters from the character string depending on
            !  the specified character set and optionally the option flag. <br>
            !  The *ChrSet* argument is a required input that contains a set of
            !  characters to be removed whereas the *Option* argument is an
            !  optional input that indicates where characters in the character
            !  string to be removed are.  Available options include: <br>
            !  - Option = -1 --> nothing to be removed. <br>
            !  - Option =  0 --> leading (prefix) and trailing (suffix) character(s)
            !                    to be removed. <br>
            !  - Option =  1 --> leading (prefix) character(s) to be removed. <br>
            !  - Option =  2 --> trailing (suffix) character(s) to be removed. <br>
            !  - Option =  3 --> all characters in the *ChrSet* set to be removed. <br>
            ! If the *Option* argument is not present, the default option is set to 0.
            tCharStar,          INTENT(IN)  :: cStrIn   !! input string
            tCharStar,          INTENT(IN)  :: ChrSet   !! set of characters to be removed
            tSInt32,  OPTIONAL, INTENT(IN)  :: Option   !! flag indicating how to remove characters
            tCharAlloc                      :: cStrOut  !! output string
        END FUNCTION RemoveCharacters
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE RemoveCharactersProtect
        !^ **Function Interface**: RemoveCharactersProtect <br>
        !  **Purpose**:  To remove characters from the character string depending on
        !                specified input. <br>
        !  **Usage**: <br>
        !   --->    OutStr = RemoveCharactersProtect(InStr, CharSet) <br>
        !   --->    OutStr = RemoveCharactersProtect(InStr, CharSet, Option=2) <br>
        !   --->    OutStr = RemoveCharactersProtect(InStr, CharSet, ExclMrk=.FALSE.) <br>
        !   --->    OutStr = RemoveCharactersProtect(InStr, CharSet, 3, .FALSE.)
        MODULE FUNCTION RemoveCharactersProtect(cStrIn,ChrSet,Option,ExclMrk) RESULT(cStrOut)
            !^ To remove characters from the unprotected regions of the character string
            !  depending on the specified character set and optionally the option flag. <br>
            !  See the <a href="../module/mbase_chrstr.html#interface-removecharacters">
            !  RemoveCharacters</a> routine for explanations regarding the *ChrSet* and
            !  *Option* arguments. <br>
            !  See the <a href="../module/mbase_chrstr.html#interface-findprotectedregions">
            !  FindProtectedRegions</a> routine for explanations regarding the protected
            !  region(s).
            tCharStar,          INTENT(IN)  :: cStrIn   !! input string
            tCharStar,          INTENT(IN)  :: ChrSet   !! set of characters to be removed
            tSInt32,  OPTIONAL, INTENT(IN)  :: Option   !! flag indicating how to remove characters
            tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tCharAlloc                      :: cStrOut  !! output string
        END FUNCTION RemoveCharactersProtect
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE RemoveSubstring
        !^ **Function Interface**: RemoveSubstring <br>
        !  **Purpose**:  To remove the substring from the character string based on
        !                specified input. <br>
        !  **Usage**: <br>
        !   --->    OutStr = RemoveSubstring(InStr, SubStr) <br>
        !   --->    OutStr = RemoveSubstring(InStr, SubStr, FirstOnly=.TRUE.) <br>
        !   --->    OutStr = RemoveSubstring(InStr, sLen, sCount, sPos) <br>
        !   --->    OutStr = RemoveSubstring(InStr, lPos, rPos) <br>
        MODULE FUNCTION RemoveSubstring(cStrIn,sStr,FirstOnly) RESULT(cStrOut)
            !^ To remove the substring from the character string.
            tCharStar,          INTENT(IN)  :: cStrIn       !! input string
            tCharStar,          INTENT(IN)  :: sStr         !! substring to be removed
            tLogical, OPTIONAL, INTENT(IN)  :: FirstOnly
            !^ flag indicating whether to remove only the first substring found or to
            !  remove all occurrences found. <br>
            !  If true, only remove the first substring found; otherwise, remove all
            !  (non-overlapping) occurrences found. <br>
            !  Default is false.
            tCharAlloc                      :: cStrOut      !! output string
        END FUNCTION RemoveSubstring
        ! ---------------------------------------------------------------------
        RECURSIVE MODULE FUNCTION RemoveSubstringKnownPos(cStrIn,sLen,sCount,sPos) RESULT(cStrOut)
            !^ To remove substring from the character string by providing length, number of
            !  occurrences and position(s) of first character of the substring.
            tCharStar, INTENT(IN)   :: cStrIn       !! input string
            tIndex,    INTENT(IN)   :: sLen         !! length of the substring to be removed
            tIndex,    INTENT(IN)   :: sCount       !! number of occurrences of the substring
            tIndex,    INTENT(IN)   :: sPos(sCount) !! position(s) of first character of the substring
            tCharAlloc              :: cStrOut      !! output character string
        END FUNCTION RemoveSubstringKnownPos
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DeleteSubstring(cStrIn,lPos,rPos) RESULT(cStrOut)
            !^ To remove substring from the character string at specified positions. <br>
            !  If the *lPos* argument is less than 1, then 1 is used as a starting point
            !  of the substring. <br>
            !  Similarly, if the *rPos* argument is greater than length of the character
            !  string, then the length is used as a ending point. <br>
            !  If the *rPos* argument is less than the *lPos* argument, the original
            !  character string is returned.
            tCharStar, INTENT(IN)   :: cStrIn   !! input string
            tIndex,    INTENT(IN)   :: lPos     !! the leftmost character position of the substring
            tIndex,    INTENT(IN)   :: rPos     !! the rightmost character position of the substring
            tCharAlloc              :: cStrOut  !! output string
        END FUNCTION DeleteSubstring
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE RemoveSubstringProtect
        !^ **Function Interface**: RemoveSubstringProtect <br>
        !  **Purpose**:  To remove the substring from the unprotected regions of the
        !                character string based on specified input. <br>
        !  **Usage**: <br>
        !   --->    OutStr = RemoveSubstringProtect(InStr, SubStr) <br>
        !   --->    OutStr = RemoveSubstringProtect(InStr, SubStr, ExclMrk=.FALSE.) <br>
        !   --->    OutStr = RemoveSubstringProtect(InStr, SubStr, FirstOnly=.TRUE.) <br>
        !   --->    OutStr = RemoveSubstringProtect(InStr, SubStr, .FALSE., .TRUE.) <br>
        RECURSIVE MODULE FUNCTION RemoveSubstringProtect(cStrIn,sStr,ExclMrk,FirstOnly) RESULT(cStrOut)
            !^ To remove the substring from the unprotected regions of the character
            !  string based on specified input. <br>
            !  See the <a href="../module/mbase_chrstr.html#interface-findprotectedregions">
            !  FindProtectedRegions</a> routine for explanations regarding the protected
            !  region(s).
            tCharStar,          INTENT(IN)  :: cStrIn       !! input string
            tCharStar,          INTENT(IN)  :: sStr         !! substring to be removed
            tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tLogical, OPTIONAL, INTENT(IN)  :: FirstOnly
            !^ flag indicating whether to remove only the first substring found or to
            !  remove all occurrences found. <br>
            !  If true, only remove the first substring found; otherwise, remove all
            !  (non-overlapping) occurrences found. <br>
            !  Default is false.
            tCharAlloc                      :: cStrOut      !! output string
        END FUNCTION RemoveSubstringProtect
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ReplaceSubstring
        !^ **Function Interface**: ReplaceSubstring <br>
        !  **Purpose**:  To replace all occurrences of the original substring found
        !                in the given character string with the new substring based
        !                on specified input. <br>
        !  **Usage**: <br>
        !   --->    OutStr = ReplaceSubstring(InStr, OldSub, NewSub) <br>
        !   --->    OutStr = ReplaceSubstring(InStr, NewSub, OldLen, OldCount, OldPos)
        MODULE FUNCTION ReplaceSubstring(cStrIn,oStr,nStr) RESULT(cStrOut)
            !^ To replace all occurrences of the original substring found in the given
            !  character string with the new substring.
            tCharStar, INTENT(IN)   :: cStrIn   !! input string
            tCharStar, INTENT(IN)   :: oStr     !! original (old) substring
            tCharStar, INTENT(IN)   :: nStr     !! new substring
            tCharAlloc              :: cStrOut  !! output character string
        END FUNCTION ReplaceSubstring
        ! ---------------------------------------------------------------------
        RECURSIVE MODULE FUNCTION ReplaceSubstringKnownPos(cStrIn,nStr,oLen,oCount,oPos) RESULT(cStrOut)
            !^ To replace all occurrences of the original substring found in the given
            !  character string with the new substring by providing length, number of
            !  occurrences and position(s) of first character of original substring.
            tCharStar, INTENT(IN)   :: cStrIn       !! input string
            tCharStar, INTENT(IN)   :: nStr         !! new substring
            tIndex,    INTENT(IN)   :: oLen         !! length of original substring
            tIndex,    INTENT(IN)   :: oCount       !! number of occurrences of original substring
            tIndex,    INTENT(IN)   :: oPos(oCount) !! position(s) of first character of original substring
            tCharAlloc              :: cStrOut      !! output string
        END FUNCTION ReplaceSubstringKnownPos
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ReplaceSubstringProtect
        !^ **Function Interface**: ReplaceSubstringProtect <br>
        !  **Purpose**:  To replace all occurrences of the original substring found
        !                in unprotected regions of the given character string with
        !                the new substring. <br>
        !  **Usage**: <br>
        !   --->    OutStr = ReplaceSubstringProtect(InStr, OldSub, NewSub) <br>
        !   --->    OutStr = ReplaceSubstringProtect(InStr, OldSub, NewSub, ExclMrk=.FALSE.)
        RECURSIVE MODULE FUNCTION ReplaceSubstringProtect(cStrIn,oStr,nStr,ExclMrk) RESULT(cStrOut)
            !^ To replace all occurrences of the original substring found in unprotected
            !  regions of the given character string with the new substring. <br>
            !  See the <a href="../module/mbase_chrstr.html#interface-findprotectedregions">
            !  FindProtectedRegions</a> routine for explanations regarding the protected
            !  region(s).
            tCharStar,          INTENT(IN)  :: cStrIn   !! input string
            tCharStar,          INTENT(IN)  :: oStr     !! original (old) substring
            tCharStar,          INTENT(IN)  :: nStr     !! new substring
            tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tCharAlloc                      :: cStrOut  !! output character string
        END FUNCTION ReplaceSubstringProtect
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ReplaceSubstringRecursive
        !^ **Function Interface**: ReplaceSubstringRecursive <br>
        !  **Purpose**:  To replace all occurrences of the original substring found
        !                in the given character string with the new substring in
        !                a recursive way. <br>
        !  **Usage**: <br>
        !   --->    OutStr = ReplaceSubstringRecursive(InStr, OldSub, NewSub) <br>
        !  **Example**: <br>
        !   *Input*: cStrIn = 'abbbbb', oStr = 'ab', nStr = 'a' <br>
        !   *Output* - non-recursive: cStrOut = 'abbbb' <br>
        !   *Output* - recursive: cStrOut = 'ab'
        MODULE FUNCTION ReplaceSubstringRecursive(cStrIn,oStr,nStr) RESULT(cStrOut)
            !^ To replace all occurrences of the original substring found in the given
            !  character string with the new substring in a recursive way.
            tCharStar, INTENT(IN)   :: cStrIn   !! input string
            tCharStar, INTENT(IN)   :: oStr     !! original (old) substring
            tCharStar, INTENT(IN)   :: nStr     !! new substring
            tCharAlloc              :: cStrOut  !! output string
        END FUNCTION ReplaceSubstringRecursive
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE Partition
        !^ **Subroutine Interface**: Partition <br>
        !  **Purpose**:  To partition a character string into two substrings by a
        !                separator (single character or a multiple-character string).
        !                The partition occurs at the first occurrence of the separator
        !                found. <br>
        !  **Usage**: <br>
        !   ! *SepSub* is a multiple-character string separator. <br>
        !   --->    CALL Partition(cStr, SepSub, bStr, aStr) <br>
        !   ! *SepSet* is a set of characters where a character in the set is a valid <br>
        !   ! separator.  Also, search the first separator from the back. <br>
        !   --->    CALL Partition(cStr, SepSet, bStr, aStr, SepChr, Back=.TRUE.) <br>
        MODULE SUBROUTINE PartitionSepSub(cStr,SepSub,bStr,aStr,Back)
            !^ To partition a character string into two substrings where the specified
            !  separator is a multiple-character string.  The partition occurs at the 
            !  first occurrence of the separator found.
            tCharStar,          INTENT(IN)  :: cStr     !! character string
            tCharStar,          INTENT(IN)  :: SepSub   !! multiple-character separator
            tCharAlloc,         INTENT(OUT) :: bStr     !! substring before the separator found
            tCharAlloc,         INTENT(OUT) :: aStr     !! substring after the separator found
            tLogical, OPTIONAL, INTENT(IN)  :: Back
            !^ flag indicating whether to search the separator from the back or not. <br>
            !  - True: search the separator from the back. <br>
            !  - False: search the separator from the front. <br>
            !  Default is false.
        END SUBROUTINE PartitionSepSub
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE PartitionSepChr(cStr,SepSet,bStr,aStr,SepChr,Back)
            !^ To partition a character string into two substrings where the separator
            !  is a single character (any character in the specified set of characters).
            !  The partition occurs at the first occurrence of the separator found.
            tCharStar,          INTENT(IN)  :: cStr     !! character string
            tCharStar,          INTENT(IN)  :: SepSet   !! set of characters representing valid separators
            tCharAlloc,         INTENT(OUT) :: bStr     !! substring before the separator found
            tCharAlloc,         INTENT(OUT) :: aStr     !! substring after the separator found
            tChar,              INTENT(OUT) :: SepChr   !! the separator found
            tLogical, OPTIONAL, INTENT(IN)  :: Back
            !^ flag indicating whether to search the separator from the back or not. <br>
            !  - True: search the separator from the back. <br>
            !  - False: search the separator from the front. <br>
            !  Default is false.
        END SUBROUTINE PartitionSepChr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE Split
        !^ **Subroutine Interface**: Split <br>
        !  **Purpose**:  To split a character string into multiple substrings by a
        !                separator (single character or a multiple-character string). <br>
        !  **Usage**: <br>
        !   ! *SepSub* is a multiple-character string separator. <br>
        !   --->    CALL Split(cStr, SepSub, qStr) <br>
        !   ! *SepSet* is a set of characters where a character in the set is a valid separator. <br>
        !   --->    CALL Split(cStr, SepSet, qStr, SepChr) <br>
        MODULE SUBROUTINE SplitSepSub(cStr,SepSub,qStr)
            !^ To split a character string into multiple substrings where the specified
            !  separator is a multiple-character string.
            tCharStar,       INTENT(IN)     :: cStr     !! character string
            tCharStar,       INTENT(IN)     :: SepSub   !! multiple-character separator
            TYPE(QueueChar), INTENT(OUT)    :: qStr     !! queue of substrings
        END SUBROUTINE SplitSepSub
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE SplitSepChr(cStr,SepSet,qStr,SepChr)
            !^ To split a character string into multiple substrings where the separator
            !  is a single character (any character in the specified set of characters).
            tCharStar,          INTENT(IN)  :: cStr         !! character string
            tCharStar,          INTENT(IN)  :: SepSet       !! set of characters representing valid separators
            TYPE(QueueChar),    INTENT(OUT) :: qStr         !! queue of substrings
            tChar, ALLOCATABLE, INTENT(OUT) :: SepChr(:)    !! separators found
        END SUBROUTINE SplitSepChr
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE SplitProtect
        !^ **Subroutine Interface**: SplitProtect <br>
        !  **Purpose**:  To split a character string into multiple substrings by a
        !                separator (single character or a multiple-character string)
        !                found in unprotected region(s). <br>
        !  **Usage**: <br>
        !   ! *SepSub* is a multiple-character string separator. <br>
        !   --->    CALL SplitProtect(cStr, SepSub, qStr) <br>
        !   ! *SepSet* is a set of characters where a character in the set is a valid separator. <br>
        !   --->    CALL SplitProtect(cStr, SepSet, qStr, SepChr) <br>
        !   ! The exclamation mark is NOT used to define the protected regions. <br>
        !   --->    CALL SplitProtect(cStr, SepSub, qStr, ExclMrk=.FALSE.) <br>
        MODULE SUBROUTINE SplitSepSubProtect(cStr,SepSub,qStr,ExclMrk)
            !^ To split a character string into multiple substrings where the specified
            !  separator is a multiple-character string found in unprotected region(s).
            tCharStar,          INTENT(IN)  :: cStr     !! character string
            tCharStar,          INTENT(IN)  :: SepSub   !! multiple-character separator
            TYPE(QueueChar),    INTENT(OUT) :: qStr     !! queue of substrings
            tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
        END SUBROUTINE SplitSepSubProtect
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE SplitSepChrProtect(cStr,SepSet,qStr,SepChr,ExclMrk)
            !^ To split a character string into multiple substrings where the separator
            !  is a single character (any character in the specified set of characters)
            !  found in unprotected region(s).
            tCharStar,          INTENT(IN)  :: cStr         !! character string
            tCharStar,          INTENT(IN)  :: SepSet       !! set of characters representing valid separators
            TYPE(QueueChar),    INTENT(OUT) :: qStr         !! queue of substrings
            tChar, ALLOCATABLE, INTENT(OUT) :: SepChr(:)    !! separators found
            tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
        END SUBROUTINE SplitSepChrProtect
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! Private Interface
    INTERFACE
        MODULE FUNCTION ReplaceProtectedRegionsWithValidCharacter(cStrIn,ChrSet,nRegion,lPos,rPos) RESULT(cStrOut)
            tCharStar, INTENT(IN)   :: cStrIn
            tCharStar, INTENT(IN)   :: ChrSet
            tIndex,    INTENT(IN)   :: nRegion
            tIndex,    INTENT(IN)   :: lPos(nRegion)
            tIndex,    INTENT(IN)   :: rPos(nRegion)
            tCharAlloc              :: cStrOut
        END FUNCTION ReplaceProtectedRegionsWithValidCharacter
        ! ---------------------------------------------------------------------
    END INTERFACE

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

END MODULE MBase_ChrStr

!******************************************************************************
