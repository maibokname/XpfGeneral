
MODULE MClass_FvlStr

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *FvlStr* type and its related routines.  The *FvlStr*
!   type represents a variable-length string class that provides routines to handle
!   and manipulate strings.  Functionally, it is similar to the Fortran's *CHARACTER*
!   type.  However, when declared as an array, elements of the array of the *FvlStr*
!   type do not have to have the same length whereas those of the Fortran intrinsic
!   type must. <br>
!   The procedures provided by the *FvlStr* type are similar to those provided in
!   the <a href="../module/mbase_chrstr.html">MBase_ChrStr</a> module.  However,
!   the *FvlStr* type employs an object-oriented interface instead of a procedural
!   interface; thus, these procedures are type-bound.  In addition, procedures
!   similar to the Fortran intrinsic procedures (for the *CHARACTER* type) are also
!   provided in a procedural interface (i.e. the same as the intrinsic ones). <br>
!   It should be noted that the *FvlStr* type is a subtype of the *Hashable* type.
!   Therefore, all operations provided for the *Object*, the *Comparable* or
!   the *Hashable* types, such as sorting and containers, are also applicable to
!   the *FvlStr* type. <br>

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#define     tHash           tIndex

!** USE STATEMENTS:
    USE ISO_C_BINDING
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MClass_Object,          ONLY: Object
    USE MClass_Comparable,      ONLY: Comparable
#ifdef Indx32Bits
    USE MBase_OptimalHash32,    ONLY: ComputeHash => Murmur3_Hash32_Opt
#else
    USE MBase_OptimalHash64,    ONLY: ComputeHash => XX_Hash64_Opt
#endif

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: FvlStr
    ! assignment and operators
    PUBLIC :: ASSIGNMENT(=), OPERATOR(+),  OPERATOR(//)
    PUBLIC :: OPERATOR(==),  OPERATOR(/=), OPERATOR(>)
    PUBLIC :: OPERATOR(>=),  OPERATOR(<),  OPERATOR(<=)
    PUBLIC :: GETLEN,   LEN_TRIM,   ACHAR,      CHAR
    ! string intrinsics
    PUBLIC :: IACHAR,   ICHAR,      ADJUSTL,    ADJUSTR
    PUBLIC :: INDEX,    REPEAT,     SCAN,       TRIM
    PUBLIC :: VERIFY,   LGT,        LGE,        LLT,       LLE
    ! user-defined input/output statements
    PUBLIC :: WRITE(UNFORMATTED),   READ(UNFORMATTED)
    PUBLIC :: WRITE(FORMATTED),     READ(FORMATTED)
    ! miscellaneous procedures
    PUBLIC :: Swap, ToCharStar, PtrToStr, IsReady

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_FvlStr'
    ! seed for computation of hash code
#ifdef Indx32Bits
    tSInt32,   PARAMETER    :: HashSeed = 313131_kInt32
#else
    tSInt64,   PARAMETER    :: HashSeed = 313131_kInt64
#endif

!** DERIVED TYPE DEFINITIONS
    !> *FvlStr* is a string type that represents a variable-length string class.
    !  It provides various methods/procedures to handle and manipulate strings.
    TYPE, EXTENDS(Comparable)   :: FvlStr
        PRIVATE
        tCharAlloc   :: cStr ! character string
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Conversion Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: ToCharString <br>
        !  **Purpose**:  To convert a FvlStr object to an allocatable character
        !       string. <br>
        !  **Usage**: <br>
        !   --->    cStr = vStr%ToCharString() <br>
        PROCEDURE   :: ToCharString     => CharAlloc_From_FvlStr
        !> **Type-Bound Function**: ToChrArrAlloc <br>
        !  **Purpose**:  To convert a FvlStr object to an allocatable array
        !       of characters. <br>
        !  **Usage**: <br>
        !   ! convert a FvlStr object to a character array <br>
        !   --->    cArray = vStr%ToChrArrAlloc() <br>
        !   ! convert a FvlStr object to a character array with a null character <br>
        !   --->    cArray = vStr%ToChrArrAlloc(IsCString=.TRUE.) <br>
        PROCEDURE   :: ToChrArrAlloc    => CharArray_From_FvlStr
        !> **Type-Bound Function**: ParseByte <br>
        !  **Purpose**:  To parse the string of a FvlStr object as an 8-bit integer number. <br>
        !  **Usage**: <br>
        !   --->    NumVal = vStr%ParseByte() <br>
        !   --->    NumVal = vStr%ParseByte(ErrFlag, ErrMsg) <br>
        PROCEDURE   :: ParseByte        => IByte_From_FvlStr
        !> **Type-Bound Function**: ParseShort <br>
        !  **Purpose**:  To parse the string of a FvlStr object as a 16-bit integer number. <br>
        !  **Usage**: <br>
        !   --->    NumVal = vStr%ParseShort() <br>
        !   --->    NumVal = vStr%ParseShort(ErrFlag, ErrMsg) <br>
        PROCEDURE   :: ParseShort       => IShort_From_FvlStr
        !> **Type-Bound Function**: ParseInteger <br>
        !  **Purpose**:  To parse the string of a FvlStr object as a 32-bit integer number. <br>
        !  **Usage**: <br>
        !   --->    NumVal = vStr%ParseInteger() <br>
        !   --->    NumVal = vStr%ParseInteger(ErrFlag, ErrMsg) <br>
        PROCEDURE   :: ParseInteger     => Integer_From_FvlStr
        !> **Type-Bound Function**: ParseLong <br>
        !  **Purpose**:  To parse the string of a FvlStr object as a 8-bit integer number. <br>
        !  **Usage**: <br>
        !   --->    NumVal = vStr%ParseLong() <br>
        !   --->    NumVal = vStr%ParseLong(ErrFlag, ErrMsg) <br>
        PROCEDURE   :: ParseLong        => ILong_From_FvlStr
        !> **Type-Bound Function**: ParseRSingle <br>
        !  **Purpose**:  To parse the string of a FvlStr object as a single-precision real number. <br>
        !  **Usage**: <br>
        !   --->    NumVal = vStr%ParseRSingle() <br>
        !   --->    NumVal = vStr%ParseRSingle(ErrFlag, ErrMsg) <br>
        PROCEDURE   :: ParseRSingle     => RSingle_From_FvlStr
        !> **Type-Bound Function**: ParseRDouble <br>
        !  **Purpose**:  To parse the string of a FvlStr object as a double-precision real number. <br>
        !  **Usage**: <br>
        !   --->    NumVal = vStr%ParseRDouble() <br>
        !   --->    NumVal = vStr%ParseRDouble(ErrFlag, ErrMsg) <br>
        PROCEDURE   :: ParseRDouble     => RDouble_From_FvlStr
        !> **Type-Bound Function**: ParseRQuad <br>
        !  **Purpose**:  To parse the string of a FvlStr object as a quadruple-precision real number. <br>
        !  **Usage**: <br>
        !   --->    NumVal = vStr%ParseRQuad() <br>
        !   --->    NumVal = vStr%ParseRQuad(ErrFlag, ErrMsg) <br>
        PROCEDURE   :: ParseRQuad       => RQuad_From_FvlStr
        !> **Type-Bound Function**: ParseCSingle <br>
        !  **Purpose**:  To parse the string of a FvlStr object as a single-precision complex number. <br>
        !  **Usage**: <br>
        !   --->    NumVal = vStr%ParseCSingle() <br>
        !   --->    NumVal = vStr%ParseCSingle(ErrFlag, ErrMsg) <br>
        !  **Note**: A valid string representing a complex number consists of a pair of
        !            real-number (or integer-number) strings, separated by a comma, and
        !            enclosed in parentheses.
        PROCEDURE   :: ParseCSingle     => CSingle_From_FvlStr
        !> **Type-Bound Function**: ParseCDouble <br>
        !  **Purpose**:  To parse the string of a FvlStr object as a double-precision complex number. <br>
        !  **Usage**: <br>
        !   --->    NumVal = vStr%ParseCDouble() <br>
        !   --->    NumVal = vStr%ParseCDouble(ErrFlag, ErrMsg) <br>
        !  **Note**: A valid string representing a complex number consists of a pair of
        !            real-number (or integer-number) strings, separated by a comma, and
        !            enclosed in parentheses.
        PROCEDURE   :: ParseCDouble     => CDouble_From_FvlStr
        !> **Type-Bound Function**: ParseCQuad <br>
        !  **Purpose**:  To parse the string of a FvlStr object as a quadruple-precision complex number. <br>
        !  **Usage**: <br>
        !   --->    NumVal = vStr%ParseCQuad() <br>
        !   --->    NumVal = vStr%ParseCQuad(ErrFlag, ErrMsg) <br>
        !  **Note**: A valid string representing a complex number consists of a pair of
        !            real-number (or integer-number) strings, separated by a comma, and
        !            enclosed in parentheses.
        PROCEDURE   :: ParseCQuad       => CQuad_From_FvlStr
        !> **Type-Bound Function**: ParseLogical<br>
        !  **Purpose**:  To parse the string of a FvlStr object as a logical value. <br>
        !  **Usage**: <br>
        !   --->    LogVal = vStr%ParseLogical() <br>
        PROCEDURE   :: ParseLogical     => Logical_From_FvlStr
        ! ---------------------------------------------------------------------
        ! -----                 Getter Procedures                         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Length <br>
        !  **Purpose**:  To return the length of the character string of a FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    StrLen = vStr%Length() <br>
        PROCEDURE   :: Length           => GetLength
        !> **Type-Bound Function**: cSubStr <br>
        !  **Purpose**:  To return a substring from FvlStr object based on the specified lPos
        !       and rPos where the returned substring is an allocatable character string. <br>
        !  **Usage**: <br>
        !   --->    SubStr = vStr%cSubStr(lPos, rPos) <br>
        !   Note: the above example is functionally equivalent to the following Fortran
        !         intrinsic statement: <br>
        !           SubStr = cStr(lPos:rPos)
        PROCEDURE   :: cSubStr          => GetSubstring_CHS
        !> **Type-Bound Function**: vSubStr <br>
        !  **Purpose**:  To return a substring from FvlStr object based on the specified lPos
        !       and rPos where the returned substring is another FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    SubStr = vStr%vSubStr(lPos, rPos) <br>
        PROCEDURE   :: vSubStr          => GetSubstring_VLS
        !> **Type-Bound Function**: cSlice <br>
        !  **Purpose**:  To extract the characters from the region between *first* and *last*
        !       indices (both inclusive) of a FvlStr object by taking strides of length *stride*.
        !       The returned character slice is an allocatable character string. <br>
        !  **Usage**: <br>
        !   --->    Slice = vStr%cSlice(first, last, stride) <br>
        !   Note: the above example is functionally equivalent to the following Fortran
        !         intrinsic statement: <br>
        !           Slice = cStr(first:last:stride)
        PROCEDURE   :: cSlice           => GetSlice_CHS
        !> **Type-Bound Function**: vSlice <br>
        !  **Purpose**:  To extract the characters from the region between *First* and *Last*
        !       indices (both inclusive) of a FvlStr object by taking strides of length *Stride*.
        !       The returned character slice is another FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    Slice = vStr%vSlice(First, Last, Stride) <br>
        PROCEDURE   :: vSlice           => GetSlice_VLS
        !> **Type-Bound Function**: Char <br>
        !  **Purpose**:  To return a (single) character from a FvlStr object based on
        !       the specified position. <br>
        !  **Usage**: <br>
        !   --->    Chr = vStr%Char(Pos) <br>
        PROCEDURE   :: Char             => GetCharacter
        ! ---------------------------------------------------------------------
        ! -----                 Inquiry Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: CountSubstring_CHS
        PROCEDURE, PRIVATE  :: CountSubstring_VLS
        PROCEDURE, PRIVATE  :: CountCharacters_CHS
        PROCEDURE, PRIVATE  :: CountCharacters_VLS
        PROCEDURE, PRIVATE  :: FindSubstring_CHS
        PROCEDURE, PRIVATE  :: FindSubstring_VLS
        PROCEDURE, PRIVATE  :: FindDelimiters_CHS
        PROCEDURE, PRIVATE  :: FindDelimiters_VLS
        PROCEDURE, PRIVATE  :: FindSeparators_CHS
        PROCEDURE, PRIVATE  :: FindSeparators_VLS
        PROCEDURE, PRIVATE  :: StartWith_CHS
        PROCEDURE, PRIVATE  :: StartWith_VLS
        PROCEDURE, PRIVATE  :: EndWith_CHS
        PROCEDURE, PRIVATE  :: EndWith_VLS
        !> **Type-Bound Function**: IsNumber <br>
        !  **Purpose**:  To return a flag indicating whether the string of a FvlStr object
        !       is a valid number and if so, what kind of number it is. <br>
        !       If the returned flag is -1, the string is NOT a number. <br>
        !       If the returned flag is  0, the string is a valid integer or real number. <br>
        !       If the returned flag is  1, the string is strictly an integer number. <br>
        !       If the returned flag is  2, the string is strictly a real number. <br>
        !       If the returned flag is  3, the string is a valid complex number. <br>
        !  **Usage**: <br>
        !   ! return -1, 0 or 3 <br>
        !   --->    Flag = vStr%IsNumber() <br>
        !   ! return -1, 1, 2 or 3 <br>
        !   --->    Flag = vStr%IsNumber(Strict=.TRUE.) <br>
        !   ! return a flag (-1, 0 or 3) and value of the string if it is a number <br>
        !   --->    Flag = vStr%IsNumber(NumVal=Value) <br>
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
        PROCEDURE   :: IsNumber         => Is_FvlStr_Number
        !> **Type-Bound Function**: IsLogical <br>
        !  **Purpose**:  To return true value if the string of a FvlStr object is a logical
        !       value where valid logical values include 'T', 'F', 't', 'f', 'TRUE', 'FALSE',
        !       'true', 'false'.  Otherwise, return false value. <br>
        !  **Usage**: <br>
        !   ! only return the flag <br>
        !   --->    Flag = vStr%IsLogical() <br>
        !   ! return the flag and also get logical value if the returned flag is true <br>
        !   --->    Flag = vStr%IsLogical(LogVal) <br>
        PROCEDURE   :: IsLogical        => Is_FvlStr_Logical
        !> **Type-Bound Function**: IsInClass <br>
        !  **Purpose**:  To check whether a FvlStr object is in the specified class where
        !       the recognized classes include ALPHABET, ALPHANUM, ASCII, BLANK, COMPLEX,
        !       CONTROL, DIGIT, FNAME, GRAPHICAL, INTEGER, LOGICAL, LOWERCASE, PUNCTUATION,
        !       PRINTABLE, REAL, UPPERCASE, WHITESPACE, HEXDIGIT, OCTDIGIT. <br>
        !  **Usage**: <br>
        !   --->    Flag = vStr%IsInClass('ALPHANUM') <br>
        !   --->    Flag = vStr%IsInClass('GRAPHICAL', FailIndex) <br>
        !   --->    IF (.NOT.vStr%IsInClass('FNAME')) DoSomething <br>
        !  **Technical Notes**:  See explanations of various recognized classes in the
        !       <a href="../module/mbase_chrstr.html#interface-ischaracterinclass">
        !       IsCharacterInClass</a> function interface and the
        !       <a href="../module/mbase_chrstr.html#interface-isstringinclass">
        !       IsStringInClass</a> function interface of the
        !       <a href="../module/mbase_chrstr.html">MBase_ChrStr</a> module. <br>
        PROCEDURE   :: IsInClass        => Is_FvlStr_InClass
        !> **Function Interface**: CountSubstring <br>
        !  **Purpose**:  To count the number of occurrences of the given substring
        !       in the specified FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    Count = vStr%CountSubstring(SubStr) <br>
        !   --->    Count = vStr%CountSubstring(SubStr, Overlap=.TRUE.) <br>
        GENERIC     :: CountSubstring   => CountSubstring_CHS, CountSubstring_VLS
        !> **Function Interface**: CountCharacters <br>
        !  **Purpose**:  To count the number of occurrences of character(s) in the
        !       specified FvlStr object for any character appearing in the given
        !       character set.  Optionally, a user can specify flags indicating
        !       whether protected regions exist or not and whether the exclamation
        !       mark is used to protect regions. <br>
        !  **Usage**: <br>
        !   --->    Count = vStr%CountCharacters(CharSet) <br>
        !   --->    Count = vStr%CountCharacters(CharSet, Protect=.TRUE.) <br>
        !   --->    Count = vStr%CountCharacters(CharSet, Protect=.TRUE., ExclMrk=.FALSE.) <br>
        GENERIC     :: CountCharacters  => CountCharacters_CHS, CountCharacters_VLS
        !> **Function Interface**: CountWords <br>
        !  **Purpose**:  To count the number of words (separated by blanks) in the
        !       specified FvlStr object where blanks are characters in the
        !       <a href="../module/mbase_chrstr.html#variable-set_blanks">
        !       SET_BLANKS</a> character set. <br>
        !  **Usage**: <br>
        !   --->    Count = vStr%CountWords() <br>
        PROCEDURE   :: CountWords       => CountWords_VLS
        !> **Function Interface**: FindProtectedRegions <br>
        !  **Purpose**:  To find the number of protected regions marked by two (single
        !       or double) quotes and/or by an exclamation mark.  Also, return positions
        !       of the first and last characters of each region. <br>
        !  **Usage**: <br>
        !   --->    nRegion = vStr%FindProtectedRegions(lPos, rPos) <br>
        !   --->    nRegion = vStr%FindProtectedRegions(lPos, rPos, ExclMrk=.FALSE.) <br>
        PROCEDURE   :: FindProtectedRegions => FindProtectedRegions_VLS
        !> **Function Interface**: FindSubstring <br>
        !  **Purpose**:  To count the number of non-overlapping occurrences of the given
        !       substring in the specified FvlStr object and also return position(s) of
        !       the first character of substring found.  Optionally, a user can specify
        !       flags indicating whether protected regions exist or not and whether the
        !       exclamation mark is used to protect regions. <br>
        !  **Usage**: <br>
        !   --->    Count = vStr%FindSubstring(SubStr, FirstPos) <br>
        !   --->    Count = vStr%FindSubstring(SubStr, FirstPos, Protect=.TRUE.) <br>
        !   --->    Count = vStr%FindSubstring(SubStr, FirstPos, Protect=.TRUE., ExclMrk=.FALSE.) <br>
        GENERIC     :: FindSubstring    => FindSubstring_CHS, FindSubstring_VLS
        !> **Function Interface**: FindDelimiters <br>
        !  **Purpose**:  To count the number of occurrences of delimiter(s) in the
        !       specified FvlStr object and also return position(s) of the delimiter(s)
        !       found.  A delimiter is any character appearing in the given character
        !       set.  Optionally, a user can specify flags indicating whether protected
        !       regions exist or not and whether the exclamation mark is used to protect
        !       regions. <br>
        !  **Usage**: <br>
        !   --->    Count = vStr%FindDelimiters(CharSet, DPos) <br>
        !   --->    Count = vStr%FindDelimiters(CharSet, DPos, Protect=.TRUE.) <br>
        !   --->    Count = vStr%FindDelimiters(CharSet, DPos, Protect=.TRUE., ExclMrk=.FALSE.) <br>
        GENERIC     :: FindDelimiters   => FindDelimiters_CHS, FindDelimiters_VLS
        !> **Function Interface**: FindSeparators <br>
        !  **Purpose**:  To count the number of occurrences of separator(s) in the
        !       specified FvlStr object and also return (the first) position(s) of the
        !       separator(s) found.  Optionally, a user can specify flags indicating
        !       whether protected regions exist or not and whether the exclamation
        !       mark is used to protect regions. <br>
        !  **Usage**: <br>
        !   ! a separator is any (single) character in the *Separator* argument <br>
        !   --->    Count = vStr%FindSeparators(Separator, .TRUE., Pos) <br>
        !   ! a separator is a character string specified by the *Separator* argument <br>
        !   --->    Count = vStr%FindSeparators(Separator, .FALSE., Pos) <br>
        !   ! both quotes and an exclamation mark used to define protected regions. <br>
        !   --->    Count = vStr%FindSeparatorsProtect(Separator, .TRUE., Pos, Protect=.TRUE.)  ! separator is a single character <br>
        !   --->    Count = vStr%FindSeparatorsProtect(Separator, .FALSE., Pos, Protect=.TRUE.) ! separator is a character string <br>
        !   ! only quotes used to define protected regions.  <br>
        !   --->    Count = vStr%FindSeparatorsProtect(Separator, .TRUE., Pos, Protect=.TRUE., ExclMrk=.FALSE.) <br>
        !   --->    Count = vStr%FindSeparatorsProtect(Separator, .FALSE., Pos, Protect=.TRUE., ExclMrk=.FALSE.) <br>
        GENERIC     :: FindSeparators   => FindSeparators_CHS, FindSeparators_VLS
        !> **Function Interface**: StartWith <br>
        !  **Purpose**:  To check whether the string of a FvlStr object starts with the
        !       specified substring or not.  Both the string and the substring must not
        !       have a zero length. <br>
        !  **Usage**: <br>
        !   --->    Flag = vStr%StartWith(SubStr) <br>
        !   --->    IF (.NOT.vStr%StartWith(SubStr)) DoSomething
        GENERIC     :: StartWith        => StartWith_CHS, StartWith_VLS
        !> **Function Interface**: EndWith <br>
        !  **Purpose**:  To check whether the string of a FvlStr object ends with the
        !       specified substring or not.  Both the string and the substring must not
        !       have a zero length. <br>
        !  **Usage**: <br>
        !   --->    Flag = vStr%EndWith(SubStr) <br>
        !   --->    IF (.NOT.vStr%EndWith(SubStr)) DoSomething
        GENERIC     :: EndWith          => EndWith_CHS, EndWith_VLS
        ! ---------------------------------------------------------------------
        ! -----             Manipulation Procedures                       -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: AlterCase
        PROCEDURE, PRIVATE  :: AlterCaseProtect
        PROCEDURE, PRIVATE  :: InsertSubstring_CHS
        PROCEDURE, PRIVATE  :: InsertSubstring_VLS
        PROCEDURE, PRIVATE  :: RemoveCharacters_CHS
        PROCEDURE, PRIVATE  :: RemoveCharacters_VLS
        PROCEDURE, PRIVATE  :: RemoveSubstring_CHS
        PROCEDURE, PRIVATE  :: RemoveSubstring_VLS
        PROCEDURE, PRIVATE  :: Delete_Substring
        PROCEDURE, PRIVATE  :: ReplaceSubstring_CHS_CHS
        PROCEDURE, PRIVATE  :: ReplaceSubstring_VLS_CHS
        PROCEDURE, PRIVATE  :: ReplaceSubstring_CHS_VLS
        PROCEDURE, PRIVATE  :: ReplaceSubstring_VLS_VLS
        PROCEDURE, PRIVATE  :: PartitionSepSub_CHS
        PROCEDURE, PRIVATE  :: PartitionSepSub_VLS
        PROCEDURE, PRIVATE  :: PartitionSepChr_CHS
        PROCEDURE, PRIVATE  :: PartitionSepChr_VLS
        PROCEDURE, PRIVATE  :: SplitSepSub_CHS
        PROCEDURE, PRIVATE  :: SplitSepSub_VLS
        PROCEDURE, PRIVATE  :: SplitSepChr_CHS
        PROCEDURE, PRIVATE  :: SplitSepChr_VLS
        !> **Subroutine Interface**: ChangeCase <br>
        !  **Purpose**:  To change case of all alphabet characters of the string of the
        !       specified FvlStr object according to the given flag.  If protected regions
        !       are specified, only characters in unprotected regions are changed. <br>
        !  **Usage**: <br>
        !   ! change all alphabet characters to upper cases <br>
        !   --->    CALL vStr%ChangeCase(.TRUE.) <br>
        !   ! change all alphabet characters only in unprotected regions to lower cases <br>
        !   --->    CALL vStr%ChangeCase(nRegion, lPos, rPos, .FALSE.)
        GENERIC     :: ChangeCase       => AlterCase, AlterCaseProtect
        !> **Subroutine Interface**: ChangeCaseBC <br>
        !  **Purpose**:  To first remove all blank characters and then change case of all
        !       alphabet characters of the string of the specified FvlStr object according
        !       to the given flag. <br>
        !  **Usage**: <br>
        !   --->    CALL vStr%ChangeCaseBC(.TRUE.)
        PROCEDURE   :: ChangeCaseBC     => BlankCompressAlterCase
        !> **Subroutine Interface**: CropBlanks <br>
        !  **Purpose**:  To remove leading and trailing blanks from the string of the
        !       specified FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    CALL vStr%CropBlanks() <br>
        !   --->    CALL vStr%CropBlanks(SpaceOnly=.TRUE.) <br>
        PROCEDURE   :: CropBlanks       => CropBlanks_VLS
        !> **Subroutine Interface**: Compact <br>
        !  **Purpose**:  To convert multiple spaces and tabs into a single space, delete
        !       control characters and removes initial (leading and trailing) spaces. <br>
        !  **Usage**: <br>
        !   --->    CALL vStr%Compact() <br>
        PROCEDURE   :: Compact          => CompactString_VLS
        !> **Subroutine Interface**: Compress <br>
        !  **Purpose**:  To remove spaces, tabs and control characters from the string
        !       of the specified FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    CALL vStr%Compress() <br>
        !  **Note**: Unlike the *Compact* method, the returned string contains no space
        !            between its characters.
        PROCEDURE   :: Compress         => CompressString_VLS
        !> **Subroutine Interface**: InsertSubstring <br>
        !  **Purpose**:  To insert a given substring into the string of the given
        !       FvlStr object at a specified position. <br>
        !  **Usage**: <br>
        !   --->    CALL vStr%InsertSubstring(Pos, SubStr)
        GENERIC     :: InsertSubstring  => InsertSubstring_CHS, InsertSubstring_VLS
        !> **Subroutine Interface**: RemoveCharacters <br>
        !  **Purpose**:  To remove characters from the string of the given FvlStr
        !       object depending on specified input. <br>
        !  **Usage**: <br>
        !   --->    CALL vStr%RemoveCharacters(CharSet) <br>
        !   --->    CALL vStr%RemoveCharacters(CharSet, Option=2) <br>
        !   --->    CALL vStr%RemoveCharacters(CharSet, Protect=.TRUE.) <br>
        !   --->    CALL vStr%RemoveCharacters(CharSet, 2, Protect=.TRUE.) <br>
        !   --->    CALL vStr%RemoveCharacters(CharSet, Protect=.TRUE., ExclMrk=.FALSE.) <br>
        !   --->    CALL vStr%RemoveCharacters(CharSet, 3, .TRUE., .FALSE.) <br>
        GENERIC     :: RemoveCharacters => RemoveCharacters_CHS, RemoveCharacters_VLS
        !> **Subroutine Interface**: RemoveSubstring <br>
        !  **Purpose**:  To remove the substring from the string of the given FvlStr
        !       object based on specified input. <br>
        !  **Usage**: <br>
        !   --->    CALL vStr%RemoveSubstring(SubStr) <br>
        !   --->    CALL vStr%RemoveSubstring(lPos, rPos) <br>
        !   --->    CALL vStr%RemoveSubstring(SubStr, FirstOnly=.TRUE.) <br>
        !   --->    CALL vStr%RemoveSubstring(SubStr, Protect=.TRUE.) <br>
        !   --->    CALL vStr%RemoveSubstring(SubStr, Protect=.TRUE., ExclMrk=.FALSE.) <br>
        !   --->    CALL vStr%RemoveSubstring(SubStr, Protect=.TRUE., FirstOnly=.TRUE.) <br>
        !   --->    CALL vStr%RemoveSubstring(SubStr, .TRUE., .FALSE., .TRUE.) <br>
        GENERIC     :: RemoveSubstring  => RemoveSubstring_CHS, RemoveSubstring_VLS, &
                                           Delete_Substring
        !> **Subroutine Interface**: ReplaceSubstring <br>
        !  **Purpose**:  To replace all occurrences of the original substring found
        !       in the string of the given FvlStr object with the new substring based
        !       on specified input. <br>
        !  **Usage**: <br>
        !   --->    CALL vStr%ReplaceSubstring(OldSub, NewSub) <br>
        !   --->    CALL vStr%ReplaceSubstring(OldSub, NewSub, Protect=.TRUE.) <br>
        !   --->    CALL vStr%ReplaceSubstring(OldSub, NewSub, Protect=.TRUE., Recur=.TRUE.) <br>
        !   --->    CALL vStr%ReplaceSubstring(OldSub, NewSub, Protect=.TRUE., ExclMrk=.FALSE.) <br>
        !   --->    CALL vStr%ReplaceSubstring(OldSub, NewSub, .TRUE., .TRUE., .FALSE.) <br>
        GENERIC     :: ReplaceSubstring => ReplaceSubstring_CHS_CHS, ReplaceSubstring_VLS_CHS, &
                                           ReplaceSubstring_CHS_VLS, ReplaceSubstring_VLS_VLS
        !> **Subroutine Interface**: Partition <br>
        !  **Purpose**:  To partition the string of the given FvlStr object into two
        !       substrings by a separator (single character or a multiple-character
        !       string).  The partition occurs at the first occurrence of the separator
        !       found. <br>
        !  **Usage**: <br>
        !   ! *SepSub* is a multiple-character string separator. <br>
        !   --->    CALL vStr%Partition(SepSub, SubStr) <br>
        !   ! *SepSet* is a set of characters where a character in the set is a valid <br>
        !   ! separator.  Also, search the first separator from the back. <br>
        !   --->    CALL vStr%Partition(SepSet, SubStr, SepChr, Back=.TRUE.) <br>
        GENERIC     :: Partition        => PartitionSepSub_CHS, PartitionSepSub_VLS, &
                                           PartitionSepChr_CHS, PartitionSepChr_VLS
        !> **Subroutine Interface**: Split <br>
        !  **Purpose**:  To split the string of the given FvlStr object into multiple
        !        substrings by a separator (single character or a multiple-character string). <br>
        !  **Usage**: <br>
        !   ! *SepSub* is a multiple-character string separator. <br>
        !   --->    CALL vStr%Split(SepSub, SubStr) <br>
        !   ! *SepSet* is a set of characters where a character in the set is a valid separator. <br>
        !   --->    CALL vStr%Split(SepSet, SubStr, SepChr) <br>
        !   ! only find a separator in the unprotected regions. <br>
        !   --->    CALL vStr%Split(SepSub, SubStr, Protect=.TRUE.) <br>
        !   ! only find separators in the unprotected regions. <br>
        !   --->    CALL vStr%Split(SepSet, SubStr, SepChr, Protect=.TRUE.) <br>
        !   ! The exclamation mark is NOT used to define the protected regions. <br>
        !   --->    CALL vStr%Split(SepSub, SubStr, Protect=.TRUE., ExclMrk=.FALSE.) <br>
        GENERIC     :: Split            => SplitSepSub_CHS, SplitSepSub_VLS, &
                                           SplitSepChr_CHS, SplitSepChr_VLS
        ! ---------------------------------------------------------------------
        ! -----         Reading and Writing Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Subroutine Interface**: WriteOutput <br>
        !  **Purpose**:  To write the string of a FvlStr object to a connected
        !       formatted unit. <br>
        !  **Usage**: <br>
        !   --->    CALL vStr%WriteOutput(IOUnit) <br>
        !   --->    CALL vStr%WriteOutput(IOUnit,IOStat,IOMsg) <br>
        PROCEDURE           :: WriteOutput
        !> **Subroutine Interface**: ReadComnandLine <br>
        !  **Purpose**:  To read a character sequence from a command line into the
        !       FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    CALL vStr%ReadComnandLine() <br>
        PROCEDURE           :: ReadComnandLine
        !> **Subroutine Interface**: ReadInput <br>
        !  **Purpose**:  To read a character sequence from a connected formatted unit
        !       into the FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    CALL vStr%ReadInput(IOUnit) <br>
        !   --->    CALL vStr%ReadInput(IOUnit,IOStat,IOMsg) <br>
        PROCEDURE           :: ReadInput
        ! ---------------------------------------------------------------------
        ! -----     Deferred/Overridden Procedures from Object Type       -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => FvlStr_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => FvlStr_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => FvlStr_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => FvlStr_ToString
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => FvlStr_HashCode
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Comparable Type          -----
        ! ---------------------------------------------------------------------
        !> Use a common logical expression to compare two *Comparable* objects.
        PROCEDURE   :: CompareTo    => FvlStr_CompareTo
        ! ---------------------------------------------------------------------
        ! -----                 Final Procedure                           -----
        ! ---------------------------------------------------------------------
        FINAL               :: FinalizeFvlStr
        ! ---------------------------------------------------------------------
    END TYPE FvlStr

!** INTERFACE DEFINITIONS:
    !------------------------------------------------------------------
    !-----      Interfaces for 'Type-Bound' Procedures            -----
    !------------------------------------------------------------------
    ! interfaces for 'SClass_FvlStr_Conversion'
    INTERFACE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION IByte_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(IntNum)
            !^ To convert a FvlStr object to an 8-bit integer number.
            CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt8                              :: IntNum   !! integer number
        END FUNCTION IByte_From_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION IShort_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(IntNum)
            !^ To convert a FvlStr object to a 16-bit integer number.
            CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt16                             :: IntNum   !! integer number
        END FUNCTION IShort_From_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION Integer_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(IntNum)
            !^ To convert a FvlStr object to a 32-bit integer number.
            CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt32                             :: IntNum   !! integer number
        END FUNCTION Integer_From_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ILong_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(IntNum)
            !^ To convert a FvlStr object to a 64-bit integer number.
            CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt64                             :: IntNum   !! integer number
        END FUNCTION ILong_From_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION RSingle_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(RealNum)
            !^ To convert a FvlStr object to a single-precision real number.
            CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tRealSP                             :: RealNum  !! real number
        END FUNCTION RSingle_From_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION RDouble_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(RealNum)
            !^ To convert a FvlStr object to a double-precision real number.
            CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tRealDP                             :: RealNum  !! real number
        END FUNCTION RDouble_From_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION RQuad_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(RealNum)
            !^ To convert a FvlStr object to a quadruple-precision real number.
            CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tRealQP                             :: RealNum  !! real number
        END FUNCTION RQuad_From_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CSingle_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(CmpxNum)
            !^ To convert a FvlStr object to a single-precision complex number.
            CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tCmpxSP                             :: CmpxNum  !! complex number
        END FUNCTION CSingle_From_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CDouble_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(CmpxNum)
            !^ To convert a FvlStr object to a double-precision complex number.
            CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tCmpxDP                             :: CmpxNum  !! complex number
        END FUNCTION CDouble_From_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CQuad_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(CmpxNum)
            !^ To convert a FvlStr object to a quadruple-precision complex number.
            CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tCmpxQP                             :: CmpxNum  !! complex number
        END FUNCTION CQuad_From_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION Logical_From_FvlStr(vStr) RESULT(Boolean)
            !^ To convert a FvlStr object to a logical value.
            CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
            tLogical                    :: Boolean  !! logical value
        END FUNCTION Logical_From_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CharArray_From_FvlStr(vStr,IsCString) RESULT(cArr)
            !^ To convert a FvlStr object to an allocatable array of characters.
            CLASS(FvlStr),      INTENT(IN)  :: vStr     !! FvlStr object
            tLogical, OPTIONAL, INTENT(IN)  :: IsCString
            !^ flag indicating whether the array is a 'C' string or not. <br>
            !  If true, the array will contain a null character. <br>
            !  Default is FALSE.
            tChar, ALLOCATABLE              :: cArr(:)  !! character array
        END FUNCTION CharArray_From_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CharAlloc_From_FvlStr(vStr) RESULT(cStr)
            !^ To convert a FvlStr object to an allocatable character string.
            CLASS(FvlStr), INTENT(IN)   :: vStr !! FvlStr object
            tCharAlloc                  :: cStr !! character string
        END FUNCTION CharAlloc_From_FvlStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for 'SClass_FvlStr_Inquiry'
    INTERFACE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION Is_FvlStr_Number(vStr,Strict,NumVal) RESULT(NumFlag)
            !^ To check whether a FvlStr object represents a valid number and
            !  if so, what kind of number it is.
            CLASS(FvlStr),         INTENT(IN)   :: vStr     !! FvlStr object
            tLogical,              INTENT(IN)   :: Strict
            !^ Flag requesting for strict integer/real number. <br>
            ! If true, NumFlag is 1 or 2 if the FvlStr object is a valid integer or real number. <br>
            ! Otherwise, NumFlag is 0 if the FvlStr object is a valid integer or real number. <br>
            ! Default is false. <br>
            CLASS(*), ALLOCATABLE, INTENT(OUT)  :: NumVal   !! Value of number if it is valid.
            OPTIONAL                            :: Strict, NumVal
            tSInt32                             :: NumFlag
            !^ Flag indicating what kind of number the FvlStr object represents. <br>
            ! NumFlag = -1, the string is NOT a number. <br>
            ! NumFlag =  0, the string is a valid integer or real number. <br>
            ! NumFlag =  1, the string is strictly an integer number. <br>
            ! NumFlag =  2, the string is strictly a real number. <br>
            ! NumFlag =  3, the string is a valid complex number. <br>
        END FUNCTION Is_FvlStr_Number
        ! ---------------------------------------------------------------------
        MODULE FUNCTION Is_FvlStr_Logical(vStr,Boolean) RESULT(LogFlag)
            !^ To check whether a FvlStr object represents a logical value where valid
            !  string include 'T', 'F', 't', 'f', 'TRUE', 'FALSE', 'true', and 'false'.
            CLASS(FvlStr),      INTENT(IN)  :: vStr     !! FvlStr object
            tLogical, OPTIONAL, INTENT(OUT) :: Boolean  !! logical value if flag is true; otherwise, set to FalseVal
            tLogical                        :: LogFlag  !! true if FvlStr object is a logical value
        END FUNCTION Is_FvlStr_Logical
        ! ---------------------------------------------------------------------
        MODULE FUNCTION Is_FvlStr_InClass(vStr,ClassType,FailIndex) RESULT(ClassFlag)
            !^ To check whether a FvlStr object is in the specified class. <br>
            ! The following FvlStr classes are recognized: <br>
            !   ALPHABET, ALPHANUM, ASCII, BLANK, COMPLEX, CONTROL, DIGIT, FNAME,
            !   GRAPHICAL, INTEGER, LOGICAL, LOWERCASE, PUNCTUATION, PRINTABLE,
            !   REAL, UPPERCASE, WHITESPACE, HEXDIGIT, OCTDIGIT. <br>
            ! See explanations of classes in the "IsStringInClass" and
            !   "IsCharacterInClass" routines in the "MBase_ChrStr" module. <br>
            CLASS(FvlStr),    INTENT(IN)    :: vStr         !! FvlStr object
            tCharStar,        INTENT(IN)    :: ClassType    !! FvlStr class
            tIndex, OPTIONAL, INTENT(OUT)   :: FailIndex    !! flag indicating position of the failed character
            tLogical                        :: ClassFlag    !! true if FvlStr object is in the specified class
        END FUNCTION Is_FvlStr_InClass
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CountSubstring_CHS(vStr,sStr,Overlap) RESULT(nCount)
            !^ To count the number of occurrences of the specified substring in
            !  the given FvlStr object. <br>
            CLASS(FvlStr),      INTENT(IN)  :: vStr     !! FvlStr object
            tCharStar,          INTENT(IN)  :: sStr     !! substring
            tLogical, OPTIONAL, INTENT(IN)  :: Overlap
            !^ flag indicating whether overlapping occurrences of the substring
            !  are allowed or not. <br>
            !  - If true, count the overlapping occurrences. <br>
            !  - If false, count the non-overlapping occurrences . <br>
            !  Default is false.
            tIndex                          :: nCount   !! number of occurrences
        END FUNCTION CountSubstring_CHS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CountSubstring_VLS(vStr,sStr,Overlap) RESULT(nCount)
            !^ See the "CountSubstring_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *sStr* argument. <br>
            CLASS(FvlStr),      INTENT(IN)  :: vStr
            TYPE(FvlStr),       INTENT(IN)  :: sStr
            tLogical, OPTIONAL, INTENT(IN)  :: Overlap
            tIndex                          :: nCount
        END FUNCTION CountSubstring_VLS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CountCharacters_CHS(vStr,ChrSet,Protect,ExclMrk) RESULT(nCount)
            !^ To count the number of occurrences of character(s) in the FvlStr object
            !  for any character appearing in the given character set.  Optionally, a
            !  user can specify flags indicating whether protected regions exist or not
            !  and whether the exclamation mark is used to protect regions.  If protected
            !  regions exist, only characters in unprotected regions are counted. <br>
            CLASS(FvlStr),      INTENT(IN)  :: vStr     !! FvlStr object
            tCharStar,          INTENT(IN)  :: ChrSet   !! Set of characters
            tLogical, OPTIONAL, INTENT(IN)  :: Protect
            !^ Protect region flag. <br>
            ! - True if protected regions exists. <br>
            ! - False if protected regions do not exist. <br>
            ! Default is false.
            tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk
            !^ Exclamation mark flag. <br>
            ! - True if exclamation mark is also used to protect regions. <br>
            ! - False if only (single and/or double) quotes are used to protect regions. <br>
            ! Default is true.
            tIndex                          :: nCount   !! number of occurrences
        END FUNCTION CountCharacters_CHS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CountCharacters_VLS(vStr,ChrSet,Protect,ExclMrk) RESULT(nCount)
            !^ See the "CountCharacters_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *ChrSet* argument. <br>
            CLASS(FvlStr),      INTENT(IN)  :: vStr
            TYPE(FvlStr),       INTENT(IN)  :: ChrSet
            tLogical, OPTIONAL, INTENT(IN)  :: Protect
            tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk
            tIndex                          :: nCount
        END FUNCTION CountCharacters_VLS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CountWords_VLS(vStr) RESULT(nCount)
            !^ To count the number of words (separated by blanks) in the FvlStr object.
            CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
            tIndex                      :: nCount   !! number of words
        END FUNCTION CountWords_VLS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION FindProtectedRegions_VLS(vStr,lPos,rPos,ExclMrk) RESULT(nRegion)
            !^ To look for quotes (and/or an exclamation mark) to find regions
            !  that must be protected from string editing.  Return the number
            !  of protected regions as well as positions of the first and last
            !  characters of each region. <br>
            !  **Technical Notes**: <br>
            !  - Single quote, double quote and optionally exclamation mark are used as
            !    delimiters to find protected regions. <br>
            !  - Two single quotes or two double quotes are used to define a protected
            !    region whereas an exclamation mark indicates that all characters
            !    following it are all protected. <br>
            !  - This routine is designed specifically for manipulating Fortran source code
            !    where an exclamation mark is used for a comment and two (single or double)
            !    quotes are used to specify a value to a character variable or literal.
            CLASS(FvlStr),       INTENT(IN)     :: vStr     !! FvlStr object
            tIndex, ALLOCATABLE, INTENT(OUT)    :: lPos(:)
            !^ positions of the first character of protected regions
            tIndex, ALLOCATABLE, INTENT(OUT)    :: rPos(:)
            !^ positions of the last character of protected regions
            tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tIndex                              :: nRegion  !! number of protected regions
        END FUNCTION FindProtectedRegions_VLS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION FindSubstring_CHS(vStr,sStr,sPos,Protect,ExclMrk) RESULT(nCount)
            !^ To count the number of non-overlapping occurrences of substring in the FvlStr object
            !  and also return position(s) of the first character of substring found. <br>
            !  If *Protect* is present and its value is true, only those occurrences found in the
            !  unprotected region(s) are counted. <br>
            !  See explanations about the protected region(s) in the "FindProtectedRegions" procedure. <br>
            CLASS(FvlStr),       INTENT(IN)     :: vStr     !! FvlStr object
            tCharStar,           INTENT(IN)     :: sStr     !! substring
            tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)
            !^ position(s) of the first character of substring found
            tLogical,  OPTIONAL, INTENT(IN)     :: Protect
            !^ flag indicating whether protected regions exist or not. <br>
            ! -> If true, protected regions exist. <br>
            ! -> If false, protected regions do not exist. <br>
            !  Default is false.
            tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tIndex                              :: nCount   !! number of occurrences
        END FUNCTION FindSubstring_CHS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION FindSubstring_VLS(vStr,sStr,sPos,Protect,ExclMrk) RESULT(nCount)
            !^ See the "FindSubstring_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *sStr* argument. <br>
            CLASS(FvlStr),       INTENT(IN)     :: vStr
            TYPE(FvlStr),        INTENT(IN)     :: sStr
            tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)
            tLogical,  OPTIONAL, INTENT(IN)     :: Protect
            tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
            tIndex                              :: nCount
        END FUNCTION FindSubstring_VLS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION FindDelimiters_CHS(vStr,ChrSet,dPos,Protect,ExclMrk) RESULT(nCount)
            !^ To count the number of occurrences of delimiter(s) in the FvlStr object
            !  and also return position(s) of the delimiter(s) found. <br>
            !  A delimiter is any character appearing in the given character set. <br>
            !  If Protect is present and its value is true, only those occurrences found in the
            !  unprotected region(s) are counted. <br>
            !  See explanations about the protected region(s) in the "FindProtectedRegions" procedure. <br>
            CLASS(FvlStr),       INTENT(IN)     :: vStr     !! FvlStr object
            tCharStar,           INTENT(IN)     :: ChrSet   !! a set of characters
            tIndex, ALLOCATABLE, INTENT(OUT)    :: dPos(:)  !! position(s) of the delimiter(s) found
            tLogical,  OPTIONAL, INTENT(IN)     :: Protect
            !^ flag indicating whether protected regions exist or not. <br>
            ! -> If true, protected regions exist. <br>
            ! -> If false, protected regions do not exist. <br>
            !  Default is false.
            tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tIndex                              :: nCount   !! number of occurrences
        END FUNCTION FindDelimiters_CHS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION FindDelimiters_VLS(vStr,ChrSet,dPos,Protect,ExclMrk) RESULT(nCount)
            !^ See the "FindDelimiters_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *ChrSet* argument. <br>
            CLASS(FvlStr),       INTENT(IN)     :: vStr
            TYPE(FvlStr),        INTENT(IN)     :: ChrSet
            tIndex, ALLOCATABLE, INTENT(OUT)    :: dPos(:)
            tLogical,  OPTIONAL, INTENT(IN)     :: Protect
            tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
            tIndex                              :: nCount
        END FUNCTION FindDelimiters_VLS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION FindSeparators_CHS(vStr,Separator,CharSet,sPos,Protect,ExclMrk) RESULT(nCount)
            !^ To count the number of occurrences of separator(s) in the FvlStr object
            !  and also return position(s) of the separator(s) found. <br>
            !  A separator can be a (single) character or a character string (multiple characters). <br>
            !  The argument "CharSet" is a flag used to specify whether the separator is a character
            !  or a character string. If it is true, the argument "Separator" contains a set of
            !  characters where a separator is any character in the set.  If it is false,
            !  the argument "Separator" specifies the character-string separator. <br>
            !  If Protect is present and its value is true, only those occurrences found in the
            !  unprotected region(s) are counted. <br>
            !  See explanations about the protected region(s) in the "FindProtectedRegions" procedure. <br>
            CLASS(FvlStr),       INTENT(IN)     :: vStr         !! FvlStr object
            tCharStar,           INTENT(IN)     :: Separator    !! separator
            tLogical,            INTENT(IN)     :: CharSet      !! a flag indicating type of the separator
            tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)      !! position(s) of the delimiter(s) found
            tLogical,  OPTIONAL, INTENT(IN)     :: Protect
            !^ flag indicating whether protected regions exist or not. <br>
            ! -> If true, protected regions exist. <br>
            ! -> If false, protected regions do not exist. <br>
            !  Default is false.
            tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tIndex                              :: nCount       !! number of occurrences
        END FUNCTION FindSeparators_CHS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION FindSeparators_VLS(vStr,Separator,CharSet,sPos,Protect,ExclMrk) RESULT(nCount)
            !^ See the "FindSeparators_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *Separator* argument. <br>
            CLASS(FvlStr),       INTENT(IN)     :: vStr
            TYPE(FvlStr),        INTENT(IN)     :: Separator
            tLogical,            INTENT(IN)     :: CharSet
            tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)
            tLogical,  OPTIONAL, INTENT(IN)     :: Protect
            tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
            tIndex                              :: nCount
        END FUNCTION FindSeparators_VLS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION GetCharacter(vStr,Pos) RESULT(Chr)
            !^ To get a character from FvlStr object based on the specified position. <br>
            CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
            tIndex,        INTENT(IN)   :: Pos      !! the position of the desired character
            tChar                       :: Chr      !! the character desired
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION GetSubstring_CHS(vStr,lPos,rPos) RESULT(cSub)
            !^ To get a specified substring from FvlStr object based on lPos and rPos. <br>
            !  If lPos is less than 1, then 1 is used as a starting point of the substring. <br>
            !  Similarly, if rPos is greater than the length of the FvlStr's string, then
            !  the length is used as an ending point. <br>
            !  If rPos is less than lPos, a zero-length string is returned. <br>
            CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
            tIndex,        INTENT(IN)   :: lPos     !! the leftmost character position of the substring
            tIndex,        INTENT(IN)   :: rPos     !! the rightmost character position of the substring
            tCharAlloc                  :: cSub     !! substring
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION GetSubstring_VLS(vStr,lPos,rPos) RESULT(vSub)
            !^ See the "GetSubstring_CHS" procedure, where the only difference
            !  between these two procedures is the type of the returned argument. <br>
            CLASS(FvlStr), INTENT(IN)   :: vStr
            tIndex,        INTENT(IN)   :: lPos
            tIndex,        INTENT(IN)   :: rPos
            TYPE(FvlStr)                :: vSub
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION GetSlice_CHS(vStr,First,Last,Stride) RESULT(Slice)
            !^ To extract the characters from the region between *First* and *Last* indices
            ! (both inclusive) of the given string by taking strides of length *Stride*. <br>
            CLASS(FvlStr),    INTENT(IN)    :: vStr     !! FvlStr object
            tIndex, OPTIONAL, INTENT(IN)    :: First    !! the first index
            tIndex, OPTIONAL, INTENT(IN)    :: Last     !! the last index
            tIndex, OPTIONAL, INTENT(IN)    :: Stride   !! the stride
            tCharAlloc                      :: Slice    !! character slice
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION GetSlice_VLS(vStr,First,Last,Stride) RESULT(Slice)
            !^ See the "GetSlice_CHS" procedure, where the only difference
            !  between these two procedures is the type of the returned argument. <br>
            CLASS(FvlStr),    INTENT(IN)    :: vStr
            tIndex, OPTIONAL, INTENT(IN)    :: First
            tIndex, OPTIONAL, INTENT(IN)    :: Last
            tIndex, OPTIONAL, INTENT(IN)    :: Stride
            TYPE(FvlStr)                    :: Slice
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StartWith_CHS(vStr,sStr) RESULT(Flag)
            !^ To check whether the string of the given FvlStr object starts with
            !  the specified substring or not. <br>
            CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
            tCharStar,     INTENT(IN)   :: sStr     !! substring
            tLogical                    :: Flag     !! true if the string starts with the substring
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StartWith_VLS(vStr,sStr) RESULT(Flag)
            !^ See the "StartWith_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *sStr* argument. <br>
            CLASS(FvlStr), INTENT(IN)   :: vStr
            TYPE(FvlStr),  INTENT(IN)   :: sStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION EndWith_CHS(vStr,sStr) RESULT(Flag)
            !^ To check whether the string of the given FvlStr object ends with
            !  the specified substring or not. <br>
            CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
            tCharStar,     INTENT(IN)   :: sStr     !! substring
            tLogical                    :: Flag     !! true if the string ends with the substring
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION EndWith_VLS(vStr,sStr) RESULT(Flag)
            !^ See the "EndWith_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *sStr* argument. <br>
            CLASS(FvlStr), INTENT(IN)   :: vStr
            TYPE(FvlStr),  INTENT(IN)   :: sStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for 'SClass_FvlStr_Manipulation'
    INTERFACE
        ! ---------------------------------------------------------------------
        MODULE ELEMENTAL SUBROUTINE AlterCase(vStr,ToUpper)
            !^ To change case of the character string of the FvlStr object according to flag.
            CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
            tLogical,      INTENT(IN)       :: ToUpper
            !^ flag indicating whether to change the string's characters to
            !  upper-case characters or not. <br>
            !  - If true, the string contains upper-case characters on exit. <br>
            !  - If false, the string contains lower-case characters on exit. <br>
        END SUBROUTINE AlterCase
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE BlankCompressAlterCase(vStr,ToUpper)
            !^ To first remove all blanks and then change case of the character string
            !  of the FvlStr object according to flag.
            CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
            tLogical,      INTENT(IN)       :: ToUpper
            !^ flag indicating whether to change the string's characters to
            !  upper-case characters or not. <br>
            !  - If true, the string contains upper-case characters on exit. <br>
            !  - If false, the string contains lower-case characters on exit. <br>
        END SUBROUTINE BlankCompressAlterCase
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE AlterCaseProtect(vStr,nRegion,lPos,rPos,ToUpper)
            !^ To change case of the character string of the FvlStr object according
            !  to flag where only unprotected region(s) are allowed to be changed.
            CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
            tIndex,        INTENT(IN)       :: nRegion  !! number of protected regions
            tIndex,        INTENT(IN)       :: lPos(:)  !! positions of the first character of protected regions
            tIndex,        INTENT(IN)       :: rPos(:)  !! positions of the last character of protected regions
            tLogical,      INTENT(IN)       :: ToUpper
            !^ flag indicating whether to change the string's characters to
            !  upper-case characters or not. <br>
            !  - If true, the string contains upper-case characters on exit. <br>
            !  - If false, the string contains lower-case characters on exit. <br>
        END SUBROUTINE AlterCaseProtect
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE InsertSubstring_CHS(vStr,Pos,sStr)
            !^ To insert substring into the FvlStr object at a specified position. <br>
            !  If Pos is less than 1, then 1 is used as an insertion point. <br>
            !  If Pos is greater than length of the character string, then
            !  the substring is inserted at the end of the character string. <br>
            CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
            tIndex,        INTENT(IN)       :: Pos      !! the insertion point
            tCharStar,     INTENT(IN)       :: sStr     !! substring
        END SUBROUTINE InsertSubstring_CHS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE InsertSubstring_VLS(vStr,Pos,sStr)
            !^ See the "InsertSubstring_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *sStr* argument. <br>
            CLASS(FvlStr), INTENT(INOUT)    :: vStr
            tIndex,        INTENT(IN)       :: Pos
            TYPE(FvlStr),  INTENT(IN)       :: sStr
        END SUBROUTINE InsertSubstring_VLS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE RemoveCharacters_CHS(vStr,ChrSet,Option,Protect,ExclMrk)
            !^ To remove characters from the FvlStr object depending on the specified
            !  Option and ChrSet. <br>
            !  If Protect is present and its value is true, only those occurrences found in the
            !  unprotected region(s) are removed. <br>
            !  See explanations about the protected region(s) in the "FindProtectedRegions"
            !  procedure in the "SClass_FvlStr_Inquiry" submodule. <br>
            !  See explanations of the *ChrSet* and *Option* arguments in the "RemoveCharacters"
            !  procedure in the "SBase_ChrStr_Manipulation" submodule of the "MBase_ChrStr"
            !  module. <br>
            CLASS(FvlStr),      INTENT(INOUT)   :: vStr     !! FvlStr object
            tCharStar,          INTENT(IN)      :: ChrSet   !! set of characters to be removed
            tSInt32,  OPTIONAL, INTENT(IN)      :: Option   !! flag indicating how to remove characters
            tLogical, OPTIONAL, INTENT(IN)      :: Protect
            !^ flag indicating whether protected regions exist or not. <br>
            ! -> If true, protected regions exist. <br>
            ! -> If false, protected regions do not exist. <br>
            !  Default is false.
            tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
        END SUBROUTINE RemoveCharacters_CHS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE RemoveCharacters_VLS(vStr,ChrSet,Option,Protect,ExclMrk)
            !^ See the "RemoveCharacters_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *ChrSet* argument. <br>
            CLASS(FvlStr),      INTENT(INOUT)   :: vStr
            TYPE(FvlStr),       INTENT(IN)      :: ChrSet
            tSInt32,  OPTIONAL, INTENT(IN)      :: Option
            tLogical, OPTIONAL, INTENT(IN)      :: Protect
            tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
        END SUBROUTINE RemoveCharacters_VLS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE RemoveSubstring_CHS(vStr,sStr,Protect,ExclMrk,FirstOnly)
            !^ To remove substring from the FvlStr object. <br>
            !  If Protect is present and its value is true, only those occurrences found in the
            !  unprotected region(s) are removed. <br>
            !  See explanations about the protected region(s) in the "FindProtectedRegions"
            !  procedure in the "SClass_FvlStr_Inquiry" submodule. <br>
            CLASS(FvlStr),      INTENT(INOUT)   :: vStr     !! FvlStr object
            tCharStar,          INTENT(IN)      :: sStr     !! substring to be removed
            tLogical, OPTIONAL, INTENT(IN)      :: Protect
            !^ flag indicating whether protected regions exist or not. <br>
            ! -> If true, protected regions exist. <br>
            ! -> If false, protected regions do not exist. <br>
            !  Default is false.
            tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tLogical, OPTIONAL, INTENT(IN)      :: FirstOnly
            !^ flag indicating whether only the first occurrence is removed. <br>
            !  - If true, only the first occurrence is removed. <br>
            !  - If false, all occurrences are removed. <br>
            !  Default is false.
        END SUBROUTINE RemoveSubstring_CHS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE RemoveSubstring_VLS(vStr,sStr,Protect,ExclMrk,FirstOnly)
            !^ See the "RemoveSubstring_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *sStr* argument. <br>
            CLASS(FvlStr),      INTENT(INOUT)   :: vStr
            TYPE(FvlStr),       INTENT(IN)      :: sStr
            tLogical, OPTIONAL, INTENT(IN)      :: Protect
            tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
            tLogical, OPTIONAL, INTENT(IN)      :: FirstOnly
        END SUBROUTINE RemoveSubstring_VLS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE Delete_Substring(vStr,lPos,rPos)
            !^ To remove substring from the FvlStr object at specified positions. <br>
            !  If lPos is less than 1, then 1 is used as a starting point of the substring. <br>
            !  Similarly, if rPos is greater than length of the FvlStr's string, then
            !  the length is used as a starting point. <br>
            !  If rPos is less than lPos, the same FvlStr object is returned. <br>
            CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
            tIndex,        INTENT(IN)       :: lPos     !! the leftmost character position of the substring
            tIndex,        INTENT(IN)       :: rPos     !! the rightmost character position of the substring
        END SUBROUTINE Delete_Substring
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE CropBlanks_VLS(vStr,SpaceOnly)
            !^ To remove leading and trailing blanks from the FvlStr object.
            CLASS(FvlStr),      INTENT(INOUT)   :: vStr     !! FvlStr object
            tLogical, OPTIONAL, INTENT(IN)      :: SpaceOnly
            !^ flag indicating whether to only remove the space character or not. <br>
            ! - True if requesting to remove only the space character. <br>
            ! - False if requesting to remove both the tab and the space characters. <br>
            ! Default is false.
        END SUBROUTINE CropBlanks_VLS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE CompactString_VLS(vStr)
            !^ To convert multiple spaces and tabs into a single space, delete
            !  control characters and removes initial (leading and trailing) spaces
            !  of the string of the specified FvlStr object.
            CLASS(FvlStr), INTENT(INOUT)    :: vStr
        END SUBROUTINE CompactString_VLS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE CompressString_VLS(vStr)
            !^ To remove spaces, tabs and control characters from the string
            !  of the specified FvlStr object.
            CLASS(FvlStr), INTENT(INOUT)    :: vStr
        END SUBROUTINE CompressString_VLS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ReplaceSubstring_CHS_CHS(vStr,oStr,nStr,Protect,Recur,ExclMrk)
            !^ To replace (all) occurrences of the original substring found in the FvlStr
            !  object with the new substring. <br>
            !  Optional inputs (Protect, Recur and ExclMrk) can change how the substring
            !  are replaced. <br>
            CLASS(FvlStr),      INTENT(INOUT)   :: vStr     !! FvlStr object
            tCharStar,          INTENT(IN)      :: oStr     !! original (old) substring
            tCharStar,          INTENT(IN)      :: nStr     !! new substring
            tLogical, OPTIONAL, INTENT(IN)      :: Protect
            !^ flag indicating whether protected regions exist or not. <br>
            ! -> If true, protected regions exist. <br>
            ! -> If false, protected regions do not exist. <br>
            !  Default is false.
            tLogical, OPTIONAL, INTENT(IN)      :: Recur
            !^ flag indicating how to replace substring(s). <br>
            !  - If true, to replace in a recursive way. <br>
            !  - If true, to replace in a non-recursive way. <br>
            !  Default is false. <br>
            !  For example: set vStr = 'abbbbb', oStr = 'ab', and nStr = 'a' <br>
            !  if Recur = TrueVal,  the returning vStr = 'ab' <br>
            !  if Recur = FalseVal, the returning vStr = 'abbbb' <br>
            !  *Important Note*: If *Protect* is present and set to true,
            !       the substring will be replaced in a non-recursive way
            !       regardless of the present and value of Recur. <br>
            tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
        END SUBROUTINE ReplaceSubstring_CHS_CHS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ReplaceSubstring_VLS_CHS(vStr,oStr,nStr,Protect,Recur,ExclMrk)
            !^ See the "ReplaceSubstring_CHS_CHS" procedure, where the only differences
            !  between these two procedures are the types of the *oStr* and/or *nStr*
            !  argument(s). <br>
            CLASS(FvlStr),      INTENT(INOUT)   :: vStr
            TYPE(FvlStr),       INTENT(IN)      :: oStr
            tCharStar,          INTENT(IN)      :: nStr
            tLogical, OPTIONAL, INTENT(IN)      :: Protect
            tLogical, OPTIONAL, INTENT(IN)      :: Recur
            tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
        END SUBROUTINE ReplaceSubstring_VLS_CHS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ReplaceSubstring_CHS_VLS(vStr,oStr,nStr,Protect,Recur,ExclMrk)
            !^ See the "ReplaceSubstring_CHS_CHS" procedure, where the only differences
            !  between these two procedures are the types of the *oStr* and/or *nStr*
            !  argument(s). <br>
            CLASS(FvlStr),      INTENT(INOUT)   :: vStr
            tCharStar,          INTENT(IN)      :: oStr
            TYPE(FvlStr),       INTENT(IN)      :: nStr
            tLogical, OPTIONAL, INTENT(IN)      :: Protect
            tLogical, OPTIONAL, INTENT(IN)      :: Recur
            tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
        END SUBROUTINE ReplaceSubstring_CHS_VLS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ReplaceSubstring_VLS_VLS(vStr,oStr,nStr,Protect,Recur,ExclMrk)
            !^ See the "ReplaceSubstring_CHS_CHS" procedure, where the only differences
            !  between these two procedures are the types of the *oStr* and/or *nStr*
            !  argument(s). <br>
            CLASS(FvlStr),      INTENT(INOUT)   :: vStr
            TYPE(FvlStr),       INTENT(IN)      :: oStr
            TYPE(FvlStr),       INTENT(IN)      :: nStr
            tLogical, OPTIONAL, INTENT(IN)      :: Protect
            tLogical, OPTIONAL, INTENT(IN)      :: Recur
            tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
        END SUBROUTINE ReplaceSubstring_VLS_VLS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE PartitionSepSub_CHS(vStr,SepSub,sStr,Back)
            !^ To partition a string into two substrings where the specified separator
            !  is a multiple-character string.  The partition occurs at the first
            !  occurrence of the separator found.
            CLASS(FvlStr),      INTENT(IN)      :: vStr     !! FvlStr object
            tCharStar,          INTENT(IN)      :: SepSub   !! multiple-character separator
            TYPE(FvlStr),       INTENT(OUT)     :: sStr(2)
            !^ substrings where sStr(1) is a substring before
            !  the separator found and sStr(2) is the one after.
            tLogical, OPTIONAL, INTENT(IN)      :: Back
            !^ If present and true, searching from the back;
            ! otherwise, searching from the front.
        END SUBROUTINE PartitionSepSub_CHS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE PartitionSepSub_VLS(vStr,SepSub,sStr,Back)
            !^ See the "PartitionSepSub_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *SepSub* argument. <br>
            CLASS(FvlStr),      INTENT(IN)      :: vStr
            TYPE(FvlStr),       INTENT(IN)      :: SepSub
            TYPE(FvlStr),       INTENT(OUT)     :: sStr(2)
            tLogical, OPTIONAL, INTENT(IN)      :: Back
        END SUBROUTINE PartitionSepSub_VLS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE PartitionSepChr_CHS(vStr,SepSet,sStr,SepChr,Back)
            !^ To partition a string into two substrings where the separator is a single
            !  character (any character in the specified set of characters).  The partition
            !  occurs at the first occurrence of the separator found.
            CLASS(FvlStr),      INTENT(IN)      :: vStr     !! FvlStr object
            tCharStar,          INTENT(IN)      :: SepSet
            !^ set of characters representing valid separators
            TYPE(FvlStr),       INTENT(OUT)     :: sStr(2)
            !^ substrings where sStr(1) is a substring before
            ! the separator found and sStr(2) is the one after.
            tChar,              INTENT(OUT)     :: SepChr   !! the separator found
            tLogical, OPTIONAL, INTENT(IN)      :: Back
            !^ If present and true, searching from the back;
            ! otherwise, searching from the front.
        END SUBROUTINE PartitionSepChr_CHS
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE PartitionSepChr_VLS(vStr,SepSet,sStr,SepChr,Back)
            !^ See the "PartitionSepChr_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *SepSet* argument. <br>
            CLASS(FvlStr),      INTENT(IN)      :: vStr
            TYPE(FvlStr),       INTENT(IN)      :: SepSet
            TYPE(FvlStr),       INTENT(OUT)     :: sStr(2)
            tChar,              INTENT(OUT)     :: SepChr
            tLogical, OPTIONAL, INTENT(IN)      :: Back
        END SUBROUTINE PartitionSepChr_VLS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION SplitSepSub_CHS(vStr,SepSub,sStr,Protect,ExclMrk) RESULT(nCount)
            !^ To split a string of the FvlStr object into multiple substrings where the
            !  specified separator is a multiple-character string.  The number of substrings
            !  is equal to the number of occurrences of the separator found plus one.  The
            !  substrings may be a zero-length string if the separator is found at the
            !  beginning or at the end of the given string. <br>
            !  If Protect is present and its value is true, only those occurrences found in
            !  the unprotected region(s) are counted. <br>
            !  See explanations about the protected region(s) in the "FindProtectedRegions"
            !  procedure in the "SClass_FvlStr_Inquiry" submodule. <br>
            CLASS(FvlStr),             INTENT(IN)   :: vStr     !! FvlStr object
            tCharStar,                 INTENT(IN)   :: SepSub   !! multiple-character separator
            TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)  :: sStr(:)  !! output substrings
            tLogical,        OPTIONAL, INTENT(IN)   :: Protect
            !^ flag indicating whether protected regions exist or not. <br>
            ! -> If true, protected regions exist. <br>
            ! -> If false, protected regions do not exist. <br>
            !  Default is false.
            tLogical,        OPTIONAL, INTENT(IN)   :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tIndex                                  :: nCount   !! number of occurrences of separator found
        END FUNCTION SplitSepSub_CHS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION SplitSepSub_VLS(vStr,SepSub,sStr,Protect,ExclMrk) RESULT(nCount)
            !^ See the "SplitSepSub_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *SepSub* argument. <br>
            CLASS(FvlStr),             INTENT(IN)   :: vStr
            TYPE(FvlStr),              INTENT(IN)   :: SepSub
            TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)  :: sStr(:)
            tLogical,        OPTIONAL, INTENT(IN)   :: Protect
            tLogical,        OPTIONAL, INTENT(IN)   :: ExclMrk
            tIndex                                  :: nCount
        END FUNCTION SplitSepSub_VLS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION SplitSepChr_CHS(vStr,SepSet,sStr,SepChr,Protect,ExclMrk) RESULT(nCount)
            !^ To split a string of the FvlStr object into multiple substrings where a
            !  separator is a single character (any character in the specified set of
            !  characters).  The number of substrings is equal to the number of occurrences
            !  of the separator found plus one.  The substrings may be a zero-length string
            !  if the separator is found at the beginning or at the end of the given string. <br>
            !  If Protect is present and its value is true, only those occurrences found in
            !  the unprotected region(s) are counted. <br>
            !  See explanations about the protected region(s) in the "FindProtectedRegions"
            !  procedure in the "SClass_FvlStr_Inquiry" submodule. <br>
            CLASS(FvlStr),             INTENT(IN)   :: vStr     !! FvlStr object
            tCharStar,                 INTENT(IN)   :: SepSet   !! set of characters representing valid separators
            TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)  :: sStr(:)  !! output substrings
            tChar,        ALLOCATABLE, INTENT(OUT)  :: SepChr(:)!! the separators found
            tLogical,        OPTIONAL, INTENT(IN)   :: Protect
            !^ flag indicating whether protected regions exist or not. <br>
            ! -> If true, protected regions exist. <br>
            ! -> If false, protected regions do not exist. <br>
            !  Default is false.
            tLogical,        OPTIONAL, INTENT(IN)   :: ExclMrk
            !^ flag indicating whether the exclamation is used to define a protected
            !  region or not. <br>
            !  - If true, both the exclamation mark and quotes are used. <br>
            !  - If false, only quotes are used. <br>
            !  Default is true.
            tIndex                                  :: nCount   !! number of occurrences of separators found
        END FUNCTION SplitSepChr_CHS
        ! ---------------------------------------------------------------------
        MODULE FUNCTION SplitSepChr_VLS(vStr,SepSet,sStr,SepChr,Protect,ExclMrk) RESULT(nCount)
            !^ See the "SplitSepChr_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *SepSet* argument. <br>
            CLASS(FvlStr),             INTENT(IN)   :: vStr
            TYPE(FvlStr),              INTENT(IN)   :: SepSet
            TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)  :: sStr(:)
            tChar,        ALLOCATABLE, INTENT(OUT)  :: SepChr(:)
            tLogical,        OPTIONAL, INTENT(IN)   :: Protect
            tLogical,        OPTIONAL, INTENT(IN)   :: ExclMrk
            tIndex                                  :: nCount
        END FUNCTION SplitSepChr_VLS
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for 'SClass_FvlStr_Miscellaneous'
    INTERFACE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE WriteOutput(vStr,IOUnit,IOStat,IOMsg)
            !^ To write a character string of the FvlStr object to a connected formatted unit.
            CLASS(FvlStr),       INTENT(IN)     :: vStr     !! FvlStr object
            tSInt32,             INTENT(IN)     :: IOUnit   !! connected io unit number
            tSInt32,   OPTIONAL, INTENT(OUT)    :: IOStat   !! status of io operation
            tCharStar, OPTIONAL, INTENT(INOUT)  :: IOMsg    !! an io message if is IOStat is non-zero
        END SUBROUTINE WriteOutput
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ReadInput(vStr,IOUnit,IOStat,IOMsg)
            !^ To read a character sequence from a connected formatted unit into the FvlStr object.
            CLASS(FvlStr),        INTENT(INOUT) :: vStr     !! FvlStr object
            tSInt32,              INTENT(IN)    :: IOUnit   !! connected io unit number
            tSInt32,    OPTIONAL, INTENT(OUT)   :: IOStat   !! status of io operation
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: IOMsg    !! an io message
        END SUBROUTINE ReadInput
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE ReadComnandLine(vStr)
            !^ To read a character sequence from a command line into the FvlStr object.
            CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
        END SUBROUTINE ReadComnandLine
        ! ---------------------------------------------------------------------
    END INTERFACE
    !------------------------------------------------------------------
    !-----      Interfaces for 'Assignment' Procedures            -----
    !------------------------------------------------------------------
    INTERFACE ASSIGNMENT(=)
        !^ **Operator Overload**: ASSIGNMENT(=) <br>
        !  **Purpose**:  To convert between a FvlStr object and an other (Fortran
        !       intrinsic) type via an assignment expression. <br>
        !  **Usage**: <br>
        !   ! convert a 64-bit integer number to a FvlStr object <br>
        !   --->    vStr = I64Num <br>
        !   ! convert a FvlStr object to a quadruple-precision real number <br>
        !   --->    RQPNum = vStr <br>
        !  **Important Note**: The *assignment* and *conversion* operations are
        !       functionally similar but have some subtle differences.  In particular
        !       for those procedures that convert from a decimal string to a number,
        !       the *assignment* procedures will silently convert to a proper value
        !       (representation) if the input string is *INVALID* whereas the *conversion*
        !       procedures can optionally report the error occurred. <br>
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_From_CharacterString(vStr,cStr)
            !^ To assign a FvlStr object from a character string.
            CLASS(FvlStr), INTENT(OUT)  :: vStr
            tCharStar,     INTENT(IN)   :: cStr
        END SUBROUTINE FvlStr_From_CharacterString
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_From_CharacterArray(vStr,cArr)
            !^ To assign a FvlStr object from an array of characters.
            CLASS(FvlStr), INTENT(OUT)  :: vStr
            tChar,         INTENT(IN)   :: cArr(:)
        END SUBROUTINE FvlStr_From_CharacterArray
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_From_IByte(vStr,IntNum)
            !^ To assign a FvlStr object from an 8-bit integer number.
            CLASS(FvlStr), INTENT(OUT)  :: vStr
            tSInt8,        INTENT(IN)   :: IntNum
        END SUBROUTINE FvlStr_From_IByte
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_From_IShort(vStr,IntNum)
            !^ To assign a FvlStr object from a 16-bit integer number.
            CLASS(FvlStr), INTENT(OUT)  :: vStr
            tSInt16,       INTENT(IN)   :: IntNum
        END SUBROUTINE FvlStr_From_IShort
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_From_Integer(vStr,IntNum)
            !^ To assign a FvlStr object from a 32-bit integer number.
            CLASS(FvlStr), INTENT(OUT)  :: vStr
            tSInt32,       INTENT(IN)   :: IntNum
        END SUBROUTINE FvlStr_From_Integer
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_From_ILong(vStr,IntNum)
            !^ To assign a FvlStr object from a 64-bit integer number.
            CLASS(FvlStr), INTENT(OUT)  :: vStr
            tSInt64,       INTENT(IN)   :: IntNum
        END SUBROUTINE FvlStr_From_ILong
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_From_RSingle(vStr,RealNum)
            !^ To assign a FvlStr object from a 32-bit real number.
            CLASS(FvlStr), INTENT(OUT)  :: vStr
            tRealSP,       INTENT(IN)   :: RealNum
        END SUBROUTINE FvlStr_From_RSingle
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_From_RDouble(vStr,RealNum)
            !^ To assign a FvlStr object from a 64-bit real number.
            CLASS(FvlStr), INTENT(OUT)  :: vStr
            tRealDP,       INTENT(IN)   :: RealNum
        END SUBROUTINE FvlStr_From_RDouble
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_From_RQuad(vStr,RealNum)
            !^ To assign a FvlStr object from a 128-bit real number.
            CLASS(FvlStr), INTENT(OUT)  :: vStr
            tRealQP,       INTENT(IN)   :: RealNum
        END SUBROUTINE FvlStr_From_RQuad
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_From_CSingle(vStr,CmpxNum)
            !^ To assign a FvlStr object from a single-precision complex number.
            CLASS(FvlStr), INTENT(OUT)  :: vStr
            tCmpxSP,       INTENT(IN)   :: CmpxNum
        END SUBROUTINE FvlStr_From_CSingle
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_From_CDouble(vStr,CmpxNum)
            !^ To assign a FvlStr object from a double-precision complex number.
            CLASS(FvlStr), INTENT(OUT)  :: vStr
            tCmpxDP,       INTENT(IN)   :: CmpxNum
        END SUBROUTINE FvlStr_From_CDouble
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_From_CQuad(vStr,CmpxNum)
            !^ To assign a FvlStr object from a quadruple-precision complex number.
            CLASS(FvlStr), INTENT(OUT)  :: vStr
            tCmpxQP,       INTENT(IN)   :: CmpxNum
        END SUBROUTINE FvlStr_From_CQuad
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_From_Logical(vStr,Boolean)
            !^ To assign a FvlStr object from a default logical value.
            CLASS(FvlStr), INTENT(OUT)  :: vStr
            tLogical,      INTENT(IN)   :: Boolean
        END SUBROUTINE FvlStr_From_Logical
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_To_CharAlloc(cStr, vStr)
            !^ To convert a FvlStr object to an allocatable character string.
            TYPE(FvlStr), INTENT(IN)    :: vStr
            tCharAlloc,   INTENT(OUT)   :: cStr
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_To_CharArray_Alloc(cArr, vStr)
            !^ To convert a FvlStr object to an allocatable array of characters.
            TYPE(FvlStr),       INTENT(IN)  :: vStr
            tChar, ALLOCATABLE, INTENT(OUT) :: cArr(:)
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_To_IByte(IntNum,vStr)
            !^ To convert a FvlStr object to an 8-bit integer number.
            TYPE(FvlStr), INTENT(IN)    :: vStr
            tSInt8,       INTENT(OUT)   :: IntNum
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_To_IShort(IntNum,vStr)
            !^ To convert a FvlStr object to a 16-bit integer number.
            TYPE(FvlStr), INTENT(IN)    :: vStr
            tSInt16,      INTENT(OUT)   :: IntNum
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_To_Integer(IntNum,vStr)
            !^ To convert a FvlStr object to a 32-bit integer number.
            TYPE(FvlStr), INTENT(IN)    :: vStr
            tSInt32,      INTENT(OUT)   :: IntNum
        END SUBROUTINE FvlStr_To_Integer
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_To_ILong(IntNum,vStr)
            !^ To convert a FvlStr object to a 64-bit integer number.
            TYPE(FvlStr), INTENT(IN)    :: vStr
            tSInt64,      INTENT(OUT)   :: IntNum
        END SUBROUTINE FvlStr_To_ILong
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_To_RSingle(RealNum,vStr)
            !^ To convert a FvlStr object to a 32-bit real number.
            TYPE(FvlStr), INTENT(IN)    :: vStr
            tRealSP,      INTENT(OUT)   :: RealNum
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_To_RDouble(RealNum,vStr)
            !^ To convert a FvlStr object to a 64-bit real number.
            TYPE(FvlStr), INTENT(IN)    :: vStr
            tRealDP,      INTENT(OUT)   :: RealNum
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_To_RQuad(RealNum,vStr)
            !^ To convert a FvlStr object to a 128-bit real number.
            TYPE(FvlStr), INTENT(IN)    :: vStr
            tRealQP,      INTENT(OUT)   :: RealNum
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_To_CSingle(CmpxNum,vStr)
            !^ To convert a FvlStr object to a single-precision complex number.
            TYPE(FvlStr), INTENT(IN)    :: vStr
            tCmpxSP,      INTENT(OUT)   :: CmpxNum
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_To_CDouble(CmpxNum,vStr)
            !^ To convert a FvlStr object to a double-precision complex number.
            TYPE(FvlStr), INTENT(IN)    :: vStr
            tCmpxDP,      INTENT(OUT)   :: CmpxNum
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_To_CQuad(CmpxNum,vStr)
            !^ To convert a FvlStr object to a quadruple-precision complex number.
            TYPE(FvlStr), INTENT(IN)    :: vStr
            tCmpxQP,      INTENT(OUT)   :: CmpxNum
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStr_To_Logical(Boolean,vStr)
            !^ To convert a FvlStr object to a default logical value.
            TYPE(FvlStr), INTENT(IN)    :: vStr
            tLogical,     INTENT(OUT)   :: Boolean
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStrArray_From_ChrStrArray(vStr,cStr)
            !^ To assign an array of FvlStr objects from an array of character strings. <br>
            TYPE(FvlStr), INTENT(OUT)   :: vStr(:)          !! array of FvlStr objects
            tCharStar,    INTENT(IN)    :: cStr(SIZE(vStr)) !! array of character strings
        END SUBROUTINE FvlStrArray_From_ChrStrArray
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE FvlStrArray_From_ChrStr(vStr,cStr)
            !^ To assign an array of FvlStr objects from a character string. <br>
            TYPE(FvlStr), INTENT(OUT)   :: vStr(:)  !! array of FvlStr objects
            tCharStar,    INTENT(IN)    :: cStr     !! character string
        END SUBROUTINE FvlStrArray_From_ChrStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    !------------------------------------------------------------------
    !-----  Interfaces for 'Conversion/Constructor' Procedures    -----
    !------------------------------------------------------------------
    INTERFACE FvlStr
        !^ **Function Interface**: FvlStr <br>
        !  **Purpose**:  To construct a FvlStr object based on specified input. <br>
        !  **Usage**: <br>
        !   ! construct a FvlStr object from an array of characters <br>
        !   --->    vStr = FvlStr(cArray) <br>
        !   ! construct a FvlStr object from double-precision real number <br>
        !   --->    vStr = FvlStr(R64, IsScientific=.TRUE.) <br>
        MODULE FUNCTION CharacterArray_To_FvlStr(cArr,IsCString) RESULT(vStr)
            !^ To convert an array of characters to a FvlStr object.
            tChar,              INTENT(IN)  :: cArr(:)  !! array of characters
            tLogical, OPTIONAL, INTENT(IN)  :: IsCString
            !^ flag indicating whether the array is a 'C' string or not. <br>
            !  If true, the array must contain a null character. <br>
            !  Default is FALSE.
            TYPE(FvlStr)                    :: vStr     !! FvlStr object
        END FUNCTION CharacterArray_To_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION IByte_To_FvlStr(IntNum) RESULT(vStr)
            !^ To convert an 8-bit integer number to a FvlStr object.
            tSInt8, INTENT(IN)  :: IntNum   !! integer number
            TYPE(FvlStr)        :: vStr     !! FvlStr object
        END FUNCTION IByte_To_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION IShort_To_FvlStr(IntNum) RESULT(vStr)
            !^ To convert a 16-bit integer number to a FvlStr object.
            tSInt16, INTENT(IN) :: IntNum   !! integer number
            TYPE(FvlStr)        :: vStr     !! FvlStr object
        END FUNCTION IShort_To_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION Integer_To_FvlStr(IntNum) RESULT(vStr)
            !^ To convert a 32-bit integer number to a FvlStr object.
            tSInt32,  INTENT(IN)    :: IntNum   !! integer number
            TYPE(FvlStr)            :: vStr     !! FvlStr object
        END FUNCTION Integer_To_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ILong_To_FvlStr(IntNum) RESULT(vStr)
            !^ To convert a 64-bit integer number to a FvlStr object.
            tSInt64, INTENT(IN) :: IntNum   !! integer number
            TYPE(FvlStr)        :: vStr     !! FvlStr object
        END FUNCTION ILong_To_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION RSingle_To_FvlStr(RealNum, IsScientific) RESULT(vStr)
            !^ To convert a single-precision real number to a FvlStr object.
            tRealSP,            INTENT(IN)  :: RealNum  !! real number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ flag indicating whether the string is expressed in the scientific format. <br>
            !  Default is false where the string is expressed in the general format.
            TYPE(FvlStr)                    :: vStr   !! FvlStr object
        END FUNCTION RSingle_To_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION RDouble_To_FvlStr(RealNum, IsScientific) RESULT(vStr)
            !^ To convert a double-precision real number to a FvlStr object.
            tRealDP,            INTENT(IN)  :: RealNum  !! real number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ flag indicating whether the string is expressed in the scientific format. <br>
            !  Default is false where the string is expressed in the general format.
            TYPE(FvlStr)                    :: vStr     !! FvlStr object
        END FUNCTION RDouble_To_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION RQuad_To_FvlStr(RealNum, IsScientific) RESULT(vStr)
            !^ To convert a quadruple-precision real number to a FvlStr object.
            tRealQP,            INTENT(IN)  :: RealNum  !! real number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ flag indicating whether the string is expressed in the scientific format. <br>
            !  Default is false where the string is expressed in the general format.
            TYPE(FvlStr)                    :: vStr     !! FvlStr object
        END FUNCTION RQuad_To_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CSingle_To_FvlStr(CmpxNum, IsScientific) RESULT(vStr)
            !^ To convert a single-precision complex number to a FvlStr object.
            tCmpxSP,            INTENT(IN)  :: CmpxNum  !! complex number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ flag indicating whether the string is expressed in the scientific format. <br>
            !  Default is false where the string is expressed in the general format.
            TYPE(FvlStr)                      :: vStr   !! FvlStr object
        END FUNCTION CSingle_To_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CDouble_To_FvlStr(CmpxNum, IsScientific) RESULT(vStr)
            !^ To convert a double-precision complex number to a FvlStr object.
            tCmpxDP,            INTENT(IN)  :: CmpxNum  !! complex number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ flag indicating whether the string is expressed in the scientific format. <br>
            !  Default is false where the string is expressed in the general format.
            TYPE(FvlStr)                      :: vStr   !! FvlStr object
        END FUNCTION CDouble_To_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CQuad_To_FvlStr(CmpxNum, IsScientific) RESULT(vStr)
            !^ To convert a quadruple-precision complex number to a FvlStr object.
            tCmpxQP,            INTENT(IN)  :: CmpxNum  !! complex number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ flag indicating whether the string is expressed in the scientific format. <br>
            !  Default is false where the string is expressed in the general format.
            TYPE(FvlStr)                    :: vStr     !! FvlStr object
        END FUNCTION CQuad_To_FvlStr
        ! ---------------------------------------------------------------------
        MODULE FUNCTION Logical_To_FvlStr(Boolean) RESULT(vStr)
            !^ To convert a logical value to a FvlStr object.
            tLogical, INTENT(IN)    :: Boolean  !! logical value
            TYPE(FvlStr)            :: vStr     !! FvlStr object
        END FUNCTION Logical_To_FvlStr
        ! ---------------------------------------------------------------------
    END INTERFACE
    !------------------------------------------------------------------
    !-----      Interfaces for 'Operator Overload' Procedures     -----
    !------------------------------------------------------------------
    INTERFACE OPERATOR(+)
        !^ **Operator Overload**: OPERATOR(+) <br>
        !  **Purpose**:  To concatenate the first and second strings. <br>
        !  **Usage**: <br>
        !   --->    vStrOut = Str1st + Str2nd <br>
        MODULE FUNCTION Concatenate_VLS_CHS(Str1st,Str2nd) RESULT(vStrOut)
            !^ To concatenate the first and second strings.
            TYPE(FvlStr), INTENT(IN)    :: Str1st   !! the first string
            tCharStar,    INTENT(IN)    :: Str2nd   !! the second string
            TYPE(FvlStr)                :: vStrOut  !! output FvlStr object
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION Concatenate_CHS_VLS(Str1st,Str2nd) RESULT(vStrOut)
            !^ To concatenate the first and second strings.
            tCharStar,    INTENT(IN)    :: Str1st   !! the first string
            TYPE(FvlStr), INTENT(IN)    :: Str2nd   !! the second string
            TYPE(FvlStr)                :: vStrOut  !! output FvlStr object
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION Concatenate_VLS_VLS(Str1st,Str2nd) RESULT(vStrOut)
            !^ To concatenate the first and second strings.
            TYPE(FvlStr), INTENT(IN)    :: Str1st   !! the first string
            TYPE(FvlStr), INTENT(IN)    :: Str2nd   !! the second string
            TYPE(FvlStr)                :: vStrOut  !! output FvlStr object
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE OPERATOR(//)
        !^ **Operator Overload**: OPERATOR('//') <br>
        !  **Purpose**:  To concatenate the first and second strings. <br>
        !  **Usage**: <br>
        !   --->    vStrOut = Str1st '//' Str2nd <br>
        MODULE PROCEDURE Concatenate_VLS_CHS
        MODULE PROCEDURE Concatenate_CHS_VLS
        MODULE PROCEDURE Concatenate_VLS_VLS
    END INTERFACE
    INTERFACE OPERATOR(==)
        !^ **Operator Overload**: OPERATOR(==) <br>
        !  **Purpose**:  To check whether two strings are equal or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS == RHS <br>
        !   --->    IF (LHS .EQ. RHS) DoSomething
        MODULE FUNCTION VLS_EQ_CHS(lStr,rStr) RESULT(Flag)
            !^ To perform equal-to operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            tCharStar,    INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CHS_EQ_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform equal-to operation of two strings.
            tCharStar,    INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE OPERATOR(/=)
        !^ **Operator Overload**: OPERATOR(/=) <br>
        !  **Purpose**:  To check whether two strings are NOT equal or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS /= RHS <br>
        !   --->    IF (LHS .NE. RHS) DoSomething
        MODULE FUNCTION VLS_NE_CHS(lStr,rStr) RESULT(Flag)
            !^ To perform not-equal-to operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            tCharStar,    INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CHS_NE_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform not-equal-to operation of two strings.
            tCharStar,    INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE OPERATOR(>)
        !^ **Operator Overload**: OPERATOR(>) <br>
        !  **Purpose**:  To check whether LHS string is greater than RHS string or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS > RHS <br>
        !   --->    IF (LHS .GT. RHS) DoSomething
        MODULE FUNCTION VLS_GT_CHS(lStr,rStr) RESULT(Flag)
            !^ To perform greater-than operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            tCharStar,    INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CHS_GT_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform greater-than operation of two strings.
            tCharStar,    INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE OPERATOR(>=)
        !^ **Operator Overload**: OPERATOR(>=) <br>
        !  **Purpose**:  To check whether LHS string is greater than or equal to
        !       RHS string or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS >= RHS <br>
        !   --->    IF (LHS .GE. RHS) DoSomething
        MODULE FUNCTION VLS_GE_CHS(lStr,rStr) RESULT(Flag)
            !^ To perform greater-than-or-equal-to operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            tCharStar,    INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CHS_GE_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform greater-than-or-equal-to operation of two strings.
            tCharStar,    INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE OPERATOR(<)
        !^ **Operator Overload**: OPERATOR(<) <br>
        !  **Purpose**:  To check whether LHS string is less than RHS string or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS < RHS <br>
        !   --->    IF (LHS .LT. RHS) DoSomething
        MODULE FUNCTION VLS_LT_CHS(lStr,rStr) RESULT(Flag)
            !^ To perform less-than operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            tCharStar,    INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CHS_LT_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform less-than operation of two strings.
            tCharStar,    INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE OPERATOR(<=)
        !^ **Operator Overload**: OPERATOR(<=) <br>
        !  **Purpose**:  To check whether LHS string is less than or equal to
        !       RHS string or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS <= RHS <br>
        !   --->    IF (LHS .LE. RHS) DoSomething
        MODULE FUNCTION VLS_LE_CHS(lStr,rStr) RESULT(Flag)
            !^ To perform less-than-or-equal-to operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            tCharStar,    INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CHS_LE_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform less-than-or-equal-to operation of two strings.
            tCharStar,    INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    !------------------------------------------------------------------
    !-----      Interfaces for Fortran 'Intrinsic' Procedures     -----
    !------------------------------------------------------------------
    INTERFACE GETLEN
        !^ **Function Interface**: GETLEN <br>
        !  **Purpose**:  To return length of the character string of a FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    Length = GETLEN(vStr) <br>
        MODULE FUNCTION GetLength(vStr) RESULT(Length)
            !^ To return length of the character string of a FvlStr object.
            CLASS(FvlStr), INTENT(IN)   :: vStr
            tIndex                      :: Length
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE LEN_TRIM
        !^ **Function Interface**: LEN_TRIM <br>
        !  **Purpose**:  To return length of the character string of a FvlStr object
        !       without counting trailing blank characters. <br>
        !  **Usage**: <br>
        !   --->    Length = LEN_TRIM(vStr) <br>
        MODULE FUNCTION GetLengthTrim(vStr) RESULT(Length)
            !^ To return length of the character string of a FvlStr object
            !  without counting trailing blank characters.
            TYPE(FvlStr), INTENT(IN)    :: vStr
            tIndex                      :: Length
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE INDEX
        !^ **Function Interface**: INDEX <br>
        !  **Purpose**:  To return the starting position of a substring within the string
        !       of a FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    Indx = INDEX(vStr, sStr) <br>
        !   --->    Indx = INDEX(vStr, sStr, Back=.TRUE.) <br>
        MODULE FUNCTION FindIndex_CHS(vStr,sStr,Back) RESULT(Indx)
        !^ To return the starting position of a substring within the string of a FvlStr object.
        TYPE(FvlStr),       INTENT(IN)  :: vStr     !! FvlStr object
        tCharStar,          INTENT(IN)  :: sStr     !! substring
        tLogical, OPTIONAL, INTENT(IN)  :: Back
        !^ If present and true, searching from the back; otherwise, searching from the front.
        tIndex                          :: Indx     !! starting position of a substring
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION FindIndex_VLS(vStr,sStr,Back) RESULT(Indx)
            !^ See the "FindIndex_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *sStr* argument. <br>
            TYPE(FvlStr),       INTENT(IN)  :: vStr
            TYPE(FvlStr),       INTENT(IN)  :: sStr
            tLogical, OPTIONAL, INTENT(IN)  :: Back
            tIndex                          :: Indx
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE SCAN
        !^ **Function Interface**: SCAN <br>
        !  **Purpose**:  To scan the string of a FvlStr object for any character in a set
        !       of characters and return the position of the first character found in the
        !       string that is in the specified set depending on the scanning direction. <br>
        !  **Usage**: <br>
        !   --->    Pos = SCAN(vStr, ChrSet) <br>
        !   --->    Pos = SCAN(vStr, ChrSet, Back=.TRUE.) <br>
        MODULE FUNCTION ScanCharacters_CHS(vStr,ChrSet,Back) RESULT(Pos)
            !^ To scan the string of a FvlStr object for any character in a set of
            !  characters and return the position of the first character found in the
            !  string that is in the specified set depending on the scanning direction.
            TYPE(FvlStr),       INTENT(IN)  :: vStr     !! FvlStr object
            tCharStar,          INTENT(IN)  :: ChrSet   !! a set of characters
            tLogical, OPTIONAL, INTENT(IN)  :: Back
            !^ If present and true, scanning from the back; otherwise, scanning from the front.
            tIndex                          :: Pos      !! position of the first character found
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ScanCharacters_VLS(vStr,ChrSet,Back) RESULT(Pos)
            !^ See the "ScanCharacters_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *ChrSet* argument. <br>
            TYPE(FvlStr),       INTENT(IN)  :: vStr
            TYPE(FvlStr),       INTENT(IN)  :: ChrSet
            tLogical, OPTIONAL, INTENT(IN)  :: Back
            tIndex                          :: Pos
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE VERIFY
        !^ **Function Interface**: VERIFY <br>
        !  **Purpose**:  To verify that a set of characters contains all the characters
        !       in the string of a FvlStr object by identifying the first character in
        !       the string that is not in the set and to return the position of the
        !       first character found in the string that is NOT in the specified set
        !       depending on the scanning direction. <br>
        !  **Usage**: <br>
        !   --->    Pos = VERIFY(vStr, ChrSet) <br>
        !   --->    Pos = VERIFY(vStr, ChrSet, Back=.TRUE.) <br>
        MODULE FUNCTION VerifyCharacters_CHS(vStr,ChrSet,Back) RESULT(Pos)
            !^ To verify that a set of characters contains all the characters in
            !  the string of a FvlStr object by identifying the first character
            !  in the string that is not in the set and to return the position
            !  of the first character found in the string that is NOT in the
            !  specified set depending on the scanning direction.  If all characters
            !  of string are in the specified set or the length of string is zero,
            !  the returned value is zero.
            TYPE(FvlStr),       INTENT(IN)  :: vStr     !! FvlStr object
            tCharStar,          INTENT(IN)  :: ChrSet   !! a set of characters
            tLogical, OPTIONAL, INTENT(IN)  :: Back
            !^ If present and true, scanning from the back; otherwise, scanning from the front.
            tIndex                          :: Pos      !! position of the first character found
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION VerifyCharacters_VLS(vStr,ChrSet,Back) RESULT(Pos)
            !^ See the "VerifyCharacters_CHS" procedure, where the only difference
            !  between these two procedures is the type of the *ChrSet* argument. <br>
            TYPE(FvlStr),       INTENT(IN)  :: vStr
            TYPE(FvlStr),       INTENT(IN)  :: ChrSet
            tLogical, OPTIONAL, INTENT(IN)  :: Back
            tIndex                          :: Pos
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ADJUSTL
        !^ **Function Interface**: ADJUSTL <br>
        !  **Purpose**:  To adjust the string of FvlStr object to the left, removing
        !       leading blanks and inserting trailing blanks. <br>
        !  **Usage**: <br>
        !   --->    vStrOut = ADJUSTL(vStrIn) <br>
        MODULE FUNCTION AdjustToLeft(vStrIn) RESULT(vStrOut)
            !^ To adjust the string of FvlStr object to the left, removing leading
            !  blanks and inserting trailing blanks.
            TYPE(FvlStr), INTENT(IN)    :: vStrIn
            TYPE(FvlStr)                :: vStrOut
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ADJUSTR
        !^ **Function Interface**: ADJUSTR <br>
        !  **Purpose**:  To adjust the string of FvlStr object to the right, removing
        !       trailing blanks and inserting leading blanks. <br>
        !  **Usage**: <br>
        !   --->    vStrOut = ADJUSTR(vStrIn) <br>
        MODULE FUNCTION AdjustToRight(vStrIn) RESULT(vStrOut)
            !^ To adjust the string of FvlStr object to the right, removing trailing
            !  blanks and inserting leading blanks.
            TYPE(FvlStr), INTENT(IN) :: vStrIn
            TYPE(FvlStr)             :: vStrOut
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE TRIM
        !^ **Function Interface**: TRIM <br>
        !  **Purpose**:  To return the argument with trailing blanks removed. <br>
        !  **Usage**: <br>
        !   --->    vStrOut = TRIM(vStrIn) <br>
        MODULE FUNCTION TrimFvlStr(vStrIn) RESULT(vStrOut)
            !^ To return the argument with trailing blanks removed.
            TYPE(FvlStr), INTENT(IN)    :: vStrIn
            TYPE(FvlStr)                :: vStrOut
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE REPEAT
        !^ **Function Interface**: REPEAT <br>
        !  **Purpose**:  To concatenate several copies of the string of the specified
        !       FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    vStrOut = REPEAT(vStrIn, nCopies) <br>
        MODULE FUNCTION RepeatString(vStrIn,nCopies) RESULT(vStrOut)
            !^ To concatenate several copies of the specified string.
            TYPE(FvlStr), INTENT(IN)    :: vStrIn   !! input FvlStr object to be repeated
            tIndex,       INTENT(IN)    :: nCopies  !! number of copies
            TYPE(FvlStr)                :: vStrOut  !! output FvlStr object
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE LGT
        !^ **Function Interface**: LGT <br>
        !  **Purpose**:  To check whether LHS string is lexically greater than
        !       RHS string or not, based on the ASCII collating sequence, even
        !       if the compiler's default collating sequence is different. <br>
        !  **Usage**: <br>
        !   --->    Flag = LGT(LHS, RHS) <br>
        !   --->    IF (.NOT.LGT(LHS, RHS)) DoSomething
        MODULE FUNCTION VLS_LGT_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform greater-than operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION VLS_LGT_CHS(lStr,rStr) RESULT(Flag)
            !^ To perform greater-than operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            tCharStar,    INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CHS_LGT_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform greater-than operation of two strings.
            tCharStar,    INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE LGE
        !^ **Function Interface**: LGE <br>
        !  **Purpose**:  To check whether LHS string is lexically greater than or equal
        !       to RHS string or not, based on the ASCII collating sequence, even if the
        !       compiler's default collating sequence is different. <br>
        !  **Usage**: <br>
        !   --->    Flag = LGE(LHS, RHS) <br>
        !   --->    IF (.NOT.LGE(LHS, RHS)) DoSomething
        MODULE FUNCTION VLS_LGE_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform greater-than-or-equal-to operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION VLS_LGE_CHS(lStr,rStr) RESULT(Flag)
            !^ To perform greater-than-or-equal-to operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            tCharStar,    INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CHS_LGE_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform greater-than-or-equal-to operation of two strings.
            tCharStar,    INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE LLT
        !^ **Function Interface**: LLT <br>
        !  **Purpose**:  To check whether LHS string is lexically less than
        !       RHS string or not, based on the ASCII collating sequence, even
        !       if the compiler's default collating sequence is different. <br>
        !  **Usage**: <br>
        !   --->    Flag = LLT(LHS, RHS) <br>
        !   --->    IF (.NOT.LLT(LHS, RHS)) DoSomething
        MODULE FUNCTION VLS_LLT_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform less-than operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION VLS_LLT_CHS(lStr,rStr) RESULT(Flag)
            !^ To perform less-than operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            tCharStar,    INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CHS_LLT_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform less-than operation of two strings.
            tCharStar,    INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE LLE
        !^ **Function Interface**: LLE <br>
        !  **Purpose**:  To check whether LHS string is lexically less than or equal
        !       to RHS string or not, based on the ASCII collating sequence, even if the
        !       compiler's default collating sequence is different. <br>
        !  **Usage**: <br>
        !   --->    Flag = LLE(LHS, RHS) <br>
        !   --->    IF (.NOT.LLE(LHS, RHS)) DoSomething
        MODULE FUNCTION VLS_LLE_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform less-than-or-equal-to operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION VLS_LLE_CHS(lStr,rStr) RESULT(Flag)
            !^ To perform less-than-or-equal-to operation of two strings.
            TYPE(FvlStr), INTENT(IN)    :: lStr
            tCharStar,    INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION CHS_LLE_VLS(lStr,rStr) RESULT(Flag)
            !^ To perform less-than-or-equal-to operation of two strings.
            tCharStar,    INTENT(IN)    :: lStr
            TYPE(FvlStr), INTENT(IN)    :: rStr
            tLogical                    :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ACHAR
        !^ **Function Interface**: ACHAR <br>
        !  **Purpose**:  To return a FvlStr object with length of 1 representing the
        !       character in the specified position of the ASCII character set. <br>
        !  **Usage**: <br>
        !   --->    Chr = ACHAR(I) <br>
        MODULE FUNCTION Get_ACHAR(I) RESULT(vStr)
        !^ To get the character in the specified position of the ASCII character set.
        tSInt32,  INTENT(IN)    :: I    !! character code (position in the set)
        TYPE(FvlStr)            :: vStr !! FvlStr object with length of 1
       END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE CHAR
        !^ **Function Interface**: CHAR <br>
        !  **Purpose**:  To return a FvlStr object with length of 1 representing the
        !       character in the specified position of the compiler's character set. <br>
        !  **Usage**: <br>
        !   --->    Chr = CHAR(I) <br>
        MODULE FUNCTION Get_CHAR(I) RESULT(vStr)
            !^ To get the character in the specified position of the processor's character set.
        tSInt32,  INTENT(IN)    :: I    !! character code (position in the set)
        TYPE(FvlStr)            :: vStr !! FvlStr object with length of 1
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE IACHAR
        !^ **Function Interface**: IACHAR <br>
        !  **Purpose**:  To return the character code based on the ASCII character set
        !       of the specified character of the FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    I = IACHAR(vStr, Pos) <br>
        !  **Note**: A character code is a position of the character in a character set. <br>
        MODULE FUNCTION Get_IACHAR(vStr, Pos) RESULT(I)
            !^ To get the character code based on the ASCII character set of the specified
            !  character.
            TYPE(FvlStr), INTENT(IN)    :: vStr !! FvlStr object
            tSInt32,      INTENT(IN)    :: Pos  !! index specifying the FvlStr's character
            tSInt32                     :: I
            !^ the requested character code (position in the character set);
            !  I = -1 if invalid set of input is given.
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ICHAR
        !^ **Function Interface**: ICHAR <br>
        !  **Purpose**:  To return the character code based on the compiler's character set
        !       of the specified character of the FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    I = ICHAR(vStr, Pos) <br>
        !  **Note**: A character code is a position of the character in a character set. <br>
        MODULE FUNCTION Get_ICHAR(vStr, Pos) RESULT(I)
            !^ To get the character code based on the processor's character set of the specified
            !  character.
            TYPE(FvlStr), INTENT(IN)    :: vStr !! FvlStr object
            tSInt32,      INTENT(IN)    :: Pos  !! index specifying the FvlStr's character
            tSInt32                     :: I
            !^ the requested character code (position in the character set);
            !  I = -1 if invalid set of input is given.
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    !------------------------------------------------------------------
    !-----      Interfaces for 'User-Defined IO' Procedures       -----
    !------------------------------------------------------------------
    INTERFACE WRITE(UNFORMATTED)
        !^ **Subroutine Interface**: WRITE(UNFORMATTED) <br>
        !  **Purpose**:  To write a character string of the FvlStr object to a connected
        !       unformatted unit. <br>
        !  **Usage**: <br>
        !   --->    WRITE(UNIT=IOUnit) vStr <br>
        MODULE SUBROUTINE Write_Unformatted(vStr,IOUnit,IOStat,IOMsg)
            !^ To write a character string of the FvlStr object to a connected unformatted unit.
            CLASS(FvlStr), INTENT(IN)       :: vStr     !! FvlStr object
            tSInt32,       INTENT(IN)       :: IOUnit   !! connected io unit number
            tSInt32,       INTENT(OUT)      :: IOStat   !! status of io operation
            tCharStar,     INTENT(INOUT)    :: IOMsg    !! an io message if is IOStat is non-zero
        END SUBROUTINE Write_Unformatted
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE READ(UNFORMATTED)
        !^ **Subroutine Interface**: READ(UNFORMATTED) <br>
        !  **Purpose**:  To read a character sequence from a connected unformatted unit
        !       into the FvlStr object. <br>
        !  **Usage**: <br>
        !   --->    READ(UNIT=IOUnit) vStr <br>
        MODULE SUBROUTINE Read_Unformatted(vStr,IOUnit,IOStat,IOMsg)
            !^ To read a character sequence from a connected unformatted unit into the FvlStr object.
            CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
            tSInt32,       INTENT(IN)       :: IOUnit   !! connected io unit number
            tSInt32,       INTENT(OUT)      :: IOStat   !! status of io operation
            tCharStar,     INTENT(INOUT)    :: IOMsg    !! an io message if is IOStat is non-zero
        END SUBROUTINE Read_Unformatted
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE WRITE(FORMATTED)
        !^ **Subroutine Interface**: WRITE(FORMATTED) <br>
        !  **Purpose**:  To write a character string of the FvlStr object to a connected
        !       formatted unit. <br>
        !  **Usage**: <br>
        !   ! using the list-directed format output <br>
        !   --->    WRITE(UNIT=IOUnit, FMT=*) vStr <br>
        !   --->    WRITE(UNIT=IOUnit, FMT=*, IOSTAT=IOStat, IOMSG=IOMsg) vStr <br>
        !   ! using the DT edit descriptor <br>
        !   --->    WRITE(UNIT=IOUnit, FMT='(DT)') vStr <br>
        !   --->    WRITE(UNIT=IOUnit, FMT='(DT)', IOSTAT=IOStat, IOMSG=IOMsg) vStr <br>
        MODULE SUBROUTINE Write_Formatted(vStr,IOUnit,IOType,VList,IOStat,IOMsg)
            !^ To write a character string of the FvlStr object to a connected formatted unit.
            CLASS(FvlStr), INTENT(IN)       :: vStr     !! FvlStr object
            tSInt32,       INTENT(IN)       :: IOUnit   !! connected io unit number
            tCharStar,     INTENT(IN)       :: IOType   !! type of io
            tSInt32,       INTENT(IN)       :: VList(:) !! list of integer array from part of the DT edit descriptor
            tSInt32,       INTENT(OUT)      :: IOStat   !! status of io operation
            tCharStar,     INTENT(INOUT)    :: IOMsg    !! an io message if is IOStat is non-zero
        END SUBROUTINE Write_Formatted
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE READ(FORMATTED)
        !^ **Subroutine Interface**: READ(FORMATTED) <br>
        !  **Purpose**:  To read a character sequence from a connected formatted unit
        !       into the FvlStr object. <br>
        !  **Usage**: <br>
        !   ! using the list-directed format input <br>
        !   --->    READ(UNIT=IOUnit, FMT=*) vStr <br>
        !   --->    READ(UNIT=IOUnit, FMT=*, IOSTAT=IOStat, IOMSG=IOMsg) vStr <br>
        !   ! using the DT edit descriptor <br>
        !   --->    READ(UNIT=IOUnit, FMT='(DT)') vStr <br>
        !   --->    READ(UNIT=IOUnit, FMT='(DT)', IOSTAT=IOStat, IOMSG=IOMsg) vStr <br>
        MODULE SUBROUTINE Read_Formatted(vStr,IOUnit,IOType,VList,IOStat,IOMsg)
            !^ To read a character sequence from a connected formatted unit into the FvlStr object.
            CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
            tSInt32,       INTENT(IN)       :: IOUnit   !! connected io unit number
            tCharStar,     INTENT(IN)       :: IOType   !! type of io
            tSInt32,       INTENT(IN)       :: VList(:) !! list of integer array from part of the DT edit descriptor
            tSInt32,       INTENT(OUT)      :: IOStat   !! status of io operation
            tCharStar,     INTENT(INOUT)    :: IOMsg    !! an io message if is IOStat is non-zero
        END SUBROUTINE Read_Formatted
        ! ---------------------------------------------------------------------
    END INTERFACE
    !------------------------------------------------------------------
    !-----      Interfaces for 'Miscellaneous' Procedures         -----
    !------------------------------------------------------------------
    INTERFACE Swap
        !^ **Subroutine Interface**: Swap <br>
        !  **Purpose**:  To swap values of two FvlStr objects or two arrays
        !       of FvlStr objects. <br>
        !  **Usage**: <br>
        !   --->    CALL Swap(vStrA, vStrB) <br>
        !   --->    CALL Swap(vStrArrA, vStrArrB) <br>
        MODULE SUBROUTINE SwapFvlStr(AVal,BVal)
            !^ To swap values of two FvlStr objects.
            TYPE(FvlStr), INTENT(INOUT)  :: AVal    !! value of A
            TYPE(FvlStr), INTENT(INOUT)  :: BVal    !! value of B
        END SUBROUTINE SwapFvlStr
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE SwapFvlStrArray(AVal,BVal)
            !^ To swap values of two arrays of FvlStr objects.
            TYPE(FvlStr), INTENT(INOUT)  :: AVal(:)  !! array A of FvlStr objects
            TYPE(FvlStr), INTENT(INOUT)  :: BVal(:)  !! array B of FvlStr objects
        END SUBROUTINE SwapFvlStrArray
        ! ---------------------------------------------------------------------
    END INTERFACE
    INTERFACE ToCharStar
        !^ **Subroutine Interface**: ToCharStar <br>
        !  **Purpose**:  To convert a FvlStr object to an assumed-length character string.  <br>
        !  **Usage**: <br>
        !   --->    CALL ToCharStar(vStr, cStr) <br>
        !  **Important Note**:  This procedure is intended to be used internally only. <br>
        MODULE PROCEDURE CharStar_From_FvlStr
    END INTERFACE
    INTERFACE PtrToStr
        !^ **Function Interface**: PtrToStr <br>
        !  **Purpose**:  To set a pointer to the string of the FvlStr object.  <br>
        !  **Usage**: <br>
        !   --->    StrPtr => PtrToStr(vStr) <br>
        !   --->    IF (.NOT.ASSOCIATED(PtrToStr(vStr))) DoSomething <br>
        !  **Important Note**:  This procedure is intended to be used internally only. <br>
        MODULE PROCEDURE Pointer_To_FvlStr
    END INTERFACE
    INTERFACE IsReady
        !^ **Function Interface**: IsReady <br>
        !  **Purpose**:  To check whether the FvlStr object is ready to be used or not.
        !       Return true if the object's string is allocated.  Otherwise, return false.  <br>
        !  **Usage**: <br>
        !   --->    Flag = IsReady(vStr) <br>
        !   --->    IF (.NOT.IsReady(vStr)) DoSomething <br>
        MODULE PROCEDURE Is_FvlStr_Ready
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!--------------------------------------------------------------------------------------
!               DEFERRED-BINDING PROCEDURES OF HASHABLE CLASS
!--------------------------------------------------------------------------------------

SUBROUTINE FvlStr_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform copy of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(IN)  :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
    !^ Flag indicating whether to perform deep copy or shallow copy. <br>
    !  - If present and true, perform a deep copy. <br>
    !  - If present and false, perform a shallow copy. <br>
    !  - If not present, perform either a shallow or a deep copy that is naturally most
    !    suitable for the object's components.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT TYPE (DstObj)
    TYPE IS (FvlStr)
        IF (ALLOCATED(SrcObj%cStr)) THEN
            ALLOCATE(DstObj%cStr, SOURCE=SrcObj%cStr)
        ELSE
            ALLOCATE(tCharLen(0) :: DstObj%cStr)
        END IF
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('FvlStr_Copy', ModName, ErrSevere, 'Type of DstObj is invalid.')
    END SELECT

    RETURN

END SUBROUTINE FvlStr_Copy

!**************************************************************************************

FUNCTION FvlStr_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: LhsObj       !! an object
    CLASS(Object), INTENT(IN)   :: RhsObj       !! another object
    tLogical                    :: Flag         !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (FvlStr)
        Flag = (LhsObj%cStr == RhsObj%cStr)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION FvlStr_IsEqualTo

!******************************************************************************

SUBROUTINE FvlStr_MemFree(Obj)

!** PURPOSE OF THIS ROUTINE:
    !^ To free memory of the FvlStr object.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(INOUT)    :: Obj  ! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Obj%cStr)) DEALLOCATE(Obj%cStr)

    RETURN

END SUBROUTINE FvlStr_MemFree

!******************************************************************************

FUNCTION FvlStr_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS ROUTINE:
    !^ To get the string representation of this object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: Obj
    tCharAlloc                  :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(Obj%cStr)) THEN
        Str = '{FlvStr : ' // Obj%cStr // '}'
    ELSE
        Str = '{FlvStr : NULL}'
    END IF

    RETURN

END FUNCTION FvlStr_ToString

!**************************************************************************************

FUNCTION FvlStr_CompareTo(A, B) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To compare a FvlStr object with a Comparable object.
    !  This is a deferred procedure inherited from the *Comparable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),     INTENT(IN)   :: A    !! FvlStr object
    CLASS(Comparable), INTENT(IN)   :: B    !! Comparable object
    tSInt32                         :: Flag
    !^ output flag with value of <br>
    !   -1 if A < B, <br>
    !    0 if A == B, or <br>
    !   +1 if A > B.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT TYPE (B)
    TYPE IS (FvlStr)
        IF (.NOT.ALLOCATED(A%cStr)) THEN
            Flag = -3
            CALL Handle_ErrLevel('FvlStr_CompareTo', ModName, ErrSevere, &
                    'The string of "A" is NOT yet ready to be used.')
        ELSEIF (.NOT.ALLOCATED(B%cStr)) THEN
            Flag = -3
            CALL Handle_ErrLevel('FvlStr_CompareTo', ModName, ErrSevere, &
                    'The string of "B" is NOT yet ready to be used.')
        ELSE
            IF (A%cStr > B%cStr) THEN
                Flag = 1
            ELSEIF (A%cStr < B%cStr) THEN
                Flag = -1
            ELSE
                Flag = 0
            END IF
        END IF
    CLASS DEFAULT
        Flag = -2
        CALL Handle_ErrLevel('FvlStr_CompareTo', ModName, ErrSevere, 'Type of B is invalid.')
    END SELECT

    RETURN

END FUNCTION FvlStr_CompareTo

!******************************************************************************

FUNCTION FvlStr_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS ROUTINE:
    !^ To get compute hash code for the given key.
    !  This is a deferred procedure inherited from the *Hashable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: Obj
    tHash                       :: Code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: KeySize
    tChar       :: Chr

! FLOW

    IF (ALLOCATED(Obj%cStr)) THEN
        Chr = Obj%cStr(1:1)
        KeySize = C_SIZEOF(Chr)*LEN(Obj%cStr)
        Code = ComputeHash(Obj%cStr, KeySize, HashSeed, RemoveSign=TrueVal)
    ELSE
        Code = 0_kIndex
    END IF

    RETURN

END FUNCTION FvlStr_HashCode

!--------------------------------------------------------------------------------------
!                           FINAL PROCEDURE
!--------------------------------------------------------------------------------------

SUBROUTINE FinalizeFvlStr(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To finalize the FvlStr object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(INOUT) :: vStr !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL vStr%MemFree()

    RETURN

END SUBROUTINE FinalizeFvlStr

!******************************************************************************

SUBROUTINE CharStar_From_FvlStr(vStr, cStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to an assumed-length character string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: vStr !! FvlStr object
    tCharStar,     INTENT(OUT)  :: cStr !! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinLen

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        MinLen = MIN(LEN(cStr), LEN(vStr%cStr))
        cStr(1:MinLen) = vStr%cStr(1:MinLen)
    ELSE
        cStr = ''
    END IF

    RETURN

END SUBROUTINE CharStar_From_FvlStr

!******************************************************************************

FUNCTION Pointer_To_FvlStr(vStr) RESULT(pStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To set a pointer to the string of the FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), TARGET, INTENT(IN)   :: vStr !! FvlStr object
    tCharLen(:),   POINTER              :: pStr !! pointer to object's string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        pStr => vStr%cStr
    ELSE
        pStr => NULL()
    END IF

    RETURN

END FUNCTION Pointer_To_FvlStr

!******************************************************************************

FUNCTION Is_FvlStr_Ready(vStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To check whether the string of the FvlStr object is allocated or not.
    !  If allocated, return true.  Otherwise, return false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: vStr !! FvlStr object
    tLogical                    :: Flag !! flag indicating whether the string is allocated or not.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        Flag = TrueVal
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION Is_FvlStr_Ready

!******************************************************************************

END MODULE MClass_FvlStr

!******************************************************************************
