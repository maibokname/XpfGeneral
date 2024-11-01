
MODULE MClass_Alphabets

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various *alphabet* data types and its related routines.
!   An *alphabet* data type is a data type for a given alphabet, which is a set
!   of characters.  It is intended to be used with string-processing code that
!   must convert between an alphabet of size Radix and the integers (i.e. indices)
!   through 1 through Radix. <br>
!   The following *concrete alphabet* data types are provided. <br>
!   - The *GenericAlphabet* type for any alphabet (i.e. any set of characters). <br>
!   - The *BinaryAlphabet* type for binary alphabet {0, 1}. <br>
!   - The *OctalAlphabet* type for octal alphabet {0, 1, 2, 3, 4, 5, 6, 7}. <br>
!   - The *DecimalAlphabet* type for decimal alphabet {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}. <br>
!   - The *HexadecimalAlphabet* type for hexadecimal alphabet
!     {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F}. <br>
!   - The *DnaAlphabet* type for DNA alphabet {A, C, T, G}. <br>
!   - The *ProteinAlphabet* type for protein alphabet
!     {A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y}. <br>
!   - The *LowercaseAlphabet* type for lowercase alphabet {a, b, c, ..., z}. <br>
!   - The *UppercaseAlphabet* type for uppercase alphabet {A, B, C, ..., Z}. <br>
!   - The *Base64Alphabet* type for base-64 alphabet {64 characters}. <br>
!   - The *Code39Alphabet* type for binary alphabet {43+1 characters}. <br>
!   - The *AsciiAlphabet* type for ASCII alphabet {128 characters}. <br>
!   - The *ExtendedAsciiAlphabet* type for extended ASCII alphabet {256 characters}. <br>
!   All *alphabet* data types provide common operations including: <br>
!   - the *Construct* method to create the instance of the alphabet data type, <br>
!   - the *Contain* method to check whether the specified character is in the alphabet's
!     set of characters, <br>
!   - the *Verify* method to check whether all characters of the specified word are in
!     the alphabet's set of characters, <br>
!   - the *GetRadix* method to get the number of characters in the alphabet's set, <br>
!   - the *GetIndex* method to get the index corresponding to the specified character, and <br>
!   - the *GetChar* method to get the character corresponding to the specified index. <br>
!   All *concrete alphabet* data types provided require an explicit construction before
!   other operations can be used.  The *GenericAlphabet* type can be constructed from
!   either a given set of characters or a specified radix (which represents a number of
!   characters in the set where the characters's code ranges from 0 to radix-1).  Other
!   types requires no argument (besides the object itself) for their construction method. <br>

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** USE STATEMENTS:
    USE, INTRINSIC  :: ISO_FORTRAN_ENV, ONLY: CHARACTER_STORAGE_SIZE
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_SIntUtil,                 ONLY: ToChar => ToDecStrSigned
#ifdef Indx64Bits
    USE MBase_SimpleHash64,             ONLY: ComputeHash => Hash64_DJB
#else
    USE MBase_SimpleHash32,             ONLY: ComputeHash => Hash32_DJB
#endif
    USE MClass_Object
    USE MClass_Comparable
    USE MClass_IntrusiveBSTrees
    USE MClass_IntrusiveHashTree
    USE MClass_FvlStr

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: BaseAlphabet
    PUBLIC :: GenericAlphabet
    PUBLIC :: BinaryAlphabet
    PUBLIC :: OctalAlphabet
    PUBLIC :: DecimalAlphabet
    PUBLIC :: HexadecimalAlphabet
    PUBLIC :: DnaAlphabet
    PUBLIC :: ProteinAlphabet
    PUBLIC :: LowercaseAlphabet
    PUBLIC :: UppercaseAlphabet
    PUBLIC :: Base64Alphabet
    PUBLIC :: Code39Alphabet
    PUBLIC :: AsciiAlphabet
    PUBLIC :: ExtendedAsciiAlphabet

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'Class_Alphabet'
    tIndex,    PARAMETER    :: MsgLen  = 128_kIndex
    tIndex,    PARAMETER    :: MaxRadix = 256_kIndex    ! extended ASCII
    ! The number of bits used by 8-bit integer
    tSInt32,   PARAMETER    :: Bits_kInt8 = BIT_SIZE(0_kInt8)    ! should be  8 bits
    ! The numbers of bits/bytes used by a character
    tSInt32,   PARAMETER    :: Bits_Char  = CHARACTER_STORAGE_SIZE
    tIndex,    PARAMETER    :: Bytes_Char = ToIndex(Bits_Char/Bits_kInt8)
    !----------------------------------------------------------------------------
    !                           Common Sets of Characters
    !----------------------------------------------------------------------------
    !% The binary alphabet [0, 1].
    tCharParam  :: BinaryCharset      = '01'
    !% The octal alphabet [0, 1, 2, 3, 4, 5, 6, 7].
    tCharParam  :: OctalCharset       = '01234567'
    !% The decimal alphabet [0, 1, 2, 3, 4, 5, 6, 7, 8, 9].
    tCharParam  :: DecimalCharset     = '0123456789'
    !% The hexadecimal alphabet [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F].
    tCharParam  :: HexadecimalCharset = '0123456789ABCDEF'
    !% The DNA alphabet [A, C, T, G]
    tCharParam  :: DnaCharset         = 'ACGT'
    !% The protein alphabet [A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y].
    tCharParam  :: ProteinCharset     = 'ACDEFGHIKLMNPQRSTVWY'
    !% The lowercase alphabet [a, b, c, ..., z].
    tCharParam  :: LowercaseCharset   = 'abcdefghijklmnopqrstuvwxyz'
    !% The uppercase alphabet [A, B, C, ..., Z].
    tCharParam  :: UppercaseCharset   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    !% The base-64 alphabet (64 characters)
    tCharParam  :: Base64Charset      = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
    !% The Code 39 alphabet (43+1 characters).
    tCharParam  :: Code39Charset      = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ* -$%./+'

!** DERIVED TYPE DEFINITIONS
    !> The *BaseAlphabet* type is a data type in the *HashTreeNode* class representing
    !  a key-value pair.  It is intended to be used in conjunction with the *BaseAlphabet*
    !  type.  This is a private type.
    TYPE, EXTENDS(HashTreeNode) :: CharIndex
        tChar   :: Char
        tIndex  :: Index
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----     Deferred/Overridden Procedures from Object Type       -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Copy <br>
        !  **Purpose**: To make a copy of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%Copy(DstObj) <br>
        PROCEDURE   :: Copy         => CharIndex_Copy
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => CharIndex_IsEqualTo
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**: To free memory/storage occupied by the object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree() <br>
        PROCEDURE   :: MemFree      => CharIndex_MemFree
        !> **Type-Bound Function**: ToString <br>
        !  **Purpose**:  To return the string representation of this object. <br>
        !  **Usage**: <br>
        !   --->    Str = Obj%ToString() <br>
        PROCEDURE   :: ToString     => CharIndex_ToString
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute the hash code of this object. <br>
        !  **Usage**: <br>
        !   --->    Code = Obj%HashCode() <br>
        PROCEDURE   :: HashCode     => CharIndex_HashCode
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Comparable Type          -----
        ! ---------------------------------------------------------------------
        !> Use a common logical expression to compare two *Comparable* objects.
        PROCEDURE   :: CompareTo    => CharIndex_CompareTo
        ! ---------------------------------------------------------------------
    END TYPE
    !> The *BaseAlphabet* type is an abstract data type for alphabets intended to
    !  be used with string-processing code that must convert between an alphabet
    !  of size R and the integers through 1 through R.
    TYPE, ABSTRACT :: BaseAlphabet
        PRIVATE
        tIndex                          :: Radix        !! the radix of the alphabet
        TYPE(CharIndex), ALLOCATABLE    :: KeyVal(:)    !! the character-index pairs of the alphabet
        TYPE(IntrusiveHashTree)         :: HashMap      !! map of the character-index pairs
    CONTAINS
        PROCEDURE, PRIVATE  :: FromCharSet  => Alphabet_FromCharSet
        PROCEDURE, PRIVATE  :: FromRadix    => Alphabet_FromRadix
        PROCEDURE, PRIVATE  :: Finalize     => Alphabet_Finalize
        PROCEDURE, PRIVATE  :: Alphabet_Verify_ChrStr
        PROCEDURE, PRIVATE  :: Alphabet_Verify_FvlStr
        !> **Type-Bound Function**: IsReady <br>
        !  **Purpose**: To check whether the Alphabet object is ready to be used. <br>
        !  **Usage**: <br>
        !   --->    Flag = Alphabet%IsReady() <br>
        !   --->    IF (.NOT.Alphabet%IsReady()) DoSomething
        PROCEDURE   :: IsReady      => Alphabet_IsReady
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**: To check whether the Alphabet object contains the specified character or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Alphabet%Contain(Chr) <br>
        !   --->    IF (.NOT.Alphabet%Contain(Chr)) DoSomething
        PROCEDURE   :: Contain      => Alphabet_Contain
        !> **Type-Bound Function**: Verify <br>
        !  **Purpose**: To verify that all characters in the specified word are in the set of
        !               characters of this alphabet.  Return 0 if they are.  Otherwise, return
        !               a number indicating the first position (index) of the character that
        !               is not in the set. <br>
        !  **Usage**: <br>
        !   --->    Flag = Alphabet%Verify(Word) <br>
        !   --->    IF (Alphabet%Verify(Word) /= 0) DoSomething
        GENERIC     :: Verify       => Alphabet_Verify_ChrStr, Alphabet_Verify_FvlStr
        !> **Type-Bound Function**: GetRadix <br>
        !  **Purpose**: To return the number of characters in the Alphabet object. <br>
        !  **Usage**: <br>
        !   --->    Radix = Alphabet%GetRadix() <br>
        PROCEDURE   :: GetRadix     => Alphabet_Radix
        !> **Type-Bound Function**: GetIndex <br>
        !  **Purpose**: To return the index corresponding to the specified character.  If the
        !               character is not in the Alphabet object, return -1. <br>
        !  **Usage**: <br>
        !   --->    Index = Alphabet%GetIndex() <br>
        PROCEDURE   :: GetIndex     => Alphabet_ToIndex
        !> **Type-Bound Function**: GetChar <br>
        !  **Purpose**: To return the character corresponding to the specified index.  If the
        !               index is not in the valid range (1 to Radix), return an empty character. <br>
        !  **Usage**: <br>
        !   --->    Chr = Alphabet%GetChar() <br>
        PROCEDURE   :: GetChar      => Alphabet_ToChar
    END TYPE BaseAlphabet
    !> The *GenericAlphabet* type is a generic data type that can represent any alphabets.
    !  An instance of the *GenericAlphabet* type can be created via the *Construct* method
    !  from either a given set of characters or a radix (representing a number of characters
    !  in the set where the characters' code are from 0 to radix-1).
    TYPE, EXTENDS(BaseAlphabet) :: GenericAlphabet
    CONTAINS
        PROCEDURE, PRIVATE  :: GenericAlphabet_FromCharSet
        PROCEDURE, PRIVATE  :: GenericAlphabet_FromRadix
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**: To create this Alphabet object either from a specified set of characters
        !               or from the radix (which is the number of characters in the set where
        !               characters' code ranges from 0 to radix-1). <br>
        !  **Usage**: <br>
        !   --->    CALL Alphabet%Construct(CharSet) <br>
        !   --->    CALL Alphabet%Construct(Radix) <br>
        GENERIC     :: Construct    => GenericAlphabet_FromCharSet, &
                                       GenericAlphabet_FromRadix
        !% To perform finalization of this object.
        FINAL       :: GenericAlphabet_Finalize
    END TYPE GenericAlphabet
    !> The *BinaryAlphabet* type is a data type representing the binary alphabet { 0, 1 }.
    TYPE, EXTENDS(BaseAlphabet) :: BinaryAlphabet
    CONTAINS
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**: To create this Alphabet object. <br>
        !  **Usage**: <br>
        !   --->    CALL Alphabet%Construct() <br>
        PROCEDURE   :: Construct    => BinaryAlphabet_Construct
        !% To perform finalization of this object.
        FINAL       :: BinaryAlphabet_Finalize
    END TYPE BinaryAlphabet
    !> The *OctalAlphabet* type is a data type representing the octal alphabet { 0, 1, 2, 3, 4, 5, 6, 7 }.
    TYPE, EXTENDS(BaseAlphabet) :: OctalAlphabet
    CONTAINS
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**: To create this Alphabet object. <br>
        !  **Usage**: <br>
        !   --->    CALL Alphabet%Construct() <br>
        PROCEDURE   :: Construct    => OctalAlphabet_Construct
        !% To perform finalization of this object.
        FINAL       :: OctalAlphabet_Finalize
    END TYPE OctalAlphabet
    !> The *DecimalAlphabet* type is a data type representing the decimal alphabet
    !  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }.
    TYPE, EXTENDS(BaseAlphabet) :: DecimalAlphabet
    CONTAINS
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**: To create this Alphabet object. <br>
        !  **Usage**: <br>
        !   --->    CALL Alphabet%Construct() <br>
        PROCEDURE   :: Construct    => DecimalAlphabet_Construct
        !% To perform finalization of this object.
        FINAL       :: DecimalAlphabet_Finalize
    END TYPE DecimalAlphabet
    !> The *HexadecimalAlphabet* type is a data type representing the hexadecimal alphabet
    !  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F }.
    TYPE, EXTENDS(BaseAlphabet) :: HexadecimalAlphabet
    CONTAINS
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**: To create this Alphabet object. <br>
        !  **Usage**: <br>
        !   --->    CALL Alphabet%Construct() <br>
        PROCEDURE   :: Construct    => HexadecimalAlphabet_Construct
        !% To perform finalization of this object.
        FINAL       :: HexadecimalAlphabet_Finalize
    END TYPE HexadecimalAlphabet
    !> The *DnaAlphabet* type is a data type representing the DNA alphabet { A, C, T, G }.
    TYPE, EXTENDS(BaseAlphabet) :: DnaAlphabet
    CONTAINS
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**: To create this Alphabet object. <br>
        !  **Usage**: <br>
        !   --->    CALL Alphabet%Construct() <br>
        PROCEDURE   :: Construct    => DnaAlphabet_Construct
        !% To perform finalization of this object.
        FINAL       :: DnaAlphabet_Finalize
    END TYPE DnaAlphabet
    !> The *ProteinAlphabet* type is a data type representing the protein alphabet
    !  { A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y }.
    TYPE, EXTENDS(BaseAlphabet) :: ProteinAlphabet
    CONTAINS
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**: To create this Alphabet object. <br>
        !  **Usage**: <br>
        !   --->    CALL Alphabet%Construct() <br>
        PROCEDURE   :: Construct    => ProteinAlphabet_Construct
        !% To perform finalization of this object.
        FINAL       :: ProteinAlphabet_Finalize
    END TYPE ProteinAlphabet
    !> The *LowercaseAlphabet* type is a data type representing the lowercase alphabet
    !  { a, b, c, ..., z }.
    TYPE, EXTENDS(BaseAlphabet) :: LowercaseAlphabet
    CONTAINS
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**: To create this Alphabet object. <br>
        !  **Usage**: <br>
        !   --->    CALL Alphabet%Construct() <br>
        PROCEDURE   :: Construct    => LowercaseAlphabet_Construct
        !% To perform finalization of this object.
        FINAL       :: LowercaseAlphabet_Finalize
    END TYPE LowercaseAlphabet
    !> The *UppercaseAlphabet* type is a data type representing the uppercase alphabet
    !  { A, B, C, ..., Z }.
    TYPE, EXTENDS(BaseAlphabet) :: UppercaseAlphabet
    CONTAINS
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**: To create this Alphabet object. <br>
        !  **Usage**: <br>
        !   --->    CALL Alphabet%Construct() <br>
        PROCEDURE   :: Construct    => UppercaseAlphabet_Construct
        !% To perform finalization of this object.
        FINAL       :: UppercaseAlphabet_Finalize
    END TYPE UppercaseAlphabet
    !> The *Base64Alphabet* type is a data type representing the base-64 alphabet { 64 characters }.
    TYPE, EXTENDS(BaseAlphabet) :: Base64Alphabet
    CONTAINS
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**: To create this Alphabet object. <br>
        !  **Usage**: <br>
        !   --->    CALL Alphabet%Construct() <br>
        PROCEDURE   :: Construct    => Base64Alphabet_Construct
        !% To perform finalization of this object.
        FINAL       :: Base64Alphabet_Finalize
    END TYPE Base64Alphabet
    !> The *Code39Alphabet* type is a data type representing the Code-39 alphabet { 43+1 characters }.
    TYPE, EXTENDS(BaseAlphabet) :: Code39Alphabet
    CONTAINS
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**: To create this Alphabet object. <br>
        !  **Usage**: <br>
        !   --->    CALL Alphabet%Construct() <br>
        PROCEDURE   :: Construct    => Code39Alphabet_Construct
        !% To perform finalization of this object.
        FINAL       :: Code39Alphabet_Finalize
    END TYPE Code39Alphabet
    !> The *AsciiAlphabet* type is a data type representing the ASCII alphabet { 0-127 characters' code }.
    TYPE, EXTENDS(BaseAlphabet) :: AsciiAlphabet
    CONTAINS
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**: To create this Alphabet object. <br>
        !  **Usage**: <br>
        !   --->    CALL Alphabet%Construct() <br>
        PROCEDURE   :: Construct    => AsciiAlphabet_Construct
        !% To perform finalization of this object.
        FINAL       :: AsciiAlphabet_Finalize
    END TYPE AsciiAlphabet
    !> The *ExtendedAsciiAlphabet* type is a data type representing the extended ASCII alphabet
    !  { 0-255 characters' code }.
    TYPE, EXTENDS(BaseAlphabet) :: ExtendedAsciiAlphabet
    CONTAINS
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**: To create this Alphabet object. <br>
        !  **Usage**: <br>
        !   --->    CALL Alphabet%Construct() <br>
        PROCEDURE   :: Construct    => ExtendedAsciiAlphabet_Construct
        !% To perform finalization of this object.
        FINAL       :: ExtendedAsciiAlphabet_Finalize
    END TYPE ExtendedAsciiAlphabet

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! ---------------------------------------------------------------------
! -----                 CharIndex Procedures                      -----
! ---------------------------------------------------------------------

SUBROUTINE CharIndex_Copy(SrcObj, DstObj, IsDeep)

!** PURPOSE OF THIS ROUTINE:
    !^ To copy the source object to the destination object.
    !  This is a deferred procedure inherited from the *Object* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharIndex),   INTENT(IN)  :: SrcObj   !! source object
    CLASS(Object),      INTENT(OUT) :: DstObj   !! destination object
    tLogical, OPTIONAL, INTENT(IN)  :: IsDeep
    !^ Flag indicating whether to perform deep copy or shallow copy. <br>
    !  - If present and true, perform a deep copy. <br>
    !  - If present and false, perform a shallow copy. <br>
    !  - If not present, perform either a shallow or a deep copy that is naturally most
    !    suitable for the object's components.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! copy Key and Value components
    SELECT TYPE (DstObj)
    TYPE IS (CharIndex)
        CALL SrcObj%CopyHashNode(DstObj)
        DstObj%Char  = SrcObj%Char
        DstObj%Index = SrcObj%Index
        ASSOCIATE (Dummy => IsDeep); END ASSOCIATE
    CLASS DEFAULT
        CALL Handle_ErrLevel('CharIndex_Copy', ModName, ErrSevere, &
                             'Type of the destination object must be "CharIndex" only.')
        RETURN
    END SELECT

    RETURN

END SUBROUTINE CharIndex_Copy

!******************************************************************************

FUNCTION CharIndex_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure by an *Object* object. <br>
    !  It should be noted that this routine uses all components of
    !  the *CharIndex* object to check equality. Therefore, although
    !  (A%CompareTo(B) == 0) returns true, (A%IsEqualTo(B)) can return
    !  false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharIndex), INTENT(IN)    :: LhsObj   !! an object
    CLASS(Object),    INTENT(IN)    :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal
    
    ! check key and value equalities
    SELECT TYPE (RhsObj)
    TYPE IS (CharIndex)
        ! check key equality
        IF (LhsObj%Char /= RhsObj%Char) RETURN
        ! check value equality where values may be represented by character strings
        IF (LhsObj%Index /= RhsObj%Index) RETURN
        ! check hash node component equality
        Flag = LhsObj%IsHashNodeEqual(RhsObj)
    CLASS DEFAULT
        RETURN
    END SELECT

    RETURN

END FUNCTION CharIndex_IsEqualTo

!******************************************************************************

SUBROUTINE CharIndex_MemFree(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the CharIndex object.
    !  This is a deferred procedure by an *Object* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharIndex), INTENT(INOUT) :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%ResetHashNode()

    RETURN

END SUBROUTINE CharIndex_MemFree

!******************************************************************************

FUNCTION CharIndex_ToString(Obj) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the string representation of this object.
    !  This is a deferred procedure by an *Object* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharIndex), INTENT(IN)    :: Obj
    tCharAlloc                      :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Str = '{CharIndex: {Char : ' // Obj%Char // '}, {Index : ' // ToChar(Obj%Index) // '}}'

    RETURN

END FUNCTION CharIndex_ToString

!******************************************************************************

FUNCTION CharIndex_CompareTo(A, B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare *A* and *B* and return <br>
    !   1 if *A* is greater than *B*, <br>
    !   0 if *A* is equal to *B*, <br>
    !  -1 if *A* is less than *B*, <br>
    !  -999 if type of *B* is invalid. <br>
    !  Also, write an error message to the default log file if this happens. <br>
    !  This is a deferred procedure by an *Comparable* object. <br>
    !  It is important to note that this routine only uses the key component
    !  of the *CharIndex* object.  Thus, even though (A%CompareTo(B) == 0)
    !  is true, A%IsEqualTo(B) may be false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharIndex),  INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tSInt32                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (B)
    TYPE IS (CharIndex)
        IF (LGT(A%Char, B%Char)) THEN
            Flag = +1
        ELSEIF (LLT(A%Char, B%Char)) THEN
            Flag = -1
        ELSE
            Flag = 0
        END IF
    CLASS DEFAULT
        Flag = -999
        CALL Handle_ErrLevel('CharIndex_CompareTo', ModName, ErrSevere, 'Type of B is valid.')
    END SELECT

    RETURN

END FUNCTION CharIndex_CompareTo

!******************************************************************************

FUNCTION CharIndex_HashCode(Obj) RESULT(Code)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash code of the specified object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CharIndex), INTENT(IN)    :: Obj  !! HashTreeNode object
    tIndex                          :: Code !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Code = ComputeHash(Obj%Char, Bytes_Char, 31311331_kIndex)

    RETURN

END FUNCTION CharIndex_HashCode

! ---------------------------------------------------------------------
! -----                 BaseAlphabet Procedures                   -----
! ---------------------------------------------------------------------

SUBROUTINE Alphabet_FromCharSet(Alpha, CharSet)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the Alphabet object from the specified string representing
    !  a given set of characters.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseAlphabet), INTENT(OUT)    :: Alpha    !! Alphabet object
    tCharStar,           INTENT(IN)     :: CharSet  !! a set of characters

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: I
    tChar               :: C
    tSInt32             :: IC
    tLogical            :: IsASCHII(256)

! FLOW

    Alpha%Radix = LEN(CharSet, KIND=kIndex)
    ALLOCATE(Alpha%KeyVal(Alpha%Radix), STAT=AllocStat, ERRMSG=AllocMsg)
    IF (AllocStat == 0) THEN
        CALL Alpha%HashMap%Construct(Alpha%Radix*2_kIndex)
        IsASCHII = FalseVal
        DO I = 1_kIndex, Alpha%Radix
            C  = CharSet(I:I)
            IC = IACHAR(C)
            IF (IsASCHII(IC)) THEN
                CALL Handle_ErrLevel('Alphabet_FromCharSet', ModName, ErrSevere, &
                                     'Illegal Alphabet.  Duplicated characters are NOT allowed.')
                DEALLOCATE(Alpha%KeyVal, STAT=AllocStat, ERRMSG=AllocMsg)
                CALL Handle_ErrDealloc('Alphabet_FromCharSet', ModName, AllocMsg, AllocStat)
                CALL Alpha%HashMap%Destruct()
                RETURN
            END IF
            IsASCHII(IC) = TrueVal
            Alpha%KeyVal(I)%Char  = C
            Alpha%KeyVal(I)%Index = I
            CALL Alpha%HashMap%Insert(Alpha%KeyVal(I))
        END DO
    ELSE
        CALL Handle_ErrAlloc('Alphabet_FromCharSet', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE Alphabet_FromCharSet

!******************************************************************************

SUBROUTINE Alphabet_FromRadix(Alpha, Radix)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the Alphabet object from a set of characters where their codes are
    !  from 0 to Radix-1.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseAlphabet), INTENT(OUT)    :: Alpha    !! Alphabet object
    tIndex,              INTENT(IN)     :: Radix    !! the number of characters in the alphabet

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg
    tIndex              :: I
    INTRINSIC           :: ACHAR

! FLOW
    
    IF (Radix > MaxRadix) THEN
        CALL Handle_ErrLevel('Alphabet_FromRadix', ModName, ErrSevere, &
                             'Illegal radix.  Radix must be less than or equal to 256.')
        RETURN
    END IF
    
    Alpha%Radix = Radix
    ALLOCATE(Alpha%KeyVal(Alpha%Radix), STAT=AllocStat, ERRMSG=AllocMsg)
    IF (AllocStat == 0) THEN
        CALL Alpha%HashMap%Construct(Alpha%Radix*2_kIndex)
        DO I = 1_kIndex, Radix
            Alpha%KeyVal(I)%Char = ACHAR(I-1)
            Alpha%KeyVal(I)%Index = I
            CALL Alpha%HashMap%Insert(Alpha%KeyVal(I))
        END DO
    ELSE
        CALL Handle_ErrAlloc('Alphabet_FromRadix', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE Alphabet_FromRadix

!******************************************************************************

PURE FUNCTION Alphabet_IsReady(Alpha) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the Alphabet object is ready to be used.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseAlphabet), INTENT(IN) :: Alpha    !! Alphabet object
    tLogical                        :: Flag     !! true if the object is ready to be used

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = ALLOCATED(Alpha%KeyVal)

    RETURN

END FUNCTION Alphabet_IsReady

!******************************************************************************

FUNCTION Alphabet_Contain(Alpha, C) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the Alphabet object contains the specified character or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseAlphabet), INTENT(INOUT)  :: Alpha    !! Alphabet object
    tChar,               INTENT(IN)     :: C        !! a character
    tLogical                            :: Flag     !! true if the character is in the Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(CharIndex) :: KeyVal

! FLOW

    KeyVal%Char = C
    Flag = Alpha%HashMap%Contain(KeyVal)

    RETURN

END FUNCTION Alphabet_Contain

!******************************************************************************

FUNCTION Alphabet_Verify_ChrStr(Alpha, Word) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To verify that all characters in the specified word are in the set of
    !  characters of this alphabet.  Return 0 if they are.  Otherwise, return
    !  a number indicating the first position (index) of the character that
    !  is not in the set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseAlphabet), INTENT(INOUT)  :: Alpha    !! Alphabet object
    tCharStar,           INTENT(IN)     :: Word     !! a word
    tIndex                              :: Flag     !! return number indicating whether all characters are in the set

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(CharIndex) :: KeyVal
    tIndex          :: I

! FLOW

    Flag = 0_kIndex
    DO I = 1_kIndex, LEN(Word, KIND=kIndex)
        KeyVal%Char = Word(I:I)
        IF (.NOT.Alpha%HashMap%Contain(KeyVal)) THEN
            Flag = I
            EXIT
        END IF
    END DO

    RETURN

END FUNCTION Alphabet_Verify_ChrStr

!******************************************************************************

FUNCTION Alphabet_Verify_FvlStr(Alpha, Word) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To verify that all characters in the specified word are in the set of
    !  characters of this alphabet.  Return 0 if they are.  Otherwise, return
    !  a number indicating the first position (index) of the character that
    !  is not in the set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseAlphabet), INTENT(INOUT)  :: Alpha    !! Alphabet object
    TYPE(FvlStr),        INTENT(IN)     :: Word     !! a word
    tIndex                              :: Flag     !! return number indicating whether all characters are in the set

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: StrPtr

! FLOW

    StrPtr => PtrToStr(Word)
    Flag = Alpha%Verify(StrPtr)

    RETURN

END FUNCTION Alphabet_Verify_FvlStr

!******************************************************************************

PURE FUNCTION Alphabet_Radix(Alpha) RESULT(Radix)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the number of characters in the Alphabet object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseAlphabet), INTENT(IN) :: Alpha    !! Alphabet object
    tIndex                          :: Radix    !! the number of characters in the alphabet

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Radix = Alpha%Radix

    RETURN

END FUNCTION Alphabet_Radix

!******************************************************************************

FUNCTION Alphabet_ToIndex(Alpha, C) RESULT(Idx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the index corresponding to the specified character.  If the character
    !  is not in the Alphabet object, return -1.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseAlphabet), INTENT(INOUT)  :: Alpha    !! Alphabet object
    tChar,               INTENT(IN)     :: C        !! a character
    tIndex                              :: Idx      !! index corresponding to the specified character

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(CharIndex)                 :: SearchNode
    CLASS(HashTreeNode), POINTER    :: StoredNode

! FLOW

    SearchNode%Char = C
    IF (Alpha%HashMap%FindNode(SearchNode, StoredNode)) THEN
        IF (ASSOCIATED(StoredNode)) THEN
            SELECT TYPE (StoredNode)
            TYPE IS (CharIndex)
                Idx = StoredNode%Index
            END SELECT
        ELSE
            Idx = -1_kIndex
        END IF
    ELSE
        Idx = -1_kIndex
    END IF

    RETURN

END FUNCTION Alphabet_ToIndex

!******************************************************************************

FUNCTION Alphabet_ToChar(Alpha, Idx) RESULT(C)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the character corresponding to the specified index.  If the index
    !  is not in the valid range (1 to Radix), return an empty character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseAlphabet), INTENT(IN) :: Alpha    !! Alphabet object
    tIndex,              INTENT(IN) :: Idx      !! an index
    tChar                           :: C        !! character corresponding to the specified index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF ((Idx >= 1_kIndex).AND.(Idx <= Alpha%Radix)) THEN
        C = Alpha%KeyVal(Idx)%Char
    ELSE
        C = ''
    END IF

    RETURN

END FUNCTION Alphabet_ToChar

!******************************************************************************

SUBROUTINE Alphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the Alphabet object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
!    TYPE(BaseAlphabet), INTENT(INOUT)   :: Alpha    !! Alphabet object
    CLASS(BaseAlphabet), INTENT(INOUT)   :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (ALLOCATED(Alpha%KeyVal)) DEALLOCATE(Alpha%KeyVal)
    CALL Alpha%HashMap%Destruct()

    RETURN

END SUBROUTINE Alphabet_Finalize

! ---------------------------------------------------------------------
! -----               GenericAlphabet Procedures                  -----
! ---------------------------------------------------------------------

SUBROUTINE GenericAlphabet_FromCharSet(Alpha, CharSet)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the Alphabet object from the specified string representing
    !  a given set of characters.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenericAlphabet), INTENT(OUT) :: Alpha    !! Alphabet object
    tCharStar,              INTENT(IN)  :: CharSet  !! a set of characters

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromCharset(CharSet)

    RETURN

END SUBROUTINE GenericAlphabet_FromCharSet

!******************************************************************************

SUBROUTINE GenericAlphabet_FromRadix(Alpha, Radix)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the Alphabet object from a set of characters where their codes are
    !  from 0 to Radix-1.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GenericAlphabet), INTENT(OUT) :: Alpha    !! Alphabet object
    tIndex,                 INTENT(IN)  :: Radix    !! the number of characters in the alphabet

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromRadix(Radix)

    RETURN

END SUBROUTINE GenericAlphabet_FromRadix

!******************************************************************************

SUBROUTINE GenericAlphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(GenericAlphabet), INTENT(INOUT)    :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%Finalize()

    RETURN

END SUBROUTINE GenericAlphabet_Finalize

! ---------------------------------------------------------------------
! -----               BinaryAlphabet Procedures                   -----
! ---------------------------------------------------------------------

SUBROUTINE BinaryAlphabet_Construct(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the BinaryAlphabet object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BinaryAlphabet), INTENT(OUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromCharset(BinaryCharset)

    RETURN

END SUBROUTINE BinaryAlphabet_Construct

!******************************************************************************

SUBROUTINE BinaryAlphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BinaryAlphabet), INTENT(INOUT) :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%Finalize()

    RETURN

END SUBROUTINE BinaryAlphabet_Finalize

! ---------------------------------------------------------------------
! -----                OctalAlphabet Procedures                   -----
! ---------------------------------------------------------------------

SUBROUTINE OctalAlphabet_Construct(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the OctalAlphabet object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(OctalAlphabet), INTENT(OUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromCharset(OctalCharset)

    RETURN

END SUBROUTINE OctalAlphabet_Construct

!******************************************************************************

SUBROUTINE OctalAlphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(OctalAlphabet), INTENT(INOUT) :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%Finalize()

    RETURN

END SUBROUTINE OctalAlphabet_Finalize

! ---------------------------------------------------------------------
! -----              DecimalAlphabet Procedures                   -----
! ---------------------------------------------------------------------

SUBROUTINE DecimalAlphabet_Construct(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the DecimalAlphabet object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DecimalAlphabet), INTENT(OUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromCharset(DecimalCharset)

    RETURN

END SUBROUTINE DecimalAlphabet_Construct

!******************************************************************************

SUBROUTINE DecimalAlphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(DecimalAlphabet), INTENT(INOUT) :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%Finalize()

    RETURN

END SUBROUTINE DecimalAlphabet_Finalize

! ---------------------------------------------------------------------
! -----           HexadecimalAlphabet Procedures                  -----
! ---------------------------------------------------------------------

SUBROUTINE HexadecimalAlphabet_Construct(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the HexadecimalAlphabet object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HexadecimalAlphabet), INTENT(OUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromCharset(HexadecimalCharset)

    RETURN

END SUBROUTINE HexadecimalAlphabet_Construct

!******************************************************************************

SUBROUTINE HexadecimalAlphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(HexadecimalAlphabet), INTENT(INOUT) :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%Finalize()

    RETURN

END SUBROUTINE HexadecimalAlphabet_Finalize

! ---------------------------------------------------------------------
! -----               DnaAlphabet Procedures                      -----
! ---------------------------------------------------------------------

SUBROUTINE DnaAlphabet_Construct(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the DnaAlphabet object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DnaAlphabet), INTENT(OUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromCharset(DnaCharset)

    RETURN

END SUBROUTINE DnaAlphabet_Construct

!******************************************************************************

SUBROUTINE DnaAlphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(DnaAlphabet), INTENT(INOUT) :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%Finalize()

    RETURN

END SUBROUTINE DnaAlphabet_Finalize

! ---------------------------------------------------------------------
! -----               ProteinAlphabet Procedures                  -----
! ---------------------------------------------------------------------

SUBROUTINE ProteinAlphabet_Construct(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the ProteinAlphabet object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ProteinAlphabet), INTENT(OUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromCharset(ProteinCharset)

    RETURN

END SUBROUTINE ProteinAlphabet_Construct

!******************************************************************************

SUBROUTINE ProteinAlphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ProteinAlphabet), INTENT(INOUT) :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%Finalize()

    RETURN

END SUBROUTINE ProteinAlphabet_Finalize

! ---------------------------------------------------------------------
! -----               LowercaseAlphabet Procedures                -----
! ---------------------------------------------------------------------

SUBROUTINE LowercaseAlphabet_Construct(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the LowercaseAlphabet object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LowercaseAlphabet), INTENT(OUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromCharset(LowercaseCharset)

    RETURN

END SUBROUTINE LowercaseAlphabet_Construct

!******************************************************************************

SUBROUTINE LowercaseAlphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(LowercaseAlphabet), INTENT(INOUT) :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%Finalize()

    RETURN

END SUBROUTINE LowercaseAlphabet_Finalize

! ---------------------------------------------------------------------
! -----               UppercaseAlphabet Procedures                -----
! ---------------------------------------------------------------------

SUBROUTINE UppercaseAlphabet_Construct(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the UppercaseAlphabet object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(UppercaseAlphabet), INTENT(OUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromCharset(UppercaseCharset)

    RETURN

END SUBROUTINE UppercaseAlphabet_Construct

!******************************************************************************

SUBROUTINE UppercaseAlphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UppercaseAlphabet), INTENT(INOUT) :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%Finalize()

    RETURN

END SUBROUTINE UppercaseAlphabet_Finalize

! ---------------------------------------------------------------------
! -----               Base64Alphabet Procedures                   -----
! ---------------------------------------------------------------------

SUBROUTINE Base64Alphabet_Construct(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the Base64Alphabet object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Base64Alphabet), INTENT(OUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromCharset(Base64Charset)

    RETURN

END SUBROUTINE Base64Alphabet_Construct

!******************************************************************************

SUBROUTINE Base64Alphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Base64Alphabet), INTENT(INOUT) :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%Finalize()

    RETURN

END SUBROUTINE Base64Alphabet_Finalize

! ---------------------------------------------------------------------
! -----               Code39Alphabet Procedures                   -----
! ---------------------------------------------------------------------

SUBROUTINE Code39Alphabet_Construct(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the Code39Alphabet object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Code39Alphabet), INTENT(OUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromCharset(Code39Charset)

    RETURN

END SUBROUTINE Code39Alphabet_Construct

!******************************************************************************

SUBROUTINE Code39Alphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Code39Alphabet), INTENT(INOUT) :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%Finalize()

    RETURN

END SUBROUTINE Code39Alphabet_Finalize

! ---------------------------------------------------------------------
! -----               AsciiAlphabet Procedures                    -----
! ---------------------------------------------------------------------

SUBROUTINE AsciiAlphabet_Construct(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the AsciiAlphabet object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AsciiAlphabet), INTENT(OUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromRadix(128_kIndex)

    RETURN

END SUBROUTINE AsciiAlphabet_Construct

!******************************************************************************

SUBROUTINE AsciiAlphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(AsciiAlphabet), INTENT(INOUT) :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%Finalize()

    RETURN

END SUBROUTINE AsciiAlphabet_Finalize

! ---------------------------------------------------------------------
! -----               ExtendedAsciiAlphabet Procedures            -----
! ---------------------------------------------------------------------

SUBROUTINE ExtendedAsciiAlphabet_Construct(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the ExtendedAsciiAlphabet object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ExtendedAsciiAlphabet), INTENT(OUT)  :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%FromRadix(256_kIndex)

    RETURN

END SUBROUTINE ExtendedAsciiAlphabet_Construct

!******************************************************************************

SUBROUTINE ExtendedAsciiAlphabet_Finalize(Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ExtendedAsciiAlphabet), INTENT(INOUT) :: Alpha    !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Alpha%Finalize()

    RETURN

END SUBROUTINE ExtendedAsciiAlphabet_Finalize

!******************************************************************************

END MODULE MClass_Alphabets

!******************************************************************************
