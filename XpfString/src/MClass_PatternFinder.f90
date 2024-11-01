
MODULE MClass_PatternFinder

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *PatternFinder* type and its related helper types and routines.
!   The *PatternFinder* type is a string type that provides various efficient algorithms for
!   a *substring searching* operation.  The *substring searching* is a fundamental string
!   operation where given a *text* string of length N and a *pattern* string of length M,
!   find an occurrence of the *pattern* within the *text*.  <br>
!   Available searching algorithms include:  <br>
!   - *BruteForce*: a simple basic searching algorithm [1, 6], <br>
!   - *BoyerMoore*: a searching algorithm by Boyer and Mooore [1, 6], <br>
!   - *KMP*: a searching algorithm by Knuth, Morris and Pratt [1, 6], <br>
!   - *KMPlus*: an optimized version of *KMP* algorithm [1, 6], <br>
!   - *RabinKarp*: a searching algorithm by Rabin and Karp [1, 6], <br>
!   - *Horspool*: an improved version of *BoyerMoore* algorithm by Horspool [2, 7], <br>
!   - *Raita*: an improved version of *Horspool* algorithm by Raita [3, 7], <br>
!   - *Sunday*: an improved version of *BoyerMoore* algorithm by Sunday [4], and <br>
!   - *BNDM*: a Backwards Non-deterministic DAWG (Directed acyclic word graph)
!      Matching algorithm by Gonzalo Navarro and Mathieu Raffinot [5, 7]. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] R. Sedgewick and K. Wayne. 2011.  Algorithms, 4th Edition.  Addison-Wesley. <br>
!   [2] <a href="https://webhome.cs.uvic.ca/~nigelh/Publications/stringsearch.pdf">
!       R.N. Horspool. 1980. Practical Fast Searching in Strings. Software-Practice and
!       Experience, vol. 10, pp. 501-506. </a> <br>
!   [3] <a href="https://www.inf.unioeste.br/~adair/ED/Artigos/Tunning%20Boyer-Moore-Horspool%20String%20Search%20Algorithm%20-%20Raita.pdf">
!       T. Raita. 1992. Tuning the Boyer-Moore-Horspool string searching algorithm.
!       Software-Practice and Experience, vol. 22(10), pp. 879-884.</a> <br>
!   [4] <a href="https://dl.acm.org/doi/10.1145/79173.79184">D.M. Sunday. 1990.  A very fast
!       substring search algorithm.  Communications of the ACM, vol. 33(8), pp. 132-142. </a> <br>
!   [5] <a href="https://www.semanticscholar.org/paper/A-Bit-Parallel-Approach-to-Suffix-Automata%3A-Fast-Navarro-Raffinot/75cb2a611f737c296a576d396334bc6298982e7a">
!       G. Navarro and M. Raffinot. 1998. A Bit-Parallel Approach to Suffix Automata:
!       Fast Extended String Matching. Combinatorial Pattern Matching, pp. 14-33. </a> <br>
!   [6] <a href="https://algs4.cs.princeton.edu/home/">R. Sedgewick and K. Wayne.
!       Algorithms, 4th Edition, Online version. </a> <br>
!   [7] <a href="https://github.com/johannburkard/StringSearch">StringSearch:
!       High-performance pattern matching algorithms in Java. </a> <br>

!** USE STATEMENTS:
    USE ISO_C_BINDING,          ONLY: C_LOC, C_F_POINTER, C_PTR, C_NULL_PTR
    USE MBase_Common
    USE MBase_CharUtil,       ONLY: IsLetter => Is_Character_Letter, &
                                      ToUpper  => ToUpperCase, &
                                      ToLower  => ToLowerCase
    USE MBase_SIntUtil,       ONLY: MAX_I32, MAX_I64
    USE MBase_MathUtil,       ONLY: IsPrime
    USE MBase_SInt128
    USE MBase_ErrHandlers
    USE MClass_Sfc64RNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: PatternFinder

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define     tSInt128    TYPE(SInt128)

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName    = 'Class_PatternFinder'
    ! radix (number of characters in the alphabet set) for extended-ASCII alphabet
    tIndex,    PARAMETER    :: Radix      = 256_kIndex
    ! Parameters for searching algorithms
    tSInt32,  PUBLIC, PARAMETER :: BruteForce = 0
    tSInt32,  PUBLIC, PARAMETER :: BoyerMoore = 1
    tSInt32,  PUBLIC, PARAMETER :: KMPlus     = 2
    tSInt32,  PUBLIC, PARAMETER :: KMP        = 3
    tSInt32,  PUBLIC, PARAMETER :: RabinKarp  = 4
    tSInt32,  PUBLIC, PARAMETER :: Horspool   = 5
    tSInt32,  PUBLIC, PARAMETER :: Raita      = 6
    tSInt32,  PUBLIC, PARAMETER :: Sunday     = 7
    tSInt32,  PUBLIC, PARAMETER :: BNDM       = 8

!** DERIVED TYPE DEFINITIONS
    !> The *BasePattern* type is a base type used to store information relating
    !  processed pattern.  This is a private helper type.
    TYPE BasePattern
        tIndex      :: M        ! length of the pattern
    END TYPE BasePattern
    !> The *RabinKarpPat* type is a pattern type storing processed information for
    !  the Rabin-Karp algorithm.  This is a private helper type.
    TYPE, EXTENDS(BasePattern) :: RabinKarpPat
        tIndex      :: Hash     ! hash code of the pattern
        tSInt64     :: Q        ! a large prime, small enough to avoid long overflow
        tIndex      :: R        ! radix
        tSInt64     :: RM       ! MOD(R**(M-1), Q)
    END TYPE RabinKarpPat
    !> The *KMPPat* type is a pattern type storing processed information for the
    !  Knuth-Morris-Pratt algorithm.  This is a private helper type.
    TYPE, EXTENDS(BasePattern) :: KMPPat
        tIndex              :: R        ! radix
        tIndex, ALLOCATABLE :: DFA(:,:) ! deterministic finite automaton
    END TYPE KMPPat
    !> The *KMPlusPat* type is a pattern type storing processed information for an optimized
    !  version of the Knuth-Morris-Pratt algorithm.  This is a private helper type.
    TYPE, EXTENDS(BasePattern) :: KMPlusPat
        tIndex, ALLOCATABLE :: Next(:)  ! non-deterministic finite automaton
    END TYPE KMPlusPat
    !> The *BoyerMoorePat* type is a pattern type storing processed information for the
    !  Boyer-Moore algorithm.  This is a private helper type.
    TYPE, EXTENDS(BasePattern) :: BoyerMoorePat
        tIndex  :: R                ! radix
        tIndex  :: Right(0:Radix-1) ! the bad character skip array
    END TYPE BoyerMoorePat
    !> The *ChrIntMap* type is a pattern type storing processed information for the
    !  Horspool and Raita algorithms.  This is a private helper type.
    TYPE, EXTENDS(BasePattern) :: ChrIntMap
        tIndex              :: Lowest   ! ASCII code of lowest character
        tIndex              :: DefVal   ! default value
        tIndex, ALLOCATABLE :: Arr(:)   ! skip array
    CONTAINS
        PROCEDURE :: Create     => ChrIntMap_Create
        PROCEDURE :: Construct  => ChrIntMap_Construct
        PROCEDURE :: Set        => ChrIntMap_Set
        PROCEDURE :: Get        => ChrIntMap_Get
    END TYPE ChrIntMap
    !> The *BNDMPatLen64* type is a pattern type storing processed information for the
    !  BNDM algorithm(s).  This is a private helper type.
    TYPE, EXTENDS(BasePattern) :: BNDMPatLen64
        tIndex              :: Lowest   ! ASCII code of lowest character
        tSInt64                 :: DefVal   ! default value
        tSInt64, ALLOCATABLE    :: Arr(:)   ! skip array
    CONTAINS
        PROCEDURE :: Create     => BNDMPatLen64_Create
        PROCEDURE :: Construct  => BNDMPatLen64_Construct
        PROCEDURE :: Set        => BNDMPatLen64_Set
        PROCEDURE :: Get        => BNDMPatLen64_Get
    END TYPE BNDMPatLen64
    !> The *BNDMPatLen128* type is a pattern type storing processed information for the
    !  BNDM algorithm(s).  This is a private helper type.
    TYPE, EXTENDS(BasePattern) :: BNDMPatLen128
        tIndex                  :: Lowest   ! ASCII code of lowest character
        tSInt128                :: DefVal   ! default value
        tSInt128, ALLOCATABLE   :: Arr(:)   ! skip array
    CONTAINS
        PROCEDURE :: Create     => BNDMPatLen128_Create
        PROCEDURE :: Construct  => BNDMPatLen128_Construct
        PROCEDURE :: Set        => BNDMPatLen128_Set
        PROCEDURE :: Get        => BNDMPatLen128_Get
    END TYPE BNDMPatLen128
    !> The *PatternFinder* type is a string type that provides various efficient
    !  algorithms for a *substring searching* operation.
    TYPE PatternFinder
        PRIVATE
        !% searching algorithm
        tSInt32                         :: Algo  = BoyerMoore
        !% current starting (one-based) index of the pattern
        tIndex                          :: Index = 0_kIndex
        !% string representing the pattern
        tChar,              POINTER     :: PatTxt(:) => NULL()
        !% string representing the searched text
        tCharLen(:),        POINTER     :: InpTxt    => NULL()
        !% pattern object containing processed information
        CLASS(BasePattern), ALLOCATABLE :: PatObj
    CONTAINS
        !> **Type-Bound Subroutine**: SetPattern <br>
        !  **Purpose**:  To process the specified pattern based on the specified searching algorithm. <br>
        !  **Usage**: <br>
        !   --->    CALL Finder%SetPattern(Pattern, SearchAlgo) <br>
        !   --->    CALL Finder%SetPattern(Pattern, BNDM, WildCard='.') <br>
        !   --->    CALL Finder%SetPattern(Pattern, BNDM, Insensitive=.TRUE.) <br>
        !   --->    CALL Finder%SetPattern(Pattern, BNDM, .TRUE., '*') <br>
        !  **Note**: <br>
        !   1. If the length of the pattern is zero, the method just returns without doing anything. <br>
        !   2. The optional *Insensitive* and *WildCard* arguments are applicable for the BNDM method only. <br>
        PROCEDURE   :: SetPattern   => PatternFinder_SetPattern
        !> **Type-Bound Function**: FindIndex <br>
        !  **Purpose**:  To find the first occurrence of the (previously specified) *pattern* within the
        !       specified *text*.  Return an (one-based) index representing the position of the first
        !       character of the occurrence found.  Return zero if there is no such pattern found.  If the
        !       pattern has not yet been set, return one. <br>
        !  **Usage**: <br>
        !   ! start searching from the beginning of the text <br>
        !   --->    Index = Finder%FindIndex(Text) <br>
        !   ! start searching from the specified position (one-based) of the text <br>
        !   --->    Index = Finder%FindIndex(Text, StartPos) <br>
        PROCEDURE   :: FindIndex    => PatternFinder_FindIndex
        !> **Type-Bound Function**: FindNext <br>
        !  **Purpose**:  To find the next occurrence of the (previously specified) *pattern* within the
        !        (previously specified) *text*. <br>
        !  **Usage**: <br>
        !   --->    Index = Finder%FindNext() <br>
        !  **Note**: Must call the *SetPattern* and *FindIndex* methods before calling this method. <br>
        PROCEDURE   :: FindNext     => PatternFinder_FindNext
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To reset/clear all components of the *PatternFinder* object. <br>
        !  **Usage**: <br>
        !   --->    CALL Finder%Clear() <br>
        !  **Note**: If the length of the pattern is zero, the method just returns without doing anything. <br>
        PROCEDURE   :: Clear        => PatternFinder_Clear
        !% To perform finalization of the *PatternFinder* object.
        FINAL       :: PatternFinder_Finalize
    END TYPE PatternFinder

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!--------------------------------------------------------------------------------------
!                           PatternFinder Procedures
!--------------------------------------------------------------------------------------

SUBROUTINE PatternFinder_SetPattern(Finder, Pattern, SearchAlgo, Insensitive, WildCard)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process the specified pattern based on the specified searching algorithm.
    !  If the length of the pattern is zero, just return without doing anything.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PatternFinder), INTENT(INOUT) :: Finder       !! 'PatternFinder' object
    tCharStar, TARGET,    INTENT(IN)    :: Pattern      !! the pattern
    tIndex,               INTENT(IN)    :: SearchAlgo   !! search algorithm
    tLogical, OPTIONAL,   INTENT(IN)    :: Insensitive
    !^ true if the pattern is case-insensitive; default is false; only applicable for BNDM algorithm.
    tChar,    OPTIONAL,   INTENT(IN)    :: WildCard
    !^ a wild card character; default is "?"; only applicable for BNDM algorithm.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(C_PTR) :: CPtr     ! C pointer to the pattern string

! FLOW

    IF (LEN(Pattern) < 1) THEN
        CALL Handle_ErrLevel('PatternFinder_SetPattern', ModName, ErrSevere, &
                    'The pattern length of zero is NOT allowed.')
        RETURN
    ELSEIF (SearchAlgo == BNDM) THEN
        IF (LEN(Pattern) > 128) THEN
            CALL Handle_ErrLevel('PatternFinder_SetPattern', ModName, ErrSevere, &
                        'The pattern length is limited to 128 for the "BNDM" algorithm.')
            RETURN
        END IF
    END IF

    ! get a C pointer to input
    CPtr = C_LOC(Pattern)

    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, Finder%PatTxt, [LEN(Pattern)])

    IF (ALLOCATED(Finder%PatObj)) DEALLOCATE(Finder%PatObj)
    Finder%Algo = SearchAlgo
    SELECT CASE (SearchAlgo)
    CASE (BNDM)
        IF (LEN(Pattern) <= 64) THEN
            ALLOCATE(BNDMPatLen64  :: Finder%PatObj)
        ELSE
            ALLOCATE(BNDMPatLen128 :: Finder%PatObj)
        END IF
        SELECT TYPE (PatObj => Finder%PatObj)
        TYPE IS (BNDMPatLen64)
            CALL BNDMLen64_Process(PatObj, Finder%PatTxt, Insensitive, WildCard)
        TYPE IS (BNDMPatLen128)
            CALL BNDMLen128_Process(PatObj, Finder%PatTxt, Insensitive, WildCard)
        END SELECT
    CASE (Horspool, Raita, Sunday)
        ALLOCATE(ChrIntMap :: Finder%PatObj)
        IF (SearchAlgo /= Sunday) THEN
            CALL HorspoolRaita_Process(Finder%PatObj, Finder%PatTxt)
        ELSE
            CALL Sunday_Process(Finder%PatObj, Finder%PatTxt)
        END IF
    CASE (BoyerMoore)
        ALLOCATE(BoyerMoorePat :: Finder%PatObj)
        CALL BoyerMoore_Process(Finder%PatObj, Finder%PatTxt)
    CASE (KMPlus)
        ALLOCATE(KMPlusPat :: Finder%PatObj)
        CALL KMPlus_Process(Finder%PatObj, Finder%PatTxt)
    CASE (KMP)
        ALLOCATE(KMPPat :: Finder%PatObj)
        CALL KMP_Process(Finder%PatObj, Finder%PatTxt)
    CASE (RabinKarp)
        ALLOCATE(RabinKarpPat :: Finder%PatObj)
        CALL RabinKarp_Process(Finder%PatObj, Finder%PatTxt)
    CASE (BruteForce)
        ALLOCATE(BasePattern :: Finder%PatObj)
        CALL BruteForce_Process(Finder%PatObj, Finder%PatTxt)
    CASE DEFAULT
        ALLOCATE(BoyerMoorePat :: Finder%PatObj)
        CALL BoyerMoore_Process(Finder%PatObj, Finder%PatTxt)
        Finder%Algo = BoyerMoore
    END SELECT

    ! nullify pointers
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE PatternFinder_SetPattern

!******************************************************************************

FUNCTION PatternFinder_FindIndex(Finder, Text, StartID) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the one-based index of the first occurrence of the pattern string
    !  in the text string.  Return zero if there is no such pattern found.  If the
    !  pattern has not yet been set, return one. <br>
    !  Note: The unset pattern is equivalent to the zero-length pattern.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PatternFinder), INTENT(INOUT)  :: Finder   !! 'PatternFinder' object
    tCharStar,  TARGET,   INTENT(IN)     :: Text     !! the text string
    tIndex,     OPTIONAL, INTENT(IN)     :: StartID
    !^ (one-based) starting index of the text string;
    !  if not present, starting at 1.
    tIndex                              :: Index
    !^ (one-based) index of the first occurrence;
    !  if pattern not found, return 0.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tChar, POINTER  :: InpTxt(:) => NULL()  ! F pointer to the input string
    TYPE(C_PTR)     :: CPtr                 ! C pointer to the input string
    tIndex          :: BeginID              ! zero-based index

! FLOW

    ! set zero-based starting index
    SET_OPTION(BeginID, 1_kIndex, StartID)
    BeginID = BeginID - 1_kIndex

    ! check validity of all input
    IF ((.NOT.ASSOCIATED(Finder%PatTxt)).OR.(.NOT.ALLOCATED(Finder%PatObj))) THEN
        Index = 1_kIndex
        CALL Handle_ErrLevel('PatternFinder_FindIndex', ModName, ErrWarning, &
                             'The pattern has not yet been set.')
        RETURN
    ELSEIF ((BeginID < 0_kIndex).OR.(BeginID == LEN(Text, KIND=kIndex))) THEN
        Index = 0_kIndex
        CALL Handle_ErrLevel('PatternFinder_FindIndex', ModName, ErrWarning, &
                             'The starting index is out of the applicable range.')
        RETURN
    ELSEIF (LEN(Text, KIND=kIndex) < SIZE(Finder%PatTxt, KIND=kIndex)) THEN
        Index = 0_kIndex
        CALL Handle_ErrLevel('PatternFinder_FindIndex', ModName, ErrWarning, &
                             'The text length is less than the pattern length.')
        RETURN
    ELSEIF ((LEN(Text, KIND=kIndex)-BeginID) < SIZE(Finder%PatTxt, KIND=kIndex)) THEN
        ! do not need to report error since all input considered valid
        Index = 0_kIndex
        RETURN
    END IF

    ! get a C pointer to input
    CPtr = C_LOC(Text)

    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, InpTxt, [LEN(Text)])

    ! set pointer for FindNext
    Finder%InpTxt => Text

    ! find pattern
    SELECT TYPE (PatObj => Finder%PatObj)
    TYPE IS (BNDMPatLen64)
        Index = BNDM_Find(PatObj, InpTxt, BeginID)
    TYPE IS (BNDMPatLen128)
        Index = BNDM_Find(PatObj, InpTxt, BeginID)
    TYPE IS (ChrIntMap)
        SELECT CASE (Finder%Algo)
        CASE (Horspool)
            Index = Horspool_Find(PatObj, Finder%PatTxt, InpTxt, BeginID)
        CASE (Raita)
            Index = Raita_Find(PatObj, Finder%PatTxt, InpTxt, BeginID)
        CASE (Sunday)
            Index = Sunday_Find(PatObj, Finder%PatTxt, InpTxt, BeginID)
        END SELECT
    TYPE IS (BoyerMoorePat)
        Index = BoyerMoore_Find(PatObj, Finder%PatTxt, InpTxt, BeginID)
    TYPE IS (KMPlusPat)
        Index = KMPlus_Find(PatObj, Finder%PatTxt, InpTxt, BeginID)
    TYPE IS (KMPPat)
        Index = KMP_Find(PatObj, InpTxt, BeginID)
    TYPE IS (RabinKarpPat)
        Index = RabinKarp_Find(PatObj, Finder%PatTxt, InpTxt(BeginID+1:))
    TYPE IS (BasePattern)
        Index = BruteForce_Find(PatObj, Finder%PatTxt, InpTxt, BeginID)
    END SELECT

    ! post-process the index
    Index = Index + 1_kIndex
    IF (Index > LEN(Text, KIND=kIndex)) Index = 0_kIndex
    Finder%Index = Index

    ! nullify pointers
    cPtr = C_NULL_PTR
    NULLIFY(InpTxt)

    RETURN

END FUNCTION PatternFinder_FindIndex

!******************************************************************************

FUNCTION PatternFinder_FindNext(Finder) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the one-based index of the next occurrence of the pattern string
    !  in the text string.  Return zero if there is no more pattern found.  This
    !  routine assumes that the *SetPattern* and the *FindIndex* methods have
    !  already been called.  If that is not true, return -1. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PatternFinder), INTENT(INOUT) :: Finder   !! 'PatternFinder' object
    tIndex                              :: Index
    !^ (one-based) index of the first occurrence;
    !  if pattern not found, return 0.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check validity of all input
    IF ((.NOT.ASSOCIATED(Finder%InpTxt)).OR.(.NOT.ASSOCIATED(Finder%PatTxt)) &
                                        .OR.(.NOT.ALLOCATED(Finder%PatObj))) THEN
        Index = -1_kIndex
        CALL Handle_ErrLevel('PatternFinder_FindNext', ModName, ErrWarning, &
                             'Must call the "SetPattern" and the "FindIndex" methods first.')
        RETURN
    ELSEIF (Finder%Index == 0_kIndex) THEN
        Index = 0_kIndex
        RETURN
    END IF

    Index = Finder%FindIndex(Finder%InpTxt, Finder%Index)

    RETURN

END FUNCTION PatternFinder_FindNext

!******************************************************************************

SUBROUTINE PatternFinder_Clear(Finder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the 'PatternFinder' object. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PatternFinder), INTENT(INOUT) :: Finder   !! 'PatternFinder' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Finder%Algo  = BoyerMoore
    Finder%Index = 0_kIndex
    NULLIFY(Finder%PatTxt)
    NULLIFY(Finder%InpTxt)
    IF (ALLOCATED(Finder%PatObj)) DEALLOCATE(Finder%PatObj)

    RETURN

END SUBROUTINE PatternFinder_Clear

!******************************************************************************

SUBROUTINE PatternFinder_Finalize(Finder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(PatternFinder), INTENT(INOUT)  :: Finder   !! 'PatternFinder' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Finder%Clear()

    RETURN

END SUBROUTINE PatternFinder_Finalize

!--------------------------------------------------------------------------------------
!                               Engine Procedures
!--------------------------------------------------------------------------------------

SUBROUTINE BNDMLen64_Process(PatObj, Pattern, Insensitive, WildCard)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pre-process the specified pattern for BNDM algorithms where pattern
    !  length is less than or equal to 64.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BNDMPatLen64), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,               INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters
    tLogical, OPTIONAL,  INTENT(IN)      :: Insensitive
    !^ true if the pattern is case-insensitive; default is false; only applicable for BNDM algorithm.
    tChar,    OPTIONAL,  INTENT(IN)      :: WildCard
    !^ a wild card character; default is "?"; only applicable for BNDM algorithm.

!** SUBROUTINE PARAMETER DECLARATIONS:
    !% a wild card for single character
    tCharParam  :: WildCardDeflt = '?'

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: N, I
    tChar       :: WC                   ! wild card character
    tLogical    :: IsCaseInsensitive    ! true if case-insensitive
    tSInt64     :: J, Val

! FLOW

    SET_OPTION(WC, WildCardDeflt, WildCard)
    SET_OPTION(IsCaseInsensitive, FalseVal, Insensitive)

    N = SIZE(Pattern, KIND=kIndex)

    ! find default value
    J = 0_kInt64
    DO I = 0_kIndex, N-1_kIndex
        IF (Pattern(I) == WC) THEN
            J = IOR(J, SHIFTL(1_kInt64, N-I-1_kIndex))
        END IF
    END DO
    IF (.NOT.IsCaseInsensitive) THEN
        ! ----- case-sensitive -----
        ! create character-integer mapping object
        CALL PatObj%Create(Pattern, N, J)
        PatObj%M = N
        ! set character-integer mapping
        J = 1_kInt64
        DO I = PatObj%M-1_kIndex, 0_kIndex, -1_kIndex
            Val = IOR(PatObj%Get(Pattern(I)), J)
            CALL PatObj%Set(Pattern(I), Val)
            J = SHIFTL(J, 1)
        END DO
    ELSE
        ! ----- case-insensitive -----
        BLOCK
            tIndex      :: MinVal, MaxVal, I
            tChar       :: P, T
            ! execution
            ! compute minval and maxval
            CALL MinMax_Insensitive(Pattern, MinVal, MaxVal)
            ! create character-integer mapping object
            CALL PatObj%Construct(MaxVal-MinVal+1_kIndex, MinVal, J)
            PatObj%M = N
            ! set character-integer mapping
            J = 1_kInt64
            DO I = PatObj%M-1_kIndex, 0_kIndex, -1_kIndex
                P = Pattern(I)
                IF (IsLetter(P)) THEN
                    T = ToLower(P)
                    Val = IOR(PatObj%Get(T), J)
                    CALL PatObj%Set(T, Val)
                    T = ToUpper(P)
                    Val = IOR(PatObj%Get(T), J)
                    CALL PatObj%Set(T, Val)
                ELSE
                    Val = IOR(PatObj%Get(P), J)
                    CALL PatObj%Set(P, Val)
                END IF
                J = SHIFTL(J, 1)
            END DO
        END BLOCK
    END IF

    RETURN

END SUBROUTINE BNDMLen64_Process

!******************************************************************************

SUBROUTINE BNDMLen128_Process(PatObj, Pattern, Insensitive, WildCard)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pre-process the specified pattern for BNDM algorithms where pattern
    !  length is less than or equal to 128.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BNDMPatLen128), INTENT(INOUT) :: PatObj       !! the pattern data object
    tChar,                INTENT(IN)    :: Pattern(0:)  !! the pattern as an array of characters
    tLogical, OPTIONAL,   INTENT(IN)    :: Insensitive
    !^ true if the pattern is case-insensitive; default is false; only applicable for BNDM algorithm.
    tChar,    OPTIONAL,   INTENT(IN)    :: WildCard
    !^ a wild card character; default is "?"; only applicable for BNDM algorithm.

!** SUBROUTINE PARAMETER DECLARATIONS:
    !% a wild card for single character
    tCharParam  :: WildCardDeflt = '?'

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: N, I
    tChar       :: WC                   ! wild card character
    tLogical    :: IsCaseInsensitive    ! true if case-insensitive
    tSInt128    :: J, Val

! FLOW

    SET_OPTION(WC, WildCardDeflt, WildCard)
    SET_OPTION(IsCaseInsensitive, FalseVal, Insensitive)

    N = SIZE(Pattern, KIND=kIndex)

    ! find default value
    J = ZeroI128
    DO I = 0_kIndex, N-1_kIndex
        IF (Pattern(I) == WC) THEN
            J = IOR(J, SHIFTL(OneI128, ToInt32(N-I-1_kIndex)))
        END IF
    END DO
    IF (.NOT.IsCaseInsensitive) THEN
        ! ----- case-sensitive -----
        ! create character-integer mapping object
        CALL PatObj%Create(Pattern, N, J)
        PatObj%M = N
        ! set character-integer mapping
        J = OneI128
        DO I = PatObj%M-1_kIndex, 0_kIndex, -1_kIndex
            Val = IOR(PatObj%Get(Pattern(I)), J)
            CALL PatObj%Set(Pattern(I), Val)
            J = ShiftLOnce(J)
        END DO
    ELSE
        ! ----- case-insensitive -----
        BLOCK
            tIndex      :: MinVal, MaxVal, I
            tChar       :: P, T
            ! execution
            ! compute minval and maxval
            CALL MinMax_Insensitive(Pattern, MinVal, MaxVal)
            ! create character-integer mapping object
            CALL PatObj%Construct(MaxVal-MinVal+1_kIndex, MinVal, J)
            PatObj%M = N
            ! set character-integer mapping
            J = OneI128
            DO I = PatObj%M-1_kIndex, 0_kIndex, -1_kIndex
                P = Pattern(I)
                IF (IsLetter(P)) THEN
                    T = ToLower(P)
                    Val = IOR(PatObj%Get(T), J)
                    CALL PatObj%Set(T, Val)
                    T = ToUpper(P)
                    Val = IOR(PatObj%Get(T), J)
                    CALL PatObj%Set(T, Val)
                ELSE
                    Val = IOR(PatObj%Get(P), J)
                    CALL PatObj%Set(P, Val)
                END IF
                J = ShiftLOnce(J)
            END DO
        END BLOCK
    END IF

    RETURN

END SUBROUTINE BNDMLen128_Process

!******************************************************************************

FUNCTION BNDM_Find(PatObj, Text, StartID) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the zero-based index of the first occurrence of the pattern string
    !  in the text string.  Return the length of the text string if there is no such
    !  pattern found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Text(0:)     !! the text string as an array of characters
    tIndex,             INTENT(IN)      :: StartID      !! (zero-based) starting index of the text string
    tIndex                              :: Index        !! (zero-based) index of the first occurrence

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Pos, J, N, Last

! FLOW

    N = SIZE(Text, KIND=kIndex)
    Pos = StartID
    SELECT TYPE (Pat => PatObj)
    TYPE IS (BNDMPatLen64)
        BLOCK
            tSInt64 :: D
            OutLoop: DO WHILE (Pos <= N-1_kIndex)
                J = Pat%M - 1_kIndex
                Last = Pat%M
                D = -1_kInt64
                InLoop: DO WHILE (D /= 0_kInt64)
                    D = IAND(D, Pat%Get(Text(Pos+J)))
                    IF (D /= 0_kInt64) THEN
                        IF (J == 0_kIndex) THEN
                            ! pattern found
                            Index = Pos
                            RETURN
                        END IF
                        Last = J
                    END IF
                    J = J - 1_kIndex
                    D = SHIFTL(D, 1)
                END DO InLoop
                Pos = Pos + Last
            END DO OutLoop
            ! pattern not found
            Index = N
        END BLOCK
    TYPE IS (BNDMPatLen128)
        BLOCK
            tSInt128    :: D
            OutLoop: DO WHILE (Pos <= N-1_kIndex)
                J = Pat%M - 1_kIndex
                Last = Pat%M
                D = -OneI128
                InLoop: DO WHILE (D /= ZeroI128)
                    D = IAND(D, Pat%Get(Text(Pos+J)))
                    IF (D /= ZeroI128) THEN
                        IF (J == 0_kIndex) THEN
                            ! pattern found
                            Index = Pos
                            RETURN
                        END IF
                        Last = J
                    END IF
                    J = J - 1_kIndex
                    D = ShiftLOnce(D)
                END DO InLoop
                Pos = Pos + Last
            END DO OutLoop
            ! pattern not found
            Index = N
        END BLOCK
    END SELECT

    RETURN

END FUNCTION BNDM_Find

!******************************************************************************

SUBROUTINE HorspoolRaita_Process(PatObj, Pattern)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pre-process the specified pattern for Horspool and Raita algorithms.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: N, I

! FLOW

    N = SIZE(Pattern, KIND=kIndex)
    PatObj%M = N
    SELECT TYPE (Pat => PatObj)
    TYPE IS (ChrIntMap)
        IF (LEN(Pattern) > 2) THEN
            CALL Pat%Create(Pattern, N, N)
            Pat%M = N       ! this statement must exists since the create method will make Pat%M undefined
            N = N - 1_kIndex
            DO I = 0_kIndex, N
                CALL Pat%Set(Pattern(I), N-I)
            END DO
        END IF
    END SELECT

    RETURN

END SUBROUTINE HorspoolRaita_Process

!******************************************************************************

FUNCTION Horspool_Find(PatObj, Pattern, Text, StartID) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the zero-based index of the first occurrence of the pattern string
    !  in the text string.  Return the length of the text string if there is no such
    !  pattern found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters
    tChar,              INTENT(IN)      :: Text(0:)     !! the text string as an array of characters
    tIndex,             INTENT(IN)      :: StartID      !! (zero-based) starting index of the text string
    tIndex                              :: Index        !! (zero-based) index of the first occurrence

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, K, TxtLen

! FLOW

    ! Unrolled fast paths for patterns of length 1 and 2
    IF (PatObj%M <= 2_kIndex) THEN
        Index = Len2OrLess_Find(PatObj, Pattern, Text, StartID)
        RETURN
    END IF

    TxtLen = SIZE(Text, KIND=kIndex)

    ! for pattern length of 3 and more
    SELECT TYPE (Pat => PatObj)
    TYPE IS (ChrIntMap)
        BLOCK
            tIndex  :: LenM1
            ! execution
            LenM1 = Pat%M - 1_kIndex
            K = LenM1
            OutLoop: DO WHILE (K < TxtLen)
                J = LenM1
                I = K
                InLoop: DO WHILE ((J >= 0_kIndex).AND.(Text(I)==Pattern(J)).AND.(I >= StartID))
                    J = J - 1_kIndex
                    I = I - 1_kIndex
                END DO InLoop
                IF (J == -1_kIndex) THEN
                    ! pattern found
                    Index = I + 1_kIndex
                    RETURN
                END IF
                K = K + Pat%Get(Text(K))
            END DO OutLoop
            ! pattern not found
            Index = TxtLen
        END BLOCK
    END SELECT

    RETURN

END FUNCTION Horspool_Find

!******************************************************************************

FUNCTION Raita_Find(PatObj, Pattern, Text, StartID) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the zero-based index of the first occurrence of the pattern string
    !  in the text string.  Return the length of the text string if there is no such
    !  pattern found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters
    tChar,              INTENT(IN)      :: Text(0:)     !! the text string as an array of characters
    tIndex,             INTENT(IN)      :: StartID      !! (zero-based) starting index of the text string
    tIndex                              :: Index        !! (zero-based) index of the first occurrence

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, K, TxtLen

! FLOW

    ! Unrolled fast paths for patterns of length 1 and 2
    IF (PatObj%M <= 2_kIndex) THEN
        Index = Len2OrLess_Find(PatObj, Pattern, Text, StartID)
        RETURN
    END IF

    TxtLen = SIZE(Text, KIND=kIndex)

    ! for pattern length of 3 and more
    SELECT TYPE (Pat => PatObj)
    TYPE IS (ChrIntMap)
        BLOCK
            tIndex  :: LenM1, MM1
            tChar   :: Last, First
            ! execution
            LenM1 = Pat%M - 1_kIndex
            MM1   = LenM1 - 1_kIndex
            Last  = Pattern(LenM1)
            First = Pattern(0)
            I     = LenM1 + StartID
            DO WHILE (I < TxtLen)
                IF ((Text(I) == Last).AND.(Text(I-LenM1) == First)) THEN
                    K = I - 1_kIndex
                    J = MM1
                    DO WHILE ((K > -1_kIndex).AND.(J > -1_kIndex).AND.(Text(K)==Pattern(J)))
                        K = K - 1_kIndex
                        J = J - 1_kIndex
                    END DO
                    IF (J == -1_kIndex) THEN
                        ! pattern found
                        Index = K + 1_kIndex
                        RETURN
                    END IF
                END IF
                I = I + Pat%Get(Text(I))
            END DO
            ! pattern not found
            Index = TxtLen
        END BLOCK
    END SELECT

    RETURN

END FUNCTION Raita_Find

!******************************************************************************

SUBROUTINE Sunday_Process(PatObj, Pattern)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pre-process the specified pattern for Sunday algorithms.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: N, I

! FLOW

    N = SIZE(Pattern, KIND=kIndex)
    PatObj%M = N
    SELECT TYPE (Pat => PatObj)
    TYPE IS (ChrIntMap)
        IF (LEN(Pattern) > 2) THEN
            CALL Pat%Create(Pattern, N, N+1_kIndex)
            Pat%M = N       ! this statement must exists since the create method will make Pat%M undefined
            N = N - 1_kIndex
            DO I = 0_kIndex, N
                CALL Pat%Set(Pattern(I), N)
            END DO
        END IF
    END SELECT

    RETURN

END SUBROUTINE Sunday_Process

!******************************************************************************

FUNCTION Sunday_Find(PatObj, Pattern, Text, StartID) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the zero-based index of the first occurrence of the pattern string
    !  in the text string.  Return the length of the text string if there is no such
    !  pattern found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters
    tChar,              INTENT(IN)      :: Text(0:)     !! the text string as an array of characters
    tIndex,             INTENT(IN)      :: StartID      !! (zero-based) starting index of the text string
    tIndex                              :: Index        !! (zero-based) index of the first occurrence

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, K, TxtLen

! FLOW

    ! Unrolled fast paths for patterns of length 1 and 2
    IF (PatObj%M <= 2_kIndex) THEN
        Index = Len2OrLess_Find(PatObj, Pattern, Text, StartID)
        RETURN
    END IF

    TxtLen = SIZE(Text, KIND=kIndex)

    ! for pattern length of 3 and more
    SELECT TYPE (Pat => PatObj)
    TYPE IS (ChrIntMap)
        BLOCK
            tIndex  :: LenM1
            ! execution
            LenM1 = Pat%M - 1_kIndex
            K = LenM1
            OutLoop: DO WHILE (K < TxtLen)
                J = LenM1
                I = K
                InLoop: DO WHILE ((J >= 0_kIndex).AND.(Text(I)==Pattern(J)).AND.(I >= StartID))
                    J = J - 1_kIndex
                    I = I - 1_kIndex
                END DO InLoop
                IF (J == -1_kIndex) THEN
                    ! pattern found
                    Index = I + 1_kIndex
                    RETURN
                END IF
                K = K + 1_kIndex
                IF (K >= TxtLen) EXIT
                K = K + Pat%Get(Text(K))
            END DO OutLoop
            ! pattern not found
            Index = TxtLen
        END BLOCK
    END SELECT

    RETURN

END FUNCTION Sunday_Find

!******************************************************************************

SUBROUTINE BoyerMoore_Process(PatObj, Pattern)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pre-process the specified pattern for Boyer-Moore algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: J

! FLOW

    SELECT TYPE (Pat => PatObj)
    TYPE IS (BoyerMoorePat)
        Pat%M = SIZE(Pattern, KIND=kIndex)
        Pat%R = Radix
        ! position of rightmost occurrence of c in the pattern
        Pat%Right = -1_kIndex
        DO J = 0_kIndex, Pat%M - 1_kIndex
            Pat%Right(IACHAR(Pattern(J))) = J
        END DO
    END SELECT

    RETURN

END SUBROUTINE BoyerMoore_Process

!******************************************************************************

FUNCTION BoyerMoore_Find(PatObj, Pattern, Text, StartID) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the zero-based index of the first occurrence of the pattern string
    !  in the text string.  Return the length of the text string if there is no such
    !  pattern found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters
    tChar,              INTENT(IN)      :: Text(0:)     !! the text string as an array of characters
    tIndex,             INTENT(IN)      :: StartID      !! (zero-based) starting index of the text string
    tIndex                              :: Index        !! (zero-based) index of the first occurrence

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, N, Skip

! FLOW

    N = SIZE(Text, KIND=kIndex)
    SELECT TYPE (Pat => PatObj)
    TYPE IS (BoyerMoorePat)
        I = StartID
        OutLoop: DO
            Skip = 0_kIndex
            InLoop: DO J = Pat%M - 1_kIndex, 0_kIndex, -1_kIndex
                IF (Pattern(J) /= Text(I+J)) THEN
                    Skip = MAX(1_kIndex, J - Pat%Right(IACHAR(Text(I+J))))
                    EXIT InLoop
                END IF
            END DO InLoop
            IF (Skip == 0_kIndex) THEN
                ! pattern found
                Index = I
                RETURN
            END IF
            I = I + Skip
            IF (I > N - Pat%M) EXIT OutLoop
        END DO OutLoop
        ! pattern not found
        Index = N
    END SELECT

    RETURN

END FUNCTION BoyerMoore_Find

!******************************************************************************

SUBROUTINE KMPlus_Process(PatObj, Pattern)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pre-process the specified pattern for optimized Knuth-Morris-Pratt algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J

! FLOW

    SELECT TYPE (Pat => PatObj)
    TYPE IS (KMPlusPat)
        Pat%M = SIZE(Pattern, KIND=kIndex)
        ALLOCATE(Pat%Next(0:Pat%M-1))
        J = -1_kIndex
        DO I = 0_kIndex, Pat%M - 1_kIndex
            IF (I == 0_kIndex) THEN
                Pat%Next(I) = -1_kIndex
            ELSEIF (Pattern(I) /= Pattern(J)) THEN
                Pat%Next(I) = J
            ELSE
                Pat%Next(I) = Pat%Next(J)
            END IF
            DO WHILE ((J >= 0_kIndex).AND.(Pattern(I) /= Pattern(J)))
                J = Pat%Next(J)
            END DO
            J = J + 1_kIndex
        END DO
    END SELECT

    RETURN

END SUBROUTINE KMPlus_Process

!******************************************************************************

FUNCTION KMPlus_Find(PatObj, Pattern, Text, StartID) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the zero-based index of the first occurrence of the pattern string
    !  in the text string.  Return the length of the text string if there is no such
    !  pattern found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters
    tChar,              INTENT(IN)      :: Text(0:)     !! the text string as an array of characters
    tIndex,             INTENT(IN)      :: StartID      !! (zero-based) starting index of the text string
    tIndex                              :: Index        !! (zero-based) index of the first occurrence

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, N

! FLOW

    N = SIZE(Text, KIND=kIndex)
    SELECT TYPE (Pat => PatObj)
    TYPE IS (KMPlusPat)
        ! simulate operation of NFA on text
        J = 0_kIndex
        DO I = StartID, N-1_kIndex
            DO WHILE ((J >= 0_kIndex).AND.(Text(I) /= Pattern(J)))
                J = Pat%Next(J)
            END DO
            J = J + 1_kIndex
            IF (J == Pat%M) EXIT
        END DO
        IF (J == Pat%M) THEN
            ! pattern found
            Index = I - Pat%M
        ELSE
            ! pattern not found
            Index = N
        END IF
    END SELECT

    RETURN

END FUNCTION KMPlus_Find

!******************************************************************************

SUBROUTINE KMP_Process(PatObj, Pattern)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pre-process the specified pattern for Knuth-Morris-Pratt algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, K, X

! FLOW

    SELECT TYPE (Pat => PatObj)
    TYPE IS (KMPPat)
        Pat%M = SIZE(Pattern, KIND=kIndex)
        Pat%R = Radix
        ALLOCATE(Pat%DFA(0:Pat%M-1, 0:Pat%R-1))
        Pat%DFA(0, IACHAR(Pattern(0))) = 1_kIndex
        X = 0_kIndex
        DO J = 1_kIndex, Pat%M - 1_kIndex
            DO I = 0_kIndex, Pat%R - 1_kIndex
                Pat%DFA(J, I) = Pat%DFA(X, I)   ! Copy mismatch cases
            END DO
            K = IACHAR(Pattern(J))
            Pat%DFA(J, K) = J + 1_kIndex        ! Set match case
            X = Pat%DFA(X, K)                   ! Update restart state
        END DO
    END SELECT

    RETURN

END SUBROUTINE KMP_Process

!******************************************************************************

FUNCTION KMP_Find(PatObj, Text, StartID) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the zero-based index of the first occurrence of the pattern string
    !  in the text string.  Return the length of the text string if there is no such
    !  pattern found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Text(0:)     !! the text string as an array of characters
    tIndex,             INTENT(IN)      :: StartID      !! (zero-based) starting index of the text string
    tIndex                              :: Index        !! (zero-based) index of the first occurrence

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, N

! FLOW

    N = SIZE(Text, KIND=kIndex)
    SELECT TYPE (Pat => PatObj)
    TYPE IS (KMPPat)
        ! simulate operation of DFA on text
        J = 0_kIndex
        DO I = StartID, N-1_kIndex
            J = Pat%DFA(J, IACHAR(Text(I)))
            J = J + 1_kIndex
            IF (J == Pat%M) EXIT
        END DO
        IF (J == Pat%M) THEN
            ! pattern found
            Index = I - Pat%M
        ELSE
            ! pattern not found
            Index = N
        END IF
    END SELECT

    RETURN

END FUNCTION KMP_Find

!******************************************************************************

SUBROUTINE RabinKarp_Process(PatObj, Pattern)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pre-process the specified pattern for Rabin-Karp algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(Sfc64RNG)  :: RNG
    tSInt64         :: RandVal
    tIndex          :: I

! FLOW

    CALL RNG%Initialize()
    RandVal = RNG%NextLong(MAX_I32+1_kInt64, MAX_I64-MAX_I32)
    SELECT TYPE (Pat => PatObj)
    TYPE IS (RabinKarpPat)
        Pat%M = SIZE(Pattern, KIND=kIndex)
        Pat%R = Radix
        Pat%Q = LongRandomPrime(RandVal)
        ! pre-compute MOD(R**(M-1), Q) for use in removing leading digit
        Pat%RM = 1_kInt64
        DO I = 1_kIndex, Pat%M-1_kIndex
            Pat%RM = MOD(Pat%R*Pat%RM, Pat%Q)
        END DO
        Pat%Hash = ComputeHash(Pattern, Pat%M, Pat%R, Pat%Q)
    END SELECT

    RETURN

CONTAINS

    FUNCTION LongRandomPrime(RndVal) RESULT(PrmVal)
        ! To find a prime number larger than the given value
        tSInt64, INTENT(IN) :: RndVal
        tSInt64             :: PrmVal
        ! execution
        PrmVal = RndVal
        DO WHILE (.NOT.IsPrime(PrmVal))
            PrmVal = PrmVal + 1_kInt64
        END DO
        RETURN
    END FUNCTION LongRandomPrime

    !**************************************************************************

    FUNCTION ComputeHash(Key, M, R, Q) RESULT(Hash)
        ! To compute hash value for key(1:M)
        tCharStar, INTENT(IN)   :: Key(0:)
        tIndex,    INTENT(IN)   :: M
        tIndex,    INTENT(IN)   :: R
        tSInt64,   INTENT(IN)   :: Q
        tSInt64                 :: Hash
        tIndex                  :: J
        ! execution
        Hash = 0_kInt64
        DO J = 0_kIndex, M - 1_kIndex
            Hash = MOD(R*Hash + IACHAR(Key(J)), Q)
        END DO
        RETURN
    END FUNCTION ComputeHash

    !**************************************************************************

END SUBROUTINE RabinKarp_Process

!******************************************************************************

FUNCTION RabinKarp_Find(PatObj, Pattern, Text) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the zero-based index of the first occurrence of the pattern string
    !  in the text string.  Return the length of the text string if there is no such
    !  pattern found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters
    tChar,              INTENT(IN)      :: Text(0:)     !! the text string as an array of characters
    tIndex                              :: Index        !! (zero-based) index of the first occurrence

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, Offset, N
    tSInt64     :: TxtHash

! FLOW

    N = SIZE(Text, KIND=kIndex)
    SELECT TYPE (Pat => PatObj)
    TYPE IS (RabinKarpPat)
        TxtHash = ComputeHash(Text, Pat%M, Pat%R, Pat%Q)
        ! check for match at StartID
        IF ((Pat%Hash == TxtHash).AND.Check(0_kIndex, Pat%M)) THEN
            ! pattern found
            Index = 0_kIndex
            RETURN
        END IF
        ! check for hash match; if hash match, check for exact match
        DO I = Pat%M, N - 1_kIndex
            ! Remove leading digit, add trailing digit, check for match
            TxtHash = MOD(MOD((TxtHash + Pat%Q - Pat%RM*IACHAR(Text(I-Pat%M))), Pat%Q), Pat%Q)
            TxtHash = MOD((TxtHash*Pat%R + IACHAR(Text(I))), Pat%Q)
            Offset = I - Pat%M + 1_kIndex
            IF ((Pat%Hash == TxtHash).AND.Check(Offset, Pat%M)) THEN
                ! pattern found
                Index = Offset
                RETURN
            END IF
        END DO
        ! pattern not found
        Index = N
    END SELECT

    RETURN

CONTAINS

    FUNCTION Check(K, M) RESULT(Flag)
        ! Las Vegas version: does pat[] match txt[i..i-m+1] ?
        tIndex, INTENT(IN)  :: K
        tIndex, INTENT(IN)  :: M
        tLogical            :: Flag
        tIndex              :: J
        ! execution
        DO J = 0_kIndex, M - 1_kIndex
            IF (Pattern(J) /= Text(K+J)) THEN
                Flag = FalseVal
                RETURN
            END IF
        END DO
        Flag = TrueVal
        RETURN
    END FUNCTION Check

    !**************************************************************************

    FUNCTION ComputeHash(Key, M, R, Q) RESULT(Hash)
        ! To compute hash value for key(1:M)
        tCharStar, INTENT(IN)   :: Key(0:)
        tIndex,    INTENT(IN)   :: M
        tIndex,    INTENT(IN)   :: R
        tSInt64,   INTENT(IN)   :: Q
        tSInt64                 :: Hash
        tIndex                  :: J
        ! execution
        Hash = 0_kInt64
        DO J = 0_kIndex, M - 1_kIndex
            Hash = MOD(R*Hash + IACHAR(Key(J)), Q)
        END DO
        RETURN
    END FUNCTION ComputeHash

    !**************************************************************************

END FUNCTION RabinKarp_Find

!******************************************************************************

SUBROUTINE BruteForce_Process(PatObj, Pattern)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pre-process the specified pattern for brute-force algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (Pat => PatObj)
    TYPE IS (BasePattern)
        Pat%M = SIZE(Pattern, KIND=kIndex)
    END SELECT

    RETURN

END SUBROUTINE BruteForce_Process

!******************************************************************************

FUNCTION BruteForce_Find(PatObj, Pattern, Text, StartID) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the zero-based index of the first occurrence of the pattern string
    !  in the text string.  Return the length of the text string if there is no such
    !  pattern found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters
    tChar,              INTENT(IN)      :: Text(0:)     !! the text string as an array of characters
    tIndex,             INTENT(IN)      :: StartID      !! (zero-based) starting index of the text string
    tIndex                              :: Index        !! (zero-based) index of the first occurrence

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, N

! FLOW

    N = SIZE(Text, KIND=kIndex)
    SELECT TYPE (Pat => PatObj)
    TYPE IS (BasePattern)
        DO I = StartID, N - Pat%M
            DO J = 0_kIndex, Pat%M - 1_kIndex
                IF (Text(I+J) /= Pattern(J)) EXIT
            END DO
            IF (J == Pat%M) THEN
                ! pattern found
                Index = I
                RETURN
            END IF
        END DO
        ! pattern not found
        Index = N
    END SELECT

    RETURN

END FUNCTION BruteForce_Find

!******************************************************************************

SUBROUTINE ChrIntMap_Create(PatObj, Pattern, EndID, DefVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create the 'ChrIntMap' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ChrIntMap), INTENT(OUT)   :: PatObj       !! the 'ChrIntMap' object
    tChar,            INTENT(IN)    :: Pattern(0:)  !! the pattern as an array of characters
    tIndex,           INTENT(IN)    :: EndID        !! ending index
    tIndex,           INTENT(IN)    :: DefVal       !! default value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinVal, MaxVal

! FLOW

    CALL MinMax_Sensitive(Pattern, EndID, MinVal, MaxVal)
    CALL PatObj%Construct(MaxVal-MinVal+1_kIndex, MinVal, DefVal)

    RETURN

END SUBROUTINE ChrIntMap_Create

!******************************************************************************

SUBROUTINE ChrIntMap_Construct(PatObj, Extent, Lowest, DefVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create the 'ChrIntMap' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ChrIntMap), INTENT(OUT)   :: PatObj       !! the 'ChrIntMap' object
    tIndex,           INTENT(IN)    :: Extent       !! the extent of the pattern
    tIndex,           INTENT(IN)    :: Lowest       !! lowest code
    tIndex,           INTENT(IN)    :: DefVal       !! default value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ALLOCATE(PatObj%Arr(0:Extent-1))
    PatObj%Lowest = Lowest
    PatObj%DefVal = DefVal
    IF (DefVal /= 0_kIndex) THEN
        PatObj%Arr = DefVal
    ELSE
        PatObj%Arr = 0_kIndex
    END IF

    RETURN

END SUBROUTINE ChrIntMap_Construct

!******************************************************************************

SUBROUTINE ChrIntMap_Set(PatObj, C, Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the stored value for the given character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ChrIntMap), INTENT(INOUT) :: PatObj   !! the 'ChrIntMap' object
    tChar,            INTENT(IN)    :: C        !! the character
    tIndex,           INTENT(IN)    :: Val      !! the new value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: X

! FLOW

    X = IACHAR(C, KIND=kIndex) - PatObj%Lowest
    IF (X < SIZE(PatObj%Arr, KIND=kIndex)) PatObj%Arr(X) = Val

    RETURN

END SUBROUTINE ChrIntMap_Set

!******************************************************************************

FUNCTION ChrIntMap_Get(PatObj, C) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the stored value for the given character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ChrIntMap), INTENT(INOUT) :: PatObj   !! the 'ChrIntMap' object
    tChar,            INTENT(IN)    :: C        !! the character
    tIndex                          :: Val      !! the stored value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: X

! FLOW

    X = IACHAR(C, KIND=kIndex) - PatObj%Lowest
    IF ((X < 0_kIndex).OR.(X >= SIZE(PatObj%Arr, KIND=kIndex))) THEN
        Val = PatObj%DefVal
    ELSE
        Val = PatObj%Arr(X)
    END IF

    RETURN

END FUNCTION ChrIntMap_Get

!******************************************************************************

SUBROUTINE BNDMPatLen64_Create(PatObj, Pattern, EndID, DefVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create the 'BNDMPatLen64' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BNDMPatLen64), INTENT(OUT)    :: PatObj       !! the 'BNDMPatLen64' object
    tChar,               INTENT(IN)     :: Pattern(0:)  !! the pattern as an array of characters
    tIndex,              INTENT(IN)     :: EndID        !! ending index
    tSInt64,             INTENT(IN)     :: DefVal       !! default value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinVal, MaxVal

! FLOW

    CALL MinMax_Sensitive(Pattern, EndID, MinVal, MaxVal)
    CALL PatObj%Construct(MaxVal-MinVal+1_kIndex, MinVal, DefVal)

    RETURN

END SUBROUTINE BNDMPatLen64_Create

!******************************************************************************

SUBROUTINE BNDMPatLen64_Construct(PatObj, Extent, Lowest, DefVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create the 'BNDMPatLen64' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BNDMPatLen64), INTENT(OUT)    :: PatObj   !! the 'BNDMPatLen64' object
    tIndex,              INTENT(IN)     :: Extent   !! the extent of the pattern
    tIndex,              INTENT(IN)     :: Lowest   !! lowest code
    tSInt64,             INTENT(IN)     :: DefVal   !! default value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ALLOCATE(PatObj%Arr(0:Extent-1))
    PatObj%Lowest = Lowest
    PatObj%DefVal = DefVal
    IF (DefVal /= 0_kInt64) THEN
        PatObj%Arr = DefVal
    ELSE
        PatObj%Arr = 0_kInt64
    END IF

    RETURN

END SUBROUTINE BNDMPatLen64_Construct

!******************************************************************************

SUBROUTINE BNDMPatLen64_Set(PatObj, C, Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the stored value for the given character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BNDMPatLen64), INTENT(INOUT)  :: PatObj   !! the 'BNDMPatLen64' object
    tChar,               INTENT(IN)     :: C        !! the character
    tSInt64,             INTENT(IN)     :: Val      !! the new value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: X

! FLOW

    X = IACHAR(C, KIND=kIndex) - PatObj%Lowest
    IF (X < SIZE(PatObj%Arr, KIND=kIndex)) PatObj%Arr(X) = Val

    RETURN

END SUBROUTINE BNDMPatLen64_Set

!******************************************************************************

FUNCTION BNDMPatLen64_Get(PatObj, C) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the stored value for the given character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BNDMPatLen64), INTENT(INOUT)  :: PatObj   !! the 'BNDMPatLen64' object
    tChar,               INTENT(IN)     :: C        !! the character
    tSInt64                             :: Val      !! the stored value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: X

! FLOW

    X = IACHAR(C, KIND=kIndex) - PatObj%Lowest
    IF ((X < 0_kIndex).OR.(X >= SIZE(PatObj%Arr, KIND=kIndex))) THEN
        Val = PatObj%DefVal
    ELSE
        Val = PatObj%Arr(X)
    END IF

    RETURN

END FUNCTION BNDMPatLen64_Get

!******************************************************************************

SUBROUTINE BNDMPatLen128_Create(PatObj, Pattern, EndID, DefVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create the 'BNDMPatLen128' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BNDMPatLen128), INTENT(OUT)   :: PatObj       !! the 'BNDMPatLen128' object
    tChar,                INTENT(IN)    :: Pattern(0:)  !! the pattern as an array of characters
    tIndex,               INTENT(IN)    :: EndID        !! ending index
    tSInt128,             INTENT(IN)    :: DefVal       !! default value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MinVal, MaxVal

! FLOW

    CALL MinMax_Sensitive(Pattern, EndID, MinVal, MaxVal)
    CALL PatObj%Construct(MaxVal-MinVal+1_kIndex, MinVal, DefVal)

    RETURN

END SUBROUTINE BNDMPatLen128_Create

!******************************************************************************

SUBROUTINE BNDMPatLen128_Construct(PatObj, Extent, Lowest, DefVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create the 'BNDMPatLen128' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BNDMPatLen128), INTENT(OUT)   :: PatObj   !! the 'BNDMPatLen128' object
    tIndex,               INTENT(IN)    :: Extent   !! the extent of the pattern
    tIndex,               INTENT(IN)    :: Lowest   !! lowest code
    tSInt128,             INTENT(IN)    :: DefVal   !! default value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ALLOCATE(PatObj%Arr(0:Extent-1))
    PatObj%Lowest = Lowest
    PatObj%DefVal = DefVal
    IF (DefVal /= ZeroI128) THEN
        PatObj%Arr = DefVal
    ELSE
        PatObj%Arr = ZeroI128
    END IF

    RETURN

END SUBROUTINE BNDMPatLen128_Construct

!******************************************************************************

SUBROUTINE BNDMPatLen128_Set(PatObj, C, Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the stored value for the given character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BNDMPatLen128), INTENT(INOUT) :: PatObj   !! the 'BNDMPatLen128' object
    tChar,                INTENT(IN)    :: C        !! the character
    tSInt128,             INTENT(IN)    :: Val      !! the new value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: X

! FLOW

    X = IACHAR(C, KIND=kIndex) - PatObj%Lowest
    IF (X < SIZE(PatObj%Arr, KIND=kIndex)) PatObj%Arr(X) = Val

    RETURN

END SUBROUTINE BNDMPatLen128_Set

!******************************************************************************

FUNCTION BNDMPatLen128_Get(PatObj, C) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the stored value for the given character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BNDMPatLen128), INTENT(INOUT) :: PatObj   !! the 'BNDMPatLen128' object
    tChar,                INTENT(IN)    :: C        !! the character
    tSInt128                            :: Val      !! the stored value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: X

! FLOW

    X = IACHAR(C, KIND=kIndex) - PatObj%Lowest
    IF ((X < 0_kIndex).OR.(X >= SIZE(PatObj%Arr, KIND=kIndex))) THEN
        Val = PatObj%DefVal
    ELSE
        Val = PatObj%Arr(X)
    END IF

    RETURN

END FUNCTION BNDMPatLen128_Get

!******************************************************************************

SUBROUTINE MinMax_Insensitive(PatVal, Min, Max)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find minimum and maximum character codes of the given pattern.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: PatVal(0:)
    tIndex, INTENT(OUT) :: Min, Max

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: ID, K
    tChar       :: PC

! FLOW

    Min = Radix - 1_kIndex
    Max = 0_kIndex
    DO ID = 0_kIndex, SIZE(PatVal,KIND=kIndex) - 1_kIndex
        PC = PatVal(ID)
        IF (IsLetter(PC)) THEN
            ! the following code is based on the fact that
            ! upper-case codes are smaller than lower-case codes.
            K = IACHAR(ToLower(PC), KIND=kIndex)
            IF (Max < K) Max = K
            K = IACHAR(ToUpper(PC), KIND=kIndex)
            IF (Min > K) Min = K
        ELSE
            K = IACHAR(PC, KIND=kIndex)
            IF (Max < K) Max = K
            IF (Min > K) Min = K
        END IF
    END DO

    RETURN

END SUBROUTINE MinMax_Insensitive

!******************************************************************************

SUBROUTINE MinMax_Sensitive(PatVal, EndID, Min, Max)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find minimum and maximum character codes of the given pattern.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,  INTENT(IN)  :: PatVal(0:)
    tIndex, INTENT(IN)  :: EndID
    tIndex, INTENT(OUT) :: Min, Max

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, C

! FLOW

    Min = Radix - 1_kIndex
    Max = 0_kIndex
    DO I = 0_kIndex, EndID - 1_kIndex
        C = IACHAR(PatVal(I), KIND=kIndex)
        IF (Max < C) Max = C
        IF (Min > C) Min = C
    END DO

    RETURN

END SUBROUTINE MinMax_Sensitive

!******************************************************************************

FUNCTION Len2OrLess_Find(PatObj, Pattern, Text, StartID) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the zero-based index of the first occurrence of the pattern string
    !  in the text string.  Return the length of the text string if there is no such
    !  pattern found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BasePattern), INTENT(INOUT)   :: PatObj       !! the pattern data object
    tChar,              INTENT(IN)      :: Pattern(0:)  !! the pattern as an array of characters
    tChar,              INTENT(IN)      :: Text(0:)     !! the text string as an array of characters
    tIndex,             INTENT(IN)      :: StartID      !! (zero-based) starting index of the text string
    tIndex                              :: Index        !! (zero-based) index of the first occurrence

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, TxtLen

! FLOW

    TxtLen = SIZE(Text, KIND=kIndex)

    ! Unrolled fast paths for patterns of length 1 and 2
    IF (PatObj%M == 1_kIndex) THEN
        DO I = StartID, TxtLen - 1_kIndex
            IF (Text(I) == Pattern(0)) THEN
                ! pattern found
                Index = I
                RETURN
            END IF
        END DO
        ! pattern not found
        Index = TxtLen
    ELSEIF (PatObj%M == 2_kIndex) THEN
        DO I = StartID, TxtLen - 2_kIndex
            IF (Text(I) == Pattern(0)) THEN
                IF (Text(I+1) == Pattern(1)) THEN
                    ! pattern found
                    Index = I
                    RETURN
                END IF
            END IF
        END DO
        ! pattern not found
        Index = TxtLen
    END IF

    RETURN

END FUNCTION Len2OrLess_Find

!******************************************************************************

END MODULE MClass_PatternFinder

!******************************************************************************
