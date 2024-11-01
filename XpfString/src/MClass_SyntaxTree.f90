
MODULE MClass_SyntaxTree

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SyntaxTree* type and its related routines.
!   The *SyntaxTree* type is a derived type representing a ... <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_CharUtil
    USE MBase_MemHandlers
    USE MBase_ErrHandlers
    USE MBase_DoublyLinkedLists,  ONLY: ListInteger  => ListInteger4B
    USE MBase_ChrStr,             ONLY: ParseInteger
    USE MClass_IntrusiveLinkedLists
    USE MClass_StringBuilder
    USE MClass_SyntaxNode

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: SyntaxTree
    PUBLIC :: EncodingLen

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'Class_SyntaxTree'
    tSInt32,   PARAMETER    :: MsgLen  = 128
#ifdef  __INTEL_COMPILER
    tSInt32,   PARAMETER    :: SetAlphaNum(*)    = [(IACHAR(SET_ALPHANUM(Idx:Idx)), &
                                                     tSInt32 :: Idx = 1, LEN(SET_ALPHANUM))]
    tSInt32,   PARAMETER    :: SetWhiteSpace(*)  = [(IACHAR(SET_WHITESPACES(Idx:Idx)), &
                                                     tSInt32 :: Idx = 1, LEN(SET_WHITESPACES))]
    tSInt32,   PARAMETER    :: SetDigit(*)       = [(IACHAR(SET_DIGITS(Idx:Idx)), &
                                                     tSInt32 :: Idx = 1, LEN(SET_DIGITS))]
#else
    tSInt32                 :: Idx 
    tSInt32,   PARAMETER    :: SetAlphaNum(*)    = [(IACHAR(SET_ALPHANUM(Idx:Idx)), &
                                                     Idx = 1, LEN(SET_ALPHANUM))]
    tSInt32,   PARAMETER    :: SetWhiteSpace(*)  = [(IACHAR(SET_WHITESPACES(Idx:Idx)), &
                                                     Idx = 1, LEN(SET_WHITESPACES))]
    tSInt32,   PARAMETER    :: SetDigit(*)       = [(IACHAR(SET_DIGITS(Idx:Idx)), &
                                                     Idx = 1, LEN(SET_DIGITS))]
#endif
    tSInt32,   PARAMETER    :: SetNewLine(1)     = [IACHAR(CHR_NEWLINE)]
    tSInt32,   PARAMETER    :: SetSlashLowerW(*) = [SetAlphaNum, 95]
    tSInt32,   PARAMETER    :: SetSlashLowerS(*) = SetWhiteSpace
    tSInt32,   PARAMETER    :: SetSlashLowerD(*) = SetDigit
    tSInt32,   PARAMETER    :: EncodingLen       = 128
    tCharParam              :: SET_WORDS         = SET_ALPHANUM // CHR_UNDERSCORE

!** DERIVED TYPE DEFINITIONS
    !> The *OperatingStack* type is a stack type...
    TYPE    :: OperatingStack
        TYPE(IntrusiveLinearList)   :: Stack
    CONTAINS
        PROCEDURE   :: Visit    => OperatingStack_Visit
        PROCEDURE   :: Pop      => OperatingStack_Pop
        PROCEDURE   :: IsEmpty  => OperatingStack_IsEmpty
    END TYPE OperatingStack
    !> The *ShuntingStack* type is a stack type... using Shunting-Yard algorithm.
    TYPE    :: ShuntingStack
        TYPE(IntrusiveLinearList)   :: FinalStack
        TYPE(IntrusiveLinearList)   :: BranchStack
    CONTAINS
        PROCEDURE   :: Visit    => ShuntingStack_Visit
        PROCEDURE   :: Finish   => ShuntingStack_Finish
    END TYPE ShuntingStack
    !> The *SyntaxTree* type is a tree type...
    TYPE    :: SyntaxTree
        PRIVATE
        tCharAlloc                  :: Regex
        tLogical                    :: ItemTerminated = .FALSE.
        TYPE(IntrusiveLinearList)   :: List
        TYPE(IntrusiveLinearList)   :: Stack
        TYPE(SyntaxNode), POINTER   :: Root => NULL()
    CONTAINS
        PRIVATE
        PROCEDURE, PUBLIC   :: Construct    => SyntaxTree_Construct
        PROCEDURE, PUBLIC   :: Destruct     => SyntaxTree_Destruct
        PROCEDURE, PUBLIC   :: GetRoot      => SyntaxTree_GetRoot
        PROCEDURE   :: Build            => SyntaxTree_Build
        PROCEDURE   :: Shunt            => SyntaxTree_Shunt
        PROCEDURE   :: Normalize        => SyntaxTree_Normalize
        PROCEDURE   :: PerformMany      => SyntaxTree_PerformMany
        PROCEDURE   :: TryConcatenation => SyntaxTree_TryConcatenation
    END TYPE SyntaxTree

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE OperatingStack_Visit(OpStack, Node)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform stack operation(s) on the specified node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(OperatingStack), INTENT(INOUT)    :: OpStack
    TYPE(SyntaxNode),      INTENT(INOUT)    :: Node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT CASE (Node%Type)
    CASE (BranchNode_Or, BranchNode_Many, BranchNode_Concat, BranchNode_LBracket, BranchNode_RBracket)
        BLOCK
            tLogical                            :: Success
            CLASS(DoublyLinkedNode), POINTER    :: Right, Left
            Success = OpStack%Stack%Pop(Right)
            Success = OpStack%Stack%Pop(Left)
            SELECT CASE (Node%Type)
            CASE (BranchNode_Or, BranchNode_Many, BranchNode_Concat)
                IF (ASSOCIATED(Right).AND.ASSOCIATED(Left)) THEN
                    CALL Node%Operate(Left, Right)
                END IF
            END SELECT
            CALL OpStack%Stack%Push(Node)
        END BLOCK
    CASE (LeafNode_Null, LeafNode_Char, LeafNode_Closure)
        CALL OpStack%Stack%Push(Node)
    END SELECT

    RETURN

END SUBROUTINE OperatingStack_Visit

!******************************************************************************

FUNCTION OperatingStack_Pop(OpStack, NodeOut) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform stack operation(s) on the specified node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(OperatingStack),     INTENT(INOUT)    :: OpStack
    TYPE(SyntaxNode), POINTER, INTENT(INOUT)    :: NodeOut
    tLogical                                    :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: TopNode

! FLOW

    Success = OpStack%Stack%Pop(TopNode)
    SELECT TYPE (TopNode)
    TYPE IS (SyntaxNode)
        NodeOut => TopNode
    END SELECT
    NULLIFY(TopNode)
    CALL OpStack%Stack%Clear(DelinkOnly=TrueVal)

    RETURN

END FUNCTION OperatingStack_Pop

!******************************************************************************

FUNCTION OperatingStack_IsEmpty(OpStack) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform stack operation(s) on the specified node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(OperatingStack), INTENT(IN)   :: OpStack
    tLogical                            :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IsEmpty = OpStack%Stack%IsEmpty()

    RETURN

END FUNCTION OperatingStack_IsEmpty

!******************************************************************************

SUBROUTINE ShuntingStack_Visit(StStack, InNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform stack operation(s) on the specified node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShuntingStack), INTENT(INOUT) :: StStack
    TYPE(SyntaxNode),     INTENT(INOUT) :: InNode

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT CASE (InNode%Type)
    CASE (BranchNode_LBracket)
        CALL StStack%BranchStack%Push(InNode)
    CASE (BranchNode_RBracket)
        BLOCK
            tLogical                            :: Success
            CLASS(DoublyLinkedNode), POINTER    :: TopNode
            TYPE(SyntaxNode),        POINTER    :: CurrNode
            Success = StStack%BranchStack%Pop(TopNode)
            IF (Success) THEN
                SELECT TYPE (TopNode)
                TYPE IS (SyntaxNode)
                    CurrNode => TopNode
                END SELECT
                DO WHILE (CurrNode%Type /= BranchNode_LBracket)
                    CALL StStack%FinalStack%Push(CurrNode)
                    Success = StStack%BranchStack%Pop(TopNode)
                    SELECT TYPE (TopNode)
                    TYPE IS (SyntaxNode)
                        CurrNode => TopNode
                    END SELECT
                END DO
            END IF
            NULLIFY(TopNode, CurrNode)
        END BLOCK
    CASE (BranchNode_Or, BranchNode_Many, BranchNode_Concat)
        BLOCK
            tLogical                            :: Success
            CLASS(DoublyLinkedNode), POINTER    :: TopNode
            DO WHILE (.NOT.StStack%BranchStack%IsEmpty())
                TopNode => StStack%BranchStack%PeekTop()
                SELECT TYPE (TopNode)
                TYPE IS (SyntaxNode)
                    IF (InNode%ID > TopNode%ID) EXIT
                END SELECT
                Success = StStack%BranchStack%Pop(TopNode)
                CALL StStack%FinalStack%Push(TopNode)
            END DO
            CALL StStack%FinalStack%Push(InNode)
            NULLIFY(TopNode)
        END BLOCK
    CASE (LeafNode_Null, LeafNode_Char, LeafNode_Closure)
        CALL StStack%FinalStack%Push(InNode)
    END SELECT

    RETURN

END SUBROUTINE ShuntingStack_Visit

!******************************************************************************

SUBROUTINE ShuntingStack_Finish(StStack, ReversedStack)

!** PURPOSE OF THIS SUBROUTINE:
    !! To return a reversed stack of syntax nodes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShuntingStack),      INTENT(INOUT)    :: StStack
    TYPE(IntrusiveLinearList), INTENT(OUT)      :: ReversedStack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical                            :: Success
    CLASS(DoublyLinkedNode), POINTER    :: TopNode

! FLOW

    DO WHILE (.NOT.StStack%BranchStack%IsEmpty())
        Success = StStack%BranchStack%Pop(TopNode)
        CALL StStack%FinalStack%Push(TopNode)
    END DO
    DO WHILE (.NOT.StStack%FinalStack%IsEmpty())
        Success = StStack%FinalStack%Pop(TopNode)
        CALL ReversedStack%Push(TopNode)
    END DO

    RETURN

END SUBROUTINE ShuntingStack_Finish

!******************************************************************************

SUBROUTINE BookToSet(Book, Cardinality, Set)

!** PURPOSE OF THIS SUBROUTINE:
    !! To return a set according to the specified book.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLogical, INTENT(IN)    :: Book(0:EncodingLen-1)
    tSInt32,  INTENT(IN)    :: Cardinality
    tSInt32,  INTENT(OUT)   :: Set(0:Cardinality-1)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I, J

! FLOW

    I = 0
    DO J = 0, EncodingLen-1
        IF (Book(J)) THEN
            Set(I) = J
            I = I + 1
        END IF
    END DO

    RETURN

END SUBROUTINE BookToSet

!******************************************************************************

SUBROUTINE Set_Complementary(Set, Comp)

!** PURPOSE OF THIS SUBROUTINE:
    !! To return a complementary set of the specified set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: Set(0:)
    tSInt32,  INTENT(OUT)   :: Comp(0:EncodingLen-SIZE(Set)-1)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Book(0:EncodingLen-1)    ! flag for complementary characters
    tSInt32     :: Cardinality              ! number of unique characters
    tSInt32     :: I, J

! FLOW

    ! initialize
    Book = TrueVal
    Cardinality = EncodingLen

    ! set book for complementary set
    DO I = 0, SIZE(Set)-1
        J = Set(I)
        IF (Book(J)) THEN
            Cardinality = Cardinality - 1
            Book(J) = FalseVal
        END IF
    END DO

    ! get complementary set
    CALL BookToSet(Book, Cardinality, Comp)

    RETURN

END SUBROUTINE Set_Complementary

!******************************************************************************

SUBROUTINE Set_Minimum(Set, MinSet)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a minimum set of the specified set.
    ! For example, [e, a, d, f, f, c, c, k, \s] -> {a, c, d, e, f, k, \0, \t}.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,               INTENT(IN)   :: Set(0:)
    tSInt32,  ALLOCATABLE, INTENT(OUT)  :: MinSet(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Book(0:EncodingLen-1)    ! flag for minimum-set characters
    tSInt32     :: Cardinality              ! number of unique characters
    tSInt32     :: I, J

! FLOW

    ! initialize
    Book = FalseVal
    Cardinality = 0

    ! set book for minimum set
    DO I = 0, SIZE(Set)-1
        J = Set(I)
        IF (.NOT.Book(J)) THEN
            Cardinality = Cardinality + 1
            Book(J) = TrueVal
        END IF
    END DO

    ! get minimum set
    CALL MemAlloc(MinSet, ToIndex(Cardinality), StartID=0_kIndex)
    CALL BookToSet(Book, Cardinality, MinSet)

    RETURN

END SUBROUTINE Set_Minimum

!******************************************************************************

SUBROUTINE InterpretToken(Token, ChrSet)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a character set according to the specified token.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,             INTENT(IN)   :: Token
    tSInt32,  ALLOCATABLE, INTENT(OUT)  :: ChrSet(:)

!** SUBROUTINE PARAMETER DECLARATIONS:
    tChar, PARAMETER    :: ESCAPE_T = 't'
    tChar, PARAMETER    :: ESCAPE_N = 'n'
    tChar, PARAMETER    :: ESCAPE_R = 'r'
    tChar, PARAMETER    :: ESCAPE_D = 'd'
    tChar, PARAMETER    :: ESCAPE_W = 'w'
    tChar, PARAMETER    :: ESCAPE_S = 's'
    tChar, PARAMETER    :: ESCAPE_D_CAPITAL = 'D'
    tChar, PARAMETER    :: ESCAPE_W_CAPITAL = 'W'
    tChar, PARAMETER    :: ESCAPE_S_CAPITAL = 'S'

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (LEN(Token) == 1) THEN
        IF (Token(1:1) == CHR_PERIOD) THEN
            ! a character set that matches any single character except the new line character
            BLOCK
                tSInt32     :: SetDot(EncodingLen-1)
                CALL Set_Complementary(SetNewLine, SetDot)
                CALL GetTokenSet(SetDot, ChrSet)
            END BLOCK
        ELSE
            ! a character set that matches the specified character
            CALL MemAlloc(ChrSet, 1_kIndex, StartID=0_kIndex)
            ChrSet(0) = IACHAR(Token(1:1))
        END IF
    ELSEIF ((LEN(Token) /= 2).OR.(Token(1:1) /= CHR_BACKSLASH)) THEN
        CALL Handle_ErrLevel('InterpretToken', ModName, ErrSevere, 'Unrecognized token: ' // Token)
    ELSE
        SELECT CASE (Token(2:2))
        CASE (ESCAPE_N)
            ! a character set that matches the new line character
            CALL MemAlloc(ChrSet, 1_kIndex, StartID=0_kIndex)
            ChrSet(0) = IACHAR(CHR_NEWLINE)
        CASE (ESCAPE_R)
            ! a character set that matches the carriage return character
            CALL MemAlloc(ChrSet, 1_kIndex, StartID=0_kIndex)
            ChrSet(0) = IACHAR(CHR_CARRIAGE_RETURN)
        CASE (ESCAPE_T)
            ! a character set that matches the (horizontal) tab character
            CALL MemAlloc(ChrSet, 1_kIndex, StartID=0_kIndex)
            ChrSet(0) = IACHAR(CHR_TAB)
        CASE (ESCAPE_W)
            ! a character set that matches the word characters
            CALL GetTokenSet(SetSlashLowerW, ChrSet)
        CASE (ESCAPE_W_CAPITAL)
            ! a character set that matches the non-word characters
            BLOCK
                tSInt32     :: SetSlashUpperW(EncodingLen-SIZE(SetSlashLowerW))
                CALL Set_Complementary(SetSlashLowerW, SetSlashUpperW)
                CALL GetTokenSet(SetSlashUpperW, ChrSet)
            END BLOCK
        CASE (ESCAPE_S)
            ! a character set that matches the white-space characters
            CALL GetTokenSet(SetSlashLowerS, ChrSet)
        CASE (ESCAPE_S_CAPITAL)
            ! a character set that matches the non-white-space characters
            BLOCK
                tSInt32     :: SetSlashUpperS(EncodingLen-SIZE(SetSlashLowerS))
                CALL Set_Complementary(SetSlashLowerS, SetSlashUpperS)
                CALL GetTokenSet(SetSlashUpperS, ChrSet)
            END BLOCK
        CASE (ESCAPE_D)
            ! a character set that matches the digit characters
            CALL GetTokenSet(SetSlashLowerD, ChrSet)
        CASE (ESCAPE_D_CAPITAL)
            ! a character set that matches the non-digit characters
            BLOCK
                tSInt32     :: SetSlashUpperD(EncodingLen-SIZE(SetSlashLowerD))
                CALL Set_Complementary(SetSlashLowerD, SetSlashUpperD)
                CALL GetTokenSet(SetSlashUpperD, ChrSet)
            END BLOCK
        CASE DEFAULT
            ! a character set that matches the specified character
            CALL MemAlloc(ChrSet, 1_kIndex, StartID=0_kIndex)
            ChrSet(0) = IACHAR(Token(2:2))
        END SELECT
    END IF

    RETURN

END SUBROUTINE InterpretToken

!******************************************************************************

SUBROUTINE GetTokenSet(Token, ChrSet)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a character set according to the specified token.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,               INTENT(IN)   :: Token(0:)
    tSInt32,  ALLOCATABLE, INTENT(OUT)  :: ChrSet(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemAlloc(ChrSet, SIZE(Token, KIND=kIndex), StartID=0_kIndex)
    ChrSet(0:) = Token(0:)

    RETURN

END SUBROUTINE GetTokenSet

!******************************************************************************

SUBROUTINE List_AddNewNode(List, NodeType)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the concatenation node if applicable.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(IntrusiveLinearList), INTENT(INOUT)    :: List
    tSInt32,                   INTENT(IN)       :: NodeType

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SyntaxNode), POINTER   :: Node

! FLOW

    CALL SyntaxNode_New(Node, NodeType)
    CALL List%AddLast(Node)
    NULLIFY(Node)

    RETURN

END SUBROUTINE List_AddNewNode

!******************************************************************************

SUBROUTINE List_AddNewCharNode(List, CharCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the concatenation node if applicable.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(IntrusiveLinearList), INTENT(INOUT)    :: List
    tSInt32,                   INTENT(IN)       :: CharCode

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SyntaxNode), POINTER   :: Node

! FLOW

    CALL SyntaxNode_New(Node, LeafNode_Char)
    CALL Node%SetChar(ACHAR(CharCode))
    CALL List%AddLast(Node)
    NULLIFY(Node)

    RETURN

END SUBROUTINE List_AddNewCharNode

!******************************************************************************

SUBROUTINE SyntaxTree_PerformMany(Tree, Least, Most)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To look back for a completed term.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxTree), INTENT(INOUT)    :: Tree
    tSInt32,           INTENT(IN)       :: Least, Most

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SyntaxNode), POINTER   :: Node

! FLOW

    IF (.NOT.((Least == 1).AND.(Most == 1))) THEN
        IF ((Least == 0).AND.(Most == -1)) THEN
            CALL List_AddNewNode(Tree%List, BranchNode_Many)
            CALL List_AddNewNode(Tree%List, LeafNode_Null)
        ELSE
            BLOCK
                TYPE(IntrusiveLinearList)           :: Sample
                CLASS(DoublyLinkedNode), POINTER    :: LastNode
                tLogical                            :: Success
                tSInt32                             :: Stack
                tIndex                              :: I, J
                ! check whether the last node is the right bracket one
                LastNode => Tree%List%GetTail()
                SELECT TYPE (LastNode)
                TYPE IS (SyntaxNode)
                    Node => LastNode
                END SELECT
                NULLIFY(LastNode)
                IF (ASSOCIATED(Node)) THEN
                    CALL Sample%AddLast(Node)
                    Success = Tree%List%RemoveLast()
                    IF (Node%Type == BranchNode_RBracket) THEN
                        Stack = 1
                        DO I = Tree%List%GetSize(), 1, -1
                            ! note: the RemoveAt method uses one-based index.
                            Success = Tree%List%RemoveAt(I, LastNode)
                            SELECT TYPE (LastNode)
                            TYPE IS (SyntaxNode)
                                Node => LastNode
                            END SELECT
                            NULLIFY(LastNode)
                            IF (Node%Type == BranchNode_RBracket) THEN
                                Stack = Stack + 1
                            ELSEIF (Node%Type == BranchNode_LBracket) THEN
                                Stack = Stack - 1
                            END IF
                            CALL Sample%AddFirst(Node)
                            IF (Stack == 0) EXIT
                        END DO
                    END IF
                END IF
                IF (Most == -1) THEN
                    DO I = 0, Least-1
                        CALL List_AddAllNodes(Tree%List, Sample)
                        CALL List_AddNewNode(Tree%List, BranchNode_Concat)
                    END DO
                    CALL List_AddAllNodes(Tree%List, Sample)
                    CALL List_AddNewNode(Tree%List, BranchNode_Many)
                    CALL List_AddNewNode(Tree%List, LeafNode_Null)
                ELSE
                    IF (Least /= Most) THEN
                        CALL List_AddNewNode(Tree%List, BranchNode_LBracket)
                        DO I = Least, Most
                            CALL List_AddNewNode(Tree%List, BranchNode_LBracket)
                            IF (I == 0) THEN
                                CALL List_AddNewNode(Tree%List, LeafNode_Closure)
                            ELSE
                                DO J = 0, I-1
                                    CALL List_AddAllNodes(Tree%List, Sample)
                                    IF (J /= I-1) CALL List_AddNewNode(Tree%List, BranchNode_Concat)
                                END DO
                            END IF
                            CALL List_AddNewNode(Tree%List, BranchNode_RBracket)
                            IF (I /= Most) CALL List_AddNewNode(Tree%List, BranchNode_Or)
                        END DO
                        CALL List_AddNewNode(Tree%List, BranchNode_RBracket)
                    ELSE
                        CALL List_AddNewNode(Tree%List, BranchNode_LBracket)
                        DO I = 0, Least-1
                            CALL List_AddAllNodes(Tree%List, Sample)
                            IF (I /= Least-1) CALL List_AddNewNode(Tree%List, BranchNode_Concat)
                        END DO
                        CALL List_AddNewNode(Tree%List, BranchNode_RBracket)
                    END IF
                END IF
                CALL Sample%Clear()
            END BLOCK
        END IF
    END IF

    RETURN

CONTAINS

    SUBROUTINE List_AddAllNodes(List, Sample)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To add the concatenation node if applicable.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(IntrusiveLinearList), INTENT(INOUT)    :: List
        TYPE(IntrusiveLinearList), INTENT(INOUT)    :: Sample

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CLASS(DoublyLinkedNode), POINTER    :: CurrNode
        CLASS(DoublyLinkedNode), POINTER    :: CopyNode
        tLogical                            :: EndOfList

    ! FLOW

        EndOfList = Sample%StartFirst(CurrNode)
        DO WHILE (.NOT.EndOfList)
            ALLOCATE(CopyNode, SOURCE=CurrNode)
            CALL List%AddLast(CopyNode)
            NULLIFY(CopyNode)
            EndOfList = Sample%MoveForward(CurrNode)
        END DO

        RETURN

    END SUBROUTINE List_AddAllNodes

    !**************************************************************************

END SUBROUTINE SyntaxTree_PerformMany

!******************************************************************************

SUBROUTINE SyntaxTree_TryConcatenation(Tree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the concatenation node if applicable.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxTree), INTENT(INOUT)    :: Tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Tree%ItemTerminated) THEN
        CALL List_AddNewNode(Tree%List, BranchNode_Concat)
        Tree%ItemTerminated = FalseVal
    END IF

    RETURN

END SUBROUTINE SyntaxTree_TryConcatenation

!******************************************************************************

FUNCTION SyntaxTree_GetRoot(Tree) RESULT(Root)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the root node of the syntax tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxTree), INTENT(IN)   :: Tree
    TYPE(SyntaxNode),  POINTER      :: Root

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Root => Tree%Root

    RETURN

END FUNCTION SyntaxTree_GetRoot

!******************************************************************************

SUBROUTINE SyntaxTree_Normalize(Tree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To parse the regular expression pattern.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxTree), INTENT(INOUT)    :: Tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: ID
    tChar       :: Chr, Nxt

! FLOW

    ID = 1
    DO WHILE (ID <= LEN(Tree%Regex))
        CaseBlock: SELECT CASE (Chr)
        CASE ('[')
            CALL Tree%TryConcatenation()
            BLOCK
                TYPE(ListInteger)       :: AllChar
                tLogical                :: IsComplementarySet
                tLogical                :: Success
                tSInt32,  ALLOCATABLE   :: AllChrSet(:)
                tSInt32,  ALLOCATABLE   :: MinChrSet(:)
                tSInt32,  ALLOCATABLE   :: I
                IF (Tree%Regex(ID:ID) == '^') THEN
                    IsComplementarySet = TrueVal
                    ID = ID + 1
                ELSE
                    IsComplementarySet = FalseVal
                END IF
                Nxt = Tree%Regex(ID:ID)
                ID = ID + 1
                DO WHILE (Nxt /= ']')
                    IF ((Nxt == '\').OR.(Nxt /= '.')) THEN
                        BLOCK
                            tSInt32,  ALLOCATABLE   :: ChrSet(:)
                            tSInt32                 :: I
                            IF (Nxt == '\') THEN
                                CALL InterpretToken(Tree%Regex(ID-1:ID), ChrSet)
                                ID = ID + 1
                            ELSE
                                CALL InterpretToken(Nxt, ChrSet)
                            END IF
                            DO I = 0, SIZE(ChrSet)-1
                                ! note: ChrSet is a zero-based array.
                                CALL AllChar%AddLast(ChrSet(I))
                            END DO
                        END BLOCK
                    ELSE
                        CALL AllChar%AddLast(IACHAR(Nxt))
                    END IF
                    Nxt = Tree%Regex(ID:ID)
                    ID = ID + 1
                END DO
                Success = AllChar%ToArray(AllChrSet)
                CALL Set_Minimum(AllChrSet, MinChrSet)
                IF (IsComplementarySet) THEN
                    CALL MemAlloc(AllChrSet, EncodingLen-SIZE(MinChrSet,KIND=kIndex), StartID=0_kIndex)
                    CALL Set_Complementary(MinChrSet, AllChrSet)
                ELSE
                    CALL MOVE_ALLOC(MinChrSet, AllChrSet)
                END IF
                CALL List_AddNewNode(Tree%List, BranchNode_LBracket)
                DoBlock: DO I = 0, SIZE(AllChrSet)-1
                    CALL List_AddNewCharNode(Tree%List, AllChrSet(I))
                    IF (I == SIZE(AllChrSet)-1) THEN
                        EXIT DoBlock
                    ELSEIF (AllChrSet(I+1) == 0) THEN
                        EXIT DoBlock
                    END IF
                    CALL List_AddNewNode(Tree%List, BranchNode_Or)
                END DO DoBlock
                CALL List_AddNewNode(Tree%List, BranchNode_RBracket)
                Tree%ItemTerminated = TrueVal
            END BLOCK
        CASE ('{')
            BLOCK
                tSInt32             :: Least, Most
                tLogical            :: DeterministicLength
                TYPE(StringBuilder) :: SB
                tCharAlloc          :: CurStr
                tChar               :: NxtNxt
                ! initialize
                Most = -1
                DeterministicLength = FalseVal
                CALL SB%CreateEmpty(InitCap=256_kIndex)
                Nxt = Tree%Regex(ID:ID)
                ID = ID + 1
                DoLoop: DO
                    CALL SB%Append(Nxt)
                    Nxt = Tree%Regex(ID:ID)
                    ID = ID + 1
                    IF (Nxt == '}') THEN
                        DeterministicLength = TrueVal
                        EXIT DoLoop
                    ELSEIF (Nxt == ',') THEN
                        EXIT DoLoop
                    END IF
                END DO DoLoop
                CurStr = SB%ToCharAlloc(ClearBuffer=.TRUE.)
                Least = ParseInteger(CurStr)
                IF (.NOT.DeterministicLength) THEN
                    Nxt = Tree%Regex(ID:ID)
                    IF (Nxt /= '}') THEN
                        NxtNxt = Tree%Regex(ID:ID)
                        ID = ID + 1
                        DO WHILE (NxtNxt /= '}')
                            CALL SB%Append(NxtNxt)
                            NxtNxt = Tree%Regex(ID:ID)
                            ID = ID + 1
                        END DO
                        IF (SB%Length() /= 0) THEN
                            CurStr = SB%ToCharAlloc(ClearBuffer=.TRUE.)
                            Most = ParseInteger(CurStr)
                        END IF
                    END IF
                ELSE
                    Most = Least
                END IF
                CALL Tree%PerformMany(Least, Most)
                Tree%ItemTerminated = TrueVal
            END BLOCK
        CASE ('(')
            CALL Tree%TryConcatenation()
            CALL List_AddNewNode(Tree%List, BranchNode_LBracket)
            Tree%ItemTerminated = FalseVal
        CASE (')')
            CALL List_AddNewNode(Tree%List, BranchNode_RBracket)
            Tree%ItemTerminated = TrueVal
        CASE ('*')
            CALL Tree%PerformMany(0, -1)
            Tree%ItemTerminated = TrueVal
        CASE ('?')
            CALL Tree%PerformMany(0, 1)
            Tree%ItemTerminated = TrueVal
        CASE ('+')
            CALL Tree%PerformMany(1, -1)
            Tree%ItemTerminated = TrueVal
        CASE ('|')
            CALL List_AddNewNode(Tree%List, BranchNode_Or)
            Tree%ItemTerminated = FalseVal
        CASE DEFAULT
            CALL Tree%TryConcatenation()
            IF ((Chr == '\').OR.(Chr == '.')) THEN
                BLOCK
                    tSInt32,  ALLOCATABLE   :: TokenSet(:)
                    tSInt32                 :: I
                    IF (Chr == '\') THEN
                        CALL InterpretToken(Tree%Regex(ID-1:ID), TokenSet)
                        ID = ID + 1
                    ELSE
                        CALL InterpretToken(Chr, TokenSet)
                    END IF
                    CALL List_AddNewNode(Tree%List, BranchNode_LBracket)
                    CALL List_AddNewCharNode(Tree%List, TokenSet(0))
                    DO I = 1, SIZE(TokenSet)-1
                        CALL List_AddNewNode(Tree%List, BranchNode_Or)
                        CALL List_AddNewCharNode(Tree%List, TokenSet(I))
                    END DO
                    CALL List_AddNewNode(Tree%List, BranchNode_RBracket)
                END BLOCK
            ELSE
                CALL List_AddNewCharNode(Tree%List, IACHAR(Chr))
            END IF
            Tree%ItemTerminated = TrueVal
        END SELECT CaseBlock
    END DO

    RETURN

END SUBROUTINE SyntaxTree_Normalize

!******************************************************************************

SUBROUTINE SyntaxTree_Shunt(Tree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform shunting yard algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxTree), INTENT(INOUT)    :: Tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(ShuntingStack)                 :: Stack
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tLogical                            :: EndOfList

! FLOW

    EndOfList = Tree%List%StartFirst(CurrNode)
    DO WHILE (.NOT.EndOfList)
        SELECT TYPE (CurrNode)
        TYPE IS (SyntaxNode)
            CALL Stack%Visit(CurrNode)
        END SELECT
        EndOfList = Tree%List%MoveForward(CurrNode)
    END DO
    CALL Stack%Finish(Tree%Stack)
    CALL Tree%List%Clear()

    RETURN

END SUBROUTINE SyntaxTree_Shunt

!******************************************************************************

SUBROUTINE SyntaxTree_Build(Tree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To build the syntax tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxTree), INTENT(INOUT)    :: Tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(OperatingStack)                :: Stack
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tLogical                            :: Success

! FLOW

    DO WHILE (.NOT.Tree%Stack%IsEmpty())
        Success = Tree%Stack%Pop(CurrNode)
        SELECT TYPE (CurrNode)
        TYPE IS (SyntaxNode)
            CALL Stack%Visit(CurrNode)
        END SELECT
    END DO
    Success = Stack%Pop(Tree%Root)

    RETURN

END SUBROUTINE SyntaxTree_Build

!******************************************************************************

SUBROUTINE SyntaxTree_Construct(Tree, Regex)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the syntax tree based on the specified regular expression pattern.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxTree), INTENT(OUT)  :: Tree
    tCharStar,         INTENT(IN)   :: Regex

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! NA

! FLOW

    Tree%Root  => NULL()
    Tree%Regex = Regex
    Tree%ItemTerminated = FalseVal
    CALL Tree%Normalize()
    CALL Tree%Shunt()
    CALL Tree%Build()

    RETURN

END SUBROUTINE SyntaxTree_Construct

!******************************************************************************

SUBROUTINE SyntaxTree_Destruct(Tree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct the syntax tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SyntaxTree), INTENT(INOUT)    :: Tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! NA

! FLOW

    DEALLOCATE(Tree%Regex)
    Tree%ItemTerminated = .FALSE.
    CALL Tree%List%Clear()
    CALL Tree%Stack%Clear()
    NULLIFY(Tree%Root)
    CALL SyntaxNode_Free()

    RETURN

END SUBROUTINE SyntaxTree_Destruct

!******************************************************************************

END MODULE MClass_SyntaxTree

!******************************************************************************
