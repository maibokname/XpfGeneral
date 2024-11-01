
MODULE MClass_AlphabetTrie

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *AlphabetTrie* type and its related routines.
!   The *AlphabetTrie* type is a derived type representing a symbol table
!   of key-value pairs, with string keys and generic values. <br>
!   The *AlphabetTrie* type supports common operations of the symbol table
!   including the *Insert*, *Remove*, *Contain*, *GetSize*, *IsEmpty* and
!   *GetValue* methods.  It also provides character-based methods for finding
!   the string in the symbol table that is the longest prefix of a given prefix,
!   finding all strings in the symbol table that start with a given prefix, and
!   finding all strings in the symbol table that match a given pattern. <br>
!   For all operations provided, the *AlphabetTrie* type supports two types of
!   character strings: the Fortran intrinsic *CHARACTER* type and the *FvlStr*
!   derived type.  A user can choose to work with one of these two types.  Like
!   other symbol tables, the *AlphabetTrie* type does not allow duplicated keys.
!   Therefore, if an inserted key is equal to a key stored in the table, an
!   associated value of the stored key is replaced by an associated value of
!   the inserted key.  <br>
!   Similar to the <a href="../module/mclass_multiwaytrie.html#type-multiwaytrie">
!   MultiwayTrie</a> type, the *AlphabetTrie* type technically employs a multi-way
!   (R-way) trie implementation.  However, unlike the *MultiwayTrie* type with its
!   fixed R (= 256), the *AlphabetTrie* type can be used with any R, which is the
!   number of characters (i.e. the radix) in an alphabet set of characters.  Also,
!   unlike the *MultiwayTrie* type, an explicit construction is required via either
!   the *CreateEmpty* or the *Construct* method where a user must specify a specific
!   set of characters via an *alphabet* data type.  <br>
!   See the <a href="../module/mclass_alphabets.html">Class_Alphabets</a> module for
!   various *alphabet* data types available to be used with the *AlphabetTrie* type.
!   Most of these data types commonly have a much smaller radix than 256; therefore,
!   the *AlphabetTrie* type is likely more efficient than the *MultiwayTrie* type
!   when working with those alphabet data types with small radix.  <br>
!   It is important to note that all provided *concrete alphabet* data types also
!   require an explicit construction via their *Construct* method.  Therefore, a
!   user must construct an *alphabet* data type before supplying it to one of the
!   *AlphabetTrie* type's construction methods.  It is also worth mentioning that
!   the given alphabet data type must contain all characters (with the exception of
!   the wild-card characters) of all strings that the user will specify as input
!   arguments.  This means that the user must know in advance what kind of characters
!   to be used so that he/she can properly select a particular *alphabet* data type. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] R. Sedgewick and K. Wayne. 2011.  Algorithms, 4th Edition.  Addison-Wesley. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_DoublyLinkedLists,  ONLY: QueueString => ListCharacter, QueueAny => ListAnyType
    USE MClass_FvlStr
    USE MClass_StringBuilder
    USE MClass_Alphabets,            ONLY: Alphabet => BaseAlphabet

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: AlphabetTrie

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'Class_AlphabetTrie'
    tSInt32,   PARAMETER    :: MsgLen  = 128
    tCharStar, PARAMETER    :: NULCHR  = ACHAR(0)   !! flag for null node
    tCharStar, PARAMETER    :: NONNUL  = ACHAR(2)   !! flag for non-null node

!** DERIVED TYPE DEFINITIONS
    !> The *TrieNode* type is a node type used in conjunction with the
    !  *AlphabetTrie* type.  It is a private type.
    TYPE TrieNode
        !% character flag indicating whether the node is null or not
        tChar                       :: Chr = NULCHR
        !% value (allocated if this is the node corresponding to the last key character)
        CLASS(*),       ALLOCATABLE :: Value
        !% child nodes of this node
        TYPE(TrieNode), ALLOCATABLE :: Next(:)
    END TYPE TrieNode
    !> The *AlphabetTrie* type is a container type that utilizes a multi-way trie
    !  implementation to provide common operations for a symbol table where its
    !  keys are character strings and its values can be of any type.
    TYPE AlphabetTrie
        PRIVATE
        CLASS(Alphabet), POINTER    :: Alpha => NULL()  !! alphabet object
        TYPE(TrieNode)              :: Root             !! root of trie
        tIndex                      :: N                !! number of keys
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !% *Put* is a working method for insertion operation
        PROCEDURE, PRIVATE  :: Put              => AlphabetTrie_Put
        !% *Del* is a working method for removal operation
        PROCEDURE, PRIVATE  :: Del              => AlphabetTrie_Delete
        !% *Get* is a working method for key inquiring operation
        PROCEDURE, PRIVATE  :: Get              => AlphabetTrie_GetNode
        !% *LongestPrefixLen* is a working method for getting the longest prefix
        PROCEDURE, PRIVATE  :: LongestPrefixLen => AlphabetTrie_LengthOfLongestPrefix
        !% *Collect* is a working method for key collecting operation
        PROCEDURE, PRIVATE  :: AlphabetTrie_CollectKeys
        PROCEDURE, PRIVATE  :: AlphabetTrie_AssembleKeys
        PROCEDURE, PRIVATE  :: AlphabetTrie_GatherWildcardKeys
        GENERIC,   PRIVATE  :: Collect          => AlphabetTrie_CollectKeys, &
                                                   AlphabetTrie_AssembleKeys, &
                                                   AlphabetTrie_GatherWildcardKeys
        !% procedures with generic interfaces
        PROCEDURE, PRIVATE  :: AlphabetTrie_Insert_ChrStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_Insert_FvlStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_Remove_ChrStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_Remove_FvlStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_Contain_ChrStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_Contain_FvlStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_StartWith_ChrStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_StartWith_FvlStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_GetValue_ChrStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_GetValue_FvlStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_AllKeys_ChrStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_AllKeys_FvlStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_KeysWithPrefix_CHCH
        PROCEDURE, PRIVATE  :: AlphabetTrie_KeysWithPrefix_CHVL
        PROCEDURE, PRIVATE  :: AlphabetTrie_KeysWithPrefix_VLCH
        PROCEDURE, PRIVATE  :: AlphabetTrie_KeysWithPrefix_VLVL
        PROCEDURE, PRIVATE  :: AlphabetTrie_KeysThatMatch_CHCH
        PROCEDURE, PRIVATE  :: AlphabetTrie_KeysThatMatch_CHVL
        PROCEDURE, PRIVATE  :: AlphabetTrie_KeysThatMatch_VLCH
        PROCEDURE, PRIVATE  :: AlphabetTrie_KeysThatMatch_VLVL
        PROCEDURE, PRIVATE  :: AlphabetTrie_WildcardKeys_CHCH
        PROCEDURE, PRIVATE  :: AlphabetTrie_WildcardKeys_CHVL
        PROCEDURE, PRIVATE  :: AlphabetTrie_WildcardKeys_VLCH
        PROCEDURE, PRIVATE  :: AlphabetTrie_WildcardKeys_VLVL
        PROCEDURE, PRIVATE  :: AlphabetTrie_LongestPrefixOf_ChrStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_LongestPrefixOf_FvlStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_ConstructByArray_ChrStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_ConstructByArray_FvlStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_Destructor_ChrStr
        PROCEDURE, PRIVATE  :: AlphabetTrie_Destructor_FvlStr
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        ! -----     constructor and destructor procedures   -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        ! **Purpose**:  To construct an empty symbol table.  <br>
        !  **Usage**: <br>
        !   --->    CALL Table%CreateEmpty(Alphabet)
        PROCEDURE   :: CreateEmpty          => AlphabetTrie_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        ! **Purpose**:  To construct a symbol table from arrays of keys and values.  <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Construct(Alphabet, 10, KeyArr, ValArr)
        GENERIC     :: Construct            => AlphabetTrie_ConstructByArray_ChrStr, &
                                               AlphabetTrie_ConstructByArray_FvlStr
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all key-value pairs from the symbol table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Clear() <br>
        PROCEDURE   :: Clear                => AlphabetTrie_Clear
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To first retrieve stored keys (and optionally their associated values)
        !                and then remove all key-value pairs from the symbol table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Destruct(KeyQ) <br>
        !   --->    CALL Table%Destruct(KeyArr) <br>
        !   --->    CALL Table%Destruct(KeyQ, ValQ) <br>
        !   --->    CALL Table%Destruct(KeyArr, ValQ) <br>
        !  **Important Note**: A user must choose which type of keys to be returned.  The method
        !       returns a queue of keys where their type is the Fortran intrinsic *CHARACTER* type
        !       or it returns an array of keys where their type is the *FvlStr* type.
        GENERIC     :: Destruct             => AlphabetTrie_Destructor_ChrStr, &
                                               AlphabetTrie_Destructor_FvlStr
        ! -------------------------------------------------------
        ! -----         adding and removing procedures      -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key-value pair to the table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Insert(Key, Value) <br>
        GENERIC     :: Insert               => AlphabetTrie_Insert_ChrStr, &
                                               AlphabetTrie_Insert_FvlStr
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified key (and its associated value) from
        !                the table (if the key found).  Optionally, the associated
        !                value can be retrieved. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Remove(Key) <br>
        !   --->    CALL Table%Remove(Key, Value) <br>
        GENERIC     :: Remove               => AlphabetTrie_Remove_ChrStr, &
                                               AlphabetTrie_Remove_FvlStr
        ! -------------------------------------------------------
        ! -----               inquiry procedures            -----
        ! -------------------------------------------------------
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified key in the table.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%Contain(Key) <br>
        !   --->    IF (.NOT.Table%Contain(Key)) DoSomething
        GENERIC     :: Contain              => AlphabetTrie_Contain_ChrStr, &
                                               AlphabetTrie_Contain_FvlStr
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size (the number of key-value pairs stored)
        !                of the table. <br>
        !  **Usage**: <br>
        !   --->    Size = Table%GetSize()
        PROCEDURE   :: GetSize              => AlphabetTrie_GetSize
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the table is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%IsEmpty() <br>
        !   --->    IF (.NOT.Table%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty              => AlphabetTrie_IsEmpty
        !> **Type-Bound Function**: StartWith <br>
        !  **Purpose**:  To return a flag indicating whether the symbol table contains
        !                a key starting with the specified prefix. <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%StartWith(Prefix) <br>
        !   --->    IF (.NOT.Table%StartWith(Prefix)) DoSomeThing
        GENERIC     :: StartWith            => AlphabetTrie_StartWith_ChrStr, &
                                               AlphabetTrie_StartWith_FvlStr
        ! -------------------------------------------------------
        ! -----             retrieval procedures            -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: GetValue <br>
        !  **Purpose**:  To get a value associated with the specified key in the table.
        !                If the key is not found, return an unallocated value. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%GetValue(Key, Value) <br>
        GENERIC     :: GetValue             => AlphabetTrie_GetValue_ChrStr, &
                                               AlphabetTrie_GetValue_FvlStr
        !> **Type-Bound Subroutine**: GetAllKeys <br>
        !  **Purpose**:  To return a queue (or an array) of all the keys in the symbol table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%GetAllKeys(KeyQ) <br>
        !   --->    CALL Table%GetAllKeys(KeyArr) <br>
        !  **Important Note**: A user must choose which type of keys to be returned.  The method
        !       returns a queue of keys where their type is the Fortran intrinsic *CHARACTER* type
        !       or it returns an array of keys where their type is the *FvlStr* type.
        GENERIC     :: GetAllKeys           => AlphabetTrie_AllKeys_ChrStr, &
                                               AlphabetTrie_AllKeys_FvlStr
        !> **Type-Bound Subroutine**: GetKeysWithPrefix <br>
        !  **Purpose**:  To return a queue (or an array) of all the keys in the symbol table that
        !                start with the specified prefix.  Return an empty queue (or an unallocated
        !                array) if no such key(s) found. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%GetKeysWithPrefix(Prefix, KeyQ) <br>
        !   --->    CALL Table%GetKeysWithPrefix(Prefix, KeyArr) <br>
        !  **Important Note**: A user must choose which type of keys to be returned.  The method
        !       returns a queue of keys where their type is the Fortran intrinsic *CHARACTER* type
        !       or it returns an array of keys where their type is the *FvlStr* type.
        GENERIC     :: GetKeysWithPrefix    => AlphabetTrie_KeysWithPrefix_CHCH, &
                                               AlphabetTrie_KeysWithPrefix_CHVL, &
                                               AlphabetTrie_KeysWithPrefix_VLCH, &
                                               AlphabetTrie_KeysWithPrefix_VLVL
        !> **Type-Bound Subroutine**: GetKeysThatMatch <br>
        !  **Purpose**:  To return a queue (or an array) of all the keys in the symbol table that
        !                match the given pattern where the question-mark character is interpreted
        !                as a wild-card character.  Return an empty queue (or an unallocated array)
        !                if no such key(s) found. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%GetKeysThatMatch(Pattern, KeyQ) <br>
        !   --->    CALL Table%GetKeysThatMatch(Pattern, KeyArr) <br>
        !  **Important Note**: A user must choose which type of keys to be returned.  The method
        !       returns a queue of keys where their type is the Fortran intrinsic *CHARACTER* type
        !       or it returns an array of keys where their type is the *FvlStr* type.
        GENERIC     :: GetKeysThatMatch     => AlphabetTrie_KeysThatMatch_CHCH, &
                                               AlphabetTrie_KeysThatMatch_CHVL, &
                                               AlphabetTrie_KeysThatMatch_VLCH, &
                                               AlphabetTrie_KeysThatMatch_VLVL
        !> **Type-Bound Subroutine**: GetWildcardKeys <br>
        !  **Purpose**:  To return a queue (or an array) of all the keys in the symbol table that
        !                match the given pattern with wild-card characters.  Return an empty queue
        !                (or an unallocated array) if no such key(s) found. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%GetWildcardKeys(Pattern, KeyQ) <br>
        !   --->    CALL Table%GetWildcardKeys(Pattern, KeyArr) <br>
        !  **Important Note**: The *GetWildcardKeys* method is mostly the same as the *GetKeysThatMatch*
        !       method, except that it recognizes two wild-card characters instead of one.  Similar to
        !       the *GetKeysThatMatch* method, the question-mark character ('?') is interpreted as a
        !       wild-card character for a single character.  Additionally, the *GetWildcardKeys* method
        !       interprets the asterisk character ('*') as a wild-card character for a sequence of
        !       characters.
        GENERIC     :: GetWildcardKeys      => AlphabetTrie_WildcardKeys_CHCH, &
                                               AlphabetTrie_WildcardKeys_CHVL, &
                                               AlphabetTrie_WildcardKeys_VLCH, &
                                               AlphabetTrie_WildcardKeys_VLVL
        !> **Type-Bound Subroutine**: GetLongestPrefixOf <br>
        !  **Purpose**:  To return the string in the symbol table that is the longest prefix of the
        !                specified query.  Return an unallocated string if no such string found. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%GetLongestPrefixOf(Query, Prefix) <br>
        GENERIC     :: GetLongestPrefixOf   => AlphabetTrie_LongestPrefixOf_ChrStr, &
                                               AlphabetTrie_LongestPrefixOf_FvlStr
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the symbol table.
        FINAL       :: AlphabetTrie_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE AlphabetTrie

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE TrieNode_New(Node, Radix)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make the specify node a new one by setting the character-flag component
    !  to non-null character and also allocate its next-node component.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(TrieNode), INTENT(INOUT)   :: Node     !! trie node
    tIndex,         INTENT(IN)      :: Radix    !! radix

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW
    
    ! set character-flag component to non-null character
    Node%Chr = NONNUL
        
    ! allocate next-node component
    ALLOCATE(Node%Next(0:Radix-1), STAT=AllocStat, ERRMSG=AllocMsg)
    
    ! check allocation status and report error if necessary
    CALL Handle_ErrAlloc('TrieNode_New', ModName, AllocMsg, AllocStat)
    
    RETURN

END SUBROUTINE TrieNode_New

!******************************************************************************

SUBROUTINE TrieNode_Clear(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clear the specify node by setting the character-flag component to null
    !  character and also deallocate its next-node component.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(TrieNode), INTENT(INOUT)   :: Node !! trie node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW
    
    ! set character-flag component to null character
    Node%Chr = NULCHR
    
    ! deallocate next-node component
    DEALLOCATE(Node%Next, STAT=AllocStat, ERRMSG=AllocMsg)

    IF (AllocStat /= 0) THEN        
        ! deallocate next-node component
        IF (ALLOCATED(Node%Value)) THEN
            DEALLOCATE(Node%Value, STAT=AllocStat, ERRMSG=AllocMsg)
            ! check allocation status and report error if necessary
            CALL Handle_ErrAlloc('TrieNode_Clear', ModName, AllocMsg, AllocStat)
        END IF
    ELSE
        ! check allocation status and report error if necessary
        CALL Handle_ErrAlloc('TrieNode_Clear', ModName, AllocMsg, AllocStat)
    END IF
    
    RETURN

END SUBROUTINE TrieNode_Clear

!******************************************************************************

SUBROUTINE AlphabetTrie_CreateEmpty(Table, Alpha)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct an empty symbol table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),     INTENT(INOUT)  :: Table        !! symbol table
    CLASS(Alphabet), TARGET, INTENT(IN)     :: Alpha        !! Alphabet object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Alpha%IsReady()) THEN
        ! set pointer to the Alphabet object
        Table%Alpha => Alpha
    ELSE
        CALL Handle_ErrLevel('AlphabetTrie_CreateEmpty', ModName, ErrSevere, &
                             'Cannot create the trie because the alphabet has not yet been created .')
    END IF

    RETURN

END SUBROUTINE AlphabetTrie_CreateEmpty

!******************************************************************************

SUBROUTINE AlphabetTrie_ConstructByArray_ChrStr(Table, Alpha, N, Keys, Values)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a symbol table based on specified arrays.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),     INTENT(INOUT)  :: Table        !! symbol table
    CLASS(Alphabet), TARGET, INTENT(IN)     :: Alpha        !! Alphabet object
    tIndex,                  INTENT(IN)     :: N            !! number of keys
    tCharStar,               INTENT(IN)     :: Keys(N)      !! an array of keys
    CLASS(*),                INTENT(IN)     :: Values(N)    !! an array of associated values

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    IF (Alpha%IsReady()) THEN
        ! set pointer to the Alphabet object
        Table%Alpha => Alpha
    ELSE
        CALL Handle_ErrLevel('AlphabetTrie_ConstructByArray_ChrStr', ModName, ErrSevere, &
                             'Cannot create the trie because the alphabet has not yet been created.')
        RETURN
    END IF

    ! built symbol table from input arrays
    DO I = 1, N
        CALL Table%Insert(Keys(I), Values(I))
    END DO

    RETURN

END SUBROUTINE AlphabetTrie_ConstructByArray_ChrStr

!******************************************************************************

SUBROUTINE AlphabetTrie_ConstructByArray_FvlStr(Table, Alpha, N, Keys, Values)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a symbol table based on specified arrays.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),     INTENT(INOUT)  :: Table        !! symbol table
    CLASS(Alphabet), TARGET, INTENT(IN)     :: Alpha        !! Alphabet object
    tIndex,                  INTENT(IN)     :: N            !! number of keys
    TYPE(FvlStr),            INTENT(IN)     :: Keys(N)      !! an array of keys
    CLASS(*),                INTENT(IN)     :: Values(N)    !! an array of associated values

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    IF (Alpha%IsReady()) THEN
        ! set pointer to the Alphabet object
        Table%Alpha => Alpha
    ELSE
        CALL Handle_ErrLevel('AlphabetTrie_ConstructByArray_FvlStr', ModName, ErrSevere, &
                             'Cannot create the trie because the alphabet has not yet been created.')
        RETURN
    END IF

    ! built symbol table from input arrays
    DO I = 1, N
        CALL Table%Insert(Keys(I), Values(I))
    END DO

    RETURN

END SUBROUTINE AlphabetTrie_ConstructByArray_FvlStr

!******************************************************************************

SUBROUTINE AlphabetTrie_Clear(Table)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct a symbol table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(QueueString)   :: KeyQ
    tCharAlloc          :: Key
    tLogical            :: Success

! FLOW

    ! get all keys
    CALL Table%GetAllKeys(KeyQ)
    
    ! remove the key-value pairs
    Success = KeyQ%Dequeue(Key)
    DO WHILE (.NOT.Success)
        CALL Table%Remove(Key)
        IF (Table%IsEmpty()) EXIT
        Success = KeyQ%Dequeue(Key)
    END DO

    RETURN

END SUBROUTINE AlphabetTrie_Clear

!******************************************************************************

SUBROUTINE AlphabetTrie_Destructor_ChrStr(Table, KeyQ, ValueQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct a symbol table and get its pair data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),      INTENT(INOUT) :: Table    !! symbol table
    TYPE(QueueString),        INTENT(OUT)   :: KeyQ     !! a queue of stored keys
    TYPE(QueueAny), OPTIONAL, INTENT(OUT)   :: ValueQ   !! a queue of stored values

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: Key
    tLogical    :: IsTheEnd

! FLOW

    ! get all keys
    CALL Table%GetAllKeys(KeyQ)
    IF (PRESENT(ValueQ)) THEN
        ! get all values while removing the key-value pairs
        BLOCK
            CLASS(*), ALLOCATABLE   :: Value
            IsTheEnd = KeyQ%StartFirst(Key)
            DO WHILE (.NOT.IsTheEnd)
                CALL Table%Remove(Key, Value)
                CALL ValueQ%Enqueue(Value)
                DEALLOCATE(Value)
                IF (Table%IsEmpty()) EXIT
                IsTheEnd = KeyQ%MoveForward(Key)
            END DO
        END BLOCK
    ELSE
        ! remove the key-value pairs
        IsTheEnd = KeyQ%StartFirst(Key)
        DO WHILE (.NOT.IsTheEnd)
            CALL Table%Remove(Key)
            IF (Table%IsEmpty()) EXIT
            IsTheEnd = KeyQ%MoveForward(Key)
        END DO
    END IF
    
    ! set pointer to null
    Table%Alpha => NULL()
        
    RETURN

END SUBROUTINE AlphabetTrie_Destructor_ChrStr

!******************************************************************************

SUBROUTINE AlphabetTrie_Destructor_FvlStr(Table, KeyArr, ValueQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct a symbol table and get its pair data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),       INTENT(INOUT)    :: Table        !! symbol table
    TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)      :: KeyArr(:)    !! an array of stored keys
    TYPE(QueueAny), OPTIONAL,  INTENT(OUT)      :: ValueQ       !! a queue of stored values

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(QueueString)   :: KeyQ

! FLOW

    CALL Table%Destruct(KeyQ, ValueQ)
    CALL KeyQueue2Array(KeyQ, KeyArr)
        
    RETURN

END SUBROUTINE AlphabetTrie_Destructor_FvlStr

!******************************************************************************

SUBROUTINE AlphabetTrie_Finalizer(Table)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(AlphabetTrie), INTENT(INOUT)   :: Table !! symbol table

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    CALL Table%Clear()
    Table%Alpha => NULL()

    RETURN

END SUBROUTINE AlphabetTrie_Finalizer

!******************************************************************************

RECURSIVE SUBROUTINE AlphabetTrie_Put(Table, X, Key, Value, D)

!** PURPOSE OF THIS SUBROUTINE:
    !^ This routine is a working routine for insertion operation.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    TYPE(TrieNode),      INTENT(INOUT)  :: X        !! node representing current subtrie
    tCharStar,           INTENT(IN)     :: Key      !! key
    CLASS(*),            INTENT(IN)     :: Value    !! value
    tIndex,              INTENT(IN)     :: D        !! index of the character in the key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: C

! FLOW

    ! create a subtrie to contain the key and its associated value
    IF (X%Chr == NULCHR) CALL TrieNode_New(X, Table%Alpha%GetRadix())
    
    IF (D > LEN(Key, KIND=kIndex)) THEN
        ! X is the node corresponding to the last key character
        IF (.NOT.ALLOCATED(X%Value)) THEN
            ! new key
            Table%N = Table%N + 1_kIndex
            ALLOCATE(X%Value, SOURCE=Value)
        ELSE
            ! existing key so replace existing value with new one
            ! note: must re-allocate since value can have different type
            !       so assignment/copy may be invalid.
            DEALLOCATE(X%Value)
            ALLOCATE(X%Value, SOURCE=Value)
        END IF
        RETURN
    END IF
    
    ! add next node to the subtrie
    C = Table%Alpha%GetIndex(Key(D:D))
    IF (C == -1_kIndex) THEN
        CALL Handle_ErrLevel('AlphabetTrie_Put', ModName, ErrSevere, &
                             'The "' // Key(D:D) // '" character is NOT in the alphabet.')
        RETURN
    END IF
    CALL Table%Put(X%Next(C), Key, Value, D+1_kIndex)

    RETURN

END SUBROUTINE AlphabetTrie_Put

!******************************************************************************

SUBROUTINE AlphabetTrie_Insert_ChrStr(Table, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert a key-value pair into the symbol table, overwriting the old value
    !  with the new value if the key is already in the symbol table.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    tCharStar,           INTENT(IN)     :: Key      !! key
    CLASS(*),            INTENT(IN)     :: Value    !! value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_Insert_ChrStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (LEN(Key) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_Insert_ChrStr', ModName, ErrSevere, &
                             'A key string with length of zero is NOT allowed.')
        RETURN
!    ELSEIF (Table%Alpha%Verify(Key) /= 0_kIndex) THEN
!        CALL Handle_ErrLevel('AlphabetTrie_Insert_ChrStr', ModName, ErrSevere, &
!                             'Invalid key.  Not all key characters are in the alphabet.')
!        RETURN
    END IF

    CALL Table%Put(Table%Root, Key, Value, 1_kIndex)

    RETURN

END SUBROUTINE AlphabetTrie_Insert_ChrStr

!******************************************************************************

SUBROUTINE AlphabetTrie_Insert_FvlStr(Table, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert a key-value pair into the symbol table, overwriting the old value
    !  with the new value if the key is already in the symbol table.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    TYPE(FvlStr),        INTENT(IN)     :: Key      !! key
    CLASS(*),            INTENT(IN)     :: Value    !! value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: StrPtr

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_Insert_FvlStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (GETLEN(Key) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_Insert_FvlStr', ModName, ErrSevere, &
                             'A key string with length of zero is NOT allowed.')
        RETURN
!    ELSEIF (Table%Alpha%Verify(Key) /= 0_kIndex) THEN
!        CALL Handle_ErrLevel('AlphabetTrie_Insert_FvlStr', ModName, ErrSevere, &
!                             'Invalid key.  Not all key characters are in the alphabet.')
!        RETURN
    END IF

    StrPtr => PtrToStr(Key)
    IF (ASSOCIATED(StrPtr)) CALL Table%Put(Table%Root, StrPtr, Value, 1_kIndex)
    NULLIFY(StrPtr)

    RETURN

END SUBROUTINE AlphabetTrie_Insert_FvlStr

!******************************************************************************

RECURSIVE FUNCTION AlphabetTrie_GetNode(Table, NodeIn, Key, D) RESULT(NodeOut)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a pointer to the last node of the subtrie that stores the
    !  specified key.  If the specified key is not found, return a null pointer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),    INTENT(INOUT)   :: Table    !! symbol table
    TYPE(TrieNode), TARGET, INTENT(IN)      :: NodeIn   !! input node
    tCharStar,              INTENT(IN)      :: Key      !! key
    tIndex,                 INTENT(IN)      :: D        !! index of the character in the key
    TYPE(TrieNode), POINTER                 :: NodeOut  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: C

! FLOW
        
    IF (NodeIn%Chr == NULCHR) THEN
        ! key is not found
        NodeOut => NULL()
    ELSEIF (D > LEN(Key, KIND=kIndex)) THEN
        ! NodeIn is the node corresponding to the last key character (i.e. key found)
        NodeOut => NodeIn
    ELSE
        ! search the next node
        C = Table%Alpha%GetIndex(Key(D:D))
        IF (C == -1_kIndex) THEN
            CALL Handle_ErrLevel('AlphabetTrie_GetNode', ModName, ErrSevere, &
                                 'The "' // Key(D:D) // '" character is NOT in the alphabet.')
            RETURN
        END IF
        NodeOut => Table%Get(NodeIn%Next(C), Key, D+1_kIndex)
    END IF

    RETURN

END FUNCTION AlphabetTrie_GetNode

!******************************************************************************

FUNCTION AlphabetTrie_Contain_ChrStr(Table, Key) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the symbol table contains the specified key or not.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    tCharStar,           INTENT(IN)     :: Key      !! key
    tLogical                            :: Found    !! true if key found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_Contain_ChrStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (LEN(Key) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_Contain_ChrStr', ModName, ErrSevere, &
                             'A key string with length of zero is NOT allowed.')
        Found = FalseVal
        RETURN
    END IF

    Found = ASSOCIATED(Table%Get(Table%Root, Key, 1_kIndex))

    RETURN

END FUNCTION AlphabetTrie_Contain_ChrStr

!******************************************************************************

FUNCTION AlphabetTrie_Contain_FvlStr(Table, Key) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the symbol table contains the specified key or not.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    TYPE(FvlStr),        INTENT(IN)     :: Key      !! key
    tLogical                            :: Found    !! true if key found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: StrPtr

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_Contain_FvlStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (GETLEN(Key) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_Contain_FvlStr', ModName, ErrSevere, &
                             'A key string with length of zero is NOT allowed.')
        Found = FalseVal
        RETURN
    END IF

    StrPtr => PtrToStr(Key)
    IF (ASSOCIATED(StrPtr)) THEN
        Found = ASSOCIATED(Table%Get(Table%Root, StrPtr, 1_kIndex))
    ELSE
        Found = FalseVal
    END IF
    NULLIFY(StrPtr)

    RETURN

END FUNCTION AlphabetTrie_Contain_FvlStr

!******************************************************************************

SUBROUTINE AlphabetTrie_GetValue_ChrStr(Table, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the value associated with the specified key.  Return
    !  an unallocated value if the key is not found in the symbol table.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),   INTENT(INOUT)    :: Table    !! symbol table
    tCharStar,             INTENT(IN)       :: Key      !! key
    CLASS(*), ALLOCATABLE, INTENT(OUT)      :: Value    !! value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TrieNode), POINTER :: NodeOut

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_GetValue_ChrStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (LEN(Key) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_GetValue_ChrStr', ModName, ErrSevere, &
                             'A key string with length of zero is NOT allowed.')
        RETURN
    END IF

    NodeOut => Table%Get(Table%Root, Key, 1_kIndex)
    IF (ASSOCIATED(NodeOut)) THEN
        ! key found
        IF (ALLOCATED(NodeOut%Value)) ALLOCATE(Value, SOURCE=NodeOut%Value)
    END IF
    NULLIFY(NodeOut)

    RETURN

END SUBROUTINE AlphabetTrie_GetValue_ChrStr

!******************************************************************************

SUBROUTINE AlphabetTrie_GetValue_FvlStr(Table, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the value associated with the specified key.  Return
    !  an unallocated value if the key is not found in the symbol table.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),   INTENT(INOUT)    :: Table    !! symbol table
    TYPE(FvlStr),          INTENT(IN)       :: Key      !! key
    CLASS(*), ALLOCATABLE, INTENT(OUT)      :: Value    !! value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: StrPtr

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_GetValue_FvlStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (GETLEN(Key) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_GetValue_FvlStr', ModName, ErrSevere, &
                             'A key string with length of zero is NOT allowed.')
        RETURN
    END IF

    StrPtr => PtrToStr(Key)
    IF (ASSOCIATED(StrPtr)) CALL Table%GetValue(StrPtr, Value)
    NULLIFY(StrPtr)

    RETURN

END SUBROUTINE AlphabetTrie_GetValue_FvlStr

!******************************************************************************

RECURSIVE SUBROUTINE AlphabetTrie_Delete(Table, X, Key, D, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ This routine is a working routine for removal operation.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),             INTENT(INOUT)  :: Table    !! symbol table
    TYPE(TrieNode),                  INTENT(INOUT)  :: X        !! node representing current subtrie
    tCharStar,                       INTENT(IN)     :: Key      !! key
    tIndex,                          INTENT(IN)     :: D        !! index of the character in the key
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: C

! FLOW

    ! return if this subtrie is null
    IF (X%Chr == NULCHR) RETURN

    IF (D > LEN(Key, KIND=kIndex)) THEN
        ! X is the node corresponding to the last key character
        IF (ALLOCATED(X%Value)) THEN
            ! the key-value pair exists so remove it
            Table%N = Table%N - 1_kIndex
            IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=X%Value)
            DEALLOCATE(X%Value)
        END IF
    ELSE
        ! search the next node of the subtrie
        C = Table%Alpha%GetIndex(Key(D:D))
        IF (C == -1_kIndex) THEN
            CALL Handle_ErrLevel('AlphabetTrie_Delete', ModName, ErrSevere, &
                                 'The "' // Key(D:D) // '" character is NOT in the alphabet.')
            RETURN
        END IF
        CALL Table%Del(X%Next(C), Key, D+1_kIndex)
    END IF

    ! remove subtrie rooted at X if it is completely empty
    IF (ALLOCATED(X%Value)) RETURN
    DO C = 0, Table%Alpha%GetRadix()-1
        IF (X%Next(C)%Chr /= NULCHR) RETURN
    END DO
    CALL TrieNode_Clear(X)

    RETURN

END SUBROUTINE AlphabetTrie_Delete

!******************************************************************************

SUBROUTINE AlphabetTrie_Remove_ChrStr(Table, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove a key-value pair from the symbol table if the specified key is found.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),             INTENT(INOUT)  :: Table    !! symbol table
    tCharStar,                       INTENT(IN)     :: Key      !! key
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_Remove_ChrStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (LEN(Key) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_Remove_ChrStr', ModName, ErrSevere, &
                             'A key string with length of zero is NOT allowed.')
        RETURN
    END IF

    CALL Table%Del(Table%Root, Key, 1_kIndex, Value)

    RETURN

END SUBROUTINE AlphabetTrie_Remove_ChrStr

!******************************************************************************

SUBROUTINE AlphabetTrie_Remove_FvlStr(Table, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove a key-value pair from the symbol table if the specified key is found.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),             INTENT(INOUT)  :: Table    !! symbol table
    TYPE(FvlStr),                    INTENT(IN)     :: Key      !! key
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: StrPtr

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_Remove_FvlStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (GETLEN(Key) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_Remove_FvlStr', ModName, ErrSevere, &
                             'A key string with length of zero is NOT allowed.')
        RETURN
    END IF

    StrPtr => PtrToStr(Key)
    IF (ASSOCIATED(StrPtr)) CALL Table%Del(Table%Root, StrPtr, 1_kIndex, Value)
    NULLIFY(StrPtr)

    RETURN

END SUBROUTINE AlphabetTrie_Remove_FvlStr

!******************************************************************************

RECURSIVE SUBROUTINE AlphabetTrie_CollectKeys(Table, X, Prefix, KeyQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To collect all of keys in the symbol table that start with the specified prefix.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    TYPE(TrieNode),      INTENT(IN)     :: X        !! node representing current subtrie
    TYPE(StringBuilder), INTENT(INOUT)  :: Prefix   !! string builder with the specified prefix
    TYPE(QueueString),   INTENT(INOUT)  :: KeyQ     !! queue of strings with the specified prefix

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: C

! FLOW
        
    IF (X%Chr == NULCHR) RETURN
    
    IF (ALLOCATED(X%Value)) CALL KeyQ%EnQueue(Prefix%ToCharAlloc())

    DO C = 0, Table%Alpha%GetRadix()-1
        CALL Prefix%Append(Table%Alpha%GetChar(C))
        CALL Table%Collect(X%Next(C), Prefix, KeyQ)
        CALL Prefix%DelLastChar()
    END DO

    RETURN

END SUBROUTINE AlphabetTrie_CollectKeys

!******************************************************************************

FUNCTION AlphabetTrie_StartWith_ChrStr(Table, Prefix) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a flag indicating whether the symbol table contains a key
    !  starting with the specified prefix.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    tCharStar,           INTENT(IN)     :: Prefix   !! prefix string
    tLogical                            :: Found    !! true if a key starting with the prefix found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_StartWith_ChrStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        Found = FalseVal
        RETURN
    ELSEIF (LEN(Prefix) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_StartWith_ChrStr', ModName, ErrSevere, &
                             'A prefix string with length of zero is NOT allowed.')
        Found = FalseVal
        RETURN
    END IF

    Found = ASSOCIATED(Table%Get(Table%Root, Prefix, 1_kIndex))

    RETURN

END FUNCTION AlphabetTrie_StartWith_ChrStr

!******************************************************************************

FUNCTION AlphabetTrie_StartWith_FvlStr(Table, Prefix) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a flag indicating whether the symbol table contains a key
    !  starting with the specified prefix.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    TYPE(FvlStr),        INTENT(IN)     :: Prefix   !! prefix string
    tLogical                            :: Found    !! true if a key starting with the prefix found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: StrPtr

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_StartWith_FvlStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        Found = FalseVal
        RETURN
    ELSEIF (GETLEN(Prefix) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_StartWith_FvlStr', ModName, ErrSevere, &
                             'A prefix string with length of zero is NOT allowed.')
        Found = FalseVal
        RETURN
    END IF

    StrPtr => PtrToStr(Prefix)
    IF (ASSOCIATED(StrPtr)) THEN
        Found = ASSOCIATED(Table%Get(Table%Root, StrPtr, 1_kIndex))
    ELSE
        Found = FalseVal
    END IF
    NULLIFY(StrPtr)

    RETURN

END FUNCTION AlphabetTrie_StartWith_FvlStr

!******************************************************************************

SUBROUTINE AlphabetTrie_KeysWithPrefix_CHCH(Table, Prefix, KeyQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a queue of all the keys in the symbol table that start with the
    !  specified prefix.  Return an empty queue if no such key(s) found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    tCharStar,           INTENT(IN)     :: Prefix   !! prefix string
    TYPE(QueueString),   INTENT(OUT)    :: KeyQ     !! queue of keys starting with the specified prefix

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TrieNode), POINTER :: X
    TYPE(StringBuilder)     :: StrBld

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_KeysWithPrefix_CHCH', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (LEN(Prefix) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_KeysWithPrefix_CHCH', ModName, ErrSevere, &
                             'A prefix string with length of zero is NOT allowed.')
        RETURN
    END IF

    X => Table%Get(Table%Root, Prefix, 1_kIndex)
    IF (ASSOCIATED(X)) THEN
        ! key(s) with prefix found
        CALL StrBld%Construct(Prefix)
        CALL Table%Collect(X, StrBld, KeyQ)
        NULLIFY(X)
    END IF

    RETURN

END SUBROUTINE AlphabetTrie_KeysWithPrefix_CHCH

!******************************************************************************

SUBROUTINE AlphabetTrie_KeysWithPrefix_CHVL(Table, Prefix, Keys)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an array of all the keys in the symbol table that start with the
    !  specified prefix.  Return an unallocated array if no such key(s) found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),       INTENT(INOUT)    :: Table    !! symbol table
    tCharStar,                 INTENT(IN)       :: Prefix   !! prefix string
    TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)      :: Keys(:)  !! array of all keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(QueueString)   :: KeyQ

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_KeysWithPrefix_CHVL', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (LEN(Prefix) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_KeysWithPrefix_CHVL', ModName, ErrSevere, &
                             'A prefix string with length of zero is NOT allowed.')
        RETURN
    END IF

    CALL Table%GetKeysWithPrefix(Prefix, KeyQ)
    CALL KeyQueue2Array(KeyQ, Keys)

    RETURN

END SUBROUTINE AlphabetTrie_KeysWithPrefix_CHVL

!******************************************************************************

SUBROUTINE AlphabetTrie_KeysWithPrefix_VLCH(Table, Prefix, KeyQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a queue of all the keys in the symbol table that start with the
    !  specified prefix.  Return an empty queue if no such key(s) found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    TYPE(FvlStr),        INTENT(IN)     :: Prefix   !! prefix string
    TYPE(QueueString),   INTENT(OUT)    :: KeyQ     !! queue of keys starting with the specified prefix

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: StrPtr

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_KeysWithPrefix_VLCH', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (GETLEN(Prefix) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_KeysWithPrefix_VLCH', ModName, ErrSevere, &
                             'A prefix string with length of zero is NOT allowed.')
        RETURN
    END IF

    StrPtr => PtrToStr(Prefix)
    IF (ASSOCIATED(StrPtr)) THEN
        CALL Table%GetKeysWithPrefix(StrPtr, KeyQ)
    END IF
    NULLIFY(StrPtr)

    RETURN

END SUBROUTINE AlphabetTrie_KeysWithPrefix_VLCH

!******************************************************************************

SUBROUTINE AlphabetTrie_KeysWithPrefix_VLVL(Table, Prefix, Keys)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an array of all the keys in the symbol table that start with the
    !  specified prefix.  Return an unallocated array if no such key(s) found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),       INTENT(INOUT)    :: Table    !! symbol table
    TYPE(FvlStr),              INTENT(IN)       :: Prefix   !! prefix string
    TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)      :: Keys(:)  !! array of all keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: StrPtr

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_KeysWithPrefix_VLVL', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (GETLEN(Prefix) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_KeysWithPrefix_VLVL', ModName, ErrSevere, &
                             'A prefix string with length of zero is NOT allowed.')
        RETURN
    END IF

    StrPtr => PtrToStr(Prefix)
    IF (ASSOCIATED(StrPtr)) THEN
        CALL Table%GetKeysWithPrefix(StrPtr, Keys)
    END IF
    NULLIFY(StrPtr)

    RETURN

END SUBROUTINE AlphabetTrie_KeysWithPrefix_VLVL

!******************************************************************************

SUBROUTINE AlphabetTrie_AllKeys_ChrStr(Table, KeyQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a queue of all the keys in the symbol table.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    TYPE(QueueString),   INTENT(OUT)    :: KeyQ     !! queue of all keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_AllKeys_ChrStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    END IF

    CALL Table%GetKeysWithPrefix('', KeyQ)

    RETURN

END SUBROUTINE AlphabetTrie_AllKeys_ChrStr

!******************************************************************************

SUBROUTINE AlphabetTrie_AllKeys_FvlStr(Table, Keys)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an array of all the keys in the symbol table.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),       INTENT(INOUT)    :: Table    !! symbol table
    TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)      :: Keys(:)  !! array of all keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(QueueString)   :: KeyQ

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_AllKeys_FvlStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    END IF

    CALL Table%GetAllKeys(KeyQ)
    CALL KeyQueue2Array(KeyQ, Keys)

    RETURN

END SUBROUTINE AlphabetTrie_AllKeys_FvlStr

!******************************************************************************

RECURSIVE SUBROUTINE AlphabetTrie_AssembleKeys(Table, X, Prefix, Pattern, KeyQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assemble all of keys in the symbol table that match the given pattern where the
    !  question-mark character is interpreted as a wild card character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    TYPE(TrieNode),      INTENT(IN)     :: X        !! node representing current subtrie
    TYPE(StringBuilder), INTENT(INOUT)  :: Prefix   !! string builder (with a prefix of the pattern)
    tCharStar,           INTENT(IN)     :: Pattern  !! string pattern
    TYPE(QueueString),   INTENT(INOUT)  :: KeyQ     !! queue of strings matching the pattern
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: WildCard = '?'

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: C
    tCharLen(1) :: Chr
    tIndex      :: D

! FLOW
        
    IF (X%Chr == NULCHR) RETURN
    
    D = Prefix%Length() + 1_kIndex
    IF (D > LEN(Pattern, KIND=kIndex)) THEN
        IF (ALLOCATED(X%Value)) CALL KeyQ%EnQueue(Prefix%ToCharAlloc())
        RETURN
    END IF
    Chr = Pattern(D:D)
    IF (Chr == WildCard) THEN
        ! a wild card character found
        DO C = 0, Table%Alpha%GetRadix()-1
            CALL Prefix%Append(Table%Alpha%GetChar(C))
            CALL Table%Collect(X%Next(C), Prefix, Pattern, KeyQ)
            CALL Prefix%DelLastChar()
        END DO
    ELSE
        CALL Prefix%Append(Chr)
        C = Table%Alpha%GetIndex(Chr)
        IF (C == -1_kIndex) THEN
            CALL Handle_ErrLevel('AlphabetTrie_AssembleKeys', ModName, ErrSevere, &
                                 'The "' // Chr // '" character is NOT in the alphabet.')
            RETURN
        END IF
        CALL Table%Collect(X%Next(C), Prefix, Pattern, KeyQ)
        CALL Prefix%DelLastChar()
    END IF

    RETURN

END SUBROUTINE AlphabetTrie_AssembleKeys

!******************************************************************************

SUBROUTINE AlphabetTrie_KeysThatMatch_CHCH(Table, Pattern, KeyQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a queue of all the keys in the symbol table that match the
    !  specified pattern.  Return an empty queue if no such key(s) found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    tCharStar,           INTENT(IN)     :: Pattern  !! a pattern
    TYPE(QueueString),   INTENT(OUT)    :: KeyQ     !! queue of keys that match the specified pattern

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(StringBuilder) :: StrBld

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_KeysThatMatch_CHCH', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    END IF

    CALL StrBld%CreateEmpty()
    CALL Table%Collect(Table%Root, StrBld, Pattern, KeyQ)

    RETURN

END SUBROUTINE AlphabetTrie_KeysThatMatch_CHCH

!******************************************************************************

SUBROUTINE AlphabetTrie_KeysThatMatch_CHVL(Table, Pattern, Keys)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an array of all the keys in the symbol table that match the
    !  specified pattern.  Return an unallocated array if no such key(s) found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),       INTENT(INOUT)    :: Table    !! symbol table
    tCharStar,                 INTENT(IN)       :: Pattern  !! a pattern
    TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)      :: Keys(:)  !! array of all keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(QueueString)   :: KeyQ

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_KeysThatMatch_CHVL', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    END IF

    CALL Table%GetKeysThatMatch(Pattern, KeyQ)
    CALL KeyQueue2Array(KeyQ, Keys)

    RETURN

END SUBROUTINE AlphabetTrie_KeysThatMatch_CHVL

!******************************************************************************

SUBROUTINE AlphabetTrie_KeysThatMatch_VLCH(Table, Pattern, KeyQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a queue of all the keys in the symbol table that match the
    !  specified pattern.  Return an empty queue if no such key(s) found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    TYPE(FvlStr),        INTENT(IN)     :: Pattern  !! a pattern
    TYPE(QueueString),   INTENT(OUT)    :: KeyQ     !! queue of keys that match the specified pattern

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: StrPtr

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_KeysThatMatch_VLCH', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    END IF

    StrPtr => PtrToStr(Pattern)
    IF (ASSOCIATED(StrPtr)) THEN
        CALL Table%GetKeysThatMatch(StrPtr, KeyQ)
    END IF
    NULLIFY(StrPtr)

    RETURN

END SUBROUTINE AlphabetTrie_KeysThatMatch_VLCH

!******************************************************************************

SUBROUTINE AlphabetTrie_KeysThatMatch_VLVL(Table, Pattern, Keys)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an array of all the keys in the symbol table that match the
    !  specified pattern.  Return an unallocated array if no such key(s) found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),       INTENT(INOUT)    :: Table    !! symbol table
    TYPE(FvlStr),              INTENT(IN)       :: Pattern  !! a pattern
    TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)      :: Keys(:)  !! array of all keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: StrPtr

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_KeysThatMatch_VLVL', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    END IF

    StrPtr => PtrToStr(Pattern)
    IF (ASSOCIATED(StrPtr)) THEN
        CALL Table%GetKeysThatMatch(StrPtr, Keys)
    END IF
    NULLIFY(StrPtr)

    RETURN

END SUBROUTINE AlphabetTrie_KeysThatMatch_VLVL

!******************************************************************************

RECURSIVE SUBROUTINE AlphabetTrie_GatherWildcardKeys(Table, X, Prefix, Pattern, D, KeyQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To gather all of keys in the symbol table that match the given pattern where
    !  the question-mark character ('?') is interpreted as a wild card character for
    !  a single character and the asterisk character ('*') is interpreted as a wild
    !  card character for a sequence of characters.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    TYPE(TrieNode),      INTENT(IN)     :: X        !! node representing current subtrie
    TYPE(StringBuilder), INTENT(INOUT)  :: Prefix   !! string builder with the specified prefix
    tCharStar,           INTENT(IN)     :: Pattern  !! string pattern
    tIndex,              INTENT(IN)     :: D        !! current position of character in the pattern
    TYPE(QueueString),   INTENT(INOUT)  :: KeyQ     !! queue of strings with the specified prefix

!** SUBROUTINE PARAMETER DECLARATIONS:
    tCharParam  :: WildCard_Single   = '?'  ! a wild card for single character
    tCharParam  :: WildCard_Sequence = '*'  ! a wild card for a sequence of characters

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: C
    tCharLen(1) :: Chr

! FLOW
        
    IF (X%Chr == NULCHR) RETURN
    
    IF (D > LEN(Pattern, KIND=kIndex)) THEN
        IF (ALLOCATED(X%Value)) CALL KeyQ%EnQueue(Prefix%ToCharAlloc())
        RETURN
    END IF
    Chr = Pattern(D:D)
    IF (Chr == WildCard_Single) THEN
        ! a wild card for a single character found
        DO C = 0, Table%Alpha%GetRadix()-1
            CALL Prefix%Append(Table%Alpha%GetChar(C))
            CALL Table%Collect(X%Next(C), Prefix, Pattern, D+1_kIndex, KeyQ)
            CALL Prefix%DelLastChar()
        END DO
    ELSEIF (Chr == WildCard_Sequence) THEN
        ! a wild card for a sequence of characters found
        CALL Table%Collect(X, Prefix, Pattern, D+1_kIndex, KeyQ)
        DO C = 0, Table%Alpha%GetRadix()-1
            CALL Prefix%Append(Table%Alpha%GetChar(C))
            CALL Table%Collect(X%Next(C), Prefix, Pattern, D, KeyQ)
            CALL Prefix%DelLastChar()
        END DO
    ELSE
        CALL Prefix%Append(Chr)
        C = Table%Alpha%GetIndex(Chr)
        IF (C == -1_kIndex) THEN
            CALL Handle_ErrLevel('AlphabetTrie_GatherWildcardKeys', ModName, ErrSevere, &
                                 'The "' // Chr // '" character is NOT in the alphabet.')
            RETURN
        END IF
        CALL Table%Collect(X%Next(C), Prefix, Pattern, D+1_kIndex, KeyQ)
        CALL Prefix%DelLastChar()
    END IF

    RETURN

END SUBROUTINE AlphabetTrie_GatherWildcardKeys

!******************************************************************************

SUBROUTINE AlphabetTrie_WildcardKeys_CHCH(Table, Pattern, KeyQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a queue of all the keys in the symbol table that match the
    !  specified pattern.  Return an empty queue if no such key(s) found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    tCharStar,           INTENT(IN)     :: Pattern  !! a pattern
    TYPE(QueueString),   INTENT(OUT)    :: KeyQ     !! queue of keys that match the specified pattern

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(StringBuilder) :: StrBld

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_WildcardKeys_CHCH', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    END IF

    CALL StrBld%CreateEmpty()
    CALL Table%Collect(Table%Root, StrBld, Pattern, 1_kIndex, KeyQ)

    RETURN

END SUBROUTINE AlphabetTrie_WildcardKeys_CHCH

!******************************************************************************

SUBROUTINE AlphabetTrie_WildcardKeys_CHVL(Table, Pattern, Keys)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an array of all the keys in the symbol table that match the
    !  specified pattern.  Return an unallocated array if no such key(s) found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),       INTENT(INOUT)    :: Table    !! symbol table
    tCharStar,                 INTENT(IN)       :: Pattern  !! a pattern
    TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)      :: Keys(:)  !! array of all keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(QueueString)   :: KeyQ

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_WildcardKeys_CHVL', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    END IF

    CALL Table%GetWildcardKeys(Pattern, KeyQ)
    CALL KeyQueue2Array(KeyQ, Keys)

    RETURN

END SUBROUTINE AlphabetTrie_WildcardKeys_CHVL

!******************************************************************************

SUBROUTINE AlphabetTrie_WildcardKeys_VLCH(Table, Pattern, KeyQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a queue of all the keys in the symbol table that match the
    !  specified pattern.  Return an empty queue if no such key(s) found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    TYPE(FvlStr),        INTENT(IN)     :: Pattern  !! a pattern
    TYPE(QueueString),   INTENT(OUT)    :: KeyQ     !! queue of keys that match the specified pattern

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: StrPtr

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_WildcardKeys_VLCH', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    END IF

    StrPtr => PtrToStr(Pattern)
    IF (ASSOCIATED(StrPtr)) THEN
        CALL Table%GetWildcardKeys(StrPtr, KeyQ)
    END IF
    NULLIFY(StrPtr)

    RETURN

END SUBROUTINE AlphabetTrie_WildcardKeys_VLCH

!******************************************************************************

SUBROUTINE AlphabetTrie_WildcardKeys_VLVL(Table, Pattern, Keys)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an array of all the keys in the symbol table that match the
    !  specified pattern.  Return an unallocated array if no such key(s) found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie),       INTENT(INOUT)    :: Table    !! symbol table
    TYPE(FvlStr),              INTENT(IN)       :: Pattern  !! a pattern
    TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)      :: Keys(:)  !! array of all keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: StrPtr

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_WildcardKeys_VLVL', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    END IF

    StrPtr => PtrToStr(Pattern)
    IF (ASSOCIATED(StrPtr)) THEN
        CALL Table%GetWildcardKeys(StrPtr, Keys)
    END IF
    NULLIFY(StrPtr)

    RETURN

END SUBROUTINE AlphabetTrie_WildcardKeys_VLVL

!******************************************************************************

RECURSIVE FUNCTION AlphabetTrie_LengthOfLongestPrefix(Table, X, Query, D, LenIn) RESULT(LenOut)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the length of the longest key in the subtrie rooted at the *X* node
    !  that is a prefix of the specified query, assuming that the first *D* character
    !  match and a prefix match of the given length (-1 if no such match) has already
    !  been found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    TYPE(TrieNode),      INTENT(IN)     :: X        !! node representing current subtrie
    tCharStar,           INTENT(IN)     :: Query    !! string query
    tIndex,              INTENT(IN)     :: D        !! current position of character in the query
    tIndex                              :: LenIn    !! input length
    tIndex                              :: LenOut   !! length of the longest prefix of the query

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: C

! FLOW

    LenOut = LenIn  ! set default value
    IF (X%Chr == NULCHR) RETURN
    IF (ALLOCATED(X%Value)) LenOut = D
    IF (D > LEN(Query, KIND=kIndex)) RETURN
    C = Table%Alpha%GetIndex(Query(D:D))
    IF (C == -1_kIndex) THEN
        CALL Handle_ErrLevel('AlphabetTrie_LengthOfLongestPrefix', ModName, ErrSevere, &
                             'The "' // Query(D:D) // '" character is NOT in the alphabet.')
        RETURN
    END IF
    LenOut = Table%LongestPrefixLen(X%Next(C), Query, D+1_kIndex, LenOut)

    RETURN

END FUNCTION AlphabetTrie_LengthOfLongestPrefix

!******************************************************************************

SUBROUTINE AlphabetTrie_LongestPrefixOf_ChrStr(Table, Query, Prefix)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the string in the symbol table that is the longest prefix of the
    !  specified query.  Return an unallocated string if no such string found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    tCharStar,           INTENT(IN)     :: Query    !! a query
    tCharAlloc,          INTENT(OUT)    :: Prefix   !! the longest prefix of the query

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_LongestPrefixOf_ChrStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (LEN(Query) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_LongestPrefixOf_ChrStr', ModName, ErrSevere, &
                             'A query string with length of zero is NOT allowed.')
        RETURN
    END IF

    Length = Table%LongestPrefixLen(Table%Root, Query, 1_kIndex, -1_kIndex)
    IF (Length /= -1_kIndex) Prefix = Query(1:Length)

    RETURN

END SUBROUTINE AlphabetTrie_LongestPrefixOf_ChrStr

!******************************************************************************

SUBROUTINE AlphabetTrie_LongestPrefixOf_FvlStr(Table, Query, Prefix)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the string in the symbol table that is the longest prefix of the
    !  specified query.  Return an unallocated string if no such string found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(INOUT)  :: Table    !! symbol table
    TYPE(FvlStr),        INTENT(IN)     :: Query    !! a query
    TYPE(FvlStr),        INTENT(OUT)    :: Prefix   !! the longest prefix of the query

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: Length
    tCharLen(:), POINTER    :: StrPtr

! FLOW

    IF (.NOT.ASSOCIATED(Table%Alpha)) THEN
        CALL Handle_ErrLevel('AlphabetTrie_LongestPrefixOf_FvlStr', ModName, ErrSevere, &
                             'The symbol table has not yet been constructed.')
        RETURN
    ELSEIF (GETLEN(Query) == 0) THEN
        CALL Handle_ErrLevel('AlphabetTrie_LongestPrefixOf_FvlStr', ModName, ErrSevere, &
                             'A query string with length of zero is NOT allowed.')
        RETURN
    END IF

    StrPtr => PtrToStr(Query)
    IF (ASSOCIATED(StrPtr)) THEN
        Length = Table%LongestPrefixLen(Table%Root, StrPtr, 1_kIndex, -1_kIndex)
        IF (Length /= -1_kIndex) Prefix = StrPtr(1:Length)
    END IF
    NULLIFY(StrPtr)

    RETURN

END SUBROUTINE AlphabetTrie_LongestPrefixOf_FvlStr

!******************************************************************************

FUNCTION AlphabetTrie_IsEmpty(Table) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the table is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(IN) :: Table    !! symbol table
    tLogical                        :: Flag     !! true if the table is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Flag = (Table%N == 0_kIndex)

    RETURN

END FUNCTION AlphabetTrie_IsEmpty

!******************************************************************************

FUNCTION AlphabetTrie_GetSize(Table) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get size (number of keys stored) of the table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(AlphabetTrie), INTENT(IN) :: Table    !! symbol table
    tIndex                          :: Size     !! size of the table

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Size = Table%N
        
    RETURN
        
END FUNCTION AlphabetTrie_GetSize

!******************************************************************************

SUBROUTINE KeyQueue2Array(KeyQ, Keys)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a queue of character-string keys to an array of keys of
    !  the FvlStr type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(QueueString),         INTENT(INOUT)    :: KeyQ     !! queue of keys
    TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)      :: Keys(:)  !! array of keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Success
    tIndex      :: I
    tCharAlloc  :: Key

! FLOW

    IF (.NOT.KeyQ%IsEmpty()) THEN
        ALLOCATE(Keys(KeyQ%GetSize()))
        I = 1_kIndex
        DO WHILE (.NOT.KeyQ%IsEmpty())
            Success = KeyQ%Dequeue(Key)
            IF (Success) THEN
                Keys(I) = Key
                I = I + 1_kIndex
            END IF
        END DO
    END IF

    RETURN

END SUBROUTINE KeyQueue2Array

!******************************************************************************

END MODULE MClass_AlphabetTrie
    
!******************************************************************************
