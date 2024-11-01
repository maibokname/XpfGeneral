
MODULE MClass_StackChar

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *StackChar* type and its related/supporting routines and derived type(s).
!   The *StackChar* type is a stack container with *CHARACTER* as the type of its stored items.  It
!   employs a conventional singly-linked list implementation. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MemHandlers
    USE MBase_ErrHandlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: StackChar

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_StackChar'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    !> *LinkedNode* is a node type that consists of an item and a pointer of the node type.  The
    !   node can point to its adjacent node in one direction (next node) allowing a singly-linked
    !   list to be formed. <br>
    !   The type of the item stored in this node is *CHARACTER*.  This node type is a private type.
    TYPE LinkedNode
        !% *Content* is a content (or item or value) stored in the stack.
        tCharAlloc                  :: Content
        !% pointer to next node
        TYPE(LinkedNode), POINTER   :: Next => NULL()
    CONTAINS
        !% destructor procedure
        PROCEDURE   :: Destruct => LinkedNode_Destructor
    END TYPE LinkedNode
    !> *StackChar* is a container type that employs a singly-linked list implementation to
    !   provide common operations for a LIFO stack container.  The type of items stored in
    !   this container is *CHARACTER*.
    TYPE StackChar
        PRIVATE
        !% size of the stack container (number of items or nodes in the stack)
        tIndex                      :: Size = 0
        !% pointer to the first item (or the head node) of the stack
        TYPE(LinkedNode), POINTER   :: Head => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----             PRIVATE PROCEDURES                            -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: Cleanup          => StackList_Cleanup
        PROCEDURE, PRIVATE  :: SetNewNode       => StackList_SetNewNode
        PROCEDURE, PRIVATE  :: RemoveNode       => StackList_RemoveFirstNode
        ! ---------------------------------------------------------------------
        ! -----             PUBLIC PROCEDURES                             -----
        ! ---------------------------------------------------------------------
        ! -----             Constructor and Destructor Procedures         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateByArray <br>
        ! **Purpose**:  To construct a stack from an array of items. <br>
        !  **Usage**: <br>
        !   --->    CALL Stack%CreateByArray(10, Arr)    ! create a stack from an array of 10 items
        PROCEDURE           :: CreateByArray    => StackList_ConstructorByArray
        !> **Type-Bound Subroutine**: Destroy <br>
        ! **Purpose**:  To destruct a stack and get its contents if requested. <br>
        !  **Usage**: <br>
        !   --->    CALL Stack%Destruct()       ! destruct the stack <br>
        !   --->    CALL Stack%Destruct(N, Arr) ! destruct the stack and get its contents <br>
        PROCEDURE           :: Destroy          => StackList_Destructor
        ! ---------------------------------------------------------------------
        ! -----             Insertion and Removal Procedures              -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Push <br>
        ! **Purpose**:  To add a new content to the top of the stack. <br>
        !  **Usage**: <br>
        !   --->    CALL Stack%Push(NewItem) <br>
        PROCEDURE           :: Push             => StackList_PushItem
        !> **Type-Bound Subroutine**: Pop <br>
        !  **Purpose**:  To get and remove the top item of the stack.  Optionally, only
        !                get the item if Peek is true. <br>
        !  **Usage**: <br>
        !   --->    CALL Stack%Pop(Item) <br>
        !   --->    CALL Stack%Pop(Item, Peek=.TRUE.) <br>
        PROCEDURE           :: Pop              => StackList_PopItem
        ! ---------------------------------------------------------------------
        ! -----                 Traversing Procedure                      -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Traverse <br>
        !  **Purpose**:  To traverse the stack and get its contents along the way
        !                by providing a user-supplied procedure. <br>
        !  **Usage**: <br>
        !   --->    CALL Stack%Traverse(UserFunc) <br>
        PROCEDURE           :: Traverse         => StackList_Traverse
        ! ---------------------------------------------------------------------
        ! -----                 Inquiry Procedures                       ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the stack is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Stack%IsEmpty() <br>
        !   --->    IF (.NOT.Stack%IsEmpty()) DoSomeThing
        PROCEDURE           :: IsEmpty          => StackList_IsEmpty
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get size of the stack. <br>
        !  **Usage**: <br>
        !   --->    StackSize = Stack%GetSize()
        PROCEDURE           :: GetSize          => StackList_GetSize
        ! ---------------------------------------------------------------------
        ! -----                 Final Procedure                           -----
        ! ---------------------------------------------------------------------
        FINAL               :: StackList_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE StackChar

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> IteratureFunc is a user-suppled procedure used to traverse the stack
        !  in order to get the stack's contents
        FUNCTION IteratorFunc(Content,Done) RESULT(ErrStat)
            IMPORT
            tCharStar, INTENT(IN)       :: Content  !! content
            tLogical,  INTENT(INOUT)    :: Done
            !^ on input, Done is set to false. <br>
            !  on exit, set it to TrueVal if user want to stop the stack traversing. <br>
            tLogical                    :: ErrStat  !! true if error occurred in the user routine
        END FUNCTION IteratorFunc
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Routines for LinkedNode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE LinkedNode_Deallocate(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To deallocate LinkedNode pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(LinkedNode), POINTER, INTENT(INOUT)    :: Node !! LinkedNode object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW
    
    IF (ASSOCIATED(Node)) THEN
        DEALLOCATE(Node, STAT=AllocStat, ERRMSG=AllocMsg)
        CALL Handle_ErrDealloc('LinkedNode_Deallocate', ModName, AllocMsg, AllocStat)
    END IF
       
    RETURN

END SUBROUTINE LinkedNode_Deallocate

!******************************************************************************

SUBROUTINE LinkedNode_Destructor(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct LinkedNode object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LinkedNode), INTENT(INOUT)    :: Node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    NULLIFY(Node%Next)
       
    RETURN

END SUBROUTINE LinkedNode_Destructor

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Routines for StackChar
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE StackList_ConstructorByArray(Stack, N, Contents)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a stack from an array of content.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackChar), INTENT(INOUT) :: Stack        !! StackChar object
    tIndex,           INTENT(IN)    :: N            !! number of contents
    tCharStar,        INTENT(IN)    :: Contents(N)  !! content array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW
        
    ! simply return if N is less than 1
    IF (N <= 0) RETURN
        
    ! built stack of input contents
    DO I = 1, N
        CALL Stack%Push(Contents(I))
    END DO
       
    RETURN

END SUBROUTINE StackList_ConstructorByArray

!******************************************************************************

SUBROUTINE StackList_Cleanup(Stack)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free up memory of the stack.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackChar), INTENT(INOUT) :: Stack !! StackChar object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! traverse the stack and destroy all nodes
    DO WHILE (.NOT.Stack%IsEmpty())
        CALL Stack%RemoveNode()
    END DO
        
    ! reset components
    Stack%Size = 0
    NULLIFY(Stack%Head)
        
    RETURN

END SUBROUTINE StackList_Cleanup

!******************************************************************************

SUBROUTINE StackList_Destructor(Stack, N, Contents)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct StackChar object and get its contents if requested.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackChar),     INTENT(INOUT) :: Stack        !! StackChar object
    tIndex,     OPTIONAL, INTENT(OUT)   :: N            !! number of contents
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: Contents(:)  !! allocatable array of contents

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MaxLength
    tIndex      :: I

! FLOW
        
    IF (PRESENT(N).AND.PRESENT(Contents)) THEN
        ! get contents if requested
        IF (.NOT.Stack%IsEmpty()) THEN
            ! get number of contents
            N = Stack%Size
            ! determine maximum length
            MaxLength = 0
            CALL Stack%Traverse(MaxLengthIterator)
            ! allocate storage for output
            CALL MemAlloc(Contents, MaxLength, [N])
            ! get contents
            I = 0
            CALL Stack%Traverse(ContentIterator)
        END IF
    END IF
        
    ! destroy all nodes and free up memory
    CALL Stack%Cleanup()
       
    RETURN
        
CONTAINS
    
    FUNCTION MaxLengthIterator(Content,Done) RESULT(ErrStat)
        ! arguments
        tCharStar, INTENT(IN)       :: Content  ! content
        tLogical,  INTENT(INOUT)    :: Done     ! on input, Done is set to FalseVal
                                                ! on exit, set it to TrueVal if user
                                                !   want to stop the stack traversing.
        tLogical                    :: ErrStat  ! true if error occurred in the user routine
        ! set flag
        ErrStat = FalseVal
        Done = FalseVal
        ! determine maximum length
        IF (MaxLength < LEN(Content)) MaxLength = LEN(Content)
        RETURN
    END FUNCTION MaxLengthIterator

    !**************************************************************************
        
    FUNCTION ContentIterator(Content,Done) RESULT(ErrStat)
        ! arguments
        tCharStar, INTENT(IN)       :: Content  ! content
        tLogical,  INTENT(INOUT)    :: Done     ! on input, Done is set to FalseVal
                                                ! on exit, set it to TrueVal if user
                                                !   want to stop the stack traversing.
        tLogical                    :: ErrStat  ! true if error occurred in the user routine
        ! set flag
        ErrStat = FalseVal
        Done = FalseVal
        ! get content
        I = I + 1
        Contents(I) = Content
        RETURN
    END FUNCTION ContentIterator

    !**************************************************************************
        
END SUBROUTINE StackList_Destructor

!******************************************************************************

SUBROUTINE StackList_Finalizer(Stack)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(StackChar), INTENT(INOUT)  :: Stack    !! StackChar object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! destroy all nodes and free up memory
    CALL Stack%Cleanup()
       
    RETURN

END SUBROUTINE StackList_Finalizer

!******************************************************************************

SUBROUTINE StackList_SetNewNode(Stack, NewNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set pointer for newly created node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackChar),          INTENT(INOUT)    :: Stack    !! StackChar object
    TYPE(LinkedNode), POINTER, INTENT(IN)       :: NewNode  !! new node to be added to the stack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! check if stack is empty or not
    IF (Stack%IsEmpty()) THEN
        Stack%Head => NewNode
    ELSE
        NewNode%Next => Stack%Head
        Stack%Head   => NewNode
    END IF
        
    ! set stack size
    Stack%Size = Stack%Size + 1
        
    RETURN

END SUBROUTINE StackList_SetNewNode

!******************************************************************************

SUBROUTINE StackList_RemoveFirstNode(Stack, Content)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the first (top) node from the stack.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackChar),     INTENT(INOUT) :: Stack    !! StackChar object
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: Content  !! content of the removed node if requested

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(LinkedNode), POINTER   :: DelNode => NULL()

! FLOW
        
    ! set routine name
    SubName = 'StackList_RemoveFirstNode'

    ! check whether requesting the content of the node to be removed
    IF (PRESENT(Content)) THEN
        Content = Stack%Head%Content
    END IF
    ! check whether there is only one node or not
    IF (Stack%Size == 1) THEN
        ! the stack has only one node so reset the stack
        CALL Stack%Head%Destruct()
        CALL LinkedNode_Deallocate(Stack%Head)
        Stack%Size = 0
    ELSE
        ! the stack has two or more nodes
        DelNode => Stack%Head
        Stack%Head => DelNode%Next
        ! remove the node
        CALL DelNode%Destruct()
        CALL LinkedNode_Deallocate(DelNode)
        ! set size
        Stack%Size = Stack%Size - 1
    END IF
        
    RETURN
        
END SUBROUTINE StackList_RemoveFirstNode

!******************************************************************************

SUBROUTINE StackList_PushItem(Stack, NewItem)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new item to the top of the stack by allocation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackChar), INTENT(INOUT) :: Stack    !! StackChar object
    tCharStar,        INTENT(IN)    :: NewItem  !! new item to be added to the stack

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(LinkedNode), POINTER   :: NewNode => NULL()
    tSInt32                     :: AllocStat
    tCharLen(MsgLen)            :: AllocMsg

! FLOW
        
    ! allocate new node, make a copy of the content and set content status
    ALLOCATE(NewNode, STAT=AllocStat, ERRMSG=AllocMsg)
    CALL Handle_ErrAlloc('LinkedList_Append', ModName, AllocMsg, AllocStat)
    NewNode%Content = NewItem
        
    ! set pointers to new node
    CALL Stack%SetNewNode(NewNode)
        
    ! free up memory
    NULLIFY(NewNode)

    RETURN

END SUBROUTINE StackList_PushItem

!******************************************************************************

SUBROUTINE StackList_PopItem(Stack, TopItem, Peek)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get and remove the top item from the stack.
    !  Optionally, only get the item if Peek is true

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackChar),   INTENT(INOUT)   :: Stack    !! StackChar object
    tCharAlloc,         INTENT(OUT)     :: TopItem  !! top item
    tLogical, OPTIONAL, INTENT(IN)      :: Peek     !! true if only peek the top item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: GetContentOnly

! FLOW
    
    ! first, check whether the stack is empty or not
    IF (Stack%IsEmpty()) THEN
        ! report error
        CALL Handle_ErrLevel('PopItem', ModName, ErrWarning, 'The stack is EMPTY.')
        RETURN
    END IF
        
    ! set default and check optional input
    SET_OPTION(GetContentOnly, FalseVal, Peek)
        
    ! then, get the top item in the stack out
    IF (GetContentOnly) THEN
        ! get item only
        TopItem = Stack%Head%Content
    ELSE
        ! get and remove item from the stack
        CALL Stack%RemoveNode(TopItem)
    END IF
       
    RETURN

END SUBROUTINE StackList_PopItem

!******************************************************************************

SUBROUTINE StackList_Traverse(Stack, IterSub)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To traverse the stack and get its contents along the way.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackChar), INTENT(INOUT) :: Stack    !! StackChar object
    PROCEDURE(IteratorFunc)         :: IterSub  !! user-supplied procedure

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(LinkedNode), POINTER   :: CurrNode => NULL()
    tLogical                    :: ErrStat
    tLogical                    :: Done

! FLOW
        
    ! return if the stack is empty
    IF (Stack%IsEmpty()) RETURN
        
    ! set defaults
    ErrStat = FalseVal
    Done = FalseVal

    ! initialize current node
    CurrNode => Stack%Head
        
    ! loop over all nodes of the stack
    DO WHILE (ASSOCIATED(CurrNode))
            
        ! call iterator subroutine
        ErrStat = IterSub(CurrNode%Content, Done)
            
        ! report error if necessary
        IF (ErrStat) THEN
            CALL Handle_ErrLevel('StackList_Traverse', ModName, ErrSevere, &
                                 'An error occurred during call to iterator function.')
            RETURN
        END IF
            
        ! exit the loop if the user want to stop the traversing
        IF (Done) EXIT

        ! set current node
        CurrNode => CurrNode%Next

    END DO
        
    NULLIFY(CurrNode)

    RETURN

END SUBROUTINE StackList_Traverse

!******************************************************************************

FUNCTION StackList_IsEmpty(Stack) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the stack is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackChar), INTENT(IN)    :: Stack    !! StackChar object
    tLogical                        :: Flag     !! true if the stack is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = .NOT.ASSOCIATED(Stack%Head)

    RETURN

END FUNCTION StackList_IsEmpty

!******************************************************************************

FUNCTION StackList_GetSize(Stack) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get size of the stack.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackChar), INTENT(IN)    :: Stack    !! StackChar object
    tIndex                          :: Size     !! stack size (number of nodes)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = Stack%Size

    RETURN

END FUNCTION StackList_GetSize

!******************************************************************************

END MODULE MClass_StackChar

!******************************************************************************
