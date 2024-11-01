
MODULE MBase_BalancedTrees

!^  **PURPOSE OF THIS MODULE**: <br>
!   This module collectively contains *balanced-tree-based* types from other modules.
!   It is provided so that a user can refer to this module instead of referring to several
!   individual modules when various types are needed concurrently. <br>
!   <br>
!   **Overview**: <br>
!   These balanced-tree-based types are container types that employ a balanced binary-search-tree
!   (BST) implementation to provide common operations for an ordered symbol table.  Specifically,
!   all balanced-tree-based types provided here utilize a left-leaning red-black (RB) tree as the
!   balanced BST.  Each individual type can be used to store key-value pairs for a specific type
!   of keys where the type of stored keys is one of Fortran intrinsic *comparable* types (i.e.
!   CHARACTER, INTEGER and REAL) or a derived type in the *Comparable* class (i.e. the *Comparable*
!   type or its subtypes).  Most of the balanced-tree-based types (except the *TreeComparable* type)
!   use an unlimited polymorphic type to store values; therefore, inserted values can have any types.
!   Unlike other balanced-tree-based types, the *TreeComparable* type uses the *Comparable* type to
!   represent a key-value pair and requires only one argument (instead of two) when inserting or
!   retrieving the key and its associated value.  Therefore, its application programming interface
!   (API) is slightly different from the API of other balanced-tree-based types.  <br>
!   Available balanced-tree-based container types include: <br>
!   - the <a href="../module/mclass_treecharacter.html#type-treecharacter">TreeCharacter</a> type
!     for character string key type, <br>
!   - the <a href="../module/mclass_treeinteger1b.html#type-treeinteger1b">TreeInteger1B</a> type
!     for 1-byte (or 8-bit) integer key type, <br>
!   - the <a href="../module/mclass_treeinteger2b.html#type-treeinteger2b">TreeInteger2B</a> type
!     for 2-byte (or 16-bit) integer key type, <br>
!   - the <a href="../module/mclass_treeinteger4b.html#type-treeinteger4b">TreeInteger4B</a> type
!     for 4-byte (or 32-bit) integer key type, <br>
!   - the <a href="../module/mclass_treeinteger8b.html#type-treeinteger8b">TreeInteger8B</a> type
!     for 8-byte (or 64-bit) integer key type, <br>
!   - the <a href="../module/mclass_treerealsp.html#type-treerealsp">TreeRealSP</a> type
!     for single-precision real key type, <br>
!   - the <a href="../module/mclass_treerealdp.html#type-treerealdp">TreeRealDP</a> type
!     for double-precision real key type, <br>
!   - the <a href="../module/mclass_treerealqp.html#type-treerealqp">TreeRealQP</a> type
!     for quadruple-precision real key type, <br>
!   - the <a href="../module/mclass_treecomparable.html#type-treecomparable">TreeComparable</a>
!     type for type of key-value pair in *Comparable* class. <br>
!   Each balanced-tree-based container type represents an ordered symbol table where various common operations
!   are provided and can be categorized as follows. <br>
!   (1) Construction and Destruction.  Methods for these operations include: <br>
!   - *Construct* method - method to construct the container from arrays of keys and values, and <br>
!   - *Destruct* method - method to destruct the container by removing all key-value pairs from
!       the container. <br>
!   (2) Insertion and Removal.  Methods for these operations include: <br>
!   - *Insert* method - method to insert a key and its associated value into the container, <br>
!   - *Remove* method - method to remove a key (and its associated value) from the container, <br>
!   - *RemoveMin* method - method to remove (and optionally retrieve) the smallest key-value pair, and <br>
!   - *RemoveMax* method - method to remove (and optionally retrieve) the largest key-value pair. <br>
!   (3) Retrieval.  Methods for this operation include: <br>
!   - *GetMinKey* method - method to retrieve the smallest key-value pair of the container, <br>
!   - *GetMaxKey* method - method to retrieve the largest key-value of the container, <br>
!   - *GetKeys* method - method to retrieve all keys (in the tree or in the specified range) and
!       their associated values, <br>
!   - *Floor* method - method to retrieve the largest key (and a value associated with it) in the
!       tree less than or equal to the given key. <br>
!   - *Ceiling* method - method to retrieve the smallest key (and a value associated with it) in
!       the tree greater than or equal to the given key, and <br>
!   - *Select* method - method to retrieve the key-value pair of the specified rank. <br>
!   (4) Inquiry.  Methods for this operation include: <br>
!   - *IsEmpty* method - method to check whether the container is empty or not, <br>
!   - *Contain* method - method to check whether the specified key is in the container or not, <br>
!   - *GetSize* method - method to get the container size (number of key-value pairs stored), <br>
!   - *GetRangeSize* method - method to get the number of key-value pairs in the specified range, and <br>
!   - *GetRank* method - method to get the number of keys in the tree strictly less than the given key. <br>
!   (5) Iteration.  Methods for this operation include: <br>
!   - *StartFirst* method - method to start a forward iteration over key-value pairs, <br>
!   - *MoveForward* method - method to move forward to the next key-value pair, <br>
!   - *StartLast* method - method to start a backward iteration over key-value pairs, and <br>
!   - *MoveBackward* method - method to move backward to the next key-value pair. <br>

!** USE STATEMENTS:
    USE MClass_TreeCharacter
    USE MClass_TreeInteger1B
    USE MClass_TreeInteger2B
    USE MClass_TreeInteger4B
    USE MClass_TreeInteger8B
    USE MClass_TreeRealSP
    USE MClass_TreeRealDP
    USE MClass_TreeRealQP
    USE MClass_TreeComparable

END MODULE MBase_BalancedTrees
