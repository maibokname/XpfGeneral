
MODULE MBase_HashTables

!^  **PURPOSE OF THIS MODULE**: <br>
!   This module collectively contains *hash-table* container types from other modules. It is
!   provided so that a user can refer to this module instead of referring to several individual
!   modules when various types are needed concurrently. <br>
!   <br>
!   **Overview**: <br>
!   These hash-table container types are container types that employ a hash table implementation
!   to provide common operations for an unordered symbol table.  Technically, all of them employ
!   the open-addressing as a collision resolution technique where the hash resolution is performed
!   through probing.  They provide three probing algorithms: linear probing, quadratic probing and
!   double hashing.  By default, the linear probing algorithm is used.  However, a user can specify
!   other probing algorithm during a construction of a table. <br>
!   Each individual type can be used to store key-value pairs for a specific type of keys where the
!   allowed types of stored keys include the Fortran intrinsic CHARACTER, INTEGER and REAL types as
!   well as any derived type in the *Object* class.  Most of the hash-table container types (except
!   the *HTabObject* type) utilize an unlimited polymorphic type to store values; therefore, a type
!   of inserted values can be any data type.  Unlike other types, the *HTabObject* type uses the
!   *Object* type to represent both key and value, and it requires only one argument (instead of
!   two) when inserting or retrieving the key and its associated value.  Therefore, its application
!   programming interface (API) is slightly different from the API of other hash-table types.  <br>
!   Available hash-table-based container types include: <br>
!   - the <a href="../module/mclass_htabcharacter.html#type-htabcharacter">HTabCharacter</a> type
!     for character string key type, <br>
!   - the <a href="../module/mclass_htabinteger1b.html#type-htabinteger1b">HTabInteger1B</a> type
!     for 1-byte (or 8-bit) integer key type, <br>
!   - the <a href="../module/mclass_htabinteger2b.html#type-htabinteger2b">HTabInteger2B</a> type
!     for 2-byte (or 16-bit) integer key type, <br>
!   - the <a href="../module/mclass_htabinteger4b.html#type-htabinteger4b">HTabInteger4B</a> type
!     for 4-byte (or 32-bit) integer key type, <br>
!   - the <a href="../module/mclass_htabinteger8b.html#type-htabinteger8b">HTabInteger8B</a> type
!     for 8-byte (or 64-bit) integer key type, <br>
!   - the <a href="../module/mclass_htabrealsp.html#type-htabrealsp">HTabRealSP</a> type
!     for single-precision real key type, <br>
!   - the <a href="../module/mclass_htabrealdp.html#type-htabrealdp">HTabRealDP</a> type
!     for double-precision real key type, <br>
!   - the <a href="../module/mclass_htabrealqp.html#type-htabrealqp">HTabRealQP</a> type
!     for quadruple-precision real key type, <br>
!   - the <a href="../module/mclass_htabobject.html#type-htabobject">HTabObject</a>
!     type for a derived type of key-value pair in the *Object* class. <br>
!   Each hash-table-based container type represents an unordered symbol table where various common
!   operations are provided and can be categorized as follows. <br>
!   (1) Construction and Destruction.  Methods for these operations include: <br>
!   - *CreateEmpty* method - method to construct an empty container, <br>
!   - *Construct* method - method to construct a container from arrays of keys and values, and <br>
!   - *Destruct* method - method to destruct a container by removing all key-value pairs from
!       the container as well as free memory storage occupied by the container. <br>
!   (2) Insertion and Removal.  Methods for these operations include: <br>
!   - *Insert* method - method to insert a key and its associated value into a container, <br>
!   - *Remove* method - method to remove a key (and its associated value) from a container, and <br>
!   - *Clear* method - method to remove all key-value pairs from a container. <br>
!   (3) Retrieval.  A method for this operation is: <br>
!   - *GetKeys* method - method to retrieve all keys and optionally all their associated values. <br>
!   (4) Inquiry.  Methods for this operation include: <br>
!   - *IsEmpty* method - method to check whether the container is empty or not, <br>
!   - *Contain* method - method to check whether the specified key is in the container or not, and <br>
!   - *GetSize* method - method to get the container size (number of key-value pairs stored). <br>
!   (5) Iteration.  Methods for this operation include: <br>
!   - *StartFirst* method - method to start a forward iteration over key-value pairs, and <br>
!   - *MoveForward* method - method to move forward to the next key-value pair. <br>

!** USE STATEMENTS:
    USE MClass_HTabCharacter
    USE MClass_HTabInteger1B
    USE MClass_HTabInteger2B
    USE MClass_HTabInteger4B
    USE MClass_HTabInteger8B
    USE MClass_HTabRealSP
    USE MClass_HTabRealDP
    USE MClass_HTabRealQP
    USE MClass_HTabObject

END MODULE MBase_HashTables
