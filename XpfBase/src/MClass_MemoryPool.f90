
MODULE MClass_MemoryPool

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *MemoryPool* type and its supporting data types and related routines.
!   The *MemoryPool* type is a general data storage type that can be used to store data of any types.
!   Internally, it stores the specified data as an array of bytes.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_MemHandlers
    USE MBase_ByteUtil
    USE MClass_FreeBlockList

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: MemoryPool
    PUBLIC :: MemHandle

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_MemoryPool'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! Default block Size in bytes
    tIndex,    PARAMETER    :: DefaultBlockSize = 1024_kIndex
    ! Minimum block size in bytes
    tIndex,    PARAMETER    :: MinBlockSize = 128_kIndex
    ! Minimum block size in bytes
    tIndex,    PARAMETER    :: MaxBlockSize = 8192_kIndex

!** DERIVED TYPE DEFINITIONS
    !> The *MemStore* type is a storage type.  This is a helper and private type.
    TYPE MemStore
        !> the block size used for reallocation
        tIndex                  :: BlockSize = 0_kIndex
        !> an array representing the storage of the memory pool
        tSInt8, ALLOCATABLE     :: Storage(:)
    CONTAINS
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Initialize <br>
        ! **Purpose**:  To perform initialization of the *MemStore* object. <br>
        !  **Usage**: <br>
        !   --->    CALL Store%Initialize(BlockSize)
        PROCEDURE   :: Initialize   => MemStore_Initialize
        !> **Type-Bound Subroutine**: Put <br>
        ! **Purpose**:  To store the specified data in the memory pool. <br>
        !  **Usage**: <br>
        !   --->    CALL Store%Put(Data, Position)
        PROCEDURE   :: Put          => MemStore_Put
        !> **Type-Bound Function**: Get <br>
        ! **Purpose**:  To retrieve the data from the memory pool. <br>
        !  **Usage**: <br>
        !   --->    CALL Store%Get(Data, Position) <br>
        PROCEDURE   :: Get          => MemStore_Get
        !> **Type-Bound Function**: GetPtr <br>
        ! **Purpose**:  To get a pointer to the data stored in the memory pool. <br>
        !  **Usage**: <br>
        !   --->    DatPtr => Store%GetPtr(Position, Size) <br>
        PROCEDURE   :: GetPtr       => MemStore_Retrieve
        !> **Type-Bound Subroutine**: Resize <br>
        ! **Purpose**:  To resize the storage of the *MemStore* object. <br>
        !  **Usage**: <br>
        !   --->    CALL Store%Resize()
        PROCEDURE   :: Resize       => MemStore_Resize
        !> **Type-Bound Subroutine**: Finalize <br>
        ! **Purpose**:  To perform finalization of the *MemStore* object. <br>
        !  **Usage**: <br>
        !   --->    CALL Store%Finalize()
        PROCEDURE   :: Finalize     => MemStore_Finalize
        ! ---------------------------------------------------------------------
    END TYPE MemStore
    !> The *MemHandle* type is a memory-handle type that contains information relating
    !  to the location and size of the data stored in the memory pool's storage.
    TYPE MemHandle
        PRIVATE
        !> starting position in the memory pool's storage
        tIndex  :: Position
        !> size of stored data in bytes
        tIndex  :: Size
    CONTAINS
        !----------------------------------------------------------------------
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To return the stored size in bytes handled by this handle. <br>
        !  **Usage**: <br>
        !   --->    Size = Handle%GetSize() <br>
        PROCEDURE   :: GetSize  => MemHandle_GetSize
        !----------------------------------------------------------------------
    END TYPE MemHandle
    !> The *MemoryPool* type is a general data storage type that can be used to store data
    !  of any types.  Internally, it stores the specified data as an array of bytes.
    TYPE MemoryPool
        PRIVATE
        !> the storage of the memory pool
        TYPE(MemStore)      :: Store
        !> the free block list to track the memory storage
        TYPE(FreeBlockList) :: List
        !> the initial block size
        tIndex              :: BlockSize = DefaultBlockSize
        !> the number of times that the memory storage allocates
        tSInt32             :: NumAlloc = -1
    CONTAINS
        !----------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: MemoryPool_InsertObject
        PROCEDURE, PRIVATE  :: MemoryPool_InsertByteArr
        !----------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new memory pool based on the optionally specified block size. <br>
        !  **Usage**: <br>
        !   --->    CALL Pool%Construct() <br>
        !   --->    CALL Pool%Construct(BlockSize) <br>
        PROCEDURE   :: Construct    => MemoryPool_Construct
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To destruct the memory pool. <br>
        !  **Usage**: <br>
        !   --->    CALL Pool%Destruct()
        PROCEDURE   :: Destruct     => MemoryPool_Destruct
        !> **Type-Bound Subroutine**: CloneTo <br>
        !  **Purpose**:  To perform cloning of the source pool. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcPool%CloneTo(DstPool) <br>
        PROCEDURE   :: CloneTo      => MemoryPool_Clone
        !> **Type-Bound Function**: Insert <br>
        !  **Purpose**:  To insert an object or a byte array into the memory pool. <br>
        !  **Usage**: <br>
        !   ! insert an object of any type <br>
        !   --->    Handle = Pool%Insert(Object) <br>
        !   ! insert a byte array and its size <br>
        !   --->    Handle = Pool%Insert(ByteArr, ByteSize) <br>
        GENERIC     :: Insert       => MemoryPool_InsertObject, MemoryPool_InsertByteArr
        !> **Type-Bound Function**: Retrieve <br>
        ! **Purpose**:  To get a pointer to the memory pool storage occupied by the specified
        !               memory handle. <br>
        !  **Usage**: <br>
        !   --->    DatPtr => Pool%Retrieve(Handle) <br>
        PROCEDURE   :: Retrieve     => MemoryPool_Retrieve
        !> **Type-Bound Subroutine**: Release <br>
        !  **Purpose**:  To release the memory pool storage occupied by the specified handle. <br>
        !  **Usage**: <br>
        !   --->    CALL Pool%Release(Handle)
        PROCEDURE   :: Release      => MemoryPool_Release
        !----------------------------------------------------------------------
    END TYPE MemoryPool

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!                           Routines for MemHandle
!------------------------------------------------------------------------------

PURE FUNCTION MemHandle_GetSize(Handle) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !! To return the stored size in bytes handled by this handle.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemHandle), INTENT(IN)    :: Handle   !! memory handle
    tIndex                          :: Size     !! size in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = Handle%Size

    RETURN

END FUNCTION MemHandle_GetSize

!------------------------------------------------------------------------------
!                           Routines for MemStore
!------------------------------------------------------------------------------

SUBROUTINE MemStore_Initialize(Store, BlockSize)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform initialization of the *MemStore* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemStore), INTENT(OUT)    :: Store        !! *MemStore* object
    tIndex,          INTENT(IN)     :: BlockSize    !! block size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemAlloc(Store%Storage, BlockSize)
    Store%BlockSize = BlockSize

    RETURN

END SUBROUTINE MemStore_Initialize

!******************************************************************************

SUBROUTINE MemStore_Resize(Store)

!** PURPOSE OF THIS SUBROUTINE:
    !! To resize the storage of the *MemStore* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemStore), INTENT(INOUT)  :: Store    !! *MemStore* object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemResize(Store%Storage, [Store%BlockSize+SIZE(Store%Storage, KIND=kIndex)])

    RETURN

END SUBROUTINE MemStore_Resize

!******************************************************************************

SUBROUTINE MemStore_Finalize(Store)

!** PURPOSE OF THIS SUBROUTINE:
    !! To release the storage of the *MemStore* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemStore), INTENT(INOUT)  :: Store    !! *MemStore* object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemFree(Store%Storage)

    RETURN

END SUBROUTINE MemStore_Finalize

!******************************************************************************

SUBROUTINE MemStore_Put(Store, Data, Position)

!** PURPOSE OF THIS SUBROUTINE:
    !! To store the data in the *MemStore* object at the specified position.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemStore), INTENT(INOUT)  :: Store    !! *MemStore* object
    tSInt8,          INTENT(IN)     :: Data(:)  !! data to be stored in the pool
    tIndex,          INTENT(IN)     :: Position !! starting position to store data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Store%Storage(Position:) = Data(1:)

    RETURN

END SUBROUTINE MemStore_Put

!******************************************************************************

SUBROUTINE MemStore_Get(Store, Data, Position)

!** PURPOSE OF THIS SUBROUTINE:
    !! To retrieve the data from the *MemStore* object at the specified position.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemStore), INTENT(IN)     :: Store    !! *MemStore* object
    tSInt8,          INTENT(OUT)    :: Data(:)  !! data to be stored in the pool
    tIndex,          INTENT(IN)     :: Position !! starting position to store data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Data(1:) = Store%Storage(Position:)

    RETURN

END SUBROUTINE MemStore_Get

!******************************************************************************

FUNCTION MemStore_Retrieve(Store, Position, Size) RESULT(DatPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To retrieve the data from the *MemStore* object at the specified position.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemStore), TARGET, INTENT(INOUT)  :: Store        !! *MemStore* object
    tIndex,                  INTENT(IN)     :: Position     !! starting position to stored data
    tIndex,                  INTENT(IN)     :: Size         !! size of stored data
    tSInt8,          POINTER                :: DatPtr(:)    !! pointer to data stored in the pool

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    DatPtr(1:Size) => Store%Storage(Position:)

    RETURN

END FUNCTION MemStore_Retrieve

!------------------------------------------------------------------------------
!                           Routines for MemoryPool
!------------------------------------------------------------------------------

SUBROUTINE MemoryPool_Construct(Pool, BlockSize)

!** PURPOSE OF THIS SUBROUTINE:
    !! To construct a new memory pool based on the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemoryPool), INTENT(OUT)  :: Pool         !! MemoryPool object
    tIndex, OPTIONAL,  INTENT(IN)   :: BlockSize    !! block size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Pool%BlockSize = DefaultBlockSize
    IF (PRESENT(BlockSize)) THEN
        ! limit the block size to a pre-determined range
        IF (BlockSize < MinBlockSize) THEN
            Pool%BlockSize = MinBlockSize
        ELSEIF (BlockSize > MaxBlockSize) THEN
            Pool%BlockSize = MaxBlockSize
        ELSE
            Pool%BlockSize = BlockSize
        END IF
    END IF
    Pool%NumAlloc  = 0
    IF (PRESENT(BlockSize)) Pool%BlockSize = BlockSize
    CALL Pool%Store%Initialize(Pool%BlockSize)
    CALL Pool%List%Construct(Pool%BlockSize)

    RETURN

END SUBROUTINE MemoryPool_Construct

!******************************************************************************

SUBROUTINE MemoryPool_Destruct(Pool)

!** PURPOSE OF THIS SUBROUTINE:
    !! To destruct the memory pool.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemoryPool), INTENT(INOUT)    :: Pool !! MemoryPool object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Pool%Store%Finalize()
    CALL Pool%List%Destruct()
    Pool%BlockSize = DefaultBlockSize
    Pool%NumAlloc  = -1

    RETURN

END SUBROUTINE MemoryPool_Destruct

!******************************************************************************

SUBROUTINE MemoryPool_Clone(SrcPool, DstPool)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform cloning of the source pool.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemoryPool), INTENT(IN)    :: SrcPool  !! source
    TYPE(MemoryPool),  INTENT(OUT)   :: DstPool  !! destination

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! NA

! FLOW

    CALL SrcPool%List%CloneTo(DstPool%List)
    DstPool%Store     = SrcPool%Store
    DstPool%BlockSize = SrcPool%BlockSize
    DstPool%NumAlloc  = SrcPool%NumAlloc

    RETURN

END SUBROUTINE MemoryPool_Clone

!******************************************************************************

FUNCTION MemoryPool_InsertObject(Pool, Obj) RESULT(Handle)

!** PURPOSE OF THIS SUBROUTINE:
    !! To insert the specified object into the pool.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemoryPool), INTENT(INOUT)    :: Pool     !! MemoryPool object
    CLASS(*),          INTENT(IN)       :: Obj      !! object to be stored in the pool
    TYPE(MemHandle)                     :: Handle   !! memory handle

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, PARAMETER   :: BitSize_Int8 = STORAGE_SIZE(1_kInt8, KIND=kIndex)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: ByteSize
    tSInt8, POINTER :: ByteArr(:)

! FLOW
    
    ByteSize = STORAGE_SIZE(Obj, KIND=kIndex)/BitSize_Int8
    CALL AnyType_2_ByteArrPtr(Obj, ByteSize, ByteArr)
    Handle = Pool%Insert(ByteArr, ByteSize)

    RETURN

END FUNCTION MemoryPool_InsertObject

!******************************************************************************

FUNCTION MemoryPool_InsertByteArr(Pool, ByteArr, Size) RESULT(Handle)

!** PURPOSE OF THIS SUBROUTINE:
    !! To insert the specified byte array into the pool.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemoryPool), INTENT(INOUT)    :: Pool         !! MemoryPool object
    tSInt8,            INTENT(IN)       :: ByteArr(:)   !! byte array to be stored in the pool
    tIndex,            INTENT(IN)       :: Size         !! size of byte array
    TYPE(MemHandle)                     :: Handle       !! memory handle

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Position, Dummy

! FLOW

    ! check if the memory pool has been constructed yet
    IF (Pool%NumAlloc == -1) CALL Pool%Construct()

    ASSOCIATE (Store => Pool%Store, List => Pool%List)
        ! find the position to insert the data
        Position = List%FindPosition(Size)
        ! if no best block fit the data, need to resize the pool storage
        DO WHILE (Position == 0_kIndex)
            ! resize the pool storage and store the data again
            CALL Store%Resize()
            Pool%NumAlloc = Pool%NumAlloc + 1
            ! new free block need to be insert to free-block list
            BLOCK
                TYPE(FreeBlockNode), POINTER    :: FreeBlock
                FreeBlock => List%GetNewNode()
                CALL FreeBlock%SetNew(Pool%BlockSize, Pool%NumAlloc*Pool%BlockSize+1_kIndex)
                CALL List%Insert(FreeBlock)
                ! update the list: block size and block position
                Position = List%FindPosition(Size)
                NULLIFY(FreeBlock)
            END BLOCK
        END DO
        ! insert the data into the memory pool storage and update the free-block list
        CALL Store%Put(ByteArr, Position)
        Dummy = List%Update(Size)
    END ASSOCIATE
    
    ! set the memory handle
    Handle = MemHandle(Position, Size)

    RETURN

END FUNCTION MemoryPool_InsertByteArr

!******************************************************************************

SUBROUTINE MemoryPool_Release(Pool, Handle)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free the memory pool storage occupied by the specified handle.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemoryPool), INTENT(INOUT)    :: Pool     !! MemoryPool object
    TYPE(MemHandle),   INTENT(IN)       :: Handle   !! memory handle

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(FreeBlockNode), POINTER    :: FreeBlock

! FLOW
    
                
    FreeBlock => Pool%List%GetNewNode()
    CALL FreeBlock%SetNew(Handle%Size, Handle%Position)
    CALL Pool%List%Insert(FreeBlock)

    RETURN

END SUBROUTINE MemoryPool_Release

!******************************************************************************

FUNCTION MemoryPool_Retrieve(Pool, Handle) RESULT(BytePtr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get a pointer to the memory pool storage occupied by the specified memory handle.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemoryPool), INTENT(INOUT)    :: Pool         !! MemoryPool object
    TYPE(MemHandle),   INTENT(IN)       :: Handle       !! memory handle
    tSInt8,            POINTER          :: BytePtr(:)   !! pointer to data stored in the pool

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    BytePtr => Pool%Store%GetPtr(Handle%Position, Handle%Size)

    RETURN

END FUNCTION MemoryPool_Retrieve

!******************************************************************************

END MODULE MClass_MemoryPool

!******************************************************************************
