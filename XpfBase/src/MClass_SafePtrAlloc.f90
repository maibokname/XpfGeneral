
MODULE MClass_SafePtrAlloc

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SafePtrAlloc* type and its supporting data types and related routines.
!   The *SafePtrAlloc* type is a safe-pointer allocator that provides a safe way to allocate memory
!   for a 1-dimensional pointer array for any intrinsic types and any derived types in the *Object*
!   class.  It is worth noting that users may use the allocator with a pointer remapping of a rank-one
!   array to safely allocate memory for a pointer array with rank higher than one.

!** USE STATEMENTS:
    USE ISO_C_BINDING,  ONLY: C_LOC, C_F_POINTER, C_PTR, C_NULL_PTR, C_SIZEOF
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_MemHandlers
    USE MBase_ByteUtil
    USE MClass_FreeBlockList
    USE MClass_Object,  ONLY: Object, ASSIGNMENT(=)

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: SafePtrAlloc
    PUBLIC :: PtrHandle

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_SafePtrAlloc'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128
    ! Default block Size in bytes
    tIndex,    PARAMETER    :: DefaultBlockSize = 5120_kIndex
    ! Minimum block size in bytes
    tIndex,    PARAMETER    :: MinBlockSize = 1024_kIndex
    ! Minimum block size in bytes
    tIndex,    PARAMETER    :: MaxBlockSize = 10240_kIndex
    ! The number of bits used by one byte
    tIndex,    PARAMETER    :: ByteBits = STORAGE_SIZE(0_kInt8, KIND=kIndex)
    tCmpxSP,   PARAMETER    :: OneCmpxSP = (1.0_kSingle, 0.0_kSingle)
    tCmpxDP,   PARAMETER    :: OneCmpxDP = (1.0_kDouble, 0.0_kDouble)
    tCmpxQP,   PARAMETER    :: OneCmpxQP = (1.0_kQuad, 0.0_kQuad)
    ! The number of bytes used by one element for intrinsic types
    tIndex,    PARAMETER    :: Int8ByteSize      = 1_kIndex
    tIndex,    PARAMETER    :: Int16ByteSize     = STORAGE_SIZE(0_kInt16, KIND=kIndex)/ByteBits
    tIndex,    PARAMETER    :: Int32ByteSize     = STORAGE_SIZE(0_kInt32, KIND=kIndex)/ByteBits
    tIndex,    PARAMETER    :: Int64ByteSize     = STORAGE_SIZE(0_kInt64, KIND=kIndex)/ByteBits
    tIndex,    PARAMETER    :: RealSPByteSize    = STORAGE_SIZE(0.0_kSingle, KIND=kIndex)/ByteBits
    tIndex,    PARAMETER    :: RealDPByteSize    = STORAGE_SIZE(0.0_kDouble, KIND=kIndex)/ByteBits
    tIndex,    PARAMETER    :: RealQPByteSize    = STORAGE_SIZE(0.0_kQuad, KIND=kIndex)/ByteBits
    tIndex,    PARAMETER    :: CmpxSPByteSize    = STORAGE_SIZE(OneCmpxSP, KIND=kIndex)/ByteBits
    tIndex,    PARAMETER    :: CmpxDPByteSize    = STORAGE_SIZE(OneCmpxDP, KIND=kIndex)/ByteBits
    tIndex,    PARAMETER    :: CmpxQPByteSize    = STORAGE_SIZE(OneCmpxQP, KIND=kIndex)/ByteBits
    tIndex,    PARAMETER    :: LogicalByteSize   = STORAGE_SIZE(TrueVal, KIND=kIndex)/ByteBits
    tIndex,    PARAMETER    :: CharacterByteSize = STORAGE_SIZE('A', KIND=kIndex)/ByteBits

!** DERIVED TYPE DEFINITIONS
    !> The *MemPool* type is a storage type.  This is a helper and private type.
    TYPE MemPool
        !> the block size used for reallocation
        tIndex                  :: BlockSize = 0_kIndex
        !> an array representing the storage of the allocator
        tSInt8, ALLOCATABLE     :: Storage(:)
    CONTAINS
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        ! **Purpose**:  To perform construction of the *MemPool* object. <br>
        !  **Usage**: <br>
        !   --->    CALL Pool%Construct(BlockSize)
        PROCEDURE   :: Construct   => MemPool_Construct
        !> **Type-Bound Function**: GetStorage <br>
        ! **Purpose**:  To get a pointer to the storage in the memory pool according to
        !               the specified input. <br>
        !  **Usage**: <br>
        !   --->    DatPtr => Pool%GetStorage(Position, Size) <br>
        PROCEDURE   :: GetStorage   => MemPool_GetStorage
        !> **Type-Bound Subroutine**: Resize <br>
        ! **Purpose**:  To resize the storage of the *MemPool* object. <br>
        !  **Usage**: <br>
        !   --->    CALL Pool%Resize()
        PROCEDURE   :: Resize       => MemPool_Resize
        !> **Type-Bound Subroutine**: Destruct <br>
        ! **Purpose**:  To destruct the *MemPool* object. <br>
        !  **Usage**: <br>
        !   --->    CALL Pool%Destruct()
        PROCEDURE   :: Destruct     => MemPool_Destruct
        ! ---------------------------------------------------------------------
    END TYPE MemPool
    !> The *PtrHandle* type is a pointer-handle type that contains information
    !  relating to the location and size of the storage occupied by the pointer.
    TYPE PtrHandle
        PRIVATE
        !> starting position in the allocator's storage
        tIndex  :: Position = 0_kIndex
        !> size of storage occupied by the pointer in bytes
        tIndex  :: Size = 0_kIndex
    CONTAINS
        !----------------------------------------------------------------------
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To return the storage size in bytes handled by this handle. <br>
        !  **Usage**: <br>
        !   --->    Size = Handle%GetSize() <br>
        PROCEDURE   :: GetSize  => PtrHandle_GetSize
        !----------------------------------------------------------------------
    END TYPE PtrHandle
    !> The *SafePtrAlloc* type is a safe-pointer allocator that provides a safe way to
    !  allocate memory for a one-dimensional pointer array.
    TYPE SafePtrAlloc
        PRIVATE
        !> the memory storage pool
        TYPE(MemPool)   :: Pool
        !> the free block list to track the memory storage pool
        TYPE(FreeBlockList)  :: List
        !> the initial block size
        tIndex          :: BlockSize = DefaultBlockSize
        !> the number of times that the memory storage pool allocates
        tSInt32         :: NumAlloc = -1
    CONTAINS
        !----------------------------------------------------------------------
        !> **Type-Bound Function**: Reserve <br>
        !  **Purpose**:  To reserve the storage according to the specified size. <br>
        !  **Usage**: <br>
        !   --->    Handle = Allocator%Reserve(ByteSize) <br>
        PROCEDURE, PRIVATE  :: Reserve      => SafePtrAlloc_Reserve
        !> **Type-Bound Function**: Retrieve <br>
        ! **Purpose**:  To get a pointer to the storage according to the specified handle. <br>
        !  **Usage**: <br>
        !   --->    DatPtr => Allocator%Retrieve(Handle) <br>
        PROCEDURE, PRIVATE  :: Retrieve     => SafePtrAlloc_Retrieve
        !> **Type-Bound Subroutine**: Release <br>
        !  **Purpose**:  To release the storage according to the specified handle. <br>
        !  **Usage**: <br>
        !   --->    CALL Allocator%Release(Handle)
        PROCEDURE, PRIVATE  :: Release     => SafePtrAlloc_Release
        !----------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemFree_Character
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemAlloc_Character
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemResize_Character
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemFree_Int8
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemAlloc_Int8
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemResize_Int8
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemFree_Int16
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemAlloc_Int16
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemResize_Int16
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemFree_Int32
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemAlloc_Int32
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemResize_Int32
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemFree_Int64
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemAlloc_Int64
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemResize_Int64
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemFree_RealSP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemAlloc_RealSP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemResize_RealSP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemFree_RealDP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemAlloc_RealDP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemResize_RealDP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemFree_RealQP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemAlloc_RealQP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemResize_RealQP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemFree_CmpxSP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemAlloc_CmpxSP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemResize_CmpxSP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemFree_CmpxDP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemAlloc_CmpxDP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemResize_CmpxDP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemFree_CmpxQP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemAlloc_CmpxQP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemResize_CmpxQP
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemFree_Logical
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemAlloc_Logical
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemResize_Logical
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemFree_Object
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemAlloc_Object
        PROCEDURE, PRIVATE  :: SafePtrAlloc_MemResize_Object
        !----------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new allocator based on the optionally specified block size. <br>
        !  **Usage**: <br>
        !   --->    CALL Allocator%Construct() <br>
        !   --->    CALL Allocator%Construct(BlockSize) <br>
        PROCEDURE   :: Construct    => SafePtrAlloc_Construct
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To destruct the allocator. <br>
        !  **Usage**: <br>
        !   --->    CALL Allocator%Destruct()
        PROCEDURE   :: Destruct     => SafePtrAlloc_Destruct
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**:  To release the storage according to the specified handle and the pointer. <br>
        !  **Usage**: <br>
        !   --->    CALL Allocator%MemFree(Handle, AnyPtr)
        GENERIC     :: MemFree      => SafePtrAlloc_MemFree_Character,  &
                                       SafePtrAlloc_MemFree_Int8,       &
                                       SafePtrAlloc_MemFree_Int16,      &
                                       SafePtrAlloc_MemFree_Int32,      &
                                       SafePtrAlloc_MemFree_Int64,      &
                                       SafePtrAlloc_MemFree_RealSP,     &
                                       SafePtrAlloc_MemFree_RealDP,     &
                                       SafePtrAlloc_MemFree_RealQP,     &
                                       SafePtrAlloc_MemFree_CmpxSP,     &
                                       SafePtrAlloc_MemFree_CmpxDP,     &
                                       SafePtrAlloc_MemFree_CmpxQP,     &
                                       SafePtrAlloc_MemFree_Logical,    &
                                       SafePtrAlloc_MemFree_Object
        !> **Type-Bound Function**: MemAlloc <br>
        !  **Purpose**:  To allocate the storage for the specified pointer array. <br>
        !  **Usage**: <br>
        !   ! for any intrinsic types except the CHARACTER type <br>
        !   --->    Handle = Allocator%MemAlloc(AnyPtr, Size) <br>
        !   --->    Handle = Allocator%MemAlloc(AnyPtr, Size, StartID) <br>
        !   ! for the CHARACTER type <br>
        !   --->    Handle = Allocator%MemAlloc(CharLen, CharPtr, Size) <br>
        !   --->    Handle = Allocator%MemAlloc(CharLen, CharPtr, Size, StartID) <br>
        !   ! for any derived types in the *Object* class <br>
        !   --->    Handle = Allocator%MemAlloc(ObjMold, SetObjPtr, ObjPtr, Size) <br>
        !   --->    Handle = Allocator%MemAlloc(ObjMold, SetObjPtr, ObjPtr, Size, StartID) <br>
        GENERIC     :: MemAlloc     => SafePtrAlloc_MemAlloc_Character,     &
                                       SafePtrAlloc_MemAlloc_Int8,          &
                                       SafePtrAlloc_MemAlloc_Int16,         &
                                       SafePtrAlloc_MemAlloc_Int32,         &
                                       SafePtrAlloc_MemAlloc_Int64,         &
                                       SafePtrAlloc_MemAlloc_RealSP,        &
                                       SafePtrAlloc_MemAlloc_RealDP,        &
                                       SafePtrAlloc_MemAlloc_RealQP,        &
                                       SafePtrAlloc_MemAlloc_CmpxSP,        &
                                       SafePtrAlloc_MemAlloc_CmpxDP,        &
                                       SafePtrAlloc_MemAlloc_CmpxQP,        &
                                       SafePtrAlloc_MemAlloc_Logical,       &
                                       SafePtrAlloc_MemAlloc_Object
        !> **Type-Bound Subroutine**: MemResize <br>
        !  **Purpose**:  To resize the storage of the specified pointer array. <br>
        !  **Usage**: <br>
        !   ! for any intrinsic types <br>
        !   --->    CALL Allocator%MemResize(Handle, AnyPtr, NewSize) <br>
        !   ! for any derived types in the *Object* class <br>
        !   --->    CALL Allocator%MemResize(ObjMold, SetObjPtr, Handle, ObjPtr, NewSize) <br>
        GENERIC     :: MemResize    => SafePtrAlloc_MemResize_Character,    &
                                       SafePtrAlloc_MemResize_Int8,         &
                                       SafePtrAlloc_MemResize_Int16,        &
                                       SafePtrAlloc_MemResize_Int32,        &
                                       SafePtrAlloc_MemResize_Int64,        &
                                       SafePtrAlloc_MemResize_RealSP,       &
                                       SafePtrAlloc_MemResize_RealDP,       &
                                       SafePtrAlloc_MemResize_RealQP,       &
                                       SafePtrAlloc_MemResize_CmpxSP,       &
                                       SafePtrAlloc_MemResize_CmpxDP,       &
                                       SafePtrAlloc_MemResize_CmpxQP,       &
                                       SafePtrAlloc_MemResize_Logical,      &
                                       SafePtrAlloc_MemResize_Object
        !----------------------------------------------------------------------
    END TYPE SafePtrAlloc

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> To set the pointer array in the Object class to the specified byte array.
        SUBROUTINE IfaceSetObjPtr(ByteArr, ObjPtr, ObjSize)
            IMPORT
            tSInt8,        TARGET,  INTENT(IN)  :: ByteArr(:)
            CLASS(Object), POINTER, INTENT(OUT) :: ObjPtr(:)
            tIndex,                 INTENT(IN)  :: ObjSize
        END SUBROUTINE IfaceSetObjPtr
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!                           Routines for PtrHandle
!------------------------------------------------------------------------------

FUNCTION PtrHandle_GetSize(Handle) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !! To return the storage size in bytes handled by this handle.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PtrHandle), INTENT(IN)    :: Handle   !! pointer handle
    tIndex                          :: Size     !! size in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = Handle%Size

    RETURN

END FUNCTION PtrHandle_GetSize

!------------------------------------------------------------------------------
!                           Routines for MemPool
!------------------------------------------------------------------------------

SUBROUTINE MemPool_Construct(Pool, BlockSize)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform initialization of the *MemPool* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemPool), INTENT(OUT) :: Pool         !! *MemPool* object
    tIndex,         INTENT(IN)  :: BlockSize    !! block size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemAlloc(Pool%Storage, BlockSize)
    Pool%BlockSize = BlockSize

    RETURN

END SUBROUTINE MemPool_Construct

!******************************************************************************

SUBROUTINE MemPool_Resize(Pool)

!** PURPOSE OF THIS SUBROUTINE:
    !! To resize the storage of the *MemPool* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemPool), INTENT(INOUT)   :: Pool !! *MemPool* object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemResize(Pool%Storage, [Pool%BlockSize+SIZE(Pool%Storage, KIND=kIndex)])

    RETURN

END SUBROUTINE MemPool_Resize

!******************************************************************************

SUBROUTINE MemPool_Destruct(Pool)

!** PURPOSE OF THIS SUBROUTINE:
    !! To release the storage of the *MemPool* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemPool), INTENT(INOUT)   :: Pool !! *MemPool* object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemFree(Pool%Storage)

    RETURN

END SUBROUTINE MemPool_Destruct

!******************************************************************************

FUNCTION MemPool_GetStorage(Pool, Position, Size) RESULT(DatPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a pointer to the storage in the memory pool according to the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MemPool), TARGET, INTENT(INOUT)   :: Pool         !! *MemPool* object
    tIndex,                 INTENT(IN)      :: Position     !! starting position to storage
    tIndex,                 INTENT(IN)      :: Size         !! size of requested data in bytes
    tSInt8,         POINTER                 :: DatPtr(:)    !! pointer to the storage in the pool

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    DatPtr(1:Size) => Pool%Storage(Position:)

    RETURN

END FUNCTION MemPool_GetStorage

!------------------------------------------------------------------------------
!                           Routines for SafePtrAlloc
!------------------------------------------------------------------------------

SUBROUTINE SafePtrAlloc_Construct(Allocator, BlockSize)

!** PURPOSE OF THIS SUBROUTINE:
    !! To construct a new allocator based on the optionally specified block size.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SafePtrAlloc), INTENT(OUT)    :: Allocator    !! *SafePtrAlloc* object
    tIndex, OPTIONAL,    INTENT(IN)     :: BlockSize    !! block size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Allocator%BlockSize = DefaultBlockSize
    IF (PRESENT(BlockSize)) THEN
        ! limit the block size to a pre-determined range
        IF (BlockSize < MinBlockSize) THEN
            Allocator%BlockSize = MinBlockSize
        ELSEIF (BlockSize > MaxBlockSize) THEN
            Allocator%BlockSize = MaxBlockSize
        ELSE
            Allocator%BlockSize = BlockSize
        END IF
    END IF
    Allocator%NumAlloc  = 0
    CALL Allocator%Pool%Construct(Allocator%BlockSize)
    CALL Allocator%List%Construct(Allocator%BlockSize)

    RETURN

END SUBROUTINE SafePtrAlloc_Construct

!******************************************************************************

SUBROUTINE SafePtrAlloc_Destruct(Allocator)

!** PURPOSE OF THIS SUBROUTINE:
    !! To destruct the allocator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SafePtrAlloc), INTENT(INOUT)  :: Allocator   !! *SafePtrAlloc* object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Allocator%Pool%Destruct()
    CALL Allocator%List%Destruct()
    Allocator%BlockSize = DefaultBlockSize
    Allocator%NumAlloc  = -1

    RETURN

END SUBROUTINE SafePtrAlloc_Destruct

!******************************************************************************

FUNCTION SafePtrAlloc_Reserve(Allocator, Size) RESULT(Handle)

!** PURPOSE OF THIS SUBROUTINE:
    !! To reserve the storage according to the specified size.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SafePtrAlloc), INTENT(INOUT)  :: Allocator    !! *SafePtrAlloc* object
    tIndex,              INTENT(IN)     :: Size         !! size of byte array
    TYPE(PtrHandle)                     :: Handle       !! pointer handle

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Position, Dummy

! FLOW

    ASSOCIATE (Pool => Allocator%Pool, List => Allocator%List)
        ! find the position to insert the data
        Position = List%FindPosition(Size)
        ! if no best block fit the data, need to resize the pool storage
        DO WHILE (Position == 0_kIndex)
            ! resize the pool storage and store the data again
            CALL Pool%Resize()
            Allocator%NumAlloc = Allocator%NumAlloc + 1
            ! new free block need to be insert to free-block list
            BLOCK
                TYPE(FreeBlockNode), POINTER    :: FreeBlock
                FreeBlock => List%GetNewNode()
                CALL FreeBlock%SetNew(Allocator%BlockSize, Allocator%NumAlloc*Allocator%BlockSize+1_kIndex)
                CALL List%Insert(FreeBlock)
                ! update the list: block size and block position
                Position = List%FindPosition(Size)
                NULLIFY(FreeBlock)
            END BLOCK
        END DO
        ! update the free-block list
        Dummy = List%Update(Size)
    END ASSOCIATE
    
    ! set the pointer handle
    Handle = PtrHandle(Position, Size)

    RETURN

END FUNCTION SafePtrAlloc_Reserve

!******************************************************************************

SUBROUTINE SafePtrAlloc_Release(Allocator, Handle)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free the storage occupied by the specified handle.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SafePtrAlloc), INTENT(INOUT)  :: Allocator    !! *SafePtrAlloc* object
    TYPE(PtrHandle),     INTENT(IN)     :: Handle       !! pointer handle

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(FreeBlockNode), POINTER    :: FreeBlock

! FLOW
    
                
    FreeBlock => Allocator%List%GetNewNode()
    CALL FreeBlock%SetNew(Handle%Size, Handle%Position)
    CALL Allocator%List%Insert(FreeBlock)

    RETURN

END SUBROUTINE SafePtrAlloc_Release

!******************************************************************************

FUNCTION SafePtrAlloc_Retrieve(Allocator, Handle) RESULT(BytePtr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get a pointer to the storage according to the specified handle.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SafePtrAlloc), INTENT(INOUT)  :: Allocator    !! *SafePtrAlloc* object
    TYPE(PtrHandle),     INTENT(IN)     :: Handle       !! pointer handle
    tSInt8,              POINTER        :: BytePtr(:)   !! pointer to data stored in the pool

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    BytePtr => Allocator%Pool%GetStorage(Handle%Position, Handle%Size)

    RETURN

END FUNCTION SafePtrAlloc_Retrieve

!******************************************************************************

SUBROUTINE SafePtrAlloc_MemFree_Character(Allocator, Handle, DatPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free the storage occupied by the specified handle and pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SafePtrAlloc),  INTENT(INOUT) :: Allocator    !! *SafePtrAlloc* object
    TYPE(PtrHandle),      INTENT(IN)    :: Handle       !! pointer handle; must match the pointer
    tCharLen(:), POINTER, INTENT(INOUT) :: DatPtr(:)    !! A 1-D pointer array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Handle%Position == 0_kIndex) RETURN
    CALL Allocator%Release(Handle)
    NULLIFY(DatPtr)

    RETURN

END SUBROUTINE SafePtrAlloc_MemFree_Character

!******************************************************************************

FUNCTION SafePtrAlloc_MemAlloc_Character(Allocator, CharLen, DatPtr, Size, StartID) RESULT(Handle)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get the storage requested by the specified pointer array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SafePtrAlloc),  INTENT(INOUT) :: Allocator    !! *SafePtrAlloc* object
    tIndex,               INTENT(IN)    :: CharLen      !! length of the character string
    tCharLen(:), POINTER, INTENT(OUT)   :: DatPtr(:)    !! A 1-D pointer array
    tIndex,               INTENT(IN)    :: Size         !! size of the array requested; must be positive
    tIndex,  OPTIONAL,    INTENT(IN)    :: StartID      !! starting index of A
    TYPE(PtrHandle)                     :: Handle       !! pointer handle

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(C_PTR)                 :: cPtr
    tCharLen(CharLen), POINTER  :: fPtr(:)
    tSInt8,            POINTER  :: BytePtr(:)
    tIndex                      :: DatSize, ByteSize
    tIndex                      :: BeginID

! FLOW

    ! check if the allocator has been constructed yet
    IF (Allocator%NumAlloc == -1) CALL Allocator%Construct()

    ! check size and starting index
    DatSize = Size
    IF (DatSize < 1_kIndex) DatSize = 1_kIndex
    BeginID = 1_kIndex
    IF (PRESENT(StartID)) BeginID = StartID
    
    ! get byte size and reserve the storage
    ByteSize = CharacterByteSize*DatSize*CharLen
    Handle   = Allocator%Reserve(ByteSize)
    
    ! get the pointer to the storage
    BytePtr => Allocator%Retrieve(Handle)
    cPtr = C_LOC(BytePtr)
    CALL C_F_POINTER(cPtr, fPtr, [DatSize])
    DatPtr(BeginID:) => fPtr(1:DatSize)
    
    ! nullify working pointers
    cPtr = C_NULL_PTR
    NULLIFY(BytePtr, fPtr)

    RETURN

END FUNCTION SafePtrAlloc_MemAlloc_Character

!******************************************************************************

SUBROUTINE SafePtrAlloc_MemResize_Character(Allocator, Handle, DatPtr, NewSize)

!** PURPOSE OF THIS SUBROUTINE:
    !! To resize the storage of the specified pointer array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SafePtrAlloc),  INTENT(INOUT) :: Allocator    !! *SafePtrAlloc* object
    TYPE(PtrHandle),      INTENT(INOUT) :: Handle       !! pointer handle; must match the array
    tCharLen(:), POINTER, INTENT(INOUT) :: DatPtr(:)    !! A 1-D pointer array
    tIndex,               INTENT(IN)    :: NewSize      !! new size of the array; must be positive

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: NewPtr(:)
    tIndex                  :: StartID
    tIndex                  :: CopySize
    tIndex                  :: CharLen
    TYPE(PtrHandle)         :: NewHandle

! FLOW

    ! check validities of the specified input
    IF (.NOT.ASSOCIATED(DatPtr)) THEN
        CALL Handle_ErrLevel('SafePtrAlloc_MemResize_Character', ModName, ErrSevere, &
                             'Invalid input: Unassociated pointer is specified.')
        RETURN
    ELSEIF (Handle%Position == 0_kIndex) THEN
        CALL Handle_ErrLevel('SafePtrAlloc_MemResize_Character', ModName, ErrSevere, &
                             'Invalid input: Pointer handle with initial state is specified.')
        RETURN
    ELSEIF (NewSize < 1_kIndex) THEN
        CALL Handle_ErrLevel('SafePtrAlloc_MemResize_Character', ModName, ErrSevere, &
                             'Invalid input: New size must be positive number.')
        RETURN
    ELSE
        StartID = LBOUND(DatPtr, DIM=1, KIND=kIndex)
        CharLen = LEN(DatPtr(StartID), KIND=kIndex)
        IF (CharacterByteSize*SIZE(DatPtr, KIND=kIndex)*CharLen /= Handle%Size) THEN
            CALL Handle_ErrLevel('SafePtrAlloc_MemResize_Character', ModName, ErrSevere, &
                                 'Invalid input: Specified pointer array and handle do not match.')
            RETURN
        END IF
    END IF

    ! get new storage
    NewHandle = Allocator%MemAlloc(CharLen, NewPtr, NewSize, StartID)
    
    ! copy data to the new pointer
    CopySize = MIN(SIZE(DatPtr, KIND=kIndex), SIZE(NewPtr, KIND=kIndex))
    NewPtr(StartID:StartID+CopySize-1) = DatPtr(StartID:)
    
    ! release the storage occupied by the specified pointer
    CALL Allocator%MemFree(Handle, DatPtr)
    
    ! set the returned pointer and handle
    Handle = NewHandle
    DatPtr => NewPtr
    
    ! free local pointer
    NULLIFY(NewPtr)

    RETURN

END SUBROUTINE SafePtrAlloc_MemResize_Character

!******************************************************************************

SUBROUTINE SafePtrAlloc_MemFree_Object(Allocator, Handle, DatPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free the storage occupied by the specified handle and pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SafePtrAlloc),    INTENT(INOUT)   :: Allocator    !! *SafePtrAlloc* object
    TYPE(PtrHandle),        INTENT(IN)      :: Handle       !! pointer handle; must match the pointer
    CLASS(Object), POINTER, INTENT(INOUT)   :: DatPtr(:)    !! A 1-D pointer array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Handle%Position == 0_kIndex) RETURN
    CALL Allocator%Release(Handle)
    NULLIFY(DatPtr)

    RETURN

END SUBROUTINE SafePtrAlloc_MemFree_Object

!******************************************************************************

FUNCTION SafePtrAlloc_MemAlloc_Object(Allocator, ObjMold, SetObjPtr, DatPtr, Size, StartID) RESULT(Handle)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get the storage requested by the specified pointer array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SafePtrAlloc),    INTENT(INOUT)   :: Allocator    !! *SafePtrAlloc* object
    CLASS(Object),          INTENT(IN)      :: ObjMold
    !^ a mold for the specified pointer array; must be a concrete type in the *Object* class;
    !  also if it has pointer/allocatable components, those components must not yet be allocated.
    PROCEDURE(IfaceSetObjPtr)               :: SetObjPtr
    !^ a procedure to set a pointer array in the *Object* class to the targeted byte array
    CLASS(Object), POINTER, INTENT(OUT)     :: DatPtr(:)    !! A 1-D pointer array
    tIndex,                 INTENT(IN)      :: Size         !! size of the array requested; must be positive
    tIndex,       OPTIONAL, INTENT(IN)      :: StartID      !! starting index of A
    TYPE(PtrHandle)                         :: Handle       !! pointer handle

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(Object), POINTER  :: fPtr(:)
    tSInt8,        POINTER  :: BytePtr(:)
    tIndex                  :: DatSize, ByteSize, ObjByteSize
    tIndex                  :: BeginID

! FLOW
    
    ! check if the allocator has been constructed yet
    IF (Allocator%NumAlloc == -1) CALL Allocator%Construct()

    ! check size and starting index
    DatSize = Size
    IF (DatSize < 1_kIndex) DatSize = 1_kIndex
    BeginID = 1_kIndex
    IF (PRESENT(StartID)) BeginID = StartID
    
    ! get byte size and reserve the storage
    ObjByteSize = STORAGE_SIZE(ObjMold, KIND=kIndex)/ByteBits
    ByteSize = ObjByteSize*DatSize
    Handle   = Allocator%Reserve(ByteSize)
    
    ! get and set the pointers to the storage
    BytePtr => Allocator%Retrieve(Handle)
    CALL SetObjPtr(BytePtr, fPtr, DatSize)
    DatPtr(BeginID:) => fPtr(1:DatSize)
    
    ! nullify working pointers
    NULLIFY(BytePtr, fPtr)

    RETURN

END FUNCTION SafePtrAlloc_MemAlloc_Object

!******************************************************************************

SUBROUTINE SafePtrAlloc_MemResize_Object(Allocator, ObjMold, SetObjPtr, Handle, DatPtr, NewSize)

!** PURPOSE OF THIS SUBROUTINE:
    !! To resize the storage of the specified pointer array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SafePtrAlloc),    INTENT(INOUT)   :: Allocator    !! *SafePtrAlloc* object
    CLASS(Object),          INTENT(IN)      :: ObjMold
    !^ a mold for the specified pointer array; must be a concrete type in the *Object* class;
    !  also if it has pointer/allocatable components, those components must not yet be allocated.
    PROCEDURE(IfaceSetObjPtr)               :: SetObjPtr
    !^ a procedure to set a pointer array in the *Object* class to the targeted byte array
    TYPE(PtrHandle),        INTENT(INOUT)   :: Handle       !! pointer handle; must match the array
    CLASS(Object), POINTER, INTENT(INOUT)   :: DatPtr(:)    !! A 1-D pointer array
    tIndex,                 INTENT(IN)      :: NewSize      !! new size of the array; must be positive

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(Object), POINTER      :: NewPtr(:)
    tIndex                      :: StartID
    tIndex                      :: CopySize
    tIndex                      :: ObjByteSize
    TYPE(PtrHandle)             :: NewHandle

! FLOW

    ! check validities of the specified input
    IF (.NOT.ASSOCIATED(DatPtr)) THEN
        CALL Handle_ErrLevel('SafePtrAlloc_MemResize_Object', ModName, ErrSevere, &
                             'Invalid input: Unassociated pointer is specified.')
        RETURN
    ELSEIF (Handle%Position == 0_kIndex) THEN
        CALL Handle_ErrLevel('SafePtrAlloc_MemResize_Object', ModName, ErrSevere, &
                             'Invalid input: Pointer handle with initial state is specified.')
        RETURN
    ELSEIF (NewSize < 1_kIndex) THEN
        CALL Handle_ErrLevel('SafePtrAlloc_MemResize_Object', ModName, ErrSevere, &
                             'Invalid input: New size must be positive number.')
        RETURN
    ELSE
        ObjByteSize = STORAGE_SIZE(ObjMold, KIND=kIndex)/ByteBits
        IF (ObjByteSize*SIZE(DatPtr, KIND=kIndex) /= Handle%Size) THEN
            CALL Handle_ErrLevel('SafePtrAlloc_MemResize_Object', ModName, ErrSevere, &
                                 'Invalid input: Specified pointer array and handle do not match.')
            RETURN
        END IF
    END IF

    ! get new storage
    StartID = LBOUND(DatPtr, DIM=1, KIND=kIndex)
    NewHandle = Allocator%MemAlloc(ObjMold, SetObjPtr, NewPtr, NewSize, StartID)
    
    ! copy data to the new pointer
    CopySize = MIN(SIZE(DatPtr, KIND=kIndex), SIZE(NewPtr, KIND=kIndex))
    NewPtr(StartID:StartID+CopySize-1) = DatPtr(StartID:)
    
    ! release the storage occupied by the specified pointer
    CALL Allocator%MemFree(Handle, DatPtr)
    
    ! set the returned pointer and handle
    Handle = NewHandle
    DatPtr => NewPtr
    
    ! free local pointer
    NULLIFY(NewPtr)

    RETURN

END SUBROUTINE SafePtrAlloc_MemResize_Object

!******************************************************************************
! 8-bit integer
#define     TypeName                tSInt8
#define     OneByteSize             Int8ByteSize
#define     SafePtrAlloc_MemFree    SafePtrAlloc_MemFree_Int8
#define     SafePtrAlloc_MemAlloc   SafePtrAlloc_MemAlloc_Int8
#define     SafePtrAlloc_MemResize  SafePtrAlloc_MemResize_Int8
#include    "Includes/Generic_PtrMemHandlers.f90"
#undef      TypeName
#undef      OneByteSize
#undef      SafePtrAlloc_MemFree
#undef      SafePtrAlloc_MemAlloc
#undef      SafePtrAlloc_MemResize
!******************************************************************************
! 16-bit integer
#define     TypeName                tSInt16
#define     OneByteSize             Int16ByteSize
#define     SafePtrAlloc_MemFree    SafePtrAlloc_MemFree_Int16
#define     SafePtrAlloc_MemAlloc   SafePtrAlloc_MemAlloc_Int16
#define     SafePtrAlloc_MemResize  SafePtrAlloc_MemResize_Int16
#include    "Includes/Generic_PtrMemHandlers.f90"
#undef      TypeName
#undef      OneByteSize
#undef      SafePtrAlloc_MemFree
#undef      SafePtrAlloc_MemAlloc
#undef      SafePtrAlloc_MemResize
!******************************************************************************
! 32-bit integer
#define     TypeName                tSInt32
#define     OneByteSize             Int32ByteSize
#define     SafePtrAlloc_MemFree    SafePtrAlloc_MemFree_Int32
#define     SafePtrAlloc_MemAlloc   SafePtrAlloc_MemAlloc_Int32
#define     SafePtrAlloc_MemResize  SafePtrAlloc_MemResize_Int32
#include    "Includes/Generic_PtrMemHandlers.f90"
#undef      TypeName
#undef      OneByteSize
#undef      SafePtrAlloc_MemFree
#undef      SafePtrAlloc_MemAlloc
#undef      SafePtrAlloc_MemResize
!******************************************************************************
! 64-bit integer
#define     TypeName                tSInt64
#define     OneByteSize             Int64ByteSize
#define     SafePtrAlloc_MemFree    SafePtrAlloc_MemFree_Int64
#define     SafePtrAlloc_MemAlloc   SafePtrAlloc_MemAlloc_Int64
#define     SafePtrAlloc_MemResize  SafePtrAlloc_MemResize_Int64
#include    "Includes/Generic_PtrMemHandlers.f90"
#undef      TypeName
#undef      OneByteSize
#undef      SafePtrAlloc_MemFree
#undef      SafePtrAlloc_MemAlloc
#undef      SafePtrAlloc_MemResize
!******************************************************************************
! single-precision real
#define     TypeName                tRealSP
#define     OneByteSize             RealSPByteSize
#define     SafePtrAlloc_MemFree    SafePtrAlloc_MemFree_RealSP
#define     SafePtrAlloc_MemAlloc   SafePtrAlloc_MemAlloc_RealSP
#define     SafePtrAlloc_MemResize  SafePtrAlloc_MemResize_RealSP
#include    "Includes/Generic_PtrMemHandlers.f90"
#undef      TypeName
#undef      OneByteSize
#undef      SafePtrAlloc_MemFree
#undef      SafePtrAlloc_MemAlloc
#undef      SafePtrAlloc_MemResize
!******************************************************************************
! double-precision real
#define     TypeName                tRealDP
#define     OneByteSize             RealDPByteSize
#define     SafePtrAlloc_MemFree    SafePtrAlloc_MemFree_RealDP
#define     SafePtrAlloc_MemAlloc   SafePtrAlloc_MemAlloc_RealDP
#define     SafePtrAlloc_MemResize  SafePtrAlloc_MemResize_RealDP
#include    "Includes/Generic_PtrMemHandlers.f90"
#undef      TypeName
#undef      OneByteSize
#undef      SafePtrAlloc_MemFree
#undef      SafePtrAlloc_MemAlloc
#undef      SafePtrAlloc_MemResize
!******************************************************************************
! quadruple-precision real
#define     TypeName                tRealQP
#define     OneByteSize             RealQPByteSize
#define     SafePtrAlloc_MemFree    SafePtrAlloc_MemFree_RealQP
#define     SafePtrAlloc_MemAlloc   SafePtrAlloc_MemAlloc_RealQP
#define     SafePtrAlloc_MemResize  SafePtrAlloc_MemResize_RealQP
#include    "Includes/Generic_PtrMemHandlers.f90"
#undef      TypeName
#undef      OneByteSize
#undef      SafePtrAlloc_MemFree
#undef      SafePtrAlloc_MemAlloc
#undef      SafePtrAlloc_MemResize
!******************************************************************************
! single-precision complex
#define     TypeName                tCmpxSP
#define     OneByteSize             CmpxSPByteSize
#define     SafePtrAlloc_MemFree    SafePtrAlloc_MemFree_CmpxSP
#define     SafePtrAlloc_MemAlloc   SafePtrAlloc_MemAlloc_CmpxSP
#define     SafePtrAlloc_MemResize  SafePtrAlloc_MemResize_CmpxSP
#include    "Includes/Generic_PtrMemHandlers.f90"
#undef      TypeName
#undef      OneByteSize
#undef      SafePtrAlloc_MemFree
#undef      SafePtrAlloc_MemAlloc
#undef      SafePtrAlloc_MemResize
!******************************************************************************
! double-precision complex
#define     TypeName                tCmpxDP
#define     OneByteSize             CmpxDPByteSize
#define     SafePtrAlloc_MemFree    SafePtrAlloc_MemFree_CmpxDP
#define     SafePtrAlloc_MemAlloc   SafePtrAlloc_MemAlloc_CmpxDP
#define     SafePtrAlloc_MemResize  SafePtrAlloc_MemResize_CmpxDP
#include    "Includes/Generic_PtrMemHandlers.f90"
#undef      TypeName
#undef      OneByteSize
#undef      SafePtrAlloc_MemFree
#undef      SafePtrAlloc_MemAlloc
#undef      SafePtrAlloc_MemResize
!******************************************************************************
! quadruple-precision complex
#define     TypeName                tCmpxQP
#define     OneByteSize             CmpxQPByteSize
#define     SafePtrAlloc_MemFree    SafePtrAlloc_MemFree_CmpxQP
#define     SafePtrAlloc_MemAlloc   SafePtrAlloc_MemAlloc_CmpxQP
#define     SafePtrAlloc_MemResize  SafePtrAlloc_MemResize_CmpxQP
#include    "Includes/Generic_PtrMemHandlers.f90"
#undef      TypeName
#undef      OneByteSize
#undef      SafePtrAlloc_MemFree
#undef      SafePtrAlloc_MemAlloc
#undef      SafePtrAlloc_MemResize
!******************************************************************************
! default (32-bit) logical
#define     TypeName                tLogical
#define     OneByteSize             LogicalByteSize
#define     SafePtrAlloc_MemFree    SafePtrAlloc_MemFree_Logical
#define     SafePtrAlloc_MemAlloc   SafePtrAlloc_MemAlloc_Logical
#define     SafePtrAlloc_MemResize  SafePtrAlloc_MemResize_Logical
#include    "Includes/Generic_PtrMemHandlers.f90"
#undef      TypeName
#undef      OneByteSize
#undef      SafePtrAlloc_MemFree
#undef      SafePtrAlloc_MemAlloc
#undef      SafePtrAlloc_MemResize
!******************************************************************************

END MODULE MClass_SafePtrAlloc

!******************************************************************************
