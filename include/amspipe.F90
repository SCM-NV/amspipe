module AMSPipeModule

   use, intrinsic :: ISO_C_BINDING

   implicit none
   private


   ! ===========
   !  Utilities
   ! ===========
   !
   integer,                 parameter :: C_BOOL_INTKIND = C_SIGNED_CHAR ! whatever C integer kind matches the C bool type
   integer(C_BOOL_INTKIND), parameter :: C_BOOL_TRUE    = 1_C_BOOL_INTKIND
   integer(C_BOOL_INTKIND), parameter :: C_BOOL_FALSE   = 0_C_BOOL_INTKIND
   interface
      subroutine C_free(ptr) bind(C, name="free")
         import
         implicit none
         type(C_PTR), value, intent(in) :: ptr
      end subroutine
      pure integer(C_SIZE_T) function C_strlen(str) bind(C, name="strlen")
         import
         implicit none
         type(C_PTR), value, intent(in) :: str
      end function
   end interface


   ! ===============
   !  AMSPipeStatus
   ! ===============
   !
   enum, bind(C)
      enumerator :: AMSPIPE_STATUS_SUCCESS          = 0
      enumerator :: AMSPIPE_STATUS_DECODE_ERROR     = 1
      enumerator :: AMSPIPE_STATUS_LOGIC_ERROR      = 2
      enumerator :: AMSPIPE_STATUS_RUNTIME_ERROR    = 3
      enumerator :: AMSPIPE_STATUS_UNKNOWN_VERSION  = 4
      enumerator :: AMSPIPE_STATUS_UNKNOWN_METHOD   = 5
      enumerator :: AMSPIPE_STATUS_UNKNOWN_ARGUMENT = 6
      enumerator :: AMSPIPE_STATUS_INVALID_ARGUMENT = 7
   end enum
   public :: AMSPIPE_STATUS_SUCCESS
   public :: AMSPIPE_STATUS_DECODE_ERROR
   public :: AMSPIPE_STATUS_LOGIC_ERROR
   public :: AMSPIPE_STATUS_RUNTIME_ERROR
   public :: AMSPIPE_STATUS_UNKNOWN_VERSION
   public :: AMSPIPE_STATUS_UNKNOWN_METHOD
   public :: AMSPIPE_STATUS_UNKNOWN_ARGUMENT
   public :: AMSPIPE_STATUS_INVALID_ARGUMENT
   integer, parameter, public :: AMSPipeStatus = kind(AMSPIPE_STATUS_SUCCESS)


   ! ==============
   !  AMSPipeError
   ! ==============
   !
   type, bind(C) :: amspipe_error_t
      integer(AMSPipeStatus) :: status   = AMSPIPE_STATUS_SUCCESS ! amspipe_status_t
      type(C_PTR)            :: method   = C_NULL_PTR             ! const char*
      type(C_PTR)            :: argument = C_NULL_PTR             ! const char*
      type(C_PTR)            :: message  = C_NULL_PTR             ! const char*
   end type
   type, public :: AMSPipeError
      integer(AMSPipeStatus),    public :: status = AMSPIPE_STATUS_SUCCESS
      character(:), allocatable, public :: method
      character(:), allocatable, public :: argument
      character(:), allocatable, public :: message
   end type


   ! ================
   !  AMSPipeMessage
   ! ================
   !
   type, bind(C) :: amspipe_message_t
      type(C_PTR) :: name = C_NULL_PTR ! const char*
      type(C_PTR) :: p    = C_NULL_PTR ! AMSPipe::Message*
   end type
   type, public :: AMSPipeMessage
      type(amspipe_message_t),   private :: msg
      character(:), allocatable, public  :: name
   contains
      generic,   public  :: Delete => DeleteMessage
      procedure, private :: DeleteMessage
      final              :: FinalizeMessage
   end type
   interface
      subroutine delete_amspipe_message(message) bind(C, name="delete_amspipe_message")
         import
         implicit none
         type(amspipe_message_t) :: message ! amspipe_message_t*
      end subroutine
   end interface


   ! =====================
   !  AMSPipeSolveRequest
   ! =====================
   !
   type, bind(C) :: amspipe_solverequest_t
      type(C_PTR)             :: title           = C_NULL_PTR ! const char*
      integer(C_BOOL_INTKIND) :: quiet           = C_BOOL_FALSE
      integer(C_BOOL_INTKIND) :: gradients       = C_BOOL_FALSE
      integer(C_BOOL_INTKIND) :: stressTensor    = C_BOOL_FALSE
      integer(C_BOOL_INTKIND) :: elasticTensor   = C_BOOL_FALSE
      integer(C_BOOL_INTKIND) :: hessian         = C_BOOL_FALSE
      integer(C_BOOL_INTKIND) :: dipoleMoment    = C_BOOL_FALSE
      integer(C_BOOL_INTKIND) :: dipoleGradients = C_BOOL_FALSE
      integer(C_BOOL_INTKIND) :: other           = C_BOOL_FALSE
   end type
   type, public :: AMSPipeSolveRequest
      character(:), allocatable, public :: title
      logical,                   public :: quiet           = .false.
      logical,                   public :: gradients       = .false.
      logical,                   public :: stressTensor    = .false.
      logical,                   public :: elasticTensor   = .false.
      logical,                   public :: hessian         = .false.
      logical,                   public :: dipoleMoment    = .false.
      logical,                   public :: dipoleGradients = .false.
      logical,                   public :: other           = .false.
   end type
   interface
      subroutine delete_amspipe_solverequest(request) bind(C, name="delete_amspipe_solverequest")
         import
         implicit none
         type(amspipe_solverequest_t) :: request ! amspipe_solverequest_t*
      end subroutine
   end interface


   ! ================
   !  AMSPipeResults
   ! ================
   !
   type, bind(C) :: amspipe_results_t
      integer(C_INT64_T) :: numMessages            = 0
      type(C_PTR)        :: messages               = C_NULL_PTR ! char**
      real(C_DOUBLE)     :: energy                 = 0.0_C_DOUBLE
      type(C_PTR)        :: gradients              = C_NULL_PTR ! double *
      integer(C_INT32_T) :: gradients_dim(2)       = [ 0, 0 ]
      type(C_PTR)        :: stressTensor           = C_NULL_PTR ! double *
      integer(C_INT32_T) :: stressTensor_dim(2)    = [ 0, 0 ]
      type(C_PTR)        :: elasticTensor          = C_NULL_PTR ! double *
      integer(C_INT32_T) :: elasticTensor_dim(2)   = [ 0, 0 ]
      type(C_PTR)        :: hessian                = C_NULL_PTR ! double *
      integer(C_INT32_T) :: hessian_dim(2)         = [ 0, 0 ]
      type(C_PTR)        :: dipoleMoment           = C_NULL_PTR ! double *
      integer(C_INT32_T) :: dipoleMoment_dim(2)    = [ 0, 0 ]
      type(C_PTR)        :: dipoleGradients        = C_NULL_PTR ! double *
      integer(C_INT32_T) :: dipoleGradients_dim(2) = [ 0, 0 ]
   end type
   type, public :: AMSPipeResults
      ! Use Fortran string arrayf for messages ...
      character(:), allocatable :: messages(:)
      ! ... rest is the same as above:
      real(C_DOUBLE)     :: energy                 = 0.0_C_DOUBLE
      type(C_PTR)        :: gradients              = C_NULL_PTR ! double *
      integer(C_INT32_T) :: gradients_dim(2)       = [ 0, 0 ]
      type(C_PTR)        :: stressTensor           = C_NULL_PTR ! double *
      integer(C_INT32_T) :: stressTensor_dim(2)    = [ 0, 0 ]
      type(C_PTR)        :: elasticTensor          = C_NULL_PTR ! double *
      integer(C_INT32_T) :: elasticTensor_dim(2)   = [ 0, 0 ]
      type(C_PTR)        :: hessian                = C_NULL_PTR ! double *
      integer(C_INT32_T) :: hessian_dim(2)         = [ 0, 0 ]
      type(C_PTR)        :: dipoleMoment           = C_NULL_PTR ! double *
      integer(C_INT32_T) :: dipoleMoment_dim(2)    = [ 0, 0 ]
      type(C_PTR)        :: dipoleGradients        = C_NULL_PTR ! double *
      integer(C_INT32_T) :: dipoleGradients_dim(2) = [ 0, 0 ]
   end type


   ! =============
   !  AMSPipe
   ! =============
   !
   type, bind(C) :: amspipe_t
      type(C_PTR) :: p = C_NULL_PTR ! AMSPipe*
   end type
   type, public :: AMSPipe
      type(amspipe_t), private :: pipe
   contains
      generic,   public  :: New => NewAMSPipe
      procedure, private :: NewAMSPipe
      generic,   public  :: Delete => DeleteAMSPipe
      procedure, private :: DeleteAMSPipe
      final              :: FinalizeAMSPipe
      procedure, public  :: Receive
      procedure, public  :: Extract_Hello
      procedure, public  :: Extract_SetSystem
      procedure, public  :: Extract_SetCoords
      procedure, public  :: Extract_SetLattice
      procedure, public  :: Extract_Solve
      procedure, public  :: Extract_DeleteResults
      procedure, public  :: Send_return
      procedure, public  :: Send_results
   end type
   interface
      type(amspipe_t) function new_amspipe(call_filename, reply_filename) bind(C, name="new_amspipe")
         import
         implicit none
         character(C_CHAR), intent(in) :: call_filename(*), reply_filename(*)
      end function
      subroutine delete_amspipe(pipe) bind(C, name="delete_amspipe")
         import
         implicit none
         type(C_PTR), value :: pipe ! amspipe_t*
      end subroutine
      subroutine amspipe_receive(pipe, message) bind(C, name="amspipe_receive")
         import
         implicit none
         type(amspipe_t), value :: pipe    ! amspipe_t
         type(C_PTR),     value :: message ! amspipe_message_t*
      end subroutine
      subroutine amspipe_extract_Hello(pipe, message, error, version) bind(C, name="amspipe_extract_Hello")
         import
         implicit none
         type(amspipe_t),         value         :: pipe    ! amspipe_t
         type(amspipe_message_t), value         :: message ! amspipe_message_t
         type(C_PTR),             intent(inout) :: error   ! amspipe_error_t**
         integer(C_INT64_T),      intent(out)   :: version ! int64_t*
      end subroutine
      subroutine amspipe_extract_SetSystem(pipe, message, error, numAtoms, atomSymbols, &
                                           coords, numLatVecs, latticeVectors, totalCharge, &
                                           numBonds, bonds, bondOrders, atomicInfo) &
         bind(C, name="amspipe_extract_SetSystem")
         import
         implicit none
         type(amspipe_t),         value         :: pipe           ! amspipe_t
         type(amspipe_message_t), value         :: message        ! amspipe_message_t
         type(C_PTR),             intent(inout) :: error          ! amspipe_error_t**
         integer(C_INT64_T),      intent(out)   :: numAtoms       ! int64_t*
         type(C_PTR),             intent(inout) :: atomSymbols    ! char***
         type(C_PTR),             intent(inout) :: coords         ! double**
         integer(C_INT64_T),      intent(out)   :: numLatVecs     ! int64_t*
         type(C_PTR),             intent(inout) :: latticeVectors ! double**
         real(C_DOUBLE),          intent(out)   :: totalCharge    ! double*
         integer(C_INT64_T),      intent(out)   :: numBonds       ! int64_t*
         type(C_PTR),             intent(inout) :: bonds          ! int64_t**
         type(C_PTR),             intent(inout) :: bondOrders     ! double**
         type(C_PTR),             intent(inout) :: atomicInfo     ! char***
      end subroutine
      subroutine amspipe_extract_SetCoords(pipe, message, error, coords) bind(C, name="amspipe_extract_SetCoords")
         import
         implicit none
         type(amspipe_t),         value         :: pipe    ! amspipe_t
         type(amspipe_message_t), value         :: message ! amspipe_message_t
         type(C_PTR),             intent(inout) :: error   ! amspipe_error_t**
         type(C_PTR),             value         :: coords  ! double*
      end subroutine
      subroutine amspipe_extract_SetLattice(pipe, message, error, numLatVecs, latticeVectors) &
         bind(C, name="amspipe_extract_SetLattice")
         import
         implicit none
         type(amspipe_t),         value         :: pipe           ! amspipe_t
         type(amspipe_message_t), value         :: message        ! amspipe_message_t
         type(C_PTR),             intent(inout) :: error          ! amspipe_error_t**
         integer(C_INT64_T),      intent(out)   :: numLatVecs     ! int64_t*
         type(C_PTR),             intent(inout) :: latticeVectors ! double**
      end subroutine
      subroutine amspipe_extract_Solve(pipe, message, error, request, keepResults, prevTitle) &
         bind(C, name="amspipe_extract_Solve")
         import
         implicit none
         type(amspipe_t),              value         :: pipe        ! amspipe_t
         type(amspipe_message_t),      value         :: message     ! amspipe_message_t
         type(C_PTR),                  intent(inout) :: error       ! amspipe_error_t**
         type(amspipe_solverequest_t), intent(out)   :: request     ! amspipe_solverequest_t*
         integer(C_BOOL_INTKIND),      intent(out)   :: keepResults ! bool*
         type(C_PTR),                  intent(inout) :: prevTitle   ! char**
      end subroutine
      subroutine amspipe_extract_DeleteResults(pipe, message, error, title) bind(C, name="amspipe_extract_DeleteResults")
         import
         implicit none
         type(amspipe_t),         value         :: pipe    ! amspipe_t
         type(amspipe_message_t), value         :: message ! amspipe_message_t
         type(C_PTR),             intent(inout) :: error   ! amspipe_error_t**
         type(C_PTR),             intent(inout) :: title   ! char**
      end subroutine
      subroutine amspipe_send_return(pipe, status, method, argument, message) bind(C, name="amspipe_send_return")
         import
         implicit none
         type(amspipe_t),        value :: pipe
         integer(AMSPipeStatus), value :: status
         character(C_CHAR), intent(in) :: method(*)
         character(C_CHAR), intent(in) :: argument(*)
         character(C_CHAR), intent(in) :: message(*)
      end subroutine
      subroutine amspipe_send_results(pipe, results) bind(C, name="amspipe_send_results")
         import
         implicit none
         type(amspipe_t),        value :: pipe
         type(amspipe_results_t)       :: results
      end subroutine
   end interface


contains


! ===== Utilities ================================================================================================================


   function C_F_string(cstr) result(fstr)
      type(C_PTR), intent(in)   :: cstr
      character(:), allocatable :: fstr
      if (C_associated(cstr)) then
         fstr = C_F_string_inner(cstr)
      else
         fstr = ""
      endif
   end function
   !
   function C_F_string_inner(cstr) result(fstr)
      type(C_PTR), intent(in)   :: cstr
      character(:), allocatable :: fstr
      character(kind=C_CHAR, len=C_strlen(cstr)), pointer :: tmp
      call C_F_POINTER(cstr, tmp)
      fstr = tmp
   end function


   logical function C_F_error(errCptr, error) result(errorOccurred)
      type(C_PTR),        intent(in)               :: errCptr
      type(AMSPipeError), intent(out), allocatable :: error

      type(amspipe_error_t), pointer :: err

      errorOccurred = C_ASSOCIATED(errCptr)
      if (errorOccurred) then
         call C_F_POINTER(errCptr, err)
         allocate(error)
         error%status   = err%status
         error%method   = C_F_string(err%method)
         error%argument = C_F_string(err%argument)
         error%message  = C_F_string(err%message)
      endif

   end function


   !pure integer(C_BOOL_INTKIND) function F_C_bool(l) result(b)
   !   logical, intent(in) :: l
   !   if (l) then
   !      b = C_BOOL_TRUE
   !   else
   !      b = C_BOOL_FALSE
   !   endif
   !end function


   pure logical function C_F_bool(b) result(l)
      integer(C_BOOL_INTKIND), intent(in) :: b
      if (b == C_BOOL_TRUE) then
         l = .true.
      else
         l = .false.
      endif
   end function


! ===== AMSPipeMessage ===========================================================================================================


   impure elemental subroutine DeleteMessage(self)
      class(AMSPipeMessage), intent(inout), target :: self
      call delete_amspipe_message(self%msg)
   end subroutine
   !
   impure elemental subroutine FinalizeMessage(self)
      type(AMSPipeMessage), intent(inout) :: self
      call self%Delete()
   end subroutine


! ===== AMSPipe ==============================================================================================================


   subroutine NewAMSPipe(self, call_filename, reply_filename)
      class(AMSPipe), intent(out) :: self
      character(*),   intent(in)  :: call_filename
      character(*),   intent(in)  :: reply_filename
      self%pipe = new_amspipe(call_filename//C_NULL_CHAR, reply_filename//C_NULL_CHAR)
   end subroutine


   impure elemental subroutine DeleteAMSPipe(self)
      class(AMSPipe), intent(inout), target :: self
      call delete_amspipe(C_LOC(self%pipe))
   end subroutine
   !
   impure elemental subroutine FinalizeAMSPipe(self)
      type(AMSPipe), intent(inout) :: self
      call self%Delete()
   end subroutine


! ===== call pipe ==============================================================================================================

   subroutine Receive(self, message)
      class(AMSPipe),       intent(in)          :: self
      type(AMSPipeMessage), intent(out), target :: message

      call amspipe_receive(self%pipe, C_LOC(message%msg))
      message%name = C_F_string(message%msg%name)

   end subroutine


   subroutine Extract_Hello(self, message, error, version)
      class(AMSPipe),                  intent(in)  :: self
      type(AMSPipeMessage),            intent(in)  :: message
      type(AMSPipeError), allocatable, intent(out) :: error
      integer(C_INT64_T),              intent(out) :: version

      type(C_PTR) :: errCptr = C_NULL_PTR
      call amspipe_extract_Hello(self%pipe, message%msg, errCptr, version)
      if (C_F_error(errCptr, error)) return

   end subroutine


   subroutine Extract_SetSystem(self, message, error, atomSymbols, coords, latticeVectors, totalCharge, &
                                bonds, bondOrders, atomicInfo)
      class(AMSPipe),                  intent(in)  :: self
      type(AMSPipeMessage),            intent(in)  :: message
      type(AMSPipeError), allocatable, intent(out) :: error
      character(:),       allocatable, intent(out) :: atomSymbols(:)
      real(C_DOUBLE),     allocatable, intent(out) :: coords(:,:)
      real(C_DOUBLE),     allocatable, intent(out) :: latticeVectors(:,:)
      real(C_DOUBLE),                  intent(out) :: totalCharge
      integer(C_INT64_T), allocatable, intent(out) :: bonds(:,:)
      real(C_DOUBLE),     allocatable, intent(out) :: bondOrders(:)
      character(:),       allocatable, intent(out) :: atomicInfo(:)

      integer(C_INT64_T) :: iAtom, numAtoms, numLatVecs, numBonds
      type(C_PTR) :: errCptr = C_NULL_PTR, atSymsCptr = C_NULL_PTR, crdsCptr = C_NULL_PTR, latVecsCPtr = C_NULL_PTR, &
                     bndsCptr = C_NULL_PTR, bndOrdsCptr = C_NULL_PTR, atInfCptr = C_NULL_PTR
      type(C_PTR),        pointer :: atSymsFptr(:)    => null()
      real(C_DOUBLE),     pointer :: crdsFptr(:,:)    => null()
      real(C_DOUBLE),     pointer :: latVecsFptr(:,:) => null()
      integer(C_INT64_T), pointer :: bndsFptr(:,:)    => null()
      real(C_DOUBLE),     pointer :: bndOrdsFptr(:)   => null()
      type(C_PTR),        pointer :: atInfFptr(:)     => null()
      integer(C_SIZE_T) :: symMaxLen, atInfMaxLen

      call amspipe_extract_SetSystem(self%pipe, message%msg, errCptr, numAtoms, atSymsCptr, &
                                     crdsCptr, numLatVecs, latVecsCPtr, totalCharge, &
                                     numBonds, bndsCptr, bndOrdsCptr, atInfCptr)
      if (C_F_error(errCptr, error)) return

      call C_F_POINTER(atSymsCptr, atSymsFptr, [numAtoms])
      ! Find out which symbol string is the longest.
      ! That determines how long we'll allocate them all in the array ...
      symMaxLen = 0
      do iAtom = 1, numAtoms
         symMaxLen = max(symMaxLen, C_strlen(atSymsFptr(iAtom)))
      enddo
      allocate(character(symMaxLen) :: atomSymbols(numAtoms))
      do iAtom = 1, numAtoms
         atomSymbols(iAtom) = C_F_string(atSymsFptr(iAtom))
      enddo

      call C_F_pointer(crdsCptr, crdsFptr, [3_C_INT64_T, numAtoms])
      coords = crdsFptr

      if (C_ASSOCIATED(latVecsCPtr)) then
         call C_F_pointer(latVecsCPtr, latVecsFptr, [3_C_INT64_T, numLatVecs])
         latticeVectors = latVecsFptr
      endif

      if (C_ASSOCIATED(bndsCptr)) then
         call C_F_pointer(bndsCptr, bndsFptr, [2_C_INT64_T, numBonds])
         bonds = bndsFptr
      endif

      if (C_ASSOCIATED(bndOrdsCptr)) then
         call C_F_pointer(bndOrdsCptr, bndOrdsFptr, [numBonds])
         bondOrders = bndOrdsFptr
      endif

      if (C_ASSOCIATED(atInfCptr)) then
         call C_F_POINTER(atInfCptr, atInfFptr, [numAtoms])
         atInfMaxLen = 0
         do iAtom = 1, numAtoms
            if (C_ASSOCIATED(atInfFptr(iAtom))) atInfMaxLen = max(atInfMaxLen, C_strlen(atInfFptr(iAtom)))
         enddo
         allocate(character(atInfMaxLen) :: atomicInfo(numAtoms))
         do iAtom = 1, numAtoms
            if (C_ASSOCIATED(atInfFptr(iAtom))) then
               atomicInfo(iAtom) = C_F_string(atInfFptr(iAtom))
            else
               atomicInfo(iAtom) = ""
            endif
         enddo
      endif

      ! free memory that the C interface allocated
      do iAtom = 1, numAtoms
         call C_free(atSymsFptr(iAtom))
      enddo
      call C_free(atSymsCptr); atSymsCptr = C_NULL_PTR
      call C_free(crdsCptr);   crdsCptr   = C_NULL_PTR
      if (C_ASSOCIATED(latVecsCPtr)) then
         call C_free(latVecsCPtr); latVecsCPtr = C_NULL_PTR
      endif
      call C_free(bndsCptr);    bndsCptr    = C_NULL_PTR
      call C_free(bndOrdsCptr); bndOrdsCptr = C_NULL_PTR

   end subroutine


   subroutine Extract_SetCoords(self, message, error, coords)
      class(AMSPipe),                  intent(in)          :: self
      type(AMSPipeMessage),            intent(in)          :: message
      type(AMSPipeError), allocatable, intent(out)         :: error
      real(C_DOUBLE), contiguous,      intent(out), target :: coords(:,:)

      type(C_PTR) :: errCptr = C_NULL_PTR
      call amspipe_extract_SetCoords(self%pipe, message%msg, errCptr, C_LOC(coords(1,1)))
      if (C_F_error(errCptr, error)) return

   end subroutine


   subroutine Extract_SetLattice(self, message, error, latticeVectors)
      class(AMSPipe),                  intent(in)  :: self
      type(AMSPipeMessage),            intent(in)  :: message
      type(AMSPipeError), allocatable, intent(out) :: error
      real(C_DOUBLE),     allocatable, intent(out) :: latticeVectors(:,:)

      integer(C_INT64_T) :: numLatVecs
      type(C_PTR) :: errCptr = C_NULL_PTR, latVecsCPtr = C_NULL_PTR
      real(C_DOUBLE), pointer :: latVecsFptr(:,:) => null()

      call amspipe_extract_SetLattice(self%pipe, message%msg, errCptr, numLatVecs, latVecsCPtr)
      if (C_F_error(errCptr, error)) return

      if (C_ASSOCIATED(latVecsCPtr)) then
         call C_F_pointer(latVecsCPtr, latVecsFptr, [3_C_INT64_T, numLatVecs])
         latticeVectors = latVecsFptr
      endif

      ! free memory that the C interface allocated
      if (C_ASSOCIATED(latVecsCPtr)) then
         call C_free(latVecsCPtr); latVecsCPtr = C_NULL_PTR
      endif

   end subroutine


   subroutine Extract_Solve(self, message, error, request, keepResults, prevTitle)
      class(AMSPipe),                  intent(in)  :: self
      type(AMSPipeMessage),            intent(in)  :: message
      type(AMSPipeError), allocatable, intent(out) :: error
      type(AMSPipeSolveRequest),       intent(out) :: request
      logical,                         intent(out) :: keepResults
      character(:),       allocatable, intent(out) :: prevTitle

      type(amspipe_solverequest_t) :: rq
      type(C_PTR) :: errCptr = C_NULL_PTR, ptCptr = C_NULL_PTR
      integer(C_BOOL_INTKIND) :: kr

      call amspipe_extract_Solve(self%pipe, message%msg, errCptr, rq, kr, ptCptr)
      if (C_F_error(errCptr, error)) return

      request%title           = C_F_string(rq%title)
      request%quiet           = C_F_bool(rq%quiet)
      request%gradients       = C_F_bool(rq%gradients)
      request%stressTensor    = C_F_bool(rq%stressTensor)
      request%elasticTensor   = C_F_bool(rq%elasticTensor)
      request%hessian         = C_F_bool(rq%hessian)
      request%dipoleMoment    = C_F_bool(rq%dipoleMoment)
      request%dipoleGradients = C_F_bool(rq%dipoleGradients)
      request%other           = C_F_bool(rq%other)
      call delete_amspipe_solverequest(rq)

      keepResults = C_F_bool(kr)

      if (C_ASSOCIATED(ptCptr)) then
         prevTitle = C_F_string(ptCptr)
         call C_free(ptCptr); ptCptr = C_NULL_PTR
      endif

   end subroutine


   subroutine Extract_DeleteResults(self, message, error, title)
      class(AMSPipe),                  intent(in)  :: self
      type(AMSPipeMessage),            intent(in)  :: message
      type(AMSPipeError), allocatable, intent(out) :: error
      character(:),       allocatable, intent(out) :: title

      type(C_PTR) :: errCptr = C_NULL_PTR, tCptr = C_NULL_PTR
      call amspipe_extract_DeleteResults(self%pipe, message%msg, errCptr, tCptr)
      if (C_F_error(errCptr, error)) return

      title = C_F_string(tCptr)
      call C_free(tCptr); tCptr = C_NULL_PTR

   end subroutine


! ===== reply pipe =============================================================================================================

   subroutine Send_return(self, status, method, argument, message)
      class(AMSPipe),         intent(in) :: self
      integer(AMSPipeStatus), intent(in) :: status
      character(*),           intent(in) :: method, argument, message

      call amspipe_send_return(self%pipe, status, method//C_NULL_CHAR, argument//C_NULL_CHAR, message//C_NULL_CHAR)

   end subroutine

   subroutine Send_results(self, results)
      class(AMSPipe),       intent(in)    :: self
      type(AMSPipeResults), intent(inout) :: results

      type(amspipe_results_t) :: r

      ! TODO: translate messages ...
      r%energy = results%energy
      r%gradients = results%gradients;
      r%gradients_dim = results%gradients_dim
      r%stressTensor = results%stressTensor
      r%stressTensor_dim = results%stressTensor_dim
      r%elasticTensor = results%elasticTensor
      r%elasticTensor_dim = results%elasticTensor_dim
      r%hessian = results%hessian
      r%hessian_dim = results%hessian_dim
      r%dipoleMoment = results%dipoleMoment
      r%dipoleMoment_dim = results%dipoleMoment_dim
      r%dipoleGradients = results%dipoleGradients
      r%dipoleGradients_dim = results%dipoleGradients_dim

      call amspipe_send_results(self%pipe, r)

   end subroutine


end module
