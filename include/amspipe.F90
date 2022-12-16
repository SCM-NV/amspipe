module amspipe

   use, intrinsic :: ISO_C_BINDING

   implicit none
   private

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
         type(C_PTR), value :: message ! amspipe_message_t*
      end subroutine
   end interface


   ! =============
   !  AMSCallPipe
   ! =============
   !
   type, bind(C) :: amscallpipe_t
      type(C_PTR) :: p = C_NULL_PTR ! AMSCallPipe*
   end type
   type, public :: AMSCallPipe
      type(amscallpipe_t), private :: cp
   contains
      generic,   public  :: New => NewCallPipe
      procedure, private :: NewCallPipe
      generic,   public  :: Delete => DeleteCallPipe
      procedure, private :: DeleteCallPipe
      final              :: FinalizeCallPipe
      procedure, public  :: Receive
   end type
   interface
      type(amscallpipe_t) function new_amscallpipe(filename) bind(C, name="new_amscallpipe")
         import
         character(C_CHAR), intent(in) :: filename(*)
      end function
      subroutine delete_amscallpipe(cp) bind(C, name="delete_amscallpipe")
         import
         type(C_PTR), value :: cp ! amscallpipe_t*
      end subroutine
      subroutine amscallpipe_receive(cp, message) bind(C, name="amscallpipe_receive")
         import
         type(amscallpipe_t), value :: cp      ! amscallpipe_t
         type(C_PTR),         value :: message ! amspipe_message_t*
      end subroutine
   end interface


   ! ==============
   !  AMSReplyPipe
   ! ==============
   !
   type, bind(C) :: amsreplypipe_t
      type(C_PTR) :: p = C_NULL_PTR ! AMSReplyPipe*
   end type
   type, public :: AMSReplyPipe
      type(amsreplypipe_t), private :: rp
   contains
      generic,   public  :: New => NewReplyPipe
      procedure, private :: NewReplyPipe
      generic,   public  :: Delete => DeleteReplyPipe
      procedure, private :: DeleteReplyPipe
      final              :: FinalizeReplyPipe
      procedure, public  :: Send_return
   end type
   interface
      type(amsreplypipe_t) function new_amsreplypipe(filename) bind(C, name="new_amsreplypipe")
         import
         character(C_CHAR), intent(in) :: filename(*)
      end function
      subroutine delete_amsreplypipe(rp) bind(C, name="delete_amsreplypipe")
         import
         type(C_PTR), value, intent(in) :: rp ! amsreplypipe_t*
      end subroutine
      subroutine amsreplypipe_send_return(rp, status, method, argument, message) bind(C, name="amsreplypipe_send_return")
         import
         type(amsreplypipe_t),   value :: rp
         integer(AMSPipeStatus), value :: status
         character(C_CHAR), intent(in) :: method(*)
         character(C_CHAR), intent(in) :: argument(*)
         character(C_CHAR), intent(in) :: message(*)
      end subroutine
   end interface


   ! ===========
   !  Utilities
   ! ===========
   !
   interface
      pure integer(C_SIZE_T) function C_strlen(str) bind(C, name="strlen")
         import
         type(C_PTR), value, intent(in) :: str
      end function
   end interface

contains


! ===== AMSPipeMessage ===========================================================================================================


   impure elemental subroutine DeleteMessage(self)
      class(AMSPipeMessage), intent(inout), target :: self
      call delete_amspipe_message(C_LOC(self%msg))
   end subroutine
   !
   impure elemental subroutine FinalizeMessage(self)
      type(AMSPipeMessage), intent(inout) :: self
      call self%Delete()
   end subroutine


! ===== AMSCallPipe ==============================================================================================================


   subroutine NewCallPipe(self, filename)
      class(AMSCallPipe), intent(out) :: self
      character(*),       intent(in)  :: filename
      self%cp = new_amscallpipe(filename//C_NULL_CHAR)
   end subroutine


   impure elemental subroutine DeleteCallPipe(self)
      class(AMSCallPipe), intent(inout), target :: self
      call delete_amscallpipe(C_LOC(self%cp))
   end subroutine
   !
   impure elemental subroutine FinalizeCallPipe(self)
      type(AMSCallPipe), intent(inout) :: self
      call self%Delete()
   end subroutine


   subroutine Receive(self, message)
      class(AMSCallPipe),   intent(in)          :: self
      type(AMSPipeMessage), intent(out), target :: message

      call amscallpipe_receive(self%cp, C_LOC(message%msg))
      message%name = C_F_string(message%msg%name)

   end subroutine


! ===== AMSReplyPipe =============================================================================================================


   subroutine NewReplyPipe(self, filename)
      class(AMSReplyPipe), intent(out) :: self
      character(*),       intent(in)   :: filename
      self%rp = new_amsreplypipe(filename//C_NULL_CHAR)
   end subroutine


   impure elemental subroutine DeleteReplyPipe(self)
      class(AMSReplyPipe), intent(inout), target :: self
      call delete_amsreplypipe(C_LOC(self%rp))
   end subroutine
   !
   impure elemental subroutine FinalizeReplyPipe(self)
      type(AMSReplyPipe), intent(inout) :: self
      call self%Delete()
   end subroutine


   subroutine Send_return(self, status, method, argument, message)
      class(AMSReplyPipe),    intent(inout) :: self
      integer(AMSPipeStatus), intent(in)    :: status
      character(*),           intent(in)    :: method, argument, message

      call amsreplypipe_send_return(self%rp, status, method//C_NULL_CHAR, argument//C_NULL_CHAR, message//C_NULL_CHAR)

   end subroutine


! ===== Utilities ================================================================================================================


   function C_F_string(cstr) result(fstr)
      type(C_PTR), intent(in)   :: cstr
      character(:), allocatable :: fstr
      character(kind=C_CHAR, len=C_strlen(cstr)), pointer :: tmp
      call C_F_POINTER(cstr, tmp)
      fstr = tmp
   end function


end module
