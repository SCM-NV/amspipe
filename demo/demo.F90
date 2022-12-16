#include <amspipe.F90>

program amspipe_demo

   use amspipe
   use, intrinsic :: ISO_FORTRAN_ENV

   type(AMSCallPipe)    :: call_pipe
   type(AMSReplyPipe)   :: reply_pipe
   type(AMSPipeMessage) :: msg

   logical :: error = .false.

   ! Variables holding our current system:
   !character(:), allocatable :: atomSymbols
   !real(real64), allocatable :: coords(:,:)
   !real(real64), allocatable :: latticeVectors(:,:)
   !real(real64)              :: totalCharge = 0.0

   call  call_pipe%New("call_pipe")
   call reply_pipe%New("reply_pipe")

   do while (.true.)
      call call_pipe%Receive(msg)
      print *, "Method called: ", msg%name

      if (msg%name == "Exit") then
         exit

      else if (error) then

         if (index(msg%name, "Set") == 1) then
            ! Calls to "Set" methods are ignored while an error is buffered.

         else
            ! Non-"Set" method called: return buffered error and clear it.

         endif

      else if (msg%name == "Hello") then

         ! TODO: actually check version ...
         call reply_pipe%Send_return(AMSPIPE_STATUS_SUCCESS, "", "", "")

      else if (msg%name == "SetCoords") then

      else if (msg%name == "SetLattice") then

      else if (msg%name == "SetSystem") then

      else if (msg%name == "Solve") then

      else if (msg%name == "DeleteResults") then

      else
         ! TODO: handle unknown method call

      endif
   end do

end program
