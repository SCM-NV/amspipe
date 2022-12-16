#include <amspipe.F90>

program amspipe_demo

   use amspipe

   type(AMSCallPipe)    :: call_pipe
   type(AMSReplyPipe)   :: reply_pipe
   type(AMSPipeMessage) :: msg

   call  call_pipe%New("call_pipe")
   call reply_pipe%New("reply_pipe")

   do while (.true.)
      call call_pipe%Receive(msg)
      print *, "Method called: ", msg%name

      exit
   end do

   call  call_pipe%Delete()
   call reply_pipe%Delete()

end program
