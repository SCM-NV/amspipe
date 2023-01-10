#include <amspipe.F90>


module amspipe_demo_module

   use, intrinsic :: ISO_C_BINDING
   implicit none
   private

   public :: amspipe_demo

contains

   subroutine amspipe_demo

      use amspipe

      type(AMSCallPipe)    :: call_pipe
      type(AMSReplyPipe)   :: reply_pipe
      type(AMSPipeMessage) :: msg

      ! Variable to store the error until we send the corresponding return message:
      type(AMSPipeError), allocatable :: error

      ! Variables holding our current system:
      character(:),       allocatable :: atomSymbols(:)
      real(C_DOUBLE),     allocatable :: coords(:,:)
      real(C_DOUBLE),     allocatable :: latticeVectors(:,:)
      real(C_DOUBLE)                  :: totalCharge = 0.0
      integer(C_INT64_T), allocatable :: bonds(:,:)
      real(C_DOUBLE),     allocatable :: bondOrders(:)
      character(:),       allocatable :: atomicInfo(:)

      ! We do not make an attempt to keep track of the cached results in the Fortran language demo.
      ! Just too much effort since there is no dictionary in standard Fortran ...

      print *, "AMSPipe demo: Fortran"

      call  call_pipe%New("call_pipe")
      call reply_pipe%New("reply_pipe")

      msgloop: do while (.true.)
         call call_pipe%Receive(msg)
         !print *, "Method called: ", msg%name

         if (msg%name == "Exit") then
            exit msgloop

         else if (allocated(error)) then

            if (index(msg%name, "Set") == 1) then
               ! Calls to "Set" methods are ignored while an error is buffered.
               cycle msgloop
            else
               ! Non-"Set" method called: return buffered error and clear it.
               call reply_pipe%Send_return(error%status, error%method, error%argument, error%message)
               deallocate(error)
            endif

         else if (msg%name == "Hello") then
            block
            integer(C_INT64_T) :: version

            call call_pipe%Extract_Hello(msg, error, version)
            if (.not.allocated(error)) then
               if (version == 1) then
                  call reply_pipe%Send_return(AMSPIPE_STATUS_SUCCESS, "", "", "")
               else
                  call reply_pipe%Send_return(AMSPIPE_STATUS_UNKNOWN_VERSION, "", "", "")
               endif
            endif

            end block
         else if (msg%name == "SetCoords") then
            call call_pipe%Extract_SetCoords(msg, error, coords)

         else if (msg%name == "SetLattice") then
            call call_pipe%Extract_SetLattice(msg, error, latticeVectors)

         else if (msg%name == "SetSystem") then
            call call_pipe%Extract_SetSystem(msg, error, atomSymbols, coords, latticeVectors, totalCharge, &
                                                         bonds, bondOrders, atomicInfo)
            if (.not.allocated(error)) then
               !print *, "Received new system!"
               !call PrintSystem(atomSymbols, coords, latticeVectors, totalCharge, bonds, bondOrders, atomicInfo)
            endif

         else if (msg%name == "Solve") then
            block
            type(AMSPipeSolveRequest) :: request
            logical :: keepResults
            character(:), allocatable :: prevTitle

            type(AMSPipeResults) :: results
            real(C_DOUBLE), allocatable, target :: gradients(:,:)

            call call_pipe%Extract_Solve(msg, error, request, keepResults, prevTitle)
            if (.not.allocated(error)) then

               !print *, "Request:"
               !print *, "   title: ", request%title
               !print *, "   gradients: ", request%gradients
               !print *, "   stressTensor: ", request%stressTensor
               !print *, "   elasticTensor: ", request%elasticTensor
               !print *, "   hessian: ", request%hessian
               !print *, "   dipoleMoment: ", request%dipoleMoment
               !print *, "   dipoleGradients: ", request%dipoleGradients
               !print *, "keepResults: ", keepResults
               !if (allocated(prevTitle)) print *, "prevTitle: ", prevTitle

               if (request%gradients) allocate(gradients(size(coords,1), size(coords, 2)))

               results%energy = LJ_potential(coords, gradients)

               if (request%gradients) then
                  results%gradients = C_LOC(gradients(1,1))
                  results%gradients_dim = [ size(gradients,1), size(gradients,2) ]
               endif

               if (.true.) then
                  call reply_pipe%Send_results(results)
                  call reply_pipe%Send_return(AMSPIPE_STATUS_SUCCESS, "", "", "")
               else
                  call reply_pipe%Send_return(AMSPIPE_STATUS_RUNTIME_ERROR, "Solve", "", "error evaluating the potential")
               endif

            endif
            end block
         else if (msg%name == "DeleteResults") then
            block
            character(:), allocatable :: title
            call call_pipe%Extract_DeleteResults(msg, error, title)
            if (.not.allocated(error)) then
               !print *, "DeleteResults title: ", title
               ! We do not keep a cache of results, so we just confirm the deletion and move on ...
               call reply_pipe%Send_return(AMSPIPE_STATUS_SUCCESS, "", "", "")
            endif

            end block
         else
            allocate(error)
            error%status = AMSPIPE_STATUS_UNKNOWN_METHOD
            error%method = msg%name
            error%argument = ""
            error%message = "unknown method "//msg%name//" called"

         endif

         if (allocated(error) .and. index(msg%name, "Set") /= 1) then
            ! Error during non-"Set" method: return and clear error immediately.
            call reply_pipe%Send_return(error%status, error%method, error%argument, error%message)
            deallocate(error)
         endif
      end do msgloop

   end subroutine


   subroutine PrintSystem(atomSymbols, coords, latticeVectors, totalCharge, bonds, bondOrders, atomicInfo)
      character(*),       intent(in)           :: atomSymbols(:)
      real(C_DOUBLE),     intent(in)           :: coords(:,:)
      real(C_DOUBLE),     intent(in), optional :: latticeVectors(:,:)
      real(C_DOUBLE),     intent(in)           :: totalCharge
      integer(C_INT64_T), intent(in), optional :: bonds(:,:)
      real(C_DOUBLE),     intent(in), optional :: bondOrders(:)
      character(*),       intent(in), optional :: atomicInfo(:)

      integer(C_INT64_T) :: iAtom, iLatVec, iBond

      print *, "System"
      print *, "   Atoms [Bohr]"
      do iAtom = 1, size(atomSymbols)
         if (present(atomicInfo)) then
            print *, "      ", atomSymbols(iAtom), coords(:,iAtom), "   ", atomicInfo(iAtom)
         else
            print *, "      ", atomSymbols(iAtom), coords(:,iAtom)
         endif
      enddo
      print *, "   End"
      if (present(bondOrders)) then
         print *, "   BondOrders"
         do iBond = 1, size(bondOrders)
            print *, "      ", bonds(1,iBond), bonds(2,iBond), bondOrders(iBond)
         enddo
         print *, "   End"
      endif
      if (totalCharge /= 0.0) print *, "   Charge ", totalCharge
      if (present(latticeVectors)) then
         print *, "   Lattice [Bohr]"
         do iLatVec = 1, size(latticeVectors, 2)
            print *, "      ", latticeVectors(:, iLatVec)
         enddo
         print *, "   End"
      endif
      print *, "End"

   end subroutine


   real(C_DOUBLE) function LJ_potential(coords, gradients) result(energy)
      real(C_DOUBLE), intent(in)            :: coords(:,:)
      real(C_DOUBLE), intent(out), optional :: gradients(:,:)

      real(C_DOUBLE), parameter :: eps   = 0.02
      real(C_DOUBLE), parameter :: rmin  = 5.0

      real(C_DOUBLE), parameter :: rmin2 = rmin*rmin
      integer :: iAtom, jAtom, nAtoms
      real(C_DOUBLE) :: rij2, rrel2, rrel6, rrel12, gradij
      real(C_DOUBLE) :: vecrij(3)

      nAtoms = size(coords,2)

      energy = 0.0
      if (present(gradients)) gradients = 0.0

      do iAtom = 1, nAtoms
         do jAtom = iAtom+1, nAtoms

         vecrij = coords(:,jAtom) - coords(:, iAtom)

         rij2 = vecrij(1)*vecrij(1) + vecrij(2)*vecrij(2) + vecrij(3)*vecrij(3)

         rrel2 = rmin2 / rij2
         rrel6 = rrel2*rrel2*rrel2
         rrel12 = rrel6*rrel6
         energy = energy + eps * (rrel12 - 2.0*rrel6)

         if (present(gradients)) then
            gradij = 12.0 * eps / rij2 * (rrel6 - rrel12);
            gradients(:,iAtom) = gradients(:,iAtom) - gradij * vecrij
            gradients(:,jAtom) = gradients(:,jAtom) + gradij * vecrij
         endif

         enddo
      enddo

   end function

end module


program demo
   use amspipe_demo_module
   call amspipe_demo
end program
