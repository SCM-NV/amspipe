#include <cstdint>
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

#include "amspipe.hpp"




int main(int argc, char* argv[]) {

   std::cout << "before opening pipes" << std::endl;

   AMSCallPipe   call_pipe;
   AMSReplyPipe reply_pipe;

   std::cout << "after opening pipes" << std::endl;

   while (true) {
      std::cout << "Reading method from call pipe... " << std::endl;
      auto method = call_pipe.next_method();
      std::cout << "Method called: " << method << std::endl;

      if (method == "Hello") {

         int64_t version;
         call_pipe.recv_hello(&version);
         std::cout << "version=" << version << std::endl;
         reply_pipe.send_return( version == 1 ? AMSReplyPipe::Status::success : AMSReplyPipe::Status::unknown_version);

      } else if (method == "Exit") {
         break;

      } else if (method == "SetCoords") {

      } else if (method == "SetLattice") {

      } else if (method == "SetSystem") {

      } else if (method == "Solve") {

      } else {
         // TODO: read until end of message ...
         throw AMSPipeError("Unknown method called!");
      }

      call_pipe.method_called();
   }

}
