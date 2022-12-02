#include <iostream>
#include <vector>
#include <string>

#include "amspipe.hpp"


int main(int argc, char* argv[]) {

   std::cout << "before opening pipes" << std::endl;

   AMSCallPipe   call_pipe;
   AMSReplyPipe reply_pipe;

   std::cout << "after opening pipes" << std::endl;

   while (true) {
      std::cout << "Reading message from call pipe... " << std::endl;
      auto msg = call_pipe.receive();
      std::cout << "Method called: " << msg.name << std::endl;

      if (msg.name == "Hello") {

         int64_t version;
         call_pipe.extract_Hello(msg, version);
         std::cout << "version=" << version << std::endl;
         reply_pipe.send_return( version == 1 ? AMSReplyPipe::Status::success : AMSReplyPipe::Status::unknown_version);

      } else if (msg.name == "Exit") {
         break;

      } else if (msg.name == "SetCoords") {

      } else if (msg.name == "SetLattice") {

      } else if (msg.name == "SetSystem") {

         std::vector<std::string> atomSymbols;
         std::vector<double>      coords;
         std::vector<double>      latticeVectors;
         double                   totalCharge;

         call_pipe.extract_SetSystem(msg, atomSymbols, coords, latticeVectors, totalCharge);

         std::cout << "Received new system!" << std::endl;
         std::cout << "System" << std::endl;
         std::cout << "   Atoms" << std::endl;
         for (size_t iat = 0; iat < atomSymbols.size(); ++iat) {
            std::cout << "      " << atomSymbols[iat];
            for (int xyz = 0; xyz < 3; ++xyz)
               std::cout << "   " << coords[3*iat+xyz];
            std::cout << std::endl;
         }
         std::cout << "   End" << std::endl;
         std::cout << "   Charge " << totalCharge << std::endl;
         if (!latticeVectors.empty()) {
            std::cout << "   Lattice" << std::endl;
            for (int ivec = 0; 3*ivec < latticeVectors.size(); ++ivec) {
               std::cout << "   ";
               for (int xyz = 0; xyz < 3; ++xyz)
                  std::cout << "   " << latticeVectors[3*ivec+xyz];
               std::cout << std::endl;
            }
            std::cout << "   End" << std::endl;
         }
         std::cout << "End" << std::endl;

      } else if (msg.name == "Solve") {

      } else {
         throw AMSPipe::Error("Unknown method called!");
      }

   }

}
