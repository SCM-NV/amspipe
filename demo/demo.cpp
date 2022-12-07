#include <iostream>
#include <vector>
#include <string>
#include <set>

#include "amspipe.hpp"


int main(int argc, char* argv[]) {

   std::cout << "before opening pipes" << std::endl;

   AMSCallPipe   call_pipe;
   AMSReplyPipe reply_pipe;

   // Variables holding our current system:
   std::vector<std::string> atomSymbols;
   std::vector<double>      coords;
   std::vector<double>      latticeVectors;
   double                   totalCharge;

   // Cache of results we have kept:
   std::set<std::string> keptResults;
   // (For this demo we just keep their titles and no actual data ...)

   std::cout << "after opening pipes" << std::endl;

   while (true) {
      std::cout << "Reading message from call pipe... " << std::endl;
      auto msg = call_pipe.receive();
      std::cout << "Method called: " << msg.name << std::endl;

      if (msg.name == "Hello") {

         int64_t version;
         call_pipe.extract_Hello(msg, version);
         std::cout << "version=" << version << std::endl;
         reply_pipe.send_return( version == 1 ? AMSPipe::Status::success : AMSPipe::Status::unknown_version);

      } else if (msg.name == "Exit") {
         break;

      } else if (msg.name == "SetCoords") {

         call_pipe.extract_SetCoords(msg, coords.data());

      } else if (msg.name == "SetLattice") {

         call_pipe.extract_SetLattice(msg, latticeVectors);

      } else if (msg.name == "SetSystem") {

         call_pipe.extract_SetSystem(msg, atomSymbols, coords, latticeVectors, totalCharge);

         std::cout << "Received new system!" << std::endl;
         std::cout << "System" << std::endl;
         std::cout << "   Atoms [Bohr]" << std::endl;
         for (size_t iat = 0; iat < atomSymbols.size(); ++iat) {
            std::cout << "      " << atomSymbols[iat];
            for (int xyz = 0; xyz < 3; ++xyz)
               std::cout << "   " << coords[3*iat+xyz];
            std::cout << std::endl;
         }
         std::cout << "   End" << std::endl;
         if (totalCharge != 0.0) {
            std::cout << "   Charge " << totalCharge << std::endl;
         }
         if (!latticeVectors.empty()) {
            std::cout << "   Lattice [Bohr]" << std::endl;
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

         AMSPipe::SolveRequest request;
         bool keepResults;
         std::string prevTitle;

         call_pipe.extract_Solve(msg, request, keepResults, prevTitle);

         std::cout << "Request:" << std::endl << request;
         std::cout << "keepResults: " << keepResults << std::endl;
         if (!prevTitle.empty()) std::cout << "prevTitle: " << prevTitle << std::endl;

         if (keptResults.find(request.title) != keptResults.end()) {
            throw AMSPipe::Error("logic_error: title in request corresponds to an already stored results object");
         }
         if (!prevTitle.empty() && keptResults.find(prevTitle) == keptResults.end()) {
            throw AMSPipe::Error("logic_error: prevTitle does not correspond to a kept results object");
         }
         if (keepResults) keptResults.insert(request.title);

         // TODO: actual computation here ...
         AMSPipe::Results results;
         results.energy = 13.7;

         std::vector<double> grads;
         if (request.gradients) {
            grads.resize(coords.size(), 0.0);
            results.gradients = grads.data();
            results.gradients_dim[0] = 3;
            results.gradients_dim[1] = grads.size()/3;
         }

         reply_pipe.send_results(results);
         reply_pipe.send_return(AMSPipe::Status::success); // we are so simple that we never fail ...

      } else if (msg.name == "DeleteResults") {

         std::string title;
         call_pipe.extract_DeleteResults(msg, title);
         std::cout << "DeleteResults title: " << title << std::endl;
         if (keptResults.erase(title) == 0) {
            throw AMSPipe::Error("logic_error: DeleteResults called with title that was never stored");
         }
         reply_pipe.send_return(AMSPipe::Status::success); // we are so simple that we never fail ...

      } else {
         throw AMSPipe::Error("Unknown method called!");
      }

   }

}
