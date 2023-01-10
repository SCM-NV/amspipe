#include <iostream>
#include <vector>
#include <string>
#include <set>
#include <cmath>
#include <optional>

#include "amspipe.hpp"

// Declare some convenient methods to use in main:
void print_system(
   const std::vector<std::string>& atomSymbols,
   const std::vector<double>&      coords,
   const std::vector<double>&      latticeVectors,
   double                          totalCharge,
   std::vector<int64_t>&           bonds,
   std::vector<double>&            bondOrders,
   std::vector<std::string>&       atomicInfo
);
double LJ_potential(const std::vector<double>& coords, std::vector<double>& gradients);


int main() {
   std::cout << "AMSPipe demo: C++" << std::endl;

   AMSCallPipe   call_pipe;
   AMSReplyPipe reply_pipe;

   // Variables holding our current system:
   std::vector<std::string> atomSymbols;
   std::vector<double>      coords;
   std::vector<double>      latticeVectors;
   double                   totalCharge = 0.0;
   std::vector<int64_t>     bonds;
   std::vector<double>      bondOrders;
   std::vector<std::string> atomicInfo;

   // Cache of results we have kept:
   std::set<std::string> keptResults; // For this demo we just keep their titles and no actual data ...

   // Variable to store the error until we send the corresponding return message:
   std::optional<AMSPipe::Error> error;

   while (true) {
      auto msg = call_pipe.receive();
      //std::cout << "Method called: " << msg << std::endl;

      try {
         if (msg.name == "Exit") {
            break;

         } else if (error) { // We still have an error buffered
            if (msg.name.rfind("Set", 0) == 0) {
               // Calls to "Set" methods are ignored while an error is buffered.
               continue;
            } else {
               // Non-"Set" method called: return buffered error and clear it.
               reply_pipe.send_return(error->status, error->method, error->argument, error->what());
               error.reset();
            }

         } else if (msg.name == "Hello") {
            int64_t version;
            call_pipe.extract_Hello(msg, version);
            reply_pipe.send_return( version == 1 ? AMSPipe::Status::success : AMSPipe::Status::unknown_version);

         } else if (msg.name == "SetCoords") {
            call_pipe.extract_SetCoords(msg, coords.data());

         } else if (msg.name == "SetLattice") {
            call_pipe.extract_SetLattice(msg, latticeVectors);

         } else if (msg.name == "SetSystem") {
            call_pipe.extract_SetSystem(msg, atomSymbols, coords, latticeVectors, totalCharge, bonds, bondOrders, atomicInfo);

            //std::cout << "Received new system!" << std::endl;
            //print_system(atomSymbols, coords, latticeVectors, totalCharge, bonds, bondOrders, atomicInfo);

         } else if (msg.name == "Solve") {
            AMSPipe::SolveRequest request;
            bool keepResults;
            std::string prevTitle;

            call_pipe.extract_Solve(msg, request, keepResults, prevTitle);

            //std::cout << "Request:" << std::endl;
            //std::cout << "   title: " << request.title << std::endl;
            //std::cout << "   gradients: " << request.gradients << std::endl;
            //std::cout << "   stressTensor: " << request.stressTensor  << std::endl;
            //std::cout << "   elasticTensor: " << request.elasticTensor  << std::endl;
            //std::cout << "   hessian: " << request.hessian  << std::endl;
            //std::cout << "   dipoleMoment: " << request.dipoleMoment  << std::endl;
            //std::cout << "   dipoleGradients: " << request.dipoleGradients  << std::endl;
            //std::cout << "keepResults: " << keepResults << std::endl;
            //if (!prevTitle.empty()) std::cout << "prevTitle: " << prevTitle << std::endl;

            if (keptResults.find(request.title) != keptResults.end()) {
               throw AMSPipe::Error(AMSPipe::Status::logic_error, "Solve", "title",
                                    "title in request corresponds to an already stored results object");
            }
            if (!prevTitle.empty() && keptResults.find(prevTitle) == keptResults.end()) {
               throw AMSPipe::Error(AMSPipe::Status::logic_error, "Solve", "prevTitle",
                                    "prevTitle does not correspond to a kept results object");
            }
            if (keepResults) keptResults.insert(request.title);

            AMSPipe::Results results;
            std::vector<double> grads( request.gradients ? coords.size() : 0 );
            results.energy = LJ_potential(coords, grads);
            if (request.gradients) {
               results.gradients = grads.data();
               results.gradients_dim[0] = 3;
               results.gradients_dim[1] = grads.size()/3;
            }

            if (true) { // we are so simple that we never fail ...
               reply_pipe.send_results(results);
               reply_pipe.send_return(AMSPipe::Status::success);
            } else { // ... but if we did, we'd send a runtime_error as the return code
               reply_pipe.send_return(AMSPipe::Status::runtime_error, "Solve", "", "error evaluating the potential");
               /* or:
                 throw AMSPipe::Error(AMSPipe::Status::runtime_error, "Solve", "", "error evaluating the potential");
               */
            }

         } else if (msg.name == "DeleteResults") {
            std::string title;
            call_pipe.extract_DeleteResults(msg, title);
            //std::cout << "DeleteResults title: " << title << std::endl;

            if (keptResults.erase(title) == 0) {
               throw AMSPipe::Error(AMSPipe::Status::logic_error, "DeleteResults", "title",
                                    "DeleteResults called with title that was never stored");
            }
            reply_pipe.send_return(AMSPipe::Status::success); // we are so simple that we never fail ...

         } else {
            throw AMSPipe::Error(AMSPipe::Status::unknown_method, msg.name, "", "unknown method "+msg.name+" called");
         }

      } catch (const AMSPipe::Error& exc) {
         if (msg.name.rfind("Set", 0) == 0) {
            // Exception thrown during "Set" method: buffer it for return later.
            if (!error) error = exc;
         } else {
            // Exception thrown during non-"Set" method: return error immediately.
            reply_pipe.send_return(exc.status, exc.method, exc.argument, exc.what());
         }
      }
   }
   return 0;
}


void print_system(
   const std::vector<std::string>& atomSymbols,
   const std::vector<double>&      coords,
   const std::vector<double>&      latticeVectors,
   double                          totalCharge,
   std::vector<int64_t>&           bonds,
   std::vector<double>&            bondOrders,
   std::vector<std::string>&       atomicInfo
) {
   std::cout << "System" << std::endl;
   std::cout << "   Atoms [Bohr]" << std::endl;
   for (size_t iat = 0; iat < atomSymbols.size(); ++iat) {
      std::cout << "      " << atomSymbols[iat];
      for (int xyz = 0; xyz < 3; ++xyz)
         std::cout << "   " << coords[3*iat+xyz];
      if (!atomicInfo.empty())
         std::cout << "   " << atomicInfo[iat];
      std::cout << std::endl;
   }
   std::cout << "   End" << std::endl;
   if (!bondOrders.empty()) {
      std::cout << "   BondOrders" << std::endl;
      for (size_t ibnd = 0; ibnd < bondOrders.size(); ++ibnd)
         std::cout << "      " << bonds[2*ibnd] << " " << bonds[2*ibnd+1] << " " << bondOrders[ibnd] << std::endl;
      std::cout << "   End" << std::endl;
   }
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
}


double LJ_potential(const std::vector<double>& coords, std::vector<double>& gradients) {
   const double eps  = 0.02;
   const double rmin = 5.0;

   int nAtoms = coords.size()/3;
   const double rmin2 = rmin * rmin;

   double energy = 0.0;
   if (!gradients.empty()) for (double& r: gradients) r = 0.0;

   for (int iAtom = 0; iAtom < nAtoms; ++iAtom) {
      for (int jAtom = iAtom+1; jAtom < nAtoms; ++jAtom) {

         const double vecrij[3] = { coords[3*jAtom+0]-coords[3*iAtom+0],
                                    coords[3*jAtom+1]-coords[3*iAtom+1],
                                    coords[3*jAtom+2]-coords[3*iAtom+2] };

         const double rij2 = vecrij[0]*vecrij[0] + vecrij[1]*vecrij[1] + vecrij[2]*vecrij[2];

         const double rrel2 = rmin2 / rij2;
         const double rrel6 = rrel2*rrel2*rrel2;
         const double rrel12 = rrel6*rrel6;
         energy += eps * (rrel12 - 2.0*rrel6);

         if (!gradients.empty()) {
            const double gradij = 12.0 * eps / rij2 * (rrel6 - rrel12);

            gradients[3*iAtom+0] -= gradij * vecrij[0];
            gradients[3*iAtom+1] -= gradij * vecrij[1];
            gradients[3*iAtom+2] -= gradij * vecrij[2];

            gradients[3*jAtom+0] += gradij * vecrij[0];
            gradients[3*jAtom+1] += gradij * vecrij[1];
            gradients[3*jAtom+2] += gradij * vecrij[2];
         }
      }
   }

   return energy;
}
