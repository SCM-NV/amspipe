#include "amspipe.hpp"

#include <ios>
#include <iostream>

#include "ubjson.hpp"



std::ostream& AMSPipe::operator<<(std::ostream& os, const AMSPipe::SolveRequest& request) {
   std::cout << "   title: " << request.title << std::endl;
   std::cout << "   gradients: " << request.gradients << std::endl;
   std::cout << "   stressTensor: " << request.stressTensor  << std::endl;
   std::cout << "   elasticTensor: " << request.elasticTensor  << std::endl;
   std::cout << "   hessian: " << request.hessian  << std::endl;
   std::cout << "   dipoleMoment: " << request.dipoleMoment  << std::endl;
   std::cout << "   dipoleGradients: " << request.dipoleGradients  << std::endl;
   return os;
}


// ===== AMSCallPipe =============================================================================================================

AMSCallPipe::AMSCallPipe(const std::string& filename) {
   pipe.exceptions(std::ofstream::failbit | std::ofstream::badbit);
   pipe.open(filename, std::ios::in | std::ios::binary);
}


AMSPipe::Message AMSCallPipe::receive() {
   AMSPipe::Message msg;

   // Each message starts with its size. Let's extract that from the pipe first:
   int32_t msgsize;
   pipe.read(reinterpret_cast<char*>(&msgsize), sizeof(msgsize));

   // Read the entire message from the pipe into a std::stringstream:
   std::string str;
   str.resize(msgsize);
   pipe.read(&str[0], msgsize);

   // DEBUG!!!
   std::cout << "==CALL=================" << std::endl;
   for (char c: str) std::cout << int(c) << " " << c << std::endl;
   std::cout << "=======================" << std::endl;
   // DEBUG!!!

   msg.payload.str(std::move(str)); // std::move here avoids copy starting with C++20

   // Message starts with { marker ...
   ubjson::verify_marker(msg.payload, '{');
   // ... followed by the string representing the name of the message ...
   msg.name = ubjson::read_key(msg.payload);
   // ... followed by the opening marker for the payload:
   ubjson::verify_marker(msg.payload, '{');
   // The string stream is now at the beginning of the payload.

   return msg;
}


void AMSCallPipe::extract_Hello(AMSPipe::Message& msg, int64_t& version) const {
   if (msg.name != "Hello") {
      throw AMSPipe::Error("AMSCallPipe::extract_Hello called on wrong message");
   }

   auto argument = ubjson::read_key(msg.payload);
   if (argument == "version") {
      version = ubjson::read_int(msg.payload);
   } else {
      throw AMSPipe::Error("unknown_argument");
   }
   ubjson::verify_marker(msg.payload, '}');
}


void AMSCallPipe::extract_SetSystem(AMSPipe::Message& msg,
   std::vector<std::string>& atomSymbols,
   std::vector<double>& coords,
   std::vector<double>& latticeVectors,
   double& totalCharge
) const {
   if (msg.name != "SetSystem") {
      throw AMSPipe::Error("AMSCallPipe::extract_SetSystem called on wrong message");
   }

   atomSymbols.clear();
   coords.clear();
   latticeVectors.clear();
   totalCharge = 0.0;

   std::vector<int64_t> atomSymbols_dim = {-1};
   std::vector<int64_t> coords_dim = {-1,-1};
   std::vector<int64_t> latticeVectors_dim = {-1,-1};

   while (ubjson::peek(msg.payload) != '}'){
      auto argument = ubjson::read_key(msg.payload);

      if (argument == "atomSymbols") {
         atomSymbols = ubjson::read_string_array(msg.payload);

      } else if (argument == "atomSymbols_dim_") {
         atomSymbols_dim = ubjson::read_int_array(msg.payload);

      } else if (argument == "coords") {
         coords = ubjson::read_real_array(msg.payload);

      } else if (argument == "coords_dim_") {
         coords_dim = ubjson::read_int_array(msg.payload);

      } else if (argument == "latticeVectors") {
         latticeVectors = ubjson::read_real_array(msg.payload);

      } else if (argument == "latticeVectors_dim_") {
         latticeVectors_dim = ubjson::read_int_array(msg.payload);

      } else if (argument == "totalCharge") {
         std::cerr << "Reading total charge" << std::endl;
         totalCharge = ubjson::read_real(msg.payload);

      } else {
         throw AMSPipe::Error("unknown_argument");
      }
   }
   ubjson::verify_marker(msg.payload, '}');

   // Checks on the completeness and consistency of the transferred data
   if (atomSymbols.empty()) {
      throw AMSPipe::Error("No atoms in SetSystem message");
   }
   if (atomSymbols_dim.size() != 1 || atomSymbols_dim[0] != atomSymbols.size()) {
      throw AMSPipe::Error("Size of atomSymbols is inconsistent with atomSymbols_dim_ in SetSystem message");
   }
   if (coords.size() != 3*atomSymbols.size()) {
      throw AMSPipe::Error("Sizes of atomSymbols and coords inconsistent in SetSystem message");
   }
   if (coords_dim.size() != 2 || coords_dim[0] != 3 || coords_dim[1] != atomSymbols.size()) {
      throw AMSPipe::Error("Size of coords is inconsistent with coords_dim_ in SetSystem message");
   }
   if (latticeVectors_dim.size() != 2 || latticeVectors_dim[0] * latticeVectors_dim[1] != latticeVectors.size()) {
      throw AMSPipe::Error("Size of latticeVectors is inconsistent with latticeVectors_dim_ in SetSystem message");
   }
   if (latticeVectors.size() != 0 && latticeVectors.size() != 3 && latticeVectors.size() != 6 && latticeVectors.size() != 9) {
      throw AMSPipe::Error("Unexpected size of latticeVectors in SetSystem message");
   }
}


void AMSCallPipe::extract_SetCoords(AMSPipe::Message& msg, double* coords) const {
   if (msg.name != "SetCoords") {
      throw AMSPipe::Error("AMSCallPipe::extract_SetCoords called on wrong message");
   }

   std::vector<double> newcoords; // TODO: remove and write directly to coords
   std::vector<int64_t> coords_dim = {-1,-1};

   while (ubjson::peek(msg.payload) != '}') {
      auto argument = ubjson::read_key(msg.payload);

      if (argument == "coords") {
         newcoords = ubjson::read_real_array(msg.payload);

      } else if (argument == "coords_dim_") {
         coords_dim = ubjson::read_int_array(msg.payload);

      } else {
         throw AMSPipe::Error("unknown_argument");
      }
   }
   ubjson::verify_marker(msg.payload, '}');

   // Checks on the completeness and consistency of the transferred data
   if (coords_dim.size() != 2 || coords_dim[0] * coords_dim[1] != newcoords.size()) {
      throw AMSPipe::Error("Size of coords is inconsistent with coords_dim_ in SetSystem message");
   }

   for (size_t i = 0; i < newcoords.size(); ++i) coords[i] = newcoords[i]; // TODO: remove and write directly to coords
}


void AMSCallPipe::extract_SetLattice(AMSPipe::Message& msg, std::vector<double>& latticeVectors) const {
   if (msg.name != "SetLattice") {
      throw AMSPipe::Error("AMSCallPipe::extract_SetLattice called on wrong message");
   }

   latticeVectors.clear();

   std::vector<int64_t> latticeVectors_dim = {-1,-1};

   while (ubjson::peek(msg.payload) != '}'){
      auto argument = ubjson::read_key(msg.payload);

      if (argument == "vectors") {
         latticeVectors = ubjson::read_real_array(msg.payload);

      } else if (argument == "vectors_dim_") {
         latticeVectors_dim = ubjson::read_int_array(msg.payload);

      } else {
         throw AMSPipe::Error("unknown_argument");
      }
   }
   ubjson::verify_marker(msg.payload, '}');

   // Checks on the completeness and consistency of the transferred data
   if (latticeVectors_dim.size() != 2 || latticeVectors_dim[0] * latticeVectors_dim[1] != latticeVectors.size()) {
      throw AMSPipe::Error("Size of latticeVectors is inconsistent with latticeVectors_dim_ in SetSystem message");
   }
   if (latticeVectors.size() != 0 && latticeVectors.size() != 3 && latticeVectors.size() != 6 && latticeVectors.size() != 9) {
      throw AMSPipe::Error("Unexpected size of latticeVectors in SetSystem message");
   }
}


void AMSCallPipe::extract_Solve(AMSPipe::Message& msg,
   AMSPipe::SolveRequest& request,
   bool& keepResults,
   std::string& prevTitle
) const {
   if (msg.name != "Solve") {
      throw AMSPipe::Error("AMSCallPipe::extract_Solve called on wrong message");
   }

   while (ubjson::peek(msg.payload) != '}') {
      auto argument = ubjson::read_key(msg.payload);

      if (argument == "request") {
         ubjson::verify_marker(msg.payload, '{');
         while (ubjson::peek(msg.payload) != '}') {
            auto field = ubjson::read_key(msg.payload);
            if (field == "title") {
               request.title = ubjson::read_string(msg.payload);
            } else if (field == "other") {
               // silently ignore "other" for now
               ubjson::read_bool(msg.payload);
            } else if (field == "quiet") {
               request.quiet = ubjson::read_bool(msg.payload);
            } else if (field == "gradients") {
               request.gradients = ubjson::read_bool(msg.payload);
            } else if (field == "stressTensor") {
               request.stressTensor = ubjson::read_bool(msg.payload);
            } else if (field == "elasticTensor") {
               request.elasticTensor = ubjson::read_bool(msg.payload);
            } else if (field == "hessian") {
               request.hessian = ubjson::read_bool(msg.payload);
            } else if (field == "dipoleMoment") {
               request.dipoleMoment = ubjson::read_bool(msg.payload);
            } else if (field == "dipoleGradients") {
               request.dipoleGradients = ubjson::read_bool(msg.payload);
            } else {
               if (ubjson::peek(msg.payload) == 'F') {
                  // The worker MAY raise an unknown argument error if an unknown Boolean is set to False ...
                  // ... but we just extract and ignore it ;-) ...
                  ubjson::read_bool(msg.payload);
               } else {
                  throw AMSPipe::Error("unknown_argument");
               }
            }
         }
         ubjson::verify_marker(msg.payload, '}');

         if (request.title.empty()) {
            throw AMSPipe::Error("invalid_argument: title may not be empty");
         }

      } else if (argument == "keepResults") {
         keepResults = ubjson::read_bool(msg.payload);

      } else if (argument == "prevTitle") {
         prevTitle = ubjson::read_string(msg.payload);

      } else {
         throw AMSPipe::Error("unknown_argument");
      }

   }
   ubjson::verify_marker(msg.payload, '}');

}


void AMSCallPipe::extract_DeleteResults(AMSPipe::Message& msg, std::string& title) const {
   if (msg.name != "DeleteResults") {
      throw AMSPipe::Error("AMSCallPipe::extract_DeleteResults called on wrong message");
   }

   auto argument = ubjson::read_key(msg.payload);
   if (argument == "title") {
      title = ubjson::read_string(msg.payload);
   } else {
      throw AMSPipe::Error("unknown_argument");
   }
   ubjson::verify_marker(msg.payload, '}');
}


// ===== AMSReplyPipe ============================================================================================================

AMSReplyPipe::AMSReplyPipe(const std::string& filename) {
   pipe.exceptions(std::ofstream::failbit | std::ofstream::badbit);
   pipe.open(filename, std::ios::out | std::ios::binary);
}


void AMSReplyPipe::send_return(AMSPipe::Status status, const std::string& method, const std::string& argument, const std::string& message) {
   std::stringstream buf;

   buf << '{';
   ubjson::write_key(buf, "return");
   buf << '{';
   ubjson::write_key(buf, "status");
   ubjson::write_int(buf, static_cast<int8_t>(status));
   if (!method.empty()) {
      ubjson::write_key(buf, "method");
      ubjson::write_string(buf, method);
   }
   if (!argument.empty()) {
      ubjson::write_key(buf, "argument");
      ubjson::write_string(buf, argument);
   }
   if (!argument.empty()) {
      ubjson::write_key(buf, "argument");
      ubjson::write_string(buf, argument);
   }
   buf << '}' << '}';

   send(buf);
}


void AMSReplyPipe::send_results(const AMSPipe::Results& results) {
   std::stringstream buf;

   buf << '{';
   ubjson::write_key(buf, "results");
   buf << '{';

   // the energy is the only result we *always* produce
   ubjson::write_key(buf, "energy");
   ubjson::write_real(buf, results.energy);

   if (results.gradients) {
      ubjson::write_key(buf, "gradients");
      ubjson::write_real_array(buf, results.gradients, results.gradients_dim[0]*results.gradients_dim[1]);
      ubjson::write_key(buf, "gradients_dim_");
      buf << '[';
      ubjson::write_int(buf, results.gradients_dim[0]);
      ubjson::write_int(buf, results.gradients_dim[1]);
      buf << ']';
   }

   // TODO: write other optional results ...

   buf << '}' << '}';

   send(buf);
}


void AMSReplyPipe::send(std::stringstream& buf) {
   auto tmp = buf.str();

   // DEBUG
   std::cout << "==REPLY================" << std::endl;
   for (char c: tmp) std::cout << int(c) << " " << c << std::endl;
   std::cout << "=======================" << std::endl;
   // DEBUG

   int32_t msgsize = tmp.size();
   pipe.write(reinterpret_cast<const char*>(&msgsize), sizeof(msgsize));
   pipe.write(&tmp[0], tmp.size());
   pipe.flush();
}
