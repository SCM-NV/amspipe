#include "amspipe.hpp"

#include <ios>
#include <iostream>

#ifdef _WIN32
#include <cerrno>
#include <direct.h>
#endif

#include "ubjson.hpp"


// ===== AMSPipe =============================================================================================================

AMSPipe::AMSPipe() : sendBuffer(new ubjson::outstream()) {
#ifdef _WIN32
   // Single Win32 bidirectional named pipe, named after the current directory
   std::string pipe_path(1024, '\0');
   char *ret;
   while ((ret = _getcwd(&pipe_path[0], pipe_path.size())) == NULL && errno == ERANGE && pipe_path.size() <= 1024*1024) {
      // Buffer was too small, grow it up to a sanity limit of 1 MiB
      pipe_path.resize(2*pipe_path.size());
   }
   if (!ret) throw std::runtime_error("Error getting the current working directory");
   pipe_path.resize(pipe_path.find('\0'));
   if (pipe_path.back() == '/' || pipe_path.back() == '\\') {
      // Prevent duplicate slash (present if CWD is the root directory of a drive) when /amspipe is added below
      pipe_path.pop_back();
   }
   for (char &c : pipe_path) {
      switch (c) {
         case ':':
         case '/':
         case '\\':
            c = '_';
            break;
      }
   }
   pipe_path = "\\\\.\\pipe\\" + pipe_path + "_amspipe";
   call_pipe = std::fopen(pipe_path.c_str(), "rb+");
   if (!call_pipe) throw std::runtime_error("Could not open pipe \"" + pipe_path + '"');
   reply_pipe = call_pipe;
#else
   // Pair of POSIX FIFOs in the current directory
   call_pipe = std::fopen("call_pipe", "r");
   if (!call_pipe) throw std::runtime_error("Could not open call pipe");
   reply_pipe = std::fopen("reply_pipe", "w");
   if (!reply_pipe) throw std::runtime_error("Could not open reply pipe");
#endif
}


AMSPipe::~AMSPipe() noexcept {
   std::fclose(call_pipe);
#ifndef _WIN32
   std::fclose(reply_pipe);
#endif
}

// ===== call pipe =============================================================================================================

AMSPipe::Message AMSPipe::receive() {
   AMSPipe::Message msg;

   // Each message starts with its size. Let's extract that from the pipe first:
   int32_t msgsize;
   if (std::fread(&msgsize, sizeof(msgsize), 1, call_pipe) != 1)
      throw AMSPipe::Error(AMSPipe::Status::runtime_error, "", "", "Could not read size of incoming message from call pipe");

   // Read the entire message from the pipe into a std::stringstream:
   std::string str;
   str.resize(msgsize);
   for (size_t pos = 0, nread = 0; pos < (size_t) msgsize; pos += nread) {
      nread = fread(&str[pos], sizeof(str[0]), msgsize - pos, call_pipe);
      if (nread == 0)
         throw AMSPipe::Error(AMSPipe::Status::runtime_error, "", "", "Could not read incoming message from call pipe");
   }

   // DEBUG!!!
   //std::cout << "==CALL=================" << std::endl;
   //for (char c: str) std::cout << int(c) << " " << c << std::endl;
   //std::cout << "=======================" << std::endl;
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


void AMSPipe::extract_Hello(AMSPipe::Message& msg, int64_t& version) const {
   if (msg.name != "Hello") {
      throw AMSPipe::Error(AMSPipe::Status::logic_error, "Hello", "",
                           "Hello called on message with name: "+msg.name);
   }

   auto argument = ubjson::read_key(msg.payload);
   if (argument == "version") {
      version = ubjson::read_int(msg.payload);
   } else {
      throw AMSPipe::Error(AMSPipe::Status::unknown_argument, "Hello", argument,
                           "unknown argument "+argument+" in Hello message");
   }
   ubjson::verify_marker(msg.payload, '}');
}


void AMSPipe::extract_SetSystem(AMSPipe::Message& msg,
   std::vector<std::string>& atomSymbols,
   std::vector<double>& coords,
   std::vector<double>& latticeVectors,
   double& totalCharge,
   std::vector<int64_t>& bonds,
   std::vector<double>& bondOrders,
   std::vector<std::string>& atomicInfo
) const {
   if (msg.name != "SetSystem") {
      throw AMSPipe::Error(AMSPipe::Status::logic_error, "SetSystem", "",
                           "SetSystem called on message with name: "+msg.name);
   }

   atomSymbols.clear();
   coords.clear();
   latticeVectors.clear();
   totalCharge = 0.0;
   bonds.clear();
   bondOrders.clear();
   atomicInfo.clear();

   std::vector<int64_t> atomSymbols_dim = {-1};
   std::vector<int64_t> coords_dim = {-1,-1};
   std::vector<int64_t> latticeVectors_dim = {-1,-1};
   std::vector<int64_t> bonds_dim = {2,0};
   std::vector<int64_t> bondOrders_dim = {0};
   std::vector<int64_t> atomicInfo_dim = {0};

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
         totalCharge = ubjson::read_real(msg.payload);

      } else if (argument == "bonds") {
         bonds = ubjson::read_int_array(msg.payload);
      } else if (argument == "bonds_dim_") {
         bonds_dim = ubjson::read_int_array(msg.payload);

      } else if (argument == "bondOrders") {
         bondOrders = ubjson::read_real_array(msg.payload);
      } else if (argument == "bondOrders_dim_") {
         bondOrders_dim = ubjson::read_int_array(msg.payload);

      } else if (argument == "atomicInfo") {
         atomicInfo = ubjson::read_string_array(msg.payload);
      } else if (argument == "atomicInfo_dim_") {
         atomicInfo_dim = ubjson::read_int_array(msg.payload);

      } else {
         throw AMSPipe::Error(AMSPipe::Status::unknown_argument, "SetSystem", argument,
                              "unknown argument "+argument+" in SetSystem message");
      }
   }
   ubjson::verify_marker(msg.payload, '}');

   // Checks on the completeness and consistency of the transferred data
   if (atomSymbols.empty()) {
      throw AMSPipe::Error(AMSPipe::Status::logic_error, "SetSystem", "atomSymbols", "no atoms in SetSystem message");
   }
   if (atomSymbols_dim.size() != 1 || atomSymbols_dim[0] != atomSymbols.size()) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetSystem", "atomSymbols",
                           "size of atomSymbols is inconsistent with atomSymbols_dim_ in SetSystem message");
   }
   if (coords.size() != 3*atomSymbols.size()) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetSystem", "coords",
                           "sizes of atomSymbols and coords inconsistent in SetSystem message");
   }
   if (coords_dim.size() != 2 || coords_dim[0] != 3 || coords_dim[1] != atomSymbols.size()) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetSystem", "coords",
                           "size of coords is inconsistent with coords_dim_ in SetSystem message");
   }
   if (latticeVectors_dim.size() != 2 || latticeVectors_dim[0] * latticeVectors_dim[1] != latticeVectors.size()) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetSystem", "latticeVectors",
                           "size of latticeVectors is inconsistent with latticeVectors_dim_ in SetSystem message");
   }
   if (latticeVectors.size() != 0 && latticeVectors.size() != 3 && latticeVectors.size() != 6 && latticeVectors.size() != 9) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetSystem", "latticeVectors",
                           "unexpected size of latticeVectors in SetSystem message");
   }
   if (bonds_dim.size() != 2 || bonds_dim[0] != 2 || bonds_dim[1] < 0) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetSystem", "bonds_dim_",
                           "unexpected bonds_dim_ in SetSystem message: should be 2 x nBonds");
   }
   if (bonds_dim[0]*bonds_dim[1] != bonds.size()) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetSystem", "bonds",
                           "size of bonds is inconsistent with bonds_dim_ in SetSystem message");
   }
   if (bondOrders_dim.size() != 1 || bondOrders_dim[0] < 0) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetSystem", "bondOrders_dim_",
                           "unexpected bondOrders_dim_ in SetSystem message");
   }
   if (bonds.size() != 2*bondOrders.size()) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetSystem", "bondOrders",
                           "size of bondOrders is inconsistent with size of bonds in SetSystem message");
   }
   if (bondOrders_dim[0] != bondOrders.size()) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetSystem", "bondOrders",
                           "size of bondOrders is inconsistent with bondOrders_dim_ in SetSystem message");
   }
   if (atomicInfo_dim.size() != 1) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetSystem", "atomicInfo_dim_",
                           "unexpected atomicInfo_dim_ in SetSystem message");
   }
   if (atomicInfo_dim[0] > 0 && (atomicInfo_dim[0] != atomSymbols.size() || atomicInfo_dim[0] != atomicInfo.size())) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetSystem", "atomicInfo",
                           "size of atomicInfo is inconsistent with number of atoms in SetSystem message");
   }
}


void AMSPipe::extract_SetCoords(AMSPipe::Message& msg, double* coords) const {
   if (msg.name != "SetCoords") {
      throw AMSPipe::Error(AMSPipe::Status::logic_error, "SetCoords", "",
                           "SetCoords called on message with name: "+msg.name);
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
         throw AMSPipe::Error(AMSPipe::Status::unknown_argument, "SetCoords", argument,
                              "unknown argument "+argument+" in SetCoords message");
      }
   }
   ubjson::verify_marker(msg.payload, '}');

   // Checks on the completeness and consistency of the transferred data
   if (coords_dim.size() != 2 || coords_dim[0] * coords_dim[1] != newcoords.size()) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetCoords", "coords",
                           "size of coords is inconsistent with coords_dim_ in SetCoords message");
   }

   for (size_t i = 0; i < newcoords.size(); ++i) coords[i] = newcoords[i]; // TODO: remove and write directly to coords
}


void AMSPipe::extract_SetLattice(AMSPipe::Message& msg, std::vector<double>& vectors) const {
   if (msg.name != "SetLattice") {
      throw AMSPipe::Error(AMSPipe::Status::logic_error, "SetLattice", "",
                           "SetLattice called on message with name: "+msg.name);
   }

   vectors.clear();
   std::vector<int64_t> vectors_dim = {-1,-1};

   while (ubjson::peek(msg.payload) != '}'){
      auto argument = ubjson::read_key(msg.payload);

      if (argument == "vectors") {
         vectors = ubjson::read_real_array(msg.payload);

      } else if (argument == "vectors_dim_") {
         vectors_dim = ubjson::read_int_array(msg.payload);

      } else {
         throw AMSPipe::Error(AMSPipe::Status::unknown_argument, "SetLattice", argument,
                              "unknown argument "+argument+" in SetLattice message");
      }
   }
   ubjson::verify_marker(msg.payload, '}');

   // Checks on the completeness and consistency of the transferred data
   if (vectors_dim.size() != 2 || vectors_dim[0] * vectors_dim[1] != vectors.size()) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetLattice", "vectors",
                           "size of vectors is inconsistent with vectors_dim_ in SetLattice message");
   }
   if (vectors.size() != 0 && vectors.size() != 3 && vectors.size() != 6 && vectors.size() != 9) {
      throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "SetLattice", "vectors",
                           "unexpected size of vectors in SetLattice message");
   }
}


void AMSPipe::extract_Solve(AMSPipe::Message& msg,
   AMSPipe::SolveRequest& request,
   bool& keepResults,
   std::string& prevTitle
) const {
   if (msg.name != "Solve") {
      throw AMSPipe::Error(AMSPipe::Status::logic_error, "Solve", "",
                           "Solve called on message with name: "+msg.name);
   }

   while (ubjson::peek(msg.payload) != '}') {
      auto argument = ubjson::read_key(msg.payload);

      if (argument == "request") {
         ubjson::verify_marker(msg.payload, '{');
         while (ubjson::peek(msg.payload) != '}') {
            auto field = ubjson::read_key(msg.payload);
            if (field == "title") {
               request.title = ubjson::read_string(msg.payload);
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
            } else if (field == "other") {
               request.other = ubjson::read_bool(msg.payload);
            } else {
               if (ubjson::peek(msg.payload) == 'F') {
                  // The worker MAY raise an unknown argument error if an unknown Boolean is set to False ...
                  // ... but we just extract and ignore it ;-) ...
                  ubjson::read_bool(msg.payload);
               } else {
                  throw AMSPipe::Error(AMSPipe::Status::unknown_argument, "Solve", field,
                                       "unknown argument "+field+" in Solve message");
               }
            }
         }
         ubjson::verify_marker(msg.payload, '}');

         if (request.title.empty()) {
            throw AMSPipe::Error(AMSPipe::Status::invalid_argument, "Solve", "title",
                                 "title may not be empty in Solve message");
         }

      } else if (argument == "keepResults") {
         keepResults = ubjson::read_bool(msg.payload);

      } else if (argument == "prevTitle") {
         prevTitle = ubjson::read_string(msg.payload);

      } else {
         throw AMSPipe::Error(AMSPipe::Status::unknown_argument, "Solve", argument,
                              "unknown argument "+argument+" in Solve message");
      }

   }
   ubjson::verify_marker(msg.payload, '}');

}


void AMSPipe::extract_DeleteResults(AMSPipe::Message& msg, std::string& title) const {
   if (msg.name != "DeleteResults") {
      throw AMSPipe::Error(AMSPipe::Status::logic_error, "DeleteResults", "",
                           "DeleteResults called on message with name: "+msg.name);
   }

   auto argument = ubjson::read_key(msg.payload);
   if (argument == "title") {
      title = ubjson::read_string(msg.payload);
   } else {
      throw AMSPipe::Error(AMSPipe::Status::unknown_argument, "DeleteResults", argument,
                           "unknown argument "+argument+" in DeleteResults message");
   }
   ubjson::verify_marker(msg.payload, '}');
}


// ===== reply pipe ============================================================================================================

void AMSPipe::send_return(AMSPipe::Status status, const std::string& method, const std::string& argument, const std::string& message) {
   ubjson::outstream &buf = *(sendBuffer.get());

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
   if (!message.empty()) {
      ubjson::write_key(buf, "message");
      ubjson::write_string(buf, message);
   }
   buf << '}' << '}';

   send(buf);
}


void write_real_2darray_with_dim(ubjson::outstream& os, const std::string& key, const double* arr, const int32_t dim[2]) {
   ubjson::write_key(os, key);
   ubjson::write_real_array(os, arr, dim[0]*dim[1]);
   ubjson::write_key(os, key+"_dim_");
   os << '[';
   ubjson::write_int(os, dim[0]);
   ubjson::write_int(os, dim[1]);
   os << ']';
}


void AMSPipe::send_results(const AMSPipe::Results& results) {
   ubjson::outstream &buf = *(sendBuffer.get());

   buf << '{';
   ubjson::write_key(buf, "results");
   buf << '{';

   // the energy is the only result we *always* produce
   ubjson::write_key(buf, "energy");
   ubjson::write_real(buf, results.energy);

   // all other results are optional
   if (results.gradients)       write_real_2darray_with_dim(buf, "gradients",       results.gradients,       results.gradients_dim);
   if (results.stressTensor)    write_real_2darray_with_dim(buf, "stressTensor",    results.stressTensor,    results.stressTensor_dim);
   if (results.elasticTensor)   write_real_2darray_with_dim(buf, "elasticTensor",   results.elasticTensor,   results.elasticTensor_dim);
   if (results.hessian)         write_real_2darray_with_dim(buf, "hessian",         results.hessian,         results.hessian_dim);
   if (results.dipoleMoment)    write_real_2darray_with_dim(buf, "dipoleMoment",    results.dipoleMoment,    results.dipoleMoment_dim);
   if (results.dipoleGradients) write_real_2darray_with_dim(buf, "dipoleGradients", results.dipoleGradients, results.dipoleGradients_dim);

   buf << '}' << '}';

   send(buf);
}


void AMSPipe::send(ubjson::outstream& buf) {
   const auto &tmp = buf.buffer();

   // DEBUG
   //std::cout << "==REPLY================" << std::endl;
   //for (char c: tmp) std::cout << int(c) << " " << c << std::endl;
   //std::cout << "=======================" << std::endl;
   // DEBUG

   int32_t msgsize = tmp.size();
   if (std::fwrite(&msgsize, sizeof(msgsize), 1, reply_pipe) != 1)
      throw AMSPipe::Error(AMSPipe::Status::runtime_error, "", "", "Could not write size of outgoing message to reply pipe");
   for (size_t pos = 0, nwritten = 0; pos < (size_t) msgsize; pos += nwritten) {
      nwritten = fwrite(&tmp[pos], sizeof(tmp[0]), msgsize - pos, reply_pipe);
      if (nwritten == 0)
         throw AMSPipe::Error(AMSPipe::Status::runtime_error, "", "", "Could not write outgoing message to reply pipe");
   }
   if (std::fflush(reply_pipe) != 0)
      throw AMSPipe::Error(AMSPipe::Status::runtime_error, "", "", "Could not flush reply pipe");
   buf.clear();
}
