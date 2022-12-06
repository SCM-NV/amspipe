#include "amspipe.hpp"

#include <ios>
#include <iostream>

#include "ubjson.hpp"


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
   std::cout << "=====================" << std::endl;
   for (char c: str) std::cout << int(c) << " " << c << std::endl;
   std::cout << "=====================" << std::endl;
   // DEBUG!!!

   msg.payload.str(std::move(str)); // std::move avoids copy starting with C++20

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

   std::vector<int64_t> atomSymbols_dim;
   std::vector<int64_t> coords_dim;
   std::vector<int64_t> latticeVectors_dim;

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


// ===== AMSReplyPipe ============================================================================================================

AMSReplyPipe::AMSReplyPipe(const std::string& filename) {
   pipe.exceptions(std::ofstream::failbit | std::ofstream::badbit);
   pipe.open(filename, std::ios::out | std::ios::binary);
}


void AMSReplyPipe::send_return(Status status, const std::string& method, const std::string& argument, const std::string& message) {
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


void AMSReplyPipe::send(std::stringstream& buf) {
   auto tmp = buf.str();
   int32_t msgsize = tmp.size();
   pipe.write(reinterpret_cast<const char*>(&msgsize), sizeof(msgsize));
   pipe.write(&tmp[0], tmp.size());
   pipe.flush();
}
