#include "amspipe.hpp"

#include <ios>
#include <sstream>
#include <iostream>


// ===== AMSCallPipe =============================================================================================================

AMSCallPipe::AMSCallPipe(const std::string& filename) {
   pipe.exceptions(std::ofstream::failbit | std::ofstream::badbit);
   pipe.open(filename, std::ios::in | std::ios::binary);
}


std::string AMSCallPipe::next_method() {
   // Each message starts with its size. Let's extract that from the pipe first ...
   int32_t msgsize;
   pipe.read(reinterpret_cast<char*>(&msgsize), sizeof(msgsize));

   // After the size follows the UBJSON. It has to start with the opening { marker ...
   verify_marker('{');

   // Next is the string specifying which method was called ...
   return read_string();
}


void AMSCallPipe::method_called() {
   // After calling the method, we extract the closing } marker of the message from the pipe ...
   verify_marker('}');
}


void AMSCallPipe::recv_hello(int64_t* version) {
   verify_marker('{');
   auto argument = read_string();
   if (argument == "version") {
      *version = read_int();
   } else {
      throw AMSPipeError("unknown_argument");
   }
   verify_marker('}');
}


void AMSCallPipe::verify_marker(char marker) {
   char c;
   pipe.read(&c, sizeof(c));
   if (c != marker) throw AMSPipeError("Unexpected marker while reading from AMSCallPipe");
}


int64_t AMSCallPipe::read_int() {
   char marker;
   pipe.read(&marker, sizeof(marker));
   if (marker == 'i') {
      int8_t i;
      pipe.read(reinterpret_cast<char*>(&i), sizeof(i));
      return static_cast<int64_t>(i);
   } else if ('U') {
      uint8_t U;
      pipe.read(reinterpret_cast<char*>(&U), sizeof(U));
      return static_cast<int64_t>(U);
   } else if ('I') {
      int16_t I;
      pipe.read(reinterpret_cast<char*>(&I), sizeof(I));
      return static_cast<int64_t>(I);
   } else if ('l') {
      int32_t l;
      pipe.read(reinterpret_cast<char*>(&l), sizeof(l));
      return static_cast<int64_t>(l);
   } else if ('L') {
      int64_t L;
      pipe.read(reinterpret_cast<char*>(&L), sizeof(L));
      return L;
   } else {
      throw AMSPipeError("Unexpected integer type");
   }
}


std::string AMSCallPipe::read_string() {
   std::string str;
   auto len = read_int();
   str.resize(len);
   pipe.read(&str[0], len);
   return str;
}


// ===== AMSReplyPipe ============================================================================================================

AMSReplyPipe::AMSReplyPipe(const std::string& filename) {
   pipe.exceptions(std::ofstream::failbit | std::ofstream::badbit);
   pipe.open(filename, std::ios::out | std::ios::binary);
}


void write_key(std::ostream& buf, const std::string& key) {
   buf << 'i';
   int8_t keylen = key.size();
   buf.write(reinterpret_cast<const char*>(&keylen), sizeof(keylen));
   buf.write(&key[0], keylen);
}


void write_int(std::ostream& buf, int8_t i) {
   buf << 'i';
   buf.write(reinterpret_cast<const char*>(&i), sizeof(i));
}


void AMSReplyPipe::send_return(Status status, const std::string& method, const std::string& argument, const std::string& message) {
   std::stringstream buf;

   buf << '{';
   write_key(buf, "return");
   buf << '{';
   write_key(buf, "status");
   write_int(buf, static_cast<int8_t>(status));
   buf << '}' << '}';

   auto tmp = buf.str();
   for (auto c : tmp) std::cout << c << " " << int(c) << std::endl;
   int32_t msgsize = tmp.size();
   pipe.write(reinterpret_cast<const char*>(&msgsize), sizeof(msgsize));
   pipe.write(&tmp[0], tmp.size());
   pipe.flush();
}
