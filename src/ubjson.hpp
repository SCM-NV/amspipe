#ifndef HPP_INCLUDED
#define HPP_INCLUDED

#include <stdint.h>
#include <iostream>
#include <vector>

namespace ubjson {

   // Error to throw in case of these methods fail
   class Error : public std::runtime_error { using std::runtime_error::runtime_error; };


   // Methods for reading UBJSON from a std::istream

   void verify_marker(std::istream& is, char marker);
   char peek(std::istream& is);

   char read_char(std::istream& is);
   bool read_bool(std::istream& is);
   int64_t read_int(std::istream& is);
   double read_real(std::istream& is);
   std::string read_key(std::istream& is); // like a string, but without preceding S marker
   std::string read_string(std::istream& is);

   std::vector<int64_t> read_int_array(std::istream& is);
   std::vector<double> read_real_array(std::istream& is);
   std::vector<std::string> read_string_array(std::istream& is);


   // Methods for writing UBJSON to a std::ostream

   void write_int(std::ostream& os, int8_t i);
   void write_int(std::ostream& os, uint8_t U);
   void write_int(std::ostream& os, int16_t I);
   void write_int(std::ostream& os, int32_t l);
   void write_int(std::ostream& os, int64_t L);
   void write_real(std::ostream& os, float d);
   void write_real(std::ostream& os, double D);
   void write_key(std::ostream& os, const std::string& key); // like a string, but without preceding S marker
   void write_string(std::ostream& os, const std::string& str);

};


#endif // HPP_INCLUDED
