#include "ubjson.hpp"


// ===== Methods for reading UBJSON from a std::istream ==========================================================================


void ubjson::verify_marker(std::istream& is, char marker) {
   // Extracts one character from the stream and checks that is is the marker we expect.
   char c;
   is.read(&c, sizeof(c));
   if (c != marker) throw ubjson::Error("Unexpected marker");
}


char ubjson::peek(std::istream& is) {
   return static_cast<char>(is.peek());
}


char ubjson::read_char(std::istream& is) {
   char c;
   is.read(&c, sizeof(c));
   return c;
}


int64_t ubjson::read_int(std::istream& is) {
   // Extracts one integer from the stream
   char marker;
   is.read(&marker, sizeof(marker));
   if (marker == 'i') {
      int8_t i;
      is.read(reinterpret_cast<char*>(&i), sizeof(i));
      return static_cast<int64_t>(i);
   } else if ('U') {
      uint8_t U;
      is.read(reinterpret_cast<char*>(&U), sizeof(U));
      return static_cast<int64_t>(U);
   } else if ('I') {
      int16_t I;
      is.read(reinterpret_cast<char*>(&I), sizeof(I));
      return static_cast<int64_t>(I);
   } else if ('l') {
      int32_t l;
      is.read(reinterpret_cast<char*>(&l), sizeof(l));
      return static_cast<int64_t>(l);
   } else if ('L') {
      int64_t L;
      is.read(reinterpret_cast<char*>(&L), sizeof(L));
      return L;
   } else {
      throw ubjson::Error("Unexpected integer type");
   }
}


double ubjson::read_real(std::istream& is) {
   char marker;
   is.read(&marker, sizeof(marker));
   if (marker == 'd') {
      float d;
      is.read(reinterpret_cast<char*>(&d), sizeof(d));
      return static_cast<double>(d);
   } else if ('D') {
      double D;
      is.read(reinterpret_cast<char*>(&D), sizeof(D));
      return D;
   } else {
      throw ubjson::Error("Unexpected real type");
   }
}


std::string ubjson::read_key(std::istream& is) {
   std::string str;
   auto len = ubjson::read_int(is);
   str.resize(len);
   is.read(&str[0], len);
   return str;
}


std::string ubjson::read_string(std::istream& is) {
   ubjson::verify_marker(is, 'S');
   return ubjson::read_key(is);
}


std::vector<std::string> ubjson::read_string_array(std::istream& is) {
   ubjson::verify_marker(is, '[');
   auto m = ubjson::peek(is);

   if (m == '$') {
      // Optimized storage with type and count
      ubjson::verify_marker(is, '$');
      ubjson::verify_marker(is, 'S');
      ubjson::verify_marker(is, '#');
      auto n = ubjson::read_int(is);
      std::vector<std::string> v(n);
      for (size_t i = 0; i<n; ++i) {
         v[i] = ubjson::read_key(is); // no preceding S marker in the array -> read like a key
      }
      return v;

   } else if (m == '#') {
      // Optimized storage with count
      throw ubjson::Error("TODO: Implement reading of optimized string arrays with just a count.");

   } else if (m == 'S') {
      // Unoptimized storage
      throw ubjson::Error("TODO: Implement reading of unoptimized string arrays");
      ubjson::verify_marker(is, ']');

   } else {
      throw ubjson::Error("Unexpected marker at the start of a string array");
   }
}


std::vector<int64_t> ubjson::read_int_array(std::istream& is) {
   ubjson::verify_marker(is, '[');
   auto m = ubjson::peek(is);

   if (m == '$') {
      // Optimized storage with type and count
      ubjson::verify_marker(is, '$');
      ubjson::verify_marker(is, 'i'); // TODO: support other integer types here ...
      ubjson::verify_marker(is, '#');
      auto n = ubjson::read_int(is);
      std::vector<int64_t> v(n);
      for (size_t i = 0; i<n; ++i) {
         int8_t k; // TODO: support other integer types here ...
         is.read(reinterpret_cast<char*>(&k), sizeof(k));
         v[i] = static_cast<int64_t>(k);
      }
      return v;

   } else if (m == '#') {
      // Optimized storage with count
      throw ubjson::Error("TODO: Implement reading of optimized int arrays with just a count");

   } else if (m == 'i' || m == 'U' || m == 'I' || m == 'l' || m == 'L') {
      // Unoptimized storage
      throw ubjson::Error("TODO: Implement reading of unoptimized int arrays");
      ubjson::verify_marker(is, ']');

   } else {
      throw ubjson::Error("Unexpected marker at the start of an int array");
   }
}


std::vector<double> ubjson::read_real_array(std::istream& is) {
   ubjson::verify_marker(is, '[');
   auto m = ubjson::peek(is);

   if (m == '$') {
      // Optimized storage with type and count
      ubjson::verify_marker(is, '$');
      ubjson::verify_marker(is, 'D'); // TODO: support float32 here
      ubjson::verify_marker(is, '#');
      auto n = ubjson::read_int(is);
      std::vector<double> v(n);
      is.read(reinterpret_cast<char*>(v.data()), n*sizeof(v[0])); // TODO: support float32 here
      return v;

   } else if (m == '#') {
      // Optimized storage with count
      throw ubjson::Error("TODO: Implement reading of optimized real arrays with just a count");

   } else if (m == 'd' || m == 'D') {
      // Unoptimized storage
      throw ubjson::Error("TODO: Implement reading of unoptimized real arrays");

   } else {
      throw ubjson::Error("Unexpected marker at the start of a real array");
   }
}


// ===== Methods for writing UBJSON to a std::ostream ============================================================================


void ubjson::write_key(std::ostream& os, const std::string& key) {
   os << 'i';
   int8_t keylen = key.size();
   os.write(reinterpret_cast<const char*>(&keylen), sizeof(keylen));
   os.write(&key[0], keylen);
}


void ubjson::write_string(std::ostream& os, const std::string& str) {
   os << 'S' << 'l';
   int32_t strlen = str.size();
   os.write(reinterpret_cast<const char*>(&strlen), sizeof(strlen));
   os.write(&str[0], strlen);
}


void ubjson::write_int(std::ostream& os, int8_t i) {
   os << 'i';
   os.write(reinterpret_cast<const char*>(&i), sizeof(i));
}

