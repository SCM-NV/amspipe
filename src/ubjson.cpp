#include "ubjson.hpp"


// ===== Methods for converting between big endian (UBJSON) and native endian ====================================================

// TODO: Will need some fixing on big endian platforms, where nothing needs to be done ...

#if defined(_MSC_VER) || defined(__MINGW32__)
  #include <stdlib.h>
  #define bswap_16(x) _byteswap_ushort(x)
  #define bswap_32(x) _byteswap_ulong(x)
  #define bswap_64(x) _byteswap_uint64(x)
#elif defined(__APPLE__)
  #include <libkern/OSByteOrder.h>
  #define bswap_16(x) OSSwapInt16(x)
  #define bswap_32(x) OSSwapInt32(x)
  #define bswap_64(x) OSSwapInt64(x)
#else
  #include <byteswap.h>  // bswap_16 bswap_32 bswap_64
#endif

inline double bswap_64f(double x) {
   uint64_t i = *reinterpret_cast<uint64_t*>(&x);
   i = bswap_64(i);
   return *reinterpret_cast<double*>(&i);
}

inline float bswap_32f(float x) {
   uint32_t i = *reinterpret_cast<uint32_t*>(&x);
   i = bswap_32(i);
   return *reinterpret_cast<float*>(&i);
}


// ===== Methods for reading UBJSON from a std::istream ==========================================================================


void ubjson::verify_marker(std::istream& is, char marker) {
   // Extracts one character from the stream and checks that is is the marker we expect.
   char c;
   is.read(&c, sizeof(c));
   if (c != marker) throw ubjson::Error(std::string("unexpected marker: found ")+c+std::string(", expected ")+marker);
}


char ubjson::peek(std::istream& is) {
   return static_cast<char>(is.peek());
}


char ubjson::read_char(std::istream& is) {
   char c;
   is.read(&c, sizeof(c));
   return c;
}


bool ubjson::read_bool(std::istream& is) {
   char b = read_char(is);
   if (b == 'T') {
      return true;
   } else if (b == 'F') {
      return false;
   } else {
      throw ubjson::Error(std::string("unexpected marker for bool: ")+b);
   }
}


int64_t ubjson::read_int(std::istream& is) {
   // Extracts one integer from the stream after determining its size
   char marker;
   is.read(&marker, sizeof(marker));
   return ubjson::read_int(is, marker);
}


int64_t ubjson::read_int(std::istream& is, char marker) {
   // Extracts one integer from the stream given its type marker
   if (marker == 'i') {
      int8_t i;
      is.read(reinterpret_cast<char*>(&i), sizeof(i));
      return i;
   } else if (marker == 'U') {
      uint8_t U;
      is.read(reinterpret_cast<char*>(&U), sizeof(U));
      return U;
   } else if (marker == 'I') {
      int16_t I;
      is.read(reinterpret_cast<char*>(&I), sizeof(I));
      return bswap_16(I);
   } else if (marker == 'l') {
      int32_t l;
      is.read(reinterpret_cast<char*>(&l), sizeof(l));
      return bswap_32(l);
   } else if (marker == 'L') {
      int64_t L;
      is.read(reinterpret_cast<char*>(&L), sizeof(L));
      return bswap_64(L);
   } else {
      throw ubjson::Error(std::string("unexpected integer type: '")+marker);
   }
}


double ubjson::read_real(std::istream& is) {
   // Extracts one real from the stream after determining its size
   char marker;
   is.read(&marker, sizeof(marker));
   return ubjson::read_real(is, marker);
}


double ubjson::read_real(std::istream& is, char marker) {
   // Extracts one real from the stream given its type marker
   if (marker == 'd') {
      float d;
      is.read(reinterpret_cast<char*>(&d), sizeof(d));
      return bswap_32f(d);
   } else if (marker == 'D') {
      double D;
      is.read(reinterpret_cast<char*>(&D), sizeof(D));
      return bswap_64f(D);
   } else {
      throw ubjson::Error(std::string("unexpected real type: ")+marker);
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
      throw ubjson::Error("TODO: implement reading of optimized string arrays with just a count");

   } else if (m == 'S') {
      // Unoptimized storage
      throw ubjson::Error("TODO: implement reading of unoptimized string arrays");
      ubjson::verify_marker(is, ']');

   } else {
      throw ubjson::Error(std::string("unexpected marker at the start of a string array: ")+m);
   }
}


std::vector<int64_t> ubjson::read_int_array(std::istream& is) {
   ubjson::verify_marker(is, '[');
   auto m = ubjson::peek(is);

   if (m == '$') {
      // Optimized storage with type and count
      ubjson::verify_marker(is, '$');
      auto im = ubjson::read_char(is);
      ubjson::verify_marker(is, '#');
      auto n = ubjson::read_int(is);
      std::vector<int64_t> v(n);
      for (size_t i = 0; i<n; ++i) {
         v[i] = ubjson::read_int(is, im);
      }
      return v;

   } else if (m == '#') {
      // Optimized storage with count
      throw ubjson::Error("TODO: implement reading of optimized int arrays with just a count");

   } else if (m == 'i' || m == 'U' || m == 'I' || m == 'l' || m == 'L') {
      // Unoptimized storage
      throw ubjson::Error("TODO: implement reading of unoptimized int arrays");
      ubjson::verify_marker(is, ']');

   } else {
      throw ubjson::Error(std::string("unexpected marker at the start of an int array: ")+m);
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
      for (double& D: v) D = bswap_64f(D);
      return v;

   } else if (m == '#') {
      // Optimized storage with count
      throw ubjson::Error("TODO: implement reading of optimized real arrays with just a count");

   } else if (m == 'd' || m == 'D') {
      // Unoptimized storage
      throw ubjson::Error("TODO: implement reading of unoptimized real arrays");

   } else {
      throw ubjson::Error(std::string("unexpected marker at the start of a real array: ")+m);
   }
}


// ===== Methods for writing UBJSON to a std::ostream ============================================================================


void ubjson::write_int(std::ostream& os, int8_t i) {
   os << 'i';
   os.write(reinterpret_cast<const char*>(&i), sizeof(i));
}


void ubjson::write_int(std::ostream& os, uint8_t U) {
   os << 'U';
   os.write(reinterpret_cast<const char*>(&U), sizeof(U));
}


void ubjson::write_int(std::ostream& os, int16_t I) {
   os << 'I';
   I = bswap_16(I);
   os.write(reinterpret_cast<const char*>(&I), sizeof(I));
}


void ubjson::write_int(std::ostream& os, int32_t l) {
   os << 'l';
   l = bswap_32(l);
   os.write(reinterpret_cast<const char*>(&l), sizeof(l));
}


//void ubjson::write_int(std::ostream& os, int64_t L) {
//   os << 'L';
//   L = bswap_64(L);
//   os.write(reinterpret_cast<const char*>(&L), sizeof(L));
//}


void ubjson::write_real(std::ostream& os, float d) {
   os << 'd';
   d = bswap_32f(d);
   os.write(reinterpret_cast<const char*>(&d), sizeof(d));
}


void ubjson::write_real(std::ostream& os, double D) {
   os << 'D';
   D = bswap_64f(D);
   os.write(reinterpret_cast<const char*>(&D), sizeof(D));
}


void ubjson::write_key(std::ostream& os, const std::string& key) {
   os << 'i';
   int8_t keylen = key.size();
   os.write(reinterpret_cast<const char*>(&keylen), sizeof(keylen));
   os.write(&key[0], keylen);
}


void ubjson::write_string(std::ostream& os, const std::string& str) {
   os << 'S';
   int32_t strlen = str.size();
   ubjson::write_int(os, strlen);
   os.write(&str[0], strlen);
}


void ubjson::write_real_array(std::ostream& os, const double* arr, int32_t n) {
   os << '[' << '$' << 'D' << '#';
   ubjson::write_int(os, n);
   for (int32_t i=0; i < n; ++i) {
      double D = bswap_64f(arr[i]);
      os.write(reinterpret_cast<const char*>(&D), sizeof(D));
   }
}
