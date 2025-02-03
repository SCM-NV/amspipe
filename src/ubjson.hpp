#ifndef UBJSON_HPP_INCLUDED
#define UBJSON_HPP_INCLUDED

#include <cstring>
#include <stdint.h>
#include <iostream>
#include <string>
#include <vector>

#include "amspipe.hpp"

namespace ubjson {

   // define ubjson::Error as AMSPipe::Error with decode_error status
   class Error : public AMSPipe::Error {
      public:
         Error(const std::string& message)
         :  AMSPipe::Error(AMSPipe::Status::decode_error, "", "", message) {}
   };

   class outstream;

   // Methods for reading UBJSON from a std::istream

   void verify_marker(std::istream& is, char marker);
   char peek(std::istream& is);

   char read_char(std::istream& is);
   bool read_bool(std::istream& is);
   int64_t read_int(std::istream& is);
   int64_t read_int(std::istream& is, char marker);
   double read_real(std::istream& is);
   double read_real(std::istream& is, char marker);
   std::string read_key(std::istream& is); // like a string, but without preceding S marker
   std::string read_string(std::istream& is);

   std::vector<int64_t> read_int_array(std::istream& is);
   std::vector<double> read_real_array(std::istream& is);
   std::vector<std::string> read_string_array(std::istream& is);


   // Methods for writing UBJSON to a std::ostream

   void write_int(outstream& os, int8_t i);
   void write_int(outstream& os, uint8_t U);
   void write_int(outstream& os, int16_t I);
   void write_int(outstream& os, int32_t l);
   //void write_int(outstream& os, int64_t L);
   void write_real(outstream& os, float d);
   void write_real(outstream& os, double D);
   void write_key(outstream& os, const std::string& key); // like a string, but without preceding S marker
   void write_string(outstream& os, const std::string& str);

   void write_real_array(outstream& os, const double* arr, int32_t n);

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

inline char bswap(char c) { return c; }
inline int8_t bswap(int8_t i) { return i; }
inline int16_t bswap(int16_t s) { return bswap_16(s); }
inline int32_t bswap(int32_t l) { return bswap_32(l); }
inline int64_t bswap(int64_t l) { return bswap_64(l); }

inline double bswap(double x) {
   int64_t tmp;
   double res;
   memcpy(&tmp, &x, sizeof(double));
   tmp = bswap(tmp);
   memcpy(&res, &tmp, sizeof(double));
   return res;
}
inline float bswap(float x) {
   int32_t tmp;
   float res;
   memcpy(&tmp, &x, sizeof(float));
   tmp = bswap(tmp);
   memcpy(&res, &tmp, sizeof(float));
   return res;
}

   class outstream {
      public:
         const std::vector<char>& buffer() const { return buf; };
         void clear() { buf.clear(); }

         template<typename T> outstream& operator<<(T x) { write(x); return *this; }

         void write(char c) { buf.push_back(c); }
         void write(const std::string &s) { write(s.data(), s.size()); }

         template<typename T> void write(T x) {
            write(&x, 1);
         }

         template<typename T> void write(T* x, size_t nelem) {
            size_t pos = buf.size(), elem_size = sizeof(T);
            buf.resize(pos + elem_size * nelem);
            for (size_t i = 0; i < nelem; i++, pos += elem_size) {
               T tmp = bswap(x[i]);
               memcpy(&(buf[pos]), &tmp, elem_size);
            }
         }

      private:
         std::vector<char> buf;
   };
};


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
      return bswap(I);
   } else if (marker == 'l') {
      int32_t l;
      is.read(reinterpret_cast<char*>(&l), sizeof(l));
      return bswap(l);
   } else if (marker == 'L') {
      int64_t L;
      is.read(reinterpret_cast<char*>(&L), sizeof(L));
      return bswap(L);
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
      return bswap(d);
   } else if (marker == 'D') {
      double D;
      is.read(reinterpret_cast<char*>(&D), sizeof(D));
      return bswap(D);
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
      for (double& D: v) D = bswap(D);
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


void ubjson::write_int(outstream& os, int8_t i) {
   os << 'i' << i;
}


void ubjson::write_int(outstream& os, uint8_t U) {
   os << 'U' << U;
}


void ubjson::write_int(outstream& os, int16_t I) {
   os << 'I' << I;
}


void ubjson::write_int(outstream& os, int32_t l) {
   os << 'l' << l;
}


//void ubjson::write_int(outstream& os, int64_t L) {
//   os << 'L' << L;
//}


void ubjson::write_real(outstream& os, float d) {
   os << 'd' << d;
}


void ubjson::write_real(outstream& os, double D) {
   os << 'D' << D;
}


void ubjson::write_key(outstream& os, const std::string& key) {
   os << 'i';
   int8_t keylen = key.size();
   os.write(keylen);
   os.write(key);
}


void ubjson::write_string(outstream& os, const std::string& str) {
   os << 'S';
   int32_t strlen = str.size();
   ubjson::write_int(os, strlen);
   os.write(str);
}


void ubjson::write_real_array(outstream& os, const double* arr, int32_t n) {
   os << '[' << '$' << 'D' << '#';
   ubjson::write_int(os, n);
   os.write(arr, n);
}

#endif // UBJSON_HPP_INCLUDED
