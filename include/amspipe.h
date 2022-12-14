// C interface to amspipe library

#ifndef AMSPIPE_H_INCLUDED
#define AMSPIPE_H_INCLUDED

#include <stdint.h>

typedef enum {
   AMSPIPE_STATUS_SUCCESS          = 0,
   AMSPIPE_STATUS_DECODE_ERROR     = 1,
   AMSPIPE_STATUS_LOGIC_ERROR      = 2,
   AMSPIPE_STATUS_RUNTIME_ERROR    = 3,
   AMSPIPE_STATUS_UNKNOWN_VERSION  = 4,
   AMSPIPE_STATUS_UNKNOWN_METHOD   = 5,
   AMSPIPE_STATUS_UNKNOWN_ARGUMENT = 6,
   AMSPIPE_STATUS_INVALID_ARGUMENT = 7
} amspipe_status_t;

typedef struct {
   const char* name;
   void* p;
} amspipe_message_t;

// type-safe handles for the call and reply pipes
typedef struct { void* p; } amscallpipe_t;
typedef struct { void* p; } amsreplypipe_t;

#ifdef __cplusplus
extern "C" {
#endif

// functions for the call pipe
amscallpipe_t new_amscallpipe(const char* filename);
void delete_amscallpipe(amscallpipe_t* cp);
void amscallpipe_receive(amscallpipe_t cp, amspipe_message_t* message);
void amscallpipe_extract_Hello(amscallpipe_t cp, amspipe_message_t message, int64_t* version);
void amscallpipe_extract_SetSystem(amscallpipe_t cp, amspipe_message_t message,
   int64_t* numAtoms,
   char***  atomSymbols,
   double** coords,
   int64_t* numLatVecs,
   double** latticeVectors,
   double*  totalCharge
);

// functions for the reply pipe
amsreplypipe_t new_amsreplypipe(const char* filename);
void delete_amsreplypipe(amsreplypipe_t* cp);
void amsreplypipe_send_return(amsreplypipe_t rp,
   amspipe_status_t status,
   const char* method,
   const char* argument,
   const char* message
);

#ifdef __cplusplus
}
#endif

#endif // AMSPIPE_H_INCLUDED
