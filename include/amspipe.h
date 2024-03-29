// C interface to amspipe library

#ifndef AMSPIPE_H_INCLUDED
#define AMSPIPE_H_INCLUDED

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif


// ===== AMSPipe::Status =========================================================================================================

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


// ===== AMSPipe::Error ==========================================================================================================

typedef struct {
   amspipe_status_t status;
   char* method;
   char* argument;
   char* message;
} amspipe_error_t;
void delete_amspipe_error(amspipe_error_t** error);


// ===== AMSPipe::Message ========================================================================================================

typedef struct {
   const char* name;
   void* p;
} amspipe_message_t;
amspipe_message_t new_amspipe_message(void);
void delete_amspipe_message(amspipe_message_t* message);


// ===== AMSPipe::SolveRequest ===================================================================================================

typedef struct {
   char* title;
   bool quiet;
   bool gradients;
   bool stressTensor;
   bool elasticTensor;
   bool hessian;
   bool dipoleMoment;
   bool dipoleGradients;
   bool other;
} amspipe_solverequest_t;
amspipe_solverequest_t new_amspipe_solverequest(void);
void delete_amspipe_solverequest(amspipe_solverequest_t* request);


// ===== AMSPipe::Results ========================================================================================================

typedef struct {
   int64_t numMessages;
   char**  messages;
   double  energy;
   double* gradients;
   int32_t gradients_dim[2];
   double* stressTensor;
   int32_t stressTensor_dim[2];
   double* elasticTensor;
   int32_t elasticTensor_dim[2];
   double* hessian;
   int32_t hessian_dim[2];
   double* dipoleMoment;
   int32_t dipoleMoment_dim[2];
   double* dipoleGradients;
   int32_t dipoleGradients_dim[2];
} amspipe_results_t;
amspipe_results_t new_amspipe_results(void);
void delete_amspipe_results(amspipe_results_t* results);


// ===== AMSPipe =============================================================================================================

typedef struct { void* p; } amspipe_t;
amspipe_t new_amspipe(void);
void delete_amspipe(amspipe_t* cp);

// ===== call pipe =============================================================================================================

void amspipe_receive(amspipe_t cp, amspipe_message_t* message);

void amspipe_extract_Hello(amspipe_t cp, amspipe_message_t message, amspipe_error_t** error, int64_t* version);

void amspipe_extract_SetSystem(amspipe_t cp, amspipe_message_t message, amspipe_error_t** error,
   int64_t* numAtoms,
   char***  atomSymbols,
   double** coords,
   int64_t* numLatVecs,
   double** latticeVectors,
   double*  totalCharge,
   int64_t*  numBonds,
   int64_t** bonds,
   double**  bondOrders,
   char***   atomicInfo
);

void amspipe_extract_SetCoords(amspipe_t cp, amspipe_message_t message, amspipe_error_t** error, double* coords);

void amspipe_extract_SetLattice(amspipe_t cp, amspipe_message_t message, amspipe_error_t** error,
                                int64_t* numLatVecs, double** latticeVectors);

void amspipe_extract_Solve(amspipe_t cp, amspipe_message_t message, amspipe_error_t** error,
   amspipe_solverequest_t* request,
   bool* keepResults,
   char** prevTitle
);

void amspipe_extract_DeleteResults(amspipe_t cp, amspipe_message_t message, amspipe_error_t** error, char** title);


// ===== reply pipe ============================================================================================================

void amspipe_send_return(amspipe_t rp,
   amspipe_status_t status,
   const char* method,
   const char* argument,
   const char* message
);

void amspipe_send_results(amspipe_t rp, const amspipe_results_t* results);


#ifdef __cplusplus
}
#endif

#endif // AMSPIPE_H_INCLUDED
