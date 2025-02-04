// C++ interface to amspipe library

#ifndef AMSPIPE_HPP_INCLUDED
#define AMSPIPE_HPP_INCLUDED

#include <memory>
#include <string>
#include <sstream>
#include <vector>
#include <stdexcept>
#include <cstdio>

namespace ubjson {
   class outstream;
};

class AMSPipe {
   public:

   enum class Status : int8_t {
      success          = 0,
      decode_error     = 1,
      logic_error      = 2,
      runtime_error    = 3,
      unknown_version  = 4,
      unknown_method   = 5,
      unknown_argument = 6,
      invalid_argument = 7
   };

   class Error : public std::runtime_error {
      public:
         Status      status;
         std::string method;
         std::string argument;
         // as the "message" we just use the .what() of the base exception

         Error(Status status, const std::string& method, const std::string& argument, const std::string& message)
         :  std::runtime_error(message),
            status(status),
            method(method),
            argument(argument)
         {}

   };

   struct Message {
      std::string name;
      std::stringstream payload;
   };

   struct SolveRequest {
      std::string title;
      bool quiet = false;
      bool gradients = false;
      bool stressTensor = false;
      bool elasticTensor = false;
      bool hessian = false;
      bool dipoleMoment = false;
      bool dipoleGradients = false;
      bool other = false;
   };

   struct Results {
      std::vector<std::string> messages;
      double energy;
      double* gradients = nullptr;
      int32_t gradients_dim[2] = {0,0};
      double* stressTensor = nullptr;
      int32_t stressTensor_dim[2] = {0,0};
      double* elasticTensor = nullptr;
      int32_t elasticTensor_dim[2] = {0,0};
      double* hessian = nullptr;
      int32_t hessian_dim[2] = {0,0};
      double* dipoleMoment = nullptr;
      int32_t dipoleMoment_dim[2] = {0,0};
      double* dipoleGradients = nullptr;
      int32_t dipoleGradients_dim[2] = {0,0};
   };

      AMSPipe();
      ~AMSPipe() noexcept;

      // Method to receive a generic message on the call pipe:
      AMSPipe::Message receive();

      // Methods to extract the payload of the specific messages:

      void extract_Hello(AMSPipe::Message& msg, int64_t& version) const;

      void extract_SetSystem(AMSPipe::Message& msg,
         std::vector<std::string>& atomSymbols,
         std::vector<double>& coords,
         std::vector<double>& latticeVectors,
         double& totalCharge,
         std::vector<int64_t>& bonds,
         std::vector<double>& bondOrders,
         std::vector<std::string>& atomicInfo
      ) const;

      void extract_SetCoords(AMSPipe::Message& msg, double* coords) const;

      void extract_SetLattice(AMSPipe::Message& msg, std::vector<double>& vectors) const;

      void extract_Solve(AMSPipe::Message& msg,
         AMSPipe::SolveRequest& request,
         bool& keepResults,
         std::string& prevTitle
      ) const;

      void extract_DeleteResults(AMSPipe::Message& msg, std::string& title) const;

      // Methods to send specific messages:

      void send_return(
         AMSPipe::Status status,
         const std::string& method="",
         const std::string& argument="",
         const std::string& message=""
      );

      void send_results(const AMSPipe::Results& results);

   private:
      std::FILE *call_pipe, *reply_pipe;
      std::unique_ptr<ubjson::outstream> sendBuffer;
      void send(ubjson::outstream& buf);

};


#endif // AMSPIPE_HPP_INCLUDED
