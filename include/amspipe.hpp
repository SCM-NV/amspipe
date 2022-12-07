#ifndef AMSPIPE_HPP_INCLUDED
#define AMSPIPE_HPP_INCLUDED

#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <span>


namespace AMSPipe {

   class Error : public std::runtime_error { using std::runtime_error::runtime_error; };

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
   };
   std::ostream& operator<<(std::ostream& os, const AMSPipe::SolveRequest& request);

   struct Results {
      std::vector<std::string> messages;
      double energy;
      double* gradients = nullptr;
      size_t gradients_dim[2];
      double* stressTensor = nullptr;
      size_t stressTensor_dim[2];
      double* elasticTensor = nullptr;
      size_t elasticTensor_dim[2];
      double* hessian = nullptr;
      size_t hessian_dim[2];
      double* dipoleMoment = nullptr;
      size_t dipoleMoment_dim[2];
      double* dipoleGradients = nullptr;
      size_t dipoleGradients_dim[2];
   };
};


class AMSCallPipe {

   public:

      AMSCallPipe(const std::string& filename="call_pipe");

      // Method to receive a generic message on the call pipe:
      AMSPipe::Message receive();

      // Methods to extract the payload of the specific messages:

      void extract_Hello(AMSPipe::Message& msg, int64_t& version) const;

      void extract_SetSystem(AMSPipe::Message& msg,
         std::vector<std::string>& atomSymbols,
         std::vector<double>& coords,
         std::vector<double>& latticeVectors,
         double& totalCharge
      ) const;

      void extract_Solve(AMSPipe::Message& msg,
         AMSPipe::SolveRequest& request,
         bool& keepResults,
         std::string& prevTitle
      ) const;

      void extract_DeleteResults(AMSPipe::Message& msg, std::string& title) const;

   private:
      std::ifstream pipe;

};


class AMSReplyPipe {

   public:

      AMSReplyPipe(const std::string& filename="reply_pipe");

      // Methods to send specific messages:

      void send_return(
         AMSPipe::Status status,
         const std::string& method="",
         const std::string& argument="",
         const std::string& message=""
      );

      void send_results(const AMSPipe::Results& results);

   private:
      std::ofstream pipe;
      void send(std::stringstream& buf);

};


#endif // AMSPIPE_HPP_INCLUDED
