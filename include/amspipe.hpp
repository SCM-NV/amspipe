#ifndef AMSPIPE_HPP_INCLUDED
#define AMSPIPE_HPP_INCLUDED

#include <fstream>
#include <string>


class AMSPipeError : public std::runtime_error { using std::runtime_error::runtime_error; };


class AMSCallPipe {

   public:

      AMSCallPipe(const std::string& filename="call_pipe");

      std::string next_method();
      void method_called();

      void recv_hello(int64_t* version);

   private:
      std::ifstream pipe;

      void verify_marker(char m);
      int64_t read_int();
      std::string read_string();

};


class AMSReplyPipe {

   public:
      AMSReplyPipe(const std::string& filename="reply_pipe");

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
      void send_return(
         Status status,
         const std::string& method="",
         const std::string& argument="",
         const std::string& message=""
      );

   private:
      std::ofstream pipe;

};


#endif // AMSPIPE_HPP_INCLUDED
