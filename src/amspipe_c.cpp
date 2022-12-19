#include <amspipe.h>
#include <amspipe.hpp>

#include <string.h>

extern "C" {


// ===== AMSPipe::Message ========================================================================================================

amspipe_message_t new_amspipe_message() {
   amspipe_message_t msg = {nullptr, nullptr};
   return msg;
}

void delete_amspipe_message(amspipe_message_t* message) {
   message->name = nullptr;
   if (message->p) {
      AMSPipe::Message* msg_p = reinterpret_cast<AMSPipe::Message*>(message->p);
      delete msg_p;
      message->p = nullptr;
   }
}


// ===== AMSPipe::Error ==========================================================================================================

void delete_amspipe_error(amspipe_error_t** error) {
   if (*error) {
      free((*error)->method);
      free((*error)->argument);
      free((*error)->message);
      free(*error);
   }
   *error = nullptr;
}

static amspipe_error_t* new_amspipe_error_from_exception(const AMSPipe::Error& exc) {
   amspipe_error_t* err = static_cast<amspipe_error_t*>(malloc(sizeof(amspipe_error_t)));
   err->status = static_cast<amspipe_status_t>(exc.status);
   err->method = exc.method.empty() ? nullptr : strdup(exc.method.c_str());
   err->argument = exc.argument.empty() ? nullptr : strdup(exc.argument.c_str());
   err->message = strdup(exc.what());
   return err;
}


// ===== AMSPipe::SolveRequest ===================================================================================================

amspipe_solverequest_t new_amspipe_solverequest() {
   amspipe_solverequest_t request;
   request.title = nullptr;
   return request;
}

void delete_amspipe_solverequest(amspipe_solverequest_t* request) {
   if (request->title) {
      free(request->title);
      request->title = nullptr;
   }
}


// ===== AMSPipe::Results ========================================================================================================

amspipe_results_t new_amspipe_results() {
   amspipe_results_t res;
   res.numMessages = 0;
   res.messages = nullptr;
   res.energy = 0.0;
   res.gradients = nullptr;
   res.gradients_dim[0] = 0; res.gradients_dim[1] = 0;
   res.stressTensor = nullptr;
   res.stressTensor_dim[0] = 0; res.stressTensor_dim[1] = 0;
   res.elasticTensor = nullptr;
   res.elasticTensor_dim[0] = 0; res.elasticTensor_dim[1] = 0;
   res.hessian = nullptr;
   res.hessian_dim[0] = 0; res.hessian_dim[1] = 0;
   res.dipoleMoment = nullptr;
   res.dipoleMoment_dim[0] = 0; res.dipoleMoment_dim[1] = 0;
   res.dipoleGradients = nullptr;
   res.dipoleGradients_dim[0] = 0; res.dipoleGradients_dim[1] = 0;
   return res;
}

void delete_amspipe_results(amspipe_results_t* results) {
   if (results->messages) {
      for (int64_t i = 0; i < results->numMessages; ++i) free(results->messages[i]);
      free(results->messages);
   }
   free(results->gradients);
   free(results->stressTensor);
   free(results->elasticTensor);
   free(results->hessian);
   free(results->dipoleMoment);
   free(results->dipoleGradients);
   *results = new_amspipe_results();
}


// ===== AMSCallPipe =============================================================================================================

amscallpipe_t new_amscallpipe(const char* filename) {
   amscallpipe_t ret;
   if (filename) {
      ret.p = new AMSCallPipe(filename);
   } else {
      ret.p = new AMSCallPipe();
   }
   return ret;
}


void delete_amscallpipe(amscallpipe_t* cp) {
   AMSCallPipe* self = reinterpret_cast<AMSCallPipe*>(cp->p);
   delete self;
   cp->p = nullptr;
}


void amscallpipe_receive(amscallpipe_t cp, amspipe_message_t* message) {
   AMSCallPipe* self = reinterpret_cast<AMSCallPipe*>(cp.p);
   AMSPipe::Message* msg_p = message->p
                           ? reinterpret_cast<AMSPipe::Message*>(message->p)
                           : new AMSPipe::Message;
   *msg_p = self->receive();
   message->p = reinterpret_cast<void*>(msg_p);
   message->name = msg_p->name.c_str();
}


void amscallpipe_extract_Hello(amscallpipe_t cp, amspipe_message_t message, amspipe_error_t** error, int64_t* version) {
   try {
      const AMSCallPipe* self = reinterpret_cast<const AMSCallPipe*>(cp.p);
      AMSPipe::Message* msg_p = reinterpret_cast<AMSPipe::Message*>(message.p);
      self->extract_Hello(*msg_p, *version);
   } catch (const AMSPipe::Error& exc) {
      *error = new_amspipe_error_from_exception(exc);
   }
}


void amscallpipe_extract_SetSystem(amscallpipe_t cp, amspipe_message_t message, amspipe_error_t** error,
   int64_t* numAtoms,
   char***  atomSymbols,
   double** coords,
   int64_t* numLatVecs,
   double** latticeVectors,
   double*  totalCharge
) {
   try {
      const AMSCallPipe* self = reinterpret_cast<const AMSCallPipe*>(cp.p);
      AMSPipe::Message* msg_p = reinterpret_cast<AMSPipe::Message*>(message.p);

      // Clean output values:
      if (*atomSymbols) {
         for (int64_t iat = 0; iat < *numAtoms; ++iat) free((*atomSymbols)[iat]);
         free(*atomSymbols); *atomSymbols = nullptr;
      }
      free(*coords); *coords = nullptr;
      free(*latticeVectors); *latticeVectors = nullptr;
      *numAtoms = 0;
      *numLatVecs = 0;
      *totalCharge = 0.0;

      // Read message into std::vectors:
      std::vector<std::string> atSyms;
      std::vector<double>      crds;
      std::vector<double>      latVecs;
      self->extract_SetSystem(*msg_p, atSyms, crds, latVecs, *totalCharge);

      // Write data to freshly malloc'd output arrays:

      *numAtoms = atSyms.size();
      *atomSymbols = static_cast<char**>(malloc(atSyms.size()*sizeof(char*)));
      for (size_t i = 0; i < atSyms.size(); ++i) (*atomSymbols)[i] = strdup(atSyms[i].c_str());

      *coords = static_cast<double*>(malloc(crds.size()*sizeof(double)));
      for (size_t i = 0; i < crds.size(); ++i) (*coords)[i] = crds[i];

      *numLatVecs = latVecs.size() / 3;
      if (*numLatVecs > 0) {
         *latticeVectors = static_cast<double*>(malloc(latVecs.size()*sizeof(double)));
         for (size_t i = 0; i < latVecs.size(); ++i) (*latticeVectors)[i] = latVecs[i];
      }

   } catch (const AMSPipe::Error& exc) {
      // output is already clean in case of error: we cleaned it in the beginning!
      *error = new_amspipe_error_from_exception(exc);
   }
}


void amscallpipe_extract_SetCoords(amscallpipe_t cp, amspipe_message_t message, amspipe_error_t** error, double* coords) {
   try {
      const AMSCallPipe* self = reinterpret_cast<const AMSCallPipe*>(cp.p);
      AMSPipe::Message* msg_p = reinterpret_cast<AMSPipe::Message*>(message.p);
      self->extract_SetCoords(*msg_p, coords);
   } catch (const AMSPipe::Error& exc) {
      *error = new_amspipe_error_from_exception(exc);
   }
}


void amscallpipe_extract_SetLattice(amscallpipe_t cp, amspipe_message_t message, amspipe_error_t** error,
   int64_t* numLatVecs,
   double** latticeVectors
) {
   try {
      const AMSCallPipe* self = reinterpret_cast<const AMSCallPipe*>(cp.p);
      AMSPipe::Message* msg_p = reinterpret_cast<AMSPipe::Message*>(message.p);

      // Read message into std::vectors:
      std::vector<double> latVecs;
      self->extract_SetLattice(*msg_p, latVecs);

      if (*numLatVecs /= latVecs.size() / 3) { // periodicity changed
         *numLatVecs = latVecs.size() / 3;
         free(*latticeVectors); *latticeVectors = nullptr;
         if (*numLatVecs != 0) *latticeVectors = static_cast<double*>(malloc(latVecs.size()*sizeof(double)));
      }
      for (size_t i = 0; i < latVecs.size(); ++i) (*latticeVectors)[i] = latVecs[i];

   } catch (const AMSPipe::Error& exc) {
      // make sure all output is clean if we return an error
      *numLatVecs = 0;
      free(*latticeVectors); *latticeVectors = nullptr;
      *error = new_amspipe_error_from_exception(exc);
   }
}


void amscallpipe_extract_Solve(amscallpipe_t cp, amspipe_message_t message, amspipe_error_t** error,
   amspipe_solverequest_t* request,
   bool* keepResults,
   char** prevTitle
) {
   try {
      const AMSCallPipe* self = reinterpret_cast<const AMSCallPipe*>(cp.p);
      AMSPipe::Message* msg_p = reinterpret_cast<AMSPipe::Message*>(message.p);

      *keepResults = false;
      free(request->title); request->title = nullptr;
      free(*prevTitle);         *prevTitle = nullptr;

      AMSPipe::SolveRequest rq;
      std::string pT;
      self->extract_Solve(*msg_p, rq, *keepResults, pT);

      if (!rq.title.empty()) request->title = strdup(rq.title.c_str());
      request->quiet           = rq.quiet;
      request->gradients       = rq.gradients;
      request->stressTensor    = rq.stressTensor;
      request->elasticTensor   = rq.elasticTensor;
      request->hessian         = rq.hessian;
      request->dipoleMoment    = rq.dipoleMoment;
      request->dipoleGradients = rq.dipoleGradients;

      if (!pT.empty()) *prevTitle = strdup(pT.c_str());

   } catch (const AMSPipe::Error& exc) {
      // output is already clean in case of errors: self->extract_Solve was the last thing that could have thrown!
      *error = new_amspipe_error_from_exception(exc);
   }
}


void amscallpipe_extract_DeleteResults(amscallpipe_t cp, amspipe_message_t message, amspipe_error_t** error, char** title) {
   try {
      const AMSCallPipe* self = reinterpret_cast<const AMSCallPipe*>(cp.p);
      AMSPipe::Message* msg_p = reinterpret_cast<AMSPipe::Message*>(message.p);

      free(*title); *title = nullptr;

      std::string t;
      self->extract_DeleteResults(*msg_p, t);

      if (!t.empty()) *title = strdup(t.c_str());
   } catch (const AMSPipe::Error& exc) {
      // output is already clean in case of errors: self->extract_DeleteResults was the last thing that could have thrown!
      *error = new_amspipe_error_from_exception(exc);
   }
}


// ===== AMSReplyPipe ============================================================================================================

amsreplypipe_t new_amsreplypipe(const char* filename) {
   amsreplypipe_t ret;
   if (filename) {
      ret.p = new AMSReplyPipe(filename);
   } else {
      ret.p = new AMSReplyPipe();
   }
   return ret;
}


void delete_amsreplypipe(amsreplypipe_t* rp) {
   AMSReplyPipe* self = reinterpret_cast<AMSReplyPipe*>(rp->p);
   delete self;
   rp->p = nullptr;
}


void amsreplypipe_send_return(amsreplypipe_t rp,
   amspipe_status_t status,
   const char* method,
   const char* argument,
   const char* message
) {
   AMSReplyPipe* self = reinterpret_cast<AMSReplyPipe*>(rp.p);
   self->send_return(
      static_cast<AMSPipe::Status>(status),
      method ? method : "",
      argument ? argument : "",
      message ? message : ""
   );
}


void amsreplypipe_send_results(amsreplypipe_t rp, const amspipe_results_t* results) {
   AMSReplyPipe* self = reinterpret_cast<AMSReplyPipe*>(rp.p);
   AMSPipe::Results res;

   if (results->numMessages > 0) {
      res.messages.resize(results->numMessages);
      for (int64_t i = 0; i < results->numMessages; ++i) res.messages[i] = results->messages[i];
   }
   res.energy = results->energy;

   res.gradients = results->gradients;
   res.gradients_dim[0] = results->gradients_dim[0];
   res.gradients_dim[1] = results->gradients_dim[1];

   res.stressTensor = results->stressTensor;
   res.stressTensor_dim[0] = 0;
   res.stressTensor_dim[1] = 0;

   res.elasticTensor = results->elasticTensor;
   res.elasticTensor_dim[0] = results->elasticTensor_dim[0];
   res.elasticTensor_dim[1] = results->elasticTensor_dim[1];

   res.hessian = results->hessian;
   res.hessian_dim[0] = results->hessian_dim[0];
   res.hessian_dim[1] = results->hessian_dim[1];

   res.dipoleMoment = results->dipoleMoment;
   res.dipoleMoment_dim[0] = results->dipoleMoment_dim[0];
   res.dipoleMoment_dim[1] = results->dipoleMoment_dim[1];

   res.dipoleGradients = results->dipoleGradients;
   res.dipoleGradients_dim[0] = results->dipoleGradients_dim[0];
   res.dipoleGradients_dim[1] = results->dipoleGradients_dim[1];

   self->send_results(res);
}


}
