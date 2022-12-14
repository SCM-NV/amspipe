#include <amspipe.h>
#include <amspipe.hpp>

#include <string.h>

extern "C" {


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


void amscallpipe_extract_Hello(amscallpipe_t cp, amspipe_message_t message, int64_t* version) {
   const AMSCallPipe* self = reinterpret_cast<const AMSCallPipe*>(cp.p);
   AMSPipe::Message* msg_p = reinterpret_cast<AMSPipe::Message*>(message.p);
   self->extract_Hello(*msg_p, *version);
}


void amscallpipe_extract_SetSystem(amscallpipe_t cp, amspipe_message_t message,
   int64_t* numAtoms,
   char***  atomSymbols,
   double** coords,
   int64_t* numLatVecs,
   double** latticeVectors,
   double*  totalCharge
) {
   const AMSCallPipe* self = reinterpret_cast<const AMSCallPipe*>(cp.p);
   AMSPipe::Message* msg_p = reinterpret_cast<AMSPipe::Message*>(message.p);

   // Free output arrays:
   if (*numAtoms != 0) {
      for (int64_t iat = 0; iat < *numAtoms; ++iat) free((*atomSymbols)[iat]);
      free(*atomSymbols); *atomSymbols = NULL;
      free(*coords); *coords = NULL;
      if (*latticeVectors) { free(*latticeVectors); } *latticeVectors = NULL;
   }

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

}


void amscallpipe_extract_SetCoords(amscallpipe_t cp, amspipe_message_t message, double* coords) {
   const AMSCallPipe* self = reinterpret_cast<const AMSCallPipe*>(cp.p);
   AMSPipe::Message* msg_p = reinterpret_cast<AMSPipe::Message*>(message.p);
   self->extract_SetCoords(*msg_p, coords);
}


void amscallpipe_extract_SetLattice(amscallpipe_t cp, amspipe_message_t message,
   int64_t* numLatVecs,
   double** latticeVectors
) {
   const AMSCallPipe* self = reinterpret_cast<const AMSCallPipe*>(cp.p);
   AMSPipe::Message* msg_p = reinterpret_cast<AMSPipe::Message*>(message.p);

   // Read message into std::vectors:
   std::vector<double> latVecs;
   self->extract_SetLattice(*msg_p, latVecs);

   if (*numLatVecs /= latVecs.size() / 3) { // periodicity changed
      *numLatVecs = latVecs.size() / 3;
      free(*latticeVectors);
      if (numLatVecs != 0) {
         *latticeVectors = static_cast<double*>(malloc(latVecs.size()*sizeof(double)));
      } else {
         *latticeVectors = nullptr;
      }
   }
   for (size_t i = 0; i < latVecs.size(); ++i) (*latticeVectors)[i] = latVecs[i];
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

}
