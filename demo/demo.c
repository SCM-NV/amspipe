#include <stdio.h>
#include <string.h>

#include <amspipe.h>

// Declare some convenient methods to use in main:
void print_system(
   int64_t       numAtoms,
   char**        atomSymbols,
   const double* coords,
   int64_t       numLatVecs,
   const double* latticeVectors,
   double        totalCharge
);
//double LJ_potential(const double* coords, double* gradients);


int main(int argc, char* argv[]) {

   amscallpipe_t   call_pipe = new_amscallpipe(NULL);
   amsreplypipe_t reply_pipe = new_amsreplypipe(NULL);

   // Variables holding our current system:
   int64_t numAtoms       = 0;
   char**  atomSymbols    = NULL;
   double* coords         = NULL;
   int64_t numLatVecs     = 0;
   double* latticeVectors = NULL;
   double  totalCharge    = 0.0;

   // Variable to store the error until we send the corresponding return message:
   int error = 0;

   while (1) {
      amspipe_message_t msg = {NULL,NULL};
      amscallpipe_receive(call_pipe, &msg);

      if (strcmp(msg.name, "Exit") == 0) {
         break;

      } else if (error) {
         if (strncmp(msg.name, "Set", 3) == 0) {
            // Calls to "Set" methods are ignored while an error is buffered.
            continue;
         } else {
            // Non-"Set" method called: return buffered error and clear it.
            // TODO
         }

      } else if (strcmp(msg.name, "Hello") == 0) {
         int64_t version;
         amscallpipe_extract_Hello(call_pipe, msg, &version);
         amsreplypipe_send_return(reply_pipe, version == 1 ? AMSPIPE_STATUS_SUCCESS : AMSPIPE_STATUS_UNKNOWN_METHOD, NULL, NULL, NULL);

      } else if (strcmp(msg.name, "SetCoords") == 0) {

      } else if (strcmp(msg.name, "SetLattice") == 0) {

      } else if (strcmp(msg.name, "SetSystem") == 0) {
         amscallpipe_extract_SetSystem(call_pipe, msg, &numAtoms, &atomSymbols, &coords, &numLatVecs, &latticeVectors, &totalCharge);

         //printf("Received new system!\n");
         //print_system(numAtoms, atomSymbols, coords, numLatVecs, latticeVectors, totalCharge);

      } else if (strcmp(msg.name, "Solve") == 0) {

      } else if (strcmp(msg.name, "DeleteResults") == 0) {

      }

   }

   delete_amscallpipe(&call_pipe);
   delete_amsreplypipe(&reply_pipe);
   return 0;
}


void print_system(
   int64_t       numAtoms,
   char**        atomSymbols,
   const double* coords,
   int64_t       numLatVecs,
   const double* latticeVectors,
   double        totalCharge
) {
   printf("System\n");
   printf("   Atoms [Bohr]\n");
   for (int64_t iat = 0; iat < numAtoms; ++iat) {
      printf("      %s", atomSymbols[iat]);
      for (int xyz = 0; xyz < 3; ++xyz)
         printf("   %f",coords[3*iat+xyz]);
      printf("\n");
   }
   printf("   End\n");
   if (totalCharge != 0.0) {
      printf("   Charge %f\n", totalCharge);
   }
   if (numLatVecs) {
      printf("   Lattice [Bohr]\n");
      for (int64_t ivec = 0; ivec < numLatVecs; ++ivec) {
         printf("   ");
         for (int xyz = 0; xyz < 3; ++xyz)
            printf("   %f",latticeVectors[3*ivec+xyz]);
         printf("\n");
      }
      printf("   End\n");
   }
   printf("End\n");
}
