#include <stdio.h>
#include <stdlib.h>
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
double LJ_potential(int nAtoms, const double* coords, double* gradients);


int main() {

   amscallpipe_t   call_pipe = new_amscallpipe(NULL);
   amsreplypipe_t reply_pipe = new_amsreplypipe(NULL);
   amspipe_message_t     msg = new_amspipe_message();

   // Variables holding our current system:
   int64_t numAtoms       = 0;
   char**  atomSymbols    = NULL;
   double* coords         = NULL;
   int64_t numLatVecs     = 0;
   double* latticeVectors = NULL;
   double  totalCharge    = 0.0;

   // We do not make an attempt to keep track of the cached results in the C language demo.
   // Just too much effort since there is no dictionary in the C standard library ...

   // Variable to store the error until we send the corresponding return message:
   amspipe_error_t* error = NULL;

   while (1) {
      amscallpipe_receive(call_pipe, &msg);
      //printf("Method called: %s\n", msg.name);

      if (strcmp(msg.name, "Exit") == 0) {
         break;

      } else if (error) {
         if (strncmp(msg.name, "Set", 3) == 0) {
            // Calls to "Set" methods are ignored while an error is buffered.
            continue;
         } else {
            // Non-"Set" method called: return buffered error and clear it.
         amsreplypipe_send_return(reply_pipe, error->status, error->method, error->argument, error->message);
         delete_amspipe_error(&error);
         }

      } else if (strcmp(msg.name, "Hello") == 0) {
         int64_t version;
         error = amscallpipe_extract_Hello(call_pipe, msg, &version);
         if (!error) {
            amsreplypipe_send_return(reply_pipe, version == 1 ? AMSPIPE_STATUS_SUCCESS : AMSPIPE_STATUS_UNKNOWN_METHOD,
                                                 NULL, NULL, NULL);
         }

      } else if (strcmp(msg.name, "SetCoords") == 0) {
         error = amscallpipe_extract_SetCoords(call_pipe, msg, coords);

      } else if (strcmp(msg.name, "SetLattice") == 0) {
         error = amscallpipe_extract_SetLattice(call_pipe, msg, &numLatVecs, &latticeVectors);

      } else if (strcmp(msg.name, "SetSystem") == 0) {
         error = amscallpipe_extract_SetSystem(call_pipe, msg, &numAtoms, &atomSymbols, &coords,
                                                               &numLatVecs, &latticeVectors, &totalCharge);
         if (!error) {
            //printf("Received new system!\n");
            //print_system(numAtoms, atomSymbols, coords, numLatVecs, latticeVectors, totalCharge);
         }

      } else if (strcmp(msg.name, "Solve") == 0) {
         amspipe_solverequest_t request = new_amspipe_solverequest();
         bool keepResults;
         char* prevTitle = NULL;

         error = amscallpipe_extract_Solve(call_pipe, msg, &request, &keepResults, &prevTitle);
         if (!error) {

            //printf("Request:\n");
            //printf("   title: %s\n", request.title);
            //printf("   gradients: %i\n", request.gradients);
            //printf("   stressTensor: %i\n", request.stressTensor);
            //printf("   elasticTensor: %i\n", request.elasticTensor);
            //printf("   hessian: %i\n", request.hessian);
            //printf("   dipoleMoment: %i\n", request.dipoleMoment);
            //printf("   dipoleGradients: %i\n", request.dipoleGradients);
            //printf("keepResults: %i\n", keepResults);
            //if (prevTitle) printf("prevTitle: %s\n", prevTitle);

            amspipe_results_t results = new_amspipe_results();
            if (request.gradients) {
               results.gradients = malloc(3*numAtoms*sizeof(double));
               results.gradients_dim[0] = 3;
               results.gradients_dim[1] = numAtoms;
            }
            results.energy = LJ_potential(numAtoms, coords, results.gradients);

            if (true) { // we are so simple that we never fail ...
               amsreplypipe_send_results(reply_pipe, &results);
               amsreplypipe_send_return(reply_pipe, AMSPIPE_STATUS_SUCCESS, NULL, NULL, NULL);
            } else { // ... but if we did, we'd send a runtime_error as the return code
               amsreplypipe_send_return(reply_pipe, AMSPIPE_STATUS_RUNTIME_ERROR, "Solve",
                                                    NULL, "error evaluating the potential");
            }
            delete_amspipe_results(&results);

         }
         delete_amspipe_solverequest(&request);
         free(prevTitle);

      } else if (strcmp(msg.name, "DeleteResults") == 0) {
         char* title;
         error = amscallpipe_extract_DeleteResults(call_pipe, msg, &title);
         if (!error) {
            //printf("DeleteResults title: %s\n", title);
            // We do not keep a cache of results, so we just confirm the deletion and move on ...
            amsreplypipe_send_return(reply_pipe, AMSPIPE_STATUS_SUCCESS, NULL, NULL, NULL);
         }

      } else {
         error = malloc(sizeof(amspipe_error_t));
         error->status = AMSPIPE_STATUS_UNKNOWN_METHOD;
         error->method = strdup(msg.name);
         error->argument = NULL;
         error->message = strdup("unknown method called");
      }

      if (error && strncmp(msg.name, "Set", 3) != 0) { // Error during non-"Set" method: return and clear error immediately.
         amsreplypipe_send_return(reply_pipe, error->status, error->method, error->argument, error->message);
         delete_amspipe_error(&error);
      }
   }

   delete_amscallpipe(&call_pipe);
   delete_amsreplypipe(&reply_pipe);
   delete_amspipe_message(&msg);
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


double LJ_potential(int nAtoms, const double* coords, double* gradients) {
   const double eps  = 0.02;
   const double rmin = 5.0;

   const double rmin2 = rmin * rmin;

   double energy = 0.0;
   if (gradients) for (int i = 0; i < 3*nAtoms; ++i) gradients[i] = 0.0;

   for (int iAtom = 0; iAtom < nAtoms; ++iAtom) {
      for (int jAtom = iAtom+1; jAtom < nAtoms; ++jAtom) {

         const double vecrij[3] = { coords[3*jAtom+0]-coords[3*iAtom+0],
                                    coords[3*jAtom+1]-coords[3*iAtom+1],
                                    coords[3*jAtom+2]-coords[3*iAtom+2] };

         const double rij2 = vecrij[0]*vecrij[0] + vecrij[1]*vecrij[1] + vecrij[2]*vecrij[2];

         const double rrel2 = rmin2 / rij2;
         const double rrel6 = rrel2*rrel2*rrel2;
         const double rrel12 = rrel6*rrel6;
         energy += eps * (rrel12 - 2.0*rrel6);

         if (gradients) {
            const double gradij = 12.0 * eps / rij2 * (rrel6 - rrel12);

            gradients[3*iAtom+0] -= gradij * vecrij[0];
            gradients[3*iAtom+1] -= gradij * vecrij[1];
            gradients[3*iAtom+2] -= gradij * vecrij[2];

            gradients[3*jAtom+0] += gradij * vecrij[0];
            gradients[3*jAtom+1] += gradij * vecrij[1];
            gradients[3*jAtom+2] += gradij * vecrij[2];
         }
      }
   }

   return energy;
}
