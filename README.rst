======================
AMSPipe worker library
======================

This is a small library implementing the worker side of the `AMS pipe protocol
<https://www.scm.com/doc/AMS/Pipe_protocol.html>`_. It is MIT licensed and can
be incorporated into any code that wants to become an engine for the `AMS driver
<https://www.scm.com/doc/AMS/index.html>`_.

Building
--------

The library is written in C++11 and has no dependencies except for the C++
standard library. It can easily be built using CMake::

   $ mkdir build
   $ cd build
   $ cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON ..
   $ cmake --build .

This will build the (shared) ``amspipe`` library, and 3 demo applications that
show how it is used from C++, C and Fortran. (The Fortran demo is automatically
skipped if no Fortran compiler is found.) The library and its header files can
then be installed in the standard way::

   $ sudo cmake --install . --prefix=...

This will install the following files into the prefix::

   prefix
   ├── include
   │   ├── amspipe.hpp
   │   ├── amspipe.h
   │   └── amspipe.F90
   └── lib
       └── libamspipe.so

Usage
-----

The library can be used from C++, C and Fortran (2008). Check the ``include``
and ``demo`` directories in this repository. The header files in ``include``
contain some description of the API, while the demo applications in ``demo``
show how to use it in the respective language. The demo applications a
Lennard-Jones potential worker that can communicate with the AMS driver via the
AMS pipe protocol. The ``demo/demo.run`` script uses the demo applications to
perform a geometry optimization of an Ar11 cluster. You can select which
language demo to run with the ``$DEMO_LANGUAGE`` environment variable::

   $ cd demo
   $ DEMO_LANGUAGE=cpp ./demo.run
   $ DEMO_LANGUAGE=c   ./demo.run
   $ DEMO_LANGUAGE=F90 ./demo.run

Note that the amspipe library only contains the C++ and C interfaces. This is
because the Fortran module files are compiler dependent, so we can not easily
ship the module file along with the library and have it work with whatever
Fortran compiler someone is using. The entire Fortran bindings are therefore
delivered as source code, to be included and compiled along with the code using
them:

.. code:: fortran

   ! Create the amspipe module:
   #include <amspipe.F90>

   ! Use it in your own code:
   program my_amspipe_worker
      use amspipe

      ! ...

   end subroutine
