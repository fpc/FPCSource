Interface unit for Fastest Fourier in the West library
------------------------------------------------------

This unit is a Pascal interface to the FFTW library version 3. FFTW is
a library to compute fast fourier transforms extremely fast, it uses
a runtime code generator to generate the best algorithm for a specific
transformation.

The unit is experimental and community involvement is welcome.

At this time we provide a single precision interface only. 
Interfaces for the double and extended precision is a simpleprogramming
exercise, you can simply replace single by double everywhere. 

See http://www.fftw.org for extensive documentation.

Usage:
* Compile FFTW. Use the "--enable-single" option to configure to select single
  precision.
* We need libgcc. Its location should be present in your fpc.cfg. If not,
  please use the tool "samplecfg" that is shipped with the compiler to
  generate a new fpc.cfg.
* The compiler should be able to find libfftw3f.a. Put something like
  -Fl/path/to/libfftw3f.a in your fpc.cfg

Short example how to perform a Fourier transformation in Pascal:

begin
  {You can use getmem, but only fftw_getmem ensures 3DNOW/SSE
   algorithms.}
  fftw_getmem(i,count*sizeof(complex_single));
  fftw_getmem(o,count*sizeof(complex_single));

  {FFTW will now generate an algoritm to compute the FFT:}
  plan:=fftw_plan_dft_1d(count,i,o,fftw_forward,[fftw_estimate]);

  {Put code to fill i^ with input data here.}
  fftw_execute(p); {Execute FFT}
  {Output in o^}

  {We can repeat, and refill i with input data.}
  {Put code to fill i^ with input data here.}
  fftw_execute(p); {Execute FFT}
  {Output in o^}

  fftw_destroy_plan(p);
  fftw_freemem(i);
  fftw_freemem(o);
end;

Differences with C version:
  * To introduce strong typing:
  ** The sign (fftw_forward,fftw_backward) is an enumeration instead of an int
     with constants.
  ** The flags ([fftw_estimate]) is a flag set instead of an int with constants.
  ** The fftw_plan_single type is an opaque pointer incompatible with anything
     else;
  * To ease programming:
  ** Functions for complex to complex, real to complex, and complex to real are
     all called equally, the compiler will determine which one needs to be
     called. So we have just fftw_plan_dft_1d instead of fftw_plan_dft_1d,
     fftw_plan_dft_r2c_1d, fftw_plan_dft_c2r_1d, etc. 
  * fftw_getmem/fftw_freemem instead of fftw_malloc/fftw_free
