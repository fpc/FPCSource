{$ifdef FPC}
  {$mode objfpc}
  {$asmmode default}
  {$H-}
  {$goto on}
  {$inline on}

  { 1Mb stack }
  {$MEMORY 1000000}

  { This reduces the memory requirements a lot }
  {$PACKENUM 1}

  {$ifndef VER1_0}
      { We don't use exceptions, so turn off the implicit
        exceptions in the constructors }
      {$IMPLICITEXCEPTIONS OFF}
      { Inline small functions, but not when EXTDEBUG is used }
      {$ifndef EXTDEBUG}
        {$define USEINLINE}
      {$endif EXTDEBUG}
  {$else}
      { Optimizer is broken when compiling with optimizations using 1.0.x }
      {$ifndef USEOPT}
        {$define NOOPT}
      {$endif}

      { 1.0.x generates broken code for sysutils }
      {$ifndef FORCE_SYSUTILS}
        {$undef USE_SYSUTILS}
      {$endif}
  {$endif VER1_0}

  {$ifdef VER1_9_4}
     { 1.9.4 generates broken code for sysutils }
     {$ifndef FORCE_SYSUTILS}
       {$undef USE_SYSUTILS}
     {$endif}
  {$endif}

  {$define USEEXCEPT}

  {$ifdef cpuarm}
    {$packrecords c}
  {$endif cpuarm}
{$endif}

{$ifdef i386}
  {$define x86}
  {$define cpuflags}
  {$define cpuextended}
  {$define USECMOV}
{$endif i386}

{$ifdef x86_64}
  {$define x86}
  {$define cpuflags}
  {$define cpu64bit}
  {$define cpuextended}
  {$define cpufloat128}
  {$define cputargethasfixedstack}
  {$define USECMOV}
{$endif x86_64}

{$ifdef alpha}
  {$define cpu64bit}
{$endif alpha}

{$ifdef sparc}
  {$define cpuflags}
  {$define cputargethasfixedstack}
  {$define cpurequiresproperalignment}
{$endif sparc}

{$ifdef powerpc}
  {$define cpuflags}
  {$define cputargethasfixedstack}
{$endif powerpc}

{$ifdef arm}
  {$define cpuflags}
  {$define cpuneedsdiv32helper}
  {$define cputargethasfixedstack}
  {$define cpurequiresproperalignment}
{$endif arm}

{$ifdef m68k}
  {$define cpuflags}
  {$define cpufpemu}
{$endif m68k}

{$IFDEF MACOS}
{$DEFINE MACOS_USE_FAKE_SYSUTILS}
{$ENDIF MACOS}

{
  $Log: fpcdefs.inc,v $
  Revision 1.51  2005/03/20 21:41:29  florian
    * USECMOV activated

  Revision 1.50  2005/02/26 01:26:59  jonas
    * fixed generic jumps optimizer and enabled it for ppc (the label table
      was not being initialised -> getfinaldestination always failed, which
      caused wrong optimizations in some cases)
    * changed the inverse_cond into a function, because tasmcond is a record
      on ppc
    + added a compare_conditions() function for the same reason

  Revision 1.49  2005/02/14 17:13:06  peter
    * truncate log

  Revision 1.48  2005/02/08 22:33:51  olle
    * fixed compilation on MacOS

}
