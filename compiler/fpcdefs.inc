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
  {$endif VER1_0}

  {$define FPCPROCVAR}
  {$define USEEXCEPT}
  
  {$ifdef cpuarm}
    {$packrecords c}
  {$endif cpuarm}
{$endif}

{$ifdef i386}
  {$define x86}
  {$define cpuflags}
  {$define cpuextended}
{$endif i386}

{$ifdef x86_64}
  {$define x86}
  {$define cpuflags}
  {$define cpu64bit}
  {$define cpuextended}
  {$define cpufloat128}
  {$define noopt}
  {$define cputargethasfixedstack}
{$endif x86_64}

{$ifdef alpha}
  {$define cpu64bit}
  {$define noopt}
{$endif alpha}

{$ifdef sparc}
  {$define cpuflags}
  {$define noopt}
  {$define cputargethasfixedstack}
{$endif sparc}

{$ifdef powerpc}
  {$define cpuflags}
  {$define noopt}
  {$define oldregvars}
{$endif powerpc}

{$ifdef arm}
  {$define cpuflags}
  {$define cpuneedsdiv32helper}
  {$define cputargethasfixedstack}
  {$define noopt}
{$endif arm}

{$ifdef m68k}
  {$define cpuflags}
  {$define cpufpemu}
  {$define noopt}
{$endif m68k}

{
  $Log$
  Revision 1.41  2004-09-21 19:59:51  peter
    * x86_64 fixes
    * cleanup of fpcdefs.icn

  Revision 1.40  2004/09/21 17:25:12  peter
    * paraloc branch merged

  Revision 1.39.4.1  2004/08/31 20:43:06  peter
    * paraloc patch

  Revision 1.39  2004/08/15 13:30:18  florian
    * fixed alignment of variant records
    * more alignment problems fixed

  Revision 1.38  2004/06/20 08:55:29  florian
    * logs truncated

  Revision 1.37  2004/06/16 20:07:07  florian
    * dwarf branch merged

  Revision 1.36  2004/05/30 21:20:40  jonas
    * enable regvars by default for PPC

  Revision 1.35.2.4  2004/05/02 12:45:32  peter
    * enabled cpuhasfixedstack for x86-64 again
    * fixed size of temp allocation for parameters

  Revision 1.35.2.3  2004/05/02 01:02:24  peter
    * remove fixed stack for x86-64

  Revision 1.35.2.2  2004/04/29 23:30:28  peter
    * fix i386 compiler

}
