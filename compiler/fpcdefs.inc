{$ifdef FPC}
  {$mode objfpc}
  {$asmmode default}
  {$H-}
  {$goto on}
  {$inline on}

{$ifdef win32}
  { 256MB stack }
  { under windows the stack can't grow }
  {$MEMORY 256000000}
{$else win32}
  { 1MB stack }
  {$MEMORY 1000000}
{$endif win32}
  { This reduces the memory requirements a lot }
  {$PACKENUM 1}

  { We don't use exceptions, so turn off the implicit
    exceptions in the constructors }
  {$IMPLICITEXCEPTIONS OFF}
  { Inline small functions, but not when EXTDEBUG is used }
  {$ifndef EXTDEBUG}
    {$define USEINLINE}
  {$endif EXTDEBUG}

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
  {$define SUPPORT_MMX}
  {$define cpumm}
  {$define fewintregisters}
{$endif i386}

{$ifdef x86_64}
  {$define x86}
  {$define cpuflags}
  {$define cpu64bit}
  {$define cpuextended}
  {$define cpufloat128}
  {$define cputargethasfixedstack}
  {$define USECMOV}
  {$define cpumm}
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
  {$define cpumm}
{$endif powerpc}

{$ifdef powerpc64}
  {$define cpu64bit}
  {$define cpuflags}
  {$define cputargethasfixedstack}
  {$define cpumm}
  {$define cpurequiresproperalignment}
{$endif powerpc64}

{$ifdef arm}
  {$define cpuflags}
  {$define cpufpemu}
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

{$define SUPPORT_UNALIGNED}
