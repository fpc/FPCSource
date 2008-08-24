{$mode objfpc}
{$asmmode default}
{$H-}
{$goto on}
{$inline on}

{$ifdef win32}
  { 256 MB stack }
  { under windows the stack can't grow }
  {$MEMORY 256000000}
{$else win32}
  {$ifdef win64}
    { 512 MB stack }
    { under windows the stack can't grow }
    {$MEMORY 512000000}
  {$else win64}
    { 1 MB stack }
    {$MEMORY 1000000}
  {$endif win64}
{$endif win32}

{ This reduces the memory requirements a lot }
{$PACKENUM 1}
{$ifdef FPC_HAS_VARSETS}
{$ifndef FPC_BIG_ENDIAN}
{ $define USE_PACKSET1}
{$endif}
{$endif FPC_HAS_VARSETS}

{$ifdef USE_PACKSET1}
{$PACKSET 1}
{$endif USE_PACKSET1}

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

{$ifdef i386}
  {$define cpu32bit}
  {$define cpu32bitaddr}
  {$define cpu32bitalu}
  {$define x86}
  {$define cpuflags}
  {$define cpuextended}
  {$define SUPPORT_MMX}
  {$define cpumm}
  {$define fewintregisters}
{$endif i386}

{$ifdef x86_64}
  {$define x86}
  {$define cpuflags}
  {$define cpu64bitalu}
  {$define cpu64bitaddr}
  {$define cpuextended}
  {$define cpufloat128}
  {$define cputargethasfixedstack}
  {$define cpumm}
{$endif x86_64}

{$ifdef alpha}
  {$define cpu64bitalu}
  {$define cpu64bitaddr}
{$endif alpha}

{$ifdef sparc}
  {$define cpu32bit}
  {$define cpu32bitaddr}
  {$define cpu32bitalu}
  {$define cpuflags}
  {$define cputargethasfixedstack}
{$endif sparc}

{$ifdef powerpc}
  {$define cpu32bit}
  {$define cpu32bitaddr}
  {$define cpu32bitalu}
  {$define cpuflags}
  {$define cputargethasfixedstack}
  {$define cpumm}
{$endif powerpc}

{$ifdef powerpc64}
  {$define cpu64bitalu}
  {$define cpu64bitaddr}
  {$define cpuflags}
  {$define cputargethasfixedstack}
  {$define cpumm}
{$endif powerpc64}

{$ifdef arm}
  {$define cpu32bit}
  {$define cpu32bitaddr}
  {$define cpu32bitalu}
  {$define cpuflags}
  {$define cpufpemu}
  {$define cpuneedsdiv32helper}
  {$define cputargethasfixedstack}
{$endif arm}

{$ifdef m68k}
  {$define cpu32bit}
  {$define cpu32bitaddr}
  {$define cpu32bitalu}
  {$define cpuflags}
  {$define cpufpemu}
{$endif m68k}

{$ifdef avr}
  {$define cpu16bit}
  {$define cpu16bitaddr}
  {$define cpu16bitalu}
  {$define cpuflags}
  {$define cpunofpu}
  {$define cpunodefaultint}
{$endif avr}

{$IFDEF MACOS}
{$DEFINE USE_FAKE_SYSUTILS}
{$ENDIF MACOS}

{$define SUPPORT_UNALIGNED}

{$if not defined(cpui386) and defined(i386)}
{$error Cross-compiling from non-i386 to i386 is not yet supported at this time }
{$endif}
