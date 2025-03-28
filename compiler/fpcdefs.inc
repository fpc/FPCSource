{$mode objfpc}
{$asmmode default}
{$modeswitch advancedrecords}
{$H-}
{$goto on}
{$ifndef DISABLE_INLINE}
  {$inline on}
  { Inline small functions, but not when EXTDEBUG is used }
  {$ifndef EXTDEBUG}
    {$define USEINLINE}
  {$endif EXTDEBUG}
{$endif}
{$interfaces corba}

{ This reduces the memory requirements a lot }
{$PACKENUM 1}
{$PACKSET 1}

{ We don't use exceptions, so turn off the implicit
  exceptions in the constructors }
{$IMPLICITEXCEPTIONS OFF}

{ We don't want the compiler to use fastmath
  optimization because it considers negative zeroes
  as normal zeroes }
{$OPTIMIZATION NOFASTMATH}

{ We don't want C operators to be used inside the compiler }
{$COPERATORS OFF}

{ This define enables codepage-aware compiler messages handling. Turning it on
  forces code page conversion from the codepage, specified in the .msg file to
  CP_ACP, before printing the message to the console. Enable this for host
  platforms, that have code page conversion support in their RTL. }
{$if defined(win32) or defined(win64) or defined(unix)}
  {$define cpawaremessages}
{$endif}

{$ifdef DEBUG_ALL_OPT}
  { for aopt unit }
  {$define DEBUG_OPTALLOC}
  {$define DEBUG_INSTRUCTIONREGISTERDEPENDENCIES}
  {for CPU/aoptcpu unit }
  {$define DEBUG_AOPTCPU}
  {$define DEBUG_PREREGSCHEDULER (arm specific) }
  { for aoptobj unit }
  {$define DEBUG_AOPTOBJ}
  {$define ALLOCREGDEBUG}
  { for optconstprop unit }
  {$define DEBUG_CONSTPROP}
  { for optcse unit }
  {$define CSEDEBUG}
  { for optdeadstore unit }
  {$define DEBUG_DEADSTORE}
  { for optdfa unit }
  {$define DEBUG_DFA}
  { for optloop unit }
  {$define DEBUG_OPTFORLOOP}
  {$define DEBUG_OPTSTRENGTH}
  { for optvirt unit }
  {$define DEBUG_DEVIRT}
{$endif}

{$define USEEXCEPT}

{ This fake CPU is used to allow incorporation of globtype unit
  into utils/ppudump without any CPU specific code PM }
{$ifdef generic_cpu}
  {$define cpu32bit}
  {$define cpu32bitaddr}
  {$define cpu32bitalu}
  {$define cpuflags}
  {$define cpuextended}
{$endif generic_cpu}

{$ifdef cpuarm}
  {$packrecords c}
{$endif cpuarm}

{$ifdef i8086}
  {$define cpu16bit}
  {$define cpu16bitaddr}
  {$define cpu16bitalu}
  {$define x86}
  {$define cpuflags}
  {$define cpuextended}
  {//$define SUPPORT_MMX}
  {$define cpumm}
  {$define fewintregisters}
  {$define cpurox}
  {$define cpurefshaveindexreg}
  {$define SUPPORT_SAFECALL}
  {$define cpuneedsmulhelper}
  { TODO: add another define in order to disable the div helper for 16-bit divs? }
  {$define cpuneedsdivhelper}
  {$define VOLATILE_ES}
  {$define SUPPORT_GET_FRAME}
  {$define cpucg64shiftsupport}
  {$define OMFOBJSUPPORT}
  {$ifdef go32v2}
    { go32v2 uses cwsdpmi extender which is incompatible with watcom extender
      thus we use the internal smartlink sections by default in that case. }
    {$define I8086_SMARTLINK_SECTIONS}
    {$define i8086_link_intern_debuginfo}
  {$endif go32v2}
{$endif i8086}

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
  {$define cpurox}
  {$define cpurefshaveindexreg}
  {$define SUPPORT_SAFECALL}
  {$define SUPPORT_GET_FRAME}
  {$define cpucapabilities}
  {$define fpucapabilities}
  {$define cpucg64shiftsupport}
{$endif i386}

{$ifdef x86_64}
  {$define x86}
  {$define cpuflags}
  {$define cpu64bit}
  {$define cpu64bitalu}
  {$define cpu64bitaddr}
  {$define cpuextended}
  {$define cpufloat128}
  {$define cputargethasfixedstack}
  {$define cpumm}
  {$define cpurox}
  {$define cpurefshaveindexreg}
  {$define SUPPORT_SAFECALL}
  {$define SUPPORT_GET_FRAME}
  {$define cpucapabilities}
  {$define fpucapabilities}
{$endif x86_64}

{$ifdef sparc}
  {$define cpu32bit}
  {$define cpu32bitaddr}
  {$define cpu32bitalu}
  {$define cpuflags}
  {$define cputargethasfixedstack}
  {$define cpurefshaveindexreg}
  {$define cpudelayslot}
  {$define SUPPORT_SAFECALL}
  {$define sparcgen}

  { the official name of the 32 Bit SPARC port is still "sparc" but
    using the sparc32 define makes things more clear }
  {$define sparc32}
{$endif sparc}

{$ifdef sparc64}
  {$define cpu64bit}
  {$define cpu64bitaddr}
  {$define cpu64bitalu}
  {$define cpuflags}
  {$define cputargethasfixedstack}
  {$define cpurefshaveindexreg}
  {$define cpudelayslot}
  {$define SUPPORT_SAFECALL}
  {$define sparcgen}
{$endif sparc64}

{$ifdef powerpc}
  {$define cpu32bit}
  {$define cpu32bitaddr}
  {$define cpu32bitalu}
  {$define cpuflags}
  {$define cputargethasfixedstack}
  {$define cpumm}
  {$define cpurox}
  {$define cpurefshaveindexreg}
  {$define SUPPORT_SAFECALL}
  {$define SUPPORT_GET_FRAME}
{$endif powerpc}

{$ifdef powerpc64}
  {$define cpu64bit}
  {$define cpu64bitalu}
  {$define cpu64bitaddr}
  {$define cpuflags}
  {$define cputargethasfixedstack}
  {$define cpumm}
  {$define cpurox}
  {$define cpurefshaveindexreg}
  {$define cpuno32bitops}
  {$define SUPPORT_SAFECALL}
{$endif powerpc64}

{$ifdef arm}
  {$define cpu32bit}
  {$define cpu32bitaddr}
  {$define cpu32bitalu}
  {$define cpuflags}
  {$define cpufpemu}
  {$define cpuneedsdivhelper}
  {$define cpurox}
  {$define cputargethasfixedstack}
  {$define cpurefshaveindexreg}
  {$define cpucapabilities}
  {$define fpucapabilities}
  {$define SUPPORT_SAFECALL}
  {$define SUPPORT_GET_FRAME}
  { default to armel }
  {$if not(defined(CPUARM)) and not(defined(CPUARMEB)) and not(defined(FPC_OARM)) and not(defined(FPC_ARMEB)) and not(defined(FPC_ARMHF))}
    {$define FPC_ARMEL}
  {$endif}
  { inherit FPC_ARMEL? }
  {$if defined(CPUARMEL) and not(defined(FPC_OARM)) and not(defined(FPC_ARMEB)) and not(defined(FPC_ARMHF))}
    {$define FPC_ARMEL}
  {$endif}
  { inherit FPC_ARMEB? }
  {$if defined(CPUARMEB) and not(defined(FPC_OARM)) and not(defined(FPC_ARMEL)) and not(defined(FPC_ARMHF))}
    {$define FPC_ARMEB}
  {$endif}
  { inherit FPC_ARMHF? }
  {$if defined(CPUARMHF) and not(defined(FPC_OARM)) and not(defined(FPC_ARMEL)) and not(defined(FPC_ARMEB))}
    {$define FPC_ARMHF}
  {$endif}
  { inherit FPC_OARM? }
  {$if defined(CPUARM) and not(defined(FPC_ARMHF)) and not(defined(FPC_ARMEL)) and not(defined(FPC_ARMEB))}
    {$define FPC_ARMHF}
  {$endif}
{$endif arm}

{$ifdef m68k}
  {$define cpu32bit}
  {$define cpu32bitaddr}
  {$define cpu32bitalu}
  {$define cpuflags}
  {$define cpurox}
  {$define cpufpemu}
  {$define cpurefshaveindexreg}
  {$define cpucapabilities}
  {$define fpucapabilities}
  {$define cpuneedsmulhelper}
  {$define cpuneedsdivhelper}
  {$define cpu_uses_separate_address_registers}
  {$define SUPPORT_SAFECALL}
  {$define SUPPORT_GET_FRAME}
{$endif m68k}

{$ifdef avr}
  {$define cpu8bit}
  {$define cpu16bitaddr}
  {$define cpu8bitalu}
  {$define cpuflags}
  {$define cpunofpu}
  {$define cpunodefaultint}
  {$define cpuneedsdivhelper}
  {$define cpuneedsmulhelper}
  {$define cpurefshaveindexreg}
  {$define cpucapabilities}
{$endif avr}

{$ifdef mipsel}
  {$define mips}
  {$define mips32}
{$else not mipsel}
  { Define both mips and mipseb if mipsel is not defined
    but mips cpu is wanted. }
  {$ifdef mipseb}
    {$define mips}
    {$define mips32}
  {$endif mipseb}
  {$ifdef mips}
    {$define mipseb}
    {$define mips32}
  {$endif mips}
{$endif mipsel}

{$ifdef mips64eb}
  {$define mips}
  {$define mips64}
{$endif mips64eb}

{ Also accept mips64 for big endian 64-bit mips, but only if mips64 el is not set }
{$ifdef mips64}
  {$define mips}
  {$ifndef mips64el}
    {$define mips64eb}
  {$endif not mips64el}
{$endif mips64}

{$ifdef mips64el}
  {$define mips}
  {$define mips64}
{$endif mips64el}

{$ifdef mips}
  {$ifndef mips64}
    {$define cpu32bit}
    {$define cpu32bitalu}
    {$define cpu32bitaddr}
  {$else}
    {$define cpu64bit}
    {$define cpu64bitalu}
    {$define cpu64bitaddr}
  {$endif}
  {$define cpufpemu}
  {$define cpuflags} { Flags are emulated }
  {$define cputargethasfixedstack}
  {$define cpurequiresproperalignment}
  { define cpumm}
  {$define cpurefshaveindexreg}
  {$define cpudelayslot}
  {$define SUPPORT_GET_FRAME}
  {$define SUPPORT_SAFECALL}
{$endif mips}

{$ifdef jvm}
  {$define cpu32bit}
  {$define cpu64bitalu}
  {$define cpu32bitaddr}
  {$define cpuhighleveltarget}
  {$define symansistr}
  {$define SUPPORT_GET_FRAME}
{$endif}

{$ifdef aarch64}
  {$define cpu64bit}
  {$define cpu64bitaddr}
  {$define cpu64bitalu}
  {$define cpuflags}
  {$define cpurox}
  {$define cputargethasfixedstack}
  {$define cpurefshaveindexreg}
  {$define SUPPORT_GET_FRAME}
  {$define SUPPORT_SAFECALL}
  {$define cpucapabilities}
  {$define fpucapabilities}
{$endif aarch64}

{$ifdef riscv32}
  {$define riscv}
  {$define cpu32bit}
  {$define cpu32bitaddr}
  {$define cpu32bitalu}
  {$define cpurox}
  {$define cpufpemu}
  {$define cputargethasfixedstack}
  {$define cpuneedsmulhelper}
  {$define cpuneedsdivhelper}
  {$define cpucapabilities}
  {$define cpurequiresproperalignment}
  {$define SUPPORT_SAFECALL}
{$endif riscv32}

{$ifdef z80}
  {$define cpu8bit}
  {$define cpu16bitaddr}
  {$define cpu8bitalu}
  {$define cpufpemu}
  {$define cpuflags}
  {$define cpunofpu}
  {$define cpunodefaultint}
  {$define cpuneedsdivhelper}
  {$define cpuneedsmulhelper}
  {$define cpucapabilities}
{$endif z80}

{$ifdef riscv64}
  {$define riscv}
  {$define cpu64bit}
  {$define cpu64bitaddr}
  {$define cpu64bitalu}
  {$define cpurox}
  {$define cpufpemu}
  {$define cputargethasfixedstack}
  {$define cpuneedsmulhelper}
  {$define cpuneedsdivhelper}
  {$define cpucapabilities}
  {$define cpurequiresproperalignment}
  {$define SUPPORT_SAFECALL}
{$endif riscv64}

{$ifdef xtensa}
  {$define cpu32bit}
  {$define cpu32bitaddr}
  {$define cpu32bitalu}
  {$define cpufpemu}
  {$define cpuflags} { xtensa has a boolean extension }
  {$define cputargethasfixedstack}
  {$define cpuneedsdivhelper}
  {$define cpucapabilities}
  {$define cpurequiresproperalignment}
  {$define cpufloatintregmov}
{$endif xtensa}

{$ifdef loongarch64}
  {$define loongarch}
  {$define cpu64bit}
  {$define cpu64bitaddr}
  {$define cpu64bitalu}
  {$define cputargethasfixedstack}
  {$define cpucapabilities}
  {$define SUPPORT_SAFECALL}
{$endif loongarch64}

{ Stabs is not officially supported on 64 bit targets by gdb, except on Mac OS X
  (but there we don't support it)
}
{$ifdef cpu64bitaddr}
{$ifndef USE_STABS_64}
{$define NoDbgStabs}
{$endif}
{$endif}

{$if not defined(FPC_HAS_TYPE_EXTENDED) and defined(i386)}
{$ifndef FPC_SOFT_FPUX80}
{$error Cross-compiling from systems without support for an 80 bit extended floating point type to i386 is not yet supported at this time }
{$endif}
{$endif}

{ when compiling with an LLVM-based compiler, default to LLVM unless -dNOLLVM is specified }
{$if defined(CPULLVM) and not defined(NOLLVM)}
{$define LLVM}
{$endif}

{ llvm backends partially use other backends for parameter info calculation,
  alignment info, data sizes etc. They always support 64 bit alu though.
}
{$ifdef llvm}
  {$undef SUPPORT_MMX}
  {$undef cpuneedsmulhelper}
  {$undef cpuneedsdivhelper}
  {$define cpuhighleveltarget}
  {$define cpucg64shiftsupport}
  {$define symansistr}
{$endif}

{$ifdef wasm32}
  {$define wasm}
  {$define cpu32bit}
  {$define cpu64bitalu}
  {$define cpu32bitaddr}
  {$define cpuhighleveltarget}
  {$define cpurox}
  {$define symansistr}
  {$define SUPPORT_GET_FRAME}
{$endif}

