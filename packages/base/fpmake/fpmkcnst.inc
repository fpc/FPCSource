Const
  // Aliases
  AmD64   = X86_64;
  PowerPC = PPC;
  dos = go32v2;

  AllOSs  = [Low(TOS)..High(TOS)];
  AllCPUs = [Low(TCPU)..High(TCPU)];
  
  // Useful
  UnitExt = '.ppu';
  PPUExt  = UnitExt;
  PasExt  = '.pas';
  PPExt   = '.pp';
  IncExt  = '.inc';
  ObjExt  = '.o';
  RstExt  = '.rst';
  LibExt  = '.a';
  SharedLibExt = '.so';
  DLLExt  = '.dll';
  ExeExt  = '.exe';
  ZipExt  = '.zip';

  // Targets
  i386_Linux = 'i386-linux';
  i386_Win32 = 'i386-win32';
  i386_Dos = 'i386-go32v2';
  i386_OS2 = 'i386-os2';
  i386_FreeBSD = 'i386-freebsd';
  i386_NetBSD = 'i386-netsd';
  i386_OpenBSD = 'i386-openbsd';
  i386_netware = 'i386-netware';
  i386_netwlibc = 'i386-netwlibc';
  i386_go32v2 = 'i386-go32v2';
  PPC_Linux = 'ppc-linux';
  powerpc_linux = PPC_linux;
  sparc_linux = 'sparc-linux';
  arm_linux = 'arm-linux';
  ppc_macos = 'ppc-macos';
  ppc_darwin = 'ppc-darwin';

  UnitTargets = [ttUnit,ttExampleUnit];
  ProgramTargets = [ttProgram,ttExampleProgram];


  AllMessages = [vlError,vlWarning,vlInfo,vlCompare,vlCommand];
