{$ifdef Delphi}
const fpmkunitsrc : array[0..1540] of string[240]=(
{$else Delphi}
const fpmkunitsrc : array[0..1540,1..240] of char=(
{$endif Delphi}
  '{'#010+
  '    This file is part of the Free Pascal Makefile Package'#010+
  #010+
  '    Implementation of fpmake classes and functions'#010+
  #010+
  '    Copyright (c) 2007 by the freepascal team'#010+
  #010+
  '    See the file COPYING.FPC, included in this distribution,'#010+
  '    for details abo','ut the copyright.'#010+
  #010+
  '    This program is distributed in the hope that it will be useful,'#010+
  '    but WITHOUT ANY WARRANTY; without even the implied warranty of'#010+
  '    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.'#010+
  #010+
  ' ***************************','****************************************'+
  '***}'#010+
  #010+
  'unit fpMkUnit;'#010+
  #010+
  '{$Mode objfpc}'#010+
  '{$H+}'#010+
  '{$inline on}'#010+
  '{$MODESWITCH TYPEHELPERS}'#010+
  '{$modeswitch advancedrecords}'#010+
  #010+
  '{ For target or cpu dependent dependencies also add an overload where '+
  'you'#010+
  '  can pass on','ly a set of cpus. This is disabled for now because it c'+
  'reates'#010+
  '  an error in the compiler with overload choosing }'#010+
  '{ define cpu_only_overloads}'#010+
  #010+
  'Interface'#010+
  #010+
  '{$ifdef CPULLVM}'#010+
  '  {$define LLVM_INTERFACE_PROBLEM}'#010+
  '{$endif CPULLVM}'#010+
  '{$IFDEF MORPHOS}',#010+
  ' {$DEFINE NO_UNIT_PROCESS}'#010+
  ' {$DEFINE NO_THREADING}'#010+
  '{$ENDIF}'#010+
  #010+
  '{$IFDEF AMIGA}'#010+
  ' {$DEFINE NO_UNIT_PROCESS}'#010+
  ' {$DEFINE NO_THREADING}'#010+
  '{$ENDIF}'#010+
  #010+
  '{$IFDEF AROS}'#010+
  ' {$DEFINE NO_UNIT_PROCESS}'#010+
  ' {$DEFINE NO_THREADING}'#010+
  '{$ENDIF}'#010+
  #010+
  '{$IFDEF OS2}'#010+
  ' {$DEFINE NO_U','NIT_PROCESS}'#010+
  '{$ENDIF OS2}'#010+
  #010+
  '{$IFDEF GO32V2}'#010+
  ' {$DEFINE NO_UNIT_PROCESS}'#010+
  ' {$DEFINE NO_THREADING}'#010+
  '{$ENDIF GO32V2}'#010+
  #010+
  '{$IFDEF NDS}'#010+
  ' {$DEFINE NO_THREADING}'#010+
  '{$ENDIF NDS}'#010+
  #010+
  '{$IFDEF NETBSD}'#010+
  ' { NetBSD pthreads are not yet working, try to use fpmake with','out th'+
  'reads }'#010+
  '  {$DEFINE NO_THREADING}'#010+
  '{$ENDIF NETBSD}'#010+
  #010+
  '{ $define NO_UNIT_PROCESS}'#010+
  '{ $define NO_TAR_SUPPORT}'#010+
  '{ $define NO_UNIT_ZIPPER}'#010+
  #010+
  '{$ifndef NO_UNIT_PROCESS}'#010+
  '  {$define HAS_UNIT_PROCESS}'#010+
  '{$endif NO_UNIT_PROCESS}'#010+
  #010+
  '{$ifndef NO_UNIT_ZIPPER','}'#010+
  '  {$define HAS_UNIT_ZIPPER}'#010+
  '{$endif NO_UNIT_ZIPPER}'#010+
  #010+
  '{$ifndef NO_TAR_SUPPORT}'#010+
  '  {$define HAS_TAR_SUPPORT}'#010+
  '{$endif NO_TAR_SUPPORT}'#010+
  #010+
  '{$ifdef unix}'#010+
  '  {$ifdef HAS_TAR_SUPPORT}'#010+
  '    {$define CREATE_TAR_FILE}'#010+
  '  {$endif HAS_TAR_SUPPORT}'#010+
  '{$endif u','nix}'#010+
  #010+
  '{$IFDEF FPC_DOTTEDUNITS}'#010+
  'uses'#010+
  '  System.Types,'#010+
  '{$ifdef Unix}'#010+
  '  UnixApi.Base,'#010+
  '{$endif Unix}'#010+
  '{$ifdef Windows}'#010+
  '  WinApi.Windows,'#010+
  '{$endif Windows}'#010+
  '{$ifndef NO_THREADING}'#010+
  '{$ifdef Unix}'#010+
  '  UnixApi.CThreads,'#010+
  '{$endif Unix}'#010+
  '{$endif NO_THREADING}',#010+
  '  System.SysUtils, System.Classes'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '  ,System.Process'#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  '{$ifdef HAS_TAR_SUPPORT}'#010+
  '  ,Libx.Libtar'#010+
  '{$endif HAS_TAR_SUPPORT}'#010+
  '{$ifdef HAS_UNIT_ZIPPER}'#010+
  '  ,System.ZLib.Zipper, System.ZLib.Zstream'#010+
  '{$','endif HAS_UNIT_ZIPPER}'#010+
  '  ;'#010+
  '{$ELSE FPC_DOTTEDUNITS}'#010+
  'uses'#010+
  '  Types,'#010+
  '{$ifdef UNIX}'#010+
  '  BaseUnix,'#010+
  '{$endif UNIX}'#010+
  '{$ifdef WINDOWS}'#010+
  '  windows,'#010+
  '{$endif WINDOWS}'#010+
  '  SysUtils, Classes'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '  ,process'#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  '{$ifdef',' HAS_TAR_SUPPORT}'#010+
  '  ,libtar'#010+
  '{$endif HAS_TAR_SUPPORT}'#010+
  '{$ifdef HAS_UNIT_ZIPPER}'#010+
  '  ,zipper, zstream'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  '  ;'#010+
  '{$ENDIF FPC_DOTTEDUNITS}'#010+
  #010+
  '{$IF DECLARED(VOLATILE)}'#010+
  '{$DEFINE HAVE_VOLATILE}'#010+
  '{$ENDIF}'#010+
  #010+
  'Type'#010+
  '{$IF NOT DECLARED(RTLStr','ing)}'#010+
  '  RTLString = AnsiString;'#010+
  '{$ENDIF}'#010+
  #010+
  '{$IF SIZEOF(CHAR)=1}'#010+
  '  TRTLStringDynArray = TStringDynArray;'#010+
  '{$ENDIF}'#010+
  #010+
  '  TFileType = (ftSource,ftUnit,ftObject,ftResource,ftExecutable,ftStat'+
  'icLibrary,'#010+
  '               ftSharedLibrary);'#010+
  '  TFileTypes ','= set of TFileType;'#010+
  #010+
  '  // Please keep this order, see OSCPUSupported below'#010+
  '  TCpu=(cpuNone,'#010+
  '    i386,m68k,powerpc,sparc,x86_64,arm,powerpc64,avr,armeb,'#010+
  '    mips,mipsel,mips64,mips64el,jvm,i8086,aarch64,wasm32,sparc64,riscv'+
  '32,riscv64,xtensa,','z80,loongarch64'#010+
  '  );'#010+
  '  TCPUS = Set of TCPU;'#010+
  #010+
  '  // Please keep this order, see OSCPUSupported below'#010+
  '  TOS=(osNone,'#010+
  '    linux,go32v2,win32,os2,freebsd,beos,netbsd,'#010+
  '    amiga,atari, solaris, qnx, netware, openbsd,wdosx,'#010+
  '    palmos,macosclassic',',darwin,emx,watcom,morphos,netwlibc,'#010+
  '    win64,wince,gba,nds,embedded,symbian,haiku,iphonesim,'#010+
  '    aix,java,android,nativent,msdos,wii,aros,dragonfly,'#010+
  '    win16,freertos,zxspectrum,msxdos,ios,amstradcpc,sinclairql,'#010+
  '    wasip1,human68k,ps1,w','asip1threads,wasip2'#010+
  '  );'#010+
  '  TOSes = Set of TOS;'#010+
  #010+
  '  TCompilerMode = (cmFPC,cmTP,cmObjFPC,cmDelphi,cmMacPas,cmDelphiUnico'+
  'de);'#010+
  '  TCompilerModes = Set of TCompilerMode;'#010+
  #010+
  '  TInstallMode = (imInstall, imUnInstall);'#010+
  #010+
  '  TTargetType = (ttProgram,ttUn','it,ttImplicitUnit,ttCleanOnlyUnit,ttE'+
  'xampleUnit,ttExampleProgram,ttFPDoc,ttSharedLibrary);'#010+
  '  TTargetTypes = set of TTargetType;'#010+
  #010+
  '  TFPDocFormat = (ffHtml, ffHtm, ffXHtml, ffLaTex, ffXMLStruct, ffChm)'+
  ';'#010+
  '  TFPDocFormats = set of TFPDocFormat;'#010,
  #010+
  '  TTargetState = (tsNeutral,tsConsidering,tsNoCompile,tsCompiled,tsIns'+
  'talled,tsNotFound);'#010+
  '  TTargetStates = Set of TTargetState;'#010+
  #010+
  '  TSourceType = (stDoc,stSrc,stExample,stTest);'#010+
  '  TSourceTypes = set of TSourceType;'#010+
  #010+
  '  TVerboseLevel = (vlEr','ror,vlWarning,vlInfo,vldebug,vlCommand);'#010+
  '  TVerboseLevels = Set of TVerboseLevel;'#010+
  #010+
  '  TCommandAt = (caBeforeCompile,caAfterCompile,'#010+
  '                caBeforeInstall,caAfterInstall,'#010+
  '                caBeforeArchive,caAfterArchive,'#010+
  '             ','   caBeforeClean,caAfterClean,'#010+
  '                caBeforeDownload,caAfterDownload);'#010+
  #010+
  '  TDependencyType = (depPackage,depImplicitPackage,depUnit,depInclude)'+
  ';'#010+
  '  TDependencyTypes = set of TDependencyType;'#010+
  #010+
  '  TLogEvent = Procedure (Level : TVerbo','seLevel; Const Msg : String) '+
  'of Object;'#010+
  '  TNotifyProcEvent = procedure(Sender: TObject);'#010+
  #010+
  '  TRunMode = (rmCompile,rmBuild,rmInstall,rmBuildInstall,rmArchive,rmC'+
  'lean,rmDistClean,rmManifest,rmZipInstall,rmPkgList,rmUnInstall,rmInfo,'+
  'rmDocProje','ct,rmDoc);'#010+
  #010+
  '  TBuildMode = (bmOneByOne, bmBuildUnit{, bmSkipImplicitUnits});'#010+
  '  TBuildModes = set of TBuildMode;'#010+
  '  TProcessPackageResult = (ppHandled, ppDelayed);'#010+
  '  TCheckDependencyResult = (cdAvailable, cdNotAvailable, cdNotYetAvail'+
  'able);'#010+
  #010,
  '  { TCompileTarget }'#010+
  #010+
  '  TCompileTarget = record'#010+
  '    OS : TOS;'#010+
  '    CPU : TCPU;'#010+
  '    Subtarget : String;'#010+
  '  Private'#010+
  '    Function GetAsString : String;'#010+
  '    Procedure SetAsString(const aValue : String);'#010+
  '  Public'#010+
  '    Function Equals(const aValue :',' TCompileTarget) : Boolean;'#010+
  '    Function ToString(aLimit83: Boolean): String;'#010+
  '    // Full target. if you need the 8.3 version, use ToString instead.'+
  #010+
  '    Property AsString : String Read GetAsString Write SetAsString;'#010+
  '  end;'#010+
  #010+
  'Const'#010+
  '  // Alias','es'#010+
  '  Amd64 = X86_64;'#010+
  '  PPC = PowerPC;'#010+
  '  PPC64 = PowerPC64;'#010+
  '  DOS = Go32v2;'#010+
  '  MacOSX = Darwin;'#010+
  #010+
  '  AllOSes = [Low(TOS)..High(TOS)];'#010+
  '  AllCPUs = [Low(TCPU)..High(TCPU)];'#010+
  '  AllUnixOSes  = [Linux,FreeBSD,NetBSD,OpenBSD,Darwin,QNX,BeOS,Solaris'+
  ',Ha','iku,iphonesim,ios,aix,Android,dragonfly];'#010+
  '  AllBSDOSes      = [FreeBSD,NetBSD,OpenBSD,Darwin,iphonesim,ios,drago'+
  'nfly];'#010+
  '  AllWindowsOSes  = [Win32,Win64,WinCE];'#010+
  '  AllAmigaLikeOSes = [Amiga,MorphOS,AROS];'#010+
  '  AllLimit83fsOses = [go32v2,os2,emx,','watcom,msdos,win16,atari,human6'+
  '8k];'#010+
  '  AllWebAssemblyOSes = [wasip1, wasip1threads, wasip2];'#010+
  #010+
  '  AllSmartLinkLibraryOSes = [Linux,msdos,win16,palmos]; // OSes that u'+
  'se .a library files for smart-linking'#010+
  '  AllImportLibraryOSes = AllWindowsOSes',' + [os2,emx,netwlibc,netware,'+
  'watcom,go32v2,macosclassic,nativent,msdos,win16];'#010+
  #010+
  '  { This table is kept OS,Cpu because it is easier to maintain (PFV) }'+
  #010+
  '  OSCPUSupported : array[TOS,TCpu] of boolean = ('#010+
  '    { os          none   i386   m68k   ','ppc    sparc  x86_64 arm    p'+
  'pc64  avr    armeb  mips   mipsel mips64 mips64el jvm    i8086 aarch64'+
  ' wasm32 sparc64 riscv32 riscv64  xtensa z80,   loongarch64}'#010+
  '    { none }    ( false, false, false, false, false, false, false, fal'+
  'se, false, ','false, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { linux }   ( false, true,  true,  true,  true,  true,  true,  tru'+
  'e,  false, true , true , true , true , true ,   false, fal','se, true ,'+
  ' false, true ,  true ,  true,    true , false, true),'#010+
  '    { go32v2 }  ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false,  false,  false,   false',', false, false),'#010+
  '    { win32 }   ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false,  false,  false,   false, false, false),'#010+
  '    { os2 }     ( false, true,','  false, false, false, false, false, f'+
  'alse, false, false, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { freebsd } ( false, true,  false, false, false, true,  false, tru'+
  'e,  fal','se, false, false, false, false, false,   false, false, true ,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { beos }    ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false,   false,',' false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { netbsd }  ( false, true,  true,  true,  true,  true,  true,  fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false,  false,  false,   ','false, false, false),'#010+
  '    { amiga }   ( false, false, true,  true,  false, false, false, fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false,  false,  false,   false, false, false),'#010+
  '    { atari }   ( false, ','false, true,  false, false, false, false, f'+
  'alse, false, false, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { solaris } ( false, true,  false, false, true,  true,  false, fal'+
  'se',', false, false, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { qnx }     ( false, false, false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false,   f','alse, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { netware } ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false,  false,  fals','e,   false, false, false),'#010+
  '    { openbsd } ( false, true,  false, false, false, true,  false, fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false,  false,  false,   false, false, false),'#010+
  '    { wdosx }   ( fa','lse, true,  false, false, false, false, false, f'+
  'alse, false, false, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { palmos }  ( false, false, true,  false, false, false, true,  ','f'+
  'alse, false, false, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '{ macosclassic }( false, false, true,  true,  false, false, false, fal'+
  'se, false, false, false, false, false, false',',   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { darwin }  ( false, true,  false, true,  false, true,  false,  tr'+
  'ue, false, false, false, false, false, false,   false, false, true , f'+
  'alse, false,  false, ',' false,   false, false, false),'#010+
  '    { emx }     ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false,  false,  false,   false, false, false),'#010+
  '    { watcom } ',' ( false, true,  false, false, false ,false, false, f'+
  'alse, false, false, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { morphos } ( false, false, false, true,  false ,false, fa','lse, f'+
  'alse, false, false, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { netwlibc }( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false, ','false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { win64   } ( false, false, false, false, false, true,  false, fal'+
  'se, false, false, false, false, false, false,   false, false, true,  f'+
  'alse, false,  fa','lse,  false,   false, false, false),'#010+
  '    { wince    }( false, true,  false, false, false, false, true,  fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false,  false,  false,   false, false, false),'#010+
  '    { gba ','   }  ( false, false, false, false, false, false, true,  f'+
  'alse, false, false, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { nds    }  ( false, false, false, false, false, fals','e, true,  f'+
  'alse, false, false, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { embedded }( false, true,  true,  true,  true,  true,  true,  tru'+
  'e,  true,  true , false, true,  fa','lse, false,    false, true , true '+
  ', true,  false,  true,   true,   true,  true,  false),'#010+
  '    { symbian } ( false, true,  false, false, false, false, true,  fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false',',  false,  false,   false, false, false),'#010+
  '    { haiku }   ( false, true,  false, false, false, true,  false, fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false,  false,  false,   false, false, false),'#010+
  '    {',' iphonesim}( false, true,  false, false, false, true,  false, f'+
  'alse, false, false, false, false, false, false,   false, false, true ,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { aix    }  ( false, false, false, true,  false,',' false, false, t'+
  'rue,  false, false, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { java }    ( false, false, false, false, false, false, false, fal'+
  'se, false, false, false, fals','e, false, false,   true , false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { android } ( false, true,  false, false, false, true,  true,  fal'+
  'se, false, false, false, true,  false, false,   true , false, true,  f'+
  'alse, ','false,  false,  false,   false, false, false),'#010+
  '    { nativent }( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false,  false,  false,   false, false, false),'#010,
  '    { msdos }   ( false, false, false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false,   false, true , false, f'+
  'alse, false,  false,  false,   false, false, false),'#010+
  '    { wii }     ( false, false, false, true , f','alse, false, false, f'+
  'alse, false, false, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { aros }    ( false, true,  false, false, false, true,  true,  fal'+
  'se, false, false, false,',' false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { dragonfly}( false, false, false, false, false, true,  false, fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'a','lse, false,  false,  false,   false, false, false),'#010+
  '    { win16 }   ( false, false, false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false,   false, true , false, f'+
  'alse, false,  false,  false,   false, false, fal','se),'#010+
  '    { freertos }( false, false, false, false, false, false, true,  fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false,  true,   false,   true , false, false),'#010+
  '    {zxspectrum}( false, false, false, fal','se, false, false, false, f'+
  'alse, false, false, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, true,  false),'#010+
  '    { msxdos }  ( false, false, false, false, false, false, false, fal'+
  'se, false, false, f','alse, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, true,  false),'#010+
  '    { ios }     ( false, false, false, false, false, false,  true, fal'+
  'se, false, false, false, false, false, false,   false, false, true',' ,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    {amstradcpc}( false, false, false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false,  false,  false,   false, true,','  false),'#010+
  '    {sinclairql}( false, false, true,  false, false, false, false, fal'+
  'se, false, false, false, false, false, false,   false, false, false, f'+
  'alse, false,  false,  false,   false, false, false),'#010+
  '    { wasip1 }  ( false, false, false',', false, false, false, false, f'+
  'alse, false, false, false, false, false, false,   false, false, false,'+
  ' true,  false,  false,  false,   false, false, false),'#010+
  '    { human68k }( false, false, true,  false, false, false, false, fal'+
  'se, false, fal','se, false, false, false, false,   false, false, false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  '    { ps1 }     ( false, false, false, false, false, false, false, fal'+
  'se, false, false, false, true,  false, false,   false, false,',' false,'+
  ' false, false,  false,  false,   false, false, false),'#010+
  ' {wasip1threads}( false, false, false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false,   false, false, false, t'+
  'rue,  false,  false,  false,   false, ','false, false),'#010+
  '    { wasip2 }  ( false, false, false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false,   false, false, false, t'+
  'rue,  false,  false,  false,   false, false, false)'#010+
  '  );'#010+
  #010+
  '  // Useful'#010+
  '  UnitExt = '#039'.p','pu'#039';'#010+
  '  PPUExt  = UnitExt;'#010+
  '  PasExt  = '#039'.pas'#039';'#010+
  '  PPExt   = '#039'.pp'#039';'#010+
  '  IncExt  = '#039'.inc'#039';'#010+
  '  ObjExt  = '#039'.o'#039';'#010+
  '  LTOExt  = '#039'.bc'#039';'#010+
  '  RstExt  = '#039'.rst'#039';'#010+
  '  RsjExt  = '#039'.rsj'#039';'#010+
  '  LibExt  = '#039'.a'#039';'#010+
  '  SharedLibExt = '#039'.so'#039';'#010+
  '  WasiSharedLibExt = '#039#039';'#010+
  '  DyLibExt ','= '#039'.dylib'#039';'#010+
  '  DLLExt  = '#039'.dll'#039';'#010+
  '  AIXSharedLibExt = '#039'.a'#039';'#010+
  '  ExeExt  = '#039'.exe'#039';'#010+
  '  WasiExeExt  = '#039'.wasm'#039';'#010+
  '  DbgExt  = '#039'.dbg'#039';'#010+
  '  ZipExt  = '#039'.zip'#039';'#010+
  '  FpmkExt = '#039'.fpm'#039';'#010+
  #010+
  '  FPMakePPFile = '#039'fpmake.pp'#039';'#010+
  '  PackageFpcFile = '#039'Package.fpc'#039';'#010+
  '  ManifestFi','le = '#039'manifest.xml'#039';'#010+
  '  DocProjectFileExt = '#039'-docs.xml'#039';'#010+
  '  PkgListFileBase = '#039'pkg-'#039';'#010+
  '  PkgListFileExt = '#039'.lst'#039';'#010+
  #010+
  '  DirNotFound = '#039'<dirnotfound>'#039';'#010+
  #010+
  '  UnitTargets = [ttUnit,ttImplicitUnit,ttCleanOnlyUnit,ttExampleUnit];'+
  #010+
  '  ProgramTargets = [ttP','rogram,ttExampleProgram,ttSharedLibrary];'#010+
  #010+
  '  DefaultMessages = [vlError,vlWarning,vlCommand];'#010+
  '  AllMessages = [vlError,vlWarning,vlCommand,vlInfo];'#010+
  #010+
  'Type'#010+
  '  TTargets = Class;'#010+
  '  TBuildEngine = Class;'#010+
  #010+
  '  { TNamedItem }'#010+
  #010+
  '  TNamedItem = Class(TC','ollectionItem)'#010+
  '  private'#010+
  '    FName: String;'#010+
  '    procedure SetName(const AValue: String);virtual;'#010+
  '  Public'#010+
  '    property Name : String Read FName Write SetName;'#010+
  '  end;'#010+
  #010+
  '  { TNamedCollection }'#010+
  #010+
  '  TNamedCollection = Class(TCollection)'#010+
  '  private',#010+
  '    FUniqueNames: Boolean;'#010+
  '  private'#010+
  '    function GetItem(Index: Integer): TNamedItem;'#010+
  '    procedure SetItem(Index: Integer; AValue: TNamedItem);'#010+
  '  Public'#010+
  '    Function IndexOfName(const AName : String) : Integer;'#010+
  '    Function ItemByName(co','nst AName : String) : TNamedItem;'#010+
  '    Property UniqueNames : Boolean Read FUniqueNames;'#010+
  '    property Items[Index: Integer]: TNamedItem read GetItem write SetI'+
  'tem;'#010+
  '  end;'#010+
  #010+
  '  { TNamedItemList }'#010+
  #010+
  '  TNamedItemList = Class(TFPList)'#010+
  '  private'#010+
  '   ',' function GetNamedItem(Index : Integer): TNamedItem;'#010+
  '    procedure SetNamedItem(Index : Integer; const AValue: TNamedItem);'+
  #010+
  '  public'#010+
  '    Function IndexOfName(const AName : String) : Integer;'#010+
  '    Function ItemByName(const ANAme : String) : T','NamedItem;'#010+
  '    Property NamedItems[Index : Integer] : TNamedItem Read GetNamedIte'+
  'm Write SetNamedItem; default;'#010+
  '  end;'#010+
  #010+
  '  { TCommand }'#010+
  '  TCommand = Class(TNamedItem)'#010+
  '  private'#010+
  '    FAfterCommand: TNotifyEvent;'#010+
  '    FBeforeCommand: TNotifyEven','t;'#010+
  '    FCommand: String;'#010+
  '    FCommandAt: TCommandAt;'#010+
  '    FDestFile: String;'#010+
  '    FIgnoreResult: Boolean;'#010+
  '    FOptions: TStrings;'#010+
  '    FSourceFile: String;'#010+
  '    Function GetOptions : TStrings;'#010+
  '    Procedure SetOptions(Const Value : TStrings);'#010+
  ' ',' Public'#010+
  '    Destructor Destroy; override;'#010+
  '    Function HaveOptions : Boolean;'#010+
  '    Function CmdLineOptions : String;'#010+
  '    Procedure ParseOptions(S : String);'#010+
  '    Property SourceFile : String Read FSourceFile Write FSourceFile;'#010+
  '    Property De','stFile : String Read FDestFile Write FDestFile;'#010+
  '    Property Command : String Read FCommand Write FCommand;'#010+
  '    Property Options : TStrings Read GetOptions Write SetOptions;'#010+
  '    Property At : TCommandAt Read FCommandAt Write FCommandAt;'#010+
  '   ',' Property IgnoreResult : Boolean Read FIgnoreResult Write FIgnore'+
  'Result;'#010+
  '    Property BeforeCommand : TNotifyEvent Read FBeforeCommand Write FB'+
  'eforeCommand;'#010+
  '    Property AfterCommand : TNotifyEvent Read FAfterCommand Write FAft'+
  'erCommand;'#010+
  '  ','end;'#010+
  #010+
  '  { TCommands }'#010+
  #010+
  '  { TCommandEnumerator }'#010+
  #010+
  '  TCommandEnumerator = Class(TCollectionEnumerator)'#010+
  '  public'#010+
  '    function GetCurrent: TCommand;'#010+
  '    property Current: TCommand read GetCurrent;'#010+
  '  end;'#010+
  #010+
  '  TCommands = Class(TNamedCollection)'#010+
  ' ',' private'#010+
  '    FDefaultAt: TCommandAt;'#010+
  '    function GetCommand(const Dest : String): TCommand;'#010+
  '    function GetCommandItem(Index : Integer): TCommand;'#010+
  '    procedure SetCommandItem(Index : Integer; const AValue: TCommand);'+
  #010+
  '  Public'#010+
  '    Functio','n AddCommand(Const Cmd : String) : TCommand;'#010+
  '    Function AddCommand(Const Cmd,Options : String) : TCommand;'#010+
  '    Function AddCommand(Const Cmd,Options,Dest,Source : String) : TCom'+
  'mand;'#010+
  '    Function AddCommand(At : TCommandAt; Const Cmd : St','ring) : TComm'+
  'and;'#010+
  '    Function AddCommand(At : TCommandAt; Const Cmd,Options : String) :'+
  ' TCommand;'#010+
  '    Function AddCommand(At : TCommandAt; Const Cmd,Options, Dest,Sourc'+
  'e : String) : TCommand;'#010+
  '    Function GetEnumerator : TCommandEnumerator',';'#010+
  '    Property CommandItems[Index : Integer] : TCommand Read GetCommandI'+
  'tem Write SetCommandItem;'#010+
  '    Property Commands[Dest : String] : TCommand Read GetCommand; defau'+
  'lt;'#010+
  '    Property DefaultAt : TCommandAt Read FDefaultAt Write FDefaultAt',';'+
  #010+
  '  end;'#010+
  #010+
  '  { TFPVersion }'#010+
  #010+
  '  TFPVersion = Class(TPersistent)'#010+
  '  private'#010+
  '    FMajor,'#010+
  '    FMinor,'#010+
  '    FMicro,'#010+
  '    FBuild    : Integer;'#010+
  '    function GetAsString: String;'#010+
  '    function GetEmpty: Boolean;'#010+
  '    procedure SetAsString(const AValue: S','tring);'#010+
  '  Public'#010+
  '   Procedure Clear;'#010+
  '   Procedure Assign(Source : TPersistent); override;'#010+
  '   Function CompareVersion(AVersion : TFPVersion) : Integer;'#010+
  '   Function SameVersion(AVersion : TFPVersion) : Boolean;'#010+
  '   Property AsString : String R','ead GetAsString Write SetAsString;'#010+
  '   Property Empty : Boolean Read GetEmpty;'#010+
  '  Published'#010+
  '   Property Major : Integer Read FMajor Write FMajor;'#010+
  '   Property Minor : Integer Read FMinor Write FMinor;'#010+
  '   Property Micro : Integer Read FMicro Wr','ite FMicro;'#010+
  '   Property Build : Integer Read FBuild Write FBuild;'#010+
  '  end;'#010+
  #010+
  '  { TConditionalString }'#010+
  '  TConditionalString = Class(TCollectionItem)'#010+
  '  private'#010+
  '    FOSes   : TOSes;'#010+
  '    FCPUs   : TCPUs;'#010+
  '    FValue  : String;'#010+
  '    procedure SetValu','e(const Value: String);'#010+
  '  Public'#010+
  '    Procedure Assign(aSource : TPersistent); override;'#010+
  '    Function Match (aCPU : TCPU; aOS : TOS) : Boolean;'#010+
  '    Function Match (const aValue : String; aCPU : TCPU; aOS : TOS) : B'+
  'oolean;'#010+
  '    Property Value ',': String Read FValue Write SetValue;'#010+
  '    Property OSes  : TOSes Read FOSes Write FOSes;'#010+
  '    Property CPUs : TCPUs Read FCPUS Write FCPUs;'#010+
  '  end;'#010+
  #010+
  '  TConditionalStringClass = class of TConditionalString;'#010+
  #010+
  '  { TConditionalStrings }'#010+
  #010+
  '  TCondit','ionalStrings = Class(TCollection)'#010+
  '  private'#010+
  '    function GetConditionalString(Index : Integer): TConditionalString'+
  ';'#010+
  '    procedure SetConditionalString(Index : Integer; const AValue: TCon'+
  'ditionalString);'#010+
  '  Public'#010+
  '    Procedure AddList(aList ',': TConditionalStrings);'#010+
  '    Function IndexOf(Value : String; aCPU : TCPU; aOS : TOS) : Integer'+
  ';'#010+
  '    Function Find(Value : String; aCPU : TCPU; aOS : TOS) : TCondition'+
  'alString;'#010+
  '    Function Add(Const Value : String) : TConditionalString;inli','ne; '+
  'overload;'#010+
  '    Function Add(Const Value : String;const OSes:TOSes) : TConditional'+
  'String;inline; overload;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs) : TConditional'+
  'String;inline; overload;'#010+
  '{$endif ','cpu_only_overloads}'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs;const OSes:TOSe'+
  's) : TConditionalString; overload;'#010+
  '    Property ConditionalStrings[Index : Integer] : TConditionalString '+
  'Read GetConditionalString Write SetConditiona','lString; default;'#010+
  '  end;'#010+
  #010+
  '  { TConditionalDestString }'#010+
  #010+
  '  TConditionalDestString = Class(TConditionalString)'#010+
  '  private'#010+
  '    FDestPath: string;'#010+
  '  public'#010+
  '    Procedure Assign(aSource : TPersistent); override;'#010+
  '    property DestPath: string read',' FDestPath write FDestPath;'#010+
  '  end;'#010+
  #010+
  '  TConditionalDestStringClass = class of TConditionalDestString;'#010+
  #010+
  '  { TConditionalDestStrings }'#010+
  #010+
  '  TConditionalDestStrings = Class(TConditionalStrings)'#010+
  '  private'#010+
  '    function GetConditionalString(Index : ','Integer): TConditionalDest'+
  'String;'#010+
  '    procedure SetConditionalString(Index : Integer; AValue: TCondition'+
  'alDestString);'#010+
  '  public'#010+
  '    Constructor Create(AClass:TConditionalDestStringClass);'#010+
  '    Function Add(Const Value : String; ADestPath: St','ring) : TConditi'+
  'onalDestString;inline;'#010+
  '    Function Add(Const Value : String;const OSes:TOSes; ADestPath: Str'+
  'ing) : TConditionalDestString;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs; ADestPath',': S'+
  'tring) : TConditionalDestString;inline;'#010+
  '{$endif cpu_only_overloads}'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs;const OSes:TOSe'+
  's; ADestPath: String) : TConditionalDestString;'#010+
  '    Property ConditionalStrings[Index : Integer] : ','TConditionalDestS'+
  'tring Read GetConditionalString Write SetConditionalString; default;'#010+
  '  end;'#010+
  #010+
  '  { TNotifyEventCollection }'#010+
  #010+
  '  TNotifyEventAction = (neaBeforeCompile, neaAfterCompile, neaBeforeIn'+
  'stall, neaAfterInstall,'#010+
  '                      ','  neaBeforeClean, neaAfterClean, neaBeforeArch'+
  'ive, neaAfterArchive,'#010+
  '                        neaBeforeManifest, neaAfterManifest, neaBefore'+
  'PkgList, neaAfterPkgList,'#010+
  '                        neaBeforeUnInstall, neaAfterUnInstall,'#010+
  '             ','           neaBeforeCreateBuildEngine, neaAfterCreateBu'+
  'ildengine,'#010+
  '                        neaBeforeDocProject, neaAfterDocProject,'#010+
  '                        neaBeforeDoc, neaAfterDoc);'#010+
  #010+
  '  TNotifyEventActionSet = set of TNotifyEventAction;'#010+
  #010+
  '  ','TNotifyEventItem = class(TCollectionItem)'#010+
  '  private'#010+
  '    FOnAction: TNotifyEventAction;'#010+
  '    FOnEvent: TNotifyEvent;'#010+
  '    FOnProcEvent: TNotifyProcEvent;'#010+
  '  public'#010+
  '    property OnAction: TNotifyEventAction read FOnAction write FOnActi'+
  'on;'#010+
  '    pr','operty OnEvent: TNotifyEvent read FOnEvent write FOnEvent;'#010+
  '    property OnProcEvent: TNotifyProcEvent read FOnProcEvent write FOn'+
  'ProcEvent;'#010+
  '    procedure CallEvent(Sender: TObject);'#010+
  '  end;'#010+
  #010+
  '  { TNotifyEventEnumerator }'#010+
  #010+
  '  TNotifyEventEnumer','ator = Class(TCollectionEnumerator)'#010+
  '  public'#010+
  '    function GetCurrent: TNotifyEventItem;'#010+
  '    property Current: TNotifyEventItem read GetCurrent;'#010+
  '  end;'#010+
  #010+
  #010+
  '  TNotifyEventCollection = class(TCollection)'#010+
  '  private'#010+
  '    FSupportedActionSet: TNotif','yEventActionSet;'#010+
  '  public'#010+
  '    constructor create(ASupportedActionSet: TNotifyEventActionSet);'#010+
  '    Function GetEnumerator : TNotifyEventEnumerator;'#010+
  '    procedure AppendEvent(AnAction: TNotifyEventAction; AnEvent: TNoti'+
  'fyEvent);'#010+
  '    procedure',' AppendProcEvent(AnACtion: TNotifyEventAction; AnProcEv'+
  'ent: TNotifyProcEvent);'#010+
  '    procedure CallEvents(AnAction: TNotifyEventAction; Sender: TObject'+
  ');'#010+
  '  end;'#010+
  #010+
  '  { TDictionary }'#010+
  #010+
  '  TReplaceFunction = Function (Const AName,Args : String) : S','tring o'+
  'f Object;'#010+
  #010+
  '  TDictionary = Class(TComponent)'#010+
  '  private'#010+
  '    FList : TStringList;'#010+
  '    Procedure ClearItem(Idx : Integer);'#010+
  '  Public'#010+
  '    Constructor Create(AOwner : TComponent); override;'#010+
  '    Destructor Destroy;override;'#010+
  '    Procedure Ad','dVariable(const AName,Value : String);'#010+
  '    Procedure AddFunction(const AName : String; FReplacement : TReplac'+
  'eFunction);'#010+
  '    Procedure RemoveItem(const AName : String);'#010+
  '    Function GetValue(AName : String) : String;'#010+
  '    Function GetValue(c','onst AName,Args : String) : String; virtual;'#010+
  '    Function ReplaceStrings(Const ASource : String; Const MaxDepth: In'+
  'teger = 10) : String; virtual;'#010+
  '    Function Substitute(Const Source : String; const Macros : Array of'+
  ' string) : String; virtu','al;'#010+
  '  end;'#010+
  #010+
  '  { TPackageDictionary }'#010+
  #010+
  '  TPackageDictionary = Class(TDictionary)'#010+
  '  private'#010+
  '    FMasterDictionary: TDictionary;'#010+
  '  Public'#010+
  '    Function GetValue(const AName,Args : String) : String; override;'#010+
  '    property MasterDictionary: TDict','ionary read FMasterDictionary wr'+
  'ite FMasterDictionary;'#010+
  '  end;'#010+
  #010+
  #010+
  '  { TDependency }'#010+
  '  TDependency = Class(TConditionalString)'#010+
  '  private'#010+
  '    FDependencyType : TDependencyType;'#010+
  '    // Package, Unit'#010+
  '    FTarget : TObject;'#010+
  '    FVersion : TFPVersi','on;'#010+
  '    FRequireChecksum : Cardinal;'#010+
  '    // Filenames, Includes'#010+
  '    FTargetFileName : String;'#010+
  '    Function GetVersion : string;'#010+
  '    Procedure SetVersion(const V : string);'#010+
  '  Public'#010+
  '    Constructor Create(aCollection : TCollection);override;',#010+
  '    Procedure Assign(aSource : TPersistent);  override;'#010+
  '    Destructor Destroy;override;'#010+
  '    Property Target : TObject Read FTarget Write FTarget;'#010+
  '    Property DependencyType : TDependencyType Read FDependencyType;'#010+
  '    Property TargetFileN','ame : String Read FTargetFileName Write FTar'+
  'getFileName;'#010+
  '    Property Version : String Read GetVersion Write SetVersion;'#010+
  '    Property RequireChecksum : Cardinal Read FRequireChecksum Write FR'+
  'equireChecksum;'#010+
  '  end;'#010+
  #010+
  '  TResourceFile = Class(T','ConditionalString);'#010+
  #010+
  '  { TPackageVariant }'#010+
  #010+
  '  TPackage = Class;'#010+
  '  TPackageVariant = class(TNamedItem)'#010+
  '  private'#010+
  '    FOptions: TStrings;'#010+
  '    FTargets: TTargets;'#010+
  '    FIncludePath: TConditionalStrings;'#010+
  '    FSourcePath: TConditionalStrings;'#010+
  '  p','ublic'#010+
  '    constructor Create(ACollection: TCollection); override;'#010+
  '    destructor Destroy; override;'#010+
  '    Procedure AddOption(const aValue : string);'#010+
  '    property Options: TStrings read FOptions;'#010+
  '    Property IncludePath : TConditionalStrings',' Read FIncludePath;'#010+
  '    Property SourcePath : TConditionalStrings Read FSourcePath;'#010+
  '    property Targets: TTargets read FTargets;'#010+
  '  end;'#010+
  #010+
  #010+
  '  { TPackageVariantEnumerator }'#010+
  #010+
  '  TPackageVariantEnumerator = Class(TCollectionEnumerator)'#010+
  '  public'#010,
  '    function GetCurrent: TPackageVariant;'#010+
  '    property Current: TPackageVariant read GetCurrent;'#010+
  '  end;'#010+
  #010+
  '  { TPackageVariants }'#010+
  #010+
  '  TPackageVariants = class(TNamedCollection)'#010+
  '  private'#010+
  '    FActivePackageVariantName: string;'#010+
  '    FAutoAddToPac','kage: boolean;'#010+
  '    FDefaultPackageVariantName: string;'#010+
  '    FIsInheritable: boolean;'#010+
  '    FMasterPackage: TPackage;'#010+
  '    FName: string;'#010+
  '    function GetActivePackageVariant: TPackageVariant;'#010+
  '    function GetDefaultPackageVariant: TPackageVaria','nt;'#010+
  '    procedure SetActivePackageVariantName(AValue: string);'#010+
  '    procedure SetDefaultPackageVariantName(AValue: string);'#010+
  '  public'#010+
  '    function Add(AName: String): TPackageVariant; overload; virtual;'#010+
  '    Function GetEnumerator :  TPackageV','ariantEnumerator;'#010+
  '    property Name: string read FName write FName;'#010+
  '    property MasterPackage: TPackage read FMasterPackage;'#010+
  '    property DefaultPackageVariant: TPackageVariant read GetDefaultPac'+
  'kageVariant;'#010+
  '    property ActivePackageVaria','nt: TPackageVariant read GetActivePac'+
  'kageVariant;'#010+
  '    property DefaultPackageVariantName: string read FDefaultPackageVar'+
  'iantName write SetDefaultPackageVariantName;'#010+
  '    property ActivePackageVariantName: string read FActivePackageVaria'+
  'ntNam','e write SetActivePackageVariantName;'#010+
  '    property IsInheritable: boolean read FIsInheritable;'#010+
  '    property AutoAddToPackage: boolean read FAutoAddToPackage;'#010+
  '  end;'#010+
  #010+
  '  { TDependencyEnumerator }'#010+
  #010+
  '  TDependencyEnumerator = Class(TCollectionEnu','merator)'#010+
  '  public'#010+
  '    function GetCurrent: TDependency;'#010+
  '    property Current: TDependency read GetCurrent;'#010+
  '  end;'#010+
  #010+
  '  { TDependencies }'#010+
  #010+
  '  TDependencies = Class(TConditionalStrings)'#010+
  '  Private'#010+
  '    function GetDependency(Index : Integer): TDep','endency;'#010+
  '    procedure SetDependency(Index : Integer; const AValue: TDependency'+
  ');'#010+
  '  Public'#010+
  '    Function Add(Const Value : String) : TDependency;inline;'#010+
  '    Function Add(Const Value : String;const OSes:TOSes) : TDependency;'+
  'inline;'#010+
  '{$ifdef cp','u_only_overloads}'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs) : TDependency;'+
  'inline;'#010+
  '{$endif cpu_only_overloads}'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs;const OSes:TOSe'+
  's) : TDependency;'#010+
  '    Function AddUnit(Const V','alue : String) : TDependency;inline;'#010+
  '    Function AddUnit(Const Value : String;const OSes:TOSes) : TDepende'+
  'ncy;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function AddUnit(Const Value : String;const CPUs:TCPUs) : TDepende'+
  'ncy;inline;'#010+
  '{$endif cpu','_only_overloads}'#010+
  '    Function AddUnit(Const Value : String;const CPUs:TCPUs;const OSes:'+
  'TOSes) : TDependency;'#010+
  '    Function AddInclude(Const Value : String) : TDependency;inline;'#010+
  '    Function AddInclude(Const Value : String;const OSes:TOSes) ',': TDe'+
  'pendency;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function AddInclude(Const Value : String;const CPUs:TCPUs) : TDepe'+
  'ndency;inline;'#010+
  '{$endif cpu_only_overloads}'#010+
  '    Function AddInclude(Const Value : String;const CPUs:TCPUs;const OS'+
  'es:TOSe','s) : TDependency;'#010+
  '    Function GetEnumerator :  TDependencyEnumerator;'#010+
  '    Property Dependencies[Index : Integer] : TDependency Read GetDepen'+
  'dency Write SetDependency; default;'#010+
  '  end;'#010+
  #010+
  '  { TResourceFiles }'#010+
  #010+
  '  TResourceFiles = Class(TConditi','onalStrings)'#010+
  '  public'#010+
  '    Procedure GetInstallFiles(AList : TStrings; const APrefixU, APrefi'+
  'xB : String; ACPU:TCPU; AOS : TOS); virtual;'#010+
  '  end;'#010+
  #010+
  '  { TTarget }'#010+
  #010+
  '  TTarget = Class(TNamedItem)'#010+
  '  private'#010+
  '    FInstall : Boolean;'#010+
  '    FAfterClean:',' TNotifyEvent;'#010+
  '    FAfterCompile: TNotifyEvent;'#010+
  '    FBeforeClean: TNotifyEvent;'#010+
  '    FBeforeCompile: TNotifyEvent;'#010+
  '    FCPUs: TCPUs;'#010+
  '    FIsFPMakePlugin: Boolean;'#010+
  '    FOSes: TOSes;'#010+
  '    FMode: TCompilerMode;'#010+
  '    FResourceStrings: Boolean;'#010+
  '   ',' FObjectPath,'#010+
  '    FUnitPath,'#010+
  '    FIncludePath : TConditionalStrings;'#010+
  '    FSubTargets: TRTLStringDynArray;'#010+
  '    FDependencies : TDependencies;'#010+
  '    FResourceFiles : TResourceFiles;'#010+
  '    FCommands : TCommands;'#010+
  '    FDirectory: String;'#010+
  '    FExtens','ion: String;'#010+
  '    FExeName : String;'#010+
  '    FTargetSourceFileName : String;'#010+
  '    FFileType: TFileType;'#010+
  '    FOptions: TStrings;'#010+
  '    FFPCTarget: String;'#010+
  '    FTargetState: TTargetState;'#010+
  '    FTargetType: TTargetType;'#010+
  '    FXML: string;'#010+
  '    function G','etOptions: TStrings;'#010+
  '    procedure SetOptions(const AValue: TStrings);'#010+
  '  Protected'#010+
  '    Function GetSourceFileName : String; virtual;'#010+
  '    Function GetUnitFileName : String; virtual;'#010+
  '    function GetUnitLibFileName(AOS: TOS): String; virtual;',#010+
  '    Function GetObjectFileName : String; virtual;'#010+
  '    Function GetLTOFileName : String; virtual;'#010+
  '    Function GetBinFileBase: String;'#010+
  '    function GetRSTFileName : String; Virtual;'#010+
  '    function GetRSJFileName : String; Virtual;'#010+
  '    functio','n GetImportLibFileName(AOS : TOS) : String; Virtual;'#010+
  '    Function GetProgramFileName(AOS : TOS) : String; Virtual;'#010+
  '    Function GetProgramDebugFileName(AOS : TOS) : String; Virtual;'#010+
  '    Function GetLibraryFileName(AOS : TOS) : String; Virtu','al;'#010+
  '    Function GetLibraryDebugFileName(AOS : TOS) : String; Virtual;'#010+
  '  Public'#010+
  '    Constructor Create(ACollection : TCollection); override;'#010+
  '    Destructor Destroy; override;'#010+
  '    procedure AssignTo(Dest: TPersistent); override;'#010+
  '    Function','  GetOutputFileName (AOs : TOS) : String; Virtual;'#010+
  '    Function HaveOptions : Boolean;'#010+
  '    Procedure AddOption(const aValue : string);'#010+
  '    Function SubTargetAllowed(Const aSubTarget : String) : Boolean;'#010+
  '    Function SubTargetsAsString: Stri','ng;'#010+
  '    procedure SetName(const AValue: String);override;'#010+
  '    procedure SetExeName(const AValue: String);'#010+
  '    procedure SetXML(const AValue: string);'#010+
  '    // Deprecated API'#010+
  '    Procedure GetCleanFiles(List : TStrings; const APrefixU, APrefix','B'+
  ' : String; ACPU:TCPU; AOS : TOS; const aSubTarget : String); virtual; '+
  'deprecated '#039'use TcompileTarget instead'#039';'#010+
  '    Procedure GetArchiveFiles(List : TStrings; ACPU:TCPU; AOS : TOS); '+
  'virtual; deprecated '#039'use TcompileTarget instead'#039';'#010+
  '    Proc','edure GetInstallFiles(List : TStrings; const APrefixU, APref'+
  'ixB : String; ACPU:TCPU; AOS : TOS; const aSubTarget : String); virtua'+
  'l; deprecated '#039'use TcompileTarget instead'#039';'#010+
  '    Procedure GetCleanFiles(List : TStrings; const APrefixU, APref','ix'+
  'B : String; const aTarget : TcompileTarget); virtual;'#010+
  '    Procedure GetInstallFiles(List : TStrings; const APrefixU, APrefix'+
  'B : String; const aTarget : TCompileTarget); virtual;'#010+
  '    Procedure GetArchiveFiles(List : TStrings; const aTarget',' : TComp'+
  'ileTarget); virtual;'#010+
  '    Property Dependencies : TDependencies Read FDependencies;'#010+
  '    Property ResourceFiles: TResourceFiles read FResourceFiles;'#010+
  '    Property Commands : TCommands Read FCommands;'#010+
  '    Property State : TTargetState R','ead FTargetState;'#010+
  '    Property TargetType : TTargetType Read FTargetType Write FTargetTy'+
  'pe;'#010+
  '    Property OSes : TOSes Read FOSes Write FOSes;'#010+
  '    Property CPUs : TCPUs Read FCPUs Write FCPUs;'#010+
  '    Property SubTargets : TRTLStringDynArray Rea','d FSubTargets Write '+
  'FSubTargets;'#010+
  '    Property Mode : TCompilerMode Read FMode Write FMode;'#010+
  '    Property Options : TStrings Read GetOptions Write SetOptions;'#010+
  '    Property SourceFileName: String Read GetSourceFileName ;'#010+
  '    Property UnitFileN','ame : String Read GetUnitFileName;'#010+
  '    Property ObjectFileName : String Read GetObjectFileName;'#010+
  '    Property LTOFileName : String Read GetLTOFileName;'#010+
  '    Property RSTFileName : String Read GetRSTFileName;'#010+
  '    Property RSJFileName : String ','Read GetRSJFileName;'#010+
  '    Property FPCTarget : String Read FFPCTarget Write FFPCTarget;'#010+
  '    Property Extension : String Read FExtension Write FExtension;'#010+
  '    Property FileType : TFileType Read FFileType Write FFileType;'#010+
  '    Property Director','y : String Read FDirectory Write FDirectory;'#010+
  '    Property ResourceStrings : Boolean Read FResourceStrings Write FRe'+
  'sourceStrings;'#010+
  '    Property Install : Boolean Read FInstall Write FInstall;'#010+
  '    Property TargetSourceFileName: String Read FT','argetSourceFileName'+
  ';'#010+
  '    Property ObjectPath : TConditionalStrings Read FObjectPath;'#010+
  '    Property UnitPath : TConditionalStrings Read FUnitPath;'#010+
  '    Property IncludePath : TConditionalStrings Read FIncludePath;'#010+
  '    Property XML: string Read',' FXML Write SetXML;'#010+
  '    Property IsFPMakePlugin : Boolean read FIsFPMakePlugin write FIsFP'+
  'MakePlugin;'#010+
  '    // Events.'#010+
  '    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FB'+
  'eforeCompile;'#010+
  '    Property AfterCompile : TNotifyEven','t Read FAfterCompile Write FA'+
  'fterCompile;'#010+
  '    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBefor'+
  'eClean;'#010+
  '    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterCl'+
  'ean;'#010+
  '  end;'#010+
  #010+
  #010+
  '  { TTargetEnumerator }'#010+
  #010+
  '  TTargetEn','umerator = Class(TCollectionEnumerator)'#010+
  '  public'#010+
  '    function GetCurrent: TTarget;'#010+
  '    property Current: TTarget read GetCurrent;'#010+
  '  end;'#010+
  #010+
  '  { TTargets }'#010+
  #010+
  '  TTargets = Class(TNamedCollection)'#010+
  '  private'#010+
  '    function GetTargetItem(Index : Inte','ger): TTarget;'#010+
  '    function GetTarget(const AName : String): TTarget;'#010+
  '    procedure SetTargetItem(Index : Integer; const AValue: TTarget);'#010+
  '  Public'#010+
  '    Function AddFPDoc(Const AUnitName, AXMLName : String) : TTarget;in'+
  'line;'#010+
  '    Function Add','Unit(Const AUnitName : String) : TTarget;inline;'#010+
  '    Function AddUnit(Const AUnitName : String;const OSes:TOSes) : TTar'+
  'get;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function AddUnit(Const AUnitName : String;const CPUs:TCPUs) : TTar'+
  'get;inline;',#010+
  '{$endif cpu_only_overloads}'#010+
  '    Function AddUnit(Const AUnitName : String;const CPUs:TCPUs;const O'+
  'Ses:TOSes) : TTarget;'#010+
  '    Function AddImplicitUnit(Const AUnitName : String;InstallUnit:bool'+
  'ean=true) : TTarget;inline;'#010+
  '    Function AddImpli','citUnit(Const AUnitName : String;const OSes:TOS'+
  'es;InstallUnit:boolean=true) : TTarget;inline;'#010+
  '    Function AddImplicitUnit(Const AUnitName : String;const CPUs:TCPUs'+
  ';InstallUnit:boolean=true) : TTarget;inline;'#010+
  '    Function AddImplicitUnit(Co','nst AUnitName : String;const CPUs:TCP'+
  'Us;const OSes:TOSes;InstallUnit:boolean=true) : TTarget;'#010+
  '    Function AddProgram(Const AProgramName : String) : TTarget;inline;'+
  #010+
  '    Function AddProgram(Const AProgramName : String;const OSes:TOSes) '+
  ': TTa','rget;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function AddProgram(Const AProgramName : String;const CPUs:TCPUs) '+
  ': TTarget;inline;'#010+
  '{$endif cpu_only_overloads}'#010+
  '    Function AddProgram(Const AProgramName : String;const CPUs:TCPUs;c'+
  'onst OSes:TOS','es) : TTarget;'#010+
  '    Function AddLibrary(Const ALibraryName : String) : TTarget;inline;'+
  #010+
  '    Function AddLibrary(Const ALibraryName : String;const OSes:TOSes) '+
  ': TTarget;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function AddLibrary(Const ALibrary','Name : String;const CPUs:TCPUs'+
  ') : TTarget;inline;'#010+
  '{$endif cpu_only_overloads}'#010+
  '    Function AddLibrary(Const ALibraryName : String;const CPUs:TCPUs;c'+
  'onst OSes:TOSes) : TTarget;'#010+
  '    Function AddExampleUnit(Const AUnitName : String) : TTarget;','inli'+
  'ne;'#010+
  '    Function AddExampleUnit(Const AUnitName : String;const OSes:TOSes)'+
  ' : TTarget;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function AddExampleUnit(Const AUnitName : String;const CPUs:TCPUs)'+
  ' : TTarget;inline;'#010+
  '{$endif cpu_only_overloads','}'#010+
  '    Function AddExampleUnit(Const AUnitName : String;const CPUs:TCPUs;'+
  'const OSes:TOSes) : TTarget;'#010+
  '    Function AddExampleProgram(Const AProgramName : String) : TTarget;'+
  'inline;'#010+
  '    Function AddExampleProgram(Const AProgramName : String;co','nst OSe'+
  's:TOSes) : TTarget;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function AddExampleProgram(Const AProgramName : String;const CPUs:'+
  'TCPUs) : TTarget;inline;'#010+
  '{$endif cpu_only_overloads}'#010+
  '    Function AddExampleProgram(Const AProgramName : Str','ing;const CPU'+
  's:TCPUs;const OSes:TOSes) : TTarget;'#010+
  '    Function GetEnumerator : TTargetEnumerator;'#010+
  '    Property Targets[AName : String] : TTarget Read GetTarget; default'+
  ';'#010+
  '    Property TargetItems[Index : Integer] : TTarget Read GetTargetItem',
  ' Write SetTargetItem;'#010+
  '  end;'#010+
  #010+
  '  { TSource }'#010+
  #010+
  '  TSource = Class(TNamedItem)'#010+
  '  private'#010+
  '    FSourceType : TSourceType;'#010+
  '    FInstallSourcePath : string;'#010+
  '    function GetInstallSourcePath: string;'#010+
  '  Public'#010+
  '    Constructor Create(ACollection : TC','ollection); override;'#010+
  '    Destructor Destroy; override;'#010+
  '    Procedure GetInstallFiles(List : TStrings); virtual;'#010+
  '    property SourceType : TSourceType read FSourceType;'#010+
  '    property InstallSourcePath : string read GetInstallSourcePath;'#010+
  '  en','d;'#010+
  #010+
  '  { TSources }'#010+
  #010+
  '  { TSourceEnumerator }'#010+
  #010+
  '  TSourceEnumerator = Class(TCollectionEnumerator)'#010+
  '  public'#010+
  '    function GetCurrent: TSource;'#010+
  '    property Current: TSource read GetCurrent;'#010+
  '  end;'#010+
  #010+
  '  TSources = Class(TNamedCollection)'#010+
  '  private',#010+
  '    function GetSourceItem(Index : Integer): TSource;'#010+
  '    procedure SetSourceItem(Index : Integer; const AValue: TSource);'#010+
  '  public'#010+
  '    Function AddDoc(const AFiles : String) : TSource;'#010+
  '    Function AddDoc(const AFiles : String; AInstallSo','urcePath : Stri'+
  'ng) : TSource;'#010+
  '    Function AddSrc(const AFiles : String) : TSource;'#010+
  '    Function AddExample(const AFiles : String) : TSource;'#010+
  '    Function AddExample(const AFiles : String; AInstallSourcePath : St'+
  'ring) : TSource;'#010+
  '    Functio','n AddTest(const AFiles : String) : TSource;'#010+
  '    Function GetEnumerator : TSourceEnumerator;'#010+
  '    procedure AddDocFiles(const AFileMask, ASearchPathPrefix: string; '+
  'Recursive: boolean = False; AInstallSourcePath : String = '#039#039');'#010+
  '    procedure A','ddSrcFiles(const AFileMask, ASearchPathPrefix: string'+
  '; Recursive: boolean = False);'#010+
  '    procedure AddExampleFiles(const AFileMask, ASearchPathPrefix: stri'+
  'ng; Recursive: boolean = False; AInstallSourcePath : String = '#039#039')'+
  ';'#010+
  '    procedure AddTe','stFiles(const AFileMask, ASearchPathPrefix: strin'+
  'g; Recursive: boolean = False);'#010+
  '    Property SourceItems[Index : Integer] : TSource Read GetSourceItem'+
  ' Write SetSourceItem;default;'#010+
  '  end;'#010+
  #010+
  '  { TPackage }'#010+
  #010+
  '  TPackage = Class(TNamedItem)'#010+
  '  pr','ivate'#010+
  '    FAfterArchive: TNotifyEvent;'#010+
  '    FAfterArchiveProc: TNotifyProcEvent;'#010+
  '    FAfterClean: TNotifyEvent;'#010+
  '    FAfterCleanProc: TNotifyProcEvent;'#010+
  '    FAfterCompile: TNotifyEvent;'#010+
  '    FAfterCompileProc: TNotifyProcEvent;'#010+
  '    FAfterInstal','l: TNotifyEvent;'#010+
  '    FAfterInstallProc: TNotifyProcEvent;'#010+
  '    FAfterManifest: TNotifyEvent;'#010+
  '    FAfterManifestProc: TNotifyProcEvent;'#010+
  '    FAfterPkgList: TNotifyEvent;'#010+
  '    FAfterPkgListProc: TNotifyProcEvent;'#010+
  '    FBeforeArchive: TNotifyEvent',';'#010+
  '    FBeforeArchiveProc: TNotifyProcEvent;'#010+
  '    FBeforeClean: TNotifyEvent;'#010+
  '    FBeforeCleanProc: TNotifyProcEvent;'#010+
  '    FBeforeCompile: TNotifyEvent;'#010+
  '    FBeforeCompileProc: TNotifyProcEvent;'#010+
  '    FBeforeInstall: TNotifyEvent;'#010+
  '    FBeforeIns','tallProc: TNotifyProcEvent;'#010+
  '    FBeforeManifest: TNotifyEvent;'#010+
  '    FBeforeManifestProc: TNotifyProcEvent;'#010+
  '    FBeforePkgList: TNotifyEvent;'#010+
  '    FBeforePkgListProc: TNotifyProcEvent;'#010+
  '    FBuildMode: TBuildMode;'#010+
  '    FFlags: TStrings;'#010+
  '    FFPD','ocFormat: TFPDocFormats;'#010+
  '    FIsFPMakeAddIn: boolean;'#010+
  '    FNamespaceMap: String;'#010+
  '    FSubTargets: TRTLStringDynArray;'#010+
  '    FSupportBuildModes: TBuildModes;'#010+
  '    FUnitPath,'#010+
  '    FObjectPath,'#010+
  '    FIncludePath,'#010+
  '    FSourcePath,'#010+
  '    FExamplePath,'#010,
  '    FTestPath,'#010+
  '    FCleanFiles   : TConditionalStrings;'#010+
  '    FInstallFiles : TConditionalDestStrings;'#010+
  '    FDependencies : TDependencies;'#010+
  '    FCPUs: TCPUs;'#010+
  '    FOSes: TOSes;'#010+
  '    FTargetState: TTargetState;'#010+
  '    FTargets: TTargets;'#010+
  '    FSources',': TSources;'#010+
  '    FDirectory: String;'#010+
  '    FOptions: TStrings;'#010+
  '    FTransmitOptions: TStrings;'#010+
  '    FFileName: String;'#010+
  '    FShortName: String; { Used to generate the short 8.3 zip file name'+
  ', must be 4 characters at most }'#010+
  '    FAuthor: String;'#010+
  ' ','   FLicense: String;'#010+
  '    FHomepageURL: String;'#010+
  '    FDownloadURL: String;'#010+
  '    FVersion: TFPVersion;'#010+
  '    FEmail : String;'#010+
  '    FNeedLibC : Boolean;'#010+
  '    FCommands : TCommands;'#010+
  '    FDescriptionFile : String;'#010+
  '    FDescription : String;'#010+
  '    FInsta','lledChecksum : Cardinal;'#010+
  '    FUnitsOutputDir: String;'#010+
  '    FPackageUnitInstallDir: String;'#010+
  '    // Cached directory of installed packages'#010+
  '    FUnitDir : String;'#010+
  '    FUnitConfigFileName : String;'#010+
  '    // Used by buildunits'#010+
  '    FBUTargets: TTarg','ets;'#010+
  '    FBUTarget: TTarget;'#010+
  '    // Used to identify if package is being processed by a thread'#010+
  '    FProcessing : boolean;'#010+
  '    // Dictionary'#010+
  '    FDictionary : TDictionary;'#010+
  '    // Is set when all sourcefiles are found'#010+
  '    FAllFilesResolved: b','oolean;'#010+
  '    FPackageVariants: TFPList;'#010+
  '{$ifndef NO_THREADING}'#010+
  '    FResolveDirsCS: TRTLCriticalSection;'#010+
  '{$endif}'#010+
  '    procedure ChangePaths(Aliases: TStrings; aTarget: TCompileTarget);'+
  #010+
  '    Function GetDescription : string;'#010+
  '    function GetDic','tionary: TDictionary;'#010+
  '    Function GetFileName : string;'#010+
  '    Function GetShortName : string;'#010+
  '    function GetOptions: TStrings;'#010+
  '    function GetTransmitOptions: TStrings;'#010+
  '    Function GetVersion : string;'#010+
  '    procedure SetOptions(const AVal','ue: TStrings);'#010+
  '    procedure SetTransmitOptions(AValue: TStrings);'#010+
  '    Procedure SetVersion(const V : string);'#010+
  '    procedure TransformAliases(Aliases: TStrings);'#010+
  '  Protected'#010+
  '    procedure SetName(const AValue: String);override;'#010+
  '    procedur','e SaveUnitConfigToStringList(Const AStringList: TStrings'+
  ';ACPU:TCPU;AOS:TOS); virtual;'#010+
  '    property Dictionary: TDictionary read GetDictionary;'#010+
  '    property PackageVariantsList: TFPList read FPackageVariants;'#010+
  '  Public'#010+
  '    constructor Create(','ACollection: TCollection); override;'#010+
  '    destructor destroy; override;'#010+
  '    Function HaveOptions : Boolean;'#010+
  '    // Deprecated'#010+
  '    Function  GetUnitsOutputDir(ACPU:TCPU; AOS : TOS; const aSubTarget'+
  ' : String ):String; deprecated '#039'use TCompileT','arget version inste'+
  'ad'#039';'#010+
  '    Function  GetUnitConfigOutputFilename(ACPU:TCPU; AOS : TOS; const '+
  'aSubTarget : String):String; deprecated '#039'use TCompileTarget versio'+
  'n instead'#039';'#010+
  '    Function  GetBinOutputDir(ACPU:TCPU; AOS : TOS; const aSubTarget',' '+
  ': String) : String; deprecated '#039'use TCompileTarget version instead'+
  #039';'#010+
  '    Procedure GetCleanFiles(List : TStrings; ACPU:TCPU; AOS : TOS; con'+
  'st aSubTarget : String); virtual; deprecated '#039'use TCompileTarget v'+
  'ersion instead'#039';'#010+
  '    Procedure Get','ArchiveFiles(List : TStrings; ACPU:TCPU; AOS : TOS)'+
  '; virtual;deprecated '#039'use TCompileTarget version instead'#039';'#010+
  #010+
  '    Function  GetUnitsOutputDir(const aTarget : TCompileTarget):String'+
  ';'#010+
  '    Function  GetUnitConfigOutputFilename(const aTarget :',' TCompileTa'+
  'rget):String;'#010+
  '    Procedure InheritPackageVariantsFromDependency(ADependencyPackage:'+
  ' TPackage);'#010+
  '    Function  GetPackageVariantsByName(AName: string): TPackageVariant'+
  's;'#010+
  '    Procedure SetUnitsOutputDir(AValue: string);'#010+
  '    Functio','n  GetPackageUnitInstallDir(ACPU:TCPU; AOS : TOS; const a'+
  'SubTarget : String):String;'#010+
  '    Procedure SetPackageUnitInstallDir(AValue: string);'#010+
  '    Function  GetBinOutputDir(const aTarget : TCompileTarget) : String'+
  ';'#010+
  '    // Output directory of ','example units and programs: the examples '+
  'subdirectory of the units output directory.'#010+
  '    Function  GetExamplesOutputDir(const aTarget : TCompileTarget) : S'+
  'tring;'#010+
  '    Procedure GetCleanFiles(List : TStrings; const aTarget : TCompileT'+
  'arget); ','virtual;'#010+
  '    procedure GetInstallFiles(List: TStrings;Types : TTargetTypes; con'+
  'st aTarget : TCompileTarget); virtual;'#010+
  '    procedure GetInstallSourceFiles(List: TStrings; SourceTypes : TSou'+
  'rceTypes; TargetTypes : TTargetTypes); virtual;'#010+
  '    ','Procedure GetArchiveFiles(List : TStrings; aTarget : TCompileTar'+
  'get); virtual;'#010+
  '    Procedure GetArchiveSourceFiles(List : TStrings); virtual;'#010+
  '    Procedure GetManifest(Manifest : TStrings);'#010+
  '    Procedure ListPackage(PkgList : TStrings);'#010+
  '   ',' Procedure AddPackageVariant(APackageVariant: TPackageVariants);'#010+
  '    procedure ApplyPackageVariantToCompilerOptions(ACompilerOptions: t'+
  'strings);'#010+
  '    procedure SetDefaultPackageVariant;'#010+
  '    procedure LoadUnitConfigFromFile(Const AFileName: S','tring);'#010+
  '    procedure SaveUnitConfigToFile(Const AFileName: String;ACPU:TCPU;A'+
  'OS:TOS);'#010+
  '    procedure EnterResolveDirsCS;'#010+
  '    procedure LeaveResolveDirsCS;'#010+
  '    Function AddTargetsFromDir(const aDirectory : String; const aMask '+
  ': String = '#039#039') ',': longint;'#010+
  '    // applies namespaces if map is set'#010+
  '    procedure ApplyNameSpaces(aEngine : TBuildEngine; aFileName : stri'+
  'ng; aTarget : TCompileTarget);'#010+
  '    procedure ApplyNameSpaces(aEngine : TBuildEngine; aTarget : TCompi'+
  'leTarget);'#010+
  '    Fun','ction SubTargetAllowed(Const aSubTarget : String) : Boolean;'#010+
  '    Property Version : String Read GetVersion Write SetVersion;'#010+
  '    Property FileName : String Read GetFileName Write FFileName;'#010+
  '    Property ShortName : String Read GetShortName ','Write FShortName;'#010+
  '    Property HomepageURL : String Read FHomepageURL Write FHomepageURL'+
  ';'#010+
  '    Property DownloadURL : String Read FDownloadURL Write FDownloadURL'+
  ';'#010+
  '    Property Email : String Read FEmail Write FEmail;'#010+
  '    Property Author : St','ring Read FAuthor Write FAuthor;'#010+
  '    Property License : String Read FLicense Write FLicense;'#010+
  '    Property Directory : String Read FDirectory Write FDirectory;'#010+
  '    Property Description : String Read GetDescription Write FDescripti'+
  'on;'#010+
  '    Pro','perty DescriptionFile : String Read FDescriptionFile Write FD'+
  'escriptionFile;'#010+
  '    Property InstalledChecksum : Cardinal Read FInstalledChecksum Writ'+
  'e FInstalledChecksum;'#010+
  '    Property IsFPMakeAddIn: boolean read FIsFPMakeAddIn write FIsFPMak'+
  'e','AddIn;'#010+
  '    Property SupportBuildModes: TBuildModes read FSupportBuildModes wr'+
  'ite FSupportBuildModes;'#010+
  '    Property BuildMode: TBuildMode read FBuildMode;'#010+
  '    Property Flags: TStrings read FFlags;'#010+
  '    Property PackageVersion: TFPVersion read ','FVersion;'#010+
  '    // Options which are passed to the compiler for packages which dep'+
  'end on'#010+
  '    // this package.'#010+
  '    Property TransmitOptions: TStrings Read GetTransmitOptions Write S'+
  'etTransmitOptions;'#010+
  '    // Compiler options.'#010+
  '    Property OSes ',': TOSes Read FOSes Write FOSes;'#010+
  '    Property CPUs : TCPUs Read FCPUs Write FCPUs;'#010+
  '    Property SubTargets : TRTLStringDynArray Read FSubTargets Write FS'+
  'ubTargets;'#010+
  '    Property NeedLibC : Boolean Read FNeedLibC Write FNeedLibC;'#010+
  '    Property ','Options: TStrings Read GetOptions Write SetOptions;'#010+
  '    Property UnitPath : TConditionalStrings Read FUnitPath;'#010+
  '    Property ObjectPath : TConditionalStrings Read FObjectPath;'#010+
  '    Property IncludePath : TConditionalStrings Read FIncludePath',';'#010+
  '    Property SourcePath : TConditionalStrings Read FSourcePath;'#010+
  '    Property ExamplePath : TConditionalStrings Read FExamplePath;'#010+
  '    Property TestPath : TConditionalStrings Read FTestPath;'#010+
  '    Property FPDocFormat: TFPDocFormats read FFP','DocFormat write FFPD'+
  'ocFormat;'#010+
  '    // Targets and dependencies'#010+
  '    Property InstallFiles : TConditionalDestStrings Read FInstallFiles'+
  ';'#010+
  '    Property CleanFiles : TConditionalStrings Read FCleanFiles;'#010+
  '    Property Dependencies : TDependencies ','Read FDependencies;'#010+
  '    Property Commands : TCommands Read FCommands;'#010+
  '    Property State : TTargetState Read FTargetState Write FTargetState'+
  ';'#010+
  '    Property Targets : TTargets Read FTargets;'#010+
  '    Property Sources : TSources Read FSources;'#010+
  '    ','Property UnitDir : String Read FUnitDir Write FUnitDir;'#010+
  '    Property UnitConfigFileName: String read FUnitConfigFileName write'+
  ' FUnitConfigFileName;'#010+
  '    Property NamespaceMap : String Read FNamespaceMap Write FNameSpace'+
  'Map;'#010+
  '    // events'#010+
  '   ',' Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write '+
  'FBeforeCompile;'#010+
  '    Property BeforeCompileProc : TNotifyProcEvent Read FBeforeCompileP'+
  'roc write FBeforeCompileProc;'#010+
  '    Property AfterCompile : TNotifyEvent Read FAfterCompile',' Write FA'+
  'fterCompile;'#010+
  '    Property AfterCompileProc : TNotifyProcEvent Read FAfterCompilePro'+
  'c Write FAfterCompileProc;'#010+
  '    Property BeforeInstall : TNotifyEvent Read FBeforeInstall Write FB'+
  'eforeInstall;'#010+
  '    Property BeforeInstallProc : TNot','ifyProcEvent Read FBeforeInstal'+
  'lProc Write FBeforeInstallProc;'#010+
  '    Property AfterInstall : TNotifyEvent Read FAfterInstall Write FAft'+
  'erInstall;'#010+
  '    Property AfterInstallProc : TNotifyProcEvent Read FAfterInstallPro'+
  'c Write FAfterInstallProc;',#010+
  '    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBefor'+
  'eClean;'#010+
  '    Property BeforeCleanProc : TNotifyProcEvent Read FBeforeCleanProc '+
  'Write FBeforeCleanProc;'#010+
  '    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfte','r'+
  'Clean;'#010+
  '    Property AfterCleanProc : TNotifyProcEvent Read FAfterCleanProc Wr'+
  'ite FAfterCleanProc;'#010+
  '    Property BeforeArchive : TNotifyEvent Read FBeforeArchive Write FB'+
  'eforeArchive;'#010+
  '    Property BeforeArchiveProc : TNotifyProcEvent Read FB','eforeArchiv'+
  'eProc Write FBeforeArchiveProc;'#010+
  '    Property AfterArchive : TNotifyEvent Read FAfterArchive Write FAft'+
  'erArchive;'#010+
  '    Property AfterArchiveProc : TNotifyProcEvent Read FAfterArchivePro'+
  'c Write FAfterArchiveProc;'#010+
  '    Property Before','Manifest : TNotifyEvent Read FBeforeManifest Writ'+
  'e FBeforeManifest;'#010+
  '    Property BeforeManifestProc : TNotifyProcEvent Read FBeforeManifes'+
  'tProc Write FBeforeManifestProc;'#010+
  '    Property AfterManifest : TNotifyEvent Read FAfterManifest Write F',
  'AfterManifest;'#010+
  '    Property AfterManifestProc : TNotifyProcEvent Read FAfterManifestP'+
  'roc Write FAfterManifestProc;'#010+
  '    Property BeforePkgList : TNotifyEvent Read FBeforePkgList Write FB'+
  'eforePkgList;'#010+
  '    Property BeforePkgListProc : TNotifyP','rocEvent Read FBeforePkgLis'+
  'tProc Write FBeforePkgListProc;'#010+
  '    Property AfterPkgList : TNotifyEvent Read FAfterPkgList Write FAft'+
  'erPkgList;'#010+
  '    Property AfterPkgListProc : TNotifyProcEvent Read FAfterPkgListPro'+
  'c Write FAfterPkgListProc;'#010+
  '  e','nd;'#010+
  #010+
  '  { TPackageEnumerator }'#010+
  #010+
  '  TPackageEnumerator = Class(TCollectionEnumerator)'#010+
  '  public'#010+
  '    function GetCurrent: TPackage;'#010+
  '    property Current: TPackage read GetCurrent;'#010+
  '  end;'#010+
  #010+
  '  { TPackages }'#010+
  #010+
  '  TPackages = Class(TNamedCollection)'#010+
  '  ','private'#010+
  '    function GetPackage(const AName : String): TPackage;'#010+
  '    function GetPackageItem(AIndex : Integer): TPackage;'#010+
  '    procedure SetPackageItem(AIndex : Integer; const AValue: TPackage)'+
  ';'#010+
  '  Public'#010+
  '    Function AddPackage(Const AName :',' String) : TPackage;'#010+
  '    Function GetEnumerator : TPackageEnumerator;'#010+
  '    Property Packages[AName : String] : TPackage Read GetPackage ; Def'+
  'ault;'#010+
  '    Property PackageItems[AIndex : Integer] : TPackage Read GetPackage'+
  'Item Write SetPackageIte','m;'#010+
  '  end;'#010+
  #010+
  '  { TCustomDefaults }'#010+
  #010+
  '  TCustomDefaults = Class(TPersistent)'#010+
  '  Private'#010+
  '    FArchive: String;'#010+
  '    FBuildMode: TBuildMode;'#010+
  '    FBuildTarget: TCompileTarget;'#010+
  '    FCompiler: String;'#010+
  '    FCopy: String;'#010+
  '    FFPDocOptions: String;'#010+
  '    ','FFPDocOutputDir: String;'#010+
  '    FFPUnitSourcePath: String;'#010+
  '    FIgnoreInvalidOptions: Boolean;'#010+
  '    FInstallExamples: Boolean;'#010+
  '    FBuildExamples: Boolean;'#010+
  '    FMkDir: String;'#010+
  '    FMove: String;'#010+
  '    FNamespaces: Boolean;'#010+
  '    FOptions: TStrings;',#010+
  '    FCompileTarget : TCompileTarget;'#010+
  '    FSourceTarget : TCompileTarget;'#010+
  '    FMode : TCompilerMode;'#010+
  '    FCompilerDate : String;'#010+
  '    FCompilerVersion : String;'#010+
  '    FFullCompilerVersion : String;'#010+
  '    FPrefix: String;'#010+
  '    FBaseInstallDir,'#010+
  '   ',' FUnitInstallDir,'#010+
  '    FUnitConfigFilesInstallDir,'#010+
  '    FBinInstallDir,'#010+
  '    FLibInstallDir,'#010+
  '    FDocInstallDir,'#010+
  '    FExamplesInstallDir : String;'#010+
  '    FSingleFPDocFile: Boolean;'#010+
  '    FSearchPath: TStrings;'#010+
  '    FSkipAllPrograms: boolean;'#010+
  '    FSk','ipCrossPrograms: boolean;'#010+
  '    FThreadsAmount: integer;'#010+
  '    FRemoveTree: String;'#010+
  '    FRemoveDir: String;'#010+
  '    FRemove: String;'#010+
  '    FUnixPaths: Boolean;'#010+
  '    FNoFPCCfg: Boolean;'#010+
  '    FUseEnvironment: Boolean;'#010+
  '    FZipPrefix: String;'#010+
  '    FExplici','tOSNone: Boolean;'#010+
  '    FUnitOutputDirectory: String;'#010+
  '    function GetTarget: String;'#010+
  '    function SafeExpandFileName(const AFileName: string): string;'#010+
  '    function GetBuildCPU: TCpu;'#010+
  '    function GetBuildOS: TOS;'#010+
  '    function GetBuildString:',' String;'#010+
  '    function GetFPDocOutputDir: String;'#010+
  '    function GetFPUnitSourcePath: String;'#010+
  '    function GetLocalUnitDir: String;'#010+
  '    function GetGlobalUnitDir: String;'#010+
  '    function GetBaseInstallDir: String;'#010+
  '    function GetBinInstallDir: S','tring;'#010+
  '    function GetLibInstallDir: String;'#010+
  '    function GetCompiler: String;'#010+
  '    function GetDocInstallDir: String;'#010+
  '    function GetExamplesInstallDir: String;'#010+
  '    function GetOptions: TStrings;'#010+
  '    function GetPrefix: String;'#010+
  '    functi','on GetSearchPath: TStrings;'#010+
  '    function GetUnitInstallDir: String;'#010+
  '    function GetUnitConfigFilesInstallDir: String;'#010+
  '    procedure SetCompileTarget(AValue: TCompileTarget);'#010+
  '    procedure SetLocalUnitDir(const AValue: String);'#010+
  '    procedur','e SetGlobalUnitDir(const AValue: String);'#010+
  '    procedure IntSetBaseInstallDir(const AValue: String);'#010+
  '    procedure SetBaseInstallDir(const AValue: String);'#010+
  '    procedure SetCPU(const AValue: TCPU);'#010+
  '    procedure SetOptions(const AValue: TStr','ings);'#010+
  '    procedure SetOS(const AValue: TOS);'#010+
  '    procedure SetPrefix(const AValue: String);'#010+
  '    procedure SetSearchPath(AValue: TStrings);'#010+
  '    procedure SetSubTarget(AValue: String);'#010+
  '    procedure SetTarget(const AValue: String);'#010+
  '    proc','edure SetUnitInstallDir(const AValue: String);'#010+
  '    procedure SetUnitConfigFilesInstallDir(const AValue: String);'#010+
  '    procedure SetZipPrefix(AValue: String);'#010+
  '  Protected'#010+
  '    procedure RecalcTarget;'#010+
  '    Function CmdLineOptions : String;'#010+
  '  Pub','lic'#010+
  '    Constructor Create;'#010+
  '    Destructor Destroy; override;'#010+
  '    Procedure InitDefaults; virtual;'#010+
  '    Function HaveOptions: Boolean;'#010+
  '    Procedure AddOption(const aValue : string);'#010+
  '    function IsBuildDifferentFromTarget: boolean;'#010+
  '    proc','edure CompilerDefaults; virtual;'#010+
  '    Procedure LocalInit(Const AFileName : String);'#010+
  '    Procedure LoadFromFile(Const AFileName : String);'#010+
  '    Procedure SaveToFile(Const AFileName : String);'#010+
  '    procedure SaveToStream(S : TStream);virtual;'#010+
  ' ','   procedure LoadFromStream(S : TStream);virtual;'#010+
  '    // Compile Information'#010+
  #010+
  '    Property CompileTarget : TCompileTarget Read FCompileTarget Write '+
  'SetCompileTarget;'#010+
  '    Property Target : String Read GetTarget Write SetTarget;'#010+
  '    Property ','OS : TOS Read FCompileTarget.OS Write SetOS;'#010+
  '    Property CPU : TCPU Read FCompileTarget.CPU Write SetCPU;'#010+
  '    Property SubTarget : String Read FCompileTarget.SubTarget Write Se'+
  'tSubTarget;'#010+
  '    Property SourceOS : TOS Read FSourceTarget.OS;'#010,
  '    Property SourceCPU : TCPU Read FSourceTarget.CPU;'#010+
  '    Property CompilerVersion : String read FCompilerVersion;'#010+
  '    Property CompilerDate : String read FCompilerDate;'#010+
  '    Property FullCompilerVersion : String read FFullCompilerVersion;'#010+
  ' ','   Property ExplicitOSNone: Boolean read FExplicitOSNone Write FExp'+
  'licitOSNone;'#010+
  '    Property BuildString : String read GetBuildString;'#010+
  '    Property BuildTarget : TCompileTarget Read FBuildTarget;'#010+
  '    Property BuildOS : TOS read GetBuildOS;'#010,
  '    Property BuildCPU : TCpu read GetBuildCPU;'#010+
  '    Property Mode : TCompilerMode Read FMode Write FMode;'#010+
  '    Property UnixPaths : Boolean Read FUnixPaths Write FUnixPaths;'#010+
  '    Property Options : TStrings Read GetOptions Write SetOptions;   ',' '+
  '// Default compiler options.'#010+
  '    Property NoFPCCfg : Boolean Read FNoFPCCfg Write FNoFPCCfg;'#010+
  '    // When ThreadsAmount is specified, #threadsamount# worker-threads'+
  ' are'#010+
  '    // created. When such a worker-thread is ready all worker-threads '+
  'a','re evaluated'#010+
  '    // to see if there are idle threads (there is always at least one '+
  'such thread.)'#010+
  '    // To each idle thread a package is assigned which has to be compi'+
  'led for the'#010+
  '    // current target and for which all dependencies are comp','iled ea'+
  'rlier.'#010+
  '    // When no package is available the thread remains idle until anot'+
  'her thread'#010+
  '    // has finished it'#039's task. Compilation stops when all packages'+
  ' are compiled'#010+
  '    // or when an error occurs.'#010+
  '    //'#010+
  '    // When ThreadsAmount ','is not specified (-1), all packages are co'+
  'mpiled on by one.'#010+
  '    // Dependencies are compiled recursively. When a package is alread'+
  'y compiled'#010+
  '    // (because some other package was depending on it) the package is'+
  ' skipped.'#010+
  '    // When the las','t package in the list is compiled, the compilatio'+
  'n stops.'#010+
  '    Property ThreadsAmount : integer Read FThreadsAmount Write FThread'+
  'sAmount;'#010+
  '    // paths etc.'#010+
  '    Property LocalUnitDir : String Read GetLocalUnitDir Write SetLocal'+
  'UnitDir;'#010+
  '    Pr','operty GlobalUnitDir : String Read GetGlobalUnitDir Write SetG'+
  'lobalUnitDir;'#010+
  '    property UnitOutputDirectory: String read FUnitOutputDirectory wri'+
  'te FUnitOutputDirectory;'#010+
  '    // The SearchPath contains a list of directories in which package',
  's are'#010+
  '    // installed. Packages are searched for in order of this list.'#010+
  '    Property SearchPath: TStrings read GetSearchPath write SetSearchPa'+
  'th;'#010+
  '    Property Prefix : String Read GetPrefix Write SetPrefix;'#010+
  '    Property ZipPrefix : String ','Read FZipPrefix Write SetZipPrefix;'#010+
  '    Property BaseInstallDir : String Read GetBaseInstallDir Write SetB'+
  'aseInstallDir;'#010+
  '    Property UnitInstallDir : String Read GetUnitInstallDir Write SetU'+
  'nitInstallDir;'#010+
  '    Property UnitConfigFilesInstal','lDir : String Read GetUnitConfigFi'+
  'lesInstallDir Write SetUnitConfigFilesInstallDir;'#010+
  '    Property BinInstallDir : String Read GetBinInstallDir Write FBinIn'+
  'stallDir;'#010+
  '    Property LibInstallDir : String Read GetLibInstallDir Write FLibIn'+
  'stallD','ir;'#010+
  '    Property DocInstallDir : String Read GetDocInstallDir Write FDocIn'+
  'stallDir;'#010+
  '    Property ExamplesInstallDir : String Read GetExamplesInstallDir Wr'+
  'ite FExamplesInstallDir;'#010+
  '    Property FPDocOutputDir : String Read GetFPDocOutputDir W','rite FF'+
  'PDocOutputDir;'#010+
  '    Property FPDocOptions : String Read FFPDocOptions Write FFPDocOpti'+
  'ons;'#010+
  '    Property FPUnitSourcePath: String read GetFPUnitSourcePath Write F'+
  'FPUnitSourcePath;'#010+
  #010+
  '    // Command tools. If not set, internal commands  w','ill be used.'#010+
  '    Property Compiler : String Read GetCompiler Write FCompiler; // Co'+
  'mpiler. Defaults to fpc'#010+
  '    Property Copy : String Read FCopy Write FCopy;             // copy'+
  ' $(FILES) to $(DEST)'#010+
  '    Property Move : String Read FMove Writ','e FMove;             // Mo'+
  've $(FILES) to $(DEST)'#010+
  '    Property Remove : String Read FRemove Write FRemove;       // Dele'+
  'te $(FILES)'#010+
  '    Property RemoveDir : String Read FRemoveDir Write FRemoveDir;     '+
  '  // Delete $(FILES)'#010+
  '    Property Remov','eTree : String Read FRemoveTree Write FRemoveTree;'+
  '       // removes $(DIRECTORY)'#010+
  '    Property MkDir : String Read FMkDir write FMkDir;          // Make'+
  ' $(DIRECTORY)'#010+
  '    Property Archive : String Read FArchive Write FArchive;    // zip '+
  '$(ARC','HIVE) $(FILESORDIRS)'#010+
  '    // Misc'#010+
  '    Property UseEnvironment : Boolean read FUseEnvironment write FUseE'+
  'nvironment;'#010+
  '    Property IgnoreInvalidOptions: Boolean read FIgnoreInvalidOptions '+
  'write FIgnoreInvalidOptions;'#010+
  '    Property BuildMode: TB','uildMode read FBuildMode write FBuildMode;'+
  #010+
  '    Property SingleFPDocFile : Boolean Read FSingleFPDocFile Write FSi'+
  'ngleFPDocFile;'#010+
  '    Property Namespaces : Boolean Read FNamespaces Write FNameSpaces;'#010+
  '    // Compile example units and programs,',' into the examples directo'+
  'ry of the units output directory.'#010+
  '    Property BuildExamples: Boolean read FBuildExamples write FBuildEx'+
  'amples;'#010+
  '    // Installation options'#010+
  '    Property InstallExamples: Boolean read FInstallExamples write FIns'+
  'tall','Examples;'#010+
  '    Property SkipCrossPrograms: boolean read FSkipCrossPrograms write '+
  'FSkipCrossPrograms;'#010+
  '    Property SkipAllPrograms: boolean read FSkipAllPrograms write FSki'+
  'pAllPrograms;'#010+
  '  end;'#010+
  #010+
  '  { TBasicDefaults }'#010+
  #010+
  '  TBasicDefaults = Class(T','CustomDefaults)'#010+
  '  end;'#010+
  #010+
  '  { TFPCDefaults }'#010+
  #010+
  '  TFPCDefaults = Class(TCustomDefaults)'#010+
  '  public'#010+
  '    procedure CompilerDefaults; override;'#010+
  '  end;'#010+
  #010+
  '  { TBuildEngine }'#010+
  '  TCopyFileProc = procedure(const APackage: TPackage; Const ASourceFil'+
  'eName, A','DestFileName : String) of object;'#010+
  #010+
  '  TBuildEngine = Class(TComponent)'#010+
  '  private'#010+
  '    // general variables'#010+
  '    FCompiler : String;'#010+
  '    FStartDir : String;'#010+
  '    FForceCompile : Boolean;'#010+
  '    FListMode : Boolean;'#010+
  '    FVerbose : boolean;'#010+
  '    FInte','ractive : boolean;'#010+
  '    FProgressMax : integer;'#010+
  '    FProgressCount : integer;'#010+
  '    FIndentCount : integer;'#010+
  '    FExternalPackages : TPackages;'#010+
  '    // Events'#010+
  '    FOnLog: TLogEvent;'#010+
  '    FNotifyEventCollection: TNotifyEventCollection;'#010+
  '    FOnCopy','File: TCopyFileProc;'#010+
  '    FOnFinishCopy: TNotifyEvent;'#010+
  #010+
  '    FCachedlibcPath: string;'#010+
  '    GCCLibWarningIssued : boolean;'#010+
  '{$ifndef NO_THREADING}'#010+
  '    FGeneralCriticalSection: TRTLCriticalSection;'#010+
  '{$endif NO_THREADING}'#010+
  '{$ifdef HAS_UNIT_ZIPPER}'#010+
  ' ','   FZipper: TZipper;'#010+
  '    FGZFileStream: TGZFileStream;'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  '{$ifdef HAS_TAR_SUPPORT}'#010+
  '    FTarWriter: TTarWriter;'#010+
  '{$endif HAS_TAR_SUPPORT}'#010+
  '    procedure AddFileToArchive(const APackage: TPackage; Const ASource'+
  'FileName, AD','estFileName : String);'#010+
  '    procedure FinishArchive(Sender: TObject);'#010+
  '    // Compile Packages when one of the packages to install is not com'+
  'piled yet.'#010+
  '    procedure CompileBeforeInstall(Packages: TPackages);'#010+
  '  Protected'#010+
  '    Procedure Error(c','onst Msg : String);'#010+
  '    Procedure Error(const Fmt : String; const Args : Array of const);'#010+
  '    // Internal copy/delete/move/archive/mkdir files'#010+
  '    Function  SysDirectoryExists(const ADir:string):Boolean;'#010+
  '    Function  SysFileExists(const AF','ileName:string):Boolean;'#010+
  '    Procedure SysCopyFile(Const Src,Dest : String); virtual;'#010+
  '    Procedure SysMoveFile(Const Src,Dest : String); virtual;'#010+
  '    Procedure SysDeleteFile(Const AFileName : String); virtual;'#010+
  '    Procedure SysDeleteDirect','ory(Const ADirectoryName : String); vir'+
  'tual;'#010+
  '    Procedure SysDeleteTree(Const ADirectoryName : String); virtual;'#010+
  '    Procedure SysArchiveFiles(List : TStrings; Const AFileName : Strin'+
  'g); virtual;'#010+
  '    procedure LogIndent;'#010+
  '    procedure LogU','nIndent;'#010+
  '    Procedure EnterDir(ADir : String);'#010+
  '    Function GetCompiler : String;'#010+
  '    Function InstallPackageFiles(APAckage : TPackage; tt : TTargetType'+
  's; Const Dest : String; Const InstallMode: TInstallMode):Boolean;'#010+
  '    Procedure Install','UnitConfigFile(APAckage : TPackage; Const Dest '+
  ': String);'#010+
  '    // Write an fpcmake Package.fpc for APackage and the current targe'+
  't, and install it in Dest.'#010+
  '    Procedure InstallPackageFpcFile(APackage : TPackage; Const Dest : '+
  'String);'#010+
  '    fu','nction GetUnitConfigFilesInstallDir(ABaseDir: string): String;'+
  #010+
  #010+
  '    Function InstallPackageSourceFiles(APAckage : TPackage; stt : TSou'+
  'rceTypes; ttt : TTargetTypes; Const Dest : String; Const InstallMode: '+
  'TInstallMode):Boolean;'#010+
  '    Function ','FileNewer(const Src,Dest : String) : Boolean;'#010+
  '    Procedure LogSearchPath(APackage: TPackage;const ASearchPathName:s'+
  'tring;Path:TConditionalStrings; ACPU:TCPU;AOS:TOS);'#010+
  '    Function FindFileInPath(APackage: TPackage; Path:TConditionalStrin'+
  'gs','; const AFileName:String; var FoundPath:String;ACPU:TCPU;AOS:TOS):'+
  'Boolean;'#010+
  #010+
  '    procedure GetDirectoriesFromFilelist(const AFileList, ADirectoryLi'+
  'st: TStringList);'#010+
  '    procedure AddPackageMacrosToDictionary(const APackage: TPackage; A'+
  'Dictio','nary: TDictionary);'#010+
  '    //package commands'#010+
  '    function  GetUnitDir(APackage:TPackage):String;'#010+
  '    procedure ResolvePackagePaths(APackage:TPackage);'#010+
  '    procedure AddDependencyPaths(L: TStrings; DependencyType: TDepende'+
  'ncyType; ATarget: TTa','rget);'#010+
  '    procedure AddDependencyUnitPaths(L:TStrings;APackage: TPackage);'#010+
  '    procedure AddDependencyTransmittedOptions(Args: TStrings; APackage'+
  ': TPackage);'#010+
  '    procedure GetDocProject(Proj: TStrings; P: TPackage; aIndent: stri'+
  'ng); virtua','l;'#010+
  #010+
  '  Public'#010+
  '    Constructor Create(AOwner : TComponent); override;'#010+
  '    destructor Destroy;override;'#010+
  '    function AddPathPrefix(APackage: TPackage; APath: string): string;'+
  #010+
  #010+
  '    property Verbose : boolean read FVerbose write FVerbose;'#010+
  '    pr','operty Interactive : boolean read FInteractive write FInteract'+
  'ive;'#010+
  '    Procedure ResolveFileNames(APackage : TPackage; ACPU:TCPU;AOS:TOS;'+
  'DoChangeDir:boolean=true; WarnIfNotFound:boolean=true);'#010+
  '    Procedure ClearResolvedFileNames(APackage :',' TPackage);'#010+
  #010+
  '    // Public Copy/delete/Move/Archive/Mkdir Commands.'#010+
  '    Procedure ExecuteCommand(const Cmd : String; const Args : TStrings'+
  '; Env: TStrings = nil; IgnoreError : Boolean = False); virtual;'#010+
  '    Function GetExecuteCommandOutput(c','onst Cmd : String; const Args '+
  ': TStrings; Env: TStrings = nil; IgnoreError : Boolean = False) : stri'+
  'ng; virtual;'#010+
  '    procedure CmdCopyFiles(List: TStrings; const DestDir: String; APac'+
  'kage: TPackage);'#010+
  '    Procedure CmdCreateDir(const DestDir',' : String);'#010+
  '    Procedure CmdMoveFiles(List : TStrings; Const DestDir : String);'#010+
  '    Procedure CmdDeleteFiles(List : TStrings);'#010+
  '    procedure CmdDeleteDestFiles(List: TStrings; const DestDir: String'+
  ');'#010+
  '    Procedure CmdArchiveFiles(List : TS','trings; Const ArchiveFile : S'+
  'tring);'#010+
  '    Procedure CmdRenameFile(const SourceName, DestName : String);'#010+
  '    Procedure CmdRemoveDirs(List: TStrings);'#010+
  '    Procedure CmdRemoveTrees(List: TStrings);'#010+
  '    Procedure ExecuteCommands(Commands : TComm','ands; At : TCommandAt;'+
  ' APackage: TPackage = nil);'#010+
  '    // Dependency commands'#010+
  '    Function  DependencyOK(ADependency : TDependency) : Boolean;'#010+
  '    // Target commands'#010+
  '    Function  GetCompilerCommand(APackage : TPackage; ATarget : TTarge'+
  't; En','v: TStrings) : String; deprecated '#039'use TStrings version'#039+
  ';'#010+
  '    Function  TargetOK(ATarget : TTarget; ACPU: TCPU; AOS: TOS; const '+
  'aSubTarget : String) : Boolean; deprecated '#039'use TCompileTarget ver'+
  'sion'#039';'#010+
  '    Function  TargetInstallOK(ATarget :',' TTarget; ACPU:TCPU; AOS : TO'+
  'S; const aSubTarget : String) : Boolean; deprecated '#039'use TCompileT'+
  'arget version'#039';'#010+
  '    Procedure GetCompilerCommand(Args: TStrings; APackage : TPackage; '+
  'ATarget : TTarget; Env: TStrings);'#010+
  '    Function  TargetOK(A','Target : TTarget; const aCompileTarget : TCo'+
  'mpileTarget) : Boolean;'#010+
  '    Function  TargetInstallOK(ATarget : TTarget; const aCompileTarget '+
  ': TCompileTarget) : Boolean;'#010+
  '    Function  NeedsCompile(APackage:TPackage; ATarget : TTarget) : Boo'+
  'lea','n;'#010+
  '    Procedure Compile(APackage:TPackage; ATarget : TTarget);  virtual;'+
  #010+
  '    Procedure MaybeCompile(APackage:TPackage; ATarget: TTarget);'#010+
  '    Procedure CompileDependencies(APackage:TPackage; ATarget: TTarget)'+
  ';'#010+
  '    // Package commands'#010+
  '{    ','Function  GetPackageDir(APackage : TPackage; AbsolutePath : Boo'+
  'lean = False) : String;'#010+
  '    Function  GetUnitsOutputDir(APackage : TPackage; AbsolutePath : Bo'+
  'olean = False) : String;'#010+
  '    Function  GetBinOutputDir(APackage : TPackage; Absolut','ePath : Bo'+
  'olean = False) : String;  }'#010+
  '    Function  PackageOK(APackage : TPackage) : Boolean; virtual;'#010+
  '    Procedure DoBeforeCompile(APackage : TPackage);virtual;'#010+
  '    Procedure DoAfterCompile(APackage : TPackage);virtual;'#010+
  '    Procedure DoBe','foreInstall(APackage : TPackage);virtual;'#010+
  '    Procedure DoAfterInstall(APackage : TPackage);virtual;'#010+
  '    Procedure DoBeforeArchive(APackage : TPackage);virtual;'#010+
  '    Procedure DoAfterArchive(APackage : TPackage);virtual;'#010+
  '    Procedure DoBefo','reClean(APackage : TPackage);virtual;'#010+
  '    Procedure DoAfterClean(APackage : TPackage);virtual;'#010+
  '    Function  NeedsCompile(APackage : TPackage) : Boolean; virtual;'#010+
  '    Procedure Compile(APackage : TPackage);'#010+
  '    Procedure MaybeCompile(APacka','ge:TPackage);'#010+
  '    Function ReadyToCompile(APackage:TPackage) : Boolean;'#010+
  '    Procedure Install(APackage : TPackage; AnArchiveFiles: boolean);'#010+
  '    Procedure UnInstall(APackage : TPackage);'#010+
  '    Procedure Archive(APackage : TPackage);'#010+
  '    Proce','dure PkgList(PkgList: TStrings; APackage : TPackage);'#010+
  '    Procedure Clean(APackage : TPackage; AllTargets: boolean);'#010+
  '    procedure Clean(APackage: TPackage; const aTarget: TCompileTarget)'+
  ';'#010+
  '    // Remove the files and directories written by ','Documentation for'+
  ' APackage'#010+
  '    procedure CleanDocumentation(APackage: TPackage);'#010+
  '    // Name of the file or directory fpdoc writes for APackage in form'+
  'at AFormat'#010+
  '    Function GetFPDocOutputName(APackage : TPackage; AFormat : TFPDocF'+
  'ormat) :',' String;'#010+
  '    // Run fpdoc for the fpdoc targets of APackage, once per format in'+
  ' APackage.FPDocFormat'#010+
  '    Procedure Documentation(APackage : TPackage);'#010+
  '    Procedure CompileDependencies(APackage : TPackage);'#010+
  '    function CheckDependencies(AP','ackage : TPackage; ErrorOnFailure: '+
  'boolean): TCheckDependencyResult;'#010+
  '    Function  CheckExternalPackage(Const APackageName, ForPackageName '+
  ': String; ErrorOnFailure: boolean):TPackage;'#010+
  '    procedure CreateOutputDir(APackage: TPackage);'#010+
  '    /','/ Packages commands'#010+
  '    Procedure Compile(Packages : TPackages);'#010+
  '    Procedure Install(Packages : TPackages);'#010+
  '    Procedure UnInstall(Packages : TPackages);'#010+
  '    Procedure ZipInstall(Packages : TPackages);'#010+
  '    Procedure Archive(Packages : TP','ackages);'#010+
  '    procedure Manifest(Packages: TPackages; Package: TPackage);'#010+
  '    procedure PkgList(Packages: TPackages);'#010+
  '    procedure FPDocProject(Packages: TPackages; SingleDocFile : Boolea'+
  'n);'#010+
  '    // Run fpdoc for all packages'#010+
  '    Procedure ','Documentation(Packages : TPackages);'#010+
  '    Procedure Clean(Packages : TPackages; AllTargets: boolean);'#010+
  #010+
  '    Procedure Log(Level : TVerboseLevel; Msg : String);'#010+
  '    Procedure Log(Level : TVerboseLevel; Fmt : String; const Args : Ar'+
  'ray Of Const',');'#010+
  #010+
  '    Property ListMode : Boolean Read FListMode Write FListMode;'#010+
  '    Property ForceCompile : Boolean Read FForceCompile Write FForceCom'+
  'pile;'#010+
  '    Property ExternalPackages: TPackages Read FExternalPackages;'#010+
  '    Property StartDir: String R','ead FStartDir;'#010+
  #010+
  '    // Events'#010+
  '    Property NotifyEventCollection: TNotifyEventCollection read FNotif'+
  'yEventCollection;'#010+
  '    Property OnLog : TLogEvent Read FOnLog Write FOnlog;'#010+
  '  end;'#010+
  #010+
  '  { TCustomInstaller }'#010+
  #010+
  '  TCustomInstaller = Class(TCompo','nent)'#010+
  '  private'#010+
  '    FBuildEngine: TBuildEngine;'#010+
  '    FPackages: TPackages;'#010+
  '    FRunMode: TRunMode;'#010+
  '    FListMode : Boolean;'#010+
  '    FInteractive : boolean;'#010+
  '    FLogLevels : TVerboseLevels;'#010+
  '    FFPMakeOptionsString: string;'#010+
  '    FPackageVariantSet','tings: TStrings;'#010+
  '    FPackageVariants: TFPList;'#010+
  '    FNotifyEventCollection: TNotifyEventCollection;'#010+
  '  Protected'#010+
  '    Procedure Log(Level : TVerboseLevel; Const Msg : String);'#010+
  '    Procedure CreatePackages; virtual;'#010+
  '    Procedure FreePackages;',' virtual;'#010+
  '    function GetPackages: TPackages; virtual;'#010+
  '    Procedure CheckPackages; virtual;'#010+
  '    // needs build engine !'#010+
  '    procedure CheckNameSpaces; virtual;'#010+
  '    Procedure CreateBuildEngine; virtual;'#010+
  '    Procedure Error(const Msg : Stri','ng);'#010+
  '    Procedure Error(const Fmt : String; const Args : Array of const);'#010+
  '    Procedure AnalyzeOptions;'#010+
  '    Procedure Usage(const FMT : String; const Args : Array of const);'#010+
  '    Procedure Compile(Force : Boolean); virtual;'#010+
  '    Procedure Cl','ean(AllTargets: boolean); virtual;'#010+
  '    Procedure Install(ForceBuild : Boolean); virtual;'#010+
  '    Procedure UnInstall; virtual;'#010+
  '    Procedure ZipInstall; virtual;'#010+
  '    Procedure Archive; virtual;'#010+
  '    Procedure Manifest; virtual;'#010+
  '    Procedure Pkg','List; virtual;'#010+
  '    Procedure FPDocProject; virtual;'#010+
  '    Procedure Documentation; virtual;'#010+
  '    Procedure Info; virtual;'#010+
  '    procedure AddAutoPackageVariantsToPackage(APackage: TPackage); vir'+
  'tual;'#010+
  '  Public'#010+
  '    Constructor Create(AOwner : TCom','ponent); override;'#010+
  '    Destructor destroy; override;'#010+
  '    Function AddPackage(Const AName : String) : TPackage;'#010+
  '    Function AddPackageVariant(AName: string; AIsInheritable: boolean;'+
  ' AutoAddToPackage: Boolean = false): TPackageVariants;'#010+
  '    ','Function AddPackageFromDir(Const AName : String; const aDirector'+
  'y : String; const aMask : string = '#039#039') : TPackage;'#010+
  '    Function Run : Boolean;'#010+
  '    Property FPMakeOptionsString: string read FFPMakeOptionsString;'#010+
  '    Property BuildEngine : TB','uildEngine Read FBuildEngine;'#010+
  '    //files in package'#010+
  '    Property Packages : TPackages Read GetPackages;'#010+
  '    Property RunMode : TRunMode Read FRunMode;'#010+
  '    Property ListMode : Boolean Read FListMode;'#010+
  '    Property NotifyEventCollection : TNo','tifyEventCollection read FNo'+
  'tifyEventCollection;'#010+
  '  end;'#010+
  #010+
  '  { TFPCInstaller }'#010+
  '  TFPCInstaller = class(TCustomInstaller)'#010+
  '  public'#010+
  '    Constructor Create(AOwner : TComponent); override;'#010+
  '  end;'#010+
  #010+
  '  { TBasicInstaller }'#010+
  '  TBasicInstaller = class(T','CustomInstaller)'#010+
  '    Constructor Create(AOwner : TComponent); override;'#010+
  '  end;'#010+
  #010+
  '  { TValueItem }'#010+
  #010+
  '  TValueItem = Class(TObject)'#010+
  '  Private'#010+
  '    FValue : String;'#010+
  '  Public'#010+
  '    Constructor Create(AValue : String);'#010+
  '    Property Value : String Rea','d FValue;'#010+
  '  end;'#010+
  #010+
  '  { TFunctionItem }'#010+
  #010+
  '  TFunctionItem = Class(TObject)'#010+
  '  Private'#010+
  '    FFunc : TReplaceFunction;'#010+
  '  public'#010+
  '    Constructor Create(AFunc : TReplaceFunction);'#010+
  '    Property Func : TReplaceFunction Read FFunc;'#010+
  '  end;'#010+
  #010+
  '{$ifndef NO_','THREADING}'#010+
  #010+
  '  { TCompileWorkerThread }'#010+
  #010+
  '  TCompileWorkerThread = class(TThread)'#010+
  '  private'#010+
  '    FBuildEngine: TBuildEngine;'#010+
  '    FCompilationOK: boolean;'#010+
  '    FDone: boolean;'#010+
  '    FErrorMessage: string;'#010+
  '    FWorkerPrefix: string;'#010+
  '    FNotifyMain','ThreadEvent: PRTLEvent;'#010+
  '    FNotifyStartTask: PRTLEvent;'#010+
  '    FPackage: TPackage;'#010+
  '  protected'#010+
  '    procedure execute; override;'#010+
  '    property Done: boolean read FDone;'#010+
  '    property APackage: TPackage read FPackage write FPackage;'#010+
  '    property ','CompilationOK: boolean read FCompilationOK;'#010+
  '    property NotifyStartTask: PRTLEvent read FNotifyStartTask;'#010+
  '    property ErrorMessage: string read FErrorMessage;'#010+
  '  public'#010+
  '    constructor Create(ABuildEngine: TBuildEngine; NotifyMainThreadEve',
  'nt: PRTLEvent); virtual;'#010+
  '    destructor Destroy; override;'#010+
  '  end;'#010+
  #010+
  '{$endif NO_THREADING}'#010+
  #010+
  '  { TfpmPlugin }'#010+
  #010+
  '  TfpmPlugin = class'#010+
  '  protected'#010+
  '    function GetName: string; virtual;'#010+
  '  public'#010+
  '    property Name: string read GetName;'#010+
  #010+
  '    proced','ure BeforeResolvePackagePath(ABuildEngine: TBuildEngine; A'+
  'Package: TPackage; out AContinue: Boolean); virtual;'#010+
  '    procedure ResolvePackagePath(ABuildEngine: TBuildEngine; APackage:'+
  ' TPackage; SearchDirectory: string; out AContinue: Boolean)','; virtual'+
  ';'#010+
  '    procedure AfterResolvePackagePath(ABuildEngine: TBuildEngine; APac'+
  'kage: TPackage; out AContinue: Boolean); virtual;'#010+
  '  end;'#010+
  '  TfpmPluginClass = class of TfpmPlugin;'#010+
  #010+
  '  { TfpmPluginManager }'#010+
  #010+
  '  TfpmPluginManager = class(TfpmPlu','gin)'#010+
  '  private'#010+
  '    FPlugins: array of TfpmPlugin;'#010+
  '  public'#010+
  '    destructor Destroy; override;'#010+
  '    procedure RegisterPlugin(APlugin: TfpmPluginClass);'#010+
  #010+
  '    procedure BeforeResolvePackagePath(ABuildEngine: TBuildEngine; APa'+
  'ckage: TPackage; out',' AContinue: Boolean); override;'#010+
  '    procedure ResolvePackagePath(ABuildEngine: TBuildEngine; APackage:'+
  ' TPackage; SearchPath: string; out AContinue: Boolean); override;'#010+
  '    procedure AfterResolvePackagePath(ABuildEngine: TBuildEngine; APac'+
  'ka','ge: TPackage; out AContinue: Boolean); override;'#010+
  '  end;'#010+
  #010+
  '  { TfpmResolvePackagePathsPlugin }'#010+
  #010+
  '  TfpmResolvePackagePathsPlugin = class(TfpmPlugin)'#010+
  '  private'#010+
  '    procedure ResolveUnitConfigFilenameForBasePath(ABuildEngine: TBuil'+
  'dEngine; APack','age: TPackage; ABasePath: string;'#010+
  '      out AContinue: Boolean);'#010+
  '  public'#010+
  '    procedure BeforeResolvePackagePath(ABuildEngine: TBuildEngine; APa'+
  'ckage: TPackage; out AContinue: Boolean); override;'#010+
  '    procedure ResolvePackagePath(ABuildEngin','e: TBuildEngine; APackag'+
  'e: TPackage; SearchPath: string;'#010+
  '      out AContinue: Boolean); override;'#010+
  '  end;'#010+
  #010+
  '  ECollectionError = Class(Exception);'#010+
  '  EDictionaryError = Class(Exception);'#010+
  '  EInstallerError = Class(Exception);'#010+
  #010+
  '  TInstallerClass',' = Class of TCustomInstaller;'#010+
  '  TDictionaryClass = Class of TDictionary;'#010+
  '  TPackageDictionaryClass = Class of TPackageDictionary;'#010+
  #010+
  'Type'#010+
  '  TArchiveEvent = Procedure (Const AFileName : String; List : TStrings'+
  ') of Object;'#010+
  '  TArchiveProc = Proc','edure (Const AFileName : String; List : TString'+
  's);'#010+
  #010+
  'Var'#010+
  '  DictionaryClass : TDictionaryClass = TDictionary;'#010+
  '  PackageDictionaryClass : TPackageDictionaryClass = TPackageDictionar'+
  'y;'#010+
  '  OnArchiveFiles : TArchiveEvent = Nil;'#010+
  '  ArchiveFilesProc ',': TArchiveProc = Nil;'#010+
  #010+
  '  Defaults : TCustomDefaults; // Set by installer.'#010+
  '  GlobalDictionary : TDictionary;'#010+
  #010+
  #010+
  'Function CurrentOS : String;'#010+
  'Function CurrentCPU : String;'#010+
  #010+
  'Function Installer(InstallerClass: TInstallerClass) : TCustomInstaller',
  '; overload;'#010+
  'Function Installer : TCustomInstaller; overload;'#010+
  #010+
  'Function OSToString(OS: TOS) : String;'#010+
  'Function OSesToString(OSes: TOSes) : String;'#010+
  'Function CPUToString(CPU: TCPU) : String;'#010+
  'Function CPUSToString(CPUS: TCPUS) : String;'#010+
  'Functio','n StringToOS(const S : String) : TOS;'#010+
  'function IsDifferentFromBuild(ACpu: TCPU; AOs: TOs): boolean;'#010+
  '//Function StringToOSes(const S : String) : TOSes;'#010+
  'Function StringToCPU(const S : String) : TCPU;'#010+
  'Function StringToCPUS(const S : String) : ','TCPUS;'#010+
  'Function ModeToString(Mode: TCompilerMode) : String;'#010+
  'Function StringToMode(const S : String) : TCompilerMode;'#010+
  'Function MakeTargetString(const aTarget : TCompileTarget; aLimit83 : B'+
  'oolean) : String;'#010+
  'Function MakeTargetString(const aTa','rget : TCompileTarget) : String;'#010+
  'Function MakeTargetString(CPU : TCPU;OS: TOS; aSubTarget : String = '#039+
  #039') : String;'#010+
  'Procedure StringToCPUOS(const S : String; Var CPU : TCPU; Var OS: TOS)'+
  ';'#010+
  'Function FixPath (const APath : String) : String; inl','ine; deprecated'+
  ' '#039'Use the overload with AIsDir instead'#039';'#010+
  'Function FixPath (const APath : String; AIsDir : Boolean) : String;'#010+
  'Function IsRelativePath(const APath : String) : boolean;'#010+
  'Procedure ChangeDir(const APath : String);'#010+
  'Procedure SplitC','ommand(Const Cmd : String; out Exe,Options : String)'+
  ';'#010+
  'Procedure AddCustomFpmakeCommandlineOption(const ACommandLineOption, H'+
  'elpMessage : string);'#010+
  'Function GetCustomFpmakeCommandlineOptionValue(const ACommandLineOptio'+
  'n : string) : string;'#010+
  'Fu','nction AddProgramExtension(const ExecutableName: string; AOS : TOS'+
  ') : string;'#010+
  'Function AddLibraryExtension(const LibraryName: string; AOS : TOS) : s'+
  'tring;'#010+
  'Function GetImportLibraryFilename(const UnitName: string; AOS : TOS) :'+
  ' string;'#010+
  #010+
  'proce','dure SearchFiles(AFileName, ASearchPathPrefix: string; Recursiv'+
  'e: boolean; var List: TStrings);'#010+
  'function GetDefaultLibGCCDir(CPU : TCPU;OS: TOS; out ErrorMessage: str'+
  'ing): string;'#010+
  #010+
  'function GetPluginManager: TfpmPluginManager;'#010+
  #010+
  'Implementati','on'#010+
  #010+
  '{$IFDEF FPC_DOTTEDUNITS}'#010+
  'uses System.TypInfo, System.RtlConsts;'#010+
  '{$ELSE FPC_DOTTEDUNITS}'#010+
  'uses typinfo, rtlconsts;'#010+
  '{$ENDIF FPC_DOTTEDUNITS}'#010+
  #010+
  'const'#010+
  '{$ifdef CREATE_TAR_FILE}'#010+
  '  {$ifdef HAS_UNIT_ZIPPER}'#010+
  '  ArchiveExtension = '#039'.tar.gz'#039';'#010+
  '  {$els','e }'#010+
  '  ArchiveExtension = '#039'.tar'#039';'#010+
  '  {$endif HAS_UNIT_ZIPPER}'#010+
  '{$else CREATE_TAR_FILE}'#010+
  '  ArchiveExtension = '#039'.zip'#039';'#010+
  '{$endif CREATE_TAR_FILE}'#010+
  #010+
  'var'#010+
  '  GPluginManager: TfpmPluginManager;'#010+
  #010+
  #010+
  '{----------------- from strutils ---------------------}'#010+
  #010+
  'f','unction FindPart(const HelpWilds, inputStr: string): Integer;'#010+
  'var'#010+
  '  i, J: Integer;'#010+
  '  Diff: Integer;'#010+
  'begin'#010+
  '  Result:=0;'#010+
  '  i:=Pos('#039'?'#039',HelpWilds);'#010+
  '  if (i=0) then'#010+
  '    Result:=Pos(HelpWilds, inputStr)'#010+
  '  else'#010+
  '    begin'#010+
  '    Diff:=Length(inputStr)',' - Length(HelpWilds);'#010+
  '    for i:=0 to Diff do'#010+
  '      begin'#010+
  '      for J:=1 to Length(HelpWilds) do'#010+
  '        if (inputStr[i + J] = HelpWilds[J]) or (HelpWilds[J] = '#039'?'#039+
  ') then'#010+
  '          begin'#010+
  '          if (J=Length(HelpWilds)) then'#010+
  '            be','gin'#010+
  '            Result:=i+1;'#010+
  '            Exit;'#010+
  '            end;'#010+
  '          end'#010+
  '        else'#010+
  '          Break;'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function isWild(inputStr, Wilds: string; ignoreCase: Boolean): Boolean'+
  ';'#010+
  #010+
  ' function SearchNext(var Wilds: s','tring): Integer;'#010+
  #010+
  ' begin'#010+
  '   Result:=Pos('#039'*'#039', Wilds);'#010+
  '   if Result>0 then'#010+
  '     Wilds:=Copy(Wilds,1,Result - 1);'#010+
  ' end;'#010+
  #010+
  'var'#010+
  '  CWild, CinputWord: Integer; { counter for positions }'#010+
  '  i, LenHelpWilds: Integer;'#010+
  '  MaxinputWord, MaxWilds: Integer;',' { Length of inputStr and Wilds }'#010+
  '  HelpWilds: string;'#010+
  'begin'#010+
  '  if Wilds = inputStr then begin'#010+
  '    Result:=True;'#010+
  '    Exit;'#010+
  '  end;'#010+
  '  repeat { delete '#039'**'#039', because '#039'**'#039' = '#039'*'#039' }'#010+
  '    i:=Pos('#039'**'#039', Wilds);'#010+
  '    if i > 0 then'#010+
  '      Wilds:=Copy(Wilds',', 1, i - 1) + '#039'*'#039' + Copy(Wilds, i + 2'+
  ', Maxint);'#010+
  '  until i = 0;'#010+
  '  if Wilds = '#039'*'#039' then begin { for fast end, if Wilds only '#039+
  '*'#039' }'#010+
  '    Result:=True;'#010+
  '    Exit;'#010+
  '  end;'#010+
  '  MaxinputWord:=Length(inputStr);'#010+
  '  MaxWilds:=Length(Wilds);'#010+
  '  if ignoreCase t','hen begin { upcase all letters }'#010+
  '    inputStr:=AnsiUpperCase(inputStr);'#010+
  '    Wilds:=AnsiUpperCase(Wilds);'#010+
  '  end;'#010+
  '  if (MaxWilds = 0) or (MaxinputWord = 0) then begin'#010+
  '    Result:=False;'#010+
  '    Exit;'#010+
  '  end;'#010+
  '  CinputWord:=1;'#010+
  '  CWild:=1;'#010+
  '  Result:=','True;'#010+
  '  repeat'#010+
  '    if inputStr[CinputWord] = Wilds[CWild] then begin { equal letters '+
  '}'#010+
  '      { goto next letter }'#010+
  '      inc(CWild);'#010+
  '      inc(CinputWord);'#010+
  '      Continue;'#010+
  '    end;'#010+
  '    if Wilds[CWild] = '#039'?'#039' then begin { equal to '#039'?'#039' }'#010+
  '      ','{ goto next letter }'#010+
  '      inc(CWild);'#010+
  '      inc(CinputWord);'#010+
  '      Continue;'#010+
  '    end;'#010+
  '    if Wilds[CWild] = '#039'*'#039' then begin { handling of '#039'*'#039' '+
  '}'#010+
  '      HelpWilds:=Copy(Wilds, CWild + 1, MaxWilds);'#010+
  '      i:=SearchNext(HelpWilds);'#010+
  '      LenHelp','Wilds:=Length(HelpWilds);'#010+
  '      if i = 0 then begin'#010+
  '        { no '#039'*'#039' in the rest, compare the ends }'#010+
  '        if HelpWilds = '#039#039' then Exit; { '#039'*'#039' is the last l'+
  'etter }'#010+
  '        { check the rest for equal Length and no '#039'?'#039' }'#010+
  '        for i:=0 to ','LenHelpWilds - 1 do begin'#010+
  '          if (HelpWilds[LenHelpWilds - i] <> inputStr[MaxinputWord - i'+
  ']) and'#010+
  '            (HelpWilds[LenHelpWilds - i]<> '#039'?'#039') then'#010+
  '          begin'#010+
  '            Result:=False;'#010+
  '            Exit;'#010+
  '          end;'#010+
  '        ','end;'#010+
  '        Exit;'#010+
  '      end;'#010+
  '      { handle all to the next '#039'*'#039' }'#010+
  '      inc(CWild, 1 + LenHelpWilds);'#010+
  '      i:=FindPart(HelpWilds, Copy(inputStr, CinputWord, Maxint));'#010+
  '      if i= 0 then begin'#010+
  '        Result:=False;'#010+
  '        Exit;'#010+
  '      end',';'#010+
  '      CinputWord:=i + LenHelpWilds;'#010+
  '      Continue;'#010+
  '    end;'#010+
  '    Result:=False;'#010+
  '    Exit;'#010+
  '  until (CinputWord > MaxinputWord) or (CWild > MaxWilds);'#010+
  '  { no completed evaluation }'#010+
  '  if CinputWord <= MaxinputWord then Result:=False;'#010+
  '  if (C','Wild <= MaxWilds) and (Wilds[MaxWilds] <> '#039'*'#039') then R'+
  'esult:=False;'#010+
  'end;'#010+
  #010+
  #010+
  'Function IndexText(S: String; aList : Array of string) : Integer;'#010+
  #010+
  'begin'#010+
  '  Result:=Length(aList)-1;'#010+
  '  While (Result>=0) and not SameText(aList[Result],S) do'#010+
  '    dec(R','esult);'#010+
  'end;'#010+
  #010+
  #010+
  'type'#010+
  '  TUnsortedDuplicatesStringList = class(TStringList)'#010+
  '  public'#010+
  '    function Add(const S: string): Integer; override;'#010+
  '  end;'#010+
  #010+
  '  TUnsortedCompilerOptionsStringList = class(TStringList)'#010+
  '  public'#010+
  '    constructor Create;'#010+
  '  end',';'#010+
  #010+
  'var'#010+
  '  CustomFpmakeCommandlineOptions: TStrings;'#010+
  '  CustomFpMakeCommandlineValues: TStrings;'#010+
  #010+
  '{$ifdef NO_THREADING}'#010+
  'var'#010+
  '{$else NO_THREADING}'#010+
  'threadvar'#010+
  '{$endif NO_THREADING}'#010+
  '  GPathPrefix : string;'#010+
  '  GLogPrefix  : string;'#010+
  #010+
  'ResourceString'#010+
  '  ','SErrInvalidCPU        = '#039'Invalid CPU name "%s"'#039';'#010+
  '  SErrInvalidOS         = '#039'Invalid OS name "%s"'#039';'#010+
  '  SErrInvalidMode       = '#039'Invalid compiler mode "%s"'#039';'#010+
  '  SErrInvalidTarget     = '#039'Invalid compiler target "%s"'#039';'#010+
  '  SErrNameExists        = '#039,'Name "%s" already exists in the collec'+
  'tion.'#039';'#010+
  '  SErrNoSuchName        = '#039'Could not find item with name "%s" in t'+
  'he collection.'#039';'#010+
  '  SErrInValidArgument   = '#039'Invalid command-line argument at positi'+
  'on %d: %s'#039';'#010+
  '  SErrNeedArgument      = '#039'Option',' at position %d (%s) needs an a'+
  'rgument'#039';'#010+
  '  SErrNoPackagesDefined = '#039'No action possible: No packages were de'+
  'fined.'#039';'#010+
  '  SErrInstaller         = '#039'The installer encountered the following'+
  ' error:'#039';'#010+
  '  SErrDepUnknownTarget  = '#039'Unknown target for un','it "%s" in depen'+
  'dencies for %s in package %s'#039';'#010+
  '  SErrExternalCommandFailed = '#039'External command "%s" failed with e'+
  'xit code %d. Console output:'#039'+LineEnding+'#039'%s'#039';'#010+
  '  SErrExtCommandNotFound= '#039'External command "%s" not found'#039';'#010+
  '  SErrCreatingDirec','tory = '#039'Failed to create directory "%s"'#039';'+
  #010+
  '  SErrDeletingFile      = '#039'Failed to delete file "%s"'#039';'#010+
  '  SErrRemovingDirectory = '#039'Failed to remove directory "%s"'#039';'#010+
  '  SErrMovingFile        = '#039'Failed to move file "%s" to "%s"'#039';'#010+
  '  SErrCopyingFile  ','     = '#039'Failed to copy file "%s" to "%s"'#039';'+
  #010+
  '  SErrChangeDirFailed   = '#039'Failed to enter directory "%s"'#039';'#010+
  '  SErrNoArchiveSupport  = '#039'This binary contains no archive support'+
  '. Please recompile with archive support'#039';'#010+
  '  SErrNoDictionaryItem  = '#039'No',' item called "%s" in the dictionary'+
  #039';'#010+
  '  {$ifdef HAS_UNIT_PROCESS}'#010+
  '  SErrInvalidFPCInfo    = '#039'Compiler returns invalid information, c'+
  'heck if fpc -iV works'#039';'#010+
  '  {$ENDIF}'#010+
  '  SErrDependencyNotFound = '#039'Could not find unit directory for depe'+
  'ndency ','package "%s" required for package "%s"'#039';'#010+
  '  SErrAlreadyInitialized = '#039'Installer can only be initialized once'+
  #039';'#010+
  '  SErrInvalidState      = '#039'Invalid state for target %s'#039';'#010+
  '  SErrCouldNotCompile   = '#039'Could not compile target %s from packag'+
  'e %s'#039';'#010,
  '  SErrPackageNotCompiled = '#039'Package %s is not compiled, cannot ins'+
  'tall it'#039';'#010+
  '  SErrPackagesNotReady  = '#039'Cannot compile packages with unresolved'+
  ' dependencies: %s'#039';'#010+
  '  SErrUnsupportedBuildmode = '#039'Package does not support this buildm'+
  'ode'#039';'#010+
  '  SErr','PackVarNotExist   = '#039'There is no package variant with the '+
  'name "%s"'#039';'#010+
  '  SErrEventNotSupported = '#039'Unsupported event type'#039';'#010+
  '  SErrorPkgNotInstalled = '#039'Package "%s" is not installed, can not '+
  'uninstall.'#039';'#010+
  '  SErrBuildUnitCompilation = '#039'Compilati','on of "%s" failed'#039';'#010+
  '  SErrExpectPkgVariant  = '#039'The Package-variant on the command-line'+
  ' position %d (%s) should have at least one item'#039';'#010+
  #010+
  '  SWarnCircularTargetDependency = '#039'Warning: Circular dependency de'+
  'tected when compiling target %s with ','target %s'#039';'#010+
  '  SWarnCircularPackageDependency = '#039'Warning: Circular dependency d'+
  'etected when compiling package %s with package %s'#039';'#010+
  '  SWarnFailedToSetTime    = '#039'Warning: Failed to set timestamp on f'+
  'ile "%s"'#039';'#010+
  '  SWarnFailedToGetTime    = '#039'Warn','ing: Failed to get timestamp fr'+
  'om file "%s"'#039';'#010+
  '  SWarnSourceFileNotFound  = '#039'Warning: Source file "%s" from packa'+
  'ge %s not found for %s'#039';'#010+
  '  SWarnIncludeFileNotFound = '#039'Warning: Include file "%s" from pack'+
  'age %s not found for %s'#039';'#010+
  '  SWarnDepU','nitNotFound     = '#039'Warning: Dependency on unit %s is '+
  'not supported for %s'#039';'#010+
  '  SWarnTargetDependsOnPackage = '#039'Warning: Target %s of package %s '+
  'depends on another package (%s). These kind of dependencies are not pr'+
  'ocessed'#039';'#010+
  '  SWarnDependOnOth','erPlatformPackage = '#039'Warning: Package %s depen'+
  'ds on package %s which is not available for the %s platform'#039';'#010+
  '  SWarnStartCompilingPackage = '#039'Start compiling package %s for tar'+
  'get %s.'#039';'#010+
  '  SWarnCompilingPackagecompleteProgress = '#039'[%3.0f%%] Com','piled pa'+
  'ckage %s'#039';'#010+
  '  SWarnCompilingPackagecomplete = '#039'Compiled package %s'#039';'#010+
  '  SWarnInstallationPackagecomplete = '#039'Installation package %s for '+
  'target %s succeeded'#039';'#010+
  '  SWarnCanNotGetAccessRights = '#039'Warning: Failed to copy access-rig'+
  'hts from f','ile %s'#039';'#010+
  '  SWarnCanNotSetAccessRights = '#039'Warning: Failed to copy access-rig'+
  'hts to file %s'#039';'#010+
  '  SWarnCanNotGetFileAge = '#039'Warning: Failed to get FileAge for %s'#039+
  ';'#010+
  '  SWarnExtCommandNotFound = '#039'Warning: External command "%s" not fo'+
  'und but "%s" is',' older then "%s"'#039';'#010+
  '  SWarnDuplicatePackage = '#039'Warning: Package %s is already added. U'+
  'sing the existing package'#039';'#010+
  '  SWarngccNotFound        = '#039'Could not find gcc'#039';'#010+
  '  SWarncrossgccNotFound   = '#039'Could not find gcc for cross-configur'+
  'ation'#039';'#010+
  '  S','Warngcclibpath         = '#039'Warning: Unable to determine the li'+
  'bgcc path.'#039';'#010+
  '{$IFNDEF HAS_UNIT_PROCESS}'#010+
  '  SWarnNoFCLProcessSupport= '#039'No FCL-Process support'#039';'#010+
  '{$ENDIF}'#010+
  '  SWarnRetryRemDirectory     = '#039'Failed to remove directory "%s". R'+
  'etry after',' a short delay'#039';'#010+
  '  SWarnRetryDeleteFile       = '#039'Failed to remove file "%s". Retry '+
  'after a short delay'#039';'#010+
  '  SWarnCombinedPathAndUDir= '#039'Warning: Better do not combine the Se'+
  'archPath and Global/Local-UnitDir parameters'#039';'#010+
  '  SWarnRemovedNonEmpty','Directory = '#039'Warning: Removed non empty di'+
  'rectory "%s"'#039';'#010+
  #010+
  '  SInfoPackageAlreadyProcessed = '#039'Package %s is already processed'#039+
  ';'#010+
  '  SInfoSkipPackageTargetProgress = '#039'[%3.0f%%] Skipped package %s w'+
  'hich has been disabled for target %s'#039';'#010+
  '  SInfoSk','ipPackageTarget = '#039'Skipped package %s which has been di'+
  'sabled for target %s'#039';'#010+
  '  SInfoCompilingTarget    = '#039'Compiling target %s'#039';'#010+
  '  SInfoExecutingCommand   = '#039'Executing command "%s %s"'#039';'#010+
  '  SInfoCreatingOutputDir  = '#039'Creating output dir "%s"'#039,';'#010+
  '  SInfoInstallingPackage  = '#039'Installing package %s'#039';'#010+
  '  SInfoUnInstallingPackage= '#039'Uninstalling package %s'#039';'#010+
  '  SInfoArchivingPackage   = '#039'Archiving package %s in "%s"'#039';'#010+
  '  SInfoCleaningPackage    = '#039'Cleaning package %s'#039';'#010+
  '  SInfoCleanPackage','complete = '#039'Clean of package %s completed'#039+
  ';'#010+
  '  SInfoManifestPackage    = '#039'Creating manifest for package %s'#039';'+
  #010+
  '  SInfoPackageDocProject  = '#039'Creating fpdoc project file for packa'+
  'ge %s'#039';'#010+
  '  SInfoDocumentingPackage = '#039'Creating documentation for pa','ckage '+
  '%s'#039';'#010+
  '  SInfoPkgListPackage    = '#039'Adding package %s to the package list'#039+
  ';'#010+
  '  SInfoCopyingFile        = '#039'Copying file "%s" to "%s"'#039';'#010+
  '  SInfoDeletedFile        = '#039'Deleted file "%s"'#039';'#010+
  '  SInfoRemovedDirectory   = '#039'Removed directory "%s"'#039';'#010+
  ' ',' SInfoSourceNewerDest    = '#039'Source file "%s" (%s) is newer than'+
  ' destination "%s" (%s).'#039';'#010+
  '  SInfoDestDoesNotExist   = '#039'Destination file "%s" does not exist.'+
  #039';'#010+
  '  SInfoFallbackBuildmode  = '#039'Buildmode not supported by package, f'+
  'alling back to o','ne by one unit compilation'#039';'#010+
  '  SInfoFallbackBuildmodeBU= '#039'Buildmode not supported by package, f'+
  'alling back to compilation using a buildunit'#039';'#010+
  '  SInfoDirectoryNoPackage = '#039'Found directory "%s" while searching '+
  'for package "%s" which does not ','contain a package'#039';'#010+
  '  SInfoSrcDirectoryNoPkg  = '#039'Found source-directory "%s" while sea'+
  'rching for package "%s" but the package is not compiled or has the wro'+
  'ng name'#039';'#010+
  #010+
  '  SDbgComparingFileTimes    = '#039'Comparing file "%s" time "%s" to "%'+
  's" time',' "%s".'#039';'#010+
  '  SDbgCompilingDependenciesOfTarget = '#039'Compiling dependencies of t'+
  'arget %s'#039';'#010+
  '  SDbgResolvingSourcesOfTarget = '#039'Resolving filenames of target %s'+
  ' for %s'#039';'#010+
  '  SDbgResolvedSourceFile    = '#039'Resolved source file %s to "%s"'#039+
  ';'#010+
  '  SDbgSourceA','lreadyResolved = '#039'Source file of %s has been resolv'+
  'ed earlier'#039';'#010+
  '  SDbgResolvedIncludeFile   = '#039'Resolved include file %s to "%s"'#039+
  ';'#010+
  '  SDbgOutputNotYetAvailable = '#039'Output file %s not available'#039';'#010+
  '  SDbgDependencyOnUnit      = '#039'Dependency of %s ','on unit %s'#039';'+
  #010+
  '  SDbgDependencyUnitRecompiled = '#039'Dependent unit %s is being recom'+
  'piled'#039';'#010+
  '  SDbgMustCompile           = '#039'Must compile %s. (%s)'#039';'#010+
  '  SDbgSkippingTargetWrongCPU = '#039'Skipping target %s, different CPU '+
  '(%s)'#039';'#010+
  '  SDbgSkippingTargetWrong','OS  = '#039'Skipping target %s, different OS'+
  ' (%s)'#039';'#010+
  '  SDbgSkippingTargetWrongSubTarget  = '#039'Skipping target %s, differe'+
  'nt Subtarget (allowed: %s)'#039';'#010+
  '  SDbgTargetIsNotAUnitOrProgram = '#039'Skipping Target %s, not an unit'+
  ' or program'#039';'#010+
  '  SDbgConsideringT','arget     = '#039'Considering target %s'#039';'#010+
  '  SDbgConsideringPackage    = '#039'Considering package %s'#039';'#010+
  '  SDbgSearchExtDepPath      = '#039'Search path for external dependency'+
  ' %s'#039';'#010+
  '  SDbgExternalDependency    = '#039'External dependency %s found in "%s'+
  '"'#039';'#010+
  '  SDb','gBuildEngineArchiving  = '#039'Build engine archiving'#039';'#010+
  '  SDbgBuildEngineGenerateManifests = '#039'Build engine generating mani'+
  'fests'#039';'#010+
  '  SDbgBuildEngineGeneratePkgList = '#039'Build engine generating packag'+
  'e list'#039';'#010+
  '  SDbgBuildEngineCleaning   = '#039'Build eng','ine cleaning'#039';'#010+
  '  SDbgGenerating            = '#039'Generating "%s"'#039';'#010+
  '  SDbgLoading               = '#039'Loading "%s"'#039';'#010+
  '  SDbgFound                 = '#039'Found'#039';'#010+
  '  SDbgNotFound              = '#039'Not Found'#039';'#010+
  '  SDbgDirectoryExists       = '#039'Directory "%s" %s',#039';'#010+
  '  SDbgFileExists            = '#039'File "%s" %s'#039';'#010+
  '  SDbgArchivingFile         = '#039'Archiving "%s"'#039';'#010+
  '  SDbgSearchPath            = '#039'Using %s path "%s"'#039';'#010+
  '  SDbgEnterDir              = '#039'Entering directory "%s"'#039';'#010+
  '  SDbgPackageChecksumChanged = '#039'Pa','ckage %s depends on the %s pac'+
  'kage which has been modified'#039';'#010+
  '  SDbgFileDoesNotExist      = '#039'File "%s" does not exist'#039';'#010+
  '  SDbgDirectoryDoesNotExist = '#039'Directory "%s" does not exist'#039';'+
  #010+
  '  SDbgDirectoryNotEmpty     = '#039'Directory "%s" is not empty','. Will'+
  ' not remove'#039';'#010+
  '  SDbgGenerateBuildUnit     = '#039'Generate build-unit %s'#039';'#010+
  '  SDbgBuildUnitFailure      = '#039'Generate build-unit %s failed'#039';'+
  #010+
  '  SDbgForcedCompile         = '#039'Forced compile'#039';'#010+
  '  SDbgOutputDoesNotExist    = '#039'Output file does not e','xist'#039';'#010+
  '  SDbgNewerSource           = '#039'Source file is newer then output fi'+
  'le'#039';'#010+
  '  SDbgNewerInclude          = '#039'The include file %s is newer then o'+
  'utput file'#039';'#010+
  '  SDbgDependencyRecompiled  = '#039'The unit %s where this unit depends'+
  ' on is recompiled',#039';'#010+
  '  SDbgPackageDepRecompiled  = '#039'The package %s where this package d'+
  'epends on is recompiled'#039';'#010+
  '  SDbgTargetHasToBeCompiled = '#039'At least one of the targets in the '+
  'package has to be compiled.'#039';'#010+
  '  SDbgDeletedFile           = '#039'Recursively delete','d file "%s"'#039+
  ';'#010+
  '  SDbgRemovedDirectory      = '#039'Recursively removed directory "%s"'#039+
  ';'#010+
  '  SDbgUnregisteredResource  = '#039'Adding resource file "%s", which is'+
  ' not registered.'#039';'#010+
  '  SDbgSearchingDir          = '#039'Searching dir  %s.'#039';'#010+
  #010+
  '  // Help messages f','or usage'#010+
  '  SValue              = '#039'Value'#039';'#010+
  '  SHelpUsage          = '#039'Usage: %s command [options]'#039';'#010+
  '  SHelpCommand        = '#039'Where command is one of the following:'#039+
  ';'#010+
  '  SHelpCompile        = '#039'Compile all units in the package(s).'#039';'+
  #010+
  '  SHelpBuild ','         = '#039'Build all units in the package(s).'#039';'+
  #010+
  '  SHelpInstall        = '#039'Install all units in the package(s).'#039';'+
  #010+
  '  SHelpBuildInstall   = '#039'Build and install all units in the packag'+
  'e(s).'#039';'#010+
  '  SHelpUnInstall      = '#039'Uninstall the package(s).'#039';'#010,
  '  SHelpClean          = '#039'Clean (remove) all generated files in the'+
  ' package(s) for current CPU-OS target.'#039';'#010+
  '  SHelpDistclean      = '#039'Clean (remove) all generated files in the'+
  ' package(s) for all targets.'#039';'#010+
  '  SHelpArchive        = '#039'Create arch','ive (zip) with all units in '+
  'the package(s).'#039';'#010+
  '  SHelpHelp           = '#039'This message.'#039';'#010+
  '  SHelpManifest       = '#039'Create a manifest suitable for import in '+
  'repository.'#039';'#010+
  '  SHelpPkgList        = '#039'Create list of all packages suitable for '+
  'FPC ins','taller.'#039';'#010+
  '  SHelpFPDocProject   = '#039'Create fpdoc project file(s) for all pack'+
  'ages'#039';'#010+
  '  SHelpDoc            = '#039'Create documentation with fpdoc for all p'+
  'ackages'#039';'#010+
  '  SHelpZipInstall     = '#039'Install all units in the package(s) into '+
  'an archive.'#039';'#010+
  ' ',' SHelpCmdOptions     = '#039'Where options is one or more of the fol'+
  'lowing:'#039';'#010+
  '  SHelpCPU            = '#039'Compile for indicated CPU.'#039';'#010+
  '  SHelpOS             = '#039'Compile for indicated OS'#039';'#010+
  '  SHelpSubTarget      = '#039'Compile for indicated subtarget'#039';'#010+
  '  ','SHelpTarget         = '#039'Compile for indicated target, string in'+
  ' the form CPU-OS(-subtarget)'#039';'#010+
  '  SHelpList           = '#039'list commands instead of actually executi'+
  'ng them.'#039';'#010+
  '  SHelpPrefix         = '#039'Use indicated prefix directory for all co'+
  'mman','ds.'#039';'#010+
  '  SHelpNoFPCCfg       = '#039'Compiler will not use fpc.cfg'#039';'#010+
  '  SHelpBaseInstallDir = '#039'Use indicated directory as base install d'+
  'ir.'#039';'#010+
  '  SHelpLocalUnitDir   = '#039'Use indicated directory as local (user) u'+
  'nit dir.'#039';'#010+
  '  SHelpOutpuUnitDir   = '#039'Use',' indicated directory to save the com'+
  'piled units.'#039';'#010+
  '  SHelpGlobalUnitDir  = '#039'Use indicated directory as global unit di'+
  'r.'#039';'#010+
  '  SHelpSearchPath     = '#039'Add search directory for packages.'#039';'#010+
  '  SHelpUnitInstallDir = '#039'Use indicated directory to inst','all unit'+
  's into.'#039';'#010+
  '  SHelpCompiler       = '#039'Use indicated binary as compiler'#039';'#010+
  '  SHelpConfig         = '#039'Use indicated config file when compiling.'+
  #039';'#010+
  '  SHelpOptions        = '#039'Pass extra options to the compiler.'#039';'+
  #010+
  '  SHelpVerbose        = '#039'Be ve','rbose when working.'#039';'#010+
  '  SHelpDebug          = '#039'Add debug information when working.'#039';'+
  #010+
  '  SHelpInteractive    = '#039'Allow to interact with child processes'#039+
  ';'#010+
  '  SHelpInstExamples   = '#039'Install the example-sources.'#039';'#010+
  '  SHelpBuildExamples  = '#039'Compile ','the example units and programs.'+
  #039';'#010+
  '  SHelpSkipCrossProgs = '#039'Skip programs when cross-compiling/instal'+
  'ling'#039';'#010+
  '  SHelpSkipAllProgs   = '#039'Skip all programs even if native-compilin'+
  'g/installing'#039';'#010+
  '  SHelpIgnoreInvOpt   = '#039'Ignore further invalid opti','ons.'#039';'#010+
  '  sHelpFpdocOutputDir = '#039'Use indicated directory as fpdoc output f'+
  'older.'#039';'#010+
  '  sHelpSingleFpdocFile = '#039'Create a single fpdoc project file for a'+
  'll projects'#039';'#010+
  '  sHelpDocOptionsFile = '#039'Name=Value File with options for fpdoc pr'+
  'oject file'#039';',#010+
  '  sHelpFPUnitSrcPath  = '#039'Sourcepath to replace in fpunits.cfg on i'+
  'nstallation.'#039';'#010+
  '  sHelpThreads        = '#039'Enable the indicated amount of worker thr'+
  'eads.'#039';'#010+
  '  {$ifdef HAS_UNIT_PROCESS}'#010+
  '  sHelpUseEnvironment = '#039'Use environment to pass options',' to comp'+
  'iler.'#039';'#010+
  '  {$endif}'#010+
  '  SHelpUseBuildUnit   = '#039'Compile package in Build-unit mode.'#039';'+
  #010+
  '  sHelpZipPrefix      = '#039'Use indicated prefix for generated archiv'+
  'es.'#039';'#010+
  '  sHelpPackageVariant1= '#039'To add a package-variant to all packages:'+
  #039';'#010+
  '  sHelpPa','ckageVariant2= '#039' +[variantname]+=[variant1],<variant2>,'+
  '...'#039';'#010+
  '  sHelpPackageVariant3= '#039'To add a package-variant to all packages '+
  'which other packages will inherit:'#039';'#010+
  '  sHelpPackageVariant4= '#039' +[variantname]*=[variant1],<variant2>,..'+
  '.'#039';'#010+
  '  sHel','pPackageVariant5= '#039'To add specific options for one package'+
  '-variant:'#039';'#010+
  '  sHelpPackageVariant6= '#039' --options_[variantname]_[variant1]=Value'+
  #039';'#010+
  '  SSwitchingToBuildUnitCompilation = '#039'Switching to buildunit compi'+
  'lation of package %s'#039';'#010+
  #010+
  #010+
  'Const'#010+
  '  //',' Keys for Defaults file. Do not localize.'#010+
  '  KeyCompiler = '#039'Compiler'#039';'#010+
  '  KeyArchive  = '#039'Archive'#039';'#010+
  '  KeyCopy     = '#039'Copy'#039';'#010+
  '  KeyMkDir    = '#039'MkDir'#039';'#010+
  '  KeyMove     = '#039'Move'#039';'#010+
  '  KeyRemove   = '#039'Remove'#039';'#010+
  '  KeyRemoveDir= '#039'RemoveDir'#039';'#010+
  '  KeyRemoveTree','= '#039'RemoveTree'#039';'#010+
  '  KeyOptions  = '#039'Options'#039';'#010+
  '  KeyCPU      = '#039'CPU'#039';'#010+
  '  KeyOS       = '#039'OS'#039';'#010+
  '  KeySubTargets = '#039'SubTargets'#039';'#010+
  '  KeyMode     = '#039'Mode'#039';'#010+
  '  KeyPrefix   = '#039'Prefix'#039';'#010+
  '  KeyTarget   = '#039'Target'#039';'#010+
  '  KeyNoFPCCfg = '#039'NoFPCCfg'#039';'#010+
  '  KeyUseEnv   = ',#039'UseEnv'#039';'#010+
  '  KeyPluginUnits        = '#039'PluginUnits'#039';'#010+
  '  KeyLocalUnitDir       = '#039'LocalUnitDir'#039';'#010+
  '  KeyGlobalUnitDir      = '#039'GlobalUnitDir'#039';'#010+
  '  KeyBaseInstallDir     = '#039'BaseInstallDir'#039';'#010+
  '  KeyUnitInstallDir     = '#039'UnitInstallDir'#039';'#010+
  '  KeyBinInstallD','ir      = '#039'BinInstallDir'#039';'#010+
  '  KeyLibInstallDir      = '#039'LibInstallDir'#039';'#010+
  '  KeyDocInstallDir      = '#039'DocInstallDir'#039';'#010+
  '  KeyExamplesInstallDir = '#039'ExamplesInstallDir'#039';'#010+
  '  KeyInstallExamples    = '#039'InstallExamples'#039';'#010+
  '  KeyBuildExamples      = '#039'BuildEx','amples'#039';'#010+
  '  KeySkipCrossPrograms  = '#039'SkipCrossPrograms'#039';'#010+
  '  KeySkipAllPrograms    = '#039'SkipAllPrograms'#039';'#010+
  '  // Keys for unit config'#010+
  '  KeyName     = '#039'Name'#039';'#010+
  '  KeyVersion  = '#039'Version'#039';'#010+
  '  KeyChecksum = '#039'Checksum'#039';'#010+
  '  KeyNeedLibC = '#039'NeedLibC'#039';'#010+
  '  KeyD','epends  = '#039'Depends'#039';'#010+
  '  KeyFlags    = '#039'Flags'#039';'#010+
  '  KeyTransmit = '#039'TransmitOptions'#039';'#010+
  '  KeyAddIn    = '#039'FPMakeAddIn'#039';'#010+
  '  KeySourcePath = '#039'SourcePath'#039';'#010+
  '  KeyFPMakeOptions = '#039'FPMakeOptions'#039';'#010+
  '  KeyPackageVar = '#039'PackageVariant_'#039';'#010+
  #010+
  '{*******************','************************************************'+
  '*********'#010+
  '                                Helpers'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  'function ExecuteFPC(Verbose, Interact','ive: boolean; const aCompiler: '+
  'string; const Args: TStrings; const Env: TStrings; ConsoleOutput: TMem'+
  'oryStream): integer;'#010+
  'var'#010+
  '  P: TProcess;'#010+
  '  BytesRead: longint;'#010+
  #010+
  '  function ReadFromStream(const ReadFromStdErr: boolean): longint;'#010+
  #010+
  '  type'#010+
  ' ','   TMessages = (mCompiling, mLinking);'#010+
  #010+
  '  var'#010+
  '    //ifdef the MsgNum so it contains the correct message numbers for '+
  'each compiler version.'#010+
  '    MsgNum : array [TMessages] of integer = (3104, 9015);'#010+
  #010+
  '    n,available: longint;'#010+
  '    BuffPos: lon','gint;'#010+
  '    sLine: string;'#010+
  '    ch: AnsiChar;'#010+
  '    msg: TMessages;'#010+
  '    ipos: integer;'#010+
  '    snum: string;'#010+
  '  begin'#010+
  #010+
  '    // try reading it'#010+
  '    if ReadFromStdErr then'#010+
  '      begin'#010+
  '        available:=P.Stderr.NumBytesAvailable;'#010+
  '        // make sure we',' have room'#010+
  '        if (bytesRead + Available > ConsoleOutput.Size) then'#010+
  '          ConsoleOutput.SetSize(BytesRead + Available);'#010+
  '        n := P.Stderr.Read((ConsoleOutput.Memory + BytesRead)^, availa'+
  'ble);'#010+
  '      end'#010+
  '    else'#010+
  '      begin'#010+
  '     ','   available:=P.Output.NumBytesAvailable;'#010+
  '        // make sure we have room'#010+
  '        if (bytesRead + Available > ConsoleOutput.Size) then'#010+
  '          ConsoleOutput.SetSize(BytesRead + Available);'#010+
  '        n := P.Output.Read((ConsoleOutput.Memor','y + BytesRead)^, avai'+
  'lable);'#010+
  '      end;'#010+
  '    if n > 0 then'#010+
  '    begin'#010+
  '      Inc(BytesRead, n);'#010+
  #010+
  '      sLine := '#039#039';'#010+
  '      BuffPos := ConsoleOutput.Position;'#010+
  '      ch:=#0;'#010+
  '      //read lines from the stream'#010+
  '      repeat'#010+
  '        ConsoleOutput.Re','ad(ch,1);'#010+
  #010+
  '        if ch in [#10, #13] then'#010+
  '        begin'#010+
  '          if Interactive then'#010+
  '            System.Writeln(output)'#010+
  '          else if Verbose then'#010+
  '            installer.log(vlInfo,sLine)'#010+
  '          else'#010+
  '            begin'#010+
  '             ',' for msg := Low(TMessages) to High(TMessages) do'#010+
  '              begin'#010+
  '                snum := Format('#039'(%d)'#039', [MsgNum[msg]]);'#010+
  '                ipos := Pos(snum, sLine);'#010+
  '                if ipos = 1 then'#010+
  '                  installer.log(vlCommand',','#039'      '#039'+ Copy(sLi'+
  'ne, ipos + Length(snum), Length(sLine) - ipos - Length(snum) + 1));'#010+
  '              end;'#010+
  '            end;'#010+
  '          if (LineEnding=#13#10) and (ch=#13) and'#010+
  '             (ConsoleOutput.Position<BytesRead) then'#010+
  '            be','gin'#010+
  '              ConsoleOutput.Read(ch,1);'#010+
  '              if ch=#10 then'#010+
  '                sLine:='#039#039#010+
  '              else'#010+
  '                begin'#010+
  '                  if Interactive then'#010+
  '                    begin'#010+
  '                      System.Write(o','utput,ch);'#010+
  '                    end'#010+
  '                  else'#010+
  '                    sLine:=ch;'#010+
  '                end;'#010+
  '            end'#010+
  '          else'#010+
  '            sLine := '#039#039';'#010+
  '          BuffPos := ConsoleOutput.Position;'#010+
  '        end'#010+
  '        else'#010+
  '    ','    begin'#010+
  '          if Interactive then'#010+
  '            System.Write(output,ch)'#010+
  '          else'#010+
  '            sLine := sLine + ch;'#010+
  '        end;'#010+
  #010+
  '      until ConsoleOutput.Position >= BytesRead;'#010+
  #010+
  '      // keep partial lines, unlessin interactive mo','de'#010+
  '      if not Interactive then'#010+
  '        ConsoleOutput.Position := BuffPos'#010+
  '        // Flush for interactive mode'#010+
  '      else if n > 0 then'#010+
  '        System.Flush(output);'#010+
  '    end;'#010+
  #010+
  '    Result := n;'#010+
  '  end;'#010+
  #010+
  'begin'#010+
  '  result := -1;'#010+
  '  BytesRead := ','0;'#010+
  '  P := TProcess.Create(nil);'#010+
  '  try'#010+
  '    P.Executable:=aCompiler;'#010+
  '    P.Parameters:=Args;'#010+
  '    if assigned(Env) then'#010+
  '      P.Environment.Assign(Env);'#010+
  #010+
  '    if Interactive then'#010+
  '      P.Options := [poUsePipes,poPassInput]'#010+
  '    else'#010+
  '      P.Opti','ons := [poUsePipes];'#010+
  #010+
  '    P.Execute;'#010+
  '    while P.Running do'#010+
  '      begin'#010+
  '        // Only call ReadFromStream if Data from corresponding stream'#010+
  '        // is already available, otherwise, on  linux, the read call'#010+
  '        // is blocking, and t','hus it is not possible to be sure to ha'+
  'ndle'#010+
  '        // big data amounts bboth on output and stderr pipes. PM.'#010+
  '        if P.Output.NumBytesAvailable > 0 then'#010+
  '          ReadFromStream(false)'#010+
  '        else if P.StdErr.NumBytesAvailable > 0 then',#010+
  '          ReadFromStream(true)'#010+
  '        else'#010+
  '      // no data, wait 100 ms'#010+
  '          Sleep(100);'#010+
  '      end;'#010+
  #010+
  '    // read last part'#010+
  '    repeat'#010+
  '    until ReadFromStream(false)=0;'#010+
  #010+
  '    // read stderr'#010+
  '    // JvdS: Note that this way stderr is a','dded to the end of the st'+
  'ream. But I'#010+
  '    // see no way showing the stderr output at the place it was actual'+
  'ly written'#010+
  '    repeat'#010+
  '    until ReadFromStream(true)=0;'#010+
  '    ConsoleOutput.SetSize(BytesRead);'#010+
  #010+
  '    result := P.ExitStatus;'#010+
  '  finally'#010,
  '    P.Free;'#010+
  '  end;'#010+
  'end;'#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  #010+
  'function IsDirectoryEmpty(const directory : string) : boolean;'#010+
  'var'#010+
  '  searchRec: TSearchRec;'#010+
  '  SearchResult: longint;'#010+
  'begin'#010+
  '  result := true;'#010+
  '  SearchResult := FindFirst(IncludeTrailingPathD','elimiter(directory)+'+
  'AllFilesMask, faAnyFile+faSymLink, searchRec);'#010+
  '  try'#010+
  '    while SearchResult=0 do'#010+
  '      begin'#010+
  '        if (searchRec.Name<>'#039'.'#039') and (searchRec.Name<>'#039'..'#039+
  ') then'#010+
  '           begin'#010+
  '             result := false;'#010+
  '             br','eak;'#010+
  '           end;'#010+
  '        SearchResult := FindNext(searchRec);'#010+
  '      end;'#010+
  '  finally'#010+
  '    {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.FindClose(searchRe'+
  'c);'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'function ParsecompilerOutput(M: TMemoryStream; Verbose: boolean): ','an'+
  'sistring;'#010+
  'type'#010+
  '  TParseCompilerOutputState = (cosBeginOfLine, cosSearchColon, cosPars'+
  'eNumber, cosOther);'#010+
  #010+
  'var'#010+
  '  presult: PAnsiChar;'#010+
  '  state: TParseCompilerOutputState;'#010+
  '  ch: AnsiChar;'#010+
  '  eolchar: AnsiChar;'#010+
  #010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  m.Seek(0, ','soBeginning);'#010+
  '  setlength(Result,M.Size);'#010+
  #010+
  '  if verbose then'#010+
  '    begin'#010+
  '      m.Read(Result[1],M.Size);'#010+
  '      Exit;'#010+
  '    end;'#010+
  #010+
  '  presult := @Result[1];'#010+
  '  eolchar := RightStr(LineEnding,1)[1];'#010+
  '  m.Seek(0,soBeginning);'#010+
  '  state := cosBeginOfLine',';'#010+
  '  while m.Position<m.Size do'#010+
  '    begin'#010+
  '      ch := AnsiChar(m.ReadByte);'#010+
  '      case state of'#010+
  '        cosBeginOfLine:'#010+
  '          begin'#010+
  '            if ch='#039'('#039' then'#010+
  '              state := cosParseNumber'#010+
  '            else if ch='#039' '#039' then'#010+
  '        ','      begin'#010+
  '                presult^ := ch;'#010+
  '                inc(presult);'#010+
  '              end'#010+
  '            else'#010+
  '              begin'#010+
  '                presult^ := ch;'#010+
  '                inc(presult);'#010+
  '                state := cosSearchColon;'#010+
  '        ','      end;'#010+
  '          end;'#010+
  '        cosParseNumber:'#010+
  '          begin'#010+
  '            if ch='#039')'#039' then'#010+
  '              begin'#010+
  '              state := cosOther;'#010+
  '              // Omit the space behind the number'#010+
  '              ch := AnsiChar(m.ReadByte);'#010+
  '  ','            assert(ch='#039' '#039');'#010+
  '              end;'#010+
  '          end;'#010+
  '        cosOther:'#010+
  '          begin'#010+
  '            presult^ := ch;'#010+
  '            inc(presult);'#010+
  '            if ch=eolchar then'#010+
  '              state := cosBeginOfLine;'#010+
  '          end;'#010+
  '     ','   cosSearchColon:'#010+
  '          begin'#010+
  '            presult^ := ch;'#010+
  '            inc(presult);'#010+
  '            if (ch='#039':'#039') or (ch=eolchar) then'#010+
  '              state := cosBeginOfLine;'#010+
  '          end;'#010+
  '      end;'#010+
  '    end;'#010+
  '  setlength(Result,presult-@resu','lt[1]);'#010+
  'end;'#010+
  #010+
  'Function QuoteXML(S : String) : string;'#010+
  #010+
  '  Procedure W(Var J : Integer; Var R : String; T : String);'#010+
  '  Var'#010+
  '    I: integer;'#010+
  '  begin'#010+
  '    If J+Length(T)>Length(R) then'#010+
  '      SetLength(R,J+Length(T));'#010+
  '    For I:=1 to Length(t) do'#010,
  '      begin'#010+
  '      R[J]:=T[i];'#010+
  '      If I<Length(T) then'#010+
  '        Inc(J);'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  'const'#010+
  '  QuotStr = '#039'&quot;'#039';'#010+
  '  AmpStr = '#039'&amp;'#039';'#010+
  '  ltStr = '#039'&lt;'#039';'#010+
  '  gtStr = '#039'&gt;'#039';'#010+
  'Var'#010+
  '  I,J : Integer;'#010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  SetLength(Result,Lengt','h(S));'#010+
  '  J:=0;'#010+
  '  For I:=1 to Length(S) do'#010+
  '    begin'#010+
  '    Inc(J);'#010+
  '    case S[i] of'#010+
  '      '#039'"'#039': W(j,Result,QuotStr);'#010+
  '      '#039'&'#039': W(J,Result,AmpStr);'#010+
  '      '#039'<'#039': W(J,Result,ltStr);'#010+
  '      '#039'>'#039': W(J,Result,gtStr);'#010+
  '      // Escape whitespace using Cha','rRefs to be consistent with W3 s'+
  'pec X 3.3.3'#010+
  '       #9: w(J,Result,'#039'&#x9;'#039');'#010+
  '{      #10: wrtStr('#039'&#xA;'#039');'#010+
  '       #13: wrtStr('#039'&#xD;'#039');}'#010+
  '    else'#010+
  '      Result[J]:=S[i];'#010+
  '    end;'#010+
  '    If (J=Length(Result)) and (I<Length(S)) then'#010+
  '      SetLength','(Result,J+Length(S)-I);'#010+
  '    end;'#010+
  '  If J<>Length(Result) then'#010+
  '    SetLength(Result,J);'#010+
  'end;'#010+
  #010+
  #010+
  'function maybequoted(const s:string):string;'#010+
  'const'#010+
  '  {$IF DEFINED(MSWINDOWS) OR DEFINED(AMIGA) OR DEFINED(MORPHOS) OR DEF'+
  'INED(AROS)}'#010+
  '    FORBIDDEN_','CHARS = ['#039'!'#039', '#039'@'#039', '#039'#'#039', '#039'$'+
  #039', '#039'%'#039', '#039'^'#039', '#039'&'#039', '#039'*'#039', '#039'('#039', '+
  #039')'#039','#010+
  '                       '#039'{'#039', '#039'}'#039', '#039#039#039#039', '#039'`'+
  #039', '#039'~'#039'];'#010+
  '  {$ELSE}'#010+
  '    FORBIDDEN_CHARS = ['#039'!'#039', '#039'@'#039', '#039'#'#039', '#039'$'#039+
  ', '#039'%'#039', '#039'^'#039', '#039'&'#039', '#039'*'#039', '#039'('#039', '#039+
  ')'#039','#010+
  '                       '#039'{'#039', '#039'}'#039', '#039#039#039#039', '#039':'+
  #039', '#039'\'#039',',' '#039'`'#039', '#039'~'#039'];'#010+
  '  {$ENDIF}'#010+
  'var'#010+
  '  s1 : string;'#010+
  '  i  : integer;'#010+
  '  quoted : boolean;'#010+
  'begin'#010+
  '  quoted:=false;'#010+
  '  s1:='#039'"'#039';'#010+
  '  for i:=1 to length(s) do'#010+
  '   begin'#010+
  '     case s[i] of'#010+
  '       '#039'"'#039' :'#010+
  '         begin'#010+
  '           quoted:=true;'#010+
  '           s1:=s1+'#039'\"',#039';'#010+
  '         end;'#010+
  '       '#039' '#039','#010+
  '       #128..#255 :'#010+
  '         begin'#010+
  '           quoted:=true;'#010+
  '           s1:=s1+s[i];'#010+
  '         end;'#010+
  '       else begin'#010+
  '         if s[i] in FORBIDDEN_CHARS then'#010+
  '           quoted:=True;'#010+
  '         s1:=s1+s[i];'#010+
  '       ','end;'#010+
  '     end;'#010+
  '   end;'#010+
  '  if quoted then'#010+
  '    maybequoted:=s1+'#039'"'#039#010+
  '  else'#010+
  '    maybequoted:=s;'#010+
  'end;'#010+
  #010+
  'Procedure GetSubTargetDirs(aBaseDir : String; aList : TStrings);'#010+
  #010+
  'Var'#010+
  '  Info : TSearchRec;'#010+
  '  aDir : string;'#010+
  #010+
  'begin'#010+
  '  aDir:=ExtractFileDir(aBase','Dir);'#010+
  '  if FindFirst(aBaseDir+'#039'-*'#039',faDirectory,Info)=0 then'#010+
  '    try'#010+
  '      Repeat'#010+
  '        if (Info.Attr and faDirectory)=faDirectory then'#010+
  '          aList.Add(aDir+Info.Name);'#010+
  '      until (FindNext(Info)<>0);'#010+
  '    finally'#010+
  '      FindClose(Info)',';'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'Function GetSubTargetDirs(aBaseDir : String) : TStringDynArray;'#010+
  #010+
  'Var'#010+
  '  L : Tstrings;'#010+
  #010+
  'begin'#010+
  '  L:=TStringList.Create;'#010+
  '  try'#010+
  '    GetSubTargetDirs(aBaseDir,L);'#010+
  '    Result:=L.ToStringArray;'#010+
  '  finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  #010+
  'end;'#010+
  #010+
  'p','rocedure ReadIniFile(Const AFileName: String;L:TStrings);'#010+
  'Var'#010+
  '  F : TFileStream;'#010+
  '  Line : String;'#010+
  '  I,P,PC : Integer;'#010+
  'begin'#010+
  '  // On some file systems and when using a large number of parallel ma'+
  'ke'#010+
  '  // processes, the lock from the creation ','of the ini file may not y'+
  'et'#010+
  '  // have been released even though the file has been closed already'#010+
  '  F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyNone);'#010+
  '  Try'#010+
  '    L.LoadFromStream(F);'#010+
  '    // Fix lines.'#010+
  '    For I:=L.Count-1 downto ','0 do'#010+
  '      begin'#010+
  '        Line:=L[I];'#010+
  '        P:=Pos('#039'='#039',Line);'#010+
  '        PC:=Pos('#039';'#039',Line);  // Comment line.'#010+
  '        If (P=0) or ((PC<>0) and (PC<P)) then'#010+
  '          L.Delete(I)'#010+
  '        else'#010+
  '          L[i]:=Trim(System.Copy(Line,1,P-1)+'#039'='#039'+Tr','im(System'+
  '.Copy(Line,P+1,Length(Line)-P)));'#010+
  '      end;'#010+
  '  Finally'#010+
  '    F.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  '// Callback for Sysutils getapplicationname.'#010+
  'Function GetFPMakeName : String;'#010+
  #010+
  'begin'#010+
  '  Result:='#039'fpmake'#039';'#010+
  'end;'#010+
  #010+
  #010+
  'Function CurrentOS : String;'#010+
  #010+
  'begin'#010+
  '  ','Result:=OSToString(Defaults.OS);'#010+
  'end;'#010+
  #010+
  'Function CurrentCPU : String;'#010+
  #010+
  'begin'#010+
  '  Result:=CPUToString(Defaults.CPU);'#010+
  'end;'#010+
  #010+
  'Function OSToString(OS: TOS) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=LowerCase(GetenumName(TypeInfo(TOS),Ord(OS)));'#010+
  'end;'#010+
  #010+
  'Function OSe','sToString(OSes: TOSes) : String;'#010+
  #010+
  'var'#010+
  '  i: TOS;'#010+
  '  Tmp: String;'#010+
  '  First: boolean;'#010+
  #010+
  'begin'#010+
  '  { can'#039't use SetToString, there are more than 32 OSes }'#010+
  '  First:=true;'#010+
  '  Result:='#039#039';'#010+
  '  for i in OSes do'#010+
  '    begin'#010+
  '      if not First then'#010+
  '        Resul','t:=Result+'#039','#039#010+
  '      else'#010+
  '        First:=false;'#010+
  '      Str(i,Tmp);'#010+
  '      Result:=Result+Tmp;'#010+
  '    end;'#010+
  '  Result:=LowerCase(Result);'#010+
  'end;'#010+
  #010+
  'Function CPUToString(CPU: TCPU) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=LowerCase(GetenumName(TypeInfo(TCPU),Ord(CPU))',');'#010+
  'end;'#010+
  #010+
  'Function CPUSToString(CPUS: TCPUS) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=LowerCase(SetToString(PTypeInfo(TypeInfo(TCPUS)),Integer(CPU'+
  'S),False));'#010+
  'end;'#010+
  #010+
  'Function StringToOS(const S : String) : TOS;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=GetEnumValue(T','ypeInfo(TOS),S);'#010+
  '  if (I=-1) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidOS,[S]);'#010+
  '  Result:=TOS(I);'#010+
  'end;'#010+
  #010+
  '(*'#010+
  'Function StringToOSes(const S : String) : TOSes;'#010+
  #010+
  'begin'#010+
  '  Result:=TOSes(StringToSet(PTypeInfo(TypeInfo(TOSes)),S));'#010+
  'end;'#010+
  '*)',#010+
  #010+
  'function IsDifferentFromBuild(ACpu: TCPU; AOs: TOs): boolean;'#010+
  'begin'#010+
  '  result := (AOs<>Defaults.BuildOS) or (ACpu<>Defaults.BuildCPU);'#010+
  'end;'#010+
  #010+
  'Function StringToCPU(const S : String) : TCPU;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=GetEnumValue(TypeIn','fo(TCPU),S);'#010+
  '  if (I=-1) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidCPU,[S]);'#010+
  '  Result:=TCPU(I);'#010+
  'end;'#010+
  #010+
  'Function StringToCPUS(const S : String) : TCPUS;'#010+
  #010+
  'begin'#010+
  '  Result:=TCPUS(StringToSet(PTypeInfo(TypeInfo(TCPUS)),S));'#010+
  'end;'#010+
  #010+
  'Functi','on ModeToString(Mode: TCompilerMode) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=LowerCase(GetenumName(TypeInfo(TCompilerMode),Ord(Mode)));'#010+
  '  Delete(Result,1,2);'#010+
  'end;'#010+
  #010+
  'Function StringToMode(const S : String) : TCompilerMode;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=','GetEnumValue(TypeInfo(TCompilerMode),S);'#010+
  '  if (I=-1) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidMode,[S]);'#010+
  '  Result:=TCompilerMode(I);'#010+
  'end;'#010+
  #010+
  #010+
  'Function MakeTargetString(const aTarget : TCompileTarget; aLimit83 : B'+
  'oolean) : String;'#010+
  #010,
  'begin'#010+
  '  Result:=aTarget.ToString(aLimit83);'#010+
  'end;'#010+
  #010+
  'Function MakeTargetString(const aTarget : TCompileTarget) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=aTarget.ToString((Defaults.BuildOS in AllLimit83fsOses) or ('+
  'aTarget.OS in AllLimit83fsOses));'#010+
  'end;'#010+
  #010+
  'Funct','ion MakeTargetString(CPU : TCPU;OS: TOS; aSubTarget : String = '+
  #039#039') : String;'#010+
  #010+
  'var'#010+
  '  CT : TCompileTarget;'#010+
  #010+
  'begin'#010+
  '  CT.CPU:=CPU;'#010+
  '  CT.OS:=OS;'#010+
  '  CT.Subtarget:=aSubTarget;'#010+
  '  Result := MakeTargetString (CT);'#010+
  'end;'#010+
  #010+
  'function MakeZipSuffix(const aT','arget : TCompileTarget; ALimit83: boo'+
  'lean) : String;'#010+
  #010+
  'begin'#010+
  '  case aTarget.OS of'#010+
  '    go32v2: result := '#039'dos'#039';'#010+
  '    watcom: result := '#039'wat'#039';'#010+
  '    os2:    result := '#039'os2'#039';'#010+
  '    emx:    result := '#039'emx'#039';'#010+
  '    osNone:'#010+
  '      begin'#010+
  '        if ALimit83',' then'#010+
  '          result := '#039'src'#039#010+
  '        else'#010+
  '          result := '#039'.source'#039#010+
  '      end'#010+
  '  else'#010+
  '    result := '#039'.'#039' + MakeTargetString(aTarget,ALimit83);'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'function MakeZipSuffix(CPU : TCPU;OS: TOS; aSubTarget : String; ALimit'+
  '83: boole','an) : String;'#010+
  #010+
  'var'#010+
  '  CT : TCompileTarget;'#010+
  #010+
  'begin'#010+
  '  CT.CPU:=CPU;'#010+
  '  CT.OS:=OS;'#010+
  '  CT.Subtarget:=aSubTarget;'#010+
  '  Result:=MakeZipSuffix(CT,aLimit83);'#010+
  'end;'#010+
  #010+
  'function MakeZipSuffix(CPU : TCPU;OS: TOS; aSubTarget : String ='#039#039+
  ' ) : String;'#010+
  #010+
  'var'#010+
  '  CT : ','TCompileTarget;'#010+
  #010+
  'begin'#010+
  '  CT.CPU:=CPU;'#010+
  '  CT.OS:=OS;'#010+
  '  CT.Subtarget:=aSubTarget;'#010+
  '  Result := MakeZipSuffix (CT,(Defaults.BuildOS in AllLimit83fsOses) o'+
  'r (OS in AllLimit83fsOses));'#010+
  'end;'#010+
  #010+
  'Procedure StringToCPUOS(const S : String; Var CPU : TCPU','; Var OS: TO'+
  'S);'#010+
  #010+
  'Var'#010+
  '  P : integer;'#010+
  #010+
  'begin'#010+
  '  P:=Pos('#039'-'#039',S);'#010+
  '  If (P=0) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidTarget,[S]);'#010+
  '  CPU:=StringToCPU(Copy(S,1,P-1));'#010+
  '  OS:=StringToOs(Copy(S,P+1,Length(S)-P));'#010+
  'end;'#010+
  #010+
  #010+
  '// Link each packag','e and unit dependency in L to its item in P, igno'+
  'ring fpdoc targets'#010+
  'Procedure ResolveDependencies(L : TDependencies; P : TNamedCollection)'+
  ';'#010+
  'Var'#010+
  '  I,J : Integer;'#010+
  '  C : TDependency;'#010+
  'begin'#010+
  '  If Assigned(L) then'#010+
  '    For I:=0 to L.Count-1 do'#010+
  '   ','   begin'#010+
  '        C:=L[i];'#010+
  '        if C.DependencyType in [depPackage,depUnit] then'#010+
  '          begin'#010+
  '            J:=P.Count-1;'#010+
  '            While (J>=0) and ((CompareText(TNamedItem(P.Items[J]).Name'+
  ',C.Value)<>0) or'#010+
  '                  ((P.Items[','J] is TTarget) and (TTarget(P.Items[J]).'+
  'TargetType=ttFPDoc))) do'#010+
  '              Dec(J);'#010+
  '            If J<>-1 then'#010+
  '              C.Target:=P.Items[J];'#010+
  '          end;'#010+
  '      end;'#010+
  'end;'#010+
  #010+
  #010+
  '//function AddConditionalStrings(APackage: TPackage; Dest ',': TStrings'+
  '; Src : TConditionalStrings;const aSubTarget : String; ACPU:TCPU;AOS:T'+
  'OS; Const APrefix : String='#039#039') : Integer ;'#010+
  'function AddConditionalStrings(APackage: TPackage; Dest : TStrings; Sr'+
  'c : TConditionalStrings; const aTarget : TCompi','leTarget; Const APref'+
  'ix : String='#039#039') : Integer ;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  C : TConditionalString;'#010+
  '  D : TPackageDictionary;'#010+
  '  S : String;'#010+
  'begin'#010+
  '  Result:=0;'#010+
  '  D := PackageDictionaryClass.Create(nil);'#010+
  '  D.MasterDictionary := APackage.Dictionary;',#010+
  '  try'#010+
  '    D.AddVariable('#039'CPU'#039',CPUToString(aTarget.CPU));'#010+
  '    D.AddVariable('#039'OS'#039',OSToString(aTarget.OS));'#010+
  '    D.AddVariable('#039'SUBTARGET'#039',aTarget.SubTarget);'#010+
  '    For I:=0 to Src.Count-1 do'#010+
  '      begin'#010+
  '        C:=Src[I];'#010+
  '        if (aTarget.CP','U in C.CPUs) and (aTarget.OS in C.OSes) then'#010+
  '          begin'#010+
  '            If (APrefix<>'#039#039') then'#010+
  '              S:=APrefix+C.Value'#010+
  '            else'#010+
  '              S:=C.Value;'#010+
  '            S := D.ReplaceStrings(s);'#010+
  '            if C is TConditiona','lDestString then'#010+
  '              begin'#010+
  '                // If a destination path is given, omit the path of th'+
  'e sourcefile'#010+
  '                if TConditionalDestString(c).DestPath='#039#039' then'#010+
  '                  Dest.values[S] :=  D.ReplaceStrings(Incl','udeTrailin'+
  'gPathDelimiter(TConditionalDestString(c).DestPath))+S'#010+
  '                else'#010+
  '                  Dest.values[S] :=  D.ReplaceStrings(IncludeTrailingP'+
  'athDelimiter(TConditionalDestString(c).DestPath)+APrefix+ExtractFileNa'+
  'me(C.Value));'#010+
  ' ','             end'#010+
  '            else'#010+
  '              Dest.Add(S);'#010+
  '            Inc(Result);'#010+
  '          end;'#010+
  '      end;'#010+
  '  finally'#010+
  '    D.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'function FileListToString(List : TStrings; const APrefix : String) : S'+
  'tring;'#010+
  'Var'#010+
  '  I : integ','er;'#010+
  '  S : String;'#010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  For I:=0 to List.Count-1 do'#010+
  '    begin'#010+
  '      If (I>0) then'#010+
  '        Result:=Result+'#039' '#039';'#010+
  '      S:=APrefix+List[i];'#010+
  '      If (Pos('#039' '#039',S)<>0) then'#010+
  '        S:='#039'"'#039'+S+'#039'"'#039';'#010+
  '      Result:=Result+S;'#010+
  '    end;'#010+
  'end',';'#010+
  #010+
  'Procedure PrependFileListWithString(List : TStrings; const APrefix : S'+
  'tring);'#010+
  'Var'#010+
  '  I : integer;'#010+
  #010+
  'begin'#010+
  '  For I:=0 to List.Count-1 do'#010+
  '    List[i]:=APrefix+List[i];'#010+
  'end;'#010+
  #010+
  #010+
  #010+
  'function FixPath (const APath : String) : String;'#010+
  'begin'#010+
  '  Result ',':= FixPath(APath, False);'#010+
  'end;'#010+
  #010+
  'function FixPath (const APath : String; AIsDir : Boolean) : String;'#010+
  'Var'#010+
  '  P : PChar;'#010+
  'begin'#010+
  '  Result:=APath;'#010+
  '  If (result<>'#039#039') then'#010+
  '    begin'#010+
  '      UniqueString(Result);'#010+
  '      P:=PChar(Result);'#010+
  '      While (P^','<>#0) do'#010+
  '        begin'#010+
  '          If P^ in ['#039'/'#039','#039'\'#039'] then'#010+
  '            P^:=PathDelim;'#010+
  '          Inc(P);'#010+
  '        end;'#010+
  '    end;'#010+
  '  if AIsDir and (Result <> '#039#039') then'#010+
  '    Result := IncludeTrailingPathDelimiter(Result);'#010+
  'end;'#010+
  #010+
  'function IsRelativePat','h(const APath: String): boolean;'#010+
  'begin'#010+
  '  if APath='#039#039' then'#010+
  '    result := true'#010+
  '{$ifdef unix}'#010+
  '  else if APath[1] in AllowDirectorySeparators then'#010+
  '    result := false'#010+
  '{$else}'#010+
  '  else if ExtractFileDrive(APath)<>'#039#039' then'#010+
  '    result := false'#010+
  '{$endi','f}'#010+
  '  else'#010+
  '    result := true;'#010+
  'end;'#010+
  #010+
  'procedure ChangeDir(const APath : String);'#010+
  'begin'#010+
  '  if Not SetCurrentDir(APath) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrChangeDirFailed,[APath]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure SearchFiles(AFileName, ASearchPathPre','fix: string; Recursiv'+
  'e: boolean; var List: TStrings);'#010+
  #010+
  '  procedure AddRecursiveFiles(const SearchDir, FileMask: string; Recur'+
  'sive: boolean);'#010+
  '  var'#010+
  '    Info : TSearchRec;'#010+
  '  begin'#010+
  '    if assigned(Installer) then'#010+
  '      Installer.Log(VlDebug,Fo','rmat(SDbgSearchingDir,[SearchDir]));'#010+
  '    if FindFirst(SearchDir+AllFilesMask,faAnyFile and faDirectory,Info'+
  ')=0 then'#010+
  '    begin'#010+
  '      repeat'#010+
  '          if ((Info.Attr and faDirectory) = faDirectory) and (Info.Nam'+
  'e <> '#039'.'#039') and (Info.Name <> '#039'..',#039') and (Recursive) then'+
  #010+
  '            AddRecursiveFiles(SearchDir + Info.Name + PathDelim, FileM'+
  'ask, Recursive);'#010+
  '          if ((Info.Attr and faDirectory) <> faDirectory) and IsWild(I'+
  'nfo.Name, FileMask, FileNameCaseSensitive) then'#010+
  '           ',' List.Add(SearchDir + Info.Name);'#010+
  '      until FindNext(Info)<>0;'#010+
  '    end;'#010+
  '    {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.FindClose(Info);'#010+
  '  end;'#010+
  #010+
  'var'#010+
  '  CurrDir,'#010+
  '  BasePath: string;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  if IsRelativePath(AFileName) and',' (ASearchPathPrefix<>'#039#039') the'+
  'n'#010+
  '    AFileName := IncludeTrailingPathDelimiter(ASearchPathPrefix) + AFi'+
  'leName;'#010+
  #010+
  '  BasePath := ExtractFilePath(ExpandFileName(AFileName));'#010+
  #010+
  '  AddRecursiveFiles(BasePath, ExtractFileName(AFileName), Recursive);'#010+
  #010+
  ' ',' CurrDir:=GetCurrentDir;'#010+
  '  for i := 0 to Pred(List.Count) do'#010+
  '    List[i] := ExtractRelativepath(IncludeTrailingPathDelimiter(Includ'+
  'eTrailingPathDelimiter(CurrDir)+ASearchPathPrefix), List[i]);'#010+
  'end;'#010+
  #010+
  'Const'#010+
  '  WhiteSpace = [#9,#10,#13,'#039' '#039'];'#010+
  '  ','QuoteChars = ['#039#039#039#039','#039'"'#039'];'#010+
  #010+
  'procedure SplitCommand(const Cmd : String; out Exe, Options : String);'+
  #010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  '  InQuote : Boolean;'#010+
  '  LastQuote : AnsiChar;'#010+
  '  S : String;'#010+
  #010+
  'begin'#010+
  '  S:=Trim(Cmd);'#010+
  '  InQuote:=False;'#010+
  '  LastQuote:=#0;'#010+
  '  I:=1',';'#010+
  '  While (I<=Length(S)) and (Inquote or not (S[I] in whiteSpace)) do'#010+
  '    begin'#010+
  '    If S[i] in QuoteChars then'#010+
  '      begin'#010+
  '      InQuote:=Not (S[i]=LastQuote);'#010+
  '      If InQuote then'#010+
  '         LastQuote:=S[i]'#010+
  '       else'#010+
  '         LastQuote:=#','0;'#010+
  '      end;'#010+
  '    Inc(I);'#010+
  '    end;'#010+
  '  Exe:=Copy(S,1,I-1);'#010+
  '  Delete(S,1,I);'#010+
  '  Options:=Trim(S);'#010+
  'end;'#010+
  #010+
  'procedure AddCustomFpmakeCommandlineOption(const ACommandLineOption, H'+
  'elpMessage : string);'#010+
  'begin'#010+
  '  if not assigned(CustomFpmakeCommandlineO','ptions) then'#010+
  '    CustomFpmakeCommandlineOptions := TUnsortedCompilerOptionsStringLi'+
  'st.Create;'#010+
  '  CustomFpmakeCommandlineOptions.Values[ACommandLineOption]:=HelpMessa'+
  'ge;'#010+
  'end;'#010+
  #010+
  'function GetCustomFpmakeCommandlineOptionValue(const ACommandLineO','pt'+
  'ion: string): string;'#010+
  'begin'#010+
  '  if not assigned(CustomFpMakeCommandlineValues) then'#010+
  '    result := '#039#039#010+
  '  else'#010+
  '    result := CustomFpMakeCommandlineValues.Values[ACommandLineOption]'+
  ';'#010+
  'end;'#010+
  #010+
  'function AddProgramExtension(const ExecutableName: str','ing; AOS : TOS'+
  '): string;'#010+
  'begin'#010+
  '  if AOS in [Go32v2,Win32,Win64,Wince,OS2,EMX,Watcom] then'#010+
  '    Result:=ExecutableName+ExeExt'#010+
  '  else if AOS in [wasip1,wasip1threads,wasip2] then'#010+
  '    Result:=ExecutableName+WasiExeExt'#010+
  '  else'#010+
  '    Result:=Executa','bleName;'#010+
  'end;'#010+
  #010+
  'function AddLibraryExtension(const LibraryName: string; AOS : TOS): st'+
  'ring;'#010+
  'begin'#010+
  '  if AOS in [Go32v2,Win32,Win64,Wince,OS2,EMX,Watcom] then'#010+
  '    Result:=LibraryName+DLLExt'#010+
  '  else if aOS in [darwin,macosclassic,iphonesim,ios] ','then'#010+
  '    Result:=LibraryName+DyLibExt'#010+
  '  else if aOS = Aix then'#010+
  '    Result:=LibraryName+AIXSharedLibExt'#010+
  '  else if AOS in [wasip1,wasip1threads,wasip2] then'#010+
  '    Result:=LibraryName+WasiSharedLibExt'#010+
  '  else'#010+
  '    Result:=LibraryName+SharedLibExt;',#010+
  'end;'#010+
  #010+
  'function GetImportLibraryFilename(const UnitName: string; AOS: TOS): s'+
  'tring;'#010+
  'begin'#010+
  '  if AOS in [go32v2,watcom] then'#010+
  '    Result := '#039'libimp'#039'+UnitName'#010+
  '  else if AOS in [os2,emx] then'#010+
  '    Result := UnitName'#010+
  '  else if AOS in [netware,netw','libc,macosclassic] then'#010+
  '    Result := '#039'lib'#039'+UnitName'#010+
  '  else'#010+
  '    Result := '#039'libimp'#039'+UnitName;'#010+
  '  Result := Result + LibExt;'#010+
  'end;'#010+
  #010+
  'Function OptionListToString(L : TStrings) : String;'#010+
  #010+
  'var'#010+
  '  I : Integer;'#010+
  '  S : String;'#010+
  #010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  For',' I:=0 to L.Count-1 do'#010+
  '    begin'#010+
  '    If (Result<>'#039#039') then'#010+
  '      Result:=Result+'#039' '#039';'#010+
  '    S:=L[I];'#010+
  '    If (Pos('#039' '#039',S)<>0) or (S='#039#039') then'#010+
  '      Result:= Result + '#039'"'#039'+S+'#039'"'#039#010+
  '    else'#010+
  '      Result:= Result + S;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'Function OptionsToStr','ingList(S : String) : TStrings;'#010+
  #010+
  'Var'#010+
  '  P : Integer;'#010+
  #010+
  'begin'#010+
  '  Result:=Nil;'#010+
  '  If (S='#039#039') then'#010+
  '    Exit;'#010+
  '  Result:=TUnsortedCompilerOptionsStringList.Create;'#010+
  '  Repeat'#010+
  '    P:=Pos('#039' '#039',S);'#010+
  '    If P=0 then'#010+
  '      P:=Length(S)+1;'#010+
  '    Result.Add(Copy(','S,1,P-1));'#010+
  '    Delete(S,1,P);'#010+
  '    S:=Trim(S);'#010+
  '  Until Length(S)=0;'#010+
  '  If Result.Count=0 then'#010+
  '    FreeAndNil(Result);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '{ function GetCompilerInfo'#010+
  '  used both for gcc and Free Pascal compiler'#010+
  '  returns stdout ou','tput of Acompiler with AOptions parameters'#010+
  '  If ReadStdErr is True, return stderr output if stdout is empty'#010+
  '  If EmptyIfStdErr, return empty string if stderr output is not empty '+
  '}'#010+
  #010+
  'function GetCompilerInfo(const ACompiler:string; Args : TSt','rings; Re'+
  'adStdErr: boolean;EmptyIfStdErr : boolean):ansistring;'#010+
  #010+
  'const'#010+
  '  BufSize = 1024;'#010+
  'Type'#010+
  '  TBufType = array [0..BufSize - 1] of AnsiChar;'#010+
  #010+
  'var'#010+
  '  Proc: TProcess;'#010+
  '  Buf: TBufType;'#010+
  '  ErrorBuf: TBufType;'#010+
  '  Count, ErrorCount: longint;'#010+
  #010+
  #010+
  'beg','in'#010+
  '  buf:=Default(TBufType);'#010+
  '  ErrorBuf:=Default(TBufType);'#010+
  '  Proc:=TProcess.Create(Nil);'#010+
  '  try'#010+
  '    Proc.Executable:=ACompiler;'#010+
  '    Proc.Parameters:=Args;'#010+
  '    Proc.Options:=[poUsePipes,poWaitOnExit];'#010+
  '    Proc.execute;'#010+
  '    Count:=Proc.output','.read(buf,BufSize);'#010+
  '    if (count=0) and ReadStdErr then'#010+
  '      Count:=Proc.Stderr.read(buf,BufSize)'#010+
  '    else if EmptyIfStdErr then'#010+
  '      begin'#010+
  '        ErrorCount:=Proc.StdErr.read(ErrorBuf,BufSize);'#010+
  '        if (ErrorCount>0) then'#010+
  '          ','begin'#010+
  '            Result:='#039#039';'#010+
  '            exit;'#010+
  '          end;'#010+
  '      end;'#010+
  '  finally'#010+
  '    Proc.Free;'#010+
  '  end;'#010+
  '  SetLength(Result,Count);'#010+
  '  if Count>0 then'#010+
  '    Move(Buf,Result[1],Count);'#010+
  'end;'#010+
  #010+
  '// Convenience function'#010+
  #010+
  'function GetCompilerInfo(co','nst ACompiler:string; aOptions : Array of'+
  ' String; ReadStdErr: boolean;EmptyIfStdErr : boolean):ansistring;'#010+
  #010+
  'Var'#010+
  '  Args : TStrings;'#010+
  #010+
  'begin'#010+
  '  Args:=TStringList.Create;'#010+
  '  try'#010+
  '    Args.AddStrings(aOptions);'#010+
  '    Result:=GetCompilerInfo(aCompiler',',Args,ReadStdErr,EmptyIfStdErr)'+
  ';'#010+
  '  finally'#010+
  '    Args.Free;'#010+
  '  end;'#010+
  #010+
  'end;'#010+
  #010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  #010+
  'function GetDefaultLibGCCDir(CPU : TCPU;OS: TOS; out ErrorMessage: str'+
  'ing): string;'#010+
  #010+
  'var'#010+
  '  CrossPrefix: Ansistring;'#010+
  '  UseBinutilsPrefix: bool','ean;'#010+
  '  SourceOS : TOS;'#010+
  '  SourceCPU : TCPU;'#010+
  #010+
  '  function Get4thWord(const AString: Ansistring): Ansistring;'#010+
  '  var p: PAnsiChar;'#010+
  '      spacecount: integer;'#010+
  '      StartWord: PAnsiChar;'#010+
  '  begin'#010+
  '    result:='#039#039';'#010+
  '    if length(AString)>6 then'#010+
  '     ',' begin'#010+
  '      p := @AString[1];'#010+
  '      spacecount:=0;'#010+
  '      StartWord:=nil;'#010+
  '      while (not (p^ in [#0,#10,#13])) and ((p^<>'#039' '#039') or (Star'+
  'tWord=nil)) do'#010+
  '        begin'#010+
  '        if p^='#039' '#039' then'#010+
  '          begin'#010+
  '          inc(spacecount);'#010+
  '         ',' if spacecount=3 then StartWord:=p+1;'#010+
  '          end;'#010+
  '        inc(p);'#010+
  '        end;'#010+
  '      if StartWord<>nil then'#010+
  '        begin'#010+
  '        SetLength(result,p-StartWord);'#010+
  '        move(StartWord^,result[1],p-StartWord);'#010+
  '        end'#010+
  '      else'#010+
  '     ','   result := '#039#039';'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  '  function GetGccDirArch(const ACpuType : AnsiString; GCCParams: TStri'+
  'ngs) : Ansistring;'#010+
  #010+
  '  var ExecResult: Ansistring;'#010+
  '      libgccFilename: Ansistring;'#010+
  '      GccExecutable: string;'#010+
  '      Parms : TString','s;'#010+
  #010+
  '  begin'#010+
  '    result := '#039#039';'#010+
  '    Parms:=TStringList.Create;'#010+
  '    try'#010+
  '    GccExecutable := ExeSearch(AddProgramExtension(CrossPrefix+'#039'gc'+
  'c'#039', OS),{$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.GetEnviron'+
  'mentVariable('#039'PATH'#039'));'#010+
  '    if not(FileE','xists(GccExecutable)) then'#010+
  '      GccExecutable := ExeSearch(AddProgramExtension(CrossPrefix+'#039'g'+
  'nu-gcc'#039', OS),{$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.GetEn'+
  'vironmentVariable('#039'PATH'#039'));'#010+
  '    if not(FileExists(GccExecutable)) and (CrossPref','ix='#039'i386-lin'+
  'ux-'#039') then'#010+
  '      GccExecutable := ExeSearch(AddProgramExtension('#039'i686-linux-g'+
  'nu-gcc'#039', OS),{$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Sysutils.GetEn'+
  'vironmentVariable('#039'PATH'#039'));'#010+
  '    if FileExists(GccExecutable) then'#010+
  '      begin'#010+
  '{$ifdef',' HAS_UNIT_PROCESS}'#010+
  '      Parms.Add('#039'-v'#039');'#010+
  '      Parms.AddStrings(GCCParams);'#010+
  '      ExecResult:=GetCompilerInfo(GccExecutable,Parms, True, True);'#010+
  '      libgccFilename:=Get4thWord(ExecResult);'#010+
  '      // Use IsRelativePath to check if the 4th w','ord is an (absolute'+
  ') path.'#010+
  '      // This depends on the language settings. In English the 4th wor'+
  'd is'#010+
  '      // empty, if this particular gcc version does not return the lib'+
  'gcc-'#010+
  '      // filename on -v. But in other languages (e.g. Dutch) it',' may '+
  'be another'#010+
  '      // word.'#010+
  '      if IsRelativePath(libgccFilename) then'#010+
  '        libgccFilename:='#039#039';'#010+
  '      if libgccFilename='#039#039' then'#010+
  '        begin'#010+
  '        Parms[0]:='#039'--print-libgcc-file-name'#039';'#010+
  '        libgccFilename:=GetCompilerInfo(GccE','xecutable,Parms, False, '+
  'True);'#010+
  '        end;'#010+
  '      result := ExtractFileDir(libgccFilename);'#010+
  '{$else HAS_UNIT_PROCESS}'#010+
  '      ErrorMessage := SWarnNoFCLProcessSupport;'#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  '      end'#010+
  '    else if UseBinutilsPrefix then'#010+
  '    ','  ErrorMessage := SWarncrossgccNotFound'#010+
  '    else'#010+
  '      ErrorMessage := SWarngccNotFound;'#010+
  #010+
  '    finally'#010+
  '      Parms.Free;'#010+
  '    end;'#010+
  '  end;'#010+
  #010+
  '  function GetGccDirArch(const ACpuType : String; GCCParams: Array of '+
  'String) : string;'#010+
  '  var'#010+
  '    Args ',': TStrings;'#010+
  #010+
  '  begin'#010+
  '    Args:=TStringList.Create;'#010+
  '    try'#010+
  '      Args.AddStrings(GCCParams);'#010+
  '      Result:=GetGCCDirArch(aCPUType,Args);'#010+
  '    finally'#010+
  '      Args.Free;'#010+
  '    end;'#010+
  '  end;'#010+
  #010+
  #010+
  'begin'#010+
  '  result := '#039#039';'#010+
  '  ErrorMessage:='#039#039';'#010+
  '  if assigned(','Defaults) then'#010+
  '    begin'#010+
  '      SourceOS:=Defaults.SourceOS;'#010+
  '      SourceCPU:=Defaults.SourceCPU;'#010+
  '    end'#010+
  '  else'#010+
  '    begin'#010+
  '      SourceOS:=StringToOS({$I %FPCTARGETOS%});'#010+
  '      SourceCPU:=StringToCPU({$I %FPCTARGETCPU%});'#010+
  '    end;'#010+
  #010+
  '  UseBinu','tilsPrefix:=(SourceOS<>OS);'#010+
  '  if (SourceCPU<>CPU) then'#010+
  '    begin'#010+
  '      { we need to accept 32<->64 conversion }'#010+
  '      { expect for OpenBSD which does not allow this }'#010+
  '      if not('#010+
  '         ((SourceCPU=aarch64) and (CPU=arm)) or'#010+
  '         ((','SourceCPU=powerpc64) and (CPU=powerpc)) or'#010+
  '         ((SourceCPU=x86_64) and (CPU=i386)) or'#010+
  '         ((SourceCPU=riscv64) and (CPU=riscv32)) or'#010+
  '         ((SourceCPU=sparc64) and (CPU=sparc)) or'#010+
  '         ((CPU=aarch64) and (SourceCPU=arm)) or',#010+
  '         ((CPU=powerpc64) and (SourceCPU=powerpc)) or'#010+
  '         ((CPU=x86_64) and (SourceCPU=i386)) or'#010+
  '         ((CPU=riscv64) and (SourceCPU=riscv32)) or'#010+
  '         ((CPU=sparc64) and (SourceCPU=sparc))'#010+
  '         ) or (SourceOS=openbsd) then'#010,
  '        UseBinutilsPrefix:=true;'#010+
  '    end;'#010+
  '  if {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.GetEnvironmentVar'+
  'iable('#039'BINUTILSPREFIX'#039')<>'#039#039' then'#010+
  '    CrossPrefix:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.GetEn'+
  'vironmentVariable('#039'BINUTI','LSPREFIX'#039')'#010+
  '  else if not UseBinutilsPrefix then'#010+
  '    CrossPrefix:='#039#039#010+
  '  else'#010+
  '    CrossPrefix:=CPUToString(CPU)+'#039'-'#039'+OSToString(OS)+'#039'-'#039+
  ';'#010+
  '  if OS in [freebsd, openbsd, dragonfly] then'#010+
  '    begin'#010+
  '      if CrossPrefix='#039#039' then'#010+
  '        result := '#039'/us','r/local/lib'#039#010+
  '    end'#010+
  '  else if OS = netbsd then'#010+
  '    begin'#010+
  '      if CrossPrefix='#039#039' then'#010+
  '        result := '#039'/usr/pkg/lib'#039#010+
  '    end'#010+
  '  else if OS = linux then'#010+
  '    case CPU of'#010+
  '      i386:         result := GetGccDirArch('#039'cpui386'#039',['#039'-m'+
  '32'#039']);'#010+
  '     ',' x86_64:       result := GetGccDirArch('#039'cpux86_64'#039',['#039+
  '-m64'#039']);'#010+
  '      powerpc:      result := GetGccDirArch('#039'cpupowerpc'#039',['#039+
  '-m32'#039']);'#010+
  '      powerpc64:    result := GetGccDirArch('#039'cpupowerpc64'#039',['#039+
  '-m64'#039']);'#010+
  '      arm:          result := GetGccDirA','rch('#039'cpuarm'#039',['#039'-'+
  'marm'#039','#039'-march=armv2'#039']);'#010+
  '      aarch64:      result := GetGccDirArch('#039'cpuaarch64'#039',['#039+
  '-march=aarch64'#039','#039'-mcmodel=large'#039']);'#010+
  '      m68k:         result := GetGccDirArch('#039'cpum68k'#039',['#039'-m'+
  'arch=68020'#039']);'#010+
  '      mips:         result := Ge','tGccDirArch('#039'cpumips'#039',['#039'-'+
  'mips32'#039','#039'-EB'#039','#039'-mabi=32'#039']);'#010+
  '      mipsel:       result := GetGccDirArch('#039'cpumipsel'#039',['#039'-'+
  'mips32'#039','#039'-EL'#039','#039'-mabi=32'#039']);'#010+
  '      mips64el:     result := GetGccDirArch('#039'cpumipsel'#039',['#039'-'+
  'mips64'#039','#039'-EL'#039','#039'-mabi=64'#039']);'#010+
  '      mips64:','       result := GetGccDirArch('#039'cpumipsel'#039',['#039+
  '-mips64'#039','#039'-EB'#039','#039'-mabi=64'#039']);'#010+
  '      riscv32:      result := GetGccDirArch('#039'cpuriscv32'#039',['#039+
  '-march=rv32imafdc'#039']);'#010+
  '      riscv64:      result := GetGccDirArch('#039'cpuriscv64'#039',['#039+
  '-march=rv64imafdc'#039']);'#010+
  '    ','  sparc:        result := GetGccDirArch('#039'cpusparc'#039',['#039+
  '-m32'#039']);'#010+
  '      sparc64:      result := GetGccDirArch('#039'cpusparc64'#039',['#039+
  '-m64'#039']);'#010+
  '      xtensa:       result := GetGccDirArch('#039'cpuxtensa'#039',[]);'#010+
  '      loongarch64:  result := GetGccDirArch('#039'cpul','oongarch64'#039','+
  '[]);'#010+
  '    else'#010+
  '      ; // avoid compiler warning'#010+
  '    end {case}'#010+
  '  else if OS = darwin then'#010+
  '    case CPU of'#010+
  '      i386:     result := GetGccDirArch('#039'cpui386'#039',['#039'-arch'#039+
  ', '#039'i386'#039']);'#010+
  '      x86_64:   result := GetGccDirArch('#039'cpux86_64'#039',','['#039'-a'+
  'rch'#039','#039'x86_64'#039']);'#010+
  '      powerpc:  result := GetGccDirArch('#039'cpupowerpc'#039',['#039'-ar'+
  'ch'#039','#039'ppc'#039']);'#010+
  '      powerpc64:result := GetGccDirArch('#039'cpupowerpc64'#039',['#039'-'+
  'arch'#039','#039'ppc64'#039']);'#010+
  '      { this target uses clang }'#010+
  '      aarch64:  result := '#039#039#010+
  '    else'#010+
  '  ','    ; // Avoid compiler warning'#010+
  '    end; {case}'#010+
  'end;'#010+
  #010+
  'function GetPluginManager: TfpmPluginManager;'#010+
  'begin'#010+
  '  if not assigned(GPluginManager) then'#010+
  '    GPluginManager := TfpmPluginManager.Create;'#010+
  '  Result := GPluginManager;'#010+
  'end;'#010+
  #010+
  '{ TConditiona','lDestString }'#010+
  #010+
  'procedure TConditionalDestString.Assign(aSource: TPersistent);'#010+
  #010+
  'Var'#010+
  '  CDS : TConditionalDestString absolute aSource;'#010+
  #010+
  'begin'#010+
  '  if aSource is TConditionalDestString then'#010+
  '    begin'#010+
  '    FDestPath:=CDS.DestPath;'#010+
  '    end;'#010+
  '  inherit','ed Assign(aSource);'#010+
  'end;'#010+
  #010+
  '{ TDependencyEnumerator }'#010+
  #010+
  'function TDependencyEnumerator.GetCurrent: TDependency;'#010+
  'begin'#010+
  '  Result:=TDependency(inherited GetCurrent);'#010+
  'end;'#010+
  #010+
  '{ TCompileTarget }'#010+
  #010+
  'function TCompileTarget.GetAsString: String;'#010+
  'begin'#010+
  '  R','esult:=ToString(False);'#010+
  'end;'#010+
  #010+
  'procedure TCompileTarget.SetAsString(const aValue: String);'#010+
  #010+
  'var'#010+
  '  P :  Integer;'#010+
  '  aTarget : string;'#010+
  #010+
  'begin'#010+
  '  aTarget:=aValue;'#010+
  '  P:=Pos('#039'-'#039',aTarget);'#010+
  '  If (P=0) then'#010+
  '    OS:=StringToOS(aTarget)'#010+
  '  else'#010+
  '    begin',#010+
  '    CPU:=StringToCPU(System.Copy(aTarget,1,P-1));'#010+
  '    Delete(aTarget,1,P);'#010+
  '    P:=Pos('#039'-'#039',aTarget);'#010+
  '    if P=0 then'#010+
  '      OS:=StringToOS(aTarget)'#010+
  '    else'#010+
  '      begin'#010+
  '      OS:=StringToOS(System.Copy(aTarget,1,P-1));'#010+
  '      Delete(aTarget,1',',P);'#010+
  '      SubTarget:=aTarget;'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function TCompileTarget.Equals(const aValue: TCompileTarget): Boolean;'+
  #010+
  'begin'#010+
  '  Result:=(OS=aValue.OS) and (CPU=aValue.CPU) and (Subtarget=aValue.Su'+
  'bTarget);'#010+
  'end;'#010+
  #010+
  'function TCompileTar','get.ToString(aLimit83: Boolean): String;'#010+
  'begin'#010+
  '  if ALimit83 then'#010+
  '    Result := OSToString(OS)'#010+
  '  else'#010+
  '    begin'#010+
  '    Result:=CPUToString(CPU)+'#039'-'#039'+OSToString(OS);'#010+
  '    if SubTarget<>'#039#039' then'#010+
  '      Result:=Result+'#039'-'#039'+LowerCase(SubTarget);'#010+
  '    en','d;'#010+
  'end;'#010+
  #010+
  '{ TPackageEnumerator }'#010+
  #010+
  'function TPackageEnumerator.GetCurrent: TPackage;'#010+
  'begin'#010+
  '  Result:=TPackage(Inherited GetCurrent);'#010+
  'end;'#010+
  #010+
  '{ TCommandEnumerator }'#010+
  #010+
  'function TCommandEnumerator.GetCurrent: TCommand;'#010+
  'begin'#010+
  '  Result:= TCommand(Inh','erited GetCurrent);'#010+
  'end;'#010+
  #010+
  '{ TNotifyEventEnumerator }'#010+
  #010+
  'function TNotifyEventEnumerator.GetCurrent: TNotifyEventItem;'#010+
  'begin'#010+
  '  Result:= TNotifyEventItem( Inherited GetCurrent);'#010+
  'end;'#010+
  #010+
  '{ TPackageVariantEnumerator }'#010+
  #010+
  'function TPackageVariantEnume','rator.GetCurrent: TPackageVariant;'#010+
  'begin'#010+
  '  Result:= TPackageVariant( Inherited GetCurrent);'#010+
  'end;'#010+
  #010+
  '{ TTargetEnumerator }'#010+
  #010+
  'function TTargetEnumerator.GetCurrent: TTarget;'#010+
  'begin'#010+
  '  Result:=TTarget(Inherited GetCurrent);'#010+
  'end;'#010+
  #010+
  '{ TSourceEnumerato','r }'#010+
  #010+
  'function TSourceEnumerator.GetCurrent: TSource;'#010+
  'begin'#010+
  '  Result:=TSource(Inherited GetCurrent);'#010+
  'end;'#010+
  #010+
  #010+
  '{ TResourceFiles }'#010+
  #010+
  'procedure TResourceFiles.GetInstallFiles(AList: TStrings; const APrefi'+
  'xU, APrefixB: String; ACPU: TCPU; AOS: TOS)',';'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  R : TResourceFile;'#010+
  'begin'#010+
  '  For I:=0 to Count-1 do'#010+
  '    begin'#010+
  '    R:=Tobject(Items[I]) as TResourceFile;'#010+
  '    if (ACPU in R.CPUs) and (AOS in R.OSes) then'#010+
  '      AList.Add(ConcatPaths([APrefixU, R.Value]));'#010+
  '    end;'#010+
  'end;'#010,
  #010+
  '{ TfpmResolvePackagePathsPlugin }'#010+
  #010+
  'procedure TfpmResolvePackagePathsPlugin.ResolveUnitConfigFilenameForBa'+
  'sePath('#010+
  '  ABuildEngine: TBuildEngine; APackage: TPackage; ABasePath: string;'#010+
  '  out AContinue: Boolean);'#010+
  'var'#010+
  '  IsPackageSourceLocation:',' boolean;'#010+
  '  ASubDir: string;'#010+
  '  AnUnitConfigFilename: string;'#010+
  '  PackageBaseDir: string;'#010+
  'begin'#010+
  '  if APackage.State=tsNotFound then'#010+
  '    // When the state is tsNotFound, the package is not part of this f'+
  'pmake, and only the package-name is known','.'#010+
  '    // In this case search for the package-name.'#010+
  '    // This is not right for packages where the package-name and direc'+
  'tory name of the source-files are'#010+
  '    // not the same. We don'#039't have a better option, though.'#010+
  '    ASubDir:=APackage.Nam','e'#010+
  '  else'#010+
  '    ASubDir:=APackage.Directory;'#010+
  #010+
  '  IsPackageSourceLocation:=ABuildEngine.SysFileExists(IncludeTrailingP'+
  'athDelimiter(IncludeTrailingPathDelimiter(ABasePath)+ASubDir)+FPMakePP'+
  'File);'#010+
  '  if IsPackageSourceLocation then'#010+
  '    begin'#010+
  '      ','PackageBaseDir:=IncludeTrailingPathDelimiter(IncludeTrailingPa'+
  'thDelimiter(ABasePath)+ASubDir);'#010+
  '      AnUnitConfigFileName:=PackageBaseDir+APackage.GetUnitConfigOutpu'+
  'tFilename(Defaults.CompileTarget);'#010+
  '      PackageBaseDir:=IncludeTrailingPat','hDelimiter(PackageBaseDir+AP'+
  'ackage.GetUnitsOutputDir(Defaults.CompileTarget));'#010+
  '    end'#010+
  '  else'#010+
  '    begin'#010+
  '      PackageBaseDir:=IncludeTrailingPathDelimiter(ABasePath);'#010+
  '      AnUnitConfigFileName:=IncludeTrailingPathDelimiter(ABuildEngine.'+
  'Get','UnitConfigFilesInstallDir(ABasePath))+APackage.Name+FpmkExt;'#010+
  '      PackageBaseDir:=IncludeTrailingPathDelimiter(IncludeTrailingPath'+
  'Delimiter(ABasePath)+APackage.GetUnitsOutputDir(Defaults.CompileTarget'+
  '))+APackage.Name;'#010+
  '    end;'#010+
  #010+
  '  if (Packa','geBaseDir<>'#039#039') and ABuildEngine.SysDirectoryExists('+
  'PackageBaseDir) then'#010+
  '    begin'#010+
  '      AnUnitConfigFilename:=APackage.Dictionary.ReplaceStrings(AnUnitC'+
  'onfigFilename);'#010+
  '      if ABuildEngine.SysFileExists(AnUnitConfigFilename) then'#010+
  '        A','Package.UnitConfigFileName:=AnUnitConfigFilename'#010+
  '      else'#010+
  '        begin'#010+
  '          // To avoid that directories which do not contain installed '+
  'packages'#010+
  '          // check that there is an unit-configfile, or Packages.fpc f'+
  'ile. (The'#010+
  '       ','   // latter to detect packages compiled using old style Make'+
  'file'#039's, like'#010+
  '          // the rtl). These directories which are not packages may ex'+
  'ist in'#010+
  '          // Lazarus-source-repositories.'#010+
  '          if not ABuildEngine.SysFileExists(Con','catPaths([PackageBase'+
  'Dir, PackageFpcFile])) then'#010+
  '            begin'#010+
  '              if IsPackageSourceLocation then'#010+
  '                begin'#010+
  '                // There is no way that I know of to confirm that a di'+
  'rectory'#010+
  '                // contains',' the compiled sources of the rtl. So we n'+
  'eed a'#010+
  '                // workaround. This test is skipped in case of the rtl'+
  '.'#010+
  '                if APackage.Name<>'#039'rtl'#039' then'#010+
  '                  begin'#010+
  '                  Installer.Log(vlInfo,Format(SInfoS','rcDirectoryNoPkg'+
  ',[PackageBaseDir, APackage.Name]));'#010+
  '                  Exit;'#010+
  '                  end;'#010+
  '                end'#010+
  '              else'#010+
  '                begin'#010+
  '                Installer.Log(vlInfo,Format(SInfoDirectoryNoPackage,[P'+
  'ackageBase','Dir, APackage.Name]));'#010+
  '                Exit;'#010+
  '                end;'#010+
  '            end;'#010+
  '        end;'#010+
  #010+
  '      APackage.UnitDir:=PackageBaseDir;'#010+
  '      AContinue := False;'#010+
  #010+
  '      if IsPackageSourceLocation then'#010+
  '        // Set the state to tsNoCompil','e and not tsCompiled. Because '+
  'packages'#010+
  '        // in the tsCompiled state trigger a rebuild of packages that '+
  'depend'#010+
  '        // on it.'#010+
  '        APackage.FTargetState:=tsNoCompile'#010+
  '      else if not (APackage.FTargetState in [tsCompiled, tsNoCo','mpile'+
  ']) then'#010+
  '        APackage.FTargetState:=tsInstalled;'#010+
  '    end'#010+
  '  else'#010+
  '    AContinue := True;'#010+
  'end;'#010+
  #010+
  'procedure TfpmResolvePackagePathsPlugin.BeforeResolvePackagePath(ABuil'+
  'dEngine: TBuildEngine;'#010+
  '  APackage: TPackage; out AContinue: Boolean);',#010+
  'begin'#010+
  '  if (APackage.State in [tsCompiled, tsNoCompile, tsInstalled]) then'#010+
  '    ResolveUnitConfigFilenameForBasePath(ABuildEngine, APackage, ABuil'+
  'dEngine.StartDir, AContinue)'#010+
  '  else'#010+
  '    AContinue := True;'#010+
  'end;'#010+
  #010+
  'procedure TfpmResolvePackageP','athsPlugin.ResolvePackagePath(ABuildEng'+
  'ine: TBuildEngine;'#010+
  '  APackage: TPackage; SearchPath: string; out AContinue: Boolean);'#010+
  'begin'#010+
  '  ResolveUnitConfigFilenameForBasePath(ABuildEngine, APackage, SearchP'+
  'ath, AContinue)'#010+
  'end;'#010+
  #010+
  '{ TfpmPlugin }'#010+
  #010+
  'f','unction TfpmPlugin.GetName: string;'#010+
  'begin'#010+
  '  Result := ClassName;'#010+
  'end;'#010+
  #010+
  'procedure TfpmPlugin.BeforeResolvePackagePath(ABuildEngine: TBuildEngi'+
  'ne; APackage: TPackage;'#010+
  '  out AContinue: Boolean);'#010+
  'begin'#010+
  '  AContinue := True;'#010+
  'end;'#010+
  #010+
  'procedure TfpmP','lugin.ResolvePackagePath(ABuildEngine: TBuildEngine; '+
  'APackage: TPackage;'#010+
  '  SearchDirectory: string; out AContinue: Boolean);'#010+
  'begin'#010+
  '  AContinue := True;'#010+
  'end;'#010+
  #010+
  'procedure TfpmPlugin.AfterResolvePackagePath(ABuildEngine: TBuildEngin'+
  'e; APackage:',' TPackage;'#010+
  '  out AContinue: Boolean);'#010+
  'begin'#010+
  '  AContinue := True;'#010+
  'end;'#010+
  #010+
  '{ TfpmPluginManager }'#010+
  #010+
  'destructor TfpmPluginManager.Destroy;'#010+
  'var'#010+
  '  i: Integer;'#010+
  'begin'#010+
  '  for i := 0 to High(FPlugins) do'#010+
  '    FPlugins[i].Free;'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  'p','rocedure TfpmPluginManager.RegisterPlugin(APlugin: TfpmPluginClass)'+
  ';'#010+
  'begin'#010+
  '  SetLength(FPlugins, Length(FPlugins)+1);'#010+
  '  FPlugins[high(FPlugins)] := APlugin.Create;'#010+
  'end;'#010+
  #010+
  'procedure TfpmPluginManager.BeforeResolvePackagePath(ABuildEngine: TBu',
  'ildEngine; APackage: TPackage;'#010+
  '  out AContinue: Boolean);'#010+
  'var'#010+
  '  i: Integer;'#010+
  'begin'#010+
  '  for i := 0 to high(FPlugins) do'#010+
  '    begin'#010+
  '      FPlugins[i].BeforeResolvePackagePath(ABuildEngine, APackage, ACo'+
  'ntinue);'#010+
  '      if not AContinue then'#010+
  '       ',' Exit;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TfpmPluginManager.ResolvePackagePath(ABuildEngine: TBuildEng'+
  'ine; APackage: TPackage;'#010+
  '  SearchPath: string; out AContinue: Boolean);'#010+
  'var'#010+
  '  i: Integer;'#010+
  'begin'#010+
  '  for i := 0 to high(FPlugins) do'#010+
  '    begin'#010+
  '      FP','lugins[i].ResolvePackagePath(ABuildEngine, APackage, SearchP'+
  'ath, AContinue);'#010+
  '      if not AContinue then'#010+
  '        Exit;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TfpmPluginManager.AfterResolvePackagePath(ABuildEngine: TBui'+
  'ldEngine; APackage: TPackage;'#010+
  '  out ','AContinue: Boolean);'#010+
  'var'#010+
  '  i: Integer;'#010+
  'begin'#010+
  '  for i := 0 to high(FPlugins) do'#010+
  '    begin'#010+
  '      FPlugins[i].AfterResolvePackagePath(ABuildEngine, APackage, ACon'+
  'tinue);'#010+
  '      if not AContinue then'#010+
  '        Exit;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'constructor TPac','kageVariant.Create(ACollection: TCollection);'#010+
  'begin'#010+
  '  inherited Create(ACollection);'#010+
  '  FTargets := TTargets.Create(TTarget);'#010+
  '  FOptions := TUnsortedCompilerOptionsStringList.Create;'#010+
  '  FIncludePath:=TConditionalStrings.Create(TConditionalStr','ing);'#010+
  '  FSourcePath:=TConditionalStrings.Create(TConditionalString);'#010+
  'end;'#010+
  #010+
  'destructor TPackageVariant.Destroy;'#010+
  'begin'#010+
  '  FOptions.Free;'#010+
  '  FTargets.Free;'#010+
  '  FIncludePath.Free;'#010+
  '  FSourcePath.Free;'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  'procedure TPackageVar','iant.AddOption(const aValue: string);'#010+
  'begin'#010+
  '  Options.Add(aValue);'#010+
  'end;'#010+
  #010+
  '{ TPackageVariants }'#010+
  #010+
  'procedure TPackageVariants.SetDefaultPackageVariantName(AValue: string'+
  ');'#010+
  'begin'#010+
  '  if FDefaultPackageVariantName=AValue then Exit;'#010+
  '  if not assigne','d(ItemByName(avalue)) then'#010+
  '    raise exception.CreateFmt(SErrPackVarNotExist,[AValue]);'#010+
  '  FDefaultPackageVariantName:=AValue;'#010+
  'end;'#010+
  #010+
  'function TPackageVariants.GetActivePackageVariant: TPackageVariant;'#010+
  'begin'#010+
  '  result := ItemByName(ActivePacka','geVariantName) as TPackageVariant;'+
  #010+
  'end;'#010+
  #010+
  'function TPackageVariants.GetDefaultPackageVariant: TPackageVariant;'#010+
  'begin'#010+
  '  result := ItemByName(DefaultPackageVariantName) as TPackageVariant;'#010+
  'end;'#010+
  #010+
  'procedure TPackageVariants.SetActivePackageVaria','ntName(AValue: strin'+
  'g);'#010+
  'begin'#010+
  '  if FActivePackageVariantName=AValue then Exit;'#010+
  '  if not assigned(ItemByName(avalue)) then'#010+
  '    raise exception.CreateFmt(SErrPackVarNotExist,[AValue]);'#010+
  '  FActivePackageVariantName:=AValue;'#010+
  'end;'#010+
  #010+
  'function TPack','ageVariants.Add(AName: String): TPackageVariant;'#010+
  'begin'#010+
  '  result := self.add as TPackageVariant;'#010+
  '  result.Name := AName;'#010+
  '  if FDefaultPackageVariantName='#039#039' then'#010+
  '    FDefaultPackageVariantName:=AName;'#010+
  'end;'#010+
  #010+
  'function TPackageVariants.GetEnumer','ator: TPackageVariantEnumerator;'#010+
  'begin'#010+
  '  Result:= TPackageVariantEnumerator.Create(Self);'#010+
  'end;'#010+
  #010+
  #010+
  '{ TConditionalDestStrings }'#010+
  #010+
  'function TConditionalDestStrings.GetConditionalString(Index : Integer)'+
  ': TConditionalDestString;'#010+
  'begin'#010+
  '  Result:=TC','onditionalDestString(Items[Index]);'#010+
  'end;'#010+
  #010+
  'procedure TConditionalDestStrings.SetConditionalString(Index : Integer'+
  '; AValue: TConditionalDestString);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  'constructor TConditionalDestStrings.Create(AClass: TCondi','tionalDestS'+
  'tringClass);'#010+
  'begin'#010+
  '  inherited Create(AClass);'#010+
  'end;'#010+
  #010+
  'function TConditionalDestStrings.Add(const Value: String; ADestPath: S'+
  'tring): TConditionalDestString;'#010+
  'begin'#010+
  '  Result:=Add(Value, AllCPUs, AllOSes, ADestPath);'#010+
  'end;'#010+
  #010+
  'function TC','onditionalDestStrings.Add(const Value: String; const OSes'+
  ': TOSes; ADestPath: String): TConditionalDestString;'#010+
  'begin'#010+
  '  Result:=Add(Value, AllCPUs, OSes, ADestPath);'#010+
  'end;'#010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TConditionalDestStrings.Add(Const',' Value : String;const CPUs'+
  ':TCPUs; ADestPath: String) : TConditionalDestString;inline;'#010+
  'begin'#010+
  '  Result:=Add(Value, CPUs, AllOSes, ADestPath);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'function TConditionalDestStrings.Add(const Value: String; const C','PUs'+
  ': TCPUs; const OSes: TOSes; ADestPath: String): TConditionalDestString'+
  ';'#010+
  'var'#010+
  '  ACondString: TConditionalDestString;'#010+
  'begin'#010+
  '  ACondString := inherited Add(Value,CPUs,OSes) as TConditionalDestStr'+
  'ing;'#010+
  '  ACondString.DestPath:=ADestPath;'#010+
  '  Resu','lt:=ACondString;'#010+
  'end;'#010+
  #010+
  '{ TPackageDictionary }'#010+
  #010+
  'function TPackageDictionary.GetValue(const AName, Args: String): Strin'+
  'g;'#010+
  'Var'#010+
  '  O : TObject;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  I:=Flist.IndexOf(AName);'#010+
  '  If (I=-1) then'#010+
  '    begin'#010+
  '      if assigned(MasterDi','ctionary) then'#010+
  '        result := MasterDictionary.GetValue(AName,Args)'#010+
  '      else'#010+
  '        result := GlobalDictionary.GetValue(AName,Args);'#010+
  '      Exit;'#010+
  '    end;'#010+
  '  O:=Flist.Objects[I];'#010+
  '  If O is TValueItem then'#010+
  '    Result:=TValueItem(O).FValu','e'#010+
  '  else'#010+
  '    Result:=TFunctionItem(O).FFunc(AName,Args);'#010+
  'end;'#010+
  #010+
  '{$ifndef NO_THREADING}'#010+
  #010+
  '{ TCompileWorkerThread }'#010+
  #010+
  'constructor TCompileWorkerThread.Create(ABuildEngine: TBuildEngine; No'+
  'tifyMainThreadEvent: PRTLEvent);'#010+
  'begin'#010+
  '  inherited Create','(true);'#010+
  '  FNotifyStartTask := RTLEventCreate;'#010+
  '  FBuildEngine := ABuildEngine;'#010+
  '  FNotifyMainThreadEvent:=NotifyMainThreadEvent;'#010+
  '  Start;'#010+
  'end;'#010+
  #010+
  'destructor TCompileWorkerThread.Destroy;'#010+
  'begin'#010+
  '  RTLeventdestroy(FNotifyStartTask);'#010+
  '  inherited De','stroy;'#010+
  'end;'#010+
  #010+
  'procedure TCompileWorkerThread.execute;'#010+
  '  procedure RaiseMainEvent;'#010+
  '  begin'#010+
  '    { Make sure all of our results are committed before we set (F)Done'+
  ' to true.'#010+
  '      While RTLeventSetEvent implies a barrier, once the main thread i'+
  's',' notified'#010+
  '      it will walk over all threads and look for those that have Done='+
  'true -> it'#010+
  '      can look at a thread between that thread setting FDone to true a'+
  'nd it'#010+
  '      calling RTLEventSetEvent }'#010+
  '    WriteBarrier;'#010+
  '    FDone:=true;'#010+
  '    R','TLeventSetEvent(FNotifyMainThreadEvent);'#010+
  '  end;'#010+
  'begin'#010+
  '  if not Terminated then'#010+
  '    RaiseMainEvent;'#010+
  '  while not Terminated do'#010+
  '    begin'#010+
  '    RTLeventWaitFor(FNotifyStartTask,500);'#010+
  '    if not FDone then'#010+
  '      begin'#010+
  '      { synchronise with Rea','dWriteBarrier in mainthread for same rea'+
  'son as above }'#010+
  '      ReadWriteBarrier;'#010+
  '      FBuildEngine.log(vlInfo,FWorkerPrefix+'#039'Compiling: '#039'+APac'+
  'kage.Name);'#010+
  '      FCompilationOK:=false;'#010+
  '      try'#010+
  '        FBuildEngine.Compile(APackage);'#010+
  '        ','FCompilationOK:=true;'#010+
  '        FBuildEngine.log(vlInfo,FWorkerPrefix+'#039'Done compiling: '#039+
  '+APackage.Name);'#010+
  '        RaiseMainEvent;'#010+
  '      except'#010+
  '        on E: Exception do'#010+
  '          begin'#010+
  '            FErrorMessage := FWorkerPrefix+'#039'Failed compil','ing: '#039+
  '+APackage.Name+'#039': '#039'+E.Message;'#010+
  '            FBuildEngine.log(vlInfo,FErrorMessage);'#010+
  '            RaiseMainEvent;'#010+
  '          end;'#010+
  '      end;'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  '{$endif NO_THREADING}'#010+
  #010+
  '{***********************************************','********************'+
  '*********'#010+
  '                           TUnsortedDuplicatesStringList'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TUnsortedDuplicatesStringList.Add(const S: string): Integer;'#010+
  #010+
  'beg','in'#010+
  '  result := IndexOf(S);'#010+
  '  If result > -1 then'#010+
  '    Case DUplicates of'#010+
  '      DupAccept,'#010+
  '      DupIgnore : Exit;'#010+
  '      DupError : Error(SDuplicateString,0)'#010+
  '    end;'#010+
  '  inherited Add(S);'#010+
  'end;'#010+
  #010+
  '{************************************************','*******************'+
  '*********'#010+
  '                           TUnsortedCompilerOptionsStringList'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor  TUnsortedCompilerOptionsStringList.Create;'#010+
  'begin'#010+
  '  Inheri','ted Create;'#010+
  '  Duplicates:=DupAccept;'#010+
  'end;'#010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TNamedItem'#010+
  '**********************************************************************'+
  '******','}'#010+
  #010+
  'procedure TNamedItem.SetName(const AValue: String);'#010+
  #010+
  'begin'#010+
  '  if FName=AValue then exit;'#010+
  '  With TNamedCollection(Collection) do'#010+
  '    If UniqueNames then'#010+
  '      If (IndexOfName(AVAlue)<>-1) then'#010+
  '        Raise ECollectionError.CreateFmt(SErrN','ameExists,[AValue]);'#010+
  '  FName:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TNamedCollection'#010+
  '********************************************************************','*'+
  '*******}'#010+
  #010+
  'function TNamedCollection.GetItem(Index: Integer): TNamedItem;'#010+
  'begin'#010+
  '  result := TNamedItem(inherited getItem(index));'#010+
  'end;'#010+
  #010+
  'procedure TNamedCollection.SetItem(Index: Integer; AValue: TNamedItem)'+
  ';'#010+
  'begin'#010+
  '  inherited SetItem(Index,',' AValue);'#010+
  'end;'#010+
  #010+
  'function TNamedCollection.IndexOfName(const AName: String): Integer;'#010+
  #010+
  'begin'#010+
  '  Result:=Count-1;'#010+
  '  While (Result>=0) and (CompareText(TNamedItem(Items[Result]).FName,A'+
  'Name)<>0) do'#010+
  '    Dec(Result);'#010+
  'end;'#010+
  #010+
  'function TNamedCollecti','on.ItemByName(const AName: String): TNamedIte'+
  'm;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=IndexOfName(AName);'#010+
  '  If (I=-1) Then'#010+
  '    Raise ECollectionError.CreateFmt(SErrNoSuchName,[AName]);'#010+
  '  Result:=TNamedItem(Items[i]);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************','**********************************************'+
  '*********'#010+
  '                             TNamedItemList'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TNamedItemList.GetNamedItem(Index : Integer): TNamed','Item;'#010+
  'begin'#010+
  '  Result:=TNamedItem(Items[Index]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TNamedItemList.SetNamedItem(Index : Integer; const AValue: T'+
  'NamedItem);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'function TNamedItemList.IndexOfName(const AName: String): Integer;',#010+
  'begin'#010+
  '  Result:=Count-1;'#010+
  '  While (Result>=0) and (CompareText(GetNamedItem(Result).Name,AName)<'+
  '>0) do'#010+
  '    Dec(Result);'#010+
  'end;'#010+
  #010+
  #010+
  'function TNamedItemList.ItemByName(const ANAme: String): TNamedItem;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  I:=IndexOfName(AN','ame);'#010+
  '  If (I=-1) Then'#010+
  '    Raise ECollectionError.CreateFmt(SErrNoSuchName,[AName]);'#010+
  '  Result:=TNamedItem(Items[i]);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TTarge','ts'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TTargets.GetTargetItem(Index : Integer): TTarget;'#010+
  'begin'#010+
  '  Result:=TTarget(Items[Index]);'#010+
  'end;'#010+
  #010+
  #010+
  'function TTargets.GetTarget(const AName : String): TT','arget;'#010+
  'begin'#010+
  '  Result:=TTarget(ItemByName(AName));'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TTargets.SetTargetItem(Index : Integer; const AValue: TTarge'+
  't);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  'function TTargets.AddFPDoc(const AUnitName, AXMLName: String): TTarget'+
  ';'#010,
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AUnitName;'#010+
  '  Result.XML:=AXMLName;'#010+
  '  Result.TargetType:=ttFPDoc;'#010+
  'end;'#010+
  #010+
  'function TTargets.AddUnit(const AUnitName: String): TTarget;'#010+
  'begin'#010+
  '  Result:=AddUnit(AUnitName,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  'fu','nction TTargets.AddUnit(const AUnitName: String; const OSes: TOSes'+
  '): TTarget;'#010+
  'begin'#010+
  '  Result:=AddUnit(AUnitName,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TTargets.AddUnit(Const AUnitName : String;const CPUs:TCPUs) :'+
  ' TTarget',';'#010+
  'begin'#010+
  '  Result:=AddUnit(AUnitName,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'function TTargets.AddUnit(const AUnitName: String; const CPUs: TCPUs;'#010+
  '  const OSes: TOSes): TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AUnitN','ame;'#010+
  '  Result.TargetType:=TTUnit;'#010+
  '  Result.CPUs:=CPUs;'#010+
  '  Result.OSes:=OSes;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTargets.AddImplicitUnit(const AUnitName: String; InstallUnit'+
  ': boolean'#010+
  '  ): TTarget;'#010+
  'begin'#010+
  '  Result:=AddImplicitUnit(AUnitName,AllCPUs,AllOSes,Insta','llUnit);'#010+
  'end;'#010+
  #010+
  #010+
  'function TTargets.AddImplicitUnit(const AUnitName: String; const OSes:'+
  ' TOSes;'#010+
  '  InstallUnit: boolean): TTarget;'#010+
  'begin'#010+
  '  Result:=AddImplicitUnit(AUnitName,AllCPUs,OSes,InstallUnit);'#010+
  'end;'#010+
  #010+
  #010+
  'function TTargets.AddImplicitUnit(co','nst AUnitName: String; const CPU'+
  's: TCPUs;'#010+
  '  InstallUnit: boolean): TTarget;'#010+
  'begin'#010+
  '  Result:=AddImplicitUnit(AUnitName,CPUs,AllOSes,InstallUnit);'#010+
  'end;'#010+
  #010+
  #010+
  'function TTargets.AddImplicitUnit(const AUnitName: String; const CPUs:'+
  ' TCPUs;'#010+
  '  const OS','es: TOSes; InstallUnit: boolean): TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AUnitName;'#010+
  '  Result.CPUs:=CPUs;'#010+
  '  Result.OSes:=OSes;'#010+
  '  if InstallUnit then'#010+
  '    Result.TargetType:=TTImplicitUnit'#010+
  '  else'#010+
  '    Result.TargetType:=TTClean','OnlyUnit;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTargets.AddProgram(const AProgramName: String): TTarget;'#010+
  'begin'#010+
  '  Result:=AddProgram(AProgramName,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  'function TTargets.AddProgram(const AProgramName: String; const OSes: T'+
  'OSes'#010+
  '  ): TTarget;'#010+
  'be','gin'#010+
  '  Result:=AddProgram(AProgramName,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TTargets.AddProgram(Const AProgramName : String;const CPUs:TC'+
  'PUs) : TTarget;'#010+
  'begin'#010+
  '  Result:=AddProgram(AProgramName,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endi','f cpu_only_overloads}'#010+
  #010+
  #010+
  'function TTargets.AddProgram(const AProgramName: String; const CPUs: T'+
  'CPUs;'#010+
  '  const OSes: TOSes): TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AProgramName;'#010+
  '  Result.CPUs:=CPUs;'#010+
  '  Result.OSes:=OSes;'#010+
  '  Resu','lt.TargetType:=ttProgram;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTargets.AddLibrary(const ALibraryName: String): TTarget;'#010+
  'begin'#010+
  '  Result:=AddLibrary(ALibraryName,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  'function TTargets.AddLibrary(const ALibraryName: String; const OSes: T'+
  'OSes'#010,
  '  ): TTarget;'#010+
  'begin'#010+
  '  Result:=AddLibrary(ALibraryName,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TTargets.AddLibrary(Const ALibraryName : String;const CPUs:TC'+
  'PUs) : TTarget;'#010+
  'begin'#010+
  '  Result:=AddLibrary(ALibraryName,CPUs,AllOS','es);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'function TTargets.AddLibrary(const ALibraryName: String; const CPUs: T'+
  'CPUs;'#010+
  '  const OSes: TOSes): TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=ALibraryName;'#010+
  '  Result.CPUs:=CPUs;'#010+
  '  Result.OS','es:=OSes;'#010+
  '  Result.TargetType:=ttSharedLibrary;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTargets.AddExampleUnit(const AUnitName: String): TTarget;'#010+
  'begin'#010+
  '  Result:=AddExampleUnit(AUnitName,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  'function TTargets.AddExampleUnit(const AUnitName: S','tring; const OSes'+
  ': TOSes'#010+
  '  ): TTarget;'#010+
  'begin'#010+
  '  Result:=AddExampleUnit(AUnitName,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TTargets.AddExampleUnit(Const AUnitName : String;const CPUs:T'+
  'CPUs) : TTarget;'#010+
  'begin'#010+
  '  Result:=AddExam','pleUnit(AUnitName,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'function TTargets.AddExampleUnit(const AUnitName: String; const CPUs: '+
  'TCPUs;'#010+
  '  const OSes: TOSes): TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AUnitName;'#010+
  '  Resu','lt.CPUs:=CPUs;'#010+
  '  Result.OSes:=OSes;'#010+
  '  Result.TargetType:=ttExampleUnit;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTargets.AddExampleProgram(const AProgramName: String): TTarg'+
  'et;'#010+
  'begin'#010+
  '  Result:=AddExampleProgram(AProgramName,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarge','ts.AddExampleProgram(const AProgramName: String;'#010+
  '  const OSes: TOSes): TTarget;'#010+
  'begin'#010+
  '  Result:=AddExampleProgram(AProgramName,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TTargets.AddExampleProgram(Const AProgramName : String',';cons'+
  't CPUs:TCPUs) : TTarget;'#010+
  'begin'#010+
  '  Result:=AddExampleProgram(AProgramName,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'function TTargets.AddExampleProgram(const AProgramName: String;'#010+
  '  const CPUs: TCPUs; const OSes: TOSes): TTarget;',#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AProgramName;'#010+
  '  Result.CPUs:=CPUs;'#010+
  '  Result.OSes:=OSes;'#010+
  '  Result.TargetType:=ttExampleProgram;'#010+
  'end;'#010+
  #010+
  'function TTargets.GetEnumerator: TTargetEnumerator;'#010+
  'begin'#010+
  '  Result:= TTargetEnumerator.Crea','te(Self);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TSources'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TSources.GetSour','ceItem(Index : Integer): TSource;'#010+
  'begin'#010+
  '  Result:=TSource(Items[Index]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TSources.SetSourceItem(Index : Integer; const AValue: TSourc'+
  'e);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'function TSources.AddDoc (const AFiles : String) ',': TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.FSourceType:=stDoc;'#010+
  'end;'#010+
  #010+
  #010+
  'function TSources.AddDoc(const AFiles: String; AInstallSourcePath: Str'+
  'ing): TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles',';'#010+
  '  Result.FInstallSourcePath:=AInstallSourcePath;'#010+
  '  Result.FSourceType:=stDoc;'#010+
  'end;'#010+
  #010+
  #010+
  'function TSources.AddSrc(const AFiles : String) : TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.FSourceType:=stSrc;'#010+
  'end;'#010+
  #010+
  #010+
  'fun','ction TSources.AddExample(const AFiles : String) : TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.FSourceType:=stExample;'#010+
  'end;'#010+
  #010+
  'function TSources.AddExample(const AFiles: String; AInstallSourcePath:'+
  ' String): TSourc','e;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.FInstallSourcePath:=AInstallSourcePath;'#010+
  '  Result.FSourceType:=stExample;'#010+
  'end;'#010+
  #010+
  #010+
  'function TSources.AddTest(const AFiles : String) : TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  ' ',' Result.Name:=AFiles;'#010+
  '  Result.FSourceType:=stTest;'#010+
  'end;'#010+
  #010+
  'function TSources.GetEnumerator: TSourceEnumerator;'#010+
  'begin'#010+
  '  Result:=TSourceEnumerator.Create(Self);'#010+
  'end;'#010+
  #010+
  'procedure TSources.AddDocFiles(const AFileMask, ASearchPathPrefix: str'+
  'ing; R','ecursive: boolean; AInstallSourcePath: String);'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, ASearchPathPrefix, Recursive, List);'#010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '    AddDoc(List[i], AI','nstallSourcePath);'#010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TSources.AddSrcFiles(const AFileMask, ASearchPathPrefix: str'+
  'ing; Recursive: boolean);'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, ASearch','PathPrefix, Recursive, List);'#010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '    AddSrc(List[i]);'#010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TSources.AddExampleFiles(const AFileMask, ASearchPathPrefix:'+
  ' string; Recursive: boolean; AInstallSourcePath: String);'#010+
  'var'#010+
  '  ','List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, ASearchPathPrefix, Recursive, List);'#010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '    AddExample(List[i], AInstallSourcePath);'#010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure T','Sources.AddTestFiles(const AFileMask, ASearchPathPrefix: '+
  'string; Recursive: boolean);'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, ASearchPathPrefix, Recursive, List);'#010+
  '  for i:= 0 to Pred','(List.Count) do'#010+
  '    AddTest(List[i]);'#010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                             TPackage'#010+
  '******************************************************************','**'+
  '********}'#010+
  #010+
  'constructor TPackage.Create(ACollection: TCollection);'#010+
  'begin'#010+
  '  inherited Create(ACollection);'#010+
  '  FVersion:=TFPVersion.Create;'#010+
  '  FTargets:=TTargets.Create(TTarget);'#010+
  '  FSources:=TSources.Create(TSource);'#010+
  '  FDependencies:=TDependen','cies.Create(TDependency);'#010+
  '  FInstallFiles:=TConditionalDestStrings.Create(TConditionalDestString'+
  ');'#010+
  '  FCleanFiles:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FUnitPath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FObjectPath:=TC','onditionalStrings.Create(TConditionalString);'#010+
  '  FIncludePath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FSourcePath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FExamplePath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  F','TestPath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FCommands:=TCommands.Create(TCommand);'#010+
  '  FUnitsOutputDir:='#039'units'#039'+PathDelim+'#039'$(target)'#039'+PathD'+
  'elim;'#010+
  '  FPackageVariants:=TFPList.Create;'#010+
  '  FCPUs:=AllCPUs;'#010+
  '  FOSes:=AllOSes;'#010+
  '  FInstal','ledChecksum:=$ffffffff;'#010+
  '  FFlags := TStringList.Create;'#010+
  '  // Implicit dependency on RTL'#010+
  '  FDependencies.Add('#039'rtl'#039');'#010+
  '  FSupportBuildModes:=[bmBuildUnit, bmOneByOne];'#010+
  '{$ifndef NO_THREADING}'#010+
  '  InitCriticalSection(FResolveDirsCS);'#010+
  '{$endif}'#010+
  'end;',#010+
  #010+
  #010+
  'destructor TPackage.destroy;'#010+
  #010+
  'begin'#010+
  '{$ifndef NO_THREADING}'#010+
  '  DoneCriticalSection(FResolveDirsCS);'#010+
  '{$endif}'#010+
  '  FreeAndNil(FDictionary);'#010+
  '  FreeAndNil(FDependencies);'#010+
  '  FreeAndNil(FInstallFiles);'#010+
  '  FreeAndNil(FCleanFiles);'#010+
  '  FreeAndNil(FIncl','udePath);'#010+
  '  FreeAndNil(FSourcePath);'#010+
  '  FreeAndNil(FExamplePath);'#010+
  '  FreeAndNil(FTestPath);'#010+
  '  FreeAndNil(FCommands);'#010+
  '  FreeAndNil(FObjectPath);'#010+
  '  FreeAndNil(FUnitPath);'#010+
  '  FreeAndNil(FSources);'#010+
  '  FreeAndNil(FTargets);'#010+
  '  FreeAndNil(FVersion);'#010+
  ' ',' FreeAndNil(FOptions);'#010+
  '  FreeAndNil(FTransmitOptions);'#010+
  '  FreeAndNil(FFlags);'#010+
  '  FreeAndNil(FPackageVariants);'#010+
  '  FreeAndNil(FBUTargets);'#010+
  '  inherited destroy;'#010+
  'end;'#010+
  #010+
  'function TPackage.HaveOptions: Boolean;'#010+
  'begin'#010+
  '  Result:=(FOptions<>Nil);'#010+
  'end;'#010,
  #010+
  'function TPackage.GetUnitsOutputDir(ACPU: TCPU; AOS: TOS; const aSubTa'+
  'rget: String): String;'#010+
  #010+
  'Var'#010+
  '  CT : TCompileTarget;'#010+
  #010+
  'begin'#010+
  '  CT.OS:=aOS;'#010+
  '  CT.CPU:=aCPU;'#010+
  '  CT.Subtarget:=aSubTarget;'#010+
  '  Result:=GetUnitsOutputDir(CT);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPa','ckage.SetName(const AValue: String);'#010+
  'begin'#010+
  '  inherited SetName(AValue);'#010+
  '  // RTL should not have any dependencies'#010+
  '  if AValue='#039'rtl'#039' then'#010+
  '    FDependencies.Clear;'#010+
  'end;'#010+
  #010+
  #010+
  'function TPackage.GetUnitsOutputDir(const aTarget: TCompileTarget): St'+
  'r','ing;'#010+
  'begin'#010+
  '  result:=FixPath(Dictionary.Substitute(FUnitsOutputDir,'#010+
  '     ['#039'CPU'#039',CPUToString(aTarget.CPU),'#010+
  '      '#039'OS'#039', OSToString(aTarget.OS),'#010+
  '      '#039'SUBTARGET'#039',aTarget.SubTarget,'#010+
  '      '#039'target'#039',MakeTargetString(aTarget)]),'#010+
  '      False);'#010+
  'end',';'#010+
  #010+
  'function TPackage.GetUnitConfigOutputFilename(ACPU: TCPU; AOS: TOS; co'+
  'nst aSubTarget: String): String;'#010+
  'Var'#010+
  '  CT : TCompileTarget;'#010+
  #010+
  'begin'#010+
  '  CT.OS:=aOS;'#010+
  '  CT.CPU:=aCPU;'#010+
  '  CT.Subtarget:=aSubTarget;'#010+
  '  Result:=GetUnitConfigOutputFilename(CT);',#010+
  'end;'#010+
  #010+
  'function TPackage.GetUnitConfigOutputFilename(const aTarget: TCompileT'+
  'arget'#010+
  '  ): String;'#010+
  'begin'#010+
  '  result:=FixPath(Dictionary.Substitute(Name+'#039'-$(target)'#039'+Fpmk'+
  'Ext,['#010+
  '    '#039'CPU'#039',CPUToString(aTarget.CPU),'#010+
  '    '#039'OS'#039',OSToString(aTarget.OS),'#010+
  ' ','   '#039'SUBTARGET'#039',aTarget.SubTarget,'#010+
  '    '#039'target'#039',MakeTargetString(atarget)]), False);'#010+
  'end;'#010+
  #010+
  'procedure TPackage.InheritPackageVariantsFromDependency(ADependencyPac'+
  'kage: TPackage);'#010+
  'var'#010+
  '  i: integer;'#010+
  '  APackageVariants: TPackageVariants;'#010+
  'begin'#010+
  ' ',' for i := 0 to ADependencyPackage.FPackageVariants.Count-1 do'#010+
  '    begin'#010+
  '      APackageVariants := TPackageVariants(ADependencyPackage.FPackage'+
  'Variants[i]);'#010+
  '      if APackageVariants.IsInheritable then'#010+
  '        begin'#010+
  '        if not assigned(G','etPackageVariantsByName(APackageVariants.Na'+
  'me)) then'#010+
  '          begin'#010+
  '          FPackageVariants.Add(APackageVariants);'#010+
  '          end;'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function TPackage.GetPackageVariantsByName(AName: string): TPackageVar'+
  'iants;'#010+
  'v','ar'#010+
  '  i: Integer;'#010+
  'begin'#010+
  '  result := nil;'#010+
  '  for i := 0 to FPackageVariants.Count-1 do'#010+
  '    if SameText(TPackageVariants(FPackageVariants.Items[i]).Name, ANam'+
  'e) then'#010+
  '      begin'#010+
  '      result := TPackageVariants(FPackageVariants.Items[i]);'#010+
  '     ',' break;'#010+
  '      end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SetUnitsOutputDir(AValue: string);'#010+
  'begin'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FUnitsOutputDir:=IncludeTrailingPathDelimiter(AValue)'#010+
  '  else'#010+
  '    FUnitsOutputDir:='#039#039';'#010+
  'end;'#010+
  #010+
  'function TPackage.GetPackageUnitInstal','lDir(ACPU: TCPU; AOS: TOS; con'+
  'st aSubTarget : String): String;'#010+
  'begin'#010+
  '  result:=FixPath(Dictionary.Substitute(FPackageUnitInstallDir,['#039'C'+
  'PU'#039',CPUToString(ACPU),'#039'OS'#039',OSToString(AOS),'#039'SUBTARGET'#039+
  ',aSubTarget,'#039'target'#039',MakeTargetString(ACPU,AOS,aSub','Target)])'+
  ', False);'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SetPackageUnitInstallDir(AValue: string);'#010+
  'begin'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FPackageUnitInstallDir:=IncludeTrailingPathDelimiter(AValue)'#010+
  '  else'#010+
  '    FPackageUnitInstallDir:='#039#039';'#010+
  'end;'#010+
  #010+
  #010+
  '{function TPacka','ge.GetBinOutputDir(ACPU: TCPU; AOS: TOS; const aSubT'+
  'arget : String): String;'#010+
  #010+
  'begin'#010+
  #010+
  'end;}'#010+
  #010+
  'function TPackage.GetBinOutputDir(const aTarget : TCompileTarget): Str'+
  'ing;'#010+
  'begin'#010+
  '  Result:='#039'bin'#039'+PathDelim+MakeTargetString(aTarget);'#010+
  'end;'#010+
  #010+
  #010+
  'functio','n TPackage.GetExamplesOutputDir(const aTarget : TCompileTarge'+
  't): String;'#010+
  'begin'#010+
  '  Result:=IncludeTrailingPathDelimiter(GetUnitsOutputDir(aTarget))+'#039+
  'examples'#039';'#010+
  'end;'#010+
  #010+
  'function TPackage.GetBinOutputDir(ACPU: TCPU; AOS: TOS; const aSubTarg'+
  'et: St','ring'#010+
  '  ): String;'#010+
  #010+
  'Var'#010+
  '  CT : TCompileTarget;'#010+
  #010+
  'begin'#010+
  '  CT.OS:=aOS;'#010+
  '  CT.CPU:=aCPU;'#010+
  '  CT.Subtarget:=aSubTarget;'#010+
  '  Result:=GetBinOutputDir(CT);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.GetCleanFiles(List: TStrings; ACPU:TCPU; AOS : TOS;'+
  ' const aSubTarget : S','tring);'#010+
  #010+
  'Var'#010+
  '  CT : TCompileTarget;'#010+
  #010+
  'begin'#010+
  '  CT.OS:=aOS;'#010+
  '  CT.CPU:=aCPU;'#010+
  '  CT.Subtarget:=aSubTarget;'#010+
  '  GetCleanFiles(List,CT);'#010+
  'end;'#010+
  #010+
  'procedure TPackage.GetCleanFiles(List: TStrings; const aTarget: TCompi'+
  'leTarget);'#010+
  'Var'#010+
  '  OB,OU,OE : String;'#010+
  ' ',' I : Integer;'#010+
  '  T : TTarget;'#010+
  'begin'#010+
  '  OB:=IncludeTrailingPathDelimiter(GetBinOutputDir(aTarget));'#010+
  '  if Not DirectoryExists(OB) then'#010+
  '    OB:='#039#039';'#010+
  '  OU:=IncludeTrailingPathDelimiter(GetUnitsOutputDir(aTarget));'#010+
  '  if not DirectoryExists(OU) then',#010+
  '    OU:='#039#039';'#010+
  '  OE:=IncludeTrailingPathDelimiter(GetExamplesOutputDir(aTarget));'#010+
  '  if not DirectoryExists(OE) then'#010+
  '    OE:='#039#039';'#010+
  '  List.Add(GetUnitConfigOutputFilename(aTarget));'#010+
  '  List.Add(ManifestFile);'#010+
  '  AddConditionalStrings(Self, List,Cle','anFiles,aTarget);'#010+
  '  For I:=0 to FTargets.Count-1 do'#010+
  '    begin'#010+
  '    T:=FTargets.TargetItems[I];'#010+
  '    if T.TargetType in [ttExampleUnit,ttExampleProgram] then'#010+
  '      T.GetCleanFiles(List, OE, OE, aTarget)'#010+
  '    else'#010+
  '      T.GetCleanFiles(List, OU,',' OB, aTarget);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.GetInstallFiles(List: TStrings;Types : TTargetTypes'+
  ';const aTarget : TCompileTarget);'#010+
  'Var'#010+
  '  OB,OU : String;'#010+
  '  I : Integer;'#010+
  '  T : TTarget;'#010+
  'begin'#010+
  '  if Types=[] then'#010+
  '    AddConditionalStrings(Se','lf, List,InstallFiles,aTarget)'#010+
  '  else'#010+
  '    begin'#010+
  '      OB:=IncludeTrailingPathDelimiter(GetBinOutputDir(Defaults.Compil'+
  'eTarget));'#010+
  '      OU:=IncludeTrailingPathDelimiter(GetUnitsOutputDir(Defaults.Comp'+
  'ileTarget));'#010+
  '      For I:=0 to FTargets.C','ount-1 do'#010+
  '        begin'#010+
  '          T:=FTargets.TargetItems[I];'#010+
  '          if (T.TargetType in Types) and Installer.BuildEngine.TargetI'+
  'nstallOK(T, aTarget) then'#010+
  '            T.GetInstallFiles(List, OU, OB, aTarget);'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010,
  #010+
  'procedure TPackage.GetInstallSourceFiles(List: TStrings; SourceTypes :'+
  ' TSourceTypes; TargetTypes : TTargetTypes);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  S : TSource;'#010+
  '  T : TTarget;'#010+
  'begin'#010+
  '  For I:=0 to FSources.Count-1 do'#010+
  '    begin'#010+
  '      S:=FSources.SourceIt','ems[I];'#010+
  '      if (S.SourceType in SourceTypes) then'#010+
  '        S.GetInstallFiles(List);'#010+
  '    end;'#010+
  '  For I:=0 to FTargets.Count-1 do'#010+
  '    begin'#010+
  '      T:=FTargets.TargetItems[I];'#010+
  '      if (T.TargetType in TargetTypes) then'#010+
  '        T.GetArchiveFile','s(List,Defaults.CPU,Defaults.OS);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.GetArchiveFiles(List: TStrings; aTarget: TCompileTa'+
  'rget);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  // Targets only'#010+
  '  For I:=0 to FTargets.Count-1 do'#010+
  '    FTargets.TargetItems[I].GetArchiv','eFiles(List,aTarget.CPU,aTarget'+
  '.OS);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.GetArchiveFiles(List: TStrings; ACPU:TCPU; AOS : TO'+
  'S);'#010+
  #010+
  'Var'#010+
  '  CT : TCompileTarget;'#010+
  #010+
  'begin'#010+
  '  CT.OS:=aOS;'#010+
  '  CT.CPU:=aCPU;'#010+
  '  CT.Subtarget:='#039#039';'#010+
  '  GetArchiveFiles(List,CT);'#010+
  'end;'#010+
  #010+
  'pro','cedure TPackage.GetArchiveSourceFiles(List: TStrings);'#010+
  'var'#010+
  '  i : integer;'#010+
  'begin'#010+
  '  for i := 0 to Sources.Count-1 do'#010+
  '    List.Add(Sources[i].Name);'#010+
  'end;'#010+
  #010+
  'function TPackage.GetDescription: string;'#010+
  'Var'#010+
  '  FN : String;'#010+
  '  L : TStringList;'#010+
  'begin'#010+
  '  ','If (FDescription<>'#039#039') then'#010+
  '    Result:=FDescription'#010+
  '  else'#010+
  '    If (FDescriptionFile<>'#039#039') then'#010+
  '      begin'#010+
  '        // Always relative to binary name.'#010+
  '        FN:=ExtractFilePath(ParamStr(0));'#010+
  '        FN:=FN+FDescriptionFile;'#010+
  '        If FileE','xists(FN) then'#010+
  '          begin'#010+
  '            L:=TStringList.Create;'#010+
  '            Try'#010+
  '              L.LoadFromFile(FN);'#010+
  '              Result:=L.Text;'#010+
  '            Finally'#010+
  '              L.Free;'#010+
  '            end;'#010+
  '          end;'#010+
  '      end;'#010+
  'end;'#010+
  #010+
  'fun','ction TPackage.GetDictionary: TDictionary;'#010+
  'begin'#010+
  '  if not assigned(FDictionary) then'#010+
  '    FDictionary:=PackageDictionaryClass.Create(Nil);'#010+
  '  result := FDictionary;'#010+
  'end;'#010+
  #010+
  #010+
  'function TPackage.GetVersion: string;'#010+
  'begin'#010+
  '  result:=FVersion.AsStrin','g;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SetOptions(const AValue: TStrings);'#010+
  'begin'#010+
  '  If (AValue=Nil) or (AValue.Count=0) then'#010+
  '    FreeAndNil(Foptions)'#010+
  '  else'#010+
  '    Options.Assign(AValue);'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SetTransmitOptions(AValue: TStrings);'#010+
  'begi','n'#010+
  '  If (AValue=Nil) or (AValue.Count=0) then'#010+
  '    FreeAndNil(FTransmitOptions)'#010+
  '  else'#010+
  '    TransmitOptions.Assign(AValue);'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SetVersion(const V: string);'#010+
  'begin'#010+
  '  FVersion.AsString:=V;'#010+
  'end;'#010+
  #010+
  #010+
  'function TPackage.GetFileNam','e: string;'#010+
  'begin'#010+
  '  If (FFileName<>'#039#039') then'#010+
  '    Result:=FFileName'#010+
  '  else'#010+
  '    if not FVersion.Empty and not'#010+
  '      ((Defaults.OS in AllLimit83fsOses) or (Defaults.BuildOS in AllLi'+
  'mit83fsOses)) then'#010+
  '      Result := Name + '#039'-'#039' + FVersion.AsStrin','g'#010+
  '    else'#010+
  '      Result := ShortName;'#010+
  'end;'#010+
  #010+
  #010+
  'function TPackage.GetShortName: string;'#010+
  'begin'#010+
  '  if FShortName<>'#039#039' then'#010+
  '    result := FShortName'#010+
  '  else'#010+
  '    result := Name;'#010+
  'end;'#010+
  #010+
  #010+
  'function TPackage.GetOptions: TStrings;'#010+
  'begin'#010+
  '  If (FOptions=Nil)',' then'#010+
  '    FOptions:=TUnsortedCompilerOptionsStringList.Create;'#010+
  '  Result:=FOptions;'#010+
  'end;'#010+
  #010+
  'function TPackage.GetTransmitOptions: TStrings;'#010+
  'begin'#010+
  '  If (FTransmitOptions=Nil) then'#010+
  '    FTransmitOptions:=TUnsortedCompilerOptionsStringList.Create;',#010+
  '  Result:=FTransmitOptions;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.GetManifest(Manifest: TStrings);'#010+
  #010+
  '  procedure AddOSes(const AIndent:string;AOSes:TOSes);'#010+
  '  var'#010+
  '    IOS : TOS;'#010+
  '  begin'#010+
  '    if (AOSes=AllOSes) then'#010+
  '      exit;'#010+
  '    Manifest.Add(AIndent+'#039'<o','ses>'#039');'#010+
  '    for IOS:=low(TOSes) to high(TOSes) do'#010+
  '      if IOS in AOSes then'#010+
  '        Manifest.Add(Format(AIndent+'#039' <os name="%s"/>'#039',[OSToSt'+
  'ring(IOS)]));'#010+
  '    Manifest.Add(AIndent+'#039'</oses>'#039');'#010+
  '  end;'#010+
  #010+
  '  procedure AddCPUs(const AIndent:string;A','CPUs:TCPUs);'#010+
  '  var'#010+
  '    ICPU : TCPU;'#010+
  '  begin'#010+
  '    if (ACPUs=AllCPUs) then'#010+
  '      exit;'#010+
  '    Manifest.Add(AIndent+'#039'<cpus>'#039');'#010+
  '    for ICPU:=low(TCPUs) to high(TCPUs) do'#010+
  '      if ICPU in ACPUs then'#010+
  '        Manifest.Add(Format(AIndent+'#039' <cpu name="','%s"/>'#039',[CPU'+
  'ToString(ICPU)]));'#010+
  '    Manifest.Add(AIndent+'#039'</cpus>'#039');'#010+
  '  end;'#010+
  #010+
  '  procedure AddSubTargets(const AIndent:string;aSubTargets:TRTLStringD'+
  'ynArray);'#010+
  '  var'#010+
  '    S : String;'#010+
  '  begin'#010+
  '    if (Length(aSubTargets)=0) then'#010+
  '      exit;'#010+
  '    Man','ifest.Add(AIndent+'#039'<subtargets>'#039');'#010+
  '    for S in aSubTargets do'#010+
  '      Manifest.Add(Format(AIndent+'#039' <subtarget name="%s"/>'#039',[S'+
  ']));'#010+
  '    Manifest.Add(AIndent+'#039'</subtargets>'#039');'#010+
  '  end;'#010+
  #010+
  #010+
  '  function GetXMLVersionString(sMajor, sMinor, sMicro, sBu','ild: integ'+
  'er): string;'#010+
  '  begin'#010+
  '    Result := '#039'<version'#039';'#010+
  '    if sMajor <> -1 then'#010+
  '      Result := Result + '#039' major="'#039' + IntToStr(sMajor) + '#039'"'+
  #039';'#010+
  '    if sMinor <> -1 then'#010+
  '      Result := Result + '#039' minor="'#039' + IntToStr(sMinor) + '#039'"'+
  #039';'#010+
  '    if sM','icro <> -1 then'#010+
  '      Result := Result + '#039' micro="'#039' + IntToStr(sMicro) + '#039'"'+
  #039';'#010+
  '    if sBuild <> -1 then'#010+
  '      Result := Result + '#039' build="'#039' + IntToStr(sBuild) + '#039'"'+
  #039';'#010+
  #010+
  '    Result := Result + '#039'/>'#039';'#010+
  '  end;'#010+
  #010+
  'Var'#010+
  '  S : String;'#010+
  '  i : Integer;'#010+
  '  D ',': TDependency;'#010+
  'begin'#010+
  '  With Manifest do'#010+
  '    begin'#010+
  '    Add(Format('#039'<package name="%s">'#039',[QuoteXml(Name)]));'#010+
  #010+
  '    Add('#039' '#039' + GetXMLVersionString(FVersion.Major,FVersion.Mino'+
  'r,FVersion.Micro,FVersion.Build));'#010+
  '    AddOSes('#039' '#039',OSes);'#010+
  '    AddCPUs(',#039' '#039',CPUs);'#010+
  '    AddSubTargets('#039' '#039',SubTargets);'#010+
  '    Add(Format('#039' <filename>%s</filename>'#039',[QuoteXml(FileName +'+
  ' MakeZipSuffix(cpuNone, osNone) + ZipExt)]));'#010+
  '    Add(Format('#039' <author>%s</author>'#039',[QuoteXml(Author)]));'#010+
  '    Add(Format('#039' <license>','%s</license>'#039',[QuoteXml(License)])'+
  ');'#010+
  '    if HomepageURL<>'#039#039' then'#010+
  '      Add(Format('#039' <homepageurl>%s</homepageurl>'#039',[QuoteXml(Ho'+
  'mepageURL)]));'#010+
  '    if DownloadURL<>'#039#039' then'#010+
  '      Add(Format('#039' <downloadurl>%s</downloadurl>'#039',[QuoteXml(Do'+
  'wnloadUR','L)]));'#010+
  '    Add(Format('#039' <email>%s</email>'#039',[QuoteXMl(Email)]));'#010+
  '    S:=Description;'#010+
  '    If (S<>'#039#039') then'#010+
  '      Add(Format('#039' <description>%s</description>'#039',[QuoteXML(S)'+
  ']));'#010+
  '    If (Dependencies.Count>0) then'#010+
  '      begin'#010+
  '        Add('#039' <depende','ncies>'#039');'#010+
  '        for I:=0 to Dependencies.Count-1 do'#010+
  '          begin'#010+
  '            D:=Dependencies[i];'#010+
  '            Add('#039'  <dependency>'#039');'#010+
  '            Add(Format('#039'   <package packagename="%s"/>'#039',[Quote'+
  'XML(D.Value)]));'#010+
  '            if not D.FVe','rsion.Empty then'#010+
  '              Add('#039'   '#039' + GetXMLVersionString(D.FVersion.Major'+
  ',D.FVersion.Minor,D.FVersion.Micro,D.FVersion.Build));'#010+
  '            AddOSes('#039'   '#039',D.OSes);'#010+
  '            AddCPUs('#039'   '#039',D.CPUs);'#010+
  '            Add('#039'  </dependency>'#039');'#010,
  '          end;'#010+
  '        Add('#039' </dependencies>'#039');'#010+
  '      end;'#010+
  '    Add('#039'</package>'#039');'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.ListPackage(PkgList: TStrings);'#010+
  #010+
  '  function GetArchiveName (const APackage: TPackage; ALimit83: boolean'+
  '): string;'#010+
  '  begin'#010+
  '{ ','Special hack to allow both long and short source files being recog'+
  'nized }'#010+
  '    if ALimit83 and (Defaults.ZipPrefix = '#039'units-'#039') then'#010+
  '      result := '#039'u'#039#010+
  '    else'#010+
  '      result := Defaults.ZipPrefix;'#010+
  '    if ALimit83 then'#010+
  '      result := result ','+ APackage.ShortName'#010+
  '    else'#010+
  '      result := result + APackage.Name;'#010+
  '    result := result + MakeZipSuffix(Defaults.CPU, Defaults.OS, '#039#039+
  ', ALimit83);'#010+
  '  end;'#010+
  #010+
  'Var'#010+
  '  S : String;'#010+
  'begin'#010+
  '{  if OSes = AllOSes then'#010+
  '    Exit;}'#010+
  '  if  ('#010+
  '       ('#010+
  '     ','   ((OSes = AllOSes) or (Defaults.OS in OSes)) and'#010+
  '        ((CPUs = AllCPUs) or (Defaults.CPU in CPUs))'#010+
  '       )'#010+
  '       or'#010+
  '       ('#010+
  '         (Defaults.OS = osNone) and (Defaults.CPU = cpuNone)'#010+
  '       )'#010+
  '      )'#010+
  '      and'#010+
  '       ((Defaults.Su','bTarget='#039#039') or SubTargetAllowed(Defaults.S'+
  'ubTarget))'#010+
  '     then'#010+
  '    begin'#010+
  '      if Defaults.OS = osNone then'#010+
  '        PkgList.Add (Format ('#039'# Source %d'#039', [Succ (PkgList.Cou'+
  'nt div 2)]))'#010+
  '      else {if OSes <> AllOSes then}'#010+
  '        PkgList.Add ','(Format ('#039'# '#039' + OSToString(Defaults.OS) '+
  '+ '#039' %d'#039', [Succ (PkgList.Count div 2)]));'#010+
  '      S := '#039'package='#039' + GetArchiveName (Self, false) + Archive'+
  'Extension;'#010+
  '      if ((ShortName <> Name) or (Defaults.ZipPrefix = '#039'units-'#039+
  ')) and'#010+
  '             ((D','efaults.OS in AllLimit83fsOSes) or (Defaults.OS = os'+
  'None)) then'#010+
  '        S := S + '#039'['#039' + GetArchiveName (Self, true) + ArchiveEx'+
  'tension + '#039']'#039';'#010+
  '      S := S + '#039','#039' + Description;'#010+
  '      PkgList.Add(S);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.GetDo','cProject(Proj: TStrings; P : TPackage; a'+
  'Indent: string);'#010+
  #010+
  '  Procedure AddLn(S : String);'#010+
  #010+
  '  begin'#010+
  '     Proj.Add(aIndent+S);'#010+
  '  end;'#010+
  #010+
  '  Procedure AddLn(Fmt : String; Args : array of const);'#010+
  #010+
  '  begin'#010+
  '    Proj.Add(aIndent+Fmt,Args);'#010+
  '  end;'#010+
  #010+
  'Var',#010+
  '  T : TTarget;'#010+
  '  S, O, FN : String;'#010+
  '  SL : TStringList;'#010+
  '  L : TUnsortedDuplicatesStringList;'#010+
  '  I : Integer;'#010+
  '  iCPU : TCPU;'#010+
  '  iOS : TOS;'#010+
  #010+
  'begin'#010+
  '  GPathPrefix:=P.Directory;'#010+
  '  AddPackageMacrosToDictionary(P,P.Dictionary);'#010+
  '  // First target OS',#010+
  '  ResolveFileNames(P,Defaults.CPU,Defaults.OS,False,True);'#010+
  '  // Then other OSes'#010+
  '  for ICPU:=Low(TCPU) to high(TCPU) do'#010+
  '    for IOS:=Low(TOS) to high(TOS) do'#010+
  '       if (IOS<>Defaults.OS) or (iCPU<>Defaults.CPU) then'#010+
  '         if OSCPUSupport','ed[IOS,ICPU] then'#010+
  '            ResolveFileNames(P,ICPU,IOS,false);'#010+
  '  AddLn('#039'<package name="%s" output="" content="%s.xct">'#039',[quot'+
  'exml(P.Name),quotexml(P.Name)]);'#010+
  '  Addln('#039'  <units>'#039');'#010+
  '  SL:=TStringList.Create;'#010+
  '  For T in P.Targets do'#010+
  '    if ','(T.TargetType in [ttUnit,ttImplicitUnit]) and (T.TargetSource'+
  'FileName<>'#039#039') then'#010+
  '      begin'#010+
  '      SL.Clear;'#010+
  '      // Writeln(T.Name,'#039' -> '#039',T.TargetSourceFileName);'#010+
  '      FN:=AddPathPrefix(P,T.TargetSourceFileName);'#010+
  '      SL.Add('#039'-d'#039'+CPUToSt','ring(Defaults.CPU));'#010+
  '      SL.Add('#039'-d'#039'+OSToString(Defaults.OS));'#010+
  '      if Defaults.OS in AllUnixOSes then'#010+
  '        SL.Add('#039'-dUNIX'#039');'#010+
  '      SL.Add('#039'-M'#039'+ModeToString(T.Mode));'#010+
  '      // Include Path'#010+
  '      L:=TUnsortedDuplicatesStringList.Create',';'#010+
  '      L.Duplicates:=dupIgnore;'#010+
  '      AddDependencyPaths(L,depInclude,T);'#010+
  '      AddConditionalStrings(P, L,P.IncludePath,Defaults.CompileTarget)'+
  ';'#010+
  '      AddConditionalStrings(P, L,T.IncludePath,Defaults.CompileTarget)'+
  ';'#010+
  '      for i:=0 to L.C','ount-1 do'#010+
  '        SL.Add('#039'-Fi'#039'+AddPathPrefix(P,L[i]));'#010+
  '      FreeAndNil(L);'#010+
  '      if P.HaveOptions Then'#010+
  '        SL.AddStrings(P.Options);'#010+
  '      if T.HaveOptions then'#010+
  '        SL.AddStrings(T.Options);'#010+
  '      O:='#039#039';'#010+
  '      for S in SL do'#010+
  '      ','  O:=O+'#039' '#039'+MaybeQuoted(P.Dictionary.ReplaceStrings(S))'+
  ';'#010+
  '      Delete(O,1,1);'#010+
  '      AddLn('#039'    <unit file="%s" options="%s"/>'#039',[FN,QuoteXML('+
  'O)]);'#010+
  '      end;'#010+
  '  Addln('#039'  </units>'#039');'#010+
  '  AddLn('#039'</package>'#039');'#010+
  'end;'#010+
  #010+
  'procedure TPackage.AddPackageVar','iant(APackageVariant: TPackageVarian'+
  'ts);'#010+
  'begin'#010+
  '  if not assigned(APackageVariant.FMasterPackage) then'#010+
  '    APackageVariant.FMasterPackage := Self;'#010+
  '  FPackageVariants.Add(APackageVariant);'#010+
  'end;'#010+
  #010+
  'procedure TPackage.ApplyPackageVariantToCompile','rOptions(ACompilerOpt'+
  'ions: tstrings);'#010+
  'var'#010+
  '  i: integer;'#010+
  '  PackageVariants: TPackageVariants;'#010+
  '  S : String;'#010+
  'begin'#010+
  '  for i := 0 to FPackageVariants.Count-1 do'#010+
  '    begin'#010+
  '    PackageVariants := TPackageVariants(FPackageVariants.Items[i]);'#010+
  '    f','or S in PackageVariants.ActivePackageVariant.Options do'#010+
  '      ACompilerOptions.Add(S);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SetDefaultPackageVariant;'#010+
  'var'#010+
  '  i,j: integer;'#010+
  '  PackageVariants: TPackageVariants;'#010+
  'begin'#010+
  '  for i := 0 to FPackageVarian','ts.Count-1 do'#010+
  '    begin'#010+
  '    PackageVariants := TPackageVariants(FPackageVariants.Items[i]);'#010+
  '    if Installer.FPackageVariantSettings.Values[PackageVariants.Name]<'+
  '>'#039#039' then'#010+
  '      PackageVariants.ActivePackageVariantName:= Installer.FPackageVa',
  'riantSettings.Values[PackageVariants.Name]'#010+
  '    else'#010+
  '      PackageVariants.ActivePackageVariantName:= PackageVariants.Defau'+
  'ltPackageVariantName;'#010+
  '    IncludePath.AddList(PackageVariants.ActivePackageVariant.IncludePa'+
  'th);'#010+
  '    SourcePath.AddLis','t(PackageVariants.ActivePackageVariant.SourcePa'+
  'th);'#010+
  '    Dictionary.AddVariable(PackageVariants.Name,PackageVariants.Active'+
  'PackageVariantName);'#010+
  '    SetUnitsOutputDir(IncludeTrailingPathDelimiter(FUnitsOutputDir)+'#039+
  '$('#039'+PackageVariants.name+'#039')'#039,');'#010+
  '    SetPackageUnitInstallDir(FixPath(FPackageUnitInstallDir, True)+'#039+
  '$('#039'+PackageVariants.Name+'#039')'#039');'#010+
  '    // Do not add targets f the package is inherited'#010+
  '    if PackageVariants.MasterPackage=Self then'#010+
  '      for j := 0 to PackageVariants.Ac','tivePackageVariant.Targets.cou'+
  'nt -1 do'#010+
  '        targets.add.assign(PackageVariants.ActivePackageVariant.Target'+
  's.items[j]);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.LoadUnitConfigFromFile(const AFileName: String);'#010+
  'var'#010+
  '  L,L2 : TStrings;'#010+
  '  VOS : TOS',';'#010+
  '  VCPU : TCPU;'#010+
  '  i,k : Integer;'#010+
  '  DepChecksum : Cardinal;'#010+
  '  DepName : String;'#010+
  '  D : TDependency;'#010+
  '  PackageVariantsStr: string;'#010+
  '  PackageVarName: string;'#010+
  '  pv: TPackageVariants;'#010+
  '  AnIsInheritable: boolean;'#010+
  'begin'#010+
  '  L:=TStringList.Create;'#010+
  '  ','Try'#010+
  '    ReadIniFile(AFileName,L);'#010+
  '    With L do'#010+
  '      begin'#010+
  '        Version:=Values[KeyVersion];'#010+
  '        InstalledChecksum:=Cardinal(StrToInt64Def(Values[KeyChecksum],'+
  '$ffffffff));'#010+
  '        VCPU:=StringToCPU(Values[KeyCPU]);'#010+
  '        VOS:=Stri','ngToOS(Values[KeyOS]);'#010+
  '        SubTargets:=Values[KeySubTargets].Split(RTLString('#039' '#039')'+
  ', TStringSplitOptions.ExcludeEmpty);'#010+
  '        OSes:=[VOS];'#010+
  '        CPUs:=[VCPU];'#010+
  '        L2:=TStringList.Create;'#010+
  '        L2.CommaText:=Values[KeyDepends];'#010+
  ' ','       for i:=0 to L2.Count-1 do'#010+
  '          begin'#010+
  '            DepName:=L2[i];'#010+
  '            k:=Pos('#039'|'#039',DepName);'#010+
  '            if k>0 then'#010+
  '              begin'#010+
  '                DepChecksum:=StrToInt(Copy(DepName,k+1,Length(DepName)'+
  '-k));'#010+
  '          ','      DepName:=Copy(DepName,1,k-1);'#010+
  '              end'#010+
  '            else'#010+
  '              DepChecksum:=$ffffffff;'#010+
  '            D:=Dependencies.Add(DepName,CPUs,OSes);'#010+
  '            D.RequireChecksum:=DepChecksum;'#010+
  '          end;'#010+
  '        FreeAndNil(L','2);'#010+
  '        NeedLibC:=Upcase(Values[KeyNeedLibC])='#039'Y'#039';'#010+
  '        IsFPMakeAddIn:=Upcase(Values[KeyAddIn])='#039'Y'#039';'#010+
  '        Flags.DelimitedText:=Values[KeyFlags];'#010+
  '        if Values[KeyTransmit]<>'#039#039' then'#010+
  '          TransmitOptions.DelimitedText:=Valu','es[KeyTransmit];'#010+
  '        i := 1;'#010+
  '        repeat'#010+
  '        PackageVariantsStr:=Values[KeyPackageVar+inttostr(i)];'#010+
  '        if PackageVariantsStr<>'#039#039' then'#010+
  '          begin'#010+
  '            k := pos('#039':'#039',PackageVariantsStr);'#010+
  '            if k > 0 then'#010+
  '  ','            begin'#010+
  '                PackageVarName:=copy(PackageVariantsStr,1,k-1);'#010+
  '                if PackageVarName[Length(PackageVarName)]='#039'*'#039' '+
  'then'#010+
  '                  begin'#010+
  '                  SetLength(PackageVarName,Length(PackageVarName)-1',')'+
  ';'#010+
  '                  AnIsInheritable:=true;'#010+
  '                  end'#010+
  '                else'#010+
  '                  AnIsInheritable:=false;'#010+
  '                PackageVariantsStr:=copy(PackageVariantsStr,k+1,length'+
  '(PackageVariantsStr)-k);'#010+
  '                ','pv := Installer.AddPackageVariant(PackageVarName, An'+
  'IsInheritable);'#010+
  '                AddPackageVariant(pv);'#010+
  #010+
  '                k := pos('#039','#039',PackageVariantsStr);'#010+
  '                while k>0 do'#010+
  '                  begin'#010+
  '                    PackageVa','rName:=copy(PackageVariantsStr,1,k-1);'#010+
  '                    PackageVariantsStr:=copy(PackageVariantsStr,k+1,le'+
  'ngth(PackageVariantsStr)-k);'#010+
  '                    pv.Add(PackageVarName);'#010+
  '                    k := pos('#039','#039',PackageVariantsStr);'#010+
  '    ','              end;'#010+
  '                pv.Add(PackageVariantsStr);'#010+
  '              end;'#010+
  '          end;'#010+
  '        inc(i);'#010+
  '        until PackageVariantsStr='#039#039';'#010+
  #010+
  '      end;'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SaveUnitConfigToStringLi','st(const AStringList: TSt'+
  'rings; ACPU: TCPU; AOS: TOS);'#010+
  'Var'#010+
  '  Subs,Deps : String;'#010+
  '  i,j : integer;'#010+
  '  D : TDependency;'#010+
  '  p : TPackage;'#010+
  '  PackageVariants : TPackageVariants;'#010+
  '  PackageVariantsStr: string;'#010+
  '  s: string;'#010+
  'begin'#010+
  '  with AStringList d','o'#010+
  '    begin'#010+
  '      Values[KeyName]:=Name;'#010+
  '      Values[KeyVersion]:=Version;'#010+
  '      // TODO Generate checksum based on PPUs'#010+
  '      InstalledChecksum:=DateTimeToFileDate(Now);'#010+
  '      Values[KeyChecksum]:=IntToStr(InstalledChecksum);'#010+
  '      Values','[KeyCPU]:=CPUToString(ACPU);'#010+
  '      Values[KeyOS]:=OSToString(AOS);'#010+
  '      Subs:='#039#039';'#010+
  '      For S in SubTargets do'#010+
  '        begin'#010+
  '        if Subs<>'#039#039' then'#010+
  '          Subs:=Subs+'#039' '#039';'#010+
  '        Subs:=Subs+S;'#010+
  '        end;'#010+
  '      Values[KeySubTargets]:','=Subs;'#010+
  '      //Installer;'#010+
  '      Values[KeySourcePath]:=IncludeTrailingPathDelimiter(IncludeTrail'+
  'ingPathDelimiter(Installer.BuildEngine.FStartDir)+Directory);'#010+
  '      Values[KeyFPMakeOptions]:=trim(Installer.FPMakeOptionsString);'#010+
  '      Deps:='#039,#039';'#010+
  '      for i:=0 to Dependencies.Count-1 do'#010+
  '        begin'#010+
  '          D:=Dependencies[i];'#010+
  '          if (ACPU in D.CPUs) and (AOS in D.OSes) then'#010+
  '            begin'#010+
  '              if Deps<>'#039#039' then'#010+
  '                Deps:=Deps+'#039','#039';'#010+
  '              D','eps:=Deps+D.Value;'#010+
  '              P:=TPackage(D.Target);'#010+
  '              if assigned(P) and (P.InstalledChecksum<>$ffffffff) then'+
  #010+
  '                Deps:=Deps+'#039'|'#039'+IntToStr(P.InstalledChecksum);'#010+
  '            end;'#010+
  '        end;'#010+
  '      Values[KeyDepen','ds]:=Deps;'#010+
  '      if Flags.Count>0 then'#010+
  '        Values[KeyFlags]:=Flags.DelimitedText;'#010+
  '      if TransmitOptions.Count>0 then'#010+
  '        Values[KeyTransmit]:=TransmitOptions.DelimitedText;'#010+
  '      if NeedLibC then'#010+
  '        Values[KeyNeedLibC]:='#039'Y'#039#010,
  '      else'#010+
  '        Values[KeyNeedLibC]:='#039'N'#039';'#010+
  '      if IsFPMakeAddIn then'#010+
  '        Values[KeyAddIn]:='#039'Y'#039#010+
  '      else'#010+
  '        Values[KeyAddIn]:='#039'N'#039';'#010+
  #010+
  '      s := '#039#039';'#010+
  '      for i := 0 to FTargets.Count-1 do'#010+
  '        begin'#010+
  '          if FTargets.Tar','getItems[i].IsFPMakePlugin then'#010+
  '            begin'#010+
  '              if s <> '#039#039' then'#010+
  '                s := s + '#039','#039';'#010+
  '              s := s + FTargets.TargetItems[i].Name;'#010+
  '            end;'#010+
  '        end;'#010+
  '      if s<>'#039#039' then'#010+
  '        Values[KeyPluginUni','ts]:=s;'#010+
  #010+
  '      for i := 0 to FPackageVariants.Count-1 do'#010+
  '        begin'#010+
  '          PackageVariants := TPackageVariants(FPackageVariants.Items[i'+
  ']);'#010+
  '          PackageVariantsStr:=PackageVariants.Name;'#010+
  '          if PackageVariants.IsInheritable ','then'#010+
  '            PackageVariantsStr:=PackageVariantsStr+'#039'*'#039';'#010+
  '          PackageVariantsStr := PackageVariantsStr +'#039':'#039'+Packag'+
  'eVariants.DefaultPackageVariantName;'#010+
  '          for j := 0 to PackageVariants.Count-1 do'#010+
  '            if not sametext(P','ackageVariants.Items[j].Name, PackageVa'+
  'riants.DefaultPackageVariantName) then'#010+
  '              PackageVariantsStr:=PackageVariantsStr+'#039','#039'+Packa'+
  'geVariants.Items[j].Name;'#010+
  '          values[KeyPackageVar+inttostr(i+1)] := PackageVariantsStr;'#010+
  '     ','   end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SaveUnitConfigToFile(const AFileName: String; ACPU:'+
  ' TCPU;'#010+
  '  AOS: TOS);'#010+
  'Var'#010+
  '  F : TFileStream;'#010+
  '  L : TStringList;'#010+
  'begin'#010+
  '  F:=TFileStream.Create(AFileName,fmCreate or fmShareDenyNone);'#010+
  '  L:=TStringLis','t.Create;'#010+
  '  try'#010+
  '    SaveUnitConfigToStringList(L,ACPU,AOS);'#010+
  '    L.SaveToStream(F);'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '    F.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.EnterResolveDirsCS;'#010+
  'begin'#010+
  '{$ifndef NO_THREADING}'#010+
  '   EnterCriticalSection(FResolveDirsCS);'#010+
  '{','$endif}'#010+
  'end;'#010+
  #010+
  'procedure TPackage.LeaveResolveDirsCS;'#010+
  'begin'#010+
  '{$ifndef NO_THREADING}'#010+
  '   LeaveCriticalSection(FResolveDirsCS);'#010+
  '{$endif}'#010+
  'end;'#010+
  #010+
  'function TPackage.AddTargetsFromDir(const aDirectory: String; const aM'+
  'ask: String): longint;'#010+
  'var'#010+
  '  lMa','sks : Array of string;'#010+
  '  lMask : string;'#010+
  '  lDir,lFile : string;'#010+
  '  lInfo : TSearchRec;'#010+
  'begin'#010+
  '  if aDirectory='#039#039' then'#010+
  '    lDir:='#039'./'#039#010+
  '  else'#010+
  '    lDir:=IncludeTrailingPathDelimiter(aDirectory);'#010+
  '  if not DirectoryExists(lDir) then'#010+
  '    Exit(-1);'#010,
  '  if aMask='#039#039' then'#010+
  '    lMasks:=['#039'*.pp'#039','#039'*.pas'#039','#039'*.lpr'#039']'#010+
  '  else'#010+
  '    lMasks:=aMask.Split(['#039';'#039']);'#010+
  '  Result:=0;'#010+
  '  for lMask in lMasks do'#010+
  '    begin'#010+
  '    if FindFirst(lDir+lMask,0,lInfo)=0 then'#010+
  '      try'#010+
  '        repeat'#010+
  '          lFile:=ldir+lInfo.','Name;'#010+
  '          if SameFileName(ExtractFileExt(lInfo.Name),'#039'.lpr'#039') t'+
  'hen'#010+
  '            Targets.AddProgram(lFile)'#010+
  '          else'#010+
  '            // Todo: add some detection for program/unit'#010+
  '            Targets.AddUnit(lFile);'#010+
  '          Installer.Lo','g(vlInfo,Format('#039'Adding unit %s to targets'+
  ' of %s'#039',[lFile,Name]));'#010+
  '          Inc(Result);'#010+
  '        until FindNext(lInfo)<>0;'#010+
  '      finally'#010+
  '        FindClose(lInfo)'#010+
  '      end;'#010+
  '    end;'#010+
  '  if Result>0 then'#010+
  '    begin'#010+
  '    Installer.Log(vlInfo, For','mat(SSwitchingToBuildUnitCompilation, ['+
  'Name]));'#010+
  '    Defaults.BuildMode:=bmBuildUnit;'#010+
  '    FSupportBuildModes:=[bmBuildUnit];'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.ChangePaths(Aliases : TStrings; aTarget : TCompileT'+
  'arget);'#010+
  #010+
  'var'#010+
  '  aDir,aDest, S, a','Line : string;'#010+
  '  aPath,aOp : Char;'#010+
  '  P : integer;'#010+
  '  CS : TConditionalString;'#010+
  '  CSL : TConditionalStrings;'#010+
  #010+
  'begin'#010+
  '  for S in Aliases do'#010+
  '    begin'#010+
  '    aLine:=Trim(S);'#010+
  '    P:=Pos('#039'}'#039',aLine);'#010+
  '    if Not (Copy(aLine,1,1)='#039'{'#039') and (Copy(aLine,4,1',')='#039':'+
  #039') and (P<>0) then'#010+
  '      Continue;'#010+
  '    aPath:=aLine[2];'#010+
  '    aOp:=aLine[3];'#010+
  '    aDir:= ExtractFileDir(Copy(aLine, 5, P - 5));'#010+
  '    P:=Pos('#039'='#039',aLine);'#010+
  '    if (P>0) then'#010+
  '      aDest:=Copy(aLine,P+1);'#010+
  '    CSL:=Nil;'#010+
  '    Case aPath of'#010+
  '      '#039's',#039' : CSL:=SourcePath;'#010+
  '      '#039'i'#039' : CSL:=IncludePath;'#010+
  '      '#039'u'#039' : CSL:=UnitPath;'#010+
  '    end;'#010+
  '    if Assigned(CSL) then'#010+
  '      Case aOp of'#010+
  '        '#039'*'#039' :'#010+
  '          if aDest<>'#039#039' then'#010+
  '              begin'#010+
  '              CS:=CSL.Find(aDir,aTarget.Cpu,aTa','rget.OS);'#010+
  '              if Assigned(CS) then'#010+
  '                CS.Value:=aDest;'#010+
  '              end;'#010+
  '        '#039'-'#039':'#010+
  '            begin'#010+
  '            CS:=CSL.Find(aDir,aTarget.Cpu,aTarget.OS);'#010+
  '            if Assigned(CS) then'#010+
  '              CS.Free;'#010+
  ' ','           end;'#010+
  '        '#039'+'#039':'#010+
  '            begin'#010+
  '            CS:=CSL.Find(aDir,aTarget.Cpu,aTarget.OS);'#010+
  '            if Assigned(CS) then'#010+
  '              CSL.Add(aDir,[aTarget.Cpu],[aTarget.OS]);'#010+
  '            end;'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'proced','ure TPackage.TransformAliases(Aliases : TStrings);'#010+
  #010+
  'Var'#010+
  '  I : integer;'#010+
  '  N,V : String;'#010+
  #010+
  'begin'#010+
  '{$IFDEF WINDOWS}'#010+
  '  for I := 0 to Pred(Aliases.Count) do'#010+
  '    Aliases[I] := FixPath(Aliases[I]);'#010+
  '{$ENDIF}'#010+
  #010+
  '  For I:=Aliases.Count-1 downto 0 do'#010+
  '    ','if pos('#039'='#039',Aliases[i])>0 then'#010+
  '    begin'#010+
  '      Aliases.GetNameValue(I,N,V);'#010+
  #010+
  '      if not N.StartsWith('#039'{'#039') then'#010+
  '      begin'#010+
  '        N:=ExtractFileName(N);'#010+
  '        V:=ExtractFileName(V);'#010+
  '        Aliases.Add(N+'#039'='#039'+V);'#010+
  '        N:=ChangeFileExt','(N,'#039#039');'#010+
  '        V:=ChangeFileExt(V,'#039#039');'#010+
  '        Aliases.Add(N+'#039'='#039'+V);'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.ApplyNameSpaces(aEngine: TBuildEngine; aFileName: s'+
  'tring; aTarget : TCompileTarget);'#010+
  #010+
  'Var'#010+
  '  Aliases : TStrings;'#010+
  '  T : TTarget',';'#010+
  '  D : TDependency;'#010+
  '  CS : TConditionalString;'#010+
  '  aDir, aUnitFileName, aNameSpaced, aNamespacedFile, aUnitName : strin'+
  'g;'#010+
  #010+
  'begin'#010+
  '  if not FileExists(aFileName) then'#010+
  '    Raise EInstallerError.CreateFmt('#039'No namespace definition file:'+
  ' %s'#039',[aFil','ename]);'#010+
  '  Aliases:=TStringList.Create;'#010+
  '  try'#010+
  '    Aliases.LoadFromFile(aFileName);'#010+
  '    TransFormAliases(Aliases);'#010+
  '    // Modify Targets'#010+
  '    For T in Targets do'#010+
  '      begin'#010+
  '      if (T.TargetType in [ttUnit,ttImplicitUnit,ttCleanOnlyUnit,ttE','x'+
  'ampleUnit]) then'#010+
  '        begin'#010+
  '        aUnitFileName:=T.TargetSourceFileName;'#010+
  '        aNamespacedFile:=Aliases.Values[aUnitFileName];'#010+
  '        if aNamespacedFile<>'#039#039' then'#010+
  '          begin'#010+
  '          aUnitName:=ExtractFileName(aNameSpacedFile)',';'#010+
  '          T.Name:=aUnitName;'#010+
  '          T.FTargetSourceFileName:=aNameSpacedFile;'#010+
  '          end;'#010+
  '        aDir:=ExtractFilePath(aUnitFileName);'#010+
  '        CS:=IncludePath.Find(aDir,aTarget.CPU,aTarget.OS);'#010+
  '        if Not Assigned(CS) then'#010+
  '    ','      IncludePath.Add(aDir,[aTarget.CPU],[aTarget.OS]);'#010+
  '        end;'#010+
  '      // Dependencies of target, regardless of type'#010+
  '      For D in T.Dependencies do'#010+
  '        if (D.DependencyType=depUnit) then'#010+
  '          begin'#010+
  '          aUnitName:=Extrac','tFileName(D.Value);'#010+
  '          aNamespaced:=Aliases.Values[aUnitName];'#010+
  '          if aNameSpaced<>'#039#039' then'#010+
  '            D.Value:=aNameSpaced;'#010+
  '          end;'#010+
  '      end;'#010+
  '    ChangePaths(Aliases,aTarget);'#010+
  '    // manipulate Paths'#010+
  '  finally'#010+
  '    Alia','ses.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.ApplyNameSpaces(aEngine: TBuildEngine; aTarget : TC'+
  'ompileTarget);'#010+
  #010+
  'Var'#010+
  '  aFileName : string;'#010+
  #010+
  'begin'#010+
  '  if NamespaceMap='#039#039' then'#010+
  '    exit;'#010+
  '  aFileName:=NameSpaceMap;'#010+
  '  if Directory<>'#039#039' then'#010+
  '    aFileNa','me:=IncludeTrailingPathDelimiter(Directory)+aFileName;'#010+
  '  ApplyNameSpaces(aEngine,aFileName,aTarget);'#010+
  'end;'#010+
  #010+
  #010+
  'function TPackage.SubTargetAllowed(const aSubTarget: String): Boolean;'+
  #010+
  'begin'#010+
  '  Result:=(Length(FSubTargets)=0);'#010+
  '  if not Result then',#010+
  '    Result:=IndexText(aSubTarget,FSubTargets)<>-1;'#010+
  'end;'#010+
  #010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                              TPackages'#010+
  '**************************************************************','******'+
  '********}'#010+
  #010+
  'function TPackages.GetPackage(const AName : String): TPackage;'#010+
  'begin'#010+
  '  Result:=TPackage(ItemByName(AName))'#010+
  'end;'#010+
  #010+
  'function TPackages.GetPackageItem(AIndex : Integer): TPackage;'#010+
  'begin'#010+
  '  Result:=TPackage(Items[AIndex]);'#010+
  'end;'#010+
  #010,
  #010+
  'procedure TPackages.SetPackageItem(AIndex : Integer; const AValue: TPa'+
  'ckage);'#010+
  'begin'#010+
  '  Items[AIndex]:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'function TPackages.AddPackage(const AName: String): TPackage;'#010+
  'var'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  i := IndexOfName(AName);'#010+
  '  if i > -','1 then'#010+
  '    begin'#010+
  '    result := PackageItems[i];'#010+
  '    Installer.Log(vlWarning,Format(SWarnDuplicatePackage,[AName]))'#010+
  '    end'#010+
  '  else'#010+
  '    begin'#010+
  '    Result:=Add as TPackage;'#010+
  '    Result.Name:=AName;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function TPackages.GetEnumerator',': TPackageEnumerator;'#010+
  'begin'#010+
  '  Result:= TPackageEnumerator.Create(Self);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                             TCustomDefaults'#010+
  '**************************************','******************************'+
  '********}'#010+
  #010+
  'procedure TCustomDefaults.SetCPU(const AValue: TCPU);'#010+
  'begin'#010+
  '  FCompileTarget.CPU:=AValue;'#010+
  '  GlobalDictionary.AddVariable('#039'CPU'#039',CPUToString(FCompileTarge'+
  't.CPU));'#010+
  '  RecalcTarget;'#010+
  'end;'#010+
  #010+
  'procedure TCusto','mDefaults.SetOptions(const AValue: TStrings);'#010+
  'begin'#010+
  '  If (AValue=Nil) or (AValue.Count=0) then'#010+
  '    FreeAndNil(Foptions)'#010+
  '  else'#010+
  '    Options.Assign(AValue)'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetBaseInstallDir: String;'#010+
  'begin'#010+
  '  If (FBaseInstallDir','<>'#039#039') then'#010+
  '    Result:=FBaseInstallDir'#010+
  '  else'#010+
  '    if UnixPaths then'#010+
  '      Result:=Prefix +'#039'lib'#039' + PathDelim + '#039'fpc'#039' + Path'+
  'Delim + FCompilerVersion + PathDelim'#010+
  '    else'#010+
  '      Result:=Prefix;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetBinInstallDir: ','String;'#010+
  'begin'#010+
  '  If (FBinInstallDir<>'#039#039') then'#010+
  '    Result:=FBinInstallDir'#010+
  '  else'#010+
  '    If UnixPaths then'#010+
  '      Result:=Prefix+'#039'bin'#039#010+
  '    else'#010+
  '      Result:=BaseInstallDir+'#039'bin'#039'+pathdelim+MakeTargetString('+
  'Defaults.cpu, Defaults.OS);'#010+
  'end;'#010+
  #010+
  'functio','n TCustomDefaults.GetLibInstallDir: String;'#010+
  'begin'#010+
  '  If (FLibInstallDir<>'#039#039') then'#010+
  '    Result:=FLibInstallDir'#010+
  '  else'#010+
  '    If UnixPaths then'#010+
  '      Result:=Prefix+'#039'lib'#039#010+
  '    else'#010+
  '      Result:=BaseInstallDir+'#039'bin'#039'+pathdelim+MakeTargetString('+
  'Defau','lts.cpu, Defaults.OS);'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetCompiler: String;'#010+
  'begin'#010+
  '  If (FCompiler<>'#039#039') then'#010+
  '    Result:=FCompiler'#010+
  '  else'#010+
  '    Result:='#039'fpc'#039';'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetDocInstallDir: String;'#010+
  'begin'#010+
  '  If (FDocInstallDir','<>'#039#039') then'#010+
  '    Result:=FDocInstallDir'#010+
  '  else if UnixPaths and ((Defaults.BuildOS=freebsd) or (Defaults.Build'+
  'OS=dragonfly)) then'#010+
  '    Result:=Prefix+'#039'share'#039'+PathDelim+'#039'doc'#039'+PathDelim+'#039+
  'fpc-$(CompilerVersion)'#039'+PathDelim+'#039'$(PackageName)'#039#010+
  '  else ','If UnixPaths then'#010+
  '    Result:=Prefix+'#039'share'#039'+PathDelim+'#039'doc'#039'+PathDelim+'#039+
  'fpc-$(CompilerVersion)'#039'+PathDelim+'#039'$(PackageName)'#039#010+
  '  else'#010+
  '    Result:=BaseInstallDir+'#039'doc'#039'+PathDelim+'#039'$(PackageName)'+
  #039';'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetExamplesInstal','lDir: String;'#010+
  'begin'#010+
  '  If (FExamplesInstallDir<>'#039#039') then'#010+
  '    Result:=FExamplesInstallDir'#010+
  '  else if UnixPaths and ((Defaults.BuildOS=freebsd) or (Defaults.Build'+
  'OS=dragonfly)) then'#010+
  '    Result:=Prefix+'#039'share'#039'+PathDelim+'#039'examples'#039'+PathDe'+
  'lim+'#039'fpc','-$(CompilerVersion)'#039'+PathDelim+'#039'$(PackageName)'#039+
  '+PathDelim+'#039'examples'#039#010+
  '  else If UnixPaths then'#010+
  '    Result:=Prefix+'#039'share'#039'+PathDelim+'#039'doc'#039'+PathDelim+'#039+
  'fpc-$(CompilerVersion)'#039'+PathDelim+'#039'$(PackageName)'#039'+PathDel'+
  'im+'#039'examples'#039#010+
  '  else'#010+
  '    Result:=B','aseInstallDir+'#039'examples'#039'+PathDelim+'#039'$(Packa'+
  'geName)'#039';'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetOptions: TStrings;'#010+
  'begin'#010+
  '  If (FOptions=Nil) then'#010+
  '    FOptions:=TUnsortedCompilerOptionsStringList.Create;'#010+
  '  Result:=FOptions;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDef','aults.GetPrefix: String;'#010+
  'begin'#010+
  '  // Use ExpandFileName to support ~/ expansion'#010+
  '  if FPrefix<>'#039#039' then'#010+
  '    Result:=IncludeTrailingPathDelimiter(ExpandFileName(FPrefix))'#010+
  '  else'#010+
  '    Result:='#039#039';'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetSearchPath: TSt','rings;'#010+
  'begin'#010+
  '  Result := FSearchPath;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetUnitInstallDir: String;'#010+
  'begin'#010+
  '  result := FUnitInstallDir;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetUnitConfigFilesInstallDir: String;'#010+
  'begin'#010+
  '  result := FUnitConfigFilesInst','allDir;'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.SetCompileTarget(AValue: TCompileTarget);'#010+
  'begin'#010+
  '  if FCompileTarget.Equals(AValue) then Exit;'#010+
  '  FCompileTarget:=AValue;'#010+
  '  RecalcTarget;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetLocalUnitDir: String;'#010+
  'begin'#010+
  ' ',' if FSearchPath.Count>0 then'#010+
  '    Result:=FSearchPath[0]'#010+
  '  else'#010+
  '    Result:='#039#039';'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetFPDocOutputDir: String;'#010+
  'begin'#010+
  '  If (FFPDocOutputDir<>'#039#039') then'#010+
  '    Result:=FixPath(FFPDocOutputDir, True)'#010+
  '  else'#010+
  '    Result:=Fix','Path('#039'.'#039'+PathDelim+'#039'docs'#039', True);'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetFPUnitSourcePath: String;'#010+
  'begin'#010+
  '  If (FFPUnitSourcePath='#039#039') or (FFPUnitSourcePath='#039'0'#039') t'+
  'hen'#010+
  '    result := FFPUnitSourcePath'#010+
  '  else'#010+
  '    Result:=FixPath(FFPUnitSourcePath, Tr','ue);'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.SafeExpandFileName(const AFileName: string): '+
  'string;'#010+
  'begin'#010+
  '  if AFileName<>'#039#039' then'#010+
  '    Result:=IncludeTrailingPathDelimiter(ExpandFileName(AFileName))'#010+
  '  else'#010+
  '    Result:='#039#039';'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults','.GetTarget: String;'#010+
  'begin'#010+
  '  Result:=FCompileTarget.AsString;'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetBuildCPU: TCpu;'#010+
  'begin'#010+
  '  Result:=FBuildTarget.CPU;'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetBuildOS: TOS;'#010+
  'begin'#010+
  '  Result:=FBuildTarget.OS;'#010+
  'end;'#010+
  #010+
  'function',' TCustomDefaults.GetBuildString: String;'#010+
  'begin'#010+
  '  result := MakeTargetString(BuildCPU, BuildOS);'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetGlobalUnitDir: String;'#010+
  'begin'#010+
  '  if FSearchPath.Count>1 then'#010+
  '    Result:=FSearchPath[1]'#010+
  '  else'#010+
  '    Result:='#039#039';'#010+
  'e','nd;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetLocalUnitDir(const AValue: String);'#010+
  'begin'#010+
  '  // Use ExpandFileName to support ~/ expansion'#010+
  '  if FSearchPath.Count=0 then'#010+
  '    FSearchPath.Add(SafeExpandFileName(AValue))'#010+
  '  else'#010+
  '    FSearchPath[0]:=SafeExpand','FileName(AValue);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetGlobalUnitDir(const AValue: String);'#010+
  'begin'#010+
  '  // Use ExpandFileName to support ~/ expansion'#010+
  '  if FSearchPath.Count<2 then'#010+
  '    begin'#010+
  '      if FSearchPath.Count<1 then'#010+
  '        FSearchPath.A','dd('#039#039');'#010+
  '      FSearchPath.Add(SafeExpandFileName(AValue));'#010+
  '    end'#010+
  '  else'#010+
  '    FSearchPath[1]:=SafeExpandFileName(AValue);'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.IntSetBaseInstallDir(const AValue: String);'#010+
  'begin'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FBaseInstal','lDir:=IncludeTrailingPathDelimiter(AValue)'#010+
  '  else'#010+
  '    FBaseInstallDir:='#039#039';'#010+
  '  GlobalDictionary.AddVariable('#039'baseinstalldir'#039',BaseInstallDir'+
  ');'#010+
  '  GlobalDictionary.AddVariable('#039'bininstalldir'#039',BinInstallDir);'+
  #010+
  '  GlobalDictionary.AddVariable('#039'libin','stalldir'#039',LibInstallDir'+
  ');'#010+
  '  BinInstallDir:='#039#039';'#010+
  '  LibInstallDir:='#039#039';'#010+
  '  ExamplesInstallDir:='#039#039';'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetBaseInstallDir(const AValue: String);'#010+
  'begin'#010+
  '  // There must be a possibility to skip ExpandFileName. So that t','he'+
  ' files'#010+
  '  // can be written into an archive with a relative path.'#010+
  '  if AValue<>'#039#039' then'#010+
  '    // Use ExpandFileName to support ~/ expansion'#010+
  '    IntSetBaseInstallDir(ExpandFileName(AValue))'#010+
  '  else'#010+
  '    IntSetBaseInstallDir(AValue);'#010+
  'end;'#010+
  #010+
  #010+
  'proce','dure TCustomDefaults.SetOS(const AValue: TOS);'#010+
  'begin'#010+
  '  FCompileTarget.OS:=AValue;'#010+
  '  GlobalDictionary.AddVariable('#039'OS'#039',OSToString(FCompileTarget.'+
  'OS));'#010+
  '  Recalctarget;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetPrefix(const AValue: String);'#010+
  'begin'#010+
  '  ','if FPrefix=AValue then exit;'#010+
  '  FPrefix:=AValue;'#010+
  '  GlobalDictionary.AddVariable('#039'prefix'#039',Prefix);'#010+
  '  GlobalDictionary.AddVariable('#039'bininstalldir'#039',BinInstallDir);'+
  #010+
  '  GlobalDictionary.AddVariable('#039'libinstalldir'#039',LibInstallDir);'+
  #010+
  '  BaseInstallDir:','='#039#039';'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.SetSearchPath(AValue: TStrings);'#010+
  'begin'#010+
  '  FSearchPath.Assign(AValue);'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.SetSubTarget(AValue: String);'#010+
  'begin'#010+
  '  if FCompileTarget.SubTarget=AValue then Exit;'#010+
  '  FCompileTarget.S','ubTarget:=Lowercase(AValue);'#010+
  '  RecalcTarget;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetTarget(const AValue: String);'#010+
  'Var'#010+
  '  aTarget : String;'#010+
  'begin'#010+
  '  aTarget:=aValue;'#010+
  '  if Target<>aTarget then'#010+
  '    begin'#010+
  '    FCompileTarget.AsString:=aValue;'#010+
  '    Glo','balDictionary.AddVariable('#039'CPU'#039',CPUToString(CPU));'#010+
  '    GlobalDictionary.AddVariable('#039'OS'#039',OSToString(OS));'#010+
  '    GlobalDictionary.AddVariable('#039'SUBTARGET'#039',SubTarget);'#010+
  '    GlobalDictionary.AddVariable('#039'target'#039',Target);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure T','CustomDefaults.SetUnitInstallDir(const AValue: String);'#010+
  'begin'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FUnitInstallDir:=AValue'#010+
  '  else'#010+
  '    FUnitInstallDir:='#039#039';'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetUnitConfigFilesInstallDir(const AValue: S'+
  'tring);'#010+
  'begin'#010+
  '  FUni','tConfigFilesInstallDir:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetZipPrefix(AValue: String);'#010+
  'begin'#010+
  '  if FZipPrefix=AValue then Exit;'#010+
  '  FZipPrefix:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.RecalcTarget;'#010+
  'begin'#010+
  '  GlobalDictionary.AddVariable','('#039'target'#039',Target);'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.CmdLineOptions: String;'#010+
  'begin'#010+
  '  If Haveoptions then'#010+
  '    Result:=OptionListToString(FOptions);'#010+
  'end;'#010+
  #010+
  #010+
  'constructor TCustomDefaults.Create;'#010+
  'begin'#010+
  '  FSearchPath:=TStringList.Create;'#010+
  '  InitDefaul','ts;'#010+
  '  FBuildTarget.OS:= StringToOS({$I %FPCTARGETOS%});'#010+
  '  FBuildTarget.CPU:= StringToCPU({$I %FPCTARGETCPU%});'#010+
  '  FBuildTarget.SubTarget:='#039#039';'#010+
  'end;'#010+
  #010+
  'destructor TCustomDefaults.Destroy;'#010+
  'begin'#010+
  '  FreeAndNil(FOptions);'#010+
  '  FreeAndNil(FSearchPath);'#010,
  '  inherited;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.InitDefaults;'#010+
  'begin'#010+
  '{$ifdef unix}'#010+
  '  UnixPaths:=True;'#010+
  '{$else}'#010+
  '  UnixPaths:=False;'#010+
  '{$endif}'#010+
  '  FNoFPCCfg:=False;'#010+
  '  FCompileTarget.CPU:=cpuNone;'#010+
  '  FCompileTarget.OS:=osNone;'#010+
  '  FCompileTarget.Subtarg','et:='#039#039';'#010+
  '  FUnitInstallDir:='#039'$(baseinstalldir)units/$(target)/$(packagename'+
  ')'#039';'#010+
  '  FUnitConfigFilesInstallDir:='#039'fpmkinst/$(target)'#039';'#010+
  '  FBuildMode:=bmOneByOne;'#010+
  '  FThreadsAmount:=-1;'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.HaveOptions: Boolean;'#010+
  'begin'#010+
  '  R','esult:=Assigned(FOptions);'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.AddOption(const aValue: string);'#010+
  'begin'#010+
  '  Options.Add(aValue);'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.IsBuildDifferentFromTarget: boolean;'#010+
  'begin'#010+
  '  result := IsDifferentFromBuild(CPU,OS);'#010+
  'end',';'#010+
  #010+
  #010+
  'procedure TCustomDefaults.LocalInit(const AFileName: String);'#010+
  'Var'#010+
  '  FN : String;'#010+
  'begin'#010+
  '  FN:=AFileName;'#010+
  '  If (FN='#039#039') then'#010+
  '    begin'#010+
  '    // Environment variable.'#010+
  '    FN:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.GetEnvironment'+
  'Varia','ble('#039'FPMAKECFG'#039');'#010+
  '    If (FN<>'#039#039') then'#010+
  '      If not FileExists(FN) then'#010+
  '        FN:='#039#039';'#010+
  '    // User config file fpmake.cfg'#010+
  '    If (FN='#039#039') then'#010+
  '      begin'#010+
  '      FN:=GetAppConfigFile(False);'#010+
  '      If Not FileExists(FN) then'#010+
  '        FN:='#039#039';'#010+
  ' ','     end;'#010+
  '    // Global config file fpmake.cfg'#010+
  '    If (FN='#039#039') then'#010+
  '      begin'#010+
  '      FN:=GetAppConfigFile(True);'#010+
  '      If Not FileExists(FN) then'#010+
  '        FN:='#039#039';'#010+
  '      end;'#010+
  '    end;'#010+
  '  If (FN<>'#039#039') and FileExists(FN) then'#010+
  '    LoadFromFile(FN)',';'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.CompilerDefaults;'#010+
  #010+
  'var'#010+
  '  infoSL : TStringList;'#010+
  #010+
  #010+
  'begin'#010+
  '  infosl:=TStringList.Create;'#010+
  '  try'#010+
  '    infosl.Delimiter:='#039' '#039';'#010+
  '    if (CPU=cpuNone) or'#010+
  '       ((OS=osNone) and not ExplicitOSNone) or'#010+
  '       (FCompile','rVersion='#039#039') then'#010+
  '    begin'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '      // Detect compiler version/target from -i option'#010+
  '        infosl.DelimitedText:=GetCompilerInfo(GetCompiler,['#039'-iVTPT'+
  'O'#039'],False, True);'#010+
  '        if infosl.Count<>3 then'#010+
  '          Raise',' EInstallerError.Create(SErrInvalidFPCInfo);'#010+
  '        if FCompilerVersion='#039#039' then'#010+
  '          FCompilerVersion:=infosl[0];'#010+
  '        if CPU=cpuNone then'#010+
  '          CPU:=StringToCPU(infosl[1]);'#010+
  '        if (OS=osNone) and not ExplicitOSNone then'#010+
  '  ','        OS:=StringToOS(infosl[2]);'#010+
  #010+
  '{$else HAS_UNIT_PROCESS}'#010+
  '      // Defaults taken from compiler used to build fpmake'#010+
  '      if CPU=cpuNone then'#010+
  '        CPU:=StringToCPU({$I %FPCTARGETCPU%});'#010+
  '      if (OS=osNone) and not ExplicitOSNone the','n'#010+
  '        OS:=StringToOS({$I %FPCTARGETOS%});'#010+
  '      if FCompilerVersion='#039#039' then'#010+
  '        FCompilerVersion:={$I %FPCVERSION%};'#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  '    end;'#010+
  '  if (FSourceTarget.OS=osNone) then'#010+
  '    begin'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '      //',' Detect compiler version/target from -i option'#010+
  '      infosl.DelimitedText:=GetCompilerInfo(GetCompiler,['#039'-iDWSPSO'+
  #039'], False, True);'#010+
  '      if infosl.Count<>4 then'#010+
  '        Raise EInstallerError.Create(SErrInvalidFPCInfo);'#010+
  '      FCompilerDate:=','infosl[0];'#010+
  '      FFullCompilerVersion:=infosl[1];'#010+
  '      FSourceTarget.CPU:=StringToCPU(infosl[2]);'#010+
  '      FSourceTarget.OS:=StringToOS(infosl[3]);'#010+
  '{$else HAS_UNIT_PROCESS}'#010+
  '      // Defaults taken from compiler used to build fpmake'#010+
  '      FSou','rceTarget.CPU:=StringToCPU({$I %FPCTARGETCPU%});'#010+
  '      FSourceTarget.OS:=StringToOS({$I %FPCTARGETOS%});'#010+
  '      FFullCompilerVersion:={$I %FPCFULLVERSION%};'#010+
  '      FCompilerDate:={$I %FPCDATE%};'#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  '    end;'#010+
  '  finally'#010+
  '  ','  infosl.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.LoadFromFile(const AFileName: String);'#010+
  'Var'#010+
  '  F : TFileStream;'#010+
  'begin'#010+
  '  F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyNone);'#010+
  '  Try'#010+
  '    LoadFromStream(F);'#010+
  '  Finally'#010+
  '    F.Free;'#010,
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SaveToFile(const AFileName: String);'#010+
  'Var'#010+
  '  F : TFileStream;'#010+
  'begin'#010+
  '  F:=TFileStream.Create(AFileName,fmCreate or fmShareDenyNone);'#010+
  '  Try'#010+
  '    SaveToStream(F);'#010+
  '  Finally'#010+
  '    F.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedu','re TCustomDefaults.SaveToStream(S : TStream);'#010+
  'Var'#010+
  '  L : TStringList;'#010+
  'begin'#010+
  '  L:=TStringList.Create;'#010+
  '  try'#010+
  '    With L do'#010+
  '      begin'#010+
  '      Values[KeyArchive]:=FArchive;'#010+
  '      Values[KeyCompiler]:=FCompiler;'#010+
  '      Values[KeyCopy]:=FCopy;'#010+
  '    ','  Values[KeyMkDir]:=FMkDir;'#010+
  '      Values[KeyMove]:=FMove;'#010+
  '      Values[KeyOptions]:=CmdLineOptions;'#010+
  '      Values[KeyCPU]:=CPUToString(CPU);'#010+
  '      Values[KeyOS]:=OSToString(OS);'#010+
  '      Values[KeyMode]:=ModeToString(FMode);'#010+
  '      Values[KeyLoc','alUnitDir]:=LocalUnitDir;'#010+
  '      Values[KeyGlobalUnitDir]:=GlobalUnitDir;'#010+
  '      Values[KeyPrefix]:=FPrefix;'#010+
  '      Values[KeyBaseInstallDir]:=FBaseInstallDir;'#010+
  '      Values[KeyUnitInstallDir]:=FUnitInstallDir;'#010+
  '      Values[KeyBinInstallDir]:=F','BinInstallDir;'#010+
  '      Values[KeyLibInstallDir]:=FLibInstallDir;'#010+
  '      Values[KeyDocInstallDir]:=FDocInstallDir;'#010+
  '      Values[KeyExamplesInstallDir]:=FExamplesInstallDir;'#010+
  '      Values[KeyRemove]:=FRemove;'#010+
  '      Values[KeyRemoveDir]:=FRemoveDi','r;'#010+
  '      Values[KeyRemoveTree]:=FRemoveTree;'#010+
  '      Values[KeyTarget]:=Target;'#010+
  '      if FNoFPCCfg then'#010+
  '        Values[KeyNoFPCCfg]:='#039'Y'#039';'#010+
  '      if FUseEnvironment then'#010+
  '        Values[KeyUseEnv]:='#039'Y'#039';'#010+
  '      if FInstallExamples then'#010+
  '          V','alues[KeyInstallExamples]:='#039'Y'#039';'#010+
  '      if FBuildExamples then'#010+
  '        Values[KeyBuildExamples]:='#039'Y'#039';'#010+
  '      if FSkipCrossPrograms then'#010+
  '        Values[KeySkipCrossPrograms]:='#039'Y'#039';'#010+
  '      if FSkipAllPrograms then'#010+
  '        Values[KeySkipAllPrograms',']:='#039'Y'#039';'#010+
  '      end;'#010+
  '    L.SaveToStream(S);'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.LoadFromStream(S: TStream);'#010+
  'Var'#010+
  '  L : TStrings;'#010+
  '  Line : String;'#010+
  '  I,P,PC : Integer;'#010+
  'begin'#010+
  '  L:=TStringList.Create;'#010+
  '  Try'#010+
  '    L.LoadFrom','Stream(S);'#010+
  '    // Fix lines.'#010+
  '    For I:=L.Count-1 downto 0 do'#010+
  '      begin'#010+
  '      Line:=L[I];'#010+
  '      P:=Pos('#039'='#039',Line);'#010+
  '      PC:=Pos('#039';'#039',Line);  // Comment line.'#010+
  '      If (P=0) or ((PC<>0) and (PC<P)) then'#010+
  '        L.Delete(I)'#010+
  '      else'#010+
  '      ','  L[i]:=Trim(System.Copy(Line,1,P-1)+'#039'='#039'+Trim(System.C'+
  'opy(Line,P+1,Length(Line)-P)));'#010+
  '      end;'#010+
  '    With L do'#010+
  '      begin'#010+
  '      FArchive:=Values[KeyArchive];'#010+
  '      FCompiler:=Values[KeyCompiler];'#010+
  '      FCopy:=Values[KeyCopy];'#010+
  '      FMkDir:','=Values[KeyMkDir];'#010+
  '      FMove:=Values[KeyMove];'#010+
  '      FRemove:=Values[KeyRemove];'#010+
  '      FRemoveDir:=Values[KeyRemoveDir];'#010+
  '      FRemoveTree:=Values[KeyRemoveTree];'#010+
  '      Self.Options:=OptionsToStringList(Values[KeyOptions]);'#010+
  '      Line:=Va','lues[KeyCPU];'#010+
  '      If (Line<>'#039#039') then'#010+
  '        FCompileTarget.CPU:=StringToCPU(Line);'#010+
  '      Line:=Values[KeyOS];'#010+
  '      If (Line<>'#039#039') then'#010+
  '        FCompileTarget.OS:=StringToOS(Line);'#010+
  '      Line:=Values[KeyMode];'#010+
  '      If (Line<>'#039#039') then'#010+
  '   ','     FMode:=StringToMode(Line);'#010+
  '      LocalUnitDir:=Values[KeyLocalUnitDir];'#010+
  '      GlobalUnitDir:=Values[KeyGlobalUnitDir];'#010+
  '      FPrefix:=Values[KeyPrefix];'#010+
  '      FBaseInstallDir:=Values[KeyBaseInstallDir];'#010+
  '      FUnitInstallDir:=Values[Ke','yUnitInstallDir];'#010+
  '      FBinInstallDir:=Values[KeyBinInstallDir];'#010+
  '      FLibInstallDir:=Values[KeyLibInstallDir];'#010+
  '      FDocInstallDir:=Values[KeyDocInstallDir];'#010+
  '      FExamplesInstallDir:=Values[KeyExamplesInstallDir];'#010+
  '      FInstallExampl','es:=(Upcase(Values[KeyInstallExamples])='#039'Y'#039+
  ');'#010+
  '      FBuildExamples:=(Upcase(Values[KeyBuildExamples])='#039'Y'#039');'#010+
  '      FSkipCrossPrograms:=(Upcase(Values[KeySkipCrossPrograms])='#039'Y'+
  #039');'#010+
  '      FSkipAllPrograms:=(Upcase(Values[KeySkipAllPrograms])='#039'Y',#039+
  ');'#010+
  '      FNoFPCCfg:=(Upcase(Values[KeyNoFPCCfg])='#039'Y'#039');'#010+
  '      FUseEnvironment:=(Upcase(Values[KeyUseEnv])='#039'Y'#039');'#010+
  #010+
  '      GlobalDictionary.AddVariable('#039'target'#039',Target);'#010+
  '      GlobalDictionary.AddVariable('#039'baseinstalldir'#039',BaseInstal'+
  'lDir);'#010+
  '     ',' GlobalDictionary.AddVariable('#039'prefix'#039',Prefix);'#010+
  '      GlobalDictionary.AddVariable('#039'bininstalldir'#039',BinInstallD'+
  'ir);'#010+
  '      GlobalDictionary.AddVariable('#039'libinstalldir'#039',LibInstallD'+
  'ir);'#010+
  '      end;'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  '{**********','*********************************************************'+
  '*********'#010+
  '                             TFPCDefaults'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'procedure TFPCDefaults.CompilerDefaults;'#010+
  'var'#010+
  '  BD : ','String;'#010+
  'begin'#010+
  '  inherited CompilerDefaults;'#010+
  #010+
  '  // Use the same algorithm as the compiler, see options.pas'#010+
  '{$ifdef Unix}'#010+
  '  BD:=FixPath(GetEnvironmentVariable('#039'FPCDIR'#039'), False);'#010+
  '  if BD='#039#039' then'#010+
  '    begin'#010+
  '      BD:='#039'/usr/local/lib/fpc/'#039'+FCompi','lerVersion;'#010+
  '      if not DirectoryExists(BD) and'#010+
  '         DirectoryExists('#039'/usr/lib/fpc/'#039'+FCompilerVersion) the'+
  'n'#010+
  '        BD:='#039'/usr/lib/fpc/'#039'+FCompilerVersion;'#010+
  '    end;'#010+
  '{$else unix}'#010+
  '  BD:=FixPath({$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUti','ls.GetEnv'+
  'ironmentVariable('#039'FPCDIR'#039'), False);'#010+
  '  if BD='#039#039' then'#010+
  '    begin'#010+
  '      BD:=ExtractFilePath(FCompiler)+'#039'..'#039';'#010+
  '      if not(DirectoryExists(BD+'#039'/units'#039')) and'#010+
  '         not(DirectoryExists(BD+'#039'/rtl'#039')) then'#010+
  '        BD:=FBaseInstallDir+'#039'..'#039';',#010+
  '    end;'#010+
  '{$endif unix}'#010+
  #010+
  '  // Where to install by default'#010+
  '  if (FBaseInstallDir='#039#039') and (FPrefix='#039#039') then'#010+
  '    BaseInstallDir:=BD;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                         ','   TCustomInstaller'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TCustomInstaller.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  Inherited;'#010+
  '  FPackageVariantSettings := TStringList.Create;'#010+
  '  FPackageVarian','ts := TFPList.Create;'#010+
  '  GlobalDictionary:=DictionaryClass.Create(Nil);'#010+
  '  AnalyzeOptions;'#010+
  '  GlobalDictionary.AddVariable('#039'baseinstalldir'#039',Defaults.BaseI'+
  'nstallDir);'#010+
  '  GlobalDictionary.AddVariable('#039'bininstalldir'#039',Defaults.BinIns'+
  'tallDir);'#010+
  '  Glo','balDictionary.AddVariable('#039'libinstalldir'#039',Defaults.LibI'+
  'nstallDir);'#010+
  '  GlobalDictionary.AddVariable('#039'Target'#039',Defaults.Target);'#010+
  '  GlobalDictionary.AddVariable('#039'BuildString'#039',Defaults.BuildStr'+
  'ing);'#010+
  '  GlobalDictionary.AddVariable('#039'Prefix'#039',Default','s.Prefix);'#010+
  '  GlobalDictionary.AddVariable('#039'CompilerVersion'#039',Defaults.Comp'+
  'ilerVersion);'#010+
  '  FNotifyEventCollection := TNotifyEventCollection.create([neaBeforeCo'+
  'mpile, neaAfterCompile, neaBeforeClean, neaAfterClean,'#010+
  '                            ','                               neaBefore'+
  'Install, neaAfterInstall, neaBeforeArchive, neaAfterArchive,'#010+
  '                                                           neaBeforeMa'+
  'nifest, neaAfterManifest, neaBeforePkgList, neaAfterPkgList,'#010+
  '        ','                                                   neaBefore'+
  'UnInstall, neaAfterUnInstall,'#010+
  '                                                           neaBeforeCr'+
  'eateBuildEngine, neaAfterCreateBuildengine,'#010+
  '                                    ','                       neaBefore'+
  'DocProject, neaAfterDocProject, neaBeforeDoc, neaAfterDoc]);'#010+
  '  CreatePackages;'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TCustomInstaller.destroy;'#010+
  'var'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  FreePackages;'#010+
  '  FreeAndNil(Defaults);'#010+
  '  FreeAndNil(GlobalDi','ctionary);'#010+
  '  FreeAndNil(FPackageVariantSettings);'#010+
  '  for i := 0 to FPackageVariants.Count-1 do'#010+
  '    begin'#010+
  '    if TPackageVariants(FPackageVariants.Items[i]).Owner=Self then'#010+
  '      TPackageVariants(FPackageVariants.Items[i]).Free;'#010+
  '    end;'#010+
  '  Fr','eeAndNil(FPackageVariants);'#010+
  '  FreeAndNil(FNotifyEventCollection);'#010+
  '  inherited destroy;'#010+
  'end;'#010+
  #010+
  'function TCustomInstaller.GetPackages: TPackages;'#010+
  'begin'#010+
  '  result := FPackages;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Log(Level: TVerboseLevel; const Msg',': Stri'+
  'ng);'#010+
  'begin'#010+
  '  If (Level in FLogLevels) or (ListMode and (level=vlCommand)) then'#010+
  '    begin'#010+
  '    Writeln(StdOut, Msg);'#010+
  '    Flush(StdOut);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.CreatePackages;'#010+
  'begin'#010+
  '  FPackages:=TPackages.Create(TPack','age);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.FreePackages;'#010+
  'begin'#010+
  '  FreeAndNil(FPackages);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.CreateBuildEngine;'#010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeCreateBuildEngine, Self);'#010+
  '  FBuildEngine:=TBuildEngin','e.Create(Self);'#010+
  '//  FBuildEngine.Defaults:=Defaults;'#010+
  '  FBuildEngine.ListMode:=FListMode;'#010+
  '  FBuildEngine.FInteractive:=FInteractive;'#010+
  '  FBuildEngine.Verbose := (vlInfo in FLogLevels) or (vlDebug in FLogLe'+
  'vels);'#010+
  '  FBuildEngine.OnLog:=@Self.Log',';'#010+
  '  NotifyEventCollection.CallEvents(neaAfterCreateBuildengine, Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Error(const Msg: String);'#010+
  'begin'#010+
  '  Raise EInstallerError.Create(Msg);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Error(const Fmt: String; const A','rgs: arra'+
  'y of const);'#010+
  'begin'#010+
  '  Raise EInstallerError.CreateFmt(Fmt,Args);'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomInstaller.AddPackage(const AName: String): TPackage;'#010+
  'begin'#010+
  '  result:=Packages.AddPackage(AName);'#010+
  '  AddAutoPackageVariantsToPackage(result);'#010+
  #010+
  '  i','f Defaults.UnitOutputDirectory <> '#039#039' then'#010+
  '    Result.FUnitsOutputDir := IncludeTrailingPathDelimiter(Defaults.Un'+
  'itOutputDirectory) + Result.FUnitsOutputDir;'#010+
  'end;'#010+
  #010+
  'function TCustomInstaller.AddPackageVariant(AName: string;'#010+
  '  AIsInheritable: ','boolean; AutoAddToPackage: Boolean): TPackageVaria'+
  'nts;'#010+
  'begin'#010+
  '  result := TPackageVariants.Create(TPackageVariant);'#010+
  '  result.Name:=AName;'#010+
  '  result.FIsInheritable:=AIsInheritable;'#010+
  '  result.FAutoAddToPackage:=AutoAddToPackage;'#010+
  '  FPackageVarian','ts.Add(result);'#010+
  'end;'#010+
  #010+
  'function TCustomInstaller.AddPackageFromDir(const AName: String; const'+
  ' aDirectory: String; const aMask: string): TPackage;'#010+
  'begin'#010+
  '  Result:=AddPackage(aName);'#010+
  '  if Result.AddTargetsFromDir(aDirectory,aMask)<=0 then'#010+
  '    ','Result.Free'#010+
  '  else'#010+
  '    Result.Dependencies.Clear;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.AnalyzeOptions;'#010+
  #010+
  '  Function CheckOption(Index : Integer;const Short,Long : String; AddT'+
  'oOptionString: boolean = false): Boolean;'#010+
  '  var'#010+
  '    O : String;'#010+
  '  begi','n'#010+
  '    O:=Paramstr(Index);'#010+
  '    Result:=(O='#039'-'#039'+short) or (O='#039'--'#039'+long) or (copy(O,'+
  '1,Length(Long)+3)=('#039'--'#039'+long+'#039'='#039'));'#010+
  '    if AddToOptionString and Result then'#010+
  '      FFPMakeOptionsString := FFPMakeOptionsString+'#039' '#039'+O;'#010+
  '  end;'#010+
  #010+
  '  Function CheckB','uildOptionSetValue(Index: Integer): boolean;'#010+
  '  var'#010+
  '    O : String;'#010+
  '    BuildModeName: string;'#010+
  '    P: integer;'#010+
  '    C: string;'#010+
  '    PV: TPackageVariants;'#010+
  '    SL: TStringList;'#010+
  '  begin'#010+
  '    O:=Paramstr(Index);'#010+
  '    result := O[1]='#039'+'#039';'#010+
  '    if resul','t then'#010+
  '      begin'#010+
  '      P:=Pos('#039'='#039',Paramstr(Index));'#010+
  '      If (P=0) then'#010+
  '        Error(SErrNeedArgument,[Index,O])'#010+
  '      else'#010+
  '        begin'#010+
  '        BuildModeName:=copy(o,2,P-2);'#010+
  '        Delete(O,1,P);'#010+
  #010+
  '        c := RightStr(BuildModeName,1',');'#010+
  '        if (c = '#039'*'#039') or (c = '#039'+'#039') then'#010+
  '          begin'#010+
  '          // Add a new package-variant'#010+
  '          BuildModeName := copy(BuildModeName, 1, length(BuildModeName'+
  ') -1);'#010+
  '          PV := AddPackageVariant(BuildModeName, (C = '#039'*'#039'), Tr'+
  'ue);',#010+
  '          SL := TStringList.Create;'#010+
  '          try'#010+
  '            SL.CommaText := O;'#010+
  '            if SL.Count=0 then'#010+
  '              Error(SErrExpectPkgVariant,[Index,ParamStr(Index)]);'#010+
  '            for P := 0 to SL.Count -1 do'#010+
  '              begin',#010+
  '              PV.Add(SL.ValueFromIndex[P]);'#010+
  '              end;'#010+
  '            FPackageVariantSettings.Values[BuildModeName] := SL.ValueF'+
  'romIndex[0];'#010+
  '          finally'#010+
  '            SL.Free;'#010+
  '          end;'#010+
  '          end'#010+
  '        else'#010+
  '          be','gin'#010+
  '          // Set the value of the package-variant.'#010+
  '          FPackageVariantSettings.Values[BuildModeName] := O;'#010+
  '          end;'#010+
  '        end;'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  '  Function CheckCustomOption(Index : Integer; out CustOptName: string)'+
  ': Bool','ean;'#010+
  '  var'#010+
  '    O : String;'#010+
  '    i : integer;'#010+
  '  begin'#010+
  '    result := false;'#010+
  '    CustOptName:='#039#039';'#010+
  '    O:=Paramstr(Index);'#010+
  '    if copy(O,1,2)<>'#039'--'#039' then'#010+
  '      Exit;'#010+
  '    i := pos('#039'='#039',O);'#010+
  '    if i=0 then'#010+
  '      Exit;'#010+
  '    O:=copy(O,3,i-3);'#010+
  '    CustO','ptName:=O;'#010+
  '    Result:=CustomFpmakeCommandlineOptions.IndexOfName(O)>-1;'#010+
  '    if Result then FFPMakeOptionsString := FFPMakeOptionsString+'#039' '+
  #039'+Paramstr(Index);'#010+
  '  end;'#010+
  #010+
  #010+
  '  Function CheckCommand(Index : Integer;const Short,Long : String): Bo'+
  'ole','an;'#010+
  '  var'#010+
  '    O : String;'#010+
  '  begin'#010+
  '    O:=Paramstr(Index);'#010+
  '    Result:=(O='#039'-'#039'+short) or (O=long);'#010+
  '  end;'#010+
  #010+
  '  Function OptionArg(Var Index : Integer; AddToOptionString: boolean ='+
  ' false) : String;'#010+
  '  Var'#010+
  '    P : Integer;'#010+
  '  begin'#010+
  '    if (Length(P','aramStr(Index))>1) and (Paramstr(Index)[2]<>'#039'-'#039+
  ') then'#010+
  '      begin'#010+
  '      If Index<ParamCount then'#010+
  '        begin'#010+
  '        Inc(Index);'#010+
  '        Result:=Paramstr(Index);'#010+
  '        if AddToOptionString then'#010+
  '          FFPMakeOptionsString := FFPMakeO','ptionsString+'#039' '#039'+R'+
  'esult;'#010+
  '        end'#010+
  '      else'#010+
  '        Error(SErrNeedArgument,[Index,ParamStr(Index)]);'#010+
  '      end'#010+
  '    else If length(ParamStr(Index))>2 then'#010+
  '      begin'#010+
  '      P:=Pos('#039'='#039',Paramstr(Index));'#010+
  '      If (P=0) then'#010+
  '        Error(S','ErrNeedArgument,[Index,ParamStr(Index)])'#010+
  '      else'#010+
  '        begin'#010+
  '        Result:=Paramstr(Index);'#010+
  '        Delete(Result,1,P);'#010+
  '        end;'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  '  function SplitSpaces(var SplitString: string) : string;'#010+
  '  var i : integer;'#010+
  '  be','gin'#010+
  '    i := pos('#039' '#039',SplitString);'#010+
  '    if i > 0 then'#010+
  '      begin'#010+
  '        result := copy(SplitString,1,i-1);'#010+
  '        delete(SplitString,1,i);'#010+
  '      end'#010+
  '    else'#010+
  '      begin'#010+
  '        result := SplitString;'#010+
  '        SplitString:='#039#039';'#010+
  '      end;'#010+
  ' ',' end;'#010+
  #010+
  '  Function CheckPackageVariantOptionSetValue(Index: Integer): boolean;'+
  #010+
  '  var'#010+
  '    O : String;'#010+
  '    OptionName: string;'#010+
  '    I,J: integer;'#010+
  '    P: Integer;'#010+
  '    PV: TPackageVariants;'#010+
  '  begin'#010+
  '    result := false;'#010+
  '    O:=Paramstr(Index);'#010+
  '   ',' if copy(O,1,2)<>'#039'--'#039' then'#010+
  '      Exit;'#010+
  '    P:=Pos('#039'='#039',Paramstr(Index));'#010+
  '    if P > 0 then'#010+
  '      begin'#010+
  '      OptionName:=copy(o,3,P-3);'#010+
  '      Delete(O,1,P);'#010+
  '      for I := 0 to FPackageVariants.Count -1 do'#010+
  '        begin'#010+
  '        PV := TObject','(FPackageVariants.Items[I]) as TPackageVariants'+
  ';'#010+
  '        for J := 0 to PV.Count-1 do'#010+
  '          begin'#010+
  '          if OptionName = '#039'options_'#039'+PV.Name+'#039'_'#039'+PV.It'+
  'ems[J].Name then'#010+
  '            begin'#010+
  '            result := true;'#010+
  '            while O <>',' '#039#039' do'#010+
  '              (PV.Items[J] as TPackageVariant).AddOption(SplitSpaces(O'+
  '));'#010+
  '            end;'#010+
  '          end;'#010+
  '        end;'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  '  DefaultsFileName : string;'#010+
  '  OptString : string;'#010+
  '  CustOptName : string;'#010+
  ' ',' UnitDirSet: Boolean;'#010+
  '  SearchPathSet: Boolean;'#010+
  'begin'#010+
  '  I:=0;'#010+
  '  FListMode:=False;'#010+
  '  UnitDirSet:=False;'#010+
  '  SearchPathSet:=False;'#010+
  '  FLogLevels:=DefaultMessages;'#010+
  '  While (I<ParamCount) do'#010+
  '    begin'#010+
  '    Inc(I);'#010+
  '    if CheckOption(I,'#039'v'#039','#039'verbose'#039,') then'#010+
  '      FLogLevels:=AllMessages'#010+
  '    else if CheckOption(I,'#039'I'#039','#039'interactive'#039') then'#010+
  '      FInteractive:=true'#010+
  '    else if CheckOption(I,'#039'd'#039','#039'debug'#039') then'#010+
  '      FLogLevels:=AllMessages+[vlDebug]'#010+
  '    else if CheckCommand(I,'#039'm'#039','#039'compile'#039') th','en'#010+
  '      FRunMode:=rmCompile'#010+
  '    else if CheckCommand(I,'#039'b'#039','#039'build'#039') then'#010+
  '      FRunMode:=rmBuild'#010+
  '    else if CheckCommand(I,'#039'i'#039','#039'install'#039') then'#010+
  '      FRunMode:=rmInstall'#010+
  '    else if CheckCommand(I,'#039'bi'#039','#039'buildinstall'#039') then'#010+
  '      FRunMode:=','rmBuildInstall'#010+
  '    else if CheckCommand(I,'#039'zi'#039','#039'zipinstall'#039') then'#010+
  '      FRunMode:=rmZipInstall'#010+
  '    else if CheckCommand(I,'#039'c'#039','#039'clean'#039') then'#010+
  '      FRunMode:=rmClean'#010+
  '    else if CheckCommand(I,'#039'dc'#039','#039'distclean'#039') then'#010+
  '      FRunMode:=rmDistClea','n'#010+
  '    else if CheckCommand(I,'#039'a'#039','#039'archive'#039') then'#010+
  '      FRunMode:=rmarchive'#010+
  '    else if CheckCommand(I,'#039'M'#039','#039'manifest'#039') then'#010+
  '      FRunMode:=rmManifest'#010+
  '    else if CheckCommand(I,'#039'l'#039','#039'pkglist'#039') then'#010+
  '      FRunMode:=rmPkgList'#010+
  '    else if Check','Command(I,'#039'u'#039','#039'uninstall'#039') then'#010+
  '      FRunMode:=rmUnInstall'#010+
  '    else if CheckCommand(I,'#039'in'#039','#039'info'#039') then'#010+
  '      FRunMode:=rmInfo'#010+
  '    else if CheckCommand(I,'#039'dp'#039','#039'fpdocproject'#039') then'#010+
  '      FRunMode:=rmDocProject'#010+
  '    else if CheckCommand(I,'#039'D'#039,','#039'doc'#039') then'#010+
  '      FRunMode:=rmDoc'#010+
  '    else if CheckOption(I,'#039'h'#039','#039'help'#039') then'#010+
  '      Usage('#039#039',[])'#010+
  '    else if Checkoption(I,'#039'C'#039','#039'cpu'#039') then'#010+
  '      Defaults.CPU:=StringToCPU(OptionArg(I))'#010+
  '    else if Checkoption(I,'#039'O'#039','#039'os'#039') then'#010+
  '      begin'#010+
  '  ','      Defaults.OS:=StringToOS(OptionArg(I));'#010+
  '        Defaults.ExplicitOSNone := OptionArg(I) = OSToString(osNone);'#010+
  '      end'#010+
  '    else if Checkoption(I,'#039't'#039','#039'target'#039') then'#010+
  '      Defaults.Target:=OptionArg(I)'#010+
  '    else if Checkoption(I,'#039's'#039','#039'sub','target'#039') then'#010+
  '      Defaults.SubTarget:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'lc'#039','#039'list-commands'#039') then'#010+
  '      FListMode:=True'#010+
  '    else if Checkoption(I,'#039'P'#039','#039'prefix'#039') then'#010+
  '      Defaults.Prefix:=OptionArg(I)'#010+
  '    else if Checkoption(I,'#039'n'#039','#039,'nofpccfg'#039') then'#010+
  '      Defaults.NoFPCCfg:=true'#010+
  '    else if Checkoption(I,'#039'zp'#039','#039'zipprefix'#039') then'#010+
  '      Defaults.ZipPrefix:=OptionArg(I)'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '    else if Checkoption(I,'#039'e'#039','#039'useenv'#039') then'#010+
  '      Defaults.UseEnvironment:=true',#010+
  '{$endif}'#010+
  '{$ifndef NO_THREADING}'#010+
  '    else if CheckOption(I,'#039'T'#039','#039'threads'#039') then'#010+
  '      Defaults.ThreadsAmount:=StrToIntDef(OptionArg(I),-1)'#010+
  '{$endif NO_THREADING}'#010+
  '    else if CheckOption(I,'#039'B'#039','#039'baseinstalldir'#039') then'#010+
  '      Defaults.BaseInstallD','ir:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'BI'#039','#039'bininstalldir'#039') then'#010+
  '      Defaults.BinInstallDir:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'LI'#039','#039'libinstalldir'#039') then'#010+
  '      Defaults.LibInstallDir:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'U'#039','#039,'unitinstalldir'#039') then'#010+
  '      Defaults.UnitInstallDir:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'UL'#039','#039'localunitdir'#039') then'#010+
  '      begin'#010+
  '        UnitDirSet:=true;'#010+
  '        // Do not warn against the combination of SearchPath and'#010+
  '        // localuni','tdir when they are the same. (This is done to be '+
  'able'#010+
  '        // to call both older and newer versions of fpmake)'#010+
  '        if SearchPathSet and (Defaults.LocalUnitDir<>OptionArg(I)) the'+
  'n'#010+
  '          Log(vlWarning,SWarnCombinedPathAndUDir);'#010+
  '   ','     Defaults.LocalUnitDir:=OptionArg(I)'#010+
  '      end'#010+
  '    else if CheckOption(I,'#039'UG'#039','#039'globalunitdir'#039') then'#010+
  '      begin'#010+
  '        UnitDirSet:=true;'#010+
  '        // Do not warn against the combination of SearchPath and'#010+
  '        // localunitdir when they',' are the same. (This is done to be '+
  'able'#010+
  '        // to call both older and newer versions of fpmake)'#010+
  '        if SearchPathSet and (Defaults.GlobalUnitDir<>OptionArg(I)) th'+
  'en'#010+
  '          Log(vlWarning,SWarnCombinedPathAndUDir);'#010+
  '        Defaults','.GlobalUnitDir:=OptionArg(I)'#010+
  '      end'#010+
  '    else if CheckOption(I,'#039'UO'#039','#039'unitoutputdir'#039') then'#010+
  '      Defaults.UnitOutputDirectory := OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'sp'#039','#039'searchpath'#039') then'#010+
  '      begin'#010+
  '        SearchPathSet:=true;'#010+
  '      ','  if UnitDirSet then'#010+
  '          Log(vlWarning,SWarnCombinedPathAndUDir);'#010+
  '        Defaults.SearchPath.Add(OptionArg(I, true));'#010+
  '      end'#010+
  '    else if CheckOption(I,'#039'o'#039','#039'options'#039', true) then'#010+
  '      begin'#010+
  '        OptString := OptionArg(I, true);'#010,
  '        while OptString <> '#039#039' do'#010+
  '          Defaults.AddOption(SplitSpaces(OptString));'#010+
  '      end'#010+
  '    else if CheckOption(I,'#039'r'#039','#039'compiler'#039') then'#010+
  '      Defaults.Compiler:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'f'#039','#039'config'#039') then'#010+
  '      Default','sFileName:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'ie'#039','#039'installexamples'#039') then'+
  #010+
  '      Defaults.InstallExamples:=true'#010+
  '    else if CheckOption(I,'#039'be'#039','#039'buildexamples'#039') then'#010+
  '      Defaults.BuildExamples:=true'#010+
  '    else if CheckOption(I,'#039'sap'#039','#039'ski','pallprograms'#039') t'+
  'hen'#010+
  '      Defaults.SkipAllPrograms:=true'#010+
  '    else if CheckOption(I,'#039'scp'#039','#039'skipcrossprograms'#039') t'+
  'hen'#010+
  '      Defaults.SkipCrossPrograms:=true'#010+
  '    else if CheckOption(I,'#039'bu'#039','#039'buildunit'#039') then'#010+
  '      Defaults.BuildMode:=bmBuildUnit',#010+
  '    else if CheckOption(I,'#039'io'#039','#039'ignoreinvalidoption'#039', '+
  'true) then'#010+
  '      Defaults.IgnoreInvalidOptions:=true'#010+
  '    else if CheckOption(I,'#039'df'#039','#039'doc-folder'#039') then'#010+
  '      Defaults.FPDocOutputDir:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'do'#039','#039'doc-op','tions'#039') then'#010+
  '      Defaults.FPDocOptions:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'sd'#039','#039'single-docfile'#039') then'#010+
  '      Defaults.SingleFPDocFile:=True'#010+
  '    else if CheckCommand(I,'#039'ns'#039','#039'namespaces'#039') then'#010+
  '      Defaults.Namespaces:=True'#010+
  '    else if',' CheckOption(I,'#039'fsp'#039','#039'fpunitsrcpath'#039') the'+
  'n'#010+
  '      Defaults.FPUnitSourcePath:=OptionArg(I)'#010+
  '    else if assigned(CustomFpmakeCommandlineOptions) and CheckCustomOp'+
  'tion(I,CustOptName) then'#010+
  '      begin'#010+
  '      if not assigned(CustomFpMakeCommandlin','eValues) then'#010+
  '        CustomFpMakeCommandlineValues := TUnsortedCompilerOptionsStrin'+
  'gList.Create;'#010+
  '      CustomFpMakeCommandlineValues.Values[CustOptName]:=OptionArg(I, '+
  'true)'#010+
  '      end'#010+
  '    else if (not CheckBuildOptionSetValue(I)) and (not C','heckPackageV'+
  'ariantOptionSetValue(I))'#010+
  '      and (not Defaults.IgnoreInvalidOptions) then'#010+
  '      begin'#010+
  '      Usage(SErrInValidArgument,[I,ParamStr(I)]);'#010+
  '      end;'#010+
  '    end;'#010+
  '  If DefaultsFileName<>'#039#039' then'#010+
  '    Defaults.LocalInit(DefaultsFileName',');'#010+
  '  Defaults.CompilerDefaults;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Usage(const FMT: String; const Args: array '+
  'of const);'#010+
  #010+
  '  Procedure LogCmd(const LC,Msg : String);'#010+
  '  begin'#010+
  '    Log(vlInfo,Format('#039'  %-12s    %s'#039',[LC,MSG]));'#010+
  '  end;'#010+
  #010+
  '  Procedur','e LogOption(const C,LC,Msg : String);'#010+
  '  begin'#010+
  '    Log(vlInfo,Format('#039'  -%s    --%-16s %s'#039',[C,LC,MSG]));'#010+
  '  end;'#010+
  #010+
  '  Procedure LogArgOption(const C,LC,Msg : String);'#010+
  '  begin'#010+
  '    if trim(c)='#039#039' then'#010+
  '      Log(vlInfo,Format('#039'       --%-20s %s'#039',[L','C+'#039'='#039'+'+
  'SValue,MSG]))'#010+
  '    else'#010+
  '      Log(vlInfo,Format('#039'  -%s    --%-20s %s'#039',[C,LC+'#039'='#039+
  '+SValue,MSG]));'#010+
  '  end;'#010+
  #010+
  'var'#010+
  '  i: Integer;'#010+
  'begin'#010+
  '  // Force the Usage to be displayed'#010+
  '  Include(FLogLevels,vlInfo);'#010+
  '  If (FMT<>'#039#039') then'#010+
  '    Log(vlInfo,Format','(Fmt,Args));'#010+
  '  Log(vlInfo,Format(SHelpUsage,[Paramstr(0)]));'#010+
  '  Log(vlInfo,SHelpCommand);'#010+
  '  LogCmd('#039'compile'#039',SHelpCompile);'#010+
  '  LogCmd('#039'build'#039',SHelpBuild);'#010+
  '  LogCmd('#039'install'#039',SHelpInstall);'#010+
  '  LogCmd('#039'buildinstall'#039',SHelpBuildInstall);'#010+
  '  LogCmd(',#039'uninstall'#039',SHelpUnInstall);'#010+
  '  LogCmd('#039'clean'#039',SHelpClean);'#010+
  '  LogCmd('#039'distclean'#039',SHelpDistclean);'#010+
  '  LogCmd('#039'archive'#039',SHelpArchive);'#010+
  '  LogCmd('#039'manifest'#039',SHelpManifest);'#010+
  '  LogCmd('#039'zipinstall'#039',SHelpZipInstall);'#010+
  '  LogCmd('#039'pkglist'#039',SHelpPkgList);',#010+
  '  LogCmd('#039'fpdocproject'#039',SHelpFPDocProject);'#010+
  '  LogCmd('#039'doc'#039',SHelpDoc);'#010+
  '  Log(vlInfo,SHelpCmdOptions);'#010+
  '  LogOption('#039'h'#039','#039'help'#039',SHelpHelp);'#010+
  '  LogOption('#039'lc'#039','#039'list-commands'#039',SHelpList);'#010+
  '  LogOption('#039'n'#039','#039'nofpccfg'#039',SHelpNoFPCCfg);'#010+
  '  LogOption('#039'v'#039,','#039'verbose'#039',SHelpVerbose);'#010+
  '  LogOption('#039'd'#039','#039'debug'#039',SHelpDebug);'#010+
  '  LogOption('#039'I'#039','#039'interactive'#039',SHelpInteractive);'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '  LogOption('#039'e'#039', '#039'useenv'#039', sHelpUseEnvironment);'#010+
  '{$endif}'#010+
  '  LogOption('#039'ie'#039','#039'installexamples'#039',SHelpInst','Examples'+
  ');'#010+
  '  LogOption('#039'be'#039','#039'buildexamples'#039',SHelpBuildExamples);'#010+
  '  LogOption('#039'bu'#039','#039'buildunit'#039',SHelpUseBuildUnit);'#010+
  '  LogOption('#039'sap'#039','#039'skipallprograms'#039',SHelpSkipAllProgs)'+
  ';'#010+
  '  LogOption('#039'scp'#039','#039'skipcrossprograms'#039',SHelpSkipCrossPr'+
  'ogs);'#010+
  '  LogOptio','n('#039'io'#039','#039'ignoreinvalidoption'#039',SHelpIgnoreIn'+
  'vOpt);'#010+
  '  LogArgOption('#039'C'#039','#039'cpu'#039',SHelpCPU);'#010+
  '  LogArgOption('#039'O'#039','#039'os'#039',SHelpOS);'#010+
  '  LogArgOption('#039's'#039','#039'subtarget'#039',SHelpSubTarget);'#010+
  '  LogArgOption('#039't'#039','#039'target'#039',SHelpTarget);'#010+
  '  LogArgOption('#039'P'#039','#039'prefix'#039',SHe','lpPrefix);'#010+
  '  LogArgOption('#039'B'#039','#039'baseinstalldir'#039',SHelpBaseInstalldi'+
  'r);'#010+
  '  LogArgOption('#039'BI'#039','#039'bininstalldir'#039',SHelpBaseInstalldi'+
  'r);'#010+
  '  LogArgOption('#039'LI'#039','#039'libinstalldir'#039',SHelpBaseInstalldi'+
  'r);'#010+
  '  LogArgOption('#039'UL'#039','#039'localunitdir'#039',SHelpLocalUnitdir);'+
  #010,
  '  LogArgOption('#039'UO'#039','#039'unitoutputdir'#039',SHelpOutpuUnitDir)'+
  ';'#010+
  '  LogArgOption('#039'UG'#039','#039'globalunitdir'#039',SHelpGlobalUnitdir'+
  ');'#010+
  '  LogArgOption('#039'sp'#039','#039'searchpath'#039',SHelpSearchPath);'#010+
  '  LogArgOption('#039'U'#039','#039'unitinstalldir'#039',SHelpUnitInstallDi'+
  'r);'#010+
  '  LogArgOption('#039'r'#039,','#039'compiler'#039',SHelpCompiler);'#010+
  '  LogArgOption('#039'f'#039','#039'config'#039',SHelpConfig);'#010+
  '  LogArgOption('#039'o'#039','#039'options'#039',SHelpOptions);'#010+
  '  LogArgOption('#039'df'#039', '#039'doc-folder'#039', sHelpFpdocOutputDir'+
  ');'#010+
  '  LogArgOption('#039'sd'#039','#039'single-docfile'#039', sHelpSingleFpdoc'+
  'File);'#010+
  '  LogArg','Option('#039'do'#039','#039'doc-options'#039', sHelpDocOptionsFi'+
  'le);'#010+
  '  LogArgOption('#039'fsp'#039', '#039'fpunitsrcpath'#039', sHelpFPUnitSrcP'+
  'ath);'#010+
  '  LogArgOption('#039'zp'#039', '#039'zipprefix'#039', sHelpZipPrefix);'#010+
  '{$ifndef NO_THREADING}'#010+
  '  LogArgOption('#039'T'#039', '#039'threads'#039', sHelpThreads);'#010+
  '{$endif NO_','THREADING}'#010+
  '  if assigned(CustomFpmakeCommandlineOptions) then for i  := 0 to Cust'+
  'omFpmakeCommandlineOptions.Count-1 do'#010+
  '    LogArgOption('#039' '#039',CustomFpmakeCommandlineOptions.Names[i],C'+
  'ustomFpmakeCommandlineOptions.ValueFromIndex[i]);'#010+
  '  Log(vlI','nfo, sHelpPackageVariant1);'#010+
  '  Log(vlInfo, sHelpPackageVariant2);'#010+
  '  Log(vlInfo, sHelpPackageVariant3);'#010+
  '  Log(vlInfo, sHelpPackageVariant4);'#010+
  '  Log(vlInfo, sHelpPackageVariant5);'#010+
  '  Log(vlInfo, sHelpPackageVariant6);'#010+
  '  Log(vlInfo,'#039#039');'#010+
  #010+
  '  If (FM','T<>'#039#039') then'#010+
  '    halt(1)'#010+
  '  else'#010+
  '    halt(0);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Info;'#010+
  'Var Cpu : TCpu;'#010+
  '    OS  : TOS;'#010+
  #010+
  'begin'#010+
  '  Write('#039'CPU_TARGET='#039');'#010+
  '  for cpu:=succ(low(cpu)) to high(tcpu) do // skip NONE'#010+
  '    begin'#010+
  '       write(cputostring(cpu)',');'#010+
  '       if cpu<>high(tcpu) then'#010+
  '       write('#039','#039');'#010+
  '    end;'#010+
  '  writeln;'#010+
  '  Write('#039'OS_TARGET='#039');'#010+
  '  for OS:=succ(low(TOS)) to high(tOS) do // skip NONE'#010+
  '    begin'#010+
  '       write(OStostring(os));'#010+
  '       if os<>high(tos) then'#010+
  '       write('#039','#039');'#010+
  '  ','  end;'#010+
  '  writeln;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Compile(Force: Boolean);'#010+
  'begin'#010+
  '  FNotifyEventCollection.CallEvents(neaBeforeCompile, Self);'#010+
  '  FBuildEngine.ForceCompile:=Force;'#010+
  '  FBuildEngine.Compile(Packages);'#010+
  '  FNotifyEventCollection.Ca','llEvents(neaAfterCompile, Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Clean(AllTargets: boolean);'#010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeClean, Self);'#010+
  '  BuildEngine.Clean(Packages, AllTargets);'#010+
  '  NotifyEventCollection.CallEvents(nea','AfterClean, Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Install(ForceBuild : Boolean);'#010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeInstall, self);'#010+
  '  BuildEngine.ForceCompile := ForceBuild;'#010+
  '  BuildEngine.Install(Packages);'#010+
  '  NotifyEventCo','llection.CallEvents(neaAfterInstall, self);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.ZipInstall;'#010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeInstall, self);'#010+
  '  BuildEngine.ZipInstall(Packages);'#010+
  '  NotifyEventCollection.CallEvents(neaAfterInstal','l, self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.UnInstall;'#010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeUnInstall, self);'#010+
  '  BuildEngine.UnInstall(Packages);'#010+
  '  NotifyEventCollection.CallEvents(neaAfterUnInstall, self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCus','tomInstaller.Archive;'#010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeArchive, self);'#010+
  '  BuildEngine.Archive(Packages);'#010+
  '  NotifyEventCollection.CallEvents(neaAfterArchive, self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Manifest;'#010+
  'begin'#010+
  '  NotifyEv','entCollection.CallEvents(neaBeforeManifest, self);'#010+
  '  BuildEngine.Manifest(Packages, nil);'#010+
  '  NotifyEventCollection.CallEvents(neaAfterManifest, self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.PkgList;'#010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBef','orePkgList, self);'#010+
  '  BuildEngine.PkgList(Packages);'#010+
  '  NotifyEventCollection.CallEvents(neaAfterPkgList, self);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.FPDocProject;'#010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeDocProject, self);'#010+
  '  BuildEngine','.FPDocProject(Packages,Defaults.SingleFPDocFile);'#010+
  '  NotifyEventCollection.CallEvents(neaAfterDocProject, self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Documentation;'#010+
  #010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeDoc, self);'#010+
  '  BuildEngine.Do','cumentation(Packages);'#010+
  '  NotifyEventCollection.CallEvents(neaAfterDoc, self);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.AddAutoPackageVariantsToPackage(APackage: T'+
  'Package);'#010+
  'var'#010+
  '  i: Integer;'#010+
  '  PV: TPackageVariants;'#010+
  'begin'#010+
  '  for i := 0 to FPackageVari','ants.Count-1 do'#010+
  '    begin'#010+
  '    PV := TObject(FPackageVariants.Items[i]) as TPackageVariants;'#010+
  '    if PV.AutoAddToPackage then'#010+
  '      APackage.AddPackageVariant(PV);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.CheckPackages;'#010+
  'begin'#010+
  '  If (Packages.','Count=0) then'#010+
  '    Error(SErrNoPackagesDefined);'#010+
  '  // Check for other obvious errors ?'#010+
  'end;'#010+
  #010+
  'Procedure TCustomInstaller.CheckNameSpaces;'#010+
  #010+
  'Var'#010+
  '  P : TPackage;'#010+
  #010+
  'begin'#010+
  '  for P in Packages do'#010+
  '    if P.NamespaceMap<>'#039#039' then'#010+
  '      begin'#010+
  '      Buil','dEngine.ResolveFileNames(P,Defaults.CPU,Defaults.OS,True,T'+
  'rue);'#010+
  '      P.ApplyNameSpaces(BuildEngine,Defaults.CompileTarget);'#010+
  '      end;'#010+
  'end;'#010+
  #010+
  'function TCustomInstaller.Run: Boolean;'#010+
  #010+
  #010+
  'begin'#010+
  '  Result:=True;'#010+
  '  try'#010+
  '    CheckPackages;'#010+
  '    Creat','eBuildEngine;'#010+
  '    if Defaults.Namespaces then'#010+
  '      CheckNameSpaces;'#010+
  '    Case RunMode of'#010+
  '      rmCompile : Compile(False);'#010+
  '      rmBuild   : Compile(True);'#010+
  '      rmInstall : Install(False);'#010+
  '      rmBuildInstall: Install(True);'#010+
  '      rmZipIn','stall : ZipInstall;'#010+
  '      rmArchive : Archive;'#010+
  '      rmClean    : Clean(False);'#010+
  '      rmDistClean: Clean(True);'#010+
  '      rmManifest : Manifest;'#010+
  '      rmPkgList : PkgList;'#010+
  '      rmUnInstall : UnInstall;'#010+
  '      rmInfo      : Info;'#010+
  '      rmDocProj','ect : FPDocProject;'#010+
  '      rmDoc : Documentation;'#010+
  '    end;'#010+
  '  except'#010+
  '    On E : Exception do'#010+
  '      begin'#010+
  '      Log(vlError,SErrInstaller);'#010+
  '      Log(vlError,E.Message);'#010+
  '      DumpExceptionBacktrace(stderr);'#010+
  '      Result:=False;'#010+
  '      end;'#010+
  '  e','nd;'#010+
  '  // Force returning an exitcode to the shell'#010+
  '  if not Result then'#010+
  '    ExitCode:=1;'#010+
  'end;'#010+
  #010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TFPCInstaller'#010+
  '********************','************************************************'+
  '********}'#010+
  #010+
  'constructor TFPCInstaller.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  if assigned(Defaults) then'#010+
  '    Error(SErrAlreadyInitialized);'#010+
  '  Defaults:=TFPCDefaults.Create;'#010+
  '  inherited Create(AOwne','r);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                 TBasicInstaller'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TBasicInsta','ller.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  if assigned(Defaults) then'#010+
  '    Error(SErrAlreadyInitialized);'#010+
  '  Defaults:=TBasicDefaults.Create;'#010+
  '  inherited Create(AOwner);'#010+
  'end;'#010+
  #010+
  #010+
  '{******************************************************************','*'+
  '*********'#010+
  '                                 TBuildEngine'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TBuildEngine.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  inherited Create(AOwner);'#010+
  '  // Maybe this s','hould be the current directory ?'#010+
  '  // Or have it as a command-line option.'#010+
  '  // Would allow to put all '#039'installers'#039' in one dir and call t'+
  'hem'#010+
  '  // With --start-dir=/path/to/sources.'#010+
  '  FStartDir:=includeTrailingPathDelimiter(GetCurrentDir);'#010+
  ' ',' FExternalPackages:=TPackages.Create(TPackage);'#010+
  '  FIndentCount:=0;'#010+
  '  FNotifyEventCollection := TNotifyEventCollection.create([neaAfterCom'+
  'pile, neaBeforeCompile, neaAfterInstall, neaBeforeInstall,'#010+
  '                                            ','               neaAfterC'+
  'lean, neaBeforeClean, neaAfterArchive, neaBeforeArchive,'#010+
  '                                                           neaAfterMan'+
  'ifest, neaBeforeManifest, neaAfterPkgList, neaBeforePkgList,'#010+
  '                            ','                               neaBefore'+
  'UnInstall, neaAfterUnInstall, neaBeforeDoc, neaAfterDoc]);'#010+
  '{$ifndef NO_THREADING}'#010+
  '  InitCriticalSection(FGeneralCriticalSection);'#010+
  '{$endif NO_THREADING}'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TBuildEngine.Destroy;'#010+
  'begin'#010+
  '  ','FreeAndNil(FExternalPackages);'#010+
  '  FreeAndNil(FNotifyEventCollection);'#010+
  '  If FIndentCount<>0 then'#010+
  '    Log(vlDebug,Format('#039'Log level at exit is %d'#039',[FIndentCount'+
  ']));'#010+
  #010+
  '{$ifndef NO_THREADING}'#010+
  '  DoneCriticalsection(FGeneralCriticalSection);'#010+
  '{$endi','f NO_THREADING}'#010+
  #010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.AddFileToArchive(const APackage: TPackage; cons'+
  't ASourceFileName, ADestFileName: String);'#010+
  #010+
  '  function GetArchiveName: string;'#010+
  '  begin'#010+
  '    result := Defaults.ZipPrefix;'#010+
  '    i','f Defaults.BuildOS in AllLimit83fsOses then'#010+
  '      result := result + APackage.ShortName'#010+
  '    else'#010+
  '      result := result + APackage.Name;'#010+
  '    result := result + MakeZipSuffix(Defaults.CPU, Defaults.OS);'#010+
  '  end;'#010+
  #010+
  '{$ifdef UNIX}'#010+
  'var'#010+
  '  FileStat: ','stat;'#010+
  '{$endif UNIX}'#010+
  'begin'#010+
  '{$ifdef CREATE_TAR_FILE}'#010+
  '  {$ifdef HAS_TAR_SUPPORT}'#010+
  '  if not assigned(FTarWriter) then'#010+
  '    begin'#010+
  '    {$ifdef HAS_UNIT_ZIPPER}'#010+
  '      FGZFileStream := TGZFileStream.create(GetArchiveName + ArchiveEx'+
  'tension, gzopenwri','te);'#010+
  '      try'#010+
  '        FTarWriter := TTarWriter.Create(FGZFileStream);'#010+
  '      except'#010+
  '        FGZFileStream.Free;'#010+
  '      end;'#010+
  '    {$else}'#010+
  '    FTarWriter := TTarWriter.Create(GetArchiveName + ArchiveExtension)'+
  ';'#010+
  '    {$endif HAS_UNIT_ZIPPER}'#010+
  '    ','FTarWriter.Permissions := [tpReadByOwner, tpWriteByOwner, tpRead'+
  'ByGroup, tpReadByOther];'#010+
  '    FTarWriter.UserName := '#039'root'#039';'#010+
  '    FTarWriter.GroupName := '#039'root'#039';'#010+
  '    end;'#010+
  '{$ifdef unix}'#010+
  '  filestat:=Default(stat);'#010+
  '  if (FpStat(ASourceFileName, ','FileStat) = 0) and (FileStat.st_mode a'+
  'nd S_IXUSR = S_IXUSR) then'#010+
  '    begin'#010+
  '    FTarWriter.Permissions := FTarWriter.Permissions + [tpExecuteByGro'+
  'up];'#010+
  '    FTarWriter.Permissions := FTarWriter.Permissions + [tpExecuteByOwn'+
  'er];'#010+
  '    FTarWriter.','Permissions := FTarWriter.Permissions + [tpExecuteByO'+
  'ther];'#010+
  '    end'#010+
  '  else'#010+
  '    begin'#010+
  '    FTarWriter.Permissions := FTarWriter.Permissions - [tpExecuteByGro'+
  'up];'#010+
  '    FTarWriter.Permissions := FTarWriter.Permissions - [tpExecuteByOwn'+
  'er];'#010+
  '    F','TarWriter.Permissions := FTarWriter.Permissions - [tpExecuteByO'+
  'ther];'#010+
  '    end;'#010+
  '{$endif unix}'#010+
  '  FTarWriter.AddFile(ASourceFileName, ADestFileName);'#010+
  '  {$endif HAS_TAR_SUPPORT}'#010+
  '{$else CREATE_TAR_FILE}'#010+
  '  {$ifdef HAS_UNIT_ZIPPER}'#010+
  '  if not assign','ed(FZipper) then'#010+
  '    begin'#010+
  '      FZipper := TZipper.Create;'#010+
  '      FZipper.FileName := GetArchiveName + ArchiveExtension;'#010+
  '    end;'#010+
  #010+
  '  FZipper.Entries.AddFileEntry(ASourceFileName, ADestFileName);'#010+
  '  {$endif HAS_UNIT_ZIPPER}'#010+
  '{$ENDIF CREATE_TAR','_FILE}'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.FinishArchive(Sender: TObject);'#010+
  'begin'#010+
  '  {$ifdef HAS_TAR_SUPPORT}'#010+
  '  if assigned(FTarWriter) then'#010+
  '    begin'#010+
  '      FreeAndNil(FTarWriter);'#010+
  '      {$ifdef HAS_UNIT_ZIPPER}'#010+
  '      FGZFileStream.Free;'#010+
  '      {$end','if HAS_UNIT_ZIPPER}'#010+
  '    end;'#010+
  '  {$endif HAS_TAR_SUPPORT}'#010+
  '  {$ifdef HAS_UNIT_ZIPPER}'#010+
  '  if assigned(FZipper) then'#010+
  '    begin'#010+
  '      try'#010+
  '        FZipper.ZipAllFiles;'#010+
  '        FZipper.Clear;'#010+
  '      finally'#010+
  '        FreeAndNil(FZipper);'#010+
  '      end;'#010+
  '   ',' end;'#010+
  '  {$endif HAS_UNIT_ZIPPER}'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Error(const Msg: String);'#010+
  'begin'#010+
  '  Raise EInstallerError.Create(Msg);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Error(const Fmt: String; const Args: array of c'+
  'onst);'#010+
  'begin'#010+
  '  Raise EInstaller','Error.CreateFmt(Fmt,Args);'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.GetExecuteCommandOutput(const Cmd : String; cons'+
  't Args : TStrings; Env: TStrings = nil; IgnoreError : Boolean = False)'+
  ' : string;'#010+
  'Var'#010+
  '  E : Integer;'#010+
  '  cmdLine: string;'#010+
  '  ConsoleOutput: ','TMemoryStream;'#010+
  '  s: string;'#010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  cmdLine:='#039#039';'#010+
  '  if Args.Count<>0 then'#010+
  '    for s in Args do'#010+
  '      cmdLine := cmdline + '#039' '#039' + S;'#010+
  '  Log(vlInfo,SInfoExecutingCommand,[Cmd,CmdLine]);'#010+
  '  if ListMode then'#010+
  '    Log(vlCommand,'#039'%s %s'#039',','[Cmd,CmdLine])'#010+
  '  else'#010+
  '    begin'#010+
  '      // We should check cmd for spaces, and move all after first spac'+
  'e to args.'#010+
  '      ConsoleOutput := TMemoryStream.Create;'#010+
  '      try'#010+
  '        if Interactive then'#010+
  '          Log(vlInfo,'#039'Starting "%s" "%s" int','eractively'#039',[Cmd'+
  ',CmdLine]);'#010+
  '        {$ifdef HAS_UNIT_PROCESS}'#010+
  '        E:=ExecuteFPC(Verbose, Interactive, cmd, args, env, ConsoleOut'+
  'put);'#010+
  '        {$else}'#010+
  '        E:=ExecuteProcess(cmd,args.ToStringArray);'#010+
  '        {$endif}'#010+
  '        If (E<>0) ','and (not IgnoreError) then'#010+
  '          begin'#010+
  '            s := ParsecompilerOutput(ConsoleOutput,Verbose);'#010+
  '            cmdLine:=cmd+'#039' '#039'+cmdLine;'#010+
  '            Error(SErrExternalCommandFailed,[cmdLine,E,s]);'#010+
  '          end;'#010+
  '      finally'#010+
  '        C','onsoleOutput.Seek(0, soBeginning);'#010+
  '        SetLength(Result,ConsoleOutput.Size);'#010+
  '        if ConsoleOutput.Size>0 then'#010+
  '          ConsoleOutput.Read(Result[1],ConsoleOutput.Size);'#010+
  '        ConsoleOutput.Free;'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedur','e TBuildEngine.ExecuteCommand(const Cmd : String; const Args'+
  ' : TStrings;  Env: TStrings = nil; IgnoreError : Boolean = False);'#010+
  'begin'#010+
  '  GetExecuteCommandOutput(Cmd,Args,Env,IgnoreError);'#010+
  'end;'#010+
  #010+
  'function TBuildEngine.SysDirectoryExists(const A','Dir: string): Boolea'+
  'n;'#010+
  'begin'#010+
  '  result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.DirectoryExi'+
  'sts(ADir);'#010+
  '  if result then'#010+
  '    Log(vlDebug,SDbgDirectoryExists,[ADir,SDbgFound])'#010+
  '  else'#010+
  '    Log(vlDebug,SDbgDirectoryExists,[ADir,SDbgNotFou','nd]);'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.SysFileExists(const AFileName: string): Boolean;'+
  #010+
  'begin'#010+
  '  // Writeln('#039'Testing : '#039',aFileName);'#010+
  '  result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.FileExists(A'+
  'FileName);'#010+
  '  if result then'#010+
  '    Log(vlDebu','g,SDbgFileExists,[AFileName,SDbgFound])'#010+
  '  else'#010+
  '    Log(vlDebug,SDbgFileExists,[AFileName,SDbgNotFound]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.SysCopyFile(const Src, Dest: String);'#010+
  'Var'#010+
  '  D,S : String;'#010+
  '  Fin,FOut : TFileStream;'#010+
  '  Count : Int64;'#010+
  '  A ',': Int64;'#010+
  '{$ifdef UNIX}'#010+
  '  FileStat: stat;'#010+
  '{$endif UNIX}'#010+
  'begin'#010+
  '  { First delete file on Darwin OS to avoid codesign issues }'#010+
  '  if (Defaults.SourceOS=Darwin) then'#010+
  '    begin'#010+
  '      D:=IncludeTrailingPathDelimiter(Dest);'#010+
  '      If DirectoryExists(','D) then'#010+
  '        begin'#010+
  '          D:=D+ExtractFileName(Src);'#010+
  '          if FileExists(D) then'#010+
  '            SysDeleteFile(D);'#010+
  '       end'#010+
  '     else if FileExists(Dest) then'#010+
  '       SysDeleteFile(Dest);'#010+
  '    end;'#010+
  '  Log(vlInfo,SInfoCopyingFile,[Src,D','est]);'#010+
  '  FIn:=TFileStream.Create(Src,fmopenRead or fmShareDenyNone);'#010+
  '  Try'#010+
  '    D:=IncludeTrailingPathDelimiter(Dest);'#010+
  '    If DirectoryExists(D) then'#010+
  '      S:=D+ExtractFileName(Src)'#010+
  '    else'#010+
  '      S:=Dest;'#010+
  '    FOut:=TFileStream.Create(S,fmCr','eate or fmShareDenyNone);'#010+
  '    Try'#010+
  '      Count:=Fout.CopyFrom(FIn,0);'#010+
  '      If (Count<>Fin.Size) then'#010+
  '        Error(SErrCopyingFile,[Src,S]);'#010+
  '    Finally'#010+
  '      FreeAndNil(Fout);'#010+
  '    end;'#010+
  '    A:=FileGetDate(FIn.Handle);'#010+
  '    If (A=-1) then'#010+
  '   ','   log(vlWarning,SWarnFailedToGetTime,[Src])'#010+
  '    else'#010+
  '      if FileSetDate(S,A)<>0 then'#010+
  '        Log(vlWarning,SWarnFailedToSetTime,[S]);'#010+
  '{$ifdef UNIX}'#010+
  '    // Copy the file-access rights on Unix, especially the executable-'+
  'bit'#010+
  '    filestat:=D','efault(stat);'#010+
  '    if FpStat(Src,FileStat) <> 0 then'#010+
  '      Log(vlWarning,SWarnCanNotGetAccessRights,[Src])'#010+
  '    else'#010+
  '      if FpChmod(s,FileStat.st_mode) <> 0 then'#010+
  '        Log(vlWarning,SWarnCanNotSetAccessRights,[S]);'#010+
  '{$endif UNIX}'#010+
  '  finally',#010+
  '    FreeAndNil(Fin);'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.SysMoveFile(const Src, Dest: String);'#010+
  'Var'#010+
  '  S : String;'#010+
  'begin'#010+
  '  If DirectoryExists(IncludeTrailingPathDelimiter(Dest)) then'#010+
  '    S:=IncludeTrailingPathDelimiter(Dest)+ExtractFileName','(Src)'#010+
  '  else'#010+
  '    S:=Dest;'#010+
  '  If Not RenameFile(Src,S) then'#010+
  '    begin'#010+
  '      Try'#010+
  '        SysCopyFile(Src,S);'#010+
  '        SysDeleteFile(Src);'#010+
  '      Except'#010+
  '        On E : Exception Do'#010+
  '          Error(SErrMovingFile,[Src,S]);'#010+
  '      end;'#010+
  '    end;'#010+
  'end;',#010+
  #010+
  #010+
  'procedure TBuildEngine.SysDeleteFile(const AFileName: String);'#010+
  'var retries : integer;'#010+
  '    res : boolean;'#010+
  'begin'#010+
  '  if not FileExists(AFileName) then'#010+
  '    Log(vldebug,SDbgFileDoesNotExist,[AFileName])'#010+
  '  else'#010+
  '    begin'#010+
  '      retries := 2;'#010+
  '   ','   res := {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.DeleteF'+
  'ile(AFileName);'#010+
  '      while not res and (retries>0) do'#010+
  '        begin'#010+
  '           log(vlWarning, SWarnRetryDeleteFile, [AFileName]);'#010+
  '           sleep(5000);'#010+
  '           dec(retri','es);'#010+
  '           res := {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.Dele'+
  'teFile(AFileName);'#010+
  '        end;'#010+
  '     if not res then'#010+
  '       Error(SErrDeletingFile,[AFileName])'#010+
  '     else'#010+
  '       Log(vlInfo,SInfoDeletedFile,[AFileName]);'#010+
  '   end;'#010+
  'en','d;'#010+
  #010+
  'procedure TBuildEngine.SysDeleteDirectory(const ADirectoryName: String'+
  ');'#010+
  'begin'#010+
  '  if not DirectoryExists(ADirectoryName) then'#010+
  '    Log(vldebug,SDbgDirectoryDoesNotExist,[ADirectoryName])'#010+
  '  else if not IsDirectoryEmpty(ADirectoryName) then',#010+
  '    Log(vldebug,SDbgDirectoryNotEmpty,[ADirectoryName])'#010+
  '  else If Not RemoveDir(ADirectoryName) then'#010+
  '    Error(SErrRemovingDirectory,[ADirectoryName])'#010+
  '  else'#010+
  '    Log(vlInfo,SInfoRemovedDirectory,[ADirectoryName]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEn','gine.SysDeleteTree(const ADirectoryName: String);'#010+
  #010+
  '  function IntRemoveTree(const ADirectoryName: String) : boolean;'#010+
  '{$ifdef MSWINDOWS}'#010+
  '  { pulling in shellapi with all it dependent units and packages makes'+
  ' things too'#010+
  '    complicated so jus','t add the constants here }'#010+
  '  const'#010+
  '    FO_DELETE                = $0003;'#010+
  '    FOF_SILENT               = $0004;'#010+
  '    FOF_NOCONFIRMATION       = $0010;'#010+
  '{$endif MSWINDOWS}'#010+
  '  var'#010+
  '    retries: integer;'#010+
  '{$ifdef MSWINDOWS}'#010+
  '    SHFileOpStruct: TSHFi','leOpStruct;'#010+
  '    DirBuf: array[0..MAX_PATH+1] of TCHAR;'#010+
  '{$else MSWINDOWS}'#010+
  '    searchRec: TSearchRec;'#010+
  '    SearchResult: longint;'#010+
  '    s: string;'#010+
  '{$endif MSWINDOWS}'#010+
  #010+
  '  begin'#010+
  '    result := true;'#010+
  '{$ifdef MSWINDOWS}'#010+
  '    retries:=2;'#010+
  '    try'#010+
  '      F','illChar(SHFileOpStruct, Sizeof(SHFileOpStruct), 0);'#010+
  '      FillChar(DirBuf, Sizeof(DirBuf), 0);'#010+
  '      StrPCopy(DirBuf, ADirectoryName);'#010+
  '      with SHFileOpStruct do'#010+
  '      begin'#010+
  '        pFrom := @DirBuf;'#010+
  '        wFunc := FO_DELETE;'#010+
  '        fF','lags := FOF_NOCONFIRMATION or FOF_SILENT;'#010+
  '      end;'#010+
  '      Result := SHFileOperation(SHFileOpStruct) = 0;'#010+
  '    except'#010+
  '      Result := False;'#010+
  '    end;'#010+
  '    while not result and (retries>0) do'#010+
  '      begin'#010+
  '        log(vlWarning, SWarnRetryRemDir','ectory, [ADirectoryName]);'#010+
  '        sleep(5000);'#010+
  '        dec(retries);'#010+
  '        result := SHFileOperation(SHFileOpStruct) = 0;;'#010+
  '      end;'#010+
  #010+
  '{$else MSWINDOWS}'#010+
  '    SearchResult := FindFirst(IncludeTrailingPathDelimiter(ADirectoryN'+
  'ame)+AllFilesM','ask, faAnyFile+faSymLink, searchRec);'#010+
  '    try'#010+
  '      while SearchResult=0 do'#010+
  '        begin'#010+
  '          if (searchRec.Name<>'#039'.'#039') and (searchRec.Name<>'#039'.'+
  '.'#039') then'#010+
  '             begin'#010+
  '               s := IncludeTrailingPathDelimiter(ADirectoryName)','+sea'+
  'rchRec.Name;'#010+
  '               if (searchRec.Attr and faDirectory)=faDirectory then'#010+
  '                 begin'#010+
  '                   if not IntRemoveTree(s) then'#010+
  '                     result := false;'#010+
  '                 end'#010+
  '               else if no','t DeleteFile(s) then'#010+
  '                 result := False'#010+
  '               else'#010+
  '                 log(vldebug, SDbgDeletedFile, [s]);'#010+
  '             end;'#010+
  '          SearchResult := FindNext(searchRec);'#010+
  '        end;'#010+
  '    finally'#010+
  '      FindClose(searchR','ec);'#010+
  '    end;'#010+
  #010+
  '    // There were reports of RemoveDir failing due to locking-problems'+
  '. To solve'#010+
  '    // these the RemoveDir is tried three times, with a delay of 5 sec'+
  'onds. See'#010+
  '    // bug 21868'#010+
  '    retries := 2;'#010+
  '    result := RemoveDir(ADire','ctoryName);'#010+
  '    while not result and (retries>0) do'#010+
  '      begin'#010+
  '        log(vlWarning, SWarnRetryRemDirectory, [ADirectoryName]);'#010+
  '        sleep(5000);'#010+
  '        dec(retries);'#010+
  '        result := RemoveDir(ADirectoryName);'#010+
  '      end;'#010+
  #010+
  '{$endif WI','NDOWS}'#010+
  #010+
  '    if result then'#010+
  '      log(vldebug, SDbgRemovedDirectory, [ADirectoryName]);'#010+
  '  end;'#010+
  #010+
  'begin'#010+
  '  if not DirectoryExists(ADirectoryName) then'#010+
  '    Log(vldebug,SDbgDirectoryDoesNotExist,[ADirectoryName])'#010+
  '  else If Not IntRemoveTree(ADire','ctoryName) then'#010+
  '    Error(SErrRemovingDirectory,[ADirectoryName])'#010+
  '  else'#010+
  '    Log(vlInfo,SInfoRemovedDirectory,[ADirectoryName]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.SysArchiveFiles(List: TStrings; const AFileName'+
  ': String);'#010+
  'begin'#010+
  '  If Not (Assigne','d(OnArchivefiles) or Assigned(ArchiveFilesProc)) th'+
  'en'#010+
  '    Raise EInstallerError.Create(SErrNoArchiveSupport);'#010+
  '  If Assigned(ArchiveFilesProc) then'#010+
  '    ArchiveFilesProc(AFileName,List)'#010+
  '  else'#010+
  '    OnArchiveFiles(AFileName,List);'#010+
  'end;'#010+
  #010+
  #010+
  'proced','ure TBuildEngine.LogIndent;'#010+
  'begin'#010+
  '{$ifndef NO_THREADING}'#010+
  '  EnterCriticalSection(FGeneralCriticalSection);'#010+
  '{$endif NO_THREADING}'#010+
  '  Inc(FIndentCount);'#010+
  '  if not (vlDebug in Installer.FLogLevels) then'#010+
  '    GLogPrefix:=GLogPrefix+'#039'  '#039#010+
  '  else'#010+
  '    ','GLogPrefix:=IntToStr(FIndentCount)+'#039'> '#039';'#010+
  '{$ifndef NO_THREADING}'#010+
  '  LeaveCriticalSection(FGeneralCriticalSection);'#010+
  '{$endif NO_THREADING}'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.LogUnIndent;'#010+
  'begin'#010+
  '{$ifndef NO_THREADING}'#010+
  '  EnterCriticalSection(FGeneralCr','iticalSection);'#010+
  '{$endif NO_THREADING}'#010+
  '  Dec(FIndentCount);'#010+
  '  if not (vlDebug in Installer.FLogLevels) then'#010+
  '    Delete(GLogPrefix,1,2)'#010+
  '  else'#010+
  '    GLogPrefix:=IntToStr(FIndentCount)+'#039'> '#039';'#010+
  '{$ifndef NO_THREADING}'#010+
  '  LeaveCriticalSection(FGeneral','CriticalSection);'#010+
  '{$endif NO_THREADING}'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Log(Level: TVerboseLevel; Msg: String);'#010+
  'begin'#010+
  '  If Assigned(FOnLog) then'#010+
  '    begin'#010+
  '{$ifndef NO_THREADING}'#010+
  '      EnterCriticalSection(FGeneralCriticalSection);'#010+
  '      try'#010+
  '{','$endif NO_THREADING}'#010+
  '      if (Level in [vlInfo,vlDebug])'#010+
  '         or (ListMode and (level=vlCommand)) then'#010+
  '        FOnLog(Level,GLogPrefix+Msg)'#010+
  '      else'#010+
  '        FOnLog(Level,Msg);'#010+
  '{$ifndef NO_THREADING}'#010+
  '      finally'#010+
  '        LeaveCritica','lSection(FGeneralCriticalSection);'#010+
  '      end;'#010+
  '{$endif NO_THREADING}'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Log(Level: TVerboseLevel; Fmt: String;const Arg'+
  's: array of const);'#010+
  'begin'#010+
  '  Log(Level,Format(Fmt,Args));'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine','.EnterDir(ADir: String);'#010+
  'Var'#010+
  '  D : String;'#010+
  'begin'#010+
  '  D:=FStartDir;'#010+
  '  D:=D+ADir;'#010+
  '  Log(vlDebug,SDbgEnterDir,[D]);'#010+
  '  If Not SetCurrentDir(D) then'#010+
  '    Error(SErrChangeDirFailed,[D]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CmdCopyFiles(List: TStrings; con','st DestDir: S'+
  'tring;'#010+
  '  APackage: TPackage);'#010+
  #010+
  'Var'#010+
  '  Args : TStrings;'#010+
  '  I : Integer;'#010+
  '  DestFileName : String;'#010+
  'begin'#010+
  '  // When the files should be written to an archive, add them'#010+
  '  if assigned(FOnCopyFile) then'#010+
  '    begin'#010+
  '      For I:=0 to List.','Count-1 do'#010+
  '        if List.Names[i]<>'#039#039' then'#010+
  '          begin'#010+
  '            if IsRelativePath(list.ValueFromIndex[i]) then'#010+
  '              DestFileName:=DestDir+list.ValueFromIndex[i]'#010+
  '            else'#010+
  '              DestFileName:=list.ValueFromIn','dex[i];'#010+
  '            FOnCopyFile(APackage, AddPathPrefix(APackage, List.Names[i'+
  ']), DestFileName);'#010+
  '          end'#010+
  '        else'#010+
  '          FOnCopyFile(APackage, AddPathPrefix(APackage, List[i]), Dest'+
  'Dir+ExtractFileName(List[i]));'#010+
  '      Exit;'#010+
  '   ',' end;'#010+
  #010+
  '  // Copy the files to their new location on disk'#010+
  '  CmdCreateDir(DestDir);'#010+
  '  If (Defaults.Copy<>'#039#039') then'#010+
  '    begin'#010+
  '      Args:=TStringList.Create;'#010+
  '      try'#010+
  '        Args.Assign(List);'#010+
  '        PrependFileListWithString(Args, IncludeTr','ailingPathDelimiter'+
  '(GPathPrefix));'#010+
  '        Args.Add(DestDir);'#010+
  '        ExecuteCommand(Defaults.Copy,Args);'#010+
  '      finally'#010+
  '        Args.Free;'#010+
  '      end;'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      if List.Names[i]<>'#039#039' then'#010+
  '        beg','in'#010+
  '          if IsRelativePath(list.ValueFromIndex[i]) then'#010+
  '            DestFileName:=DestDir+list.ValueFromIndex[i]'#010+
  '          else'#010+
  '            DestFileName:=list.ValueFromIndex[i];'#010+
  '          CmdCreateDir(ExtractFilePath(DestFileName));'#010+
  '   ','       SysCopyFile(AddPathPrefix(APackage, List.Names[i]),DestFil'+
  'eName)'#010+
  '        end'#010+
  '      else if List[i]<>'#039#039' then'#010+
  '        SysCopyFile(AddPathPrefix(APackage, List[i]), DestDir);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CmdCreateDir(const DestDir: Str','ing);'#010+
  #010+
  'Var'#010+
  '  Args : TStrings;'#010+
  #010+
  'begin'#010+
  '  If (Defaults.MkDir<>'#039#039') then'#010+
  '    begin'#010+
  '    Args:=TstringList.Create;'#010+
  '    try'#010+
  '      Args.Append('#039'-p'#039'); // Same effect as forcedirectories'#010+
  '      Args.Append(DestDir);'#010+
  '      ExecuteCommand(Defaults.MkDir,','Args);'#010+
  '    finally'#010+
  '      Args.Free;'#010+
  '    end;'#010+
  '    end'#010+
  '  else'#010+
  '    If not ForceDirectories(DestDir) then'#010+
  '      Error(SErrCreatingDirectory,[DestDir]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CmdMoveFiles(List: TStrings; const DestDir: Str'+
  'ing);'#010+
  'Var'#010+
  '  Arg','s : TStrings;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  CmdCreateDir(DestDir);'#010+
  '  If (Defaults.Move<>'#039#039') then'#010+
  '    begin'#010+
  '      Args:=TStringList.Create;'#010+
  '      try'#010+
  '        Args.Assign(List);'#010+
  '        Args.Add(DestDir);'#010+
  '        ExecuteCommand(Defaults.Move,Args);'#010,
  '      finally'#010+
  '        Args.Free;'#010+
  '      end;'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      SysMoveFile(List[i],DestDir);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CmdDeleteFiles(List: TStrings);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  If (Defaults.Remove<>'#039#039')',' then'#010+
  '    begin'#010+
  '    ExecuteCommand(Defaults.Remove,List);'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      SysDeleteFile(List[i]);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.CmdDeleteDestFiles(List: TStrings; const DestDi'+
  'r: String'#010+
  '  );'#010+
  #010+
  'Var'#010+
  '  I : Int','eger;'#010+
  '  DeleteFileName : String;'#010+
  'begin'#010+
  '  // Delete files from their location on disk'#010+
  '  For I:=0 to List.Count-1 do'#010+
  '    begin'#010+
  '      if List.Names[i]<>'#039#039' then'#010+
  '        begin'#010+
  '          if IsRelativePath(list.ValueFromIndex[i]) then'#010+
  '            ','DeleteFileName:=DestDir+list.ValueFromIndex[i]'#010+
  '          else'#010+
  '            DeleteFileName:=list.ValueFromIndex[i];'#010+
  '        end'#010+
  '      else'#010+
  '        DeleteFileName:=DestDir+ExtractFileName(list[i]);'#010+
  '      SysDeleteFile(DeleteFileName);'#010+
  '    end;',#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CmdArchiveFiles(List: TStrings; const ArchiveFi'+
  'le: String'#010+
  '  );'#010+
  #010+
  'Var'#010+
  '  P : Integer;'#010+
  '  CmdLine : Array of String;'#010+
  '  i : integer;'#010+
  '  S,Cmd : String;'#010+
  #010+
  '  Files,Args : TStrings;'#010+
  #010+
  'begin'#010+
  '  If (Defaults.Archive='#039#039') then',#010+
  '    SysArchiveFiles(List,ArchiveFile)'#010+
  '  else'#010+
  '    begin'#010+
  '      Files:=Nil;'#010+
  '      Args:=TStringList.Create;'#010+
  '      try'#010+
  '        Files:=TStringList.Create;'#010+
  '        Files.Assign(List);'#010+
  '        PrependFileListWithString(Files,IncludeTrailingPathDe','limiter'+
  '(GPathPrefix));'#010+
  '        CmdLine:=Defaults.Archive.Split(RTLString('#039' '#039'),'#039'"'#039+
  ','#039'"'#039');'#010+
  '        Cmd:=CmdLine[0];'#010+
  '        For I:=1 to Length(CmdLine)-1 do'#010+
  '          begin'#010+
  '          S:=CmdLine[i];'#010+
  '          P:=Pos('#039'$(FILESORDIRS)'#039',S);'#010+
  '       ','   if (P<>0) then'#010+
  '            Args.AddStrings(Files)'#010+
  '          else'#010+
  '            Args.Append(GlobalDictionary.Substitute(S,['#039'ARCHIVE'#039+
  ',ArchiveFile]));'#010+
  '          end;'#010+
  '        ExecuteCommand(Cmd,Args);'#010+
  '      finally'#010+
  '        List.Free;'#010+
  '        A','rgs.Free;'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.CmdRenameFile(const SourceName, DestName: Strin'+
  'g);'#010+
  #010+
  'var'#010+
  '  Args: TStrings;'#010+
  #010+
  'begin'#010+
  '  If (Defaults.Move<>'#039#039') then'#010+
  '    begin'#010+
  '      Args:=TstringList.Create;'#010+
  '      try'#010+
  '        Args.Add(S','ourceName);'#010+
  '        Args.Add(DestName);'#010+
  '        ExecuteCommand(Defaults.Move,Args);'#010+
  '      finally'#010+
  '        Args.Free;'#010+
  '      end;'#010+
  '    end'#010+
  '  else'#010+
  '    SysMoveFile(SourceName,DestName);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.CmdRemoveDirs(List: TStrings);',#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  If (Defaults.RemoveDir<>'#039#039') then'#010+
  '    begin'#010+
  '      ExecuteCommand(Defaults.RemoveDir,List);'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      SysDeleteDirectory(List[i]);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.CmdRemove','Trees(List: TStrings);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  If (Defaults.RemoveTree<>'#039#039') then'#010+
  '    begin'#010+
  '      ExecuteCommand(Defaults.RemoveTree,List);'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      SysDeleteTree(List[i]);'#010+
  'end;'#010+
  #010+
  'function TBuild','Engine.FileNewer(const Src, Dest: String): Boolean;'#010+
  #010+
  'Var'#010+
  '  DS,DD : Int64;'#010+
  '  D1,D2 : TDateTime;'#010+
  #010+
  'begin'#010+
  '  DS:=FileAge(Src);'#010+
  '  { Return false if file not found or not accessible }'#010+
  '  if DS=-1 then'#010+
  '    begin'#010+
  '      Log(vlWarning,SWarnCanNotGetFil','eAge,[Src]);'#010+
  '      Result:=false;'#010+
  '      exit;'#010+
  '    end;'#010+
  '  DD:=FileAge(Dest);'#010+
  '  { Return true if dest file not found or not accessible }'#010+
  '  if DD=-1 then'#010+
  '    begin'#010+
  '      Result:=True;'#010+
  '      exit;'#010+
  '    end;'#010+
  #010+
  '  D1:=FileDateToDateTime(DS);'#010+
  '  D2:=F','ileDateToDateTime(DD);'#010+
  '  Log(vlDebug,SDbgComparingFileTimes,[Src,DateTimeToStr(D1),Dest,DateT'+
  'imeToStr(D2)]);'#010+
  '  Result:=D1>D2;'#010+
  '  If Result then'#010+
  '    Log(vlInfo,SInfoSourceNewerDest,[Src,DateTimeToStr(D1),Dest,DateTi'+
  'meToStr(D2)]);'#010+
  'end;'#010+
  #010+
  #010+
  'proce','dure TBuildEngine.ExecuteCommands(Commands: TCommands; At: TCom'+
  'mandAt; APackage: TPackage);'#010+
  'Var'#010+
  '  C : TCommand;'#010+
  '  I : Integer;'#010+
  '  Opts,Cmd : String;'#010+
  '  Args : TStrings;'#010+
  '  CmdLine : Array of String;'#010+
  '  E : Boolean;'#010+
  '  ADictionary: TDictionary;'#010+
  ' ',' SourceFile, DestFile: string;'#010+
  'begin'#010+
  '  For I:=0 to Commands.Count-1 do'#010+
  '    begin'#010+
  '      C:=Commands.CommandItems[i];'#010+
  '      if (C.At=At) then'#010+
  '        begin'#010+
  '          E:=True;'#010+
  #010+
  '          if assigned(APackage) then'#010+
  '            ADictionary := AP','ackage.Dictionary'#010+
  '          else'#010+
  '            ADictionary := GlobalDictionary;'#010+
  '          SourceFile := ADictionary.ReplaceStrings(C.SourceFile);'#010+
  '          DestFile := ADictionary.ReplaceStrings(C.DestFile);'#010+
  '          if IsRelativePath(Source','File) then'#010+
  '            SourceFile := AddPathPrefix(APackage,SourceFile);'#010+
  '          if IsRelativePath(DestFile) then'#010+
  '            DestFile := AddPathPrefix(APackage,DestFile);'#010+
  #010+
  '          Cmd:=C.Command;'#010+
  '          If (ExtractFilePath(Cmd)='#039#039') ','then'#010+
  '            Cmd:=ExeSearch(Cmd,{$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}'+
  'SysUtils.GetEnvironmentvariable('#039'PATH'#039'));'#010+
  #010+
  '          If (SourceFile<>'#039#039') and (DestFile<>'#039#039')  then'#010+
  '            begin'#010+
  '              if not FileExists(DestFile) then'#010+
  '  ','              Log(vlInfo,SInfoDestDoesNotExist,[DestFile])'#010+
  '              else'#010+
  '                begin'#010+
  '                E:=FileNewer(SourceFile, DestFile);'#010+
  '                if E and (cmd = '#039#039') then'#010+
  '                  begin'#010+
  '                  log(v','lWarning,SWarnExtCommandNotFound,[C.Command,D'+
  'estFile,SourceFile]);'#010+
  '                  E := False;'#010+
  '                  end;'#010+
  '                end;'#010+
  '            end;'#010+
  '          If E then'#010+
  '            begin'#010+
  '            if Cmd = '#039#039' then'#010+
  '              e','rror(SErrExtCommandNotFound,[C.Command]);'#010+
  #010+
  '            If Assigned(C.BeforeCommand) then'#010+
  '              C.BeforeCommand(C);'#010+
  '            Opts:=ADictionary.Substitute(C.CmdLineOptions,['#039'SOURCE'+
  #039',SourceFile,'#039'DEST'#039',DestFile]);'#010+
  '            CmdLine',':=Opts.Split(['#039' '#039'],'#039'"'#039','#039'"'#039+
  ');'#010+
  '            Args:=TstringList.Create;'#010+
  '            try'#010+
  '              Args.AddStrings(CmdLine);'#010+
  '              Log(vlCommand,SInfoExecutingCommand,[Cmd,Opts]);'#010+
  '              ExecuteCommand(Cmd,Args,nil,C.IgnoreResu','lt);'#010+
  '            finally'#010+
  '              Args.Free;'#010+
  '            end;'#010+
  '            If Assigned(C.AfterCommand) then'#010+
  '              C.AfterCommand(C);'#010+
  '            end;'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.LogSearchPath(APackage: TP','ackage; const ASear'+
  'chPathName: string; Path: TConditionalStrings; ACPU: TCPU; AOS: TOS);'#010+
  'var'#010+
  '  S : String;'#010+
  '  I : Integer;'#010+
  '  C : TConditionalString;'#010+
  'begin'#010+
  '  S:='#039#039';'#010+
  '  for i:=0 to Path.Count-1 do'#010+
  '    begin'#010+
  '      C:=Path[I];'#010+
  '      if (ACPU in C','.CPUs) and (AOS in C.OSes) then'#010+
  '        begin'#010+
  '          if S<>'#039#039' then'#010+
  '            S:=S+PathSeparator;'#010+
  '          S:=S+APackage.Dictionary.ReplaceStrings(C.Value)'#010+
  '        end;'#010+
  '    end;'#010+
  '  if S<>'#039#039' then'#010+
  '    Log(vlDebug,SDbgSearchPath,[ASearchPa','thName,S]);'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.FindFileInPath(APackage: TPackage;'#010+
  '  Path: TConditionalStrings; const AFileName: String; var FoundPath: S'+
  'tring;'#010+
  '  ACPU: TCPU; AOS: TOS): Boolean;'#010+
  'var'#010+
  '  I : Integer;'#010+
  '  C : TConditionalString;'#010+
  'begin'#010+
  '  ','Result:=false;'#010+
  '  for i:=0 to Path.Count-1 do'#010+
  '    begin'#010+
  '      C:=Path[I];'#010+
  '      if (ACPU in C.CPUs) and (AOS in C.OSes) then'#010+
  '        begin'#010+
  '          FoundPath:=IncludeTrailingPathDelimiter(APackage.Dictionary.'+
  'ReplaceStrings(C.Value));'#010+
  '      ','    if FileExists(AddPathPrefix(APackage,FoundPath+AFileName))'+
  ' then'#010+
  '            begin'#010+
  '              result:=true;'#010+
  '              exit;'#010+
  '            end;'#010+
  '        end;'#010+
  '    end;'#010+
  '  FoundPath:='#039#039';'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.GetDirectoriesFromFil','elist(const AFileList, A'+
  'DirectoryList: TStringList);'#010+
  'var'#010+
  '  i: integer;'#010+
  '  s: string;'#010+
  'begin'#010+
  '  ADirectoryList.Sorted:=true;'#010+
  '  ADirectoryList.Duplicates:=dupIgnore;'#010+
  '  for i := 0 to AFileList.Count-1 do'#010+
  '    begin'#010+
  '      s := ExtractFileDir(AFileL','ist.Strings[i]);'#010+
  '      if s <> '#039#039' then'#010+
  '        ADirectoryList.Add(s);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.AddPackageMacrosToDictionary(const APackage: TP'+
  'ackage; ADictionary: TDictionary);'#010+
  'begin'#010+
  '  APackage.Dictionary.AddVariable('#039'UNITSOUTP','UTDIR'#039',AddPathPr'+
  'efix(APackage,APackage.GetUnitsOutputDir(Defaults.CompileTarget)));'#010+
  '  APackage.Dictionary.AddVariable('#039'BINOUTPUTDIR'#039',AddPathPrefix'+
  '(APackage,APackage.GetBinOutputDir(Defaults.CompileTarget)));'#010+
  '  APackage.Dictionary.AddVariabl','e('#039'PACKAGEVERSION'#039',APackage.'+
  'Version);'#010+
  '  APackage.Dictionary.AddVariable('#039'PACKAGEDIRECTORY'#039',APackage.'+
  'Directory);'#010+
  '  APackage.Dictionary.AddVariable('#039'PackageName'#039',APackage.Name)'+
  ';'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.ResolveFileNames(APackage: TPackage','; ACPU: TC'+
  'PU;'#010+
  '  AOS: TOS; DoChangeDir: boolean; WarnIfNotFound: boolean);'#010+
  #010+
  '  procedure FindMainSource(T:TTarget);'#010+
  '  var'#010+
  '    SD,SF  : String;'#010+
  '  begin'#010+
  '    LogSearchPath(APackage,'#039'package source'#039',APackage.SourcePat'+
  'h,ACPU,AOS);'#010+
  '    SD:=APackage','.Dictionary.ReplaceStrings(T.Directory);'#010+
  '    SF:=APackage.Dictionary.ReplaceStrings(T.SourceFileName);'#010+
  '    if SD='#039#039' then'#010+
  '      FindFileInPath(APackage,APackage.SourcePath,SF,SD,ACPU,AOS);'#010+
  '    if SD<>'#039#039' then'#010+
  '      SD:=IncludeTrailingPathDeli','miter(SD);'#010+
  '    T.FTargetSourceFileName:=SD+SF;'#010+
  '    if FileExists(AddPathPrefix(APackage,T.TargetSourceFileName)) then'+
  #010+
  '      Log(vlDebug,SDbgResolvedSourceFile,[T.SourceFileName,T.TargetSou'+
  'rceFileName])'#010+
  '    else'#010+
  '      begin'#010+
  '        if WarnIf','NotFound then'#010+
  '          Log(vlWarning,SWarnSourceFileNotFound,[T.SourceFileName,APac'+
  'kage.Name,MakeTargetString(ACPU,AOS)]);'#010+
  '        APackage.FAllFilesResolved:=false;'#010+
  '        T.FTargetSourceFileName:='#039#039';'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  '  procedure FindI','ncludeSources(T:TTarget);'#010+
  '  var'#010+
  '    SD,SF  : String;'#010+
  '    D : TDependency;'#010+
  '    j : integer;'#010+
  '  begin'#010+
  '    LogSearchPath(APackage,'#039'target include'#039',T.IncludePath,ACPU'+
  ',AOS);'#010+
  '    LogSearchPath(APackage,'#039'package include'#039',APackage.IncludeP'+
  'ath,ACPU,A','OS);'#010+
  '    for j:=0 to T.Dependencies.Count-1 do'#010+
  '      begin'#010+
  '        D:=T.Dependencies[j];'#010+
  '        if (D.DependencyType=depInclude) then'#010+
  '          begin'#010+
  '            if D.TargetFileName<>'#039#039' then'#010+
  '              Log(vlDebug,SDbgSourceAlreadyResol','ved,[D.Value])'#010+
  '            else if (ACPU in D.CPUs) and (AOS in D.OSes) then'#010+
  '              begin'#010+
  '                if ExtractFilePath(D.Value)='#039#039' then'#010+
  '                  begin'#010+
  '                    SF:=APAckage.Dictionary.ReplaceStrings(D.Value)',';'+
  #010+
  '                    SD:='#039#039';'#010+
  '                    // first check the target specific path'#010+
  '                    if not FindFileInPath(APackage, T.IncludePath,SF,S'+
  'D,ACPU,AOS) then'#010+
  '                      FindFileInPath(APackage, APackage.Include','Path,'+
  'SF,SD,ACPU,AOS);'#010+
  '                     if SD<>'#039#039' then'#010+
  '                       SD:=IncludeTrailingPathDelimiter(SD);'#010+
  '                     D.TargetFileName:=SD+SF;'#010+
  '                  end'#010+
  '                else'#010+
  '                  D.TargetFileNa','me:=D.Value;'#010+
  '                if FileExists(AddPathPrefix(APackage,D.TargetFileName)'+
  ') then'#010+
  '                  Log(vlDebug,SDbgResolvedIncludeFile,[D.Value,D.Targe'+
  'tFileName])'#010+
  '                else'#010+
  '                  begin'#010+
  '                    if ','WarnIfNotFound then'#010+
  '                      Log(vlWarning,SWarnIncludeFileNotFound,[D.Value,'+
  ' APackage.Name, MakeTargetString(ACPU,AOS)]);'#010+
  '                    APackage.FAllFilesResolved:=false;'#010+
  '                    D.TargetFileName:='#039#039';'#010+
  '       ','           end;'#010+
  '              end;'#010+
  '          end;'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  '  procedure FindExampleSource(T:TTarget);'#010+
  '  var'#010+
  '    SD,SF  : String;'#010+
  '  begin'#010+
  '    LogSearchPath(APackage,'#039'package example'#039',APackage.ExampleP'+
  'ath,ACPU,AOS);'#010+
  '    SD:=APackage.','Dictionary.ReplaceStrings(T.Directory);'#010+
  '    SF:=APackage.Dictionary.ReplaceStrings(T.SourceFileName);'#010+
  '    if SD='#039#039' then'#010+
  '      FindFileInPath(APackage, APackage.ExamplePath,SF,SD,ACPU,AOS);'#010+
  '    if SD<>'#039#039' then'#010+
  '      SD:=IncludeTrailingPathDel','imiter(SD);'#010+
  '    T.FTargetSourceFileName:=SD+SF;'#010+
  '    if FileExists(AddPathPrefix(APackage,T.TargetSourceFileName)) then'+
  #010+
  '      Log(vlDebug,SDbgResolvedSourceFile,[T.SourceFileName,T.TargetSou'+
  'rceFileName])'#010+
  '    else'#010+
  '      begin'#010+
  '        if WarnI','fNotFound then'#010+
  '          Log(vlWarning,SWarnSourceFileNotFound,[T.SourceFileName, APa'+
  'ckage.Name, MakeTargetString(ACPU,AOS)]);'#010+
  '        T.FTargetSourceFileName:='#039#039';'#010+
  '        APackage.FAllFilesResolved:=false;'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  'var'#010+
  '  T : TTar','get;'#010+
  '  i : Integer;'#010+
  'begin'#010+
  '  if not((ACPU in APackage.CPUs) and (AOS in APackage.OSes)) then'#010+
  '    exit;'#010+
  '  if APackage.FAllFilesResolved then'#010+
  '    Exit;'#010+
  '  APackage.FAllFilesResolved:=true;'#010+
  '  try'#010+
  '    if DoChangeDir and (APackage.Directory<>'#039#039') t','hen'#010+
  '      GPathPrefix := APackage.Directory;'#010+
  '    APackage.Dictionary.AddVariable('#039'CPU'#039',CPUToString(ACPU));'#010+
  '    APackage.Dictionary.AddVariable('#039'OS'#039',OSToString(AOS));'#010+
  '    For I:=0 to APackage.Targets.Count-1 do'#010+
  '      begin'#010+
  '        T:=APackag','e.FTargets.TargetItems[I];'#010+
  '        if (ACPU in T.CPUs) and (AOS in T.OSes) then'#010+
  '          begin'#010+
  '            // Debug information'#010+
  '            Log(vlDebug,SDbgResolvingSourcesOfTarget,[T.Name,MakeTarge'+
  'tString(ACPU,AOS)]);'#010+
  '            try'#010+
  '    ','          LogIndent;'#010+
  #010+
  '              case T.TargetType of'#010+
  '                ttProgram,'#010+
  '                ttSharedLibrary,'#010+
  '                ttUnit,'#010+
  '                ttImplicitUnit :'#010+
  '                  begin'#010+
  '                    if T.FTargetSourceFile','Name<>'#039#039' then'#010+
  '                      Log(vlDebug,SDbgSourceAlreadyResolved,[T.Name])'#010+
  '                    else'#010+
  '                      FindMainSource(T);'#010+
  '                    if T.Dependencies.Count>0 then'#010+
  '                      FindIncludeSource','s(T);'#010+
  '                  end;'#010+
  '                ttExampleUnit,'#010+
  '                ttExampleProgram :'#010+
  '                  begin'#010+
  '                    if T.FTargetSourceFileName<>'#039#039' then'#010+
  '                      Log(vlDebug,SDbgSourceAlreadyResolved,[T.Na','me]'+
  ')'#010+
  '                    else'#010+
  '                      FindExampleSource(T);'#010+
  '                  end;'#010+
  '                ttCleanOnlyUnit, // not sure about this one ?'#010+
  '                ttFPDoc:'#010+
  '                  ; // Avoid compiler warning'#010+
  '          ','    end;'#010+
  '            finally'#010+
  '              LogUnIndent;'#010+
  '            end;'#010+
  '          end;'#010+
  '      end;'#010+
  '  finally'#010+
  '    If DoChangeDir and (APackage.Directory<>'#039#039') then'#010+
  '      GPathPrefix := '#039#039';'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.ClearResolvedFile','Names(APackage: TPackage);'#010+
  #010+
  '  procedure ClearResolvedFileNamesForDependencies(ADependencies: TDepe'+
  'ndencies);'#010+
  '  var'#010+
  '    I: Integer;'#010+
  '    D: TDependency;'#010+
  '  begin'#010+
  '    For I:=0 to ADependencies.Count-1 do'#010+
  '      begin'#010+
  '        D := ADependencies[I','];'#010+
  '        D.TargetFileName:='#039#039';'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  'var'#010+
  '  T : TTarget;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  APackage.FAllFilesResolved:=false;'#010+
  '  ClearResolvedFileNamesForDependencies(APackage.Dependencies);'#010+
  '  For I:=0 to APackage.Targets.Count-1 do'#010+
  '    b','egin'#010+
  '      T:=APackage.FTargets.TargetItems[I];'#010+
  '      T.FTargetSourceFileName:='#039#039';'#010+
  '      ClearResolvedFileNamesForDependencies(T.Dependencies);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.ResolvePackagePaths(APackage:TPackage);'#010+
  #010+
  'var'#010+
  '  i: Integer',';'#010+
  '  Continue: Boolean;'#010+
  'begin'#010+
  '{$ifndef NO_THREADING}'#010+
  '  APackage.EnterResolveDirsCS;'#010+
  '  try'#010+
  '{$endif}'#010+
  '    if APackage.UnitDir='#039#039' then'#010+
  '      begin'#010+
  '        Log(vldebug, SDbgSearchExtDepPath, [APackage.Name]);'#010+
  '        GetPluginManager.BeforeResolv','ePackagePath(Self, APackage, Co'+
  'ntinue);'#010+
  '        if Continue then'#010+
  '          begin'#010+
  '          for I := 0 to Defaults.SearchPath.Count-1 do'#010+
  '            begin'#010+
  '              if Defaults.SearchPath[i]<>'#039#039' then'#010+
  '                GetPluginManager.Reso','lvePackagePath(Self, APackage, '+
  'Defaults.SearchPath[i], Continue);'#010+
  '              if not Continue then'#010+
  '                Break'#010+
  '            end;'#010+
  #010+
  '          if Continue then'#010+
  '            GetPluginManager.AfterResolvePackagePath(Self, APackage, C'+
  'on','tinue);'#010+
  '          end;'#010+
  #010+
  '        if APackage.UnitDir = '#039#039' then'#010+
  '          APackage.UnitDir := DirNotFound'#010+
  '      end;'#010+
  '{$ifndef NO_THREADING}'#010+
  '  finally'#010+
  '    APackage.LeaveResolveDirsCS;'#010+
  '  end;'#010+
  '{$endif}'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.GetUnitDir(APa','ckage:TPackage):String;'#010+
  'begin'#010+
  '  ResolvePackagePaths(APackage);'#010+
  '  // Special error marker to prevent searches in case of error'#010+
  '  if APackage.UnitDir=DirNotFound then'#010+
  '    Result:='#039#039#010+
  '  else'#010+
  '    Result:=APackage.UnitDir;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuild','Engine.AddDependencyPaths(L: TStrings; DependencyTyp'+
  'e: TDependencyType; ATarget: TTarget);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  D : TDependency;'#010+
  '  SD : String;'#010+
  'begin'#010+
  '  For I:=0 to ATarget.Dependencies.Count-1 do'#010+
  '    begin'#010+
  '      D:=ATarget.Dependencies[i];'#010,
  '      if (D.DependencyType=DependencyType) and'#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '        begin'#010+
  '          SD:=ExcludeTrailingPathDelimiter(ExtractFilePath(D.TargetFil'+
  'eName));'#010+
  '          if SD<>'#039#039' then'#010+
  '        ','    L.Add(SD);'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.AddDependencyUnitPaths(L:TStrings;APackage: TPa'+
  'ckage);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  D : TDependency;'#010+
  '  S : String;'#010+
  'begin'#010+
  '  For I:=0 to APackage.Dependencies.Count-1 ','do'#010+
  '    begin'#010+
  '      D:=APackage.Dependencies[i];'#010+
  '      if (D.DependencyType=depPackage) and'#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '        begin'#010+
  '          P:=TPackage(D.Target);'#010+
  '          If Assigned(P) then'#010+
  '     ','       begin'#010+
  '              // Already processed?'#010+
  '              S:=GetUnitDir(P);'#010+
  '              if L.IndexOf(S)=-1 then'#010+
  '                begin'#010+
  '                  // Add this package and then dependencies'#010+
  '                  L.Add(S);'#010+
  '           ','       AddDependencyUnitPaths(L,P);'#010+
  '                end;'#010+
  '            end;'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  '{$ifdef NO_THREADING}'#010+
  'var'#010+
  '{$else NO_THREADING}'#010+
  'threadvar'#010+
  '{$endif NO_THREADING}'#010+
  '  GHandledRecursiveDependencies: TStrings;'#010+
  #010+
  'procedure TBuil','dEngine.AddDependencyTransmittedOptions(Args: TString'+
  's; APackage: TPackage);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  D : TDependency;'#010+
  '  IsRootLevel: Boolean;'#010+
  'begin'#010+
  '  if not Assigned(GHandledRecursiveDependencies) then'#010+
  '    begin'#010+
  '      GHandledR','ecursiveDependencies := TStringList.Create;'#010+
  '      IsRootLevel := True;'#010+
  '    end'#010+
  '  else'#010+
  '    IsRootLevel := False;'#010+
  #010+
  '  try'#010+
  '    For I:=0 to APackage.Dependencies.Count-1 do'#010+
  '      begin'#010+
  '        D:=APackage.Dependencies[i];'#010+
  '        if (D.Dependenc','yType=depPackage) and'#010+
  '           (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '          begin'#010+
  '            P:=TPackage(D.Target);'#010+
  '            If Assigned(P) then'#010+
  '              begin'#010+
  '                // Already processed?'#010+
  '        ','        if GHandledRecursiveDependencies.IndexOf(P.Name)=-1 '+
  'then'#010+
  '                  begin'#010+
  '                    GHandledRecursiveDependencies.Add(P.Name);'#010+
  '                    AddDependencyTransmittedOptions(Args,P);'#010+
  '                    Args.Ad','dStrings(P.TransmitOptions);'#010+
  '                  end;'#010+
  '              end;'#010+
  '          end;'#010+
  '      end;'#010+
  '  finally'#010+
  '    if IsRootLevel then'#010+
  '      FreeAndNil(GHandledRecursiveDependencies);'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'function TBuildEngine.AddPathPrefix(APackage: T','Package; APath: strin'+
  'g): string;'#010+
  'begin'#010+
  '  if IsRelativePath(APath) and (GPathPrefix<>'#039#039') then'#010+
  '    result := IncludeTrailingPathDelimiter(GPathPrefix) + APath'#010+
  '  else'#010+
  '    result := APath;'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.GetCompilerCommand(APackag','e: TPackage; ATarget'+
  ': TTarget;'#010+
  '  Env: TStrings): String;'#010+
  #010+
  'var'#010+
  '  Args : TStringList;'#010+
  '  i : Integer;'#010+
  #010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  Args:=TStringList.Create;'#010+
  '  try'#010+
  '    Args:=TStringList.Create;'#010+
  '    Args.Duplicates:=dupIgnore;'#010+
  '    GetCompilerCommand(A','rgs,aPackage,aTarget,Env);'#010+
  '    // Convert to string'#010+
  '    if Defaults.UseEnvironment and assigned(Env) then'#010+
  '      Result:='#039'!FPCEXTCMD'#039' // Result is in environment.'#010+
  '    else'#010+
  '      for i:=0 to Args.Count-1 do'#010+
  '        Result:=Result+'#039' '#039'+Args[i];',#010+
  '    Delete(result,1,1);'#010+
  '  finally'#010+
  '    Args.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.GetCompilerCommand(Args: TStrings; APackage: TP'+
  'ackage;'#010+
  '  ATarget: TTarget; Env: TStrings);'#010+
  #010+
  'Var'#010+
  '  L : TUnsortedDuplicatesStringList;'#010+
  '  s : string;'#010+
  '  ExtC','md,ErrS: string;'#010+
  '  i : Integer;'#010+
  'begin'#010+
  '  if ATarget.TargetSourceFileName = '#039#039' then'#010+
  '    Error(SErrCouldNotCompile,[ATarget.Name, APackage.Name]);'#010+
  #010+
  '  //compiler configuration'#010+
  '  if Defaults.NoFPCCfg then'#010+
  '    Args.Add('#039'-n'#039');'#010+
  #010+
  '  // Target OS'#010+
  '  Ar','gs.Add('#039'-T'#039'+OSToString(Defaults.OS));'#010+
  #010+
  '  // Target CPU.'#010+
  '  // This setting is only applicable when '#039'fpc'#039' is used as com'+
  'piler-executable.'#010+
  '  if ExtractFileName(GetCompiler) = '#039'fpc'#039' then'#010+
  '    Args.Add('#039'-P'#039'+CPUToString(Defaults.CPU));'#010+
  #010+
  '  // Subta','rget'#010+
  '  if (Defaults.SubTarget<>'#039#039') then'#010+
  '    Args.Add('#039'-t'#039'+Defaults.SubTarget);'#010+
  #010+
  '  // Compile mode'#010+
  '  If ATarget.Mode<>cmFPC then'#010+
  '    Args.Add('#039'-M'#039'+ModeToString(ATarget.Mode))'#010+
  '  else If Defaults.Mode<>cmFPC then'#010+
  '    Args.Add('#039'-M'#039'+ModeToString','(Defaults.Mode));'#010+
  '  // Output file paths'#010+
  '  If ATarget.TargetType in [ttExampleUnit,ttExampleProgram] then'#010+
  '    begin'#010+
  '    Args.Add('#039'-FE'#039'+AddPathPrefix(APackage,APackage.GetExamples'+
  'OutputDir(Defaults.CompileTarget)));'#010+
  '    Args.Add('#039'-FU'#039'+AddPat','hPrefix(APackage,APackage.GetExampl'+
  'esOutputDir(Defaults.CompileTarget)));'#010+
  '    Args.Add('#039'-Fu'#039'+AddPathPrefix(APackage,APackage.GetUnitsOut'+
  'putDir(Defaults.CompileTarget)));'#010+
  '    end'#010+
  '  else'#010+
  '    begin'#010+
  '    If ATarget.TargetType in ProgramTargets t','hen'#010+
  '      Args.Add('#039'-FE'#039'+AddPathPrefix(APackage,APackage.GetBinOut'+
  'putDir(Defaults.CompileTarget)));'#010+
  '    Args.Add('#039'-FU'#039'+AddPathPrefix(APackage,APackage.GetUnitsOut'+
  'putDir(Defaults.CompileTarget)));'#010+
  '    end;'#010+
  '  // Object Path'#010+
  '  L:=TUnsortedDupl','icatesStringList.Create;'#010+
  '  L.Duplicates:=dupIgnore;'#010+
  '  AddConditionalStrings(APackage, L,APackage.ObjectPath, Defaults.Comp'+
  'ileTarget);'#010+
  '  AddConditionalStrings(APackage, L,ATarget.ObjectPath, Defaults.Compi'+
  'leTarget);'#010+
  '  for i:=0 to L.Count-1 d','o'#010+
  '    Args.Add('#039'-Fo'#039'+AddPathPrefix(APackage,L[i]));'#010+
  '  FreeAndNil(L);'#010+
  '  // Unit Dirs'#010+
  '  L:=TUnsortedDuplicatesStringList.Create;'#010+
  '  L.Duplicates:=dupIgnore;'#010+
  '  AddDependencyUnitPaths(L,APackage);'#010+
  '  AddDependencyPaths(L,depUnit,ATarget);'#010+
  '  AddCo','nditionalStrings(APackage, L,APackage.UnitPath,Defaults.Compi'+
  'leTarget);'#010+
  '  AddConditionalStrings(APackage, L,ATarget.UnitPath,Defaults.CompileT'+
  'arget);'#010+
  '  for i:=0 to L.Count-1 do'#010+
  '    Args.Add('#039'-Fu'#039'+AddPathPrefix(APackage,L[i]));'#010+
  '  FreeAndNil(','L);'#010+
  '  // Include Path'#010+
  '  L:=TUnsortedDuplicatesStringList.Create;'#010+
  '  L.Duplicates:=dupIgnore;'#010+
  '  AddDependencyPaths(L,depInclude,ATarget);'#010+
  '  AddConditionalStrings(APackage, L,APackage.IncludePath, Defaults.Com'+
  'pileTarget);'#010+
  '  AddConditionalStrin','gs(APackage, L,ATarget.IncludePath, Defaults.Co'+
  'mpileTarget);'#010+
  '  for i:=0 to L.Count-1 do'#010+
  '    Args.Add('#039'-Fi'#039'+AddPathPrefix(APackage,L[i]));'#010+
  '  FreeAndNil(L);'#010+
  #010+
  '  // libc-linker path (always for Linux, since required for LLVM and S'+
  'EH; this does ','not'#010+
  '  // force the linking of anything by itself, but just adds a search d'+
  'irectory)'#010+
  '  // Do not add it if -Xd option is used'#010+
  '  if (APackage.NeedLibC or (Defaults.OS=linux)) and'#010+
  '     ((not Defaults.HaveOptions) or (Defaults.Options.IndexOf('#039,'-X'+
  'd'#039')=-1)) then'#010+
  '    begin'#010+
  '      if FCachedlibcPath='#039#039' then'#010+
  '        begin'#010+
  #010+
  '          s:=GetDefaultLibGCCDir(Defaults.CPU, Defaults.OS,ErrS);'#010+
  '          if s='#039#039' then'#010+
  '            begin'#010+
  '              if (ErrS<>'#039#039') and not (GCCLibWarningIssued) t','hen'+
  #010+
  '                Log(vlWarning, SWarngcclibpath +'#039' '#039'+ErrS);'#010+
  '             GCCLibWarningIssued:=True;'#010+
  '           end'#010+
  '          else'#010+
  '            begin'#010+
  '{$ifndef NO_THREADING}'#010+
  '              EnterCriticalsection(FGeneralCriticalSection);'#010+
  '     ','         { prevent FCachedlibcPath getting freed by thread 2 wh'+
  'ile thread 1 is'#010+
  '                concatenating it to -Fl below }'#010+
  '              try'#010+
  '                if {$IFDEF HAVE_VOLATILE}volatile{$ENDIF}(FCachedlibcP'+
  'ath)='#039#039' then'#010+
  '            ','      begin'#010+
  '{$endif NO_THREADING}'#010+
  '                    FCachedlibcPath:=s;'#010+
  '{$ifndef NO_THREADING}'#010+
  '                  end;'#010+
  '              finally'#010+
  '                LeaveCriticalsection(FGeneralCriticalSection);'#010+
  '              end;'#010+
  '{$endif NO_THREA','DING}'#010+
  '            end;'#010+
  '        end'#010+
  '      else'#010+
  '        { make sure we don'#039't access the contents of the string bef'+
  'ore they'#039've been'#010+
  '          synchronised from the thread that wrote them; the critical s'+
  'ection there'#010+
  '          acts as a read/wri','te barrier }'#010+
  '        ReadBarrier;'#010+
  '{$ifdef NO_THREADING}'#010+
  '      Args.Add('#039'-Fl'#039'+FCachedlibcPath);'#010+
  '{$ELSE}'#010+
  '      Args.Add('#039'-Fl'#039'+{$IFDEF HAVE_VOLATILE}volatile{$ENDIF}(FC'+
  'achedlibcPath));'#010+
  '{$ENDIF}'#010+
  '    end;'#010+
  #010+
  '  // Custom options which are added by ','dependencies'#010+
  '  AddDependencyTransmittedOptions(Args, APackage);'#010+
  #010+
  '  // Custom Options'#010+
  '  If (Defaults.HaveOptions) then'#010+
  '    Args.AddStrings(Defaults.Options);'#010+
  #010+
  '  APackage.ApplyPackageVariantToCompilerOptions(Args);'#010+
  #010+
  '  If (APackage.HaveOptions',') then'#010+
  '    Args.AddStrings(APackage.Options);'#010+
  '  If (ATarget.HaveOptions) then'#010+
  '    Args.AddStrings(ATarget.Options);'#010+
  #010+
  '  {$ifdef HAS_UNIT_PROCESS}'#010+
  '  // Force the compiler-output to be easy parseable'#010+
  '  if not Verbose then'#010+
  '    args.Add('#039'-viq'#039');',#010+
  '  {$endif}'#010+
  #010+
  '  // Convert to string'#010+
  '  ExtCmd:='#039#039';'#010+
  '  for i:=0 to Args.Count-1 do'#010+
  '    ExtCmd:=ExtCmd+'#039' '#039'+maybequoted(APackage.Dictionary.Replace'+
  'Strings(Args[i]));'#010+
  '  Delete(ExtCmd,1,1);'#010+
  #010+
  '  if Defaults.UseEnvironment and assigned(Env) then'#010+
  '    ','begin'#010+
  '      env.Values['#039'FPCEXTCMD'#039'] := ExtCmd;'#010+
  '      // Make sure that this process'#039' environment variables are pa'+
  'ssed to the'#010+
  '      // compiler'#039's environment'#010+
  '      for i := 0 to GetEnvironmentVariableCount-1 do'#010+
  '        env.Add(GetEnvironment','String(i));'#010+
  '    end;'#010+
  #010+
  '  // Replace strings'#010+
  '  for I:=0 to Args.Count-1 do'#010+
  '    Args[i]:=APackage.Dictionary.ReplaceStrings(Args[i]);'#010+
  '  // Add Filename to compile'#010+
  '  Args.Add(AddPathPrefix(APackage,ATarget.TargetSourceFileName));'#010+
  'end;'#010+
  #010+
  'function',' TBuildEngine.TargetOK(ATarget: TTarget; ACPU: TCPU; AOS: TO'+
  'S;'#010+
  '  const aSubTarget: String): Boolean;'#010+
  #010+
  'Var'#010+
  '  CT : TCompileTarget;'#010+
  #010+
  'begin'#010+
  '  CT.OS:=aOS;'#010+
  '  CT.CPU:=aCPU;'#010+
  '  CT.Subtarget:=aSubTarget;'#010+
  '  Result:=TargetOK(aTarget,CT);'#010+
  'end;'#010+
  #010+
  #010+
  'functio','n TBuildEngine.GetCompiler: String;'#010+
  'Var'#010+
  '  S : String;'#010+
  'begin'#010+
  '  // Cache in FCompiler for speed.'#010+
  '  If (FCompiler='#039#039') then'#010+
  '    begin'#010+
  '    FCompiler:=Defaults.Compiler;'#010+
  '    If (ExtractFilePath(FCompiler)='#039#039') then'#010+
  '      begin'#010+
  '      S:=ExeSearch(F','Compiler,{$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}'+
  'SysUtils.GetEnvironmentVariable('#039'PATH'#039'));'#010+
  '      If (S<>'#039#039') then'#010+
  '         FCompiler:=S;'#010+
  '      end;'#010+
  '    end;'#010+
  '  Result:=FCompiler;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CreateOutputDir(APackage: TPackage',');'#010+
  'Var'#010+
  '  D : String;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  //create a units directory'#010+
  '  D:=AddPathPrefix(APackage,APackage.GetUnitsOutputDir(Defaults.Compil'+
  'eTarget));'#010+
  '  If not SysDirectoryExists(D) then'#010+
  '    begin'#010+
  '      Log(vlInfo,SInfoCreatingOutputDir,[D]',');'#010+
  '      CmdCreateDir(D);'#010+
  '    end;'#010+
  #010+
  '  //also create a bin directory for programtargets'#010+
  '  For i := 0 to Pred(APackage.Targets.Count) do'#010+
  '    begin'#010+
  '      if APackage.Targets.TargetItems[i].TargetType in (ProgramTargets'+
  '-[ttExampleProgram]) then',#010+
  '        begin'#010+
  '          D:=AddPathPrefix(APackage,APackage.GetBinOutputDir(Defaults.'+
  'CompileTarget));'#010+
  '          If not SysDirectoryExists(D) then'#010+
  '            begin'#010+
  '              Log(vlInfo,SInfoCreatingOutputDir,[D]);'#010+
  '              CmdCreat','eDir(D);'#010+
  '            end;'#010+
  '          //do not continue loop, directory is made anyway'#010+
  '          break;'#010+
  '        end;'#010+
  '    end;'#010+
  #010+
  '  //and an examples directory when examples are compiled'#010+
  '  if Defaults.BuildExamples then'#010+
  '    For i := 0 to Pred(AP','ackage.Targets.Count) do'#010+
  '      begin'#010+
  '        if APackage.Targets.TargetItems[i].TargetType in [ttExampleUni'+
  't,ttExampleProgram] then'#010+
  '          begin'#010+
  '            D:=AddPathPrefix(APackage,APackage.GetExamplesOutputDir(De'+
  'faults.CompileTarget))',';'#010+
  '            If not SysDirectoryExists(D) then'#010+
  '              begin'#010+
  '                Log(vlInfo,SInfoCreatingOutputDir,[D]);'#010+
  '                CmdCreateDir(D);'#010+
  '              end;'#010+
  '            break;'#010+
  '          end;'#010+
  '      end;'#010+
  'end;'#010+
  #010+
  #010+
  'function TBu','ildEngine.DependencyOK(ADependency: TDependency): Boolea'+
  'n;'#010+
  'begin'#010+
  '  Result:=(Defaults.CPU in ADependency.CPUs) and (Defaults.OS in ADepe'+
  'ndency.OSes);'#010+
  'end;'#010+
  #010+
  'function TBuildEngine.TargetOK(ATarget: TTarget; const aCompileTarget '+
  ': TCompileTarge','t): Boolean;'#010+
  #010+
  #010+
  'begin'#010+
  '  if (ATarget.TargetType in ProgramTargets) and'#010+
  '     (Defaults.SkipAllPrograms or'#010+
  '      (Defaults.SkipCrossPrograms and'#010+
  '       IsDifferentFromBuild(aCOmpileTarget.CPU, aCOmpileTarget.OS))) t'+
  'hen'#010+
  '    result := False'#010+
  '  els','e'#010+
  '    Result:=(aCompileTarget.CPU in ATarget.CPUs)'#010+
  '            and (aCompileTarget.OS in ATarget.OSes)'#010+
  '            and aTarget.SubTargetAllowed(aCompileTarget.SubTarget);'#010+
  'end;'#010+
  #010+
  'function TBuildEngine.TargetInstallOK(ATarget: TTarget; ACPU: T','CPU; '+
  'AOS: TOS;'#010+
  '  const aSubTarget: String): Boolean;'#010+
  'Var'#010+
  '  CT : TCompileTarget;'#010+
  #010+
  'begin'#010+
  '  CT.OS:=aOS;'#010+
  '  CT.CPU:=aCPU;'#010+
  '  CT.Subtarget:=aSubTarget;'#010+
  '  Result:=TargetInstallOK(aTarget,CT);'#010+
  'end;'#010+
  #010+
  'function TBuildEngine.TargetInstallOK(ATarget: TTa','rget;'#010+
  '  const aCompileTarget: TCompileTarget): Boolean;'#010+
  'begin'#010+
  '  result := TargetOK(ATarget, aCompileTarget) and ATarget.Install;'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.PackageOK(APackage: TPackage): Boolean;'#010+
  'begin'#010+
  '  Result:=(Defaults.CPU in APackage.','CPUs) and (Defaults.OS in APacka'+
  'ge.OSes);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoBeforeCompile(APackage: TPackage);'#010+
  'begin'#010+
  '  ExecuteCommands(APackage.Commands,caBeforeCompile);'#010+
  '  If Assigned(APackage.BeforeCompile) then'#010+
  '    APackage.BeforeCompile(A','Package);'#010+
  '  If Assigned(APackage.BeforeCompileProc) then'#010+
  '    APackage.BeforeCompileProc(APackage);'#010+
  '  // It could be that files that weren'#039't found before are availabl'+
  'e now.'#010+
  '  ResolveFileNames(APackage,Defaults.CPU,Defaults.OS,false,true);'#010+
  'en','d;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoAfterCompile(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterCompile) then'#010+
  '    APackage.AfterCompile(APackage);'#010+
  '  If Assigned(APackage.AfterCompileProc) then'#010+
  '    APackage.AfterCompileProc(APackage);'#010+
  '  Ex','ecuteCommands(APackage.Commands,caAfterCompile);'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.NeedsCompile(APackage: TPackage; ATarget: TTarge'+
  't'#010+
  '  ): Boolean;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  D : TDependency;'#010+
  '  T : TTarget;'#010+
  '  OD,OFN,TFN : String;'#010+
  '  CompileReason: Strin','g;'#010+
  'begin'#010+
  '  Result:=False;'#010+
  #010+
  '  // Examples are only compiled on request'#010+
  '  if (ATarget.TargetType in [ttExampleUnit, ttExampleProgram]) and not'+
  ' Defaults.BuildExamples then'#010+
  '    Exit;'#010+
  #010+
  '  // Forced recompile?'#010+
  '  if FForceCompile then'#010+
  '    begin'#010+
  '   ',' Result:=true;'#010+
  '    CompileReason:=SDbgForcedCompile;'#010+
  '    end;'#010+
  #010+
  '  // Files which should not be compiled on this target can not trigger'+
  ' a compile.'#010+
  '  if not TargetOK(ATarget, Defaults.CompileTarget) then'#010+
  '    Exit;'#010+
  #010+
  '  // Check output file'#010+
  '  if ','not result then'#010+
  '    begin'#010+
  '      if ATarget.TargetType in [ttExampleUnit, ttExampleProgram] then'#010+
  '        OD:=APackage.GetExamplesOutputDir(Defaults.CompileTarget)'#010+
  '      else if ATarget.TargetType in ProgramTargets then'#010+
  '        OD:=APackage.G','etBinOutputDir(Defaults.CompileTarget)'#010+
  '      else'#010+
  '        OD:=APackage.GetUnitsOutputDir(Defaults.CompileTarget);'#010+
  '      If (OD<>'#039#039') then'#010+
  '        OD:=IncludeTrailingPathDelimiter(OD);'#010+
  '      OFN:=AddPathPrefix(APackage, OD+ATarget.GetOutPutFi','leName(Defa'+
  'ults.OS));'#010+
  '      Result:=Not FileExists(OFN);'#010+
  '      if Result then'#010+
  '        begin'#010+
  '        CompileReason:=SDbgOutputDoesNotExist;'#010+
  '        Log(vlDebug,SDbgOutputNotYetAvailable,[OFN]);'#010+
  '        end;'#010+
  '    end;'#010+
  #010+
  '  // Check main source, ','only if the TargetSourceFileName is found'#010+
  '  If not Result and (ATarget.TargetSourceFileName<>'#039#039') then'#010+
  '    begin'#010+
  '      TFN := AddPathPrefix(APackage,ATarget.TargetSourceFileName);'#010+
  '      if FileExists(TFN) then'#010+
  '        Result:=FileNewer(TFN,O','FN);'#010+
  '      if Result then'#010+
  '        CompileReason:=SDbgNewerSource;'#010+
  '    end;'#010+
  #010+
  '  // Check unit and include dependencies'#010+
  '  If not Result then'#010+
  '    begin'#010+
  '      ResolveDependencies(ATarget.Dependencies,ATarget.Collection as T'+
  'Targets);'#010+
  '      I:=0;'#010,
  '      for i:=0 to ATarget.Dependencies.Count-1 do'#010+
  '        begin'#010+
  '          D:=ATarget.Dependencies[i];'#010+
  '          if (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'+
  #010+
  '            begin'#010+
  '              case D.DependencyType of'#010+
  '          ','      depUnit :'#010+
  '                  begin'#010+
  '                    T:=TTarget(D.Target);'#010+
  '                    If (T=Nil) then'#010+
  '                      Error(SErrDepUnknownTarget,[D.Value, ATarget.Nam'+
  'e, APackage.Name]);'#010+
  '                    // If a depe','ndent package is compiled we always '+
  'need to recompile'#010+
  '                    Log(vldebug, SDbgDependencyOnUnit, [ATarget.Name,T'+
  '.Name]);'#010+
  '                    Result:=(T.State=tsCompiled);'#010+
  '                    if Result then'#010+
  '                      ','begin'#010+
  '                      Log(vldebug, SDbgDependencyUnitRecompiled, [T.Na'+
  'me]);'#010+
  '                      CompileReason:=Format(SDbgDependencyRecompiled,['+
  'T.Name]);'#010+
  '                      end;'#010+
  '                  end;'#010+
  '                depInclude :',#010+
  '                  begin'#010+
  '                    if D.TargetFileName<>'#039#039' then'#010+
  '                      begin'#010+
  '                        TFN:=AddPathPrefix(APackage,D.TargetFileName);'+
  #010+
  '                        Result:=FileNewer(TFN,OFN);'#010+
  '               ','         if result then'#010+
  '                          CompileReason:=Format(SDbgNewerInclude,[D.Ta'+
  'rgetFileName]);'#010+
  '                      end;'#010+
  '                  end;'#010+
  '                depPackage :'#010+
  '                  begin'#010+
  '                    log(vlW','arning,SWarnTargetDependsOnPackage,[ATarg'+
  'et.Name, APackage.Name, d.Value]);'#010+
  '                  end;'#010+
  '                depImplicitPackage:'#010+
  '                  ; // Avoid compiler warning'#010+
  '              end;'#010+
  '              if result then'#010+
  '           ','     break;'#010+
  '            end;'#010+
  '        end;'#010+
  '    end;'#010+
  #010+
  '  if result then'#010+
  '    Log(vlDebug,SDbgMustCompile,[ATarget.Name, CompileReason]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Compile(APackage: TPackage; ATarget: TTarget);'#010+
  'Var'#010+
  '  Env : TStrings;'#010+
  '  Args :',' TStrings;'#010+
  '  FN : String;'#010+
  #010+
  'begin'#010+
  '  Log(vlInfo,SInfoCompilingTarget,[ATarget.Name]);'#010+
  '  Args:=nil;'#010+
  '  Env:=nil;'#010+
  '  try'#010+
  '    LogIndent;'#010+
  '    ExecuteCommands(ATarget.Commands,caBeforeCompile);'#010+
  '    If Assigned(ATarget.BeforeCompile) then'#010+
  '      ATarg','et.BeforeCompile(ATarget);'#010+
  '    if (APackage.BuildMode=bmBuildUnit) and not (ATarget.TargetType in'+
  ' [ttProgram,ttSharedLibrary,ttExampleUnit,ttExampleProgram]) then'#010+
  '      begin'#010+
  '        begin'#010+
  '        FN:=ExtractFileName(ATarget.TargetSourceFil','eName);'#010+
  '        if IndexText(ExtractFileExt(FN),['#039'.pp'#039','#039'.pas'#039','+
  #039'lpr'#039'])<>-1 then'#010+
  '          FN:=ChangeFileExt(FN,'#039#039');'#010+
  '        APackage.FBUTarget.Dependencies.AddUnit(FN).FTargetFileName:=A'+
  'Target.TargetSourceFileName;'#010+
  '        end;'#010+
  '      end'#010+
  '   ',' else'#010+
  '      begin'#010+
  '      Args:=TstringList.Create;'#010+
  '      if Defaults.UseEnvironment then'#010+
  '        Env:=TStringList.Create;'#010+
  '      GetCompilerCommand(Args,APackage,ATarget,Env);'#010+
  '      ExecuteCommand(GetCompiler,Args,Env);'#010+
  '      If Assigned(ATar','get.AfterCompile) then'#010+
  '        ATarget.AfterCompile(ATarget);'#010+
  '      ExecuteCommands(ATarget.Commands,caAfterCompile);'#010+
  '      end;'#010+
  '  finally'#010+
  '    LogUnIndent;'#010+
  '    Args.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CompileDependencies(APackage:TPa','ckage; ATarge'+
  't: TTarget);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  T : TTarget;'#010+
  '  D : TDependency;'#010+
  'begin'#010+
  '  Log(vlDebug, Format(SDbgCompilingDependenciesOfTarget, [ATarget.Name'+
  ']));'#010+
  '  try'#010+
  '  LogIndent;'#010+
  '  For I:=0 to ATarget.Dependencies.Count-1 do'#010+
  '    begin'#010+
  '     ',' D:=ATarget.Dependencies[i];'#010+
  '      if (D.DependencyType=depPackage) then'#010+
  '        log(vlWarning,SWarnTargetDependsOnPackage,[ATarget.Name, APack'+
  'age.Name, d.Value])'#010+
  '      else if (D.DependencyType=depUnit) and'#010+
  '         (Defaults.CPU in D.CPUs',') and (Defaults.OS in D.OSes) then'#010+
  '        begin'#010+
  '          T:=TTarget(D.Target);'#010+
  '          if Assigned(T) and (T<>ATarget) then'#010+
  '            begin'#010+
  '              if TargetOK(T, Defaults.CompileTarget) then'#010+
  '                begin'#010+
  '              ','    // We don'#039't need to compile implicit units, th'+
  'ey are only'#010+
  '                  // used for dependency checking'#010+
  '                  if (T.TargetType<>ttImplicitUnit) then'#010+
  '                    begin'#010+
  '                      case T.State of'#010+
  '       ','                 tsNeutral :'#010+
  '                          MaybeCompile(APackage,T);'#010+
  '                        tsConsidering :'#010+
  '                          Log(vlWarning,SWarnCircularTargetDependency,'+
  '[ATarget.Name,T.Name]);'#010+
  '                        t','sNoCompile,tsCompiled,tsInstalled,tsNotFoun'+
  'd:'#010+
  '                          ; // Avoid compiler warning'#010+
  '                      end;'#010+
  '                    end;'#010+
  '                end'#010+
  '              else'#010+
  '                Log(vlWarning, Format(SWarnDepUni','tNotFound, [T.Name,'+
  ' MakeTargetString(Defaults.CPU,Defaults.OS)]));'#010+
  '            end'#010+
  '          else'#010+
  '            Error(SErrDepUnknownTarget,[D.Value, ATarget.Name, APackag'+
  'e.Name]);'#010+
  '        end;'#010+
  '    end;'#010+
  '  finally'#010+
  '  LogUnIndent;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'p','rocedure TBuildEngine.MaybeCompile(APackage: TPackage; ATarget: TTa'+
  'rget);'#010+
  'begin'#010+
  '  if ATarget.State<>tsNeutral then'#010+
  '    Error(SErrInvalidState,[ATarget.Name]);'#010+
  '  Log(vlDebug, Format(SDbgConsideringTarget, [ATarget.Name]));'#010+
  '  try'#010+
  '  LogIndent;',#010+
  '  ATarget.FTargetState:=tsConsidering;'#010+
  '  ResolveDependencies(ATarget.Dependencies,ATarget.Collection as TTarg'+
  'ets);'#010+
  '  CompileDependencies(APackage, ATarget);'#010+
  '  if NeedsCompile(APackage, ATarget) then'#010+
  '    begin'#010+
  '      Compile(APackage,ATarget',');'#010+
  '      ATarget.FTargetState:=tsCompiled;'#010+
  '    end'#010+
  '  else'#010+
  '    ATarget.FTargetState:=tsNoCompile;'#010+
  '  finally'#010+
  '  LogUnIndent;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.NeedsCompile(APackage: TPackage): Boolean;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  D ',': TDependency;'#010+
  '  CompileReason: string;'#010+
  'begin'#010+
  '  Result:=False;'#010+
  #010+
  '  // Forced recompile?'#010+
  '  if FForceCompile then'#010+
  '    begin'#010+
  '    Result:=true;'#010+
  '    CompileReason:=SDbgForcedCompile;'#010+
  '    end;'#010+
  #010+
  '  // Recompile because of Package Dependencies?'#010+
  '  if ','not Result then'#010+
  '    begin'#010+
  '       I:=0;'#010+
  '       For I:=0 to APackage.Dependencies.Count-1 do'#010+
  '         begin'#010+
  '           D:=APackage.Dependencies[i];'#010+
  '           if (D.DependencyType=depPackage) and'#010+
  '              (Defaults.CPU in D.CPUs) and (De','faults.OS in D.OSes) t'+
  'hen'#010+
  '             begin'#010+
  '               P:=TPackage(D.Target);'#010+
  '               if Assigned(P) then'#010+
  '                 begin'#010+
  '                   Result:=(P.State=tsCompiled);'#010+
  '                   if Result then'#010+
  '                ','     begin'#010+
  '                     CompileReason:=Format(SDbgPackageDepRecompiled,[P'+
  '.Name]);'#010+
  '                     break;'#010+
  '                     end;'#010+
  '                 end;'#010+
  '             end;'#010+
  '         end;'#010+
  '    end;'#010+
  #010+
  '  // Recompile a Target of this ','package?'#010+
  '  If Not Result then'#010+
  '    begin'#010+
  '      GPathPrefix := APackage.Directory;'#010+
  '      try'#010+
  '        for i:=0 to APackage.Targets.Count-1 do'#010+
  '          begin'#010+
  '            Result:=NeedsCompile(APackage,APackage.Targets.TargetItems'+
  '[i]);'#010+
  '         ','   if Result then'#010+
  '              begin'#010+
  '              CompileReason:=Format(SDbgTargetHasToBeCompiled,[APackag'+
  'e.Targets.TargetItems[i].Name]);'#010+
  '              break;'#010+
  '              end;'#010+
  '          end;'#010+
  '      Finally'#010+
  '        GPathPrefix := '#039#039';'#010+
  '   ','   end;'#010+
  '    end;'#010+
  #010+
  '  if result then'#010+
  '    Log(vlDebug,SDbgMustCompile,[APackage.Name, CompileReason]);'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.CheckExternalPackage(const APackageName,'#010+
  '  ForPackageName: String; ErrorOnFailure: boolean): TPackage;'#010+
  'var'#010+
  '  S ',': String;'#010+
  '  F : String;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  // Already checked?'#010+
  '  I:=ExternalPackages.IndexOfName(APackageName);'#010+
  '  if I<>-1 then'#010+
  '    begin'#010+
  '      result:=ExternalPackages.PackageItems[I];'#010+
  '      exit;'#010+
  '    end;'#010+
  '  // Create new external pack','age'#010+
  '  Result:=ExternalPackages.AddPackage(APackageName);'#010+
  '  Result.FTargetState:=tsNotFound;'#010+
  '  // Load unit config'#010+
  '  S:=GetUnitDir(Result);'#010+
  '  if S<>'#039#039' then'#010+
  '    begin'#010+
  '      Log(vldebug, SDbgExternalDependency, [APackageName,S]);'#010+
  '      // Load',' unit config if it exists'#010+
  '      F:=result.UnitConfigFileName;'#010+
  '      if (F<>'#039#039') then'#010+
  '        begin'#010+
  '          Log(vlDebug, Format(SDbgLoading, [F]));'#010+
  '          Result.LoadUnitConfigFromFile(F);'#010+
  '          result.SetDefaultPackageVariant;'#010+
  '     ','     result.UnitDir:=ConcatPaths([result.UnitDir,Result.GetPack'+
  'ageUnitInstallDir(Defaults.CPU, Defaults.OS, Defaults.SubTarget)]);'#010+
  '        end;'#010+
  '      // Check recursive implicit dependencies'#010+
  '      CompileDependencies(Result);'#010+
  '    end'#010+
  '  else',' if ErrorOnFailure then'#010+
  '    Error(SErrDependencyNotFound,[APackageName,ForPackageName]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CompileDependencies(APackage: TPackage);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  D : TDependency;'#010+
  'begin'#010+
  '  For I:=0 to APacka','ge.Dependencies.Count-1 do'#010+
  '    begin'#010+
  '      D:=APackage.Dependencies[i];'#010+
  '      if (D.DependencyType=depPackage) and'#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '        begin'#010+
  '          P:=TPackage(D.Target);'#010+
  '          I','f Assigned(P) then'#010+
  '            begin'#010+
  '              if (Defaults.CPU in P.CPUs) and (Defaults.OS in P.OSes) '+
  'then'#010+
  '                begin'#010+
  '                  case P.State of'#010+
  '                    tsNeutral :'#010+
  '                      MaybeCompile(P);'#010+
  ' ','                   tsConsidering :'#010+
  '                      Log(vlWarning,SWarnCircularPackageDependency,[AP'+
  'ackage.Name,P.Name]);'#010+
  '                    tsNoCompile,tsCompiled,tsInstalled,tsNotFound:'#010+
  '                      ; // avoid compiler warn','ing'#010+
  '                  end;'#010+
  '                end'#010+
  '              else'#010+
  '                Log(vlWarning,SWarnDependOnOtherPlatformPackage,[APack'+
  'age.Name, D.Value, MakeTargetString(Defaults.CPU, Defaults.OS)]);'#010+
  '            end'#010+
  '          else'#010+
  '       ','     begin'#010+
  '              D.Target:=CheckExternalPackage(D.Value, APackage.Name, t'+
  'rue);'#010+
  '              P:=TPackage(D.Target);'#010+
  '            end;'#010+
  '          if (D.RequireChecksum<>$ffffffff) and (D.RequireChecksum<>0)'+
  ' and'#010+
  '             (P.Installe','dChecksum<>$ffffffff) and'#010+
  '             (P.InstalledChecksum<>D.RequireChecksum) then'#010+
  '            Log(vlWarning,SDbgPackageChecksumChanged,[APackage.Name, P'+
  '.Name]);'#010+
  '          APackage.InheritPackageVariantsFromDependency(P);'#010+
  '        end;'#010+
  '   ',' end;'#010+
  'end;'#010+
  #010+
  'function TBuildEngine.CheckDependencies(APackage: TPackage; ErrorOnFai'+
  'lure: boolean): TCheckDependencyResult;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  D : TDependency;'#010+
  'begin'#010+
  '  result := cdAvailable;'#010+
  '  For I:=0 to APackage.Dependenci','es.Count-1 do'#010+
  '    begin'#010+
  '      D:=APackage.Dependencies[i];'#010+
  '      if (D.DependencyType=depPackage) and'#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '        begin'#010+
  '          P:=TPackage(D.Target);'#010+
  '          If Assigned(P)',' then'#010+
  '            begin'#010+
  '              if (Defaults.CPU in P.CPUs) and (Defaults.OS in P.OSes) '+
  'then'#010+
  '                begin'#010+
  '                  case P.State of'#010+
  '                    tsNeutral :'#010+
  '                      result := cdNotYetAvailable;'#010+
  '  ','                  tsConsidering :'#010+
  '                      Log(vlWarning,SWarnCircularPackageDependency,[AP'+
  'ackage.Name,P.Name]);'#010+
  '                    tsNoCompile,tsCompiled,tsInstalled,tsNotFound:'#010+
  '                      ; // Avoid compiler warni','ng'#010+
  '                  end;'#010+
  '                end'#010+
  '              else'#010+
  '                Log(vlWarning,SWarnDependOnOtherPlatformPackage,[APack'+
  'age.Name, D.Value, MakeTargetString(Defaults.CPU, Defaults.OS)]);'#010+
  '            end'#010+
  '          else'#010+
  '        ','    begin'#010+
  '              D.Target:=CheckExternalPackage(D.Value, APackage.Name, E'+
  'rrorOnFailure);'#010+
  '              P:=TPackage(D.Target);'#010+
  '            end;'#010+
  '          if (D.RequireChecksum<>$ffffffff) and'#010+
  '             (P.InstalledChecksum<>$ffffff','ff) and'#010+
  '             (P.InstalledChecksum<>D.RequireChecksum) then'#010+
  '            Log(vldebug,SDbgPackageChecksumChanged,[APackage.Name, P.N'+
  'ame]);'#010+
  '          APackage.InheritPackageVariantsFromDependency(P);'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedu','re TBuildEngine.Compile(APackage: TPackage);'#010+
  'Var'#010+
  '  T : TTarget;'#010+
  '  I : Integer;'#010+
  '  aPath,UC: string;'#010+
  '  RegenerateUnitconfigFile: boolean;'#010+
  '  BUName: string;'#010+
  #010+
  '  procedure CompileBuildUnit;'#010+
  '  var'#010+
  '    I: Integer;'#010+
  '    T: TTarget;'#010+
  '    L: TStrings;'#010,
  '    F: Text;'#010+
  '    Dep : TDependency;'#010+
  '    aUnitName, aUnitSourceDir : string;'#010+
  '    CompilationFailed: Boolean;'#010+
  #010+
  '  begin'#010+
  '    if (APackage.FBUTarget.Dependencies.Count>0) then'#010+
  '      begin'#010+
  '        Log(vldebug, Format(SDbgGenerateBuildUnit, [APack','age.FBUTarg'+
  'et.Name]));'#010+
  '        aPath:=AddPathPrefix(APackage,APackage.GetUnitsOutputDir(Defau'+
  'lts.CompileTarget));'#010+
  '        system.Assign(F,IncludeTrailingPathDelimiter(apath)+APackage.F'+
  'BUTarget.FTargetSourceFileName);'#010+
  '        Rewrite(F);'#010+
  '   ','     writeln(F,'#039'unit '#039' + APackage.FBUTarget.Name +'#039';'#039+
  ');'#010+
  '        writeln(F,'#039'interface'#039');'#010+
  '        writeln(F,'#039'uses'#039');'#010+
  '        for i := 0 to APackage.FBUTarget.Dependencies.Count-1 do'#010+
  '          begin'#010+
  '            if i<>0 then'#010+
  '              write','(F,'#039','#039');'#010+
  '            Dep:=APackage.FBUTarget.Dependencies.Dependencies[i];'#010+
  '            aUnitName:=Dep.Value;'#010+
  #009'    aUnitSourceDir:=ExtractFileDir(Dep.TargetFileName);'#010+
  '            if aUnitSourceDir='#039#039' then'#010+
  '              APackage.UnitPath.Add(',#039'.'#039')'#010+
  '            else'#010+
  '              APackage.UnitPath.Add(aUnitSourceDir);'#010+
  '            writeln(F,aUnitName);'#010+
  '          end;'#010+
  '        writeln(F,'#039';'#039');'#010+
  '        writeln(F,'#039'implementation'#039');'#010+
  '        writeln(F,'#039'end.'#039');'#010+
  #010+
  '        system.close(F);'#010+
  #010+
  ' ','       APackage.FBuildMode:=bmOneByOne;'#010+
  '        Compilationfailed:=false;'#010+
  '        APackage.FBUTarget.FTargetSourceFileName:=IncludeTrailingPathD'+
  'elimiter(APackage.GetUnitsOutputDir(Defaults.CompileTarget))+APackage.'+
  'FBUTarget.FTargetSourceFil','eName;'#010+
  '        try'#010+
  '          try'#010+
  '            Compile(APackage,APackage.FBUTarget);'#010+
  '          except'#010+
  '            on E: Exception do'#010+
  '              begin'#010+
  '                Log(vlError,E.Message);'#010+
  '                Compilationfailed:=true;'#010+
  '        ','      end;'#010+
  '          end;'#010+
  '        finally'#010+
  '          if CompilationFailed then'#010+
  '            begin'#010+
  '              Log(vlDebug,Format(SDbgBuildUnitFailure,[APackage.FBUTar'+
  'get.FTargetSourceFileName]));'#010+
  '              // Raise failure exception aga','in.'#010+
  '              Error(SErrBuildUnitCompilation,[APackage.FBUTarget.FTarg'+
  'etSourceFileName]);'#010+
  '            end'#010+
  '          else'#010+
  '            begin'#010+
  '            // Delete temporary build-unit files'#010+
  '            L := TStringList.Create;'#010+
  '           ',' try'#010+
  '              aPath:=AddPathPrefix(APackage,APackage.GetUnitsOutputDir'+
  '(Defaults.CompileTarget));'#010+
  '              APackage.FBUTarget.GetCleanFiles(L,IncludeTrailingPathDe'+
  'limiter(aPath),'#039#039',Defaults.CompileTarget);'#010+
  '              L.Add(AddPa','thPrefix(APackage,APackage.FBUTarget.FTarge'+
  'tSourceFileName));'#010+
  '              CmdDeleteFiles(L);'#010+
  '            finally'#010+
  '              L.Free;'#010+
  '            end;'#010+
  '          end;'#010+
  '        end;'#010+
  '      end;'#010+
  #010+
  '    For I:=0 to APackage.Targets.Count-1 do'#010+
  '  ','    begin'#010+
  '        T:=APackage.Targets.TargetItems[i];'#010+
  '        if (T.TargetType = ttUnit) and (TargetOK(T, Defaults.CompileTa'+
  'rget)) then'#010+
  '          begin'#010+
  '            If Assigned(T.AfterCompile) then'#010+
  '              T.AfterCompile(T);'#010+
  '          ','  ExecuteCommands(T.Commands,caAfterCompile);'#010+
  '          end'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  '  procedure ProcessCompileTarget;'#010+
  '  begin'#010+
  '    if TargetOK(T, Defaults.CompileTarget) then'#010+
  '      begin'#010+
  '        if T.State=tsNeutral then'#010+
  '          MaybeCompile(AP','ackage,T);'#010+
  '        // If a target is compiled, re-generate the UnitConfigFile'#010+
  '        if T.FTargetState<>tsNoCompile then'#010+
  '          RegenerateUnitconfigFile:= True;'#010+
  '      end'#010+
  '    else'#010+
  '      begin'#010+
  '        if not(Defaults.CPU in T.CPUs) then'#010,
  '          Log(vldebug, Format(SDbgSkippingTargetWrongCPU, [T.Name, CPU'+
  'sToString(T.CPUs)]));'#010+
  '        if not(Defaults.OS in T.OSes) then'#010+
  '          Log(vldebug, Format(SDbgSkippingTargetWrongOS, [T.Name, OSes'+
  'ToString(T.OSes)]));'#010+
  '        if (De','faults.SubTarget<>'#039#039') and not T.SubTargetAllowed'+
  '(Defaults.SubTarget) then'#010+
  '          Log(vldebug, Format(SDbgSkippingTargetWrongSubTarget, [T.Nam'+
  'e, T.SubTargetsAsString]));'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  'begin'#010+
  '  log(vlWarning,SWarnStartCompilingPackage,','[APackage.Name, Defaults.'+
  'Target]);'#010+
  #010+
  '  case Defaults.BuildMode of'#010+
  '    bmOneByOne:  begin'#010+
  '                   if (bmOneByOne in APackage.SupportBuildModes) then'#010+
  '                     APackage.FBuildMode:=bmOneByOne'#010+
  '                   else if bm','BuildUnit in APackage.SupportBuildModes'+
  ' then'#010+
  '                     begin'#010+
  '                       log(vlInfo,SInfoFallbackBuildmodeBU);'#010+
  '                       APackage.FBuildMode:=bmBuildUnit;'#010+
  '                     end'#010+
  '                   else'#010+
  ' ','                    raise exception.create(SErrUnsupportedBuildmode'+
  ');'#010+
  '                 end;'#010+
  '    bmBuildUnit: begin'#010+
  '                   // When bmBuildUnit is supported by the package use'+
  ' a buildunit.'#010+
  '                   // Unless there is onl','y one target and bmOneByOne'+
  ' is also supported'#010+
  '                   if (bmBuildUnit in APackage.SupportBuildModes) and'#010+
  '                      not ((APackage.Targets.Count=1) and (bmOneByOne '+
  'in APackage.SupportBuildModes)) then'#010+
  '                 ','    APackage.FBuildMode:=bmBuildUnit'#010+
  '                   else if bmOneByOne in APackage.SupportBuildModes th'+
  'en'#010+
  '                     begin'#010+
  '                       log(vlInfo,SInfoFallbackBuildmode);'#010+
  '                       APackage.FBuildMode:=','bmOneByOne'#010+
  '                     end'#010+
  '                   else'#010+
  '                     raise exception.create(SErrUnsupportedBuildmode);'+
  #010+
  '                 end;'#010+
  '  end;'#010+
  #010+
  '  GPathPrefix:=APackage.Directory;'#010+
  '  Try'#010+
  '    CreateOutputDir(APackage);'#010+
  '    Add','PackageMacrosToDictionary(APackage, APackage.Dictionary);'#010+
  '    DoBeforeCompile(APackage);'#010+
  '    RegenerateUnitconfigFile:=False;'#010+
  '    if APackage.BuildMode=bmBuildUnit then'#010+
  '      begin'#010+
  '        APackage.FBUTargets := TTargets.Create(TTarget);'#010+
  '  ','      if (Defaults.BuildOS in AllLimit83fsOses) or'#010+
  '           (Defaults.OS in AllLimit83fsOses) then'#010+
  '          BUName := '#039'BUnit.pp'#039#010+
  '        else'#010+
  '          BUName := '#039'BuildUnit_'#039'+StringReplace(APackage.Name,'#039+
  '-'#039','#039'_'#039',[rfReplaceAll])+'#039'.pp'#039';'#010+
  '   ','     APackage.FBUTarget := APackage.FBUTargets.AddUnit(BUName);'#010+
  '        APackage.FBUTarget.FTargetSourceFileName := APackage.FBUTarget'+
  '.SourceFileName;'#010+
  '      end;'#010+
  '    For I:=0 to APackage.Targets.Count-1 do'#010+
  '      begin'#010+
  '        T:=APackage.Ta','rgets.TargetItems[i];'#010+
  '        case T.TargetType of'#010+
  '        ttUnit:'#010+
  '          begin'#010+
  '            ProcessCompileTarget;'#010+
  '          end;'#010+
  '        ttSharedLibrary,'#010+
  '        ttProgram,'#010+
  '        ttFPDoc:'#010+
  '          begin // do nothing, programs are com','piled later'#010+
  '          end;'#010+
  '        else'#010+
  '          log(vldebug, SDbgTargetIsNotAUnitOrProgram,[T.Name]);'#010+
  '        end;'#010+
  '      end;'#010+
  #010+
  '    if APackage.BuildMode=bmBuildUnit then'#010+
  '      CompileBuildUnit;'#010+
  #010+
  '    FreeAndNil(APackage.FBUTarget);'#010+
  #010+
  '    Fo','r I:=0 to APackage.Targets.Count-1 do'#010+
  '      begin'#010+
  '        T:=APackage.Targets.TargetItems[i];'#010+
  '        if T.TargetType in [ttProgram,ttSharedLibrary] then'#010+
  '          begin'#010+
  '            ProcessCompileTarget;'#010+
  '          end;'#010+
  '      end;'#010+
  #010+
  '    if De','faults.BuildExamples then'#010+
  '      For I:=0 to APackage.Targets.Count-1 do'#010+
  '        begin'#010+
  '          T:=APackage.Targets.TargetItems[i];'#010+
  '          if T.TargetType in [ttExampleUnit,ttExampleProgram] then'#010+
  '            ProcessCompileTarget;'#010+
  '       ',' end;'#010+
  #010+
  '    if RegenerateUnitconfigFile then'#010+
  '      begin'#010+
  '        UC:=AddPathPrefix(APackage, APackage.GetUnitConfigOutputFilena'+
  'me(Defaults.CompileTarget));'#010+
  '        Log(vlInfo, Format(SDbgGenerating, [UC]));'#010+
  '        APackage.SaveUnitConfigToF','ile(UC,Defaults.CPU,Defaults.OS);'#010+
  '      end;'#010+
  #010+
  '    DoAfterCompile(APackage);'#010+
  '  Finally'#010+
  '    GPathPrefix:='#039#039';'#010+
  '  end;'#010+
  '  inc(FProgressCount);'#010+
  '  if FProgressMax>0 then'#010+
  '    log(vlWarning,SWarnCompilingPackagecompleteProgress,[(FProgressCou'+
  'nt)/FPro','gressMax * 100, APackage.Name])'#010+
  '  else'#010+
  '    log(vlWarning,SWarnCompilingPackagecomplete,[APackage.Name]);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.MaybeCompile(APackage: TPackage);'#010+
  'begin'#010+
  '  if ReadyToCompile(APackage) then'#010+
  '    begin'#010+
  '      Compile(APackag','e);'#010+
  '      APackage.FTargetState:=tsCompiled;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function TBuildEngine.ReadyToCompile(APackage: TPackage) : Boolean;'#010+
  'begin'#010+
  '  result := False;'#010+
  '  if APackage.State in [tsCompiled, tsNoCompile] then'#010+
  '    begin'#010+
  '      Log(vlInfo,SInfoP','ackageAlreadyProcessed,[APackage.Name]);'#010+
  '      Exit;'#010+
  '    end;'#010+
  '  if APackage.State<>tsNeutral then'#010+
  '    Error(SErrInvalidState,[APackage.Name]);'#010+
  '  Log(vlDebug,SDbgConsideringPackage,[APackage.Name]);'#010+
  '  try'#010+
  '  LogIndent;'#010+
  '  if Defaults.ThreadsAm','ount=-1 then'#010+
  '    APackage.FTargetState:=tsConsidering;'#010+
  '  ResolveDependencies(APackage.Dependencies,(APackage.Collection as TP'+
  'ackages));'#010+
  '  // When multiple threads are used, delay the compilation of the pack'+
  'age when'#010+
  '  // there are unsolved d','ependencies. When no threads are used, comp'+
  'ile all'#010+
  '  // dependencies.'#010+
  #010+
  '  if Defaults.ThreadsAmount=-1 then'#010+
  '    CompileDependencies(APackage)'#010+
  '  else if CheckDependencies(APackage, true)=cdNotYetAvailable then'#010+
  '    begin'#010+
  '      log(vlInfo,'#039'Dela','ying package '#039'+apackage.name);'#010+
  '      //LogUnIndent; Done in Finally below'#010+
  '      result := False;'#010+
  '      Exit;'#010+
  '    end;'#010+
  '  APackage.SetDefaultPackageVariant;'#010+
  #010+
  '  ResolveFileNames(APackage,Defaults.CPU,Defaults.OS,True,False);'#010+
  '  If NeedsCompile(','APackage) then'#010+
  '    result := True'#010+
  '  else'#010+
  '    begin'#010+
  '      APackage.FTargetState:=tsNoCompile;'#010+
  '      inc(FProgressCount);'#010+
  '    end;'#010+
  '  finally'#010+
  '  LogUnIndent;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.InstallPackageFiles(APAckage: TPackage; tt: TTar'+
  'ge','tTypes;'#010+
  '  const Dest: String; const InstallMode: TInstallMode): Boolean;'#010+
  'Var'#010+
  '  List : TStringList;'#010+
  'begin'#010+
  '  Result:=False;'#010+
  '  List:=TStringList.Create;'#010+
  '  Try'#010+
  '    APackage.GetInstallFiles(List,tt,Defaults.CompileTarget);'#010+
  '    if (List.Count>0) ','then'#010+
  '      begin'#010+
  '        Result:=True;'#010+
  '        case InstallMode of'#010+
  '          imInstall:   CmdCopyFiles(List,Dest,APackage);'#010+
  '          imUnInstall: CmdDeleteDestFiles(List,Dest);'#010+
  '        end;'#010+
  '      end;'#010+
  '  Finally'#010+
  '    List.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010,
  'function TBuildEngine.GetUnitConfigFilesInstallDir(ABaseDir: string): '+
  'String;'#010+
  'begin'#010+
  '  result := FixPath(ABaseDir,True)+Defaults.UnitConfigFilesInstallDir;'+
  #010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.InstallUnitConfigFile(APAckage: TPackage; const'+
  ' Dest: St','ring);'#010+
  'Var'#010+
  '  List : TStringList;'#010+
  '  ConfigFileName: String;'#010+
  '  ConfigFileContent: TStrings;'#010+
  '  Index: integer;'#010+
  'begin'#010+
  '  ConfigFileName:=APackage.GetUnitConfigOutputFilename(Defaults.Compil'+
  'eTarget);'#010+
  '  List:=TStringList.Create;'#010+
  '  Try'#010+
  '    if Defau','lts.FPUnitSourcePath<>'#039#039' then'#010+
  '      begin'#010+
  '        ConfigFileContent := TStringList.Create;'#010+
  '        try'#010+
  '          ConfigFileContent.LoadFromFile(AddPathPrefix(APAckage, Confi'+
  'gFileName));'#010+
  '          if Defaults.FPUnitSourcePath='#039'0'#039' then'#010+
  '      ','      begin'#010+
  '              Index := ConfigFileContent.IndexOfName(KeySourcePath);'#010+
  '              if Index > -1 then'#010+
  '                ConfigFileContent.Delete(Index)'#010+
  '            end'#010+
  '          else'#010+
  '            ConfigFileContent.Values[KeySourceP','ath] := Defaults.FPUn'+
  'itSourcePath;'#010+
  '          ConfigFileContent.SaveToFile(AddPathPrefix(APAckage, ConfigF'+
  'ileName));'#010+
  '        finally'#010+
  '          ConfigFileContent.Free;'#010+
  '        end;'#010+
  '      end;'#010+
  '    List.Values[ConfigFileName] := APAckage.Name +',' FpmkExt;'#010+
  '    CmdCopyFiles(List,Dest,APackage);'#010+
  '  Finally'#010+
  '    List.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.InstallPackageFpcFile(APackage: TPackage; const'+
  ' Dest: String);'#010+
  #010+
  'var'#010+
  '  lContent, lList: TStringList;'#010+
  '  lFileName, lRequires: String',';'#010+
  '  lIndex: Integer;'#010+
  '  lDependency: TDependency;'#010+
  #010+
  'begin'#010+
  '  lRequires:='#039'rtl'#039';'#010+
  '  for lIndex:=0 to APackage.Dependencies.Count-1 do'#010+
  '    begin'#010+
  '      lDependency:=APackage.Dependencies[lIndex];'#010+
  '      if (lDependency.DependencyType=depPackage)'#010+
  '   ','      and (Defaults.CPU in lDependency.CPUs) and (Defaults.OS in '+
  'lDependency.OSes)'#010+
  '         and not SameText(lDependency.Value,'#039'rtl'#039') then'#010+
  '        lRequires:=lRequires+'#039' '#039'+lDependency.Value;'#010+
  '    end;'#010+
  '  lFileName:=IncludeTrailingPathDelimite','r(APackage.GetUnitsOutputDir'+
  '(Defaults.CompileTarget))+PackageFpcFile;'#010+
  '  lContent:=TStringList.Create;'#010+
  '  try'#010+
  '    lContent.Add('#039'[package]'#039');'#010+
  '    lContent.Add('#039'name='#039'+APackage.Name);'#010+
  '    lContent.Add('#039'version='#039'+APackage.Version);'#010+
  '    lContent.','Add('#039'[require]'#039');'#010+
  '    lContent.Add('#039'packages_'#039'+OSToString(Defaults.OS)+'#039'_'#039+
  '+CPUToString(Defaults.CPU)+'#039'='#039'+lRequires);'#010+
  '    lContent.SaveToFile(AddPathPrefix(APackage,lFileName));'#010+
  '  finally'#010+
  '    lContent.Free;'#010+
  '  end;'#010+
  '  lList:=TStringList.Create',';'#010+
  '  try'#010+
  '    lList.Add(lFileName);'#010+
  '    CmdCopyFiles(lList,Dest,APackage);'#010+
  '  finally'#010+
  '    lList.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.InstallPackageSourceFiles(APAckage: TPackage;'#010+
  '  stt: TSourceTypes; ttt: TTargetTypes; const Dest: String;',#010+
  '  const InstallMode: TInstallMode): Boolean;'#010+
  'Var'#010+
  '  List : TStringList;'#010+
  'begin'#010+
  '  Result:=False;'#010+
  '  List:=TStringList.Create;'#010+
  '  Try'#010+
  '    APackage.GetInstallSourceFiles(List,stt,ttt);'#010+
  '    if (List.Count>0) then'#010+
  '      begin'#010+
  '        Result:=True;'#010,
  '        case InstallMode of'#010+
  '          imInstall:   CmdCopyFiles(List,Dest,APackage);'#010+
  '          imUnInstall: CmdDeleteDestFiles(List,Dest);'#010+
  '        end;'#010+
  '      end;'#010+
  '  Finally'#010+
  '    List.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoBeforeInstall','(APackage: TPackage);'#010+
  'begin'#010+
  '  ExecuteCommands(APackage.Commands,caBeforeInstall);'#010+
  '  If Assigned(APackage.BeforeInstall) then'#010+
  '    APackage.BeforeInstall(APackage);'#010+
  '  If Assigned(APackage.BeforeInstallProc) then'#010+
  '    APackage.BeforeInstallProc','(APackage);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoAfterInstall(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterInstall) then'#010+
  '    APackage.AfterInstall(APackage);'#010+
  '  If Assigned(APackage.AfterInstallProc) then'#010+
  '    APackage.AfterInstallProc(A','Package);'#010+
  '  ExecuteCommands(APackage.Commands,caAfterInstall);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Install(APackage: TPackage; AnArchiveFiles: boo'+
  'lean);'#010+
  'Var'#010+
  '  D : String;'#010+
  '  B : Boolean;'#010+
  'begin'#010+
  '  If (Apackage.State<>tsCompiled) then'#010+
  '    MaybeCompil','e(APackage);'#010+
  '  if APackage.State=tsNeutral then'#010+
  '    Error(SErrPackageNotCompiled,[APackage.Name]);'#010+
  '  Log(vlCommand,SInfoInstallingPackage,[APackage.Name]);'#010+
  '  if AnArchiveFiles then'#010+
  '    FinishArchive(APackage);'#010+
  '  try'#010+
  '    If (APackage.Directo','ry<>'#039#039') then'#010+
  '      GPathPrefix := APackage.Directory;'#010+
  '    if AnArchiveFiles then'#010+
  '      begin'#010+
  '        FOnCopyFile:=@AddFileToArchive;'#010+
  '        FOnFinishCopy:=@FinishArchive;'#010+
  '      end;'#010+
  '    DoBeforeInstall(APackage);'#010+
  '    // units'#010+
  '    B:=false;',#010+
  '    AddPackageMacrosToDictionary(APackage, APackage.Dictionary);'#010+
  '    GlobalDictionary.AddVariable('#039'unitinstalldir'#039', FixPath(APa'+
  'ckage.Dictionary.ReplaceStrings(Defaults.UnitInstallDir), False));'#010+
  '    GlobalDictionary.AddVariable('#039'packageunit','installdir'#039',APa'+
  'ckage.GetPackageUnitInstallDir(Defaults.CPU,Defaults.OS, Defaults.SubT'+
  'arget));'#010+
  #010+
  '    D:=FixPath(Defaults.Prefix,true);'#010+
  '    // This is to install the TPackage.Installfiles, which are not rel'+
  'ated to any'#010+
  '    // target'#010+
  '    if Inst','allPackageFiles(APackage,[],D, imInstall) then'#010+
  '      B:=true;'#010+
  '    D:=FixPath(APackage.Dictionary.ReplaceStrings(Defaults.UnitInstall'+
  'Dir), True)+ExcludeLeadingPathDelimiter(APackage.GetPackageUnitInstall'+
  'Dir(Defaults.CPU,Defaults.OS, Defaults','.SubTarget));'#010+
  '    if InstallPackageFiles(APackage,[ttUnit, ttImplicitUnit],D, imInst'+
  'all) then'#010+
  '      begin'#010+
  '      B:=true;'#010+
  '      InstallPackageFpcFile(APackage,IncludeTrailingPathDelimiter(D));'+
  #010+
  '      end;'#010+
  '    // By default do not install the ','examples. Maybe add an option f'+
  'or this later'#010+
  '    //if InstallPackageFiles(APAckage,ttExampleUnit,D) then'#010+
  '    //  B:=true;'#010+
  '    // Unit (dependency) configuration if there were units installed'#010+
  '    D:=FixPath(APackage.Dictionary.ReplaceStrings','(GetUnitConfigFiles'+
  'InstallDir(Defaults.BaseInstallDir)), True);'#010+
  '    if B then'#010+
  '      InstallUnitConfigFile(APackage,D);'#010+
  '    // Programs'#010+
  '    D:=IncludeTrailingPathDelimiter(Defaults.BinInstallDir);'#010+
  '    InstallPackageFiles(APAckage,[ttProgram]',',D, imInstall);'#010+
  '    // Shared libraries'#010+
  '    D:=IncludeTrailingPathDelimiter(Defaults.LibInstallDir);'#010+
  '    InstallPackageFiles(APAckage,[ttSharedLibrary],D, imInstall);'#010+
  '    //InstallPackageFiles(APAckage,ttExampleProgram,D);'#010+
  '    // Documentat','ion'#010+
  '    D:=FixPath(APackage.Dictionary.ReplaceStrings(Defaults.DocInstallD'+
  'ir), True);'#010+
  '    InstallPackageSourceFiles(APackage,[stDoc],[],D, imInstall);'#010+
  '    // Examples'#010+
  '    if Defaults.InstallExamples then'#010+
  '      begin'#010+
  '        D:=FixPath(APack','age.Dictionary.ReplaceStrings(Defaults.Examp'+
  'lesInstallDir), True);'#010+
  '        InstallPackageSourceFiles(APackage,[stExample],[ttExampleProgr'+
  'am,ttExampleUnit],D, imInstall);'#010+
  '      end;'#010+
  '    // Done.'#010+
  '    APackage.FTargetState:=tsInstalled;'#010+
  '    Do','AfterInstall(APackage);'#010+
  '    if AnArchiveFiles then'#010+
  '      begin'#010+
  '      FOnCopyFile:=nil;'#010+
  '      FOnFinishCopy:=nil;'#010+
  '      end;'#010+
  '  Finally'#010+
  '    If (APackage.Directory<>'#039#039') then'#010+
  '      GPathPrefix := '#039#039';'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.UnInsta','ll(APackage: TPackage);'#010+
  'Var'#010+
  '  D : String;'#010+
  'begin'#010+
  '  CheckDependencies(APackage, False);'#010+
  '  ResolvePackagePaths(APackage);'#010+
  '  APackage.SetDefaultPackageVariant;'#010+
  '  If (Apackage.State<>tsInstalled) then'#010+
  '    begin'#010+
  '    Error(SErrorPkgNotInstalled,[A','Package.Name]);'#010+
  '    exit;'#010+
  '    end;'#010+
  '  Log(vlCommand,SInfoUnInstallingPackage,[APackage.Name]);'#010+
  #010+
  '  //DoBeforeUnInstall(APackage);'#010+
  #010+
  '  // units'#010+
  '  AddPackageMacrosToDictionary(APackage, APackage.Dictionary);'#010+
  '  GlobalDictionary.AddVariable('#039'uniti','nstalldir'#039', FixPath(APa'+
  'ckage.Dictionary.ReplaceStrings(Defaults.UnitInstallDir), False));'#010+
  '  GlobalDictionary.AddVariable('#039'packageunitinstalldir'#039',APackag'+
  'e.GetPackageUnitInstallDir(Defaults.CPU,Defaults.OS, Defaults.SubTarge'+
  't));'#010+
  #010+
  '  D:=FixPath','(Defaults.Prefix,true);'#010+
  '  // This is to uninstall the TPackage.Installfiles, which are not rel'+
  'ated to any'#010+
  '  // target'#010+
  '  InstallPackageFiles(APackage,[],D,imUnInstall);'#010+
  '  D:=FixPath(APackage.Dictionary.ReplaceStrings(Defaults.UnitInstallDi'+
  'r)',', True)+APackage.GetPackageUnitInstallDir(Defaults.CPU,Defaults.OS'+
  ', Defaults.SubTarget);'#010+
  '  InstallPackageFiles(APackage,[ttUnit, ttImplicitUnit],D, imUnInstall'+
  ');'#010+
  '  SysDeleteFile(IncludeTrailingPathDelimiter(D)+PackageFpcFile);'#010+
  '  SysDeleteDi','rectory(D);'#010+
  #010+
  '  // Unit (dependency) configuration'#010+
  '  D:=FixPath(APackage.Dictionary.ReplaceStrings(GetUnitConfigFilesInst'+
  'allDir(Defaults.BaseInstallDir)), True);'#010+
  '  SysDeleteFile(D+APackage.Name+FpmkExt);'#010+
  #010+
  '  // Programs'#010+
  '  D:=IncludeTrailingPa','thDelimiter(Defaults.BinInstallDir);'#010+
  '  InstallPackageFiles(APAckage,[ttProgram],D, imUnInstall);'#010+
  '  SysDeleteDirectory(D);'#010+
  '  // Libraries'#010+
  '  D:=IncludeTrailingPathDelimiter(Defaults.LibInstallDir);'#010+
  '  InstallPackageFiles(APAckage,[ttSharedLibr','ary],D, imUnInstall);'#010+
  '  SysDeleteDirectory(D);'#010+
  '  // Documentation'#010+
  '  D:=FixPath(APackage.Dictionary.ReplaceStrings(Defaults.DocInstallDir'+
  '), True);'#010+
  '  InstallPackageSourceFiles(APackage,[stDoc],[],D, imUnInstall);'#010+
  '  SysDeleteDirectory(D);'#010+
  '  //',' Examples'#010+
  '  if Defaults.InstallExamples then'#010+
  '    begin'#010+
  '      D:=FixPath(APackage.Dictionary.ReplaceStrings(Defaults.ExamplesI'+
  'nstallDir), True);'#010+
  '      InstallPackageSourceFiles(APackage,[stExample],[ttExampleProgram'+
  ',ttExampleUnit],D, imUnIns','tall);'#010+
  '      SysDeleteDirectory(D);'#010+
  '    end;'#010+
  '  // Done.'#010+
  '  APackage.FTargetState:=tsNeutral;'#010+
  #010+
  '  //DoAfterUnInstall(APackage);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoBeforeArchive(APackage: TPackage);'#010+
  'begin'#010+
  '  ExecuteCommands(APackage.Commands,caBefo','reArchive);'#010+
  '  If Assigned(APackage.BeforeArchive) then'#010+
  '    APackage.BeforeArchive(APackage);'#010+
  '  If Assigned(APackage.BeforeArchiveProc) then'#010+
  '    APackage.BeforeArchiveProc(APackage);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoAfterArchive(APackage: TPa','ckage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterArchive) then'#010+
  '    APackage.AfterArchive(APackage);'#010+
  '  If Assigned(APackage.AfterArchiveProc) then'#010+
  '    APackage.AfterArchiveProc(APackage);'#010+
  '  ExecuteCommands(APackage.Commands,caAfterArchive);'#010+
  'end;'#010+
  #010+
  #010,
  'procedure TBuildEngine.Archive(APackage: TPackage);'#010+
  'Var'#010+
  '  L : TStringList;'#010+
  '  A : String;'#010+
  '  i: integer;'#010+
  '  ICPU : TCPU;'#010+
  '  IOS  : TOS;'#010+
  '  aTarget : TCompileTarget;'#010+
  '{$ifdef HAS_UNIT_ZIPPER}'#010+
  '  ZipFile: TZipper;'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  'begin'#010+
  '  A:','=Defaults.ZipPrefix + APackage.FileName + MakeZipSuffix(cpuNone,'+
  ' osNone) + ZipExt;'#010+
  '  Log(vlInfo,SInfoArchivingPackage,[APackage.Name,A]);'#010+
  '  try'#010+
  '    If (APackage.Directory<>'#039#039') then'#010+
  '      GPathPrefix := APackage.Directory;'#010+
  '    DoBeforeArchiv','e(Apackage);'#010+
  '    AddPackageMacrosToDictionary(APackage, APackage.Dictionary);'#010+
  '    L:=TStringList.Create;'#010+
  '    L.Sorted:=true;'#010+
  '    L.Duplicates:=dupIgnore;'#010+
  '    Try'#010+
  '      // Add fpmake.pp & manifest.xml always'#010+
  '      L.Add(FPMakePPFile);'#010+
  '      ','L.Add(ManifestFile);'#010+
  '      //get all files from all targets'#010+
  '      for ICPU:=Low(TCPU) to high(TCPU) do'#010+
  '        for IOS:=Low(TOS) to high(TOS) do'#010+
  '          if OSCPUSupported[IOS,ICPU] then'#010+
  '            begin'#010+
  '              aTarget.OS:=IOS;'#010+
  '   ','           aTarget.CPU:=ICPU;'#010+
  '              aTarget.Subtarget:='#039#039';'#010+
  '            // Make sure that the package is resolved for each target'#010+
  '              ClearResolvedFileNames(APackage);'#010+
  '              ResolveFileNames(APackage,ICPU,IOS,false)',';'#010+
  '              APackage.GetArchiveFiles(L,aTarget);'#010+
  '            end;'#010+
  '      //from sources'#010+
  '      APackage.GetArchiveSourceFiles(L);'#010+
  #010+
  '      //show all files'#010+
  '      for i := 0 to L.Count-1 do'#010+
  '        Log(vlDebug, Format(SDbgArchivingFile, [L[i',']]));'#010+
  #010+
  '{$ifdef HAS_UNIT_ZIPPER}'#010+
  '      if not Assigned(ArchiveFilesProc) then'#010+
  '        begin'#010+
  '          ZipFile := TZipper.Create;'#010+
  '          try'#010+
  '            ZipFile.FileName:=A;'#010+
  '            A := APackage.Dictionary.ReplaceStrings(Defaults.FPre','fix'+
  ');'#010+
  '            if A <> '#039#039' then'#010+
  '              A:=IncludeTrailingPathDelimiter(A);'#010+
  '            for i := 0 to L.Count-1 do'#010+
  '              begin'#010+
  '                ZipFile.Entries.AddFileEntry(AddPathPrefix(APackage, L'+
  '[i]), A+L[i]);'#010+
  '            ','  end;'#010+
  '            ZipFile.ZipAllFiles;'#010+
  '          finally'#010+
  '            ZipFile.Free;'#010+
  '          end;'#010+
  '        end'#010+
  '      else'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  '        CmdArchiveFiles(L,A);'#010+
  '    Finally'#010+
  '      L.Free;'#010+
  '    end;'#010+
  '    DoAfterArchive(Apackage)',';'#010+
  '  Finally'#010+
  '    If (APackage.Directory<>'#039#039') then'#010+
  '      GPathPrefix := '#039#039';'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoBeforeClean(APackage: TPackage);'#010+
  'begin'#010+
  '  ExecuteCommands(APackage.Commands,caBeforeClean);'#010+
  '  If Assigned(APackage.BeforeClean) ','then'#010+
  '    APackage.BeforeClean(APackage);'#010+
  '  If Assigned(APackage.BeforeCleanProc) then'#010+
  '    APackage.BeforeCleanProc(APackage);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoAfterClean(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterClean) then'#010+
  '    ','APackage.AfterClean(APackage);'#010+
  '  If Assigned(APackage.AfterCleanProc) then'#010+
  '    APackage.AfterCleanProc(APackage);'#010+
  '  ExecuteCommands(APackage.Commands,caAfterClean);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Clean(APackage: TPackage; AllTargets: boolean',')'+
  ';'#010+
  'var'#010+
  '  ACPU: TCpu;'#010+
  '  AOS: TOS;'#010+
  '  aTarget : TCompileTarget;'#010+
  '  DirectoryList : TStringList;'#010+
  '  aDir : string;'#010+
  #010+
  'begin'#010+
  '  if not AllTargets and (not(Defaults.OS in APackage.OSes) or'#010+
  '     not (Defaults.CPU in APackage.CPUs))  then'#010+
  '    exit;'#010+
  '  aT','arget:=Default(TCompileTarget);'#010+
  '  Log(vlInfo,SInfoCleaningPackage,[APackage.Name]);'#010+
  '  try'#010+
  '    If (APackage.Directory<>'#039#039') then'#010+
  '      EnterDir(APackage.Directory);'#010+
  '    // Check for inherited options (packagevariants) from other packag'+
  'es'#010+
  '    ','if (Defaults.OS in APackage.OSes) and (Defaults.CPU in APackage.'+
  'CPUs) then'#010+
  '      begin'#010+
  '        ResolveDependencies(APackage.Dependencies, (APackage.Collectio'+
  'n as TPackages));'#010+
  '        CheckDependencies(APackage, False);'#010+
  '        APackage.SetD','efaultPackageVariant;'#010+
  '      end;'#010+
  '    DoBeforeClean(Apackage);'#010+
  '    AddPackageMacrosToDictionary(APackage, APackage.Dictionary);'#010+
  '    if AllTargets then'#010+
  '      begin'#010+
  '        // Remove the unit-and bin-directories completely. This is saf'+
  'er in ca','se of files'#010+
  '        // being renamed and such. See also bug 19655'#010+
  '        DirectoryList := TStringList.Create;'#010+
  '        try'#010+
  '          for ACPU:=low(TCpu) to high(TCpu) do if ACPU<>cpuNone then'#010+
  '            for AOS:=low(TOS) to high(TOS) do if',' AOS<>osNone then'#010+
  '              begin'#010+
  '                aTarget.OS:=aOS;'#010+
  '                aTarget.CPU:=aCPU;'#010+
  '                if OSCPUSupported[AOS,ACPU] and (AOS in APackage.OSes)'+
  ' and'#010+
  '                   (ACPU in APackage.CPUs) then'#010+
  '           ','       begin'#010+
  '                    // First perform a normal clean, to be sure that a'+
  'll files'#010+
  '                    // which are not in the units- or bin-dir are clea'+
  'ned. (like'#010+
  '                    // the .fpm file)'#010+
  '                    aTarget.S','ubTarget:=Defaults.SubTarget;'#010+
  '                    Clean(APackage, aTarget);'#010+
  '                    aTarget.SubTarget:='#039#039';'#010+
  '                    DirectoryList.Add(ExtractFileDir(APackage.GetUnits'+
  'OutputDir(aTarget)));'#010+
  '                    // We don',#039't know the full list of subtargets. '+
  'So we detect existing CPU-OS-NN directories.'#010+
  '                    For aDir in GetSubTargetDirs(APackage.GetUnitsOutp'+
  'utDir(aTarget)) do'#010+
  '                      DirectoryList.Add(aDir);'#010+
  '                    Di','rectoryList.Add(ExtractFileDir(APackage.GetBin'+
  'OutputDir(aTarget)));'#010+
  '                    For aDir in GetSubTargetDirs(APackage.GetUnitsOutp'+
  'utDir(aTarget)) do'#010+
  '                      DirectoryList.Add(aDir);'#010+
  '                  end;'#010+
  '             ',' end;'#010+
  '          CmdRemoveTrees(DirectoryList);'#010+
  '        finally'#010+
  '          DirectoryList.Free;'#010+
  '        end;'#010+
  '      end'#010+
  '    else'#010+
  '      Clean(APackage, Defaults.CompileTarget);'#010+
  '    CleanDocumentation(APackage);'#010+
  '    DoAfterClean(Apackage);'#010+
  '  Fina','lly'#010+
  '    log(vlInfo, SInfoCleanPackagecomplete, [APackage.Name]);'#010+
  '    If (APackage.Directory<>'#039#039') then'#010+
  '      EnterDir('#039#039');'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Clean(APackage: TPackage; const aTarget : TComp'+
  'ileTarget);'#010+
  'Var'#010+
  '  List,List2 : TStr','ingList;'#010+
  '  DirectoryList : TStringList;'#010+
  '  RemainingList : TStrings;'#010+
  '  i : longint;'#010+
  'begin'#010+
  '  List:=TUnsortedDuplicatesStringList.Create;'#010+
  '  List.Duplicates:=DupIgnore;'#010+
  '  try'#010+
  '    List.Add(APackage.GetUnitConfigOutputFilename(aTarget));'#010+
  '    APac','kage.GetCleanFiles(List,aTarget);'#010+
  '    if (List.Count>0) then'#010+
  '      begin'#010+
  '      CmdDeleteFiles(List);'#010+
  '      DirectoryList:=TUnsortedDuplicatesStringList.Create;'#010+
  '      DirectoryList.Duplicates:=DupIgnore;'#010+
  '      try'#010+
  '        GetDirectoriesFromF','ilelist(List,DirectoryList);'#010+
  '        CmdRemoveDirs(DirectoryList);'#010+
  #010+
  '        DirectoryList.Clear;'#010+
  '        if DirectoryExists(APackage.GetBinOutputDir(aTarget)) then'#010+
  '          DirectoryList.Add(APackage.GetBinOutputDir(aTarget));'#010+
  '        if D','irectoryExists(APackage.GetExamplesOutputDir(aTarget)) t'+
  'hen'#010+
  '          DirectoryList.Add(APackage.GetExamplesOutputDir(aTarget));'#010+
  '        if DirectoryExists(APackage.GetUnitsOutputDir(aTarget)) then'#010+
  '          DirectoryList.Add(APackage.GetUn','itsOutputDir(aTarget));'#010+
  '        CmdRemoveDirs(DirectoryList);'#010+
  #010+
  '        DirectoryList.Clear;'#010+
  #010+
  '        { force directory removal for units and bin dir if it ends wit'+
  'h /$fpc_target }'#010+
  '        if DirectoryExists(APackage.GetBinOutputDir(aTarget)',') and'#010+
  '           (MakeTargetString(aTarget)=ExtractFileName(ExcludeTrailingP'+
  'athDelimiter(APackage.GetBinOutputDir(aTarget)))) then'#010+
  '          begin'#010+
  '            Installer.Log(vlWarning,Format(SWarnRemovedNonEmptyDirecto'+
  'ry,[APackage.Directory+','APackage.GetBinOutputDir(aTarget)]));'#010+
  '            DirectoryList.Add(APackage.GetBinOutputDir(aTarget));'#010+
  '            RemainingList := TStringList.Create;'#010+
  '            List2:=TStringList.Create;'#010+
  '            SearchFiles(AllFilesMask, APackage.G','etBinOutputDir(aTarg'+
  'et), true, RemainingList);'#010+
  '            for i:=0 to RemainingList.Count-1 do'#010+
  '              begin'#010+
  '                if ExtractFileExt(Remaininglist[i])=PPUExt then'#010+
  '                  Installer.log(vlDebug,format('#039'File %s stil','l pr'+
  'esent, add corresponding entry to fpmake'#039',[RemainingList[i]]))'#010+
  '                else'#010+
  '                  Installer.log(vlDebug,format('#039'File %s still pres'+
  'ent'#039',[RemainingList[i]]));'#010+
  '                List2.Add(IncludeTrailingPathDelimiter(APac','kage.GetU'+
  'nitsOutputDir(aTarget))+Remaininglist[i]);'#010+
  '              end;'#010+
  '            CmdDeleteFiles(List2);'#010+
  '            List2.Free;'#010+
  '            RemainingList.Free;'#010+
  '            CmdRemoveTrees(DirectoryList);'#010+
  '            DirectoryList.Clear;'#010+
  '  ','        end;'#010+
  '        if DirectoryExists(APackage.GetUnitsOutputDir(aTarget)) and'#010+
  '           (MakeTargetString(aTarget)=ExtractFileName(ExcludeTrailingP'+
  'athDelimiter(APackage.GetUnitsOutputDir(aTarget)))) then'#010+
  '          begin'#010+
  '            Inst','aller.Log(vlWarning,Format(SWarnRemovedNonEmptyDirec'+
  'tory,[APackage.Directory+APackage.GetUnitsOutputDir(aTarget)]));'#010+
  '            DirectoryList.Add(APackage.GetUnitsOutputDir(aTarget));'#010+
  '            RemainingList := TStringList.Create;'#010+
  '      ','      List2:=TStringList.Create;'#010+
  '            SearchFiles(AllFilesMask, APackage.GetUnitsOutputDir(aTarg'+
  'et), true, RemainingList);'#010+
  '            for i:=0 to RemainingList.Count-1 do'#010+
  '              begin'#010+
  '                if ExtractFileExt(Remaini','nglist[i])=PPUExt then'#010+
  '                  Installer.log(vlDebug,format('#039'File %s still pres'+
  'ent, add corresponding entry to fpmake'#039',[RemainingList[i]]))'#010+
  '                else'#010+
  '                  Installer.log(vlDebug,format('#039'File %s still pres'+
  'ent',#039',[RemainingList[i]]));'#010+
  '                List2.Add(IncludeTrailingPathDelimiter(APackage.GetUni'+
  'tsOutputDir(aTarget))+RemainingList[i]);'#010+
  '              end;'#010+
  '            CmdDeleteFiles(List2);'#010+
  '            List2.free;'#010+
  '            RemainingList.F','ree;'#010+
  '            CmdRemoveTrees(DirectoryList);'#010+
  '            DirectoryList.Clear;'#010+
  '          end;'#010+
  '        { Also remove units/ or bin/ directory if empty }'#010+
  '        if IsDirectoryEmpty(ExtractFileDir(ExcludeTrailingPathDelimite'+
  'r(APackage.GetBi','nOutputDir(aTarget)))) then'#010+
  '          DirectoryList.Add(ExtractFileDir(ExcludeTrailingPathDelimite'+
  'r(APackage.GetBinOutputDir(aTarget))));'#010+
  '        if IsDirectoryEmpty(ExtractFileDir(ExcludeTrailingPathDelimite'+
  'r(APackage.GetUnitsOutputDir(aTa','rget)))) then'#010+
  '          DirectoryList.Add(ExtractFileDir(ExcludeTrailingPathDelimite'+
  'r(APackage.GetUnitsOutputDir(aTarget))));'#010+
  '        CmdRemoveDirs(DirectoryList);'#010+
  '      finally'#010+
  '        DirectoryList.Free;'#010+
  '      end;'#010+
  '      end;'#010+
  '  Finally'#010+
  '  ','  List.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CleanDocumentation(APackage: TPackage);'#010+
  #010+
  'var'#010+
  '  lFiles,lDirs : TStringList;'#010+
  '  lFormat : TFPDocFormat;'#010+
  '  lName : String;'#010+
  '  I : Integer;'#010+
  '  lHasDoc : Boolean;'#010+
  #010+
  'begin'#010+
  '  lHasDoc:=False;'#010+
  '  for I:=0',' to APackage.Targets.Count-1 do'#010+
  '    if APackage.Targets.TargetItems[I].TargetType=ttFPDoc then'#010+
  '      lHasDoc:=True;'#010+
  '  if not lHasDoc then'#010+
  '    Exit;'#010+
  '  lDirs:=nil;'#010+
  '  lFiles:=TStringList.Create;'#010+
  '  try'#010+
  '    lDirs:=TStringList.Create;'#010+
  '    for lFo','rmat in APackage.FPDocFormat do'#010+
  '      begin'#010+
  '      lName:=GetFPDocOutputName(APackage,lFormat);'#010+
  '      if SysDirectoryExists(lName) then'#010+
  '        lDirs.Add(lName)'#010+
  '      else if SysFileExists(lName) then'#010+
  '        lFiles.Add(lName);'#010+
  '      end;'#010+
  '  ','  CmdDeleteFiles(lFiles);'#010+
  '    CmdRemoveTrees(lDirs);'#010+
  '    lDirs.Clear;'#010+
  '    if IsDirectoryEmpty(ExcludeTrailingPathDelimiter(Defaults.FPDocOut'+
  'putDir)) then'#010+
  '      begin'#010+
  '      lDirs.Add(ExcludeTrailingPathDelimiter(Defaults.FPDocOutputDir))'+
  ';'#010+
  '  ','    CmdRemoveDirs(lDirs);'#010+
  '      end;'#010+
  '  finally'#010+
  '    lDirs.Free;'#010+
  '    lFiles.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.GetFPDocOutputName(APackage: TPackage; AFormat: '+
  'TFPDocFormat): String;'#010+
  #010+
  'begin'#010+
  '  Result:=Defaults.FPDocOutputDir+APackage.Nam','e;'#010+
  '  case AFormat of'#010+
  '    ffHtml      : ;'#010+
  '    ffHtm       : Result:=Result+'#039'-htm'#039';'#010+
  '    ffXHtml     : Result:=Result+'#039'-xhtml'#039';'#010+
  '    ffLaTex     : Result:=Result+'#039'.tex'#039';'#010+
  '    ffXMLStruct : Result:=Result+'#039'-xml'#039';'#010+
  '    ffChm       : Result:=Result+',#039'.chm'#039';'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Documentation(APackage: TPackage);'#010+
  #010+
  'const'#010+
  '  FormatNames : array[TFPDocFormat] of String = ('#039'html'#039','#039'h'+
  'tm'#039','#039'xhtml'#039','#039'latex'#039','#039'xml-struct'#039','#039'chm'#039+
  ');'#010+
  #010+
  'var'#010+
  '  lArgs,lRunArgs,lOpts : TStringList;'#010+
  '  lTarget :',' TTarget;'#010+
  '  lDep : TDependency;'#010+
  '  lFormat : TFPDocFormat;'#010+
  '  lCmd,lName,lValue : String;'#010+
  '  I,J : Integer;'#010+
  #010+
  'begin'#010+
  '  lOpts:=nil;'#010+
  '  lRunArgs:=nil;'#010+
  '  lArgs:=TStringList.Create;'#010+
  '  try'#010+
  '    for I:=0 to APackage.Targets.Count-1 do'#010+
  '      begin'#010+
  '      ','lTarget:=APackage.Targets.TargetItems[I];'#010+
  '      if lTarget.TargetType<>ttFPDoc then'#010+
  '        continue;'#010+
  '      for J:=0 to lTarget.Dependencies.Count-1 do'#010+
  '        begin'#010+
  '        lDep:=lTarget.Dependencies[J];'#010+
  '        if lDep.DependencyType=depU','nit then'#010+
  '          lArgs.Add('#039'--input='#039'+lDep.Value);'#010+
  '        end;'#010+
  '      lArgs.Add('#039'--input='#039'+lTarget.Directory+lTarget.Name+lTar'+
  'get.Extension);'#010+
  '      if lTarget.XML<>'#039#039' then'#010+
  '        lArgs.Add('#039'--descr='#039'+lTarget.XML);'#010+
  '      end;'#010+
  '    if (lArg','s.Count=0) or (APackage.FPDocFormat=[]) then'#010+
  '      Exit;'#010+
  '    Log(vlInfo,SInfoDocumentingPackage,[APackage.Name]);'#010+
  '    lArgs.Add('#039'--package='#039'+APackage.Name);'#010+
  '    if Defaults.FPDocOptions<>'#039#039' then'#010+
  '      begin'#010+
  '      lOpts:=TStringList.Create;'#010,
  '      lOpts.LoadFromFile(Defaults.FPDocOptions);'#010+
  '      for I:=0 to lOpts.Count-1 do'#010+
  '        begin'#010+
  '        lOpts.GetNameValue(I,lName,lValue);'#010+
  '        if (lName='#039#039') or SameText(lValue,'#039'false'#039') then'#010+
  '          continue;'#010+
  '        if (lValue='#039#039') ','or SameText(lValue,'#039'true'#039') then'+
  #010+
  '          lArgs.Add('#039'--'#039'+lName)'#010+
  '        else'#010+
  '          lArgs.Add('#039'--'#039'+lName+'#039'='#039'+lValue);'#010+
  '        end;'#010+
  '      end;'#010+
  '    lCmd:=ExeSearch('#039'fpdoc'#039',{$IFDEF FPC_DOTTEDUNITS}System.{$E'+
  'NDIF}SysUtils.GetEnvironmentvaria','ble('#039'PATH'#039'));'#010+
  '    if lCmd='#039#039' then'#010+
  '      lCmd:='#039'fpdoc'#039';'#010+
  '    lRunArgs:=TStringList.Create;'#010+
  '    if APackage.Directory<>'#039#039' then'#010+
  '      EnterDir(APackage.Directory);'#010+
  '    try'#010+
  '      if not ListMode then'#010+
  '        ForceDirectories(Defaults.FPDocOutput','Dir);'#010+
  '      for lFormat in APackage.FPDocFormat do'#010+
  '        begin'#010+
  '        lRunArgs.Clear;'#010+
  '        lRunArgs.Add('#039'--format='#039'+FormatNames[lFormat]);'#010+
  '        lRunArgs.Add('#039'--output='#039'+GetFPDocOutputName(APackage,l'+
  'Format));'#010+
  '        lRunArgs.AddStr','ings(lArgs);'#010+
  '        ExecuteCommand(lCmd,lRunArgs);'#010+
  '        end;'#010+
  '    finally'#010+
  '      if APackage.Directory<>'#039#039' then'#010+
  '        EnterDir('#039#039');'#010+
  '    end;'#010+
  '  finally'#010+
  '    lRunArgs.Free;'#010+
  '    lOpts.Free;'#010+
  '    lArgs.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngi','ne.PkgList(PkgList: TStrings; APackage: TPackage'+
  ');'#010+
  'begin'#010+
  '  Log(vlInfo, Format(SInfoPkgListPackage,[APackage.Name]));'#010+
  '  APackage.ListPackage(PkgList);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Compile(Packages: TPackages);'#010+
  #010+
  '  function IsReadyToCompile(A','Package:TPackage): boolean;'#010+
  '  begin'#010+
  '    result := False;'#010+
  '    if not APackage.FProcessing and (APackage.State=tsNeutral) then'#010+
  '      begin'#010+
  '        if PackageOK(APackage) then'#010+
  '          result := ReadyToCompile(APackage)'#010+
  '        else'#010+
  '         ',' begin'#010+
  '            inc(FProgressCount);'#010+
  '            log(vlInfo,SInfoSkipPackageTargetProgress,[(FProgressCount'+
  ')/FProgressMax * 100, APackage.Name, Defaults.Target]);'#010+
  '            APackage.FTargetState:=tsNoCompile;'#010+
  '          end;'#010+
  '      end;'#010,
  '  end;'#010+
  #010+
  'Var'#010+
  '  I : integer;'#010+
  '{$ifndef NO_THREADING}'#010+
  '  Thr, ThreadCount : Integer;'#010+
  '  Finished : boolean;'#010+
  '  ErrorState: boolean;'#010+
  '  ErrorMessage: string;'#010+
  '  NotifyThreadWaiting : PRTLEvent;'#010+
  '  Threads : array of TCompileWorkerThread;'#010+
  '  lPending : ','Integer;'#010+
  '{$endif NO_THREADING}'#010+
  '  P : TPackage;'#010+
  #010+
  '{$ifndef NO_THREADING}'#010+
  '  // True if a worker thread has not yet reported back'#010+
  '  function AnyThreadBusy: Boolean;'#010+
  #010+
  '  var'#010+
  '    lThr: Integer;'#010+
  #010+
  '  begin'#010+
  '    Result:=False;'#010+
  '    for lThr:=0 to Length','(Threads)-1 do'#010+
  '      if Assigned(Threads[lThr]) and not Threads[lThr].Done then'#010+
  '        Exit(True);'#010+
  '  end;'#010+
  #010+
  #010+
  '  // The number of packages that are neither compiled nor skipped'#010+
  '  function PendingCount: Integer;'#010+
  #010+
  '  var'#010+
  '    lIdx: Integer;'#010+
  #010+
  '  be','gin'#010+
  '    Result:=0;'#010+
  '    for lIdx:=0 to Packages.Count-1 do'#010+
  '      if Packages.PackageItems[lIdx].State=tsNeutral then'#010+
  '        Inc(Result);'#010+
  '  end;'#010+
  #010+
  #010+
  '  // Comma-separated names of the packages that are neither compiled n'+
  'or skipped'#010+
  '  function Pe','ndingNames: String;'#010+
  #010+
  '  var'#010+
  '    lIdx: Integer;'#010+
  #010+
  '  begin'#010+
  '    Result:='#039#039';'#010+
  '    for lIdx:=0 to Packages.Count-1 do'#010+
  '      if Packages.PackageItems[lIdx].State=tsNeutral then'#010+
  '        begin'#010+
  '        if Result<>'#039#039' then'#010+
  '          Result:=Result+'#039', '#039';'#010,
  '        Result:=Result+Packages.PackageItems[lIdx].Name;'#010+
  '        end;'#010+
  '  end;'#010+
  #010+
  #010+
  '  procedure ProcessThreadResult(ATHread: TCompileWorkerThread);'#010+
  '  var'#010+
  '    StartI: integer;'#010+
  '    CompilePackage: TPackage;'#010+
  '    PackageAvailable: boolean;'#010+
  '  begin'#010+
  ' ','   if AThread.Done then'#010+
  '      begin'#010+
  '        { synchronise with the WriteBarrier in the thread (-> ReadBarr'+
  'ier), and prevent'#010+
  '          any writes we do here afterwards to be reordered before that'+
  ' (so the compile'#010+
  '          thread won'#039't see th','ese writes either -> also WriteBarr'+
  'ier) }'#010+
  '        ReadWriteBarrier;'#010+
  '        if assigned(AThread.APackage) then'#010+
  '          begin'#010+
  '            // The thread has completed compiling the package'#010+
  '            if AThread.CompilationOK then'#010+
  '         ','     AThread.APackage.FTargetState:=tsCompiled'#010+
  '            else // A problem occurred, stop the compilation'#010+
  '              begin'#010+
  '              ErrorState:=true;'#010+
  '              ErrorMessage:='#039'Error inside worker thread for packag'+
  'e '#039'+Athread.AP','ackage.Name+'#039': '#039'+AThread.ErrorMessage;'#010+
  '              Finished:=true;'#010+
  '              end;'#010+
  '            AThread.APackage := nil;'#010+
  '          end;'#010+
  '        StartI := I;'#010+
  #010+
  '        CompilePackage := nil;'#010+
  '        PackageAvailable:=false;'#010+
  '        repeat',#010+
  '        if IsReadyToCompile(Packages.PackageItems[i]) then'#010+
  '          CompilePackage := Packages.PackageItems[i];'#010+
  '        if not (Packages.PackageItems[i].State in [tsCompiled, tsNoCom'+
  'pile]) then'#010+
  '          PackageAvailable:=true;'#010+
  '        in','c(I);'#010+
  '        if I=packages.Count then'#010+
  '          i := 0;'#010+
  '        until Assigned(CompilePackage) or (I=StartI);'#010+
  '        if Assigned(CompilePackage) then'#010+
  '          begin'#010+
  '          // Instruct thread to compile package'#010+
  '          AThread.APacka','ge := CompilePackage;'#010+
  '          AThread.APackage.FProcessing := true;'#010+
  '          { Commit changes before setting FDone to false, because the '+
  'threads'#010+
  '            only wait for an event 500ms at a time and hence way wake '+
  'up'#010+
  '            and see',' that FDone=false before the event is sent and th'+
  'e changes'#010+
  '            are all committed by the event code }'#010+
  '          WriteBarrier;'#010+
  '          AThread.FDone:=False;'#010+
  '          RTLeventSetEvent(AThread.NotifyStartTask);'#010+
  '          end;'#010+
  '       ',' if not PackageAvailable then'#010+
  '          Finished := True;'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  '{$endif NO_THREADING}'#010+
  #010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeCompile, Self);'#010+
  '  FProgressMax:=Packages.Count;'#010+
  '  FProgressCount:=0;'#010+
  #010+
  '  if Defaults.Thread','sAmount<0 then'#010+
  '    begin'#010+
  '      // Do not use any threading to compile the packages'#010+
  '      For I:=0 to Packages.Count-1 do'#010+
  '        begin'#010+
  '          P:=Packages.PackageItems[i];'#010+
  '          If PackageOK(P) then'#010+
  '            MaybeCompile(P)'#010+
  '       ','   else'#010+
  '            begin'#010+
  '            inc(FProgressCount);'#010+
  '            log(vlInfo,SInfoSkipPackageTargetProgress,[(FProgressCount'+
  ')/FProgressMax * 100, P.Name, Defaults.Target]);'#010+
  '            end;'#010+
  '        end;'#010+
  '    end'#010+
  '  else'#010+
  '    begin'#010+
  '{$ifnde','f NO_THREADING}'#010+
  '      // Use worker-threads to compile the packages'#010+
  '      ErrorState := False;'#010+
  '      Finished := False;'#010+
  '      I := 0;'#010+
  '      ThreadCount:=0;'#010+
  '      // This event is set by the worker-threads to notify the main/th'+
  'is thread'#010+
  '    ','  // that a package finished it'#039's task.'#010+
  '      NotifyThreadWaiting := RTLEventCreate;'#010+
  '      Threads:=[];'#010+
  '      SetLength(Threads,Defaults.ThreadsAmount);'#010+
  '      try'#010+
  '        // Create all worker-threads'#010+
  '        try'#010+
  '          for Thr:=0 to Defa','ults.ThreadsAmount-1 do'#010+
  '            begin'#010+
  '              Threads[Thr] := TCompileWorkerThread.Create(self,NotifyT'+
  'hreadWaiting);'#010+
  '              if assigned(Threads[Thr]) then'#010+
  '                begin'#010+
  '                  inc(ThreadCount);'#010+
  '          ','        Threads[Thr].FWorkerPrefix:=Format('#039'(%d/%d) '#039+
  ',[Thr,Defaults.ThreadsAmount]);'#010+
  '                end;'#010+
  '            end;'#010+
  '        except'#010+
  '          on E: Exception do'#010+
  '            begin'#010+
  '              ErrorMessage := E.Message;'#010+
  '              ','ErrorState:=true;'#010+
  '            end;'#010+
  '        end;'#010+
  '        try'#010+
  '          // When a thread notifies this thread that it is ready, loop'+
  ' on all'#010+
  '          // threads to check their state and if possible assign a new'+
  ' package'#010+
  '          // to them to',' compile.'#010+
  '          // With all threads idle, scan again as long as a scan skips'+
  ' packages.'#010+
  '          lPending:=-1;'#010+
  '          while not Finished do'#010+
  '            begin'#010+
  '              if AnyThreadBusy then'#010+
  '                begin'#010+
  '                R','TLeventWaitFor(NotifyThreadWaiting);'#010+
  '                RTLeventResetEvent(NotifyThreadWaiting);'#010+
  '                end'#010+
  '              else if PendingCount=lPending then'#010+
  '                begin'#010+
  '                ErrorMessage:=Format(SErrPackagesNotRea','dy,[PendingNa'+
  'mes]);'#010+
  '                ErrorState:=true;'#010+
  '                Finished:=true;'#010+
  '                Break;'#010+
  '                end;'#010+
  '              lPending:=PendingCount;'#010+
  '              for Thr:=0 to Defaults.ThreadsAmount-1 do'#010+
  '                ','if assigned(Threads[Thr]) and not Finished then'#010+
  '                  ProcessThreadResult(Threads[Thr]);'#010+
  '            end;'#010+
  '        except'#010+
  '          on E: Exception do'#010+
  '            begin'#010+
  '              if not ErrorState then'#010+
  '                ErrorMe','ssage := E.Message;'#010+
  '              ErrorState:=true;'#010+
  '            end;'#010+
  '        end;'#010+
  '        try'#010+
  '          // Compilation finished or aborted. Wait for all threads to '+
  'end.'#010+
  '          for thr:=0 to Defaults.ThreadsAmount-1 do'#010+
  '            if assi','gned(Threads[Thr]) then'#010+
  '              begin'#010+
  '                Threads[Thr].Terminate;'#010+
  '                RTLeventSetEvent(Threads[Thr].NotifyStartTask);'#010+
  '                Threads[Thr].WaitFor;'#010+
  '              end;'#010+
  '        except'#010+
  '          on E: Exce','ption do'#010+
  '            begin'#010+
  '              if not ErrorState then'#010+
  '                ErrorMessage := E.Message;'#010+
  '              ErrorState:=true;'#010+
  '            end;'#010+
  '        end;'#010+
  '      finally'#010+
  '        RTLeventdestroy(NotifyThreadWaiting);'#010+
  '        for',' thr:=0 to Defaults.ThreadsAmount-1 do'#010+
  '          if assigned(Threads[Thr]) then'#010+
  '            begin'#010+
  '              Threads[Thr].Free;'#010+
  '              dec(ThreadCount);'#010+
  '            end;'#010+
  '      end;'#010+
  '    if ErrorState then'#010+
  '      raise Exception.Crea','te(ErrorMessage);'#010+
  '{$endif NO_THREADING}'#010+
  '    end;'#010+
  '  NotifyEventCollection.CallEvents(neaAfterCompile, Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CompileBeforeInstall(Packages: TPackages);'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  #010+
  'begin'#010+
  '  For I:=0 to Pa','ckages.Count-1 do'#010+
  '    begin'#010+
  '      P:=Packages.PackageItems[i];'#010+
  '      If PackageOK(P) and not (P.State in [tsCompiled,tsNoCompile]) th'+
  'en'#010+
  '        begin'#010+
  '          Compile(Packages);'#010+
  '          Exit;'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuild','Engine.Install(Packages: TPackages);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  CompileBeforeInstall(Packages);'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeInstall, Self);'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '      P:=Packages.Packag','eItems[i];'#010+
  '      If PackageOK(P) then'#010+
  '        begin'#010+
  '          Install(P, False);'#010+
  '          log(vlWarning, SWarnInstallationPackagecomplete, [P.Name, De'+
  'faults.Target]);'#010+
  '        end'#010+
  '      else'#010+
  '        log(vlInfo,SInfoSkipPackageTarget,[P.Name',', Defaults.Target])'+
  ';'#010+
  '    end;'#010+
  '  NotifyEventCollection.CallEvents(neaAfterInstall, Self);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.ZipInstall(Packages: TPackages);'#010+
  #010+
  'var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  lOldUnixPaths : Boolean;'#010+
  #010+
  'begin'#010+
  '  CompileBeforeInst','all(Packages);'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeInstall, Self);'#010+
  #010+
  '  { An archive has the directory layout of the target OS, not of the b'+
  'uild OS }'#010+
  '  lOldUnixPaths:=Defaults.UnixPaths;'#010+
  '  Defaults.UnixPaths:=Defaults.OS in AllUnixOSe','s;'#010+
  '  if Defaults.UnixPaths then'#010+
  '    Defaults.IntSetBaseInstallDir('#039'lib/fpc/'#039' + Defaults.FCompi'+
  'lerVersion+ '#039'/'#039')'#010+
  '  else'#010+
  '    Defaults.IntSetBaseInstallDir('#039#039');'#010+
  #010+
  '  try'#010+
  '    For I:=0 to Packages.Count-1 do'#010+
  '      begin'#010+
  '        P:=Packages.PackageI','tems[i];'#010+
  '        If PackageOK(P) then'#010+
  '          begin'#010+
  '            Install(P, True);'#010+
  '            log(vlWarning, SWarnInstallationPackagecomplete, [P.Name, '+
  'Defaults.Target]);'#010+
  '          end'#010+
  '        else'#010+
  '          log(vlInfo,SInfoSkipPackageTar','get,[P.Name, Defaults.Target'+
  ']);'#010+
  '      end;'#010+
  '  finally'#010+
  '    FinishArchive(P);'#010+
  '    Defaults.UnixPaths:=lOldUnixPaths;'#010+
  '  end;'#010+
  #010+
  '  NotifyEventCollection.CallEvents(neaAfterInstall, Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.UnInstall(Packages: TPackages',');'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeUnInstall, Self);'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '      P:=Packages.PackageItems[i];'#010+
  '      UnInstall(P);'#010+
  '    end;'#010+
  '  NotifyEventCollection.Ca','llEvents(neaAfterUnInstall, Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Archive(Packages: TPackages);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeArchive, Self);'#010+
  '  Log(vlDebug, SDbgBuildEngineArchiving);'#010+
  '  ','For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '      P:=Packages.PackageItems[i];'#010+
  '      // Force generation of manifest.xml, this is required for the re'+
  'pository'#010+
  '      Manifest(nil, P);'#010+
  '      Archive(P);'#010+
  '    end;'#010+
  '  NotifyEventCollection.CallEvent','s(neaAfterArchive, Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Manifest(Packages: TPackages; Package: TPackage'+
  ');'#010+
  'Var'#010+
  '  L : TStrings;'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  FN: string;'#010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeManifest, Self);'#010+
  '  ','Log(vlDebug, SDbgBuildEngineGenerateManifests);'#010+
  #010+
  '  L:=TStringList.Create;'#010+
  '  Try'#010+
  '    Log(vlDebug, Format(SDbgGenerating, [ManifestFile]));'#010+
  '    L.Add('#039'<?xml version="1.0"?>'#039');'#010+
  '    L.Add('#039'<packages>'#039');'#010+
  '    if assigned(Packages) then'#010+
  '      begi','n'#010+
  '        For I:=0 to Packages.Count-1 do'#010+
  '          begin'#010+
  '            P:=Packages.PackageItems[i];'#010+
  '            Log(vlInfo, Format(SInfoManifestPackage,[P.Name]));'#010+
  '            P.GetManifest(L);'#010+
  '          end'#010+
  '      end;'#010+
  '    if assigned(Packag','e) then'#010+
  '      begin'#010+
  '        Log(vlInfo, Format(SInfoManifestPackage,[Package.Name]));'#010+
  '        Package.GetManifest(L);'#010+
  '      end;'#010+
  '    L.Add('#039'</packages>'#039');'#010+
  '    FN := ManifestFile;'#010+
  '    if assigned(Package) then'#010+
  '      FN := FixPath(Package.Dir','ectory, True)+FN;'#010+
  '    L.SaveToFile(FN);'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  #010+
  '  NotifyEventCollection.CallEvents(neaAfterManifest, Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.PkgList(Packages: TPackages);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  L : TStrings;',#010+
  '  PKGL : String;'#010+
  'begin'#010+
  '  L:=TStringList.Create;'#010+
  '  NotifyEventCollection.CallEvents(neaBeforePkgList, Self);'#010+
  '  Log(vlDebug, SDbgBuildEngineGeneratePkgList);'#010+
  '{ Consider only the target OS, because the installer would be run ther'+
  'e }'#010+
  '  if Defa','ults.OS in AllLimit83fsOSes then'#010+
  '    PKGL := PkgListFileBase + OSToString (Defaults.OS) + PkgListFileEx'+
  't'#010+
  '  else if Defaults.OS = osNone then'#010+
  '    PKGL := PkgListFileBase + '#039'src'#039' + PkgListFileExt'#010+
  '  else'#010+
  '    begin'#010+
  '    PKGL := PkgListFileBase +',' MakeTargetString(Defaults.CPU,Defaults'+
  '.OS,Defaults.SubTarget);'#010+
  '    PKGL := PKGL+ PkgListFileExt;'#010+
  '    end;'#010+
  #010+
  '  Try'#010+
  '    Log(vlDebug, Format(SDbgGenerating, [PKGL]));'#010+
  #010+
  '    For I:=0 to Packages.Count-1 do'#010+
  '      begin'#010+
  '        P:=Packages.Package','Items[i];'#010+
  '        PkgList(L, P);'#010+
  '      end;'#010+
  #010+
  '    L.SaveToFile(PKGL);'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  #010+
  '  NotifyEventCollection.CallEvents(neaAfterPkgList, Self);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.FPDocProject(Packages: TPackages; SingleDocFile'+
  ': Bool','ean);'#010+
  #010+
  '  Procedure AddHeader(L,Opts : Tstrings);'#010+
  #010+
  '  Var'#010+
  '    I : Integer;'#010+
  '    N,V : String;'#010+
  #010+
  '  begin'#010+
  '    L.Add('#039'<?xml version="1.0" encoding="utf-8"?>'#039');'#010+
  '    L.Add('#039'<docproject>'#039');'#010+
  '    L.Add('#039'  <options>'#039');'#010+
  '    L.Add('#039'    <option name="ostar','get" value="'#039'+QuoteXML(CPU'+
  'ToString(Defaults.CPU))+'#039'"/>'#039');'#010+
  '    L.Add('#039'    <option name="cputarget" value="'#039'+QuoteXML(OSTo'+
  'String(Defaults.OS))+'#039'"/>'#039');'#010+
  '    L.Add('#039'    <option name="parse-impl" value="false"/>'#039');'#010+
  '    L.Add('#039'    <option name="do','nt-trim" value="false"/>'#039');'#010+
  '    if assigned(Opts) then'#010+
  '      begin'#010+
  '      For I:=0 to Opts.Count-1 do'#010+
  '        begin'#010+
  '        Opts.GetNameValue(I,N,V);'#010+
  '        L.Add('#039'    <option name="%s" value="%s"/>'#039',[QuoteXML(N'+
  '),QuoteXML(V)]);'#010+
  '        end;',#010+
  '      end;'#010+
  '    L.Add('#039'  </options>'#039');'#010+
  '    L.Add('#039'  <packages>'#039');'#010+
  '  end;'#010+
  #010+
  '  Procedure AddFooter(L : Tstrings);'#010+
  '  begin'#010+
  '    L.Add('#039'  </packages>'#039');'#010+
  '    L.Add('#039'</docproject>'#039');'#010+
  '  end;'#010+
  #010+
  'Var'#010+
  '  L,LOpts : TStringList;'#010+
  '  FN : String;'#010+
  '  P : TPackag','e;'#010+
  #010+
  'begin'#010+
  '  LOpts:=Nil;'#010+
  '  L:=TStringList.Create;'#010+
  '  Try'#010+
  '    if Defaults.FPDocOptions<>'#039#039' then'#010+
  '       begin'#010+
  '       LOpts:=TStringList.Create;'#010+
  '       LOpts.LoadFromFile(Defaults.FPDocOptions);'#010+
  '       end;'#010+
  '    if SingleDocFile then'#010+
  '      begin'#010,
  '      FN:='#039'fpmake'#039'+DocProjectFileExt;'#010+
  '      if Defaults.FPDocOutputDir<>'#039#039' then'#010+
  '        FN:=IncludeTrailingPathDelimiter(Defaults.FPDocOutputDir)+FN;'#010+
  '      Log(vlDebug, Format(SDbgGenerating, [FN]));'#010+
  '      AddHeader(L,Lopts);'#010+
  '      Log(vlIn','fo, Format(SInfoPackageDocProject,['#039'<all>'#039']));'+
  #010+
  '      For P in Packages do'#010+
  '        GetDocProject(L,P,'#039'    '#039');'#010+
  '      AddFooter(L);'#010+
  '      L.SaveToFile(FN);'#010+
  '      end'#010+
  '   else'#010+
  '     For P in Packages do'#010+
  '       begin'#010+
  '       L.Clear;'#010+
  '       FN:=P.N','ame+DocProjectFileExt;'#010+
  '       if Defaults.FPDocOutputDir<>'#039#039' then'#010+
  '         FN:=IncludeTrailingPathDelimiter(Defaults.FPDocOutputDir)+FN;'+
  #010+
  '       Log(vlDebug, Format(SDbgGenerating, [FN]));'#010+
  '       AddHeader(L,Lopts);'#010+
  '       Log(vlInfo, Format','(SInfoPackageDocProject,[P.Name]));'#010+
  '       GetDocProject(L,P,'#039'    '#039');'#010+
  '       AddFooter(L);'#010+
  '       L.SaveToFile(FN);'#010+
  '       end;'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '    Lopts.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Documentation(Packages: TPackages);'#010+
  #010+
  'v','ar'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  #010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeDoc, Self);'#010+
  '  for I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '    P:=Packages.PackageItems[I];'#010+
  '    if PackageOK(P) then'#010+
  '      Documentation(P);'#010+
  '    end;'#010+
  '  Notify','EventCollection.CallEvents(neaAfterDoc, Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Clean(Packages: TPackages; AllTargets: boolean)'+
  ';'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  NotifyEventCollection.CallEvents(neaBeforeClean, Self);'#010+
  '  Log(vldebug, ','SDbgBuildEngineCleaning);'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '      P:=Packages.PackageItems[i];'#010+
  '      If AllTargets or PackageOK(P) then'#010+
  '        Clean(P, AllTargets);'#010+
  '    end;'#010+
  '  NotifyEventCollection.CallEvents(neaAfterClean, Self)',';'#010+
  'end;'#010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                               TFPVersion'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TFPVersion.GetAsString: ','String;'#010+
  'begin'#010+
  '  if Empty then'#010+
  '    Result:='#039'<none>'#039#010+
  '  else'#010+
  '  begin'#010+
  '    Result := '#039#039';'#010+
  '    if Major <> -1 then'#010+
  '      Result := Result + IntToStr(Major);'#010+
  '    if Minor <> -1 then'#010+
  '      Result := Result + '#039'.'#039' + IntToStr(Minor);'#010+
  '    if Micro <> -1',' then'#010+
  '      Result := Result + '#039'.'#039' + IntToStr(Micro);'#010+
  '    if Build <> -1 then'#010+
  '      Result := Result + '#039'-'#039'  + IntToStr(Build);'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'function TFPVersion.GetEmpty: Boolean;'#010+
  'begin'#010+
  '  Result:=(Major=0) and (Minor=0) and (Micro=0) and (Bu','ild=0);'#010+
  'end;'#010+
  #010+
  'procedure TFPVersion.SetAsString(const AValue: String);'#010+
  #010+
  '  Function NextDigit(sep : AnsiChar; var V : string) : integer;'#010+
  '  Var'#010+
  '    P : Integer;'#010+
  '  begin'#010+
  '    P:=Pos(Sep,V);'#010+
  '    If (P=0) then'#010+
  '      P:=Length(V)+1;'#010+
  '    Result:=Str','ToIntDef(Copy(V,1,P-1),-1);'#010+
  '    If Result<>-1 then'#010+
  '      Delete(V,1,P);'#010+
  '  end;'#010+
  #010+
  'Var'#010+
  '  V : String;'#010+
  'begin'#010+
  '  Clear;'#010+
  '  // Special support for empty version string'#010+
  '  if (AValue='#039#039') or (AValue='#039'<none>'#039') then'#010+
  '    exit;'#010+
  '  V:=AValue;'#010+
  '  Major:=NextDi','git('#039'.'#039',V);'#010+
  '  Minor:=NextDigit('#039'.'#039',V);'#010+
  '  Micro:=NextDigit('#039'-'#039',V);'#010+
  '  Build:=NextDigit(#0,V);'#010+
  'end;'#010+
  #010+
  'procedure TFPVersion.Clear;'#010+
  'begin'#010+
  '  Micro:=-1;'#010+
  '  Major:=-1;'#010+
  '  Minor:=-1;'#010+
  '  Build:=-1;'#010+
  'end;'#010+
  #010+
  'procedure TFPVersion.Assign(Source: TPersistent);'#010,
  'Var'#010+
  '  V : TFPVersion;'#010+
  'begin'#010+
  '  if Source is TFPVersion then'#010+
  '    begin'#010+
  '    V:=Source as TFPVersion;'#010+
  '    Major:=V.Major;'#010+
  '    Minor:=V.Minor;'#010+
  '    Micro:=V.Micro;'#010+
  '    Build:=V.Build;'#010+
  '    end'#010+
  '  else'#010+
  '    inherited Assign(Source);'#010+
  'end;'#010+
  #010+
  'function TF','PVersion.CompareVersion(AVersion: TFPVersion): Integer;'#010+
  'begin'#010+
  '  Result:=Major-AVersion.Major;'#010+
  '  If (Result=0) then'#010+
  '    begin'#010+
  '      Result:=Minor-AVersion.Minor;'#010+
  '      if (Result=0) then'#010+
  '        begin'#010+
  '          Result:=Micro-AVersion.Micro;'#010,
  '          If (Result=0) then'#010+
  '            Result:=Build-AVersion.Build;'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function TFPVersion.SameVersion(AVersion: TFPVersion): Boolean;'#010+
  'begin'#010+
  '  Result:=CompareVersion(AVersion)=0;'#010+
  'end;'#010+
  #010+
  #010+
  '{*************************','******************************************'+
  '*********'#010+
  '                                 TTarget'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TTarget.Create(ACollection: TCollection);'#010+
  'begin'#010+
  '  inheri','ted Create(ACollection);'#010+
  '  FInstall:=True;'#010+
  '  FCPUs:=AllCPUs;'#010+
  '  FOSes:=AllOSes;'#010+
  '  FUnitPath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FIncludePath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FObjectPath:=TConditionalStrings.C','reate(TConditionalString);'#010+
  '  FDependencies:=TDependencies.Create(TDependency);'#010+
  '  FResourceFiles:=TResourceFiles.Create(TResourceFile);'#010+
  '  FCommands:=TCommands.Create(TCommand);'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TTarget.Destroy;'#010+
  'begin'#010+
  '  FreeAndNil(FUnitPath)',';'#010+
  '  FreeAndNil(FObjectPath);'#010+
  '  FreeAndNil(FIncludePath);'#010+
  '  FreeAndNil(FDependencies);'#010+
  '  FreeAndNil(FResourceFiles);'#010+
  '  FreeAndNil(FCommands);'#010+
  '  FreeAndNil(Foptions);'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  'procedure TTarget.AssignTo(Dest: TPersistent);'#010+
  'v','ar'#010+
  '  DestTarget: TTarget;'#010+
  'begin'#010+
  '  if Dest is TTarget then'#010+
  '    begin'#010+
  '    DestTarget := TTarget(Dest);'#010+
  '    DestTarget.Dependencies.Assign(Dependencies);'#010+
  '    DestTarget.Commands.Assign(Commands);'#010+
  '    DestTarget.FTargetState := FTargetState;'#010+
  '  ','  DestTarget.TargetType := TargetType;'#010+
  '    DestTarget.CPUs := CPUs;'#010+
  '    DestTarget.OSes := OSes;'#010+
  '    DestTarget.Mode := Mode;'#010+
  '    DestTarget.Options := Options;'#010+
  '    DestTarget.Name :=  Name;'#010+
  '    DestTarget.Extension:= Extension;'#010+
  '    DestTar','get.FPCTarget := FPCTarget;'#010+
  '    DestTarget.FileType := FileType;'#010+
  '    DestTarget.Directory := Directory;'#010+
  '    DestTarget.ResourceStrings := ResourceStrings;'#010+
  '    DestTarget.ResourceFiles.Assign(ResourceFiles);'#010+
  '    DestTarget.Install := Install',';'#010+
  '    DestTarget.FTargetSourceFileName := fTargetSourceFileName;'#010+
  '    DestTarget.ObjectPath.Assign(ObjectPath);'#010+
  '    DestTarget.UnitPath.Assign(UnitPath);'#010+
  '    DestTarget.IncludePath.Assign(IncludePath);'#010+
  '    DestTarget.FXML := FXML;'#010+
  '    DestTa','rget.AfterCompile := AfterCompile;'#010+
  '    DestTarget.BeforeCompile := BeforeCompile;'#010+
  '    DestTarget.BeforeClean := BeforeCompile;'#010+
  '    DestTarget.AfterClean := AfterClean;'#010+
  '    end'#010+
  '  else'#010+
  '    inherited AssignTo(Dest);'#010+
  'end;'#010+
  #010+
  'function TTarget.GetO','ptions: TStrings;'#010+
  'begin'#010+
  '  If Foptions=Nil then'#010+
  '    FOptions:=TUnsortedCompilerOptionsStringList.Create;'#010+
  '  Result:=FOptions;'#010+
  'end;'#010+
  #010+
  'function TTarget.GetImportLibFileName(AOS : TOS) : String;'#010+
  'begin'#010+
  '  result := GetImportLibraryFilename(Name,AOS',');'#010+
  'end;'#010+
  #010+
  'function TTarget.GetUnitLibFileName(AOS : TOS): String;'#010+
  'begin'#010+
  '  if AOS in [atari,netwlibc,go32v2,watcom,wdosx,msdos,win16] then'#010+
  '    Result := Name+LibExt'#010+
  '  else if AOS in [java] then'#010+
  '    Result:=Name+'#039'.jar'#039#010+
  '  else if AOS in [macosc','lassic] then'#010+
  '    Result:=Name+'#039'Lib'#039#010+
  '  else'#010+
  '    Result:='#039'libp'#039'+Name+LibExt;'#010+
  'end;'#010+
  #010+
  'procedure TTarget.SetOptions(const AValue: TStrings);'#010+
  'begin'#010+
  '  If (AValue=Nil) or (AValue.Count=0) then'#010+
  '    FreeAndNil(FOptions)'#010+
  '  else'#010+
  '    Options.Assign(AValu','e);'#010+
  'end;'#010+
  #010+
  'function TTarget.GetSourceFileName: String;'#010+
  'begin'#010+
  '  Result:=Name+FExtension;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetUnitFileName: String;'#010+
  'begin'#010+
  '  Result:=Name+UnitExt;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetObjectFileName: String;'#010+
  'begin'#010+
  '  Result:=Name+Ob','jExt;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetLTOFileName: String;'#010+
  'begin'#010+
  '  Result:=Name+LTOExt;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetRSTFileName: String;'#010+
  'begin'#010+
  '  Result:=Name+RSText;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetRSJFileName: String;'#010+
  'begin'#010+
  '  Result:=Name+RSJext;'#010+
  'end',';'#010+
  #010+
  #010+
  'function TTarget.GetBinFileBase: String;'#010+
  #010+
  'var'#010+
  '  S : String;'#010+
  #010+
  'begin'#010+
  '  if FExeName <> '#039#039' then'#010+
  '    Result := FExeName'#010+
  '  else'#010+
  '    begin'#010+
  '    Result:=Name;'#010+
  '    for S in Options do'#010+
  '      if Copy(S,1,2)='#039'-o'#039' then'#010+
  '        Result:=Copy(S,3);'#010+
  '    ','end;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetProgramFileName(AOS : TOS): String;'#010+
  'begin'#010+
  '    result := AddProgramExtension(GetBinFileBase, AOS);'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetProgramDebugFileName(AOS: TOS): String;'#010+
  'begin'#010+
  '  result := GetBinFileBase + DbgExt;'#010,
  'end;'#010+
  #010+
  'function TTarget.GetLibraryFileName(AOS : TOS): String;'#010+
  'begin'#010+
  '  Result:=GetBinFileBase;'#010+
  '  if ExtractFileExt(Result)='#039#039' then'#010+
  '    result := AddLibraryExtension(Result, AOS);'#010+
  '  if aOS in AllUnixOSes then'#010+
  '    if Copy(Result,1,3)<>'#039'lib'#039' th','en'#010+
  '      Result:='#039'lib'#039'+Result;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetLibraryDebugFileName(AOS: TOS): String;'#010+
  'begin'#010+
  '  result := GetBinFileBase + DbgExt;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetOutputFileName(AOs: TOS): String;'#010+
  'begin'#010+
  '  if TargetType in UnitTargets t','hen'#010+
  '    Result:=GetUnitFileName'#010+
  '  else if TargetType=ttSharedLibrary then'#010+
  '    Result:=GetLibraryFileName(AOs)'#010+
  '  else'#010+
  '    Result:=GetProgramFileName(AOs);'#010+
  'end;'#010+
  #010+
  'function TTarget.HaveOptions: Boolean;'#010+
  'begin'#010+
  '  Result:=(FOptions<>Nil);'#010+
  'end;'#010+
  #010+
  'fu','nction TTarget.SubTargetAllowed(const aSubTarget: String): Boolean'+
  ';'#010+
  #010+
  'begin'#010+
  '  Result:=(Length(FSubTargets)=0) or (Length(aSubTarget)=0);'#010+
  '  if not Result then'#010+
  '    Result:=IndexText(aSubTarget,FSubTargets)<>-1;'#010+
  'end;'#010+
  #010+
  'function TTarget.SubTarget','sAsString: String;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  if Length(SubTargets)>0 then'#010+
  '    Result:=SubTargets[0];'#010+
  '  for I:=1 to Length(SubTargets) do'#010+
  '    Result:=Result+'#039' '#039'+SubTargets[i];'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TTarget.SetName(const AValue: S','tring);'#010+
  'Var'#010+
  '  D,N,E : String;'#010+
  'begin'#010+
  '  N:=FixPath(AValue, False);'#010+
  '  D:=ExtractFilePath(N);'#010+
  '  E:=ExtractFileExt(N);'#010+
  '  N:=ExtractFileName(N);'#010+
  '  inherited SetName(Copy(N,1,Length(N)-Length(E)));'#010+
  '  FExtension:=E;'#010+
  '  FDirectory:=D;'#010+
  'end;'#010+
  #010+
  'procedure',' TTarget.SetExeName(const AValue: String);'#010+
  'Var'#010+
  '  N,E : String;'#010+
  'begin'#010+
  '  N:=FixPath(AValue, False);'#010+
  '  E:=ExtractFileExt(N);'#010+
  '  N:=ExtractFileName(N);'#010+
  '  FExeName:=Copy(N,1,Length(N)-Length(E));'#010+
  '  { Use exact AValue for -o option }'#010+
  '  AddOption('#039,'-o'#039'+AValue);'#010+
  'end;'#010+
  #010+
  'procedure TTarget.SetXML(const AValue: string);'#010+
  'begin'#010+
  '  FXML:=FixPath(AValue, False);'#010+
  'end;'#010+
  #010+
  'procedure TTarget.GetCleanFiles(List: TStrings; const APrefixU,'#010+
  '  APrefixB: String; const aTarget: TcompileTarget);'#010+
  #010+
  'Var'#010+
  '  aOS : ','TOS;'#010+
  '  aCPU : TCPU;'#010+
  #010+
  'begin'#010+
  '  aCPU:=aTarget.CPU;'#010+
  '  aOS:=aTarget.OS;'#010+
  '  If not(aCPU in CPUs) or not(AOS in OSes) then'#010+
  '    exit;'#010+
  '  if aPrefixU<>'#039#039' then'#010+
  '    begin'#010+
  '    List.Add(APrefixU + ObjectFileName);'#010+
  '    List.Add(APrefixU + LTOFileName);'#010+
  '   ',' end;'#010+
  '  Case TargetType of'#010+
  '  ttUnit,ttImplicitUnit,ttExampleUnit, ttCleanOnlyUnit:'#010+
  '      if aPrefixU<>'#039#039' then'#010+
  '        begin'#010+
  '        List.Add(APrefixU + UnitFileName);'#010+
  '        if (AOS in AllSmartLinkLibraryOSes) and FileExists(APrefixU + '+
  'Get','UnitLibFileName(AOS)) then'#010+
  '          List.Add(APrefixU + GetUnitLibFileName(AOS));'#010+
  '        if (AOS in AllImportLibraryOSes) and FileExists(APrefixU + Get'+
  'ImportLibFilename(AOS)) then'#010+
  '          List.Add(APrefixU + GetImportLibFilename(AOS));'#010,
  '        end;'#010+
  '  ttProgram,ttExampleProgram:'#010+
  '    begin'#010+
  '    if (aPrefixB<>'#039#039') then'#010+
  '      begin'#010+
  '      List.Add(APrefixB + GetProgramFileName(AOS));'#010+
  '        if FileExists(APrefixB + GetProgramDebugFileName(AOS)) then'#010+
  '          List.Add(APrefixB ','+ GetProgramDebugFileName(AOS));'#010+
  '      end;'#010+
  '    if (aPrefixU<>'#039#039') then'#010+
  '      begin'#010+
  '      if (AOS in AllImportLibraryOSes) and FileExists(APrefixU + GetIm'+
  'portLibFilename(AOS)) then'#010+
  '        List.Add(APrefixU + GetImportLibFilename(AOS));'#010+
  '    ','  end;'#010+
  '    end;'#010+
  '  ttSharedLibrary:'#010+
  '    if (APrefixB<>'#039#039') then'#010+
  '    begin'#010+
  '      List.Add(APrefixB + GetLibraryFileName(AOS));'#010+
  '      if FileExists(APrefixB + GetLibraryDebugFileName(AOS)) then'#010+
  '        List.Add(APrefixB + GetLibraryDebugFileNam','e(AOS));'#010+
  '    end;'#010+
  '  end; { case }'#010+
  '  if (APrefixU<>'#039#039') then'#010+
  '    begin'#010+
  '    If ResourceStrings  then'#010+
  '      begin'#010+
  '        // choose between 2 possible resource files'#010+
  '        if FileExists(APrefixU + RSJFileName) then'#010+
  '          List.Add(APrefixU',' + RSJFileName)'#010+
  '        else'#010+
  '          List.Add(APrefixU + RSTFileName);'#010+
  '      end'#010+
  '    else'#010+
  '      begin'#010+
  '        if FileExists(APrefixU + RSJFileName) then'#010+
  '          begin'#010+
  '            Installer.Log(VlDebug,Format(SDbgUnregisteredResource,[AP',
  'refixU + RSJFileName]));'#010+
  '            List.Add(APrefixU + RSJFileName);'#010+
  '          end'#010+
  '        else if FileExists(APrefixU + RSTFileName) then'#010+
  '          begin'#010+
  '            Installer.Log(VlDebug,Format(SDbgUnregisteredResource,[APr'+
  'efixU + RSTFi','leName]));'#010+
  '            List.Add(APrefixU + RSTFileName);'#010+
  '          end;'#010+
  '       end;'#010+
  '     end;'#010+
  '  // Maybe add later ?  AddConditionalStrings(List,CleanFiles);'#010+
  'end;'#010+
  #010+
  'procedure TTarget.GetCleanFiles(List: TStrings; const APrefixU,'#010+
  '  APrefixB: ','String; ACPU: TCPU; AOS: TOS; const aSubTarget: String);'+
  #010+
  'Var'#010+
  '  CT : TCompileTarget;'#010+
  #010+
  'begin'#010+
  '  CT.OS:=aOS;'#010+
  '  CT.CPU:=aCPU;'#010+
  '  CT.Subtarget:=aSubTarget;'#010+
  '  GetCleanFiles(List,aPrefixU,aPrefixB,CT);'#010+
  'end;'#010+
  #010+
  'procedure TTarget.GetInstallFiles(List: T','Strings; const APrefixU,'#010+
  '  APrefixB: String; ACPU: TCPU; AOS: TOS; const aSubTarget: String);'#010+
  'Var'#010+
  '  CT : TCompileTarget;'#010+
  #010+
  'begin'#010+
  '  CT.OS:=aOS;'#010+
  '  CT.CPU:=aCPU;'#010+
  '  CT.Subtarget:=aSubTarget;'#010+
  '  GetInstallFiles(List,aPrefixU,aPrefixB,CT);'#010+
  'end;'#010+
  #010+
  #010+
  'p','rocedure TTarget.GetInstallFiles(List: TStrings; const APrefixU,'#010+
  '  APrefixB: String; const aTarget: TCompileTarget);'#010+
  'var'#010+
  '  UnitsDir : string;'#010+
  '  aOS : TOS;'#010+
  '  aCPU : TCPU;'#010+
  'begin'#010+
  '  aOS:=aTarget.OS;'#010+
  '  aCPU:=aTarget.CPU;'#010+
  '  UnitsDir := Installer.','BuildEngine.AddPathPrefix(nil, APrefixU);'#010+
  '  If Not (TargetType in [ttProgram,ttSharedLibrary,ttExampleProgram]) '+
  'then'#010+
  '    begin'#010+
  '      // The compiler does not create an objectfile for all programs.'#010+
  '      if FileExists(UnitsDir + ObjectFileNa','me) then'#010+
  '        List.Add(APrefixU + ObjectFileName);'#010+
  '      if FileExists(UnitsDir + LTOFileName) then'#010+
  '        List.Add(APrefixU + LTOFileName);'#010+
  '    end;'#010+
  '  If (TargetType in [ttUnit,ttImplicitUnit,ttExampleUnit]) then'#010+
  '    begin'#010+
  '      List.A','dd(APrefixU + UnitFileName);'#010+
  '      if (AOS in AllSmartLinkLibraryOSes) and FileExists(UnitsDir + Ge'+
  'tUnitLibFileName(AOS)) then'#010+
  '        List.Add(APrefixU + GetUnitLibFileName(AOS));'#010+
  '      if (AOS in AllImportLibraryOSes) and FileExists(Units','Dir + Get'+
  'ImportLibFilename(AOS)) then'#010+
  '        List.Add(APrefixU + GetImportLibFilename(AOS));'#010+
  '    end'#010+
  '  else If (TargetType in [ttProgram,ttExampleProgram]) then'#010+
  '    List.Add(APrefixB + GetProgramFileName(AOS))'#010+
  '  else If (TargetType in [ttS','haredLibrary]) then'#010+
  '    List.Add(APrefixB + GetLibraryFileName(AOS));'#010+
  '  If ResourceStrings then'#010+
  '    begin'#010+
  '      // choose between 2 possible resource files'#010+
  '      if FileExists(UnitsDir + RSJFileName) then'#010+
  '        List.Add(APrefixU + RSJFile','Name)'#010+
  '      else'#010+
  '        List.Add(APrefixU + RSTFileName);'#010+
  '    end'#010+
  '  else'#010+
  '    begin'#010+
  '      if FileExists(UnitsDir + RSJFileName) then'#010+
  '        begin'#010+
  '          Installer.Log(VlDebug,Format(SDbgUNregisteredResource,[APref'+
  'ixU + RSJFileName]));'#010+
  ' ','         List.Add(APrefixU + RSJFileName);'#010+
  '        end'#010+
  '      else if FileExists(UnitsDir + RSTFileName) then'#010+
  '        begin'#010+
  '          Installer.Log(VlDebug,Format(SDbgUNregisteredResource,[APref'+
  'ixU + RSTFileName]));'#010+
  '          List.Add(APrefi','xU + RSTFileName);'#010+
  '        end;'#010+
  '     end;'#010+
  '  FResourceFiles.GetInstallFiles(List, APrefixU, APrefixB, ACPU, AOS);'+
  #010+
  'end;'#010+
  #010+
  'procedure TTarget.GetArchiveFiles(List: TStrings; ACPU: TCPU; AOS : TO'+
  'S);'#010+
  #010+
  #010+
  'Var'#010+
  '  CT : TCompileTarget;'#010+
  #010+
  'begin'#010+
  '  CT.OS:=aO','S;'#010+
  '  CT.CPU:=aCPU;'#010+
  '  CT.Subtarget:='#039#039';'#010+
  '  GetArchiveFiles(List,CT);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TTarget.GetArchiveFiles(List: TStrings; const aTarget: TComp'+
  'ileTarget);'#010+
  'var'#010+
  '  i : integer;'#010+
  '  D : TDependency;'#010+
  'begin'#010+
  '  If not(aTarget.CPU in CPUs) or not(aTa','rget.OS in OSes) then'#010+
  '    exit;'#010+
  '  // Main source'#010+
  '  if TargetSourceFileName<>'#039#039' then'#010+
  '    List.Add(TargetSourceFileName);'#010+
  '  // Includes'#010+
  '  for i:=0 to Dependencies.Count-1 do'#010+
  '    begin'#010+
  '      D:=Dependencies[i];'#010+
  '      if (D.DependencyType=depIn','clude) and'#010+
  '         (D.TargetFileName<>'#039#039') then'#010+
  '        List.Add(D.TargetFileName);'#010+
  '    end;'#010+
  '  // FPDoc files'#010+
  '  if XML <> '#039#039' then'#010+
  '  begin'#010+
  '    List.Add(Directory + Name + Extension);'#010+
  '    List.Add(XML);'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TTarget.AddOptio','n(const aValue: String);'#010+
  'begin'#010+
  '  Options.Add(aValue);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                 TSource'#010+
  '************************************************************','********'+
  '********}'#010+
  #010+
  'function TSource.GetInstallSourcePath: string;'#010+
  'begin'#010+
  '  if FInstallSourcePath<>'#039#039' then'#010+
  '    result := FInstallSourcePath'#010+
  '  else if SourceType=stExample then'#010+
  '    result := '#039'examples'#039#010+
  '  else'#010+
  '    result := '#039#039';'#010+
  'end;'#010+
  #010+
  'constructo','r TSource.Create(ACollection: TCollection);'#010+
  'begin'#010+
  '  inherited Create(ACollection);'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TSource.Destroy;'#010+
  'begin'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  'procedure TSource.GetInstallFiles(List: TStrings);'#010+
  'begin'#010+
  '  if InstallSourcePath<>'#039#039' then'#010,
  '    list.Values[name] := (IncludeTrailingPathDelimiter(InstallSourcePa'+
  'th)+ExtractFileName(Name))'#010+
  '  else'#010+
  '    list.add(Name);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                               ','  TCommands'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TCommands.GetCommand(const Dest : String): TCommand;'#010+
  'begin'#010+
  '  Result:=TCommand(ItemByName(Dest));'#010+
  'end;'#010+
  #010+
  'function TCommands.GetCommandItem(Ind','ex : Integer): TCommand;'#010+
  'begin'#010+
  '  Result:=TCommand(Items[Index]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCommands.SetCommandItem(Index : Integer; const AValue: TCom'+
  'mand);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCommands.AddCommand(const Cmd: String): TCom','mand;'#010+
  'begin'#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,'#039#039','#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  #010+
  'function TCommands.AddCommand(const Cmd, Options: String): TCommand;'#010+
  'begin'#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,Options,'#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  #010+
  'function TCommands.AddCommand(const Cmd,',' Options, Dest, Source: Stri'+
  'ng ): TCommand;'#010+
  'begin'#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,options,Dest,Source);'#010+
  'end;'#010+
  #010+
  #010+
  'function TCommands.AddCommand(At: TCommandAt; const Cmd: String): TCom'+
  'mand;'#010+
  'begin'#010+
  '  Result:=AddCommand(At,Cmd,'#039#039','#039#039','#039#039');'#010+
  'end;',#010+
  #010+
  #010+
  'function TCommands.AddCommand(At: TCommandAt; const Cmd, Options: Stri'+
  'ng  ): TCommand;'#010+
  'begin'#010+
  '  Result:=AddCommand(At,Cmd,Options,'#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  #010+
  'function TCommands.AddCommand(At: TCommandAt; const Cmd, Options, Dest'+
  ', Source: String): TCom','mand;'#010+
  'begin'#010+
  '  Result:=Add as TCommand;'#010+
  '  Result.Command:=Cmd;'#010+
  '  If (Options<>'#039#039') then'#010+
  '    Result.ParseOptions(Options);'#010+
  '  Result.At:=At;'#010+
  '  Result.SourceFile:=Source;'#010+
  '  Result.DestFile:=Dest;'#010+
  'end;'#010+
  #010+
  'function TCommands.GetEnumerator: TCommandE','numerator;'#010+
  'begin'#010+
  '  Result:=TCommandEnumerator.Create(Self);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                           TConditionalString'#010+
  '*************************************************','*******************'+
  '********}'#010+
  #010+
  #010+
  'procedure TConditionalString.Assign(aSource: TPersistent);'#010+
  #010+
  'Var'#010+
  '  CS : TConditionalString absolute aSource;'#010+
  #010+
  'begin'#010+
  '  if (aSource is TConditionalString) then'#010+
  '    begin'#010+
  '    FValue:=CS.Value;'#010+
  '    FOSes:=CS.OSes;'#010,
  '    FCPUs:=CS.CPUs;'#010+
  '    end'#010+
  '  else'#010+
  '    inherited Assign(aSource);'#010+
  'end;'#010+
  #010+
  'function TConditionalString.Match(aCPU: TCPU; aOS: TOS): Boolean;'#010+
  'begin'#010+
  '  Result:=(aCPU in CPUs) or (CPUs=[]);'#010+
  '  Result:=Result and ((aOS in OSes) or (OSes=[]));'#010+
  'end;'#010+
  #010,
  'function TConditionalString.Match(const aValue: String; aCPU: TCPU; aO'+
  'S: TOS'#010+
  '  ): Boolean;'#010+
  'begin'#010+
  '  Result:=Match(aCPU,aOS) and (aValue=Value)'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TConditionalString.SetValue(const Value: String);'#010+
  'begin'#010+
  '  FValue := FixPath(Value',');'#010+
  'end;'#010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                           TConditionalStrings'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  #010+
  'procedure TConditionalStri','ngs.AddList(aList: TConditionalStrings);'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  For I:=0 to aList.Count-1 do'#010+
  '    With Self.Add() do'#010+
  '      Assign(aList[i]);'#010+
  'end;'#010+
  #010+
  'function TConditionalStrings.IndexOf(Value: String; aCPU: TCPU; aOS: T'+
  'OS'#010+
  '  ): Integer;'#010+
  'b','egin'#010+
  '  Result:=Count-1;'#010+
  '  While (Result>=0) and Not GetConditionalString(Result).Match(Value,a'+
  'CPU,aOS) do'#010+
  '    Dec(Result);'#010+
  'end;'#010+
  #010+
  'function TConditionalStrings.Find(Value: String; aCPU: TCPU; aOS: TOS'#010+
  '  ): TConditionalString;'#010+
  #010+
  'var'#010+
  '  Idx : Int','eger;'#010+
  #010+
  'begin'#010+
  '  Result:=Nil;'#010+
  '  Idx:=IndexOf(Value,aCPU,aOS);'#010+
  '  if Idx<>-1 then'#010+
  '    Result:=GetConditionalString(Idx);'#010+
  'end;'#010+
  #010+
  #010+
  'function TConditionalStrings.GetConditionalString(Index : Integer): TC'+
  'onditionalString;'#010+
  'begin'#010+
  '  Result:=TConditional','String(Items[Index]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TConditionalStrings.SetConditionalString(Index : Integer; co'+
  'nst AValue: TConditionalString);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'function TConditionalStrings.Add(const Value: String): TConditionalStr'+
  'i','ng;'#010+
  'begin'#010+
  '  result:=Add(Value,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TConditionalStrings.Add(Const Value : String;const CPUs:TCPUs'+
  ') : TConditionalString;'#010+
  'begin'#010+
  '  result:=Add(Value,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endif cpu_only_','overloads}'#010+
  #010+
  #010+
  'function TConditionalStrings.Add(const Value: String; const OSes: TOSe'+
  's'#010+
  '  ): TConditionalString;'#010+
  'begin'#010+
  '  result:=Add(Value,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  'function TConditionalStrings.Add(const Value: String; const CPUs: TCPU'+
  's;'#010+
  '  const O','Ses: TOSes): TConditionalString;'#010+
  'begin'#010+
  '  Result:=(Inherited Add) as TConditionalString;'#010+
  '  Result.Value:=Value;'#010+
  '  Result.OSes:=OSes;'#010+
  '  Result.CPUs:=CPUs;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '  ','                              TDependency'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TDependency.Create(aCollection: TCollection);'#010+
  'begin'#010+
  '  inherited;'#010+
  '  FVersion:=TFPVersion.Create;'#010+
  'end;'#010+
  #010+
  'proce','dure TDependency.Assign(aSource: TPersistent);'#010+
  #010+
  'Var'#010+
  '  D : TDependency absolute asource;'#010+
  #010+
  'begin'#010+
  '  if (aSource is TDependency) then'#010+
  '    begin'#010+
  '    FDependencyType:=D.DependencyType;'#010+
  '    FTarget:=D.Target;'#010+
  '    Version:=D.Version;'#010+
  '    FRequireCh','ecksum:=D.RequireChecksum;'#010+
  '    FTargetFileName:=D.TargetFileName;'#010+
  '    end;'#010+
  '  Inherited;'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TDependency.Destroy;'#010+
  'begin'#010+
  '  FreeAndNil(FVersion);'#010+
  '  Inherited;'#010+
  'end;'#010+
  #010+
  #010+
  'function TDependency.GetVersion: string;'#010+
  'begin'#010+
  '  result:=FVersi','on.AsString;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TDependency.SetVersion(const V: string);'#010+
  'begin'#010+
  '  FVersion.AsString:=V;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TDependencies'#010+
  '********','************************************************************'+
  '********}'#010+
  #010+
  'function TDependencies.GetDependency(Index : Integer): TDependency;'#010+
  'begin'#010+
  '  Result:=TDependency(Items[Index]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TDependencies.SetDependency(Index : Integ','er; const AValue:'+
  ' TDependency);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'function TDependencies.Add(const Value: String): TDependency;'#010+
  'begin'#010+
  '  result:=Add(Value,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TDependencies.Add(Con','st Value : String;const CPUs:TCPUs) : '+
  'TDependency;'#010+
  'begin'#010+
  '  result:=Add(Value,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'function TDependencies.Add(const Value: String; const OSes: TOSes): TD'+
  'ependency;'#010+
  'begin'#010+
  '  result:=Add(Value,AllCPU','s,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  'function TDependencies.Add(const Value: String; const CPUs: TCPUs;'#010+
  '  const OSes: TOSes): TDependency;'#010+
  'begin'#010+
  '  Result:=inherited Add(Value,CPUs,OSes) as TDependency;'#010+
  '  Result.Target:=nil;'#010+
  '  Result.FDependencyType:=depPackage;',#010+
  'end;'#010+
  #010+
  #010+
  'function TDependencies.AddUnit(const Value: String): TDependency;'#010+
  'begin'#010+
  '  result:=AddUnit(Value,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TDependencies.AddUnit(Const Value : String;const CPUs:TCPUs) '+
  ': TDependency',';'#010+
  'begin'#010+
  '  result:=AddUnit(Value,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'function TDependencies.AddUnit(const Value: String; const OSes: TOSes'#010+
  '  ): TDependency;'#010+
  'begin'#010+
  '  result:=AddUnit(Value,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  'function TDependen','cies.AddUnit(const Value: String; const CPUs: TCPU'+
  's;'#010+
  '  const OSes: TOSes): TDependency;'#010+
  'begin'#010+
  '  Result:=inherited Add(Value,CPUs,OSes) as TDependency;'#010+
  '  Result.Target:=nil;'#010+
  '  Result.FDependencyType:=depUnit;'#010+
  'end;'#010+
  #010+
  #010+
  'function TDependencies.Ad','dInclude(const Value: String): TDependency;'+
  #010+
  'begin'#010+
  '  result:=AddInclude(Value,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TDependencies.AddInclude(Const Value : String;const CPUs:TCPU'+
  's) : TDependency;'#010+
  'begin'#010+
  '  result:=AddInc','lude(Value,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'Function TDependencies.AddInclude(Const Value : String;const OSes:TOSe'+
  's) : TDependency;'#010+
  'begin'#010+
  '  result:=AddInclude(Value,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TDependencies.AddInclude(Co','nst Value : String;const CPUs:TC'+
  'PUs;const OSes:TOSes) : TDependency;'#010+
  'Var'#010+
  '  N : String;'#010+
  'begin'#010+
  '  N:=FixPath(Value, False);'#010+
  '  if ExtractFileExt(N)='#039#039' then'#010+
  '    N:=ChangeFileExt(N,IncExt);'#010+
  '  Result:=inherited Add(N,CPUs,OSes) as TDependency;'#010+
  '  R','esult.FDependencyType:=depInclude;'#010+
  'end;'#010+
  #010+
  'function TDependencies.GetEnumerator: TDependencyEnumerator;'#010+
  'begin'#010+
  '  Result:=TDependencyEnumerator.Create(Self);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  ' ','                              TValueItem'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TValueItem.Create(AValue: String);'#010+
  'begin'#010+
  '  FValue:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  '{*****************************************','**************************'+
  '*********'#010+
  '                              TFunctionItem'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TFunctionItem.Create(AFunc: TReplaceFunction);'#010+
  'begin'#010+
  '  FFunc:=AFunc;'#010,
  'end;'#010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                           TNotifyEventItem'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'procedure TNotifyEventItem.CallEv','ent(Sender: TObject);'#010+
  'begin'#010+
  '  if assigned(OnEvent) then'#010+
  '    OnEvent(Sender);'#010+
  '  if assigned(OnProcEvent) then'#010+
  '    OnProcEvent(sender);'#010+
  'end;'#010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                      ','     TNotifyEventCollection'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TNotifyEventCollection.create(ASupportedActionSet: TNotify'+
  'EventActionSet);'#010+
  'begin'#010+
  '  FSupportedActionSet:=ASupportedActionS','et;'#010+
  '  inherited create(TNotifyEventItem);'#010+
  'end;'#010+
  #010+
  'function TNotifyEventCollection.GetEnumerator: TNotifyEventEnumerator;'+
  #010+
  'begin'#010+
  '  Result:= TNotifyEventEnumerator.Create(Self);'#010+
  'end;'#010+
  #010+
  'procedure TNotifyEventCollection.AppendEvent(AnAction: TNotif','yEventA'+
  'ction; AnEvent: TNotifyEvent);'#010+
  'var'#010+
  '  item: TNotifyEventItem;'#010+
  'begin'#010+
  '  if not (AnAction in FSupportedActionSet) then'#010+
  '    raise Exception.Create(SErrEventNotSupported);'#010+
  '  item := TNotifyEventItem(add);'#010+
  '  item.OnEvent:=AnEvent;'#010+
  '  item.On','Action:=AnAction;'#010+
  'end;'#010+
  #010+
  'procedure TNotifyEventCollection.AppendProcEvent(AnACtion: TNotifyEven'+
  'tAction;'#010+
  '  AnProcEvent: TNotifyProcEvent);'#010+
  'var'#010+
  '  item: TNotifyEventItem;'#010+
  'begin'#010+
  '  if not (AnAction in FSupportedActionSet) then'#010+
  '    raise Exception','.Create(SErrEventNotSupported);'#010+
  '  item := TNotifyEventItem(add);'#010+
  '  item.OnProcEvent:=AnProcEvent;'#010+
  '  item.OnAction:=AnAction;'#010+
  'end;'#010+
  #010+
  'procedure TNotifyEventCollection.CallEvents(AnAction: TNotifyEventActi'+
  'on; Sender: TObject);'#010+
  'var'#010+
  '  i: integer;',#010+
  '  item: TNotifyEventItem;'#010+
  'begin'#010+
  '  for i := 0 to Count-1 do'#010+
  '    begin'#010+
  '      item := TNotifyEventItem(Items[i]);'#010+
  '      if item.OnAction=AnAction then'#010+
  '        item.CallEvent(Sender);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  '{*******************************************','************************'+
  '*********'#010+
  '                                 TDictionary'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'procedure TDictionary.ClearItem(Idx: Integer);'#010+
  #010+
  'Var'#010+
  '  O : TObject;'#010+
  #010+
  'begin'#010+
  '  O:=FLi','st.Objects[Idx];'#010+
  '  O.Free;'#010+
  '  FList.Objects[Idx]:=nil;'#010+
  'end;'#010+
  #010+
  'constructor TDictionary.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  inherited Create(AOwner);'#010+
  '  FList:=TStringList.Create;'#010+
  '  FList.Sorted:=True;'#010+
  '  FList.Duplicates:=dupError;'#010+
  '  FList.OwnsO','bjects:=True;'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TDictionary.Destroy;'#010+
  #010+
  'begin'#010+
  '  FreeAndNil(FList);'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TDictionary.AddVariable(const AName, Value: String);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  I:=Flist.IndexOf(AName);'#010+
  '  If I=-1 then'#010+
  ' ','   I:=FList.Add(Aname)'#010+
  '  else'#010+
  '    ClearItem(I);'#010+
  '  Flist.Objects[i]:=TValueItem.Create(Value);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TDictionary.AddFunction(const AName: String; FReplacement: T'+
  'ReplaceFunction);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=Flist.IndexOf(AName)',';'#010+
  '  If I=-1 then'#010+
  '    I:=Flist.Add(AName)'#010+
  '  else'#010+
  '    ClearItem(I);'#010+
  '  Flist.Objects[i]:=TFunctionItem.Create(FReplacement);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TDictionary.RemoveItem(const AName: String);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=Flist.IndexOf(AName);'#010+
  '  I','f (I<>-1) then'#010+
  '    begin'#010+
  '    ClearItem(I);'#010+
  '    FList.Delete(I);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'function TDictionary.GetValue(AName: String): String;'#010+
  'begin'#010+
  '  Result:=GetValue(AName,'#039#039');'#010+
  'end;'#010+
  #010+
  #010+
  'function TDictionary.GetValue(const AName,Args: String): String',';'#010+
  'Var'#010+
  '  O : TObject;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  I:=Flist.IndexOf(AName);'#010+
  '  If (I=-1) then'#010+
  '    Raise EDictionaryError.CreateFmt(SErrNoDictionaryItem,[AName]);'#010+
  '  O:=Flist.Objects[I];'#010+
  '  If O is TValueItem then'#010+
  '    Result:=TValueItem(O).FValue'#010+
  '  el','se'#010+
  '    Result:=TFunctionItem(O).FFunc(AName,Args);'#010+
  'end;'#010+
  #010+
  #010+
  'function TDictionary.ReplaceStrings(Const ASource: String; Const MaxDe'+
  'pth: Integer = 10): String;'#010+
  'Var'#010+
  '  S,FN,FV : String;'#010+
  '  P: Integer;'#010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  S:=ASource;'#010+
  '  P:=Pos('#039'$(',#039',S);'#010+
  '  While (P<>0) do'#010+
  '    begin'#010+
  '      Result:=Result+Copy(S,1,P-1);'#010+
  '      Delete(S,1,P+1);'#010+
  '      P:=Pos('#039')'#039',S);'#010+
  '      FN:=Copy(S,1,P-1);'#010+
  '      Delete(S,1,P);'#010+
  '      P:=Pos('#039' '#039',FN);'#010+
  '      If (P<>0) then // function arguments ?'#010+
  '        begin',#010+
  '        FV:=FN;'#010+
  '        FN:=Copy(FN,1,P);'#010+
  '        System.Delete(FV,1,P);'#010+
  '        end'#010+
  '      else'#010+
  '        FV:='#039#039';'#010+
  '      if MaxDepth > 0 then'#010+
  '        Result:=Result+ReplaceStrings(GetValue(FN,FV), MaxDepth-1)'#010+
  '      else'#010+
  '        Result:=Result','+GetValue(FN,FV);'#010+
  '      P:=Pos('#039'$('#039',S);'#010+
  '    end;'#010+
  '  Result:=Result+S;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TDictionary.Substitute(Const Source : String; const Macros : '+
  'Array of string) : String;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  I:=0;'#010+
  '  While I<High(Macros) do'#010+
  '    begi','n'#010+
  '      AddVariable(Macros[i],Macros[I+1]);'#010+
  '      Inc(I,2);'#010+
  '    end;'#010+
  '  Result:=ReplaceStrings(Source);'#010+
  '  I:=0;'#010+
  '  While I<High(Macros) do'#010+
  '    begin'#010+
  '      RemoveItem(Macros[i]);'#010+
  '      Inc(I,2);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  '{********************************','***********************************'+
  '*********'#010+
  '                                 Default Instances'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'var'#010+
  '  DefInstaller : TCustomInstaller;'#010+
  #010+
  'Function Installer(Instal','lerClass: TInstallerClass): TCustomInstalle'+
  'r;'#010+
  'begin'#010+
  '  If Not Assigned(DefInstaller) then'#010+
  '    begin'#010+
  '      try'#010+
  '        DefInstaller:=InstallerClass.Create(Nil);'#010+
  '      except'#010+
  '        On E : Exception do'#010+
  '          begin'#010+
  '            if IsConsole',' then'#010+
  '              begin'#010+
  '                WriteLn(SErrInstaller);'#010+
  '                WriteLn(E.Message);'#010+
  '                halt(1);'#010+
  '              end'#010+
  '            else'#010+
  '              raise;'#010+
  '          end;'#010+
  '      end;'#010+
  '    end;'#010+
  '  Result:=DefInstaller',';'#010+
  'end;'#010+
  #010+
  #010+
  'Function Installer: TCustomInstaller;'#010+
  'begin'#010+
  '  Result := Installer(TFPCInstaller);'#010+
  'end;'#010+
  #010+
  #010+
  #010+
  '{ TCommand }'#010+
  #010+
  'function TCommand.GetOptions: TStrings;'#010+
  'begin'#010+
  '  If (FOptions=Nil) then'#010+
  '    FOptions:=TUnsortedCompilerOptionsStringList.Create;',#010+
  '  Result:=FOptions;'#010+
  'end;'#010+
  #010+
  'procedure TCommand.SetOptions(const Value: TStrings);'#010+
  'begin'#010+
  '  If (Value=Nil) or (Value.Count=0) then'#010+
  '    FreeAndNil(FOptions)'#010+
  '  else'#010+
  '    Options.Assign(Value);'#010+
  'end;'#010+
  #010+
  'destructor TCommand.Destroy;'#010+
  'begin'#010+
  '  FreeAndNil','(FOptions);'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  'function TCommand.HaveOptions: Boolean;'#010+
  'begin'#010+
  '  Result:=(FOptions<>Nil);'#010+
  'end;'#010+
  #010+
  #010+
  'function TCommand.CmdLineOptions: String;'#010+
  'begin'#010+
  '  If HaveOptions then'#010+
  '    Result:=OptionListToString(Options);'#010+
  'end;'#010+
  #010+
  'proc','edure TCommand.ParseOptions(S: String);'#010+
  #010+
  'begin'#010+
  '  Options:=OptionsToStringList(S);'#010+
  'end;'#010+
  #010+
  'Initialization'#010+
  '  OnGetApplicationName:=@GetFPMakeName;'#010+
  '  CustomFpmakeCommandlineOptions:=nil;'#010+
  '  CustomFpMakeCommandlineValues:=nil;'#010+
  #010+
  '  GetPluginManager.','RegisterPlugin(TfpmResolvePackagePathsPlugin);'#010+
  #010+
  'Finalization'#010+
  '{$ifndef LLVM_INTERFACE_PROBLEM}'#010+
  '  FreeAndNil(CustomFpMakeCommandlineValues);'#010+
  '  FreeAndNil(CustomFpmakeCommandlineOptions);'#010+
  '  FreeAndNil(DefInstaller);'#010+
  '  FreeAndNil(GlobalDictiona','ry);'#010+
  '  FreeAndNil(Defaults);'#010+
  '  FreeAndNil(GPluginManager);'#010+
  '{$endif ndef LLVM_INTERFACE_PROBLEM}'#010+
  'end.'#010+
  #010+
  #010
);
