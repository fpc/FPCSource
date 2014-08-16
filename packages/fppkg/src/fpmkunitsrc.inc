{$ifdef Delphi}
const fpmkunitsrc : array[0..1041] of string[240]=(
{$else Delphi}
const fpmkunitsrc : array[0..1041,1..240] of char=(
{$endif Delphi}
  '{'#013#010+
  '    This file is part of the Free Pascal Makefile Package'#013#010+
  #013#010+
  '    Implementation of fpmake classes and functions'#013#010+
  #013#010+
  '    Copyright (c) 2007 by the freepascal team'#013#010+
  #013#010+
  '    See the file COPYING.FPC, included in this distribution,'#013#010+
  '    for det','ails about the copyright.'#013#010+
  #013#010+
  '    This program is distributed in the hope that it will be useful,'#013+
  #010+
  '    but WITHOUT ANY WARRANTY; without even the implied warranty of'#013+
  #010+
  '    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.'#013#010+
  #013#010+
  ' *************','******************************************************'+
  '***}'#013#010+
  #013#010+
  'unit fpmkunit;'#013#010+
  #013#010+
  '{$Mode objfpc}'#013#010+
  '{$H+}'#013#010+
  '{$inline on}'#013#010+
  #013#010+
  '{ For target or cpu dependent dependencies also add an overload where '+
  'you'#013#010+
  '  can pass only a set of cpus. This is disable','d for now because it c'+
  'reates'#013#010+
  '  an error in the compiler with overload choosing }'#013#010+
  '{ define cpu_only_overloads}'#013#010+
  #013#010+
  'Interface'#013#010+
  #013#010+
  '{$IFDEF MORPHOS}'#013#010+
  ' {$DEFINE NO_UNIT_PROCESS}'#013#010+
  ' {$DEFINE NO_THREADING}'#013#010+
  '{$ENDIF}'#013#010+
  #013#010+
  '{$IFDEF OS2}'#013#010+
  ' {$DEFINE NO_UN','IT_PROCESS}'#013#010+
  '{$ENDIF OS2}'#013#010+
  #013#010+
  '{$IFDEF GO32V2}'#013#010+
  ' {$DEFINE NO_UNIT_PROCESS}'#013#010+
  '{$ENDIF GO32V2}'#013#010+
  #013#010+
  '{$IFDEF NETBSD}'#013#010+
  ' { NetBSD pthreads are not yet working, try to use fpmake without thre'+
  'ads }'#013#010+
  '  {$DEFINE NO_THREADING}'#013#010+
  '{$ENDIF NETBSD}'#013#010+
  #013#010+
  '{$ifndef',' NO_UNIT_PROCESS}'#013#010+
  '  {$define HAS_UNIT_PROCESS}'#013#010+
  '{$endif NO_UNIT_PROCESS}'#013#010+
  #013#010+
  '{$ifndef NO_UNIT_ZIPPER}'#013#010+
  '  {$define HAS_UNIT_ZIPPER}'#013#010+
  '{$endif NO_UNIT_ZIPPER}'#013#010+
  #013#010+
  '{$ifndef NO_TAR_SUPPORT}'#013#010+
  '  {$define HAS_TAR_SUPPORT}'#013#010+
  '{$endif NO_TAR_SUPPORT}'#013#010+
  #013,#010+
  'uses'#013#010+
  '{$ifdef UNIX}'#013#010+
  '  BaseUnix,'#013#010+
  '{$endif UNIX}'#013#010+
  '{$ifndef NO_THREADING}'#013#010+
  '{$ifdef UNIX}'#013#010+
  '  cthreads,'#013#010+
  '{$endif UNIX}'#013#010+
  '{$endif NO_THREADING}'#013#010+
  '  SysUtils, Classes, StrUtils'#013#010+
  '{$ifdef HAS_UNIT_PROCESS}'#013#010+
  '  ,process'#013#010+
  '{$endif HAS_UNIT_PROCESS}'#013#010+
  '{$i','fdef HAS_TAR_SUPPORT}'#013#010+
  '  ,libtar'#013#010+
  '{$endif HAS_TAR_SUPPORT}'#013#010+
  '{$ifdef HAS_UNIT_ZIPPER}'#013#010+
  '  ,zipper, zstream'#013#010+
  '{$endif HAS_UNIT_ZIPPER}'#013#010+
  '  ;'#013#010+
  #013#010+
  'Type'#013#010+
  '  TFileType = (ftSource,ftUnit,ftObject,ftResource,ftExecutable,ftStat'+
  'icLibrary,'#013#010+
  '             ','  ftSharedLibrary);'#013#010+
  '  TFileTypes = set of TFileType;'#013#010+
  #013#010+
  '  // Please keep this order, see OSCPUSupported below'#013#010+
  '  TCpu=(cpuNone,'#013#010+
  '    i386,m68k,powerpc,sparc,x86_64,arm,powerpc64,avr,armeb,'#013#010+
  '    mips,mipsel,jvm,i8086'#013#010+
  '  );'#013#010+
  '  TCPUS = Set of',' TCPU;'#013#010+
  #013#010+
  '  // Please keep this order, see OSCPUSupported below'#013#010+
  '  TOS=(osNone,'#013#010+
  '    linux,go32v2,win32,os2,freebsd,beos,netbsd,'#013#010+
  '    amiga,atari, solaris, qnx, netware, openbsd,wdosx,'#013#010+
  '    palmos,macos,darwin,emx,watcom,morphos,netwlibc,'#013#010,
  '    win64,wince,gba,nds,embedded,symbian,haiku,iphonesim,'#013#010+
  '    aix,java,android,nativent,msdos,wii'#013#010+
  '  );'#013#010+
  '  TOSes = Set of TOS;'#013#010+
  #013#010+
  '  TCompilerMode = (cmFPC,cmTP,cmObjFPC,cmDelphi,cmMacPas);'#013#010+
  '  TCompilerModes = Set of TCompilerMode;'#013#010+
  #013#010+
  '  TTa','rgetType = (ttProgram,ttUnit,ttImplicitUnit,ttCleanOnlyUnit,ttE'+
  'xampleUnit,ttExampleProgram,ttFPDoc);'#013#010+
  '  TTargetTypes = set of TTargetType;'#013#010+
  #013#010+
  '  TFPDocFormat = (ffHtml, ffHtm, ffXHtml, ffLaTex, ffXMLStruct, ffChm)'+
  ';'#013#010+
  '  TFPDocFormats = set of ','TFPDocFormat;'#013#010+
  #013#010+
  '  TTargetState = (tsNeutral,tsConsidering,tsNoCompile,tsCompiled,tsIns'+
  'talled,tsNotFound);'#013#010+
  '  TTargetStates = Set of TTargetState;'#013#010+
  #013#010+
  '  TSourceType = (stDoc,stSrc,stExample,stTest);'#013#010+
  '  TSourceTypes = set of TSourceType;'#013#010+
  #013#010+
  ' ',' TVerboseLevel = (vlError,vlWarning,vlInfo,vldebug,vlCommand);'#013#010+
  '  TVerboseLevels = Set of TVerboseLevel;'#013#010+
  #013#010+
  '  TCommandAt = (caBeforeCompile,caAfterCompile,'#013#010+
  '                caBeforeInstall,caAfterInstall,'#013#010+
  '                caBeforeArchive,ca','AfterArchive,'#013#010+
  '                caBeforeClean,caAfterClean,'#013#010+
  '                caBeforeDownload,caAfterDownload);'#013#010+
  #013#010+
  '  TDependencyType = (depPackage,depImplicitPackage,depUnit,depInclude)'+
  ';'#013#010+
  '  TDependencyTypes = set of TDependencyType;'#013#010+
  #013#010+
  '  TLo','gEvent = Procedure (Level : TVerboseLevel; Const Msg : String) '+
  'of Object;'#013#010+
  '  TNotifyProcEvent = procedure(Sender: TObject);'#013#010+
  #013#010+
  '  TRunMode = (rmCompile,rmBuild,rmInstall,rmArchive,rmClean,rmDistClea'+
  'n,rmManifest,rmZipInstall);'#013#010+
  #013#010+
  '  TBuildMode',' = (bmOneByOne, bmBuildUnit{, bmSkipImplicitUnits});'#013+
  #010+
  '  TBuildModes = set of TBuildMode;'#013#010+
  '  TProcessPackageResult = (ppHandled, ppDelayed);'#013#010+
  '  TCheckDependencyResult = (cdAvailable, cdNotAvailable, cdNotYetAvail'+
  'able);'#013#010+
  #013#010+
  'Const'#013#010+
  '  // Aliases',#013#010+
  '  Amd64   = X86_64;'#013#010+
  '  PPC = PowerPC;'#013#010+
  '  PPC64 = PowerPC64;'#013#010+
  '  DOS = Go32v2;'#013#010+
  '  MacOSX = Darwin;'#013#010+
  #013#010+
  '  AllOSes = [Low(TOS)..High(TOS)];'#013#010+
  '  AllCPUs = [Low(TCPU)..High(TCPU)];'#013#010+
  '  AllUnixOSes  = [Linux,FreeBSD,NetBSD,OpenBSD,Darwin,QNX,BeOS,S','olar'+
  'is,Haiku,iphonesim,aix,Android];'#013#010+
  '  AllBSDOSes      = [FreeBSD,NetBSD,OpenBSD,Darwin,iphonesim];'#013#010+
  '  AllWindowsOSes  = [Win32,Win64,WinCE];'#013#010+
  '  AllLimit83fsOses= [go32v2,os2,emx,watcom,msdos];'#013#010+
  #013#010+
  '  AllSmartLinkLibraryOSes = [Linux,msdos,a','miga,morphos]; // OSes tha'+
  't use .a library files for smart-linking'#013#010+
  '  AllImportLibraryOSes = AllWindowsOSes + [os2,emx,netwlibc,netware,wa'+
  'tcom,go32v2,macos,nativent,msdos];'#013#010+
  #013#010+
  '  { This table is kept OS,Cpu because it is easier to maintain (','PFV)'+
  ' }'#013#010+
  '  OSCPUSupported : array[TOS,TCpu] of boolean = ('#013#010+
  '    { os          none   i386    m68k  ppc    sparc  x86_64 arm    ppc'+
  '64  avr    armeb  mips   mipsel jvm    i8086}'#013#010+
  '    { none }    ( false, false, false, false, false, false, fals','e, f'+
  'alse, false, false, false, false, false, false),'#013#010+
  '    { linux }   ( false, true,  true,  true,  true,  true,  true,  tru'+
  'e,  false, true , true , true , false, false),'#013#010+
  '    { go32v2 }  ( false, true,  false, false, false, false, false, f','a'+
  'lse, false, false, false, false, false, false),'#013#010+
  '    { win32 }   ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false),'#013#010+
  '    { os2 }     ( false, true,  false, false, false, false, false, fal'+
  'se',', false, false, false, false, false, false),'#013#010+
  '    { freebsd } ( false, true,  true,  false, false, true,  false, fal'+
  'se, false, false, false, false, false, false),'#013#010+
  '    { beos }    ( false, true,  false, false, false, false, false, fal'+
  'se, fa','lse, false, false, false, false, false),'#013#010+
  '    { netbsd }  ( false, true,  true,  true,  true,  false, false, fal'+
  'se, false, false, false, false, false, false),'#013#010+
  '    { amiga }   ( false, false, true,  true,  false, false, false, fal'+
  'se, false,',' false, false, false, false, false),'#013#010+
  '    { atari }   ( false, false, true,  false, false, false, false, fal'+
  'se, false, false, false, false, false, false),'#013#010+
  '    { solaris } ( false, true,  false, false, true,  false, false, fal'+
  'se, false, fal','se, false, false, false, false),'#013#010+
  '    { qnx }     ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false),'#013#010+
  '    { netware } ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, ','false, false, false, false),'#013#010+
  '    { openbsd } ( false, true,  true,  false, false, false, false, fal'+
  'se, false, false, false, false, false, false),'#013#010+
  '    { wdosx }   ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, fals','e, false, false, false),'#013#010+
  '    { palmos }  ( false, false, true,  false, false, false, true,  fal'+
  'se, false, false, false, false, false, false),'#013#010+
  '    { macos }   ( false, false, false, true,  false, false, false, fal'+
  'se, false, false, false, f','alse, false, false),'#013#010+
  '    { darwin }  ( false, true,  false, true,  false, true,  true,  tru'+
  'e,  false, false, false, false, false, false),'#013#010+
  '    { emx }     ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false',', false, false),'#013#010+
  '    { watcom }  ( false, true,  false, false, false ,false, false, fal'+
  'se, false, false, false, false, false, false),'#013#010+
  '    { morphos } ( false, false, false, true,  false ,false, false, fal'+
  'se, false, false, false, false, fa','lse, false),'#013#010+
  '    { netwlibc }( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false),'#013#010+
  '    { win64   } ( false, false, false, false, false, true,  false, fal'+
  'se, false, false, false, false, false,',' false),'#013#010+
  '    { wince    }( false, true,  false, false, false, false, true,  fal'+
  'se, false, false, false, false, false, false),'#013#010+
  '    { gba    }  ( false, false, false, false, false, false, true,  fal'+
  'se, false, false, false, false, false, fal','se),'#013#010+
  '    { nds    }  ( false, false, false, false, false, false, true,  fal'+
  'se, false, false, false, false, false, false),'#013#010+
  '    { embedded }( false, true,  true,  true,  true,  true,  true,  tru'+
  'e,  true,  true , false, false, false, false),',#013#010+
  '    { symbian } ( false, true,  false, false, false, false, true,  fal'+
  'se, false, false, false, false, false, false),'#013#010+
  '    { haiku }   ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false, false),'#013#010+
  '  ','  { iphonesim}( false, true,  false, false, false, false, false, f'+
  'alse, false, false, false, false, false, false),'#013#010+
  '    { aix    }  ( false, false, false, true,  false, false, false, tru'+
  'e,  false, false, false, false, false, false),'#013#010+
  '    { ','java }    ( false, false, false, false, false, false, false, f'+
  'alse, false, false, false, false, true , false),'#013#010+
  '    { android } ( false, true,  false, false, false, false, true,  fal'+
  'se, false, false, false, true,  true , false),'#013#010+
  '    { nati','vent }( false, true,  false, false, false, false, false, f'+
  'alse, false, false, false, false, false, false),'#013#010+
  '    { msdos }   ( false, false, false, false, false, false, false, fal'+
  'se, false, false, false, false, false, true ),'#013#010+
  '    { wii }   ','  ( false, false, false, true , false, false, false, f'+
  'alse, false, false, false, false, false, false )'#013#010+
  '  );'#013#010+
  #013#010+
  '  // Useful'#013#010+
  '  UnitExt = '#039'.ppu'#039';'#013#010+
  '  PPUExt  = UnitExt;'#013#010+
  '  PasExt  = '#039'.pas'#039';'#013#010+
  '  PPExt   = '#039'.pp'#039';'#013#010+
  '  IncExt  = '#039'.inc'#039';'#013#010+
  '  ObjExt  ','= '#039'.o'#039';'#013#010+
  '  RstExt  = '#039'.rst'#039';'#013#010+
  '  RsjExt  = '#039'.rsj'#039';'#013#010+
  '  LibExt  = '#039'.a'#039';'#013#010+
  '  SharedLibExt = '#039'.so'#039';'#013#010+
  '  DLLExt  = '#039'.dll'#039';'#013#010+
  '  ExeExt  = '#039'.exe'#039';'#013#010+
  '  DbgExt  = '#039'.dbg'#039';'#013#010+
  '  ZipExt  = '#039'.zip'#039';'#013#010+
  #013#010+
  '  FPMakePPFile = '#039'fpmake.pp'#039';'#013#010+
  '  ManifestFile = '#039'manifest.x','ml'#039';'#013#010+
  '  UnitConfigFile = '#039'fpunits.cfg'#039';'#013#010+
  #013#010+
  '  DirNotFound = '#039'<dirnotfound>'#039';'#013#010+
  #013#010+
  '  UnitTargets = [ttUnit,ttImplicitUnit,ttCleanOnlyUnit,ttExampleUnit];'+
  #013#010+
  '  ProgramTargets = [ttProgram,ttExampleProgram];'#013#010+
  #013#010+
  '  DefaultMessages = [vlError,vlWarnin','g,vlCommand];'#013#010+
  '  AllMessages = [vlError,vlWarning,vlCommand,vlInfo];'#013#010+
  #013#010+
  'Type'#013#010+
  '  TTargets = Class;'#013#010+
  '  { TNamedItem }'#013#010+
  #013#010+
  '  TNamedItem = Class(TCollectionItem)'#013#010+
  '  private'#013#010+
  '    FName: String;'#013#010+
  '    procedure SetName(const AValue: String);virtual',';'#013#010+
  '  Public'#013#010+
  '    property Name : String Read FName Write SetName;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TNamedCollection }'#013#010+
  #013#010+
  '  TNamedCollection = Class(TCollection)'#013#010+
  '  private'#013#010+
  '    FUniqueNames: Boolean;'#013#010+
  '  private'#013#010+
  '    function GetItem(Index: Integer): TNamedIt','em;'#013#010+
  '    procedure SetItem(Index: Integer; AValue: TNamedItem);'#013#010+
  '  Public'#013#010+
  '    Function IndexOfName(const AName : String) : Integer;'#013#010+
  '    Function ItemByName(const AName : String) : TNamedItem;'#013#010+
  '    Property UniqueNames : Boolean Read FUniq','ueNames;'#013#010+
  '    property Items[Index: Integer]: TNamedItem read GetItem write SetI'+
  'tem;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TNamedItemList }'#013#010+
  #013#010+
  '  TNamedItemList = Class(TFPList)'#013#010+
  '  private'#013#010+
  '    function GetNamedItem(Index : Integer): TNamedItem;'#013#010+
  '    procedure Set','NamedItem(Index : Integer; const AValue: TNamedItem'+
  ');'#013#010+
  '  public'#013#010+
  '    Function IndexOfName(const AName : String) : Integer;'#013#010+
  '    Function ItemByName(const ANAme : String) : TNamedItem;'#013#010+
  '    Property NamedItems[Index : Integer] : TNamedItem R','ead GetNamedI'+
  'tem Write SetNamedItem; default;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TCommand }'#013#010+
  '  TCommand = Class(TNamedItem)'#013#010+
  '  private'#013#010+
  '    FAfterCommand: TNotifyEvent;'#013#010+
  '    FBeforeCommand: TNotifyEvent;'#013#010+
  '    FCommand: String;'#013#010+
  '    FCommandAt: TCommandAt;'#013#010+
  '   ',' FDestFile: String;'#013#010+
  '    FIgnoreResult: Boolean;'#013#010+
  '    FOptions: TStrings;'#013#010+
  '    FSourceFile: String;'#013#010+
  '    Function GetOptions : TStrings;'#013#010+
  '    Procedure SetOptions(Const Value : TStrings);'#013#010+
  '  Public'#013#010+
  '    Destructor Destroy; override;'#013#010+
  '    Fu','nction HaveOptions : Boolean;'#013#010+
  '    Function CmdLineOptions : String;'#013#010+
  '    Procedure ParseOptions(S : String);'#013#010+
  '    Property SourceFile : String Read FSourceFile Write FSourceFile;'#013+
  #010+
  '    Property DestFile : String Read FDestFile Write FDestFi','le;'#013#010+
  '    Property Command : String Read FCommand Write FCommand;'#013#010+
  '    Property Options : TStrings Read GetOptions Write SetOptions;'#013#010+
  '    Property At : TCommandAt Read FCommandAt Write FCommandAt;'#013#010+
  '    Property IgnoreResult : Boolean Read FI','gnoreResult Write FIgnore'+
  'Result;'#013#010+
  '    Property BeforeCommand : TNotifyEvent Read FBeforeCommand Write FB'+
  'eforeCommand;'#013#010+
  '    Property AfterCommand : TNotifyEvent Read FAfterCommand Write FAft'+
  'erCommand;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TCommands }'#013#010+
  #013#010+
  '  TCommand','s = Class(TNamedCollection)'#013#010+
  '  private'#013#010+
  '    FDefaultAt: TCommandAt;'#013#010+
  '    function GetCommand(const Dest : String): TCommand;'#013#010+
  '    function GetCommandItem(Index : Integer): TCommand;'#013#010+
  '    procedure SetCommandItem(Index : Integer; const AValu','e: TCommand'+
  ');'#013#010+
  '  Public'#013#010+
  '    Function AddCommand(Const Cmd : String) : TCommand;'#013#010+
  '    Function AddCommand(Const Cmd,Options : String) : TCommand;'#013#010+
  '    Function AddCommand(Const Cmd,Options,Dest,Source : String) : TCom'+
  'mand;'#013#010+
  '    Function Add','Command(At : TCommandAt; Const Cmd : String) : TComm'+
  'and;'#013#010+
  '    Function AddCommand(At : TCommandAt; Const Cmd,Options : String) :'+
  ' TCommand;'#013#010+
  '    Function AddCommand(At : TCommandAt; Const Cmd,Options, Dest,Sourc'+
  'e : String) : TCommand;'#013#010+
  '    P','roperty CommandItems[Index : Integer] : TCommand Read GetComman'+
  'dItem Write SetCommandItem;'#013#010+
  '    Property Commands[Dest : String] : TCommand Read GetCommand; defau'+
  'lt;'#013#010+
  '    Property DefaultAt : TCommandAt Read FDefaultAt Write FDefaultAt;'#013+
  #010+
  '  ','end;'#013#010+
  #013#010+
  '  { TFPVersion }'#013#010+
  #013#010+
  '  TFPVersion = Class(TPersistent)'#013#010+
  '  private'#013#010+
  '    FMajor,'#013#010+
  '    FMinor,'#013#010+
  '    FMicro,'#013#010+
  '    FBuild    : Integer;'#013#010+
  '    function GetAsString: String;'#013#010+
  '    function GetEmpty: Boolean;'#013#010+
  '    procedure SetAsString(const A','Value: String);'#013#010+
  '  Public'#013#010+
  '   Procedure Clear;'#013#010+
  '   Procedure Assign(Source : TPersistent); override;'#013#010+
  '   Function CompareVersion(AVersion : TFPVersion) : Integer;'#013#010+
  '   Function SameVersion(AVersion : TFPVersion) : Boolean;'#013#010+
  '   Property AsStr','ing : String Read GetAsString Write SetAsString;'#013+
  #010+
  '   Property Empty : Boolean Read GetEmpty;'#013#010+
  '  Published'#013#010+
  '   Property Major : Integer Read FMajor Write FMajor;'#013#010+
  '   Property Minor : Integer Read FMinor Write FMinor;'#013#010+
  '   Property Micro : Int','eger Read FMicro Write FMicro;'#013#010+
  '   Property Build : Integer Read FBuild Write FBuild;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TConditionalString }'#013#010+
  '  TConditionalString = Class'#013#010+
  '  private'#013#010+
  '    FOSes   : TOSes;'#013#010+
  '    FCPUs   : TCPUs;'#013#010+
  '    FValue  : String;'#013#010+
  '  Public'#013,#010+
  '    Constructor Create;virtual;'#013#010+
  '    Property Value : String Read FValue Write FValue;'#013#010+
  '    Property OSes  : TOSes Read FOSes Write FOSes;'#013#010+
  '    Property CPUs : TCPUs Read FCPUS Write FCPUs;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  TConditionalStringClass = class of T','ConditionalString;'#013#010+
  #013#010+
  '  { TConditionalStrings }'#013#010+
  #013#010+
  '  TConditionalStrings = Class(TFPList)'#013#010+
  '  private'#013#010+
  '    FCSClass : TConditionalStringClass;'#013#010+
  '    function GetConditionalString(Index : Integer): TConditionalString'+
  ';'#013#010+
  '    procedure SetConditi','onalString(Index : Integer; const AValue: TC'+
  'onditionalString);'#013#010+
  '  Public'#013#010+
  '    Constructor Create(AClass:TConditionalStringClass);'#013#010+
  '    Function Add(Const Value : String) : TConditionalString;inline;'#013+
  #010+
  '    Function Add(Const Value : String;co','nst OSes:TOSes) : TCondition'+
  'alString;inline;'#013#010+
  '{$ifdef cpu_only_overloads}'#013#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs) : TConditional'+
  'String;inline;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  '    Function Add(Const Value : String;const CPUs:T','CPUs;const OSes:TO'+
  'Ses) : TConditionalString;'#013#010+
  '    Property ConditionalStrings[Index : Integer] : TConditionalString '+
  'Read GetConditionalString Write SetConditionalString; default;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TConditionalDestString }'#013#010+
  #013#010+
  '  TConditionalDest','String = Class(TConditionalString)'#013#010+
  '  private'#013#010+
  '    FDestPath: string;'#013#010+
  '  public'#013#010+
  '    property DestPath: string read FDestPath write FDestPath;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  TConditionalDestStringClass = class of TConditionalDestString;'#013#010+
  #013#010+
  '  { TConditionalDe','stStrings }'#013#010+
  #013#010+
  '  TConditionalDestStrings = Class(TConditionalStrings)'#013#010+
  '  private'#013#010+
  '    function GetConditionalString(Index : Integer): TConditionalDestSt'+
  'ring;'#013#010+
  '    procedure SetConditionalString(Index : Integer; AValue: TCondition'+
  'alDestStrin','g);'#013#010+
  '  public'#013#010+
  '    Constructor Create(AClass:TConditionalDestStringClass);'#013#010+
  '    Function Add(Const Value : String; ADestPath: String) : TCondition'+
  'alDestString;inline;'#013#010+
  '    Function Add(Const Value : String;const OSes:TOSes; ADestPath: Str'+
  'in','g) : TConditionalDestString;inline;'#013#010+
  '{$ifdef cpu_only_overloads}'#013#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs; ADestPath: Str'+
  'ing) : TConditionalDestString;inline;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  '    Function Add(Const Value : Strin','g;const CPUs:TCPUs;const OSes:TO'+
  'Ses; ADestPath: String) : TConditionalDestString;'#013#010+
  '    Property ConditionalStrings[Index : Integer] : TConditionalDestStr'+
  'ing Read GetConditionalString Write SetConditionalString; default;'#013+
  #010+
  '  end;'#013#010+
  #013#010+
  '  { TDict','ionary }'#013#010+
  #013#010+
  '  TReplaceFunction = Function (Const AName,Args : String) : String of '+
  'Object;'#013#010+
  #013#010+
  '  TDictionary = Class(TComponent)'#013#010+
  '  private'#013#010+
  '    FList : TStringList;'#013#010+
  '  Public'#013#010+
  '    Constructor Create(AOwner : TComponent); override;'#013#010+
  '    Destr','uctor Destroy;override;'#013#010+
  '    Procedure AddVariable(const AName,Value : String);'#013#010+
  '    Procedure AddFunction(const AName : String; FReplacement : TReplac'+
  'eFunction);'#013#010+
  '    Procedure RemoveItem(const AName : String);'#013#010+
  '    Function GetValue(AName',' : String) : String;'#013#010+
  '    Function GetValue(const AName,Args : String) : String; virtual;'#013+
  #010+
  '    Function ReplaceStrings(Const ASource : String) : String; virtual;'+
  #013#010+
  '    Function Substitute(Const Source : String; Macros : Array of strin'+
  'g) : St','ring; virtual;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TPackageDictionary }'#013#010+
  #013#010+
  '  TPackageDictionary = Class(TDictionary)'#013#010+
  '  private'#013#010+
  '    FMasterDictionary: TDictionary;'#013#010+
  '  Public'#013#010+
  '    Function GetValue(const AName,Args : String) : String; override;'#013+
  #010+
  '    property Ma','sterDictionary: TDictionary read FMasterDictionary wr'+
  'ite FMasterDictionary;'#013#010+
  '  end;'#013#010+
  #013#010+
  #013#010+
  '  { TDependency }'#013#010+
  '  TDependency = Class(TConditionalString)'#013#010+
  '  private'#013#010+
  '    FDependencyType : TDependencyType;'#013#010+
  '    // Package, Unit'#013#010+
  '    FTarget : TO','bject;'#013#010+
  '    FVersion : TFPVersion;'#013#010+
  '    FRequireChecksum : Cardinal;'#013#010+
  '    // Filenames, Includes'#013#010+
  '    FTargetFileName : String;'#013#010+
  '    Function GetVersion : string;'#013#010+
  '    Procedure SetVersion(const V : string);'#013#010+
  '  Public'#013#010+
  '    Constructor Creat','e;override;'#013#010+
  '    Destructor Destroy;override;'#013#010+
  '    Property Target : TObject Read FTarget Write FTarget;'#013#010+
  '    Property DependencyType : TDependencyType Read FDependencyType;'#013+
  #010+
  '    Property TargetFileName : String Read FTargetFileName Write F','Tar'+
  'getFileName;'#013#010+
  '    Property Version : String Read GetVersion Write SetVersion;'#013#010+
  '    Property RequireChecksum : Cardinal Read FRequireChecksum Write FR'+
  'equireChecksum;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TPackageVariant }'#013#010+
  #013#010+
  '  TPackage = Class;'#013#010+
  '  TPackageVaria','nt = class(TNamedItem)'#013#010+
  '  private'#013#010+
  '    FOptions: TStrings;'#013#010+
  '    FTargets: TTargets;'#013#010+
  '  public'#013#010+
  '    constructor Create(ACollection: TCollection); override;'#013#010+
  '    destructor Destroy; override;'#013#010+
  '    property Options: TStrings read FOptions;'#013#010+
  '  ','  property Targets: TTargets read FTargets;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TPackageVariants }'#013#010+
  #013#010+
  '  TPackageVariants = class(TNamedCollection)'#013#010+
  '  private'#013#010+
  '    FActivePackageVariantName: string;'#013#010+
  '    FDefaultPackageVariantName: string;'#013#010+
  '    FIsInheritable: b','oolean;'#013#010+
  '    FMasterPackage: TPackage;'#013#010+
  '    FName: string;'#013#010+
  '    function GetActivePackageVariant: TPackageVariant;'#013#010+
  '    function GetDefaultPackageVariant: TPackageVariant;'#013#010+
  '    procedure SetActivePackageVariantName(AValue: string);'#013#010+
  '    pro','cedure SetDefaultPackageVariantName(AValue: string);'#013#010+
  '  public'#013#010+
  '    function Add(AName: String): TPackageVariant; overload; virtual;'#013+
  #010+
  '    property Name: string read FName write FName;'#013#010+
  '    property MasterPackage: TPackage read FMasterPackag','e;'#013#010+
  '    property DefaultPackageVariant: TPackageVariant read GetDefaultPac'+
  'kageVariant;'#013#010+
  '    property ActivePackageVariant: TPackageVariant read GetActivePacka'+
  'geVariant;'#013#010+
  '    property DefaultPackageVariantName: string read FDefaultPackageVar',
  'iantName write SetDefaultPackageVariantName;'#013#010+
  '    property ActivePackageVariantName: string read FActivePackageVaria'+
  'ntName write SetActivePackageVariantName;'#013#010+
  '    property IsInheritable: boolean read FIsInheritable;'#013#010+
  '  end;'#013#010+
  #013#010+
  #013#010+
  '  TDependen','cies = Class(TConditionalStrings)'#013#010+
  '    function GetDependency(Index : Integer): TDependency;'#013#010+
  '    procedure SetDependency(Index : Integer; const AValue: TDependency'+
  ');'#013#010+
  '  Public'#013#010+
  '    Function Add(Const Value : String) : TDependency;inline;'#013#010,
  '    Function Add(Const Value : String;const OSes:TOSes) : TDependency;'+
  'inline;'#013#010+
  '{$ifdef cpu_only_overloads}'#013#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs) : TDependency;'+
  'inline;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  '    Function Add(Const V','alue : String;const CPUs:TCPUs;const OSes:TO'+
  'Ses) : TDependency;'#013#010+
  '    Function AddUnit(Const Value : String) : TDependency;inline;'#013#010+
  '    Function AddUnit(Const Value : String;const OSes:TOSes) : TDepende'+
  'ncy;inline;'#013#010+
  '{$ifdef cpu_only_overloads','}'#013#010+
  '    Function AddUnit(Const Value : String;const CPUs:TCPUs) : TDepende'+
  'ncy;inline;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  '    Function AddUnit(Const Value : String;const CPUs:TCPUs;const OSes:'+
  'TOSes) : TDependency;'#013#010+
  '    Function AddInclude(Const Va','lue : String) : TDependency;inline;'#013+
  #010+
  '    Function AddInclude(Const Value : String;const OSes:TOSes) : TDepe'+
  'ndency;inline;'#013#010+
  '{$ifdef cpu_only_overloads}'#013#010+
  '    Function AddInclude(Const Value : String;const CPUs:TCPUs) : TDepe'+
  'ndency;inline;'#013#010+
  '{$','endif cpu_only_overloads}'#013#010+
  '    Function AddInclude(Const Value : String;const CPUs:TCPUs;const OS'+
  'es:TOSes) : TDependency;'#013#010+
  '    Property Dependencies[Index : Integer] : TDependency Read GetDepen'+
  'dency Write SetDependency; default;'#013#010+
  '  end;'#013#010+
  #013#010,
  '  { TTarget }'#013#010+
  #013#010+
  '  TTarget = Class(TNamedItem)'#013#010+
  '  private'#013#010+
  '    FInstall : Boolean;'#013#010+
  '    FAfterClean: TNotifyEvent;'#013#010+
  '    FAfterCompile: TNotifyEvent;'#013#010+
  '    FBeforeClean: TNotifyEvent;'#013#010+
  '    FBeforeCompile: TNotifyEvent;'#013#010+
  '    FCPUs: TCPUs;'#013#010+
  '   ',' FOSes: TOSes;'#013#010+
  '    FMode: TCompilerMode;'#013#010+
  '    FResourceStrings: Boolean;'#013#010+
  '    FObjectPath,'#013#010+
  '    FUnitPath,'#013#010+
  '    FIncludePath : TConditionalStrings;'#013#010+
  '    FDependencies : TDependencies;'#013#010+
  '    FCommands : TCommands;'#013#010+
  '    FDirectory: String;'#013#010+
  ' ','   FExtension: String;'#013#010+
  '    FTargetSourceFileName : String;'#013#010+
  '    FFileType: TFileType;'#013#010+
  '    FOptions: TStrings;'#013#010+
  '    FFPCTarget: String;'#013#010+
  '    FTargetState: TTargetState;'#013#010+
  '    FTargetType: TTargetType;'#013#010+
  '    FXML: string;'#013#010+
  '    function GetOpt','ions: TStrings;'#013#010+
  '    procedure SetOptions(const AValue: TStrings);'#013#010+
  '  Protected'#013#010+
  '    Function GetSourceFileName : String; virtual;'#013#010+
  '    Function GetUnitFileName : String; virtual;'#013#010+
  '    function GetUnitLibFileName(AOS: TOS): String; virtual;',#013#010+
  '    Function GetObjectFileName : String; virtual;'#013#010+
  '    function GetRSTFileName : String; Virtual;'#013#010+
  '    function GetRSJFileName : String; Virtual;'#013#010+
  '    function GetImportLibFileName(AOS : TOS) : String; Virtual;'#013#010+
  '    Function GetProgramFil','eName(AOS : TOS) : String; Virtual;'#013#010+
  '    Function GetProgramDebugFileName(AOS : TOS) : String; Virtual;'#013+
  #010+
  '  Public'#013#010+
  '    Constructor Create(ACollection : TCollection); override;'#013#010+
  '    Destructor Destroy; override;'#013#010+
  '    procedure AssignTo(Dest:',' TPersistent); override;'#013#010+
  '    Function  GetOutputFileName (AOs : TOS) : String; Virtual;'#013#010+
  '    Function HaveOptions : Boolean;'#013#010+
  '    procedure SetName(const AValue: String);override;'#013#010+
  '    procedure SetXML(const AValue: string);'#013#010+
  '    Procedure',' GetCleanFiles(List : TStrings; const APrefixU, APrefix'+
  'B : String; ACPU:TCPU; AOS : TOS); virtual;'#013#010+
  '    Procedure GetInstallFiles(List : TStrings; const APrefixU, APrefix'+
  'B: String; ACPU:TCPU; AOS : TOS); virtual;'#013#010+
  '    Procedure GetArchiveFi','les(List : TStrings; ACPU:TCPU; AOS : TOS)'+
  '; virtual;'#013#010+
  '    Property Dependencies : TDependencies Read FDependencies;'#013#010+
  '    Property Commands : TCommands Read FCommands;'#013#010+
  '    Property State : TTargetState Read FTargetState;'#013#010+
  '    Property Targe','tType : TTargetType Read FTargetType Write FTarget'+
  'Type;'#013#010+
  '    Property OSes : TOSes Read FOSes Write FOSes;'#013#010+
  '    Property CPUs : TCPUs Read FCPUs Write FCPUs;'#013#010+
  '    Property Mode : TCompilerMode Read FMode Write FMode;'#013#010+
  '    Property Options :',' TStrings Read GetOptions Write SetOptions;'#013+
  #010+
  '    Property SourceFileName: String Read GetSourceFileName ;'#013#010+
  '    Property UnitFileName : String Read GetUnitFileName;'#013#010+
  '    Property ObjectFileName : String Read GetObjectFileName;'#013#010+
  '    Property ','RSTFileName : String Read GetRSTFileName;'#013#010+
  '    Property RSJFileName : String Read GetRSJFileName;'#013#010+
  '    Property FPCTarget : String Read FFPCTarget Write FFPCTarget;'#013#010+
  '    Property Extension : String Read FExtension Write FExtension;'#013#010+
  '    Pro','perty FileType : TFileType Read FFileType Write FFileType;'#013+
  #010+
  '    Property Directory : String Read FDirectory Write FDirectory;'#013#010+
  '    Property ResourceStrings : Boolean Read FResourceStrings Write FRe'+
  'sourceStrings;'#013#010+
  '    Property Install : Bool','ean Read FInstall Write FInstall;'#013#010+
  '    Property TargetSourceFileName: String Read FTargetSourceFileName;'#013+
  #010+
  '    Property ObjectPath : TConditionalStrings Read FObjectPath;'#013#010+
  '    Property UnitPath : TConditionalStrings Read FUnitPath;'#013#010+
  '    Prop','erty IncludePath : TConditionalStrings Read FIncludePath;'#013+
  #010+
  '    Property XML: string Read FXML Write SetXML;'#013#010+
  '    // Events.'#013#010+
  '    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FB'+
  'eforeCompile;'#013#010+
  '    Property AfterCompile : TN','otifyEvent Read FAfterCompile Write FA'+
  'fterCompile;'#013#010+
  '    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBefor'+
  'eClean;'#013#010+
  '    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterCl'+
  'ean;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TTargets }'#013#010+
  #013#010+
  '  TTa','rgets = Class(TNamedCollection)'#013#010+
  '  private'#013#010+
  '    function GetTargetItem(Index : Integer): TTarget;'#013#010+
  '    function GetTarget(const AName : String): TTarget;'#013#010+
  '    procedure SetTargetItem(Index : Integer; const AValue: TTarget);'#013+
  #010+
  '  Public'#013#010+
  '    F','unction AddFPDoc(Const AUnitName, AXMLName : String) : TTarget;'+
  'inline;'#013#010+
  '    Function AddUnit(Const AUnitName : String) : TTarget;inline;'#013#010+
  '    Function AddUnit(Const AUnitName : String;const OSes:TOSes) : TTar'+
  'get;inline;'#013#010+
  '{$ifdef cpu_only_ov','erloads}'#013#010+
  '    Function AddUnit(Const AUnitName : String;const CPUs:TCPUs) : TTar'+
  'get;inline;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  '    Function AddUnit(Const AUnitName : String;const CPUs:TCPUs;const O'+
  'Ses:TOSes) : TTarget;'#013#010+
  '    Function AddImplicitU','nit(Const AUnitName : String;InstallUnit:bo'+
  'olean=true) : TTarget;inline;'#013#010+
  '    Function AddImplicitUnit(Const AUnitName : String;const OSes:TOSes'+
  ';InstallUnit:boolean=true) : TTarget;inline;'#013#010+
  '    Function AddImplicitUnit(Const AUnitName : Str','ing;const CPUs:TCP'+
  'Us;InstallUnit:boolean=true) : TTarget;inline;'#013#010+
  '    Function AddImplicitUnit(Const AUnitName : String;const CPUs:TCPUs'+
  ';const OSes:TOSes;InstallUnit:boolean=true) : TTarget;'#013#010+
  '    Function AddProgram(Const AProgramName : Str','ing) : TTarget;inlin'+
  'e;'#013#010+
  '    Function AddProgram(Const AProgramName : String;const OSes:TOSes) '+
  ': TTarget;inline;'#013#010+
  '{$ifdef cpu_only_overloads}'#013#010+
  '    Function AddProgram(Const AProgramName : String;const CPUs:TCPUs) '+
  ': TTarget;inline;'#013#010+
  '{$endif c','pu_only_overloads}'#013#010+
  '    Function AddProgram(Const AProgramName : String;const CPUs:TCPUs;c'+
  'onst OSes:TOSes) : TTarget;'#013#010+
  '    Function AddExampleUnit(Const AUnitName : String) : TTarget;inline'+
  ';'#013#010+
  '    Function AddExampleUnit(Const AUnitName : St','ring;const OSes:TOSe'+
  's) : TTarget;inline;'#013#010+
  '{$ifdef cpu_only_overloads}'#013#010+
  '    Function AddExampleUnit(Const AUnitName : String;const CPUs:TCPUs)'+
  ' : TTarget;inline;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  '    Function AddExampleUnit(Const AUnitName : Stri','ng;const CPUs:TCPU'+
  's;const OSes:TOSes) : TTarget;'#013#010+
  '    Function AddExampleProgram(Const AProgramName : String) : TTarget;'+
  'inline;'#013#010+
  '    Function AddExampleProgram(Const AProgramName : String;const OSes:'+
  'TOSes) : TTarget;inline;'#013#010+
  '{$ifdef cpu_onl','y_overloads}'#013#010+
  '    Function AddExampleProgram(Const AProgramName : String;const CPUs:'+
  'TCPUs) : TTarget;inline;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  '    Function AddExampleProgram(Const AProgramName : String;const CPUs:'+
  'TCPUs;const OSes:TOSes) : TTarg','et;'#013#010+
  '    Property Targets[AName : String] : TTarget Read GetTarget; default'+
  ';'#013#010+
  '    Property TargetItems[Index : Integer] : TTarget Read GetTargetItem'+
  ' Write SetTargetItem;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TSource }'#013#010+
  #013#010+
  '  TSource = Class(TNamedItem)'#013#010+
  '  private'#013#010,
  '    FSourceType : TSourceType;'#013#010+
  '    FInstallSourcePath : string;'#013#010+
  '    function GetInstallSourcePath: string;'#013#010+
  '  Public'#013#010+
  '    Constructor Create(ACollection : TCollection); override;'#013#010+
  '    Destructor Destroy; override;'#013#010+
  '    Procedure GetInstal','lFiles(List : TStrings); virtual;'#013#010+
  '    property SourceType : TSourceType read FSourceType;'#013#010+
  '    property InstallSourcePath : string read GetInstallSourcePath;'#013+
  #010+
  '  end;'#013#010+
  #013#010+
  '  { TSources }'#013#010+
  #013#010+
  '  TSources = Class(TNamedCollection)'#013#010+
  '  private'#013#010+
  '   ',' function GetSourceItem(Index : Integer): TSource;'#013#010+
  '    procedure SetSourceItem(Index : Integer; const AValue: TSource);'#013+
  #010+
  '  public'#013#010+
  '    Function AddDoc(const AFiles : String) : TSource;'#013#010+
  '    Function AddDoc(const AFiles : String; AInstallSo','urcePath : Stri'+
  'ng) : TSource;'#013#010+
  '    Function AddSrc(const AFiles : String) : TSource;'#013#010+
  '    Function AddExample(const AFiles : String) : TSource;'#013#010+
  '    Function AddExample(const AFiles : String; AInstallSourcePath : St'+
  'ring) : TSource;'#013#010+
  '    Fun','ction AddTest(const AFiles : String) : TSource;'#013#010+
  '    procedure AddDocFiles(const AFileMask: string; Recursive: boolean '+
  '= False; AInstallSourcePath : String = '#039#039');'#013#010+
  '    procedure AddSrcFiles(const AFileMask: string; Recursive: boolean '+
  '= Fals','e);'#013#010+
  '    procedure AddExampleFiles(const AFileMask: string; Recursive: bool'+
  'ean = False; AInstallSourcePath : String = '#039#039');'#013#010+
  '    procedure AddTestFiles(const AFileMask: string; Recursive: boolean'+
  ' = False);'#013#010+
  '    Property SourceItems[Index : I','nteger] : TSource Read GetSourceIt'+
  'em Write SetSourceItem;default;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TPackage }'#013#010+
  #013#010+
  '  TPackage = Class(TNamedItem)'#013#010+
  '  private'#013#010+
  '    FAfterArchive: TNotifyEvent;'#013#010+
  '    FAfterArchiveProc: TNotifyProcEvent;'#013#010+
  '    FAfterClean: TNotifyEv','ent;'#013#010+
  '    FAfterCleanProc: TNotifyProcEvent;'#013#010+
  '    FAfterCompile: TNotifyEvent;'#013#010+
  '    FAfterCompileProc: TNotifyProcEvent;'#013#010+
  '    FAfterInstall: TNotifyEvent;'#013#010+
  '    FAfterInstallProc: TNotifyProcEvent;'#013#010+
  '    FAfterManifest: TNotifyEvent;'#013#010+
  '    FAf','terManifestProc: TNotifyProcEvent;'#013#010+
  '    FBeforeArchive: TNotifyEvent;'#013#010+
  '    FBeforeArchiveProc: TNotifyProcEvent;'#013#010+
  '    FBeforeClean: TNotifyEvent;'#013#010+
  '    FBeforeCleanProc: TNotifyProcEvent;'#013#010+
  '    FBeforeCompile: TNotifyEvent;'#013#010+
  '    FBeforeCompil','eProc: TNotifyProcEvent;'#013#010+
  '    FBeforeInstall: TNotifyEvent;'#013#010+
  '    FBeforeInstallProc: TNotifyProcEvent;'#013#010+
  '    FBeforeManifest: TNotifyEvent;'#013#010+
  '    FBeforeManifestProc: TNotifyProcEvent;'#013#010+
  '    FBuildMode: TBuildMode;'#013#010+
  '    FFlags: TStrings;'#013#010+
  '    ','FFPDocFormat: TFPDocFormats;'#013#010+
  '    FIsFPMakeAddIn: boolean;'#013#010+
  '    FSupportBuildModes: TBuildModes;'#013#010+
  '    FUnitPath,'#013#010+
  '    FObjectPath,'#013#010+
  '    FIncludePath,'#013#010+
  '    FSourcePath,'#013#010+
  '    FExamplePath,'#013#010+
  '    FTestPath,'#013#010+
  '    FCleanFiles   : TConditionalStri','ngs;'#013#010+
  '    FInstallFiles : TConditionalDestStrings;'#013#010+
  '    FDependencies : TDependencies;'#013#010+
  '    FCPUs: TCPUs;'#013#010+
  '    FOSes: TOSes;'#013#010+
  '    FTargetState: TTargetState;'#013#010+
  '    FTargets: TTargets;'#013#010+
  '    FSources: TSources;'#013#010+
  '    FDirectory: String;'#013#010+
  '    FO','ptions: TStrings;'#013#010+
  '    FFileName: String;'#013#010+
  '    FAuthor: String;'#013#010+
  '    FLicense: String;'#013#010+
  '    FHomepageURL: String;'#013#010+
  '    FDownloadURL: String;'#013#010+
  '    FVersion: TFPVersion;'#013#010+
  '    FEmail : String;'#013#010+
  '    FNeedLibC : Boolean;'#013#010+
  '    FCommands : TComman','ds;'#013#010+
  '    FDescriptionFile : String;'#013#010+
  '    FDescription : String;'#013#010+
  '    FInstalledChecksum : Cardinal;'#013#010+
  '    FUnitsOutputDir: String;'#013#010+
  '    FPackageUnitInstallDir: String;'#013#010+
  '    // Cached directory of installed packages'#013#010+
  '    FUnitDir : String;'#013#010+
  ' ','   // Used by buildunits'#013#010+
  '    FBUTargets: TTargets;'#013#010+
  '    FBUTarget: TTarget;'#013#010+
  '    // Used to identify if package is being processed by a thread'#013#010+
  '    FProcessing : boolean;'#013#010+
  '    // Dictionary'#013#010+
  '    FDictionary : TDictionary;'#013#010+
  '    // Is set wh','en all sourcefiles are found'#013#010+
  '    FAllFilesResolved: boolean;'#013#010+
  '    FPackageVariants: TFPList;'#013#010+
  '    Function GetDescription : string;'#013#010+
  '    function GetDictionary: TDictionary;'#013#010+
  '    Function GetFileName : string;'#013#010+
  '    function GetOptions: TSt','rings;'#013#010+
  '    Function GetVersion : string;'#013#010+
  '    procedure SetOptions(const AValue: TStrings);'#013#010+
  '    Procedure SetVersion(const V : string);'#013#010+
  '  Protected'#013#010+
  '    procedure SetName(const AValue: String);override;'#013#010+
  '    procedure SaveUnitConfigToStr','ingList(Const AStringList: TStrings'+
  ';ACPU:TCPU;AOS:TOS); virtual;'#013#010+
  '    property Dictionary: TDictionary read GetDictionary;'#013#010+
  '  Public'#013#010+
  '    constructor Create(ACollection: TCollection); override;'#013#010+
  '    destructor destroy; override;'#013#010+
  '    Functi','on HaveOptions : Boolean;'#013#010+
  '    Function  GetUnitsOutputDir(ACPU:TCPU; AOS : TOS):String;'#013#010+
  '    Function  GetUnitConfigOutputDir(ACPU:TCPU; AOS : TOS):String;'#013+
  #010+
  '    Procedure InheritPackageVariantsFromDependency(ADependencyPackage:'+
  ' TPackage);'#013,#010+
  '    Function  GetPackageVariantsByName(AName: string): TPackageVariant'+
  's;'#013#010+
  '    Procedure SetUnitsOutputDir(AValue: string);'#013#010+
  '    Function  GetPackageUnitInstallDir(ACPU:TCPU; AOS : TOS):String;'#013+
  #010+
  '    Procedure SetPackageUnitInstallDir(AValue',': string);'#013#010+
  '    Function  GetBinOutputDir(ACPU:TCPU; AOS : TOS) : String;'#013#010+
  '    Procedure GetCleanFiles(List : TStrings; ACPU:TCPU; AOS : TOS); vi'+
  'rtual;'#013#010+
  '    procedure GetInstallFiles(List: TStrings;Types : TTargetTypes;ACPU'+
  ':TCPU; AOS : TOS)','; virtual;'#013#010+
  '    procedure GetInstallSourceFiles(List: TStrings; SourceTypes : TSou'+
  'rceTypes; TargetTypes : TTargetTypes); virtual;'#013#010+
  '    Procedure GetArchiveFiles(List : TStrings; ACPU:TCPU; AOS : TOS); '+
  'virtual;'#013#010+
  '    Procedure GetArchiveSourc','eFiles(List : TStrings); virtual;'#013#010+
  '    Procedure GetManifest(Manifest : TStrings);'#013#010+
  '    Procedure AddPackageVariant(APackageVariant: TPackageVariants);'#013+
  #010+
  '    procedure ApplyPackageVariantToCompilerOptions(ACompilerOptions: t'+
  'strings);'#013#010+
  '    pr','ocedure SetDefaultPackageVariant;'#013#010+
  '    procedure LoadUnitConfigFromFile(Const AFileName: String);'#013#010+
  '    procedure SaveUnitConfigToFile(Const AFileName: String;ACPU:TCPU;A'+
  'OS:TOS);'#013#010+
  '    Property Version : String Read GetVersion Write SetVersio','n;'#013#010+
  '    Property FileName : String Read GetFileName Write FFileName;'#013#010+
  '    Property HomepageURL : String Read FHomepageURL Write FHomepageURL'+
  ';'#013#010+
  '    Property DownloadURL : String Read FDownloadURL Write FDownloadURL'+
  ';'#013#010+
  '    Property Email : Str','ing Read FEmail Write FEmail;'#013#010+
  '    Property Author : String Read FAuthor Write FAuthor;'#013#010+
  '    Property License : String Read FLicense Write FLicense;'#013#010+
  '    Property Directory : String Read FDirectory Write FDirectory;'#013#010+
  '    Property Descriptio','n : String Read GetDescription Write FDescrip'+
  'tion;'#013#010+
  '    Property DescriptionFile : String Read FDescriptionFile Write FDes'+
  'criptionFile;'#013#010+
  '    Property InstalledChecksum : Cardinal Read FInstalledChecksum Writ'+
  'e FInstalledChecksum;'#013#010+
  '    Proper','ty IsFPMakeAddIn: boolean read FIsFPMakeAddIn write FIsFPM'+
  'akeAddIn;'#013#010+
  '    Property SupportBuildModes: TBuildModes read FSupportBuildModes wr'+
  'ite FSupportBuildModes;'#013#010+
  '    Property BuildMode: TBuildMode read FBuildMode;'#013#010+
  '    Property Flags: TSt','rings read FFlags;'#013#010+
  '    // Compiler options.'#013#010+
  '    Property OSes : TOSes Read FOSes Write FOSes;'#013#010+
  '    Property CPUs : TCPUs Read FCPUs Write FCPUs;'#013#010+
  '    Property NeedLibC : Boolean Read FNeedLibC Write FNeedLibC;'#013#010+
  '    Property Options: TStri','ngs Read GetOptions Write SetOptions;'#013+
  #010+
  '    Property UnitPath : TConditionalStrings Read FUnitPath;'#013#010+
  '    Property ObjectPath : TConditionalStrings Read FObjectPath;'#013#010+
  '    Property IncludePath : TConditionalStrings Read FIncludePath;'#013#010+
  '    Prop','erty SourcePath : TConditionalStrings Read FSourcePath;'#013#010+
  '    Property ExamplePath : TConditionalStrings Read FExamplePath;'#013#010+
  '    Property TestPath : TConditionalStrings Read FTestPath;'#013#010+
  '    Property FPDocFormat: TFPDocFormats read FFPDocForm','at write FFPD'+
  'ocFormat;'#013#010+
  '    // Targets and dependencies'#013#010+
  '    Property InstallFiles : TConditionalDestStrings Read FInstallFiles'+
  ';'#013#010+
  '    Property CleanFiles : TConditionalStrings Read FCleanFiles;'#013#010+
  '    Property Dependencies : TDependencies Rea','d FDependencies;'#013#010+
  '    Property Commands : TCommands Read FCommands;'#013#010+
  '    Property State : TTargetState Read FTargetState;'#013#010+
  '    Property Targets : TTargets Read FTargets;'#013#010+
  '    Property Sources : TSources Read FSources;'#013#010+
  '    Property UnitDir ',': String Read FUnitDir Write FUnitDir;'#013#010+
  '    // events'#013#010+
  '    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FB'+
  'eforeCompile;'#013#010+
  '    Property BeforeCompileProc : TNotifyProcEvent Read FBeforeCompileP'+
  'roc write FBeforeCompileProc;'#013,#010+
  '    Property AfterCompile : TNotifyEvent Read FAfterCompile Write FAft'+
  'erCompile;'#013#010+
  '    Property AfterCompileProc : TNotifyProcEvent Read FAfterCompilePro'+
  'c Write FAfterCompileProc;'#013#010+
  '    Property BeforeInstall : TNotifyEvent Read FBeforeInsta','ll Write '+
  'FBeforeInstall;'#013#010+
  '    Property BeforeInstallProc : TNotifyProcEvent Read FBeforeInstallP'+
  'roc Write FBeforeInstallProc;'#013#010+
  '    Property AfterInstall : TNotifyEvent Read FAfterInstall Write FAft'+
  'erInstall;'#013#010+
  '    Property AfterInstallProc :',' TNotifyProcEvent Read FAfterInstallP'+
  'roc Write FAfterInstallProc;'#013#010+
  '    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBefor'+
  'eClean;'#013#010+
  '    Property BeforeCleanProc : TNotifyProcEvent Read FBeforeCleanProc '+
  'Write FBeforeCleanProc;'#013,#010+
  '    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterCl'+
  'ean;'#013#010+
  '    Property AfterCleanProc : TNotifyProcEvent Read FAfterCleanProc Wr'+
  'ite FAfterCleanProc;'#013#010+
  '    Property BeforeArchive : TNotifyEvent Read FBeforeArchive Write FB'+
  'e','foreArchive;'#013#010+
  '    Property BeforeArchiveProc : TNotifyProcEvent Read FBeforeArchiveP'+
  'roc Write FBeforeArchiveProc;'#013#010+
  '    Property AfterArchive : TNotifyEvent Read FAfterArchive Write FAft'+
  'erArchive;'#013#010+
  '    Property AfterArchiveProc : TNotifyProc','Event Read FAfterArchiveP'+
  'roc Write FAfterArchiveProc;'#013#010+
  '    Property BeforeManifest : TNotifyEvent Read FBeforeManifest Write '+
  'FBeforeManifest;'#013#010+
  '    Property BeforeManifestProc : TNotifyProcEvent Read FBeforeManifes'+
  'tProc Write FBeforeManifest','Proc;'#013#010+
  '    Property AfterManifest : TNotifyEvent Read FAfterManifest Write FA'+
  'fterManifest;'#013#010+
  '    Property AfterManifestProc : TNotifyProcEvent Read FAfterManifestP'+
  'roc Write FAfterManifestProc;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TPackages }'#013#010+
  #013#010+
  '  TPackages = Clas','s(TNamedCollection)'#013#010+
  '  private'#013#010+
  '    function GetPackage(const AName : String): TPackage;'#013#010+
  '    function GetPackageItem(AIndex : Integer): TPackage;'#013#010+
  '    procedure SetPackageItem(AIndex : Integer; const AValue: TPackage)'+
  ';'#013#010+
  '  Public'#013#010+
  '    Funct','ion AddPackage(Const AName : String) : TPackage;'#013#010+
  '    Property Packages[AName : String] : TPackage Read GetPackage ; Def'+
  'ault;'#013#010+
  '    Property PackageItems[AIndex : Integer] : TPackage Read GetPackage'+
  'Item Write SetPackageItem;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { T','CustomDefaults }'#013#010+
  #013#010+
  '  TCustomDefaults = Class(TPersistent)'#013#010+
  '  Private'#013#010+
  '    FArchive: String;'#013#010+
  '    FBuildMode: TBuildMode;'#013#010+
  '    FCompiler: String;'#013#010+
  '    FCopy: String;'#013#010+
  '    FFPDocOutputDir: String;'#013#010+
  '    FIgnoreInvalidOptions: Boolean;'#013#010+
  '    FI','nstallExamples: Boolean;'#013#010+
  '    FMkDir: String;'#013#010+
  '    FMove: String;'#013#010+
  '    FOptions: TStrings;'#013#010+
  '    FCPU: TCPU;'#013#010+
  '    FOS: TOS;'#013#010+
  '    FMode : TCompilerMode;'#013#010+
  '    FCompilerVersion : String;'#013#010+
  '    FPrefix: String;'#013#010+
  '    FLocalUnitDir,'#013#010+
  '    FGlobalUni','tDir,'#013#010+
  '    FBaseInstallDir,'#013#010+
  '    FUnitInstallDir,'#013#010+
  '    FBinInstallDir,'#013#010+
  '    FDocInstallDir,'#013#010+
  '    FExamplesInstallDir : String;'#013#010+
  '    FSkipCrossPrograms: boolean;'#013#010+
  '    FThreadsAmount: integer;'#013#010+
  '    FRemoveTree: String;'#013#010+
  '    FRemoveDir: String',';'#013#010+
  '    FRemove: String;'#013#010+
  '    FTarget: String;'#013#010+
  '    FUnixPaths: Boolean;'#013#010+
  '    FNoFPCCfg: Boolean;'#013#010+
  '    FUseEnvironment: Boolean;'#013#010+
  '    FZipPrefix: String;'#013#010+
  '    function GetBuildCPU: TCpu;'#013#010+
  '    function GetBuildOS: TOS;'#013#010+
  '    function GetBuildS','tring: String;'#013#010+
  '    function GetFPDocOutputDir: String;'#013#010+
  '    function GetLocalUnitDir: String;'#013#010+
  '    function GetGlobalUnitDir: String;'#013#010+
  '    function GetBaseInstallDir: String;'#013#010+
  '    function GetBinInstallDir: String;'#013#010+
  '    function GetCompile','r: String;'#013#010+
  '    function GetDocInstallDir: String;'#013#010+
  '    function GetExamplesInstallDir: String;'#013#010+
  '    function GetOptions: TStrings;'#013#010+
  '    function GetUnitInstallDir: String;'#013#010+
  '    procedure SetLocalUnitDir(const AValue: String);'#013#010+
  '    procedur','e SetGlobalUnitDir(const AValue: String);'#013#010+
  '    procedure IntSetBaseInstallDir(const AValue: String);'#013#010+
  '    procedure SetBaseInstallDir(const AValue: String);'#013#010+
  '    procedure SetCPU(const AValue: TCPU);'#013#010+
  '    procedure SetOptions(const AValue: ','TStrings);'#013#010+
  '    procedure SetOS(const AValue: TOS);'#013#010+
  '    procedure SetPrefix(const AValue: String);'#013#010+
  '    procedure SetTarget(const AValue: String);'#013#010+
  '    procedure SetUnitInstallDir(const AValue: String);'#013#010+
  '    procedure SetZipPrefix(AValue: ','String);'#013#010+
  '  Protected'#013#010+
  '    procedure RecalcTarget;'#013#010+
  '    Function CmdLineOptions : String;'#013#010+
  '  Public'#013#010+
  '    Constructor Create;'#013#010+
  '    Procedure InitDefaults;'#013#010+
  '    Function HaveOptions: Boolean;'#013#010+
  '    function IsBuildDifferentFromTarget: boolean;',#013#010+
  '    procedure CompilerDefaults; virtual;'#013#010+
  '    Procedure LocalInit(Const AFileName : String);'#013#010+
  '    Procedure LoadFromFile(Const AFileName : String);'#013#010+
  '    Procedure SaveToFile(Const AFileName : String);'#013#010+
  '    procedure SaveToStream(S : TStre','am);virtual;'#013#010+
  '    procedure LoadFromStream(S : TStream);virtual;'#013#010+
  '    // Compile Information'#013#010+
  '    Property Target : String Read FTarget Write SetTarget;'#013#010+
  '    Property OS : TOS Read FOS Write SetOS;'#013#010+
  '    Property CPU : TCPU Read FCPU Write S','etCPU;'#013#010+
  '    Property BuildString : String read GetBuildString;'#013#010+
  '    Property BuildOS : TOS read GetBuildOS;'#013#010+
  '    Property BuildCPU : TCpu read GetBuildCPU;'#013#010+
  '    Property Mode : TCompilerMode Read FMode Write FMode;'#013#010+
  '    Property UnixPaths :',' Boolean Read FUnixPaths Write FUnixPaths;'#013+
  #010+
  '    Property Options : TStrings Read GetOptions Write SetOptions;    /'+
  '/ Default compiler options.'#013#010+
  '    Property NoFPCCfg : Boolean Read FNoFPCCfg Write FNoFPCCfg;'#013#010+
  '    // When ThreadsAmount is spe','cified, #threadsamount# worker-threa'+
  'ds are'#013#010+
  '    // created. When such a worker-thread is ready all worker-threads '+
  'are evaluated'#013#010+
  '    // to see if there are idle threads (there is always at least one '+
  'such thread.)'#013#010+
  '    // To each idle thread',' a package is assigned which has to be com'+
  'piled for the'#013#010+
  '    // current target and for which all dependencies are compiled earl'+
  'ier.'#013#010+
  '    // When no package is available the thread remains idle until anot'+
  'her thread'#013#010+
  '    // has finished it'#039's ','task. Compilation stops when all packag'+
  'es are compiled'#013#010+
  '    // or when an error occures.'#013#010+
  '    //'#013#010+
  '    // When ThreadsAmount is not specified (-1), all packages are comp'+
  'iled on by one.'#013#010+
  '    // Dependencies are compiled recursively. When a pa','ckage is alre'+
  'ady compiled'#013#010+
  '    // (because some other package was depending on it) the package is'+
  ' skipped.'#013#010+
  '    // When the last package in the list is compiled, the compilation '+
  'stops.'#013#010+
  '    Property ThreadsAmount : integer Read FThreadsAmou','nt Write FThre'+
  'adsAmount;'#013#010+
  '    // paths etc.'#013#010+
  '    Property LocalUnitDir : String Read GetLocalUnitDir Write SetLocal'+
  'UnitDir;'#013#010+
  '    Property GlobalUnitDir : String Read GetGlobalUnitDir Write SetGlo'+
  'balUnitDir;'#013#010+
  '    Property Prefix : String Rea','d FPrefix Write SetPrefix;'#013#010+
  '    Property ZipPrefix : String Read FZipPrefix Write SetZipPrefix;'#013+
  #010+
  '    Property BaseInstallDir : String Read GetBaseInstallDir Write SetB'+
  'aseInstallDir;'#013#010+
  '    Property UnitInstallDir : String Read GetUnitInstallD','ir Write Se'+
  'tUnitInstallDir;'#013#010+
  '    Property BinInstallDir : String Read GetBinInstallDir Write FBinIn'+
  'stallDir;'#013#010+
  '    Property DocInstallDir : String Read GetDocInstallDir Write FDocIn'+
  'stallDir;'#013#010+
  '    Property ExamplesInstallDir : String Read Get','ExamplesInstallDir '+
  'Write FExamplesInstallDir;'#013#010+
  '    Property FPDocOutputDir : String Read GetFPDocOutputDir Write FFPD'+
  'ocOutputDir;'#013#010+
  '    // Command tools. If not set, internal commands  will be used.'#013+
  #010+
  '    Property Compiler : String Read GetCo','mpiler Write FCompiler; // '+
  'Compiler. Defaults to fpc'#013#010+
  '    Property Copy : String Read FCopy Write FCopy;             // copy'+
  ' $(FILES) to $(DEST)'#013#010+
  '    Property Move : String Read FMove Write FMove;             // Move'+
  ' $(FILES) to $(DEST)'#013#010+
  '  ','  Property Remove : String Read FRemove Write FRemove;       // De'+
  'lete $(FILES)'#013#010+
  '    Property RemoveDir : String Read FRemoveDir Write FRemoveDir;     '+
  '  // Delete $(FILES)'#013#010+
  '    Property RemoveTree : String Read FRemoveTree Write FRemoveTree',';'+
  '       // removes $(DIRECTORY)'#013#010+
  '    Property MkDir : String Read FMkDir write FMkDir;          // Make'+
  ' $(DIRECTORY)'#013#010+
  '    Property Archive : String Read FArchive Write FArchive;    // zip '+
  '$(ARCHIVE) $(FILESORDIRS)'#013#010+
  '    // Misc'#013#010+
  '    Property',' UseEnvironment : Boolean read FUseEnvironment write FUs'+
  'eEnvironment;'#013#010+
  '    Property IgnoreInvalidOptions: Boolean read FIgnoreInvalidOptions '+
  'write FIgnoreInvalidOptions;'#013#010+
  '    Property BuildMode: TBuildMode read FBuildMode write FBuildMode;'#013,
  #010+
  '    // Installation optioms'#013#010+
  '    Property InstallExamples: Boolean read FInstallExamples write FIns'+
  'tallExamples;'#013#010+
  '    Property SkipCrossPrograms: boolean read FSkipCrossPrograms write '+
  'FSkipCrossPrograms;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TBasicDefaults }'#013#010+
  #013#010,
  '  TBasicDefaults = Class(TCustomDefaults)'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TFPCDefaults }'#013#010+
  #013#010+
  '  TFPCDefaults = Class(TCustomDefaults)'#013#010+
  '  public'#013#010+
  '    procedure CompilerDefaults; override;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TBuildEngine }'#013#010+
  #013#010+
  '  TBuildEngine = Class(TComponent)'#013#010+
  '  p','rivate'#013#010+
  '    // general variables'#013#010+
  '    FCompiler : String;'#013#010+
  '    FStartDir : String;'#013#010+
  '    FForceCompile : Boolean;'#013#010+
  '    FListMode : Boolean;'#013#010+
  '    FVerbose : boolean;'#013#010+
  '    FProgressMax : integer;'#013#010+
  '    FProgressCount : integer;'#013#010+
  '{$ifdef HAS_UNI','T_ZIPPER}'#013#010+
  '    FZipFile: TZipper;'#013#010+
  '{$endif HAS_UNIT_ZIPPER}'#013#010+
  '    FExternalPackages : TPackages;'#013#010+
  '    // Events'#013#010+
  '    FOnLog: TLogEvent;'#013#010+
  '    FAfterArchive: TNotifyEvent;'#013#010+
  '    FAfterClean: TNotifyEvent;'#013#010+
  '    FAfterCompile: TNotifyEvent;'#013#010+
  '    ','FAfterInstall: TNotifyEvent;'#013#010+
  '    FAfterManifest: TNotifyEvent;'#013#010+
  '    FBeforeArchive: TNotifyEvent;'#013#010+
  '    FBeforeClean: TNotifyEvent;'#013#010+
  '    FBeforeCompile: TNotifyEvent;'#013#010+
  '    FBeforeInstall: TNotifyEvent;'#013#010+
  '    FBeforeManifest: TNotifyEvent;'#013#010+
  #013,#010+
  '    FCachedlibcPath: string;'#013#010+
  '    FGeneralCriticalSection: TRTLCriticalSection;'#013#010+
  '{$ifdef HAS_UNIT_ZIPPER}'#013#010+
  '    FZipper: TZipper;'#013#010+
  '{$endif HAS_UNIT_ZIPPER}'#013#010+
  '{$ifdef HAS_TAR_SUPPORT}'#013#010+
  '    FTarWriter: TTarWriter;'#013#010+
  '{$endif HAS_TAR_SUPPORT}'#013#010+
  '  ','Protected'#013#010+
  '    Procedure Error(const Msg : String);'#013#010+
  '    Procedure Error(const Fmt : String; const Args : Array of const);'#013+
  #010+
  '    // Internal copy/delete/move/archive/mkdir files'#013#010+
  '    Function  SysDirectoryExists(const ADir:string):Boolean;'#013#010,
  '    Function  SysFileExists(const AFileName:string):Boolean;'#013#010+
  '    Procedure SysCopyFile(Const Src,Dest : String); virtual;'#013#010+
  '    Procedure SysMoveFile(Const Src,Dest : String); virtual;'#013#010+
  '    Procedure SysDeleteFile(Const AFileName : String);',' virtual;'#013#010+
  '    Procedure SysDeleteDirectory(Const ADirectoryName : String); virtu'+
  'al;'#013#010+
  '    Procedure SysDeleteTree(Const ADirectoryName : String); virtual;'#013+
  #010+
  '    Procedure SysArchiveFiles(List : TStrings; Const AFileName : Strin'+
  'g); virtual;'#013#010,
  '    procedure LogIndent;'#013#010+
  '    procedure LogUnIndent;'#013#010+
  '    Procedure EnterDir(ADir : String);'#013#010+
  '    Function GetCompiler : String;'#013#010+
  '    Function InstallPackageFiles(APAckage : TPackage; tt : TTargetType'+
  's; Const Dest : String):Boolean;'#013#010+
  '    Pr','ocedure InstallUnitConfigFile(APAckage : TPackage; Const Dest '+
  ': String);'#013#010+
  '    Function InstallPackageSourceFiles(APAckage : TPackage; stt : TSou'+
  'rceTypes; ttt : TTargetTypes; Const Dest : String):Boolean;'#013#010+
  '    Function FileNewer(const Src,De','st : String) : Boolean;'#013#010+
  '    Procedure LogSearchPath(APackage: TPackage;const ASearchPathName:s'+
  'tring;Path:TConditionalStrings; ACPU:TCPU;AOS:TOS);'#013#010+
  '    Function FindFileInPath(APackage: TPackage; Path:TConditionalStrin'+
  'gs; AFileName:String; ','var FoundPath:String;ACPU:TCPU;AOS:TOS):Boolea'+
  'n;'#013#010+
  #013#010+
  '    procedure GetDirectoriesFromFilelist(const AFileList, ADirectoryLi'+
  'st: TStringList);'#013#010+
  '    procedure AddPackageMacrosToDictionary(const APackage: TPackage; A'+
  'Dictionary: TDictionary);'#013#010+
  '  ','  //package commands'#013#010+
  '    function  GetUnitDir(APackage:TPackage):String;'#013#010+
  '    procedure AddDependencyPaths(L: TStrings; DependencyType: TDepende'+
  'ncyType; ATarget: TTarget);'#013#010+
  '    procedure AddDependencyUnitPaths(L:TStrings;APackage: TPackage',');'+
  #013#010+
  '  Public'#013#010+
  '    Constructor Create(AOwner : TComponent); override;'#013#010+
  '    destructor Destroy;override;'#013#010+
  '    function AddPathPrefix(APackage: TPackage; APath: string): string;'+
  #013#010+
  #013#010+
  '    property Verbose : boolean read FVerbose write FVerbose;'#013#010,
  '    Procedure ResolveFileNames(APackage : TPackage; ACPU:TCPU;AOS:TOS;'+
  'DoChangeDir:boolean=true; WarnIfNotFound:boolean=true);'#013#010+
  #013#010+
  '    // Public Copy/delete/Move/Archive/Mkdir Commands.'#013#010+
  '    Procedure ExecuteCommand(const Cmd,Args : String; c','onst Env: TSt'+
  'rings = nil; IgnoreError : Boolean = False); virtual;'#013#010+
  '    Procedure CmdCopyFiles(List : TStrings; Const DestDir : String);'#013+
  #010+
  '    Procedure CmdCreateDir(const DestDir : String);'#013#010+
  '    Procedure CmdMoveFiles(List : TStrings; Const',' DestDir : String);'+
  #013#010+
  '    Procedure CmdDeleteFiles(List : TStrings);'#013#010+
  '    Procedure CmdArchiveFiles(List : TStrings; Const ArchiveFile : Str'+
  'ing);'#013#010+
  '    Procedure CmdRenameFile(SourceName, DestName : String);'#013#010+
  '    Procedure CmdRemoveDirs(List:',' TStrings);'#013#010+
  '    Procedure CmdRemoveTrees(List: TStrings);'#013#010+
  '    Procedure ExecuteCommands(Commands : TCommands; At : TCommandAt; A'+
  'Package: TPackage = nil);'#013#010+
  '    // Dependency commands'#013#010+
  '    Function  DependencyOK(ADependency : TDependency) :',' Boolean;'#013+
  #010+
  '    // Target commands'#013#010+
  '    Function  GetCompilerCommand(APackage : TPackage; ATarget : TTarge'+
  't; Env: TStrings) : String;'#013#010+
  '    Function  TargetOK(ATarget : TTarget; ACPU: TCPU; AOS: TOS) : Bool'+
  'ean;'#013#010+
  '    Function  TargetInstallOK(','ATarget : TTarget;ACPU:TCPU; AOS : TOS'+
  ') : Boolean;'#013#010+
  '    Function  NeedsCompile(APackage:TPackage; ATarget : TTarget) : Boo'+
  'lean;'#013#010+
  '    Procedure Compile(APackage:TPackage; ATarget : TTarget);  virtual;'+
  #013#010+
  '    Procedure MaybeCompile(APackage:TPa','ckage; ATarget: TTarget);'#013+
  #010+
  '    Procedure CompileDependencies(APackage:TPackage; ATarget: TTarget)'+
  ';'#013#010+
  '    // Package commands'#013#010+
  '{    Function  GetPackageDir(APackage : TPackage; AbsolutePath : Boole'+
  'an = False) : String;'#013#010+
  '    Function  GetUnits','OutputDir(APackage : TPackage; AbsolutePath : '+
  'Boolean = False) : String;'#013#010+
  '    Function  GetBinOutputDir(APackage : TPackage; AbsolutePath : Bool'+
  'ean = False) : String;  }'#013#010+
  '    Function  PackageOK(APackage : TPackage) : Boolean; virtual;'#013#010+
  '   ',' Procedure DoBeforeCompile(APackage : TPackage);virtual;'#013#010+
  '    Procedure DoAfterCompile(APackage : TPackage);virtual;'#013#010+
  '    Procedure DoBeforeInstall(APackage : TPackage);virtual;'#013#010+
  '    Procedure DoAfterInstall(APackage : TPackage);virtual;'#013#010+
  ' ','   Procedure DoBeforeArchive(APackage : TPackage);virtual;'#013#010+
  '    Procedure DoAfterArchive(APackage : TPackage);virtual;'#013#010+
  '    Procedure DoBeforeClean(APackage : TPackage);virtual;'#013#010+
  '    Procedure DoAfterClean(APackage : TPackage);virtual;'#013#010+
  '   ',' Function  NeedsCompile(APackage : TPackage) : Boolean; virtual;'#013+
  #010+
  '    Procedure Compile(APackage : TPackage);'#013#010+
  '    Procedure MaybeCompile(APackage:TPackage);'#013#010+
  '    Function ReadyToCompile(APackage:TPackage) : Boolean;'#013#010+
  '    Procedure Install(','APackage : TPackage);'#013#010+
  '    Procedure Archive(APackage : TPackage);'#013#010+
  '    Procedure Manifest(APackage : TPackage);'#013#010+
  '    Procedure Clean(APackage : TPackage; AllTargets: boolean);'#013#010+
  '    Procedure Clean(APackage : TPackage; ACPU:TCPU; AOS : TOS)',';'#013#010+
  '    Procedure CompileDependencies(APackage : TPackage);'#013#010+
  '    function CheckDependencies(APackage : TPackage): TCheckDependencyR'+
  'esult;'#013#010+
  '    Function  CheckExternalPackage(Const APackageName : String):TPacka'+
  'ge;'#013#010+
  '    procedure CreateOutputD','ir(APackage: TPackage);'#013#010+
  '    // Packages commands'#013#010+
  '    Procedure Compile(Packages : TPackages);'#013#010+
  '    Procedure Install(Packages : TPackages);'#013#010+
  '    Procedure ZipInstall(Packages : TPackages);'#013#010+
  '    Procedure Archive(Packages : TPackages);'#013#010+
  '  ','  procedure Manifest(Packages: TPackages);'#013#010+
  '    Procedure Clean(Packages : TPackages; AllTargets: boolean);'#013#010+
  #013#010+
  '    Procedure Log(Level : TVerboseLevel; Msg : String);'#013#010+
  '    Procedure Log(Level : TVerboseLevel; Fmt : String; const Args : Ar'+
  'ra','y Of Const);'#013#010+
  #013#010+
  '    Property ListMode : Boolean Read FListMode Write FListMode;'#013#010+
  '    Property ForceCompile : Boolean Read FForceCompile Write FForceCom'+
  'pile;'#013#010+
  '    Property ExternalPackages: TPackages Read FExternalPackages;'#013#010+
  '    Property Sta','rtDir: String Read FStartDir;'#013#010+
  '    // Events'#013#010+
  '    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FB'+
  'eforeCompile;'#013#010+
  '    Property AfterCompile : TNotifyEvent Read FAfterCompile Write FAft'+
  'erCompile;'#013#010+
  '    Property BeforeInstall ',': TNotifyEvent Read FBeforeInstall Write '+
  'FBeforeInstall;'#013#010+
  '    Property AfterInstall : TNotifyEvent Read FAfterInstall Write FAft'+
  'erInstall;'#013#010+
  '    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBefor'+
  'eClean;'#013#010+
  '    Property AfterCle','an : TNotifyEvent Read FAfterClean Write FAfter'+
  'Clean;'#013#010+
  '    Property BeforeArchive : TNotifyEvent Read FBeforeArchive Write FB'+
  'eforeArchive;'#013#010+
  '    Property AfterArchive : TNotifyEvent Read FAfterArchive Write FAft'+
  'erArchive;'#013#010+
  '    Property Befor','eManifest : TNotifyEvent Read FBeforeManifest Writ'+
  'e FBeforeManifest;'#013#010+
  '    Property AfterManifest : TNotifyEvent Read FAfterManifest Write FA'+
  'fterManifest;'#013#010+
  '    Property OnLog : TLogEvent Read FOnLog Write FOnlog;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TCustomInstal','ler }'#013#010+
  #013#010+
  '  TCustomInstaller = Class(TComponent)'#013#010+
  '  private'#013#010+
  '    FBuildEngine: TBuildEngine;'#013#010+
  '    FPackages: TPackages;'#013#010+
  '    FRunMode: TRunMode;'#013#010+
  '    FListMode : Boolean;'#013#010+
  '    FLogLevels : TVerboseLevels;'#013#010+
  '    FFPMakeOptionsString: string;'#013#010,
  '    FPackageVariantSettings: TStrings;'#013#010+
  '    FPackageVariants: TFPList;'#013#010+
  '  Protected'#013#010+
  '    Procedure Log(Level : TVerboseLevel; Const Msg : String);'#013#010+
  '    Procedure CreatePackages; virtual;'#013#010+
  '    Procedure FreePackages; virtual;'#013#010+
  '    function G','etPackages: TPackages; virtual;'#013#010+
  '    Procedure CheckPackages; virtual;'#013#010+
  '    Procedure CreateBuildEngine; virtual;'#013#010+
  '    Procedure Error(const Msg : String);'#013#010+
  '    Procedure Error(const Fmt : String; Args : Array of const);'#013#010+
  '    Procedure Anal','yzeOptions;'#013#010+
  '    Procedure Usage(const FMT : String; Args : Array of const);'#013#010+
  '    Procedure Compile(Force : Boolean); virtual;'#013#010+
  '    Procedure Clean(AllTargets: boolean); virtual;'#013#010+
  '    Procedure Install; virtual;'#013#010+
  '    Procedure ZipInstall; v','irtual;'#013#010+
  '    Procedure Archive; virtual;'#013#010+
  '    Procedure Manifest; virtual;'#013#010+
  '  Public'#013#010+
  '    Constructor Create(AOwner : TComponent); virtual;'#013#010+
  '    Destructor destroy; override;'#013#010+
  '    Function AddPackage(Const AName : String) : TPackage;'#013#010+
  '    F','unction  AddPackageVariant(AName: string; AIsInheritable: boole'+
  'an): TPackageVariants;'#013#010+
  '    Function Run : Boolean;'#013#010+
  '    Property FPMakeOptionsString: string read FFPMakeOptionsString;'#013+
  #010+
  '    Property BuildEngine : TBuildEngine Read FBuildEngi','ne;'#013#010+
  '    //files in package'#013#010+
  '    Property Packages : TPackages Read GetPackages;'#013#010+
  '    Property RunMode : TRunMode Read FRunMode;'#013#010+
  '    Property ListMode : Boolean Read FListMode;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TFPCInstaller }'#013#010+
  '  TFPCInstaller = class(TCusto','mInstaller)'#013#010+
  '  public'#013#010+
  '    Constructor Create(AOwner : TComponent); override;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TBasicInstaller }'#013#010+
  '  TBasicInstaller = class(TCustomInstaller)'#013#010+
  '    Constructor Create(AOwner : TComponent); override;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TValueItem }'#013,#010+
  #013#010+
  '  TValueItem = Class(TObject)'#013#010+
  '    FValue : String;'#013#010+
  '    Constructor Create(AValue : String);'#013#010+
  '  end;'#013#010+
  #013#010+
  '  { TFunctionItem }'#013#010+
  #013#010+
  '  TFunctionItem = Class(TObject)'#013#010+
  '    FFunc : TReplaceFunction;'#013#010+
  '    Constructor Create(AFunc : TReplaceFunct','ion);'#013#010+
  '  end;'#013#010+
  #013#010+
  '{$ifndef NO_THREADING}'#013#010+
  #013#010+
  '  { TCompileWorkerThread }'#013#010+
  #013#010+
  '  TCompileWorkerThread = class(TThread)'#013#010+
  '  private'#013#010+
  '    FBuildEngine: TBuildEngine;'#013#010+
  '    FCompilationOK: boolean;'#013#010+
  '    FDone: boolean;'#013#010+
  '    FErrorMessage: string;'#013#010+
  '   ',' FNotifyMainThreadEvent: PRTLEvent;'#013#010+
  '    FNotifyStartTask: PRTLEvent;'#013#010+
  '    FPackage: TPackage;'#013#010+
  '  protected'#013#010+
  '    procedure execute; override;'#013#010+
  '    property Done: boolean read FDone;'#013#010+
  '    property APackage: TPackage read FPackage write FPack','age;'#013#010+
  '    property CompilationOK: boolean read FCompilationOK;'#013#010+
  '    property NotifyStartTask: PRTLEvent read FNotifyStartTask;'#013#010+
  '    property ErrorMessage: string read FErrorMessage;'#013#010+
  '  public'#013#010+
  '    constructor Create(ABuildEngine: TBuildEngi','ne; NotifyMainThreadE'+
  'vent: PRTLEvent); virtual;'#013#010+
  '    destructor Destroy; override;'#013#010+
  '  end;'#013#010+
  #013#010+
  '{$endif NO_THREADING}'#013#010+
  #013#010+
  '  ECollectionError = Class(Exception);'#013#010+
  '  EDictionaryError = Class(Exception);'#013#010+
  '  EInstallerError = Class(Exception);'#013#010+
  #013#010,
  '  TInstallerClass = Class of TCustomInstaller;'#013#010+
  '  TDictionaryClass = Class of TDictionary;'#013#010+
  '  TPackageDictionaryClass = Class of TPackageDictionary;'#013#010+
  #013#010+
  'Type'#013#010+
  '  TArchiveEvent = Procedure (Const AFileName : String; List : TStrings'+
  ') of Object;',#013#010+
  '  TArchiveProc = Procedure (Const AFileName : String; List : TStrings)'+
  ';'#013#010+
  #013#010+
  'Var'#013#010+
  '  DictionaryClass : TDictionaryClass = TDictionary;'#013#010+
  '  PackageDictionaryClass : TPackageDictionaryClass = TPackageDictionar'+
  'y;'#013#010+
  '  OnArchiveFiles : TArchiveEven','t = Nil;'#013#010+
  '  ArchiveFilesProc : TArchiveProc = Nil;'#013#010+
  #013#010+
  '  Defaults : TCustomDefaults; // Set by installer.'#013#010+
  '  GlobalDictionary : TDictionary;'#013#010+
  #013#010+
  #013#010+
  'Function CurrentOS : String;'#013#010+
  'Function CurrentCPU : String;'#013#010+
  #013#010+
  'Function Installer(InstallerClas','s: TInstallerClass) : TCustomInstall'+
  'er; overload;'#013#010+
  'Function Installer : TCustomInstaller; overload;'#013#010+
  #013#010+
  'Function OSToString(OS: TOS) : String;'#013#010+
  'Function OSesToString(OSes: TOSes) : String;'#013#010+
  'Function CPUToString(CPU: TCPU) : String;'#013#010+
  'Function',' CPUSToString(CPUS: TCPUS) : String;'#013#010+
  'Function StringToOS(const S : String) : TOS;'#013#010+
  'function IsDifferentFromBuild(ACpu: TCPU; AOs: TOs): boolean;'#013#010+
  '//Function StringToOSes(const S : String) : TOSes;'#013#010+
  'Function StringToCPU(const S : String) : ','TCPU;'#013#010+
  'Function StringToCPUS(const S : String) : TCPUS;'#013#010+
  'Function ModeToString(Mode: TCompilerMode) : String;'#013#010+
  'Function StringToMode(const S : String) : TCompilerMode;'#013#010+
  'Function MakeTargetString(CPU : TCPU;OS: TOS) : String;'#013#010+
  'Procedure Stri','ngToCPUOS(const S : String; Var CPU : TCPU; Var OS: TO'+
  'S);'#013#010+
  'Function FixPath (const APath : String) : String; inline; deprecated '#039+
  'Use the overload with AIsDir instead'#039';'#013#010+
  'Function FixPath (const APath : String; AIsDir : Boolean) : String;'#013+
  #010+
  'Fu','nction IsRelativePath(const APath : String) : boolean;'#013#010+
  'Procedure ChangeDir(const APath : String);'#013#010+
  'Procedure SplitCommand(Const Cmd : String; Var Exe,Options : String);'#013+
  #010+
  'Procedure AddCustomFpmakeCommandlineOption(const ACommandLineOption, ',
  'HelpMessage : string);'#013#010+
  'Function GetCustomFpmakeCommandlineOptionValue(const ACommandLineOptio'+
  'n : string) : string;'#013#010+
  'Function AddProgramExtension(const ExecutableName: string; AOS : TOS) '+
  ': string;'#013#010+
  'Function GetImportLibraryFilename(const Un','itName: string; AOS : TOS)'+
  ' : string;'#013#010+
  #013#010+
  'procedure SearchFiles(const AFileName: string; Recursive: boolean; var'+
  ' List: TStrings);'#013#010+
  'function GetDefaultLibGCCDir(CPU : TCPU;OS: TOS; var ErrorMessage: str'+
  'ing): string;'#013#010+
  #013#010+
  'Implementation'#013#010+
  #013#010+
  'uses t','ypinfo, rtlconsts;'#013#010+
  #013#010+
  'type'#013#010+
  '  TUnsortedDuplicatesStringList = class(TStringList)'#013#010+
  '  public'#013#010+
  '    function Add(const S: string): Integer; override;'#013#010+
  '  end;'#013#010+
  #013#010+
  'var'#013#010+
  '  CustomFpmakeCommandlineOptions: TStrings;'#013#010+
  '  CustomFpMakeCommandlineValues: ','TStrings;'#013#010+
  #013#010+
  '{$ifdef NO_THREADING}'#013#010+
  'var'#013#010+
  '{$else NO_THREADING}'#013#010+
  'threadvar'#013#010+
  '{$endif NO_THREADING}'#013#010+
  '  GPathPrefix : string;'#013#010+
  '  GLogPrefix  : string;'#013#010+
  #013#010+
  'ResourceString'#013#010+
  '  SErrInvalidCPU        = '#039'Invalid CPU name "%s"'#039';'#013#010+
  '  SErrInvalidOS        ',' = '#039'Invalid OS name "%s"'#039';'#013#010+
  '  SErrInvalidMode       = '#039'Invalid compiler mode "%s"'#039';'#013#010+
  '  SErrInvalidTarget     = '#039'Invalid compiler target "%s"'#039';'#013#010+
  '  SErrNameExists        = '#039'Name "%s" already exists in the collect'+
  'ion.'#039';'#013#010+
  '  SErrNoSuchName      ','  = '#039'Could not find item with name "%s" in'+
  ' the collection.'#039';'#013#010+
  '  SErrInValidArgument   = '#039'Invalid command-line argument at positi'+
  'on %d: %s'#039';'#013#010+
  '  SErrNeedArgument      = '#039'Option at position %d (%s) needs an arg'+
  'ument'#039';'#013#010+
  '  SErrNoPackagesDefined ','= '#039'No action possible: No packages were '+
  'defined.'#039';'#013#010+
  '  SErrInstaller         = '#039'The installer encountered the following'+
  ' error:'#039';'#013#010+
  '  SErrDepUnknownTarget  = '#039'Unknown target for unit "%s" in depende'+
  'ncies for %s in package %s'#039';'#013#010+
  '  SErrExternalC','ommandFailed = '#039'External command "%s" failed with'+
  ' exit code %d. Console output:'#039'+LineEnding+'#039'%s'#039';'#013#010+
  '  SErrExtCommandNotFound= '#039'External command "%s" not found'#039';'#013+
  #010+
  '  SErrCreatingDirectory = '#039'Failed to create directory "%s"'#039';'#013+
  #010+
  '  SErrDeletingFil','e      = '#039'Failed to delete file "%s"'#039';'#013#010+
  '  SErrRemovingDirectory = '#039'Failed to remove directory "%s"'#039';'#013+
  #010+
  '  SErrMovingFile        = '#039'Failed to move file "%s" to "%s"'#039';'#013+
  #010+
  '  SErrCopyingFile       = '#039'Failed to copy file "%s" to "%s"'#039';'#013+
  #010+
  '  SErrChange','DirFailed   = '#039'Failed to enter directory "%s"'#039';'#013+
  #010+
  '  SErrInvalidArgumentToSubstitute = '#039'Invalid number of arguments t'+
  'o Substitute'#039';'#013#010+
  '  SErrNoArchiveSupport  = '#039'This binary contains no archive support'+
  '. Please recompile with archive support'#039';'#013#010,
  '  SErrNoDictionaryItem  = '#039'No item called "%s" in the dictionary'#039+
  ';'#013#010+
  '  SErrNoDictionaryValue = '#039'The item "%s" in the dictionary is not '+
  'a value'#039';'#013#010+
  '  SErrNoDictionaryFunc  = '#039'The item "%s" in the dictionary is not '+
  'a function'#039';'#013#010+
  '  SErrInvalidFP','CInfo    = '#039'Compiler returns invalid information,'+
  ' check if fpc -iV works'#039';'#013#010+
  '  SErrDependencyNotFound = '#039'Could not find unit directory for depe'+
  'ndency package "%s"'#039';'#013#010+
  '  SErrAlreadyInitialized = '#039'Installer can only be initialized once'+
  #039';'#013#010+
  '  SEr','rInvalidState      = '#039'Invalid state for target %s'#039';'#013+
  #010+
  '  SErrCouldNotCompile   = '#039'Could not compile target %s from packag'+
  'e %s'#039';'#013#010+
  '  SErrUnsupportedBuildmode = '#039'Package does not support this buildm'+
  'ode'#039';'#013#010+
  '  SErrPackVarNotExist   = '#039'There is no p','ackage variant with the '+
  'name "%s"'#039';'#013#010+
  #013#010+
  '  SWarnCircularTargetDependency = '#039'Warning: Circular dependency de'+
  'tected when compiling target %s with target %s'#039';'#013#010+
  '  SWarnCircularPackageDependency = '#039'Warning: Circular dependency d'+
  'etected when compil','ing package %s with package %s'#039';'#013#010+
  '  SWarnFailedToSetTime    = '#039'Warning: Failed to set timestamp on f'+
  'ile "%s"'#039';'#013#010+
  '  SWarnFailedToGetTime    = '#039'Warning: Failed to get timestamp from'+
  ' file "%s"'#039';'#013#010+
  '  SWarnAttemptingToCompileNonNeutralTarget = '#039'Wa','rning: Attemptin'+
  'g to compile non-neutral target %s'#039';'#013#010+
  '  SWarnSourceFileNotFound  = '#039'Warning: Source file "%s" from packa'+
  'ge %s not found for %s'#039';'#013#010+
  '  SWarnIncludeFileNotFound = '#039'Warning: Include file "%s" from pack'+
  'age %s not found for %s'#039';'#013#010+
  ' ',' SWarnDepUnitNotFound     = '#039'Warning: Dependency on unit %s is '+
  'not supported for %s'#039';'#013#010+
  '  SWarnTargetDependsOnPackage = '#039'Warning: Target %s of package %s '+
  'depends on another package (%s). These kind of dependencies are not pr'+
  'ocessed'#039';'#013#010+
  '  SWar','nDependOnOtherPlatformPackage = '#039'Warning: Package %s depen'+
  'ds on package %s which is not available for the %s platform'#039';'#013#010+
  '  SWarnStartCompilingPackage = '#039'Start compiling package %s for tar'+
  'get %s.'#039';'#013#010+
  '  SWarnCompilingPackagecompleteProgress = ',#039'[%3.0f%%] Compiled pack'+
  'age %s'#039';'#013#010+
  '  SWarnCompilingPackagecomplete = '#039'Compiled package %s'#039';'#013#010+
  '  SWarnSkipPackageTargetProgress = '#039'[%3.0f%%] Skipped package %s w'+
  'hich has been disabled for target %s'#039';'#013#010+
  '  SWarnSkipPackageTarget = '#039'Skipped packag','e %s which has been di'+
  'sabled for target %s'#039';'#013#010+
  '  SWarnInstallationPackagecomplete = '#039'Installation package %s for '+
  'target %s succeeded'#039';'#013#010+
  '  SWarnCleanPackagecomplete = '#039'Clean of package %s completed'#039';'+
  #013#010+
  '  SWarnCanNotGetAccessRights = '#039'Warning: ','Failed to copy access-r'+
  'ights from file %s'#039';'#013#010+
  '  SWarnCanNotSetAccessRights = '#039'Warning: Failed to copy access-rig'+
  'hts to file %s'#039';'#013#010+
  '  SWarnCanNotGetFileAge = '#039'Warning: Failed to get FileAge for %s'#039+
  ';'#013#010+
  '  SWarnExtCommandNotFound = '#039'Warning: Exter','nal command "%s" not '+
  'found but "%s" is older then "%s"'#039';'#013#010+
  '  SWarnDuplicatePackage = '#039'Warning: Package %s is already added. U'+
  'sing the existing package'#039';'#013#010+
  '  SWarngccNotFound        = '#039'Could not find libgcc'#039';'#013#010+
  '  SWarngcclibpath         = '#039'Warn','ing: Unable to determine the li'+
  'bgcc path.'#039';'#013#010+
  '  SWarnNoFCLProcessSupport= '#039'No FCL-Process support'#039';'#013#010+
  #013#010+
  '  SInfoPackageAlreadyProcessed = '#039'Package %s is already processed'#039+
  ';'#013#010+
  '  SInfoCompilingTarget    = '#039'Compiling target %s'#039';'#013#010+
  '  SInfoExecutingC','ommand   = '#039'Executing command "%s %s"'#039';'#013+
  #010+
  '  SInfoCreatingOutputDir  = '#039'Creating output dir "%s"'#039';'#013#010+
  '  SInfoInstallingPackage  = '#039'Installing package %s'#039';'#013#010+
  '  SInfoArchivingPackage   = '#039'Archiving package %s in "%s"'#039';'#013+
  #010+
  '  SInfoCleaningPackage    =',' '#039'Cleaning package %s'#039';'#013#010+
  '  SInfoManifestPackage    = '#039'Creating manifest for package %s'#039';'+
  #013#010+
  '  SInfoCopyingFile        = '#039'Copying file "%s" to "%s"'#039';'#013#010+
  '  SInfoDeletedFile        = '#039'Deleted file "%s"'#039';'#013#010+
  '  SInfoRemovedDirectory   = '#039'Removed direc','tory "%s"'#039';'#013#010+
  '  SInfoSourceNewerDest    = '#039'Source file "%s" (%s) is newer than d'+
  'estination "%s" (%s).'#039';'#013#010+
  '  SInfoDestDoesNotExist   = '#039'Destination file "%s" does not exist.'+
  #039';'#013#010+
  '  SInfoFallbackBuildmode  = '#039'Buildmode not supported by package, f',
  'alling back to one by one unit compilation'#039';'#013#010+
  '  SInfoFallbackBuildmodeBU= '#039'Buildmode not supported by package, f'+
  'alling back to compilation using a buildunit'#039';'#013#010+
  #013#010+
  '  SDbgComparingFileTimes    = '#039'Comparing file "%s" time "%s" to "%'+
  's" time "%s"','.'#039';'#013#010+
  '  SDbgCompilingDependenciesOfTarget = '#039'Compiling dependencies of t'+
  'arget %s'#039';'#013#010+
  '  SDbgResolvingSourcesOfTarget = '#039'Resolving filenames of target %s'+
  ' for %s'#039';'#013#010+
  '  SDbgResolvedSourceFile    = '#039'Resolved source file %s to "%s"'#039+
  ';'#013#010+
  '  SDbgSourceAl','readyResolved = '#039'Source file of %s has been resolv'+
  'ed earlier'#039';'#013#010+
  '  SDbgResolvedIncludeFile   = '#039'Resolved include file %s to "%s"'#039+
  ';'#013#010+
  '  SDbgOutputNotYetAvailable = '#039'Output file %s not available'#039';'#013+
  #010+
  '  SDbgDependencyOnUnit      = '#039'Dependency of %','s on unit %s'#039';'+
  #013#010+
  '  SDbgDependencyUnitRecompiled = '#039'Dependent unit %s is being recom'+
  'piled'#039';'#013#010+
  '  SDbgMustCompile           = '#039'Must compile %s. (%s)'#039';'#013#010+
  '  SDbgSkippingTargetWrongCPU = '#039'Skipping target %s, different CPU '+
  '(%s)'#039';'#013#010+
  '  SDbgSkippingTarge','tWrongOS  = '#039'Skipping target %s, different OS'+
  ' (%s)'#039';'#013#010+
  '  SDbgTargetIsNotAUnitOrProgram = '#039'Skipping Target %s, not an unit'+
  ' or program'#039';'#013#010+
  '  SDbgConsideringTarget     = '#039'Considering target %s'#039';'#013#010+
  '  SDbgConsideringPackage    = '#039'Considering packag','e %s'#039';'#013#010+
  '  SDbgExternalDependency    = '#039'External dependency %s found in "%s'+
  '"'#039';'#013#010+
  '  SDbgBuildEngineArchiving  = '#039'Build engine archiving'#039';'#013#010+
  '  SDbgBuildEngineGenerateManifests = '#039'Build engine generating mani'+
  'fests'#039';'#013#010+
  '  SDbgBuildEngineCleaning   =',' '#039'Build engine cleaning'#039';'#013#010+
  '  SDbgGenerating            = '#039'Generating "%s"'#039';'#013#010+
  '  SDbgLoading               = '#039'Loading "%s"'#039';'#013#010+
  '  SDbgFound                 = '#039'Found'#039';'#013#010+
  '  SDbgNotFound              = '#039'Not Found'#039';'#013#010+
  '  SDbgDirectoryExists       = '#039'D','irectory "%s" %s'#039';'#013#010+
  '  SDbgFileExists            = '#039'File "%s" %s'#039';'#013#010+
  '  SDbgArchivingFile         = '#039'Archiving "%s"'#039';'#013#010+
  '  SDbgSearchPath            = '#039'Using %s path "%s"'#039';'#013#010+
  '  SDbgEnterDir              = '#039'Entering directory "%s"'#039';'#013#010+
  '  SDbgPackage','ChecksumChanged = '#039'Dependent package %s is modified'+
  #039';'#013#010+
  '  SDbgFileDoesNotExist      = '#039'File "%s" does not exist'#039';'#013#010+
  '  SDbgDirectoryDoesNotExist = '#039'Directory "%s" does not exist'#039';'+
  #013#010+
  '  SDbgDirectoryNotEmpty     = '#039'Directory "%s" is not empty. Wi','ll'+
  ' not remove'#039';'#013#010+
  '  SDbgGenerateBuildUnit     = '#039'Generate build-unit %s'#039';'#013#010+
  '  SDbgForcedCompile         = '#039'Forced compile'#039';'#013#010+
  '  SDbgOutputDoesNotExist    = '#039'Output file does not exist'#039';'#013+
  #010+
  '  SDbgNewerSource           = '#039'Source file is newer then',' output '+
  'file'#039';'#013#010+
  '  SDbgNewerInclude          = '#039'The include file %s is newer then o'+
  'utput file'#039';'#013#010+
  '  SDbgDependencyRecompiled  = '#039'The unit %s where this unit depends'+
  ' on is recompiled'#039';'#013#010+
  '  SDbgPackageDepRecompiled  = '#039'The package %s where this ','package'+
  ' depends on is recompiled'#039';'#013#010+
  '  SDbgTargetHasToBeCompiled = '#039'At least one of the targets in the '+
  'package has to be compiled.'#039';'#013#010+
  '  SDbgDeletedFile           = '#039'Recursively deleted file "%s"'#039';'+
  #013#010+
  '  SDbgRemovedDirectory      = '#039'Recursively ','removed directory "%s'+
  '"'#039';'#013#010+
  #013#010+
  #013#010+
  '  // Help messages for usage'#013#010+
  '  SValue              = '#039'Value'#039';'#013#010+
  '  SHelpUsage          = '#039'Usage: %s command [options]'#039';'#013#010+
  '  SHelpCommand        = '#039'Where command is one of the following:'#039+
  ';'#013#010+
  '  SHelpCompile        =',' '#039'Compile all units in the package(s).'#039+
  ';'#013#010+
  '  SHelpBuild          = '#039'Build all units in the package(s).'#039';'#013+
  #010+
  '  SHelpInstall        = '#039'Install all units in the package(s).'#039';'+
  #013#010+
  '  SHelpClean          = '#039'Clean (remove) all units in the package(s'+
  ').'#039';',#013#010+
  '  SHelpArchive        = '#039'Create archive (zip) with all units in th'+
  'e package(s).'#039';'#013#010+
  '  SHelpHelp           = '#039'This message.'#039';'#013#010+
  '  SHelpManifest       = '#039'Create a manifest suitable for import in '+
  'repository.'#039';'#013#010+
  '  SHelpZipInstall     = '#039'Install',' all units in the package(s) int'+
  'o an archive.'#039';'#013#010+
  '  SHelpCmdOptions     = '#039'Where options is one or more of the follo'+
  'wing:'#039';'#013#010+
  '  SHelpCPU            = '#039'Compile for indicated CPU.'#039';'#013#010+
  '  SHelpOS             = '#039'Compile for indicated OS'#039';'#013#010+
  '  SHelpT','arget         = '#039'Compile for indicated target'#039';'#013#010+
  '  SHelpList           = '#039'list commands instead of actually executi'+
  'ng them.'#039';'#013#010+
  '  SHelpPrefix         = '#039'Use indicated prefix directory for all co'+
  'mmands.'#039';'#013#010+
  '  SHelpNoFPCCfg       = '#039'Compiler wi','ll not use fpc.cfg'#039';'#013+
  #010+
  '  SHelpBaseInstallDir = '#039'Use indicated directory as base install d'+
  'ir.'#039';'#013#010+
  '  SHelpLocalUnitDir   = '#039'Use indicated directory as local (user) u'+
  'nit dir.'#039';'#013#010+
  '  SHelpGlobalUnitDir  = '#039'Use indicated directory as global unit di'+
  'r','.'#039';'#013#010+
  '  SHelpUnitInstallDir = '#039'Use indicated directory to install units '+
  'into.'#039';'#013#010+
  '  SHelpCompiler       = '#039'Use indicated binary as compiler'#039';'#013+
  #010+
  '  SHelpConfig         = '#039'Use indicated config file when compiling.'+
  #039';'#013#010+
  '  SHelpOptions        = '#039'Pass',' extra options to the compiler.'#039+
  ';'#013#010+
  '  SHelpVerbose        = '#039'Be verbose when working.'#039';'#013#010+
  '  SHelpInstExamples   = '#039'Install the example-sources.'#039';'#013#010+
  '  SHelpSkipCrossProgs = '#039'Skip programs when cross-compiling/instal'+
  'ling'#039';'#013#010+
  '  SHelpIgnoreInvOpt  ',' = '#039'Ignore further invalid options.'#039';'#013+
  #010+
  '  sHelpFpdocOutputDir = '#039'Use indicated directory as fpdoc output f'+
  'older.'#039';'#013#010+
  '  sHelpThreads        = '#039'Enable the indicated amount of worker thr'+
  'eads.'#039';'#013#010+
  '  sHelpUseEnvironment = '#039'Use environment to pass o','ptions to comp'+
  'iler.'#039';'#013#010+
  '  SHelpUseBuildUnit   = '#039'Compile package in Build-unit mode.'#039';'+
  #013#010+
  '  sHelpZipPrefix      = '#039'Use indicated prefix for generated archiv'+
  'es.'#039';'#013#010+
  #013#010+
  #013#010+
  'Const'#013#010+
  '  // Keys for Defaults file. Do not localize.'#013#010+
  '  KeyCompiler = '#039'Compi','ler'#039';'#013#010+
  '  KeyArchive  = '#039'Archive'#039';'#013#010+
  '  KeyCopy     = '#039'Copy'#039';'#013#010+
  '  KeyMkDir    = '#039'MkDir'#039';'#013#010+
  '  KeyMove     = '#039'Move'#039';'#013#010+
  '  KeyRemove   = '#039'Remove'#039';'#013#010+
  '  KeyRemoveDir= '#039'RemoveDir'#039';'#013#010+
  '  KeyRemoveTree= '#039'RemoveTree'#039';'#013#010+
  '  KeyOptions  = '#039'Options'#039';'#013#010+
  '  KeyCPU    ','  = '#039'CPU'#039';'#013#010+
  '  KeyOS       = '#039'OS'#039';'#013#010+
  '  KeyMode     = '#039'Mode'#039';'#013#010+
  '  KeyPrefix   = '#039'Prefix'#039';'#013#010+
  '  KeyTarget   = '#039'Target'#039';'#013#010+
  '  KeyNoFPCCfg = '#039'NoFPCCfg'#039';'#013#010+
  '  KeyUseEnv   = '#039'UseEnv'#039';'#013#010+
  '  KeyLocalUnitDir       = '#039'LocalUnitDir'#039';'#013#010+
  '  KeyGlobalUnitDir      = '#039,'GlobalUnitDir'#039';'#013#010+
  '  KeyBaseInstallDir     = '#039'BaseInstallDir'#039';'#013#010+
  '  KeyUnitInstallDir     = '#039'UnitInstallDir'#039';'#013#010+
  '  KeyBinInstallDir      = '#039'BinInstallDir'#039';'#013#010+
  '  KeyDocInstallDir      = '#039'DocInstallDir'#039';'#013#010+
  '  KeyExamplesInstallDir = '#039'ExamplesInstallDir',#039';'#013#010+
  '  KeyInstallExamples    = '#039'InstallExamples'#039';'#013#010+
  '  KeySkipCrossProdrams  = '#039'SkipCrossPrograms'#039';'#013#010+
  '  // Keys for unit config'#013#010+
  '  KeyName     = '#039'Name'#039';'#013#010+
  '  KeyVersion  = '#039'Version'#039';'#013#010+
  '  KeyChecksum = '#039'Checksum'#039';'#013#010+
  '  KeyNeedLibC = '#039'NeedLibC'#039';'#013#010+
  '  Ke','yDepends  = '#039'Depends'#039';'#013#010+
  '  KeyFlags    = '#039'Flags'#039';'#013#010+
  '  KeyAddIn    = '#039'FPMakeAddIn'#039';'#013#010+
  '  KeySourcePath = '#039'SourcePath'#039';'#013#010+
  '  KeyFPMakeOptions = '#039'FPMakeOptions'#039';'#013#010+
  '  KeyPackageVar = '#039'PackageVariant_'#039';'#013#010+
  #013#010+
  '{*********************************************','**********************'+
  '*********'#013#010+
  '                                Helpers'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  '{$ifdef HAS_UNIT_PROCESS}'#013#010+
  'function ExecuteFPC(Verbose: boolean; const Path: string; ','const ComL'+
  'ine: string; const Env: TStrings; ConsoleOutput: TMemoryStream): integ'+
  'er;'#013#010+
  'var'#013#010+
  '  P: TProcess;'#013#010+
  '  BytesRead: longint;'#013#010+
  #013#010+
  '  function ReadFromStream(const ReadFromStdErr: boolean): longint;'#013+
  #010+
  #013#010+
  '  const'#013#010+
  '    READ_BYTES = 2048;'#013#010+
  #013#010+
  '  ty','pe'#013#010+
  '    TMessages = (mCompiling, mLinking);'#013#010+
  #013#010+
  '  var'#013#010+
  '    //ifdef the MsgNum so it contains the correct message numbers for '+
  'each compiler version.'#013#010+
  '    MsgNum : array [TMessages] of integer = (3104, 9015);'#013#010+
  #013#010+
  '    n: longint;'#013#010+
  '    BuffPos: l','ongint;'#013#010+
  '    sLine: string;'#013#010+
  '    ch: char;'#013#010+
  '    msg: TMessages;'#013#010+
  '    ipos: integer;'#013#010+
  '    snum: string;'#013#010+
  '  begin'#013#010+
  '    // make sure we have room'#013#010+
  '    ConsoleOutput.SetSize(BytesRead + READ_BYTES);'#013#010+
  #013#010+
  '    // try reading it'#013#010+
  '    if ReadFromStdE','rr then'#013#010+
  '      n := P.Stderr.Read((ConsoleOutput.Memory + BytesRead)^, READ_BYT'+
  'ES)'#013#010+
  '    else'#013#010+
  '      n := P.Output.Read((ConsoleOutput.Memory + BytesRead)^, READ_BYT'+
  'ES);'#013#010+
  '    if n > 0 then'#013#010+
  '    begin'#013#010+
  '      Inc(BytesRead, n);'#013#010+
  #013#010+
  '      sLine ',':= '#039#039';'#013#010+
  '      BuffPos := ConsoleOutput.Position;'#013#010+
  #013#010+
  '      //read lines from the stream'#013#010+
  '      repeat'#013#010+
  '        ConsoleOutput.Read(ch,1);'#013#010+
  #013#010+
  '        if ch in [#10, #13] then'#013#010+
  '        begin'#013#010+
  '          if Verbose then'#013#010+
  '            installer.log','(vlInfo,sLine)'#013#010+
  '          else'#013#010+
  '            begin'#013#010+
  '              for msg := Low(TMessages) to High(TMessages) do'#013#010+
  '              begin'#013#010+
  '                snum := Format('#039'(%d)'#039', [MsgNum[msg]]);'#013#010+
  '                ipos := Pos(snum, sLine);'#013#010+
  '      ','          if ipos = 1 then'#013#010+
  '                  installer.log(vlCommand,'#039'      '#039'+ Copy(sLine'+
  ', ipos + Length(snum), Length(sLine) - ipos - Length(snum) + 1));'#013#010+
  '              end;'#013#010+
  '            end;'#013#010+
  '          if (LineEnding=#13#10) and (ch=#13)',' and'#013#010+
  '             (ConsoleOutput.Position<BytesRead) then'#013#010+
  '            begin'#013#010+
  '              ConsoleOutput.Read(ch,1);'#013#010+
  '              if ch=#10 then'#013#010+
  '                sLine:='#039#039#013#010+
  '              else'#013#010+
  '                sLine:=ch;'#013#010+
  '            end',#013#010+
  '          else'#013#010+
  '            sLine := '#039#039';'#013#010+
  '          BuffPos := ConsoleOutput.Position;'#013#010+
  '        end'#013#010+
  '        else'#013#010+
  '          sLine := sLine + ch;'#013#010+
  #013#010+
  '      until ConsoleOutput.Position >= BytesRead;'#013#010+
  #013#010+
  '      ConsoleOutput.Position := BuffP','os;'#013#010+
  '    end'#013#010+
  '    else'#013#010+
  '    begin'#013#010+
  '      // no data, wait 100 ms'#013#010+
  '      Sleep(100);'#013#010+
  '    end;'#013#010+
  #013#010+
  '    Result := n;'#013#010+
  '  end;'#013#010+
  #013#010+
  'begin'#013#010+
  '  result := -1;'#013#010+
  '  BytesRead := 0;'#013#010+
  '  P := TProcess.Create(nil);'#013#010+
  '  try'#013#010+
  '    P.CommandLine := Path + '#039' '#039' + C','omLine;'#013#010+
  '    if assigned(Env) then'#013#010+
  '      P.Environment.Assign(Env);'#013#010+
  #013#010+
  '    P.Options := [poUsePipes];'#013#010+
  #013#010+
  '    P.Execute;'#013#010+
  '    while P.Running do'#013#010+
  '      ReadFromStream(false);'#013#010+
  #013#010+
  '    // read last part'#013#010+
  '    repeat'#013#010+
  '    until ReadFromStream(fa','lse)=0;'#013#010+
  #013#010+
  '    // read stderr'#013#010+
  '    // JvdS: Note that this way stderr is added to the end of the stre'+
  'am. But I'#013#010+
  '    // see no way showing the stderr output at the place it was actual'+
  'ly written'#013#010+
  '    repeat'#013#010+
  '    until ReadFromStream(true)=0;'#013,#010+
  '    ConsoleOutput.SetSize(BytesRead);'#013#010+
  #013#010+
  '    result := P.ExitStatus;'#013#010+
  '  finally'#013#010+
  '    P.Free;'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  '{$endif HAS_UNIT_PROCESS}'#013#010+
  #013#010+
  'function IsDirectoryEmpty(const directory : string) : boolean;'#013#010+
  'var'#013#010+
  '  searchRec: TSearchRec;'#013#010+
  '  Searc','hResult: longint;'#013#010+
  'begin'#013#010+
  '  result := true;'#013#010+
  '  SearchResult := FindFirst(IncludeTrailingPathDelimiter(directory)+Al'+
  'lFilesMask, faAnyFile+faSymLink, searchRec);'#013#010+
  '  try'#013#010+
  '    while SearchResult=0 do'#013#010+
  '      begin'#013#010+
  '        if (searchRec.Name<>'#039'.',#039') and (searchRec.Name<>'#039'..'#039+
  ') then'#013#010+
  '           begin'#013#010+
  '             result := false;'#013#010+
  '             break;'#013#010+
  '           end;'#013#010+
  '        SearchResult := FindNext(searchRec);'#013#010+
  '      end;'#013#010+
  '  finally'#013#010+
  '    FindClose(searchRec);'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  'funct','ion ParsecompilerOutput(M: TMemoryStream; Verbose: boolean): st'+
  'ring;'#013#010+
  'type'#013#010+
  '  TParseCompilerOutputState = (cosBeginOfLine, cosSearchColon, cosPars'+
  'eNumber, cosOther);'#013#010+
  #013#010+
  'var'#013#010+
  '  presult: pchar;'#013#010+
  '  state: TParseCompilerOutputState;'#013#010+
  '  ch: char',';'#013#010+
  '  eolchar: char;'#013#010+
  'begin'#013#010+
  '  m.Seek(0, soBeginning);'#013#010+
  '  setlength(Result,M.Size);'#013#010+
  #013#010+
  '  if verbose then'#013#010+
  '    begin'#013#010+
  '      m.Read(Result[1],M.Size);'#013#010+
  '      Exit;'#013#010+
  '    end;'#013#010+
  #013#010+
  '  presult := @Result[1];'#013#010+
  '  eolchar := RightStr(LineEnding,1)[1];'#013,#010+
  '  m.Seek(0,soBeginning);'#013#010+
  '  state := cosBeginOfLine;'#013#010+
  '  while m.Position<m.Size do'#013#010+
  '    begin'#013#010+
  '      ch := char(m.ReadByte);'#013#010+
  '      case state of'#013#010+
  '        cosBeginOfLine:'#013#010+
  '          begin'#013#010+
  '            if ch='#039'('#039' then'#013#010+
  '              state :=',' cosParseNumber'#013#010+
  '            else if ch='#039' '#039' then'#013#010+
  '              begin'#013#010+
  '                presult^ := ch;'#013#010+
  '                inc(presult);'#013#010+
  '              end'#013#010+
  '            else'#013#010+
  '              begin'#013#010+
  '                presult^ := ch;'#013#010+
  '              ','  inc(presult);'#013#010+
  '                state := cosSearchColon;'#013#010+
  '              end;'#013#010+
  '          end;'#013#010+
  '        cosParseNumber:'#013#010+
  '          begin'#013#010+
  '            if ch='#039')'#039' then'#013#010+
  '              begin'#013#010+
  '              state := cosOther;'#013#010+
  '              // Omi','t the space behind the number'#013#010+
  '              ch := char(m.ReadByte);'#013#010+
  '              assert(ch='#039' '#039');'#013#010+
  '              end;'#013#010+
  '          end;'#013#010+
  '        cosOther:'#013#010+
  '          begin'#013#010+
  '            presult^ := ch;'#013#010+
  '            inc(presult);'#013#010+
  '           ',' if ch=eolchar then'#013#010+
  '              state := cosBeginOfLine;'#013#010+
  '          end;'#013#010+
  '        cosSearchColon:'#013#010+
  '          begin'#013#010+
  '            presult^ := ch;'#013#010+
  '            inc(presult);'#013#010+
  '            if (ch='#039':'#039') or (ch=eolchar) then'#013#010+
  '              state',' := cosBeginOfLine;'#013#010+
  '          end;'#013#010+
  '      end;'#013#010+
  '    end;'#013#010+
  '  setlength(Result,presult-@result[1]);'#013#010+
  'end;'#013#010+
  #013#010+
  'Function QuoteXML(S : String) : string;'#013#010+
  #013#010+
  '  Procedure W(Var J : Integer; Var R : String; T : String);'#013#010+
  '  Var'#013#010+
  '    I: integer;'#013#010+
  '  be','gin'#013#010+
  '    If J+Length(T)>Length(R) then'#013#010+
  '      SetLength(R,J+Length(T));'#013#010+
  '    For I:=1 to Length(t) do'#013#010+
  '      begin'#013#010+
  '      R[J]:=T[i];'#013#010+
  '      If I<Length(T) then'#013#010+
  '        Inc(J);'#013#010+
  '      end;'#013#010+
  '  end;'#013#010+
  #013#010+
  'const'#013#010+
  '  QuotStr = '#039'&quot;'#039';'#013#010+
  '  AmpStr ','= '#039'&amp;'#039';'#013#010+
  '  ltStr = '#039'&lt;'#039';'#013#010+
  '  gtStr = '#039'&gt;'#039';'#013#010+
  'Var'#013#010+
  '  I,J : Integer;'#013#010+
  'begin'#013#010+
  '  SetLength(Result,Length(S));'#013#010+
  '  J:=0;'#013#010+
  '  For I:=1 to Length(S) do'#013#010+
  '    begin'#013#010+
  '    Inc(J);'#013#010+
  '    case S[i] of'#013#010+
  '      '#039'"'#039': W(j,Result,QuotStr);'#013#010+
  '      '#039'&'#039': W(J,R','esult,AmpStr);'#013#010+
  '      '#039'<'#039': W(J,Result,ltStr);'#013#010+
  '      '#039'>'#039': W(J,Result,gtStr);'#013#010+
  '      // Escape whitespace using CharRefs to be consistent with W3 spe'+
  'c X 3.3.3'#013#010+
  '       #9: w(J,Result,'#039'&#x9;'#039');'#013#010+
  '{      #10: wrtStr('#039'&#xA;'#039');'#013#010+
  '       #13: wrtStr','('#039'&#xD;'#039');}'#013#010+
  '    else'#013#010+
  '      Result[J]:=S[i];'#013#010+
  '    end;'#013#010+
  '    If (J=Length(Result)) and (I<Length(S)) then'#013#010+
  '      SetLength(Result,J+Length(S)-I);'#013#010+
  '    end;'#013#010+
  '  If J<>Length(Result) then'#013#010+
  '    SetLength(Result,J);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function maybequot','ed(const s:string):string;'#013#010+
  'const'#013#010+
  '  {$IF DEFINED(MSWINDOWS) OR DEFINED(AMIGA) OR DEFINED(MORPHOS)}'#013#010+
  '    FORBIDDEN_CHARS = ['#039'!'#039', '#039'@'#039', '#039'#'#039', '#039'$'#039+
  ', '#039'%'#039', '#039'^'#039', '#039'&'#039', '#039'*'#039', '#039'('#039', '#039+
  ')'#039','#013#010+
  '                       '#039'{'#039', '#039'}'#039', '#039#039#039#039', '#039'`'+
  #039', '#039'~'#039'];'#013#010+
  '  {$ELSE}'#013#010+
  '   ',' FORBIDDEN_CHARS = ['#039'!'#039', '#039'@'#039', '#039'#'#039', '#039'$'+
  #039', '#039'%'#039', '#039'^'#039', '#039'&'#039', '#039'*'#039', '#039'('#039', '+
  #039')'#039','#013#010+
  '                       '#039'{'#039', '#039'}'#039', '#039#039#039#039', '#039':'+
  #039', '#039'\'#039', '#039'`'#039', '#039'~'#039'];'#013#010+
  '  {$ENDIF}'#013#010+
  'var'#013#010+
  '  s1 : string;'#013#010+
  '  i  : integer;'#013#010+
  '  quoted : boolean;'#013#010+
  'begin'#013#010+
  '  quoted:=false;'#013#010+
  '  s1:='#039'"'#039';'#013#010,
  '  for i:=1 to length(s) do'#013#010+
  '   begin'#013#010+
  '     case s[i] of'#013#010+
  '       '#039'"'#039' :'#013#010+
  '         begin'#013#010+
  '           quoted:=true;'#013#010+
  '           s1:=s1+'#039'\"'#039';'#013#010+
  '         end;'#013#010+
  '       '#039' '#039','#013#010+
  '       #128..#255 :'#013#010+
  '         begin'#013#010+
  '           quoted:=true;'#013#010+
  '           ','s1:=s1+s[i];'#013#010+
  '         end;'#013#010+
  '       else begin'#013#010+
  '         if s[i] in FORBIDDEN_CHARS then'#013#010+
  '           quoted:=True;'#013#010+
  '         s1:=s1+s[i];'#013#010+
  '       end;'#013#010+
  '     end;'#013#010+
  '   end;'#013#010+
  '  if quoted then'#013#010+
  '    maybequoted:=s1+'#039'"'#039#013#010+
  '  else'#013#010+
  '    maybequoted:=','s;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure ReadIniFile(Const AFileName: String;L:TStrings);'#013#010+
  'Var'#013#010+
  '  F : TFileStream;'#013#010+
  '  Line : String;'#013#010+
  '  I,P,PC : Integer;'#013#010+
  'begin'#013#010+
  '  // On some file systems and when using a large number of parallel ma'+
  'ke'#013#010+
  '  // processes, the l','ock from the creation of the ini file may not y'+
  'et'#013#010+
  '  // have been released even though the file has been closed already'#013+
  #010+
  '  F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyNone);'#013#010+
  '  Try'#013#010+
  '    L.LoadFromStream(F);'#013#010+
  '    // Fix lines.'#013#010,
  '    For I:=L.Count-1 downto 0 do'#013#010+
  '      begin'#013#010+
  '        Line:=L[I];'#013#010+
  '        P:=Pos('#039'='#039',Line);'#013#010+
  '        PC:=Pos('#039';'#039',Line);  // Comment line.'#013#010+
  '        If (P=0) or ((PC<>0) and (PC<P)) then'#013#010+
  '          L.Delete(I)'#013#010+
  '        else'#013#010+
  '          L[i]:','=Trim(System.Copy(Line,1,P-1)+'#039'='#039'+Trim(System'+
  '.Copy(Line,P+1,Length(Line)-P)));'#013#010+
  '      end;'#013#010+
  '  Finally'#013#010+
  '    F.Free;'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '// Callback for Sysutils getapplicationname.'#013#010+
  'Function GetFPMakeName : String;'#013#010+
  #013#010+
  'begin'#013#010+
  '  Result:='#039'fpmake',#039';'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function CurrentOS : String;'#013#010+
  #013#010+
  'begin'#013#010+
  '  Result:=OSToString(Defaults.OS);'#013#010+
  'end;'#013#010+
  #013#010+
  'Function CurrentCPU : String;'#013#010+
  #013#010+
  'begin'#013#010+
  '  Result:=CPUToString(Defaults.CPU);'#013#010+
  'end;'#013#010+
  #013#010+
  'Function OSToString(OS: TOS) : String;'#013#010+
  #013#010+
  'begin'#013#010+
  '  Result',':=LowerCase(GetenumName(TypeInfo(TOS),Ord(OS)));'#013#010+
  'end;'#013#010+
  #013#010+
  'Function OSesToString(OSes: TOSes) : String;'#013#010+
  #013#010+
  'var'#013#010+
  '  i: TOS;'#013#010+
  '  Tmp: String;'#013#010+
  '  First: boolean;'#013#010+
  #013#010+
  'begin'#013#010+
  '  { can'#039't use SetToString, there are more than 32 OSes }'#013#010+
  '  First:=true;'#013#010,
  '  for i in OSes do'#013#010+
  '    begin'#013#010+
  '      if not First then'#013#010+
  '        Result:=Result+'#039','#039#013#010+
  '      else'#013#010+
  '        First:=false;'#013#010+
  '      Str(i,Tmp);'#013#010+
  '      Result:=Result+Tmp;'#013#010+
  '    end;'#013#010+
  '  Result:=LowerCase(Tmp);'#013#010+
  'end;'#013#010+
  #013#010+
  'Function CPUToString(CPU: TCPU',') : String;'#013#010+
  #013#010+
  'begin'#013#010+
  '  Result:=LowerCase(GetenumName(TypeInfo(TCPU),Ord(CPU)));'#013#010+
  'end;'#013#010+
  #013#010+
  'Function CPUSToString(CPUS: TCPUS) : String;'#013#010+
  #013#010+
  'begin'#013#010+
  '  Result:=LowerCase(SetToString(PTypeInfo(TypeInfo(TCPUS)),Integer(CPU'+
  'S),False));'#013#010+
  'end;'#013#010+
  #013#010+
  'Func','tion StringToOS(const S : String) : TOS;'#013#010+
  #013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  #013#010+
  'begin'#013#010+
  '  I:=GetEnumValue(TypeInfo(TOS),S);'#013#010+
  '  if (I=-1) then'#013#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidOS,[S]);'#013#010+
  '  Result:=TOS(I);'#013#010+
  'end;'#013#010+
  #013#010+
  '(*'#013#010+
  'Function StringToOSes(c','onst S : String) : TOSes;'#013#010+
  #013#010+
  'begin'#013#010+
  '  Result:=TOSes(StringToSet(PTypeInfo(TypeInfo(TOSes)),S));'#013#010+
  'end;'#013#010+
  '*)'#013#010+
  #013#010+
  'function IsDifferentFromBuild(ACpu: TCPU; AOs: TOs): boolean;'#013#010+
  'begin'#013#010+
  '  result := (AOs<>Defaults.BuildOS) or (ACpu<>Defaults.BuildC','PU);'#013+
  #010+
  'end;'#013#010+
  #013#010+
  'Function StringToCPU(const S : String) : TCPU;'#013#010+
  #013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  #013#010+
  'begin'#013#010+
  '  I:=GetEnumValue(TypeInfo(TCPU),S);'#013#010+
  '  if (I=-1) then'#013#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidCPU,[S]);'#013#010+
  '  Result:=TCPU(I);'#013#010+
  'end;'#013#010+
  #013#010+
  'Func','tion StringToCPUS(const S : String) : TCPUS;'#013#010+
  #013#010+
  'begin'#013#010+
  '  Result:=TCPUS(StringToSet(PTypeInfo(TypeInfo(TCPUS)),S));'#013#010+
  'end;'#013#010+
  #013#010+
  'Function ModeToString(Mode: TCompilerMode) : String;'#013#010+
  #013#010+
  'begin'#013#010+
  '  Result:=LowerCase(GetenumName(TypeInfo(TCompilerMod','e),Ord(Mode)));'+
  #013#010+
  '  Delete(Result,1,2);'#013#010+
  'end;'#013#010+
  #013#010+
  'Function StringToMode(const S : String) : TCompilerMode;'#013#010+
  #013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  #013#010+
  'begin'#013#010+
  '  I:=GetEnumValue(TypeInfo(TCompilerMode),S);'#013#010+
  '  if (I=-1) then'#013#010+
  '    Raise EInstallerError.CreateFmt(S','ErrInvalidMode,[S]);'#013#010+
  '  Result:=TCompilerMode(I);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function MakeTargetString(CPU : TCPU;OS: TOS) : String;'#013#010+
  #013#010+
  'begin'#013#010+
  '  if OS in AllLimit83fsOses then'#013#010+
  '    Result := OSToString(OS)'#013#010+
  '  else'#013#010+
  '    Result:=CPUToString(CPU)+'#039'-'#039'+OSToStri','ng(OS);'#013#010+
  'end;'#013#010+
  #013#010+
  'Procedure StringToCPUOS(const S : String; Var CPU : TCPU; Var OS: TOS)'+
  ';'#013#010+
  #013#010+
  'Var'#013#010+
  '  P : integer;'#013#010+
  #013#010+
  'begin'#013#010+
  '  P:=Pos('#039'-'#039',S);'#013#010+
  '  If (P=0) then'#013#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidTarget,[S]);'#013#010+
  '  CPU:=StringToCPU(Cop','y(S,1,P-1));'#013#010+
  '  OS:=StringToOs(Copy(S,P+1,Length(S)-P));'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Procedure ResolveDependencies(L : TDependencies; P : TNamedCollection)'+
  ';'#013#010+
  'Var'#013#010+
  '  I,J : Integer;'#013#010+
  '  C : TDependency;'#013#010+
  'begin'#013#010+
  '  If Assigned(L) then'#013#010+
  '    For I:=0 to L.Count-1 ','do'#013#010+
  '      begin'#013#010+
  '        C:=L[i];'#013#010+
  '        if C.DependencyType in [depPackage,depUnit] then'#013#010+
  '          begin'#013#010+
  '            J:=P.IndexOfName(C.Value);'#013#010+
  '            If J<>-1 then'#013#010+
  '              C.Target:=P.Items[J];'#013#010+
  '          end;'#013#010+
  '      end;',#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function AddConditionalStrings(APackage: TPackage; Dest : TStrings; Sr'+
  'c : TConditionalStrings;ACPU:TCPU;AOS:TOS; Const APrefix : String='#039+
  #039') : Integer ;'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  '  C : TConditionalString;'#013#010+
  '  D : TPackageDictionary;'#013#010,
  '  S : String;'#013#010+
  'begin'#013#010+
  '  Result:=0;'#013#010+
  '  D := PackageDictionaryClass.Create(nil);'#013#010+
  '  D.MasterDictionary := APackage.Dictionary;'#013#010+
  '  try'#013#010+
  '    D.AddVariable('#039'CPU'#039',CPUToString(ACPU));'#013#010+
  '    D.AddVariable('#039'OS'#039',OSToString(AOS));'#013#010+
  '    For I:=0 to Src.','Count-1 do'#013#010+
  '      begin'#013#010+
  '        C:=Src[I];'#013#010+
  '        if (ACPU in C.CPUs) and (AOS in C.OSes) then'#013#010+
  '          begin'#013#010+
  '            If (APrefix<>'#039#039') then'#013#010+
  '              S:=APrefix+C.Value'#013#010+
  '            else'#013#010+
  '              S:=C.Value;'#013#010+
  '          ','  S := D.ReplaceStrings(s);'#013#010+
  '            if C is TConditionalDestString then'#013#010+
  '              begin'#013#010+
  '                // If a destination path is given, omit the path of th'+
  'e sourcefile'#013#010+
  '                if TConditionalDestString(c).DestPath='#039#039' ','then'#013+
  #010+
  '                  Dest.values[S] :=  D.ReplaceStrings(IncludeTrailingP'+
  'athDelimiter(TConditionalDestString(c).DestPath))+S'#013#010+
  '                else'#013#010+
  '                  Dest.values[S] :=  D.ReplaceStrings(IncludeTrailingP'+
  'athDelimiter(TCondi','tionalDestString(c).DestPath)+APrefix+ExtractFile'+
  'Name(C.Value));'#013#010+
  '              end'#013#010+
  '            else'#013#010+
  '              Dest.Add(S);'#013#010+
  '            Inc(Result);'#013#010+
  '          end;'#013#010+
  '      end;'#013#010+
  '  finally'#013#010+
  '    D.Free;'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function File','ListToString(List : TStrings; const APrefix : String) :'+
  ' String;'#013#010+
  'Var'#013#010+
  '  I : integer;'#013#010+
  '  S : String;'#013#010+
  'begin'#013#010+
  '  Result:='#039#039';'#013#010+
  '  For I:=0 to List.Count-1 do'#013#010+
  '    begin'#013#010+
  '      If (I>0) then'#013#010+
  '        Result:=Result+'#039' '#039';'#013#010+
  '      S:=APrefix+List[i];',#013#010+
  '      If (Pos('#039' '#039',S)<>0) then'#013#010+
  '        S:='#039'"'#039'+S+'#039'"'#039';'#013#010+
  '      Result:=Result+S;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function FixPath (const APath : String) : String;'#013#010+
  'begin'#013#010+
  '  Result := FixPath(APath, False);'#013#010+
  'end;'#013#010+
  #013#010+
  'function FixPath (const APath : Strin','g; AIsDir : Boolean) : String;'#013+
  #010+
  'Var'#013#010+
  '  P : PChar;'#013#010+
  'begin'#013#010+
  '  Result:=APath;'#013#010+
  '  If (result<>'#039#039') then'#013#010+
  '    begin'#013#010+
  '      UniqueString(Result);'#013#010+
  '      P:=PChar(Result);'#013#010+
  '      While (P^<>#0) do'#013#010+
  '        begin'#013#010+
  '          If P^ in ['#039'/'#039','#039'\'#039'] then'#013#010,
  '            P^:=PathDelim;'#013#010+
  '          Inc(P);'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  '  if AIsDir and (Result <> '#039#039') then'#013#010+
  '    Result := IncludeTrailingPathDelimiter(Result);;'#013#010+
  'end;'#013#010+
  #013#010+
  'function IsRelativePath(const APath: String): boolean;'#013#010+
  'begin'#013#010+
  '  if AP','ath='#039#039' then'#013#010+
  '    result := true'#013#010+
  '{$ifdef unix}'#013#010+
  '  else if APath[1] in AllowDirectorySeparators then'#013#010+
  '    result := false'#013#010+
  '{$else}'#013#010+
  '  else if ExtractFileDrive(APath)<>'#039#039' then'#013#010+
  '    result := false'#013#010+
  '{$endif}'#013#010+
  '  else'#013#010+
  '    result := true;'#013#010+
  'end;'#013,#010+
  #013#010+
  'procedure ChangeDir(const APath : String);'#013#010+
  'begin'#013#010+
  '  if Not SetCurrentDir(APath) then'#013#010+
  '    Raise EInstallerError.CreateFmt(SErrChangeDirFailed,[APath]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure SearchFiles(const AFileName: string; Recursive: boolean; var'+
  ' Li','st: TStrings);'#013#010+
  #013#010+
  '  procedure AddRecursiveFiles(const SearchDir, FileMask: string; Recur'+
  'sive: boolean);'#013#010+
  '  var'#013#010+
  '    Info : TSearchRec;'#013#010+
  '  begin'#013#010+
  '    if FindFirst(SearchDir+AllFilesMask,faAnyFile and faDirectory,Info'+
  ')=0 then'#013#010+
  '    begin'#013#010+
  '    ','  repeat'#013#010+
  '          if ((Info.Attr and faDirectory) = faDirectory) and (Info.Nam'+
  'e <> '#039'.'#039') and (Info.Name <> '#039'..'#039') and (Recursive) then'+
  #013#010+
  '            AddRecursiveFiles(SearchDir + Info.Name + PathDelim, FileM'+
  'ask, Recursive);'#013#010+
  '          if ((I','nfo.Attr and faDirectory) <> faDirectory) and IsWild'+
  '(Info.Name, FileMask, FileNameCaseSensitive) then'#013#010+
  '            List.Add(SearchDir + Info.Name);'#013#010+
  '      until FindNext(Info)<>0;'#013#010+
  '    end;'#013#010+
  '    FindClose(Info);'#013#010+
  '  end;'#013#010+
  #013#010+
  'var'#013#010+
  '  CurrDir,'#013#010,
  '  BasePath: string;'#013#010+
  '  i: integer;'#013#010+
  'begin'#013#010+
  '  BasePath := ExtractFilePath(ExpandFileName(AFileName));'#013#010+
  '  AddRecursiveFiles(BasePath, ExtractFileName(AFileName), Recursive);'#013+
  #010+
  #013#010+
  '  CurrDir:=GetCurrentDir;'#013#010+
  '  for i := 0 to Pred(List.Count) do'#013#010+
  ' ','   List[i] := ExtractRelativepath(IncludeTrailingPathDelimiter(Curr'+
  'Dir), List[i]);'#013#010+
  'end;'#013#010+
  #013#010+
  'Const'#013#010+
  '  WhiteSpace = [#9,#10,#13,'#039' '#039'];'#013#010+
  '  QuoteChars = ['#039#039#039#039','#039'"'#039'];'#013#010+
  #013#010+
  'procedure SplitCommand(const Cmd : String; var Exe, Options : String);'+
  #013#010+
  #013#010+
  'Var',#013#010+
  '  I : Integer;'#013#010+
  '  InQuote : Boolean;'#013#010+
  '  LastQuote : Char;'#013#010+
  '  S : String;'#013#010+
  #013#010+
  'begin'#013#010+
  '  S:=Trim(Cmd);'#013#010+
  '  InQuote:=False;'#013#010+
  '  LastQuote:=#0;'#013#010+
  '  I:=1;'#013#010+
  '  While (I<=Length(S)) and (Inquote or not (S[I] in whiteSpace)) do'#013+
  #010+
  '    begin'#013#010+
  '    If S[i] ','in QuoteChars then'#013#010+
  '      begin'#013#010+
  '      InQuote:=Not (S[i]=LastQuote);'#013#010+
  '      If InQuote then'#013#010+
  '         LastQuote:=S[i]'#013#010+
  '       else'#013#010+
  '         LastQuote:=#0;'#013#010+
  '      end;'#013#010+
  '    Inc(I);'#013#010+
  '    end;'#013#010+
  '  Exe:=Copy(S,1,I-1);'#013#010+
  '  Delete(S,1,I);'#013#010+
  '  Opti','ons:=Trim(S);'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure AddCustomFpmakeCommandlineOption(const ACommandLineOption, H'+
  'elpMessage : string);'#013#010+
  'begin'#013#010+
  '  if not assigned(CustomFpmakeCommandlineOptions) then'#013#010+
  '    CustomFpmakeCommandlineOptions := TStringList.Create;'#013#010+
  '  ','CustomFpmakeCommandlineOptions.Values[ACommandLineOption]:=HelpMes'+
  'sage;'#013#010+
  'end;'#013#010+
  #013#010+
  'function GetCustomFpmakeCommandlineOptionValue(const ACommandLineOptio'+
  'n: string): string;'#013#010+
  'begin'#013#010+
  '  if not assigned(CustomFpMakeCommandlineValues) then'#013#010+
  '    re','sult := '#039#039#013#010+
  '  else'#013#010+
  '    result := CustomFpMakeCommandlineValues.Values[ACommandLineOption]'+
  ';'#013#010+
  'end;'#013#010+
  #013#010+
  'function AddProgramExtension(const ExecutableName: string; AOS : TOS):'+
  ' string;'#013#010+
  'begin'#013#010+
  '  if AOS in [Go32v2,Win32,Win64,Wince,OS2,EMX,Watcom','] then'#013#010+
  '    Result:=ExecutableName+ExeExt'#013#010+
  '  else'#013#010+
  '    Result:=ExecutableName;'#013#010+
  'end;'#013#010+
  #013#010+
  'function GetImportLibraryFilename(const UnitName: string; AOS: TOS): s'+
  'tring;'#013#010+
  'begin'#013#010+
  '  if AOS in [go32v2,watcom] then'#013#010+
  '    Result := '#039'libimp'#039'+UnitName'#013#010,
  '  else if AOS in [os2,emx] then'#013#010+
  '    Result := UnitName'#013#010+
  '  else if AOS in [netware,netwlibc,macos] then'#013#010+
  '    Result := '#039'lib'#039'+UnitName'#013#010+
  '  else'#013#010+
  '    Result := '#039'libimp'#039'+UnitName;'#013#010+
  '  Result := Result + LibExt;'#013#010+
  'end;'#013#010+
  #013#010+
  'Function OptionListToStri','ng(L : TStrings) : String;'#013#010+
  #013#010+
  'var'#013#010+
  '  I : Integer;'#013#010+
  '  S : String;'#013#010+
  #013#010+
  'begin'#013#010+
  '  Result:='#039#039';'#013#010+
  '  For I:=0 to L.Count-1 do'#013#010+
  '    begin'#013#010+
  '    If (Result<>'#039#039') then'#013#010+
  '      Result:=Result+'#039' '#039';'#013#010+
  '    S:=L[I];'#013#010+
  '    If (Pos('#039' '#039',S)<>0) or (S='#039#039') then'#013#010+
  '     ',' Result:= Result + '#039'"'#039'+S+'#039'"'#039#013#010+
  '    else'#013#010+
  '      Result:= Result + S;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  'Function OptionsToStringList(S : String) : TStrings;'#013#010+
  #013#010+
  'Var'#013#010+
  '  P : Integer;'#013#010+
  #013#010+
  'begin'#013#010+
  '  Result:=Nil;'#013#010+
  '  If (S='#039#039') then'#013#010+
  '    Exit;'#013#010+
  '  Result:=TStringList.C','reate;'#013#010+
  '  Repeat'#013#010+
  '    P:=Pos('#039' '#039',S);'#013#010+
  '    If P=0 then'#013#010+
  '      P:=Length(S)+1;'#013#010+
  '    Result.Add(Copy(S,1,P-1));'#013#010+
  '    Delete(S,1,P);'#013#010+
  '    S:=Trim(S);'#013#010+
  '  Until Length(S)=0;'#013#010+
  '  If Result.Count=0 then'#013#010+
  '    FreeAndNil(Result);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{$ifdef HA','S_UNIT_PROCESS}'#013#010+
  'function GetCompilerInfo(const ACompiler,AOptions:string; ReadStdErr: '+
  'boolean):string;'#013#010+
  'const'#013#010+
  '  BufSize = 1024;'#013#010+
  'var'#013#010+
  '  S: TProcess;'#013#010+
  '  Buf: array [0..BufSize - 1] of char;'#013#010+
  '  Count: longint;'#013#010+
  'begin'#013#010+
  '  S:=TProcess.Create(N','il);'#013#010+
  '  S.Commandline:=ACompiler+'#039' '#039'+AOptions;'#013#010+
  '  S.Options:=[poUsePipes];'#013#010+
  '  S.execute;'#013#010+
  '  Count:=s.output.read(buf,BufSize);'#013#010+
  '  if (count=0) and ReadStdErr then'#013#010+
  '    Count:=s.Stderr.read(buf,BufSize);'#013#010+
  '  S.Free;'#013#010+
  '  SetLength(Result,Count)',';'#013#010+
  '  Move(Buf,Result[1],Count);'#013#010+
  'end;'#013#010+
  '{$endif HAS_UNIT_PROCESS}'#013#010+
  #013#010+
  'function GetDefaultLibGCCDir(CPU : TCPU;OS: TOS; var ErrorMessage: str'+
  'ing): string;'#013#010+
  #013#010+
  '  function Get4thWord(const AString: string): string;'#013#010+
  '  var p: pchar;'#013#010+
  '      spaceco','unt: integer;'#013#010+
  '      StartWord: pchar;'#013#010+
  '  begin'#013#010+
  '    result:='#039#039';'#013#010+
  '    if length(AString)>6 then'#013#010+
  '      begin'#013#010+
  '      p := @AString[1];'#013#010+
  '      spacecount:=0;'#013#010+
  '      StartWord:=nil;'#013#010+
  '      while (not (p^ in [#0,#10,#13])) and ((p^<>'#039' '#039') or (St','a'+
  'rtWord=nil)) do'#013#010+
  '        begin'#013#010+
  '        if p^='#039' '#039' then'#013#010+
  '          begin'#013#010+
  '          inc(spacecount);'#013#010+
  '          if spacecount=3 then StartWord:=p+1;'#013#010+
  '          end;'#013#010+
  '        inc(p);'#013#010+
  '        end;'#013#010+
  '      if StartWord<>nil then'#013#010+
  '        begin',#013#010+
  '        SetLength(result,p-StartWord);'#013#010+
  '        move(StartWord^,result[1],p-StartWord);'#013#010+
  '        end'#013#010+
  '      else'#013#010+
  '        result := '#039#039';'#013#010+
  '      end;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  function GetGccDirArch(const ACpuType, GCCParams: string) : string;'#013+
  #010+
  '  var Ex','ecResult: string;'#013#010+
  '      libgccFilename: string;'#013#010+
  '      GccExecutable: string;'#013#010+
  '  begin'#013#010+
  '    result := '#039#039';'#013#010+
  '    GccExecutable := ExeSearch(AddProgramExtension('#039'gcc'#039', OS),'+
  'GetEnvironmentVariable('#039'PATH'#039'));'#013#010+
  '    if FileExists(GccExecutable) the','n'#013#010+
  '      begin'#013#010+
  '{$ifdef HAS_UNIT_PROCESS}'#013#010+
  '      ExecResult:=GetCompilerInfo(GccExecutable,'#039'-v '#039'+GCCParam'+
  's, True);'#013#010+
  '      libgccFilename:=Get4thWord(ExecResult);'#013#010+
  '      if libgccFilename='#039#039' then'#013#010+
  '        libgccFilename:=GetCompilerInfo(GccE','xecutable,'#039'--print-l'+
  'ibgcc-file-name '#039'+GCCParams, False);'#013#010+
  '      result := ExtractFileDir(libgccFilename);'#013#010+
  '{$else HAS_UNIT_PROCESS}'#013#010+
  '      ErrorMessage := SWarnNoFCLProcessSupport;'#013#010+
  '{$endif HAS_UNIT_PROCESS}'#013#010+
  '      end'#013#010+
  '    else'#013#010+
  '      Erro','rMessage := SWarngccNotFound;'#013#010+
  '  end;'#013#010+
  #013#010+
  'begin'#013#010+
  '  result := '#039#039';'#013#010+
  '  ErrorMessage:='#039#039';'#013#010+
  '  if OS in [freebsd, openbsd] then'#013#010+
  '    result := '#039'/usr/local/lib'#039#013#010+
  '  else if OS = netbsd then'#013#010+
  '    result := '#039'/usr/pkg/lib'#039#013#010+
  '  else if OS = linux then'#013#010+
  ' ','   case CPU of'#013#010+
  '      i386:     result := GetGccDirArch('#039'cpui386'#039','#039'-m32'#039+
  ');'#013#010+
  '      x86_64:   result := GetGccDirArch('#039'cpux86_64'#039','#039'-m64'#039+
  ');'#013#010+
  '      powerpc:  result := GetGccDirArch('#039'cpupowerpc'#039','#039'-m32'+
  #039');'#013#010+
  '      powerpc64:result := GetGccDirArch','('#039'cpupowerpc64'#039','#039'-'+
  'm64'#039');'#013#010+
  '    end {case}'#013#010+
  '  else if OS = darwin then'#013#010+
  '    case CPU of'#013#010+
  '      i386:     result := GetGccDirArch('#039'cpui386'#039','#039'-arch i'+
  '386'#039');'#013#010+
  '      x86_64:   result := GetGccDirArch('#039'cpux86_64'#039','#039'-arch'+
  ' x86_64'#039');'#013#010+
  '      powerpc:  r','esult := GetGccDirArch('#039'cpupowerpc'#039','#039'-a'+
  'rch ppc'#039');'#013#010+
  '      powerpc64:result := GetGccDirArch('#039'cpupowerpc64'#039','#039'-a'+
  'rch ppc64'#039');'#013#010+
  '    end; {case}'#013#010+
  'end;'#013#010+
  #013#010+
  'constructor TPackageVariant.Create(ACollection: TCollection);'#013#010+
  'begin'#013#010+
  '  inherited Create(ACo','llection);'#013#010+
  '  FTargets := TTargets.Create(TTarget);'#013#010+
  '  FOptions := TStringList.Create;'#013#010+
  'end;'#013#010+
  #013#010+
  'destructor TPackageVariant.Destroy;'#013#010+
  'begin'#013#010+
  '  FOptions.Free;'#013#010+
  '  FTargets.Free;'#013#010+
  '  inherited Destroy;'#013#010+
  'end;'#013#010+
  #013#010+
  '{ TPackageVariants }'#013#010+
  #013#010+
  'procedure ','TPackageVariants.SetDefaultPackageVariantName(AValue: stri'+
  'ng);'#013#010+
  'begin'#013#010+
  '  if FDefaultPackageVariantName=AValue then Exit;'#013#010+
  '  if not assigned(ItemByName(avalue)) then'#013#010+
  '    raise exception.CreateFmt(SErrPackVarNotExist,[AValue]);'#013#010+
  '  FDefaultPa','ckageVariantName:=AValue;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TPackageVariants.GetActivePackageVariant: TPackageVariant;'#013+
  #010+
  'begin'#013#010+
  '  result := ItemByName(ActivePackageVariantName) as TPackageVariant;'#013+
  #010+
  'end;'#013#010+
  #013#010+
  'function TPackageVariants.GetDefaultPackageVariant:',' TPackageVariant;'+
  #013#010+
  'begin'#013#010+
  '  result := ItemByName(DefaultPackageVariantName) as TPackageVariant;'#013+
  #010+
  'end;'#013#010+
  #013#010+
  'procedure TPackageVariants.SetActivePackageVariantName(AValue: string)'+
  ';'#013#010+
  'begin'#013#010+
  '  if FActivePackageVariantName=AValue then Exit;'#013#010+
  '  if ','not assigned(ItemByName(avalue)) then'#013#010+
  '    raise exception.CreateFmt(SErrPackVarNotExist,[AValue]);'#013#010+
  '  FActivePackageVariantName:=AValue;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TPackageVariants.Add(AName: String): TPackageVariant;'#013#010+
  'begin'#013#010+
  '  result := self.add a','s TPackageVariant;'#013#010+
  '  result.Name := AName;'#013#010+
  '  if FDefaultPackageVariantName='#039#039' then'#013#010+
  '    FDefaultPackageVariantName:=AName;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{ TConditionalDestStrings }'#013#010+
  #013#010+
  'function TConditionalDestStrings.GetConditionalString(Index : Integer)'+
  ': T','ConditionalDestString;'#013#010+
  'begin'#013#010+
  '  Result:=TConditionalDestString(Items[Index]);'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TConditionalDestStrings.SetConditionalString(Index : Integer'+
  '; AValue: TConditionalDestString);'#013#010+
  'begin'#013#010+
  '  Items[Index]:=AValue;'#013#010+
  'end;'#013#010+
  #013#010+
  'constr','uctor TConditionalDestStrings.Create(AClass: TConditionalDestS'+
  'tringClass);'#013#010+
  'begin'#013#010+
  '  inherited Create(AClass);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TConditionalDestStrings.Add(const Value: String; ADestPath: S'+
  'tring): TConditionalDestString;'#013#010+
  'begin'#013#010+
  '  Add(Valu','e, AllCPUs, AllOSes, ADestPath);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TConditionalDestStrings.Add(const Value: String; const OSes: '+
  'TOSes; ADestPath: String): TConditionalDestString;'#013#010+
  'begin'#013#010+
  '  Add(Value, AllCPUs, OSes, ADestPath);'#013#010+
  'end;'#013#010+
  #013#010+
  '{$ifdef cpu_only_ove','rloads}'#013#010+
  'Function TConditionalDestStrings.Add(Const Value : String;const CPUs:T'+
  'CPUs; ADestPath: String) : TConditionalDestString;inline;'#013#010+
  'begin'#013#010+
  '  Add(Value, CPUs, AllOSes, ADestPath);'#013#010+
  'end;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  #013#010+
  #013#010+
  'function TCondi','tionalDestStrings.Add(const Value: String; const CPUs'+
  ': TCPUs; const OSes: TOSes; ADestPath: String): TConditionalDestString'+
  ';'#013#010+
  'var'#013#010+
  '  ACondString: TConditionalDestString;'#013#010+
  'begin'#013#010+
  '  ACondString := inherited Add(Value,CPUs,OSes) as TConditiona','lDestS'+
  'tring;'#013#010+
  '  ACondString.DestPath:=ADestPath;'#013#010+
  'end;'#013#010+
  #013#010+
  '{ TPackageDictionary }'#013#010+
  #013#010+
  'function TPackageDictionary.GetValue(const AName, Args: String): Strin'+
  'g;'#013#010+
  'Var'#013#010+
  '  O : TObject;'#013#010+
  '  I : Integer;'#013#010+
  'begin'#013#010+
  '  I:=Flist.IndexOf(AName);'#013#010+
  '  If (I=-1',') then'#013#010+
  '    begin'#013#010+
  '      if assigned(MasterDictionary) then'#013#010+
  '        result := MasterDictionary.GetValue(AName,Args)'#013#010+
  '      else'#013#010+
  '        result := GlobalDictionary.GetValue(AName,Args);'#013#010+
  '      Exit;'#013#010+
  '    end;'#013#010+
  '  O:=Flist.Objects[I];'#013#010+
  '  If ','O is TValueItem then'#013#010+
  '    Result:=TValueItem(O).FValue'#013#010+
  '  else'#013#010+
  '    Result:=TFunctionItem(O).FFunc(AName,Args);'#013#010+
  'end;'#013#010+
  #013#010+
  '{$ifndef NO_THREADING}'#013#010+
  #013#010+
  '{ TCompileWorkerThread }'#013#010+
  #013#010+
  'constructor TCompileWorkerThread.Create(ABuildEngine: TBuildEngin','e; '+
  'NotifyMainThreadEvent: PRTLEvent);'#013#010+
  'begin'#013#010+
  '  inherited Create(false);'#013#010+
  '  FNotifyStartTask := RTLEventCreate;'#013#010+
  '  FBuildEngine := ABuildEngine;'#013#010+
  '  FNotifyMainThreadEvent:=NotifyMainThreadEvent;'#013#010+
  'end;'#013#010+
  #013#010+
  'destructor TCompileWorkerThread.Dest','roy;'#013#010+
  'begin'#013#010+
  '  RTLeventdestroy(FNotifyStartTask);'#013#010+
  '  inherited Destroy;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TCompileWorkerThread.execute;'#013#010+
  'begin'#013#010+
  '  while not Terminated do'#013#010+
  '    begin'#013#010+
  '    FDone:=true;'#013#010+
  '    RTLeventSetEvent(FNotifyMainThreadEvent);'#013#010+
  '    RTLe','ventWaitFor(FNotifyStartTask,500);'#013#010+
  '    if not FDone then'#013#010+
  '      begin'#013#010+
  '      FBuildEngine.log(vlInfo,'#039'Compiling: '#039'+APackage.Name);'#013+
  #010+
  '      FCompilationOK:=false;'#013#010+
  '      try'#013#010+
  '        FBuildEngine.Compile(APackage);'#013#010+
  '        FCompilationOK:=t','rue;'#013#010+
  '      except'#013#010+
  '        on E: Exception do'#013#010+
  '          FErrorMessage := E.Message;'#013#010+
  '      end;'#013#010+
  '      end;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  '{$endif NO_THREADING}'#013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '       ','                    TUnsortedDuplicatesStringList'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'function TUnsortedDuplicatesStringList.Add(const S: string): Integer;'#013+
  #010+
  #013#010+
  'begin'#013#010+
  '  result := IndexOf(S);'#013#010+
  '  I','f result > -1 then'#013#010+
  '    Case DUplicates of'#013#010+
  '      DupIgnore : Exit;'#013#010+
  '      DupError : Error(SDuplicateString,0)'#013#010+
  '    end;'#013#010+
  '  inherited Add(S);'#013#010+
  'end;'#013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '         ','                       TNamedItem'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'procedure TNamedItem.SetName(const AValue: String);'#013#010+
  #013#010+
  'begin'#013#010+
  '  if FName=AValue then exit;'#013#010+
  '  With TNamedCollection(Collecti','on) do'#013#010+
  '    If UniqueNames then'#013#010+
  '      If (IndexOfName(AVAlue)<>-1) then'#013#010+
  '        Raise ECollectionError.CreateFmt(SErrNameExists,[AValue]);'#013+
  #010+
  '  FName:=AValue;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************',
  '*******'#013#010+
  '                                TNamedCollection'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'function TNamedCollection.GetItem(Index: Integer): TNamedItem;'#013#010+
  'begin'#013#010+
  '  result := TNamedItem(inheri','ted getItem(index));'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TNamedCollection.SetItem(Index: Integer; AValue: TNamedItem)'+
  ';'#013#010+
  'begin'#013#010+
  '  inherited SetItem(Index, AValue);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TNamedCollection.IndexOfName(const AName: String): Integer;'#013+
  #010+
  #013#010+
  'begin'#013#010+
  '  Resu','lt:=Count-1;'#013#010+
  '  While (Result>=0) and (CompareText(TNamedItem(Items[Result]).FName,A'+
  'Name)<>0) do'#013#010+
  '    Dec(Result);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TNamedCollection.ItemByName(const AName: String): TNamedItem;'+
  #013#010+
  #013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  #013#010+
  'begin'#013#010+
  '  I:=IndexOf','Name(AName);'#013#010+
  '  If (I=-1) Then'#013#010+
  '    Raise ECollectionError.CreateFmt(SErrNoSuchName,[AName]);'#013#010+
  '  Result:=TNamedItem(Items[i]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                       ','      TNamedItemList'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'function TNamedItemList.GetNamedItem(Index : Integer): TNamedItem;'#013+
  #010+
  'begin'#013#010+
  '  Result:=TNamedItem(Items[Index]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TName','dItemList.SetNamedItem(Index : Integer; const AValue:'+
  ' TNamedItem);'#013#010+
  'begin'#013#010+
  '  Items[Index]:=AValue;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TNamedItemList.IndexOfName(const AName: String): Integer;'#013+
  #010+
  'begin'#013#010+
  '  Result:=Count-1;'#013#010+
  '  While (Result>=0) and (CompareTex','t(GetNamedItem(Result).Name,AName'+
  ')<>0) do'#013#010+
  '    Dec(Result);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TNamedItemList.ItemByName(const ANAme: String): TNamedItem;'#013+
  #010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  'begin'#013#010+
  '  I:=IndexOfName(AName);'#013#010+
  '  If (I=-1) Then'#013#010+
  '    Raise ECollectionError.','CreateFmt(SErrNoSuchName,[AName]);'#013#010+
  '  Result:=TNamedItem(Items[i]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                                TTargets'#013#010+
  '****************************************','****************************'+
  '********}'#013#010+
  #013#010+
  'function TTargets.GetTargetItem(Index : Integer): TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=TTarget(Items[Index]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TTargets.GetTarget(const AName : String): TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=TTarget(It','emByName(AName));'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TTargets.SetTargetItem(Index : Integer; const AValue: TTarge'+
  't);'#013#010+
  'begin'#013#010+
  '  Items[Index]:=AValue;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TTargets.AddFPDoc(const AUnitName, AXMLName: String): TTarget'+
  ';'#013#010+
  'begin'#013#010+
  '  Result:=Add as',' TTarget;'#013#010+
  '  Result.Name:=AUnitName;'#013#010+
  '  Result.XML:=AXMLName;'#013#010+
  '  Result.TargetType:=ttFPDoc;'#013#010+
  'end;'#013#010+
  #013#010+
  'Function TTargets.AddUnit(Const AUnitName : String) : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=AddUnit(AUnitName,AllCPUs,AllOSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function T','Targets.AddUnit(Const AUnitName : String;const OSes:TOSes)'+
  ' : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=AddUnit(AUnitName,AllCPUs,OSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{$ifdef cpu_only_overloads}'#013#010+
  'Function TTargets.AddUnit(Const AUnitName : String;const CPUs:TCPUs) :'+
  ' TTarget;',#013#010+
  'begin'#013#010+
  '  Result:=AddUnit(AUnitName,CPUs,AllOSes);'#013#010+
  'end;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  #013#010+
  #013#010+
  'Function TTargets.AddUnit(Const AUnitName : String;const CPUs:TCPUs;co'+
  'nst OSes:TOSes) : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=Add as TTarget;'#013#010+
  '  Result.Name:=A','UnitName;'#013#010+
  '  Result.TargetType:=TTUnit;'#013#010+
  '  Result.CPUs:=CPUs;'#013#010+
  '  Result.OSes:=OSes;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TTargets.AddImplicitUnit(Const AUnitName : String;InstallUnit'+
  ':boolean=true) : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=AddImplicitUnit(AUnitName,AllCP','Us,AllOSes,InstallUnit);'#013+
  #010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TTargets.AddImplicitUnit(Const AUnitName : String;const OSes:'+
  'TOSes;InstallUnit:boolean=true) : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=AddImplicitUnit(AUnitName,AllCPUs,OSes,InstallUnit);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function ','TTargets.AddImplicitUnit(Const AUnitName : String;const CPU'+
  's:TCPUs;InstallUnit:boolean=true) : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=AddImplicitUnit(AUnitName,CPUs,AllOSes,InstallUnit);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TTargets.AddImplicitUnit(Const AUnitName : St','ring;const CPU'+
  's:TCPUs;const OSes:TOSes;InstallUnit:boolean=true) : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=Add as TTarget;'#013#010+
  '  Result.Name:=AUnitName;'#013#010+
  '  Result.CPUs:=CPUs;'#013#010+
  '  Result.OSes:=OSes;'#013#010+
  '  if InstallUnit then'#013#010+
  '    Result.TargetType:=TTImplicitUn','it'#013#010+
  '  else'#013#010+
  '    Result.TargetType:=TTCleanOnlyUnit;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TTargets.AddProgram(Const AProgramName : String) : TTarget;'#013+
  #010+
  'begin'#013#010+
  '  Result:=AddProgram(AProgramName,AllCPUs,AllOSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TTargets.AddProgram(Const AP','rogramName : String;const OSes:'+
  'TOSes) : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=AddProgram(AProgramName,AllCPUs,OSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{$ifdef cpu_only_overloads}'#013#010+
  'Function TTargets.AddProgram(Const AProgramName : String;const CPUs:TC'+
  'PUs) : TTarget;'#013#010+
  'begin'#013#010,
  '  Result:=AddProgram(AProgramName,CPUs,AllOSes);'#013#010+
  'end;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  #013#010+
  #013#010+
  'Function TTargets.AddProgram(Const AProgramName : String;const CPUs:TC'+
  'PUs;const OSes:TOSes) : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=Add as TTarget;'#013#010+
  '  Result.Name',':=AProgramName;'#013#010+
  '  Result.CPUs:=CPUs;'#013#010+
  '  Result.OSes:=OSes;'#013#010+
  '  Result.TargetType:=ttProgram;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TTargets.AddExampleUnit(Const AUnitName : String) : TTarget;'#013+
  #010+
  'begin'#013#010+
  '  Result:=AddExampleUnit(AUnitName,AllCPUs,AllOSes);'#013#010+
  'end;',#013#010+
  #013#010+
  #013#010+
  'Function TTargets.AddExampleUnit(Const AUnitName : String;const OSes:T'+
  'OSes) : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=AddExampleUnit(AUnitName,AllCPUs,OSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{$ifdef cpu_only_overloads}'#013#010+
  'Function TTargets.AddExampleUnit(Const AUnitName ',': String;const CPUs'+
  ':TCPUs) : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=AddExampleUnit(AUnitName,CPUs,AllOSes);'#013#010+
  'end;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  #013#010+
  #013#010+
  'Function TTargets.AddExampleUnit(Const AUnitName : String;const CPUs:T'+
  'CPUs;const OSes:TOSes) : TTarget;'#013,#010+
  'begin'#013#010+
  '  Result:=Add as TTarget;'#013#010+
  '  Result.Name:=AUnitName;'#013#010+
  '  Result.CPUs:=CPUs;'#013#010+
  '  Result.OSes:=OSes;'#013#010+
  '  Result.TargetType:=ttExampleUnit;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TTargets.AddExampleProgram(Const AProgramName : String) : TTa'+
  'rget;'#013#010+
  'begin'#013#010+
  '  R','esult:=AddExampleProgram(AProgramName,AllCPUs,AllOSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TTargets.AddExampleProgram(Const AProgramName : String;const '+
  'OSes:TOSes) : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=AddExampleProgram(AProgramName,AllCPUs,OSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{$ifd','ef cpu_only_overloads}'#013#010+
  'Function TTargets.AddExampleProgram(Const AProgramName : String;const '+
  'CPUs:TCPUs) : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=AddExampleProgram(AProgramName,CPUs,AllOSes);'#013#010+
  'end;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  #013#010+
  #013#010+
  'Function TTargets.A','ddExampleProgram(Const AProgramName : String;cons'+
  't CPUs:TCPUs;const OSes:TOSes) : TTarget;'#013#010+
  'begin'#013#010+
  '  Result:=Add as TTarget;'#013#010+
  '  Result.Name:=AProgramName;'#013#010+
  '  Result.CPUs:=CPUs;'#013#010+
  '  Result.OSes:=OSes;'#013#010+
  '  Result.TargetType:=ttExampleProgram;'#013#010,
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                                TSources'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'function TSources.GetSourceI','tem(Index : Integer): TSource;'#013#010+
  'begin'#013#010+
  '  Result:=TSource(Items[Index]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TSources.SetSourceItem(Index : Integer; const AValue: TSourc'+
  'e);'#013#010+
  'begin'#013#010+
  '  Items[Index]:=AValue;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TSources.AddDoc (const AFiles :',' String) : TSource;'#013#010+
  'begin'#013#010+
  '  Result:=Add as TSource;'#013#010+
  '  Result.Name:=AFiles;'#013#010+
  '  Result.FSourceType:=stDoc;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TSources.AddDoc(const AFiles: String; AInstallSourcePath: Str'+
  'ing): TSource;'#013#010+
  'begin'#013#010+
  '  Result:=Add as TSource;'#013#010+
  ' ',' Result.Name:=AFiles;'#013#010+
  '  Result.FInstallSourcePath:=AInstallSourcePath;'#013#010+
  '  Result.FSourceType:=stDoc;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TSources.AddSrc(const AFiles : String) : TSource;'#013#010+
  'begin'#013#010+
  '  Result:=Add as TSource;'#013#010+
  '  Result.Name:=AFiles;'#013#010+
  '  Result.','FSourceType:=stSrc;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TSources.AddExample(const AFiles : String) : TSource;'#013#010+
  'begin'#013#010+
  '  Result:=Add as TSource;'#013#010+
  '  Result.Name:=AFiles;'#013#010+
  '  Result.FSourceType:=stExample;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TSources.AddExample(const AFiles: St','ring; AInstallSourcePat'+
  'h: String): TSource;'#013#010+
  'begin'#013#010+
  '  Result:=Add as TSource;'#013#010+
  '  Result.Name:=AFiles;'#013#010+
  '  Result.FInstallSourcePath:=AInstallSourcePath;'#013#010+
  '  Result.FSourceType:=stExample;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TSources.AddTest(const AFiles : St','ring) : TSource;'#013#010+
  'begin'#013#010+
  '  Result:=Add as TSource;'#013#010+
  '  Result.Name:=AFiles;'#013#010+
  '  Result.FSourceType:=stTest;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TSources.AddDocFiles(const AFileMask: string; Recursive: boo'+
  'lean; AInstallSourcePath : String = '#039#039');'#013#010+
  'var'#013#010+
  '  List',' : TStrings;'#013#010+
  '  i: integer;'#013#010+
  'begin'#013#010+
  '  List := TStringList.Create;'#013#010+
  '  SearchFiles(AFileMask, Recursive, List);'#013#010+
  '  for i:= 0 to Pred(List.Count) do'#013#010+
  '    AddDoc(List[i], AInstallSourcePath);'#013#010+
  '  List.Free;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TSources.AddSrcFi','les(const AFileMask: string; Recursive: b'+
  'oolean);'#013#010+
  'var'#013#010+
  '  List : TStrings;'#013#010+
  '  i: integer;'#013#010+
  'begin'#013#010+
  '  List := TStringList.Create;'#013#010+
  '  SearchFiles(AFileMask, Recursive, List);'#013#010+
  '  for i:= 0 to Pred(List.Count) do'#013#010+
  '    AddSrc(List[i]);'#013#010+
  '  List.Fr','ee;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TSources.AddExampleFiles(const AFileMask: string; Recursive:'+
  ' boolean; AInstallSourcePath : String = '#039#039');'#013#010+
  'var'#013#010+
  '  List : TStrings;'#013#010+
  '  i: integer;'#013#010+
  'begin'#013#010+
  '  List := TStringList.Create;'#013#010+
  '  SearchFiles(AFileMask, Recurs','ive, List);'#013#010+
  '  for i:= 0 to Pred(List.Count) do'#013#010+
  '    AddExample(List[i], AInstallSourcePath);'#013#010+
  '  List.Free;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TSources.AddTestFiles(const AFileMask: string; Recursive: bo'+
  'olean);'#013#010+
  'var'#013#010+
  '  List : TStrings;'#013#010+
  '  i: integer;'#013#010+
  'be','gin'#013#010+
  '  List := TStringList.Create;'#013#010+
  '  SearchFiles(AFileMask, Recursive, List);'#013#010+
  '  for i:= 0 to Pred(List.Count) do'#013#010+
  '    AddTest(List[i]);'#013#010+
  '  List.Free;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******',#013#010+
  '                             TPackage'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'constructor TPackage.Create(ACollection: TCollection);'#013#010+
  'begin'#013#010+
  '  inherited Create(ACollection);'#013#010+
  '  FVersion:=TFPVersio','n.Create;'#013#010+
  '  FTargets:=TTargets.Create(TTarget);'#013#010+
  '  FSources:=TSources.Create(TSource);'#013#010+
  '  FDependencies:=TDependencies.Create(TDependency);'#013#010+
  '  FInstallFiles:=TConditionalDestStrings.Create(TConditionalDestString'+
  ');'#013#010+
  '  FCleanFiles:=TConditio','nalStrings.Create(TConditionalString);'#013#010+
  '  FUnitPath:=TConditionalStrings.Create(TConditionalString);'#013#010+
  '  FObjectPath:=TConditionalStrings.Create(TConditionalString);'#013#010+
  '  FIncludePath:=TConditionalStrings.Create(TConditionalString);'#013#010+
  '  FSource','Path:=TConditionalStrings.Create(TConditionalString);'#013#010+
  '  FExamplePath:=TConditionalStrings.Create(TConditionalString);'#013#010+
  '  FTestPath:=TConditionalStrings.Create(TConditionalString);'#013#010+
  '  FCommands:=TCommands.Create(TCommand);'#013#010+
  '  FUnitsOutputDi','r:='#039'units'#039'+PathDelim+'#039'$(target)'#039'+Pat'+
  'hDelim;'#013#010+
  '  FPackageVariants:=TFPList.Create;'#013#010+
  '  FCPUs:=AllCPUs;'#013#010+
  '  FOSes:=AllOSes;'#013#010+
  '  FInstalledChecksum:=$ffffffff;'#013#010+
  '  FFlags := TStringList.Create;'#013#010+
  '  // Implicit dependency on RTL'#013#010+
  '  FDependencies.Add(',#039'rtl'#039');'#013#010+
  '  FSupportBuildModes:=[bmBuildUnit, bmOneByOne];'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'destructor TPackage.destroy;'#013#010+
  'var'#013#010+
  '  i: integer;'#013#010+
  'begin'#013#010+
  '  FreeAndNil(FDictionary);'#013#010+
  '  FreeAndNil(FDependencies);'#013#010+
  '  FreeAndNil(FInstallFiles);'#013#010+
  '  FreeAndNil(FCleanFiles);',#013#010+
  '  FreeAndNil(FIncludePath);'#013#010+
  '  FreeAndNil(FSourcePath);'#013#010+
  '  FreeAndNil(FExamplePath);'#013#010+
  '  FreeAndNil(FTestPath);'#013#010+
  '  FreeAndNil(FCommands);'#013#010+
  '  FreeAndNil(FObjectPath);'#013#010+
  '  FreeAndNil(FUnitPath);'#013#010+
  '  FreeAndNil(FSources);'#013#010+
  '  FreeAndNil(FTargets',');'#013#010+
  '  FreeAndNil(FVersion);'#013#010+
  '  FreeAndNil(FOptions);'#013#010+
  '  FreeAndNil(FFlags);'#013#010+
  '  FreeAndNil(FPackageVariants);'#013#010+
  '  inherited destroy;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TPackage.HaveOptions: Boolean;'#013#010+
  'begin'#013#010+
  '  Result:=(FOptions<>Nil);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TPac','kage.SetName(const AValue: String);'#013#010+
  'begin'#013#010+
  '  inherited SetName(AValue);'#013#010+
  '  // RTL should not have any dependencies'#013#010+
  '  if AValue='#039'rtl'#039' then'#013#010+
  '    FDependencies.Clear;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TPackage.GetUnitsOutputDir(ACPU:TCPU; AOS : TOS):Strin','g;'#013+
  #010+
  'begin'#013#010+
  '  result:=FixPath(Dictionary.Substitute(FUnitsOutputDir,['#039'CPU'#039','+
  'CPUToString(ACPU),'#039'OS'#039',OSToString(AOS),'#039'target'#039',MakeTa'+
  'rgetString(ACPU,AOS)]), False);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TPackage.GetUnitConfigOutputDir(ACPU: TCPU; AOS: TOS): String'+
  ';',#013#010+
  'begin'#013#010+
  '  result:=FixPath(Dictionary.Substitute('#039'units'#039'+PathDelim+'#039+
  '$(target)'#039'+PathDelim,['#039'CPU'#039',CPUToString(ACPU),'#039'OS'#039','+
  'OSToString(AOS),'#039'target'#039',MakeTargetString(ACPU,AOS)]), False);'+
  #013#010+
  'end;'#013#010+
  #013#010+
  'procedure TPackage.InheritPackageVariantsFromDepe','ndency(ADependencyP'+
  'ackage: TPackage);'#013#010+
  'var'#013#010+
  '  i: integer;'#013#010+
  '  APackageVariants: TPackageVariants;'#013#010+
  'begin'#013#010+
  '  for i := 0 to ADependencyPackage.FPackageVariants.Count-1 do'#013#010+
  '    begin'#013#010+
  '      APackageVariants := TPackageVariants(ADependencyPackag','e.FPacka'+
  'geVariants[i]);'#013#010+
  '      if APackageVariants.IsInheritable then'#013#010+
  '        begin'#013#010+
  '        if not assigned(GetPackageVariantsByName(APackageVariants.Name'+
  ')) then'#013#010+
  '          begin'#013#010+
  '          FPackageVariants.Add(APackageVariants);'#013#010+
  '       ','   end;'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TPackage.GetPackageVariantsByName(AName: string): TPackageVar'+
  'iants;'#013#010+
  'var'#013#010+
  '  i: Integer;'#013#010+
  'begin'#013#010+
  '  result := nil;'#013#010+
  '  for i := 0 to FPackageVariants.Count-1 do'#013#010+
  '    if SameText(TPackageVariant','s(FPackageVariants.Items[i]).Name, AN'+
  'ame) then'#013#010+
  '      begin'#013#010+
  '      result := TPackageVariants(FPackageVariants.Items[i]);'#013#010+
  '      break;'#013#010+
  '      end;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TPackage.SetUnitsOutputDir(AValue: string);'#013#010+
  'begin'#013#010+
  '  if AValue<>'#039#039' then'#013,#010+
  '    FUnitsOutputDir:=IncludeTrailingPathDelimiter(AValue)'#013#010+
  '  else'#013#010+
  '    FUnitsOutputDir:='#039#039';'#013#010+
  'end;'#013#010+
  #013#010+
  'function TPackage.GetPackageUnitInstallDir(ACPU: TCPU; AOS: TOS): Stri'+
  'ng;'#013#010+
  'begin'#013#010+
  '  result:=FixPath(Dictionary.Substitute(FPackageUnitInst','allDir,['#039+
  'CPU'#039',CPUToString(ACPU),'#039'OS'#039',OSToString(AOS),'#039'target'#039+
  ',MakeTargetString(ACPU,AOS)]), False);'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TPackage.SetPackageUnitInstallDir(AValue: string);'#013#010+
  'begin'#013#010+
  '  if AValue<>'#039#039' then'#013#010+
  '    FPackageUnitInstallDir:=IncludeTrail','ingPathDelimiter(AValue)'#013+
  #010+
  '  else'#013#010+
  '    FPackageUnitInstallDir:='#039#039';'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TPackage.GetBinOutputDir(ACPU:TCPU; AOS : TOS) : String;'#013#010+
  'begin'#013#010+
  '  Result:='#039'bin'#039'+PathDelim+MakeTargetString(ACPU,AOS);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TPackage.GetCl','eanFiles(List: TStrings; ACPU:TCPU; AOS : TO'+
  'S);'#013#010+
  'Var'#013#010+
  '  OB,OU : String;'#013#010+
  '  I : Integer;'#013#010+
  'begin'#013#010+
  '  OB:=IncludeTrailingPathDelimiter(GetBinOutputDir(ACPU,AOS));'#013#010+
  '  OU:=IncludeTrailingPathDelimiter(GetUnitsOutputDir(ACPU,AOS));'#013#010+
  '  AddCondition','alStrings(Self, List,CleanFiles,ACPU,AOS);'#013#010+
  '  For I:=0 to FTargets.Count-1 do'#013#010+
  '    FTargets.TargetItems[I].GetCleanFiles(List, OU, OB, ACPU, AOS);'#013+
  #010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TPackage.GetInstallFiles(List: TStrings;Types : TTargetTypes'+
  ';ACPU:TCPU; ','AOS : TOS);'#013#010+
  'Var'#013#010+
  '  OB,OU : String;'#013#010+
  '  I : Integer;'#013#010+
  '  T : TTarget;'#013#010+
  'begin'#013#010+
  '  if Types=[] then'#013#010+
  '    AddConditionalStrings(Self, List,InstallFiles,ACPU,AOS)'#013#010+
  '  else'#013#010+
  '    begin'#013#010+
  '      OB:=IncludeTrailingPathDelimiter(GetBinOutputDir(Defaults.','CPU,'+
  'Defaults.OS));'#013#010+
  '      OU:=IncludeTrailingPathDelimiter(GetUnitsOutputDir(Defaults.CPU,'+
  'Defaults.OS));'#013#010+
  '      For I:=0 to FTargets.Count-1 do'#013#010+
  '        begin'#013#010+
  '          T:=FTargets.TargetItems[I];'#013#010+
  '          if (T.TargetType in Types) and ','Installer.BuildEngine.Targe'+
  'tInstallOK(T, ACPU, AOS) then'#013#010+
  '            T.GetInstallFiles(List, OU, OB, ACPU, AOS);'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TPackage.GetInstallSourceFiles(List: TStrings; SourceTypes :'+
  ' TSourceTypes; Target','Types : TTargetTypes);'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  '  S : TSource;'#013#010+
  '  T : TTarget;'#013#010+
  'begin'#013#010+
  '  For I:=0 to FSources.Count-1 do'#013#010+
  '    begin'#013#010+
  '      S:=FSources.SourceItems[I];'#013#010+
  '      if (S.SourceType in SourceTypes) then'#013#010+
  '        S.GetInstallFiles(List',');'#013#010+
  '    end;'#013#010+
  '  For I:=0 to FTargets.Count-1 do'#013#010+
  '    begin'#013#010+
  '      T:=FTargets.TargetItems[I];'#013#010+
  '      if (T.TargetType in TargetTypes) then'#013#010+
  '        T.GetArchiveFiles(List,Defaults.CPU,Defaults.OS);'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TPackage.Ge','tArchiveFiles(List: TStrings; ACPU:TCPU; AOS : '+
  'TOS);'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  'begin'#013#010+
  '  // Targets only'#013#010+
  '  For I:=0 to FTargets.Count-1 do'#013#010+
  '    FTargets.TargetItems[I].GetArchiveFiles(List,ACPU,AOS);'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TPackage.GetArchiveSource','Files(List: TStrings);'#013#010+
  'var'#013#010+
  '  i : integer;'#013#010+
  'begin'#013#010+
  '  for i := 0 to Sources.Count-1 do'#013#010+
  '    List.Add(Sources[i].Name);'#013#010+
  'end;'#013#010+
  #013#010+
  'Function TPackage.GetDescription : string;'#013#010+
  'Var'#013#010+
  '  FN : String;'#013#010+
  '  L : TStringList;'#013#010+
  'begin'#013#010+
  '  If (FDescription<>',#039#039') then'#013#010+
  '    Result:=FDescription'#013#010+
  '  else'#013#010+
  '    If (FDescriptionFile<>'#039#039') then'#013#010+
  '      begin'#013#010+
  '        // Always relative to binary name.'#013#010+
  '        FN:=ExtractFilePath(ParamStr(0));'#013#010+
  '        FN:=FN+FDescriptionFile;'#013#010+
  '        If FileExists(FN) ','then'#013#010+
  '          begin'#013#010+
  '            L:=TStringList.Create;'#013#010+
  '            Try'#013#010+
  '              L.LoadFromFile(FN);'#013#010+
  '              Result:=L.Text;'#013#010+
  '            Finally'#013#010+
  '              L.Free;'#013#010+
  '            end;'#013#010+
  '          end;'#013#010+
  '      end;'#013#010+
  'end;'#013#010+
  #013#010,
  'function TPackage.GetDictionary: TDictionary;'#013#010+
  'begin'#013#010+
  '  if not assigned(FDictionary) then'#013#010+
  '    FDictionary:=PackageDictionaryClass.Create(Nil);'#013#010+
  '  result := FDictionary;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TPackage.GetVersion : string;'#013#010+
  'begin'#013#010+
  '  result:=FV','ersion.AsString;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TPackage.SetOptions(const AValue: TStrings);'#013#010+
  'begin'#013#010+
  '  If (AValue=Nil) or (AValue.Count=0) then'#013#010+
  '    FreeAndNil(Foptions)'#013#010+
  '  else'#013#010+
  '    Options.Assign(AValue);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Procedure TPackage.SetVersion(cons','t V : string);'#013#010+
  'begin'#013#010+
  '  FVersion.AsString:=V;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TPackage.GetFileName : string;'#013#010+
  'begin'#013#010+
  '  If (FFileName<>'#039#039') then'#013#010+
  '    Result:=FFileName'#013#010+
  '  else'#013#010+
  '    if not FVersion.Empty then'#013#010+
  '      Result := Name + '#039'-'#039' + FVersion.AsStri','ng'#013#010+
  '    else'#013#010+
  '      Result := Name;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TPackage.GetOptions: TStrings;'#013#010+
  'begin'#013#010+
  '  If (FOptions=Nil) then'#013#010+
  '    FOptions:=TStringList.Create;'#013#010+
  '  Result:=FOptions;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Procedure TPackage.GetManifest(Manifest : TStrings);'#013#010+
  #013,#010+
  '  procedure AddOSes(const AIndent:string;AOSes:TOSes);'#013#010+
  '  var'#013#010+
  '    IOS : TOS;'#013#010+
  '  begin'#013#010+
  '    if (AOSes=AllOSes) then'#013#010+
  '      exit;'#013#010+
  '    Manifest.Add(AIndent+'#039'<oses>'#039');'#013#010+
  '    for IOS:=low(TOSes) to high(TOSes) do'#013#010+
  '      if IOS in AOSes then'#013#010+
  ' ','       Manifest.Add(Format(AIndent+'#039' <os name="%s"/>'#039',[OSTo'+
  'String(IOS)]));'#013#010+
  '    Manifest.Add(AIndent+'#039'</oses>'#039');'#013#010+
  '  end;'#013#010+
  #013#010+
  '  procedure AddCPUs(const AIndent:string;ACPUs:TCPUs);'#013#010+
  '  var'#013#010+
  '    ICPU : TCPU;'#013#010+
  '  begin'#013#010+
  '    if (ACPUs=AllCPUs) the','n'#013#010+
  '      exit;'#013#010+
  '    Manifest.Add(AIndent+'#039'<cpus>'#039');'#013#010+
  '    for ICPU:=low(TCPUs) to high(TCPUs) do'#013#010+
  '      if ICPU in ACPUs then'#013#010+
  '        Manifest.Add(Format(AIndent+'#039' <cpu name="%s"/>'#039',[CPUTo'+
  'String(ICPU)]));'#013#010+
  '    Manifest.Add(AIndent+'#039'</cpus>'#039,');'#013#010+
  '  end;'#013#010+
  #013#010+
  '  function GetXMLVersionString(sMajor, sMinor, sMicro, sBuild: integer'+
  '): string;'#013#010+
  '  begin'#013#010+
  '    Result := '#039'<version'#039';'#013#010+
  '    if sMajor <> -1 then'#013#010+
  '      Result := Result + '#039' major="'#039' + IntToStr(sMajor) + '#039'"'+
  #039';'#013#010+
  '    if sMinor <> -1',' then'#013#010+
  '      Result := Result + '#039' minor="'#039' + IntToStr(sMinor) + '#039'"'+
  #039';'#013#010+
  '    if sMicro <> -1 then'#013#010+
  '      Result := Result + '#039' micro="'#039' + IntToStr(sMicro) + '#039'"'+
  #039';'#013#010+
  '    if sBuild <> -1 then'#013#010+
  '      Result := Result + '#039' build="'#039' + IntToStr(sBuild) ','+ '#039+
  '"'#039';'#013#010+
  #013#010+
  '    Result := Result + '#039'/>'#039';'#013#010+
  '  end;'#013#010+
  #013#010+
  'Var'#013#010+
  '  S : String;'#013#010+
  '  i : Integer;'#013#010+
  '  D : TDependency;'#013#010+
  'begin'#013#010+
  '  With Manifest do'#013#010+
  '    begin'#013#010+
  '    Add(Format('#039'<package name="%s">'#039',[QuoteXml(Name)]));'#013#010+
  #013#010+
  '    Add('#039' '#039' + GetXMLVersionString(FV','ersion.Major,FVersion.Mi'+
  'nor,FVersion.Micro,FVersion.Build));'#013#010+
  '    AddOSes('#039' '#039',OSes);'#013#010+
  '    AddCPUs('#039' '#039',CPUs);'#013#010+
  '    Add(Format('#039' <filename>%s</filename>'#039',[QuoteXml(FileName +'+
  ' ZipExt)]));'#013#010+
  '    Add(Format('#039' <author>%s</author>'#039',[QuoteXml(Author',')]));'#013+
  #010+
  '    Add(Format('#039' <license>%s</license>'#039',[QuoteXml(License)]));'+
  #013#010+
  '    if HomepageURL<>'#039#039' then'#013#010+
  '      Add(Format('#039' <homepageurl>%s</homepageurl>'#039',[QuoteXml(Ho'+
  'mepageURL)]));'#013#010+
  '    if DownloadURL<>'#039#039' then'#013#010+
  '      Add(Format('#039' <downloadurl>%','s</downloadurl>'#039',[QuoteXml('+
  'DownloadURL)]));'#013#010+
  '    Add(Format('#039' <email>%s</email>'#039',[QuoteXMl(Email)]));'#013#010+
  '    S:=Description;'#013#010+
  '    If (S<>'#039#039') then'#013#010+
  '      Add(Format('#039' <description>%s</description>'#039',[QuoteXML(S)'+
  ']));'#013#010+
  '    If (Dependencies.Count>','0) then'#013#010+
  '      begin'#013#010+
  '        Add('#039' <dependencies>'#039');'#013#010+
  '        for I:=0 to Dependencies.Count-1 do'#013#010+
  '          begin'#013#010+
  '            D:=Dependencies[i];'#013#010+
  '            Add('#039'  <dependency>'#039');'#013#010+
  '            Add(Format('#039'   <package packagename="%s"/>',#039',[Quote'+
  'XML(D.Value)]));'#013#010+
  '            if not D.FVersion.Empty then'#013#010+
  '              Add('#039'   '#039' + GetXMLVersionString(D.FVersion.Major'+
  ',D.FVersion.Minor,D.FVersion.Micro,D.FVersion.Build));'#013#010+
  '            AddOSes('#039'   '#039',D.OSes);'#013#010+
  '            AddCPU','s('#039'   '#039',D.CPUs);'#013#010+
  '            Add('#039'  </dependency>'#039');'#013#010+
  '          end;'#013#010+
  '        Add('#039' </dependencies>'#039');'#013#010+
  '      end;'#013#010+
  '    Add('#039'</package>'#039');'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TPackage.AddPackageVariant(APackageVariant: TPackageVariants'+
  ');'#013#010+
  'begin'#013#010,
  '  if not assigned(APackageVariant.FMasterPackage) then'#013#010+
  '    APackageVariant.FMasterPackage := Self;'#013#010+
  '  FPackageVariants.Add(APackageVariant);'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TPackage.ApplyPackageVariantToCompilerOptions(ACompilerOptio'+
  'ns: tstrings);'#013#010+
  'var',#013#010+
  '  i: integer;'#013#010+
  '  PackageVariants: TPackageVariants;'#013#010+
  'begin'#013#010+
  '  for i := 0 to FPackageVariants.Count-1 do'#013#010+
  '    begin'#013#010+
  '    PackageVariants := TPackageVariants(FPackageVariants.Items[i]);'#013+
  #010+
  '    ACompilerOptions.AddStrings(PackageVariants.Activ','ePackageVariant'+
  '.Options);'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TPackage.SetDefaultPackageVariant;'#013#010+
  'var'#013#010+
  '  i,j: integer;'#013#010+
  '  PackageVariants: TPackageVariants;'#013#010+
  'begin'#013#010+
  '  for i := 0 to FPackageVariants.Count-1 do'#013#010+
  '    begin'#013#010+
  '    PackageVariants := TPa','ckageVariants(FPackageVariants.Items[i]);'#013+
  #010+
  '    if Installer.FPackageVariantSettings.Values[PackageVariants.Name]<'+
  '>'#039#039' then'#013#010+
  '      PackageVariants.ActivePackageVariantName:= Installer.FPackageVar'+
  'iantSettings.Values[PackageVariants.Name]'#013#010+
  '    ','else'#013#010+
  '      PackageVariants.ActivePackageVariantName:= PackageVariants.Defau'+
  'ltPackageVariantName;'#013#010+
  '    Dictionary.AddVariable(PackageVariants.Name,PackageVariants.Active'+
  'PackageVariantName);'#013#010+
  '    SetUnitsOutputDir(FUnitsOutputDir+'#039'$('#039'+Packag','eVariants.n'+
  'ame+'#039')'#039');'#013#010+
  '    SetPackageUnitInstallDir(FPackageUnitInstallDir+'#039'$('#039'+Packa'+
  'geVariants.Name+'#039')'#039');'#013#010+
  '    // Do not add targets f the package is inherited'#013#010+
  '    if PackageVariants.MasterPackage=Self then'#013#010+
  '      for j := 0 to PackageVari','ants.ActivePackageVariant.Targets.cou'+
  'nt -1 do'#013#010+
  '        targets.add.assign(PackageVariants.ActivePackageVariant.Target'+
  's.items[j]);'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TPackage.LoadUnitConfigFromFile(Const AFileName: String);'#013+
  #010+
  'var'#013#010+
  '  L,L2 : TStrin','gs;'#013#010+
  '  VOS : TOS;'#013#010+
  '  VCPU : TCPU;'#013#010+
  '  i,k : Integer;'#013#010+
  '  DepChecksum : Cardinal;'#013#010+
  '  DepName : String;'#013#010+
  '  D : TDependency;'#013#010+
  '  PackageVariantsStr: string;'#013#010+
  '  PackageVarName: string;'#013#010+
  '  pv: TPackageVariants;'#013#010+
  '  AnIsInheritable: boolean;'#013#010+
  'begin'#013#010,
  '  L:=TStringList.Create;'#013#010+
  '  Try'#013#010+
  '    ReadIniFile(AFileName,L);'#013#010+
  '    With L do'#013#010+
  '      begin'#013#010+
  '        Version:=Values[KeyVersion];'#013#010+
  '        InstalledChecksum:=Cardinal(StrToInt64Def(Values[KeyChecksum],'+
  '$ffffffff));'#013#010+
  '        VCPU:=StringToCPU(','Values[KeyCPU]);'#013#010+
  '        VOS:=StringToOS(Values[KeyOS]);'#013#010+
  '        OSes:=[VOS];'#013#010+
  '        CPUs:=[VCPU];'#013#010+
  '        L2:=TStringList.Create;'#013#010+
  '        L2.CommaText:=Values[KeyDepends];'#013#010+
  '        for i:=0 to L2.Count-1 do'#013#010+
  '          begin'#013#010+
  '        ','    DepName:=L2[i];'#013#010+
  '            k:=Pos('#039'|'#039',DepName);'#013#010+
  '            if k>0 then'#013#010+
  '              begin'#013#010+
  '                DepChecksum:=StrToInt(Copy(DepName,k+1,Length(DepName)'+
  '-k));'#013#010+
  '                DepName:=Copy(DepName,1,k-1);'#013#010+
  '              e','nd'#013#010+
  '            else'#013#010+
  '              DepChecksum:=$ffffffff;'#013#010+
  '            D:=Dependencies.Add(DepName,CPUs,OSes);'#013#010+
  '            D.RequireChecksum:=DepChecksum;'#013#010+
  '          end;'#013#010+
  '        FreeAndNil(L2);'#013#010+
  '        NeedLibC:=Upcase(Values[KeyNeedL','ibC])='#039'Y'#039';'#013#010+
  '        IsFPMakeAddIn:=Upcase(Values[KeyAddIn])='#039'Y'#039';'#013#010+
  '        Flags.DelimitedText:=Values[KeyFlags];'#013#010+
  #013#010+
  '        i := 1;'#013#010+
  '        repeat'#013#010+
  '        PackageVariantsStr:=Values[KeyPackageVar+inttostr(i)];'#013#010+
  '        if PackageVariantsS','tr<>'#039#039' then'#013#010+
  '          begin'#013#010+
  '            k := pos('#039':'#039',PackageVariantsStr);'#013#010+
  '            if k > 0 then'#013#010+
  '              begin'#013#010+
  '                PackageVarName:=copy(PackageVariantsStr,1,k-1);'#013#010+
  '                if PackageVarName[Length(PackageVa','rName)]='#039'*'#039+
  ' then'#013#010+
  '                  begin'#013#010+
  '                  SetLength(PackageVarName,Length(PackageVarName)-1);'#013+
  #010+
  '                  AnIsInheritable:=true;'#013#010+
  '                  end'#013#010+
  '                else'#013#010+
  '                  AnIsInheritable:=fals','e;'#013#010+
  '                PackageVariantsStr:=copy(PackageVariantsStr,k+1,length'+
  '(PackageVariantsStr)-k);'#013#010+
  '                pv := Installer.AddPackageVariant(PackageVarName, AnIs'+
  'Inheritable);'#013#010+
  '                AddPackageVariant(pv);'#013#010+
  #013#010+
  '             ','   k := pos('#039','#039',PackageVariantsStr);'#013#010+
  '                while k>0 do'#013#010+
  '                  begin'#013#010+
  '                    PackageVarName:=copy(PackageVariantsStr,1,k-1);'#013+
  #010+
  '                    PackageVariantsStr:=copy(PackageVariantsStr,k+1,le'+
  'ngth(Pac','kageVariantsStr)-k);'#013#010+
  '                    pv.Add(PackageVarName);'#013#010+
  '                    k := pos('#039','#039',PackageVariantsStr);'#013#010+
  '                  end;'#013#010+
  '                pv.Add(PackageVariantsStr);'#013#010+
  '              end;'#013#010+
  '          end;'#013#010+
  '        inc(i',');'#013#010+
  '        until PackageVariantsStr='#039#039';'#013#010+
  #013#010+
  '      end;'#013#010+
  '  Finally'#013#010+
  '    L.Free;'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TPackage.SaveUnitConfigToStringList(const AStringList: TStri'+
  'ngs; ACPU: TCPU; AOS: TOS);'#013#010+
  'Var'#013#010+
  '  Deps : String;'#013#010+
  '  i,j : integer;'#013#010+
  '  D ',': TDependency;'#013#010+
  '  p : TPackage;'#013#010+
  '  PackageVariants : TPackageVariants;'#013#010+
  '  PackageVariantsStr: string;'#013#010+
  'begin'#013#010+
  '  with AStringList do'#013#010+
  '    begin'#013#010+
  '      Values[KeyName]:=Name;'#013#010+
  '      Values[KeyVersion]:=Version;'#013#010+
  '      // TODO Generate checksu','m based on PPUs'#013#010+
  '      Values[KeyChecksum]:=IntToStr(DateTimeToFileDate(Now));'#013#010+
  '      Values[KeyCPU]:=CPUToString(ACPU);'#013#010+
  '      Values[KeyOS]:=OSToString(AOS);'#013#010+
  '      //Installer;'#013#010+
  '      Values[KeySourcePath]:=IncludeTrailingPathDelimiter(I','ncludeTra'+
  'ilingPathDelimiter(Installer.BuildEngine.FStartDir)+Directory);'#013#010+
  '      Values[KeyFPMakeOptions]:=trim(Installer.FPMakeOptionsString);'#013+
  #010+
  '      Deps:='#039#039';'#013#010+
  '      for i:=0 to Dependencies.Count-1 do'#013#010+
  '        begin'#013#010+
  '          D:=Dependen','cies[i];'#013#010+
  '          if (ACPU in D.CPUs) and (AOS in D.OSes) then'#013#010+
  '            begin'#013#010+
  '              if Deps<>'#039#039' then'#013#010+
  '                Deps:=Deps+'#039','#039';'#013#010+
  '              Deps:=Deps+D.Value;'#013#010+
  '              P:=TPackage(D.Target);'#013#010+
  '              if ','assigned(P) and (P.InstalledChecksum<>$ffffffff) th'+
  'en'#013#010+
  '                Deps:=Deps+'#039'|'#039'+IntToStr(P.InstalledChecksum);'#013+
  #010+
  '            end;'#013#010+
  '        end;'#013#010+
  '      Values[KeyDepends]:=Deps;'#013#010+
  '      if Flags.Count>0 then'#013#010+
  '        Values[KeyFlags]:=Fl','ags.DelimitedText;'#013#010+
  '      if NeedLibC then'#013#010+
  '        Values[KeyNeedLibC]:='#039'Y'#039#013#010+
  '      else'#013#010+
  '        Values[KeyNeedLibC]:='#039'N'#039';'#013#010+
  '      if IsFPMakeAddIn then'#013#010+
  '        Values[KeyAddIn]:='#039'Y'#039#013#010+
  '      else'#013#010+
  '        Values[KeyAddIn]:='#039'N'#039';'#013#010+
  '      for i',' := 0 to FPackageVariants.Count-1 do'#013#010+
  '        begin'#013#010+
  '          PackageVariants := TPackageVariants(FPackageVariants.Items[i'+
  ']);'#013#010+
  '          PackageVariantsStr:=PackageVariants.Name;'#013#010+
  '          if PackageVariants.IsInheritable then'#013#010+
  '          ','  PackageVariantsStr:=PackageVariantsStr+'#039'*'#039';'#013#010+
  '          PackageVariantsStr := PackageVariantsStr +'#039':'#039'+Packag'+
  'eVariants.DefaultPackageVariantName;'#013#010+
  '          for j := 0 to PackageVariants.Count-1 do'#013#010+
  '            if not sametext(PackageVarian','ts.Items[j].Name, PackageVa'+
  'riants.DefaultPackageVariantName) then'#013#010+
  '              PackageVariantsStr:=PackageVariantsStr+'#039','#039'+Packa'+
  'geVariants.Items[j].Name;'#013#010+
  '          values[KeyPackageVar+inttostr(i+1)] := PackageVariantsStr;'#013+
  #010+
  '        end;'#013#010,
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TPackage.SaveUnitConfigToFile(Const AFileName: String;ACPU:T'+
  'CPU;AOS:TOS);'#013#010+
  'Var'#013#010+
  '  F : TFileStream;'#013#010+
  '  L : TStringList;'#013#010+
  'begin'#013#010+
  '  F:=TFileStream.Create(AFileName,fmCreate or fmShareDenyNone);'#013#010+
  '  L:=TStringList.Cre','ate;'#013#010+
  '  try'#013#010+
  '    SaveUnitConfigToStringList(L,ACPU,AOS);'#013#010+
  '    L.SaveToStream(F);'#013#010+
  '  Finally'#013#010+
  '    L.Free;'#013#010+
  '    F.Free;'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                      ','        TPackages'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'function TPackages.GetPackage(const AName : String): TPackage;'#013#010+
  'begin'#013#010+
  '  Result:=TPackage(ItemByName(AName))'#013#010+
  'end;'#013#010+
  #013#010+
  'function TPackages.Get','PackageItem(AIndex : Integer): TPackage;'#013#010+
  'begin'#013#010+
  '  Result:=TPackage(Items[AIndex]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TPackages.SetPackageItem(AIndex : Integer; const AValue: TPa'+
  'ckage);'#013#010+
  'begin'#013#010+
  '  Items[AIndex]:=AValue;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TPackages.AddP','ackage(const AName: String): TPackage;'#013#010+
  'var'#013#010+
  '  i: integer;'#013#010+
  'begin'#013#010+
  '  i := IndexOfName(AName);'#013#010+
  '  if i > -1 then'#013#010+
  '    begin'#013#010+
  '    result := PackageItems[i];'#013#010+
  '    Installer.Log(vlWarning,Format(SWarnDuplicatePackage,[AName]))'#013+
  #010+
  '    end'#013#010+
  '  else'#013,#010+
  '    begin'#013#010+
  '    Result:=Add as TPackage;'#013#010+
  '    Result.Name:=AName;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                             TCustomDefaults'#013#010+
  '****************************','****************************************'+
  '********}'#013#010+
  #013#010+
  'procedure TCustomDefaults.SetCPU(const AValue: TCPU);'#013#010+
  'begin'#013#010+
  '  FCPU:=AValue;'#013#010+
  '  GlobalDictionary.AddVariable('#039'CPU'#039',CPUToString(FCPU));'#013#010+
  '  RecalcTarget;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TCustomDefaults','.SetOptions(const AValue: TStrings);'#013#010+
  'begin'#013#010+
  '  If (AValue=Nil) or (AValue.Count=0) then'#013#010+
  '    FreeAndNil(Foptions)'#013#010+
  '  else'#013#010+
  '    Options.Assign(AValue)'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TCustomDefaults.GetBaseInstallDir: String;'#013#010+
  'begin'#013#010+
  '  If (FBaseInstallD','ir<>'#039#039') then'#013#010+
  '    Result:=FBaseInstallDir'#013#010+
  '  else'#013#010+
  '    if UnixPaths then'#013#010+
  '      Result:=Prefix +'#039'lib'#039' + PathDelim + '#039'fpc'#039' + Path'+
  'Delim + FCompilerVersion + PathDelim'#013#010+
  '    else'#013#010+
  '      Result:=Prefix;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TCustomDefaults.GetBin','InstallDir: String;'#013#010+
  'begin'#013#010+
  '  If (FBinInstallDir<>'#039#039') then'#013#010+
  '    Result:=FBinInstallDir'#013#010+
  '  else'#013#010+
  '    If UnixPaths then'#013#010+
  '      Result:=Prefix+'#039'bin'#039#013#010+
  '    else'#013#010+
  '      Result:=BaseInstallDir+'#039'bin'#039'+pathdelim+MakeTargetString('+
  'Defaults.cpu, Default','s.OS);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TCustomDefaults.GetCompiler: String;'#013#010+
  'begin'#013#010+
  '  If (FCompiler<>'#039#039') then'#013#010+
  '    Result:=FCompiler'#013#010+
  '  else'#013#010+
  '    Result:='#039'fpc'#039';'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TCustomDefaults.GetDocInstallDir: String;'#013#010+
  'begin'#013#010+
  '  If (FDocInstallDir<','>'#039#039') then'#013#010+
  '    Result:=FDocInstallDir'#013#010+
  '  else'#013#010+
  '    If UnixPaths then'#013#010+
  '      Result:=Prefix+'#039'share'#039'+PathDelim+'#039'doc'#039#013#010+
  '    else'#013#010+
  '      Result:=BaseInstallDir+'#039'docs'#039';'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TCustomDefaults.GetExamplesInstallDir: String;'#013#010+
  'begin'#013#010+
  '  ','If (FExamplesInstallDir<>'#039#039') then'#013#010+
  '    Result:=FExamplesInstallDir'#013#010+
  '  else'#013#010+
  '    If UnixPaths then'#013#010+
  '      Result:=Prefix+'#039'share'#039'+PathDelim+'#039'doc'#039#013#010+
  '    else'#013#010+
  '      Result:=BaseInstallDir+'#039'examples'#039';'#013#010+
  'end;'#013#010+
  #013#010+
  'function TCustomDefaults.GetOptions',': TStrings;'#013#010+
  'begin'#013#010+
  '  If (FOptions=Nil) then'#013#010+
  '    FOptions:=TStringList.Create;'#013#010+
  '  Result:=FOptions;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TCustomDefaults.GetUnitInstallDir: String;'#013#010+
  'begin'#013#010+
  '  result := FixPath(GlobalDictionary.ReplaceStrings(FUnitInstallDir)',','+
  ' False);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TCustomDefaults.GetLocalUnitDir: String;'#013#010+
  'begin'#013#010+
  '  Result:=FLocalUnitDir;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TCustomDefaults.GetFPDocOutputDir: String;'#013#010+
  'begin'#013#010+
  '  If (FFPDocOutputDir<>'#039#039') then'#013#010+
  '    Result:=FixPath(FFPDocOutputDi','r, True)'#013#010+
  '  else'#013#010+
  '    Result:=FixPath('#039'.'#039'+PathDelim+'#039'docs'#039', True);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TCustomDefaults.GetBuildCPU: TCpu;'#013#010+
  'begin'#013#010+
  '  result := StringToCPU({$I %FPCTARGETCPU%});'#013#010+
  'end;'#013#010+
  #013#010+
  'function TCustomDefaults.GetBuildOS: TOS;'#013#010+
  'begin'#013#010+
  '  resul','t := StringToOS({$I %FPCTARGETOS%});'#013#010+
  'end;'#013#010+
  #013#010+
  'function TCustomDefaults.GetBuildString: String;'#013#010+
  'begin'#013#010+
  '  result := MakeTargetString(BuildCPU, BuildOS);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TCustomDefaults.GetGlobalUnitDir: String;'#013#010+
  'begin'#013#010+
  '  If (FGlobalUnitDir','<>'#039#039') then'#013#010+
  '    Result:=FGlobalUnitDir'#013#010+
  '  else'#013#010+
  '    Result:=UnitInstallDir;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomDefaults.SetLocalUnitDir(const AValue: String);'#013#010+
  'begin'#013#010+
  '  // Use ExpandFileName to support ~/ expansion'#013#010+
  '  if AValue<>'#039#039' then'#013#010+
  '    FLoca','lUnitDir:=IncludeTrailingPathDelimiter(ExpandFileName(AValu'+
  'e))'#013#010+
  '  else'#013#010+
  '    FLocalUnitDir:='#039#039';'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomDefaults.SetGlobalUnitDir(const AValue: String);'#013#010+
  'begin'#013#010+
  '  // Use ExpandFileName to support ~/ expansion'#013#010+
  '  if AValue','<>'#039#039' then'#013#010+
  '    FGlobalUnitDir:=IncludeTrailingPathDelimiter(ExpandFileName(AValue'+
  '))'#013#010+
  '  else'#013#010+
  '    FGlobalUnitDir:='#039#039';'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TCustomDefaults.IntSetBaseInstallDir(const AValue: String);'#013+
  #010+
  'begin'#013#010+
  '  if AValue<>'#039#039' then'#013#010+
  '    FBaseInsta','llDir:=IncludeTrailingPathDelimiter(AValue)'#013#010+
  '  else'#013#010+
  '    FBaseInstallDir:='#039#039';'#013#010+
  '  GlobalDictionary.AddVariable('#039'baseinstalldir'#039',BaseInstallDir'+
  ');'#013#010+
  '  GlobalDictionary.AddVariable('#039'bininstalldir'#039',BinInstallDir);'+
  #013#010+
  '  BinInstallDir:='#039#039';'#013#010+
  '  Example','sInstallDir:='#039#039';'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomDefaults.SetBaseInstallDir(const AValue: String);'#013+
  #010+
  'begin'#013#010+
  '  // There must be a possibility to skip ExpandFileName. So that the f'+
  'iles'#013#010+
  '  // can be written into an archive with a relative path.'#013#010+
  '  ','if AValue<>'#039#039' then'#013#010+
  '    // Use ExpandFileName to support ~/ expansion'#013#010+
  '    IntSetBaseInstallDir(ExpandFileName(AValue))'#013#010+
  '  else'#013#010+
  '    IntSetBaseInstallDir(AValue);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomDefaults.SetOS(const AValue: TOS);'#013#010+
  'begin'#013#010+
  '  FOS:','=AValue;'#013#010+
  '  GlobalDictionary.AddVariable('#039'OS'#039',OSToString(FOS));'#013#010+
  '  Recalctarget;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomDefaults.SetPrefix(const AValue: String);'#013#010+
  'begin'#013#010+
  '  if FPrefix=AValue then exit;'#013#010+
  '  // Use ExpandFileName to support ~/ expansion'#013#010,
  '  if AValue<>'#039#039' then'#013#010+
  '    FPrefix:=IncludeTrailingPathDelimiter(ExpandFileName(AValue))'#013#010+
  '  else'#013#010+
  '    FPrefix:='#039#039';'#013#010+
  '  GlobalDictionary.AddVariable('#039'prefix'#039',Prefix);'#013#010+
  '  GlobalDictionary.AddVariable('#039'bininstalldir'#039',BinInstallDir);'+
  #013#010+
  '  BaseInsta','llDir:='#039#039';'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomDefaults.SetTarget(const AValue: String);'#013#010+
  'Var'#013#010+
  '  P : Integer;'#013#010+
  'begin'#013#010+
  '  if FTarget<>AValue then'#013#010+
  '    begin'#013#010+
  '      P:=Pos('#039'-'#039',AValue);'#013#010+
  '      If (P<>0) then'#013#010+
  '        begin'#013#010+
  '          FOS:=StringToOS(Sy','stem.Copy(Avalue,P+1,Length(AValue)-P));'+
  #013#010+
  '          GlobalDictionary.AddVariable('#039'OS'#039',OSToString(FOS));'#013+
  #010+
  '          FCPU:=StringToCPU(System.Copy(Avalue,1,P-1));'#013#010+
  '          GlobalDictionary.AddVariable('#039'CPU'#039',CPUToString(FCPU)'+
  ');'#013#010+
  '        end'#013,#010+
  '      else'#013#010+
  '        FOS:=StringToOS(AValue);'#013#010+
  '      FTarget:=AValue;'#013#010+
  '      GlobalDictionary.AddVariable('#039'target'#039',Target);'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TCustomDefaults.SetUnitInstallDir(const AValue: String);'#013+
  #010+
  'begin'#013#010+
  '  if AValue<>'#039#039' then'#013#010,
  '    FUnitInstallDir:=IncludeTrailingPathDelimiter(AValue)'#013#010+
  '  else'#013#010+
  '    FUnitInstallDir:='#039#039';'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TCustomDefaults.SetZipPrefix(AValue: String);'#013#010+
  'begin'#013#010+
  '  if FZipPrefix=AValue then Exit;'#013#010+
  '  FZipPrefix:=AValue;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedu','re TCustomDefaults.RecalcTarget;'#013#010+
  'begin'#013#010+
  '  Ftarget:=MakeTargetString(FCPU,FOS);'#013#010+
  '  GlobalDictionary.AddVariable('#039'target'#039',Target);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TCustomDefaults.CmdLineOptions: String;'#013#010+
  'begin'#013#010+
  '  If Haveoptions then'#013#010+
  '    Result:=OptionLis','tToString(FOptions);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'constructor TCustomDefaults.Create;'#013#010+
  'begin'#013#010+
  '  InitDefaults;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomDefaults.InitDefaults;'#013#010+
  'begin'#013#010+
  '{$ifdef unix}'#013#010+
  '  UnixPaths:=True;'#013#010+
  '{$else}'#013#010+
  '  UnixPaths:=False;'#013#010+
  '{$endif}'#013#010+
  '  FNoFPCCfg:=F','alse;'#013#010+
  '  FCPU:=cpuNone;'#013#010+
  '  FOS:=osNone;'#013#010+
  '  FUnitInstallDir:='#039'$(BaseInstallDir)units/$(target)/$(packagename'+
  ')'#039';'#013#010+
  '  FBuildMode:=bmOneByOne;'#013#010+
  '  FThreadsAmount:=-1;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TCustomDefaults.HaveOptions: Boolean;'#013#010+
  'begin'#013#010+
  '  Result:=Assig','ned(FOptions);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TCustomDefaults.IsBuildDifferentFromTarget: boolean;'#013#010+
  'begin'#013#010+
  '  result := IsDifferentFromBuild(CPU,OS);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomDefaults.LocalInit(Const AFileName : String);'#013#010+
  'Var'#013#010+
  '  FN : String;'#013#010+
  'begin'#013#010+
  ' ',' FN:=AFileName;'#013#010+
  '  If (FN='#039#039') then'#013#010+
  '    begin'#013#010+
  '    // Environment variable.'#013#010+
  '    FN:=GetEnvironmentVariable('#039'FPMAKECFG'#039');'#013#010+
  '    If (FN<>'#039#039') then'#013#010+
  '      If not FileExists(FN) then'#013#010+
  '        FN:='#039#039';'#013#010+
  '    // User config file fpmake.cfg'#013#010+
  '    If (','FN='#039#039') then'#013#010+
  '      begin'#013#010+
  '      FN:=GetAppConfigFile(False);'#013#010+
  '      If Not FileExists(FN) then'#013#010+
  '        FN:='#039#039';'#013#010+
  '      end;'#013#010+
  '    // Global config file fpmake.cfg'#013#010+
  '    If (FN='#039#039') then'#013#010+
  '      begin'#013#010+
  '      FN:=GetAppConfigFile(True);'#013#010+
  '      If',' Not FileExists(FN) then'#013#010+
  '        FN:='#039#039';'#013#010+
  '      end;'#013#010+
  '    end;'#013#010+
  '  If (FN<>'#039#039') and FileExists(FN) then'#013#010+
  '    LoadFromFile(FN);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomDefaults.CompilerDefaults;'#013#010+
  '{$ifdef HAS_UNIT_PROCESS}'#013#010+
  'var'#013#010+
  '  infoSL : TStringList;'#013#010+
  '{','$endif HAS_UNIT_PROCESS}'#013#010+
  'begin'#013#010+
  '  if (CPU=cpuNone) or (OS=osNone) or (FCompilerVersion='#039#039') then'#013+
  #010+
  '    begin'#013#010+
  '{$ifdef HAS_UNIT_PROCESS}'#013#010+
  '      // Detect compiler version/target from -i option'#013#010+
  '      infosl:=TStringList.Create;'#013#010+
  '      infosl.','Delimiter:='#039' '#039';'#013#010+
  '      infosl.DelimitedText:=GetCompilerInfo(GetCompiler,'#039'-iVTPTO'#039+
  ', False);'#013#010+
  '      if infosl.Count<>3 then'#013#010+
  '        Raise EInstallerError.Create(SErrInvalidFPCInfo);'#013#010+
  '      if FCompilerVersion='#039#039' then'#013#010+
  '        FCompilerVersi','on:=infosl[0];'#013#010+
  '      if CPU=cpuNone then'#013#010+
  '        CPU:=StringToCPU(infosl[1]);'#013#010+
  '      if OS=osNone then'#013#010+
  '        OS:=StringToOS(infosl[2]);'#013#010+
  '{$else HAS_UNIT_PROCESS}'#013#010+
  '      // Defaults taken from compiler used to build fpmake'#013#010+
  '      if CPU','=cpuNone then'#013#010+
  '        CPU:=StringToCPU({$I %FPCTARGETCPU%});'#013#010+
  '      if OS=osNone then'#013#010+
  '        OS:=StringToOS({$I %FPCTARGETOS%});'#013#010+
  '      if FCompilerVersion='#039#039' then'#013#010+
  '        FCompilerVersion:={$I %FPCVERSION%};'#013#010+
  '{$endif HAS_UNIT_PROCESS}'#013,#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomDefaults.LoadFromFile(Const AFileName: String);'#013#010+
  'Var'#013#010+
  '  F : TFileStream;'#013#010+
  'begin'#013#010+
  '  F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyNone);'#013#010+
  '  Try'#013#010+
  '    LoadFromStream(F);'#013#010+
  '  Finally'#013#010+
  '    F.Free;'#013,#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomDefaults.SaveToFile(Const AFileName: String);'#013#010+
  'Var'#013#010+
  '  F : TFileStream;'#013#010+
  'begin'#013#010+
  '  F:=TFileStream.Create(AFileName,fmCreate or fmShareDenyNone);'#013#010+
  '  Try'#013#010+
  '    SaveToStream(F);'#013#010+
  '  Finally'#013#010+
  '    F.Free;'#013#010+
  '  end;'#013,#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomDefaults.SaveToStream(S : TStream);'#013#010+
  'Var'#013#010+
  '  L : TStringList;'#013#010+
  'begin'#013#010+
  '  L:=TStringList.Create;'#013#010+
  '  try'#013#010+
  '    With L do'#013#010+
  '      begin'#013#010+
  '      Values[KeyArchive]:=FArchive;'#013#010+
  '      Values[KeyCompiler]:=FCompiler;'#013#010+
  '      ','Values[KeyCopy]:=FCopy;'#013#010+
  '      Values[KeyMkDir]:=FMkDir;'#013#010+
  '      Values[KeyMove]:=FMove;'#013#010+
  '      Values[KeyOptions]:=CmdLineOptions;'#013#010+
  '      Values[KeyCPU]:=CPUToString(FCPU);'#013#010+
  '      Values[KeyOS]:=OSToString(FOS);'#013#010+
  '      Values[KeyMode]:=Mode','ToString(FMode);'#013#010+
  '      Values[KeyLocalUnitDir]:=FLocalUnitDir;'#013#010+
  '      Values[KeyGlobalUnitDir]:=FGlobalUnitDir;'#013#010+
  '      Values[KeyPrefix]:=FPrefix;'#013#010+
  '      Values[KeyBaseInstallDir]:=FBaseInstallDir;'#013#010+
  '      Values[KeyUnitInstallDir]:=FUnitIn','stallDir;'#013#010+
  '      Values[KeyBinInstallDir]:=FBinInstallDir;'#013#010+
  '      Values[KeyDocInstallDir]:=FDocInstallDir;'#013#010+
  '      Values[KeyExamplesInstallDir]:=FExamplesInstallDir;'#013#010+
  '      Values[KeyRemove]:=FRemove;'#013#010+
  '      Values[KeyRemoveDir]:=FRemoveDi','r;'#013#010+
  '      Values[KeyRemoveTree]:=FRemoveTree;'#013#010+
  '      Values[KeyTarget]:=FTarget;'#013#010+
  '      if FNoFPCCfg then'#013#010+
  '        Values[KeyNoFPCCfg]:='#039'Y'#039';'#013#010+
  '      if FUseEnvironment then'#013#010+
  '        Values[KeyUseEnv]:='#039'Y'#039';'#013#010+
  '      if FInstallExamples then'#013#010+
  '  ','        Values[KeyInstallExamples]:='#039'Y'#039';'#013#010+
  '      if FSkipCrossPrograms then'#013#010+
  '        Values[KeySkipCrossProdrams]:='#039'Y'#039';'#013#010+
  '      end;'#013#010+
  '    L.SaveToStream(S);'#013#010+
  '  Finally'#013#010+
  '    L.Free;'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomDefaults.LoadFromStream(S',': TStream);'#013#010+
  'Var'#013#010+
  '  L : TStrings;'#013#010+
  '  Line : String;'#013#010+
  '  I,P,PC : Integer;'#013#010+
  'begin'#013#010+
  '  L:=TStringList.Create;'#013#010+
  '  Try'#013#010+
  '    L.LoadFromStream(S);'#013#010+
  '    // Fix lines.'#013#010+
  '    For I:=L.Count-1 downto 0 do'#013#010+
  '      begin'#013#010+
  '      Line:=L[I];'#013#010+
  '      P:=Pos('#039'=',#039',Line);'#013#010+
  '      PC:=Pos('#039';'#039',Line);  // Comment line.'#013#010+
  '      If (P=0) or ((PC<>0) and (PC<P)) then'#013#010+
  '        L.Delete(I)'#013#010+
  '      else'#013#010+
  '        L[i]:=Trim(System.Copy(Line,1,P-1)+'#039'='#039'+Trim(System.Cop'+
  'y(Line,P+1,Length(Line)-P)));'#013#010+
  '      end;'#013#010+
  '   ',' With L do'#013#010+
  '      begin'#013#010+
  '      FArchive:=Values[KeyArchive];'#013#010+
  '      FCompiler:=Values[KeyCompiler];'#013#010+
  '      FCopy:=Values[KeyCopy];'#013#010+
  '      FMkDir:=Values[KeyMkDir];'#013#010+
  '      FMove:=Values[KeyMove];'#013#010+
  '      FRemove:=Values[KeyRemove];'#013#010+
  '      FRe','moveDir:=Values[KeyRemoveDir];'#013#010+
  '      FRemoveTree:=Values[KeyRemoveTree];'#013#010+
  '      Options:=OptionsToStringList(Values[KeyOptions]);'#013#010+
  '      Line:=Values[KeyCPU];'#013#010+
  '      If (Line<>'#039#039') then'#013#010+
  '        FCPU:=StringToCPU(Line);'#013#010+
  '      Line:=Values[','KeyOS];'#013#010+
  '      If (Line<>'#039#039') then'#013#010+
  '        FOS:=StringToOS(Line);'#013#010+
  '      Line:=Values[KeyMode];'#013#010+
  '      If (Line<>'#039#039') then'#013#010+
  '        FMode:=StringToMode(Line);'#013#010+
  '      FTarget:=Values[KeyTarget];'#013#010+
  '      FLocalUnitDir:=Values[KeyLocalUnitDir];'#013,#010+
  '      FGlobalUnitDir:=Values[KeyGlobalUnitDir];'#013#010+
  '      FPrefix:=Values[KeyPrefix];'#013#010+
  '      FBaseInstallDir:=Values[KeyBaseInstallDir];'#013#010+
  '      FUnitInstallDir:=Values[KeyUnitInstallDir];'#013#010+
  '      FBinInstallDir:=Values[KeyBinInstallDir];'#013#010+
  '    ','  FDocInstallDir:=Values[KeyDocInstallDir];'#013#010+
  '      FExamplesInstallDir:=Values[KeyExamplesInstallDir];'#013#010+
  '      FInstallExamples:=(Upcase(Values[KeyInstallExamples])='#039'Y'#039+
  ');'#013#010+
  '      FSkipCrossPrograms:=(Upcase(Values[KeySkipCrossProdrams])='#039'Y'+
  #039');',#013#010+
  '      FNoFPCCfg:=(Upcase(Values[KeyNoFPCCfg])='#039'Y'#039');'#013#010+
  '      FUseEnvironment:=(Upcase(Values[KeyUseEnv])='#039'Y'#039');'#013#010+
  #013#010+
  '      GlobalDictionary.AddVariable('#039'target'#039',Target);'#013#010+
  '      GlobalDictionary.AddVariable('#039'baseinstalldir'#039',BaseInstal'+
  'lDir);'#013#010+
  '  ','    GlobalDictionary.AddVariable('#039'prefix'#039',Prefix);'#013#010+
  '      GlobalDictionary.AddVariable('#039'bininstalldir'#039',BinInstallD'+
  'ir);'#013#010+
  '      end;'#013#010+
  '  Finally'#013#010+
  '    L.Free;'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*****************************************************************','**'+
  '*********'#013#010+
  '                             TFPCDefaults'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'procedure TFPCDefaults.CompilerDefaults;'#013#010+
  'var'#013#010+
  '  BD : String;'#013#010+
  'begin'#013#010+
  '  inherited CompilerDefaults;'#013#010+
  #013#010,
  '  // Use the same algorithm as the compiler, see options.pas'#013#010+
  '{$ifdef Unix}'#013#010+
  '  BD:=FixPath(GetEnvironmentVariable('#039'FPCDIR'#039'), False);'#013#010+
  '  if BD='#039#039' then'#013#010+
  '    begin'#013#010+
  '      BD:='#039'/usr/local/lib/fpc/'#039'+FCompilerVersion;'#013#010+
  '      if not DirectoryExist','s(BD) and'#013#010+
  '         DirectoryExists('#039'/usr/lib/fpc/'#039'+FCompilerVersion) the'+
  'n'#013#010+
  '        BD:='#039'/usr/lib/fpc/'#039'+FCompilerVersion;'#013#010+
  '    end;'#013#010+
  '{$else unix}'#013#010+
  '  BD:=FixPath(GetEnvironmentVariable('#039'FPCDIR'#039'), False);'#013#010+
  '  if BD='#039#039' then'#013#010+
  '    begin'#013#010+
  '      BD',':=ExtractFilePath(FCompiler)+'#039'..'#039';'#013#010+
  '      if not(DirectoryExists(BD+'#039'/units'#039')) and'#013#010+
  '         not(DirectoryExists(BD+'#039'/rtl'#039')) then'#013#010+
  '        BD:=FBaseInstallDir+'#039'..'#039';'#013#010+
  '    end;'#013#010+
  '{$endif unix}'#013#010+
  #013#010+
  '  // Where to install by default'#013#010+
  '  if (FBaseIn','stallDir='#039#039') and (FPrefix='#039#039') then'#013#010+
  '    BaseInstallDir:=BD;'#013#010+
  #013#010+
  '  // Where to find the units by default'#013#010+
  '  if (FGlobalUnitDir='#039#039') then'#013#010+
  '    GlobalUnitDir:=IncludeTrailingPathDelimiter(BD)+'#039'units'#039'+Pa'+
  'thDelim+Target;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{***************','****************************************************'+
  '*********'#013#010+
  '                            TCustomInstaller'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'constructor TCustomInstaller.Create(AOwner: TCompo','nent);'#013#010+
  'begin'#013#010+
  '  FPackageVariantSettings := TStringList.Create;'#013#010+
  '  FPackageVariants := TFPList.Create;'#013#010+
  '  GlobalDictionary:=DictionaryClass.Create(Nil);'#013#010+
  '  AnalyzeOptions;'#013#010+
  '  GlobalDictionary.AddVariable('#039'BaseInstallDir'#039',Defaults.BaseI'+
  'nstal','lDir);'#013#010+
  '  GlobalDictionary.AddVariable('#039'bininstalldir'#039',Defaults.BinIns'+
  'tallDir);'#013#010+
  '  GlobalDictionary.AddVariable('#039'Target'#039',Defaults.Target);'#013#010+
  '  GlobalDictionary.AddVariable('#039'BuildString'#039',Defaults.BuildStr'+
  'ing);'#013#010+
  '  GlobalDictionary.AddVariable(',#039'Prefix'#039',Defaults.Prefix);'#013#010+
  '  CreatePackages;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'destructor TCustomInstaller.Destroy;'#013#010+
  'var'#013#010+
  '  i: integer;'#013#010+
  'begin'#013#010+
  '  FreePackages;'#013#010+
  '  FreeAndNil(Defaults);'#013#010+
  '  FreeAndNil(GlobalDictionary);'#013#010+
  '  FreeAndNil(FPackageVariantSettings);'#013#010+
  '  f','or i := 0 to FPackageVariants.Count-1 do'#013#010+
  '    begin'#013#010+
  '    if TPackageVariants(FPackageVariants.Items[i]).Owner=Self then'#013+
  #010+
  '      TPackageVariants(FPackageVariants.Items[i]).Free;'#013#010+
  '    end;'#013#010+
  '  FreeAndNil(FPackageVariants);'#013#010+
  '  inherited destroy',';'#013#010+
  'end;'#013#010+
  #013#010+
  'function TCustomInstaller.GetPackages: TPackages;'#013#010+
  'begin'#013#010+
  '  result := FPackages;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TCustomInstaller.Log(Level: TVerboseLevel; Const Msg: String'+
  ');'#013#010+
  'begin'#013#010+
  '  If Level in FLogLevels then'#013#010+
  '    begin'#013#010+
  '    Writeln(StdO','ut, Msg);'#013#010+
  '    Flush(StdOut);'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomInstaller.CreatePackages;'#013#010+
  'begin'#013#010+
  '  FPackages:=TPackages.Create(TPackage);'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TCustomInstaller.FreePackages;'#013#010+
  'begin'#013#010+
  '  FreeAndNil(FPackages);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'pro','cedure TCustomInstaller.CreateBuildEngine;'#013#010+
  'begin'#013#010+
  '  FBuildEngine:=TBuildEngine.Create(Self);'#013#010+
  '//  FBuildEngine.Defaults:=Defaults;'#013#010+
  '  FBuildEngine.ListMode:=FListMode;'#013#010+
  '  FBuildEngine.Verbose := (FLogLevels = AllMessages);'#013#010+
  '  FBuildEngine.','OnLog:=@Self.Log;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomInstaller.Error(const Msg: String);'#013#010+
  'begin'#013#010+
  '  Raise EInstallerError.Create(Msg);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomInstaller.Error(const Fmt: String; Args: array of con'+
  'st);'#013#010+
  'begin'#013#010+
  '  Raise EInstallerE','rror.CreateFmt(Fmt,Args);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TCustomInstaller.AddPackage(const AName: String) : TPackage;'#013+
  #010+
  'begin'#013#010+
  '  result:=Packages.AddPackage(AName);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TCustomInstaller.AddPackageVariant(AName: string; AIsInherita'+
  'ble: boo','lean): TPackageVariants;'#013#010+
  'begin'#013#010+
  '  result := TPackageVariants.Create(TPackageVariant);'#013#010+
  '  result.Name:=AName;'#013#010+
  '  result.FIsInheritable:=AIsInheritable;'#013#010+
  '  FPackageVariants.Add(result);'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TCustomInstaller.AnalyzeOptions;'#013#010+
  #013#010,
  '  Function CheckOption(Index : Integer;const Short,Long : String; AddT'+
  'oOptionString: boolean = true): Boolean;'#013#010+
  '  var'#013#010+
  '    O : String;'#013#010+
  '  begin'#013#010+
  '    O:=Paramstr(Index);'#013#010+
  '    Result:=(O='#039'-'#039'+short) or (O='#039'--'#039'+long) or (copy(O,'+
  '1,Length(Long)+3',')=('#039'--'#039'+long+'#039'='#039'));'#013#010+
  '    if AddToOptionString and Result then FFPMakeOptionsString := FFPMa'+
  'keOptionsString+'#039' '#039'+O;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  Function CheckBuildOptionSetValue(Index: Integer): boolean;'#013#010+
  '  var'#013#010+
  '    O : String;'#013#010+
  '    BuildModeName: string;'#013#010,
  '    P: integer;'#013#010+
  '  begin'#013#010+
  '    O:=Paramstr(Index);'#013#010+
  '    result := O[1]='#039'+'#039';'#013#010+
  '    if result then'#013#010+
  '      begin'#013#010+
  '      P:=Pos('#039'='#039',Paramstr(Index));'#013#010+
  '      If (P=0) then'#013#010+
  '        Error(SErrNeedArgument,[Index,O])'#013#010+
  '      else'#013#010+
  '        begin'#013#010+
  '    ','    BuildModeName:=copy(o,2,P-2);'#013#010+
  '        Delete(O,1,P);'#013#010+
  '        FPackageVariantSettings.Values[BuildModeName] := O;'#013#010+
  '        end;'#013#010+
  '      end;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  Function CheckCustomOption(Index : Integer; out CustOptName: string)'+
  ': Boolean;'#013#010+
  '  ','var'#013#010+
  '    O : String;'#013#010+
  '    i : integer;'#013#010+
  '  begin'#013#010+
  '    result := false;'#013#010+
  '    CustOptName:='#039#039';'#013#010+
  '    O:=Paramstr(Index);'#013#010+
  '    if copy(O,1,2)<>'#039'--'#039' then'#013#010+
  '      Exit;'#013#010+
  '    i := pos('#039'='#039',O);'#013#010+
  '    if i=0 then'#013#010+
  '      Exit;'#013#010+
  '    O:=copy(O,3,i-3);'#013#010+
  '   ',' CustOptName:=O;'#013#010+
  '    Result:=CustomFpmakeCommandlineOptions.IndexOfName(O)>-1;'#013#010+
  '    if Result then FFPMakeOptionsString := FFPMakeOptionsString+'#039' '+
  #039'+Paramstr(Index);'#013#010+
  '  end;'#013#010+
  #013#010+
  #013#010+
  '  Function CheckCommand(Index : Integer;const Short,Long : St','ring): '+
  'Boolean;'#013#010+
  '  var'#013#010+
  '    O : String;'#013#010+
  '  begin'#013#010+
  '    O:=Paramstr(Index);'#013#010+
  '    Result:=(O='#039'-'#039'+short) or (O=long);'#013#010+
  '  end;'#013#010+
  #013#010+
  '  Function OptionArg(Var Index : Integer) : String;'#013#010+
  '  Var'#013#010+
  '    P : Integer;'#013#010+
  '  begin'#013#010+
  '    if (Length(ParamStr(Inde','x))>1) and (Paramstr(Index)[2]<>'#039'-'#039+
  ') then'#013#010+
  '      begin'#013#010+
  '      If Index<ParamCount then'#013#010+
  '        begin'#013#010+
  '        Inc(Index);'#013#010+
  '        Result:=Paramstr(Index);'#013#010+
  '        end'#013#010+
  '      else'#013#010+
  '        Error(SErrNeedArgument,[Index,ParamStr(Index)]);'#013,#010+
  '      end'#013#010+
  '    else If length(ParamStr(Index))>2 then'#013#010+
  '      begin'#013#010+
  '      P:=Pos('#039'='#039',Paramstr(Index));'#013#010+
  '      If (P=0) then'#013#010+
  '        Error(SErrNeedArgument,[Index,ParamStr(Index)])'#013#010+
  '      else'#013#010+
  '        begin'#013#010+
  '        Result:=Paramstr(Index',');'#013#010+
  '        Delete(Result,1,P);'#013#010+
  '        end;'#013#010+
  '      end;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  function SplitSpaces(var SplitString: string) : string;'#013#010+
  '  var i : integer;'#013#010+
  '  begin'#013#010+
  '    i := pos('#039' '#039',SplitString);'#013#010+
  '    if i > 0 then'#013#010+
  '      begin'#013#010+
  '        result := c','opy(SplitString,1,i-1);'#013#010+
  '        delete(SplitString,1,i);'#013#010+
  '      end'#013#010+
  '    else'#013#010+
  '      begin'#013#010+
  '        result := SplitString;'#013#010+
  '        SplitString:='#039#039';'#013#010+
  '      end;'#013#010+
  '  end;'#013#010+
  #013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  '  DefaultsFileName : string;'#013#010+
  '  OptString : st','ring;'#013#010+
  '  CustOptName : string;'#013#010+
  'begin'#013#010+
  '  I:=0;'#013#010+
  '  FListMode:=False;'#013#010+
  '  FLogLevels:=DefaultMessages;'#013#010+
  '  While (I<ParamCount) do'#013#010+
  '    begin'#013#010+
  '    Inc(I);'#013#010+
  '    if CheckOption(I,'#039'v'#039','#039'verbose'#039',false) then'#013#010+
  '      FLogLevels:=AllMessages'#013#010+
  '    else ','if CheckOption(I,'#039'd'#039','#039'debug'#039',false) then'#013+
  #010+
  '      FLogLevels:=AllMessages+[vlDebug]'#013#010+
  '    else if CheckCommand(I,'#039'm'#039','#039'compile'#039') then'#013#010+
  '      FRunMode:=rmCompile'#013#010+
  '    else if CheckCommand(I,'#039'b'#039','#039'build'#039') then'#013#010+
  '      FRunMode:=rmBuild'#013#010+
  '    else if',' CheckCommand(I,'#039'i'#039','#039'install'#039') then'#013#010+
  '      FRunMode:=rmInstall'#013#010+
  '    else if CheckCommand(I,'#039'zi'#039','#039'zipinstall'#039') then'#013+
  #010+
  '      FRunMode:=rmZipInstall'#013#010+
  '    else if CheckCommand(I,'#039'c'#039','#039'clean'#039') then'#013#010+
  '      FRunMode:=rmClean'#013#010+
  '    else if CheckComman','d(I,'#039'dc'#039','#039'distclean'#039') then'#013+
  #010+
  '      FRunMode:=rmDistClean'#013#010+
  '    else if CheckCommand(I,'#039'a'#039','#039'archive'#039') then'#013#010+
  '      FRunMode:=rmarchive'#013#010+
  '    else if CheckCommand(I,'#039'M'#039','#039'manifest'#039') then'#013#010+
  '      FRunMode:=rmManifest'#013#010+
  '    else if CheckOption(I,'#039'h'#039','#039,'help'#039') then'#013#010+
  '      Usage('#039#039',[])'#013#010+
  '    else if Checkoption(I,'#039'C'#039','#039'cpu'#039') then'#013#010+
  '      Defaults.CPU:=StringToCPU(OptionArg(I))'#013#010+
  '    else if Checkoption(I,'#039'O'#039','#039'os'#039') then'#013#010+
  '      Defaults.OS:=StringToOS(OptionArg(I))'#013#010+
  '    else if Checkoption(I,'#039't'#039',',#039'target'#039') then'#013#010+
  '      Defaults.Target:=OptionArg(I)'#013#010+
  '    else if CheckOption(I,'#039'l'#039','#039'list-commands'#039') then'#013+
  #010+
  '      FListMode:=True'#013#010+
  '    else if Checkoption(I,'#039'P'#039','#039'prefix'#039') then'#013#010+
  '      Defaults.Prefix:=OptionArg(I)'#013#010+
  '    else if Checkoption(I,'#039'n',#039','#039'nofpccfg'#039') then'#013#010+
  '      Defaults.NoFPCCfg:=true'#013#010+
  '    else if Checkoption(I,'#039'zp'#039','#039'zipprefix'#039') then'#013#010+
  '      Defaults.ZipPrefix:=OptionArg(I)'#013#010+
  '{$ifdef HAS_UNIT_PROCESS}'#013#010+
  '    else if Checkoption(I,'#039'e'#039','#039'useenv'#039') then'#013#010+
  '      Defaults.UseEnvironm','ent:=true'#013#010+
  '{$endif}'#013#010+
  '{$ifndef NO_THREADING}'#013#010+
  '    else if CheckOption(I,'#039'T'#039','#039'threads'#039') then'#013#010+
  '      Defaults.ThreadsAmount:=StrToIntDef(OptionArg(I),-1)'#013#010+
  '{$endif NO_THREADING}'#013#010+
  '    else if CheckOption(I,'#039'B'#039','#039'baseinstalldir'#039') then'#013+
  #010+
  '      Defau','lts.BaseInstallDir:=OptionArg(I)'#013#010+
  '    else if CheckOption(I,'#039'U'#039','#039'unitinstalldir'#039') then'#013+
  #010+
  '      Defaults.UnitInstallDir:=OptionArg(I)'#013#010+
  '    else if CheckOption(I,'#039'UL'#039','#039'localunitdir'#039') then'#013+
  #010+
  '      Defaults.LocalUnitDir:=OptionArg(I)'#013#010+
  '    else if',' CheckOption(I,'#039'UG'#039','#039'globalunitdir'#039') then'+
  #013#010+
  '      Defaults.GlobalUnitDir:=OptionArg(I)'#013#010+
  '    else if CheckOption(I,'#039'o'#039','#039'options'#039') then'#013#010+
  '      begin'#013#010+
  '        OptString := OptionArg(I);'#013#010+
  '        while OptString <> '#039#039' do'#013#010+
  '          Defaults.Opti','ons.Add(SplitSpaces(OptString));'#013#010+
  '      end'#013#010+
  '    else if CheckOption(I,'#039'r'#039','#039'compiler'#039') then'#013#010+
  '      Defaults.Compiler:=OptionArg(I)'#013#010+
  '    else if CheckOption(I,'#039'f'#039','#039'config'#039') then'#013#010+
  '      DefaultsFileName:=OptionArg(I)'#013#010+
  '    else if CheckOption(','I,'#039'ie'#039','#039'installexamples'#039') th'+
  'en'#013#010+
  '      Defaults.InstallExamples:=true'#013#010+
  '    else if CheckOption(I,'#039'sp'#039','#039'skipcrossprograms'#039') th'+
  'en'#013#010+
  '      Defaults.SkipCrossPrograms:=true'#013#010+
  '    else if CheckOption(I,'#039'bu'#039','#039'buildunit'#039') then'#013#010+
  '      Defaults.BuildMo','de:=bmBuildUnit'#013#010+
  '    else if CheckOption(I,'#039'io'#039','#039'ignoreinvalidoption'#039') '+
  'then'#013#010+
  '      Defaults.IgnoreInvalidOptions:=true'#013#010+
  '    else if CheckOption(I,'#039'd'#039','#039'doc-folder'#039') then'#013#010+
  '      Defaults.FPDocOutputDir:=OptionArg(I)'#013#010+
  '    else if assigned(Cust','omFpmakeCommandlineOptions) and CheckCustom'+
  'Option(I,CustOptName) then'#013#010+
  '      begin'#013#010+
  '      if not assigned(CustomFpMakeCommandlineValues) then'#013#010+
  '        CustomFpMakeCommandlineValues := TStringList.Create;'#013#010+
  '      CustomFpMakeCommandlineValues','.Values[CustOptName]:=OptionArg(I'+
  ')'#013#010+
  '      end'#013#010+
  '    else if (not CheckBuildOptionSetValue(I)) and (not Defaults.Ignore'+
  'InvalidOptions) then'#013#010+
  '      begin'#013#010+
  '      Usage(SErrInValidArgument,[I,ParamStr(I)]);'#013#010+
  '      end;'#013#010+
  '    end;'#013#010+
  '  If DefaultsFi','leName<>'#039#039' then'#013#010+
  '    Defaults.LocalInit(DefaultsFileName);'#013#010+
  '  Defaults.CompilerDefaults;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomInstaller.Usage(const FMT: String; Args: array of con'+
  'st);'#013#010+
  #013#010+
  '  Procedure LogCmd(const LC,Msg : String);'#013#010+
  '  begin'#013#010+
  '    Log(v','lInfo,Format('#039' %-12s %s'#039',[LC,MSG]));'#013#010+
  '  end;'#013#010+
  #013#010+
  '  Procedure LogOption(const C,LC,Msg : String);'#013#010+
  '  begin'#013#010+
  '    Log(vlInfo,Format('#039' -%s --%-16s %s'#039',[C,LC,MSG]));'#013#010+
  '  end;'#013#010+
  #013#010+
  '  Procedure LogArgOption(const C,LC,Msg : String);'#013#010+
  '  begin'#013#010+
  '    if t','rim(c)='#039#039' then'#013#010+
  '      Log(vlInfo,Format('#039'    --%-20s %s'#039',[LC+'#039'='#039'+SValu'+
  'e,MSG]))'#013#010+
  '    else'#013#010+
  '      Log(vlInfo,Format('#039' -%s --%-20s %s'#039',[C,LC+'#039'='#039'+SV'+
  'alue,MSG]));'#013#010+
  '  end;'#013#010+
  #013#010+
  'var'#013#010+
  '  i: Integer;'#013#010+
  'begin'#013#010+
  '  // Force the Usage to be displayed'#013#010+
  '  Inclu','de(FLogLevels,vlInfo);'#013#010+
  '  If (FMT<>'#039#039') then'#013#010+
  '    Log(vlInfo,Format(Fmt,Args));'#013#010+
  '  Log(vlInfo,Format(SHelpUsage,[Paramstr(0)]));'#013#010+
  '  Log(vlInfo,SHelpCommand);'#013#010+
  '  LogCmd('#039'compile'#039',SHelpCompile);'#013#010+
  '  LogCmd('#039'build'#039',SHelpBuild);'#013#010+
  '  LogCmd('#039'instal','l'#039',SHelpInstall);'#013#010+
  '  LogCmd('#039'clean'#039',SHelpClean);'#013#010+
  '  LogCmd('#039'archive'#039',SHelpArchive);'#013#010+
  '  LogCmd('#039'manifest'#039',SHelpManifest);'#013#010+
  '  LogCmd('#039'zipinstall'#039',SHelpZipInstall);'#013#010+
  '  Log(vlInfo,SHelpCmdOptions);'#013#010+
  '  LogOption('#039'h'#039','#039'help'#039',SHelpHelp);'#013#010+
  '  LogOpti','on('#039'l'#039','#039'list-commands'#039',SHelpList);'#013#010+
  '  LogOption('#039'n'#039','#039'nofpccfg'#039',SHelpNoFPCCfg);'#013#010+
  '  LogOption('#039'v'#039','#039'verbose'#039',SHelpVerbose);'#013#010+
  '{$ifdef HAS_UNIT_PROCESS}'#013#010+
  '  LogOption('#039'e'#039', '#039'useenv'#039', sHelpUseEnvironment);'#013#010+
  '{$endif}'#013#010+
  '  LogOption('#039'ie'#039','#039'installexampl','es'#039',SHelpInstExamples'+
  ');'#013#010+
  '  LogOption('#039'bu'#039','#039'buildunit'#039',SHelpUseBuildUnit);'#013#010+
  '  LogOption('#039'sp'#039','#039'skipcrossprograms'#039',SHelpSkipCrossPro'+
  'gs);'#013#010+
  '  LogArgOption('#039'C'#039','#039'cpu'#039',SHelpCPU);'#013#010+
  '  LogArgOption('#039'O'#039','#039'os'#039',SHelpOS);'#013#010+
  '  LogArgOption('#039't'#039','#039'target'#039',SHelp','Target);'#013#010+
  '  LogArgOption('#039'P'#039','#039'prefix'#039',SHelpPrefix);'#013#010+
  '  LogArgOption('#039'B'#039','#039'baseinstalldir'#039',SHelpBaseInstalldi'+
  'r);'#013#010+
  '  LogArgOption('#039'UL'#039','#039'localunitdir'#039',SHelpLocalUnitdir);'+
  #013#010+
  '  LogArgOption('#039'UG'#039','#039'globalunitdir'#039',SHelpGlobalUnitdir'+
  ');'#013#010+
  '  LogArgOption','('#039'U'#039','#039'unitinstalldir'#039',SHelpUnitInstall'+
  'Dir);'#013#010+
  '  LogArgOption('#039'r'#039','#039'compiler'#039',SHelpCompiler);'#013#010+
  '  LogArgOption('#039'f'#039','#039'config'#039',SHelpConfig);'#013#010+
  '  LogArgOption('#039'o'#039','#039'options'#039',SHelpOptions);'#013#010+
  '  LogArgOption('#039'io'#039','#039'ignoreinvalidoption'#039',SHelpIgnoreI'+
  'nvOpt)',';'#013#010+
  '  LogArgOption('#039'd'#039', '#039'doc-folder'#039', sHelpFpdocOutputDir)'+
  ';'#013#010+
  '  LogArgOption('#039'zp'#039', '#039'zipprefix'#039', sHelpZipPrefix);'#013+
  #010+
  '{$ifndef NO_THREADING}'#013#010+
  '  LogArgOption('#039'T'#039', '#039'threads'#039', sHelpThreads);'#013#010+
  '{$endif NO_THREADING}'#013#010+
  '  if assigned(CustomFpmakeCommandl','ineOptions) then for i  := 0 to Cu'+
  'stomFpmakeCommandlineOptions.Count-1 do'#013#010+
  '    LogArgOption('#039' '#039',CustomFpmakeCommandlineOptions.Names[i],C'+
  'ustomFpmakeCommandlineOptions.ValueFromIndex[i]);'#013#010+
  '  Log(vlInfo,'#039#039');'#013#010+
  '  If (FMT<>'#039#039') then'#013#010+
  '    halt(1)'#013,#010+
  '  else'#013#010+
  '    halt(0);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomInstaller.Compile(Force: Boolean);'#013#010+
  'begin'#013#010+
  '  FBuildEngine.ForceCompile:=Force;'#013#010+
  '  FBuildEngine.Compile(Packages);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomInstaller.Clean(AllTargets: boolean);'#013#010+
  'begin'#013#010+
  '  ','BuildEngine.Clean(Packages, AllTargets);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomInstaller.Install;'#013#010+
  'begin'#013#010+
  '  BuildEngine.Install(Packages);'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TCustomInstaller.ZipInstall;'#013#010+
  'begin'#013#010+
  '  BuildEngine.ZipInstall(Packages);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedur','e TCustomInstaller.Archive;'#013#010+
  'begin'#013#010+
  '  // Force generation of manifest.xml, this is required for the reposi'+
  'tory'#013#010+
  '  BuildEngine.Manifest(Packages);'#013#010+
  '  BuildEngine.Archive(Packages);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomInstaller.Manifest;'#013#010+
  'begin'#013#010+
  '  Bu','ildEngine.Manifest(Packages);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCustomInstaller.CheckPackages;'#013#010+
  'begin'#013#010+
  '  If (Packages.Count=0) then'#013#010+
  '    Error(SErrNoPackagesDefined);'#013#010+
  '  // Check for other obvious errors ?'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TCustomInstaller.Run : Boo','lean;'#013#010+
  'begin'#013#010+
  '  Result:=True;'#013#010+
  '  try'#013#010+
  '    CheckPackages;'#013#010+
  '    CreateBuildEngine;'#013#010+
  '    Case RunMode of'#013#010+
  '      rmCompile : Compile(False);'#013#010+
  '      rmBuild   : Compile(True);'#013#010+
  '      rmInstall : Install;'#013#010+
  '      rmZipInstall : ZipInstall;'#013#010+
  '      ','rmArchive : Archive;'#013#010+
  '      rmClean    : Clean(False);'#013#010+
  '      rmDistClean: Clean(True);'#013#010+
  '      rmManifest : Manifest;'#013#010+
  '    end;'#013#010+
  '  except'#013#010+
  '    On E : Exception do'#013#010+
  '      begin'#013#010+
  '      Log(vlError,SErrInstaller);'#013#010+
  '      Log(vlError,E.Message)',';'#013#010+
  '      Result:=False;'#013#010+
  '      end;'#013#010+
  '  end;'#013#010+
  '  // Force returning an exitcode to the shell'#013#010+
  '  if not Result then'#013#010+
  '    ExitCode:=1;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                   ','             TFPCInstaller'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'constructor TFPCInstaller.Create(AOwner: TComponent);'#013#010+
  'begin'#013#010+
  '  if assigned(Defaults) then'#013#010+
  '    Error(SErrAlreadyInitialized);'#013#010+
  '  D','efaults:=TFPCDefaults.Create;'#013#010+
  '  inherited Create(AOwner);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                                 TBasicInstaller'#013#010+
  '*****************************************','***************************'+
  '********}'#013#010+
  #013#010+
  'constructor TBasicInstaller.Create(AOwner: TComponent);'#013#010+
  'begin'#013#010+
  '  if assigned(Defaults) then'#013#010+
  '    Error(SErrAlreadyInitialized);'#013#010+
  '  Defaults:=TBasicDefaults.Create;'#013#010+
  '  inherited Create(AOwner);'#013#010+
  'end;'#013,#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                                 TBuildEngine'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'constructor TBuildEngine.Cre','ate(AOwner: TComponent);'#013#010+
  'begin'#013#010+
  '  inherited Create(AOwner);'#013#010+
  '  // Maybe this should be the current directory ?'#013#010+
  '  // Or have it as a command-line option.'#013#010+
  '  // Would allow to put all '#039'installers'#039' in one dir and call t'+
  'hem'#013#010+
  '  // With --start','-dir=/path/to/sources.'#013#010+
  '  FStartDir:=includeTrailingPathDelimiter(GetCurrentDir);'#013#010+
  '  FExternalPackages:=TPackages.Create(TPackage);'#013#010+
  #013#010+
  '{$ifndef NO_THREADING}'#013#010+
  '  InitCriticalSection(FGeneralCriticalSection);'#013#010+
  '{$endif NO_THREADING}'#013#010+
  'end;'#013#010+
  #013#010+
  #013,#010+
  'destructor TBuildEngine.Destroy;'#013#010+
  'begin'#013#010+
  '  FreeAndNil(FExternalPackages);'#013#010+
  #013#010+
  '{$ifndef NO_THREADING}'#013#010+
  '  DoneCriticalsection(FGeneralCriticalSection);'#013#010+
  '{$endif NO_THREADING}'#013#010+
  #013#010+
  '  inherited Destroy;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.Error(con','st Msg: String);'#013#010+
  'begin'#013#010+
  '  Raise EInstallerError.Create(Msg);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.Error(const Fmt: String; const Args: array of c'+
  'onst);'#013#010+
  'begin'#013#010+
  '  Raise EInstallerError.CreateFmt(Fmt,Args);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.Exe','cuteCommand(const Cmd,Args : String; const'+
  ' Env: TStrings = nil; IgnoreError : Boolean = False);'#013#010+
  'Var'#013#010+
  '  E : Integer;'#013#010+
  '  cmdLine: string;'#013#010+
  '  ConsoleOutput: TMemoryStream;'#013#010+
  '  s: string;'#013#010+
  'begin'#013#010+
  '  Log(vlInfo,SInfoExecutingCommand,[Cmd,Args]);'#013,#010+
  '  if ListMode then'#013#010+
  '    Log(vlCommand,'#039'%s %s'#039',[Cmd,Args])'#013#010+
  '  else'#013#010+
  '    begin'#013#010+
  '      // We should check cmd for spaces, and move all after first spac'+
  'e to args.'#013#010+
  '      ConsoleOutput := TMemoryStream.Create;'#013#010+
  '      try'#013#010+
  '        {$ifdef HAS_UN','IT_PROCESS}'#013#010+
  '        E:=ExecuteFPC(Verbose, cmd, args, env, ConsoleOutput);'#013#010+
  '        {$else}'#013#010+
  '        E:=ExecuteProcess(cmd,args);'#013#010+
  '        {$endif}'#013#010+
  '        If (E<>0) and (not IgnoreError) then'#013#010+
  '          begin'#013#010+
  '            if trim(Args)<>',#039#039' then'#013#010+
  '              cmdLine := cmd + '#039' '#039' + trim(args)'#013#010+
  '            else'#013#010+
  '              cmdline := cmd;'#013#010+
  '            s := ParsecompilerOutput(ConsoleOutput,Verbose);'#013#010+
  '            Error(SErrExternalCommandFailed,[cmdLine,E,s]);'#013#010+
  '          ','end;'#013#010+
  '      finally'#013#010+
  '        ConsoleOutput.Free;'#013#010+
  '      end;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TBuildEngine.SysDirectoryExists(const ADir:string):Boolean;'#013+
  #010+
  'begin'#013#010+
  '  result:=SysUtils.DirectoryExists(ADir);'#013#010+
  '  if result then'#013#010+
  '    Log(vlDebug,SDbg','DirectoryExists,[ADir,SDbgFound])'#013#010+
  '  else'#013#010+
  '    Log(vlDebug,SDbgDirectoryExists,[ADir,SDbgNotFound]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TBuildEngine.SysFileExists(const AFileName:string):Boolean;'#013+
  #010+
  'begin'#013#010+
  '  result:=SysUtils.FileExists(AFileName);'#013#010+
  '  if res','ult then'#013#010+
  '    Log(vlDebug,SDbgFileExists,[AFileName,SDbgFound])'#013#010+
  '  else'#013#010+
  '    Log(vlDebug,SDbgFileExists,[AFileName,SDbgNotFound]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.SysCopyFile(Const Src,Dest : String);'#013#010+
  'Var'#013#010+
  '  D,S : String;'#013#010+
  '  Fin,FOut : T','FileStream;'#013#010+
  '  Count : Int64;'#013#010+
  '  A : Integer;'#013#010+
  '{$ifdef UNIX}'#013#010+
  '  FileStat: stat;'#013#010+
  '{$endif UNIX}'#013#010+
  'begin'#013#010+
  '  Log(vlInfo,SInfoCopyingFile,[Src,Dest]);'#013#010+
  '  FIn:=TFileStream.Create(Src,fmopenRead or fmShareDenyNone);'#013#010+
  '  Try'#013#010+
  '    D:=IncludeTrailingP','athDelimiter(Dest);'#013#010+
  '    If DirectoryExists(D) then'#013#010+
  '      S:=D+ExtractFileName(Src)'#013#010+
  '    else'#013#010+
  '      S:=Dest;'#013#010+
  '    FOut:=TFileStream.Create(S,fmCreate or fmShareDenyNone);'#013#010+
  '    Try'#013#010+
  '      Count:=Fout.CopyFrom(FIn,0);'#013#010+
  '      If (Count<>Fin.','Size) then'#013#010+
  '        Error(SErrCopyingFile,[Src,S]);'#013#010+
  '    Finally'#013#010+
  '      FreeAndNil(Fout);'#013#010+
  '    end;'#013#010+
  '    A:=FileGetDate(FIn.Handle);'#013#010+
  '    If (A=-1) then'#013#010+
  '      log(vlWarning,SWarnFailedToGetTime,[Src])'#013#010+
  '    else'#013#010+
  '      if FileSetDate(S,A)<>','0 then'#013#010+
  '        Log(vlWarning,SWarnFailedToSetTime,[S]);'#013#010+
  '{$ifdef UNIX}'#013#010+
  '    // Copy the file-access rights on Unix, especially the executable-'+
  'bit'#013#010+
  '    if FpStat(Src,FileStat) <> 0 then'#013#010+
  '      Log(vlWarning,SWarnCanNotGetAccessRights,[Src])',#013#010+
  '    else'#013#010+
  '      if FpChmod(s,FileStat.st_mode) <> 0 then'#013#010+
  '        Log(vlWarning,SWarnCanNotSetAccessRights,[S]);'#013#010+
  '{$endif UNIX}'#013#010+
  '  finally'#013#010+
  '    FreeAndNil(Fin);'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.SysMoveFile(Const Src,Dest : String',');'#013#010+
  'Var'#013#010+
  '  S : String;'#013#010+
  'begin'#013#010+
  '  If DirectoryExists(IncludeTrailingPathDelimiter(Dest)) then'#013#010+
  '    S:=IncludeTrailingPathDelimiter(Dest)+ExtractFileName(Src)'#013#010+
  '  else'#013#010+
  '    S:=Dest;'#013#010+
  '  If Not RenameFile(Src,S) then'#013#010+
  '    begin'#013#010+
  '      Try'#013#010+
  '     ','   SysCopyFile(Src,S);'#013#010+
  '        SysDeleteFile(Src);'#013#010+
  '      Except'#013#010+
  '        On E : Exception Do'#013#010+
  '          Error(SErrMovingFile,[Src,S]);'#013#010+
  '      end;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.SysDeleteFile(Const AFileName : String);'#013#010+
  'begin',#013#010+
  '  if not FileExists(AFileName) then'#013#010+
  '    Log(vldebug,SDbgFileDoesNotExist,[AFileName])'#013#010+
  '  else If Not DeleteFile(AFileName) then'#013#010+
  '    Error(SErrDeletingFile,[AFileName])'#013#010+
  '  else'#013#010+
  '    Log(vlInfo,SInfoDeletedFile,[AFileName]);'#013#010+
  'end;'#013#010+
  #013#010+
  'proc','edure TBuildEngine.SysDeleteDirectory(Const ADirectoryName: Stri'+
  'ng);'#013#010+
  'begin'#013#010+
  '  if not DirectoryExists(ADirectoryName) then'#013#010+
  '    Log(vldebug,SDbgDirectoryDoesNotExist,[ADirectoryName])'#013#010+
  '  else if not IsDirectoryEmpty(ADirectoryName) then'#013#010+
  '  ','  Log(vldebug,SDbgDirectoryNotEmpty,[ADirectoryName])'#013#010+
  '  else If Not RemoveDir(ADirectoryName) then'#013#010+
  '    Error(SErrRemovingDirectory,[ADirectoryName])'#013#010+
  '  else'#013#010+
  '    Log(vlInfo,SInfoRemovedDirectory,[ADirectoryName]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBu','ildEngine.SysDeleteTree(Const ADirectoryName: String);'#013+
  #010+
  #013#010+
  '  function IntRemoveTree(const ADirectoryName: String) : boolean;'#013#010+
  '  var'#013#010+
  '    searchRec: TSearchRec;'#013#010+
  '    SearchResult: longint;'#013#010+
  '    s: string;'#013#010+
  '  begin'#013#010+
  '    result := true;'#013#010+
  '    Se','archResult := FindFirst(IncludeTrailingPathDelimiter(ADirector'+
  'yName)+AllFilesMask, faAnyFile+faSymLink, searchRec);'#013#010+
  '    try'#013#010+
  '      while SearchResult=0 do'#013#010+
  '        begin'#013#010+
  '          if (searchRec.Name<>'#039'.'#039') and (searchRec.Name<>'#039'.'+
  '.'#039') then'#013#010,
  '             begin'#013#010+
  '               s := IncludeTrailingPathDelimiter(ADirectoryName)+searc'+
  'hRec.Name;'#013#010+
  '               if (searchRec.Attr and faDirectory)=faDirectory then'#013+
  #010+
  '                 begin'#013#010+
  '                   if not IntRemoveTree(s) th','en'#013#010+
  '                     result := false;'#013#010+
  '                 end'#013#010+
  '               else if not DeleteFile(s) then'#013#010+
  '                 result := False'#013#010+
  '               else'#013#010+
  '                 log(vldebug, SDbgDeletedFile, [s]);'#013#010+
  '             end;'#013#010,
  '          SearchResult := FindNext(searchRec);'#013#010+
  '        end;'#013#010+
  '    finally'#013#010+
  '      FindClose(searchRec);'#013#010+
  '    end;'#013#010+
  '    if not RemoveDir(ADirectoryName) then'#013#010+
  '      result := false'#013#010+
  '    else'#013#010+
  '      log(vldebug, SDbgRemovedDirectory, [ADirecto','ryName]);'#013#010+
  '  end;'#013#010+
  #013#010+
  'begin'#013#010+
  '  if not DirectoryExists(ADirectoryName) then'#013#010+
  '    Log(vldebug,SDbgDirectoryDoesNotExist,[ADirectoryName])'#013#010+
  '  else If Not IntRemoveTree(ADirectoryName) then'#013#010+
  '    Error(SErrRemovingDirectory,[ADirectoryName])'#013#010+
  '  e','lse'#013#010+
  '    Log(vlInfo,SInfoRemovedDirectory,[ADirectoryName]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.SysArchiveFiles(List: TStrings;Const AFileName:'+
  ' String);'#013#010+
  'begin'#013#010+
  '  If Not (Assigned(OnArchivefiles) or Assigned(ArchiveFilesProc)) then'+
  #013#010+
  '    Rais','e EInstallerError.Create(SErrNoArchiveSupport);'#013#010+
  '  If Assigned(ArchiveFilesProc) then'#013#010+
  '    ArchiveFilesProc(AFileName,List)'#013#010+
  '  else'#013#010+
  '    OnArchiveFiles(AFileName,List);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.LogIndent;'#013#010+
  'begin'#013#010+
  '  GLogPrefix:=GLog','Prefix+'#039'  '#039';'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.LogUnIndent;'#013#010+
  'begin'#013#010+
  '  Delete(GLogPrefix,1,2);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.Log(Level: TVerboseLevel; Msg: String);'#013#010+
  'begin'#013#010+
  '  If Assigned(FOnLog) then'#013#010+
  '    begin'#013#010+
  '      if Level in [vlInfo',',vlDebug] then'#013#010+
  '        FOnLog(Level,GLogPrefix+Msg)'#013#010+
  '      else'#013#010+
  '        FOnLog(Level,Msg);'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.Log(Level: TVerboseLevel; Fmt: String;const Arg'+
  's: array of const);'#013#010+
  'begin'#013#010+
  '  Log(Level,Format(Fmt,Args)',');'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.EnterDir(ADir: String);'#013#010+
  'Var'#013#010+
  '  D : String;'#013#010+
  'begin'#013#010+
  '  D:=FStartDir;'#013#010+
  '  D:=D+ADir;'#013#010+
  '  Log(vlDebug,SDbgEnterDir,[D]);'#013#010+
  '  If Not SetCurrentDir(D) then'#013#010+
  '    Error(SErrChangeDirFailed,[D]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedu','re TBuildEngine.CmdCopyFiles(List: TStrings; Const DestDir: S'+
  'tring);'#013#010+
  #013#010+
  'Var'#013#010+
  '  Args : String;'#013#010+
  '  I : Integer;'#013#010+
  '  DestFileName : String;'#013#010+
  'begin'#013#010+
  '  // When the files should be written to an archive, add them'#013#010+
  '  if assigned(FZipper) then'#013#010+
  '    ','begin'#013#010+
  '      For I:=0 to List.Count-1 do'#013#010+
  '        if List.Names[i]<>'#039#039' then'#013#010+
  '          begin'#013#010+
  '            if IsRelativePath(list.ValueFromIndex[i]) then'#013#010+
  '              DestFileName:=DestDir+list.ValueFromIndex[i]'#013#010+
  '            else'#013#010+
  '        ','      DestFileName:=list.ValueFromIndex[i];'#013#010+
  '            FZipper.Entries.AddFileEntry(List.names[i], DestFileName);'+
  #013#010+
  '          end'#013#010+
  '        else'#013#010+
  '          FZipper.Entries.AddFileEntry(List[i], DestDir+ExtractFileNam'+
  'e(List[i]));'#013#010+
  '      Exit',';'#013#010+
  '    end;'#013#010+
  '  {$ifdef HAS_TAR_SUPPORT}'#013#010+
  '  if assigned(FTarWriter) then'#013#010+
  '    begin'#013#010+
  '      For I:=0 to List.Count-1 do'#013#010+
  '        if List.Names[i]<>'#039#039' then'#013#010+
  '          begin'#013#010+
  '            if IsRelativePath(list.ValueFromIndex[i]) then'#013#010+
  '         ','     DestFileName:=DestDir+list.ValueFromIndex[i]'#013#010+
  '            else'#013#010+
  '              DestFileName:=list.ValueFromIndex[i];'#013#010+
  '            FTarWriter.AddFile(List.names[i], DestFileName);'#013#010+
  '          end'#013#010+
  '        else'#013#010+
  '          FTarWriter.AddFil','e(List[i], DestDir+ExtractFileName(List[i'+
  ']));'#013#010+
  '      Exit;'#013#010+
  '    end;'#013#010+
  '  {$endif HAS_TAR_SUPPORT}'#013#010+
  #013#010+
  '  // Copy the files to their new location on disk'#013#010+
  '  CmdCreateDir(DestDir);'#013#010+
  '  If (Defaults.Copy<>'#039#039') then'#013#010+
  '    begin'#013#010+
  '      Args:=FileListT','oString(List,'#039#039');'#013#010+
  '      Args:=Args+'#039' '#039'+DestDir;'#013#010+
  '      ExecuteCommand(Defaults.Copy,Args);'#013#010+
  '    end'#013#010+
  '  else'#013#010+
  '    For I:=0 to List.Count-1 do'#013#010+
  '      if List.Names[i]<>'#039#039' then'#013#010+
  '        begin'#013#010+
  '          if IsRelativePath(list.ValueFromIndex[i',']) then'#013#010+
  '            DestFileName:=DestDir+list.ValueFromIndex[i]'#013#010+
  '          else'#013#010+
  '            DestFileName:=list.ValueFromIndex[i];'#013#010+
  '          CmdCreateDir(ExtractFilePath(DestFileName));'#013#010+
  '          SysCopyFile(List.names[i],DestFileName)'#013,#010+
  '        end'#013#010+
  '      else'#013#010+
  '        SysCopyFile(List[i],DestDir);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.CmdCreateDir(const DestDir: String);'#013#010+
  'begin'#013#010+
  '  If (Defaults.MkDir<>'#039#039') then'#013#010+
  '    ExecuteCommand(Defaults.MkDir,DestDir)'#013#010+
  '  else'#013#010+
  '    If not Fo','rceDirectories(DestDir) then'#013#010+
  '      Error(SErrCreatingDirectory,[DestDir]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.CmdMoveFiles(List: TStrings; Const DestDir: Str'+
  'ing);'#013#010+
  'Var'#013#010+
  '  Args : String;'#013#010+
  '  I : Integer;'#013#010+
  'begin'#013#010+
  '  CmdCreateDir(DestDir);'#013#010+
  '  If',' (Defaults.Move<>'#039#039') then'#013#010+
  '    begin'#013#010+
  '      Args:=FileListToString(List,'#039#039');'#013#010+
  '      Args:=Args+'#039' '#039'+DestDir;'#013#010+
  '      ExecuteCommand(Defaults.Move,Args);'#013#010+
  '    end'#013#010+
  '  else'#013#010+
  '    For I:=0 to List.Count-1 do'#013#010+
  '      SysMoveFile(List[i],DestDir);'#013#010+
  'e','nd;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.CmdDeleteFiles(List: TStrings);'#013#010+
  'Var'#013#010+
  '  Args : String;'#013#010+
  '  I : Integer;'#013#010+
  'begin'#013#010+
  '  If (Defaults.Remove<>'#039#039') then'#013#010+
  '    begin'#013#010+
  '      Args:=FileListToString(List,'#039#039');'#013#010+
  '      ExecuteCommand(Defaults.Remove,Args);'#013#010,
  '    end'#013#010+
  '  else'#013#010+
  '    For I:=0 to List.Count-1 do'#013#010+
  '      SysDeleteFile(List[i]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.CmdArchiveFiles(List: TStrings; Const ArchiveFi'+
  'le: String);'#013#010+
  'Var'#013#010+
  '  S,C,O : String;'#013#010+
  'begin'#013#010+
  '  If (Defaults.Archive='#039#039') then'#013#010+
  ' ','   SysArchiveFiles(List,ArchiveFile)'#013#010+
  '  else'#013#010+
  '    begin'#013#010+
  '      S:=FileListToString(List,'#039#039');'#013#010+
  '      SplitCommand(Defaults.Archive,C,O);'#013#010+
  '      If (O='#039#039') then'#013#010+
  '        O:=ArchiveFile+'#039' '#039'+S'#013#010+
  '      else'#013#010+
  '        O:=GlobalDictionary.Substitute(','O,['#039'ARCHIVE'#039',ArchiveF'+
  'ile,'#039'FILESORDIRS'#039']);'#013#010+
  '      ExecuteCommand(C,O);'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TBuildEngine.CmdRenameFile(SourceName, DestName: String);'#013+
  #010+
  'var'#013#010+
  '  Args: string;'#013#010+
  'begin'#013#010+
  '  If (Defaults.Move<>'#039#039') then'#013#010+
  '    begin'#013#010+
  '      Args:','=SourceName;'#013#010+
  '      Args:=Args+'#039' '#039'+DestName;'#013#010+
  '      ExecuteCommand(Defaults.Move,Args);'#013#010+
  '    end'#013#010+
  '  else'#013#010+
  '    SysMoveFile(SourceName,DestName);'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TBuildEngine.CmdRemoveDirs(List: TStrings);'#013#010+
  'Var'#013#010+
  '  Args : String;'#013#010+
  '  I : Int','eger;'#013#010+
  'begin'#013#010+
  '  If (Defaults.RemoveDir<>'#039#039') then'#013#010+
  '    begin'#013#010+
  '      Args:=FileListToString(List,'#039#039');'#013#010+
  '      ExecuteCommand(Defaults.RemoveDir,Args);'#013#010+
  '    end'#013#010+
  '  else'#013#010+
  '    For I:=0 to List.Count-1 do'#013#010+
  '      SysDeleteDirectory(List[i]);'#013#010+
  'end;'#013,#010+
  #013#010+
  'procedure TBuildEngine.CmdRemoveTrees(List: TStrings);'#013#010+
  'Var'#013#010+
  '  Args : String;'#013#010+
  '  I : Integer;'#013#010+
  'begin'#013#010+
  '  If (Defaults.RemoveTree<>'#039#039') then'#013#010+
  '    begin'#013#010+
  '      Args:=FileListToString(List,'#039#039');'#013#010+
  '      ExecuteCommand(Defaults.RemoveTree,Args);',#013#010+
  '    end'#013#010+
  '  else'#013#010+
  '    For I:=0 to List.Count-1 do'#013#010+
  '      SysDeleteTree(List[i]);'#013#010+
  'end;'#013#010+
  #013#010+
  'Function TBuildEngine.FileNewer(const Src,Dest : String) : Boolean;'#013+
  #010+
  #013#010+
  'Var'#013#010+
  '  DS,DD : Longint;'#013#010+
  '  D1,D2 : TDateTime;'#013#010+
  #013#010+
  'begin'#013#010+
  '  DS:=FileAge(Src);'#013#010+
  ' ',' { Return false if file not found or not accessible }'#013#010+
  '  if DS=-1 then'#013#010+
  '    begin'#013#010+
  '      Log(vlWarning,SWarnCanNotGetFileAge,[Src]);'#013#010+
  '      Result:=false;'#013#010+
  '      exit;'#013#010+
  '    end;'#013#010+
  '  DD:=FileAge(Dest);'#013#010+
  '  D1:=FileDateToDateTime(DS);'#013#010+
  '  D2:=Fi','leDateToDateTime(DD);'#013#010+
  '  Log(vlDebug,SDbgComparingFileTimes,[Src,DateTimeToStr(D1),Dest,DateT'+
  'imeToStr(D2)]);'#013#010+
  '  Result:=D1>D2;'#013#010+
  '  If Result then'#013#010+
  '    Log(vlInfo,SInfoSourceNewerDest,[Src,DateTimeToStr(D1),Dest,DateTi'+
  'meToStr(D2)]);'#013#010+
  'end;'#013#010+
  #013#010,
  #013#010+
  'procedure TBuildEngine.ExecuteCommands(Commands: TCommands; At: TComma'+
  'ndAt; APackage: TPackage);'#013#010+
  'Var'#013#010+
  '  C : TCommand;'#013#010+
  '  I : Integer;'#013#010+
  '  Cmd,O : String;'#013#010+
  '  E : Boolean;'#013#010+
  '  ADictionary: TDictionary;'#013#010+
  '  SourceFile, DestFile: string;'#013#010+
  'begin',#013#010+
  '  For I:=0 to Commands.Count-1 do'#013#010+
  '    begin'#013#010+
  '      C:=Commands.CommandItems[i];'#013#010+
  '      if (C.At=At) then'#013#010+
  '        begin'#013#010+
  '          E:=True;'#013#010+
  #013#010+
  '          if assigned(APackage) then'#013#010+
  '            ADictionary := APackage.Dictionary'#013#010+
  '        ','  else'#013#010+
  '            ADictionary := GlobalDictionary;'#013#010+
  '          SourceFile := ADictionary.ReplaceStrings(C.SourceFile);'#013#010+
  '          DestFile := ADictionary.ReplaceStrings(C.DestFile);'#013#010+
  '          if IsRelativePath(SourceFile) then'#013#010+
  '          ','  SourceFile := AddPathPrefix(APackage,SourceFile);'#013#010+
  '          if IsRelativePath(DestFile) then'#013#010+
  '            DestFile := AddPathPrefix(APackage,DestFile);'#013#010+
  #013#010+
  '          Cmd:=C.Command;'#013#010+
  '          If (ExtractFilePath(Cmd)='#039#039') then'#013#010+
  '          ','  Cmd:=ExeSearch(Cmd,GetEnvironmentvariable('#039'PATH'#039')'+
  ');'#013#010+
  #013#010+
  '          If (SourceFile<>'#039#039') and (DestFile<>'#039#039')  then'#013+
  #010+
  '            begin'#013#010+
  '              if not FileExists(DestFile) then'#013#010+
  '                Log(vlInfo,SInfoDestDoesNotExist,[DestFile])',#013#010+
  '              else'#013#010+
  '                begin'#013#010+
  '                E:=FileNewer(SourceFile, DestFile);'#013#010+
  '                if E and (cmd = '#039#039') then'#013#010+
  '                  begin'#013#010+
  '                  log(vlWarning,SWarnExtCommandNotFound,[C.Command,Des'+
  'tFile',',SourceFile]);'#013#010+
  '                  E := False;'#013#010+
  '                  end;'#013#010+
  '                end;'#013#010+
  '            end;'#013#010+
  '          If E then'#013#010+
  '            begin'#013#010+
  '            if Cmd = '#039#039' then'#013#010+
  '              error(SErrExtCommandNotFound,[C.Command]);'#013#010+
  #013,#010+
  '            If Assigned(C.BeforeCommand) then'#013#010+
  '              C.BeforeCommand(C);'#013#010+
  '            O:=ADictionary.Substitute(C.CmdLineOptions,['#039'SOURCE'#039+
  ',SourceFile,'#039'DEST'#039',DestFile]);'#013#010+
  #013#010+
  '            Log(vlCommand,SInfoExecutingCommand,[Cmd,O]);'#013#010,
  '            ExecuteCommand(Cmd,O,nil,C.IgnoreResult);'#013#010+
  '            If Assigned(C.AfterCommand) then'#013#010+
  '              C.AfterCommand(C);'#013#010+
  '            end;'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.LogSearchPath(APackage: TPacka','ge; const ASear'+
  'chPathName: string; Path: TConditionalStrings; ACPU: TCPU; AOS: TOS);'#013+
  #010+
  'var'#013#010+
  '  S : String;'#013#010+
  '  I : Integer;'#013#010+
  '  C : TConditionalString;'#013#010+
  'begin'#013#010+
  '  S:='#039#039';'#013#010+
  '  for i:=0 to Path.Count-1 do'#013#010+
  '    begin'#013#010+
  '      C:=Path[I];'#013#010+
  '      if (ACP','U in C.CPUs) and (AOS in C.OSes) then'#013#010+
  '        begin'#013#010+
  '          if S<>'#039#039' then'#013#010+
  '            S:=S+PathSeparator;'#013#010+
  '          S:=S+APackage.Dictionary.ReplaceStrings(C.Value)'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  '  if S<>'#039#039' then'#013#010+
  '    Log(vlDebug,SDbgSearchP','ath,[ASearchPathName,S]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TBuildEngine.FindFileInPath(APackage: TPackage; Path:TConditi'+
  'onalStrings; AFileName:String; var FoundPath:String;ACPU:TCPU;AOS:TOS)'+
  ':Boolean;'#013#010+
  'var'#013#010+
  '  I : Integer;'#013#010+
  '  C : TConditionalString;'#013#010+
  'begi','n'#013#010+
  '  Result:=false;'#013#010+
  '  for i:=0 to Path.Count-1 do'#013#010+
  '    begin'#013#010+
  '      C:=Path[I];'#013#010+
  '      if (ACPU in C.CPUs) and (AOS in C.OSes) then'#013#010+
  '        begin'#013#010+
  '          FoundPath:=IncludeTrailingPathDelimiter(APackage.Dictionary.'+
  'ReplaceStrings(C.Valu','e));'#013#010+
  '          if FileExists(AddPathPrefix(APackage,FoundPath+AFileName)) t'+
  'hen'#013#010+
  '            begin'#013#010+
  '              result:=true;'#013#010+
  '              exit;'#013#010+
  '            end;'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  '  FoundPath:='#039#039';'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TBuildEngine','.GetDirectoriesFromFilelist(const AFileList, A'+
  'DirectoryList: TStringList);'#013#010+
  'var'#013#010+
  '  i: integer;'#013#010+
  'begin'#013#010+
  '  ADirectoryList.Sorted:=true;'#013#010+
  '  ADirectoryList.Duplicates:=dupIgnore;'#013#010+
  '  for i := 0 to AFileList.Count-1 do'#013#010+
  '    ADirectoryList.Add(Ext','ractFileDir(AFileList.Strings[i]));'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TBuildEngine.AddPackageMacrosToDictionary(const APackage: TP'+
  'ackage; ADictionary: TDictionary);'#013#010+
  'begin'#013#010+
  '  APackage.Dictionary.AddVariable('#039'UNITSOUTPUTDIR'#039',AddPathPref'+
  'ix(APackage,APackage','.GetUnitsOutputDir(Defaults.CPU,Defaults.OS)));'#013+
  #010+
  '  APackage.Dictionary.AddVariable('#039'BINOUTPUTDIR'#039',AddPathPrefix'+
  '(APackage,APackage.GetBinOutputDir(Defaults.CPU,Defaults.OS)));'#013#010+
  'end;'#013#010+
  #013#010+
  'Procedure TBuildEngine.ResolveFileNames(APackage : TPack','age; ACPU:TC'+
  'PU;AOS:TOS;DoChangeDir:boolean=true; WarnIfNotFound:boolean=true);'#013+
  #010+
  #013#010+
  '  procedure FindMainSource(T:TTarget);'#013#010+
  '  var'#013#010+
  '    SD,SF  : String;'#013#010+
  '  begin'#013#010+
  '    LogSearchPath(APackage,'#039'package source'#039',APackage.SourcePat'+
  'h,ACPU,AOS);'#013#010+
  '    ','SD:=APackage.Dictionary.ReplaceStrings(T.Directory);'#013#010+
  '    SF:=APackage.Dictionary.ReplaceStrings(T.SourceFileName);'#013#010+
  '    if SD='#039#039' then'#013#010+
  '      FindFileInPath(APackage,APackage.SourcePath,SF,SD,ACPU,AOS);'#013+
  #010+
  '    if SD<>'#039#039' then'#013#010+
  '      SD:=Includ','eTrailingPathDelimiter(SD);'#013#010+
  '    T.FTargetSourceFileName:=SD+SF;'#013#010+
  '    if FileExists(AddPathPrefix(APackage,T.TargetSourceFileName)) then'+
  #013#010+
  '      Log(vlDebug,SDbgResolvedSourceFile,[T.SourceFileName,T.TargetSou'+
  'rceFileName])'#013#010+
  '    else'#013#010+
  '      b','egin'#013#010+
  '        if WarnIfNotFound then'#013#010+
  '          Log(vlWarning,SWarnSourceFileNotFound,[T.SourceFileName,APac'+
  'kage.Name,MakeTargetString(ACPU,AOS)]);'#013#010+
  '        APackage.FAllFilesResolved:=false;'#013#010+
  '        T.FTargetSourceFileName:='#039#039';'#013#010+
  '      end',';'#013#010+
  '  end;'#013#010+
  #013#010+
  '  procedure FindIncludeSources(T:TTarget);'#013#010+
  '  var'#013#010+
  '    SD,SF  : String;'#013#010+
  '    D : TDependency;'#013#010+
  '    j : integer;'#013#010+
  '  begin'#013#010+
  '    LogSearchPath(APackage,'#039'target include'#039',T.IncludePath,ACPU'+
  ',AOS);'#013#010+
  '    LogSearchPath(APackage,'#039'package',' include'#039',APackage.Includ'+
  'ePath,ACPU,AOS);'#013#010+
  '    for j:=0 to T.Dependencies.Count-1 do'#013#010+
  '      begin'#013#010+
  '        D:=T.Dependencies[j];'#013#010+
  '        if (D.DependencyType=depInclude) then'#013#010+
  '          begin'#013#010+
  '            if D.TargetFileName<>'#039#039' then'#013#010+
  '    ','          Log(vlDebug,SDbgSourceAlreadyResolved,[D.Value])'#013#010+
  '            else if (ACPU in D.CPUs) and (AOS in D.OSes) then'#013#010+
  '              begin'#013#010+
  '                if ExtractFilePath(D.Value)='#039#039' then'#013#010+
  '                  begin'#013#010+
  '                  ','  SF:=APAckage.Dictionary.ReplaceStrings(D.Value);'+
  #013#010+
  '                    SD:='#039#039';'#013#010+
  '                    // first check the target specific path'#013#010+
  '                    if not FindFileInPath(APackage, T.IncludePath,SF,S'+
  'D,ACPU,AOS) then'#013#010+
  '          ','            FindFileInPath(APackage, APackage.IncludePath,'+
  'SF,SD,ACPU,AOS);'#013#010+
  '                     if SD<>'#039#039' then'#013#010+
  '                       SD:=IncludeTrailingPathDelimiter(SD);'#013#010+
  '                     D.TargetFileName:=SD+SF;'#013#010+
  '                  ','end'#013#010+
  '                else'#013#010+
  '                  D.TargetFileName:=D.Value;'#013#010+
  '                if FileExists(AddPathPrefix(APackage,D.TargetFileName)'+
  ') then'#013#010+
  '                  Log(vlDebug,SDbgResolvedIncludeFile,[D.Value,D.Targe'+
  'tFileName])'#013#010+
  '      ','          else'#013#010+
  '                  begin'#013#010+
  '                    if WarnIfNotFound then'#013#010+
  '                      Log(vlWarning,SWarnIncludeFileNotFound,[D.Value,'+
  ' APackage.Name, MakeTargetString(ACPU,AOS)]);'#013#010+
  '                    APackage.FAllFiles','Resolved:=false;'#013#010+
  '                    D.TargetFileName:='#039#039';'#013#010+
  '                  end;'#013#010+
  '              end;'#013#010+
  '          end;'#013#010+
  '      end;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  procedure FindExampleSource(T:TTarget);'#013#010+
  '  var'#013#010+
  '    SD,SF  : String;'#013#010+
  '  begin'#013#010+
  '    LogSearchPat','h(APackage,'#039'package example'#039',APackage.Exampl'+
  'ePath,ACPU,AOS);'#013#010+
  '    SD:=APackage.Dictionary.ReplaceStrings(T.Directory);'#013#010+
  '    SF:=APackage.Dictionary.ReplaceStrings(T.SourceFileName);'#013#010+
  '    if SD='#039#039' then'#013#010+
  '      FindFileInPath(APackage, APackag','e.ExamplePath,SF,SD,ACPU,AOS);'+
  #013#010+
  '    if SD<>'#039#039' then'#013#010+
  '      SD:=IncludeTrailingPathDelimiter(SD);'#013#010+
  '    T.FTargetSourceFileName:=SD+SF;'#013#010+
  '    if FileExists(AddPathPrefix(APackage,T.TargetSourceFileName)) then'+
  #013#010+
  '      Log(vlDebug,SDbgResolvedSour','ceFile,[T.SourceFileName,T.TargetS'+
  'ourceFileName])'#013#010+
  '    else'#013#010+
  '      begin'#013#010+
  '        if WarnIfNotFound then'#013#010+
  '          Log(vlWarning,SWarnSourceFileNotFound,[T.SourceFileName, APa'+
  'ckage.Name, MakeTargetString(ACPU,AOS)]);'#013#010+
  '        T.FTargetSour','ceFileName:='#039#039';'#013#010+
  '        APackage.FAllFilesResolved:=false;'#013#010+
  '      end;'#013#010+
  '  end;'#013#010+
  #013#010+
  'var'#013#010+
  '  T : TTarget;'#013#010+
  '  i : Integer;'#013#010+
  'begin'#013#010+
  '  if not((ACPU in APackage.CPUs) and (AOS in APackage.OSes)) then'#013#010+
  '    exit;'#013#010+
  '  if APackage.FAllFilesResolved the','n'#013#010+
  '    Exit;'#013#010+
  '  APackage.FAllFilesResolved:=true;'#013#010+
  '  try'#013#010+
  '    if DoChangeDir and (APackage.Directory<>'#039#039') then'#013#010+
  '      GPathPrefix := APackage.Directory;'#013#010+
  '    APackage.Dictionary.AddVariable('#039'CPU'#039',CPUToString(ACPU));'#013+
  #010+
  '    APackage.Dictionary','.AddVariable('#039'OS'#039',OSToString(AOS));'#013+
  #010+
  '    For I:=0 to APackage.Targets.Count-1 do'#013#010+
  '      begin'#013#010+
  '        T:=APackage.FTargets.TargetItems[I];'#013#010+
  '        if (ACPU in T.CPUs) and (AOS in T.OSes) then'#013#010+
  '          begin'#013#010+
  '            // Debug informa','tion'#013#010+
  '            Log(vlDebug,SDbgResolvingSourcesOfTarget,[T.Name,MakeTarge'+
  'tString(ACPU,AOS)]);'#013#010+
  '            LogIndent;'#013#010+
  #013#010+
  '            case T.TargetType of'#013#010+
  '              ttProgram,'#013#010+
  '              ttUnit,'#013#010+
  '              ttImplicitUnit :'#013#010+
  ' ','               begin'#013#010+
  '                  if T.FTargetSourceFileName<>'#039#039' then'#013#010+
  '                    Log(vlDebug,SDbgSourceAlreadyResolved,[T.Name])'#013+
  #010+
  '                  else'#013#010+
  '                    FindMainSource(T);'#013#010+
  '                  if T.Depende','ncies.Count>0 then'#013#010+
  '                    FindIncludeSources(T);'#013#010+
  '                end;'#013#010+
  '              ttExampleUnit,'#013#010+
  '              ttExampleProgram :'#013#010+
  '                begin'#013#010+
  '                  if T.FTargetSourceFileName<>'#039#039' then'#013#010+
  '            ','        Log(vlDebug,SDbgSourceAlreadyResolved,[T.Name])'#013+
  #010+
  '                  else'#013#010+
  '                    FindExampleSource(T);'#013#010+
  '                end;'#013#010+
  '            end;'#013#010+
  #013#010+
  '            LogUnIndent;'#013#010+
  '          end;'#013#010+
  '      end;'#013#010+
  '  finally'#013#010+
  '    If Do','ChangeDir and (APackage.Directory<>'#039#039') then'#013#010+
  '      GPathPrefix := '#039#039';'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TBuildEngine.GetUnitDir(APackage:TPackage):String;'#013#010+
  'begin'#013#010+
  '  if APackage.UnitDir='#039#039' then'#013#010+
  '    begin'#013#010+
  '      // Retrieve Full directory name whe','re to find the units.'#013#010+
  '      // The search order is:'#013#010+
  '      //  - Package in this fpmake.pp'#013#010+
  '      //  - LocalUnitDir'#013#010+
  '      //  - GlobalUnitDir'#013#010+
  '      if (APackage.State in [tsCompiled, tsNoCompile, tsInstalled]) th'+
  'en'#013#010+
  '        begin'#013#010+
  '     ','     APackage.UnitDir:=IncludeTrailingPathDelimiter(FStartDir)+'+
  'IncludeTrailingPathDelimiter(APackage.Directory)+APackage.GetUnitsOutp'+
  'utDir(Defaults.CPU,Defaults.OS);'#013#010+
  '        end;'#013#010+
  '      if (APackage.UnitDir='#039#039') and'#013#010+
  '         (Defaults.Loca','lUnitDir<>'#039#039') then'#013#010+
  '        begin'#013#010+
  '          APackage.UnitDir:=IncludeTrailingPathDelimiter(Defaults.Loca'+
  'lUnitDir)+APackage.Name+PathDelim+APackage.GetPackageUnitInstallDir(de'+
  'faults.CPU, Defaults.OS);'#013#010+
  '          if not SysDirectoryExists(AP','ackage.UnitDir) then'#013#010+
  '            APackage.UnitDir:='#039#039';'#013#010+
  '        end;'#013#010+
  '      if APackage.UnitDir='#039#039' then'#013#010+
  '        begin'#013#010+
  '          APackage.UnitDir:=IncludeTrailingPathDelimiter(Defaults.Glob'+
  'alUnitDir)+APackage.Name+PathDelim+APackage.GetPa','ckageUnitInstallDir'+
  '(defaults.CPU, Defaults.OS);'#013#010+
  '          if not SysDirectoryExists(APackage.UnitDir) then'#013#010+
  '            APackage.UnitDir:=DirNotFound;'#013#010+
  '        end;'#013#010+
  #013#010+
  '      if (APackage.UnitDir<>DirNotFound) then'#013#010+
  '        begin'#013#010+
  '         ',' if FileExists(IncludeTrailingPathDelimiter(APackage.UnitDi'+
  'r)+FPMakePPFile) then'#013#010+
  '            begin'#013#010+
  '              // The package is not installed, but the source-path is '+
  'detected.'#013#010+
  '              // It is an external package so it is imposs','ible to co'+
  'mpile it, so'#013#010+
  '              // assume that it has been compiled earlier.'#013#010+
  '              APackage.UnitDir := IncludeTrailingPathDelimiter(APackag'+
  'e.UnitDir) + APackage.GetUnitsOutputDir(Defaults.CPU,Defaults.OS);'#013+
  #010+
  '              // I','f the unit-directory does not exist, you know for '+
  'sure that'#013#010+
  '              // the package is not compiled'#013#010+
  '              if not SysDirectoryExists(APackage.UnitDir) then'#013#010+
  '                APackage.UnitDir:=DirNotFound'#013#010+
  '              else'#013#010+
  '  ','              // Set the state to tsNoCompile and not tsCompiled. '+
  'Because packages'#013#010+
  '                // in the tsCompiled state trigger a rebuild of packag'+
  'es that depend'#013#010+
  '                // on it.'#013#010+
  '                APackage.FTargetState:=tsNo','Compile;'#013#010+
  '            end'#013#010+
  '          else if not (APackage.FTargetState in [tsCompiled, tsNoCompi'+
  'le]) then'#013#010+
  '            begin'#013#010+
  '              APackage.FTargetState:=tsInstalled;'#013#010+
  '            end;'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  #013#010+
  '  // Special error',' marker to prevent searches in case of error'#013#010+
  '  if APackage.UnitDir=DirNotFound then'#013#010+
  '    Result:='#039#039#013#010+
  '  else'#013#010+
  '    Result:=APackage.UnitDir;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.AddDependencyPaths(L: TStrings; DependencyType:'+
  ' TDependencyType; A','Target: TTarget);'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  '  D : TDependency;'#013#010+
  '  SD : String;'#013#010+
  'begin'#013#010+
  '  For I:=0 to ATarget.Dependencies.Count-1 do'#013#010+
  '    begin'#013#010+
  '      D:=ATarget.Dependencies[i];'#013#010+
  '      if (D.DependencyType=DependencyType) and'#013#010+
  '         (Defaul','ts.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#013+
  #010+
  '        begin'#013#010+
  '          SD:=ExcludeTrailingPathDelimiter(ExtractFilePath(D.TargetFil'+
  'eName));'#013#010+
  '          if SD<>'#039#039' then'#013#010+
  '            L.Add(SD);'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure',' TBuildEngine.AddDependencyUnitPaths(L:TStrings;APackage: T'+
  'Package);'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  '  P : TPackage;'#013#010+
  '  D : TDependency;'#013#010+
  '  S : String;'#013#010+
  'begin'#013#010+
  '  For I:=0 to APackage.Dependencies.Count-1 do'#013#010+
  '    begin'#013#010+
  '      D:=APackage.Dependencies[','i];'#013#010+
  '      if (D.DependencyType=depPackage) and'#013#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#013+
  #010+
  '        begin'#013#010+
  '          P:=TPackage(D.Target);'#013#010+
  '          If Assigned(P) then'#013#010+
  '            begin'#013#010+
  '              // Already',' processed?'#013#010+
  '              S:=GetUnitDir(P);'#013#010+
  '              if L.IndexOf(S)=-1 then'#013#010+
  '                begin'#013#010+
  '                  // Add this package and then dependencies'#013#010+
  '                  L.Add(S);'#013#010+
  '                  AddDependencyUnitPaths(L',',P);'#013#010+
  '                end;'#013#010+
  '            end;'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TBuildEngine.AddPathPrefix(APackage: TPackage; APath: string)'+
  ': string;'#013#010+
  'begin'#013#010+
  '  if IsRelativePath(APath) and (GPathPrefix<>'#039#039') then'#013#010+
  '    result := Inclu','deTrailingPathDelimiter(GPathPrefix) + APath'#013#010+
  '  else'#013#010+
  '    result := APath;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TBuildEngine.GetCompilerCommand(APackage : TPackage; ATarget '+
  ': TTarget; Env: TStrings) : String;'#013#010+
  'Var'#013#010+
  '  L : TUnsortedDuplicatesStringList;'#013#010+
  '  Ar','gs : TStringList;'#013#010+
  '  s : string;'#013#010+
  '  ErrS: string;'#013#010+
  '  i : Integer;'#013#010+
  'begin'#013#010+
  '  if ATarget.TargetSourceFileName = '#039#039' then'#013#010+
  '    Error(SErrCouldNotCompile,[ATarget.Name, APackage.Name]);'#013#010+
  #013#010+
  '  Args:=TStringList.Create;'#013#010+
  '  Args.Duplicates:=dupIgnor','e;'#013#010+
  #013#010+
  '  Result := '#039#039';'#013#010+
  #013#010+
  '  //compiler configuration'#013#010+
  '  if Defaults.NoFPCCfg then'#013#010+
  '    Args.Add('#039'-n'#039');'#013#010+
  #013#010+
  '  // Target OS'#013#010+
  '  Args.Add('#039'-T'#039'+OSToString(Defaults.OS));'#013#010+
  #013#010+
  '  // Target CPU.'#013#010+
  '  // This setting is only applicable when '#039'fpc'#039' is used ','as c'+
  'ompiler-executable.'#013#010+
  '  if ExtractFileName(GetCompiler) = '#039'fpc'#039' then'#013#010+
  '    Args.Add('#039'-P'#039'+CPUToString(Defaults.CPU));'#013#010+
  #013#010+
  '  // Compile mode'#013#010+
  '  If ATarget.Mode<>cmFPC then'#013#010+
  '    Args.Add('#039'-M'#039'+ModeToString(ATarget.Mode))'#013#010+
  '  else If Defaults.Mo','de<>cmFPC then'#013#010+
  '    Args.Add('#039'-M'#039'+ModeToString(Defaults.Mode));'#013#010+
  '  // Output file paths'#013#010+
  '  If ATarget.TargetType in ProgramTargets then'#013#010+
  '    Args.Add('#039'-FE'#039'+AddPathPrefix(APackage,APackage.GetBinOutpu'+
  'tDir(Defaults.CPU,Defaults.OS)));'#013#010+
  '  Args','.Add('#039'-FU'#039'+AddPathPrefix(APackage,APackage.GetUnitsOut'+
  'putDir(Defaults.CPU,Defaults.OS)));'#013#010+
  '  // Object Path'#013#010+
  '  L:=TUnsortedDuplicatesStringList.Create;'#013#010+
  '  L.Duplicates:=dupIgnore;'#013#010+
  '  AddConditionalStrings(APackage, L,APackage.ObjectPath,Def','aults.CPU'+
  ',Defaults.OS);'#013#010+
  '  AddConditionalStrings(APackage, L,ATarget.ObjectPath,Defaults.CPU,De'+
  'faults.OS);'#013#010+
  '  for i:=0 to L.Count-1 do'#013#010+
  '    Args.Add('#039'-Fo'#039'+AddPathPrefix(APackage,L[i]));'#013#010+
  '  FreeAndNil(L);'#013#010+
  '  // Unit Dirs'#013#010+
  '  L:=TUnsortedDupli','catesStringList.Create;'#013#010+
  '  L.Duplicates:=dupIgnore;'#013#010+
  '  AddDependencyUnitPaths(L,APackage);'#013#010+
  '  AddDependencyPaths(L,depUnit,ATarget);'#013#010+
  '  AddConditionalStrings(APackage, L,APackage.UnitPath,Defaults.CPU,Def'+
  'aults.OS);'#013#010+
  '  AddConditionalStrings(','APackage, L,ATarget.UnitPath,Defaults.CPU,De'+
  'faults.OS);'#013#010+
  '  for i:=0 to L.Count-1 do'#013#010+
  '    Args.Add('#039'-Fu'#039'+AddPathPrefix(APackage,L[i]));'#013#010+
  '  FreeAndNil(L);'#013#010+
  '  // Include Path'#013#010+
  '  L:=TUnsortedDuplicatesStringList.Create;'#013#010+
  '  L.Duplicates:=dupIgno','re;'#013#010+
  '  AddDependencyPaths(L,depInclude,ATarget);'#013#010+
  '  AddConditionalStrings(APackage, L,APackage.IncludePath,Defaults.CPU,'+
  'Defaults.OS);'#013#010+
  '  AddConditionalStrings(APackage, L,ATarget.IncludePath,Defaults.CPU,D'+
  'efaults.OS);'#013#010+
  '  for i:=0 to L.Count','-1 do'#013#010+
  '    Args.Add('#039'-Fi'#039'+AddPathPrefix(APackage,L[i]));'#013#010+
  '  FreeAndNil(L);'#013#010+
  #013#010+
  '  // libc-linker path'#013#010+
  '  if APackage.NeedLibC then'#013#010+
  '    begin'#013#010+
  '    if FCachedlibcPath='#039#039' then'#013#010+
  '      begin'#013#010+
  '      s:=GetDefaultLibGCCDir(Defaults.CPU, Defaults.OS',',ErrS);'#013#010+
  '      if s='#039#039' then'#013#010+
  '        Log(vlWarning, SWarngcclibpath +'#039' '#039'+ErrS)'#013#010+
  '      else'#013#010+
  '        begin'#013#010+
  '{$ifndef NO_THREADING}'#013#010+
  '        EnterCriticalsection(FGeneralCriticalSection);'#013#010+
  '        try'#013#010+
  '{$endif NO_THREADING}'#013#010+
  '          FCached','libcPath:=s;'#013#010+
  '{$ifndef NO_THREADING}'#013#010+
  '        finally'#013#010+
  '          LeaveCriticalsection(FGeneralCriticalSection);'#013#010+
  '        end;'#013#010+
  '{$endif NO_THREADING}'#013#010+
  '        end;'#013#010+
  '      end;'#013#010+
  #013#010+
  '    Args.Add('#039'-Fl'#039'+FCachedlibcPath);'#013#010+
  '    end;'#013#010+
  #013#010+
  '  // Custom ','Options'#013#010+
  '  If (Defaults.HaveOptions) then'#013#010+
  '    Args.AddStrings(Defaults.Options);'#013#010+
  #013#010+
  '  APackage.ApplyPackageVariantToCompilerOptions(Args);'#013#010+
  #013#010+
  '  If (APackage.HaveOptions) then'#013#010+
  '    Args.AddStrings(APackage.Options);'#013#010+
  '  If (ATarget.HaveOptio','ns) then'#013#010+
  '    Args.AddStrings(ATarget.Options);'#013#010+
  #013#010+
  '  {$ifdef HAS_UNIT_PROCESS}'#013#010+
  '  // Force the compiler-output to be easy parseable'#013#010+
  '  if not Verbose then'#013#010+
  '    args.Add('#039'-viq'#039');'#013#010+
  '  {$endif}'#013#010+
  #013#010+
  '  // Convert to string'#013#010+
  '  Result:='#039#039';'#013#010+
  '  for i:','=0 to Args.Count-1 do'#013#010+
  '    Result:=Result+'#039' '#039'+maybequoted(Args[i]);'#013#010+
  '  Delete(result,1,1);'#013#010+
  #013#010+
  '  if Defaults.UseEnvironment and assigned(Env) then'#013#010+
  '    begin'#013#010+
  '      env.Values['#039'FPCEXTCMD'#039'] := Result;'#013#010+
  '      result := '#039'!FPCEXTCMD'#039';'#013#010+
  '      // ','Make sure that this process'#039' environment variables are '+
  'passed to the'#013#010+
  '      // compiler'#039's environment'#013#010+
  '      for i := 0 to GetEnvironmentVariableCount-1 do'#013#010+
  '        env.Add(GetEnvironmentString(i));'#013#010+
  '    end;'#013#010+
  #013#010+
  '  // Add Filename to compile',#013#010+
  '  result := result + '#039' '#039' + AddPathPrefix(APackage,ATarget.Targ'+
  'etSourceFileName);'#013#010+
  #013#010+
  '  Args.Free;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TBuildEngine.GetCompiler : String;'#013#010+
  'Var'#013#010+
  '  S : String;'#013#010+
  'begin'#013#010+
  '  // Cache in FCompiler for speed.'#013#010+
  '  If (FCompiler='#039#039') th','en'#013#010+
  '    begin'#013#010+
  '    FCompiler:=Defaults.Compiler;'#013#010+
  '    If (ExtractFilePath(FCompiler)='#039#039') then'#013#010+
  '      begin'#013#010+
  '      S:=ExeSearch(FCompiler,GetEnvironmentVariable('#039'PATH'#039'));'#013+
  #010+
  '      If (S<>'#039#039') then'#013#010+
  '         FCompiler:=S;'#013#010+
  '      end;'#013#010+
  '    end;'#013#010,
  '  Result:=FCompiler;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.CreateOutputDir(APackage: TPackage);'#013#010+
  'Var'#013#010+
  '  D : String;'#013#010+
  '  i: integer;'#013#010+
  'begin'#013#010+
  '  //create a units directory'#013#010+
  '  D:=AddPathPrefix(APackage,APackage.GetUnitsOutputDir(Defaults.CPU,De'+
  'fault','s.OS));'#013#010+
  '  If not SysDirectoryExists(D) then'#013#010+
  '    begin'#013#010+
  '      Log(vlInfo,SInfoCreatingOutputDir,[D]);'#013#010+
  '      CmdCreateDir(D);'#013#010+
  '    end;'#013#010+
  #013#010+
  '  //also create a bin directory for programtargets'#013#010+
  '  For i := 0 to Pred(APackage.Targets.Count) do'#013,#010+
  '    begin'#013#010+
  '      if APackage.Targets.TargetItems[i].TargetType in (ProgramTargets'+
  '-[ttExampleProgram]) then'#013#010+
  '        begin'#013#010+
  '          D:=AddPathPrefix(APackage,APackage.GetBinOutputDir(Defaults.'+
  'CPU,Defaults.OS));'#013#010+
  '          If not SysDirect','oryExists(D) then'#013#010+
  '            begin'#013#010+
  '              Log(vlInfo,SInfoCreatingOutputDir,[D]);'#013#010+
  '              CmdCreateDir(D);'#013#010+
  '            end;'#013#010+
  '          //do not continue loop, directory is made anyway'#013#010+
  '          break;'#013#010+
  '        end;'#013#010+
  '    e','nd;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TBuildEngine.DependencyOK(ADependency : TDependency) : Boolea'+
  'n;'#013#010+
  'begin'#013#010+
  '  Result:=(Defaults.CPU in ADependency.CPUs) and (Defaults.OS in ADepe'+
  'ndency.OSes);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TBuildEngine.TargetOK(ATarget: TTarget; AC','PU: TCPU; AOS: TO'+
  'S): Boolean;'#013#010+
  'begin'#013#010+
  '  if Defaults.SkipCrossPrograms and'#013#010+
  '     (ATarget.TargetType in ProgramTargets) and'#013#010+
  '     IsDifferentFromBuild(ACPU, AOS) then'#013#010+
  '    result := False'#013#010+
  '  else'#013#010+
  '    Result:=(ACPU in ATarget.CPUs) and (AOS ','in ATarget.OSes);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TBuildEngine.TargetInstallOK(ATarget: TTarget; ACPU: TCPU; AO'+
  'S: TOS): Boolean;'#013#010+
  'begin'#013#010+
  '  result := TargetOK(ATarget, ACPU, AOS) and ATarget.Install;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TBuildEngine.PackageOK(APackage : T','Package) : Boolean;'#013#010+
  'begin'#013#010+
  '  Result:=(Defaults.CPU in APackage.CPUs) and (Defaults.OS in APackage'+
  '.OSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.DoBeforeCompile(APackage: TPackage);'#013#010+
  'begin'#013#010+
  '  ExecuteCommands(APackage.Commands,caBeforeCompile);'#013#010,
  '  If Assigned(APackage.BeforeCompile) then'#013#010+
  '    APackage.BeforeCompile(APackage);'#013#010+
  '  If Assigned(APackage.BeforeCompileProc) then'#013#010+
  '    APackage.BeforeCompileProc(APackage);'#013#010+
  '  // It could be that files that weren'#039't found before are availabl',
  'e now.'#013#010+
  '  ResolveFileNames(APackage,Defaults.CPU,Defaults.OS,false,true);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.DoAfterCompile(APackage: TPackage);'#013#010+
  'begin'#013#010+
  '  If Assigned(APackage.AfterCompile) then'#013#010+
  '    APackage.AfterCompile(APackage);'#013#010+
  '  If As','signed(APackage.AfterCompileProc) then'#013#010+
  '    APackage.AfterCompileProc(APackage);'#013#010+
  '  ExecuteCommands(APackage.Commands,caAfterCompile);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TBuildEngine.NeedsCompile(APackage:TPackage;ATarget: TTarget)'+
  ': Boolean;'#013#010+
  'Var'#013#010+
  '  I : I','nteger;'#013#010+
  '  D : TDependency;'#013#010+
  '  T : TTarget;'#013#010+
  '  OD,OFN,TFN : String;'#013#010+
  '  CompileReason: String;'#013#010+
  'begin'#013#010+
  '  Result:=False;'#013#010+
  #013#010+
  '  // Forced recompile?'#013#010+
  '  if FForceCompile then'#013#010+
  '    begin'#013#010+
  '    Result:=true;'#013#010+
  '    CompileReason:=SDbgForcedCompile;'#013#010,
  '    end;'#013#010+
  #013#010+
  '  // For now examples are not compiled at all'#013#010+
  '  if ATarget.TargetType in [ttExampleUnit, ttExampleProgram] then'#013#010+
  '    Exit;'#013#010+
  #013#010+
  '  // Files which should not be compiled on this target can not trigger'+
  ' a compile.'#013#010+
  '  if not TargetOK(','ATarget, Defaults.CPU, Defaults.OS) then'#013#010+
  '    Exit;'#013#010+
  #013#010+
  '  // Check output file'#013#010+
  '  if not result then'#013#010+
  '    begin'#013#010+
  '      if ATarget.TargetType in ProgramTargets then'#013#010+
  '        OD:=APackage.GetBinOutputDir(Defaults.CPU,Defaults.OS)'#013#010+
  '      else'#013#010,
  '        OD:=APackage.GetUnitsOutputDir(Defaults.CPU,Defaults.OS);'#013#010+
  '      If (OD<>'#039#039') then'#013#010+
  '        OD:=IncludeTrailingPathDelimiter(OD);'#013#010+
  '      OFN:=AddPathPrefix(APackage, OD+ATarget.GetOutPutFileName(Defaul'+
  'ts.OS));'#013#010+
  '      Result:=Not File','Exists(OFN);'#013#010+
  '      if Result then'#013#010+
  '        begin'#013#010+
  '        CompileReason:=SDbgOutputDoesNotExist;'#013#010+
  '        Log(vlDebug,SDbgOutputNotYetAvailable,[OFN]);'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  #013#010+
  '  // Check main source'#013#010+
  '  If not Result then'#013#010+
  '    begin'#013#010+
  '   ','   TFN := AddPathPrefix(APackage,ATarget.TargetSourceFileName);'#013+
  #010+
  '      if FileExists(TFN) then'#013#010+
  '        Result:=FileNewer(TFN,OFN);'#013#010+
  '      if Result then'#013#010+
  '        CompileReason:=SDbgNewerSource;'#013#010+
  '    end;'#013#010+
  #013#010+
  '  // Check unit and include depe','ndencies'#013#010+
  '  If not Result then'#013#010+
  '    begin'#013#010+
  '      ResolveDependencies(ATarget.Dependencies,ATarget.Collection as T'+
  'Targets);'#013#010+
  '      I:=0;'#013#010+
  '      for i:=0 to ATarget.Dependencies.Count-1 do'#013#010+
  '        begin'#013#010+
  '          D:=ATarget.Dependencies[i];',#013#010+
  '          if (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'+
  #013#010+
  '            begin'#013#010+
  '              case D.DependencyType of'#013#010+
  '                depUnit :'#013#010+
  '                  begin'#013#010+
  '                    T:=TTarget(D.Target);'#013#010+
  '            ','        If (T=Nil) then'#013#010+
  '                      Error(SErrDepUnknownTarget,[D.Value, ATarget.Nam'+
  'e, APackage.Name]);'#013#010+
  '                    // If a dependent package is compiled we always ne'+
  'ed to recompile'#013#010+
  '                    Log(vldebug, SDbg','DependencyOnUnit, [ATarget.Name'+
  ',T.Name]);'#013#010+
  '                    Result:=(T.State=tsCompiled);'#013#010+
  '                    if Result then'#013#010+
  '                      begin'#013#010+
  '                      Log(vldebug, SDbgDependencyUnitRecompiled, [T.Na'+
  'me]);'#013#010+
  '    ','                  CompileReason:=Format(SDbgDependencyRecompiled'+
  ',[T.Name]);'#013#010+
  '                      end;'#013#010+
  '                  end;'#013#010+
  '                depInclude :'#013#010+
  '                  begin'#013#010+
  '                    if D.TargetFileName<>'#039#039' then'#013#010+
  '      ','                begin'#013#010+
  '                        TFN:=AddPathPrefix(APackage,D.TargetFileName);'+
  #013#010+
  '                        Result:=FileNewer(TFN,OFN);'#013#010+
  '                        if result then'#013#010+
  '                          CompileReason:=Format(SDbg','NewerInclude,[D.'+
  'TargetFileName]);'#013#010+
  '                      end;'#013#010+
  '                  end;'#013#010+
  '                depPackage :'#013#010+
  '                  begin'#013#010+
  '                    log(vlWarning,SWarnTargetDependsOnPackage,[ATarget'+
  '.Name, APackage.Name, d.Valu','e]);'#013#010+
  '                  end;'#013#010+
  '              end;'#013#010+
  '              if result then'#013#010+
  '                break;'#013#010+
  '            end;'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  #013#010+
  '  if result then'#013#010+
  '    Log(vlDebug,SDbgMustCompile,[ATarget.Name, CompileReason]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013,#010+
  'procedure TBuildEngine.Compile(APackage: TPackage; ATarget: TTarget);'#013+
  #010+
  'Var'#013#010+
  '  S : String;'#013#010+
  '  Env : TStrings;'#013#010+
  'begin'#013#010+
  '  Log(vlInfo,SInfoCompilingTarget,[ATarget.Name]);'#013#010+
  '  LogIndent;'#013#010+
  '  ExecuteCommands(ATarget.Commands,caBeforeCompile);'#013#010+
  '  ','If Assigned(ATarget.BeforeCompile) then'#013#010+
  '    ATarget.BeforeCompile(ATarget);'#013#010+
  #013#010+
  '  if (APackage.BuildMode=bmBuildUnit) and not (ATarget.TargetType in ['+
  'ttProgram,ttExampleProgram]) then'#013#010+
  '    begin'#013#010+
  '      APackage.FBUTarget.Dependencies.AddUni','t(ATarget.Name).FTargetF'+
  'ileName:=ATarget.TargetSourceFileName;'#013#010+
  '    end'#013#010+
  '  else'#013#010+
  '    begin'#013#010+
  '      if Defaults.UseEnvironment then'#013#010+
  '        begin'#013#010+
  '          Env := TStringList.Create;'#013#010+
  '          try'#013#010+
  '            S:=GetCompilerCommand(APackag','e,ATarget,Env);'#013#010+
  '            ExecuteCommand(GetCompiler,S,Env);'#013#010+
  '          finally'#013#010+
  '            Env.Free;'#013#010+
  '          end;'#013#010+
  '        end'#013#010+
  '      else'#013#010+
  '        begin'#013#010+
  '          S:=GetCompilerCommand(APackage,ATarget,Env);'#013#010+
  '          ExecuteComm','and(GetCompiler,S,nil);'#013#010+
  '        end;'#013#010+
  '      If Assigned(ATarget.AfterCompile) then'#013#010+
  '        ATarget.AfterCompile(ATarget);'#013#010+
  '      ExecuteCommands(ATarget.Commands,caAfterCompile);'#013#010+
  '    end;'#013#010+
  '  LogUnIndent;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine','.CompileDependencies(APackage:TPackage; ATarge'+
  't: TTarget);'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  '  T : TTarget;'#013#010+
  '  D : TDependency;'#013#010+
  'begin'#013#010+
  '  Log(vlDebug, Format(SDbgCompilingDependenciesOfTarget, [ATarget.Name'+
  ']));'#013#010+
  '  LogIndent;'#013#010+
  '  For I:=0 to ATarget.Depe','ndencies.Count-1 do'#013#010+
  '    begin'#013#010+
  '      D:=ATarget.Dependencies[i];'#013#010+
  '      if (D.DependencyType=depPackage) then'#013#010+
  '        log(vlWarning,SWarnTargetDependsOnPackage,[ATarget.Name, APack'+
  'age.Name, d.Value])'#013#010+
  '      else if (D.DependencyType=depUn','it) and'#013#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#013+
  #010+
  '        begin'#013#010+
  '          T:=TTarget(D.Target);'#013#010+
  '          if Assigned(T) and (T<>ATarget) then'#013#010+
  '            begin'#013#010+
  '              if TargetOK(T, Defaults.CPU, Defau','lts.OS) then'#013#010+
  '                begin'#013#010+
  '                  // We don'#039't need to compile implicit units, they'+
  ' are only'#013#010+
  '                  // used for dependency checking'#013#010+
  '                  if (T.TargetType<>ttImplicitUnit) then'#013#010+
  '                 ','   begin'#013#010+
  '                      case T.State of'#013#010+
  '                        tsNeutral :'#013#010+
  '                          MaybeCompile(APackage,T);'#013#010+
  '                        tsConsidering :'#013#010+
  '                          Log(vlWarning,SWarnCircularTargetD','ependenc'+
  'y,[ATarget.Name,T.Name]);'#013#010+
  '                      end;'#013#010+
  '                    end;'#013#010+
  '                end'#013#010+
  '              else'#013#010+
  '                Log(vlWarning, Format(SWarnDepUnitNotFound, [T.Name, M'+
  'akeTargetString(Defaults.CPU,Defaults.OS',')]));'#013#010+
  '            end'#013#010+
  '          else'#013#010+
  '            Error(SErrDepUnknownTarget,[D.Value, ATarget.Name, APackag'+
  'e.Name]);'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  '  LogUnIndent;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.MaybeCompile(APackage: TPackage; ATarget: TTar',
  'get);'#013#010+
  'begin'#013#010+
  '  if ATarget.State<>tsNeutral then'#013#010+
  '    Error(SErrInvalidState,[ATarget.Name]);'#013#010+
  '  Log(vlDebug, Format(SDbgConsideringTarget, [ATarget.Name]));'#013#010+
  '  LogIndent;'#013#010+
  '  ATarget.FTargetState:=tsConsidering;'#013#010+
  '  ResolveDependencies(ATarg','et.Dependencies,ATarget.Collection as TTa'+
  'rgets);'#013#010+
  '  CompileDependencies(APackage, ATarget);'#013#010+
  '  if NeedsCompile(APackage, ATarget) then'#013#010+
  '    begin'#013#010+
  '      Compile(APackage,ATarget);'#013#010+
  '      ATarget.FTargetState:=tsCompiled;'#013#010+
  '    end'#013#010+
  '  else'#013#010+
  ' ','   ATarget.FTargetState:=tsNoCompile;'#013#010+
  '  LogUnIndent;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TBuildEngine.NeedsCompile(APackage: TPackage): Boolean;'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  '  P : TPackage;'#013#010+
  '  D : TDependency;'#013#010+
  '  CompileReason: string;'#013#010+
  'begin'#013#010+
  '  Result:=False;'#013#010,
  #013#010+
  '  // Forced recompile?'#013#010+
  '  if FForceCompile then'#013#010+
  '    begin'#013#010+
  '    Result:=true;'#013#010+
  '    CompileReason:=SDbgForcedCompile;'#013#010+
  '    end;'#013#010+
  #013#010+
  '  // Recompile because of Package Dependencies?'#013#010+
  '  if not Result then'#013#010+
  '    begin'#013#010+
  '       I:=0;'#013#010+
  '       For I',':=0 to APackage.Dependencies.Count-1 do'#013#010+
  '         begin'#013#010+
  '           D:=APackage.Dependencies[i];'#013#010+
  '           if (D.DependencyType=depPackage) and'#013#010+
  '              (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) the'+
  'n'#013#010+
  '             begin'#013#010,
  '               P:=TPackage(D.Target);'#013#010+
  '               if Assigned(P) then'#013#010+
  '                 begin'#013#010+
  '                   Result:=(P.State=tsCompiled);'#013#010+
  '                   if Result then'#013#010+
  '                     begin'#013#010+
  '                     break;'#013,#010+
  '                     CompileReason:=Format(SDbgPackageDepRecompiled,[P'+
  '.Name]);'#013#010+
  '                     end;'#013#010+
  '                 end;'#013#010+
  '             end;'#013#010+
  '         end;'#013#010+
  '    end;'#013#010+
  #013#010+
  '  // Recompile a Target of this package?'#013#010+
  '  If Not Result then'#013,#010+
  '    begin'#013#010+
  '      GPathPrefix := APackage.Directory;'#013#010+
  '      try'#013#010+
  '        for i:=0 to APackage.Targets.Count-1 do'#013#010+
  '          begin'#013#010+
  '            Result:=NeedsCompile(APackage,APackage.Targets.TargetItems'+
  '[i]);'#013#010+
  '            if Result then'#013#010+
  '    ','          begin'#013#010+
  '              break;'#013#010+
  '              CompileReason:=Format(SDbgTargetHasToBeCompiled,[APackag'+
  'e.Targets.TargetItems[i].Name]);'#013#010+
  '              end;'#013#010+
  '          end;'#013#010+
  '      Finally'#013#010+
  '        GPathPrefix := '#039#039';'#013#010+
  '      end;'#013#010+
  '    en','d;'#013#010+
  #013#010+
  '  if result then'#013#010+
  '    Log(vlDebug,SDbgMustCompile,[APackage.Name, CompileReason]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TBuildEngine.CheckExternalPackage(Const APackageName : String'+
  '):TPackage;'#013#010+
  'var'#013#010+
  '  S : String;'#013#010+
  '  F : String;'#013#010+
  '  I : Integer;'#013#010+
  'begin'#013,#010+
  '  // Already checked?'#013#010+
  '  I:=ExternalPackages.IndexOfName(APackageName);'#013#010+
  '  if I<>-1 then'#013#010+
  '    begin'#013#010+
  '      result:=ExternalPackages.PackageItems[I];'#013#010+
  '      exit;'#013#010+
  '    end;'#013#010+
  '  // Create new external package'#013#010+
  '  Result:=ExternalPackages.AddPa','ckage(APackageName);'#013#010+
  '  Result.FTargetState:=tsNotFound;'#013#010+
  '  // Load unit config'#013#010+
  '  S:=GetUnitDir(Result);'#013#010+
  '  if S<>'#039#039' then'#013#010+
  '    begin'#013#010+
  '      Log(vldebug, SDbgExternalDependency, [APackageName,S]);'#013#010+
  '      // Load unit config if it exists'#013#010+
  '  ','    F:=IncludeTrailingPathDelimiter(S)+UnitConfigFile;'#013#010+
  '      if FileExists(F) then'#013#010+
  '        begin'#013#010+
  '          Log(vlDebug, Format(SDbgLoading, [F]));'#013#010+
  '          Result.LoadUnitConfigFromFile(F);'#013#010+
  '          result.SetDefaultPackageVariant;'#013#010,
  '          result.UnitDir:=result.UnitDir+Result.GetPackageUnitInstallD'+
  'ir(Defaults.CPU, Defaults.OS);'#013#010+
  '        end;'#013#010+
  '      // Check recursive implicit dependencies'#013#010+
  '      CompileDependencies(Result);'#013#010+
  '    end'#013#010+
  '  else'#013#010+
  '    Error(SErrDependenc','yNotFound,[APackageName]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.CompileDependencies(APackage: TPackage);'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  '  P : TPackage;'#013#010+
  '  D : TDependency;'#013#010+
  'begin'#013#010+
  '  For I:=0 to APackage.Dependencies.Count-1 do'#013#010+
  '    begin'#013#010+
  '      D:=APac','kage.Dependencies[i];'#013#010+
  '      if (D.DependencyType=depPackage) and'#013#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#013+
  #010+
  '        begin'#013#010+
  '          P:=TPackage(D.Target);'#013#010+
  '          If Assigned(P) then'#013#010+
  '            begin'#013#010+
  '      ','        if (Defaults.CPU in P.CPUs) and (Defaults.OS in P.OSes'+
  ') then'#013#010+
  '                begin'#013#010+
  '                  case P.State of'#013#010+
  '                    tsNeutral :'#013#010+
  '                      MaybeCompile(P);'#013#010+
  '                    tsConsidering :'#013#010+
  '  ','                    Log(vlWarning,SWarnCircularPackageDependency,['+
  'APackage.Name,P.Name]);'#013#010+
  '                  end;'#013#010+
  '                end'#013#010+
  '              else'#013#010+
  '                Log(vlWarning,SWarnDependOnOtherPlatformPackage,[APack'+
  'age.Name, D.Va','lue, MakeTargetString(Defaults.CPU, Defaults.OS)]);'#013+
  #010+
  '            end'#013#010+
  '          else'#013#010+
  '            begin'#013#010+
  '              D.Target:=CheckExternalPackage(D.Value);'#013#010+
  '              P:=TPackage(D.Target);'#013#010+
  '            end;'#013#010+
  '          if (D.Require','Checksum<>$ffffffff) and'#013#010+
  '             (P.InstalledChecksum<>$ffffffff) and'#013#010+
  '             (P.InstalledChecksum<>D.RequireChecksum) then'#013#010+
  '            Log(vlDebug,SDbgPackageChecksumChanged,[P.Name]);'#013#010+
  '          APackage.InheritPackageVariant','sFromDependency(P);'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TBuildEngine.CheckDependencies(APackage: TPackage): TCheckDep'+
  'endencyResult;'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  '  P : TPackage;'#013#010+
  '  D : TDependency;'#013#010+
  'begin'#013#010+
  '  result := cdAvailable;'#013#010+
  '  For I:=0',' to APackage.Dependencies.Count-1 do'#013#010+
  '    begin'#013#010+
  '      D:=APackage.Dependencies[i];'#013#010+
  '      if (D.DependencyType=depPackage) and'#013#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#013+
  #010+
  '        begin'#013#010+
  '          P:=TPackage(D.Targ','et);'#013#010+
  '          If Assigned(P) then'#013#010+
  '            begin'#013#010+
  '              if (Defaults.CPU in P.CPUs) and (Defaults.OS in P.OSes) '+
  'then'#013#010+
  '                begin'#013#010+
  '                  case P.State of'#013#010+
  '                    tsNeutral :'#013#010+
  '                 ','     result := cdNotYetAvailable;'#013#010+
  '                    tsConsidering :'#013#010+
  '                      Log(vlWarning,SWarnCircularPackageDependency,[AP'+
  'ackage.Name,P.Name]);'#013#010+
  '                  end;'#013#010+
  '                end'#013#010+
  '              else'#013#010+
  '          ','      Log(vlWarning,SWarnDependOnOtherPlatformPackage,[APa'+
  'ckage.Name, D.Value, MakeTargetString(Defaults.CPU, Defaults.OS)]);'#013+
  #010+
  '            end'#013#010+
  '          else'#013#010+
  '            begin'#013#010+
  '              D.Target:=CheckExternalPackage(D.Value);'#013#010+
  '     ','         P:=TPackage(D.Target);'#013#010+
  '            end;'#013#010+
  '          if (D.RequireChecksum<>$ffffffff) and'#013#010+
  '             (P.InstalledChecksum<>$ffffffff) and'#013#010+
  '             (P.InstalledChecksum<>D.RequireChecksum) then'#013#010+
  '            Log(vlDebug,SDbgP','ackageChecksumChanged,[P.Name]);'#013#010+
  '          APackage.InheritPackageVariantsFromDependency(P);'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.Compile(APackage: TPackage);'#013#010+
  'Var'#013#010+
  '  T : TTarget;'#013#010+
  '  I : Integer;'#013#010+
  '  Cmd: string;'#013#010+
  '  cmd','Opts: string;'#013#010+
  '  sFPDocFormat: string;'#013#010+
  '  IFPDocFormat: TFPDocFormat;'#013#010+
  '  d: integer;'#013#010+
  '  UC: string;'#013#010+
  '  dep: TDependency;'#013#010+
  '  RegenerateUnitconfigFile: boolean;'#013#010+
  '  BUName: string;'#013#010+
  #013#010+
  '  procedure CompileBuildUnit;'#013#010+
  '  var'#013#010+
  '    I: Integer;'#013#010+
  '    ','T: TTarget;'#013#010+
  '    L: TStrings;'#013#010+
  '    F: Text;'#013#010+
  #013#010+
  '  begin'#013#010+
  '    if (APackage.FBUTarget.Dependencies.Count>0) then'#013#010+
  '      begin'#013#010+
  '        Log(vldebug, Format(SDbgGenerateBuildUnit, [APackage.FBUTarget'+
  '.Name]));'#013#010+
  '        system.Assign(F,AddPathPref','ix(APackage,APackage.FBUTarget.FT'+
  'argetSourceFileName));'#013#010+
  '        Rewrite(F);'#013#010+
  '        writeln(F,'#039'unit '#039' + APackage.FBUTarget.Name +'#039';'#039+
  ');'#013#010+
  '        writeln(F,'#039'interface'#039');'#013#010+
  '        writeln(F,'#039'uses'#039');'#013#010+
  '        for i := 0 to APackage.FBUTarget.','Dependencies.Count-1 do'#013+
  #010+
  '          begin'#013#010+
  '            if i<>0 then'#013#010+
  '              write(F,'#039','#039');'#013#010+
  '            writeln(F,APackage.FBUTarget.Dependencies.Dependencies[i].'+
  'Value);'#013#010+
  '          end;'#013#010+
  '        writeln(F,'#039';'#039');'#013#010+
  '        writeln(F,'#039'impl','ementation'#039');'#013#010+
  '        writeln(F,'#039'end.'#039');'#013#010+
  #013#010+
  '        system.close(F);'#013#010+
  #013#010+
  '        APackage.FBuildMode:=bmOneByOne;'#013#010+
  '        try'#013#010+
  '          Compile(APackage,APackage.FBUTarget);'#013#010+
  '        finally'#013#010+
  '          // Delete temporary build-unit files',#013#010+
  '          L := TStringList.Create;'#013#010+
  '          try'#013#010+
  '            APackage.FBUTarget.GetCleanFiles(L,IncludeTrailingPathDeli'+
  'miter(AddPathPrefix(APackage,APackage.GetUnitsOutputDir(Defaults.CPU,D'+
  'efaults.OS))),'#039#039',Defaults.CPU,Defaults.OS);'#013#010+
  '  ','          L.Add(AddPathPrefix(APackage,APackage.FBUTarget.SourceFi'+
  'leName));'#013#010+
  '            CmdDeleteFiles(L);'#013#010+
  '          finally'#013#010+
  '            L.Free;'#013#010+
  '          end;'#013#010+
  '        end;'#013#010+
  '      end;'#013#010+
  #013#010+
  '    For I:=0 to APackage.Targets.Count-1 do'#013#010+
  '  ','    begin'#013#010+
  '        T:=APackage.Targets.TargetItems[i];'#013#010+
  '        if (T.TargetType = ttUnit) and (TargetOK(T, Defaults.CPU, Defa'+
  'ults.OS)) then'#013#010+
  '          begin'#013#010+
  '            If Assigned(T.AfterCompile) then'#013#010+
  '              T.AfterCompile(T);'#013#010+
  ' ','           ExecuteCommands(T.Commands,caAfterCompile);'#013#010+
  '          end'#013#010+
  '      end;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  procedure ProcessCompileTarget;'#013#010+
  '  begin'#013#010+
  '    if TargetOK(T, Defaults.CPU, Defaults.OS) then'#013#010+
  '      begin'#013#010+
  '        if T.State=tsNeutral then'#013#010+
  '   ','       MaybeCompile(APackage,T);'#013#010+
  '        // If a target is compiled, re-generate the UnitConfigFile'#013+
  #010+
  '        if T.FTargetState<>tsNoCompile then'#013#010+
  '          RegenerateUnitconfigFile:= True;'#013#010+
  '      end'#013#010+
  '    else'#013#010+
  '      begin'#013#010+
  '        if not(','Defaults.CPU in T.CPUs) then'#013#010+
  '          Log(vldebug, Format(SDbgSkippingTargetWrongCPU, [T.Name, CPU'+
  'sToString(T.CPUs)]));'#013#010+
  '        if not(Defaults.OS in T.OSes) then'#013#010+
  '          Log(vldebug, Format(SDbgSkippingTargetWrongOS, [T.Name, OSes'+
  'ToS','tring(T.OSes)]));'#013#010+
  '      end;'#013#010+
  '  end;'#013#010+
  #013#010+
  'begin'#013#010+
  '  cmdOpts := '#039#039';'#013#010+
  #013#010+
  '  log(vlWarning,SWarnStartCompilingPackage,[APackage.Name, Defaults.Ta'+
  'rget]);'#013#010+
  #013#010+
  '  case Defaults.BuildMode of'#013#010+
  '    bmOneByOne:  begin'#013#010+
  '                   if (bmOneByOne in ','APackage.SupportBuildModes) the'+
  'n'#013#010+
  '                     APackage.FBuildMode:=bmOneByOne'#013#010+
  '                   else if bmBuildUnit in APackage.SupportBuildModes t'+
  'hen'#013#010+
  '                     begin'#013#010+
  '                       log(vlInfo,SInfoFallbackBu','ildmodeBU);'#013#010+
  '                       APackage.FBuildMode:=bmBuildUnit;'#013#010+
  '                     end'#013#010+
  '                   else'#013#010+
  '                     raise exception.create(SErrUnsupportedBuildmode);'+
  #013#010+
  '                 end;'#013#010+
  '    bmBuildUnit: begin'#013,#010+
  '                   // When bmBuildUnit is supported by the package use'+
  ' a buildunit.'#013#010+
  '                   // Unless there is only one target and bmOneByOne i'+
  's also supported'#013#010+
  '                   if (bmBuildUnit in APackage.SupportBuildModes) ','an'+
  'd'#013#010+
  '                      not ((APackage.Targets.Count=1) and (bmOneByOne '+
  'in APackage.SupportBuildModes)) then'#013#010+
  '                     APackage.FBuildMode:=bmBuildUnit'#013#010+
  '                   else if bmOneByOne in APackage.SupportBuildModes th'+
  'en',#013#010+
  '                     begin'#013#010+
  '                       log(vlInfo,SInfoFallbackBuildmode);'#013#010+
  '                       APackage.FBuildMode:=bmOneByOne'#013#010+
  '                     end'#013#010+
  '                   else'#013#010+
  '                     raise exception.create','(SErrUnsupportedBuildmode'+
  ');'#013#010+
  '                 end;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  GPathPrefix:=APackage.Directory;'#013#010+
  '  Try'#013#010+
  '    CreateOutputDir(APackage);'#013#010+
  '    AddPackageMacrosToDictionary(APackage, APackage.Dictionary);'#013#010+
  '    DoBeforeCompile(APackage);'#013#010+
  '    R','egenerateUnitconfigFile:=False;'#013#010+
  '    if APackage.BuildMode=bmBuildUnit then'#013#010+
  '      begin'#013#010+
  '        APackage.FBUTargets := TTargets.Create(TTarget);'#013#010+
  '        if Defaults.OS in AllLimit83fsOses then'#013#010+
  '          BUName := '#039'BUnit.pp'#039#013#010+
  '        els','e'#013#010+
  '          BUName := '#039'BuildUnit_'#039'+StringReplace(APackage.Name,'#039+
  '-'#039','#039'_'#039',[rfReplaceAll])+'#039'.pp'#039';'#013#010+
  '        APackage.FBUTarget := APackage.FBUTargets.AddUnit(BUName);'#013+
  #010+
  '        APackage.FBUTarget.FTargetSourceFileName := APackage.FBUTarget'+
  '.Sourc','eFileName;'#013#010+
  '      end;'#013#010+
  '    For I:=0 to APackage.Targets.Count-1 do'#013#010+
  '      begin'#013#010+
  '        T:=APackage.Targets.TargetItems[i];'#013#010+
  '        case T.TargetType of'#013#010+
  '        ttUnit:'#013#010+
  '          begin'#013#010+
  '            ProcessCompileTarget;'#013#010+
  '          end;',#013#010+
  '        ttProgram:'#013#010+
  '          begin // do nothing, are compiled later'#013#010+
  '          end;'#013#010+
  '        ttFPDoc:'#013#010+
  '          begin'#013#010+
  '            for d := 0 to T.Dependencies.Count - 1 do'#013#010+
  '            begin'#013#010+
  '              dep := TDependency(T.Depende','ncies[d]);'#013#010+
  #013#010+
  '              //add unit dependencies'#013#010+
  '              if dep.DependencyType = depUnit then'#013#010+
  '                cmdOpts := cmdOpts + '#039' --input='#039' + AddPathPref'+
  'ix(APackage,dep.Value);'#013#010+
  '            end;'#013#010+
  #013#010+
  '            //check if a doc','umentation target is given'#013#010+
  '            cmdOpts := cmdOpts + '#039' --input='#039' + AddPathPrefix(A'+
  'Package,T.Directory + T.Name + T.Extension) + '#039' --descr='#039'+ T.X'+
  'ML;'#013#010+
  '          end'#013#010+
  '        else'#013#010+
  '          log(vldebug, SDbgTargetIsNotAUnitOrProgram,[','T.Name]);'#013#010+
  '        end;'#013#010+
  '      end;'#013#010+
  #013#010+
  '    if APackage.BuildMode=bmBuildUnit then'#013#010+
  '      CompileBuildUnit;'#013#010+
  #013#010+
  '    FreeAndNil(APackage.FBUTarget);'#013#010+
  #013#010+
  '    For I:=0 to APackage.Targets.Count-1 do'#013#010+
  '      begin'#013#010+
  '        T:=APackage.Targets.Targe','tItems[i];'#013#010+
  '        if T.TargetType=ttProgram then'#013#010+
  '          begin'#013#010+
  '            ProcessCompileTarget;'#013#010+
  '          end;'#013#010+
  '      end;'#013#010+
  #013#010+
  '    if RegenerateUnitconfigFile then'#013#010+
  '      begin'#013#010+
  '        UC:=IncludeTrailingPathDelimiter(AddPathPrefix(','APackage,APac'+
  'kage.GetUnitConfigOutputDir(Defaults.CPU,Defaults.OS)))+UnitConfigFile'+
  ';'#013#010+
  '        Log(vlInfo, Format(SDbgGenerating, [UC]));'#013#010+
  '        APackage.SaveUnitConfigToFile(UC,Defaults.CPU,Defaults.OS);'#013+
  #010+
  '      end;'#013#010+
  #013#010+
  '    //compile docum','entation, because options were found'#013#010+
  '    if cmdOpts <> '#039#039' then'#013#010+
  '    begin'#013#010+
  '      //append package name'#013#010+
  '      cmdOpts := cmdOpts + '#039' --package='#039' + APackage.Name;'#013#010+
  #013#010+
  '      for IFPDocFormat:=Low(TFPDocFormat) to High(TFPDocFormat) do'#013+
  #010+
  '      ','begin'#013#010+
  '        if IFPDocFormat in APackage.FPDocFormat then'#013#010+
  '        begin'#013#010+
  '          //prepend output format'#013#010+
  '          case IFPDocFormat of'#013#010+
  '            ffHtml:      sFPDocFormat := '#039'--format=html --output='#039+
  ' + AddPathPrefix(APackage,Defau','lts.FPDocOutputDir);'#013#010+
  '            ffHtm:       sFPDocFormat := '#039'--format=htm --output='#039+
  ' + AddPathPrefix(APackage,Defaults.FPDocOutputDir);'#013#010+
  '            ffXHtml:     sFPDocFormat := '#039'--format=xhtml --output='+
  #039' + AddPathPrefix(APackage,Default','s.FPDocOutputDir);'#013#010+
  '            ffLaTex:     sFPDocFormat := '#039'--format=latex --output='+
  #039' + AddPathPrefix(APackage,Defaults.FPDocOutputDir) + APackage.Name '+
  '+ '#039'.tex'#039';'#013#010+
  '            ffXMLStruct: sFPDocFormat := '#039'--format=xml-struct --ou'+
  'tput='#039' + ','AddPathPrefix(APackage,Defaults.FPDocOutputDir);'#013#010+
  '            ffChm:       sFPDocFormat := '#039'--format=chm --output='#039+
  ' + AddPathPrefix(APackage,Defaults.FPDocOutputDir) + APackage.Name + '#039+
  '.chm'#039';'#013#010+
  '          end;'#013#010+
  #013#010+
  '          //execute fpdoc'#013#010+
  '  ','        Cmd:=ExeSearch('#039'fpdoc'#039',GetEnvironmentvariable('#039+
  'PATH'#039'));'#013#010+
  '          if Cmd = '#039#039' then Cmd := '#039'fpdoc'#039';'#013#010+
  '          ExecuteProcess(Cmd, sFPDocFormat + cmdOpts);'#013#010+
  '        end;'#013#010+
  '      end;'#013#010+
  '    end;'#013#010+
  #013#010+
  '    DoAfterCompile(APackage);'#013#010+
  '  Fina','lly'#013#010+
  '    GPathPrefix:='#039#039';'#013#010+
  '  end;'#013#010+
  '  inc(FProgressCount);'#013#010+
  '  if FProgressMax>0 then'#013#010+
  '    log(vlWarning,SWarnCompilingPackagecompleteProgress,[(FProgressCou'+
  'nt)/FProgressMax * 100, APackage.Name])'#013#010+
  '  else'#013#010+
  '    log(vlWarning,SWarnCompilingPack','agecomplete,[APackage.Name]);'#013+
  #010+
  'end;'#013#010+
  #013#010+
  'procedure TBuildEngine.MaybeCompile(APackage: TPackage);'#013#010+
  'begin'#013#010+
  '  if ReadyToCompile(APackage) then'#013#010+
  '    begin'#013#010+
  '      Compile(APackage);'#013#010+
  '      APackage.FTargetState:=tsCompiled;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  'fun','ction TBuildEngine.ReadyToCompile(APackage: TPackage) : Boolean;'#013+
  #010+
  'begin'#013#010+
  '  result := False;'#013#010+
  '  if APackage.State in [tsCompiled, tsNoCompile] then'#013#010+
  '    begin'#013#010+
  '      Log(vlInfo,SInfoPackageAlreadyProcessed,[APackage.Name]);'#013#010+
  '      Exit;'#013#010+
  '   ',' end;'#013#010+
  '  if APackage.State<>tsNeutral then'#013#010+
  '    Error(SErrInvalidState,[APackage.Name]);'#013#010+
  '  Log(vlDebug,SDbgConsideringPackage,[APackage.Name]);'#013#010+
  '  LogIndent;'#013#010+
  '  if Defaults.ThreadsAmount=-1 then'#013#010+
  '    APackage.FTargetState:=tsConsidering;'#013#010,
  '  ResolveDependencies(APackage.Dependencies,(APackage.Collection as TP'+
  'ackages));'#013#010+
  '  // When multiple threads are used, delay the compilation of the pack'+
  'age when'#013#010+
  '  // there are unsolved dependencies. When no threads are used, compil'+
  'e all'#013#010+
  ' ',' // dependencies.'#013#010+
  '  if Defaults.ThreadsAmount=-1 then'#013#010+
  '    CompileDependencies(APackage)'#013#010+
  '  else if CheckDependencies(APackage)=cdNotYetAvailable then'#013#010+
  '    begin'#013#010+
  '      log(vlInfo,'#039'Delaying package '#039'+apackage.name);'#013#010+
  '      result := False;',#013#010+
  '      Exit;'#013#010+
  '    end;'#013#010+
  '  APackage.SetDefaultPackageVariant;'#013#010+
  #013#010+
  '  ResolveFileNames(APackage,Defaults.CPU,Defaults.OS,True,False);'#013#010+
  '  If NeedsCompile(APackage) then'#013#010+
  '    result := True'#013#010+
  '  else'#013#010+
  '    begin'#013#010+
  '      APackage.FTargetState:=tsNoCo','mpile;'#013#010+
  '      inc(FProgressCount);'#013#010+
  '    end;'#013#010+
  '  LogUnIndent;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TBuildEngine.InstallPackageFiles(APAckage : TPackage; tt : TT'+
  'argetTypes; Const Dest : String):Boolean;'#013#010+
  'Var'#013#010+
  '  List : TStringList;'#013#010+
  'begin'#013#010+
  '  Result:=False;'#013#010+
  '  ','List:=TStringList.Create;'#013#010+
  '  Try'#013#010+
  '    APackage.GetInstallFiles(List,tt,Defaults.CPU, Defaults.OS);'#013#010+
  '    if (List.Count>0) then'#013#010+
  '      begin'#013#010+
  '        Result:=True;'#013#010+
  '        CmdCopyFiles(List,Dest);'#013#010+
  '      end;'#013#010+
  '  Finally'#013#010+
  '    List.Free;'#013#010+
  '  e','nd;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TBuildEngine.InstallUnitConfigFile(APAckage: TPackage; const'+
  ' Dest: String);'#013#010+
  'Var'#013#010+
  '  List : TStringList;'#013#010+
  'begin'#013#010+
  '  List:=TStringList.Create;'#013#010+
  '  Try'#013#010+
  '    List.add(IncludeTrailingPathDelimiter(APackage.GetUnitConfigOutpu',
  'tDir(Defaults.CPU,Defaults.OS))+UnitConfigFile);'#013#010+
  '    CmdCopyFiles(List,Dest);'#013#010+
  '  Finally'#013#010+
  '    List.Free;'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TBuildEngine.InstallPackageSourceFiles(APAckage : TPackage; s'+
  'tt : TSourceTypes; ttt : TTargetTypes; Const De','st : String): Boolean'+
  ';'#013#010+
  'Var'#013#010+
  '  List : TStringList;'#013#010+
  'begin'#013#010+
  '  Result:=False;'#013#010+
  '  List:=TStringList.Create;'#013#010+
  '  Try'#013#010+
  '    APackage.GetInstallSourceFiles(List,stt,ttt);'#013#010+
  '    if (List.Count>0) then'#013#010+
  '      begin'#013#010+
  '        Result:=True;'#013#010+
  '        CmdC','opyFiles(List,Dest);'#013#010+
  '      end;'#013#010+
  '  Finally'#013#010+
  '    List.Free;'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.DoBeforeInstall(APackage: TPackage);'#013#010+
  'begin'#013#010+
  '  ExecuteCommands(APackage.Commands,caBeforeInstall);'#013#010+
  '  If Assigned(APackage.BeforeInstall) ','then'#013#010+
  '    APackage.BeforeInstall(APackage);'#013#010+
  '  If Assigned(APackage.BeforeInstallProc) then'#013#010+
  '    APackage.BeforeInstallProc(APackage);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.DoAfterInstall(APackage: TPackage);'#013#010+
  'begin'#013#010+
  '  If Assigned(APackage.Afte','rInstall) then'#013#010+
  '    APackage.AfterInstall(APackage);'#013#010+
  '  If Assigned(APackage.AfterInstallProc) then'#013#010+
  '    APackage.AfterInstallProc(APackage);'#013#010+
  '  ExecuteCommands(APackage.Commands,caAfterInstall);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.Install(AP','ackage: TPackage);'#013#010+
  'Var'#013#010+
  '  UC,D : String;'#013#010+
  '  B : Boolean;'#013#010+
  'begin'#013#010+
  '  If (Apackage.State<>tsCompiled) then'#013#010+
  '    MaybeCompile(APackage);'#013#010+
  '  try'#013#010+
  '    Log(vlCommand,SInfoInstallingPackage,[APackage.Name]);'#013#010+
  '    If (APackage.Directory<>'#039#039') then'#013#010,
  '      EnterDir(APackage.Directory);'#013#010+
  '    DoBeforeInstall(APackage);'#013#010+
  '    // units'#013#010+
  '    B:=false;'#013#010+
  '    GlobalDictionary.AddVariable('#039'PackageName'#039',APackage.Name);'+
  #013#010+
  '    GlobalDictionary.AddVariable('#039'unitinstalldir'#039',Defaults.Uni'+
  'tInstallDir);'#013#010+
  ' ','   GlobalDictionary.AddVariable('#039'packageunitinstalldir'#039',APa'+
  'ckage.GetPackageUnitInstallDir(Defaults.CPU,Defaults.OS));'#013#010+
  #013#010+
  '    D:=IncludeTrailingPathDelimiter(Defaults.BaseInstallDir);'#013#010+
  '    // This is to install the TPackage.Installfiles, whi','ch are not r'+
  'elated to any'#013#010+
  '    // target'#013#010+
  '    if InstallPackageFiles(APackage,[],D) then'#013#010+
  '      B:=true;'#013#010+
  '    D:=IncludeTrailingPathDelimiter(Defaults.UnitInstallDir)+APackage.'+
  'GetPackageUnitInstallDir(Defaults.CPU,Defaults.OS);'#013#010+
  '    if Inst','allPackageFiles(APackage,[ttUnit, ttImplicitUnit],D) then'+
  #013#010+
  '      B:=true;'#013#010+
  '    // By default do not install the examples. Maybe add an option for'+
  ' this later'#013#010+
  '    //if InstallPackageFiles(APAckage,ttExampleUnit,D) then'#013#010+
  '    //  B:=true;'#013#010+
  '   ',' // Unit (dependency) configuration if there were units installed'+
  #013#010+
  '    D:=IncludeTrailingPathDelimiter(Defaults.UnitInstallDir);'#013#010+
  '    if B then'#013#010+
  '      InstallUnitConfigFile(APackage,D);'#013#010+
  '    // Programs'#013#010+
  '    D:=IncludeTrailingPathDelimiter(','Defaults.BinInstallDir);'#013#010+
  '    InstallPackageFiles(APAckage,[ttProgram],D);'#013#010+
  '    //InstallPackageFiles(APAckage,ttExampleProgram,D);'#013#010+
  '    // Documentation'#013#010+
  '    D:=IncludeTrailingPathDelimiter(Defaults.DocInstallDir)+'#039'fpc-'#039+
  '+APackage.FileName+','PathDelim;'#013#010+
  '    InstallPackageSourceFiles(APackage,[stDoc],[],D);'#013#010+
  '    // Examples'#013#010+
  '    if Defaults.InstallExamples then'#013#010+
  '      begin'#013#010+
  '        D:=IncludeTrailingPathDelimiter(Defaults.ExamplesInstallDir)+'#039+
  'fpc-'#039'+APackage.FileName+PathDelim+'#039,'examples'#039'+PathDelim;'#013+
  #010+
  '        InstallPackageSourceFiles(APackage,[stExample],[ttExampleProgr'+
  'am,ttExampleUnit],D);'#013#010+
  '      end;'#013#010+
  '    // Done.'#013#010+
  '    APackage.FTargetState:=tsInstalled;'#013#010+
  '    DoAfterInstall(APackage);'#013#010+
  '  Finally'#013#010+
  '    If (APackage','.Directory<>'#039#039') then'#013#010+
  '      EnterDir('#039#039');'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.DoBeforeArchive(APackage: TPackage);'#013#010+
  'begin'#013#010+
  '  ExecuteCommands(APackage.Commands,caBeforeArchive);'#013#010+
  '  If Assigned(APackage.BeforeArchive) then'#013#010+
  '    APackage','.BeforeArchive(APackage);'#013#010+
  '  If Assigned(APackage.BeforeArchiveProc) then'#013#010+
  '    APackage.BeforeArchiveProc(APackage);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.DoAfterArchive(APackage: TPackage);'#013#010+
  'begin'#013#010+
  '  If Assigned(APackage.AfterArchive) then'#013#010+
  '  ','  APackage.AfterArchive(APackage);'#013#010+
  '  If Assigned(APackage.AfterArchiveProc) then'#013#010+
  '    APackage.AfterArchiveProc(APackage);'#013#010+
  '  ExecuteCommands(APackage.Commands,caAfterArchive);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.Archive(APackage: TPackage);',#013#010+
  'Var'#013#010+
  '  L : TStringList;'#013#010+
  '  A : String;'#013#010+
  '  i: integer;'#013#010+
  '  ICPU : TCPU;'#013#010+
  '  IOS  : TOS;'#013#010+
  'begin'#013#010+
  '  A:=FStartDir+ APackage.FileName + ZipExt;'#013#010+
  '  Log(vlInfo,SInfoArchivingPackage,[APackage.Name,A]);'#013#010+
  '  try'#013#010+
  '    If (APackage.Directory<>'#039#039') then'#013,#010+
  '      EnterDir(APackage.Directory);'#013#010+
  '    DoBeforeArchive(Apackage);'#013#010+
  '    L:=TStringList.Create;'#013#010+
  '    L.Sorted:=true;'#013#010+
  '    L.Duplicates:=dupIgnore;'#013#010+
  '    Try'#013#010+
  '      // Add fpmake.pp & manifest.xml always'#013#010+
  '      L.Add(FPMakePPFile);'#013#010+
  '      L.','Add(ManifestFile);'#013#010+
  '      //get all files from all targets'#013#010+
  '      for ICPU:=Low(TCPU) to high(TCPU) do'#013#010+
  '        for IOS:=Low(TOS) to high(TOS) do'#013#010+
  '          if OSCPUSupported[IOS,ICPU] then'#013#010+
  '            begin'#013#010+
  '              // Make sure tha','t the package is resolved for each tar'+
  'gbet'#013#010+
  '              APackage.FAllFilesResolved:=false;'#013#010+
  '              ResolveFileNames(APackage,ICPU,IOS,false);'#013#010+
  '              APackage.GetArchiveFiles(L, ICPU, IOS);'#013#010+
  '            end;'#013#010+
  '      //from so','urces'#013#010+
  '      APackage.GetArchiveSourceFiles(L);'#013#010+
  #013#010+
  '      //show all files'#013#010+
  '      for i := 0 to L.Count-1 do'#013#010+
  '        Log(vlDebug, Format(SDbgArchivingFile, [L[i]]));'#013#010+
  #013#010+
  '{$ifdef HAS_UNIT_ZIPPER}'#013#010+
  '      if not Assigned(ArchiveFilesProc) then'#013,#010+
  '        begin'#013#010+
  '          FZipFile := TZipper.Create;'#013#010+
  '          FZipFile.ZipFiles(A, L);'#013#010+
  '        end'#013#010+
  '      else'#013#010+
  '{$endif HAS_UNIT_ZIPPER}'#013#010+
  '        CmdArchiveFiles(L,A);'#013#010+
  '    Finally'#013#010+
  '      L.Free;'#013#010+
  #013#010+
  '{$ifdef HAS_UNIT_ZIPPER}'#013#010+
  '      if no','t Assigned(ArchiveFilesProc) then'#013#010+
  '        FreeAndNil(FZipFile);'#013#010+
  '{$endif HAS_UNIT_ZIPPER}'#013#010+
  '    end;'#013#010+
  '    DoAfterArchive(Apackage);'#013#010+
  '  Finally'#013#010+
  '    If (APackage.Directory<>'#039#039') then'#013#010+
  '      EnterDir('#039#039');'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEng','ine.DoBeforeClean(APackage: TPackage);'#013#010+
  'begin'#013#010+
  '  ExecuteCommands(APackage.Commands,caBeforeClean);'#013#010+
  '  If Assigned(APackage.BeforeClean) then'#013#010+
  '    APackage.BeforeClean(APackage);'#013#010+
  '  If Assigned(APackage.BeforeCleanProc) then'#013#010+
  '    APackage.Be','foreCleanProc(APackage);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.DoAfterClean(APackage: TPackage);'#013#010+
  'begin'#013#010+
  '  If Assigned(APackage.AfterClean) then'#013#010+
  '    APackage.AfterClean(APackage);'#013#010+
  '  If Assigned(APackage.AfterInstallProc) then'#013#010+
  '    APackage.Af','terCleanProc(APackage);'#013#010+
  '  ExecuteCommands(APackage.Commands,caAfterClean);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.Clean(APackage: TPackage; AllTargets: boolean);'+
  #013#010+
  'var'#013#010+
  '  ACPU: TCpu;'#013#010+
  '  AOS: TOS;'#013#010+
  '  DirectoryList : TStringList;'#013#010+
  'begin'#013#010+
  '  Log(vlI','nfo,SInfoCleaningPackage,[APackage.Name]);'#013#010+
  '  try'#013#010+
  '    If (APackage.Directory<>'#039#039') then'#013#010+
  '      EnterDir(APackage.Directory);'#013#010+
  '    DoBeforeClean(Apackage);'#013#010+
  '    AddPackageMacrosToDictionary(APackage, APackage.Dictionary);'#013#010+
  '    if AllTargets ','then'#013#010+
  '      begin'#013#010+
  '        // Remove the unit-and bin-directories completely. This is saf'+
  'er in case of files'#013#010+
  '        // being renamed and such. See also bug 19655'#013#010+
  '        DirectoryList := TStringList.Create;'#013#010+
  '        try'#013#010+
  '          for AC','PU:=low(TCpu) to high(TCpu) do if ACPU<>cpuNone then'+
  #013#010+
  '            for AOS:=low(TOS) to high(TOS) do if AOS<>osNone then'#013#010+
  '              begin'#013#010+
  '                if OSCPUSupported[AOS,ACPU] then'#013#010+
  '                  begin'#013#010+
  '                    Dir','ectoryList.Add(ExtractFileDir(APackage.GetUni'+
  'tsOutputDir(ACPU,AOS)));'#013#010+
  '                    DirectoryList.Add(ExtractFileDir(APackage.GetBinOu'+
  'tputDir(ACPU,AOS)));'#013#010+
  '                  end;'#013#010+
  '              end;'#013#010+
  '          CmdRemoveTrees(Director','yList);'#013#010+
  '        finally'#013#010+
  '          DirectoryList.Free;'#013#010+
  '        end;'#013#010+
  '{        for ACPU:=low(TCpu) to high(TCpu) do'#013#010+
  '          for AOS:=low(TOS) to high(TOS) do'#013#010+
  '            begin'#013#010+
  '              if FileExists(APackage.GetUnitsOutputDir(ACP','U,AOS)) or'+
  #013#010+
  '                 FileExists(APackage.GetBinOutputDir(ACPU,AOS)) then'#013+
  #010+
  '                Clean(APackage,ACPU,AOS);'#013#010+
  '            end;}'#013#010+
  '      end'#013#010+
  '    else'#013#010+
  '      Clean(APackage, Defaults.CPU, Defaults.OS);'#013#010+
  '    DoAfterClean(Apackag','e);'#013#010+
  '  Finally'#013#010+
  '    If (APackage.Directory<>'#039#039') then'#013#010+
  '      EnterDir('#039#039');'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TBuildEngine.Clean(APackage: TPackage; ACPU: TCPU; AOS: TOS)'+
  ';'#013#010+
  'Var'#013#010+
  '  List : TStringList;'#013#010+
  '  DirectoryList : TStringList;'#013#010+
  'begin'#013#010+
  '  List:=T','StringList.Create;'#013#010+
  '  try'#013#010+
  '    List.Add(APackage.GetUnitsOutputDir(ACPU,AOS) + PathDelim + UnitCo'+
  'nfigFile);'#013#010+
  '    APackage.GetCleanFiles(List,ACPU,AOS);'#013#010+
  '    if (List.Count>0) then'#013#010+
  '      begin'#013#010+
  '      CmdDeleteFiles(List);'#013#010+
  '      DirectoryLi','st := TStringList.Create;'#013#010+
  '      try'#013#010+
  '        GetDirectoriesFromFilelist(List,DirectoryList);'#013#010+
  '        CmdRemoveDirs(DirectoryList);'#013#010+
  #013#010+
  '        DirectoryList.Clear;'#013#010+
  '        if DirectoryExists(APackage.GetBinOutputDir(ACPU,AOS)) then'#013+
  #010+
  '     ','     DirectoryList.Add(APackage.GetBinOutputDir(ACPU,AOS));'#013+
  #010+
  '        if DirectoryExists(APackage.GetUnitsOutputDir(ACPU,AOS)) then'#013+
  #010+
  '          DirectoryList.Add(APackage.GetUnitsOutputDir(ACPU,AOS));'#013+
  #010+
  '        CmdRemoveDirs(DirectoryList);'#013#010+
  #013,#010+
  '        DirectoryList.Clear;'#013#010+
  '        if DirectoryExists(ExtractFileDir(APackage.GetBinOutputDir(ACP'+
  'U,AOS))) then'#013#010+
  '          DirectoryList.Add(ExtractFileDir(APackage.GetBinOutputDir(AC'+
  'PU,AOS)));'#013#010+
  '        if DirectoryExists(ExtractFileDir(','APackage.GetUnitsOutputDir'+
  '(ACPU,AOS))) then'#013#010+
  '          DirectoryList.Add(ExtractFileDir(APackage.GetUnitsOutputDir('+
  'ACPU,AOS)));'#013#010+
  '        CmdRemoveDirs(DirectoryList);'#013#010+
  '      finally'#013#010+
  '        DirectoryList.Free;'#013#010+
  '      end;'#013#010+
  '      end;'#013#010+
  '  Fi','nally'#013#010+
  '    List.Free;'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Procedure TBuildEngine.Manifest(APackage : TPackage);'#013#010+
  'Var'#013#010+
  '  L : TStrings;'#013#010+
  '  PD,'#013#010+
  '  MF : String;'#013#010+
  'begin'#013#010+
  '  L:=TStringList.Create;'#013#010+
  '  Try'#013#010+
  '    Log(vlInfo, Format(SInfoManifestPackage,[APackage.Name]','));'#013#010+
  '    PD:=APackage.Directory;'#013#010+
  '    if PD<>'#039#039' then'#013#010+
  '      PD:=IncludeTrailingPathDelimiter(PD);'#013#010+
  '    MF:=PD+ManifestFile;'#013#010+
  '    Log(vlDebug, Format(SDbgGenerating, [MF]));'#013#010+
  '    L.Add('#039'<?xml version="1.0"?>'#039');'#013#010+
  '    L.Add('#039'<packages>'#039');'#013#010+
  '   ',' APackage.GetManifest(L);'#013#010+
  '    L.Add('#039'</packages>'#039');'#013#010+
  '    L.SaveToFile(MF);'#013#010+
  '  Finally'#013#010+
  '    L.Free;'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.Compile(Packages: TPackages);'#013#010+
  #013#010+
  '  function IsReadyToCompile(APackage:TPackage): boolean;'#013#010+
  '  begin',#013#010+
  '    result := False;'#013#010+
  '    if not APackage.FProcessing and (APackage.State=tsNeutral) then'#013+
  #010+
  '      begin'#013#010+
  '        if PackageOK(APackage) then'#013#010+
  '          result := ReadyToCompile(APackage)'#013#010+
  '        else'#013#010+
  '          begin'#013#010+
  '            inc(FPro','gressCount);'#013#010+
  '            log(vlWarning,SWarnSkipPackageTargetProgress,[(FProgressCo'+
  'unt)/FProgressMax * 100, APackage.Name, Defaults.Target]);'#013#010+
  '            APackage.FTargetState:=tsNoCompile;'#013#010+
  '          end;'#013#010+
  '      end;'#013#010+
  '  end;'#013#010+
  #013#010+
  'Var'#013#010+
  '  I ',': integer;'#013#010+
  '{$ifndef NO_THREADING}'#013#010+
  '  Thr : Integer;'#013#010+
  '  Finished : boolean;'#013#010+
  '  ErrorState: boolean;'#013#010+
  '  ErrorMessage: string;'#013#010+
  '  NotifyThreadWaiting : PRTLEvent;'#013#010+
  '  Threads : array of TCompileWorkerThread;'#013#010+
  '{$endif NO_THREADING}'#013#010+
  '  P : TPack','age;'#013#010+
  #013#010+
  '{$ifndef NO_THREADING}'#013#010+
  '  procedure ProcessThreadResult(ATHread: TCompileWorkerThread);'#013#010+
  '  var'#013#010+
  '    StartI: integer;'#013#010+
  '    CompilePackage: TPackage;'#013#010+
  '    PackageAvailable: boolean;'#013#010+
  '  begin'#013#010+
  '    if AThread.Done then'#013#010+
  '      begin'#013#010+
  '   ','     if assigned(AThread.APackage) then'#013#010+
  '          begin'#013#010+
  '            // The thread has completed compiling the package'#013#010+
  '            if AThread.CompilationOK then'#013#010+
  '              AThread.APackage.FTargetState:=tsCompiled'#013#010+
  '            else //',' A problem occured, stop the compilation'#013#010+
  '              begin'#013#010+
  '              ErrorState:=true;'#013#010+
  '              ErrorMessage:=AThread.ErrorMessage;'#013#010+
  '              Finished:=true;'#013#010+
  '              end;'#013#010+
  '            AThread.APackage := nil;'#013#010+
  '    ','      end;'#013#010+
  '        StartI := I;'#013#010+
  #013#010+
  '        CompilePackage := nil;'#013#010+
  '        PackageAvailable:=false;'#013#010+
  '        repeat'#013#010+
  '        if IsReadyToCompile(Packages.PackageItems[i]) then'#013#010+
  '          CompilePackage := Packages.PackageItems[i];'#013#010+
  '       ',' if not (Packages.PackageItems[i].State in [tsCompiled, tsNoC'+
  'ompile]) then'#013#010+
  '          PackageAvailable:=true;'#013#010+
  '        inc(I);'#013#010+
  '        if I=packages.Count then'#013#010+
  '          i := 0;'#013#010+
  '        until Assigned(CompilePackage) or (I=StartI);'#013#010+
  '    ','    if Assigned(CompilePackage) then'#013#010+
  '          begin'#013#010+
  '          // Instruct thread to compile package'#013#010+
  '          AThread.APackage := CompilePackage;'#013#010+
  '          AThread.APackage.FProcessing := true;'#013#010+
  '          AThread.FDone:=False;'#013#010+
  '       ','   RTLeventSetEvent(AThread.NotifyStartTask);'#013#010+
  '          end;'#013#010+
  '        if not PackageAvailable then'#013#010+
  '          Finished := True;'#013#010+
  '      end;'#013#010+
  '  end;'#013#010+
  #013#010+
  '{$endif NO_THREADING}'#013#010+
  #013#010+
  'begin'#013#010+
  '  If Assigned(BeforeCompile) then'#013#010+
  '    BeforeCompile(Sel','f);'#013#010+
  '  FProgressMax:=Packages.Count;'#013#010+
  '  FProgressCount:=0;'#013#010+
  #013#010+
  '  if Defaults.ThreadsAmount<0 then'#013#010+
  '    begin'#013#010+
  '      // Do not use any threading to compile the packages'#013#010+
  '      For I:=0 to Packages.Count-1 do'#013#010+
  '        begin'#013#010+
  '          P:=Packa','ges.PackageItems[i];'#013#010+
  '          If PackageOK(P) then'#013#010+
  '            MaybeCompile(P)'#013#010+
  '          else'#013#010+
  '            begin'#013#010+
  '            inc(FProgressCount);'#013#010+
  '            log(vlWarning,SWarnSkipPackageTargetProgress,[(FProgressCo'+
  'unt)/FProgressMax ','* 100, P.Name, Defaults.Target]);'#013#010+
  '            end;'#013#010+
  '        end;'#013#010+
  '    end'#013#010+
  '  else'#013#010+
  '    begin'#013#010+
  '{$ifndef NO_THREADING}'#013#010+
  '      // Use worker-threads to compile the packages'#013#010+
  '      ErrorState := False;'#013#010+
  '      Finished := False;'#013#010+
  '      I := 0;'#013,#010+
  '      // This event is set by the worker-threads to notify the main/th'+
  'is thread'#013#010+
  '      // that a package finished it'#039's task.'#013#010+
  '      NotifyThreadWaiting := RTLEventCreate;'#013#010+
  '      SetLength(Threads,Defaults.ThreadsAmount);'#013#010+
  '      // Create a','ll worker-threads'#013#010+
  '      for Thr:=0 to Defaults.ThreadsAmount-1 do'#013#010+
  '        Threads[Thr] := TCompileWorkerThread.Create(self,NotifyThreadW'+
  'aiting);'#013#010+
  '      try'#013#010+
  '        // When a thread notifies this thread that it is ready, loop o'+
  'n all'#013#010+
  '    ','    // threads to check their state and if possible assign a new'+
  ' package'#013#010+
  '        // to them to compile.'#013#010+
  '        while not Finished do'#013#010+
  '          begin'#013#010+
  '            RTLeventWaitFor(NotifyThreadWaiting);'#013#010+
  '            for Thr:=0 to Defaults.','ThreadsAmount-1 do if not Finishe'+
  'd then'#013#010+
  '              ProcessThreadResult(Threads[Thr]);'#013#010+
  '          end;'#013#010+
  '        // Compilation finished or aborted. Wait for all threads to en'+
  'd.'#013#010+
  '        for thr:=0 to Defaults.ThreadsAmount-1 do'#013#010+
  '        ','  begin'#013#010+
  '            Threads[Thr].Terminate;'#013#010+
  '            RTLeventSetEvent(Threads[Thr].NotifyStartTask);'#013#010+
  '            Threads[Thr].WaitFor;'#013#010+
  '          end;'#013#010+
  '      finally'#013#010+
  '        RTLeventdestroy(NotifyThreadWaiting);'#013#010+
  '        for thr:=0 t','o Defaults.ThreadsAmount-1 do'#013#010+
  '          Threads[Thr].Free;'#013#010+
  '      end;'#013#010+
  '    if ErrorState then'#013#010+
  '      raise Exception.Create(ErrorMessage);'#013#010+
  '{$endif NO_THREADING}'#013#010+
  '    end;'#013#010+
  '  If Assigned(AfterCompile) then'#013#010+
  '    AfterCompile(Self);'#013#010+
  'end;'#013#010,
  #013#010+
  #013#010+
  'procedure TBuildEngine.Install(Packages: TPackages);'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  '  P : TPackage;'#013#010+
  'begin'#013#010+
  '  If Assigned(BeforeInstall) then'#013#010+
  '    BeforeInstall(Self);'#013#010+
  '  For I:=0 to Packages.Count-1 do'#013#010+
  '    begin'#013#010+
  '      P:=Packages.PackageItems','[i];'#013#010+
  '      If PackageOK(P) then'#013#010+
  '        begin'#013#010+
  '          Install(P);'#013#010+
  '          log(vlWarning, SWarnInstallationPackagecomplete, [P.Name, De'+
  'faults.Target]);'#013#010+
  '        end'#013#010+
  '      else'#013#010+
  '        log(vlWarning,SWarnSkipPackageTarget,[P.Name, D','efaults.Targe'+
  't]);'#013#010+
  '    end;'#013#010+
  '  If Assigned(AfterInstall) then'#013#010+
  '    AfterInstall(Self);'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TBuildEngine.ZipInstall(Packages: TPackages);'#013#010+
  #013#010+
  '  procedure CreateZipFile;'#013#010+
  '  var'#013#010+
  '    I : Integer;'#013#010+
  '    P : TPackage;'#013#010+
  '  begin'#013#010+
  '    ','FZipper := TZipper.Create;'#013#010+
  '    try'#013#010+
  '      For I:=0 to Packages.Count-1 do'#013#010+
  '        begin'#013#010+
  '          P:=Packages.PackageItems[i];'#013#010+
  '          If PackageOK(P) then'#013#010+
  '            begin'#013#010+
  '              FZipper.FileName := Defaults.ZipPrefix + P.N','ame + '#039+
  '.'#039' + MakeTargetString(Defaults.CPU,Defaults.OS) +'#039'.zip'#039';'#013+
  #010+
  '              Install(P);'#013#010+
  '              FZipper.ZipAllFiles;'#013#010+
  '              FZipper.Clear;'#013#010+
  '              log(vlWarning, SWarnInstallationPackagecomplete, [P.Name'+
  ', Defaults.T','arget]);'#013#010+
  '            end'#013#010+
  '          else'#013#010+
  '            log(vlWarning,SWarnSkipPackageTarget,[P.Name, Defaults.Tar'+
  'get]);'#013#010+
  '        end;'#013#010+
  '    finally'#013#010+
  '      FZipper.Free;'#013#010+
  '    end;'#013#010+
  '  end;'#013#010+
  #013#010+
  '  {$ifdef HAS_TAR_SUPPORT}'#013#010+
  '  procedure CreateTarFi','le;'#013#010+
  '  var'#013#010+
  '    I : Integer;'#013#010+
  '    P : TPackage;'#013#010+
  '    S : TGZFileStream;'#013#010+
  '  begin;'#013#010+
  '    For I:=0 to Packages.Count-1 do'#013#010+
  '      begin'#013#010+
  '        P:=Packages.PackageItems[i];'#013#010+
  '        If PackageOK(P) then'#013#010+
  '          begin'#013#010+
  '            S := TGZFi','leStream.create(Defaults.ZipPrefix + P.Name + '+
  #039'.'#039' + MakeTargetString(Defaults.CPU,Defaults.OS) +'#039'.tar.gz'#039+
  ', gzopenwrite);'#013#010+
  '            try'#013#010+
  '              FTarWriter := TTarWriter.Create(S);'#013#010+
  '              FTarWriter.Permissions := [tpReadByO','wner, tpWriteByOwn'+
  'er, tpReadByGroup, tpReadByOther];'#013#010+
  '              FTarWriter.UserName := '#039'root'#039';'#013#010+
  '              FTarWriter.GroupName := '#039'root'#039';'#013#010+
  '              try'#013#010+
  '                Install(P);'#013#010+
  '                log(vlWarning, SWarnInstallat','ionPackagecomplete, [P.'+
  'Name, Defaults.Target]);'#013#010+
  '              finally'#013#010+
  '                FTarWriter.Free;'#013#010+
  '              end;'#013#010+
  '            finally'#013#010+
  '              S.Free;'#013#010+
  '            end;'#013#010+
  '          end'#013#010+
  '        else'#013#010+
  '          log(vlWarning',',SWarnSkipPackageTarget,[P.Name, Defaults.Tar'+
  'get]);'#013#010+
  '      end;'#013#010+
  '  end;'#013#010+
  '  {$endif HAS_TAR_SUPPORT}'#013#010+
  #013#010+
  'begin'#013#010+
  '  If Assigned(BeforeInstall) then'#013#010+
  '    BeforeInstall(Self);'#013#010+
  #013#010+
  '  Defaults.IntSetBaseInstallDir('#039'lib/fpc/'#039' + Defaults.FCompile'+
  'rVers','ion+ '#039'/'#039');'#013#010+
  #013#010+
  '  {$ifdef unix}'#013#010+
  '  {$ifdef HAS_TAR_SUPPORT}'#013#010+
  '  CreateTarFile;'#013#010+
  '  {$else HAS_TAR_SUPPORT}'#013#010+
  '  CreateZipFile;'#013#010+
  '  {$endif HAS_TAR_SUPPORT}'#013#010+
  '  {$else unix}'#013#010+
  '  CreateZipFile;'#013#010+
  '  {$endif unix}'#013#010+
  #013#010+
  '  If Assigned(AfterInstall) then'#013#010+
  '   ',' AfterInstall(Self);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.Archive(Packages: TPackages);'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  '  P : TPackage;'#013#010+
  'begin'#013#010+
  '  If Assigned(BeforeArchive) then'#013#010+
  '    BeforeArchive(Self);'#013#010+
  '  Log(vlDebug, SDbgBuildEngineArchiving);'#013#010+
  '  For',' I:=0 to Packages.Count-1 do'#013#010+
  '    begin'#013#010+
  '      P:=Packages.PackageItems[i];'#013#010+
  '      Archive(P);'#013#010+
  '    end;'#013#010+
  '  If Assigned(AfterArchive) then'#013#010+
  '    AfterArchive(Self);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.Manifest(Packages: TPackages);'#013#010+
  'Var'#013#010+
  '  I :',' Integer;'#013#010+
  '  P : TPackage;'#013#010+
  'begin'#013#010+
  '  If Assigned(BeforeManifest) then'#013#010+
  '    BeforeManifest(Self);'#013#010+
  '  Log(vlDebug, SDbgBuildEngineGenerateManifests);'#013#010+
  '  For I:=0 to Packages.Count-1 do'#013#010+
  '    begin'#013#010+
  '      P:=Packages.PackageItems[i];'#013#010+
  '      Man','ifest(P);'#013#010+
  '    end;'#013#010+
  '  If Assigned(AfterManifest) then'#013#010+
  '    AfterManifest(Self);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TBuildEngine.Clean(Packages: TPackages; AllTargets: boolean)'+
  ';'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  '  P : TPackage;'#013#010+
  'begin'#013#010+
  '  If Assigned(BeforeClean) the','n'#013#010+
  '    BeforeClean(Self);'#013#010+
  '  Log(vldebug, SDbgBuildEngineCleaning);'#013#010+
  '  For I:=0 to Packages.Count-1 do'#013#010+
  '    begin'#013#010+
  '      P:=Packages.PackageItems[i];'#013#010+
  '      If AllTargets or PackageOK(P) then'#013#010+
  '        Clean(P, AllTargets);'#013#010+
  '      log(vlWarn','ing, SWarnCleanPackagecomplete, [P.Name]);'#013#010+
  '    end;'#013#010+
  '  If Assigned(AfterClean) then'#013#010+
  '    AfterClean(Self);'#013#010+
  'end;'#013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                               TFPVersion'#013#010+
  '*','*******************************************************************'+
  '********}'#013#010+
  #013#010+
  'function TFPVersion.GetAsString: String;'#013#010+
  'begin'#013#010+
  '  if Empty then'#013#010+
  '    Result:='#039'<none>'#039#013#010+
  '  else'#013#010+
  '  begin'#013#010+
  '    Result := '#039#039';'#013#010+
  '    if Major <> -1 then'#013#010+
  '      Resul','t := Result + IntToStr(Major);'#013#010+
  '    if Minor <> -1 then'#013#010+
  '      Result := Result + '#039'.'#039' + IntToStr(Minor);'#013#010+
  '    if Micro <> -1 then'#013#010+
  '      Result := Result + '#039'.'#039' + IntToStr(Micro);'#013#010+
  '    if Build <> -1 then'#013#010+
  '      Result := Result + '#039'-'#039'  + Int','ToStr(Build);'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TFPVersion.GetEmpty: Boolean;'#013#010+
  'begin'#013#010+
  '  Result:=(Major=0) and (Minor=0) and (Micro=0) and (Build=0);'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TFPVersion.SetAsString(const AValue: String);'#013#010+
  #013#010+
  '  Function NextDigit(sep : Char','; var V : string) : integer;'#013#010+
  '  Var'#013#010+
  '    P : Integer;'#013#010+
  '  begin'#013#010+
  '    P:=Pos(Sep,V);'#013#010+
  '    If (P=0) then'#013#010+
  '      P:=Length(V)+1;'#013#010+
  '    Result:=StrToIntDef(Copy(V,1,P-1),-1);'#013#010+
  '    If Result<>-1 then'#013#010+
  '      Delete(V,1,P);'#013#010+
  '  end;'#013#010+
  #013#010+
  'Var'#013#010+
  '  V : Str','ing;'#013#010+
  'begin'#013#010+
  '  Clear;'#013#010+
  '  // Special support for empty version string'#013#010+
  '  if (AValue='#039#039') or (AValue='#039'<none>'#039') then'#013#010+
  '    exit;'#013#010+
  '  V:=AValue;'#013#010+
  '  Major:=NextDigit('#039'.'#039',V);'#013#010+
  '  Minor:=NextDigit('#039'.'#039',V);'#013#010+
  '  Micro:=NextDigit('#039'-'#039',V);'#013#010+
  '  Build:=NextDigi','t(#0,V);'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TFPVersion.Clear;'#013#010+
  'begin'#013#010+
  '  Micro:=-1;'#013#010+
  '  Major:=-1;'#013#010+
  '  Minor:=-1;'#013#010+
  '  Build:=-1;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TFPVersion.Assign(Source: TPersistent);'#013#010+
  'Var'#013#010+
  '  V : TFPVersion;'#013#010+
  'begin'#013#010+
  '  if Source is TFPVersion then'#013#010+
  '    begi','n'#013#010+
  '    V:=Source as TFPVersion;'#013#010+
  '    Major:=V.Major;'#013#010+
  '    Minor:=V.Minor;'#013#010+
  '    Micro:=V.Micro;'#013#010+
  '    Build:=V.Build;'#013#010+
  '    end'#013#010+
  '  else'#013#010+
  '    inherited Assign(Source);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TFPVersion.CompareVersion(AVersion: TFPVersion): Integer;'#013,
  #010+
  'begin'#013#010+
  '  Result:=Major-AVersion.Major;'#013#010+
  '  If (Result=0) then'#013#010+
  '    begin'#013#010+
  '      Result:=Minor-AVersion.Minor;'#013#010+
  '      if (Result=0) then'#013#010+
  '        begin'#013#010+
  '          Result:=Micro-AVersion.Micro;'#013#010+
  '          If (Result=0) then'#013#010+
  '            Resul','t:=Build-AVersion.Build;'#013#010+
  '        end;'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  'function TFPVersion.SameVersion(AVersion: TFPVersion): Boolean;'#013#010+
  'begin'#013#010+
  '  Result:=CompareVersion(AVersion)=0;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{************************************************************','*******'+
  '*********'#013#010+
  '                                 TTarget'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'constructor TTarget.Create(ACollection: TCollection);'#013#010+
  'begin'#013#010+
  '  inherited Create(ACollection);'#013#010+
  '  F','Install:=True;'#013#010+
  '  FCPUs:=AllCPUs;'#013#010+
  '  FOSes:=AllOSes;'#013#010+
  '  FUnitPath:=TConditionalStrings.Create(TConditionalString);'#013#010+
  '  FIncludePath:=TConditionalStrings.Create(TConditionalString);'#013#010+
  '  FObjectPath:=TConditionalStrings.Create(TConditionalStrin','g);'#013#010+
  '  FDependencies:=TDependencies.Create(TDependency);'#013#010+
  '  FCommands:=TCommands.Create(TCommand);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'destructor TTarget.Destroy;'#013#010+
  'begin'#013#010+
  '  FreeAndNil(FUnitPath);'#013#010+
  '  FreeAndNil(FObjectPath);'#013#010+
  '  FreeAndNil(FIncludePath);'#013#010+
  '  FreeAndNi','l(FDependencies);'#013#010+
  '  FreeAndNil(FCommands);'#013#010+
  '  FreeAndNil(Foptions);'#013#010+
  '  inherited Destroy;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TTarget.AssignTo(Dest: TPersistent);'#013#010+
  'var'#013#010+
  '  DestTarget: TTarget;'#013#010+
  'begin'#013#010+
  '  if Dest is TTarget then'#013#010+
  '    begin'#013#010+
  '    DestTarget := ','TTarget(Dest);'#013#010+
  '    DestTarget.Dependencies.Assign(Dependencies);'#013#010+
  '    DestTarget.Commands.Assign(Commands);'#013#010+
  '    DestTarget.FTargetState := FTargetState;'#013#010+
  '    DestTarget.TargetType := TargetType;'#013#010+
  '    DestTarget.CPUs := CPUs;'#013#010+
  '    DestTarg','et.OSes := OSes;'#013#010+
  '    DestTarget.Mode := Mode;'#013#010+
  '    DestTarget.Options := Options;'#013#010+
  '    DestTarget.Name :=  Name;'#013#010+
  '    DestTarget.Extension:= Extension;'#013#010+
  '    DestTarget.FPCTarget := FPCTarget;'#013#010+
  '    DestTarget.FileType := FileType;'#013#010+
  '    Dest','Target.Directory := Directory;'#013#010+
  '    DestTarget.ResourceStrings := ResourceStrings;'#013#010+
  '    DestTarget.Install := Install;'#013#010+
  '    DestTarget.FTargetSourceFileName := fTargetSourceFileName;'#013#010+
  '    DestTarget.ObjectPath.Assign(ObjectPath);'#013#010+
  '    DestT','arget.UnitPath.Assign(UnitPath);'#013#010+
  '    DestTarget.IncludePath.Assign(IncludePath);'#013#010+
  '    DestTarget.FXML := FXML;'#013#010+
  '    DestTarget.AfterCompile := AfterCompile;'#013#010+
  '    DestTarget.BeforeCompile := BeforeCompile;'#013#010+
  '    DestTarget.BeforeClean := Bef','oreCompile;'#013#010+
  '    DestTarget.AfterClean := AfterClean;'#013#010+
  '    end'#013#010+
  '  else'#013#010+
  '    inherited AssignTo(Dest);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TTarget.GetOptions: TStrings;'#013#010+
  'begin'#013#010+
  '  If Foptions=Nil then'#013#010+
  '    FOptions:=TStringList.Create;'#013#010+
  '  Result:=FOptions;'#013#010+
  'en','d;'#013#010+
  #013#010+
  'function TTarget.GetImportLibFileName(AOS : TOS) : String;'#013#010+
  'begin'#013#010+
  '  result := GetImportLibraryFilename(Name,AOS);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TTarget.GetUnitLibFileName(AOS : TOS): String;'#013#010+
  'begin'#013#010+
  '  if AOS in [atari,netwlibc,go32v2,watcom,wdos','x,msdos] then'#013#010+
  '    Result := Name+LibExt'#013#010+
  '  else if AOS in [java] then'#013#010+
  '    Result:=Name+'#039'.jar'#039#013#010+
  '  else if AOS in [macos] then'#013#010+
  '    Result:=Name+'#039'Lib'#039#013#010+
  '  else'#013#010+
  '    Result:='#039'libp'#039'+Name+LibExt;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TTarget.SetOptions(const AVal','ue: TStrings);'#013#010+
  'begin'#013#010+
  '  If (AValue=Nil) or (AValue.Count=0) then'#013#010+
  '    FreeAndNil(FOptions)'#013#010+
  '  else'#013#010+
  '    Options.Assign(AValue);'#013#010+
  'end;'#013#010+
  #013#010+
  'function TTarget.GetSourceFileName: String;'#013#010+
  'begin'#013#010+
  '  Result:=Name+FExtension;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TTa','rget.GetUnitFileName: String;'#013#010+
  'begin'#013#010+
  '  Result:=Name+UnitExt;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TTarget.GetObjectFileName: String;'#013#010+
  'begin'#013#010+
  '  Result:=Name+ObjExt;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TTarget.GetRSTFileName: String;'#013#010+
  'begin'#013#010+
  '  Result:=Name+RSText;'#013#010+
  'end;'#013#010+
  #013#010,
  #013#010+
  'function TTarget.GetRSJFileName: String;'#013#010+
  'begin'#013#010+
  '  Result:=Name+RSJext;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TTarget.GetProgramFileName(AOS : TOS): String;'#013#010+
  'begin'#013#010+
  '  result := AddProgramExtension(Name, AOS);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TTarget.GetProgramDebugFile','Name(AOS: TOS): String;'#013#010+
  'begin'#013#010+
  '  result := Name + DbgExt;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TTarget.GetOutputFileName(AOs: TOS): String;'#013#010+
  'begin'#013#010+
  '  if TargetType in UnitTargets then'#013#010+
  '    Result:=GetUnitFileName'#013#010+
  '  else'#013#010+
  '    Result:=GetProgramFileName(AOs',');'#013#010+
  'end;'#013#010+
  #013#010+
  'function TTarget.HaveOptions: Boolean;'#013#010+
  'begin'#013#010+
  '  Result:=(FOptions<>Nil);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TTarget.SetName(const AValue: String);'#013#010+
  'Var'#013#010+
  '  D,N,E : String;'#013#010+
  'begin'#013#010+
  '  N:=FixPath(AValue, False);'#013#010+
  '  D:=ExtractFilePath(N);'#013#010+
  '  E:=E','xtractFileExt(N);'#013#010+
  '  N:=ExtractFileName(N);'#013#010+
  '  inherited SetName(Copy(N,1,Length(N)-Length(E)));'#013#010+
  '  FExtension:=E;'#013#010+
  '  FDirectory:=D;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TTarget.SetXML(const AValue: string);'#013#010+
  'begin'#013#010+
  '  FXML:=FixPath(AValue, False);'#013#010+
  'end;'#013#010+
  #013#010+
  'p','rocedure TTarget.GetCleanFiles(List: TStrings; const APrefixU, APre'+
  'fixB : String; ACPU: TCPU; AOS : TOS);'#013#010+
  'begin'#013#010+
  '  If not(ACPU in CPUs) or not(AOS in OSes) then'#013#010+
  '    exit;'#013#010+
  '  List.Add(APrefixU + ObjectFileName);'#013#010+
  '  If (TargetType in [ttUni','t,ttImplicitUnit,ttExampleUnit, ttCleanOnl'+
  'yUnit]) then'#013#010+
  '    begin'#013#010+
  '      List.Add(APrefixU + UnitFileName);'#013#010+
  '      if (AOS in AllSmartLinkLibraryOSes) and FileExists(APrefixU + Ge'+
  'tUnitLibFileName(AOS)) then'#013#010+
  '        List.Add(APrefixU + GetU','nitLibFileName(AOS));'#013#010+
  '      if (AOS in AllImportLibraryOSes) and FileExists(APrefixU + GetIm'+
  'portLibFilename(AOS)) then'#013#010+
  '        List.Add(APrefixU + GetImportLibFilename(AOS));'#013#010+
  '    end'#013#010+
  '  else If (TargetType in [ttProgram,ttExampleProgram]',') then'#013#010+
  '    begin'#013#010+
  '    List.Add(APrefixB + GetProgramFileName(AOS));'#013#010+
  '    if FileExists(APrefixB + GetProgramDebugFileName(AOS)) then'#013#010+
  '      List.Add(APrefixB + GetProgramDebugFileName(AOS));'#013#010+
  '    end;'#013#010+
  '  If ResourceStrings then'#013#010+
  '    begin'#013,#010+
  '      // choose between 2 possible resource files'#013#010+
  '      if FileExists(APrefixU + RSJFileName) then'#013#010+
  '        List.Add(APrefixU + RSJFileName)'#013#010+
  '      else'#013#010+
  '        List.Add(APrefixU + RSTFileName);'#013#010+
  '    end;'#013#010+
  '  // Maybe add later ?  AddCond','itionalStrings(List,CleanFiles);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TTarget.GetInstallFiles(List: TStrings; const APrefixU, APre'+
  'fixB: String; ACPU: TCPU; AOS : TOS);'#013#010+
  'begin'#013#010+
  '  If Not (TargetType in [ttProgram,ttExampleProgram]) and FileExists(A'+
  'PrefixU + ','ObjectFileName) then'#013#010+
  '    // The compiler does not create an objectfile for all programs.'#013+
  #010+
  '    List.Add(APrefixU + ObjectFileName);'#013#010+
  '  If (TargetType in [ttUnit,ttImplicitUnit,ttExampleUnit]) then'#013#010+
  '    begin'#013#010+
  '    List.Add(APrefixU + UnitFil','eName);'#013#010+
  '    if (AOS in AllSmartLinkLibraryOSes) and FileExists(APrefixU + GetU'+
  'nitLibFileName(AOS)) then'#013#010+
  '      List.Add(APrefixU + GetUnitLibFileName(AOS));'#013#010+
  '    if (AOS in AllImportLibraryOSes) and FileExists(APrefixU + GetImpo'+
  'rtLibFilena','me(AOS)) then'#013#010+
  '      List.Add(APrefixU + GetImportLibFilename(AOS));'#013#010+
  '    end'#013#010+
  '  else If (TargetType in [ttProgram,ttExampleProgram]) then'#013#010+
  '    List.Add(APrefixB + GetProgramFileName(AOS));'#013#010+
  '  If ResourceStrings then'#013#010+
  '    begin'#013#010+
  '      // ch','oose between 2 possible resource files'#013#010+
  '      if FileExists(APrefixU + RSJFileName) then'#013#010+
  '        List.Add(APrefixU + RSJFileName)'#013#010+
  '      else'#013#010+
  '        List.Add(APrefixU + RSTFileName);'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TTarget.GetArchiveFiles','(List: TStrings; ACPU: TCPU; AOS : '+
  'TOS);'#013#010+
  'var'#013#010+
  '  i : integer;'#013#010+
  '  D : TDependency;'#013#010+
  'begin'#013#010+
  '  If not(ACPU in CPUs) or not(AOS in OSes) then'#013#010+
  '    exit;'#013#010+
  '  // Main source'#013#010+
  '  if TargetSourceFileName<>'#039#039' then'#013#010+
  '    List.Add(TargetSourceFileName);'#013,#010+
  '  // Includes'#013#010+
  '  for i:=0 to Dependencies.Count-1 do'#013#010+
  '    begin'#013#010+
  '      D:=Dependencies[i];'#013#010+
  '      if (D.DependencyType=depInclude) and'#013#010+
  '         (D.TargetFileName<>'#039#039') then'#013#010+
  '        List.Add(D.TargetFileName);'#013#010+
  '    end;'#013#010+
  '  // FPDoc files'#013#010,
  '  if XML <> '#039#039' then'#013#010+
  '  begin'#013#010+
  '    List.Add(Directory + Name + Extension);'#013#010+
  '    List.Add(XML);'#013#010+
  '  end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                                 TSource'#013#010+
  '******','**************************************************************'+
  '********}'#013#010+
  #013#010+
  'function TSource.GetInstallSourcePath: string;'#013#010+
  'begin'#013#010+
  '  if FInstallSourcePath<>'#039#039' then'#013#010+
  '    result := FInstallSourcePath'#013#010+
  '  else if SourceType=stExample then'#013#010+
  '    r','esult := '#039'examples'#039#013#010+
  '  else'#013#010+
  '    result := '#039#039';'#013#010+
  'end;'#013#010+
  #013#010+
  'constructor TSource.Create(ACollection: TCollection);'#013#010+
  'begin'#013#010+
  '  inherited Create(ACollection);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'destructor TSource.Destroy;'#013#010+
  'begin'#013#010+
  '  inherited Destroy;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TS','ource.GetInstallFiles(List: TStrings);'#013#010+
  'begin'#013#010+
  '  if InstallSourcePath<>'#039#039' then'#013#010+
  '    list.Values[name] := (IncludeTrailingPathDelimiter(InstallSourcePa'+
  'th)+ExtractFileName(Name))'#013#010+
  '  else'#013#010+
  '    list.add(Name);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{**********************','*********************************************'+
  '*********'#013#010+
  '                                 TCommands'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'function TCommands.GetCommand(const Dest : String): TComman','d;'#013#010+
  'begin'#013#010+
  '  Result:=TCommand(ItemByName(Dest));'#013#010+
  'end;'#013#010+
  #013#010+
  'function TCommands.GetCommandItem(Index : Integer): TCommand;'#013#010+
  'begin'#013#010+
  '  Result:=TCommand(Items[Index]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TCommands.SetCommandItem(Index : Integer; const AValue: T','C'+
  'ommand);'#013#010+
  'begin'#013#010+
  '  Items[Index]:=AValue;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TCommands.AddCommand(const Cmd: String) : TCommand;'#013#010+
  'begin'#013#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,'#039#039','#039#039','#039#039');'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TCommands.AddCommand(const Cmd, Options: String): T','Command;'+
  #013#010+
  'begin'#013#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,Options,'#039#039','#039#039');'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TCommands.AddCommand(const Cmd, Options, Dest, Source: String'+
  ' ): TCommand;'#013#010+
  'begin'#013#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,options,Dest,Source);'#013#010+
  'end;'#013#010+
  #013#010+
  #013,#010+
  'Function TCommands.AddCommand(At: TCommandAt; const Cmd: String) : TCo'+
  'mmand;'#013#010+
  'begin'#013#010+
  '  Result:=AddCommand(At,Cmd,'#039#039','#039#039','#039#039');'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TCommands.AddCommand(At: TCommandAt; const Cmd, Options: Stri'+
  'ng  ): TCommand;'#013#010+
  'begin'#013#010+
  '  Result:','=AddCommand(At,Cmd,Options,'#039#039','#039#039');'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TCommands.AddCommand(At: TCommandAt; const Cmd, Options, Dest'+
  ', Source: String): TCommand;'#013#010+
  'begin'#013#010+
  '  Result:=Add as TCommand;'#013#010+
  '  Result.Command:=Cmd;'#013#010+
  '  If (Options<>'#039#039') then'#013#010+
  '    Result','.ParseOptions(Options);'#013#010+
  '  Result.At:=At;'#013#010+
  '  Result.SourceFile:=Source;'#013#010+
  '  Result.DestFile:=Dest;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                           TConditionalString'#013#010+
  '*****','***************************************************************'+
  '********}'#013#010+
  #013#010+
  'Constructor TConditionalString.Create;'#013#010+
  'begin'#013#010+
  '  inherited Create;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '       ','                    TConditionalStrings'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'Constructor TConditionalStrings.Create(AClass:TConditionalStringClass)'+
  ';'#013#010+
  'begin'#013#010+
  '  inherited Create;'#013#010+
  '  FCSClass:=AClas','s;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TConditionalStrings.GetConditionalString(Index : Integer): TC'+
  'onditionalString;'#013#010+
  'begin'#013#010+
  '  Result:=TConditionalString(Items[Index]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TConditionalStrings.SetConditionalString(Index : Integer; co'+
  'nst A','Value: TConditionalString);'#013#010+
  'begin'#013#010+
  '  Items[Index]:=AValue;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TConditionalStrings.Add(Const Value : String) : TConditionalS'+
  'tring;'#013#010+
  'begin'#013#010+
  '  result:=Add(Value,AllCPUs,AllOSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{$ifdef cpu_only_overloads}'#013#010+
  'Functi','on TConditionalStrings.Add(Const Value : String;const CPUs:TCP'+
  'Us) : TConditionalString;'#013#010+
  'begin'#013#010+
  '  result:=Add(Value,CPUs,AllOSes);'#013#010+
  'end;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  #013#010+
  #013#010+
  'Function TConditionalStrings.Add(Const Value : String;const OSes:TOSe',
  's) : TConditionalString;'#013#010+
  'begin'#013#010+
  '  result:=Add(Value,AllCPUs,OSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TConditionalStrings.Add(Const Value : String;const CPUs:TCPUs'+
  ';const OSes:TOSes) : TConditionalString;'#013#010+
  'begin'#013#010+
  '  Result:=FCSClass.Create;'#013#010+
  '  Result.Value',':=Value;'#013#010+
  '  Result.OSes:=OSes;'#013#010+
  '  Result.CPUs:=CPUs;'#013#010+
  '  inherited Add(Result);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                                TDependency'#013#010+
  '**************************','******************************************'+
  '********}'#013#010+
  #013#010+
  'Constructor TDependency.Create;'#013#010+
  'begin'#013#010+
  '  inherited Create;'#013#010+
  '  FVersion:=TFPVersion.Create;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Destructor TDependency.Destroy;'#013#010+
  'begin'#013#010+
  '  FreeAndNil(FVersion);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Functio','n TDependency.GetVersion : string;'#013#010+
  'begin'#013#010+
  '  result:=FVersion.AsString;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Procedure TDependency.SetVersion(const V : string);'#013#010+
  'begin'#013#010+
  '  FVersion.AsString:=V;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*************************************************************','******'+
  '*********'#013#010+
  '                                TDependencies'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'function TDependencies.GetDependency(Index : Integer): TDependency;'#013+
  #010+
  'begin'#013#010+
  '  Result:=TDependen','cy(Items[Index]);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TDependencies.SetDependency(Index : Integer; const AValue: T'+
  'Dependency);'#013#010+
  'begin'#013#010+
  '  Items[Index]:=AValue;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TDependencies.Add(Const Value : String) : TDependency;'#013#010+
  'begin'#013#010+
  '  result:=Add','(Value,AllCPUs,AllOSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{$ifdef cpu_only_overloads}'#013#010+
  'Function TDependencies.Add(Const Value : String;const CPUs:TCPUs) : TD'+
  'ependency;'#013#010+
  'begin'#013#010+
  '  result:=Add(Value,CPUs,AllOSes);'#013#010+
  'end;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  #013#010+
  #013#010+
  'Function TD','ependencies.Add(Const Value : String;const OSes:TOSes) : '+
  'TDependency;'#013#010+
  'begin'#013#010+
  '  result:=Add(Value,AllCPUs,OSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TDependencies.Add(Const Value : String;const CPUs:TCPUs;const'+
  ' OSes:TOSes) : TDependency;'#013#010+
  'begin'#013#010+
  '  Result:=','inherited Add(Value,CPUs,OSes) as TDependency;'#013#010+
  '  Result.Target:=nil;'#013#010+
  '  Result.FDependencyType:=depPackage;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TDependencies.AddUnit(Const Value : String) : TDependency;'#013+
  #010+
  'begin'#013#010+
  '  result:=AddUnit(Value,AllCPUs,AllOSes);'#013#010+
  'e','nd;'#013#010+
  #013#010+
  #013#010+
  '{$ifdef cpu_only_overloads}'#013#010+
  'Function TDependencies.AddUnit(Const Value : String;const CPUs:TCPUs) '+
  ': TDependency;'#013#010+
  'begin'#013#010+
  '  result:=AddUnit(Value,CPUs,AllOSes);'#013#010+
  'end;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  #013#010+
  #013#010+
  'Function TDependencies.AddUnit','(Const Value : String;const OSes:TOSes'+
  ') : TDependency;'#013#010+
  'begin'#013#010+
  '  result:=AddUnit(Value,AllCPUs,OSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TDependencies.AddUnit(Const Value : String;const CPUs:TCPUs;c'+
  'onst OSes:TOSes) : TDependency;'#013#010+
  'begin'#013#010+
  '  Result:=inherit','ed Add(Value,CPUs,OSes) as TDependency;'#013#010+
  '  Result.Target:=nil;'#013#010+
  '  Result.FDependencyType:=depUnit;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TDependencies.AddInclude(Const Value : String) : TDependency;'+
  #013#010+
  'begin'#013#010+
  '  result:=AddInclude(Value,AllCPUs,AllOSes);'#013#010+
  'end;'#013,#010+
  #013#010+
  #013#010+
  '{$ifdef cpu_only_overloads}'#013#010+
  'Function TDependencies.AddInclude(Const Value : String;const CPUs:TCPU'+
  's) : TDependency;'#013#010+
  'begin'#013#010+
  '  result:=AddInclude(Value,CPUs,AllOSes);'#013#010+
  'end;'#013#010+
  '{$endif cpu_only_overloads}'#013#010+
  #013#010+
  #013#010+
  'Function TDependencies.AddIn','clude(Const Value : String;const OSes:TO'+
  'Ses) : TDependency;'#013#010+
  'begin'#013#010+
  '  result:=AddInclude(Value,AllCPUs,OSes);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TDependencies.AddInclude(Const Value : String;const CPUs:TCPU'+
  's;const OSes:TOSes) : TDependency;'#013#010+
  'Var'#013#010+
  '  N : St','ring;'#013#010+
  'begin'#013#010+
  '  N:=FixPath(Value, False);'#013#010+
  '  if ExtractFileExt(N)='#039#039' then'#013#010+
  '    ChangeFileExt(N,IncExt);'#013#010+
  '  Result:=inherited Add(N,CPUs,OSes) as TDependency;'#013#010+
  '  Result.FDependencyType:=depInclude;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*******************************','************************************'+
  '*********'#013#010+
  '                               TValueItem'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'constructor TValueItem.Create(AValue: String);'#013#010+
  'begin'#013#010+
  '  FValue:=AVal','ue;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                              TFunctionItem'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'constructor TFunctio','nItem.Create(AFunc: TReplaceFunction);'#013#010+
  'begin'#013#010+
  '  FFunc:=AFunc;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  '{*********************************************************************'+
  '*******'#013#010+
  '                                 TDictionary'#013#010+
  '*****************************************','***************************'+
  '********}'#013#010+
  #013#010+
  'constructor TDictionary.Create(AOwner: TComponent);'#013#010+
  'begin'#013#010+
  '  inherited Create(AOwner);'#013#010+
  '  FList:=TStringList.Create;'#013#010+
  '  FList.Sorted:=True;'#013#010+
  '  FList.Duplicates:=dupError;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'destructor TDicti','onary.Destroy;'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  'begin'#013#010+
  '  For I:=0 to Flist.Count-1 do'#013#010+
  '    FList.Objects[i].Free;'#013#010+
  '  FreeAndNil(FList);'#013#010+
  '  inherited Destroy;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TDictionary.AddVariable(const AName, Value: String);'#013#010+
  'Var'#013#010+
  '  I : Integer',';'#013#010+
  'begin'#013#010+
  '  I:=Flist.IndexOf(AName);'#013#010+
  '  If I=-1 then'#013#010+
  '    I:=FList.Add(Aname)'#013#010+
  '  else'#013#010+
  '    Flist.Objects[i].Free;'#013#010+
  '  Flist.Objects[i]:=TValueItem.Create(Value);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TDictionary.AddFunction(const AName: String; FReplacement:',' '+
  'TReplaceFunction);'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  'begin'#013#010+
  '  I:=Flist.IndexOf(AName);'#013#010+
  '  If I=-1 then'#013#010+
  '    I:=Flist.Add(AName)'#013#010+
  '  else'#013#010+
  '    Flist.Objects[i].Free;'#013#010+
  '  Flist.Objects[i]:=TFunctionItem.Create(FReplacement);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'procedure TDictiona','ry.RemoveItem(const AName: String);'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  'begin'#013#010+
  '  I:=Flist.IndexOf(AName);'#013#010+
  '  If (I<>-1) then'#013#010+
  '    begin'#013#010+
  '    FList.Objects[i].Free;'#013#010+
  '    FList.Delete(I);'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TDictionary.GetValue(AName: String): S','tring;'#013#010+
  'begin'#013#010+
  '  Result:=GetValue(AName,'#039#039');'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TDictionary.GetValue(const AName,Args: String): String;'#013#010+
  'Var'#013#010+
  '  O : TObject;'#013#010+
  '  I : Integer;'#013#010+
  'begin'#013#010+
  '  I:=Flist.IndexOf(AName);'#013#010+
  '  If (I=-1) then'#013#010+
  '    Raise EDictionaryError.C','reateFmt(SErrNoDictionaryItem,[AName]);'#013+
  #010+
  '  O:=Flist.Objects[I];'#013#010+
  '  If O is TValueItem then'#013#010+
  '    Result:=TValueItem(O).FValue'#013#010+
  '  else'#013#010+
  '    Result:=TFunctionItem(O).FFunc(AName,Args);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TDictionary.ReplaceStrings(Const ASour','ce: String): String;'#013+
  #010+
  'Var'#013#010+
  '  S,FN,FV : String;'#013#010+
  '  P: Integer;'#013#010+
  'begin'#013#010+
  '  Result:='#039#039';'#013#010+
  '  S:=ASource;'#013#010+
  '  P:=Pos('#039'$('#039',S);'#013#010+
  '  While (P<>0) do'#013#010+
  '    begin'#013#010+
  '      Result:=Result+Copy(S,1,P-1);'#013#010+
  '      Delete(S,1,P+1);'#013#010+
  '      P:=Pos('#039')'#039',S);'#013#010+
  '      FN',':=Copy(S,1,P-1);'#013#010+
  '      Delete(S,1,P);'#013#010+
  '      P:=Pos('#039' '#039',FN);'#013#010+
  '      If (P<>0) then // function arguments ?'#013#010+
  '        begin'#013#010+
  '        FV:=FN;'#013#010+
  '        FN:=Copy(FN,1,P);'#013#010+
  '        System.Delete(FV,1,P);'#013#010+
  '        end'#013#010+
  '      else'#013#010+
  '        FV:='#039#039';',#013#010+
  '      Result:=Result+GetValue(FN,FV);'#013#010+
  '      P:=Pos('#039'$('#039',S);'#013#010+
  '    end;'#013#010+
  '  Result:=Result+S;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function TDictionary.Substitute(Const Source : String; Macros : Array '+
  'of string) : String;'#013#010+
  'Var'#013#010+
  '  I : Integer;'#013#010+
  'begin'#013#010+
  '  I:=0;'#013#010+
  '  Whil','e I<High(Macros) do'#013#010+
  '    begin'#013#010+
  '      AddVariable(Macros[i],Macros[I+1]);'#013#010+
  '      Inc(I,2);'#013#010+
  '    end;'#013#010+
  '  Result:=ReplaceStrings(Source);'#013#010+
  '  While I<High(Macros) do'#013#010+
  '    begin'#013#010+
  '      RemoveItem(Macros[i]);'#013#010+
  '      Inc(I,2);'#013#010+
  '    end;'#013#010+
  'end;'#013#010+
  #013#010,
  '{*********************************************************************'+
  '*******'#013#010+
  '                                 Default Instances'#013#010+
  '**********************************************************************'+
  '******}'#013#010+
  #013#010+
  'var'#013#010+
  '  DefInstaller : TCusto','mInstaller;'#013#010+
  #013#010+
  'Function Installer(InstallerClass: TInstallerClass): TCustomInstaller;'+
  #013#010+
  'begin'#013#010+
  '  If Not Assigned(DefInstaller) then'#013#010+
  '    DefInstaller:=InstallerClass.Create(Nil);'#013#010+
  '  Result:=DefInstaller;'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'Function Installer: TCusto','mInstaller;'#013#010+
  'begin'#013#010+
  '  Result := Installer(TFPCInstaller);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  #013#010+
  '{ TCommand }'#013#010+
  #013#010+
  'function TCommand.GetOptions: TStrings;'#013#010+
  'begin'#013#010+
  '  If (FOptions=Nil) then'#013#010+
  '    FOptions:=TStringList.Create;'#013#010+
  '  Result:=FOptions;'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TComm','and.SetOptions(const Value: TStrings);'#013#010+
  'begin'#013#010+
  '  If (Value=Nil) or (Value.Count=0) then'#013#010+
  '    FreeAndNil(FOptions)'#013#010+
  '  else'#013#010+
  '    Options.Assign(Value);'#013#010+
  'end;'#013#010+
  #013#010+
  'destructor TCommand.Destroy;'#013#010+
  'begin'#013#010+
  '  FreeAndNil(FOptions);'#013#010+
  '  inherited Destroy',';'#013#010+
  'end;'#013#010+
  #013#010+
  'function TCommand.HaveOptions: Boolean;'#013#010+
  'begin'#013#010+
  '  Result:=(FOptions<>Nil);'#013#010+
  'end;'#013#010+
  #013#010+
  #013#010+
  'function TCommand.CmdLineOptions: String;'#013#010+
  'begin'#013#010+
  '  If HaveOptions then'#013#010+
  '    Result:=OptionListToString(Options);'#013#010+
  'end;'#013#010+
  #013#010+
  'procedure TCommand.P','arseOptions(S: String);'#013#010+
  #013#010+
  'begin'#013#010+
  '  Options:=OptionsToStringList(S);'#013#010+
  'end;'#013#010+
  #013#010+
  'Initialization'#013#010+
  '  OnGetApplicationName:=@GetFPMakeName;'#013#010+
  '  CustomFpmakeCommandlineOptions:=nil;'#013#010+
  '  CustomFpMakeCommandlineValues:=nil;'#013#010+
  #013#010+
  'Finalization'#013#010+
  '  FreeAndN','il(CustomFpMakeCommandlineValues);'#013#010+
  '  FreeAndNil(CustomFpmakeCommandlineOptions);'#013#010+
  '  FreeAndNil(DefInstaller);'#013#010+
  '  FreeAndNil(GlobalDictionary);'#013#010+
  '  FreeAndNil(Defaults);'#013#010+
  'end.'#013#010
);
