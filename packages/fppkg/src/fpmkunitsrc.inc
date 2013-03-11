{$ifdef Delphi}
const fpmkunitsrc : array[0..970] of string[240]=(
{$else Delphi}
const fpmkunitsrc : array[0..970,1..240] of char=(
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
  'unit fpmkunit;'#010+
  #010+
  '{$Mode objfpc}'#010+
  '{$H+}'#010+
  '{$inline on}'#010+
  #010+
  '{ For target or cpu dependent dependencies also add an overload where '+
  'you'#010+
  '  can pass only a set of cpus. This is disabled for now because it cr',
  'eates'#010+
  '  an error in the compiler with overload choosing }'#010+
  '{ define cpu_only_overloads}'#010+
  #010+
  'Interface'#010+
  #010+
  '{$IFDEF OS2}'#010+
  ' {$DEFINE NO_UNIT_PROCESS}'#010+
  '{$ENDIF OS2}'#010+
  #010+
  '{$IFDEF GO32V2}'#010+
  ' {$DEFINE NO_UNIT_PROCESS}'#010+
  '{$ENDIF GO32V2}'#010+
  #010+
  '{$IFDEF NETBSD}'#010+
  ' { NetBSD p','threads are not yet working, try to use fpmake without th'+
  'reads }'#010+
  '  {$DEFINE NO_THREADING}'#010+
  '{$ENDIF NETBSD}'#010+
  #010+
  '{$ifndef NO_UNIT_PROCESS}'#010+
  '  {$define HAS_UNIT_PROCESS}'#010+
  '{$endif NO_UNIT_PROCESS}'#010+
  #010+
  '{$ifndef NO_UNIT_ZIPPER}'#010+
  '  {$define HAS_UNIT_ZIPPER}',#010+
  '{$endif NO_UNIT_ZIPPER}'#010+
  #010+
  'uses'#010+
  '{$ifdef UNIX}'#010+
  '  BaseUnix,'#010+
  '{$endif UNIX}'#010+
  '{$ifndef NO_THREADING}'#010+
  '{$ifdef UNIX}'#010+
  '  cthreads,'#010+
  '{$endif UNIX}'#010+
  '{$endif NO_THREADING}'#010+
  '  SysUtils, Classes, StrUtils'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '  ,process'#010+
  '{$endif HAS_UNIT_','PROCESS}'#010+
  '{$ifdef HAS_UNIT_ZIPPER}'#010+
  '  ,zipper'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  '  ;'#010+
  #010+
  'Type'#010+
  '  TFileType = (ftSource,ftUnit,ftObject,ftResource,ftExecutable,ftStat'+
  'icLibrary,'#010+
  '               ftSharedLibrary);'#010+
  '  TFileTypes = set of TFileType;'#010+
  #010+
  '  // Please k','eep this order, see OSCPUSupported below'#010+
  '  TCpu=(cpuNone,'#010+
  '    i386,m68k,powerpc,sparc,x86_64,arm,powerpc64,avr,armeb,'#010+
  '    mips,mipsel,jvm'#010+
  '  );'#010+
  '  TCPUS = Set of TCPU;'#010+
  #010+
  '  // Please keep this order, see OSCPUSupported below'#010+
  '  TOS=(osNone,'#010+
  '    ','linux,go32v2,win32,os2,freebsd,beos,netbsd,'#010+
  '    amiga,atari, solaris, qnx, netware, openbsd,wdosx,'#010+
  '    palmos,macos,darwin,emx,watcom,morphos,netwlibc,'#010+
  '    win64,wince,gba,nds,embedded,symbian,haiku,iphonesim,'#010+
  '    aix,java,android,nativent'#010,
  '  );'#010+
  '  TOSes = Set of TOS;'#010+
  #010+
  '  TCompilerMode = (cmFPC,cmTP,cmObjFPC,cmDelphi,cmMacPas);'#010+
  '  TCompilerModes = Set of TCompilerMode;'#010+
  #010+
  '  TTargetType = (ttProgram,ttUnit,ttImplicitUnit,ttCleanOnlyUnit,ttExa'+
  'mpleUnit,ttExampleProgram,ttFPDoc);'#010+
  '  TTa','rgetTypes = set of TTargetType;'#010+
  #010+
  '  TFPDocFormat = (ffHtml, ffHtm, ffXHtml, ffLaTex, ffXMLStruct, ffChm)'+
  ';'#010+
  '  TFPDocFormats = set of TFPDocFormat;'#010+
  #010+
  '  TTargetState = (tsNeutral,tsConsidering,tsNoCompile,tsCompiled,tsIns'+
  'talled,tsNotFound);'#010+
  '  TTa','rgetStates = Set of TTargetState;'#010+
  #010+
  '  TSourceType = (stDoc,stSrc,stExample,stTest);'#010+
  '  TSourceTypes = set of TSourceType;'#010+
  #010+
  '  TVerboseLevel = (vlError,vlWarning,vlInfo,vldebug,vlCommand);'#010+
  '  TVerboseLevels = Set of TVerboseLevel;'#010+
  #010+
  '  TCommandAt ','= (caBeforeCompile,caAfterCompile,'#010+
  '                caBeforeInstall,caAfterInstall,'#010+
  '                caBeforeArchive,caAfterArchive,'#010+
  '                caBeforeClean,caAfterClean,'#010+
  '                caBeforeDownload,caAfterDownload);'#010+
  #010+
  '  TDependency','Type = (depPackage,depImplicitPackage,depUnit,depInclud'+
  'e);'#010+
  '  TDependencyTypes = set of TDependencyType;'#010+
  #010+
  '  TLogEvent = Procedure (Level : TVerboseLevel; Const Msg : String) of'+
  ' Object;'#010+
  '  TNotifyProcEvent = procedure(Sender: TObject);'#010+
  #010+
  '  TRun','Mode = (rmCompile,rmBuild,rmInstall,rmArchive,rmClean,rmDistCl'+
  'ean,rmManifest,rmZipInstall);'#010+
  #010+
  '  TBuildMode = (bmOneByOne, bmBuildUnit{, bmSkipImplicitUnits});'#010+
  '  TBuildModes = set of TBuildMode;'#010+
  '  TProcessPackageResult = (ppHandled, ppDelayed',');'#010+
  '  TCheckDependencyResult = (cdAvailable, cdNotAvailable, cdNotYetAvail'+
  'able);'#010+
  #010+
  'Const'#010+
  '  // Aliases'#010+
  '  Amd64   = X86_64;'#010+
  '  PPC = PowerPC;'#010+
  '  PPC64 = PowerPC64;'#010+
  '  DOS = Go32v2;'#010+
  '  MacOSX = Darwin;'#010+
  #010+
  '  AllOSes = [Low(TOS)..High(TOS)];'#010+
  '  AllCPUs =',' [Low(TCPU)..High(TCPU)];'#010+
  '  AllUnixOSes  = [Linux,FreeBSD,NetBSD,OpenBSD,Darwin,QNX,BeOS,Solaris'+
  ',Haiku,iphonesim,aix,Android];'#010+
  '  AllBSDOSes      = [FreeBSD,NetBSD,OpenBSD,Darwin,iphonesim];'#010+
  '  AllWindowsOSes  = [Win32,Win64,WinCE];'#010+
  '  AllLimi','t83fsOses= [go32v2,os2,emx,watcom];'#010+
  #010+
  '  AllSmartLinkLibraryOSes = [Linux]; // OSes that use .a library files'+
  ' for smart-linking'#010+
  '  AllImportLibraryOSes = AllWindowsOSes + [os2,emx,netwlibc,netware,wa'+
  'tcom,go32v2,macos,nativent];'#010+
  #010+
  '  { This table',' is kept OS,Cpu because it is easier to maintain (PFV)'+
  ' }'#010+
  '  OSCPUSupported : array[TOS,TCpu] of boolean = ('#010+
  '    { os          none   i386    m68k  ppc    sparc  x86_64 arm    ppc'+
  '64  avr    armeb  mips   mipsel jvm}'#010+
  '    { none }    ( false, f','alse, false, false, false, false, false, f'+
  'alse, false, false, false, false, false),'#010+
  '    { linux }   ( false, true,  true,  true,  true,  true,  true,  tru'+
  'e,  false, true , true , true , false),'#010+
  '    { go32v2 }  ( false, true,  false, false, ','false, false, false, f'+
  'alse, false, false, false, false, false),'#010+
  '    { win32 }   ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false),'#010+
  '    { os2 }     ( false, true,  false, false, false, false, false,',' f'+
  'alse, false, false, false, false, false),'#010+
  '    { freebsd } ( false, true,  true,  false, false, true,  false, fal'+
  'se, false, false, false, false, false),'#010+
  '    { beos }    ( false, true,  false, false, false, false, false, fal'+
  'se, false, false',', false, false, false),'#010+
  '    { netbsd }  ( false, true,  true,  true,  true,  false, false, fal'+
  'se, false, false, false, false, false),'#010+
  '    { amiga }   ( false, false, true,  true,  false, false, false, fal'+
  'se, false, false, false, false, fals','e),'#010+
  '    { atari }   ( false, false, true,  false, false, false, false, fal'+
  'se, false, false, false, false, false),'#010+
  '    { solaris } ( false, true,  false, false, true,  false, false, fal'+
  'se, false, false, false, false, false),'#010+
  '    { qnx }     ','( false, true,  false, false, false, false, false, f'+
  'alse, false, false, false, false, false),'#010+
  '    { netware } ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false),'#010+
  '    { openbsd } ( false, true,  true',',  false, false, false, false, f'+
  'alse, false, false, false, false, false),'#010+
  '    { wdosx }   ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false),'#010+
  '    { palmos }  ( false, false, true,  false, false, fal','se, true,  f'+
  'alse, false, false, false, false, false),'#010+
  '    { macos }   ( false, false, false, true,  false, false, false, fal'+
  'se, false, false, false, false, false),'#010+
  '    { darwin }  ( false, true,  false, true,  false, true,  true,  tru'+
  'e,  fa','lse, false, false, false, false),'#010+
  '    { emx }     ( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false),'#010+
  '    { watcom }  ( false, true,  false, false, false ,false, false, fal'+
  'se, false, false, false, f','alse, false),'#010+
  '    { morphos } ( false, false, false, true,  false ,false, false, fal'+
  'se, false, false, false, false, false),'#010+
  '    { netwlibc }( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false),'#010+
  '    { ','win64   } ( false, false, false, false, false, true,  false, f'+
  'alse, false, false, false, false, false),'#010+
  '    { wince    }( false, true,  false, false, false, false, true,  fal'+
  'se, false, false, false, false, false),'#010+
  '    { gba    }  ( false, f','alse, false, false, false, false, true,  f'+
  'alse, false, false, false, false, false),'#010+
  '    { nds    }  ( false, false, false, false, false, false, true,  fal'+
  'se, false, false, false, false, false),'#010+
  '    { embedded }( false, true,  true,  true,  ','true,  true,  true,  t'+
  'rue,  true,  true , false, false, false),'#010+
  '    { symbian } ( false, true,  false, false, false, false, true,  fal'+
  'se, false, false, false, false, false),'#010+
  '    { haiku }   ( false, true,  false, false, false, false, false,',' f'+
  'alse, false, false, false, false, false),'#010+
  '    { iphonesim}( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false),'#010+
  '    { aix    }  ( false, false, false, true,  false, false, false, tru'+
  'e,  false, false',', false, false, false),'#010+
  '    { java }    ( false, false, false, false, false, false, false, fal'+
  'se, false, false, false, false, true ),'#010+
  '    { android } ( false, true,  false, false, false, false, true,  fal'+
  'se, false, false, false, false, true',' ),'#010+
  '    { nativent }( false, true,  false, false, false, false, false, fal'+
  'se, false, false, false, false, false)'#010+
  '  );'#010+
  #010+
  '  // Useful'#010+
  '  UnitExt = '#039'.ppu'#039';'#010+
  '  PPUExt  = UnitExt;'#010+
  '  PasExt  = '#039'.pas'#039';'#010+
  '  PPExt   = '#039'.pp'#039';'#010+
  '  IncExt  = '#039'.inc'#039';'#010+
  '  ObjExt ',' = '#039'.o'#039';'#010+
  '  RstExt  = '#039'.rst'#039';'#010+
  '  LibExt  = '#039'.a'#039';'#010+
  '  SharedLibExt = '#039'.so'#039';'#010+
  '  DLLExt  = '#039'.dll'#039';'#010+
  '  ExeExt  = '#039'.exe'#039';'#010+
  '  DbgExt  = '#039'.dbg'#039';'#010+
  '  ZipExt  = '#039'.zip'#039';'#010+
  #010+
  '  FPMakePPFile = '#039'fpmake.pp'#039';'#010+
  '  ManifestFile = '#039'manifest.xml'#039';'#010+
  '  UnitConfigFile = '#039'fpuni','ts.cfg'#039';'#010+
  #010+
  '  DirNotFound = '#039'<dirnotfound>'#039';'#010+
  #010+
  '  UnitTargets = [ttUnit,ttImplicitUnit,ttCleanOnlyUnit,ttExampleUnit];'+
  #010+
  '  ProgramTargets = [ttProgram,ttExampleProgram];'#010+
  #010+
  '  DefaultMessages = [vlError,vlWarning,vlCommand];'#010+
  '  AllMessages = [vlError',',vlWarning,vlCommand,vlInfo];'#010+
  #010+
  'Type'#010+
  '  TTargets = Class;'#010+
  '  { TNamedItem }'#010+
  #010+
  '  TNamedItem = Class(TCollectionItem)'#010+
  '  private'#010+
  '    FName: String;'#010+
  '    procedure SetName(const AValue: String);virtual;'#010+
  '  Public'#010+
  '    property Name : String Read FName',' Write SetName;'#010+
  '  end;'#010+
  #010+
  '  { TNamedCollection }'#010+
  #010+
  '  TNamedCollection = Class(TCollection)'#010+
  '  private'#010+
  '    FUniqueNames: Boolean;'#010+
  '  private'#010+
  '    function GetItem(Index: Integer): TNamedItem;'#010+
  '    procedure SetItem(Index: Integer; AValue: TNamedIte','m);'#010+
  '  Public'#010+
  '    Function IndexOfName(const AName : String) : Integer;'#010+
  '    Function ItemByName(const AName : String) : TNamedItem;'#010+
  '    Property UniqueNames : Boolean Read FUniqueNames;'#010+
  '    property Items[Index: Integer]: TNamedItem read Get','Item write Se'+
  'tItem;'#010+
  '  end;'#010+
  #010+
  '  { TNamedItemList }'#010+
  #010+
  '  TNamedItemList = Class(TFPList)'#010+
  '  private'#010+
  '    function GetNamedItem(Index : Integer): TNamedItem;'#010+
  '    procedure SetNamedItem(Index : Integer; const AValue: TNamedItem);'+
  #010+
  '  public'#010+
  '    Functi','on IndexOfName(const AName : String) : Integer;'#010+
  '    Function ItemByName(const ANAme : String) : TNamedItem;'#010+
  '    Property NamedItems[Index : Integer] : TNamedItem Read GetNamedIte'+
  'm Write SetNamedItem; default;'#010+
  '  end;'#010+
  #010+
  '  { TCommand }'#010+
  '  TComma','nd = Class(TNamedItem)'#010+
  '  private'#010+
  '    FAfterCommand: TNotifyEvent;'#010+
  '    FBeforeCommand: TNotifyEvent;'#010+
  '    FCommand: String;'#010+
  '    FCommandAt: TCommandAt;'#010+
  '    FDestFile: String;'#010+
  '    FIgnoreResult: Boolean;'#010+
  '    FOptions: TStrings;'#010+
  '    FSourceFile',': String;'#010+
  '    Function GetOptions : TStrings;'#010+
  '    Procedure SetOptions(Const Value : TStrings);'#010+
  '  Public'#010+
  '    Destructor Destroy; override;'#010+
  '    Function HaveOptions : Boolean;'#010+
  '    Function CmdLineOptions : String;'#010+
  '    Procedure ParseOptions(','S : String);'#010+
  '    Property SourceFile : String Read FSourceFile Write FSourceFile;'#010+
  '    Property DestFile : String Read FDestFile Write FDestFile;'#010+
  '    Property Command : String Read FCommand Write FCommand;'#010+
  '    Property Options : TStrings Rea','d GetOptions Write SetOptions;'#010+
  '    Property At : TCommandAt Read FCommandAt Write FCommandAt;'#010+
  '    Property IgnoreResult : Boolean Read FIgnoreResult Write FIgnoreRe'+
  'sult;'#010+
  '    Property BeforeCommand : TNotifyEvent Read FBeforeCommand Write FB',
  'eforeCommand;'#010+
  '    Property AfterCommand : TNotifyEvent Read FAfterCommand Write FAft'+
  'erCommand;'#010+
  '  end;'#010+
  #010+
  '  { TCommands }'#010+
  #010+
  '  TCommands = Class(TNamedCollection)'#010+
  '  private'#010+
  '    FDefaultAt: TCommandAt;'#010+
  '    function GetCommand(const Dest : String)',': TCommand;'#010+
  '    function GetCommandItem(Index : Integer): TCommand;'#010+
  '    procedure SetCommandItem(Index : Integer; const AValue: TCommand);'+
  #010+
  '  Public'#010+
  '    Function AddCommand(Const Cmd : String) : TCommand;'#010+
  '    Function AddCommand(Const Cmd,Op','tions : String) : TCommand;'#010+
  '    Function AddCommand(Const Cmd,Options,Dest,Source : String) : TCom'+
  'mand;'#010+
  '    Function AddCommand(At : TCommandAt; Const Cmd : String) : TComman'+
  'd;'#010+
  '    Function AddCommand(At : TCommandAt; Const Cmd,Options : St','ring)'+
  ' : TCommand;'#010+
  '    Function AddCommand(At : TCommandAt; Const Cmd,Options, Dest,Sourc'+
  'e : String) : TCommand;'#010+
  '    Property CommandItems[Index : Integer] : TCommand Read GetCommandI'+
  'tem Write SetCommandItem;'#010+
  '    Property Commands[Dest : Str','ing] : TCommand Read GetCommand; def'+
  'ault;'#010+
  '    Property DefaultAt : TCommandAt Read FDefaultAt Write FDefaultAt;'#010+
  '  end;'#010+
  #010+
  '  { TFPVersion }'#010+
  #010+
  '  TFPVersion = Class(TPersistent)'#010+
  '  private'#010+
  '    FMajor,'#010+
  '    FMinor,'#010+
  '    FMicro,'#010+
  '    FBuild    : Intege','r;'#010+
  '    function GetAsString: String;'#010+
  '    function GetEmpty: Boolean;'#010+
  '    procedure SetAsString(const AValue: String);'#010+
  '  Public'#010+
  '   Procedure Clear;'#010+
  '   Procedure Assign(Source : TPersistent); override;'#010+
  '   Function CompareVersion(AVersion : TF','PVersion) : Integer;'#010+
  '   Function SameVersion(AVersion : TFPVersion) : Boolean;'#010+
  '   Property AsString : String Read GetAsString Write SetAsString;'#010+
  '   Property Empty : Boolean Read GetEmpty;'#010+
  '  Published'#010+
  '   Property Major : Integer Read FMajor ','Write FMajor;'#010+
  '   Property Minor : Integer Read FMinor Write FMinor;'#010+
  '   Property Micro : Integer Read FMicro Write FMicro;'#010+
  '   Property Build : Integer Read FBuild Write FBuild;'#010+
  '  end;'#010+
  #010+
  '  { TConditionalString }'#010+
  '  TConditionalString = Class'#010+
  '  ','private'#010+
  '    FOSes   : TOSes;'#010+
  '    FCPUs   : TCPUs;'#010+
  '    FValue  : String;'#010+
  '  Public'#010+
  '    Constructor Create;virtual;'#010+
  '    Property Value : String Read FValue Write FValue;'#010+
  '    Property OSes  : TOSes Read FOSes Write FOSes;'#010+
  '    Property CPUs : TC','PUs Read FCPUS Write FCPUs;'#010+
  '  end;'#010+
  #010+
  '  TConditionalStringClass = class of TConditionalString;'#010+
  #010+
  '  { TConditionalStrings }'#010+
  #010+
  '  TConditionalStrings = Class(TFPList)'#010+
  '  private'#010+
  '    FCSClass : TConditionalStringClass;'#010+
  '    function GetConditionalStr','ing(Index : Integer): TConditionalStri'+
  'ng;'#010+
  '    procedure SetConditionalString(Index : Integer; const AValue: TCon'+
  'ditionalString);'#010+
  '  Public'#010+
  '    Constructor Create(AClass:TConditionalStringClass);'#010+
  '    Function Add(Const Value : String) : TCond','itionalString;inline;'#010+
  '    Function Add(Const Value : String;const OSes:TOSes) : TConditional'+
  'String;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs) : TConditional'+
  'String;inline;'#010+
  '{$endif cpu_only_ove','rloads}'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs;const OSes:TOSe'+
  's) : TConditionalString;'#010+
  '    Property ConditionalStrings[Index : Integer] : TConditionalString '+
  'Read GetConditionalString Write SetConditionalString; default;'#010+
  '  en','d;'#010+
  #010+
  '  { TConditionalDestString }'#010+
  #010+
  '  TConditionalDestString = Class(TConditionalString)'#010+
  '  private'#010+
  '    FDestPath: string;'#010+
  '  public'#010+
  '    property DestPath: string read FDestPath write FDestPath;'#010+
  '  end;'#010+
  #010+
  '  TConditionalDestStringClass = class of ','TConditionalDestString;'#010+
  #010+
  '  { TConditionalDestStrings }'#010+
  #010+
  '  TConditionalDestStrings = Class(TConditionalStrings)'#010+
  '  private'#010+
  '    function GetConditionalString(Index : Integer): TConditionalDestSt'+
  'ring;'#010+
  '    procedure SetConditionalString(Index : ','Integer; AValue: TConditi'+
  'onalDestString);'#010+
  '  public'#010+
  '    Constructor Create(AClass:TConditionalDestStringClass);'#010+
  '    Function Add(Const Value : String; ADestPath: String) : TCondition'+
  'alDestString;inline;'#010+
  '    Function Add(Const Value : String;','const OSes:TOSes; ADestPath: S'+
  'tring) : TConditionalDestString;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs; ADestPath: Str'+
  'ing) : TConditionalDestString;inline;'#010+
  '{$endif cpu_only_overloads}'#010+
  '    Fu','nction Add(Const Value : String;const CPUs:TCPUs;const OSes:TO'+
  'Ses; ADestPath: String) : TConditionalDestString;'#010+
  '    Property ConditionalStrings[Index : Integer] : TConditionalDestStr'+
  'ing Read GetConditionalString Write SetConditionalString; ','default;'#010+
  '  end;'#010+
  #010+
  '  { TDictionary }'#010+
  #010+
  '  TReplaceFunction = Function (Const AName,Args : String) : String of '+
  'Object;'#010+
  #010+
  '  TDictionary = Class(TComponent)'#010+
  '  private'#010+
  '    FList : TStringList;'#010+
  '  Public'#010+
  '    Constructor Create(AOwner : TComponent); ov','erride;'#010+
  '    Destructor Destroy;override;'#010+
  '    Procedure AddVariable(const AName,Value : String);'#010+
  '    Procedure AddFunction(const AName : String; FReplacement : TReplac'+
  'eFunction);'#010+
  '    Procedure RemoveItem(const AName : String);'#010+
  '    Function G','etValue(AName : String) : String;'#010+
  '    Function GetValue(const AName,Args : String) : String; virtual;'#010+
  '    Function ReplaceStrings(Const ASource : String) : String; virtual;'+
  #010+
  '    Function Substitute(Const Source : String; Macros : Array of st','r'+
  'ing) : String; virtual;'#010+
  '  end;'#010+
  #010+
  '  { TPackageDictionary }'#010+
  #010+
  '  TPackageDictionary = Class(TDictionary)'#010+
  '  private'#010+
  '    FMasterDictionary: TDictionary;'#010+
  '  Public'#010+
  '    Function GetValue(const AName,Args : String) : String; override;'#010+
  '    property Ma','sterDictionary: TDictionary read FMasterDictionary wr'+
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
  '  ','  FVersion : TFPVersion;'#010+
  '    FRequireChecksum : Cardinal;'#010+
  '    // Filenames, Includes'#010+
  '    FTargetFileName : String;'#010+
  '    Function GetVersion : string;'#010+
  '    Procedure SetVersion(const V : string);'#010+
  '  Public'#010+
  '    Constructor Create;override;'#010+
  '    D','estructor Destroy;override;'#010+
  '    Property Target : TObject Read FTarget Write FTarget;'#010+
  '    Property DependencyType : TDependencyType Read FDependencyType;'#010+
  '    Property TargetFileName : String Read FTargetFileName Write FTarge'+
  'tFileName;'#010+
  '    P','roperty Version : String Read GetVersion Write SetVersion;'#010+
  '    Property RequireChecksum : Cardinal Read FRequireChecksum Write FR'+
  'equireChecksum;'#010+
  '  end;'#010+
  #010+
  '  { TPackageVariant }'#010+
  #010+
  '  TPackage = Class;'#010+
  '  TPackageVariant = class(TNamedItem)'#010+
  '  priv','ate'#010+
  '    FOptions: TStrings;'#010+
  '    FTargets: TTargets;'#010+
  '  public'#010+
  '    constructor Create(ACollection: TCollection); override;'#010+
  '    destructor Destroy; override;'#010+
  '    property Options: TStrings read FOptions;'#010+
  '    property Targets: TTargets read FTa','rgets;'#010+
  '  end;'#010+
  #010+
  '  { TPackageVariants }'#010+
  #010+
  '  TPackageVariants = class(TNamedCollection)'#010+
  '  private'#010+
  '    FActivePackageVariantName: string;'#010+
  '    FDefaultPackageVariantName: string;'#010+
  '    FIsInheritable: boolean;'#010+
  '    FMasterPackage: TPackage;'#010+
  '    FNam','e: string;'#010+
  '    function GetActivePackageVariant: TPackageVariant;'#010+
  '    function GetDefaultPackageVariant: TPackageVariant;'#010+
  '    procedure SetActivePackageVariantName(AValue: string);'#010+
  '    procedure SetDefaultPackageVariantName(AValue: string);',#010+
  '  public'#010+
  '    function Add(AName: String): TPackageVariant; overload; virtual;'#010+
  '    property Name: string read FName write FName;'#010+
  '    property MasterPackage: TPackage read FMasterPackage;'#010+
  '    property DefaultPackageVariant: TPackageVariant r','ead GetDefaultP'+
  'ackageVariant;'#010+
  '    property ActivePackageVariant: TPackageVariant read GetActivePacka'+
  'geVariant;'#010+
  '    property DefaultPackageVariantName: string read FDefaultPackageVar'+
  'iantName write SetDefaultPackageVariantName;'#010+
  '    property A','ctivePackageVariantName: string read FActivePackageVar'+
  'iantName write SetActivePackageVariantName;'#010+
  '    property IsInheritable: boolean read FIsInheritable;'#010+
  '  end;'#010+
  #010+
  #010+
  '  TDependencies = Class(TConditionalStrings)'#010+
  '    function GetDependency(Inde','x : Integer): TDependency;'#010+
  '    procedure SetDependency(Index : Integer; const AValue: TDependency'+
  ');'#010+
  '  Public'#010+
  '    Function Add(Const Value : String) : TDependency;inline;'#010+
  '    Function Add(Const Value : String;const OSes:TOSes) : TDependency;',
  'inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs) : TDependency;'+
  'inline;'#010+
  '{$endif cpu_only_overloads}'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs;const OSes:TOSe'+
  's) : TDependency;'#010+
  '    Functi','on AddUnit(Const Value : String) : TDependency;inline;'#010+
  '    Function AddUnit(Const Value : String;const OSes:TOSes) : TDepende'+
  'ncy;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function AddUnit(Const Value : String;const CPUs:TCPUs) : TDepende'+
  'ncy;i','nline;'#010+
  '{$endif cpu_only_overloads}'#010+
  '    Function AddUnit(Const Value : String;const CPUs:TCPUs;const OSes:'+
  'TOSes) : TDependency;'#010+
  '    Function AddInclude(Const Value : String) : TDependency;inline;'#010+
  '    Function AddInclude(Const Value : String;','const OSes:TOSes) : TDe'+
  'pendency;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function AddInclude(Const Value : String;const CPUs:TCPUs) : TDepe'+
  'ndency;inline;'#010+
  '{$endif cpu_only_overloads}'#010+
  '    Function AddInclude(Const Value : String;const CPUs:TCP','Us;const '+
  'OSes:TOSes) : TDependency;'#010+
  '    Property Dependencies[Index : Integer] : TDependency Read GetDepen'+
  'dency Write SetDependency; default;'#010+
  '  end;'#010+
  #010+
  '  { TTarget }'#010+
  #010+
  '  TTarget = Class(TNamedItem)'#010+
  '  private'#010+
  '    FInstall : Boolean;'#010+
  '    FAfterC','lean: TNotifyEvent;'#010+
  '    FAfterCompile: TNotifyEvent;'#010+
  '    FBeforeClean: TNotifyEvent;'#010+
  '    FBeforeCompile: TNotifyEvent;'#010+
  '    FCPUs: TCPUs;'#010+
  '    FOSes: TOSes;'#010+
  '    FMode: TCompilerMode;'#010+
  '    FResourceStrings: Boolean;'#010+
  '    FObjectPath,'#010+
  '    FUnitPa','th,'#010+
  '    FIncludePath : TConditionalStrings;'#010+
  '    FDependencies : TDependencies;'#010+
  '    FCommands : TCommands;'#010+
  '    FDirectory: String;'#010+
  '    FExtension: String;'#010+
  '    FTargetSourceFileName : String;'#010+
  '    FFileType: TFileType;'#010+
  '    FOptions: TStrings;'#010,
  '    FFPCTarget: String;'#010+
  '    FTargetState: TTargetState;'#010+
  '    FTargetType: TTargetType;'#010+
  '    FXML: string;'#010+
  '    function GetOptions: TStrings;'#010+
  '    procedure SetOptions(const AValue: TStrings);'#010+
  '  Protected'#010+
  '    Function GetSourceFileName : String','; virtual;'#010+
  '    Function GetUnitFileName : String; virtual;'#010+
  '    function GetUnitLibFileName: String; virtual;'#010+
  '    Function GetObjectFileName : String; virtual;'#010+
  '    Function GetRSTFileName : String; Virtual;'#010+
  '    function GetImportLibFileName(','AOS : TOS) : String; Virtual;'#010+
  '    Function GetProgramFileName(AOS : TOS) : String; Virtual;'#010+
  '    Function GetProgramDebugFileName(AOS : TOS) : String; Virtual;'#010+
  '  Public'#010+
  '    Constructor Create(ACollection : TCollection); override;'#010+
  '    Destruc','tor Destroy; override;'#010+
  '    procedure AssignTo(Dest: TPersistent); override;'#010+
  '    Function  GetOutputFileName (AOs : TOS) : String; Virtual;'#010+
  '    Function HaveOptions : Boolean;'#010+
  '    procedure SetName(const AValue: String);override;'#010+
  '    procedu','re SetXML(const AValue: string);'#010+
  '    Procedure GetCleanFiles(List : TStrings; const APrefixU, APrefixB '+
  ': String; ACPU:TCPU; AOS : TOS); virtual;'#010+
  '    Procedure GetInstallFiles(List : TStrings; const APrefixU, APrefix'+
  'B: String; ACPU:TCPU; AOS',' : TOS); virtual;'#010+
  '    Procedure GetArchiveFiles(List : TStrings; ACPU:TCPU; AOS : TOS); '+
  'virtual;'#010+
  '    Property Dependencies : TDependencies Read FDependencies;'#010+
  '    Property Commands : TCommands Read FCommands;'#010+
  '    Property State : TTargetSta','te Read FTargetState;'#010+
  '    Property TargetType : TTargetType Read FTargetType Write FTargetTy'+
  'pe;'#010+
  '    Property OSes : TOSes Read FOSes Write FOSes;'#010+
  '    Property CPUs : TCPUs Read FCPUs Write FCPUs;'#010+
  '    Property Mode : TCompilerMode Read FMode',' Write FMode;'#010+
  '    Property Options : TStrings Read GetOptions Write SetOptions;'#010+
  '    Property SourceFileName: String Read GetSourceFileName ;'#010+
  '    Property UnitFileName : String Read GetUnitFileName;'#010+
  '    Property UnitLibFileName : String Read',' GetUnitLibFileName;'#010+
  '    Property ObjectFileName : String Read GetObjectFileName;'#010+
  '    Property RSTFileName : String Read GetRSTFileName;'#010+
  '    Property FPCTarget : String Read FFPCTarget Write FFPCTarget;'#010+
  '    Property Extension : String Read ','FExtension Write FExtension;'#010+
  '    Property FileType : TFileType Read FFileType Write FFileType;'#010+
  '    Property Directory : String Read FDirectory Write FDirectory;'#010+
  '    Property ResourceStrings : Boolean Read FResourceStrings Write FRe'+
  'sourceStr','ings;'#010+
  '    Property Install : Boolean Read FInstall Write FInstall;'#010+
  '    Property TargetSourceFileName: String Read FTargetSourceFileName;'#010+
  '    Property ObjectPath : TConditionalStrings Read FObjectPath;'#010+
  '    Property UnitPath : TConditionalStr','ings Read FUnitPath;'#010+
  '    Property IncludePath : TConditionalStrings Read FIncludePath;'#010+
  '    Property XML: string Read FXML Write SetXML;'#010+
  '    // Events.'#010+
  '    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FB'+
  'eforeCompile;'#010+
  '    P','roperty AfterCompile : TNotifyEvent Read FAfterCompile Write FA'+
  'fterCompile;'#010+
  '    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBefor'+
  'eClean;'#010+
  '    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterCl'+
  'ean;'#010+
  '  end;'#010+
  #010+
  '  {',' TTargets }'#010+
  #010+
  '  TTargets = Class(TNamedCollection)'#010+
  '  private'#010+
  '    function GetTargetItem(Index : Integer): TTarget;'#010+
  '    function GetTarget(const AName : String): TTarget;'#010+
  '    procedure SetTargetItem(Index : Integer; const AValue: TTarget);'#010+
  '  ','Public'#010+
  '    Function AddFPDoc(Const AUnitName, AXMLName : String) : TTarget;in'+
  'line;'#010+
  '    Function AddUnit(Const AUnitName : String) : TTarget;inline;'#010+
  '    Function AddUnit(Const AUnitName : String;const OSes:TOSes) : TTar'+
  'get;inline;'#010+
  '{$ifdef cp','u_only_overloads}'#010+
  '    Function AddUnit(Const AUnitName : String;const CPUs:TCPUs) : TTar'+
  'get;inline;'#010+
  '{$endif cpu_only_overloads}'#010+
  '    Function AddUnit(Const AUnitName : String;const CPUs:TCPUs;const O'+
  'Ses:TOSes) : TTarget;'#010+
  '    Function AddImpl','icitUnit(Const AUnitName : String;InstallUnit:bo'+
  'olean=true) : TTarget;inline;'#010+
  '    Function AddImplicitUnit(Const AUnitName : String;const OSes:TOSes'+
  ';InstallUnit:boolean=true) : TTarget;inline;'#010+
  '    Function AddImplicitUnit(Const AUnitName : ','String;const CPUs:TCP'+
  'Us;InstallUnit:boolean=true) : TTarget;inline;'#010+
  '    Function AddImplicitUnit(Const AUnitName : String;const CPUs:TCPUs'+
  ';const OSes:TOSes;InstallUnit:boolean=true) : TTarget;'#010+
  '    Function AddProgram(Const AProgramName : St','ring) : TTarget;inlin'+
  'e;'#010+
  '    Function AddProgram(Const AProgramName : String;const OSes:TOSes) '+
  ': TTarget;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function AddProgram(Const AProgramName : String;const CPUs:TCPUs) '+
  ': TTarget;inline;'#010+
  '{$endif cpu_','only_overloads}'#010+
  '    Function AddProgram(Const AProgramName : String;const CPUs:TCPUs;c'+
  'onst OSes:TOSes) : TTarget;'#010+
  '    Function AddExampleUnit(Const AUnitName : String) : TTarget;inline'+
  ';'#010+
  '    Function AddExampleUnit(Const AUnitName : String;c','onst OSes:TOSe'+
  's) : TTarget;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010+
  '    Function AddExampleUnit(Const AUnitName : String;const CPUs:TCPUs)'+
  ' : TTarget;inline;'#010+
  '{$endif cpu_only_overloads}'#010+
  '    Function AddExampleUnit(Const AUnitName : String;const C','PUs:TCPU'+
  's;const OSes:TOSes) : TTarget;'#010+
  '    Function AddExampleProgram(Const AProgramName : String) : TTarget;'+
  'inline;'#010+
  '    Function AddExampleProgram(Const AProgramName : String;const OSes:'+
  'TOSes) : TTarget;inline;'#010+
  '{$ifdef cpu_only_overloads}'#010,
  '    Function AddExampleProgram(Const AProgramName : String;const CPUs:'+
  'TCPUs) : TTarget;inline;'#010+
  '{$endif cpu_only_overloads}'#010+
  '    Function AddExampleProgram(Const AProgramName : String;const CPUs:'+
  'TCPUs;const OSes:TOSes) : TTarget;'#010+
  '    Property',' Targets[AName : String] : TTarget Read GetTarget; defau'+
  'lt;'#010+
  '    Property TargetItems[Index : Integer] : TTarget Read GetTargetItem'+
  ' Write SetTargetItem;'#010+
  '  end;'#010+
  #010+
  '  { TSource }'#010+
  #010+
  '  TSource = Class(TNamedItem)'#010+
  '  private'#010+
  '    FSourceType : TSource','Type;'#010+
  '    FInstallSourcePath : string;'#010+
  '    function GetInstallSourcePath: string;'#010+
  '  Public'#010+
  '    Constructor Create(ACollection : TCollection); override;'#010+
  '    Destructor Destroy; override;'#010+
  '    Procedure GetInstallFiles(List : TStrings); virtua','l;'#010+
  '    property SourceType : TSourceType read FSourceType;'#010+
  '    property InstallSourcePath : string read GetInstallSourcePath;'#010+
  '  end;'#010+
  #010+
  '  { TSources }'#010+
  #010+
  '  TSources = Class(TNamedCollection)'#010+
  '  private'#010+
  '    function GetSourceItem(Index : Integer)',': TSource;'#010+
  '    procedure SetSourceItem(Index : Integer; const AValue: TSource);'#010+
  '  public'#010+
  '    Function AddDoc(const AFiles : String) : TSource;'#010+
  '    Function AddDoc(const AFiles : String; AInstallSourcePath : String'+
  ') : TSource;'#010+
  '    Function A','ddSrc(const AFiles : String) : TSource;'#010+
  '    Function AddExample(const AFiles : String) : TSource;'#010+
  '    Function AddExample(const AFiles : String; AInstallSourcePath : St'+
  'ring) : TSource;'#010+
  '    Function AddTest(const AFiles : String) : TSource;'#010,
  '    procedure AddDocFiles(const AFileMask: string; Recursive: boolean '+
  '= False; AInstallSourcePath : String = '#039#039');'#010+
  '    procedure AddSrcFiles(const AFileMask: string; Recursive: boolean '+
  '= False);'#010+
  '    procedure AddExampleFiles(const AFileMask:',' string; Recursive: bo'+
  'olean = False; AInstallSourcePath : String = '#039#039');'#010+
  '    procedure AddTestFiles(const AFileMask: string; Recursive: boolean'+
  ' = False);'#010+
  '    Property SourceItems[Index : Integer] : TSource Read GetSourceItem'+
  ' Write SetSourceI','tem;default;'#010+
  '  end;'#010+
  #010+
  '  { TPackage }'#010+
  #010+
  '  TPackage = Class(TNamedItem)'#010+
  '  private'#010+
  '    FAfterArchive: TNotifyEvent;'#010+
  '    FAfterArchiveProc: TNotifyProcEvent;'#010+
  '    FAfterClean: TNotifyEvent;'#010+
  '    FAfterCleanProc: TNotifyProcEvent;'#010+
  '    FAfterCompile:',' TNotifyEvent;'#010+
  '    FAfterCompileProc: TNotifyProcEvent;'#010+
  '    FAfterInstall: TNotifyEvent;'#010+
  '    FAfterInstallProc: TNotifyProcEvent;'#010+
  '    FAfterManifest: TNotifyEvent;'#010+
  '    FAfterManifestProc: TNotifyProcEvent;'#010+
  '    FBeforeArchive: TNotifyEvent;'#010,
  '    FBeforeArchiveProc: TNotifyProcEvent;'#010+
  '    FBeforeClean: TNotifyEvent;'#010+
  '    FBeforeCleanProc: TNotifyProcEvent;'#010+
  '    FBeforeCompile: TNotifyEvent;'#010+
  '    FBeforeCompileProc: TNotifyProcEvent;'#010+
  '    FBeforeInstall: TNotifyEvent;'#010+
  '    FBeforeInsta','llProc: TNotifyProcEvent;'#010+
  '    FBeforeManifest: TNotifyEvent;'#010+
  '    FBeforeManifestProc: TNotifyProcEvent;'#010+
  '    FBuildMode: TBuildMode;'#010+
  '    FFlags: TStrings;'#010+
  '    FFPDocFormat: TFPDocFormats;'#010+
  '    FIsFPMakeAddIn: boolean;'#010+
  '    FSupportBuildModes: ','TBuildModes;'#010+
  '    FUnitPath,'#010+
  '    FObjectPath,'#010+
  '    FIncludePath,'#010+
  '    FSourcePath,'#010+
  '    FExamplePath,'#010+
  '    FTestPath,'#010+
  '    FCleanFiles   : TConditionalStrings;'#010+
  '    FInstallFiles : TConditionalDestStrings;'#010+
  '    FDependencies : TDependencies;'#010+
  '    FC','PUs: TCPUs;'#010+
  '    FOSes: TOSes;'#010+
  '    FTargetState: TTargetState;'#010+
  '    FTargets: TTargets;'#010+
  '    FSources: TSources;'#010+
  '    FDirectory: String;'#010+
  '    FOptions: TStrings;'#010+
  '    FFileName: String;'#010+
  '    FAuthor: String;'#010+
  '    FLicense: String;'#010+
  '    FHomepageURL',': String;'#010+
  '    FDownloadURL: String;'#010+
  '    FVersion: TFPVersion;'#010+
  '    FEmail : String;'#010+
  '    FNeedLibC : Boolean;'#010+
  '    FCommands : TCommands;'#010+
  '    FDescriptionFile : String;'#010+
  '    FDescription : String;'#010+
  '    FInstalledChecksum : Cardinal;'#010+
  '    FUnitsOu','tputDir: String;'#010+
  '    FPackageUnitInstallDir: String;'#010+
  '    // Cached directory of installed packages'#010+
  '    FUnitDir : String;'#010+
  '    // Used by buildunits'#010+
  '    FBUTargets: TTargets;'#010+
  '    FBUTarget: TTarget;'#010+
  '    // Used to identify if package is bein','g processed by a thread'#010+
  '    FProcessing : boolean;'#010+
  '    // Dictionary'#010+
  '    FDictionary : TDictionary;'#010+
  '    // Is set when all sourcefiles are found'#010+
  '    FAllFilesResolved: boolean;'#010+
  '    FPackageVariants: TFPList;'#010+
  '    Function GetDescription : st','ring;'#010+
  '    function GetDictionary: TDictionary;'#010+
  '    Function GetFileName : string;'#010+
  '    function GetOptions: TStrings;'#010+
  '    Function GetVersion : string;'#010+
  '    procedure SetOptions(const AValue: TStrings);'#010+
  '    Procedure SetVersion(const V : stri','ng);'#010+
  '  Protected'#010+
  '    procedure SetName(const AValue: String);override;'#010+
  '    procedure SaveUnitConfigToStringList(Const AStringList: TStrings;A'+
  'CPU:TCPU;AOS:TOS); virtual;'#010+
  '    property Dictionary: TDictionary read GetDictionary;'#010+
  '  Public'#010+
  '    c','onstructor Create(ACollection: TCollection); override;'#010+
  '    destructor destroy; override;'#010+
  '    Function HaveOptions : Boolean;'#010+
  '    Function  GetUnitsOutputDir(ACPU:TCPU; AOS : TOS):String;'#010+
  '    Function  GetUnitConfigOutputDir(ACPU:TCPU; AOS :',' TOS):String;'#010+
  '    Procedure InheritPackageVariantsFromDependency(ADependencyPackage:'+
  ' TPackage);'#010+
  '    Function  GetPackageVariantsByName(AName: string): TPackageVariant'+
  's;'#010+
  '    Procedure SetUnitsOutputDir(AValue: string);'#010+
  '    Function  GetPacka','geUnitInstallDir(ACPU:TCPU; AOS : TOS):String;'+
  #010+
  '    Procedure SetPackageUnitInstallDir(AValue: string);'#010+
  '    Function  GetBinOutputDir(ACPU:TCPU; AOS : TOS) : String;'#010+
  '    Procedure GetCleanFiles(List : TStrings; ACPU:TCPU; AOS : TOS); vi'+
  'rtual',';'#010+
  '    procedure GetInstallFiles(List: TStrings;Types : TTargetTypes;ACPU'+
  ':TCPU; AOS : TOS); virtual;'#010+
  '    procedure GetInstallSourceFiles(List: TStrings; SourceTypes : TSou'+
  'rceTypes; TargetTypes : TTargetTypes); virtual;'#010+
  '    Procedure GetArchi','veFiles(List : TStrings; ACPU:TCPU; AOS : TOS)'+
  '; virtual;'#010+
  '    Procedure GetArchiveSourceFiles(List : TStrings); virtual;'#010+
  '    Procedure GetManifest(Manifest : TStrings);'#010+
  '    Procedure AddPackageVariant(APackageVariant: TPackageVariants);'#010+
  '    ','procedure ApplyPackageVariantToCompilerOptions(ACompilerOptions:'+
  ' tstrings);'#010+
  '    procedure SetDefaultPackageVariant;'#010+
  '    procedure LoadUnitConfigFromFile(Const AFileName: String);'#010+
  '    procedure SaveUnitConfigToFile(Const AFileName: String;AC','PU:TCPU'+
  ';AOS:TOS);'#010+
  '    Property Version : String Read GetVersion Write SetVersion;'#010+
  '    Property FileName : String Read GetFileName Write FFileName;'#010+
  '    Property HomepageURL : String Read FHomepageURL Write FHomepageURL'+
  ';'#010+
  '    Property Download','URL : String Read FDownloadURL Write FDownloadU'+
  'RL;'#010+
  '    Property Email : String Read FEmail Write FEmail;'#010+
  '    Property Author : String Read FAuthor Write FAuthor;'#010+
  '    Property License : String Read FLicense Write FLicense;'#010+
  '    Property Direc','tory : String Read FDirectory Write FDirectory;'#010+
  '    Property Description : String Read GetDescription Write FDescripti'+
  'on;'#010+
  '    Property DescriptionFile : String Read FDescriptionFile Write FDes'+
  'criptionFile;'#010+
  '    Property InstalledChecksum : C','ardinal Read FInstalledChecksum Wr'+
  'ite FInstalledChecksum;'#010+
  '    Property IsFPMakeAddIn: boolean read FIsFPMakeAddIn write FIsFPMak'+
  'eAddIn;'#010+
  '    Property SupportBuildModes: TBuildModes read FSupportBuildModes wr'+
  'ite FSupportBuildModes;'#010+
  '    Proper','ty BuildMode: TBuildMode read FBuildMode;'#010+
  '    Property Flags: TStrings read FFlags;'#010+
  '    // Compiler options.'#010+
  '    Property OSes : TOSes Read FOSes Write FOSes;'#010+
  '    Property CPUs : TCPUs Read FCPUs Write FCPUs;'#010+
  '    Property NeedLibC : Boolean',' Read FNeedLibC Write FNeedLibC;'#010+
  '    Property Options: TStrings Read GetOptions Write SetOptions;'#010+
  '    Property UnitPath : TConditionalStrings Read FUnitPath;'#010+
  '    Property ObjectPath : TConditionalStrings Read FObjectPath;'#010+
  '    Property Inclu','dePath : TConditionalStrings Read FIncludePath;'#010+
  '    Property SourcePath : TConditionalStrings Read FSourcePath;'#010+
  '    Property ExamplePath : TConditionalStrings Read FExamplePath;'#010+
  '    Property TestPath : TConditionalStrings Read FTestPath;'#010+
  '  ','  Property FPDocFormat: TFPDocFormats read FFPDocFormat write FFPD'+
  'ocFormat;'#010+
  '    // Targets and dependencies'#010+
  '    Property InstallFiles : TConditionalDestStrings Read FInstallFiles'+
  ';'#010+
  '    Property CleanFiles : TConditionalStrings Read FCleanFil','es;'#010+
  '    Property Dependencies : TDependencies Read FDependencies;'#010+
  '    Property Commands : TCommands Read FCommands;'#010+
  '    Property State : TTargetState Read FTargetState;'#010+
  '    Property Targets : TTargets Read FTargets;'#010+
  '    Property Sources : T','Sources Read FSources;'#010+
  '    Property UnitDir : String Read FUnitDir Write FUnitDir;'#010+
  '    // events'#010+
  '    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FB'+
  'eforeCompile;'#010+
  '    Property BeforeCompileProc : TNotifyProcEvent Read FBef','oreCompil'+
  'eProc write FBeforeCompileProc;'#010+
  '    Property AfterCompile : TNotifyEvent Read FAfterCompile Write FAft'+
  'erCompile;'#010+
  '    Property AfterCompileProc : TNotifyProcEvent Read FAfterCompilePro'+
  'c Write FAfterCompileProc;'#010+
  '    Property BeforeIn','stall : TNotifyEvent Read FBeforeInstall Write '+
  'FBeforeInstall;'#010+
  '    Property BeforeInstallProc : TNotifyProcEvent Read FBeforeInstallP'+
  'roc Write FBeforeInstallProc;'#010+
  '    Property AfterInstall : TNotifyEvent Read FAfterInstall Write FAft'+
  'erInsta','ll;'#010+
  '    Property AfterInstallProc : TNotifyProcEvent Read FAfterInstallPro'+
  'c Write FAfterInstallProc;'#010+
  '    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBefor'+
  'eClean;'#010+
  '    Property BeforeCleanProc : TNotifyProcEvent Read FBeforeC','leanPro'+
  'c Write FBeforeCleanProc;'#010+
  '    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterCl'+
  'ean;'#010+
  '    Property AfterCleanProc : TNotifyProcEvent Read FAfterCleanProc Wr'+
  'ite FAfterCleanProc;'#010+
  '    Property BeforeArchive : TNotifyEvent',' Read FBeforeArchive Write '+
  'FBeforeArchive;'#010+
  '    Property BeforeArchiveProc : TNotifyProcEvent Read FBeforeArchiveP'+
  'roc Write FBeforeArchiveProc;'#010+
  '    Property AfterArchive : TNotifyEvent Read FAfterArchive Write FAft'+
  'erArchive;'#010+
  '    Property Aft','erArchiveProc : TNotifyProcEvent Read FAfterArchiveP'+
  'roc Write FAfterArchiveProc;'#010+
  '    Property BeforeManifest : TNotifyEvent Read FBeforeManifest Write '+
  'FBeforeManifest;'#010+
  '    Property BeforeManifestProc : TNotifyProcEvent Read FBeforeManifes'+
  'tP','roc Write FBeforeManifestProc;'#010+
  '    Property AfterManifest : TNotifyEvent Read FAfterManifest Write FA'+
  'fterManifest;'#010+
  '    Property AfterManifestProc : TNotifyProcEvent Read FAfterManifestP'+
  'roc Write FAfterManifestProc;'#010+
  '  end;'#010+
  #010+
  '  { TPackages }'#010+
  #010,
  '  TPackages = Class(TNamedCollection)'#010+
  '  private'#010+
  '    function GetPackage(const AName : String): TPackage;'#010+
  '    function GetPackageItem(AIndex : Integer): TPackage;'#010+
  '    procedure SetPackageItem(AIndex : Integer; const AValue: TPackage)'+
  ';'#010+
  '  Publ','ic'#010+
  '    Function AddPackage(Const AName : String) : TPackage;'#010+
  '    Property Packages[AName : String] : TPackage Read GetPackage ; Def'+
  'ault;'#010+
  '    Property PackageItems[AIndex : Integer] : TPackage Read GetPackage'+
  'Item Write SetPackageItem;'#010+
  '  end;',#010+
  #010+
  '  { TCustomDefaults }'#010+
  #010+
  '  TCustomDefaults = Class(TPersistent)'#010+
  '  Private'#010+
  '    FArchive: String;'#010+
  '    FBuildMode: TBuildMode;'#010+
  '    FCompiler: String;'#010+
  '    FCopy: String;'#010+
  '    FFPDocOutputDir: String;'#010+
  '    FIgnoreInvalidOptions: Boolean;'#010+
  '    FInst','allExamples: Boolean;'#010+
  '    FMkDir: String;'#010+
  '    FMove: String;'#010+
  '    FOptions: TStrings;'#010+
  '    FCPU: TCPU;'#010+
  '    FOS: TOS;'#010+
  '    FMode : TCompilerMode;'#010+
  '    FCompilerVersion : String;'#010+
  '    FPrefix: String;'#010+
  '    FLocalUnitDir,'#010+
  '    FGlobalUnitDir,'#010+
  '    FBa','seInstallDir,'#010+
  '    FUnitInstallDir,'#010+
  '    FBinInstallDir,'#010+
  '    FDocInstallDir,'#010+
  '    FExamplesInstallDir : String;'#010+
  '    FSkipCrossPrograms: boolean;'#010+
  '    FThreadsAmount: integer;'#010+
  '    FRemoveTree: String;'#010+
  '    FRemoveDir: String;'#010+
  '    FRemove: String;',#010+
  '    FTarget: String;'#010+
  '    FUnixPaths: Boolean;'#010+
  '    FNoFPCCfg: Boolean;'#010+
  '    FUseEnvironment: Boolean;'#010+
  '    function GetBuildCPU: TCpu;'#010+
  '    function GetBuildOS: TOS;'#010+
  '    function GetBuildString: String;'#010+
  '    function GetFPDocOutputDir: String;'#010,
  '    function GetLocalUnitDir: String;'#010+
  '    function GetGlobalUnitDir: String;'#010+
  '    function GetBaseInstallDir: String;'#010+
  '    function GetBinInstallDir: String;'#010+
  '    function GetCompiler: String;'#010+
  '    function GetDocInstallDir: String;'#010+
  '    functio','n GetExamplesInstallDir: String;'#010+
  '    function GetOptions: TStrings;'#010+
  '    function GetUnitInstallDir: String;'#010+
  '    procedure SetLocalUnitDir(const AValue: String);'#010+
  '    procedure SetGlobalUnitDir(const AValue: String);'#010+
  '    procedure IntSetBaseI','nstallDir(const AValue: String);'#010+
  '    procedure SetBaseInstallDir(const AValue: String);'#010+
  '    procedure SetCPU(const AValue: TCPU);'#010+
  '    procedure SetOptions(const AValue: TStrings);'#010+
  '    procedure SetOS(const AValue: TOS);'#010+
  '    procedure SetPre','fix(const AValue: String);'#010+
  '    procedure SetTarget(const AValue: String);'#010+
  '    procedure SetUnitInstallDir(const AValue: String);'#010+
  '  Protected'#010+
  '    procedure RecalcTarget;'#010+
  '    Function CmdLineOptions : String;'#010+
  '  Public'#010+
  '    Constructor Create;'#010,
  '    Procedure InitDefaults;'#010+
  '    Function HaveOptions: Boolean;'#010+
  '    function IsBuildDifferentFromTarget: boolean;'#010+
  '    procedure CompilerDefaults; virtual;'#010+
  '    Procedure LocalInit(Const AFileName : String);'#010+
  '    Procedure LoadFromFile(Const AF','ileName : String);'#010+
  '    Procedure SaveToFile(Const AFileName : String);'#010+
  '    procedure SaveToStream(S : TStream);virtual;'#010+
  '    procedure LoadFromStream(S : TStream);virtual;'#010+
  '    // Compile Information'#010+
  '    Property Target : String Read FTarget ','Write SetTarget;'#010+
  '    Property OS : TOS Read FOS Write SetOS;'#010+
  '    Property CPU : TCPU Read FCPU Write SetCPU;'#010+
  '    Property BuildString : String read GetBuildString;'#010+
  '    Property BuildOS : TOS read GetBuildOS;'#010+
  '    Property BuildCPU : TCpu rea','d GetBuildCPU;'#010+
  '    Property Mode : TCompilerMode Read FMode Write FMode;'#010+
  '    Property UnixPaths : Boolean Read FUnixPaths Write FUnixPaths;'#010+
  '    Property Options : TStrings Read GetOptions Write SetOptions;    /'+
  '/ Default compiler options.'#010+
  '  ','  Property NoFPCCfg : Boolean Read FNoFPCCfg Write FNoFPCCfg;'#010+
  '    // When ThreadsAmount is specified, #threadsamount# worker-threads'+
  ' are'#010+
  '    // created. When such a worker-thread is ready all worker-threads '+
  'are evaluated'#010+
  '    // to see if th','ere are idle threads (there is always at least on'+
  'e such thread.)'#010+
  '    // To each idle thread a package is assigned which has to be compi'+
  'led for the'#010+
  '    // current target and for which all dependencies are compiled earl'+
  'ier.'#010+
  '    // When no pac','kage is available the thread remains idle until an'+
  'other thread'#010+
  '    // has finished it'#039's task. Compilation stops when all packages'+
  ' are compiled'#010+
  '    // or when an error occures.'#010+
  '    //'#010+
  '    // When ThreadsAmount is not specified (-1), all pack','ages are co'+
  'mpiled on by one.'#010+
  '    // Dependencies are compiled recursively. When a package is alread'+
  'y compiled'#010+
  '    // (because some other package was depending on it) the package is'+
  ' skipped.'#010+
  '    // When the last package in the list is compil','ed, the compilatio'+
  'n stops.'#010+
  '    Property ThreadsAmount : integer Read FThreadsAmount Write FThread'+
  'sAmount;'#010+
  '    // paths etc.'#010+
  '    Property LocalUnitDir : String Read GetLocalUnitDir Write SetLocal'+
  'UnitDir;'#010+
  '    Property GlobalUnitDir : String R','ead GetGlobalUnitDir Write SetG'+
  'lobalUnitDir;'#010+
  '    Property Prefix : String Read FPrefix Write SetPrefix;'#010+
  '    Property BaseInstallDir : String Read GetBaseInstallDir Write SetB'+
  'aseInstallDir;'#010+
  '    Property UnitInstallDir : String Read GetUnitIn','stallDir Write Se'+
  'tUnitInstallDir;'#010+
  '    Property BinInstallDir : String Read GetBinInstallDir Write FBinIn'+
  'stallDir;'#010+
  '    Property DocInstallDir : String Read GetDocInstallDir Write FDocIn'+
  'stallDir;'#010+
  '    Property ExamplesInstallDir : String Read ','GetExamplesInstallDir '+
  'Write FExamplesInstallDir;'#010+
  '    Property FPDocOutputDir : String Read GetFPDocOutputDir Write FFPD'+
  'ocOutputDir;'#010+
  '    // Command tools. If not set, internal commands  will be used.'#010+
  '    Property Compiler : String Read GetCo','mpiler Write FCompiler; // '+
  'Compiler. Defaults to fpc'#010+
  '    Property Copy : String Read FCopy Write FCopy;             // copy'+
  ' $(FILES) to $(DEST)'#010+
  '    Property Move : String Read FMove Write FMove;             // Move'+
  ' $(FILES) to $(DEST)'#010+
  '    P','roperty Remove : String Read FRemove Write FRemove;       // De'+
  'lete $(FILES)'#010+
  '    Property RemoveDir : String Read FRemoveDir Write FRemoveDir;     '+
  '  // Delete $(FILES)'#010+
  '    Property RemoveTree : String Read FRemoveTree Write FRemoveTree;  '+
  '  ','   // removes $(DIRECTORY)'#010+
  '    Property MkDir : String Read FMkDir write FMkDir;          // Make'+
  ' $(DIRECTORY)'#010+
  '    Property Archive : String Read FArchive Write FArchive;    // zip '+
  '$(ARCHIVE) $(FILESORDIRS)'#010+
  '    // Misc'#010+
  '    Property UseEnvir','onment : Boolean read FUseEnvironment write FUs'+
  'eEnvironment;'#010+
  '    Property IgnoreInvalidOptions: Boolean read FIgnoreInvalidOptions '+
  'write FIgnoreInvalidOptions;'#010+
  '    Property BuildMode: TBuildMode read FBuildMode write FBuildMode;'#010+
  '    // Inst','allation optioms'#010+
  '    Property InstallExamples: Boolean read FInstallExamples write FIns'+
  'tallExamples;'#010+
  '    Property SkipCrossPrograms: boolean read FSkipCrossPrograms write '+
  'FSkipCrossPrograms;'#010+
  '  end;'#010+
  #010+
  '  { TBasicDefaults }'#010+
  #010+
  '  TBasicDefaults = ','Class(TCustomDefaults)'#010+
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
  #010+
  '  TBuildEngine = Class(TComponent)'#010+
  '  private'#010+
  '    // general variables'#010,
  '    FCompiler : String;'#010+
  '    FStartDir : String;'#010+
  '    FForceCompile : Boolean;'#010+
  '    FListMode : Boolean;'#010+
  '    FVerbose : boolean;'#010+
  '    FProgressMax : integer;'#010+
  '    FProgressCount : integer;'#010+
  '{$ifdef HAS_UNIT_ZIPPER}'#010+
  '    FZipFile: TZipper;'#010+
  '{$endif ','HAS_UNIT_ZIPPER}'#010+
  '    FExternalPackages : TPackages;'#010+
  '    // Events'#010+
  '    FOnLog: TLogEvent;'#010+
  '    FAfterArchive: TNotifyEvent;'#010+
  '    FAfterClean: TNotifyEvent;'#010+
  '    FAfterCompile: TNotifyEvent;'#010+
  '    FAfterInstall: TNotifyEvent;'#010+
  '    FAfterManifest: T','NotifyEvent;'#010+
  '    FBeforeArchive: TNotifyEvent;'#010+
  '    FBeforeClean: TNotifyEvent;'#010+
  '    FBeforeCompile: TNotifyEvent;'#010+
  '    FBeforeInstall: TNotifyEvent;'#010+
  '    FBeforeManifest: TNotifyEvent;'#010+
  '    FZipper: TZipper;'#010+
  '  Protected'#010+
  '    Procedure Error(cons','t Msg : String);'#010+
  '    Procedure Error(const Fmt : String; const Args : Array of const);'#010+
  '    // Internal copy/delete/move/archive/mkdir files'#010+
  '    Function  SysDirectoryExists(const ADir:string):Boolean;'#010+
  '    Function  SysFileExists(const AFile','Name:string):Boolean;'#010+
  '    Procedure SysCopyFile(Const Src,Dest : String); virtual;'#010+
  '    Procedure SysMoveFile(Const Src,Dest : String); virtual;'#010+
  '    Procedure SysDeleteFile(Const AFileName : String); virtual;'#010+
  '    Procedure SysDeleteDirectory','(Const ADirectoryName : String); vir'+
  'tual;'#010+
  '    Procedure SysDeleteTree(Const ADirectoryName : String); virtual;'#010+
  '    Procedure SysArchiveFiles(List : TStrings; Const AFileName : Strin'+
  'g); virtual;'#010+
  '    procedure LogIndent;'#010+
  '    procedure LogUnIn','dent;'#010+
  '    Procedure EnterDir(ADir : String);'#010+
  '    Function GetCompiler : String;'#010+
  '    Function InstallPackageFiles(APAckage : TPackage; tt : TTargetType'+
  's; Const Dest : String):Boolean;'#010+
  '    Procedure InstallUnitConfigFile(APAckage : TPackage; ','Const Dest '+
  ': String);'#010+
  '    Function InstallPackageSourceFiles(APAckage : TPackage; stt : TSou'+
  'rceTypes; ttt : TTargetTypes; Const Dest : String):Boolean;'#010+
  '    Function FileNewer(const Src,Dest : String) : Boolean;'#010+
  '    Procedure LogSearchPath(A','Package: TPackage;const ASearchPathName'+
  ':string;Path:TConditionalStrings; ACPU:TCPU;AOS:TOS);'#010+
  '    Function FindFileInPath(APackage: TPackage; Path:TConditionalStrin'+
  'gs; AFileName:String; var FoundPath:String;ACPU:TCPU;AOS:TOS):Boolean;'+
  #010+
  #010+
  '    p','rocedure GetDirectoriesFromFilelist(const AFileList, ADirectory'+
  'List: TStringList);'#010+
  '    //package commands'#010+
  '    function  GetUnitDir(APackage:TPackage):String;'#010+
  '    procedure AddDependencyPaths(L: TStrings; DependencyType: TDepende'+
  'ncyType; ATa','rget: TTarget);'#010+
  '    procedure AddDependencyUnitPaths(L:TStrings;APackage: TPackage);'#010+
  '  Public'#010+
  '    Constructor Create(AOwner : TComponent); override;'#010+
  '    destructor Destroy;override;'#010+
  '    function AddPathPrefix(APackage: TPackage; APath: stri','ng): strin'+
  'g;'#010+
  #010+
  '    property Verbose : boolean read FVerbose write FVerbose;'#010+
  '    Procedure ResolveFileNames(APackage : TPackage; ACPU:TCPU;AOS:TOS;'+
  'DoChangeDir:boolean=true; WarnIfNotFound:boolean=true);'#010+
  #010+
  '    // Public Copy/delete/Move/Archive','/Mkdir Commands.'#010+
  '    Procedure ExecuteCommand(const Cmd,Args : String; const Env: TStri'+
  'ngs = nil; IgnoreError : Boolean = False); virtual;'#010+
  '    Procedure CmdCopyFiles(List : TStrings; Const DestDir : String);'#010+
  '    Procedure CmdCreateDir(const',' DestDir : String);'#010+
  '    Procedure CmdMoveFiles(List : TStrings; Const DestDir : String);'#010+
  '    Procedure CmdDeleteFiles(List : TStrings);'#010+
  '    Procedure CmdArchiveFiles(List : TStrings; Const ArchiveFile : Str'+
  'ing);'#010+
  '    Procedure CmdRenameFile(','SourceName, DestName : String);'#010+
  '    Procedure CmdRemoveDirs(List: TStrings);'#010+
  '    Procedure CmdRemoveTrees(List: TStrings);'#010+
  '    Procedure ExecuteCommands(Commands : TCommands; At : TCommandAt; A'+
  'Package: TPackage = nil);'#010+
  '    // Dependency com','mands'#010+
  '    Function  DependencyOK(ADependency : TDependency) : Boolean;'#010+
  '    // Target commands'#010+
  '    Function  GetCompilerCommand(APackage : TPackage; ATarget : TTarge'+
  't; Env: TStrings) : String;'#010+
  '    Function  TargetOK(ATarget : TTarget; ACPU: ','TCPU; AOS: TOS) : Bo'+
  'olean;'#010+
  '    Function  TargetInstallOK(ATarget : TTarget;ACPU:TCPU; AOS : TOS) '+
  ': Boolean;'#010+
  '    Function  NeedsCompile(APackage:TPackage; ATarget : TTarget) : Boo'+
  'lean;'#010+
  '    Procedure Compile(APackage:TPackage; ATarget : TTarg','et);  virtua'+
  'l;'#010+
  '    Procedure MaybeCompile(APackage:TPackage; ATarget: TTarget);'#010+
  '    Procedure CompileDependencies(APackage:TPackage; ATarget: TTarget)'+
  ';'#010+
  '    // Package commands'#010+
  '{    Function  GetPackageDir(APackage : TPackage; AbsolutePath :',' Boo'+
  'lean = False) : String;'#010+
  '    Function  GetUnitsOutputDir(APackage : TPackage; AbsolutePath : Bo'+
  'olean = False) : String;'#010+
  '    Function  GetBinOutputDir(APackage : TPackage; AbsolutePath : Bool'+
  'ean = False) : String;  }'#010+
  '    Function  Package','OK(APackage : TPackage) : Boolean; virtual;'#010+
  '    Procedure DoBeforeCompile(APackage : TPackage);virtual;'#010+
  '    Procedure DoAfterCompile(APackage : TPackage);virtual;'#010+
  '    Procedure DoBeforeInstall(APackage : TPackage);virtual;'#010+
  '    Procedure DoA','fterInstall(APackage : TPackage);virtual;'#010+
  '    Procedure DoBeforeArchive(APackage : TPackage);virtual;'#010+
  '    Procedure DoAfterArchive(APackage : TPackage);virtual;'#010+
  '    Procedure DoBeforeClean(APackage : TPackage);virtual;'#010+
  '    Procedure DoAfter','Clean(APackage : TPackage);virtual;'#010+
  '    Function  NeedsCompile(APackage : TPackage) : Boolean; virtual;'#010+
  '    Procedure Compile(APackage : TPackage);'#010+
  '    Procedure MaybeCompile(APackage:TPackage);'#010+
  '    Function ReadyToCompile(APackage:TPackage',') : Boolean;'#010+
  '    Procedure Install(APackage : TPackage);'#010+
  '    Procedure Archive(APackage : TPackage);'#010+
  '    Procedure Manifest(APackage : TPackage);'#010+
  '    Procedure Clean(APackage : TPackage; AllTargets: boolean);'#010+
  '    Procedure Clean(APackage : ','TPackage; ACPU:TCPU; AOS : TOS);'#010+
  '    Procedure CompileDependencies(APackage : TPackage);'#010+
  '    function CheckDependencies(APackage : TPackage): TCheckDependencyR'+
  'esult;'#010+
  '    Function  CheckExternalPackage(Const APackageName : String):TPacka'+
  'ge;'#010,
  '    procedure CreateOutputDir(APackage: TPackage);'#010+
  '    // Packages commands'#010+
  '    Procedure Compile(Packages : TPackages);'#010+
  '    Procedure Install(Packages : TPackages);'#010+
  '    Procedure ZipInstall(Packages : TPackages);'#010+
  '    Procedure Archive(Pack','ages : TPackages);'#010+
  '    procedure Manifest(Packages: TPackages);'#010+
  '    Procedure Clean(Packages : TPackages; AllTargets: boolean);'#010+
  #010+
  '    Procedure Log(Level : TVerboseLevel; Msg : String);'#010+
  '    Procedure Log(Level : TVerboseLevel; Fmt : String; ','const Args : '+
  'Array Of Const);'#010+
  #010+
  '    Property ListMode : Boolean Read FListMode Write FListMode;'#010+
  '    Property ForceCompile : Boolean Read FForceCompile Write FForceCom'+
  'pile;'#010+
  '    Property ExternalPackages: TPackages Read FExternalPackages;'#010+
  '    ','Property StartDir: String Read FStartDir;'#010+
  '    // Events'#010+
  '    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FB'+
  'eforeCompile;'#010+
  '    Property AfterCompile : TNotifyEvent Read FAfterCompile Write FAft'+
  'erCompile;'#010+
  '    Property Before','Install : TNotifyEvent Read FBeforeInstall Write '+
  'FBeforeInstall;'#010+
  '    Property AfterInstall : TNotifyEvent Read FAfterInstall Write FAft'+
  'erInstall;'#010+
  '    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBefor'+
  'eClean;'#010+
  '    Property Aft','erClean : TNotifyEvent Read FAfterClean Write FAfter'+
  'Clean;'#010+
  '    Property BeforeArchive : TNotifyEvent Read FBeforeArchive Write FB'+
  'eforeArchive;'#010+
  '    Property AfterArchive : TNotifyEvent Read FAfterArchive Write FAft'+
  'erArchive;'#010+
  '    Property Bef','oreManifest : TNotifyEvent Read FBeforeManifest Writ'+
  'e FBeforeManifest;'#010+
  '    Property AfterManifest : TNotifyEvent Read FAfterManifest Write FA'+
  'fterManifest;'#010+
  '    Property OnLog : TLogEvent Read FOnLog Write FOnlog;'#010+
  '  end;'#010+
  #010+
  '  { TCustomInstaller',' }'#010+
  #010+
  '  TCustomInstaller = Class(TComponent)'#010+
  '  private'#010+
  '    FBuildEngine: TBuildEngine;'#010+
  '    FPackages: TPackages;'#010+
  '    FRunMode: TRunMode;'#010+
  '    FListMode : Boolean;'#010+
  '    FLogLevels : TVerboseLevels;'#010+
  '    FFPMakeOptionsString: string;'#010+
  '    FPackageV','ariantSettings: TStrings;'#010+
  '    FPackageVariants: TFPList;'#010+
  '  Protected'#010+
  '    Procedure Log(Level : TVerboseLevel; Const Msg : String);'#010+
  '    Procedure CreatePackages; virtual;'#010+
  '    Procedure FreePackages; virtual;'#010+
  '    function GetPackages: TPackag','es; virtual;'#010+
  '    Procedure CheckPackages; virtual;'#010+
  '    Procedure CreateBuildEngine; virtual;'#010+
  '    Procedure Error(const Msg : String);'#010+
  '    Procedure Error(const Fmt : String; Args : Array of const);'#010+
  '    Procedure AnalyzeOptions;'#010+
  '    Procedur','e Usage(const FMT : String; Args : Array of const);'#010+
  '    Procedure Compile(Force : Boolean); virtual;'#010+
  '    Procedure Clean(AllTargets: boolean); virtual;'#010+
  '    Procedure Install; virtual;'#010+
  '    Procedure ZipInstall; virtual;'#010+
  '    Procedure Archive','; virtual;'#010+
  '    Procedure Manifest; virtual;'#010+
  '  Public'#010+
  '    Constructor Create(AOwner : TComponent); virtual;'#010+
  '    Destructor destroy; override;'#010+
  '    Function AddPackage(Const AName : String) : TPackage;'#010+
  '    Function  AddPackageVariant(AName: st','ring; AIsInheritable: boole'+
  'an): TPackageVariants;'#010+
  '    Function Run : Boolean;'#010+
  '    Property FPMakeOptionsString: string read FFPMakeOptionsString;'#010+
  '    Property BuildEngine : TBuildEngine Read FBuildEngine;'#010+
  '    //files in package'#010+
  '    Property',' Packages : TPackages Read GetPackages;'#010+
  '    Property RunMode : TRunMode Read FRunMode;'#010+
  '    Property ListMode : Boolean Read FListMode;'#010+
  '  end;'#010+
  #010+
  '  { TFPCInstaller }'#010+
  '  TFPCInstaller = class(TCustomInstaller)'#010+
  '  public'#010+
  '    Constructor Create(AOw','ner : TComponent); override;'#010+
  '  end;'#010+
  #010+
  '  { TBasicInstaller }'#010+
  '  TBasicInstaller = class(TCustomInstaller)'#010+
  '    Constructor Create(AOwner : TComponent); override;'#010+
  '  end;'#010+
  #010+
  '  { TValueItem }'#010+
  #010+
  '  TValueItem = Class(TObject)'#010+
  '    FValue : String;'#010+
  '    C','onstructor Create(AValue : String);'#010+
  '  end;'#010+
  #010+
  '  { TFunctionItem }'#010+
  #010+
  '  TFunctionItem = Class(TObject)'#010+
  '    FFunc : TReplaceFunction;'#010+
  '    Constructor Create(AFunc : TReplaceFunction);'#010+
  '  end;'#010+
  #010+
  '{$ifndef NO_THREADING}'#010+
  #010+
  '  { TCompileWorkerThread }'#010+
  #010+
  '  ','TCompileWorkerThread = class(TThread)'#010+
  '  private'#010+
  '    FBuildEngine: TBuildEngine;'#010+
  '    FCompilationOK: boolean;'#010+
  '    FDone: boolean;'#010+
  '    FErrorMessage: string;'#010+
  '    FNotifyMainThreadEvent: PRTLEvent;'#010+
  '    FNotifyStartTask: PRTLEvent;'#010+
  '    FPackage',': TPackage;'#010+
  '  protected'#010+
  '    procedure execute; override;'#010+
  '    property Done: boolean read FDone;'#010+
  '    property APackage: TPackage read FPackage write FPackage;'#010+
  '    property CompilationOK: boolean read FCompilationOK;'#010+
  '    property NotifyStartT','ask: PRTLEvent read FNotifyStartTask;'#010+
  '    property ErrorMessage: string read FErrorMessage;'#010+
  '  public'#010+
  '    constructor Create(ABuildEngine: TBuildEngine; NotifyMainThreadEve'+
  'nt: PRTLEvent); virtual;'#010+
  '    destructor Destroy; override;'#010+
  '  end;'#010+
  #010+
  '{$','endif NO_THREADING}'#010+
  #010+
  '  ECollectionError = Class(Exception);'#010+
  '  EDictionaryError = Class(Exception);'#010+
  '  EInstallerError = Class(Exception);'#010+
  #010+
  '  TInstallerClass = Class of TCustomInstaller;'#010+
  '  TDictionaryClass = Class of TDictionary;'#010+
  '  TPackageDi','ctionaryClass = Class of TPackageDictionary;'#010+
  #010+
  'Type'#010+
  '  TArchiveEvent = Procedure (Const AFileName : String; List : TStrings'+
  ') of Object;'#010+
  '  TArchiveProc = Procedure (Const AFileName : String; List : TStrings)'+
  ';'#010+
  #010+
  'Var'#010+
  '  DictionaryClass : TDictiona','ryClass = TDictionary;'#010+
  '  PackageDictionaryClass : TPackageDictionaryClass = TPackageDictionar'+
  'y;'#010+
  '  OnArchiveFiles : TArchiveEvent = Nil;'#010+
  '  ArchiveFilesProc : TArchiveProc = Nil;'#010+
  #010+
  '  Defaults : TCustomDefaults; // Set by installer.'#010+
  '  GlobalDic','tionary : TDictionary;'#010+
  #010+
  #010+
  'Function CurrentOS : String;'#010+
  'Function CurrentCPU : String;'#010+
  #010+
  'Function Installer(InstallerClass: TInstallerClass) : TCustomInstaller'+
  '; overload;'#010+
  'Function Installer : TCustomInstaller; overload;'#010+
  #010+
  'Function OSToString(OS:',' TOS) : String;'#010+
  'Function OSesToString(OSes: TOSes) : String;'#010+
  'Function CPUToString(CPU: TCPU) : String;'#010+
  'Function CPUSToString(CPUS: TCPUS) : String;'#010+
  'Function StringToOS(const S : String) : TOS;'#010+
  'function IsDifferentFromBuild(ACpu: TCPU; AOs: ','TOs): boolean;'#010+
  '//Function StringToOSes(const S : String) : TOSes;'#010+
  'Function StringToCPU(const S : String) : TCPU;'#010+
  'Function StringToCPUS(const S : String) : TCPUS;'#010+
  'Function ModeToString(Mode: TCompilerMode) : String;'#010+
  'Function StringToMode(con','st S : String) : TCompilerMode;'#010+
  'Function MakeTargetString(CPU : TCPU;OS: TOS) : String;'#010+
  'Procedure StringToCPUOS(const S : String; Var CPU : TCPU; Var OS: TOS)'+
  ';'#010+
  'Function FixPath (const APath : String) : String; inline; deprecated '#039+
  'Use the ov','erload with AIsDir instead'#039';'#010+
  'Function FixPath (const APath : String; AIsDir : Boolean) : String;'#010+
  'Function IsRelativePath(const APath : String) : boolean;'#010+
  'Procedure ChangeDir(const APath : String);'#010+
  'Procedure SplitCommand(Const Cmd : String; ','Var Exe,Options : String)'+
  ';'#010+
  'Procedure AddCustomFpmakeCommandlineOption(const ACommandLineOption, H'+
  'elpMessage : string);'#010+
  'Function GetCustomFpmakeCommandlineOptionValue(const ACommandLineOptio'+
  'n : string) : string;'#010+
  'Function AddProgramExtension(','const ExecutableName: string; AOS : TOS'+
  ') : string;'#010+
  'Function GetImportLibraryFilename(const UnitName: string; AOS : TOS) :'+
  ' string;'#010+
  #010+
  'procedure SearchFiles(const AFileName: string; Recursive: boolean; var'+
  ' List: TStrings);'#010+
  #010+
  'Implementation'#010+
  #010+
  'uses',' typinfo, rtlconsts;'#010+
  #010+
  'type'#010+
  '  TUnsortedDuplicatesStringList = class(TStringList)'#010+
  '  public'#010+
  '    function Add(const S: string): Integer; override;'#010+
  '  end;'#010+
  #010+
  'var'#010+
  '  CustomFpmakeCommandlineOptions: TStrings;'#010+
  '  CustomFpMakeCommandlineValues: TStrings',';'#010+
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
  '  SErrInvalidCPU        = '#039'Invalid CPU name "%s"'#039';'#010+
  '  SErrInvalidOS         = '#039'Invalid OS name ','"%s"'#039';'#010+
  '  SErrInvalidMode       = '#039'Invalid compiler mode "%s"'#039';'#010+
  '  SErrInvalidTarget     = '#039'Invalid compiler target "%s"'#039';'#010+
  '  SErrNameExists        = '#039'Name "%s" already exists in the collect'+
  'ion.'#039';'#010+
  '  SErrNoSuchName        = '#039'Could not find item',' with name "%s" in'+
  ' the collection.'#039';'#010+
  '  SErrInValidArgument   = '#039'Invalid command-line argument at positi'+
  'on %d: %s'#039';'#010+
  '  SErrNeedArgument      = '#039'Option at position %d (%s) needs an arg'+
  'ument'#039';'#010+
  '  SErrNoPackagesDefined = '#039'No action possible: No p','ackages were '+
  'defined.'#039';'#010+
  '  SErrInstaller         = '#039'The installer encountered the following'+
  ' error:'#039';'#010+
  '  SErrDepUnknownTarget  = '#039'Unknown target for unit "%s" in depende'+
  'ncies for %s in package %s'#039';'#010+
  '  SErrExternalCommandFailed = '#039'External comma','nd "%s" failed with'+
  ' exit code %d. Console output:'#039'+LineEnding+'#039'%s'#039';'#010+
  '  SErrExtCommandNotFound= '#039'External command "%s" not found'#039';'#010+
  '  SErrCreatingDirectory = '#039'Failed to create directory "%s"'#039';'#010+
  '  SErrDeletingFile      = '#039'Failed to delete file "','%s"'#039';'#010+
  '  SErrRemovingDirectory = '#039'Failed to remove directory "%s"'#039';'#010+
  '  SErrMovingFile        = '#039'Failed to move file "%s" to "%s"'#039';'#010+
  '  SErrCopyingFile       = '#039'Failed to copy file "%s" to "%s"'#039';'#010+
  '  SErrChangeDirFailed   = '#039'Failed to enter direct','ory "%s"'#039';'#010+
  '  SErrInvalidArgumentToSubstitute = '#039'Invalid number of arguments t'+
  'o Substitute'#039';'#010+
  '  SErrNoArchiveSupport  = '#039'This binary contains no archive support'+
  '. Please recompile with archive support'#039';'#010+
  '  SErrNoDictionaryItem  = '#039'No item calle','d "%s" in the dictionary'+
  #039';'#010+
  '  SErrNoDictionaryValue = '#039'The item "%s" in the dictionary is not '+
  'a value'#039';'#010+
  '  SErrNoDictionaryFunc  = '#039'The item "%s" in the dictionary is not '+
  'a function'#039';'#010+
  '  SErrInvalidFPCInfo    = '#039'Compiler returns invalid inform','ation,'+
  ' check if fpc -iV works'#039';'#010+
  '  SErrDependencyNotFound = '#039'Could not find unit directory for depe'+
  'ndency package "%s"'#039';'#010+
  '  SErrAlreadyInitialized = '#039'Installer can only be initialized once'+
  #039';'#010+
  '  SErrInvalidState      = '#039'Invalid state for target',' %s'#039';'#010+
  '  SErrCouldNotCompile   = '#039'Could not compile target %s from packag'+
  'e %s'#039';'#010+
  '  SErrUnsupportedBuildmode = '#039'Package does not support this buildm'+
  'ode'#039';'#010+
  '  SErrPackVarNotExist   = '#039'There is no package variant with the na'+
  'me "%s"'#039';'#010+
  #010+
  '  SWarnCircu','larTargetDependency = '#039'Warning: Circular dependency '+
  'detected when compiling target %s with target %s'#039';'#010+
  '  SWarnCircularPackageDependency = '#039'Warning: Circular dependency d'+
  'etected when compiling package %s with package %s'#039';'#010+
  '  SWarnFailedToSetT','ime    = '#039'Warning: Failed to set timestamp on'+
  ' file "%s"'#039';'#010+
  '  SWarnFailedToGetTime    = '#039'Warning: Failed to get timestamp from'+
  ' file "%s"'#039';'#010+
  '  SWarnAttemptingToCompileNonNeutralTarget = '#039'Warning: Attempting '+
  'to compile non-neutral target %s'#039';'#010+
  '  ','SWarnSourceFileNotFound  = '#039'Warning: Source file "%s" from pac'+
  'kage %s not found for %s'#039';'#010+
  '  SWarnIncludeFileNotFound = '#039'Warning: Include file "%s" from pack'+
  'age %s not found for %s'#039';'#010+
  '  SWarnDepUnitNotFound     = '#039'Warning: Dependency on unit %','s is '+
  'not supported for %s'#039';'#010+
  '  SWarnTargetDependsOnPackage = '#039'Warning: Target %s of package %s '+
  'depends on another package (%s). These kind of dependencies are not pr'+
  'ocessed'#039';'#010+
  '  SWarnDependOnOtherPlatformPackage = '#039'Warning: Package %s depends',
  ' on package %s which is not available for the %s platform'#039';'#010+
  '  SWarnStartCompilingPackage = '#039'Start compiling package %s for tar'+
  'get %s.'#039';'#010+
  '  SWarnCompilingPackagecompleteProgress = '#039'[%3.0f%%] Compiled pack'+
  'age %s'#039';'#010+
  '  SWarnCompilingPackagecomple','te = '#039'Compiled package %s'#039';'#010+
  '  SWarnSkipPackageTargetProgress = '#039'[%3.0f%%] Skipped package %s w'+
  'hich has been disabled for target %s'#039';'#010+
  '  SWarnSkipPackageTarget = '#039'Skipped package %s which has been disa'+
  'bled for target %s'#039';'#010+
  '  SWarnInstallationP','ackagecomplete = '#039'Installation package %s fo'+
  'r target %s succeeded'#039';'#010+
  '  SWarnCleanPackagecomplete = '#039'Clean of package %s completed'#039';'+
  #010+
  '  SWarnCanNotGetAccessRights = '#039'Warning: Failed to copy access-rig'+
  'hts from file %s'#039';'#010+
  '  SWarnCanNotSetAccessRi','ghts = '#039'Warning: Failed to copy access-r'+
  'ights to file %s'#039';'#010+
  '  SWarnCanNotGetFileAge = '#039'Warning: Failed to get FileAge for %s'#039+
  ';'#010+
  '  SWarnExtCommandNotFound = '#039'Warning: External command "%s" not fo'+
  'und but "%s" is older then "%s"'#039';'#010+
  '  SWarnDuplica','tePackage = '#039'Warning: Package %s is already added.'+
  ' Using the existing package'#039';'#010+
  #010+
  '  SInfoPackageAlreadyProcessed = '#039'Package %s is already processed'#039+
  ';'#010+
  '  SInfoCompilingTarget    = '#039'Compiling target %s'#039';'#010+
  '  SInfoExecutingCommand   = '#039'Executing c','ommand "%s %s"'#039';'#010+
  '  SInfoCreatingOutputDir  = '#039'Creating output dir "%s"'#039';'#010+
  '  SInfoInstallingPackage  = '#039'Installing package %s'#039';'#010+
  '  SInfoArchivingPackage   = '#039'Archiving package %s in "%s"'#039';'#010+
  '  SInfoCleaningPackage    = '#039'Cleaning package %s'#039';'#010+
  '  S','InfoManifestPackage    = '#039'Creating manifest for package %s'#039+
  ';'#010+
  '  SInfoCopyingFile        = '#039'Copying file "%s" to "%s"'#039';'#010+
  '  SInfoDeletingFile       = '#039'Deleting file "%s"'#039';'#010+
  '  SInfoSourceNewerDest    = '#039'Source file "%s" (%s) is newer than d'+
  'estina','tion "%s" (%s).'#039';'#010+
  '  SInfoDestDoesNotExist   = '#039'Destination file "%s" does not exist.'+
  #039';'#010+
  '  SInfoFallbackBuildmode  = '#039'Buildmode not supported by package, f'+
  'alling back to one by one unit compilation'#039';'#010+
  '  SInfoFallbackBuildmodeBU= '#039'Buildmode not',' supported by package,'+
  ' falling back to compilation using a buildunit'#039';'#010+
  #010+
  '  SDbgComparingFileTimes    = '#039'Comparing file "%s" time "%s" to "%'+
  's" time "%s".'#039';'#010+
  '  SDbgCompilingDependenciesOfTarget = '#039'Compiling dependencies of t'+
  'arget %s'#039';'#010+
  '  SDbgRes','olvingSourcesOfTarget = '#039'Resolving filenames of target '+
  '%s for %s'#039';'#010+
  '  SDbgResolvedSourceFile    = '#039'Resolved source file %s to "%s"'#039+
  ';'#010+
  '  SDbgSourceAlreadyResolved = '#039'Source file of %s has been resolved'+
  ' earlier'#039';'#010+
  '  SDbgResolvedIncludeFile   = '#039,'Resolved include file %s to "%s"'#039+
  ';'#010+
  '  SDbgOutputNotYetAvailable = '#039'Output file %s not available'#039';'#010+
  '  SDbgDependencyOnUnit      = '#039'Dependency of %s on unit %s'#039';'#010+
  '  SDbgDependencyUnitRecompiled = '#039'Dependent unit %s is being recom'+
  'piled'#039';'#010+
  '  SDbgMu','stCompile           = '#039'Must compile %s. (%s)'#039';'#010+
  '  SDbgSkippingTargetWrongCPU = '#039'Skipping target %s, different CPU '+
  '(%s)'#039';'#010+
  '  SDbgSkippingTargetWrongOS  = '#039'Skipping target %s, different OS ('+
  '%s)'#039';'#010+
  '  SDbgTargetIsNotAUnitOrProgram = '#039'Skipping Targ','et %s, not an un'+
  'it or program'#039';'#010+
  '  SDbgConsideringTarget     = '#039'Considering target %s'#039';'#010+
  '  SDbgConsideringPackage    = '#039'Considering package %s'#039';'#010+
  '  SDbgExternalDependency    = '#039'External dependency %s found in "%s'+
  '"'#039';'#010+
  '  SDbgBuildEngineArchiving ',' = '#039'Build engine archiving'#039';'#010+
  '  SDbgBuildEngineGenerateManifests = '#039'Build engine generating mani'+
  'fests'#039';'#010+
  '  SDbgBuildEngineCleaning   = '#039'Build engine cleaning'#039';'#010+
  '  SDbgGenerating            = '#039'Generating "%s"'#039';'#010+
  '  SDbgLoading               = '#039'Lo','ading "%s"'#039';'#010+
  '  SDbgFound                 = '#039'Found'#039';'#010+
  '  SDbgNotFound              = '#039'Not Found'#039';'#010+
  '  SDbgDirectoryExists       = '#039'Directory "%s" %s'#039';'#010+
  '  SDbgFileExists            = '#039'File "%s" %s'#039';'#010+
  '  SDbgArchivingFile         = '#039'Archiving "%s"'#039';'#010,
  '  SDbgSearchPath            = '#039'Using %s path "%s"'#039';'#010+
  '  SDbgEnterDir              = '#039'Entering directory "%s"'#039';'#010+
  '  SDbgPackageChecksumChanged = '#039'Dependent package %s is modified'#039+
  ';'#010+
  '  SDbgFileDoesNotExist      = '#039'File "%s" does not exist'#039';'#010+
  '  SDbg','DirectoryDoesNotExist = '#039'Directory "%s" does not exist'#039+
  ';'#010+
  '  SDbgDirectoryNotEmpty     = '#039'Directory "%s" is not empty. Will n'+
  'ot remove'#039';'#010+
  '  SDbgGenerateBuildUnit     = '#039'Generate build-unit %s'#039';'#010+
  '  SDbgForcedCompile         = '#039'Forced compile'#039';'#010+
  ' ',' SDbgOutputDoesNotExist    = '#039'Output file does not exist'#039';'#010+
  '  SDbgNewerSource           = '#039'Source file is newer then output fi'+
  'le'#039';'#010+
  '  SDbgNewerInclude          = '#039'The include file %s is newer then o'+
  'utput file'#039';'#010+
  '  SDbgDependencyRecompiled  = '#039,'The unit %s where this unit depend'+
  's on is recompiled'#039';'#010+
  '  SDbgPackageDepRecompiled  = '#039'The package %s where this package d'+
  'epends on is recompiled'#039';'#010+
  '  SDbgTargetHasToBeCompiled = '#039'At least one of the targets in the '+
  'package has to be compiled.',#039';'#010+
  #010+
  '  // Help messages for usage'#010+
  '  SValue              = '#039'Value'#039';'#010+
  '  SHelpUsage          = '#039'Usage: %s command [options]'#039';'#010+
  '  SHelpCommand        = '#039'Where command is one of the following:'#039+
  ';'#010+
  '  SHelpCompile        = '#039'Compile all units in the pac','kage(s).'#039+
  ';'#010+
  '  SHelpBuild          = '#039'Build all units in the package(s).'#039';'#010+
  '  SHelpInstall        = '#039'Install all units in the package(s).'#039';'+
  #010+
  '  SHelpClean          = '#039'Clean (remove) all units in the package(s'+
  ').'#039';'#010+
  '  SHelpArchive        = '#039'Create ','archive (zip) with all units in '+
  'the package(s).'#039';'#010+
  '  SHelpHelp           = '#039'This message.'#039';'#010+
  '  SHelpManifest       = '#039'Create a manifest suitable for import in '+
  'repository.'#039';'#010+
  '  SHelpCmdOptions     = '#039'Where options is one or more of the follo'+
  'win','g:'#039';'#010+
  '  SHelpCPU            = '#039'Compile for indicated CPU.'#039';'#010+
  '  SHelpOS             = '#039'Compile for indicated OS'#039';'#010+
  '  SHelpTarget         = '#039'Compile for indicated target'#039';'#010+
  '  SHelpList           = '#039'list commands instead of actually executi'+
  'ng them','.'#039';'#010+
  '  SHelpPrefix         = '#039'Use indicated prefix directory for all co'+
  'mmands.'#039';'#010+
  '  SHelpNoFPCCfg       = '#039'Compiler will not use fpc.cfg'#039';'#010+
  '  SHelpBaseInstallDir = '#039'Use indicated directory as base install d'+
  'ir.'#039';'#010+
  '  SHelpLocalUnitDir   = '#039'Use in','dicated directory as local (user)'+
  ' unit dir.'#039';'#010+
  '  SHelpGlobalUnitDir  = '#039'Use indicated directory as global unit di'+
  'r.'#039';'#010+
  '  SHelpUnitInstallDir = '#039'Use indicated directory to install units '+
  'into.'#039';'#010+
  '  SHelpCompiler       = '#039'Use indicated binary as ','compiler'#039';'#010+
  '  SHelpConfig         = '#039'Use indicated config file when compiling.'+
  #039';'#010+
  '  SHelpOptions        = '#039'Pass extra options to the compiler.'#039';'+
  #010+
  '  SHelpVerbose        = '#039'Be verbose when working.'#039';'#010+
  '  SHelpInstExamples   = '#039'Install the example-','sources.'#039';'#010+
  '  SHelpSkipCrossProgs = '#039'Skip programs when cross-compiling/instal'+
  'ling'#039';'#010+
  '  SHelpIgnoreInvOpt   = '#039'Ignore further invalid options.'#039';'#010+
  '  sHelpFpdocOutputDir = '#039'Use indicated directory as fpdoc output f'+
  'older.'#039';'#010+
  '  sHelpThreads        ','= '#039'Enable the indicated amount of worker t'+
  'hreads.'#039';'#010+
  '  sHelpUseEnvironment = '#039'Use environment to pass options to compil'+
  'er.'#039';'#010+
  '  SHelpUseBuildUnit   = '#039'Compile package in Build-unit mode.'#039';'+
  #010+
  #010+
  #010+
  'Const'#010+
  '  // Keys for Defaults file. Do not localize.',#010+
  '  KeyCompiler = '#039'Compiler'#039';'#010+
  '  KeyArchive  = '#039'Archive'#039';'#010+
  '  KeyCopy     = '#039'Copy'#039';'#010+
  '  KeyMkDir    = '#039'MkDir'#039';'#010+
  '  KeyMove     = '#039'Move'#039';'#010+
  '  KeyRemove   = '#039'Remove'#039';'#010+
  '  KeyRemoveDir= '#039'RemoveDir'#039';'#010+
  '  KeyRemoveTree= '#039'RemoveTree'#039';'#010+
  '  KeyOptions  = '#039'Options'#039,';'#010+
  '  KeyCPU      = '#039'CPU'#039';'#010+
  '  KeyOS       = '#039'OS'#039';'#010+
  '  KeyMode     = '#039'Mode'#039';'#010+
  '  KeyPrefix   = '#039'Prefix'#039';'#010+
  '  KeyTarget   = '#039'Target'#039';'#010+
  '  KeyNoFPCCfg = '#039'NoFPCCfg'#039';'#010+
  '  KeyUseEnv   = '#039'UseEnv'#039';'#010+
  '  KeyLocalUnitDir       = '#039'LocalUnitDir'#039';'#010+
  '  KeyGlobalUnitDir   ','   = '#039'GlobalUnitDir'#039';'#010+
  '  KeyBaseInstallDir     = '#039'BaseInstallDir'#039';'#010+
  '  KeyUnitInstallDir     = '#039'UnitInstallDir'#039';'#010+
  '  KeyBinInstallDir      = '#039'BinInstallDir'#039';'#010+
  '  KeyDocInstallDir      = '#039'DocInstallDir'#039';'#010+
  '  KeyExamplesInstallDir = '#039'ExamplesInstallDi','r'#039';'#010+
  '  KeyInstallExamples    = '#039'InstallExamples'#039';'#010+
  '  KeySkipCrossProdrams  = '#039'SkipCrossPrograms'#039';'#010+
  '  // Keys for unit config'#010+
  '  KeyName     = '#039'Name'#039';'#010+
  '  KeyVersion  = '#039'Version'#039';'#010+
  '  KeyChecksum = '#039'Checksum'#039';'#010+
  '  KeyNeedLibC = '#039'NeedLibC'#039';'#010+
  '  KeyDepend','s  = '#039'Depends'#039';'#010+
  '  KeyFlags    = '#039'Flags'#039';'#010+
  '  KeyAddIn    = '#039'FPMakeAddIn'#039';'#010+
  '  KeySourcePath = '#039'SourcePath'#039';'#010+
  '  KeyFPMakeOptions = '#039'FPMakeOptions'#039';'#010+
  '  KeyPackageVar = '#039'PackageVariant_'#039';'#010+
  #010+
  '{***********************************************************','********'+
  '*********'#010+
  '                                Helpers'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  'function ExecuteFPC(Verbose: boolean; const Path: string; const ComLin'+
  'e: stri','ng; const Env: TStrings; ConsoleOutput: TMemoryStream): integ'+
  'er;'#010+
  'var'#010+
  '  P: TProcess;'#010+
  '  BytesRead: longint;'#010+
  #010+
  '  function ReadFromStream: longint;'#010+
  #010+
  '  const'#010+
  '    READ_BYTES = 2048;'#010+
  #010+
  '  type'#010+
  '    TMessages = (mCompiling, mLinking);'#010+
  #010+
  '  var'#010+
  '    //ifde','f the MsgNum so it contains the correct message numbers fo'+
  'r each compiler version.'#010+
  '    MsgNum : array [TMessages] of integer = (3104, 9015);'#010+
  #010+
  '    n: longint;'#010+
  '    BuffPos: longint;'#010+
  '    sLine: string;'#010+
  '    ch: char;'#010+
  '    msg: TMessages;'#010+
  '    ipo','s: integer;'#010+
  '    snum: string;'#010+
  '  begin'#010+
  '    // make sure we have room'#010+
  '    ConsoleOutput.SetSize(BytesRead + READ_BYTES);'#010+
  #010+
  '    // try reading it'#010+
  '    n := P.Output.Read((ConsoleOutput.Memory + BytesRead)^, READ_BYTES'+
  ');'#010+
  '    if n > 0 then'#010+
  '    beg','in'#010+
  '      Inc(BytesRead, n);'#010+
  #010+
  '      sLine := '#039#039';'#010+
  '      BuffPos := ConsoleOutput.Position;'#010+
  #010+
  '      //read lines from the stream'#010+
  '      repeat'#010+
  '        ConsoleOutput.Read(ch,1);'#010+
  #010+
  '        if ch in [#10, #13] then'#010+
  '        begin'#010+
  '          if Verbose',' then'#010+
  '            installer.log(vlInfo,sLine)'#010+
  '          else'#010+
  '            begin'#010+
  '              for msg := Low(TMessages) to High(TMessages) do'#010+
  '              begin'#010+
  '                snum := Format('#039'(%d)'#039', [MsgNum[msg]]);'#010+
  '                ipos := ','Pos(snum, sLine);'#010+
  '                if ipos = 1 then'#010+
  '                  installer.log(vlCommand,'#039'      '#039'+ Copy(sLine'+
  ', ipos + Length(snum), Length(sLine) - ipos - Length(snum) + 1));'#010+
  '              end;'#010+
  '            end;'#010+
  '          if (LineEnding=','#13#10) and (ch=#13) and'#010+
  '             (ConsoleOutput.Position<BytesRead) then'#010+
  '            begin'#010+
  '              ConsoleOutput.Read(ch,1);'#010+
  '              if ch=#10 then'#010+
  '                sLine:='#039#039#010+
  '              else'#010+
  '                sLine:=ch;'#010+
  '   ','         end'#010+
  '          else'#010+
  '            sLine := '#039#039';'#010+
  '          BuffPos := ConsoleOutput.Position;'#010+
  '        end'#010+
  '        else'#010+
  '          sLine := sLine + ch;'#010+
  #010+
  '      until ConsoleOutput.Position >= BytesRead;'#010+
  #010+
  '      ConsoleOutput.Position := Buf','fPos;'#010+
  '    end'#010+
  '    else'#010+
  '    begin'#010+
  '      // no data, wait 100 ms'#010+
  '      Sleep(100);'#010+
  '    end;'#010+
  #010+
  '    Result := n;'#010+
  '  end;'#010+
  #010+
  'begin'#010+
  '  result := -1;'#010+
  '  BytesRead := 0;'#010+
  '  P := TProcess.Create(nil);'#010+
  '  try'#010+
  '    P.CommandLine := Path + '#039' '#039' + ComLine;'#010+
  '    if',' assigned(Env) then'#010+
  '      P.Environment.Assign(Env);'#010+
  #010+
  '    P.Options := [poUsePipes];'#010+
  #010+
  '    P.Execute;'#010+
  '    while P.Running do'#010+
  '      ReadFromStream;'#010+
  #010+
  '    // read last part'#010+
  '    repeat'#010+
  '    until ReadFromStream = 0;'#010+
  '    ConsoleOutput.SetSize(Byte','sRead);'#010+
  #010+
  '    result := P.ExitStatus;'#010+
  '  finally'#010+
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
  ' ',' SearchResult := FindFirst(IncludeTrailingPathDelimiter(directory)+'+
  'AllFilesMask, faAnyFile+faSymLink, searchRec);'#010+
  '  try'#010+
  '    while SearchResult=0 do'#010+
  '      begin'#010+
  '        if (searchRec.Name<>'#039'.'#039') and (searchRec.Name<>'#039'..'#039+
  ') then'#010+
  '           begi','n'#010+
  '             result := false;'#010+
  '             break;'#010+
  '           end;'#010+
  '        SearchResult := FindNext(searchRec);'#010+
  '      end;'#010+
  '  finally'#010+
  '    FindClose(searchRec);'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'function ParsecompilerOutput(M: TMemoryStream; Verbose: boolean): s','t'+
  'ring;'#010+
  'type'#010+
  '  TParseCompilerOutputState = (cosBeginOfLine, cosSearchColon, cosPars'+
  'eNumber, cosOther);'#010+
  #010+
  'var'#010+
  '  presult: pchar;'#010+
  '  state: TParseCompilerOutputState;'#010+
  '  ch: char;'#010+
  '  eolchar: char;'#010+
  'begin'#010+
  '  m.Seek(0, soBeginning);'#010+
  '  setlength(Result',',M.Size);'#010+
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
  '  state := cosBeginOfLine;'#010+
  '  while m.Position<m.Size do'#010+
  ' ','   begin'#010+
  '      ch := char(m.ReadByte);'#010+
  '      case state of'#010+
  '        cosBeginOfLine:'#010+
  '          begin'#010+
  '            if ch='#039'('#039' then'#010+
  '              state := cosParseNumber'#010+
  '            else if ch='#039' '#039' then'#010+
  '              begin'#010+
  '                presult^',' := ch;'#010+
  '                inc(presult);'#010+
  '              end'#010+
  '            else'#010+
  '              begin'#010+
  '                presult^ := ch;'#010+
  '                inc(presult);'#010+
  '                state := cosSearchColon;'#010+
  '              end;'#010+
  '          end;'#010+
  '        co','sParseNumber:'#010+
  '          begin'#010+
  '            if ch='#039')'#039' then'#010+
  '              begin'#010+
  '              state := cosOther;'#010+
  '              // Omit the space behind the number'#010+
  '              ch := char(m.ReadByte);'#010+
  '              assert(ch='#039' '#039');'#010+
  '            ','  end;'#010+
  '          end;'#010+
  '        cosOther:'#010+
  '          begin'#010+
  '            presult^ := ch;'#010+
  '            inc(presult);'#010+
  '            if ch=eolchar then'#010+
  '              state := cosBeginOfLine;'#010+
  '          end;'#010+
  '        cosSearchColon:'#010+
  '          begin'#010+
  '     ','       presult^ := ch;'#010+
  '            inc(presult);'#010+
  '            if (ch='#039':'#039') or (ch=eolchar) then'#010+
  '              state := cosBeginOfLine;'#010+
  '          end;'#010+
  '      end;'#010+
  '    end;'#010+
  '  setlength(Result,presult-@result[1]);'#010+
  'end;'#010+
  #010+
  'Function QuoteXML(S : Stri','ng) : string;'#010+
  #010+
  '  Procedure W(Var J : Integer; Var R : String; T : String);'#010+
  '  Var'#010+
  '    I: integer;'#010+
  '  begin'#010+
  '    If J+Length(T)>Length(R) then'#010+
  '      SetLength(R,J+Length(T));'#010+
  '    For I:=1 to Length(t) do'#010+
  '      begin'#010+
  '      R[J]:=T[i];'#010+
  '      If I','<Length(T) then'#010+
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
  '  SetLength(Result,Length(S));'#010+
  '  J:=0;'#010+
  '  For I:=1 to Length(S) do'#010+
  '    begin'#010+
  '  ','  Inc(J);'#010+
  '    case S[i] of'#010+
  '      '#039'"'#039': W(j,Result,QuotStr);'#010+
  '      '#039'&'#039': W(J,Result,AmpStr);'#010+
  '      '#039'<'#039': W(J,Result,ltStr);'#010+
  '      '#039'>'#039': W(J,Result,gtStr);'#010+
  '      // Escape whitespace using CharRefs to be consistent with W3 spe'+
  'c X 3.3.3'#010+
  '       #9:',' w(J,Result,'#039'&#x9;'#039');'#010+
  '{      #10: wrtStr('#039'&#xA;'#039');'#010+
  '       #13: wrtStr('#039'&#xD;'#039');}'#010+
  '    else'#010+
  '      Result[J]:=S[i];'#010+
  '    end;'#010+
  '    If (J=Length(Result)) and (I<Length(S)) then'#010+
  '      SetLength(Result,J+Length(S)-I);'#010+
  '    end;'#010+
  '  If J<>Length(Result',') then'#010+
  '    SetLength(Result,J);'#010+
  'end;'#010+
  #010+
  #010+
  'function maybequoted(const s:string):string;'#010+
  'const'#010+
  '  {$IFDEF MSWINDOWS}'#010+
  '    FORBIDDEN_CHARS = ['#039'!'#039', '#039'@'#039', '#039'#'#039', '#039'$'#039+
  ', '#039'%'#039', '#039'^'#039', '#039'&'#039', '#039'*'#039', '#039'('#039', '#039+
  ')'#039','#010+
  '                       '#039'{'#039', '#039'}'#039', '#039#039#039#039', '#039'`'+
  #039', '#039'~'#039'];'#010+
  '  {$EL','SE}'#010+
  '    FORBIDDEN_CHARS = ['#039'!'#039', '#039'@'#039', '#039'#'#039', '#039'$'#039+
  ', '#039'%'#039', '#039'^'#039', '#039'&'#039', '#039'*'#039', '#039'('#039', '#039+
  ')'#039','#010+
  '                       '#039'{'#039', '#039'}'#039', '#039#039#039#039', '#039':'+
  #039', '#039'\'#039', '#039'`'#039', '#039'~'#039'];'#010+
  '  {$ENDIF}'#010+
  'var'#010+
  '  s1 : string;'#010+
  '  i  : integer;'#010+
  '  quoted : boolean;'#010+
  'begin'#010+
  '  quoted:=false;'#010+
  '  s1:='#039'"'#039';'#010+
  '  f','or i:=1 to length(s) do'#010+
  '   begin'#010+
  '     case s[i] of'#010+
  '       '#039'"'#039' :'#010+
  '         begin'#010+
  '           quoted:=true;'#010+
  '           s1:=s1+'#039'\"'#039';'#010+
  '         end;'#010+
  '       '#039' '#039','#010+
  '       #128..#255 :'#010+
  '         begin'#010+
  '           quoted:=true;'#010+
  '           s1:=s1+s[i];'#010+
  '  ','       end;'#010+
  '       else begin'#010+
  '         if s[i] in FORBIDDEN_CHARS then'#010+
  '           quoted:=True;'#010+
  '         s1:=s1+s[i];'#010+
  '       end;'#010+
  '     end;'#010+
  '   end;'#010+
  '  if quoted then'#010+
  '    maybequoted:=s1+'#039'"'#039#010+
  '  else'#010+
  '    maybequoted:=s;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure ReadIni','File(Const AFileName: String;L:TStrings);'#010+
  'Var'#010+
  '  F : TFileStream;'#010+
  '  Line : String;'#010+
  '  I,P,PC : Integer;'#010+
  'begin'#010+
  '  // On some file systems and when using a large number of parallel ma'+
  'ke'#010+
  '  // processes, the lock from the creation of the ini file ','may not y'+
  'et'#010+
  '  // have been released even though the file has been closed already'#010+
  '  F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyNone);'#010+
  '  Try'#010+
  '    L.LoadFromStream(F);'#010+
  '    // Fix lines.'#010+
  '    For I:=L.Count-1 downto 0 do'#010+
  '      begin',#010+
  '        Line:=L[I];'#010+
  '        P:=Pos('#039'='#039',Line);'#010+
  '        PC:=Pos('#039';'#039',Line);  // Comment line.'#010+
  '        If (P=0) or ((PC<>0) and (PC<P)) then'#010+
  '          L.Delete(I)'#010+
  '        else'#010+
  '          L[i]:=Trim(System.Copy(Line,1,P-1)+'#039'='#039'+Trim(System.C'+
  'opy(L','ine,P+1,Length(Line)-P)));'#010+
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
  '  Result:=OSToStri','ng(Defaults.OS);'#010+
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
  'Function OSesToString(OSes: ','TOSes) : String;'#010+
  #010+
  'var'#010+
  '  i: TOS;'#010+
  '  Tmp: String;'#010+
  '  First: boolean;'#010+
  #010+
  'begin'#010+
  '  { can'#039't use SetToString, there are more than 32 OSes }'#010+
  '  First:=true;'#010+
  '  for i in OSes do'#010+
  '    begin'#010+
  '      if not First then'#010+
  '        Result:=Result+'#039','#039#010+
  '      else'#010+
  '     ','   First:=false;'#010+
  '      Str(i,Tmp);'#010+
  '      Result:=Result+Tmp;'#010+
  '    end;'#010+
  '  Result:=LowerCase(Tmp);'#010+
  'end;'#010+
  #010+
  'Function CPUToString(CPU: TCPU) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=LowerCase(GetenumName(TypeInfo(TCPU),Ord(CPU)));'#010+
  'end;'#010+
  #010+
  'Function CPUSToString(CP','US: TCPUS) : String;'#010+
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
  '  I:=GetEnumValue(TypeInfo(TOS),S);'#010+
  '  if (I=-1) then',#010+
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
  '*)'#010+
  #010+
  'function IsDifferentFromBuild(A','Cpu: TCPU; AOs: TOs): boolean;'#010+
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
  '  I:=GetEnumValue(TypeInfo(TCPU),S);'#010+
  '  if (I=-1) then'#010+
  '   ',' Raise EInstallerError.CreateFmt(SErrInvalidCPU,[S]);'#010+
  '  Result:=TCPU(I);'#010+
  'end;'#010+
  #010+
  'Function StringToCPUS(const S : String) : TCPUS;'#010+
  #010+
  'begin'#010+
  '  Result:=TCPUS(StringToSet(PTypeInfo(TypeInfo(TCPUS)),S));'#010+
  'end;'#010+
  #010+
  'Function ModeToString(Mode: TCompilerMo','de) : String;'#010+
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
  '  I:=GetEnumValue(TypeInfo(TCompilerMo','de),S);'#010+
  '  if (I=-1) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidMode,[S]);'#010+
  '  Result:=TCompilerMode(I);'#010+
  'end;'#010+
  #010+
  #010+
  'Function MakeTargetString(CPU : TCPU;OS: TOS) : String;'#010+
  #010+
  'begin'#010+
  '  if OS in AllLimit83fsOses then'#010+
  '    Result := OSToString(O','S)'#010+
  '  else'#010+
  '    Result:=CPUToString(CPU)+'#039'-'#039'+OSToString(OS);'#010+
  'end;'#010+
  #010+
  'Procedure StringToCPUOS(const S : String; Var CPU : TCPU; Var OS: TOS)'+
  ';'#010+
  #010+
  'Var'#010+
  '  P : integer;'#010+
  #010+
  'begin'#010+
  '  P:=Pos('#039'-'#039',S);'#010+
  '  If (P=0) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrInv','alidTarget,[S]);'#010+
  '  CPU:=StringToCPU(Copy(S,1,P-1));'#010+
  '  OS:=StringToOs(Copy(S,P+1,Length(S)-P));'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure ResolveDependencies(L : TDependencies; P : TNamedCollection)'+
  ';'#010+
  'Var'#010+
  '  I,J : Integer;'#010+
  '  C : TDependency;'#010+
  'begin'#010+
  '  If Assigned(L) the','n'#010+
  '    For I:=0 to L.Count-1 do'#010+
  '      begin'#010+
  '        C:=L[i];'#010+
  '        if C.DependencyType in [depPackage,depUnit] then'#010+
  '          begin'#010+
  '            J:=P.IndexOfName(C.Value);'#010+
  '            If J<>-1 then'#010+
  '              C.Target:=P.Items[J];'#010+
  '      ','    end;'#010+
  '      end;'#010+
  'end;'#010+
  #010+
  #010+
  'function AddConditionalStrings(APackage: TPackage; Dest : TStrings; Sr'+
  'c : TConditionalStrings;ACPU:TCPU;AOS:TOS; Const APrefix : String='#039+
  #039') : Integer ;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  C : TConditionalString;'#010+
  '  D : TPackageDi','ctionary;'#010+
  '  S : String;'#010+
  'begin'#010+
  '  Result:=0;'#010+
  '  D := PackageDictionaryClass.Create(nil);'#010+
  '  D.MasterDictionary := APackage.Dictionary;'#010+
  '  try'#010+
  '    D.AddVariable('#039'CPU'#039',CPUToString(ACPU));'#010+
  '    D.AddVariable('#039'OS'#039',OSToString(AOS));'#010+
  '    For I:=0 to Sr','c.Count-1 do'#010+
  '      begin'#010+
  '        C:=Src[I];'#010+
  '        if (ACPU in C.CPUs) and (AOS in C.OSes) then'#010+
  '          begin'#010+
  '            If (APrefix<>'#039#039') then'#010+
  '              S:=APrefix+C.Value'#010+
  '            else'#010+
  '              S:=C.Value;'#010+
  '            S := ','D.ReplaceStrings(s);'#010+
  '            if C is TConditionalDestString then'#010+
  '              begin'#010+
  '                // If a destination path is given, omit the path of th'+
  'e sourcefile'#010+
  '                if TConditionalDestString(c).DestPath='#039#039' then'#010+
  '      ','            Dest.values[S] :=  D.ReplaceStrings(IncludeTrailin'+
  'gPathDelimiter(TConditionalDestString(c).DestPath))+S'#010+
  '                else'#010+
  '                  Dest.values[S] :=  D.ReplaceStrings(IncludeTrailingP'+
  'athDelimiter(TConditionalDestStri','ng(c).DestPath)+APrefix+ExtractFile'+
  'Name(C.Value));'#010+
  '              end'#010+
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
  'function FileListToString(List : TString','s; const APrefix : String) :'+
  ' String;'#010+
  'Var'#010+
  '  I : integer;'#010+
  '  S : String;'#010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  For I:=0 to List.Count-1 do'#010+
  '    begin'#010+
  '      If (I>0) then'#010+
  '        Result:=Result+'#039' '#039';'#010+
  '      S:=APrefix+List[i];'#010+
  '      If (Pos('#039' '#039',S)<>0) then'#010+
  '      ','  S:='#039'"'#039'+S+'#039'"'#039';'#010+
  '      Result:=Result+S;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'function FixPath (const APath : String) : String;'#010+
  'begin'#010+
  '  Result := FixPath(APath, False);'#010+
  'end;'#010+
  #010+
  'function FixPath (const APath : String; AIsDir : Boolean) : String;'#010+
  'Var'#010+
  '  P : PChar;'#010+
  'be','gin'#010+
  '  Result:=APath;'#010+
  '  If (result<>'#039#039') then'#010+
  '    begin'#010+
  '      UniqueString(Result);'#010+
  '      P:=PChar(Result);'#010+
  '      While (P^<>#0) do'#010+
  '        begin'#010+
  '          If P^ in ['#039'/'#039','#039'\'#039'] then'#010+
  '            P^:=PathDelim;'#010+
  '          Inc(P);'#010+
  '        end;'#010+
  '    ','end;'#010+
  '  if AIsDir and (Result <> '#039#039') then'#010+
  '    Result := IncludeTrailingPathDelimiter(Result);;'#010+
  'end;'#010+
  #010+
  'function IsRelativePath(const APath: String): boolean;'#010+
  'begin'#010+
  '  if APath='#039#039' then'#010+
  '    result := true'#010+
  '{$ifdef unix}'#010+
  '  else if APath[1] in Allow','DirectorySeparators then'#010+
  '    result := false'#010+
  '{$else}'#010+
  '  else if ExtractFileDrive(APath)<>'#039#039' then'#010+
  '    result := false'#010+
  '{$endif}'#010+
  '  else'#010+
  '    result := true;'#010+
  'end;'#010+
  #010+
  'procedure ChangeDir(const APath : String);'#010+
  'begin'#010+
  '  if Not SetCurrentDir(APath) the','n'#010+
  '    Raise EInstallerError.CreateFmt(SErrChangeDirFailed,[APath]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure SearchFiles(const AFileName: string; Recursive: boolean; var'+
  ' List: TStrings);'#010+
  #010+
  '  procedure AddRecursiveFiles(const SearchDir, FileMask: string; Recur'+
  'sive: ','boolean);'#010+
  '  var'#010+
  '    Info : TSearchRec;'#010+
  '  begin'#010+
  '    if FindFirst(SearchDir+AllFilesMask,faAnyFile and faDirectory,Info'+
  ')=0 then'#010+
  '    begin'#010+
  '      repeat'#010+
  '          if ((Info.Attr and faDirectory) = faDirectory) and (Info.Nam'+
  'e <> '#039'.'#039') and (Info.N','ame <> '#039'..'#039') and (Recursive) th'+
  'en'#010+
  '            AddRecursiveFiles(SearchDir + Info.Name + PathDelim, FileM'+
  'ask, Recursive);'#010+
  '          if ((Info.Attr and faDirectory) <> faDirectory) and IsWild(I'+
  'nfo.Name, FileMask, FileNameCaseSensitive) then'#010+
  ' ','           List.Add(SearchDir + Info.Name);'#010+
  '      until FindNext(Info)<>0;'#010+
  '    end;'#010+
  '    FindClose(Info);'#010+
  '  end;'#010+
  #010+
  'var'#010+
  '  CurrDir,'#010+
  '  BasePath: string;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  BasePath := ExtractFilePath(ExpandFileName(AFileName));'#010+
  '  AddRecursive','Files(BasePath, ExtractFileName(AFileName), Recursive)'+
  ';'#010+
  #010+
  '  CurrDir:=GetCurrentDir;'#010+
  '  for i := 0 to Pred(List.Count) do'#010+
  '    List[i] := ExtractRelativepath(IncludeTrailingPathDelimiter(CurrDi'+
  'r), List[i]);'#010+
  'end;'#010+
  #010+
  'Const'#010+
  '  WhiteSpace = [#9,#10,#1','3,'#039' '#039'];'#010+
  '  QuoteChars = ['#039#039#039#039','#039'"'#039'];'#010+
  #010+
  'procedure SplitCommand(const Cmd : String; var Exe, Options : String);'+
  #010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  '  InQuote : Boolean;'#010+
  '  LastQuote : Char;'#010+
  '  S : String;'#010+
  #010+
  'begin'#010+
  '  S:=Trim(Cmd);'#010+
  '  InQuote:=False;'#010+
  '  LastQuote:=#0;'#010,
  '  I:=1;'#010+
  '  While (I<=Length(S)) and (Inquote or not (S[I] in whiteSpace)) do'#010+
  '    begin'#010+
  '    If S[i] in QuoteChars then'#010+
  '      begin'#010+
  '      InQuote:=Not (S[i]=LastQuote);'#010+
  '      If InQuote then'#010+
  '         LastQuote:=S[i]'#010+
  '       else'#010+
  '         LastQu','ote:=#0;'#010+
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
  '  if not assigned(CustomFpmakeComman','dlineOptions) then'#010+
  '    CustomFpmakeCommandlineOptions := TStringList.Create;'#010+
  '  CustomFpmakeCommandlineOptions.Values[ACommandLineOption]:=HelpMessa'+
  'ge;'#010+
  'end;'#010+
  #010+
  'function GetCustomFpmakeCommandlineOptionValue(const ACommandLineOptio'+
  'n: string): s','tring;'#010+
  'begin'#010+
  '  if not assigned(CustomFpMakeCommandlineValues) then'#010+
  '    result := '#039#039#010+
  '  else'#010+
  '    result := CustomFpMakeCommandlineValues.Values[ACommandLineOption]'+
  ';'#010+
  'end;'#010+
  #010+
  'function AddProgramExtension(const ExecutableName: string; AOS : TOS):'+
  ' ','string;'#010+
  'begin'#010+
  '  if AOS in [Go32v2,Win32,Win64,Wince,OS2,EMX,Watcom] then'#010+
  '    Result:=ExecutableName+ExeExt'#010+
  '  else'#010+
  '    Result:=ExecutableName;'#010+
  'end;'#010+
  #010+
  'function GetImportLibraryFilename(const UnitName: string; AOS: TOS): s'+
  'tring;'#010+
  'begin'#010+
  '  if AOS ','in [go32v2,watcom,os2,emx] then'#010+
  '    Result := '#039'libimp'#039'+UnitName'#010+
  '  else if AOS in [netware,netwlibc,macos] then'#010+
  '    Result := '#039'lib'#039'+UnitName'#010+
  '  else'#010+
  '    Result := '#039'libimp'#039'+UnitName;'#010+
  '  Result := Result + LibExt;'#010+
  'end;'#010+
  #010+
  'Function OptionListToStri','ng(L : TStrings) : String;'#010+
  #010+
  'var'#010+
  '  I : Integer;'#010+
  '  S : String;'#010+
  #010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  For I:=0 to L.Count-1 do'#010+
  '    begin'#010+
  '    If (Result<>'#039#039') then'#010+
  '      Result:=Result+'#039' '#039';'#010+
  '    S:=L[I];'#010+
  '    If (Pos('#039' '#039',S)<>0) or (S='#039#039') then'#010+
  '      Result:= Resu','lt + '#039'"'#039'+S+'#039'"'#039#010+
  '    else'#010+
  '      Result:= Result + S;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'Function OptionsToStringList(S : String) : TStrings;'#010+
  #010+
  'Var'#010+
  '  P : Integer;'#010+
  #010+
  'begin'#010+
  '  Result:=Nil;'#010+
  '  If (S='#039#039') then'#010+
  '    Exit;'#010+
  '  Result:=TStringList.Create;'#010+
  '  Repeat'#010+
  '    P:=Pos('#039' ',#039',S);'#010+
  '    If P=0 then'#010+
  '      P:=Length(S)+1;'#010+
  '    Result.Add(Copy(S,1,P-1));'#010+
  '    Delete(S,1,P);'#010+
  '    S:=Trim(S);'#010+
  '  Until Length(S)=0;'#010+
  '  If Result.Count=0 then'#010+
  '    FreeAndNil(Result);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  'function GetCompilerInfo(co','nst ACompiler,AOptions:string):string;'#010+
  'const'#010+
  '  BufSize = 1024;'#010+
  'var'#010+
  '  S: TProcess;'#010+
  '  Buf: array [0..BufSize - 1] of char;'#010+
  '  Count: longint;'#010+
  'begin'#010+
  '  S:=TProcess.Create(Nil);'#010+
  '  S.Commandline:=ACompiler+'#039' '#039'+AOptions;'#010+
  '  S.Options:=[poUsePipes];'#010,
  '  S.execute;'#010+
  '  Count:=s.output.read(buf,BufSize);'#010+
  '  S.Free;'#010+
  '  SetLength(Result,Count);'#010+
  '  Move(Buf,Result[1],Count);'#010+
  'end;'#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  #010+
  'constructor TPackageVariant.Create(ACollection: TCollection);'#010+
  'begin'#010+
  '  inherited Create(AColl','ection);'#010+
  '  FTargets := TTargets.Create(TTarget);'#010+
  '  FOptions := TStringList.Create;'#010+
  'end;'#010+
  #010+
  'destructor TPackageVariant.Destroy;'#010+
  'begin'#010+
  '  FOptions.Free;'#010+
  '  FTargets.Free;'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  '{ TPackageVariants }'#010+
  #010+
  'procedure TPackageVariants','.SetDefaultPackageVariantName(AValue: stri'+
  'ng);'#010+
  'begin'#010+
  '  if FDefaultPackageVariantName=AValue then Exit;'#010+
  '  if not assigned(ItemByName(avalue)) then'#010+
  '    raise exception.CreateFmt(SErrPackVarNotExist,[AValue]);'#010+
  '  FDefaultPackageVariantName:=AVa','lue;'#010+
  'end;'#010+
  #010+
  'function TPackageVariants.GetActivePackageVariant: TPackageVariant;'#010+
  'begin'#010+
  '  result := ItemByName(ActivePackageVariantName) as TPackageVariant;'#010+
  'end;'#010+
  #010+
  'function TPackageVariants.GetDefaultPackageVariant: TPackageVariant;'#010+
  'begin'#010+
  '  res','ult := ItemByName(DefaultPackageVariantName) as TPackageVariant'+
  ';'#010+
  'end;'#010+
  #010+
  'procedure TPackageVariants.SetActivePackageVariantName(AValue: string)'+
  ';'#010+
  'begin'#010+
  '  if FActivePackageVariantName=AValue then Exit;'#010+
  '  if not assigned(ItemByName(avalue)) then',#010+
  '    raise exception.CreateFmt(SErrPackVarNotExist,[AValue]);'#010+
  '  FActivePackageVariantName:=AValue;'#010+
  'end;'#010+
  #010+
  'function TPackageVariants.Add(AName: String): TPackageVariant;'#010+
  'begin'#010+
  '  result := self.add as TPackageVariant;'#010+
  '  result.Name := AName;'#010+
  ' ',' if FDefaultPackageVariantName='#039#039' then'#010+
  '    FDefaultPackageVariantName:=AName;'#010+
  'end;'#010+
  #010+
  #010+
  '{ TConditionalDestStrings }'#010+
  #010+
  'function TConditionalDestStrings.GetConditionalString(Index : Integer)'+
  ': TConditionalDestString;'#010+
  'begin'#010+
  '  Result:=TConditionalDe','stString(Items[Index]);'#010+
  'end;'#010+
  #010+
  'procedure TConditionalDestStrings.SetConditionalString(Index : Integer'+
  '; AValue: TConditionalDestString);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  'constructor TConditionalDestStrings.Create(AClass: TConditionalDestSt',
  'ringClass);'#010+
  'begin'#010+
  '  inherited Create(AClass);'#010+
  'end;'#010+
  #010+
  'function TConditionalDestStrings.Add(const Value: String; ADestPath: S'+
  'tring): TConditionalDestString;'#010+
  'begin'#010+
  '  Add(Value, AllCPUs, AllOSes, ADestPath);'#010+
  'end;'#010+
  #010+
  'function TConditionalDestString','s.Add(const Value: String; const OSes'+
  ': TOSes; ADestPath: String): TConditionalDestString;'#010+
  'begin'#010+
  '  Add(Value, AllCPUs, OSes, ADestPath);'#010+
  'end;'#010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TConditionalDestStrings.Add(Const Value : String;const CPUs:T',
  'CPUs; ADestPath: String) : TConditionalDestString;inline;'#010+
  'begin'#010+
  '  Add(Value, CPUs, AllOSes, ADestPath);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'function TConditionalDestStrings.Add(const Value: String; const CPUs: '+
  'TCPUs; const OSes: TOSes; ADest','Path: String): TConditionalDestString'+
  ';'#010+
  'var'#010+
  '  ACondString: TConditionalDestString;'#010+
  'begin'#010+
  '  ACondString := inherited Add(Value,CPUs,OSes) as TConditionalDestStr'+
  'ing;'#010+
  '  ACondString.DestPath:=ADestPath;'#010+
  'end;'#010+
  #010+
  '{ TPackageDictionary }'#010+
  #010+
  'function TPa','ckageDictionary.GetValue(const AName, Args: String): Str'+
  'ing;'#010+
  'Var'#010+
  '  O : TObject;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  I:=Flist.IndexOf(AName);'#010+
  '  If (I=-1) then'#010+
  '    begin'#010+
  '      if assigned(MasterDictionary) then'#010+
  '        result := MasterDictionary.GetValue(','AName,Args)'#010+
  '      else'#010+
  '        result := GlobalDictionary.GetValue(AName,Args);'#010+
  '      Exit;'#010+
  '    end;'#010+
  '  O:=Flist.Objects[I];'#010+
  '  If O is TValueItem then'#010+
  '    Result:=TValueItem(O).FValue'#010+
  '  else'#010+
  '    Result:=TFunctionItem(O).FFunc(AName,Args);'#010+
  'en','d;'#010+
  #010+
  '{$ifndef NO_THREADING}'#010+
  #010+
  '{ TCompileWorkerThread }'#010+
  #010+
  'constructor TCompileWorkerThread.Create(ABuildEngine: TBuildEngine; No'+
  'tifyMainThreadEvent: PRTLEvent);'#010+
  'begin'#010+
  '  inherited Create(false);'#010+
  '  FNotifyStartTask := RTLEventCreate;'#010+
  '  FBuildEngi','ne := ABuildEngine;'#010+
  '  FNotifyMainThreadEvent:=NotifyMainThreadEvent;'#010+
  'end;'#010+
  #010+
  'destructor TCompileWorkerThread.Destroy;'#010+
  'begin'#010+
  '  RTLeventdestroy(FNotifyStartTask);'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  'procedure TCompileWorkerThread.execute;'#010+
  'begin'#010+
  '  while ','not Terminated do'#010+
  '    begin'#010+
  '    FDone:=true;'#010+
  '    RTLeventSetEvent(FNotifyMainThreadEvent);'#010+
  '    RTLeventWaitFor(FNotifyStartTask,500);'#010+
  '    if not FDone then'#010+
  '      begin'#010+
  '      FBuildEngine.log(vlInfo,'#039'Compiling: '#039'+APackage.Name);'#010+
  '      FCompi','lationOK:=false;'#010+
  '      try'#010+
  '        FBuildEngine.Compile(APackage);'#010+
  '        FCompilationOK:=true;'#010+
  '      except'#010+
  '        on E: Exception do'#010+
  '          FErrorMessage := E.Message;'#010+
  '      end;'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  '{$endif NO_THREADING}'#010+
  #010+
  '{****','***************************************************************'+
  '*********'#010+
  '                           TUnsortedDuplicatesStringList'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TUnsortedDuplicatesStr','ingList.Add(const S: string): Integer'+
  ';'#010+
  #010+
  'begin'#010+
  '  result := IndexOf(S);'#010+
  '  If result > -1 then'#010+
  '    Case DUplicates of'#010+
  '      DupIgnore : Exit;'#010+
  '      DupError : Error(SDuplicateString,0)'#010+
  '    end;'#010+
  '  inherited Add(S);'#010+
  'end;'#010+
  #010+
  '{**********************','*********************************************'+
  '*********'#010+
  '                                TNamedItem'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'procedure TNamedItem.SetName(const AValue: String);'#010+
  #010+
  'begin'#010+
  '  if',' FName=AValue then exit;'#010+
  '  With TNamedCollection(Collection) do'#010+
  '    If UniqueNames then'#010+
  '      If (IndexOfName(AVAlue)<>-1) then'#010+
  '        Raise ECollectionError.CreateFmt(SErrNameExists,[AValue]);'#010+
  '  FName:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  '{********************','***********************************************'+
  '*********'#010+
  '                                TNamedCollection'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TNamedCollection.GetItem(Index: Integer): TNam','edItem;'#010+
  'begin'#010+
  '  result := TNamedItem(inherited getItem(index));'#010+
  'end;'#010+
  #010+
  'procedure TNamedCollection.SetItem(Index: Integer; AValue: TNamedItem)'+
  ';'#010+
  'begin'#010+
  '  inherited SetItem(Index, AValue);'#010+
  'end;'#010+
  #010+
  'function TNamedCollection.IndexOfName(const AName:',' String): Integer;'+
  #010+
  #010+
  'begin'#010+
  '  Result:=Count-1;'#010+
  '  While (Result>=0) and (CompareText(TNamedItem(Items[Result]).FName,A'+
  'Name)<>0) do'#010+
  '    Dec(Result);'#010+
  'end;'#010+
  #010+
  'function TNamedCollection.ItemByName(const AName: String): TNamedItem;'+
  #010+
  #010+
  'Var'#010+
  '  I : Integer',';'#010+
  #010+
  'begin'#010+
  '  I:=IndexOfName(AName);'#010+
  '  If (I=-1) Then'#010+
  '    Raise ECollectionError.CreateFmt(SErrNoSuchName,[AName]);'#010+
  '  Result:=TNamedItem(Items[i]);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '          ','                   TNamedItemList'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TNamedItemList.GetNamedItem(Index : Integer): TNamedItem;'#010+
  'begin'#010+
  '  Result:=TNamedItem(Items[Index]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure T','NamedItemList.SetNamedItem(Index : Integer; const AValue:'+
  ' TNamedItem);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'function TNamedItemList.IndexOfName(const AName: String): Integer;'#010+
  'begin'#010+
  '  Result:=Count-1;'#010+
  '  While (Result>=0) and (CompareText(Get','NamedItem(Result).Name,AName'+
  ')<>0) do'#010+
  '    Dec(Result);'#010+
  'end;'#010+
  #010+
  #010+
  'function TNamedItemList.ItemByName(const ANAme: String): TNamedItem;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  I:=IndexOfName(AName);'#010+
  '  If (I=-1) Then'#010+
  '    Raise ECollectionError.CreateFmt(SErrNo','SuchName,[AName]);'#010+
  '  Result:=TNamedItem(Items[i]);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TTargets'#010+
  '***************************************************************','*****'+
  '********}'#010+
  #010+
  'function TTargets.GetTargetItem(Index : Integer): TTarget;'#010+
  'begin'#010+
  '  Result:=TTarget(Items[Index]);'#010+
  'end;'#010+
  #010+
  #010+
  'function TTargets.GetTarget(const AName : String): TTarget;'#010+
  'begin'#010+
  '  Result:=TTarget(ItemByName(AName));'#010+
  'end;'#010+
  #010+
  #010+
  'procedur','e TTargets.SetTargetItem(Index : Integer; const AValue: TTar'+
  'get);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  'function TTargets.AddFPDoc(const AUnitName, AXMLName: String): TTarget'+
  ';'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AUnitName;'#010+
  '  Result','.XML:=AXMLName;'#010+
  '  Result.TargetType:=ttFPDoc;'#010+
  'end;'#010+
  #010+
  'Function TTargets.AddUnit(Const AUnitName : String) : TTarget;'#010+
  'begin'#010+
  '  Result:=AddUnit(AUnitName,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddUnit(Const AUnitName : String;const OSes:TOSe','s)'+
  ' : TTarget;'#010+
  'begin'#010+
  '  Result:=AddUnit(AUnitName,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TTargets.AddUnit(Const AUnitName : String;const CPUs:TCPUs) :'+
  ' TTarget;'#010+
  'begin'#010+
  '  Result:=AddUnit(AUnitName,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endif ','cpu_only_overloads}'#010+
  #010+
  #010+
  'Function TTargets.AddUnit(Const AUnitName : String;const CPUs:TCPUs;co'+
  'nst OSes:TOSes) : TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AUnitName;'#010+
  '  Result.TargetType:=TTUnit;'#010+
  '  Result.CPUs:=CPUs;'#010+
  '  Result.OSes',':=OSes;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddImplicitUnit(Const AUnitName : String;InstallUnit'+
  ':boolean=true) : TTarget;'#010+
  'begin'#010+
  '  Result:=AddImplicitUnit(AUnitName,AllCPUs,AllOSes,InstallUnit);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddImplicitUnit(Const AUnitName ',': String;const OSe'+
  's:TOSes;InstallUnit:boolean=true) : TTarget;'#010+
  'begin'#010+
  '  Result:=AddImplicitUnit(AUnitName,AllCPUs,OSes,InstallUnit);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddImplicitUnit(Const AUnitName : String;const CPUs:'+
  'TCPUs;InstallUnit:boolean=true)',' : TTarget;'#010+
  'begin'#010+
  '  Result:=AddImplicitUnit(AUnitName,CPUs,AllOSes,InstallUnit);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddImplicitUnit(Const AUnitName : String;const CPUs:'+
  'TCPUs;const OSes:TOSes;InstallUnit:boolean=true) : TTarget;'#010+
  'begin'#010+
  '  Result:=Add as',' TTarget;'#010+
  '  Result.Name:=AUnitName;'#010+
  '  Result.CPUs:=CPUs;'#010+
  '  Result.OSes:=OSes;'#010+
  '  if InstallUnit then'#010+
  '    Result.TargetType:=TTImplicitUnit'#010+
  '  else'#010+
  '    Result.TargetType:=TTCleanOnlyUnit;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddProgram(Const AProgramName ',': String) : TTarget;'+
  #010+
  'begin'#010+
  '  Result:=AddProgram(AProgramName,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddProgram(Const AProgramName : String;const OSes:TO'+
  'Ses) : TTarget;'#010+
  'begin'#010+
  '  Result:=AddProgram(AProgramName,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifde','f cpu_only_overloads}'#010+
  'Function TTargets.AddProgram(Const AProgramName : String;const CPUs:TC'+
  'PUs) : TTarget;'#010+
  'begin'#010+
  '  Result:=AddProgram(AProgramName,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'Function TTargets.AddProgram(Const AProgra','mName : String;const CPUs:'+
  'TCPUs;const OSes:TOSes) : TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AProgramName;'#010+
  '  Result.CPUs:=CPUs;'#010+
  '  Result.OSes:=OSes;'#010+
  '  Result.TargetType:=ttProgram;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddExampleUnit(Cons','t AUnitName : String) : TTarget'+
  ';'#010+
  'begin'#010+
  '  Result:=AddExampleUnit(AUnitName,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddExampleUnit(Const AUnitName : String;const OSes:T'+
  'OSes) : TTarget;'#010+
  'begin'#010+
  '  Result:=AddExampleUnit(AUnitName,AllCPUs,OSes)',';'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TTargets.AddExampleUnit(Const AUnitName : String;const CPUs:T'+
  'CPUs) : TTarget;'#010+
  'begin'#010+
  '  Result:=AddExampleUnit(AUnitName,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'Function TTargets.AddExam','pleUnit(Const AUnitName : String;const CPUs'+
  ':TCPUs;const OSes:TOSes) : TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AUnitName;'#010+
  '  Result.CPUs:=CPUs;'#010+
  '  Result.OSes:=OSes;'#010+
  '  Result.TargetType:=ttExampleUnit;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.','AddExampleProgram(Const AProgramName : String) : T'+
  'Target;'#010+
  'begin'#010+
  '  Result:=AddExampleProgram(AProgramName,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddExampleProgram(Const AProgramName : String;const '+
  'OSes:TOSes) : TTarget;'#010+
  'begin'#010+
  '  Result:=A','ddExampleProgram(AProgramName,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TTargets.AddExampleProgram(Const AProgramName : String;const '+
  'CPUs:TCPUs) : TTarget;'#010+
  'begin'#010+
  '  Result:=AddExampleProgram(AProgramName,CPUs,AllOSes);'#010+
  'end;'#010,
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'Function TTargets.AddExampleProgram(Const AProgramName : String;const '+
  'CPUs:TCPUs;const OSes:TOSes) : TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AProgramName;'#010+
  '  Result.CPUs:=CPUs;'#010+
  '  Result.OSes:=OSe','s;'#010+
  '  Result.TargetType:=ttExampleProgram;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TSources'#010+
  '**********************************************************************'+
  '**','****}'#010+
  #010+
  'function TSources.GetSourceItem(Index : Integer): TSource;'#010+
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
  'function TSources','.AddDoc (const AFiles : String) : TSource;'#010+
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
  '  Result:=Add a','s TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.FInstallSourcePath:=AInstallSourcePath;'#010+
  '  Result.FSourceType:=stDoc;'#010+
  'end;'#010+
  #010+
  #010+
  'function TSources.AddSrc(const AFiles : String) : TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Resul','t.FSourceType:=stSrc;'#010+
  'end;'#010+
  #010+
  #010+
  'function TSources.AddExample(const AFiles : String) : TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.FSourceType:=stExample;'#010+
  'end;'#010+
  #010+
  'function TSources.AddExample(const AFiles: String; AIn','stallSourcePat'+
  'h: String): TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.FInstallSourcePath:=AInstallSourcePath;'#010+
  '  Result.FSourceType:=stExample;'#010+
  'end;'#010+
  #010+
  #010+
  'function TSources.AddTest(const AFiles : String) : TSource;'#010+
  'b','egin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.FSourceType:=stTest;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TSources.AddDocFiles(const AFileMask: string; Recursive: boo'+
  'lean; AInstallSourcePath : String = '#039#039');'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'b','egin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, Recursive, List);'#010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '    AddDoc(List[i], AInstallSourcePath);'#010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TSources.AddSrcFiles(const AFileMask: string; Recursive:',' b'+
  'oolean);'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, Recursive, List);'#010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '    AddSrc(List[i]);'#010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TSources.AddExampleFiles(co','nst AFileMask: string; Recursiv'+
  'e: boolean; AInstallSourcePath : String = '#039#039');'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, Recursive, List);'#010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '    AddExam','ple(List[i], AInstallSourcePath);'#010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TSources.AddTestFiles(const AFileMask: string; Recursive: bo'+
  'olean);'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, Recursive,',' List);'#010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '    AddTest(List[i]);'#010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                             TPackage'#010+
  '***************************************','*****************************'+
  '********}'#010+
  #010+
  'constructor TPackage.Create(ACollection: TCollection);'#010+
  'begin'#010+
  '  inherited Create(ACollection);'#010+
  '  FVersion:=TFPVersion.Create;'#010+
  '  FTargets:=TTargets.Create(TTarget);'#010+
  '  FSources:=TSources.Create(TSource);',#010+
  '  FDependencies:=TDependencies.Create(TDependency);'#010+
  '  FInstallFiles:=TConditionalDestStrings.Create(TConditionalDestString'+
  ');'#010+
  '  FCleanFiles:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FUnitPath:=TConditionalStrings.Create(TConditiona','lString);'#010+
  '  FObjectPath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FIncludePath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FSourcePath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FExamplePath:=TConditionalStrings.Crea','te(TConditionalString);'#010+
  '  FTestPath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FCommands:=TCommands.Create(TCommand);'#010+
  '  FUnitsOutputDir:='#039'units'#039'+PathDelim+'#039'$(target)'#039'+PathD'+
  'elim;'#010+
  '  FPackageVariants:=TFPList.Create;'#010+
  '  FCPUs:=AllCPUs;'#010,
  '  FOSes:=AllOSes;'#010+
  '  FInstalledChecksum:=$ffffffff;'#010+
  '  FFlags := TStringList.Create;'#010+
  '  // Implicit dependency on RTL'#010+
  '  FDependencies.Add('#039'rtl'#039');'#010+
  '  FSupportBuildModes:=[bmBuildUnit, bmOneByOne];'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TPackage.destroy;'#010+
  'var'#010+
  '  i: int','eger;'#010+
  'begin'#010+
  '  FreeAndNil(FDictionary);'#010+
  '  FreeAndNil(FDependencies);'#010+
  '  FreeAndNil(FInstallFiles);'#010+
  '  FreeAndNil(FCleanFiles);'#010+
  '  FreeAndNil(FIncludePath);'#010+
  '  FreeAndNil(FSourcePath);'#010+
  '  FreeAndNil(FExamplePath);'#010+
  '  FreeAndNil(FTestPath);'#010+
  '  FreeAn','dNil(FCommands);'#010+
  '  FreeAndNil(FObjectPath);'#010+
  '  FreeAndNil(FUnitPath);'#010+
  '  FreeAndNil(FSources);'#010+
  '  FreeAndNil(FTargets);'#010+
  '  FreeAndNil(FVersion);'#010+
  '  FreeAndNil(FOptions);'#010+
  '  FreeAndNil(FFlags);'#010+
  '  FreeAndNil(FPackageVariants);'#010+
  '  inherited destroy;'#010,
  'end;'#010+
  #010+
  'function TPackage.HaveOptions: Boolean;'#010+
  'begin'#010+
  '  Result:=(FOptions<>Nil);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.SetName(const AValue: String);'#010+
  'begin'#010+
  '  inherited SetName(AValue);'#010+
  '  // RTL should not have any dependencies'#010+
  '  if AValue='#039'rtl'#039' then'#010+
  '    ','FDependencies.Clear;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TPackage.GetUnitsOutputDir(ACPU:TCPU; AOS : TOS):String;'#010+
  'begin'#010+
  '  result:=FixPath(Dictionary.Substitute(FUnitsOutputDir,['#039'CPU'#039','+
  'CPUToString(ACPU),'#039'OS'#039',OSToString(AOS),'#039'target'#039',MakeTa'+
  'rgetString(ACPU,AOS)]),',' True);'#010+
  'end;'#010+
  #010+
  'function TPackage.GetUnitConfigOutputDir(ACPU: TCPU; AOS: TOS): String'+
  ';'#010+
  'begin'#010+
  '  result:=FixPath(Dictionary.Substitute('#039'units'#039'+PathDelim+'#039+
  '$(target)'#039'+PathDelim,['#039'CPU'#039',CPUToString(ACPU),'#039'OS'#039','+
  'OSToString(AOS),'#039'target'#039',MakeTargetStr','ing(ACPU,AOS)]), True)'+
  ';'#010+
  'end;'#010+
  #010+
  'procedure TPackage.InheritPackageVariantsFromDependency(ADependencyPac'+
  'kage: TPackage);'#010+
  'var'#010+
  '  i: integer;'#010+
  '  APackageVariants: TPackageVariants;'#010+
  'begin'#010+
  '  for i := 0 to ADependencyPackage.FPackageVariants.Count-1 d','o'#010+
  '    begin'#010+
  '      APackageVariants := TPackageVariants(ADependencyPackage.FPackage'+
  'Variants[i]);'#010+
  '      if APackageVariants.IsInheritable then'#010+
  '        begin'#010+
  '        if not assigned(GetPackageVariantsByName(APackageVariants.Name'+
  ')) then'#010+
  '       ','   begin'#010+
  '          FPackageVariants.Add(APackageVariants);'#010+
  '          end;'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function TPackage.GetPackageVariantsByName(AName: string): TPackageVar'+
  'iants;'#010+
  'var'#010+
  '  i: Integer;'#010+
  'begin'#010+
  '  result := nil;'#010+
  '  for i := 0 to FPac','kageVariants.Count-1 do'#010+
  '    if SameText(TPackageVariants(FPackageVariants.Items[i]).Name, ANam'+
  'e) then'#010+
  '      begin'#010+
  '      result := TPackageVariants(FPackageVariants.Items[i]);'#010+
  '      break;'#010+
  '      end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SetUnitsOutputDi','r(AValue: string);'#010+
  'begin'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FUnitsOutputDir:=IncludeTrailingPathDelimiter(AValue)'#010+
  '  else'#010+
  '    FUnitsOutputDir:='#039#039';'#010+
  'end;'#010+
  #010+
  'function TPackage.GetPackageUnitInstallDir(ACPU: TCPU; AOS: TOS): Stri'+
  'ng;'#010+
  'begin'#010+
  '  result:=FixPath(','Dictionary.Substitute(FPackageUnitInstallDir,['#039+
  'CPU'#039',CPUToString(ACPU),'#039'OS'#039',OSToString(AOS),'#039'target'#039+
  ',MakeTargetString(ACPU,AOS)]), True);'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SetPackageUnitInstallDir(AValue: string);'#010+
  'begin'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FPack','ageUnitInstallDir:=IncludeTrailingPathDelimiter(AValue)'#010+
  '  else'#010+
  '    FPackageUnitInstallDir:='#039#039';'#010+
  'end;'#010+
  #010+
  #010+
  'Function TPackage.GetBinOutputDir(ACPU:TCPU; AOS : TOS) : String;'#010+
  'begin'#010+
  '  Result:='#039'bin'#039'+PathDelim+MakeTargetString(ACPU,AOS);'#010+
  'end;'#010+
  #010+
  #010+
  'proce','dure TPackage.GetCleanFiles(List: TStrings; ACPU:TCPU; AOS : TO'+
  'S);'#010+
  'Var'#010+
  '  OB,OU : String;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  OB:=IncludeTrailingPathDelimiter(GetBinOutputDir(ACPU,AOS));'#010+
  '  OU:=IncludeTrailingPathDelimiter(GetUnitsOutputDir(ACPU,AOS));'#010+
  '  ','AddConditionalStrings(Self, List,CleanFiles,ACPU,AOS);'#010+
  '  For I:=0 to FTargets.Count-1 do'#010+
  '    FTargets.TargetItems[I].GetCleanFiles(List, OU, OB, ACPU, AOS);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.GetInstallFiles(List: TStrings;Types : TTargetTypes'+
  ';ACPU:','TCPU; AOS : TOS);'#010+
  'Var'#010+
  '  OB,OU : String;'#010+
  '  I : Integer;'#010+
  '  T : TTarget;'#010+
  'begin'#010+
  '  if Types=[] then'#010+
  '    AddConditionalStrings(Self, List,InstallFiles,ACPU,AOS)'#010+
  '  else'#010+
  '    begin'#010+
  '      OB:=IncludeTrailingPathDelimiter(GetBinOutputDir(Defaults.CPU,','D'+
  'efaults.OS));'#010+
  '      OU:=IncludeTrailingPathDelimiter(GetUnitsOutputDir(Defaults.CPU,'+
  'Defaults.OS));'#010+
  '      For I:=0 to FTargets.Count-1 do'#010+
  '        begin'#010+
  '          T:=FTargets.TargetItems[I];'#010+
  '          if (T.TargetType in Types) and Installer','.BuildEngine.Targe'+
  'tInstallOK(T, ACPU, AOS) then'#010+
  '            T.GetInstallFiles(List, OU, OB, ACPU, AOS);'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.GetInstallSourceFiles(List: TStrings; SourceTypes :'+
  ' TSourceTypes; TargetTypes : TTargetT','ypes);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  S : TSource;'#010+
  '  T : TTarget;'#010+
  'begin'#010+
  '  For I:=0 to FSources.Count-1 do'#010+
  '    begin'#010+
  '      S:=FSources.SourceItems[I];'#010+
  '      if (S.SourceType in SourceTypes) then'#010+
  '        S.GetInstallFiles(List);'#010+
  '    end;'#010+
  '  For I:=0 to ','FTargets.Count-1 do'#010+
  '    begin'#010+
  '      T:=FTargets.TargetItems[I];'#010+
  '      if (T.TargetType in TargetTypes) then'#010+
  '        T.GetArchiveFiles(List,Defaults.CPU,Defaults.OS);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.GetArchiveFiles(List: TStrings; ACPU:TC','PU; AOS : '+
  'TOS);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  // Targets only'#010+
  '  For I:=0 to FTargets.Count-1 do'#010+
  '    FTargets.TargetItems[I].GetArchiveFiles(List,ACPU,AOS);'#010+
  'end;'#010+
  #010+
  'procedure TPackage.GetArchiveSourceFiles(List: TStrings);'#010+
  'var'#010+
  '  i : integer;'#010+
  'begi','n'#010+
  '  for i := 0 to Sources.Count-1 do'#010+
  '    List.Add(Sources[i].Name);'#010+
  'end;'#010+
  #010+
  'Function TPackage.GetDescription : string;'#010+
  'Var'#010+
  '  FN : String;'#010+
  '  L : TStringList;'#010+
  'begin'#010+
  '  If (FDescription<>'#039#039') then'#010+
  '    Result:=FDescription'#010+
  '  else'#010+
  '    If (FDescripti','onFile<>'#039#039') then'#010+
  '      begin'#010+
  '        // Always relative to binary name.'#010+
  '        FN:=ExtractFilePath(ParamStr(0));'#010+
  '        FN:=FN+FDescriptionFile;'#010+
  '        If FileExists(FN) then'#010+
  '          begin'#010+
  '            L:=TStringList.Create;'#010+
  '           ',' Try'#010+
  '              L.LoadFromFile(FN);'#010+
  '              Result:=L.Text;'#010+
  '            Finally'#010+
  '              L.Free;'#010+
  '            end;'#010+
  '          end;'#010+
  '      end;'#010+
  'end;'#010+
  #010+
  'function TPackage.GetDictionary: TDictionary;'#010+
  'begin'#010+
  '  if not assigned(FDictionar','y) then'#010+
  '    FDictionary:=PackageDictionaryClass.Create(Nil);'#010+
  '  result := FDictionary;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TPackage.GetVersion : string;'#010+
  'begin'#010+
  '  result:=FVersion.AsString;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SetOptions(const AValue: TStrings);'#010+
  'begin'#010+
  '  If (','AValue=Nil) or (AValue.Count=0) then'#010+
  '    FreeAndNil(Foptions)'#010+
  '  else'#010+
  '    Options.Assign(AValue);'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure TPackage.SetVersion(const V : string);'#010+
  'begin'#010+
  '  FVersion.AsString:=V;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TPackage.GetFileName : string;'#010+
  'begin'#010+
  '  If',' (FFileName<>'#039#039') then'#010+
  '    Result:=FFileName'#010+
  '  else'#010+
  '    if not FVersion.Empty then'#010+
  '      Result := Name + '#039'-'#039' + FVersion.AsString'#010+
  '    else'#010+
  '      Result := Name;'#010+
  'end;'#010+
  #010+
  'function TPackage.GetOptions: TStrings;'#010+
  'begin'#010+
  '  If (FOptions=Nil) then'#010+
  '   ',' FOptions:=TStringList.Create;'#010+
  '  Result:=FOptions;'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure TPackage.GetManifest(Manifest : TStrings);'#010+
  #010+
  '  procedure AddOSes(const AIndent:string;AOSes:TOSes);'#010+
  '  var'#010+
  '    IOS : TOS;'#010+
  '  begin'#010+
  '    if (AOSes=AllOSes) then'#010+
  '      exit;'#010+
  '    ','Manifest.Add(AIndent+'#039'<oses>'#039');'#010+
  '    for IOS:=low(TOSes) to high(TOSes) do'#010+
  '      if IOS in AOSes then'#010+
  '        Manifest.Add(Format(AIndent+'#039' <os name="%s"/>'#039',[OSToSt'+
  'ring(IOS)]));'#010+
  '    Manifest.Add(AIndent+'#039'</oses>'#039');'#010+
  '  end;'#010+
  #010+
  '  procedure AddCPU','s(const AIndent:string;ACPUs:TCPUs);'#010+
  '  var'#010+
  '    ICPU : TCPU;'#010+
  '  begin'#010+
  '    if (ACPUs=AllCPUs) then'#010+
  '      exit;'#010+
  '    Manifest.Add(AIndent+'#039'<cpus>'#039');'#010+
  '    for ICPU:=low(TCPUs) to high(TCPUs) do'#010+
  '      if ICPU in ACPUs then'#010+
  '        Manifest.Add(Form','at(AIndent+'#039' <cpu name="%s"/>'#039',[CPU'+
  'ToString(ICPU)]));'#010+
  '    Manifest.Add(AIndent+'#039'</cpus>'#039');'#010+
  '  end;'#010+
  #010+
  '  function GetXMLVersionString(sMajor, sMinor, sMicro, sBuild: integer'+
  '): string;'#010+
  '  begin'#010+
  '    Result := '#039'<version'#039';'#010+
  '    if sMajor <> -1 then'#010+
  ' ','     Result := Result + '#039' major="'#039' + IntToStr(sMajor) + '#039+
  '"'#039';'#010+
  '    if sMinor <> -1 then'#010+
  '      Result := Result + '#039' minor="'#039' + IntToStr(sMinor) + '#039'"'+
  #039';'#010+
  '    if sMicro <> -1 then'#010+
  '      Result := Result + '#039' micro="'#039' + IntToStr(sMicro) + '#039'"'+
  #039';'#010+
  '    i','f sBuild <> -1 then'#010+
  '      Result := Result + '#039' build="'#039' + IntToStr(sBuild) + '#039'"'+
  #039';'#010+
  #010+
  '    Result := Result + '#039'/>'#039';'#010+
  '  end;'#010+
  #010+
  'Var'#010+
  '  S : String;'#010+
  '  i : Integer;'#010+
  '  D : TDependency;'#010+
  'begin'#010+
  '  With Manifest do'#010+
  '    begin'#010+
  '    Add(Format('#039'<package name="%s','">'#039',[QuoteXml(Name)]));'#010+
  #010+
  '    Add('#039' '#039' + GetXMLVersionString(FVersion.Major,FVersion.Mino'+
  'r,FVersion.Micro,FVersion.Build));'#010+
  '    AddOSes('#039' '#039',OSes);'#010+
  '    AddCPUs('#039' '#039',CPUs);'#010+
  '    Add(Format('#039' <filename>%s</filename>'#039',[QuoteXml(FileName +'+
  ' ZipExt)])',');'#010+
  '    Add(Format('#039' <author>%s</author>'#039',[QuoteXml(Author)]));'#010+
  '    Add(Format('#039' <license>%s</license>'#039',[QuoteXml(License)]));'+
  #010+
  '    if HomepageURL<>'#039#039' then'#010+
  '      Add(Format('#039' <homepageurl>%s</homepageurl>'#039',[QuoteXml(Ho'+
  'mepageURL)]));'#010+
  '    if Do','wnloadURL<>'#039#039' then'#010+
  '      Add(Format('#039' <downloadurl>%s</downloadurl>'#039',[QuoteXml(Do'+
  'wnloadURL)]));'#010+
  '    Add(Format('#039' <email>%s</email>'#039',[QuoteXMl(Email)]));'#010+
  '    S:=Description;'#010+
  '    If (S<>'#039#039') then'#010+
  '      Add(Format('#039' <description>%s</description','>'#039',[QuoteXML('+
  'S)]));'#010+
  '    If (Dependencies.Count>0) then'#010+
  '      begin'#010+
  '        Add('#039' <dependencies>'#039');'#010+
  '        for I:=0 to Dependencies.Count-1 do'#010+
  '          begin'#010+
  '            D:=Dependencies[i];'#010+
  '            Add('#039'  <dependency>'#039');'#010+
  '            Ad','d(Format('#039'   <package packagename="%s"/>'#039',[Quo'+
  'teXML(D.Value)]));'#010+
  '            if not D.FVersion.Empty then'#010+
  '              Add('#039'   '#039' + GetXMLVersionString(D.FVersion.Major'+
  ',D.FVersion.Minor,D.FVersion.Micro,D.FVersion.Build));'#010+
  '            AddOS','es('#039'   '#039',D.OSes);'#010+
  '            AddCPUs('#039'   '#039',D.CPUs);'#010+
  '            Add('#039'  </dependency>'#039');'#010+
  '          end;'#010+
  '        Add('#039' </dependencies>'#039');'#010+
  '      end;'#010+
  '    Add('#039'</package>'#039');'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.AddPackageVariant(APackageVariant: ','TPackageVarian'+
  'ts);'#010+
  'begin'#010+
  '  if not assigned(APackageVariant.FMasterPackage) then'#010+
  '    APackageVariant.FMasterPackage := Self;'#010+
  '  FPackageVariants.Add(APackageVariant);'#010+
  'end;'#010+
  #010+
  'procedure TPackage.ApplyPackageVariantToCompilerOptions(ACompilerOpti',
  'ons: tstrings);'#010+
  'var'#010+
  '  i: integer;'#010+
  '  PackageVariants: TPackageVariants;'#010+
  'begin'#010+
  '  for i := 0 to FPackageVariants.Count-1 do'#010+
  '    begin'#010+
  '    PackageVariants := TPackageVariants(FPackageVariants.Items[i]);'#010+
  '    ACompilerOptions.AddStrings(PackageVa','riants.ActivePackageVariant'+
  '.Options);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SetDefaultPackageVariant;'#010+
  'var'#010+
  '  i,j: integer;'#010+
  '  PackageVariants: TPackageVariants;'#010+
  'begin'#010+
  '  for i := 0 to FPackageVariants.Count-1 do'#010+
  '    begin'#010+
  '    PackageVariants := TP','ackageVariants(FPackageVariants.Items[i]);'#010+
  '    if Installer.FPackageVariantSettings.Values[PackageVariants.Name]<'+
  '>'#039#039' then'#010+
  '      PackageVariants.ActivePackageVariantName:= Installer.FPackageVar'+
  'iantSettings.Values[PackageVariants.Name]'#010+
  '    el','se'#010+
  '      PackageVariants.ActivePackageVariantName:= PackageVariants.Defau'+
  'ltPackageVariantName;'#010+
  '    Dictionary.AddVariable(PackageVariants.Name,PackageVariants.Active'+
  'PackageVariantName);'#010+
  '    SetUnitsOutputDir(FUnitsOutputDir+'#039'$('#039'+PackageVari','ants.n'+
  'ame+'#039')'#039');'#010+
  '    SetPackageUnitInstallDir(FPackageUnitInstallDir+'#039'$('#039'+Packa'+
  'geVariants.Name+'#039')'#039');'#010+
  '    // Do not add targets f the package is inherited'#010+
  '    if PackageVariants.MasterPackage=Self then'#010+
  '      for j := 0 to PackageVariants.Acti','vePackageVariant.Targets.cou'+
  'nt -1 do'#010+
  '        targets.add.assign(PackageVariants.ActivePackageVariant.Target'+
  's.items[j]);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.LoadUnitConfigFromFile(Const AFileName: String);'#010+
  'var'#010+
  '  L,L2 : TStrings;'#010+
  '  VOS : TOS;'#010,
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
  '  Tr','y'#010+
  '    ReadIniFile(AFileName,L);'#010+
  '    With L do'#010+
  '      begin'#010+
  '        Version:=Values[KeyVersion];'#010+
  '        InstalledChecksum:=Cardinal(StrToInt64Def(Values[KeyChecksum],'+
  '$ffffffff));'#010+
  '        VCPU:=StringToCPU(Values[KeyCPU]);'#010+
  '        VOS:=String','ToOS(Values[KeyOS]);'#010+
  '        OSes:=[VOS];'#010+
  '        CPUs:=[VCPU];'#010+
  '        L2:=TStringList.Create;'#010+
  '        L2.CommaText:=Values[KeyDepends];'#010+
  '        for i:=0 to L2.Count-1 do'#010+
  '          begin'#010+
  '            DepName:=L2[i];'#010+
  '            k:=Pos('#039'|'#039',D','epName);'#010+
  '            if k>0 then'#010+
  '              begin'#010+
  '                DepChecksum:=StrToInt(Copy(DepName,k+1,Length(DepName)'+
  '-k));'#010+
  '                DepName:=Copy(DepName,1,k-1);'#010+
  '              end'#010+
  '            else'#010+
  '              DepChecksum:=$ff','ffffff;'#010+
  '            D:=Dependencies.Add(DepName,CPUs,OSes);'#010+
  '            D.RequireChecksum:=DepChecksum;'#010+
  '          end;'#010+
  '        FreeAndNil(L2);'#010+
  '        NeedLibC:=Upcase(Values[KeyNeedLibC])='#039'Y'#039';'#010+
  '        IsFPMakeAddIn:=Upcase(Values[KeyAddIn]',')='#039'Y'#039';'#010+
  '        Flags.DelimitedText:=Values[KeyFlags];'#010+
  #010+
  '        i := 1;'#010+
  '        repeat'#010+
  '        PackageVariantsStr:=Values[KeyPackageVar+inttostr(i)];'#010+
  '        if PackageVariantsStr<>'#039#039' then'#010+
  '          begin'#010+
  '            k := pos('#039':'#039',PackageVari','antsStr);'#010+
  '            if k > 0 then'#010+
  '              begin'#010+
  '                PackageVarName:=copy(PackageVariantsStr,1,k-1);'#010+
  '                if PackageVarName[Length(PackageVarName)]='#039'*'#039' '+
  'then'#010+
  '                  begin'#010+
  '                  SetLength(P','ackageVarName,Length(PackageVarName)-1)'+
  ';'#010+
  '                  AnIsInheritable:=true;'#010+
  '                  end'#010+
  '                else'#010+
  '                  AnIsInheritable:=false;'#010+
  '                PackageVariantsStr:=copy(PackageVariantsStr,k+1,length'+
  '(Pa','ckageVariantsStr)-k);'#010+
  '                pv := Installer.AddPackageVariant(PackageVarName, AnIs'+
  'Inheritable);'#010+
  '                AddPackageVariant(pv);'#010+
  #010+
  '                k := pos('#039','#039',PackageVariantsStr);'#010+
  '                while k>0 do'#010+
  '               ','   begin'#010+
  '                    PackageVarName:=copy(PackageVariantsStr,1,k-1);'#010+
  '                    PackageVariantsStr:=copy(PackageVariantsStr,k+1,le'+
  'ngth(PackageVariantsStr)-k);'#010+
  '                    pv.Add(PackageVarName);'#010+
  '                    ','k := pos('#039','#039',PackageVariantsStr);'#010+
  '                  end;'#010+
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
  'proce','dure TPackage.SaveUnitConfigToStringList(const AStringList: TSt'+
  'rings; ACPU: TCPU; AOS: TOS);'#010+
  'Var'#010+
  '  Deps : String;'#010+
  '  i,j : integer;'#010+
  '  D : TDependency;'#010+
  '  p : TPackage;'#010+
  '  PackageVariants : TPackageVariants;'#010+
  '  PackageVariantsStr: string;'#010+
  'begin'#010,
  '  with AStringList do'#010+
  '    begin'#010+
  '      Values[KeyName]:=Name;'#010+
  '      Values[KeyVersion]:=Version;'#010+
  '      // TODO Generate checksum based on PPUs'#010+
  '      Values[KeyChecksum]:=IntToStr(DateTimeToFileDate(Now));'#010+
  '      Values[KeyCPU]:=CPUToString(AC','PU);'#010+
  '      Values[KeyOS]:=OSToString(AOS);'#010+
  '      //Installer;'#010+
  '      Values[KeySourcePath]:=IncludeTrailingPathDelimiter(IncludeTrail'+
  'ingPathDelimiter(Installer.BuildEngine.FStartDir)+Directory);'#010+
  '      Values[KeyFPMakeOptions]:=trim(Installer','.FPMakeOptionsString);'+
  #010+
  '      Deps:='#039#039';'#010+
  '      for i:=0 to Dependencies.Count-1 do'#010+
  '        begin'#010+
  '          D:=Dependencies[i];'#010+
  '          if (ACPU in D.CPUs) and (AOS in D.OSes) then'#010+
  '            begin'#010+
  '              if Deps<>'#039#039' then'#010+
  '           ','     Deps:=Deps+'#039','#039';'#010+
  '              Deps:=Deps+D.Value;'#010+
  '              P:=TPackage(D.Target);'#010+
  '              if assigned(P) and (P.InstalledChecksum<>$ffffffff) then'+
  #010+
  '                Deps:=Deps+'#039'|'#039'+IntToStr(P.InstalledChecksum);'#010+
  '            end',';'#010+
  '        end;'#010+
  '      Values[KeyDepends]:=Deps;'#010+
  '      if Flags.Count>0 then'#010+
  '        Values[KeyFlags]:=Flags.DelimitedText;'#010+
  '      if NeedLibC then'#010+
  '        Values[KeyNeedLibC]:='#039'Y'#039#010+
  '      else'#010+
  '        Values[KeyNeedLibC]:='#039'N'#039';'#010+
  '      if IsFPMake','AddIn then'#010+
  '        Values[KeyAddIn]:='#039'Y'#039#010+
  '      else'#010+
  '        Values[KeyAddIn]:='#039'N'#039';'#010+
  '      for i := 0 to FPackageVariants.Count-1 do'#010+
  '        begin'#010+
  '          PackageVariants := TPackageVariants(FPackageVariants.Items[i'+
  ']);'#010+
  '          PackageVari','antsStr:=PackageVariants.Name;'#010+
  '          if PackageVariants.IsInheritable then'#010+
  '            PackageVariantsStr:=PackageVariantsStr+'#039'*'#039';'#010+
  '          PackageVariantsStr := PackageVariantsStr +'#039':'#039'+Packag'+
  'eVariants.DefaultPackageVariantName;'#010+
  '      ','    for j := 0 to PackageVariants.Count-1 do'#010+
  '            if not sametext(PackageVariants.Items[j].Name, PackageVari'+
  'ants.DefaultPackageVariantName) then'#010+
  '              PackageVariantsStr:=PackageVariantsStr+'#039','#039'+Packa'+
  'geVariants.Items[j].Name;'#010,
  '          values[KeyPackageVar+inttostr(i+1)] := PackageVariantsStr;'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SaveUnitConfigToFile(Const AFileName: String;ACPU:T'+
  'CPU;AOS:TOS);'#010+
  'Var'#010+
  '  F : TFileStream;'#010+
  '  L : TStringList;'#010+
  'begin'#010+
  '  F:=TFileS','tream.Create(AFileName,fmCreate);'#010+
  '  L:=TStringList.Create;'#010+
  '  try'#010+
  '    SaveUnitConfigToStringList(L,ACPU,AOS);'#010+
  '    L.SaveToStream(F);'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '    F.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  #010+
  '{**********************************************************','*********'+
  '*********'#010+
  '                              TPackages'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TPackages.GetPackage(const AName : String): TPackage;'#010+
  'begin'#010+
  '  Result:=TPackage(ItemByName(ANa','me))'#010+
  'end;'#010+
  #010+
  'function TPackages.GetPackageItem(AIndex : Integer): TPackage;'#010+
  'begin'#010+
  '  Result:=TPackage(Items[AIndex]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackages.SetPackageItem(AIndex : Integer; const AValue: TPa'+
  'ckage);'#010+
  'begin'#010+
  '  Items[AIndex]:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'fu','nction TPackages.AddPackage(const AName: String): TPackage;'#010+
  'var'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  i := IndexOfName(AName);'#010+
  '  if i > -1 then'#010+
  '    begin'#010+
  '    result := PackageItems[i];'#010+
  '    Installer.Log(vlWarning,Format(SWarnDuplicatePackage,[AName]))'#010+
  '    ','end'#010+
  '  else'#010+
  '    begin'#010+
  '    Result:=Add as TPackage;'#010+
  '    Result.Name:=AName;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                             TCustomDefaults'#010+
  '***************************','*****************************************'+
  '********}'#010+
  #010+
  'procedure TCustomDefaults.SetCPU(const AValue: TCPU);'#010+
  'begin'#010+
  '  FCPU:=AValue;'#010+
  '  GlobalDictionary.AddVariable('#039'CPU'#039',CPUToString(FCPU));'#010+
  '  RecalcTarget;'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.SetOpti','ons(const AValue: TStrings);'#010+
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
  '  If (FBaseInstallDir<>'#039#039') then'#010+
  '    Re','sult:=FBaseInstallDir'#010+
  '  else'#010+
  '    if UnixPaths then'#010+
  '      Result:=Prefix +'#039'lib'#039' + PathDelim + '#039'fpc'#039' + Path'+
  'Delim + FCompilerVersion + PathDelim'#010+
  '    else'#010+
  '      Result:=Prefix;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetBinInstallDir: String;'#010+
  'begin'#010+
  '  I','f (FBinInstallDir<>'#039#039') then'#010+
  '    Result:=FBinInstallDir'#010+
  '  else'#010+
  '    If UnixPaths then'#010+
  '      Result:=Prefix+'#039'bin'#039#010+
  '    else'#010+
  '      Result:=BaseInstallDir+'#039'bin'#039'+pathdelim+MakeTargetString('+
  'Defaults.cpu, Defaults.OS);'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefault','s.GetCompiler: String;'#010+
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
  '  If (FDocInstallDir<>'#039#039') then'#010+
  '    Result:=FDocInstallDir'#010+
  '  else'#010+
  '    If U','nixPaths then'#010+
  '      Result:=Prefix+'#039'share'#039'+PathDelim+'#039'doc'#039#010+
  '    else'#010+
  '      Result:=BaseInstallDir+'#039'docs'#039';'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetExamplesInstallDir: String;'#010+
  'begin'#010+
  '  If (FExamplesInstallDir<>'#039#039') then'#010+
  '    Result:=FExamplesInstallDi','r'#010+
  '  else'#010+
  '    If UnixPaths then'#010+
  '      Result:=Prefix+'#039'share'#039'+PathDelim+'#039'doc'#039#010+
  '    else'#010+
  '      Result:=BaseInstallDir+'#039'examples'#039';'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetOptions: TStrings;'#010+
  'begin'#010+
  '  If (FOptions=Nil) then'#010+
  '    FOptions:=TStringList.Crea','te;'#010+
  '  Result:=FOptions;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetUnitInstallDir: String;'#010+
  'begin'#010+
  '  result := FixPath(GlobalDictionary.ReplaceStrings(FUnitInstallDir), '+
  'True);'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetLocalUnitDir: String;'#010+
  'begin'#010+
  '  Result:=F','LocalUnitDir;'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetFPDocOutputDir: String;'#010+
  'begin'#010+
  '  If (FFPDocOutputDir<>'#039#039') then'#010+
  '    Result:=FixPath(FFPDocOutputDir, True)'#010+
  '  else'#010+
  '    Result:=FixPath('#039'.'#039'+PathDelim+'#039'docs'#039', True);'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.','GetBuildCPU: TCpu;'#010+
  'begin'#010+
  '  result := StringToCPU({$I %FPCTARGETCPU%});'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetBuildOS: TOS;'#010+
  'begin'#010+
  '  result := StringToOS({$I %FPCTARGETOS%});'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetBuildString: String;'#010+
  'begin'#010+
  '  result :','= MakeTargetString(BuildCPU, BuildOS);'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetGlobalUnitDir: String;'#010+
  'begin'#010+
  '  If (FGlobalUnitDir<>'#039#039') then'#010+
  '    Result:=FGlobalUnitDir'#010+
  '  else'#010+
  '    Result:=UnitInstallDir;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetLocalUnit','Dir(const AValue: String);'#010+
  'begin'#010+
  '  // Use ExpandFileName to support ~/ expansion'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FLocalUnitDir:=IncludeTrailingPathDelimiter(ExpandFileName(AValue)'+
  ')'#010+
  '  else'#010+
  '    FLocalUnitDir:='#039#039';'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.Set','GlobalUnitDir(const AValue: String);'#010+
  'begin'#010+
  '  // Use ExpandFileName to support ~/ expansion'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FGlobalUnitDir:=IncludeTrailingPathDelimiter(ExpandFileName(AValue'+
  '))'#010+
  '  else'#010+
  '    FGlobalUnitDir:='#039#039';'#010+
  'end;'#010+
  #010+
  'procedure TCustomD','efaults.IntSetBaseInstallDir(const AValue: String)'+
  ';'#010+
  'begin'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FBaseInstallDir:=IncludeTrailingPathDelimiter(AValue)'#010+
  '  else'#010+
  '    FBaseInstallDir:='#039#039';'#010+
  '  GlobalDictionary.AddVariable('#039'baseinstalldir'#039',BaseInstallDir'+
  ');'#010+
  '  Glob','alDictionary.AddVariable('#039'bininstalldir'#039',BinInstallDir'+
  ');'#010+
  '  BinInstallDir:='#039#039';'#010+
  '  ExamplesInstallDir:='#039#039';'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetBaseInstallDir(const AValue: String);'#010+
  'begin'#010+
  '  // There must be a possibility to skip ExpandFileName.',' So that the'+
  ' files'#010+
  '  // can be written into an archive with a relative path.'#010+
  '  if AValue<>'#039#039' then'#010+
  '    // Use ExpandFileName to support ~/ expansion'#010+
  '    IntSetBaseInstallDir(ExpandFileName(AValue))'#010+
  '  else'#010+
  '    IntSetBaseInstallDir(AValue);'#010+
  'en','d;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetOS(const AValue: TOS);'#010+
  'begin'#010+
  '  FOS:=AValue;'#010+
  '  GlobalDictionary.AddVariable('#039'OS'#039',OSToString(FOS));'#010+
  '  Recalctarget;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetPrefix(const AValue: String);'#010+
  'begin'#010+
  '  if FPrefix=AValue ','then exit;'#010+
  '  // Use ExpandFileName to support ~/ expansion'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FPrefix:=IncludeTrailingPathDelimiter(ExpandFileName(AValue))'#010+
  '  else'#010+
  '    FPrefix:='#039#039';'#010+
  '  GlobalDictionary.AddVariable('#039'prefix'#039',Prefix);'#010+
  '  GlobalDictionary.Ad','dVariable('#039'bininstalldir'#039',BinInstallDir'+
  ');'#010+
  '  BaseInstallDir:='#039#039';'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetTarget(const AValue: String);'#010+
  'Var'#010+
  '  P : Integer;'#010+
  'begin'#010+
  '  if FTarget<>AValue then'#010+
  '    begin'#010+
  '      P:=Pos('#039'-'#039',AValue);'#010+
  '      If (P<>0) then'#010+
  '  ','      begin'#010+
  '          FOS:=StringToOS(System.Copy(Avalue,P+1,Length(AValue)-P));'#010+
  '          GlobalDictionary.AddVariable('#039'OS'#039',OSToString(FOS));'#010+
  '          FCPU:=StringToCPU(System.Copy(Avalue,1,P-1));'#010+
  '          GlobalDictionary.AddVariable('#039'C','PU'#039',CPUToString(FCP'+
  'U));'#010+
  '        end'#010+
  '      else'#010+
  '        FOS:=StringToOS(AValue);'#010+
  '      FTarget:=AValue;'#010+
  '      GlobalDictionary.AddVariable('#039'target'#039',Target);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.SetUnitInstallDir(const AValue: String);'#010+
  'be','gin'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FUnitInstallDir:=IncludeTrailingPathDelimiter(AValue)'#010+
  '  else'#010+
  '    FUnitInstallDir:='#039#039';'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.RecalcTarget;'#010+
  'begin'#010+
  '  Ftarget:=MakeTargetString(FCPU,FOS);'#010+
  '  GlobalDictionary.AddVariable('#039't','arget'#039',Target);'#010+
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
  '  InitDefaults;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.I','nitDefaults;'#010+
  'begin'#010+
  '{$ifdef unix}'#010+
  '  UnixPaths:=True;'#010+
  '{$else}'#010+
  '  UnixPaths:=False;'#010+
  '{$endif}'#010+
  '  FNoFPCCfg:=False;'#010+
  '  FCPU:=cpuNone;'#010+
  '  FOS:=osNone;'#010+
  '  FUnitInstallDir:='#039'$(BaseInstallDir)units/$(target)/$(packagename'+
  ')'#039';'#010+
  '  FBuildMode:=bmOneByOne;'#010+
  '  F','ThreadsAmount:=-1;'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.HaveOptions: Boolean;'#010+
  'begin'#010+
  '  Result:=Assigned(FOptions);'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.IsBuildDifferentFromTarget: boolean;'#010+
  'begin'#010+
  '  result := IsDifferentFromBuild(CPU,OS);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure',' TCustomDefaults.LocalInit(Const AFileName : String);'#010+
  'Var'#010+
  '  FN : String;'#010+
  'begin'#010+
  '  FN:=AFileName;'#010+
  '  If (FN='#039#039') then'#010+
  '    begin'#010+
  '    // Environment variable.'#010+
  '    FN:=GetEnvironmentVariable('#039'FPMAKECFG'#039');'#010+
  '    If (FN<>'#039#039') then'#010+
  '      If not FileExis','ts(FN) then'#010+
  '        FN:='#039#039';'#010+
  '    // User config file fpmake.cfg'#010+
  '    If (FN='#039#039') then'#010+
  '      begin'#010+
  '      FN:=GetAppConfigFile(False);'#010+
  '      If Not FileExists(FN) then'#010+
  '        FN:='#039#039';'#010+
  '      end;'#010+
  '    // Global config file fpmake.cfg'#010+
  '    If (FN='#039#039,') then'#010+
  '      begin'#010+
  '      FN:=GetAppConfigFile(True);'#010+
  '      If Not FileExists(FN) then'#010+
  '        FN:='#039#039';'#010+
  '      end;'#010+
  '    end;'#010+
  '  If (FN<>'#039#039') and FileExists(FN) then'#010+
  '    LoadFromFile(FN);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.CompilerDefaults;'#010+
  '{$ifdef',' HAS_UNIT_PROCESS}'#010+
  'var'#010+
  '  infoSL : TStringList;'#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  'begin'#010+
  '  if (CPU=cpuNone) or (OS=osNone) or (FCompilerVersion='#039#039') then'#010+
  '    begin'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '      // Detect compiler version/target from -i option'#010+
  '      ','infosl:=TStringList.Create;'#010+
  '      infosl.Delimiter:='#039' '#039';'#010+
  '      infosl.DelimitedText:=GetCompilerInfo(GetCompiler,'#039'-iVTPTO'#039+
  ');'#010+
  '      if infosl.Count<>3 then'#010+
  '        Raise EInstallerError.Create(SErrInvalidFPCInfo);'#010+
  '      if FCompilerVersion='#039,#039' then'#010+
  '        FCompilerVersion:=infosl[0];'#010+
  '      if CPU=cpuNone then'#010+
  '        CPU:=StringToCPU(infosl[1]);'#010+
  '      if OS=osNone then'#010+
  '        OS:=StringToOS(infosl[2]);'#010+
  '{$else HAS_UNIT_PROCESS}'#010+
  '      // Defaults taken from compiler used to bui','ld fpmake'#010+
  '      if CPU=cpuNone then'#010+
  '        CPU:=StringToCPU({$I %FPCTARGETCPU%});'#010+
  '      if OS=osNone then'#010+
  '        OS:=StringToOS({$I %FPCTARGETOS%});'#010+
  '      if FCompilerVersion='#039#039' then'#010+
  '        FCompilerVersion:={$I %FPCVERSION%};'#010+
  '{$endif HA','S_UNIT_PROCESS}'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.LoadFromFile(Const AFileName: String);'#010+
  'Var'#010+
  '  F : TFileStream;'#010+
  'begin'#010+
  '  F:=TFileStream.Create(AFileName,fmOpenRead);'#010+
  '  Try'#010+
  '    LoadFromStream(F);'#010+
  '  Finally'#010+
  '    F.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'pr','ocedure TCustomDefaults.SaveToFile(Const AFileName: String);'#010+
  'Var'#010+
  '  F : TFileStream;'#010+
  'begin'#010+
  '  F:=TFileStream.Create(AFileName,fmCreate);'#010+
  '  Try'#010+
  '    SaveToStream(F);'#010+
  '  Finally'#010+
  '    F.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SaveToStream(S :',' TStream);'#010+
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
  '      Values[KeyMkDir]:=FMkDir;'#010+
  '      V','alues[KeyMove]:=FMove;'#010+
  '      Values[KeyOptions]:=CmdLineOptions;'#010+
  '      Values[KeyCPU]:=CPUToString(FCPU);'#010+
  '      Values[KeyOS]:=OSToString(FOS);'#010+
  '      Values[KeyMode]:=ModeToString(FMode);'#010+
  '      Values[KeyLocalUnitDir]:=FLocalUnitDir;'#010+
  '      ','Values[KeyGlobalUnitDir]:=FGlobalUnitDir;'#010+
  '      Values[KeyPrefix]:=FPrefix;'#010+
  '      Values[KeyBaseInstallDir]:=FBaseInstallDir;'#010+
  '      Values[KeyUnitInstallDir]:=FUnitInstallDir;'#010+
  '      Values[KeyBinInstallDir]:=FBinInstallDir;'#010+
  '      Values[Key','DocInstallDir]:=FDocInstallDir;'#010+
  '      Values[KeyExamplesInstallDir]:=FExamplesInstallDir;'#010+
  '      Values[KeyRemove]:=FRemove;'#010+
  '      Values[KeyRemoveDir]:=FRemoveDir;'#010+
  '      Values[KeyRemoveTree]:=FRemoveTree;'#010+
  '      Values[KeyTarget]:=FTarget;'#010,
  '      if FNoFPCCfg then'#010+
  '        Values[KeyNoFPCCfg]:='#039'Y'#039';'#010+
  '      if FUseEnvironment then'#010+
  '        Values[KeyUseEnv]:='#039'Y'#039';'#010+
  '      if FInstallExamples then'#010+
  '          Values[KeyInstallExamples]:='#039'Y'#039';'#010+
  '      if FSkipCrossPrograms then'#010+
  '        Value','s[KeySkipCrossProdrams]:='#039'Y'#039';'#010+
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
  '  L:=TStringList.Create',';'#010+
  '  Try'#010+
  '    L.LoadFromStream(S);'#010+
  '    // Fix lines.'#010+
  '    For I:=L.Count-1 downto 0 do'#010+
  '      begin'#010+
  '      Line:=L[I];'#010+
  '      P:=Pos('#039'='#039',Line);'#010+
  '      PC:=Pos('#039';'#039',Line);  // Comment line.'#010+
  '      If (P=0) or ((PC<>0) and (PC<P)) then'#010+
  '        L.Delet','e(I)'#010+
  '      else'#010+
  '        L[i]:=Trim(System.Copy(Line,1,P-1)+'#039'='#039'+Trim(System.Cop'+
  'y(Line,P+1,Length(Line)-P)));'#010+
  '      end;'#010+
  '    With L do'#010+
  '      begin'#010+
  '      FArchive:=Values[KeyArchive];'#010+
  '      FCompiler:=Values[KeyCompiler];'#010+
  '      FCopy:=Values[K','eyCopy];'#010+
  '      FMkDir:=Values[KeyMkDir];'#010+
  '      FMove:=Values[KeyMove];'#010+
  '      FRemove:=Values[KeyRemove];'#010+
  '      FRemoveDir:=Values[KeyRemoveDir];'#010+
  '      FRemoveTree:=Values[KeyRemoveTree];'#010+
  '      Options:=OptionsToStringList(Values[KeyOptions]',');'#010+
  '      Line:=Values[KeyCPU];'#010+
  '      If (Line<>'#039#039') then'#010+
  '        FCPU:=StringToCPU(Line);'#010+
  '      Line:=Values[KeyOS];'#010+
  '      If (Line<>'#039#039') then'#010+
  '        FOS:=StringToOS(Line);'#010+
  '      Line:=Values[KeyMode];'#010+
  '      If (Line<>'#039#039') then'#010+
  '        FMode:','=StringToMode(Line);'#010+
  '      FTarget:=Values[KeyTarget];'#010+
  '      FLocalUnitDir:=Values[KeyLocalUnitDir];'#010+
  '      FGlobalUnitDir:=Values[KeyGlobalUnitDir];'#010+
  '      FPrefix:=Values[KeyPrefix];'#010+
  '      FBaseInstallDir:=Values[KeyBaseInstallDir];'#010+
  '      F','UnitInstallDir:=Values[KeyUnitInstallDir];'#010+
  '      FBinInstallDir:=Values[KeyBinInstallDir];'#010+
  '      FDocInstallDir:=Values[KeyDocInstallDir];'#010+
  '      FExamplesInstallDir:=Values[KeyExamplesInstallDir];'#010+
  '      FInstallExamples:=(Upcase(Values[KeyI','nstallExamples])='#039'Y'#039+
  ');'#010+
  '      FSkipCrossPrograms:=(Upcase(Values[KeySkipCrossProdrams])='#039'Y'+
  #039');'#010+
  '      FNoFPCCfg:=(Upcase(Values[KeyNoFPCCfg])='#039'Y'#039');'#010+
  '      FUseEnvironment:=(Upcase(Values[KeyUseEnv])='#039'Y'#039');'#010+
  #010+
  '      GlobalDictionary.AddVariable('#039'ta','rget'#039',Target);'#010+
  '      GlobalDictionary.AddVariable('#039'baseinstalldir'#039',BaseInstal'+
  'lDir);'#010+
  '      GlobalDictionary.AddVariable('#039'prefix'#039',Prefix);'#010+
  '      GlobalDictionary.AddVariable('#039'bininstalldir'#039',BinInstallD'+
  'ir);'#010+
  '      end;'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  e','nd;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                             TFPCDefaults'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'procedure TFPCDefaults.Compile','rDefaults;'#010+
  'var'#010+
  '  BD : String;'#010+
  'begin'#010+
  '  inherited CompilerDefaults;'#010+
  #010+
  '  // Use the same algorithm as the compiler, see options.pas'#010+
  '{$ifdef Unix}'#010+
  '  BD:=FixPath(GetEnvironmentVariable('#039'FPCDIR'#039'), True);'#010+
  '  if BD='#039#039' then'#010+
  '    begin'#010+
  '      BD:='#039'/usr/l','ocal/lib/fpc/'#039'+FCompilerVersion;'#010+
  '      if not DirectoryExists(BD) and'#010+
  '         DirectoryExists('#039'/usr/lib/fpc/'#039'+FCompilerVersion) the'+
  'n'#010+
  '        BD:='#039'/usr/lib/fpc/'#039'+FCompilerVersion;'#010+
  '    end;'#010+
  '{$else unix}'#010+
  '  BD:=FixPath(GetEnvironmentVariable('#039,'FPCDIR'#039'), True);'#010+
  '  if BD='#039#039' then'#010+
  '    begin'#010+
  '      BD:=ExtractFilePath(FCompiler)+'#039'..'#039';'#010+
  '      if not(DirectoryExists(BD+'#039'/units'#039')) and'#010+
  '         not(DirectoryExists(BD+'#039'/rtl'#039')) then'#010+
  '        BD:=FBaseInstallDir+'#039'..'#039';'#010+
  '    end;'#010+
  '{$endif unix}'#010+
  #010+
  '  /','/ Where to install by default'#010+
  '  if (FBaseInstallDir='#039#039') and (FPrefix='#039#039') then'#010+
  '    BaseInstallDir:=BD;'#010+
  #010+
  '  // Where to find the units by default'#010+
  '  if (FGlobalUnitDir='#039#039') then'#010+
  '    GlobalUnitDir:=IncludeTrailingPathDelimiter(BD)+'#039'units'#039'+Pa'+
  'thDel','im+Target;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                            TCustomInstaller'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TCustomI','nstaller.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  FPackageVariantSettings := TStringList.Create;'#010+
  '  FPackageVariants := TFPList.Create;'#010+
  '  GlobalDictionary:=DictionaryClass.Create(Nil);'#010+
  '  AnalyzeOptions;'#010+
  '  GlobalDictionary.AddVariable('#039'BaseInstall','Dir'#039',Defaults.Bas'+
  'eInstallDir);'#010+
  '  GlobalDictionary.AddVariable('#039'bininstalldir'#039',Defaults.BinIns'+
  'tallDir);'#010+
  '  GlobalDictionary.AddVariable('#039'Target'#039',Defaults.Target);'#010+
  '  GlobalDictionary.AddVariable('#039'BuildString'#039',Defaults.BuildStr'+
  'ing);'#010+
  '  GlobalDic','tionary.AddVariable('#039'Prefix'#039',Defaults.Prefix);'#010+
  '  CreatePackages;'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TCustomInstaller.Destroy;'#010+
  'var'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  FreePackages;'#010+
  '  FreeAndNil(Defaults);'#010+
  '  FreeAndNil(GlobalDictionary);'#010+
  '  FreeAndNil(FPackageVariantSetting','s);'#010+
  '  for i := 0 to FPackageVariants.Count-1 do'#010+
  '    begin'#010+
  '    if TPackageVariants(FPackageVariants.Items[i]).Owner=Self then'#010+
  '      TPackageVariants(FPackageVariants.Items[i]).Free;'#010+
  '    end;'#010+
  '  FreeAndNil(FPackageVariants);'#010+
  '  inherited destro','y;'#010+
  'end;'#010+
  #010+
  'function TCustomInstaller.GetPackages: TPackages;'#010+
  'begin'#010+
  '  result := FPackages;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Log(Level: TVerboseLevel; Const Msg: String'+
  ');'#010+
  'begin'#010+
  '  If Level in FLogLevels then'#010+
  '    begin'#010+
  '    Writeln(StdOut, Msg);'#010+
  ' ','   Flush(StdOut);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.CreatePackages;'#010+
  'begin'#010+
  '  FPackages:=TPackages.Create(TPackage);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.FreePackages;'#010+
  'begin'#010+
  '  FreeAndNil(FPackages);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Crea','teBuildEngine;'#010+
  'begin'#010+
  '  FBuildEngine:=TBuildEngine.Create(Self);'#010+
  '//  FBuildEngine.Defaults:=Defaults;'#010+
  '  FBuildEngine.ListMode:=FListMode;'#010+
  '  FBuildEngine.Verbose := (FLogLevels = AllMessages);'#010+
  '  FBuildEngine.OnLog:=@Self.Log;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure',' TCustomInstaller.Error(const Msg: String);'#010+
  'begin'#010+
  '  Raise EInstallerError.Create(Msg);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Error(const Fmt: String; Args: array of con'+
  'st);'#010+
  'begin'#010+
  '  Raise EInstallerError.CreateFmt(Fmt,Args);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TCus','tomInstaller.AddPackage(const AName: String) : TPackage'+
  ';'#010+
  'begin'#010+
  '  result:=Packages.AddPackage(AName);'#010+
  'end;'#010+
  #010+
  'function TCustomInstaller.AddPackageVariant(AName: string; AIsInherita'+
  'ble: boolean): TPackageVariants;'#010+
  'begin'#010+
  '  result := TPackageVari','ants.Create(TPackageVariant);'#010+
  '  result.Name:=AName;'#010+
  '  result.FIsInheritable:=AIsInheritable;'#010+
  '  FPackageVariants.Add(result);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.AnalyzeOptions;'#010+
  #010+
  '  Function CheckOption(Index : Integer;const Short,Long : String;',' Ad'+
  'dToOptionString: boolean = true): Boolean;'#010+
  '  var'#010+
  '    O : String;'#010+
  '  begin'#010+
  '    O:=Paramstr(Index);'#010+
  '    Result:=(O='#039'-'#039'+short) or (O='#039'--'#039'+long) or (copy(O,'+
  '1,Length(Long)+3)=('#039'--'#039'+long+'#039'='#039'));'#010+
  '    if AddToOptionString and Result then FFPMakeOp','tionsString := FFP'+
  'MakeOptionsString+'#039' '#039'+O;'#010+
  '  end;'#010+
  #010+
  '  Function CheckBuildOptionSetValue(Index: Integer): boolean;'#010+
  '  var'#010+
  '    O : String;'#010+
  '    BuildModeName: string;'#010+
  '    P: integer;'#010+
  '  begin'#010+
  '    O:=Paramstr(Index);'#010+
  '    result := O[1]='#039'+'#039';'#010+
  '    if',' result then'#010+
  '      begin'#010+
  '      P:=Pos('#039'='#039',Paramstr(Index));'#010+
  '      If (P=0) then'#010+
  '        Error(SErrNeedArgument,[Index,O])'#010+
  '      else'#010+
  '        begin'#010+
  '        BuildModeName:=copy(o,2,P-2);'#010+
  '        Delete(O,1,P);'#010+
  '        FPackageVariantSettings.','Values[BuildModeName] := O;'#010+
  '        end;'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  '  Function CheckCustomOption(Index : Integer; out CustOptName: string)'+
  ': Boolean;'#010+
  '  var'#010+
  '    O : String;'#010+
  '    i : integer;'#010+
  '  begin'#010+
  '    result := false;'#010+
  '    CustOptName:='#039#039';'#010+
  '    O:=Par','amstr(Index);'#010+
  '    if copy(O,1,2)<>'#039'--'#039' then'#010+
  '      Exit;'#010+
  '    i := pos('#039'='#039',O);'#010+
  '    if i=0 then'#010+
  '      Exit;'#010+
  '    O:=copy(O,3,i-3);'#010+
  '    CustOptName:=O;'#010+
  '    Result:=CustomFpmakeCommandlineOptions.IndexOfName(O)>-1;'#010+
  '    if Result then FFPMakeOptio','nsString := FFPMakeOptionsString+'#039+
  ' '#039'+Paramstr(Index);'#010+
  '  end;'#010+
  #010+
  #010+
  '  Function CheckCommand(Index : Integer;const Short,Long : String): Bo'+
  'olean;'#010+
  '  var'#010+
  '    O : String;'#010+
  '  begin'#010+
  '    O:=Paramstr(Index);'#010+
  '    Result:=(O='#039'-'#039'+short) or (O=long);'#010+
  '  end;'#010,
  #010+
  '  Function OptionArg(Var Index : Integer) : String;'#010+
  '  Var'#010+
  '    P : Integer;'#010+
  '  begin'#010+
  '    if (Length(ParamStr(Index))>1) and (Paramstr(Index)[2]<>'#039'-'#039+
  ') then'#010+
  '      begin'#010+
  '      If Index<ParamCount then'#010+
  '        begin'#010+
  '        Inc(Index);'#010+
  '        R','esult:=Paramstr(Index);'#010+
  '        end'#010+
  '      else'#010+
  '        Error(SErrNeedArgument,[Index,ParamStr(Index)]);'#010+
  '      end'#010+
  '    else If length(ParamStr(Index))>2 then'#010+
  '      begin'#010+
  '      P:=Pos('#039'='#039',Paramstr(Index));'#010+
  '      If (P=0) then'#010+
  '        Error(SE','rrNeedArgument,[Index,ParamStr(Index)])'#010+
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
  '  beg','in'#010+
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
  '  ','end;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  '  DefaultsFileName : string;'#010+
  '  OptString : string;'#010+
  '  CustOptName : string;'#010+
  'begin'#010+
  '  I:=0;'#010+
  '  FListMode:=False;'#010+
  '  FLogLevels:=DefaultMessages;'#010+
  '  While (I<ParamCount) do'#010+
  '    begin'#010+
  '    Inc(I);'#010+
  '    if CheckOption(I,'#039'v'#039','#039'v','erbose'#039',false) then'#010+
  '      FLogLevels:=AllMessages'#010+
  '    else if CheckOption(I,'#039'd'#039','#039'debug'#039',false) then'#010+
  '      FLogLevels:=AllMessages+[vlDebug]'#010+
  '    else if CheckCommand(I,'#039'm'#039','#039'compile'#039') then'#010+
  '      FRunMode:=rmCompile'#010+
  '    else if CheckCommand(I,',#039'b'#039','#039'build'#039') then'#010+
  '      FRunMode:=rmBuild'#010+
  '    else if CheckCommand(I,'#039'i'#039','#039'install'#039') then'#010+
  '      FRunMode:=rmInstall'#010+
  '    else if CheckCommand(I,'#039'zi'#039','#039'zipinstall'#039') then'#010+
  '      FRunMode:=rmZipInstall'#010+
  '    else if CheckCommand(I,'#039'c'#039','#039'clean'#039') then'#010,
  '      FRunMode:=rmClean'#010+
  '    else if CheckCommand(I,'#039'dc'#039','#039'distclean'#039') then'#010+
  '      FRunMode:=rmDistClean'#010+
  '    else if CheckCommand(I,'#039'a'#039','#039'archive'#039') then'#010+
  '      FRunMode:=rmarchive'#010+
  '    else if CheckCommand(I,'#039'M'#039','#039'manifest'#039') then'#010+
  '      FRunMode:=r','mManifest'#010+
  '    else if CheckOption(I,'#039'h'#039','#039'help'#039') then'#010+
  '      Usage('#039#039',[])'#010+
  '    else if Checkoption(I,'#039'C'#039','#039'cpu'#039') then'#010+
  '      Defaults.CPU:=StringToCPU(OptionArg(I))'#010+
  '    else if Checkoption(I,'#039'O'#039','#039'os'#039') then'#010+
  '      Defaults.OS:=StringToOS(OptionArg','(I))'#010+
  '    else if Checkoption(I,'#039't'#039','#039'target'#039') then'#010+
  '      Defaults.Target:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'l'#039','#039'list-commands'#039') then'#010+
  '      FListMode:=True'#010+
  '    else if Checkoption(I,'#039'P'#039','#039'prefix'#039') then'#010+
  '      Defaults.Prefix:=OptionArg(I)',#010+
  '    else if Checkoption(I,'#039'n'#039','#039'nofpccfg'#039') then'#010+
  '      Defaults.NoFPCCfg:=true'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '    else if Checkoption(I,'#039'e'#039','#039'useenv'#039') then'#010+
  '      Defaults.UseEnvironment:=true'#010+
  '{$endif}'#010+
  '{$ifndef NO_THREADING}'#010+
  '    else if CheckOption','(I,'#039'T'#039','#039'threads'#039') then'#010+
  '      Defaults.ThreadsAmount:=StrToIntDef(OptionArg(I),-1)'#010+
  '{$endif NO_THREADING}'#010+
  '    else if CheckOption(I,'#039'B'#039','#039'baseinstalldir'#039') then'#010+
  '      Defaults.BaseInstallDir:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'U'#039','#039'unitinst','alldir'#039') then'+
  #010+
  '      Defaults.UnitInstallDir:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'UL'#039','#039'localunitdir'#039') then'#010+
  '      Defaults.LocalUnitDir:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'UG'#039','#039'globalunitdir'#039') then'#010+
  '      Defaults.GlobalUnitDir:=Optio','nArg(I)'#010+
  '    else if CheckOption(I,'#039'o'#039','#039'options'#039') then'#010+
  '      begin'#010+
  '        OptString := OptionArg(I);'#010+
  '        while OptString <> '#039#039' do'#010+
  '          Defaults.Options.Add(SplitSpaces(OptString));'#010+
  '      end'#010+
  '    else if CheckOption(I,'#039'r'#039','#039'compiler'#039,') then'#010+
  '      Defaults.Compiler:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'f'#039','#039'config'#039') then'#010+
  '      DefaultsFileName:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'ie'#039','#039'installexamples'#039') then'+
  #010+
  '      Defaults.InstallExamples:=true'#010+
  '    else if CheckOpti','on(I,'#039'sp'#039','#039'skipcrossprograms'#039') '+
  'then'#010+
  '      Defaults.SkipCrossPrograms:=true'#010+
  '    else if CheckOption(I,'#039'bu'#039','#039'buildunit'#039') then'#010+
  '      Defaults.BuildMode:=bmBuildUnit'#010+
  '    else if CheckOption(I,'#039'io'#039','#039'ignoreinvalidoption'#039') '+
  'then'#010+
  '      Defaults.Igno','reInvalidOptions:=true'#010+
  '    else if CheckOption(I,'#039'd'#039','#039'doc-folder'#039') then'#010+
  '      Defaults.FPDocOutputDir:=OptionArg(I)'#010+
  '    else if assigned(CustomFpmakeCommandlineOptions) and CheckCustomOp'+
  'tion(I,CustOptName) then'#010+
  '      begin'#010+
  '      if not assi','gned(CustomFpMakeCommandlineValues) then'#010+
  '        CustomFpMakeCommandlineValues := TStringList.Create;'#010+
  '      CustomFpMakeCommandlineValues.Values[CustOptName]:=OptionArg(I)'#010+
  '      end'#010+
  '    else if (not CheckBuildOptionSetValue(I)) and (not Def','aults.Igno'+
  'reInvalidOptions) then'#010+
  '      begin'#010+
  '      Usage(SErrInValidArgument,[I,ParamStr(I)]);'#010+
  '      end;'#010+
  '    end;'#010+
  '  If DefaultsFileName<>'#039#039' then'#010+
  '    Defaults.LocalInit(DefaultsFileName);'#010+
  '  Defaults.CompilerDefaults;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCusto','mInstaller.Usage(const FMT: String; Args: array of c'+
  'onst);'#010+
  #010+
  '  Procedure LogCmd(const LC,Msg : String);'#010+
  '  begin'#010+
  '    Log(vlInfo,Format('#039' %-12s %s'#039',[LC,MSG]));'#010+
  '  end;'#010+
  #010+
  '  Procedure LogOption(const C,LC,Msg : String);'#010+
  '  begin'#010+
  '    Log(vlInfo,Form','at('#039' -%s --%-16s %s'#039',[C,LC,MSG]));'#010+
  '  end;'#010+
  #010+
  '  Procedure LogArgOption(const C,LC,Msg : String);'#010+
  '  begin'#010+
  '    if trim(c)='#039#039' then'#010+
  '      Log(vlInfo,Format('#039'    --%-20s %s'#039',[LC+'#039'='#039'+SValu'+
  'e,MSG]))'#010+
  '    else'#010+
  '      Log(vlInfo,Format('#039' -%s --%-20s %s'#039',[','C,LC+'#039'='#039'+'+
  'SValue,MSG]));'#010+
  '  end;'#010+
  #010+
  'var'#010+
  '  i: Integer;'#010+
  'begin'#010+
  '  // Force the Usage to be displayed'#010+
  '  Include(FLogLevels,vlInfo);'#010+
  '  If (FMT<>'#039#039') then'#010+
  '    Log(vlInfo,Format(Fmt,Args));'#010+
  '  Log(vlInfo,Format(SHelpUsage,[Paramstr(0)]));'#010+
  '  Log(vlInfo,SH','elpCommand);'#010+
  '  LogCmd('#039'compile'#039',SHelpCompile);'#010+
  '  LogCmd('#039'build'#039',SHelpBuild);'#010+
  '  LogCmd('#039'install'#039',SHelpInstall);'#010+
  '  LogCmd('#039'clean'#039',SHelpClean);'#010+
  '  LogCmd('#039'archive'#039',SHelpArchive);'#010+
  '  LogCmd('#039'manifest'#039',SHelpManifest);'#010+
  '  Log(vlInfo,SHelpCmdOptions)',';'#010+
  '  LogOption('#039'h'#039','#039'help'#039',SHelpHelp);'#010+
  '  LogOption('#039'l'#039','#039'list-commands'#039',SHelpList);'#010+
  '  LogOption('#039'n'#039','#039'nofpccfg'#039',SHelpNoFPCCfg);'#010+
  '  LogOption('#039'v'#039','#039'verbose'#039',SHelpVerbose);'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '  LogOption('#039'e'#039', '#039'useenv'#039', sHelpUseEnvironment);'#010,
  '{$endif}'#010+
  '  LogOption('#039'ie'#039','#039'installexamples'#039',SHelpInstExamples);'+
  #010+
  '  LogOption('#039'bu'#039','#039'buildunit'#039',SHelpUseBuildUnit);'#010+
  '  LogOption('#039'sp'#039','#039'skipcrossprograms'#039',SHelpSkipCrossPro'+
  'gs);'#010+
  '  LogArgOption('#039'C'#039','#039'cpu'#039',SHelpCPU);'#010+
  '  LogArgOption('#039'O'#039','#039'os'#039',SHelpOS)',';'#010+
  '  LogArgOption('#039't'#039','#039'target'#039',SHelpTarget);'#010+
  '  LogArgOption('#039'P'#039','#039'prefix'#039',SHelpPrefix);'#010+
  '  LogArgOption('#039'B'#039','#039'baseinstalldir'#039',SHelpBaseInstalldi'+
  'r);'#010+
  '  LogArgOption('#039'UL'#039','#039'localunitdir'#039',SHelpLocalUnitdir);'+
  #010+
  '  LogArgOption('#039'UG'#039','#039'globalunitdir'#039',SHelp','GlobalUnitd'+
  'ir);'#010+
  '  LogArgOption('#039'U'#039','#039'unitinstalldir'#039',SHelpUnitInstallDi'+
  'r);'#010+
  '  LogArgOption('#039'r'#039','#039'compiler'#039',SHelpCompiler);'#010+
  '  LogArgOption('#039'f'#039','#039'config'#039',SHelpConfig);'#010+
  '  LogArgOption('#039'o'#039','#039'options'#039',SHelpOptions);'#010+
  '  LogArgOption('#039'io'#039','#039'ignoreinvalid','option'#039',SHelpIgnor'+
  'eInvOpt);'#010+
  '  LogArgOption('#039'd'#039', '#039'doc-folder'#039', sHelpFpdocOutputDir)'+
  ';'#010+
  '{$ifndef NO_THREADING}'#010+
  '  LogArgOption('#039'T'#039', '#039'threads'#039', sHelpThreads);'#010+
  '{$endif NO_THREADING}'#010+
  '  if assigned(CustomFpmakeCommandlineOptions) then for i  := 0 to',' Cu'+
  'stomFpmakeCommandlineOptions.Count-1 do'#010+
  '    LogArgOption('#039' '#039',CustomFpmakeCommandlineOptions.Names[i],C'+
  'ustomFpmakeCommandlineOptions.ValueFromIndex[i]);'#010+
  '  Log(vlInfo,'#039#039');'#010+
  '  If (FMT<>'#039#039') then'#010+
  '    halt(1)'#010+
  '  else'#010+
  '    halt(0);'#010+
  'end;'#010+
  #010+
  #010+
  'procedur','e TCustomInstaller.Compile(Force: Boolean);'#010+
  'begin'#010+
  '  FBuildEngine.ForceCompile:=Force;'#010+
  '  FBuildEngine.Compile(Packages);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Clean(AllTargets: boolean);'#010+
  'begin'#010+
  '  BuildEngine.Clean(Packages, AllTargets);'#010+
  'end;'#010+
  #010+
  #010+
  'pr','ocedure TCustomInstaller.Install;'#010+
  'begin'#010+
  '  BuildEngine.Install(Packages);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.ZipInstall;'#010+
  'begin'#010+
  '  BuildEngine.ZipInstall(Packages);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Archive;'#010+
  'begin'#010+
  '  // Force generation of manife','st.xml, this is required for the repo'+
  'sitory'#010+
  '  BuildEngine.Manifest(Packages);'#010+
  '  BuildEngine.Archive(Packages);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Manifest;'#010+
  'begin'#010+
  '  BuildEngine.Manifest(Packages);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.CheckPackag','es;'#010+
  'begin'#010+
  '  If (Packages.Count=0) then'#010+
  '    Error(SErrNoPackagesDefined);'#010+
  '  // Check for other obvious errors ?'#010+
  'end;'#010+
  #010+
  #010+
  'Function TCustomInstaller.Run : Boolean;'#010+
  'begin'#010+
  '  Result:=True;'#010+
  '  try'#010+
  '    CheckPackages;'#010+
  '    CreateBuildEngine;'#010+
  '    Case Ru','nMode of'#010+
  '      rmCompile : Compile(False);'#010+
  '      rmBuild   : Compile(True);'#010+
  '      rmInstall : Install;'#010+
  '      rmZipInstall : ZipInstall;'#010+
  '      rmArchive : Archive;'#010+
  '      rmClean    : Clean(False);'#010+
  '      rmDistClean: Clean(True);'#010+
  '      rmMani','fest : Manifest;'#010+
  '    end;'#010+
  '  except'#010+
  '    On E : Exception do'#010+
  '      begin'#010+
  '      Log(vlError,SErrInstaller);'#010+
  '      Log(vlError,E.Message);'#010+
  '      Result:=False;'#010+
  '      end;'#010+
  '  end;'#010+
  '  // Force returning an exitcode to the shell'#010+
  '  if not Result then',#010+
  '    ExitCode:=1;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TFPCInstaller'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor ','TFPCInstaller.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  if assigned(Defaults) then'#010+
  '    Error(SErrAlreadyInitialized);'#010+
  '  Defaults:=TFPCDefaults.Create;'#010+
  '  inherited Create(AOwner);'#010+
  'end;'#010+
  #010+
  #010+
  '{***********************************************************','********'+
  '*********'#010+
  '                                 TBasicInstaller'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TBasicInstaller.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  if assigned(Defaults) then'#010+
  '   ',' Error(SErrAlreadyInitialized);'#010+
  '  Defaults:=TBasicDefaults.Create;'#010+
  '  inherited Create(AOwner);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                 TBuildEngine'#010+
  '**************','******************************************************'+
  '********}'#010+
  #010+
  'constructor TBuildEngine.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  inherited Create(AOwner);'#010+
  '  // Maybe this should be the current directory ?'#010+
  '  // Or have it as a command-line opti','on.'#010+
  '  // Would allow to put all '#039'installers'#039' in one dir and call t'+
  'hem'#010+
  '  // With --start-dir=/path/to/sources.'#010+
  '  FStartDir:=includeTrailingPathDelimiter(GetCurrentDir);'#010+
  '  FExternalPackages:=TPackages.Create(TPackage);'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TBuil','dEngine.Destroy;'#010+
  'begin'#010+
  '  FreeAndNil(FExternalPackages);'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Error(const Msg: String);'#010+
  'begin'#010+
  '  Raise EInstallerError.Create(Msg);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Error(const Fmt: String; const Arg','s: array of'+
  ' const);'#010+
  'begin'#010+
  '  Raise EInstallerError.CreateFmt(Fmt,Args);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.ExecuteCommand(const Cmd,Args : String; const E'+
  'nv: TStrings = nil; IgnoreError : Boolean = False);'#010+
  'Var'#010+
  '  E : Integer;'#010+
  '  cmdLine: string;'#010+
  '  ','ConsoleOutput: TMemoryStream;'#010+
  '  s: string;'#010+
  'begin'#010+
  '  Log(vlInfo,SInfoExecutingCommand,[Cmd,Args]);'#010+
  '  if ListMode then'#010+
  '    Log(vlCommand,'#039'%s %s'#039',[Cmd,Args])'#010+
  '  else'#010+
  '    begin'#010+
  '      // We should check cmd for spaces, and move all after first spa',
  'ce to args.'#010+
  '      ConsoleOutput := TMemoryStream.Create;'#010+
  '      try'#010+
  '        {$ifdef HAS_UNIT_PROCESS}'#010+
  '        E:=ExecuteFPC(Verbose, cmd, args, env, ConsoleOutput);'#010+
  '        {$else}'#010+
  '        E:=ExecuteProcess(cmd,args);'#010+
  '        {$endif}'#010+
  '      ','  If (E<>0) and (not IgnoreError) then'#010+
  '          begin'#010+
  '            if trim(Args)<>'#039#039' then'#010+
  '              cmdLine := cmd + '#039' '#039' + trim(args)'#010+
  '            else'#010+
  '              cmdline := cmd;'#010+
  '            s := ParsecompilerOutput(ConsoleOutput,Verb','ose);'#010+
  '            Error(SErrExternalCommandFailed,[cmdLine,E,s]);'#010+
  '          end;'#010+
  '      finally'#010+
  '        ConsoleOutput.Free;'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.SysDirectoryExists(const ADir:string):Boolean;'#010+
  'begin'#010+
  '  result:=SysUt','ils.DirectoryExists(ADir);'#010+
  '  if result then'#010+
  '    Log(vlDebug,SDbgDirectoryExists,[ADir,SDbgFound])'#010+
  '  else'#010+
  '    Log(vlDebug,SDbgDirectoryExists,[ADir,SDbgNotFound]);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.SysFileExists(const AFileName:string):Boolean;'#010+
  'b','egin'#010+
  '  result:=SysUtils.FileExists(AFileName);'#010+
  '  if result then'#010+
  '    Log(vlDebug,SDbgFileExists,[AFileName,SDbgFound])'#010+
  '  else'#010+
  '    Log(vlDebug,SDbgFileExists,[AFileName,SDbgNotFound]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.SysCopyFile(Const Src,Dest ',': String);'#010+
  'Var'#010+
  '  D,S : String;'#010+
  '  Fin,FOut : TFileStream;'#010+
  '  Count : Int64;'#010+
  '  A : Integer;'#010+
  '{$ifdef UNIX}'#010+
  '  FileStat: stat;'#010+
  '{$endif UNIX}'#010+
  'begin'#010+
  '  Log(vlInfo,SInfoCopyingFile,[Src,Dest]);'#010+
  '  FIn:=TFileStream.Create(Src,fmopenRead);'#010+
  '  Try'#010+
  '    D:=','IncludeTrailingPathDelimiter(Dest);'#010+
  '    If DirectoryExists(D) then'#010+
  '      S:=D+ExtractFileName(Src)'#010+
  '    else'#010+
  '      S:=Dest;'#010+
  '    FOut:=TFileStream.Create(S,fmCreate);'#010+
  '    Try'#010+
  '      Count:=Fout.CopyFrom(FIn,0);'#010+
  '      If (Count<>Fin.Size) then'#010,
  '        Error(SErrCopyingFile,[Src,S]);'#010+
  '    Finally'#010+
  '      FreeAndNil(Fout);'#010+
  '    end;'#010+
  '    A:=FileGetDate(FIn.Handle);'#010+
  '    If (A=-1) then'#010+
  '      log(vlWarning,SWarnFailedToGetTime,[Src])'#010+
  '    else'#010+
  '      if FileSetDate(S,A)<>0 then'#010+
  '        Log(v','lWarning,SWarnFailedToSetTime,[S]);'#010+
  '{$ifdef UNIX}'#010+
  '    // Copy the file-access rights on Unix, especially the executable-'+
  'bit'#010+
  '    if FpStat(Src,FileStat) <> 0 then'#010+
  '      Log(vlWarning,SWarnCanNotGetAccessRights,[Src])'#010+
  '    else'#010+
  '      if FpChmo','d(s,FileStat.st_mode) <> 0 then'#010+
  '        Log(vlWarning,SWarnCanNotSetAccessRights,[S]);'#010+
  '{$endif UNIX}'#010+
  '  finally'#010+
  '    FreeAndNil(Fin);'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.SysMoveFile(Const Src,Dest : String);'#010+
  'Var'#010+
  '  S : String;'#010+
  'begin'#010+
  '  If Dire','ctoryExists(IncludeTrailingPathDelimiter(Dest)) then'#010+
  '    S:=IncludeTrailingPathDelimiter(Dest)+ExtractFileName(Src)'#010+
  '  else'#010+
  '    S:=Dest;'#010+
  '  If Not RenameFile(Src,S) then'#010+
  '    begin'#010+
  '      Try'#010+
  '        SysCopyFile(Src,S);'#010+
  '        SysDeleteFile(Sr','c);'#010+
  '      Except'#010+
  '        On E : Exception Do'#010+
  '          Error(SErrMovingFile,[Src,S]);'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.SysDeleteFile(Const AFileName : String);'#010+
  'begin'#010+
  '  Log(vlInfo,SInfoDeletingFile,[AFileName]);'#010+
  '  if not Fil','eExists(AFileName) then'#010+
  '    Log(vldebug,SDbgFileDoesNotExist,[AFileName])'#010+
  '  else If Not DeleteFile(AFileName) then'#010+
  '    Error(SErrDeletingFile,[AFileName]);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.SysDeleteDirectory(Const ADirectoryName: String'+
  ');'#010+
  'begin',#010+
  '  if not DirectoryExists(ADirectoryName) then'#010+
  '    Log(vldebug,SDbgDirectoryDoesNotExist,[ADirectoryName])'#010+
  '  else if not IsDirectoryEmpty(ADirectoryName) then'#010+
  '    Log(vldebug,SDbgDirectoryNotEmpty,[ADirectoryName])'#010+
  '  else If Not RemoveDir(A','DirectoryName) then'#010+
  '    Error(SErrRemovingDirectory,[ADirectoryName]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.SysDeleteTree(Const ADirectoryName: String);'#010+
  #010+
  '  function IntRemoveTree(const ADirectoryName: String) : boolean;'#010+
  '  var'#010+
  '    searchRec: TSearc','hRec;'#010+
  '    SearchResult: longint;'#010+
  '  begin'#010+
  '    result := true;'#010+
  '    SearchResult := FindFirst(IncludeTrailingPathDelimiter(ADirectoryN'+
  'ame)+AllFilesMask, faAnyFile+faSymLink, searchRec);'#010+
  '    try'#010+
  '      while SearchResult=0 do'#010+
  '        begin'#010+
  '     ','     if (searchRec.Name<>'#039'.'#039') and (searchRec.Name<>'#039+
  '..'#039') then'#010+
  '             begin'#010+
  '               if (searchRec.Attr and faDirectory)=faDirectory then'#010+
  '                 begin'#010+
  '                   if not IntRemoveTree(IncludeTrailingPathDelimiter','('+
  'ADirectoryName)+searchRec.Name) then'#010+
  '                     result := false;'#010+
  '                 end'#010+
  '               else if not DeleteFile(IncludeTrailingPathDelimiter(ADi'+
  'rectoryName)+searchRec.Name) then'#010+
  '                 result := False;'#010+
  '     ','        end;'#010+
  '          SearchResult := FindNext(searchRec);'#010+
  '        end;'#010+
  '    finally'#010+
  '      FindClose(searchRec);'#010+
  '    end;'#010+
  '    if not RemoveDir(ADirectoryName) then'#010+
  '      result := false;'#010+
  '  end;'#010+
  #010+
  'begin'#010+
  '  if not DirectoryExists(ADirectoryName',') then'#010+
  '    Log(vldebug,SDbgDirectoryDoesNotExist,[ADirectoryName])'#010+
  '  else If Not IntRemoveTree(ADirectoryName) then'#010+
  '    Error(SErrRemovingDirectory,[ADirectoryName]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.SysArchiveFiles(List: TStrings;Const AFileN','am'+
  'e: String);'#010+
  'begin'#010+
  '  If Not (Assigned(OnArchivefiles) or Assigned(ArchiveFilesProc)) then'+
  #010+
  '    Raise EInstallerError.Create(SErrNoArchiveSupport);'#010+
  '  If Assigned(ArchiveFilesProc) then'#010+
  '    ArchiveFilesProc(AFileName,List)'#010+
  '  else'#010+
  '    OnArchiv','eFiles(AFileName,List);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.LogIndent;'#010+
  'begin'#010+
  '  GLogPrefix:=GLogPrefix+'#039'  '#039';'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.LogUnIndent;'#010+
  'begin'#010+
  '  Delete(GLogPrefix,1,2);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Log(Level: TVerboseLevel; Msg: St','ring);'#010+
  'begin'#010+
  '  If Assigned(FOnLog) then'#010+
  '    begin'#010+
  '      if Level in [vlInfo,vlDebug] then'#010+
  '        FOnLog(Level,GLogPrefix+Msg)'#010+
  '      else'#010+
  '        FOnLog(Level,Msg);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Log(Level: TVerboseLevel; Fmt: Strin','g;const A'+
  'rgs: array of const);'#010+
  'begin'#010+
  '  Log(Level,Format(Fmt,Args));'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.EnterDir(ADir: String);'#010+
  'Var'#010+
  '  D : String;'#010+
  'begin'#010+
  '  D:=FStartDir;'#010+
  '  D:=D+ADir;'#010+
  '  Log(vlDebug,SDbgEnterDir,[D]);'#010+
  '  If Not SetCurrentDir(D) then'#010+
  ' ','   Error(SErrChangeDirFailed,[D]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CmdCopyFiles(List: TStrings; Const DestDir: Str'+
  'ing);'#010+
  #010+
  'Var'#010+
  '  Args : String;'#010+
  '  I : Integer;'#010+
  '  DestFileName : String;'#010+
  'begin'#010+
  '  // When the files should be written to an archive, a','dd them'#010+
  '  if assigned(FZipper) then'#010+
  '    begin'#010+
  '      For I:=0 to List.Count-1 do'#010+
  '        if List.Names[i]<>'#039#039' then'#010+
  '          begin'#010+
  '            if IsRelativePath(list.ValueFromIndex[i]) then'#010+
  '              DestFileName:=DestDir+list.ValueFromI','ndex[i]'#010+
  '            else'#010+
  '              DestFileName:=list.ValueFromIndex[i];'#010+
  '            FZipper.Entries.AddFileEntry(List.names[i], DestFileName);'+
  #010+
  '          end'#010+
  '        else'#010+
  '          FZipper.Entries.AddFileEntry(List[i], DestDir+ExtractFi','leN'+
  'ame(List[i]));'#010+
  '      Exit;'#010+
  '    end;'#010+
  #010+
  '  // Copy the files to their new location on disk'#010+
  '  CmdCreateDir(DestDir);'#010+
  '  If (Defaults.Copy<>'#039#039') then'#010+
  '    begin'#010+
  '      Args:=FileListToString(List,'#039#039');'#010+
  '      Args:=Args+'#039' '#039'+DestDir;'#010+
  '      ExecuteCom','mand(Defaults.Copy,Args);'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      if List.Names[i]<>'#039#039' then'#010+
  '        begin'#010+
  '          if IsRelativePath(list.ValueFromIndex[i]) then'#010+
  '            DestFileName:=DestDir+list.ValueFromIndex[i]'#010+
  '       ','   else'#010+
  '            DestFileName:=list.ValueFromIndex[i];'#010+
  '          CmdCreateDir(ExtractFilePath(DestFileName));'#010+
  '          SysCopyFile(List.names[i],DestFileName)'#010+
  '        end'#010+
  '      else'#010+
  '        SysCopyFile(List[i],DestDir);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure',' TBuildEngine.CmdCreateDir(const DestDir: String);'#010+
  'begin'#010+
  '  If (Defaults.MkDir<>'#039#039') then'#010+
  '    ExecuteCommand(Defaults.MkDir,DestDir)'#010+
  '  else'#010+
  '    If not ForceDirectories(DestDir) then'#010+
  '      Error(SErrCreatingDirectory,[DestDir]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedu','re TBuildEngine.CmdMoveFiles(List: TStrings; Const DestDir: S'+
  'tring);'#010+
  'Var'#010+
  '  Args : String;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  CmdCreateDir(DestDir);'#010+
  '  If (Defaults.Move<>'#039#039') then'#010+
  '    begin'#010+
  '      Args:=FileListToString(List,'#039#039');'#010+
  '      Args:=Args+'#039' '#039'+Dest','Dir;'#010+
  '      ExecuteCommand(Defaults.Move,Args);'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      SysMoveFile(List[i],DestDir);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CmdDeleteFiles(List: TStrings);'#010+
  'Var'#010+
  '  Args : String;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  If (','Defaults.Remove<>'#039#039') then'#010+
  '    begin'#010+
  '      Args:=FileListToString(List,'#039#039');'#010+
  '      ExecuteCommand(Defaults.Remove,Args);'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      SysDeleteFile(List[i]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CmdArchiveFile','s(List: TStrings; Const Archive'+
  'File: String);'#010+
  'Var'#010+
  '  S,C,O : String;'#010+
  'begin'#010+
  '  If (Defaults.Archive='#039#039') then'#010+
  '    SysArchiveFiles(List,ArchiveFile)'#010+
  '  else'#010+
  '    begin'#010+
  '      S:=FileListToString(List,'#039#039');'#010+
  '      SplitCommand(Defaults.Archive,C,O);'#010+
  ' ','     If (O='#039#039') then'#010+
  '        O:=ArchiveFile+'#039' '#039'+S'#010+
  '      else'#010+
  '        O:=GlobalDictionary.Substitute(O,['#039'ARCHIVE'#039',ArchiveFil'+
  'e,'#039'FILESORDIRS'#039']);'#010+
  '      ExecuteCommand(C,O);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.CmdRenameFile(SourceName, DestName',': String);'#010+
  'var'#010+
  '  Args: string;'#010+
  'begin'#010+
  '  If (Defaults.Move<>'#039#039') then'#010+
  '    begin'#010+
  '      Args:=SourceName;'#010+
  '      Args:=Args+'#039' '#039'+DestName;'#010+
  '      ExecuteCommand(Defaults.Move,Args);'#010+
  '    end'#010+
  '  else'#010+
  '    SysMoveFile(SourceName,DestName);'#010+
  'end;'#010+
  #010+
  'procedu','re TBuildEngine.CmdRemoveDirs(List: TStrings);'#010+
  'Var'#010+
  '  Args : String;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  If (Defaults.RemoveDir<>'#039#039') then'#010+
  '    begin'#010+
  '      Args:=FileListToString(List,'#039#039');'#010+
  '      ExecuteCommand(Defaults.RemoveDir,Args);'#010+
  '    end'#010+
  '  else'#010+
  '    F','or I:=0 to List.Count-1 do'#010+
  '      SysDeleteDirectory(List[i]);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.CmdRemoveTrees(List: TStrings);'#010+
  'Var'#010+
  '  Args : String;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  If (Defaults.RemoveTree<>'#039#039') then'#010+
  '    begin'#010+
  '      Args:=FileListToString(','List,'#039#039');'#010+
  '      ExecuteCommand(Defaults.RemoveTree,Args);'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      SysDeleteTree(List[i]);'#010+
  'end;'#010+
  #010+
  'Function TBuildEngine.FileNewer(const Src,Dest : String) : Boolean;'#010+
  #010+
  'Var'#010+
  '  DS,DD : Longint;'#010+
  '  D1,D2',' : TDateTime;'#010+
  #010+
  'begin'#010+
  '  DS:=FileAge(Src);'#010+
  '  { Return false if file not found or not accessible }'#010+
  '  if DS=-1 then'#010+
  '    begin'#010+
  '      Log(vlWarning,SWarnCanNotGetFileAge,[Src]);'#010+
  '      Result:=false;'#010+
  '      exit;'#010+
  '    end;'#010+
  '  DD:=FileAge(Dest);'#010+
  '  D1:','=FileDateToDateTime(DS);'#010+
  '  D2:=FileDateToDateTime(DD);'#010+
  '  Log(vlDebug,SDbgComparingFileTimes,[Src,DateTimeToStr(D1),Dest,DateT'+
  'imeToStr(D2)]);'#010+
  '  Result:=D1>=D2;'#010+
  '  If Result then'#010+
  '    Log(vlInfo,SInfoSourceNewerDest,[Src,DateTimeToStr(D1),Dest,','Date'+
  'TimeToStr(D2)]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.ExecuteCommands(Commands: TCommands; At: TComma'+
  'ndAt; APackage: TPackage);'#010+
  'Var'#010+
  '  C : TCommand;'#010+
  '  I : Integer;'#010+
  '  Cmd,O : String;'#010+
  '  E : Boolean;'#010+
  '  ADictionary: TDictionary;'#010+
  '  SourceFile, DestF','ile: string;'#010+
  'begin'#010+
  '  For I:=0 to Commands.Count-1 do'#010+
  '    begin'#010+
  '      C:=Commands.CommandItems[i];'#010+
  '      if (C.At=At) then'#010+
  '        begin'#010+
  '          E:=True;'#010+
  #010+
  '          if assigned(APackage) then'#010+
  '            ADictionary := APackage.Dictionary'#010,
  '          else'#010+
  '            ADictionary := GlobalDictionary;'#010+
  '          SourceFile := ADictionary.ReplaceStrings(C.SourceFile);'#010+
  '          DestFile := ADictionary.ReplaceStrings(C.DestFile);'#010+
  '          if IsRelativePath(SourceFile) then'#010+
  '       ','     SourceFile := AddPathPrefix(APackage,SourceFile);'#010+
  '          if IsRelativePath(DestFile) then'#010+
  '            DestFile := AddPathPrefix(APackage,DestFile);'#010+
  #010+
  '          Cmd:=C.Command;'#010+
  '          If (ExtractFilePath(Cmd)='#039#039') then'#010+
  '            C','md:=ExeSearch(Cmd,GetEnvironmentvariable('#039'PATH'#039')'+
  ');'#010+
  #010+
  '          If (SourceFile<>'#039#039') and (DestFile<>'#039#039')  then'#010+
  '            begin'#010+
  '              if not FileExists(DestFile) then'#010+
  '                Log(vlInfo,SInfoDestDoesNotExist,[DestFile])'#010+
  '       ','       else'#010+
  '                begin'#010+
  '                E:=FileNewer(SourceFile, DestFile);'#010+
  '                if E and (cmd = '#039#039') then'#010+
  '                  begin'#010+
  '                  log(vlWarning,SWarnExtCommandNotFound,[C.Command,Des'+
  'tFile,SourceFile]);',#010+
  '                  E := False;'#010+
  '                  end;'#010+
  '                end;'#010+
  '            end;'#010+
  '          If E then'#010+
  '            begin'#010+
  '            if Cmd = '#039#039' then'#010+
  '              error(SErrExtCommandNotFound,[C.Command]);'#010+
  #010+
  '            If Assigned','(C.BeforeCommand) then'#010+
  '              C.BeforeCommand(C);'#010+
  '            O:=ADictionary.Substitute(C.CmdLineOptions,['#039'SOURCE'#039+
  ',SourceFile,'#039'DEST'#039',DestFile]);'#010+
  #010+
  '            Log(vlCommand,SInfoExecutingCommand,[Cmd,O]);'#010+
  '            ExecuteCommand(Cm','d,O,nil,C.IgnoreResult);'#010+
  '            If Assigned(C.AfterCommand) then'#010+
  '              C.AfterCommand(C);'#010+
  '            end;'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.LogSearchPath(APackage: TPackage; const ASearch'+
  'PathName: string; Pat','h: TConditionalStrings; ACPU: TCPU; AOS: TOS);'#010+
  'var'#010+
  '  S : String;'#010+
  '  I : Integer;'#010+
  '  C : TConditionalString;'#010+
  'begin'#010+
  '  S:='#039#039';'#010+
  '  for i:=0 to Path.Count-1 do'#010+
  '    begin'#010+
  '      C:=Path[I];'#010+
  '      if (ACPU in C.CPUs) and (AOS in C.OSes) then'#010+
  '        be','gin'#010+
  '          if S<>'#039#039' then'#010+
  '            S:=S+PathSeparator;'#010+
  '          S:=S+APackage.Dictionary.ReplaceStrings(C.Value)'#010+
  '        end;'#010+
  '    end;'#010+
  '  if S<>'#039#039' then'#010+
  '    Log(vlDebug,SDbgSearchPath,[ASearchPathName,S]);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.F','indFileInPath(APackage: TPackage; Path:TCondi'+
  'tionalStrings; AFileName:String; var FoundPath:String;ACPU:TCPU;AOS:TO'+
  'S):Boolean;'#010+
  'var'#010+
  '  I : Integer;'#010+
  '  C : TConditionalString;'#010+
  'begin'#010+
  '  Result:=false;'#010+
  '  for i:=0 to Path.Count-1 do'#010+
  '    begin'#010+
  '     ',' C:=Path[I];'#010+
  '      if (ACPU in C.CPUs) and (AOS in C.OSes) then'#010+
  '        begin'#010+
  '          FoundPath:=IncludeTrailingPathDelimiter(APackage.Dictionary.'+
  'ReplaceStrings(C.Value));'#010+
  '          if FileExists(AddPathPrefix(APackage,FoundPath+AFileName','))'+
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
  'procedure TBuildEngine.GetDirectoriesFromFilelist(const AFileList, ADi'+
  'rectoryList: TStringList);'#010+
  'var'#010+
  '  i',': integer;'#010+
  'begin'#010+
  '  ADirectoryList.Sorted:=true;'#010+
  '  ADirectoryList.Duplicates:=dupIgnore;'#010+
  '  for i := 0 to AFileList.Count-1 do'#010+
  '    ADirectoryList.Add(ExtractFileDir(AFileList.Strings[i]));'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure TBuildEngine.ResolveFileNames(APacka','ge : TPackage; ACPU:TC'+
  'PU;AOS:TOS;DoChangeDir:boolean=true; WarnIfNotFound:boolean=true);'#010+
  #010+
  '  procedure FindMainSource(T:TTarget);'#010+
  '  var'#010+
  '    SD,SF  : String;'#010+
  '  begin'#010+
  '    LogSearchPath(APackage,'#039'package source'#039',APackage.SourcePat'+
  'h,ACPU,AOS);'#010+
  ' ','   SD:=APackage.Dictionary.ReplaceStrings(T.Directory);'#010+
  '    SF:=APackage.Dictionary.ReplaceStrings(T.SourceFileName);'#010+
  '    if SD='#039#039' then'#010+
  '      FindFileInPath(APackage,APackage.SourcePath,SF,SD,ACPU,AOS);'#010+
  '    if SD<>'#039#039' then'#010+
  '      SD:=IncludeT','railingPathDelimiter(SD);'#010+
  '    T.FTargetSourceFileName:=SD+SF;'#010+
  '    if FileExists(AddPathPrefix(APackage,T.TargetSourceFileName)) then'+
  #010+
  '      Log(vlDebug,SDbgResolvedSourceFile,[T.SourceFileName,T.TargetSou'+
  'rceFileName])'#010+
  '    else'#010+
  '      begin'#010+
  '  ','      if WarnIfNotFound then'#010+
  '          Log(vlWarning,SWarnSourceFileNotFound,[T.SourceFileName,APac'+
  'kage.Name,MakeTargetString(ACPU,AOS)]);'#010+
  '        APackage.FAllFilesResolved:=false;'#010+
  '        T.FTargetSourceFileName:='#039#039';'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  '  ','procedure FindIncludeSources(T:TTarget);'#010+
  '  var'#010+
  '    SD,SF  : String;'#010+
  '    D : TDependency;'#010+
  '    j : integer;'#010+
  '  begin'#010+
  '    LogSearchPath(APackage,'#039'target include'#039',T.IncludePath,ACPU'+
  ',AOS);'#010+
  '    LogSearchPath(APackage,'#039'package include'#039',APackage.Inc','lud'+
  'ePath,ACPU,AOS);'#010+
  '    for j:=0 to T.Dependencies.Count-1 do'#010+
  '      begin'#010+
  '        D:=T.Dependencies[j];'#010+
  '        if (D.DependencyType=depInclude) then'#010+
  '          begin'#010+
  '            if D.TargetFileName<>'#039#039' then'#010+
  '              begin'#010+
  '             ',' Exit;'#010+
  '              Log(vlDebug,SDbgSourceAlreadyResolved,[T.Name]);'#010+
  '              end;'#010+
  '            if (ACPU in D.CPUs) and (AOS in D.OSes) then'#010+
  '              begin'#010+
  '                if ExtractFilePath(D.Value)='#039#039' then'#010+
  '                  begi','n'#010+
  '                    SF:=APAckage.Dictionary.ReplaceStrings(D.Value);'#010+
  '                    SD:='#039#039';'#010+
  '                    // first check the target specific path'#010+
  '                    if not FindFileInPath(APackage, T.IncludePath,SF,S'+
  'D,ACPU,AOS)',' then'#010+
  '                      FindFileInPath(APackage, APackage.IncludePath,SF'+
  ',SD,ACPU,AOS);'#010+
  '                     if SD<>'#039#039' then'#010+
  '                       SD:=IncludeTrailingPathDelimiter(SD);'#010+
  '                     D.TargetFileName:=SD+SF;'#010+
  '      ','            end'#010+
  '                else'#010+
  '                  D.TargetFileName:=D.Value;'#010+
  '                if FileExists(AddPathPrefix(APackage,D.TargetFileName)'+
  ') then'#010+
  '                  Log(vlDebug,SDbgResolvedIncludeFile,[D.Value,D.Targe'+
  'tFileName])',#010+
  '                else'#010+
  '                  begin'#010+
  '                    if WarnIfNotFound then'#010+
  '                      Log(vlWarning,SWarnIncludeFileNotFound,[D.Value,'+
  ' APackage.Name, MakeTargetString(ACPU,AOS)]);'#010+
  '                    APackage.FAllFi','lesResolved:=false;'#010+
  '                    D.TargetFileName:='#039#039';'#010+
  '                  end;'#010+
  '              end;'#010+
  '          end;'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  '  procedure FindExampleSource(T:TTarget);'#010+
  '  var'#010+
  '    SD,SF  : String;'#010+
  '  begin'#010+
  '    LogSearchPath(APackag','e,'#039'package example'#039',APackage.Exampl'+
  'ePath,ACPU,AOS);'#010+
  '    SD:=APackage.Dictionary.ReplaceStrings(T.Directory);'#010+
  '    SF:=APackage.Dictionary.ReplaceStrings(T.SourceFileName);'#010+
  '    if SD='#039#039' then'#010+
  '      FindFileInPath(APackage, APackage.ExamplePath',',SF,SD,ACPU,AOS);'+
  #010+
  '    if SD<>'#039#039' then'#010+
  '      SD:=IncludeTrailingPathDelimiter(SD);'#010+
  '    T.FTargetSourceFileName:=SD+SF;'#010+
  '    if FileExists(AddPathPrefix(APackage,T.TargetSourceFileName)) then'+
  #010+
  '      Log(vlDebug,SDbgResolvedSourceFile,[T.SourceFi','leName,T.TargetS'+
  'ourceFileName])'#010+
  '    else'#010+
  '      begin'#010+
  '        if WarnIfNotFound then'#010+
  '          Log(vlWarning,SWarnSourceFileNotFound,[T.SourceFileName, APa'+
  'ckage.Name, MakeTargetString(ACPU,AOS)]);'#010+
  '        T.FTargetSourceFileName:='#039#039';'#010+
  '       ',' APackage.FAllFilesResolved:=false;'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  'var'#010+
  '  T : TTarget;'#010+
  '  i : Integer;'#010+
  'begin'#010+
  '  if not((ACPU in APackage.CPUs) and (AOS in APackage.OSes)) then'#010+
  '    exit;'#010+
  '  if APackage.FAllFilesResolved then'#010+
  '    Exit;'#010+
  '  APackage.FAllFilesRe','solved:=true;'#010+
  '  try'#010+
  '    if DoChangeDir and (APackage.Directory<>'#039#039') then'#010+
  '      GPathPrefix := APackage.Directory;'#010+
  '    APackage.Dictionary.AddVariable('#039'CPU'#039',CPUToString(ACPU));'#010+
  '    APackage.Dictionary.AddVariable('#039'OS'#039',OSToString(AOS));'#010+
  '    F','or I:=0 to APackage.Targets.Count-1 do'#010+
  '      begin'#010+
  '        T:=APackage.FTargets.TargetItems[I];'#010+
  '        if (ACPU in T.CPUs) and (AOS in T.OSes) then'#010+
  '          begin'#010+
  '            // Debug information'#010+
  '            Log(vlDebug,SDbgResolvingSourc','esOfTarget,[T.Name,MakeTar'+
  'getString(ACPU,AOS)]);'#010+
  '            LogIndent;'#010+
  #010+
  '            case T.TargetType of'#010+
  '              ttProgram,'#010+
  '              ttUnit,'#010+
  '              ttImplicitUnit :'#010+
  '                begin'#010+
  '                  if T.FTargetSour','ceFileName<>'#039#039' then'#010+
  '                    Log(vlDebug,SDbgSourceAlreadyResolved,[T.Name])'#010+
  '                  else'#010+
  '                    FindMainSource(T);'#010+
  '                  if T.Dependencies.Count>0 then'#010+
  '                    FindIncludeSources(T)',';'#010+
  '                end;'#010+
  '              ttExampleUnit,'#010+
  '              ttExampleProgram :'#010+
  '                begin'#010+
  '                  if T.FTargetSourceFileName<>'#039#039' then'#010+
  '                    Log(vlDebug,SDbgSourceAlreadyResolved,[T.Name])'#010+
  '           ','       else'#010+
  '                    FindExampleSource(T);'#010+
  '                end;'#010+
  '            end;'#010+
  #010+
  '            LogUnIndent;'#010+
  '          end;'#010+
  '      end;'#010+
  '  finally'#010+
  '    If DoChangeDir and (APackage.Directory<>'#039#039') then'#010+
  '      GPathPrefix := '#039#039';'#010+
  '  end;'#010+
  'e','nd;'#010+
  #010+
  #010+
  'function TBuildEngine.GetUnitDir(APackage:TPackage):String;'#010+
  'begin'#010+
  '  if APackage.UnitDir='#039#039' then'#010+
  '    begin'#010+
  '      // Retrieve Full directory name where to find the units.'#010+
  '      // The search order is:'#010+
  '      //  - Package in this fpmake.','pp'#010+
  '      //  - LocalUnitDir'#010+
  '      //  - GlobalUnitDir'#010+
  '      if (APackage.State in [tsCompiled, tsNoCompile, tsInstalled]) th'+
  'en'#010+
  '        begin'#010+
  '          APackage.UnitDir:=IncludeTrailingPathDelimiter(FStartDir)+In'+
  'cludeTrailingPathDelimiter(AP','ackage.Directory)+APackage.GetUnitsOutp'+
  'utDir(Defaults.CPU,Defaults.OS);'#010+
  '        end;'#010+
  '      if (APackage.UnitDir='#039#039') and'#010+
  '         (Defaults.LocalUnitDir<>'#039#039') then'#010+
  '        begin'#010+
  '          APackage.UnitDir:=IncludeTrailingPathDelimiter(Default','s.Lo'+
  'calUnitDir)+APackage.Name+PathDelim+APackage.GetPackageUnitInstallDir('+
  'defaults.CPU, Defaults.OS);'#010+
  '          if not SysDirectoryExists(APackage.UnitDir) then'#010+
  '            APackage.UnitDir:='#039#039';'#010+
  '        end;'#010+
  '      if APackage.UnitDir='#039#039' the','n'#010+
  '        begin'#010+
  '          APackage.UnitDir:=IncludeTrailingPathDelimiter(Defaults.Glob'+
  'alUnitDir)+APackage.Name+PathDelim+APackage.GetPackageUnitInstallDir(d'+
  'efaults.CPU, Defaults.OS);'#010+
  '          if not SysDirectoryExists(APackage.UnitDir) the','n'#010+
  '            APackage.UnitDir:=DirNotFound;'#010+
  '        end;'#010+
  #010+
  '      if (APackage.UnitDir<>DirNotFound) then'#010+
  '        begin'#010+
  '          if FileExists(IncludeTrailingPathDelimiter(APackage.UnitDir)'+
  '+FPMakePPFile) then'#010+
  '            begin'#010+
  '             ',' // The package is not installed, but the source-path i'+
  's detected.'#010+
  '              // It is an external package so it is impossible to comp'+
  'ile it, so'#010+
  '              // assume that it has been compiled earlier.'#010+
  '              APackage.UnitDir :=',' IncludeTrailingPathDelimiter(APack'+
  'age.UnitDir) + APackage.GetUnitsOutputDir(Defaults.CPU,Defaults.OS);'#010+
  '              // If the unit-directory does not exist, you know for su'+
  're that'#010+
  '              // the package is not compiled'#010+
  '             ',' if not SysDirectoryExists(APackage.UnitDir) then'#010+
  '                APackage.UnitDir:=DirNotFound'#010+
  '              else'#010+
  '                // Set the state to tsNoCompile and not tsCompiled. Be'+
  'cause packages'#010+
  '                // in the tsCompiled sta','te trigger a rebuild of pack'+
  'ages that depend'#010+
  '                // on it.'#010+
  '                APackage.FTargetState:=tsNoCompile;'#010+
  '            end'#010+
  '          else if not (APackage.FTargetState in [tsCompiled, tsNoCompi'+
  'le]) then'#010+
  '            begin'#010+
  '   ','           APackage.FTargetState:=tsInstalled;'#010+
  '            end;'#010+
  '        end;'#010+
  '    end;'#010+
  #010+
  '  // Special error marker to prevent searches in case of error'#010+
  '  if APackage.UnitDir=DirNotFound then'#010+
  '    Result:='#039#039#010+
  '  else'#010+
  '    Result:=APackage.UnitDir;',#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.AddDependencyPaths(L: TStrings; DependencyType:'+
  ' TDependencyType; ATarget: TTarget);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  D : TDependency;'#010+
  '  SD : String;'#010+
  'begin'#010+
  '  For I:=0 to ATarget.Dependencies.Count-1 do'#010+
  '    begin'#010+
  '      D:=A','Target.Dependencies[i];'#010+
  '      if (D.DependencyType=DependencyType) and'#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '        begin'#010+
  '          SD:=ExcludeTrailingPathDelimiter(ExtractFilePath(D.TargetFil'+
  'eName));'#010+
  '         ',' if SD<>'#039#039' then'#010+
  '            L.Add(SD);'#010+
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
  '  For I:=0 to APacka','ge.Dependencies.Count-1 do'#010+
  '    begin'#010+
  '      D:=APackage.Dependencies[i];'#010+
  '      if (D.DependencyType=depPackage) and'#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '        begin'#010+
  '          P:=TPackage(D.Target);'#010+
  '          I','f Assigned(P) then'#010+
  '            begin'#010+
  '              // Already processed?'#010+
  '              S:=GetUnitDir(P);'#010+
  '              if L.IndexOf(S)=-1 then'#010+
  '                begin'#010+
  '                  // Add this package and then dependencies'#010+
  '               ','   L.Add(S);'#010+
  '                  AddDependencyUnitPaths(L,P);'#010+
  '                end;'#010+
  '            end;'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function TBuildEngine.AddPathPrefix(APackage: TPackage; APath: string)'+
  ': string;'#010+
  'begin'#010+
  '  if IsRelativePath(APath) a','nd (GPathPrefix<>'#039#039') then'#010+
  '    result := IncludeTrailingPathDelimiter(GPathPrefix) + APath'#010+
  '  else'#010+
  '    result := APath;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.GetCompilerCommand(APackage : TPackage; ATarget '+
  ': TTarget; Env: TStrings) : String;'#010+
  'Var'#010+
  '  L :',' TUnsortedDuplicatesStringList;'#010+
  '  Args : TStringList;'#010+
  '  s : string;'#010+
  '  i : Integer;'#010+
  'begin'#010+
  '  if ATarget.TargetSourceFileName = '#039#039' then'#010+
  '    Error(SErrCouldNotCompile,[ATarget.Name, APackage.Name]);'#010+
  #010+
  '  Args:=TStringList.Create;'#010+
  '  Args.Duplicate','s:=dupIgnore;'#010+
  #010+
  '  Result := '#039#039';'#010+
  #010+
  '  //compiler configuration'#010+
  '  if Defaults.NoFPCCfg then'#010+
  '    Args.Add('#039'-n'#039');'#010+
  #010+
  '  // Target OS'#010+
  '  Args.Add('#039'-T'#039'+OSToString(Defaults.OS));'#010+
  #010+
  '  // Target CPU.'#010+
  '  // This setting is only applicable when '#039'fpc'#039' is used a','s c'+
  'ompiler-executable.'#010+
  '  if ExtractFileName(GetCompiler) = '#039'fpc'#039' then'#010+
  '    Args.Add('#039'-P'#039'+CPUToString(Defaults.CPU));'#010+
  #010+
  '  // Compile mode'#010+
  '  If ATarget.Mode<>cmFPC then'#010+
  '    Args.Add('#039'-M'#039'+ModeToString(ATarget.Mode))'#010+
  '  else If Defaults.Mode<>cmFP','C then'#010+
  '    Args.Add('#039'-M'#039'+ModeToString(Defaults.Mode));'#010+
  '  // Output file paths'#010+
  '  If ATarget.TargetType in ProgramTargets then'#010+
  '    Args.Add('#039'-FE'#039'+AddPathPrefix(APackage,APackage.GetBinOutpu'+
  'tDir(Defaults.CPU,Defaults.OS)));'#010+
  '  Args.Add('#039'-FU'#039'+Ad','dPathPrefix(APackage,APackage.GetUnitsOut'+
  'putDir(Defaults.CPU,Defaults.OS)));'#010+
  '  // Object Path'#010+
  '  L:=TUnsortedDuplicatesStringList.Create;'#010+
  '  L.Duplicates:=dupIgnore;'#010+
  '  AddConditionalStrings(APackage, L,APackage.ObjectPath,Defaults.CPU,D'+
  'efault','s.OS);'#010+
  '  AddConditionalStrings(APackage, L,ATarget.ObjectPath,Defaults.CPU,De'+
  'faults.OS);'#010+
  '  for i:=0 to L.Count-1 do'#010+
  '    Args.Add('#039'-Fo'#039'+AddPathPrefix(APackage,L[i]));'#010+
  '  FreeAndNil(L);'#010+
  '  // Unit Dirs'#010+
  '  L:=TUnsortedDuplicatesStringList.Create;',#010+
  '  L.Duplicates:=dupIgnore;'#010+
  '  AddDependencyUnitPaths(L,APackage);'#010+
  '  AddDependencyPaths(L,depUnit,ATarget);'#010+
  '  AddConditionalStrings(APackage, L,APackage.UnitPath,Defaults.CPU,Def'+
  'aults.OS);'#010+
  '  AddConditionalStrings(APackage, L,ATarget.UnitPath',',Defaults.CPU,De'+
  'faults.OS);'#010+
  '  for i:=0 to L.Count-1 do'#010+
  '    Args.Add('#039'-Fu'#039'+AddPathPrefix(APackage,L[i]));'#010+
  '  FreeAndNil(L);'#010+
  '  // Include Path'#010+
  '  L:=TUnsortedDuplicatesStringList.Create;'#010+
  '  L.Duplicates:=dupIgnore;'#010+
  '  AddDependencyPaths(L,depIncl','ude,ATarget);'#010+
  '  AddConditionalStrings(APackage, L,APackage.IncludePath,Defaults.CPU,'+
  'Defaults.OS);'#010+
  '  AddConditionalStrings(APackage, L,ATarget.IncludePath,Defaults.CPU,D'+
  'efaults.OS);'#010+
  '  for i:=0 to L.Count-1 do'#010+
  '    Args.Add('#039'-Fi'#039'+AddPathPrefix','(APackage,L[i]));'#010+
  '  FreeAndNil(L);'#010+
  '  // Custom Options'#010+
  '  If (Defaults.HaveOptions) then'#010+
  '    Args.AddStrings(Defaults.Options);'#010+
  #010+
  '  APackage.ApplyPackageVariantToCompilerOptions(Args);'#010+
  #010+
  '  If (APackage.HaveOptions) then'#010+
  '    Args.AddStrings(APa','ckage.Options);'#010+
  '  If (ATarget.HaveOptions) then'#010+
  '    Args.AddStrings(ATarget.Options);'#010+
  #010+
  '  {$ifdef HAS_UNIT_PROCESS}'#010+
  '  // Force the compiler-output to be easy parseable'#010+
  '  if not Verbose then'#010+
  '    args.Add('#039'-viq'#039');'#010+
  '  {$endif}'#010+
  #010+
  '  // Convert to s','tring'#010+
  '  Result:='#039#039';'#010+
  '  for i:=0 to Args.Count-1 do'#010+
  '    Result:=Result+'#039' '#039'+maybequoted(Args[i]);'#010+
  '  Delete(result,1,1);'#010+
  #010+
  '  if Defaults.UseEnvironment and assigned(Env) then'#010+
  '    begin'#010+
  '      env.Values['#039'FPCEXTCMD'#039'] := Result;'#010+
  '      result := '#039'!F','PCEXTCMD'#039';'#010+
  '      // Make sure that this process'#039' environment variables are pa'+
  'ssed to the'#010+
  '      // compiler'#039's environment'#010+
  '      for i := 0 to GetEnvironmentVariableCount-1 do'#010+
  '        env.Add(GetEnvironmentString(i));'#010+
  '    end;'#010+
  #010+
  '  // Add Filen','ame to compile'#010+
  '  result := result + '#039' '#039' + AddPathPrefix(APackage,ATarget.Targ'+
  'etSourceFileName);'#010+
  #010+
  '  Args.Free;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.GetCompiler : String;'#010+
  'Var'#010+
  '  S : String;'#010+
  'begin'#010+
  '  // Cache in FCompiler for speed.'#010+
  '  If (FCompiler='#039#039') ','then'#010+
  '    begin'#010+
  '    FCompiler:=Defaults.Compiler;'#010+
  '    If (ExtractFilePath(FCompiler)='#039#039') then'#010+
  '      begin'#010+
  '      S:=ExeSearch(FCompiler,GetEnvironmentVariable('#039'PATH'#039'));'#010+
  '      If (S<>'#039#039') then'#010+
  '         FCompiler:=S;'#010+
  '      end;'#010+
  '    end;'#010+
  '  Result',':=FCompiler;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CreateOutputDir(APackage: TPackage);'#010+
  'Var'#010+
  '  D : String;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  //create a units directory'#010+
  '  D:=AddPathPrefix(APackage,APackage.GetUnitsOutputDir(Defaults.CPU,De'+
  'faults.OS));'#010+
  '  If not S','ysDirectoryExists(D) then'#010+
  '    begin'#010+
  '      Log(vlInfo,SInfoCreatingOutputDir,[D]);'#010+
  '      CmdCreateDir(D);'#010+
  '    end;'#010+
  #010+
  '  //also create a bin directory for programtargets'#010+
  '  For i := 0 to Pred(APackage.Targets.Count) do'#010+
  '    begin'#010+
  '      if APackag','e.Targets.TargetItems[i].TargetType in (ProgramTarge'+
  'ts-[ttExampleProgram]) then'#010+
  '        begin'#010+
  '          D:=AddPathPrefix(APackage,APackage.GetBinOutputDir(Defaults.'+
  'CPU,Defaults.OS));'#010+
  '          If not SysDirectoryExists(D) then'#010+
  '            b','egin'#010+
  '              Log(vlInfo,SInfoCreatingOutputDir,[D]);'#010+
  '              CmdCreateDir(D);'#010+
  '            end;'#010+
  '          //do not continue loop, directory is made anyway'#010+
  '          break;'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.Depend','encyOK(ADependency : TDependency) : Bool'+
  'ean;'#010+
  'begin'#010+
  '  Result:=(Defaults.CPU in ADependency.CPUs) and (Defaults.OS in ADepe'+
  'ndency.OSes);'#010+
  'end;'#010+
  #010+
  'function TBuildEngine.TargetOK(ATarget: TTarget; ACPU: TCPU; AOS: TOS)'+
  ': Boolean;'#010+
  'begin'#010+
  '  if Default','s.SkipCrossPrograms and'#010+
  '     (ATarget.TargetType in ProgramTargets) and'#010+
  '     IsDifferentFromBuild(ACPU, AOS) then'#010+
  '    result := False'#010+
  '  else'#010+
  '    Result:=(ACPU in ATarget.CPUs) and (AOS in ATarget.OSes);'#010+
  'end;'#010+
  #010+
  'function TBuildEngine.TargetIns','tallOK(ATarget: TTarget; ACPU: TCPU; '+
  'AOS: TOS): Boolean;'#010+
  'begin'#010+
  '  result := TargetOK(ATarget, ACPU, AOS) and ATarget.Install;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.PackageOK(APackage : TPackage) : Boolean;'#010+
  'begin'#010+
  '  Result:=(Defaults.CPU in APackage.CP','Us) and (Defaults.OS in APacka'+
  'ge.OSes);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoBeforeCompile(APackage: TPackage);'#010+
  'begin'#010+
  '  ExecuteCommands(APackage.Commands,caBeforeCompile);'#010+
  '  If Assigned(APackage.BeforeCompile) then'#010+
  '    APackage.BeforeCompile(APa','ckage);'#010+
  '  If Assigned(APackage.BeforeCompileProc) then'#010+
  '    APackage.BeforeCompileProc(APackage);'#010+
  '  // It could be that files that weren'#039't found before are availabl'+
  'e now.'#010+
  '  ResolveFileNames(APackage,Defaults.CPU,Defaults.OS,false,true);'#010+
  'end;',#010+
  #010+
  #010+
  'procedure TBuildEngine.DoAfterCompile(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterCompile) then'#010+
  '    APackage.AfterCompile(APackage);'#010+
  '  If Assigned(APackage.AfterCompileProc) then'#010+
  '    APackage.AfterCompileProc(APackage);'#010+
  '  Exec','uteCommands(APackage.Commands,caAfterCompile);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.NeedsCompile(APackage:TPackage;ATarget: TTarget)'+
  ': Boolean;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  D : TDependency;'#010+
  '  T : TTarget;'#010+
  '  OD,OFN,TFN : String;'#010+
  '  CompileReason: String;'#010+
  'begi','n'#010+
  '  Result:=False;'#010+
  #010+
  '  // Forced recompile?'#010+
  '  if FForceCompile then'#010+
  '    begin'#010+
  '    Result:=true;'#010+
  '    CompileReason:=SDbgForcedCompile;'#010+
  '    end;'#010+
  #010+
  '  // For now examples are not compiled at all'#010+
  '  if ATarget.TargetType in [ttExampleUnit, ttExampl','eProgram] then'#010+
  '    Exit;'#010+
  #010+
  '  // Files which should not be compiled on this target can not trigger'+
  ' a compile.'#010+
  '  if not TargetOK(ATarget, Defaults.CPU, Defaults.OS) then'#010+
  '    Exit;'#010+
  #010+
  '  // Check output file'#010+
  '  if not result then'#010+
  '    begin'#010+
  '      if',' ATarget.TargetType in ProgramTargets then'#010+
  '        OD:=APackage.GetBinOutputDir(Defaults.CPU,Defaults.OS)'#010+
  '      else'#010+
  '        OD:=APackage.GetUnitsOutputDir(Defaults.CPU,Defaults.OS);'#010+
  '      If (OD<>'#039#039') then'#010+
  '        OD:=IncludeTrailingPathDel','imiter(OD);'#010+
  '      OFN:=AddPathPrefix(APackage, OD+ATarget.GetOutPutFileName(Defaul'+
  'ts.OS));'#010+
  '      Result:=Not FileExists(OFN);'#010+
  '      if Result then'#010+
  '        begin'#010+
  '        CompileReason:=SDbgOutputDoesNotExist;'#010+
  '        Log(vlDebug,SDbgOutputNo','tYetAvailable,[OFN]);'#010+
  '        end;'#010+
  '    end;'#010+
  #010+
  '  // Check main source'#010+
  '  If not Result then'#010+
  '    begin'#010+
  '      TFN := AddPathPrefix(APackage,ATarget.TargetSourceFileName);'#010+
  '      if FileExists(TFN) then'#010+
  '        Result:=FileNewer(TFN,OFN);'#010+
  '      if',' Result then'#010+
  '        CompileReason:=SDbgNewerSource;'#010+
  '    end;'#010+
  #010+
  '  // Check unit and include dependencies'#010+
  '  If not Result then'#010+
  '    begin'#010+
  '      ResolveDependencies(ATarget.Dependencies,ATarget.Collection as T'+
  'Targets);'#010+
  '      I:=0;'#010+
  '      for i:=','0 to ATarget.Dependencies.Count-1 do'#010+
  '        begin'#010+
  '          D:=ATarget.Dependencies[i];'#010+
  '          if (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'+
  #010+
  '            begin'#010+
  '              case D.DependencyType of'#010+
  '                depUnit',' :'#010+
  '                  begin'#010+
  '                    T:=TTarget(D.Target);'#010+
  '                    If (T=Nil) then'#010+
  '                      Error(SErrDepUnknownTarget,[D.Value, ATarget.Nam'+
  'e, APackage.Name]);'#010+
  '                    // If a dependent package',' is compiled we always '+
  'need to recompile'#010+
  '                    Log(vldebug, SDbgDependencyOnUnit, [ATarget.Name,T'+
  '.Name]);'#010+
  '                    Result:=(T.State=tsCompiled);'#010+
  '                    if Result then'#010+
  '                      begin'#010+
  '       ','               Log(vldebug, SDbgDependencyUnitRecompiled, [T.'+
  'Name]);'#010+
  '                      CompileReason:=Format(SDbgDependencyRecompiled,['+
  'T.Name]);'#010+
  '                      end;'#010+
  '                  end;'#010+
  '                depInclude :'#010+
  '            ','      begin'#010+
  '                    if D.TargetFileName<>'#039#039' then'#010+
  '                      begin'#010+
  '                        TFN:=AddPathPrefix(APackage,D.TargetFileName);'+
  #010+
  '                        Result:=FileNewer(TFN,OFN);'#010+
  '                        if r','esult then'#010+
  '                          CompileReason:=Format(SDbgNewerInclude,[D.Ta'+
  'rgetFileName]);'#010+
  '                      end;'#010+
  '                  end;'#010+
  '                depPackage :'#010+
  '                  begin'#010+
  '                    log(vlWarning,SWarnT','argetDependsOnPackage,[ATarg'+
  'et.Name, APackage.Name, d.Value]);'#010+
  '                  end;'#010+
  '              end;'#010+
  '              if result then'#010+
  '                break;'#010+
  '            end;'#010+
  '        end;'#010+
  '    end;'#010+
  #010+
  '  if result then'#010+
  '    Log(vlDebug,SDbgMustCo','mpile,[ATarget.Name, CompileReason]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Compile(APackage: TPackage; ATarget: TTarget);'#010+
  'Var'#010+
  '  S : String;'#010+
  '  Env : TStrings;'#010+
  'begin'#010+
  '  Log(vlInfo,SInfoCompilingTarget,[ATarget.Name]);'#010+
  '  LogIndent;'#010+
  '  ExecuteCommands(A','Target.Commands,caBeforeCompile);'#010+
  '  If Assigned(ATarget.BeforeCompile) then'#010+
  '    ATarget.BeforeCompile(ATarget);'#010+
  #010+
  '  if (APackage.BuildMode=bmBuildUnit) and not (ATarget.TargetType in ['+
  'ttProgram,ttExampleProgram]) then'#010+
  '    begin'#010+
  '      APackag','e.FBUTarget.Dependencies.AddUnit(ATarget.Name).FTargetF'+
  'ileName:=ATarget.TargetSourceFileName;'#010+
  '    end'#010+
  '  else'#010+
  '    begin'#010+
  '      if Defaults.UseEnvironment then'#010+
  '        begin'#010+
  '          Env := TStringList.Create;'#010+
  '          try'#010+
  '            S:=Get','CompilerCommand(APackage,ATarget,Env);'#010+
  '            ExecuteCommand(GetCompiler,S,Env);'#010+
  '          finally'#010+
  '            Env.Free;'#010+
  '          end;'#010+
  '        end'#010+
  '      else'#010+
  '        begin'#010+
  '          S:=GetCompilerCommand(APackage,ATarget,Env);'#010+
  '       ','   ExecuteCommand(GetCompiler,S,nil);'#010+
  '        end;'#010+
  '      If Assigned(ATarget.AfterCompile) then'#010+
  '        ATarget.AfterCompile(ATarget);'#010+
  '      ExecuteCommands(ATarget.Commands,caAfterCompile);'#010+
  '    end;'#010+
  '  LogUnIndent;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEn','gine.CompileDependencies(APackage:TPackage; ATarge'+
  't: TTarget);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  T : TTarget;'#010+
  '  D : TDependency;'#010+
  'begin'#010+
  '  Log(vlDebug, Format(SDbgCompilingDependenciesOfTarget, [ATarget.Name'+
  ']));'#010+
  '  LogIndent;'#010+
  '  For I:=0 to ATarget.Dependen','cies.Count-1 do'#010+
  '    begin'#010+
  '      D:=ATarget.Dependencies[i];'#010+
  '      if (D.DependencyType=depPackage) then'#010+
  '        log(vlWarning,SWarnTargetDependsOnPackage,[ATarget.Name, APack'+
  'age.Name, d.Value])'#010+
  '      else if (D.DependencyType=depUnit) and'#010+
  ' ','        (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '        begin'#010+
  '          T:=TTarget(D.Target);'#010+
  '          if Assigned(T) and (T<>ATarget) then'#010+
  '            begin'#010+
  '              if TargetOK(T, Defaults.CPU, Defaults.OS) then'#010+
  '  ','              begin'#010+
  '                  // We don'#039't need to compile implicit units, they'+
  ' are only'#010+
  '                  // used for dependency checking'#010+
  '                  if (T.TargetType<>ttImplicitUnit) then'#010+
  '                    begin'#010+
  '           ','           case T.State of'#010+
  '                        tsNeutral :'#010+
  '                          MaybeCompile(APackage,T);'#010+
  '                        tsConsidering :'#010+
  '                          Log(vlWarning,SWarnCircularTargetDependency,'+
  '[ATarget.Name,T','.Name]);'#010+
  '                      end;'#010+
  '                    end;'#010+
  '                end'#010+
  '              else'#010+
  '                Log(vlWarning, Format(SWarnDepUnitNotFound, [T.Name, M'+
  'akeTargetString(Defaults.CPU,Defaults.OS)]));'#010+
  '            end'#010+
  '        ','  else'#010+
  '            Error(SErrDepUnknownTarget,[D.Value, ATarget.Name, APackag'+
  'e.Name]);'#010+
  '        end;'#010+
  '    end;'#010+
  '  LogUnIndent;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.MaybeCompile(APackage: TPackage; ATarget: TTarg'+
  'et);'#010+
  'begin'#010+
  '  if ATarget.State<>tsNeutra','l then'#010+
  '    Error(SErrInvalidState,[ATarget.Name]);'#010+
  '  Log(vlDebug, Format(SDbgConsideringTarget, [ATarget.Name]));'#010+
  '  LogIndent;'#010+
  '  ATarget.FTargetState:=tsConsidering;'#010+
  '  ResolveDependencies(ATarget.Dependencies,ATarget.Collection as TTarg'+
  'ets)',';'#010+
  '  CompileDependencies(APackage, ATarget);'#010+
  '  if NeedsCompile(APackage, ATarget) then'#010+
  '    begin'#010+
  '      Compile(APackage,ATarget);'#010+
  '      ATarget.FTargetState:=tsCompiled;'#010+
  '    end'#010+
  '  else'#010+
  '    ATarget.FTargetState:=tsNoCompile;'#010+
  '  LogUnIndent;'#010+
  'en','d;'#010+
  #010+
  #010+
  'function TBuildEngine.NeedsCompile(APackage: TPackage): Boolean;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  D : TDependency;'#010+
  '  CompileReason: string;'#010+
  'begin'#010+
  '  Result:=False;'#010+
  #010+
  '  // Forced recompile?'#010+
  '  if FForceCompile then'#010+
  '    begin'#010+
  '    Result',':=true;'#010+
  '    CompileReason:=SDbgForcedCompile;'#010+
  '    end;'#010+
  #010+
  '  // Recompile because of Package Dependencies?'#010+
  '  if not Result then'#010+
  '    begin'#010+
  '       I:=0;'#010+
  '       For I:=0 to APackage.Dependencies.Count-1 do'#010+
  '         begin'#010+
  '           D:=APackage.De','pendencies[i];'#010+
  '           if (D.DependencyType=depPackage) and'#010+
  '              (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) the'+
  'n'#010+
  '             begin'#010+
  '               P:=TPackage(D.Target);'#010+
  '               if Assigned(P) then'#010+
  '             ','    begin'#010+
  '                   Result:=(P.State=tsCompiled);'#010+
  '                   if Result then'#010+
  '                     begin'#010+
  '                     break;'#010+
  '                     CompileReason:=Format(SDbgPackageDepRecompiled,[P'+
  '.Name]);'#010+
  '             ','        end;'#010+
  '                 end;'#010+
  '             end;'#010+
  '         end;'#010+
  '    end;'#010+
  #010+
  '  // Recompile a Target of this package?'#010+
  '  If Not Result then'#010+
  '    begin'#010+
  '      GPathPrefix := APackage.Directory;'#010+
  '      try'#010+
  '        for i:=0 to APackage.Targets.Cou','nt-1 do'#010+
  '          begin'#010+
  '            Result:=NeedsCompile(APackage,APackage.Targets.TargetItems'+
  '[i]);'#010+
  '            if Result then'#010+
  '              begin'#010+
  '              break;'#010+
  '              CompileReason:=Format(SDbgTargetHasToBeCompiled,[APackag'+
  'e.','Targets.TargetItems[i].Name]);'#010+
  '              end;'#010+
  '          end;'#010+
  '      Finally'#010+
  '        GPathPrefix := '#039#039';'#010+
  '      end;'#010+
  '    end;'#010+
  #010+
  '  if result then'#010+
  '    Log(vlDebug,SDbgMustCompile,[APackage.Name, CompileReason]);'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.Ch','eckExternalPackage(Const APackageName : Stri'+
  'ng):TPackage;'#010+
  'var'#010+
  '  S : String;'#010+
  '  F : String;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  // Already checked?'#010+
  '  I:=ExternalPackages.IndexOfName(APackageName);'#010+
  '  if I<>-1 then'#010+
  '    begin'#010+
  '      result:=ExternalPackages.P','ackageItems[I];'#010+
  '      exit;'#010+
  '    end;'#010+
  '  // Create new external package'#010+
  '  Result:=ExternalPackages.AddPackage(APackageName);'#010+
  '  Result.FTargetState:=tsNotFound;'#010+
  '  // Load unit config'#010+
  '  S:=GetUnitDir(Result);'#010+
  '  if S<>'#039#039' then'#010+
  '    begin'#010+
  '      Log','(vldebug, SDbgExternalDependency, [APackageName,S]);'#010+
  '      // Load unit config if it exists'#010+
  '      F:=IncludeTrailingPathDelimiter(S)+UnitConfigFile;'#010+
  '      if FileExists(F) then'#010+
  '        begin'#010+
  '          Log(vlDebug, Format(SDbgLoading, [F]));',#010+
  '          Result.LoadUnitConfigFromFile(F);'#010+
  '          result.SetDefaultPackageVariant;'#010+
  '          result.UnitDir:=result.UnitDir+Result.GetPackageUnitInstallD'+
  'ir(Defaults.CPU, Defaults.OS);'#010+
  '        end;'#010+
  '      // Check recursive implicit depe','ndencies'#010+
  '      CompileDependencies(Result);'#010+
  '    end'#010+
  '  else'#010+
  '    Error(SErrDependencyNotFound,[APackageName]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CompileDependencies(APackage: TPackage);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  D : TDependency;'#010+
  'begin'#010,
  '  For I:=0 to APackage.Dependencies.Count-1 do'#010+
  '    begin'#010+
  '      D:=APackage.Dependencies[i];'#010+
  '      if (D.DependencyType=depPackage) and'#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '        begin'#010+
  '          P:=TPackage(D.','Target);'#010+
  '          If Assigned(P) then'#010+
  '            begin'#010+
  '              if (Defaults.CPU in P.CPUs) and (Defaults.OS in P.OSes) '+
  'then'#010+
  '                begin'#010+
  '                  case P.State of'#010+
  '                    tsNeutral :'#010+
  '                    ','  MaybeCompile(P);'#010+
  '                    tsConsidering :'#010+
  '                      Log(vlWarning,SWarnCircularPackageDependency,[AP'+
  'ackage.Name,P.Name]);'#010+
  '                  end;'#010+
  '                end'#010+
  '              else'#010+
  '                Log(vlWarning,S','WarnDependOnOtherPlatformPackage,[APa'+
  'ckage.Name, D.Value, MakeTargetString(Defaults.CPU, Defaults.OS)]);'#010+
  '            end'#010+
  '          else'#010+
  '            begin'#010+
  '              D.Target:=CheckExternalPackage(D.Value);'#010+
  '              P:=TPackage(D.Tar','get);'#010+
  '            end;'#010+
  '          if (D.RequireChecksum<>$ffffffff) and'#010+
  '             (P.InstalledChecksum<>$ffffffff) and'#010+
  '             (P.InstalledChecksum<>D.RequireChecksum) then'#010+
  '            Log(vlDebug,SDbgPackageChecksumChanged,[P.Name])',';'#010+
  '          APackage.InheritPackageVariantsFromDependency(P);'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function TBuildEngine.CheckDependencies(APackage: TPackage): TCheckDep'+
  'endencyResult;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  D : TDependency;'#010+
  'begin'#010+
  '  res','ult := cdAvailable;'#010+
  '  For I:=0 to APackage.Dependencies.Count-1 do'#010+
  '    begin'#010+
  '      D:=APackage.Dependencies[i];'#010+
  '      if (D.DependencyType=depPackage) and'#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '        begin'#010+
  '    ','      P:=TPackage(D.Target);'#010+
  '          If Assigned(P) then'#010+
  '            begin'#010+
  '              if (Defaults.CPU in P.CPUs) and (Defaults.OS in P.OSes) '+
  'then'#010+
  '                begin'#010+
  '                  case P.State of'#010+
  '                    tsNeutral :'#010,
  '                      result := cdNotYetAvailable;'#010+
  '                    tsConsidering :'#010+
  '                      Log(vlWarning,SWarnCircularPackageDependency,[AP'+
  'ackage.Name,P.Name]);'#010+
  '                  end;'#010+
  '                end'#010+
  '              else',#010+
  '                Log(vlWarning,SWarnDependOnOtherPlatformPackage,[APack'+
  'age.Name, D.Value, MakeTargetString(Defaults.CPU, Defaults.OS)]);'#010+
  '            end'#010+
  '          else'#010+
  '            begin'#010+
  '              D.Target:=CheckExternalPackage(D.Value);',#010+
  '              P:=TPackage(D.Target);'#010+
  '            end;'#010+
  '          if (D.RequireChecksum<>$ffffffff) and'#010+
  '             (P.InstalledChecksum<>$ffffffff) and'#010+
  '             (P.InstalledChecksum<>D.RequireChecksum) then'#010+
  '            Log(vlDebug,SDbg','PackageChecksumChanged,[P.Name]);'#010+
  '          APackage.InheritPackageVariantsFromDependency(P);'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Compile(APackage: TPackage);'#010+
  'Var'#010+
  '  T : TTarget;'#010+
  '  I : Integer;'#010+
  '  Cmd: string;'#010+
  '  cmdOpts: strin','g;'#010+
  '  sFPDocFormat: string;'#010+
  '  IFPDocFormat: TFPDocFormat;'#010+
  '  d: integer;'#010+
  '  UC: string;'#010+
  '  dep: TDependency;'#010+
  '  RegenerateUnitconfigFile: boolean;'#010+
  '  BUName: string;'#010+
  #010+
  '  procedure CompileBuildUnit;'#010+
  '  var'#010+
  '    I: Integer;'#010+
  '    T: TTarget;'#010+
  '    L: TStr','ings;'#010+
  '    F: Text;'#010+
  #010+
  '  begin'#010+
  '    if (APackage.FBUTarget.Dependencies.Count>0) then'#010+
  '      begin'#010+
  '        Log(vldebug, Format(SDbgGenerateBuildUnit, [APackage.FBUTarget'+
  '.Name]));'#010+
  '        system.Assign(F,AddPathPrefix(APackage,APackage.FBUTarget.','FT'+
  'argetSourceFileName));'#010+
  '        Rewrite(F);'#010+
  '        writeln(F,'#039'unit '#039' + APackage.FBUTarget.Name +'#039';'#039+
  ');'#010+
  '        writeln(F,'#039'interface'#039');'#010+
  '        writeln(F,'#039'uses'#039');'#010+
  '        for i := 0 to APackage.FBUTarget.Dependencies.Count-1 do'#010+
  '          be','gin'#010+
  '            if i<>0 then'#010+
  '              write(F,'#039','#039');'#010+
  '            writeln(F,APackage.FBUTarget.Dependencies.Dependencies[i].'+
  'Value);'#010+
  '          end;'#010+
  '        writeln(F,'#039';'#039');'#010+
  '        writeln(F,'#039'implementation'#039');'#010+
  '        writeln(F,'#039'end.'#039');'#010+
  #010+
  ' ','       system.close(F);'#010+
  #010+
  '        APackage.FBuildMode:=bmOneByOne;'#010+
  '        try'#010+
  '          Compile(APackage,APackage.FBUTarget);'#010+
  '        finally'#010+
  '          // Delete temporary build-unit files'#010+
  '          L := TStringList.Create;'#010+
  '          try'#010+
  '  ','          APackage.FBUTarget.GetCleanFiles(L,IncludeTrailingPathDe'+
  'limiter(AddPathPrefix(APackage,APackage.GetUnitsOutputDir(Defaults.CPU'+
  ',Defaults.OS))),'#039#039',Defaults.CPU,Defaults.OS);'#010+
  '            L.Add(AddPathPrefix(APackage,APackage.FBUTarge','t.SourceFi'+
  'leName));'#010+
  '            CmdDeleteFiles(L);'#010+
  '          finally'#010+
  '            L.Free;'#010+
  '          end;'#010+
  '        end;'#010+
  '      end;'#010+
  #010+
  '    For I:=0 to APackage.Targets.Count-1 do'#010+
  '      begin'#010+
  '        T:=APackage.Targets.TargetItems[i];'#010+
  '        if ','(T.TargetType = ttUnit) and (TargetOK(T, Defaults.CPU, De'+
  'faults.OS)) then'#010+
  '          begin'#010+
  '            If Assigned(T.AfterCompile) then'#010+
  '              T.AfterCompile(T);'#010+
  '            ExecuteCommands(T.Commands,caAfterCompile);'#010+
  '          end'#010+
  '  ','    end;'#010+
  '  end;'#010+
  #010+
  '  procedure ProcessCompileTarget;'#010+
  '  begin'#010+
  '    if TargetOK(T, Defaults.CPU, Defaults.OS) then'#010+
  '      begin'#010+
  '        if T.State=tsNeutral then'#010+
  '          MaybeCompile(APackage,T);'#010+
  '        // If a target is compiled, re-generate ','the UnitConfigFile'#010+
  '        if T.FTargetState<>tsNoCompile then'#010+
  '          RegenerateUnitconfigFile:= True;'#010+
  '      end'#010+
  '    else'#010+
  '      begin'#010+
  '        if not(Defaults.CPU in T.CPUs) then'#010+
  '          Log(vldebug, Format(SDbgSkippingTargetWrongCPU, [','T.Name, C'+
  'PUsToString(T.CPUs)]));'#010+
  '        if not(Defaults.OS in T.OSes) then'#010+
  '          Log(vldebug, Format(SDbgSkippingTargetWrongOS, [T.Name, OSes'+
  'ToString(T.OSes)]));'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  'begin'#010+
  '  cmdOpts := '#039#039';'#010+
  #010+
  '  log(vlWarning,SWarnStartComp','ilingPackage,[APackage.Name, Defaults.'+
  'Target]);'#010+
  #010+
  '  case Defaults.BuildMode of'#010+
  '    bmOneByOne:  begin'#010+
  '                   if (bmOneByOne in APackage.SupportBuildModes) then'#010+
  '                     APackage.FBuildMode:=bmBuildUnit'#010+
  '               ','    else if bmBuildUnit in APackage.SupportBuildModes'+
  ' then'#010+
  '                     begin'#010+
  '                       log(vlInfo,SInfoFallbackBuildmodeBU);'#010+
  '                       APackage.FBuildMode:=bmBuildUnit;'#010+
  '                     end'#010+
  '           ','        else'#010+
  '                     raise exception.create(SErrUnsupportedBuildmode);'+
  #010+
  '                 end;'#010+
  '    bmBuildUnit: begin'#010+
  '                   // When bmBuildUnit is supported by the package use'+
  ' a buildunit.'#010+
  '                   // Unles','s there is only one target and bmOneByOne'+
  ' is also supported'#010+
  '                   if (bmBuildUnit in APackage.SupportBuildModes) and'#010+
  '                      not ((APackage.Targets.Count=1) and (bmOneByOne '+
  'in APackage.SupportBuildModes)) then'#010+
  '   ','                  APackage.FBuildMode:=bmBuildUnit'#010+
  '                   else if bmOneByOne in APackage.SupportBuildModes th'+
  'en'#010+
  '                     begin'#010+
  '                       log(vlInfo,SInfoFallbackBuildmode);'#010+
  '                       APackag','e.FBuildMode:=bmOneByOne'#010+
  '                     end'#010+
  '                   else'#010+
  '                     raise exception.create(SErrUnsupportedBuildmode);'+
  #010+
  '                 end;'#010+
  '  end;'#010+
  #010+
  '  GPathPrefix:=APackage.Directory;'#010+
  '  Try'#010+
  '    CreateOutputDir(APac','kage);'#010+
  '    APackage.Dictionary.AddVariable('#039'UNITSOUTPUTDIR'#039',AddPathPr'+
  'efix(APackage,APackage.GetUnitsOutputDir(Defaults.CPU,Defaults.OS)));'#010+
  '    APackage.Dictionary.AddVariable('#039'BINOUTPUTDIR'#039',AddPathPref'+
  'ix(APackage,APackage.GetBinOutputDir(De','faults.CPU,Defaults.OS)));'#010+
  '    DoBeforeCompile(APackage);'#010+
  '    RegenerateUnitconfigFile:=False;'#010+
  '    if APackage.BuildMode=bmBuildUnit then'#010+
  '      begin'#010+
  '        APackage.FBUTargets := TTargets.Create(TTarget);'#010+
  '        if Defaults.OS in AllLimi','t83fsOses then'#010+
  '          BUName := '#039'BUnit.pp'#039#010+
  '        else'#010+
  '          BUName := '#039'BuildUnit_'#039'+StringReplace(APackage.Name,'#039+
  '-'#039','#039'_'#039',[rfReplaceAll])+'#039'.pp'#039';'#010+
  '        APackage.FBUTarget := APackage.FBUTargets.AddUnit(BUName);'#010+
  '        APackage.FBUTa','rget.FTargetSourceFileName := APackage.FBUTarg'+
  'et.SourceFileName;'#010+
  '      end;'#010+
  '    For I:=0 to APackage.Targets.Count-1 do'#010+
  '      begin'#010+
  '        T:=APackage.Targets.TargetItems[i];'#010+
  '        case T.TargetType of'#010+
  '        ttUnit:'#010+
  '          begin'#010+
  '   ','         ProcessCompileTarget;'#010+
  '          end;'#010+
  '        ttProgram:'#010+
  '          begin // do nothing, are compiled later'#010+
  '          end;'#010+
  '        ttFPDoc:'#010+
  '          begin'#010+
  '            for d := 0 to T.Dependencies.Count - 1 do'#010+
  '            begin'#010+
  '     ','         dep := TDependency(T.Dependencies[d]);'#010+
  #010+
  '              //add unit dependencies'#010+
  '              if dep.DependencyType = depUnit then'#010+
  '                cmdOpts := cmdOpts + '#039' --input='#039' + AddPathPref'+
  'ix(APackage,dep.Value);'#010+
  '            end;',#010+
  #010+
  '            //check if a documentation target is given'#010+
  '            cmdOpts := cmdOpts + '#039' --input='#039' + AddPathPrefix(A'+
  'Package,T.Directory + T.Name + T.Extension) + '#039' --descr='#039'+ T.X'+
  'ML;'#010+
  '          end'#010+
  '        else'#010+
  '          log(vldebug, SDbgT','argetIsNotAUnitOrProgram,[T.Name]);'#010+
  '        end;'#010+
  '      end;'#010+
  #010+
  '    if APackage.BuildMode=bmBuildUnit then'#010+
  '      CompileBuildUnit;'#010+
  #010+
  '    FreeAndNil(APackage.FBUTarget);'#010+
  #010+
  '    For I:=0 to APackage.Targets.Count-1 do'#010+
  '      begin'#010+
  '        T:=APackag','e.Targets.TargetItems[i];'#010+
  '        if T.TargetType=ttProgram then'#010+
  '          begin'#010+
  '            ProcessCompileTarget;'#010+
  '          end;'#010+
  '      end;'#010+
  #010+
  '    if RegenerateUnitconfigFile then'#010+
  '      begin'#010+
  '        UC:=IncludeTrailingPathDelimiter(AddPathP','refix(APackage,APac'+
  'kage.GetUnitConfigOutputDir(Defaults.CPU,Defaults.OS)))+UnitConfigFile'+
  ';'#010+
  '        Log(vlInfo, Format(SDbgGenerating, [UC]));'#010+
  '        APackage.SaveUnitConfigToFile(UC,Defaults.CPU,Defaults.OS);'#010+
  '      end;'#010+
  #010+
  '    //compile docu','mentation, because options were found'#010+
  '    if cmdOpts <> '#039#039' then'#010+
  '    begin'#010+
  '      //append package name'#010+
  '      cmdOpts := cmdOpts + '#039' --package='#039' + APackage.Name;'#010+
  #010+
  '      for IFPDocFormat:=Low(TFPDocFormat) to High(TFPDocFormat) do'#010+
  '      begin'#010,
  '        if IFPDocFormat in APackage.FPDocFormat then'#010+
  '        begin'#010+
  '          //prepend output format'#010+
  '          case IFPDocFormat of'#010+
  '            ffHtml:      sFPDocFormat := '#039'--format=html --output='#039+
  ' + AddPathPrefix(APackage,Defaults.FPDocOu','tputDir);'#010+
  '            ffHtm:       sFPDocFormat := '#039'--format=htm --output='#039+
  ' + AddPathPrefix(APackage,Defaults.FPDocOutputDir);'#010+
  '            ffXHtml:     sFPDocFormat := '#039'--format=xhtml --output='+
  #039' + AddPathPrefix(APackage,Defaults.FPDocOutput','Dir);'#010+
  '            ffLaTex:     sFPDocFormat := '#039'--format=latex --output='+
  #039' + AddPathPrefix(APackage,Defaults.FPDocOutputDir) + APackage.Name '+
  '+ '#039'.tex'#039';'#010+
  '            ffXMLStruct: sFPDocFormat := '#039'--format=xml-struct --ou'+
  'tput='#039' + AddPathPrefix(A','Package,Defaults.FPDocOutputDir);'#010+
  '            ffChm:       sFPDocFormat := '#039'--format=chm --output='#039+
  ' + AddPathPrefix(APackage,Defaults.FPDocOutputDir) + APackage.Name + '#039+
  '.chm'#039';'#010+
  '          end;'#010+
  #010+
  '          //execute fpdoc'#010+
  '          Cmd:=ExeSear','ch('#039'fpdoc'#039',GetEnvironmentvariable('#039+
  'PATH'#039'));'#010+
  '          if Cmd = '#039#039' then Cmd := '#039'fpdoc'#039';'#010+
  '          ExecuteProcess(Cmd, sFPDocFormat + cmdOpts);'#010+
  '        end;'#010+
  '      end;'#010+
  '    end;'#010+
  #010+
  '    DoAfterCompile(APackage);'#010+
  '  Finally'#010+
  '    GPathPrefix:='#039#039';'#010+
  '  e','nd;'#010+
  '  inc(FProgressCount);'#010+
  '  if FProgressMax>0 then'#010+
  '    log(vlWarning,SWarnCompilingPackagecompleteProgress,[(FProgressCou'+
  'nt)/FProgressMax * 100, APackage.Name])'#010+
  '  else'#010+
  '    log(vlWarning,SWarnCompilingPackagecomplete,[APackage.Name]);'#010+
  'end;'#010,
  #010+
  'procedure TBuildEngine.MaybeCompile(APackage: TPackage);'#010+
  'begin'#010+
  '  if ReadyToCompile(APackage) then'#010+
  '    begin'#010+
  '      Compile(APackage);'#010+
  '      APackage.FTargetState:=tsCompiled;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function TBuildEngine.ReadyToCompile(APackage: TPa','ckage) : Boolean;'#010+
  'begin'#010+
  '  result := False;'#010+
  '  if APackage.State in [tsCompiled, tsNoCompile] then'#010+
  '    begin'#010+
  '      Log(vlInfo,SInfoPackageAlreadyProcessed,[APackage.Name]);'#010+
  '      Exit;'#010+
  '    end;'#010+
  '  if APackage.State<>tsNeutral then'#010+
  '    Error(SE','rrInvalidState,[APackage.Name]);'#010+
  '  Log(vlDebug,SDbgConsideringPackage,[APackage.Name]);'#010+
  '  LogIndent;'#010+
  '  if Defaults.ThreadsAmount=-1 then'#010+
  '    APackage.FTargetState:=tsConsidering;'#010+
  '  ResolveDependencies(APackage.Dependencies,(APackage.Collect','ion as '+
  'TPackages));'#010+
  '  // When multiple threads are used, delay the compilation of the pack'+
  'age when'#010+
  '  // there are unsolved dependencies. When no threads are used, compil'+
  'e all'#010+
  '  // dependencies.'#010+
  '  if Defaults.ThreadsAmount=-1 then'#010+
  '    Compil','eDependencies(APackage)'#010+
  '  else if CheckDependencies(APackage)=cdNotYetAvailable then'#010+
  '    begin'#010+
  '      log(vlInfo,'#039'Delaying package '#039'+apackage.name);'#010+
  '      result := False;'#010+
  '      Exit;'#010+
  '    end;'#010+
  '  APackage.SetDefaultPackageVariant;'#010+
  #010+
  '  ResolveF','ileNames(APackage,Defaults.CPU,Defaults.OS,True,False);'#010+
  '  If NeedsCompile(APackage) then'#010+
  '    result := True'#010+
  '  else'#010+
  '    begin'#010+
  '      APackage.FTargetState:=tsNoCompile;'#010+
  '      inc(FProgressCount);'#010+
  '    end;'#010+
  '  LogUnIndent;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuild','Engine.InstallPackageFiles(APAckage : TPackage; tt : '+
  'TTargetTypes; Const Dest : String):Boolean;'#010+
  'Var'#010+
  '  List : TStringList;'#010+
  'begin'#010+
  '  Result:=False;'#010+
  '  List:=TStringList.Create;'#010+
  '  Try'#010+
  '    APackage.GetInstallFiles(List,tt,Defaults.CPU, Defaults.','OS);'#010+
  '    if (List.Count>0) then'#010+
  '      begin'#010+
  '        Result:=True;'#010+
  '        CmdCopyFiles(List,Dest);'#010+
  '      end;'#010+
  '  Finally'#010+
  '    List.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.InstallUnitConfigFile(APAckage: TPackage; const'+
  ' Dest: String);'#010+
  'Var'#010+
  '  ','List : TStringList;'#010+
  'begin'#010+
  '  List:=TStringList.Create;'#010+
  '  Try'#010+
  '    List.add(IncludeTrailingPathDelimiter(APackage.GetUnitConfigOutput'+
  'Dir(Defaults.CPU,Defaults.OS))+UnitConfigFile);'#010+
  '    CmdCopyFiles(List,Dest);'#010+
  '  Finally'#010+
  '    List.Free;'#010+
  '  end;'#010+
  'e','nd;'#010+
  #010+
  'function TBuildEngine.InstallPackageSourceFiles(APAckage : TPackage; s'+
  'tt : TSourceTypes; ttt : TTargetTypes; Const Dest : String): Boolean;'#010+
  'Var'#010+
  '  List : TStringList;'#010+
  'begin'#010+
  '  Result:=False;'#010+
  '  List:=TStringList.Create;'#010+
  '  Try'#010+
  '    APackage','.GetInstallSourceFiles(List,stt,ttt);'#010+
  '    if (List.Count>0) then'#010+
  '      begin'#010+
  '        Result:=True;'#010+
  '        CmdCopyFiles(List,Dest);'#010+
  '      end;'#010+
  '  Finally'#010+
  '    List.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoBeforeInstall(APackage: TPackage)',';'#010+
  'begin'#010+
  '  ExecuteCommands(APackage.Commands,caBeforeInstall);'#010+
  '  If Assigned(APackage.BeforeInstall) then'#010+
  '    APackage.BeforeInstall(APackage);'#010+
  '  If Assigned(APackage.BeforeInstallProc) then'#010+
  '    APackage.BeforeInstallProc(APackage);'#010+
  'end;'#010+
  #010+
  #010+
  'p','rocedure TBuildEngine.DoAfterInstall(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterInstall) then'#010+
  '    APackage.AfterInstall(APackage);'#010+
  '  If Assigned(APackage.AfterInstallProc) then'#010+
  '    APackage.AfterInstallProc(APackage);'#010+
  '  ExecuteC','ommands(APackage.Commands,caAfterInstall);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Install(APackage: TPackage);'#010+
  'Var'#010+
  '  UC,D : String;'#010+
  '  B : Boolean;'#010+
  'begin'#010+
  '  If (Apackage.State<>tsCompiled) then'#010+
  '    MaybeCompile(APackage);'#010+
  '  try'#010+
  '    Log(vlCommand,SInfo','InstallingPackage,[APackage.Name]);'#010+
  '    If (APackage.Directory<>'#039#039') then'#010+
  '      EnterDir(APackage.Directory);'#010+
  '    DoBeforeInstall(APackage);'#010+
  '    // units'#010+
  '    B:=false;'#010+
  '    GlobalDictionary.AddVariable('#039'PackageName'#039',APackage.Name);'+
  #010+
  '    Global','Dictionary.AddVariable('#039'unitinstalldir'#039',Defaults.U'+
  'nitInstallDir);'#010+
  '    GlobalDictionary.AddVariable('#039'packageunitinstalldir'#039',APack'+
  'age.GetPackageUnitInstallDir(Defaults.CPU,Defaults.OS));'#010+
  #010+
  '    D:=IncludeTrailingPathDelimiter(Defaults.BaseInsta','llDir);'#010+
  '    // This is to install the TPackage.Installfiles, which are not rel'+
  'ated to any'#010+
  '    // target'#010+
  '    if InstallPackageFiles(APackage,[],D) then'#010+
  '      B:=true;'#010+
  '    D:=IncludeTrailingPathDelimiter(Defaults.UnitInstallDir)+APackage.'+
  'GetP','ackageUnitInstallDir(Defaults.CPU,Defaults.OS);'#010+
  '    if InstallPackageFiles(APackage,[ttUnit, ttImplicitUnit],D) then'#010+
  '      B:=true;'#010+
  '    // By default do not install the examples. Maybe add an option for'+
  ' this later'#010+
  '    //if InstallPackageFil','es(APAckage,ttExampleUnit,D) then'#010+
  '    //  B:=true;'#010+
  '    // Unit (dependency) configuration if there were units installed'#010+
  '    D:=IncludeTrailingPathDelimiter(Defaults.UnitInstallDir);'#010+
  '    if B then'#010+
  '      InstallUnitConfigFile(APackage,D);'#010+
  '   ',' // Programs'#010+
  '    D:=IncludeTrailingPathDelimiter(Defaults.BinInstallDir);'#010+
  '    InstallPackageFiles(APAckage,[ttProgram],D);'#010+
  '    //InstallPackageFiles(APAckage,ttExampleProgram,D);'#010+
  '    // Documentation'#010+
  '    D:=IncludeTrailingPathDelimiter(Defa','ults.DocInstallDir)+'#039'fpc'+
  '-'#039'+APackage.FileName+PathDelim;'#010+
  '    InstallPackageSourceFiles(APackage,[stDoc],[],D);'#010+
  '    // Examples'#010+
  '    if Defaults.InstallExamples then'#010+
  '      begin'#010+
  '        D:=IncludeTrailingPathDelimiter(Defaults.ExamplesInstallD','ir)'+
  '+'#039'fpc-'#039'+APackage.FileName+PathDelim+'#039'examples'#039'+PathDel'+
  'im;'#010+
  '        InstallPackageSourceFiles(APackage,[stExample],[ttExampleProgr'+
  'am,ttExampleUnit],D);'#010+
  '      end;'#010+
  '    // Done.'#010+
  '    APackage.FTargetState:=tsInstalled;'#010+
  '    DoAfterInstall(APac','kage);'#010+
  '  Finally'#010+
  '    If (APackage.Directory<>'#039#039') then'#010+
  '      EnterDir('#039#039');'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoBeforeArchive(APackage: TPackage);'#010+
  'begin'#010+
  '  ExecuteCommands(APackage.Commands,caBeforeArchive);'#010+
  '  If Assigned(APackage.BeforeArc','hive) then'#010+
  '    APackage.BeforeArchive(APackage);'#010+
  '  If Assigned(APackage.BeforeArchiveProc) then'#010+
  '    APackage.BeforeArchiveProc(APackage);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoAfterArchive(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterAr','chive) then'#010+
  '    APackage.AfterArchive(APackage);'#010+
  '  If Assigned(APackage.AfterArchiveProc) then'#010+
  '    APackage.AfterArchiveProc(APackage);'#010+
  '  ExecuteCommands(APackage.Commands,caAfterArchive);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Archive(APackage: TPa','ckage);'#010+
  'Var'#010+
  '  L : TStringList;'#010+
  '  A : String;'#010+
  '  i: integer;'#010+
  '  ICPU : TCPU;'#010+
  '  IOS  : TOS;'#010+
  'begin'#010+
  '  A:=FStartDir+ APackage.FileName + ZipExt;'#010+
  '  Log(vlInfo,SInfoArchivingPackage,[APackage.Name,A]);'#010+
  '  try'#010+
  '    If (APackage.Directory<>'#039#039') then'#010+
  '    ','  EnterDir(APackage.Directory);'#010+
  '    DoBeforeArchive(Apackage);'#010+
  '    L:=TStringList.Create;'#010+
  '    L.Sorted:=true;'#010+
  '    L.Duplicates:=dupIgnore;'#010+
  '    Try'#010+
  '      // Add fpmake.pp & manifest.xml always'#010+
  '      L.Add(FPMakePPFile);'#010+
  '      L.Add(ManifestF','ile);'#010+
  '      //get all files from all targets'#010+
  '      for ICPU:=Low(TCPU) to high(TCPU) do'#010+
  '        for IOS:=Low(TOS) to high(TOS) do'#010+
  '          if OSCPUSupported[IOS,ICPU] then'#010+
  '            begin'#010+
  '              ResolveFileNames(APackage,ICPU,IOS,','false);'#010+
  '              APackage.GetArchiveFiles(L, ICPU, IOS);'#010+
  '            end;'#010+
  '      //from sources'#010+
  '      APackage.GetArchiveSourceFiles(L);'#010+
  #010+
  '      //show all files'#010+
  '      for i := 0 to L.Count-1 do'#010+
  '        Log(vlDebug, Format(SDbgArchivingF','ile, [L[i]]));'#010+
  #010+
  '{$ifdef HAS_UNIT_ZIPPER}'#010+
  '      if not Assigned(ArchiveFilesProc) then'#010+
  '        begin'#010+
  '          FZipFile := TZipper.Create;'#010+
  '          FZipFile.ZipFiles(A, L);'#010+
  '        end'#010+
  '      else'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  '        CmdArchiveF','iles(L,A);'#010+
  '    Finally'#010+
  '      L.Free;'#010+
  #010+
  '{$ifdef HAS_UNIT_ZIPPER}'#010+
  '      if not Assigned(ArchiveFilesProc) then'#010+
  '        FreeAndNil(FZipFile);'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  '    end;'#010+
  '    DoAfterArchive(Apackage);'#010+
  '  Finally'#010+
  '    If (APackage.Directory<>',#039#039') then'#010+
  '      EnterDir('#039#039');'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoBeforeClean(APackage: TPackage);'#010+
  'begin'#010+
  '  ExecuteCommands(APackage.Commands,caBeforeClean);'#010+
  '  If Assigned(APackage.BeforeClean) then'#010+
  '    APackage.BeforeClean(APackage);'#010+
  '  If',' Assigned(APackage.BeforeCleanProc) then'#010+
  '    APackage.BeforeCleanProc(APackage);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoAfterClean(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterClean) then'#010+
  '    APackage.AfterClean(APackage);'#010+
  '  If Assigned(','APackage.AfterInstallProc) then'#010+
  '    APackage.AfterCleanProc(APackage);'#010+
  '  ExecuteCommands(APackage.Commands,caAfterClean);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Clean(APackage: TPackage; AllTargets: boolean);'+
  #010+
  'var'#010+
  '  ACPU: TCpu;'#010+
  '  AOS: TOS;'#010+
  '  Director','yList : TStringList;'#010+
  'begin'#010+
  '  Log(vlInfo,SInfoCleaningPackage,[APackage.Name]);'#010+
  '  try'#010+
  '    If (APackage.Directory<>'#039#039') then'#010+
  '      EnterDir(APackage.Directory);'#010+
  '    DoBeforeClean(Apackage);'#010+
  '    if AllTargets then'#010+
  '      begin'#010+
  '        // Remove ','the unit-and bin-directories completely. This is s'+
  'afer in case of files'#010+
  '        // being renamed and such. See also bug 19655'#010+
  '        DirectoryList := TStringList.Create;'#010+
  '        try'#010+
  '          for ACPU:=low(TCpu) to high(TCpu) do'#010+
  '          ','  for AOS:=low(TOS) to high(TOS) do'#010+
  '              begin'#010+
  '                DirectoryList.Add(ExtractFileDir(APackage.GetUnitsOutp'+
  'utDir(ACPU,AOS)));'#010+
  '                DirectoryList.Add(ExtractFileDir(APackage.GetBinOutput'+
  'Dir(ACPU,AOS)));'#010+
  '        ','      end;'#010+
  '          CmdRemoveTrees(DirectoryList);'#010+
  '        finally'#010+
  '          DirectoryList.Free;'#010+
  '        end;'#010+
  '{        for ACPU:=low(TCpu) to high(TCpu) do'#010+
  '          for AOS:=low(TOS) to high(TOS) do'#010+
  '            begin'#010+
  '              if File','Exists(APackage.GetUnitsOutputDir(ACPU,AOS)) or'+
  #010+
  '                 FileExists(APackage.GetBinOutputDir(ACPU,AOS)) then'#010+
  '                Clean(APackage,ACPU,AOS);'#010+
  '            end;}'#010+
  '      end'#010+
  '    else'#010+
  '      Clean(APackage, Defaults.CPU, Defaults','.OS);'#010+
  '    DoAfterClean(Apackage);'#010+
  '  Finally'#010+
  '    If (APackage.Directory<>'#039#039') then'#010+
  '      EnterDir('#039#039');'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Clean(APackage: TPackage; ACPU: TCPU; AOS: TOS)'+
  ';'#010+
  'Var'#010+
  '  List : TStringList;'#010+
  '  DirectoryList : TStringLis','t;'#010+
  'begin'#010+
  '  List:=TStringList.Create;'#010+
  '  try'#010+
  '    List.Add(APackage.GetUnitsOutputDir(ACPU,AOS) + PathDelim + UnitCo'+
  'nfigFile);'#010+
  '    APackage.GetCleanFiles(List,ACPU,AOS);'#010+
  '    if (List.Count>0) then'#010+
  '      begin'#010+
  '      CmdDeleteFiles(List);'#010+
  '      ','DirectoryList := TStringList.Create;'#010+
  '      try'#010+
  '        GetDirectoriesFromFilelist(List,DirectoryList);'#010+
  '        CmdRemoveDirs(DirectoryList);'#010+
  #010+
  '        DirectoryList.Clear;'#010+
  '        if DirectoryExists(APackage.GetBinOutputDir(ACPU,AOS)) then'#010+
  ' ','         DirectoryList.Add(APackage.GetBinOutputDir(ACPU,AOS));'#010+
  '        if DirectoryExists(APackage.GetUnitsOutputDir(ACPU,AOS)) then'#010+
  '          DirectoryList.Add(APackage.GetUnitsOutputDir(ACPU,AOS));'#010+
  '        CmdRemoveDirs(DirectoryList);'#010+
  #010,
  '        DirectoryList.Clear;'#010+
  '        if DirectoryExists(ExtractFileDir(APackage.GetBinOutputDir(ACP'+
  'U,AOS))) then'#010+
  '          DirectoryList.Add(ExtractFileDir(APackage.GetBinOutputDir(AC'+
  'PU,AOS)));'#010+
  '        if DirectoryExists(ExtractFileDir(APac','kage.GetUnitsOutputDir'+
  '(ACPU,AOS))) then'#010+
  '          DirectoryList.Add(ExtractFileDir(APackage.GetUnitsOutputDir('+
  'ACPU,AOS)));'#010+
  '        CmdRemoveDirs(DirectoryList);'#010+
  '      finally'#010+
  '        DirectoryList.Free;'#010+
  '      end;'#010+
  '      end;'#010+
  '  Finally'#010+
  '    L','ist.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure TBuildEngine.Manifest(APackage : TPackage);'#010+
  'Var'#010+
  '  L : TStrings;'#010+
  '  PD,'#010+
  '  MF : String;'#010+
  'begin'#010+
  '  L:=TStringList.Create;'#010+
  '  Try'#010+
  '    Log(vlInfo, Format(SInfoManifestPackage,[APackage.Name]));'#010+
  '    PD:=APackage.Dire','ctory;'#010+
  '    if PD<>'#039#039' then'#010+
  '      PD:=IncludeTrailingPathDelimiter(PD);'#010+
  '    MF:=PD+ManifestFile;'#010+
  '    Log(vlDebug, Format(SDbgGenerating, [MF]));'#010+
  '    L.Add('#039'<?xml version="1.0"?>'#039');'#010+
  '    L.Add('#039'<packages>'#039');'#010+
  '    APackage.GetManifest(L);'#010+
  '    L.A','dd('#039'</packages>'#039');'#010+
  '    L.SaveToFile(MF);'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Compile(Packages: TPackages);'#010+
  #010+
  '  function IsReadyToCompile(APackage:TPackage): boolean;'#010+
  '  begin'#010+
  '    result := False;'#010+
  '    if not APackage.FPr','ocessing and (APackage.State=tsNeutral) then'#010+
  '      begin'#010+
  '        if PackageOK(APackage) then'#010+
  '          result := ReadyToCompile(APackage)'#010+
  '        else'#010+
  '          begin'#010+
  '            inc(FProgressCount);'#010+
  '            log(vlWarning,SWarnSkipPacka','geTargetProgress,[(FProgress'+
  'Count)/FProgressMax * 100, APackage.Name, Defaults.Target]);'#010+
  '            APackage.FTargetState:=tsNoCompile;'#010+
  '          end;'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  'Var'#010+
  '  I : integer;'#010+
  '{$ifndef NO_THREADING}'#010+
  '  Thr : Integer;'#010+
  '  Finished',' : boolean;'#010+
  '  ErrorState: boolean;'#010+
  '  ErrorMessage: string;'#010+
  '  NotifyThreadWaiting : PRTLEvent;'#010+
  '  Threads : array of TCompileWorkerThread;'#010+
  '{$endif NO_THREADING}'#010+
  '  P : TPackage;'#010+
  #010+
  '{$ifndef NO_THREADING}'#010+
  '  procedure ProcessThreadResult(ATHread: ','TCompileWorkerThread);'#010+
  '  var'#010+
  '    StartI: integer;'#010+
  '    CompilePackage: TPackage;'#010+
  '    PackageAvailable: boolean;'#010+
  '  begin'#010+
  '    if AThread.Done then'#010+
  '      begin'#010+
  '        if assigned(AThread.APackage) then'#010+
  '          begin'#010+
  '            // The thread',' has completed compiling the package'#010+
  '            if AThread.CompilationOK then'#010+
  '              AThread.APackage.FTargetState:=tsCompiled'#010+
  '            else // A problem occured, stop the compilation'#010+
  '              begin'#010+
  '              ErrorState:','=true;'#010+
  '              ErrorMessage:=AThread.ErrorMessage;'#010+
  '              Finished:=true;'#010+
  '              end;'#010+
  '            AThread.APackage := nil;'#010+
  '          end;'#010+
  '        StartI := I;'#010+
  #010+
  '        CompilePackage := nil;'#010+
  '        PackageAvailable:=fal','se;'#010+
  '        repeat'#010+
  '        if IsReadyToCompile(Packages.PackageItems[i]) then'#010+
  '          CompilePackage := Packages.PackageItems[i];'#010+
  '        if not (Packages.PackageItems[i].State in [tsCompiled, tsNoCom'+
  'pile]) then'#010+
  '          PackageAvailable',':=true;'#010+
  '        inc(I);'#010+
  '        if I=packages.Count then'#010+
  '          i := 0;'#010+
  '        until Assigned(CompilePackage) or (I=StartI);'#010+
  '        if Assigned(CompilePackage) then'#010+
  '          begin'#010+
  '          // Instruct thread to compile package'#010+
  '      ','    AThread.APackage := CompilePackage;'#010+
  '          AThread.APackage.FProcessing := true;'#010+
  '          AThread.FDone:=False;'#010+
  '          RTLeventSetEvent(AThread.NotifyStartTask);'#010+
  '          end;'#010+
  '        if not PackageAvailable then'#010+
  '          Finis','hed := True;'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  '{$endif NO_THREADING}'#010+
  #010+
  'begin'#010+
  '  If Assigned(BeforeCompile) then'#010+
  '    BeforeCompile(Self);'#010+
  '  FProgressMax:=Packages.Count;'#010+
  '  FProgressCount:=0;'#010+
  #010+
  '  if Defaults.ThreadsAmount<0 then'#010+
  '    begin'#010+
  '      // Do not use a','ny threading to compile the packages'#010+
  '      For I:=0 to Packages.Count-1 do'#010+
  '        begin'#010+
  '          P:=Packages.PackageItems[i];'#010+
  '          If PackageOK(P) then'#010+
  '            MaybeCompile(P)'#010+
  '          else'#010+
  '            begin'#010+
  '            inc(FPro','gressCount);'#010+
  '            log(vlWarning,SWarnSkipPackageTargetProgress,[(FProgressCo'+
  'unt)/FProgressMax * 100, P.Name, Defaults.Target]);'#010+
  '            end;'#010+
  '        end;'#010+
  '    end'#010+
  '  else'#010+
  '    begin'#010+
  '{$ifndef NO_THREADING}'#010+
  '      // Use worker-threads',' to compile the packages'#010+
  '      ErrorState := False;'#010+
  '      Finished := False;'#010+
  '      I := 0;'#010+
  '      // This event is set by the worker-threads to notify the main/th'+
  'is thread'#010+
  '      // that a package finished it'#039's task.'#010+
  '      NotifyThreadWaiting',' := RTLEventCreate;'#010+
  '      SetLength(Threads,Defaults.ThreadsAmount);'#010+
  '      // Create all worker-threads'#010+
  '      for Thr:=0 to Defaults.ThreadsAmount-1 do'#010+
  '        Threads[Thr] := TCompileWorkerThread.Create(self,NotifyThreadW'+
  'aiting);'#010+
  '      try',#010+
  '        // When a thread notifies this thread that it is ready, loop o'+
  'n all'#010+
  '        // threads to check their state and if possible assign a new p'+
  'ackage'#010+
  '        // to them to compile.'#010+
  '        while not Finished do'#010+
  '          begin'#010+
  '         ','   RTLeventWaitFor(NotifyThreadWaiting);'#010+
  '            for Thr:=0 to Defaults.ThreadsAmount-1 do if not Finished '+
  'then'#010+
  '              ProcessThreadResult(Threads[Thr]);'#010+
  '          end;'#010+
  '        // Compilation finished or aborted. Wait for all thr','eads to '+
  'end.'#010+
  '        for thr:=0 to Defaults.ThreadsAmount-1 do'#010+
  '          begin'#010+
  '            Threads[Thr].Terminate;'#010+
  '            RTLeventSetEvent(Threads[Thr].NotifyStartTask);'#010+
  '            Threads[Thr].WaitFor;'#010+
  '          end;'#010+
  '      finally'#010+
  '  ','      RTLeventdestroy(NotifyThreadWaiting);'#010+
  '        for thr:=0 to Defaults.ThreadsAmount-1 do'#010+
  '          Threads[Thr].Free;'#010+
  '      end;'#010+
  '    if ErrorState then'#010+
  '      raise Exception.Create(ErrorMessage);'#010+
  '{$endif NO_THREADING}'#010+
  '    end;'#010+
  '  If Ass','igned(AfterCompile) then'#010+
  '    AfterCompile(Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Install(Packages: TPackages);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  If Assigned(BeforeInstall) then'#010+
  '    BeforeInstall(Self);'#010+
  '  For I:=0 to Packages.Count-1 ','do'#010+
  '    begin'#010+
  '      P:=Packages.PackageItems[i];'#010+
  '      If PackageOK(P) then'#010+
  '        begin'#010+
  '          Install(P);'#010+
  '          log(vlWarning, SWarnInstallationPackagecomplete, [P.Name, De'+
  'faults.Target]);'#010+
  '        end'#010+
  '      else'#010+
  '        log(vlWarni','ng,SWarnSkipPackageTarget,[P.Name, Defaults.Targe'+
  't]);'#010+
  '    end;'#010+
  '  If Assigned(AfterInstall) then'#010+
  '    AfterInstall(Self);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.ZipInstall(Packages: TPackages);'#010+
  'var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  If Assigned(Bef','oreInstall) then'#010+
  '    BeforeInstall(Self);'#010+
  #010+
  '  FZipper := TZipper.Create;'#010+
  '  try'#010+
  '    Defaults.IntSetBaseInstallDir('#039'lib/fpc/'#039' + Defaults.FCompi'+
  'lerVersion+ '#039'/'#039');'#010+
  '    For I:=0 to Packages.Count-1 do'#010+
  '      begin'#010+
  '        P:=Packages.PackageItems[i','];'#010+
  '        If PackageOK(P) then'#010+
  '          begin'#010+
  '            FZipper.FileName := P.Name + '#039'.'#039' + MakeTargetStrin'+
  'g(Defaults.CPU,Defaults.OS) +'#039'.zip'#039';'#010+
  '            Install(P);'#010+
  '            FZipper.ZipAllFiles;'#010+
  '            FZipper.Clear;'#010+
  '         ','   log(vlWarning, SWarnInstallationPackagecomplete, [P.Name'+
  ', Defaults.Target]);'#010+
  '          end'#010+
  '        else'#010+
  '          log(vlWarning,SWarnSkipPackageTarget,[P.Name, Defaults.Targe'+
  't]);'#010+
  '      end;'#010+
  '  finally'#010+
  '    FZipper.Free;'#010+
  '  end;'#010+
  #010+
  '  If Assign','ed(AfterInstall) then'#010+
  '    AfterInstall(Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Archive(Packages: TPackages);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  If Assigned(BeforeArchive) then'#010+
  '    BeforeArchive(Self);'#010+
  '  Log(vlDebug, SDbgBuildEngineArch','iving);'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '      P:=Packages.PackageItems[i];'#010+
  '      Archive(P);'#010+
  '    end;'#010+
  '  If Assigned(AfterArchive) then'#010+
  '    AfterArchive(Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Manifest(Packages: TPackages);'#010+
  'Var'#010+
  '  I ',': Integer;'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  If Assigned(BeforeManifest) then'#010+
  '    BeforeManifest(Self);'#010+
  '  Log(vlDebug, SDbgBuildEngineGenerateManifests);'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '      P:=Packages.PackageItems[i];'#010+
  '      Manifest(P)',';'#010+
  '    end;'#010+
  '  If Assigned(AfterManifest) then'#010+
  '    AfterManifest(Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Clean(Packages: TPackages; AllTargets: boolean)'+
  ';'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  If Assigned(BeforeClean) then'#010+
  '    BeforeClean(Se','lf);'#010+
  '  Log(vldebug, SDbgBuildEngineCleaning);'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '    P:=Packages.PackageItems[i];'#010+
  '    If AllTargets or PackageOK(P) then'#010+
  '      Clean(P, AllTargets);'#010+
  '    log(vlWarning, SWarnCleanPackagecomplete, [P.N','ame]);'#010+
  '    end;'#010+
  '  If Assigned(AfterClean) then'#010+
  '    AfterClean(Self);'#010+
  'end;'#010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                               TFPVersion'#010+
  '*********************************************','***********************'+
  '********}'#010+
  #010+
  'function TFPVersion.GetAsString: String;'#010+
  'begin'#010+
  '  if Empty then'#010+
  '    Result:='#039'<none>'#039#010+
  '  else'#010+
  '  begin'#010+
  '    Result := '#039#039';'#010+
  '    if Major <> -1 then'#010+
  '      Result := Result + IntToStr(Major);'#010+
  '    if Minor <> -1 then',#010+
  '      Result := Result + '#039'.'#039' + IntToStr(Minor);'#010+
  '    if Micro <> -1 then'#010+
  '      Result := Result + '#039'.'#039' + IntToStr(Micro);'#010+
  '    if Build <> -1 then'#010+
  '      Result := Result + '#039'-'#039'  + IntToStr(Build);'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'function TFPVersion.GetEmpty: Boo','lean;'#010+
  'begin'#010+
  '  Result:=(Major=0) and (Minor=0) and (Micro=0) and (Build=0);'#010+
  'end;'#010+
  #010+
  'procedure TFPVersion.SetAsString(const AValue: String);'#010+
  #010+
  '  Function NextDigit(sep : Char; var V : string) : integer;'#010+
  '  Var'#010+
  '    P : Integer;'#010+
  '  begin'#010+
  '    P:=Pos(','Sep,V);'#010+
  '    If (P=0) then'#010+
  '      P:=Length(V)+1;'#010+
  '    Result:=StrToIntDef(Copy(V,1,P-1),-1);'#010+
  '    If Result<>-1 then'#010+
  '      Delete(V,1,P);'#010+
  '  end;'#010+
  #010+
  'Var'#010+
  '  V : String;'#010+
  'begin'#010+
  '  Clear;'#010+
  '  // Special support for empty version string'#010+
  '  if (AValue='#039#039') o','r (AValue='#039'<none>'#039') then'#010+
  '    exit;'#010+
  '  V:=AValue;'#010+
  '  Major:=NextDigit('#039'.'#039',V);'#010+
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
  '  Buil','d:=-1;'#010+
  'end;'#010+
  #010+
  'procedure TFPVersion.Assign(Source: TPersistent);'#010+
  'Var'#010+
  '  V : TFPVersion;'#010+
  'begin'#010+
  '  if Source is TFPVersion then'#010+
  '    begin'#010+
  '    V:=Source as TFPVersion;'#010+
  '    Major:=V.Major;'#010+
  '    Minor:=V.Minor;'#010+
  '    Micro:=V.Micro;'#010+
  '    Build:=V.Build;',#010+
  '    end'#010+
  '  else'#010+
  '    inherited Assign(Source);'#010+
  'end;'#010+
  #010+
  'function TFPVersion.CompareVersion(AVersion: TFPVersion): Integer;'#010+
  'begin'#010+
  '  Result:=Major-AVersion.Major;'#010+
  '  If (Result=0) then'#010+
  '    begin'#010+
  '      Result:=Minor-AVersion.Minor;'#010+
  '      if (Result','=0) then'#010+
  '        begin'#010+
  '          Result:=Micro-AVersion.Micro;'#010+
  '          If (Result=0) then'#010+
  '            Result:=Build-AVersion.Build;'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function TFPVersion.SameVersion(AVersion: TFPVersion): Boolean;'#010+
  'begin'#010+
  '  Result',':=CompareVersion(AVersion)=0;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                 TTarget'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'const','ructor TTarget.Create(ACollection: TCollection);'#010+
  'begin'#010+
  '  inherited Create(ACollection);'#010+
  '  FInstall:=True;'#010+
  '  FCPUs:=AllCPUs;'#010+
  '  FOSes:=AllOSes;'#010+
  '  FUnitPath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FIncludePath:=TConditionalStrings.C','reate(TConditionalString);'#010+
  '  FObjectPath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FDependencies:=TDependencies.Create(TDependency);'#010+
  '  FCommands:=TCommands.Create(TCommand);'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TTarget.Destroy;'#010+
  'begin'#010+
  '  FreeAndNil(FUn','itPath);'#010+
  '  FreeAndNil(FObjectPath);'#010+
  '  FreeAndNil(FIncludePath);'#010+
  '  FreeAndNil(FDependencies);'#010+
  '  FreeAndNil(FCommands);'#010+
  '  FreeAndNil(Foptions);'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  'procedure TTarget.AssignTo(Dest: TPersistent);'#010+
  'var'#010+
  '  DestTarget: TTarge','t;'#010+
  'begin'#010+
  '  if Dest is TTarget then'#010+
  '    begin'#010+
  '    DestTarget := TTarget(Dest);'#010+
  '    DestTarget.Dependencies.Assign(Dependencies);'#010+
  '    DestTarget.Commands.Assign(Commands);'#010+
  '    DestTarget.FTargetState := FTargetState;'#010+
  '    DestTarget.TargetType',' := TargetType;'#010+
  '    DestTarget.CPUs := CPUs;'#010+
  '    DestTarget.OSes := OSes;'#010+
  '    DestTarget.Mode := Mode;'#010+
  '    DestTarget.Options := Options;'#010+
  '    DestTarget.Name :=  Name;'#010+
  '    DestTarget.Extension:= Extension;'#010+
  '    DestTarget.FPCTarget := FPCTar','get;'#010+
  '    DestTarget.FileType := FileType;'#010+
  '    DestTarget.Directory := Directory;'#010+
  '    DestTarget.ResourceStrings := ResourceStrings;'#010+
  '    DestTarget.Install := Install;'#010+
  '    DestTarget.FTargetSourceFileName := fTargetSourceFileName;'#010+
  '    DestTa','rget.ObjectPath.Assign(ObjectPath);'#010+
  '    DestTarget.UnitPath.Assign(UnitPath);'#010+
  '    DestTarget.IncludePath.Assign(IncludePath);'#010+
  '    DestTarget.FXML := FXML;'#010+
  '    DestTarget.AfterCompile := AfterCompile;'#010+
  '    DestTarget.BeforeCompile := BeforeCo','mpile;'#010+
  '    DestTarget.BeforeClean := BeforeCompile;'#010+
  '    DestTarget.AfterClean := AfterClean;'#010+
  '    end'#010+
  '  else'#010+
  '    inherited AssignTo(Dest);'#010+
  'end;'#010+
  #010+
  'function TTarget.GetOptions: TStrings;'#010+
  'begin'#010+
  '  If Foptions=Nil then'#010+
  '    FOptions:=TStringList.Cr','eate;'#010+
  '  Result:=FOptions;'#010+
  'end;'#010+
  #010+
  'function TTarget.GetImportLibFileName(AOS : TOS) : String;'#010+
  'begin'#010+
  '  result := GetImportLibraryFilename(Name,AOS);'#010+
  'end;'#010+
  #010+
  'function TTarget.GetUnitLibFileName: String;'#010+
  'begin'#010+
  '  Result:='#039'libp'#039'+Name+LibExt;'#010+
  'end;'#010+
  #010+
  'pr','ocedure TTarget.SetOptions(const AValue: TStrings);'#010+
  'begin'#010+
  '  If (AValue=Nil) or (AValue.Count=0) then'#010+
  '    FreeAndNil(FOptions)'#010+
  '  else'#010+
  '    Options.Assign(AValue);'#010+
  'end;'#010+
  #010+
  'function TTarget.GetSourceFileName: String;'#010+
  'begin'#010+
  '  Result:=Name+FExtensi','on;'#010+
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
  '  Result:=Name+ObjExt;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetRSTFileName: String;'#010+
  'begin'#010+
  '  Result:=Name+RSText;'#010,
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetProgramFileName(AOS : TOS): String;'#010+
  'begin'#010+
  '  result := AddProgramExtension(Name, AOS);'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetProgramDebugFileName(AOS: TOS): String;'#010+
  'begin'#010+
  '  result := Name + DbgExt;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.Get','OutputFileName(AOs: TOS): String;'#010+
  'begin'#010+
  '  if TargetType in UnitTargets then'#010+
  '    Result:=GetUnitFileName'#010+
  '  else'#010+
  '    Result:=GetProgramFileName(AOs);'#010+
  'end;'#010+
  #010+
  'function TTarget.HaveOptions: Boolean;'#010+
  'begin'#010+
  '  Result:=(FOptions<>Nil);'#010+
  'end;'#010+
  #010+
  #010+
  'procedu','re TTarget.SetName(const AValue: String);'#010+
  'Var'#010+
  '  D,N,E : String;'#010+
  'begin'#010+
  '  N:=FixPath(AValue, False);'#010+
  '  D:=ExtractFilePath(N);'#010+
  '  E:=ExtractFileExt(N);'#010+
  '  N:=ExtractFileName(N);'#010+
  '  inherited SetName(Copy(N,1,Length(N)-Length(E)));'#010+
  '  FExtension:=E',';'#010+
  '  FDirectory:=D;'#010+
  'end;'#010+
  #010+
  'procedure TTarget.SetXML(const AValue: string);'#010+
  'begin'#010+
  '  FXML:=FixPath(AValue, False);'#010+
  'end;'#010+
  #010+
  'procedure TTarget.GetCleanFiles(List: TStrings; const APrefixU, APrefi'+
  'xB : String; ACPU: TCPU; AOS : TOS);'#010+
  'begin'#010+
  '  If not(A','CPU in CPUs) or not(AOS in OSes) then'#010+
  '    exit;'#010+
  '  List.Add(APrefixU + ObjectFileName);'#010+
  '  If (TargetType in [ttUnit,ttImplicitUnit,ttExampleUnit, ttCleanOnlyU'+
  'nit]) then'#010+
  '    begin'#010+
  '      List.Add(APrefixU + UnitFileName);'#010+
  '      if (AOS in AllS','martLinkLibraryOSes) and FileExists(APrefixU + '+
  'UnitLibFileName) then'#010+
  '        List.Add(APrefixU + UnitLibFileName);'#010+
  '      if (AOS in AllImportLibraryOSes) and FileExists(APrefixU + GetIm'+
  'portLibFilename(AOS)) then'#010+
  '        List.Add(APrefixU + ','GetImportLibFilename(AOS));'#010+
  '    end'#010+
  '  else If (TargetType in [ttProgram,ttExampleProgram]) then'#010+
  '    begin'#010+
  '    List.Add(APrefixB + GetProgramFileName(AOS));'#010+
  '    if FileExists(APrefixB + GetProgramDebugFileName(AOS)) then'#010+
  '      List.Add(APref','ixB + GetProgramDebugFileName(AOS));'#010+
  '    end;'#010+
  '  If ResourceStrings then'#010+
  '    List.Add(APrefixU + RSTFileName);'#010+
  '  // Maybe add later ?  AddConditionalStrings(List,CleanFiles);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TTarget.GetInstallFiles(List: TStrings; const APr','efixU, AP'+
  'refixB: String; ACPU: TCPU; AOS : TOS);'#010+
  'begin'#010+
  '  If Not (TargetType in [ttProgram,ttExampleProgram]) and FileExists(A'+
  'PrefixU + ObjectFileName) then'#010+
  '    // The compiler does not create an objectfile for all programs.'#010+
  '    List.Add(APr','efixU + ObjectFileName);'#010+
  '  If (TargetType in [ttUnit,ttImplicitUnit,ttExampleUnit]) then'#010+
  '    begin'#010+
  '    List.Add(APrefixU + UnitFileName);'#010+
  '    if (AOS in AllSmartLinkLibraryOSes) and FileExists(APrefixU + Unit'+
  'LibFileName) then'#010+
  '      List.Add','(APrefixU + UnitLibFileName);'#010+
  '    if (AOS in AllImportLibraryOSes) and FileExists(APrefixU + GetImpo'+
  'rtLibFilename(AOS)) then'#010+
  '      List.Add(APrefixU + GetImportLibFilename(AOS));'#010+
  '    end'#010+
  '  else If (TargetType in [ttProgram,ttExampleProgram]',') then'#010+
  '    List.Add(APrefixB + GetProgramFileName(AOS));'#010+
  '  If ResourceStrings then'#010+
  '    List.Add(APrefixU + RSTFileName);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TTarget.GetArchiveFiles(List: TStrings; ACPU: TCPU; AOS : TO'+
  'S);'#010+
  'var'#010+
  '  i : integer;'#010+
  '  D : TDependency;'#010,
  'begin'#010+
  '  If not(ACPU in CPUs) or not(AOS in OSes) then'#010+
  '    exit;'#010+
  '  // Main source'#010+
  '  if TargetSourceFileName<>'#039#039' then'#010+
  '    List.Add(TargetSourceFileName);'#010+
  '  // Includes'#010+
  '  for i:=0 to Dependencies.Count-1 do'#010+
  '    begin'#010+
  '      D:=Dependencies[i];'#010,
  '      if (D.DependencyType=depInclude) and'#010+
  '         (D.TargetFileName<>'#039#039') then'#010+
  '        List.Add(D.TargetFileName);'#010+
  '    end;'#010+
  '  // FPDoc files'#010+
  '  if XML <> '#039#039' then'#010+
  '  begin'#010+
  '    List.Add(Directory + Name + Extension);'#010+
  '    List.Add(XML);'#010+
  '  end;'#010,
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                 TSource'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TSource.GetInstallSourcePa','th: string;'#010+
  'begin'#010+
  '  if FInstallSourcePath<>'#039#039' then'#010+
  '    result := FInstallSourcePath'#010+
  '  else if SourceType=stExample then'#010+
  '    result := '#039'examples'#039#010+
  '  else'#010+
  '    result := '#039#039';'#010+
  'end;'#010+
  #010+
  'constructor TSource.Create(ACollection: TCollection);'#010+
  'begin'#010+
  '  in','herited Create(ACollection);'#010+
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
  '  if InstallSourcePath<>'#039#039' then'#010+
  '    list.Values[name] := (IncludeTrailingPathDelimiter','(InstallSource'+
  'Path)+ExtractFileName(Name))'#010+
  '  else'#010+
  '    list.add(Name);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                 TCommands'#010+
  '******************************************','**************************'+
  '********}'#010+
  #010+
  'function TCommands.GetCommand(const Dest : String): TCommand;'#010+
  'begin'#010+
  '  Result:=TCommand(ItemByName(Dest));'#010+
  'end;'#010+
  #010+
  'function TCommands.GetCommandItem(Index : Integer): TCommand;'#010+
  'begin'#010+
  '  Result:=TCommand(Item','s[Index]);'#010+
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
  'Function TCommands.AddCommand(const Cmd: String) : TCommand;'#010+
  'begin'#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,'#039#039','#039#039,','#039#039');'#010+
  'end;'#010+
  #010+
  #010+
  'function TCommands.AddCommand(const Cmd, Options: String): TCommand;'#010+
  'begin'#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,Options,'#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  #010+
  'function TCommands.AddCommand(const Cmd, Options, Dest, Source: String'+
  ' ): TCommand;'#010+
  'begin'#010+
  '  R','esult:=AddCommand(fdefaultAt,Cmd,options,Dest,Source);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TCommands.AddCommand(At: TCommandAt; const Cmd: String) : TCo'+
  'mmand;'#010+
  'begin'#010+
  '  Result:=AddCommand(At,Cmd,'#039#039','#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  #010+
  'function TCommands.AddCommand(At: TCommandAt; con','st Cmd, Options: St'+
  'ring  ): TCommand;'#010+
  'begin'#010+
  '  Result:=AddCommand(At,Cmd,Options,'#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  #010+
  'function TCommands.AddCommand(At: TCommandAt; const Cmd, Options, Dest'+
  ', Source: String): TCommand;'#010+
  'begin'#010+
  '  Result:=Add as TCommand;'#010+
  '  Result.Comm','and:=Cmd;'#010+
  '  If (Options<>'#039#039') then'#010+
  '    Result.ParseOptions(Options);'#010+
  '  Result.At:=At;'#010+
  '  Result.SourceFile:=Source;'#010+
  '  Result.DestFile:=Dest;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                ','           TConditionalString'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'Constructor TConditionalString.Create;'#010+
  'begin'#010+
  '  inherited Create;'#010+
  'end;'#010+
  #010+
  #010+
  '{**********************************************************','*********'+
  '*********'#010+
  '                           TConditionalStrings'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'Constructor TConditionalStrings.Create(AClass:TConditionalStringClass)'+
  ';'#010+
  'begin'#010+
  '  inherited Creat','e;'#010+
  '  FCSClass:=AClass;'#010+
  'end;'#010+
  #010+
  #010+
  'function TConditionalStrings.GetConditionalString(Index : Integer): TC'+
  'onditionalString;'#010+
  'begin'#010+
  '  Result:=TConditionalString(Items[Index]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TConditionalStrings.SetConditionalString(Index : Intege','r; '+
  'const AValue: TConditionalString);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TConditionalStrings.Add(Const Value : String) : TConditionalS'+
  'tring;'#010+
  'begin'#010+
  '  result:=Add(Value,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function ','TConditionalStrings.Add(Const Value : String;const CPUs:TCP'+
  'Us) : TConditionalString;'#010+
  'begin'#010+
  '  result:=Add(Value,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'Function TConditionalStrings.Add(Const Value : String;const OSes:TOSes'+
  ') : TCond','itionalString;'#010+
  'begin'#010+
  '  result:=Add(Value,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TConditionalStrings.Add(Const Value : String;const CPUs:TCPUs'+
  ';const OSes:TOSes) : TConditionalString;'#010+
  'begin'#010+
  '  Result:=FCSClass.Create;'#010+
  '  Result.Value:=Value;'#010+
  '  Result.O','Ses:=OSes;'#010+
  '  Result.CPUs:=CPUs;'#010+
  '  inherited Add(Result);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TDependency'#010+
  '******************************************************','**************'+
  '********}'#010+
  #010+
  'Constructor TDependency.Create;'#010+
  'begin'#010+
  '  inherited Create;'#010+
  '  FVersion:=TFPVersion.Create;'#010+
  'end;'#010+
  #010+
  #010+
  'Destructor TDependency.Destroy;'#010+
  'begin'#010+
  '  FreeAndNil(FVersion);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TDependency.GetVersion : string;'#010+
  'begin'#010+
  '  ','result:=FVersion.AsString;'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure TDependency.SetVersion(const V : string);'#010+
  'begin'#010+
  '  FVersion.AsString:=V;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TDepend','encies'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TDependencies.GetDependency(Index : Integer): TDependency;'#010+
  'begin'#010+
  '  Result:=TDependency(Items[Index]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TDependencies.SetDependenc','y(Index : Integer; const AValue:'+
  ' TDependency);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TDependencies.Add(Const Value : String) : TDependency;'#010+
  'begin'#010+
  '  result:=Add(Value,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TDep','endencies.Add(Const Value : String;const CPUs:TCPUs) : '+
  'TDependency;'#010+
  'begin'#010+
  '  result:=Add(Value,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'Function TDependencies.Add(Const Value : String;const OSes:TOSes) : TD'+
  'ependency;'#010+
  'begin'#010+
  '  result:','=Add(Value,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TDependencies.Add(Const Value : String;const CPUs:TCPUs;const'+
  ' OSes:TOSes) : TDependency;'#010+
  'begin'#010+
  '  Result:=inherited Add(Value,CPUs,OSes) as TDependency;'#010+
  '  Result.Target:=nil;'#010+
  '  Result.FDependencyType',':=depPackage;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TDependencies.AddUnit(Const Value : String) : TDependency;'#010+
  'begin'#010+
  '  result:=AddUnit(Value,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TDependencies.AddUnit(Const Value : String;const CPUs:TCPUs',')'+
  ' : TDependency;'#010+
  'begin'#010+
  '  result:=AddUnit(Value,CPUs,AllOSes);'#010+
  'end;'#010+
  '{$endif cpu_only_overloads}'#010+
  #010+
  #010+
  'Function TDependencies.AddUnit(Const Value : String;const OSes:TOSes) '+
  ': TDependency;'#010+
  'begin'#010+
  '  result:=AddUnit(Value,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  #010+
  'Functi','on TDependencies.AddUnit(Const Value : String;const CPUs:TCPUs'+
  ';const OSes:TOSes) : TDependency;'#010+
  'begin'#010+
  '  Result:=inherited Add(Value,CPUs,OSes) as TDependency;'#010+
  '  Result.Target:=nil;'#010+
  '  Result.FDependencyType:=depUnit;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TDepende','ncies.AddInclude(Const Value : String) : TDependenc'+
  'y;'#010+
  'begin'#010+
  '  result:=AddInclude(Value,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef cpu_only_overloads}'#010+
  'Function TDependencies.AddInclude(Const Value : String;const CPUs:TCPU'+
  's) : TDependency;'#010+
  'begin'#010+
  '  resu','lt:=AddInclude(Value,CPUs,AllOSes);'#010+
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
  'Function TDependencies.Add','Include(Const Value : String;const CPUs:TC'+
  'PUs;const OSes:TOSes) : TDependency;'#010+
  'Var'#010+
  '  N : String;'#010+
  'begin'#010+
  '  N:=FixPath(Value, False);'#010+
  '  if ExtractFileExt(N)='#039#039' then'#010+
  '    ChangeFileExt(N,IncExt);'#010+
  '  Result:=inherited Add(N,CPUs,OSes) as TDependen','cy;'#010+
  '  Result.FDependencyType:=depInclude;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                               TValueItem'#010+
  '**********************************************************************'+
  '*','*****}'#010+
  #010+
  'constructor TValueItem.Create(AValue: String);'#010+
  'begin'#010+
  '  FValue:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                              TFunctionItem'#010+
  '********************************','************************************'+
  '********}'#010+
  #010+
  'constructor TFunctionItem.Create(AFunc: TReplaceFunction);'#010+
  'begin'#010+
  '  FFunc:=AFunc;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                           ','      TDictionary'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TDictionary.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  inherited Create(AOwner);'#010+
  '  FList:=TStringList.Create;'#010+
  '  FList.Sorted:=True;'#010+
  '  FLis','t.Duplicates:=dupError;'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TDictionary.Destroy;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  For I:=0 to Flist.Count-1 do'#010+
  '    FList.Objects[i].Free;'#010+
  '  FreeAndNil(FList);'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TDictionary.AddVariable(const AName',', Value: String);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  I:=Flist.IndexOf(AName);'#010+
  '  If I=-1 then'#010+
  '    I:=FList.Add(Aname)'#010+
  '  else'#010+
  '    Flist.Objects[i].Free;'#010+
  '  Flist.Objects[i]:=TValueItem.Create(Value);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TDictionary.AddFunction(const ANam','e: String; FReplacement:'+
  ' TReplaceFunction);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  I:=Flist.IndexOf(AName);'#010+
  '  If I=-1 then'#010+
  '    I:=Flist.Add(AName)'#010+
  '  else'#010+
  '    Flist.Objects[i].Free;'#010+
  '  Flist.Objects[i]:=TFunctionItem.Create(FReplacement);'#010+
  'end;'#010+
  #010+
  #010+
  'procedur','e TDictionary.RemoveItem(const AName: String);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  I:=Flist.IndexOf(AName);'#010+
  '  If (I<>-1) then'#010+
  '    begin'#010+
  '    FList.Objects[i].Free;'#010+
  '    FList.Delete(I);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'function TDictionary.GetValue(AName: String): Str','ing;'#010+
  'begin'#010+
  '  Result:=GetValue(AName,'#039#039');'#010+
  'end;'#010+
  #010+
  #010+
  'function TDictionary.GetValue(const AName,Args: String): String;'#010+
  'Var'#010+
  '  O : TObject;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  I:=Flist.IndexOf(AName);'#010+
  '  If (I=-1) then'#010+
  '    Raise EDictionaryError.CreateFmt(SErrNo','DictionaryItem,[AName]);'#010+
  '  O:=Flist.Objects[I];'#010+
  '  If O is TValueItem then'#010+
  '    Result:=TValueItem(O).FValue'#010+
  '  else'#010+
  '    Result:=TFunctionItem(O).FFunc(AName,Args);'#010+
  'end;'#010+
  #010+
  #010+
  'function TDictionary.ReplaceStrings(Const ASource: String): String;'#010+
  'Var',#010+
  '  S,FN,FV : String;'#010+
  '  P: Integer;'#010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  S:=ASource;'#010+
  '  P:=Pos('#039'$('#039',S);'#010+
  '  While (P<>0) do'#010+
  '    begin'#010+
  '      Result:=Result+Copy(S,1,P-1);'#010+
  '      Delete(S,1,P+1);'#010+
  '      P:=Pos('#039')'#039',S);'#010+
  '      FN:=Copy(S,1,P-1);'#010+
  '      Delete(S,1,P);',#010+
  '      P:=Pos('#039' '#039',FN);'#010+
  '      If (P<>0) then // function arguments ?'#010+
  '        begin'#010+
  '        FV:=FN;'#010+
  '        FN:=Copy(FN,1,P);'#010+
  '        System.Delete(FV,1,P);'#010+
  '        end'#010+
  '      else'#010+
  '        FV:='#039#039';'#010+
  '      Result:=Result+GetValue(FN,FV);'#010+
  '      P:','=Pos('#039'$('#039',S);'#010+
  '    end;'#010+
  '  Result:=Result+S;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TDictionary.Substitute(Const Source : String; Macros : Array '+
  'of string) : String;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  I:=0;'#010+
  '  While I<High(Macros) do'#010+
  '    begin'#010+
  '      AddVariable(Macros[i],Ma','cros[I+1]);'#010+
  '      Inc(I,2);'#010+
  '    end;'#010+
  '  Result:=ReplaceStrings(Source);'#010+
  '  While I<High(Macros) do'#010+
  '    begin'#010+
  '      RemoveItem(Macros[i]);'#010+
  '      Inc(I,2);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  '{*********************************************************************'+
  '***','****'#010+
  '                                 Default Instances'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'var'#010+
  '  DefInstaller : TCustomInstaller;'#010+
  #010+
  'Function Installer(InstallerClass: TInstallerClass): TCustomInsta','lle'+
  'r;'#010+
  'begin'#010+
  '  If Not Assigned(DefInstaller) then'#010+
  '    DefInstaller:=InstallerClass.Create(Nil);'#010+
  '  Result:=DefInstaller;'#010+
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
  'function T','Command.GetOptions: TStrings;'#010+
  'begin'#010+
  '  If (FOptions=Nil) then'#010+
  '    FOptions:=TStringList.Create;'#010+
  '  Result:=FOptions;'#010+
  'end;'#010+
  #010+
  'procedure TCommand.SetOptions(const Value: TStrings);'#010+
  'begin'#010+
  '  If (Value=Nil) or (Value.Count=0) then'#010+
  '    FreeAndNil(FOp','tions)'#010+
  '  else'#010+
  '    Options.Assign(Value);'#010+
  'end;'#010+
  #010+
  'destructor TCommand.Destroy;'#010+
  'begin'#010+
  '  FreeAndNil(FOptions);'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  'function TCommand.HaveOptions: Boolean;'#010+
  'begin'#010+
  '  Result:=(FOptions<>Nil);'#010+
  'end;'#010+
  #010+
  #010+
  'function TCommand.CmdLineOp','tions: String;'#010+
  'begin'#010+
  '  If HaveOptions then'#010+
  '    Result:=OptionListToString(Options);'#010+
  'end;'#010+
  #010+
  'procedure TCommand.ParseOptions(S: String);'#010+
  #010+
  'begin'#010+
  '  Options:=OptionsToStringList(S);'#010+
  'end;'#010+
  #010+
  'Initialization'#010+
  '  OnGetApplicationName:=@GetFPMakeName;'#010+
  '  C','ustomFpmakeCommandlineOptions:=nil;'#010+
  '  CustomFpMakeCommandlineValues:=nil;'#010+
  #010+
  'Finalization'#010+
  '  FreeAndNil(CustomFpMakeCommandlineValues);'#010+
  '  FreeAndNil(CustomFpmakeCommandlineOptions);'#010+
  '  FreeAndNil(DefInstaller);'#010+
  '  FreeAndNil(GlobalDictionary);'#010+
  ' ',' FreeAndNil(Defaults);'#010+
  'end.'#010
);
