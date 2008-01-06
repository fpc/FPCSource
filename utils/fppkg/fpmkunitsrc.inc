{$ifdef Delphi}
const fpmkunitsrc : array[0..549] of string[240]=(
{$else Delphi}
const fpmkunitsrc : array[0..549,1..240] of char=(
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
  'Interface'#010+
  #010+
  '{$ifndef NO_UNIT_PROCESS}'#010+
  '  {$define HAS_UNIT_PROCESS}'#010+
  '{$endif NO_UNIT_PROCESS}'#010+
  #010+
  '{$ifndef NO_UNIT_ZIPPER}'#010+
  '  {$define HAS_UNIT_ZIPPER','}'#010+
  '{$endif NO_UNIT_ZIPPER}'#010+
  #010+
  'uses'#010+
  '  SysUtils, Classes, StrUtils'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '  ,process'#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  '{$ifdef HAS_UNIT_ZIPPER}'#010+
  '  ,zipper'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  '  ;'#010+
  #010+
  'Type'#010+
  '  TFileType = (ftSource,ftUnit,ftObject,ftRes','ource,ftExecutable,ftSt'+
  'aticLibrary,'#010+
  '               ftSharedLibrary);'#010+
  '  TFileTypes = set of TFileType;'#010+
  #010+
  '  // Please keep this order, see OSCPUpossible below'#010+
  '  TCpu=(cpuNone,'#010+
  '    i386,m68k,powerpc,sparc,x86_64,arm,powerpc64'#010+
  '  );'#010+
  '  TCPUS = Set',' of TCPU;'#010+
  #010+
  '  // Please keep this order, see OSCPUpossible below'#010+
  '  TOS=(osNone,'#010+
  '    linux,go32v2,win32,os2,freebsd,beos,netbsd,'#010+
  '    amiga,atari, solaris, qnx, netware, openbsd,wdosx,'#010+
  '    palmos,macos,darwin,emx,watcom,morphos,netwlibc,'#010+
  '    w','in64,wince,gba,nds,embedded,symbian'#010+
  '  );'#010+
  '  TOSes = Set of TOS;'#010+
  #010+
  '  TCompilerMode = (cmFPC,cmTP,cmObjFPC,cmDelphi,cmMacPas);'#010+
  '  TCompilerModes = Set of TCompilerMode;'#010+
  #010+
  '  TTargetType = (ttProgram,ttUnit,ttImplicitUnit,ttCleanOnlyUnit,ttExa'+
  'mpleU','nit,ttExampleProgram);'#010+
  '  TTargetTypes = set of TTargetType;'#010+
  #010+
  '  TTargetState = (tsNeutral,tsNeedCompile,tsNoCompile,tsCompiled,tsIns'+
  'talled,tsNotFound);'#010+
  '  TTargetStates = Set of TTargetState;'#010+
  #010+
  '  TSourceType = (stDoc,stSrc,stExample,stTest);'#010+
  ' ',' TSourceTypes = set of TSourceType;'#010+
  #010+
  '  TVerboseLevel = (vlError,vlWarning,vlInfo,vldebug);'#010+
  '  TVerboseLevels = Set of TVerboseLevel;'#010+
  #010+
  '  TCommandAt = (caBeforeCompile,caAfterCompile,'#010+
  '                caBeforeInstall,caAfterInstall,'#010+
  '           ','     caBeforeArchive,caAfterArchive,'#010+
  '                caBeforeClean,caAfterClean,'#010+
  '                caBeforeDownload,caAfterDownload);'#010+
  #010+
  '  TDependencyType = (depPackage,depImplicitPackage,depUnit,depInclude)'+
  ';'#010+
  '  TDependencyTypes = set of TDepend','encyType;'#010+
  #010+
  '  TLogEvent = Procedure (Level : TVerboseLevel; Const Msg : String) of'+
  ' Object;'#010+
  #010+
  '  TRunMode = (rmCompile,rmBuild,rmInstall,rmArchive,rmClean,rmManifest'+
  ');'#010+
  #010+
  'Const'#010+
  '  // Aliases'#010+
  '  Amd64   = X86_64;'#010+
  '  PPC = PowerPC;'#010+
  '  PPC64 = PowerPC64',';'#010+
  '  DOS = Go32v2;'#010+
  '  MacOSX = Darwin;'#010+
  #010+
  '  AllOSes = [Low(TOS)..High(TOS)];'#010+
  '  AllCPUs = [Low(TCPU)..High(TCPU)];'#010+
  '  AllUnixOSes  = [Linux,FreeBSD,NetBSD,OpenBSD,Darwin,QNX,BeOS,Solaris'+
  '];'#010+
  '  AllWindowsOSes  = [Win32,Win64,WinCE];'#010+
  #010+
  '  { This table ','is kept OS,Cpu because it is easier to maintain (PFV)'+
  ' }'#010+
  '  OSCpupossible : array[TOS,TCpu] of boolean = ('#010+
  '    { os          none   i386   m68k   ppc    sparc  x86_64 arm    ppc'+
  '64}'#010+
  '    { none  }   ( false, false, false, false, false, false, f','alse, f'+
  'alse),'#010+
  '    { linux }   ( false, true,  true,  true,  true,  true,  true,  tru'+
  'e),'#010+
  '    { go32v2 }  ( false, true,  false, false, false, false, false, fal'+
  'se),'#010+
  '    { win32 }   ( false, true,  false, false, false, false, false, fal'+
  'se),'#010+
  '  ','  { os2 }     ( false, true,  false, false, false, false, false, f'+
  'alse),'#010+
  '    { freebsd } ( false, true,  true,  false, false, true,  false, fal'+
  'se),'#010+
  '    { beos }    ( false, true,  false, false, false, false, false, fal'+
  'se),'#010+
  '    { netbsd }  (',' false, true,  true,  true,  true,  false, false, f'+
  'alse),'#010+
  '    { amiga }   ( false, false, true,  true,  false, false, false, fal'+
  'se),'#010+
  '    { atari }   ( false, false, true,  false, false, false, false, fal'+
  'se),'#010+
  '    { solaris } ( false, true,  ','false, false, true,  false, false, f'+
  'alse),'#010+
  '    { qnx }     ( false, true,  false, false, false, false, false, fal'+
  'se),'#010+
  '    { netware } ( false, true,  false, false, false, false, false, fal'+
  'se),'#010+
  '    { openbsd } ( false, true,  true,  false, f','alse, false, false, f'+
  'alse),'#010+
  '    { wdosx }   ( false, true,  false, false, false, false, false, fal'+
  'se),'#010+
  '    { palmos }  ( false, false, true,  false, false, false, true,  fal'+
  'se),'#010+
  '    { macos }   ( false, false, false, true,  false, false, fa','lse, f'+
  'alse),'#010+
  '    { darwin }  ( false, true,  false, true,  false, false, false, tru'+
  'e),'#010+
  '    { emx }     ( false, true,  false, false, false, false, false, fal'+
  'se),'#010+
  '    { watcom }  ( false, true,  false, false, false ,false, false, fal'+
  'se),'#010+
  '   ',' { morphos } ( false, false, false, true,  false ,false, false, f'+
  'alse),'#010+
  '    { netwlibc }( false, true,  false, false, false, false, false, fal'+
  'se),'#010+
  '    { win64   } ( false, false, false, false, false, true,  false, fal'+
  'se),'#010+
  '    { wince    }( ','false, true,  false, false, false, false, true,  f'+
  'alse),'#010+
  '    { gba    }  ( false, false, false, false, false, false, true,  fal'+
  'se),'#010+
  '    { nds    }  ( false, false, false, false, false, false, true,  fal'+
  'se),'#010+
  '    { embedded }( false, true,  t','rue,  true,  true,  true,  true,  t'+
  'rue),'#010+
  '    { symbian } ( false, true,  false, false, false, false, true,  fal'+
  'se)'#010+
  '  );'#010+
  #010+
  '  // Useful'#010+
  '  UnitExt = '#039'.ppu'#039';'#010+
  '  PPUExt  = UnitExt;'#010+
  '  PasExt  = '#039'.pas'#039';'#010+
  '  PPExt   = '#039'.pp'#039';'#010+
  '  IncExt  = '#039'.inc'#039';'#010+
  '  ObjEx','t  = '#039'.o'#039';'#010+
  '  RstExt  = '#039'.rst'#039';'#010+
  '  LibExt  = '#039'.a'#039';'#010+
  '  SharedLibExt = '#039'.so'#039';'#010+
  '  DLLExt  = '#039'.dll'#039';'#010+
  '  ExeExt  = '#039'.exe'#039';'#010+
  '  ZipExt  = '#039'.zip'#039';'#010+
  #010+
  '  FPMakePPFile = '#039'fpmake.pp'#039';'#010+
  '  ManifestFile = '#039'manifest.xml'#039';'#010+
  '  UnitConfigFile = '#039'fpunits.conf'#039';'#010+
  #010+
  '  DirNo','tFound = '#039'<dirnotfound>'#039';'#010+
  #010+
  '  UnitTargets = [ttUnit,ttImplicitUnit,ttCleanOnlyUnit,ttExampleUnit];'+
  #010+
  '  ProgramTargets = [ttProgram,ttExampleProgram];'#010+
  #010+
  '  DefaultMessages = [vlError,vlWarning,vlInfo];'#010+
  '  AllMessages = [vlError,vlWarning,vlInfo];'#010,
  #010+
  #010+
  'Type'#010+
  '  { TNamedItem }'#010+
  #010+
  '  TNamedItem = Class(TCollectionItem)'#010+
  '  private'#010+
  '    FName: String;'#010+
  '    procedure SetName(const AValue: String);virtual;'#010+
  '  Public'#010+
  '    property Name : String Read FName Write SetName;'#010+
  '  end;'#010+
  #010+
  '  { TNamedCollection }'#010+
  #010+
  ' ',' TNamedCollection = Class(TCollection)'#010+
  '  private'#010+
  '    FUniqueNames: Boolean;'#010+
  '  Public'#010+
  '    Function IndexOfName(const AName : String) : Integer;'#010+
  '    Function ItemByName(const AName : String) : TNamedItem;'#010+
  '    Property UniqueNames : Boolean Re','ad FUniqueNames;'#010+
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
  '    Function ','IndexOfName(const AName : String) : Integer;'#010+
  '    Function ItemByName(const ANAme : String) : TNamedItem;'#010+
  '    Property NamedItems[Index : Integer] : TNamedItem Read GetNamedIte'+
  'm Write SetNamedItem; default;'#010+
  '  end;'#010+
  #010+
  '  { TCommand }'#010+
  '  TCommand ','= Class(TNamedItem)'#010+
  '  private'#010+
  '    FAfterCommand: TNotifyEvent;'#010+
  '    FBeforeCommand: TNotifyEvent;'#010+
  '    FCommand: String;'#010+
  '    FCommandAt: TCommandAt;'#010+
  '    FDestFile: String;'#010+
  '    FIgnoreResult: Boolean;'#010+
  '    FOptions: String;'#010+
  '    FSourceFile: Str','ing;'#010+
  '  Public'#010+
  '    Property SourceFile : String Read FSourceFile Write FSourceFile;'#010+
  '    Property DestFile : String Read FDestFile Write FDestFile;'#010+
  '    Property Command : String Read FCommand Write FCommand;'#010+
  '    Property Options : String Read',' FOptions Write FOptions;'#010+
  '    Property At : TCommandAt Read FCommandAt Write FCommandAt;'#010+
  '    Property IgnoreResult : Boolean Read FIgnoreResult Write FIgnoreRe'+
  'sult;'#010+
  '    Property BeforeCommand : TNotifyEvent Read FBeforeCommand Write FB'+
  'efore','Command;'#010+
  '    Property AfterCommand : TNotifyEvent Read FAfterCommand Write FAft'+
  'erCommand;'#010+
  '  end;'#010+
  #010+
  '  { TCommands }'#010+
  #010+
  '  TCommands = Class(TNamedCollection)'#010+
  '  private'#010+
  '    FDefaultAt: TCommandAt;'#010+
  '    function GetCommand(const Dest : String): TCo','mmand;'#010+
  '    function GetCommandItem(Index : Integer): TCommand;'#010+
  '    procedure SetCommandItem(Index : Integer; const AValue: TCommand);'+
  #010+
  '  Public'#010+
  '    Function AddCommand(Const Cmd : String) : TCommand;'#010+
  '    Function AddCommand(Const Cmd,Options',' : String) : TCommand;'#010+
  '    Function AddCommand(Const Cmd,Options,Dest,Source : String) : TCom'+
  'mand;'#010+
  '    Function AddCommand(At : TCommandAt; Const Cmd : String) : TComman'+
  'd;'#010+
  '    Function AddCommand(At : TCommandAt; Const Cmd,Options : String)',' '+
  ': TCommand;'#010+
  '    Function AddCommand(At : TCommandAt; Const Cmd,Options, Dest,Sourc'+
  'e : String) : TCommand;'#010+
  '    Property CommandItems[Index : Integer] : TCommand Read GetCommandI'+
  'tem Write SetCommandItem;'#010+
  '    Property Commands[Dest : String] ',': TCommand Read GetCommand; def'+
  'ault;'#010+
  '    Property DefaultAt : TCommandAt Read FDefaultAt Write FDefaultAt;'#010+
  '  end;'#010+
  #010+
  '  { TConditionalString }'#010+
  '  TConditionalString = Class'#010+
  '  private'#010+
  '    FOSes   : TOSes;'#010+
  '    FCPUs   : TCPUs;'#010+
  '    FValue  : Strin','g;'#010+
  '  Public'#010+
  '    Property Value : String Read FValue Write FValue;'#010+
  '    Property OSes  : TOSes Read FOSes Write FOSes;'#010+
  '    Property CPUs : TCPUs Read FCPUS Write FCPUs;'#010+
  '  end;'#010+
  #010+
  '  { TConditionalStrings }'#010+
  #010+
  '  TConditionalStrings = Class(TFPList)',#010+
  '  private'#010+
  '    FCSClass : TClass;'#010+
  '    function GetConditionalString(Index : Integer): TConditionalString'+
  ';'#010+
  '    procedure SetConditionalString(Index : Integer; const AValue: TCon'+
  'ditionalString);'#010+
  '  Public'#010+
  '    Constructor Create(AClass:TClass);',#010+
  '    Function Add(Const Value : String) : TConditionalString;inline;'#010+
  '    Function Add(Const Value : String;const OSes:TOSes) : TConditional'+
  'String;inline;'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs) : TConditional'+
  'String;inline;'#010+
  ' ','   Function Add(Const Value : String;const CPUs:TCPUs;const OSes:TO'+
  'Ses) : TConditionalString;'#010+
  '    Property ConditionalStrings[Index : Integer] : TConditionalString '+
  'Read GetConditionalString Write SetConditionalString; default;'#010+
  '  end;'#010+
  #010+
  '  { T','Dependency }'#010+
  '  TDependency = Class(TConditionalString)'#010+
  '  private'#010+
  '    // Package, Unit'#010+
  '    FTarget : TObject;'#010+
  '    // Includes and implicit/external packages'#010+
  '    FDirectory,'#010+
  '    FFullFileName : String;'#010+
  '    FDependencyType : TDependencyType;'#010+
  ' ',' Public'#010+
  '    Property Target : TObject Read FTarget Write FTarget;'#010+
  '    Property DependencyType : TDependencyType Read FDependencyType;'#010+
  '    Property Directory: String Read FDirectory;'#010+
  '    Property FullFileName: String Read FFullFileName;'#010+
  '  en','d;'#010+
  #010+
  '  TDependencies = Class(TConditionalStrings)'#010+
  '    function GetDependency(Index : Integer): TDependency;'#010+
  '    procedure SetDependency(Index : Integer; const AValue: TDependency'+
  ');'#010+
  '  Public'#010+
  '    Function Add(Const Value : String) : TDependenc','y;inline;'#010+
  '    Function Add(Const Value : String;const OSes:TOSes) : TDependency;'+
  'inline;'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs) : TDependency;'+
  'inline;'#010+
  '    Function Add(Const Value : String;const CPUs:TCPUs;const OSes:TOSe'+
  's) :',' TDependency;'#010+
  '    Function AddUnit(Const Value : String) : TDependency;inline;'#010+
  '    Function AddUnit(Const Value : String;const OSes:TOSes) : TDepende'+
  'ncy;inline;'#010+
  '    Function AddUnit(Const Value : String;const CPUs:TCPUs) : TDepende'+
  'ncy;inlin','e;'#010+
  '    Function AddUnit(Const Value : String;const CPUs:TCPUs;const OSes:'+
  'TOSes) : TDependency;'#010+
  '    Function AddInclude(Const Value : String) : TDependency;inline;'#010+
  '    Function AddInclude(Const Value : String;const OSes:TOSes) : TDepe'+
  'ndency;','inline;'#010+
  '    Function AddInclude(Const Value : String;const CPUs:TCPUs) : TDepe'+
  'ndency;inline;'#010+
  '    Function AddInclude(Const Value : String;const CPUs:TCPUs;const OS'+
  'es:TOSes) : TDependency;'#010+
  '    Property Dependencies[Index : Integer] : TDepend','ency Read GetDep'+
  'endency Write SetDependency; default;'#010+
  '  end;'#010+
  #010+
  '  { TTarget }'#010+
  #010+
  '  TTarget = Class(TNamedItem)'#010+
  '  private'#010+
  '    FInstall : Boolean;'#010+
  '    FAfterClean: TNotifyEvent;'#010+
  '    FAfterCompile: TNotifyEvent;'#010+
  '    FBeforeClean: TNotifyEvent;'#010+
  '   ',' FBeforeCompile: TNotifyEvent;'#010+
  '    FCPUs: TCPUs;'#010+
  '    FMode: TCompilerMode;'#010+
  '    FResourceStrings: Boolean;'#010+
  '    FObjectPath,'#010+
  '    FUnitPath,'#010+
  '    FIncludePath : TConditionalStrings;'#010+
  '    FDependencies : TDependencies;'#010+
  '    FCommands : TCommands;'#010,
  '    FDirectory: String;'#010+
  '    FExtension: String;'#010+
  '    FFullSourceFileName : String;'#010+
  '    FFileType: TFileType;'#010+
  '    FOptions: String;'#010+
  '    FOSes: TOSes;'#010+
  '    FFPCTarget: String;'#010+
  '    FTargetState: TTargetState;'#010+
  '    FTargetType: TTargetType;'#010+
  '  Prot','ected'#010+
  '    Function GetSourceFileName : String; virtual;'#010+
  '    Function GetUnitFileName : String; virtual;'#010+
  '    Function GetObjectFileName : String; virtual;'#010+
  '    Function GetRSTFileName : String; Virtual;'#010+
  '    Function GetProgramFileName(AOS : T','OS) : String; Virtual;'#010+
  '  Public'#010+
  '    Constructor Create(ACollection : TCollection); override;'#010+
  '    Destructor Destroy; override;'#010+
  '    Function GetOutputFileName (AOs : TOS) : String; Virtual;'#010+
  '    procedure SetName(const AValue: String);overrid','e;'#010+
  '    Procedure GetCleanFiles(List : TStrings; const APrefixU, APrefixB '+
  ': String; ACPU:TCPU; AOS : TOS); virtual;'#010+
  '    Procedure GetInstallFiles(List : TStrings; const APrefixU, APrefix'+
  'B: String; ACPU:TCPU; AOS : TOS); virtual;'#010+
  '    Procedur','e GetArchiveFiles(List : TStrings; ACPU:TCPU; AOS : TOS)'+
  '; virtual;'#010+
  '    Property Dependencies : TDependencies Read FDependencies;'#010+
  '    Property Commands : TCommands Read FCommands;'#010+
  '    Property State : TTargetState Read FTargetState;'#010+
  '    Prop','erty TargetType : TTargetType Read FTargetType Write FTarget'+
  'Type;'#010+
  '    Property OSes : TOSes Read FOSes Write FOSes;'#010+
  '    Property CPUs : TCPUs Read FCPUs Write FCPUs;'#010+
  '    Property Mode : TCompilerMode Read FMode Write FMode;'#010+
  '    Property Opt','ions : String Read FOptions Write Foptions;'#010+
  '    Property SourceFileName: String Read GetSourceFileName ;'#010+
  '    Property UnitFileName : String Read GetUnitFileName;'#010+
  '    Property ObjectFileName : String Read GetObjectFileName;'#010+
  '    Property RSTF','ileName : String Read GetRSTFileName;'#010+
  '    Property FPCTarget : String Read FFPCTarget Write FFPCTarget;'#010+
  '    Property Extension : String Read FExtension Write FExtension;'#010+
  '    Property FileType : TFileType Read FFileType Write FFileType;'#010+
  '    ','Property Directory : String Read FDirectory Write FDirectory;'#010+
  '    Property ResourceStrings : Boolean Read FResourceStrings Write FRe'+
  'sourceStrings;'#010+
  '    Property Install : Boolean Read FInstall Write FInstall;'#010+
  '    Property FullSourceFileName:',' String Read FFullSourceFileName;'#010+
  '    Property ObjectPath : TConditionalStrings Read FObjectPath;'#010+
  '    Property UnitPath : TConditionalStrings Read FUnitPath;'#010+
  '    Property IncludePath : TConditionalStrings Read FIncludePath;'#010+
  '    // Events.'#010+
  ' ','   Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write '+
  'FBeforeCompile;'#010+
  '    Property AfterCompile : TNotifyEvent Read FAfterCompile Write FAft'+
  'erCompile;'#010+
  '    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBefor'+
  'eClean',';'#010+
  '    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterCl'+
  'ean;'#010+
  '  end;'#010+
  #010+
  '  { TTargets }'#010+
  #010+
  '  TTargets = Class(TNamedCollection)'#010+
  '  private'#010+
  '    function GetTargetItem(Index : Integer): TTarget;'#010+
  '    function GetTarget(const AName : S','tring): TTarget;'#010+
  '    procedure SetTargetItem(Index : Integer; const AValue: TTarget);'#010+
  '  Public'#010+
  '    Function AddUnit(const AUnitName : String) : TTarget;'#010+
  '    Function AddImplicitUnit(const AUnitName : String;InstallUnit:bool'+
  'ean=true) : TTarg','et;'#010+
  '    Function AddProgram(const AProgramName : String) : TTarget;'#010+
  '    Function AddExampleUnit(const AUnitName : String) : TTarget;'#010+
  '    Function AddExampleProgram(const AProgramName : String) : TTarget;'+
  #010+
  '    Property Targets[AName : String]',' : TTarget Read GetTarget; defau'+
  'lt;'#010+
  '    Property TargetItems[Index : Integer] : TTarget Read GetTargetItem'+
  ' Write SetTargetItem;'#010+
  '  end;'#010+
  #010+
  '  { TSource }'#010+
  #010+
  '  TSource = Class(TNamedItem)'#010+
  '  private'#010+
  '    FSourceType : TSourceType;'#010+
  '  Public'#010+
  '    Const','ructor Create(ACollection : TCollection); override;'#010+
  '    Destructor Destroy; override;'#010+
  '    property SourceType : TSourceType read FSourceType;'#010+
  '  end;'#010+
  #010+
  '  { TSources }'#010+
  #010+
  '  TSources = Class(TNamedCollection)'#010+
  '  private'#010+
  '    function GetSourceItem(','Index : Integer): TSource;'#010+
  '    procedure SetSourceItem(Index : Integer; const AValue: TSource);'#010+
  '  public'#010+
  '    Function AddDocFiles(const AFiles : String) : TSource;'#010+
  '    Function AddSrcFiles(const AFiles : String) : TSource;'#010+
  '    Function AddE','xampleFiles(const AFiles : String) : TSource;'#010+
  '    Function AddTestFiles(const AFiles : String) : TSource;'#010+
  '    Property SourceItems[Index : Integer] : TSource Read GetSourceItem'+
  ' Write SetSourceItem;default;'#010+
  '  end;'#010+
  #010+
  '  { TPackage }'#010+
  #010+
  '  TPackage',' = Class(TNamedItem)'#010+
  '  private'#010+
  '    FAfterArchive: TNotifyEvent;'#010+
  '    FAfterClean: TNotifyEvent;'#010+
  '    FAfterCompile: TNotifyEvent;'#010+
  '    FAfterInstall: TNotifyEvent;'#010+
  '    FAfterManifest: TNotifyEvent;'#010+
  '    FBeforeArchive: TNotifyEvent;'#010+
  '    FBefore','Clean: TNotifyEvent;'#010+
  '    FBeforeCompile: TNotifyEvent;'#010+
  '    FBeforeInstall: TNotifyEvent;'#010+
  '    FBeforeManifest: TNotifyEvent;'#010+
  '    FUnitPath,'#010+
  '    FObjectPath,'#010+
  '    FIncludePath,'#010+
  '    FSourcePath,'#010+
  '    FExamplePath,'#010+
  '    FTestPath,'#010+
  '    FCleanFiles,',#010+
  '    FArchiveFiles,'#010+
  '    FInstallFiles : TConditionalStrings;'#010+
  '    FDependencies : TDependencies;'#010+
  '    FCPUs: TCPUs;'#010+
  '    FOSes: TOSes;'#010+
  '    FTargetState: TTargetState;'#010+
  '    FTargets: TTargets;'#010+
  '    FSources: TSources;'#010+
  '    FDirectory: String;'#010+
  '    ','FOptions: String;'#010+
  '    FFileName: String;'#010+
  '    FAuthor: String;'#010+
  '    FLicense: String;'#010+
  '    FExternalURL: String;'#010+
  '    FVersion: String;'#010+
  '    FEmail : String;'#010+
  '    FNeedLibC : Boolean;'#010+
  '    FCommands : TCommands;'#010+
  '    FDescriptionFile : String;'#010+
  '    ','FDescription : String;'#010+
  '    // Cached directory of installed packages'#010+
  '    FUnitDir : String;'#010+
  '    Function GetDescription : string;'#010+
  '    Function GetFileName : string;'#010+
  '  Protected'#010+
  '    procedure LoadUnitConfigFromFile(Const AFileName: String);'#010,
  '    procedure SaveUnitConfigToFile(Const AFileName: String;ACPU:TCPU;A'+
  'OS:TOS);'#010+
  '    procedure SaveUnitConfigToStream(S : TStream;ACPU:TCPU;AOS:TOS);'#010+
  '    procedure LoadUnitConfigFromStream(S: TStream);'#010+
  '  Public'#010+
  '    constructor Create(ACollect','ion: TCollection); override;'#010+
  '    destructor destroy; override;'#010+
  '    Function AddTarget(AName : String) : TTarget;'#010+
  '    Procedure AddDependency(AName : String);'#010+
  '    Procedure AddInstallFile(AFileName : String);'#010+
  '    procedure AddDocFiles(const ','AFileMask: string; Recursive: boolea'+
  'n = False);'#010+
  '    procedure AddSrcFiles(const AFileMask: string; Recursive: boolean '+
  '= False);'#010+
  '    procedure AddExampleFiles(const AFileMask: string; Recursive: bool'+
  'ean = False);'#010+
  '    procedure AddTestFiles(c','onst AFileMask: string; Recursive: boole'+
  'an = False);'#010+
  '    Procedure GetCleanFiles(List : TStrings; Const APrefixU, APrefixB '+
  ': String; ACPU:TCPU; AOS : TOS); virtual;'#010+
  '    procedure GetInstallFiles(List: TStrings;Types : TTargetTypes;Cons'+
  't APr','efix, APrefixU, APrefixB: String; ACPU:TCPU; AOS : TOS);'#010+
  '    Procedure GetArchiveFiles(List : TStrings; ACPU:TCPU; AOS : TOS); '+
  'virtual;'#010+
  '    Procedure GetManifest(Manifest : TStrings);'#010+
  '    Property Version : String Read FVersion Write FVersi','on;'#010+
  '    Property FileName : String Read GetFileName Write FFileName;'#010+
  '    Property ExternalURL : String Read FExternalURL Write FExternalURL'+
  ';'#010+
  '    Property Email : String Read FEmail Write FEmail;'#010+
  '    Property Author : String Read FAuthor Wri','te FAuthor;'#010+
  '    Property License : String Read FLicense Write FLicense;'#010+
  '    Property Directory : String Read FDirectory Write FDirectory;'#010+
  '    Property Description : String Read GetDescription Write FDescripti'+
  'on;'#010+
  '    Property DescriptionFile',' : String Read FDescriptionFile Write FD'+
  'escriptionFile;'#010+
  '    // Compiler options.'#010+
  '    Property OSes : TOSes Read FOSes Write FOSes;'#010+
  '    Property CPUs : TCPUs Read FCPUs Write FCPUs;'#010+
  '    Property NeedLibC : Boolean Read FNeedLibC Write FNeedL','ibC;'#010+
  '    Property Options: String Read FOptions Write FOptions;'#010+
  '    Property UnitPath : TConditionalStrings Read FUnitPath;'#010+
  '    Property ObjectPath : TConditionalStrings Read FObjectPath;'#010+
  '    Property IncludePath : TConditionalStrings Read ','FIncludePath;'#010+
  '    Property SourcePath : TConditionalStrings Read FSourcePath;'#010+
  '    Property ExamplePath : TConditionalStrings Read FExamplePath;'#010+
  '    Property TestPath : TConditionalStrings Read FTestPath;'#010+
  '    // Targets and dependencies'#010+
  '    ','Property InstallFiles : TConditionalStrings Read FInstallFiles;'#010+
  '    Property CleanFiles : TConditionalStrings Read FCleanFiles;'#010+
  '    Property ArchiveFiles : TConditionalStrings Read FArchiveFiles;'#010+
  '    Property Dependencies : TDependencies Re','ad FDependencies;'#010+
  '    Property Commands : TCommands Read FCommands;'#010+
  '    Property State : TTargetState Read FTargetState;'#010+
  '    Property Targets : TTargets Read FTargets;'#010+
  '    Property Sources : TSources Read FSources;'#010+
  '    Property UnitDir : St','ring Read FUnitDir Write FUnitDir;'#010+
  '    // events'#010+
  '    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FB'+
  'eforeCompile;'#010+
  '    Property AfterCompile : TNotifyEvent Read FAfterCompile Write FAft'+
  'erCompile;'#010+
  '    Property BeforeInstall',' : TNotifyEvent Read FBeforeInstall Write '+
  'FBeforeInstall;'#010+
  '    Property AfterInstall : TNotifyEvent Read FAfterInstall Write FAft'+
  'erInstall;'#010+
  '    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBefor'+
  'eClean;'#010+
  '    Property AfterClean',' : TNotifyEvent Read FAfterClean Write FAfter'+
  'Clean;'#010+
  '    Property BeforeArchive : TNotifyEvent Read FBeforeArchive Write FB'+
  'eforeArchive;'#010+
  '    Property AfterArchive : TNotifyEvent Read FAfterArchive Write FAft'+
  'erArchive;'#010+
  '    Property BeforeMani','fest : TNotifyEvent Read FBeforeManifest Writ'+
  'e FBeforeManifest;'#010+
  '    Property AfterManifest : TNotifyEvent Read FAfterManifest Write FA'+
  'fterManifest;'#010+
  '  end;'#010+
  #010+
  '  { TPackages }'#010+
  #010+
  '  TPackages = Class(TNamedCollection)'#010+
  '  private'#010+
  '    function GetPac','kage(const AName : String): TPackage;'#010+
  '    function GetPackageItem(AIndex : Integer): TPackage;'#010+
  '    procedure SetPackageItem(AIndex : Integer; const AValue: TPackage)'+
  ';'#010+
  '  Public'#010+
  '    Function AddPackage(Const AName : String) : TPackage;'#010+
  '    Pr','operty Packages[AName : String] : TPackage Read GetPackage ; D'+
  'efault;'#010+
  '    Property PackageItems[AIndex : Integer] : TPackage Read GetPackage'+
  'Item Write SetPackageItem;'#010+
  '  end;'#010+
  #010+
  '  { TCustomDefaults }'#010+
  #010+
  '  TCustomDefaults = Class(TPersistent)'#010+
  '  P','rivate'#010+
  '    FArchive: String;'#010+
  '    FCompiler: String;'#010+
  '    FCopy: String;'#010+
  '    FMkDir: String;'#010+
  '    FMove: String;'#010+
  '    FOptions: String;'#010+
  '    FCPU: TCPU;'#010+
  '    FOS: TOS;'#010+
  '    FMode : TCompilerMode;'#010+
  '    FCompilerVersion : String;'#010+
  '    FPrefix: String;',#010+
  '    FLocalUnitDir,'#010+
  '    FGlobalUnitDir,'#010+
  '    FBaseInstallDir,'#010+
  '    FUnitInstallDir,'#010+
  '    FBinInstallDir,'#010+
  '    FDocInstallDir,'#010+
  '    FExamplesInstallDir : String;'#010+
  '    FRemove: String;'#010+
  '    FTarget: String;'#010+
  '    FUnixPaths: Boolean;'#010+
  '    FNoFPCCfg: Bo','olean;'#010+
  '    FSourceExt : String;'#010+
  '    function GetLocalUnitDir: String;'#010+
  '    function GetGlobalUnitDir: String;'#010+
  '    function GetBaseInstallDir: String;'#010+
  '    function GetBinInstallDir: String;'#010+
  '    function GetCompiler: String;'#010+
  '    function GetDo','cInstallDir: String;'#010+
  '    function GetExamplesInstallDir: String;'#010+
  '    function GetUnitInstallDir: String;'#010+
  '    procedure SetLocalUnitDir(const AValue: String);'#010+
  '    procedure SetGlobalUnitDir(const AValue: String);'#010+
  '    procedure SetBaseInstall','Dir(const AValue: String);'#010+
  '    procedure SetCPU(const AValue: TCPU);'#010+
  '    procedure SetOS(const AValue: TOS);'#010+
  '    procedure SetPrefix(const AValue: String);'#010+
  '    procedure SetTarget(const AValue: String);'#010+
  '  Protected'#010+
  '    procedure RecalcTarge','t;'#010+
  '  Public'#010+
  '    Constructor Create;'#010+
  '    Procedure InitDefaults;'#010+
  '    Procedure Assign(ASource : TPersistent);override;'#010+
  '    procedure CompilerDefaults; virtual;'#010+
  '    Procedure LocalInit(Const AFileName : String);'#010+
  '    Procedure LoadFromFile(Con','st AFileName : String);'#010+
  '    Procedure SaveToFile(Const AFileName : String);'#010+
  '    procedure SaveToStream(S : TStream);virtual;'#010+
  '    procedure LoadFromStream(S : TStream);virtual;'#010+
  '    // Compile Information'#010+
  '    Property Target : String Read FTa','rget Write SetTarget;'#010+
  '    Property OS : TOS Read FOS Write SetOS;'#010+
  '    Property CPU : TCPU Read FCPU Write SetCPU;'#010+
  '    Property Mode : TCompilerMode Read FMode Write FMode;'#010+
  '    Property UnixPaths : Boolean Read FUnixPaths Write FUnixPaths;'#010+
  ' ','   Property Options : String Read FOptions Write FOptions;    // De'+
  'fault compiler options.'#010+
  '    Property SourceExt : String Read FSourceExt Write FSourceExt;'#010+
  '    Property NoFPCCfg : Boolean Read FNoFPCCfg Write FNoFPCCfg;'#010+
  '    // paths etc.'#010+
  ' ','   Property LocalUnitDir : String Read GetLocalUnitDir Write SetLoc'+
  'alUnitDir;'#010+
  '    Property GlobalUnitDir : String Read GetGlobalUnitDir Write SetGlo'+
  'balUnitDir;'#010+
  '    Property Prefix : String Read FPrefix Write SetPrefix;'#010+
  '    Property BaseInst','allDir : String Read GetBaseInstallDir Write Se'+
  'tBaseInstallDir;'#010+
  '    Property UnitInstallDir : String Read GetUnitInstallDir Write FUni'+
  'tInstallDir;'#010+
  '    Property BinInstallDir : String Read GetBinInstallDir Write FBinIn'+
  'stallDir;'#010+
  '    Property ','DocInstallDir : String Read GetDocInstallDir Write FDoc'+
  'InstallDir;'#010+
  '    Property ExamplesInstallDir : String Read GetExamplesInstallDir Wr'+
  'ite FExamplesInstallDir;'#010+
  '    // Command tools. If not set, internal commands  will be used.'#010+
  '    Propert','y Compiler : String Read GetCompiler Write FCompiler; // '+
  'Compiler. Defaults to fpc'#010+
  '    Property Copy : String Read FCopy Write FCopy;             // copy'+
  ' $(FILES) to $(DEST)'#010+
  '    Property Move : String Read FMove Write FMove;             // ','Mo'+
  've $(FILES) to $(DEST)'#010+
  '    Property Remove : String Read FRemove Write FRemove;       // Dele'+
  'te $(FILES)'#010+
  '    Property MkDir : String Read FMkDir write FMkDir;          // Make'+
  ' $(DIRECTORY)'#010+
  '    Property Archive : String Read FArchive Write',' FArchive;    // zi'+
  'p $(ARCHIVE) $(FILESORDIRS)'#010+
  '  end;'#010+
  #010+
  '  { TBasicDefaults }'#010+
  #010+
  '  TBasicDefaults = Class(TCustomDefaults)'#010+
  '  end;'#010+
  #010+
  '  { TFPCDefaults }'#010+
  #010+
  '  TFPCDefaults = Class(TCustomDefaults)'#010+
  '  public'#010+
  '    procedure CompilerDefaults; override;'#010+
  '  ','end;'#010+
  #010+
  '  { TBuildEngine }'#010+
  #010+
  '  TBuildEngine = Class(TComponent)'#010+
  '  private'#010+
  '    // general variables'#010+
  '    FCompiler : String;'#010+
  '    FStartDir : String;'#010+
  '    FTargetDir : String;'#010+
  '    FForceCompile : Boolean;'#010+
  '    FListMode : Boolean;'#010+
  '{$ifdef HAS_UNIT_','ZIPPER}'#010+
  '    FZipFile: TZipper;'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  '    // Variables used when compiling a package.'#010+
  '    // Only valid during compilation of the package.'#010+
  '    FCurrentOutputDir : String;'#010+
  '    FCurrentPackage: TPackage;'#010+
  '    FExternalPackage','s : TPackages;'#010+
  '    // Events'#010+
  '    FOnLog: TLogEvent;'#010+
  '    FAfterArchive: TNotifyEvent;'#010+
  '    FAfterClean: TNotifyEvent;'#010+
  '    FAfterCompile: TNotifyEvent;'#010+
  '    FAfterInstall: TNotifyEvent;'#010+
  '    FAfterManifest: TNotifyEvent;'#010+
  '    FBeforeArchive: TNot','ifyEvent;'#010+
  '    FBeforeClean: TNotifyEvent;'#010+
  '    FBeforeCompile: TNotifyEvent;'#010+
  '    FBeforeInstall: TNotifyEvent;'#010+
  '    FBeforeManifest: TNotifyEvent;'#010+
  '    procedure SetTargetDir(const AValue: String);'#010+
  '  Protected'#010+
  '    Procedure Error(const Msg : S','tring);'#010+
  '    Procedure Error(const Fmt : String; Args : Array of const);'#010+
  '    // Internal copy/delete/move/archive/mkdir files'#010+
  '    Function  SysDirectoryExists(const ADir:string):Boolean;'#010+
  '    Function  SysFileExists(const AFileName:string):Bo','olean;'#010+
  '    Procedure SysCopyFile(Const Src,Dest : String); virtual;'#010+
  '    Procedure SysMoveFile(Const Src,Dest : String); virtual;'#010+
  '    Procedure SysDeleteFile(Const AFileName : String); virtual;'#010+
  '    Procedure SysArchiveFiles(List : TStrings; ','Const AFileName : Str'+
  'ing); virtual;'#010+
  '    Procedure Log(Level : TVerboseLevel; Const Msg : String);'#010+
  '    Procedure Log(Level : TVerboseLevel; Const Fmt : String; Args : Ar'+
  'ray Of Const);'#010+
  '    Procedure EnterDir(ADir : String);'#010+
  '    Function GetCo','mpiler : String;'#010+
  '    Function InstallPackageFiles(APAckage : TPackage; tt : TTargetType'+
  '; Const Dest : String):Boolean;'#010+
  '    Function FileNewer(const Src,Dest : String) : Boolean;'#010+
  '    Function FindFileInPath(Path:TConditionalStrings; AFileNam','e:Stri'+
  'ng; var FoundPath:String;ACPU:TCPU;AOS:TOS; Const PathPrefix :String='#039+
  #039'):Boolean;'#010+
  #010+
  '    //package commands'#010+
  '    Procedure ResolveFileNames(APackage : TPackage; ACPU:TCPU;AOS:TOS)'+
  ';'#010+
  '    Function  GetOutputDir(AName: string; APackage : TPa','ckage; Absol'+
  'utePath : Boolean = False) : String;'#010+
  '    function  GetUnitDir(APackage:TPackage):String;'#010+
  '    procedure AddDependencyUnitPaths(L:TStrings;APackage: TPackage);'#010+
  '  Public'#010+
  '    Constructor Create(AOwner : TComponent); override;'#010+
  '    de','structor Destroy;override;'#010+
  '    // Public Copy/delete/Move/Archive/Mkdir Commands.'#010+
  '    Procedure ExecuteCommand(const Cmd,Args : String; IgnoreError : Bo'+
  'olean = False); virtual;'#010+
  '    Procedure CmdCopyFiles(List : TStrings; Const DestDir : Str','ing);'+
  #010+
  '    Procedure CmdCreateDir(const DestDir : String);'#010+
  '    Procedure CmdMoveFiles(List : TStrings; Const DestDir : String);'#010+
  '    Procedure CmdDeleteFiles(List : TStrings);'#010+
  '    Procedure CmdArchiveFiles(List : TStrings; Const ArchiveFile :',' S'+
  'tring);'#010+
  '    Procedure ExecuteCommands(Commands : TCommands; At : TCommandAt);'#010+
  '    // Dependency commands'#010+
  '    Function  DependencyOK(ADependency : TDependency) : Boolean;'#010+
  '    // Target commands'#010+
  '    Function  GetTargetDir(APackage : TPackag','e; ATarget : TTarget; A'+
  'bsolutePath : Boolean = False) : String;'#010+
  '    Function  GetCompilerCommand(APackage : TPackage; Target : TTarget'+
  ') : String;'#010+
  '    Function  TargetOK(ATarget : TTarget) : Boolean;'#010+
  '    Function  NeedsCompile(Target : TTarg','et) : Boolean;'#010+
  '    Procedure Compile(Target : TTarget);  virtual;'#010+
  '    Procedure MaybeCompile(Target: TTarget);'#010+
  '    Procedure CompileDependencies(Target: TTarget);'#010+
  '    // Package commands'#010+
  '    Function  GetPackageDir(APackage : TPackage; Abso','lutePath : Bool'+
  'ean = False) : String;'#010+
  '    Function  GetUnitsOutputDir(APackage : TPackage; AbsolutePath : Bo'+
  'olean = False) : String;'#010+
  '    Function  GetBinOutputDir(APackage : TPackage; AbsolutePath : Bool'+
  'ean = False) : String;'#010+
  '    Function  ','PackageOK(APackage : TPackage) : Boolean; virtual;'#010+
  '    Procedure DoBeforeCompile(APackage : TPackage);virtual;'#010+
  '    Procedure DoAfterCompile(APackage : TPackage);virtual;'#010+
  '    Procedure DoBeforeInstall(APackage : TPackage);virtual;'#010+
  '    Proced','ure DoAfterInstall(APackage : TPackage);virtual;'#010+
  '    Procedure DoBeforeArchive(APackage : TPackage);virtual;'#010+
  '    Procedure DoAfterArchive(APackage : TPackage);virtual;'#010+
  '    Procedure DoBeforeClean(APackage : TPackage);virtual;'#010+
  '    Procedure ','DoAfterClean(APackage : TPackage);virtual;'#010+
  '    Function NeedsCompile(APackage : TPackage) : Boolean; virtual;'#010+
  '    Procedure Compile(APackage : TPackage);'#010+
  '    Procedure Install(APackage : TPackage);'#010+
  '    Procedure Archive(APackage : TPackage)',';'#010+
  '    Procedure Clean(APackage : TPackage);'#010+
  '    Procedure CompileDependencies(APackage : TPackage);'#010+
  '    Procedure GetManifest(APackage : TPackage; Manifest : TStrings);'#010+
  '    Function CheckExternalPackage(Const APackageName : String):TPackag'+
  'e',';'#010+
  '    procedure CreateOutputDir(APackage: TPackage);'#010+
  '    // Packages commands'#010+
  '    Procedure Compile(Packages : TPackages);'#010+
  '    Procedure Install(Packages : TPackages);'#010+
  '    Procedure Archive(Packages : TPackages);'#010+
  '    Procedure Clean(Package','s : TPackages);'#010+
  '    Procedure GetManifest(Packages : TPackages; Manifest : TStrings);'#010+
  '    Property ListMode : Boolean Read FListMode Write FListMode;'#010+
  '    Property ForceCompile : Boolean Read FForceCompile Write FForceCom'+
  'pile;'#010+
  '//    Property',' Defaults : TCustomDefaults Read FDefaults Write SetDe'+
  'faults;'#010+
  '    Property TargetDir : String Read FTargetDir Write SetTargetDir;'#010+
  '    Property ExternalPackages: TPackages Read FExternalPackages;'#010+
  '    // Events'#010+
  '    Property BeforeCompile : TN','otifyEvent Read FBeforeCompile Write '+
  'FBeforeCompile;'#010+
  '    Property AfterCompile : TNotifyEvent Read FAfterCompile Write FAft'+
  'erCompile;'#010+
  '    Property BeforeInstall : TNotifyEvent Read FBeforeInstall Write FB'+
  'eforeInstall;'#010+
  '    Property AfterInst','all : TNotifyEvent Read FAfterInstall Write FA'+
  'fterInstall;'#010+
  '    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBefor'+
  'eClean;'#010+
  '    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterCl'+
  'ean;'#010+
  '    Property BeforeArchive :',' TNotifyEvent Read FBeforeArchive Write '+
  'FBeforeArchive;'#010+
  '    Property AfterArchive : TNotifyEvent Read FAfterArchive Write FAft'+
  'erArchive;'#010+
  '    Property BeforeManifest : TNotifyEvent Read FBeforeManifest Write '+
  'FBeforeManifest;'#010+
  '    Property Aft','erManifest : TNotifyEvent Read FAfterManifest Write '+
  'FAfterManifest;'#010+
  '    Property OnLog : TLogEvent Read FOnLog Write FOnlog;'#010+
  '  end;'#010+
  #010+
  '  { TCustomInstaller }'#010+
  #010+
  '  TCustomInstaller = Class(TComponent)'#010+
  '  private'#010+
  '    FBuildEngine: TBuildEngine;'#010+
  '  ','  FDefaultPackage: TPackage;'#010+
  '    FPackages: TPackages;'#010+
  '    FRunMode: TRunMode;'#010+
  '    FListMode : Boolean;'#010+
  '    FLogLevels : TVerboseLevels;'#010+
  '    function GetBaseInstallDir: string;'#010+
  '    Function GetPackageString(Index : Integer) : String;'#010+
  '    Pr','ocedure SetPackageString(Index : Integer; const AValue : Strin'+
  'g);'#010+
  '    function GetConditionalStrings(AIndex : Integer): TConditionalStri'+
  'ngs;'#010+
  '    function GetOSes: TOSes;'#010+
  '    function GetTargets: TTargets;'#010+
  '    function GetSources: TSources;'#010,
  '    procedure SetBaseInstallDir(const AValue: string);'#010+
  '    procedure SetDefaultPackage(const AValue: TPackage);'#010+
  '    procedure SetConditionalStrings(AIndex : Integer; const AValue: TC'+
  'onditionalStrings);'#010+
  '    procedure SeTOSes(const AValue: TO','Ses);'#010+
  '  Protected'#010+
  '    Procedure Log(Level : TVerboseLevel; Const Msg : String);'#010+
  '    Procedure CreatePackages; virtual;'#010+
  '    Procedure CheckPackages; virtual;'#010+
  '    Procedure CreateBuildEngine; virtual;'#010+
  '    Procedure CheckDefaultPackage;'#010+
  '    Pr','ocedure Error(const Msg : String);'#010+
  '    Procedure Error(const Fmt : String; Args : Array of const);'#010+
  '    Procedure AnalyzeOptions;'#010+
  '    Procedure Usage(const FMT : String; Args : Array of const);'#010+
  '    Procedure Compile(Force : Boolean); virtual',';'#010+
  '    Procedure Clean; virtual;'#010+
  '    Procedure Install; virtual;'#010+
  '    Procedure Archive; virtual;'#010+
  '    Procedure Manifest; virtual;'#010+
  '    Property BuildEngine : TBuildEngine Read FBuildEngine;'#010+
  '  Public'#010+
  '    Constructor Create(AOwner : TComponent)','; virtual;'#010+
  '    Destructor destroy; override;'#010+
  '    Function StartPackage(Const AName : String) : TPackage;'#010+
  '    Procedure EndPackage;'#010+
  '    Function Run : Boolean;'#010+
  '    Function AddTarget(const AName : String) : TTarget;'#010+
  '    Procedure AddDependen','cy(const AName : String);'#010+
  '    //files in package'#010+
  '    Property DefaultPackage : TPackage read FDefaultPackage write SetD'+
  'efaultPackage;'#010+
  '    Property Packages : TPackages Read FPackages;'#010+
  '    Property Dependencies : TConditionalStrings Index 0 ','Read GetCond'+
  'itionalStrings Write SetConditionalStrings;'#010+
  '    Property InstallFiles : TConditionalStrings Index 1 Read GetCondit'+
  'ionalStrings Write SetConditionalStrings;'#010+
  '    Property CleanFiles : TConditionalStrings Index 2 Read GetConditio'+
  'na','lStrings Write SetConditionalStrings;'#010+
  '    Property ArchiveFiles : TConditionalStrings Index 3 Read GetCondit'+
  'ionalStrings Write SetConditionalStrings;'#010+
  '    Property RunMode : TRunMode Read FRunMode;'#010+
  '    Property ListMode : Boolean Read FListM','ode;'#010+
  '    Property BaseInstallDir : String Read GetBaseInstallDir Write SetB'+
  'aseInstallDir;'#010+
  '    // Default Package redirects.'#010+
  '    Property Targets : TTargets Read GetTargets;'#010+
  '    Property Sources : TSources Read GetSources;'#010+
  '    Property OS: T','OSes Read GetOSes Write SetOSes;'#010+
  '    Property Author : String Index 0 Read GetPackageString Write SetPa'+
  'ckageString;'#010+
  '    Property Directory : String Index 1 Read GetPackageString Write Se'+
  'tPackageString;'#010+
  '    Property License : String Index 2 ','Read GetPackageString Write Se'+
  'tPackageString;'#010+
  '    Property Options : String Index 3 Read GetPackageString Write SetP'+
  'ackageString;'#010+
  '    Property ExternalURL : String Index 4 Read GetPackageString Write '+
  'SetPackageString;'#010+
  '    Property Email : S','tring Index 5 Read GetPackageString Write SetP'+
  'ackageString;'#010+
  '    Property Description: String Index 6 Read GetPackageString Write S'+
  'etPackageString;'#010+
  '    Property DescriptionFileName: String Index 7 Read GetPackageString'+
  ' Write SetPackageString',';'#010+
  '    Property Version : String Index 8 Read GetPackageString Write SetP'+
  'ackageString;'#010+
  '    Property FileName : String Index 9 Read GetPackageString Write Set'+
  'PackageString;'#010+
  '  end;'#010+
  #010+
  '  { TFPCInstaller }'#010+
  '  TFPCInstaller = class(TCustomInstaller)',#010+
  '  public'#010+
  '    Constructor Create(AOwner : TComponent); override;'#010+
  '  end;'#010+
  #010+
  '  { TBasicInstaller }'#010+
  '  TBasicInstaller = class(TCustomInstaller)'#010+
  '    Constructor Create(AOwner : TComponent); override;'#010+
  '  end;'#010+
  #010+
  '  TReplaceFunction = Function (Const A','Name,Args : String) : String o'+
  'f Object;'#010+
  #010+
  '  { TValueItem }'#010+
  #010+
  '  TValueItem = Class(TObject)'#010+
  '    FValue : String;'#010+
  '    Constructor Create(AValue : String);'#010+
  '  end;'#010+
  #010+
  '  { TFunctionItem }'#010+
  #010+
  '  TFunctionItem = Class(TObject)'#010+
  '    FFunc : TReplaceFunctio','n;'#010+
  '    Constructor Create(AFunc : TReplaceFunction);'#010+
  '  end;'#010+
  #010+
  '  { TDictionary }'#010+
  #010+
  '  TDictionary = Class(TComponent)'#010+
  '    FList : TStringList;'#010+
  '  Public'#010+
  '    Constructor Create(AOwner : TComponent); override;'#010+
  '    Destructor Destroy;override;'#010+
  '    ','Procedure AddVariable(Const AName,Value : String);'#010+
  '    Procedure AddFunction(Const AName : String; FReplacement : TReplac'+
  'eFunction);'#010+
  '    Procedure RemoveItem(Const AName : String);'#010+
  '    Function GetValue(Const AName : String) : String;'#010+
  '    F','unction GetValue(Const AName,Args : String) : String; virtual;'#010+
  '    Function ReplaceStrings(Const ASource : String) : String; virtual;'+
  #010+
  '  end;'#010+
  #010+
  '  ECollectionError = Class(Exception);'#010+
  '  EDictionaryError = Class(Exception);'#010+
  '  EInstallerError = ','Class(Exception);'#010+
  #010+
  '  TInstallerClass = Class of TCustomInstaller;'#010+
  '  TDictionaryClass = Class of TDictionary;'#010+
  #010+
  'Type'#010+
  '  TArchiveEvent = Procedure (Const AFileName : String; List : TStrings'+
  ') of Object;'#010+
  '  TArchiveProc = Procedure (Const AFileNam','e : String; List : TString'+
  's);'#010+
  #010+
  'Var'#010+
  '  DictionaryClass : TDictionaryClass = TDictionary;'#010+
  '  OnArchiveFiles : TArchiveEvent = Nil;'#010+
  '  ArchiveFilesProc : TArchiveProc = Nil;'#010+
  #010+
  '  Defaults : TCustomDefaults; // Set by installer.'#010+
  '  Dictionary : TDict','ionary;'#010+
  #010+
  #010+
  'Function CurrentOS : String;'#010+
  'Function CurrentCPU : String;'#010+
  #010+
  'Function Installer(InstallerClass: TInstallerClass) : TCustomInstaller'+
  '; overload;'#010+
  'Function Installer : TCustomInstaller; overload;'#010+
  #010+
  'Function OSToString(OS: TOS) : String;',#010+
  'Function OSesToString(OSes: TOSes) : String;'#010+
  'Function CPUToString(CPU: TCPU) : String;'#010+
  'Function CPUSToString(CPUS: TCPUS) : String;'#010+
  'Function StringToOS(const S : String) : TOS;'#010+
  'Function OSesToString(const S : String) : TOSes;'#010+
  'Function Stri','ngToCPU(const S : String) : TCPU;'#010+
  'Function StringToCPUS(const S : String) : TCPUS;'#010+
  'Function ModeToString(Mode: TCompilerMode) : String;'#010+
  'Function StringToMode(const S : String) : TCompilerMode;'#010+
  'Function MakeTargetString(CPU : TCPU;OS: TOS) :',' String;'#010+
  'Procedure StringToCPUOS(const S : String; Var CPU : TCPU; Var OS: TOS)'+
  ';'#010+
  'Function FixPath (const APath : String) : String;'#010+
  'Procedure ChangeDir(const APath : String);'#010+
  'Function Substitute(Const Source : String; Macros : Array of strin','g)'+
  ' : String;'#010+
  'Procedure SplitCommand(Const Cmd : String; Var Exe,Options : String);'#010+
  #010+
  'Implementation'#010+
  #010+
  'uses typinfo;'#010+
  #010+
  'ResourceString'#010+
  '  SErrInvalidCPU        = '#039'Invalid CPU name "%s"'#039';'#010+
  '  SErrInvalidOS         = '#039'Invalid OS name "%s"'#039';'#010+
  '  SErrInv','alidMode       = '#039'Invalid compiler mode "%s"'#039';'#010+
  '  SErrInvalidTarget     = '#039'Invalid compiler target "%s"'#039';'#010+
  '  SErrNameExists        = '#039'Name "%s" already exists in the collect'+
  'ion.'#039';'#010+
  '  SErrNoSuchName        = '#039'Could not find item with name "%s" ','in'+
  ' the collection.'#039';'#010+
  '  SErrNoPackage         = '#039'No package available. Add package with '+
  'StartPackage Call'#039';'#010+
  '  SErrInValidArgument   = '#039'Invalid command-line argument at positi'+
  'on %d: %s'#039';'#010+
  '  SErrNeedArgument      = '#039'Option at position %d (%s) n','eeds an a'+
  'rgument'#039';'#010+
  '  SErrNoPackagesDefined = '#039'No action possible: No packages were de'+
  'fined.'#039';'#010+
  '  SErrInstaller         = '#039'The installer encountered the following'+
  ' error:'#039';'#010+
  '  SErrDepUnknownTarget  = '#039'Unknown target in dependencies for %s: '+
  '%s'#039';',#010+
  '  SErrExternalCommandFailed = '#039'External command "%s" failed with e'+
  'xit code %d'#039';'#010+
  '  SErrCreatingDirectory = '#039'Failed to create directory: "%s"'#039';'#010+
  '  SErrDeletingFile      = '#039'Failed to delete file "%s"'#039';'#010+
  '  SErrMovingFile        = '#039'Failed to move',' file "%s" to "%s"'#039';'+
  #010+
  '  SErrCopyingFile       = '#039'Failed to copy file "%s" to "%s"'#039';'#010+
  '  SErrChangeDirFailed   = '#039'Failed to enter directory: %s'#039';'#010+
  '  SErrInvalidArgumentToSubstitute = '#039'Invalid number of arguments t'+
  'o Substitute'#039';'#010+
  '  SErrNoArchiveSu','pport  = '#039'This binary contains no archive suppo'+
  'rt. Please recompile with archive support'#039';'#010+
  '  SErrNoDictionaryItem  = '#039'No item called "%s" in the dictionary'#039+
  ';'#010+
  '  SErrNoDictionaryValue = '#039'The item "%s" in the dictionary is not '+
  'a value.'#039';'#010+
  '  SEr','rNoDictionaryFunc  = '#039'The item "%s" in the dictionary is no'+
  't a function.'#039';'#010+
  '  SErrInvalidFPCInfo    = '#039'Compiler returns invalid information, c'+
  'heck if fpc -iV works'#039';'#010+
  '  SErrDependencyNotFound = '#039'Could not find unit directory for depe'+
  'ndency pa','ckage "%s"'#039';'#010+
  '  SErrAlreadyInitialized = '#039'Installer can only be initialized once'+
  #039';'#010+
  #010+
  '  SWarnCircularDependency = '#039'Warning: Circular dependency detected'+
  ' when compiling target %s: %s'#039';'#010+
  '  SWarnFailedToSetTime    = '#039'Warning: Failed to set timesta','mp on'+
  ' file %s'#039';'#010+
  '  SWarnFailedToGetTime    = '#039'Warning: Failed to get timestamp from'+
  ' file %s'#039';'#010+
  '  SWarnFileDoesNotExist   = '#039'Warning: File "%s" does not exist'#039+
  ';'#010+
  '  SWarnAttemptingToCompileNonNeutralTarget = '#039'Attempting to compil'+
  'e non-neutral ta','rget: %s'#039';'#010+
  #010+
  '  SInfoEnterDir           = '#039'Entering directory "%s"'#039';'#010+
  '  SInfoCompilingPackage   = '#039'Compiling package %s'#039';'#010+
  '  SInfoCompilingTarget    = '#039'Compiling target %s'#039';'#010+
  '  SInfoExecutingCommand   = '#039'Executing command "%s %s"'#039';'#010+
  '  SInfoCreati','ngOutputDir  = '#039'Creating output dir "%s"'#039';'#010+
  '  SInfoInstallingPackage  = '#039'Installing package %s'#039';'#010+
  '  SInfoArchivingPackage   = '#039'Archiving package %s'#039';'#010+
  '  SInfoArchivingFile      = '#039'Archiving "%s"'#039';'#010+
  '  SInfoCleaningPackage    = '#039'Cleaning package ','%s'#039';'#010+
  '  SInfoCopyingFile        = '#039'Copying file "%s" to "%s"'#039';'#010+
  '  SInfoSourceNewerDest    = '#039'Source file "%s" (%s) is newer than d'+
  'estination "%s" (%s).'#039';'#010+
  #010+
  '  SDbgComparingFileTimes    = '#039'Comparing file "%s" time "%s" to "%'+
  's" time "%s".'#039';'#010+
  '  SDb','gCompilingDependenciesOfTarget = '#039'Compiling dependencies of'+
  ' target: %s'#039';'#010+
  '  SDbgResolvedSourceFile    = '#039'Resolved source file %s to "%s"'#039+
  ';'#010+
  '  SDbgResolvedIncludeFile   = '#039'Resolved include file %s to "%s"'#039+
  ';'#010+
  '  SDbgOutputNotYetAvailable = '#039'Outpu','t file %s not available'#039';'+
  #010+
  '  SDbgDependencyOnUnit      = '#039'Dependency of %s on unit %s'#039';'#010+
  '  SDbgDependencyUnitRecompiled = '#039'Dependent unit %s is being recom'+
  'piled'#039';'#010+
  '  SDbgMustCompile           = '#039'Must compile %s'#039';'#010+
  '  SDbgTargetHasWrongOS      = ',#039'Target has wrong OS: %s'#039';'#010+
  '  SDbgTargetHasWrongCPU     = '#039'Target has wrong CPU: %s'#039';'#010+
  '  SDbgTargetIsNotAUnitOrProgram = '#039'Skipping Target %s, not an unit'+
  ' or program'#039';'#010+
  '  SDbgConsideringTarget     = '#039'Considering target %s'#039';'#010+
  '  SDbgConsideringPac','kage    = '#039'Considering package %s'#039';'#010+
  '  SDbgExternalDependency    = '#039'External dependency %s found in "%s'+
  '"'#039';'#010+
  '  SDbgBuildEngineArchiving  = '#039'Build engine archiving.'#039';'#010+
  '  SDbgBuildEngineCleaning   = '#039'Build engine cleaning.'#039';'#010+
  '  SDbgGenerating     ','       = '#039'Generating "%s"'#039';'#010+
  '  SDbgLoading               = '#039'Loading "%s"'#039';'#010+
  '  SDbgFound                 = '#039'Found'#039';'#010+
  '  SDbgNotFound              = '#039'Not Found'#039';'#010+
  '  SDbgDirectoryExists       = '#039'Directory "%s" %s'#039';'#010+
  '  SDbgFileExists            = '#039'Fi','le "%s" %s'#039';'#010+
  #010+
  '  // Help messages for usage'#010+
  '  SValue              = '#039'Value'#039';'#010+
  '  SHelpUsage          = '#039'Usage: %s command [options]'#039';'#010+
  '  SHelpCommand        = '#039'Where command is one of the following:'#039+
  ';'#010+
  '  SHelpCompile        = '#039'Compile all units ','in the package(s).'#039+
  ';'#010+
  '  SHelpBuild          = '#039'Build all units in the package(s).'#039';'#010+
  '  SHelpInstall        = '#039'Install all units in the package(s).'#039';'+
  #010+
  '  SHelpClean          = '#039'Clean (remove) all units in the package(s'+
  ').'#039';'#010+
  '  SHelpArchive        ','= '#039'Create archive (zip) with all units in '+
  'the package(s).'#039';'#010+
  '  SHelpHelp           = '#039'This message.'#039';'#010+
  '  SHelpManifest       = '#039'Create a manifest suitable for import in '+
  'repository.'#039';'#010+
  '  SHelpCmdOptions     = '#039'Where options is one or more of th','e fol'+
  'lowing:'#039';'#010+
  '  SHelpCPU            = '#039'Compile for indicated CPU.'#039';'#010+
  '  SHelpOS             = '#039'Compile for indicated OS'#039';'#010+
  '  SHelpTarget         = '#039'Compile for indicated target'#039';'#010+
  '  SHelpList           = '#039'list commands instead of actually exec','u'+
  'ting them.'#039';'#010+
  '  SHelpPrefix         = '#039'Use indicated prefix directory for all co'+
  'mmands.'#039';'#010+
  '  SHelpNoFPCCfg       = '#039'Compiler will not use fpc.cfg'#039';'#010+
  '  SHelpBaseInstallDir = '#039'Use indicated directory as base install d'+
  'ir.'#039';'#010+
  '  SHelpLocalUnitDir  ',' = '#039'Use indicated directory as local (user)'+
  ' unit dir.'#039';'#010+
  '  SHelpGlobalUnitDir  = '#039'Use indicated directory as global unit di'+
  'r.'#039';'#010+
  '  SHelpCompiler       = '#039'Use indicated binary as compiler'#039';'#010+
  '  SHelpConfig         = '#039'Use indicated config file wh','en compilin'+
  'g.'#039';'#010+
  '  SHelpVerbose        = '#039'Be verbose when working.'#039';'#010+
  #010+
  #010+
  'Const'#010+
  '  // Keys for Defaults file. Do not localize.'#010+
  '  KeyCompiler = '#039'Compiler'#039';'#010+
  '  KeyArchive  = '#039'Archive'#039';'#010+
  '  KeyCopy     = '#039'Copy'#039';'#010+
  '  KeyMkDir    = '#039'MkDir'#039';'#010+
  '  KeyMove     ','= '#039'Move'#039';'#010+
  '  KeyRemove   = '#039'Remove'#039';'#010+
  '  KeyOptions  = '#039'Options'#039';'#010+
  '  KeyCPU      = '#039'CPU'#039';'#010+
  '  KeyOS       = '#039'OS'#039';'#010+
  '  KeyMode     = '#039'Mode'#039';'#010+
  '  KeyPrefix   = '#039'Prefix'#039';'#010+
  '  KeyTarget   = '#039'Target'#039';'#010+
  '  KeyNoFPCCfg = '#039'NoFPCCfg'#039';'#010+
  '  KeyLocalUnitDir       = '#039'L','ocalUnitDir'#039';'#010+
  '  KeyGlobalUnitDir      = '#039'GlobalUnitDir'#039';'#010+
  '  KeyBaseInstallDir     = '#039'BaseInstallDir'#039';'#010+
  '  KeyUnitInstallDir     = '#039'UnitInstallDir'#039';'#010+
  '  KeyBinInstallDir      = '#039'BinInstallDir'#039';'#010+
  '  KeyDocInstallDir      = '#039'DocInstallDir'#039';'#010+
  '  KeyExam','plesInstallDir = '#039'ExamplesInstallDir'#039';'#010+
  '  KeySourceExt = '#039'SourceExt'#039';'#010+
  '  // Keys for unit config'#010+
  '  KeyName     = '#039'Name'#039';'#010+
  '  KeyVersion  = '#039'Version'#039';'#010+
  '  KeyNeedLibC = '#039'NeedLibC'#039';'#010+
  '  KeyDepends  = '#039'Depends'#039';'#010+
  #010+
  '{*************************************','******************************'+
  '*********'#010+
  '                                Helpers'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  #010+
  'Procedure SplitVersion(AValue: String; Var Release,Major,Minor : Word;'+
  ' Var Suffi','x : String);'#010+
  #010+
  '  Function NextDigit(sep : Char; var V : string) : integer;'#010+
  '  Var'#010+
  '    P : Integer;'#010+
  '  begin'#010+
  '    P:=Pos(Sep,V);'#010+
  '    If (P=0) then'#010+
  '      P:=Length(V)+1;'#010+
  '    Result:=StrToIntDef(Copy(V,1,P-1),-1);'#010+
  '    If Result<>-1 then'#010+
  '      Dele','te(V,1,P)'#010+
  '    else'#010+
  '      Result:=0;'#010+
  '  end;'#010+
  #010+
  'Var'#010+
  '  P : Integer;'#010+
  '  V : String;'#010+
  'begin'#010+
  '  Release:=0;'#010+
  '  Major:=0;'#010+
  '  Minor:=0;'#010+
  '  Suffix:='#039#039';'#010+
  '  V:=AValue;'#010+
  '  Release:=NextDigit('#039'.'#039',V);'#010+
  '  Major:=NextDigit('#039'.'#039',V);'#010+
  '  Minor:=NextDigit('#039'-'#039',V);'#010+
  '  P:=Pos(',#039'-'#039',V);'#010+
  '  If (P<>0) then'#010+
  '    Delete(V,1,P);'#010+
  '  Suffix:=V;'#010+
  'end;'#010+
  #010+
  #010+
  'Function QuoteXML(S : String) : string;'#010+
  #010+
  '  Procedure W(Var J : Integer; Var R : String; T : String);'#010+
  '  Var'#010+
  '    I: integer;'#010+
  '  begin'#010+
  '    If J+Length(T)>Length(R) then'#010+
  '      SetLe','ngth(R,J+Length(T));'#010+
  '    For I:=1 to Length(t) do'#010+
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
  '  I,J : Int','eger;'#010+
  'begin'#010+
  '  SetLength(Result,Length(S));'#010+
  '  J:=0;'#010+
  '  For I:=1 to Length(S) do'#010+
  '    begin'#010+
  '    Inc(J);'#010+
  '    case S[i] of'#010+
  '      '#039'"'#039': W(j,Result,QuotStr);'#010+
  '      '#039'&'#039': W(J,Result,AmpStr);'#010+
  '      '#039'<'#039': W(J,Result,ltStr);'#010+
  '      // Escape whitespace usi','ng CharRefs to be consistent with W3 s'+
  'pec X 3.3.3'#010+
  '       #9: w(J,Result,'#039'&#x9;'#039');'#010+
  '{      #10: wrtStr('#039'&#xA;'#039');'#010+
  '       #13: wrtStr('#039'&#xD;'#039');}'#010+
  '    else'#010+
  '      Result[J]:=S[i];'#010+
  '    end;'#010+
  '    If (J=Length(Result)) and (I<Length(S)) then'#010+
  '      Set','Length(Result,J+Length(S)-I);'#010+
  '    end;'#010+
  '  If J<>Length(Result) then'#010+
  '    SetLength(Result,J);'#010+
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
  'Function CurrentOS : String;'#010,
  #010+
  'begin'#010+
  '  Result:=OSToString(Defaults.OS);'#010+
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
  'Fun','ction OSesToString(OSes: TOSes) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=LowerCase(SetToString(PtypeInfo(TypeInfo(TOSes)),Integer(OSe'+
  's),False));'#010+
  'end;'#010+
  #010+
  'Function CPUToString(CPU: TCPU) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=LowerCase(GetenumName(TypeInfo(TCPU),Ord(CPU',')));'#010+
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
  '  I:=GetEnumValue','(TypeInfo(TOS),S);'#010+
  '  if (I=-1) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidOS,[S]);'#010+
  '  Result:=TOS(I);'#010+
  'end;'#010+
  #010+
  #010+
  'Function OSesToString(const S : String) : TOSes;'#010+
  #010+
  'begin'#010+
  '  Result:=TOSes(StringToSet(PTypeInfo(TypeInfo(TOSes)),S));'#010+
  'end;'#010+
  #010+
  'F','unction StringToCPU(const S : String) : TCPU;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=GetEnumValue(TypeInfo(TCPU),S);'#010+
  '  if (I=-1) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidCPU,[S]);'#010+
  '  Result:=TCPU(I);'#010+
  'end;'#010+
  #010+
  'Function StringToCPUS(const S :',' String) : TCPUS;'#010+
  #010+
  'begin'#010+
  '  Result:=TCPUS(StringToSet(PTypeInfo(TypeInfo(TCPUS)),S));'#010+
  'end;'#010+
  #010+
  'Function ModeToString(Mode: TCompilerMode) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=LowerCase(GetenumName(TypeInfo(TCompilerMode),Ord(Mode)));'#010+
  '  Delete(Result,1,2)',';'#010+
  'end;'#010+
  #010+
  'Function StringToMode(const S : String) : TCompilerMode;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=GetEnumValue(TypeInfo(TCompilerMode),S);'#010+
  '  if (I=-1) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidMode,[S]);'#010+
  '  Result:=TCompilerMode(I);',#010+
  'end;'#010+
  #010+
  #010+
  'Function MakeTargetString(CPU : TCPU;OS: TOS) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=CPUToString(CPU)+'#039'-'#039'+OSToString(OS);'#010+
  'end;'#010+
  #010+
  'Procedure StringToCPUOS(const S : String; Var CPU : TCPU; Var OS: TOS)'+
  ';'#010+
  #010+
  'Var'#010+
  '  P : integer;'#010+
  #010+
  'begin'#010+
  '  P:=Pos('#039'-'#039',S);'#010,
  '  If (P=0) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidTarget,[S]);'#010+
  '  CPU:=StringToCPU(Copy(S,1,P-1));'#010+
  '  OS:=StringToOs(Copy(S,P+1,Length(S)-P));'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure ResolveDependencies(L : TDependencies; P : TNamedCollection)'+
  ';'#010+
  'Var'#010+
  '  I',',J : Integer;'#010+
  '  C : TDependency;'#010+
  'begin'#010+
  '  If Assigned(L) then'#010+
  '    For I:=0 to L.Count-1 do'#010+
  '      begin'#010+
  '        C:=L[i];'#010+
  '        if C.DependencyType in [depPackage,depUnit] then'#010+
  '          begin'#010+
  '            J:=P.IndexOfName(C.Value);'#010+
  '         ','   If J<>-1 then'#010+
  '              C.Target:=P.Items[J];'#010+
  '          end;'#010+
  '      end;'#010+
  'end;'#010+
  #010+
  #010+
  'function AddConditionalStrings(Dest : TStrings; Src : TConditionalStri'+
  'ngs;ACPU:TCPU;AOS:TOS; Const APrefix : String='#039#039') : Integer ;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  C',' : TConditionalString;'#010+
  'begin'#010+
  '  Result:=0;'#010+
  '  For I:=0 to Src.Count-1 do'#010+
  '    begin'#010+
  '      C:=Src[I];'#010+
  '      if (ACPU in C.CPUs) and (AOS in C.OSes) then'#010+
  '        begin'#010+
  '          If (APrefix<>'#039#039') then'#010+
  '            Dest.Add(APrefix+C.Value)'#010+
  '       ','   else'#010+
  '            Dest.Add(C.Value);'#010+
  '          Inc(Result);'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure AddConditionalStrings(Var S : String; Src : TConditionalStri'+
  'ngs;ACPU:TCPU;AOS:TOS; const APrefix : String='#039#039');'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  C : TCo','nditionalString;'#010+
  'begin'#010+
  '  For I:=0 to Src.Count-1 do'#010+
  '    begin'#010+
  '      C:=Src[I];'#010+
  '      if (ACPU in C.CPUs) and (AOS in C.OSes) then'#010+
  '        begin'#010+
  '          if (S<>'#039#039') then'#010+
  '            S:=S+'#039' '#039';'#010+
  '          S:=S+APrefix+C.Value;'#010+
  '        end;'#010+
  '   ',' end;'#010+
  'end;'#010+
  #010+
  #010+
  'Function EnsureConditionalStrings(Var S : TConditionalStrings) : TCond'+
  'itionalStrings;'#010+
  'begin'#010+
  '  If (S=Nil) then'#010+
  '    S:=TConditionalStrings.Create(TConditionalString);'#010+
  '  Result:=S;'#010+
  'end;'#010+
  #010+
  #010+
  'function FileListToString(List : TStrings;',' const APrefix : String) :'+
  ' String;'#010+
  'Var'#010+
  '  I : integer;'#010+
  '  S : String;'#010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  For I:=0 to List.Count-1 do'#010+
  '    begin'#010+
  '    If (I>0) then'#010+
  '      Result:=Result+'#039' '#039';'#010+
  '    S:=APrefix+List[i];'#010+
  '    If (Pos('#039' '#039',S)<>0) then'#010+
  '      S:='#039'"'#039'+S+'#039,'"'#039';'#010+
  '    Result:=Result+S;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function FixPath (const APath : String) : String;'#010+
  #010+
  'Var'#010+
  '  P : PChar;'#010+
  #010+
  'begin'#010+
  '  Result:=APath;'#010+
  '  If (result<>'#039#039') then'#010+
  '    begin'#010+
  '    P:=PChar(Result);'#010+
  '    While (P^<>#0) do'#010+
  '      begin'#010+
  '      If P^ in ['#039'/',#039','#039'\'#039'] then'#010+
  '        P^:=PathDelim;'#010+
  '      Inc(P);'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure ChangeDir(const APath : String);'#010+
  #010+
  'begin'#010+
  '  if Not SetCurrentDir(APath) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrChangeDirFailed,[APath]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedu','re SearchFiles(const AFileName: string; Recursive: boolean; v'+
  'ar List: TStrings);'#010+
  #010+
  '  procedure AddRecursiveFiles(const SearchDir, FileMask: string; Recur'+
  'sive: boolean);'#010+
  '  const'#010+
  '    IgnoreCase = {$ifdef UNIX}False{$else}True{$endif};'#010+
  '  var'#010+
  '  ','  Info : TSearchRec;'#010+
  '  begin'#010+
  '    if FindFirst(SearchDir+'#039'*'#039',faAnyFile and faDirectory,Info)'+
  '=0 then'#010+
  '    begin'#010+
  '      repeat'#010+
  '          if ((Info.Attr and faDirectory) = faDirectory) and (Info.Nam'+
  'e <> '#039'.'#039') and (Info.Name <> '#039'..'#039') and (Recursive',') th'+
  'en'#010+
  '            AddRecursiveFiles(SearchDir + Info.Name + PathDelim, FileM'+
  'ask, Recursive);'#010+
  '          if ((Info.Attr and faDirectory) <> faDirectory) and IsWild(I'+
  'nfo.Name, FileMask, IgnoreCase) then'#010+
  '            List.Add(SearchDir + Info.N','ame);'#010+
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
  '  AddRecursiveFiles(BasePath, ExtractFileName(AFileN','ame), Recursive)'+
  ';'#010+
  #010+
  '  CurrDir:=GetCurrentDir;'#010+
  '  for i := 0 to Pred(List.Count) do'#010+
  '    List[i] := ExtractRelativepath(CurrDir, List[i]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure SplitCommand(const Cmd : String; var Exe, Options : String);'+
  #010+
  #010+
  'Const'#010+
  '  WhiteSpace = [#9,#','10,#13,'#039' '#039'];'#010+
  '  QuoteChars = ['#039#039#039#039','#039'"'#039'];'#010+
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
  '  LastQuote:=#0;'#010+
  '  I:=1;'#010+
  '  While (I<=Length(S)) and (Inquote or not (S[I] in whiteSp','ace)) do'#010+
  '    begin'#010+
  '    If S[i] in QuoteChars then'#010+
  '      begin'#010+
  '      InQuote:=Not (S[i]=LastQuote);'#010+
  '      If InQuote then'#010+
  '         LastQuote:=S[i]'#010+
  '       else'#010+
  '         LastQuote:=#0;'#010+
  '      end;'#010+
  '    Inc(I);'#010+
  '    end;'#010+
  '  Exe:=Copy(S,1,I-1);'#010+
  '  De','lete(S,1,I);'#010+
  '  Options:=Trim(S);'#010+
  'end;'#010+
  #010+
  #010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  'function GetCompilerInfo(const ACompiler,AOptions:string):string;'#010+
  'const'#010+
  '  BufSize = 1024;'#010+
  'var'#010+
  '  S: TProcess;'#010+
  '  Buf: array [0..BufSize - 1] of char;'#010+
  '  Count: longint;'#010+
  'begin'#010+
  '  ','S:=TProcess.Create(Nil);'#010+
  '  S.Commandline:=ACompiler+'#039' '#039'+AOptions;'#010+
  '  S.Options:=[poUsePipes];'#010+
  '  S.execute;'#010+
  '  Count:=s.output.read(buf,BufSize);'#010+
  '  S.Free;'#010+
  '  SetLength(Result,Count);'#010+
  '  Move(Buf,Result[1],Count);'#010+
  'end;'#010+
  '{$endif HAS_UNIT_PROCESS}'#010,
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TNamedItem'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'procedure TNamedItem.SetName(const AVa','lue: String);'#010+
  #010+
  'begin'#010+
  '  if FName=AValue then exit;'#010+
  '  With TNamedCollection(Collection) do'#010+
  '    If UniqueNames then'#010+
  '      If (IndexOfName(AVAlue)<>-1) then'#010+
  '        Raise ECollectionError.CreateFmt(SErrNameExists,[AValue]);'#010+
  '  FName:=AValue;'#010+
  'end',';'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TNamedCollection'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TNamedCollection.Inde','xOfName(const AName: String): Integer;'+
  #010+
  #010+
  'begin'#010+
  '  Result:=Count-1;'#010+
  '  While (Result>=0) and (CompareText(TNamedItem(Items[Result]).FName,A'+
  'Name)<>0) do'#010+
  '    Dec(Result);'#010+
  'end;'#010+
  #010+
  'function TNamedCollection.ItemByName(const AName: String): TNamedItem',
  ';'#010+
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
  '{*******************************************************************','*'+
  '********'#010+
  '                             TNamedItemList'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TNamedItemList.GetNamedItem(Index : Integer): TNamedItem;'#010+
  'begin'#010+
  '  Result:=TNamedItem(Items[Index])',';'#010+
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
  'function TNamedItemList.IndexOfName(const AName: String): Integer;'#010+
  'begin'#010+
  '  Result:=Count-1;'#010+
  '  While (Result>=0) ','and (CompareText(GetNamedItem(Result).Name,AName'+
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
  '    Raise ECollectionEr','ror.CreateFmt(SErrNoSuchName,[AName]);'#010+
  '  Result:=TNamedItem(Items[i]);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TTargets'#010+
  '*******************************************','*************************'+
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
  '  Result:=TTarget(ItemByName(ANam','e));'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TTargets.SetTargetItem(Index : Integer; const AValue: TTarge'+
  't);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddUnit(const AUnitName : String) : TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AUnitNa','me;'#010+
  '  Result.TargetType:=TTUnit;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddImplicitUnit(const AUnitName : String;InstallUnit'+
  ':boolean=true) : TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AUnitName;'#010+
  '  if InstallUnit then'#010+
  '    Result.TargetType:=T','TImplicitUnit'#010+
  '  else'#010+
  '    Result.TargetType:=TTCleanOnlyUnit;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddProgram(const AProgramName: String) : TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AProgramName;'#010+
  '  Result.TargetType:=ttProgram;'#010+
  'end;'#010+
  #010+
  #010+
  'Func','tion TTargets.AddExampleUnit(const AUnitName: String): TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AUnitName;'#010+
  '  Result.TargetType:=ttExampleUnit;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TTargets.AddExampleProgram(const AProgramName: String): TTarg'+
  'et;'#010+
  'b','egin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AProgramName;'#010+
  '  Result.TargetType:=ttExampleProgram;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                TSources'#010+
  '***************','*****************************************************'+
  '********}'#010+
  #010+
  'function TSources.GetSourceItem(Index : Integer): TSource;'#010+
  'begin'#010+
  '  Result:=TSource(Items[Index]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TSources.SetSourceItem(Index : Integer; const AValue: TSourc'+
  'e',');'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'function TSources.AddDocFiles(const AFiles : String) : TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.FSourceType:=stDoc;'#010+
  'end;'#010+
  #010+
  #010+
  'function TSources.AddSrcFiles(const AFiles : ','String) : TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.FSourceType:=stSrc;'#010+
  'end;'#010+
  #010+
  #010+
  'function TSources.AddExampleFiles(const AFiles : String) : TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Resul','t.FSourceType:=stExample;'#010+
  'end;'#010+
  #010+
  #010+
  'function TSources.AddTestFiles(const AFiles : String) : TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.FSourceType:=stTest;'#010+
  'end;'#010+
  #010+
  #010+
  '{*************************************************','******************'+
  '*********'#010+
  '                             TPackage'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TPackage.Create(ACollection: TCollection);'#010+
  'begin'#010+
  '  inherited Create(ACollection);'#010+
  ' ',' FTargets:=TTargets.Create(TTarget);'#010+
  '  FSources:=TSources.Create(TSource);'#010+
  '  FDependencies:=TDependencies.Create(TDependency);'#010+
  '  FInstallFiles:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FCleanFiles:=TConditionalStrings.Create(TCondi','tionalString);'#010+
  '  FArchiveFiles:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FUnitPath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FObjectPath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FIncludePath:=TConditionalStrings.','Create(TConditionalString);'#010+
  '  FSourcePath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FExamplePath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FTestPath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FCommands:=TCommands.C','reate(TCommand);'#010+
  '  FCPUs:=AllCPUs;'#010+
  '  FOSes:=AllOSes;'#010+
  '  // Implicit dependency on RTL'#010+
  '  FDependencies.Add('#039'rtl'#039');'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TPackage.destroy;'#010+
  'begin'#010+
  '  FreeAndNil(FDependencies);'#010+
  '  FreeAndNil(FInstallFiles);'#010+
  '  FreeAndNil(FCleanFiles);'#010,
  '  FreeAndNil(FArchiveFiles);'#010+
  '  FreeAndNil(FIncludePath);'#010+
  '  FreeAndNil(FSourcePath);'#010+
  '  FreeAndNil(FExamplePath);'#010+
  '  FreeAndNil(FTestPath);'#010+
  '  FreeAndNil(FObjectPath);'#010+
  '  FreeAndNil(FUnitPath);'#010+
  '  FreeAndNil(FSources);'#010+
  '  FreeAndNil(FTargets);'#010+
  '  i','nherited destroy;'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure TPackage.GetManifest(Manifest : TStrings);'#010+
  'Var'#010+
  '  S : String;'#010+
  '  Release,Minor,Major : Word;'#010+
  '  i : Integer;'#010+
  '  D : TConditionalString;'#010+
  'begin'#010+
  '  With Manifest do'#010+
  '    begin'#010+
  '    Add(Format('#039'<package name="%s">'#039',[','QuoteXml(Name)]));'#010+
  '    SplitVersion(Version,Release,Minor,Major,S);'#010+
  '    Add(Format('#039'<version release="%d" major="%d" minor="%d" suffix'+
  '="%s"/>'#039',[Release,Minor,Major,QuoteXMl(S)]));'#010+
  '    Add(Format('#039'<filename>%s</filename>'#039',[QuoteXml(FileName ','+'+
  ' ZipExt)]));'#010+
  '    Add(Format('#039'<author>%s</author>'#039',[QuoteXml(Author)]));'#010+
  '    Add(Format('#039'<license>%s</license>'#039',[QuoteXml(License)]));'#010+
  '    if ExternalURL<>'#039#039' then'#010+
  '      Add(Format('#039'<externalurl>%s</externalurl>'#039',[QuoteXml(Ext'+
  'ernalURL)]));'#010+
  ' ','   Add(Format('#039'<email>%s</email>'#039',[QuoteXMl(Email)]));'#010+
  '    S:=Description;'#010+
  '    If (S<>'#039#039') then'#010+
  '      Add(Format('#039'<description>%s</description>'#039',[QuoteXML(S)]'+
  '));'#010+
  '    If (Dependencies.Count>0) then'#010+
  '      begin'#010+
  '        Add('#039'<dependencies>'#039');'#010+
  ' ','       for I:=0 to Dependencies.Count-1 do'#010+
  '          begin'#010+
  '            D:=Dependencies[i];'#010+
  '            S:='#039#039';'#010+
  '            if (D.OSes<>AllOSes) then'#010+
  '              S:=S+'#039' os="'#039'+OSesToString(D.OSes)+'#039'"'#039';'#010+
  '            if (D.CPUs<>AllCPUs) then'#010+
  ' ','             S:=S+'#039' cpu="'#039'+CPUsToString(D.CPUs)+'#039'"'#039';'+
  #010+
  '            Add(Format('#039'<dependency><package%s packagename="%s"/><'+
  '/dependency>'#039',[S,QuoteXML(D.Value)]));'#010+
  '          end;'#010+
  '        Add('#039'</dependencies>'#039');'#010+
  '      end;'#010+
  '    Add('#039'</package>'#039');'#010+
  ' ','   end;'#010+
  'end;'#010+
  #010+
  #010+
  'function TPackage.AddTarget(AName: String): TTarget;'#010+
  'begin'#010+
  '  Result:=Targets.Add as TTarget;'#010+
  '  Result.Name:=AName;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.AddDependency(AName: String);'#010+
  'begin'#010+
  '  FDependencies.Add(AName);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPa','ckage.AddInstallFile(AFileName: String);'#010+
  'begin'#010+
  '  FInstallFiles.add(AFileName);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.GetCleanFiles(List: TStrings; Const APrefixU, APref'+
  'ixB : String; ACPU:TCPU; AOS : TOS);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  AddConditionalString','s(List,CleanFiles,ACPU,AOS,APrefixU);'#010+
  '  For I:=0 to FTargets.Count-1 do'#010+
  '    FTargets.TargetItems[I].GetCleanFiles(List, APrefixU, APrefixB, AC'+
  'PU, AOS);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.GetInstallFiles(List: TStrings;Types : TTargetTypes'+
  ';Const APre','fix, APrefixU, APrefixB: String; ACPU:TCPU; AOS : TOS);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  T : TTarget;'#010+
  'begin'#010+
  '  AddConditionalStrings(List,InstallFiles,ACPU,AOS,APrefix);'#010+
  '  For I:=0 to FTargets.Count-1 do'#010+
  '    begin'#010+
  '      T:=FTargets.TargetItems[I];'#010+
  '     ',' if (T.TargetType in Types)  then'#010+
  '        T.GetInstallFiles(List, APrefixU, APrefixB, ACPU, AOS);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.GetArchiveFiles(List: TStrings; ACPU:TCPU; AOS : TO'+
  'S);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  // Targets only'#010+
  '  For I:=','0 to FTargets.Count-1 do'#010+
  '    FTargets.TargetItems[I].GetArchiveFiles(List,ACPU,AOS);'#010+
  '  // Additional archive files'#010+
  '  AddConditionalStrings(List,ArchiveFiles,ACPU,AOS);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TPackage.GetDescription : string;'#010+
  'Var'#010+
  '  FN : String;'#010+
  '  L',' : TStringList;'#010+
  'begin'#010+
  '  If (FDescription<>'#039#039') then'#010+
  '    Result:=FDescription'#010+
  '  else'#010+
  '    If (FDescriptionFile<>'#039#039') then'#010+
  '      begin'#010+
  '        // Always relative to binary name.'#010+
  '        FN:=ExtractFilePath(ParamStr(0));'#010+
  '        FN:=FN+FDescripti','onFile;'#010+
  '        If FileExists(FN) then'#010+
  '          begin'#010+
  '            L:=TStringList.Create;'#010+
  '            Try'#010+
  '              L.LoadFromFile(FN);'#010+
  '              Result:=L.Text;'#010+
  '            Finally'#010+
  '              L.Free;'#010+
  '            end;'#010+
  '          e','nd;'#010+
  '      end;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TPackage.GetFileName : string;'#010+
  'begin'#010+
  '  If (FFileName<>'#039#039') then'#010+
  '    Result:=FFileName'#010+
  '  else'#010+
  '    if Version <> '#039#039' then'#010+
  '      Result := Name + '#039'-'#039' + Version'#010+
  '    else'#010+
  '      Result := Name;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPacka','ge.AddDocFiles(const AFileMask: string; Recursive: b'+
  'oolean);'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, Recursive, List);'#010+
  #010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '    Sources.AddDocFiles(Lis','t[i]);'#010+
  #010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.AddSrcFiles(const AFileMask: string; Recursive: boo'+
  'lean);'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, Recursive, List);'#010+
  #010+
  '  for i:= 0 to Pre','d(List.Count) do'#010+
  '    Sources.AddSrcFiles(List[i]);'#010+
  #010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.AddExampleFiles(const AFileMask: string; Recursive:'+
  ' boolean);'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(','AFileMask, Recursive, List);'#010+
  #010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '    Sources.AddExampleFiles(List[i]);'#010+
  #010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.AddTestFiles(const AFileMask: string; Recursive: bo'+
  'olean);'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'b','egin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, Recursive, List);'#010+
  #010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '    Sources.AddTestFiles(List[i]);'#010+
  #010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.LoadUnitConfigFromFile(Const AFileName: String);'#010+
  'Va','r'#010+
  '  F : TFileStream;'#010+
  'begin'#010+
  '  F:=TFileStream.Create(AFileName,fmOpenRead);'#010+
  '  Try'#010+
  '    LoadUnitConfigFromStream(F);'#010+
  '  Finally'#010+
  '    F.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.SaveUnitConfigToFile(Const AFileName: String;ACPU:T'+
  'CPU;AOS:TOS);'#010+
  'Var'#010+
  '  F',' : TFileStream;'#010+
  'begin'#010+
  '  F:=TFileStream.Create(AFileName,fmCreate);'#010+
  '  Try'#010+
  '    SaveUnitConfigToStream(F,ACPU,AOS);'#010+
  '  Finally'#010+
  '    F.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.SaveUnitConfigToStream(S : TStream;ACPU:TCPU;AOS:TO'+
  'S);'#010+
  'Var'#010+
  '  L : TString','List;'#010+
  '  Deps : String;'#010+
  '  i : integer;'#010+
  '  D : TDependency;'#010+
  'begin'#010+
  '  L:=TStringList.Create;'#010+
  '  try'#010+
  '    With L do'#010+
  '      begin'#010+
  '        Values[KeyName]:=Name;'#010+
  '        Values[KeyVersion]:=Version;'#010+
  '        Values[KeyCPU]:=CPUToString(ACPU);'#010+
  '        V','alues[KeyOS]:=OSToString(AOS);'#010+
  '        Deps:='#039#039';'#010+
  '        for i:=0 to Dependencies.Count-1 do'#010+
  '          begin'#010+
  '            D:=Dependencies[i];'#010+
  '            if (ACPU in D.CPUs) and (AOS in D.OSes) then'#010+
  '              begin'#010+
  '                if Dep','s='#039#039' then'#010+
  '                  Deps:=D.Value'#010+
  '                else'#010+
  '                  Deps:=Deps+'#039','#039'+D.Value;'#010+
  '              end;'#010+
  '          end;'#010+
  '        Values[KeyDepends]:=Deps;'#010+
  '        if NeedLibC then'#010+
  '          Values[KeyNeedLibC]:='#039'Y'#039#010+
  '       ',' else'#010+
  '          Values[KeyNeedLibC]:='#039'N'#039';'#010+
  '      end;'#010+
  '    L.SaveToStream(S);'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.LoadUnitConfigFromStream(S: TStream);'#010+
  'Var'#010+
  '  L,L2 : TStrings;'#010+
  '  Line : String;'#010+
  '  I,P,PC : Integer;'#010+
  '  VOS : TOS',';'#010+
  '  VCPU : TCPU;'#010+
  'begin'#010+
  '  L:=TStringList.Create;'#010+
  '  Try'#010+
  '    L.LoadFromStream(S);'#010+
  '    // Fix lines.'#010+
  '    For I:=L.Count-1 downto 0 do'#010+
  '      begin'#010+
  '        Line:=L[I];'#010+
  '        P:=Pos('#039'='#039',Line);'#010+
  '        PC:=Pos('#039';'#039',Line);  // Comment line.'#010+
  '       ',' If (P=0) or ((PC<>0) and (PC<P)) then'#010+
  '          L.Delete(I)'#010+
  '        else'#010+
  '          L[i]:=Trim(System.Copy(Line,1,P-1)+'#039'='#039'+Trim(System.C'+
  'opy(Line,P+1,Length(Line)-P)));'#010+
  '      end;'#010+
  '    With L do'#010+
  '      begin'#010+
  '        Version:=Values[KeyVersion]',';'#010+
  '        VCPU:=StringToCPU(Values[KeyCPU]);'#010+
  '        VOS:=StringToOS(Values[KeyOS]);'#010+
  '        OSes:=[VOS];'#010+
  '        CPUs:=[VCPU];'#010+
  '        L2:=TStringList.Create;'#010+
  '        L2.CommaText:=Values[KeyDepends];'#010+
  '        for i:=0 to L2.Count-1 do'#010+
  '    ','      Dependencies.Add(L2[i],CPUs,OSes);'#010+
  '        FreeAndNil(L2);'#010+
  '        NeedLibC:=Upcase(Values[KeyNeedLibC])='#039'Y'#039';'#010+
  '      end;'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******',#010+
  '                              TPackages'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'function TPackages.GetPackage(const AName : String): TPackage;'#010+
  'begin'#010+
  '  Result:=TPackage(ItemByName(AName))'#010+
  'end;'#010+
  #010+
  'functio','n TPackages.GetPackageItem(AIndex : Integer): TPackage;'#010+
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
  'function TPackages.A','ddPackage(const AName: String): TPackage;'#010+
  'begin'#010+
  '  Result:=Add as TPackage;'#010+
  '  Result.Name:=AName;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                             TCustomDefaults'#010+
  '*************','*******************************************************'+
  '********}'#010+
  #010+
  'procedure TCustomDefaults.SetCPU(const AValue: TCPU);'#010+
  'begin'#010+
  '  FCPU:=AValue;'#010+
  '  RecalcTarget;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetBaseInstallDir: String;'#010+
  'begin'#010+
  '  If (FBaseInstal','lDir<>'#039#039') then'#010+
  '    Result:=FBaseInstallDir'#010+
  '  else'#010+
  '    if UnixPaths then'#010+
  '      Result:=Prefix +'#039'lib'#039' + PathDelim + '#039'fpc'#039' + Path'+
  'Delim'#010+
  '    else'#010+
  '      Result:=Prefix;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetBinInstallDir: String;'#010+
  'begin'#010+
  '  If (FBinIns','tallDir<>'#039#039') then'#010+
  '    Result:=FBinInstallDir'#010+
  '  else'#010+
  '    If UnixPaths then'#010+
  '      Result:=BaseInstallDir+'#039'bin'#039#010+
  '    else'#010+
  '      Result:=BaseInstallDir+'#039'bin'#039';'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetCompiler: String;'#010+
  'begin'#010+
  '  If (FCompiler<>'#039#039') then'#010+
  ' ','   Result:=FCompiler'#010+
  '  else'#010+
  '    Result:='#039'fpc'#039';'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetDocInstallDir: String;'#010+
  'begin'#010+
  '  If (FBinInstallDir<>'#039#039') then'#010+
  '    Result:=FBinInstallDir'#010+
  '  else'#010+
  '    If UnixPaths then'#010+
  '      Result:=Prefix+'#039'share'#039'+PathDelim+'#039'do','c'#039#010+
  '    else'#010+
  '      Result:=BaseInstallDir+'#039'docs'#039';'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetExamplesInstallDir: String;'#010+
  'begin'#010+
  '  If (FExamplesInstallDir<>'#039#039') then'#010+
  '    Result:=FExamplesInstallDir'#010+
  '  else'#010+
  '    If UnixPaths then'#010+
  '      Result:=Prefix+'#039'sha','re'#039'+PathDelim+'#039'docs'#039'+PathDe'+
  'lim+'#039'examples'#039#010+
  '    else'#010+
  '      Result:=BaseInstallDir+'#039'examples'#039';'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetUnitInstallDir: String;'#010+
  'begin'#010+
  '  If (FUnitInstallDir<>'#039#039') then'#010+
  '    Result:=FUnitInstallDir'#010+
  '  else'#010+
  '    If UnixPaths',' then'#010+
  '      Result:=BaseInstallDir+'#039'units'#039'+PathDelim+Target'#010+
  '    else'#010+
  '      Result:=BaseInstallDir+'#039'units'#039'+PathDelim+Target;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefaults.GetLocalUnitDir: String;'#010+
  'begin'#010+
  '  Result:=FLocalUnitDir;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomDefau','lts.GetGlobalUnitDir: String;'#010+
  'begin'#010+
  '  If (FGlobalUnitDir<>'#039#039') then'#010+
  '    Result:=FGlobalUnitDir'#010+
  '  else'#010+
  '    Result:=UnitInstallDir;'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.SetLocalUnitDir(const AValue: String);'#010+
  'begin'#010+
  '  // Use ExpandFileName to support',' ~/ expansion'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FLocalUnitDir:=IncludeTrailingPathDelimiter(ExpandFileName(AValue)'+
  ')'#010+
  '  else'#010+
  '    FLocalUnitDir:='#039#039';'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.SetGlobalUnitDir(const AValue: String);'#010+
  'begin'#010+
  '  // Use ExpandFileName t','o support ~/ expansion'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FGlobalUnitDir:=IncludeTrailingPathDelimiter(ExpandFileName(AValue'+
  '))'#010+
  '  else'#010+
  '    FGlobalUnitDir:='#039#039';'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetBaseInstallDir(const AValue: String);'#010+
  'begin'#010+
  '  // Use Exp','andFileName to support ~/ expansion'#010+
  '  if AValue<>'#039#039' then'#010+
  '    FBaseInstallDir:=IncludeTrailingPathDelimiter(ExpandFileName(AValu'+
  'e))'#010+
  '  else'#010+
  '    FBaseInstallDir:='#039#039';'#010+
  '  UnitInstallDir:='#039#039';'#010+
  '  BinInstallDir:='#039#039';'#010+
  '  ExamplesInstallDir:='#039#039';'#010+
  'end;'#010+
  #010+
  #010+
  'p','rocedure TCustomDefaults.SetOS(const AValue: TOS);'#010+
  'begin'#010+
  '  FOS:=AValue;'#010+
  '  Recalctarget;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SetPrefix(const AValue: String);'#010+
  'begin'#010+
  '  if FPrefix=AValue then exit;'#010+
  '  FPrefix:=IncludeTrailingPathDelimiter(AValue);'#010,
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
  '        begin'#010+
  '          FOS:=StringToOS(Syst','em.Copy(Avalue,P+1,Length(AValue)-P));'+
  #010+
  '          FCPU:=StringToCPU(System.Copy(Avalue,1,P-1));'#010+
  '        end'#010+
  '      else'#010+
  '        FOS:=StringToOS(AValue);'#010+
  '      FTarget:=AValue;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.RecalcTarget;'#010+
  'begin'#010+
  '  Ft','arget:=CPUToString(FCPU)+'#039'-'#039'+OStoString(FOS);'#010+
  'end;'#010+
  #010+
  #010+
  'constructor TCustomDefaults.Create;'#010+
  'begin'#010+
  '  InitDefaults;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.InitDefaults;'#010+
  'begin'#010+
  '  {$ifdef unix}'#010+
  '  UnixPaths:=True;'#010+
  '  {$else}'#010+
  '  UnixPaths:=False;'#010+
  '  {$endif}'#010,
  '  FSourceExt:=PPExt;'#010+
  '  FNoFPCCfg:=False;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.Assign(ASource: TPersistent);'#010+
  'Var'#010+
  '  d : TCustomDefaults;'#010+
  'begin'#010+
  '  If ASource is TCustomDefaults then'#010+
  '    begin'#010+
  '      D:=ASource as TCustomDefaults;'#010+
  '      FArchive:=D.F','archive;'#010+
  '      FCompiler:=D.Compiler;'#010+
  '      FCopy:=D.FCopy;'#010+
  '      FCPU:=D.FCPU;'#010+
  '      FMode:=D.FMode;'#010+
  '      FMkDir:=D.FMkDir;'#010+
  '      FMove:=D.FMove;'#010+
  '      FOptions:=D.FOptions;'#010+
  '      FOS:=D.FOS;'#010+
  '      FLocalUnitDir:=D.FLocalUnitDir;'#010+
  '      FG','lobalUnitDir:=D.FGlobalUnitDir;'#010+
  '      FPrefix:=D.FPrefix;'#010+
  '      FBaseInstallDir:=D.FBaseInstallDir;'#010+
  '      FUnitInstallDir:=D.FUnitInstallDir;'#010+
  '      FBinInstallDir:=D.FBinInstallDir;'#010+
  '      FDocInstallDir:=D.FDocInstallDir;'#010+
  '      FExamplesIns','tallDir:=D.FExamplesInstallDir;'#010+
  '      FRemove:=D.FRemove;'#010+
  '      FTarget:=D.FTarget;'#010+
  '      FUnixPaths:=D.FUnixPaths;'#010+
  '      FSourceExt:=D.SourceExt;'#010+
  '    end'#010+
  '  else'#010+
  '    Inherited;'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.LocalInit(Const AFileName : Str','ing);'#010+
  #010+
  'Var'#010+
  '  FN : String;'#010+
  #010+
  'begin'#010+
  '  FN:=AFileName;'#010+
  '  If (FN='#039#039') then'#010+
  '    begin'#010+
  '    // Environment variable.'#010+
  '    FN:=GetEnvironmentVariable('#039'FPMAKECFG'#039');'#010+
  '    If (FN<>'#039#039') then'#010+
  '      If not FileExists(FN) then'#010+
  '        FN:='#039#039';'#010+
  '    // User config',' file fpmake.cfg'#010+
  '    If (FN='#039#039') then'#010+
  '      begin'#010+
  '      FN:=GetAppConfigFile(False);'#010+
  '      If Not FileExists(FN) then'#010+
  '        FN:='#039#039';'#010+
  '      end;'#010+
  '    // Global config file fpmake.cfg'#010+
  '    If (FN='#039#039') then'#010+
  '      begin'#010+
  '      FN:=GetAppConfigFile(','True);'#010+
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
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  'var'#010+
  '  infoSL : TStringList;',#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  'begin'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '  // Detect compiler version/target from -i option'#010+
  '  infosl:=TStringList.Create;'#010+
  '  infosl.Delimiter:='#039' '#039';'#010+
  '  infosl.DelimitedText:=GetCompilerInfo(GetCompiler,'#039'-iVTPTO'#039')'+
  ';'#010+
  '  if infosl','.Count<>3 then'#010+
  '    Raise EInstallerError.Create(SErrInvalidFPCInfo);'#010+
  '  FCompilerVersion:=infosl[0];'#010+
  '  CPU:=StringToCPU(infosl[1]);'#010+
  '  OS:=StringToOS(infosl[2]);'#010+
  '{$else HAS_UNIT_PROCESS}'#010+
  '  if CPU=cpuNone then'#010+
  '    CPU:=StringToCPU({$I %FPCTARG','ETCPU%});'#010+
  '  if OS=osNone then'#010+
  '    OS:=StringToOS({$I %FPCTARGETOS%});'#010+
  '  if FCompilerVersion='#039#039' then'#010+
  '    FCompilerVersion:={$I %FPCVERSION%};'#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.LoadFromFile(Const AFileName: String);'#010+
  'V','ar'#010+
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
  'procedure TCustomDefaults.SaveToFile(Const AFileName: String);'#010+
  'Var'#010+
  '  F : TFileStream;'#010+
  'begin'#010+
  '  F:=TFi','leStream.Create(AFileName,fmCreate);'#010+
  '  Try'#010+
  '    SaveToStream(F);'#010+
  '  Finally'#010+
  '    F.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomDefaults.SaveToStream(S : TStream);'#010+
  'Var'#010+
  '  L : TStringList;'#010+
  'begin'#010+
  '  L:=TStringList.Create;'#010+
  '  try'#010+
  '    With L do'#010+
  '      begin'#010+
  ' ','     Values[KeyArchive]:=FArchive;'#010+
  '      Values[KeyCompiler]:=FCompiler;'#010+
  '      Values[KeyCopy]:=FCopy;'#010+
  '      Values[KeyMkDir]:=FMkDir;'#010+
  '      Values[KeyMove]:=FMove;'#010+
  '      Values[KeyOptions]:=FOptions;'#010+
  '      Values[KeyCPU]:=CPUToString(FCPU)',';'#010+
  '      Values[KeyOS]:=OSToString(FOS);'#010+
  '      Values[KeyMode]:=ModeToString(FMode);'#010+
  '      Values[KeyLocalUnitDir]:=FLocalUnitDir;'#010+
  '      Values[KeyGlobalUnitDir]:=FGlobalUnitDir;'#010+
  '      Values[KeyPrefix]:=FPrefix;'#010+
  '      Values[KeyBaseInstallD','ir]:=FBaseInstallDir;'#010+
  '      Values[KeyUnitInstallDir]:=FUnitInstallDir;'#010+
  '      Values[KeyBinInstallDir]:=FBinInstallDir;'#010+
  '      Values[KeyDocInstallDir]:=FDocInstallDir;'#010+
  '      Values[KeyExamplesInstallDir]:=FExamplesInstallDir;'#010+
  '      Values[K','eyRemove]:=FRemove;'#010+
  '      Values[KeyTarget]:=FTarget;'#010+
  '      Values[KeySourceExt]:=FSourceExt;'#010+
  '      if FNoFPCCfg then'#010+
  '        Values[KeyNoFPCCfg]:='#039'Y'#039';'#010+
  '      end;'#010+
  '    L.SaveToStream(S);'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomD','efaults.LoadFromStream(S: TStream);'#010+
  'Var'#010+
  '  L : TStrings;'#010+
  '  Line : String;'#010+
  '  I,P,PC : Integer;'#010+
  'begin'#010+
  '  L:=TStringList.Create;'#010+
  '  Try'#010+
  '    L.LoadFromStream(S);'#010+
  '    // Fix lines.'#010+
  '    For I:=L.Count-1 downto 0 do'#010+
  '      begin'#010+
  '      Line:=L[I];'#010+
  '    ','  P:=Pos('#039'='#039',Line);'#010+
  '      PC:=Pos('#039';'#039',Line);  // Comment line.'#010+
  '      If (P=0) or ((PC<>0) and (PC<P)) then'#010+
  '        L.Delete(I)'#010+
  '      else'#010+
  '        L[i]:=Trim(System.Copy(Line,1,P-1)+'#039'='#039'+Trim(System.Cop'+
  'y(Line,P+1,Length(Line)-P)));'#010+
  '      end;',#010+
  '    With L do'#010+
  '      begin'#010+
  '      FArchive:=Values[KeyArchive];'#010+
  '      FCompiler:=Values[KeyCompiler];'#010+
  '      FCopy:=Values[KeyCopy];'#010+
  '      FMkDir:=Values[KeyMkDir];'#010+
  '      FMove:=Values[KeyMove];'#010+
  '      FRemove:=Values[KeyRemove];'#010+
  '      FOption','s:=Values[KeyOptions];'#010+
  '      Line:=Values[KeyCPU];'#010+
  '      If (Line<>'#039#039') then'#010+
  '        FCPU:=StringToCPU(Line);'#010+
  '      Line:=Values[KeyOS];'#010+
  '      If (Line<>'#039#039') then'#010+
  '        FOS:=StringToOS(Line);'#010+
  '      Line:=Values[KeyMode];'#010+
  '      If (Line<>'#039#039')',' then'#010+
  '        FMode:=StringToMode(Line);'#010+
  '      FTarget:=Values[KeyTarget];'#010+
  '      FLocalUnitDir:=Values[KeyLocalUnitDir];'#010+
  '      FGlobalUnitDir:=Values[KeyGlobalUnitDir];'#010+
  '      FPrefix:=Values[KeyPrefix];'#010+
  '      FBaseInstallDir:=Values[KeyBase','InstallDir];'#010+
  '      FUnitInstallDir:=Values[KeyUnitInstallDir];'#010+
  '      FBinInstallDir:=Values[KeyBinInstallDir];'#010+
  '      FDocInstallDir:=Values[KeyDocInstallDir];'#010+
  '      FExamplesInstallDir:=Values[KeyExamplesInstallDir];'#010+
  '      FSourceExt:=Value','s[KeySourceExt];'#010+
  '      If (FSourceExt='#039#039') then'#010+
  '        FSourceExt:=PPExt;'#010+
  '      FNoFPCCfg:=(Upcase(Values[KeyNoFPCCfg])='#039'Y'#039');'#010+
  '      end;'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  '{******************************************************************','*'+
  '*********'#010+
  '                             TFPCDefaults'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'procedure TFPCDefaults.CompilerDefaults;'#010+
  'var'#010+
  '  BD : String;'#010+
  'begin'#010+
  '  inherited CompilerDefaults;'#010+
  #010+
  '  // Use th','e same algorithm as the compiler, see options.pas'#010+
  '{$ifdef Unix}'#010+
  '  BD:=FixPath(GetEnvironmentVariable('#039'FPCDIR'#039'));'#010+
  '  if BD='#039#039' then'#010+
  '    begin'#010+
  '      BD:='#039'/usr/local/lib/fpc/'#039'+FCompilerVersion;'#010+
  '      if not DirectoryExists(BD) and'#010+
  '         Direc','toryExists('#039'/usr/lib/fpc/'#039'+FCompilerVersion) t'+
  'hen'#010+
  '        BD:='#039'/usr/lib/fpc/'#039'+FCompilerVersion;'#010+
  '    end;'#010+
  '{$else unix}'#010+
  '  BD:=FixPath(GetEnvironmentVariable('#039'FPCDIR'#039'));'#010+
  '  if BD='#039#039' then'#010+
  '    begin'#010+
  '      BD:=ExtractFilePath(FCompiler)+'#039'..'#039';'#010+
  '    ','  if not(DirectoryExists(BD+'#039'/units'#039')) and'#010+
  '         not(DirectoryExists(BD+'#039'/rtl'#039')) then'#010+
  '        BD:=FBaseInstallDir+'#039'..'#039';'#010+
  '    end;'#010+
  '{$endif unix}'#010+
  #010+
  '  // Where to install by default'#010+
  '  if (FBaseInstallDir='#039#039') and (FPrefix='#039#039') then'#010+
  '    BaseInst','allDir:=BD;'#010+
  #010+
  '  // Where to find the units by default'#010+
  '  if (FGlobalUnitDir='#039#039') then'#010+
  '    GlobalUnitDir:=IncludeTrailingPathDelimiter(BD)+'#039'units'#039'+Pa'+
  'thDelim+Target;'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '**','*****'#010+
  '                            TCustomInstaller'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TCustomInstaller.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  Dictionary:=DictionaryClass.Create(Nil);'#010+
  '  An','alyzeOptions;'#010+
  '  CreatePackages;'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TCustomInstaller.Destroy;'#010+
  'begin'#010+
  '  FreeAndNil(Defaults);'#010+
  '  FreeAndNil(Dictionary);'#010+
  '  inherited destroy;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomInstaller.GetConditionalStrings(AIndex : Integer): TCo'+
  'nditionalS','trings;'#010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  Case AIndex of'#010+
  '    0:  Result:=DefaultPackage.Dependencies;'#010+
  '    1:  Result:=DefaultPackage.InstallFiles;'#010+
  '    2:  Result:=DefaultPackage.CleanFiles;'#010+
  '    3:  Result:=DefaultPackage.ArchiveFiles;'#010+
  '  end;'#010+
  'e','nd;'#010+
  #010+
  #010+
  'function TCustomInstaller.GetBaseInstallDir: string;'#010+
  'begin'#010+
  '  Result := Defaults.BaseInstallDir;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TCustomInstaller.GetPackageString(Index : Integer) : String;'#010+
  'Var'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  P:=Default','Package;'#010+
  '  Case Index of'#010+
  '    0 : Result:=P.Author;'#010+
  '    1 : Result:=P.Directory;'#010+
  '    2 : Result:=P.License;'#010+
  '    3 : Result:=P.Options;'#010+
  '    4 : Result:=P.ExternalURL;'#010+
  '    5 : Result:=P.Email;'#010+
  '    6 : Result:=P.Description;'#010+
  '    7 : Result:=P.D','escriptionFile;'#010+
  '    8 : Result:=P.Version;'#010+
  '    9 : Result:=P.FileName + ZipExt;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure TCustomInstaller.SetPackageString(Index : Integer; const AVa'+
  'lue : String);'#010+
  'Var'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  P:=Default','Package;'#010+
  '  Case Index of'#010+
  '    0 : P.Author:=AValue;'#010+
  '    1 : P.Directory:=AValue;'#010+
  '    2 : P.License:=AValue;'#010+
  '    3 : P.Options:=AValue;'#010+
  '    4 : P.ExternalURL:=AValue;'#010+
  '    5 : P.Email:=AValue;'#010+
  '    6 : P.Description:=AValue;'#010+
  '    7 : P.Descripti','onFile:=AValue;'#010+
  '    8 : P.Version:=AValue;'#010+
  '    9 : P.FileName:=AValue;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomInstaller.GetOSes: TOSes;'#010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  Result:=DefaultPackage.OSes;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomInstaller.GetTargets: TTargets',';'#010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  Result:=DefaultPackage.Targets;'#010+
  'end;'#010+
  #010+
  'function TCustomInstaller.GetSources: TSources;'#010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  Result:=DefaultPackage.Sources;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.SetBaseInstallDir(const A','Value: string);'#010+
  'begin'#010+
  '  if AValue <> Defaults.BaseInstallDir then'#010+
  '    Defaults.BaseInstallDir := AValue;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.SetDefaultPackage(const AValue: TPackage);'#010+
  'begin'#010+
  '  if FDefaultPackage=AValue then exit;'#010+
  '  FDefaultPack','age:=AValue;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.SetConditionalStrings(AIndex : Integer; con'+
  'st AValue: TConditionalStrings);'#010+
  #010+
  'Var'#010+
  '  Res : TConditionalStrings;'#010+
  #010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  Case AIndex of'#010+
  '    0:  Res:=DefaultPackage.Dependenc','ies;'#010+
  '    1:  Res:=DefaultPackage.InstallFiles;'#010+
  '    2:  Res:=DefaultPackage.CleanFiles;'#010+
  '    3:  Res:=DefaultPackage.ArchiveFiles;'#010+
  '  end;'#010+
  '  Res.Assign(Avalue);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.SetOSes(const AValue: TOSes);'#010+
  'begin'#010+
  '  CheckDefaul','tPackage;'#010+
  '  DefaultPackage.OSes:=AValue;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Log(Level: TVerboseLevel; const Msg: String'+
  ');'#010+
  'begin'#010+
  '  If Level in FLogLevels then'#010+
  '    Writeln(StdErr,Msg);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.CreatePackages;'#010+
  'begin'#010+
  '  ','FPAckages:=TPackages.Create(TPackage);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.CreateBuildEngine;'#010+
  'begin'#010+
  '  FBuildEngine:=TBuildEngine.Create(Self);'#010+
  '//  FBuildEngine.Defaults:=Defaults;'#010+
  '  FBuildEngine.ListMode:=FListMode;'#010+
  '  FBuildEngine.OnLog:=@Sel','f.Log;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.CheckDefaultPackage;'#010+
  'begin'#010+
  '  If (FDefaultPackage=Nil) then'#010+
  '    Raise EInstallerError.Create(SErrNoPackage);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Error(const Msg: String);'#010+
  'begin'#010+
  '  Raise EInstallerError.C','reate(Msg);'#010+
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
  'Function TCustomInstaller.StartPackage(const AName: String) : TPackage'+
  ';'#010+
  'begin'#010+
  '  FDefault','Package:=FPackages.AddPackage(AName);'#010+
  '  Result:=FDefaultPackage;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.EndPackage;'#010+
  'begin'#010+
  '  FDefaultPackage:=Nil;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.AnalyzeOptions;'#010+
  #010+
  '  Function CheckOption(Index : Integer;const Short',',Long : String): B'+
  'oolean;'#010+
  #010+
  '  var'#010+
  '    O : String;'#010+
  #010+
  '  begin'#010+
  '    O:=Paramstr(Index);'#010+
  '    Result:=(O='#039'-'#039'+short) or (O='#039'--'#039'+long) or (copy(O,'+
  '1,Length(Long)+3)=('#039'--'#039'+long+'#039'='#039'));'#010+
  '  end;'#010+
  #010+
  '  Function CheckCommand(Index : Integer;const Short,Long : S','tring): '+
  'Boolean;'#010+
  #010+
  '  var'#010+
  '    O : String;'#010+
  #010+
  '  begin'#010+
  '    O:=Paramstr(Index);'#010+
  '    Result:=(O='#039'-'#039'+short) or (O=long);'#010+
  '  end;'#010+
  #010+
  '  Function OptionArg(Var Index : Integer) : String;'#010+
  #010+
  '  Var'#010+
  '    P : Integer;'#010+
  #010+
  '  begin'#010+
  '    if (Length(ParamStr(Index))>1) ','and (Paramstr(Index)[2]<>'#039'-'#039+
  ') then'#010+
  '      begin'#010+
  '      If Index<ParamCount then'#010+
  '        begin'#010+
  '        Inc(Index);'#010+
  '        Result:=Paramstr(Index);'#010+
  '        end'#010+
  '      else'#010+
  '        Error(SErrNeedArgument,[Index,ParamStr(Index)]);'#010+
  '      end'#010+
  '    e','lse If length(ParamStr(Index))>2 then'#010+
  '      begin'#010+
  '      P:=Pos('#039'='#039',Paramstr(Index));'#010+
  '      If (P=0) then'#010+
  '        Error(SErrNeedArgument,[Index,ParamStr(Index)])'#010+
  '      else'#010+
  '        begin'#010+
  '        Result:=Paramstr(Index);'#010+
  '        Delete(Result',',1,P);'#010+
  '        end;'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  '  DefaultsFileName : string;'#010+
  #010+
  'begin'#010+
  '  I:=0;'#010+
  '  FListMode:=False;'#010+
  '  FLogLevels:=DefaultMessages;'#010+
  '  While (I<ParamCount) do'#010+
  '    begin'#010+
  '    Inc(I);'#010+
  '    if CheckOption(I,'#039'v'#039','#039'verbose'#039') the','n'#010+
  '      FLogLevels:=AllMessages'#010+
  '    else if CheckOption(I,'#039'd'#039','#039'debug'#039') then'#010+
  '      FLogLevels:=AllMessages+[vlDebug]'#010+
  '    else if CheckCommand(I,'#039'm'#039','#039'compile'#039') then'#010+
  '      FRunMode:=rmCompile'#010+
  '    else if CheckCommand(I,'#039'b'#039','#039'build'#039') then'#010+
  '      ','FRunMode:=rmBuild'#010+
  '    else if CheckCommand(I,'#039'i'#039','#039'install'#039') then'#010+
  '      FRunMode:=rmInstall'#010+
  '    else if CheckCommand(I,'#039'c'#039','#039'clean'#039') then'#010+
  '      FRunMode:=rmClean'#010+
  '    else if CheckCommand(I,'#039'a'#039','#039'archive'#039') then'#010+
  '      FRunMode:=rmarchive'#010+
  '    els','e if CheckCommand(I,'#039'M'#039','#039'manifest'#039') then'#010+
  '      FRunMode:=rmManifest'#010+
  '    else if CheckOption(I,'#039'h'#039','#039'help'#039') then'#010+
  '      Usage('#039#039',[])'#010+
  '    else if Checkoption(I,'#039'C'#039','#039'CPU'#039') then'#010+
  '      Defaults.CPU:=StringToCPU(OptionArg(I))'#010+
  '    else if Checkoptio','n(I,'#039'O'#039','#039'OS'#039') then'#010+
  '      Defaults.OS:=StringToOS(OptionArg(I))'#010+
  '    else if Checkoption(I,'#039't'#039','#039'target'#039') then'#010+
  '      Defaults.Target:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'l'#039','#039'list-commands'#039') then'#010+
  '      FListMode:=True'#010+
  '    else if Checkoptio','n(I,'#039'P'#039','#039'prefix'#039') then'#010+
  '      Defaults.Prefix:=OptionArg(I)'#010+
  '    else if Checkoption(I,'#039'n'#039','#039'nofpccfg'#039') then'#010+
  '      Defaults.NoFPCCfg:=true'#010+
  '    else if CheckOption(I,'#039'B'#039','#039'baseinstalldir'#039') then'#010+
  '      Defaults.BaseInstallDir:=OptionArg(I)'#010+
  '    els','e if CheckOption(I,'#039'UL'#039','#039'localunitdir'#039') then'#010+
  '      Defaults.LocalUnitDir:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'UG'#039','#039'globalunitdir'#039') then'#010+
  '      Defaults.GlobalUnitDir:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'r'#039','#039'compiler'#039') then'#010+
  '      Defa','ults.Compiler:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'f'#039','#039'config'#039') then'#010+
  '      DefaultsFileName:=OptionArg(I)'#010+
  '    else'#010+
  '      begin'#010+
  '      Usage(SErrInValidArgument,[I,ParamStr(I)]);'#010+
  '      end;'#010+
  '    end;'#010+
  '  If DefaultsFileName<>'#039#039' then'#010+
  '    Defa','ults.LocalInit(DefaultsFileName);'#010+
  '  Defaults.CompilerDefaults;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.Usage(const FMT: String; Args: array of con'+
  'st);'#010+
  #010+
  '  Procedure LogCmd(const LC,Msg : String);'#010+
  '  begin'#010+
  '    Log(vlInfo,Format('#039' %-12s %s'#039',[LC,MSG])',');'#010+
  '  end;'#010+
  #010+
  '  Procedure LogOption(const C,LC,Msg : String);'#010+
  '  begin'#010+
  '    Log(vlInfo,Format('#039' -%s --%-16s %s'#039',[C,LC,MSG]));'#010+
  '  end;'#010+
  #010+
  '  Procedure LogArgOption(const C,LC,Msg : String);'#010+
  '  begin'#010+
  '    Log(vlInfo,Format('#039' -%s --%-20s %s'#039',[C,LC+'#039'='#039'+SV','a'+
  'lue,MSG]));'#010+
  '  end;'#010+
  #010+
  'begin'#010+
  '  If (FMT<>'#039#039') then'#010+
  '    Log(vlInfo,Format(Fmt,Args));'#010+
  '  Log(vlInfo,Format(SHelpUsage,[Paramstr(0)]));'#010+
  '  Log(vlInfo,SHelpCommand);'#010+
  '  LogCmd('#039'compile'#039',SHelpCompile);'#010+
  '  LogCmd('#039'build'#039',SHelpBuild);'#010+
  '  LogCmd('#039'install'#039',','SHelpInstall);'#010+
  '  LogCmd('#039'clean'#039',SHelpClean);'#010+
  '  LogCmd('#039'archive'#039',SHelpArchive);'#010+
  '  LogCmd('#039'manifest'#039',SHelpManifest);'#010+
  '  Log(vlInfo,SHelpCmdOptions);'#010+
  '  LogOption('#039'h'#039','#039'help'#039',SHelpHelp);'#010+
  '  LogOption('#039'l'#039','#039'list-commands'#039',SHelpList);'#010+
  '  LogOption('#039'n'#039,','#039'nofpccfg'#039',SHelpNoFPCCfg);'#010+
  '  LogOption('#039'v'#039','#039'verbose'#039',SHelpVerbose);'#010+
  '  LogArgOption('#039'C'#039','#039'CPU'#039',SHelpCPU);'#010+
  '  LogArgOption('#039'O'#039','#039'OS'#039',SHelpOS);'#010+
  '  LogArgOption('#039't'#039','#039'target'#039',SHelpTarget);'#010+
  '  LogArgOption('#039'P'#039','#039'prefix'#039',SHelpPrefix);'#010+
  '  LogArgOption('#039'B',#039','#039'baseinstalldir'#039',SHelpBaseInstalldi'+
  'r);'#010+
  '  LogArgOption('#039'UL'#039','#039'localunitdir'#039',SHelpLocalUnitdir);'+
  #010+
  '  LogArgOption('#039'UG'#039','#039'globalunitdir'#039',SHelpGlobalUnitdir'+
  ');'#010+
  '  LogArgOption('#039'r'#039','#039'compiler'#039',SHelpCompiler);'#010+
  '  LogArgOption('#039'f'#039','#039'config'#039',SHelpConfig);',#010+
  '  Log(vlInfo,'#039#039');'#010+
  '  If (FMT<>'#039#039') then'#010+
  '    halt(1)'#010+
  '  else'#010+
  '    halt(0);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Compile(Force: Boolean);'#010+
  'begin'#010+
  '  FBuildEngine.ForceCompile:=Force;'#010+
  '  FBuildEngine.Compile(FPackages);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.C','lean;'#010+
  'begin'#010+
  '  BuildEngine.Clean(FPackages);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Install;'#010+
  'begin'#010+
  '  BuildEngine.Install(FPackages);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Archive;'#010+
  'begin'#010+
  '  // Force generation of manifest.xml, this is required for the re','po'+
  'sitory'#010+
  '  Manifest;'#010+
  '  FBuildEngine.Archive(FPackages);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Manifest;'#010+
  #010+
  'Var'#010+
  '  L : TStrings;'#010+
  'begin'#010+
  '  L:=TStringList.Create;'#010+
  '  Try'#010+
  '    Log(vlDebug, Format(SDbgGenerating, [ManifestFile]));'#010+
  '    L.Add('#039'<?xml version=','"1.0"?>'#039');'#010+
  '    BuildEngine.GetManifest(FPackages,L);'#010+
  '    L.SaveToFile(ManifestFile);'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TCustomInstaller.CheckPackages;'#010+
  'begin'#010+
  '  If (FPackages.Count=0) then'#010+
  '    Error(SErrNoPackagesDefined);'#010+
  '  // Ch','eck for other obvious errors ?'#010+
  'end;'#010+
  #010+
  #010+
  'Function TCustomInstaller.Run : Boolean;'#010+
  'begin'#010+
  '  Result:=True;'#010+
  '  try'#010+
  '    CheckPackages;'#010+
  '    CreateBuildEngine;'#010+
  '    Case RunMode of'#010+
  '      rmCompile : Compile(False);'#010+
  '      rmBuild   : Compile(True);'#010+
  '    ','  rmInstall : Install;'#010+
  '      rmArchive : Archive;'#010+
  '      rmClean    : Clean;'#010+
  '      rmManifest : Manifest;'#010+
  '    end;'#010+
  '  except'#010+
  '    On E : Exception do'#010+
  '      begin'#010+
  '      Log(vlError,SErrInstaller);'#010+
  '      Log(vlError,E.Message);'#010+
  '      Result:=Fal','se;'#010+
  '      end;'#010+
  '  end;'#010+
  '  // Force returning an exitcode to the shell'#010+
  '  if not Result then'#010+
  '    ExitCode:=1;'#010+
  'end;'#010+
  #010+
  'function TCustomInstaller.AddTarget(const AName: String): TTarget;'#010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  Result:=DefaultPackage.AddTarg','et(AName);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.AddDependency(const AName: String);'#010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  DefaultPackage.AddDependency(AName);'#010+
  'end;'#010+
  #010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '      ','                          TFPCInstaller'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'constructor TFPCInstaller.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  if assigned(Defaults) then'#010+
  '    Error(SErrAlreadyInitialized',');'#010+
  '  Defaults:=TFPCDefaults.Create;'#010+
  '  inherited Create(AOwner);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                 TBasicInstaller'#010+
  '******************************************','**************************'+
  '********}'#010+
  #010+
  'constructor TBasicInstaller.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  if assigned(Defaults) then'#010+
  '    Error(SErrAlreadyInitialized);'#010+
  '  Defaults:=TBasicDefaults.Create;'#010+
  '  inherited Create(AOwner);'#010+
  'end;'#010+
  #010+
  #010+
  '{******','*************************************************************'+
  '*********'#010+
  '                                 TBuildEngine'#010+
  '**********************************************************************'+
  '******}'#010+
  #010+
  'procedure TBuildEngine.SetTargetDir(const AV','alue: String);'#010+
  'begin'#010+
  '  if FTargetDir=AValue then exit;'#010+
  '  FTargetDir:=AValue;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Error(const Msg: String);'#010+
  'begin'#010+
  '  Raise EInstallerError.Create(Msg);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Error(const Fmt: String; Args: array',' of const'+
  ');'#010+
  'begin'#010+
  '  Raise EInstallerError.CreateFmt(Fmt,Args);'#010+
  'end;'#010+
  #010+
  'constructor TBuildEngine.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  inherited Create(AOwner);'#010+
  '  // Maybe this should be the current directory ?'#010+
  '  // Or have it as a command-line ','option.'#010+
  '  // Would allow to put all '#039'installers'#039' in one dir and call t'+
  'hem'#010+
  '  // With --start-dir=/path/to/sources.'#010+
  '  FStartDir:=includeTrailingPathDelimiter(GetCurrentDir);'#010+
  '  FExternalPackages:=TPackages.Create(TPackage);'#010+
  'end;'#010+
  #010+
  #010+
  'destructor T','BuildEngine.Destroy;'#010+
  'begin'#010+
  '  FreeAndNil(FExternalPackages);'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.ExecuteCommand(const Cmd,Args : String; IgnoreE'+
  'rror : Boolean = False);'#010+
  #010+
  'Var'#010+
  '  E : Integer;'#010+
  #010+
  'begin'#010+
  '  Log(vlInfo,SInfoExecutingCom','mand,[Cmd,Args]);'#010+
  '  // We should check cmd for spaces, and move all after first space to'+
  ' args.'#010+
  '  E:=ExecuteProcess(cmd,args);'#010+
  '  If (E<>0) and (not IgnoreError) then'#010+
  '    Error(SErrExternalCommandFailed,[Cmd,E]);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.','SysDirectoryExists(const ADir:string):Boolean;'+
  #010+
  'begin'#010+
  '  result:=SysUtils.DirectoryExists(ADir);'#010+
  '  if result then'#010+
  '    Log(vlDebug,SDbgDirectoryExists,[ADir,SDbgFound])'#010+
  '  else'#010+
  '    Log(vlDebug,SDbgDirectoryExists,[ADir,SDbgNotFound]);'#010+
  'end;'#010+
  #010+
  #010+
  'Fu','nction TBuildEngine.SysFileExists(const AFileName:string):Boolean;'+
  #010+
  'begin'#010+
  '  result:=SysUtils.FileExists(AFileName);'#010+
  '  if result then'#010+
  '    Log(vlDebug,SDbgFileExists,[AFileName,SDbgFound])'#010+
  '  else'#010+
  '    Log(vlDebug,SDbgFileExists,[AFileName,SDbgN','otFound]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.SysCopyFile(Const Src,Dest : String);'#010+
  'Var'#010+
  '  D,S : String;'#010+
  '  Fin,FOut : TFileStream;'#010+
  '  Count : Int64;'#010+
  '  A : Integer;'#010+
  'begin'#010+
  '  Log(vlInfo,SInfoCopyingFile,[Src,Dest]);'#010+
  '  FIn:=TFileStream.Create(Src,fmop','enRead);'#010+
  '  Try'#010+
  '    D:=IncludeTrailingPathDelimiter(Dest);'#010+
  '    If DirectoryExists(D) then'#010+
  '      S:=D+ExtractFileName(Src)'#010+
  '    else'#010+
  '      S:=Dest;'#010+
  '    FOut:=TFileStream.Create(S,fmCreate);'#010+
  '    Try'#010+
  '      Count:=Fout.CopyFrom(FIn,0);'#010+
  '      If (','Count<>Fin.Size) then'#010+
  '        Error(SErrCopyingFile,[Src,S]);'#010+
  '    Finally'#010+
  '      FreeAndNil(Fout);'#010+
  '    end;'#010+
  '    A:=FileGetDate(FIn.Handle);'#010+
  '    If (A=-1) then'#010+
  '      log(vlWarning,SWarnFailedToGetTime,[Src])'#010+
  '    else'#010+
  '      if FileSetDate(S,A)','<>0 then'#010+
  '        Log(vlWarning,SWarnFailedToSetTime,[S]);'#010+
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
  '    If DirectoryExists(IncludeTrailingPathDelimiter(','Dest)) then'#010+
  '      S:=IncludeTrailingPathDelimiter(Dest)+ExtractFileName(Src)'#010+
  '    else'#010+
  '      S:=Dest;'#010+
  '  If Not RenameFile(Src,S) then'#010+
  '    begin'#010+
  '    Try'#010+
  '      SysCopyFile(Src,S);'#010+
  '      SysDeleteFile(Src);'#010+
  '    Except'#010+
  '      On E : Exception Do'#010,
  '        Error(SErrMovingFile,[Src,S]);'#010+
  '    end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.SysDeleteFile(Const AFileName : String);'#010+
  'begin'#010+
  '  if not FileExists(AFileName) then'#010+
  '    Log(vlWarning,SWarnFileDoesNotExist,[AFileName])'#010+
  '  else If Not Del','eteFile(AFileName) then'#010+
  '    Error(SErrDeletingFile,[AFileName]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.SysArchiveFiles(List: TStrings;Const AFileName:'+
  ' String);'#010+
  'begin'#010+
  '  If Not (Assigned(OnArchivefiles) or Assigned(ArchiveFilesProc)) then'+
  #010+
  '    Raise E','InstallerError.Create(SErrNoArchiveSupport);'#010+
  '  If Assigned(ArchiveFilesProc) then'#010+
  '    ArchiveFilesProc(AFileName,List)'#010+
  '  else'#010+
  '    OnArchiveFiles(AFileName,List);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Log(Level: TVerboseLevel; const Msg: String);'#010+
  'be','gin'#010+
  '  If Assigned(FOnLog) then'#010+
  '    FOnLog(Level,Msg);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Log(Level: TVerboseLevel; const Fmt: String;'#010+
  '  Args: array of const);'#010+
  'begin'#010+
  '  Log(Level,Format(Fmt,Args));'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.EnterDir(ADir: String',');'#010+
  #010+
  'Var'#010+
  '  D : String;'#010+
  #010+
  'begin'#010+
  '  D:=FStartDir;'#010+
  '  D:=D+ADir;'#010+
  '  Log(vlInfo,SInfoEnterDir,[D]);'#010+
  '  If Not SetCurrentDir(D) then'#010+
  '    Error(SErrChangeDirFailed,[D]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CmdCopyFiles(List: TStrings; Const DestDir: Str'+
  'ing);',#010+
  #010+
  'Var'#010+
  '  Args : String;'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  CmdCreateDir(DestDir);'#010+
  '  If (Defaults.Copy<>'#039#039') then'#010+
  '    begin'#010+
  '    Args:=FileListToString(List,'#039#039');'#010+
  '    Args:=Args+'#039' '#039'+DestDir;'#010+
  '    ExecuteCommand(Defaults.Copy,Args);'#010+
  '    end'#010+
  '  else'#010+
  '    For I:','=0 to List.Count-1 do'#010+
  '      SysCopyFile(List[i],DestDir);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.CmdCreateDir(const DestDir: String);'#010+
  #010+
  'begin'#010+
  '  If (Defaults.MkDir<>'#039#039') then'#010+
  '    ExecuteCommand(Defaults.MkDir,DestDir)'#010+
  '  else'#010+
  '    If not ForceDirectories(','DestDir) then'#010+
  '      Error(SErrCreatingDirectory,[DestDir]);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.CmdMoveFiles(List: TStrings; Const DestDir: Str'+
  'ing);'#010+
  #010+
  'Var'#010+
  '  Args : String;'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  CmdCreateDir(DestDir);'#010+
  '  If (Defaults.Move<>'#039#039') then',#010+
  '    begin'#010+
  '    Args:=FileListToString(List,'#039#039');'#010+
  '    Args:=Args+'#039' '#039'+DestDir;'#010+
  '    ExecuteCommand(Defaults.Move,Args);'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      SysMoveFile(List[i],DestDir);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.CmdDeleteFil','es(List: TStrings);'#010+
  #010+
  'Var'#010+
  '  Args : String;'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  If (Defaults.Remove<>'#039#039') then'#010+
  '    begin'#010+
  '    Args:=FileListToString(List,'#039#039');'#010+
  '    ExecuteCommand(Defaults.Remove,Args);'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      Sy','sDeleteFile(List[i]);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.CmdArchiveFiles(List: TStrings; Const ArchiveFi'+
  'le: String);'#010+
  #010+
  'Var'#010+
  '  S,C,O : String;'#010+
  #010+
  'begin'#010+
  '  If (Defaults.Archive='#039#039') then'#010+
  '    SysArchiveFiles(List,ArchiveFile)'#010+
  '  else'#010+
  '    begin'#010+
  '    S:=FileL','istToString(List,'#039#039');'#010+
  '    SplitCommand(Defaults.Archive,C,O);'#010+
  '    If (O='#039#039') then'#010+
  '      O:=ArchiveFile+'#039' '#039'+S'#010+
  '    else'#010+
  '      O:=Substitute(O,['#039'ARCHIVE'#039',ArchiveFile,'#039'FILESORDIRS'#039+
  ']);'#010+
  '    ExecuteCommand(C,O);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'Function TBuildEngine','.FileNewer(const Src,Dest : String) : Boolean;'#010+
  #010+
  'Var'#010+
  '  DS,DD : Longint;'#010+
  '  D1,D2 : TDateTime;'#010+
  #010+
  'begin'#010+
  '  DS:=FileAge(Src);'#010+
  '  DD:=FileAge(Dest);'#010+
  '  D1:=FileDateToDateTime(DS);'#010+
  '  D2:=FileDateToDateTime(DD);'#010+
  '  Log(vlDebug,SDbgComparingFileTimes,[Sr','c,DateTimeToStr(D1),Dest,Dat'+
  'eTimeToStr(D2)]);'#010+
  '  Result:=D1>=D2;'#010+
  '  If Result then'#010+
  '    Log(vlInfo,SInfoSourceNewerDest,[Src,DateTimeToStr(D1),Dest,DateTi'+
  'meToStr(D2)]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.ExecuteCommands(Commands: TCommands; At: TCo','m'+
  'mandAt);'#010+
  'Var'#010+
  '  C : TCommand;'#010+
  '  I : Integer;'#010+
  '  Cmd,O : String;'#010+
  '  E : Boolean;'#010+
  'begin'#010+
  '  For I:=0 to Commands.Count-1 do'#010+
  '    begin'#010+
  '    C:=Commands.CommandItems[i];'#010+
  '    if (C.At=At) then'#010+
  '      begin'#010+
  '      E:=True;'#010+
  '      If (C.SourceFile<>'#039#039') an','d (C.DestFile<>'#039#039')  then'#010+
  '        E:=FileNewer(C.SourceFile,IncludeTrailingPathDelimiter(Diction'+
  'ary.GetValue('#039'OUTPUTDIR'#039'))+C.DestFile);'#010+
  '      If E then'#010+
  '        begin'#010+
  '        If Assigned(C.BeforeCommand) then'#010+
  '          C.BeforeCommand(C);'#010+
  '   ','     O:=Substitute(C.Options,['#039'SOURCE'#039',C.SourceFile,'#039'D'+
  'EST'#039',C.DestFile]);'#010+
  '        Cmd:=C.Command;'#010+
  '        If (ExtractFilePath(Cmd)='#039#039') then'#010+
  '          Cmd:=FileSearch(Cmd,GetEnvironmentvariable('#039'PATH'#039'));'+
  #010+
  '        ExecuteCommand(Cmd,O,C.IgnoreR','esult);'#010+
  '        If Assigned(C.AfterCommand) then'#010+
  '          C.AfterCommand(C);'#010+
  '        end;'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  '// Relative to startdir.'#010+
  'Function TBuildEngine.GetTargetDir(APackage : TPackage; ATarget : TTar'+
  'get; AbsolutePath : Boolean ','= False) : String;'#010+
  #010+
  'begin'#010+
  '  If AbsolutePath then'#010+
  '    Result:=IncludeTrailingPathDelimiter(FStartDir)'#010+
  '  else'#010+
  '    Result:='#039#039';'#010+
  '  If (APackage.Directory<>'#039#039') then'#010+
  '    Result:=Result+IncludeTrailingPathDelimiter(APackage.Directory);'#010+
  '  If (ATarge','t.Directory<>'#039#039') then'#010+
  '    Result:=IncludeTrailingPathDelimiter(Result+ATarget.Directory);'#010+
  #010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.FindFileInPath(Path:TConditionalStrings; AFileNa'+
  'me:String; var FoundPath:String;ACPU:TCPU;AOS:TOS; Const PathPrefix :S'+
  'tr','ing='#039#039'):Boolean;'#010+
  'var'#010+
  '  Prefix : String;'#010+
  '  I : Integer;'#010+
  '  C : TConditionalString;'#010+
  'begin'#010+
  '  Result:=false;'#010+
  '  if PathPrefix<>'#039#039' then'#010+
  '    Prefix:=IncludeTrailingPathDelimiter(PathPrefix)'#010+
  '  else'#010+
  '    Prefix:='#039#039';'#010+
  '  for i:=0 to Path.Count-1 do'#010+
  '    b','egin'#010+
  '      C:=Path[I];'#010+
  '      if (ACPU in C.CPUs) and (AOS in C.OSes) then'#010+
  '        begin'#010+
  '          FoundPath:=IncludeTrailingPathDelimiter(Dictionary.ReplaceSt'+
  'rings(Prefix+C.Value));'#010+
  '          if FileExists(FoundPath+AFileName) then'#010+
  '        ','    begin'#010+
  '              result:=true;'#010+
  '              exit;'#010+
  '            end;'#010+
  '        end;'#010+
  '    end;'#010+
  '  FoundPath:='#039#039';'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure TBuildEngine.ResolveFileNames(APackage : TPackage; ACPU:TCPU'+
  ';AOS:TOS);'#010+
  'var'#010+
  '  SD  : String;'#010+
  '  D   : TDependenc','y;'#010+
  '  Target : TTarget;'#010+
  '   i,j : Integer;'#010+
  'begin'#010+
  '  Dictionary.AddVariable('#039'CPU'#039',CPUToString(ACPU));'#010+
  '  Dictionary.AddVariable('#039'OS'#039',OSToString(AOS));'#010+
  '  For I:=0 to APackage.Targets.Count-1 do'#010+
  '    begin'#010+
  '      Target:=APackage.FTargets.TargetItem','s[I];'#010+
  #010+
  '      // Main source file'#010+
  '      SD:=Target.Directory;'#010+
  '      if SD='#039#039' then'#010+
  '        FindFileInPath(APackage.SourcePath,Target.SourceFileName,SD,AC'+
  'PU,AOS,APackage.Directory)'#010+
  '      else'#010+
  '        if APackage.Directory<>'#039#039' then'#010+
  '          SD',':=IncludeTrailingPathDelimiter(APackage.Directory)+SD;'#010+
  '      if SD<>'#039#039' then'#010+
  '        SD:=IncludeTrailingPathDelimiter(SD);'#010+
  '      Target.FFullSourceFileName:=SD+Target.SourceFileName;'#010+
  '      Log(vlDebug,SDbgResolvedSourceFile,[Target.SourceFil','eName,Targ'+
  'et.FFullSourceFileName]);'#010+
  #010+
  '      // Include files'#010+
  '      for j:=0 to Target.Dependencies.Count-1 do'#010+
  '        begin'#010+
  '          D:=Target.Dependencies[j];'#010+
  '          if (D.DependencyType=depInclude) and DependencyOK(D)  then'#010+
  '           ',' begin'#010+
  '              SD:=D.Directory;'#010+
  '              if SD='#039#039' then'#010+
  '                FindFileInPath(APackage.IncludePath,D.Value,SD,ACPU,AO'+
  'S,APackage.Directory)'#010+
  '              else'#010+
  '                if APackage.Directory<>'#039#039' then'#010+
  '                ','  SD:=IncludeTrailingPathDelimiter(APackage.Director'+
  'y)+SD;'#010+
  '               if SD<>'#039#039' then'#010+
  '                 SD:=IncludeTrailingPathDelimiter(SD);'#010+
  '               D.FFullFileName:=SD+D.Value;'#010+
  '               Log(vlDebug,SDbgResolvedIncludeFile,[','D.Value,D.FFullF'+
  'ileName]);'#010+
  '             end;'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.NeedsCompile(Target: TTarget): Boolean;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  T : TTarget;'#010+
  '  D : TDependency;'#010+
  '  OD,OFN : String;'#010+
  'begin'#010+
  '  Result:=False;'#010+
  '  case Ta','rget.FTargetState of'#010+
  '    tsNeedCompile :'#010+
  '      begin'#010+
  '        result:=true;'#010+
  '        exit;'#010+
  '      end;'#010+
  '    tsNoCompile,'#010+
  '    tsCompiled :'#010+
  '      exit;'#010+
  '  end;'#010+
  #010+
  '  Log(vlDebug, Format(SDbgConsideringTarget, [Target.Name]));'#010+
  #010+
  '  if Target.TargetType ','in ProgramTargets then'#010+
  '    OD:=GetBinOutputDir(FCurrentPackage, True)'#010+
  '  else'#010+
  '    OD:=GetUnitsOutputDir(FCurrentPackage, True);'#010+
  '  If (OD<>'#039#039') then'#010+
  '    OD:=IncludeTrailingPathDelimiter(OD);'#010+
  '  OFN:=OD+Target.GetOutPutFileName(Defaults.OS);'#010+
  #010+
  '  ','Result:=Not FileExists(OFN);'#010+
  '  if Result then'#010+
  '    Log(vlDebug,SDbgOutputNotYetAvailable,[OFN]);'#010+
  #010+
  '  // Check main source'#010+
  '  If not Result then'#010+
  '    begin'#010+
  '      if FileExists(Target.FullSourceFileName) then'#010+
  '        Result:=FileNewer(Target.Full','SourceFileName,OFN)'#010+
  '    end;'#010+
  #010+
  '  // Check unit and include dependencies'#010+
  '  If not Result then'#010+
  '    begin'#010+
  '      ResolveDependencies(Target.Dependencies,Target.Collection as TTa'+
  'rgets);'#010+
  '      I:=0;'#010+
  '      for i:=0 to Target.Dependencies.Count-1 do',#010+
  '        begin'#010+
  '          D:=Target.Dependencies[i];'#010+
  '          if (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'+
  #010+
  '            begin'#010+
  '              case D.DependencyType of'#010+
  '                depUnit :'#010+
  '                  begin'#010+
  '          ','          T:=TTarget(D.Target);'#010+
  '                    If (T=Nil) then'#010+
  '                      Error(SErrDepUnknownTarget,[Target.Name,D.Value]'+
  ');'#010+
  '                    // If a dependent package is compiled we always ne'+
  'ed to recompile'#010+
  '             ','       Log(vldebug, SDbgDependencyOnUnit, [Target.Name,'+
  'T.Name]);'#010+
  '                    Result:=(T.State in [tsNeedCompile,tsCompiled]) or'+
  ' NeedsCompile(T);'#010+
  '                    if Result then'#010+
  '                      Log(vldebug, SDbgDependencyUni','tRecompiled, [T.'+
  'Name]);'#010+
  '                  end;'#010+
  '                depInclude :'#010+
  '                  begin'#010+
  '                    if FileExists(D.FullFileName) then'#010+
  '                      Result:=FileNewer(D.FullFileName,OFN)'#010+
  '                  end;'#010+
  '  ','            end;'#010+
  '              if result then'#010+
  '                break;'#010+
  '            end;'#010+
  '        end;'#010+
  '    end;'#010+
  #010+
  '  // Upate also target state so a second check is faster'#010+
  '  if result then'#010+
  '    begin'#010+
  '      Target.FTargetState:=tsNeedCompile;'#010+
  '     ',' Log(vlDebug,SDbgMustCompile,[Target.Name]);'#010+
  '    end'#010+
  '  else'#010+
  '    Target.FTargetState:=tsNoCompile;'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.GetUnitDir(APackage:TPackage):String;'#010+
  'begin'#010+
  '  if (APackage.UnitDir='#039#039') and'#010+
  '     (Defaults.LocalUnitDir<>'#039#039') then'#010,
  '    begin'#010+
  '      APackage.UnitDir:=IncludeTrailingPathDelimiter(Defaults.LocalUni'+
  'tDir)+APackage.Name;'#010+
  '      if not SysDirectoryExists(APackage.UnitDir) then'#010+
  '        APackage.UnitDir:='#039#039';'#010+
  '    end;'#010+
  '  if APackage.UnitDir='#039#039' then'#010+
  '    begin'#010+
  '     ',' APackage.UnitDir:=IncludeTrailingPathDelimiter(Defaults.Global'+
  'UnitDir)+APackage.Name;'#010+
  '      if not SysDirectoryExists(APackage.UnitDir) then'#010+
  '        APackage.UnitDir:=DirNotFound;'#010+
  '    end;'#010+
  '  // Special error marker to prevent searches in c','ase of error'#010+
  '  if APackage.UnitDir=DirNotFound then'#010+
  '    Result:='#039#039#010+
  '  else'#010+
  '    Result:=APackage.UnitDir;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.AddDependencyUnitPaths(L:TStrings;APackage: TPa'+
  'ckage);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  D : TDependenc','y;'#010+
  '  S : String;'#010+
  'begin'#010+
  '  // Already processed?'#010+
  '  S:=GetUnitDir(APackage);'#010+
  '  if L.IndexOf(S)<>-1 then'#010+
  '    exit;'#010+
  '  // Add this package and then dependencies'#010+
  '  L.Add(S);'#010+
  '  For I:=0 to APackage.Dependencies.Count-1 do'#010+
  '    begin'#010+
  '      D:=APackag','e.Dependencies[i];'#010+
  '      if (D.DependencyType=depPackage) and'#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '        begin'#010+
  '          P:=TPackage(D.Target);'#010+
  '          If Assigned(P) then'#010+
  '            AddDependencyUnitPaths','(L,P);'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  #010+
  'Function TBuildEngine.GetCompilerCommand(APackage : TPackage; Target :'+
  ' TTarget) : String;'#010+
  #010+
  'Var'#010+
  '  PD,OD : String;'#010+
  '  L : TStringList;'#010+
  '  i : Integer;'#010+
  'begin'#010+
  '  PD:=GetPackageDir(APackage,True);'#010+
  #010+
  '  Result := '#039,#039';'#010+
  #010+
  '  //compiler configuration'#010+
  '  if Defaults.NoFPCCfg then'#010+
  '    Result := '#039'-n'#039';'#010+
  #010+
  '  // Compile mode'#010+
  '  If Target.Mode<>cmFPC then'#010+
  '    Result:=Result+'#039' -M'#039'+ModeToString(Target.Mode)'#010+
  '  else If Defaults.Mode<>cmFPC then'#010+
  '    Result:=Result+'#039' -M'#039'+M','odeToString(Defaults.Mode);'#010+
  '  // Output file paths'#010+
  '  If Target.TargetType in ProgramTargets then'#010+
  '    begin'#010+
  '      OD:=GetBinOutputDir(APackage,True);'#010+
  '      Result:=Result+'#039' -FE'#039' + ExtractRelativePath(PD,OD);'#010+
  '    end;'#010+
  '  OD:=GetUnitsOutputDir(','APackage,True);'#010+
  '  Result := Result + '#039' -FU'#039' + ExtractRelativePath(PD,OD);'#010+
  '  // Package Input file paths'#010+
  '  AddConditionalStrings(Result,APackage.UnitPath,Defaults.CPU,Defaults'+
  '.OS,'#039'-Fu'#039');'#010+
  '  AddConditionalStrings(Result,APackage.IncludePath,De','faults.CPU,Def'+
  'aults.OS,'#039'-Fi'#039');'#010+
  '  AddConditionalStrings(Result,APackage.ObjectPath,Defaults.CPU,Defaul'+
  'ts.OS,'#039'-Fo'#039');'#010+
  '  AddConditionalStrings(Result,Target.UnitPath,Defaults.CPU,Defaults.O'+
  'S,'#039'-Fu'#039');'#010+
  '  AddConditionalStrings(Result,Target.Include','Path,Defaults.CPU,Defau'+
  'lts.OS,'#039'-Fi'#039');'#010+
  '  AddConditionalStrings(Result,Target.ObjectPath,Defaults.CPU,Defaults'+
  '.OS,'#039'-Fo'#039');'#010+
  '  // Global unit dirs'#010+
  '  L:=TStringList.Create;'#010+
  '  L.Sorted:=true;'#010+
  '  L.Duplicates:=dupIgnore;'#010+
  '  AddDependencyUnitPaths(L,A','Package);'#010+
  '  for i:=0 to L.Count-1 do'#010+
  '    Result:=Result+'#039' -Fu'#039'+L[i];'#010+
  '  FreeAndNil(L);'#010+
  '  // Custom Options'#010+
  '  If (Defaults.Options<>'#039#039') then'#010+
  '    Result:=Result+'#039' '#039'+Defaults.Options;'#010+
  '  If (APackage.Options<>'#039#039') then'#010+
  '    Result:=Result+'#039' '#039'+APac','kage.Options;'#010+
  '  If (Target.Options<>'#039#039') then'#010+
  '    Result:=Result+'#039' '#039'+Target.Options;'#010+
  '  // Add Filename to compile'#010+
  '  Result:=Result+'#039' '#039'+ExtractRelativePath(PD, ExpandFileName(Ta'+
  'rget.FullSourceFileName));'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.GetCompil','er : String;'#010+
  'Var'#010+
  '  S : String;'#010+
  'begin'#010+
  '  // Cache in FCompiler for speed.'#010+
  '  If (FCompiler='#039#039') then'#010+
  '    begin'#010+
  '    FCompiler:=Defaults.Compiler;'#010+
  '    If (ExtractFilePath(FCompiler)='#039#039') then'#010+
  '      begin'#010+
  '      S:=FileSearch(FCompiler,GetEnvironmen','tVariable('#039'PATH'#039')'+
  ');'#010+
  '      If (S<>'#039#039') then'#010+
  '         FCompiler:=S;'#010+
  '      end;'#010+
  '    end;'#010+
  '  Result:=FCompiler;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Compile(Target: TTarget);'#010+
  'Var'#010+
  '  S : String;'#010+
  'begin'#010+
  '  if Target.State in [tsNeutral,tsNeedCompile] then'#010+
  '  ','  begin'#010+
  '      Log(vlInfo,SInfoCompilingTarget,[Target.Name]);'#010+
  '      ExecuteCommands(Target.Commands,caBeforeCompile);'#010+
  '      If Assigned(Target.BeforeCompile) then'#010+
  '        Target.BeforeCompile(Target);'#010+
  '      S:=GetCompilerCommand(FCurrentPac','kage,Target);'#010+
  '      ExecuteCommand(GetCompiler,S);'#010+
  '      Target.FTargetState:=tsCompiled;'#010+
  '      If Assigned(Target.AfterCompile) then'#010+
  '        Target.AfterCompile(Target);'#010+
  '      ExecuteCommands(Target.Commands,caAfterCompile);'#010+
  '    end'#010+
  '  else',' if Target.State<>tsCompiled then'#010+
  '    Log(vlWarning, Format(SWarnAttemptingToCompileNonNeutralTarget, [T'+
  'arget.Name]));'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CompileDependencies(Target: TTarget);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  T : TTarget;'#010+
  '  D : TDependency;'#010,
  'begin'#010+
  '  if Target.State in [tsCompiled,tsNoCompile] then'#010+
  '    exit;'#010+
  '  Log(vlDebug, Format(SDbgCompilingDependenciesOfTarget, [Target.Name]'+
  '));'#010+
  '  For I:=0 to Target.Dependencies.Count-1 do'#010+
  '    begin'#010+
  '      D:=Target.Dependencies[i];'#010+
  '      if (D','.DependencyType=depUnit) and'#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '        begin'#010+
  '          T:=TTarget(D.Target);'#010+
  '          If Assigned(T) then'#010+
  '            begin'#010+
  '              // We don'#039't need to compile implicit',' units, they a'+
  're only'#010+
  '              // used for dependency checking'#010+
  '              if (T.TargetType<>ttImplicitUnit) then'#010+
  '                begin'#010+
  '{$warning Circular dependency check is disabled}'#010+
  '//                    Log(vlWarning,SWarnCircula','rDependency,[Target.'+
  'Name,T.Name])'#010+
  '                  MaybeCompile(T);'#010+
  '                end;'#010+
  '            end'#010+
  '          else'#010+
  '            Error(SErrDepUnknownTarget,[Target.Name,D.Value]);'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Mayb','eCompile(Target: TTarget);'#010+
  'begin'#010+
  '  ResolveDependencies(Target.Dependencies,Target.Collection as TTarget'+
  's);'#010+
  '  CompileDependencies(Target);'#010+
  '  if NeedsCompile(Target) then'#010+
  '    begin'#010+
  '      Compile(Target);'#010+
  '      Target.FTargetState:=tsCompiled;',#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuildEngine.GetPackageDir(APackage: TPackage; AbsolutePath: '+
  'Boolean): String;'#010+
  'begin'#010+
  '  If AbsolutePath then'#010+
  '    Result:= IncludeTrailingPathDelimiter(FStartDir)'#010+
  '  else'#010+
  '    Result:='#039#039';'#010+
  '  Result:=Result+APackage.Dire','ctory;'#010+
  '  If (Result<>'#039#039') then'#010+
  '    Result:= IncludeTrailingPathDelimiter(Result);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.GetOutputDir(AName: string; APackage : TPackage;'+
  ' AbsolutePath : Boolean = False) : String;'#010+
  'begin'#010+
  '  If (TargetDir<>'#039#039') then'#010+
  '    Res','ult:=IncludeTrailingPathDelimiter(TargetDir)'#010+
  '  else'#010+
  '    begin'#010+
  '      If AbsolutePath then'#010+
  '        Result:=IncludeTrailingPathDelimiter(FStartDir)'#010+
  '      else'#010+
  '        Result:='#039#039';'#010+
  '      If (APackage.Directory<>'#039#039') then'#010+
  '        Result:=IncludeTr','ailingPathDelimiter(Result+APackage.Directo'+
  'ry);'#010+
  '      Result := IncludeTrailingPathDelimiter(Result + AName + PathDeli'+
  'm + Defaults.Target);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.GetUnitsOutputDir(APackage : TPackage; AbsoluteP'+
  'ath : Boolean ','= False) : String;'#010+
  'begin'#010+
  '  Result := GetOutputDir('#039'units'#039', APackage, AbsolutePath);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.GetBinOutputDir(APackage : TPackage; AbsolutePat'+
  'h : Boolean = False) : String;'#010+
  'begin'#010+
  '  Result := GetOutputDir('#039'bin'#039', APackage, ','AbsolutePath);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CreateOutputDir(APackage: TPackage);'#010+
  'Var'#010+
  '  D : String;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  //create a units directory'#010+
  '  D:=GetUnitsOutputDir(APackage,True);'#010+
  '  If not SysDirectoryExists(D) then'#010+
  '    begin'#010+
  '      ','Log(vlInfo,SInfoCreatingOutputDir,[D]);'#010+
  '      CmdCreateDir(D);'#010+
  '    end;'#010+
  #010+
  '  //also create a bin directory for programtargets'#010+
  '  For i := 0 to Pred(APackage.Targets.Count) do'#010+
  '  begin'#010+
  '    if APackage.Targets.TargetItems[i].TargetType in Program','Targets '+
  'then'#010+
  '    begin'#010+
  '      D:=GetBinOutputDir(APackage,True);'#010+
  '      If not SysDirectoryExists(D) then'#010+
  '        begin'#010+
  '          Log(vlInfo,SInfoCreatingOutputDir,[D]);'#010+
  '          CmdCreateDir(D);'#010+
  '        end;'#010+
  '      exit; //do not continue lo','op, directory is made anyway'#010+
  '    end;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.DependencyOK(ADependency : TDependency) : Boolea'+
  'n;'#010+
  'begin'#010+
  '  Result:=(Defaults.CPU in ADependency.CPUs) and (Defaults.OS in ADepe'+
  'ndency.OSes);'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEng','ine.TargetOK(ATarget : TTarget) : Boolean;'#010+
  'begin'#010+
  '  Result:=(Defaults.CPU in ATarget.CPUs) and (Defaults.OS in ATarget.O'+
  'Ses);'#010+
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
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoAfterCompile(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterCompile) then'#010+
  '    APackage.AfterCompile(APackage);'#010+
  '  ExecuteCommands(APackage.Commands,caAfterCompile);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngin','e.Compile(APackage: TPackage);'#010+
  'Var'#010+
  '  T : TTarget;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  Log(vlInfo,SInfoCompilingPackage,[APackage.Name]);'#010+
  '  FCurrentPackage:=APackage;'#010+
  '  FCurrentOutputDir:=GetUnitsOutputDir(APackage,True);'#010+
  '  Try'#010+
  '    If (APackage.Directory','<>'#039#039') then'#010+
  '      EnterDir(APackage.Directory);'#010+
  '    CreateOutputDir(APackage);'#010+
  '    Dictionary.AddVariable('#039'OUTPUTDIR'#039',FCurrentOutputDir);'#010+
  '    DoBeforeCompile(APackage);'#010+
  '    Try'#010+
  '      For I:=0 to APackage.Targets.Count-1 do'#010+
  '        begin'#010+
  '    ','      T:=APackage.Targets.TargetItems[i];'#010+
  '          if (T.TargetType in [ttUnit,ttProgram]) then'#010+
  '            begin'#010+
  '              if TargetOK(T) then'#010+
  '                begin'#010+
  '                  if FForceCompile then'#010+
  '                    T.FTarget','State:=tsNeedCompile;'#010+
  '                  MaybeCompile(T);'#010+
  '                end'#010+
  '              else'#010+
  '                begin'#010+
  '                  if not(Defaults.CPU in T.CPUs) then'#010+
  '                    Log(vldebug, Format(SDbgTargetHasWrongCPU, [CPUs','T'+
  'oString(T.CPUs)]));'#010+
  '                  if not(Defaults.OS in T.OSes) then'#010+
  '                    Log(vldebug, Format(SDbgTargetHasWrongOS, [OSesToS'+
  'tring(T.OSes)]));'#010+
  '                end;'#010+
  '            end'#010+
  '          else'#010+
  '            log(vldebug, S','DbgTargetIsNotAUnitOrProgram,[T.Name]);'#010+
  '        end;'#010+
  '      DoAfterCompile(APackage);'#010+
  '    Finally'#010+
  '      If (APackage.Directory<>'#039#039') then'#010+
  '        EnterDir('#039#039');'#010+
  '    end;'#010+
  '  Finally'#010+
  '    FCurrentPackage:=Nil;'#010+
  '    FCurrentOutputDir:='#039#039';'#010+
  '  end;'#010+
  'end',';'#010+
  #010+
  #010+
  'function TBuildEngine.CheckExternalPackage(Const APackageName : String'+
  '):TPackage;'#010+
  'var'#010+
  '  S : String;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  // Already checked?'#010+
  '  I:=ExternalPackages.IndexOfName(APackageName);'#010+
  '  if I<>-1 then'#010+
  '    begin'#010+
  '      result:=Exte','rnalPackages.PackageItems[I];'#010+
  '      exit;'#010+
  '    end;'#010+
  '  // Create new external package'#010+
  '  Result:=ExternalPackages.AddPackage(APackageName);'#010+
  '  Result.FTargetState:=tsNotFound;'#010+
  '  // Load unit config'#010+
  '  S:=GetUnitDir(Result);'#010+
  '  if S<>'#039#039' then'#010+
  '    b','egin'#010+
  '      Log(vldebug, SDbgExternalDependency, [APackageName,S]);'#010+
  '      Result.FTargetState:=tsInstalled;'#010+
  '      // Load unit config if it exists'#010+
  '      S:=IncludeTrailingPathDelimiter(S)+UnitConfigFile;'#010+
  '      if FileExists(S) then'#010+
  '        b','egin'#010+
  '          Log(vlDebug, Format(SDbgLoading, [S]));'#010+
  '          Result.LoadUnitConfigFromFile(S);'#010+
  '        end;'#010+
  '      // Check recursive implicit dependencies'#010+
  '      CompileDependencies(Result);'#010+
  '    end'#010+
  '  else'#010+
  '    Error(SErrDependencyNotFoun','d,[APackageName]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CompileDependencies(APackage: TPackage);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  D : TDependency;'#010+
  'begin'#010+
  '  For I:=0 to APackage.Dependencies.Count-1 do'#010+
  '    begin'#010+
  '      D:=APackage.Dependencies[i]',';'#010+
  '      if (D.DependencyType=depPackage) and'#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '        begin'#010+
  '          P:=TPackage(D.Target);'#010+
  '          // If it already was compiled, then State<>tsNeutral, and it'+
  ' won'#039't be c','ompiled again.'#010+
  '          If Assigned(P) then'#010+
  '            Compile(P)'#010+
  '          else'#010+
  '            D.Target:=CheckExternalPackage(D.Value);'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TBuildEngine.InstallPackageFiles(APAckage : TPackage; tt : TT'+
  'arget','Type; Const Dest : String):Boolean;'#010+
  'Var'#010+
  '  List : TStringList;'#010+
  '  PD,UD,BD : string;'#010+
  'begin'#010+
  '  Result:=False;'#010+
  '  List:=TStringList.Create;'#010+
  '  Try'#010+
  '    UD:=GetUnitsOutputDir(APackage);'#010+
  '    BD:=GetBinOutputDir(APackage);'#010+
  '    PD:=GetPackageDir(APacka','ge);'#010+
  '    APackage.GetInstallFiles(List,[tt],PD, UD, BD, Defaults.CPU, Defau'+
  'lts.OS);'#010+
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
  'procedure TB','uildEngine.DoBeforeInstall(APackage: TPackage);'#010+
  'begin'#010+
  '  ExecuteCommands(APackage.Commands,caBeforeInstall);'#010+
  '  If Assigned(APackage.BeforeInstall) then'#010+
  '    APackage.BeforeInstall(APackage);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoAfterInstall(APacka','ge: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterInstall) then'#010+
  '    APackage.AfterInstall(APackage);'#010+
  '  ExecuteCommands(APackage.Commands,caAfterInstall);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Install(APackage: TPackage);'#010+
  'Var'#010+
  '  UC,D : String;'#010+
  '  B : B','oolean;'#010+
  'begin'#010+
  '  If (Apackage.State<>tsCompiled) then'#010+
  '    Compile(APackage);'#010+
  '  Log(vlInfo,SInfoInstallingPackage,[APackage.Name]);'#010+
  '  DoBeforeInstall(APackage);'#010+
  '  // units'#010+
  '  B:=false;'#010+
  '  D:=IncludeTrailingPathDelimiter(Defaults.UnitInstallDir)','+APackage.'+
  'Name;'#010+
  '  if InstallPackageFiles(APAckage,ttUnit,D) then'#010+
  '    B:=true;'#010+
  '  if InstallPackageFiles(APAckage,ttImplicitUnit,D) then'#010+
  '    B:=true;'#010+
  '  // Unit (dependency) configuration if there were units installed'#010+
  '  if B then'#010+
  '    begin'#010+
  '   ','   UC:=IncludeTrailingPathDelimiter(D)+UnitConfigFile;'#010+
  '      Log(vlInfo, Format(SDbgGenerating, [UC]));'#010+
  '      APackage.SaveUnitConfigToFile(UC,Defaults.CPU,Defaults.OS);'#010+
  '    end;'#010+
  '  // Programs'#010+
  '  D:=IncludeTrailingPathDelimiter(Defaults.BinI','nstallDir);'#010+
  '  InstallPackageFiles(APAckage,ttProgram,D);'#010+
  '  // Done.'#010+
  '  APackage.FTargetState:=tsInstalled;'#010+
  '  DoAfterInstall(APackage);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoBeforeArchive(APackage: TPackage);'#010+
  'begin'#010+
  '  ExecuteCommands(APackage.Comman','ds,caBeforeArchive);'#010+
  '  If Assigned(APackage.BeforeArchive) then'#010+
  '    APackage.BeforeArchive(APackage);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoAfterArchive(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterArchive) then'#010+
  '    APackage.AfterArchiv','e(APackage);'#010+
  '  ExecuteCommands(APackage.Commands,caAfterArchive);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Archive(APackage: TPackage);'#010+
  'Var'#010+
  '  L : TStringList;'#010+
  '  PD,A : String;'#010+
  '  i: integer;'#010+
  '  ICPU : TCPU;'#010+
  '  IOS  : TOS;'#010+
  'begin'#010+
  '  Log(vlInfo,SInfoArchivin','gPackage,[APackage.Name]);'#010+
  '  DoBeforeArchive(Apackage);'#010+
  '  L:=TStringList.Create;'#010+
  '  L.Sorted:=true;'#010+
  '  L.Duplicates:=dupIgnore;'#010+
  '  Try'#010+
  '    // Add fpmake.pp & manifest.xml always'#010+
  '    PD:=GetPackageDir(APackage,False);'#010+
  '    L.Add(PD+FPMakePPFile)',';'#010+
  '    L.Add(PD+ManifestFile);'#010+
  '    //get all files from all targets'#010+
  '    for ICPU:=Low(TCPU) to high(TCPU) do'#010+
  '      for IOS:=Low(TOS) to high(TOS) do'#010+
  '        if OSCpupossible[IOS,ICPU] then'#010+
  '          begin'#010+
  '            ResolveFileNames(APackag','e,ICPU,IOS);'#010+
  '            APackage.GetArchiveFiles(L, ICPU, IOS);'#010+
  '          end;'#010+
  '    //from sources'#010+
  '    for i := 0 to APackage.Sources.Count-1 do'#010+
  '      L.Add(APackage.Sources[i].Name);'#010+
  #010+
  '    //show all files'#010+
  '    for i := 0 to L.Count-1 do'#010+
  '   ','   Log(vlInfo, Format(SInfoArchivingFile, [L[i]]));'#010+
  #010+
  '    A:=APackage.FileName + ZipExt;'#010+
  #010+
  '{$ifdef HAS_UNIT_ZIPPER}'#010+
  '    if not Assigned(ArchiveFilesProc) then'#010+
  '      begin'#010+
  '        FZipFile := TZipper.Create;'#010+
  '        FZipFile.ZipFiles(A, L);'#010+
  '  ','    end'#010+
  '    else'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  '      CmdArchiveFiles(L,A);'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  #010+
  '{$ifdef HAS_UNIT_ZIPPER}'#010+
  '    if not Assigned(ArchiveFilesProc) then'#010+
  '      FZipFile.Free;'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  '  end;'#010+
  '  DoAfterArchive(Apackage)',';'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoBeforeClean(APackage: TPackage);'#010+
  'begin'#010+
  '  ExecuteCommands(APackage.Commands,caBeforeClean);'#010+
  '  If Assigned(APackage.BeforeClean) then'#010+
  '    APackage.BeforeClean(APackage);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoAfterC','lean(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterClean) then'#010+
  '    APackage.AfterClean(APackage);'#010+
  '  ExecuteCommands(APackage.Commands,caAfterClean);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Clean(APackage: TPackage);'#010+
  'Var'#010+
  '  OU : String;'#010+
  '  OB :',' String;'#010+
  '  List : TStringList;'#010+
  'begin'#010+
  '  Log(vlInfo,SInfoCleaningPackage,[APackage.Name]);'#010+
  '  DoBeforeClean(Apackage);'#010+
  '  OU:=IncludeTrailingPathDelimiter(GetUnitsOutputDir(APAckage));'#010+
  '  OB:=IncludeTrailingPathDelimiter(GetBinOutputDir(APAckage','));'#010+
  '  List:=TStringList.Create;'#010+
  '  try'#010+
  '    APackage.GetCleanFiles(List,OU, OB, Defaults.CPU,Defaults.OS);'#010+
  '    if (List.Count>0) then'#010+
  '      CmdDeleteFiles(List);'#010+
  '  Finally'#010+
  '    List.Free;'#010+
  '  end;'#010+
  '  DoAfterClean(Apackage);'#010+
  'end;'#010+
  #010+
  #010+
  'function TBuild','Engine.NeedsCompile(APackage: TPackage): Boolean;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  '  D : TDependency;'#010+
  'begin'#010+
  '  Result:=False;'#010+
  '  case APackage.State of'#010+
  '    tsNeedCompile :'#010+
  '      begin'#010+
  '        result:=true;'#010+
  '        exit;'#010+
  '      end;'#010+
  '    tsCom','piled :'#010+
  '      exit;'#010+
  '  end;'#010+
  #010+
  '  I:=0;'#010+
  '  For I:=0 to APackage.Dependencies.Count-1 do'#010+
  '    begin'#010+
  '      D:=APackage.Dependencies[i];'#010+
  '      if (D.DependencyType=depPackage) and'#010+
  '         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then'#010+
  '  ','      begin'#010+
  '          P:=TPackage(D.Target);'#010+
  '          // I'#039'm not sure whether the target dir is OK here ??'#010+
  '          Result:=Assigned(P) and NeedsCompile(P);'#010+
  '          if Result then'#010+
  '            exit;'#010+
  '        end;'#010+
  '    end;'#010+
  '  If Not Result ','then'#010+
  '    begin'#010+
  '      I:=0;'#010+
  '      While (Not Result) and (I<APackage.Targets.Count) do'#010+
  '        begin'#010+
  '          Result:=NeedsCompile(APackage.Targets.TargetItems[i]);'#010+
  '          Inc(I);'#010+
  '        end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure TBuildEngine.GetMa','nifest(APackage : TPackage; Manifest : T'+
  'Strings);'#010+
  'begin'#010+
  '  APackage.GetManifest(Manifest);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Compile(Packages: TPackages);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  If Assigned(BeforeCompile) then'#010+
  '    BeforeCompi','le(Self);'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '      P:=Packages.PackageItems[i];'#010+
  '      FCurrentPackage := P;'#010+
  '      If PackageOK(P) then'#010+
  '        If (P.State=tsNeutral) then'#010+
  '          begin'#010+
  '            Log(vlDebug,SDbgConsideringPacka','ge,[P.Name]);'#010+
  '            if FForceCompile then'#010+
  '              P.FTargetState:=tsNeedCompile;'#010+
  '            ResolveDependencies(P.Dependencies,(P.Collection as TPacka'+
  'ges));'#010+
  '            CompileDependencies(P);'#010+
  '            ResolveFileNames(P,Def','aults.CPU,Defaults.OS);'#010+
  '            If NeedsCompile(P) then'#010+
  '              begin'#010+
  '                Compile(P);'#010+
  '                P.FTargetState:=tsCompiled;'#010+
  '              end;'#010+
  '          end;'#010+
  '    end;'#010+
  '  If Assigned(AfterCompile) then'#010+
  '    AfterCom','pile(Self);'#010+
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
  '  For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '      P:=Packages.Packag','eItems[i];'#010+
  '      If PackageOK(P) then'#010+
  '        Install(P);'#010+
  '    end;'#010+
  '  If Assigned(AfterInstall) then'#010+
  '    AfterInstall(Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Archive(Packages: TPackages);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  If Assigned(B','eforeArchive) then'#010+
  '    BeforeArchive(Self);'#010+
  '  Log(vlDebug, SDbgBuildEngineArchiving);'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '    P:=Packages.PackageItems[i];'#010+
  '    If PackageOK(P) then'#010+
  '      Archive(P);'#010+
  '    end;'#010+
  '  If Assigned(AfterArchiv','e) then'#010+
  '    AfterArchive(Self);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Clean(Packages: TPackages);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  If Assigned(BeforeClean) then'#010+
  '    BeforeClean(Self);'#010+
  '  Log(vldebug, SDbgBuildEngineCleaning);'#010+
  '  For I:=0 to',' Packages.Count-1 do'#010+
  '    begin'#010+
  '    P:=Packages.PackageItems[i];'#010+
  '    If PackageOK(P) then'#010+
  '      Clean(P);'#010+
  '    end;'#010+
  '  If Assigned(AfterClean) then'#010+
  '    AfterClean(Self);'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure TBuildEngine.GetManifest(Packages : TPackages; Manifest ',': '+
  'TStrings);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  If Assigned(BeforeManifest) then'#010+
  '    BeforeManifest(Self);'#010+
  '  Manifest.Add('#039'<packages>'#039');'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    GetManifest(Packages.PackageItems[i],Manifest);'#010+
  '  Manifest.Add('#039'</package','s>'#039');'#010+
  '  If Assigned(AfterManifest) then'#010+
  '    AfterManifest(Self);'#010+
  'end;'#010+
  #010+
  #010+
  '{*********************************************************************'+
  '*******'#010+
  '                                 TTarget'#010+
  '*************************************************','*******************'+
  '********}'#010+
  #010+
  'constructor TTarget.Create(ACollection: TCollection);'#010+
  'begin'#010+
  '  inherited Create(ACollection);'#010+
  '  FInstall:=True;'#010+
  '  FCPUs:=AllCPUs;'#010+
  '  FOSes:=AllOSes;'#010+
  '  FUnitPath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  ','FIncludePath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FObjectPath:=TConditionalStrings.Create(TConditionalString);'#010+
  '  FDependencies:=TDependencies.Create(TDependency);'#010+
  '  FCommands:=TCOmmands.Create(TCommand);'#010+
  'end;'#010+
  #010+
  #010+
  'destructor TTar','get.Destroy;'#010+
  'begin'#010+
  '  FreeAndNil(FUnitPath);'#010+
  '  FreeAndNil(FObjectPath);'#010+
  '  FreeAndNil(FIncludePath);'#010+
  '  FreeAndNil(FDependencies);'#010+
  '  FreeAndNil(FCommands);'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetSourceFileName: String;'#010+
  'begin'#010+
  '  Result:','=Name+FExtension;'#010+
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
  '  Result:','=Name+RSText;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetProgramFileName(AOS : TOS): String;'#010+
  'begin'#010+
  '  if AOS in [Go32v2,Win32,Win64,OS2] then'#010+
  '    Result:=Name+ExeExt'#010+
  '  else'#010+
  '    Result:=Name;'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetOutputFileName(AOs: TOS): String;'#010+
  'begin',#010+
  '  if TargetType in UnitTargets then'#010+
  '    Result:=GetUnitFileName'#010+
  '  else'#010+
  '    Result:=GetProgramFileName(AOs);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TTarget.SetName(const AValue: String);'#010+
  #010+
  'Var'#010+
  '  D,N,E : String;'#010+
  #010+
  'begin'#010+
  '  N:=FixPath(AValue);'#010+
  '  D:=ExtractFilePath(N)',';'#010+
  '  E:=ExtractFileExt(N);'#010+
  '  N:=ExtractFileName(N);'#010+
  '  If (E<>'#039#039') then'#010+
  '    N:=Copy(N,1,Length(N)-Length(E))'#010+
  '  else'#010+
  '    E:=Defaults.SourceExt;'#010+
  '  inherited SetName(N);'#010+
  '  FExtension:=E;'#010+
  '  FDirectory:=D;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TTarget.GetCleanFiles(Lis','t: TStrings; const APrefixU, APre'+
  'fixB : String; ACPU: TCPU; AOS : TOS);'#010+
  'begin'#010+
  '  If not(ACPU in CPUs) or not(AOS in OSes) then'#010+
  '    exit;'#010+
  '  List.Add(APrefixU + ObjectFileName);'#010+
  '  If (TargetType in [ttUnit,ttImplicitUnit,ttExampleUnit, ttClean','Onl'+
  'yUnit]) then'#010+
  '    List.Add(APrefixU + UnitFileName)'#010+
  '  else If (TargetType in [ttProgram,ttExampleProgram]) then'#010+
  '    List.Add(APrefixB + GetProgramFileName(AOS));'#010+
  '  If ResourceStrings then'#010+
  '    List.Add(APrefixU + RSTFileName);'#010+
  '  // Maybe a','dd later ?  AddConditionalStrings(List,CleanFiles);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TTarget.GetInstallFiles(List: TStrings; const APrefixU, APre'+
  'fixB: String; ACPU: TCPU; AOS : TOS);'#010+
  'begin'#010+
  '  If not(ACPU in CPUs) or not(AOS in OSes) then'#010+
  '    exit;'#010+
  '  If Not ','(TargetType in [ttProgram,ttExampleProgram]) then'#010+
  '    List.Add(APrefixU + ObjectFileName);'#010+
  '  If (TargetType in [ttUnit,ttImplicitUnit,ttExampleUnit]) then'#010+
  '    List.Add(APrefixU + UnitFileName)'#010+
  '  else If (TargetType in [ttProgram,ttExamplePr','ogram]) then'#010+
  '    List.Add(APrefixB + GetProgramFileName(AOS));'#010+
  '  If ResourceStrings then'#010+
  '    List.Add(APrefixU + RSTFileName);'#010+
  '  // Maybe add later ?  AddConditionalStrings(List,InstallFiles);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TTarget.GetArchiveFiles(List: ','TStrings; ACPU: TCPU; AOS : '+
  'TOS);'#010+
  'var'#010+
  '  i : integer;'#010+
  '  D : TDependency;'#010+
  'begin'#010+
  '  If not(ACPU in CPUs) or not(AOS in OSes) then'#010+
  '    exit;'#010+
  '  // Main source'#010+
  '  List.Add(FullSourceFileName);'#010+
  '  // Includes'#010+
  '  for i:=0 to Dependencies.Count-1 do'#010+
  '   ',' begin'#010+
  '      D:=Dependencies[i];'#010+
  '      if (D.DependencyType=depInclude) and'#010+
  '         (ACPU in D.CPUs) and (AOS in D.OSes) then'#010+
  '        List.Add(D.FullFileName);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  '{ TSource }'#010+
  #010+
  'constructor TSource.Create(ACollection: TCollectio','n);'#010+
  'begin'#010+
  '  inherited Create(ACollection);'#010+
  'end;'#010+
  #010+
  'destructor TSource.Destroy;'#010+
  'begin'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  #010+
  '{ TCommands }'#010+
  #010+
  'function TCommands.GetCommand(const Dest : String): TCommand;'#010+
  #010+
  'begin'#010+
  '  Result:=TCommand(ItemByName(Dest));'#010+
  'end;'#010+
  #010+
  'f','unction TCommands.GetCommandItem(Index : Integer): TCommand;'#010+
  'begin'#010+
  '  Result:=TCommand(Items[Index]);'#010+
  'end;'#010+
  #010+
  'procedure TCommands.SetCommandItem(Index : Integer; const AValue: TCom'+
  'mand);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  'Function TCommands.A','ddCommand(const Cmd: String) : TCommand;'#010+
  'begin'#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,'#039#039','#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  'function TCommands.AddCommand(const Cmd, Options: String): TCommand;'#010+
  'begin'#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,Options,'#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  'functio','n TCommands.AddCommand(const Cmd, Options, Dest, Source: Stri'+
  'ng'#010+
  '  ): TCommand;'#010+
  'begin'#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,options,Dest,Source);'#010+
  'end;'#010+
  #010+
  'Function TCommands.AddCommand(At: TCommandAt; const Cmd: String) : TCo'+
  'mmand;'#010+
  'begin'#010+
  '  Result',':=AddCommand(At,Cmd,'#039#039','#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  'function TCommands.AddCommand(At: TCommandAt; const Cmd, Options: Stri'+
  'ng'#010+
  '  ): TCommand;'#010+
  'begin'#010+
  '  Result:=AddCommand(At,Cmd,Options,'#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  'function TCommands.AddCommand(At: TCommandAt; const Cmd, Op','tions, De'+
  'st,'#010+
  '  Source: String): TCommand;'#010+
  'begin'#010+
  '  Result:=Add as TCommand;'#010+
  '  Result.Command:=Cmd;'#010+
  '  Result.Options:=Options;'#010+
  '  Result.At:=At;'#010+
  '  Result.SourceFile:=Source;'#010+
  '  Result.DestFile:=Dest;'#010+
  'end;'#010+
  #010+
  '{ TConditionalStrings }'#010+
  #010+
  'Constructor T','ConditionalStrings.Create(AClass:TClass);'#010+
  'begin'#010+
  '  FCSClass:=AClass;'#010+
  'end;'#010+
  #010+
  'function TConditionalStrings.GetConditionalString(Index : Integer): TC'+
  'onditionalString;'#010+
  'begin'#010+
  '  Result:=TConditionalString(Items[Index]);'#010+
  'end;'#010+
  #010+
  'procedure TConditional','Strings.SetConditionalString(Index : Integer; '+
  'const AValue: TConditionalString);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  'Function TConditionalStrings.Add(Const Value : String) : TConditionalS'+
  'tring;'#010+
  'begin'#010+
  '  result:=Add(Value,AllCPUs,AllOSes);'#010+
  'en','d;'#010+
  #010+
  'Function TConditionalStrings.Add(Const Value : String;const CPUs:TCPUs'+
  ') : TConditionalString;'#010+
  'begin'#010+
  '  result:=Add(Value,CPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  'Function TConditionalStrings.Add(Const Value : String;const OSes:TOSes'+
  ') : TConditionalString;'#010+
  'b','egin'#010+
  '  result:=Add(Value,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  'Function TConditionalStrings.Add(Const Value : String;const CPUs:TCPUs'+
  ';const OSes:TOSes) : TConditionalString;'#010+
  'begin'#010+
  '  Result:=FCSClass.Create as TConditionalString;'#010+
  '  Result.Value:=Value;'#010+
  '  Res','ult.OSes:=OSes;'#010+
  '  Result.CPUs:=CPUs;'#010+
  '  inherited Add(Result);'#010+
  'end;'#010+
  #010+
  #010+
  'function TDependencies.GetDependency(Index : Integer): TDependency;'#010+
  'begin'#010+
  '  Result:=TDependency(Items[Index]);'#010+
  'end;'#010+
  #010+
  'procedure TDependencies.SetDependency(Index : Integer;',' const AValue:'+
  ' TDependency);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  'Function TDependencies.Add(Const Value : String) : TDependency;'#010+
  'begin'#010+
  '  result:=Add(Value,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  'Function TDependencies.Add(Const Value : String;const CPUs:TC','PUs) : '+
  'TDependency;'#010+
  'begin'#010+
  '  result:=Add(Value,CPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  'Function TDependencies.Add(Const Value : String;const OSes:TOSes) : TD'+
  'ependency;'#010+
  'begin'#010+
  '  result:=Add(Value,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  'Function TDependencies.Add(Const Value : Stri','ng;const CPUs:TCPUs;con'+
  'st OSes:TOSes) : TDependency;'#010+
  'begin'#010+
  '  Result:=inherited Add(Value,CPUs,OSes) as TDependency;'#010+
  '  Result.Target:=nil;'#010+
  '  Result.FDependencyType:=depPackage;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TDependencies.AddUnit(Const Value : String) : TD','ependency;'#010+
  'begin'#010+
  '  result:=AddUnit(Value,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  'Function TDependencies.AddUnit(Const Value : String;const CPUs:TCPUs) '+
  ': TDependency;'#010+
  'begin'#010+
  '  result:=AddUnit(Value,CPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  'Function TDependencies.AddUnit(Const Va','lue : String;const OSes:TOSes'+
  ') : TDependency;'#010+
  'begin'#010+
  '  result:=AddUnit(Value,AllCPUs,OSes);'#010+
  'end;'#010+
  #010+
  'Function TDependencies.AddUnit(Const Value : String;const CPUs:TCPUs;c'+
  'onst OSes:TOSes) : TDependency;'#010+
  'begin'#010+
  '  Result:=inherited Add(Value,CPUs,','OSes) as TDependency;'#010+
  '  Result.Target:=nil;'#010+
  '  Result.FDependencyType:=depUnit;'#010+
  'end;'#010+
  #010+
  #010+
  'Function TDependencies.AddInclude(Const Value : String) : TDependency;'+
  #010+
  'begin'#010+
  '  result:=AddInclude(Value,AllCPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  'Function TDependencies.Add','Include(Const Value : String;const CPUs:TC'+
  'PUs) : TDependency;'#010+
  'begin'#010+
  '  result:=AddInclude(Value,CPUs,AllOSes);'#010+
  'end;'#010+
  #010+
  'Function TDependencies.AddInclude(Const Value : String;const OSes:TOSe'+
  's) : TDependency;'#010+
  'begin'#010+
  '  result:=AddInclude(Value,All','CPUs,OSes);'#010+
  'end;'#010+
  #010+
  'Function TDependencies.AddInclude(Const Value : String;const CPUs:TCPU'+
  's;const OSes:TOSes) : TDependency;'#010+
  'Var'#010+
  '  D,N : String;'#010+
  'begin'#010+
  '  N:=FixPath(Value);'#010+
  '  D:=ExtractFilePath(N);'#010+
  '  N:=ExtractFileName(N);'#010+
  '  if ExtractFileExt(','N)='#039#039' then'#010+
  '    ChangeFileExt(N,IncExt);'#010+
  '  Result:=inherited Add(N,CPUs,OSes) as TDependency;'#010+
  '  Result.FDirectory:=D;'#010+
  '  Result.FDependencyType:=depInclude;'#010+
  'end;'#010+
  #010+
  '{ Default Instances }'#010+
  #010+
  'var'#010+
  '  DefInstaller : TCustomInstaller;'#010+
  #010+
  'Function Install','er(InstallerClass: TInstallerClass): TCustomInstalle'+
  'r;'#010+
  'begin'#010+
  '  If Not Assigned(DefInstaller) then'#010+
  '    DefInstaller:=InstallerClass.Create(Nil);'#010+
  '  Result:=DefInstaller;'#010+
  'end;'#010+
  #010+
  'Function Installer: TCustomInstaller;'#010+
  'begin'#010+
  '  Result := Installer(','TFPCInstaller);'#010+
  'end;'#010+
  #010+
  #010+
  '{ TValueItem }'#010+
  #010+
  'constructor TValueItem.Create(AValue: String);'#010+
  'begin'#010+
  '  FValue:=AValue;'#010+
  'end;'#010+
  #010+
  '{ TFunctionItem }'#010+
  #010+
  'constructor TFunctionItem.Create(AFunc: TReplaceFunction);'#010+
  'begin'#010+
  '  FFunc:=AFunc;'#010+
  'end;'#010+
  #010+
  '{ TDictionary }'#010+
  #010+
  'c','onstructor TDictionary.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  inherited Create(AOwner);'#010+
  '  FList:=TStringList.Create;'#010+
  '  FList.Sorted:=True;'#010+
  '  FList.Duplicates:=dupError;'#010+
  'end;'#010+
  #010+
  'destructor TDictionary.Destroy;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  For I:=','0 to Flist.Count-1 do'#010+
  '    FList.Objects[i].Free;'#010+
  '  FreeAndNil(FList);'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  'procedure TDictionary.AddVariable(const AName, Value: String);'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=Flist.IndexOf(AName);'#010+
  '  If I=-1 then'#010+
  '    I:=FLis','t.Add(Aname)'#010+
  '  else'#010+
  '    Flist.Objects[i].Free;'#010+
  '  Flist.Objects[i]:=TValueItem.Create(Value);'#010+
  'end;'#010+
  #010+
  'procedure TDictionary.AddFunction(const AName: String;'#010+
  '  FReplacement: TReplaceFunction);'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=Flist.IndexOf(AName',');'#010+
  '  If I=-1 then'#010+
  '    I:=Flist.Add(AName)'#010+
  '  else'#010+
  '    Flist.Objects[i].Free;'#010+
  '  Flist.Objects[i]:=TFunctionItem.Create(FReplacement);'#010+
  'end;'#010+
  #010+
  'procedure TDictionary.RemoveItem(const AName: String);'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=Flist.IndexOf(A','Name);'#010+
  '  If (I<>-1) then'#010+
  '    begin'#010+
  '    FList.Objects[i].Free;'#010+
  '    FList.Delete(I);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function TDictionary.GetValue(const AName: String): String;'#010+
  #010+
  'begin'#010+
  '  Result:=GetValue(AName,'#039#039');'#010+
  'end;'#010+
  #010+
  #010+
  'function TDictionary.GetValue(const AN','ame,Args: String): String;'#010+
  #010+
  'Var'#010+
  '  O : TObject;'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=Flist.IndexOf(AName);'#010+
  '  If (I=-1) then'#010+
  '    Raise EDictionaryError.CreateFmt(SErrNoDictionaryItem,[AName]);'#010+
  '  O:=Flist.Objects[I];'#010+
  '  If O is TValueItem then'#010+
  '    Result',':=TValueItem(O).FValue'#010+
  '  else'#010+
  '    Result:=TFunctionItem(O).FFunc(AName,Args);'#010+
  'end;'#010+
  #010+
  'function TDictionary.ReplaceStrings(Const ASource: String): String;'#010+
  #010+
  #010+
  'Var'#010+
  '  S,FN,FV : String;'#010+
  '  P: Integer;'#010+
  #010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  S:=ASource;'#010+
  '  P:=Pos('#039'$('#039,',S);'#010+
  '  While (P<>0) do'#010+
  '    begin'#010+
  '    Result:=Result+Copy(S,1,P-1);'#010+
  '    Delete(S,1,P+1);'#010+
  '    P:=Pos('#039')'#039',S);'#010+
  '    FN:=Copy(S,1,P-1);'#010+
  '    Delete(S,1,P);'#010+
  '    P:=Pos('#039' '#039',FN);'#010+
  '    If (P<>0) then // function arguments ?'#010+
  '      begin'#010+
  '      FV:=FN;'#010+
  '  ','    FN:=Copy(FN,1,P);'#010+
  '      System.Delete(FV,1,P);'#010+
  '      end'#010+
  '    else'#010+
  '      FV:='#039#039';'#010+
  '    Result:=Result+GetValue(FN,FV);'#010+
  '    P:=Pos('#039'$('#039',S);'#010+
  '    end;'#010+
  '  Result:=Result+S;'#010+
  'end;'#010+
  #010+
  'Function Substitute(Const Source : String; Macros : Array of stri','ng)'+
  ' : String;'#010+
  'Var'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  I:=0;'#010+
  '  While I<High(Macros) do'#010+
  '    begin'#010+
  '    Dictionary.AddVariable(Macros[i],Macros[I+1]);'#010+
  '    Inc(I,2);'#010+
  '    end;'#010+
  '  Result:=Dictionary.ReplaceStrings(Source);'#010+
  '  While I<High(Macros) do'#010+
  '    begin'#010+
  '  ','  Dictionary.RemoveItem(Macros[i]);'#010+
  '    Inc(I,2);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'Initialization'#010+
  '  OnGetApplicationName:=@GetFPMakeName;'#010+
  #010+
  'Finalization'#010+
  '  FreeAndNil(DefInstaller);'#010+
  '  FreeAndNil(Dictionary);'#010+
  '  FreeAndNil(Defaults);'#010+
  'end.'#010
);
