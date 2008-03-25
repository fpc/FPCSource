{
    This file is part of the Free Pascal Makefile Package

    Implementation of fpmake classes and functions

    Copyright (c) 2007 by the freepascal team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpmkunit;

{$Mode objfpc}
{$H+}
{$inline on}

{ For target or cpu dependent dependencies add also an overload were you
  can pass only a set of cpus. This is disabled for now because it creates
  an error in the compiler with overload choosing }
{ define cpu_only_overloads}

Interface

{$ifndef NO_UNIT_PROCESS}
  {$define HAS_UNIT_PROCESS}
{$endif NO_UNIT_PROCESS}

{$ifndef NO_UNIT_ZIPPER}
  {$define HAS_UNIT_ZIPPER}
{$endif NO_UNIT_ZIPPER}

uses
  SysUtils, Classes, StrUtils
{$ifdef HAS_UNIT_PROCESS}
  ,process
{$endif HAS_UNIT_PROCESS}
{$ifdef HAS_UNIT_ZIPPER}
  ,zipper
{$endif HAS_UNIT_ZIPPER}
  ;

Type
  TFileType = (ftSource,ftUnit,ftObject,ftResource,ftExecutable,ftStaticLibrary,
               ftSharedLibrary);
  TFileTypes = set of TFileType;

  // Please keep this order, see OSCPUSupported below
  TCpu=(cpuNone,
    i386,m68k,powerpc,sparc,x86_64,arm,powerpc64
  );
  TCPUS = Set of TCPU;

  // Please keep this order, see OSCPUSupported below
  TOS=(osNone,
    linux,go32v2,win32,os2,freebsd,beos,netbsd,
    amiga,atari, solaris, qnx, netware, openbsd,wdosx,
    palmos,macos,darwin,emx,watcom,morphos,netwlibc,
    win64,wince,gba,nds,embedded,symbian
  );
  TOSes = Set of TOS;

  TCompilerMode = (cmFPC,cmTP,cmObjFPC,cmDelphi,cmMacPas);
  TCompilerModes = Set of TCompilerMode;

  TTargetType = (ttProgram,ttUnit,ttImplicitUnit,ttCleanOnlyUnit,ttExampleUnit,ttExampleProgram);
  TTargetTypes = set of TTargetType;

  TTargetState = (tsNeutral,tsConsidering,tsNoCompile,tsCompiled,tsInstalled,tsNotFound);
  TTargetStates = Set of TTargetState;

  TSourceType = (stDoc,stSrc,stExample,stTest);
  TSourceTypes = set of TSourceType;

  TVerboseLevel = (vlError,vlWarning,vlInfo,vldebug,vlCommand);
  TVerboseLevels = Set of TVerboseLevel;

  TCommandAt = (caBeforeCompile,caAfterCompile,
                caBeforeInstall,caAfterInstall,
                caBeforeArchive,caAfterArchive,
                caBeforeClean,caAfterClean,
                caBeforeDownload,caAfterDownload);

  TDependencyType = (depPackage,depImplicitPackage,depUnit,depInclude);
  TDependencyTypes = set of TDependencyType;

  TLogEvent = Procedure (Level : TVerboseLevel; Const Msg : String) of Object;

  TRunMode = (rmCompile,rmBuild,rmInstall,rmArchive,rmClean,rmManifest);

Const
  // Aliases
  Amd64   = X86_64;
  PPC = PowerPC;
  PPC64 = PowerPC64;
  DOS = Go32v2;
  MacOSX = Darwin;

  AllOSes = [Low(TOS)..High(TOS)];
  AllCPUs = [Low(TCPU)..High(TCPU)];
  AllUnixOSes  = [Linux,FreeBSD,NetBSD,OpenBSD,Darwin,QNX,BeOS,Solaris];
  AllBSDOSes      = [FreeBSD,NetBSD,OpenBSD,Darwin];
  AllWindowsOSes  = [Win32,Win64,WinCE];

  { This table is kept OS,Cpu because it is easier to maintain (PFV) }
  OSCPUSupported : array[TOS,TCpu] of boolean = (
    { os          none   i386   m68k   ppc    sparc  x86_64 arm    ppc64}
    { none  }   ( false, false, false, false, false, false, false, false),
    { linux }   ( false, true,  true,  true,  true,  true,  true,  true ),
    { go32v2 }  ( false, true,  false, false, false, false, false, false),
    { win32 }   ( false, true,  false, false, false, false, false, false),
    { os2 }     ( false, true,  false, false, false, false, false, false),
    { freebsd } ( false, true,  true,  false, false, true,  false, false),
    { beos }    ( false, true,  false, false, false, false, false, false),
    { netbsd }  ( false, false, false, false, false, false, false, false),
    { amiga }   ( false, false, true,  true,  false, false, false, false),
    { atari }   ( false, false, false, false, false, false, false, false),
    { solaris } ( false, true,  false, false, true,  false, false, false),
    { qnx }     ( false, false, false, false, false, false, false, false),
    { netware } ( false, true,  false, false, false, false, false, false),
    { openbsd } ( false, false, false, false, false, false, false, false),
    { wdosx }   ( false, true,  false, false, false, false, false, false),
    { palmos }  ( false, false, true,  false, false, false, true,  false),
    { macos }   ( false, false, false, true,  false, false, false, false),
    { darwin }  ( false, true,  false, true,  false, true,  false, true ),
    { emx }     ( false, true,  false, false, false, false, false, false),
    { watcom }  ( false, true,  false, false, false ,false, false, false),
    { morphos } ( false, false, false, true,  false ,false, false, false),
    { netwlibc }( false, true,  false, false, false, false, false, false),
    { win64   } ( false, false, false, false, false, true,  false, false),
    { wince    }( false, true,  false, false, false, false, true,  false),
    { gba    }  ( false, false, false, false, false, false, true,  false),
    { nds    }  ( false, false, false, false, false, false, true,  false),
    { embedded }( false, false, false, false, false, false, true,  false),
    { symbian } ( false, true,  false, false, false, false, true,  false)
  );

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

  FPMakePPFile = 'fpmake.pp';
  ManifestFile = 'manifest.xml';
  UnitConfigFile = 'fpunits.conf';

  DirNotFound = '<dirnotfound>';

  UnitTargets = [ttUnit,ttImplicitUnit,ttCleanOnlyUnit,ttExampleUnit];
  ProgramTargets = [ttProgram,ttExampleProgram];

  DefaultMessages = [vlError,vlWarning,vlCommand];
  AllMessages = [vlError,vlWarning,vlCommand,vlInfo];


Type
  { TNamedItem }

  TNamedItem = Class(TCollectionItem)
  private
    FName: String;
    procedure SetName(const AValue: String);virtual;
  Public
    property Name : String Read FName Write SetName;
  end;

  { TNamedCollection }

  TNamedCollection = Class(TCollection)
  private
    FUniqueNames: Boolean;
  Public
    Function IndexOfName(const AName : String) : Integer;
    Function ItemByName(const AName : String) : TNamedItem;
    Property UniqueNames : Boolean Read FUniqueNames;
  end;

  { TNamedItemList }

  TNamedItemList = Class(TFPList)
  private
    function GetNamedItem(Index : Integer): TNamedItem;
    procedure SetNamedItem(Index : Integer; const AValue: TNamedItem);
  public
    Function IndexOfName(const AName : String) : Integer;
    Function ItemByName(const ANAme : String) : TNamedItem;
    Property NamedItems[Index : Integer] : TNamedItem Read GetNamedItem Write SetNamedItem; default;
  end;

  { TCommand }
  TCommand = Class(TNamedItem)
  private
    FAfterCommand: TNotifyEvent;
    FBeforeCommand: TNotifyEvent;
    FCommand: String;
    FCommandAt: TCommandAt;
    FDestFile: String;
    FIgnoreResult: Boolean;
    FOptions: String;
    FSourceFile: String;
  Public
    Property SourceFile : String Read FSourceFile Write FSourceFile;
    Property DestFile : String Read FDestFile Write FDestFile;
    Property Command : String Read FCommand Write FCommand;
    Property Options : String Read FOptions Write FOptions;
    Property At : TCommandAt Read FCommandAt Write FCommandAt;
    Property IgnoreResult : Boolean Read FIgnoreResult Write FIgnoreResult;
    Property BeforeCommand : TNotifyEvent Read FBeforeCommand Write FBeforeCommand;
    Property AfterCommand : TNotifyEvent Read FAfterCommand Write FAfterCommand;
  end;

  { TCommands }

  TCommands = Class(TNamedCollection)
  private
    FDefaultAt: TCommandAt;
    function GetCommand(const Dest : String): TCommand;
    function GetCommandItem(Index : Integer): TCommand;
    procedure SetCommandItem(Index : Integer; const AValue: TCommand);
  Public
    Function AddCommand(Const Cmd : String) : TCommand;
    Function AddCommand(Const Cmd,Options : String) : TCommand;
    Function AddCommand(Const Cmd,Options,Dest,Source : String) : TCommand;
    Function AddCommand(At : TCommandAt; Const Cmd : String) : TCommand;
    Function AddCommand(At : TCommandAt; Const Cmd,Options : String) : TCommand;
    Function AddCommand(At : TCommandAt; Const Cmd,Options, Dest,Source : String) : TCommand;
    Property CommandItems[Index : Integer] : TCommand Read GetCommandItem Write SetCommandItem;
    Property Commands[Dest : String] : TCommand Read GetCommand; default;
    Property DefaultAt : TCommandAt Read FDefaultAt Write FDefaultAt;
  end;

  { TFPVersion }

  TFPVersion = Class(TPersistent)
  private
    FMajor,
    FMinor,
    FMicro,
    FBuild    : Word;
    function GetAsString: String;
    function GetEmpty: Boolean;
    procedure SetAsString(const AValue: String);
  Public
   Procedure Clear;
   Procedure Assign(Source : TPersistent); override;
   Function CompareVersion(AVersion : TFPVersion) : Integer;
   Function SameVersion(AVersion : TFPVersion) : Boolean;
   Property AsString : String Read GetAsString Write SetAsString;
   Property Empty : Boolean Read GetEmpty;
  Published
   Property Major : Word Read FMajor Write FMajor;
   Property Minor : Word Read FMinor Write FMinor;
   Property Micro : Word Read FMicro Write FMicro;
   Property Build : Word Read FBuild Write FBuild;
  end;

  { TConditionalString }
  TConditionalString = Class
  private
    FOSes   : TOSes;
    FCPUs   : TCPUs;
    FValue  : String;
  Public
    Constructor Create;virtual;
    Property Value : String Read FValue Write FValue;
    Property OSes  : TOSes Read FOSes Write FOSes;
    Property CPUs : TCPUs Read FCPUS Write FCPUs;
  end;

  TConditionalStringClass = class of TConditionalString;

  { TConditionalStrings }

  TConditionalStrings = Class(TFPList)
  private
    FCSClass : TConditionalStringClass;
    function GetConditionalString(Index : Integer): TConditionalString;
    procedure SetConditionalString(Index : Integer; const AValue: TConditionalString);
  Public
    Constructor Create(AClass:TConditionalStringClass);
    Function Add(Const Value : String) : TConditionalString;inline;
    Function Add(Const Value : String;const OSes:TOSes) : TConditionalString;inline;
{$ifdef cpu_only_overloads}
    Function Add(Const Value : String;const CPUs:TCPUs) : TConditionalString;inline;
{$endif cpu_only_overloads}
    Function Add(Const Value : String;const CPUs:TCPUs;const OSes:TOSes) : TConditionalString;
    Property ConditionalStrings[Index : Integer] : TConditionalString Read GetConditionalString Write SetConditionalString; default;
  end;

  { TDependency }
  TDependency = Class(TConditionalString)
  private
    FDependencyType : TDependencyType;
    // Package, Unit
    FTarget : TObject;
    FVersion : TFPVersion;
    FRequireChecksum : Cardinal;
    // Filenames, Includes
    FTargetFileName : String;
    Function GetVersion : string;
    Procedure SetVersion(const V : string);
  Public
    Constructor Create;override;
    Destructor Destroy;override;
    Property Target : TObject Read FTarget Write FTarget;
    Property DependencyType : TDependencyType Read FDependencyType;
    Property TargetFileName : String Read FTargetFileName Write FTargetFileName;
    Property Version : String Read GetVersion Write SetVersion;
    Property RequireChecksum : Cardinal Read FRequireChecksum Write FRequireChecksum;
  end;

  TDependencies = Class(TConditionalStrings)
    function GetDependency(Index : Integer): TDependency;
    procedure SetDependency(Index : Integer; const AValue: TDependency);
  Public
    Function Add(Const Value : String) : TDependency;inline;
    Function Add(Const Value : String;const OSes:TOSes) : TDependency;inline;
{$ifdef cpu_only_overloads}
    Function Add(Const Value : String;const CPUs:TCPUs) : TDependency;inline;
{$endif cpu_only_overloads}
    Function Add(Const Value : String;const CPUs:TCPUs;const OSes:TOSes) : TDependency;
    Function AddUnit(Const Value : String) : TDependency;inline;
    Function AddUnit(Const Value : String;const OSes:TOSes) : TDependency;inline;
{$ifdef cpu_only_overloads}
    Function AddUnit(Const Value : String;const CPUs:TCPUs) : TDependency;inline;
{$endif cpu_only_overloads}
    Function AddUnit(Const Value : String;const CPUs:TCPUs;const OSes:TOSes) : TDependency;
    Function AddInclude(Const Value : String) : TDependency;inline;
    Function AddInclude(Const Value : String;const OSes:TOSes) : TDependency;inline;
{$ifdef cpu_only_overloads}
    Function AddInclude(Const Value : String;const CPUs:TCPUs) : TDependency;inline;
{$endif cpu_only_overloads}
    Function AddInclude(Const Value : String;const CPUs:TCPUs;const OSes:TOSes) : TDependency;
    Property Dependencies[Index : Integer] : TDependency Read GetDependency Write SetDependency; default;
  end;

  { TTarget }

  TTarget = Class(TNamedItem)
  private
    FInstall : Boolean;
    FAfterClean: TNotifyEvent;
    FAfterCompile: TNotifyEvent;
    FBeforeClean: TNotifyEvent;
    FBeforeCompile: TNotifyEvent;
    FCPUs: TCPUs;
    FOSes: TOSes;
    FMode: TCompilerMode;
    FResourceStrings: Boolean;
    FObjectPath,
    FUnitPath,
    FIncludePath : TConditionalStrings;
    FDependencies : TDependencies;
    FCommands : TCommands;
    FDirectory: String;
    FExtension: String;
    FTargetSourceFileName : String;
    FFileType: TFileType;
    FOptions: String;
    FFPCTarget: String;
    FTargetState: TTargetState;
    FTargetType: TTargetType;
  Protected
    Function GetSourceFileName : String; virtual;
    Function GetUnitFileName : String; virtual;
    Function GetObjectFileName : String; virtual;
    Function GetRSTFileName : String; Virtual;
    Function GetProgramFileName(AOS : TOS) : String; Virtual;
  Public
    Constructor Create(ACollection : TCollection); override;
    Destructor Destroy; override;
    Function  GetOutputFileName (AOs : TOS) : String; Virtual;
    procedure SetName(const AValue: String);override;
    Procedure GetCleanFiles(List : TStrings; const APrefixU, APrefixB : String; ACPU:TCPU; AOS : TOS); virtual;
    Procedure GetInstallFiles(List : TStrings; const APrefixU, APrefixB: String; ACPU:TCPU; AOS : TOS); virtual;
    Procedure GetArchiveFiles(List : TStrings; ACPU:TCPU; AOS : TOS); virtual;
    Property Dependencies : TDependencies Read FDependencies;
    Property Commands : TCommands Read FCommands;
    Property State : TTargetState Read FTargetState;
    Property TargetType : TTargetType Read FTargetType Write FTargetType;
    Property OSes : TOSes Read FOSes Write FOSes;
    Property CPUs : TCPUs Read FCPUs Write FCPUs;
    Property Mode : TCompilerMode Read FMode Write FMode;
    Property Options : String Read FOptions Write Foptions;
    Property SourceFileName: String Read GetSourceFileName ;
    Property UnitFileName : String Read GetUnitFileName;
    Property ObjectFileName : String Read GetObjectFileName;
    Property RSTFileName : String Read GetRSTFileName;
    Property FPCTarget : String Read FFPCTarget Write FFPCTarget;
    Property Extension : String Read FExtension Write FExtension;
    Property FileType : TFileType Read FFileType Write FFileType;
    Property Directory : String Read FDirectory Write FDirectory;
    Property ResourceStrings : Boolean Read FResourceStrings Write FResourceStrings;
    Property Install : Boolean Read FInstall Write FInstall;
    Property TargetSourceFileName: String Read FTargetSourceFileName;
    Property ObjectPath : TConditionalStrings Read FObjectPath;
    Property UnitPath : TConditionalStrings Read FUnitPath;
    Property IncludePath : TConditionalStrings Read FIncludePath;
    // Events.
    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FBeforeCompile;
    Property AfterCompile : TNotifyEvent Read FAfterCompile Write FAfterCompile;
    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBeforeClean;
    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterClean;
  end;

  { TTargets }

  TTargets = Class(TNamedCollection)
  private
    function GetTargetItem(Index : Integer): TTarget;
    function GetTarget(const AName : String): TTarget;
    procedure SetTargetItem(Index : Integer; const AValue: TTarget);
  Public
    Function AddUnit(Const AUnitName : String) : TTarget;inline;
    Function AddUnit(Const AUnitName : String;const OSes:TOSes) : TTarget;inline;
{$ifdef cpu_only_overloads}
    Function AddUnit(Const AUnitName : String;const CPUs:TCPUs) : TTarget;inline;
{$endif cpu_only_overloads}
    Function AddUnit(Const AUnitName : String;const CPUs:TCPUs;const OSes:TOSes) : TTarget;
    Function AddImplicitUnit(Const AUnitName : String;InstallUnit:boolean=true) : TTarget;inline;
    Function AddImplicitUnit(Const AUnitName : String;const OSes:TOSes;InstallUnit:boolean=true) : TTarget;inline;
    Function AddImplicitUnit(Const AUnitName : String;const CPUs:TCPUs;InstallUnit:boolean=true) : TTarget;inline;
    Function AddImplicitUnit(Const AUnitName : String;const CPUs:TCPUs;const OSes:TOSes;InstallUnit:boolean=true) : TTarget;
    Function AddProgram(Const AProgramName : String) : TTarget;inline;
    Function AddProgram(Const AProgramName : String;const OSes:TOSes) : TTarget;inline;
{$ifdef cpu_only_overloads}
    Function AddProgram(Const AProgramName : String;const CPUs:TCPUs) : TTarget;inline;
{$endif cpu_only_overloads}
    Function AddProgram(Const AProgramName : String;const CPUs:TCPUs;const OSes:TOSes) : TTarget;
    Function AddExampleUnit(Const AUnitName : String) : TTarget;inline;
    Function AddExampleUnit(Const AUnitName : String;const OSes:TOSes) : TTarget;inline;
{$ifdef cpu_only_overloads}
    Function AddExampleUnit(Const AUnitName : String;const CPUs:TCPUs) : TTarget;inline;
{$endif cpu_only_overloads}
    Function AddExampleUnit(Const AUnitName : String;const CPUs:TCPUs;const OSes:TOSes) : TTarget;
    Function AddExampleProgram(Const AProgramName : String) : TTarget;inline;
    Function AddExampleProgram(Const AProgramName : String;const OSes:TOSes) : TTarget;inline;
{$ifdef cpu_only_overloads}
    Function AddExampleProgram(Const AProgramName : String;const CPUs:TCPUs) : TTarget;inline;
{$endif cpu_only_overloads}
    Function AddExampleProgram(Const AProgramName : String;const CPUs:TCPUs;const OSes:TOSes) : TTarget;
    Property Targets[AName : String] : TTarget Read GetTarget; default;
    Property TargetItems[Index : Integer] : TTarget Read GetTargetItem Write SetTargetItem;
  end;

  { TSource }

  TSource = Class(TNamedItem)
  private
    FSourceType : TSourceType;
  Public
    Constructor Create(ACollection : TCollection); override;
    Destructor Destroy; override;
    property SourceType : TSourceType read FSourceType;
  end;

  { TSources }

  TSources = Class(TNamedCollection)
  private
    function GetSourceItem(Index : Integer): TSource;
    procedure SetSourceItem(Index : Integer; const AValue: TSource);
  public
    Function AddDoc(const AFiles : String) : TSource;
    Function AddSrc(const AFiles : String) : TSource;
    Function AddExample(const AFiles : String) : TSource;
    Function AddTest(const AFiles : String) : TSource;
    procedure AddDocFiles(const AFileMask: string; Recursive: boolean = False);
    procedure AddSrcFiles(const AFileMask: string; Recursive: boolean = False);
    procedure AddExampleFiles(const AFileMask: string; Recursive: boolean = False);
    procedure AddTestFiles(const AFileMask: string; Recursive: boolean = False);
    Property SourceItems[Index : Integer] : TSource Read GetSourceItem Write SetSourceItem;default;
  end;

  { TPackage }

  TPackage = Class(TNamedItem)
  private
    FAfterArchive: TNotifyEvent;
    FAfterClean: TNotifyEvent;
    FAfterCompile: TNotifyEvent;
    FAfterInstall: TNotifyEvent;
    FAfterManifest: TNotifyEvent;
    FBeforeArchive: TNotifyEvent;
    FBeforeClean: TNotifyEvent;
    FBeforeCompile: TNotifyEvent;
    FBeforeInstall: TNotifyEvent;
    FBeforeManifest: TNotifyEvent;
    FUnitPath,
    FObjectPath,
    FIncludePath,
    FSourcePath,
    FExamplePath,
    FTestPath,
    FCleanFiles,
    FInstallFiles : TConditionalStrings;
    FDependencies : TDependencies;
    FCPUs: TCPUs;
    FOSes: TOSes;
    FTargetState: TTargetState;
    FTargets: TTargets;
    FSources: TSources;
    FDirectory: String;
    FOptions: String;
    FFileName: String;
    FAuthor: String;
    FLicense: String;
    FExternalURL: String;
    FVersion: TFPVersion;
    FEmail : String;
    FNeedLibC : Boolean;
    FCommands : TCommands;
    FDescriptionFile : String;
    FDescription : String;
    FInstalledChecksum : Cardinal;
    // Cached directory of installed packages
    FUnitDir : String;
    Function GetDescription : string;
    Function GetFileName : string;
    Function GetVersion : string;
    Procedure SetVersion(const V : string);
  Protected
    procedure SetName(const AValue: String);override;
    procedure LoadUnitConfigFromFile(Const AFileName: String);
    procedure SaveUnitConfigToFile(Const AFileName: String;ACPU:TCPU;AOS:TOS);
  Public
    constructor Create(ACollection: TCollection); override;
    destructor destroy; override;
    Function  GetUnitsOutputDir(ACPU:TCPU; AOS : TOS):String;
    Function  GetBinOutputDir(ACPU:TCPU; AOS : TOS) : String;
    Procedure GetCleanFiles(List : TStrings; ACPU:TCPU; AOS : TOS); virtual;
    procedure GetInstallFiles(List: TStrings;Types : TTargetTypes;ACPU:TCPU; AOS : TOS);
    Procedure GetArchiveFiles(List : TStrings; ACPU:TCPU; AOS : TOS); virtual;
    Procedure GetManifest(Manifest : TStrings);
    Property Version : String Read GetVersion Write SetVersion;
    Property FileName : String Read GetFileName Write FFileName;
    Property ExternalURL : String Read FExternalURL Write FExternalURL;
    Property Email : String Read FEmail Write FEmail;
    Property Author : String Read FAuthor Write FAuthor;
    Property License : String Read FLicense Write FLicense;
    Property Directory : String Read FDirectory Write FDirectory;
    Property Description : String Read GetDescription Write FDescription;
    Property DescriptionFile : String Read FDescriptionFile Write FDescriptionFile;
    Property InstalledChecksum : Cardinal Read FInstalledChecksum Write FInstalledChecksum;
    // Compiler options.
    Property OSes : TOSes Read FOSes Write FOSes;
    Property CPUs : TCPUs Read FCPUs Write FCPUs;
    Property NeedLibC : Boolean Read FNeedLibC Write FNeedLibC;
    Property Options: String Read FOptions Write FOptions;
    Property UnitPath : TConditionalStrings Read FUnitPath;
    Property ObjectPath : TConditionalStrings Read FObjectPath;
    Property IncludePath : TConditionalStrings Read FIncludePath;
    Property SourcePath : TConditionalStrings Read FSourcePath;
    Property ExamplePath : TConditionalStrings Read FExamplePath;
    Property TestPath : TConditionalStrings Read FTestPath;
    // Targets and dependencies
    Property InstallFiles : TConditionalStrings Read FInstallFiles;
    Property CleanFiles : TConditionalStrings Read FCleanFiles;
    Property Dependencies : TDependencies Read FDependencies;
    Property Commands : TCommands Read FCommands;
    Property State : TTargetState Read FTargetState;
    Property Targets : TTargets Read FTargets;
    Property Sources : TSources Read FSources;
    Property UnitDir : String Read FUnitDir Write FUnitDir;
    // events
    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FBeforeCompile;
    Property AfterCompile : TNotifyEvent Read FAfterCompile Write FAfterCompile;
    Property BeforeInstall : TNotifyEvent Read FBeforeInstall Write FBeforeInstall;
    Property AfterInstall : TNotifyEvent Read FAfterInstall Write FAfterInstall;
    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBeforeClean;
    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterClean;
    Property BeforeArchive : TNotifyEvent Read FBeforeArchive Write FBeforeArchive;
    Property AfterArchive : TNotifyEvent Read FAfterArchive Write FAfterArchive;
    Property BeforeManifest : TNotifyEvent Read FBeforeManifest Write FBeforeManifest;
    Property AfterManifest : TNotifyEvent Read FAfterManifest Write FAfterManifest;
  end;

  { TPackages }

  TPackages = Class(TNamedCollection)
  private
    function GetPackage(const AName : String): TPackage;
    function GetPackageItem(AIndex : Integer): TPackage;
    procedure SetPackageItem(AIndex : Integer; const AValue: TPackage);
  Public
    Function AddPackage(Const AName : String) : TPackage;
    Property Packages[AName : String] : TPackage Read GetPackage ; Default;
    Property PackageItems[AIndex : Integer] : TPackage Read GetPackageItem Write SetPackageItem;
  end;

  { TCustomDefaults }

  TCustomDefaults = Class(TPersistent)
  Private
    FArchive: String;
    FCompiler: String;
    FCopy: String;
    FMkDir: String;
    FMove: String;
    FOptions: String;
    FCPU: TCPU;
    FOS: TOS;
    FMode : TCompilerMode;
    FCompilerVersion : String;
    FPrefix: String;
    FLocalUnitDir,
    FGlobalUnitDir,
    FBaseInstallDir,
    FUnitInstallDir,
    FBinInstallDir,
    FDocInstallDir,
    FExamplesInstallDir : String;
    FRemove: String;
    FTarget: String;
    FUnixPaths: Boolean;
    FNoFPCCfg: Boolean;
    function GetLocalUnitDir: String;
    function GetGlobalUnitDir: String;
    function GetBaseInstallDir: String;
    function GetBinInstallDir: String;
    function GetCompiler: String;
    function GetDocInstallDir: String;
    function GetExamplesInstallDir: String;
    function GetUnitInstallDir: String;
    procedure SetLocalUnitDir(const AValue: String);
    procedure SetGlobalUnitDir(const AValue: String);
    procedure SetBaseInstallDir(const AValue: String);
    procedure SetCPU(const AValue: TCPU);
    procedure SetOS(const AValue: TOS);
    procedure SetPrefix(const AValue: String);
    procedure SetTarget(const AValue: String);
  Protected
    procedure RecalcTarget;
  Public
    Constructor Create;
    Procedure InitDefaults;
    procedure CompilerDefaults; virtual;
    Procedure LocalInit(Const AFileName : String);
    Procedure LoadFromFile(Const AFileName : String);
    Procedure SaveToFile(Const AFileName : String);
    procedure SaveToStream(S : TStream);virtual;
    procedure LoadFromStream(S : TStream);virtual;
    // Compile Information
    Property Target : String Read FTarget Write SetTarget;
    Property OS : TOS Read FOS Write SetOS;
    Property CPU : TCPU Read FCPU Write SetCPU;
    Property Mode : TCompilerMode Read FMode Write FMode;
    Property UnixPaths : Boolean Read FUnixPaths Write FUnixPaths;
    Property Options : String Read FOptions Write FOptions;    // Default compiler options.
    Property NoFPCCfg : Boolean Read FNoFPCCfg Write FNoFPCCfg;
    // paths etc.
    Property LocalUnitDir : String Read GetLocalUnitDir Write SetLocalUnitDir;
    Property GlobalUnitDir : String Read GetGlobalUnitDir Write SetGlobalUnitDir;
    Property Prefix : String Read FPrefix Write SetPrefix;
    Property BaseInstallDir : String Read GetBaseInstallDir Write SetBaseInstallDir;
    Property UnitInstallDir : String Read GetUnitInstallDir Write FUnitInstallDir;
    Property BinInstallDir : String Read GetBinInstallDir Write FBinInstallDir;
    Property DocInstallDir : String Read GetDocInstallDir Write FDocInstallDir;
    Property ExamplesInstallDir : String Read GetExamplesInstallDir Write FExamplesInstallDir;
    // Command tools. If not set, internal commands  will be used.
    Property Compiler : String Read GetCompiler Write FCompiler; // Compiler. Defaults to fpc
    Property Copy : String Read FCopy Write FCopy;             // copy $(FILES) to $(DEST)
    Property Move : String Read FMove Write FMove;             // Move $(FILES) to $(DEST)
    Property Remove : String Read FRemove Write FRemove;       // Delete $(FILES)
    Property MkDir : String Read FMkDir write FMkDir;          // Make $(DIRECTORY)
    Property Archive : String Read FArchive Write FArchive;    // zip $(ARCHIVE) $(FILESORDIRS)
  end;

  { TBasicDefaults }

  TBasicDefaults = Class(TCustomDefaults)
  end;

  { TFPCDefaults }

  TFPCDefaults = Class(TCustomDefaults)
  public
    procedure CompilerDefaults; override;
  end;

  { TBuildEngine }

  TBuildEngine = Class(TComponent)
  private
    // general variables
    FCompiler : String;
    FStartDir : String;
    FForceCompile : Boolean;
    FListMode : Boolean;
{$ifdef HAS_UNIT_ZIPPER}
    FZipFile: TZipper;
{$endif HAS_UNIT_ZIPPER}
    FExternalPackages : TPackages;
    // Logging
    FLogPrefix : String;
    // Events
    FOnLog: TLogEvent;
    FAfterArchive: TNotifyEvent;
    FAfterClean: TNotifyEvent;
    FAfterCompile: TNotifyEvent;
    FAfterInstall: TNotifyEvent;
    FAfterManifest: TNotifyEvent;
    FBeforeArchive: TNotifyEvent;
    FBeforeClean: TNotifyEvent;
    FBeforeCompile: TNotifyEvent;
    FBeforeInstall: TNotifyEvent;
    FBeforeManifest: TNotifyEvent;
  Protected
    Procedure Error(const Msg : String);
    Procedure Error(const Fmt : String; const Args : Array of const);
    // Internal copy/delete/move/archive/mkdir files
    Function  SysDirectoryExists(const ADir:string):Boolean;
    Function  SysFileExists(const AFileName:string):Boolean;
    Procedure SysCopyFile(Const Src,Dest : String); virtual;
    Procedure SysMoveFile(Const Src,Dest : String); virtual;
    Procedure SysDeleteFile(Const AFileName : String); virtual;
    Procedure SysArchiveFiles(List : TStrings; Const AFileName : String); virtual;
    procedure LogIndent;
    procedure LogUnIndent;
    Procedure Log(Level : TVerboseLevel; Const Msg : String);
    Procedure Log(Level : TVerboseLevel; Const Fmt : String; const Args : Array Of Const);
    Procedure EnterDir(ADir : String);
    Function GetCompiler : String;
    Function InstallPackageFiles(APAckage : TPackage; tt : TTargetType; Const Dest : String):Boolean;
    Function FileNewer(const Src,Dest : String) : Boolean;
    Procedure LogSearchPath(const ASearchPathName:string;Path:TConditionalStrings; ACPU:TCPU;AOS:TOS);
    Function FindFileInPath(Path:TConditionalStrings; AFileName:String; var FoundPath:String;ACPU:TCPU;AOS:TOS):Boolean;

    //package commands
    Procedure ResolveFileNames(APackage : TPackage; ACPU:TCPU;AOS:TOS;DoChangeDir:boolean=true);
    function  GetUnitDir(APackage:TPackage):String;
    procedure AddDependencyIncludePaths(L:TStrings;ATarget: TTarget);
    procedure AddDependencyUnitPaths(L:TStrings;APackage: TPackage);
  Public
    Constructor Create(AOwner : TComponent); override;
    destructor Destroy;override;
    // Public Copy/delete/Move/Archive/Mkdir Commands.
    Procedure ExecuteCommand(const Cmd,Args : String; IgnoreError : Boolean = False); virtual;
    Procedure CmdCopyFiles(List : TStrings; Const DestDir : String);
    Procedure CmdCreateDir(const DestDir : String);
    Procedure CmdMoveFiles(List : TStrings; Const DestDir : String);
    Procedure CmdDeleteFiles(List : TStrings);
    Procedure CmdArchiveFiles(List : TStrings; Const ArchiveFile : String);
    Procedure ExecuteCommands(Commands : TCommands; At : TCommandAt);
    // Dependency commands
    Function  DependencyOK(ADependency : TDependency) : Boolean;
    // Target commands
    Function  GetCompilerCommand(APackage : TPackage; ATarget : TTarget) : String;
    Function  TargetOK(ATarget : TTarget) : Boolean;
    Function  NeedsCompile(APackage:TPackage; ATarget : TTarget) : Boolean;
    Procedure Compile(APackage:TPackage; ATarget : TTarget);  virtual;
    Procedure MaybeCompile(APackage:TPackage; ATarget: TTarget);
    Procedure CompileDependencies(APackage:TPackage; ATarget: TTarget);
    // Package commands
{    Function  GetPackageDir(APackage : TPackage; AbsolutePath : Boolean = False) : String;
    Function  GetUnitsOutputDir(APackage : TPackage; AbsolutePath : Boolean = False) : String;
    Function  GetBinOutputDir(APackage : TPackage; AbsolutePath : Boolean = False) : String;  }
    Function  PackageOK(APackage : TPackage) : Boolean; virtual;
    Procedure DoBeforeCompile(APackage : TPackage);virtual;
    Procedure DoAfterCompile(APackage : TPackage);virtual;
    Procedure DoBeforeInstall(APackage : TPackage);virtual;
    Procedure DoAfterInstall(APackage : TPackage);virtual;
    Procedure DoBeforeArchive(APackage : TPackage);virtual;
    Procedure DoAfterArchive(APackage : TPackage);virtual;
    Procedure DoBeforeClean(APackage : TPackage);virtual;
    Procedure DoAfterClean(APackage : TPackage);virtual;
    Function  NeedsCompile(APackage : TPackage) : Boolean; virtual;
    Procedure Compile(APackage : TPackage);
    Procedure MaybeCompile(APackage:TPackage);
    Procedure Install(APackage : TPackage);
    Procedure Archive(APackage : TPackage);
    Procedure Manifest(APackage : TPackage);
    Procedure Clean(APackage : TPackage);
    Procedure CompileDependencies(APackage : TPackage);
    Function  CheckExternalPackage(Const APackageName : String):TPackage;
    procedure CreateOutputDir(APackage: TPackage);
    // Packages commands
    Procedure Compile(Packages : TPackages);
    Procedure Install(Packages : TPackages);
    Procedure Archive(Packages : TPackages);
    procedure Manifest(Packages: TPackages);
    Procedure Clean(Packages : TPackages);
    Property ListMode : Boolean Read FListMode Write FListMode;
    Property ForceCompile : Boolean Read FForceCompile Write FForceCompile;
    Property ExternalPackages: TPackages Read FExternalPackages;
    // Events
    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FBeforeCompile;
    Property AfterCompile : TNotifyEvent Read FAfterCompile Write FAfterCompile;
    Property BeforeInstall : TNotifyEvent Read FBeforeInstall Write FBeforeInstall;
    Property AfterInstall : TNotifyEvent Read FAfterInstall Write FAfterInstall;
    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBeforeClean;
    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterClean;
    Property BeforeArchive : TNotifyEvent Read FBeforeArchive Write FBeforeArchive;
    Property AfterArchive : TNotifyEvent Read FAfterArchive Write FAfterArchive;
    Property BeforeManifest : TNotifyEvent Read FBeforeManifest Write FBeforeManifest;
    Property AfterManifest : TNotifyEvent Read FAfterManifest Write FAfterManifest;
    Property OnLog : TLogEvent Read FOnLog Write FOnlog;
  end;

  { TCustomInstaller }

  TCustomInstaller = Class(TComponent)
  private
    FBuildEngine: TBuildEngine;
    FPackages: TPackages;
    FRunMode: TRunMode;
    FListMode : Boolean;
    FLogLevels : TVerboseLevels;
  Protected
    Procedure Log(Level : TVerboseLevel; Const Msg : String);
    Procedure CreatePackages; virtual;
    Procedure CheckPackages; virtual;
    Procedure CreateBuildEngine; virtual;
    Procedure Error(const Msg : String);
    Procedure Error(const Fmt : String; Args : Array of const);
    Procedure AnalyzeOptions;
    Procedure Usage(const FMT : String; Args : Array of const);
    Procedure Compile(Force : Boolean); virtual;
    Procedure Clean; virtual;
    Procedure Install; virtual;
    Procedure Archive; virtual;
    Procedure Manifest; virtual;
    Property BuildEngine : TBuildEngine Read FBuildEngine;
  Public
    Constructor Create(AOwner : TComponent); virtual;
    Destructor destroy; override;
    Function AddPackage(Const AName : String) : TPackage;
    Function Run : Boolean;
    //files in package
    Property Packages : TPackages Read FPackages;
    Property RunMode : TRunMode Read FRunMode;
    Property ListMode : Boolean Read FListMode;
  end;

  { TFPCInstaller }
  TFPCInstaller = class(TCustomInstaller)
  public
    Constructor Create(AOwner : TComponent); override;
  end;

  { TBasicInstaller }
  TBasicInstaller = class(TCustomInstaller)
    Constructor Create(AOwner : TComponent); override;
  end;

  TReplaceFunction = Function (Const AName,Args : String) : String of Object;

  { TValueItem }

  TValueItem = Class(TObject)
    FValue : String;
    Constructor Create(AValue : String);
  end;

  { TFunctionItem }

  TFunctionItem = Class(TObject)
    FFunc : TReplaceFunction;
    Constructor Create(AFunc : TReplaceFunction);
  end;

  { TDictionary }

  TDictionary = Class(TComponent)
    FList : TStringList;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy;override;
    Procedure AddVariable(Const AName,Value : String);
    Procedure AddFunction(Const AName : String; FReplacement : TReplaceFunction);
    Procedure RemoveItem(Const AName : String);
    Function GetValue(Const AName : String) : String;
    Function GetValue(Const AName,Args : String) : String; virtual;
    Function ReplaceStrings(Const ASource : String) : String; virtual;
  end;

  ECollectionError = Class(Exception);
  EDictionaryError = Class(Exception);
  EInstallerError = Class(Exception);

  TInstallerClass = Class of TCustomInstaller;
  TDictionaryClass = Class of TDictionary;

Type
  TArchiveEvent = Procedure (Const AFileName : String; List : TStrings) of Object;
  TArchiveProc = Procedure (Const AFileName : String; List : TStrings);

Var
  DictionaryClass : TDictionaryClass = TDictionary;
  OnArchiveFiles : TArchiveEvent = Nil;
  ArchiveFilesProc : TArchiveProc = Nil;

  Defaults : TCustomDefaults; // Set by installer.
  Dictionary : TDictionary;


Function CurrentOS : String;
Function CurrentCPU : String;

Function Installer(InstallerClass: TInstallerClass) : TCustomInstaller; overload;
Function Installer : TCustomInstaller; overload;

Function OSToString(OS: TOS) : String;
Function OSesToString(OSes: TOSes) : String;
Function CPUToString(CPU: TCPU) : String;
Function CPUSToString(CPUS: TCPUS) : String;
Function StringToOS(const S : String) : TOS;
Function OSesToString(const S : String) : TOSes;
Function StringToCPU(const S : String) : TCPU;
Function StringToCPUS(const S : String) : TCPUS;
Function ModeToString(Mode: TCompilerMode) : String;
Function StringToMode(const S : String) : TCompilerMode;
Function MakeTargetString(CPU : TCPU;OS: TOS) : String;
Procedure StringToCPUOS(const S : String; Var CPU : TCPU; Var OS: TOS);
Function FixPath (const APath : String) : String;
Procedure ChangeDir(const APath : String);
Function Substitute(Const Source : String; Macros : Array of string) : String;
Procedure SplitCommand(Const Cmd : String; Var Exe,Options : String);

Implementation

uses typinfo;

ResourceString
  SErrInvalidCPU        = 'Invalid CPU name "%s"';
  SErrInvalidOS         = 'Invalid OS name "%s"';
  SErrInvalidMode       = 'Invalid compiler mode "%s"';
  SErrInvalidTarget     = 'Invalid compiler target "%s"';
  SErrNameExists        = 'Name "%s" already exists in the collection.';
  SErrNoSuchName        = 'Could not find item with name "%s" in the collection.';
  SErrInValidArgument   = 'Invalid command-line argument at position %d: %s';
  SErrNeedArgument      = 'Option at position %d (%s) needs an argument';
  SErrNoPackagesDefined = 'No action possible: No packages were defined.';
  SErrInstaller         = 'The installer encountered the following error:';
  SErrDepUnknownTarget  = 'Unknown target in dependencies for %s: %s';
  SErrExternalCommandFailed = 'External command "%s" failed with exit code %d';
  SErrCreatingDirectory = 'Failed to create directory "%s"';
  SErrDeletingFile      = 'Failed to delete file "%s"';
  SErrMovingFile        = 'Failed to move file "%s" to "%s"';
  SErrCopyingFile       = 'Failed to copy file "%s" to "%s"';
  SErrChangeDirFailed   = 'Failed to enter directory "%s"';
  SErrInvalidArgumentToSubstitute = 'Invalid number of arguments to Substitute';
  SErrNoArchiveSupport  = 'This binary contains no archive support. Please recompile with archive support';
  SErrNoDictionaryItem  = 'No item called "%s" in the dictionary';
  SErrNoDictionaryValue = 'The item "%s" in the dictionary is not a value';
  SErrNoDictionaryFunc  = 'The item "%s" in the dictionary is not a function';
  SErrInvalidFPCInfo    = 'Compiler returns invalid information, check if fpc -iV works';
  SErrDependencyNotFound = 'Could not find unit directory for dependency package "%s"';
  SErrAlreadyInitialized = 'Installer can only be initialized once';
  SErrInvalidState      = 'Invalid state for target %s';

  SWarnCircularTargetDependency = 'Warning: Circular dependency detected when compiling target %s with target %s';
  SWarnCircularPackageDependency = 'Warning: Circular dependency detected when compiling package %s with package %s';
  SWarnFailedToSetTime    = 'Warning: Failed to set timestamp on file "%s"';
  SWarnFailedToGetTime    = 'Warning: Failed to get timestamp from file "%s"';
  SWarnFileDoesNotExist   = 'Warning: File "%s" does not exist';
  SWarnAttemptingToCompileNonNeutralTarget = 'Warning: Attempting to compile non-neutral target %s';
  SWarnSourceFileNotFound  = 'Warning: Source file "%s" not found for %s';
  SWarnIncludeFileNotFound = 'Warning: Include file "%s" not found for %s';
  SWarnDepUnitNotFound     = 'Warning: Dependency on unit %s is not supported for %s';

  SInfoCompilingPackage   = 'Compiling package %s';
  SInfoCompilingTarget    = 'Compiling target %s';
  SInfoExecutingCommand   = 'Executing command "%s %s"';
  SInfoCreatingOutputDir  = 'Creating output dir "%s"';
  SInfoInstallingPackage  = 'Installing package %s';
  SInfoArchivingPackage   = 'Archiving package %s in "%s"';
  SInfoCleaningPackage    = 'Cleaning package %s';
  SInfoManifestPackage    = 'Creating manifest for package %s';
  SInfoCopyingFile        = 'Copying file "%s" to "%s"';
  SInfoSourceNewerDest    = 'Source file "%s" (%s) is newer than destination "%s" (%s).';

  SDbgComparingFileTimes    = 'Comparing file "%s" time "%s" to "%s" time "%s".';
  SDbgCompilingDependenciesOfTarget = 'Compiling dependencies of target %s';
  SDbgResolvingSourcesOfTarget = 'Resolving filenames of target %s for %s';
  SDbgResolvedSourceFile    = 'Resolved source file %s to "%s"';
  SDbgResolvedIncludeFile   = 'Resolved include file %s to "%s"';
  SDbgOutputNotYetAvailable = 'Output file %s not available';
  SDbgDependencyOnUnit      = 'Dependency of %s on unit %s';
  SDbgDependencyUnitRecompiled = 'Dependent unit %s is being recompiled';
  SDbgMustCompile           = 'Must compile %s';
  SDbgSkippingTargetWrongCPU = 'Skipping target %s, different CPU (%s)';
  SDbgSkippingTargetWrongOS  = 'Skipping target %s, different OS (%s)';
  SDbgTargetIsNotAUnitOrProgram = 'Skipping Target %s, not an unit or program';
  SDbgConsideringTarget     = 'Considering target %s';
  SDbgConsideringPackage    = 'Considering package %s';
  SDbgExternalDependency    = 'External dependency %s found in "%s"';
  SDbgBuildEngineArchiving  = 'Build engine archiving';
  SDbgBuildEngineGenerateManifests = 'Build engine generating manifests';
  SDbgBuildEngineCleaning   = 'Build engine cleaning';
  SDbgGenerating            = 'Generating "%s"';
  SDbgLoading               = 'Loading "%s"';
  SDbgFound                 = 'Found';
  SDbgNotFound              = 'Not Found';
  SDbgDirectoryExists       = 'Directory "%s" %s';
  SDbgFileExists            = 'File "%s" %s';
  SDbgArchivingFile         = 'Archiving "%s"';
  SDbgSearchPath            = 'Using %s path "%s"';
  SDbgEnterDir              = 'Entering directory "%s"';
  SDbgPackageChecksumChanged = 'Dependent package %s is modified';

  // Help messages for usage
  SValue              = 'Value';
  SHelpUsage          = 'Usage: %s command [options]';
  SHelpCommand        = 'Where command is one of the following:';
  SHelpCompile        = 'Compile all units in the package(s).';
  SHelpBuild          = 'Build all units in the package(s).';
  SHelpInstall        = 'Install all units in the package(s).';
  SHelpClean          = 'Clean (remove) all units in the package(s).';
  SHelpArchive        = 'Create archive (zip) with all units in the package(s).';
  SHelpHelp           = 'This message.';
  SHelpManifest       = 'Create a manifest suitable for import in repository.';
  SHelpCmdOptions     = 'Where options is one or more of the following:';
  SHelpCPU            = 'Compile for indicated CPU.';
  SHelpOS             = 'Compile for indicated OS';
  SHelpTarget         = 'Compile for indicated target';
  SHelpList           = 'list commands instead of actually executing them.';
  SHelpPrefix         = 'Use indicated prefix directory for all commands.';
  SHelpNoFPCCfg       = 'Compiler will not use fpc.cfg';
  SHelpBaseInstallDir = 'Use indicated directory as base install dir.';
  SHelpLocalUnitDir   = 'Use indicated directory as local (user) unit dir.';
  SHelpGlobalUnitDir  = 'Use indicated directory as global unit dir.';
  SHelpCompiler       = 'Use indicated binary as compiler';
  SHelpConfig         = 'Use indicated config file when compiling.';
  SHelpVerbose        = 'Be verbose when working.';


Const
  // Keys for Defaults file. Do not localize.
  KeyCompiler = 'Compiler';
  KeyArchive  = 'Archive';
  KeyCopy     = 'Copy';
  KeyMkDir    = 'MkDir';
  KeyMove     = 'Move';
  KeyRemove   = 'Remove';
  KeyOptions  = 'Options';
  KeyCPU      = 'CPU';
  KeyOS       = 'OS';
  KeyMode     = 'Mode';
  KeyPrefix   = 'Prefix';
  KeyTarget   = 'Target';
  KeyNoFPCCfg = 'NoFPCCfg';
  KeyLocalUnitDir       = 'LocalUnitDir';
  KeyGlobalUnitDir      = 'GlobalUnitDir';
  KeyBaseInstallDir     = 'BaseInstallDir';
  KeyUnitInstallDir     = 'UnitInstallDir';
  KeyBinInstallDir      = 'BinInstallDir';
  KeyDocInstallDir      = 'DocInstallDir';
  KeyExamplesInstallDir = 'ExamplesInstallDir';
  // Keys for unit config
  KeyName     = 'Name';
  KeyVersion  = 'Version';
  KeyChecksum = 'Checksum';
  KeyNeedLibC = 'NeedLibC';
  KeyDepends  = 'Depends';

{****************************************************************************
                                Helpers
****************************************************************************}


Function QuoteXML(S : String) : string;

  Procedure W(Var J : Integer; Var R : String; T : String);
  Var
    I: integer;
  begin
    If J+Length(T)>Length(R) then
      SetLength(R,J+Length(T));
    For I:=1 to Length(t) do
      begin
      R[J]:=T[i];
      If I<Length(T) then
        Inc(J);
      end;
  end;

const
  QuotStr = '&quot;';
  AmpStr = '&amp;';
  ltStr = '&lt;';
  gtStr = '&gt;';
Var
  I,J : Integer;
begin
  SetLength(Result,Length(S));
  J:=0;
  For I:=1 to Length(S) do
    begin
    Inc(J);
    case S[i] of
      '"': W(j,Result,QuotStr);
      '&': W(J,Result,AmpStr);
      '<': W(J,Result,ltStr);
      '>': W(J,Result,gtStr);
      // Escape whitespace using CharRefs to be consistent with W3 spec X 3.3.3
       #9: w(J,Result,'&#x9;');
{      #10: wrtStr('&#xA;');
       #13: wrtStr('&#xD;');}
    else
      Result[J]:=S[i];
    end;
    If (J=Length(Result)) and (I<Length(S)) then
      SetLength(Result,J+Length(S)-I);
    end;
  If J<>Length(Result) then
    SetLength(Result,J);
end;


function maybequoted(const s:string):string;
const
  {$IFDEF MSWINDOWS}
    FORBIDDEN_CHARS = ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
                       '{', '}', '''', '`', '~'];
  {$ELSE}
    FORBIDDEN_CHARS = ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
                       '{', '}', '''', ':', '\', '`', '~'];
  {$ENDIF}
var
  s1 : string;
  i  : integer;
  quoted : boolean;
begin
  quoted:=false;
  s1:='"';
  for i:=1 to length(s) do
   begin
     case s[i] of
       '"' :
         begin
           quoted:=true;
           s1:=s1+'\"';
         end;
       ' ',
       #128..#255 :
         begin
           quoted:=true;
           s1:=s1+s[i];
         end;
       else begin
         if s[i] in FORBIDDEN_CHARS then
           quoted:=True;
         s1:=s1+s[i];
       end;
     end;
   end;
  if quoted then
    maybequoted:=s1+'"'
  else
    maybequoted:=s;
end;


procedure ReadIniFile(Const AFileName: String;L:TStrings);
Var
  F : TFileStream;
  Line : String;
  I,P,PC : Integer;
begin
  F:=TFileStream.Create(AFileName,fmOpenRead);
  Try
    L.LoadFromStream(F);
    // Fix lines.
    For I:=L.Count-1 downto 0 do
      begin
        Line:=L[I];
        P:=Pos('=',Line);
        PC:=Pos(';',Line);  // Comment line.
        If (P=0) or ((PC<>0) and (PC<P)) then
          L.Delete(I)
        else
          L[i]:=Trim(System.Copy(Line,1,P-1)+'='+Trim(System.Copy(Line,P+1,Length(Line)-P)));
      end;
  Finally
    F.Free;
  end;
end;


// Callback for Sysutils getapplicationname.
Function GetFPMakeName : String;

begin
  Result:='fpmake';
end;


Function CurrentOS : String;

begin
  Result:=OSToString(Defaults.OS);
end;

Function CurrentCPU : String;

begin
  Result:=CPUToString(Defaults.CPU);
end;

Function OSToString(OS: TOS) : String;

begin
  Result:=LowerCase(GetenumName(TypeInfo(TOS),Ord(OS)));
end;

Function OSesToString(OSes: TOSes) : String;

begin
  Result:=LowerCase(SetToString(PtypeInfo(TypeInfo(TOSes)),Integer(OSes),False));
end;

Function CPUToString(CPU: TCPU) : String;

begin
  Result:=LowerCase(GetenumName(TypeInfo(TCPU),Ord(CPU)));
end;

Function CPUSToString(CPUS: TCPUS) : String;

begin
  Result:=LowerCase(SetToString(PTypeInfo(TypeInfo(TCPUS)),Integer(CPUS),False));
end;

Function StringToOS(const S : String) : TOS;

Var
  I : Integer;

begin
  I:=GetEnumValue(TypeInfo(TOS),S);
  if (I=-1) then
    Raise EInstallerError.CreateFmt(SErrInvalidOS,[S]);
  Result:=TOS(I);
end;


Function OSesToString(const S : String) : TOSes;

begin
  Result:=TOSes(StringToSet(PTypeInfo(TypeInfo(TOSes)),S));
end;

Function StringToCPU(const S : String) : TCPU;

Var
  I : Integer;

begin
  I:=GetEnumValue(TypeInfo(TCPU),S);
  if (I=-1) then
    Raise EInstallerError.CreateFmt(SErrInvalidCPU,[S]);
  Result:=TCPU(I);
end;

Function StringToCPUS(const S : String) : TCPUS;

begin
  Result:=TCPUS(StringToSet(PTypeInfo(TypeInfo(TCPUS)),S));
end;

Function ModeToString(Mode: TCompilerMode) : String;

begin
  Result:=LowerCase(GetenumName(TypeInfo(TCompilerMode),Ord(Mode)));
  Delete(Result,1,2);
end;

Function StringToMode(const S : String) : TCompilerMode;

Var
  I : Integer;

begin
  I:=GetEnumValue(TypeInfo(TCompilerMode),S);
  if (I=-1) then
    Raise EInstallerError.CreateFmt(SErrInvalidMode,[S]);
  Result:=TCompilerMode(I);
end;


Function MakeTargetString(CPU : TCPU;OS: TOS) : String;

begin
  Result:=CPUToString(CPU)+'-'+OSToString(OS);
end;

Procedure StringToCPUOS(const S : String; Var CPU : TCPU; Var OS: TOS);

Var
  P : integer;

begin
  P:=Pos('-',S);
  If (P=0) then
    Raise EInstallerError.CreateFmt(SErrInvalidTarget,[S]);
  CPU:=StringToCPU(Copy(S,1,P-1));
  OS:=StringToOs(Copy(S,P+1,Length(S)-P));
end;


Procedure ResolveDependencies(L : TDependencies; P : TNamedCollection);
Var
  I,J : Integer;
  C : TDependency;
begin
  If Assigned(L) then
    For I:=0 to L.Count-1 do
      begin
        C:=L[i];
        if C.DependencyType in [depPackage,depUnit] then
          begin
            J:=P.IndexOfName(C.Value);
            If J<>-1 then
              C.Target:=P.Items[J];
          end;
      end;
end;


function AddConditionalStrings(Dest : TStrings; Src : TConditionalStrings;ACPU:TCPU;AOS:TOS; Const APrefix : String='') : Integer ;
Var
  I : Integer;
  C : TConditionalString;
  S : String;
begin
  Result:=0;
  Dictionary.AddVariable('CPU',CPUToString(ACPU));
  Dictionary.AddVariable('OS',OSToString(AOS));
  For I:=0 to Src.Count-1 do
    begin
      C:=Src[I];
      if (ACPU in C.CPUs) and (AOS in C.OSes) then
        begin
          If (APrefix<>'') then
            S:=APrefix+C.Value
          else
            S:=C.Value;
          Dest.Add(Dictionary.ReplaceStrings(S));
          Inc(Result);
        end;
    end;
end;


function FileListToString(List : TStrings; const APrefix : String) : String;
Var
  I : integer;
  S : String;
begin
  Result:='';
  For I:=0 to List.Count-1 do
    begin
      If (I>0) then
        Result:=Result+' ';
      S:=APrefix+List[i];
      If (Pos(' ',S)<>0) then
        S:='"'+S+'"';
      Result:=Result+S;
    end;
end;


function FixPath (const APath : String) : String;
Var
  P : PChar;
begin
  Result:=APath;
  If (result<>'') then
    begin
      P:=PChar(Result);
      While (P^<>#0) do
        begin
          If P^ in ['/','\'] then
            P^:=PathDelim;
          Inc(P);
        end;
    end;
end;


procedure ChangeDir(const APath : String);
begin
  if Not SetCurrentDir(APath) then
    Raise EInstallerError.CreateFmt(SErrChangeDirFailed,[APath]);
end;


procedure SearchFiles(const AFileName: string; Recursive: boolean; var List: TStrings);

  procedure AddRecursiveFiles(const SearchDir, FileMask: string; Recursive: boolean);
  const
    IgnoreCase = {$ifdef UNIX}False{$else}True{$endif};
  var
    Info : TSearchRec;
  begin
    if FindFirst(SearchDir+'*',faAnyFile and faDirectory,Info)=0 then
    begin
      repeat
          if ((Info.Attr and faDirectory) = faDirectory) and (Info.Name <> '.') and (Info.Name <> '..') and (Recursive) then
            AddRecursiveFiles(SearchDir + Info.Name + PathDelim, FileMask, Recursive);
          if ((Info.Attr and faDirectory) <> faDirectory) and IsWild(Info.Name, FileMask, IgnoreCase) then
            List.Add(SearchDir + Info.Name);
      until FindNext(Info)<>0;
    end;
    FindClose(Info);
  end;

var
  CurrDir,
  BasePath: string;
  i: integer;
begin
  BasePath := ExtractFilePath(ExpandFileName(AFileName));
  AddRecursiveFiles(BasePath, ExtractFileName(AFileName), Recursive);

  CurrDir:=GetCurrentDir;
  for i := 0 to Pred(List.Count) do
    List[i] := ExtractRelativepath(IncludeTrailingPathDelimiter(CurrDir), List[i]);
end;


procedure SplitCommand(const Cmd : String; var Exe, Options : String);

Const
  WhiteSpace = [#9,#10,#13,' '];
  QuoteChars = ['''','"'];

Var
  I : Integer;
  InQuote : Boolean;
  LastQuote : Char;
  S : String;

begin
  S:=Trim(Cmd);
  InQuote:=False;
  LastQuote:=#0;
  I:=1;
  While (I<=Length(S)) and (Inquote or not (S[I] in whiteSpace)) do
    begin
    If S[i] in QuoteChars then
      begin
      InQuote:=Not (S[i]=LastQuote);
      If InQuote then
         LastQuote:=S[i]
       else
         LastQuote:=#0;
      end;
    Inc(I);
    end;
  Exe:=Copy(S,1,I-1);
  Delete(S,1,I);
  Options:=Trim(S);
end;


{$ifdef HAS_UNIT_PROCESS}
function GetCompilerInfo(const ACompiler,AOptions:string):string;
const
  BufSize = 1024;
var
  S: TProcess;
  Buf: array [0..BufSize - 1] of char;
  Count: longint;
begin
  S:=TProcess.Create(Nil);
  S.Commandline:=ACompiler+' '+AOptions;
  S.Options:=[poUsePipes];
  S.execute;
  Count:=s.output.read(buf,BufSize);
  S.Free;
  SetLength(Result,Count);
  Move(Buf,Result[1],Count);
end;
{$endif HAS_UNIT_PROCESS}


{****************************************************************************
                                TNamedItem
****************************************************************************}

procedure TNamedItem.SetName(const AValue: String);

begin
  if FName=AValue then exit;
  With TNamedCollection(Collection) do
    If UniqueNames then
      If (IndexOfName(AVAlue)<>-1) then
        Raise ECollectionError.CreateFmt(SErrNameExists,[AValue]);
  FName:=AValue;
end;


{****************************************************************************
                                TNamedCollection
****************************************************************************}

function TNamedCollection.IndexOfName(const AName: String): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(TNamedItem(Items[Result]).FName,AName)<>0) do
    Dec(Result);
end;

function TNamedCollection.ItemByName(const AName: String): TNamedItem;

Var
  I : Integer;

begin
  I:=IndexOfName(AName);
  If (I=-1) Then
    Raise ECollectionError.CreateFmt(SErrNoSuchName,[AName]);
  Result:=TNamedItem(Items[i]);
end;


{****************************************************************************
                             TNamedItemList
****************************************************************************}

function TNamedItemList.GetNamedItem(Index : Integer): TNamedItem;
begin
  Result:=TNamedItem(Items[Index]);
end;


procedure TNamedItemList.SetNamedItem(Index : Integer; const AValue: TNamedItem);
begin
  Items[Index]:=AValue;
end;


function TNamedItemList.IndexOfName(const AName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetNamedItem(Result).Name,AName)<>0) do
    Dec(Result);
end;


function TNamedItemList.ItemByName(const ANAme: String): TNamedItem;
Var
  I : Integer;
begin
  I:=IndexOfName(AName);
  If (I=-1) Then
    Raise ECollectionError.CreateFmt(SErrNoSuchName,[AName]);
  Result:=TNamedItem(Items[i]);
end;


{****************************************************************************
                                TTargets
****************************************************************************}

function TTargets.GetTargetItem(Index : Integer): TTarget;
begin
  Result:=TTarget(Items[Index]);
end;


function TTargets.GetTarget(const AName : String): TTarget;
begin
  Result:=TTarget(ItemByName(AName));
end;


procedure TTargets.SetTargetItem(Index : Integer; const AValue: TTarget);
begin
  Items[Index]:=AValue;
end;


Function TTargets.AddUnit(Const AUnitName : String) : TTarget;
begin
  Result:=AddUnit(AUnitName,AllCPUs,AllOSes);
end;


Function TTargets.AddUnit(Const AUnitName : String;const OSes:TOSes) : TTarget;
begin
  Result:=AddUnit(AUnitName,AllCPUs,OSes);
end;


{$ifdef cpu_only_overloads}
Function TTargets.AddUnit(Const AUnitName : String;const CPUs:TCPUs) : TTarget;
begin
  Result:=AddUnit(AUnitName,CPUs,AllOSes);
end;
{$endif cpu_only_overloads}


Function TTargets.AddUnit(Const AUnitName : String;const CPUs:TCPUs;const OSes:TOSes) : TTarget;
begin
  Result:=Add as TTarget;
  Result.Name:=AUnitName;
  Result.TargetType:=TTUnit;
  Result.CPUs:=CPUs;
  Result.OSes:=OSes;
end;


Function TTargets.AddImplicitUnit(Const AUnitName : String;InstallUnit:boolean=true) : TTarget;
begin
  Result:=AddImplicitUnit(AUnitName,AllCPUs,AllOSes,InstallUnit);
end;


Function TTargets.AddImplicitUnit(Const AUnitName : String;const OSes:TOSes;InstallUnit:boolean=true) : TTarget;
begin
  Result:=AddImplicitUnit(AUnitName,AllCPUs,OSes,InstallUnit);
end;


Function TTargets.AddImplicitUnit(Const AUnitName : String;const CPUs:TCPUs;InstallUnit:boolean=true) : TTarget;
begin
  Result:=AddImplicitUnit(AUnitName,CPUs,AllOSes,InstallUnit);
end;


Function TTargets.AddImplicitUnit(Const AUnitName : String;const CPUs:TCPUs;const OSes:TOSes;InstallUnit:boolean=true) : TTarget;
begin
  Result:=Add as TTarget;
  Result.Name:=AUnitName;
  Result.CPUs:=CPUs;
  Result.OSes:=OSes;
  if InstallUnit then
    Result.TargetType:=TTImplicitUnit
  else
    Result.TargetType:=TTCleanOnlyUnit;
end;


Function TTargets.AddProgram(Const AProgramName : String) : TTarget;
begin
  Result:=AddProgram(AProgramName,AllCPUs,AllOSes);
end;


Function TTargets.AddProgram(Const AProgramName : String;const OSes:TOSes) : TTarget;
begin
  Result:=AddProgram(AProgramName,AllCPUs,OSes);
end;


{$ifdef cpu_only_overloads}
Function TTargets.AddProgram(Const AProgramName : String;const CPUs:TCPUs) : TTarget;
begin
  Result:=AddProgram(AProgramName,CPUs,AllOSes);
end;
{$endif cpu_only_overloads}


Function TTargets.AddProgram(Const AProgramName : String;const CPUs:TCPUs;const OSes:TOSes) : TTarget;
begin
  Result:=Add as TTarget;
  Result.Name:=AProgramName;
  Result.CPUs:=CPUs;
  Result.OSes:=OSes;
  Result.TargetType:=ttProgram;
end;


Function TTargets.AddExampleUnit(Const AUnitName : String) : TTarget;
begin
  Result:=AddExampleUnit(AUnitName,AllCPUs,AllOSes);
end;


Function TTargets.AddExampleUnit(Const AUnitName : String;const OSes:TOSes) : TTarget;
begin
  Result:=AddExampleUnit(AUnitName,AllCPUs,OSes);
end;


{$ifdef cpu_only_overloads}
Function TTargets.AddExampleUnit(Const AUnitName : String;const CPUs:TCPUs) : TTarget;
begin
  Result:=AddExampleUnit(AUnitName,CPUs,AllOSes);
end;
{$endif cpu_only_overloads}


Function TTargets.AddExampleUnit(Const AUnitName : String;const CPUs:TCPUs;const OSes:TOSes) : TTarget;
begin
  Result:=Add as TTarget;
  Result.Name:=AUnitName;
  Result.CPUs:=CPUs;
  Result.OSes:=OSes;
  Result.TargetType:=ttExampleUnit;
end;


Function TTargets.AddExampleProgram(Const AProgramName : String) : TTarget;
begin
  Result:=AddExampleProgram(AProgramName,AllCPUs,AllOSes);
end;


Function TTargets.AddExampleProgram(Const AProgramName : String;const OSes:TOSes) : TTarget;
begin
  Result:=AddExampleProgram(AProgramName,AllCPUs,OSes);
end;


{$ifdef cpu_only_overloads}
Function TTargets.AddExampleProgram(Const AProgramName : String;const CPUs:TCPUs) : TTarget;
begin
  Result:=AddExampleProgram(AProgramName,CPUs,AllOSes);
end;
{$endif cpu_only_overloads}


Function TTargets.AddExampleProgram(Const AProgramName : String;const CPUs:TCPUs;const OSes:TOSes) : TTarget;
begin
  Result:=Add as TTarget;
  Result.Name:=AProgramName;
  Result.CPUs:=CPUs;
  Result.OSes:=OSes;
  Result.TargetType:=ttExampleProgram;
end;


{****************************************************************************
                                TSources
****************************************************************************}

function TSources.GetSourceItem(Index : Integer): TSource;
begin
  Result:=TSource(Items[Index]);
end;


procedure TSources.SetSourceItem(Index : Integer; const AValue: TSource);
begin
  Items[Index]:=AValue;
end;


function TSources.AddDoc (const AFiles : String) : TSource;
begin
  Result:=Add as TSource;
  Result.Name:=AFiles;
  Result.FSourceType:=stDoc;
end;


function TSources.AddSrc(const AFiles : String) : TSource;
begin
  Result:=Add as TSource;
  Result.Name:=AFiles;
  Result.FSourceType:=stSrc;
end;


function TSources.AddExample(const AFiles : String) : TSource;
begin
  Result:=Add as TSource;
  Result.Name:=AFiles;
  Result.FSourceType:=stExample;
end;


function TSources.AddTest(const AFiles : String) : TSource;
begin
  Result:=Add as TSource;
  Result.Name:=AFiles;
  Result.FSourceType:=stTest;
end;


procedure TSources.AddDocFiles(const AFileMask: string; Recursive: boolean);
var
  List : TStrings;
  i: integer;
begin
  List := TStringList.Create;
  SearchFiles(AFileMask, Recursive, List);
  for i:= 0 to Pred(List.Count) do
    AddDoc(List[i]);
  List.Free;
end;


procedure TSources.AddSrcFiles(const AFileMask: string; Recursive: boolean);
var
  List : TStrings;
  i: integer;
begin
  List := TStringList.Create;
  SearchFiles(AFileMask, Recursive, List);
  for i:= 0 to Pred(List.Count) do
    AddSrc(List[i]);
  List.Free;
end;


procedure TSources.AddExampleFiles(const AFileMask: string; Recursive: boolean);
var
  List : TStrings;
  i: integer;
begin
  List := TStringList.Create;
  SearchFiles(AFileMask, Recursive, List);
  for i:= 0 to Pred(List.Count) do
    AddExample(List[i]);
  List.Free;
end;


procedure TSources.AddTestFiles(const AFileMask: string; Recursive: boolean);
var
  List : TStrings;
  i: integer;
begin
  List := TStringList.Create;
  SearchFiles(AFileMask, Recursive, List);
  for i:= 0 to Pred(List.Count) do
    AddTest(List[i]);
  List.Free;
end;


{****************************************************************************
                             TPackage
****************************************************************************}

constructor TPackage.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVersion:=TFPVersion.Create;
  FTargets:=TTargets.Create(TTarget);
  FSources:=TSources.Create(TSource);
  FDependencies:=TDependencies.Create(TDependency);
  FInstallFiles:=TConditionalStrings.Create(TConditionalString);
  FCleanFiles:=TConditionalStrings.Create(TConditionalString);
  FUnitPath:=TConditionalStrings.Create(TConditionalString);
  FObjectPath:=TConditionalStrings.Create(TConditionalString);
  FIncludePath:=TConditionalStrings.Create(TConditionalString);
  FSourcePath:=TConditionalStrings.Create(TConditionalString);
  FExamplePath:=TConditionalStrings.Create(TConditionalString);
  FTestPath:=TConditionalStrings.Create(TConditionalString);
  FCommands:=TCommands.Create(TCommand);
  FCPUs:=AllCPUs;
  FOSes:=AllOSes;
  FInstalledChecksum:=$ffffffff;
  // Implicit dependency on RTL
  FDependencies.Add('rtl');
end;


destructor TPackage.destroy;
begin
  FreeAndNil(FDependencies);
  FreeAndNil(FInstallFiles);
  FreeAndNil(FCleanFiles);
  FreeAndNil(FIncludePath);
  FreeAndNil(FSourcePath);
  FreeAndNil(FExamplePath);
  FreeAndNil(FTestPath);
  FreeAndNil(FObjectPath);
  FreeAndNil(FUnitPath);
  FreeAndNil(FSources);
  FreeAndNil(FTargets);
  FreeAndNil(FVersion);
  inherited destroy;
end;


procedure TPackage.SetName(const AValue: String);
begin
  inherited SetName(AValue);
  // RTL should not have any dependencies
  if AValue='rtl' then
    FDependencies.Clear;
end;


Function TPackage.GetUnitsOutputDir(ACPU:TCPU; AOS : TOS):String;
begin
  Result:='units'+PathDelim+MakeTargetString(ACPU,AOS);
end;


Function TPackage.GetBinOutputDir(ACPU:TCPU; AOS : TOS) : String;
begin
  Result:='bin'+PathDelim+MakeTargetString(ACPU,AOS);
end;


procedure TPackage.GetCleanFiles(List: TStrings; ACPU:TCPU; AOS : TOS);
Var
  OB,OU : String;
  I : Integer;
begin
  OB:=IncludeTrailingPathDelimiter(GetBinOutputDir(Defaults.CPU,Defaults.OS));
  OU:=IncludeTrailingPathDelimiter(GetUnitsOutputDir(Defaults.CPU,Defaults.OS));
  AddConditionalStrings(List,CleanFiles,ACPU,AOS);
  For I:=0 to FTargets.Count-1 do
    FTargets.TargetItems[I].GetCleanFiles(List, OU, OB, ACPU, AOS);
end;


procedure TPackage.GetInstallFiles(List: TStrings;Types : TTargetTypes;ACPU:TCPU; AOS : TOS);
Var
  OB,OU : String;
  I : Integer;
  T : TTarget;
begin
  OB:=IncludeTrailingPathDelimiter(GetBinOutputDir(Defaults.CPU,Defaults.OS));
  OU:=IncludeTrailingPathDelimiter(GetUnitsOutputDir(Defaults.CPU,Defaults.OS));
  AddConditionalStrings(List,InstallFiles,ACPU,AOS);
  For I:=0 to FTargets.Count-1 do
    begin
      T:=FTargets.TargetItems[I];
      if (T.TargetType in Types) and (T.Install) then
        T.GetInstallFiles(List, OU, OB, ACPU, AOS);
    end;
end;


procedure TPackage.GetArchiveFiles(List: TStrings; ACPU:TCPU; AOS : TOS);
Var
  I : Integer;
begin
  // Targets only
  For I:=0 to FTargets.Count-1 do
    FTargets.TargetItems[I].GetArchiveFiles(List,ACPU,AOS);
end;


Function TPackage.GetDescription : string;
Var
  FN : String;
  L : TStringList;
begin
  If (FDescription<>'') then
    Result:=FDescription
  else
    If (FDescriptionFile<>'') then
      begin
        // Always relative to binary name.
        FN:=ExtractFilePath(ParamStr(0));
        FN:=FN+FDescriptionFile;
        If FileExists(FN) then
          begin
            L:=TStringList.Create;
            Try
              L.LoadFromFile(FN);
              Result:=L.Text;
            Finally
              L.Free;
            end;
          end;
      end;
end;


Function TPackage.GetVersion : string;
begin
  result:=FVersion.AsString;
end;


Procedure TPackage.SetVersion(const V : string);
begin
  FVersion.AsString:=V;
end;


Function TPackage.GetFileName : string;
begin
  If (FFileName<>'') then
    Result:=FFileName
  else
    if not FVersion.Empty then
      Result := Name + '-' + FVersion.AsString
    else
      Result := Name;
end;


Procedure TPackage.GetManifest(Manifest : TStrings);

  procedure AddOSes(const AIndent:string;AOSes:TOSes);
  var
    IOS : TOS;
  begin
    if (AOSes=AllOSes) then
      exit;
    Manifest.Add(AIndent+'<oses>');
    for IOS:=low(TOSes) to high(TOSes) do
      if IOS in AOSes then
        Manifest.Add(Format(AIndent+' <os name="%s"/>',[OSToString(IOS)]));
    Manifest.Add(AIndent+'</oses>');
  end;

  procedure AddCPUs(const AIndent:string;ACPUs:TCPUs);
  var
    ICPU : TCPU;
  begin
    if (ACPUs=AllCPUs) then
      exit;
    Manifest.Add(AIndent+'<cpus>');
    for ICPU:=low(TCPUs) to high(TCPUs) do
      if ICPU in ACPUs then
        Manifest.Add(Format(AIndent+' <cpu name="%s"/>',[CPUToString(ICPU)]));
    Manifest.Add(AIndent+'</cpus>');
  end;

Var
  S : String;
  i : Integer;
  D : TDependency;
begin
  With Manifest do
    begin
    Add(Format('<package name="%s">',[QuoteXml(Name)]));
    Add(Format(' <version major="%d" minor="%d" micro="%d" build="%d"/>',[FVersion.Major,FVersion.Minor,FVersion.Micro,FVersion.Build]));
    AddOSes(' ',OSes);
    AddCPUs(' ',CPUs);
    Add(Format(' <filename>%s</filename>',[QuoteXml(FileName + ZipExt)]));
    Add(Format(' <author>%s</author>',[QuoteXml(Author)]));
    Add(Format(' <license>%s</license>',[QuoteXml(License)]));
    if ExternalURL<>'' then
      Add(Format(' <externalurl>%s</externalurl>',[QuoteXml(ExternalURL)]));
    Add(Format(' <email>%s</email>',[QuoteXMl(Email)]));
    S:=Description;
    If (S<>'') then
      Add(Format(' <description>%s</description>',[QuoteXML(S)]));
    If (Dependencies.Count>0) then
      begin
        Add(' <dependencies>');
        for I:=0 to Dependencies.Count-1 do
          begin
            D:=Dependencies[i];
            Add('  <dependency>');
            Add(Format('   <package packagename="%s"/>',[QuoteXML(D.Value)]));
            if not D.FVersion.Empty then
              Add(Format('   <version major="%d" minor="%d" micro="%d" build="%d"/>',[D.FVersion.Major,D.FVersion.Minor,D.FVersion.Micro,D.FVersion.Build]));
            AddOSes('   ',D.OSes);
            AddCPUs('   ',D.CPUs);
            Add('  </dependency>');
          end;
        Add(' </dependencies>');
      end;
    Add('</package>');
    end;
end;


procedure TPackage.LoadUnitConfigFromFile(Const AFileName: String);
var
  L,L2 : TStrings;
  VOS : TOS;
  VCPU : TCPU;
  i,k : Integer;
  DepChecksum : Cardinal;
  DepName : String;
  D : TDependency;
begin
  L:=TStringList.Create;
  Try
    ReadIniFile(AFileName,L);
    With L do
      begin
        Version:=Values[KeyVersion];
        InstalledChecksum:=Cardinal(StrToInt64Def(Values[KeyChecksum],$ffffffff));
        VCPU:=StringToCPU(Values[KeyCPU]);
        VOS:=StringToOS(Values[KeyOS]);
        OSes:=[VOS];
        CPUs:=[VCPU];
        L2:=TStringList.Create;
        L2.CommaText:=Values[KeyDepends];
        for i:=0 to L2.Count-1 do
          begin
            DepName:=L2[i];
            k:=Pos('|',DepName);
            if k>0 then
              begin
                DepChecksum:=StrToInt(Copy(DepName,k+1,Length(DepName)-k));
                DepName:=Copy(DepName,1,k-1);
              end
            else
              DepChecksum:=$ffffffff;
            D:=Dependencies.Add(DepName,CPUs,OSes);
            D.RequireChecksum:=DepChecksum;
          end;
        FreeAndNil(L2);
        NeedLibC:=Upcase(Values[KeyNeedLibC])='Y';
      end;
  Finally
    L.Free;
  end;
end;


procedure TPackage.SaveUnitConfigToFile(Const AFileName: String;ACPU:TCPU;AOS:TOS);
Var
  F : TFileStream;
  L : TStringList;
  Deps : String;
  i : integer;
  D : TDependency;
  p : TPackage;
begin
  F:=TFileStream.Create(AFileName,fmCreate);
  L:=TStringList.Create;
  try
    With L do
      begin
        Values[KeyName]:=Name;
        Values[KeyVersion]:=Version;
        // TODO Generate checksum based on PPUs
        Values[KeyChecksum]:=IntToStr(DateTimeToFileDate(Now));
        Values[KeyCPU]:=CPUToString(ACPU);
        Values[KeyOS]:=OSToString(AOS);
        Deps:='';
        for i:=0 to Dependencies.Count-1 do
          begin
            D:=Dependencies[i];
            if (ACPU in D.CPUs) and (AOS in D.OSes) then
              begin
                if Deps<>'' then
                  Deps:=Deps+',';
                Deps:=Deps+D.Value;
                P:=TPackage(D.Target);
                if assigned(P) and (P.InstalledChecksum<>$ffffffff) then
                  Deps:=Deps+'|'+IntToStr(P.InstalledChecksum);
              end;
          end;
        Values[KeyDepends]:=Deps;
        if NeedLibC then
          Values[KeyNeedLibC]:='Y'
        else
          Values[KeyNeedLibC]:='N';
      end;
    L.SaveToStream(F);
  Finally
    L.Free;
    F.Free;
  end;
end;



{****************************************************************************
                              TPackages
****************************************************************************}

function TPackages.GetPackage(const AName : String): TPackage;
begin
  Result:=TPackage(ItemByName(AName))
end;

function TPackages.GetPackageItem(AIndex : Integer): TPackage;
begin
  Result:=TPackage(Items[AIndex]);
end;


procedure TPackages.SetPackageItem(AIndex : Integer; const AValue: TPackage);
begin
  Items[AIndex]:=AValue;
end;


function TPackages.AddPackage(const AName: String): TPackage;
begin
  Result:=Add as TPackage;
  Result.Name:=AName;
end;


{****************************************************************************
                             TCustomDefaults
****************************************************************************}

procedure TCustomDefaults.SetCPU(const AValue: TCPU);
begin
  FCPU:=AValue;
  RecalcTarget;
end;


function TCustomDefaults.GetBaseInstallDir: String;
begin
  If (FBaseInstallDir<>'') then
    Result:=FBaseInstallDir
  else
    if UnixPaths then
      Result:=Prefix +'lib' + PathDelim + 'fpc' + PathDelim
    else
      Result:=Prefix;
end;


function TCustomDefaults.GetBinInstallDir: String;
begin
  If (FBinInstallDir<>'') then
    Result:=FBinInstallDir
  else
    If UnixPaths then
      Result:=BaseInstallDir+'bin'
    else
      Result:=BaseInstallDir+'bin';
end;


function TCustomDefaults.GetCompiler: String;
begin
  If (FCompiler<>'') then
    Result:=FCompiler
  else
    Result:='fpc';
end;


function TCustomDefaults.GetDocInstallDir: String;
begin
  If (FBinInstallDir<>'') then
    Result:=FBinInstallDir
  else
    If UnixPaths then
      Result:=Prefix+'share'+PathDelim+'doc'
    else
      Result:=BaseInstallDir+'docs';
end;


function TCustomDefaults.GetExamplesInstallDir: String;
begin
  If (FExamplesInstallDir<>'') then
    Result:=FExamplesInstallDir
  else
    If UnixPaths then
      Result:=Prefix+'share'+PathDelim+'docs'+PathDelim+'examples'
    else
      Result:=BaseInstallDir+'examples';
end;


function TCustomDefaults.GetUnitInstallDir: String;
begin
  If (FUnitInstallDir<>'') then
    Result:=FUnitInstallDir
  else
    If UnixPaths then
      Result:=BaseInstallDir+'units'+PathDelim+Target
    else
      Result:=BaseInstallDir+'units'+PathDelim+Target;
end;


function TCustomDefaults.GetLocalUnitDir: String;
begin
  Result:=FLocalUnitDir;
end;


function TCustomDefaults.GetGlobalUnitDir: String;
begin
  If (FGlobalUnitDir<>'') then
    Result:=FGlobalUnitDir
  else
    Result:=UnitInstallDir;
end;


procedure TCustomDefaults.SetLocalUnitDir(const AValue: String);
begin
  // Use ExpandFileName to support ~/ expansion
  if AValue<>'' then
    FLocalUnitDir:=IncludeTrailingPathDelimiter(ExpandFileName(AValue))
  else
    FLocalUnitDir:='';
end;


procedure TCustomDefaults.SetGlobalUnitDir(const AValue: String);
begin
  // Use ExpandFileName to support ~/ expansion
  if AValue<>'' then
    FGlobalUnitDir:=IncludeTrailingPathDelimiter(ExpandFileName(AValue))
  else
    FGlobalUnitDir:='';
end;


procedure TCustomDefaults.SetBaseInstallDir(const AValue: String);
begin
  // Use ExpandFileName to support ~/ expansion
  if AValue<>'' then
    FBaseInstallDir:=IncludeTrailingPathDelimiter(ExpandFileName(AValue))
  else
    FBaseInstallDir:='';
  UnitInstallDir:='';
  BinInstallDir:='';
  ExamplesInstallDir:='';
end;


procedure TCustomDefaults.SetOS(const AValue: TOS);
begin
  FOS:=AValue;
  Recalctarget;
end;


procedure TCustomDefaults.SetPrefix(const AValue: String);
begin
  if FPrefix=AValue then exit;
  FPrefix:=IncludeTrailingPathDelimiter(AValue);
  BaseInstallDir:='';
end;


procedure TCustomDefaults.SetTarget(const AValue: String);
Var
  P : Integer;
begin
  if FTarget<>AValue then
    begin
      P:=Pos('-',AValue);
      If (P<>0) then
        begin
          FOS:=StringToOS(System.Copy(Avalue,P+1,Length(AValue)-P));
          FCPU:=StringToCPU(System.Copy(Avalue,1,P-1));
        end
      else
        FOS:=StringToOS(AValue);
      FTarget:=AValue;
    end;
end;


procedure TCustomDefaults.RecalcTarget;
begin
  Ftarget:=CPUToString(FCPU)+'-'+OStoString(FOS);
end;


constructor TCustomDefaults.Create;
begin
  InitDefaults;
end;


procedure TCustomDefaults.InitDefaults;
begin
{$ifdef unix}
  UnixPaths:=True;
{$else}
  UnixPaths:=False;
{$endif}
  FNoFPCCfg:=False;
  FCPU:=cpuNone;
  FOS:=osNone;
end;


procedure TCustomDefaults.LocalInit(Const AFileName : String);
Var
  FN : String;
begin
  FN:=AFileName;
  If (FN='') then
    begin
    // Environment variable.
    FN:=GetEnvironmentVariable('FPMAKECFG');
    If (FN<>'') then
      If not FileExists(FN) then
        FN:='';
    // User config file fpmake.cfg
    If (FN='') then
      begin
      FN:=GetAppConfigFile(False);
      If Not FileExists(FN) then
        FN:='';
      end;
    // Global config file fpmake.cfg
    If (FN='') then
      begin
      FN:=GetAppConfigFile(True);
      If Not FileExists(FN) then
        FN:='';
      end;
    end;
  If (FN<>'') and FileExists(FN) then
    LoadFromFile(FN);
end;


procedure TCustomDefaults.CompilerDefaults;
{$ifdef HAS_UNIT_PROCESS}
var
  infoSL : TStringList;
{$endif HAS_UNIT_PROCESS}
begin
  if (CPU=cpuNone) or (OS=osNone) or (FCompilerVersion='') then
    begin
{$ifdef HAS_UNIT_PROCESS}
      // Detect compiler version/target from -i option
      infosl:=TStringList.Create;
      infosl.Delimiter:=' ';
      infosl.DelimitedText:=GetCompilerInfo(GetCompiler,'-iVTPTO');
      if infosl.Count<>3 then
        Raise EInstallerError.Create(SErrInvalidFPCInfo);
      if FCompilerVersion='' then
        FCompilerVersion:=infosl[0];
      if CPU=cpuNone then
        CPU:=StringToCPU(infosl[1]);
      if OS=osNone then
        OS:=StringToOS(infosl[2]);
{$else HAS_UNIT_PROCESS}
      // Defaults taken from compiler used to build fpmake
      if CPU=cpuNone then
        CPU:=StringToCPU({$I %FPCTARGETCPU%});
      if OS=osNone then
        OS:=StringToOS({$I %FPCTARGETOS%});
      if FCompilerVersion='' then
        FCompilerVersion:={$I %FPCVERSION%};
{$endif HAS_UNIT_PROCESS}
    end;
end;


procedure TCustomDefaults.LoadFromFile(Const AFileName: String);
Var
  F : TFileStream;
begin
  F:=TFileStream.Create(AFileName,fmOpenRead);
  Try
    LoadFromStream(F);
  Finally
    F.Free;
  end;
end;


procedure TCustomDefaults.SaveToFile(Const AFileName: String);
Var
  F : TFileStream;
begin
  F:=TFileStream.Create(AFileName,fmCreate);
  Try
    SaveToStream(F);
  Finally
    F.Free;
  end;
end;


procedure TCustomDefaults.SaveToStream(S : TStream);
Var
  L : TStringList;
begin
  L:=TStringList.Create;
  try
    With L do
      begin
      Values[KeyArchive]:=FArchive;
      Values[KeyCompiler]:=FCompiler;
      Values[KeyCopy]:=FCopy;
      Values[KeyMkDir]:=FMkDir;
      Values[KeyMove]:=FMove;
      Values[KeyOptions]:=FOptions;
      Values[KeyCPU]:=CPUToString(FCPU);
      Values[KeyOS]:=OSToString(FOS);
      Values[KeyMode]:=ModeToString(FMode);
      Values[KeyLocalUnitDir]:=FLocalUnitDir;
      Values[KeyGlobalUnitDir]:=FGlobalUnitDir;
      Values[KeyPrefix]:=FPrefix;
      Values[KeyBaseInstallDir]:=FBaseInstallDir;
      Values[KeyUnitInstallDir]:=FUnitInstallDir;
      Values[KeyBinInstallDir]:=FBinInstallDir;
      Values[KeyDocInstallDir]:=FDocInstallDir;
      Values[KeyExamplesInstallDir]:=FExamplesInstallDir;
      Values[KeyRemove]:=FRemove;
      Values[KeyTarget]:=FTarget;
      if FNoFPCCfg then
        Values[KeyNoFPCCfg]:='Y';
      end;
    L.SaveToStream(S);
  Finally
    L.Free;
  end;
end;


procedure TCustomDefaults.LoadFromStream(S: TStream);
Var
  L : TStrings;
  Line : String;
  I,P,PC : Integer;
begin
  L:=TStringList.Create;
  Try
    L.LoadFromStream(S);
    // Fix lines.
    For I:=L.Count-1 downto 0 do
      begin
      Line:=L[I];
      P:=Pos('=',Line);
      PC:=Pos(';',Line);  // Comment line.
      If (P=0) or ((PC<>0) and (PC<P)) then
        L.Delete(I)
      else
        L[i]:=Trim(System.Copy(Line,1,P-1)+'='+Trim(System.Copy(Line,P+1,Length(Line)-P)));
      end;
    With L do
      begin
      FArchive:=Values[KeyArchive];
      FCompiler:=Values[KeyCompiler];
      FCopy:=Values[KeyCopy];
      FMkDir:=Values[KeyMkDir];
      FMove:=Values[KeyMove];
      FRemove:=Values[KeyRemove];
      FOptions:=Values[KeyOptions];
      Line:=Values[KeyCPU];
      If (Line<>'') then
        FCPU:=StringToCPU(Line);
      Line:=Values[KeyOS];
      If (Line<>'') then
        FOS:=StringToOS(Line);
      Line:=Values[KeyMode];
      If (Line<>'') then
        FMode:=StringToMode(Line);
      FTarget:=Values[KeyTarget];
      FLocalUnitDir:=Values[KeyLocalUnitDir];
      FGlobalUnitDir:=Values[KeyGlobalUnitDir];
      FPrefix:=Values[KeyPrefix];
      FBaseInstallDir:=Values[KeyBaseInstallDir];
      FUnitInstallDir:=Values[KeyUnitInstallDir];
      FBinInstallDir:=Values[KeyBinInstallDir];
      FDocInstallDir:=Values[KeyDocInstallDir];
      FExamplesInstallDir:=Values[KeyExamplesInstallDir];
      FNoFPCCfg:=(Upcase(Values[KeyNoFPCCfg])='Y');
      end;
  Finally
    L.Free;
  end;
end;


{****************************************************************************
                             TFPCDefaults
****************************************************************************}

procedure TFPCDefaults.CompilerDefaults;
var
  BD : String;
begin
  inherited CompilerDefaults;

  // Use the same algorithm as the compiler, see options.pas
{$ifdef Unix}
  BD:=FixPath(GetEnvironmentVariable('FPCDIR'));
  if BD='' then
    begin
      BD:='/usr/local/lib/fpc/'+FCompilerVersion;
      if not DirectoryExists(BD) and
         DirectoryExists('/usr/lib/fpc/'+FCompilerVersion) then
        BD:='/usr/lib/fpc/'+FCompilerVersion;
    end;
{$else unix}
  BD:=FixPath(GetEnvironmentVariable('FPCDIR'));
  if BD='' then
    begin
      BD:=ExtractFilePath(FCompiler)+'..';
      if not(DirectoryExists(BD+'/units')) and
         not(DirectoryExists(BD+'/rtl')) then
        BD:=FBaseInstallDir+'..';
    end;
{$endif unix}

  // Where to install by default
  if (FBaseInstallDir='') and (FPrefix='') then
    BaseInstallDir:=BD;

  // Where to find the units by default
  if (FGlobalUnitDir='') then
    GlobalUnitDir:=IncludeTrailingPathDelimiter(BD)+'units'+PathDelim+Target;
end;


{****************************************************************************
                            TCustomInstaller
****************************************************************************}

constructor TCustomInstaller.Create(AOwner: TComponent);
begin
  Dictionary:=DictionaryClass.Create(Nil);
  AnalyzeOptions;
  CreatePackages;
end;


destructor TCustomInstaller.Destroy;
begin
  FreeAndNil(Defaults);
  FreeAndNil(Dictionary);
  inherited destroy;
end;


procedure TCustomInstaller.Log(Level: TVerboseLevel; const Msg: String);
begin
  If Level in FLogLevels then
    begin
      if Level in [vlError,vlWarning] then
        Writeln(StdErr,Msg)
      else
        Writeln(StdOut,Msg);
    end;
end;


procedure TCustomInstaller.CreatePackages;
begin
  FPAckages:=TPackages.Create(TPackage);
end;


procedure TCustomInstaller.CreateBuildEngine;
begin
  FBuildEngine:=TBuildEngine.Create(Self);
//  FBuildEngine.Defaults:=Defaults;
  FBuildEngine.ListMode:=FListMode;
  FBuildEngine.OnLog:=@Self.Log;
end;


procedure TCustomInstaller.Error(const Msg: String);
begin
  Raise EInstallerError.Create(Msg);
end;


procedure TCustomInstaller.Error(const Fmt: String; Args: array of const);
begin
  Raise EInstallerError.CreateFmt(Fmt,Args);
end;


Function TCustomInstaller.AddPackage(const AName: String) : TPackage;
begin
  result:=FPackages.AddPackage(AName);
end;


procedure TCustomInstaller.AnalyzeOptions;

  Function CheckOption(Index : Integer;const Short,Long : String): Boolean;
  var
    O : String;
  begin
    O:=Paramstr(Index);
    Result:=(O='-'+short) or (O='--'+long) or (copy(O,1,Length(Long)+3)=('--'+long+'='));
  end;

  Function CheckCommand(Index : Integer;const Short,Long : String): Boolean;
  var
    O : String;
  begin
    O:=Paramstr(Index);
    Result:=(O='-'+short) or (O=long);
  end;

  Function OptionArg(Var Index : Integer) : String;
  Var
    P : Integer;
  begin
    if (Length(ParamStr(Index))>1) and (Paramstr(Index)[2]<>'-') then
      begin
      If Index<ParamCount then
        begin
        Inc(Index);
        Result:=Paramstr(Index);
        end
      else
        Error(SErrNeedArgument,[Index,ParamStr(Index)]);
      end
    else If length(ParamStr(Index))>2 then
      begin
      P:=Pos('=',Paramstr(Index));
      If (P=0) then
        Error(SErrNeedArgument,[Index,ParamStr(Index)])
      else
        begin
        Result:=Paramstr(Index);
        Delete(Result,1,P);
        end;
      end;
  end;

Var
  I : Integer;
  DefaultsFileName : string;
begin
  I:=0;
  FListMode:=False;
  FLogLevels:=DefaultMessages;
  While (I<ParamCount) do
    begin
    Inc(I);
    if CheckOption(I,'v','verbose') then
      FLogLevels:=AllMessages
    else if CheckOption(I,'d','debug') then
      FLogLevels:=AllMessages+[vlDebug]
    else if CheckCommand(I,'m','compile') then
      FRunMode:=rmCompile
    else if CheckCommand(I,'b','build') then
      FRunMode:=rmBuild
    else if CheckCommand(I,'i','install') then
      FRunMode:=rmInstall
    else if CheckCommand(I,'c','clean') then
      FRunMode:=rmClean
    else if CheckCommand(I,'a','archive') then
      FRunMode:=rmarchive
    else if CheckCommand(I,'M','manifest') then
      FRunMode:=rmManifest
    else if CheckOption(I,'h','help') then
      Usage('',[])
    else if Checkoption(I,'C','cpu') then
      Defaults.CPU:=StringToCPU(OptionArg(I))
    else if Checkoption(I,'O','os') then
      Defaults.OS:=StringToOS(OptionArg(I))
    else if Checkoption(I,'t','target') then
      Defaults.Target:=OptionArg(I)
    else if CheckOption(I,'l','list-commands') then
      FListMode:=True
    else if Checkoption(I,'P','prefix') then
      Defaults.Prefix:=OptionArg(I)
    else if Checkoption(I,'n','nofpccfg') then
      Defaults.NoFPCCfg:=true
    else if CheckOption(I,'B','baseinstalldir') then
      Defaults.BaseInstallDir:=OptionArg(I)
    else if CheckOption(I,'UL','localunitdir') then
      Defaults.LocalUnitDir:=OptionArg(I)
    else if CheckOption(I,'UG','globalunitdir') then
      Defaults.GlobalUnitDir:=OptionArg(I)
    else if CheckOption(I,'r','compiler') then
      Defaults.Compiler:=OptionArg(I)
    else if CheckOption(I,'f','config') then
      DefaultsFileName:=OptionArg(I)
    else
      begin
      Usage(SErrInValidArgument,[I,ParamStr(I)]);
      end;
    end;
  If DefaultsFileName<>'' then
    Defaults.LocalInit(DefaultsFileName);
  Defaults.CompilerDefaults;
end;


procedure TCustomInstaller.Usage(const FMT: String; Args: array of const);

  Procedure LogCmd(const LC,Msg : String);
  begin
    Log(vlInfo,Format(' %-12s %s',[LC,MSG]));
  end;

  Procedure LogOption(const C,LC,Msg : String);
  begin
    Log(vlInfo,Format(' -%s --%-16s %s',[C,LC,MSG]));
  end;

  Procedure LogArgOption(const C,LC,Msg : String);
  begin
    Log(vlInfo,Format(' -%s --%-20s %s',[C,LC+'='+SValue,MSG]));
  end;

begin
  // Force the Usage to be displayed
  Include(FLogLevels,vlInfo);
  If (FMT<>'') then
    Log(vlInfo,Format(Fmt,Args));
  Log(vlInfo,Format(SHelpUsage,[Paramstr(0)]));
  Log(vlInfo,SHelpCommand);
  LogCmd('compile',SHelpCompile);
  LogCmd('build',SHelpBuild);
  LogCmd('install',SHelpInstall);
  LogCmd('clean',SHelpClean);
  LogCmd('archive',SHelpArchive);
  LogCmd('manifest',SHelpManifest);
  Log(vlInfo,SHelpCmdOptions);
  LogOption('h','help',SHelpHelp);
  LogOption('l','list-commands',SHelpList);
  LogOption('n','nofpccfg',SHelpNoFPCCfg);
  LogOption('v','verbose',SHelpVerbose);
  LogArgOption('C','cpu',SHelpCPU);
  LogArgOption('O','os',SHelpOS);
  LogArgOption('t','target',SHelpTarget);
  LogArgOption('P','prefix',SHelpPrefix);
  LogArgOption('B','baseinstalldir',SHelpBaseInstalldir);
  LogArgOption('UL','localunitdir',SHelpLocalUnitdir);
  LogArgOption('UG','globalunitdir',SHelpGlobalUnitdir);
  LogArgOption('r','compiler',SHelpCompiler);
  LogArgOption('f','config',SHelpConfig);
  Log(vlInfo,'');
  If (FMT<>'') then
    halt(1)
  else
    halt(0);
end;


procedure TCustomInstaller.Compile(Force: Boolean);
begin
  FBuildEngine.ForceCompile:=Force;
  FBuildEngine.Compile(FPackages);
end;


procedure TCustomInstaller.Clean;
begin
  BuildEngine.Clean(FPackages);
end;


procedure TCustomInstaller.Install;
begin
  BuildEngine.Install(FPackages);
end;


procedure TCustomInstaller.Archive;
begin
  // Force generation of manifest.xml, this is required for the repository
  BuildEngine.Manifest(FPackages);
  BuildEngine.Archive(FPackages);
end;


procedure TCustomInstaller.Manifest;
begin
  BuildEngine.Manifest(FPackages);
end;


procedure TCustomInstaller.CheckPackages;
begin
  If (FPackages.Count=0) then
    Error(SErrNoPackagesDefined);
  // Check for other obvious errors ?
end;


Function TCustomInstaller.Run : Boolean;
begin
  Result:=True;
  try
    CheckPackages;
    CreateBuildEngine;
    Case RunMode of
      rmCompile : Compile(False);
      rmBuild   : Compile(True);
      rmInstall : Install;
      rmArchive : Archive;
      rmClean    : Clean;
      rmManifest : Manifest;
    end;
  except
    On E : Exception do
      begin
      Log(vlError,SErrInstaller);
      Log(vlError,E.Message);
      Result:=False;
      end;
  end;
  // Force returning an exitcode to the shell
  if not Result then
    ExitCode:=1;
end;


{****************************************************************************
                                TFPCInstaller
****************************************************************************}

constructor TFPCInstaller.Create(AOwner: TComponent);
begin
  if assigned(Defaults) then
    Error(SErrAlreadyInitialized);
  Defaults:=TFPCDefaults.Create;
  inherited Create(AOwner);
end;


{****************************************************************************
                                 TBasicInstaller
****************************************************************************}

constructor TBasicInstaller.Create(AOwner: TComponent);
begin
  if assigned(Defaults) then
    Error(SErrAlreadyInitialized);
  Defaults:=TBasicDefaults.Create;
  inherited Create(AOwner);
end;


{****************************************************************************
                                 TBuildEngine
****************************************************************************}

constructor TBuildEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Maybe this should be the current directory ?
  // Or have it as a command-line option.
  // Would allow to put all 'installers' in one dir and call them
  // With --start-dir=/path/to/sources.
  FStartDir:=includeTrailingPathDelimiter(GetCurrentDir);
  FExternalPackages:=TPackages.Create(TPackage);
end;


destructor TBuildEngine.Destroy;
begin
  FreeAndNil(FExternalPackages);
  inherited Destroy;
end;


procedure TBuildEngine.Error(const Msg: String);
begin
  Raise EInstallerError.Create(Msg);
end;


procedure TBuildEngine.Error(const Fmt: String; const Args: array of const);
begin
  Raise EInstallerError.CreateFmt(Fmt,Args);
end;


procedure TBuildEngine.ExecuteCommand(const Cmd,Args : String; IgnoreError : Boolean = False);
Var
  E : Integer;
begin
  Log(vlInfo,SInfoExecutingCommand,[Cmd,Args]);
  if ListMode then
    Log(vlCommand,'%s %s',[Cmd,Args])
  else
    begin
      // We should check cmd for spaces, and move all after first space to args.
      E:=ExecuteProcess(cmd,args);
      If (E<>0) and (not IgnoreError) then
        Error(SErrExternalCommandFailed,[Cmd,E]);
    end;
end;


Function TBuildEngine.SysDirectoryExists(const ADir:string):Boolean;
begin
  result:=SysUtils.DirectoryExists(ADir);
  if result then
    Log(vlDebug,SDbgDirectoryExists,[ADir,SDbgFound])
  else
    Log(vlDebug,SDbgDirectoryExists,[ADir,SDbgNotFound]);
end;


Function TBuildEngine.SysFileExists(const AFileName:string):Boolean;
begin
  result:=SysUtils.FileExists(AFileName);
  if result then
    Log(vlDebug,SDbgFileExists,[AFileName,SDbgFound])
  else
    Log(vlDebug,SDbgFileExists,[AFileName,SDbgNotFound]);
end;


procedure TBuildEngine.SysCopyFile(Const Src,Dest : String);
Var
  D,S : String;
  Fin,FOut : TFileStream;
  Count : Int64;
  A : Integer;
begin
  Log(vlInfo,SInfoCopyingFile,[Src,Dest]);
  FIn:=TFileStream.Create(Src,fmopenRead);
  Try
    D:=IncludeTrailingPathDelimiter(Dest);
    If DirectoryExists(D) then
      S:=D+ExtractFileName(Src)
    else
      S:=Dest;
    FOut:=TFileStream.Create(S,fmCreate);
    Try
      Count:=Fout.CopyFrom(FIn,0);
      If (Count<>Fin.Size) then
        Error(SErrCopyingFile,[Src,S]);
    Finally
      FreeAndNil(Fout);
    end;
    A:=FileGetDate(FIn.Handle);
    If (A=-1) then
      log(vlWarning,SWarnFailedToGetTime,[Src])
    else
      if FileSetDate(S,A)<>0 then
        Log(vlWarning,SWarnFailedToSetTime,[S]);
  finally
    FreeAndNil(Fin);
  end;
end;


procedure TBuildEngine.SysMoveFile(Const Src,Dest : String);
Var
  S : String;
begin
  If DirectoryExists(IncludeTrailingPathDelimiter(Dest)) then
    S:=IncludeTrailingPathDelimiter(Dest)+ExtractFileName(Src)
  else
    S:=Dest;
  If Not RenameFile(Src,S) then
    begin
      Try
        SysCopyFile(Src,S);
        SysDeleteFile(Src);
      Except
        On E : Exception Do
          Error(SErrMovingFile,[Src,S]);
      end;
    end;
end;


procedure TBuildEngine.SysDeleteFile(Const AFileName : String);
begin
  if not FileExists(AFileName) then
    Log(vlWarning,SWarnFileDoesNotExist,[AFileName])
  else If Not DeleteFile(AFileName) then
    Error(SErrDeletingFile,[AFileName]);
end;


procedure TBuildEngine.SysArchiveFiles(List: TStrings;Const AFileName: String);
begin
  If Not (Assigned(OnArchivefiles) or Assigned(ArchiveFilesProc)) then
    Raise EInstallerError.Create(SErrNoArchiveSupport);
  If Assigned(ArchiveFilesProc) then
    ArchiveFilesProc(AFileName,List)
  else
    OnArchiveFiles(AFileName,List);
end;


procedure TBuildEngine.LogIndent;
begin
  FLogPrefix:=FLogPrefix+'  ';
end;


procedure TBuildEngine.LogUnIndent;
begin
  Delete(FLogPrefix,1,2);
end;


procedure TBuildEngine.Log(Level: TVerboseLevel; const Msg: String);
begin
  If Assigned(FOnLog) then
    begin
      if Level in [vlInfo,vlDebug] then
        FOnLog(Level,FLogPrefix+Msg)
      else
        FOnLog(Level,Msg);
    end;
end;


procedure TBuildEngine.Log(Level: TVerboseLevel; const Fmt: String;const Args: array of const);
begin
  Log(Level,Format(Fmt,Args));
end;


procedure TBuildEngine.EnterDir(ADir: String);
Var
  D : String;
begin
  D:=FStartDir;
  D:=D+ADir;
  Log(vlDebug,SDbgEnterDir,[D]);
  If Not SetCurrentDir(D) then
    Error(SErrChangeDirFailed,[D]);
end;


procedure TBuildEngine.CmdCopyFiles(List: TStrings; Const DestDir: String);

Var
  Args : String;
  I : Integer;

begin
  CmdCreateDir(DestDir);
  If (Defaults.Copy<>'') then
    begin
      Args:=FileListToString(List,'');
      Args:=Args+' '+DestDir;
      ExecuteCommand(Defaults.Copy,Args);
    end
  else
    For I:=0 to List.Count-1 do
      SysCopyFile(List[i],DestDir);
end;


procedure TBuildEngine.CmdCreateDir(const DestDir: String);
begin
  If (Defaults.MkDir<>'') then
    ExecuteCommand(Defaults.MkDir,DestDir)
  else
    If not ForceDirectories(DestDir) then
      Error(SErrCreatingDirectory,[DestDir]);
end;


procedure TBuildEngine.CmdMoveFiles(List: TStrings; Const DestDir: String);
Var
  Args : String;
  I : Integer;
begin
  CmdCreateDir(DestDir);
  If (Defaults.Move<>'') then
    begin
      Args:=FileListToString(List,'');
      Args:=Args+' '+DestDir;
      ExecuteCommand(Defaults.Move,Args);
    end
  else
    For I:=0 to List.Count-1 do
      SysMoveFile(List[i],DestDir);
end;


procedure TBuildEngine.CmdDeleteFiles(List: TStrings);
Var
  Args : String;
  I : Integer;
begin
  If (Defaults.Remove<>'') then
    begin
      Args:=FileListToString(List,'');
      ExecuteCommand(Defaults.Remove,Args);
    end
  else
    For I:=0 to List.Count-1 do
      SysDeleteFile(List[i]);
end;


procedure TBuildEngine.CmdArchiveFiles(List: TStrings; Const ArchiveFile: String);
Var
  S,C,O : String;
begin
  If (Defaults.Archive='') then
    SysArchiveFiles(List,ArchiveFile)
  else
    begin
      S:=FileListToString(List,'');
      SplitCommand(Defaults.Archive,C,O);
      If (O='') then
        O:=ArchiveFile+' '+S
      else
        O:=Substitute(O,['ARCHIVE',ArchiveFile,'FILESORDIRS']);
      ExecuteCommand(C,O);
    end;
end;

Function TBuildEngine.FileNewer(const Src,Dest : String) : Boolean;

Var
  DS,DD : Longint;
  D1,D2 : TDateTime;

begin
  DS:=FileAge(Src);
  DD:=FileAge(Dest);
  D1:=FileDateToDateTime(DS);
  D2:=FileDateToDateTime(DD);
  Log(vlDebug,SDbgComparingFileTimes,[Src,DateTimeToStr(D1),Dest,DateTimeToStr(D2)]);
  Result:=D1>=D2;
  If Result then
    Log(vlInfo,SInfoSourceNewerDest,[Src,DateTimeToStr(D1),Dest,DateTimeToStr(D2)]);
end;


procedure TBuildEngine.ExecuteCommands(Commands: TCommands; At: TCommandAt);
Var
  C : TCommand;
  I : Integer;
  Cmd,O : String;
  E : Boolean;
begin
  For I:=0 to Commands.Count-1 do
    begin
      C:=Commands.CommandItems[i];
      if (C.At=At) then
        begin
          E:=True;
          If (C.SourceFile<>'') and (C.DestFile<>'')  then
            E:=FileNewer(C.SourceFile,IncludeTrailingPathDelimiter(Dictionary.GetValue('OUTPUTDIR'))+C.DestFile);
          If E then
            begin
            If Assigned(C.BeforeCommand) then
              C.BeforeCommand(C);
            O:=Substitute(C.Options,['SOURCE',C.SourceFile,'DEST',C.DestFile]);
            Cmd:=C.Command;
            If (ExtractFilePath(Cmd)='') then
              Cmd:=FileSearch(Cmd,GetEnvironmentvariable('PATH'));
            ExecuteCommand(Cmd,O,C.IgnoreResult);
            If Assigned(C.AfterCommand) then
              C.AfterCommand(C);
            end;
        end;
    end;
end;


Procedure TBuildEngine.LogSearchPath(const ASearchPathName:string;Path:TConditionalStrings; ACPU:TCPU;AOS:TOS);
var
  S : String;
  I : Integer;
  C : TConditionalString;
begin
  S:='';
  for i:=0 to Path.Count-1 do
    begin
      C:=Path[I];
      if (ACPU in C.CPUs) and (AOS in C.OSes) then
        begin
          if S<>'' then
            S:=S+PathSeparator;
          S:=S+Dictionary.ReplaceStrings(C.Value)
        end;
    end;
  if S<>'' then
    Log(vlDebug,SDbgSearchPath,[ASearchPathName,S]);
end;


Function TBuildEngine.FindFileInPath(Path:TConditionalStrings; AFileName:String; var FoundPath:String;ACPU:TCPU;AOS:TOS):Boolean;
var
  I : Integer;
  C : TConditionalString;
begin
  Result:=false;
  for i:=0 to Path.Count-1 do
    begin
      C:=Path[I];
      if (ACPU in C.CPUs) and (AOS in C.OSes) then
        begin
          FoundPath:=IncludeTrailingPathDelimiter(Dictionary.ReplaceStrings(C.Value));
          if FileExists(FoundPath+AFileName) then
            begin
              result:=true;
              exit;
            end;
        end;
    end;
  FoundPath:='';
end;


Procedure TBuildEngine.ResolveFileNames(APackage : TPackage; ACPU:TCPU;AOS:TOS;DoChangeDir:boolean=true);

  procedure FindMainSource(T:TTarget);
  var
    SD,SF  : String;
  begin
    LogSearchPath('package source',APackage.SourcePath,ACPU,AOS);
    SD:=Dictionary.ReplaceStrings(T.Directory);
    SF:=Dictionary.ReplaceStrings(T.SourceFileName);
    if SD='' then
      FindFileInPath(APackage.SourcePath,SF,SD,ACPU,AOS);
    if SD<>'' then
      SD:=IncludeTrailingPathDelimiter(SD);
    T.FTargetSourceFileName:=SD+SF;
    if FileExists(T.TargetSourceFileName) then
      Log(vlDebug,SDbgResolvedSourceFile,[T.SourceFileName,T.TargetSourceFileName])
    else
      begin
        Log(vlWarning,SWarnSourceFileNotFound,[T.SourceFileName,MakeTargetString(ACPU,AOS)]);
        T.FTargetSourceFileName:='';
      end;
  end;

  procedure FindIncludeSources(T:TTarget);
  var
    SD,SF  : String;
    D : TDependency;
    j : integer;
  begin
    LogSearchPath('target include',T.IncludePath,ACPU,AOS);
    LogSearchPath('package include',APackage.IncludePath,ACPU,AOS);
    for j:=0 to T.Dependencies.Count-1 do
      begin
        D:=T.Dependencies[j];
        if (D.DependencyType=depInclude) then
          begin
            D.TargetFileName:='';
            if (ACPU in D.CPUs) and (AOS in D.OSes) then
              begin
                if ExtractFilePath(D.Value)='' then
                  begin
                    SF:=Dictionary.ReplaceStrings(D.Value);
                    SD:='';
                    // first check the target specific path
                    if not FindFileInPath(T.IncludePath,SF,SD,ACPU,AOS) then
                      FindFileInPath(APackage.IncludePath,SF,SD,ACPU,AOS);
                     if SD<>'' then
                       SD:=IncludeTrailingPathDelimiter(SD);
                     D.TargetFileName:=SD+SF;
                  end
                else
                  D.TargetFileName:=D.Value;
                if FileExists(D.TargetFileName) then
                  Log(vlDebug,SDbgResolvedIncludeFile,[D.Value,D.TargetFileName])
                else
                  begin
                    Log(vlWarning,SWarnIncludeFileNotFound,[D.Value,MakeTargetString(ACPU,AOS)]);
                    D.TargetFileName:='';
                  end;
              end;
          end;
      end;
  end;

  procedure FindExampleSource(T:TTarget);
  var
    SD,SF  : String;
  begin
    LogSearchPath('package example',APackage.ExamplePath,ACPU,AOS);
    SD:=Dictionary.ReplaceStrings(T.Directory);
    SF:=Dictionary.ReplaceStrings(T.SourceFileName);
    if SD='' then
      FindFileInPath(APackage.ExamplePath,SF,SD,ACPU,AOS);
    if SD<>'' then
      SD:=IncludeTrailingPathDelimiter(SD);
    T.FTargetSourceFileName:=SD+SF;
    if FileExists(T.TargetSourceFileName) then
      Log(vlDebug,SDbgResolvedSourceFile,[T.SourceFileName,T.TargetSourceFileName])
    else
      begin
        Log(vlWarning,SWarnSourceFileNotFound,[T.SourceFileName,MakeTargetString(ACPU,AOS)]);
        T.FTargetSourceFileName:='';
      end;
  end;

var
  T : TTarget;
  i : Integer;
begin
  if not((ACPU in APackage.CPUs) and (AOS in APackage.OSes)) then
    exit;
  try
    if DoChangeDir and (APackage.Directory<>'') then
      EnterDir(APackage.Directory);
    Dictionary.AddVariable('CPU',CPUToString(ACPU));
    Dictionary.AddVariable('OS',OSToString(AOS));
    For I:=0 to APackage.Targets.Count-1 do
      begin
        T:=APackage.FTargets.TargetItems[I];
        if (ACPU in T.CPUs) and (AOS in T.OSes) then
          begin
            // Debug information
            Log(vlDebug,SDbgResolvingSourcesOfTarget,[T.Name,MakeTargetString(ACPU,AOS)]);
            LogIndent;

            case T.TargetType of
              ttProgram,
              ttUnit,
              ttImplicitUnit :
                begin
                  FindMainSource(T);
                  if T.Dependencies.Count>0 then
                    FindIncludeSources(T);
                end;
              ttExampleUnit,
              ttExampleProgram :
                begin
                  FindExampleSource(T);
                end;
            end;

            LogUnIndent;
          end;
      end;
  finally
    If DoChangeDir and (APackage.Directory<>'') then
      EnterDir('');
  end;
end;


function TBuildEngine.GetUnitDir(APackage:TPackage):String;
begin
  // Retrieve Full directory name where to find the units.
  // The search order is:
  //  - Package in this fpmake.pp
  //  - LocalUnitDir
  //  - GlobalUnitDir
  if (APackage.UnitDir='') and
     (APackage.State=tsCompiled) then
    begin
      APackage.UnitDir:=IncludeTrailingPathDelimiter(FStartDir)+IncludeTrailingPathDelimiter(APackage.Directory)+APackage.GetUnitsOutputDir(Defaults.CPU,Defaults.OS);
    end;
  if (APackage.UnitDir='') and
     (Defaults.LocalUnitDir<>'') then
    begin
      APackage.UnitDir:=IncludeTrailingPathDelimiter(Defaults.LocalUnitDir)+APackage.Name;
      if not SysDirectoryExists(APackage.UnitDir) then
        APackage.UnitDir:='';
    end;
  if APackage.UnitDir='' then
    begin
      APackage.UnitDir:=IncludeTrailingPathDelimiter(Defaults.GlobalUnitDir)+APackage.Name;
      if not SysDirectoryExists(APackage.UnitDir) then
        APackage.UnitDir:=DirNotFound;
    end;
  // Special error marker to prevent searches in case of error
  if APackage.UnitDir=DirNotFound then
    Result:=''
  else
    Result:=APackage.UnitDir;
end;


procedure TBuildEngine.AddDependencyIncludePaths(L:TStrings;ATarget: TTarget);
Var
  I : Integer;
  D : TDependency;
  SD : String;
begin
  For I:=0 to ATarget.Dependencies.Count-1 do
    begin
      D:=ATarget.Dependencies[i];
      if (D.DependencyType=depInclude) and
         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then
        begin
          SD:=ExcludeTrailingPathDelimiter(ExtractFilePath(D.TargetFileName));
          if SD<>'' then
            L.Add(SD);
        end;
    end;
end;


procedure TBuildEngine.AddDependencyUnitPaths(L:TStrings;APackage: TPackage);
Var
  I : Integer;
  P : TPackage;
  D : TDependency;
  S : String;
begin
  For I:=0 to APackage.Dependencies.Count-1 do
    begin
      D:=APackage.Dependencies[i];
      if (D.DependencyType=depPackage) and
         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then
        begin
          P:=TPackage(D.Target);
          If Assigned(P) then
            begin
              // Already processed?
              S:=GetUnitDir(P);
              if L.IndexOf(S)=-1 then
                begin
                  // Add this package and then dependencies
                  L.Add(S);
                  AddDependencyUnitPaths(L,P);
                end;
            end;
        end;
    end;
end;


Function TBuildEngine.GetCompilerCommand(APackage : TPackage; ATarget : TTarget) : String;
Var
  L,Args : TStringList;
  i : Integer;
begin
  Args:=TStringList.Create;
  Args.Duplicates:=dupIgnore;

  Result := '';

  //compiler configuration
  if Defaults.NoFPCCfg then
    Args.Add('-n');

  // Target OS
  Args.Add('-T'+OSToString(Defaults.OS));

  // Compile mode
  If ATarget.Mode<>cmFPC then
    Args.Add('-M'+ModeToString(ATarget.Mode))
  else If Defaults.Mode<>cmFPC then
    Args.Add('-M'+ModeToString(Defaults.Mode));
  // Output file paths
  If ATarget.TargetType in ProgramTargets then
    Args.Add('-FE'+APackage.GetBinOutputDir(Defaults.CPU,Defaults.OS));
  Args.Add('-FU'+APackage.GetUnitsOutputDir(Defaults.CPU,Defaults.OS));
  // Object Path
  L:=TStringList.Create;
  L.Sorted:=true;
  L.Duplicates:=dupIgnore;
  AddConditionalStrings(L,APackage.ObjectPath,Defaults.CPU,Defaults.OS);
  AddConditionalStrings(L,ATarget.ObjectPath,Defaults.CPU,Defaults.OS);
  for i:=0 to L.Count-1 do
    Args.Add('-Fo'+L[i]);
  FreeAndNil(L);
  // Unit Dirs
  L:=TStringList.Create;
  L.Sorted:=true;
  L.Duplicates:=dupIgnore;
  AddDependencyUnitPaths(L,APackage);
  AddConditionalStrings(L,APackage.UnitPath,Defaults.CPU,Defaults.OS);
  AddConditionalStrings(L,ATarget.UnitPath,Defaults.CPU,Defaults.OS);
  for i:=0 to L.Count-1 do
    Args.Add('-Fu'+L[i]);
  FreeAndNil(L);
  // Include Path
  L:=TStringList.Create;
  L.Sorted:=true;
  L.Duplicates:=dupIgnore;
  AddDependencyIncludePaths(L,ATarget);
  AddConditionalStrings(L,APackage.IncludePath,Defaults.CPU,Defaults.OS);
  AddConditionalStrings(L,ATarget.IncludePath,Defaults.CPU,Defaults.OS);
  for i:=0 to L.Count-1 do
    Args.Add('-Fi'+L[i]);
  FreeAndNil(L);
  // Custom Options
  If (Defaults.Options<>'') then
    Args.Add(Defaults.Options);
  If (APackage.Options<>'') then
    Args.Add(APackage.Options);
  If (ATarget.Options<>'') then
    Args.Add(ATarget.Options);
  // Add Filename to compile
  Args.Add(ATarget.TargetSourceFileName);
  // Convert to string
  Result:='';
  for i:=0 to Args.Count-1 do
    Result:=Result+' '+maybequoted(Args[i]);
  Delete(result,1,1);
  Args.Free;
end;


Function TBuildEngine.GetCompiler : String;
Var
  S : String;
begin
  // Cache in FCompiler for speed.
  If (FCompiler='') then
    begin
    FCompiler:=Defaults.Compiler;
    If (ExtractFilePath(FCompiler)='') then
      begin
      S:=FileSearch(FCompiler,GetEnvironmentVariable('PATH'));
      If (S<>'') then
         FCompiler:=S;
      end;
    end;
  Result:=FCompiler;
end;


procedure TBuildEngine.CreateOutputDir(APackage: TPackage);
Var
  D : String;
  i: integer;
begin
  //create a units directory
  D:=APackage.GetUnitsOutputDir(Defaults.CPU,Defaults.OS);
  If not SysDirectoryExists(D) then
    begin
      Log(vlInfo,SInfoCreatingOutputDir,[D]);
      CmdCreateDir(D);
    end;

  //also create a bin directory for programtargets
  For i := 0 to Pred(APackage.Targets.Count) do
    begin
      if APackage.Targets.TargetItems[i].TargetType in ProgramTargets then
        begin
          D:=APackage.GetBinOutputDir(Defaults.CPU,Defaults.OS);
          If not SysDirectoryExists(D) then
            begin
              Log(vlInfo,SInfoCreatingOutputDir,[D]);
              CmdCreateDir(D);
            end;
          //do not continue loop, directory is made anyway
          break;
        end;
    end;
end;


Function TBuildEngine.DependencyOK(ADependency : TDependency) : Boolean;
begin
  Result:=(Defaults.CPU in ADependency.CPUs) and (Defaults.OS in ADependency.OSes);
end;


Function TBuildEngine.TargetOK(ATarget : TTarget) : Boolean;
begin
  Result:=(Defaults.CPU in ATarget.CPUs) and (Defaults.OS in ATarget.OSes);
end;


Function TBuildEngine.PackageOK(APackage : TPackage) : Boolean;
begin
  Result:=(Defaults.CPU in APackage.CPUs) and (Defaults.OS in APackage.OSes);
end;


procedure TBuildEngine.DoBeforeCompile(APackage: TPackage);
begin
  ExecuteCommands(APackage.Commands,caBeforeCompile);
  If Assigned(APackage.BeforeCompile) then
    APackage.BeforeCompile(APackage);
end;


procedure TBuildEngine.DoAfterCompile(APackage: TPackage);
begin
  If Assigned(APackage.AfterCompile) then
    APackage.AfterCompile(APackage);
  ExecuteCommands(APackage.Commands,caAfterCompile);
end;


Function TBuildEngine.NeedsCompile(APackage:TPackage;ATarget: TTarget): Boolean;
Var
  I : Integer;
  D : TDependency;
  T : TTarget;
  OD,OFN : String;
begin
  Result:=False;

  // Forced recompile?
  if FForceCompile then
    Result:=true;

  // Check output file
  if not result then
    begin
      if ATarget.TargetType in ProgramTargets then
        OD:=APackage.GetBinOutputDir(Defaults.CPU,Defaults.OS)
      else
        OD:=APackage.GetUnitsOutputDir(Defaults.CPU,Defaults.OS);
      If (OD<>'') then
        OD:=IncludeTrailingPathDelimiter(OD);
      OFN:=OD+ATarget.GetOutPutFileName(Defaults.OS);
      Result:=Not FileExists(OFN);
      if Result then
        Log(vlDebug,SDbgOutputNotYetAvailable,[OFN]);
    end;

  // Check main source
  If not Result then
    begin
      if FileExists(ATarget.TargetSourceFileName) then
        Result:=FileNewer(ATarget.TargetSourceFileName,OFN)
    end;

  // Check unit and include dependencies
  If not Result then
    begin
      ResolveDependencies(ATarget.Dependencies,ATarget.Collection as TTargets);
      I:=0;
      for i:=0 to ATarget.Dependencies.Count-1 do
        begin
          D:=ATarget.Dependencies[i];
          if (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then
            begin
              case D.DependencyType of
                depUnit :
                  begin
                    T:=TTarget(D.Target);
                    If (T=Nil) then
                      Error(SErrDepUnknownTarget,[ATarget.Name,D.Value]);
                    // If a dependent package is compiled we always need to recompile
                    Log(vldebug, SDbgDependencyOnUnit, [ATarget.Name,T.Name]);
                    Result:=(T.State=tsCompiled);
                    if Result then
                      Log(vldebug, SDbgDependencyUnitRecompiled, [T.Name]);
                  end;
                depInclude :
                  begin
                    if D.TargetFileName<>'' then
                      Result:=FileNewer(D.TargetFileName,OFN)
                  end;
              end;
              if result then
                break;
            end;
        end;
    end;

  if result then
    Log(vlDebug,SDbgMustCompile,[ATarget.Name]);
end;


procedure TBuildEngine.Compile(APackage: TPackage; ATarget: TTarget);
Var
  S : String;
begin
  Log(vlInfo,SInfoCompilingTarget,[ATarget.Name]);
  LogIndent;
  ExecuteCommands(ATarget.Commands,caBeforeCompile);
  If Assigned(ATarget.BeforeCompile) then
    ATarget.BeforeCompile(ATarget);
  S:=GetCompilerCommand(APackage,ATarget);
  ExecuteCommand(GetCompiler,S);
  If Assigned(ATarget.AfterCompile) then
    ATarget.AfterCompile(ATarget);
  ExecuteCommands(ATarget.Commands,caAfterCompile);
  LogUnIndent;
end;


procedure TBuildEngine.CompileDependencies(APackage:TPackage; ATarget: TTarget);
Var
  I : Integer;
  T : TTarget;
  D : TDependency;
begin
  Log(vlDebug, Format(SDbgCompilingDependenciesOfTarget, [ATarget.Name]));
  LogIndent;
  For I:=0 to ATarget.Dependencies.Count-1 do
    begin
      D:=ATarget.Dependencies[i];
      if (D.DependencyType=depUnit) and
         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then
        begin
          T:=TTarget(D.Target);
          if Assigned(T) and (T<>ATarget) then
            begin
              if TargetOK(T) then
                begin
                  // We don't need to compile implicit units, they are only
                  // used for dependency checking
                  if (T.TargetType<>ttImplicitUnit) then
                    begin
                      case T.State of
                        tsNeutral :
                          MaybeCompile(APackage,T);
                        tsConsidering :
                          Log(vlWarning,SWarnCircularTargetDependency,[ATarget.Name,T.Name]);
                      end;
                    end;
                end
              else
                Log(vlWarning, Format(SWarnDepUnitNotFound, [T.Name, MakeTargetString(Defaults.CPU,Defaults.OS)]));
            end
          else
            Error(SErrDepUnknownTarget,[ATarget.Name,D.Value]);
        end;
    end;
  LogUnIndent;
end;


procedure TBuildEngine.MaybeCompile(APackage: TPackage; ATarget: TTarget);
begin
  if ATarget.State<>tsNeutral then
    Error(SErrInvalidState,[ATarget.Name]);
  Log(vlDebug, Format(SDbgConsideringTarget, [ATarget.Name]));
  LogIndent;
  ATarget.FTargetState:=tsConsidering;
  ResolveDependencies(ATarget.Dependencies,ATarget.Collection as TTargets);
  CompileDependencies(APackage, ATarget);
  if NeedsCompile(APackage, ATarget) then
    begin
      Compile(APackage,ATarget);
      ATarget.FTargetState:=tsCompiled;
    end
  else
    ATarget.FTargetState:=tsNoCompile;
  LogUnIndent;
end;


function TBuildEngine.NeedsCompile(APackage: TPackage): Boolean;
Var
  I : Integer;
  P : TPackage;
  D : TDependency;
begin
  Result:=False;

  // Forced recompile?
  if FForceCompile then
    Result:=true;

  // Recompile because of Package Dependencies?
  if not Result then
    begin
       I:=0;
       For I:=0 to APackage.Dependencies.Count-1 do
         begin
           D:=APackage.Dependencies[i];
           if (D.DependencyType=depPackage) and
              (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then
             begin
               P:=TPackage(D.Target);
               if Assigned(P) then
                 begin
                   Result:=(P.State=tsCompiled);
                   if Result then
                     break;
                 end;
             end;
         end;
    end;

  // Recompile a Target of this package?
  If Not Result then
    begin
      try
        If (APackage.Directory<>'') then
          EnterDir(APackage.Directory);
        for i:=0 to APackage.Targets.Count-1 do
          begin
            Result:=NeedsCompile(APackage,APackage.Targets.TargetItems[i]);
            if Result then
              break;
          end;
      Finally
        If (APackage.Directory<>'') then
          EnterDir('');
      end;
    end;

  if result then
    Log(vlDebug,SDbgMustCompile,[APackage.Name]);
end;


function TBuildEngine.CheckExternalPackage(Const APackageName : String):TPackage;
var
  S : String;
  I : Integer;
begin
  // Already checked?
  I:=ExternalPackages.IndexOfName(APackageName);
  if I<>-1 then
    begin
      result:=ExternalPackages.PackageItems[I];
      exit;
    end;
  // Create new external package
  Result:=ExternalPackages.AddPackage(APackageName);
  Result.FTargetState:=tsNotFound;
  // Load unit config
  S:=GetUnitDir(Result);
  if S<>'' then
    begin
      Log(vldebug, SDbgExternalDependency, [APackageName,S]);
      Result.FTargetState:=tsInstalled;
      // Load unit config if it exists
      S:=IncludeTrailingPathDelimiter(S)+UnitConfigFile;
      if FileExists(S) then
        begin
          Log(vlDebug, Format(SDbgLoading, [S]));
          Result.LoadUnitConfigFromFile(S);
        end;
      // Check recursive implicit dependencies
      CompileDependencies(Result);
    end
  else
    Error(SErrDependencyNotFound,[APackageName]);
end;


procedure TBuildEngine.CompileDependencies(APackage: TPackage);
Var
  I : Integer;
  P : TPackage;
  D : TDependency;
begin
  For I:=0 to APackage.Dependencies.Count-1 do
    begin
      D:=APackage.Dependencies[i];
      if (D.DependencyType=depPackage) and
         (Defaults.CPU in D.CPUs) and (Defaults.OS in D.OSes) then
        begin
          P:=TPackage(D.Target);
          If Assigned(P) then
            begin
              case P.State of
                tsNeutral :
                  MaybeCompile(P);
                tsConsidering :
                  Log(vlWarning,SWarnCircularPackageDependency,[APackage.Name,P.Name]);
              end;
            end
          else
            begin
              D.Target:=CheckExternalPackage(D.Value);
              P:=TPackage(D.Target);
            end;
          if (D.RequireChecksum<>$ffffffff) and
             (P.InstalledChecksum<>$ffffffff) and
             (P.InstalledChecksum<>D.RequireChecksum) then
            Log(vlDebug,SDbgPackageChecksumChanged,[P.Name]);
        end;
    end;
end;


procedure TBuildEngine.Compile(APackage: TPackage);
Var
  T : TTarget;
  I : Integer;
begin
  Try
    Log(vlInfo,SInfoCompilingPackage,[APackage.Name]);
    If (APackage.Directory<>'') then
      EnterDir(APackage.Directory);
    CreateOutputDir(APackage);
    Dictionary.AddVariable('UNITSOUTPUTDIR',APackage.GetUnitsOutputDir(Defaults.CPU,Defaults.OS));
    Dictionary.AddVariable('BINOUTPUTDIR',APackage.GetBinOutputDir(Defaults.CPU,Defaults.OS));
    DoBeforeCompile(APackage);
    For I:=0 to APackage.Targets.Count-1 do
      begin
        T:=APackage.Targets.TargetItems[i];
        if (T.TargetType in [ttUnit,ttProgram]) then
          begin
            if TargetOK(T) then
              begin
                if T.State=tsNeutral then
                  MaybeCompile(APackage,T);
              end
            else
              begin
                if not(Defaults.CPU in T.CPUs) then
                  Log(vldebug, Format(SDbgSkippingTargetWrongCPU, [T.Name, CPUsToString(T.CPUs)]));
                if not(Defaults.OS in T.OSes) then
                  Log(vldebug, Format(SDbgSkippingTargetWrongOS, [T.Name, OSesToString(T.OSes)]));
              end;
          end
        else
          log(vldebug, SDbgTargetIsNotAUnitOrProgram,[T.Name]);
      end;
    DoAfterCompile(APackage);
  Finally
    If (APackage.Directory<>'') then
      EnterDir('');
  end;
end;


procedure TBuildEngine.MaybeCompile(APackage: TPackage);
begin
  if APackage.State<>tsNeutral then
    Error(SErrInvalidState,[APackage.Name]);
  Log(vlDebug,SDbgConsideringPackage,[APackage.Name]);
  LogIndent;
  APackage.FTargetState:=tsConsidering;
  ResolveDependencies(APackage.Dependencies,(APackage.Collection as TPackages));
  CompileDependencies(APackage);
  ResolveFileNames(APackage,Defaults.CPU,Defaults.OS);
  If NeedsCompile(APackage) then
    begin
      Compile(APackage);
      APackage.FTargetState:=tsCompiled;
    end
  else
    APackage.FTargetState:=tsNoCompile;
  LogUnIndent;
end;


Function TBuildEngine.InstallPackageFiles(APAckage : TPackage; tt : TTargetType; Const Dest : String):Boolean;
Var
  List : TStringList;
begin
  Result:=False;
  List:=TStringList.Create;
  Try
    APackage.GetInstallFiles(List,[tt],Defaults.CPU, Defaults.OS);
    if (List.Count>0) then
      begin
        Result:=True;
        CmdCopyFiles(List,Dest);
      end;
  Finally
    List.Free;
  end;
end;


procedure TBuildEngine.DoBeforeInstall(APackage: TPackage);
begin
  ExecuteCommands(APackage.Commands,caBeforeInstall);
  If Assigned(APackage.BeforeInstall) then
    APackage.BeforeInstall(APackage);
end;


procedure TBuildEngine.DoAfterInstall(APackage: TPackage);
begin
  If Assigned(APackage.AfterInstall) then
    APackage.AfterInstall(APackage);
  ExecuteCommands(APackage.Commands,caAfterInstall);
end;


procedure TBuildEngine.Install(APackage: TPackage);
Var
  UC,D : String;
  B : Boolean;
begin
  If (Apackage.State<>tsCompiled) then
    MaybeCompile(APackage);
  try
    Log(vlInfo,SInfoInstallingPackage,[APackage.Name]);
    If (APackage.Directory<>'') then
      EnterDir(APackage.Directory);
    DoBeforeInstall(APackage);
    // units
    B:=false;
    D:=IncludeTrailingPathDelimiter(Defaults.UnitInstallDir)+APackage.Name;
    if InstallPackageFiles(APAckage,ttUnit,D) then
      B:=true;
    if InstallPackageFiles(APAckage,ttImplicitUnit,D) then
      B:=true;
    // Unit (dependency) configuration if there were units installed
    if B then
      begin
        UC:=IncludeTrailingPathDelimiter(D)+UnitConfigFile;
        Log(vlInfo, Format(SDbgGenerating, [UC]));
        APackage.SaveUnitConfigToFile(UC,Defaults.CPU,Defaults.OS);
      end;
    // Programs
    D:=IncludeTrailingPathDelimiter(Defaults.BinInstallDir);
    InstallPackageFiles(APAckage,ttProgram,D);
    // Done.
    APackage.FTargetState:=tsInstalled;
    DoAfterInstall(APackage);
  Finally
    If (APackage.Directory<>'') then
      EnterDir('');
  end;
end;


procedure TBuildEngine.DoBeforeArchive(APackage: TPackage);
begin
  ExecuteCommands(APackage.Commands,caBeforeArchive);
  If Assigned(APackage.BeforeArchive) then
    APackage.BeforeArchive(APackage);
end;


procedure TBuildEngine.DoAfterArchive(APackage: TPackage);
begin
  If Assigned(APackage.AfterArchive) then
    APackage.AfterArchive(APackage);
  ExecuteCommands(APackage.Commands,caAfterArchive);
end;


procedure TBuildEngine.Archive(APackage: TPackage);
Var
  L : TStringList;
  A : String;
  i: integer;
  ICPU : TCPU;
  IOS  : TOS;
begin
  A:=FStartDir+ APackage.FileName + ZipExt;
  Log(vlInfo,SInfoArchivingPackage,[APackage.Name,A]);
  try
    If (APackage.Directory<>'') then
      EnterDir(APackage.Directory);
    DoBeforeArchive(Apackage);
    L:=TStringList.Create;
    L.Sorted:=true;
    L.Duplicates:=dupIgnore;
    Try
      // Add fpmake.pp & manifest.xml always
      L.Add(FPMakePPFile);
      L.Add(ManifestFile);
      //get all files from all targets
      for ICPU:=Low(TCPU) to high(TCPU) do
        for IOS:=Low(TOS) to high(TOS) do
          if OSCPUSupported[IOS,ICPU] then
            begin
              ResolveFileNames(APackage,ICPU,IOS,false);
              APackage.GetArchiveFiles(L, ICPU, IOS);
            end;
      //from sources
      for i := 0 to APackage.Sources.Count-1 do
        L.Add(APackage.Sources[i].Name);

      //show all files
      for i := 0 to L.Count-1 do
        Log(vlDebug, Format(SDbgArchivingFile, [L[i]]));

{$ifdef HAS_UNIT_ZIPPER}
      if not Assigned(ArchiveFilesProc) then
        begin
          FZipFile := TZipper.Create;
          FZipFile.ZipFiles(A, L);
        end
      else
{$endif HAS_UNIT_ZIPPER}
        CmdArchiveFiles(L,A);
    Finally
      L.Free;

{$ifdef HAS_UNIT_ZIPPER}
      if not Assigned(ArchiveFilesProc) then
        FreeAndNil(FZipFile);
{$endif HAS_UNIT_ZIPPER}
    end;
    DoAfterArchive(Apackage);
  Finally
    If (APackage.Directory<>'') then
      EnterDir('');
  end;
end;


procedure TBuildEngine.DoBeforeClean(APackage: TPackage);
begin
  ExecuteCommands(APackage.Commands,caBeforeClean);
  If Assigned(APackage.BeforeClean) then
    APackage.BeforeClean(APackage);
end;


procedure TBuildEngine.DoAfterClean(APackage: TPackage);
begin
  If Assigned(APackage.AfterClean) then
    APackage.AfterClean(APackage);
  ExecuteCommands(APackage.Commands,caAfterClean);
end;


procedure TBuildEngine.Clean(APackage: TPackage);
Var
  List : TStringList;
begin
  Log(vlInfo,SInfoCleaningPackage,[APackage.Name]);
  try
    If (APackage.Directory<>'') then
      EnterDir(APackage.Directory);
    DoBeforeClean(Apackage);
    List:=TStringList.Create;
    try
      APackage.GetCleanFiles(List,Defaults.CPU,Defaults.OS);
      if (List.Count>0) then
        CmdDeleteFiles(List);
    Finally
      List.Free;
    end;
    DoAfterClean(Apackage);
  Finally
    If (APackage.Directory<>'') then
      EnterDir('');
  end;
end;


Procedure TBuildEngine.Manifest(APackage : TPackage);
Var
  L : TStrings;
  PD,
  MF : String;
begin
  L:=TStringList.Create;
  Try
    Log(vlInfo, Format(SInfoManifestPackage,[APackage.Name]));
    PD:=APackage.Directory;
    if PD<>'' then
      PD:=IncludeTrailingPathDelimiter(PD);
    MF:=PD+ManifestFile;
    Log(vlDebug, Format(SDbgGenerating, [MF]));
    L.Add('<?xml version="1.0"?>');
    L.Add('<packages>');
    APackage.GetManifest(L);
    L.Add('</packages>');
    L.SaveToFile(MF);
  Finally
    L.Free;
  end;
end;


procedure TBuildEngine.Compile(Packages: TPackages);
Var
  I : Integer;
  P : TPackage;
begin
  If Assigned(BeforeCompile) then
    BeforeCompile(Self);
  For I:=0 to Packages.Count-1 do
    begin
      P:=Packages.PackageItems[i];
      If PackageOK(P) then
        MaybeCompile(P);
    end;
  If Assigned(AfterCompile) then
    AfterCompile(Self);
end;


procedure TBuildEngine.Install(Packages: TPackages);
Var
  I : Integer;
  P : TPackage;
begin
  If Assigned(BeforeInstall) then
    BeforeInstall(Self);
  For I:=0 to Packages.Count-1 do
    begin
      P:=Packages.PackageItems[i];
      If PackageOK(P) then
        Install(P);
    end;
  If Assigned(AfterInstall) then
    AfterInstall(Self);
end;


procedure TBuildEngine.Archive(Packages: TPackages);
Var
  I : Integer;
  P : TPackage;
begin
  If Assigned(BeforeArchive) then
    BeforeArchive(Self);
  Log(vlDebug, SDbgBuildEngineArchiving);
  For I:=0 to Packages.Count-1 do
    begin
      P:=Packages.PackageItems[i];
      Archive(P);
    end;
  If Assigned(AfterArchive) then
    AfterArchive(Self);
end;


procedure TBuildEngine.Manifest(Packages: TPackages);
Var
  I : Integer;
  P : TPackage;
begin
  If Assigned(BeforeManifest) then
    BeforeManifest(Self);
  Log(vlDebug, SDbgBuildEngineGenerateManifests);
  For I:=0 to Packages.Count-1 do
    begin
      P:=Packages.PackageItems[i];
      Manifest(P);
    end;
  If Assigned(AfterManifest) then
    AfterManifest(Self);
end;


procedure TBuildEngine.Clean(Packages: TPackages);
Var
  I : Integer;
  P : TPackage;
begin
  If Assigned(BeforeClean) then
    BeforeClean(Self);
  Log(vldebug, SDbgBuildEngineCleaning);
  For I:=0 to Packages.Count-1 do
    begin
    P:=Packages.PackageItems[i];
    If PackageOK(P) then
      Clean(P);
    end;
  If Assigned(AfterClean) then
    AfterClean(Self);
end;


{****************************************************************************
                               TFPVersion
****************************************************************************}

function TFPVersion.GetAsString: String;
begin
  if Empty then
    Result:='<none>'
  else
    Result:=Format('%d.%d.%d-%d',[Major,Minor,Micro,Build]);
end;

function TFPVersion.GetEmpty: Boolean;
begin
  Result:=(Major=0) and (Minor=0) and (Micro=0) and (Build=0);
end;

procedure TFPVersion.SetAsString(const AValue: String);

  Function NextDigit(sep : Char; var V : string) : integer;

  Var
    P : Integer;

  begin
    P:=Pos(Sep,V);
    If (P=0) then
      P:=Length(V)+1;
    Result:=StrToIntDef(Copy(V,1,P-1),-1);
    If Result<>-1 then
      Delete(V,1,P)
    else
      Result:=0;
  end;

Var
  V : String;
begin
  Clear;
  // Special support for empty version string
  if (AValue='') or (AValue='<none>') then
    exit;
  V:=AValue;
  Major:=NextDigit('.',V);
  Minor:=NextDigit('.',V);
  Micro:=NextDigit('-',V);
  Build:=NextDigit(#0,V);
end;

procedure TFPVersion.Clear;
begin
  Micro:=0;
  Major:=0;
  Minor:=0;
  Build:=0;
end;

procedure TFPVersion.Assign(Source: TPersistent);

Var
  V : TFPVersion;

begin
  if Source is TFPVersion then
    begin
    V:=Source as TFPVersion;
    Major:=V.Major;
    Minor:=V.Minor;
    Micro:=V.Micro;
    Build:=V.Build;
    end
  else
    inherited Assign(Source);
end;

function TFPVersion.CompareVersion(AVersion: TFPVersion): Integer;
begin
  Result:=Major-AVersion.Major;
  If (Result=0) then
    begin
      Result:=Minor-AVersion.Minor;
      if (Result=0) then
        begin
          Result:=Micro-AVersion.Micro;
          If (Result=0) then
            Result:=Build-AVersion.Build;
        end;
    end;
end;

function TFPVersion.SameVersion(AVersion: TFPVersion): Boolean;
begin
  Result:=CompareVersion(AVersion)=0;
end;


{****************************************************************************
                                 TTarget
****************************************************************************}

constructor TTarget.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FInstall:=True;
  FCPUs:=AllCPUs;
  FOSes:=AllOSes;
  FUnitPath:=TConditionalStrings.Create(TConditionalString);
  FIncludePath:=TConditionalStrings.Create(TConditionalString);
  FObjectPath:=TConditionalStrings.Create(TConditionalString);
  FDependencies:=TDependencies.Create(TDependency);
  FCommands:=TCOmmands.Create(TCommand);
end;


destructor TTarget.Destroy;
begin
  FreeAndNil(FUnitPath);
  FreeAndNil(FObjectPath);
  FreeAndNil(FIncludePath);
  FreeAndNil(FDependencies);
  FreeAndNil(FCommands);
  inherited Destroy;
end;


function TTarget.GetSourceFileName: String;
begin
  Result:=Name+FExtension;
end;


function TTarget.GetUnitFileName: String;
begin
  Result:=Name+UnitExt;
end;


function TTarget.GetObjectFileName: String;
begin
  Result:=Name+ObjExt;
end;


function TTarget.GetRSTFileName: String;
begin
  Result:=Name+RSText;
end;


function TTarget.GetProgramFileName(AOS : TOS): String;
begin
  if AOS in [Go32v2,Win32,Win64,OS2] then
    Result:=Name+ExeExt
  else
    Result:=Name;
end;


function TTarget.GetOutputFileName(AOs: TOS): String;
begin
  if TargetType in UnitTargets then
    Result:=GetUnitFileName
  else
    Result:=GetProgramFileName(AOs);
end;


procedure TTarget.SetName(const AValue: String);
Var
  D,N,E : String;
begin
  N:=FixPath(AValue);
  D:=ExtractFilePath(N);
  E:=ExtractFileExt(N);
  N:=ExtractFileName(N);
  inherited SetName(Copy(N,1,Length(N)-Length(E)));
  FExtension:=E;
  FDirectory:=D;
end;


procedure TTarget.GetCleanFiles(List: TStrings; const APrefixU, APrefixB : String; ACPU: TCPU; AOS : TOS);
begin
  If not(ACPU in CPUs) or not(AOS in OSes) then
    exit;
  List.Add(APrefixU + ObjectFileName);
  If (TargetType in [ttUnit,ttImplicitUnit,ttExampleUnit, ttCleanOnlyUnit]) then
    List.Add(APrefixU + UnitFileName)
  else If (TargetType in [ttProgram,ttExampleProgram]) then
    List.Add(APrefixB + GetProgramFileName(AOS));
  If ResourceStrings then
    List.Add(APrefixU + RSTFileName);
  // Maybe add later ?  AddConditionalStrings(List,CleanFiles);
end;


procedure TTarget.GetInstallFiles(List: TStrings; const APrefixU, APrefixB: String; ACPU: TCPU; AOS : TOS);
begin
  If not(ACPU in CPUs) or not(AOS in OSes) then
    exit;
  If Not (TargetType in [ttProgram,ttExampleProgram]) then
    List.Add(APrefixU + ObjectFileName);
  If (TargetType in [ttUnit,ttImplicitUnit,ttExampleUnit]) then
    List.Add(APrefixU + UnitFileName)
  else If (TargetType in [ttProgram,ttExampleProgram]) then
    List.Add(APrefixB + GetProgramFileName(AOS));
  If ResourceStrings then
    List.Add(APrefixU + RSTFileName);
  // Maybe add later ?  AddConditionalStrings(List,InstallFiles);
end;


procedure TTarget.GetArchiveFiles(List: TStrings; ACPU: TCPU; AOS : TOS);
var
  i : integer;
  D : TDependency;
begin
  If not(ACPU in CPUs) or not(AOS in OSes) then
    exit;
  // Main source
  if TargetSourceFileName<>'' then
    List.Add(TargetSourceFileName);
  // Includes
  for i:=0 to Dependencies.Count-1 do
    begin
      D:=Dependencies[i];
      if (D.DependencyType=depInclude) and
         (D.TargetFileName<>'') then
        List.Add(D.TargetFileName);
    end;
end;


{****************************************************************************
                                 TSource
****************************************************************************}

constructor TSource.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;


destructor TSource.Destroy;
begin
  inherited Destroy;
end;


{****************************************************************************
                                 TCommands
****************************************************************************}

function TCommands.GetCommand(const Dest : String): TCommand;
begin
  Result:=TCommand(ItemByName(Dest));
end;

function TCommands.GetCommandItem(Index : Integer): TCommand;
begin
  Result:=TCommand(Items[Index]);
end;


procedure TCommands.SetCommandItem(Index : Integer; const AValue: TCommand);
begin
  Items[Index]:=AValue;
end;


Function TCommands.AddCommand(const Cmd: String) : TCommand;
begin
  Result:=AddCommand(fdefaultAt,Cmd,'','','');
end;


function TCommands.AddCommand(const Cmd, Options: String): TCommand;
begin
  Result:=AddCommand(fdefaultAt,Cmd,Options,'','');
end;


function TCommands.AddCommand(const Cmd, Options, Dest, Source: String ): TCommand;
begin
  Result:=AddCommand(fdefaultAt,Cmd,options,Dest,Source);
end;


Function TCommands.AddCommand(At: TCommandAt; const Cmd: String) : TCommand;
begin
  Result:=AddCommand(At,Cmd,'','','');
end;


function TCommands.AddCommand(At: TCommandAt; const Cmd, Options: String  ): TCommand;
begin
  Result:=AddCommand(At,Cmd,Options,'','');
end;


function TCommands.AddCommand(At: TCommandAt; const Cmd, Options, Dest, Source: String): TCommand;
begin
  Result:=Add as TCommand;
  Result.Command:=Cmd;
  Result.Options:=Options;
  Result.At:=At;
  Result.SourceFile:=Source;
  Result.DestFile:=Dest;
end;


{****************************************************************************
                           TConditionalString
****************************************************************************}

Constructor TConditionalString.Create;
begin
  inherited Create;
end;


{****************************************************************************
                           TConditionalStrings
****************************************************************************}

Constructor TConditionalStrings.Create(AClass:TConditionalStringClass);
begin
  inherited Create;
  FCSClass:=AClass;
end;


function TConditionalStrings.GetConditionalString(Index : Integer): TConditionalString;
begin
  Result:=TConditionalString(Items[Index]);
end;


procedure TConditionalStrings.SetConditionalString(Index : Integer; const AValue: TConditionalString);
begin
  Items[Index]:=AValue;
end;


Function TConditionalStrings.Add(Const Value : String) : TConditionalString;
begin
  result:=Add(Value,AllCPUs,AllOSes);
end;


{$ifdef cpu_only_overloads}
Function TConditionalStrings.Add(Const Value : String;const CPUs:TCPUs) : TConditionalString;
begin
  result:=Add(Value,CPUs,AllOSes);
end;
{$endif cpu_only_overloads}


Function TConditionalStrings.Add(Const Value : String;const OSes:TOSes) : TConditionalString;
begin
  result:=Add(Value,AllCPUs,OSes);
end;


Function TConditionalStrings.Add(Const Value : String;const CPUs:TCPUs;const OSes:TOSes) : TConditionalString;
begin
  Result:=FCSClass.Create;
  Result.Value:=Value;
  Result.OSes:=OSes;
  Result.CPUs:=CPUs;
  inherited Add(Result);
end;


{****************************************************************************
                                TDependency
****************************************************************************}

Constructor TDependency.Create;
begin
  inherited Create;
  FVersion:=TFPVersion.Create;
end;


Destructor TDependency.Destroy;
begin
  FreeAndNil(FVersion);
end;


Function TDependency.GetVersion : string;
begin
  result:=FVersion.AsString;
end;


Procedure TDependency.SetVersion(const V : string);
begin
  FVersion.AsString:=V;
end;


{****************************************************************************
                                TDependencies
****************************************************************************}

function TDependencies.GetDependency(Index : Integer): TDependency;
begin
  Result:=TDependency(Items[Index]);
end;


procedure TDependencies.SetDependency(Index : Integer; const AValue: TDependency);
begin
  Items[Index]:=AValue;
end;


Function TDependencies.Add(Const Value : String) : TDependency;
begin
  result:=Add(Value,AllCPUs,AllOSes);
end;


{$ifdef cpu_only_overloads}
Function TDependencies.Add(Const Value : String;const CPUs:TCPUs) : TDependency;
begin
  result:=Add(Value,CPUs,AllOSes);
end;
{$endif cpu_only_overloads}


Function TDependencies.Add(Const Value : String;const OSes:TOSes) : TDependency;
begin
  result:=Add(Value,AllCPUs,OSes);
end;


Function TDependencies.Add(Const Value : String;const CPUs:TCPUs;const OSes:TOSes) : TDependency;
begin
  Result:=inherited Add(Value,CPUs,OSes) as TDependency;
  Result.Target:=nil;
  Result.FDependencyType:=depPackage;
end;


Function TDependencies.AddUnit(Const Value : String) : TDependency;
begin
  result:=AddUnit(Value,AllCPUs,AllOSes);
end;


{$ifdef cpu_only_overloads}
Function TDependencies.AddUnit(Const Value : String;const CPUs:TCPUs) : TDependency;
begin
  result:=AddUnit(Value,CPUs,AllOSes);
end;
{$endif cpu_only_overloads}


Function TDependencies.AddUnit(Const Value : String;const OSes:TOSes) : TDependency;
begin
  result:=AddUnit(Value,AllCPUs,OSes);
end;


Function TDependencies.AddUnit(Const Value : String;const CPUs:TCPUs;const OSes:TOSes) : TDependency;
begin
  Result:=inherited Add(Value,CPUs,OSes) as TDependency;
  Result.Target:=nil;
  Result.FDependencyType:=depUnit;
end;


Function TDependencies.AddInclude(Const Value : String) : TDependency;
begin
  result:=AddInclude(Value,AllCPUs,AllOSes);
end;


{$ifdef cpu_only_overloads}
Function TDependencies.AddInclude(Const Value : String;const CPUs:TCPUs) : TDependency;
begin
  result:=AddInclude(Value,CPUs,AllOSes);
end;
{$endif cpu_only_overloads}


Function TDependencies.AddInclude(Const Value : String;const OSes:TOSes) : TDependency;
begin
  result:=AddInclude(Value,AllCPUs,OSes);
end;


Function TDependencies.AddInclude(Const Value : String;const CPUs:TCPUs;const OSes:TOSes) : TDependency;
Var
  N : String;
begin
  N:=FixPath(Value);
  if ExtractFileExt(N)='' then
    ChangeFileExt(N,IncExt);
  Result:=inherited Add(N,CPUs,OSes) as TDependency;
  Result.FDependencyType:=depInclude;
end;


{****************************************************************************
                               TValueItem
****************************************************************************}

constructor TValueItem.Create(AValue: String);
begin
  FValue:=AValue;
end;


{****************************************************************************
                              TFunctionItem
****************************************************************************}

constructor TFunctionItem.Create(AFunc: TReplaceFunction);
begin
  FFunc:=AFunc;
end;


{****************************************************************************
                                 TDictionary
****************************************************************************}

constructor TDictionary.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList:=TStringList.Create;
  FList.Sorted:=True;
  FList.Duplicates:=dupError;
end;


destructor TDictionary.Destroy;
Var
  I : Integer;
begin
  For I:=0 to Flist.Count-1 do
    FList.Objects[i].Free;
  FreeAndNil(FList);
  inherited Destroy;
end;


procedure TDictionary.AddVariable(const AName, Value: String);
Var
  I : Integer;
begin
  I:=Flist.IndexOf(AName);
  If I=-1 then
    I:=FList.Add(Aname)
  else
    Flist.Objects[i].Free;
  Flist.Objects[i]:=TValueItem.Create(Value);
end;


procedure TDictionary.AddFunction(const AName: String; FReplacement: TReplaceFunction);
Var
  I : Integer;
begin
  I:=Flist.IndexOf(AName);
  If I=-1 then
    I:=Flist.Add(AName)
  else
    Flist.Objects[i].Free;
  Flist.Objects[i]:=TFunctionItem.Create(FReplacement);
end;


procedure TDictionary.RemoveItem(const AName: String);
Var
  I : Integer;
begin
  I:=Flist.IndexOf(AName);
  If (I<>-1) then
    begin
    FList.Objects[i].Free;
    FList.Delete(I);
    end;
end;


function TDictionary.GetValue(const AName: String): String;
begin
  Result:=GetValue(AName,'');
end;


function TDictionary.GetValue(const AName,Args: String): String;
Var
  O : TObject;
  I : Integer;
begin
  I:=Flist.IndexOf(AName);
  If (I=-1) then
    Raise EDictionaryError.CreateFmt(SErrNoDictionaryItem,[AName]);
  O:=Flist.Objects[I];
  If O is TValueItem then
    Result:=TValueItem(O).FValue
  else
    Result:=TFunctionItem(O).FFunc(AName,Args);
end;


function TDictionary.ReplaceStrings(Const ASource: String): String;
Var
  S,FN,FV : String;
  P: Integer;
begin
  Result:='';
  S:=ASource;
  P:=Pos('$(',S);
  While (P<>0) do
    begin
      Result:=Result+Copy(S,1,P-1);
      Delete(S,1,P+1);
      P:=Pos(')',S);
      FN:=Copy(S,1,P-1);
      Delete(S,1,P);
      P:=Pos(' ',FN);
      If (P<>0) then // function arguments ?
        begin
        FV:=FN;
        FN:=Copy(FN,1,P);
        System.Delete(FV,1,P);
        end
      else
        FV:='';
      Result:=Result+GetValue(FN,FV);
      P:=Pos('$(',S);
    end;
  Result:=Result+S;
end;


Function Substitute(Const Source : String; Macros : Array of string) : String;
Var
  I : Integer;
begin
  I:=0;
  While I<High(Macros) do
    begin
      Dictionary.AddVariable(Macros[i],Macros[I+1]);
      Inc(I,2);
    end;
  Result:=Dictionary.ReplaceStrings(Source);
  While I<High(Macros) do
    begin
      Dictionary.RemoveItem(Macros[i]);
      Inc(I,2);
    end;
end;


{****************************************************************************
                                 Default Instances
****************************************************************************}

var
  DefInstaller : TCustomInstaller;

Function Installer(InstallerClass: TInstallerClass): TCustomInstaller;
begin
  If Not Assigned(DefInstaller) then
    DefInstaller:=InstallerClass.Create(Nil);
  Result:=DefInstaller;
end;


Function Installer: TCustomInstaller;
begin
  Result := Installer(TFPCInstaller);
end;


Initialization
  OnGetApplicationName:=@GetFPMakeName;

Finalization
  FreeAndNil(DefInstaller);
  FreeAndNil(Dictionary);
  FreeAndNil(Defaults);
end.
