{$Mode objfpc}
{$H+}
{$define debug}
unit fpmkunit;

Interface

uses SysUtils,Classes;

Type

  TFileType = (ftSource,ftUnit,ftObject,ftResource,ftExecutable,ftStaticLibrary,
               ftSharedLibrary);
  TFileTypes = set of TFileType;

  TOS = (Amiga,Atari,Darwin,FreeBSD,Go32v2,Linux,MacOS,MorphOS,NetBSD,
         Netware,NetwLibc,OpenBSD,OS2,PalmOS,Solaris,Win32,Emx);
  TOSes = Set of TOS;
  
  TCPU = (Arm,I386,PPC,SPARC,X86_64);
  TCPUS = Set of TCPU;
  
  TCompilerMode = (FPC,TP,ObjFPC,Delphi,MacPas);
  TCompilerModes = Set of TCompilerMode;
  
  TTargetType = (ttUnit,ttProgram,ttExampleUnit,ttExampleProgram);
  TTargetTypes = set of TTargetType;
  
  TTargetState = (tsNeutral,tsCompiling,tsCompiled,tsInstalled);
  TTargetStates = Set of TTargetState;

  TRunMode = (rmHelp,rmCompile,rmBuild,rmInstall,rmArchive,rmClean,rmDownload);

  TVerboseLevel = (vlError,vlWarning,vlInfo,vlCompare,vlCommand,vldebug);
  TVerboseLevels = Set of TVerboseLevel;
  
  TLogEvent = Procedure (Level : TVerboseLevel; Const Msg : String) of Object;


  
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
    Function IndexOfName(AName : String) : Integer;
    Function ItemByName(AName : String) : TNamedItem;
    Property UniqueNames : Boolean Read FUniqueNames;
  end;
  
  { TNamedItemList }
  
  TNamedItemList = Class(TList)
  private
    function GetNamedItem(Index : Integer): TNamedItem;
    procedure SetNamedItem(Index : Integer; const AValue: TNamedItem);
  public
    Function IndexOfName(AName : String) : Integer;
    Function ItemByName(ANAme : String) : TNamedItem;
    Property NamedItems[Index : Integer] : TNamedItem Read GetNamedItem Write SetNamedItem; default;
  end;
  

  { TFileItem }

  TFileItem = Class(TNamedItem)
  private
    FFileType: TFileType;
    function GetExtension: String;
    function GetPath: String;
  Public
    Property Extension : String Read GetExtension;
    Property FileType : TFileType Read FFileType Write FFileType;
    Property Path : String Read GetPath;
  end;
  
  { TFileItems }

  TFileItems = Class(TNamedCollection)
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
    FMode: TCompilerMode;
    FResourceStrings: Boolean;
    FObjectPath,
    FUnitPath,
    FIncludePath,
    FDependencies: TStrings;
    FDirectory: String;
    FExtension: String;
    FFileType: TFileType;
    FOptions: String;
    FOSes: TOSes;
    FFPCTarget: String;
    FTargetState: TTargetState;
    FTargetType: TTargetType;
    function GetHasStrings(AIndex: integer): Boolean;
    function GetStrings(AIndex: integer): TStrings;
    procedure SetStrings(AIndex: integer; const AValue: TStrings);
  Protected
    Function GetSourceFileName : String; virtual;
    Function GetUnitFileName : String; virtual;
    Function GetObjectFileName : String; virtual;
    Function GetRSTFileName : String; Virtual;
    Function GetProgramFileName(AnOS : TOS) : String; Virtual;
  Public
    Constructor Create(ACollection : TCollection); override;
    Destructor Destroy; override;
    Function GetOutputFileName (AOs : TOS) : String; Virtual;
    Function NeedsCompile(ATargetDir : String;AOs : TOS) : Boolean; virtual;
    procedure SetName(const AValue: String);override;
    Procedure GetCleanFiles(List : TStrings; APrefix : String; AnOS : TOS); virtual;
    Procedure GetInstallFiles(List : TStrings; APrefix : String; AnOS : TOS); virtual;
    Procedure GetArchiveFiles(List : TStrings; APrefix : String; AnOS : TOS); virtual;
    Property HasUnitPath : Boolean Index 0 Read GetHasStrings;
    Property HasObjectPath : Boolean Index 1 Read GetHasStrings;
    Property HasIncludePath : Boolean Index 2 Read GetHasStrings;
    Property HasDependencies : Boolean Index 3 Read GetHasStrings;
    Property UnitPath : TStrings Index 0 Read GetStrings Write SetStrings;
    Property ObjectPath : TStrings Index 1  Read GetStrings Write SetStrings;
    Property IncludePath : TStrings Index 2 Read GetStrings Write SetStrings;
    Property Dependencies : TStrings Index 3 Read GetStrings Write SetStrings;
    Property State : TTargetState Read FTargetState;
    Property TargetType : TTargetType Read FTargetType Write FTargetType;
    Property OS : TOSes Read FOSes Write FOSes;
    Property Mode : TCompilerMode Read FMode Write FMode;
    Property Options : String Read FOptions Write Foptions;
    Property SourceFileName: String Read GetSourceFileName ;
    Property UnitFileName : String Read GetUnitFileName;
    Property ObjectFileName : String Read GetObjectFileName;
    Property RSTFileName : String Read GetRSTFileName;
    Property CPU : TCPUs Read FCPUs Write FCPUs;
    Property FPCTarget : String Read FFPCTarget Write FFPCTarget;
    Property Extension : String Read FExtension Write FExtension;
    Property FileType : TFileType Read FFileType Write FFileType;
    Property Directory : String Read FDirectory Write FDirectory;
    Property ResourceStrings : Boolean Read FResourceStrings Write FResourceStrings;
    Property Install : Boolean Read FInstall Write FInstall;
    // Events.
    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FBeforeCompile;
    Property AfterCompile : TNotifyEvent Read FAfterCompile Write FAfterCompile;
    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBeforeClean;
    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterClean;
  end;

  { TTargets }

  TTargets = Class(TNamedCollection)
  private
    FDefaultCPU: TCPUs;
    FDefaultDir : String;
    FDefaultOS: TOSes;
    function GetTargetItem(Index : Integer): TTarget;
    function GetTarget(AName : String): TTarget;
    procedure SetDefaultDir(const AValue: String);
    procedure SetTargetItem(Index : Integer; const AValue: TTarget);
    Procedure ApplyDefaults(ATarget : TTarget);
  Public
    Procedure ResetDefaults;
    Function AddUnit(AUnitName : String) : TTarget;
    Function AddProgram(AProgramName : String) : TTarget;
    Function AddExampleUnit(AUnitName : String) : TTarget;
    Function AddExampleProgram(AProgramName : String) : TTarget;
    Property Targets[AName : String] : TTarget Read GetTarget; default;
    Property TargetItems[Index : Integer] : TTarget Read GetTargetItem Write SetTargetItem;
    Property DefaultDir : String Read FDefaultDir Write SetDefaultDir;
    Property DefaultOS : TOSes Read FDefaultOS Write FDefaultOS;
    Property DefaultCPU : TCPUs Read FDefaultCPU Write FDefaultCPU;
  end;
  
  { TPackage }
  
  TPackage = Class(TNamedItem) // Maybe descend from/use TTarget ?
  private
    FAfterArchive: TNotifyEvent;
    FAfterClean: TNotifyEvent;
    FAfterCompile: TNotifyEvent;
    FAfterDownload: TNotifyEvent;
    FAfterInstall: TNotifyEvent;
    FBeforeArchive: TNotifyEvent;
    FBeforeClean: TNotifyEvent;
    FBeforeCompile: TNotifyEvent;
    FBeforeDownload: TNotifyEvent;
    FBeforeInstall: TNotifyEvent;
    FUnitPath,
    FObjectPath,
    FIncludePath,
    FCleanFiles,
    FArchiveFiles,
    FInstallFiles,
    FDependencies: TStrings;
    FCPU: TCPUs;
    FOS: TOses;
    FTargetState: TTargetState;
    FTargets: TTargets;
    FDirectory: String;
    FOptions: String;
    FAuthor: String;
    FLicense: String;
    FURL: String;
    FVersion: String;
    function GetHasStrings(AIndex: integer): Boolean;
    function GetStrings(AIndex: integer): TStrings;
    procedure SetStrings(AIndex: integer; const AValue: TStrings);
  Public
    constructor Create(ACollection: TCollection); override;
    destructor destroy; override;
    Function AddTarget(AName : String) : TTarget;
    Procedure AddDependency(AName : String);
    Procedure AddInstallFile(AFileName : String);
    Procedure GetCleanFiles(List : TStrings; Const APrefix : String; AOS : TOS); virtual;
    procedure GetInstallFiles(List: TStrings;Types : TTargetTypes;Const APrefix : String; AOS : TOS);
    Procedure GetArchiveFiles(List : TStrings; Const APrefix : String; AOS : TOS); virtual;
    Function NeedsCompile(ATargetDir : String; AOS : TOS) : Boolean; virtual;
    Property Version : String Read FVersion Write FVersion;
    Property URL : String Read FURL Write FURL;
    Property Author : String Read FAuthor Write FAuthor;
    Property License : String Read FLicense Write FLicense;
    Property Directory : String Read FDirectory Write FDirectory;
    // Compiler options.
    Property OS : TOses Read FOS Write FOS;
    Property CPU : TCPUs Read FCPU Write FCPU;
    Property Options: String Read FOptions Write FOptions;
    Property HasUnitPath : Boolean Index 0 Read GetHasStrings;
    Property HasObjectPath : Boolean Index 1 Read GetHasStrings;
    Property HasIncludePath : Boolean Index 2 Read GetHasStrings;
    Property HasDependencies : Boolean Index 3 Read GetHasStrings;
    Property HasInstallFiles: Boolean Index 4 Read GetHasStrings;
    Property HasCleanFiles : Boolean Index 5 Read GetHasStrings;
    Property HasArchiveFiles : Boolean Index 6 Read GetHasStrings;
    Property UnitPath : TStrings Index 0 Read GetStrings Write SetStrings;
    Property ObjectPath : TStrings Index 1  Read GetStrings Write SetStrings;
    Property IncludePath : TStrings Index 2 Read GetStrings Write SetStrings;
    // Targets and dependencies
    Property Dependencies : TStrings Index 3 Read GetStrings Write SetStrings;
    Property InstallFiles : TStrings Index 4 Read GetStrings Write SetStrings;
    Property CleanFiles : TStrings Index 5 Read GetStrings Write SetStrings;
    Property ArchiveFiles : TStrings Index 6 Read GetStrings Write SetStrings;
    Property State : TTargetState Read FTargetState;
    Property Targets : TTargets Read FTargets;
    // events
    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FBeforeCompile;
    Property AfterCompile : TNotifyEvent Read FAfterCompile Write FAfterCompile;
    Property BeforeInstall : TNotifyEvent Read FBeforeInstall Write FBeforeInstall;
    Property AfterInstall : TNotifyEvent Read FAfterInstall Write FAfterInstall;
    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBeforeClean;
    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterClean;
    Property BeforeArchive : TNotifyEvent Read FBeforeArchive Write FBeforeArchive;
    Property AfterArchive : TNotifyEvent Read FAfterArchive Write FAfterArchive;
    Property BeforeDownload : TNotifyEvent Read FBeforeDownload Write FBeforeDownload;
    Property AfterDownload : TNotifyEvent Read FAfterDownload Write FAfterDownload;
  end;

  { TPackages }

  TPackages = Class(TNamedCollection)
  private
    function GetPackage(AName : String): TPackage;
    function GetPackageItem(AIndex : Integer): TPackage;
    procedure SetPackageItem(AIndex : Integer; const AValue: TPackage);
  Public
    Function AddPackage(Const AName : String) : TPackage;
    Property Packages[AName : String] : TPackage Read GetPackage ; Default;
    Property PackageItems[AIndex : Integer] : TPackage Read GetPackageItem Write SetPackageItem;
  end;

  { TDefaults }
  
  TDefaults = Class(TPersistent)
  Private
    FArchive: String;
    FCompiler: String;
    FCopy: String;
    FDownload: String;
    FMkDir: String;
    FMove: String;
    FOptions: String;
    FCPU: TCPU;
    FOS: TOS;
    FMode : TCompilerMode;
    FPrefix: String;
    FBaseInstallDir,
    FUnitInstallDir,
    FBinInstallDir,
    FDocInstallDir,
    FExamplesInstallDir : String;
    FRemove: String;
    FTarget: String;
    FUnixPaths: Boolean;
    function GetBaseInstallDir: String;
    function GetBinInstallDir: String;
    function GetCompiler: String;
    function GetDocInstallDir: String;
    function GetExamplesInstallDir: String;
    function GetUnitInstallDir: String;
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
    Procedure Assign(ASource : TPersistent);override;
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
    // paths etc.
    Property Prefix : String Read FPrefix Write SetPrefix;
    Property BaseInstallDir : String Read GetBaseInstallDir Write SetBaseInstallDir;
    Property UnitInstallDir : String Read GetUnitInstallDir Write FUnitInstallDir;
    Property BinInstallDir : String Read GetBinInstallDir Write FBinInstallDir;
    Property DocInstallDir : String Read GetDocInstallDir Write FDocInstallDir;
    Property ExamplesInstallDir : String Read GetExamplesInstallDir Write FExamplesInstallDir;
    // Command tools. If not set, internal commands  will be used.
    Property Compiler : String Read GetCompiler Write FCompiler; // Compiler. Defaults to fpc/ppc386
    Property Copy : String Read FCopy Write FCopy;             // copy %FILES% to %DEST%
    Property Move : String Read FMove Write FMove;             // Move %FILES% to %DEST%
    Property Remove : String Read FRemove Write FRemove;       // Delete %FILES%
    Property MkDir : String Read FMkDir write FMkDir;          // Make %DIRECTORY%
    Property Archive : String Read FArchive Write FArchive;    // zip %ARCHIVE% %FILESORDIRS%
    Property Download : String Read FDownload Write FDownload; // wget %URL% %DESTFILE%
  end;

  { TBuildEngine }

  TBuildEngine = Class(TComponent)
  private
    // general variables
    FCompiler : String;
    FStartDir : String;
    FTargetDir : String;
    FDefaults : TDefaults;
    FForceCompile : Boolean;
    FListMode : Boolean;
    // Variables used when compiling a package.
    // Only valid during compilation of the package.
    FCurrentPackage: TPackage;
    // Events
    FOnLog: TLogEvent;
    FAfterArchive: TNotifyEvent;
    FAfterClean: TNotifyEvent;
    FAfterCompile: TNotifyEvent;
    FAfterDownload: TNotifyEvent;
    FAfterInstall: TNotifyEvent;
    FBeforeArchive: TNotifyEvent;
    FBeforeClean: TNotifyEvent;
    FBeforeCompile: TNotifyEvent;
    FBeforeDownload: TNotifyEvent;
    FBeforeInstall: TNotifyEvent;
    procedure SetDefaults(const AValue: TDefaults);
    procedure SetTargetDir(const AValue: String);
  Protected
    Procedure Error(Msg : String);
    Procedure Error(Fmt : String; Args : Array of const);
    // Internal copy/delete/move/archive/mkdir files
    Procedure SysCopyFile(Const Src,Dest : String); virtual;
    Procedure SysMoveFile(Const Src,Dest : String); virtual;
    Procedure SysDeleteFile(Const AFileName : String); virtual;
    Procedure SysArchiveFiles(List : TStrings; Const AFileName : String); virtual;
    Procedure Log(Level : TVerboseLevel; Const Msg : String);
    Procedure Log(Level : TVerboseLevel; Const Fmt : String; Args : Array Of Const);
    Procedure EnterDir(ADir : String);
    Function GetCompiler : String;
    Procedure InstallPackageFiles(APAckage : TPackage; tt : TTargetType; Const Src,Dest : String); virtual;

  Public
    Constructor Create(AOwner : TComponent); override;
    // Public Copy/delete/Move/Archive/Mkdir Commands.
    Procedure ExecuteCommand(Cmd : String; Args : String); virtual;
    Procedure CmdCopyFiles(List : TStrings; Const DestDir : String);
    Procedure CmdCreateDir(DestDir : String);
    Procedure CmdMoveFiles(List : TStrings; Const DestDir : String);
    Procedure CmdDeleteFiles(List : TStrings);
    Procedure CmdArchiveFiles(List : TStrings; Const ArchiveFile : String);
    // Target commands
    Function  GetTargetDir(APackage : TPackage; ATarget : TTarget; AbsolutePath : Boolean = False) : String;
    Function  GetCompilerCommand(APackage : TPackage; Target : TTarget) : String;
    Function  TargetOK(Target : TTarget) : Boolean;
    Procedure Compile(Target : TTarget);  virtual;
    Procedure FixDependencies(Target: TTarget);
    // Package commands
    Function  GetPackageDir(APackage : TPackage; AbsolutePath : Boolean = False) : String;
    Function  GetOutputDir(APackage : TPackage; AbsolutePath : Boolean = False) : String;
    Function  PackageOK(APackage : TPackage) : Boolean;
    Procedure Compile(APackage : TPackage); virtual;
    Procedure Install(APackage : TPackage); virtual;
    Procedure Archive(APackage : TPackage); virtual;
    Procedure Clean(APackage : TPackage); virtual;
    Procedure Download(APackage : TPackage); virtual;
    Procedure FixDependencies(APackage : TPackage);virtual;
    procedure CheckExternalPackage(Const APackageName : String); virtual;
    procedure CreateOutputDir(APackage: TPackage);
    // Packages commands
    Procedure Compile(Packages : TPackages);
    Procedure Install(Packages : TPackages);
    Procedure Archive(Packages : TPackages);
    Procedure Clean(Packages : TPackages);
    Procedure Download(Packages : TPackages);
    Property ListMode : Boolean Read FListMode Write FListMode;
    Property ForceCompile : Boolean Read FForceCompile Write FForceCompile;
    Property Defaults : TDefaults Read FDefaults Write SetDefaults;
    Property TargetDir : String Read FTargetDir Write SetTargetDir;
    // Events
    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FBeforeCompile;
    Property AfterCompile : TNotifyEvent Read FAfterCompile Write FAfterCompile;
    Property BeforeInstall : TNotifyEvent Read FBeforeInstall Write FBeforeInstall;
    Property AfterInstall : TNotifyEvent Read FAfterInstall Write FAfterInstall;
    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBeforeClean;
    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterClean;
    Property BeforeArchive : TNotifyEvent Read FBeforeArchive Write FBeforeArchive;
    Property AfterArchive : TNotifyEvent Read FAfterArchive Write FAfterArchive;
    Property BeforeDownload : TNotifyEvent Read FBeforeDownload Write FBeforeDownload;
    Property AfterDownload : TNotifyEvent Read FAfterDownload Write FAfterDownload;
    Property OnLog : TLogEvent Read FOnLog Write FOnlog;
  end;

  { TInstaller }

  TInstaller = Class(TComponent)
  private
    FBuildEngine: TBuildEngine;
    FDefaultPackage: TPackage;
    FDefaults: TDefaults;
    FPackages: TPackages;
    FRunMode: TRunMode;
    FListMode : Boolean;
    FLogLevels : TVerboseLevels;
    function GetAuthor: String;
    function GetStrings(AIndex : Integer): TStrings;
    function GetDirectory: String;
    function GetLicense: String;
    function GetOSes: TOSes;
    function GetOptions: String;
    function GetTargets: TTargets;
    function GetURL: String;
    function GetVersion: String;
    procedure SetAuthor(const AValue: String);
    procedure SetDefaultPackage(const AValue: TPackage);
    procedure SetDefaults(const AValue: TDefaults);
    procedure SetStrings(AIndex : Integer; const AValue: TStrings);
    procedure SetDirectory(const AValue: String);
    procedure SetOptions(const AValue: String);
    procedure SetOses(const AValue: TOSes);
    procedure SetURL(const AValue: String);
    procedure SetVersion(const AValue: String);
    procedure SetLicense(const AValue: String);
  Protected
    Procedure Log(Level : TVerboseLevel; Const Msg : String);
    Procedure CreatePackages; virtual;
    Procedure CheckPackages; virtual;
    Procedure CreateBuildEngine; virtual;
    Procedure CheckDefaultPackage;
    Procedure Error(Msg : String);
    Procedure Error(Fmt : String; Args : Array of const);
    Procedure AnalyzeOptions;
    Procedure Usage(FMT : String; Args : Array of const);
    Procedure Compile(Force : Boolean); virtual;
    Procedure Clean; virtual;
    Procedure Install; virtual;
    Procedure Archive; virtual;
    Procedure Download; Virtual;
    Property BuildEngine : TBuildEngine Read FBuildEngine;
  Public
    Constructor Create(AOWner : TComponent); override;
    Destructor destroy; override;
    Function StartPackage(Const AName : String) : TPackage;
    Procedure EndPackage;
    Function Run : Boolean;
    Function AddTarget(AName : String) : TTarget;
    Procedure AddDependency(AName : String);
    Property DefaultPackage : TPackage read FDefaultPackage write SetDefaultPackage;
    Property Packages : TPackages Read FPackages;
    Property Dependencies : TStrings Index 0 Read GetStrings Write SetStrings;
    Property InstallFiles : TStrings Index 1 Read GetStrings Write SetStrings;
    Property CleanFiles : TStrings Index 2 Read GetStrings Write SetStrings;
    Property ArchiveFiles : TStrings Index 3 Read GetStrings Write SetStrings;
    Property Defaults : TDefaults Read FDefaults Write SetDefaults;
    Property RunMode : TRunMode Read FRunMode;
    Property ListMode : Boolean Read FListMode;
    // Default Package redirects.
    Property Targets : TTargets Read GetTargets;
    Property Version : String Read GetVersion Write SetVersion;
    Property URL : String Read GetURL Write SetURL;
    Property Author : String Read GetAuthor Write SetAuthor;
    Property License : String Read GetLicense Write SetLicense;
    Property Directory : String Read GetDirectory Write SetDirectory;
    Property Options : String Read GetOptions Write SetOptions;
    Property OS: TOSes Read GetOSes Write SetOses;
  end;

  ECollectionError = Class(Exception);
  EInstallerError = Class(Exception);
  
Const
  // Aliases
  AmD64   = X86_64;
  PowerPC = PPC;
  dos = go32v2;

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


Var
  Installer : TInstaller;
  Defaults : TDefaults; // Set by installer.

Function OSToString(OS: TOS) : String;
Function OSesToString(OSes: TOSes) : String;
Function CPUToString(CPU: TCPU) : String;
Function CPUSToString(CPUS: TCPUS) : String;
Function StringToOS(S : String) : TOS;
Function OSesToString(S : String) : TOSes;
Function StringToCPU(S : String) : TCPU;
Function StringToCPUS(S : String) : TCPUS;
Function ModeToString(Mode: TCompilerMode) : String;
Function StringToMode(S : String) : TCompilerMode;
Function MakeTargetString(CPU : TCPU;OS: TOS) : String;
Procedure StringToCPUOS(S : String; Var CPU : TCPU; Var OS: TOS);
Procedure ResolveDependencies(L : TStrings; P : TNamedCollection);
Function AddStrings(Dest,Src : TStrings) : Integer ;
function AddStrings(Dest, Src : TStrings; Const APrefix : String) : Integer ;
Function FileListToString(List : TStrings; Prefix : String) : String;
Function FixPath (APath : String) : String;
Procedure ChangeDir(APath : String);

Implementation

uses TypInfo;

ResourceString
  SErrNameExists        = 'Name "%s" already exists in the collection.';
  SErrNoSuchName        = 'Could not find item with name "%s" in the collection.';
  SErrInvalidCPU        = 'Invalid CPU name : "%s"';
  SErrInvalidOS         = 'Invalid OS name : "%s"';
  SErrInvalidMode       = 'Invalid compiler mode : "%s"';
  SErrNoPackage         = 'No package available. Add package with StartPackage Call';
  SErrInValidArgument   = 'Invalid command-line argument at position %d : %s';
  SErrNeedArgument      = 'Option at position %d (%s) needs an argument';
  SErrInvalidTarget     = 'Invalid compiler target: %s';
  SErrNoPackagesDefined = 'No action possible: No packages were defined.';
  SErrInstaller         = 'The installer encountered the following error:';
  SErrDepUnknownTarget  = 'Unknown target in dependencies for %s: %s';
  SErrExternalCommandFailed = 'External command "%s" failed with exit code: %d';
  SErrCreatingDirectory = 'Failed to create directory: %s';
  SErrDeletingFile      = 'Failed to delete file: %s';
  SErrMovingFile        = 'Failed to move file "%s" to "%s"';
  SErrCopyingFile       = 'Failed to copy file "%s" to "%s"';
  SErrChangeDirFailed   = 'Failed to enter directory: %s';
  SWarnCircularDependency = 'Warning: Circular dependency detected when compiling target %s: %s';
  SWarnFailedToSetTime  = 'Warning: Failed to set timestamp on file : %s';
  SWarnFailedToGetTime  = 'Warning: Failed to get timestamp from file : %s';
  SWarnFileDoesNotExist = 'Warning: File "%s" does not exist';
  
  // Log messages
  SLogEnterDir           = 'Entering directory: %s';
  SLogCompilingPackage   = 'Compiling package : %s';
  SLogCompilingTarget    = 'Compiling target  : %s';
  SLogExecutingCommand   = 'Executing command %s with options: %s';
  SLogCreatingOutputDir  = 'Creating output dir : %s';
  SLogInstallingPackage  = 'Installing package : %s';
  SLogArchivingPackage   = 'Archiving package : %s';
  SLogCleaningPackage    = 'Cleaning package : %s';
  SLogDownloadingPackage = 'Downloading package : %s';
  SLogCopyingFile        = 'Copying file "%s" to "%s"';
  
Const
  // Keys for Defaults file. Do not localize.
  KeyCompiler = 'Compiler';
  KeyArchive  = 'Archive';
  KeyCopy     = 'Copy';
  KeyDownLoad = 'Download';
  KeyMkDir    = 'MkDir';
  KeyMove     = 'Move';
  KeyRemove   = 'Remove';
  KeyOptions  = 'Options';
  KeyCPU      = 'CPU';
  KeyOS       = 'OS';
  KeyMode     = 'Mode';
  KeyPrefix   = 'Prefix';
  KeyTarget   = 'Target';
  KeyBaseInstallDir     = 'BaseInstallDir';
  KeyUnitInstallDir     = 'UnitInstallDir';
  KeyBinInstallDir      = 'BinInstallDir';
  KeyDocInstallDir      = 'DocInstallDir';
  KeyExamplesInstallDir = 'ExamplesInstallDir';

// Callback for Sysutils getapplicationname.

Function GetFPMakeName : String;

begin
  Result:='fpmake';
end;


Function OSToString(OS: TOS) : String;

begin
  Result:=LowerCase(GetenumName(TypeInfo(TOS),Ord(OS)));
end;

Function OSesToString(OSes: TOSes) : String;

begin
  Result:=LowerCase(SetToString(TypeInfo(TOSes),Integer(OSes),False));
end;

Function CPUToString(CPU: TCPU) : String;

begin
  Result:=LowerCase(GetenumName(TypeInfo(TCPU),Ord(CPU)));
end;

Function CPUSToString(CPUS: TCPUS) : String;

begin
  Result:=LowerCase(SetToString(TypeInfo(TOSes),Integer(CPUS),False));
end;

Function StringToOS(S : String) : TOS;

Var
  I : Integer;

begin
  I:=GetEnumValue(TypeInfo(TOS),S);
  if (I=-1) then
    Raise EInstallerError.CreateFmt(SErrInvalidOS,[S]);
  Result:=TOS(I);
end;


Function OSesToString(S : String) : TOSes;

begin
  Result:=TOSes(StringToSet(TypeInfo(TOSes),S));
end;

Function StringToCPU(S : String) : TCPU;

Var
  I : Integer;

begin
  I:=GetEnumValue(TypeInfo(TCPU),S);
  if (I=-1) then
    Raise EInstallerError.CreateFmt(SErrInvalidCPU,[S]);
  Result:=TCPU(I);
end;

Function StringToCPUS(S : String) : TCPUS;

begin
  Result:=TCPUS(StringToSet(TypeInfo(TCPUS),S));
end;

Function ModeToString(Mode: TCompilerMode) : String;

begin
  Result:=LowerCase(GetenumName(TypeInfo(TCompilerMode),Ord(Mode)));
end;

Function StringToMode(S : String) : TCompilerMode;

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

Procedure StringToCPUOS(S : String; Var CPU : TCPU; Var OS: TOS);

Var
  P : integer;

begin
  P:=Pos('-',S);
  If (P=0) then
    Raise EInstallerError.CreateFmt(SErrInvalidTarget,[S]);
  CPU:=StringToCPU(Copy(S,1,P-1));
  OS:=StringToOs(Copy(S,P+1,Length(S)-P));
end;

Procedure ResolveDependencies(L : TStrings; P : TNamedCollection);

Var
  I,J : Integer;

begin
  If Assigned(L) then
    For I:=0 to L.Count-1 do
      begin
      J:=P.IndexOfName(L[i]);
      If J<>-1 then
        L.Objects[I]:=P.Items[J];
      end;
end;

Function AddStrings(Dest,Src : TStrings) : Integer ;

begin
  Result:=AddStrings(Dest,Src,'');
end;

Procedure AddStrings(Var S : String; L : TStrings; Prefix : String);

Var
  I : Integer;

begin
  For I:=0 to L.Count-1 do
    begin
    if (S<>'') then
      S:=S+' ';
    S:=S+Prefix+L[i];
    end;
end;

function AddStrings(Dest, Src : TStrings; Const APrefix : String) : Integer ;
begin
  Result:=0;
  While (Result<Src.Count) do
    begin
    If (APrefix<>'') then
      Dest.Add(APrefix+Src[Result]) // Not sure whether '' is optimized away.
    else
      Dest.Add(Src[Result]);
    Inc(Result);
    end;
end;

function FileListToString(List : TStrings; Prefix : String) : String;

Var
  I : integer;
  S : String;

begin
  Result:='';
  For I:=0 to List.Count-1 do
    begin
    If (I>0) then
      Result:=Result+' ';
    S:=Prefix+List[i];
    If (Pos(' ',S)<>0) then
      S:='"'+S+'"';
    Result:=Result+S;
    end;
end;

function FixPath (APath : String) : String;

Var
  P : PChar;
  
begin
  Result:=APath;
  If (result<>'') then
    begin
    P:=PChar(Result);
    While (P^<>#0) do
      begin
      If P^ in ['/','\',':'] then // do not use drive letters.
        P^:=PathDelim;
      Inc(P);
      end;
    end;
end;

procedure ChangeDir(APath : String);

begin
  if Not SetCurrentDir(APath) then
    Raise EInstallerError.CreateFmt(SErrChangeDirFailed,[APath]);
end;

{ TNamedItem }

procedure TNamedItem.SetName(const AValue: String);

begin
  if FName=AValue then exit;
  With TNamedCollection(Collection) do
    If UniqueNames then
      If (IndexOfName(AVAlue)<>-1) then
        Raise ECollectionError.CreateFmt(SErrNameExists,[AValue]);
  FName:=AValue;
end;

{ TNamedCollection }

function TNamedCollection.IndexOfName(AName: String): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(TNamedItem(Items[Result]).FName,AName)<>0) do
    Dec(Result);
end;

function TNamedCollection.ItemByName(AName: String): TNamedItem;

Var
  I : Integer;

begin
  I:=IndexOfName(AName);
  If (I=-1) Then
    Raise ECollectionError.CreateFmt(SErrNoSuchName,[AName]);
  Result:=TNamedItem(Items[i]);
end;

{ TFileItem }

function TFileItem.GetExtension: String;
begin
  Result:=ExtractFileExt(Name);
end;

function TFileItem.GetPath: String;
begin
  Result:=ExtractFilePath(Name);
end;

{ TTargets }

function TTargets.GetTargetItem(Index : Integer): TTarget;
begin
  Result:=TTarget(Items[Index]);
end;

function TTargets.GetTarget(AName : String): TTarget;
begin
  Result:=TTarget(ItemByName(AName));
end;

procedure TTargets.SetDefaultDir(const AValue: String);
begin
  If (AValue<>'') then
    FDefaultDir:=IncludeTrailingPathDelimiter(AValue)
  else
    FDefaultDir:='';
end;

procedure TTargets.SetTargetItem(Index : Integer; const AValue: TTarget);

begin
  Items[Index]:=AValue;
end;

procedure TTargets.ApplyDefaults(ATarget: TTarget);
begin
  If (ATarget.Directory='') then
    ATarget.Directory:=FDefaultDir;
   ATarget.OS:=FDefaultOS;
   ATarget.CPU:=FDefaultCPU;
end;

procedure TTargets.ResetDefaults;
begin
  FDefaultDir:='';
  FDefaultOS:=[];
  FDefaultCPU:=[];
end;

Function TTargets.AddUnit(AUnitName: String) : TTarget;


begin
  Result:=Add as TTarget;
  Result.Name:=AUnitName;
  Result.TargetType:=TTUnit;
  ApplyDefaults(Result);
end;

Function TTargets.AddProgram(AProgramName: String) : TTarget;
begin
  Result:=Add as TTarget;
  Result.Name:=AProgramName;
  Result.TargetType:=ttProgram;
  ApplyDefaults(Result);
end;

Function TTargets.AddExampleUnit(AUnitName: String): TTarget;
begin
  Result:=Add as TTarget;
  Result.Name:=AUnitName;
  Result.TargetType:=ttExampleUnit;
  ApplyDefaults(Result);
end;

Function TTargets.AddExampleProgram(AProgramName: String): TTarget;
begin
  Result:=Add as TTarget;
  Result.Name:=AProgramName;
  Result.TargetType:=ttExampleProgram;
  ApplyDefaults(Result);
end;


{ TNamedItemList }

function TNamedItemList.GetNamedItem(Index : Integer): TNamedItem;
begin
  Result:=TNamedItem(Items[Index]);
end;

procedure TNamedItemList.SetNamedItem(Index : Integer; const AValue: TNamedItem);
begin
  Items[Index]:=AValue;
end;

function TNamedItemList.IndexOfName(AName: String): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetNamedItem(Result).Name,AName)<>0) do
    Dec(Result);
end;

function TNamedItemList.ItemByName(ANAme: String): TNamedItem;

Var
  I : Integer;

begin
  I:=IndexOfName(AName);
  If (I=-1) Then
    Raise ECollectionError.CreateFmt(SErrNoSuchName,[AName]);
  Result:=TNamedItem(Items[i]);
end;

{ TDefaults }

procedure TDefaults.SetCPU(const AValue: TCPU);
begin
  FCPU:=AValue;
  RecalcTarget;
end;

function TDefaults.GetBaseInstallDir: String;
begin
  If (FBaseInstallDir<>'') then
    Result:=FBaseInstallDir
  else
    if UnixPaths then
      Result:=Prefix+PathDelim+'lib'+PathDelim+'fpc'
    else
      Result:=Prefix;

end;

function TDefaults.GetBinInstallDir: String;
begin
  If (FBinInstallDir<>'') then
    Result:=FBinInstallDir
  else
    If UnixPaths then
      Result:=BaseInstallDir+PathDelim+'bin'
    else
      Result:=BaseInstallDir+PathDelim+'bin';
end;

function TDefaults.GetCompiler: String;
begin
  If (FCompiler<>'') then
    Result:=FCompiler
  else
    Case CPU of
      i386    : Result:='ppc386';
      PowerPC : Result:='ppcppc';
      sparc   : Result:='ppcsparc';
      arm     : Result:='ppcarm';
      x86_64  : Result:='ppcx64';
    end;
end;

function TDefaults.GetDocInstallDir: String;
begin
  If (FBinInstallDir<>'') then
    Result:=FBinInstallDir
  else
    If UnixPaths then
      Result:=Prefix+PathDelim+'share'+PathDelim+'docs'
    else
      Result:=BaseInstallDir+PathDelim+'docs';
end;

function TDefaults.GetExamplesInstallDir: String;
begin
  If (FExamplesInstallDir<>'') then
    Result:=FExamplesInstallDir
  else
    If UnixPaths then
      Result:=Prefix+PathDelim+'share'+PathDelim+'docs'+PathDelim+'examples'
    else
      Result:=BaseInstallDir+PathDelim+'examples';
end;

function TDefaults.GetUnitInstallDir: String;
begin
  If (FUnitInstallDir<>'') then
    Result:=FBinInstallDir
  else
    If UnixPaths then
      Result:=BaseInstallDir+PathDelim+'units'+PathDelim+Target
    else
      Result:=BaseInstallDir+PathDelim+'units'+PathDelim+Target;
end;

procedure TDefaults.SetBaseInstallDir(const AValue: String);
begin
  FBaseInstallDir:=AValue;
  UnitInstallDir:='';
  BinInstallDir:='';
  ExamplesInstallDir:='';
end;

procedure TDefaults.SetOS(const AValue: TOS);
begin
  FOS:=AValue;
  Recalctarget;
end;

procedure TDefaults.SetPrefix(const AValue: String);
begin
  if FPrefix=AValue then exit;
  FPrefix:=AValue;
  BaseInstallDir:='';
end;

procedure TDefaults.SetTarget(const AValue: String);

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

procedure TDefaults.RecalcTarget;
begin
  Ftarget:=CPUToString(FCPU)+'-'+OStoString(FOS);
end;

constructor TDefaults.Create;
begin
  InitDefaults;
end;

procedure TDefaults.InitDefaults;
begin
  {$ifdef unix}
  UnixPaths:=True;
  {$else}
  UnixPaths:=False;
  {$endif}
  // Code to init defaults for compiled platform.
  CPU:=StringToCPU({$I %FPCTARGETCPU%});
  OS:=StringToOS({$I %FPCTARGETOS%});
  Compiler:='ppc386';
end;

procedure TDefaults.Assign(ASource: TPersistent);

Var
  d : TDefaults;

begin
  If ASource is TDefaults then
    begin
    D:=ASource as TDefaults;
    FArchive:=D.Farchive;
    FCompiler:=D.Compiler;
    FCopy:=D.FCopy;
    FCPU:=D.FCPU;
    FMode:=D.FMode;
    FDownload:=D.FDownload;
    FMkDir:=D.FDownload;
    FMove:=D.FDownload;
    FOptions:=D.FOptions;
    FOS:=D.FOS;
    FPrefix:=D.FPrefix;
    FBaseInstallDir:=D.FBaseInstallDir;
    FUnitInstallDir:=D.FUnitInstallDir;
    FBinInstallDir:=D.FBinInstallDir;
    FDocInstallDir:=D.FDocInstallDir;
    FExamplesInstallDir:=D.FExamplesInstallDir;
    FRemove:=D.FRemove;
    FTarget:=D.FTarget;
    FUnixPaths:=D.FUnixPaths;
    end;
end;

procedure TDefaults.LocalInit(Const AFileName : String);

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
    LoadFromFile(FN)
  // Code to find local config file and load it using LoadFromFile.
end;

procedure TDefaults.LoadFromFile(Const AFileName: String);

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

procedure TDefaults.SaveToFile(Const AFileName: String);

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

procedure TDefaults.SaveToStream(S : TStream);

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
      Values[KeyDownLoad]:=FDownload;
      Values[KeyMkDir]:=FMkDir;
      Values[KeyMove]:=FMove;
      Values[KeyOptions]:=FOptions;
      Values[KeyCPU]:=CPUToString(FCPU);
      Values[KeyOS]:=OSToString(FOS);
      Values[KeyMode]:=ModeToString(FMode);
      Values[KeyPrefix]:=FPrefix;
      Values[KeyBaseInstallDir]:=FBaseInstallDir;
      Values[KeyUnitInstallDir]:=FUnitInstallDir;
      Values[KeyBinInstallDir]:=FBinInstallDir;
      Values[KeyDocInstallDir]:=FDocInstallDir;
      Values[KeyExamplesInstallDir]:=FExamplesInstallDir;
      Values[KeyRemove]:=FRemove;
      Values[KeyTarget]:=FTarget;
      end;
    L.SaveToStream(S);
  Finally
    L.Free;
  end;
end;

procedure TDefaults.LoadFromStream(S: TStream);

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
      FDownload:=Values[KeyDownLoad];
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
      FPrefix:=Values[KeyPrefix];
      FBaseInstallDir:=Values[KeyBaseInstallDir];
      FUnitInstallDir:=Values[KeyUnitInstallDir];
      FBinInstallDir:=Values[KeyBinInstallDir];
      FDocInstallDir:=Values[KeyDocInstallDir];
      FExamplesInstallDir:=Values[KeyExamplesInstallDir];
      end;
  Finally
    L.Free;
  end;
end;

{ TPackage }

function TPackage.GetHasStrings(AIndex: integer): Boolean;
begin
  Result:=False;
  Case AIndex Of
    0 : Result:=FUnitPath<>Nil;
    1 : Result:=FObjectPath<>Nil;
    2 : Result:=FIncludePath<>Nil;
    3 : Result:=FDependencies<>Nil;
    4 : Result:=FInstallFiles<>Nil;
    5 : Result:=FCleanFiles<>Nil;
    6 : Result:=FArchiveFiles<>Nil;
  end;
end;

function TPackage.GetStrings(AIndex: integer): TStrings;

  Function EnsureStrings(Var S : TStrings) : TStrings;

  begin
    If (S=Nil) then
      S:=TStringList.Create;
    Result:=S;
  end;

begin
  Result:=Nil;
  Case AIndex Of
    0 : Result:=EnsureStrings(FUnitPath);
    1 : Result:=EnsureStrings(FObjectPath);
    2 : Result:=EnsureStrings(FIncludePath);
    3 : begin
        Result:=EnsureStrings(FDependencies);
        With TStringList(Result) do
          if (Count=0) then
            begin
            Sorted:=True;
            Duplicates:=dupError;
            end;
        end;
    4 : Result:=EnsureStrings(FInstallFiles);
    5 : Result:=EnsureStrings(FCleanFiles);
    6 : Result:=EnsureStrings(FArchiveFiles);
  end;


end;

procedure TPackage.SetStrings(AIndex: integer; const AValue: TStrings);
begin
  GetStrings(AIndex).Assign(AValue);
end;

constructor TPackage.Create(ACollection: TCollection);

Var
  L : TStringList;

begin
  inherited Create(ACollection);
  FTargets:=TTargets.Create(TTarget);
  L:=TStringList.Create;
  FDependencies:=L;
  FInstallFiles:=TStringList.Create;
  FCleanFiles:=TStringList.Create;
  FArchiveFiles:=TStringList.Create;
end;

destructor TPackage.destroy;
begin
  FreeAndNil(FDependencies);
  FreeAndNil(FInstallFiles);
  FreeAndNil(FCleanFiles);
  FreeAndNil(FArchiveFiles);
  FreeAndNil(FIncludePath);
  FreeAndNil(FObjectPath);
  FreeAndNil(FUnitPath);
  FreeAndNil(FTargets);
  inherited destroy;
end;

function TPackage.AddTarget(AName: String): TTarget;
begin
  Result:=Targets.Add as TTarget;
  Result.Name:=AName;
end;

procedure TPackage.AddDependency(AName: String);
begin
  If FDependencies.IndexOf(AName)=-1 then
    FDependencies.Add(AName);
end;

procedure TPackage.AddInstallFile(AFileName: String);
begin
  FInstallFiles.add(AFileName);
end;

procedure TPackage.GetCleanFiles(List: TStrings; Const APrefix : String; AOS : TOS);

Var
  I : Integer;

begin
  AddStrings(List,CleanFiles,APrefix);
  For I:=0 to FTargets.Count-1 do
    FTargets.TargetItems[I].GetCleanFiles(List,APrefix,AOS);
end;

procedure TPackage.GetInstallFiles(List: TStrings;Types : TTargetTypes;Const APrefix : String; AOS : TOS);

Var
  I : Integer;
  T : TTarget;

begin
  AddStrings(List,InstallFiles,APrefix);
  For I:=0 to FTargets.Count-1 do
    begin
    T:=FTargets.TargetItems[I];
    if (T.TargetType in Types)  then
      T.GetInstallFiles(List,APrefix,AOS);
    end;
end;


procedure TPackage.GetArchiveFiles(List: TStrings;Const APrefix : String; AOS : TOS);

Var
  I : Integer;

begin
  If (OS=[]) or (AOS in OS) then
    begin
    AddStrings(List,ArchiveFiles,APrefix);
    For I:=0 to FTargets.Count-1 do
      FTargets.TargetItems[I].GetArchiveFiles(List,APrefix,AOS);
    end;
end;

function TPackage.NeedsCompile(ATargetDir : String; AOS : TOS): Boolean;

Var
  I : Integer;
  P : TPackage;

begin
  ResolveDependencies(Dependencies,(Collection as TPackages));
  Result:=False;
  I:=0;
  While (Not Result) and (I<FDependencies.Count) do
    begin
    P:=TPackage(FDependencies.Objects[i]);
    // I'm not sure whether the target dir is OK here ??
    Result:=Assigned(P) and P.NeedsCompile(ATargetDir,AOS);
    Inc(I);
    end;
  If Not Result then
    begin
    I:=0;
    While (Not Result) and (I<FTargets.Count) do
      begin
      Result:=FTargets.TargetItems[i].NeedsCompile(ATargetDir,AOS);
      Inc(I);
      end;
    end;
end;


{ TPackages }

function TPackages.GetPackage(AName : String): TPackage;
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


{ TInstaller }

function TInstaller.GetStrings(AIndex : Integer): TStrings;
begin
  CheckDefaultPackage;
  Case AIndex of
    0:  Result:=DefaultPackage.Dependencies;
    1:  Result:=DefaultPackage.InstallFiles;
    2:  Result:=DefaultPackage.CleanFiles;
    3:  Result:=DefaultPackage.ArchiveFiles;
  end;
end;

function TInstaller.GetDirectory: String;
begin
  CheckDefaultPackage;
  Result:=DefaultPackage.Directory;
end;

function TInstaller.GetLicense: String;
begin
  CheckDefaultPackage;
  Result:=DefaultPackage.License;
end;

function TInstaller.GetOSes: TOSes;
begin
  CheckDefaultPackage;
  Result:=DefaultPackage.OS;
end;

function TInstaller.GetOptions: String;
begin
  CheckDefaultPackage;
  Result:=DefaultPackage.Options;
end;

function TInstaller.GetAuthor: String;
begin
  CheckDefaultPackage;
  Result:=DefaultPackage.Author;
end;

function TInstaller.GetTargets: TTargets;
begin
  CheckDefaultPackage;
  Result:=DefaultPackage.Targets;
end;

function TInstaller.GetURL: String;
begin
  CheckDefaultPackage;
  Result:=DefaultPackage.URL;
end;

function TInstaller.GetVersion: String;
begin
  CheckDefaultPackage;
  Result:=DefaultPackage.Version;
end;

procedure TInstaller.SetAuthor(const AValue: String);
begin
  CheckDefaultPackage;
  DefaultPackage.Author:=AValue;
end;

procedure TInstaller.SetDefaultPackage(const AValue: TPackage);
begin
  if FDefaultPackage=AValue then exit;
  FDefaultPackage:=AValue;
end;

procedure TInstaller.SetDefaults(const AValue: TDefaults);
begin
  FDefaults.Assign(AValue);
end;

procedure TInstaller.SetStrings(AIndex : Integer; const AValue: TStrings);

Var
  Res : TStrings;

begin
  CheckDefaultPackage;
  Case AIndex of
    0:  Res:=DefaultPackage.Dependencies;
    1:  Res:=DefaultPackage.InstallFiles;
    2:  Res:=DefaultPackage.CleanFiles;
    3:  Res:=DefaultPackage.ArchiveFiles;
  end;
  Res.Assign(Avalue);
end;

procedure TInstaller.SetDirectory(const AValue: String);
begin
  CheckDefaultPackage;
  DefaultPackage.Directory:=AValue;
end;

procedure TInstaller.SetOptions(const AValue: String);
begin
  CheckDefaultPackage;
  DefaultPackage.Options:=AValue;
end;

procedure TInstaller.SetOses(const AValue: TOSes);
begin
  CheckDefaultPackage;
  DefaultPackage.OS:=AValue;
end;

procedure TInstaller.SetURL(const AValue: String);
begin
  CheckDefaultPackage;
  DefaultPackage.URL:=AValue;
end;

procedure TInstaller.SetVersion(const AValue: String);
begin
  CheckDefaultPackage;
  DefaultPackage.Version:=AValue;
end;

procedure TInstaller.SetLicense(const AValue: String);
begin
  CheckDefaultPackage;
  DefaultPackage.License:=AValue;
end;

procedure TInstaller.Log(Level: TVerboseLevel; const Msg: String);
begin
  If Level in FLogLevels then
    Writeln(StdErr,Msg);
end;

procedure TInstaller.CreatePackages;
begin
  FPAckages:=TPackages.Create(TPackage);
end;

procedure TInstaller.CreateBuildEngine;
begin
  FBuildEngine:=TBuildEngine.Create(Self);
  FBuildEngine.Defaults:=Defaults;
  FBuildEngine.ListMode:=FListMode;
  FBuildEngine.OnLog:=@Self.Log;
end;

procedure TInstaller.CheckDefaultPackage;
begin
  If (FDefaultPackage=Nil) then
    Raise EInstallerError.Create(SErrNoPackage);
end;

procedure TInstaller.Error(Msg: String);
begin
  Raise EInstallerError.Create(Msg);
end;

procedure TInstaller.Error(Fmt: String; Args: array of const);
begin
  Raise EInstallerError.CreateFmt(Fmt,Args);
end;

Function TInstaller.StartPackage(const AName: String) : TPackage;
begin
  FDefaultPackage:=FPackages.AddPackage(AName);
  Result:=FDefaultPackage;
end;

procedure TInstaller.EndPackage;
begin
  FDefaultPackage:=Nil;
end;

procedure TInstaller.AnalyzeOptions;

  Function CheckOption(Index : Integer;Short,Long : String): Boolean;

  var
    O : String;

  begin
    O:=Paramstr(Index);
    Result:=(O='-'+short) or (O='--'+long) or (copy(O,1,Length(Long)+3)=('--'+long+'='));
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
  Nodefaults : Boolean;
  DefaultsFileName : string;

begin
  I:=0;
  NoDefaults:=False;
  FListMode:=False;
  While (I<ParamCount) do
    begin
    Inc(I);
    if Checkoption(I,'m','compile') then
      FRunMode:=rmCompile
    else if Checkoption(I,'b','build') then
      FRunMode:=rmBuild
    else if CheckOption(I,'i','install') then
      FRunMode:=rmInstall
    else if CheckOption(I,'c','clean') then
      FRunMode:=rmClean
    else if CheckOption(I,'a','archive') then
      FRunMode:=rmarchive
    else if CheckOption(I,'d','download') then
      FRunMode:=rmDownload
    else if CheckOption(I,'h','help') then
      FRunMode:=rmhelp
    else if Checkoption(I,'C','CPU') then
      Defaults.CPU:=StringToCPU(OptionArg(I))
    else if Checkoption(I,'O','OS') then
      Defaults.OS:=StringToOS(OptionArg(I))
    else if Checkoption(I,'t','target') then
      Defaults.Target:=OptionArg(I)
    else if CheckOption(I,'l','list-commands') then
      FListMode:=True
    else if Checkoption(I,'P','prefix') then
      Defaults.Prefix:=OptionArg(I)
    else if Checkoption(I,'n','nodefaults') then
      NoDefaults:=true
    else if CheckOption(I,'b','baseinstalldir') then
      Defaults.BaseInstallDir:=OptionArg(I)
    else if CheckOption(I,'r','compiler') then
      Defaults.Compiler:=OptionArg(I)
    else if CheckOption(I,'f','config') then
      DefaultsFileName:=OptionArg(I)
    else if CheckOption(I,'v','verbose') then
      begin
      Try
        FLogLevels:=TVerboseLevels(StringToSet(TypeInfo(TVerboseLevels),OptionArg(I)));
      except
        FLogLevels:=AllMessages;
      end;
      end
    else
      begin
      Usage(SErrInValidArgument,[I,ParamStr(I)]);
      end;
    end;
  If Not NoDefaults then
    Defaults.LocalInit(DefaultsFileName);
{$ifdef debug}
  FLogLevels:=AllMessages;
{$endif}
end;

procedure TInstaller.Usage(FMT: String; Args: array of const);
begin
  If (FMT<>'') then
    Writeln(stderr,Format(Fmt,Args));
  Writeln(stderr,ExtractFileName(Paramstr(0)),' usage: ');
  Writeln(stderr,'');
  If (FMT<>'') then
    halt(1);
end;

procedure TInstaller.Compile(Force: Boolean);
begin
  FBuildEngine.ForceCompile:=Force;
  FBuildEngine.Compile(FPackages);
end;

procedure TInstaller.Clean;
begin
  BuildEngine.Clean(FPackages);
end;

procedure TInstaller.Install;
begin
  BuildEngine.Install(FPackages);
end;

procedure TInstaller.Archive;
begin
  FBuildEngine.Archive(FPackages);
end;

procedure TInstaller.Download;
begin
  BuildEngine.Download(FPackages);
end;

constructor TInstaller.Create(AOWner: TComponent);
begin
  inherited Create(AOWner);
  FDefaults:=TDefaults.Create;
  AnalyzeOptions;
  CreatePackages;
end;

destructor TInstaller.destroy;
begin
  inherited destroy;
end;

procedure TInstaller.CheckPackages;

begin
  If (FPackages.Count=0) then
    Error(SErrNoPackagesDefined);
  // Check for other obvious errors ?
end;

Function TInstaller.Run : Boolean;

begin
  Result:=True;
  try
    If RunMode<>rmHelp then
      begin
      CheckPackages;
      CreateBuildEngine;
      end;
    Case RunMode of
      rmHelp    : Usage('',[]);
      rmCompile : Compile(False);
      rmBuild   : Compile(True);
      rmInstall : Install;
      rmArchive : Archive;
      rmClean    : Clean;
      rmDownload : Download;
    end;
  except
    On E : Exception do
      begin
      Writeln(StdErr,SErrInstaller);
      Writeln(StdErr,E.Message);
      Result:=False;
      end;
  end;
end;

function TInstaller.AddTarget(AName: String): TTarget;
begin
  CheckDefaultPackage;
  Result:=DefaultPackage.AddTarget(AName);
end;

procedure TInstaller.AddDependency(AName: String);
begin
  CheckDefaultPackage;
  DefaultPackage.AddDependency(AName);
end;

{ TBuildEngine }

procedure TBuildEngine.SetDefaults(const AValue: TDefaults);
begin
  FDefaults.Assign(AValue);
end;

procedure TBuildEngine.SetTargetDir(const AValue: String);
begin
  if FTargetDir=AValue then exit;
  FTargetDir:=AValue;
end;

procedure TBuildEngine.Error(Msg: String);
begin
  Raise EInstallerError.Create(Msg);
end;

procedure TBuildEngine.Error(Fmt: String; Args: array of const);
begin
  Raise EInstallerError.CreateFmt(Fmt,Args);
end;

constructor TBuildEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaults:=TDefaults.Create;
  // Maybe this should be the current directory ?
  // Or have it as a command-line option.
  // Would allow to put all 'installers' in one dir and call them
  // With --start-dir=/path/to/sources.
  FStartDir:=includeTrailingPathDelimiter(GetCurrentDir);
end;

procedure TBuildEngine.ExecuteCommand(Cmd: String; Args : String);

Var
  E : Integer;

begin
  Log(vlCommand,SLogExecutingCommand,[Cmd,Args]);
  // We should check cmd for spaces, and move all after first space to args.
  E:=ExecuteProcess(cmd,args);
  If (E<>0) then
    Error(SErrExternalCommandFailed,[Cmd,E]);
end;

procedure TBuildEngine.SysCopyFile(Const Src,Dest : String);

Var
  D,S : String;
  Fin,FOut : TFileStream;
  Count : Int64;
  A : Integer;
  
begin
  Log(vlCommand,SLogCopyingFile,[Src,Dest]);
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
  // To be implemented
end;

procedure TBuildEngine.Log(Level: TVerboseLevel; const Msg: String);
begin
  If Assigned(FOnLog) then
    FOnLog(Level,Msg);
end;

procedure TBuildEngine.Log(Level: TVerboseLevel; const Fmt: String;
  Args: array of const);
begin
  Log(Level,Format(Fmt,Args));
end;

procedure TBuildEngine.EnterDir(ADir: String);

Var
  D : String;

begin
  D:=FStartDir;
  D:=D+ADir;
  Log(vlInfo,SLogEnterDir,[D]);
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

procedure TBuildEngine.CmdCreateDir(DestDir: String);

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
  Args : String;

begin
  If (Defaults.Archive<>'') then
    begin
    Args:=FileListToString(List,'');
    Args:=ArchiveFile+' '+Args;
    ExecuteCommand(Defaults.Archive,Args);
    end
  else
    SysArchiveFiles(List,ArchiveFile);
end;

// Relative to startdir.
Function TBuildEngine.GetTargetDir(APackage : TPackage; ATarget : TTarget; AbsolutePath : Boolean = False) : String;

begin
  If AbsolutePath then
    Result:=IncludeTrailingPathDelimiter(FStartDir)
  else
    Result:='';
  If (APackage.Directory<>'') then
    Result:=Result+IncludeTrailingPathDelimiter(APackage.Directory);
  If (ATarget.Directory<>'') then
    Result:=IncludeTrailingPathDelimiter(Result+ATarget.Directory);

end;

Function TBuildEngine.TargetOK(Target : TTarget) : Boolean;

begin
  Result:=(Target.TargetType in [ttUnit,ttProgram])
          and
          ((Target.CPU=[]) or (Defaults.CPU in Target.CPU))
          and
          ((Target.OS=[]) or (Defaults.OS in Target.OS));
end;



Function TBuildEngine.GetCompilerCommand(APackage : TPackage; Target : TTarget) : String;


Var
  PD,TD,OD,RD : String;

begin
  PD:=IncludeTrailingPathDelimiter(GetPackageDir(APackage,True));
  OD:=IncludeTrailingPathDelimiter(GetOutputDir(APackage,True));
  RD:=ExtractRelativePath(PD,OD);
  If Target.TargetType in ProgramTargets then
    Result:='-FE.' // Make this relative to target directory.
  else
    Result:='-FU'+RD;
  If Target.Mode<>fpc then
    Result:=Result+' -M'+ModeToString(Target.Mode)
  else If Defaults.Mode<>fpc then
    Result:=Result+' -M'+ModeToString(Defaults.Mode);
  If (Defaults.Options<>'') then
    Result:=Result+' '+Defaults.Options;
  If (APackage.Options<>'') then
    Result:=Result+' '+APackage.Options;
  If APackage.HasUnitPath then
    AddStrings(Result,APackage.UnitPath,'-Fu');
  If APackage.HasIncludePath then
    AddStrings(Result,APackage.IncludePath,'-Fi');
  If APackage.HasObjectPath then
    AddStrings(Result,APackage.ObjectPath,'-Fo');
  If Target.HasUnitPath then
    AddStrings(Result,Target.UnitPath,'-Fu');
  If Target.HasIncludePath then
    AddStrings(Result,Target.IncludePath,'-Fi');
  If Target.HasObjectPath then
    AddStrings(Result,Target.ObjectPath,'-Fo');
  If (Target.Options<>'') then
    Result:=Result+' '+Target.Options;
  TD:=Target.Directory;
  if (TD<>'') then
    TD:=IncludeTrailingPathDelimiter(TD);
  Result:=Result+' '+TD+Target.SourceFileName;
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

procedure TBuildEngine.Compile(Target: TTarget);

Var
  S : String;
  
begin
  Log(vlInfo,SLogCompilingTarget,[Target.Name]);
  If Assigned(Target.BeforeCompile) then
    Target.BeforeCompile(Target);
  S:=GetCompilerCommand(FCurrentPackage,Target);
  ExecuteCommand(GetCompiler,S);
  If Assigned(Target.AfterCompile) then
    Target.AfterCompile(Target);
end;

procedure TBuildEngine.FixDependencies(Target: TTarget);

Var
  I : Integer;
  T : TTarget;

begin
  If Target.HasDependencies then
    For I:=0 to Target.Dependencies.Count-1 do
      begin
      T:=TTarget(Target.Dependencies.Objects[i]);
      If Assigned(T) then
        begin
        If (T.State=tsCompiling) then
          Log(vlWarning,SWarnCircularDependency,[Target.Name,T.Name])
        else
          Compile(T) // If it already was compiled, then State<>tsNeutral, and it won't be compiled again.
        end
      else
        Error(SErrDepUnknownTarget,[Target.Name,Target.Dependencies[i]]);
      end;
end;

function TBuildEngine.GetPackageDir(APackage: TPackage; AbsolutePath: Boolean
  ): String;
begin
  If AbsolutePath then
    Result:= IncludeTrailingPathDelimiter(FStartDir)
  else
    Result:='';
  Result:=Result+APackage.Directory;
  If (Result<>'') then
    Result:= ExcludeTrailingPathDelimiter(Result)
end;


Function TBuildEngine.GetOutputDir(APackage : TPackage; AbsolutePath : Boolean = False) : String;

begin
  If (TargetDir<>'') then
    Result:=TargetDir
  else
    begin
    If AbsolutePath then
      Result:=IncludeTrailingPathDelimiter(FStartDir)
    else
      Result:='';
    If (APackage.Directory<>'') then
      Result:=IncludeTrailingPathDelimiter(Result+APackage.Directory);
    Result:=Result+'units'+PathDelim+Defaults.Target;
    end;
end;

procedure TBuildEngine.CreateOutputDir(APackage: TPackage);

Var
  D : String;

begin
  D:=GetOutputDir(APackage,True);
  Log(vlInfo,SLogCreatingOutputDir,[D]);
  CmdCreateDir(D);
end;

Function TBuildEngine.PackageOK(APackage : TPackage) : Boolean;

begin
  Result:=((APackage.CPU=[]) or (Defaults.CPU in APackage.CPU))
          and
          ((APAckage.OS=[]) or (Defaults.OS in APackage.OS));
end;

procedure TBuildEngine.Compile(APackage: TPackage);


Var
  T : TTarget;
  I : Integer;
  
begin
  Log(vlInfo,SLogCompilingPackage,[APackage.Name]);
  FCurrentPackage:=APackage;
  Try
    If Assigned(APackage.BeforeCompile) then
      APackage.BeforeCompile(APackage);
    If (APackage.Directory<>'') then
      EnterDir(APackage.Directory);
    Try
      CreateOutputDir(APackage);
      For I:=0 to APackage.Targets.Count-1 do
        begin
        T:=APackage.Targets.TargetItems[i];
        If TargetOK(T) then
          If (T.State=tsNeutral) then
            begin
            If (FForceCompile or T.NeedsCompile(GetOutputDir(APackage,true),Defaults.OS)) then
              begin
              T.FTargetState:=tsCompiling;
              FixDependencies(T);
              Compile(T);
              end;
            T.FTargetState:=tsCompiled;
            end;
        end;
      If Assigned(APackage.AfterCompile) then
        APackage.AfterCompile(APackage);
    Finally
      If (APackage.Directory<>'') then
        EnterDir('');
    end;
  Finally
    FCurrentPackage:=Nil;
  end;
end;

procedure TBuildEngine.CheckExternalPackage(Const APackageName : String);

begin
  // A check needs to be implemented here.
  Log(vldebug,'Unresolved external dependency : %s',[APackageName]);
end;

procedure TBuildEngine.FixDependencies(APackage: TPackage);

Var
  I : Integer;
  P : TPackage;

begin
  if APackage.HasDependencies then
    For I:=0 to APAckage.Dependencies.Count-1 do
      begin
      P:=TPackage(Apackage.Dependencies.Objects[i]);
      If Assigned(P) then
        Compile(P) // If it already was compiled, then State<>tsNeutral, and it won't be compiled again.
      else
        CheckExternalPackage(Apackage.Dependencies[i]);
      end;
end;

Procedure TBuildEngine.InstallPackageFiles(APAckage : TPackage; tt : TTargetType; Const Src,Dest : String);

Var
  I : Integer;
  List : TStringList;

begin
  List:=TStringList.Create;
  Try
    APackage.GetInstallFiles(List,[tt],Src,Defaults.OS);
    if (List.Count>0) then
      CmdCopyFiles(List,Dest);
  Finally
    List.Free;
  end;
end;

procedure TBuildEngine.Install(APackage: TPackage);


Var
  PD,D,O : String;

begin
  If (Apackage.State<>tsCompiled) then
    Compile(APackage);
  Log(vlInfo,SLogInstallingPackage,[APackage.Name]);
  If Assigned(APackage.BeforeInstall) then
    APackage.BeforeInstall(APackage);
  O:=IncludeTrailingPathDelimiter(GetOutputDir(APAckage));
  PD:=IncludeTrailingPathDelimiter(GetPackageDir(APackage));
  // units
  D:=IncludeTrailingPathDelimiter(Defaults.UnitInstallDir)+APackage.Name;
  InstallPackageFiles(APAckage,ttUnit,O,D);
  // Programs
  D:=IncludeTrailingPathDelimiter(Defaults.BinInstallDir);
  InstallPackageFiles(APAckage,ttProgram,PD,D);
  // Done.
  APackage.FTargetState:=tsInstalled;
  If Assigned(APackage.AfterInstall) then
    APackage.AfterInstall(APackage);
end;

procedure TBuildEngine.Archive(APackage: TPackage);
begin
  Log(vlInfo,SLogArchivingPackage,[APackage.Name]);
  If Assigned(APackage.BeforeArchive) then
    APackage.BeforeArchive(APackage);
  Writeln('Archiving : ',APackage.Name);
  If Assigned(APackage.AfterArchive) then
    APackage.AfterArchive(APackage);
end;

procedure TBuildEngine.Clean(APackage: TPackage);

Var
  O : String;
  List : TStringList;
  
begin
  Log(vlInfo,SLogCleaningPackage,[APackage.Name]);
  If Assigned(APackage.BeforeClean) then
    APackage.BeforeClean(APackage);
  O:=IncludeTrailingPathDelimiter(GetOutputDir(APAckage));
  List:=TStringList.Create;
  try
    APackage.GetCleanFiles(List,O,Defaults.OS);
    if (List.Count>0) then
      CmdDeleteFiles(List);
  Finally
    List.Free;
  end;
  If Assigned(APackage.AfterClean) then
    APackage.AfterClean(APackage);
end;

procedure TBuildEngine.Download(APackage: TPackage);
begin
  Log(vlInfo,SLogDownloadingPackage,[APackage.Name]);
  If Assigned(APackage.BeforeDownload) then
    APackage.BeforeDownload(APackage);
  Writeln('Downloading : ',APackage.Name);
  If Assigned(APackage.AfterDownload) then
    APackage.AfterDownload(APackage);
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
      If (P.State=tsNeutral) then
        begin
        If (FForceCompile or P.NeedsCompile(TargetDir,Defaults.OS)) then
          begin
          P.FTargetState:=tsCompiling;
          FixDependencies(P);
          Compile(P);
          end;
        P.FTargetState:=tsCompiled;
        end;
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
  Log(vlDebug,'Build engine archiving.');
  For I:=0 to Packages.Count-1 do
    begin
    P:=Packages.PackageItems[i];
    If PackageOK(P) then
      Archive(P);
    end;
  If Assigned(AfterArchive) then
    AfterArchive(Self);
end;

procedure TBuildEngine.Clean(Packages: TPackages);

Var
  I : Integer;
  P : TPackage;

begin
  If Assigned(BeforeClean) then
    BeforeClean(Self);
  Log(vldebug,'Build engine cleaning.');
  For I:=0 to Packages.Count-1 do
    begin
    P:=Packages.PackageItems[i];
    If PackageOK(P) then
      Clean(P);
    end;
  If Assigned(AfterClean) then
    AfterClean(Self);
end;

procedure TBuildEngine.Download(Packages: TPackages);

Var
  I : Integer;
  P : TPackage;

begin
  If Assigned(BeforeDownload) then
    BeforeDownload(Self);
  For I:=0 to Packages.Count-1 do
    begin
    P:=Packages.PackageItems[i];
    If PackageOK(P) then
      Download(P);
    end;
  If Assigned(AfterDownload) then
    AfterDownload(Self);
end;

{ TTarget }

function TTarget.GetHasStrings(AIndex: integer): Boolean;
begin
  Result:=False;
  Case AIndex Of
    0 : Result:=FUnitPath<>Nil;
    1 : Result:=FObjectPath<>Nil;
    2 : Result:=FIncludePath<>Nil;
    3 : Result:=FDependencies<>Nil;
  end;
end;

function TTarget.GetStrings(AIndex: integer): TStrings;

  Function EnsureStrings(Var S : TStrings) : TStrings;

  begin
    If (S=Nil) then
      S:=TStringList.Create;
    Result:=S;
  end;
  
begin
  Result:=Nil;
  Case AIndex Of
    0 : Result:=EnsureStrings(FUnitPath);
    1 : Result:=EnsureStrings(FObjectPath);
    2 : Result:=EnsureStrings(FIncludePath);
    3 : Result:=EnsureStrings(FDependencies);
  end;
end;

procedure TTarget.SetStrings(AIndex: integer; const AValue: TStrings);
begin
  GetStrings(AIndex).Assign(AValue);
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

function TTarget.GetProgramFileName(AnOS : TOS): String;
begin
  if AnOS in [dos,win32,os2] then
    Result:=Name+ExeExt
  else
    Result:=Name;
end;

constructor TTarget.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FInstall:=True;
end;

destructor TTarget.Destroy;
begin
  FreeAndNil(FUnitPath);
  FreeAndNil(FObjectPath);
  FreeAndNil(FIncludePath);
  FreeAndNil(FDependencies);
  inherited Destroy;
end;

function TTarget.GetOutputFileName(AOs: TOS): String;
begin
  Result:=Name;
  if TargetType in UnitTargets then
    Result:=Result+UnitExt
  else if AOs in [Win32,dos,OS2] then
    Result:=Result+ExeExt
end;

function TTarget.NeedsCompile(ATargetDir : String;AOS : TOS): Boolean;

Var
  I : Integer;
  T : TTarget;
  OD,SD,SFN,OFN : String;

begin
  Result:=False;
  OD:=ATargetDir;
  If (OD<>'') then
    OD:=IncludeTrailingPathDelimiter(ATargetDir);
  OFN:=OD+GetOutPutFileName(AOS);
  SD:=Directory;
  If (SD<>'') then
      SD:=IncludeTrailingPathDelimiter(SD);
  Result:=Not FileExists(OFN);
  // Check dependencies
  If not Result then
    If HasDependencies then
      begin
      ResolveDependencies(FDependencies,Collection as TTargets);
      I:=0;
      While (Not Result) and (I<FDependencies.Count) do
        begin
        T:=TTarget(FDependencies.Objects[i]);
        If (T<>Nil) then
          Result:=T.NeedsCompile(ATargetDir,AOS)
        else // if it is a filename, check dates.
          if FileExists(FDependencies[i]) then
            Result:=(FileAge(OFN)<FileAge(FDependencies[i]))
          else if FileExists(SD+FDependencies[i]) then
            Result:=(FileAge(OFN)<FileAge(SD+FDependencies[i]));
        Inc(I)
        end;
      end;
  If not Result then
    begin
    SFN:=SD+SourceFileName;
    If (ExtractFileExt(SFN)='') then
      if FileExists(SFN+'.pp') then
        SFN:=SFN+'.pp'
      else
        SFN:=SFN+'.pas';
    // Writeln('Checking : ',OFN,' against ',SFN);
    Result:=(FileAge(OFN)<FileAge(SFN));
    // here we should check file timestamps.
    end;
end;

procedure TTarget.SetName(const AValue: String);

Var
  D,N,E : String;

begin
  N:=FixPath(AValue);
  D:=ExtractFilePath(N);
  E:=ExtractFileExt(N);
  N:=ExtractFileName(N);
  If (E<>'') then
    N:=Copy(N,1,Length(N)-Length(E));
  inherited SetName(N);
  FExtension:=E;
  FDirectory:=D;
end;

procedure TTarget.GetCleanFiles(List: TStrings; APrefix : String; AnOS : TOS);
begin
  If (OS=[]) or (AnOS in OS) then
    begin
    List.Add(APrefix+ObjectFileName);
    If (TargetType in [ttUnit,ttExampleUnit]) then
      List.Add(APrefix+UnitFileName)
    else If (TargetType in [ttProgram,ttExampleProgram]) then
      List.Add(APrefix+GetProgramFileName(AnOS));
    If ResourceStrings then
      List.Add(APrefix+RSTFileName);
    // Maybe add later ?  AddStrings(List,CleanFiles);
    end;
end;

procedure TTarget.GetInstallFiles(List: TStrings; APrefix : String; AnOS : TOS);
begin
  If (OS=[]) or (AnOS in OS) then
    begin
    If Not (TargetType in [ttProgram,ttExampleProgram]) then
      List.Add(APrefix+ObjectFileName);
    If (TargetType in [ttUnit,ttExampleUnit]) then
      List.Add(APrefix+UnitFileName)
    else If (TargetType in [ttProgram,ttExampleProgram]) then
      List.Add(APrefix+GetProgramFileName(AnOS));
    If ResourceStrings then
      List.Add(APrefix+RSTFileName);
    // Maybe add later ?  AddStrings(List,InstallFiles);
    end;
end;

procedure TTarget.GetArchiveFiles(List: TStrings; APrefix : String; AnOS : TOS);
begin
  If (OS=[]) or (AnOS in OS) then
    begin
    List.Add(APrefix+SourceFileName);
    // Maybe add later ?  AddStrings(List,ArchiveFiles);
    end;
end;



Initialization
  OnGetApplicationName:=@GetFPMakeName;
  Installer:=TInstaller.Create(Nil);
  Defaults:=Installer.Defaults;

Finalization
  FreeAndNil(Installer);
  FreeAndNil(Defaults);
end.
