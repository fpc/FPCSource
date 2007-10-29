{$ifdef Delphi}
const fpmkunitsrc : array[0..460] of string[240]=(
{$else Delphi}
const fpmkunitsrc : array[0..460,1..240] of char=(
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
  #010+
  'Interface'#010+
  #010+
  '{$ifndef NO_UNIT_PROCESS}'#010+
  '  {$define HAS_UNIT_PROCESS}'#010+
  '{$endif NO_UNIT_PROCESS}'#010+
  #010+
  '{$ifndef NO_UNIT_ZIPPER}'#010+
  '  {$define HAS_UNIT_ZIPPER}'#010+
  '{$endif NO_','UNIT_ZIPPER}'#010+
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
  '  TFileType = (ftSource,ftUnit,ftObject,ftResource,ftExecu','table,ftSt'+
  'aticLibrary,'#010+
  '               ftSharedLibrary);'#010+
  '  TFileTypes = set of TFileType;'#010+
  #010+
  '  TOS = (osNone,Amiga,Atari,Darwin,FreeBSD,Go32v2,Linux,MacOS,MorphOS,'+
  'NetBSD,'#010+
  '         Netware,NetwLibc,OpenBSD,OS2,PalmOS,Solaris,Win32,Win64,WinCE'+
  ',E','mx);'#010+
  '  TOSes = Set of TOS;'#010+
  #010+
  '  TCPU = (cpuNone,Arm,I386,PPC,SPARC,X86_64,M68K,PPC64);'#010+
  '  TCPUS = Set of TCPU;'#010+
  #010+
  '  TCompilerMode = (cmFPC,cmTP,cmObjFPC,cmDelphi,cmMacPas);'#010+
  '  TCompilerModes = Set of TCompilerMode;'#010+
  #010+
  '  TTargetType = (ttUnit,ttProg','ram,ttExampleUnit,ttExampleProgram);'#010+
  '  TTargetTypes = set of TTargetType;'#010+
  #010+
  '  TTargetState = (tsNeutral,tsCompiling,tsCompiled,tsInstalled);'#010+
  '  TTargetStates = Set of TTargetState;'#010+
  #010+
  '  TSourceType = (stDoc,stSrc,stExample,stTest);'#010+
  '  TSourceTyp','es = set of TSourceType;'#010+
  #010+
  '  TVerboseLevel = (vlError,vlWarning,vlInfo,vlCompare,vlCommand,vldebu'+
  'g);'#010+
  '  TVerboseLevels = Set of TVerboseLevel;'#010+
  #010+
  '  TCommandAt = (caBeforeCompile,caAfterCompile,'#010+
  '                caBeforeInstall,caAfterInstall,'#010+
  '  ','              caBeforeArchive,caAfterArchive,'#010+
  '                caBeforeClean,caAfterClean,'#010+
  '                caBeforeDownload,caAfterDownload);'#010+
  #010+
  '  TLogEvent = Procedure (Level : TVerboseLevel; Const Msg : String) of'+
  ' Object;'#010+
  #010+
  '  TRunMode = (rmCo','mpile,rmBuild,rmInstall,rmArchive,rmClean,rmManife'+
  'st,rmListSources);'#010+
  #010+
  'Const'#010+
  '  // Aliases'#010+
  '  Amd64   = X86_64;'#010+
  '  PowerPC = PPC;'#010+
  '  PowerPC64 = PPC64;'#010+
  '  DOS = Go32v2;'#010+
  '  MacOSX = Darwin;'#010+
  #010+
  '  AllOSs  = [Low(TOS)..High(TOS)];'#010+
  '  AllCPUs = [Low(TCPU)','..High(TCPU)];'#010+
  #010+
  '  // Useful'#010+
  '  UnitExt = '#039'.ppu'#039';'#010+
  '  PPUExt  = UnitExt;'#010+
  '  PasExt  = '#039'.pas'#039';'#010+
  '  PPExt   = '#039'.pp'#039';'#010+
  '  IncExt  = '#039'.inc'#039';'#010+
  '  ObjExt  = '#039'.o'#039';'#010+
  '  RstExt  = '#039'.rst'#039';'#010+
  '  LibExt  = '#039'.a'#039';'#010+
  '  SharedLibExt = '#039'.so'#039';'#010+
  '  DLLExt  = '#039'.dll'#039';'#010+
  '  ExeExt  = ',#039'.exe'#039';'#010+
  '  ZipExt  = '#039'.zip'#039';'#010+
  #010+
  '  ManifestFile = '#039'manifest.xml'#039';'#010+
  #010+
  '  UnitTargets = [ttUnit,ttExampleUnit];'#010+
  '  ProgramTargets = [ttProgram,ttExampleProgram];'#010+
  #010+
  '  AllMessages = [vlError,vlWarning,vlInfo,vlCompare,vlCommand];'#010+
  #010+
  'Type'#010+
  '  { TNamedItem }'#010,
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
  '  TNamedCollection = Clas','s(TCollection)'#010+
  '  private'#010+
  '    FUniqueNames: Boolean;'#010+
  '  Public'#010+
  '    Function IndexOfName(AName : String) : Integer;'#010+
  '    Function ItemByName(AName : String) : TNamedItem;'#010+
  '    Property UniqueNames : Boolean Read FUniqueNames;'#010+
  '  end;'#010+
  #010+
  '  { TNamedI','temList }'#010+
  #010+
  '  TNamedItemList = Class(TList)'#010+
  '  private'#010+
  '    function GetNamedItem(Index : Integer): TNamedItem;'#010+
  '    procedure SetNamedItem(Index : Integer; const AValue: TNamedItem);'+
  #010+
  '  public'#010+
  '    Function IndexOfName(AName : String) : Integer;',#010+
  '    Function ItemByName(ANAme : String) : TNamedItem;'#010+
  '    Property NamedItems[Index : Integer] : TNamedItem Read GetNamedIte'+
  'm Write SetNamedItem; default;'#010+
  '  end;'#010+
  #010+
  '  { TCommand }'#010+
  '  TCommand = Class(TNamedItem)'#010+
  '  private'#010+
  '    FAfterCommand: T','NotifyEvent;'#010+
  '    FBeforeCommand: TNotifyEvent;'#010+
  '    FCommand: String;'#010+
  '    FCommandAt: TCommandAt;'#010+
  '    FDestFile: String;'#010+
  '    FIgnoreResult: Boolean;'#010+
  '    FOptions: String;'#010+
  '    FSourceFile: String;'#010+
  '  Public'#010+
  '    Property SourceFile : String Rea','d FSourceFile Write FSourceFile;'+
  #010+
  '    Property DestFile : String Read FDestFile Write FDestFile;'#010+
  '    Property Command : String Read FCommand Write FCommand;'#010+
  '    Property Options : String Read FOptions Write FOptions;'#010+
  '    Property At : TComma','ndAt Read FCommandAt Write FCommandAt;'#010+
  '    Property IgnoreResult : Boolean Read FIgnoreResult Write FIgnoreRe'+
  'sult;'#010+
  '    Property BeforeCommand : TNotifyEvent Read FBeforeCommand Write FB'+
  'eforeCommand;'#010+
  '    Property AfterCommand : TNotifyEvent ','Read FAfterCommand Write FA'+
  'fterCommand;'#010+
  '  end;'#010+
  #010+
  '  { TCommands }'#010+
  #010+
  '  TCommands = Class(TNamedCollection)'#010+
  '  private'#010+
  '    FDefaultAt: TCommandAt;'#010+
  '    function GetCommand(Dest : String): TCommand;'#010+
  '    function GetCommandItem(Index : Integer): TCo','mmand;'#010+
  '    procedure SetCommandItem(Index : Integer; const AValue: TCommand);'+
  #010+
  '  Public'#010+
  '    Function AddCommand(Const Cmd : String) : TCommand;'#010+
  '    Function AddCommand(Const Cmd,Options : String) : TCommand;'#010+
  '    Function AddCommand(Const Cmd',',Options,Dest,Source : String) : TC'+
  'ommand;'#010+
  '    Function AddCommand(At : TCommandAt; Const Cmd : String) : TComman'+
  'd;'#010+
  '    Function AddCommand(At : TCommandAt; Const Cmd,Options : String) :'+
  ' TCommand;'#010+
  '    Function AddCommand(At : TCommandAt; Co','nst Cmd,Options, Dest,Sou'+
  'rce : String) : TCommand;'#010+
  '    Property CommandItems[Index : Integer] : TCommand Read GetCommandI'+
  'tem Write SetCommandItem;'#010+
  '    Property Commands[Dest : String] : TCommand Read GetCommand; defau'+
  'lt;'#010+
  '    Property Defaul','tAt : TCommandAt Read FDefaultAt Write FDefaultAt'+
  ';'#010+
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
  '    FB','eforeCompile: TNotifyEvent;'#010+
  '    FCPUs: TCPUs;'#010+
  '    FMode: TCompilerMode;'#010+
  '    FResourceStrings: Boolean;'#010+
  '    FObjectPath,'#010+
  '    FUnitPath,'#010+
  '    FIncludePath,'#010+
  '    FDependencies: TStrings;'#010+
  '    FCommands : TCommands;'#010+
  '    FDirectory: String;'#010+
  '    FEx','tension: String;'#010+
  '    FFileType: TFileType;'#010+
  '    FOptions: String;'#010+
  '    FOSes: TOSes;'#010+
  '    FFPCTarget: String;'#010+
  '    FTargetState: TTargetState;'#010+
  '    FTargetType: TTargetType;'#010+
  '    function GetCommands: TCommands;'#010+
  '    function GetHasCommands: Boole','an;'#010+
  '    function GetHasStrings(AIndex: integer): Boolean;'#010+
  '    function GetStrings(AIndex: integer): TStrings;'#010+
  '    procedure SetCommands(const AValue: TCommands);'#010+
  '    procedure SetStrings(AIndex: integer; const AValue: TStrings);'#010+
  '  Protected',#010+
  '    Function GetSourceFileName : String; virtual;'#010+
  '    Function GetUnitFileName : String; virtual;'#010+
  '    Function GetObjectFileName : String; virtual;'#010+
  '    Function GetRSTFileName : String; Virtual;'#010+
  '    Function GetProgramFileName(AnOS : TOS) ',': String; Virtual;'#010+
  '  Public'#010+
  '    Constructor Create(ACollection : TCollection); override;'#010+
  '    Destructor Destroy; override;'#010+
  '    Function GetOutputFileName (AOs : TOS) : String; Virtual;'#010+
  '    procedure SetName(const AValue: String);override;'#010+
  ' ','   Procedure GetCleanFiles(List : TStrings; APrefixU, APrefixB : St'+
  'ring; AnOS : TOS); virtual;'#010+
  '    Procedure GetSourceFiles(List : TStrings; APrefix : String; AnOS :'+
  ' TOS); virtual;'#010+
  '    Procedure GetInstallFiles(List : TStrings; APrefixU, AP','refixB: S'+
  'tring; AnOS : TOS); virtual;'#010+
  '    Procedure GetArchiveFiles(List : TStrings; APrefix : String; AnOS '+
  ': TOS); virtual;'#010+
  '    Property HasUnitPath : Boolean Index 0 Read GetHasStrings;'#010+
  '    Property HasObjectPath : Boolean Index 1 Read Ge','tHasStrings;'#010+
  '    Property HasIncludePath : Boolean Index 2 Read GetHasStrings;'#010+
  '    Property HasDependencies : Boolean Index 3 Read GetHasStrings;'#010+
  '    Property HasCommands : Boolean Read GetHasCommands;'#010+
  '    Property UnitPath : TStrings Index',' 0 Read GetStrings Write SetSt'+
  'rings;'#010+
  '    Property ObjectPath : TStrings Index 1  Read GetStrings Write SetS'+
  'trings;'#010+
  '    Property IncludePath : TStrings Index 2 Read GetStrings Write SetS'+
  'trings;'#010+
  '    Property Dependencies : TStrings Index 3 Re','ad GetStrings Write S'+
  'etStrings;'#010+
  '    Property Commands : TCommands Read GetCommands Write SetCommands;'#010+
  '    Property State : TTargetState Read FTargetState;'#010+
  '    Property TargetType : TTargetType Read FTargetType Write FTargetTy'+
  'pe;'#010+
  '    Propert','y OS : TOSes Read FOSes Write FOSes;'#010+
  '    Property Mode : TCompilerMode Read FMode Write FMode;'#010+
  '    Property Options : String Read FOptions Write Foptions;'#010+
  '    Property SourceFileName: String Read GetSourceFileName ;'#010+
  '    Property UnitFileNam','e : String Read GetUnitFileName;'#010+
  '    Property ObjectFileName : String Read GetObjectFileName;'#010+
  '    Property RSTFileName : String Read GetRSTFileName;'#010+
  '    Property CPU : TCPUs Read FCPUs Write FCPUs;'#010+
  '    Property FPCTarget : String Read FFPCT','arget Write FFPCTarget;'#010+
  '    Property Extension : String Read FExtension Write FExtension;'#010+
  '    Property FileType : TFileType Read FFileType Write FFileType;'#010+
  '    Property Directory : String Read FDirectory Write FDirectory;'#010+
  '    Property Resou','rceStrings : Boolean Read FResourceStrings Write F'+
  'ResourceStrings;'#010+
  '    Property Install : Boolean Read FInstall Write FInstall;'#010+
  '    // Events.'#010+
  '    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FB'+
  'eforeCompile;'#010+
  '    Property ','AfterCompile : TNotifyEvent Read FAfterCompile Write FA'+
  'fterCompile;'#010+
  '    Property BeforeClean : TNotifyEvent Read FBeforeClean Write FBefor'+
  'eClean;'#010+
  '    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterCl'+
  'ean;'#010+
  '  end;'#010+
  #010+
  '  { TTarget','s }'#010+
  #010+
  '  TTargets = Class(TNamedCollection)'#010+
  '  private'#010+
  '    FDefaultCPU: TCPUs;'#010+
  '    FDefaultDir : String;'#010+
  '    FDefaultOS: TOSes;'#010+
  '    function GetTargetItem(Index : Integer): TTarget;'#010+
  '    function GetTarget(AName : String): TTarget;'#010+
  '    procedur','e SetDefaultDir(const AValue: String);'#010+
  '    procedure SetTargetItem(Index : Integer; const AValue: TTarget);'#010+
  '    Procedure ApplyDefaults(ATarget : TTarget);'#010+
  '  Public'#010+
  '    Procedure ResetDefaults;'#010+
  '    Function AddUnit(AUnitName : String) : TTa','rget;'#010+
  '    Function AddProgram(AProgramName : String) : TTarget;'#010+
  '    Function AddExampleUnit(AUnitName : String) : TTarget;'#010+
  '    Function AddExampleProgram(AProgramName : String) : TTarget;'#010+
  '    Property Targets[AName : String] : TTarget Read ','GetTarget; defau'+
  'lt;'#010+
  '    Property TargetItems[Index : Integer] : TTarget Read GetTargetItem'+
  ' Write SetTargetItem;'#010+
  '    Property DefaultDir : String Read FDefaultDir Write SetDefaultDir;'+
  #010+
  '    Property DefaultOS : TOSes Read FDefaultOS Write FDef','aultOS;'#010+
  '    Property DefaultCPU : TCPUs Read FDefaultCPU Write FDefaultCPU;'#010+
  '  end;'#010+
  #010+
  '  { TSource }'#010+
  #010+
  '  TSource = Class(TNamedItem)'#010+
  '  private'#010+
  '    FSourceType: TSourceType;'#010+
  '  Public'#010+
  '    Constructor Create(ACollection : TCollection); override;'#010+
  ' ','   Destructor Destroy; override;'#010+
  '    procedure GetSourceFiles(List: TStrings);'#010+
  '    Property SourceType : TSourceType Read FSourceType Write FSourceTy'+
  'pe;'#010+
  '  end;'#010+
  #010+
  '  { TSources }'#010+
  #010+
  '  TSources = Class(TNamedCollection)'#010+
  '  private'#010+
  '    function Get','SourceItem(Index : Integer): TSource;'#010+
  '    procedure SetSourceItem(Index : Integer; const AValue: TSource);'#010+
  '  public'#010+
  '    Function AddDocFiles(AFiles : String) : TSource;'#010+
  '    Function AddSrcFiles(AFiles : String) : TSource;'#010+
  '    Function AddEx','ampleFiles(AFiles : String) : TSource;'#010+
  '    Function AddTestFiles(AFiles : String) : TSource;'#010+
  '    Property SourceItems[Index : Integer] : TSource Read GetSourceItem'+
  ' Write SetSourceItem;default;'#010+
  '  end;'#010+
  #010+
  '  { TPackage }'#010+
  #010+
  '  TPackage = Class(TNam','edItem) // Maybe descend from/use TTarget ?'#010+
  '  private'#010+
  '    FAfterArchive: TNotifyEvent;'#010+
  '    FAfterClean: TNotifyEvent;'#010+
  '    FAfterCompile: TNotifyEvent;'#010+
  '    FAfterInstall: TNotifyEvent;'#010+
  '    FAfterManifest: TNotifyEvent;'#010+
  '    FBeforeArchive: TN','otifyEvent;'#010+
  '    FBeforeClean: TNotifyEvent;'#010+
  '    FBeforeCompile: TNotifyEvent;'#010+
  '    FBeforeInstall: TNotifyEvent;'#010+
  '    FBeforeManifest: TNotifyEvent;'#010+
  '    FUnitPath,'#010+
  '    FObjectPath,'#010+
  '    FIncludePath,'#010+
  '    FCleanFiles,'#010+
  '    FArchiveFiles,'#010+
  '    FIn','stallFiles,'#010+
  '    FDependencies: TStrings;'#010+
  '    FCPU: TCPUs;'#010+
  '    FOS: TOses;'#010+
  '    FTargetState: TTargetState;'#010+
  '    FTargets: TTargets;'#010+
  '    FSources: TSources;'#010+
  '    FDirectory: String;'#010+
  '    FOptions: String;'#010+
  '    FFileName: String;'#010+
  '    FAuthor: Stri','ng;'#010+
  '    FLicense: String;'#010+
  '    FExternalURL: String;'#010+
  '    FVersion: String;'#010+
  '    FEmail : String;'#010+
  '    FCommands : TCommands;'#010+
  '    FDescriptionFile : String;'#010+
  '    FDescription : String;'#010+
  '    Function GetDescription : string;'#010+
  '    Function GetFileNa','me : string;'#010+
  '    function GetCommands: TCommands;'#010+
  '    function GetHasCommands: Boolean;'#010+
  '    function GetHasStrings(AIndex: integer): Boolean;'#010+
  '    function GetStrings(AIndex: integer): TStrings;'#010+
  '    procedure SetCommands(const AValue: TComma','nds);'#010+
  '    procedure SetStrings(AIndex: integer; const AValue: TStrings);'#010+
  '  Public'#010+
  '    constructor Create(ACollection: TCollection); override;'#010+
  '    destructor destroy; override;'#010+
  '    Function AddTarget(AName : String) : TTarget;'#010+
  '    Procedure ','AddDependency(AName : String);'#010+
  '    Procedure AddInstallFile(AFileName : String);'#010+
  '    Procedure GetCleanFiles(List : TStrings; Const APrefixU, APrefixB '+
  ': String; AOS : TOS); virtual;'#010+
  '    procedure GetInstallFiles(List: TStrings;Types : TTarg','etTypes;Co'+
  'nst APrefix, APrefixU, APrefixB: String; AOS : TOS);'#010+
  '    Procedure GetArchiveFiles(List : TStrings; Const APrefix : String;'+
  ' AOS : TOS); virtual;'#010+
  '    Procedure GetManifest(Manifest : TStrings);'#010+
  '    Property Version : String Read FV','ersion Write FVersion;'#010+
  '    Property FileName : String Read GetFileName Write FFileName;'#010+
  '    Property ExternalURL : String Read FExternalURL Write FExternalURL'+
  ';'#010+
  '    Property Email : String Read FEmail Write FEmail;'#010+
  '    Property Author : Stri','ng Read FAuthor Write FAuthor;'#010+
  '    Property License : String Read FLicense Write FLicense;'#010+
  '    Property Directory : String Read FDirectory Write FDirectory;'#010+
  '    Property Description : String Read GetDescription Write FDescripti'+
  'on;'#010+
  '    Prope','rty DescriptionFile : String Read FDescriptionFile Write FD'+
  'escriptionFile;'#010+
  '    // Compiler options.'#010+
  '    Property OS : TOses Read FOS Write FOS;'#010+
  '    Property CPU : TCPUs Read FCPU Write FCPU;'#010+
  '    Property Options: String Read FOptions Write ','FOptions;'#010+
  '    Property HasUnitPath : Boolean Index 0 Read GetHasStrings;'#010+
  '    Property HasObjectPath : Boolean Index 1 Read GetHasStrings;'#010+
  '    Property HasIncludePath : Boolean Index 2 Read GetHasStrings;'#010+
  '    Property HasDependencies : Boole','an Index 3 Read GetHasStrings;'#010+
  '    Property HasInstallFiles: Boolean Index 4 Read GetHasStrings;'#010+
  '    Property HasCleanFiles : Boolean Index 5 Read GetHasStrings;'#010+
  '    Property HasArchiveFiles : Boolean Index 6 Read GetHasStrings;'#010+
  '    Propert','y HasCommands : Boolean Read GetHasCommands;'#010+
  '    Property UnitPath : TStrings Index 0 Read GetStrings Write SetStri'+
  'ngs;'#010+
  '    Property ObjectPath : TStrings Index 1  Read GetStrings Write SetS'+
  'trings;'#010+
  '    Property IncludePath : TStrings Index ','2 Read GetStrings Write Se'+
  'tStrings;'#010+
  '    // Targets and dependencies'#010+
  '    Property Dependencies : TStrings Index 3 Read GetStrings Write Set'+
  'Strings;'#010+
  '    Property InstallFiles : TStrings Index 4 Read GetStrings Write Set'+
  'Strings;'#010+
  '    Property C','leanFiles : TStrings Index 5 Read GetStrings Write Set'+
  'Strings;'#010+
  '    Property ArchiveFiles : TStrings Index 6 Read GetStrings Write Set'+
  'Strings;'#010+
  '    Property Commands : TCommands Read GetCommands Write SetCommands;'#010+
  '    Property State : TTarget','State Read FTargetState;'#010+
  '    Property Targets : TTargets Read FTargets;'#010+
  '    Property Sources : TSources Read FSources;'#010+
  '    // events'#010+
  '    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FB'+
  'eforeCompile;'#010+
  '    Property AfterCompi','le : TNotifyEvent Read FAfterCompile Write FA'+
  'fterCompile;'#010+
  '    Property BeforeInstall : TNotifyEvent Read FBeforeInstall Write FB'+
  'eforeInstall;'#010+
  '    Property AfterInstall : TNotifyEvent Read FAfterInstall Write FAft'+
  'erInstall;'#010+
  '    Property Befo','reClean : TNotifyEvent Read FBeforeClean Write FBef'+
  'oreClean;'#010+
  '    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterCl'+
  'ean;'#010+
  '    Property BeforeArchive : TNotifyEvent Read FBeforeArchive Write FB'+
  'eforeArchive;'#010+
  '    Property AfterAr','chive : TNotifyEvent Read FAfterArchive Write FA'+
  'fterArchive;'#010+
  '    Property BeforeManifest : TNotifyEvent Read FBeforeManifest Write '+
  'FBeforeManifest;'#010+
  '    Property AfterManifest : TNotifyEvent Read FAfterManifest Write FA'+
  'fterManifest;'#010+
  '  end;'#010+
  #010,
  '  { TPackages }'#010+
  #010+
  '  TPackages = Class(TNamedCollection)'#010+
  '  private'#010+
  '    function GetPackage(AName : String): TPackage;'#010+
  '    function GetPackageItem(AIndex : Integer): TPackage;'#010+
  '    procedure SetPackageItem(AIndex : Integer; const AValue: TPacka','g'+
  'e);'#010+
  '  Public'#010+
  '    Function AddPackage(Const AName : String) : TPackage;'#010+
  '    Property Packages[AName : String] : TPackage Read GetPackage ; Def'+
  'ault;'#010+
  '    Property PackageItems[AIndex : Integer] : TPackage Read GetPackage'+
  'Item Write SetPackageI','tem;'#010+
  '  end;'#010+
  #010+
  '  { TCustomDefaults }'#010+
  #010+
  '  TCustomDefaults = Class(TPersistent)'#010+
  '  Private'#010+
  '    FArchive: String;'#010+
  '    FCompiler: String;'#010+
  '    FCopy: String;'#010+
  '    FMkDir: String;'#010+
  '    FMove: String;'#010+
  '    FOptions: String;'#010+
  '    FCPU: TCPU;'#010+
  '    FOS: TOS;'#010,
  '    FMode : TCompilerMode;'#010+
  '    FCompilerVersion : String;'#010+
  '    FPrefix: String;'#010+
  '    FBaseInstallDir,'#010+
  '    FUnitInstallDir,'#010+
  '    FBinInstallDir,'#010+
  '    FDocInstallDir,'#010+
  '    FExamplesInstallDir : String;'#010+
  '    FRemove: String;'#010+
  '    FTarget: String;'#010+
  '   ',' FUnixPaths: Boolean;'#010+
  '    function GetBaseInstallDir: String;'#010+
  '    function GetBinInstallDir: String;'#010+
  '    function GetCompiler: String;'#010+
  '    function GetDocInstallDir: String;'#010+
  '    function GetExamplesInstallDir: String;'#010+
  '    function GetUnitIn','stallDir: String;'#010+
  '    procedure SetBaseInstallDir(const AValue: String);'#010+
  '    procedure SetCPU(const AValue: TCPU);'#010+
  '    procedure SetOS(const AValue: TOS);'#010+
  '    procedure SetPrefix(const AValue: String);'#010+
  '    procedure SetTarget(const AValue: ','String);'#010+
  '  Protected'#010+
  '    procedure RecalcTarget;'#010+
  '  Public'#010+
  '    Constructor Create;'#010+
  '    Procedure InitDefaults;'#010+
  '    Procedure Assign(ASource : TPersistent);override;'#010+
  '    procedure CompilerDefaults; virtual;'#010+
  '    Procedure LocalInit(Const AFile','Name : String);'#010+
  '    Procedure LoadFromFile(Const AFileName : String);'#010+
  '    Procedure SaveToFile(Const AFileName : String);'#010+
  '    procedure SaveToStream(S : TStream);virtual;'#010+
  '    procedure LoadFromStream(S : TStream);virtual;'#010+
  '    // Compile Inf','ormation'#010+
  '    Property Target : String Read FTarget Write SetTarget;'#010+
  '    Property OS : TOS Read FOS Write SetOS;'#010+
  '    Property CPU : TCPU Read FCPU Write SetCPU;'#010+
  '    Property Mode : TCompilerMode Read FMode Write FMode;'#010+
  '    Property UnixPaths',' : Boolean Read FUnixPaths Write FUnixPaths;'#010+
  '    Property Options : String Read FOptions Write FOptions;    // Defa'+
  'ult compiler options.'#010+
  '    // paths etc.'#010+
  '    Property Prefix : String Read FPrefix Write SetPrefix;'#010+
  '    Property BaseInstallDi','r : String Read GetBaseInstallDir Write Se'+
  'tBaseInstallDir;'#010+
  '    Property UnitInstallDir : String Read GetUnitInstallDir Write FUni'+
  'tInstallDir;'#010+
  '    Property BinInstallDir : String Read GetBinInstallDir Write FBinIn'+
  'stallDir;'#010+
  '    Property DocIn','stallDir : String Read GetDocInstallDir Write FDoc'+
  'InstallDir;'#010+
  '    Property ExamplesInstallDir : String Read GetExamplesInstallDir Wr'+
  'ite FExamplesInstallDir;'#010+
  '    // Command tools. If not set, internal commands  will be used.'#010+
  '    Property Com','piler : String Read GetCompiler Write FCompiler; // '+
  'Compiler. Defaults to fpc'#010+
  '    Property Copy : String Read FCopy Write FCopy;             // copy'+
  ' $(FILES) to $(DEST)'#010+
  '    Property Move : String Read FMove Write FMove;             // Move'+
  ' ','$(FILES) to $(DEST)'#010+
  '    Property Remove : String Read FRemove Write FRemove;       // Dele'+
  'te $(FILES)'#010+
  '    Property MkDir : String Read FMkDir write FMkDir;          // Make'+
  ' $(DIRECTORY)'#010+
  '    Property Archive : String Read FArchive Write FArc','hive;    // zi'+
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
  '  end;'#010,
  #010+
  '  { TBuildEngine }'#010+
  #010+
  '  TBuildEngine = Class(TComponent)'#010+
  '  private'#010+
  '    // general variables'#010+
  '    FCompiler : String;'#010+
  '    FStartDir : String;'#010+
  '    FTargetDir : String;'#010+
  '    FDefaults : TCustomDefaults;'#010+
  '    FForceCompile : Boolean;'#010+
  '    FListMode ',': Boolean;'#010+
  '{$ifdef HAS_UNIT_ZIPPER}'#010+
  '    FZipFile: TZipper;'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  '    // Variables used when compiling a package.'#010+
  '    // Only valid during compilation of the package.'#010+
  '    FCurrentOutputDir : String;'#010+
  '    FCurrentPackage: TP','ackage;'#010+
  '    // Events'#010+
  '    FOnLog: TLogEvent;'#010+
  '    FAfterArchive: TNotifyEvent;'#010+
  '    FAfterClean: TNotifyEvent;'#010+
  '    FAfterCompile: TNotifyEvent;'#010+
  '    FAfterInstall: TNotifyEvent;'#010+
  '    FAfterManifest: TNotifyEvent;'#010+
  '    FBeforeArchive: TNotifyEven','t;'#010+
  '    FBeforeClean: TNotifyEvent;'#010+
  '    FBeforeCompile: TNotifyEvent;'#010+
  '    FBeforeInstall: TNotifyEvent;'#010+
  '    FBeforeManifest: TNotifyEvent;'#010+
  '    procedure SetDefaults(const AValue: TCustomDefaults);'#010+
  '    procedure SetTargetDir(const AValue: Str','ing);'#010+
  '  Protected'#010+
  '    Procedure Error(Msg : String);'#010+
  '    Procedure Error(Fmt : String; Args : Array of const);'#010+
  '    // Internal copy/delete/move/archive/mkdir files'#010+
  '    Procedure SysCopyFile(Const Src,Dest : String); virtual;'#010+
  '    Procedure S','ysMoveFile(Const Src,Dest : String); virtual;'#010+
  '    Procedure SysDeleteFile(Const AFileName : String); virtual;'#010+
  '    Procedure SysArchiveFiles(List : TStrings; Const AFileName : Strin'+
  'g); virtual;'#010+
  '    Procedure Log(Level : TVerboseLevel; Const ','Msg : String);'#010+
  '    Procedure Log(Level : TVerboseLevel; Const Fmt : String; Args : Ar'+
  'ray Of Const);'#010+
  '    Procedure EnterDir(ADir : String);'#010+
  '    Function GetCompiler : String;'#010+
  '    Procedure InstallPackageFiles(APAckage : TPackage; tt : TTarge','tT'+
  'ype; Const Src,Dest : String); virtual;'#010+
  '    Function FileNewer(Src,Dest : String) : Boolean;'#010+
  #010+
  '    //package commands'#010+
  '    Function  GetOutputDir(AName: string; APackage : TPackage; Absolut'+
  'ePath : Boolean = False) : String;'#010+
  #010+
  '  Public'#010+
  '    Co','nstructor Create(AOwner : TComponent); override;'#010+
  '    // Public Copy/delete/Move/Archive/Mkdir Commands.'#010+
  '    Procedure ExecuteCommand(Cmd : String; Args : String; IgnoreError '+
  ': Boolean = False); virtual;'#010+
  '    Procedure CmdCopyFiles(List : TSt','rings; Const DestDir : String);'+
  #010+
  '    Procedure CmdCreateDir(DestDir : String);'#010+
  '    Procedure CmdMoveFiles(List : TStrings; Const DestDir : String);'#010+
  '    Procedure CmdDeleteFiles(List : TStrings);'#010+
  '    Procedure CmdArchiveFiles(List : TStrings;',' Const ArchiveFile : S'+
  'tring);'#010+
  '    Procedure ExecuteCommands(Commands : TCommands; At : TCommandAt);'#010+
  '    // Target commands'#010+
  '    Function  GetTargetDir(APackage : TPackage; ATarget : TTarget; Abs'+
  'olutePath : Boolean = False) : String;'#010+
  '    Func','tion  GetCompilerCommand(APackage : TPackage; Target : TTarg'+
  'et) : String;'#010+
  '    Function  TargetOK(Target : TTarget) : Boolean;'#010+
  '    Function  NeedsCompile(Target : TTarget) : Boolean;'#010+
  '    Procedure Compile(Target : TTarget);  virtual;'#010+
  '    Pro','cedure FixDependencies(Target: TTarget);'#010+
  '    // Package commands'#010+
  '    Function  GetPackageDir(APackage : TPackage; AbsolutePath : Boolea'+
  'n = False) : String;'#010+
  '    Function  GetUnitsOutputDir(APackage : TPackage; AbsolutePath : Bo'+
  'olean = False)',' : String;'#010+
  '    Function  GetBinOutputDir(APackage : TPackage; AbsolutePath : Bool'+
  'ean = False) : String;'#010+
  '    Function  PackageOK(APackage : TPackage) : Boolean; virtual;'#010+
  '    Procedure DoBeforeCompile(APackage : TPackage);virtual;'#010+
  '    Procedu','re DoAfterCompile(APackage : TPackage);virtual;'#010+
  '    Procedure DoBeforeInstall(APackage : TPackage);virtual;'#010+
  '    Procedure DoAfterInstall(APackage : TPackage);virtual;'#010+
  '    Procedure DoBeforeArchive(APackage : TPackage);virtual;'#010+
  '    Procedure',' DoAfterArchive(APackage : TPackage);virtual;'#010+
  '    Procedure DoBeforeClean(APackage : TPackage);virtual;'#010+
  '    Procedure DoAfterClean(APackage : TPackage);virtual;'#010+
  '    Function NeedsCompile(APackage : TPackage) : Boolean; virtual;'#010+
  '    Procedur','e Compile(APackage : TPackage);'#010+
  '    Procedure Install(APackage : TPackage);'#010+
  '    Procedure Archive(APackage : TPackage);'#010+
  '    Procedure Clean(APackage : TPackage);'#010+
  '    Procedure FixDependencies(APackage : TPackage);'#010+
  '    Procedure GetSourceFil','es(APackage : TPackage; List : TStrings);'#010+
  '    Procedure GetManifest(APackage : TPackage; Manifest : TStrings);'#010+
  '    procedure CheckExternalPackage(Const APackageName : String);'#010+
  '    procedure CreateOutputDir(APackage: TPackage);'#010+
  '    // Packag','es commands'#010+
  '    Procedure Compile(Packages : TPackages);'#010+
  '    Procedure Install(Packages : TPackages);'#010+
  '    Procedure Archive(Packages : TPackages);'#010+
  '    Procedure Clean(Packages : TPackages);'#010+
  '    Procedure GetSourceFiles(Packages : TPackages;','List : TStrings);'#010+
  '    Procedure GetManifest(Packages : TPackages; Manifest : TStrings);'#010+
  '    Property ListMode : Boolean Read FListMode Write FListMode;'#010+
  '    Property ForceCompile : Boolean Read FForceCompile Write FForceCom'+
  'pile;'#010+
  '    Property',' Defaults : TCustomDefaults Read FDefaults Write SetDefa'+
  'ults;'#010+
  '    Property TargetDir : String Read FTargetDir Write SetTargetDir;'#010+
  '    // Events'#010+
  '    Property BeforeCompile : TNotifyEvent Read FBeforeCompile Write FB'+
  'eforeCompile;'#010+
  '    Property',' AfterCompile : TNotifyEvent Read FAfterCompile Write FA'+
  'fterCompile;'#010+
  '    Property BeforeInstall : TNotifyEvent Read FBeforeInstall Write FB'+
  'eforeInstall;'#010+
  '    Property AfterInstall : TNotifyEvent Read FAfterInstall Write FAft'+
  'erInstall;'#010+
  '    Pr','operty BeforeClean : TNotifyEvent Read FBeforeClean Write FBef'+
  'oreClean;'#010+
  '    Property AfterClean : TNotifyEvent Read FAfterClean Write FAfterCl'+
  'ean;'#010+
  '    Property BeforeArchive : TNotifyEvent Read FBeforeArchive Write FB'+
  'eforeArchive;'#010+
  '    Prope','rty AfterArchive : TNotifyEvent Read FAfterArchive Write FA'+
  'fterArchive;'#010+
  '    Property BeforeManifest : TNotifyEvent Read FBeforeManifest Write '+
  'FBeforeManifest;'#010+
  '    Property AfterManifest : TNotifyEvent Read FAfterManifest Write FA'+
  'fterManifes','t;'#010+
  '    Property OnLog : TLogEvent Read FOnLog Write FOnlog;'#010+
  '  end;'#010+
  #010+
  '  { TCustomInstaller }'#010+
  #010+
  '  TCustomInstaller = Class(TComponent)'#010+
  '  private'#010+
  '    FBaseInstallDir: string;'#010+
  '    FBuildEngine: TBuildEngine;'#010+
  '    FDefaultPackage: TPackage;'#010+
  '    FDe','faults: TCustomDefaults;'#010+
  '    FPackages: TPackages;'#010+
  '    FRunMode: TRunMode;'#010+
  '    FListMode : Boolean;'#010+
  '    FLogLevels : TVerboseLevels;'#010+
  '    function GetBaseInstallDir: string;'#010+
  '    Function GetPackageString(Index : Integer) : String;'#010+
  '    Proced','ure SetPackageString(Index : Integer; AValue : String);'#010+
  '    function GetStrings(AIndex : Integer): TStrings;'#010+
  '    function GetOSes: TOSes;'#010+
  '    function GetTargets: TTargets;'#010+
  '    function GetSources: TSources;'#010+
  '    procedure SetBaseInstallDir(','AValue: string);'#010+
  '    procedure SetDefaultPackage(const AValue: TPackage);'#010+
  '    procedure SetDefaults(const AValue: TCustomDefaults);'#010+
  '    procedure SetStrings(AIndex : Integer; const AValue: TStrings);'#010+
  '    procedure SetOses(const AValue: TOSe','s);'#010+
  '    procedure SearchFiles(FileName: string; Recursive: boolean; var Li'+
  'st: TStrings);'#010+
  '  Protected'#010+
  '    Procedure Log(Level : TVerboseLevel; Const Msg : String);'#010+
  '    Procedure CreatePackages; virtual;'#010+
  '    Procedure CheckPackages; virtual;'#010,
  '    Procedure CreateBuildEngine; virtual;'#010+
  '    Procedure CheckDefaultPackage;'#010+
  '    Procedure Error(Msg : String);'#010+
  '    Procedure Error(Fmt : String; Args : Array of const);'#010+
  '    Procedure AnalyzeOptions;'#010+
  '    Procedure Usage(FMT : String; Args :',' Array of const);'#010+
  '    Procedure Compile(Force : Boolean); virtual;'#010+
  '    Procedure Clean; virtual;'#010+
  '    Procedure Install; virtual;'#010+
  '    Procedure Archive; virtual;'#010+
  '    Procedure Manifest; virtual;'#010+
  '    Procedure GetSourceFiles; virtual;'#010+
  '    Pro','perty BuildEngine : TBuildEngine Read FBuildEngine;'#010+
  '  Public'#010+
  '    Constructor Create(AOwner : TComponent); virtual; abstract;'#010+
  '    Destructor destroy; override;'#010+
  '    Function StartPackage(Const AName : String) : TPackage;'#010+
  '    Procedure EndPack','age;'#010+
  '    Function Run : Boolean;'#010+
  '    Function AddTarget(AName : String) : TTarget;'#010+
  '    Procedure AddDependency(AName : String);'#010+
  '    //files in package'#010+
  '    procedure AddDocFiles(AFileMask: string; Recursive: boolean = Fals'+
  'e);'#010+
  '    procedure A','ddSrcFiles(AFileMask: string; Recursive: boolean = Fa'+
  'lse);'#010+
  '    procedure AddExampleFiles(AFileMask: string; Recursive: boolean = '+
  'False);'#010+
  '    procedure AddTestFiles(AFileMask: string; Recursive: boolean = Fal'+
  'se);'#010+
  '    Property DefaultPackage ',': TPackage read FDefaultPackage write Se'+
  'tDefaultPackage;'#010+
  '    Property Packages : TPackages Read FPackages;'#010+
  '    Property Dependencies : TStrings Index 0 Read GetStrings Write Set'+
  'Strings;'#010+
  '    Property InstallFiles : TStrings Index 1 Read GetS','trings Write S'+
  'etStrings;'#010+
  '    Property CleanFiles : TStrings Index 2 Read GetStrings Write SetSt'+
  'rings;'#010+
  '    Property ArchiveFiles : TStrings Index 3 Read GetStrings Write Set'+
  'Strings;'#010+
  '    Property Defaults : TCustomDefaults Read FDefaults Writ','e SetDefa'+
  'ults;'#010+
  '    Property RunMode : TRunMode Read FRunMode;'#010+
  '    Property ListMode : Boolean Read FListMode;'#010+
  '    Property BaseInstallDir : String Read GetBaseInstallDir Write SetB'+
  'aseInstallDir;'#010+
  '    // Default Package redirects.'#010+
  '    Propert','y Targets : TTargets Read GetTargets;'#010+
  '    Property Sources : TSources Read GetSources;'#010+
  '    Property OS: TOSes Read GetOSes Write SetOses;'#010+
  '    Property Author : String Index 0 Read GetPackageString Write SetPa'+
  'ckageString;'#010+
  '    Property Direct','ory : String Index 1 Read GetPackageString Write '+
  'SetPackageString;'#010+
  '    Property License : String Index 2 Read GetPackageString Write SetP'+
  'ackageString;'#010+
  '    Property Options : String Index 3 Read GetPackageString Write SetP'+
  'ackageString;'#010+
  '    P','roperty ExternalURL : String Index 4 Read GetPackageString Writ'+
  'e SetPackageString;'#010+
  '    Property Email : String Index 5 Read GetPackageString Write SetPac'+
  'kageString;'#010+
  '    Property Description: String Index 6 Read GetPackageString Write S'+
  'etPac','kageString;'#010+
  '    Property DescriptionFileName: String Index 7 Read GetPackageString'+
  ' Write SetPackageString;'#010+
  '    Property Version : String Index 8 Read GetPackageString Write SetP'+
  'ackageString;'#010+
  '    Property FileName : String Index 9 Read GetPa','ckageString Write S'+
  'etPackageString;'#010+
  '  end;'#010+
  #010+
  '  { TFPCInstaller }'#010+
  '  TFPCInstaller = class(TCustomInstaller)'#010+
  '  public'#010+
  '    Constructor Create(AOwner : TComponent); override;'#010+
  '  end;'#010+
  #010+
  '  { TBasicInstaller }'#010+
  '  TBasicInstaller = class(TCustomInstall','er)'#010+
  '    Constructor Create(AOwner : TComponent); override;'#010+
  '  end;'#010+
  #010+
  '  TReplaceFunction = Function (Const AName,Args : String) : String of '+
  'Object;'#010+
  #010+
  '  { TValueItem }'#010+
  #010+
  '  TValueItem = Class(TObject)'#010+
  '    FValue : String;'#010+
  '    Constructor Create(AV','alue : String);'#010+
  '  end;'#010+
  #010+
  '  { TFunctionItem }'#010+
  #010+
  '  TFunctionItem = Class(TObject)'#010+
  '    FFunc : TReplaceFunction;'#010+
  '    Constructor Create(AFunc : TReplaceFunction);'#010+
  '  end;'#010+
  #010+
  '  { TDictionary }'#010+
  #010+
  '  TDictionary = Class(TComponent)'#010+
  '    FList : TStringLi','st;'#010+
  '  Public'#010+
  '    Constructor Create(AOwner : TComponent); override;'#010+
  '    Destructor Destroy;override;'#010+
  '    Procedure AddVariable(Const AName,Value : String);'#010+
  '    Procedure AddFunction(Const AName : String; FReplacement : TReplac'+
  'eFunction);'#010+
  '  ','  Procedure RemoveItem(Const AName : String);'#010+
  '    Function GetValue(Const AName : String) : String;'#010+
  '    Function GetValue(Const AName,Args : String) : String; virtual;'#010+
  '    Function ReplaceStrings(Const ASource : String) : String; virtual;'+
  #010+
  ' ',' end;'#010+
  #010+
  '  ECollectionError = Class(Exception);'#010+
  '  EDictionaryError = Class(Exception);'#010+
  '  EInstallerError = Class(Exception);'#010+
  #010+
  '  TInstallerClass = Class of TCustomInstaller;'#010+
  '  TDictionaryClass = Class of TDictionary;'#010+
  #010+
  'Type'#010+
  '  TArchiveEvent = Pr','ocedure (Const AFileName : String; List : TStrin'+
  'gs) of Object;'#010+
  '  TArchiveProc = Procedure (Const AFileName : String; List : TStrings)'+
  ';'#010+
  #010+
  'Var'#010+
  '  DictionaryClass : TDictionaryClass = TDictionary;'#010+
  '  OnArchiveFiles : TArchiveEvent = Nil;'#010+
  '  Archiv','eFilesProc : TArchiveProc = Nil;'#010+
  #010+
  'Function CurrentOS : String;'#010+
  'Function CurrentCPU : String;'#010+
  #010+
  'Function Installer(InstallerClass: TInstallerClass) : TCustomInstaller'+
  '; overload;'#010+
  'Function Installer : TCustomInstaller; overload;'#010+
  #010+
  'Function Defau','lts : TCustomDefaults; // Set by installer.'#010+
  'Function Dictionary : TDictionary;'#010+
  #010+
  'Function OSToString(OS: TOS) : String;'#010+
  'Function OSesToString(OSes: TOSes) : String;'#010+
  'Function CPUToString(CPU: TCPU) : String;'#010+
  'Function CPUSToString(CPUS: TCPUS)',' : String;'#010+
  'Function StringToOS(S : String) : TOS;'#010+
  'Function OSesToString(S : String) : TOSes;'#010+
  'Function StringToCPU(S : String) : TCPU;'#010+
  'Function StringToCPUS(S : String) : TCPUS;'#010+
  'Function ModeToString(Mode: TCompilerMode) : String;'#010+
  'Function S','tringToMode(S : String) : TCompilerMode;'#010+
  'Function MakeTargetString(CPU : TCPU;OS: TOS) : String;'#010+
  'Procedure StringToCPUOS(S : String; Var CPU : TCPU; Var OS: TOS);'#010+
  'Procedure ResolveDependencies(L : TStrings; P : TNamedCollection);'#010+
  'Function A','ddStrings(Dest,Src : TStrings) : Integer ;'#010+
  'function AddStrings(Dest, Src : TStrings; Const APrefix : String) : In'+
  'teger ;'#010+
  'Function FileListToString(List : TStrings; Prefix : String) : String;'#010+
  'Function FixPath (APath : String) : String;'#010+
  'Proce','dure ChangeDir(APath : String);'#010+
  'Function Substitute(Const Source : String; Macros : Array of string) :'+
  ' String;'#010+
  'Procedure SplitCommand(Const Cmd : String; Var Exe,Options : String);'#010+
  #010+
  'Implementation'#010+
  #010+
  'uses typinfo;'#010+
  #010+
  'ResourceString'#010+
  '  SErrInvali','dCPU        = '#039'Invalid CPU name : "%s"'#039';'#010+
  '  SErrInvalidOS         = '#039'Invalid OS name : "%s"'#039';'#010+
  '  SErrInvalidMode       = '#039'Invalid compiler mode : "%s"'#039';'#010+
  '  SErrInvalidTarget     = '#039'Invalid compiler target: %s'#039';'#010+
  '  SErrNameExists        = '#039'Name ','"%s" already exists in the colle'+
  'ction.'#039';'#010+
  '  SErrNoSuchName        = '#039'Could not find item with name "%s" in t'+
  'he collection.'#039';'#010+
  '  SErrNoPackage         = '#039'No package available. Add package with '+
  'StartPackage Call'#039';'#010+
  '  SErrInValidArgument   = '#039'Inv','alid command-line argument at posi'+
  'tion %d : %s'#039';'#010+
  '  SErrNeedArgument      = '#039'Option at position %d (%s) needs an arg'+
  'ument'#039';'#010+
  '  SErrNoPackagesDefined = '#039'No action possible: No packages were de'+
  'fined.'#039';'#010+
  '  SErrInstaller         = '#039'The installer e','ncountered the followi'+
  'ng error:'#039';'#010+
  '  SErrDepUnknownTarget  = '#039'Unknown target in dependencies for %s: '+
  '%s'#039';'#010+
  '  SErrExternalCommandFailed = '#039'External command "%s" failed with e'+
  'xit code: %d'#039';'#010+
  '  SErrCreatingDirectory = '#039'Failed to create directory:',' %s'#039';'#010+
  '  SErrDeletingFile      = '#039'Failed to delete file: %s'#039';'#010+
  '  SErrMovingFile        = '#039'Failed to move file "%s" to "%s"'#039';'#010+
  '  SErrCopyingFile       = '#039'Failed to copy file "%s" to "%s"'#039';'#010+
  '  SErrChangeDirFailed   = '#039'Failed to enter directory: %','s'#039';'#010+
  '  SErrInvalidArgumentToSubstitute = '#039'Invalid number of arguments t'+
  'o Substitute'#039';'#010+
  '  SErrNoArchiveSupport  = '#039'This binary contains no archive support'+
  '. Please recompile with archive support'#039';'#010+
  '  SErrNoDictionaryItem  = '#039'No item called "%s" ','in the dictionary'+
  #039';'#010+
  '  SErrNoDictionaryValue = '#039'The item "%s" in the dictionary is not '+
  'a value.'#039';'#010+
  '  SErrNoDictionaryFunc  = '#039'The item "%s" in the dictionary is not '+
  'a function.'#039';'#010+
  '  SErrInvalidFPCInfo    = '#039'Compiler returns invalid information',','+
  ' check if fpc -iV works'#039';'#010+
  '  SWarnCircularDependency = '#039'Warning: Circular dependency detected'+
  ' when compiling target %s: %s'#039';'#010+
  '  SWarnFailedToSetTime  = '#039'Warning: Failed to set timestamp on fil'+
  'e : %s'#039';'#010+
  '  SWarnFailedToGetTime  = '#039'Warning: Fail','ed to get timestamp from'+
  ' file : %s'#039';'#010+
  '  SWarnFileDoesNotExist = '#039'Warning: File "%s" does not exist'#039';'+
  #010+
  '  SWarnAttemptingToCompileNonNeutralTarget = '#039'Attempting to compil'+
  'e non-neutral target: %s'#039';'#010+
  '  SDebugCheckingDependenciesForTarget = '#039'Checki','ng dependencies f'+
  'or target: %s'#039';'#010+
  '  SDebugCheckingSAgainstS  = '#039'Checking : %s against %s'#039';'#010+
  '  SDebugCheckingS          = '#039'Checking : %s'#039';'#010+
  '  SDebugTargetHasWrongOS   = '#039'Target has wrong OS: %s'#039';'#010+
  '  SDebugTargetHasWrongCPU  = '#039'Target has wrong C','PU: %s'#039';'#010+
  '  SDebugTargetIsNotAUnitOrProgram = '#039'Target is not a unit or progr'+
  'am'#039';'#010+
  '  SDebugConsideringTarget = '#039'Considering target: %s'#039';'#010+
  '  SDebugUnresolvedExternalDependencyS = '#039'Unresolved external depen'+
  'dency : %s'#039';'#010+
  '  SDebugBuildEngineArchivin','g = '#039'Build engine archiving.'#039';'#010+
  '  SDebugBuildEngineCleaning = '#039'Build engine cleaning.'#039';'#010+
  '  SCmdGenerating         = '#039'Generating %s'#039';'#010+
  '  SInfoArchiving         = '#039'Archiving : %s'#039';'#010+
  #010+
  '  // Log messages'#010+
  '  SLogEnterDir           = '#039'Entering director','y: %s'#039';'#010+
  '  SLogCompilingPackage   = '#039'Compiling package : %s'#039';'#010+
  '  SLogCompilingTarget    = '#039'Compiling target  : %s'#039';'#010+
  '  SLogExecutingCommand   = '#039'Executing command %s with options: %s'#039+
  ';'#010+
  '  SLogCreatingOutputDir  = '#039'Creating output dir : %s'#039';'#010+
  '  S','LogOutputDirExists    = '#039'Output dir exists : %s'#039';'#010+
  '  SLogInstallingPackage  = '#039'Installing package : %s'#039';'#010+
  '  SLogArchivingPackage   = '#039'Archiving package : %s'#039';'#010+
  '  SLogCleaningPackage    = '#039'Cleaning package : %s'#039';'#010+
  '  SLogCopyingFile        = '#039'Cop','ying file "%s" to "%s"'#039';'#010+
  '  SLogCompilingFileTimes = '#039'Comparing file "%s" time "%s" to "%s" '+
  'time "%s".'#039';'#010+
  '  SLogSourceNewerDest    = '#039'Source file "%s" (%s) is newer than de'+
  'stination "%s" (%s).'#039';'#010+
  #010+
  '  // Help messages for usage'#010+
  '  SValue         ','     = '#039'Value'#039';'#010+
  '  SHelpUsage          = '#039'Usage: %s command [options]'#039';'#010+
  '  SHelpCommand        = '#039'Where command is one of the following:'#039+
  ';'#010+
  '  SHelpCompile        = '#039'Compile all units in the package(s).'#039';'+
  #010+
  '  SHelpBuild          = '#039'Build all unit','s in the package(s).'#039';'+
  #010+
  '  SHelpInstall        = '#039'Install all units in the package(s).'#039';'+
  #010+
  '  SHelpClean          = '#039'Clean (remove) all units in the package(s'+
  ').'#039';'#010+
  '  SHelpArchive        = '#039'Create archive (zip) with all units in th'+
  'e package(s).'#039';'#010,
  '  SHelpHelp           = '#039'This message.'#039';'#010+
  '  SHelpManifest       = '#039'Create a manifest suitable for import in '+
  'repository.'#039';'#010+
  '  SHelpListSources    = '#039'Create a list of sources in the package.'#039+
  ';'#010+
  '  SHelpCmdOptions     = '#039'Where options is one or mo','re of the fol'+
  'lowing:'#039';'#010+
  '  SHelpCPU            = '#039'Compile for indicated CPU.'#039';'#010+
  '  SHelpOS             = '#039'Compile for indicated OS'#039';'#010+
  '  SHelpTarget         = '#039'Compile for indicated target'#039';'#010+
  '  SHelpList           = '#039'list commands instead of actua','lly execu'+
  'ting them.'#039';'#010+
  '  SHelpPrefix         = '#039'Use indicated prefix directory for all co'+
  'mmands.'#039';'#010+
  '  SHelpNoDefaults     = '#039'Do not use defaults when compiling.'#039';'+
  #010+
  '  SHelpBaseInstallDir = '#039'Use indicated directory as base install d'+
  'ir.'#039';'#010+
  '  SHelp','Compiler       = '#039'Use indicated binary as compiler'#039';'#010+
  '  SHelpConfig         = '#039'Use indicated config file when compiling.'+
  #039';'#010+
  '  SHelpVerbose        = '#039'Be verbose when working.'#039';'#010+
  #010+
  #010+
  'Const'#010+
  '  // Keys for Defaults file. Do not localize.'#010+
  '  KeyCompile','r = '#039'Compiler'#039';'#010+
  '  KeyArchive  = '#039'Archive'#039';'#010+
  '  KeyCopy     = '#039'Copy'#039';'#010+
  '  KeyMkDir    = '#039'MkDir'#039';'#010+
  '  KeyMove     = '#039'Move'#039';'#010+
  '  KeyRemove   = '#039'Remove'#039';'#010+
  '  KeyOptions  = '#039'Options'#039';'#010+
  '  KeyCPU      = '#039'CPU'#039';'#010+
  '  KeyOS       = '#039'OS'#039';'#010+
  '  KeyMode     = '#039'Mode'#039';'#010+
  '  ','KeyPrefix   = '#039'Prefix'#039';'#010+
  '  KeyTarget   = '#039'Target'#039';'#010+
  '  KeyBaseInstallDir     = '#039'BaseInstallDir'#039';'#010+
  '  KeyUnitInstallDir     = '#039'UnitInstallDir'#039';'#010+
  '  KeyBinInstallDir      = '#039'BinInstallDir'#039';'#010+
  '  KeyDocInstallDir      = '#039'DocInstallDir'#039';'#010+
  '  KeyExamplesIns','tallDir = '#039'ExamplesInstallDir'#039';'#010+
  #010+
  '// Callback for Sysutils getapplicationname.'#010+
  #010+
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
  '  Result:=OSToString(Defaults.OS);'#010+
  'end;'#010+
  #010+
  'Function CurrentC','PU : String;'#010+
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
  'Function OSesToString(OSes: TOSes) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=LowerC','ase(SetToString(PtypeInfo(TypeInfo(TOSes)),Integer(O'+
  'Ses),False));'#010+
  'end;'#010+
  #010+
  'Function CPUToString(CPU: TCPU) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=LowerCase(GetenumName(TypeInfo(TCPU),Ord(CPU)));'#010+
  'end;'#010+
  #010+
  'Function CPUSToString(CPUS: TCPUS) : String;'#010+
  #010+
  'begin'#010+
  '  ','Result:=LowerCase(SetToString(PTypeInfo(TypeInfo(TCPUS)),Integer(C'+
  'PUS),False));'#010+
  'end;'#010+
  #010+
  'Function StringToOS(S : String) : TOS;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=GetEnumValue(TypeInfo(TOS),S);'#010+
  '  if (I=-1) then'#010+
  '    Raise EInstallerError.CreateFmt','(SErrInvalidOS,[S]);'#010+
  '  Result:=TOS(I);'#010+
  'end;'#010+
  #010+
  #010+
  'Function OSesToString(S : String) : TOSes;'#010+
  #010+
  'begin'#010+
  '  Result:=TOSes(StringToSet(PTypeInfo(TypeInfo(TOSes)),S));'#010+
  'end;'#010+
  #010+
  'Function StringToCPU(S : String) : TCPU;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=GetEn','umValue(TypeInfo(TCPU),S);'#010+
  '  if (I=-1) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidCPU,[S]);'#010+
  '  Result:=TCPU(I);'#010+
  'end;'#010+
  #010+
  'Function StringToCPUS(S : String) : TCPUS;'#010+
  #010+
  'begin'#010+
  '  Result:=TCPUS(StringToSet(PTypeInfo(TypeInfo(TCPUS)),S));'#010+
  'end;',#010+
  #010+
  'Function ModeToString(Mode: TCompilerMode) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=LowerCase(GetenumName(TypeInfo(TCompilerMode),Ord(Mode)));'#010+
  'end;'#010+
  #010+
  'Function StringToMode(S : String) : TCompilerMode;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=GetEnumValue(TypeInf','o(TCompilerMode),S);'#010+
  '  if (I=-1) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidMode,[S]);'#010+
  '  Result:=TCompilerMode(I);'#010+
  'end;'#010+
  #010+
  #010+
  'Function MakeTargetString(CPU : TCPU;OS: TOS) : String;'#010+
  #010+
  'begin'#010+
  '  Result:=CPUToString(CPU)+'#039'-'#039'+OSToString(OS);',#010+
  'end;'#010+
  #010+
  'Procedure StringToCPUOS(S : String; Var CPU : TCPU; Var OS: TOS);'#010+
  #010+
  'Var'#010+
  '  P : integer;'#010+
  #010+
  'begin'#010+
  '  P:=Pos('#039'-'#039',S);'#010+
  '  If (P=0) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrInvalidTarget,[S]);'#010+
  '  CPU:=StringToCPU(Copy(S,1,P-1));'#010+
  '  OS:=String','ToOs(Copy(S,P+1,Length(S)-P));'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure ResolveDependencies(L : TStrings; P : TNamedCollection);'#010+
  #010+
  'Var'#010+
  '  I,J : Integer;'#010+
  #010+
  'begin'#010+
  '  If Assigned(L) then'#010+
  '    For I:=0 to L.Count-1 do'#010+
  '      begin'#010+
  '      J:=P.IndexOfName(L[i]);'#010+
  '      If J<>-','1 then'#010+
  '        L.Objects[I]:=P.Items[J];'#010+
  '      end;'#010+
  'end;'#010+
  #010+
  'Function AddStrings(Dest,Src : TStrings) : Integer ;'#010+
  'begin'#010+
  '  Result:=AddStrings(Dest,Src,'#039#039');'#010+
  'end;'#010+
  #010+
  'Procedure AddStrings(Var S : String; L : TStrings; Prefix : String);'#010+
  'Var'#010+
  '  I : Int','eger;'#010+
  'begin'#010+
  '  For I:=0 to L.Count-1 do'#010+
  '    begin'#010+
  '    if (S<>'#039#039') then'#010+
  '      S:=S+'#039' '#039';'#010+
  '    S:=S+Prefix+L[i];'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function AddStrings(Dest, Src : TStrings; Const APrefix : String) : In'+
  'teger ;'#010+
  'begin'#010+
  '  Result:=0;'#010+
  '  While (Result<Src.C','ount) do'#010+
  '    begin'#010+
  '    If (APrefix<>'#039#039') then'#010+
  '      Dest.Add(APrefix+Src[Result]) // Not sure whether '#039#039' is op'+
  'timized away.'#010+
  '    else'#010+
  '      Dest.Add(Src[Result]);'#010+
  '    Inc(Result);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function FileListToString(List : TStrings; Pref','ix : String) : String'+
  ';'#010+
  #010+
  'Var'#010+
  '  I : integer;'#010+
  '  S : String;'#010+
  #010+
  'begin'#010+
  '  Result:='#039#039';'#010+
  '  For I:=0 to List.Count-1 do'#010+
  '    begin'#010+
  '    If (I>0) then'#010+
  '      Result:=Result+'#039' '#039';'#010+
  '    S:=Prefix+List[i];'#010+
  '    If (Pos('#039' '#039',S)<>0) then'#010+
  '      S:='#039'"'#039'+S+'#039'"'#039';'#010+
  '    Res','ult:=Result+S;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function FixPath (APath : String) : String;'#010+
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
  '      If P^ in ['#039'/'#039','#039'\'#039'] then'#010+
  '     ','   P^:=PathDelim;'#010+
  '      Inc(P);'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure ChangeDir(APath : String);'#010+
  #010+
  'begin'#010+
  '  if Not SetCurrentDir(APath) then'#010+
  '    Raise EInstallerError.CreateFmt(SErrChangeDirFailed,[APath]);'#010+
  'end;'#010+
  #010+
  'procedure SplitCommand(const Cm','d : String; var Exe, Options : String'+
  ');'#010+
  #010+
  'Const'#010+
  '  WhiteSpace = [#9,#10,#13,'#039' '#039'];'#010+
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
  #010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  'function GetCompilerInfo(const ACompiler,AOptions:string):string;'#010+
  'const'#010+
  '  BufSize = 1024;'#010+
  'var'#010+
  '  S: TProcess',';'#010+
  '  Buf: array [0..BufSize - 1] of char;'#010+
  '  Count: longint;'#010+
  'begin'#010+
  '  S:=TProcess.Create(Nil);'#010+
  '  S.Commandline:=ACompiler+'#039' '#039'+AOptions;'#010+
  '  S.Options:=[poUsePipes];'#010+
  '  S.execute;'#010+
  '  Count:=s.output.read(buf,BufSize);'#010+
  '  S.Free;'#010+
  '  SetLength(Result,C','ount);'#010+
  '  Move(Buf,Result[1],Count);'#010+
  'end;'#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  #010+
  #010+
  '{ TNamedItem }'#010+
  #010+
  'procedure TNamedItem.SetName(const AValue: String);'#010+
  #010+
  'begin'#010+
  '  if FName=AValue then exit;'#010+
  '  With TNamedCollection(Collection) do'#010+
  '    If UniqueNames then'#010+
  '    ','  If (IndexOfName(AVAlue)<>-1) then'#010+
  '        Raise ECollectionError.CreateFmt(SErrNameExists,[AValue]);'#010+
  '  FName:=AValue;'#010+
  'end;'#010+
  #010+
  '{ TNamedCollection }'#010+
  #010+
  'function TNamedCollection.IndexOfName(AName: String): Integer;'#010+
  #010+
  'begin'#010+
  '  Result:=Count-1;'#010+
  '  W','hile (Result>=0) and (CompareText(TNamedItem(Items[Result]).FName'+
  ',AName)<>0) do'#010+
  '    Dec(Result);'#010+
  'end;'#010+
  #010+
  'function TNamedCollection.ItemByName(AName: String): TNamedItem;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=IndexOfName(AName);'#010+
  '  If (I=-1) Then'#010+
  '   ',' Raise ECollectionError.CreateFmt(SErrNoSuchName,[AName]);'#010+
  '  Result:=TNamedItem(Items[i]);'#010+
  'end;'#010+
  #010+
  #010+
  '{ TTargets }'#010+
  #010+
  'function TTargets.GetTargetItem(Index : Integer): TTarget;'#010+
  'begin'#010+
  '  Result:=TTarget(Items[Index]);'#010+
  'end;'#010+
  #010+
  'function TTargets.GetTar','get(AName : String): TTarget;'#010+
  'begin'#010+
  '  Result:=TTarget(ItemByName(AName));'#010+
  'end;'#010+
  #010+
  'procedure TTargets.SetDefaultDir(const AValue: String);'#010+
  'begin'#010+
  '  If (AValue<>'#039#039') then'#010+
  '    FDefaultDir:=IncludeTrailingPathDelimiter(AValue)'#010+
  '  else'#010+
  '    FDefaultDi','r:='#039#039';'#010+
  'end;'#010+
  #010+
  'procedure TTargets.SetTargetItem(Index : Integer; const AValue: TTarge'+
  't);'#010+
  #010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  'procedure TTargets.ApplyDefaults(ATarget: TTarget);'#010+
  'begin'#010+
  '  If (ATarget.Directory='#039#039') then'#010+
  '    ATarget.Directory:=FD','efaultDir;'#010+
  '   ATarget.OS:=FDefaultOS;'#010+
  '   ATarget.CPU:=FDefaultCPU;'#010+
  'end;'#010+
  #010+
  'procedure TTargets.ResetDefaults;'#010+
  'begin'#010+
  '  FDefaultDir:='#039#039';'#010+
  '  FDefaultOS:=[];'#010+
  '  FDefaultCPU:=[];'#010+
  'end;'#010+
  #010+
  'Function TTargets.AddUnit(AUnitName: String) : TTarget;'#010+
  #010+
  #010+
  'begin'#010+
  ' ',' Result:=Add as TTarget;'#010+
  '  Result.Name:=AUnitName;'#010+
  '  Result.TargetType:=TTUnit;'#010+
  '  ApplyDefaults(Result);'#010+
  'end;'#010+
  #010+
  'Function TTargets.AddProgram(AProgramName: String) : TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AProgramName;'#010+
  '  Resu','lt.TargetType:=ttProgram;'#010+
  '  ApplyDefaults(Result);'#010+
  'end;'#010+
  #010+
  'Function TTargets.AddExampleUnit(AUnitName: String): TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AUnitName;'#010+
  '  Result.TargetType:=ttExampleUnit;'#010+
  '  ApplyDefaults(Result);'#010+
  'en','d;'#010+
  #010+
  'Function TTargets.AddExampleProgram(AProgramName: String): TTarget;'#010+
  'begin'#010+
  '  Result:=Add as TTarget;'#010+
  '  Result.Name:=AProgramName;'#010+
  '  Result.TargetType:=ttExampleProgram;'#010+
  '  ApplyDefaults(Result);'#010+
  'end;'#010+
  #010+
  #010+
  '{ TSources }'#010+
  #010+
  'function TSources.GetS','ourceItem(Index : Integer): TSource;'#010+
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
  'function TSources.AddDocFiles(AFiles : String)',' : TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.SourceType:=stDoc;'#010+
  'end;'#010+
  #010+
  #010+
  'function TSources.AddSrcFiles(AFiles : String) : TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.SourceType:=stSr','c;'#010+
  'end;'#010+
  #010+
  'function TSources.AddExampleFiles(AFiles : String) : TSource;'#010+
  'begin'#010+
  '  Result:=Add as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.SourceType:=stExample;'#010+
  'end;'#010+
  #010+
  'function TSources.AddTestFiles(AFiles : String) : TSource;'#010+
  'begin'#010+
  '  Result:=A','dd as TSource;'#010+
  '  Result.Name:=AFiles;'#010+
  '  Result.SourceType:=stTest;'#010+
  'end;'#010+
  #010+
  '{ TNamedItemList }'#010+
  #010+
  'function TNamedItemList.GetNamedItem(Index : Integer): TNamedItem;'#010+
  'begin'#010+
  '  Result:=TNamedItem(Items[Index]);'#010+
  'end;'#010+
  #010+
  'procedure TNamedItemList.SetName','dItem(Index : Integer; const AValue:'+
  ' TNamedItem);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  'function TNamedItemList.IndexOfName(AName: String): Integer;'#010+
  #010+
  'begin'#010+
  '  Result:=Count-1;'#010+
  '  While (Result>=0) and (CompareText(GetNamedItem(Result).Name,ANam','e'+
  ')<>0) do'#010+
  '    Dec(Result);'#010+
  'end;'#010+
  #010+
  'function TNamedItemList.ItemByName(ANAme: String): TNamedItem;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  I:=IndexOfName(AName);'#010+
  '  If (I=-1) Then'#010+
  '    Raise ECollectionError.CreateFmt(SErrNoSuchName,[AName]);'#010+
  '  Result:=TNa','medItem(Items[i]);'#010+
  'end;'#010+
  #010+
  '{ TCustomDefaults }'#010+
  #010+
  'procedure TCustomDefaults.SetCPU(const AValue: TCPU);'#010+
  'begin'#010+
  '  FCPU:=AValue;'#010+
  '  RecalcTarget;'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetBaseInstallDir: String;'#010+
  'begin'#010+
  '  If (FBaseInstallDir<>'#039#039') then'#010+
  '    Re','sult:=FBaseInstallDir'#010+
  '  else'#010+
  '    if UnixPaths then'#010+
  '      Result:=Prefix +'#039'lib'#039' + PathDelim + '#039'fpc'#039#010+
  '    else'#010+
  '      Result:=Prefix;'#010+
  #010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetBinInstallDir: String;'#010+
  'begin'#010+
  '  If (FBinInstallDir<>'#039#039') then'#010+
  '    Result:=FBi','nInstallDir'#010+
  '  else'#010+
  '    If UnixPaths then'#010+
  '      Result:=BaseInstallDir+'#039'bin'#039#010+
  '    else'#010+
  '      Result:=BaseInstallDir+'#039'bin'#039';'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetCompiler: String;'#010+
  'begin'#010+
  '  If (FCompiler<>'#039#039') then'#010+
  '    Result:=FCompiler'#010+
  '  else'#010+
  '    Re','sult:='#039'fpc'#039';'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetDocInstallDir: String;'#010+
  'begin'#010+
  '  If (FBinInstallDir<>'#039#039') then'#010+
  '    Result:=FBinInstallDir'#010+
  '  else'#010+
  '    If UnixPaths then'#010+
  '      Result:=Prefix +'#039'share'#039'+PathDelim+'#039'docs'#039#010+
  '    else'#010+
  '      Result:=BaseIns','tallDir+'#039'docs'#039';'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetExamplesInstallDir: String;'#010+
  'begin'#010+
  '  If (FExamplesInstallDir<>'#039#039') then'#010+
  '    Result:=FExamplesInstallDir'#010+
  '  else'#010+
  '    If UnixPaths then'#010+
  '      Result:=Prefix +'#039'share'#039'+PathDelim+'#039'docs'#039'+PathDel'+
  'im+'#039'e','xamples'#039#010+
  '    else'#010+
  '      Result:=BaseInstallDir+'#039'examples'#039';'#010+
  'end;'#010+
  #010+
  'function TCustomDefaults.GetUnitInstallDir: String;'#010+
  'begin'#010+
  '  If (FUnitInstallDir<>'#039#039') then'#010+
  '    Result:=FUnitInstallDir'#010+
  '  else'#010+
  '    If UnixPaths then'#010+
  '      Result:=BaseInstallDir','+'#039'units'#039'+PathDelim+Target'#010+
  '    else'#010+
  '      Result:=BaseInstallDir+'#039'units'#039'+PathDelim+Target;'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.SetBaseInstallDir(const AValue: String);'#010+
  'begin'#010+
  '  FBaseInstallDir:=IncludeTrailingPathDelimiter(AValue);'#010+
  '  UnitInstallD','ir:='#039#039';'#010+
  '  BinInstallDir:='#039#039';'#010+
  '  ExamplesInstallDir:='#039#039';'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.SetOS(const AValue: TOS);'#010+
  'begin'#010+
  '  FOS:=AValue;'#010+
  '  Recalctarget;'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.SetPrefix(const AValue: String);'#010+
  'begin'#010+
  '  if FPrefix=AValue',' then exit;'#010+
  '  FPrefix:=IncludeTrailingPathDelimiter(AValue);'#010+
  '  BaseInstallDir:='#039#039';'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.SetTarget(const AValue: String);'#010+
  #010+
  'Var'#010+
  '  P : Integer;'#010+
  #010+
  'begin'#010+
  '  if FTarget<>AValue then'#010+
  '    begin'#010+
  '    P:=Pos('#039'-'#039',AValue);'#010+
  '    I','f (P<>0) then'#010+
  '      begin'#010+
  '      FOS:=StringToOS(System.Copy(Avalue,P+1,Length(AValue)-P));'#010+
  '      FCPU:=StringToCPU(System.Copy(Avalue,1,P-1));'#010+
  '      end'#010+
  '    else'#010+
  '      FOS:=StringToOS(AValue);'#010+
  '    FTarget:=AValue;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure T','CustomDefaults.RecalcTarget;'#010+
  'begin'#010+
  '  Ftarget:=CPUToString(FCPU)+'#039'-'#039'+OStoString(FOS);'#010+
  'end;'#010+
  #010+
  'constructor TCustomDefaults.Create;'#010+
  'begin'#010+
  '  InitDefaults;'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.InitDefaults;'#010+
  'begin'#010+
  '  {$ifdef unix}'#010+
  '  UnixPaths:=True;'#010+
  '  {$','else}'#010+
  '  UnixPaths:=False;'#010+
  '  {$endif}'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.Assign(ASource: TPersistent);'#010+
  #010+
  'Var'#010+
  '  d : TCustomDefaults;'#010+
  #010+
  'begin'#010+
  '  If ASource is TCustomDefaults then'#010+
  '    begin'#010+
  '    D:=ASource as TCustomDefaults;'#010+
  '    FArchive:=D.Farchive',';'#010+
  '    FCompiler:=D.Compiler;'#010+
  '    FCopy:=D.FCopy;'#010+
  '    FCPU:=D.FCPU;'#010+
  '    FMode:=D.FMode;'#010+
  '    FMkDir:=D.FMkDir;'#010+
  '    FMove:=D.FMove;'#010+
  '    FOptions:=D.FOptions;'#010+
  '    FOS:=D.FOS;'#010+
  '    FPrefix:=D.FPrefix;'#010+
  '    FBaseInstallDir:=D.FBaseInstallDir;'#010+
  '    F','UnitInstallDir:=D.FUnitInstallDir;'#010+
  '    FBinInstallDir:=D.FBinInstallDir;'#010+
  '    FDocInstallDir:=D.FDocInstallDir;'#010+
  '    FExamplesInstallDir:=D.FExamplesInstallDir;'#010+
  '    FRemove:=D.FRemove;'#010+
  '    FTarget:=D.FTarget;'#010+
  '    FUnixPaths:=D.FUnixPaths;'#010+
  '   ',' end;'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.LocalInit(Const AFileName : String);'#010+
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
  '    If (FN<>'#039#039') the','n'#010+
  '      If not FileExists(FN) then'#010+
  '        FN:='#039#039';'#010+
  '    // User config file fpmake.cfg'#010+
  '    If (FN='#039#039') then'#010+
  '      begin'#010+
  '      FN:=GetAppConfigFile(False);'#010+
  '      If Not FileExists(FN) then'#010+
  '        FN:='#039#039';'#010+
  '      end;'#010+
  '    // Global config file f','pmake.cfg'#010+
  '    If (FN='#039#039') then'#010+
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
  'procedure TCustomDefaults.Co','mpilerDefaults;'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  'var'#010+
  '  infoSL : TStringList;'#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  'begin'#010+
  '{$ifdef HAS_UNIT_PROCESS}'#010+
  '  // Detect compiler version/target from -i option'#010+
  '  infosl:=TStringList.Create;'#010+
  '  infosl.Delimiter:='#039' '#039';'#010+
  '  info','sl.DelimitedText:=GetCompilerInfo(FCompiler,'#039'-iVTPTO'#039')'+
  ';'#010+
  '  if infosl.Count<>3 then'#010+
  '    Raise EInstallerError.Create(SErrInvalidFPCInfo);'#010+
  '  FCompilerVersion:=infosl[0];'#010+
  '  CPU:=StringToCPU(infosl[1]);'#010+
  '  OS:=StringToOS(infosl[2]);'#010+
  '{$else HAS_UN','IT_PROCESS}'#010+
  '  if CPU=cpuNone then'#010+
  '    CPU:=StringToCPU({$I %FPCTARGETCPU%});'#010+
  '  if OS=osNone then'#010+
  '    OS:=StringToOS({$I %FPCTARGETOS%});'#010+
  '  if FCompilerVersion='#039#039' then'#010+
  '    FCompilerVersion:={$I %FPCVERSION%};'#010+
  '{$endif HAS_UNIT_PROCESS}'#010+
  'end;'#010+
  #010,
  'procedure TCustomDefaults.LoadFromFile(Const AFileName: String);'#010+
  #010+
  'Var'#010+
  '  F : TFileStream;'#010+
  #010+
  'begin'#010+
  '  F:=TFileStream.Create(AFileName,fmOpenRead);'#010+
  '  Try'#010+
  '    LoadFromStream(F);'#010+
  '  Finally'#010+
  '    F.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.SaveToF','ile(Const AFileName: String);'#010+
  #010+
  'Var'#010+
  '  F : TFileStream;'#010+
  #010+
  'begin'#010+
  '  F:=TFileStream.Create(AFileName,fmCreate);'#010+
  '  Try'#010+
  '    SaveToStream(F);'#010+
  '  Finally'#010+
  '    F.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.SaveToStream(S : TStream);'#010+
  #010+
  'Var'#010+
  '  L : TStringL','ist;'#010+
  #010+
  'begin'#010+
  '  L:=TStringList.Create;'#010+
  '  try'#010+
  '    With L do'#010+
  '      begin'#010+
  '      Values[KeyArchive]:=FArchive;'#010+
  '      Values[KeyCompiler]:=FCompiler;'#010+
  '      Values[KeyCopy]:=FCopy;'#010+
  '      Values[KeyMkDir]:=FMkDir;'#010+
  '      Values[KeyMove]:=FMove;'#010+
  '     ',' Values[KeyOptions]:=FOptions;'#010+
  '      Values[KeyCPU]:=CPUToString(FCPU);'#010+
  '      Values[KeyOS]:=OSToString(FOS);'#010+
  '      Values[KeyMode]:=ModeToString(FMode);'#010+
  '      Values[KeyPrefix]:=FPrefix;'#010+
  '      Values[KeyBaseInstallDir]:=FBaseInstallDir;'#010+
  '  ','    Values[KeyUnitInstallDir]:=FUnitInstallDir;'#010+
  '      Values[KeyBinInstallDir]:=FBinInstallDir;'#010+
  '      Values[KeyDocInstallDir]:=FDocInstallDir;'#010+
  '      Values[KeyExamplesInstallDir]:=FExamplesInstallDir;'#010+
  '      Values[KeyRemove]:=FRemove;'#010+
  '    ','  Values[KeyTarget]:=FTarget;'#010+
  '      end;'#010+
  '    L.SaveToStream(S);'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TCustomDefaults.LoadFromStream(S: TStream);'#010+
  #010+
  'Var'#010+
  '  L : TStrings;'#010+
  '  Line : String;'#010+
  '  I,P,PC : Integer;'#010+
  #010+
  #010+
  'begin'#010+
  '  L:=TStringList.Crea','te;'#010+
  '  Try'#010+
  '    L.LoadFromStream(S);'#010+
  '    // Fix lines.'#010+
  '    For I:=L.Count-1 downto 0 do'#010+
  '      begin'#010+
  '      Line:=L[I];'#010+
  '      P:=Pos('#039'='#039',Line);'#010+
  '      PC:=Pos('#039';'#039',Line);  // Comment line.'#010+
  '      If (P=0) or ((PC<>0) and (PC<P)) then'#010+
  '        L.Del','ete(I)'#010+
  '      else'#010+
  '        L[i]:=Trim(System.Copy(Line,1,P-1)+'#039'='#039'+Trim(System.Cop'+
  'y(Line,P+1,Length(Line)-P)));'#010+
  '      end;'#010+
  '    With L do'#010+
  '      begin'#010+
  '      FArchive:=Values[KeyArchive];'#010+
  '      FCompiler:=Values[KeyCompiler];'#010+
  '      FCopy:=Values','[KeyCopy];'#010+
  '      FMkDir:=Values[KeyMkDir];'#010+
  '      FMove:=Values[KeyMove];'#010+
  '      FRemove:=Values[KeyRemove];'#010+
  '      FOptions:=Values[KeyOptions];'#010+
  '      Line:=Values[KeyCPU];'#010+
  '      If (Line<>'#039#039') then'#010+
  '        FCPU:=StringToCPU(Line);'#010+
  '      Line:','=Values[KeyOS];'#010+
  '      If (Line<>'#039#039') then'#010+
  '        FOS:=StringToOS(Line);'#010+
  '      Line:=Values[KeyMode];'#010+
  '      If (Line<>'#039#039') then'#010+
  '        FMode:=StringToMode(Line);'#010+
  '      FTarget:=Values[KeyTarget];'#010+
  '      FPrefix:=Values[KeyPrefix];'#010+
  '      FBase','InstallDir:=Values[KeyBaseInstallDir];'#010+
  '      FUnitInstallDir:=Values[KeyUnitInstallDir];'#010+
  '      FBinInstallDir:=Values[KeyBinInstallDir];'#010+
  '      FDocInstallDir:=Values[KeyDocInstallDir];'#010+
  '      FExamplesInstallDir:=Values[KeyExamplesInstallDir','];'#010+
  '      end;'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  '{ TFPCDefaults }'#010+
  #010+
  'procedure TFPCDefaults.CompilerDefaults;'#010+
  'begin'#010+
  '  inherited CompilerDefaults;'#010+
  #010+
  '  if (FBaseInstallDir='#039#039') and (FPrefix='#039#039') then'#010+
  '    begin'#010+
  '      // Use the same algorithm as th','e compiler, see options.pas'#010+
  '{$ifdef Unix}'#010+
  '      FBaseInstallDir:=FixPath(GetEnvironmentVariable('#039'FPCDIR'#039')'+
  ');'#010+
  '      if FBaseInstallDir='#039#039' then'#010+
  '        begin'#010+
  '          FBaseInstallDir:='#039'/usr/local/lib/fpc/'#039'+FCompilerVers'+
  'ion;'#010+
  '          if not D','irectoryExists(FBaseInstallDir) and'#010+
  '             DirectoryExists('#039'/usr/lib/fpc/'#039'+FCompilerVersion)'+
  ' then'#010+
  '            FBaseInstallDir:='#039'/usr/lib/fpc/'#039'+FCompilerVersion;'+
  #010+
  '        end;'#010+
  '{$else unix}'#010+
  '      FBaseInstallDir:=FixPath(GetEnvironmentVa','riable('#039'FPCDIR'#039+
  '));'#010+
  '      if FBaseInstallDir='#039#039' then'#010+
  '        begin'#010+
  '          FBaseInstallDir:=ExtractFilePath(FCompiler)+'#039'..'#039';'#010+
  '          if not(DirectoryExists(FBaseInstallDir+'#039'/units'#039')) an'+
  'd'#010+
  '             not(DirectoryExists(FBaseInstallDir+'#039,'/rtl'#039')) the'+
  'n'#010+
  '            FBaseInstallDir:=FBaseInstallDir+'#039'..'#039';'#010+
  '        end;'#010+
  '{$endif unix}'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  '{ TPackage }'#010+
  #010+
  'function TPackage.GetHasStrings(AIndex: integer): Boolean;'#010+
  'begin'#010+
  '  Result:=False;'#010+
  '  Case AIndex Of'#010+
  '    0 : Result:=FUnit','Path<>Nil;'#010+
  '    1 : Result:=FObjectPath<>Nil;'#010+
  '    2 : Result:=FIncludePath<>Nil;'#010+
  '    3 : Result:=FDependencies<>Nil;'#010+
  '    4 : Result:=FInstallFiles<>Nil;'#010+
  '    5 : Result:=FCleanFiles<>Nil;'#010+
  '    6 : Result:=FArchiveFiles<>Nil;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'funct','ion TPackage.GetCommands: TCommands;'#010+
  'begin'#010+
  '  If Not Assigned(FCommands) then'#010+
  '    FCommands:=TCommands.Create(TCommand);'#010+
  '  Result:=FCommands;'#010+
  'end;'#010+
  #010+
  'function TPackage.GetHasCommands: Boolean;'#010+
  'begin'#010+
  '  Result:=Assigned(FCommands);'#010+
  'end;'#010+
  #010+
  'Procedu','re SplitVersion(AValue: String; Var Release,Major,Minor : Wor'+
  'd; Var Suffix : String);'#010+
  #010+
  '  Function NextDigit(sep : Char; var V : string) : integer;'#010+
  #010+
  '  Var'#010+
  '    P : Integer;'#010+
  #010+
  '  begin'#010+
  '    P:=Pos(Sep,V);'#010+
  '    If (P=0) then'#010+
  '      P:=Length(V)+1;'#010+
  ' ','   Result:=StrToIntDef(Copy(V,1,P-1),-1);'#010+
  '    If Result<>-1 then'#010+
  '      Delete(V,1,P)'#010+
  '    else'#010+
  '      Result:=0;'#010+
  '  end;'#010+
  #010+
  'Var'#010+
  '  P : Integer;'#010+
  '  V : String;'#010+
  #010+
  'begin'#010+
  '  Release:=0;'#010+
  '  Major:=0;'#010+
  '  Minor:=0;'#010+
  '  Suffix:='#039#039';'#010+
  '  V:=AValue;'#010+
  '  Release:=NextD','igit('#039'.'#039',V);'#010+
  '  Major:=NextDigit('#039'.'#039',V);'#010+
  '  Minor:=NextDigit('#039'-'#039',V);'#010+
  '  P:=Pos('#039'-'#039',V);'#010+
  '  If (P<>0) then'#010+
  '    Delete(V,1,P);'#010+
  '  Suffix:=V;'#010+
  'end;'#010+
  #010+
  'Function QuoteXML(S : String) : string;'#010+
  #010+
  '  Procedure W(Var J : Integer; Var R : String; T : String);'#010,
  #010+
  '  Var'#010+
  '    I: integer;'#010+
  #010+
  '  begin'#010+
  '    If J+Length(T)>Length(R) then'#010+
  '      SetLength(R,J+Length(T));'#010+
  '    For I:=1 to Length(t) do'#010+
  '      begin'#010+
  '      R[J]:=T[i];'#010+
  '      If I<Length(T) then'#010+
  '        Inc(J);'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  'const'#010+
  '  QuotStr = '#039'&qu','ot;'#039';'#010+
  '  AmpStr = '#039'&amp;'#039';'#010+
  '  ltStr = '#039'&lt;'#039';'#010+
  '  gtStr = '#039'&gt;'#039';'#010+
  #010+
  'Var'#010+
  '  I,J : Integer;'#010+
  #010+
  'begin'#010+
  '  SetLength(Result,Length(S));'#010+
  '  J:=0;'#010+
  '  For I:=1 to Length(S) do'#010+
  '    begin'#010+
  '    Inc(J);'#010+
  '    case S[i] of'#010+
  '      '#039'"'#039': W(j,Result,QuotStr);'#010+
  '      '#039'&'#039': W','(J,Result,AmpStr);'#010+
  '      '#039'<'#039': W(J,Result,ltStr);'#010+
  '      // Escape whitespace using CharRefs to be consistent with W3 spe'+
  'c X 3.3.3'#010+
  '       #9: w(J,Result,'#039'&#x9;'#039');'#010+
  '{      #10: wrtStr('#039'&#xA;'#039');'#010+
  '       #13: wrtStr('#039'&#xD;'#039');}'#010+
  '    else'#010+
  '      Resul','t[J]:=S[i];'#010+
  '    end;'#010+
  '    If (J=Length(Result)) and (I<Length(S)) then'#010+
  '      SetLength(Result,J+Length(S)-I);'#010+
  '    end;'#010+
  '  If J<>Length(Result) then'#010+
  '    SetLength(Result,J);'#010+
  'end;'#010+
  #010+
  #010+
  #010+
  'Procedure TPackage.GetManifest(Manifest : TStrings);'#010+
  'Var'#010+
  '  S ',': String;'#010+
  '  Release,Minor,Major : Word;'#010+
  '  i : Integer;'#010+
  'begin'#010+
  '  With Manifest do'#010+
  '    begin'#010+
  '    Add(Format('#039'<package name="%s">'#039',[QuoteXml(Name)]));'#010+
  '    SplitVersion(Version,Release,Minor,Major,S);'#010+
  '    Add(Format('#039'<version release="%d" major=','"%d" minor="%d" suff'+
  'ix="%s"/>'#039',[Release,Minor,Major,QuoteXMl(S)]));'#010+
  '    Add(Format('#039'<filename>%s</filename>'#039',[QuoteXml(FileName + '+
  'ZipExt)]));'#010+
  '    Add(Format('#039'<author>%s</author>'#039',[QuoteXml(Author)]));'#010+
  '    Add(Format('#039'<license>%s</license>'#039',','[QuoteXml(License)]))'+
  ';'#010+
  '    if ExternalURL<>'#039#039' then'#010+
  '      Add(Format('#039'<externalurl>%s</externalurl>'#039',[QuoteXml(Ext'+
  'ernalURL)]));'#010+
  '    Add(Format('#039'<email>%s</email>'#039',[QuoteXMl(Email)]));'#010+
  '    S:=Description;'#010+
  '    If (S<>'#039#039') then'#010+
  '      Add(Format(',#039'<description>%s</description>'#039',[QuoteXML(S)]'+
  '));'#010+
  '    if HasDependencies then'#010+
  '      begin'#010+
  '        If (Dependencies.Count>0) then'#010+
  '          begin'#010+
  '            Add('#039'<dependencies>'#039');'#010+
  '            for I:=0 to Dependencies.Count-1 do'#010+
  '             ',' Add(Format('#039'<dependency><package packagename="%s"/'+
  '></dependency>'#039',[QuoteXML(Dependencies[i])]));'#010+
  '            Add('#039'</dependencies>'#039');'#010+
  '          end;'#010+
  '      end;'#010+
  '    Add('#039'</package>'#039');'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'function TPackage.GetStrings(AIndex: integ','er): TStrings;'#010+
  #010+
  '  Function EnsureStrings(Var S : TStrings) : TStrings;'#010+
  #010+
  '  begin'#010+
  '    If (S=Nil) then'#010+
  '      S:=TStringList.Create;'#010+
  '    Result:=S;'#010+
  '  end;'#010+
  #010+
  'begin'#010+
  '  Result:=Nil;'#010+
  '  Case AIndex Of'#010+
  '    0 : Result:=EnsureStrings(FUnitPath);'#010+
  '    1 : ','Result:=EnsureStrings(FObjectPath);'#010+
  '    2 : Result:=EnsureStrings(FIncludePath);'#010+
  '    3 : begin'#010+
  '        Result:=EnsureStrings(FDependencies);'#010+
  '        With TStringList(Result) do'#010+
  '          if (Count=0) then'#010+
  '            begin'#010+
  '            Sorte','d:=True;'#010+
  '            Duplicates:=dupError;'#010+
  '            end;'#010+
  '        end;'#010+
  '    4 : Result:=EnsureStrings(FInstallFiles);'#010+
  '    5 : Result:=EnsureStrings(FCleanFiles);'#010+
  '    6 : Result:=EnsureStrings(FArchiveFiles);'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TPackage','.SetCommands(const AValue: TCommands);'#010+
  'begin'#010+
  '  Commands.Assign(AValue);'#010+
  'end;'#010+
  #010+
  'procedure TPackage.SetStrings(AIndex: integer; const AValue: TStrings)'+
  ';'#010+
  'begin'#010+
  '  GetStrings(AIndex).Assign(AValue);'#010+
  'end;'#010+
  #010+
  'constructor TPackage.Create(ACollection: ','TCollection);'#010+
  #010+
  'Var'#010+
  '  L : TStringList;'#010+
  #010+
  'begin'#010+
  '  inherited Create(ACollection);'#010+
  '  FTargets:=TTargets.Create(TTarget);'#010+
  '  FSources:=TSources.Create(TSource);'#010+
  '  L:=TStringList.Create;'#010+
  '  FDependencies:=L;'#010+
  '  FInstallFiles:=TStringList.Create;'#010+
  '  FC','leanFiles:=TStringList.Create;'#010+
  '  FArchiveFiles:=TStringList.Create;'#010+
  'end;'#010+
  #010+
  'destructor TPackage.destroy;'#010+
  'begin'#010+
  '  FreeAndNil(FDependencies);'#010+
  '  FreeAndNil(FInstallFiles);'#010+
  '  FreeAndNil(FCleanFiles);'#010+
  '  FreeAndNil(FArchiveFiles);'#010+
  '  FreeAndNil(FInc','ludePath);'#010+
  '  FreeAndNil(FObjectPath);'#010+
  '  FreeAndNil(FUnitPath);'#010+
  '  FreeAndNil(FSources);'#010+
  '  FreeAndNil(FTargets);'#010+
  '  inherited destroy;'#010+
  'end;'#010+
  #010+
  'function TPackage.AddTarget(AName: String): TTarget;'#010+
  'begin'#010+
  '  Result:=Targets.Add as TTarget;'#010+
  '  Result.','Name:=AName;'#010+
  'end;'#010+
  #010+
  'procedure TPackage.AddDependency(AName: String);'#010+
  'begin'#010+
  '  If FDependencies.IndexOf(AName)=-1 then'#010+
  '    FDependencies.Add(AName);'#010+
  'end;'#010+
  #010+
  'procedure TPackage.AddInstallFile(AFileName: String);'#010+
  'begin'#010+
  '  FInstallFiles.add(AFileNam','e);'#010+
  'end;'#010+
  #010+
  'procedure TPackage.GetCleanFiles(List: TStrings; Const APrefixU, APref'+
  'ixB : String; AOS : TOS);'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  AddStrings(List,CleanFiles,APrefixU);'#010+
  '  For I:=0 to FTargets.Count-1 do'#010+
  '    FTargets.TargetItems[I].GetCl','eanFiles(List, APrefixU, APrefixB, '+
  'AOS);'#010+
  'end;'#010+
  #010+
  'procedure TPackage.GetInstallFiles(List: TStrings;Types : TTargetTypes'+
  ';Const APrefix, APrefixU, APrefixB: String; AOS : TOS);'#010+
  'Var'#010+
  '  I : Integer;'#010+
  '  T : TTarget;'#010+
  'begin'#010+
  '  AddStrings(List,InstallFi','les,APrefix);'#010+
  '  For I:=0 to FTargets.Count-1 do'#010+
  '    begin'#010+
  '    T:=FTargets.TargetItems[I];'#010+
  '    if (T.TargetType in Types)  then'#010+
  '      T.GetInstallFiles(List, APrefixU, APrefixB, AOS);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TPackage.GetArchiveFiles(List: ','TStrings;Const APrefix : St'+
  'ring; AOS : TOS);'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  If (OS=[]) or (AOS in OS) then'#010+
  '    begin'#010+
  '    AddStrings(List,ArchiveFiles,APrefix);'#010+
  '    For I:=0 to FTargets.Count-1 do'#010+
  '      FTargets.TargetItems[I].GetArchiveFiles(','List,APrefix,AOS);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'Function TPackage.GetDescription : string;'#010+
  #010+
  'Var'#010+
  '  FN : String;'#010+
  '  L : TStringList;'#010+
  #010+
  'begin'#010+
  '  If (FDescription<>'#039#039') then'#010+
  '    Result:=FDescription'#010+
  '  else'#010+
  '    If (FDescriptionFile<>'#039#039') then'#010+
  '      begin'#010+
  '      // ','Always relative to binary name.'#010+
  '      FN:=ExtractFilePath(ParamStr(0));'#010+
  '      FN:=FN+FDescriptionFile;'#010+
  '      If FileExists(FN) then'#010+
  '        begin'#010+
  '        L:=TStringList.Create;'#010+
  '        Try'#010+
  '          L.LoadFromFile(FN);'#010+
  '          Result:=L.T','ext;'#010+
  '        Finally'#010+
  '          L.Free;'#010+
  '        end;'#010+
  '        end;'#010+
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
  '      Result := Name + '#039'-'#039' + V','ersion'#010+
  '    else'#010+
  '      Result := Name;'#010+
  'end;'#010+
  #010+
  #010+
  '{ TPackages }'#010+
  #010+
  'function TPackages.GetPackage(AName : String): TPackage;'#010+
  'begin'#010+
  '  Result:=TPackage(ItemByName(AName))'#010+
  'end;'#010+
  #010+
  'function TPackages.GetPackageItem(AIndex : Integer): TPackage;'#010+
  'begin'#010+
  '  Re','sult:=TPackage(Items[AIndex]);'#010+
  'end;'#010+
  #010+
  'procedure TPackages.SetPackageItem(AIndex : Integer; const AValue: TPa'+
  'ckage);'#010+
  'begin'#010+
  '  Items[AIndex]:=AValue;'#010+
  'end;'#010+
  #010+
  'function TPackages.AddPackage(const AName: String): TPackage;'#010+
  'begin'#010+
  '  Result:=Add as TPa','ckage;'#010+
  '  Result.Name:=AName;'#010+
  'end;'#010+
  #010+
  #010+
  '{ TCustomInstaller }'#010+
  #010+
  'function TCustomInstaller.GetStrings(AIndex : Integer): TStrings;'#010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  Case AIndex of'#010+
  '    0:  Result:=DefaultPackage.Dependencies;'#010+
  '    1:  Result:=DefaultPa','ckage.InstallFiles;'#010+
  '    2:  Result:=DefaultPackage.CleanFiles;'#010+
  '    3:  Result:=DefaultPackage.ArchiveFiles;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'function TCustomInstaller.GetBaseInstallDir: string;'#010+
  'begin'#010+
  '  Result := Defaults.BaseInstallDir;'#010+
  'end;'#010+
  #010+
  'Function TCustomI','nstaller.GetPackageString(Index : Integer) : String'+
  ';'#010+
  'Var'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  P:=DefaultPackage;'#010+
  '  Case Index of'#010+
  '    0 : Result:=P.Author;'#010+
  '    1 : Result:=P.Directory;'#010+
  '    2 : Result:=P.License;'#010+
  '    3 : Result:=P.O','ptions;'#010+
  '    4 : Result:=P.ExternalURL;'#010+
  '    5 : Result:=P.Email;'#010+
  '    6 : Result:=P.Description;'#010+
  '    7 : Result:=P.DescriptionFile;'#010+
  '    8 : Result:=P.Version;'#010+
  '    9 : Result:=P.FileName + ZipExt;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure TCustomInstaller.SetPa','ckageString(Index : Integer; AValue '+
  ': String);'#010+
  #010+
  'Var'#010+
  '  P : TPackage;'#010+
  #010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  P:=DefaultPackage;'#010+
  '  Case Index of'#010+
  '    0 : P.Author:=AValue;'#010+
  '    1 : P.Directory:=AValue;'#010+
  '    2 : P.License:=AValue;'#010+
  '    3 : P.Options:=AVal','ue;'#010+
  '    4 : P.ExternalURL:=AValue;'#010+
  '    5 : P.Email:=AValue;'#010+
  '    6 : P.Description:=AValue;'#010+
  '    7 : P.DescriptionFile:=AValue;'#010+
  '    8 : P.Version:=AValue;'#010+
  '    9 : P.FileName:=AValue;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'function TCustomInstaller.GetOSes: TOSes;'#010+
  'beg','in'#010+
  '  CheckDefaultPackage;'#010+
  '  Result:=DefaultPackage.OS;'#010+
  'end;'#010+
  #010+
  'function TCustomInstaller.GetTargets: TTargets;'#010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  Result:=DefaultPackage.Targets;'#010+
  'end;'#010+
  #010+
  'function TCustomInstaller.GetSources: TSources;'#010+
  'begin'#010+
  '  CheckD','efaultPackage;'#010+
  '  Result:=DefaultPackage.Sources;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.SetBaseInstallDir(AValue: string);'#010+
  'begin'#010+
  '  if AValue <> Defaults.BaseInstallDir then'#010+
  '    Defaults.BaseInstallDir := AValue;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.S','etDefaultPackage(const AValue: TPackage)'+
  ';'#010+
  'begin'#010+
  '  if FDefaultPackage=AValue then exit;'#010+
  '  FDefaultPackage:=AValue;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.SetDefaults(const AValue: TCustomDefaults);'+
  #010+
  'begin'#010+
  '  FDefaults.Assign(AValue);'#010+
  'end;'#010+
  #010+
  'procedure',' TCustomInstaller.SetStrings(AIndex : Integer; const AValue'+
  ': TStrings);'#010+
  #010+
  'Var'#010+
  '  Res : TStrings;'#010+
  #010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  Case AIndex of'#010+
  '    0:  Res:=DefaultPackage.Dependencies;'#010+
  '    1:  Res:=DefaultPackage.InstallFiles;'#010+
  '    2:  Res:=D','efaultPackage.CleanFiles;'#010+
  '    3:  Res:=DefaultPackage.ArchiveFiles;'#010+
  '  end;'#010+
  '  Res.Assign(Avalue);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.SetOses(const AValue: TOSes);'#010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  DefaultPackage.OS:=AValue;'#010+
  'end;'#010+
  #010+
  'procedure TCusto','mInstaller.SearchFiles(FileName: string; Recursive: '+
  'boolean;'#010+
  '  var List: TStrings);'#010+
  #010+
  '  procedure AddRecursiveFiles(SearchDir, FileMask: string; Recursive: '+
  'boolean);'#010+
  '  var'#010+
  '    Info : TSearchRec;'#010+
  '  begin'#010+
  '    if FindFirst(SearchDir+'#039'*'#039',faAnyFi','le and faDirectory,Inf'+
  'o)=0 then'#010+
  '    begin'#010+
  '      repeat'#010+
  '          if ((Info.Attr and faDirectory) = faDirectory) and (Info.Nam'+
  'e <> '#039'.'#039') and (Info.Name <> '#039'..'#039') and (Recursive) then'+
  #010+
  '            AddRecursiveFiles(SearchDir + Info.Name + PathDe','lim, Fil'+
  'eMask, Recursive);'#010+
  #010+
  '          {$IFDEF UNIX}'#010+
  '          if ((Info.Attr and faDirectory) <> faDirectory) and IsWild(I'+
  'nfo.Name, FileMask, False) then'#010+
  '          {$ELSE}'#010+
  '          if ((Info.Attr and faDirectory) <> faDirectory) and IsWild','('+
  'Info.Name, FileMask, True) then'#010+
  '          {$ENDIF}'#010+
  '            List.Add(SearchDir + Info.Name);'#010+
  '      until FindNext(Info)<>0;'#010+
  '    end;'#010+
  '    FindClose(Info);'#010+
  '  end;'#010+
  #010+
  'var'#010+
  '  BasePath: string;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  BasePath := ExtractFilePath(','ExpandFileName(FileName));'#010+
  '  AddRecursiveFiles(BasePath, ExtractFileName(FileName), Recursive);'#010+
  #010+
  '  for i := 0 to Pred(List.Count) do'#010+
  '    List[i] := ExtractRelativepath(ExtractFilePath(ParamStr(0)), List['+
  'i]);'#010+
  #010+
  'end;'#010+
  #010+
  'procedure TCustomInstalle','r.Log(Level: TVerboseLevel; const Msg: Stri'+
  'ng);'#010+
  'begin'#010+
  '  If Level in FLogLevels then'#010+
  '    Writeln(StdErr,Msg);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.CreatePackages;'#010+
  'begin'#010+
  '  FPAckages:=TPackages.Create(TPackage);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Cr','eateBuildEngine;'#010+
  'begin'#010+
  '  FBuildEngine:=TBuildEngine.Create(Self);'#010+
  '  FBuildEngine.Defaults:=Defaults;'#010+
  '  FBuildEngine.ListMode:=FListMode;'#010+
  '  FBuildEngine.OnLog:=@Self.Log;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.CheckDefaultPackage;'#010+
  'begin'#010+
  '  If (FDef','aultPackage=Nil) then'#010+
  '    Raise EInstallerError.Create(SErrNoPackage);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Error(Msg: String);'#010+
  'begin'#010+
  '  Raise EInstallerError.Create(Msg);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Error(Fmt: String; Args: array of const)',';'+
  #010+
  'begin'#010+
  '  Raise EInstallerError.CreateFmt(Fmt,Args);'#010+
  'end;'#010+
  #010+
  'Function TCustomInstaller.StartPackage(const AName: String) : TPackage'+
  ';'#010+
  'begin'#010+
  '  FDefaultPackage:=FPackages.AddPackage(AName);'#010+
  '  Result:=FDefaultPackage;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInsta','ller.EndPackage;'#010+
  'begin'#010+
  '  FDefaultPackage:=Nil;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.AnalyzeOptions;'#010+
  #010+
  '  Function CheckOption(Index : Integer;Short,Long : String): Boolean;'#010+
  #010+
  '  var'#010+
  '    O : String;'#010+
  #010+
  '  begin'#010+
  '    O:=Paramstr(Index);'#010+
  '    Result:=(O='#039'-',#039'+short) or (O='#039'--'#039'+long) or (copy(O,'+
  '1,Length(Long)+3)=('#039'--'#039'+long+'#039'='#039'));'#010+
  '  end;'#010+
  #010+
  '  Function CheckCommand(Index : Integer;Short,Long : String): Boolean;'+
  #010+
  #010+
  '  var'#010+
  '    O : String;'#010+
  #010+
  '  begin'#010+
  '    O:=Paramstr(Index);'#010+
  '    Result:=(O='#039'-'#039'+short) or (O=','long);'#010+
  '  end;'#010+
  #010+
  '  Function OptionArg(Var Index : Integer) : String;'#010+
  #010+
  '  Var'#010+
  '    P : Integer;'#010+
  #010+
  '  begin'#010+
  '    if (Length(ParamStr(Index))>1) and (Paramstr(Index)[2]<>'#039'-'#039+
  ') then'#010+
  '      begin'#010+
  '      If Index<ParamCount then'#010+
  '        begin'#010+
  '        Inc(I','ndex);'#010+
  '        Result:=Paramstr(Index);'#010+
  '        end'#010+
  '      else'#010+
  '        Error(SErrNeedArgument,[Index,ParamStr(Index)]);'#010+
  '      end'#010+
  '    else If length(ParamStr(Index))>2 then'#010+
  '      begin'#010+
  '      P:=Pos('#039'='#039',Paramstr(Index));'#010+
  '      If (P=0) then'#010,
  '        Error(SErrNeedArgument,[Index,ParamStr(Index)])'#010+
  '      else'#010+
  '        begin'#010+
  '        Result:=Paramstr(Index);'#010+
  '        Delete(Result,1,P);'#010+
  '        end;'#010+
  '      end;'#010+
  '  end;'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  '  Nodefaults : Boolean;'#010+
  '  DefaultsFileName : st','ring;'#010+
  #010+
  'begin'#010+
  '  I:=0;'#010+
  '  NoDefaults:=False;'#010+
  '  FListMode:=False;'#010+
  '  FLogLevels := [vlInfo];'#010+
  '  While (I<ParamCount) do'#010+
  '    begin'#010+
  '    Inc(I);'#010+
  '    if CheckOption(I,'#039'v'#039','#039'verbose'#039') then'#010+
  '      begin'#010+
  '      Try'#010+
  '        FLogLevels:=TVerboseLevels(String','ToSet(PtypeInfo(TypeInfo(TV'+
  'erboseLevels)),OptionArg(I)));'#010+
  '      except'#010+
  '        FLogLevels:=AllMessages;'#010+
  '      end;'#010+
  '      end'#010+
  '    else if CheckCommand(I,'#039'm'#039','#039'compile'#039') then'#010+
  '      FRunMode:=rmCompile'#010+
  '    else if CheckCommand(I,'#039'b'#039','#039'build'#039') th','en'#010+
  '      FRunMode:=rmBuild'#010+
  '    else if CheckCommand(I,'#039'i'#039','#039'install'#039') then'#010+
  '      FRunMode:=rmInstall'#010+
  '    else if CheckCommand(I,'#039'c'#039','#039'clean'#039') then'#010+
  '      FRunMode:=rmClean'#010+
  '    else if CheckCommand(I,'#039'a'#039','#039'archive'#039') then'#010+
  '      FRunMode:=rmarchiv','e'#010+
  '    else if CheckCommand(I,'#039'M'#039','#039'manifest'#039') then'#010+
  '      FRunMode:=rmManifest'#010+
  '    else if CheckCommand(I,'#039'M'#039','#039'listsources'#039') then'#010+
  '      FRunMode:=rmListSources'#010+
  '    else if CheckOption(I,'#039'h'#039','#039'help'#039') then'#010+
  '      Usage('#039#039',[])'#010+
  '    else if Checkopt','ion(I,'#039'C'#039','#039'CPU'#039') then'#010+
  '      Defaults.CPU:=StringToCPU(OptionArg(I))'#010+
  '    else if Checkoption(I,'#039'O'#039','#039'OS'#039') then'#010+
  '      Defaults.OS:=StringToOS(OptionArg(I))'#010+
  '    else if Checkoption(I,'#039't'#039','#039'target'#039') then'#010+
  '      Defaults.Target:=OptionArg(I)'#010+
  '    el','se if CheckOption(I,'#039'l'#039','#039'list-commands'#039') then'#010+
  '      FListMode:=True'#010+
  '    else if Checkoption(I,'#039'P'#039','#039'prefix'#039') then'#010+
  '      Defaults.Prefix:=OptionArg(I)'#010+
  '    else if Checkoption(I,'#039'n'#039','#039'nodefaults'#039') then'#010+
  '      NoDefaults:=true'#010+
  '    else if CheckOp','tion(I,'#039'B'#039','#039'baseinstalldir'#039') then'+
  #010+
  '      Defaults.BaseInstallDir:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'r'#039','#039'compiler'#039') then'#010+
  '      Defaults.Compiler:=OptionArg(I)'#010+
  '    else if CheckOption(I,'#039'f'#039','#039'config'#039') then'#010+
  '      DefaultsFileName:=OptionAr','g(I)'#010+
  '    else'#010+
  '      begin'#010+
  '      Usage(SErrInValidArgument,[I,ParamStr(I)]);'#010+
  '      end;'#010+
  '    end;'#010+
  '  If Not NoDefaults then'#010+
  '    Defaults.LocalInit(DefaultsFileName);'#010+
  '  Defaults.CompilerDefaults;'#010+
  '{$ifdef debug}'#010+
  '  FLogLevels:=AllMessages;'#010+
  '{$endi','f}'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Usage(FMT: String; Args: array of const);'#010+
  #010+
  '  Procedure LogCmd(LC : String; Msg : String);'#010+
  '  begin'#010+
  '    Log(vlInfo,Format('#039' %-12s %s'#039',[LC,MSG]));'#010+
  '  end;'#010+
  #010+
  '  Procedure LogOption(C: Char; LC : String; Msg : St','ring);'#010+
  '  begin'#010+
  '    Log(vlInfo,Format('#039' -%s --%-16s %s'#039',[C,LC,MSG]));'#010+
  '  end;'#010+
  #010+
  '  Procedure LogArgOption(C: Char; LC : String; Msg : String);'#010+
  '  begin'#010+
  '    Log(vlInfo,Format('#039' -%s --%-20s %s'#039',[C,LC+'#039'='#039'+SVal'+
  'ue,MSG]));'#010+
  '  end;'#010+
  #010+
  'begin'#010+
  '  If (FMT<>'#039#039')',' then'#010+
  '    Log(vlInfo,Format(Fmt,Args));'#010+
  '  Log(vlInfo,Format(SHelpUsage,[Paramstr(0)]));'#010+
  '  Log(vlInfo,SHelpCommand);'#010+
  '  LogCmd('#039'compile'#039',SHelpCompile);'#010+
  '  LogCmd('#039'build'#039',SHelpBuild);'#010+
  '  LogCmd('#039'install'#039',SHelpInstall);'#010+
  '  LogCmd('#039'clean'#039',SHelpClea','n);'#010+
  '  LogCmd('#039'archive'#039',SHelpArchive);'#010+
  '  LogCmd('#039'manifest'#039',SHelpManifest);'#010+
  '  LogCmd('#039'listsources'#039',SHelpListSources);'#010+
  '  Log(vlInfo,SHelpCmdOptions);'#010+
  '  LogOption('#039'h'#039','#039'help'#039',SHelpHelp);'#010+
  '  LogOption('#039'l'#039','#039'list-commands'#039',SHelpList);'#010+
  '  LogOption('#039'n',#039','#039'nodefaults'#039',SHelpNoDefaults);'#010+
  '  LogOption('#039'v'#039','#039'verbose'#039',SHelpVerbose);'#010+
  '  LogArgOption('#039'C'#039','#039'CPU'#039',SHelpCPU);'#010+
  '  LogArgOption('#039'O'#039','#039'OS'#039',SHelpOS);'#010+
  '  LogArgOption('#039't'#039','#039'target'#039',SHelpTarget);'#010+
  '  LogArgOption('#039'P'#039','#039'prefix'#039',SHelpPrefix);'#010+
  '  LogArgOpti','on('#039'B'#039','#039'baseinstalldir'#039',SHelpBaseInstall'+
  'dir);'#010+
  '  LogArgOption('#039'r'#039','#039'compiler'#039',SHelpCompiler);'#010+
  '  LogArgOption('#039'f'#039','#039'config'#039',SHelpConfig);'#010+
  '  Log(vlInfo,'#039#039');'#010+
  '  If (FMT<>'#039#039') then'#010+
  '    halt(1)'#010+
  '  else'#010+
  '    halt(0);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Com','pile(Force: Boolean);'#010+
  'begin'#010+
  '  FBuildEngine.ForceCompile:=Force;'#010+
  '  FBuildEngine.Compile(FPackages);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Clean;'#010+
  'begin'#010+
  '  BuildEngine.Clean(FPackages);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Install;'#010+
  'begin'#010+
  '  BuildEngine.I','nstall(FPackages);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.Archive;'#010+
  'begin'#010+
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
  '    Log(vlCommand, Format(SCmdGenerat','ing, [ManifestFile]));'#010+
  '    L.Add('#039'<?xml version="1.0"?>'#039');'#010+
  '    BuildEngine.GetManifest(FPackages,L);'#010+
  '    L.SaveToFile(ManifestFile);'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.GetSourceFiles;'#010+
  #010+
  'Var'#010+
  '  L : TStrings;'#010+
  'begin'#010+
  '  ','L:=TStringList.Create;'#010+
  '  Try'#010+
  '    Log(vlCommand,'#039'Generating sources.xml'#039');'#010+
  '    L.Add('#039'<?xml version="1.0"?>'#039');'#010+
  '    BuildEngine.GetSourceFiles(FPackages,L);'#010+
  '    L.SaveToFile('#039'sources.xml'#039');'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'destructor TCusto','mInstaller.destroy;'#010+
  'begin'#010+
  '  FreeAndNil(FDefaults);'#010+
  '  inherited destroy;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.CheckPackages;'#010+
  #010+
  'begin'#010+
  '  If (FPackages.Count=0) then'#010+
  '    Error(SErrNoPackagesDefined);'#010+
  '  // Check for other obvious errors ?'#010+
  'end;'#010+
  #010+
  'Funct','ion TCustomInstaller.Run : Boolean;'#010+
  #010+
  'begin'#010+
  '  Result:=True;'#010+
  '  try'#010+
  '    CheckPackages;'#010+
  '    CreateBuildEngine;'#010+
  '    Case RunMode of'#010+
  '      rmCompile : Compile(False);'#010+
  '      rmBuild   : Compile(True);'#010+
  '      rmInstall : Install;'#010+
  '      rmArchive : A','rchive;'#010+
  '      rmClean    : Clean;'#010+
  '      rmManifest : Manifest;'#010+
  '      rmListSources : GetSourceFiles;'#010+
  '    end;'#010+
  '  except'#010+
  '    On E : Exception do'#010+
  '      begin'#010+
  '      Log(vlError,SErrInstaller);'#010+
  '      Log(vlError,E.Message);'#010+
  '      Result:=False;'#010,
  '      end;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'function TCustomInstaller.AddTarget(AName: String): TTarget;'#010+
  'begin'#010+
  '  CheckDefaultPackage;'#010+
  '  Result:=DefaultPackage.AddTarget(AName);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.AddDependency(AName: String);'#010+
  'begin'#010+
  '  CheckDefault','Package;'#010+
  '  DefaultPackage.AddDependency(AName);'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.AddDocFiles(AFileMask: string; Recursive: b'+
  'oolean);'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, Recursive',', List);'#010+
  #010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '    FDefaultPackage.Sources.AddDocFiles(List[i]);'#010+
  #010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.AddSrcFiles(AFileMask: string; Recursive: b'+
  'oolean);'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  L','ist := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, Recursive, List);'#010+
  #010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '    FDefaultPackage.Sources.AddSrcFiles(List[i]);'#010+
  #010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  'procedure TCustomInstaller.AddExampleFiles(AFileMask: string; Re','curs'+
  'ive: boolean);'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, Recursive, List);'#010+
  #010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '    FDefaultPackage.Sources.AddExampleFiles(List[i]);'#010+
  #010+
  '  List.Free;'#010+
  'e','nd;'#010+
  #010+
  'procedure TCustomInstaller.AddTestFiles(AFileMask: string; Recursive: '+
  'boolean);'#010+
  'var'#010+
  '  List : TStrings;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  List := TStringList.Create;'#010+
  '  SearchFiles(AFileMask, Recursive, List);'#010+
  #010+
  '  for i:= 0 to Pred(List.Count) do'#010+
  '   ',' FDefaultPackage.Sources.AddTestFiles(List[i]);'#010+
  #010+
  '  List.Free;'#010+
  'end;'#010+
  #010+
  '{ TFPCInstaller }'#010+
  #010+
  'constructor TFPCInstaller.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  FDefaults:=TFPCDefaults.Create;'#010+
  '  AnalyzeOptions;'#010+
  '  CreatePackages;'#010+
  'end;'#010+
  #010+
  '{ TBasicInstaller',' }'#010+
  #010+
  'constructor TBasicInstaller.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  FDefaults:=TBasicDefaults.Create;'#010+
  '  AnalyzeOptions;'#010+
  '  CreatePackages;'#010+
  'end;'#010+
  #010+
  '{ TBuildEngine }'#010+
  #010+
  'procedure TBuildEngine.SetDefaults(const AValue: TCustomDefaults);'#010+
  'begin'#010+
  '  FDe','faults.Assign(AValue);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.SetTargetDir(const AValue: String);'#010+
  'begin'#010+
  '  if FTargetDir=AValue then exit;'#010+
  '  FTargetDir:=AValue;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Error(Msg: String);'#010+
  'begin'#010+
  '  Raise EInstallerError.Create(Msg)',';'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Error(Fmt: String; Args: array of const);'#010+
  'begin'#010+
  '  Raise EInstallerError.CreateFmt(Fmt,Args);'#010+
  'end;'#010+
  #010+
  'constructor TBuildEngine.Create(AOwner: TComponent);'#010+
  'begin'#010+
  '  inherited Create(AOwner);'#010+
  '  FDefaults:=TCustomDef','aults.Create;'#010+
  '  // Maybe this should be the current directory ?'#010+
  '  // Or have it as a command-line option.'#010+
  '  // Would allow to put all '#039'installers'#039' in one dir and call t'+
  'hem'#010+
  '  // With --start-dir=/path/to/sources.'#010+
  '  FStartDir:=includeTrailing','PathDelimiter(GetCurrentDir);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.ExecuteCommand(Cmd: String; Args : String; Igno'+
  'reError : Boolean = False);'#010+
  #010+
  'Var'#010+
  '  E : Integer;'#010+
  #010+
  'begin'#010+
  '  Log(vlCommand,SLogExecutingCommand,[Cmd,Args]);'#010+
  '  // We should check cmd for ','spaces, and move all after first space '+
  'to args.'#010+
  '  E:=ExecuteProcess(cmd,args);'#010+
  '  If (E<>0) and (not IgnoreError) then'#010+
  '    Error(SErrExternalCommandFailed,[Cmd,E]);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.SysCopyFile(Const Src,Dest : String);'#010+
  #010+
  'Var'#010+
  '  D,','S : String;'#010+
  '  Fin,FOut : TFileStream;'#010+
  '  Count : Int64;'#010+
  '  A : Integer;'#010+
  #010+
  'begin'#010+
  '  Log(vlCommand,SLogCopyingFile,[Src,Dest]);'#010+
  '  FIn:=TFileStream.Create(Src,fmopenRead);'#010+
  '  Try'#010+
  '    D:=IncludeTrailingPathDelimiter(Dest);'#010+
  '    If DirectoryExists(D) ','then'#010+
  '      S:=D+ExtractFileName(Src)'#010+
  '    else'#010+
  '      S:=Dest;'#010+
  '    FOut:=TFileStream.Create(S,fmCreate);'#010+
  '    Try'#010+
  '      Count:=Fout.CopyFrom(FIn,0);'#010+
  '      If (Count<>Fin.Size) then'#010+
  '        Error(SErrCopyingFile,[Src,S]);'#010+
  '    Finally'#010+
  '      Free','AndNil(Fout);'#010+
  '    end;'#010+
  '    A:=FileGetDate(FIn.Handle);'#010+
  '    If (A=-1) then'#010+
  '      log(vlWarning,SWarnFailedToGetTime,[Src])'#010+
  '    else'#010+
  '      if FileSetDate(S,A)<>0 then'#010+
  '        Log(vlWarning,SWarnFailedToSetTime,[S]);'#010+
  '  finally'#010+
  '    FreeAndNil(F','in);'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.SysMoveFile(Const Src,Dest : String);'#010+
  #010+
  'Var'#010+
  '  S : String;'#010+
  #010+
  'begin'#010+
  '    If DirectoryExists(IncludeTrailingPathDelimiter(Dest)) then'#010+
  '      S:=IncludeTrailingPathDelimiter(Dest)+ExtractFileName(Src)'#010+
  '    el','se'#010+
  '      S:=Dest;'#010+
  '  If Not RenameFile(Src,S) then'#010+
  '    begin'#010+
  '    Try'#010+
  '      SysCopyFile(Src,S);'#010+
  '      SysDeleteFile(Src);'#010+
  '    Except'#010+
  '      On E : Exception Do'#010+
  '        Error(SErrMovingFile,[Src,S]);'#010+
  '    end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngi','ne.SysDeleteFile(Const AFileName : String);'#010+
  #010+
  'begin'#010+
  '  if not FileExists(AFileName) then'#010+
  '    Log(vlWarning,SWarnFileDoesNotExist,[AFileName])'#010+
  '  else If Not DeleteFile(AFileName) then'#010+
  '    Error(SErrDeletingFile,[AFileName]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure T','BuildEngine.SysArchiveFiles(List: TStrings;Const AFileNam'+
  'e: String);'#010+
  'begin'#010+
  '  If Not (Assigned(OnArchivefiles) or Assigned(ArchiveFilesProc)) then'+
  #010+
  '    Raise EInstallerError.Create(SErrNoArchiveSupport);'#010+
  '  If Assigned(ArchiveFilesProc) then'#010+
  ' ','   ArchiveFilesProc(AFileName,List)'#010+
  '  else'#010+
  '    OnArchiveFiles(AFileName,List);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Log(Level: TVerboseLevel; const Msg: String);'#010+
  'begin'#010+
  '  If Assigned(FOnLog) then'#010+
  '    FOnLog(Level,Msg);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.L','og(Level: TVerboseLevel; const Fmt: String;'#010+
  '  Args: array of const);'#010+
  'begin'#010+
  '  Log(Level,Format(Fmt,Args));'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.EnterDir(ADir: String);'#010+
  #010+
  'Var'#010+
  '  D : String;'#010+
  #010+
  'begin'#010+
  '  D:=FStartDir;'#010+
  '  D:=D+ADir;'#010+
  '  Log(vlInfo,SLogEnterDir,','[D]);'#010+
  '  If Not SetCurrentDir(D) then'#010+
  '    Error(SErrChangeDirFailed,[D]);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.CmdCopyFiles(List: TStrings; Const DestDir: Str'+
  'ing);'#010+
  #010+
  'Var'#010+
  '  Args : String;'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  CmdCreateDir(DestDir);'#010+
  '  If (Defaults.','Copy<>'#039#039') then'#010+
  '    begin'#010+
  '    Args:=FileListToString(List,'#039#039');'#010+
  '    Args:=Args+'#039' '#039'+DestDir;'#010+
  '    ExecuteCommand(Defaults.Copy,Args);'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      SysCopyFile(List[i],DestDir);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngin','e.CmdCreateDir(DestDir: String);'#010+
  #010+
  'begin'#010+
  '  If (Defaults.MkDir<>'#039#039') then'#010+
  '    ExecuteCommand(Defaults.MkDir,DestDir)'#010+
  '  else'#010+
  '    If not ForceDirectories(DestDir) then'#010+
  '      Error(SErrCreatingDirectory,[DestDir]);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Cm','dMoveFiles(List: TStrings; Const DestDir: S'+
  'tring);'#010+
  #010+
  'Var'#010+
  '  Args : String;'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  CmdCreateDir(DestDir);'#010+
  '  If (Defaults.Move<>'#039#039') then'#010+
  '    begin'#010+
  '    Args:=FileListToString(List,'#039#039');'#010+
  '    Args:=Args+'#039' '#039'+DestDir;'#010+
  '    ExecuteComm','and(Defaults.Move,Args);'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      SysMoveFile(List[i],DestDir);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.CmdDeleteFiles(List: TStrings);'#010+
  #010+
  'Var'#010+
  '  Args : String;'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  If (Defaults.Remove<>'#039#039') ','then'#010+
  '    begin'#010+
  '    Args:=FileListToString(List,'#039#039');'#010+
  '    ExecuteCommand(Defaults.Remove,Args);'#010+
  '    end'#010+
  '  else'#010+
  '    For I:=0 to List.Count-1 do'#010+
  '      SysDeleteFile(List[i]);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.CmdArchiveFiles(List: TStrings; Const Ar','chive'+
  'File: String);'#010+
  #010+
  'Var'#010+
  '  S,C,O : String;'#010+
  #010+
  'begin'#010+
  '  If (Defaults.Archive='#039#039') then'#010+
  '    SysArchiveFiles(List,ArchiveFile)'#010+
  '  else'#010+
  '    begin'#010+
  '    S:=FileListToString(List,'#039#039');'#010+
  '    SplitCommand(Defaults.Archive,C,O);'#010+
  '    If (O='#039#039') then'#010+
  '      O:=A','rchiveFile+'#039' '#039'+S'#010+
  '    else'#010+
  '      O:=Substitute(O,['#039'ARCHIVE'#039',ArchiveFile,'#039'FILESORDIRS'#039+
  ']);'#010+
  '    ExecuteCommand(C,O);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'Function TBuildEngine.FileNewer(Src,Dest : String) : Boolean;'#010+
  #010+
  'Var'#010+
  '  DS,DD : Longint;'#010+
  '  D1,D2 : TDateTime;'#010+
  #010+
  'begi','n'#010+
  '  DS:=FileAge(Src);'#010+
  '  DD:=FileAge(Dest);'#010+
  '  D1:=FileDateToDateTime(DS);'#010+
  '  D2:=FileDateToDateTime(DD);'#010+
  '  Log(vlDebug,SLogCompilingFileTimes,[Src,DateTimeToStr(D1),Dest,DateT'+
  'imeToStr(D2)]);'#010+
  '  Result:=D1>=D2;'#010+
  '  If Result then'#010+
  '    Log(vlCompar','e,SLogSourceNewerDest,[Src,DateTimeToStr(D1),Dest,Da'+
  'teTimeToStr(D2)]);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.ExecuteCommands(Commands: TCommands; At: TComma'+
  'ndAt);'#010+
  #010+
  'Var'#010+
  '  C : TCommand;'#010+
  '  I : Integer;'#010+
  '  Cmd,O : String;'#010+
  '  E : Boolean;'#010+
  #010+
  'begin'#010+
  '  For I:=0',' to Commands.Count-1 do'#010+
  '    begin'#010+
  '    C:=Commands.CommandItems[i];'#010+
  '    if (C.At=At) then'#010+
  '      begin'#010+
  '      E:=True;'#010+
  '      If (C.SourceFile<>'#039#039') and (C.DestFile<>'#039#039')  then'#010+
  '        E:=FileNewer(C.SourceFile,IncludeTrailingPathDelimiter(Dictio',
  'nary.GetValue('#039'OUTPUTDIR'#039'))+C.DestFile);'#010+
  '      If E then'#010+
  '        begin'#010+
  '        If Assigned(C.BeforeCommand) then'#010+
  '          C.BeforeCommand(C);'#010+
  '        O:=Substitute(C.Options,['#039'SOURCE'#039',C.SourceFile,'#039'DE'+
  'ST'#039',C.DestFile]);'#010+
  '        Cmd:=C.Comman','d;'#010+
  '        If (ExtractFilePath(Cmd)='#039#039') then'#010+
  '          Cmd:=FileSearch(Cmd,GetEnvironmentvariable('#039'PATH'#039'));'+
  #010+
  '        ExecuteCommand(Cmd,O,C.IgnoreResult);'#010+
  '        If Assigned(C.AfterCommand) then'#010+
  '          C.AfterCommand(C);'#010+
  '        end;'#010+
  '   ','   end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  '// Relative to startdir.'#010+
  'Function TBuildEngine.GetTargetDir(APackage : TPackage; ATarget : TTar'+
  'get; AbsolutePath : Boolean = False) : String;'#010+
  #010+
  'begin'#010+
  '  If AbsolutePath then'#010+
  '    Result:=IncludeTrailingPathDelimiter(FSta','rtDir)'#010+
  '  else'#010+
  '    Result:='#039#039';'#010+
  '  If (APackage.Directory<>'#039#039') then'#010+
  '    Result:=Result+IncludeTrailingPathDelimiter(APackage.Directory);'#010+
  '  If (ATarget.Directory<>'#039#039') then'#010+
  '    Result:=IncludeTrailingPathDelimiter(Result+ATarget.Directory);'#010+
  #010+
  'end',';'#010+
  #010+
  'Function TBuildEngine.TargetOK(Target : TTarget) : Boolean;'#010+
  #010+
  'begin'#010+
  '  Result:=(Target.TargetType in [ttUnit,ttProgram])'#010+
  '          and'#010+
  '          ((Target.CPU=[]) or (Defaults.CPU in Target.CPU))'#010+
  '          and'#010+
  '          ((Target.OS=[]) or (','Defaults.OS in Target.OS));'#010+
  '  If not Result then'#010+
  '    begin'#010+
  '    log(vldebug, SDebugTargetIsNotAUnitOrProgram);'#010+
  '    If Not ((Target.CPU=[]) or (Defaults.CPU in Target.CPU)) then'#010+
  '      Log(vldebug, Format(SDebugTargetHasWrongCPU, [CPUsToString','(Tar'+
  'get.CPU)]));'#010+
  '    if not ((Target.OS=[]) or (Defaults.OS in Target.OS)) then'#010+
  '      Log(vldebug, Format(SDebugTargetHasWrongOS, [OSesToString(Target'+
  '.OS)]));'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'Function TBuildEngine.NeedsCompile(Target: TTarget): Boolean;'#010+
  #010+
  'Var',#010+
  '  I : Integer;'#010+
  '  T : TTarget;'#010+
  '  OD,SD,SFN,OFN : String;'#010+
  #010+
  'begin'#010+
  '  Result:=False;'#010+
  '  OD:=FCurrentOutputDir;'#010+
  '  If (OD<>'#039#039') then'#010+
  '    OD:=IncludeTrailingPathDelimiter(OD);'#010+
  '  OFN:=OD+Target.GetOutPutFileName(Defaults.OS);'#010+
  #010+
  '  if Target.TargetType ','in ProgramTargets then'#010+
  '    OFN := GetBinOutputDir(FCurrentPackage, True) + PathDelim + OFN'#010+
  '  else'#010+
  '    OFN := GetUnitsOutputDir(FCurrentPackage, True) + PathDelim + OFN;'+
  #010+
  #010+
  '  SD:=Target.Directory;'#010+
  '  If (SD<>'#039#039') then'#010+
  '      SD:=IncludeTrailingPa','thDelimiter(SD);'#010+
  #010+
  '  Result:=Not FileExists(OFN);'#010+
  '  Log(vldebug, SDebugCheckingS, [OFN]);'#010+
  #010+
  '  // Check dependencies'#010+
  '  If not Result then'#010+
  '    If Target.HasDependencies then'#010+
  '      begin'#010+
  '      ResolveDependencies(Target.Dependencies,Target.Colle','ction as T'+
  'Targets);'#010+
  '      I:=0;'#010+
  '      While (Not Result) and (I<Target.Dependencies.Count) do'#010+
  '        begin'#010+
  '        T:=TTarget(Target.Dependencies.Objects[i]);'#010+
  '        If (T<>Nil) then'#010+
  '          Result:=NeedsCompile(T)'#010+
  '        else // if it',' is a filename, check dates.'#010+
  '          if FileExists(Target.Dependencies[i]) then'#010+
  '            Result:=FileNewer(Target.Dependencies[i],OFN)'#010+
  '          else if FileExists(SD+Target.Dependencies[i]) then'#010+
  '            Result:=FileNewer(SD+Target','.Dependencies[i],OFN);'#010+
  '        Inc(I)'#010+
  '        end;'#010+
  '      end;'#010+
  '  If not Result then'#010+
  '    begin'#010+
  '    SFN:=SD+Target.SourceFileName;'#010+
  '    If (ExtractFileExt(SFN)='#039#039') then'#010+
  '      if FileExists(SFN+'#039'.pp'#039') then'#010+
  '        SFN:=SFN+'#039'.pp'#039#010+
  '      else'#010+
  '     ','   SFN:=SFN+'#039'.pas'#039';'#010+
  '    Log(vldebug, SDebugCheckingSAgainstS, [OFN, SFN]);'#010+
  '    Result:=FileNewer(SFN,OFN);'#010+
  '    // here we should check file timestamps.'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  #010+
  'Function TBuildEngine.GetCompilerCommand(APackage : TPackage; Target :'+
  ' ','TTarget) : String;'#010+
  #010+
  'Var'#010+
  '  PD,TD,OD,RD : String;'#010+
  #010+
  'begin'#010+
  '  PD:=IncludeTrailingPathDelimiter(GetPackageDir(APackage,True));'#010+
  #010+
  '  OD:=IncludeTrailingPathDelimiter(GetBinOutputDir(APackage,True));'#010+
  '  RD:=ExtractRelativePath(PD,OD);'#010+
  '  Result := '#039#039';'#010,
  '  If Target.TargetType in ProgramTargets then'#010+
  '    Result:='#039'-FE'#039' + RD;'#010+
  #010+
  '  OD:=IncludeTrailingPathDelimiter(GetUnitsOutputDir(APackage,True));'#010+
  '  RD:=ExtractRelativePath(PD,OD);'#010+
  '  Result := Result + '#039' -FU'#039' + RD;'#010+
  '  If Target.Mode<>cmFPC then'#010+
  '  ','  Result:=Result+'#039' -M'#039'+ModeToString(Target.Mode)'#010+
  '  else If Defaults.Mode<>cmFPC then'#010+
  '    Result:=Result+'#039' -M'#039'+ModeToString(Defaults.Mode);'#010+
  '  If (Defaults.Options<>'#039#039') then'#010+
  '    Result:=Result+'#039' '#039'+Defaults.Options;'#010+
  '  If (APackage.Options<>'#039#039')',' then'#010+
  '    Result:=Result+'#039' '#039'+APackage.Options;'#010+
  '  If APackage.HasUnitPath then'#010+
  '    AddStrings(Result,APackage.UnitPath,'#039'-Fu'#039');'#010+
  '  If APackage.HasIncludePath then'#010+
  '    AddStrings(Result,APackage.IncludePath,'#039'-Fi'#039');'#010+
  '  If APackage.HasObjectPath t','hen'#010+
  '    AddStrings(Result,APackage.ObjectPath,'#039'-Fo'#039');'#010+
  '  If Target.HasUnitPath then'#010+
  '    AddStrings(Result,Target.UnitPath,'#039'-Fu'#039');'#010+
  '  If Target.HasIncludePath then'#010+
  '    AddStrings(Result,Target.IncludePath,'#039'-Fi'#039');'#010+
  '  If Target.HasObjectPath then',#010+
  '    AddStrings(Result,Target.ObjectPath,'#039'-Fo'#039');'#010+
  '  If (Target.Options<>'#039#039') then'#010+
  '    Result:=Result+'#039' '#039'+Target.Options;'#010+
  '  TD:=Target.Directory;'#010+
  '  if (TD<>'#039#039') then'#010+
  '    TD:=IncludeTrailingPathDelimiter(TD);'#010+
  '  Result:=Result+'#039' '#039'+TD+Target.Sourc','eFileName;'#010+
  'end;'#010+
  #010+
  'Function TBuildEngine.GetCompiler : String;'#010+
  #010+
  'Var'#010+
  '  S : String;'#010+
  #010+
  'begin'#010+
  '  // Cache in FCompiler for speed.'#010+
  '  If (FCompiler='#039#039') then'#010+
  '    begin'#010+
  '    FCompiler:=Defaults.Compiler;'#010+
  '    If (ExtractFilePath(FCompiler)='#039#039') then'#010+
  '     ',' begin'#010+
  '      S:=FileSearch(FCompiler,GetEnvironmentVariable('#039'PATH'#039'));'+
  #010+
  '      If (S<>'#039#039') then'#010+
  '         FCompiler:=S;'#010+
  '      end;'#010+
  '    end;'#010+
  '  Result:=FCompiler;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Compile(Target: TTarget);'#010+
  #010+
  'Var'#010+
  '  S : String;'#010+
  #010+
  'begin'#010+
  '  i','f Target.State in [tsNeutral,tsCompiling] then'#010+
  '    begin'#010+
  '    Log(vlInfo,SLogCompilingTarget,[Target.Name]);'#010+
  '    If Target.HasCommands then'#010+
  '      ExecuteCommands(Target.Commands,caBeforeCompile);'#010+
  '    If Assigned(Target.BeforeCompile) then'#010+
  '  ','    Target.BeforeCompile(Target);'#010+
  '    S:=GetCompilerCommand(FCurrentPackage,Target);'#010+
  '    ExecuteCommand(GetCompiler,S);'#010+
  '    Target.FTargetState:=tsCompiled;'#010+
  '    If Assigned(Target.AfterCompile) then'#010+
  '      Target.AfterCompile(Target);'#010+
  '    If',' Target.HasCommands then'#010+
  '      ExecuteCommands(Target.Commands,caAfterCompile);'#010+
  '    end'#010+
  '  else if Target.State<>tsCompiled then'#010+
  '    Log(vlWarning, Format(SWarnAttemptingToCompileNonNeutralTarget, [T'+
  'arget.Name])'#010+
  '      );'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBu','ildEngine.FixDependencies(Target: TTarget);'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  '  T : TTarget;'#010+
  #010+
  'begin'#010+
  '  Log(vlDebug, Format(SDebugCheckingDependenciesForTarget, [Target.Nam'+
  'e]));'#010+
  '  ResolveDependencies(Target.Dependencies,Target.Collection as TTarget'+
  's);'#010+
  '  If',' Target.HasDependencies then'#010+
  '    For I:=0 to Target.Dependencies.Count-1 do'#010+
  '      begin'#010+
  '      T:=TTarget(Target.Dependencies.Objects[i]);'#010+
  '      If Assigned(T) then'#010+
  '        begin'#010+
  '        If (T.State=tsCompiling) then'#010+
  '          Log(vlWarning,','SWarnCircularDependency,[Target.Name,T.Name]'+
  ')'#010+
  '        else'#010+
  '          Compile(T)'#010+
  '        end'#010+
  '      else if Not FileExists(Target.Dependencies[i]) then'#010+
  '        Error(SErrDepUnknownTarget,[Target.Name,Target.Dependencies[i]'+
  ']);'#010+
  '      end;'#010+
  'end;'#010,
  #010+
  'function TBuildEngine.GetPackageDir(APackage: TPackage; AbsolutePath: '+
  'Boolean'#010+
  '  ): String;'#010+
  'begin'#010+
  '  If AbsolutePath then'#010+
  '    Result:= IncludeTrailingPathDelimiter(FStartDir)'#010+
  '  else'#010+
  '    Result:='#039#039';'#010+
  '  Result:=Result+APackage.Directory;'#010+
  '  If (','Result<>'#039#039') then'#010+
  '    Result:= ExcludeTrailingPathDelimiter(Result)'#010+
  'end;'#010+
  #010+
  'Function TBuildEngine.GetOutputDir(AName: string; APackage : TPackage;'+
  ' AbsolutePath : Boolean = False) : String;'#010+
  'begin'#010+
  '  If (TargetDir<>'#039#039') then'#010+
  '    Result:=TargetDir'#010,
  '  else'#010+
  '    begin'#010+
  '    If AbsolutePath then'#010+
  '      Result:=IncludeTrailingPathDelimiter(FStartDir)'#010+
  '    else'#010+
  '      Result:='#039#039';'#010+
  '    If (APackage.Directory<>'#039#039') then'#010+
  '      Result:=IncludeTrailingPathDelimiter(Result+APackage.Directory);'+
  #010+
  '    Resul','t := IncludeTrailingPathDelimiter(Result + AName + PathDeli'+
  'm + Defaults.Target);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'Function TBuildEngine.GetUnitsOutputDir(APackage : TPackage; AbsoluteP'+
  'ath : Boolean = False) : String;'#010+
  'begin'#010+
  '  Result := GetOutputDir('#039'units'#039', A','Package, AbsolutePath);'#010+
  'end;'#010+
  #010+
  'Function TBuildEngine.GetBinOutputDir(APackage : TPackage; AbsolutePat'+
  'h : Boolean = False) : String;'#010+
  'begin'#010+
  '  Result := GetOutputDir('#039'bin'#039', APackage, AbsolutePath);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.CreateOutputDir(A','Package: TPackage);'#010+
  'Var'#010+
  '  D : String;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  //create a units directory'#010+
  '  D:=GetUnitsOutputDir(APackage,True);'#010+
  '  If DirectoryExists(D) then'#010+
  '    Log(vlInfo,SLogOutputDirExists,[D])'#010+
  '  else'#010+
  '    begin'#010+
  '    Log(vlInfo,SLogCreatingO','utputDir,[D]);'#010+
  '    CmdCreateDir(D);'#010+
  '    end;'#010+
  #010+
  '  //also create a bin directory for programtargets'#010+
  '  For i := 0 to Pred(APackage.Targets.Count) do'#010+
  '  begin'#010+
  '    if APackage.Targets.TargetItems[i].TargetType in ProgramTargets th'+
  'en'#010+
  '    begin'#010+
  '    ','  D:=GetBinOutputDir(APackage,True);'#010+
  '      If DirectoryExists(D) then'#010+
  '        Log(vlInfo,SLogOutputDirExists,[D])'#010+
  '      else'#010+
  '      begin'#010+
  '        Log(vlInfo,SLogCreatingOutputDir,[D]);'#010+
  '        CmdCreateDir(D);'#010+
  '      end;'#010+
  #010+
  '      exit; //do no','t continue loop, directory is made anyway'#010+
  '    end;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'Function TBuildEngine.PackageOK(APackage : TPackage) : Boolean;'#010+
  #010+
  'begin'#010+
  '  Result:=((APackage.CPU=[]) or (Defaults.CPU in APackage.CPU))'#010+
  '          and'#010+
  '          ((APAckage.OS=[])',' or (Defaults.OS in APackage.OS));'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.DoBeforeCompile(APackage: TPackage);'#010+
  'begin'#010+
  '  If APackage.HasCommands then'#010+
  '    ExecuteCommands(APackage.Commands,caBeforeCompile);'#010+
  '  If Assigned(APackage.BeforeCompile) then'#010+
  '  ','  APackage.BeforeCompile(APackage);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.DoAfterCompile(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterCompile) then'#010+
  '    APackage.AfterCompile(APackage);'#010+
  '  If APackage.HasCommands then'#010+
  '    ExecuteCommands(APa','ckage.Commands,caAfterCompile);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Compile(APackage: TPackage);'#010+
  'Var'#010+
  '  T : TTarget;'#010+
  '  I : Integer;'#010+
  'begin'#010+
  '  Log(vlInfo,SLogCompilingPackage,[APackage.Name]);'#010+
  '  FCurrentPackage:=APackage;'#010+
  '  FCurrentOutputDir:=GetUnits','OutputDir(APackage,True);'#010+
  '  Try'#010+
  '    If (APackage.Directory<>'#039#039') then'#010+
  '      EnterDir(APackage.Directory);'#010+
  '    CreateOutputDir(APackage);'#010+
  '    Dictionary.AddVariable('#039'OUTPUTDIR'#039',FCurrentOutputDir);'#010+
  '    DoBeforeCompile(APackage);'#010+
  '    Try'#010+
  '      ','For I:=0 to APackage.Targets.Count-1 do'#010+
  '        begin'#010+
  '        T:=APackage.Targets.TargetItems[i];'#010+
  '        Log(vlDebug, Format(SDebugConsideringTarget, [T.Name]));'#010+
  '        If TargetOK(T) then'#010+
  '          If (T.State=tsNeutral) then'#010+
  '           ',' begin'#010+
  '            If (FForceCompile or NeedsCompile(T)) then'#010+
  '              begin'#010+
  '              T.FTargetState:=tsCompiling;'#010+
  '              FixDependencies(T);'#010+
  '              Compile(T);'#010+
  '              end;'#010+
  '            T.FTargetState:=tsCompil','ed;'#010+
  '            end;'#010+
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
  'end;'#010+
  #010+
  'procedure TBuild','Engine.CheckExternalPackage(Const APackageName : Str'+
  'ing);'#010+
  #010+
  'begin'#010+
  '  // A check needs to be implemented here.'#010+
  '  Log(vldebug, SDebugUnresolvedExternalDependencyS, [APackageName]);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.FixDependencies(APackage: TPackage',');'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  #010+
  'begin'#010+
  '  if APackage.HasDependencies then'#010+
  '    For I:=0 to APAckage.Dependencies.Count-1 do'#010+
  '      begin'#010+
  '      P:=TPackage(Apackage.Dependencies.Objects[i]);'#010+
  '      If Assigned(P) then'#010+
  '        Compile(P) ','// If it already was compiled, then State<>tsNeut'+
  'ral, and it won'#039't be compiled again.'#010+
  '      else'#010+
  '        CheckExternalPackage(Apackage.Dependencies[i]);'#010+
  '      end;'#010+
  'end;'#010+
  #010+
  'Procedure TBuildEngine.InstallPackageFiles(APAckage : TPackage; tt : T',
  'TargetType; Const Src,Dest : String);'#010+
  #010+
  'Var'#010+
  '  List : TStringList;'#010+
  '  UnitsDir: string;'#010+
  '  BinDir: string;'#010+
  'begin'#010+
  '  List:=TStringList.Create;'#010+
  '  Try'#010+
  '    UnitsDir := GetUnitsOutputDir(APackage);'#010+
  '    BinDir := GetBinOutputDir(APackage);'#010+
  '    APackag','e.GetInstallFiles(List,[tt],Src, UnitsDir, BinDir, Defaul'+
  'ts.OS);'#010+
  '    if (List.Count>0) then'#010+
  '      CmdCopyFiles(List,Dest);'#010+
  '  Finally'#010+
  '    List.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.DoBeforeInstall(APackage: TPackage);'#010+
  'begin'#010+
  '  If APackage','.HasCommands then'#010+
  '    ExecuteCommands(APackage.Commands,caBeforeInstall);'#010+
  '  If Assigned(APackage.BeforeInstall) then'#010+
  '    APackage.BeforeInstall(APackage);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.DoAfterInstall(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(','APackage.AfterInstall) then'#010+
  '    APackage.AfterInstall(APackage);'#010+
  '  If APackage.HasCommands then'#010+
  '    ExecuteCommands(APackage.Commands,caAfterInstall);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Install(APackage: TPackage);'#010+
  #010+
  #010+
  'Var'#010+
  '  PD,D,O : String;'#010+
  #010+
  'begi','n'#010+
  '  If (Apackage.State<>tsCompiled) then'#010+
  '    Compile(APackage);'#010+
  '  Log(vlInfo,SLogInstallingPackage,[APackage.Name]);'#010+
  '  DoBeforeInstall(APackage);'#010+
  '  O:=IncludeTrailingPathDelimiter(GetUnitsOutputDir(APAckage));'#010+
  '  PD:=IncludeTrailingPathDelim','iter(GetPackageDir(APackage));'#010+
  '  // units'#010+
  '  D:=IncludeTrailingPathDelimiter(Defaults.UnitInstallDir)+APackage.Na'+
  'me;'#010+
  '  InstallPackageFiles(APAckage,ttUnit,O,D);'#010+
  '  // Programs'#010+
  '  D:=IncludeTrailingPathDelimiter(Defaults.BinInstallDir);'#010+
  '  Insta','llPackageFiles(APAckage,ttProgram,PD,D);'#010+
  '  // Done.'#010+
  '  APackage.FTargetState:=tsInstalled;'#010+
  '  DoAfterInstall(APackage);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.DoBeforeArchive(APackage: TPackage);'#010+
  'begin'#010+
  '  If APackage.HasCommands then'#010+
  '    ExecuteCommands','(APackage.Commands,caBeforeArchive);'#010+
  '  If Assigned(APackage.BeforeArchive) then'#010+
  '    APackage.BeforeArchive(APackage);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.DoAfterArchive(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterArchive) then'#010+
  '    APack','age.AfterArchive(APackage);'#010+
  '  If APackage.HasCommands then'#010+
  '    ExecuteCommands(APackage.Commands,caAfterArchive);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Archive(APackage: TPackage);'#010+
  'Var'#010+
  '  L : TStringList;'#010+
  '  L2: TStringList;'#010+
  '  A : String;'#010+
  '  UnitsDir:',' string;'#010+
  '  BinDir: string;'#010+
  '  i: integer;'#010+
  'begin'#010+
  '  Log(vlInfo,SLogArchivingPackage,[APackage.Name]);'#010+
  '  DoBeforeArchive(Apackage);'#010+
  '  L:=TStringList.Create;'#010+
  '  L2:=TStringList.Create;'#010+
  '  Try'#010+
  '    //get all files'#010+
  '    //from targets'#010+
  '    APackage.Get','ArchiveFiles(L, TargetDir, Defaults.OS);'#010+
  '    //from sources'#010+
  '    for i := 0 to Pred(APackage.Sources.Count) do'#010+
  '      L.Add(APackage.Sources[i].Name);'#010+
  #010+
  '    //expand all filenames and ignore duplicates'#010+
  '    L2.Sorted := True;'#010+
  '    L2.Duplicates ',':= dupIgnore;'#010+
  '    for i := 0 to Pred(L.Count) do'#010+
  '      L2.Add(L[i]);'#010+
  #010+
  '    //show all files'#010+
  '    for i := 0 to Pred(L2.Count) do'#010+
  '      Log(vlInfo, Format(SInfoArchiving, [L2[i]]));'#010+
  #010+
  '    A:=APackage.FileName + ZipExt;'#010+
  #010+
  '{$ifdef HAS_UNIT_ZIPPER}',#010+
  '    if not Assigned(ArchiveFilesProc) then'#010+
  '      begin'#010+
  '        FZipFile := TZipper.Create;'#010+
  '        FZipFile.ZipFiles(A, L2);'#010+
  '      end'#010+
  '    else'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  '      CmdArchiveFiles(L2,A);'#010+
  '  Finally'#010+
  '    L.Free;'#010+
  '    L2.Free;'#010+
  #010+
  '{$ifd','ef HAS_UNIT_ZIPPER}'#010+
  '    if not Assigned(ArchiveFilesProc) then'#010+
  '      FZipFile.Free;'#010+
  '{$endif HAS_UNIT_ZIPPER}'#010+
  '  end;'#010+
  '  DoAfterArchive(Apackage);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.DoBeforeClean(APackage: TPackage);'#010+
  'begin'#010+
  '  If APackage.HasCommands ','then'#010+
  '    ExecuteCommands(APackage.Commands,caBeforeClean);'#010+
  '  If Assigned(APackage.BeforeClean) then'#010+
  '    APackage.BeforeClean(APackage);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.DoAfterClean(APackage: TPackage);'#010+
  'begin'#010+
  '  If Assigned(APackage.AfterClean) ','then'#010+
  '    APackage.AfterClean(APackage);'#010+
  '  If APackage.HasCommands then'#010+
  '    ExecuteCommands(APackage.Commands,caAfterClean);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Clean(APackage: TPackage);'#010+
  #010+
  'Var'#010+
  '  OU : String;'#010+
  '  OB : String;'#010+
  '  List : TStringList;'#010+
  #010+
  'be','gin'#010+
  '  Log(vlInfo,SLogCleaningPackage,[APackage.Name]);'#010+
  '  DoBeforeClean(Apackage);'#010+
  '  OU:=IncludeTrailingPathDelimiter(GetUnitsOutputDir(APAckage));'#010+
  '  OB:=IncludeTrailingPathDelimiter(GetBinOutputDir(APAckage));'#010+
  '  List:=TStringList.Create;'#010+
  '  ','try'#010+
  '    APackage.GetCleanFiles(List,OU, OB,Defaults.OS);'#010+
  '    if (List.Count>0) then'#010+
  '      CmdDeleteFiles(List);'#010+
  '  Finally'#010+
  '    List.Free;'#010+
  '  end;'#010+
  '  DoAfterClean(Apackage);'#010+
  'end;'#010+
  #010+
  'function TBuildEngine.NeedsCompile(APackage: TPackage): Boolean;',#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  #010+
  'begin'#010+
  '  ResolveDependencies(APackage.Dependencies,(APackage.Collection as TP'+
  'ackages));'#010+
  '  Result:=False;'#010+
  '  I:=0;'#010+
  '  While (Not Result) and (I<APackage.Dependencies.Count) do'#010+
  '    begin'#010+
  '    P:=TPackage(APac','kage.Dependencies.Objects[i]);'#010+
  '    // I'#039'm not sure whether the target dir is OK here ??'#010+
  '    Result:=Assigned(P) and NeedsCompile(P);'#010+
  '    Inc(I);'#010+
  '    end;'#010+
  '  If Not Result then'#010+
  '    begin'#010+
  '    I:=0;'#010+
  '    While (Not Result) and (I<APackage.Target','s.Count) do'#010+
  '      begin'#010+
  '      Result:=NeedsCompile(APackage.Targets.TargetItems[i]);'#010+
  '      Inc(I);'#010+
  '      end;'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'Procedure TBuildEngine.GetSourceFiles(APackage : TPackage; List : TStr'+
  'ings);'#010+
  'var'#010+
  '  L : TStrings;'#010+
  '  i : integer;'#010+
  '  s','XMLNode: string;'#010+
  'begin'#010+
  '  try'#010+
  '    L:=TStringList.Create;'#010+
  '    //APackage.GetSourceFiles(L);'#010+
  #010+
  '    List.Add(Format('#039'<sources packagename="%s">'#039',[QuoteXml(APa'+
  'ckage.Name)]));'#010+
  #010+
  '    for i := 0 to Pred(APackage.Sources.Count) do'#010+
  '    begin'#010+
  '      case',' APackage.Sources[i].SourceType of'#010+
  '        stDoc: sXMLNode := '#039'<source type="document">"%s"</source>'#039+
  ';'#010+
  '        stSrc: sXMLNode := '#039'<source type="source">"%s"</source>'#039+
  ';'#010+
  '        stExample: sXMLNode := '#039'<source type="example">"%s"</sourc'+
  'e>'#039';'#010,
  '        stTest: sXMLNode := '#039'<source type="test">"%s"</source>'#039+
  ';'#010+
  '      else'#010+
  '        sXMLNode := '#039'<source type="unknown">"%s"</source>'#039';'#010+
  '      end;'#010+
  #010+
  '      List.Add(Format(sXMLNode,[QuoteXml(APackage.Sources[i].Name)]));'+
  #010+
  '    end;'#010+
  #010+
  '    List.Ad','d('#039'</sources>'#039');'#010+
  '  finally'#010+
  '    L.Free;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure TBuildEngine.GetManifest(APackage : TPackage; Manifest : TSt'+
  'rings);'#010+
  #010+
  'begin'#010+
  '  APackage.GetManifest(Manifest);'#010+
  'end;'#010+
  #010+
  #010+
  'procedure TBuildEngine.Compile(Packages: TPackages);'#010+
  #010+
  'Var'#010+
  '  I',' : Integer;'#010+
  '  P : TPackage;'#010+
  #010+
  'begin'#010+
  '  If Assigned(BeforeCompile) then'#010+
  '    BeforeCompile(Self);'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '    P:=Packages.PackageItems[i];'#010+
  '    FCurrentPackage := P;'#010+
  '    If PackageOK(P) then'#010+
  '      If (P.State=','tsNeutral) then'#010+
  '        begin'#010+
  '        If (FForceCompile or NeedsCompile(P)) then'#010+
  '          begin'#010+
  '          P.FTargetState:=tsCompiling;'#010+
  '          FixDependencies(P);'#010+
  '          Compile(P);'#010+
  '          end;'#010+
  '        P.FTargetState:=tsCompiled;'#010+
  ' ','       end;'#010+
  '    end;'#010+
  '  If Assigned(AfterCompile) then'#010+
  '    AfterCompile(Self);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Install(Packages: TPackages);'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  #010+
  'begin'#010+
  '  If Assigned(BeforeInstall) then'#010+
  '    BeforeInstall(Self);'#010+
  ' ',' For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '    P:=Packages.PackageItems[i];'#010+
  '    If PackageOK(P) then'#010+
  '      Install(P);'#010+
  '    end;'#010+
  '  If Assigned(AfterInstall) then'#010+
  '    AfterInstall(Self);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Archive(Packages: TPackage','s);'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  #010+
  'begin'#010+
  '  If Assigned(BeforeArchive) then'#010+
  '    BeforeArchive(Self);'#010+
  '  Log(vlDebug, SDebugBuildEngineArchiving);'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '    P:=Packages.PackageItems[i];'#010+
  '    If Packag','eOK(P) then'#010+
  '      Archive(P);'#010+
  '    end;'#010+
  '  If Assigned(AfterArchive) then'#010+
  '    AfterArchive(Self);'#010+
  'end;'#010+
  #010+
  'procedure TBuildEngine.Clean(Packages: TPackages);'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  '  P : TPackage;'#010+
  #010+
  'begin'#010+
  '  If Assigned(BeforeClean) then'#010+
  '    BeforeCl','ean(Self);'#010+
  '  Log(vldebug, SDebugBuildEngineCleaning);'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    begin'#010+
  '    P:=Packages.PackageItems[i];'#010+
  '    If PackageOK(P) then'#010+
  '      Clean(P);'#010+
  '    end;'#010+
  '  If Assigned(AfterClean) then'#010+
  '    AfterClean(Self);'#010+
  'end;'#010+
  #010,
  'Procedure TBuildEngine.GetManifest(Packages : TPackages; Manifest : TS'+
  'trings);'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'begin'#010+
  '  If Assigned(BeforeManifest) then'#010+
  '    BeforeManifest(Self);'#010+
  '  Manifest.Add('#039'<packages>'#039');'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    GetMa','nifest(Packages.PackageItems[i],Manifest);'#010+
  '  Manifest.Add('#039'</packages>'#039');'#010+
  '  If Assigned(AfterManifest) then'#010+
  '    AfterManifest(Self);'#010+
  'end;'#010+
  #010+
  #010+
  'Procedure TBuildEngine.GetSourceFiles(Packages : TPackages; List : TSt'+
  'rings);'#010+
  #010+
  'Var'#010+
  '  I : Integer;'#010+
  #010+
  'b','egin'#010+
  '  List.Add('#039'<packages>'#039');'#010+
  '  For I:=0 to Packages.Count-1 do'#010+
  '    GetSourceFiles(Packages.PackageItems[i],List);'#010+
  '  List.Add('#039'</packages>'#039');'#010+
  'end;'#010+
  #010+
  #010+
  '{ TTarget }'#010+
  #010+
  'function TTarget.GetHasStrings(AIndex: integer): Boolean;'#010+
  'begin'#010+
  '  Result:=Fal','se;'#010+
  '  Case AIndex Of'#010+
  '    0 : Result:=FUnitPath<>Nil;'#010+
  '    1 : Result:=FObjectPath<>Nil;'#010+
  '    2 : Result:=FIncludePath<>Nil;'#010+
  '    3 : Result:=FDependencies<>Nil;'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'function TTarget.GetCommands: TCommands;'#010+
  'begin'#010+
  '  If FCommands=Nil the','n'#010+
  '    FCommands:=TCommands.Create(TCommand);'#010+
  '  Result:=FCommands;'#010+
  'end;'#010+
  #010+
  'function TTarget.GetHasCommands: Boolean;'#010+
  'begin'#010+
  '  Result:=(FCommands<>Nil);'#010+
  'end;'#010+
  #010+
  'function TTarget.GetStrings(AIndex: integer): TStrings;'#010+
  #010+
  '  Function EnsureStrings(Var ','S : TStrings) : TStrings;'#010+
  #010+
  '  begin'#010+
  '    If (S=Nil) then'#010+
  '      S:=TStringList.Create;'#010+
  '    Result:=S;'#010+
  '  end;'#010+
  #010+
  'begin'#010+
  '  Result:=Nil;'#010+
  '  Case AIndex Of'#010+
  '    0 : Result:=EnsureStrings(FUnitPath);'#010+
  '    1 : Result:=EnsureStrings(FObjectPath);'#010+
  '    2 : R','esult:=EnsureStrings(FIncludePath);'#010+
  '    3 : Result:=EnsureStrings(FDependencies);'#010+
  '  end;'#010+
  'end;'#010+
  #010+
  'procedure TTarget.SetCommands(const AValue: TCommands);'#010+
  'begin'#010+
  #010+
  'end;'#010+
  #010+
  'procedure TTarget.SetStrings(AIndex: integer; const AValue: TStrings);'+
  #010+
  'begin',#010+
  '  GetStrings(AIndex).Assign(AValue);'#010+
  'end;'#010+
  #010+
  #010+
  'function TTarget.GetSourceFileName: String;'#010+
  'begin'#010+
  '  Result:=Name+FExtension;'#010+
  'end;'#010+
  #010+
  'function TTarget.GetUnitFileName: String;'#010+
  'begin'#010+
  '  Result:=Name+UnitExt;'#010+
  'end;'#010+
  #010+
  'function TTarget.GetObjectFileName',': String;'#010+
  'begin'#010+
  '  Result:=Name+ObjExt;'#010+
  'end;'#010+
  #010+
  'function TTarget.GetRSTFileName: String;'#010+
  'begin'#010+
  '  Result:=Name+RSText;'#010+
  'end;'#010+
  #010+
  'function TTarget.GetProgramFileName(AnOS : TOS): String;'#010+
  'begin'#010+
  '  if AnOS in [Go32v2,Win32,Win64,OS2] then'#010+
  '    Result:=N','ame+ExeExt'#010+
  '  else'#010+
  '    Result:=Name;'#010+
  'end;'#010+
  #010+
  'constructor TTarget.Create(ACollection: TCollection);'#010+
  'begin'#010+
  '  inherited Create(ACollection);'#010+
  '  FInstall:=True;'#010+
  'end;'#010+
  #010+
  'destructor TTarget.Destroy;'#010+
  'begin'#010+
  '  FreeAndNil(FUnitPath);'#010+
  '  FreeAndNil(FObjectPa','th);'#010+
  '  FreeAndNil(FIncludePath);'#010+
  '  FreeAndNil(FDependencies);'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  'function TTarget.GetOutputFileName(AOs: TOS): String;'#010+
  'begin'#010+
  '  if TargetType in UnitTargets then'#010+
  '    Result:=GetUnitFileName'#010+
  '  else'#010+
  '    Result:=GetProgr','amFileName(AOs);'#010+
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
  '  D:=ExtractFilePath(N);'#010+
  '  E:=ExtractFileExt(N);'#010+
  '  N:=ExtractFileName(N);'#010+
  '  If (E<>'#039#039') then'#010+
  '    N:=Copy(N,1,Length(','N)-Length(E));'#010+
  '  inherited SetName(N);'#010+
  '  FExtension:=E;'#010+
  '  FDirectory:=D;'#010+
  'end;'#010+
  #010+
  'procedure TTarget.GetCleanFiles(List: TStrings; APrefixU, APrefixB : S'+
  'tring; AnOS : TOS);'#010+
  'begin'#010+
  '  If (OS=[]) or (AnOS in OS) then'#010+
  '    begin'#010+
  '    List.Add(APrefixU',' + ObjectFileName);'#010+
  '    If (TargetType in [ttUnit,ttExampleUnit]) then'#010+
  '      List.Add(APrefixU + UnitFileName)'#010+
  '    else If (TargetType in [ttProgram,ttExampleProgram]) then'#010+
  '      List.Add(APrefixB + GetProgramFileName(AnOS));'#010+
  '    If Resourc','eStrings then'#010+
  '      List.Add(APrefixU + RSTFileName);'#010+
  '    // Maybe add later ?  AddStrings(List,CleanFiles);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TTarget.GetSourceFiles(List: TStrings; APrefix : String; AnO'+
  'S : TOS);'#010+
  'begin'#010+
  '  If (OS=[]) or (AnOS in OS) t','hen'#010+
  '    begin'#010+
  '    List.Add(APrefix+ObjectFileName);'#010+
  '    If (TargetType in [ttUnit,ttExampleUnit]) then'#010+
  '      List.Add(APrefix+UnitFileName)'#010+
  '    else If (TargetType in [ttProgram,ttExampleProgram]) then'#010+
  '      List.Add(APrefix+GetProgramFileN','ame(AnOS));'#010+
  '    If ResourceStrings then'#010+
  '      List.Add(APrefix+RSTFileName);'#010+
  '    // Maybe add later ?  AddStrings(List,CleanFiles);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TTarget.GetInstallFiles(List: TStrings; APrefixU, APrefixB: '+
  'String; AnOS : TOS);'#010+
  'be','gin'#010+
  '  If (OS=[]) or (AnOS in OS) then'#010+
  '    begin'#010+
  '    If Not (TargetType in [ttProgram,ttExampleProgram]) then'#010+
  '      List.Add(APrefixU + ObjectFileName);'#010+
  '    If (TargetType in [ttUnit,ttExampleUnit]) then'#010+
  '      List.Add(APrefixU + UnitFileNam','e)'#010+
  '    else If (TargetType in [ttProgram,ttExampleProgram]) then'#010+
  '      List.Add(APrefixB + GetProgramFileName(AnOS));'#010+
  '    If ResourceStrings then'#010+
  '      List.Add(APrefixU + RSTFileName);'#010+
  '    // Maybe add later ?  AddStrings(List,InstallFiles',');'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'procedure TTarget.GetArchiveFiles(List: TStrings; APrefix : String; An'+
  'OS : TOS);'#010+
  'begin'#010+
  '  If (OS=[]) or (AnOS in OS) then'#010+
  '    begin'#010+
  '    List.Add(APrefix+SourceFileName);'#010+
  '    // Maybe add later ?  AddStrings(List,ArchiveFile','s);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  #010+
  '{ TSource }'#010+
  #010+
  'constructor TSource.Create(ACollection: TCollection);'#010+
  'begin'#010+
  '  inherited Create(ACollection);'#010+
  'end;'#010+
  #010+
  'destructor TSource.Destroy;'#010+
  'begin'#010+
  '  inherited Destroy;'#010+
  'end;'#010+
  #010+
  'procedure TSource.GetSourceFiles(List: TStrings',');'#010+
  'begin'#010+
  '  List.Add(Name);'#010+
  'end;'#010+
  #010+
  #010+
  '{ TCommands }'#010+
  #010+
  'function TCommands.GetCommand(Dest : String): TCommand;'#010+
  #010+
  'begin'#010+
  '  Result:=TCommand(ItemByName(Dest));'#010+
  'end;'#010+
  #010+
  'function TCommands.GetCommandItem(Index : Integer): TCommand;'#010+
  'begin'#010+
  '  Result:=TComma','nd(Items[Index]);'#010+
  'end;'#010+
  #010+
  'procedure TCommands.SetCommandItem(Index : Integer; const AValue: TCom'+
  'mand);'#010+
  'begin'#010+
  '  Items[Index]:=AValue;'#010+
  'end;'#010+
  #010+
  'Function TCommands.AddCommand(const Cmd: String) : TCommand;'#010+
  'begin'#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,',#039#039','#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  'function TCommands.AddCommand(const Cmd, Options: String): TCommand;'#010+
  'begin'#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,Options,'#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  'function TCommands.AddCommand(const Cmd, Options, Dest, Source: String'+
  #010+
  '  ): TCommand;'#010+
  'begi','n'#010+
  '  Result:=AddCommand(fdefaultAt,Cmd,options,Dest,Source);'#010+
  'end;'#010+
  #010+
  'Function TCommands.AddCommand(At: TCommandAt; const Cmd: String) : TCo'+
  'mmand;'#010+
  'begin'#010+
  '  Result:=AddCommand(At,Cmd,'#039#039','#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  'function TCommands.AddCommand(At: TCommandAt; ','const Cmd, Options: St'+
  'ring'#010+
  '  ): TCommand;'#010+
  'begin'#010+
  '  Result:=AddCommand(At,Cmd,Options,'#039#039','#039#039');'#010+
  'end;'#010+
  #010+
  'function TCommands.AddCommand(At: TCommandAt; const Cmd, Options, Dest'+
  ','#010+
  '  Source: String): TCommand;'#010+
  'begin'#010+
  '  Result:=Add as TCommand;'#010+
  '  Result','.Command:=Cmd;'#010+
  '  Result.Options:=Options;'#010+
  '  Result.At:=At;'#010+
  '  Result.SourceFile:=Source;'#010+
  '  Result.DestFile:=Dest;'#010+
  'end;'#010+
  #010+
  'Var'#010+
  '  DefInstaller : TCustomInstaller = Nil;'#010+
  '  DefDictionary : TDictionary = Nil;'#010+
  #010+
  'Function Installer(InstallerClass: TIn','stallerClass): TCustomInstalle'+
  'r;'#010+
  'begin'#010+
  '  If Not Assigned(DefInstaller) then'#010+
  '    DefInstaller:=InstallerClass.Create(Nil);'#010+
  '  Result:=DefInstaller;'#010+
  'end;'#010+
  #010+
  'Function Installer: TCustomInstaller;'#010+
  'begin'#010+
  '  Result := Installer(TFPCInstaller);'#010+
  'end;'#010+
  #010,
  'Function Defaults : TCustomDefaults;'#010+
  #010+
  'begin'#010+
  '  Result:=Installer.Defaults;'#010+
  'end;'#010+
  #010+
  #010+
  'function Dictionary : TDictionary;'#010+
  'begin'#010+
  '  If Not Assigned(DefDictionary) then'#010+
  '    DefDictionary:=DictionaryClass.Create(Nil);'#010+
  '  Result:=DefDictionary;'#010+
  'end;'#010+
  #010+
  '{',' TValueItem }'#010+
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
  'constructor TDictionary.C','reate(AOwner: TComponent);'#010+
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
  '  For I:=0 to Flist.Count-1 do'#010+
  '  ','  FList.Objects[i].Free;'#010+
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
  '    I:=FList.Add(Aname)'#010+
  '  else'#010+
  '    ','Flist.Objects[i].Free;'#010+
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
  '  I:=Flist.IndexOf(AName);'#010+
  '  If I=-1 then'#010+
  '    I:','=Flist.Add(AName)'#010+
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
  '  I:=Flist.IndexOf(AName);'#010+
  '  If (I<>-1) then',#010+
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
  'function TDictionary.GetValue(const AName,Args: String): Strin','g;'#010+
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
  '    Result:=TValueItem(O).FValue'#010+
  ' ',' else'#010+
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
  '  P:=Pos('#039'$('#039',S);'#010+
  '  While (P<>0) do'#010+
  ' ','   begin'#010+
  '    Result:=Result+Copy(S,1,P-1);'#010+
  '    Delete(S,1,P+1);'#010+
  '    P:=Pos('#039')'#039',S);'#010+
  '    FN:=Copy(S,1,P-1);'#010+
  '    Delete(S,1,P);'#010+
  '    P:=Pos('#039' '#039',FN);'#010+
  '    If (P<>0) then // function arguments ?'#010+
  '      begin'#010+
  '      FV:=FN;'#010+
  '      FN:=Copy(FN,1,P);'#010+
  '  ','    System.Delete(FV,1,P);'#010+
  '      end'#010+
  '    else'#010+
  '      FV:='#039#039';'#010+
  '    Result:=Result+GetValue(FN,FV);'#010+
  '    P:=Pos('#039'$('#039',S);'#010+
  '    end;'#010+
  '  Result:=Result+S;'#010+
  'end;'#010+
  #010+
  'Function Substitute(Const Source : String; Macros : Array of string) :'+
  ' String;'#010+
  'Var'#010+
  '  I : ','Integer;'#010+
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
  '    Dictionary.RemoveItem(','Macros[i]);'#010+
  '    Inc(I,2);'#010+
  '    end;'#010+
  'end;'#010+
  #010+
  'Initialization'#010+
  '  OnGetApplicationName:=@GetFPMakeName;'#010+
  #010+
  'Finalization'#010+
  '  FreeAndNil(DefInstaller);'#010+
  '  FreeAndNil(DefDictionary);'#010+
  'end.'#010
);
