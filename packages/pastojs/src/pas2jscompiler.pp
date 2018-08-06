{ Author: Mattias Gaertner  2018  mattias@freepascal.org

Abstract:
  TPas2jsCompiler is the wheel boss of the pas2js compiler.
  It can be used in a command line program or compiled into an application.

Compiler-ToDos:
  Warn if -Ju and -Fu intersect
  -Fa<x>[,y] (for a program) load units <x> and [y] before uses is parsed
  Add Windows macros, see InitMacros.
  add options for names of globals like 'pas' and 'rtl'
}
unit Pas2jsCompiler;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, RtlConsts, AVL_Tree, contnrs, PScanner, PParser,
  PasTree, PasResolver, PasUseAnalyzer, PasResolveEval, jstree, jswriter,
  JSSrcMap, FPPas2Js, FPPJsSrcMap, Pas2jsFileUtils, Pas2jsLogger,
  Pas2jsFileCache, Pas2jsPParser, Pas2JsFiler;

const
  VersionMajor = 1;
  VersionMinor = 1;
  VersionRelease = 1;
  VersionExtra = '';
  DefaultConfigFile = 'pas2js.cfg';

//------------------------------------------------------------------------------
// Messages
const
  nOptionIsEnabled = 101; sOptionIsEnabled = 'Option "%s" is %s';
  nSyntaxModeIs = 102; sSyntaxModeIs = 'Syntax mode is %s';
  nMacroDefined = 103; sMacroDefined = 'Macro defined: %s';
  nUsingPath = 104; sUsingPath = 'Using %s: "%s"';
  nFolderNotFound = 105; sFolderNotFound = '%s not found: %s';
  nNameValue = 106; sNameValue = '%s: %s';
  nReadingOptionsFromFile = 107; sReadingOptionsFromFile = 'Reading options from file %s';
  nEndOfReadingConfigFile = 108; sEndOfReadingConfigFile = 'End of reading config file %s';
  nInterpretingFileOption = 109; sInterpretingFileOption = 'interpreting file option %s';
  nSourceFileNotFound = 110; sSourceFileNotFound = 'source file not found %s';
  nFileIsFolder = 111; sFileIsFolder = 'expected file, but found directory %s';
  nConfigFileSearch = 112; sConfigFileSearch = 'Configfile search: %s';
  nHandlingOption = 113; sHandlingOption = 'handling option %s';
  nQuickHandlingOption = 114; sQuickHandlingOption = 'quick handling option %s';
  nOutputDirectoryNotFound = 115; sOutputDirectoryNotFound = 'output directory not found: %s';
  nUnableToWriteFile = 116; sUnableToWriteFile = 'Unable to write file %s';
  nWritingFile = 117; sWritingFile = 'Writing file %s ...';
  nCompilationAborted = 118; sCompilationAborted = 'Compilation aborted';
  nCfgDirective = 119; sCfgDirective = 'cfg directive %s: %s';
  nUnitCycle = 120; sUnitCycle = 'Unit cycle found %s';
  nOptionForbidsCompile = 121; sOptionForbidsCompile = 'Option -Ju forbids to compile unit "%s"';
  nUnitNeedsCompileDueToUsedUnit = 122; sUnitsNeedCompileDueToUsedUnit = 'Unit "%s" needs compile due to used unit "%s"';
  nUnitNeedsCompileDueToOption = 123; sUnitsNeedCompileDueToOption = 'Unit "%s" needs compile due to option "%s"';
  nUnitNeedsCompileJSMissing = 124; sUnitsNeedCompileJSMissing = 'Unit "%s" needs compile, js file missing "%s"';
  nUnitNeedsCompilePasHasChanged = 125; sUnitsNeedCompilePasHasChanged = 'Unit "%s" needs compile, Pascal file has changed, js is %s';
  nParsingFile = 126; sParsingFile = 'Parsing %s ...';
  nCompilingFile = 127; sCompilingFile = 'Compiling %s ...';
  nExpectedButFound = 128; sExpectedButFound = 'Illegal unit name: Expected "%s", but found "%s"';
  nLinesInFilesCompiled = 129; sLinesInFilesCompiled = '%s lines in %s files compiled, %s sec%s';
  nTargetPlatformIs = 130; sTargetPlatformIs = 'Target platform is %s';
  nTargetProcessorIs = 131; sTargetProcessorIs = 'Target processor is %s';
  nMessageEncodingIs = 132; sMessageEncodingIs = 'Message encoding is %s';
  nUnableToTranslatePathToDir = 133; sUnableToTranslatePathToDir = 'Unable to translate path %s to directory %s';
  nSrcMapSourceRootIs = 134; sSrcMapSourceRootIs = 'source map "sourceRoot" is %s';
  nSrcMapBaseDirIs = 135; sSrcMapBaseDirIs = 'source map "local base directory" is %s';
  nUnitFileNotFound = 136; sUnitFileNotFound = 'unit file not found %s';
  nClassInterfaceStyleIs = 137; sClassInterfaceStyleIs = 'Class interface style is %s';
  nMacroXSetToY = 138; sMacroXSetToY = 'Macro %s set to %s';
  // Note: error numbers 201+ are used by Pas2jsFileCache

//------------------------------------------------------------------------------
// Options
type
  TP2jsCompilerOption = (
    coSkipDefaultConfigs,
    coBuildAll,
    // verbosity
    coShowLogo,
    coShowErrors,
    coShowWarnings,
    coShowNotes,
    coShowHints,
    coShowInfos,
    coShowLineNumbers,
    coShowTriedUsedFiles,
    coShowConditionals,
    coShowUsedTools,
    coShowDebug,
    coShowMessageNumbers, // not in "show all"
    // checks
    coOverflowChecks,
    coRangeChecks,
    coObjectChecks,
    coAssertions,
    // features
    coAllowCAssignments,
    coAllowMacros,
    // output
    coLowerCase,
    coUseStrict,
    coWriteDebugLog,
    coWriteMsgToStdErr,
    // optimizations
    coEnumValuesAsNumbers,
    coKeepNotUsedPrivates,
    coKeepNotUsedDeclarationsWPO,
    // source map
    coSourceMapCreate,
    coSourceMapInclude,
    coSourceMapXSSIHeader
    );
  TP2jsCompilerOptions = set of TP2jsCompilerOption;
  TP2jsOptimization = coEnumValuesAsNumbers..coKeepNotUsedDeclarationsWPO;
const
  DefaultP2jsCompilerOptions = [coShowErrors,coSourceMapXSSIHeader,coUseStrict];
  coShowAll = [coShowErrors..coShowDebug];
  coO1Enable = [coEnumValuesAsNumbers];
  coO1Disable = [coKeepNotUsedPrivates,coKeepNotUsedDeclarationsWPO];

  p2jscoCaption: array[TP2jsCompilerOption] of string = (
    // only used by experts or programs parsing the pas2js output, no need for resourcestrings
    'Skip default configs',
    'Build all',
    'Show logo',
    'Show errors',
    'Show warnings',
    'Show notes',
    'Show hints',
    'Show infos',
    'Show line numbers',
    'Show tried/used files',
    'Show conditionals',
    'Show used tools',
    'Show debug',
    'Show message numbers',
    'Overflow checking',
    'Range checking',
    'Method call checking',
    'Assertions',
    'Allow C assignments',
    'Allow macros',
    'Lowercase identifiers',
    'Use strict',
    'Write pas2jsdebug.log',
    'Write messages to StdErr',
    'Enum values as numbers',
    'Keep not used private declarations',
    'Keep not used declarations (WPO)',
    'Create source map',
    'Include Pascal sources in source map',
    'Prepend XSSI protection )]} to source map'
    );

//------------------------------------------------------------------------------
// $mode and $modeswitches
type
  TP2jsMode = (
    p2jmObjFPC,
    p2jmDelphi
    );
  TP2jsModes = set of TP2jsMode;
const
  p2jscModeNames: array[TP2jsMode] of string = (
    'ObjFPC',
    'Delphi'
    );
  p2jsMode_SwitchSets: array[TP2jsMode] of TModeSwitches = (
    OBJFPCModeSwitches*msAllPas2jsModeSwitches+msAllPas2jsModeSwitchesReadOnly,
    DelphiModeSwitches*msAllPas2jsModeSwitches+msAllPas2jsModeSwitchesReadOnly
    );

//------------------------------------------------------------------------------
// param macros
type
  EPas2jsMacro = class(Exception);

  TOnSubstituteMacro = function(Sender: TObject; var Params: string; Lvl: integer): boolean of object;

  { TPas2jsMacro }

  TPas2jsMacro = class
  public
    Name: string;
    Description: string;
    Value: string;
    CanHaveParams: boolean;
    OnSubstitute: TOnSubstituteMacro;
  end;

  { TPas2jsMacroEngine }

  TPas2jsMacroEngine = class
  private
    fMacros: TObjectList; // list of TPas2jsMacro
    FMaxLevel: integer;
    function GetMacros(Index: integer): TPas2jsMacro;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    function AddValue(const aName, aDescription, aValue: string): TPas2jsMacro;
    function AddFunction(const aName, aDescription: string;
      const OnSubstitute: TOnSubstituteMacro; CanHaveParams: boolean): TPas2jsMacro;
    function IndexOf(const aName: string): integer;
    procedure Delete(Index: integer);
    function FindMacro(const aName: string): TPas2jsMacro;
    procedure Substitute(var s: string; Sender: TObject = nil; Lvl: integer = 0);
    property Macros[Index: integer]: TPas2jsMacro read GetMacros; default;
    property MaxLevel: integer read FMaxLevel write FMaxLevel;
  end;

//------------------------------------------------------------------------------
// Module file
type
  ECompilerTerminate = class(Exception);

  TPas2jsCompiler = class;

  TUsedBySection = (
    ubMainSection,
    ubImplSection
    );

  TPas2jsReaderState = (
    prsNone,
    prsReading,
    prsWaitingForUsedUnits,
    prsCanContinue,
    prsFinished,
    prsError
    );

  { TPas2jsCompilerFile }

  TPas2jsCompilerFile = class
  private
    FCompiler: TPas2jsCompiler;
    FConverter: TPasToJSConverter;
    FFileResolver: TPas2jsFileResolver;
    FIsForeign: boolean;
    FIsMainFile: boolean;
    FJSFilename: string;
    FJSModule: TJSElement;
    FLog: TPas2jsLogger;
    FNeedBuild: Boolean;
    FParser: TPas2jsPasParser;
    FPasFilename: String;
    FPasModule: TPasModule;
    FPasResolver: TPas2jsCompilerResolver;
    FPasUnitName: string;
    FPCUFilename: string;
    FPCUFormat: TPas2JSPrecompileFormat;
    FPCUReader: TPCUCustomReader;
    FPCUReaderStream: TStream;
    FReaderState: TPas2jsReaderState;
    FScanner: TPascalScanner;
    FShowDebug: boolean;
    FUseAnalyzer: TPasAnalyzer;
    FUsedBy: array[TUsedBySection] of TFPList; // list of TPas2jsCompilerFile
    function GetUsedBy(Section: TUsedBySection; Index: integer): TPas2jsCompilerFile;
    function GetUsedByCount(Section: TUsedBySection): integer;
    function OnConverterIsElementUsed(Sender: TObject; El: TPasElement): boolean;
    function OnConverterIsTypeInfoUsed(Sender: TObject; El: TPasElement): boolean;
    procedure OnFilerGetSrc(Sender: TObject; aFilename: string; out p: PChar;
      out Count: integer);
    procedure OnPasResolverLog(Sender: TObject; const Msg: String);
    procedure OnParserLog(Sender: TObject; const Msg: String);
    function OnPCUConverterIsElementUsed(Sender: TObject; El: TPasElement
      ): boolean;
    function OnPCUConverterIsTypeInfoUsed(Sender: TObject; El: TPasElement
      ): boolean;
    procedure OnScannerLog(Sender: TObject; const Msg: String);
    procedure OnUseAnalyzerMessage(Sender: TObject; Msg: TPAMessage);
    procedure HandleEParserError(E: EParserError);
    procedure HandleEPasResolve(E: EPasResolve);
    procedure HandleEPas2JS(E: EPas2JS);
    procedure HandleEPCUReader(E: EPas2JsReadError);
    procedure HandleUnknownException(E: Exception);
    procedure HandleException(E: Exception);
    procedure DoLogMsgAtEl(MsgType: TMessageType; const Msg: string;
      MsgNumber: integer; El: TPasElement);
    function OnWriterIsElementUsed(Sender: TObject; El: TPasElement): boolean;
    procedure RaiseInternalError(id: int64; Msg: string);
    procedure ReaderFinished;
    procedure WritePCU;
  public
    constructor Create(aCompiler: TPas2jsCompiler; const aPasFilename: string;
      aFormat: TPas2JSPrecompileFormat = nil);
    destructor Destroy; override;
    function GetInitialModeSwitches: TModeSwitches;
    function GetInitialBoolSwitches: TBoolSwitches;
    function GetInitialConverterOptions: TPasToJsConverterOptions;
    procedure CreateScannerAndParser(aFileResolver: TPas2jsFileResolver);
    procedure CreatePCUReader;
    procedure CreateConverter;
    function FindPCU(const UseUnitName: string; out aFormat: TPas2JSPrecompileFormat): string;
    function OnResolverFindModule(const UseUnitName, InFilename: String; NameExpr,
      InFileExpr: TPasExpr): TPasModule;
    function LoadUsedUnit(const UseFilename, UseUnitname, InFilename: String;
      NameExpr, InFileExpr: TPasExpr; UseIsForeign: boolean;
      aFormat: TPas2JSPrecompileFormat): TPas2jsCompilerFile;
    procedure OnResolverCheckSrcName(const Element: TPasElement);
    procedure OpenFile(aFilename: string);// beware: this changes FileResolver.BaseDirectory
    procedure ReadUnit;
    function ReadContinue: boolean; // true=finished
    function ReaderState: TPas2jsReaderState;
    procedure CreateJS;
    function GetPasFirstSection: TPasSection;
    function GetPasImplSection: TPasSection;
    function GetPasMainUsesClause: TPasUsesClause;
    function GetPasImplUsesClause: TPasUsesClause;
    function GetCurPasModule: TPasModule;
    function GetModuleName: string;
    class function GetFile(aModule: TPasModule): TPas2jsCompilerFile;
  public
    property Compiler: TPas2jsCompiler read FCompiler;
    property Converter: TPasToJSConverter read FConverter;
    property FileResolver: TPas2jsFileResolver read FFileResolver;
    property IsForeign: boolean read FIsForeign write FIsForeign;// true = do not build
    property IsMainFile: boolean read FIsMainFile write FIsMainFile;
    property JSFilename: string read FJSFilename write FJSFilename;
    property JSModule: TJSElement read FJSModule;
    property Log: TPas2jsLogger read FLog;
    property NeedBuild: Boolean read FNeedBuild write FNeedBuild;
    property Parser: TPas2jsPasParser read FParser;
    property PascalResolver: TPas2jsCompilerResolver read FPasResolver;
    property PasFilename: String read FPasFilename; // can be the PCUFilename
    property PasModule: TPasModule read FPasModule;
    property PasUnitName: string read FPasUnitName write FPasUnitName;// unit name in program
    property PCUFilename: string read FPCUFilename;
    property PCUFormat: TPas2JSPrecompileFormat read FPCUFormat write FPCUFormat;
    property PCUReader: TPCUCustomReader read FPCUReader;
    property PCUReaderStream: TStream read FPCUReaderStream;
    property Scanner: TPascalScanner read FScanner;
    property ShowDebug: boolean read FShowDebug write FShowDebug;
    property UseAnalyzer: TPasAnalyzer read FUseAnalyzer; // unit analysis
    property UsedByCount[Section: TUsedBySection]: integer read GetUsedByCount;
    property UsedBy[Section: TUsedBySection; Index: integer]: TPas2jsCompilerFile read GetUsedBy;
  end;

  { TPas2JSWPOptimizer }

  TPas2JSWPOptimizer = class(TPasAnalyzer)
  end;

  { TPas2jsCompiler }

  TPas2jsCompiler = class
  private
    FCompilerExe: string;
    FConditionEval: TCondDirectiveEvaluator;
    FCurrentCfgFilename: string;
    FCurrentCfgLineNumber: integer;
    FDefines: TStrings; // Objects can be TMacroDef
    FDirectoryCache: TPas2jsCachedDirectories;
    FFileCache: TPas2jsFilesCache;
    FFileCacheAutoFree: boolean;
    FFiles: TAVLTree; // tree of TPas2jsCompilerFile sorted for PasFilename
    FReadingModules: TFPList; // list of TPas2jsCompilerFile ordered by uses sections
    FHasShownEncoding: boolean;
    FHasShownLogo: boolean;
    FLog: TPas2jsLogger;
    FMainFile: TPas2jsCompilerFile;
    FMode: TP2jsMode;
    FOptions: TP2jsCompilerOptions;
    FParamMacros: TPas2jsMacroEngine;
    FSrcMapSourceRoot: string;
    FTargetPlatform: TPasToJsPlatform;
    FTargetProcessor: TPasToJsProcessor;
    FUnits: TAVLTree; // tree of TPas2jsCompilerFile sorted for UnitName
    FWPOAnalyzer: TPas2JSWPOptimizer;
    FPrecompileGUID: TGUID;
    procedure ConditionEvalLog(Sender: TCondDirectiveEvaluator;
      Args: array of const);
    function ConditionEvalVariable(Sender: TCondDirectiveEvaluator;
      aName: String; out Value: string): boolean;
    function GetDefaultNamespace: String;
    function GetFileCount: integer;
    function GetShowDebug: boolean;
    function GetShowFullPaths: boolean;
    function GetShowLogo: Boolean; inline;
    function GetShowTriedUsedFiles: boolean;
    function GetShowUsedTools: boolean; inline;
    function GetSkipDefaultConfig: Boolean; inline;
    function GetSrcMapBaseDir: string;
    function GetSrcMapEnable: boolean;
    function GetSrcMapInclude: boolean;
    function GetSrcMapXSSIHeader: boolean;
    function GetWriteDebugLog: boolean;
    function GetWriteMsgToStdErr: boolean;
    function OnMacroCfgDir(Sender: TObject; var Params: string; Lvl: integer
      ): boolean;
    function OnMacroEnv(Sender: TObject; var Params: string; Lvl: integer
      ): boolean;
    procedure SetCompilerExe(AValue: string);
    procedure SetFileCache(AValue: TPas2jsFilesCache);
    procedure SetMode(AValue: TP2jsMode);
    procedure SetOptions(AValue: TP2jsCompilerOptions);
    procedure SetShowDebug(AValue: boolean);
    procedure SetShowFullPaths(AValue: boolean);
    procedure SetShowLogo(AValue: Boolean);
    procedure SetShowTriedUsedFiles(AValue: boolean);
    procedure SetShowUsedTools(AValue: boolean);
    procedure SetSkipDefaultConfig(AValue: Boolean);
    procedure SetSrcMapBaseDir(const AValue: string);
    procedure SetSrcMapEnable(const AValue: boolean);
    procedure SetSrcMapInclude(const AValue: boolean);
    procedure SetSrcMapXSSIHeader(const AValue: boolean);
    procedure SetTargetPlatform(const AValue: TPasToJsPlatform);
    procedure SetTargetProcessor(const AValue: TPasToJsProcessor);
    procedure SetWriteDebugLog(const AValue: boolean);
    procedure SetWriteMsgToStdErr(const AValue: boolean);
  private
    FInterfaceType: TPasClassInterfaceType;
    FPrecompileInitialFlags: TPCUInitialFlags;
    procedure AddDefinesForTargetPlatform;
    procedure AddDefinesForTargetProcessor;
    procedure AddReadingModule(aFile: TPas2jsCompilerFile);
    procedure RemoveReadingModule(aFile: TPas2jsCompilerFile);
    procedure CfgSyntaxError(const Msg: string);
    procedure LoadConfig(CfgFilename: string);
    procedure LoadDefaultConfig;
    procedure ParamFatal(Msg: string);
    procedure ReadParam(Param: string; Quick, FromCmdLine: boolean);
    procedure ReadSingleLetterOptions(const Param: string; p: PChar;
      const Allowed: string; out Enabled, Disabled: string);
    procedure ReadCodeGenerationFlags(Param: String; p: PChar);
    procedure ReadSyntaxFlags(Param: String; p: PChar);
    procedure ReadVerbosityFlags(Param: String; p: PChar);
    procedure RegisterMessages;
  protected
    // DoWriteJSFile: return false to use the default write function.
    function DoWriteJSFile(const DestFilename: String; aWriter: TPas2JSMapper): Boolean; virtual;
    procedure Compile(StartTime: TDateTime);
    procedure ProcessQueue;
    function MarkNeedBuilding(aFile: TPas2jsCompilerFile; Checked: TAVLTree;
      var SrcFileCount: integer): boolean;
    procedure OptimizeProgram(aFile: TPas2jsCompilerFile); virtual;
    function CreatePrecompileFilename(aFile: TPas2jsCompilerFile): string; virtual;
    procedure CreateJavaScript(aFile: TPas2jsCompilerFile; Checked: TAVLTree);
    procedure FinishSrcMap(SrcMap: TPas2JSSrcMap); virtual;
    procedure WriteJSFiles(aFile: TPas2jsCompilerFile;
      var CombinedFileWriter: TPas2JSMapper; Checked: TAVLTree);
    procedure InitParamMacros;
    procedure ClearDefines;
    procedure RaiseInternalError(id: int64; Msg: string);
    function GetExitCode: Longint; virtual;
    procedure SetExitCode(Value: Longint); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Reset;
    procedure Run(
      aCompilerExe: string; // needed for default config and help
      aWorkingDir: string;
      ParamList: TStrings;
      DoReset: boolean = true);
    procedure Terminate(TheExitCode: integer);

    class function GetVersion(ShortVersion: boolean): string;
    procedure WriteHelp;
    procedure WriteLogo;
    procedure WriteEncoding;
    procedure WriteVersionLine;
    procedure WriteOptions;
    procedure WriteDefines;
    procedure WriteFoldersAndSearchPaths;
    procedure WriteInfo;
    function GetShownMsgTypes: TMessageTypes;

    procedure AddDefine(const aName: String);
    procedure AddDefine(const aName, Value: String);
    procedure RemoveDefine(const aName: String);
    function IsDefined(const aName: String): boolean;
    procedure SetOption(Flag: TP2jsCompilerOption; Enable: boolean);

    function FindUnitWithFile(PasFilename: string): TPas2jsCompilerFile;
    procedure LoadPasFile(UnitFilename, UseUnitName: string;
        out aFile: TPas2jsCompilerFile; aFormat: TPas2JSPrecompileFormat = nil);
    function FindUnitWithName(const TheUnitName: string): TPas2jsCompilerFile;
    procedure AddUsedUnit(aFile: TPas2jsCompilerFile);

    function DirectoryExists(const Filename: string): boolean;
    function ExpandFileName(const Filename: string): string;
  public
    property CompilerExe: string read FCompilerExe write SetCompilerExe;
    property ConditionEvaluator: TCondDirectiveEvaluator read FConditionEval;
    property CurrentCfgFilename: string read FCurrentCfgFilename;
    property CurrentCfgLineNumber: integer read FCurrentCfgLineNumber;
    property DefaultNamespace: String read GetDefaultNamespace;
    property Defines: TStrings read FDefines;
    property DirectoryCache: TPas2jsCachedDirectories read FDirectoryCache;
    property FileCache: TPas2jsFilesCache read FFileCache write SetFileCache;
    property FileCacheAutoFree: boolean read FFileCacheAutoFree write FFileCacheAutoFree;
    property FileCount: integer read GetFileCount;
    property InterfaceType: TPasClassInterfaceType read FInterfaceType write FInterfaceType;
    property Log: TPas2jsLogger read FLog;
    property MainFile: TPas2jsCompilerFile read FMainFile;
    property Mode: TP2jsMode read FMode write SetMode;
    property Options: TP2jsCompilerOptions read FOptions write SetOptions;
    property ParamMacros: TPas2jsMacroEngine read FParamMacros;
    property PrecompileGUID: TGUID read FPrecompileGUID write FPrecompileGUID;
    property PrecompileInitialFlags: TPCUInitialFlags read FPrecompileInitialFlags;
    property SrcMapEnable: boolean read GetSrcMapEnable write SetSrcMapEnable;
    property SrcMapSourceRoot: string read FSrcMapSourceRoot write FSrcMapSourceRoot;
    property SrcMapBaseDir: string read GetSrcMapBaseDir write SetSrcMapBaseDir;
    property SrcMapInclude: boolean read GetSrcMapInclude write SetSrcMapInclude;
    property SrcMapXSSIHeader: boolean read GetSrcMapXSSIHeader write SetSrcMapXSSIHeader;
    property ShowDebug: boolean read GetShowDebug write SetShowDebug;
    property ShowFullPaths: boolean read GetShowFullPaths write SetShowFullPaths;
    property ShowLogo: Boolean read GetShowLogo write SetShowLogo;
    property ShowTriedUsedFiles: boolean read GetShowTriedUsedFiles write SetShowTriedUsedFiles;
    property ShowUsedTools: boolean read GetShowUsedTools write SetShowUsedTools;
    property SkipDefaultConfig: Boolean read GetSkipDefaultConfig write SetSkipDefaultConfig;
    property TargetPlatform: TPasToJsPlatform read FTargetPlatform write SetTargetPlatform;
    property TargetProcessor: TPasToJsProcessor read FTargetProcessor write SetTargetProcessor;
    property WPOAnalyzer: TPas2JSWPOptimizer read FWPOAnalyzer; // Whole Program Optimization
    property WriteDebugLog: boolean read GetWriteDebugLog write SetWriteDebugLog;
    property WriteMsgToStdErr: boolean read GetWriteMsgToStdErr write SetWriteMsgToStdErr;
    property ExitCode: longint read GetExitCode write SetExitCode;
  end;

function CompareCompilerFilesPasFile(Item1, Item2: Pointer): integer;
function CompareFileAndCompilerFilePasFile(Filename, Item: Pointer): integer;
function CompareCompilerFilesPasUnitname(Item1, Item2: Pointer): integer;
function CompareUnitnameAndCompilerFile(TheUnitname, Item: Pointer): integer;

function GetCompiledDate: string;
function GetCompiledFPCVersion: string;
function GetCompiledTargetOS: string;
function GetCompiledTargetCPU: string;

implementation

function CompareCompilerFilesPasFile(Item1, Item2: Pointer): integer;
var
  File1: TPas2jsCompilerFile absolute Item1;
  File2: TPas2jsCompilerFile absolute Item2;
begin
  Result:=CompareFilenames(File1.PasFilename,File2.PasFilename);
end;

function CompareFileAndCompilerFilePasFile(Filename, Item: Pointer): integer;
var
  aFile: TPas2jsCompilerFile absolute Item;
  aFilename: String;
begin
  aFilename:=AnsiString(Filename);
  Result:=CompareFilenames(aFilename,aFile.PasFilename);
end;

function CompareCompilerFilesPasUnitname(Item1, Item2: Pointer): integer;
var
  File1: TPas2jsCompilerFile absolute Item1;
  File2: TPas2jsCompilerFile absolute Item2;
begin
  Result:=CompareText(File1.PasUnitName,File2.PasUnitName);
end;

function CompareUnitnameAndCompilerFile(TheUnitname, Item: Pointer): integer;
var
  aFile: TPas2jsCompilerFile absolute Item;
  anUnitname: String;
begin
  anUnitname:=AnsiString(TheUnitname);
  Result:=CompareText(anUnitname,aFile.PasUnitName);
end;

function GetCompiledDate: string;
begin
  Result:={$I %Date%};
end;

function GetCompiledFPCVersion: string;
begin
  Result:={$I %FPCVERSION%};
end;

function GetCompiledTargetOS: string;
begin
  Result:=lowerCase({$I %FPCTARGETOS%});
end;

function GetCompiledTargetCPU: string;
begin
  Result:=lowerCase({$I %FPCTARGETCPU%});
end;


{ TPas2jsMacroEngine }

function TPas2jsMacroEngine.GetMacros(Index: integer): TPas2jsMacro;
begin
  Result:=TPas2jsMacro(fMacros[Index]);
end;

constructor TPas2jsMacroEngine.Create;
begin
  fMacros:=TObjectList.Create(true);
  FMaxLevel:=10;
end;

destructor TPas2jsMacroEngine.Destroy;
begin
  FreeAndNil(fMacros);
  inherited Destroy;
end;

function TPas2jsMacroEngine.Count: integer;
begin
  Result:=fMacros.Count;
end;

function TPas2jsMacroEngine.AddValue(const aName, aDescription, aValue: string
  ): TPas2jsMacro;
begin
  if not IsValidIdent(aName) then
    raise EPas2jsMacro.Create('invalid macro name "'+aName+'"');
  if IndexOf(aName)>=0 then
    raise EPas2jsMacro.Create('duplicate macro name "'+aName+'"');
  Result:=TPas2jsMacro.Create;
  Result.Name:=aName;
  Result.Description:=aDescription;
  Result.Value:=aValue;
  fMacros.Add(Result);
end;

function TPas2jsMacroEngine.AddFunction(const aName, aDescription: string;
  const OnSubstitute: TOnSubstituteMacro; CanHaveParams: boolean): TPas2jsMacro;
begin
  if not IsValidIdent(aName) then
    raise EPas2jsMacro.Create('invalid macro name "'+aName+'"');
  if IndexOf(aName)>=0 then
    raise EPas2jsMacro.Create('duplicate macro name "'+aName+'"');
  Result:=TPas2jsMacro.Create;
  Result.Name:=aName;
  Result.Description:=aDescription;
  Result.CanHaveParams:=CanHaveParams;
  Result.OnSubstitute:=OnSubstitute;
  fMacros.Add(Result);
end;

function TPas2jsMacroEngine.IndexOf(const aName: string): integer;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    if CompareText(Macros[i].Name,aName)=0 then
      exit(i);
  Result:=-1;
end;

procedure TPas2jsMacroEngine.Delete(Index: integer);
begin
  fMacros.Delete(Index);
end;

function TPas2jsMacroEngine.FindMacro(const aName: string): TPas2jsMacro;
var
  i: Integer;
begin
  i:=IndexOf(aName);
  if i>=0 then
    Result:=Macros[i]
  else
    Result:=nil;
end;

procedure TPas2jsMacroEngine.Substitute(var s: string; Sender: TObject;
  Lvl: integer);
// Rules:
//   $macro or $macro$
// if Macro.OnSubstitute is set then optional brackets are allowed: $macro(params)
var
  p, StartP, BracketLvl, ParamStartP: Integer;
  MacroName, NewValue: String;
  Macro: TPas2jsMacro;
begin
  if Lvl>=MaxLevel then
    raise EPas2jsMacro.Create('macro cycle detected: "'+s+'"');
  p:=1;
  while p<length(s) do begin
    if (s[p]='$') and (s[p+1] in ['_','a'..'z','A'..'Z']) then
    begin
      StartP:=p;
      inc(p,2);
      while (p<=length(s)) and (s[p] in ['_','a'..'z','A'..'Z','0'..'9']) do
        inc(p);
      MacroName:=copy(s,StartP+1,p-StartP-1);
      Macro:=FindMacro(MacroName);
      if Macro=nil then
        raise EPas2jsMacro.Create('macro not found "'+MacroName+'" in "'+s+'"');
      NewValue:='';
      if Macro.CanHaveParams and (p<=length(s)) and (s[p]='(') then
      begin
        // read NewValue
        inc(p);
        ParamStartP:=p;
        BracketLvl:=1;
        repeat
          if p>length(s) then
            raise EPas2jsMacro.Create('missing closing bracket ) in "'+s+'"');
          case s[p] of
          '(': inc(BracketLvl);
          ')':
            if BracketLvl=1 then
            begin
              NewValue:=copy(s,ParamStartP,p-ParamStartP);
              break;
            end else begin
              dec(BracketLvl);
            end;
          end;
        until false;
      end else if (p<=length(s)) and (s[p]='$') then
        inc(p);
      if Assigned(Macro.OnSubstitute) then
      begin
        if not Macro.OnSubstitute(Sender,NewValue,Lvl+1) then
          raise EPas2jsMacro.Create('macro "'+MacroName+'" failed in "'+s+'"');
      end else
        NewValue:=Macro.Value;
      s:=LeftStr(s,StartP-1)+NewValue+copy(s,p,length(s));
      p:=StartP;
    end;
    inc(p);
  end;
end;

{ TPas2jsCompilerFile }

constructor TPas2jsCompilerFile.Create(aCompiler: TPas2jsCompiler;
  const aPasFilename: string; aFormat: TPas2JSPrecompileFormat);
var
  ub: TUsedBySection;
begin
  FCompiler:=aCompiler;
  FPasFilename:=aPasFilename;
  FPCUFormat:=aFormat;
  if aFormat<>nil then
    FPCUFilename:=aPasFilename;
  FLog:=Compiler.Log;
  FPasResolver:=TPas2jsCompilerResolver.Create;
  FPasResolver.Owner:=Self;
  FPasResolver.OnFindModule:=@OnResolverFindModule;
  FPasResolver.OnCheckSrcName:=@OnResolverCheckSrcName;
  FPasResolver.OnLog:=@OnPasResolverLog;
  FPasResolver.Log:=Log;
  FPasResolver.AddObjFPCBuiltInIdentifiers(btAllJSBaseTypes,bfAllJSBaseProcs);
  FIsMainFile:=CompareFilenames(Compiler.FileCache.MainSrcFile,PasFilename)=0;
  for ub in TUsedBySection do
    FUsedBy[ub]:=TFPList.Create;
  FUseAnalyzer:=TPasAnalyzer.Create;
  FUseAnalyzer.OnMessage:=@OnUseAnalyzerMessage;
  FUseAnalyzer.Resolver:=FPasResolver;
end;

destructor TPas2jsCompilerFile.Destroy;
var
  ub: TUsedBySection;
begin
  FreeAndNil(FPCUReader);
  FreeAndNil(FPCUReaderStream);
  FreeAndNil(FUseAnalyzer);
  for ub in TUsedBySection do
    FreeAndNil(FUsedBy[ub]);
  FreeAndNil(FJSModule);
  FreeAndNil(FConverter);
  FreeAndNil(FParser);
  FreeAndNil(FScanner);
  FreeAndNil(FFileResolver);
  FreeAndNil(FPasResolver);
  if FPasModule<>nil then
    FPasModule.ReleaseUsedUnits;
  ReleaseAndNil(TPasElement(FPasModule){$IFDEF CheckPasTreeRefCount},'CreateElement'{$ENDIF});
  inherited Destroy;
end;

function TPas2jsCompilerFile.GetInitialModeSwitches: TModeSwitches;
begin
  Result:=p2jsMode_SwitchSets[Compiler.Mode];
end;

function TPas2jsCompilerFile.GetInitialBoolSwitches: TBoolSwitches;
var
  bs: TBoolSwitches;
begin
  bs:=[bsLongStrings,bsWriteableConst];
  if coAllowMacros in Compiler.Options then
    Include(bs,bsMacro);
  if coOverflowChecks in Compiler.Options then
    Include(bs,bsOverflowChecks);
  if coRangeChecks in Compiler.Options then
    Include(bs,bsRangeChecks);
  if coObjectChecks in Compiler.Options then
    Include(bs,bsObjectChecks);
  if coAssertions in Compiler.Options then
    Include(bs,bsAssertions);
  if coShowHints in Compiler.Options then
    Include(bs,bsHints);
  if coShowNotes in Compiler.Options then
    Include(bs,bsNotes);
  if coShowWarnings in Compiler.Options then
    Include(bs,bsWarnings);
  Result:=bs;
end;

function TPas2jsCompilerFile.
  GetInitialConverterOptions: TPasToJsConverterOptions;
begin
  Result:=DefaultPasToJSOptions;

  if coUseStrict in Compiler.Options then
    Include(Result,fppas2js.coUseStrict)
  else
    Exclude(Result,fppas2js.coUseStrict);

  if coEnumValuesAsNumbers in Compiler.Options then
    Include(Result,fppas2js.coEnumNumbers);

  if coLowerCase in Compiler.Options then
    Include(Result,fppas2js.coLowerCase)
  else
    Exclude(Result,fppas2js.coLowerCase);
end;

procedure TPas2jsCompilerFile.CreateScannerAndParser(aFileResolver: TPas2jsFileResolver);
var
  aUnitName: String;
  i: Integer;
  M: TMacroDef;
begin
  FFileResolver:=aFileResolver;
  // scanner
  if FScanner<>nil then
    RaiseInternalError(20180707193258,PasFilename);
  FScanner := TPascalScanner.Create(FileResolver);
  Scanner.LogEvents:=PascalResolver.ScannerLogEvents;
  Scanner.OnLog:=@OnScannerLog;
  Scanner.OnFormatPath:=@Compiler.FileCache.FormatPath;

  // create parser (Note: this sets some scanner options to defaults)
  FParser := TPas2jsPasParser.Create(Scanner, FileResolver, PascalResolver);

  // set options
  Scanner.Options:=Scanner.Options+[po_StopOnErrorDirective];
  Scanner.AllowedModeSwitches:=msAllPas2jsModeSwitches;
  Scanner.ReadOnlyModeSwitches:=msAllPas2jsModeSwitchesReadOnly;
  Scanner.CurrentModeSwitches:=GetInitialModeSwitches;
  Scanner.AllowedBoolSwitches:=msAllPas2jsBoolSwitches;
  Scanner.ReadOnlyBoolSwitches:=msAllPas2jsBoolSwitchesReadOnly;
  Scanner.CurrentBoolSwitches:=GetInitialBoolSwitches;
  Scanner.CurrentValueSwitch[vsInterfaces]:=InterfaceTypeNames[Compiler.InterfaceType];
  if coAllowCAssignments in Compiler.Options then
    Scanner.Options:=Scanner.Options+[po_cassignments];
  if Compiler.Mode=p2jmDelphi then
    Scanner.Options:=Scanner.Options+[po_delphi];
  // Note: some Scanner.Options are set by TPasResolver
  for i:=0 to Compiler.Defines.Count-1 do
    begin
    M:=TMacroDef(Compiler.Defines.Objects[i]);
    if M=nil then
      Scanner.AddDefine(Compiler.Defines[i])
    else
      Scanner.AddMacro(M.Name,M.Value);
    end;

  // parser
  Parser.LogEvents:=PascalResolver.ParserLogEvents;
  Parser.OnLog:=@OnParserLog;
  Parser.Log:=Log;
  PascalResolver.P2JParser:=Parser;

  if not IsMainFile then
  begin
    aUnitName:=ExtractFilenameOnly(PasFilename);
    if CompareText(aUnitName,'system')=0 then
      Parser.ImplicitUses.Clear;
  end;
end;

procedure TPas2jsCompilerFile.CreatePCUReader;
var
  aFile: TPas2jsCachedFile;
  s: String;
begin
  if PCUFilename='' then
    RaiseInternalError(20180312144742,PCUFilename);
  if PCUReader<>nil then
    RaiseInternalError(20180312142938,GetObjName(PCUReader));
  if PCUFormat=nil then
    RaiseInternalError(20180312142954,'');
  FPCUReader:=PCUFormat.ReaderClass.Create;
  FPCUReader.SourceFilename:=ExtractFileName(PCUFilename);

  if ShowDebug then
    Log.LogMsg(nParsingFile,[QuoteStr(PCUFilename)]);
  aFile:=Compiler.FileCache.LoadFile(PCUFilename,true);
  if aFile=nil then
    RaiseInternalError(20180312145941,PCUFilename);
  FPCUReaderStream:=TMemoryStream.Create;
  s:=aFile.Source;
  //writeln('TPas2jsCompilerFile.CreatePCUReader ',PCUFilename,'-----START-----');
  //writeln(s);
  //writeln('TPas2jsCompilerFile.CreatePCUReader ',PCUFilename,'-----END-------');
  if s<>'' then
  begin
    PCUReaderStream.Write(s[1],length(s));
    PCUReaderStream.Position:=0;
  end;
end;

procedure TPas2jsCompilerFile.CreateConverter;
begin
  if FConverter<>nil then exit;
  FConverter:=TPasToJSConverter.Create;
  FConverter.Options:=GetInitialConverterOptions;
  FConverter.TargetPlatform:=Compiler.TargetPlatform;
  FConverter.TargetProcessor:=Compiler.TargetProcessor;
end;

procedure TPas2jsCompilerFile.OnResolverCheckSrcName(const Element: TPasElement);
var
  SrcName, ExpectedSrcName: String;
begin
  //writeln('TPas2jsCompilerFile.OnPasTreeCheckSrcName ',PasFilename,' Name=',Element.Name,' IsMainFile=',IsMainFile);
  if (Element.ClassType=TPasUnitModule) or (Element.ClassType=TPasModule) then
  begin
    SrcName:=Element.Name;
    if IsMainFile then
    begin
      // main source is an unit
      if PasUnitName='' then
      begin
        {$IFDEF VerboseSetPasUnitName}
        writeln('TPas2jsCompilerFile.OnPasTreeCheckSrcName ',PasFilename,' Name=',Element.Name,' IsMainFile=',IsMainFile);
        {$ENDIF}
        PasUnitName:=SrcName;
        Compiler.AddUsedUnit(Self);
      end;
    end else begin
      // an unit name must fit its filename
      ExpectedSrcName:=ExtractFilenameOnly(PasFilename);
      if CompareText(SrcName,ExpectedSrcName)=0 then
        exit; // ok
      Parser.RaiseParserError(nExpectedButFound,[ExpectedSrcName,SrcName]);
    end;
  end;
end;

function TPas2jsCompilerFile.GetUsedBy(Section: TUsedBySection; Index: integer
  ): TPas2jsCompilerFile;
begin
  Result:=TPas2jsCompilerFile(FUsedBy[Section][Index]);
end;

function TPas2jsCompilerFile.GetUsedByCount(Section: TUsedBySection): integer;
begin
  Result:=FUsedBy[Section].Count;
end;

function TPas2jsCompilerFile.OnConverterIsElementUsed(Sender: TObject;
  El: TPasElement): boolean;
begin
  if (Compiler.WPOAnalyzer<>nil)
      and not (coKeepNotUsedDeclarationsWPO in Compiler.Options) then
    Result:=Compiler.WPOAnalyzer.IsUsed(El)
  else if not (coKeepNotUsedPrivates in Compiler.Options) then
    Result:=UseAnalyzer.IsUsed(El)
  else
    Result:=true;
end;

function TPas2jsCompilerFile.OnConverterIsTypeInfoUsed(Sender: TObject;
  El: TPasElement): boolean;
begin
  if (Compiler.WPOAnalyzer<>nil)
      and not (coKeepNotUsedDeclarationsWPO in Compiler.Options) then
    Result:=Compiler.WPOAnalyzer.IsTypeInfoUsed(El)
  else if not (coKeepNotUsedPrivates in Compiler.Options) then
    Result:=UseAnalyzer.IsTypeInfoUsed(El)
  else
    Result:=true;
end;

procedure TPas2jsCompilerFile.OnFilerGetSrc(Sender: TObject; aFilename: string;
  out p: PChar; out Count: integer);
var
  SrcFile: TPas2jsCachedFile;
begin
  if Sender=nil then
    RaiseInternalError(20180311135558,aFilename);
  SrcFile:=Compiler.FileCache.LoadFile(aFilename);
  if SrcFile=nil then
    RaiseInternalError(20180311135329,aFilename);
  p:=PChar(SrcFile.Source);
  Count:=length(SrcFile.Source);
end;

procedure TPas2jsCompilerFile.OnPasResolverLog(Sender: TObject; const Msg: String);
var
  aResolver: TPasResolver;
begin
  if Msg='' then ; // ignore standard formatted message
  aResolver:=TPasResolver(Sender);
  DoLogMsgAtEl(aResolver.LastMsgType,aResolver.LastMsg,aResolver.LastMsgNumber,
          aResolver.LastElement);
end;

procedure TPas2jsCompilerFile.OnParserLog(Sender: TObject; const Msg: String);
var
  aParser: TPasParser;
  aScanner: TPascalScanner;
begin
  if Msg='' then ; // ignore standard formatted message
  aParser:=TPasParser(Sender);
  aScanner:=aParser.Scanner;
  Log.Log(aParser.LastMsgType,aParser.LastMsg,aParser.LastMsgNumber,
          aScanner.CurFilename,aScanner.CurRow,aScanner.CurColumn);
end;

function TPas2jsCompilerFile.OnPCUConverterIsElementUsed(Sender: TObject;
  El: TPasElement): boolean;
begin
  if (coKeepNotUsedPrivates in Compiler.Options) then
    Result:=true
  else
    Result:=UseAnalyzer.IsUsed(El);
end;

function TPas2jsCompilerFile.OnPCUConverterIsTypeInfoUsed(Sender: TObject;
  El: TPasElement): boolean;
begin
  if Sender=nil then ;
  if El=nil then ;
  // PCU does not need precompiled typeinfo
  Result:=false;
end;

procedure TPas2jsCompilerFile.OnScannerLog(Sender: TObject; const Msg: String);
var
  aScanner: TPascalScanner;
begin
  if Msg='' then ; // ignore standard formatted message
  aScanner:=TPascalScanner(Sender);
  Log.Log(aScanner.LastMsgType,aScanner.LastMsg,aScanner.LastMsgNumber,
          aScanner.CurFilename,aScanner.CurRow,aScanner.CurColumn);
end;

procedure TPas2jsCompilerFile.OnUseAnalyzerMessage(Sender: TObject;
  Msg: TPAMessage);
begin
  Log.Log(Msg.MsgType,Msg.MsgText,Msg.MsgNumber,Msg.Filename,Msg.Row,Msg.Col);
end;

procedure TPas2jsCompilerFile.HandleEParserError(E: EParserError);
begin
  Log.Log(Parser.LastMsgType,Parser.LastMsg,Parser.LastMsgNumber,
          E.Filename,E.Row,E.Column);
  Compiler.Terminate(ExitCodeSyntaxError);
end;

procedure TPas2jsCompilerFile.HandleEPasResolve(E: EPasResolve);
var
  aFilename: String;
  aRow, aColumn: integer;
begin
  if E.PasElement<>nil then
  begin
    aFilename:=E.PasElement.SourceFilename;
    PascalResolver.UnmangleSourceLineNumber(E.PasElement.SourceLinenumber,aRow,aColumn);
  end else begin
    aFilename:=Scanner.CurFilename;
    aRow:=Scanner.CurRow;
    aColumn:=Scanner.CurColumn;
  end;
  Log.Log(E.MsgType,E.Message,E.MsgNumber,aFilename,aRow,aColumn);
  Compiler.Terminate(ExitCodeSyntaxError);
end;

procedure TPas2jsCompilerFile.HandleEPas2JS(E: EPas2JS);
var
  aFilename: String;
  aRow, aColumn: integer;
begin
  if E.PasElement<>nil then
  begin
    aFilename:=E.PasElement.SourceFilename;
    PascalResolver.UnmangleSourceLineNumber(E.PasElement.SourceLinenumber,aRow,aColumn);
    Log.Log(E.MsgType,E.Message,E.MsgNumber,aFilename,aRow,aColumn);
  end else begin
    Log.Log(E.MsgType,E.Message,E.MsgNumber);
  end;
  Compiler.Terminate(ExitCodeConverterError);
end;

procedure TPas2jsCompilerFile.HandleEPCUReader(E: EPas2JsReadError);
begin
  if E.Owner is TPCUCustomReader then
  begin
    Log.Log(mtError,E.Message,0,PCUFilename);
  end else begin
    Log.Log(mtError,E.Message);
  end;
  Compiler.Terminate(ExitCodePCUError);
end;

procedure TPas2jsCompilerFile.HandleUnknownException(E: Exception);
begin
  if not (E is ECompilerTerminate) then
    Log.Log(mtFatal,'bug: uncaught '+E.ClassName+': '+E.Message,0); // must use on E:ECompilerTerminate do raise;
  Log.Log(mtFatal,E.ClassName+': '+E.Message,0);
  Compiler.Terminate(ExitCodeErrorInternal);
  // Note: a "raise E" is not allowed by caught exceptions, try..except will free it
end;

procedure TPas2jsCompilerFile.HandleException(E: Exception);
begin
  if ShowDebug then
    Log.LogExceptionBackTrace;
  if E is EScannerError then
  begin
    Log.Log(Scanner.LastMsgType,Scanner.LastMsg,Scanner.LastMsgNumber,
            Scanner.CurFilename,Scanner.CurRow,Scanner.CurColumn);
    Compiler.Terminate(ExitCodeSyntaxError);
  end else if E is EParserError then
    HandleEParserError(EParserError(E))
  else if E is EPasResolve then
    HandleEPasResolve(EPasResolve(E))
  else if E is EPas2JS then
    HandleEPas2JS(EPas2JS(E))
  else if E is EPas2JsReadError then
    HandleEPCUReader(EPas2JsReadError(E))
  else if E is EFileNotFoundError then
  begin
    if (E.Message<>'') or (Log.LastMsgType<>mtFatal) then
      Log.Log(mtFatal,E.Message);
    Compiler.Terminate(ExitCodeFileNotFound);
  end
  else if E is EPas2jsFileCache then
  begin
    Log.Log(mtFatal,E.Message);
    Compiler.Terminate(ExitCodeFileNotFound);
  end
  else if (E is EPas2JsWriteError) or (E is EPas2JsReadError) then
  begin
    Log.Log(mtFatal,E.ClassName+':'+E.Message);
    Compiler.Terminate(ExitCodeErrorInternal);
  end
  else
    HandleUnknownException(E);
end;

procedure TPas2jsCompilerFile.DoLogMsgAtEl(MsgType: TMessageType;
  const Msg: string; MsgNumber: integer; El: TPasElement);
var
  Line, Col: integer;
  Filename: String;
begin
  if (El<>nil) then
  begin
    Filename:=El.SourceFilename;
    TPasResolver.UnmangleSourceLineNumber(El.SourceLinenumber,Line,Col);
  end else begin
    Filename:='';
    Line:=0;
    Col:=0;
  end;
  Log.Log(MsgType,Msg,MsgNumber,Filename,Line,Col);
end;

function TPas2jsCompilerFile.OnWriterIsElementUsed(Sender: TObject;
  El: TPasElement): boolean;
begin
  Result:=UseAnalyzer.IsUsed(El);
end;

procedure TPas2jsCompilerFile.RaiseInternalError(id: int64; Msg: string);
begin
  Compiler.RaiseInternalError(id,Msg);
end;

procedure TPas2jsCompilerFile.ReaderFinished;
var
  aPrecompileFormat: TPas2JSPrecompileFormat;
begin
  FReaderState:=prsFinished;
  try
    Compiler.RemoveReadingModule(Self);

    if coWriteDebugLog in Compiler.Options then
    begin
      Log.DebugLogWriteLn('Pas-Module:');
      Log.DebugLogWriteLn(PasModule.GetDeclaration(true));
    end;

    if PCUReader=nil then
      begin
      // read source module (instead of precompiled module)

      // -> analyze module
      aPrecompileFormat:=Compiler.FileCache.PrecompileFormat;
      if aPrecompileFormat<>nil then
        UseAnalyzer.Options:=UseAnalyzer.Options+[paoImplReferences];

      {$IFDEF VerboseUnitQueue}
      writeln('TPas2jsCompilerFile.ReaderFinished analyzing ',PasFilename,' ...');
      {$ENDIF}
      UseAnalyzer.AnalyzeModule(FPasModule);
      {$IF defined(VerboseUnitQueue) or defined(VerbosePCUFiler)}
      writeln('TPas2jsCompilerFile.ReaderFinished analyzed ',PasFilename,' ScopeModule=',GetObjName(UseAnalyzer.ScopeModule));
      {$ENDIF}

      if (aPrecompileFormat<>nil) and (PCUReader=nil) then
        WritePCU;
      end;
  except
    on E: ECompilerTerminate do
      raise;
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TPas2jsCompilerFile.WritePCU;
var
  PF: TPas2JSPrecompileFormat;
  Writer: TPCUWriter;
  ms: TMemoryStream;
  DestDir: String;
  JS: TJSElement;
begin
  if PasModule.ClassType<>TPasModule then
  begin
    {$IF defined(VerboseUnitQueue) or defined(VerbosePCUFiler)}
    writeln('TPas2jsCompilerFile.WritePCU not a unit: ',PasFilename,' skip');
    {$ENDIF}
    exit;
  end;

  if (PCUFilename<>'') or (PCUReader<>nil) then
  begin
    {$IF defined(VerboseUnitQueue) or defined(VerbosePCUFiler)}
    writeln('TPas2jsCompilerFile.WritePCU already precompiled "',PCUFilename,'" Reader=',GetObjName(PCUReader));
    {$ENDIF}
    exit;
  end;
  FPCUFilename:=Compiler.CreatePrecompileFilename(Self);
  FPCUFormat:=Compiler.FileCache.PrecompileFormat;

  {$IF defined(VerboseUnitQueue) or defined(VerbosePCUFiler)}
  writeln('TPas2jsCompilerFile.WritePCU precompiling ',PCUFilename);
  {$ENDIF}

  JS:=nil;
  PF:=Compiler.FileCache.PrecompileFormat;
  ms:=TMemoryStream.Create;
  Writer:=PF.WriterClass.Create;
  try
    Writer.GUID:=Compiler.PrecompileGUID;
    Writer.OnGetSrc:=@OnFilerGetSrc;
    Writer.OnIsElementUsed:=@OnWriterIsElementUsed;

    // create JavaScript for procs, initialization, finalization
    CreateConverter;
    Converter.Options:=Converter.Options+[coStoreImplJS];
    Converter.OnIsElementUsed:=@OnPCUConverterIsElementUsed;
    Converter.OnIsTypeInfoUsed:=@OnPCUConverterIsTypeInfoUsed;
    JS:=Converter.ConvertPasElement(PasModule,PascalResolver);
    Converter.Options:=Converter.Options-[coStoreImplJS];
    {$IF defined(VerboseUnitQueue) or defined(VerbosePCUFiler)}
    writeln('TPas2jsCompilerFile.WritePCU create pcu ... ',PCUFilename);
    {$ENDIF}
    Writer.WritePCU(PascalResolver,Converter,Compiler.PrecompileInitialFlags,ms,
      {$IFDEF DisablePCUCompressed}false{$ELSE}true{$ENDIF});
    {$IF defined(VerboseUnitQueue) or defined(VerbosePCUFiler)}
    writeln('TPas2jsCompilerFile.WritePCU precompiled ',PCUFilename);
    {$ENDIF}

    Log.LogMsg(nWritingFile,[QuoteStr(Compiler.FileCache.FormatPath(PCUFilename))],'',0,0,
               not (coShowLineNumbers in Compiler.Options));

    // check output directory
    DestDir:=ChompPathDelim(ExtractFilePath(PCUFilename));
    if (DestDir<>'') and not Compiler.DirectoryExists(DestDir) then
    begin
      {$IF defined(VerboseUnitQueue) or defined(VerbosePCUFiler)}
      writeln('TPas2jsCompilerFile.WritePCU output dir not found "',DestDir,'"');
      {$ENDIF}
      Log.LogMsg(nOutputDirectoryNotFound,[QuoteStr(Compiler.FileCache.FormatPath(DestDir))]);
      Compiler.Terminate(ExitCodeFileNotFound);
    end;
    if Compiler.DirectoryExists(PCUFilename) then
    begin
      {$IF defined(VerboseUnitQueue) or defined(VerbosePCUFiler)}
      writeln('TPas2jsCompilerFile.WritePCU file is folder "',DestDir,'"');
      {$ENDIF}
      Log.LogMsg(nFileIsFolder,[QuoteStr(Compiler.FileCache.FormatPath(PCUFilename))]);
      Compiler.Terminate(ExitCodeWriteError);
    end;

    ms.Position:=0;
    Compiler.FileCache.SaveToFile(ms,PCUFilename);
    {$IF defined(VerboseUnitQueue) or defined(VerbosePCUFiler)}
    writeln('TPas2jsCompilerFile.WritePCU written ',PCUFilename);
    {$ENDIF}
  finally
    JS.Free;
    Writer.Free;
    ms.Free;
  end;
end;

procedure TPas2jsCompilerFile.OpenFile(aFilename: string);
begin
  FPasFilename:=aFilename;
  try
    Scanner.OpenFile(PasFilename);
  except
    on E: ECompilerTerminate do
      raise;
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TPas2jsCompilerFile.ReadUnit;
begin
  if ShowDebug then
    Log.LogMsg(nParsingFile,[QuoteStr(PasFilename)]);
  if FPasModule<>nil then
    Compiler.RaiseInternalError(20180305190321,PasFilename);
  FReaderState:=prsReading;
  try
    {$IFDEF VerboseUnitQueue}
    writeln('TPas2jsCompilerFile.ReadUnit ',PasFilename,' START');
    {$ENDIF}
    Compiler.AddReadingModule(Self);
    PascalResolver.InterfaceOnly:=IsForeign;

    if PCUReader<>nil then
    begin
      PCUReader.ReadPCU(PascalResolver,PCUReaderStream);
      FPasModule:=PascalResolver.RootElement;
      FReaderState:=prsCanContinue;
    end else begin
      if IsMainFile then
        Parser.ParseMain(FPasModule)
      else
        Parser.ParseSubModule(FPasModule);
      if Parser.CurModule=nil then
        ReaderFinished
      else
        FReaderState:=prsWaitingForUsedUnits;
    end;
    {$IFDEF VerboseUnitQueue}
    writeln('TPas2jsCompilerFile.ReadUnit ',PasFilename,' ReaderState=',ReaderState);
    {$ENDIF}
  except
    on E: ECompilerTerminate do
      raise;
    on E: Exception do
      HandleException(E);
  end;
  if FReaderState=prsReading then
    FReaderState:=prsError;
  if (PasModule<>nil) and (PasModule.CustomData=nil) then
    PasModule.CustomData:=Self;
end;

function TPas2jsCompilerFile.ReadContinue: boolean;
begin
  Result:=true;
  if ShowDebug then
    Log.LogPlain(['Debug: Continue reading unit "',PasFilename,'"...']);
  if FPasModule=nil then
    Compiler.RaiseInternalError(20180305190338,PasFilename);
  FReaderState:=prsReading;
  try
    {$IFDEF VerboseUnitQueue}
    writeln('TPas2jsCompilerFile.ReadContinue ',PasFilename);
    {$ENDIF}
    if PCUReader<>nil then
      Result:=PCUReader.ReadContinue
    else
    begin
      Parser.ParseContinue;
      Result:=Parser.CurModule=nil;
    end;
    {$IFDEF VerboseUnitQueue}
    writeln('TPas2jsCompilerFile.ReadContinue ',PasFilename,' finished=',Result);
    {$ENDIF}
    if Result then
      ReaderFinished
    else
      FReaderState:=prsWaitingForUsedUnits;
  except
    on E: ECompilerTerminate do
      raise;
    on E: Exception do
      HandleException(E);
  end;
  if FReaderState=prsReading then
    FReaderState:=prsError;
end;

function TPas2jsCompilerFile.ReaderState: TPas2jsReaderState;
var
  Section: TPasSection;
begin
  Result:=FReaderState;
  if Result=prsWaitingForUsedUnits then
    if PCUReader<>nil then
    begin
      if PCUReader.ReadCanContinue then
        Result:=prsCanContinue;
    end else begin
      if Parser.CanParseContinue(Section) then
        Result:=prsCanContinue;
    end;
end;

procedure TPas2jsCompilerFile.CreateJS;
begin
  try
    // show hints only for units that are actually converted
    if PCUReader=nil then
      begin
      //writeln('TPas2jsCompilerFile.CreateJS ',PasFilename);
      UseAnalyzer.EmitModuleHints(PasModule);
      end;

    // convert
    CreateConverter;
    Converter.OnIsElementUsed:=@OnConverterIsElementUsed;
    Converter.OnIsTypeInfoUsed:=@OnConverterIsTypeInfoUsed;
    FJSModule:=Converter.ConvertPasElement(PasModule,PascalResolver);
  except
    on E: ECompilerTerminate do
      raise;
    on E: Exception do
      HandleException(E);
  end;
end;

function TPas2jsCompilerFile.GetPasFirstSection: TPasSection;
var
  aModule: TPasModule;
begin
  aModule:=GetCurPasModule;
  if aModule=nil then exit;
  if aModule.ClassType=TPasProgram then
    Result:=TPasProgram(aModule).ProgramSection
  else if aModule.ClassType=TPasLibrary then
    Result:=TPasLibrary(aModule).LibrarySection
  else
    Result:=aModule.InterfaceSection;
end;

function TPas2jsCompilerFile.GetPasImplSection: TPasSection;
var
  aModule: TPasModule;
begin
  Result:=nil;
  aModule:=GetCurPasModule;
  if aModule=nil then exit;
  Result:=aModule.ImplementationSection;
end;

function TPas2jsCompilerFile.GetPasMainUsesClause: TPasUsesClause;
var
  aModule: TPasModule;
  IntfSection: TInterfaceSection;
  PrgSection: TProgramSection;
  LibSection: TLibrarySection;
begin
  Result:=nil;
  aModule:=GetCurPasModule;
  if aModule=nil then exit;
  if aModule.ClassType=TPasModule then
  begin
    IntfSection:=TPasModule(aModule).InterfaceSection;
    if IntfSection<>nil then
      Result:=IntfSection.UsesClause;
  end else if aModule.ClassType=TPasProgram then
  begin
    PrgSection:=TPasProgram(aModule).ProgramSection;
    if PrgSection<>nil then
      Result:=PrgSection.UsesClause;
  end else if aModule.ClassType=TPasLibrary then
  begin
    LibSection:=TPasLibrary(aModule).LibrarySection;
    if LibSection<>nil then
      Result:=LibSection.UsesClause;
  end;
end;

function TPas2jsCompilerFile.GetPasImplUsesClause: TPasUsesClause;
var
  aModule: TPasModule;
begin
  Result:=nil;
  aModule:=GetCurPasModule;
  if aModule=nil then exit;
  if aModule.ImplementationSection<>nil then
    Result:=aModule.ImplementationSection.UsesClause;
end;

function TPas2jsCompilerFile.GetCurPasModule: TPasModule;
begin
  if PasModule<>nil then
    Result:=PasModule
  else if (PascalResolver<>nil) and (PascalResolver.RootElement<>nil) then
    Result:=PascalResolver.RootElement
  else if Parser<>nil then
    Result:=Parser.CurModule
  else
    Result:=nil;
end;

function TPas2jsCompilerFile.GetModuleName: string;
var
  aModule: TPasModule;
begin
  aModule:=GetCurPasModule;
  if aModule<>nil then
    Result:=aModule.Name
  else
    Result:='';
  if Result='' then
    Result:=ExtractFilenameOnly(PasFilename);
end;

class function TPas2jsCompilerFile.GetFile(aModule: TPasModule
  ): TPas2jsCompilerFile;
var
  Scope: TPasModuleScope;
  Resolver: TPas2jsCompilerResolver;
begin
  Result:=nil;
  if (aModule=nil) or (aModule.CustomData=nil) then exit;
  if aModule.CustomData is TPas2jsCompilerFile then
    Result:=TPas2jsCompilerFile(aModule.CustomData)
  else if aModule.CustomData is TPasModuleScope then
  begin
    Scope:=TPasModuleScope(aModule.CustomData);
    Resolver:=NoNil(Scope.Owner) as TPas2jsCompilerResolver;
    Result:=Resolver.Owner as TPas2jsCompilerFile;
  end;
end;

function TPas2jsCompilerFile.FindPCU(const UseUnitName: string; out
  aFormat: TPas2JSPrecompileFormat): string;

  function SearchInDir(DirPath: string): boolean;
  var
    i: Integer;
    CurFormat: TPas2JSPrecompileFormat;
    Filename: String;
  begin
    if DirPath='' then exit(false);
    DirPath:=IncludeTrailingPathDelimiter(DirPath);
    for i:=0 to PrecompileFormats.Count-1 do
    begin
      CurFormat:=PrecompileFormats[i];
      if not CurFormat.Enabled then continue;
      Filename:=DirPath+UseUnitName+'.'+CurFormat.Ext;
      if FileResolver.SearchLowUpCase(Filename) then
      begin
        FindPCU:=Filename;
        aFormat:=CurFormat;
        exit(true);
      end;
    end;
    Result:=false;
  end;

var
  Cache: TPas2jsFilesCache;
  i: Integer;
begin
  Result:='';
  aFormat:=nil;
  Cache:=Compiler.FileCache;

  // search in output directory
  if Cache.UnitOutputPath<>'' then
    if SearchInDir(Cache.UnitOutputPath) then exit;

  // then in BaseDirectory
  if SearchInDir(FileResolver.BaseDirectory) then exit;

  // finally search in unit paths
  for i:=0 to Cache.UnitPaths.Count-1 do
    if SearchInDir(Cache.UnitPaths[i]) then exit;
end;

function TPas2jsCompilerFile.OnResolverFindModule(const UseUnitName,
  InFilename: String; NameExpr, InFileExpr: TPasExpr): TPasModule;
var
  FoundPasFilename, FoundPasUnitName, FoundPCUFilename, FoundPCUUnitName: string;
  FoundPasIsForeign: Boolean;
  FoundPCUFormat: TPas2JSPrecompileFormat;

  procedure TryUnitName(const TestUnitName: string);
  var
    aFile: TPas2jsCompilerFile;
  begin
    if FoundPasFilename='' then
    begin
      // search loaded units
      aFile:=Compiler.FindUnitWithName(TestUnitName);
      if aFile<>nil then
      begin
        FoundPasFilename:=aFile.PasFilename;
        FoundPasUnitName:=TestUnitName;
      end else begin
        // search pas in unit path
        FoundPasFilename:=FileResolver.FindUnitFileName(TestUnitName,'',FoundPasIsForeign);
        if FoundPasFilename<>'' then
          FoundPasUnitName:=TestUnitName;
      end;
    end;
    if FoundPCUFilename='' then
    begin
      FoundPCUFilename:=FindPCU(TestUnitName,FoundPCUFormat);
      if FoundPCUFilename<>'' then
        FoundPCUUnitName:=TestUnitName;
    end;
  end;

var
  aNameSpace, DefNameSpace: String;
  i: Integer;
  aFile: TPas2jsCompilerFile;
begin
  Result:=nil;
  if CompareText(ExtractFilenameOnly(PasFilename),UseUnitname)=0 then
  begin
    // duplicate identifier or unit cycle
    Parser.RaiseParserError(nUnitCycle,[UseUnitname]);
  end;

  FoundPasFilename:='';
  FoundPasIsForeign:=false;
  FoundPasUnitName:='';
  FoundPCUFilename:='';
  FoundPCUFormat:=nil;
  FoundPCUUnitName:='';
  if (InFilename='') and (Pos('.',UseUnitname)<1) then
  begin
    // generic unit -> search with namespaces
    // first the default program namespace
    DefNameSpace:=Compiler.GetDefaultNamespace;
    if DefNameSpace<>'' then
      TryUnitName(DefNameSpace+'.'+UseUnitname);

    if (FoundPasFilename='') or (FoundPCUFilename='') then
    begin
      // then the cmdline namespaces
      for i:=0 to Compiler.FileCache.Namespaces.Count-1 do begin
        aNameSpace:=Compiler.FileCache.Namespaces[i];
        if aNameSpace='' then continue;
        if SameText(aNameSpace,DefNameSpace) then continue;
        TryUnitName(aNameSpace+'.'+UseUnitname);
      end;
    end;
  end;

  if FoundPasFilename='' then
  begin
    if InFilename='' then
    begin
      // search unitname in loaded units
      aFile:=Compiler.FindUnitWithName(UseUnitname);
      if aFile<>nil then
      begin
        FoundPasFilename:=aFile.PasFilename;
        FoundPasUnitName:=UseUnitName;
      end;
    end;
    if FoundPasFilename='' then
    begin
      // search Pascal file
      FoundPasFilename:=FileResolver.FindUnitFileName(UseUnitname,InFilename,FoundPasIsForeign);
      if FoundPasFilename<>'' then
        begin
        if InFilename<>'' then
          FoundPasUnitName:=ExtractFilenameOnly(InFilename)
        else
          FoundPasUnitName:=UseUnitName;
        end
      else if InFilename<>'' then
        exit; // an in-filename unit source is missing -> stop
    end;
  end;
  if FoundPCUFilename='' then
  begin
    FoundPCUFilename:=FindPCU(UseUnitName,FoundPCUFormat);
    FoundPCUUnitName:=UseUnitName;
  end;

  if (FoundPCUFilename<>'') and (FoundPasFilename='') then
  begin
    aFile:=LoadUsedUnit(FoundPCUFilename,FoundPCUUnitName,'',NameExpr,nil,false,FoundPCUFormat);
    if aFile<>nil then
      Result:=aFile.PasModule;
    exit;
  end;

  if FoundPasFilename<>'' then
  begin
    // load unit
    aFile:=LoadUsedUnit(FoundPasFilename,FoundPasUnitName,InFilename,
                         NameExpr,InFileExpr,FoundPasIsForeign,nil);
    if aFile<>nil then
      Result:=aFile.PasModule;
  end;
  // if Result=nil resolver will give a nice error position
end;

function TPas2jsCompilerFile.LoadUsedUnit(const UseFilename, UseUnitname,
  InFilename: String; NameExpr, InFileExpr: TPasExpr; UseIsForeign: boolean;
  aFormat: TPas2JSPrecompileFormat): TPas2jsCompilerFile;

  function FindCycle(aFile, SearchFor: TPas2jsCompilerFile;
    var Cycle: TFPList): boolean;
  var
    i: Integer;
    aParent: TPas2jsCompilerFile;
  begin
    for i:=0 to aFile.UsedByCount[ubMainSection]-1 do begin
      aParent:=aFile.UsedBy[ubMainSection,i];
      if aParent=SearchFor then
      begin
        // unit cycle found
        Cycle:=TFPList.Create;
        Cycle.Add(aParent);
        Cycle.Add(aFile);
        exit(true);
      end;
      if FindCycle(aParent,SearchFor,Cycle) then
      begin
        Cycle.Add(aFile);
        exit(true);
      end;
    end;
    Result:=false;
  end;

var
  aFile: TPas2jsCompilerFile;

  procedure CheckCycle;
  var
    i: Integer;
    Cycle: TFPList;
    CyclePath: String;
  begin
    if PasModule.ImplementationSection=nil then
    begin
      // main uses section (e.g. interface or program, not implementation)
      // -> check for cycles

      aFile.FUsedBy[ubMainSection].Add(Self);

      Cycle:=nil;
      try
        if FindCycle(aFile,aFile,Cycle) then
        begin
          CyclePath:='';
          for i:=0 to Cycle.Count-1 do begin
            if i>0 then CyclePath+=',';
            CyclePath+=TPas2jsCompilerFile(Cycle[i]).GetModuleName;
          end;
          PascalResolver.RaiseMsg(20180223141537,nUnitCycle,sUnitCycle,[CyclePath],NameExpr);
        end;
      finally
        Cycle.Free;
      end;
    end else begin
      // implementation uses section
      aFile.FUsedBy[ubImplSection].Add(Self);
    end;
  end;

var
  UseJSFilename: String;
  OtherFile: TPas2jsCompilerFile;
begin
  Result:=nil;

  aFile:=Compiler.FindUnitWithFile(UseFilename);

  if aFile<>nil then
  begin
    // known unit
    if (aFile.PasUnitName<>'') and (CompareText(aFile.PasUnitName,UseUnitname)<>0) then
    begin
      Log.LogPlain(['Debug: TPas2jsPasTree.FindUnit unitname MISMATCH aFile.PasUnitname="',aFile.PasUnitName,'"',
         ' Self=',FileResolver.Cache.FormatPath(PasFilename),
         ' Uses=',UseUnitname,
         ' IsForeign=',IsForeign]);
      RaiseInternalError(20170504161412,'TPas2jsPasTree.FindUnit unit name mismatch');
    end;
    CheckCycle;
  end else begin
    // new unit

    if InFilename<>'' then
    begin
      aFile:=Compiler.FindUnitWithName(UseUnitname);
      if aFile<>nil then
      begin
        {$IF defined(VerbosePasResolver) or defined(VerbosePas2JS)}
        writeln('TPas2jsCompilerFile.FindUnit in-file unit name duplicate: New=',UseFilename,' Old=',aFile.PasFilename);
        {$ENDIF}
        PascalResolver.RaiseMsg(20180223141323,nDuplicateFileFound,sDuplicateFileFound,
          [UseFilename,aFile.PasFilename],InFileExpr);
      end;
    end;

    UseJSFilename:='';
    if (not IsForeign) then
      UseJSFilename:=FileResolver.FindUnitJSFileName(UseFilename);
    //  Log.LogPlain(['Debug: TPas2jsPasTree.FindUnit Self=',FileResolver.Cache.FormatPath(PasFilename),
    //    ' Uses=',ActualUnitname,' Found="',FileResolver.Cache.FormatPath(UseFilename),'"',
    //    ' IsForeign=',IsForeign,' JSFile="',FileResolver.Cache.FormatPath(useJSFilename),'"']);

    // load Pascal or PCU file
    Compiler.LoadPasFile(UseFilename,UseUnitname,aFile,aFormat);

    // consistency checks
    if aFile.PasUnitName<>UseUnitname then
      RaiseInternalError(20170922143329,'aFile.PasUnitName='+aFile.PasUnitName+' UseUnitname='+UseUnitname);
    if aFormat=nil then
    begin
      if CompareFilenames(aFile.PasFilename,UseFilename)<>0 then
        RaiseInternalError(20170922143330,'aFile.PasFilename='+aFile.PasFilename+' UseFilename='+UseFilename);
    end else begin
      if CompareFilenames(aFile.PCUFilename,UseFilename)<>0 then
        RaiseInternalError(20180312122331,'aFile.PCUFilename='+aFile.PCUFilename+' UseFilename='+UseFilename);
    end;

    if aFile=Self then
    begin
      // unit uses itself -> cycle
      Parser.RaiseParserError(nUnitCycle,[UseUnitname]);
    end;

    // add file to trees
    Compiler.AddUsedUnit(aFile);
    // consistency checks
    OtherFile:=Compiler.FindUnitWithName(UseUnitname);
    if aFile<>OtherFile then
    begin
      if OtherFile=nil then
        RaiseInternalError(20170922143405,'UseUnitname='+UseUnitname)
      else
        RaiseInternalError(20170922143511,'UseUnitname='+UseUnitname+' Found='+OtherFile.PasUnitName);
    end;
    OtherFile:=Compiler.FindUnitWithFile(UseFilename);
    if aFile<>OtherFile then
    begin
      if OtherFile=nil then
        RaiseInternalError(20180224094625,'UsePasFilename='+UseFilename)
      else
        RaiseInternalError(20180224094627,'UsePasFilename='+UseFilename+' Found='+OtherFile.PasFilename);
    end;

    CheckCycle;

    aFile.JSFilename:=UseJSFilename;
    aFile.IsForeign:=UseIsForeign;

    // read
    aFile.ReadUnit;
    // beware: the parser may not yet have finished
  end;

  Result:=aFile;
end;

{ TPas2jsCompiler }

procedure TPas2jsCompiler.SetFileCache(AValue: TPas2jsFilesCache);
begin
  if FFileCache=AValue then Exit;
  FFileCacheAutoFree:=false;
  FFileCache:=AValue;
end;

function TPas2jsCompiler.GetFileCount: integer;
begin
  Result:=FFiles.Count;
end;

function TPas2jsCompiler.GetDefaultNamespace: String;
var
  C: TClass;
begin
  Result:='';
  if FMainFile=nil then exit;
  if FMainFile.PasModule=nil then exit;
  C:=FMainFile.PasModule.ClassType;
  if (C=TPasProgram) or (C=TPasLibrary) or (C=TPasPackage) then
    Result:=FMainFile.PascalResolver.DefaultNameSpace;
end;

procedure TPas2jsCompiler.ConditionEvalLog(Sender: TCondDirectiveEvaluator;
  Args: array of const);
begin
  CfgSyntaxError(SafeFormat(Sender.MsgPattern,Args));
end;

function TPas2jsCompiler.ConditionEvalVariable(Sender: TCondDirectiveEvaluator;
  aName: String; out Value: string): boolean;
var
  i: Integer;
  M: TMacroDef;
  ms: TModeSwitch;
begin
  // check defines
  i:=FDefines.IndexOf(aName);
  if i>=0 then
  begin
    M:=TMacroDef(FDefines.Objects[i]);
    if M=nil then
      Value:=CondDirectiveBool[true]
    else
      Value:=M.Value;
    exit(true);
  end;

  // check modeswitches
  ms:=StrToModeSwitch(aName);
  if (ms<>msNone) and (ms in p2jsMode_SwitchSets[Mode]) then
  begin
    Value:=CondDirectiveBool[true];
    exit(true);
  end;
end;

procedure TPas2jsCompiler.Compile(StartTime: TDateTime);
var
  Checked: TAVLTree;
  CombinedFileWriter: TPas2JSMapper;
  SrcFileCount: integer;
  Seconds: TDateTime;
  ok: Boolean;
begin
  if FMainFile<>nil then
    RaiseInternalError(20170504192137,'');
  Checked:=nil;
  CombinedFileWriter:=nil;
  SrcFileCount:=0;

  if FileCache.PrecompileFormat<>nil then
    CreateGUID(FPrecompileGUID);

  ok:=false;
  try
    // load main Pascal file
    LoadPasFile(FileCache.MainSrcFile,'',FMainFile);
    if MainFile=nil then exit;
    // parse and load Pascal files recursively
    PrecompileInitialFlags.ParserOptions:=FMainFile.Parser.Options;
    PrecompileInitialFlags.ModeSwitches:=FMainFile.Scanner.CurrentModeSwitches;
    PrecompileInitialFlags.BoolSwitches:=FMainFile.Scanner.CurrentBoolSwitches;
    PrecompileInitialFlags.ConverterOptions:=FMainFile.GetInitialConverterOptions;
    PrecompileInitialFlags.TargetPlatform:=TargetPlatform;
    PrecompileInitialFlags.TargetProcessor:=TargetProcessor;
    FMainFile.ReadUnit;
    ProcessQueue;

    // whole program optimization
    if MainFile.PasModule is TPasProgram then
      OptimizeProgram(MainFile);

    // check what files need building
    Checked:=TAVLTree.Create;
    MarkNeedBuilding(MainFile,Checked,SrcFileCount);
    SrcFileCount:=Checked.Count;// all modules, including skipped modules
    FreeAndNil(Checked);

    // convert all Pascal to JavaScript
    Checked:=TAVLTree.Create;
    CreateJavaScript(MainFile,Checked);
    FreeAndNil(Checked);

    // write .js files
    Checked:=TAVLTree.Create;
    WriteJSFiles(MainFile,CombinedFileWriter,Checked);
    FreeAndNil(Checked);

    // write success message
    if ExitCode=0 then
    begin
      Seconds:=(Now-StartTime)*86400;
      Log.LogMsgIgnoreFilter(nLinesInFilesCompiled,
             [IntToStr(FileCache.ReadLineCounter),IntToStr(SrcFileCount),
              FormatFloat('0.0',Seconds),'s']);
      ok:=true;
    end;
  finally
    Checked.Free;
    if not Ok then
      Log.LogMsgIgnoreFilter(nCompilationAborted,[]);
    CombinedFileWriter.Free;
  end;
end;

procedure TPas2jsCompiler.ProcessQueue;
var
  i: Integer;
  aFile: TPas2jsCompilerFile;
  Found: Boolean;
  Section: TPasSection;
begin
  // parse til exception or all modules have finished
  repeat
    {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
    writeln('TPas2jsCompiler.ProcessQueue FParsingModules.Count=',FReadingModules.Count);
    {$ENDIF}
    Found:=false;
    for i:=FReadingModules.Count-1 downto 0 do
      begin
      aFile:=TPas2jsCompilerFile(FReadingModules[i]);
      if aFile.ReaderState<>prsCanContinue then
        begin
        {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
        writeln('TPas2jsCompiler.ProcessQueue aFile=',aFile.PasFilename,' NOT YET READY');
        {$ENDIF}
        if (aFile.PCUReader=nil) and (aFile.Parser.CurModule=nil) then
          RaiseInternalError(20180306111410,'File='+aFile.PasFilename+' Parser.CurModule=nil');
        continue;
        end;
      Found:=true;
      {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
      writeln('TPas2jsCompiler.ProcessQueue aFile=',aFile.PasFilename);
      {$ENDIF}
      aFile.ReadContinue;
      if aFile.ReaderState=prsCanContinue then
      begin
        {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
        writeln('TPas2jsCompiler.ProcessQueue aFile=',aFile.PasFilename,' ReadContinue buggy');
        {$ENDIF}
        RaiseInternalError(20180313130300,'File='+aFile.PasFilename+' ReadContinue buggy');
      end;
      break;
      end;
  until not Found;
  {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
  writeln('TPas2jsCompiler.ProcessQueue END FParsingModules.Count=',FReadingModules.Count);
  {$ENDIF}

  // check consistency
  for i:=0 to FReadingModules.Count-1 do
    begin
    aFile:=TPas2jsCompilerFile(FReadingModules[i]);
    if aFile.PascalResolver=nil then
      RaiseInternalError(20180313124125,aFile.PasFilename);
    if (aFile.PCUReader=nil) and (aFile.Parser.CurModule<>nil) then
      begin
      {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
      writeln('TPas2jsCompiler.ProcessQueue aFile=',aFile.PasFilename,' was not finished');
      {$ENDIF}
      RaiseInternalError(20180305185342,aFile.PasFilename);
      end;
    Section:=aFile.PascalResolver.GetLastSection;
    if Section=nil then
      RaiseInternalError(20180313124207,aFile.PasFilename);
    if Section.PendingUsedIntf<>nil then
      RaiseInternalError(20180313124226,aFile.PasFilename+' '+GetObjName(Section)+' PendingUsedIntf='+GetObjName(Section.PendingUsedIntf));
    end;
end;

function TPas2jsCompiler.MarkNeedBuilding(aFile: TPas2jsCompilerFile;
  Checked: TAVLTree; var SrcFileCount: integer): boolean;

  procedure Mark(MsgNumber: integer; Args: array of const);
  begin
    if aFile.NeedBuild then exit;
    aFile.NeedBuild:=true;
    inc(SrcFileCount);
    if ShowDebug or ShowTriedUsedFiles then
      Log.LogMsg(MsgNumber,Args,'',0,0,false);
  end;

  procedure CheckUsesClause(UsesClause: TPasUsesClause);
  var
    i: Integer;
    UsedFile: TPas2jsCompilerFile;
    aModule: TPasModule;
  begin
    if length(UsesClause)=0 then exit;
    for i:=0 to length(UsesClause)-1 do begin
      aModule:=UsesClause[i].Module as TPasModule;
      UsedFile:=TPas2jsCompilerFile.GetFile(aModule);
      if UsedFile=nil then
        RaiseInternalError(20171214121631,aModule.Name);
      if MarkNeedBuilding(UsedFile,Checked,SrcFileCount) then
      begin
        if not aFile.NeedBuild then
          Mark(nUnitNeedsCompileDueToUsedUnit,
                                   [aFile.GetModuleName,UsedFile.GetModuleName]);
      end;
    end;
  end;

begin
  Result:=false;
  // check each file only once
  if Checked.Find(aFile)<>nil then
    exit(aFile.NeedBuild);
  Checked.Add(aFile);

  if FileCache.AllJSIntoMainJS and (WPOAnalyzer<>nil)
  and not WPOAnalyzer.IsUsed(aFile.PasModule) then
  begin
    {$IF defined(VerboseUnitQueue) or defined(VerbosePCUFiler)}
    writeln('TPas2jsCompiler.MarkNeedBuilding module not used by WPO: ',aFile.PasFilename);
    {$ENDIF}
    exit(false);
  end;

  // check dependencies
  CheckUsesClause(aFile.GetPasMainUsesClause);
  CheckUsesClause(aFile.GetPasImplUsesClause);

  if (not aFile.NeedBuild) and (not aFile.IsForeign) then
  begin
    // this unit can be compiled
    if aFile.IsMainFile then
      Mark(nUnitNeedsCompileDueToOption,[aFile.GetModuleName,'<main source file>'])
    else if coBuildAll in Options then
      Mark(nUnitNeedsCompileDueToOption,[aFile.GetModuleName,'-B'])
    else if FileCache.AllJSIntoMainJS then
      Mark(nUnitNeedsCompileDueToOption,[aFile.GetModuleName,'-Jc'])
    else if (aFile.JSFilename<>'') and (not DirectoryCache.FileExists(aFile.JSFilename)) then
      Mark(nUnitNeedsCompileJSMissing,[aFile.GetModuleName,FileCache.FormatPath(aFile.JSFilename)])
    else if (aFile.JSFilename<>'')
    and (DirectoryCache.FileAge(aFile.PasFilename)>DirectoryCache.FileAge(aFile.JSFilename))
    then begin
      Mark(nUnitNeedsCompilePasHasChanged,[aFile.GetModuleName,QuoteStr(FileCache.FormatPath(aFile.JSFilename))])
    end;
  end;

  if aFile.NeedBuild then
  begin
    // unit needs compile
    if aFile.IsForeign then
    begin
      // ... but is forbidden to compile
      Log.LogMsg(nOptionForbidsCompile,[aFile.GetModuleName]);
      Terminate(ExitCodeWriteError);
    end;
  end;

  Result:=aFile.NeedBuild;
end;

procedure TPas2jsCompiler.OptimizeProgram(aFile: TPas2jsCompilerFile);
begin
  if not FileCache.AllJSIntoMainJS then exit;
  if coKeepNotUsedDeclarationsWPO in Options then exit;
  if not (aFile.PasModule is TPasProgram) then exit;
  FWPOAnalyzer:=TPas2JSWPOptimizer.Create;
  FWPOAnalyzer.Resolver:=aFile.PascalResolver;
  FWPOAnalyzer.Options:=FWPOAnalyzer.Options+[paoOnlyExports];
  FWPOAnalyzer.AnalyzeWholeProgram(TPasProgram(aFile.PasModule));
end;

function TPas2jsCompiler.CreatePrecompileFilename(aFile: TPas2jsCompilerFile
  ): string;
begin
  Result:=ExtractFilenameOnly(aFile.PasFilename)+'.'+FileCache.PrecompileFormat.Ext;
  if FileCache.UnitOutputPath<>'' then
    Result:=FileCache.UnitOutputPath+Result
  else
    Result:=ExtractFilePath(aFile.PasFilename)+Result;
end;

procedure TPas2jsCompiler.CreateJavaScript(aFile: TPas2jsCompilerFile;
  Checked: TAVLTree);

  procedure CheckUsesClause(UsesClause: TPasUsesClause);
  var
    i: Integer;
    UsedFile: TPas2jsCompilerFile;
    aModule: TPasModule;
  begin
    if length(UsesClause)=0 then exit;
    for i:=0 to length(UsesClause)-1 do begin
      aModule:=UsesClause[i].Module as TPasModule;
      UsedFile:=TPas2jsCompilerFile.GetFile(aModule);
      if UsedFile=nil then
        RaiseInternalError(20171214121720,aModule.Name);
      CreateJavaScript(UsedFile,Checked);
    end;
  end;

begin
  if (aFile.JSModule<>nil) or (not aFile.NeedBuild) then exit;
  // check each file only once
  if Checked.Find(aFile)<>nil then exit;
  Checked.Add(aFile);

  Log.LogMsg(nCompilingFile,[QuoteStr(FileCache.FormatPath(aFile.PasFilename))],'',0,0,
    not (coShowLineNumbers in Options));

  // convert dependencies
  CheckUsesClause(aFile.GetPasMainUsesClause);
  CheckUsesClause(aFile.GetPasImplUsesClause);

  aFile.CreateJS;
end;

procedure TPas2jsCompiler.FinishSrcMap(SrcMap: TPas2JSSrcMap);
var
  LocalFilename, MapFilename, BaseDir: String;
  aFile: TPas2jsCachedFile;
  i: Integer;
begin
  if SrcMapBaseDir<>'' then
    BaseDir:=SrcMapBaseDir
  else
    BaseDir:=ExtractFilePath(ExtractFilePath(SrcMap.LocalFilename));
  for i:=0 to SrcMap.SourceCount-1 do begin
    LocalFilename:=SrcMap.SourceFiles[i];
    if LocalFilename='' then continue;
    if SrcMapInclude then
    begin
      // include source in SrcMap
      aFile:=FileCache.LoadFile(LocalFilename);
      SrcMap.SourceContents[i]:=aFile.Source;
    end;
    // translate local file name
    if BaseDir<>'' then
    begin
      if not TryCreateRelativePath(LocalFilename,BaseDir,true,MapFilename) then
      begin
        // e.g. file is on another partition
        if not SrcMapInclude then
        begin
          Log.Log(mtError,
            SafeFormat(sUnableToTranslatePathToDir,[QuoteStr(LocalFilename),QuoteStr(BaseDir)]),
                       nUnableToTranslatePathToDir);
          Terminate(ExitCodeConverterError);
        end;
        // the source is included, do not translate the filename
        MapFilename:=LocalFilename;
      end;
      {$IFNDEF Unix}
      // use / as PathDelim
      MapFilename:=StringReplace(MapFilename,PathDelim,'/',[rfReplaceAll]);
      {$ENDIF}
      SrcMap.SourceTranslatedFiles[i]:=MapFilename;
    end;
  end;
end;

function TPas2jsCompiler.DoWriteJSFile(const DestFilename: String;
  aWriter: TPas2JSMapper): Boolean;
begin
  Result:=False;
  if DestFilename='' then ;
  if aWriter=nil then ;
end;

procedure TPas2jsCompiler.WriteJSFiles(aFile: TPas2jsCompilerFile;
  var CombinedFileWriter: TPas2JSMapper; Checked: TAVLTree);

  procedure CheckUsesClause(UsesClause: TPasUsesClause);
  var
    i: Integer;
    UsedFile: TPas2jsCompilerFile;
    aModule: TPasModule;
  begin
    if length(UsesClause)=0 then exit;
    for i:=0 to length(UsesClause)-1 do begin
      aModule:=UsesClause[i].Module as TPasModule;
      UsedFile:=TPas2jsCompilerFile.GetFile(aModule);
      if UsedFile=nil then
        RaiseInternalError(20171214121720,aModule.Name);
      WriteJSFiles(UsedFile,CombinedFileWriter,Checked);
    end;
  end;

var
  aFileWriter: TPas2JSMapper;
  FreeWriter: Boolean;

  procedure CreateFileWriter(aFilename: string);
  var
    SrcMap: TPas2JSSrcMap;
  begin
    aFileWriter:=TPas2JSMapper.Create(4096);
    FreeWriter:=true;
    if SrcMapEnable then
    begin
      SrcMap:=TPas2JSSrcMap.Create(ExtractFilename(aFilename));
      aFileWriter.SrcMap:=SrcMap;
      SrcMap.Release;// release the refcount from the Create
      SrcMap.SourceRoot:=SrcMapSourceRoot;
      SrcMap.LocalFilename:=aFile.JSFilename;
      if SrcMapXSSIHeader then
        SrcMap.Options:=SrcMap.Options+[smoSafetyHeader]
      else
        SrcMap.Options:=SrcMap.Options-[smoSafetyHeader];
    end;
  end;

var
  DestFilename, DestDir, Src, MapFilename: String;
  aJSWriter: TJSWriter;
  ms: TMemoryStream;
begin
  //writeln('TPas2jsCompiler.WriteJSFiles ',aFile.PasFilename,' Need=',aFile.NeedBuild,' Checked=',Checked.Find(aFile)<>nil);
  if (aFile.JSModule=nil) or (not aFile.NeedBuild) then exit;
  // check each file only once
  if Checked.Find(aFile)<>nil then exit;
  Checked.Add(aFile);

  FreeWriter:=false;
  if FileCache.AllJSIntoMainJS and (CombinedFileWriter=nil) then
  begin
    // create CombinedFileWriter
    DestFilename:=FileCache.GetResolvedMainJSFile;
    CreateFileWriter(DestFilename);
    CombinedFileWriter:=aFileWriter;
    FileCache.InsertCustomJSFiles(CombinedFileWriter);
  end else begin
    DestFilename:=aFile.JSFilename;
  end;

  // convert dependencies
  CheckUsesClause(aFile.GetPasMainUsesClause);
  CheckUsesClause(aFile.GetPasImplUsesClause);

  aJSWriter:=nil;
  aFileWriter:=CombinedFileWriter;
  try
    if aFileWriter=nil then
    begin
      // create writer for this file
      CreateFileWriter(DestFilename);
      if aFile.IsMainFile and not FileCache.AllJSIntoMainJS then
        FileCache.InsertCustomJSFiles(aFileWriter);
    end;

    // write JavaScript
    aJSWriter:=TJSWriter.Create(aFileWriter);
    aJSWriter.Options:=DefaultJSWriterOptions;
    aJSWriter.IndentSize:=2;
    try
      aJSWriter.WriteJS(aFile.JSModule);
    except
      on E: Exception do begin
        if ShowDebug then
          Log.LogExceptionBackTrace;
        Log.LogPlain('[20180204193420] Error while creating JavaScript "'+FileCache.FormatPath(DestFilename)+'": '+E.Message);
        Terminate(ExitCodeErrorInternal);
      end;
    end;

    if aFile.IsMainFile and (TargetPlatform=PlatformNodeJS) then
      aFileWriter.WriteFile('rtl.run();'+LineEnding,aFile.PasFilename);

    // Give chance to descendants to write file
    if DoWriteJSFile(aFile.JSFilename,aFileWriter) then
      exit;// descendant has written -> finished

    if (aFile.JSFilename='') and (FileCache.MainJSFile='.') then
    begin
      // write to stdout
      if FreeWriter then
      begin
        Log.WriteMsgToStdErr:=false;
        try
          Log.LogRaw(aFileWriter.AsAnsistring);
        finally
          Log.WriteMsgToStdErr:=coWriteMsgToStdErr in Options;
        end;
      end;
    end else if FreeWriter then
    begin
      // write to file

      //writeln('TPas2jsCompiler.WriteJSFiles ',aFile.PasFilename,' ',aFile.JSFilename);
      Log.LogMsg(nWritingFile,[QuoteStr(FileCache.FormatPath(DestFilename))],'',0,0,
                 not (coShowLineNumbers in Options));

      // check output directory
      DestDir:=ChompPathDelim(ExtractFilePath(DestFilename));
      if (DestDir<>'') and not DirectoryExists(DestDir) then
      begin
        Log.LogMsg(nOutputDirectoryNotFound,[QuoteStr(FileCache.FormatPath(DestDir))]);
        Terminate(ExitCodeFileNotFound);
      end;
      if DirectoryExists(DestFilename) then
      begin
        Log.LogMsg(nFileIsFolder,[QuoteStr(FileCache.FormatPath(DestFilename))]);
        Terminate(ExitCodeWriteError);
      end;

      MapFilename:=DestFilename+'.map';

      // write js
      try
        ms:=TMemoryStream.Create;
        try
          // UTF8-BOM
          if (Log.Encoding='') or (Log.Encoding='utf8') then
          begin
            Src:=String(UTF8BOM);
            ms.Write(Src[1],length(Src));
          end;
          // JS source
          ms.Write(aFileWriter.Buffer^,aFileWriter.BufferLength);
          // source map comment
          if aFileWriter.SrcMap<>nil then
          begin
            Src:='//# sourceMappingURL='+ExtractFilename(MapFilename)+LineEnding;
            ms.Write(Src[1],length(Src));
          end;
          ms.Position:=0;
          FileCache.SaveToFile(ms,DestFilename);
        finally
          ms.Free;
        end;
      except
        on E: Exception do begin
          if ShowDebug then
            Log.LogExceptionBackTrace;
          if E.Message<>SafeFormat(SFCreateError,[DestFileName]) then
            Log.LogPlain('Error: '+E.Message);
          Log.LogMsg(nUnableToWriteFile,[QuoteStr(FileCache.FormatPath(DestFilename))]);
          Terminate(ExitCodeWriteError);
        end;
      end;

      // write source map
      if aFileWriter.SrcMap<>nil then
      begin
        Log.LogMsg(nWritingFile,[QuoteStr(FileCache.FormatPath(MapFilename))],'',0,0,
                   not (coShowLineNumbers in Options));
        FinishSrcMap(aFileWriter.SrcMap);
        try
          ms:=TMemoryStream.Create;
          try
            // Note: No UTF-8 BOM in source map, Chrome 59 gives an error
            aFileWriter.SrcMap.SaveToStream(ms);
            ms.Position:=0;
            FileCache.SaveToFile(ms,MapFilename);
          finally
            ms.Free;
          end;
        except
          on E: Exception do begin
            if ShowDebug then
              Log.LogExceptionBackTrace;
            if E.Message<>SafeFormat(SFCreateError,[DestFileName]) then
              Log.LogPlain('Error: '+E.Message);
            Log.LogMsg(nUnableToWriteFile,[QuoteStr(FileCache.FormatPath(MapFilename))]);
            Terminate(ExitCodeWriteError);
          end;
        end;
      end;
    end;

  finally
    if FreeWriter then
    begin
      if CombinedFileWriter=aFileWriter then
        CombinedFileWriter:=nil;
      aFileWriter.Free
    end;
    aJSWriter.Free;
  end;
end;

procedure TPas2jsCompiler.InitParamMacros;
begin
  ParamMacros.AddValue('Pas2jsFullVersion','major.minor.release<extra>',GetVersion(false));
  ParamMacros.AddValue('Pas2jsVersion','major.minor.release',GetVersion(true));
  ParamMacros.AddFunction('Env','environment variable, e.g. $Env(HOME)',@OnMacroEnv,true);
  ParamMacros.AddFunction('CfgDir','Use within a config file. The directory of this config file',@OnMacroCfgDir,false);
  // Additionally, under windows the following special variables are recognized:

{ ToDo:
  LOCAL_APPDATA
      Usually the directory ”Local settings/Application Data” under the user’s home directory.
  APPDATA
      Usually the directory ”Application Data” under the user’s home directory.
  COMMON_APPDATA
      Usually the directory ”Application Data” under the ’All users’ directory.
  PERSONAL
      Usually the ”My documents” directory of the user.
  PROGRAM_FILES
      Usually ”program files” directory on the system drive
  PROGRAM_FILES_COMMON
      Usually the ”Common files” directory under the program files directory.
  PROFILE
      The user’s home directory.   }
end;

procedure TPas2jsCompiler.ClearDefines;
var
  i: Integer;
  M: TMacroDef;
begin
  for i:=0 to FDefines.Count-1 do
    begin
    M:=TMacroDef(FDefines.Objects[i]);
    M.Free;
    end;
  FDefines.Clear;
end;

procedure TPas2jsCompiler.RaiseInternalError(id: int64; Msg: string);
begin
  Log.LogPlain('['+IntToStr(id)+'] '+Msg);
  raise Exception.Create(Msg);
end;

function TPas2jsCompiler.GetExitCode: Longint;
begin
  Result:=System.ExitCode;
end;

procedure TPas2jsCompiler.SetExitCode(Value: Longint);
begin
  System.ExitCode:=Value;
end;

procedure TPas2jsCompiler.Terminate(TheExitCode: integer);
begin
  ExitCode:=TheExitCode;
  if Log<>nil then Log.Flush;
  raise ECompilerTerminate.Create('');
end;

function TPas2jsCompiler.GetShowDebug: boolean;
begin
  Result:=coShowDebug in Options;
end;

function TPas2jsCompiler.GetShowFullPaths: boolean;
begin
  Result:=FileCache.ShowFullPaths;
end;

function TPas2jsCompiler.GetShowLogo: Boolean;
begin
  Result:=coShowLogo in FOptions;
end;

function TPas2jsCompiler.GetShowTriedUsedFiles: boolean;
begin
  Result:=coShowTriedUsedFiles in FOptions;
end;

function TPas2jsCompiler.GetShowUsedTools: boolean;
begin
  Result:=coShowUsedTools in Options;
end;

function TPas2jsCompiler.GetSkipDefaultConfig: Boolean;
begin
  Result:=coSkipDefaultConfigs in FOptions;
end;

function TPas2jsCompiler.GetSrcMapBaseDir: string;
begin
  Result:=FileCache.SrcMapBaseDir;
end;

function TPas2jsCompiler.GetSrcMapEnable: boolean;
begin
  Result:=coSourceMapCreate in FOptions;
end;

function TPas2jsCompiler.GetSrcMapInclude: boolean;
begin
  Result:=coSourceMapInclude in FOptions;
end;

function TPas2jsCompiler.GetSrcMapXSSIHeader: boolean;
begin
  Result:=coSourceMapXSSIHeader in FOptions;
end;

function TPas2jsCompiler.GetWriteDebugLog: boolean;
begin
  Result:=coWriteDebugLog in FOptions;
end;

function TPas2jsCompiler.GetWriteMsgToStdErr: boolean;
begin
  Result:=coWriteMsgToStdErr in FOptions;
end;

procedure TPas2jsCompiler.SetCompilerExe(AValue: string);
begin
  if AValue<>'' then
    AValue:=ExpandFileName(AValue);
  if FCompilerExe=AValue then Exit;
  FCompilerExe:=AValue;
end;

procedure TPas2jsCompiler.SetMode(AValue: TP2jsMode);
begin
  if FMode=AValue then Exit;
  FMode:=AValue;
  case FMode of
  p2jmObjFPC: Options:=Options-[coAllowCAssignments];
  p2jmDelphi: Options:=Options-[coAllowCAssignments];
  end;
end;

procedure TPas2jsCompiler.SetOptions(AValue: TP2jsCompilerOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
  Log.ShowMsgNumbers:=coShowMessageNumbers in FOptions;
  Log.ShowMsgTypes:=GetShownMsgTypes;
  FileCache.ShowTriedUsedFiles:=coShowTriedUsedFiles in FOptions;
end;

procedure TPas2jsCompiler.SetShowDebug(AValue: boolean);
begin
  if AValue then
    FOptions:=FOptions+[coShowNotes,coShowInfos,coShowDebug]
  else
    Exclude(FOptions,coShowNotes);
end;

procedure TPas2jsCompiler.SetShowFullPaths(AValue: boolean);
begin
  FileCache.ShowFullPaths:=AValue;
end;

procedure TPas2jsCompiler.SetShowLogo(AValue: Boolean);
begin
  SetOption(coShowLogo,AValue);
end;

procedure TPas2jsCompiler.SetShowTriedUsedFiles(AValue: boolean);
begin
  FileCache.ShowTriedUsedFiles:=AValue;
  SetOption(coShowTriedUsedFiles,AValue);
end;

procedure TPas2jsCompiler.SetShowUsedTools(AValue: boolean);
begin
  SetOption(coShowUsedTools,AValue);
end;

procedure TPas2jsCompiler.SetSkipDefaultConfig(AValue: Boolean);
begin
  SetOption(coSkipDefaultConfigs,AValue);
end;

procedure TPas2jsCompiler.SetSrcMapBaseDir(const AValue: string);
begin
  FileCache.SrcMapBaseDir:=AValue;
end;

procedure TPas2jsCompiler.SetSrcMapEnable(const AValue: boolean);
begin
  SetOption(coSourceMapCreate,AValue);
end;

procedure TPas2jsCompiler.SetSrcMapInclude(const AValue: boolean);
begin
  SetOption(coSourceMapInclude,AValue);
end;

procedure TPas2jsCompiler.SetSrcMapXSSIHeader(const AValue: boolean);
begin
  SetOption(coSourceMapXSSIHeader,AValue);
end;

procedure TPas2jsCompiler.SetTargetPlatform(const AValue: TPasToJsPlatform);
begin
  if FTargetPlatform=AValue then Exit;
  RemoveDefine(PasToJsPlatformNames[TargetPlatform]);
  FTargetPlatform:=AValue;
  if FTargetPlatform=PlatformNodeJS then
    FileCache.AllJSIntoMainJS:=true;
  AddDefinesForTargetPlatform;
end;

procedure TPas2jsCompiler.SetTargetProcessor(const AValue: TPasToJsProcessor);
begin
  if FTargetProcessor=AValue then Exit;
  RemoveDefine(PasToJsProcessorNames[TargetProcessor]);
  FTargetProcessor:=AValue;
  AddDefinesForTargetProcessor;
end;

procedure TPas2jsCompiler.SetWriteDebugLog(const AValue: boolean);
begin
  SetOption(coWriteDebugLog,AValue);
end;

procedure TPas2jsCompiler.SetWriteMsgToStdErr(const AValue: boolean);
begin
  SetOption(coWriteMsgToStdErr,AValue);
  Log.WriteMsgToStdErr:=AValue;
end;

procedure TPas2jsCompiler.AddDefinesForTargetPlatform;
begin
  AddDefine(PasToJsPlatformNames[TargetPlatform]);
end;

procedure TPas2jsCompiler.AddDefinesForTargetProcessor;
begin
  AddDefine(PasToJsProcessorNames[TargetProcessor]);
  case TargetProcessor of
    ProcessorECMAScript5: AddDefine('ECMAScript', '5');
    ProcessorECMAScript6: AddDefine('ECMAScript', '6');
  end;
end;

procedure TPas2jsCompiler.AddReadingModule(aFile: TPas2jsCompilerFile);
begin
  if FReadingModules.IndexOf(aFile)>=0 then
    exit;
  FReadingModules.Add(aFile);
end;

procedure TPas2jsCompiler.RemoveReadingModule(aFile: TPas2jsCompilerFile);
begin
  FReadingModules.Remove(aFile);
end;

procedure TPas2jsCompiler.CfgSyntaxError(const Msg: string);
begin
  Log.Log(mtError,Msg,0,CurrentCfgFilename,CurrentCfgLineNumber,0);
  Terminate(ExitCodeErrorInConfig);
end;

procedure TPas2jsCompiler.LoadConfig(CfgFilename: string);
type
  TSkip = (
    skipNone,
    skipIf,
    skipElse
  );
const
  IdentChars = ['a'..'z','A'..'Z','_','0'..'9'];
var
  Line: String;
  p, StartP: PChar;

  function GetWord: String;
  begin
    StartP:=p;
    while (p^ in IdentChars) or (p^>#127) do inc(p);
    Result:=copy(Line,StartP-PChar(Line)+1,p-StartP);
    while p^ in [' ',#9] do inc(p);
  end;

  procedure DebugCfgDirective(const s: string);
  begin
    Log.LogMsg(nCfgDirective,[QuoteStr(Line),s],CurrentCfgFilename,CurrentCfgLineNumber,1,false);
  end;

var
  OldCfgFilename, Directive, aName, Expr: String;
  aFile: TPas2jsFileLineReader;
  IfLvl, SkipLvl, OldCfgLineNumber: Integer;
  Skip: TSkip;
  CacheFile: TPas2jsCachedFile;
begin
  if ShowTriedUsedFiles then
    Log.LogMsgIgnoreFilter(nReadingOptionsFromFile,[QuoteStr(CfgFilename)]);
  IfLvl:=0;
  SkipLvl:=0;
  Skip:=skipNone;
  aFile:=nil;
  try
    OldCfgFilename:=FCurrentCfgFilename;
    FCurrentCfgFilename:=CfgFilename;
    OldCfgLineNumber:=FCurrentCfgLineNumber;
    CacheFile:=FileCache.LoadFile(CfgFilename);
    aFile:=CacheFile.CreateLineReader(true);
    while not aFile.IsEOF do begin
      Line:=aFile.ReadLine;
      FCurrentCfgLineNumber:=aFile.LineNumber;
      if ShowDebug then
        Log.LogMsgIgnoreFilter(nInterpretingFileOption,[QuoteStr(Line)]);
      if Line='' then continue;
      p:=PChar(Line);
      while (p^ in [' ',#9]) do inc(p);
      if p^=#0 then continue; // empty line

      if p^='#' then
      begin
        // cfg directive
        inc(p);
        if p^ in [#0,#9,' ','-'] then continue; // comment
        Directive:=lowercase(GetWord);
        case Directive of
        'ifdef','ifndef':
          begin
            inc(IfLvl);
            if Skip=skipNone then
            begin
              aName:=GetWord;
              if IsDefined(aName)=(Directive='ifdef') then
              begin
                // execute block
                if ShowDebug then
                  DebugCfgDirective('true -> execute');
              end else begin
                // skip block
                if ShowDebug then
                  DebugCfgDirective('false -> skip');
                SkipLvl:=IfLvl;
                Skip:=skipIf;
              end;
            end;
          end;
        'if':
          begin
            inc(IfLvl);
            if Skip=skipNone then
            begin
              Expr:=copy(Line,p-PChar(Line)+1,length(Line));
              if ConditionEvaluator.Eval(Expr) then
              begin
                // execute block
                if ShowDebug then
                  DebugCfgDirective('true -> execute');
              end else begin
                // skip block
                if ShowDebug then
                  DebugCfgDirective('false -> skip');
                SkipLvl:=IfLvl;
                Skip:=skipIf;
              end;
            end;
          end;
        'else':
          begin
            if IfLvl=0 then
              CfgSyntaxError('"'+Directive+'" without #ifdef');
            if (Skip=skipElse) and (IfLvl=SkipLvl) then
              CfgSyntaxError('"there was already an #else');
            if (Skip=skipIf) and (IfLvl=SkipLvl) then
            begin
              // if-block was skipped -> execute else block
              if ShowDebug then
                DebugCfgDirective('execute');
              SkipLvl:=0;
              Skip:=skipNone;
            end else if Skip=skipNone then
            begin
              // if-block was executed -> skip else block
              if ShowDebug then
                DebugCfgDirective('skip');
              Skip:=skipElse;
              SkipLvl:=IfLvl;
            end;
          end;
        'elseif':
          begin
            if IfLvl=0 then
              CfgSyntaxError('"'+Directive+'" without #ifdef');
            if (Skip=skipIf) and (IfLvl=SkipLvl) then
            begin
              // if-block was skipped -> try this elseif
              Expr:=copy(Line,p-PChar(Line)+1,length(Line));
              if ConditionEvaluator.Eval(Expr) then
              begin
                // execute elseif block
                if ShowDebug then
                  DebugCfgDirective('true -> execute');
                SkipLvl:=0;
                Skip:=skipNone;
              end else begin
                // skip elseif block
                if ShowDebug then
                  DebugCfgDirective('false -> skip');
              end;
            end else if Skip=skipNone then
            begin
              // if-block was executed -> skip without test
              if ShowDebug then
                DebugCfgDirective('no test -> skip');
              Skip:=skipIf;
            end;
          end;
        'endif':
          begin
            if IfLvl=0 then
              CfgSyntaxError('"'+Directive+'" without #ifdef');
            dec(IfLvl);
            if IfLvl<SkipLvl then
            begin
              // end block
              if ShowDebug then
                DebugCfgDirective('end block');
              SkipLvl:=0;
              Skip:=skipNone;
            end;
          end;
        'error':
          ParamFatal('user defined: '+copy(Line,p-PChar(Line)+1,length(Line)))
        else
          if Skip=skipNone then
            CfgSyntaxError('unknown directive "#'+Directive+'"')
          else
            DebugCfgDirective('skipping unknown directive');
        end;
      end else if Skip=skipNone then
      begin
        // option line
        Line:=String(p);
        ReadParam(Line,false,false);
      end;
    end;
  finally
    FCurrentCfgFilename:=OldCfgFilename;
    FCurrentCfgLineNumber:=OldCfgLineNumber;
    aFile.Free;
  end;
  if ShowTriedUsedFiles then
    Log.LogMsgIgnoreFilter(nEndOfReadingConfigFile,[QuoteStr(CfgFilename)]);
end;

procedure TPas2jsCompiler.LoadDefaultConfig;

  function TryConfig(aFilename: string): boolean;
  begin
    Result:=false;
    if aFilename='' then exit;
    aFilename:=ExpandFileName(aFilename);
    if ShowTriedUsedFiles then
      Log.LogMsgIgnoreFilter(nConfigFileSearch,[aFilename]);
    if not DirectoryCache.FileExists(aFilename) then exit;
    Result:=true;
    LoadConfig(aFilename);
  end;

var
  aFilename: String;
begin
  // first try HOME directory
  aFilename:=ChompPathDelim(GetEnvironmentVariableUTF8('HOME'));
  if aFilename<>'' then
    begin
    aFilename:=aFilename+PathDelim{$IFDEF UNIX}+'.'{$ENDIF}+DefaultConfigFile;
    if TryConfig(aFileName) then exit;
    end;

  // then try compiler directory
  if (CompilerExe<>'') then
  begin
    aFilename:=ExtractFilePath(CompilerExe);
    if aFilename<>'' then
    begin
      aFilename:=IncludeTrailingPathDelimiter(aFilename)+DefaultConfigFile;
      if TryConfig(aFilename) then exit;
    end;
  end;

  // finally try global directory
  {$IFDEF Unix}
  if TryConfig('/etc/'+DefaultConfigFile) then exit;
  {$ENDIF}
end;

procedure TPas2jsCompiler.ParamFatal(Msg: string);
begin
  Log.LogPlain(['Fatal: ',Msg]);
  Terminate(ExitCodeErrorInParams);
end;

procedure TPas2jsCompiler.ReadParam(Param: string; Quick, FromCmdLine: boolean);

  procedure UnknownParam;
  begin
    ParamFatal('unknown parameter "'+Param+'". Use -h for help.');
  end;

  procedure AppendInfo(var Value: string; Add: string);
  begin
    if Value<>'' then
      Value:=Value+' ';
    Value:=Value+Add;
  end;

var
  p: PChar;
  EnabledFlags, DisabledFlags, Identifier, Value, aFilename, ErrorMsg: string;
  i: Integer;
  c: Char;
  aProc, pr: TPasToJsProcessor;
  Enable, Found: Boolean;
  aPlatform, pl: TPasToJsPlatform;
  PF: TPas2JSPrecompileFormat;
begin
  //writeln('TPas2jsCompiler.ReadParam ',Param,' ',Quick,' ',FromCmdLine);
  if ShowDebug then
    if Quick then
      Log.LogMsgIgnoreFilter(nQuickHandlingOption,[QuoteStr(Param)])
    else
      Log.LogMsgIgnoreFilter(nHandlingOption,[QuoteStr(Param)]);
  if Param='' then exit;
  ParamMacros.Substitute(Param,Self);
  if Param='' then exit;

  if Quick and ((Param='-h') or (Param='-?') or (Param='--help')) then
  begin
    WriteHelp;
    Terminate(0);
  end;

  p:=PChar(Param);
  case p^ of
  '-':
    begin
      inc(p);
      case p^ of
      'i':
        begin
          // write information and halt
          inc(p);
          Value:='';
          repeat
            case p^ of
            #0:
              if p-PChar(Param)=length(Param) then
                begin
                if length(Param)=2 then
                  WriteInfo;
                break;
                end;
            'D': // wite compiler date
              AppendInfo(Value,GetCompiledDate);
            'V': // write short version
              AppendInfo(Value,GetVersion(true));
            'W': // write long version
              AppendInfo(Value,GetVersion(false));
            'S':
              begin
              inc(p);
              case p^ of
              #0:
                ParamFatal('missing info option after S in "'+Param+'".');
              'O': // write source OS
                AppendInfo(Value,GetCompiledTargetOS);
              'P': // write source processor
                AppendInfo(Value,GetCompiledTargetCPU);
              else
                ParamFatal('unknown info option S"'+p^+'" in "'+Param+'".');
              end;
              end;
            'T':
              begin
              inc(p);
              case p^ of
              #0:
                ParamFatal('missing info option after T in "'+Param+'".');
              'O': // write target platform
                AppendInfo(Value,PasToJsPlatformNames[TargetPlatform]);
              'P': // write target processor
                AppendInfo(Value,PasToJsProcessorNames[TargetProcessor]);
              else
                ParamFatal('unknown info option S"'+p^+'" in "'+Param+'".');
              end;
              end;
            'c':
              // write list of supported JS processors
              for pr in TPasToJsProcessor do
                Log.LogPlain(PasToJsProcessorNames[pr]);
            'o':
              begin
              // write list of optimizations
              Log.LogPlain('EnumNumbers');
              Log.LogPlain('RemoveNotUsedPrivates');
              Log.LogPlain('RemoveNotUsedDeclarations');
              end;
            't':
              // write list of supported targets
              for pl in TPasToJsPlatform do
                Log.LogPlain(PasToJsPlatformNames[pl]);
            else
              ParamFatal('unknown info option "'+p^+'" in "'+Param+'".');
            end;
            inc(p);
          until false;
          if Value<>'' then
            Log.LogPlain(Value);
          Terminate(0);
        end;
      'B','l','n':
        begin
          ReadSingleLetterOptions(Param,p,'Bln',EnabledFlags,DisabledFlags);
          for i:=1 to length(EnabledFlags) do begin
            case EnabledFlags[i] of
            'B': Options:=Options+[coBuildAll];
            'l': ShowLogo:=true;
            'n': SkipDefaultConfig:=true;
            end;
          end;
          for i:=1 to length(DisabledFlags) do begin
            case DisabledFlags[i] of
            'B': Options:=Options-[coBuildAll];
            'l': ShowLogo:=false;
            'n': SkipDefaultConfig:=false;
            end;
          end;
        end;
      'C': // code generation
        begin
          inc(p);
          ReadCodeGenerationFlags(Param,p);
        end;
      'd': // define
        if not Quick then
        begin
          Identifier:=copy(Param,3,length(Param));
          i:=Pos(':=',Identifier);
          if i>0 then
          begin
            Value:=copy(Identifier,i+2,length(Identifier));
            Identifier:=LeftStr(Identifier,i-1);
            if not IsValidIdent(Identifier) then
              ParamFatal('invalid define name (-d): "'+Param+'"');
            AddDefine(Identifier,Value);
          end else begin
            if not IsValidIdent(Identifier) then
              ParamFatal('invalid define (-d): "'+Param+'"');
            AddDefine(Identifier);
          end;
        end;
      'F': // folders and search paths
        begin
          inc(p);
          c:=p^;
          inc(p);
          case c of
          'e': Log.OutputFilename:=String(p);
          'E': FileCache.MainOutputPath:=String(p);
          'i': if not FileCache.AddIncludePaths(String(p),FromCmdLine,ErrorMsg) then
                 ParamFatal('invalid include path (-Fi) "'+ErrorMsg+'"');
          'N': if not FileCache.AddNamespaces(String(p),FromCmdLine,ErrorMsg) then
                 ParamFatal('invalid namespace (-FN) "'+ErrorMsg+'"');
          'r': if not Quick then
                 Log.Log(mtNote,'-Fr not yet implemented');
          'u': if not FileCache.AddUnitPaths(String(p),FromCmdLine,ErrorMsg) then
                 ParamFatal('invalid unit path (-Fu) "'+ErrorMsg+'"');
          'U': FileCache.UnitOutputPath:=String(p);
          else UnknownParam;
          end;
        end;
      'I': // include path, same as -Fi
        if not Quick then
        begin
          inc(p);
          if not FileCache.AddIncludePaths(String(p),FromCmdLine,ErrorMsg) then
            ParamFatal('invalid include path (-I) "'+ErrorMsg+'"');
        end;
      'J': // extra pas2js options
        begin
          inc(p);
          c:=p^;
          inc(p);
          case c of
          'c':
            begin
              if p^='-' then
              begin
                FileCache.AllJSIntoMainJS:=false;
                inc(p);
              end else
                FileCache.AllJSIntoMainJS:=true;
              if p^<>#0 then
                ParamFatal('invalid value (-Jc) "'+String(p)+'"');
            end;
          'e':
            begin
            Identifier:=NormalizeEncoding(String(p));
            case Identifier of
            'console','system','utf8', 'json':
              if Log.Encoding<>Identifier then begin
                Log.Encoding:=Identifier;
                if FHasShownEncoding then begin
                  FHasShownEncoding:=false;
                  WriteEncoding;
                end;
              end;
            else ParamFatal('invalid encoding (-Je) "'+String(p)+'"');
            end;
            end;
          'i':
            if p^=#0 then
              ParamFatal('missing insertion file "'+Param+'"')
            else if not Quick then
            begin
              aFilename:=String(p);
              if aFilename='' then
                UnknownParam;
              if aFilename[length(aFilename)]='-' then
              begin
                Delete(aFilename,length(aFilename),1);
                if aFilename='' then
                  UnknownParam;
                FileCache.RemoveInsertJSFilename(aFilename);
              end else
                FileCache.AddInsertJSFilename(aFilename);
            end;
          'l': SetOption(coLowerCase,p^<>'-');
          'm':
            // source map options
            if p^=#0 then
              SrcMapEnable:=true
            else if p^='-' then
              begin
              if p[1]<>#0 then
                UnknownParam;
              SrcMapEnable:=false;
              end
            else
              begin
              Value:=String(p);
              if Value='include' then
                SrcMapInclude:=true
              else if Value='include-' then
                SrcMapInclude:=false
              else if Value='xssiheader' then
                SrcMapXSSIHeader:=true
              else if Value='xssiheader-' then
                SrcMapXSSIHeader:=false
              else
                begin
                i:=Pos('=',Value);
                if i<1 then
                  UnknownParam;
                Identifier:=LeftStr(Value,i-1);
                Delete(Value,1,i);
                if Identifier='sourceroot' then
                  SrcMapSourceRoot:=Value
                else if Identifier='basedir' then
                  SrcMapBaseDir:=Value
                else
                  UnknownParam;
                end;
              // enable source maps when setting any -Jm<x> option
              SrcMapEnable:=true;
              end;
          'o':
            begin
              // -Jo<flag>
              Identifier:=String(p);
              if Identifier='' then
                ParamFatal('missing value of -Jo option');
              inc(p,length(Identifier));
              Enable:=true;
              c:=Identifier[length(Identifier)];
              if c in ['+','-'] then
              begin
                Enable:=c='+';
                Delete(Identifier,length(Identifier),1);
              end;
              if SameText(Identifier,'SearchLikeFPC') then
                FileCache.SearchLikeFPC:=Enable
              else if SameText(Identifier,'UseStrict') then
                SetOption(coUseStrict,Enable)
              else
                UnknownParam;
            end;
          'u':
            if not Quick then
              if not FileCache.AddSrcUnitPaths(String(p),FromCmdLine,ErrorMsg) then
                ParamFatal('invalid foreign unit path (-Ju) "'+ErrorMsg+'"');
          'U':
            begin
            Value:=String(p);
            Found:=false;
            for i:=0 to PrecompileFormats.Count-1 do
            begin
              PF:=PrecompileFormats[i];
              if not SameText(Value,PF.Ext) then continue;
              FileCache.PrecompileFormat:=PrecompileFormats[i];
              Found:=true;
            end;
            if not Found then
              ParamFatal('invalid precompile output format (-JU) "'+Value+'"');
            end;
          else UnknownParam;
          end;
        end;
      'M': // syntax mode
        begin
          inc(p);
          Identifier:=String(p);
          if CompareText(Identifier,'delphi')=0 then Mode:=p2jmDelphi
          else if CompareText(Identifier,'objfpc')=0 then Mode:=p2jmObjFPC
          else ParamFatal('invalid syntax mode  (-M) "'+Identifier+'"');
        end;
      'N':
        begin
          inc(p);
          case p^ of
          'S':
            begin
            Log.Log(mtWarning,'obsolete option -NS, use -FN instead');
            if not FileCache.AddNamespaces(String(p+1),FromCmdLine,ErrorMsg) then
              ParamFatal('invalid namespace (-NS) "'+ErrorMsg+'"');
            end;
          else UnknownParam;
          end;
        end;
      'o': // output file, main JavaScript file
        begin
          inc(p);
          aFilename:=String(p);
          if aFilename='' then
            ParamFatal('invalid empty output file (-o)')
          else if aFilename='..' then
            ParamFatal('invalid output file (-o) "'+aFilename+'"')
          else if aFilename='.' then
            // ok, stdout
          else
            aFilename:=ExpandFileName(aFilename);
          FileCache.MainJSFile:=aFilename;
        end;
      'O': // optimizations
        begin
        inc(p);
        case p^ of
        '-':
          begin
          inc(p);
          Options:=Options-coO1Enable+coO1Disable;
          end;
        '1':
          begin
          inc(p);
          Options:=Options+coO1Enable-coO1Disable;
          end;
        'o':
          begin
          inc(p);
          Identifier:=String(p);
          if Identifier='' then
            ParamFatal('missing -Oo option');
          inc(p,length(Identifier));
          Enable:=true;
          c:=Identifier[length(Identifier)];
          if c in ['+','-'] then
          begin
            Enable:=c='+';
            Delete(Identifier,length(Identifier),1);
          end;
          if CompareText(Identifier,'EnumNumbers')=0 then
            SetOption(coEnumValuesAsNumbers,Enable)
          else if CompareText(Identifier,'RemoveNotUsedPrivates')=0 then
            SetOption(coKeepNotUsedPrivates,not Enable)
          else if CompareText(Identifier,'RemoveNotUsedDeclarations')=0 then
            SetOption(coKeepNotUsedDeclarationsWPO,not Enable)
          else
            UnknownParam;
          end;
        else
          UnknownParam;
        end;
        if p-PChar(Param)<length(Param) then
          UnknownParam;
        end;
      'P': // target processor
        begin
        inc(p);
        Identifier:=String(p);
        for aProc in TPasToJsProcessor do
          if SameText(Identifier,PasToJsProcessorNames[aProc]) then
            begin
            TargetProcessor:=aProc;
            Identifier:='';
            break;
            end;
        if Identifier<>'' then
          ParamFatal('invalid target processor (-P) "'+Identifier+'"');
        end;
      'S': // Syntax
        begin
          inc(p);
          if p^='I' then
            begin
            Identifier:=String(p);
            if SameText(Identifier,'com') then
              InterfaceType:=citCom
            else if SameText(Identifier,'corba') then
              InterfaceType:=citCorba
            else
              ParamFatal('invalid interface style (-SI) "'+Identifier+'"');
            end
          else
            ReadSyntaxFlags(Param,p);
        end;
      'T': // target platform
        begin
        inc(p);
        Identifier:=String(p);
        for aPlatform in TPasToJsPlatform do
          if SameText(Identifier,PasToJsPlatformNames[aPlatform]) then
            begin
            TargetPlatform:=aPlatform;
            Identifier:='';
            break;
            end;
        if Identifier<>'' then
          ParamFatal('invalid target platform (-T) "'+Identifier+'"');
        end;
      'u': // undefine
        if not Quick then
        begin
          inc(p);
          Identifier:=String(p);
          if not IsValidIdent(Identifier) then
            ParamFatal('invalid undefine (-u): "'+Identifier+'"');
          RemoveDefine(Identifier);
        end;
      'v': // verbose
        begin
          inc(p);
          ReadVerbosityFlags(Param,p);
        end;
      else
        UnknownParam;
      end;
    end;
  '@':
    if not Quick then
    begin
      // load extra config file
      aFilename:=copy(Param,2,length(Param));
      if aFilename='' then
        ParamFatal('invalid config file at param position '+IntToStr(i));
      aFilename:=ExpandFileName(aFilename);
      if not DirectoryCache.FileExists(aFilename) then
        ParamFatal('config file not found: "'+copy(Param,2,length(Param))+'"');
      LoadConfig(aFilename);
    end;
  else
    // filename
    if (not Quick) then
    begin
      if not FromCmdLine then
        CfgSyntaxError('invalid parameter');
      if FileCache.MainSrcFile<>'' then
        ParamFatal('Only one Pascal file is supported, but got "'+FileCache.MainSrcFile+'" and "'+Param+'".');
      aFilename:=ExpandFileName(Param);
      FileCache.MainSrcFile:=aFilename;
    end;
  end;
end;

procedure TPas2jsCompiler.ReadSingleLetterOptions(const Param: string; p: PChar;
  const Allowed: string; out Enabled, Disabled: string);
// e.g. 'B' 'lB' 'l-' 'l+B-'
var
  Letter: Char;
  i: SizeInt;
begin
  if p^=#0 then
    ParamFatal('Invalid option "'+Param+'"');
  Enabled:='';
  Disabled:='';
  repeat
    Letter:=p^;
    if Letter='-' then
      ParamFatal('Invalid option "'+Param+'"');
    if Pos(Letter,Allowed)<1 then
      ParamFatal('unknown option "'+Param+'". Use -h for help.');
    inc(p);
    if p^='-' then
    begin
      // disable
      if Pos(Letter,Disabled)<1 then Disabled+=Letter;
      i:=Pos(Letter,Enabled);
      if i>0 then Delete(Enabled,i,1);
      inc(p);
    end else begin
      // enable
      if Pos(Letter,Enabled)<1 then Enabled+=Letter;
      i:=Pos(Letter,Disabled);
      if i>0 then Delete(Disabled,i,1);
      if p^='+' then inc(p);
    end;
  until p^=#0;
end;

procedure TPas2jsCompiler.ReadCodeGenerationFlags(Param: String; p: PChar);
var
  Enabled, Disabled: string;
  i: Integer;
begin
  ReadSingleLetterOptions(Param,p,'orR',Enabled,Disabled);
  for i:=1 to length(Enabled) do begin
    case Enabled[i] of
    'o': Options:=Options+[coOverflowChecks];
    'r': Options:=Options+[coRangeChecks];
    'R': Options:=Options+[coObjectChecks];
    end;
  end;
  for i:=1 to length(Disabled) do begin
    case Disabled[i] of
    'o': Options:=Options-[coOverflowChecks];
    'r': Options:=Options-[coRangeChecks];
    'R': Options:=Options-[coObjectChecks];
    end;
  end;
end;

procedure TPas2jsCompiler.ReadSyntaxFlags(Param: String; p: PChar);
var
  Enabled, Disabled: string;
  i: Integer;
begin
  ReadSingleLetterOptions(Param,p,'2acdm',Enabled,Disabled);
  for i:=1 to length(Enabled) do begin
    case Enabled[i] of
    '2': Mode:=p2jmObjFPC;
    'a': Options:=Options+[coAssertions];
    'c': Options:=Options+[coAllowCAssignments];
    'd': Mode:=p2jmDelphi;
    'm': Options:=Options+[coAllowMacros];
    end;
  end;
  for i:=1 to length(Disabled) do begin
    case Disabled[i] of
    '2': ;
    'a': Options:=Options-[coAssertions];
    'c': Options:=Options-[coAllowCAssignments];
    'd': ;
    'm': Options:=Options-[coAllowMacros];
    end;
  end;
end;

procedure TPas2jsCompiler.ReadVerbosityFlags(Param: String; p: PChar);
var
  Enabled, Disabled: string;
  i: Integer;
begin
  if p^='m' then
  begin
    // read m-flags
    repeat
      inc(p);
      if not (p^ in ['0'..'9']) then
        ParamFatal('missing number in "'+Param+'"');
      i:=0;
      while p^ in ['0'..'9'] do begin
        i:=i*10+ord(p^)-ord('0');
        if i>99999 then
          ParamFatal('Invalid -vm parameter in "'+Param+'"');
        inc(p);
      end;
      Log.MsgNumberDisabled[i]:=p^<>'-';
      if p^='-' then inc(p);
      if p^=#0 then break;
      if p^<>',' then
        ParamFatal('Invalid option "'+Param+'"');
    until false;
    exit;
  end;

  // read other flags
  ReadSingleLetterOptions(Param,p,'ewnhila0bctdqxvz',Enabled,Disabled);
  for i:=1 to length(Enabled) do begin
    case Enabled[i] of
    'e': Options:=Options+[coShowErrors];
    'w': Options:=Options+[coShowWarnings];
    'n': Options:=Options+[coShowNotes];
    'h': Options:=Options+[coShowHints];
    'i': Options:=Options+[coShowInfos];
    'l': Options:=Options+[coShowLineNumbers];
    'a': Options:=Options+coShowAll;
    '0': Options:=Options-coShowAll+[coShowErrors];
    'b': ShowFullPaths:=true;
    'c': Options:=Options+[coShowConditionals,coShowInfos];
    't': ShowTriedUsedFiles:=true;
    'd': ShowDebug:=true;
    'q': Options:=Options+[coShowMessageNumbers];
    'x': Options:=Options+[coShowUsedTools];
    'v': Options:=Options+[coWriteDebugLog];
    'z': WriteMsgToStdErr:=true;
    end;
  end;
  for i:=1 to length(Disabled) do begin
    case Disabled[i] of
    'e': Options:=Options-[coShowErrors];
    'w': Options:=Options-[coShowWarnings];
    'n': Options:=Options-[coShowNotes];
    'h': Options:=Options-[coShowHints];
    'i': Options:=Options-[coShowInfos];
    'l': Options:=Options-[coShowLineNumbers];
    'a': ;
    '0': ;
    'b': ShowFullPaths:=false;
    'c': Options:=Options-[coShowConditionals];
    't': ShowTriedUsedFiles:=false;
    'd': ShowDebug:=false;
    'q': Options:=Options-[coShowMessageNumbers];
    'x': Options:=Options-[coShowUsedTools];
    'v': Options:=Options+[coWriteDebugLog];
    'z': WriteMsgToStdErr:=false;
    end;
  end;
end;

procedure TPas2jsCompiler.RegisterMessages;
var
  LastMsgNumber: integer;

  procedure r(MsgType: TMessageType; MsgNumber: integer; const MsgPattern: string);
  var
    s: String;
  begin
    if (LastMsgNumber>=0) and (MsgNumber<>LastMsgNumber+1) then
      begin
      s:='TPas2jsCompiler.RegisterMessages: gap in registered message numbers: '+IntToStr(LastMsgNumber)+' '+IntToStr(MsgNumber);
      RaiseInternalError(20170504161422,s);
      end;
    Log.RegisterMsg(MsgType,MsgNumber,MsgPattern);
    LastMsgNumber:=MsgNumber;
  end;

begin
  LastMsgNumber:=-1;
  r(mtInfo,nOptionIsEnabled,sOptionIsEnabled);
  r(mtInfo,nSyntaxModeIs,sSyntaxModeIs);
  r(mtInfo,nMacroDefined,sMacroDefined);
  r(mtInfo,nUsingPath,sUsingPath);
  r(mtNote,nFolderNotFound,sFolderNotFound);
  r(mtInfo,nNameValue,sNameValue);
  r(mtInfo,nReadingOptionsFromFile,sReadingOptionsFromFile);
  r(mtInfo,nEndOfReadingConfigFile,sEndOfReadingConfigFile);
  r(mtDebug,nInterpretingFileOption,sInterpretingFileOption);
  r(mtFatal,nSourceFileNotFound,sSourceFileNotFound);
  r(mtFatal,nFileIsFolder,sFileIsFolder);
  r(mtInfo,nConfigFileSearch,sConfigFileSearch);
  r(mtDebug,nHandlingOption,sHandlingOption);
  r(mtDebug,nQuickHandlingOption,sQuickHandlingOption);
  r(mtFatal,nOutputDirectoryNotFound,sOutputDirectoryNotFound);
  r(mtError,nUnableToWriteFile,sUnableToWriteFile);
  r(mtInfo,nWritingFile,sWritingFile);
  r(mtFatal,nCompilationAborted,sCompilationAborted);
  r(mtDebug,nCfgDirective,sCfgDirective);
  r(mtError,nUnitCycle,sUnitCycle);
  r(mtError,nOptionForbidsCompile,sOptionForbidsCompile);
  r(mtInfo,nUnitNeedsCompileDueToUsedUnit,sUnitsNeedCompileDueToUsedUnit);
  r(mtInfo,nUnitNeedsCompileDueToOption,sUnitsNeedCompileDueToOption);
  r(mtInfo,nUnitNeedsCompileJSMissing,sUnitsNeedCompileJSMissing);
  r(mtInfo,nUnitNeedsCompilePasHasChanged,sUnitsNeedCompilePasHasChanged);
  r(mtInfo,nParsingFile,sParsingFile);
  r(mtInfo,nCompilingFile,sCompilingFile);
  r(mtError,nExpectedButFound,sExpectedButFound);
  r(mtInfo,nLinesInFilesCompiled,sLinesInFilesCompiled);
  r(mtInfo,nTargetPlatformIs,sTargetPlatformIs);
  r(mtInfo,nTargetProcessorIs,sTargetProcessorIs);
  r(mtInfo,nMessageEncodingIs,sMessageEncodingIs);
  r(mtError,nUnableToTranslatePathToDir,sUnableToTranslatePathToDir);
  r(mtInfo,nSrcMapSourceRootIs,sSrcMapSourceRootIs);
  r(mtInfo,nSrcMapBaseDirIs,sSrcMapBaseDirIs);
  r(mtFatal,nUnitFileNotFound,sUnitFileNotFound);
  r(mtInfo,nClassInterfaceStyleIs,sClassInterfaceStyleIs);
  r(mtInfo,nMacroXSetToY,sMacroXSetToY);
  Pas2jsPParser.RegisterMessages(Log);
end;

constructor TPas2jsCompiler.Create;
begin
  FOptions:=DefaultP2jsCompilerOptions;
  FLog:=TPas2jsLogger.Create;
  FParamMacros:=TPas2jsMacroEngine.Create;
  RegisterMessages;

  FFileCache:=TPas2jsFilesCache.Create(Log);
  FFileCache.BaseDirectory:=GetCurrentDirUTF8;
  FFileCacheAutoFree:=true;
  FDirectoryCache:=FFileCache.DirectoryCache;
  FLog.OnFormatPath:=@FileCache.FormatPath;

  FDefines:=TStringList.Create;
  // Done by Reset: TStringList(FDefines).Sorted:=True;
  // Done by Reset: TStringList(FDefines).Duplicates:=dupError;
  FPrecompileInitialFlags:=TPCUInitialFlags.Create;

  FConditionEval:=TCondDirectiveEvaluator.Create;
  FConditionEval.OnLog:=@ConditionEvalLog;
  FConditionEval.OnEvalVariable:=@ConditionEvalVariable;
  //FConditionEval.OnEvalFunction:=@ConditionEvalFunction;

  FFiles:=TAVLTree.Create(@CompareCompilerFilesPasFile);
  FReadingModules:=TFPList.Create;
  FUnits:=TAVLTree.Create(@CompareCompilerFilesPasUnitname);

  InitParamMacros;
  Reset;
end;

destructor TPas2jsCompiler.Destroy;

  procedure FreeStuff;
  begin
    FreeAndNil(FPrecompileInitialFlags);
    FreeAndNil(FWPOAnalyzer);

    FMainFile:=nil;
    FreeAndNil(FUnits);
    FreeAndNil(FReadingModules);
    FFiles.FreeAndClear;
    FreeAndNil(FFiles);

    ClearDefines;
    FreeAndNil(FDefines);
    FreeAndNil(FConditionEval);

    FLog.OnFormatPath:=nil;
    if FFileCacheAutoFree then
      FreeAndNil(FFileCache)
    else
      FFileCache:=nil;
    FDirectoryCache:=nil;

    FreeAndNil(FParamMacros);
  end;

begin
  if ShowDebug then
    try
      FreeStuff;
    except
      on E: Exception do
        Log.LogExceptionBackTrace;
    end
  else
    FreeStuff;

  FreeAndNil(FLog);
  inherited Destroy;
end;

function TPas2jsCompiler.OnMacroCfgDir(Sender: TObject; var Params: string;
  Lvl: integer): boolean;
begin
  if Lvl=0 then ;
  Params:=ExtractFilePath(CurrentCfgFilename);
  Result:=true;
end;

function TPas2jsCompiler.OnMacroEnv(Sender: TObject; var Params: string;
  Lvl: integer): boolean;
begin
  if Lvl=0 then ;
  Params:=GetEnvironmentVariableUTF8(Params);
  Result:=true;
end;

procedure TPas2jsCompiler.AddDefine(const aName: String);
begin
  if FDefines.IndexOf(aName)>=0 then exit;
  FDefines.Add(aName);
end;

procedure TPas2jsCompiler.AddDefine(const aName, Value: String);
var
  Index: Integer;
  M: TMacroDef;
begin
  Index:=FDefines.IndexOf(aName);
  If (Index<0) then
    FDefines.AddObject(aName,TMacroDef.Create(aName,Value))
  else begin
    M:=TMacroDef(FDefines.Objects[Index]);
    if M=nil then
      FDefines.Objects[Index]:=TMacroDef.Create(aName,Value)
    else
      M.Value:=Value;
  end;
end;

procedure TPas2jsCompiler.RemoveDefine(const aName: String);
var
  i: Integer;
  M: TMacroDef;
begin
  i:=FDefines.IndexOf(aName);
  if (i<>-1) then
  begin
    M:=TMacroDef(FDefines.Objects[i]);
    M.Free;
    FDefines.Delete(i);
  end;
end;

function TPas2jsCompiler.IsDefined(const aName: String): boolean;
begin
  Result:=FDefines.IndexOf(aName)>=0;
end;

class function TPas2jsCompiler.GetVersion(ShortVersion: boolean): string;
begin
  Result:=IntToStr(VersionMajor)+'.'+IntToStr(VersionMinor)+'.'+IntToStr(VersionRelease);
  if not ShortVersion then
    Result+=VersionExtra;
end;

procedure TPas2jsCompiler.Reset;
begin
  FreeAndNil(FWPOAnalyzer);

  FPrecompileGUID:=default(TGUID);
  FPrecompileInitialFlags.Clear;

  FMainFile:=nil;
  FUnits.Clear;
  FReadingModules.Clear;
  FFiles.FreeAndClear;

  FCompilerExe:='';
  FOptions:=DefaultP2jsCompilerOptions;
  FMode:=p2jmObjFPC;
  FTargetPlatform:=PlatformBrowser;
  FTargetProcessor:=ProcessorECMAScript5;

  Log.Reset;
  Log.ShowMsgTypes:=GetShownMsgTypes;

  ClearDefines;
  TStringList(FDefines).Sorted:=True;
  TStringList(FDefines).Duplicates:=dupError;

  AddDefine('PAS2JS');
  AddDefine('PAS2JS_FULLVERSION',IntToStr((VersionMajor*100+VersionMinor)*100+VersionRelease));
  AddDefinesForTargetPlatform;
  AddDefinesForTargetProcessor;
  // add FPC compatibility flags
  AddDefine('FPC_HAS_FEATURE_CLASSES');
  AddDefine('FPC_HAS_FEATURE_INIT');
  AddDefine('FPC_HAS_FEATURE_DYNARRAYS');
  AddDefine('FPC_HAS_FEATURE_EXCEPTIONS');
  AddDefine('FPC_HAS_FEATURE_EXITCODE');
  AddDefine('FPC_HAS_FEATURE_INITFINAL');
  AddDefine('FPC_HAS_FEATURE_RTTI');
  AddDefine('FPC_HAS_FEATURE_SUPPORT');
  AddDefine('FPC_HAS_FEATURE_UNICODESTRINGS');
  AddDefine('FPC_HAS_FEATURE_WIDESTRINGS');
  AddDefine('FPC_HAS_TYPE_DOUBLE');
  AddDefine('FPC_HAS_UNICODESTRING');
  AddDefine('FPC_UNICODESTRINGS');
  AddDefine('FPC_WIDESTRING_EQUAL_UNICODESTRING');
  AddDefine('STR_CONCAT_PROCS');
  AddDefine('UNICODE');

  FHasShownLogo:=false;
  FHasShownEncoding:=false;
  FFileCache.Reset;
end;

procedure TPas2jsCompiler.Run(aCompilerExe: string; aWorkingDir: string;
  ParamList: TStrings; DoReset: boolean);
var
  i: Integer;
  StartTime: TDateTime;
begin
  StartTime:=Now;

  if DoReset then Reset;
  if FileCount>0 then
    RaiseInternalError(20170504161340,'internal error: TPas2jsCompiler.Run FileCount>0');

  // ste working directory, need by all relative filenames
  FileCache.BaseDirectory:=aWorkingDir;

  CompilerExe:=aCompilerExe; // maybe needed to find the default config

  // quick check command line params
  for i:=0 to ParamList.Count-1 do
    ReadParam(ParamList[i],true,true);
  if WriteDebugLog then
    Log.OpenDebugLog;
  if ShowLogo then
    WriteLogo;

  // read default config
  if not SkipDefaultConfig then
    LoadDefaultConfig;

  // read command line parameters
  for i:=0 to ParamList.Count-1 do
    ReadParam(ParamList[i],false,true);

  // now we know, if the logo can be displayed
  if ShowLogo then
    WriteLogo;

  // show debug info
  if ShowDebug then
  begin
    WriteOptions;
    WriteDefines;
  end;
  if ShowDebug or ShowTriedUsedFiles then
    WriteFoldersAndSearchPaths;

  if FileCache.MainSrcFile='' then
    ParamFatal('No source file name in command line');
  if not DirectoryCache.FileExists(FileCache.MainSrcFile) then
    ParamFatal('Pascal file not found: "'+FileCache.MainSrcFile+'"');

  // compile
  try
    Compile(StartTime);
  except
    on E: ECompilerTerminate do
    begin
    end else begin
      if ShowDebug then
        Log.LogExceptionBackTrace;
      raise;
    end;
  end;
end;

procedure TPas2jsCompiler.WriteHelp;
const
  MaxLineLen = 78;
  Indent = 12;

  procedure l(s: string);
  var
    p, LastCharStart, WordBreak: PChar;
    Len: integer;
    CodePointCount: Integer;

    procedure InitLine;
    begin
      p:=PChar(s);
      LastCharStart:=p;
      WordBreak:=nil;
      CodePointCount:=0;
    end;

  begin
    if length(s)<=MaxLineLen then
    begin
      Log.LogRaw(s);
      exit;
    end;
    InitLine;
    repeat
      case p^ of
      #0:
        if p-PChar(s)=length(s) then
          break
        else
          inc(p);
      'a'..'z','A'..'Z','0'..'9','_','-','.',',','"','''','`',#128..#255:
        begin
        LastCharStart:=p;
        Len:=UTF8CharacterStrictLength(p);
        if Len=0 then Len:=1;
        inc(p,Len);
        end;
      else
        LastCharStart:=p;
        WordBreak:=p;
        inc(p);
      end;
      inc(CodePointCount);
      if CodePointCount>=MaxLineLen then
      begin
        if (WordBreak=nil) or (WordBreak-PChar(s)<MaxLineLen div 3) then
          WordBreak:=LastCharStart;
        Len:=WordBreak-PChar(s);
        Log.LogRaw(LeftStr(s,Len));
        Delete(s,1,len);
        s:=Space(Indent)+Trim(s);
        InitLine;
      end;
    until false;
    Log.LogRaw(s);
  end;

var
  i: Integer;
  ParamMacro: TPas2jsMacro;
begin
  WriteLogo;
  Log.LogLn;
  if CompilerExe<>'' then
  begin
    l('Usage: '+CompilerExe+' <your.pas>');
  end else begin
    l('Usage: pas2js <your.pas>');
  end;
  Log.LogLn;
  l('Options:');
  l('Put + after a boolean switch option to enable it, - to disable it');
  l('  @<x>    : Read compiler options from file <x> in addition to the default '+DefaultConfigFile);
  l('  -B      : Rebuild all');
  l('  -d<x>   : Defines the symbol <x>. Optional: -d<x>:=<value>');
  l('  -i<x>   : Write information and halt. <x> is a combination of the following:');
  l('    -iD   : Write compiler date');
  l('    -iSO  : Write compiler OS');
  l('    -iSP  : Write compiler host processor');
  l('    -iTO  : Write target platform');
  l('    -iTP  : Write target processor');
  l('    -iV   : Write short compiler version');
  l('    -iW   : Write full compiler version');
  l('    -ic   : Write list of supported JS processors usable by -P<x>');
  l('    -io   : Write list of supported optimizations usable by -Oo<x>');
  l('    -it   : Write list of supported targets usable by -T<x>');
  l('  -C<x>   : Code generation options. <x> is a combination of the following letters:');
  // -C3        Turn on ieee error checking for constants
  l('    o     : Overflow checking of integer operations');
  // -CO        Check for possible overflow of integer operations
  l('    r     : Range checking');
  l('    R     : Object checks. Verify method calls and object type casts.');
  l('  -F...   Set file names and paths:');
  l('   -Fe<x> : Redirect output to file <x>. UTF-8 encoded.');
  l('   -FE<x> : Set main output path to <x>');
  l('   -Fi<x> : Add <x> to include paths');
  l('   -FN<x> : add <x> to namespaces. Namespaces with trailing - are removed.');
  l('            Delphi calls this flag "unit scope names".');
  //l('   -Fr<x> : Load error message file <x>');
  l('   -Fu<x> : Add <x> to unit paths');
  l('   -FU<x> : Set unit output path to <x>');
  l('  -I<x>   : Add <x> to include paths, same as -Fi');
  l('  -J...  Extra options of pas2js');
  l('   -Jc    : Write all JavaScript concatenated into the output file');
  l('   -Je<x> : Encode messages as <x>.');
  l('     -Jeconsole : Console codepage. This is the default.');
  l('     -Jesystem  : System codepage. On non Windows console and system are the same.');
  l('     -Jeutf-8   : Unicode UTF-8. Default when using -Fe.');
  l('     -JeJSON    : Output compiler messages as JSON. Logo etc are outputted as-is.');
  l('   -Ji<x> : Insert JS file <x> into main JS file. E.g. -Jirtl.js. Can be given multiple times. To remove a file name append a minus, e.g. -Jirtl.js-.');
  l('   -Jl    : lower case identifiers');
  l('   -Jm    : generate source maps');
  l('     -Jmsourceroot=<x> : use x as "sourceRoot", prefix URL for source file names.');
  l('     -Jmbasedir=<x> : write source file names relative to directory x.');
  l('     -Jminclude : include Pascal sources in source map.');
  l('     -Jmxssiheader : start source map with XSSI protection )]}'', default.');
  l('     -Jm- : disable generating source maps');
  l('   -Jo<x> : Enable or disable extra option. The x is case insensitive:');
  l('     -JoSearchLikeFPC : search source files like FPC, default: search case insensitive.');
  l('     -JoUseStrict : add "use strict" to modules, default.');
  l('   -Ju<x> : Add <x> to foreign unit paths. Foreign units are not compiled.');
  if PrecompileFormats.Count>0 then
  begin
    l('   -JU<x> : Create precompiled units in format x.');
    for i:=0 to PrecompileFormats.Count-1 do
      with PrecompileFormats[i] do
        l('     -JU'+Ext+' : '+Description);
    l('     -JU- : Disable prior -JU<x> option. Do not create precompiled units.');
  end;
  l('  -l      : Write logo');
  l('  -MDelphi: Delphi 7 compatibility mode');
  l('  -MObjFPC: FPC''s Object Pascal compatibility mode (default)');
  l('  -NS<x>  : obsolete: add <x> to namespaces. Same as -FN<x>');
  l('  -n      : Do not read the default config files');
  l('  -o<x>   : Change main JavaScript file to <x>, "." means stdout');
  l('  -O<x>   : Optimizations:');
  l('    -O-   : Disable optimizations');
  l('    -O1   : Level 1 optimizations (quick and debugger friendly)');
  //l('    -O2   : Level 2 optimizations (Level 1 + not debugger friendly)');
  l('    -Oo<x> : Enable or disable optimization. The x is case insensitive:');
  l('      -OoEnumNumbers[-] : write enum value as number instead of name. Default in -O1.');
  l('      -OoRemoveNotUsedPrivates[-] : Default is enabled');
  l('      -OoRemoveNotUsedDeclarations[-] : Default enabled for programs with -Jc');
  l('  -P<x>   : Set target processor. Case insensitive:');
  l('    -Pecmascript5 : default');
  l('    -Pecmascript6');
  l('  -S<x>   : Syntax options. <x> is a combination of the following letters:');
  l('    a     : Turn on assertions');
  l('    c     : Support operators like C (*=,+=,/= and -=)');
  l('    d     : Same as -Mdelphi');
  l('    m     : Enables macro replacements');
  l('    2     : Same as -Mobjfpc (default)');
  l('  -SI<x>   : Set interface style to <x>');
  l('    -SIcom   : COM compatible interface (default)');
  l('    -SIcorba : CORBA compatible interface');
  l('  -T<x>   : Set target platform');
  l('    -Tbrowser : default');
  l('    -Tnodejs  : add pas.run(), includes -Jc');
  l('  -u<x>   : Undefines the symbol <x>');
  l('  -v<x>   : Be verbose. <x> is a combination of the following letters:');
  l('    e     : Show errors (default)');
  l('    w     : Show warnings');
  l('    n     : Show notes');
  l('    h     : Show hints');
  l('    i     : Show info');
  l('    l     : Show line numbers, needs -vi');
  l('    a     : Show everything');
  l('    0     : Show nothing (except errors)');
  l('    b     : Show file names with full path');
  l('    c     : Show conditionals');
  l('    t     : Show tried/used files');
  l('    d     : Show debug notes and info, enables -vni');
  l('    q     : Show message numbers');
  l('    x     : Show used tools');
  l('    v     : Write pas2jsdebug.log with lots of debugging info');
  l('    z     : Write messages to stderr, -o. still uses stdout.');
  l('  -vm<x>,<y>: Do not show messages numbered <x> and <y>.');
  l('  -?      : Show this help');
  l('  -h      : Show this help');
  Log.LogLn;
  l('Macros: Format is $Name, $Name$ or $Name()');
  for i:=0 to ParamMacros.Count-1 do begin
    ParamMacro:=ParamMacros[i];
    Log.LogRaw(['  $',ParamMacro.Name,BoolToStr(ParamMacro.CanHaveParams,'()',''),': ',ParamMacro.Description]);
  end;
end;

procedure TPas2jsCompiler.WriteLogo;
begin
  if FHasShownLogo then exit;
  FHasShownLogo:=true;
  WriteVersionLine;
  Log.LogPlain('Copyright (c) 2018 Free Pascal team.');
  if coShowInfos in Options then
    WriteEncoding;
end;

procedure TPas2jsCompiler.WriteEncoding;
begin
  if FHasShownEncoding then exit;
  FHasShownEncoding:=true;
  Log.LogMsgIgnoreFilter(nMessageEncodingIs,[Log.GetEncodingCaption]);
end;

procedure TPas2jsCompiler.WriteVersionLine;
var
  s: String;
begin
  s:='Pas2JS Compiler version '+GetVersion(false);
  s:=s+' ['+{$i %Date%}+'] for '+{$i %FPCTargetOS%}+' '+{$i %FPCTargetCPU%};
  Log.LogPlain(s);
  if coShowInfos in Options then
    WriteEncoding;
end;

procedure TPas2jsCompiler.WriteOptions;
var
  co: TP2jsCompilerOption;
  fco: TP2jsFileCacheOption;
begin
  // message encoding
  WriteEncoding;
  // target platform
  Log.LogMsgIgnoreFilter(nTargetPlatformIs,[PasToJsPlatformNames[TargetPlatform]]);
  Log.LogMsgIgnoreFilter(nTargetProcessorIs,[PasToJsProcessorNames[TargetProcessor]]);
  // default syntax mode
  Log.LogMsgIgnoreFilter(nSyntaxModeIs,[p2jscModeNames[Mode]]);
  Log.LogMsgIgnoreFilter(nClassInterfaceStyleIs,[InterfaceTypeNames[InterfaceType]]);
  // boolean options
  for co in TP2jsCompilerOption do
    Log.LogMsgIgnoreFilter(nOptionIsEnabled,
      [p2jscoCaption[co],BoolToStr(co in Options,'enabled','disabled')]);
  for fco in TP2jsFileCacheOption do
    Log.LogMsgIgnoreFilter(nOptionIsEnabled,
      [p2jsfcoCaption[fco],BoolToStr(fco in FileCache.Options,'enabled','disabled')]);

  // source map options
  if SrcMapEnable then
  begin
    Log.LogMsgIgnoreFilter(nSrcMapSourceRootIs,[QuoteStr(SrcMapSourceRoot)]);
    Log.LogMsgIgnoreFilter(nSrcMapBaseDirIs,[QuoteStr(SrcMapBaseDir)]);
  end;
end;

procedure TPas2jsCompiler.WriteDefines;
var
  i: Integer;
  S: String;
  M: TMacroDef;
begin
  for i:=0 to Defines.Count-1 do
    begin
    S:=Defines[i];
    M:=TMacroDef(Defines.Objects[i]);
    if M<>nil then
      Log.LogMsgIgnoreFilter(nMacroXSetToY,[S,QuoteStr(M.Value)])
    else
      Log.LogMsgIgnoreFilter(nMacroDefined,[S]);
    end;
end;

procedure TPas2jsCompiler.WriteFoldersAndSearchPaths;

  procedure WriteFolder(aName, Folder: string);
  begin
    if Folder='' then exit;
    Log.LogMsgIgnoreFilter(nUsingPath,[aName,Folder]);
    if not DirectoryExists(ChompPathDelim(Folder)) then
      Log.LogMsgIgnoreFilter(nFolderNotFound,[aName,QuoteStr(Folder)]);
  end;

var
  i: Integer;
begin
  for i:=0 to FileCache.ForeignUnitPaths.Count-1 do
    WriteFolder('foreign unit path',FileCache.ForeignUnitPaths[i]);
  for i:=0 to FileCache.UnitPaths.Count-1 do
    WriteFolder('unit path',FileCache.UnitPaths[i]);
  for i:=0 to FileCache.Namespaces.Count-1 do
    Log.LogMsgIgnoreFilter(nUsingPath,['unit scope',FileCache.Namespaces[i]]);
  for i:=0 to FileCache.IncludePaths.Count-1 do
    WriteFolder('include path',FileCache.IncludePaths[i]);
  WriteFolder('unit output path',FileCache.UnitOutputPath);
  WriteFolder('main output path',FileCache.MainOutputPath);
  Log.LogMsgIgnoreFilter(nNameValue,['output file',QuoteStr(FileCache.MainJSFile)]);
end;

procedure TPas2jsCompiler.WriteInfo;
begin
  WriteVersionLine;
  Log.LogLn;
  Log.LogPlain('Compiler date      : '+GetCompiledDate);
  Log.LogPlain('Compiler CPU target: '+GetCompiledTargetCPU);
  Log.LogLn;
  Log.LogPlain('Supported targets (targets marked with ''{*}'' are under development):');
  Log.LogPlain(['  ',PasToJsPlatformNames[PlatformBrowser],': webbrowser']);
  Log.LogPlain(['  ',PasToJsPlatformNames[PlatformNodeJS],': Node.js']);
  Log.LogLn;
  Log.LogPlain('Supported CPU instruction sets:');
  Log.LogPlain('  ECMAScript5, ECMAScript6');
  Log.LogLn;
  Log.LogPlain('Recognized compiler and RTL features:');
  Log.LogPlain('  RTTI,CLASSES,EXCEPTIONS,EXITCODE,RANDOM,DYNARRAYS,COMMANDARGS,');
  Log.LogPlain('  UNICODESTRINGS');
  Log.LogLn;
  Log.LogPlain('Supported Optimizations:');
  Log.LogPlain('  EnumNumbers');
  Log.LogPlain('  RemoveNotUsedPrivates');
  Log.LogLn;
  Log.LogPlain('Supported Whole Program Optimizations:');
  Log.LogPlain('  RemoveNotUsedDeclarations');
  Log.LogLn;
  Log.LogPlain('This program comes under the Library GNU General Public License');
  Log.LogPlain('For more information read COPYING.FPC, included in this distribution');
  Log.LogLn;
  Log.LogPlain('Please report bugs in our bug tracker on:');
  Log.LogPlain('                 http://bugs.freepascal.org');
  Log.LogLn;
  Log.LogPlain('More information may be found on our WWW pages (including directions');
  Log.LogPlain('for mailing lists useful for asking questions or discussing potential');
  Log.LogPlain('new features, etc.):');
  Log.LogPlain('                 http://www.freepascal.org');
end;

function TPas2jsCompiler.GetShownMsgTypes: TMessageTypes;
begin
  Result:=[mtFatal];
  if coShowErrors in FOptions then Include(Result,mtError);
  if coShowWarnings in FOptions then Include(Result,mtWarning);
  if coShowNotes in FOptions then Include(Result,mtNote);
  if coShowHints in FOptions then Include(Result,mtHint);
  if coShowInfos in FOptions then Include(Result,mtInfo);
  if coShowDebug in FOptions then Include(Result,mtDebug);
end;

procedure TPas2jsCompiler.SetOption(Flag: TP2jsCompilerOption; Enable: boolean);
begin
  if Enable then
    Options:=Options+[Flag]
  else
    Options:=Options-[Flag];
end;

function TPas2jsCompiler.FindUnitWithFile(PasFilename: string): TPas2jsCompilerFile;
var
  Node: TAVLTreeNode;
begin
  Result:=nil;
  if PasFilename='' then exit;
  Node:=FFiles.FindKey(Pointer(PasFilename),@CompareFileAndCompilerFilePasFile);
  if Node=nil then exit;
  Result:=TPas2jsCompilerFile(Node.Data);
end;

procedure TPas2jsCompiler.LoadPasFile(UnitFilename, UseUnitName: string; out
  aFile: TPas2jsCompilerFile; aFormat: TPas2JSPrecompileFormat);
var
  aPasTree: TPas2jsCompilerResolver;
begin
  aFile:=nil;
  Log.LogMsg(nParsingFile,[FileCache.FormatPath(UnitFilename)],'',0,0,not (coShowLineNumbers in Options));

  aFile:=FindUnitWithFile(UnitFilename);
  if aFile<>nil then exit;

  if (UnitFilename='') or not DirectoryCache.FileExists(UnitFilename) then
  begin
    if aFormat=nil then
      Log.LogMsg(nSourceFileNotFound,[QuoteStr(UnitFilename)])
    else
      Log.LogMsg(nUnitFileNotFound,[QuoteStr(UnitFilename)]);
    Terminate(ExitCodeFileNotFound);
  end;

  UnitFilename:=ExpandFileName(UnitFilename);
  if DirectoryCache.DirectoryExists(UnitFilename) then
  begin
    Log.LogMsg(nFileIsFolder,[QuoteStr(UnitFilename)]);
    Terminate(ExitCodeFileNotFound);
  end;

  aFile:=TPas2jsCompilerFile.Create(Self,UnitFilename,aFormat);
  if UseUnitName<>'' then
    begin
    {$IFDEF VerboseSetPasUnitName}
    writeln('TPas2jsCompiler.LoadPasFile File="',aFile.PasFilename,'" UseUnit="',UseUnitName,'"');
    {$ENDIF}
    if CompareText(ExtractFilenameOnly(UnitFilename),UseUnitName)=0 then
      aFile.PasUnitName:=UseUnitName
    else
      aFile.PasUnitName:=ExtractFilenameOnly(UnitFilename);
    end;
  FFiles.Add(aFile);
  aFile.ShowDebug:=ShowDebug;
  if aFile.IsMainFile then
    aFile.JSFilename:=FileCache.GetResolvedMainJSFile;

  // pastree (engine)
  aPasTree:=aFile.PascalResolver;
  if coShowLineNumbers in Options then
    aPasTree.ScannerLogEvents:=aPasTree.ScannerLogEvents+[sleLineNumber];
  if coShowConditionals in Options then
    aPasTree.ScannerLogEvents:=aPasTree.ScannerLogEvents+[sleConditionals];
  if [coShowLineNumbers,coShowInfos,coShowDebug]*Options<>[] then
    aPasTree.ParserLogEvents:=aPasTree.ParserLogEvents+[pleInterface,pleImplementation];

  // scanner
  aFile.CreateScannerAndParser(FileCache.CreateResolver);

  if ShowDebug then
    Log.LogPlain(['Debug: Opening file "',UnitFilename,'"...']);
  if aFile.PCUFormat=nil then
  begin
    // open file (beware: this changes FileResolver.BaseDirectory)
    aFile.OpenFile(UnitFilename);
  end else begin
    aFile.FileResolver.BaseDirectory:=ExtractFilePath(UnitFilename);
    aFile.CreatePCUReader;
  end;
end;

function TPas2jsCompiler.FindUnitWithName(const TheUnitName: string
  ): TPas2jsCompilerFile;
var
  Node: TAVLTreeNode;
begin
  if not IsValidIdent(TheUnitName,true) then exit(nil);
  Node:=FUnits.FindKey(Pointer(TheUnitName),@CompareUnitnameAndCompilerFile);
  if Node=nil then
    Result:=nil
  else
    Result:=TPas2jsCompilerFile(Node.Data);
end;

procedure TPas2jsCompiler.AddUsedUnit(aFile: TPas2jsCompilerFile);
var
  OldFile: TPas2jsCompilerFile;
begin
  if aFile.PasUnitName='' then
    RaiseInternalError(20170504161347,'missing PasUnitName "'+aFile.PasFilename+'"');
  OldFile:=FindUnitWithName(aFile.PasUnitName);
  if OldFile<>nil then
  begin
    if OldFile<>aFile then
      RaiseInternalError(20170504161354,'duplicate unit "'+OldFile.PasUnitName+'" "'+aFile.PasFilename+'" "'+OldFile.PasFilename+'"');
  end else begin
    FUnits.Add(aFile);
  end;
end;

function TPas2jsCompiler.DirectoryExists(const Filename: string): boolean;
begin
  Result:=FileCache.DirectoryCache.DirectoryExists(Filename);
end;

function TPas2jsCompiler.ExpandFileName(const Filename: string): string;
begin
  Result:=ExpandFileNameUTF8(Filename,FileCache.BaseDirectory);
end;

end.

