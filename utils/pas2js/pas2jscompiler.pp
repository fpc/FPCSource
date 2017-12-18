{ Author: Mattias Gaertner  2017  mattias@freepascal.org

Abstract:
  TPas2jsCompiler is the wheel boss of the pas2js compiler.
  It can be used in a command line program or compiled into an application.

Compiler-ToDos:
  Warn if -Ju and -Fu intersect
  -Fa<x>[,y] (for a program) load units <x> and [y] before uses is parsed
  Add Windows macros, see InitMacros.
  add options for names of globals like 'pas' and 'rtl'

FileCache:
  uses 'in'
}
unit Pas2jsCompiler;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, AVL_Tree, contnrs,
  PScanner, PParser, PasTree, PasResolver, PasUseAnalyzer, PasResolveEval,
  jstree, jswriter, FPPas2Js, FPPJsSrcMap,
  Pas2jsFileUtils, Pas2jsLogger, Pas2jsFileCache, Pas2jsPParser;

const
  VersionMajor = 0;
  VersionMinor = 8;
  VersionRelease = 39;
  VersionExtra = '+beta';
  DefaultConfigFile = 'pas2js.cfg';

//------------------------------------------------------------------------------
// Messages
const
  nOptionIsEnabled = 101; sOptionIsEnabled = 'Option "%s" is %s';
  nSyntaxModeIs = 102; sSyntaxModeIs = 'Syntax mode is %s';
  nMacroDefined = 103; sMacroDefined = 'Macro defined: %s';
  nUsingPath = 104; sUsingPath = 'Using %s: "%s"';
  nFolderNotFound = 105; sFolderNotFound = '%s not found: "%s"';
  nNameValue = 106; sNameValue = '%s: "%s"';
  nReadingOptionsFromFile = 107; sReadingOptionsFromFile = 'Reading options from file "%s"';
  nEndOfReadingConfigFile = 108; sEndOfReadingConfigFile = 'End of reading config file "%s"';
  nInterpretingFileOption = 109; sInterpretingFileOption = 'interpreting file option "%s"';
  nSourceFileNotFound = 110; sSourceFileNotFound = 'source file not found "%s"';
  nFileIsFolder = 111; sFileIsFolder = 'expected file, but found directory "%s"';
  nConfigFileSearch = 112; sConfigFileSearch = 'Configfile search: %s';
  nHandlingOption = 113; sHandlingOption = 'handling option "%s"';
  nQuickHandlingOption = 114; sQuickHandlingOption = 'quick handling option "%s"';
  nOutputDirectoryNotFound = 115; sOutputDirectoryNotFound = 'output directory not found: "%s"';
  nUnableToWriteFile = 116; sUnableToWriteFile = 'Unable to write file "%s"';
  nWritingFile = 117; sWritingFile = 'Writing file "%s" ...';
  nCompilationAborted = 118; sCompilationAborted = 'Compilation aborted';
  nCfgDirective = 119; sCfgDirective = 'cfg directive "%s": %s';
  nUnitCycle = 120; sUnitCycle = 'Unit cycle found %s';
  nOptionForbidsCompile = 121; sOptionForbidsCompile = 'Option -Ju forbids to compile unit "%s"';
  nUnitNeedsCompileDueToUsedUnit = 122; sUnitsNeedCompileDueToUsedUnit = 'Unit "%s" needs compile due to used unit "%s"';
  nUnitNeedsCompileDueToOption = 123; sUnitsNeedCompileDueToOption = 'Unit "%s" needs compile due to option "%s"';
  nUnitNeedsCompileJSMissing = 124; sUnitsNeedCompileJSMissing = 'Unit "%s" needs compile, js file missing "%s"';
  nUnitNeedsCompilePasHasChanged = 125; sUnitsNeedCompilePasHasChanged = 'Unit "%s" needs compile, Pascal file has changed, js is "%s"';
  nParsingFile = 126; sParsingFile = 'Parsing "%s" ...';
  nCompilingFile = 127; sCompilingFile = 'Compiling "%s" ...';
  nExpectedButFound = 128; sExpectedButFound = 'Illegal unit name: Expected "%s", but found "%s"';
  nLinesInFilesCompiled = 129; sLinesInFilesCompiled = '%s lines in %s files compiled, %s sec';
  nTargetPlatformIs = 130; sTargetPlatformIs = 'Target platform is %s';
  nTargetProcessorIs = 131; sTargetProcessorIs = 'Target processor is %s';
  nMessageEncodingIs = 132; sMessageEncodingIs = 'Message encoding is %s';
  nUnableToTranslatePathToDir = 133; sUnableToTranslatePathToDir = 'Unable to translate path "%s" to directory "%s"';
  nSrcMapSourceRootIs = 134; sSrcMapSourceRootIs = 'source map "sourceRoot" is %s';
  nSrcMapBaseDirIs = 135; sSrcMapBaseDirIs = 'source map "local base directory" is %s';

//------------------------------------------------------------------------------
// Options
type
  TP2jsCompilerOption = (
    coSkipDefaultConfigs,
    coBuildAll,
    coShowLogo,
    coShowErrors,
    coShowWarnings,
    coShowNotes,
    coShowHints,
    coShowInfos,
    coShowLineNumbers,
    coShowConditionals,
    coShowUsedTools,
    coShowMessageNumbers, // not in "show all"
    coShowDebug,    // not in "show all"
    coAllowCAssignments,
    coLowerCase,
    coEnumValuesAsNumbers,
    coKeepNotUsedPrivates,
    coKeepNotUsedDeclarationsWPO,
    coSourceMapCreate,
    coSourceMapInclude
    );
  TP2jsCompilerOptions = set of TP2jsCompilerOption;
const
  DefaultP2jsCompilerOptions = [coShowErrors];
  coShowAll = [coShowErrors..coShowUsedTools];
  coO1Enable = [coEnumValuesAsNumbers];
  coO1Disable = [coKeepNotUsedPrivates,coKeepNotUsedDeclarationsWPO];

  p2jscoCaption: array[TP2jsCompilerOption] of string = (
    // only used by experts, no need for resourcestrings
    'Skip default configs',
    'Build all',
    'Show logo',
    'Show errors',
    'Show warnings',
    'Show notes',
    'Show hints',
    'Show infos',
    'Show line numbers',
    'Show conditionals',
    'Show used tools',
    'Show message numbers',
    'Show debug',
    'Allow C assignments',
    'Lowercase identifiers',
    'Enum values as numbers',
    'Keep not used private declarations',
    'Keep not used declarations (WPO)',
    'Create source map',
    'Include Pascal sources in source map'
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
    FScanner: TPascalScanner;
    FShowDebug: boolean;
    FUseAnalyzer: TPasAnalyzer;
    FUsedBy: array[TUsedBySection] of TFPList; // list of TPas2jsCompilerFile
    procedure FPasResolverContinueParsing(Sender: TObject);
    function GetUsedBy(Section: TUsedBySection; Index: integer): TPas2jsCompilerFile;
    function GetUsedByCount(Section: TUsedBySection): integer;
    function OnConverterIsElementUsed(Sender: TObject; El: TPasElement): boolean;
    function OnConverterIsTypeInfoUsed(Sender: TObject; El: TPasElement): boolean;
    procedure OnPasResolverLog(Sender: TObject; const Msg: String);
    procedure OnParserLog(Sender: TObject; const Msg: String);
    procedure OnScannerLog(Sender: TObject; const Msg: String);
    procedure OnUseAnalyzerMessage(Sender: TObject; Msg: TPAMessage);
    procedure SetJSFilename(AValue: string);
    procedure HandleEParserError(E: EParserError);
    procedure HandleEPasResolve(E: EPasResolve);
    procedure HandleEPas2JS(E: EPas2JS);
    procedure HandleUnknownException(E: Exception);
    procedure HandleException(E: Exception);
    procedure DoLogMsgAtEl(MsgType: TMessageType; const Msg: string;
      MsgNumber: integer; El: TPasElement);
    procedure RaiseInternalError(id: int64; Msg: string);
    procedure ParserFinished;
  public
    constructor Create(aCompiler: TPas2jsCompiler; const aPasFilename: string);
    destructor Destroy; override;
    procedure CreateScannerAndParser(aFileResolver: TPas2jsFileResolver);
    function OnPasTreeFindModule(const UseUnitname: String): TPasModule;
    function FindUnit(const UseUnitname: String): TPasModule;
    procedure OnPasTreeCheckSrcName(const Element: TPasElement);
    procedure OpenFile(aFilename: string);// beware: this changes FileResolver.BaseDirectory
    procedure ParsePascal;
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
    property JSFilename: string read FJSFilename write SetJSFilename;
    property JSModule: TJSElement read FJSModule;
    property Log: TPas2jsLogger read FLog;
    property NeedBuild: Boolean read FNeedBuild write FNeedBuild;
    property Parser: TPas2jsPasParser read FParser;
    property PascalResolver: TPas2jsCompilerResolver read FPasResolver;
    property PasFilename: String read FPasFilename;
    property PasModule: TPasModule read FPasModule;
    property PasUnitName: string read FPasUnitName write FPasUnitName;// unit name in program
    property Scanner: TPascalScanner read FScanner;
    property ShowDebug: boolean read FShowDebug write FShowDebug;
    property UseAnalyzer: TPasAnalyzer read FUseAnalyzer; // unit analysis
    property UsedByCount[Section: TUsedBySection]: integer read GetUsedByCount;
    property UsedBy[Section: TUsedBySection; Index: integer]: TPas2jsCompilerFile read GetUsedBy;
  end;

  { TPas2JSWPOptimizer }

  TPas2JSWPOptimizer = class(TPasAnalyzer)
  public
  end;

  { TPas2jsCompiler }

  TPas2jsCompiler = class
  private
    FCompilerExe: string;
    FConditionEval: TCondDirectiveEvaluator;
    FCurrentCfgFilename: string;
    FCurrentCfgLineNumber: integer;
    FDefines: TStrings; // Objects can be TMacroDef
    FFileCache: TPas2jsFilesCache;
    FFileCacheAutoFree: boolean;
    FFiles: TAVLTree; // tree of TPas2jsCompilerFile sorted for PasFilename
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
    function ConditionEvalVariable(Sender: TCondDirectiveEvaluator;
      aName: String; out Value: string): boolean;
    function GetDefaultNamespace: String;
    function GetFileCount: integer;
    function GetShowDebug: boolean; inline;
    function GetShowFullPaths: boolean;
    function GetShowLogo: Boolean; inline;
    function GetShowTriedUsedFiles: boolean; inline;
    function GetShowUsedTools: boolean; inline;
    function GetSkipDefaultConfig: Boolean; inline;
    function GetSrcMapBaseDir: string;
    function GetSrcMapEnable: boolean;
    function GetSrcMapInclude: boolean;
    function OnMacroCfgDir(Sender: TObject; var Params: string; Lvl: integer
      ): boolean;
    function OnMacroEnv(Sender: TObject; var Params: string; Lvl: integer
      ): boolean;
    procedure AddDefinesForTargetPlatform;
    procedure AddDefinesForTargetProcessor;
    procedure CfgSyntaxError(const Msg: string);
    procedure ConditionEvalLog(Sender: TCondDirectiveEvaluator;
      Args: array of const);
    procedure LoadConfig(CfgFilename: string);
    procedure LoadDefaultConfig;
    procedure ParamFatal(Msg: string);
    procedure ReadParam(Param: string; Quick, FromCmdLine: boolean);
    procedure ReadSingleLetterOptions(const Param: string; p: PChar;
      const Allowed: string; out Enabled, Disabled: string);
    procedure ReadSyntaxFlags(Param: String; p: PChar);
    procedure ReadVerbosityFlags(Param: String; p: PChar);
    procedure RegisterMessages;
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
    procedure SetTargetPlatform(const AValue: TPasToJsPlatform);
    procedure SetTargetProcessor(const AValue: TPasToJsProcessor);
  protected
    // If this function returns true, the compiler assumes the file was written.
    // If false, the compiler will attempt to write the file itself.
    function DoWriteJSFile(const DestFilename: String; aWriter: TPas2JSMapper): Boolean; virtual;
    procedure Compile(StartTime: TDateTime);
    function MarkNeedBuilding(aFile: TPas2jsCompilerFile; Checked: TAVLTree;
      var SrcFileCount: integer): boolean;
    procedure OptimizeProgram(aFile: TPas2jsCompilerFile); virtual;
    procedure CreateJavaScript(aFile: TPas2jsCompilerFile; Checked: TAVLTree);
    procedure FinishSrcMap(SrcMap: TPas2JSSrcMap); virtual;
    procedure WriteJSFiles(aFile: TPas2jsCompilerFile;
      var CombinedFileWriter: TPas2JSMapper; Checked: TAVLTree);
    procedure InitParamMacros;
    procedure ClearDefines;
    procedure RaiseInternalError(id: int64; Msg: string);
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

    function FindPasFile(PasFilename: string): TPas2jsCompilerFile;
    procedure LoadPasFile(PasFilename, UseUnitName: string; out aFile: TPas2jsCompilerFile);
    function FindUsedUnit(const TheUnitName: string): TPas2jsCompilerFile;
    procedure AddUsedUnit(aFile: TPas2jsCompilerFile);
  public
    property CompilerExe: string read FCompilerExe write SetCompilerExe;
    property ConditionEvaluator: TCondDirectiveEvaluator read FConditionEval;
    property CurrentCfgFilename: string read FCurrentCfgFilename;
    property CurrentCfgLineNumber: integer read FCurrentCfgLineNumber;
    property DefaultNamespace: String read GetDefaultNamespace;
    property Defines: TStrings read FDefines;
    property FileCache: TPas2jsFilesCache read FFileCache write SetFileCache;
    property FileCacheAutoFree: boolean read FFileCacheAutoFree write FFileCacheAutoFree;
    property FileCount: integer read GetFileCount;
    property Log: TPas2jsLogger read FLog;
    property MainFile: TPas2jsCompilerFile read FMainFile;
    property Mode: TP2jsMode read FMode write SetMode;
    property Options: TP2jsCompilerOptions read FOptions write SetOptions;
    property ParamMacros: TPas2jsMacroEngine read FParamMacros;
    property SrcMapEnable: boolean read GetSrcMapEnable write SetSrcMapEnable;
    property SrcMapSourceRoot: string read FSrcMapSourceRoot write FSrcMapSourceRoot;
    property SrcMapBaseDir: string read GetSrcMapBaseDir write SetSrcMapBaseDir;
    property SrcMapInclude: boolean read GetSrcMapInclude write SetSrcMapInclude;
    property ShowDebug: boolean read GetShowDebug write SetShowDebug;
    property ShowFullPaths: boolean read GetShowFullPaths write SetShowFullPaths;
    property ShowLogo: Boolean read GetShowLogo write SetShowLogo;
    property ShowTriedUsedFiles: boolean read GetShowTriedUsedFiles write SetShowTriedUsedFiles;
    property ShowUsedTools: boolean read GetShowUsedTools write SetShowUsedTools;
    property SkipDefaultConfig: Boolean read GetSkipDefaultConfig write SetSkipDefaultConfig;
    property TargetPlatform: TPasToJsPlatform read FTargetPlatform write SetTargetPlatform;
    property TargetProcessor: TPasToJsProcessor read FTargetProcessor write SetTargetProcessor;
    property WPOAnalyzer: TPas2JSWPOptimizer read FWPOAnalyzer; // Whole Program Optimization
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
    if (s[p]='$') and (s[p+1] in ['_','a'..'z','A'..'Z']) then begin
      StartP:=p;
      inc(p,2);
      while (p<=length(s)) and (s[p] in ['_','a'..'z','A'..'Z','0'..'9']) do
        inc(p);
      MacroName:=copy(s,StartP+1,p-StartP-1);
      Macro:=FindMacro(MacroName);
      if Macro=nil then
        raise EPas2jsMacro.Create('macro not found "'+MacroName+'" in "'+s+'"');
      NewValue:='';
      if Macro.CanHaveParams and (p<=length(s)) and (s[p]='(') then begin
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
            if BracketLvl=1 then begin
              NewValue:=copy(s,ParamStartP,p-ParamStartP);
              break;
            end else begin
              dec(BracketLvl);
            end;
          end;
        until false;
      end else if (p<=length(s)) and (s[p]='$') then
        inc(p);
      if Assigned(Macro.OnSubstitute) then begin
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
  const aPasFilename: string);
var
  ub: TUsedBySection;
begin
  FCompiler:=aCompiler;
  FLog:=Compiler.Log;
  FPasFilename:=aPasFilename;
  FPasResolver:=TPas2jsCompilerResolver.Create;
  FPasResolver.Owner:=Self;
  FPasResolver.OnContinueParsing:=@FPasResolverContinueParsing;
  FPasResolver.OnFindModule:=@OnPasTreeFindModule;
  FPasResolver.OnCheckSrcName:=@OnPasTreeCheckSrcName;
  FPasResolver.OnLog:=@OnPasResolverLog;
  FPasResolver.Log:=Log;
  FPasResolver.AddObjFPCBuiltInIdentifiers(btAllJSBaseTypes,bfAllJSBaseProcs);
  FIsMainFile:=CompareFilenames(aCompiler.FileCache.MainSrcFile,aPasFilename)=0;
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
  FreeAndNil(FUseAnalyzer);
  for ub in TUsedBySection do
    FreeAndNil(FUsedBy[ub]);
  FreeAndNil(FJSModule);
  FreeAndNil(FConverter);
  if FPasModule<>nil then begin
    FPasModule.Release;
    FPasModule:=nil;
  end;
  FreeAndNil(FParser);
  FreeAndNil(FScanner);
  FreeAndNil(FFileResolver);
  FreeAndNil(FPasResolver);
  inherited Destroy;
end;

procedure TPas2jsCompilerFile.CreateScannerAndParser(aFileResolver: TPas2jsFileResolver);
var
  aUnitName: String;
  i: Integer;
  M: TMacroDef;
begin
  FFileResolver:=aFileResolver;
  // scanner
  FScanner := TPascalScanner.Create(FileResolver);
  Scanner.LogEvents:=PascalResolver.ScannerLogEvents;
  Scanner.OnLog:=@OnScannerLog;
  Scanner.OnFormatPath:=@Compiler.FileCache.FormatPath;

  // create parser (Note: this sets some scanner options to defaults)
  FParser := TPas2jsPasParser.Create(Scanner, FileResolver, PascalResolver);

  // set options
  Scanner.AllowedModeSwitches:=msAllPas2jsModeSwitches;
  Scanner.ReadOnlyModeSwitches:=msAllPas2jsModeSwitchesReadOnly;
  Scanner.CurrentModeSwitches:=p2jsMode_SwitchSets[Compiler.Mode];
  // Note: some Scanner.Options are set by TPasResolver
  for i:=0 to Compiler.Defines.Count-1 do
    begin
    M:=TMacroDef(Compiler.Defines.Objects[i]);
    if M=nil then
      Scanner.AddDefine(Compiler.Defines[i])
    else
      Scanner.AddMacro(M.Name,M.Value);
    end;
  if coAllowCAssignments in Compiler.Options then
    Scanner.Options:=Scanner.Options+[po_cassignments];
  if Compiler.Mode=p2jmDelphi then
    Scanner.Options:=Scanner.Options+[po_delphi];

  // parser
  Parser.LogEvents:=PascalResolver.ParserLogEvents;
  Parser.OnLog:=@OnParserLog;
  Parser.Log:=Log;
  PascalResolver.P2JParser:=Parser;

  if not IsMainFile then begin
    aUnitName:=ExtractFilenameOnly(PasFilename);
    if CompareText(aUnitName,'system')=0 then
      Parser.ImplicitUses.Clear;
  end;
end;

procedure TPas2jsCompilerFile.OnPasTreeCheckSrcName(const Element: TPasElement);
var
  SrcName, ExpectedSrcName: String;
begin
  //writeln('TPas2jsCompilerFile.OnPasTreeCheckSrcName ',PasFilename,' Name=',Element.Name,' IsMainFile=',IsMainFile);
  if (Element.ClassType=TPasUnitModule) or (Element.ClassType=TPasModule) then
  begin
    SrcName:=Element.Name;
    if IsMainFile then begin
      // main source is an unit
      if PasUnitName='' then begin
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

procedure TPas2jsCompilerFile.FPasResolverContinueParsing(Sender: TObject);
begin
  try
    Parser.ParseContinueImplementation;
  except
    on E: Exception do
      HandleException(E);
  end;
  ParserFinished;
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

procedure TPas2jsCompilerFile.SetJSFilename(AValue: string);
begin
  if FJSFilename=AValue then Exit;
  FJSFilename:=AValue;
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
  if E.PasElement<>nil then begin
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
  if E.PasElement<>nil then begin
    aFilename:=E.PasElement.SourceFilename;
    PascalResolver.UnmangleSourceLineNumber(E.PasElement.SourceLinenumber,aRow,aColumn);
    Log.Log(E.MsgType,E.Message,E.MsgNumber,aFilename,aRow,aColumn);
  end else begin
    Log.Log(E.MsgType,E.Message,E.MsgNumber);
  end;
  Compiler.Terminate(ExitCodeConverterError);
end;

procedure TPas2jsCompilerFile.HandleUnknownException(E: Exception);
begin
  if not (E is ECompilerTerminate) then
    Log.Log(mtFatal,E.ClassName+': '+E.Message,0);
  Compiler.Terminate(ExitCodeErrorInternal);
end;

procedure TPas2jsCompilerFile.HandleException(E: Exception);
begin
  if E is EScannerError then begin
    Log.Log(Scanner.LastMsgType,Scanner.LastMsg,Scanner.LastMsgNumber,
            Scanner.CurFilename,Scanner.CurRow,Scanner.CurColumn);
    Compiler.Terminate(ExitCodeSyntaxError);
  end else if E is EParserError then
    HandleEParserError(EParserError(E))
  else if E is EPasResolve then
    HandleEPasResolve(EPasResolve(E))
  else if E is EPas2JS then
    HandleEPas2JS(EPas2JS(E))
  else
    HandleUnknownException(E);
end;

procedure TPas2jsCompilerFile.DoLogMsgAtEl(MsgType: TMessageType;
  const Msg: string; MsgNumber: integer; El: TPasElement);
var
  Line, Col: integer;
  Filename: String;
begin
  if (El<>nil) then begin
    Filename:=El.SourceFilename;
    TPasResolver.UnmangleSourceLineNumber(El.SourceLinenumber,Line,Col);
  end else begin
    Filename:='';
    Line:=0;
    Col:=0;
  end;
  Log.Log(MsgType,Msg,MsgNumber,Filename,Line,Col);
end;

procedure TPas2jsCompilerFile.RaiseInternalError(id: int64; Msg: string);
begin
  Compiler.RaiseInternalError(id,Msg);
end;

procedure TPas2jsCompilerFile.ParserFinished;
begin
  try
    if ShowDebug then begin
      Log.LogRaw('Pas-Module:');
      Log.LogRaw(PasModule.GetDeclaration(true));
    end;
    if PasModule.CustomData=nil then
      PasModule.CustomData:=Self;

    // analyze
    UseAnalyzer.AnalyzeModule(FPasModule);
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TPas2jsCompilerFile.OpenFile(aFilename: string);
begin
  FPasFilename:=aFilename;
  try
    Scanner.OpenFile(PasFilename);
  except
    on E: EScannerError do begin
      Log.Log(Scanner.LastMsgType,Scanner.LastMsg,Scanner.LastMsgNumber,
              Scanner.CurFilename,Scanner.CurRow,Scanner.CurColumn);
      Compiler.Terminate(ExitCodeSyntaxError);
    end;
  end;
end;

procedure TPas2jsCompilerFile.ParsePascal;
begin
  if ShowDebug then
    Log.LogRaw(['Debug: Parsing Pascal "',PasFilename,'"...']);
  try
    // parse Pascal
    PascalResolver.InterfaceOnly:=IsForeign;
    if IsMainFile then
      Parser.ParseMain(FPasModule)
    else
      Parser.ParseSubModule(FPasModule);
  except
    on E: Exception do
      HandleException(E);
  end;
  if (FPasModule.ImplementationSection<>nil)
      and (FPasModule.ImplementationSection.PendingUsedIntf<>nil) then
    exit;
  ParserFinished;
end;

procedure TPas2jsCompilerFile.CreateJS;
begin
  try
    // show hints only for units that are actually converted
    UseAnalyzer.EmitModuleHints(PasModule);

    // convert
    FConverter:=TPasToJSConverter.Create;
    FConverter.Options:=FConverter.Options+[coUseStrict];
    if coEnumValuesAsNumbers in Compiler.Options then
      FConverter.Options:=FConverter.Options+[fppas2js.coEnumNumbers];
    FConverter.UseLowerCase:=coLowerCase in Compiler.Options;
    FConverter.TargetPlatform:=Compiler.TargetPlatform;
    FConverter.TargetProcessor:=Compiler.TargetProcessor;
    FConverter.OnIsElementUsed:=@OnConverterIsElementUsed;
    FConverter.OnIsTypeInfoUsed:=@OnConverterIsTypeInfoUsed;
    FJSModule:=Converter.ConvertPasElement(PasModule,PascalResolver);
  except
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
  if aModule.ClassType=TPasUnitModule then
    Result:=TPasUnitModule(aModule).InterfaceSection
  else if aModule.ClassType=TPasProgram then
    Result:=TPasProgram(aModule).ProgramSection
  else if aModule.ClassType=TPasLibrary then
    Result:=TPasLibrary(aModule).LibrarySection
  else
    Result:=nil;
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
  if aModule.ClassType=TPasModule then begin
    IntfSection:=TPasModule(aModule).InterfaceSection;
    if IntfSection<>nil then
      Result:=IntfSection.UsesClause;
  end else if aModule.ClassType=TPasProgram then begin
    PrgSection:=TPasProgram(aModule).ProgramSection;
    if PrgSection<>nil then
      Result:=PrgSection.UsesClause;
  end else if aModule.ClassType=TPasLibrary then begin
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
  else if aModule.CustomData is TPasModuleScope then begin
    Scope:=TPasModuleScope(aModule.CustomData);
    Resolver:=NoNil(Scope.Owner) as TPas2jsCompilerResolver;
    Result:=Resolver.Owner as TPas2jsCompilerFile;
  end;
end;

function TPas2jsCompilerFile.OnPasTreeFindModule(const UseUnitname: String): TPasModule;
var
  aNameSpace: String;
  LastEl: TPasElement;
  i: Integer;
begin
  Result:=nil;
  if CompareText(ExtractFilenameOnly(PasFilename),UseUnitname)=0 then begin
    // duplicate identifier or unit cycle
    Parser.RaiseParserError(nUnitCycle,[UseUnitname]);
  end;

  LastEl:=PascalResolver.LastElement;
  if (LastEl<>nil)
      and ((LastEl is TPasSection) or (LastEl.ClassType=TPasUsesUnit)
        or (LastEl.Parent is TPasSection)) then
    // ok
  else
    RaiseInternalError(20170504161408,'internal error TPas2jsCompilerFile.FindModule PasTree.LastElement='+GetObjName(LastEl)+' '+GetObjName(LastEl.Parent));

  if (Pos('.',UseUnitname)<1) then begin
    // generic unit -> search with namespaces
    // first the default program namespace
    aNameSpace:=Compiler.GetDefaultNamespace;
    if aNameSpace<>'' then begin
      Result:=FindUnit(aNameSpace+'.'+UseUnitname);
      if Result<>nil then exit;
    end;

    // then the cmdline namespaces
    for i:=0 to Compiler.FileCache.Namespaces.Count-1 do begin
      aNameSpace:=Compiler.FileCache.Namespaces[i];
      if aNameSpace='' then continue;
      Result:=FindUnit(aNameSpace+'.'+UseUnitname);
      if Result<>nil then exit;
    end
  end;

  // search in unitpath
  Result:=FindUnit(UseUnitname);
  // if nil resolver will give a nice error position
end;

function TPas2jsCompilerFile.FindUnit(const UseUnitname: String): TPasModule;

  function FindCycle(aFile, SearchFor: TPas2jsCompilerFile;
    var Cycle: TFPList): boolean;
  var
    i: Integer;
    aParent: TPas2jsCompilerFile;
  begin
    for i:=0 to aFile.UsedByCount[ubMainSection]-1 do begin
      aParent:=aFile.UsedBy[ubMainSection,i];
      if aParent=SearchFor then begin
        // unit cycle found
        Cycle:=TFPList.Create;
        Cycle.Add(aParent);
        Cycle.Add(aFile);
        exit(true);
      end;
      if FindCycle(aParent,SearchFor,Cycle) then begin
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
    if Parser.CurModule.ImplementationSection=nil then begin
      // main uses section (e.g. interface or program, not implementation)
      // -> check for cycles

      aFile.FUsedBy[ubMainSection].Add(Self);

      Cycle:=nil;
      try
        if FindCycle(aFile,aFile,Cycle) then begin
          CyclePath:='';
          for i:=0 to Cycle.Count-1 do begin
            if i>0 then CyclePath+=',';
            CyclePath+=TPas2jsCompilerFile(Cycle[i]).GetModuleName;
          end;
          Parser.RaiseParserError(nUnitCycle,[CyclePath]);
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
  UsePasFilename, InFilename, UseJSFilename: String;
  UseIsForeign: boolean;
begin
  Result:=nil;
  InFilename:='';

  // first try registered units
  aFile:=Compiler.FindUsedUnit(UseUnitname);
  if aFile<>nil then begin
    // known unit
    if (aFile.PasUnitName<>'') and (CompareText(aFile.PasUnitName,UseUnitname)<>0) then
    begin
      Log.LogRaw(['Debug: TPas2jsPasTree.FindUnit unitname MISMATCH aFile.PasUnitname="',aFile.PasUnitName,'"',
         ' Self=',FileResolver.Cache.FormatPath(PasFilename),
         ' Uses=',UseUnitname,
         ' IsForeign=',IsForeign]);
      RaiseInternalError(20170504161412,'TPas2jsPasTree.FindUnit unit name mismatch');
    end;
    CheckCycle;
  end else begin
    // new unit -> search

    // search Pascal file
    UsePasFilename:=FileResolver.FindUnitFileName(UseUnitname,InFilename,UseIsForeign);
    if UsePasFilename='' then begin
      // can't find unit
      exit;
    end;

    UseJSFilename:='';
    if (not IsForeign) then
      UseJSFilename:=FileResolver.FindUnitJSFileName(UsePasFilename);
    //  Log.LogRaw(['Debug: TPas2jsPasTree.FindUnit Self=',FileResolver.Cache.FormatPath(PasFilename),
    //    ' Uses=',UseUnitname,' Found="',FileResolver.Cache.FormatPath(UsePasFilename),'"',
    //    ' IsForeign=',IsForeign,' JSFile="',FileResolver.Cache.FormatPath(useJSFilename),'"']);

    // load Pascal file
    Compiler.LoadPasFile(UsePasFilename,UseUnitname,aFile);
    if aFile=Self then begin
      // unit uses itself -> cycle
      Parser.RaiseParserError(nUnitCycle,[UseUnitname]);
    end;
    if aFile.PasUnitName<>UseUnitname then
      RaiseInternalError(20170922143329,'aFile.PasUnitName='+aFile.PasUnitName+' UseUnitname='+UseUnitname);

    Compiler.AddUsedUnit(aFile);
    if aFile<>Compiler.FindUsedUnit(UseUnitname) then
      begin
      if Compiler.FindUsedUnit(UseUnitname)=nil then
        RaiseInternalError(20170922143405,'UseUnitname='+UseUnitname)
      else
        RaiseInternalError(20170922143511,'UseUnitname='+UseUnitname+' Found='+Compiler.FindUsedUnit(UseUnitname).PasUnitName);
      end;
    CheckCycle;

    aFile.JSFilename:=UseJSFilename;
    aFile.IsForeign:=UseIsForeign;

    // parse Pascal
    aFile.ParsePascal;
    // beware: the parser may not yet have finished due to unit cycles
  end;

  Result:=aFile.PasModule;
end;

{ TPas2jsCompiler }

procedure TPas2jsCompiler.SetFileCache(AValue: TPas2jsFilesCache);
begin
  if FFileCache=AValue then Exit;
  FFileCacheAutoFree:=false;
  FFileCache:=AValue;
end;

procedure TPas2jsCompiler.CfgSyntaxError(const Msg: string);
begin
  Log.Log(mtError,Msg,0,CurrentCfgFilename,CurrentCfgLineNumber,0);
  Terminate(ExitCodeErrorInConfig);
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

procedure TPas2jsCompiler.AddDefinesForTargetProcessor;
begin
  AddDefine(PasToJsProcessorNames[TargetProcessor]);
  case TargetProcessor of
    ProcessorECMAScript5: AddDefine('ECMAScript', '5');
    ProcessorECMAScript6: AddDefine('ECMAScript', '6');
  end;
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
  if i>=0 then begin
    M:=TMacroDef(FDefines.Objects[i]);
    if M=nil then
      Value:=CondDirectiveBool[true]
    else
      Value:=M.Value;
    exit(true);
  end;

  // check modeswitches
  ms:=StrToModeSwitch(aName);
  if (ms<>msNone) and (ms in p2jsMode_SwitchSets[Mode]) then begin
    Value:=CondDirectiveBool[true];
    exit(true);
  end;
end;

procedure TPas2jsCompiler.AddDefinesForTargetPlatform;
begin
  AddDefine(PasToJsPlatformNames[TargetPlatform]);
end;

procedure TPas2jsCompiler.Compile(StartTime: TDateTime);
var
  Checked: TAVLTree;
  CombinedFileWriter: TPas2JSMapper;
  SrcFileCount: integer;
  Seconds: TDateTime;
begin
  if FMainFile<>nil then
    RaiseInternalError(20170504192137,'');
  Checked:=nil;
  CombinedFileWriter:=nil;
  SrcFileCount:=0;
  try
    // load main Pascal file
    LoadPasFile(FileCache.MainSrcFile,'',FMainFile);
    if MainFile=nil then exit;
    // parse and load Pascal files recursively
    FMainFile.ParsePascal;

    // whole program optimization
    if MainFile.PasModule is TPasProgram then
      OptimizeProgram(MainFile);

    // check what files need building
    Checked:=TAVLTree.Create;
    MarkNeedBuilding(MainFile,Checked,SrcFileCount);
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
    if ExitCode=0 then begin
      Seconds:=(Now-StartTime)*86400;
      Log.LogMsgIgnoreFilter(nLinesInFilesCompiled,
             [IntToStr(FileCache.ReadLineCounter),IntToStr(SrcFileCount),
              FormatFloat('0.0',Seconds)]);
    end;
  finally
    Checked.Free;
    if ExitCode<>0 then
      Log.LogMsgIgnoreFilter(nCompilationAborted,[]);
    CombinedFileWriter.Free;
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
      if MarkNeedBuilding(UsedFile,Checked,SrcFileCount) then begin
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
    exit(false);

  // check dependencies
  CheckUsesClause(aFile.GetPasMainUsesClause);
  CheckUsesClause(aFile.GetPasImplUsesClause);

  if (not aFile.NeedBuild) and (not aFile.IsForeign) then begin
    // this unit can be compiled
    if aFile.IsMainFile then
      Mark(nUnitNeedsCompileDueToOption,[aFile.GetModuleName,'<main source file>'])
    else if coBuildAll in Options then
      Mark(nUnitNeedsCompileDueToOption,[aFile.GetModuleName,'-B'])
    else if FileCache.AllJSIntoMainJS then
      Mark(nUnitNeedsCompileDueToOption,[aFile.GetModuleName,'-Jc'])
    else if (aFile.JSFilename<>'') and (not FileExists(aFile.JSFilename)) then
      Mark(nUnitNeedsCompileJSMissing,[aFile.GetModuleName,FileCache.FormatPath(aFile.JSFilename)])
    else if (aFile.JSFilename<>'')
    and (FileAge(aFile.PasFilename)>FileAge(aFile.JSFilename)) then begin
      // ToDo: replace FileAge with checksum
      Mark(nUnitNeedsCompilePasHasChanged,[aFile.GetModuleName,FileCache.FormatPath(aFile.JSFilename)])
    end;
  end;

  if aFile.NeedBuild then begin
    // unit needs compile
    if aFile.IsForeign then begin
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

  Log.LogMsg(nCompilingFile,[FileCache.FormatPath(aFile.PasFilename)],'',0,0,
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
    if SrcMapInclude then begin
      // include source in SrcMap
      aFile:=FileCache.LoadTextFile(LocalFilename);
      SrcMap.SourceContents[i]:=aFile.Source;
    end;
    // translate local file name
    if BaseDir<>'' then begin
      if not TryCreateRelativePath(LocalFilename,BaseDir,true,MapFilename)
      then begin
        // e.g. file is on another partition
        if not SrcMapInclude then begin
          Log.Log(mtError,
            SafeFormat(sUnableToTranslatePathToDir,[LocalFilename,BaseDir]),
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
    if SrcMapEnable then begin
      SrcMap:=TPas2JSSrcMap.Create(ExtractFilename(aFilename));
      aFileWriter.SrcMap:=SrcMap;
      SrcMap.Release;// release the refcount from the Create
      SrcMap.SourceRoot:=SrcMapSourceRoot;
      SrcMap.LocalFilename:=aFile.JSFilename;
    end;
  end;

var
  DestFilename, DestDir, Src, MapFilename: String;
  aJSWriter: TJSWriter;
  fs: TFileStream;
  ms: TMemoryStream;
begin
  //writeln('TPas2jsCompiler.WriteJSFiles ',aFile.PasFilename,' Need=',aFile.NeedBuild,' Checked=',Checked.Find(aFile)<>nil);
  if (aFile.JSModule=nil) or (not aFile.NeedBuild) then exit;
  // check each file only once
  if Checked.Find(aFile)<>nil then exit;
  Checked.Add(aFile);

  FreeWriter:=false;
  if FileCache.AllJSIntoMainJS and (CombinedFileWriter=nil) then begin
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
    if aFileWriter=nil then begin
      // create writer for this file
      CreateFileWriter(DestFilename);
      if aFile.IsMainFile and not FileCache.AllJSIntoMainJS then
        FileCache.InsertCustomJSFiles(aFileWriter);
    end;

    // write JavaScript
    aJSWriter:=TJSWriter.Create(aFileWriter);
    aJSWriter.Options:=[woUseUTF8,woCompactArrayLiterals,woCompactObjectLiterals,woCompactArguments];
    aJSWriter.IndentSize:=2;
    aJSWriter.WriteJS(aFile.JSModule);

    if aFile.IsMainFile and (TargetPlatform=PlatformNodeJS) then
      aFileWriter.WriteFile('rtl.run();'+LineEnding,aFile.PasFilename);

    // Give chance to descendants to write file
    if DoWriteJSFile(aFile.JSFilename,aFileWriter) then
      exit;// descendant has written -> finished

    if (aFile.JSFilename='') and (FileCache.MainJSFile='.') then begin
      // write to stdout
      Log.LogRaw(aFileWriter.AsAnsistring);
    end else if FreeWriter then begin
      // write to file

      //writeln('TPas2jsCompiler.WriteJSFiles ',aFile.PasFilename,' ',aFile.JSFilename);
      Log.LogMsg(nWritingFile,[FileCache.FormatPath(DestFilename)],'',0,0,
                 not (coShowLineNumbers in Options));

      // check output directory
      DestDir:=ChompPathDelim(ExtractFilePath(DestFilename));
      if (DestDir<>'') and not DirectoryExists(DestDir) then begin
        Log.LogMsg(nOutputDirectoryNotFound,[FileCache.FormatPath(DestDir)]);
        Terminate(ExitCodeFileNotFound);
      end;
      if DirectoryExists(DestFilename) then begin
        Log.LogMsg(nFileIsFolder,[FileCache.FormatPath(DestFilename)]);
        Terminate(ExitCodeWriteError);
      end;

      MapFilename:=DestFilename+'.map';

      // write js
      try
        fs:=TFileStream.Create(DestFilename,fmCreate);
        try
          // UTF8-BOM
          if (Log.Encoding='') or (Log.Encoding='utf8') then begin
            Src:=String(UTF8BOM);
            fs.Write(Src[1],length(Src));
          end;
          // JS source
          fs.Write(aFileWriter.Buffer^,aFileWriter.BufferLength);
          // source map comment
          if aFileWriter.SrcMap<>nil then begin
            Src:='//# sourceMappingURL='+ExtractFilename(MapFilename)+LineEnding;
            fs.Write(Src[1],length(Src));
          end;
        finally
          fs.Free;
        end;
      except
        on E: Exception do begin
          Log.LogRaw('Error: '+E.Message);
          Log.LogMsg(nUnableToWriteFile,[FileCache.FormatPath(DestFilename)]);
          Terminate(ExitCodeWriteError);
        end;
      end;

      // write source map
      if aFileWriter.SrcMap<>nil then begin
        Log.LogMsg(nWritingFile,[FileCache.FormatPath(MapFilename)],'',0,0,
                   not (coShowLineNumbers in Options));
        FinishSrcMap(aFileWriter.SrcMap);
        try
          ms:=TMemoryStream.Create;
          try
            // Note: No UTF-8 BOM in source map, Chrome 59 gives an error
            aFileWriter.SrcMap.SaveToStream(ms);
            ms.Position:=0;
            ms.SaveToFile(MapFilename);
          finally
            ms.Free;
          end;
        except
          on E: Exception do begin
            Log.LogRaw('Error: '+E.Message);
            Log.LogMsg(nUnableToWriteFile,[FileCache.FormatPath(MapFilename)]);
            Terminate(ExitCodeWriteError);
          end;
        end;
      end;
    end;

  finally
    if FreeWriter then begin
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
  Log.LogRaw('['+IntToStr(id)+'] '+Msg);
  raise Exception.Create(Msg);
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
  Result:=FileCache.ShowTriedUsedFiles;
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
    Log.LogMsg(nCfgDirective,[Line,s],CurrentCfgFilename,CurrentCfgLineNumber,1,false);
  end;

var
  OldCfgFilename, Directive, aName, Expr: String;
  aFile: TPas2jsFileLineReader;
  IfLvl, SkipLvl, OldCfgLineNumber: Integer;
  Skip: TSkip;
begin
  if ShowTriedUsedFiles then
    Log.LogMsgIgnoreFilter(nReadingOptionsFromFile,[CfgFilename]);
  IfLvl:=0;
  SkipLvl:=0;
  Skip:=skipNone;
  aFile:=nil;
  try
    OldCfgFilename:=FCurrentCfgFilename;
    FCurrentCfgFilename:=CfgFilename;
    OldCfgLineNumber:=FCurrentCfgLineNumber;
    aFile:=TPas2jsFileLineReader.Create(CfgFilename);
    while not aFile.IsEOF do begin
      Line:=aFile.ReadLine;
      FCurrentCfgLineNumber:=aFile.LineNumber;
      if ShowDebug then
        Log.LogMsgIgnoreFilter(nInterpretingFileOption,[Line]);
      if Line='' then continue;
      p:=PChar(Line);
      while (p^ in [' ',#9]) do inc(p);
      if p^=#0 then continue; // empty line

      if p^='#' then begin
        // cfg directive
        inc(p);
        if p^ in [#0,#9,' ','-'] then continue; // comment
        Directive:=lowercase(GetWord);
        case Directive of
        'ifdef','ifndef':
          begin
            inc(IfLvl);
            if Skip=skipNone then begin
              aName:=GetWord;
              if IsDefined(aName)=(Directive='ifdef') then begin
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
            if Skip=skipNone then begin
              Expr:=copy(Line,p-PChar(Line)+1,length(Line));
              if ConditionEvaluator.Eval(Expr) then begin
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
              CfgSyntaxError('"'+Directive+'" without ifdef');
            if (Skip=skipElse) and (IfLvl=SkipLvl) then
              CfgSyntaxError('"there was already an $else');;
            if (Skip=skipIf) and (IfLvl=SkipLvl) then begin
              // if-block was skipped -> execute else block
              if ShowDebug then
                DebugCfgDirective('execute');
              SkipLvl:=0;
              Skip:=skipNone;
            end else if Skip=skipNone then begin
              // if-block was executed -> skip else block
              if ShowDebug then
                DebugCfgDirective('skip');
              Skip:=skipElse;
            end;
          end;
        'elseif':
          begin
            if IfLvl=0 then
              CfgSyntaxError('"'+Directive+'" without ifdef');
            if (Skip=skipIf) and (IfLvl=SkipLvl) then begin
              // if-block was skipped -> try this elseif
              Expr:=copy(Line,p-PChar(Line)+1,length(Line));
              if ConditionEvaluator.Eval(Expr) then begin
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
            end else if Skip=skipNone then begin
              // if-block was executed -> skip without test
              if ShowDebug then
                DebugCfgDirective('no test -> skip');
              Skip:=skipIf;
            end;
          end;
        'endif':
          begin
            if IfLvl=0 then
              CfgSyntaxError('"'+Directive+'" without ifdef');
            dec(IfLvl);
            if IfLvl<SkipLvl then begin
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
            CfgSyntaxError('unknown directive "'+Directive+'"')
          else
            DebugCfgDirective('skipping unknown directive');
        end;
      end else if Skip=skipNone then begin
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
    Log.LogMsgIgnoreFilter(nEndOfReadingConfigFile,[CfgFilename]);
end;

procedure TPas2jsCompiler.LoadDefaultConfig;

  function TryConfig(aFilename: string): boolean;
  begin
    Result:=false;
    if aFilename='' then exit;
    aFilename:=ExpandFileNameUTF8(aFilename);
    if ShowTriedUsedFiles then
      Log.LogMsgIgnoreFilter(nConfigFileSearch,[aFilename]);
    if not FileExists(aFilename) then exit;
    Result:=true;
    LoadConfig(aFilename);
  end;

var
  aFilename: String;
begin
  // first try HOME directory
  aFilename:=ChompPathDelim(GetEnvironmentVariableUTF8('HOME'));
  if aFilename<>'' then
    if TryConfig(aFilename+PathDelim+DefaultConfigFile) then exit;

  // then try compiler directory
  if (CompilerExe<>'') then begin
    aFilename:=ExtractFilePath(CompilerExe);
    if aFilename<>'' then begin
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
  Log.LogRaw(['Fatal: ',Msg]);
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
  aProc: TPasToJsProcessor;
  Enable: Boolean;
  aPlatform: TPasToJsPlatform;
begin
  if ShowDebug then
    if Quick then
      Log.LogMsgIgnoreFilter(nQuickHandlingOption,[Param])
    else
      Log.LogMsgIgnoreFilter(nHandlingOption,[Param]);
  if Param='' then exit;
  ParamMacros.Substitute(Param,Self);
  if Param='' then exit;

  if Quick and ((Param='-h') or (Param='-?') or (Param='--help')) then begin
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
            else
              ParamFatal('unknown info option "'+p^+'" in "'+Param+'".');
            end;
            inc(p);
          until false;
          Log.LogRaw(Value);
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
      'd': // define
        if not Quick then begin
          Identifier:=copy(Param,3,length(Param));
          i:=Pos(':=',Identifier);
          if i>0 then begin
            Value:=copy(Identifier,i+2,length(Identifier));
            Identifier:=LeftStr(Identifier,i-1);
            if not IsValidIdent(Identifier) then
              ParamFatal('invalid define: "'+Param+'"');
            AddDefine(Identifier,Value);
          end else begin
            if not IsValidIdent(Identifier) then
              ParamFatal('invalid define: "'+Param+'"');
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
          'i': if not FileCache.AddIncludePaths(String(p),FromCmdLine,ErrorMsg) then
                 ParamFatal('invalid include path "'+ErrorMsg+'"');
          'u': if not FileCache.AddUnitPaths(String(p),FromCmdLine,ErrorMsg) then
                 ParamFatal('invalid unit path "'+ErrorMsg+'"');
          'U': FileCache.UnitOutputPath:=String(p);
          else UnknownParam;
          end;
        end;
      'I': // include path, same as -Fi
        if not Quick then begin
          inc(p);
          if not FileCache.AddIncludePaths(String(p),FromCmdLine,ErrorMsg) then
            ParamFatal('invalid include path "'+ErrorMsg+'"');
        end;
      'J': // extra pas2js options
        begin
          inc(p);
          c:=p^;
          inc(p);
          case c of
          'c': FileCache.AllJSIntoMainJS:=p^<>'-';
          'i':
            if p^=#0 then
              ParamFatal('missing insertion file: '+Param)
            else if not Quick then begin
              aFilename:=String(p);
              if aFilename='' then
                UnknownParam;
              if aFilename[length(aFilename)]='-' then begin
                Delete(aFilename,length(aFilename),1);
                if aFilename='' then
                  UnknownParam;
                FileCache.RemoveInsertFilename(aFilename);
              end else
                FileCache.AddInsertFilename(aFilename);
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
          'u':
            if not Quick then
              if not FileCache.AddSrcUnitPaths(String(p),FromCmdLine,ErrorMsg) then
                ParamFatal('invalid foreign unit path "'+ErrorMsg+'"');
          'e':
            begin
            Identifier:=NormalizeEncoding(String(p));
            case Identifier of
            'console','system','utf8': Log.Encoding:=Identifier;
            else ParamFatal('invalid encoding "'+String(p)+'"');
            end;
            end
          else UnknownParam;
          end;
        end;
      'M': // syntax mode
        begin
          inc(p);
          Identifier:=String(p);
          if CompareText(Identifier,'delphi')=0 then Mode:=p2jmDelphi
          else if CompareText(Identifier,'objfpc')=0 then Mode:=p2jmObjFPC
          else ParamFatal('invalid syntax mode "'+Identifier+'"');
        end;
      'N':
        begin
          inc(p);
          case p^ of
          'S': if not FileCache.AddNamespaces(String(p+1),FromCmdLine,ErrorMsg) then
                 ParamFatal('invalid namespace "'+ErrorMsg+'"');
          else UnknownParam;
          end;
        end;
      'o': // output file, main JavaScript file
        begin
          inc(p);
          FileCache.MainJSFile:=String(p);
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
          if Identifier='' then UnknownParam;
          inc(p,length(Identifier));
          Enable:=true;
          c:=Identifier[length(Identifier)];
          if c in ['+','-'] then begin
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
          if CompareText(Identifier,PasToJsProcessorNames[aProc])=0 then
            begin
            TargetProcessor:=aProc;
            Identifier:='';
            break;
            end;
        if Identifier<>'' then
          ParamFatal('invalid target processor "'+Identifier+'"');
        end;
      'S': // Syntax
        begin
          inc(p);
          ReadSyntaxFlags(Param,p);
        end;
      'T': // target platform
        begin
        inc(p);
        Identifier:=String(p);
        for aPlatform in TPasToJsPlatform do
          if CompareText(Identifier,PasToJsPlatformNames[aPlatform])=0 then
            begin
            TargetPlatform:=aPlatform;
            Identifier:='';
            break;
            end;
        if Identifier<>'' then
          ParamFatal('invalid target platform "'+Identifier+'"');
        end;
      'u': // undefine
        if not Quick then begin
          Identifier:=copy(Param,3,length(Param));
          if not IsValidIdent(Identifier) then
            ParamFatal('-u: invalid undefine: "'+Param+'"');
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
    if not Quick then begin
      // load extra config file
      aFilename:=copy(Param,2,length(Param));
      if aFilename='' then
        ParamFatal('invalid config file at param position '+IntToStr(i));
      aFilename:=ExpandFileNameUTF8(aFilename);
      if not FileExists(aFilename) then
        ParamFatal('config file not found: "'+copy(Param,2,length(Param))+'"');
      LoadConfig(aFilename);
    end;
  else
    // filename
    if (not Quick) then begin
      if not FromCmdLine then
        CfgSyntaxError('invalid parameter');
      if FileCache.MainSrcFile<>'' then
        ParamFatal('Two Pascal files. Only one Pascal file is supported.');
      aFilename:=ExpandFileNameUTF8(Param);
      if not FileExists(aFilename) then
        ParamFatal('Pascal file not found: "'+Param+'"');
      FileCache.MainSrcFile:=aFilename;
    end;
  end;
end;

procedure TPas2jsCompiler.ReadVerbosityFlags(Param: String; p: PChar);
var
  Enabled, Disabled: string;
  i: Integer;
begin
  if p^='m' then begin
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
  ReadSingleLetterOptions(Param,p,'ewnhila0bctdqxz',Enabled,Disabled);
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
    end;
  end;
end;

procedure TPas2jsCompiler.ReadSyntaxFlags(Param: String; p: PChar);
var
  Enabled, Disabled: string;
  i: Integer;
begin
  ReadSingleLetterOptions(Param,p,'c',Enabled,Disabled);
  for i:=1 to length(Enabled) do begin
    case Enabled[i] of
    '2': Mode:=p2jmObjFPC;
    'c': Options:=Options+[coAllowCAssignments];
    'd': Mode:=p2jmDelphi;
    end;
  end;
  for i:=1 to length(Disabled) do begin
    case Disabled[i] of
    '2': ;
    'c': Options:=Options-[coAllowCAssignments];
    'd': ;
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
    if p^='-' then begin
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
  r(mtInfo,nUnableToWriteFile,sUnableToWriteFile);
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
  Pas2jsPParser.RegisterMessages(Log);
end;

procedure TPas2jsCompiler.SetCompilerExe(AValue: string);
begin
  if AValue<>'' then
    AValue:=ExpandFileNameUTF8(AValue);
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

constructor TPas2jsCompiler.Create;
begin
  FOptions:=DefaultP2jsCompilerOptions;
  FLog:=TPas2jsLogger.Create;
  FParamMacros:=TPas2jsMacroEngine.Create;
  RegisterMessages;

  FFileCache:=TPas2jsFilesCache.Create(Log);
  FFileCacheAutoFree:=true;
  FLog.OnFormatPath:=@FileCache.FormatPath;

  FDefines:=TStringList.Create;
  // Done by Reset: TStringList(FDefines).Sorted:=True;
  // Done by Reset: TStringList(FDefines).Duplicates:=dupError;

  FConditionEval:=TCondDirectiveEvaluator.Create;
  FConditionEval.OnLog:=@ConditionEvalLog;
  FConditionEval.OnEvalVariable:=@ConditionEvalVariable;
  //FConditionEval.OnEvalFunction:=@ConditionEvalFunction;

  FFiles:=TAVLTree.Create(@CompareCompilerFilesPasFile);
  FUnits:=TAVLTree.Create(@CompareCompilerFilesPasUnitname);

  InitParamMacros;
  Reset;
end;

destructor TPas2jsCompiler.Destroy;
begin
  FreeAndNil(FWPOAnalyzer);

  FMainFile:=nil;
  FreeAndNil(FUnits);
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

  FreeAndNil(FParamMacros);
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
  if (i<>-1) then begin
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

  FMainFile:=nil;
  FUnits.Clear;
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

  CompilerExe:=aCompilerExe;
  FileCache.BaseDirectory:=aWorkingDir;

  // quick check command line params
  for i:=0 to ParamList.Count-1 do
    ReadParam(ParamList[i],true,true);
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
  if ShowDebug then begin
    WriteOptions;
    WriteDefines;
  end;
  if ShowDebug or ShowTriedUsedFiles then
    WriteFoldersAndSearchPaths;

  if FileCache.MainSrcFile='' then
    ParamFatal('No source file name in command line');

  // compile
  try
    Compile(StartTime);
  except
    on E: ECompilerTerminate do ;
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
    if length(s)<=MaxLineLen then begin
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
      if CodePointCount>=MaxLineLen then begin
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
  if CompilerExe<>'' then begin
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
  l('    D     : Write compiler date');
  l('    SO    : Write compiler OS');
  l('    SP    : Write compiler host processor');
  l('    TO    : Write target platform');
  l('    TP    : Write target processor');
  l('    V     : Write short compiler version');
  l('    W     : Write full compiler version');
  l('  -F...   Set file names and paths:');
  l('   -Fe<x> : Redirect output to <x>. UTF-8 encoded.');
  l('   -Fi<x> : Add <x> to include paths');
  l('   -Fu<x> : Add <x> to unit paths');
  l('   -FU<x> : Set unit output path to <x>');
  l('  -I<x>   : Add <x> to include paths, same as -Fi');
  l('  -J...  Extra options of pas2js');
  l('   -Jc    : Write all JavaScript concatenated into the output file');
  l('   -Je<x> : Encode messages as <x>.');
  l('     -Jeconsole : Console codepage. This is the default.');
  l('     -Jesystem  : System codepage. On non Windows console and system are the same.');
  l('     -Jeutf-8   : Unicode UTF-8. Default when using -Fe.');
  l('   -Ji<x> : Insert JS file <x> into main JS file. E.g. -Jirtl.js. Can be given multiple times. To remove a file name append a minus, e.g. -Jirtl.js-.');
  l('   -Jl    : lower case identifiers');
  l('   -Jm    : generate source maps');
  l('     -Jmsourceroot=<x> : use x as "sourceRoot", prefix URL for source file names.');
  l('     -Jmbasedir=<x> : write source file names relative to directory x.');
  l('     -Jminclude : include Pascal sources in source map.');
  l('     -Jm- : disable generating source maps');
  l('   -Ju<x> : Add <x> to foreign unit paths. Foreign units are not compiled.');
  //l('   -Jg<x> : Add <x> to group paths. A "-" starts a new group.');
  //l('   -JU<x> : Set unit output path of current group to <y>');
  l('  -l      : Write logo');
  l('  -MDelphi: Delphi 7 compatibility mode');
  l('  -MObjFPC: FPC''s Object Pascal compatibility mode (default)');
  l('  -NS<x>  : add <x> to namespaces. Namespaces with trailing - are removed.');
  l('            Delphi calls this flag "unit scope names".');
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
  l('    c     : Support operators like C (*=,+=,/= and -=)');
  l('    d     : Same as -Mdelphi');
  l('    2     : Same as -Mobjfpc (default)');
  l('  -T<x>   : Set target platform');
  l('    -Tbrowser : default');
  l('    -Tnodejs  : add pas.run(), includes -Jc');
  l('  -u<x>   : Undefines the symbol <x>');
  l('  -v<x>   : Be verbose. <x> is a combination of the following letters:');
  l('    e     : show errors (default)');
  l('    w     : show warnings');
  l('    n     : show notes');
  l('    h     : show hints');
  l('    i     : show info');
  l('    l     : show line numbers');
  l('    a     : show everything');
  l('    0     : show nothing (except errors)');
  l('    b     : show file names with full path');
  l('    c     : show conditionals');
  l('    t     : show tried/used files');
  l('    d     : show debug notes and info, enables -vni');
  l('    q     : show message numbers');
  l('    x     : show used tools');
  l('  -vm<x>,<y>: Do not show messages numbered <x> and <y>.');
  l('  -?      : Show this help');
  l('  -h      : Show this help');
  Log.LogLn;
  l('Macros:  $Name, $Name$ or $Name()');
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
  Log.LogRaw('Copyright (c) 2017 Mattias Gaertner and others');
end;

procedure TPas2jsCompiler.WriteVersionLine;
begin
  Log.LogRaw('Pas2JS Compiler version '+GetVersion(false));
end;

procedure TPas2jsCompiler.WriteOptions;
var
  co: TP2jsCompilerOption;
  fco: TP2jsFileCacheOption;
begin
  // boolean options
  for co in TP2jsCompilerOption do
    Log.LogMsgIgnoreFilter(nOptionIsEnabled,
      [p2jscoCaption[co],BoolToStr(co in Options,'enabled','disabled')]);
  for fco in TP2jsFileCacheOption do
    Log.LogMsgIgnoreFilter(nOptionIsEnabled,
      [p2jsfcoCaption[fco],BoolToStr(fco in FileCache.Options,'enabled','disabled')]);

  // default syntax mode
  Log.LogMsgIgnoreFilter(nSyntaxModeIs,[p2jscModeNames[Mode]]);
  // target platform
  Log.LogMsgIgnoreFilter(nTargetPlatformIs,[PasToJsPlatformNames[TargetPlatform]]);
  Log.LogMsgIgnoreFilter(nTargetProcessorIs,[PasToJsProcessorNames[TargetProcessor]]);
  // message encoding
  Log.LogMsgIgnoreFilter(nMessageEncodingIs,[IntToStr(Log.MsgCount)]);
  // source map options
  if SrcMapEnable then begin
    Log.LogMsgIgnoreFilter(nSrcMapSourceRootIs,[SrcMapSourceRoot]);
    Log.LogMsgIgnoreFilter(nSrcMapBaseDirIs,[SrcMapBaseDir]);
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
      S:=S+'='+M.Value;
    Log.LogMsgIgnoreFilter(nMacroDefined,[S]);
    end;
end;

procedure TPas2jsCompiler.WriteFoldersAndSearchPaths;

  procedure WriteFolder(aName, Folder: string);
  begin
    if Folder='' then exit;
    Log.LogMsgIgnoreFilter(nUsingPath,[aName,Folder]);
    if not DirectoryExists(ChompPathDelim(Folder)) then
      Log.LogMsgIgnoreFilter(nFolderNotFound,[aName,Folder]);
  end;

var
  i: Integer;
begin
  for i:=0 to FileCache.ForeignUnitPaths.Count-1 do
    WriteFolder('foreign unit path',FileCache.ForeignUnitPaths[i]);
  for i:=0 to FileCache.UnitPaths.Count-1 do
    WriteFolder('unit path',FileCache.UnitPaths[i]);
  for i:=0 to FileCache.IncludePaths.Count-1 do
    WriteFolder('include path',FileCache.IncludePaths[i]);
  WriteFolder('unit output path',FileCache.UnitOutputPath);
  Log.LogMsgIgnoreFilter(nNameValue,['output file',FileCache.MainJSFile]);
end;

procedure TPas2jsCompiler.WriteInfo;
begin
  WriteVersionLine;
  Log.LogLn;
  Log.LogRaw('Compiler date      : '+GetCompiledDate);
  Log.LogRaw('Compiler CPU target: '+GetCompiledTargetCPU);
  Log.LogLn;
  Log.LogRaw('Supported targets (targets marked with ''{*}'' are under development):');
  Log.LogRaw(['  ',PasToJsPlatformNames[PlatformBrowser],': webbrowser']);
  Log.LogRaw(['  ',PasToJsPlatformNames[PlatformNodeJS],': Node.js']);
  Log.LogLn;
  Log.LogRaw('Supported CPU instruction sets:');
  Log.LogRaw('  ECMAScript5, ECMAScript6');
  Log.LogLn;
  Log.LogRaw('Recognized compiler and RTL features:');
  Log.LogRaw('  RTTI,CLASSES,EXCEPTIONS,EXITCODE,RANDOM,DYNARRAYS,COMMANDARGS,');
  Log.LogRaw('  UNICODESTRINGS');
  Log.LogLn;
  Log.LogRaw('Supported Optimizations:');
  Log.LogRaw('  EnumNumbers');
  Log.LogRaw('  RemoveNotUsedPrivates');
  Log.LogLn;
  Log.LogRaw('Supported Whole Program Optimizations:');
  Log.LogRaw('  RemoveNotUsedDeclarations');
  Log.LogLn;
  Log.LogRaw('This program comes under the Library GNU General Public License');
  Log.LogRaw('For more information read COPYING.FPC, included in this distribution');
  Log.LogLn;
  Log.LogRaw('Please report bugs in our bug tracker on:');
  Log.LogRaw('                 http://bugs.freepascal.org');
  Log.LogLn;
  Log.LogRaw('More information may be found on our WWW pages (including directions');
  Log.LogRaw('for mailing lists useful for asking questions or discussing potential');
  Log.LogRaw('new features, etc.):');
  Log.LogRaw('                 http://www.freepascal.org');
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

function TPas2jsCompiler.FindPasFile(PasFilename: string): TPas2jsCompilerFile;
var
  Node: TAVLTreeNode;
begin
  Result:=nil;
  if PasFilename='' then exit;
  Node:=FFiles.FindKey(Pointer(PasFilename),@CompareFileAndCompilerFilePasFile);
  if Node=nil then exit;
  Result:=TPas2jsCompilerFile(Node.Data);
end;

procedure TPas2jsCompiler.LoadPasFile(PasFilename, UseUnitName: string; out
  aFile: TPas2jsCompilerFile);
var
  aPasTree: TPas2jsCompilerResolver;
begin
  aFile:=nil;
  Log.LogMsg(nParsingFile,[FileCache.FormatPath(PasFilename)],'',0,0,not (coShowLineNumbers in Options));

  aFile:=FindPasFile(PasFilename);
  if aFile<>nil then exit;

  if (PasFilename='') or not FileExists(PasFilename) then begin
    Log.LogMsg(nSourceFileNotFound,[PasFilename]);
    Terminate(ExitCodeFileNotFound);
  end;

  PasFilename:=ExpandFileNameUTF8(PasFilename);
  if DirectoryExists(PasFilename) then begin
    Log.LogMsg(nFileIsFolder,[PasFilename]);
    Terminate(ExitCodeFileNotFound);
  end;

  aFile:=TPas2jsCompilerFile.Create(Self,PasFilename);
  if UseUnitName<>'' then
    begin
    {$IFDEF VerboseSetPasUnitName}
    writeln('TPas2jsCompiler.LoadPasFile File="',PasFilename,'" UseUnit="',UseUnitName,'"');
    {$ENDIF}
    aFile.PasUnitName:=UseUnitName;
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
    Log.LogRaw(['Debug: Opening file "',PasFilename,'"...']);
  // open file (beware: this changes aPasTree.FileResolver.BaseDirectory)
  aFile.OpenFile(PasFilename);
end;

function TPas2jsCompiler.FindUsedUnit(const TheUnitName: string
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
  OldFile:=FindUsedUnit(aFile.PasUnitName);
  if OldFile<>nil then begin
    if OldFile<>aFile then
      RaiseInternalError(20170504161354,'duplicate unit "'+OldFile.PasUnitName+'" "'+aFile.PasFilename+'" "'+OldFile.PasFilename+'"');
  end else begin
    FUnits.Add(aFile);
  end;
end;

end.

