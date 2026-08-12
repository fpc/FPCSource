{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Basic types and the project/config-file parser for the analyzer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Types;


{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

const
  // Semantic version of the Fp-Sonar analysis core.
  FpSonarVersion = '0.1.0';

type
  { Which tier produced a diagnostic }
  TFpSonarDiagnosticKind = (dkParseError,  // parser-level failures
    dkScanError,   // raw scanner failure
    dkResolveError,
    // resolution failure (EPasResolve) caught by the tolerant resolver wrapper
    dkFileNotFound
    // the source file could not be opened (EFileNotFoundError from the resolver)
    );

  { A single structural diagnostic }
  TFpSonarDiagnostic = record
    FileName: string;
    Row: integer; // 1-based
    Col: integer; // 1-based
    Kind: TFpSonarDiagnosticKind;
    Message: string;
  end;

  TFpSonarDiagnosticArray = array of TFpSonarDiagnostic;

  // Fixed severity taxonomy.
  TFpSonarSeverity = (sevInfo, sevMinor, sevMajor, sevCritical, sevBlocker);

  // Fixed issue category/type taxonomy.
  TFpSonarIssueType = (itCodeSmell, itBug, itVulnerability);

  // Reporting confidence.
  TFpSonarConfidence = (cfLow, cfMedium, cfHigh);

  { Why was an issue suppressed?}
  TFpSonarSuppressionSource = (ssActive, ssNoSonar, ssConfig, ssBaseline);

  // Args for a message template (Format-style), produced by the emitting rule.
  TFpSonarStringArray = array of string;

  // A single normalized finding. Fingerprint is filled by the IssueCollector,
  TFpSonarIssue = record
    RuleId: string;
    FileName: string;
    StartLine: integer;
    StartCol: integer;
    EndLine: integer;
    EndCol: integer;
    Severity: TFpSonarSeverity;
    IssueType: TFpSonarIssueType;
    Confidence: TFpSonarConfidence;
    MessageKey: string;
    MessageArgs: TFpSonarStringArray;
    Fingerprint: string;
    SuppressionSource: TFpSonarSuppressionSource;
    // Builds an issue from its parts, leaving Fingerprint empty and SuppressionSource ssActive.
    class function Make(const aRuleId, aFileName: string;
      aStartLine, aStartCol, aEndLine, aEndCol: integer;
      aSeverity: TFpSonarSeverity; aType: TFpSonarIssueType;
      aConfidence: TFpSonarConfidence; const aMessageKey: string;
      const aArgs: array of string): TFpSonarIssue; static;
  end;

  TFpSonarIssueArray = array of TFpSonarIssue;

  // The result of evaluating a quality gate: whether it failed, the process exit
  // code (0 pass / 1 fail), and a human reason naming the first exceeded axis.
  // Produced by TFpSonarGateThresholds.Evaluate (FpSonar.Config).
  TFpSonarGateOutcome = record
    Failed: Boolean;
    ExitCode: Integer;
    Reason: string;
  end;

  { The source dialect. dlDefault is the byte-identical FPC path; dlPas2js
    enables pas2js parse-relevant switches and a .ppu-free, real-source
    resolution profile. dlDelphi is reserved for a future increment. }
  TFpSonarDialect = (dlDefault, dlPas2js);

  { The config tuple — the single source of truth for an analysis run. }
  TFpSonarAnalysisConfig = record
    Mode: string;                         // compiler mode, e.g. 'OBJFPC'
    Dialect: TFpSonarDialect;             // source dialect (default dlDefault)
    ModeSwitches: TFpSonarStringArray;    // carried for the resolver (not consumed yet)
    Defines: TFpSonarStringArray;         // user/project -d defines
    UnitSearchPaths: TFpSonarStringArray; // -Fu (carried for the resolver)
    IncludePaths: TFpSonarStringArray;    // -Fi (carried for the resolver)
    TargetCPU: string;                    // e.g. 'x86_64' (feeds CPU define)
    TargetOS: string;                      // e.g. 'linux' (feeds OS define(s))
    TargetFiles: TFpSonarStringArray;     // the resolved set of files to analyze
    // Built-in defaults: Mode='OBJFPC'; TargetCPU/TargetOS = the build host via the compile-time macros
    class function Default: TFpSonarAnalysisConfig; static;
    // Composes the deterministic define list passed to RuleEngine.Analyze:
    function EffectiveDefines: TFpSonarStringArray;
    // Applies precedence with Self as the base.
    function Merge(const aOverride: TFpSonarAnalysisConfig): TFpSonarAnalysisConfig;
    // A stable, human-readable one-line dump of the effective tuple
    function Description: string;
  end;

  // The five capability tiers, declared in dispatch order.
  TRuleTier = (rtLex, rtTok, rtAst, rtSem, rtUse);

  // The declared TYPE of a per-rule tunable parameter.
  TRuleParamKind = (rpkInt, rpkString, rpkRegex, rpkBool, rpkTargets);

  // One tunable parameter a rule accepts in its config 'params' object
  TRuleParamSpec = record
    Name: string;
    Kind: TRuleParamKind;
    // The rule's built-in default, rendered as text and interpreted per Kind:
    // an integer literal (rpkInt), 'true'/'false' (rpkBool), the string/regex
    // verbatim (rpkString/rpkRegex), or '' for rpkTargets (an empty
    // disallow-list). Surfaced verbatim by "fpsonar init-config".
    DefaultValue: string;
  end;

  TRuleParamSpecArray = array of TRuleParamSpec;

  // The feed a rule consumes
  TRuleFeed = (rfLineText, rfTokenStream, rfAst, rfResolver, rfUseGraph);

  // Optional descriptive tags for a rule
  TRuleTagArray = array of string;

  // First-class metadata every registered rule exposes.
  TRuleMetadata = record
    RuleId: string;
    Tier: TRuleTier;
    Feed: TRuleFeed;
    Severity: TFpSonarSeverity;
    Category: TFpSonarIssueType;
    DefaultConfidence: TFpSonarConfidence;
    DefaultEnabled: boolean;
    MessageKey: string;
    Tags: TRuleTagArray;
    Title: string;
    // A one-line, human-readable explanation of what the rule checks and why.
    // Surfaced in the IDE config editor and available to SARIF rules[]/docs.
    Description: string;
    // The tunable parameters this rule reads from its config 'params' object.
    ParamSpecs: TRuleParamSpecArray;
    // Convenience builder. Tags/Title default empty; MessageKey defaults to
    // 'rule.'+aRuleId+'.message' when aMessageKey is empty.
    class function Make(const aRuleId: string; aTier: TRuleTier; aFeed: TRuleFeed;
      aSeverity: TFpSonarSeverity; aCategory: TFpSonarIssueType;
      aConfidence: TFpSonarConfidence; aDefaultEnabled: boolean;
      const aMessageKey: string): TRuleMetadata; static;
    // Appends one tunable-parameter spec. The overloads record the rule's
    // built-in default so it can be surfaced in a generated config; the
    // no-default form is for params with no meaningful scalar default (targets).
    procedure AddParam(const aName: string; aKind: TRuleParamKind); overload;
    procedure AddParam(const aName: string; aKind: TRuleParamKind;
      const aDefault: string); overload;
    procedure AddParam(const aName: string; aKind: TRuleParamKind;
      aDefault: integer); overload;
    procedure AddParam(const aName: string; aKind: TRuleParamKind;
      aDefault: boolean); overload;
    // Sets Description and returns the (value) metadata, for fluent use at
    // registration: Make(...).WithDescription('...').
    function WithDescription(const aDescription: string): TRuleMetadata;
    // The spec for aName in ParamSpecs, or False when the rule declares no such parameter
    function FindParam(const aName: string; out aSpec: TRuleParamSpec): boolean;
    // True iff the metadata is complete: a non-empty RuleId and a non-empty MessageKey
    function IsComplete: boolean;
    // Empty when complete, otherwise a human-readable reason naming the offending rule
    function Problem: string;
  end;

  { The sole .lpi/.lpk/fpc.cfg parser (was FpSonar.ProjectModel). Stateless }
  TFpSonarProjectModel = class
  public
    // Loads aFileName, dispatching by extension (.lpi/.lpk => XML; .cfg or a
    // basename of fpc.cfg => text). Returns the DERIVED config fields.
    function TryLoad(const aFileName: string;
      out aConfig: TFpSonarAnalysisConfig;
      out aDiag: TFpSonarDiagnostic): boolean;
  end;

// Returns the one-line product/version banner printed by the CLI.
function FpSonarBanner: string;

// Registers (or overwrites) the template for a dotted message key. Idempotent.
procedure RegisterMessage(const aKey, aTemplate: string);

// Formats the template for aKey with aArgs. Unknown key -> '!'+aKey+'!';
// arg-count/type mismatch -> the template verbatim. Never empty, never raises.
function FormatMessage(const aKey: string; const aArgs: array of string): string;

// Splits a Lazarus search-path string on ';'
function SplitSearchPath(const aValue: string): TFpSonarStringArray;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Xml.Dom, Xml.Read,
{$ELSE}
  Classes, SysUtils, DOM, XMLRead,
{$ENDIF}
  FpSonar.Consts;

function FpSonarBanner: string;
begin
  Result := Format(SBanner,
    [FpSonarVersion]);
end;

// Appends aValue to aList only if no earlier entry equals it
procedure AddUnique(var aList: TFpSonarStringArray; const aValue: string);
var
  i: integer;
begin
  if aValue = '' then
    Exit;
  for i := 0 to High(aList) do
    if aList[i] = aValue then
      Exit;
  SetLength(aList, Length(aList) + 1);
  aList[High(aList)] := aValue;
end;


// Maps a TargetOS string to the FPC OS define(s).
function OSDefines(const aTargetOS: string): TFpSonarStringArray;
var
  lOS: string;
begin
  SetLength(Result, 0);
  lOS := LowerCase(aTargetOS);
  if lOS = '' then
    Exit;
  case lOS of
    'linux':
    begin
      AddUnique(Result, 'UNIX');
      AddUnique(Result, 'LINUX');
    end;
    'darwin':
    begin
      AddUnique(Result, 'UNIX');
      AddUnique(Result, 'DARWIN');
    end;
    'win32', 'win64':
    begin
      AddUnique(Result, 'WINDOWS');
      AddUnique(Result, 'MSWINDOWS');
    end;
    else
      // Unknown OS: carry the upper-cased name only (best-effort, documented).
      AddUnique(Result, UpperCase(aTargetOS));
  end;
end;


// Returns aOverride when non-empty, else aBase (scalar precedence helper).
function PickScalar(const aBase, aOverride: string): string;
begin
  if aOverride <> '' then
    Result := aOverride
  else
    Result := aBase;
end;


// Returns a new array = aBase followed by aOverride (array append precedence).
function AppendArray(const aBase, aOverride: TFpSonarStringArray): TFpSonarStringArray;
var
  i: integer;
begin
  SetLength(Result, Length(aBase) + Length(aOverride));
  for i := 0 to High(aBase) do
    Result[i] := aBase[i];
  for i := 0 to High(aOverride) do
    Result[Length(aBase) + i] := aOverride[i];
end;


// Joins an array as "[a, b, c]" for the stable description dump.
function JoinArray(const aValues: TFpSonarStringArray): string;
var
  i: integer;
begin
  Result := '[';
  for i := 0 to High(aValues) do
  begin
    if i > 0 then
      Result := Result + ', ';
    Result := Result + aValues[i];
  end;
  Result := Result + ']';
end;


class function TFpSonarIssue.Make(const aRuleId, aFileName: string;
  aStartLine, aStartCol, aEndLine, aEndCol: integer;
  aSeverity: TFpSonarSeverity; aType: TFpSonarIssueType;
  aConfidence: TFpSonarConfidence; const aMessageKey: string;
  const aArgs: array of string): TFpSonarIssue; static;
var
  i: integer;
begin
  Result.RuleId := aRuleId;
  Result.FileName := aFileName;
  Result.StartLine := aStartLine;
  Result.StartCol := aStartCol;
  Result.EndLine := aEndLine;
  Result.EndCol := aEndCol;
  Result.Severity := aSeverity;
  Result.IssueType := aType;
  Result.Confidence := aConfidence;
  Result.MessageKey := aMessageKey;
  SetLength(Result.MessageArgs, Length(aArgs));
  for i := 0 to High(aArgs) do
    Result.MessageArgs[i] := aArgs[i];
  Result.Fingerprint := '';
  // Every emit site is active until the governance pipeline classifies it.
  Result.SuppressionSource := ssActive;
end;


class function TFpSonarAnalysisConfig.Default: TFpSonarAnalysisConfig; static;
begin
  Result.Mode := 'OBJFPC';
  Result.Dialect := dlDefault;
  SetLength(Result.ModeSwitches, 0);
  SetLength(Result.Defines, 0);
  SetLength(Result.UnitSearchPaths, 0);
  SetLength(Result.IncludePaths, 0);
  Result.TargetCPU := {$I %FPCTARGETCPU%};
  Result.TargetOS := {$I %FPCTARGETOS%};
  SetLength(Result.TargetFiles, 0);
end;


function TFpSonarAnalysisConfig.EffectiveDefines: TFpSonarStringArray;
var
  lOS: TFpSonarStringArray;
  i: integer;
begin
  SetLength(Result, 0);
  // 1. FPC is always defined.
  AddUnique(Result, 'FPC');
  // 2. The CPU define, e.g. x86_64 => CPUX86_64.
  if TargetCPU <> '' then
    AddUnique(Result, 'CPU' + UpperCase(TargetCPU));
  // 3. The OS define(s).
  lOS := OSDefines(TargetOS);
  for i := 0 to High(lOS) do
    AddUnique(Result, lOS[i]);
  // 4. The user/project defines, in order, de-duplicated against the above.
  for i := 0 to High(Defines) do
    AddUnique(Result, Defines[i]);
end;


function TFpSonarAnalysisConfig.Merge(
  const aOverride: TFpSonarAnalysisConfig): TFpSonarAnalysisConfig;
begin
  // Scalars: a non-empty override wins, else keep base (Self).
  Result.Mode := PickScalar(Mode, aOverride.Mode);
  // Dialect: a non-default override wins, else keep base.
  if aOverride.Dialect <> dlDefault then
    Result.Dialect := aOverride.Dialect
  else
    Result.Dialect := Dialect;
  Result.TargetCPU := PickScalar(TargetCPU, aOverride.TargetCPU);
  Result.TargetOS := PickScalar(TargetOS, aOverride.TargetOS);
  // Arrays: append override after base (override ADDS to, never erases, base).
  Result.ModeSwitches := AppendArray(ModeSwitches, aOverride.ModeSwitches);
  Result.Defines := AppendArray(Defines, aOverride.Defines);
  Result.UnitSearchPaths := AppendArray(UnitSearchPaths, aOverride.UnitSearchPaths);
  Result.IncludePaths := AppendArray(IncludePaths, aOverride.IncludePaths);
  Result.TargetFiles := AppendArray(TargetFiles, aOverride.TargetFiles);
end;


function TFpSonarAnalysisConfig.Description: string;
begin
  Result :=
    'Mode=' + Mode +
    ' CPU=' + TargetCPU +
    ' OS=' + TargetOS +
    ' EffectiveDefines=' + JoinArray(EffectiveDefines) +
    ' UnitSearchPaths=' + JoinArray(UnitSearchPaths) +
    ' IncludePaths=' + JoinArray(IncludePaths) +
    ' TargetFiles=' + JoinArray(TargetFiles);
  // Only surfaced for a non-default dialect, so dlDefault stays byte-identical.
  if Dialect <> dlDefault then
    Result := Result + ' Dialect=pas2js';
end;


class function TRuleMetadata.Make(const aRuleId: string; aTier: TRuleTier;
  aFeed: TRuleFeed; aSeverity: TFpSonarSeverity; aCategory: TFpSonarIssueType;
  aConfidence: TFpSonarConfidence; aDefaultEnabled: boolean;
  const aMessageKey: string): TRuleMetadata; static;
begin
  Result.RuleId := aRuleId;
  Result.Tier := aTier;
  Result.Feed := aFeed;
  Result.Severity := aSeverity;
  Result.Category := aCategory;
  Result.DefaultConfidence := aConfidence;
  Result.DefaultEnabled := aDefaultEnabled;
  if aMessageKey <> '' then
    Result.MessageKey := aMessageKey
  else
    Result.MessageKey := 'rule.' + aRuleId + '.message';
  SetLength(Result.Tags, 0);
  Result.Title := '';
  Result.Description := '';
  SetLength(Result.ParamSpecs, 0);
end;


function TRuleMetadata.WithDescription(const aDescription: string): TRuleMetadata;
begin
  Result := Self;
  Result.Description := aDescription;
end;


procedure TRuleMetadata.AddParam(const aName: string; aKind: TRuleParamKind;
  const aDefault: string);
begin
  SetLength(ParamSpecs, Length(ParamSpecs) + 1);
  ParamSpecs[High(ParamSpecs)].Name := aName;
  ParamSpecs[High(ParamSpecs)].Kind := aKind;
  ParamSpecs[High(ParamSpecs)].DefaultValue := aDefault;
end;


procedure TRuleMetadata.AddParam(const aName: string; aKind: TRuleParamKind);
begin
  AddParam(aName, aKind, '');
end;


procedure TRuleMetadata.AddParam(const aName: string; aKind: TRuleParamKind;
  aDefault: integer);
begin
  AddParam(aName, aKind, IntToStr(aDefault));
end;


procedure TRuleMetadata.AddParam(const aName: string; aKind: TRuleParamKind;
  aDefault: boolean);
begin
  if aDefault then
    AddParam(aName, aKind, 'true')
  else
    AddParam(aName, aKind, 'false');
end;


function TRuleMetadata.FindParam(const aName: string;
  out aSpec: TRuleParamSpec): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to High(ParamSpecs) do
    if ParamSpecs[i].Name = aName then
    begin
      aSpec := ParamSpecs[i];
      Result := True;
      Exit;
    end;
end;


function TRuleMetadata.IsComplete: boolean;
begin
  Result := (RuleId <> '') and (MessageKey <> '');
end;


function TRuleMetadata.Problem: string;
begin
  if RuleId = '' then
    Result := 'empty RuleId'
  else if MessageKey = '' then
    Result := RuleId + ': empty MessageKey'
  else
    Result := '';
end;


type
  TMessageEntry = record
    Key: string;
    Template: string;
  end;

var
  // Unit-global catalog; insertion order is irrelevant (lookup is by key).
  GCatalog: array of TMessageEntry;

// Returns the index of aKey in GCatalog, or -1 if absent.
function IndexOfKey(const aKey: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to High(GCatalog) do
    if GCatalog[i].Key = aKey then
    begin
      Result := i;
      Exit;
    end;
end;


procedure RegisterMessage(const aKey, aTemplate: string);
var
  lIdx: integer;
begin
  lIdx := IndexOfKey(aKey);
  if lIdx >= 0 then
    GCatalog[lIdx].Template := aTemplate
  else
  begin
    SetLength(GCatalog, Length(GCatalog) + 1);
    GCatalog[High(GCatalog)].Key := aKey;
    GCatalog[High(GCatalog)].Template := aTemplate;
  end;
end;


function FormatMessage(const aKey: string; const aArgs: array of string): string;
var
  lIdx: integer;
  lTemplate: string;
  lConst: array of TVarRec;
  i: integer;
begin
  lIdx := IndexOfKey(aKey);
  if lIdx < 0 then
  begin
    // Deterministic, obvious, non-empty fallback for an unknown key.
    Result := '!' + aKey + '!';
    Exit;
  end;

  lTemplate := GCatalog[lIdx].Template;
  // array of string -> array of const (vtAnsiString) for Format.
  SetLength(lConst, Length(aArgs));
  for i := 0 to High(aArgs) do
  begin
    lConst[i].VType := vtAnsiString;
    lConst[i].VAnsiString := Pointer(aArgs[i]);
  end;

  try
    Result := Format(lTemplate, lConst);
  except
    // Arg-count/type mismatch: fall back to the template verbatim, never raise.
    on E: Exception do
      Result := lTemplate;
  end;
end;


{ Project/config parser }

// Appends aValue to aList (no de-duplication; project order preserved).
procedure AddStr(var aList: TFpSonarStringArray; const aValue: string);
begin
  SetLength(aList, Length(aList) + 1);
  aList[High(aList)] := aValue;
end;


function SplitSearchPath(const aValue: string): TFpSonarStringArray;
var
  lParts: TStringArray;
  lTrimmed: string;
  i: integer;
begin
  SetLength(Result, 0);
  lParts := aValue.Split([';']);
  for i := 0 to High(lParts) do
  begin
    lTrimmed := Trim(lParts[i]);
    if lTrimmed <> '' then
      AddStr(Result, lTrimmed);
  end;
end;


// Scans a free-form options string for -d<DEFINE> tokens.
procedure ScanDefineTokens(const aText: string; var aList: TFpSonarStringArray);
var
  lTokens: TStringArray;
  lTok: string;
  i: integer;
begin
  lTokens := aText.Split([' ', #9, #10, #13], TStringSplitOptions.ExcludeEmpty);
  for i := 0 to High(lTokens) do
  begin
    lTok := Trim(lTokens[i]);
    if (Length(lTok) > 2) and (Copy(lTok, 1, 2) = '-d') then
      AddStr(aList, Copy(lTok, 3, Length(lTok) - 2));
  end;
end;


// Resets a config to empty (TryLoad returns only derived fields).
procedure ClearConfig(out aConfig: TFpSonarAnalysisConfig);
begin
  aConfig.Mode := '';
  aConfig.Dialect := dlDefault;
  SetLength(aConfig.ModeSwitches, 0);
  SetLength(aConfig.Defines, 0);
  SetLength(aConfig.UnitSearchPaths, 0);
  SetLength(aConfig.IncludePaths, 0);
  aConfig.TargetCPU := '';
  aConfig.TargetOS := '';
  SetLength(aConfig.TargetFiles, 0);
end;


// True iff aPath is absolute (leading path delimiter.
function IsAbsolutePath(const aPath: string): boolean;
begin
  Result := (aPath <> '') and (aPath[1] = PathDelim);
end;


// Resolves a (possibly relative) project-unit path against the project file's
// directory to a normalized absolute path.
function ResolveAgainst(const aBaseDir, aRawPath: string): string;
var
  lPath: string;
begin
  // Normalize separators to the host's so ExpandFileName collapses '..' etc.
  lPath := StringReplace(aRawPath, '/', PathDelim, [rfReplaceAll]);
  lPath := StringReplace(lPath, '\', PathDelim, [rfReplaceAll]);
  if IsAbsolutePath(lPath) then
    Result := ExpandFileName(lPath)
  else
    Result := ExpandFileName(aBaseDir + lPath);
end;


{ Anchors each search-path entry against aBaseDir, so a relative '-Fu ../X' 
  resolves where the compiler resolves it — relative to the project/.cfg dir, 
  not the process CWD. 
  Note: Entries with an unexpanded macro ('$...') are left untouched }
procedure ResolvePathList(const aBaseDir: string;
  var aList: TFpSonarStringArray);
var
  i: integer;
begin
  for i := 0 to High(aList) do
    if (aList[i] <> '') and (Pos('$', aList[i]) = 0) then
      aList[i] := ResolveAgainst(aBaseDir, aList[i]);
end;


// Returns the 'Value' attribute of the named direct child of aParent, or '' if
// either the child or the attribute is absent.
function ChildValue(aParent: TDOMNode; const aName: string): string;
var
  lNode: TDOMNode;
begin
  Result := '';
  if aParent = nil then
    Exit;
  lNode := aParent.FindNode(aName);
  if (lNode <> nil) and (lNode is TDOMElement) then
    Result := TDOMElement(lNode).GetAttribute('Value');
end;


// Walks a chain of named direct children returning nil as soon as a link is missing.
function ChildPath(aRoot: TDOMNode; const aNames: array of string): TDOMNode;
var
  i: integer;
begin
  Result := aRoot;
  for i := Low(aNames) to High(aNames) do
  begin
    if Result = nil then
      Exit(nil);
    Result := Result.FindNode(aNames[i]);
  end;
end;


// Parses the .lpi/.lpk compiler options + unit list out of the DOM into aConfig.
procedure ParseXmlConfig(aDoc: TXMLDocument; const aBaseDir: string;
  aIsPackage: boolean; var aConfig: TFpSonarAnalysisConfig);
var
  lRoot, lOpts, lUnits, lUnit, lFile, lPart: TDOMNode;
  lCompilerOpts, lValue: string;
  i: integer;
begin
  lRoot := aDoc.DocumentElement;
  if lRoot = nil then
    Exit;

  // CompilerOptions lives under <Package> in a .lpk, directly under root in .lpi.
  if aIsPackage then
    lOpts := ChildPath(lRoot, ['Package', 'CompilerOptions'])
  else
    lOpts := lRoot.FindNode('CompilerOptions');

  if lOpts <> nil then
  begin
    aConfig.UnitSearchPaths := SplitSearchPath(
      ChildValue(lOpts.FindNode('SearchPaths'), 'OtherUnitFiles'));
    aConfig.IncludePaths := SplitSearchPath(
      ChildValue(lOpts.FindNode('SearchPaths'), 'IncludeFiles'));
    // Anchor relative search paths (e.g. '../X') to the project dir, matching
    // the target-file resolution — otherwise they'd resolve against the CWD.
    ResolvePathList(aBaseDir, aConfig.UnitSearchPaths);
    ResolvePathList(aBaseDir, aConfig.IncludePaths);
    lValue := ChildValue(ChildPath(lOpts, ['Parsing', 'SyntaxOptions']),
      'SyntaxMode');
    if lValue <> '' then
      aConfig.Mode := lValue;
    // Defines: scan CustomOptions for -d tokens (the common case).
    lCompilerOpts := ChildValue(lOpts.FindNode('Other'), 'CustomOptions');
    if lCompilerOpts <> '' then
      ScanDefineTokens(lCompilerOpts, aConfig.Defines);
  end;

  // Unit files => analysis targets, resolved relative to the project dir.
  if aIsPackage then
    lUnits := ChildPath(lRoot, ['Package', 'Files'])
  else
    lUnits := ChildPath(lRoot, ['ProjectOptions', 'Units']);

  if lUnits <> nil then
    for i := 0 to lUnits.ChildNodes.Count - 1 do
    begin
      lUnit := lUnits.ChildNodes[i];
      // .lpi child nodes are <Unit0>,<Unit1>,...; .lpk are <Item1>,<Item2>,...
      if (Copy(lUnit.NodeName, 1, 4) <> 'Unit')
        and (Copy(lUnit.NodeName, 1, 4) <> 'Item') then
        Continue;
      lFile := lUnit.FindNode('Filename');
      if (lFile = nil) or not (lFile is TDOMElement) then
        Continue;
      lValue := TDOMElement(lFile).GetAttribute('Value');
      if lValue = '' then
        Continue;
      // For a .lpi honour IsPartOfProject when present; absent => include.
      if not aIsPackage then
      begin
        lPart := lUnit.FindNode('IsPartOfProject');
        if (lPart <> nil) and (lPart is TDOMElement)
          and (CompareText(TDOMElement(lPart).GetAttribute('Value'),
          'True') <> 0) then
          Continue;
      end;
      AddStr(aConfig.TargetFiles, ResolveAgainst(aBaseDir, lValue));
    end;
end;


// Parses an fpc.cfg-style text file: recognises -Fu/-Fi/-d/-M directives, one
// per line; ignores comments (# / ;) and blanks; unknown directives skipped.
procedure ParseCfgConfig(const aFileName, aBaseDir: string;
  var aConfig: TFpSonarAnalysisConfig);
var
  lLines: TStringList;
  lLine: string;
  i: integer;
begin
  lLines := TStringList.Create;
  try
    lLines.LoadFromFile(aFileName);
    for i := 0 to lLines.Count - 1 do
    begin
      lLine := Trim(lLines[i]);
      if (lLine = '') or (lLine[1] = '#') or (lLine[1] = ';') then
        Continue;
      if Copy(lLine, 1, 3) = '-Fu' then
        AddStr(aConfig.UnitSearchPaths, Trim(Copy(lLine, 4, Length(lLine) - 3)))
      else if Copy(lLine, 1, 3) = '-Fi' then
        AddStr(aConfig.IncludePaths, Trim(Copy(lLine, 4, Length(lLine) - 3)))
      else if Copy(lLine, 1, 5) = '-Mode' then
        aConfig.Mode := Trim(Copy(lLine, 6, Length(lLine) - 5))
      else if Copy(lLine, 1, 2) = '-M' then
        aConfig.Mode := Trim(Copy(lLine, 3, Length(lLine) - 2))
      else if Copy(lLine, 1, 2) = '-d' then
        AddStr(aConfig.Defines, Trim(Copy(lLine, 3, Length(lLine) - 2)));
    end;
    // Anchor relative -Fu/-Fi paths to the .cfg's dir, like the .lpi/.lpk path.
    ResolvePathList(aBaseDir, aConfig.UnitSearchPaths);
    ResolvePathList(aBaseDir, aConfig.IncludePaths);
  finally
    lLines.Free;
  end;
end;


function TFpSonarProjectModel.TryLoad(const aFileName: string;
  out aConfig: TFpSonarAnalysisConfig;
  out aDiag: TFpSonarDiagnostic): boolean;
var
  lDoc: TXMLDocument;
  lExt: string;
  lBaseDir: string;
begin
  ClearConfig(aConfig);
  aDiag.FileName := aFileName;
  aDiag.Row := 0;
  aDiag.Col := 0;
  aDiag.Kind := dkParseError;
  aDiag.Message := '';
  Result := False;

  // The whole body is guarded: ReadXMLFile/LoadFromFile raise on a missing or
  // malformed file, which must become a diagnostic, never an exception.
  try
    lExt := LowerCase(ExtractFileExt(aFileName));
    lBaseDir := IncludeTrailingPathDelimiter(
      ExtractFilePath(ExpandFileName(aFileName)));

    if (lExt = '.lpi') or (lExt = '.lpk') then
    begin
      lDoc := nil;
      try
        ReadXMLFile(lDoc, aFileName);
        ParseXmlConfig(lDoc, lBaseDir, lExt = '.lpk', aConfig);
      finally
        // DOM trees are heap-owned — free on every path.
        lDoc.Free;
      end;
    end
    else
      // .cfg / fpc.cfg (or any other extension) => text directive parse.
      ParseCfgConfig(aFileName, lBaseDir, aConfig);

    Result := True;
  except
    on E: Exception do
    begin
      ClearConfig(aConfig);
      aDiag.FileName := aFileName;
      aDiag.Row := 0;
      aDiag.Col := 0;
      aDiag.Kind := dkParseError;
      aDiag.Message := E.Message;
      Result := False;
    end;
  end;
end;


initialization
  // Parse/scan-error catalog seed (reserved RuleIds).
  RegisterMessage('rule.ParseError.message', SParseError);
  RegisterMessage('rule.ScanError.message', SScanError);
  RegisterMessage('rule.FileNotFound.message', SFileNotFound);

end.
