{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Rule contract, self-registration registry, and tier dispatch engine

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.RuleFramework;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  FpSonar.Types, FpSonar.Config, FpSonar.Issues, FpSonar.Ingest,
  FpSonar.Resolver, FpSonar.Traversal, FpSonar.SourceFile,
{$IFDEF FPC_DOTTEDUNITS}
  Pascal.Tree;
{$ELSE}
  PasTree;
{$ENDIF}

type
  // A read-only view over one analyzed file, handed to every rule.
  TRuleContext = record
    FileName: string;
    CompilerMode: string;
    // Tokens survive a failed parse.
    Tokens: TFpSonarTokenArray;
    // Lines are the raw physical lines, 1-based to the rule (Lines[0] = source line 1),
    Lines: TFpSonarStringArray;
    // Module is nil when the parse failed;
    Module: TPasModule;
    ParseSucceeded: boolean;
    // The tolerant SEM resolver for this file, or nil when the parse failed or no resolver was built.
    Resolver: TFpSonarResolver;
    // The resolved ruleset/fail policy.
    Config: TFpSonarConfig;
    // The project-wide name-reference index (USE tier), or nil for a  single-file run with no project context.
    ProjectIndex: TFpSonarProjectIndex;
  end;

  { The rule contract (CORBA — non-refcounted; the registry owns the object). }
  IAnalysisRule = interface
    ['{BD899C2A-989D-4461-826C-2A781D7D9CF8}']
    // The rule's first-class metadata (tier, feed, identity, taxonomy).
    function Metadata: TRuleMetadata;
    // Inspect the declared feed in aContext; emit findings via aCollector only.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector);
  end;

  { Abstract base every concrete rule descends. Constructed with its metadata;
    the registry owns and frees it (CORBA => no refcount). }
  TRuleBase = class(TObject, IAnalysisRule)
  protected
    FMetadata: TRuleMetadata;
  public
    // Stores the metadata the rule will expose for the rest of its life.
    constructor Create(const aMetadata: TRuleMetadata);
    // The rule's first-class metadata.
    function Metadata: TRuleMetadata;
    // Inspect the declared feed; emit findings via aCollector only.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); virtual; abstract;
  end;

  { Owns its registered rules; validates metadata; dispatch is the engine's job. }
  TRuleRegistry = class
  private
    FRules: array of TRuleBase;
    function GetCount: integer;
  public
    // Frees every owned rule before the registry itself goes away.
    destructor Destroy; override;
    // Appends aRule and TAKES OWNERSHIP. No completeness gate here: incomplete
    // rules must still register so Validate can flag them.
    procedure Register(aRule: TRuleBase);
    // The owned rule at aIndex (0-based), exposed as its concrete base.
    function Rule(aIndex: integer): TRuleBase;
    // The first registered rule with aRuleId, or nil if none.
    function FindById(const aRuleId: string): TRuleBase;
    // Problem strings: every rule failing IsComplete plus every
    // duplicate RuleId. Empty array => the registry is clean.
    function Validate: TFpSonarStringArray;
    // Validates a loaded config's per-rule 'params' against this registry's rule
    // metadata: unknown param key, wrong kind, or an uncompilable rpkRegex is a
    // False + aError. An unknown rule id is IGNORED (forward-compat). Nil-safe.
    function ValidateConfig(const aConfig: TFpSonarConfig;
      out aError: string): boolean;
    // Frees all owned rules and empties the registry (test-local reuse).
    procedure Clear;
    // Number of registered rules.
    property Count: integer read GetCount;
  end;

  { Dispatches a registry's enabled rules over one analyzed file in tier order
    with graceful degradation and a per-rule fault boundary. }
  TFpSonarRuleEngine = class
  private
    FRegistry: TRuleRegistry;
    FConfig: TFpSonarConfig;
    FSuppressionMap: TFpSonarSuppressionMap;
    FProjectIndex: TFpSonarProjectIndex;
    FPpuAutoDetect: boolean;
    FPpuCacheDir: string;
    function FeedAvailable(aFeed: TRuleFeed;
      const aContext: TRuleContext): boolean;
    procedure RunRule(aRule: TRuleBase; const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector);
  public
    // Dispatch over the global RuleRegistry.
    constructor Create;
    // Dispatch over a specific registry.
    constructor CreateWith(aRegistry: TRuleRegistry);
    // Analyzes aFileName  then dispatches the registry's rules over it.
    procedure Analyze(const aFileName, aCompilerMode: string;
      const aDefines: array of string;
      const aCollector: TFpSonarIssueCollector); overload;
    // As above, going over the resolver's unit/include search paths
    // down to the SEM resolver built in the per-unit boundary.
    procedure Analyze(const aFileName, aCompilerMode: string;
      const aDefines, aUnitPaths, aIncludePaths: array of string;
      const aCollector: TFpSonarIssueCollector); overload;
    // As above, additionally opting the per-unit SEM resolver into the real-RTL-preferred mode
    // and selecting the source dialect (dlPas2js enables pas2js parsing).
    procedure Analyze(const aFileName, aCompilerMode: string;
      const aDefines, aUnitPaths, aIncludePaths: array of string;
      aRealRtl: boolean; aTargetPointerSize: integer;
      const aCollector: TFpSonarIssueCollector;
      aDialect: TFpSonarDialect = dlDefault); overload;
    // The pure dispatch over an already-analyzed SourceFile, so tests can drive it over a hand-analyzed file.
    procedure Dispatch(aSourceFile: TFpSonarSourceFile;
      const aFileName, aCompilerMode: string;
      const aCollector: TFpSonarIssueCollector);
    // The metadata-completeness check over the wrapped registry.
    function ValidateRegistry: TFpSonarStringArray;
    // The ruleset/fail policy. Defaults to TFpSonarConfig.Default;
    property Config: TFpSonarConfig read FConfig write FConfig;
    // The optional NOSONAR suppression map, caller-owned. Default Nil.
    property SuppressionMap: TFpSonarSuppressionMap
      read FSuppressionMap write FSuppressionMap;
    // The optional project-wide name-reference index, caller-owned. Defaults to nil.
    property ProjectIndex: TFpSonarProjectIndex read FProjectIndex write FProjectIndex;
    // Auto-detect ppudump-from-live-.ppu resolution
    property PpuAutoDetect: boolean read FPpuAutoDetect write FPpuAutoDetect;
    // Persistent ppudump-stub cache directory (--ppu-cache)
    property PpuCacheDir: string read FPpuCacheDir write FPpuCacheDir;
  end;

// The unit-global default registry singleton.
function RuleRegistry: TRuleRegistry;

// Self-registration entry every rule unit calls in its initialization:
procedure RegisterRule(aRule: TRuleBase);


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Regexpr,
{$ELSE}
  SysUtils, regexpr,
{$ENDIF}
  FpSonar.Consts;

var
  // The process-wide registry, owned by this unit (freed in finalization).
  GRuleRegistry: TRuleRegistry;

  { TRuleBase }

constructor TRuleBase.Create(const aMetadata: TRuleMetadata);
begin
  inherited Create;
  FMetadata := aMetadata;
end;


function TRuleBase.Metadata: TRuleMetadata;
begin
  Result := FMetadata;
end;


{ TRuleRegistry }

destructor TRuleRegistry.Destroy;
begin
  Clear;
  inherited Destroy;
end;


function TRuleRegistry.GetCount: integer;
begin
  Result := Length(FRules);
end;


procedure TRuleRegistry.Register(aRule: TRuleBase);
begin
  SetLength(FRules, Length(FRules) + 1);
  FRules[High(FRules)] := aRule;
end;


function TRuleRegistry.Rule(aIndex: integer): TRuleBase;
begin
  Result := FRules[aIndex];
end;


function TRuleRegistry.FindById(const aRuleId: string): TRuleBase;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to High(FRules) do
    if FRules[i].Metadata.RuleId = aRuleId then
    begin
      Result := FRules[i];
      Exit;
    end;
end;


function TRuleRegistry.Validate: TFpSonarStringArray;
var
  i, j: integer;
  lProblem: string;
  lRuleId: string;
  lDuplicate: boolean;

  procedure AddProblem(const aProblem: string);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := aProblem;
  end;

begin
  SetLength(Result, 0);

  // Completeness problems first (registration order, deterministic).
  for i := 0 to High(FRules) do
  begin
    lProblem := FRules[i].Metadata.Problem;
    if lProblem <> '' then
      AddProblem(lProblem);
  end;

  // Then duplicate RuleIds: report once.
  for i := 0 to High(FRules) do
  begin
    lRuleId := FRules[i].Metadata.RuleId;
    if lRuleId = '' then
      Continue;
    lDuplicate := False;
    for j := 0 to i - 1 do
      if FRules[j].Metadata.RuleId = lRuleId then
      begin
        lDuplicate := True;
        Break;
      end;
    if lDuplicate then
      AddProblem(lRuleId + ': duplicate RuleId');
  end;
end;


procedure TRuleRegistry.Clear;
var
  i: integer;
begin
  for i := 0 to High(FRules) do
  begin
    FRules[i].Free;
    FRules[i] := nil;
  end;
  SetLength(FRules, 0);
end;


function RuleRegistry: TRuleRegistry;
begin
  Result := GRuleRegistry;
end;


procedure RegisterRule(aRule: TRuleBase);
begin
  GRuleRegistry.Register(aRule);
end;


// True iff a configured param value of kind aActual satisfies a spec of kind aDeclared.
function ParamKindMatches(aDeclared: TRuleParamKind;
  aActual: TFpSonarRuleParamKind): boolean;
begin
  case aDeclared of
    rpkInt: Result := aActual = cpkInt;
    rpkString, rpkRegex: Result := aActual = cpkStr;
    rpkBool: Result := aActual = cpkBool;
    rpkTargets: Result := aActual = cpkTargets;
    else
      Result := False;
  end;
end;


// The human-readable name of a declared param kind, for the validation error.
function ParamKindName(aKind: TRuleParamKind): string;
begin
  case aKind of
    rpkInt: Result := 'an integer';
    rpkString: Result := 'a string';
    rpkRegex: Result := 'a string (regex pattern)';
    rpkBool: Result := 'a boolean';
    rpkTargets: Result := 'an array of targets';
    else
      Result := 'a value';
  end;
end;


// True iff aPattern compiles as a regular expression.
function RegexCompiles(const aPattern: string): boolean;
var
  lRe: TRegExpr;
begin
  Result := True;
  lRe := TRegExpr.Create;
  try
    try
      lRe.Expression := aPattern;
      lRe.Compile;
    except
      on E: Exception do
        Result := False;
    end;
  finally
    lRe.Free;
  end;
end;


function TRuleRegistry.ValidateConfig(const aConfig: TFpSonarConfig;
  out aError: string): boolean;
var
  i, j: integer;
  lRule: TRuleBase;
  lMeta: TRuleMetadata;
  lParam: TFpSonarRuleParam;
  lSpec: TRuleParamSpec;
begin
  Result := False;
  aError := '';
  // Nil-safe: a nil registry validates nothing (Self=nil short-circuits before
  // any method call).
  if Self = nil then
  begin
    Result := True;
    Exit;
  end;
  for i := 0 to High(aConfig.Rules) do
  begin
    // Unknown rule id => IGNORED (forward-compat), exactly like enable/severity.
    lRule := FindById(aConfig.Rules[i].RuleId);
    if lRule = nil then
      Continue;
    lMeta := lRule.Metadata;
    for j := 0 to High(aConfig.Rules[i].Params) do
    begin
      lParam := aConfig.Rules[i].Params[j];
      if not lMeta.FindParam(lParam.Key, lSpec) then
      begin
        aError := 'rule "' + aConfig.Rules[i].RuleId +
          '": unknown parameter "' + lParam.Key + '"';
        Exit;
      end;
      if not ParamKindMatches(lSpec.Kind, lParam.Kind) then
      begin
        aError := 'rule "' + aConfig.Rules[i].RuleId + '": parameter "' +
          lParam.Key + '" must be ' + ParamKindName(lSpec.Kind);
        Exit;
      end;
      if (lSpec.Kind = rpkRegex) and not RegexCompiles(lParam.StrVal) then
      begin
        aError := 'rule "' + aConfig.Rules[i].RuleId + '": parameter "' +
          lParam.Key + '" is not a valid regular expression';
        Exit;
      end;
    end;
  end;
  Result := True;
end;

constructor TFpSonarRuleEngine.Create;
begin
  inherited Create;
  FRegistry := RuleRegistry;
  FConfig := TFpSonarConfig.Default;
end;


constructor TFpSonarRuleEngine.CreateWith(aRegistry: TRuleRegistry);
begin
  inherited Create;
  FRegistry := aRegistry;
  FConfig := TFpSonarConfig.Default;
end;


function TFpSonarRuleEngine.FeedAvailable(aFeed: TRuleFeed;
  const aContext: TRuleContext): boolean;
begin
  case aFeed of
    // Token stream survives a failed parse.
    rfLineText, rfTokenStream:
      Result := True;
    // AST is available only when the parse produced a module.
    rfAst:
      Result := aContext.Module <> nil;
    // SEM resolver feed: available iff a resolver was built for this file and it produced a fully resolved module.
    rfResolver:
      Result := (aContext.Resolver <> nil) and aContext.Resolver.Succeeded;
      // No use-graph feed exists yet — always skip.
    else
      Result := False;
  end;
end;


procedure TFpSonarRuleEngine.RunRule(aRule: TRuleBase;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
begin
  // Per-rule fault boundary: a raising rule becomes a RuleError issue
  // and the engine continues with the remaining rules.
  try
    aRule.Apply(aContext, aCollector);
  except
    on E: Exception do
      aCollector.AddIssue('RuleError', aContext.FileName, 0, 0, 0, 0,
        sevMajor, itCodeSmell, cfHigh, 'rule.RuleError.message',
        [aRule.Metadata.RuleId, E.Message], aRule.Metadata.RuleId);
  end;
end;


procedure TFpSonarRuleEngine.Analyze(const aFileName, aCompilerMode: string;
  const aDefines: array of string;
  const aCollector: TFpSonarIssueCollector);
begin
  // No extra resolver search paths.
  Analyze(aFileName, aCompilerMode, aDefines, [], [], aCollector);
end;


procedure TFpSonarRuleEngine.Analyze(const aFileName, aCompilerMode: string;
  const aDefines, aUnitPaths, aIncludePaths: array of string;
  const aCollector: TFpSonarIssueCollector);
begin
  Analyze(aFileName, aCompilerMode, aDefines, aUnitPaths, aIncludePaths,
    False, 0, aCollector);
end;


procedure TFpSonarRuleEngine.Analyze(const aFileName, aCompilerMode: string;
  const aDefines, aUnitPaths, aIncludePaths: array of string;
  aRealRtl: boolean; aTargetPointerSize: integer;
  const aCollector: TFpSonarIssueCollector;
  aDialect: TFpSonarDialect = dlDefault);
var
  lSourceFile: TFpSonarSourceFile;
begin
  // The engine owns the SourceFile it creates and frees it here.
  lSourceFile := TFpSonarSourceFile.Create;
  try
    // Story 6-18 — forward auto-detect ppudump resolution into the per-unit boundary.
    lSourceFile.PpuAutoDetect := FPpuAutoDetect;
    lSourceFile.PpuCacheDir := FPpuCacheDir;
    lSourceFile.Analyze(aFileName, aCompilerMode, aDefines, aUnitPaths,
      aIncludePaths, aRealRtl, aTargetPointerSize, aDialect);
    Dispatch(lSourceFile, aFileName, aCompilerMode, aCollector);
  finally
    lSourceFile.Free;
  end;
end;


procedure TFpSonarRuleEngine.Dispatch(aSourceFile: TFpSonarSourceFile;
  const aFileName, aCompilerMode: string;
  const aCollector: TFpSonarIssueCollector);
var
  lContext: TRuleContext;
  lTier: TRuleTier;
  lMeta: TRuleMetadata;
  lRule: TRuleBase;
  i: integer;
begin
  // 1. Fold diagnostics FIRST so ParseError/ScanError precede rule issues.
  for i := 0 to High(aSourceFile.Diagnostics) do
    aCollector.CollectDiagnostic(aSourceFile.Diagnostics[i]);

  // 2. Build the read-only context (filename threaded in explicitly).
  lContext.FileName := aFileName;
  lContext.CompilerMode := aCompilerMode;
  lContext.Tokens := aSourceFile.Tokens;
  lContext.Lines := aSourceFile.Lines;
  lContext.Module := aSourceFile.Module;
  lContext.ParseSucceeded := aSourceFile.ParseSucceeded;
  // SEM resolver handle: nil when the parse failed;
  lContext.Resolver := aSourceFile.Resolver;
  // The resolved ruleset/fail policy: a migrated rule reads its tunable params from here.
  lContext.Config := FConfig;
  // The project-wide name-reference index (USE tier): nil for a single-file run
  lContext.ProjectIndex := FProjectIndex;

  // 2b. Populate the optional NOSONAR map from this file's comment tokens.
  if FSuppressionMap <> nil then
    FSuppressionMap.AddFile(aFileName, aSourceFile.Tokens);

  // 3. Tiers in fixed order; within a tier, registry in registration order.
  for lTier := Low(TRuleTier) to High(TRuleTier) do
    for i := 0 to FRegistry.Count - 1 do
    begin
      lRule := FRegistry.Rule(i);
      lMeta := lRule.Metadata;
      if (lMeta.Tier = lTier)
        and FConfig.RuleEnabled(lMeta.RuleId, lMeta.DefaultEnabled)
        and FeedAvailable(lMeta.Feed, lContext) then
        RunRule(lRule, lContext, aCollector);
    end;
end;


function TFpSonarRuleEngine.ValidateRegistry: TFpSonarStringArray;
begin
  Result := FRegistry.Validate;
end;


initialization
  GRuleRegistry := TRuleRegistry.Create;
  RegisterMessage('rule.RuleError.message', SRuleError);

finalization
  GRuleRegistry.Free;
  GRuleRegistry := nil;
end.
