{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Naming-convention AST analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Naming;

{ Naming AST rules:
  a declared name that does not match its default pattern,
  plus a cross-cutting minimum-length check. Patterns are config-tunable. }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  Pascal.Tree,
{$ELSE}
  PasTree,
{$ENDIF}
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Traversal, FpSonar.Rules.Consts;

type
  { Flags a class type whose name fails the configured pattern. }
  TRuleClassNaming = class(TRuleBase)
  public
    // Emits one issue per class type whose name does not match the pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a record type whose name fails the configured pattern. }
  TRuleRecordNaming = class(TRuleBase)
  public
    // Emits one issue per record type whose name does not match the pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an interface type whose name fails the configured pattern. }
  TRuleInterfaceNaming = class(TRuleBase)
  public
    // Emits one issue per interface type whose name does not match the pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an enumeration type whose name fails the configured pattern. }
  TRuleEnumNaming = class(TRuleBase)
  public
    // Emits one issue per enumeration type whose name does not match the pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a class/record/type helper whose name fails the pattern. }
  TRuleHelperNaming = class(TRuleBase)
  public
    // Emits one issue per helper type whose name does not match the pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a pointer type whose name fails the configured pattern. }
  TRulePointerNaming = class(TRuleBase)
  public
    // Emits one issue per pointer type whose name does not match the pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a custom attribute class whose name fails the pattern. }
  TRuleAttributeNaming = class(TRuleBase)
  public
    // Emits one issue per attribute class whose name does not match the pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a constant whose name fails the configured pattern. }
  TRuleConstantNaming = class(TRuleBase)
  public
    // Emits one issue per constant whose name does not match the pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a class/record field whose name fails the configured pattern. }
  TRuleFieldNaming = class(TRuleBase)
  public
    // Emits one issue per field whose name does not match the pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a (unit-level) variable whose name fails the pattern. }
  TRuleVariableNaming = class(TRuleBase)
  public
    // Emits one issue per variable whose name does not match the pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a routine (procedure/function/method/destructor, NOT a
    constructor) whose simple name fails the configured pattern. }
  TRuleRoutineNaming = class(TRuleBase)
  public
    // Emits one issue per non-constructor routine whose name fails the pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a constructor whose simple name fails the configured pattern. }
  TRuleConstructorNaming = class(TRuleBase)
  public
    // Emits one issue per constructor whose name does not match the pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a unit whose name fails the configured pattern (programs and
    libraries are excluded). }
  TRuleUnitNaming = class(TRuleBase)
  public
    // Emits one issue when the module's unit name does not match the pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags any declared identifier shorter than cMinIdentLength that is not
    on the conventional short-name allowlist. A LENGTH check, orthogonal to the
    pattern rules (overlap is intended). }
  TRuleIdentifierTooShort = class(TRuleBase)
  public
    // Emits one issue per too-short declared identifier across all surfaces.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Regexpr,
{$ELSE}
  SysUtils, regexpr,
{$ENDIF}
  FpSonar.Config;

const
  // Per-kind default name patterns. Anchored ^...$, so a successful Exec is a
  // whole-string match.
  cPatternClass = '^T[A-Z][A-Za-z0-9]*$';
  cPatternRecord = '^T[A-Z][A-Za-z0-9]*$';
  cPatternInterface = '^I[A-Z][A-Za-z0-9]*$';
  cPatternEnum = '^T[A-Z][A-Za-z0-9]*$';
  cPatternHelper = '^T[A-Z][A-Za-z0-9]*Helper$';
  // Member/value defaults.
  cPatternPointer = '^P[A-Z][A-Za-z0-9]*$';
  cPatternAttribute = '^[A-Z][A-Za-z0-9]*Attribute$';
  cPatternConst = '^[A-Z][A-Za-z0-9_]*$';
  cPatternField = '^F[A-Z][A-Za-z0-9]*$';
  cPatternVariable = '^[A-Z][A-Za-z0-9]*$';
  // Routine/unit defaults.
  cPatternRoutine = '^[A-Z][A-Za-z0-9]*$';
  cPatternConstructor = '^Create[A-Za-z0-9]*$';
  cPatternUnit = '^[A-Z][A-Za-z0-9.]*$';
  // Minimum declared-identifier length and the conventional short-name allowlist — both case-sensitive.
  cMinIdentLength = 3;
  cShortNameAllow: array[0..6] of string = ('i', 'j', 'k', 'x', 'y', 'z', 'n');

  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyClassNaming = 'rule.ClassNaming.message';
  cKeyRecordNaming = 'rule.RecordNaming.message';
  cKeyInterfaceNaming = 'rule.InterfaceNaming.message';
  cKeyEnumNaming = 'rule.EnumNaming.message';
  cKeyHelperNaming = 'rule.HelperNaming.message';
  cKeyPointerNaming = 'rule.PointerNaming.message';
  cKeyAttributeNaming = 'rule.AttributeNaming.message';
  cKeyConstantNaming = 'rule.ConstantNaming.message';
  cKeyFieldNaming = 'rule.FieldNaming.message';
  cKeyVariableNaming = 'rule.VariableNaming.message';
  cKeyRoutineNaming = 'rule.RoutineNaming.message';
  cKeyConstructorNaming = 'rule.ConstructorNaming.message';
  cKeyUnitNaming = 'rule.UnitNaming.message';
  cKeyIdentifierTooShort = 'rule.IdentifierTooShort.message';

// True iff aName fully matches aPattern. Case-sensitive
function MatchesPattern(const aName, aPattern: string): boolean;
var
  lRe: TRegExpr;
begin
  lRe := TRegExpr.Create;
  try
    lRe.Expression := aPattern;
    Result := lRe.Exec(aName);
  finally
    lRe.Free;
  end;
end;


// Returns the trailing identifier of aName: the substring after the last '.' or aName unchanged when there is no '.'.
function LastIdentifier(const aName: string): string;
var
  i: integer;
begin
  for i := Length(aName) downto 1 do
    if aName[i] = '.' then
    begin
      Result := Copy(aName, i + 1, Length(aName) - i);
      Exit;
    end;
  Result := aName;
end;


// True iff aName is one of the conventional short identifiers the length rule excludes.
function IsShortNameAllowed(const aName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := Low(cShortNameAllow) to High(cShortNameAllow) do
    if aName = cShortNameAllow[i] then
    begin
      Result := True;
      Exit;
    end;
end;


// Core emitter — one issue at (aLine,1)-(aLine,1),
// reporting aName with [aName, aArg] as the two message args and aName as the snippet.
procedure EmitNaming(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aLine: integer;
  const aName, aArg: string); overload;
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aLine, 1, aLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    [aName, aArg], aName);
end;


// Element form — emits for aElem at its own declaration line, reporting its own Name.
procedure EmitNaming(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aElem: TPasElement;
  const aPattern: string); overload;
begin
  EmitNaming(aMeta, aContext, aCollector, aElem.SourceLinenumber, aElem.Name,
    aPattern);
end;


{ True iff aType is a non-helper class whose (syntactic, unresolved) immediate
  ancestor name ends in 'Attribute' — the AST-tier "is a custom attribute"
  heuristic (no type resolution available at this tier) }
function IsAttributeCandidate(aType: TPasType): boolean;
const
  cAttrSuffix = 'Attribute';
var
  lAnc: TPasType;
  lName: string;
begin
  Result := False;
  if not ((aType is TPasClassType) and (TPasClassType(aType).ObjKind = okClass)) then
    Exit;
  lAnc := TPasClassType(aType).AncestorType;
  if (lAnc = nil) or (lAnc.Name = '') then
    Exit;
  lName := lAnc.Name;
  Result := (Length(lName) >= Length(cAttrSuffix))
    and (Copy(lName, Length(lName) - Length(cAttrSuffix) + 1, Length(cAttrSuffix))
    = cAttrSuffix);
end;


{ TRuleClassNaming }

procedure TRuleClassNaming.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  i: integer;
  lType: TPasType;
  lPat: string;
begin
  lPat := aContext.Config.RuleParamStr(FMetadata.RuleId, 'pattern',
    cPatternClass);
  lTypes := EnumerateTypes(aContext.Module);
  for i := 0 to High(lTypes) do
  begin
    lType := lTypes[i];
    if (lType.Name <> '') and (lType is TPasClassType)
      and (TPasClassType(lType).ObjKind = okClass)
      and not IsAttributeCandidate(lType)
      and not MatchesPattern(lType.Name, lPat) then
      EmitNaming(FMetadata, aContext, aCollector, lType, lPat);
  end;
end;


{ TRuleRecordNaming }

procedure TRuleRecordNaming.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  i: integer;
  lType: TPasType;
  lPat: string;
begin
  lPat := aContext.Config.RuleParamStr(FMetadata.RuleId, 'pattern',
    cPatternRecord);
  lTypes := EnumerateTypes(aContext.Module);
  for i := 0 to High(lTypes) do
  begin
    lType := lTypes[i];
    if (lType.Name <> '') and (lType is TPasRecordType)
      and not MatchesPattern(lType.Name, lPat) then
      EmitNaming(FMetadata, aContext, aCollector, lType, lPat);
  end;
end;


{ TRuleInterfaceNaming }

procedure TRuleInterfaceNaming.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  i: integer;
  lType: TPasType;
  lPat: string;
begin
  lPat := aContext.Config.RuleParamStr(FMetadata.RuleId, 'pattern',
    cPatternInterface);
  lTypes := EnumerateTypes(aContext.Module);
  for i := 0 to High(lTypes) do
  begin
    lType := lTypes[i];
    if (lType.Name <> '') and (lType is TPasClassType)
      and (TPasClassType(lType).ObjKind = okInterface)
      and not MatchesPattern(lType.Name, lPat) then
      EmitNaming(FMetadata, aContext, aCollector, lType, lPat);
  end;
end;


{ TRuleEnumNaming }

procedure TRuleEnumNaming.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  i: integer;
  lType: TPasType;
  lPat: string;
begin
  lPat := aContext.Config.RuleParamStr(FMetadata.RuleId, 'pattern',
    cPatternEnum);
  lTypes := EnumerateTypes(aContext.Module);
  for i := 0 to High(lTypes) do
  begin
    lType := lTypes[i];
    if (lType.Name <> '') and (lType is TPasEnumType)
      and not MatchesPattern(lType.Name, lPat) then
      EmitNaming(FMetadata, aContext, aCollector, lType, lPat);
  end;
end;


{ TRuleHelperNaming }

procedure TRuleHelperNaming.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  i: integer;
  lType: TPasType;
  lPat: string;
begin
  lPat := aContext.Config.RuleParamStr(FMetadata.RuleId, 'pattern',
    cPatternHelper);
  lTypes := EnumerateTypes(aContext.Module);
  for i := 0 to High(lTypes) do
  begin
    lType := lTypes[i];
    if (lType.Name <> '') and (lType is TPasClassType)
      and (TPasClassType(lType).ObjKind in [okClassHelper, okRecordHelper,
      okTypeHelper])
      and not MatchesPattern(lType.Name, lPat) then
      EmitNaming(FMetadata, aContext, aCollector, lType, lPat);
  end;
end;


{ TRulePointerNaming }

procedure TRulePointerNaming.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  i: integer;
  lType: TPasType;
  lPat: string;
begin
  lPat := aContext.Config.RuleParamStr(FMetadata.RuleId, 'pattern',
    cPatternPointer);
  lTypes := EnumerateTypes(aContext.Module);
  for i := 0 to High(lTypes) do
  begin
    lType := lTypes[i];
    if (lType.Name <> '') and (lType is TPasPointerType)
      and not MatchesPattern(lType.Name, lPat) then
      EmitNaming(FMetadata, aContext, aCollector, lType, lPat);
  end;
end;


{ TRuleAttributeNaming }

procedure TRuleAttributeNaming.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  i: integer;
  lType: TPasType;
  lPat: string;
begin
  lPat := aContext.Config.RuleParamStr(FMetadata.RuleId, 'pattern',
    cPatternAttribute);
  lTypes := EnumerateTypes(aContext.Module);
  for i := 0 to High(lTypes) do
  begin
    lType := lTypes[i];
    if (lType.Name <> '') and IsAttributeCandidate(lType)
      and not MatchesPattern(lType.Name, lPat) then
      EmitNaming(FMetadata, aContext, aCollector, lType, lPat);
  end;
end;


{ TRuleConstantNaming }

procedure TRuleConstantNaming.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lVals: TPasValueDeclArray;
  i: integer;
  lPat: string;
begin
  lPat := aContext.Config.RuleParamStr(FMetadata.RuleId, 'pattern',
    cPatternConst);
  lVals := EnumerateValueDecls(aContext.Module);
  for i := 0 to High(lVals) do
    if (lVals[i].Kind = vkConst) and (lVals[i].Decl.Name <> '')
      and not MatchesPattern(lVals[i].Decl.Name, lPat) then
      EmitNaming(FMetadata, aContext, aCollector, lVals[i].Decl, lPat);
end;


{ TRuleFieldNaming }

procedure TRuleFieldNaming.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lVals: TPasValueDeclArray;
  i: integer;
  lPat: string;
begin
  lPat := aContext.Config.RuleParamStr(FMetadata.RuleId, 'pattern',
    cPatternField);
  lVals := EnumerateValueDecls(aContext.Module);
  for i := 0 to High(lVals) do
    if (lVals[i].Kind = vkField) and (lVals[i].Decl.Name <> '')
      and not MatchesPattern(lVals[i].Decl.Name, lPat) then
      EmitNaming(FMetadata, aContext, aCollector, lVals[i].Decl, lPat);
end;


{ TRuleVariableNaming }

procedure TRuleVariableNaming.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lVals: TPasValueDeclArray;
  i: integer;
  lPat: string;
begin
  lPat := aContext.Config.RuleParamStr(FMetadata.RuleId, 'pattern',
    cPatternVariable);
  lVals := EnumerateValueDecls(aContext.Module);
  for i := 0 to High(lVals) do
    if (lVals[i].Kind = vkVar) and (lVals[i].Decl.Name <> '')
      and not MatchesPattern(lVals[i].Decl.Name, lPat) then
      EmitNaming(FMetadata, aContext, aCollector, lVals[i].Decl, lPat);
end;


{ TRuleRoutineNaming }

procedure TRuleRoutineNaming.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  i: integer;
  lName, lPat: string;
begin
  lPat := aContext.Config.RuleParamStr(FMetadata.RuleId, 'pattern',
    cPatternRoutine);
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
    if not (lRoutines[i].Decl is TPasConstructor) then
    begin
      lName := LastIdentifier(lRoutines[i].Decl.Name);
      if (lName <> '') and not MatchesPattern(lName, lPat) then
        EmitNaming(FMetadata, aContext, aCollector,
          lRoutines[i].Decl.SourceLinenumber, lName, lPat);
    end;
end;


{ TRuleConstructorNaming }

procedure TRuleConstructorNaming.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  i: integer;
  lName, lPat: string;
begin
  lPat := aContext.Config.RuleParamStr(FMetadata.RuleId, 'pattern',
    cPatternConstructor);
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
    if lRoutines[i].Decl is TPasConstructor then
    begin
      lName := LastIdentifier(lRoutines[i].Decl.Name);
      if (lName <> '') and not MatchesPattern(lName, lPat) then
        EmitNaming(FMetadata, aContext, aCollector,
          lRoutines[i].Decl.SourceLinenumber, lName, lPat);
    end;
end;


{ TRuleUnitNaming }

procedure TRuleUnitNaming.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lPat: string;
begin
  if aContext.Module = nil then
    Exit;
  if (aContext.Module is TPasProgram) or (aContext.Module is TPasLibrary) then
    Exit;
  lPat := aContext.Config.RuleParamStr(FMetadata.RuleId, 'pattern',
    cPatternUnit);
  if (aContext.Module.Name <> '')
    and not MatchesPattern(aContext.Module.Name, lPat) then
    EmitNaming(FMetadata, aContext, aCollector, aContext.Module, lPat);
end;


{ TRuleIdentifierTooShort }

procedure TRuleIdentifierTooShort.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  lVals: TPasValueDeclArray;
  lRoutines: TAstRoutineArray;
  i, lMin: integer;

// Emits a too-short finding for aName at aLine unless aName is allowlisted.
  procedure CheckIdent(const aName: string; aLine: integer);
  begin
    if (aName <> '') and (Length(aName) < lMin)
      and not IsShortNameAllowed(aName) then
      EmitNaming(FMetadata, aContext, aCollector, aLine, aName,
        IntToStr(lMin));
  end;

begin
  if aContext.Module = nil then
    Exit;
  lMin := aContext.Config.RuleParamInt(FMetadata.RuleId, 'minLength',
    cMinIdentLength);
  lTypes := EnumerateTypes(aContext.Module);
  for i := 0 to High(lTypes) do
    CheckIdent(lTypes[i].Name, lTypes[i].SourceLinenumber);
  lVals := EnumerateValueDecls(aContext.Module);
  for i := 0 to High(lVals) do
    CheckIdent(lVals[i].Decl.Name, lVals[i].Decl.SourceLinenumber);
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
    CheckIdent(LastIdentifier(lRoutines[i].Decl.Name),
      lRoutines[i].Decl.SourceLinenumber);
  CheckIdent(aContext.Module.Name, aContext.Module.SourceLinenumber);
end;


// Builds an rtAst naming-rule metadata declaring its single tunable 'pattern'
// regex param. Keeps the registration block flat — no schema DSL.
function NamingPatternMeta(const aRuleId, aMessageKey,
  aDefaultPattern: string): TRuleMetadata;
begin
  Result := TRuleMetadata.Make(aRuleId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, aMessageKey);
  Result.AddParam('pattern', rpkRegex, aDefaultPattern);
end;


// Builds the IdentifierTooShort metadata declaring its integer 'minLength' param.
function IdentTooShortMeta: TRuleMetadata;
begin
  Result := TRuleMetadata.Make('IdentifierTooShort', rtAst, rfAst, sevMinor,
    itCodeSmell, cfHigh, True, cKeyIdentifierTooShort);
  Result.AddParam('minLength', rpkInt, cMinIdentLength);
end;


initialization
  RegisterRule(TRuleClassNaming.Create(NamingPatternMeta('ClassNaming',
    cKeyClassNaming, cPatternClass).WithDescription(
    'Flags class type names that do not match the configured regex '
    + '(default: a T-prefixed PascalCase name, e.g. TMyClass).')));
  RegisterMessage(cKeyClassNaming, SClassNaming);

  RegisterRule(TRuleRecordNaming.Create(NamingPatternMeta('RecordNaming',
    cKeyRecordNaming, cPatternRecord).WithDescription(
    'Flags record type names that do not match the configured regex '
    + '(default: a T-prefixed PascalCase name, e.g. TMyRecord).')));
  RegisterMessage(cKeyRecordNaming, SRecordNaming);

  RegisterRule(TRuleInterfaceNaming.Create(NamingPatternMeta('InterfaceNaming',
    cKeyInterfaceNaming, cPatternInterface).WithDescription(
    'Flags interface type names that do not match the configured regex '
    + '(default: an I-prefixed name, e.g. IMyInterface).')));
  RegisterMessage(cKeyInterfaceNaming, SInterfaceNaming);

  RegisterRule(TRuleEnumNaming.Create(NamingPatternMeta('EnumNaming',
    cKeyEnumNaming, cPatternEnum).WithDescription(
    'Flags enumeration type names that do not match the configured regex '
    + '(default: a T-prefixed name).')));
  RegisterMessage(cKeyEnumNaming, SEnumNaming);

  RegisterRule(TRuleHelperNaming.Create(NamingPatternMeta('HelperNaming',
    cKeyHelperNaming, cPatternHelper).WithDescription(
    'Flags class/record helper type names that do not match the configured '
    + 'regex (default: T-prefixed with a Helper suffix).')));
  RegisterMessage(cKeyHelperNaming, SHelperNaming);

  RegisterRule(TRulePointerNaming.Create(NamingPatternMeta('PointerNaming',
    cKeyPointerNaming, cPatternPointer).WithDescription(
    'Flags pointer type names that do not match the configured regex '
    + '(default: a P-prefixed name, e.g. PMyRecord).')));
  RegisterMessage(cKeyPointerNaming, SPointerNaming);

  RegisterRule(TRuleAttributeNaming.Create(NamingPatternMeta('AttributeNaming',
    cKeyAttributeNaming, cPatternAttribute).WithDescription(
    'Flags custom attribute class names that do not match the configured regex '
    + '(default: an Attribute suffix).')));
  RegisterMessage(cKeyAttributeNaming, SAttributeNaming);

  RegisterRule(TRuleConstantNaming.Create(NamingPatternMeta('ConstantNaming',
    cKeyConstantNaming, cPatternConst).WithDescription(
    'Flags declared constant names that do not match the configured regex.')));
  RegisterMessage(cKeyConstantNaming, SConstantNaming);

  RegisterRule(TRuleFieldNaming.Create(NamingPatternMeta('FieldNaming',
    cKeyFieldNaming, cPatternField).WithDescription(
    'Flags class/record field names that do not match the configured regex '
    + '(default: an F-prefixed name).')));
  RegisterMessage(cKeyFieldNaming, SFieldNaming);

  RegisterRule(TRuleVariableNaming.Create(NamingPatternMeta('VariableNaming',
    cKeyVariableNaming, cPatternVariable).WithDescription(
    'Flags variable names that do not match the configured regex.')));
  RegisterMessage(cKeyVariableNaming, SVariableNaming);

  RegisterRule(TRuleRoutineNaming.Create(NamingPatternMeta('RoutineNaming',
    cKeyRoutineNaming, cPatternRoutine).WithDescription(
    'Flags procedure and function names that do not match the configured '
    + 'regex.')));
  RegisterMessage(cKeyRoutineNaming, SRoutineNaming);

  RegisterRule(TRuleConstructorNaming.Create(NamingPatternMeta(
    'ConstructorNaming', cKeyConstructorNaming, cPatternConstructor)
    .WithDescription('Flags constructor names that do not match the configured '
    + 'regex (default: a Create-prefixed name).')));
  RegisterMessage(cKeyConstructorNaming, SConstructorNaming);

  RegisterRule(TRuleUnitNaming.Create(NamingPatternMeta('UnitNaming',
    cKeyUnitNaming, cPatternUnit).WithDescription(
    'Flags unit names that do not match the configured regex.')));
  RegisterMessage(cKeyUnitNaming, SUnitNaming);

  RegisterRule(TRuleIdentifierTooShort.Create(IdentTooShortMeta.WithDescription(
    'Flags identifiers shorter than the configured minimum length, so names '
    + 'convey intent.')));
  RegisterMessage(cKeyIdentifierTooShort, SIdentifierTooShort);

end.
