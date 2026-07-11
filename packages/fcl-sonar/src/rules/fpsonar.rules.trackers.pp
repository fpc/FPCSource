{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Config-driven disallow-list tracker rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Trackers;

{ Config-driven TRACKER rules:
  disallow-list checks: disallowed identifier / constant / enum / field /
  property / routine / type / import-by-path, tracked type aliases }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  Pascal.Tree,
{$ELSE}
  PasTree,
{$ENDIF}
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Config, FpSonar.Resolver,
  FpSonar.Traversal, FpSonar.Rules.Consts;

type
  { reports a used unit whose resolved source path matches a disallow-glob. }
  TRuleDisallowedImportByPath = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a reference resolving to a disallowed constant. }
  TRuleDisallowedConstant = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a reference resolving to a disallowed enum value. }
  TRuleDisallowedEnumValue = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a reference resolving to a disallowed class/record/object field. }
  TRuleDisallowedField = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a disallowed identifier; resolved-by-name or spelling-by-name mode. }
  TRuleDisallowedIdentifier = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a reference resolving to a disallowed property. }
  TRuleDisallowedProperty = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a reference resolving to a disallowed routine }
  TRuleDisallowedRoutine = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports an expression-position reference resolving to a disallowed type. }
  TRuleDisallowedType = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a type-alias DECLARATION matching a target.
    Reads the AST only, does not consults the resolver. }
  TRuleTypeAliases = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

const
  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyDisallowedImportByPath = 'rule.DisallowedImportByPath.message';
  cKeyDisallowedConstant = 'rule.DisallowedConstant.message';
  cKeyDisallowedEnumValue = 'rule.DisallowedEnumValue.message';
  cKeyDisallowedField = 'rule.DisallowedField.message';
  cKeyDisallowedIdentifier = 'rule.DisallowedIdentifier.message';
  cKeyDisallowedProperty = 'rule.DisallowedProperty.message';
  cKeyDisallowedRoutine = 'rule.DisallowedRoutine.message';
  cKeyDisallowedType = 'rule.DisallowedType.message';
  cKeyTrackTypeAliases = 'rule.TrackTypeAliases.message';
  cTargetsKey = 'targets';
  cMatchUnresolvedKey = 'matchUnresolvedByName';

// The index of the first target whose Pattern glob-matches any of aCandidates, or -1 when none matches
function FirstMatchingTarget(const aTargets: TFpSonarRuleTargetArray;
  const aCandidates: array of string): integer;
var
  t, c: integer;
begin
  Result := -1;
  for t := 0 to High(aTargets) do
    for c := Low(aCandidates) to High(aCandidates) do
      if GlobMatch(aTargets[t].Pattern, aCandidates[c]) then
      begin
        Result := t;
        Exit;
      end;
end;


// Emits one issue at aLine, column 1, carrying [aSymbol, message].
procedure EmitTargetLine(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aLine: integer;
  const aSymbol: string; const aTarget: TFpSonarRuleTarget);
var
  lSev: TFpSonarSeverity;
begin
  if aTarget.HasSeverity then
    lSev := aTarget.Severity
  else
    lSev := aMeta.Severity;
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aLine, 1, aLine, 1,
    lSev, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    [aSymbol, aTarget.Message], aSymbol);
end;


// Emits one issue at aNode's resolved row, column 1,
procedure EmitTarget(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aNode: TPasElement;
  const aSymbol: string; const aTarget: TFpSonarRuleTarget);
begin
  EmitTargetLine(aMeta, aContext, aCollector,
    aContext.Resolver.SourceRow(aNode), aSymbol, aTarget);
end;


// The owning unit name of aDecl ('' when undeterminable)
function ModuleNameOf(aDecl: TPasElement): string;
var
  lMod: TPasModule;
begin
  Result := '';
  lMod := aDecl.GetModule;
  if lMod <> nil then
    Result := lMod.Name;
end;


// The owning class/record/object type name of aDecl ('' when its Parent is not a members type)
function OwnerTypeNameOf(aDecl: TPasElement): string;
begin
  Result := '';
  if (aDecl.Parent <> nil) and (aDecl.Parent is TPasMembersType) then
    Result := aDecl.Parent.Name;
end;


{ TRuleDisallowedImportByPath }

procedure TRuleDisallowedImportByPath.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTargets: TFpSonarRuleTargetArray;
  lNodes: TPasElementArray;
  lPaths: TFpSonarStringArray;
  i, t: integer;
begin
  lTargets := aContext.Config.RuleParamTargets(FMetadata.RuleId, cTargetsKey);
  if Length(lTargets) = 0 then
    Exit;
  if (aContext.Resolver = nil) or (not aContext.Resolver.Succeeded) then
    Exit;
  if not aContext.Resolver.TryUsesUnitPaths(lNodes, lPaths) then
    Exit;
  for i := 0 to High(lNodes) do
  begin
    t := FirstMatchingTarget(lTargets, [lPaths[i]]);
    if t >= 0 then
      EmitTarget(FMetadata, aContext, aCollector, lNodes[i],
        TPasUsesUnit(lNodes[i]).Name, lTargets[t]);
  end;
end;


{ TRuleDisallowedConstant }

procedure TRuleDisallowedConstant.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTargets: TFpSonarRuleTargetArray;
  lNodes, lDecls: TPasElementArray;
  lName, lModName: string;
  i, t: integer;
begin
  lTargets := aContext.Config.RuleParamTargets(FMetadata.RuleId, cTargetsKey);
  if Length(lTargets) = 0 then
    Exit;
  if (aContext.Resolver = nil) or (not aContext.Resolver.Succeeded) then
    Exit;
  if not aContext.Resolver.TryReferenceSites(lNodes, lDecls) then
    Exit;
  for i := 0 to High(lDecls) do
  begin
    if not (lDecls[i] is TPasConst) then
      Continue;
    lName := lDecls[i].Name;
    if lName = '' then
      Continue;
    lModName := ModuleNameOf(lDecls[i]);
    if lModName <> '' then
      t := FirstMatchingTarget(lTargets, [lName, lModName + '.' + lName])
    else
      t := FirstMatchingTarget(lTargets, [lName]);
    if t >= 0 then
      EmitTarget(FMetadata, aContext, aCollector, lNodes[i], lName,
        lTargets[t]);
  end;
end;


{ TRuleDisallowedEnumValue }

procedure TRuleDisallowedEnumValue.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTargets: TFpSonarRuleTargetArray;
  lNodes, lDecls: TPasElementArray;
  lName, lModName, lEnumName: string;
  i, t: integer;
begin
  lTargets := aContext.Config.RuleParamTargets(FMetadata.RuleId, cTargetsKey);
  if Length(lTargets) = 0 then
    Exit;
  if (aContext.Resolver = nil) or (not aContext.Resolver.Succeeded) then
    Exit;
  if not aContext.Resolver.TryReferenceSites(lNodes, lDecls) then
    Exit;
  for i := 0 to High(lDecls) do
  begin
    if not (lDecls[i] is TPasEnumValue) then
      Continue;
    lName := lDecls[i].Name;
    if lName = '' then
      Continue;
    lModName := ModuleNameOf(lDecls[i]);
    // The owning enum type (the value's Parent) gives the EnumType.Value form;
    lEnumName := '';
    if (lDecls[i].Parent <> nil) and (lDecls[i].Parent is TPasEnumType) then
      lEnumName := lDecls[i].Parent.Name;
    if (lModName <> '') and (lEnumName <> '') then
      t := FirstMatchingTarget(lTargets,
        [lName, lModName + '.' + lName, lEnumName + '.' + lName])
    else if lEnumName <> '' then
      t := FirstMatchingTarget(lTargets, [lName, lEnumName + '.' + lName])
    else if lModName <> '' then
      t := FirstMatchingTarget(lTargets, [lName, lModName + '.' + lName])
    else
      t := FirstMatchingTarget(lTargets, [lName]);
    if t >= 0 then
      EmitTarget(FMetadata, aContext, aCollector, lNodes[i], lName,
        lTargets[t]);
  end;
end;


// The candidate match forms for a member symbol: the bare Name, the Unit-qualified form, and the OwnerType-qualified
function MemberCandidates(aDecl: TPasElement;
  const aName: string): TFpSonarStringArray;
var
  lMod, lOwner: string;
begin
  SetLength(Result, 1);
  Result[0] := aName;
  lMod := ModuleNameOf(aDecl);
  if lMod <> '' then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := lMod + '.' + aName;
  end;
  lOwner := OwnerTypeNameOf(aDecl);
  if lOwner <> '' then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := lOwner + '.' + aName;
  end;
end;


{ TRuleDisallowedField }

procedure TRuleDisallowedField.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTargets: TFpSonarRuleTargetArray;
  lNodes, lDecls: TPasElementArray;
  lName: string;
  i, t: integer;
begin
  lTargets := aContext.Config.RuleParamTargets(FMetadata.RuleId, cTargetsKey);
  if Length(lTargets) = 0 then
    Exit;
  if (aContext.Resolver = nil) or (not aContext.Resolver.Succeeded) then
    Exit;
  if not aContext.Resolver.TryReferenceSites(lNodes, lDecls) then
    Exit;
  for i := 0 to High(lDecls) do
  begin
    // A field is a member TPasVariable that is neither a const nor a property
    if not (lDecls[i] is TPasVariable) then
      Continue;
    if (lDecls[i] is TPasConst) or (lDecls[i] is TPasProperty) then
      Continue;
    if not (lDecls[i].Parent is TPasMembersType) then
      Continue;
    lName := lDecls[i].Name;
    if lName = '' then
      Continue;
    t := FirstMatchingTarget(lTargets, MemberCandidates(lDecls[i], lName));
    if t >= 0 then
      EmitTarget(FMetadata, aContext, aCollector, lNodes[i], lName,
        lTargets[t]);
  end;
end;


{ TRuleDisallowedProperty }

procedure TRuleDisallowedProperty.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTargets: TFpSonarRuleTargetArray;
  lNodes, lDecls: TPasElementArray;
  lName: string;
  i, t: integer;
begin
  lTargets := aContext.Config.RuleParamTargets(FMetadata.RuleId, cTargetsKey);
  if Length(lTargets) = 0 then
    Exit;
  if (aContext.Resolver = nil) or (not aContext.Resolver.Succeeded) then
    Exit;
  if not aContext.Resolver.TryReferenceSites(lNodes, lDecls) then
    Exit;
  for i := 0 to High(lDecls) do
  begin
    if not (lDecls[i] is TPasProperty) then
      Continue;
    lName := lDecls[i].Name;
    if lName = '' then
      Continue;
    t := FirstMatchingTarget(lTargets, MemberCandidates(lDecls[i], lName));
    if t >= 0 then
      EmitTarget(FMetadata, aContext, aCollector, lNodes[i], lName,
        lTargets[t]);
  end;
end;


{ TRuleDisallowedIdentifier }

procedure TRuleDisallowedIdentifier.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTargets: TFpSonarRuleTargetArray;
  lNodes, lDecls: TPasElementArray;
  lNames: TFpSonarStringArray;
  lName, lModName: string;
  lByName: boolean;
  i, t: integer;
begin
  lTargets := aContext.Config.RuleParamTargets(FMetadata.RuleId, cTargetsKey);
  if Length(lTargets) = 0 then
    Exit;
  if (aContext.Resolver = nil) or (not aContext.Resolver.Succeeded) then
    Exit;
  lByName := aContext.Config.RuleParamBool(FMetadata.RuleId,
    cMatchUnresolvedKey, False);
  if lByName then
  begin
    // Name mode: match every identifier leaf by its raw spelling
    if not aContext.Resolver.TryIdentifierNameSites(lNodes, lNames) then
      Exit;
    for i := 0 to High(lNames) do
    begin
      lName := lNames[i];
      if lName = '' then
        Continue;
      t := FirstMatchingTarget(lTargets, [lName]);
      if t >= 0 then
        EmitTarget(FMetadata, aContext, aCollector, lNodes[i], lName,
          lTargets[t]);
    end;
  end
  else
  begin
    // Resolved mode: match ANY resolved declaration (no class filter) by its
    // bare or Unit-qualified name.
    if not aContext.Resolver.TryReferenceSites(lNodes, lDecls) then
      Exit;
    for i := 0 to High(lDecls) do
    begin
      lName := lDecls[i].Name;
      if lName = '' then
        Continue;
      lModName := ModuleNameOf(lDecls[i]);
      if lModName <> '' then
        t := FirstMatchingTarget(lTargets, [lName, lModName + '.' + lName])
      else
        t := FirstMatchingTarget(lTargets, [lName]);
      if t >= 0 then
        EmitTarget(FMetadata, aContext, aCollector, lNodes[i], lName,
          lTargets[t]);
    end;
  end;
end;


{ TRuleDisallowedRoutine }

procedure TRuleDisallowedRoutine.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTargets: TFpSonarRuleTargetArray;
  lNodes, lDecls: TPasElementArray;
  lName: string;
  i, t: integer;
begin
  lTargets := aContext.Config.RuleParamTargets(FMetadata.RuleId, cTargetsKey);
  if Length(lTargets) = 0 then
    Exit;
  if (aContext.Resolver = nil) or (not aContext.Resolver.Succeeded) then
    Exit;
  if not aContext.Resolver.TryReferenceSites(lNodes, lDecls) then
    Exit;
  for i := 0 to High(lDecls) do
  begin
      { Every routine kind (function, method, ctor/dtor, operator) descends
        TPasProcedure; a method also gets the OwnerType-qualified candidate, a
        free routine degrades to bare/Unit }
    if not (lDecls[i] is TPasProcedure) then
      Continue;
    lName := lDecls[i].Name;
    if lName = '' then
      Continue;
    t := FirstMatchingTarget(lTargets, MemberCandidates(lDecls[i], lName));
    if t >= 0 then
      EmitTarget(FMetadata, aContext, aCollector, lNodes[i], lName,
        lTargets[t]);
  end;
end;


{ TRuleDisallowedType }

procedure TRuleDisallowedType.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTargets: TFpSonarRuleTargetArray;
  lNodes, lDecls: TPasElementArray;
  lName, lModName: string;
  i, t: integer;
begin
  lTargets := aContext.Config.RuleParamTargets(FMetadata.RuleId, cTargetsKey);
  if Length(lTargets) = 0 then
    Exit;
  if (aContext.Resolver = nil) or (not aContext.Resolver.Succeeded) then
    Exit;
  if not aContext.Resolver.TryReferenceSites(lNodes, lDecls) then
    Exit;
  for i := 0 to High(lDecls) do
  begin
      { A reference in expression position ((typecast / class-ref / is-as operand)
        that resolves to a type declaration }
    if not (lDecls[i] is TPasType) then
      Continue;
    lName := lDecls[i].Name;
    if lName = '' then
      Continue;
    lModName := ModuleNameOf(lDecls[i]);
    if lModName <> '' then
      t := FirstMatchingTarget(lTargets, [lName, lModName + '.' + lName])
    else
      t := FirstMatchingTarget(lTargets, [lName]);
    if t >= 0 then
      EmitTarget(FMetadata, aContext, aCollector, lNodes[i], lName,
        lTargets[t]);
  end;
end;


{ TRuleTypeAliases }

procedure TRuleTypeAliases.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTargets: TFpSonarRuleTargetArray;
  lTypes: TPasTypeArray;
  lName, lModName: string;
  i, t: integer;
begin
  // AST-tier: read declarations directly, emit at each alias's own line.
  lTargets := aContext.Config.RuleParamTargets(FMetadata.RuleId, cTargetsKey);
  if Length(lTargets) = 0 then
    Exit;
  if aContext.Module = nil then
    Exit;
  lTypes := EnumerateTypes(aContext.Module);
  for i := 0 to High(lTypes) do
  begin
    // TPasAliasType subsumes the strong-alias TPasTypeAliasType (type T = type X);
    if not (lTypes[i] is TPasAliasType) then
      Continue;
    lName := lTypes[i].Name;
    if lName = '' then
      Continue;
    lModName := ModuleNameOf(lTypes[i]);
    if lModName <> '' then
      t := FirstMatchingTarget(lTargets, [lName, lModName + '.' + lName])
    else
      t := FirstMatchingTarget(lTargets, [lName]);
    if t >= 0 then
      EmitTargetLine(FMetadata, aContext, aCollector,
        lTypes[i].SourceLinenumber, lName, lTargets[t]);
  end;
end;


// Builds a tracker rule's metadata and declares its single 'targets' param
function TrackerMeta(const aRuleId, aMessageKey: string): TRuleMetadata;
begin
  Result := TRuleMetadata.Make(aRuleId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfHigh, True, aMessageKey);
  Result.AddParam(cTargetsKey, rpkTargets);
end;


function IdentifierMeta: TRuleMetadata;
begin
  Result := TRuleMetadata.Make('DisallowedIdentifier', rtSem, rfResolver, sevMajor,
    itCodeSmell, cfHigh, True, cKeyDisallowedIdentifier);
  Result.AddParam(cTargetsKey, rpkTargets);
  Result.AddParam(cMatchUnresolvedKey, rpkBool, False);
  Result.Description :=
    'Reports use of identifiers matching a configured disallow-list (targets).';
end;


// TrackTypeAliases's metadata
function AstTrackerMeta(const aRuleId, aMessageKey: string): TRuleMetadata;
begin
  Result := TRuleMetadata.Make(aRuleId, rtAst, rfAst, sevMajor, itCodeSmell,
    cfHigh, True, aMessageKey);
  Result.AddParam(cTargetsKey, rpkTargets);
end;


initialization
  RegisterRule(TRuleDisallowedImportByPath.Create(
    TrackerMeta('DisallowedImportByPath', cKeyDisallowedImportByPath).WithDescription(
    'Reports use of units whose path matches a configured disallow-list (targets).')));
  RegisterMessage(cKeyDisallowedImportByPath, SDisallowedImportByPath);

  RegisterRule(TRuleDisallowedConstant.Create(
    TrackerMeta('DisallowedConstant', cKeyDisallowedConstant).WithDescription(
    'Reports use of constants matching a configured disallow-list (targets).')));
  RegisterMessage(cKeyDisallowedConstant, SDisallowedConstant);

  RegisterRule(TRuleDisallowedEnumValue.Create(
    TrackerMeta('DisallowedEnumValue', cKeyDisallowedEnumValue).WithDescription(
    'Reports use of enum values matching a configured disallow-list (targets).')));
  RegisterMessage(cKeyDisallowedEnumValue, SDisallowedEnumValue);

  RegisterRule(TRuleDisallowedField.Create(
    TrackerMeta('DisallowedField', cKeyDisallowedField).WithDescription(
    'Reports use of fields matching a configured disallow-list (targets).')));
  RegisterMessage(cKeyDisallowedField, SDisallowedField);

  RegisterRule(TRuleDisallowedIdentifier.Create(IdentifierMeta));
  RegisterMessage(cKeyDisallowedIdentifier, SDisallowedIdentifier);

  RegisterRule(TRuleDisallowedProperty.Create(
    TrackerMeta('DisallowedProperty', cKeyDisallowedProperty).WithDescription(
    'Reports use of properties matching a configured disallow-list (targets).')));
  RegisterMessage(cKeyDisallowedProperty, SDisallowedProperty);

  RegisterRule(TRuleDisallowedRoutine.Create(
    TrackerMeta('DisallowedRoutine', cKeyDisallowedRoutine).WithDescription(
    'Reports calls to routines matching a configured disallow-list (targets).')));
  RegisterMessage(cKeyDisallowedRoutine, SDisallowedRoutine);

  RegisterRule(TRuleDisallowedType.Create(
    TrackerMeta('DisallowedType', cKeyDisallowedType).WithDescription(
    'Reports use of types matching a configured disallow-list (targets).')));
  RegisterMessage(cKeyDisallowedType, SDisallowedType);

  RegisterRule(TRuleTypeAliases.Create(
    AstTrackerMeta('TrackTypeAliases', cKeyTrackTypeAliases).WithDescription(
    'Reports type aliases matching a configured pattern (targets).')));
  RegisterMessage(cKeyTrackTypeAliases, STrackTypeAliases);

end.
