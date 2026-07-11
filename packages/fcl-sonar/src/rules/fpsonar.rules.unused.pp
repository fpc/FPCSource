{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    USE-tier unused-declaration analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Unused;

{ USE-tier rules (rtUse):
  unused-declaration checks whether a declaration is referenced within its
  accessibility scope. }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, Pascal.Tree,
{$ELSE}
  Classes, PasTree,
{$ENDIF}
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Traversal,
  FpSonar.Config, FpSonar.Rules.Consts;

type
  { reports a routine-local variable not referenced in its routine body. }
  TRuleRemoveUnusedLocalVariable = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a class/record private field not referenced in its unit. }
  TRuleRemoveUnusedField = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a class private property not referenced in its unit. }
  TRuleRemoveUnusedProperty = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a class-private OR implementation-section const not referenced
    in its unit. }
  TRuleRemoveUnusedConstant = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a private method not referenced in its unit. }
  TRuleRemoveUnusedRoutine = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a private nested type OR an implementation-section type not
    referenced in its unit; also reports an interface-section
    type not referenced anywhere in the project. }
  TRuleRemoveUnusedType = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a `uses`-clause entry whose imported unit's interface declares no
    name the importing unit references. }
  TRuleRemoveUnusedImports = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a unit-level global var not referenced anywhere in the project. }
  TRuleRemoveUnusedGlobalVariable = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

const
  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyUnusedLocal = 'rule.RemoveUnusedLocalVariable.message';
  cKeyUnusedField = 'rule.RemoveUnusedField.message';
  cKeyUnusedProperty = 'rule.RemoveUnusedProperty.message';
  cKeyUnusedConstant = 'rule.RemoveUnusedConstant.message';
  cKeyUnusedRoutine = 'rule.RemoveUnusedRoutine.message';
  cKeyUnusedType = 'rule.RemoveUnusedType.message';
  cKeyUnusedImport = 'rule.RemoveUnusedImports.message';
  cKeyUnusedGlobal = 'rule.RemoveUnusedGlobalVariable.message';
  cKeyUnusedPublicRoutine = 'rule.RemoveUnusedRoutine.publicMessage';
  cKeyUnusedPublicType = 'rule.RemoveUnusedType.publicMessage';
  cParamFlagOperatorOnly = 'flagOperatorOnlyImports';
  cParamFlagSideEffect = 'flagSideEffectImports';

// True iff aEl has private (or strict-private) visibility
function IsPrivateMember(aEl: TPasElement): boolean;
begin
  Result := aEl.Visibility in [visPrivate, visStrictPrivate];
end;


// True iff aEl is a direct member of a class/record
function IsClassMember(aEl: TPasElement): boolean;
begin
  Result := (aEl.Parent <> nil) and (aEl.Parent is TPasMembersType);
end;


// True iff aEl lives in aModule's implementation section
function IsInImplementation(aEl: TPasElement; aModule: TPasModule): boolean;
var
  lEl: TPasElement;
begin
  Result := False;
  if (aModule = nil) or (aModule.ImplementationSection = nil) then
    Exit;
  lEl := aEl;
  while lEl <> nil do
  begin
    if lEl = aModule.ImplementationSection then
    begin
      Result := True;
      Exit;
    end;
    lEl := lEl.Parent;
  end;
end;


{ True iff a unit-scope const/type candidate qualifies: a private member OR an
  implementation-section (top-level) declaration. }
function IsUnitScopeCandidate(aEl: TPasElement; aModule: TPasModule): boolean;
begin
  if IsClassMember(aEl) then
    Result := IsPrivateMember(aEl)
  else
    Result := IsInImplementation(aEl, aModule);
end;


{ True iff aProc is a private method safe to consider for "unused": a plain
  procedure/function member with no modifier that allows a non-textual call site.}
function IsFlaggablePrivateMethod(aProc: TPasProcedure): boolean;
begin
  Result := IsPrivateMember(aProc)
    and not (aProc is TPasConstructor)
    and not (aProc is TPasDestructor)
    and not (aProc is TPasOperator)
    and not aProc.IsVirtual and not aProc.IsDynamic
    and not aProc.IsAbstract and not aProc.IsOverride
    and not aProc.IsMessage and not aProc.IsExternal;
end;


// Emits one issue for aDecl at its declaration line, column 1
procedure EmitUnused(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aDecl: TPasElement;
  const aMessageKey: string = '');
var
  lKey: string;
begin
  lKey := aMessageKey;
  if lKey = '' then
    lKey := aMeta.MessageKey;
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aDecl.SourceLinenumber, 1,
    aDecl.SourceLinenumber, 1, aMeta.Severity, aMeta.Category,
    aMeta.DefaultConfidence, lKey, [aDecl.Name], aDecl.Name);
end;


{ Creates a per-Apply analyzer over the context's module with the project-wide
  index attached, so a usProject query can be answered. }
function MakeAnalyzer(
  const aContext: TRuleContext): TFpSonarUseAnalyzer;
begin
  Result := MakeUseAnalyzer(aContext.Module, aContext.Resolver,
    aContext.Config.UseTierResolution = utrPrefer);
  Result.ProjectIndex := aContext.ProjectIndex;
end;


// The interface section's top-level declaration list or nil when the module has no interface section.
function InterfaceDecls(aModule: TPasModule): TFPList;
begin
  if (aModule <> nil) and (aModule.InterfaceSection <> nil) then
    Result := aModule.InterfaceSection.Declarations
  else
    Result := nil;
end;


// True iff aType is a forward class declaration
function IsForwardClass(aType: TPasElement): boolean;
begin
  Result := (aType is TPasClassType) and TPasClassType(aType).IsForward;
end;


{ True iff aVar has external linkage (cvar / external / public / export): it can
  be referenced from outside the analyzed sources. }
function HasGlobalExternalLinkage(aVar: TPasVariable): boolean;
begin
  Result := aVar.VarModifiers * [vmCVar, vmExternal, vmPublic, vmExport] <> [];
end;


// True iff aProc has external linkage (external / export / public): it binds outside the analyzed sources.
function HasRoutineExternalLinkage(aProc: TPasProcedure): boolean;
begin
  Result := aProc.IsExternal or aProc.IsExported or (pmPublic in aProc.Modifiers);
end;


{ TRuleRemoveUnusedLocalVariable }

procedure TRuleRemoveUnusedLocalVariable.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalyzer: TFpSonarUseAnalyzer;
  lRoutines: TAstRoutineArray;
  lDecls: TFPList;
  lObj: TObject;
  i, j: integer;
begin
  lAnalyzer := MakeAnalyzer(aContext);
  try
    lRoutines := EnumerateRoutines(aContext.Module);
    for i := 0 to High(lRoutines) do
    begin
      // A body-bearing routine's locals live in its TProcedureBody declaration list
      lDecls := lRoutines[i].Decl.Body.Declarations;
      for j := 0 to lDecls.Count - 1 do
      begin
        lObj := TObject(lDecls[j]);
        // Plain local vars only
        if (lObj is TPasVariable) and not (lObj is TPasConst)
          and not (lObj is TPasProperty) and (TPasVariable(lObj).Name <> '')
          and (lAnalyzer.IsReferenced(TPasElement(lObj), usRoutine) = rrUnused) then
          EmitUnused(FMetadata, aContext, aCollector, TPasElement(lObj));
      end;
    end;
  finally
    lAnalyzer.Free;
  end;
end;


{ TRuleRemoveUnusedField }

procedure TRuleRemoveUnusedField.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalyzer: TFpSonarUseAnalyzer;
  lVals: TPasValueDeclArray;
  i: integer;
begin
  lAnalyzer := MakeAnalyzer(aContext);
  try
    lVals := EnumerateValueDecls(aContext.Module);
    for i := 0 to High(lVals) do
      if (lVals[i].Kind = vkField) and (lVals[i].Decl.Name <> '')
        and IsPrivateMember(lVals[i].Decl)
        and (lAnalyzer.IsReferenced(lVals[i].Decl, usUnit) = rrUnused) then
        EmitUnused(FMetadata, aContext, aCollector, lVals[i].Decl);
  finally
    lAnalyzer.Free;
  end;
end;


{ TRuleRemoveUnusedConstant }

procedure TRuleRemoveUnusedConstant.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalyzer: TFpSonarUseAnalyzer;
  lVals: TPasValueDeclArray;
  i: integer;
begin
  lAnalyzer := MakeAnalyzer(aContext);
  try
    lVals := EnumerateValueDecls(aContext.Module);
    for i := 0 to High(lVals) do
      if (lVals[i].Kind = vkConst) and (lVals[i].Decl.Name <> '')
        and IsUnitScopeCandidate(lVals[i].Decl, aContext.Module)
        and (lAnalyzer.IsReferenced(lVals[i].Decl, usUnit) = rrUnused) then
        EmitUnused(FMetadata, aContext, aCollector, lVals[i].Decl);
  finally
    lAnalyzer.Free;
  end;
end;


{ shared class-member enumeration (properties / methods) }

// Every direct member of every class/record type in aModule
function AllClassMembers(aModule: TPasModule): TPasElementArray;
var
  lTypes: TPasTypeArray;
  lMembers: TFPList;
  i, j: integer;
begin
  SetLength(Result, 0);
  lTypes := EnumerateTypes(aModule);
  for i := 0 to High(lTypes) do
    if lTypes[i] is TPasMembersType then
    begin
      lMembers := TPasMembersType(lTypes[i]).Members;
      for j := 0 to lMembers.Count - 1 do
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := TPasElement(lMembers[j]);
      end;
    end;
end;


{ TRuleRemoveUnusedProperty }

procedure TRuleRemoveUnusedProperty.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalyzer: TFpSonarUseAnalyzer;
  lMembers: TPasElementArray;
  lProp: TPasProperty;
  i: integer;
begin
  lAnalyzer := MakeAnalyzer(aContext);
  try
    lMembers := AllClassMembers(aContext.Module);
    for i := 0 to High(lMembers) do
    begin
      if not (lMembers[i] is TPasProperty) then
        Continue;
      lProp := TPasProperty(lMembers[i]);
      // Default + indexed (array) properties are excluded
      if (lProp.Name = '') or lProp.IsDefault or (lProp.Args.Count > 0)
        or not IsPrivateMember(lProp) then
        Continue;
      if lAnalyzer.IsReferenced(lProp, usUnit) = rrUnused then
        EmitUnused(FMetadata, aContext, aCollector, lProp);
    end;
  finally
    lAnalyzer.Free;
  end;
end;


{ TRuleRemoveUnusedRoutine }

procedure TRuleRemoveUnusedRoutine.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalyzer: TFpSonarUseAnalyzer;
  lMembers: TPasElementArray;
  lDecls: TFPList;
  lProc: TPasProcedure;
  i: integer;
begin
  lAnalyzer := MakeAnalyzer(aContext);
  try
    // Private class methods — referenceable only within the unit (usUnit).
    lMembers := AllClassMembers(aContext.Module);
    for i := 0 to High(lMembers) do
      if (lMembers[i] is TPasProcedure) and (lMembers[i].Name <> '')
        and IsFlaggablePrivateMethod(TPasProcedure(lMembers[i]))
        and (lAnalyzer.IsReferenced(lMembers[i], usUnit) = rrUnused) then
        EmitUnused(FMetadata, aContext, aCollector, lMembers[i]);

    // Interface-section (public) top-level routines
    lDecls := InterfaceDecls(aContext.Module);
    if lDecls <> nil then
      for i := 0 to lDecls.Count - 1 do
        if (TObject(lDecls[i]) is TPasProcedure)
          and not (TObject(lDecls[i]) is TPasOperator) then
        begin
          lProc := TPasProcedure(lDecls[i]);
          if (lProc.Name <> '') and not HasRoutineExternalLinkage(lProc)
            and (lAnalyzer.IsReferenced(lProc, usProject) = rrUnused) then
            EmitUnused(FMetadata, aContext, aCollector, lProc,
              cKeyUnusedPublicRoutine);
        end;
  finally
    lAnalyzer.Free;
  end;
end;


{ TRuleRemoveUnusedType }

procedure TRuleRemoveUnusedType.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalyzer: TFpSonarUseAnalyzer;
  lTypes: TPasTypeArray;
  lDecls: TFPList;
  lType: TPasType;
  i: integer;
begin
  lAnalyzer := MakeAnalyzer(aContext);
  try
    // Private nested / implementation-section types — unit scope (usUnit).
    lTypes := EnumerateTypes(aContext.Module);
    for i := 0 to High(lTypes) do
      if (lTypes[i].Name <> '')
        and IsUnitScopeCandidate(lTypes[i], aContext.Module)
        and (lAnalyzer.IsReferenced(lTypes[i], usUnit) = rrUnused) then
        EmitUnused(FMetadata, aContext, aCollector, lTypes[i]);

    // Interface-section (public) top-level types
    lDecls := InterfaceDecls(aContext.Module);
    if lDecls <> nil then
      for i := 0 to lDecls.Count - 1 do
        if (TObject(lDecls[i]) is TPasType) and not
          IsForwardClass(TPasElement(lDecls[i])) then
        begin
          lType := TPasType(lDecls[i]);
          if (lType.Name <> '')
            and (lAnalyzer.IsReferenced(lType, usProject) = rrUnused) then
            EmitUnused(FMetadata, aContext, aCollector, lType,
              cKeyUnusedPublicType);
        end;
  finally
    lAnalyzer.Free;
  end;
end;


{ TRuleRemoveUnusedImports }

procedure TRuleRemoveUnusedImports.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalyzer: TFpSonarUseAnalyzer;
  lFlagOperatorOnly, lFlagSideEffect: boolean;

  { Tests every uses entry in aClause: an import is unnecessary iff the importer
    references none of the imported unit's exported names — unless that unit is
    imported for an operator/helper or an init/final side effect }
  procedure CheckUses(const aClause: TPasUsesClause);
  var
    i: integer;
    lUses: TPasUsesUnit;
    lName: string;
    lIfaceNames: TStringList;
  begin
    for i := 0 to High(aClause) do
    begin
      lUses := aClause[i];
      lName := lUses.Name;
      if lName = '' then
        Continue;
      // The imported unit must be a locatable project target whose interface we parsed
      lIfaceNames := aContext.ProjectIndex.InterfaceNames(lName);
      if lIfaceNames = nil then
        Continue;
      // The importer references at least one exported name => the import is used.
      if lAnalyzer.ReferencesAny(lIfaceNames) then
        Continue;
      if aContext.ProjectIndex.UnitHasOperatorOrHelper(lName)
        and not lFlagOperatorOnly then
        Continue;
      if aContext.ProjectIndex.UnitHasInitFinal(lName)
        and not lFlagSideEffect then
        Continue;
      // Flag the unnecessary import at its uses-clause entry, column 1.
      aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
        lUses.SourceLinenumber, 1, lUses.SourceLinenumber, 1,
        FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
        FMetadata.MessageKey, [lName], lName);
    end;
  end;

begin
  // No project context => we cannot resolve imported-unit interfaces
  if (aContext.ProjectIndex = nil) or (aContext.Module = nil) then
    Exit;
  lFlagOperatorOnly := aContext.Config.RuleParamBool(FMetadata.RuleId,
    cParamFlagOperatorOnly, False);
  lFlagSideEffect := aContext.Config.RuleParamBool(FMetadata.RuleId,
    cParamFlagSideEffect, False);
  lAnalyzer := MakeAnalyzer(aContext);
  try
    if aContext.Module.InterfaceSection <> nil then
      CheckUses(aContext.Module.InterfaceSection.UsesClause);
    if aContext.Module.ImplementationSection <> nil then
      CheckUses(aContext.Module.ImplementationSection.UsesClause);
  finally
    lAnalyzer.Free;
  end;
end;


{ TRuleRemoveUnusedGlobalVariable }

procedure TRuleRemoveUnusedGlobalVariable.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalyzer: TFpSonarUseAnalyzer;
  lVals: TPasValueDeclArray;
  i: integer;
begin
  lAnalyzer := MakeAnalyzer(aContext);
  try
    // Module-levelvars; vkVar already excludes class fields (vkField) and routine locals.
    lVals := EnumerateValueDecls(aContext.Module);
    for i := 0 to High(lVals) do
      if (lVals[i].Kind = vkVar) and (lVals[i].Decl.Name <> '')
        and not HasGlobalExternalLinkage(lVals[i].Decl)
        and (lAnalyzer.IsReferenced(lVals[i].Decl, usProject) = rrUnused) then
        EmitUnused(FMetadata, aContext, aCollector, lVals[i].Decl);
  finally
    lAnalyzer.Free;
  end;
end;


// Builds an rtUse / rfAst metadata for an unused-declaration rule
function UnusedMeta(const aRuleId, aMessageKey: string): TRuleMetadata;
begin
  Result := TRuleMetadata.Make(aRuleId, rtUse, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, aMessageKey);
end;


{ The RemoveUnusedImports metadata }
function UnusedImportsMeta: TRuleMetadata;
begin
  Result := UnusedMeta('RemoveUnusedImports', cKeyUnusedImport);
  Result.AddParam(cParamFlagOperatorOnly, rpkBool, False);
  Result.AddParam(cParamFlagSideEffect, rpkBool, False);
  Result.Description :=
    'Flags a unit in the uses clause that is never used.';
end;


initialization
  RegisterRule(TRuleRemoveUnusedLocalVariable.Create(UnusedMeta(
    'RemoveUnusedLocalVariable', cKeyUnusedLocal).WithDescription(
    'Flags a local variable that is declared but never used.')));
  RegisterMessage(cKeyUnusedLocal, SRemoveUnusedLocalVariable);

  RegisterRule(TRuleRemoveUnusedField.Create(UnusedMeta(
    'RemoveUnusedField', cKeyUnusedField).WithDescription(
    'Flags a private field that is never used.')));
  RegisterMessage(cKeyUnusedField, SRemoveUnusedField);

  RegisterRule(TRuleRemoveUnusedProperty.Create(UnusedMeta(
    'RemoveUnusedProperty', cKeyUnusedProperty).WithDescription(
    'Flags a private property that is never used.')));
  RegisterMessage(cKeyUnusedProperty, SRemoveUnusedProperty);

  RegisterRule(TRuleRemoveUnusedConstant.Create(UnusedMeta(
    'RemoveUnusedConstant', cKeyUnusedConstant).WithDescription(
    'Flags a constant that is never used.')));
  RegisterMessage(cKeyUnusedConstant, SRemoveUnusedConstant);

  RegisterRule(TRuleRemoveUnusedRoutine.Create(UnusedMeta(
    'RemoveUnusedRoutine', cKeyUnusedRoutine).WithDescription(
    'Flags a private method that is never used.')));
  RegisterMessage(cKeyUnusedRoutine, SRemoveUnusedRoutine);
  RegisterMessage(cKeyUnusedPublicRoutine, SRemoveUnusedRoutinePublic);

  RegisterRule(TRuleRemoveUnusedType.Create(UnusedMeta(
    'RemoveUnusedType', cKeyUnusedType).WithDescription(
    'Flags a private type that is never used.')));
  RegisterMessage(cKeyUnusedType, SRemoveUnusedType);
  RegisterMessage(cKeyUnusedPublicType, SRemoveUnusedTypePublic);

  RegisterRule(TRuleRemoveUnusedImports.Create(UnusedImportsMeta));
  RegisterMessage(cKeyUnusedImport, SRemoveUnusedImports);

  RegisterRule(TRuleRemoveUnusedGlobalVariable.Create(UnusedMeta(
    'RemoveUnusedGlobalVariable', cKeyUnusedGlobal).WithDescription(
    'Flags a global variable that is never used.')));
  RegisterMessage(cKeyUnusedGlobal, SRemoveUnusedGlobalVariable);

end.
