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


{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Pascal.Tree,
{$ELSE}
  Classes, SysUtils, PasTree,
{$ENDIF}
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Traversal,
  FpSonar.Config, FpSonar.UseAnalysis, FpSonar.Rules.Consts;

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

  { reports a routine parameter the routine body never reads. }
  { Absence: silent unless the use closure is complete; published/RTTI and
    override-chain signatures are excluded by modifier, an attribute or a
    specialize marks the owner used, initialization is walked, and an
    address-of use is residue (DW-419). }
  TRuleRemoveUnusedParameter = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a value parameter assigned in the routine body and never read. }
  { Absence: silent unless the use closure is complete; same modifier and
    visibility exclusions, same specialize/attribute/initialization handling and
    the same address-of residue as RemoveUnusedParameter. }
  TRuleParameterAssignedButNeverUsed = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports an `on E:` handler variable the handler never uses. }
  { Absence: silent unless the use closure is complete; a handler variable has
    no RTTI, no override chain and no procedural-variable channel, a bare
    `raise` counts as a use, and a handler outside any routine — an
    initialization or finalization section — is skipped. }
  TRuleUnusedExceptionVariable = class(TRuleBase)
  private
    FHandlers: TFPList;
    procedure CollectHandler(aStmt: TPasImplElement);
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a declared label no `goto` in its routine targets. }
  { Absence: silent unless the unit parsed; a label is reachable only by `goto`,
    so the statement walk over the declaring routine is the whole evidence. }
  TRuleUnusedLabel = class(TRuleBase)
  private
    FTargets: TStringList;
    procedure CollectGoto(aStmt: TPasImplElement);
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a generic type parameter the generic body never names. }
  { Absence: silent unless the use closure is complete, and silent for a type
    declaring a specialization, whose arguments the name set never holds; the
    evidence is otherwise unit-wide, so any occurrence counts — in an attribute,
    an RTTI-reachable member or an initialization section alike. }
  TRuleUnusedGenericParameter = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports an interface-section import only the implementation section needs. }
  { Absence: silent without a project index; an interface-section ancestor,
    field type, attribute, specialize or RTTI-reachable member all count as
    interface uses, and an import needed only for its init/final cannot fire. }
  TRuleUnusedUnitInInterface = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a private member exactly one method of its own type references. }
  { Absence: silent without an undegraded project index; published/RTTI members
    are excluded by visibility, override-chain methods by modifier, specialize,
    procedural-variable and initialization references all count as reference
    sites, and an attribute-only reference is residue (DW-420). }
  TRulePrivateMemberOnlyUsedByOneMethod = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a routine local or private field written and never read. }
  { Absence: silent unless the use closure is complete; locals and private
    fields only, so RTTI never applies, a var/out parameter or an address-of
    counts as a read, initialization is walked, and an attribute-only
    reference is residue (DW-420). }
  TRuleWriteOnlyVariable = class(TRuleBase)
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
  cKeyUnusedParameter = 'rule.RemoveUnusedParameter.message';
  cKeyParameterAssigned = 'rule.ParameterAssignedButNeverUsed.message';
  cKeyUnusedExceptVar = 'rule.UnusedExceptionVariable.message';
  cKeyUnusedLabel = 'rule.UnusedLabel.message';
  cKeyUnusedGenericParam = 'rule.UnusedGenericParameter.message';
  cKeyUnusedInterfaceUnit = 'rule.UnusedUnitInInterface.message';
  cKeyOneMethodMember = 'rule.PrivateMemberOnlyUsedByOneMethod.message';
  cKeyWriteOnlyVariable = 'rule.WriteOnlyVariable.message';
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


// Emits one issue naming aName at aRow, column 1
procedure EmitUnusedNamed(const aMeta: TRuleMetadata;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector;
  const aName: string; aRow: integer);
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aRow, 1, aRow, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    [aName], aName);
end;


// The nearest TPasProcedure ancestor of aEl (aEl itself when it is one), or nil
function OwningProcedure(aEl: TPasElement): TPasProcedure;
var
  lEl: TPasElement;
begin
  Result := nil;
  lEl := aEl;
  while lEl <> nil do
  begin
    if lEl is TPasProcedure then
      Exit(TPasProcedure(lEl));
    lEl := lEl.Parent;
  end;
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


type
  { The dual unused-declaration oracle: the precise use analysis answers the
    unit- and routine-scoped queries whenever it ran on a complete use closure,
    the name/resolution engine answers every other query. }
  TDualUseAnalyzer = class
  private
    FFallback: TFpSonarUseAnalyzer;
    FPrecise: TFpSonarUseAnalysis;
    // Structural identity of every precisely-unused declaration, or nil when the precise path is unavailable.
    FUnusedKeys: TStringList;
  public
    // Builds the fallback engine, plus the precise key set when resolution is preferred and the analysis answered.
    constructor Create(const aContext: TRuleContext);
    // Frees the key set, the precise analysis and the fallback engine.
    destructor Destroy; override;
    // rrUnused iff the precise analysis listed aDecl; delegates for usProject and whenever the precise path is unavailable.
    function IsReferenced(aDecl: TPasElement;
      aScope: TFpSonarUseScope): TFpSonarRefResult;
  end;


constructor TDualUseAnalyzer.Create(const aContext: TRuleContext);
var
  lUnused: TPasElementArray;
  lKey: string;
  i: integer;
begin
  inherited Create;
  FFallback := MakeAnalyzer(aContext);
  if aContext.Config.UseTierResolution <> utrPrefer then
    Exit;
  FPrecise := TFpSonarUseAnalysis.Create(aContext.Resolver);
  if not FPrecise.TryUnusedDeclarations(lUnused) then
    Exit;
  FUnusedKeys := TStringList.Create;
  FUnusedKeys.CaseSensitive := False;
  FUnusedKeys.Sorted := True;
  FUnusedKeys.Duplicates := dupIgnore;
  for i := 0 to High(lUnused) do
  begin
    // The resolved tree packs row and column into SourceLinenumber.
    lKey := DeclKey(lUnused[i], aContext.Resolver.SourceRow(lUnused[i]));
    if lKey <> '' then
      FUnusedKeys.Add(lKey);
  end;
end;


destructor TDualUseAnalyzer.Destroy;
begin
  FreeAndNil(FUnusedKeys);
  FreeAndNil(FPrecise);
  FreeAndNil(FFallback);
  inherited Destroy;
end;


function TDualUseAnalyzer.IsReferenced(aDecl: TPasElement;
  aScope: TFpSonarUseScope): TFpSonarRefResult;
var
  lKey: string;
begin
  // usProject is unanswerable from the precise list: a unit's non-private
  // interface declarations are force-marked used.
  if (FUnusedKeys = nil) or (aScope = usProject) or (aDecl = nil) then
    Exit(FFallback.IsReferenced(aDecl, aScope));
  lKey := DeclKey(aDecl, aDecl.SourceLinenumber);
  if lKey = '' then
    Exit(FFallback.IsReferenced(aDecl, aScope));
  if FUnusedKeys.IndexOf(lKey) >= 0 then
    Result := rrUnused
  else
    Result := rrUsed;
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


{ True iff aProc is a method of a class declaring an interface that names it, or
  one whose interface list cannot be read — its signature is then the
  interface's, not its own. }
function ImplementsInterfaceMethod(aProc: TPasProcedure): boolean;
var
  lClass: TPasClassType;
  lIntf: TPasElement;
  lMembers: TFPList;
  i, j: integer;
begin
  Result := False;
  if not (aProc.Parent is TPasClassType) then
    Exit;
  lClass := TPasClassType(aProc.Parent);
  for i := 0 to lClass.Interfaces.Count - 1 do
  begin
    lIntf := TPasElement(lClass.Interfaces[i]);
    if not (lIntf is TPasClassType) then
      Exit(True);
    lMembers := TPasClassType(lIntf).Members;
    for j := 0 to lMembers.Count - 1 do
      if SameText(TPasElement(lMembers[j]).Name, aProc.Name) then
        Exit(True);
  end;
end;


{ True iff aProc's signature is fixed outside its own body — by an override or
  dispatch chain, by an implemented interface, by RTTI/published visibility, or
  by an external binding. }
function HasFixedSignature(aProc: TPasProcedure): boolean;
begin
  Result := (aProc is TPasOperator)
    or aProc.IsVirtual or aProc.IsDynamic or aProc.IsAbstract
    or aProc.IsOverride or aProc.IsMessage
    or HasRoutineExternalLinkage(aProc)
    or (aProc.Visibility = visPublished)
    or ImplementsInterfaceMethod(aProc);
end;


{ TRuleRemoveUnusedLocalVariable }

procedure TRuleRemoveUnusedLocalVariable.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalyzer: TDualUseAnalyzer;
  lRoutines: TAstRoutineArray;
  lDecls: TFPList;
  lObj: TObject;
  i, j: integer;
begin
  lAnalyzer := TDualUseAnalyzer.Create(aContext);
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
  lAnalyzer: TDualUseAnalyzer;
  lVals: TPasValueDeclArray;
  i: integer;
begin
  lAnalyzer := TDualUseAnalyzer.Create(aContext);
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
  lAnalyzer: TDualUseAnalyzer;
  lVals: TPasValueDeclArray;
  i: integer;
begin
  lAnalyzer := TDualUseAnalyzer.Create(aContext);
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

// True iff every entry of aOverloads was declared on its own source line.
function OverloadRowsDistinct(aOverloads: TFPList): boolean;
var
  i: integer;
  j: integer;
begin
  Result := True;
  for i := 0 to aOverloads.Count - 2 do
  begin
    for j := i + 1 to aOverloads.Count - 1 do
    begin
      if TPasElement(aOverloads[i]).SourceLinenumber
         = TPasElement(aOverloads[j]).SourceLinenumber then
      begin
        Result := False;
      end;
    end;
  end;
end;


{ Every direct member of every class/record type in aModule, with the
  same-named methods the parser collapsed into one TPasOverloadedProc
  yielded individually in its place. }
function AllClassMembers(aModule: TPasModule): TPasElementArray;

  procedure Append(aEl: TPasElement);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := aEl;
  end;

  procedure AppendMember(aMember: TPasElement);
  var
    lOverloads: TFPList;
    n: integer;
  begin
    if aMember is TPasOverloadedProc then
    begin
      lOverloads := TPasOverloadedProc(aMember).Overloads;
      // Same-row overloads share a DeclKey, so neither engine can tell them apart.
      if OverloadRowsDistinct(lOverloads) then
      begin
        for n := 0 to lOverloads.Count - 1 do
        begin
          Append(TPasElement(lOverloads[n]));
        end;
      end;
    end
    else
    begin
      Append(aMember);
    end;
  end;

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
        AppendMember(TPasElement(lMembers[j]));
      end;
    end;
end;


{ TRuleRemoveUnusedProperty }

procedure TRuleRemoveUnusedProperty.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalyzer: TDualUseAnalyzer;
  lMembers: TPasElementArray;
  lProp: TPasProperty;
  i: integer;
begin
  lAnalyzer := TDualUseAnalyzer.Create(aContext);
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
  lAnalyzer: TDualUseAnalyzer;
  lMembers: TPasElementArray;
  lDecls: TFPList;
  lProc: TPasProcedure;
  i: integer;
begin
  lAnalyzer := TDualUseAnalyzer.Create(aContext);
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
  lAnalyzer: TDualUseAnalyzer;
  lTypes: TPasTypeArray;
  lDecls: TFPList;
  lType: TPasType;
  i: integer;
begin
  lAnalyzer := TDualUseAnalyzer.Create(aContext);
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
  lAnalyzer: TDualUseAnalyzer;
  lVals: TPasValueDeclArray;
  i: integer;
begin
  lAnalyzer := TDualUseAnalyzer.Create(aContext);
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


{ TRuleRemoveUnusedParameter }

procedure TRuleRemoveUnusedParameter.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalysis: TFpSonarUseAnalysis;
  lUnused: TPasElementArray;
  lProc: TPasProcedure;
  i: integer;
begin
  if aContext.Config.UseTierResolution <> utrPrefer then
    Exit;
  lAnalysis := TFpSonarUseAnalysis.Create(aContext.Resolver);
  try
    if not lAnalysis.TryUnusedDeclarations(lUnused) then
      Exit;
    for i := 0 to High(lUnused) do
    begin
      if not (lUnused[i] is TPasArgument) or (lUnused[i].Name = '') then
        Continue;
      lProc := OwningProcedure(lUnused[i]);
      if (lProc = nil) or HasFixedSignature(lProc) then
        Continue;
      EmitUnusedNamed(FMetadata, aContext, aCollector, lUnused[i].Name,
        aContext.Resolver.SourceRow(lUnused[i]));
    end;
  finally
    lAnalysis.Free;
  end;
end;


{ TRuleParameterAssignedButNeverUsed }

procedure TRuleParameterAssignedButNeverUsed.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalysis: TFpSonarUseAnalysis;
  lRoutines: TAstRoutineArray;
  lArgs: TFPList;
  lArg: TPasArgument;
  lReads, lWrites: integer;
  i, j: integer;
begin
  if (aContext.Config.UseTierResolution <> utrPrefer)
    or (aContext.Resolver = nil) then
    Exit;
  lAnalysis := TFpSonarUseAnalysis.Create(aContext.Resolver);
  try
    lRoutines := EnumerateRoutines(aContext.Resolver.ResolvedModule);
    for i := 0 to High(lRoutines) do
    begin
      if HasFixedSignature(lRoutines[i].Decl)
        or (lRoutines[i].Decl.ProcType = nil) then
        Continue;
      lArgs := lRoutines[i].Decl.ProcType.Args;
      for j := 0 to lArgs.Count - 1 do
      begin
        lArg := TPasArgument(lArgs[j]);
        if lArg.Access <> argDefault then
          Continue;
        if lAnalysis.TryAccessCounts(lArg, lReads, lWrites)
          and (lWrites > 0) and (lReads = 0) then
          EmitUnusedNamed(FMetadata, aContext, aCollector, lArg.Name,
            aContext.Resolver.SourceRow(lArg));
      end;
    end;
  finally
    lAnalysis.Free;
  end;
end;


{ TRuleUnusedExceptionVariable }

procedure TRuleUnusedExceptionVariable.CollectHandler(aStmt: TPasImplElement);
begin
  if (aStmt is TPasImplExceptOn) and (TPasImplExceptOn(aStmt).VarEl <> nil) then
    FHandlers.Add(aStmt);
end;


procedure TRuleUnusedExceptionVariable.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalysis: TFpSonarUseAnalysis;
  lRoots: TPasImplElementArray;
  lVar: TPasVariable;
  lProc: TPasProcedure;
  lUsed, lProcUsed: boolean;
  i: integer;
begin
  if (aContext.Config.UseTierResolution <> utrPrefer)
    or (aContext.Resolver = nil) then
    Exit;
  FHandlers := TFPList.Create;
  try
    lAnalysis := TFpSonarUseAnalysis.Create(aContext.Resolver);
    try
      lRoots := EnumerateStatementRoots(aContext.Resolver.ResolvedModule);
      for i := 0 to High(lRoots) do
        WalkStatements(lRoots[i], @CollectHandler);
      for i := 0 to FHandlers.Count - 1 do
      begin
        lVar := TPasImplExceptOn(FHandlers[i]).VarEl;
        if lVar.Name = '' then
          Continue;
        // A handler the analysis never walked marks nothing inside it.
        lProc := OwningProcedure(lVar);
        if (lProc = nil)
          or not lAnalysis.TryDeclarationUsed(lProc, lProcUsed)
          or not lProcUsed then
          Continue;
        if lAnalysis.TryDeclarationUsed(lVar, lUsed) and not lUsed then
          EmitUnusedNamed(FMetadata, aContext, aCollector, lVar.Name,
            aContext.Resolver.SourceRow(lVar));
      end;
    finally
      lAnalysis.Free;
    end;
  finally
    FreeAndNil(FHandlers);
  end;
end;


{ TRuleUnusedLabel }

procedure TRuleUnusedLabel.CollectGoto(aStmt: TPasImplElement);
begin
  if aStmt is TPasImplGoto then
    FTargets.Add(TPasImplGoto(aStmt).LabelName);
end;


procedure TRuleUnusedLabel.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  lDecls: TFPList;
  lLabels: TPasLabels;
  i, j, k: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
  begin
    FTargets := TStringList.Create;
    try
      FTargets.CaseSensitive := False;
      FTargets.Sorted := True;
      FTargets.Duplicates := dupIgnore;
      WalkStatements(lRoutines[i].Block, @CollectGoto);
      lDecls := lRoutines[i].Decl.Body.Declarations;
      for j := 0 to lDecls.Count - 1 do
      begin
        if not (TObject(lDecls[j]) is TPasLabels) then
          Continue;
        lLabels := TPasLabels(lDecls[j]);
        for k := 0 to lLabels.Labels.Count - 1 do
          if FTargets.IndexOf(lLabels.Labels[k]) < 0 then
            EmitUnusedNamed(FMetadata, aContext, aCollector,
              lLabels.Labels[k], lLabels.SourceLinenumber);
      end;
    finally
      FreeAndNil(FTargets);
    end;
  end;
end;


{ TRuleUnusedGenericParameter }

// True iff aType is a specialization, or an array/pointer that ends in one
function IsSpecializationType(aType: TPasType): boolean;
var
  lType: TPasType;
begin
  Result := False;
  lType := aType;
  while lType <> nil do
  begin
    if lType is TPasSpecializeType then
      Exit(True);
    if lType is TPasArrayType then
      lType := TPasArrayType(lType).ElType
    else if lType is TPasPointerType then
      lType := TPasPointerType(lType).DestType
    else
      Break;
  end;
end;


// True iff aProcType's arguments or its result name a specialization
function SignatureSpecializes(aProcType: TPasProcedureType): boolean;
var
  i: integer;
begin
  Result := False;
  if aProcType = nil then
    Exit;
  for i := 0 to aProcType.Args.Count - 1 do
    if IsSpecializationType(TPasArgument(aProcType.Args[i]).ArgType) then
      Exit(True);
  Result := (aProcType is TPasFunctionType)
    and (TPasFunctionType(aProcType).ResultEl <> nil)
    and IsSpecializationType(TPasFunctionType(aProcType).ResultEl.ResultType);
end;


{ True iff aType names a specialization in its own declaration, in its own
  signature or in a member signature: the unit reference set never holds a
  specialize argument (DW-422). }
function DeclaresSpecialization(aType: TPasType): boolean;
var
  lMembers: TFPList;
  lMember: TPasElement;
  lClass: TPasClassType;
  i: integer;
begin
  Result := IsSpecializationType(aType);
  if Result then
    Exit;
  // A generic procedure type is a TPasGenericType with no members.
  if aType is TPasProcedureType then
    Exit(SignatureSpecializes(TPasProcedureType(aType)));
  if not (aType is TPasMembersType) then
    Exit;
  if aType is TPasClassType then
  begin
    lClass := TPasClassType(aType);
    if IsSpecializationType(lClass.AncestorType) then
      Exit(True);
    for i := 0 to lClass.Interfaces.Count - 1 do
      if IsSpecializationType(TPasType(lClass.Interfaces[i])) then
        Exit(True);
  end;
  lMembers := TPasMembersType(aType).Members;
  for i := 0 to lMembers.Count - 1 do
  begin
    lMember := TPasElement(lMembers[i]);
    if (lMember is TPasVariable)
      and IsSpecializationType(TPasVariable(lMember).VarType) then
      Exit(True);
    if (lMember is TPasProcedure)
      and SignatureSpecializes(TPasProcedure(lMember).ProcType) then
      Exit(True);
  end;
end;


procedure TRuleUnusedGenericParameter.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalysis: TFpSonarUseAnalysis;
  lAnalyzer: TFpSonarUseAnalyzer;
  lTypes: TPasTypeArray;
  lTemplates: TFPList;
  lTemplate: TPasElement;
  lComplete: boolean;
  i, j: integer;
begin
  if aContext.Config.UseTierResolution <> utrPrefer then
    Exit;
  lAnalysis := TFpSonarUseAnalysis.Create(aContext.Resolver);
  try
    lComplete := lAnalysis.TryComplete;
  finally
    lAnalysis.Free;
  end;
  if not lComplete then
    Exit;
  // The name engine, never the resolved one, which never sees a template.
  lAnalyzer := MakeUseAnalyzer(aContext.Module, aContext.Resolver, False);
  try
    lTypes := EnumerateTypes(aContext.Module);
    for i := 0 to High(lTypes) do
    begin
      if not (lTypes[i] is TPasGenericType) then
        Continue;
      if DeclaresSpecialization(lTypes[i]) then
        Continue;
      lTemplates := TPasGenericType(lTypes[i]).GenericTemplateTypes;
      if lTemplates = nil then
        Continue;
      for j := 0 to lTemplates.Count - 1 do
      begin
        lTemplate := TPasElement(lTemplates[j]);
        if (lTemplate.Name <> '')
          and (lAnalyzer.IsReferenced(lTemplate, usUnit) = rrUnused) then
          EmitUnusedNamed(FMetadata, aContext, aCollector, lTemplate.Name,
            lTemplate.SourceLinenumber);
      end;
    end;
  finally
    lAnalyzer.Free;
  end;
end;


{ TRuleUnusedUnitInInterface }

procedure TRuleUnusedUnitInInterface.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalyzer: TFpSonarUseAnalyzer;
  lClause: TPasUsesClause;
  lNames: TStringList;
  lName: string;
  i: integer;
begin
  if (aContext.ProjectIndex = nil) or (aContext.Module = nil)
    or (aContext.Module.InterfaceSection = nil) then
    Exit;
  lAnalyzer := MakeAnalyzer(aContext);
  try
    lClause := aContext.Module.InterfaceSection.UsesClause;
    for i := 0 to High(lClause) do
    begin
      lName := lClause[i].Name;
      if lName = '' then
        Continue;
      lNames := aContext.ProjectIndex.InterfaceNames(lName);
      if lNames = nil then
        Continue;
      // The interface needs it, or nobody does (RemoveUnusedImports owns that).
      if lAnalyzer.InterfaceReferencesAny(lNames)
        or not lAnalyzer.ReferencesAny(lNames) then
        Continue;
      // An operator or helper is reached by dispatch, naming no identifier.
      if aContext.ProjectIndex.UnitHasOperatorOrHelper(lName) then
        Continue;
      EmitUnusedNamed(FMetadata, aContext, aCollector, lName,
        lClause[i].SourceLinenumber);
    end;
  finally
    lAnalyzer.Free;
  end;
end;


{ TRulePrivateMemberOnlyUsedByOneMethod }

procedure TRulePrivateMemberOnlyUsedByOneMethod.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lNodes, lDecls: TPasElementArray;
  lTypes: TPasTypeArray;
  lOwner: TPasMembersType;
  lMembers: TFPList;
  lMember: TPasElement;
  i, j: integer;

  // True iff aName is a read/write/stored accessor name of a property of lOwner
  function IsAccessorName(const aName: string): boolean;
  var
    lProp: TPasProperty;
    n: integer;
  begin
    Result := False;
    for n := 0 to lOwner.Members.Count - 1 do
      if TObject(lOwner.Members[n]) is TPasProperty then
      begin
        lProp := TPasProperty(lOwner.Members[n]);
        if SameText(aName, lProp.ReadAccessorName)
          or SameText(aName, lProp.WriteAccessorName)
          or SameText(aName, lProp.StoredAccessorName) then
          Exit(True);
      end;
  end;

  { How many distinct routines hold a resolved reference to aDecl, or -1 when a
    reference sits outside any routine — an initialization section or a
    declaration initializer, which no routine can absorb. }
  function ReferringMethods(aDecl: TPasElement): integer;
  var
    lProcs: TFPList;
    lProc: TPasProcedure;
    n: integer;
  begin
    lProcs := TFPList.Create;
    try
      Result := 0;
      for n := 0 to High(lDecls) do
      begin
        if lDecls[n] <> aDecl then
          Continue;
        lProc := OwningProcedure(lNodes[n]);
        if lProc = nil then
          Exit(-1);
        if lProcs.IndexOf(lProc) < 0 then
          lProcs.Add(lProc);
      end;
      Result := lProcs.Count;
    finally
      lProcs.Free;
    end;
  end;

  // True iff aEl is a private field or a flaggable private method
  function IsCandidate(aEl: TPasElement): boolean;
  begin
    if aEl is TPasProcedure then
      Result := IsFlaggablePrivateMethod(TPasProcedure(aEl))
    else
      Result := (aEl is TPasVariable) and not (aEl is TPasConst)
        and not (aEl is TPasProperty) and IsPrivateMember(aEl);
  end;

begin
  if (aContext.Config.UseTierResolution <> utrPrefer)
    or (aContext.ProjectIndex = nil) or (aContext.Resolver = nil) then
    Exit;
  if not aContext.Resolver.TryReferenceSites(lNodes, lDecls) then
    Exit;
  lTypes := EnumerateTypes(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lTypes) do
  begin
    if not (lTypes[i] is TPasMembersType) then
      Continue;
    lOwner := TPasMembersType(lTypes[i]);
    lMembers := lOwner.Members;
    for j := 0 to lMembers.Count - 1 do
    begin
      lMember := TPasElement(lMembers[j]);
      if (lMember.Name = '') or not IsCandidate(lMember)
        or IsAccessorName(lMember.Name) then
        Continue;
      if aContext.ProjectIndex.IsReferencedInProject(lMember.Name) = rrUnknown then
        Continue;
      if ReferringMethods(lMember) = 1 then
        EmitUnusedNamed(FMetadata, aContext, aCollector, lMember.Name,
          aContext.Resolver.SourceRow(lMember));
    end;
  end;
end;


{ TRuleWriteOnlyVariable }

// True iff aVar has an interface type, or a type the tree cannot classify
function IsInterfaceTyped(aVar: TPasVariable): boolean;
var
  lType: TPasType;
begin
  Result := True;
  lType := aVar.VarType;
  // An alias, including a specialization, stands for the type it names.
  while lType is TPasAliasType do
    lType := TPasAliasType(lType).DestType;
  if lType = nil then
    Exit;
  Result := (lType is TPasClassType)
    and (TPasClassType(lType).ObjKind in [okInterface, okDispInterface]);
end;


procedure TRuleWriteOnlyVariable.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAnalysis: TFpSonarUseAnalysis;
  lRoutines: TAstRoutineArray;
  lTypes: TPasTypeArray;
  lList: TFPList;
  i, j: integer;

  // Emits when the analysis recorded writes and no read for aEl
  procedure CheckVar(aEl: TObject);
  var
    lReads, lWrites: integer;
  begin
    if not (aEl is TPasVariable) or (aEl is TPasConst) or (aEl is TPasProperty) then
      Exit;
    if (TPasVariable(aEl).Name = '') or HasGlobalExternalLinkage(TPasVariable(aEl)) then
      Exit;
    // An alias writes through to its host, and an interface reference is held
    // for its lifetime rather than for a later read.
    if (TPasVariable(aEl).AbsoluteExpr <> nil)
      or IsInterfaceTyped(TPasVariable(aEl)) then
      Exit;
    if lAnalysis.TryAccessCounts(TPasElement(aEl), lReads, lWrites)
      and (lWrites > 0) and (lReads = 0) then
      EmitUnusedNamed(FMetadata, aContext, aCollector, TPasVariable(aEl).Name,
        aContext.Resolver.SourceRow(TPasElement(aEl)));
  end;

begin
  if (aContext.Config.UseTierResolution <> utrPrefer)
    or (aContext.Resolver = nil) then
    Exit;
  lAnalysis := TFpSonarUseAnalysis.Create(aContext.Resolver);
  try
    lRoutines := EnumerateRoutines(aContext.Resolver.ResolvedModule);
    for i := 0 to High(lRoutines) do
    begin
      lList := lRoutines[i].Decl.Body.Declarations;
      for j := 0 to lList.Count - 1 do
        CheckVar(TObject(lList[j]));
    end;
    lTypes := EnumerateTypes(aContext.Resolver.ResolvedModule);
    for i := 0 to High(lTypes) do
    begin
      if not (lTypes[i] is TPasMembersType) then
        Continue;
      lList := TPasMembersType(lTypes[i]).Members;
      for j := 0 to lList.Count - 1 do
        if IsPrivateMember(TPasElement(lList[j])) then
          CheckVar(TObject(lList[j]));
    end;
  finally
    lAnalysis.Free;
  end;
end;


// Builds an rtUse / rfAst metadata for an unused-declaration rule
function UnusedMeta(const aRuleId, aMessageKey: string): TRuleMetadata;
begin
  Result := TRuleMetadata.Make(aRuleId, rtUse, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, aMessageKey);
end;


// Builds an rtUse / rfAst metadata, shipped disabled, at the given severity
function NewUnusedMeta(const aRuleId, aMessageKey: string;
  aSeverity: TFpSonarSeverity; aConfidence: TFpSonarConfidence): TRuleMetadata;
begin
  Result := TRuleMetadata.Make(aRuleId, rtUse, rfAst, aSeverity, itCodeSmell,
    aConfidence, False, aMessageKey);
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

  RegisterRule(TRuleRemoveUnusedParameter.Create(NewUnusedMeta(
    'RemoveUnusedParameter', cKeyUnusedParameter, sevMinor,
    cfMedium).WithDescription(
    'Flags a parameter no statement in the routine reads.')));
  RegisterMessage(cKeyUnusedParameter, SRemoveUnusedParameter);

  RegisterRule(TRuleParameterAssignedButNeverUsed.Create(NewUnusedMeta(
    'ParameterAssignedButNeverUsed', cKeyParameterAssigned, sevMinor,
    cfMedium).WithDescription(
    'Flags a value parameter that is assigned and whose value is then discarded.')));
  RegisterMessage(cKeyParameterAssigned, SParameterAssignedButNeverUsed);

  RegisterRule(TRuleUnusedExceptionVariable.Create(NewUnusedMeta(
    'UnusedExceptionVariable', cKeyUnusedExceptVar, sevMinor,
    cfHigh).WithDescription(
    'Flags an on E: handler variable the handler never uses.')));
  RegisterMessage(cKeyUnusedExceptVar, SUnusedExceptionVariable);

  RegisterRule(TRuleUnusedLabel.Create(NewUnusedMeta(
    'UnusedLabel', cKeyUnusedLabel, sevMinor, cfMedium).WithDescription(
    'Flags a declared label no goto targets.')));
  RegisterMessage(cKeyUnusedLabel, SUnusedLabel);

  RegisterRule(TRuleUnusedGenericParameter.Create(NewUnusedMeta(
    'UnusedGenericParameter', cKeyUnusedGenericParam, sevMinor,
    cfMedium).WithDescription(
    'Flags a generic type parameter the generic body never names.')));
  RegisterMessage(cKeyUnusedGenericParam, SUnusedGenericParameter);

  RegisterRule(TRuleUnusedUnitInInterface.Create(NewUnusedMeta(
    'UnusedUnitInInterface', cKeyUnusedInterfaceUnit, sevMinor,
    cfMedium).WithDescription(
    'Flags a unit imported in the interface section that only the '
    + 'implementation section needs.')));
  RegisterMessage(cKeyUnusedInterfaceUnit, SUnusedUnitInInterface);

  RegisterRule(TRulePrivateMemberOnlyUsedByOneMethod.Create(NewUnusedMeta(
    'PrivateMemberOnlyUsedByOneMethod', cKeyOneMethodMember, sevInfo,
    cfLow).WithDescription(
    'Flags a private member only one method of its own type references.')));
  RegisterMessage(cKeyOneMethodMember, SPrivateMemberOnlyUsedByOneMethod);

  RegisterRule(TRuleWriteOnlyVariable.Create(NewUnusedMeta(
    'WriteOnlyVariable', cKeyWriteOnlyVariable, sevMinor,
    cfMedium).WithDescription(
    'Flags a variable that is written and never read.')));
  RegisterMessage(cKeyWriteOnlyVariable, SWriteOnlyVariable);

end.
