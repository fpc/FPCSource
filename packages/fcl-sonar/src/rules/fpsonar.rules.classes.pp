{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Class-hygiene AST analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Classes;


{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, Pascal.Tree,
{$ELSE}
  SysUtils, PasTree,
{$ENDIF}
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Traversal, FpSonar.Resolver,
  FpSonar.Config, FpSonar.Rules.Consts;

type
  { Flags a visibility section that is out of ascending order. }
  TRuleVisibilityAscendingOrder = class(TRuleBase)
  public
    // Emits one issue per out-of-order visibility section in a class/object.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a member that breaks the field/method/property order within a
    visibility section. }
  TRuleDeclarationsFollowVisibilityOrder = class(TRuleBase)
  public
    // Emits one issue per member declared out of field/method/property order.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a field declared in public/published visibility. }
  TRuleFieldsNotPublic = class(TRuleBase)
  public
    // Emits one issue per public/published field in a class/object.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a unit declaring more than cMaxClasses class types. }
  TRuleFileNotTooManyClasses = class(TRuleBase)
  public
    // Emits one issue at the unit when it declares more than 5 classes.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an interface type with no methods and no properties. }
  TRuleInterfaceNotEmpty = class(TRuleBase)
  public
    // Emits one issue per empty interface (no method/property).
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags interfaces sharing a GUID, or a COM interface missing one. }
  TRuleInterfaceUniqueGuid = class(TRuleBase)
  public
    // Emits one issue per interface with a duplicate or missing-COM GUID.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a constructor body with no inherited call. }
  TRuleConstructorInherited = class(TRuleBase)
  public
    // Emits one issue per constructor whose body never calls inherited.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a destructor body with no inherited call. }
  TRuleDestructorInherited = class(TRuleBase)
  public
    // Emits one issue per destructor whose body never calls inherited.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a top-level class declared with no ancestor clause. }
  TRuleTopLevelClassInheritsTObject = class(TRuleBase)
  public
    // Emits one issue per top-level ancestor-less class (TFoo = class).
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a method that silently hides an overridable ancestor method.
    Polarity: positive. }
  TRuleMethodHidesVirtualWithoutOverride = class(TRuleBase)
  public
    // Emits one issue per method hiding a virtual ancestor without saying so.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an override that restates an inherited default with another value.
    Polarity: positive. }
  TRuleOverrideChangesDefaultParameterValue = class(TRuleBase)
  public
    // Emits one issue per override changing an inherited default parameter value.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an inherited call to a method the ancestor declares abstract.
    Polarity: positive. }
  TRuleAbstractMethodCalledDirectly = class(TRuleBase)
  public
    // Emits one issue per inherited call bound to an abstract ancestor method.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a constructor call on a class with unimplemented abstract methods.
    Polarity: positive. }
  TRuleInstantiatesClassWithAbstractMethods = class(TRuleBase)
  public
    // Emits one issue per construction of a class with an abstract method left.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a dynamic interface query naming a GUID-less COM interface.
    Polarity: positive. }
  TRuleInterfaceWithoutGuidUsedDynamically = class(TRuleBase)
  public
    // Emits one issue per as-cast or interface query naming a GUID-less COM interface.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a Supports call whose boolean result is thrown away.
    Polarity: positive. }
  TRuleSupportsResultIgnored = class(TRuleBase)
  public
    // Emits one issue per Supports call written as a bare statement.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a class-helper method whose name the extended type already declares.
    Polarity: positive. }
  TRuleClassHelperHidesAncestorMethod = class(TRuleBase)
  public
    // Emits one issue per helper method shadowing a method of the extended chain.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags Assigned applied to a value-typed operand.
    Polarity: positive. }
  TRuleAssignedOnNonReference = class(TRuleBase)
  public
    // Emits one issue per Assigned call whose argument names a value type.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags storage reachable both through a public field and through a property.
    Polarity: positive. }
  TRulePublicFieldAndPropertyForSameStorage = class(TRuleBase)
  public
    // Emits one issue per property whose accessor is a public/published field.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a property whose accessor routine is more visible than the property.
    Polarity: positive. }
  TRulePropertyAccessorVisibilityWiderThanProperty = class(TRuleBase)
  public
    // Emits one issue per property with a wider-visibility accessor routine.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a property read accessor that mutates the object's own field state.
    Polarity: positive. }
  TRulePropertyGetterWithSideEffect = class(TRuleBase)
  public
    // Emits one issue per property whose getter writes one of its own fields.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a non-virtual constructor a descendant class redeclares.
    Polarity: positive. }
  TRuleConstructorNotVirtualInPolymorphicHierarchy = class(TRuleBase)
  public
    // Emits one issue per non-virtual constructor of a polymorphic base class
    // that a descendant redeclares.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a constructor whose inherited call is not its first statement.
    Polarity: positive. }
  TRuleInheritedCreateNotFirstStatement = class(TRuleBase)
  public
    // Emits one issue per constructor that runs a statement before inherited.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a destructor whose inherited call is not its last statement.
    Polarity: positive. }
  TRuleInheritedDestroyNotLastStatement = class(TRuleBase)
  public
    // Emits one issue per destructor that runs a statement after inherited.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an = or <> comparison of two operands of a class type that is not an
    interface, which compares references. Polarity: positive. }
  TRuleComparingClassReferencesWithEquals = class(TRuleBase)
  public
    // Emits one issue per comparison whose two operands classify ltkClass.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

const
  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyVisibilityAscendingOrder = 'rule.VisibilityAscendingOrder.message';
  cKeyDeclarationsFollowVisibilityOrder =
    'rule.DeclarationsFollowVisibilityOrder.message';
  cKeyFieldsNotPublic = 'rule.FieldsNotPublic.message';
  cKeyFileNotTooManyClasses = 'rule.FileNotTooManyClasses.message';
  cKeyInterfaceNotEmpty = 'rule.InterfaceNotEmpty.message';
  cKeyInterfaceUniqueGuid = 'rule.InterfaceUniqueGuid.message';
  cKeyConstructorInherited = 'rule.ConstructorInherited.message';
  cKeyDestructorInherited = 'rule.DestructorInherited.message';
  cKeyTopLevelClassInheritsTObject =
    'rule.TopLevelClassInheritsTObject.message';
  cKeyMethodHidesVirtualWithoutOverride =
    'rule.MethodHidesVirtualWithoutOverride.message';
  cKeyOverrideChangesDefaultParameterValue =
    'rule.OverrideChangesDefaultParameterValue.message';
  cKeyAbstractMethodCalledDirectly =
    'rule.AbstractMethodCalledDirectly.message';
  cKeyInstantiatesClassWithAbstractMethods =
    'rule.InstantiatesClassWithAbstractMethods.message';
  cKeyInterfaceWithoutGuidUsedDynamically =
    'rule.InterfaceWithoutGuidUsedDynamically.message';
  cKeySupportsResultIgnored = 'rule.SupportsResultIgnored.message';
  cKeyClassHelperHidesAncestorMethod =
    'rule.ClassHelperHidesAncestorMethod.message';
  cKeyAssignedOnNonReference = 'rule.AssignedOnNonReference.message';
  cKeyPublicFieldAndPropertyForSameStorage =
    'rule.PublicFieldAndPropertyForSameStorage.message';
  cKeyPropertyAccessorVisibilityWiderThanProperty =
    'rule.PropertyAccessorVisibilityWiderThanProperty.message';
  cKeyPropertyGetterWithSideEffect =
    'rule.PropertyGetterWithSideEffect.message';
  cKeyConstructorNotVirtualInPolymorphicHierarchy =
    'rule.ConstructorNotVirtualInPolymorphicHierarchy.message';
  cKeyInheritedCreateNotFirstStatement =
    'rule.InheritedCreateNotFirstStatement.message';
  cKeyInheritedDestroyNotLastStatement =
    'rule.InheritedDestroyNotLastStatement.message';
  cKeyComparingClassReferencesWithEquals =
    'rule.ComparingClassReferencesWithEquals.message';

  // Thresholds/params as named constants.
  cMaxClasses = 5;        // More than this many classes -> flag.
  cAllowMarker = False;   // Marker (empty) interfaces ARE flagged.
  cRequireGuid = True;    // A COM interface missing a GUID is flagged.

  // The two interface object kinds; every interface rule here accepts both.
  cInterfaceKinds = [okInterface, okDispInterface];

  // The callee identifiers the interface-query and Assigned rules match by name.
  cSupportsRoutine = 'Supports';
  cQueryInterfaceRoutine = 'QueryInterface';
  cAssignedRoutine = 'Assigned';


// Accessibility rank for the ascending-order rule:
// strict private < private < strict protected < protected < public < published.
function VisibilityRank(aVis: TPasMemberVisibility): integer;
begin
  case aVis of
    visStrictPrivate: Result := 0;
    visPrivate: Result := 1;
    visStrictProtected: Result := 2;
    visProtected: Result := 3;
    visPublic: Result := 4;
    visPublished: Result := 5;
    else
      Result := -1;
  end;
end;


// The lowercased visibility keyword for the message arg/snippet
function VisibilityName(aVis: TPasMemberVisibility): string;
begin
  case aVis of
    visStrictPrivate: Result := 'strict private';
    visPrivate: Result := 'private';
    visStrictProtected: Result := 'strict protected';
    visProtected: Result := 'protected';
    visPublic: Result := 'public';
    visPublished: Result := 'published';
    else
      Result := '';
  end;
end;


// The field/method/property phase for the declaration-order rule:
function MemberPhase(aMember: TPasElement): integer;
begin
  if aMember is TPasProperty then
    Result := 2
  else if aMember is TPasProcedure then
    Result := 1
  else if (aMember is TPasVariable) and not (aMember is TPasConst) then
    Result := 0
  else
    Result := -1;
end;


// Core emitter — one issue at (aLine,1)-(aLine,1),
procedure EmitClass(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aLine: integer; const aArg: string);
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aLine, 1, aLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    [aArg], aArg);
end;


{ TRuleVisibilityAscendingOrder }

procedure TRuleVisibilityAscendingOrder.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  lClass: TPasClassType;
  lMember: TPasElement;
  t, m, lRank, lMaxRank, lPrevRank: integer;
begin
  lTypes := EnumerateTypes(aContext.Module);
  for t := 0 to High(lTypes) do
    if (lTypes[t] is TPasClassType)
      and (TPasClassType(lTypes[t]).ObjKind in [okClass, okObject]) then
    begin
      lClass := TPasClassType(lTypes[t]);
      lMaxRank := -1;
      lPrevRank := -2; // sentinel: no rank-eligible member seen yet
      for m := 0 to lClass.Members.Count - 1 do
      begin
        lMember := TPasElement(lClass.Members[m]);
        lRank := VisibilityRank(lMember.Visibility);
        if lRank < 0 then
          Continue; // visDefault/visAutomated: skip (Fact B)
        if lRank <> lPrevRank then
        begin
          // First member of a new visibility section.
          if lRank < lMaxRank then
            EmitClass(FMetadata, aContext, aCollector,
              lMember.SourceLinenumber, VisibilityName(lMember.Visibility));
          lPrevRank := lRank;
        end;
        if lRank > lMaxRank then
          lMaxRank := lRank;
      end;
    end;
end;


{ TRuleDeclarationsFollowVisibilityOrder }

procedure TRuleDeclarationsFollowVisibilityOrder.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  lClass: TPasClassType;
  lMember: TPasElement;
  t, m, lRank, lPrevRank, lPhase, lPhaseMax: integer;
begin
  lTypes := EnumerateTypes(aContext.Module);
  for t := 0 to High(lTypes) do
    if (lTypes[t] is TPasClassType)
      and (TPasClassType(lTypes[t]).ObjKind in [okClass, okObject]) then
    begin
      lClass := TPasClassType(lTypes[t]);
      lPrevRank := -2; // sentinel: no rank-eligible member seen yet
      lPhaseMax := -1;
      for m := 0 to lClass.Members.Count - 1 do
      begin
        lMember := TPasElement(lClass.Members[m]);
        lRank := VisibilityRank(lMember.Visibility);
        if lRank < 0 then
          Continue; // skip the implicit/automated sections (Fact B)
        if lRank <> lPrevRank then
        begin
          // New visibility section: reset the phase tracker.
          lPhaseMax := -1;
          lPrevRank := lRank;
        end;
        lPhase := MemberPhase(lMember);
        if lPhase < 0 then
          Continue; // const/nested type: neither advances nor violates
        if lPhase < lPhaseMax then
          EmitClass(FMetadata, aContext, aCollector,
            lMember.SourceLinenumber, lMember.Name)
        else if lPhase > lPhaseMax then
          lPhaseMax := lPhase;
      end;
    end;
end;


{ TRuleFieldsNotPublic }

procedure TRuleFieldsNotPublic.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  lClass: TPasClassType;
  lMember: TPasElement;
  t, m: integer;
begin
  lTypes := EnumerateTypes(aContext.Module);
  for t := 0 to High(lTypes) do
    if (lTypes[t] is TPasClassType)
      and (TPasClassType(lTypes[t]).ObjKind in [okClass, okObject]) then
    begin
      lClass := TPasClassType(lTypes[t]);
      for m := 0 to lClass.Members.Count - 1 do
      begin
        lMember := TPasElement(lClass.Members[m]);
        if (lMember is TPasVariable) and not (lMember is TPasProperty)
          and not (lMember is TPasConst)
          and (lMember.Visibility in [visPublic, visPublished]) then
          EmitClass(FMetadata, aContext, aCollector,
            lMember.SourceLinenumber, lMember.Name);
      end;
    end;
end;


{ TRuleFileNotTooManyClasses }

procedure TRuleFileNotTooManyClasses.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  t, lCount, lLimit: integer;
begin
  if aContext.Module = nil then
    Exit;
  lLimit := aContext.Config.RuleParamInt(FMetadata.RuleId, 'maxClasses',
    cMaxClasses);
  lCount := 0;
  lTypes := EnumerateTypes(aContext.Module);
  for t := 0 to High(lTypes) do
    if (lTypes[t] is TPasClassType)
      and (TPasClassType(lTypes[t]).ObjKind = okClass) then
      Inc(lCount);
  // One issue at the 'unit X;' line (col 1), as UnitNotEmpty reports.
  if lCount > lLimit then
    EmitClass(FMetadata, aContext, aCollector,
      aContext.Module.SourceLinenumber, IntToStr(lCount));
end;


{ TRuleInterfaceNotEmpty }

// True when aClass (an interface) declares at least one method or property.
function InterfaceHasMembers(aClass: TPasClassType): boolean;
var
  m: integer;
  lMember: TPasElement;
begin
  Result := False;
  for m := 0 to aClass.Members.Count - 1 do
  begin
    lMember := TPasElement(aClass.Members[m]);
    if (lMember is TPasProcedure) or (lMember is TPasProperty) then
      Exit(True);
  end;
end;


procedure TRuleInterfaceNotEmpty.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  lClass: TPasClassType;
  t: integer;
begin
  lTypes := EnumerateTypes(aContext.Module);
  for t := 0 to High(lTypes) do
    if (lTypes[t] is TPasClassType)
      and (TPasClassType(lTypes[t]).ObjKind in cInterfaceKinds) then
    begin
      lClass := TPasClassType(lTypes[t]);
      // cAllowMarker is False -> marker (empty) interfaces ARE flagged.
      if not cAllowMarker and not InterfaceHasMembers(lClass) then
        EmitClass(FMetadata, aContext, aCollector,
          lClass.SourceLinenumber, lClass.Name);
    end;
end;


{ TRuleInterfaceUniqueGuid }

// The interface's GUID, normalised for comparison ('' when it has none).
function NormalisedGuid(aClass: TPasClassType): string;
begin
  Result := UpperCase(Trim(aClass.InterfaceGUID));
end;


procedure TRuleInterfaceUniqueGuid.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  lIfaces: array of TPasClassType;
  lGuids: array of string;
  t, i, j, lDup: integer;
begin
  lTypes := EnumerateTypes(aContext.Module);
  // Pass 1: collect every interface and its normalised GUID, in order.
  SetLength(lIfaces, 0);
  SetLength(lGuids, 0);
  for t := 0 to High(lTypes) do
    if (lTypes[t] is TPasClassType)
      and (TPasClassType(lTypes[t]).ObjKind in cInterfaceKinds) then
    begin
      SetLength(lIfaces, Length(lIfaces) + 1);
      SetLength(lGuids, Length(lGuids) + 1);
      lIfaces[High(lIfaces)] := TPasClassType(lTypes[t]);
      lGuids[High(lGuids)] := NormalisedGuid(TPasClassType(lTypes[t]));
    end;
  // Pass 2: a non-empty GUID shared with another interface is a duplicate;
  for i := 0 to High(lIfaces) do
    if lGuids[i] <> '' then
    begin
      lDup := 0;
      for j := 0 to High(lGuids) do
        if lGuids[j] = lGuids[i] then
          Inc(lDup);
      if lDup > 1 then
        EmitClass(FMetadata, aContext, aCollector,
          lIfaces[i].SourceLinenumber, lIfaces[i].Name);
    end
    else if cRequireGuid and (lIfaces[i].InterfaceType = citCom) then
      EmitClass(FMetadata, aContext, aCollector,
        lIfaces[i].SourceLinenumber, lIfaces[i].Name);
end;


{ inherited-detection helpers }

// True when the expression tree rooted at aExpr contains an 'inherited'
function ExprHasInherited(aExpr: TPasExpr): boolean;
var
  k: integer;
begin
  Result := False;
  if aExpr = nil then
    Exit;
  if aExpr is TInheritedExpr then
    Exit(True);
  if aExpr is TBinaryExpr then
    Result := ExprHasInherited(TBinaryExpr(aExpr).Left)
      or ExprHasInherited(TBinaryExpr(aExpr).Right)
  else if aExpr is TUnaryExpr then
    Result := ExprHasInherited(TUnaryExpr(aExpr).Operand)
  else if aExpr is TParamsExpr then
  begin
    Result := ExprHasInherited(TParamsExpr(aExpr).Value);
    if not Result then
      for k := 0 to High(TParamsExpr(aExpr).Params) do
        if ExprHasInherited(TParamsExpr(aExpr).Params[k]) then
          Exit(True);
  end;
end;


// True when the statement subtree rooted at aStmt calls 'inherited' anywhere.
function StmtHasInherited(aStmt: TPasImplElement): boolean;
var
  lChildren: TPasImplElementArray;
  i: integer;
begin
  Result := False;
  if aStmt = nil then
    Exit;
  if aStmt is TPasImplSimple then
    Result := ExprHasInherited(TPasImplSimple(aStmt).Expr)
  else if aStmt is TPasImplAssign then
    Result := ExprHasInherited(TPasImplAssign(aStmt).Left)
      or ExprHasInherited(TPasImplAssign(aStmt).Right);
  if Result then
    Exit;
  lChildren := ChildStatements(aStmt);
  for i := 0 to High(lChildren) do
    if StmtHasInherited(lChildren[i]) then
      Exit(True);
end;


// True when aStmt is a bare 'inherited' or a named inherited call.
function StmtIsInheritedCall(aStmt: TPasImplElement): boolean;
var
  lExpr: TPasExpr;
begin
  Result := False;
  if not (aStmt is TPasImplSimple) then
    Exit;
  lExpr := TPasImplSimple(aStmt).Expr;
  Result := (lExpr is TInheritedExpr)
    or ((lExpr is TBinaryExpr) and (TBinaryExpr(lExpr).OpCode = eopNone)
    and (TBinaryExpr(lExpr).Left is TInheritedExpr));
end;


// The first (aFirst) or last top-level statement of aBlock, or nil.
function EdgeStatement(aBlock: TPasImplBlock;
  aFirst: boolean): TPasImplElement;
var
  lStmts: TPasImplElementArray;
begin
  Result := nil;
  lStmts := ChildStatements(aBlock);
  if Length(lStmts) = 0 then
    Exit;
  if aFirst then
    Result := lStmts[0]
  else
    Result := lStmts[High(lStmts)];
end;


{ TRuleConstructorInherited }

procedure TRuleConstructorInherited.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  r: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Module);
  for r := 0 to High(lRoutines) do
    // A class constructor chains is excluded;
    if (lRoutines[r].Decl is TPasConstructor)
      and not (lRoutines[r].Decl is TPasClassConstructor)
      and not StmtHasInherited(lRoutines[r].Block) then
      EmitClass(FMetadata, aContext, aCollector,
        lRoutines[r].Decl.SourceLinenumber, lRoutines[r].Decl.Name);
end;


{ TRuleDestructorInherited }

procedure TRuleDestructorInherited.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  r: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Module);
  for r := 0 to High(lRoutines) do
    // A class destructor is excluded.
    if (lRoutines[r].Decl is TPasDestructor)
      and not (lRoutines[r].Decl is TPasClassDestructor)
      and not StmtHasInherited(lRoutines[r].Block) then
      EmitClass(FMetadata, aContext, aCollector,
        lRoutines[r].Decl.SourceLinenumber, lRoutines[r].Decl.Name);
end;


{ TRuleTopLevelClassInheritsTObject }

procedure TRuleTopLevelClassInheritsTObject.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  lClass: TPasClassType;
  t: integer;
begin
  lTypes := EnumerateTypes(aContext.Module);
  for t := 0 to High(lTypes) do
    if (lTypes[t] is TPasClassType)
      and (TPasClassType(lTypes[t]).ObjKind = okClass) then
    begin
      lClass := TPasClassType(lTypes[t]);
      // Top-level only and not an external class
      if (lClass.AncestorType = nil) and not lClass.IsExternal
        and not (lClass.Parent is TPasMembersType) then
        EmitClass(FMetadata, aContext, aCollector,
          lClass.SourceLinenumber, lClass.Name);
    end;
end;


{ signature-contract helpers }

// Every method DECLARATION in aModule's class and object types.
function AllMethodDecls(aModule: TPasModule): TPasElementArray;
var
  lTypes: TPasTypeArray;
  lClass: TPasClassType;
  t, m: integer;
begin
  SetLength(Result, 0);
  lTypes := EnumerateTypes(aModule);
  for t := 0 to High(lTypes) do
    if (lTypes[t] is TPasClassType)
      and (TPasClassType(lTypes[t]).ObjKind in [okClass, okObject]) then
    begin
      lClass := TPasClassType(lTypes[t]);
      if lClass.Members = nil then
        Continue;
      for m := 0 to lClass.Members.Count - 1 do
        if TObject(lClass.Members[m]) is TPasProcedure then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := TPasElement(lClass.Members[m]);
        end;
    end;
end;


{ TRuleMethodHidesVirtualWithoutOverride }

procedure TRuleMethodHidesVirtualWithoutOverride.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lMethods: TPasElementArray;
  i: integer;
begin
  lMethods := AllMethodDecls(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lMethods) do
    if aContext.Resolver.TryMethodHidesVirtual(lMethods[i]) then
      EmitClass(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lMethods[i]), lMethods[i].Name);
end;


{ TRuleOverrideChangesDefaultParameterValue }

procedure TRuleOverrideChangesDefaultParameterValue.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lMethods: TPasElementArray;
  i: integer;
begin
  lMethods := AllMethodDecls(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lMethods) do
    if aContext.Resolver.TryOverrideDefaultParamChange(lMethods[i]) then
      EmitClass(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lMethods[i]), lMethods[i].Name);
end;


{ TRuleAbstractMethodCalledDirectly }

// The class-qualifier of an implementation routine name ('' when unqualified).
function ClassQualifier(const aName: string): string;
var
  p: integer;
begin
  p := LastDelimiter('.', aName);
  if p > 0 then
    Result := Copy(aName, 1, p - 1)
  else
    Result := '';
end;


// The single top-level class or object type named aName in aTypes; nil when
// none or more than one carries the name.
function FindClassNamed(const aTypes: TPasTypeArray;
  const aName: string): TPasClassType;
var
  t: integer;
begin
  Result := nil;
  if aName = '' then
    Exit;
  for t := 0 to High(aTypes) do
    if (aTypes[t] is TPasClassType)
      and (TPasClassType(aTypes[t]).ObjKind in [okClass, okObject])
      and not (aTypes[t].Parent is TPasMembersType)
      and SameText(aTypes[t].Name, aName) then
    begin
      if Result <> nil then
        Exit(nil);
      Result := TPasClassType(aTypes[t]);
    end;
end;


// The method part of an implementation routine name (the whole name when
// unqualified).
function MethodPart(const aName: string): string;
var
  p: integer;
begin
  p := LastDelimiter('.', aName);
  if p > 0 then
    Result := Copy(aName, p + 1, Length(aName))
  else
    Result := aName;
end;


// The last member of aClass named aMethod; aCount is how many carry that name.
function ProcNamed(aClass: TPasClassType; const aMethod: string;
  out aCount: integer): TPasProcedure;
var
  m: integer;
begin
  Result := nil;
  aCount := 0;
  if aClass.Members = nil then
    Exit;
  for m := 0 to aClass.Members.Count - 1 do
    if (TObject(aClass.Members[m]) is TPasProcedure)
      and SameText(TPasElement(aClass.Members[m]).Name, aMethod) then
    begin
      Result := TPasProcedure(aClass.Members[m]);
      Inc(aCount);
    end;
end;


// True when exactly one class above aClass, in the same file, declares aMethod
// and declares it abstract; a second declaration of the name is ambiguous => False.
function AncestorDeclaresAbstract(const aTypes: TPasTypeArray;
  aClass: TPasClassType; const aMethod: string): boolean;
var
  lCur: TPasClassType;
  lFound, lOnly: TPasProcedure;
  lCount, lTotal, lStep: integer;
begin
  Result := False;
  lOnly := nil;
  lTotal := 0;
  lStep := 0;
  lCur := aClass;
  // A by-name walk can cycle on a malformed hierarchy; the chain is at most
  // as long as the file's type list.
  while (lCur.AncestorType <> nil) and (lStep <= Length(aTypes)) do
  begin
    lCur := FindClassNamed(aTypes, lCur.AncestorType.Name);
    if lCur = nil then
      Break;
    lFound := ProcNamed(lCur, aMethod, lCount);
    if lCount > 0 then
    begin
      Inc(lTotal, lCount);
      if lOnly = nil then
        lOnly := lFound;
    end;
    Inc(lStep);
  end;
  if lTotal = 1 then
    Result := pmAbstract in lOnly.Modifiers;
end;


// Appends every 'inherited' expression in aExpr's tree to aList: the named
// 'inherited Name(...)' binary, and the bare 'inherited' on its own.
procedure CollectInheritedExprs(aExpr: TPasExpr; var aList: TPasExprArray);
var
  i: integer;

  procedure Add(aNode: TPasExpr);
  begin
    SetLength(aList, Length(aList) + 1);
    aList[High(aList)] := aNode;
  end;

begin
  if aExpr = nil then
    Exit;
  if aExpr is TInheritedExpr then
    Add(aExpr)
  else if aExpr is TBinaryExpr then
  begin
    if (TBinaryExpr(aExpr).OpCode = eopNone)
      and (TBinaryExpr(aExpr).Left is TInheritedExpr) then
      Add(aExpr)
    else
    begin
      CollectInheritedExprs(TBinaryExpr(aExpr).Left, aList);
      CollectInheritedExprs(TBinaryExpr(aExpr).Right, aList);
    end;
  end
  else if aExpr is TParamsExpr then
  begin
    CollectInheritedExprs(TParamsExpr(aExpr).Value, aList);
    for i := 0 to High(TParamsExpr(aExpr).Params) do
      CollectInheritedExprs(TParamsExpr(aExpr).Params[i], aList);
  end
  else if aExpr is TUnaryExpr then
    CollectInheritedExprs(TUnaryExpr(aExpr).Operand, aList);
end;


// The expressions aStmt owns directly (not those of its child statements).
function StmtExprs(aStmt: TPasImplElement): TPasExprArray;
var
  lWith: TPasImplWithDo;
  j: integer;

  procedure Take(aExpr: TPasExpr);
  begin
    if aExpr = nil then
      Exit;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := aExpr;
  end;

begin
  SetLength(Result, 0);
  if aStmt = nil then
    Exit;
  if aStmt is TPasImplAssign then
  begin
    Take(TPasImplAssign(aStmt).Left);
    Take(TPasImplAssign(aStmt).Right);
  end
  else if aStmt is TPasImplSimple then
    Take(TPasImplSimple(aStmt).Expr)
  else if aStmt is TPasImplIfElse then
    Take(TPasImplIfElse(aStmt).ConditionExpr)
  else if aStmt is TPasImplWhileDo then
    Take(TPasImplWhileDo(aStmt).ConditionExpr)
  else if aStmt is TPasImplRepeatUntil then
    Take(TPasImplRepeatUntil(aStmt).ConditionExpr)
  else if aStmt is TPasImplForLoop then
  begin
    Take(TPasImplForLoop(aStmt).StartExpr);
    Take(TPasImplForLoop(aStmt).EndExpr);
  end
  else if aStmt is TPasImplCaseOf then
    Take(TPasImplCaseOf(aStmt).CaseExpr)
  else if aStmt is TPasImplWithDo then
  begin
    lWith := TPasImplWithDo(aStmt);
    if lWith.Expressions <> nil then
      for j := 0 to lWith.Expressions.Count - 1 do
        if TObject(lWith.Expressions[j]) is TPasExpr then
          Take(TPasExpr(lWith.Expressions[j]));
  end;
end;


// Appends every statement strictly BELOW aRoot to aList.
procedure CollectStatements(aRoot: TPasImplElement;
  var aList: TPasImplElementArray);
var
  lChildren: TPasImplElementArray;
  i: integer;
begin
  lChildren := ChildStatements(aRoot);
  for i := 0 to High(lChildren) do
  begin
    SetLength(aList, Length(aList) + 1);
    aList[High(aList)] := lChildren[i];
    CollectStatements(lChildren[i], aList);
  end;
end;


// Every statement node in aModule
function AllStatements(aModule: TPasModule): TPasImplElementArray;
var
  lRoots: TPasImplElementArray;
  i: integer;
begin
  SetLength(Result, 0);
  lRoots := EnumerateStatementRoots(aModule);
  for i := 0 to High(lRoots) do
    CollectStatements(lRoots[i], Result);
end;


// Appends every 'inherited' expression in aStmt's subtree to aList.
procedure CollectInheritedCalls(aStmt: TPasImplElement;
  var aList: TPasExprArray);
var
  lExprs: TPasExprArray;
  lChildren: TPasImplElementArray;
  i: integer;
begin
  if aStmt = nil then
    Exit;
  lExprs := StmtExprs(aStmt);
  for i := 0 to High(lExprs) do
    CollectInheritedExprs(lExprs[i], aList);
  lChildren := ChildStatements(aStmt);
  for i := 0 to High(lChildren) do
    CollectInheritedCalls(lChildren[i], aList);
end;


// The method name an 'inherited' expression names: the explicit one of
// 'inherited Name(...)', or aSelfName for the bare form.
function InheritedCallName(aExpr: TPasExpr; const aSelfName: string): string;
var
  lCall: TPasExpr;
begin
  if aExpr is TInheritedExpr then
    Exit(aSelfName);
  Result := '';
  lCall := TBinaryExpr(aExpr).Right;
  if (lCall is TParamsExpr) and (TParamsExpr(lCall).Kind = pekFuncParams) then
    lCall := TParamsExpr(lCall).Value;
  if lCall is TPrimitiveExpr then
    Result := TPrimitiveExpr(lCall).Value;
end;


procedure TRuleAbstractMethodCalledDirectly.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  lRoutines: TAstRoutineArray;
  lCalls: TPasExprArray;
  lClass: TPasClassType;
  lName: string;
  r, i: integer;
begin
  lTypes := EnumerateTypes(aContext.Module);
  lRoutines := EnumerateRoutines(aContext.Module);
  for r := 0 to High(lRoutines) do
  begin
    lClass := FindClassNamed(lTypes, ClassQualifier(lRoutines[r].Decl.Name));
    if lClass = nil then
      Continue;
    SetLength(lCalls, 0);
    CollectInheritedCalls(lRoutines[r].Block, lCalls);
    for i := 0 to High(lCalls) do
    begin
      lName := InheritedCallName(lCalls[i], MethodPart(lRoutines[r].Decl.Name));
      if (lName <> '') and AncestorDeclaresAbstract(lTypes, lClass, lName) then
        EmitClass(FMetadata, aContext, aCollector,
          lCalls[i].SourceLinenumber, lName);
    end;
  end;
end;


{ TRuleInstantiatesClassWithAbstractMethods }

procedure TRuleInstantiatesClassWithAbstractMethods.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lNames: TFpSonarStringArray;
  i: integer;
begin
  if not aContext.Resolver.TryAbstractInstantiation(lNodes, lNames) then
    Exit;
  for i := 0 to High(lNodes) do
    EmitClass(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lNodes[i]), lNames[i]);
end;


{ interface-query and helper-shadowing helpers }

// The single top-level interface type named aName in aTypes, forward
// declarations skipped; nil when none or more than one carries the name.
function FindInterfaceNamed(const aTypes: TPasTypeArray;
  const aName: string): TPasClassType;
var
  t: integer;
begin
  Result := nil;
  if aName = '' then
    Exit;
  for t := 0 to High(aTypes) do
    if (aTypes[t] is TPasClassType)
      and (TPasClassType(aTypes[t]).ObjKind in cInterfaceKinds)
      and not TPasClassType(aTypes[t]).IsForward
      and not (aTypes[t].Parent is TPasMembersType)
      and SameText(aTypes[t].Name, aName) then
    begin
      if Result <> nil then
        Exit(nil);
      Result := TPasClassType(aTypes[t]);
    end;
end;


// Appends aExpr and every expression below it to aList.
procedure CollectExprNodes(aExpr: TPasExpr; var aList: TPasExprArray);
var
  i: integer;
begin
  if aExpr = nil then
    Exit;
  SetLength(aList, Length(aList) + 1);
  aList[High(aList)] := aExpr;
  if aExpr is TBinaryExpr then
  begin
    CollectExprNodes(TBinaryExpr(aExpr).Left, aList);
    CollectExprNodes(TBinaryExpr(aExpr).Right, aList);
  end
  else if aExpr is TUnaryExpr then
    CollectExprNodes(TUnaryExpr(aExpr).Operand, aList)
  else if aExpr is TParamsExpr then
  begin
    CollectExprNodes(TParamsExpr(aExpr).Value, aList);
    for i := 0 to High(TParamsExpr(aExpr).Params) do
      CollectExprNodes(TParamsExpr(aExpr).Params[i], aList);
  end;
end;


// Every expression node reachable from the statements of aModule.
function AllExprs(aModule: TPasModule): TPasExprArray;
var
  lStmts: TPasImplElementArray;
  lOwned: TPasExprArray;
  i, j: integer;
begin
  SetLength(Result, 0);
  lStmts := AllStatements(aModule);
  for i := 0 to High(lStmts) do
  begin
    lOwned := StmtExprs(lStmts[i]);
    for j := 0 to High(lOwned) do
      CollectExprNodes(lOwned[j], Result);
  end;
end;


// The identifier an expression spells: the primitive itself, or the member part
// of a qualified name; '' for any other shape.
function IdentifierOf(aExpr: TPasExpr): string;
begin
  Result := '';
  if aExpr = nil then
    Exit;
  if (aExpr is TBinaryExpr) and (TBinaryExpr(aExpr).OpCode = eopSubIdent) then
    Exit(IdentifierOf(TBinaryExpr(aExpr).Right));
  if aExpr is TPrimitiveExpr then
    Result := TPrimitiveExpr(aExpr).Value;
end;


// The identifier of an unqualified name expression; '' for any other shape.
function PlainName(aExpr: TPasExpr): string;
begin
  if aExpr is TPrimitiveExpr then
    Result := TPrimitiveExpr(aExpr).Value
  else
    Result := '';
end;


// The callee identifier of a call expression; '' when aExpr is not a call.
function CalleeName(aExpr: TPasExpr): string;
begin
  Result := '';
  if (aExpr is TParamsExpr) and (TParamsExpr(aExpr).Kind = pekFuncParams) then
    Result := IdentifierOf(TParamsExpr(aExpr).Value);
end;


// True iff aExpr is an = or <> whose two operands both classify ltkClass, i.e.
// a class type that is not an interface; aTypeName is the left operand's name.
function IsClassIdentityComparison(const aResolver: TFpSonarResolver;
  aExpr: TPasExpr; out aTypeName: string): boolean;
var
  lBinary: TBinaryExpr;
  lLeft, lRight: TFpSonarResolvedType;
begin
  Result := False;
  aTypeName := '';
  if not (aExpr is TBinaryExpr) then
    Exit;
  lBinary := TBinaryExpr(aExpr);
  if not (lBinary.OpCode in [eopEqual, eopNotEqual]) then
    Exit;
  if not aResolver.TryResolvedType(lBinary.Left, lLeft) then
    Exit;
  if lLeft.Kind <> ltkClass then
    Exit;
  if not aResolver.TryResolvedType(lBinary.Right, lRight) then
    Exit;
  if lRight.Kind <> ltkClass then
    Exit;
  aTypeName := lLeft.TypeName;
  Result := True;
end;


{ TRuleInterfaceWithoutGuidUsedDynamically }

procedure TRuleInterfaceWithoutGuidUsedDynamically.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  lExprs: TPasExprArray;
  lCall: TParamsExpr;
  lIface: TPasClassType;
  lCallee: string;
  i, p: integer;

  // The GUID-less COM interface aName denotes in this module, or nil.
  function GuidlessCom(const aName: string): TPasClassType;
  var
    lFound: TPasClassType;
  begin
    Result := nil;
    lFound := FindInterfaceNamed(lTypes, aName);
    if (lFound <> nil) and (lFound.InterfaceType = citCom)
      and (NormalisedGuid(lFound) = '') then
      Result := lFound;
  end;

begin
  lTypes := EnumerateTypes(aContext.Module);
  lExprs := AllExprs(aContext.Module);
  for i := 0 to High(lExprs) do
  begin
    if (lExprs[i] is TBinaryExpr)
      and (TBinaryExpr(lExprs[i]).OpCode = eopAs) then
    begin
      lIface := GuidlessCom(PlainName(TBinaryExpr(lExprs[i]).Right));
      if lIface <> nil then
        EmitClass(FMetadata, aContext, aCollector,
          lExprs[i].SourceLinenumber, lIface.Name);
      Continue;
    end;
    lCallee := CalleeName(lExprs[i]);
    if not (SameText(lCallee, cSupportsRoutine)
      or SameText(lCallee, cQueryInterfaceRoutine)) then
      Continue;
    lCall := TParamsExpr(lExprs[i]);
    for p := 0 to High(lCall.Params) do
    begin
      lIface := GuidlessCom(PlainName(lCall.Params[p]));
      if lIface <> nil then
        EmitClass(FMetadata, aContext, aCollector,
          lCall.SourceLinenumber, lIface.Name);
    end;
  end;
end;


{ TRuleSupportsResultIgnored }

procedure TRuleSupportsResultIgnored.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lName: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplSimple then
    begin
      lName := CalleeName(TPasImplSimple(lStmts[i]).Expr);
      if SameText(lName, cSupportsRoutine) then
        EmitClass(FMetadata, aContext, aCollector,
          lStmts[i].SourceLinenumber, lName);
    end;
end;


{ TRuleClassHelperHidesAncestorMethod }

procedure TRuleClassHelperHidesAncestorMethod.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lNames: TFpSonarStringArray;
  i: integer;
begin
  if not aContext.Resolver.TryClassHelperHiddenMethods(lNodes, lNames) then
    Exit;
  for i := 0 to High(lNodes) do
    EmitClass(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lNodes[i]), lNames[i]);
end;


{ TRuleAssignedOnNonReference }

// The type of aRoutine's argument named aName; nil when it has none.
function ArgumentType(aRoutine: TPasProcedure; const aName: string): TPasType;
var
  i: integer;
begin
  Result := nil;
  if (aRoutine.ProcType = nil) or (aRoutine.ProcType.Args = nil) then
    Exit;
  for i := 0 to aRoutine.ProcType.Args.Count - 1 do
    if SameText(TPasArgument(aRoutine.ProcType.Args[i]).Name, aName) then
      Exit(TPasArgument(aRoutine.ProcType.Args[i]).ArgType);
end;


// The type of aRoutine's local variable named aName; nil when it has none.
function LocalType(aRoutine: TPasProcedure; const aName: string): TPasType;
var
  lDecl: TPasElement;
  i: integer;
begin
  Result := nil;
  if aRoutine.Body = nil then
    Exit;
  for i := 0 to aRoutine.Body.Declarations.Count - 1 do
  begin
    lDecl := TPasElement(aRoutine.Body.Declarations[i]);
    if SameText(lDecl.Name, aName) and (lDecl is TPasVariable)
      and not (lDecl is TPasProperty) then
      Exit(TPasVariable(lDecl).VarType);
  end;
end;


// True when aType is a value type: a record, an enumeration, a subrange or a
// static array.
function IsValueType(aType: TPasType): boolean;
begin
  Result := (aType is TPasRecordType) or (aType is TPasEnumType)
    or (aType is TPasRangeType)
    or ((aType is TPasArrayType) and (Length(TPasArrayType(aType).Ranges) > 0));
end;


procedure TRuleAssignedOnNonReference.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  lValues: TPasValueDeclArray;
  lRoutines: TAstRoutineArray;
  lStmts: TPasImplElementArray;
  lOwned, lExprs: TPasExprArray;
  lCall: TParamsExpr;
  lName: string;
  r, s, e, i: integer;

  // The single top-level type named aName; nil when none or more than one is.
  function TypeNamed(const aName: string): TPasType;
  var
    t: integer;
  begin
    Result := nil;
    for t := 0 to High(lTypes) do
      if not (lTypes[t].Parent is TPasMembersType)
        and SameText(lTypes[t].Name, aName) then
      begin
        if Result <> nil then
          Exit(nil);
        Result := lTypes[t];
      end;
  end;

  { The type aName is declared with, innermost scope first: aRoutine's
    arguments, then its locals, then the section variables and the fields of
    aRoutine's own class (the last only when the name is unique there). }
  function DeclaredType(aRoutine: TPasProcedure; const aName: string): TPasType;
  var
    lOwner: TPasClassType;
    lHits, v: integer;
  begin
    Result := ArgumentType(aRoutine, aName);
    if Result = nil then
      Result := LocalType(aRoutine, aName);
    if Result <> nil then
      Exit;
    if aRoutine.Parent is TProcedureBody then
      Exit(nil);
    lOwner := FindClassNamed(lTypes, ClassQualifier(aRoutine.Name));
    lHits := 0;
    for v := 0 to High(lValues) do
      if SameText(lValues[v].Decl.Name, aName)
        and (not (lValues[v].Decl.Parent is TPasMembersType)
        or (lValues[v].Decl.Parent = lOwner)) then
      begin
        Inc(lHits);
        Result := lValues[v].Decl.VarType;
      end;
    if lHits <> 1 then
      Result := nil;
  end;

  { True when aName is declared in this module with a value type. A named type
    is looked up among the module's top-level types; an alias, an off-file type
    and an unknown name all answer False. }
  function NamesValueTyped(aRoutine: TPasProcedure;
    const aName: string): boolean;
  var
    lType: TPasType;
  begin
    Result := False;
    lType := DeclaredType(aRoutine, aName);
    if lType = nil then
      Exit;
    if IsValueType(lType) then
      Exit(True);
    if lType is TPasUnresolvedTypeRef then
      Result := IsValueType(TypeNamed(lType.Name));
  end;

begin
  lTypes := EnumerateTypes(aContext.Module);
  lValues := EnumerateValueDecls(aContext.Module);
  lRoutines := EnumerateRoutines(aContext.Module);
  for r := 0 to High(lRoutines) do
  begin
    SetLength(lExprs, 0);
    SetLength(lStmts, 0);
    CollectStatements(lRoutines[r].Block, lStmts);
    for s := 0 to High(lStmts) do
    begin
      lOwned := StmtExprs(lStmts[s]);
      for e := 0 to High(lOwned) do
        CollectExprNodes(lOwned[e], lExprs);
    end;
    for i := 0 to High(lExprs) do
    begin
      if not SameText(CalleeName(lExprs[i]), cAssignedRoutine) then
        Continue;
      lCall := TParamsExpr(lExprs[i]);
      if Length(lCall.Params) <> 1 then
        Continue;
      if not (lCall.Params[0] is TPrimitiveExpr) then
        Continue;
      lName := TPrimitiveExpr(lCall.Params[0]).Value;
      if NamesValueTyped(lRoutines[r].Decl, lName) then
        EmitClass(FMetadata, aContext, aCollector,
          lCall.SourceLinenumber, lName);
    end;
  end;
end;


{ TRulePublicFieldAndPropertyForSameStorage }

procedure TRulePublicFieldAndPropertyForSameStorage.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lNames: TFpSonarStringArray;
  i: integer;
begin
  if not aContext.Resolver.TryPublicFieldBackedProperties(lNodes, lNames) then
    Exit;
  for i := 0 to High(lNodes) do
    EmitClass(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lNodes[i]), lNames[i]);
end;


{ TRulePropertyAccessorVisibilityWiderThanProperty }

procedure TRulePropertyAccessorVisibilityWiderThanProperty.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lNames: TFpSonarStringArray;
  i: integer;
begin
  if not aContext.Resolver.TryWiderAccessorProperties(lNodes, lNames) then
    Exit;
  for i := 0 to High(lNodes) do
    EmitClass(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lNodes[i]), lNames[i]);
end;


{ TRulePropertyGetterWithSideEffect }

procedure TRulePropertyGetterWithSideEffect.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lNames: TFpSonarStringArray;
  i: integer;
begin
  if not aContext.Resolver.TryPropertyGetterFieldWrites(lNodes, lNames) then
    Exit;
  for i := 0 to High(lNodes) do
    EmitClass(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lNodes[i]), lNames[i]);
end;


{ TRuleConstructorNotVirtualInPolymorphicHierarchy }

procedure TRuleConstructorNotVirtualInPolymorphicHierarchy.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lNames: TFpSonarStringArray;
  i: integer;
begin
  if not aContext.Resolver.TryHiddenNonVirtualConstructors(lNodes, lNames) then
    Exit;
  for i := 0 to High(lNodes) do
    EmitClass(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lNodes[i]), lNames[i]);
end;


{ TRuleInheritedCreateNotFirstStatement }

procedure TRuleInheritedCreateNotFirstStatement.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  r: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Module);
  for r := 0 to High(lRoutines) do
    // A body with no inherited at all is ConstructorInherited's; a class
    // constructor is excluded.
    if (lRoutines[r].Decl is TPasConstructor)
      and not (lRoutines[r].Decl is TPasClassConstructor)
      and StmtHasInherited(lRoutines[r].Block)
      and not StmtIsInheritedCall(
      EdgeStatement(lRoutines[r].Block, True)) then
      EmitClass(FMetadata, aContext, aCollector,
        lRoutines[r].Decl.SourceLinenumber, lRoutines[r].Decl.Name);
end;


{ TRuleInheritedDestroyNotLastStatement }

procedure TRuleInheritedDestroyNotLastStatement.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  r: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Module);
  for r := 0 to High(lRoutines) do
    // A body with no inherited at all is DestructorInherited's; a class
    // destructor is excluded.
    if (lRoutines[r].Decl is TPasDestructor)
      and not (lRoutines[r].Decl is TPasClassDestructor)
      and StmtHasInherited(lRoutines[r].Block)
      and not StmtIsInheritedCall(
      EdgeStatement(lRoutines[r].Block, False)) then
      EmitClass(FMetadata, aContext, aCollector,
        lRoutines[r].Decl.SourceLinenumber, lRoutines[r].Decl.Name);
end;


{ TRuleComparingClassReferencesWithEquals }

procedure TRuleComparingClassReferencesWithEquals.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lTypeName: string;
  i: integer;
begin
  lExprs := AllExprs(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if IsClassIdentityComparison(aContext.Resolver, lExprs[i], lTypeName) then
      EmitClass(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), lTypeName);
end;


// Builds the FileNotTooManyClasses metadata declaring its integer 'maxClasses'
// threshold param.
function FileNotTooManyClassesMeta: TRuleMetadata;
begin
  Result := TRuleMetadata.Make('FileNotTooManyClasses', rtAst, rfAst, sevMinor,
    itCodeSmell, cfHigh, True, cKeyFileNotTooManyClasses);
  Result.AddParam('maxClasses', rpkInt, cMaxClasses);
  Result.Description :=
    'Flags a unit that declares more classes than the configured maximum.';
end;


initialization
  RegisterRule(TRuleVisibilityAscendingOrder.Create(TRuleMetadata.Make(
    'VisibilityAscendingOrder', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyVisibilityAscendingOrder).WithDescription(
    'Flags a visibility section that is out of ascending (private..published) order.')));
  RegisterMessage(cKeyVisibilityAscendingOrder, SVisibilityAscendingOrder);

  RegisterRule(TRuleDeclarationsFollowVisibilityOrder.Create(TRuleMetadata.Make(
    'DeclarationsFollowVisibilityOrder', rtAst, rfAst, sevMinor, itCodeSmell,
    cfHigh, True, cKeyDeclarationsFollowVisibilityOrder).WithDescription(
    'Flags declarations that do not follow the field, then method, then property order.')));
  RegisterMessage(cKeyDeclarationsFollowVisibilityOrder,
    SDeclarationsFollowVisibilityOrder);

  RegisterRule(TRuleFieldsNotPublic.Create(TRuleMetadata.Make(
    'FieldsNotPublic', rtAst, rfAst, sevMajor, itCodeSmell, cfHigh,
    True, cKeyFieldsNotPublic).WithDescription(
    'Flags a public field; expose state through a property over a private field instead.')));
  RegisterMessage(cKeyFieldsNotPublic, SFieldsNotPublic);

  RegisterRule(TRuleFileNotTooManyClasses.Create(
    FileNotTooManyClassesMeta));
  RegisterMessage(cKeyFileNotTooManyClasses, SFileNotTooManyClasses);

  RegisterRule(TRuleInterfaceNotEmpty.Create(TRuleMetadata.Make(
    'InterfaceNotEmpty', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyInterfaceNotEmpty).WithDescription(
    'Flags an interface that declares no methods or properties.')));
  RegisterMessage(cKeyInterfaceNotEmpty, SInterfaceNotEmpty);

  RegisterRule(TRuleInterfaceUniqueGuid.Create(TRuleMetadata.Make(
    'InterfaceUniqueGuid', rtAst, rfAst, sevMajor, itBug, cfHigh,
    True, cKeyInterfaceUniqueGuid).WithDescription(
    'Flags an interface without a unique GUID.')));
  RegisterMessage(cKeyInterfaceUniqueGuid, SInterfaceUniqueGuid);

  RegisterRule(TRuleConstructorInherited.Create(TRuleMetadata.Make(
    'ConstructorInherited', rtAst, rfAst, sevMajor, itBug, cfHigh,
    True, cKeyConstructorInherited).WithDescription(
    'Flags a constructor that does not call its inherited constructor.')));
  RegisterMessage(cKeyConstructorInherited, SConstructorInherited);

  RegisterRule(TRuleDestructorInherited.Create(TRuleMetadata.Make(
    'DestructorInherited', rtAst, rfAst, sevMajor, itBug, cfHigh,
    True, cKeyDestructorInherited).WithDescription(
    'Flags a destructor that does not call its inherited destructor.')));
  RegisterMessage(cKeyDestructorInherited, SDestructorInherited);

  RegisterRule(TRuleTopLevelClassInheritsTObject.Create(TRuleMetadata.Make(
    'TopLevelClassInheritsTObject', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyTopLevelClassInheritsTObject).WithDescription(
    'Flags a class that does not explicitly declare an ancestor.')));
  RegisterMessage(cKeyTopLevelClassInheritsTObject, STopLevelClassInheritsTObject);

  RegisterRule(TRuleMethodHidesVirtualWithoutOverride.Create(TRuleMetadata.Make(
    'MethodHidesVirtualWithoutOverride', rtSem, rfResolver, sevMajor, itBug,
    cfHigh, False, cKeyMethodHidesVirtualWithoutOverride).WithDescription(
    'Flags a method that hides an overridable ancestor method without override, reintroduce or overload.')));
  RegisterMessage(cKeyMethodHidesVirtualWithoutOverride,
    SMethodHidesVirtualWithoutOverride);

  RegisterRule(TRuleOverrideChangesDefaultParameterValue.Create(
    TRuleMetadata.Make('OverrideChangesDefaultParameterValue', rtSem,
    rfResolver, sevMajor, itBug, cfHigh, False,
    cKeyOverrideChangesDefaultParameterValue).WithDescription(
    'Flags an override that declares a different default parameter value than the method it overrides.')));
  RegisterMessage(cKeyOverrideChangesDefaultParameterValue,
    SOverrideChangesDefaultParameterValue);

  RegisterRule(TRuleAbstractMethodCalledDirectly.Create(TRuleMetadata.Make(
    'AbstractMethodCalledDirectly', rtAst, rfAst, sevMajor, itBug, cfMedium,
    False, cKeyAbstractMethodCalledDirectly).WithDescription(
    'Flags an inherited call to a method the ancestor declares abstract.')));
  RegisterMessage(cKeyAbstractMethodCalledDirectly,
    SAbstractMethodCalledDirectly);

  RegisterRule(TRuleInstantiatesClassWithAbstractMethods.Create(
    TRuleMetadata.Make('InstantiatesClassWithAbstractMethods', rtSem,
    rfResolver, sevMajor, itBug, cfHigh, False,
    cKeyInstantiatesClassWithAbstractMethods).WithDescription(
    'Flags a named class constructed while it still has an unimplemented abstract method.')));
  RegisterMessage(cKeyInstantiatesClassWithAbstractMethods,
    SInstantiatesClassWithAbstractMethods);

  RegisterRule(TRuleInterfaceWithoutGuidUsedDynamically.Create(
    TRuleMetadata.Make('InterfaceWithoutGuidUsedDynamically', rtAst, rfAst,
    sevMajor, itBug, cfMedium, False,
    cKeyInterfaceWithoutGuidUsedDynamically).WithDescription(
    'Flags a dynamic interface query naming a COM interface that declares no GUID.')));
  RegisterMessage(cKeyInterfaceWithoutGuidUsedDynamically,
    SInterfaceWithoutGuidUsedDynamically);

  RegisterRule(TRuleSupportsResultIgnored.Create(TRuleMetadata.Make(
    'SupportsResultIgnored', rtAst, rfAst, sevMajor, itBug, cfMedium,
    False, cKeySupportsResultIgnored).WithDescription(
    'Flags a Supports call written as a statement, so its boolean result is discarded.')));
  RegisterMessage(cKeySupportsResultIgnored, SSupportsResultIgnored);

  RegisterRule(TRuleClassHelperHidesAncestorMethod.Create(TRuleMetadata.Make(
    'ClassHelperHidesAncestorMethod', rtSem, rfResolver, sevMajor, itCodeSmell,
    cfHigh, False, cKeyClassHelperHidesAncestorMethod).WithDescription(
    'Flags a class-helper method whose name the extended type or one of its ancestors already declares.')));
  RegisterMessage(cKeyClassHelperHidesAncestorMethod,
    SClassHelperHidesAncestorMethod);

  RegisterRule(TRuleAssignedOnNonReference.Create(TRuleMetadata.Make(
    'AssignedOnNonReference', rtAst, rfAst, sevMajor, itBug, cfMedium,
    False, cKeyAssignedOnNonReference).WithDescription(
    'Flags Assigned applied to an operand whose declared type is not a reference type.')));
  RegisterMessage(cKeyAssignedOnNonReference, SAssignedOnNonReference);

  RegisterRule(TRulePublicFieldAndPropertyForSameStorage.Create(
    TRuleMetadata.Make('PublicFieldAndPropertyForSameStorage', rtSem,
    rfResolver, sevMajor, itCodeSmell, cfHigh, False,
    cKeyPublicFieldAndPropertyForSameStorage).WithDescription(
    'Flags a property whose accessor is a public or published field of the same class.')));
  RegisterMessage(cKeyPublicFieldAndPropertyForSameStorage,
    SPublicFieldAndPropertyForSameStorage);

  RegisterRule(TRulePropertyAccessorVisibilityWiderThanProperty.Create(
    TRuleMetadata.Make('PropertyAccessorVisibilityWiderThanProperty', rtSem,
    rfResolver, sevMajor, itCodeSmell, cfHigh, False,
    cKeyPropertyAccessorVisibilityWiderThanProperty).WithDescription(
    'Flags a property whose read or write accessor routine is declared more visible than the property.')));
  RegisterMessage(cKeyPropertyAccessorVisibilityWiderThanProperty,
    SPropertyAccessorVisibilityWiderThanProperty);

  RegisterRule(TRulePropertyGetterWithSideEffect.Create(TRuleMetadata.Make(
    'PropertyGetterWithSideEffect', rtSem, rfResolver, sevMajor, itCodeSmell,
    cfLow, False, cKeyPropertyGetterWithSideEffect).WithDescription(
    'Flags a property whose read accessor writes to a field of the type that declares it.')));
  RegisterMessage(cKeyPropertyGetterWithSideEffect,
    SPropertyGetterWithSideEffect);

  RegisterRule(TRuleConstructorNotVirtualInPolymorphicHierarchy.Create(
    TRuleMetadata.Make('ConstructorNotVirtualInPolymorphicHierarchy', rtSem,
    rfResolver, sevMajor, itCodeSmell, cfMedium, False,
    cKeyConstructorNotVirtualInPolymorphicHierarchy).WithDescription(
    'Flags a non-virtual constructor of a class with virtual methods that a descendant redeclares.')));
  RegisterMessage(cKeyConstructorNotVirtualInPolymorphicHierarchy,
    SConstructorNotVirtualInPolymorphicHierarchy);

  RegisterRule(TRuleInheritedCreateNotFirstStatement.Create(
    TRuleMetadata.Make('InheritedCreateNotFirstStatement', rtAst, rfAst,
    sevMajor, itBug, cfMedium, False,
    cKeyInheritedCreateNotFirstStatement).WithDescription(
    'Flags a constructor that touches state before chaining to its inherited constructor.')));
  RegisterMessage(cKeyInheritedCreateNotFirstStatement,
    SInheritedCreateNotFirstStatement);

  RegisterRule(TRuleInheritedDestroyNotLastStatement.Create(
    TRuleMetadata.Make('InheritedDestroyNotLastStatement', rtAst, rfAst,
    sevMajor, itBug, cfMedium, False,
    cKeyInheritedDestroyNotLastStatement).WithDescription(
    'Flags a destructor that runs a statement after chaining to its inherited destructor.')));
  RegisterMessage(cKeyInheritedDestroyNotLastStatement,
    SInheritedDestroyNotLastStatement);

  RegisterRule(TRuleComparingClassReferencesWithEquals.Create(
    TRuleMetadata.Make('ComparingClassReferencesWithEquals', rtSem, rfResolver,
    sevMinor, itCodeSmell, cfLow, False,
    cKeyComparingClassReferencesWithEquals).WithDescription(
    'Flags an = or <> comparison of two operands of a non-interface class type, which compares references.')));
  RegisterMessage(cKeyComparingClassReferencesWithEquals,
    SComparingClassReferencesWithEquals);

end.
