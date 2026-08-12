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

{ Class-hygiene AST rules (rtAst / rfAst):
  visibility order, member declaration order, non-public fields, empty / unique-GUID interfaces,
  constructor/destructor inheritance, TObject ancestry, per-file class count. }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, Pascal.Tree,
{$ELSE}
  SysUtils, PasTree,
{$ENDIF}
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Traversal,
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

  // Thresholds/params as named constants.
  cMaxClasses = 5;        // More than this many classes -> flag.
  cAllowMarker = False;   // Marker (empty) interfaces ARE flagged.
  cRequireGuid = True;    // A COM interface missing a GUID is flagged.

  // The two interface object kinds (InterfaceNotEmpty/InterfaceUniqueGuid accept both).
  cInterfaceKinds = [okInterface, okDispInterface];


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

end.
