{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Rules over generic declarations, specializations and anonymous methods

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Generics;


{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  Pascal.Tree,
{$ELSE}
  PasTree,
{$ENDIF}
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework;

type
  { Flags a template parameter whose declared constraint the generic never
    relies on. Polarity: positive. }
  TRuleGenericConstraintUnused = class(TRuleBase)
  public
    // Emits one issue per idle constraint, at that parameter's row.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a specialization of a generic that constrains none of its template
    parameters. Polarity: positive. }
  TRuleSpecializationOfUnconstrainedGeneric = class(TRuleBase)
  public
    // Emits one issue per offending specialization, at its own row.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a specialization nesting more specialization levels than the
    configured maximum. Polarity: positive. }
  TRuleNestedGenericSpecializationDepth = class(TRuleBase)
  public
    // Emits one issue per outermost offending specialization, at its own row.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags an anonymous method capturing the control variable of an enclosing
    classic for loop. Polarity: positive. }
  TRuleAnonymousMethodCapturesLoopVariable = class(TRuleBase)
  public
    // Emits one issue per captured declaration, at its first reference's row.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags an anonymous method whose body reaches the enclosing instance.
    Polarity: positive. }
  TRuleAnonymousMethodCapturesSelf = class(TRuleBase)
  public
    // Emits one issue per capturing anonymous method, at its own row.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags an attribute on a class member that no RTTI reaches.
    Polarity: positive. }
  TRuleAttributeOnNonRttiMember = class(TRuleBase)
  public
    // Emits one issue per unreachable bracket group, at that group's row.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  FpSonar.Config, FpSonar.Resolver, FpSonar.Traversal, FpSonar.Rules.Consts;

const
  cConstraintUnusedId = 'GenericConstraintUnused';
  cUnconstrainedSpecId = 'SpecializationOfUnconstrainedGeneric';
  cNestingDepthId = 'NestedGenericSpecializationDepth';
  cKeyConstraintUnused = 'rule.GenericConstraintUnused.message';
  cKeyUnconstrainedSpec = 'rule.SpecializationOfUnconstrainedGeneric.message';
  cKeyNestingDepth = 'rule.NestedGenericSpecializationDepth.message';
  cCapturesLoopVarId = 'AnonymousMethodCapturesLoopVariable';
  cCapturesSelfId = 'AnonymousMethodCapturesSelf';
  cKeyCapturesLoopVar = 'rule.AnonymousMethodCapturesLoopVariable.message';
  cKeyCapturesSelf = 'rule.AnonymousMethodCapturesSelf.message';
  cAttributeNonRttiId = 'AttributeOnNonRttiMember';
  cKeyAttributeNonRtti = 'rule.AttributeOnNonRttiMember.message';

  cMaxDepthParam = 'maxDepth';
  cDefaultMaxDepth = 3;

// Emits one issue at aLine, column 1.
procedure EmitAstIssue(const aMeta: TRuleMetadata;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector;
  aLine: integer; const aArg: string);
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aLine, 1, aLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    [aArg], aArg);
end;


// Emits one issue at aNode's resolved row, column 1.
procedure EmitGenericIssue(const aMeta: TRuleMetadata;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector;
  aNode: TPasElement; const aArgs: array of string; const aSnippet: string);
var
  lLine: integer;
begin
  lLine := aContext.Resolver.SourceRow(aNode);
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, lLine, 1, lLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    aArgs, aSnippet);
end;


// The generic a specialization node targets, '' when it did not resolve.
function SpecializedName(aNode: TPasElement): string;
begin
  Result := '';
  if (aNode is TPasSpecializeType)
    and (TPasSpecializeType(aNode).DestType <> nil) then
    Result := TPasSpecializeType(aNode).DestType.Name;
end;


// The source spelling of a capturing reference node, '' when it is not one.
function CapturedName(aNode: TPasElement): string;
begin
  Result := '';
  if aNode is TPrimitiveExpr then
    Result := TPrimitiveExpr(aNode).Value;
end;


// The name of the first routine enclosing aNode that is not itself anonymous.
function EnclosingRoutineName(aNode: TPasElement): string;
var
  lEl: TPasElement;
begin
  Result := '';
  lEl := aNode;
  while lEl <> nil do
  begin
    if (lEl is TPasProcedure) and not (lEl is TPasAnonymousProcedure) then
      Exit(lEl.Name);
    lEl := lEl.Parent;
  end;
end;


{ The member the attribute node at aIndex decorates: the next member of aClass
  that is not itself an attribute node, nil when the class ends first. }
function DecoratedMember(aClass: TPasClassType; aIndex: integer): TPasElement;
var
  i: integer;
begin
  Result := nil;
  for i := aIndex + 1 to aClass.Members.Count - 1 do
    if not (TPasElement(aClass.Members[i]) is TPasAttributes) then
      Exit(TPasElement(aClass.Members[i]));
end;


// Whether aMember is one of the three member kinds HasExtRTTI decides.
function HasRttiJudgeableKind(aMember: TPasElement): boolean;
begin
  Result := (aMember is TPasProperty) or (aMember is TPasProcedure)
    or ((aMember is TPasVariable) and not (aMember is TPasConst));
end;


{ Whether aMember of aClass is a member kind RTTI knows and is provably outside
  it. visDefault is the implicit section, which {$M+} may publish, so it is
  never judged. }
function IsOutsideRtti(aClass: TPasClassType; aMember: TPasElement): boolean;
begin
  Result := (aMember <> nil) and HasRttiJudgeableKind(aMember)
    and (aMember.Visibility <> visDefault) and not aClass.HasExtRTTI(aMember);
end;


{ TRuleGenericConstraintUnused }

procedure TRuleGenericConstraintUnused.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  i: integer;
begin
  // The wrapper returns every idle template parameter (or False to degrade).
  if not aContext.Resolver.TryUnusedGenericConstraints(lNodes) then
    Exit;
  for i := 0 to High(lNodes) do
    EmitGenericIssue(FMetadata, aContext, aCollector, lNodes[i],
      [lNodes[i].Name], lNodes[i].Name);
end;


{ TRuleSpecializationOfUnconstrainedGeneric }

procedure TRuleSpecializationOfUnconstrainedGeneric.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lName: string;
  i: integer;
begin
  if not aContext.Resolver.TryUnconstrainedSpecializations(lNodes) then
    Exit;
  for i := 0 to High(lNodes) do
  begin
    lName := SpecializedName(lNodes[i]);
    if lName <> '' then
      EmitGenericIssue(FMetadata, aContext, aCollector, lNodes[i], [lName],
        lName);
  end;
end;


{ TRuleNestedGenericSpecializationDepth }

procedure TRuleNestedGenericSpecializationDepth.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lDepths: TFpSonarDepthArray;
  lName: string;
  lMax, i: integer;
begin
  lMax := aContext.Config.RuleParamInt(FMetadata.RuleId, cMaxDepthParam,
    cDefaultMaxDepth);
  if not aContext.Resolver.TryDeepNestedSpecializations(lMax, lNodes,
    lDepths) then
    Exit;
  for i := 0 to High(lNodes) do
  begin
    lName := SpecializedName(lNodes[i]);
    if lName <> '' then
      EmitGenericIssue(FMetadata, aContext, aCollector, lNodes[i],
        [lName, IntToStr(lDepths[i]), IntToStr(lMax)], lName);
  end;
end;


{ TRuleAnonymousMethodCapturesLoopVariable }

procedure TRuleAnonymousMethodCapturesLoopVariable.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lName: string;
  i: integer;
begin
  if not aContext.Resolver.TryAnonMethodLoopVarCaptures(lNodes) then
    Exit;
  for i := 0 to High(lNodes) do
  begin
    lName := CapturedName(lNodes[i]);
    if lName <> '' then
      EmitGenericIssue(FMetadata, aContext, aCollector, lNodes[i], [lName],
        lName);
  end;
end;


{ TRuleAnonymousMethodCapturesSelf }

procedure TRuleAnonymousMethodCapturesSelf.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lName: string;
  i: integer;
begin
  if not aContext.Resolver.TryAnonMethodSelfCaptures(lNodes) then
    Exit;
  for i := 0 to High(lNodes) do
  begin
    lName := EnclosingRoutineName(lNodes[i]);
    if lName <> '' then
      EmitGenericIssue(FMetadata, aContext, aCollector, lNodes[i], [lName],
        lName);
  end;
end;


{ TRuleAttributeOnNonRttiMember }

procedure TRuleAttributeOnNonRttiMember.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  lClass: TPasClassType;
  lAttrs: TPasElement;
  lMember: TPasElement;
  t, m: integer;
begin
  lTypes := EnumerateTypes(aContext.Module);
  for t := 0 to High(lTypes) do
    if (lTypes[t] is TPasClassType)
      and (TPasClassType(lTypes[t]).ObjKind = okClass) then
    begin
      lClass := TPasClassType(lTypes[t]);
      for m := 0 to lClass.Members.Count - 1 do
      begin
        lAttrs := TPasElement(lClass.Members[m]);
        if not (lAttrs is TPasAttributes) then
          Continue;
        lMember := DecoratedMember(lClass, m);
        if IsOutsideRtti(lClass, lMember) then
          EmitAstIssue(FMetadata, aContext, aCollector,
            lAttrs.SourceLinenumber, lMember.Name);
      end;
    end;
end;


var
  lMeta: TRuleMetadata;

initialization
  RegisterRule(TRuleGenericConstraintUnused.Create(TRuleMetadata.Make(
    cConstraintUnusedId, rtSem, rfResolver, sevMinor, itCodeSmell,
    cfLow, False, cKeyConstraintUnused).WithDescription(
    'Flags a generic template parameter carrying a declared type constraint '
    + 'that none of the implemented routines of the generic rely on.')));
  RegisterMessage(cKeyConstraintUnused, SGenericConstraintUnused);


  RegisterRule(TRuleSpecializationOfUnconstrainedGeneric.Create(
    TRuleMetadata.Make(cUnconstrainedSpecId, rtSem, rfResolver, sevInfo,
    itCodeSmell, cfMedium, False, cKeyUnconstrainedSpec).WithDescription(
    'Flags a declaration-site specialization of a generic that declares '
    + 'template parameters and constrains none of them.')));
  RegisterMessage(cKeyUnconstrainedSpec, SSpecializationOfUnconstrainedGeneric);


  // maxDepth declared AFTER Make, which resets ParamSpecs.
  lMeta := TRuleMetadata.Make(cNestingDepthId, rtSem, rfResolver, sevMinor,
    itCodeSmell, cfMedium, False, cKeyNestingDepth).WithDescription(
    'Flags a declaration-site specialization nesting more levels of generic '
    + 'specialization than the configured maximum.');
  lMeta.AddParam(cMaxDepthParam, rpkInt, cDefaultMaxDepth);
  RegisterRule(TRuleNestedGenericSpecializationDepth.Create(lMeta));
  RegisterMessage(cKeyNestingDepth, SNestedGenericSpecializationDepth);


  RegisterRule(TRuleAnonymousMethodCapturesLoopVariable.Create(
    TRuleMetadata.Make(cCapturesLoopVarId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, cKeyCapturesLoopVar).WithDescription(
    'Flags an anonymous method that captures the control variable of an '
    + 'enclosing for loop declaring it outside the loop, so every closure the '
    + 'loop creates observes the one variable the iterations share.')));
  RegisterMessage(cKeyCapturesLoopVar, SAnonymousMethodCapturesLoopVariable);


  RegisterRule(TRuleAnonymousMethodCapturesSelf.Create(TRuleMetadata.Make(
    cCapturesSelfId, rtSem, rfResolver, sevInfo, itCodeSmell, cfLow, False,
    cKeyCapturesSelf).WithDescription(
    'Flags an anonymous method whose body reaches the enclosing instance, '
    + 'through Self or through an instance member named without a qualifier, '
    + 'so the closure captures Self.')));
  RegisterMessage(cKeyCapturesSelf, SAnonymousMethodCapturesSelf);


  RegisterRule(TRuleAttributeOnNonRttiMember.Create(TRuleMetadata.Make(
    cAttributeNonRttiId, rtAst, rfAst, sevMinor, itCodeSmell, cfMedium, False,
    cKeyAttributeNonRtti).WithDescription(
    'Flags an attribute written on a class field, method or property that '
    + 'sits outside published, so no RTTI can reach the annotation. Members '
    + 'of the implicit first section are never judged.')));
  RegisterMessage(cKeyAttributeNonRtti, SAttributeOnNonRttiMember);

end.
