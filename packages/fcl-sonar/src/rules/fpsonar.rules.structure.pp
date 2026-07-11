{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Structural AST analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Structure;

{ Structure AST rules (rtAst / rfAst):
  structural, volume and count checks on units, routines, blocks and statements:
  begin/end required, no goto / with / inline-asm / self-assignment, empty blocks,
  redundant jumps and boolean
  literals, inline-var type inference,
  project-file constraints and related. }

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
  { reports a routine whose cyclomatic complexity exceeds the limit. }
  TRuleCyclomaticComplexity = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a routine whose cognitive complexity exceeds the limit. }
  TRuleCognitiveComplexity = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a routine with more statements than the limit. }
  TRuleRoutineTooLarge = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a routine whose control nesting is deeper than the limit. }
  TRuleRoutineTooDeeplyNested = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a routine with more directly-nested routines than the limit. }
  TRuleTooManyNestedRoutines = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a routine with more parameters than the limit. }
  TRuleTooManyParameters = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a routine with more local variables than the limit. }
  TRuleTooManyVariables = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a routine with more default parameters than the limit. }
  TRuleTooManyDefaultParameters = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports an if/else/while/for/with body that is not a begin..end block. }
  TRuleBeginEndRequired = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports every goto statement. }
  TRuleNoGoto = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports every with statement. }
  TRuleNoWith = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a direct self-assignment (x := x, a.b := a.b). }
  TRuleNoSelfAssignment = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports every inline asm..end block. }
  TRuleNoInlineAssembly = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a case statement with fewer than two branches. }
  TRuleCaseAtLeastTwoItems = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports an empty begin..end block used below a routine root. }
  TRuleNoEmptyBlock = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a routine whose begin..end body has no statements. }
  TRuleRoutineNotEmpty = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a unit with no declarations or statements. }
  TRuleUnitNotEmpty = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports an unconditional trailing Exit/Continue jump. }
  TRuleRedundantJump = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a function declared without an explicit result type. }
  TRuleFunctionReturnTypeRequired = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports an =/<> comparison carrying a redundant boolean literal. }
  TRuleRedundantBooleanLiteral = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports an =/<> comparison against nil (should use Assigned). }
  TRuleNilCheckViaAssigned = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a type declared with the legacy `object` keyword. }
  TRuleNoObjectTypes = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a unit using a legacy begin..end. body instead of initialization. }
  TRuleNoLegacyInitializationSection = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an inline `const` without an explicit type. INERT (see Apply). }
  TRuleInlineConstNoTypeInference = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a `for var i := ...` whose inline loop variable has no type. }
  TRuleInlineLoopVarNoTypeInference = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an inline `var x := expr;` without an explicit type. }
  TRuleInlineVarNoTypeInference = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a routine declared in a program (project) file. }
  TRuleProjectFileNoRoutines = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a global variable declared in a program (project) file. }
  TRuleProjectFileNoVariables = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
{$ELSE}
  Classes, SysUtils,
{$ENDIF}
  FpSonar.Ingest, FpSonar.Config;

const
  // Thresholds (a routine is flagged strictly when its metric is GREATER THAN
  // the limit).
  cMaxCyclomatic = 10;
  cMaxCognitive = 15;
  cMaxStatements = 60;
  cMaxNestingDepth = 4;

  // Count thresholds (same convention as above; a routine is flagged strictly
  // when its count is GREATER THAN the limit).
  cMaxNestedRoutines = 3;
  cMaxParameters = 7;
  cMaxVariables = 15;
  cMaxDefaultParameters = 3;

  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyCyclomatic = 'rule.CyclomaticComplexity.message';
  cKeyCognitive = 'rule.CognitiveComplexity.message';
  cKeyRoutineTooLarge = 'rule.RoutineTooLarge.message';
  cKeyRoutineTooDeeplyNested = 'rule.RoutineTooDeeplyNested.message';

  // Count message keys.
  cKeyTooManyNestedRoutines = 'rule.TooManyNestedRoutines.message';
  cKeyTooManyParameters = 'rule.TooManyParameters.message';
  cKeyTooManyVariables = 'rule.TooManyVariables.message';
  cKeyTooManyDefaultParameters = 'rule.TooManyDefaultParameters.message';

  // Statement & control message keys.
  cKeyBeginEndRequired = 'rule.BeginEndRequired.message';
  cKeyNoGoto = 'rule.NoGoto.message';
  cKeyNoWith = 'rule.NoWith.message';
  cKeyNoInlineAssembly = 'rule.NoInlineAssembly.message';
  cKeyCaseAtLeastTwoItems = 'rule.CaseAtLeastTwoItems.message';

  // Statement, block & emptiness message keys.
  cKeyNoEmptyBlock = 'rule.NoEmptyBlock.message';
  cKeyRoutineNotEmpty = 'rule.RoutineNotEmpty.message';
  cKeyUnitNotEmpty = 'rule.UnitNotEmpty.message';
  cKeyRedundantJump = 'rule.RedundantJump.message';
  cKeyFunctionReturnTypeRequired = 'rule.FunctionReturnTypeRequired.message';

  // Statement-expression & legacy-construct message keys.
  cKeyRedundantBooleanLiteral = 'rule.RedundantBooleanLiteral.message';
  cKeyNilCheckViaAssigned = 'rule.NilCheckViaAssigned.message';
  cKeyNoObjectTypes = 'rule.NoObjectTypes.message';
  cKeyNoLegacyInitializationSection = 'rule.NoLegacyInitializationSection.message';

  // Self-assignment message key.
  cKeyNoSelfAssignment = 'rule.NoSelfAssignment.message';

  // Inline-declaration & project-file message keys.
  cKeyInlineConstNoTypeInference = 'rule.InlineConstNoTypeInference.message';
  cKeyInlineLoopVarNoTypeInference = 'rule.InlineLoopVarNoTypeInference.message';
  cKeyInlineVarNoTypeInference = 'rule.InlineVarNoTypeInference.message';
  cKeyProjectFileNoRoutines = 'rule.ProjectFileNoRoutines.message';
  cKeyProjectFileNoVariables = 'rule.ProjectFileNoVariables.message';

  { shared expression-level boolean-operator counting }

// Counts every 'and' / 'or' (eopAnd / eopOr) in the boolean-operator tree of aExpr
function CountAndOr(aExpr: TPasExpr): integer;
begin
  Result := 0;
  if aExpr = nil then
    Exit;
  if aExpr is TBinaryExpr then
  begin
    if TBinaryExpr(aExpr).OpCode in [eopAnd, eopOr] then
      Inc(Result);
    Result := Result + CountAndOr(TBinaryExpr(aExpr).Left)
      + CountAndOr(TBinaryExpr(aExpr).Right);
  end
  else if aExpr is TUnaryExpr then
    Result := Result + CountAndOr(TUnaryExpr(aExpr).Operand);
end;


{ Counts runs of like boolean operators in aExpr:
  a new run is counted whenever an eopAnd/eopOr node's operator differs from the boolean
  operator of its parent (aParentOp; eopNone at the top).
  'a and b' => 1;
  'a and b or c' => 2.
}
function CountBoolRuns(aExpr: TPasExpr; aParentOp: TExprOpCode): integer;
var
  lOp, lNextParent: TExprOpCode;
  lIsBool: boolean;
begin
  Result := 0;
  if aExpr = nil then
    Exit;
  if aExpr is TBinaryExpr then
  begin
    lOp := TBinaryExpr(aExpr).OpCode;
    lIsBool := lOp in [eopAnd, eopOr];
    if lIsBool and (lOp <> aParentOp) then
      Inc(Result);
    if lIsBool then
      lNextParent := lOp
    else
      lNextParent := aParentOp;
    Result := Result + CountBoolRuns(TBinaryExpr(aExpr).Left, lNextParent)
      + CountBoolRuns(TBinaryExpr(aExpr).Right, lNextParent);
  end
  else if aExpr is TUnaryExpr then
    Result := Result + CountBoolRuns(TUnaryExpr(aExpr).Operand, aParentOp);
end;


{ cyclomatic complexity }

// The cyclomatic contribution of aStmt itself (excluding its descendants).
function CyclomaticNode(aStmt: TPasImplElement): integer;
begin
  Result := 0;
  if aStmt is TPasImplIfElse then
    Result := 1 + CountAndOr(TPasImplIfElse(aStmt).ConditionExpr)
  else if aStmt is TPasImplWhileDo then
    Result := 1 + CountAndOr(TPasImplWhileDo(aStmt).ConditionExpr)
  else if aStmt is TPasImplRepeatUntil then
    Result := 1 + CountAndOr(TPasImplRepeatUntil(aStmt).ConditionExpr)
  else if aStmt is TPasImplForLoop then
    Result := 1
  else if aStmt is TPasImplCaseStatement then
    Result := 1
  else if aStmt is TPasImplExceptOn then
    Result := 1;
end;


// Sums the cyclomatic contributions of aStmt and every descendant statement.
function CyclomaticSum(aStmt: TPasImplElement): integer;
var
  lChildren: TPasImplElementArray;
  i: integer;
begin
  Result := CyclomaticNode(aStmt);
  lChildren := ChildStatements(aStmt);
  for i := 0 to High(lChildren) do
    Result := Result + CyclomaticSum(lChildren[i]);
end;


// The cyclomatic complexity of one routine block: base 1 plus every decision.
function CyclomaticScore(aBlock: TPasImplBlock): integer;
begin
  Result := 1 + CyclomaticSum(aBlock);
end;


{ cognitive complexity }

procedure CognitiveDescend(aStmt: TPasImplElement; aNesting: integer;
  var aScore: integer); forward;

{ Processes one if (or else-if continuation) of an if/else chain:
  The head if pays the nesting penalty (1 + aNesting);
  An else-if continuation pays +1 flat at the same nesting;
  A plain else pays +1 flat.
  The then-branch and a non-if else body are one nesting level deeper}
procedure CognitiveIf(aIf: TPasImplIfElse; aNesting: integer;
  aContinuation: boolean; var aScore: integer);
var
  lElse: TPasImplElement;
begin
  if aContinuation then
    aScore := aScore + 1
  else
    aScore := aScore + 1 + aNesting;
  aScore := aScore + CountBoolRuns(aIf.ConditionExpr, eopNone);
  CognitiveDescend(aIf.IfBranch, aNesting + 1, aScore);
  lElse := aIf.ElseBranch;
  if lElse is TPasImplIfElse then
    CognitiveIf(TPasImplIfElse(lElse), aNesting, True, aScore)
  else if lElse <> nil then
  begin
    aScore := aScore + 1;
    CognitiveDescend(lElse, aNesting + 1, aScore);
  end;
end;


{ Recurses aStmt accumulating cognitive complexity.
  Nesting structures add (1 + nesting) and deepen their bodies;
  goto adds +1;
  with / containers addnothing and keep the nesting level for their children.
}
procedure CognitiveDescend(aStmt: TPasImplElement; aNesting: integer;
  var aScore: integer);
var
  lChildren: TPasImplElementArray;
  i: integer;
begin
  if aStmt = nil then
    Exit;
  if aStmt is TPasImplIfElse then
  begin
    CognitiveIf(TPasImplIfElse(aStmt), aNesting, False, aScore);
    Exit;
  end;
  if (aStmt is TPasImplWhileDo) or (aStmt is TPasImplForLoop)
    or (aStmt is TPasImplRepeatUntil) or (aStmt is TPasImplCaseOf)
    or (aStmt is TPasImplExceptOn) then
  begin
    aScore := aScore + 1 + aNesting;
    if aStmt is TPasImplWhileDo then
      aScore := aScore + CountBoolRuns(TPasImplWhileDo(aStmt).ConditionExpr,
        eopNone)
    else if aStmt is TPasImplRepeatUntil then
      aScore := aScore + CountBoolRuns(
        TPasImplRepeatUntil(aStmt).ConditionExpr, eopNone);
    lChildren := ChildStatements(aStmt);
    for i := 0 to High(lChildren) do
      CognitiveDescend(lChildren[i], aNesting + 1, aScore);
    Exit;
  end;
  if aStmt is TPasImplGoto then
    aScore := aScore + 1;
  { with / try / begin / case branch / leaf do not contribute.
    recurse the children at the SAME nesting level. }
  lChildren := ChildStatements(aStmt);
  for i := 0 to High(lChildren) do
    CognitiveDescend(lChildren[i], aNesting, aScore);
end;


// The cognitive complexity of one routine block.
function CognitiveScore(aBlock: TPasImplBlock): integer;
begin
  Result := 0;
  CognitiveDescend(aBlock, 0, Result);
end;


{ statement count }

// True iff aStmt is an executable statement counted toward routine size.
function IsCountedStatement(aStmt: TPasImplElement): boolean;
begin
  Result := (aStmt is TPasImplSimple) or (aStmt is TPasImplAssign)
    or (aStmt is TPasImplIfElse) or (aStmt is TPasImplWhileDo)
    or (aStmt is TPasImplForLoop) or (aStmt is TPasImplRepeatUntil)
    or (aStmt is TPasImplCaseOf) or (aStmt is TPasImplWithDo)
    or (aStmt is TPasImplTry) or (aStmt is TPasImplRaise)
    or (aStmt is TPasImplGoto) or (aStmt is TPasImplAsmStatement)
    or (aStmt is TPasInlineVarDeclStatement);
end;


// Counts the executable statements in aStmt and its descendants.
function StatementCount(aStmt: TPasImplElement): integer;
var
  lChildren: TPasImplElementArray;
  i: integer;
begin
  Result := 0;
  if aStmt = nil then
    Exit;
  if IsCountedStatement(aStmt) then
    Inc(Result);
  lChildren := ChildStatements(aStmt);
  for i := 0 to High(lChildren) do
    Result := Result + StatementCount(lChildren[i]);
end;


{ nesting depth }

// True iff entering aStmt deepens the control-nesting level.
function IsDepthNode(aStmt: TPasImplElement): boolean;
begin
  Result := (aStmt is TPasImplIfElse) or (aStmt is TPasImplWhileDo)
    or (aStmt is TPasImplForLoop) or (aStmt is TPasImplRepeatUntil)
    or (aStmt is TPasImplCaseOf) or (aStmt is TPasImplWithDo)
    or (aStmt is TPasImplTry) or (aStmt is TPasImplExceptOn);
end;


{ Descends aStmt at nesting aDepth, recording the deepest level reached in aMax.
  An else-if ladder is kept flat:
  an if's else-if ElseBranch is processed at the enclosing if's depth, not one deeper. }
procedure NestingDescend(aStmt: TPasImplElement; aDepth: integer;
  var aMax: integer);
var
  lChildren: TPasImplElementArray;
  lDepth, i: integer;
  lElse: TPasImplElement;
begin
  if aStmt = nil then
    Exit;
  if IsDepthNode(aStmt) then
  begin
    lDepth := aDepth + 1;
    if lDepth > aMax then
      aMax := lDepth;
  end
  else
    lDepth := aDepth;
  if aStmt is TPasImplIfElse then
  begin
    NestingDescend(TPasImplIfElse(aStmt).IfBranch, lDepth, aMax);
    lElse := TPasImplIfElse(aStmt).ElseBranch;
    if lElse is TPasImplIfElse then
      NestingDescend(lElse, aDepth, aMax)
    else
      NestingDescend(lElse, lDepth, aMax);
    Exit;
  end;
  lChildren := ChildStatements(aStmt);
  for i := 0 to High(lChildren) do
    NestingDescend(lChildren[i], lDepth, aMax);
end;


// The deepest control-nesting level reached in one routine block.
function NestingDepth(aBlock: TPasImplBlock): integer;
begin
  Result := 0;
  NestingDescend(aBlock, 0, Result);
end;


{ shared emission }

{ Emits one issue for aRoutine at its declaration line, column 1, carrying the
  computed value and the limit as message args and the routine name as snippet. }
procedure EmitMetric(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; const aRoutine: TAstRoutine;
  aValue, aLimit: integer);
var
  lLine: integer;
begin
  lLine := aRoutine.Decl.SourceLinenumber;
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, lLine, 1, lLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    [IntToStr(aValue), IntToStr(aLimit)], aRoutine.Decl.Name);
end;


{ shared statement collection + emission }

{ Appends every statement strictly below aRoot to aList}
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


// Every statement node in aModule, in deterministic order.
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


// Emits one issue at aLine, column 1
procedure EmitStmt(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aLine: integer;
  const aArgs: array of string; const aSnippet: string);
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aLine, 1, aLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    aArgs, aSnippet);
end;


{ shared expression-comparison collection }

// Appends every equality/inequality comparison. Nil-safe; appends nothing for a leaf / nil.
procedure CollectComparisons(aExpr: TPasExpr; var aList: TPasExprArray);
var
  i: integer;
begin
  if aExpr = nil then
    Exit;
  if aExpr is TBinaryExpr then
  begin
    if TBinaryExpr(aExpr).OpCode in [eopEqual, eopNotEqual] then
    begin
      SetLength(aList, Length(aList) + 1);
      aList[High(aList)] := aExpr;
    end;
    CollectComparisons(TBinaryExpr(aExpr).Left, aList);
    CollectComparisons(TBinaryExpr(aExpr).Right, aList);
  end
  else if aExpr is TUnaryExpr then
    CollectComparisons(TUnaryExpr(aExpr).Operand, aList)
  else if aExpr is TParamsExpr then
  begin
    CollectComparisons(TParamsExpr(aExpr).Value, aList);
    for i := 0 to High(TParamsExpr(aExpr).Params) do
      CollectComparisons(TParamsExpr(aExpr).Params[i], aList);
  end;
end;


{ Every equality/inequality comparison node in aModule's statement expressions,
  in deterministic statement-then-expression order. }
function AllComparisons(aModule: TPasModule): TPasExprArray;
var
  lStmts: TPasImplElementArray;
  lWith: TPasImplWithDo;
  i, j: integer;

  procedure Take(aExpr: TPasExpr);
  begin
    CollectComparisons(aExpr, Result);
  end;

begin
  SetLength(Result, 0);
  lStmts := AllStatements(aModule);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplAssign then
    begin
      Take(TPasImplAssign(lStmts[i]).Left);
      Take(TPasImplAssign(lStmts[i]).Right);
    end
    else if lStmts[i] is TPasImplSimple then
      Take(TPasImplSimple(lStmts[i]).Expr)
    else if lStmts[i] is TPasImplIfElse then
      Take(TPasImplIfElse(lStmts[i]).ConditionExpr)
    else if lStmts[i] is TPasImplWhileDo then
      Take(TPasImplWhileDo(lStmts[i]).ConditionExpr)
    else if lStmts[i] is TPasImplRepeatUntil then
      Take(TPasImplRepeatUntil(lStmts[i]).ConditionExpr)
    else if lStmts[i] is TPasImplForLoop then
    begin
      Take(TPasImplForLoop(lStmts[i]).StartExpr);
      Take(TPasImplForLoop(lStmts[i]).EndExpr);
    end
    else if lStmts[i] is TPasImplCaseOf then
      Take(TPasImplCaseOf(lStmts[i]).CaseExpr)
    else if lStmts[i] is TPasImplWithDo then
    begin
      lWith := TPasImplWithDo(lStmts[i]);
      if lWith.Expressions <> nil then
        for j := 0 to lWith.Expressions.Count - 1 do
          if TObject(lWith.Expressions[j]) is TPasExpr then
            Take(TPasExpr(lWith.Expressions[j]));
    end;
end;


{ program-section access }

{ The Declarations list of aModule's program section when aModule is a program, else nil.}
function ProgramSectionDecls(aModule: TPasModule): TFPList;
begin
  Result := nil;
  if not (aModule is TPasProgram) then
    Exit;
  if TPasProgram(aModule).ProgramSection = nil then
    Exit;
  Result := TPasProgram(aModule).ProgramSection.Declarations;
end;


{ shared jump / tail classification }

{ Returns 'exit' / 'break' / 'continue' (lowercased) when aStmt is a bare jump statement }
function BareJumpKind(aStmt: TPasImplElement): string;
var
  lExpr: TPasExpr;
  lName: string;
begin
  Result := '';
  if not (aStmt is TPasImplSimple) then
    Exit;
  lExpr := TPasImplSimple(aStmt).Expr;
  if not (lExpr is TPrimitiveExpr) then
    Exit;
  if TPrimitiveExpr(lExpr).Kind <> pekIdent then
    Exit;
  lName := LowerCase(TPrimitiveExpr(lExpr).Value);
  if (lName = 'exit') or (lName = 'break') or (lName = 'continue') then
    Result := lName;
end;


{ The immediate fall-through last statement of aBody:
  for a begin..end block or a repeat..until, the last Elements entry (nil when empty);
  for any other single statement, aBody itself. }
function LastStatement(aBody: TPasImplElement): TPasImplElement;
var
  lList: TFPList;
begin
  Result := nil;
  if aBody = nil then
    Exit;
  if (aBody is TPasImplBeginBlock) or (aBody is TPasImplRepeatUntil) then
  begin
    lList := TPasImplBlock(aBody).Elements;
    if (lList <> nil) and (lList.Count > 0)
      and (TObject(lList[lList.Count - 1]) is TPasImplElement) then
      Result := TPasImplElement(lList[lList.Count - 1]);
  end
  else
    Result := aBody;
end;


{ True iff aLeft and aRight are the same syntactic lvalue:
   an identifier, the 'Self' instance reference or a member-access chain whose every leg matches. }
function SameLValue(aLeft, aRight: TPasExpr): boolean;
begin
  Result := False;
  if (aLeft = nil) or (aRight = nil) then
    Exit;
  if aLeft.ClassType <> aRight.ClassType then
    Exit;
  if aLeft is TPrimitiveExpr then
    Result := (TPrimitiveExpr(aLeft).Kind = pekIdent)
      and (TPrimitiveExpr(aRight).Kind = pekIdent)
      and (LowerCase(TPrimitiveExpr(aLeft).Value)
      = LowerCase(TPrimitiveExpr(aRight).Value))
  else if aLeft is TSelfExpr then
    Result := True
  else if aLeft is TBinaryExpr then
    Result := (TBinaryExpr(aLeft).OpCode = eopSubIdent)
      and (TBinaryExpr(aRight).OpCode = eopSubIdent)
      and SameLValue(TBinaryExpr(aLeft).Left, TBinaryExpr(aRight).Left)
      and SameLValue(TBinaryExpr(aLeft).Right, TBinaryExpr(aRight).Right);
end;


{ declaration-level counts }

// Directly-nested routine count: entries in aDecl's body that are themselves routines
function CountNestedRoutines(aDecl: TPasProcedure): integer;
var
  lDecls: TFPList;
  i: integer;
begin
  Result := 0;
  if (aDecl = nil) or (aDecl.Body = nil) or (aDecl.Body.Declarations = nil) then
    Exit;
  lDecls := aDecl.Body.Declarations;
  for i := 0 to lDecls.Count - 1 do
    if TObject(lDecls[i]) is TPasProcedure then
      Inc(Result);
end;


// Formal parameter count: every TPasArgument is one parameter, so the count is semantic, not syntactic.
function CountParameters(aDecl: TPasProcedure): integer;
begin
  Result := 0;
  if (aDecl = nil) or (aDecl.ProcType = nil) or (aDecl.ProcType.Args = nil) then
    Exit;
  Result := aDecl.ProcType.Args.Count;
end;


{ Default-parameter count: parameters carrying a default value }
function CountDefaultParameters(aDecl: TPasProcedure): integer;
var
  lArgs: TFPList;
  i: integer;
begin
  Result := 0;
  if (aDecl = nil) or (aDecl.ProcType = nil) or (aDecl.ProcType.Args = nil) then
    Exit;
  lArgs := aDecl.ProcType.Args;
  for i := 0 to lArgs.Count - 1 do
    if TPasArgument(lArgs[i]).ValueExpr <> nil then
      Inc(Result);
end;


{ Local variable count: classic var-section locals in aDecl's body }
function CountLocalVariables(aDecl: TPasProcedure): integer;
var
  lDecls: TFPList;
  lNode: TObject;
  i: integer;
begin
  Result := 0;
  if (aDecl = nil) or (aDecl.Body = nil) or (aDecl.Body.Declarations = nil) then
    Exit;
  lDecls := aDecl.Body.Declarations;
  for i := 0 to lDecls.Count - 1 do
  begin
    lNode := TObject(lDecls[i]);
    if (lNode is TPasVariable) and not (lNode is TPasConst) then
      Inc(Result);
  end;
end;


{ TRuleCyclomaticComplexity }

procedure TRuleCyclomaticComplexity.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  i, lScore, lLimit: integer;
begin
  lLimit := aContext.Config.RuleParamInt(FMetadata.RuleId, 'maxComplexity',
    cMaxCyclomatic);
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
  begin
    lScore := CyclomaticScore(lRoutines[i].Block);
    if lScore > lLimit then
      EmitMetric(FMetadata, aContext, aCollector, lRoutines[i], lScore,
        lLimit);
  end;
end;


{ TRuleCognitiveComplexity }

procedure TRuleCognitiveComplexity.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  i, lScore, lLimit: integer;
begin
  lLimit := aContext.Config.RuleParamInt(FMetadata.RuleId, 'maxComplexity',
    cMaxCognitive);
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
  begin
    lScore := CognitiveScore(lRoutines[i].Block);
    if lScore > lLimit then
      EmitMetric(FMetadata, aContext, aCollector, lRoutines[i], lScore,
        lLimit);
  end;
end;


{ TRuleRoutineTooLarge }

procedure TRuleRoutineTooLarge.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  i, lCount, lLimit: integer;
begin
  lLimit := aContext.Config.RuleParamInt(FMetadata.RuleId, 'maxStatements',
    cMaxStatements);
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
  begin
    lCount := StatementCount(lRoutines[i].Block);
    if lCount > lLimit then
      EmitMetric(FMetadata, aContext, aCollector, lRoutines[i], lCount,
        lLimit);
  end;
end;


{ TRuleRoutineTooDeeplyNested }

procedure TRuleRoutineTooDeeplyNested.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  i, lDepth, lLimit: integer;
begin
  lLimit := aContext.Config.RuleParamInt(FMetadata.RuleId, 'maxDepth',
    cMaxNestingDepth);
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
  begin
    lDepth := NestingDepth(lRoutines[i].Block);
    if lDepth > lLimit then
      EmitMetric(FMetadata, aContext, aCollector, lRoutines[i], lDepth,
        lLimit);
  end;
end;


{ TRuleTooManyNestedRoutines }

procedure TRuleTooManyNestedRoutines.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  i, lCount, lLimit: integer;
begin
  lLimit := aContext.Config.RuleParamInt(FMetadata.RuleId, 'maxNestedRoutines',
    cMaxNestedRoutines);
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
  begin
    lCount := CountNestedRoutines(lRoutines[i].Decl);
    if lCount > lLimit then
      EmitMetric(FMetadata, aContext, aCollector, lRoutines[i], lCount,
        lLimit);
  end;
end;


{ TRuleTooManyParameters }

procedure TRuleTooManyParameters.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  i, lCount, lLimit: integer;
begin
  lLimit := aContext.Config.RuleParamInt(FMetadata.RuleId, 'maxParameters',
    cMaxParameters);
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
  begin
    lCount := CountParameters(lRoutines[i].Decl);
    if lCount > lLimit then
      EmitMetric(FMetadata, aContext, aCollector, lRoutines[i], lCount,
        lLimit);
  end;
end;


{ TRuleTooManyVariables }

procedure TRuleTooManyVariables.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  i, lCount, lLimit: integer;
begin
  lLimit := aContext.Config.RuleParamInt(FMetadata.RuleId, 'maxVariables',
    cMaxVariables);
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
  begin
    lCount := CountLocalVariables(lRoutines[i].Decl);
    if lCount > lLimit then
      EmitMetric(FMetadata, aContext, aCollector, lRoutines[i], lCount,
        lLimit);
  end;
end;


{ TRuleTooManyDefaultParameters }

procedure TRuleTooManyDefaultParameters.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  i, lCount, lLimit: integer;
begin
  lLimit := aContext.Config.RuleParamInt(FMetadata.RuleId,
    'maxDefaultParameters', cMaxDefaultParameters);
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
  begin
    lCount := CountDefaultParameters(lRoutines[i].Decl);
    if lCount > lLimit then
      EmitMetric(FMetadata, aContext, aCollector, lRoutines[i], lCount,
        lLimit);
  end;
end;


{ TRuleBeginEndRequired }

procedure TRuleBeginEndRequired.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  i: integer;

  procedure CheckBody(aBody: TPasImplElement; const aKind: string);
  begin
    if (aBody <> nil) and not (aBody is TPasImplBeginBlock) then
      EmitStmt(FMetadata, aContext, aCollector, aBody.SourceLinenumber,
        [aKind], aKind);
  end;

begin
  // The 'if cond then Exit;' idiom is settled via config, not a hardcoded
  // verdict. Default False = flag single-statement bodies;
  // allowSingleStatement=true exempts them entirely.
  if aContext.Config.RuleParamBool(FMetadata.RuleId, 'allowSingleStatement',
    False) then
    Exit;
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplIfElse then
    begin
      CheckBody(TPasImplIfElse(lStmts[i]).IfBranch, 'if');
      // else-if chains are exempt: a TPasImplIfElse ElseBranch is a continuation, visited as its own node.
      if not (TPasImplIfElse(lStmts[i]).ElseBranch is TPasImplIfElse) then
        CheckBody(TPasImplIfElse(lStmts[i]).ElseBranch, 'else');
    end
    else if lStmts[i] is TPasImplWhileDo then
      CheckBody(TPasImplWhileDo(lStmts[i]).Body, 'while')
    else if lStmts[i] is TPasImplForLoop then
      CheckBody(TPasImplForLoop(lStmts[i]).Body, 'for')
    else if lStmts[i] is TPasImplWithDo then
      CheckBody(TPasImplWithDo(lStmts[i]).Body, 'with');
end;


{ TRuleNoGoto }

procedure TRuleNoGoto.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  i: integer;
  lSnippet: string;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplGoto then
    begin
      lSnippet := TPasImplGoto(lStmts[i]).LabelName;
      if lSnippet = '' then
        lSnippet := 'goto';
      EmitStmt(FMetadata, aContext, aCollector, lStmts[i].SourceLinenumber,
        [], lSnippet);
    end;
end;


{ TRuleNoWith }

procedure TRuleNoWith.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  i: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplWithDo then
      EmitStmt(FMetadata, aContext, aCollector, lStmts[i].SourceLinenumber,
        [], 'with');
end;


{ TRuleNoSelfAssignment }

procedure TRuleNoSelfAssignment.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lAssign: TPasImplAssign;
  i: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplAssign then
    begin
      lAssign := TPasImplAssign(lStmts[i]);
      if (lAssign.Kind = akDefault)
        and SameLValue(lAssign.Left, lAssign.Right) then
        EmitStmt(FMetadata, aContext, aCollector,
          lAssign.SourceLinenumber, [], 'self-assignment');
    end;
end;


{ TRuleNoInlineAssembly }

procedure TRuleNoInlineAssembly.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  i: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplAsmStatement then
      EmitStmt(FMetadata, aContext, aCollector, lStmts[i].SourceLinenumber,
        [], 'asm');
end;


{ TRuleCaseAtLeastTwoItems }

procedure TRuleCaseAtLeastTwoItems.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lCase: TPasImplCaseOf;
  i, j, lCount: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplCaseOf then
    begin
      lCase := TPasImplCaseOf(lStmts[i]);
      lCount := 0;
      if lCase.Elements <> nil then
        for j := 0 to lCase.Elements.Count - 1 do
          if TObject(lCase.Elements[j]) is TPasImplCaseStatement then
            Inc(lCount);
      if lCount < 2 then
        EmitStmt(FMetadata, aContext, aCollector, lCase.SourceLinenumber,
          [IntToStr(lCount)], 'case');
    end;
end;


{ TRuleNoEmptyBlock }

procedure TRuleNoEmptyBlock.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lBlock: TPasImplBlock;
  i: integer;
begin
  // AllStatements excludes the per-routine root blocks, so a routine's own empty
  // body is NOT seen here (that is RoutineNotEmpty's job — the
  // NoEmptyBlock/RoutineNotEmpty partition).
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplBeginBlock then
    begin
      lBlock := TPasImplBlock(lStmts[i]);
      if (lBlock.Elements = nil) or (lBlock.Elements.Count = 0) then
        EmitStmt(FMetadata, aContext, aCollector, lBlock.SourceLinenumber,
          [], 'begin');
    end;
end;


{ TRuleRoutineNotEmpty }

procedure TRuleRoutineNotEmpty.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  lBlock: TPasImplBlock;
  i: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
    if lRoutines[i].Block is TPasImplBeginBlock then
    begin
      lBlock := lRoutines[i].Block;
      if (lBlock.Elements = nil) or (lBlock.Elements.Count = 0) then
        EmitStmt(FMetadata, aContext, aCollector,
          lRoutines[i].Decl.SourceLinenumber, [lRoutines[i].Decl.Name],
          lRoutines[i].Decl.Name);
    end;
end;


{ TRuleUnitNotEmpty }

procedure TRuleUnitNotEmpty.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

  function SectionEmpty(aSec: TPasSection): boolean;
  begin
    Result := (aSec = nil) or (aSec.Declarations = nil)
      or (aSec.Declarations.Count = 0);
  end;

  function BlockEmpty(aBlk: TPasImplBlock): boolean;
  begin
    Result := (aBlk = nil) or (aBlk.Elements = nil) or (aBlk.Elements.Count = 0);
  end;

var
  lMod: TPasModule;
begin
  // Unit-only: a program/library is never "empty" in the unit sense.
  lMod := aContext.Module;
  if (lMod = nil) or (lMod is TPasProgram) or (lMod is TPasLibrary) then
    Exit;
  // 'uses' clauses are ignored.
  if SectionEmpty(lMod.InterfaceSection)
    and SectionEmpty(lMod.ImplementationSection)
    and BlockEmpty(lMod.InitializationSection)
    and BlockEmpty(lMod.FinalizationSection) then
    EmitStmt(FMetadata, aContext, aCollector, lMod.SourceLinenumber,
      [lMod.Name], lMod.Name);
end;


{ TRuleRedundantJump }

procedure TRuleRedundantJump.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  lStmts: TPasImplElementArray;
  lLast: TPasImplElement;
  i: integer;
begin
  // Trailing Exit: last statement of a routine body.
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
  begin
    lLast := LastStatement(lRoutines[i].Block);
    if BareJumpKind(lLast) = 'exit' then
      EmitStmt(FMetadata, aContext, aCollector, lLast.SourceLinenumber,
        ['exit'], 'exit');
  end;
  { Trailing Continue: last statement of a loop body.
    A trailing Break matches BareJumpKind but is never emitted }
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
  begin
    lLast := nil;
    if lStmts[i] is TPasImplWhileDo then
      lLast := LastStatement(TPasImplWhileDo(lStmts[i]).Body)
    else if lStmts[i] is TPasImplForLoop then
      lLast := LastStatement(TPasImplForLoop(lStmts[i]).Body)
    else if lStmts[i] is TPasImplRepeatUntil then
      lLast := LastStatement(lStmts[i]);
    if BareJumpKind(lLast) = 'continue' then
      EmitStmt(FMetadata, aContext, aCollector, lLast.SourceLinenumber,
        ['continue'], 'continue');
  end;
end;


{ TRuleFunctionReturnTypeRequired }

procedure TRuleFunctionReturnTypeRequired.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  lFn: TPasFunction;
  i: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
    if lRoutines[i].Decl is TPasFunction then
    begin
      lFn := TPasFunction(lRoutines[i].Decl);
      if (lFn.FuncType = nil) or (lFn.FuncType.ResultEl = nil)
        or (lFn.FuncType.ResultEl.ResultType = nil) then
        EmitStmt(FMetadata, aContext, aCollector, lFn.SourceLinenumber,
          [lFn.Name], lFn.Name);
    end;
end;


{ TRuleRedundantBooleanLiteral }

procedure TRuleRedundantBooleanLiteral.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCmps: TPasExprArray;
  lBin: TBinaryExpr;
  i: integer;
begin
  lCmps := AllComparisons(aContext.Module);
  for i := 0 to High(lCmps) do
  begin
    lBin := TBinaryExpr(lCmps[i]);
    if (lBin.Left is TBoolConstExpr) or (lBin.Right is TBoolConstExpr) then
      EmitStmt(FMetadata, aContext, aCollector, lBin.SourceLinenumber,
        [], 'boolean');
  end;
end;


{ TRuleNilCheckViaAssigned }

procedure TRuleNilCheckViaAssigned.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCmps: TPasExprArray;
  lBin: TBinaryExpr;
  i: integer;
begin
  lCmps := AllComparisons(aContext.Module);
  for i := 0 to High(lCmps) do
  begin
    lBin := TBinaryExpr(lCmps[i]);
    if (lBin.Left is TNilExpr) or (lBin.Right is TNilExpr) then
      EmitStmt(FMetadata, aContext, aCollector, lBin.SourceLinenumber,
        [], 'nil');
  end;
end;


{ TRuleNoObjectTypes }

procedure TRuleNoObjectTypes.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  i: integer;
begin
  lTypes := EnumerateTypes(aContext.Module);
  for i := 0 to High(lTypes) do
    if (lTypes[i] is TPasClassType)
      and (TPasClassType(lTypes[i]).ObjKind = okObject) then
      EmitStmt(FMetadata, aContext, aCollector, lTypes[i].SourceLinenumber,
        [lTypes[i].Name], lTypes[i].Name);
end;


{ TRuleNoLegacyInitializationSection }

procedure TRuleNoLegacyInitializationSection.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lMod: TPasModule;
  lLine, i: integer;
begin
  lMod := aContext.Module;
  if (lMod = nil) or (lMod is TPasProgram) or (lMod is TPasLibrary) then
    Exit;
  if lMod.InitializationSection = nil then
    Exit;
  lLine := lMod.InitializationSection.SourceLinenumber;
  { The first significant token on the init-section line is the introducing
    keyword: 'begin' (legacy) or 'initialization' (compliant). }
  for i := 0 to High(aContext.Tokens) do
    if (aContext.Tokens[i].Row = lLine)
      and not aContext.Tokens[i].IsTrivia then
    begin
      if aContext.Tokens[i].IsBegin then
        EmitStmt(FMetadata, aContext, aCollector, lLine, [], 'begin');
      Break;
    end;
end;


procedure TRuleInlineConstNoTypeInference.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  // Not in FPC
end;


procedure TRuleInlineLoopVarNoTypeInference.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lFor: TPasImplForLoop;
  i: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplForLoop then
    begin
      lFor := TPasImplForLoop(lStmts[i]);
      if lFor.ImplicitTyped then
        EmitStmt(FMetadata, aContext, aCollector, lFor.SourceLinenumber,
          [], 'for');
    end;
end;


procedure TRuleInlineVarNoTypeInference.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lDecl: TPasInlineVarDeclStatement;
  lVar: TPasVariable;
  i, j: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasInlineVarDeclStatement then
    begin
      lDecl := TPasInlineVarDeclStatement(lStmts[i]);
      if lDecl.Declarations <> nil then
        for j := 0 to lDecl.Declarations.Count - 1 do
          if TObject(lDecl.Declarations[j]) is TPasVariable then
          begin
            lVar := TPasVariable(lDecl.Declarations[j]);
            if lVar.VarType = nil then
              EmitStmt(FMetadata, aContext, aCollector,
                lVar.SourceLinenumber, [lVar.Name], lVar.Name);
          end;
    end;
end;


procedure TRuleProjectFileNoRoutines.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lDecls: TFPList;
  lProc: TPasProcedure;
  i: integer;
begin
  lDecls := ProgramSectionDecls(aContext.Module);
  if lDecls = nil then
    Exit;
  for i := 0 to lDecls.Count - 1 do
    if TObject(lDecls[i]) is TPasProcedure then
    begin
      lProc := TPasProcedure(lDecls[i]);
      EmitStmt(FMetadata, aContext, aCollector, lProc.SourceLinenumber,
        [lProc.Name], lProc.Name);
    end;
end;


procedure TRuleProjectFileNoVariables.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lDecls: TFPList;
  lVar: TPasVariable;
  i: integer;
begin
  lDecls := ProgramSectionDecls(aContext.Module);
  if lDecls = nil then
    Exit;
  for i := 0 to lDecls.Count - 1 do
    if (TObject(lDecls[i]) is TPasVariable)
      and not (TObject(lDecls[i]) is TPasConst) then
    begin
      lVar := TPasVariable(lDecls[i]);
      EmitStmt(FMetadata, aContext, aCollector, lVar.SourceLinenumber,
        [lVar.Name], lVar.Name);
    end;
end;


{ Builds an rtAst metric-rule metadata declaring its single integer threshold param. }
function IntMetricMeta(const aRuleId: string; aSeverity: TFpSonarSeverity;
  const aMessageKey, aParamName: string; aDefault: integer): TRuleMetadata;
begin
  Result := TRuleMetadata.Make(aRuleId, rtAst, rfAst, aSeverity, itCodeSmell, cfHigh,
    True, aMessageKey);
  Result.AddParam(aParamName, rpkInt, aDefault);
end;


{ Builds the BeginEndRequired metadata declaring its boolean param 'allowSingleStatement' }
function BeginEndRequiredMeta: TRuleMetadata;
begin
  Result := TRuleMetadata.Make('BeginEndRequired', rtAst, rfAst, sevMinor, itCodeSmell,
    cfHigh, True, cKeyBeginEndRequired);
  Result.AddParam('allowSingleStatement', rpkBool, False);
  Result.Description :=
    'Flags a control-flow body that is a single statement rather than a begin..end block.';
end;


initialization
  RegisterRule(TRuleCyclomaticComplexity.Create(IntMetricMeta(
    'CyclomaticComplexity', sevMajor, cKeyCyclomatic, 'maxComplexity',
    cMaxCyclomatic).WithDescription(
    'Flags a routine whose cyclomatic complexity exceeds the configured maximum.')));
  RegisterMessage(cKeyCyclomatic, SCyclomaticComplexity);

  RegisterRule(TRuleCognitiveComplexity.Create(IntMetricMeta(
    'CognitiveComplexity', sevMajor, cKeyCognitive, 'maxComplexity',
    cMaxCognitive).WithDescription(
    'Flags a routine whose cognitive complexity exceeds the configured maximum.')));
  RegisterMessage(cKeyCognitive, SCognitiveComplexity);

  RegisterRule(TRuleRoutineTooLarge.Create(IntMetricMeta(
    'RoutineTooLarge', sevMajor, cKeyRoutineTooLarge, 'maxStatements',
    cMaxStatements).WithDescription(
    'Flags a routine with more statements than the configured maximum.')));
  RegisterMessage(cKeyRoutineTooLarge, SRoutineTooLarge);

  RegisterRule(TRuleRoutineTooDeeplyNested.Create(IntMetricMeta(
    'RoutineTooDeeplyNested', sevMajor, cKeyRoutineTooDeeplyNested, 'maxDepth',
    cMaxNestingDepth).WithDescription(
    'Flags a routine whose nesting depth exceeds the configured maximum.')));
  RegisterMessage(cKeyRoutineTooDeeplyNested, SRoutineTooDeeplyNested);

  // Count rules (sevMinor).
  RegisterRule(TRuleTooManyNestedRoutines.Create(IntMetricMeta(
    'TooManyNestedRoutines', sevMinor, cKeyTooManyNestedRoutines,
    'maxNestedRoutines', cMaxNestedRoutines).WithDescription(
    'Flags a routine with more nested routines than the configured maximum.')));
  RegisterMessage(cKeyTooManyNestedRoutines, STooManyNestedRoutines);

  RegisterRule(TRuleTooManyParameters.Create(IntMetricMeta(
    'TooManyParameters', sevMinor, cKeyTooManyParameters, 'maxParameters',
    cMaxParameters).WithDescription(
    'Flags a routine with more parameters than the configured maximum.')));
  RegisterMessage(cKeyTooManyParameters, STooManyParameters);

  RegisterRule(TRuleTooManyVariables.Create(IntMetricMeta(
    'TooManyVariables', sevMinor, cKeyTooManyVariables, 'maxVariables',
    cMaxVariables).WithDescription(
    'Flags a routine with more local variables than the configured maximum.')));
  RegisterMessage(cKeyTooManyVariables, STooManyVariables);

  RegisterRule(TRuleTooManyDefaultParameters.Create(IntMetricMeta(
    'TooManyDefaultParameters', sevMinor, cKeyTooManyDefaultParameters,
    'maxDefaultParameters', cMaxDefaultParameters).WithDescription(
    'Flags a routine with more default parameters than the configured maximum.')));
  RegisterMessage(cKeyTooManyDefaultParameters, STooManyDefaultParameters);

  // Statement & control rules
  RegisterRule(TRuleBeginEndRequired.Create(BeginEndRequiredMeta));
  RegisterMessage(cKeyBeginEndRequired, SBeginEndRequired);

  RegisterRule(TRuleNoGoto.Create(TRuleMetadata.Make(
    'NoGoto', rtAst, rfAst, sevMajor, itCodeSmell, cfHigh,
    True, cKeyNoGoto).WithDescription(
    'Flags goto statements.')));
  RegisterMessage(cKeyNoGoto, SNoGoto);

  RegisterRule(TRuleNoWith.Create(TRuleMetadata.Make(
    'NoWith', rtAst, rfAst, sevMajor, itCodeSmell, cfHigh,
    True, cKeyNoWith).WithDescription(
    'Flags with statements.')));
  RegisterMessage(cKeyNoWith, SNoWith);

  RegisterRule(TRuleNoSelfAssignment.Create(TRuleMetadata.Make(
    'NoSelfAssignment', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyNoSelfAssignment).WithDescription(
    'Flags a self-assignment, which has no effect.')));
  RegisterMessage(cKeyNoSelfAssignment, SNoSelfAssignment);

  RegisterRule(TRuleNoInlineAssembly.Create(TRuleMetadata.Make(
    'NoInlineAssembly', rtAst, rfAst, sevMajor, itCodeSmell, cfHigh,
    True, cKeyNoInlineAssembly).WithDescription(
    'Flags inline assembly (asm..end) blocks.')));
  RegisterMessage(cKeyNoInlineAssembly, SNoInlineAssembly);

  RegisterRule(TRuleCaseAtLeastTwoItems.Create(TRuleMetadata.Make(
    'CaseAtLeastTwoItems', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyCaseAtLeastTwoItems).WithDescription(
    'Flags a case statement with fewer than two branches.')));
  RegisterMessage(cKeyCaseAtLeastTwoItems, SCaseAtLeastTwoItems);

  // Statement, block & emptiness rules.
  RegisterRule(TRuleNoEmptyBlock.Create(TRuleMetadata.Make(
    'NoEmptyBlock', rtAst, rfAst, sevMajor, itCodeSmell, cfHigh,
    True, cKeyNoEmptyBlock).WithDescription(
    'Flags an empty begin..end block.')));
  RegisterMessage(cKeyNoEmptyBlock, SNoEmptyBlock);

  RegisterRule(TRuleRoutineNotEmpty.Create(TRuleMetadata.Make(
    'RoutineNotEmpty', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyRoutineNotEmpty).WithDescription(
    'Flags a routine with an empty body.')));
  RegisterMessage(cKeyRoutineNotEmpty, SRoutineNotEmpty);

  RegisterRule(TRuleUnitNotEmpty.Create(TRuleMetadata.Make(
    'UnitNotEmpty', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyUnitNotEmpty).WithDescription(
    'Flags a unit with no declarations or statements.')));
  RegisterMessage(cKeyUnitNotEmpty, SUnitNotEmpty);

  RegisterRule(TRuleRedundantJump.Create(TRuleMetadata.Make(
    'RedundantJump', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyRedundantJump).WithDescription(
    'Flags a redundant jump statement that can be removed.')));
  RegisterMessage(cKeyRedundantJump, SRedundantJump);

  RegisterRule(TRuleFunctionReturnTypeRequired.Create(TRuleMetadata.Make(
    'FunctionReturnTypeRequired', rtAst, rfAst, sevMajor, itCodeSmell, cfHigh,
    True, cKeyFunctionReturnTypeRequired).WithDescription(
    'Flags a function without an explicit result type.')));
  RegisterMessage(cKeyFunctionReturnTypeRequired, SFunctionReturnTypeRequired);

  // Statement-expression & legacy-construct rules.
  RegisterRule(TRuleRedundantBooleanLiteral.Create(TRuleMetadata.Make(
    'RedundantBooleanLiteral', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyRedundantBooleanLiteral).WithDescription(
    'Flags a comparison against a boolean literal that can be simplified.')));
  RegisterMessage(cKeyRedundantBooleanLiteral, SRedundantBooleanLiteral);

  RegisterRule(TRuleNilCheckViaAssigned.Create(TRuleMetadata.Make(
    'NilCheckViaAssigned', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyNilCheckViaAssigned).WithDescription(
    'Flags comparing to nil where Assigned() is clearer.')));
  RegisterMessage(cKeyNilCheckViaAssigned, SNilCheckViaAssigned);

  RegisterRule(TRuleNoObjectTypes.Create(TRuleMetadata.Make(
    'NoObjectTypes', rtAst, rfAst, sevMajor, itCodeSmell, cfHigh,
    True, cKeyNoObjectTypes).WithDescription(
    'Flags the legacy object type; use class or record instead.')));
  RegisterMessage(cKeyNoObjectTypes, SNoObjectTypes);

  RegisterRule(TRuleNoLegacyInitializationSection.Create(TRuleMetadata.Make(
    'NoLegacyInitializationSection', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyNoLegacyInitializationSection).WithDescription(
    'Flags a legacy begin..end unit body; use an explicit initialization section.')));
  RegisterMessage(cKeyNoLegacyInitializationSection, SNoLegacyInitializationSection);

  // Inline-declaration & project-file rules.
  RegisterRule(TRuleInlineConstNoTypeInference.Create(TRuleMetadata.Make(
    'InlineConstNoTypeInference', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyInlineConstNoTypeInference).WithDescription(
    'Flags an inline constant declared without an explicit type.')));
  RegisterMessage(cKeyInlineConstNoTypeInference, SInlineConstNoTypeInference);

  RegisterRule(TRuleInlineLoopVarNoTypeInference.Create(TRuleMetadata.Make(
    'InlineLoopVarNoTypeInference', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyInlineLoopVarNoTypeInference).WithDescription(
    'Flags an inline loop variable declared without an explicit type.')));
  RegisterMessage(cKeyInlineLoopVarNoTypeInference, SInlineLoopVarNoTypeInference);

  RegisterRule(TRuleInlineVarNoTypeInference.Create(TRuleMetadata.Make(
    'InlineVarNoTypeInference', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyInlineVarNoTypeInference).WithDescription(
    'Flags an inline variable declared without an explicit type.')));
  RegisterMessage(cKeyInlineVarNoTypeInference, SInlineVarNoTypeInference);

  RegisterRule(TRuleProjectFileNoRoutines.Create(TRuleMetadata.Make(
    'ProjectFileNoRoutines', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyProjectFileNoRoutines).WithDescription(
    'Flags a routine declared in the project file; move it into a unit.')));
  RegisterMessage(cKeyProjectFileNoRoutines, SProjectFileNoRoutines);

  RegisterRule(TRuleProjectFileNoVariables.Create(TRuleMetadata.Make(
    'ProjectFileNoVariables', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyProjectFileNoVariables).WithDescription(
    'Flags a global variable declared in the project file; move it into a unit.')));
  RegisterMessage(cKeyProjectFileNoVariables, SProjectFileNoVariables);

end.
