{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Control-flow semantic analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Control;


{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  Pascal.Tree,
{$ELSE}
  PasTree,
{$ENDIF}
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Traversal, FpSonar.Resolver,
  FpSonar.Rules.Consts;

type
  { Flags a case over an enum that omits values and has no else. }
  TRuleExhaustiveCaseStatement = class(TRuleBase)
  public
    // Emits one issue per non-exhaustive enum case statement.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an exception instance constructed but silently discarded. }
  TRuleExceptionRaised = class(TRuleBase)
  public
    // Emits one issue per discarded exception construction.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a loop that provably runs at most once. }
  TRuleSingleIterationLoop = class(TRuleBase)
  public
    // Emits one issue per loop whose body unconditionally exits on pass one.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a function that returns by assigning to its own name. }
  TRuleNoPascalStyleResultAssignment = class(TRuleBase)
  public
    // Emits one issue per legacy function-name result assignment.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a Free/FreeAndNil needlessly guarded by an Assigned/nil check. }
  TRuleRedundantAssignedCheckBeforeFree = class(TRuleBase)
  public
    // Emits one issue per redundant Assigned/nil guard around a Free call.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a counted for-loop that indexes a fixed-length collection past
    its end (the off-by-one `for i := 0 to Length(A)`). }
  TRuleLoopBeyondCollectionEnd = class(TRuleBase)
  public
    // Emits one issue per loop whose body provably overruns a static array.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a function whose result is unassigned at the exit node of its
    control-flow graph. }
  TRuleRoutineResultAssigned = class(TRuleBase)
  public
    // Emits one issue per function with a provable no-result-write return path.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an except handler that catches the root Exception class, or a
    non-empty bare catch-all `except`, masking unrelated failures. }
  TRuleNoCatchRawException = class(TRuleBase)
  public
    // Emits one issue per raw-Exception catch (on E: Exception, or a swallowing
    // bare except) that does not re-raise.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a `raise` of the root Exception class directly. }
  TRuleNoRaiseRawException = class(TRuleBase)
  public
    // Emits one issue per 'raise Exception.Create(...)' of the raw root class.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an if statement whose then and else branches are identical. }
  TRuleIdenticalBranches = class(TRuleBase)
  public
    // Emits one issue per if statement with structurally identical branches.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a condition repeated inside one if/else if chain. }
  TRuleDuplicateConditionInChain = class(TRuleBase)
  public
    // Emits one issue per chain link whose condition text was already used.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a case label used more than once in one case statement. }
  TRuleDuplicateCaseLabel = class(TRuleBase)
  public
    // Emits one issue per repeated case label.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a comparison whose two operands are the same reference. }
  TRuleSelfComparison = class(TRuleBase)
  public
    // Emits one issue per comparison of an lvalue chain with itself.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags `if C then ;` followed by a further statement in the same block. }
  TRuleEmptyThenWithFollowingStatement = class(TRuleBase)
  public
    // Emits one issue per empty then branch with a following sibling statement.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a boolean operator mixed with a comparison and no parentheses. }
  TRuleMixedBooleanAndRelational = class(TRuleBase)
  public
    // Emits one issue per boolean/comparison pairing at one parenthesis depth.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an and/or/xor mixing boolean and integer, or bit-testing a condition. }
  TRuleBitwiseOnBooleanOperands = class(TRuleBase)
  public
    // Emits one issue per operator node that spans the boolean/integer boundary.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a `:=` in a call argument list, where the grammar implies `=`. }
  TRuleAssignmentInsteadOfComparison = class(TRuleBase)
  public
    // Emits one issue per named call argument.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a call mutating a var/out argument from a short-circuited operand. }
  TRuleConditionWithSideEffect = class(TRuleBase)
  public
    // Emits one issue per var/out call in the right operand of an and/or.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an else after a then branch that ends in exit, break, continue,
    halt or raise. }
  TRuleRedundantElseAfterExit = class(TRuleBase)
  public
    // Emits one issue per if whose then branch tail is a terminator statement.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an if nested as the sole content of another if, neither with an else. }
  TRuleCollapsibleNestedIf = class(TRuleBase)
  public
    // Emits one issue per outer if whose single branch statement is such an if.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a `not` condition on an if that carries a real else branch. }
  TRuleNegatedConditionWithElse = class(TRuleBase)
  public
    // Emits one issue per if with a top-level not and a non-empty else.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a case statement whose selector resolves to a Boolean type. }
  TRuleSwitchOnBooleanExpression = class(TRuleBase)
  public
    // Emits one issue per case statement over a Boolean selector.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a while/repeat whose condition variables the body never writes. }
  TRuleLoopConditionNeverChanges = class(TRuleBase)
  public
    // Emits one issue per loop whose condition locals the body leaves untouched.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a statement no control path through its routine body reaches. }
  TRuleUnreachableCode = class(TRuleBase)
  public
    // Emits one issue per dead run of a statement list, at the run's first
    // statement, and none for the file when any one graph could not be built.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, FpSonar.Ingest, FpSonar.DataFlow;
{$ELSE}
  SysUtils, FpSonar.Ingest, FpSonar.DataFlow;
{$ENDIF}

const
  cReportNamesLimit = 5;

  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyExhaustiveCaseStatement = 'rule.ExhaustiveCaseStatement.message';
  cKeyExceptionRaised = 'rule.ExceptionRaised.message';
  cKeySingleIterationLoop = 'rule.SingleIterationLoop.message';
  cKeyNoPascalStyleResultAssignment =
    'rule.NoPascalStyleResultAssignment.message';
  cKeyRedundantAssignedCheckBeforeFree =
    'rule.RedundantAssignedCheckBeforeFree.message';
  cKeyLoopBeyondCollectionEnd = 'rule.LoopBeyondCollectionEnd.message';
  cKeyRoutineResultAssigned = 'rule.RoutineResultAssigned.message';
  cKeyNoCatchRawException = 'rule.NoCatchRawException.message';
  cKeyNoRaiseRawException = 'rule.NoRaiseRawException.message';
  cKeyIdenticalBranches = 'rule.IdenticalBranches.message';
  cKeyDuplicateConditionInChain = 'rule.DuplicateConditionInChain.message';
  cKeyDuplicateCaseLabel = 'rule.DuplicateCaseLabel.message';
  cKeySelfComparison = 'rule.SelfComparison.message';
  cKeyEmptyThenWithFollowingStatement =
    'rule.EmptyThenWithFollowingStatement.message';
  cKeyMixedBooleanAndRelational = 'rule.MixedBooleanAndRelational.message';
  cKeyBitwiseOnBooleanOperands = 'rule.BitwiseOnBooleanOperands.message';
  cKeyAssignmentInsteadOfComparison =
    'rule.AssignmentInsteadOfComparison.message';
  cKeyConditionWithSideEffect = 'rule.ConditionWithSideEffect.message';
  cKeyRedundantElseAfterExit = 'rule.RedundantElseAfterExit.message';
  cKeyCollapsibleNestedIf = 'rule.CollapsibleNestedIf.message';
  cKeyNegatedConditionWithElse = 'rule.NegatedConditionWithElse.message';
  cKeySwitchOnBooleanExpression = 'rule.SwitchOnBooleanExpression.message';
  cKeyLoopConditionNeverChanges = 'rule.LoopConditionNeverChanges.message';
  cKeyUnreachableCode = 'rule.UnreachableCode.message';

  cFlagBareExcept = True;

  { ---- shared statement collection + emission (mirrors Rules.Exceptions) ---- }

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


// Emits one issue at aLine, column 1
procedure EmitStmt(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aLine: integer;
  const aArgs: array of string; const aSnippet: string);
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aLine, 1, aLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    aArgs, aSnippet);
end;


// Joins the missing enum value names into the single message arg: the first
// cReportNamesLimit names comma-separated, then "and N more" when truncated.
function JoinMissing(const aMissing: TFpSonarStringArray): string;
var
  i, lShown: integer;
begin
  Result := '';
  lShown := Length(aMissing);
  if lShown > cReportNamesLimit then
    lShown := cReportNamesLimit;
  for i := 0 to lShown - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + aMissing[i];
  end;
  if Length(aMissing) > cReportNamesLimit then
    Result := Result + ', and ' + IntToStr(Length(aMissing) - cReportNamesLimit)
      + ' more';
end;


// The straight-line body statement list of a loop
function LoopBody(aLoop: TPasImplElement): TPasImplElementArray;
begin
  Result := ChildStatements(aLoop);
  if (Length(Result) = 1) and (Result[0] is TPasImplBeginBlock) then
    Result := ChildStatements(Result[0]);
end;


// True iff a Continue is reachable on the loop body's entry path.
function BodyHasContinue(const aContext: TRuleContext;
  const aStmts: TPasImplElementArray): boolean;
var
  lKind: TFpSonarLoopExitKind;
  i: integer;
begin
  Result := False;
  for i := 0 to High(aStmts) do
  begin
    if (aStmts[i] is TPasImplForLoop) or (aStmts[i] is TPasImplWhileDo)
      or (aStmts[i] is TPasImplRepeatUntil) then
      Continue;
    if aStmts[i] is TPasImplSimple then
      if aContext.Resolver.TryLoopControlFlow(aStmts[i], lKind)
        and (lKind = lekContinue) then
        Exit(True);
    if BodyHasContinue(aContext, ChildStatements(aStmts[i])) then
      Exit(True);
  end;
end;


// True iff the statement subtree rooted at aNode re-raises the active exception
function HandlerReraises(aNode: TPasImplElement): boolean;
var
  lChildren: TPasImplElementArray;
  i: integer;
begin
  Result := False;
  if aNode = nil then
    Exit;
  if aNode is TPasImplExceptOn then
    Exit;
  if (aNode is TPasImplRaise) and (TPasImplRaise(aNode).ExceptObject = nil) then
    Exit(True);
  lChildren := ChildStatements(aNode);
  for i := 0 to High(lChildren) do
    if HandlerReraises(lChildren[i]) then
      Exit(True);
end;


// The number of 'on E: T do' handlers (TPasImplExceptOn children) an except block
// holds; zero means a bare catch-all 'except <stmts> end'. Nil-safe.
function ExceptOnChildCount(aExcept: TPasImplTryExcept): integer;
var
  i: integer;
begin
  Result := 0;
  if aExcept.Elements = nil then
    Exit;
  for i := 0 to aExcept.Elements.Count - 1 do
    if TObject(aExcept.Elements[i]) is TPasImplExceptOn then
      Inc(Result);
end;


  { ---- shared expression/statement comparison (the AST-tier cluster) ---- }

// The PasTree serialization of aExpr, case preserved; '' for nil.
function ExprText(aExpr: TPasExpr): string;
begin
  if aExpr = nil then
    Result := ''
  else
    Result := aExpr.GetDeclaration(True);
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


{ True iff aLeft and aRight are structurally identical statements. Only a simple
   statement, an assignment and a begin-block are modelled; anything else is False. }
function SameStmt(aLeft, aRight: TPasImplElement): boolean;
var
  lText: string;
  i: integer;
begin
  Result := False;
  if (aLeft = nil) or (aRight = nil) then
    Exit;
  if aLeft.ClassType <> aRight.ClassType then
    Exit;
  if aLeft is TPasImplSimple then
  begin
    lText := ExprText(TPasImplSimple(aLeft).Expr);
    Result := (lText <> '') and (lText = ExprText(TPasImplSimple(aRight).Expr));
  end
  else if aLeft is TPasImplAssign then
  begin
    if TPasImplAssign(aLeft).Kind <> TPasImplAssign(aRight).Kind then
      Exit;
    lText := ExprText(TPasImplAssign(aLeft).Left);
    if (lText = '') or (lText <> ExprText(TPasImplAssign(aRight).Left)) then
      Exit;
    lText := ExprText(TPasImplAssign(aLeft).Right);
    Result := (lText <> '') and (lText = ExprText(TPasImplAssign(aRight).Right));
  end
  else if aLeft is TPasImplBeginBlock then
  begin
    if (TPasImplBlock(aLeft).Elements = nil)
      or (TPasImplBlock(aRight).Elements = nil) then
      Exit;
    if (TPasImplBlock(aLeft).Elements.Count = 0)
      or (TPasImplBlock(aLeft).Elements.Count
      <> TPasImplBlock(aRight).Elements.Count) then
      Exit;
    for i := 0 to TPasImplBlock(aLeft).Elements.Count - 1 do
      if not ((TObject(TPasImplBlock(aLeft).Elements[i]) is TPasImplElement)
        and (TObject(TPasImplBlock(aRight).Elements[i]) is TPasImplElement)
        and SameStmt(TPasImplElement(TPasImplBlock(aLeft).Elements[i]),
        TPasImplElement(TPasImplBlock(aRight).Elements[i]))) then
        Exit;
    Result := True;
  end;
end;


// The direct sibling statements of aBlock, in source order.
function BlockStatements(aBlock: TPasImplBlock): TPasImplElementArray;
var
  i: integer;
begin
  SetLength(Result, 0);
  if (aBlock = nil) or (aBlock.Elements = nil) then
    Exit;
  for i := 0 to aBlock.Elements.Count - 1 do
    if TObject(aBlock.Elements[i]) is TPasImplElement then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := TPasImplElement(aBlock.Elements[i]);
    end;
end;


// Appends aExpr and every expression below it to aList.
procedure CollectExpressions(aExpr: TPasExpr; var aList: TPasExprArray);
var
  i: integer;
begin
  if aExpr = nil then
    Exit;
  SetLength(aList, Length(aList) + 1);
  aList[High(aList)] := aExpr;
  if aExpr is TParamsExpr then
  begin
    CollectExpressions(TParamsExpr(aExpr).Value, aList);
    for i := 0 to High(TParamsExpr(aExpr).Params) do
      CollectExpressions(TParamsExpr(aExpr).Params[i], aList);
  end
  else if aExpr is TBinaryExpr then
  begin
    CollectExpressions(TBinaryExpr(aExpr).Left, aList);
    CollectExpressions(TBinaryExpr(aExpr).Right, aList);
  end
  else if aExpr is TUnaryExpr then
    CollectExpressions(TUnaryExpr(aExpr).Operand, aList);
end;


// Appends the expressions aStmt itself carries, and everything below them,
// to aList; the expressions of its child statements are not visited.
procedure StmtExpressions(aStmt: TPasImplElement; var aList: TPasExprArray);
var
  lWith: TPasImplWithDo;
  j: integer;
begin
  if aStmt is TPasImplAssign then
  begin
    CollectExpressions(TPasImplAssign(aStmt).Left, aList);
    CollectExpressions(TPasImplAssign(aStmt).Right, aList);
  end
  else if aStmt is TPasImplSimple then
    CollectExpressions(TPasImplSimple(aStmt).Expr, aList)
  else if aStmt is TPasImplIfElse then
    CollectExpressions(TPasImplIfElse(aStmt).ConditionExpr, aList)
  else if aStmt is TPasImplWhileDo then
    CollectExpressions(TPasImplWhileDo(aStmt).ConditionExpr, aList)
  else if aStmt is TPasImplRepeatUntil then
    CollectExpressions(TPasImplRepeatUntil(aStmt).ConditionExpr, aList)
  else if aStmt is TPasImplForLoop then
  begin
    CollectExpressions(TPasImplForLoop(aStmt).StartExpr, aList);
    CollectExpressions(TPasImplForLoop(aStmt).EndExpr, aList);
  end
  else if aStmt is TPasImplCaseOf then
    CollectExpressions(TPasImplCaseOf(aStmt).CaseExpr, aList)
  else if aStmt is TPasImplWithDo then
  begin
    lWith := TPasImplWithDo(aStmt);
    if lWith.Expressions <> nil then
      for j := 0 to lWith.Expressions.Count - 1 do
        if TObject(lWith.Expressions[j]) is TPasExpr then
          CollectExpressions(TPasExpr(lWith.Expressions[j]), aList);
  end;
end;


// Every expression node in aModule's statement expressions
function AllExpressions(aModule: TPasModule): TPasExprArray;
var
  lStmts: TPasImplElementArray;
  i: integer;
begin
  SetLength(Result, 0);
  lStmts := AllStatements(aModule);
  for i := 0 to High(lStmts) do
    StmtExpressions(lStmts[i], Result);
end;


  { ---- operator kinds (the operator-trap cluster) ---- }

// True iff aToken is the and, or or xor keyword.
function IsBoolOpToken(const aToken: TFpSonarToken): boolean;
var
  lText: string;
begin
  Result := False;
  if not aToken.IsKeyword then
    Exit;
  lText := LowerCase(aToken.Text);
  Result := (lText = 'and') or (lText = 'or') or (lText = 'xor');
end;


// True iff aToken is one of the six relational operators.
function IsCmpOpToken(const aToken: TFpSonarToken): boolean;
var
  lPunct: string;
begin
  lPunct := aToken.Punct;
  Result := (lPunct = '=') or (lPunct = '<>') or (lPunct = '<')
    or (lPunct = '>') or (lPunct = '<=') or (lPunct = '>=');
end;


// True iff aToken ends the operand region two operators must share to pair up.
function EndsOperandRegion(const aToken: TFpSonarToken): boolean;
var
  lText: string;
begin
  lText := aToken.Punct;
  Result := (lText = ';') or (lText = ',') or (lText = ':') or (lText = ':=');
  if Result or not aToken.IsKeyword then
    Exit;
  lText := LowerCase(aToken.Text);
  Result := (lText = 'then') or (lText = 'do') or (lText = 'of')
    or (lText = 'else') or (lText = 'begin') or (lText = 'end')
    or (lText = 'until') or (lText = 'repeat') or (lText = 'to')
    or (lText = 'downto') or (lText = 'try') or (lText = 'except')
    or (lText = 'finally') or (lText = 'otherwise');
end;


// The condition expression of an if, while or repeat statement; nil otherwise.
function CondOf(aStmt: TPasImplElement): TPasExpr;
begin
  if aStmt is TPasImplIfElse then
    Result := TPasImplIfElse(aStmt).ConditionExpr
  else if aStmt is TPasImplWhileDo then
    Result := TPasImplWhileDo(aStmt).ConditionExpr
  else if aStmt is TPasImplRepeatUntil then
    Result := TPasImplRepeatUntil(aStmt).ConditionExpr
  else
    Result := nil;
end;


// Emits one issue at aToken's own row and column
procedure EmitTok(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; const aToken: TFpSonarToken;
  const aArgs: array of string);
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aToken.Row, aToken.Col,
    aToken.Row, aToken.Col, aMeta.Severity, aMeta.Category,
    aMeta.DefaultConfidence, aMeta.MessageKey, aArgs, aToken.Text);
end;


  { ---- branch shape (the if-shape cluster) ---- }

{ 'raise' for a raise statement; otherwise the lowercased spelling of aStmt when
   it names exit, break, continue or halt in bare or call form, matched on the
   identifier text and not on what it resolves to; '' for anything else. }
function TerminatorKind(aStmt: TPasImplElement): string;
var
  lExpr: TPasExpr;
  lName: string;
begin
  Result := '';
  if aStmt is TPasImplRaise then
    Exit('raise');
  if not (aStmt is TPasImplSimple) then
    Exit;
  lExpr := TPasImplSimple(aStmt).Expr;
  if (lExpr is TParamsExpr) and (TParamsExpr(lExpr).Kind = pekFuncParams) then
    lExpr := TParamsExpr(lExpr).Value;
  if not (lExpr is TPrimitiveExpr) then
    Exit;
  if TPrimitiveExpr(lExpr).Kind <> pekIdent then
    Exit;
  lName := LowerCase(TPrimitiveExpr(lExpr).Value);
  if (lName = 'exit') or (lName = 'break') or (lName = 'continue')
    or (lName = 'halt') then
    Result := lName;
end;


{ The fall-through last statement of a branch: the last entry of a begin block
   (nil when it is empty), otherwise aBranch itself. One level, no recursion. }
function BranchTail(aBranch: TPasImplElement): TPasImplElement;
var
  lEntries: TPasImplElementArray;
begin
  Result := nil;
  if aBranch = nil then
    Exit;
  if not (aBranch is TPasImplBeginBlock) then
    Exit(aBranch);
  lEntries := BlockStatements(TPasImplBlock(aBranch));
  if Length(lEntries) > 0 then
    Result := lEntries[High(lEntries)];
end;


{ The whole content of a branch when it is exactly one statement: the single
   entry of a begin block, otherwise aBranch itself when it is not a block. }
function SoleStatement(aBranch: TPasImplElement): TPasImplElement;
var
  lEntries: TPasImplElementArray;
begin
  Result := nil;
  if aBranch = nil then
    Exit;
  if not (aBranch is TPasImplBeginBlock) then
    Exit(aBranch);
  lEntries := BlockStatements(TPasImplBlock(aBranch));
  if Length(lEntries) = 1 then
    Result := lEntries[0];
end;


{ TRuleExhaustiveCaseStatement }

procedure TRuleExhaustiveCaseStatement.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lMissing: TFpSonarStringArray;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplCaseOf then
      if aContext.Resolver.TryNonExhaustiveEnumCase(lStmts[i], lMissing) then
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(lStmts[i]),
          [JoinMissing(lMissing)], 'case');
end;


{ TRuleExceptionRaised }

procedure TRuleExceptionRaised.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lExpr: TPasExpr;
  lName: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    // A bare call-statement is the discard signal:
    // the construction result is not assigned, passed, or raised — it is the whole statement expression.
    if lStmts[i] is TPasImplSimple then
    begin
      lExpr := TPasImplSimple(lStmts[i]).Expr;
      if (lExpr is TParamsExpr) and (TParamsExpr(lExpr).Kind = pekFuncParams) then
        if aContext.Resolver.TryDiscardedExceptionConstruction(lExpr, lName) then
          EmitStmt(FMetadata, aContext, aCollector,
            aContext.Resolver.SourceRow(lExpr), [lName], lName);
    end;
end;


{ TRuleSingleIterationLoop }

procedure TRuleSingleIterationLoop.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts, lBody: TPasImplElementArray;
  lTerm: TPasImplElement;
  lKind: TFpSonarLoopExitKind;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if (lStmts[i] is TPasImplForLoop) or (lStmts[i] is TPasImplWhileDo)
      or (lStmts[i] is TPasImplRepeatUntil) then
    begin
      lBody := LoopBody(lStmts[i]);
      if Length(lBody) = 0 then
        Continue;
      // A Continue anywhere on the body's entry path keeps the loop multi-iteration
      if BodyHasContinue(aContext, lBody) then
        Continue;
      // Fire only when the terminal straight-line statement is an unconditional exit
      lTerm := lBody[High(lBody)];
      if lTerm is TPasImplRaise then
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(lStmts[i]), [], 'loop')
      else if lTerm is TPasImplSimple then
        if aContext.Resolver.TryLoopControlFlow(lTerm, lKind)
          and (lKind in [lekBreak, lekExit]) then
          EmitStmt(FMetadata, aContext, aCollector,
            aContext.Resolver.SourceRow(lStmts[i]), [], 'loop');
    end;
end;


{ TRuleNoPascalStyleResultAssignment }

procedure TRuleNoPascalStyleResultAssignment.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lFuncName: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplAssign then
      if aContext.Resolver.TryPascalStyleResultAssign(lStmts[i], lFuncName) then
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(TPasImplAssign(lStmts[i]).Left),
          [lFuncName], lFuncName);
end;


{ TRuleRedundantAssignedCheckBeforeFree }

procedure TRuleRedundantAssignedCheckBeforeFree.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplIfElse then
      // The wrapper confirms a no-else if whose single branch is exactly a
      // Free/FreeAndNil of the SAME object the guard tests
      if aContext.Resolver.TryRedundantAssignedCheckBeforeFree(lStmts[i]) then
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(TPasImplIfElse(lStmts[i]).ConditionExpr),
          [], 'if');
end;


{ TRuleLoopBeyondCollectionEnd }

procedure TRuleLoopBeyondCollectionEnd.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lOverrun: TPasElement;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplForLoop then
      // The wrapper proves a counted loop whose body indexes a static array past
      // its constant range with a loop-variable index, returning the overrunning indexing node
      if aContext.Resolver.TryLoopBeyondCollectionEnd(lStmts[i], lOverrun) then
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(lOverrun), [], 'for');
end;


{ TRuleRoutineResultAssigned }

procedure TRuleRoutineResultAssigned.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lFlow: TFpSonarDataFlow;
  lFindings: TFpSonarFlowFindingArray;
  lOk: boolean;
  i: integer;
begin
  lFlow := TFpSonarDataFlow.Create(aContext.Resolver);
  try
    lOk := lFlow.TryFlowFindings(lFindings);
  finally
    lFlow.Free;
  end;
  if not lOk then
    Exit;
  for i := 0 to High(lFindings) do
    if lFindings[i].Verdict = fvResultUnassigned then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lFindings[i].Site), [], 'function');
end;


{ TRuleNoCatchRawException }

procedure TRuleNoCatchRawException.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lExcept: TPasImplTryExcept;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
  begin
    // The 'on E: T do' form:
    if lStmts[i] is TPasImplExceptOn then
    begin
      if HandlerReraises(TPasImplExceptOn(lStmts[i]).Body) then
        Continue;
      if aContext.Resolver.TryHandlerCatchesRawException(lStmts[i]) then
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(lStmts[i]), [], 'except');
    end
    // The bare catch-all 'except <stmts> end' form (no 'on')
    else if lStmts[i] is TPasImplTryExcept then
    begin
      lExcept := TPasImplTryExcept(lStmts[i]);
      if cFlagBareExcept and (ExceptOnChildCount(lExcept) = 0)
        and (lExcept.Elements <> nil) and (lExcept.Elements.Count > 0)
        and not HandlerReraises(lExcept) then
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(lStmts[i]), [], 'except');
    end;
  end;
end;


{ TRuleNoRaiseRawException }

procedure TRuleNoRaiseRawException.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    // Mirrors ExceptionRaised: walk every statement, check on a raise
    if lStmts[i] is TPasImplRaise then
      if aContext.Resolver.TryRaisesRawException(lStmts[i]) then
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(TPasImplRaise(lStmts[i]).ExceptObject),
          [], 'raise');
end;


{ TRuleIdenticalBranches }

procedure TRuleIdenticalBranches.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lIf: TPasImplIfElse;
  lText: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplIfElse then
    begin
      lIf := TPasImplIfElse(lStmts[i]);
      if not SameStmt(lIf.IfBranch, lIf.ElseBranch) then
        Continue;
      lText := ExprText(lIf.ConditionExpr);
      if lText = '' then
        Continue;
      EmitStmt(FMetadata, aContext, aCollector, lIf.SourceLinenumber,
        [lText], 'if');
    end;
end;


{ TRuleDuplicateConditionInChain }

procedure TRuleDuplicateConditionInChain.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lSeen: TFpSonarStringArray;
  lLink: TPasImplIfElse;
  lText: string;
  i, j: integer;
  lSeenBefore: boolean;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplIfElse then
    begin
      // An inner chain link is its parent if's ElseBranch; only the head is walked
      if (lStmts[i].Parent is TPasImplIfElse)
        and (TPasImplIfElse(lStmts[i].Parent).ElseBranch = lStmts[i]) then
        Continue;
      SetLength(lSeen, 0);
      lLink := TPasImplIfElse(lStmts[i]);
      repeat
        lText := ExprText(lLink.ConditionExpr);
        if lText <> '' then
        begin
          lSeenBefore := False;
          for j := 0 to High(lSeen) do
            if lSeen[j] = lText then
              lSeenBefore := True;
          if lSeenBefore then
            EmitStmt(FMetadata, aContext, aCollector,
              lLink.ConditionExpr.SourceLinenumber, [lText], 'if')
          else
          begin
            SetLength(lSeen, Length(lSeen) + 1);
            lSeen[High(lSeen)] := lText;
          end;
        end;
        if not (lLink.ElseBranch is TPasImplIfElse) then
          Break;
        lLink := TPasImplIfElse(lLink.ElseBranch);
      until False;
    end;
end;


{ TRuleDuplicateCaseLabel }

procedure TRuleDuplicateCaseLabel.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts, lBranches: TPasImplElementArray;
  lSeen: TFpSonarStringArray;
  lBranch: TPasImplCaseStatement;
  lText: string;
  i, j, k, m: integer;
  lSeenBefore: boolean;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplCaseOf then
    begin
      SetLength(lSeen, 0);
      lBranches := ChildStatements(lStmts[i]);
      for j := 0 to High(lBranches) do
        if lBranches[j] is TPasImplCaseStatement then
        begin
          lBranch := TPasImplCaseStatement(lBranches[j]);
          if lBranch.Expressions = nil then
            Continue;
          for k := 0 to lBranch.Expressions.Count - 1 do
            if TObject(lBranch.Expressions[k]) is TPasExpr then
            begin
              lText := ExprText(TPasExpr(lBranch.Expressions[k]));
              if lText = '' then
                Continue;
              lSeenBefore := False;
              for m := 0 to High(lSeen) do
                if lSeen[m] = lText then
                  lSeenBefore := True;
              if lSeenBefore then
                EmitStmt(FMetadata, aContext, aCollector,
                  TPasExpr(lBranch.Expressions[k]).SourceLinenumber,
                  [lText], 'case')
              else
              begin
                SetLength(lSeen, Length(lSeen) + 1);
                lSeen[High(lSeen)] := lText;
              end;
            end;
        end;
    end;
end;


{ TRuleSelfComparison }

procedure TRuleSelfComparison.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lBin: TBinaryExpr;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Module);
  for i := 0 to High(lExprs) do
    if lExprs[i] is TBinaryExpr then
    begin
      lBin := TBinaryExpr(lExprs[i]);
      if not (lBin.OpCode in [eopEqual, eopNotEqual, eopLessThan,
        eopGreaterThan, eopLessthanEqual, eopGreaterThanEqual]) then
        Continue;
      if SameLValue(lBin.Left, lBin.Right) then
        EmitStmt(FMetadata, aContext, aCollector, lBin.SourceLinenumber,
          [ExprText(lBin.Left), OpcodeStrings[lBin.OpCode]], 'comparison');
    end;
end;


{ TRuleEmptyThenWithFollowingStatement }

procedure TRuleEmptyThenWithFollowingStatement.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lBlocks, lStmts, lEntries: TPasImplElementArray;
  lIf: TPasImplIfElse;
  lText: string;
  i, j: integer;
begin
  lBlocks := EnumerateStatementRoots(aContext.Module);
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    // The classes whose Elements really are a sibling statement list.
    if (lStmts[i] is TPasImplBlock)
      and not (lStmts[i] is TPasImplStatement)
      and not (lStmts[i] is TPasImplIfElse)
      and not (lStmts[i] is TPasImplCaseOf) then
    begin
      SetLength(lBlocks, Length(lBlocks) + 1);
      lBlocks[High(lBlocks)] := lStmts[i];
    end;
  for i := 0 to High(lBlocks) do
  begin
    lEntries := BlockStatements(lBlocks[i] as TPasImplBlock);
    for j := 0 to High(lEntries) - 1 do
      if lEntries[j] is TPasImplIfElse then
      begin
        lIf := TPasImplIfElse(lEntries[j]);
        if lIf.IfBranch <> nil then
          Continue;
        lText := ExprText(lIf.ConditionExpr);
        if lText = '' then
          Continue;
        EmitStmt(FMetadata, aContext, aCollector, lIf.SourceLinenumber,
          [lText], 'if');
      end;
  end;
end;


{ TRuleMixedBooleanAndRelational }

procedure TRuleMixedBooleanAndRelational.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lBoolAt, lCmpAt: array of integer;
  lDepth, i: integer;
  lPunct: string;

  // Grows the per-depth state to cover aDepth, then clears that level's slots.
  procedure Reset(aDepth: integer);
  begin
    while Length(lBoolAt) <= aDepth do
    begin
      SetLength(lBoolAt, Length(lBoolAt) + 1);
      SetLength(lCmpAt, Length(lCmpAt) + 1);
      lBoolAt[High(lBoolAt)] := -1;
      lCmpAt[High(lCmpAt)] := -1;
    end;
    lBoolAt[aDepth] := -1;
    lCmpAt[aDepth] := -1;
  end;

begin
  SetLength(lBoolAt, 0);
  SetLength(lCmpAt, 0);
  lDepth := 0;
  Reset(0);
  for i := 0 to High(aContext.Tokens) do
  begin
    if aContext.Tokens[i].IsTrivia then
      Continue;
    lPunct := aContext.Tokens[i].Punct;
    if (lPunct = '(') or (lPunct = '[') then
    begin
      Inc(lDepth);
      Reset(lDepth);
    end
    else if (lPunct = ')') or (lPunct = ']') then
    begin
      Reset(lDepth);
      if lDepth > 0 then
        Dec(lDepth);
    end
    else if EndsOperandRegion(aContext.Tokens[i]) then
      Reset(lDepth)
    else if IsBoolOpToken(aContext.Tokens[i]) then
    begin
      if lCmpAt[lDepth] >= 0 then
      begin
        EmitTok(FMetadata, aContext, aCollector, aContext.Tokens[i],
          [aContext.Tokens[i].Text, aContext.Tokens[lCmpAt[lDepth]].Punct]);
        Reset(lDepth);
      end
      else if lBoolAt[lDepth] < 0 then
        lBoolAt[lDepth] := i;
    end
    else if IsCmpOpToken(aContext.Tokens[i]) then
    begin
      if lBoolAt[lDepth] >= 0 then
      begin
        EmitTok(FMetadata, aContext, aCollector,
          aContext.Tokens[lBoolAt[lDepth]],
          [aContext.Tokens[lBoolAt[lDepth]].Text, lPunct]);
        Reset(lDepth);
      end
      else if lCmpAt[lDepth] < 0 then
        lCmpAt[lDepth] := i;
    end;
  end;
end;


{ TRuleBitwiseOnBooleanOperands }

procedure TRuleBitwiseOnBooleanOperands.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs, lSeen: TPasExprArray;
  lStmts: TPasImplElementArray;
  lCond: TPasExpr;
  lBin: TBinaryExpr;
  i: integer;

  function IsBoolShape(aExpr: TPasExpr): boolean;
  begin
    Result := (aExpr <> nil) and ((aExpr.Kind = pekBoolConst)
      or ((aExpr is TBinaryExpr) and (TBinaryExpr(aExpr).OpCode
      in [eopEqual, eopNotEqual, eopLessThan, eopGreaterThan,
      eopLessthanEqual, eopGreaterThanEqual])));
  end;

  function IsNumShape(aExpr: TPasExpr): boolean;
  begin
    Result := (aExpr <> nil) and (aExpr.Kind = pekNumber);
  end;

  // Emits once per node: a match on an already-reported node is dropped.
  procedure Emit(aBin: TBinaryExpr);
  var
    k: integer;
  begin
    for k := 0 to High(lSeen) do
      if lSeen[k] = aBin then
        Exit;
    SetLength(lSeen, Length(lSeen) + 1);
    lSeen[High(lSeen)] := aBin;
    EmitStmt(FMetadata, aContext, aCollector, aBin.SourceLinenumber,
      [OpcodeStrings[aBin.OpCode]], OpcodeStrings[aBin.OpCode]);
  end;

begin
  SetLength(lSeen, 0);
  lExprs := AllExpressions(aContext.Module);
  for i := 0 to High(lExprs) do
    if lExprs[i] is TBinaryExpr then
    begin
      lBin := TBinaryExpr(lExprs[i]);
      if not (lBin.OpCode in [eopAnd, eopOr, eopXor]) then
        Continue;
      if (IsBoolShape(lBin.Left) and IsNumShape(lBin.Right))
        or (IsBoolShape(lBin.Right) and IsNumShape(lBin.Left)) then
        Emit(lBin);
    end;
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
  begin
    lCond := CondOf(lStmts[i]);
    if not (lCond is TBinaryExpr) then
      Continue;
    lBin := TBinaryExpr(lCond);
    if not (lBin.OpCode in [eopAnd, eopOr, eopXor]) then
      Continue;
    if IsNumShape(lBin.Left) or IsNumShape(lBin.Right) then
      Emit(lBin);
  end;
end;


{ TRuleAssignmentInsteadOfComparison }

procedure TRuleAssignmentInsteadOfComparison.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lNamed: TNamedArgExpr;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Module);
  for i := 0 to High(lExprs) do
    if lExprs[i] is TNamedArgExpr then
    begin
      lNamed := TNamedArgExpr(lExprs[i]);
      if lNamed.NameExpr = nil then
        Continue;
      // ParseParams builds a named argument for an index or a set too.
      if not ((lNamed.Parent is TParamsExpr)
        and (TParamsExpr(lNamed.Parent).Kind = pekFuncParams)) then
        Continue;
      EmitStmt(FMetadata, aContext, aCollector,
        lNamed.NameExpr.SourceLinenumber, [ExprText(lNamed.NameExpr)], ':=');
    end;
end;


{ TRuleConditionWithSideEffect }

procedure TRuleConditionWithSideEffect.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lCond: TPasExpr;
  lInCond, lInRight, lSeen: TPasExprArray;
  lCall: TParamsExpr;
  lDecl: TPasElement;
  lProc: TPasProcedure;
  lMutates: boolean;
  i, j, k, m: integer;

  function AlreadyReported(aExpr: TPasExpr): boolean;
  var
    n: integer;
  begin
    Result := False;
    for n := 0 to High(lSeen) do
      if lSeen[n] = aExpr then
        Exit(True);
  end;

begin
  SetLength(lSeen, 0);
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
  begin
    lCond := CondOf(lStmts[i]);
    if lCond = nil then
      Continue;
    SetLength(lInCond, 0);
    CollectExpressions(lCond, lInCond);
    for j := 0 to High(lInCond) do
    begin
      if not (lInCond[j] is TBinaryExpr) then
        Continue;
      if not (TBinaryExpr(lInCond[j]).OpCode in [eopAnd, eopOr]) then
        Continue;
      SetLength(lInRight, 0);
      CollectExpressions(TBinaryExpr(lInCond[j]).Right, lInRight);
      for k := 0 to High(lInRight) do
      begin
        if not (lInRight[k] is TParamsExpr) then
          Continue;
        lCall := TParamsExpr(lInRight[k]);
        if (lCall.Kind <> pekFuncParams) or AlreadyReported(lCall) then
          Continue;
        lDecl := aContext.Resolver.ReferencedDecl(lCall.Value);
        if not (lDecl is TPasProcedure) then
          Continue;
        lProc := TPasProcedure(lDecl);
        if (lProc.ProcType = nil) or (lProc.ProcType.Args = nil) then
          Continue;
        lMutates := False;
        for m := 0 to lProc.ProcType.Args.Count - 1 do
          if (TObject(lProc.ProcType.Args[m]) is TPasArgument)
            and (TPasArgument(lProc.ProcType.Args[m]).Access
            in [argVar, argOut]) then
            lMutates := True;
        if not lMutates then
          Continue;
        SetLength(lSeen, Length(lSeen) + 1);
        lSeen[High(lSeen)] := lCall;
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(lCall), [lProc.Name], lProc.Name);
      end;
    end;
  end;
end;


{ TRuleRedundantElseAfterExit }

procedure TRuleRedundantElseAfterExit.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lIf: TPasImplIfElse;
  lKind: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplIfElse then
    begin
      lIf := TPasImplIfElse(lStmts[i]);
      // A TPasImplCommand then branch is the parser's 'then else', not a statement
      if (lIf.ElseBranch = nil) or (lIf.IfBranch = nil)
        or (lIf.IfBranch is TPasImplCommand) then
        Continue;
      lKind := TerminatorKind(BranchTail(lIf.IfBranch));
      if lKind = '' then
        Continue;
      EmitStmt(FMetadata, aContext, aCollector, lIf.SourceLinenumber,
        [lKind], 'if');
    end;
end;


{ TRuleCollapsibleNestedIf }

procedure TRuleCollapsibleNestedIf.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lIf: TPasImplIfElse;
  lInner: TPasImplElement;
  lText: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplIfElse then
    begin
      lIf := TPasImplIfElse(lStmts[i]);
      if lIf.ElseBranch <> nil then
        Continue;
      lInner := SoleStatement(lIf.IfBranch);
      if not (lInner is TPasImplIfElse) then
        Continue;
      // A dangling else on the inner if binds to it, so the merge is unsound
      if TPasImplIfElse(lInner).ElseBranch <> nil then
        Continue;
      lText := ExprText(TPasImplIfElse(lInner).ConditionExpr);
      if lText = '' then
        Continue;
      EmitStmt(FMetadata, aContext, aCollector, lIf.SourceLinenumber,
        [lText], 'if');
    end;
end;


{ TRuleNegatedConditionWithElse }

procedure TRuleNegatedConditionWithElse.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lIf: TPasImplIfElse;
  lText: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplIfElse then
    begin
      lIf := TPasImplIfElse(lStmts[i]);
      if not (lIf.ConditionExpr is TUnaryExpr) then
        Continue;
      if (TUnaryExpr(lIf.ConditionExpr).OpCode <> eopNot)
        or (TUnaryExpr(lIf.ConditionExpr).Operand = nil) then
        Continue;
      // An else if is a chain continuation and an empty else inverts into the
      // empty then branch EmptyThenWithFollowingStatement reports
      if (lIf.ElseBranch = nil) or (lIf.ElseBranch is TPasImplIfElse)
        or (lIf.ElseBranch is TPasImplCommand)
        or (BranchTail(lIf.ElseBranch) = nil) then
        Continue;
      lText := ExprText(TUnaryExpr(lIf.ConditionExpr).Operand);
      if lText = '' then
        Continue;
      EmitStmt(FMetadata, aContext, aCollector, lIf.SourceLinenumber,
        [lText], 'if');
    end;
end;


  { ---- loop condition locality (the selector-and-loop cluster) ---- }

// The lowercased identifier a bare identifier expression names, '' otherwise.
function IdentName(aExpr: TPasExpr): string;
begin
  if (aExpr is TPrimitiveExpr) and (TPrimitiveExpr(aExpr).Kind = pekIdent) then
    Result := LowerCase(TPrimitiveExpr(aExpr).Value)
  else
    Result := '';
end;


{ The lowercased leftmost identifier of an lvalue chain of member accesses and
   index expressions; '' when the chain does not bottom out in an identifier. }
function LValueName(aExpr: TPasExpr): string;
var
  lWalk: TPasExpr;
begin
  lWalk := aExpr;
  while lWalk <> nil do
    if (lWalk is TBinaryExpr) and (TBinaryExpr(lWalk).OpCode = eopSubIdent) then
      lWalk := TBinaryExpr(lWalk).Left
    else if lWalk is TParamsExpr then
      lWalk := TParamsExpr(lWalk).Value
    else
      Break;
  Result := IdentName(lWalk);
end;


// True iff aName is non-empty and listed in aNames.
function HasName(const aNames: TFpSonarStringArray;
  const aName: string): boolean;
var
  i: integer;
begin
  Result := False;
  if aName = '' then
    Exit;
  for i := 0 to High(aNames) do
    if aNames[i] = aName then
      Exit(True);
end;


// Appends aName to aNames when it is not already there.
procedure AddName(var aNames: TFpSonarStringArray; const aName: string);
begin
  if HasName(aNames, aName) then
    Exit;
  SetLength(aNames, Length(aNames) + 1);
  aNames[High(aNames)] := aName;
end;


// True iff one of aNames occurs anywhere in the subtree rooted at aExpr.
function SubtreeHasName(aExpr: TPasExpr;
  const aNames: TFpSonarStringArray): boolean;
var
  lNodes: TPasExprArray;
  i: integer;
begin
  Result := False;
  SetLength(lNodes, 0);
  CollectExpressions(aExpr, lNodes);
  for i := 0 to High(lNodes) do
    if HasName(aNames, IdentName(lNodes[i])) then
      Exit(True);
end;


{ True iff aExpr hands one of aNames somewhere it could be written: into a
   parameter or index list, or under an address-of operator. }
function TouchesName(aExpr: TPasExpr;
  const aNames: TFpSonarStringArray): boolean;
var
  i: integer;
begin
  Result := False;
  if aExpr is TParamsExpr then
  begin
    for i := 0 to High(TParamsExpr(aExpr).Params) do
      if SubtreeHasName(TParamsExpr(aExpr).Params[i], aNames) then
        Exit(True);
  end
  else if (aExpr is TUnaryExpr)
    and (TUnaryExpr(aExpr).OpCode in [eopAddress, eopMemAddress]) then
    Result := SubtreeHasName(TUnaryExpr(aExpr).Operand, aNames);
end;


// True iff aProc is declared in the declaration list of another routine's body.
function IsNestedRoutine(aProc: TPasProcedure): boolean;
begin
  Result := (aProc <> nil) and (aProc.Parent is TProcedureBody);
end;


// The routine aDecl is declared in, nil when it is declared outside one.
function OwningRoutine(aDecl: TPasElement): TPasProcedure;
var
  lWalk: TPasElement;
begin
  Result := nil;
  lWalk := aDecl;
  while (lWalk <> nil) and not (lWalk is TPasProcedure) do
    lWalk := lWalk.Parent;
  if lWalk <> nil then
    Result := TPasProcedure(lWalk);
end;


{ True iff aDecl is an argument of aRoutine or of its separate declaration
   header; a nested routine is matched on identity alone. }
function IsArgumentOf(aDecl: TPasElement; aRoutine: TPasProcedure): boolean;
var
  lOwner: TPasProcedure;
begin
  lOwner := OwningRoutine(aDecl);
  Result := (lOwner <> nil)
    and ((lOwner = aRoutine)
    or (not IsNestedRoutine(aRoutine) and not IsNestedRoutine(lOwner)));
end;


// True iff aDecl is an argument or a variable of a procedural type.
function HasProceduralType(aDecl: TPasElement): boolean;
begin
  Result := ((aDecl is TPasArgument)
    and (TPasArgument(aDecl).ArgType is TPasProcedureType))
    or ((aDecl is TPasVariable)
    and (TPasVariable(aDecl).VarType is TPasProcedureType));
end;


{ True iff aDecl is storage aRoutine alone owns: a by-value argument of it, or a
   variable of its own body. A procedural name is a call, not storage. }
function IsRoutineLocal(aDecl: TPasElement; aRoutine: TPasProcedure): boolean;
begin
  Result := False;
  if (aDecl = nil) or (aRoutine = nil) or HasProceduralType(aDecl) then
    Exit;
  if aDecl is TPasArgument then
    Exit(not (TPasArgument(aDecl).Access in [argVar, argOut])
      and IsArgumentOf(aDecl, aRoutine));
  Result := (aDecl is TPasVariable) and not (aDecl is TPasConst)
    and (aDecl.Parent = aRoutine.Body);
end;


// True iff aProc's body declares a routine of its own.
function HasNestedRoutine(aProc: TPasProcedure): boolean;
begin
  Result := (aProc <> nil) and (aProc.Body <> nil)
    and (aProc.Body.Functions <> nil) and (aProc.Body.Functions.Count > 0);
end;


{ True iff aCond is a name-only expression naming at least one local or argument
   of aRoutine and nothing else; aNames then holds those names, lowercased. }
function TryConditionLocals(const aContext: TRuleContext; aCond: TPasExpr;
  aRoutine: TPasProcedure; out aNames: TFpSonarStringArray): boolean;
var
  lNodes: TPasExprArray;
  lName: string;
  i: integer;
begin
  Result := False;
  SetLength(aNames, 0);
  if aCond = nil then
    Exit;
  SetLength(lNodes, 0);
  CollectExpressions(aCond, lNodes);
  for i := 0 to High(lNodes) do
  begin
    if lNodes[i] is TParamsExpr then
      Exit;
    if (lNodes[i] is TBinaryExpr)
      and (TBinaryExpr(lNodes[i]).OpCode = eopSubIdent) then
      Exit;
    if (lNodes[i] is TUnaryExpr) and (TUnaryExpr(lNodes[i]).OpCode
      in [eopAddress, eopMemAddress, eopDeref]) then
      Exit;
    lName := IdentName(lNodes[i]);
    if lName = '' then
      Continue;
    if not IsRoutineLocal(aContext.Resolver.ReferencedDecl(lNodes[i]),
      aRoutine) then
      Exit;
    AddName(aNames, lName);
  end;
  Result := Length(aNames) > 0;
end;


{ True iff aStmt raises or names exit, break or halt in bare, called or
   unit-qualified form; DW-330 records the qualifier TerminatorKind misses. }
function EscapesLoop(aStmt: TPasImplElement): boolean;
var
  lExpr: TPasExpr;
  lName: string;
begin
  if aStmt is TPasImplRaise then
    Exit(True);
  Result := False;
  if not (aStmt is TPasImplSimple) then
    Exit;
  lExpr := TPasImplSimple(aStmt).Expr;
  if (lExpr is TParamsExpr) and (TParamsExpr(lExpr).Kind = pekFuncParams) then
    lExpr := TParamsExpr(lExpr).Value;
  if (lExpr is TBinaryExpr) and (TBinaryExpr(lExpr).OpCode = eopSubIdent) then
    lExpr := TBinaryExpr(lExpr).Right;
  lName := IdentName(lExpr);
  Result := (lName = 'exit') or (lName = 'break') or (lName = 'halt');
end;


{ True iff the statement subtree below aLoop can leave the loop or can write
   one of aNames. Any appearance in a parameter list counts as a write. }
function BodyChangesCondition(aLoop: TPasImplElement;
  const aNames: TFpSonarStringArray): boolean;
var
  lStmts: TPasImplElementArray;
  lExprs: TPasExprArray;
  i, j: integer;
begin
  Result := True;
  SetLength(lStmts, 0);
  CollectStatements(aLoop, lStmts);
  for i := 0 to High(lStmts) do
  begin
    if (lStmts[i] is TPasImplGoto) or (lStmts[i] is TPasImplAsmStatement) then
      Exit;
    if EscapesLoop(lStmts[i]) then
      Exit;
    if (lStmts[i] is TPasImplForLoop) and HasName(aNames,
      IdentName(TPasImplForLoop(lStmts[i]).VariableName)) then
      Exit;
    if (lStmts[i] is TPasImplAssign) and HasName(aNames,
      LValueName(TPasImplAssign(lStmts[i]).Left)) then
      Exit;
    SetLength(lExprs, 0);
    StmtExpressions(lStmts[i], lExprs);
    for j := 0 to High(lExprs) do
      if TouchesName(lExprs[j], aNames) then
        Exit;
  end;
  Result := False;
end;


{ TRuleSwitchOnBooleanExpression }

procedure TRuleSwitchOnBooleanExpression.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lSelector: TPasExpr;
  lType: TFpSonarResolvedType;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplCaseOf then
    begin
      lSelector := TPasImplCaseOf(lStmts[i]).CaseExpr;
      if (lSelector <> nil)
        and aContext.Resolver.TryResolvedType(lSelector, lType)
        and (lType.Kind = ltkBool) then
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(lStmts[i]), [ExprText(lSelector)],
          'case');
    end;
end;


{ TRuleLoopConditionNeverChanges }

procedure TRuleLoopConditionNeverChanges.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  lStmts: TPasImplElementArray;
  lNames: TFpSonarStringArray;
  lCond: TPasExpr;
  lWord: string;
  i, j: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lRoutines) do
  begin
    if HasNestedRoutine(lRoutines[i].Decl) then
      Continue;
    SetLength(lStmts, 0);
    CollectStatements(lRoutines[i].Block, lStmts);
    for j := 0 to High(lStmts) do
    begin
      if lStmts[j] is TPasImplWhileDo then
        lWord := 'while'
      else if lStmts[j] is TPasImplRepeatUntil then
        lWord := 'repeat'
      else
        Continue;
      lCond := CondOf(lStmts[j]);
      if not TryConditionLocals(aContext, lCond, lRoutines[i].Decl,
        lNames) then
        Continue;
      if BodyChangesCondition(lStmts[j], lNames) then
        Continue;
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lStmts[j]), [ExprText(lCond)], lWord);
    end;
  end;
end;


  { ---- CFG reachability (the unreachable-code rule) ---- }

{ TRuleUnreachableCode }

procedure TRuleUnreachableCode.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lDead: TPasElementArray;
  lBlocks, lStmts, lEntries: TPasImplElementArray;
  i, j: integer;

  function IsDead(aStmt: TPasImplElement): boolean;
  var
    k: integer;
  begin
    Result := False;
    for k := 0 to High(lDead) do
      if lDead[k] = aStmt then
        Exit(True);
  end;

begin
  if not TFpSonarDataFlow.TryUnreachableStatements(aContext.Module, lDead) then
    Exit;
  if Length(lDead) = 0 then
    Exit;
  lBlocks := EnumerateStatementRoots(aContext.Module);
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    // The classes whose Elements really are a sibling statement list.
    if (lStmts[i] is TPasImplBlock)
      and not (lStmts[i] is TPasImplStatement)
      and not (lStmts[i] is TPasImplIfElse)
      and not (lStmts[i] is TPasImplCaseOf) then
    begin
      SetLength(lBlocks, Length(lBlocks) + 1);
      lBlocks[High(lBlocks)] := lStmts[i];
    end;
  for i := 0 to High(lBlocks) do
  begin
    lEntries := BlockStatements(lBlocks[i] as TPasImplBlock);
    for j := 1 to High(lEntries) do
      if IsDead(lEntries[j]) and not IsDead(lEntries[j - 1]) then
        EmitStmt(FMetadata, aContext, aCollector,
          lEntries[j].SourceLinenumber, [], 'statement');
  end;
end;


initialization
  RegisterRule(TRuleExhaustiveCaseStatement.Create(TRuleMetadata.Make(
    'ExhaustiveCaseStatement', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyExhaustiveCaseStatement).WithDescription(
    'Flags a case statement over an enumeration that does not handle every value.')));
  RegisterMessage(cKeyExhaustiveCaseStatement, SExhaustiveCaseStatement);

  RegisterRule(TRuleExceptionRaised.Create(TRuleMetadata.Make(
    'ExceptionRaised', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyExceptionRaised).WithDescription(
    'Flags an exception object that is constructed but never raised.')));
  RegisterMessage(cKeyExceptionRaised, SExceptionRaised);

  RegisterRule(TRuleSingleIterationLoop.Create(TRuleMetadata.Make(
    'SingleIterationLoop', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeySingleIterationLoop).WithDescription(
    'Flags a loop whose body always exits on the first iteration; use if instead.')));
  RegisterMessage(cKeySingleIterationLoop, SSingleIterationLoop);

  RegisterRule(TRuleNoPascalStyleResultAssignment.Create(TRuleMetadata.Make(
    'NoPascalStyleResultAssignment', rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, True, cKeyNoPascalStyleResultAssignment).WithDescription(
    'Flags a function returning by assigning to its own name; use Result instead.')));
  RegisterMessage(cKeyNoPascalStyleResultAssignment, SNoPascalStyleResultAssignment);

  RegisterRule(TRuleRedundantAssignedCheckBeforeFree.Create(TRuleMetadata.Make(
    'RedundantAssignedCheckBeforeFree', rtSem, rfResolver, sevMinor,
    itCodeSmell, cfHigh, True, cKeyRedundantAssignedCheckBeforeFree).WithDescription(
    'Flags a redundant Assigned/nil check before Free, which is already nil-safe.')));
  RegisterMessage(cKeyRedundantAssignedCheckBeforeFree,
    SRedundantAssignedCheckBeforeFree);

  // LoopBeyondCollectionEnd — Major/Bug (an out-of-bounds access)
  RegisterRule(TRuleLoopBeyondCollectionEnd.Create(TRuleMetadata.Make(
    'LoopBeyondCollectionEnd', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyLoopBeyondCollectionEnd).WithDescription(
    'Flags a loop that indexes a collection past its last valid element.')));
  RegisterMessage(cKeyLoopBeyondCollectionEnd, SLoopBeyondCollectionEnd);

  // RoutineResultAssigned — Major/Bug (an indeterminate return value)
  RegisterRule(TRuleRoutineResultAssigned.Create(TRuleMetadata.Make(
    'RoutineResultAssigned', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyRoutineResultAssigned).WithDescription(
    'Flags a function that may return without assigning a result on some path.')));
  RegisterMessage(cKeyRoutineResultAssigned, SRoutineResultAssigned);

  // NoCatchRawException / NoRaiseRawException
  RegisterRule(TRuleNoCatchRawException.Create(TRuleMetadata.Make(
    'NoCatchRawException', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyNoCatchRawException).WithDescription(
    'Flags catching the root Exception class, which masks unrelated failures; catch a specific subclass.')));
  RegisterMessage(cKeyNoCatchRawException, SNoCatchRawException);

  RegisterRule(TRuleNoRaiseRawException.Create(TRuleMetadata.Make(
    'NoRaiseRawException', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyNoRaiseRawException).WithDescription(
    'Flags raising the root Exception class; raise a specific subclass instead.')));
  RegisterMessage(cKeyNoRaiseRawException, SNoRaiseRawException);

  // The AST-tier duplicate/identical shape cluster — all five Major/Bug, disabled
  RegisterRule(TRuleIdenticalBranches.Create(TRuleMetadata.Make(
    'IdenticalBranches', rtAst, rfAst, sevMajor, itBug, cfHigh,
    False, cKeyIdenticalBranches).WithDescription(
    'Flags an if statement whose then and else branches are structurally identical.')));
  RegisterMessage(cKeyIdenticalBranches, SIdenticalBranches);

  RegisterRule(TRuleDuplicateConditionInChain.Create(TRuleMetadata.Make(
    'DuplicateConditionInChain', rtAst, rfAst, sevMajor, itBug, cfMedium,
    False, cKeyDuplicateConditionInChain).WithDescription(
    'Flags a condition that appears more than once in one if/else if chain.')));
  RegisterMessage(cKeyDuplicateConditionInChain, SDuplicateConditionInChain);

  RegisterRule(TRuleDuplicateCaseLabel.Create(TRuleMetadata.Make(
    'DuplicateCaseLabel', rtAst, rfAst, sevMajor, itBug, cfHigh,
    False, cKeyDuplicateCaseLabel).WithDescription(
    'Flags a case label repeated within one case statement.')));
  RegisterMessage(cKeyDuplicateCaseLabel, SDuplicateCaseLabel);

  RegisterRule(TRuleSelfComparison.Create(TRuleMetadata.Make(
    'SelfComparison', rtAst, rfAst, sevMajor, itBug, cfMedium,
    False, cKeySelfComparison).WithDescription(
    'Flags a comparison whose two operands are the same reference.')));
  RegisterMessage(cKeySelfComparison, SSelfComparison);

  RegisterRule(TRuleEmptyThenWithFollowingStatement.Create(TRuleMetadata.Make(
    'EmptyThenWithFollowingStatement', rtAst, rfAst, sevMajor, itBug, cfMedium,
    False, cKeyEmptyThenWithFollowingStatement).WithDescription(
    'Flags an if statement with an empty then branch followed by another statement.')));
  RegisterMessage(cKeyEmptyThenWithFollowingStatement,
    SEmptyThenWithFollowingStatement);

  // The operator-trap cluster — three tiers, all disabled
  RegisterRule(TRuleMixedBooleanAndRelational.Create(TRuleMetadata.Make(
    'MixedBooleanAndRelational', rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfMedium, False, cKeyMixedBooleanAndRelational).WithDescription(
    'Flags a boolean operator mixed with a comparison operator and no parentheses to group them.')));
  RegisterMessage(cKeyMixedBooleanAndRelational, SMixedBooleanAndRelational);

  RegisterRule(TRuleBitwiseOnBooleanOperands.Create(TRuleMetadata.Make(
    'BitwiseOnBooleanOperands', rtAst, rfAst, sevMajor, itBug, cfMedium,
    False, cKeyBitwiseOnBooleanOperands).WithDescription(
    'Flags an and, or or xor mixing a boolean with an integer operand, or bit-testing an integer as a condition.')));
  RegisterMessage(cKeyBitwiseOnBooleanOperands, SBitwiseOnBooleanOperands);

  RegisterRule(TRuleAssignmentInsteadOfComparison.Create(TRuleMetadata.Make(
    'AssignmentInsteadOfComparison', rtAst, rfAst, sevMajor, itBug, cfMedium,
    False, cKeyAssignmentInsteadOfComparison).WithDescription(
    'Flags a := inside a call argument list, where the grammar implies a comparison.')));
  RegisterMessage(cKeyAssignmentInsteadOfComparison,
    SAssignmentInsteadOfComparison);

  RegisterRule(TRuleConditionWithSideEffect.Create(TRuleMetadata.Make(
    'ConditionWithSideEffect', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyConditionWithSideEffect).WithDescription(
    'Flags a call with a var or out argument in a short-circuited operand of a condition.')));
  RegisterMessage(cKeyConditionWithSideEffect, SConditionWithSideEffect);

  // The if-shape cluster — all three AST tier, all disabled
  RegisterRule(TRuleRedundantElseAfterExit.Create(TRuleMetadata.Make(
    'RedundantElseAfterExit', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    False, cKeyRedundantElseAfterExit).WithDescription(
    'Flags an else branch that follows a then branch ending in exit, break, continue, halt or raise.')));
  RegisterMessage(cKeyRedundantElseAfterExit, SRedundantElseAfterExit);

  RegisterRule(TRuleCollapsibleNestedIf.Create(TRuleMetadata.Make(
    'CollapsibleNestedIf', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    False, cKeyCollapsibleNestedIf).WithDescription(
    'Flags an if nested as the sole content of another if when neither carries an else.')));
  RegisterMessage(cKeyCollapsibleNestedIf, SCollapsibleNestedIf);

  RegisterRule(TRuleNegatedConditionWithElse.Create(TRuleMetadata.Make(
    'NegatedConditionWithElse', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    False, cKeyNegatedConditionWithElse).WithDescription(
    'Flags an if whose condition is negated with not while the statement has an else branch.')));
  RegisterMessage(cKeyNegatedConditionWithElse, SNegatedConditionWithElse);

  // The selector-and-loop cluster — both resolver tier, both disabled
  RegisterRule(TRuleSwitchOnBooleanExpression.Create(TRuleMetadata.Make(
    'SwitchOnBooleanExpression', rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, False, cKeySwitchOnBooleanExpression).WithDescription(
    'Flags a case statement whose selector is a Boolean expression, which an if states in two branches.')));
  RegisterMessage(cKeySwitchOnBooleanExpression, SSwitchOnBooleanExpression);

  RegisterRule(TRuleLoopConditionNeverChanges.Create(TRuleMetadata.Make(
    'LoopConditionNeverChanges', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyLoopConditionNeverChanges).WithDescription(
    'Flags a while or repeat loop whose body writes no variable of its condition.')));
  RegisterMessage(cKeyLoopConditionNeverChanges, SLoopConditionNeverChanges);

  // UnreachableCode — AST tier, Major/Bug, disabled
  RegisterRule(TRuleUnreachableCode.Create(TRuleMetadata.Make(
    'UnreachableCode', rtAst, rfAst, sevMajor, itBug, cfMedium,
    False, cKeyUnreachableCode).WithDescription(
    'Flags a statement no control path through its routine body reaches.')));
  RegisterMessage(cKeyUnreachableCode, SUnreachableCode);

end.
