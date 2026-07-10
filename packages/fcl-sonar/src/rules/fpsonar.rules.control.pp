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

{ Control-flow SEM rules (rtSem / rfResolver):
  statement-level checks — exhaustive case, raised exceptions,
  single-iteration / past-the-end loops, Result assignment,
  assigned-before-free, raw exception raise/catch. }

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

  { Flags a function with a return path that never assigns its result. }
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


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
{$ELSE}
  SysUtils;
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
  lRoutines: TAstRoutineArray;
  i: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lRoutines) do
    if lRoutines[i].Decl is TPasFunction then
      if aContext.Resolver.TryRoutineResultNotAssigned(lRoutines[i].Decl) then
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(lRoutines[i].Decl), [], 'function');
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

end.
