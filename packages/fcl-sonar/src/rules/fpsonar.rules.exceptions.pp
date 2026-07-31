{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Exception-structure analysis rules, mixed tier

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Exceptions;


{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, Pascal.Tree,
{$ELSE}
  SysUtils, PasTree,
{$ENDIF}
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Traversal, FpSonar.Rules.Consts;

type
  { Flags a finally block with no statements. }
  TRuleNoEmptyFinally = class(TRuleBase)
  public
    // Emits one issue per empty finally block.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an except handler with no statements (a swallowed exception). }
  TRuleExceptionsNotSwallowed = class(TRuleBase)
  public
    // Emits one issue per empty except handler.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags `raise E;` re-raising the caught variable instead of bare raise. }
  TRuleNoExplicitReRaise = class(TRuleBase)
  public
    // Emits one issue per explicit re-raise of an on-handler's catch variable.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an `exit` inside a finally block (positive: the exit is the defect). }
  TRuleExitInsideFinally = class(TRuleBase)
  public
    // Emits one issue per exit statement in a finally block's subtree.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a `raise` inside a finally block (positive: the raise is the defect). }
  TRuleRaiseInsideFinally = class(TRuleBase)
  public
    // Emits one issue per raise statement in a finally block's subtree.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an `on` handler an earlier sibling handler already catches
    (positive: the shadowed handler is the defect). }
  TRuleHandlerOrderShadowsDerived = class(TRuleBase)
  public
    // Emits one issue per unreachable on-handler.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a resource acquired inside the try block whose finally releases it
    (positive: the acquisition is the defect). }
  TRuleTryFinallyAcquireOutsideTry = class(TRuleBase)
  public
    // Emits one issue per acquisition assignment inside the protecting try body.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a raise of a class that does not descend from Exception
    (positive: the raised class is the defect). }
  TRuleExceptionClassNotDerivedFromException = class(TRuleBase)
  public
    // Emits one issue per raise of a class proven not to descend from Exception.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a try with an empty body and a non-empty handler
    (positive: the empty try body is the defect). }
  TRuleEmptyTryBody = class(TRuleBase)
  public
    // Emits one issue per try statement whose body holds no statements.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a raise in a destructor that no except handler stops
    (positive: the raise is the defect). }
  TRuleRaiseInDestructor = class(TRuleBase)
  public
    // Emits one issue per unguarded raise in a destructor body.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an Assert whose asserted expression calls a routine
    (positive: the call in the assertion is the defect). }
  TRuleAssertUsedForControlFlow = class(TRuleBase)
  public
    // Emits one issue per Assert whose first argument calls a user routine.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

uses
  FpSonar.Resolver;

const
  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyNoEmptyFinally = 'rule.NoEmptyFinally.message';
  cKeyExceptionsNotSwallowed = 'rule.ExceptionsNotSwallowed.message';
  cKeyNoExplicitReRaise = 'rule.NoExplicitReRaise.message';
  cKeyExitInsideFinally = 'rule.ExitInsideFinally.message';
  cKeyRaiseInsideFinally = 'rule.RaiseInsideFinally.message';
  cKeyHandlerOrderShadowsDerived = 'rule.HandlerOrderShadowsDerived.message';
  cKeyTryFinallyAcquireOutsideTry = 'rule.TryFinallyAcquireOutsideTry.message';
  cKeyExceptionClassNotDerivedFromException =
    'rule.ExceptionClassNotDerivedFromException.message';
  cKeyEmptyTryBody = 'rule.EmptyTryBody.message';
  cKeyRaiseInDestructor = 'rule.RaiseInDestructor.message';
  cKeyAssertUsedForControlFlow = 'rule.AssertUsedForControlFlow.message';


  { ---- shared statement collection + emission (mirrors Structure.pas) ---- }

// Appends every statement strictly BELOW aRoot (not aRoot) to aList
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


// Emits one issue at aLine, column 1, returning the caller-supplied message args and snippet.
procedure EmitStmt(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aLine: integer;
  const aArgs: array of string; const aSnippet: string);
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aLine, 1, aLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    aArgs, aSnippet);
end;


// True when aBlock holds no statements.
function IsEmptyBlock(aBlock: TPasImplBlock): boolean;
begin
  Result := (aBlock.Elements = nil) or (aBlock.Elements.Count = 0);
end;


{ TRuleNoEmptyFinally }

procedure TRuleNoEmptyFinally.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  i: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    // The three handlers are siblings: 'is TPasImplTryFinally' never
    // matches an except or an except-else handler.
    if lStmts[i] is TPasImplTryFinally then
      if IsEmptyBlock(TPasImplTryFinally(lStmts[i])) then
        EmitStmt(FMetadata, aContext, aCollector, lStmts[i].SourceLinenumber,
          [], 'finally');
end;


{ TRuleExceptionsNotSwallowed }

procedure TRuleExceptionsNotSwallowed.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  i: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    // is TPasImplTryExcept is exclusive of the except-else handler.
    if lStmts[i] is TPasImplTryExcept then
      if IsEmptyBlock(TPasImplTryExcept(lStmts[i])) then
        EmitStmt(FMetadata, aContext, aCollector, lStmts[i].SourceLinenumber,
          [], 'except');
end;


{ NoExplicitReRaise helpers }

// True when aExpr is a bare identifier equal (case-insensitively) to aVarName —
// i.e. the raised expression is exactly the caught variable.
function ExprIsVar(aExpr: TPasExpr; const aVarName: string): boolean;
begin
  Result := (aExpr <> nil) and (aExpr is TPrimitiveExpr)
    and (TPrimitiveExpr(aExpr).Kind = pekIdent)
    and SameText(TPrimitiveExpr(aExpr).Value, aVarName);
end;


// Returns the first 'raise aVarName;' found in the statement subtree rooted at
// aNode (its own body and descendants), or nil.
function RaisesVar(aNode: TPasImplElement;
  const aVarName: string): TPasImplRaise;
var
  lChildren: TPasImplElementArray;
  i: integer;
begin
  Result := nil;
  if aNode = nil then
    Exit;
  if aNode is TPasImplExceptOn then
    Exit;
  if (aNode is TPasImplRaise)
    and ExprIsVar(TPasImplRaise(aNode).ExceptObject, aVarName) then
    Exit(TPasImplRaise(aNode));
  lChildren := ChildStatements(aNode);
  for i := 0 to High(lChildren) do
  begin
    Result := RaisesVar(lChildren[i], aVarName);
    if Result <> nil then
      Exit;
  end;
end;


{ TRuleNoExplicitReRaise }

procedure TRuleNoExplicitReRaise.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  i: integer;
  lOn: TPasImplExceptOn;
  lRaise: TPasImplRaise;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplExceptOn then
    begin
      lOn := TPasImplExceptOn(lStmts[i]);
      // 'on SomeType do' (no catch variable) has nothing to compare — skip.
      if lOn.VarEl <> nil then
      begin
        lRaise := RaisesVar(lOn.Body, lOn.VarEl.Name);
        if lRaise <> nil then
          EmitStmt(FMetadata, aContext, aCollector,
            lRaise.SourceLinenumber, [lOn.VarEl.Name], lOn.VarEl.Name);
      end;
    end;
end;


{ finally/handler-hazard helpers }

// True when aStmt is the built-in exit, in the bare and in the Exit(x) form.
function IsExitStatement(const aContext: TRuleContext;
  aStmt: TPasImplElement): boolean;
var
  lKind: TFpSonarLoopExitKind;
begin
  Result := aContext.Resolver.TryLoopControlFlow(aStmt, lKind)
    and (lKind = lekExit);
end;


// The nearest TPasImplTryFinally enclosing aStmt, or nil when there is none.
function EnclosingFinally(aStmt: TPasImplElement): TPasImplElement;
var
  lParent: TPasElement;
begin
  Result := nil;
  if aStmt = nil then
    Exit;
  lParent := aStmt.Parent;
  while lParent is TPasImplElement do
  begin
    if lParent is TPasImplTryFinally then
      Exit(TPasImplElement(lParent));
    lParent := lParent.Parent;
  end;
end;


// The resolved declarations a Free/FreeAndNil call releases anywhere in the
// statement subtree of aFinally.
function ReleasedDecls(const aContext: TRuleContext;
  aFinally: TPasImplElement): TPasElementArray;
var
  lStmts: TPasImplElementArray;
  lInner: TPasExpr;
  lDecl: TPasElement;
  i: integer;
begin
  SetLength(Result, 0);
  SetLength(lStmts, 0);
  CollectStatements(aFinally, lStmts);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplSimple then
      if aContext.Resolver.TryFreeCall(TPasImplSimple(lStmts[i]).Expr,
        lInner) <> lfkNone then
      begin
        lDecl := aContext.Resolver.ReferencedDecl(lInner);
        if lDecl <> nil then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := lDecl;
        end;
      end;
end;


{ TRuleExitInsideFinally }

procedure TRuleExitInsideFinally.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if (EnclosingFinally(lStmts[i]) <> nil)
      and IsExitStatement(aContext, lStmts[i]) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lStmts[i]), [], 'exit');
end;


{ TRuleRaiseInsideFinally }

procedure TRuleRaiseInsideFinally.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  i: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if (lStmts[i] is TPasImplRaise)
      and (EnclosingFinally(lStmts[i]) <> nil) then
      EmitStmt(FMetadata, aContext, aCollector,
        lStmts[i].SourceLinenumber, [], 'raise');
end;


{ TRuleHandlerOrderShadowsDerived }

procedure TRuleHandlerOrderShadowsDerived.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lHandler, lAncestor: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplExceptOn then
      if aContext.Resolver.TryHandlerShadowedByEarlier(lStmts[i], lHandler,
        lAncestor) then
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(lStmts[i]), [lHandler, lAncestor],
          lHandler);
end;


{ TRuleTryFinallyAcquireOutsideTry }

procedure TRuleTryFinallyAcquireOutsideTry.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lReleased: TPasElementArray;
  lTry: TPasImplTry;
  lAssign: TPasImplAssign;
  lDecl: TPasElement;
  i, j, k: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
  begin
    if not (lStmts[i] is TPasImplTry) then
      Continue;
    lTry := TPasImplTry(lStmts[i]);
    if not (lTry.FinallyExcept is TPasImplTryFinally) then
      Continue;
    if lTry.Elements = nil then
      Continue;
    lReleased := ReleasedDecls(aContext, lTry.FinallyExcept);
    if Length(lReleased) = 0 then
      Continue;
    // Scanned: the try's own top-level statements. Not scanned: an assignment
    // nested inside one of them.
    for j := 0 to lTry.Elements.Count - 1 do
      if TObject(lTry.Elements[j]) is TPasImplAssign then
      begin
        lAssign := TPasImplAssign(lTry.Elements[j]);
        if lAssign.Kind <> akDefault then
          Continue;
        lDecl := aContext.Resolver.ReferencedDecl(lAssign.Left);
        if lDecl = nil then
          Continue;
        for k := 0 to High(lReleased) do
          if lReleased[k] = lDecl then
          begin
            EmitStmt(FMetadata, aContext, aCollector,
              aContext.Resolver.SourceRow(lAssign), [lDecl.Name], lDecl.Name);
            Break;
          end;
      end;
  end;
end;


{ TRuleExceptionClassNotDerivedFromException }

procedure TRuleExceptionClassNotDerivedFromException.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lClassName: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if aContext.Resolver.TryRaisedClassNotException(lStmts[i], lClassName) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lStmts[i]), [lClassName], lClassName);
end;


{ TRuleEmptyTryBody }

procedure TRuleEmptyTryBody.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lTry: TPasImplTry;
  i: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplTry then
    begin
      lTry := TPasImplTry(lStmts[i]);
      if IsEmptyBlock(lTry) and (lTry.FinallyExcept <> nil)
        and not IsEmptyBlock(lTry.FinallyExcept) then
        EmitStmt(FMetadata, aContext, aCollector, lTry.SourceLinenumber,
          [], 'try');
    end;
end;


{ RaiseInDestructor helper }

// True when aStmt sits in the protected body of a try..except between it and
// aBlock, rather than in that try's handler. The on clauses are not read.
function ProtectedByHandler(aStmt: TPasImplElement;
  aBlock: TPasImplBlock): boolean;
var
  lNode, lParent: TPasElement;
  lTry: TPasImplTry;
begin
  Result := False;
  lNode := aStmt;
  while (lNode <> nil) and (lNode <> aBlock) do
  begin
    lParent := lNode.Parent;
    if lParent is TPasImplTry then
    begin
      lTry := TPasImplTry(lParent);
      if (lTry.FinallyExcept is TPasImplTryExcept)
        and (lNode <> lTry.FinallyExcept) and (lNode <> lTry.ElseBranch) then
        Exit(True);
    end;
    lNode := lParent;
  end;
end;


{ TRuleRaiseInDestructor }

procedure TRuleRaiseInDestructor.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  lStmts: TPasImplElementArray;
  i, j: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
  begin
    // A class destructor is a finalizer: it destroys no instance.
    if not (lRoutines[i].Decl is TPasDestructor)
      or (lRoutines[i].Decl is TPasClassDestructor) then
      Continue;
    SetLength(lStmts, 0);
    CollectStatements(lRoutines[i].Block, lStmts);
    for j := 0 to High(lStmts) do
      if (lStmts[j] is TPasImplRaise)
        and not ProtectedByHandler(lStmts[j], lRoutines[i].Block) then
        EmitStmt(FMetadata, aContext, aCollector, lStmts[j].SourceLinenumber,
          [lRoutines[i].Decl.Name], 'raise');
  end;
end;


{ TRuleAssertUsedForControlFlow }

procedure TRuleAssertUsedForControlFlow.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lRoutineName: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if lStmts[i] is TPasImplSimple then
      if aContext.Resolver.TryAssertControlFlowOperand(
        TPasImplSimple(lStmts[i]).Expr, lRoutineName) then
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(lStmts[i]), [lRoutineName], lRoutineName);
end;


initialization
  RegisterRule(TRuleNoEmptyFinally.Create(TRuleMetadata.Make(
    'NoEmptyFinally', rtAst, rfAst, sevMajor, itCodeSmell, cfHigh,
    True, cKeyNoEmptyFinally).WithDescription(
    'Flags an empty finally block.')));
  RegisterMessage(cKeyNoEmptyFinally, SNoEmptyFinally);

  RegisterRule(TRuleExceptionsNotSwallowed.Create(TRuleMetadata.Make(
    'ExceptionsNotSwallowed', rtAst, rfAst, sevMajor, itBug, cfHigh,
    True, cKeyExceptionsNotSwallowed).WithDescription(
    'Flags an exception swallowed by an empty except handler.')));
  RegisterMessage(cKeyExceptionsNotSwallowed, SExceptionsNotSwallowed);

  RegisterRule(TRuleNoExplicitReRaise.Create(TRuleMetadata.Make(
    'NoExplicitReRaise', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, cKeyNoExplicitReRaise).WithDescription(
    'Flags re-raising a caught exception by name; use a bare raise instead.')));
  RegisterMessage(cKeyNoExplicitReRaise, SNoExplicitReRaise);

  RegisterRule(TRuleExitInsideFinally.Create(TRuleMetadata.Make(
    'ExitInsideFinally', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    False, cKeyExitInsideFinally).WithDescription(
    'Flags an exit in a finally block, which discards the in-flight exception.')));
  RegisterMessage(cKeyExitInsideFinally, SExitInsideFinally);

  RegisterRule(TRuleRaiseInsideFinally.Create(TRuleMetadata.Make(
    'RaiseInsideFinally', rtAst, rfAst, sevMajor, itBug, cfHigh,
    False, cKeyRaiseInsideFinally).WithDescription(
    'Flags a raise in a finally block, which replaces the in-flight exception.')));
  RegisterMessage(cKeyRaiseInsideFinally, SRaiseInsideFinally);

  RegisterRule(TRuleHandlerOrderShadowsDerived.Create(TRuleMetadata.Make(
    'HandlerOrderShadowsDerived', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    False, cKeyHandlerOrderShadowsDerived).WithDescription(
    'Flags an on-handler an earlier handler for an ancestor class already catches.')));
  RegisterMessage(cKeyHandlerOrderShadowsDerived, SHandlerOrderShadowsDerived);

  RegisterRule(TRuleTryFinallyAcquireOutsideTry.Create(TRuleMetadata.Make(
    'TryFinallyAcquireOutsideTry', rtSem, rfResolver, sevMinor, itCodeSmell, cfLow,
    False, cKeyTryFinallyAcquireOutsideTry).WithDescription(
    'Flags a resource acquired inside the try block whose finally releases it.')));
  RegisterMessage(cKeyTryFinallyAcquireOutsideTry, STryFinallyAcquireOutsideTry);

  RegisterRule(TRuleExceptionClassNotDerivedFromException.Create(
    TRuleMetadata.Make('ExceptionClassNotDerivedFromException', rtSem,
    rfResolver, sevMajor, itBug, cfHigh, False,
    cKeyExceptionClassNotDerivedFromException).WithDescription(
    'Flags a raise of a class that does not descend from Exception.')));
  RegisterMessage(cKeyExceptionClassNotDerivedFromException,
    SExceptionClassNotDerivedFromException);

  RegisterRule(TRuleEmptyTryBody.Create(TRuleMetadata.Make(
    'EmptyTryBody', rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    False, cKeyEmptyTryBody).WithDescription(
    'Flags a try block with an empty body and a non-empty handler.')));
  RegisterMessage(cKeyEmptyTryBody, SEmptyTryBody);

  RegisterRule(TRuleRaiseInDestructor.Create(TRuleMetadata.Make(
    'RaiseInDestructor', rtAst, rfAst, sevMajor, itBug, cfHigh,
    False, cKeyRaiseInDestructor).WithDescription(
    'Flags a raise in a destructor that no except handler stops.')));
  RegisterMessage(cKeyRaiseInDestructor, SRaiseInDestructor);

  RegisterRule(TRuleAssertUsedForControlFlow.Create(TRuleMetadata.Make(
    'AssertUsedForControlFlow', rtSem, rfResolver, sevMajor, itBug, cfLow,
    False, cKeyAssertUsedForControlFlow).WithDescription(
    'Flags an Assert whose asserted expression calls a routine.')));
  RegisterMessage(cKeyAssertUsedForControlFlow, SAssertUsedForControlFlow);

end.
