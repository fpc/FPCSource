{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Exception-structure AST analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Exceptions;

{ Exception-structure AST rules (rtAst / rfAst):
  empty finally, swallowed exceptions, explicit re-raise that loses the original stack. }

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


implementation

const
  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyNoEmptyFinally = 'rule.NoEmptyFinally.message';
  cKeyExceptionsNotSwallowed = 'rule.ExceptionsNotSwallowed.message';
  cKeyNoExplicitReRaise = 'rule.NoExplicitReRaise.message';


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

end.
