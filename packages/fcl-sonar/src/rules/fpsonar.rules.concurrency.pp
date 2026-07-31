{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Concurrency analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Concurrency;


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
  { Flags a write to a unit-level variable in a thread routine that no critical
    section covers. }
  TRuleGlobalWrittenFromThreadRoutine = class(TRuleBase)
  public
    // Emits one issue per unguarded global write, with the global's name.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a main-thread callback dispatch made while a critical section is
    held on some path. }
  TRuleSynchronizeWithLockHeld = class(TRuleBase)
  public
    // Emits one issue per dispatch, with the callee and the held section.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a critical-section call on a routine local that no
    InitCriticalSection precedes on some path. }
  TRuleCriticalSectionNotInitialized = class(TRuleBase)
  public
    // Emits one issue per such call, with the section's name.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a UI-control member touched directly in the Execute body of a
    TThread descendant. Polarity: positive. }
  TRuleVclAccessOffMainThread = class(TRuleBase)
  public
    // Emits one issue per access, with the member and the thread routine.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a threadvar that is read but that no routine body ever assigns.
    Polarity: absence. }
  TRuleThreadvarInitialization = class(TRuleBase)
  public
    // Emits one issue per such threadvar, at its declaration row.
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
  FpSonar.Resolver, FpSonar.DataFlow, FpSonar.Traversal, FpSonar.Rules.Consts;

const
  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyGlobalWrittenFromThreadRoutine =
    'rule.GlobalWrittenFromThreadRoutine.message';
  cKeySynchronizeWithLockHeld = 'rule.SynchronizeWithLockHeld.message';
  cKeyCriticalSectionNotInitialized =
    'rule.CriticalSectionNotInitialized.message';
  cKeyVclAccessOffMainThread = 'rule.VclAccessOffMainThread.message';
  cKeyThreadvarInitialization = 'rule.ThreadvarInitialization.message';

  // The thread routine VclAccessOffMainThread recognises, by written name.
  cThreadClassName = 'TThread';
  cThreadRoutineName = 'Execute';
  cMaxAncestorDepth = 200;

  // The members VclAccessOffMainThread reads as a UI control's, by written name.
  cUiMemberNames: array[0..9] of string = ('Canvas', 'Caption', 'Checked',
    'Color', 'Cursor', 'Enabled', 'Font', 'Hint', 'Lines', 'Visible');

  // The modifiers that make a threadvar assignable from outside the unit.
  cForeignLinkage = [vmCVar, vmExternal, vmPublic, vmExport];

// Emits one issue per concurrency finding of aVerdict, at its site's row.
procedure EmitConcurrencyVerdict(const aMeta: TRuleMetadata;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector;
  aVerdict: TFpSonarConcurrencyVerdict);

var
  lFlow: TFpSonarDataFlow;
  lFindings: TFpSonarConcurrencyFindingArray;
  lOk: boolean;
  lRow, i: integer;

begin
  lFlow := TFpSonarDataFlow.Create(aContext.Resolver);
  try
    lOk := lFlow.TryConcurrencyFindings(lFindings);
  finally
    lFlow.Free;
  end;
  if not lOk then
    Exit;
  for i := 0 to High(lFindings) do
  begin
    if lFindings[i].Verdict <> aVerdict then
      Continue;
    lRow := aContext.Resolver.SourceRow(lFindings[i].Site);
    if aVerdict = cvSyncWithLock then
      aCollector.AddIssue(aMeta.RuleId, aContext.FileName, lRow, 1, lRow, 1,
        aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence,
        aMeta.MessageKey, [lFindings[i].Callee, lFindings[i].Name],
        lFindings[i].Callee)
    else
      aCollector.AddIssue(aMeta.RuleId, aContext.FileName, lRow, 1, lRow, 1,
        aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence,
        aMeta.MessageKey, [lFindings[i].Name], lFindings[i].Name);
  end;
end;


// Emits one issue at aLine, column 1, with aArgs and aSnippet.
procedure EmitAt(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aLine: integer;
  const aArgs: array of string; const aSnippet: string);

begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aLine, 1, aLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    aArgs, aSnippet);
end;


// Appends every statement strictly below aRoot to aList.
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


{ Appends the expressions aStmt itself carries, and everything below them,
  to aList; the expressions of its child statements are not visited. }
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


// The part of aName after its last dot, aName itself when it carries none.
function LastNamePart(const aName: string): string;

begin
  Result := Copy(aName, LastDelimiter('.', aName) + 1, Length(aName));
end;


// The part of aName before its last dot, '' when it carries none.
function QualifierPart(const aName: string): string;

begin
  Result := Copy(aName, 1, LastDelimiter('.', aName) - 1);
end;


{ The single top-level class or object type of aTypes written with the name
  aName; nil when none or more than one carries the name. }
function FindClassNamed(const aTypes: TPasTypeArray;
  const aName: string): TPasClassType;

var
  i: integer;

begin
  Result := nil;
  if aName = '' then
    Exit;
  for i := 0 to High(aTypes) do
    if (aTypes[i] is TPasClassType)
      and (TPasClassType(aTypes[i]).ObjKind in [okClass, okObject])
      and not (aTypes[i].Parent is TPasMembersType)
      and SameText(aTypes[i].Name, aName) then
    begin
      if Result <> nil then
        Exit(nil);
      Result := TPasClassType(aTypes[i]);
    end;
end;


{ The class aDecl is a method of: its parent when it is declared in the class,
  otherwise the type of aTypes its dotted name qualifies. }
function OwnerClass(const aTypes: TPasTypeArray;
  aDecl: TPasProcedure): TPasClassType;

begin
  if aDecl.Parent is TPasClassType then
    Result := TPasClassType(aDecl.Parent)
  else
    Result := FindClassNamed(aTypes, QualifierPart(aDecl.Name));
end;


{ True when aDecl is the Execute body of a class whose ancestor chain, by
  written name within the module, reaches TThread. }
function IsThreadRoutine(const aTypes: TPasTypeArray;
  aDecl: TPasProcedure): boolean;

var
  lClass: TPasClassType;
  lAncestor: TPasType;
  i: integer;

begin
  Result := False;
  if not SameText(LastNamePart(aDecl.Name), cThreadRoutineName) then
    Exit;
  lClass := OwnerClass(aTypes, aDecl);
  i := 0;
  while (lClass <> nil) and (i < cMaxAncestorDepth) do
  begin
    if SameText(lClass.Name, cThreadClassName) then
      Exit(True);
    lAncestor := lClass.AncestorType;
    if lAncestor = nil then
      Exit;
    if SameText(lAncestor.Name, cThreadClassName) then
      Exit(True);
    if lAncestor is TPasClassType then
      lClass := TPasClassType(lAncestor)
    else
      lClass := FindClassNamed(aTypes, lAncestor.Name);
    Inc(i);
  end;
end;


{ True when aNode is the qualifier of a member access: the left operand of a
  dotted selection. }
function IsMemberQualifier(aNode: TPasElement): boolean;

begin
  Result := (aNode <> nil) and (aNode.Parent is TBinaryExpr)
    and (TBinaryExpr(aNode.Parent).OpCode = eopSubIdent)
    and (TBinaryExpr(aNode.Parent).Left = aNode);
end;


// The curated UI name aExpr writes, '' when it writes none.
function CuratedName(aExpr: TPasExpr): string;

var
  i: integer;

begin
  Result := '';
  if not ((aExpr is TPrimitiveExpr)
    and (TPrimitiveExpr(aExpr).Kind = pekIdent)) then
    Exit;
  for i := Low(cUiMemberNames) to High(cUiMemberNames) do
    if SameText(TPrimitiveExpr(aExpr).Value, cUiMemberNames[i]) then
      Exit(TPrimitiveExpr(aExpr).Value);
end;


{ The curated UI member aExpr selects, '' when aExpr is not a member access,
  when the member is outside the list, or when the selection qualified by aExpr
  names a curated member of its own and so stands for the whole chain. }
function UiMemberOf(aExpr: TPasExpr): string;

begin
  Result := '';
  if not ((aExpr is TBinaryExpr) and (aExpr.OpCode = eopSubIdent)) then
    Exit;
  if IsMemberQualifier(aExpr)
    and (CuratedName(TBinaryExpr(aExpr.Parent).Right) <> '') then
    Exit;
  Result := CuratedName(TBinaryExpr(aExpr).Right);
end;


// True when aNode is the operand of an address-of operator.
function IsAddressOperand(aNode: TPasElement): boolean;

begin
  Result := (aNode <> nil) and (aNode.Parent is TUnaryExpr)
    and (TUnaryExpr(aNode.Parent).OpCode = eopAddress);
end;


{ True when aNode is the base of a member or an element access, so a write
  reaching it lands on the member rather than on aNode's own declaration. }
function IsAccessBase(aNode: TPasElement): boolean;

begin
  Result := IsMemberQualifier(aNode)
    or ((aNode <> nil) and (aNode.Parent is TParamsExpr)
      and (TParamsExpr(aNode.Parent).Value = aNode));
end;


{ TRuleGlobalWrittenFromThreadRoutine }

procedure TRuleGlobalWrittenFromThreadRoutine.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);

begin
  EmitConcurrencyVerdict(FMetadata, aContext, aCollector, cvGlobalWrite);
end;


{ TRuleSynchronizeWithLockHeld }

procedure TRuleSynchronizeWithLockHeld.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  EmitConcurrencyVerdict(FMetadata, aContext, aCollector, cvSyncWithLock);
end;


{ TRuleCriticalSectionNotInitialized }

procedure TRuleCriticalSectionNotInitialized.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);

begin
  EmitConcurrencyVerdict(FMetadata, aContext, aCollector,
    cvSectionNotInitialized);
end;


{ TRuleVclAccessOffMainThread }

procedure TRuleVclAccessOffMainThread.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

var
  lTypes: TPasTypeArray;
  lRoutines: TAstRoutineArray;
  lStmts: TPasImplElementArray;
  lExprs: TPasExprArray;
  lMember: string;
  i, j, k: integer;

begin
  lTypes := EnumerateTypes(aContext.Module);
  lRoutines := EnumerateRoutines(aContext.Module);
  for i := 0 to High(lRoutines) do
  begin
    if not IsThreadRoutine(lTypes, lRoutines[i].Decl) then
      Continue;
    SetLength(lStmts, 0);
    CollectStatements(lRoutines[i].Block, lStmts);
    SetLength(lExprs, 0);
    for j := 0 to High(lStmts) do
      StmtExpressions(lStmts[j], lExprs);
    for k := 0 to High(lExprs) do
    begin
      lMember := UiMemberOf(lExprs[k]);
      if lMember <> '' then
        EmitAt(FMetadata, aContext, aCollector, lExprs[k].SourceLinenumber,
          [lMember, lRoutines[i].Decl.Name], lMember);
    end;
  end;
end;


{ TRuleThreadvarInitialization }

procedure TRuleThreadvarInitialization.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

var
  lModule: TPasModule;
  lCands: TPasElementArray;
  lRead, lCleared: array of boolean;
  lAnswered: boolean;
  lDecls: TPasValueDeclArray;
  lRoutines: TAstRoutineArray;
  i: integer;

  // The index of aDecl among the candidates, -1 when it is not one.
  function IndexOf(aDecl: TPasElement): integer;
  var
    n: integer;
  begin
    Result := -1;
    for n := 0 to High(lCands) do
      if lCands[n] = aDecl then
        Exit(n);
  end;

  { Files every access of aStmts against the candidates when aCounts; otherwise
    aStmts is scanned for answerability alone. One unanswerable statement
    clears lAnswered. }
  procedure ClassifyStatements(const aStmts: TPasImplElementArray;
    aCounts: boolean);
  var
    lReadNodes, lReadDecls, lWriteDecls: TPasElementArray;
    lQualifies: boolean;
    lAt, n, m: integer;
  begin
    for n := 0 to High(aStmts) do
    begin
      if not aContext.Resolver.TryStatementAccess(aStmts[n], lReadNodes,
        lReadDecls, lWriteDecls) then
      begin
        lAnswered := False;
        Exit;
      end;
      if not aCounts then
        Continue;
      // A with header reads its qualifier, and its body writes the members.
      lQualifies := aStmts[n] is TPasImplWithDo;
      for m := 0 to High(lReadDecls) do
      begin
        lAt := IndexOf(lReadDecls[m]);
        if lAt < 0 then
          Continue;
        lRead[lAt] := True;
        if lQualifies or IsAddressOperand(lReadNodes[m])
          or IsAccessBase(lReadNodes[m]) then
          lCleared[lAt] := True;
      end;
      for m := 0 to High(lWriteDecls) do
      begin
        lAt := IndexOf(lWriteDecls[m]);
        if lAt >= 0 then
          lCleared[lAt] := True;
      end;
    end;
  end;

  // Every statement below aRoot, classified as aCounts.
  procedure ClassifyBelow(aRoot: TPasImplElement; aCounts: boolean);
  var
    lStmts: TPasImplElementArray;
  begin
    if aRoot = nil then
      Exit;
    // An asm routine body is the root itself, which carries no child statement.
    if aRoot is TPasImplAsmStatement then
    begin
      lAnswered := False;
      Exit;
    end;
    SetLength(lStmts, 0);
    CollectStatements(aRoot, lStmts);
    ClassifyStatements(lStmts, aCounts);
  end;

begin
  if aContext.Resolver = nil then
    Exit;
  lModule := aContext.Resolver.ResolvedModule;
  if lModule = nil then
    Exit;
  lDecls := EnumerateValueDecls(lModule);
  for i := 0 to High(lDecls) do
    if (lDecls[i].Kind = vkVar)
      and (vmThread in lDecls[i].Decl.VarModifiers)
      and (lDecls[i].Decl.VarModifiers * cForeignLinkage = [])
      and (lDecls[i].Decl.AbsoluteExpr = nil)
      and (lDecls[i].Decl.Parent is TImplementationSection) then
    begin
      SetLength(lCands, Length(lCands) + 1);
      lCands[High(lCands)] := lDecls[i].Decl;
    end;
  if Length(lCands) = 0 then
    Exit;
  SetLength(lRead, Length(lCands));
  SetLength(lCleared, Length(lCands));
  lAnswered := True;
  lRoutines := EnumerateRoutines(lModule);
  for i := 0 to High(lRoutines) do
  begin
    ClassifyBelow(lRoutines[i].Block, True);
    if not lAnswered then
      Exit;
  end;
  ClassifyBelow(lModule.InitializationSection, False);
  ClassifyBelow(lModule.FinalizationSection, False);
  if not lAnswered then
    Exit;
  for i := 0 to High(lCands) do
    if lRead[i] and not lCleared[i] then
      EmitAt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCands[i]), [lCands[i].Name],
        lCands[i].Name);
end;


initialization
  // Polarity: absence — only a section tracked in this routine counts as held.
  RegisterRule(TRuleGlobalWrittenFromThreadRoutine.Create(TRuleMetadata.Make(
    'GlobalWrittenFromThreadRoutine', rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, cKeyGlobalWrittenFromThreadRoutine).WithDescription(
    'Flags a write to a unit-level variable in a thread routine that no '
    + 'critical section covers.')));
  RegisterMessage(cKeyGlobalWrittenFromThreadRoutine,
    SGlobalWrittenFromThreadRoutine);

  // Polarity: positive — the dispatch under a held section is itself the defect.
  RegisterRule(TRuleSynchronizeWithLockHeld.Create(TRuleMetadata.Make(
    'SynchronizeWithLockHeld', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeySynchronizeWithLockHeld).WithDescription(
    'Flags a main-thread callback dispatched while a critical section is '
    + 'held.')));
  RegisterMessage(cKeySynchronizeWithLockHeld, SSynchronizeWithLockHeld);

  // Polarity: absence — reported only on locals, whose initialisation is visible.
  RegisterRule(TRuleCriticalSectionNotInitialized.Create(TRuleMetadata.Make(
    'CriticalSectionNotInitialized', rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, cKeyCriticalSectionNotInitialized).WithDescription(
    'Flags a critical-section call on a routine local that no '
    + 'InitCriticalSection precedes.')));
  RegisterMessage(cKeyCriticalSectionNotInitialized,
    SCriticalSectionNotInitialized);

  // Polarity: positive — the unwrapped UI access is itself the defect.
  RegisterRule(TRuleVclAccessOffMainThread.Create(TRuleMetadata.Make(
    'VclAccessOffMainThread', rtAst, rfAst, sevMajor, itBug, cfLow, False,
    cKeyVclAccessOffMainThread).WithDescription(
    'Flags a UI-control member accessed directly in the Execute body of a '
    + 'class reaching TThread by written ancestor name; the member names are a '
    + 'curated list.')));
  RegisterMessage(cKeyVclAccessOffMainThread, SVclAccessOffMainThread);

  // Polarity: absence — only a write in a routine body counts as thread-local.
  RegisterRule(TRuleThreadvarInitialization.Create(TRuleMetadata.Make(
    'ThreadvarInitialization', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyThreadvarInitialization).WithDescription(
    'Flags an implementation-section threadvar that a routine body reads and '
    + 'that no routine body assigns.')));
  RegisterMessage(cKeyThreadvarInitialization, SThreadvarInitialization);

end.
