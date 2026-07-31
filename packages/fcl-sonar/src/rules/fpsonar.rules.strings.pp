{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    String, character and encoding analysis rules, resolver tier

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Strings;


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
  { Flags a character-pointer cast of a string temporary the statement frees.
    Polarity: positive detection — it reports the presence of the defect. }
  TRulePCharOfTemporaryString = class(TRuleBase)
  public
    // Emits one issue per character-pointer cast of a string temporary.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an assignment converting a wide string to an ANSI one with no cast.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleImplicitStringConversionWithDataLoss = class(TRuleBase)
  public
    // Emits one issue per implicit wide-to-ANSI string assignment.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a Move/FillChar byte count taken from Length of a wide string.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleLengthUsedAsByteCount = class(TRuleBase)
  public
    // Emits one issue per Move/FillChar counted by a wide string's Length.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a string Copy whose start index is 0 rather than 1.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleCopyWithZeroIndex = class(TRuleBase)
  public
    // Emits one issue per string Copy started at index 0.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a Pos result compared as if a miss returned a negative value.
    Polarity: positive detection — it reports the presence of the defect. }
  TRulePosResultComparedToZeroBased = class(TRuleBase)
  public
    // Emits one issue per Pos result compared to 0 or -1 as a zero-based index.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a constant assigned to a string[N] that cannot hold it.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleShortStringTruncation = class(TRuleBase)
  public
    // Emits one issue per assignment of an over-long constant to a string[N].
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a char compared to a string constant that is not one character long.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleCharComparedToString = class(TRuleBase)
  public
    // Emits one issue per char compared to a multi- or zero-character constant.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a RawByteString assigned to or from a code-paged string.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleRawByteStringCodePageMix = class(TRuleBase)
  public
    // Emits one issue per uncast assignment mixing RawByteString and a code page.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a string rebuilt by concatenating onto itself inside a loop.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleStringConcatInLoop = class(TRuleBase)
  public
    // Emits one issue per self-concatenating assignment enclosed by a loop.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a StrToInt on non-constant text that no handler guards.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleStrToIntWithoutGuard = class(TRuleBase)
  public
    // Emits one issue per unguarded StrToInt call on a non-folding argument.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a WideString declaration in a unit built for a non-Windows target.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleWideStringOnNonWindows = class(TRuleBase)
  public
    // Emits one issue per variable or argument declared WideString.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an indexed read of a declaration a SetLength left unwritten.
    Polarity: absence -- an intervening write satisfies it. }
  TRuleSetLengthWithoutFill = class(TRuleBase)
  public
    // Emits one issue per indexed read of storage no write followed.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

uses
  FpSonar.DataFlow;

const
  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyPCharOfTemporaryString = 'rule.PCharOfTemporaryString.message';
  cKeyImplicitStringConversionWithDataLoss =
    'rule.ImplicitStringConversionWithDataLoss.message';
  cKeyLengthUsedAsByteCount = 'rule.LengthUsedAsByteCount.message';
  cKeyCopyWithZeroIndex = 'rule.CopyWithZeroIndex.message';
  cKeyPosResultComparedToZeroBased =
    'rule.PosResultComparedToZeroBased.message';
  cKeyShortStringTruncation = 'rule.ShortStringTruncation.message';
  cKeyCharComparedToString = 'rule.CharComparedToString.message';
  cKeyRawByteStringCodePageMix = 'rule.RawByteStringCodePageMix.message';
  cKeyStringConcatInLoop = 'rule.StringConcatInLoop.message';
  cKeyStrToIntWithoutGuard = 'rule.StrToIntWithoutGuard.message';
  cKeyWideStringOnNonWindows = 'rule.WideStringOnNonWindows.message';
  cKeySetLengthWithoutFill = 'rule.SetLengthWithoutFill.message';

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
  if aModule = nil then
    Exit;
  lRoots := EnumerateStatementRoots(aModule);
  for i := 0 to High(lRoots) do
    CollectStatements(lRoots[i], Result);
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


// Every expression node in aModule's statement expressions
function AllExpressions(aModule: TPasModule): TPasExprArray;
var
  lStmts: TPasImplElementArray;
  lWith: TPasImplWithDo;
  i, j: integer;

  procedure Take(aExpr: TPasExpr);
  begin
    CollectExpressions(aExpr, Result);
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


// Emits one issue at aLine, column 1
procedure EmitStmt(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aLine: integer;
  const aArgs: array of string; const aSnippet: string);
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aLine, 1, aLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    aArgs, aSnippet);
end;


{ TRulePCharOfTemporaryString }

procedure TRulePCharOfTemporaryString.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lTarget: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryPCharOfTemporaryString(lExprs[i], lTarget) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lTarget], lTarget);
end;


{ TRuleImplicitStringConversionWithDataLoss }

procedure TRuleImplicitStringConversionWithDataLoss.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lSource, lTarget: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if aContext.Resolver.TryImplicitStringConversion(lStmts[i], lSource,
      lTarget) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lStmts[i]), [lSource, lTarget], lSource);
end;


{ TRuleLengthUsedAsByteCount }

procedure TRuleLengthUsedAsByteCount.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lStringName, lCallee: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryLengthUsedAsByteCount(lExprs[i], lStringName,
      lCallee) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lStringName, lCallee],
        lStringName);
end;


{ TRuleCopyWithZeroIndex }

procedure TRuleCopyWithZeroIndex.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lStringName: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryCopyWithZeroIndex(lExprs[i], lStringName) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lStringName], lStringName);
end;


{ TRulePosResultComparedToZeroBased }

procedure TRulePosResultComparedToZeroBased.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lCallee, lConstText: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryPosResultComparedToZeroBased(lExprs[i], lCallee,
      lConstText) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lCallee, lConstText], lCallee);
end;


{ TRuleShortStringTruncation }

procedure TRuleShortStringTruncation.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lTargetName, lCapacity: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if aContext.Resolver.TryShortStringTruncation(lStmts[i], lTargetName,
      lCapacity) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lStmts[i]), [lTargetName, lCapacity],
        lTargetName);
end;


{ TRuleCharComparedToString }

procedure TRuleCharComparedToString.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lCharTypeName, lLength: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryCharComparedToString(lExprs[i], lCharTypeName,
      lLength) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lCharTypeName, lLength],
        lCharTypeName);
end;


{ TRuleRawByteStringCodePageMix }

procedure TRuleRawByteStringCodePageMix.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lRawName, lCodePage: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if aContext.Resolver.TryRawByteStringCodePageMix(lStmts[i], lRawName,
      lCodePage) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lStmts[i]), [lRawName, lCodePage],
        lRawName);
end;


{ TRuleStringConcatInLoop }

procedure TRuleStringConcatInLoop.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lTargetName: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if aContext.Resolver.TryStringConcatInLoop(lStmts[i], lTargetName) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lStmts[i]), [lTargetName], lTargetName);
end;


{ TRuleStrToIntWithoutGuard }

procedure TRuleStrToIntWithoutGuard.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lArgName: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryStrToIntWithoutGuard(lExprs[i], lArgName) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lArgName], lArgName);
end;


{ TRuleWideStringOnNonWindows }

procedure TRuleWideStringOnNonWindows.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lDecls: TPasElementArray;
  lName: string;
  i: integer;

  procedure Take(aDecl: TPasElement);
  begin
    if aDecl = nil then
      Exit;
    SetLength(lDecls, Length(lDecls) + 1);
    lDecls[High(lDecls)] := aDecl;
  end;

  // Section-level and member value declarations, routine locals and arguments.
  procedure CollectDeclarations(aModule: TPasModule);
  var
    lValues: TPasValueDeclArray;
    lRoutines: TAstRoutineArray;
    lProc: TPasProcedure;
    j, k: integer;
  begin
    lValues := EnumerateValueDecls(aModule);
    for j := 0 to High(lValues) do
      Take(lValues[j].Decl);
    lRoutines := EnumerateRoutines(aModule);
    for j := 0 to High(lRoutines) do
    begin
      lProc := lRoutines[j].Decl;
      if lProc = nil then
        Continue;
      if (lProc.Body <> nil) and (lProc.Body.Declarations <> nil) then
        for k := 0 to lProc.Body.Declarations.Count - 1 do
          if TObject(lProc.Body.Declarations[k]) is TPasVariable then
            Take(TPasElement(lProc.Body.Declarations[k]));
      if (lProc.ProcType <> nil) and (lProc.ProcType.Args <> nil) then
        for k := 0 to lProc.ProcType.Args.Count - 1 do
          Take(TPasElement(lProc.ProcType.Args[k]));
    end;
  end;

begin
  SetLength(lDecls, 0);
  CollectDeclarations(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lDecls) do
    if aContext.Resolver.TryWideStringDeclaration(lDecls[i], lName) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lDecls[i]), [lName], lName);
end;


{ TRuleSetLengthWithoutFill }

procedure TRuleSetLengthWithoutFill.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

var
  lFlow: TFpSonarDataFlow;
  lFindings: TFpSonarSetLengthFindingArray;
  lOk: boolean;
  i: integer;

begin
  lFlow := TFpSonarDataFlow.Create(aContext.Resolver);
  try
    lOk := lFlow.TrySetLengthFindings(lFindings);
  finally
    lFlow.Free;
  end;
  if not lOk then
    Exit;
  for i := 0 to High(lFindings) do
    EmitStmt(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lFindings[i].Site), [lFindings[i].Name],
      lFindings[i].Name);
end;


initialization
  RegisterRule(TRulePCharOfTemporaryString.Create(TRuleMetadata.Make(
    'PCharOfTemporaryString', rtSem, rfResolver, sevCritical, itBug, cfMedium,
    False, cKeyPCharOfTemporaryString).WithDescription(
    'Flags a character-pointer cast whose operand is a function result or a '
    + 'string concatenation.')));
  RegisterMessage(cKeyPCharOfTemporaryString, SPCharOfTemporaryString);

  RegisterRule(TRuleImplicitStringConversionWithDataLoss.Create(
    TRuleMetadata.Make('ImplicitStringConversionWithDataLoss', rtSem,
    rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyImplicitStringConversionWithDataLoss).WithDescription(
    'Flags an assignment of a wide-encoded string to an ANSI-encoded one.')));
  RegisterMessage(cKeyImplicitStringConversionWithDataLoss,
    SImplicitStringConversionWithDataLoss);

  RegisterRule(TRuleLengthUsedAsByteCount.Create(TRuleMetadata.Make(
    'LengthUsedAsByteCount', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyLengthUsedAsByteCount).WithDescription(
    'Flags a Move or FillChar whose byte count is the Length of a '
    + 'two-byte-per-character string.')));
  RegisterMessage(cKeyLengthUsedAsByteCount, SLengthUsedAsByteCount);

  RegisterRule(TRuleCopyWithZeroIndex.Create(TRuleMetadata.Make(
    'CopyWithZeroIndex', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    False, cKeyCopyWithZeroIndex).WithDescription(
    'Flags a string Copy whose start index const-folds to 0 rather than 1.')));
  RegisterMessage(cKeyCopyWithZeroIndex, SCopyWithZeroIndex);

  RegisterRule(TRulePosResultComparedToZeroBased.Create(TRuleMetadata.Make(
    'PosResultComparedToZeroBased', rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, cKeyPosResultComparedToZeroBased).WithDescription(
    'Flags a unit-level Pos result compared to 0 or -1 in a shape that reads '
    + 'a miss as a negative index.')));
  RegisterMessage(cKeyPosResultComparedToZeroBased,
    SPosResultComparedToZeroBased);

  RegisterRule(TRuleShortStringTruncation.Create(TRuleMetadata.Make(
    'ShortStringTruncation', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyShortStringTruncation).WithDescription(
    'Flags a string constant assigned to a string[N] whose declared length is '
    + 'smaller than the constant.')));
  RegisterMessage(cKeyShortStringTruncation, SShortStringTruncation);

  RegisterRule(TRuleCharComparedToString.Create(TRuleMetadata.Make(
    'CharComparedToString', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    False, cKeyCharComparedToString).WithDescription(
    'Flags an equality comparison between a char and a string constant that '
    + 'is not one character long.')));
  RegisterMessage(cKeyCharComparedToString, SCharComparedToString);

  RegisterRule(TRuleRawByteStringCodePageMix.Create(TRuleMetadata.Make(
    'RawByteStringCodePageMix', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyRawByteStringCodePageMix).WithDescription(
    'Flags an assignment between a RawByteString and a string whose '
    + 'declaration writes a code page, with no cast written.')));
  RegisterMessage(cKeyRawByteStringCodePageMix, SRawByteStringCodePageMix);

  RegisterRule(TRuleStringConcatInLoop.Create(TRuleMetadata.Make(
    'StringConcatInLoop', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    False, cKeyStringConcatInLoop).WithDescription(
    'Flags a string assigned the concatenation of itself and another operand '
    + 'inside a for, while or repeat loop.')));
  RegisterMessage(cKeyStringConcatInLoop, SStringConcatInLoop);

  RegisterRule(TRuleStrToIntWithoutGuard.Create(TRuleMetadata.Make(
    'StrToIntWithoutGuard', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyStrToIntWithoutGuard).WithDescription(
    'Flags a unit-level StrToInt on an argument that does not fold to a '
    + 'constant and that no enclosing try..except guards.')));
  RegisterMessage(cKeyStrToIntWithoutGuard, SStrToIntWithoutGuard);

  RegisterRule(TRuleWideStringOnNonWindows.Create(TRuleMetadata.Make(
    'WideStringOnNonWindows', rtSem, rfResolver, sevMinor, itCodeSmell,
    cfMedium, False, cKeyWideStringOnNonWindows).WithDescription(
    'Flags a variable or argument declared WideString, which is a COM BSTR '
    + 'only on Windows.')));
  RegisterMessage(cKeyWideStringOnNonWindows, SWideStringOnNonWindows);

  // Polarity: absence -- a write or a hand-over between the two rows
  // satisfies it.
  RegisterRule(TRuleSetLengthWithoutFill.Create(TRuleMetadata.Make(
    'SetLengthWithoutFill', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeySetLengthWithoutFill).WithDescription(
    'Flags an indexed read of a local whose single SetLength no later '
    + 'statement writes to.')));
  RegisterMessage(cKeySetLengthWithoutFill, SSetLengthWithoutFill);

end.
