{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Constant-evaluation semantic analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Eval;


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
  { Flags a /, div or mod whose divisor folds to a constant zero.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleDivisionByZeroConstant = class(TRuleBase)
  public
    // Emits one issue per division by a constant zero.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an if/while/until condition that folds to a constant boolean.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleConstantConditionAlwaysTrueOrFalse = class(TRuleBase)
  public
    // Emits one issue per constant-folding condition, bar the deliberate
    // infinite loops `while True` and `repeat .. until False`.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a comparison whose verdict is fixed by the range of the compared
    operand's type.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleComparisonAlwaysTrueForType = class(TRuleBase)
  public
    // Emits one issue per comparison fixed by the operand's type range.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an assignment of a constant outside the range of the target type.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleConstantOutOfRangeForTarget = class(TRuleBase)
  public
    // Emits one issue per assignment whose constant does not fit the target.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a constant arithmetic expression that overflows the integer range.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleConstantOverflowInExpression = class(TRuleBase)
  public
    // Emits one issue per overflowing constant add, subtract or multiply.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a shl/shr whose constant count exceeds the shifted type's width.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleShiftCountExceedsWidth = class(TRuleBase)
  public
    // Emits one issue per shift wider than the shifted operand's type.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an `in` whose set constructor holds an out-of-range element.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleSetElementOutOfRange = class(TRuleBase)
  public
    // Emits one issue per set constructor outside the left operand's range.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an integer-to-enumeration cast outside the enumeration's ordinals.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleEnumOrdinalOutOfRange = class(TRuleBase)
  public
    // Emits one issue per out-of-range enumeration cast.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a constant array index outside the declared bounds.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleArrayIndexConstantOutOfBounds = class(TRuleBase)
  public
    // Emits one issue per array access whose first offending subscript is out
    // of bounds.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a SizeOf whose argument is a class, interface, dynamic array or long
    string, where the answer is the pointer size.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleSizeOfOnReferenceType = class(TRuleBase)
  public
    // Emits one issue per SizeOf of a reference type.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a Move/FillChar byte count that counts elements or sizes a pointer.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleMoveFillCharSizeMismatch = class(TRuleBase)
  public
    // Emits one issue per Move/FillChar whose count is not a byte count.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an exact equality comparison on a floating-point operand.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleFloatEqualityComparison = class(TRuleBase)
  public
    // Emits one issue per float = or <> comparison.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an integer div whose result is assigned to a floating-point target.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleIntegerDivisionAssignedToFloat = class(TRuleBase)
  public
    // Emits one issue per integer division assigned to a float.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a comparison of a signed against an unsigned integer of equal width.
    Polarity: positive detection — it reports the presence of the defect. }
  TRuleMixedSignedUnsignedComparison = class(TRuleBase)
  public
    // Emits one issue per signed/unsigned comparison at the same width.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

const
  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyDivisionByZeroConstant = 'rule.DivisionByZeroConstant.message';
  cKeyConstantConditionAlwaysTrueOrFalse =
    'rule.ConstantConditionAlwaysTrueOrFalse.message';
  cKeyComparisonAlwaysTrueForType =
    'rule.ComparisonAlwaysTrueForType.message';
  cKeyConstantOutOfRangeForTarget =
    'rule.ConstantOutOfRangeForTarget.message';
  cKeyConstantOverflowInExpression =
    'rule.ConstantOverflowInExpression.message';
  cKeyShiftCountExceedsWidth = 'rule.ShiftCountExceedsWidth.message';
  cKeySetElementOutOfRange = 'rule.SetElementOutOfRange.message';
  cKeyEnumOrdinalOutOfRange = 'rule.EnumOrdinalOutOfRange.message';
  cKeyArrayIndexConstantOutOfBounds =
    'rule.ArrayIndexConstantOutOfBounds.message';
  cKeySizeOfOnReferenceType = 'rule.SizeOfOnReferenceType.message';
  cKeyMoveFillCharSizeMismatch = 'rule.MoveFillCharSizeMismatch.message';
  cKeyFloatEqualityComparison = 'rule.FloatEqualityComparison.message';
  cKeyIntegerDivisionAssignedToFloat =
    'rule.IntegerDivisionAssignedToFloat.message';
  cKeyMixedSignedUnsignedComparison =
    'rule.MixedSignedUnsignedComparison.message';

  cValueNames: array[boolean] of string = ('False', 'True');

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


{ TRuleDivisionByZeroConstant }

procedure TRuleDivisionByZeroConstant.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lOperator: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryConstDivisionByZero(lExprs[i], lOperator) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lOperator], lOperator);
end;


{ TRuleConstantConditionAlwaysTrueOrFalse }

procedure TRuleConstantConditionAlwaysTrueOrFalse.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
type
  TConditionKind = (ckIf, ckWhile, ckRepeat);
var
  lStmts: TPasImplElementArray;
  lCondition: TPasExpr;
  lKeyword: string;
  lKind: TConditionKind;
  lValue, lInfinite: boolean;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
  begin
    if lStmts[i] is TPasImplIfElse then
    begin
      lCondition := TPasImplIfElse(lStmts[i]).ConditionExpr;
      lKeyword := 'if';
      lKind := ckIf;
    end
    else if lStmts[i] is TPasImplWhileDo then
    begin
      lCondition := TPasImplWhileDo(lStmts[i]).ConditionExpr;
      lKeyword := 'while';
      lKind := ckWhile;
    end
    else if lStmts[i] is TPasImplRepeatUntil then
    begin
      lCondition := TPasImplRepeatUntil(lStmts[i]).ConditionExpr;
      lKeyword := 'repeat';
      lKind := ckRepeat;
    end
    else
      Continue;
    if not aContext.Resolver.TryConstBooleanValue(lCondition, lValue) then
      Continue;
    // `while True` and `repeat .. until False` are the deliberate infinite loop.
    lInfinite := ((lKind = ckWhile) and lValue)
      or ((lKind = ckRepeat) and not lValue);
    if lInfinite then
      Continue;
    EmitStmt(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lCondition),
      [lKeyword, cValueNames[lValue]], lKeyword);
  end;
end;


{ TRuleComparisonAlwaysTrueForType }

procedure TRuleComparisonAlwaysTrueForType.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lTypeName, lConstant: string;
  lValue: boolean;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryComparisonAlwaysTrueForType(lExprs[i], lTypeName,
      lConstant, lValue) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]),
        [lTypeName, lConstant, cValueNames[lValue]], lTypeName);
end;


{ TRuleConstantOutOfRangeForTarget }

procedure TRuleConstantOutOfRangeForTarget.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lConstant, lTypeName: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if (lStmts[i] is TPasImplAssign)
      and aContext.Resolver.TryConstantOutOfRangeForTarget(lStmts[i],
        lConstant, lTypeName) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lStmts[i]), [lConstant, lTypeName],
        lConstant);
end;


{ TRuleConstantOverflowInExpression }

procedure TRuleConstantOverflowInExpression.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lOperator: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryConstantOverflowInExpression(lExprs[i],
      lOperator) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lOperator], lOperator);
end;


{ TRuleShiftCountExceedsWidth }

procedure TRuleShiftCountExceedsWidth.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lCount, lTypeName: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryShiftCountExceedsWidth(lExprs[i], lCount,
      lTypeName) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lCount, lTypeName], lCount);
end;


{ TRuleSetElementOutOfRange }

procedure TRuleSetElementOutOfRange.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lTypeName: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TrySetElementOutOfRange(lExprs[i], lTypeName) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lTypeName], lTypeName);
end;


{ TRuleEnumOrdinalOutOfRange }

procedure TRuleEnumOrdinalOutOfRange.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lOrdinal, lEnumName: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryEnumOrdinalOutOfRange(lExprs[i], lOrdinal,
      lEnumName) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lOrdinal, lEnumName],
        lOrdinal);
end;


{ TRuleArrayIndexConstantOutOfBounds }

procedure TRuleArrayIndexConstantOutOfBounds.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lIndex, lArrayName: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryArrayIndexConstantOutOfBounds(lExprs[i], lIndex,
      lArrayName) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lIndex, lArrayName], lIndex);
end;


{ TRuleSizeOfOnReferenceType }

procedure TRuleSizeOfOnReferenceType.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lTypeName: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TrySizeOfOnReferenceType(lExprs[i], lTypeName) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lTypeName], lTypeName);
end;


{ TRuleMoveFillCharSizeMismatch }

procedure TRuleMoveFillCharSizeMismatch.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lCallee, lCount: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryMoveFillCharSizeMismatch(lExprs[i], lCallee,
      lCount) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lCallee, lCount], lCallee);
end;


{ TRuleFloatEqualityComparison }

procedure TRuleFloatEqualityComparison.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lTypeName: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryFloatEqualityComparison(lExprs[i], lTypeName) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lTypeName], lTypeName);
end;


{ TRuleIntegerDivisionAssignedToFloat }

procedure TRuleIntegerDivisionAssignedToFloat.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lTypeName: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if (lStmts[i] is TPasImplAssign)
      and aContext.Resolver.TryIntegerDivisionAssignedToFloat(lStmts[i],
        lTypeName) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lStmts[i]), [lTypeName], lTypeName);
end;


{ TRuleMixedSignedUnsignedComparison }

procedure TRuleMixedSignedUnsignedComparison.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lExprs: TPasExprArray;
  lSignedType, lUnsignedType: string;
  i: integer;
begin
  lExprs := AllExpressions(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lExprs) do
    if aContext.Resolver.TryMixedSignedUnsignedComparison(lExprs[i],
      lSignedType, lUnsignedType) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lExprs[i]), [lSignedType, lUnsignedType],
        lSignedType);
end;


initialization
  RegisterRule(TRuleDivisionByZeroConstant.Create(TRuleMetadata.Make(
    'DivisionByZeroConstant', rtSem, rfResolver, sevCritical, itBug, cfHigh,
    False, cKeyDivisionByZeroConstant).WithDescription(
    'Flags a division, div or mod whose divisor is a constant zero.')));
  RegisterMessage(cKeyDivisionByZeroConstant, SDivisionByZeroConstant);

  RegisterRule(TRuleConstantConditionAlwaysTrueOrFalse.Create(TRuleMetadata.Make(
    'ConstantConditionAlwaysTrueOrFalse', rtSem, rfResolver, sevMajor,
    itCodeSmell, cfMedium,
    False, cKeyConstantConditionAlwaysTrueOrFalse).WithDescription(
    'Flags an if or loop condition that constant-folds to a fixed boolean.')));
  RegisterMessage(cKeyConstantConditionAlwaysTrueOrFalse,
    SConstantConditionAlwaysTrueOrFalse);

  RegisterRule(TRuleComparisonAlwaysTrueForType.Create(TRuleMetadata.Make(
    'ComparisonAlwaysTrueForType', rtSem, rfResolver, sevMajor, itCodeSmell,
    cfMedium, False, cKeyComparisonAlwaysTrueForType).WithDescription(
    'Flags a comparison whose result is fixed by the range of the compared '
    + 'operand''s type.')));
  RegisterMessage(cKeyComparisonAlwaysTrueForType, SComparisonAlwaysTrueForType);

  RegisterRule(TRuleConstantOutOfRangeForTarget.Create(TRuleMetadata.Make(
    'ConstantOutOfRangeForTarget', rtSem, rfResolver, sevCritical, itBug,
    cfHigh, False, cKeyConstantOutOfRangeForTarget).WithDescription(
    'Flags an assignment of a constant outside the range of the target type.')));
  RegisterMessage(cKeyConstantOutOfRangeForTarget, SConstantOutOfRangeForTarget);

  RegisterRule(TRuleConstantOverflowInExpression.Create(TRuleMetadata.Make(
    'ConstantOverflowInExpression', rtSem, rfResolver, sevCritical, itBug,
    cfHigh, False, cKeyConstantOverflowInExpression).WithDescription(
    'Flags a constant arithmetic expression that overflows the integer range.')));
  RegisterMessage(cKeyConstantOverflowInExpression,
    SConstantOverflowInExpression);

  RegisterRule(TRuleShiftCountExceedsWidth.Create(TRuleMetadata.Make(
    'ShiftCountExceedsWidth', rtSem, rfResolver, sevCritical, itBug, cfHigh,
    False, cKeyShiftCountExceedsWidth).WithDescription(
    'Flags a shl or shr whose constant count exceeds the width of the '
    + 'shifted type.')));
  RegisterMessage(cKeyShiftCountExceedsWidth, SShiftCountExceedsWidth);

  RegisterRule(TRuleSetElementOutOfRange.Create(TRuleMetadata.Make(
    'SetElementOutOfRange', rtSem, rfResolver, sevCritical, itBug, cfHigh,
    False, cKeySetElementOutOfRange).WithDescription(
    'Flags a set constructor holding an element outside the range of the '
    + 'tested operand.')));
  RegisterMessage(cKeySetElementOutOfRange, SSetElementOutOfRange);

  RegisterRule(TRuleEnumOrdinalOutOfRange.Create(TRuleMetadata.Make(
    'EnumOrdinalOutOfRange', rtSem, rfResolver, sevCritical, itBug, cfHigh,
    False, cKeyEnumOrdinalOutOfRange).WithDescription(
    'Flags an integer-to-enumeration cast outside the declared ordinals.')));
  RegisterMessage(cKeyEnumOrdinalOutOfRange, SEnumOrdinalOutOfRange);

  RegisterRule(TRuleArrayIndexConstantOutOfBounds.Create(TRuleMetadata.Make(
    'ArrayIndexConstantOutOfBounds', rtSem, rfResolver, sevCritical, itBug,
    cfHigh, False, cKeyArrayIndexConstantOutOfBounds).WithDescription(
    'Flags a constant array index outside the declared bounds.')));
  RegisterMessage(cKeyArrayIndexConstantOutOfBounds,
    SArrayIndexConstantOutOfBounds);

  RegisterRule(TRuleSizeOfOnReferenceType.Create(TRuleMetadata.Make(
    'SizeOfOnReferenceType', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeySizeOfOnReferenceType).WithDescription(
    'Flags a SizeOf of a class, interface, dynamic array or long string, '
    + 'which yields the pointer size.')));
  RegisterMessage(cKeySizeOfOnReferenceType, SSizeOfOnReferenceType);

  RegisterRule(TRuleMoveFillCharSizeMismatch.Create(TRuleMetadata.Make(
    'MoveFillCharSizeMismatch', rtSem, rfResolver, sevCritical, itBug,
    cfMedium, False, cKeyMoveFillCharSizeMismatch).WithDescription(
    'Flags a Move or FillChar byte count that counts elements or sizes a '
    + 'pointer instead of the data.')));
  RegisterMessage(cKeyMoveFillCharSizeMismatch, SMoveFillCharSizeMismatch);

  RegisterRule(TRuleFloatEqualityComparison.Create(TRuleMetadata.Make(
    'FloatEqualityComparison', rtSem, rfResolver, sevMajor, itCodeSmell,
    cfMedium, False, cKeyFloatEqualityComparison).WithDescription(
    'Flags an exact equality comparison on a floating-point operand.')));
  RegisterMessage(cKeyFloatEqualityComparison, SFloatEqualityComparison);

  RegisterRule(TRuleIntegerDivisionAssignedToFloat.Create(TRuleMetadata.Make(
    'IntegerDivisionAssignedToFloat', rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, cKeyIntegerDivisionAssignedToFloat).WithDescription(
    'Flags an integer div whose truncated result is assigned to a '
    + 'floating-point target.')));
  RegisterMessage(cKeyIntegerDivisionAssignedToFloat,
    SIntegerDivisionAssignedToFloat);

  RegisterRule(TRuleMixedSignedUnsignedComparison.Create(TRuleMetadata.Make(
    'MixedSignedUnsignedComparison', rtSem, rfResolver, sevMajor, itCodeSmell,
    cfMedium, False, cKeyMixedSignedUnsignedComparison).WithDescription(
    'Flags a comparison of a signed against an unsigned integer of the same '
    + 'width.')));
  RegisterMessage(cKeyMixedSignedUnsignedComparison,
    SMixedSignedUnsignedComparison);

end.
