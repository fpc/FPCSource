{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Type-cast semantic analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Casts;

{ Cast SEM rules (rtSem / rfResolver) for type-cast hazards:
  Char/PChar, out-of-hierarchy object casts, redundant and platform-dependent
  casts, cast-before-free, Unicode->Ansi. }

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
  { Flags a single character cast to a character-pointer type. }
  TRuleCharToCharPointerCast = class(TRuleBase)
  public
    // Emits one issue per char-to-char-pointer cast.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a class cast between two unrelated class types. }
  TRuleObjectCastNotInHierarchy = class(TRuleBase)
  public
    // Emits one issue per cast between sibling/unrelated class types.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a cast whose operand already has the target type. }
  TRuleRedundantCast = class(TRuleBase)
  public
    // Emits one issue per redundant cast.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a class typecast that exists only to call .Free / FreeAndNil. }
  TRuleObjectCastBeforeFree = class(TRuleBase)
  public
    // Emits one issue per class cast disposed by .Free or FreeAndNil.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a cast from a Unicode/wide string-or-char type to an ANSI one. }
  TRuleUnicodeToAnsiCast = class(TRuleBase)
  public
    // Emits one issue per Unicode/wide-to-ANSI string-or-char cast.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a pointer/class/PChar cast to-or-from a fixed-width integer. }
  TRulePlatformDependentCast = class(TRuleBase)
  public
    // Emits one issue per pointerish<->fixed-width-integer cast (either way).
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a wide integer narrowed to a fixed <=32-bit integer. }
  TRulePlatformDependentTruncation = class(TRuleBase)
  public
    // Emits one issue per wide-integer->narrow-integer cast.
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
  flagAliasCasts = False;

  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyCharToCharPointerCast = 'rule.CharToCharPointerCast.message';
  cKeyObjectCastNotInHierarchy = 'rule.ObjectCastNotInHierarchy.message';
  cKeyRedundantCast = 'rule.RedundantCast.message';
  cKeyObjectCastBeforeFree = 'rule.ObjectCastBeforeFree.message';
  cKeyUnicodeToAnsiCast = 'rule.UnicodeToAnsiCast.message';
  cKeyPlatformDependentCast = 'rule.PlatformDependentCast.message';
  cKeyPlatformDependentTruncation = 'rule.PlatformDependentTruncation.message';

  {  shared cast-candidate collection  }

procedure CollectCasts(aExpr: TPasExpr; var aList: TPasExprArray);
var
  i: integer;
begin
  if aExpr = nil then
    Exit;
  if aExpr is TParamsExpr then
  begin
    if TParamsExpr(aExpr).Kind = pekFuncParams then
    begin
      SetLength(aList, Length(aList) + 1);
      aList[High(aList)] := aExpr;
    end;
    CollectCasts(TParamsExpr(aExpr).Value, aList);
    for i := 0 to High(TParamsExpr(aExpr).Params) do
      CollectCasts(TParamsExpr(aExpr).Params[i], aList);
  end
  else if aExpr is TBinaryExpr then
  begin
    CollectCasts(TBinaryExpr(aExpr).Left, aList);
    CollectCasts(TBinaryExpr(aExpr).Right, aList);
  end
  else if aExpr is TUnaryExpr then
    CollectCasts(TUnaryExpr(aExpr).Operand, aList);
end;


// Flattens aRoot's statement subtree into aList (recursive).
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


// Every pekFuncParams TParamsExpr in aModule's statement expressions
function AllCasts(aModule: TPasModule): TPasExprArray;
var
  lRoots, lStmts: TPasImplElementArray;
  lWith: TPasImplWithDo;
  i, j: integer;

  procedure Take(aExpr: TPasExpr);
  begin
    CollectCasts(aExpr, Result);
  end;

begin
  SetLength(Result, 0);
  if aModule = nil then
    Exit;
  SetLength(lStmts, 0);
  lRoots := EnumerateStatementRoots(aModule);
  for i := 0 to High(lRoots) do
    CollectStatements(lRoots[i], lStmts);
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


{  free-call candidate collection (for ObjectCastBeforeFree)  }

// Appends every disposal-shaped node to aList
procedure CollectFreeCalls(aExpr: TPasExpr; var aList: TPasExprArray);
var
  i: integer;
begin
  if aExpr = nil then
    Exit;
  if aExpr is TParamsExpr then
  begin
    if TParamsExpr(aExpr).Kind = pekFuncParams then
    begin
      SetLength(aList, Length(aList) + 1);
      aList[High(aList)] := aExpr;
    end;
    CollectFreeCalls(TParamsExpr(aExpr).Value, aList);
    for i := 0 to High(TParamsExpr(aExpr).Params) do
      CollectFreeCalls(TParamsExpr(aExpr).Params[i], aList);
  end
  else if aExpr is TBinaryExpr then
  begin
    if TBinaryExpr(aExpr).OpCode = eopSubIdent then
    begin
      SetLength(aList, Length(aList) + 1);
      aList[High(aList)] := aExpr;
    end;
    CollectFreeCalls(TBinaryExpr(aExpr).Left, aList);
    CollectFreeCalls(TBinaryExpr(aExpr).Right, aList);
  end
  else if aExpr is TUnaryExpr then
    CollectFreeCalls(TUnaryExpr(aExpr).Operand, aList);
end;


// Every disposal-shaped node in aModule's statement expressions
function AllFreeCandidates(aModule: TPasModule): TPasExprArray;
var
  lRoots, lStmts: TPasImplElementArray;
  lWith: TPasImplWithDo;
  i, j: integer;

  procedure Take(aExpr: TPasExpr);
  begin
    CollectFreeCalls(aExpr, Result);
  end;

begin
  SetLength(Result, 0);
  if aModule = nil then
    Exit;
  SetLength(lStmts, 0);
  lRoots := EnumerateStatementRoots(aModule);
  for i := 0 to High(lRoots) do
    CollectStatements(lRoots[i], lStmts);
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
procedure EmitCast(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aLine: integer;
  const aArgs: array of string; const aSnippet: string);
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aLine, 1, aLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    aArgs, aSnippet);
end;


{ TRuleCharToCharPointerCast }

procedure TRuleCharToCharPointerCast.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCasts: TPasExprArray;
  lTarget, lSource, lDest: TFpSonarResolvedType;
  i: integer;
begin
  lCasts := AllCasts(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCasts) do
  begin
    if not aContext.Resolver.TryTypecast(lCasts[i], lTarget, lSource) then
      Continue;
    // Operand must be a single character
    if lSource.Kind <> ltkChar then
      Continue;
    // Target must be a pointer type whose DestType resolves to a char base type
    if (lTarget.Kind <> ltkPointer) or not (lTarget.TypeEl is TPasPointerType) then
      Continue;
    if not aContext.Resolver.TryResolvedType(
      TPasPointerType(lTarget.TypeEl).DestType, lDest) then
      Continue;
    if lDest.Kind <> ltkChar then
      Continue;
    EmitCast(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lCasts[i]),
      [lTarget.NamedTypeName], lTarget.NamedTypeName);
  end;
end;


{ TRuleObjectCastNotInHierarchy }

procedure TRuleObjectCastNotInHierarchy.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCasts: TPasExprArray;
  lTarget, lSource: TFpSonarResolvedType;
  lRelated: boolean;
  i: integer;
begin
  lCasts := AllCasts(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCasts) do
  begin
    if not aContext.Resolver.TryTypecast(lCasts[i], lTarget, lSource) then
      Continue;
    // Both sides must be instance class types
    if (lTarget.Kind <> ltkClass) or (lSource.Kind <> ltkClass) then
      Continue;
    // An undeterminable relation degrades to silence
    if not aContext.Resolver.TryClassRelation(lSource.TypeEl, lTarget.TypeEl,
      lRelated) then
      Continue;
    if lRelated then
      Continue;
    EmitCast(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lCasts[i]),
      [lSource.NamedTypeName, lTarget.NamedTypeName], lTarget.NamedTypeName);
  end;
end;


{ TRuleRedundantCast }

procedure TRuleRedundantCast.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCasts: TPasExprArray;
  lTarget, lSource: TFpSonarResolvedType;
  i: integer;
begin
  lCasts := AllCasts(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCasts) do
  begin
    if not aContext.Resolver.TryTypecast(lCasts[i], lTarget, lSource) then
      Continue;
    // Redundant = the operand already has the target type: identical alias-collapsed element AND kind.
    if (lTarget.TypeEl = lSource.TypeEl)
      and (lTarget.Kind = lSource.Kind)
      and (flagAliasCasts or (lTarget.NamedTypeName = lSource.NamedTypeName)) then
      EmitCast(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCasts[i]),
        [lTarget.NamedTypeName], lTarget.NamedTypeName);
  end;
end;


{ TRuleObjectCastBeforeFree }

procedure TRuleObjectCastBeforeFree.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCands: TPasExprArray;
  lInner: TPasExpr;
  lTarget, lSource: TFpSonarResolvedType;
  i: integer;
begin
  // Disposal-shaped candidates: '<x>.Free' member accesses + 'FreeAndNil(<x>)' calls.
  lCands := AllFreeCandidates(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCands) do
  begin
    if aContext.Resolver.TryFreeCall(lCands[i], lInner) = lfkNone then
      Continue;
    // The disposed expression must itself be a confirmed class->class typecast;
    if not aContext.Resolver.TryTypecast(lInner, lTarget, lSource) then
      Continue;
    if (lSource.Kind <> ltkClass) or (lTarget.Kind <> ltkClass) then
      Continue;
    EmitCast(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lInner),
      [lTarget.NamedTypeName], lTarget.NamedTypeName);
  end;
end;


{ TRuleUnicodeToAnsiCast }

procedure TRuleUnicodeToAnsiCast.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCasts: TPasExprArray;
  lTarget, lSource: TFpSonarResolvedType;
  i: integer;
begin
  lCasts := AllCasts(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCasts) do
  begin
    if not aContext.Resolver.TryTypecast(lCasts[i], lTarget, lSource) then
      Continue;
    // Fire only on a confirmed lossy: a Unicode/wide source cast to  an ANSI target.
    if (lSource.Encoding = lseWide) and (lTarget.Encoding = lseAnsi) then
      EmitCast(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCasts[i]),
        [lTarget.NamedTypeName], lTarget.NamedTypeName);
  end;
end;


{ TRulePlatformDependentCast }

// True for a resolved kind that holds an address: a pointer type
function IsPointerish(aKind: TFpSonarTypeKind): boolean;
begin
  Result := aKind in [ltkPointer, ltkClass, ltkClassRef];
end;


procedure TRulePlatformDependentCast.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCasts: TPasExprArray;
  lTarget, lSource: TFpSonarResolvedType;
  i: integer;
begin
  lCasts := AllCasts(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCasts) do
  begin
    if not aContext.Resolver.TryTypecast(lCasts[i], lTarget, lSource) then
      Continue;
    // Fire (DEGRADED) only on the unambiguous shape
    if (IsPointerish(lTarget.Kind) and (lSource.IntWidth = liwFixed)) then
      EmitCast(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCasts[i]),
        [lSource.NamedTypeName], lSource.NamedTypeName)
    else if (IsPointerish(lSource.Kind) and (lTarget.IntWidth = liwFixed)) then
      EmitCast(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCasts[i]),
        [lTarget.NamedTypeName], lTarget.NamedTypeName);
  end;
end;


{ TRulePlatformDependentTruncation }

procedure TRulePlatformDependentTruncation.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCasts: TPasExprArray;
  lTarget, lSource: TFpSonarResolvedType;
  i: integer;
begin
  lCasts := AllCasts(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCasts) do
  begin
    if not aContext.Resolver.TryTypecast(lCasts[i], lTarget, lSource) then
      Continue;
    // Fire only on a wide integer narrowed to a fixed <=32-bit one:
    if (lSource.IntWidth in [liwWide, liwPointerSized])
      and (lTarget.IntWidth = liwFixed) then
      EmitCast(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCasts[i]),
        [lTarget.NamedTypeName], lTarget.NamedTypeName);
  end;
end;


initialization
  RegisterRule(TRuleCharToCharPointerCast.Create(TRuleMetadata.Make(
    'CharToCharPointerCast', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyCharToCharPointerCast).WithDescription(
    'Flags casting a single character to a string pointer, which yields a '
    + 'pointer to a temporary rather than a string.')));
  RegisterMessage(cKeyCharToCharPointerCast, SCharToCharPointerCast);

  RegisterRule(TRuleObjectCastNotInHierarchy.Create(TRuleMetadata.Make(
    'ObjectCastNotInHierarchy', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyObjectCastNotInHierarchy).WithDescription(
    'Flags a cast between unrelated class types that can never succeed at run '
    + 'time.')));
  RegisterMessage(cKeyObjectCastNotInHierarchy, SObjectCastNotInHierarchy);

  RegisterRule(TRuleRedundantCast.Create(TRuleMetadata.Make(
    'RedundantCast', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyRedundantCast).WithDescription(
    'Flags a cast to a type the operand already has.')));
  RegisterMessage(cKeyRedundantCast, SRedundantCast);

  RegisterRule(TRuleObjectCastBeforeFree.Create(TRuleMetadata.Make(
    'ObjectCastBeforeFree', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyObjectCastBeforeFree).WithDescription(
    'Flags a redundant cast before Free, which already operates on TObject.')));
  RegisterMessage(cKeyObjectCastBeforeFree, SObjectCastBeforeFree);

  RegisterRule(TRuleUnicodeToAnsiCast.Create(TRuleMetadata.Make(
    'UnicodeToAnsiCast', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyUnicodeToAnsiCast).WithDescription(
    'Flags a cast to an ANSI type from a Unicode/wide type, which silently '
    + 'loses characters.')));
  RegisterMessage(cKeyUnicodeToAnsiCast, SUnicodeToAnsiCast);

  RegisterRule(TRulePlatformDependentCast.Create(TRuleMetadata.Make(
    'PlatformDependentCast', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyPlatformDependentCast).WithDescription(
    'Flags a cast between a pointer and a fixed-width integer, which is not '
    + '64-bit safe; use PtrInt/PtrUInt.')));
  RegisterMessage(cKeyPlatformDependentCast, SPlatformDependentCast);

  RegisterRule(TRulePlatformDependentTruncation.Create(TRuleMetadata.Make(
    'PlatformDependentTruncation', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyPlatformDependentTruncation).WithDescription(
    'Flags narrowing a wide integer to a fixed-width type, which silently drops '
    + 'high bits on 64-bit.')));
  RegisterMessage(cKeyPlatformDependentTruncation, SPlatformDependentTruncation);

end.
