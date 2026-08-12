{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Call-site semantic analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Calls;

{ Call-based SEM rules (rtSem / rfResolver): resolver-backed checks on call
  sites and calling conventions — FreeAndNil, constructor/destructor use,
  Format strings, inherited, TStringList, if-then short-circuit, Math overloads
  and related hazards. Each self-registers in initialization. }

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
  { Flags a Format argument whose type mismatches its conversion specifier. }
  TRuleFormatArgumentType = class(TRuleBase)
  public
    // Emits one issue per argument whose resolved kind mismatches its specifier.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a Format call supplied fewer arguments than its specifiers need. }
  TRuleFormatArgumentCount = class(TRuleBase)
  public
    // Emits one issue per Format call with too few inline arguments.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a Format call whose const-folded format string is malformed. }
  TRuleValidFormatString = class(TRuleBase)
  public
    // Emits one issue per Format call with an invalid format string.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags FreeAndNil(X) where X is not a class instance. }
  TRuleFreeAndNilArgument = class(TRuleBase)
  public
    // Emits one issue per FreeAndNil call on a non-class-instance argument.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a constructor called on an instance variable. }
  TRuleConstructorOnInstanceVariable = class(TRuleBase)
  public
    // Emits one issue per constructor call made on an already-allocated instance.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags TStringList.Duplicates set without Sorted:=True. }
  TRuleStringListDuplicatesNeedsSorted = class(TRuleBase)
  public
    // Emits one issue per Duplicates assignment lacking a same-routine Sorted.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a TObject-descendant destructor that won't override Destroy. }
  TRuleDestructorShouldOverrideDestroy = class(TRuleBase)
  public
    // Emits one issue per destructor wrongly named or missing the override.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an override method whose whole body only forwards to inherited. }
  TRuleOverrideOnlyCallsInherited = class(TRuleBase)
  public
    // Emits one issue per override method that does nothing but forward.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an IfThen call whose value arg relies on the condition for safety. }
  TRuleIfThenNotShortCircuit = class(TRuleBase)
  public
    // Emits one issue per IfThen call with a condition-guarded value argument.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a built-in Assert call with no diagnostic message. }
  TRuleAssertWithoutMessage = class(TRuleBase)
  public
    // Emits one issue per single-argument built-in Assert call.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a SysUtils date/time call that reads the global DefaultFormatSettings
    (no TFormatSettings overload bound). }
  TRuleDefaultFormatSettingsInDateFormat = class(TRuleBase)
  public
    // Emits one issue per date/time call bound to a no-settings overload.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an explicit access to a class's default array property
    (Obj.Items[i] where the shorthand Obj[i] is equivalent). }
  TRuleExplicitDefaultArrayProperty = class(TRuleBase)
  public
    // Emits one issue per explicit named access to a default array property.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags reading the first character of a string by literal index
    (S[1]) where an intent-revealing form is clearer. }
  TRuleStringFirstCharByIndex = class(TRuleBase)
  public
    // Emits one issue per string indexed at the low bound (the first character).
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags fetching the last TList element by computing L[L.Count-1]
    instead of the dedicated L.Last. }
  TRuleTListLastByIndex = class(TRuleBase)
  public
    // Emits one issue per TList last-element access written as L[L.Count-1].
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a bare `inherited;` in a method whose class has no overridable
    same-signature ancestor, so the statement binds to nothing and is redundant. }
  TRuleRedundantInherited = class(TRuleBase)
  public
    // Emits one issue per bare 'inherited;' that binds to no overridable parent.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a call to a curated byte/string-conversion routine (e.g.
    TStringList.LoadFromFile) whose bound overload omits the Encoding parameter
    while an encoding-aware sibling overload exists, so it silently relies on the
    platform/locale-dependent TEncoding.Default. }
  TRuleImplicitTEncodingDefault = class(TRuleBase)
  public
    // Emits one issue per curated call bound to an encoding-omitting overload.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a call to a curated standard math routine (e.g. Sqrt, Power) that
    binds its Single-precision overload while a higher-precision Double/Extended
    sibling overload of the same routine is visible in the same scope, so the call
    silently loses precision. }
  TRuleSingleOverloadOfMathFunction = class(TRuleBase)
  public
    // Emits one issue per curated math call bound to its Single-precision overload.
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
  // The Format-rule checks also for too many arguments
  flagSurplusArguments = False;

  // The FPC Format conversion type letters. b (boolean) and o (object/class) are FPC-specific.
  cFormatTypeLetters = ['d', 'u', 'e', 'f', 'g', 'n', 'm', 's', 'p', 'x', 'b', 'o'];

  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyFormatArgumentType = 'rule.FormatArgumentType.message';
  cKeyFormatArgumentCount = 'rule.FormatArgumentCount.message';
  cKeyValidFormatString = 'rule.ValidFormatString.message';
  cKeyFreeAndNilArgument = 'rule.FreeAndNilArgument.message';
  cKeyConstructorOnInstanceVariable = 'rule.ConstructorOnInstanceVariable.message';
  cKeyStringListDuplicatesNeedsSorted =
    'rule.StringListDuplicatesNeedsSorted.message';
  cKeyDestructorShouldOverrideDestroy =
    'rule.DestructorShouldOverrideDestroy.message';
  cKeyOverrideOnlyCallsInherited = 'rule.OverrideOnlyCallsInherited.message';
  cKeyIfThenNotShortCircuit = 'rule.IfThenNotShortCircuit.message';
  cKeyAssertWithoutMessage = 'rule.AssertWithoutMessage.message';
  cKeyDefaultFormatSettingsInDateFormat =
    'rule.DefaultFormatSettingsInDateFormat.message';
  cKeyExplicitDefaultArrayProperty =
    'rule.ExplicitDefaultArrayProperty.message';
  cKeyStringFirstCharByIndex = 'rule.StringFirstCharByIndex.message';
  cKeyTListLastByIndex = 'rule.TListLastByIndex.message';
  cKeyRedundantInherited = 'rule.RedundantInherited.message';
  cKeyImplicitTEncodingDefault = 'rule.ImplicitTEncodingDefault.message';
  cKeySingleOverloadOfMathFunction =
    'rule.SingleOverloadOfMathFunction.message';

type
  // Describe a Format template element:
  TFormatSpec = record
    TypeChar: char;    // (lower-cased) type letter
    ArgIndex: integer;
    // the 0-based positional argument index it (possibly modified by a %N: index specifier).
  end;
  TFormatSpecArray = array of TFormatSpec;

  // The result of one pass over a format string.
  TFormatParse = record
    Specs: TFormatSpecArray;
    // (highest consumed positional index + 1); 0 when the string has no specs.
    Required: integer;
    // False on the first grammar violation .
    Valid: boolean;
    // 1-based offset of the '%' starting the offending spec
    ErrSpecStart: integer;
    ErrOffset: integer;
  end;

  {  shared call-candidate collection (mirror of Casts' AllCasts/CollectCasts)  }

// Appends every call-shaped node in aExpr's tree to aList
procedure CollectCalls(aExpr: TPasExpr; var aList: TPasExprArray);
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
    CollectCalls(TParamsExpr(aExpr).Value, aList);
    for i := 0 to High(TParamsExpr(aExpr).Params) do
      CollectCalls(TParamsExpr(aExpr).Params[i], aList);
  end
  else if aExpr is TBinaryExpr then
  begin
    CollectCalls(TBinaryExpr(aExpr).Left, aList);
    CollectCalls(TBinaryExpr(aExpr).Right, aList);
  end
  else if aExpr is TUnaryExpr then
    CollectCalls(TUnaryExpr(aExpr).Operand, aList);
end;


// Flattens aRoot's statement subtree into aList (recurses ChildStatements).
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
function AllCalls(aModule: TPasModule): TPasExprArray;
var
  lRoots, lStmts: TPasImplElementArray;
  lWith: TPasImplWithDo;
  i, j: integer;

  procedure Take(aExpr: TPasExpr);
  begin
    CollectCalls(aExpr, Result);
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


// Emits one issue at aLine, column 1, using the caller-supplied message args and snippet
procedure EmitCall(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aLine: integer;
  const aArgs: array of string; const aSnippet: string);
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aLine, 1, aLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    aArgs, aSnippet);
end;


{  constructor-candidate collection (for ConstructorOnInstanceVariable)  }

// Appends every constructor-call-shaped node to aList
procedure CollectConstructorCandidates(aExpr: TPasExpr; var aList: TPasExprArray);
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
    CollectConstructorCandidates(TParamsExpr(aExpr).Value, aList);
    for i := 0 to High(TParamsExpr(aExpr).Params) do
      CollectConstructorCandidates(TParamsExpr(aExpr).Params[i], aList);
  end
  else if aExpr is TBinaryExpr then
  begin
    if TBinaryExpr(aExpr).OpCode = eopSubIdent then
    begin
      SetLength(aList, Length(aList) + 1);
      aList[High(aList)] := aExpr;
    end;
    CollectConstructorCandidates(TBinaryExpr(aExpr).Left, aList);
    CollectConstructorCandidates(TBinaryExpr(aExpr).Right, aList);
  end
  else if aExpr is TUnaryExpr then
    CollectConstructorCandidates(TUnaryExpr(aExpr).Operand, aList);
end;


// Every constructor-call-shaped node in aModule's statement expressions
function AllConstructorCandidates(aModule: TPasModule): TPasExprArray;
var
  lRoots, lStmts: TPasImplElementArray;
  lWith: TPasImplWithDo;
  i, j: integer;

  procedure Take(aExpr: TPasExpr);
  begin
    CollectConstructorCandidates(aExpr, Result);
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


{  indexed-access collection (for ExplicitDefaultArrayProperty)  }

// Appends every indexed-access node in aExpr's tree to aList — the pekArrayParams analogue of CollectCalls.
procedure CollectArrayAccesses(aExpr: TPasExpr; var aList: TPasExprArray);
var
  i: integer;
begin
  if aExpr = nil then
    Exit;
  if aExpr is TParamsExpr then
  begin
    if TParamsExpr(aExpr).Kind = pekArrayParams then
    begin
      SetLength(aList, Length(aList) + 1);
      aList[High(aList)] := aExpr;
    end;
    CollectArrayAccesses(TParamsExpr(aExpr).Value, aList);
    for i := 0 to High(TParamsExpr(aExpr).Params) do
      CollectArrayAccesses(TParamsExpr(aExpr).Params[i], aList);
  end
  else if aExpr is TBinaryExpr then
  begin
    CollectArrayAccesses(TBinaryExpr(aExpr).Left, aList);
    CollectArrayAccesses(TBinaryExpr(aExpr).Right, aList);
  end
  else if aExpr is TUnaryExpr then
    CollectArrayAccesses(TUnaryExpr(aExpr).Operand, aList);
end;


// Every pekArrayParams TParamsExpr in aModule's statement expressions
function AllArrayAccesses(aModule: TPasModule): TPasExprArray;
var
  lRoots, lStmts: TPasImplElementArray;
  lWith: TPasImplWithDo;
  i, j: integer;

  procedure Take(aExpr: TPasExpr);
  begin
    CollectArrayAccesses(aExpr, Result);
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


{  StringList Duplicates/Sorted scan helpers (for StringListDuplicatesNeedsSorted)  }

// True iff aType is TStringList or a descendant of it.
function IsStringListType(aType: TPasType): boolean;
var
  lCls: TPasClassType;
  lNext: TPasType;
  lGuard: integer;
begin
  Result := False;
  if not (aType is TPasClassType) then
    Exit;
  lCls := TPasClassType(aType);
  lGuard := 0;
  while (lCls <> nil) and (lGuard < 100) do
  begin
    if SameText(lCls.Name, 'TStringList') then
      Exit(True);
    lNext := lCls.AncestorType;
    if not (lNext is TPasClassType) then
      Exit;
    lCls := TPasClassType(lNext);
    Inc(lGuard);
  end;
end;


// True iff aAsg is '<recv>.Duplicates := dupIgnore/dupError' where recv resolves
// to a TStringList(-descendant) tied to a single declaration.
function IsDuplicatesAssign(const aResolver: TFpSonarResolver;
  aAsg: TPasImplAssign; out aRecvDecl: TPasElement;
  out aEnumName: string): boolean;
var
  lLeft: TBinaryExpr;
  lMember, lRhs: TPasElement;
  lTy: TFpSonarResolvedType;
begin
  Result := False;
  aRecvDecl := nil;
  aEnumName := '';
  if not ((aAsg.Left is TBinaryExpr)
    and (TBinaryExpr(aAsg.Left).OpCode = eopSubIdent)) then
    Exit;
  lLeft := TBinaryExpr(aAsg.Left);
  lMember := aResolver.ReferencedDecl(lLeft.Right);
  if not ((lMember is TPasProperty) and SameText(lMember.Name, 'Duplicates')) then
    Exit;
  // The receiver must resolve to TStringList or a descendant.
  if not aResolver.TryResolvedType(lLeft.Left, lTy) then
    Exit;
  if not IsStringListType(lTy.TypeEl) then
    Exit;
  // The receiver must be tied to a single declaration
  aRecvDecl := aResolver.ReferencedDecl(lLeft.Left);
  if aRecvDecl = nil then
    Exit;
  // The RHS must be a known dup* enum value other than the default dupAccept.
  lRhs := aResolver.ReferencedDecl(aAsg.Right);
  if not (lRhs is TPasEnumValue) then
    Exit;
  if not (SameText(lRhs.Name, 'dupIgnore') or SameText(lRhs.Name, 'dupError')) then
    Exit;
  aEnumName := lRhs.Name;
  Result := True;
end;


// True iff aStmts contains a '<recv>.Sorted := True' whose receiver is the same declaration as aRecvDecl
function HasSortedTrueOn(const aResolver: TFpSonarResolver;
  const aStmts: TPasImplElementArray; aRecvDecl: TPasElement): boolean;
var
  i: integer;
  lAsg: TPasImplAssign;
  lLeft: TBinaryExpr;
  lMember: TPasElement;
begin
  Result := False;
  for i := 0 to High(aStmts) do
  begin
    if not (aStmts[i] is TPasImplAssign) then
      Continue;
    lAsg := TPasImplAssign(aStmts[i]);
    if not ((lAsg.Left is TBinaryExpr)
      and (TBinaryExpr(lAsg.Left).OpCode = eopSubIdent)) then
      Continue;
    lLeft := TBinaryExpr(lAsg.Left);
    lMember := aResolver.ReferencedDecl(lLeft.Right);
    if not ((lMember is TPasProperty) and SameText(lMember.Name, 'Sorted')) then
      Continue;
    if aResolver.ReferencedDecl(lLeft.Left) <> aRecvDecl then
      Continue;
    if (lAsg.Right is TBoolConstExpr) and TBoolConstExpr(lAsg.Right).Value then
      Exit(True);
  end;
end;


{  the shared, resolver-free format-spec parser  }

{ Parses aFmt per the FPC Format grammar
   '%' [index ':'] ['-'] [width | '*'] ['.' (prec | '*')] type
  in ONE pass}
function ParseFormatSpecs(const aFmt: string): TFormatParse;
var
  lLen, i, j, lSpecStart, lConsumed, lCursor, lMaxIndex, lExplicit: integer;
  lCh: char;

  procedure Fail(aSpecStart, aOffset: integer);
  begin
    Result.Valid := False;
    Result.ErrSpecStart := aSpecStart;
    Result.ErrOffset := aOffset;
  end;

  // A '*' width/precision placeholder consumes the next sequential integer arg.
  procedure ConsumeStar;
  begin
    if lCursor > lMaxIndex then
      lMaxIndex := lCursor;
    Inc(lCursor);
  end;

  procedure AddSpec(aType: char; aIndex: integer);
  begin
    SetLength(Result.Specs, Length(Result.Specs) + 1);
    Result.Specs[High(Result.Specs)].TypeChar := aType;
    Result.Specs[High(Result.Specs)].ArgIndex := aIndex;
  end;

begin
  SetLength(Result.Specs, 0);
  Result.Required := 0;
  Result.Valid := True;
  Result.ErrSpecStart := 0;
  Result.ErrOffset := 0;
  lCursor := 0;
  lMaxIndex := -1;
  lLen := Length(aFmt);
  i := 1;
  while i <= lLen do
  begin
    if aFmt[i] <> '%' then
    begin
      Inc(i);
      Continue;
    end;
    lSpecStart := i;
    Inc(i);                              // consume '%'
    if i > lLen then
    begin
      Fail(lSpecStart, lSpecStart);    // a trailing, unterminated '%'
      Break;
    end;
    if aFmt[i] = '%' then                // '%%' literal escape
    begin
      Inc(i);
      Continue;
    end;
    lExplicit := -1;
    // Optional index: digits followed by ':'. Digits NOT followed by ':' are a
    // width and are re-read below (i is not advanced in that case).
    j := i;
    while (j <= lLen) and (aFmt[j] in ['0'..'9']) do
      Inc(j);
    if (j > i) and (j <= lLen) and (aFmt[j] = ':') then
    begin
      lExplicit := StrToIntDef(Copy(aFmt, i, j - i), -1);
      i := j + 1;                      // skip past ':'
    end
    else if (j = i) and (aFmt[i] = ':') then
    begin
      Fail(lSpecStart, i);             // ':' index marker without digits
      Break;
    end;
    // Optional left-justify flag.
    if (i <= lLen) and (aFmt[i] = '-') then
      Inc(i);
    // Optional width: digits OR '*'.
    if (i <= lLen) and (aFmt[i] = '*') then
    begin
      ConsumeStar;
      Inc(i);
    end
    else
      while (i <= lLen) and (aFmt[i] in ['0'..'9']) do
        Inc(i);
    // Optional '.' precision: digits OR '*'.
    if (i <= lLen) and (aFmt[i] = '.') then
    begin
      Inc(i);
      if (i <= lLen) and (aFmt[i] = '*') then
      begin
        ConsumeStar;
        Inc(i);
      end
      else
      begin
        j := i;
        while (i <= lLen) and (aFmt[i] in ['0'..'9']) do
          Inc(i);
        if i = j then
        begin
          Fail(lSpecStart, i);     // '.' with no precision number
          Break;
        end;
      end;
    end;
    // The conversion type letter.
    if i > lLen then
    begin
      Fail(lSpecStart, lLen);          // unterminated specifier
      Break;
    end;
    lCh := LowerCase(aFmt[i]);
    if not (lCh in cFormatTypeLetters) then
    begin
      Fail(lSpecStart, i);             // unknown/garbage conversion char
      Break;
    end;
    // The value specifier consumes a positional index (explicit re-targets).
    if lExplicit >= 0 then
    begin
      lConsumed := lExplicit;
      lCursor := lExplicit + 1;
    end
    else
    begin
      lConsumed := lCursor;
      Inc(lCursor);
    end;
    if lConsumed > lMaxIndex then
      lMaxIndex := lConsumed;
    AddSpec(lCh, lConsumed);
    Inc(i);
  end;
  if Result.Valid and (lMaxIndex >= 0) then
    Result.Required := lMaxIndex + 1;
end;


{  the conservative FormatArgumentType type-acceptance table  }

// True iff aKind is provable incompatible with conversion letter aSpec
function KindMismatches(aSpec: char; aKind: TFpSonarTypeKind): boolean;
begin
  case aSpec of
    'd', 'u', 'x':
      Result := aKind in [ltkFloat, ltkString, ltkChar];
    'e', 'f', 'g', 'n', 'm':
      Result := aKind in [ltkInteger, ltkString, ltkChar];
    's':
      Result := aKind in [ltkInteger, ltkFloat, ltkBool];
    'b':
      Result := aKind in [ltkString, ltkChar, ltkFloat];
    else
      // 'p' and 'o' (and any unexpected letter): never an unambiguous mismatch.
      Result := False;
  end;
end;


{ TRuleFormatArgumentType }

procedure TRuleFormatArgumentType.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCalls: TPasExprArray;
  lCall: TFpSonarFormatCall;
  lParse: TFormatParse;
  lArg: TFpSonarResolvedType;
  lIdx, i, k: integer;
begin
  lCalls := AllCalls(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCalls) do
  begin
    if not aContext.Resolver.TryFormatCall(lCalls[i], lCall) then
      Continue;
    // Need both a folded string AND inline args to type-check.
    if not (lCall.FmtResolved and lCall.ArgsInline) then
      Continue;
    lParse := ParseFormatSpecs(lCall.FmtText);
    // A malformed string defers to ValidFormatString.
    if not lParse.Valid then
      Continue;
    for k := 0 to High(lParse.Specs) do
    begin
      lIdx := lParse.Specs[k].ArgIndex;
      // Only an in-range argument can be type-checked (out-of-range is a
      // FormatArgumentCount concern).
      if (lIdx < 0) or (lIdx >= lCall.ArgCount) then
        Continue;
      // An argument that does not resolve degrades to silence.
      if not aContext.Resolver.TryResolvedType(lCall.Args[lIdx], lArg) then
        Continue;
      if KindMismatches(lParse.Specs[k].TypeChar, lArg.Kind) then
        EmitCall(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(lCall.Args[lIdx]),
          [lParse.Specs[k].TypeChar, lArg.TypeName], lArg.TypeName);
    end;
  end;
end;


{ TRuleFormatArgumentCount }

procedure TRuleFormatArgumentCount.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCalls: TPasExprArray;
  lCall: TFpSonarFormatCall;
  lParse: TFormatParse;
  i: integer;
begin
  lCalls := AllCalls(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCalls) do
  begin
    if not aContext.Resolver.TryFormatCall(lCalls[i], lCall) then
      Continue;
    if not (lCall.FmtResolved and lCall.ArgsInline) then
      Continue;
    lParse := ParseFormatSpecs(lCall.FmtText);
    // A malformed string defers to ValidFormatString.
    if not lParse.Valid then
      Continue;
    // Too-few only (flagSurplusArguments=false => surplus silent).
    if lCall.ArgCount < lParse.Required then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCall.FmtNode),
        [IntToStr(lParse.Required), IntToStr(lCall.ArgCount)],
        lCall.FmtText)
    else if flagSurplusArguments and (lCall.ArgCount > lParse.Required) then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCall.FmtNode),
        [IntToStr(lParse.Required), IntToStr(lCall.ArgCount)],
        lCall.FmtText);
  end;
end;


{ TRuleValidFormatString }

procedure TRuleValidFormatString.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCalls: TPasExprArray;
  lCall: TFpSonarFormatCall;
  lParse: TFormatParse;
  lConv: string;
  i: integer;
begin
  lCalls := AllCalls(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCalls) do
  begin
    if not aContext.Resolver.TryFormatCall(lCalls[i], lCall) then
      Continue;
    // Never validate a string we cannot see in full.
    if not lCall.FmtResolved then
      Continue;
    lParse := ParseFormatSpecs(lCall.FmtText);
    if lParse.Valid then
      Continue;
    // The offending conversion text (from its '%' to the bad char inclusive).
    lConv := Copy(lCall.FmtText, lParse.ErrSpecStart,
      lParse.ErrOffset - lParse.ErrSpecStart + 1);
    EmitCall(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lCall.FmtNode), [lConv], lConv);
  end;
end;


{ TRuleFreeAndNilArgument }

procedure TRuleFreeAndNilArgument.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCalls: TPasExprArray;
  lInner: TPasExpr;
  lArg: TFpSonarResolvedType;
  i: integer;
begin
  lCalls := AllCalls(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCalls) do
  begin
    // Act only on the FreeAndNil(X) shape (lfkFreeMethod / lfkNone are not ours).
    if aContext.Resolver.TryFreeCall(lCalls[i], lInner) <> lfkFreeAndNil then
      Continue;
    // An argument that does not resolve degrades to silence.
    if not aContext.Resolver.TryResolvedType(lInner, lArg) then
      Continue;
    // Accept a class instance and degrade on variant/untyped (ltkOther); flag everything else that resolves
    if not (lArg.Kind in [ltkClass, ltkOther]) then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCalls[i]), [lArg.TypeName], lArg.TypeName);
  end;
end;


{ TRuleConstructorOnInstanceVariable }

procedure TRuleConstructorOnInstanceVariable.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCands: TPasExprArray;
  lOnInstance: boolean;
  lCtorName: string;
  i: integer;
begin
  lCands := AllConstructorCandidates(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCands) do
    if aContext.Resolver.TryConstructorCall(lCands[i], lOnInstance, lCtorName)
      and lOnInstance then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCands[i]), [lCtorName], lCtorName);
end;


{ TRuleStringListDuplicatesNeedsSorted }

procedure TRuleStringListDuplicatesNeedsSorted.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoots, lStmts: TPasImplElementArray;
  lRecvDecl: TPasElement;
  lEnumName: string;
  r, i: integer;
begin
  lRoots := EnumerateStatementRoots(aContext.Resolver.ResolvedModule);
  for r := 0 to High(lRoots) do
  begin
    SetLength(lStmts, 0);
    CollectStatements(lRoots[r], lStmts);
    for i := 0 to High(lStmts) do
      if (lStmts[i] is TPasImplAssign)
        and IsDuplicatesAssign(aContext.Resolver, TPasImplAssign(lStmts[i]),
        lRecvDecl, lEnumName)
        and not HasSortedTrueOn(aContext.Resolver, lStmts, lRecvDecl) then
        EmitCall(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(TPasImplAssign(lStmts[i]).Left),
          [lEnumName], lEnumName);
  end;
end;


{  declaration / inherited-forward collection
       (for DestructorShouldOverrideDestroy / OverrideOnlyCallsInherited)  }

type
  // The destructor declarations gathered for DestructorShouldOverrideDestroy
  TPasDestructorArray = array of TPasDestructor;

// Every destructor DECLARATION in aModule's classes
function AllDestructorDecls(aModule: TPasModule): TPasDestructorArray;
var
  lTypes: TPasTypeArray;
  lCls: TPasClassType;
  i, j: integer;
begin
  SetLength(Result, 0);
  if aModule = nil then
    Exit;
  lTypes := EnumerateTypes(aModule);
  for i := 0 to High(lTypes) do
    if lTypes[i] is TPasClassType then
    begin
      lCls := TPasClassType(lTypes[i]);
      if lCls.Members <> nil then
        for j := 0 to lCls.Members.Count - 1 do
          if TObject(lCls.Members[j]) is TPasDestructor then
          begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := TPasDestructor(lCls.Members[j]);
          end;
    end;
end;


// True iff aExpr is an 'inherited ...' expression: a bare TInheritedExpr
function IsInheritedShape(aExpr: TPasExpr): boolean;
begin
  Result := (aExpr is TInheritedExpr)
    or ((aExpr is TBinaryExpr) and (TBinaryExpr(aExpr).OpCode = eopNone)
    and (TBinaryExpr(aExpr).Left is TInheritedExpr));
end;


// The inherited-forward expression of aBlock's single effective statement
function InheritedForwardExpr(aBlock: TPasImplBlock;
  out aAssignLeft: TPasExpr): TPasExpr;
var
  lStmts: TPasImplElementArray;
  lEff: TPasImplElement;
  lCount, i: integer;
begin
  Result := nil;
  aAssignLeft := nil;
  if aBlock = nil then
    Exit;
  lStmts := ChildStatements(aBlock);
  lEff := nil;
  lCount := 0;
  for i := 0 to High(lStmts) do
  begin
    // An empty statement (a stray ';') is not effective.
    if (lStmts[i] is TPasImplSimple)
      and (TPasImplSimple(lStmts[i]).Expr = nil) then
      Continue;
    Inc(lCount);
    lEff := lStmts[i];
  end;
  if lCount <> 1 then
    Exit;
  if lEff is TPasImplSimple then
  begin
    if IsInheritedShape(TPasImplSimple(lEff).Expr) then
      Result := TPasImplSimple(lEff).Expr;
  end
  else if lEff is TPasImplAssign then
    if IsInheritedShape(TPasImplAssign(lEff).Right) then
    begin
      Result := TPasImplAssign(lEff).Right;
      aAssignLeft := TPasImplAssign(lEff).Left;
    end;
end;


{ TRuleDestructorShouldOverrideDestroy }

procedure TRuleDestructorShouldOverrideDestroy.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lDtors: TPasDestructorArray;
  lDtor: TPasDestructor;
  lDescendsTObject, lHasInheritedVirtualDestroy: boolean;
  i: integer;
begin
  lDtors := AllDestructorDecls(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lDtors) do
  begin
    lDtor := lDtors[i];
    // Degrade to silence when the proc scope / ancestry is not fully resolvable.
    if not aContext.Resolver.TryDestructorContract(lDtor,
      lDescendsTObject, lHasInheritedVirtualDestroy) then
      Continue;
    if not lDescendsTObject then
      Continue;
    // Case (a): a TObject-descendant destructor not named Destroy cannot be
    // reached by Free / polymorphic destruction.
    if not SameText(lDtor.Name, 'Destroy') then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lDtor), [lDtor.Name], lDtor.Name)
    // Case (b): a Destroy with an overridable inherited Destroy above it but missing the override modifier.
    else if lHasInheritedVirtualDestroy
      and not (pmOverride in lDtor.Modifiers) then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lDtor), ['Destroy'], 'Destroy');
  end;
end;


{ TRuleOverrideOnlyCallsInherited }

procedure TRuleOverrideOnlyCallsInherited.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  lInh, lAssignLeft: TPasExpr;
  i: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lRoutines) do
  begin
    lInh := InheritedForwardExpr(lRoutines[i].Block, lAssignLeft);
    if lInh = nil then
      Continue;
    // Function form: the assignment LHS must be the routine's Result element
    if (lAssignLeft <> nil)
      and not (aContext.Resolver.ReferencedDecl(lAssignLeft) is TPasResultElement) then
      Continue;
    // The query confirms the override modifier, the same-signature ancestor
    if aContext.Resolver.TryOverrideOnlyForwards(lRoutines[i].Decl, lInh) then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lRoutines[i].Decl),
        [lRoutines[i].Decl.Name], lRoutines[i].Decl.Name);
  end;
end;


{ TRuleIfThenNotShortCircuit }

procedure TRuleIfThenNotShortCircuit.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCalls: TPasExprArray;
  lSubject: string;
  i: integer;
begin
  lCalls := AllCalls(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCalls) do
    if aContext.Resolver.TryIfThenMisuse(lCalls[i], lSubject) then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCalls[i]), [lSubject], lSubject);
end;


{ TRuleAssertWithoutMessage }

procedure TRuleAssertWithoutMessage.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCalls: TPasExprArray;
  lArgCount: integer;
  i: integer;
begin
  lCalls := AllCalls(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCalls) do
    // The query confirms a RESOLVED call to the compiler built-in Assert
    if aContext.Resolver.TryAssertCall(lCalls[i], lArgCount)
      and (lArgCount = 1) then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCalls[i]), [], '');
end;


{ TRuleDefaultFormatSettingsInDateFormat }

procedure TRuleDefaultFormatSettingsInDateFormat.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lCalls: TPasExprArray;
  lRoutineName: string;
  i: integer;
begin
  lCalls := AllCalls(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCalls) do
    // The query confirms a SysUtils locale-sensitive date/time call
    if aContext.Resolver.TryDefaultFormatSettingsDateCall(lCalls[i], lRoutineName) then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCalls[i]), [lRoutineName], lRoutineName);
end;


{ TRuleExplicitDefaultArrayProperty }

procedure TRuleExplicitDefaultArrayProperty.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAccesses: TPasExprArray;
  lPropName: string;
  i: integer;
begin
  lAccesses := AllArrayAccesses(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lAccesses) do
    // The query confirms an explicit Obj.Items[i] access to a class DEFAULT array propert
    if aContext.Resolver.TryExplicitDefaultArrayProperty(lAccesses[i], lPropName) then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lAccesses[i]), [lPropName], lPropName);
end;


{ TRuleStringFirstCharByIndex }

procedure TRuleStringFirstCharByIndex.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAccesses: TPasExprArray;
  i: integer;
begin
  lAccesses := AllArrayAccesses(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lAccesses) do
    // The query confirms a string operand indexed at the low bound
    if aContext.Resolver.TryStringFirstCharByIndex(lAccesses[i]) then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lAccesses[i]), [], '');
end;


{ TRuleTListLastByIndex }

procedure TRuleTListLastByIndex.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lAccesses: TPasExprArray;
  i: integer;
begin
  lAccesses := AllArrayAccesses(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lAccesses) do
    // The query confirms a TList last-element fetch written as
    // L[L.Count-1] (same-receiver Count-1 index) for which L.Last is the clearer form.
    if aContext.Resolver.TryListLastByIndex(lAccesses[i]) then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lAccesses[i]), [], '');
end;


{ TRuleRedundantInherited }

procedure TRuleRedundantInherited.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  lStmts: TPasImplElementArray;
  lExpr: TPasExpr;
  i, j: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lRoutines) do
  begin
    // The query confirms that this method has no overridable same-signature
    // ancestor and that its class ancestry fully resolves
    if not aContext.Resolver.TryRedundantInherited(lRoutines[i].Decl) then
      Continue;
    SetLength(lStmts, 0);
    CollectStatements(lRoutines[i].Block, lStmts);
    for j := 0 to High(lStmts) do
      if lStmts[j] is TPasImplSimple then
      begin
        lExpr := TPasImplSimple(lStmts[j]).Expr;
        // Only the BARE 'inherited;' form. the named 'inherited Name(...)' form is Case B and not flagged.
        if lExpr is TInheritedExpr then
          EmitCall(FMetadata, aContext, aCollector,
            aContext.Resolver.SourceRow(lExpr), [], '');
      end;
  end;
end;


{ TRuleImplicitTEncodingDefault }

procedure TRuleImplicitTEncodingDefault.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCalls: TPasExprArray;
  i: integer;
begin
  lCalls := AllCalls(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCalls) do
    // The query confirms a call bound to a curated stdlib byte/string-conversion overload
    if aContext.Resolver.TryImplicitTEncodingDefault(lCalls[i]) then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCalls[i]), [], '');
end;


{ TRuleSingleOverloadOfMathFunction }

procedure TRuleSingleOverloadOfMathFunction.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lCalls: TPasExprArray;
  i: integer;
begin
  lCalls := AllCalls(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lCalls) do
    // The query confirms a call bound to a curated standard math routine
    if aContext.Resolver.TrySingleOverloadOfMathFunction(lCalls[i]) then
      EmitCall(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lCalls[i]), [], '');
end;


initialization
  // The Format trio
  RegisterRule(TRuleFormatArgumentType.Create(TRuleMetadata.Make(
    'FormatArgumentType', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyFormatArgumentType).WithDescription(
    'Flags a Format() argument whose type does not match its conversion '
    + 'specifier (for example a string passed for %d).')));
  RegisterMessage(cKeyFormatArgumentType, SFormatArgumentType);

  RegisterRule(TRuleFormatArgumentCount.Create(TRuleMetadata.Make(
    'FormatArgumentCount', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyFormatArgumentCount).WithDescription(
    'Flags a Format() call whose argument count does not match the number of '
    + 'conversion specifiers in the format string.')));
  RegisterMessage(cKeyFormatArgumentCount, SFormatArgumentCount);

  RegisterRule(TRuleValidFormatString.Create(TRuleMetadata.Make(
    'ValidFormatString', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyValidFormatString).WithDescription(
    'Flags an invalid or malformed conversion specifier in a Format() format '
    + 'string.')));
  RegisterMessage(cKeyValidFormatString, SValidFormatString);

  // the call/constructor rules
  RegisterRule(TRuleFreeAndNilArgument.Create(TRuleMetadata.Make(
    'FreeAndNilArgument', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyFreeAndNilArgument).WithDescription(
    'Flags FreeAndNil applied to a non-class value; only class instances may be '
    + 'passed.')));
  RegisterMessage(cKeyFreeAndNilArgument, SFreeAndNilArgument);

  RegisterRule(TRuleConstructorOnInstanceVariable.Create(TRuleMetadata.Make(
    'ConstructorOnInstanceVariable', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyConstructorOnInstanceVariable).WithDescription(
    'Flags a constructor called on an already-allocated instance, which '
    + 're-initialises it in place instead of creating a new object.')));
  RegisterMessage(cKeyConstructorOnInstanceVariable, SConstructorOnInstanceVariable);

  RegisterRule(TRuleStringListDuplicatesNeedsSorted.Create(TRuleMetadata.Make(
    'StringListDuplicatesNeedsSorted', rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, True, cKeyStringListDuplicatesNeedsSorted).WithDescription(
    'Flags a TStringList whose Duplicates property is set but which is never '
    + 'sorted, so the setting has no effect.')));
  RegisterMessage(cKeyStringListDuplicatesNeedsSorted, SStringListDuplicatesNeedsSorted);

  // the override/control rules
  RegisterRule(TRuleDestructorShouldOverrideDestroy.Create(TRuleMetadata.Make(
    'DestructorShouldOverrideDestroy', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyDestructorShouldOverrideDestroy).WithDescription(
    'Flags a destructor not named Destroy and marked override, so it will not '
    + 'participate in polymorphic destruction.')));
  RegisterMessage(cKeyDestructorShouldOverrideDestroy, SDestructorShouldOverrideDestroy);

  RegisterRule(TRuleOverrideOnlyCallsInherited.Create(TRuleMetadata.Make(
    'OverrideOnlyCallsInherited', rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, True, cKeyOverrideOnlyCallsInherited).WithDescription(
    'Flags an override method that only forwards to the inherited method and '
    + 'can therefore be removed.')));
  RegisterMessage(cKeyOverrideOnlyCallsInherited, SOverrideOnlyCallsInherited);

  RegisterRule(TRuleIfThenNotShortCircuit.Create(TRuleMetadata.Make(
    'IfThenNotShortCircuit', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyIfThenNotShortCircuit).WithDescription(
    'Flags IfThen usage where both value arguments are always evaluated, so a '
    + 'condition-guarded expression may be used unsafely.')));
  RegisterMessage(cKeyIfThenNotShortCircuit, SIfThenNotShortCircuit);

  // the call/builtin rules
  RegisterRule(TRuleAssertWithoutMessage.Create(TRuleMetadata.Make(
    'AssertWithoutMessage', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyAssertWithoutMessage).WithDescription(
    'Flags an Assert with no message, so the raised EAssertionFailed carries no '
    + 'diagnostic text.')));
  RegisterMessage(cKeyAssertWithoutMessage, SAssertWithoutMessage);

  RegisterRule(TRuleDefaultFormatSettingsInDateFormat.Create(TRuleMetadata.Make(
    'DefaultFormatSettingsInDateFormat', rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, True, cKeyDefaultFormatSettingsInDateFormat).WithDescription(
    'Flags date/number formatting that relies on the global '
    + 'DefaultFormatSettings; pass an explicit TFormatSettings for '
    + 'locale-independent output.')));
  RegisterMessage(cKeyDefaultFormatSettingsInDateFormat,
    SDefaultFormatSettingsInDateFormat);

  RegisterRule(TRuleExplicitDefaultArrayProperty.Create(TRuleMetadata.Make(
    'ExplicitDefaultArrayProperty', rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, True, cKeyExplicitDefaultArrayProperty).WithDescription(
    'Flags a default array property accessed by name where the [] shorthand is '
    + 'equivalent and clearer.')));
  RegisterMessage(cKeyExplicitDefaultArrayProperty, SExplicitDefaultArrayProperty);

  // the index-access rules
  RegisterRule(TRuleStringFirstCharByIndex.Create(TRuleMetadata.Make(
    'StringFirstCharByIndex', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyStringFirstCharByIndex).WithDescription(
    'Flags reading a string first character by index where a clearer '
    + 'intent-revealing form is preferable.')));
  RegisterMessage(cKeyStringFirstCharByIndex, SStringFirstCharByIndex);

  RegisterRule(TRuleTListLastByIndex.Create(TRuleMetadata.Make(
    'TListLastByIndex', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyTListLastByIndex).WithDescription(
    'Flags fetching a list last element by index instead of using the '
    + 'dedicated Last accessor.')));
  RegisterMessage(cKeyTListLastByIndex, STListLastByIndex);

  // the redundant-'inherited' rule
  RegisterRule(TRuleRedundantInherited.Create(TRuleMetadata.Make(
    'RedundantInherited', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyRedundantInherited).WithDescription(
    'Flags an inherited statement that binds to no overridable parent method '
    + 'and can be removed.')));
  RegisterMessage(cKeyRedundantInherited, SRedundantInherited);

  // the implicit-TEncoding.Default rule
  RegisterRule(TRuleImplicitTEncodingDefault.Create(TRuleMetadata.Make(
    'ImplicitTEncodingDefault', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyImplicitTEncodingDefault).WithDescription(
    'Flags an overload that implicitly uses the platform-dependent '
    + 'TEncoding.Default; specify an explicit encoding.')));
  RegisterMessage(cKeyImplicitTEncodingDefault, SImplicitTEncodingDefault);

  // the single-precision math-overload rule
  RegisterRule(TRuleSingleOverloadOfMathFunction.Create(TRuleMetadata.Make(
    'SingleOverloadOfMathFunction', rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, True, cKeySingleOverloadOfMathFunction).WithDescription(
    'Flags a math routine bound to its Single-precision overload when a '
    + 'higher-precision Double/Extended overload is available.')));
  RegisterMessage(cKeySingleOverloadOfMathFunction, SSingleOverloadOfMathFunction);

end.
