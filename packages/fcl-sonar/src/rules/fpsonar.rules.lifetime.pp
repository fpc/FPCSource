{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Object-lifetime analysis rules, resolver tier

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Lifetime;


{$mode objfpc}{$H+}

interface

uses
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework;

type
  { Flags a Free/FreeAndNil disposal whose operand is an interface-typed
    reference (positive: the disposal is the defect). }
  TRuleFreeOnInterfaceReference = class(TRuleBase)
  public
    // Emits one issue per disposal of an interface-typed reference.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a method other than a destructor that frees its own Self
    (positive: the disposal is the defect). }
  TRuleSelfDestroyedInMethod = class(TRuleBase)
  public
    // Emits one issue per self-disposal in a non-destructor method body.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a pointer released with a deallocator that does not match its
    allocator (positive: the release is the defect). }
  TRuleNewDisposeMismatch = class(TRuleBase)
  public
    // Emits one issue per mismatched release of a declaration allocated in the same routine.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a field constructed in a constructor whose class destructor never
    mentions it (absence: the missing release is the defect). }
  TRuleOwnedFieldNotFreedInDestructor = class(TRuleBase)
  public
    // Emits one issue per owned field the owning class destructor never mentions.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a local created and released somewhere in its routine with no
    try..finally covering the construction (absence: the missing protection is
    the defect). }
  TRuleCreateWithoutTryFinally = class(TRuleBase)
  public
    // Emits one issue per unprotected construction the routine releases.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an exit path that leaves a constructed local unreleased (absence: the
    missing release on that path is the defect). }
  TRuleLeakOnEarlyExit = class(TRuleBase)
  public
    // Emits one issue per exit or raise reached while a local is still owned.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a stream constructed into a local with no try..finally covering it
    (absence: the missing protection is the defect). }
  TRuleStreamNotProtected = class(TRuleBase)
  public
    // Emits one issue per unprotected stream construction.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a Free/FreeAndNil applied to the exception variable of the handler
    that caught it (positive: the disposal is the defect). }
  TRuleExceptionObjectFreedInHandler = class(TRuleBase)
  public
    // Emits one issue per handler that disposes of its own exception variable.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an exception instance referenced on a row after the raise that
    transferred its ownership (positive: the later reference is the defect). }
  TRuleRaisedExceptionInstanceReused = class(TRuleBase)
  public
    // Emits one issue per raise whose operand is referenced on a later row.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a read of a for-loop control variable after its loop, whose value the
    language leaves undefined (positive: the read is the defect). }
  TRuleLoopVariableUsedAfterLoop = class(TRuleBase)
  public
    // Emits one issue per loop whose control variable is read after it ends.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an assignment to a for-loop control variable inside its own body
    (positive: the assignment is the defect). }
  TRuleLoopVariableModifiedInBody = class(TRuleBase)
  public
    // Emits one issue per assignment to an enclosing loop's control variable.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a read of a local reference on a path that already released it
    (positive: the read is the defect). }
  TRuleUseAfterFree = class(TRuleBase)
  public
    // Emits one issue per read of a routine local that is dangling on that path.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a release of a reference that a path reaching it already released
    (positive: the second release is the defect). }
  TRuleDoubleFree = class(TRuleBase)
  public
    // Emits one issue per release of a reference that is already dangling.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a field released with Free rather than FreeAndNil and read afterwards
    (positive: the unnilled release is the defect). }
  TRuleFreeNotFreeAndNilOnField = class(TRuleBase)
  public
    // Emits one issue per field release whose dangling value a later read sees.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a heap allocation into a local that no path of the routine releases
    (absence: the missing release is the defect). }
  TRuleGetMemWithoutFreeMem = class(TRuleBase)
  public
    // Emits one issue per allocation the routine leaves owned at its exit.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a construction in a loop body that the loop does not release (absence:
    the missing release inside the loop is the defect). }
  TRuleObjectCreatedInLoopNotFreed = class(TRuleBase)
  public
    // Emits one issue per loop-body construction the previous iteration still owned.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an acquire whose matching release the routine makes outside a
    covering finally (positive: the unprotected pairing is the defect). }
  TRuleUnbalancedPair = class(TRuleBase)
  public
    // Emits one issue per acquire the routine releases outside a finally.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, Pascal.Tree,
{$ELSE}
  SysUtils, PasTree,
{$ENDIF}
  FpSonar.Config, FpSonar.Traversal, FpSonar.Resolver, FpSonar.DataFlow,
  FpSonar.Rules.Consts;

const
  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyFreeOnInterfaceReference = 'rule.FreeOnInterfaceReference.message';
  cKeySelfDestroyedInMethod = 'rule.SelfDestroyedInMethod.message';
  cKeyNewDisposeMismatch = 'rule.NewDisposeMismatch.message';
  cKeyOwnedFieldNotFreedInDestructor =
    'rule.OwnedFieldNotFreedInDestructor.message';
  cKeyCreateWithoutTryFinally = 'rule.CreateWithoutTryFinally.message';
  cKeyExceptionObjectFreedInHandler =
    'rule.ExceptionObjectFreedInHandler.message';
  cKeyRaisedExceptionInstanceReused =
    'rule.RaisedExceptionInstanceReused.message';
  cKeyLoopVariableUsedAfterLoop = 'rule.LoopVariableUsedAfterLoop.message';
  cKeyLoopVariableModifiedInBody = 'rule.LoopVariableModifiedInBody.message';
  cKeyLeakOnEarlyExit = 'rule.LeakOnEarlyExit.message';
  cKeyStreamNotProtected = 'rule.StreamNotProtected.message';
  cKeyUseAfterFree = 'rule.UseAfterFree.message';
  cKeyDoubleFree = 'rule.DoubleFree.message';
  cKeyFreeNotFreeAndNilOnField = 'rule.FreeNotFreeAndNilOnField.message';
  cKeyGetMemWithoutFreeMem = 'rule.GetMemWithoutFreeMem.message';
  cKeyObjectCreatedInLoopNotFreed =
    'rule.ObjectCreatedInLoopNotFreed.message';
  cKeyUnbalancedPair = 'rule.UnbalancedPair.message';

  // Parent-walk bound, as elsewhere in the tree.
  cMaxParentDepth = 200;

  // The configured acquire/release table of TRuleUnbalancedPair.
  cTargetsKey = 'targets';

  // The vocabulary TRuleUnbalancedPair pairs on when none is configured.
  cDefaultPairs: array[0..4] of string = ('Acquire/Release', 'Enter/Leave',
    'BeginUpdate/EndUpdate', 'Lock/Unlock', 'BeginWrite/EndWrite');

type
  { One declaration's allocation and releases inside a single routine body. }
  TMemoryOperand = record
    Decl: TPasElement;
    Name: string;
    Allocator: TFpSonarMemoryOp;
    // Allocation runs seen; anything but 1 makes the pairing ambiguous.
    Allocators: integer;
    Releases: array of TFpSonarMemoryOp;
    ReleaseRows: array of integer;
  end;
  TMemoryOperandArray = array of TMemoryOperand;

  { One field a constructor assigns a freshly constructed instance to. }
  TOwnedField = record
    Decl: TPasElement;
    Owner: TPasClassType;
  end;
  TOwnedFieldArray = array of TOwnedField;


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


// Emits one issue per resource finding of aVerdict, at its site's row.
procedure EmitResourceVerdict(const aMeta: TRuleMetadata;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector;
  aVerdict: TFpSonarResourceVerdict);
var
  lFlow: TFpSonarDataFlow;
  lFindings: TFpSonarResourceFindingArray;
  lOk: boolean;
  i: integer;
begin
  lFlow := TFpSonarDataFlow.Create(aContext.Resolver);
  try
    lOk := lFlow.TryResourceFindings(lFindings);
  finally
    lFlow.Free;
  end;
  if not lOk then
    Exit;
  for i := 0 to High(lFindings) do
    if lFindings[i].Verdict = aVerdict then
      EmitStmt(aMeta, aContext, aCollector,
        aContext.Resolver.SourceRow(lFindings[i].Site), [lFindings[i].Name],
        lFindings[i].Name);
end;


// Emits one issue per free-state finding of aVerdict, at its site's row.
procedure EmitFreeStateVerdict(const aMeta: TRuleMetadata;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector;
  aVerdict: TFpSonarFreeStateVerdict);
var
  lFlow: TFpSonarDataFlow;
  lFindings: TFpSonarFreeStateFindingArray;
  lOk: boolean;
  i: integer;
begin
  lFlow := TFpSonarDataFlow.Create(aContext.Resolver);
  try
    lOk := lFlow.TryFreeStateFindings(lFindings);
  finally
    lFlow.Free;
  end;
  if not lOk then
    Exit;
  for i := 0 to High(lFindings) do
    if lFindings[i].Verdict = aVerdict then
      EmitStmt(aMeta, aContext, aCollector,
        aContext.Resolver.SourceRow(lFindings[i].Site), [lFindings[i].Name],
        lFindings[i].Name);
end;


// The acquire/release pair aText spells, False when it spells no pair.
function SplitPair(const aText: string; out aPair: TFpSonarPairSpec): boolean;

var
  i: integer;

begin
  Result := False;
  i := Pos('/', aText);
  if i < 1 then
    Exit;
  aPair.Acquire := Trim(Copy(aText, 1, i - 1));
  aPair.Release := Trim(Copy(aText, i + 1, Length(aText) - i));
  Result := (aPair.Acquire <> '') and (aPair.Release <> '');
end;


// The pairs configured for aRuleId, or the built-in five when none are.
function ConfiguredPairs(const aContext: TRuleContext;
  const aRuleId: string): TFpSonarPairSpecArray;

var
  lTargets: TFpSonarRuleTargetArray;
  lPair: TFpSonarPairSpec;
  i: integer;

begin
  SetLength(Result, 0);
  lTargets := aContext.Config.RuleParamTargets(aRuleId, cTargetsKey);
  if Length(lTargets) = 0 then
  begin
    SetLength(Result, Length(cDefaultPairs));
    for i := Low(cDefaultPairs) to High(cDefaultPairs) do
      SplitPair(cDefaultPairs[i], Result[i]);
    Exit;
  end;
  for i := 0 to High(lTargets) do
    if SplitPair(lTargets[i].Pattern, lPair) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := lPair;
    end;
end;


// Emits one issue per unprotected pairing of aPairs, at the acquire row.
procedure EmitPairVerdicts(const aMeta: TRuleMetadata;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector;
  const aPairs: TFpSonarPairSpecArray);

var
  lFlow: TFpSonarDataFlow;
  lFindings: TFpSonarPairFindingArray;
  lOk: boolean;
  i: integer;

begin
  if Length(aPairs) = 0 then
    Exit;
  lFlow := TFpSonarDataFlow.Create(aContext.Resolver);
  try
    lOk := lFlow.TryPairFindings(aPairs, lFindings);
  finally
    lFlow.Free;
  end;
  if not lOk then
    Exit;
  for i := 0 to High(lFindings) do
    EmitStmt(aMeta, aContext, aCollector,
      aContext.Resolver.SourceRow(lFindings[i].Site),
      [lFindings[i].Name, lFindings[i].Release], lFindings[i].Name);
end;


// The expression of a simple statement, nil for any other statement kind.
function SimpleExpr(aStmt: TPasImplElement): TPasExpr;
begin
  if aStmt is TPasImplSimple then
    Result := TPasImplSimple(aStmt).Expr
  else
    Result := nil;
end;


// The source spelling of a heap-management routine.
function OpWord(aOp: TFpSonarMemoryOp): string;
begin
  case aOp of
    lmoNew: Result := 'New';
    lmoGetMem: Result := 'GetMem';
    lmoDispose: Result := 'Dispose';
    else
      Result := 'FreeMem';
  end;
end;


// True when aRelease is the wrong deallocator for aAlloc.
function Mismatches(aAlloc, aRelease: TFpSonarMemoryOp): boolean;
begin
  Result := ((aAlloc = lmoNew) and (aRelease = lmoFreeMem))
    or ((aAlloc = lmoGetMem) and (aRelease = lmoDispose));
end;


// The class-qualifier of an implementation routine name ('' when unqualified).
function ClassQualifier(const aName: string): string;
var
  i: integer;
begin
  Result := '';
  for i := Length(aName) downto 1 do
    if aName[i] = '.' then
      Exit(Copy(aName, 1, i - 1));
end;


// The identifier an assignment's left side names, unwrapping a Self./Obj. prefix.
function TargetIdent(aExpr: TPasExpr): TPasExpr;
begin
  Result := aExpr;
  if (Result is TBinaryExpr) and (TBinaryExpr(Result).OpCode = eopSubIdent) then
    Result := TBinaryExpr(Result).Right;
end;


// True when aNode is aRoot or lies below it within the parent-walk bound.
function UnderElement(aNode, aRoot: TPasElement): boolean;
var
  lWalk: TPasElement;
  i: integer;
begin
  Result := False;
  if (aNode = nil) or (aRoot = nil) then
    Exit;
  lWalk := aNode;
  i := 0;
  while (lWalk <> nil) and (i < cMaxParentDepth) do
  begin
    if lWalk = aRoot then
      Exit(True);
    lWalk := lWalk.Parent;
    Inc(i);
  end;
end;


// True when aClass declares a method named aName.
function HasMethod(aClass: TPasClassType; const aName: string): boolean;
var
  i: integer;
begin
  Result := False;
  if (aClass = nil) or (aClass.Members = nil) then
    Exit;
  for i := 0 to aClass.Members.Count - 1 do
    if (TObject(aClass.Members[i]) is TPasProcedure)
      and SameText(TPasElement(aClass.Members[i]).Name, aName) then
      Exit(True);
end;


// True when aNode is, or lies under, the left-hand side of an assignment.
function UnderAssignTarget(aNode: TPasElement): boolean;
var
  lWalk: TPasElement;
  i: integer;
begin
  lWalk := aNode;
  i := 0;
  while (lWalk <> nil) and (i < cMaxParentDepth) do
  begin
    if (lWalk.Parent is TPasImplAssign)
      and (TPasImplAssign(lWalk.Parent).Left = lWalk) then
      Exit(True);
    lWalk := lWalk.Parent;
    Inc(i);
  end;
  Result := lWalk <> nil;
end;


// The identifier a bare identifier expression names, '' for anything else.
function BareIdent(aExpr: TPasExpr): string;
begin
  if (aExpr is TPrimitiveExpr) and (TPrimitiveExpr(aExpr).Kind = pekIdent) then
    Result := TPrimitiveExpr(aExpr).Value
  else
    Result := '';
end;


{ TRuleFreeOnInterfaceReference }

procedure TRuleFreeOnInterfaceReference.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lInner: TPasExpr;
  lType, lTarget, lSource: TFpSonarResolvedType;
  lName: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
  begin
    if aContext.Resolver.TryFreeCall(SimpleExpr(lStmts[i]), lInner) = lfkNone then
      Continue;
    lName := '';
    if aContext.Resolver.TryResolvedType(lInner, lType)
      and (lType.Kind = ltkInterface) then
      lName := lType.NamedTypeName
    // A cast off an interface reference: the defect is the source type.
    else if aContext.Resolver.TryTypecast(lInner, lTarget, lSource)
      and (lSource.Kind = ltkInterface) then
      lName := lSource.NamedTypeName;
    if lName <> '' then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lStmts[i]), [lName], lName);
  end;
end;


{ TRuleSelfDestroyedInMethod }

procedure TRuleSelfDestroyedInMethod.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  lStmts: TPasImplElementArray;
  i, j: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lRoutines) do
  begin
    // A destructor is where an object is meant to be taken apart.
    if lRoutines[i].Decl is TPasDestructor then
      Continue;
    SetLength(lStmts, 0);
    CollectStatements(lRoutines[i].Block, lStmts);
    for j := 0 to High(lStmts) do
      if aContext.Resolver.TryDisposesSelf(lStmts[j]) then
        EmitStmt(FMetadata, aContext, aCollector,
          aContext.Resolver.SourceRow(lStmts[j]), [lRoutines[i].Decl.Name],
          lRoutines[i].Decl.Name);
  end;
end;


{ TRuleNewDisposeMismatch }

procedure TRuleNewDisposeMismatch.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

  // The entry for aDecl in aOps, appended when this is its first call site.
  function EntryOf(var aOps: TMemoryOperandArray; aDecl: TPasElement;
    const aName: string): integer;
  var
    i: integer;
  begin
    for i := 0 to High(aOps) do
      if aOps[i].Decl = aDecl then
        Exit(i);
    SetLength(aOps, Length(aOps) + 1);
    Result := High(aOps);
    aOps[Result].Decl := aDecl;
    aOps[Result].Name := aName;
    aOps[Result].Allocators := 0;
  end;

var
  lRoutines: TAstRoutineArray;
  lStmts: TPasImplElementArray;
  lOps: TMemoryOperandArray;
  lOp: TFpSonarMemoryOp;
  lOperand: TPasElement;
  lName: string;
  i, j, k, n: integer;
begin
  lRoutines := EnumerateRoutines(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lRoutines) do
  begin
    SetLength(lOps, 0);
    SetLength(lStmts, 0);
    CollectStatements(lRoutines[i].Block, lStmts);
    for j := 0 to High(lStmts) do
    begin
      if not aContext.Resolver.TryMemoryOpCall(lStmts[j], lOp, lOperand,
        lName) then
        Continue;
      n := EntryOf(lOps, lOperand, lName);
      if lOp in [lmoNew, lmoGetMem] then
      begin
        if (lOps[n].Allocators = 0) or (lOps[n].Allocator <> lOp) then
          Inc(lOps[n].Allocators);
        lOps[n].Allocator := lOp;
      end
      // A release before the routine allocates pairs with an allocation elsewhere.
      else if lOps[n].Allocators > 0 then
      begin
        SetLength(lOps[n].Releases, Length(lOps[n].Releases) + 1);
        lOps[n].Releases[High(lOps[n].Releases)] := lOp;
        SetLength(lOps[n].ReleaseRows, Length(lOps[n].ReleaseRows) + 1);
        lOps[n].ReleaseRows[High(lOps[n].ReleaseRows)] :=
          aContext.Resolver.SourceRow(lStmts[j]);
      end;
    end;
    for n := 0 to High(lOps) do
    begin
      // Two allocators for one declaration: the pairing needs a control-flow graph.
      if lOps[n].Allocators <> 1 then
        Continue;
      for k := 0 to High(lOps[n].Releases) do
        if Mismatches(lOps[n].Allocator, lOps[n].Releases[k]) then
          EmitStmt(FMetadata, aContext, aCollector, lOps[n].ReleaseRows[k],
            [lOps[n].Name, OpWord(lOps[n].Allocator),
            OpWord(lOps[n].Releases[k])], lOps[n].Name);
    end;
  end;
end;


{ TRuleOwnedFieldNotFreedInDestructor }

procedure TRuleOwnedFieldNotFreedInDestructor.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);

  // Records aDecl as owned by its declaring class when it is not already there.
  procedure AddOwned(var aOwned: TOwnedFieldArray; aDecl: TPasElement);
  var
    i: integer;
  begin
    for i := 0 to High(aOwned) do
      if aOwned[i].Decl = aDecl then
        Exit;
    SetLength(aOwned, Length(aOwned) + 1);
    aOwned[High(aOwned)].Decl := aDecl;
    aOwned[High(aOwned)].Owner := TPasClassType(aDecl.Parent);
  end;

var
  lRoutines: TAstRoutineArray;
  lStmts: TPasImplElementArray;
  lOwned: TOwnedFieldArray;
  lNodes, lDecls, lSeen: TPasElementArray;
  lAssign: TPasImplAssign;
  lDecl: TPasElement;
  lOwner: TPasClassType;
  lDtor: TPasProcedure;
  lDtorBlock: TPasImplBlock;
  lCtorName: string;
  lOnInstance, lAbstain, lMentioned: boolean;
  i, j, k: integer;
begin
  SetLength(lOwned, 0);
  lRoutines := EnumerateRoutines(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lRoutines) do
  begin
    if not (lRoutines[i].Decl is TPasConstructor)
      or (lRoutines[i].Decl is TPasClassConstructor) then
      Continue;
    SetLength(lStmts, 0);
    CollectStatements(lRoutines[i].Block, lStmts);
    for j := 0 to High(lStmts) do
    begin
      if not (lStmts[j] is TPasImplAssign) then
        Continue;
      lAssign := TPasImplAssign(lStmts[j]);
      if lAssign.Kind <> akDefault then
        Continue;
      if not aContext.Resolver.TryConstructorCall(lAssign.Right, lOnInstance,
        lCtorName) then
        Continue;
      lDecl := aContext.Resolver.ReferencedDecl(TargetIdent(lAssign.Left));
      if (lDecl = nil) or not (lDecl is TPasVariable)
        or (lDecl is TPasProperty) or not (lDecl.Parent is TPasClassType) then
        Continue;
      AddOwned(lOwned, lDecl);
    end;
  end;
  if Length(lOwned) = 0 then
    Exit;
  if not aContext.Resolver.TryReferenceSites(lNodes, lDecls) then
    Exit;
  for i := 0 to High(lOwned) do
  begin
    lOwner := lOwned[i].Owner;
    if lOwner = nil then
      Continue;
    lDtor := nil;
    lDtorBlock := nil;
    for j := 0 to High(lRoutines) do
      if (lRoutines[j].Decl is TPasDestructor)
        and not (lRoutines[j].Decl is TPasClassDestructor)
        and SameText(ClassQualifier(lRoutines[j].Decl.Name), lOwner.Name) then
      begin
        lDtor := lRoutines[j].Decl;
        lDtorBlock := lRoutines[j].Block;
        Break;
      end;
    if (lDtor = nil) or HasMethod(lOwner, 'BeforeDestruction') then
      Continue;
    SetLength(lSeen, 0);
    lAbstain := False;
    for k := 0 to High(lNodes) do
      if UnderElement(lNodes[k], lDtorBlock) then
      begin
        // A call to a method of the same class may release anything the class owns.
        if (lDecls[k] is TPasProcedure) and (lDecls[k].Parent = lOwner) then
        begin
          lAbstain := True;
          Break;
        end;
        SetLength(lSeen, Length(lSeen) + 1);
        lSeen[High(lSeen)] := lDecls[k];
      end;
    if lAbstain then
      Continue;
    lMentioned := False;
    for k := 0 to High(lSeen) do
      if lSeen[k] = lOwned[i].Decl then
      begin
        lMentioned := True;
        Break;
      end;
    if not lMentioned then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lOwned[i].Decl),
        [lOwned[i].Decl.Name, lDtor.Name], lOwned[i].Decl.Name);
  end;
end;


{ TRuleCreateWithoutTryFinally }

procedure TRuleCreateWithoutTryFinally.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitResourceVerdict(FMetadata, aContext, aCollector, rvUnprotectedRelease);
end;


{ TRuleLeakOnEarlyExit }

procedure TRuleLeakOnEarlyExit.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitResourceVerdict(FMetadata, aContext, aCollector, rvEarlyExitLeak);
end;


{ TRuleStreamNotProtected }

procedure TRuleStreamNotProtected.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitResourceVerdict(FMetadata, aContext, aCollector, rvUnprotectedStream);
end;


{ TRuleExceptionObjectFreedInHandler }

procedure TRuleExceptionObjectFreedInHandler.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lStmts, lBody: TPasImplElementArray;
  lOn: TPasImplExceptOn;
  lInner: TPasExpr;
  i, j: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
  begin
    if not (lStmts[i] is TPasImplExceptOn) then
      Continue;
    lOn := TPasImplExceptOn(lStmts[i]);
    // 'on SomeType do' binds no variable.
    if (lOn.VarEl = nil) or (lOn.Body = nil) then
      Continue;
    SetLength(lBody, 1);
    lBody[0] := lOn.Body;
    CollectStatements(lOn.Body, lBody);
    for j := 0 to High(lBody) do
      if aContext.Resolver.TryFreeCall(SimpleExpr(lBody[j]),
        lInner) <> lfkNone then
        if aContext.Resolver.ReferencedDecl(lInner) = lOn.VarEl then
        begin
          EmitStmt(FMetadata, aContext, aCollector,
            aContext.Resolver.SourceRow(lBody[j]), [lOn.VarEl.Name],
            lOn.VarEl.Name);
          Break;
        end;
  end;
end;


{ TRuleRaisedExceptionInstanceReused }

procedure TRuleRaisedExceptionInstanceReused.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lRoutines: TAstRoutineArray;
  lStmts: TPasImplElementArray;
  lNodes, lDecls: TPasElementArray;
  lDecl: TPasElement;
  lRow: integer;
  i, j, k: integer;
begin
  if not aContext.Resolver.TryReferenceSites(lNodes, lDecls) then
    Exit;
  lRoutines := EnumerateRoutines(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lRoutines) do
  begin
    SetLength(lStmts, 0);
    CollectStatements(lRoutines[i].Block, lStmts);
    for j := 0 to High(lStmts) do
    begin
      if not (lStmts[j] is TPasImplRaise) then
        Continue;
      lDecl := aContext.Resolver.ReferencedDecl(
        TPasImplRaise(lStmts[j]).ExceptObject);
      if (lDecl = nil) or (lDecl is TPasProperty)
        or not ((lDecl is TPasVariable) or (lDecl is TPasArgument)) then
        Continue;
      lRow := aContext.Resolver.SourceRow(lStmts[j]);
      for k := 0 to High(lNodes) do
        if (lDecls[k] = lDecl)
          and UnderElement(lNodes[k], lRoutines[i].Block)
          and (aContext.Resolver.SourceRow(lNodes[k]) > lRow)
          and not UnderAssignTarget(lNodes[k]) then
        begin
          EmitStmt(FMetadata, aContext, aCollector, lRow, [lDecl.Name],
            lDecl.Name);
          Break;
        end;
    end;
  end;
end;


{ TRuleLoopVariableUsedAfterLoop }

procedure TRuleLoopVariableUsedAfterLoop.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lSite: TPasElement;
  lName: string;
  i: integer;
begin
  lStmts := AllStatements(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lStmts) do
    if aContext.Resolver.TryLoopVarReadAfterLoop(lStmts[i], lName, lSite) then
      EmitStmt(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lSite), [lName], lName);
end;


{ TRuleLoopVariableModifiedInBody }

procedure TRuleLoopVariableModifiedInBody.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStmts: TPasImplElementArray;
  lAssign: TPasImplAssign;
  lWalk: TPasElement;
  lName: string;
  i, lDepth: integer;
begin
  lStmts := AllStatements(aContext.Module);
  for i := 0 to High(lStmts) do
  begin
    if not (lStmts[i] is TPasImplAssign) then
      Continue;
    lAssign := TPasImplAssign(lStmts[i]);
    lName := BareIdent(lAssign.Left);
    if lName = '' then
      Continue;
    lWalk := lAssign.Parent;
    lDepth := 0;
    while (lWalk <> nil) and (lDepth < cMaxParentDepth) do
    begin
      if (lWalk is TPasImplForLoop)
        and SameText(BareIdent(TPasImplForLoop(lWalk).VariableName), lName) then
      begin
        EmitStmt(FMetadata, aContext, aCollector, lAssign.SourceLinenumber,
          [lName], lName);
        Break;
      end;
      lWalk := lWalk.Parent;
      Inc(lDepth);
    end;
  end;
end;


{ TRuleUseAfterFree }

procedure TRuleUseAfterFree.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitFreeStateVerdict(FMetadata, aContext, aCollector, fsvUseAfterFree);
end;


{ TRuleDoubleFree }

procedure TRuleDoubleFree.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitFreeStateVerdict(FMetadata, aContext, aCollector, fsvDoubleFree);
end;


{ TRuleFreeNotFreeAndNilOnField }

procedure TRuleFreeNotFreeAndNilOnField.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitFreeStateVerdict(FMetadata, aContext, aCollector,
    fsvFieldFreedNotNilled);
end;


{ TRuleGetMemWithoutFreeMem }

procedure TRuleGetMemWithoutFreeMem.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitFreeStateVerdict(FMetadata, aContext, aCollector,
    fsvUnpairedAllocation);
end;


{ TRuleObjectCreatedInLoopNotFreed }

procedure TRuleObjectCreatedInLoopNotFreed.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitFreeStateVerdict(FMetadata, aContext, aCollector,
    fsvLoopAllocationNotFreed);
end;


{ TRuleUnbalancedPair }

procedure TRuleUnbalancedPair.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  EmitPairVerdicts(FMetadata, aContext, aCollector,
    ConfiguredPairs(aContext, FMetadata.RuleId));
end;


// UnbalancedPair's metadata and its acquire/release table parameter.
function UnbalancedPairMeta: TRuleMetadata;

begin
  Result := TRuleMetadata.Make('UnbalancedPair', rtSem, rfResolver, sevMajor,
    itBug, cfMedium, False, cKeyUnbalancedPair).WithDescription(
    'Flags an acquire whose matching release the routine makes outside a '
    + 'covering try..finally.');
  Result.AddParam(cTargetsKey, rpkTargets);
end;


initialization
  RegisterRule(TRuleFreeOnInterfaceReference.Create(TRuleMetadata.Make(
    'FreeOnInterfaceReference', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    False, cKeyFreeOnInterfaceReference).WithDescription(
    'Flags a Free or FreeAndNil disposal whose operand is an interface-typed reference.')));
  RegisterMessage(cKeyFreeOnInterfaceReference, SFreeOnInterfaceReference);

  RegisterRule(TRuleSelfDestroyedInMethod.Create(TRuleMetadata.Make(
    'SelfDestroyedInMethod', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeySelfDestroyedInMethod).WithDescription(
    'Flags a method other than a destructor that frees Self.')));
  RegisterMessage(cKeySelfDestroyedInMethod, SSelfDestroyedInMethod);

  RegisterRule(TRuleNewDisposeMismatch.Create(TRuleMetadata.Make(
    'NewDisposeMismatch', rtSem, rfResolver, sevCritical, itBug, cfHigh,
    False, cKeyNewDisposeMismatch).WithDescription(
    'Flags a pointer released with a deallocator that does not match its allocator.')));
  RegisterMessage(cKeyNewDisposeMismatch, SNewDisposeMismatch);

  RegisterRule(TRuleOwnedFieldNotFreedInDestructor.Create(TRuleMetadata.Make(
    'OwnedFieldNotFreedInDestructor', rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, cKeyOwnedFieldNotFreedInDestructor).WithDescription(
    'Flags a class field constructed in a constructor that its destructor never releases.')));
  RegisterMessage(cKeyOwnedFieldNotFreedInDestructor,
    SOwnedFieldNotFreedInDestructor);

  RegisterRule(TRuleCreateWithoutTryFinally.Create(TRuleMetadata.Make(
    'CreateWithoutTryFinally', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyCreateWithoutTryFinally).WithDescription(
    'Flags an object the routine releases with no try..finally covering its construction.')));
  RegisterMessage(cKeyCreateWithoutTryFinally, SCreateWithoutTryFinally);

  RegisterRule(TRuleExceptionObjectFreedInHandler.Create(TRuleMetadata.Make(
    'ExceptionObjectFreedInHandler', rtSem, rfResolver, sevCritical, itBug,
    cfHigh, False, cKeyExceptionObjectFreedInHandler).WithDescription(
    'Flags a Free or FreeAndNil applied to the exception variable of the handler that caught it.')));
  RegisterMessage(cKeyExceptionObjectFreedInHandler,
    SExceptionObjectFreedInHandler);

  RegisterRule(TRuleRaisedExceptionInstanceReused.Create(TRuleMetadata.Make(
    'RaisedExceptionInstanceReused', rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, cKeyRaisedExceptionInstanceReused).WithDescription(
    'Flags an exception instance referenced after a raise transferred its ownership.')));
  RegisterMessage(cKeyRaisedExceptionInstanceReused,
    SRaisedExceptionInstanceReused);

  RegisterRule(TRuleLoopVariableUsedAfterLoop.Create(TRuleMetadata.Make(
    'LoopVariableUsedAfterLoop', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyLoopVariableUsedAfterLoop).WithDescription(
    'Flags a read of a for-loop control variable after the loop, whose value the language leaves undefined.')));
  RegisterMessage(cKeyLoopVariableUsedAfterLoop, SLoopVariableUsedAfterLoop);

  RegisterRule(TRuleLoopVariableModifiedInBody.Create(TRuleMetadata.Make(
    'LoopVariableModifiedInBody', rtAst, rfAst, sevMajor, itBug, cfMedium,
    False, cKeyLoopVariableModifiedInBody).WithDescription(
    'Flags an assignment to a for-loop control variable inside its own body.')));
  RegisterMessage(cKeyLoopVariableModifiedInBody, SLoopVariableModifiedInBody);

  RegisterRule(TRuleLeakOnEarlyExit.Create(TRuleMetadata.Make(
    'LeakOnEarlyExit', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyLeakOnEarlyExit).WithDescription(
    'Flags an exit or raise reached while a constructed local is still unreleased.')));
  RegisterMessage(cKeyLeakOnEarlyExit, SLeakOnEarlyExit);

  RegisterRule(TRuleStreamNotProtected.Create(TRuleMetadata.Make(
    'StreamNotProtected', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyStreamNotProtected).WithDescription(
    'Flags a stream constructed into a local with no try..finally covering its construction.')));
  RegisterMessage(cKeyStreamNotProtected, SStreamNotProtected);

  // Polarity: positive -- the read on a released path is the defect.
  RegisterRule(TRuleUseAfterFree.Create(TRuleMetadata.Make(
    'UseAfterFree', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyUseAfterFree).WithDescription(
    'Flags a read of a routine local on a path that already released it.')));
  RegisterMessage(cKeyUseAfterFree, SUseAfterFree);

  // Polarity: positive -- the second release is the defect.
  RegisterRule(TRuleDoubleFree.Create(TRuleMetadata.Make(
    'DoubleFree', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyDoubleFree).WithDescription(
    'Flags a release of a reference that a path reaching it already released.')));
  RegisterMessage(cKeyDoubleFree, SDoubleFree);

  // Polarity: positive -- the release that leaves the field dangling is the defect.
  RegisterRule(TRuleFreeNotFreeAndNilOnField.Create(TRuleMetadata.Make(
    'FreeNotFreeAndNilOnField', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyFreeNotFreeAndNilOnField).WithDescription(
    'Flags a field released with Free rather than FreeAndNil and read afterwards.')));
  RegisterMessage(cKeyFreeNotFreeAndNilOnField, SFreeNotFreeAndNilOnField);

  // Polarity: absence -- a release on any path satisfies the rule.
  RegisterRule(TRuleGetMemWithoutFreeMem.Create(TRuleMetadata.Make(
    'GetMemWithoutFreeMem', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyGetMemWithoutFreeMem).WithDescription(
    'Flags a heap allocation into a local that no path of the routine releases.')));
  RegisterMessage(cKeyGetMemWithoutFreeMem, SGetMemWithoutFreeMem);

  // Polarity: absence -- a release inside the loop body satisfies the rule.
  RegisterRule(TRuleObjectCreatedInLoopNotFreed.Create(TRuleMetadata.Make(
    'ObjectCreatedInLoopNotFreed', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyObjectCreatedInLoopNotFreed).WithDescription(
    'Flags a construction in a loop body that the loop does not release.')));
  RegisterMessage(cKeyObjectCreatedInLoopNotFreed,
    SObjectCreatedInLoopNotFreed);

  // Polarity: positive -- the release outside a finally is the defect.
  RegisterRule(TRuleUnbalancedPair.Create(UnbalancedPairMeta));
  RegisterMessage(cKeyUnbalancedPair, SUnbalancedPair);

end.
