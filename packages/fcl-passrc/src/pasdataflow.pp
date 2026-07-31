{
    This file is part of the Free Component Library

    Pascal data-flow analysis (uninitialized-variable detection)
    Copyright (c) 2026

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
Abstract:
  After running TPasResolver, run this to emit a warning for a simple-typed
  local (or program-global) variable whose use precedes every definition of it
  ("Variable %s does not seem to be initialized").

  This complements PasUseAnalyzer, which reports unused declarations and an
  unset function result but does NOT flag uninitialized *variable use*.

Design:
  This reproduces the behaviour of the FPC compiler's -Oodfa switch, which does
  a limited, not a dynamic/correlated, analysis.

  The pass is intentionally optimistic:
  a definition anywhere textually earlier — even inside a single
  conditional branch — suppresses the warning for all later uses.
  It is therefore NOT a sound "definitely uninitialized" analysis:
  it can miss a use that is uninitialized only on some paths.

  This matches FPC: e.g. a variable assigned in one `if` branch and used under
  a later, independent `if` is not flagged.

  Equivalent rule: flag a use that precedes every definition of the variable
  in the order the graph's blocks are visited.

  A use at the top of a loop body, before a later definition in the same
  body, is still flagged — first iteration is uninitialized.

  Only simple typed variables are considered (ordinals, floats, booleans, chars,
  pointers, enums).

  Structured types (records, objects, classes, arrays, strings, sets,
  interfaces, procvars) are never flagged, because they may be
  initialized field-by-field or through a method call.

  Definitions include:
  assignment LHS, compound-assignment LHS, passing to a
  var/out/untyped parameter (this covers FillChar/Move destinations and the
  implicit Self of an object method), and a mention inside an asm block.

  The classification relies on the read/write access the resolver records on
  each reference (TResolvedReference.Access). TPasDataFlowEngine iterates a
  flow-insensitive lattice over the routine's TPasCFG.

Scope:
  - every procedure/function body is analysed over its own local variables;
  - the main program begin..end block is analysed over the program's globals;
  - unit initialization/finalization sections are NOT analysed (that needs a
    cross-section / whole-program analysis);
  - only the passed-in module is analysed, never the units it uses.

Reporting:
  The warning is emitted through the (overridable) EmitMessage,
  which by default routes to Resolver.LogMsg with mtWarning and
  MsgNumber nUninitializedVariable.
  A consumer that wants -Sew semantics sets
    Scanner.WarnMsgState[nUninitializedVariable] := wmsError;
  before running the analysis;

  LogMsg then raises EPasResolve, aborting the compile — exactly as the
  FPC compiler does for -Oodfa -Sew.

  Consumers that run the analysis after the scanner is gone should
  override EmitMessage, and read the findings as (variable, position) pairs
  from ResultCount/Results rather than parsing the formatted message.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit PasDataFlow;
{$ENDIF FPC_DOTTEDUNITS}

{$i fcl-passrc.inc}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils,
  Pascal.Tree, Pascal.Scanner, Pascal.ResolveEval, Pascal.Resolver, Pascal.CFG;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils,
  PasTree, PScanner, PasResolveEval, PasResolver, PasCFG;
{$ENDIF FPC_DOTTEDUNITS}

const
  // Matches the FPC compiler message sym_w_uninitialized_local_variable.
  nUninitializedVariable = 5036;
  sUninitializedVariable = 'Variable "%s" does not seem to be initialized';

type
  // One uninitialized-variable finding: the variable and its use site.
  TPasDataFlowResult = record
    Variable: TPasVariable;
    PosEl: TPasElement;
  end;

  // Direction in which the engine propagates a lattice's states.
  TPasDataFlowDirection = (dfdForward, dfdBackward);

  { TPasDataFlowLattice }

  // Merge and Transfer must be monotone; the engine has no iteration cap.
  TPasDataFlowLattice = class
  public
    // Direction in which the engine propagates this lattice's states.
    function Direction: TPasDataFlowDirection; virtual; abstract;
    // A new state holding the value every node starts from.
    function CreateState: TObject; virtual; abstract;
    // An independent copy of aState.
    function CopyState(aState: TObject): TObject; virtual; abstract;
    // Releases a state obtained from CreateState or CopyState.
    procedure FreeState(aState: TObject); virtual; abstract;
    // Joins aSource into aTarget.
    procedure Merge(aTarget,aSource: TObject); virtual; abstract;
    // Applies aNode's own effect to aState.
    procedure Transfer(aNode: TPasCFGNode; aState: TObject); virtual; abstract;
    // True when aLeft and aRight hold the same value.
    function SameState(aLeft,aRight: TObject): Boolean; virtual; abstract;
  end;

  { TPasDataFlowEngine }

  TPasDataFlowEngine = class
  private
    FLattice: TPasDataFlowLattice;
    FPredecessors: TFPList; // one TFPList of TPasCFGNode per node index
    FQueue: TFPList;        // indices of the nodes still to transfer
    FQueued: array of Boolean;
    FStates: TFPList;       // one lattice state per node index
    procedure BuildPredecessors(aCFG: TPasCFG);
    procedure ClearRun;
    procedure Enqueue(aIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    // Iterates aLattice over every node of aCFG until no node state changes.
    // aLattice must outlive the engine: it frees the states on Destroy.
    procedure Run(aCFG: TPasCFG; aLattice: TPasDataFlowLattice);
    // State the last Run left on aNode after its transfer, or nil when it has none.
    function StateOf(aNode: TPasCFGNode): TObject;
  end;

  { TPasDataFlowAnalyzer }

  TPasDataFlowAnalyzer = class
  private
    FResolver: TPasResolver;
    FTracked: TFPList;   // TPasVariable being tracked for the current routine
    FAssigned: TFPList;  // TPasVariable already possibly-assigned
    FReported: TFPList;  // TPasVariable already reported (dedupe)
    FResultVars: TFPList; // TPasVariable of each finding, in report order
    FResultPos: TFPList;  // TPasElement use site of each finding
    FState: TFPList;      // lattice state of the node being transferred
    function GetResultCount: Integer;
    function GetResult(Index: Integer): TPasDataFlowResult;
    function IsTracked(El: TPasElement): Boolean;
    function IsAssigned(V: TPasElement): Boolean;
    procedure MarkAssigned(V: TPasElement);
    procedure MarkAsmIdents(const S: String);
    procedure ReportUninit(V: TPasVariable; PosEl: TPasElement);
    procedure HandleRef(Expr: TPasExpr);
    procedure ProcessExpr(Expr: TPasExpr);
    procedure ProcessStmt(El: TPasElement);
    procedure TransferNode(aNode: TPasCFGNode; aState: TFPList);
    function IsSimpleVarType(V: TPasVariable): Boolean;
    procedure CollectLocals(Decls: TPasDeclarations; List: TFPList);
    procedure AnalyzeRoutine(Body: TPasElement; Locals: TFPList);
    procedure AnalyzeProcs(Decls: TPasDeclarations);
  protected
    // Emit the "not initialized" diagnostic.
    // Default routes through Resolver.LogMsg.
    // Override to decouple from the scanner (e.g. a post-parse batch run).
    procedure EmitMessage(MsgNumber: Integer; const Fmt: String;
      const Args: array of const; PosEl: TPasElement); virtual;
  public
    constructor Create(AResolver: TPasResolver);
    destructor Destroy; override;
    // Analyse the given module for uninitialized-variable use.
    procedure AnalyzeModule(aModule: TPasModule);
    property Resolver: TPasResolver read FResolver;
    // Number of findings recorded by the last AnalyzeModule.
    property ResultCount: Integer read GetResultCount;
    // The findings of the last AnalyzeModule, in report order.
    property Results[Index: Integer]: TPasDataFlowResult read GetResult;
  end;

implementation

type

  { TPasAssignedLattice — assigned-variable sets over one routine }

  TPasAssignedLattice = class(TPasDataFlowLattice)
  private
    FAnalyzer: TPasDataFlowAnalyzer;
  public
    // Binds the lattice to the analyzer whose tracked set it works over.
    constructor Create(aAnalyzer: TPasDataFlowAnalyzer);
    function Direction: TPasDataFlowDirection; override;
    function CreateState: TObject; override;
    function CopyState(aState: TObject): TObject; override;
    procedure FreeState(aState: TObject); override;
    procedure Merge(aTarget,aSource: TObject); override;
    procedure Transfer(aNode: TPasCFGNode; aState: TObject); override;
    function SameState(aLeft,aRight: TObject): Boolean; override;
  end;


{ TPasDataFlowEngine }

constructor TPasDataFlowEngine.Create;

begin
  FPredecessors := TFPList.Create;
  FQueue := TFPList.Create;
  FStates := TFPList.Create;
end;


destructor TPasDataFlowEngine.Destroy;

begin
  ClearRun;
  FStates.Free;
  FQueue.Free;
  FPredecessors.Free;
  inherited Destroy;
end;


procedure TPasDataFlowEngine.BuildPredecessors(aCFG: TPasCFG);

var
  I, J: Integer;
  lNode: TPasCFGNode;

begin
  for I := 0 to aCFG.NodeCount - 1 do
    FPredecessors.Add(TFPList.Create);
  for I := 0 to aCFG.NodeCount - 1 do
  begin
    lNode := aCFG.Nodes[I];
    for J := 0 to lNode.SuccessorCount - 1 do
      TFPList(FPredecessors[lNode.Successors[J].Index]).Add(lNode);
  end;
end;


procedure TPasDataFlowEngine.ClearRun;

var
  I: Integer;

begin
  if FLattice <> nil then
    for I := 0 to FStates.Count - 1 do
      FLattice.FreeState(TObject(FStates[I]));
  FStates.Clear;
  for I := 0 to FPredecessors.Count - 1 do
    TFPList(FPredecessors[I]).Free;
  FPredecessors.Clear;
  FQueue.Clear;
  SetLength(FQueued, 0);
  FLattice := nil;
end;


procedure TPasDataFlowEngine.Enqueue(aIndex: Integer);

begin
  if FQueued[aIndex] then Exit;
  FQueued[aIndex] := True;
  FQueue.Add(Pointer(PtrInt(aIndex)));
end;


procedure TPasDataFlowEngine.Run(aCFG: TPasCFG; aLattice: TPasDataFlowLattice);

var
  I, lIndex: Integer;
  lBackward, lChanged: Boolean;
  lIn, lInitial: TObject;
  lNode: TPasCFGNode;
  lPreds: TFPList;

begin
  ClearRun;
  if (aCFG = nil) or (aLattice = nil) then Exit;
  FLattice := aLattice;
  lBackward := aLattice.Direction = dfdBackward;
  BuildPredecessors(aCFG);
  SetLength(FQueued, aCFG.NodeCount);
  lInitial := aLattice.CreateState;
  try
    // Index order is reverse postorder with the unreachable nodes last, so a
    // lattice reporting from Transfer reports in that order.
    for lIndex := 0 to aCFG.NodeCount - 1 do
    begin
      FStates.Add(Pointer(aLattice.CopyState(lInitial)));
      Enqueue(lIndex);
    end;
  finally
    aLattice.FreeState(lInitial);
  end;

  while FQueue.Count > 0 do
  begin
    lIndex := PtrInt(FQueue[0]);
    FQueue.Delete(0);
    FQueued[lIndex] := False;
    lNode := aCFG.Nodes[lIndex];
    lPreds := TFPList(FPredecessors[lIndex]);
    lIn := aLattice.CreateState;
    try
      if lBackward then
        for I := 0 to lNode.SuccessorCount - 1 do
          aLattice.Merge(lIn, TObject(FStates[lNode.Successors[I].Index]))
      else
        for I := 0 to lPreds.Count - 1 do
          aLattice.Merge(lIn, TObject(FStates[TPasCFGNode(lPreds[I]).Index]));
      aLattice.Transfer(lNode, lIn);
      lChanged := not aLattice.SameState(TObject(FStates[lIndex]), lIn);
      if lChanged then
      begin
        aLattice.FreeState(TObject(FStates[lIndex]));
        FStates[lIndex] := Pointer(lIn);
        lIn := nil;
      end;
    finally
      if lIn <> nil then
        aLattice.FreeState(lIn);
    end;
    if lChanged then
    begin
      if lBackward then
        for I := 0 to lPreds.Count - 1 do
          Enqueue(TPasCFGNode(lPreds[I]).Index)
      else
        for I := 0 to lNode.SuccessorCount - 1 do
          Enqueue(lNode.Successors[I].Index);
    end;
  end;
end;


function TPasDataFlowEngine.StateOf(aNode: TPasCFGNode): TObject;

begin
  Result := nil;
  if (aNode = nil) or (aNode.Index < 0) or (aNode.Index >= FStates.Count) then Exit;
  Result := TObject(FStates[aNode.Index]);
end;


{ TPasAssignedLattice }

constructor TPasAssignedLattice.Create(aAnalyzer: TPasDataFlowAnalyzer);

begin
  FAnalyzer := aAnalyzer;
end;


function TPasAssignedLattice.Direction: TPasDataFlowDirection;

begin
  Result := dfdForward;
end;


function TPasAssignedLattice.CreateState: TObject;

begin
  // The routine-wide accumulate, not a constant: this is what makes the client
  // flow-insensitive.
  Result := TFPList.Create;
  TFPList(Result).Assign(FAnalyzer.FAssigned);
end;


function TPasAssignedLattice.CopyState(aState: TObject): TObject;

begin
  Result := TFPList.Create;
  TFPList(Result).Assign(TFPList(aState));
end;


procedure TPasAssignedLattice.FreeState(aState: TObject);

begin
  aState.Free;
end;


procedure TPasAssignedLattice.Merge(aTarget,aSource: TObject);

var
  I: Integer;
  lSource, lTarget: TFPList;

begin
  lTarget := TFPList(aTarget);
  lSource := TFPList(aSource);
  for I := 0 to lSource.Count - 1 do
    if lTarget.IndexOf(lSource[I]) < 0 then
      lTarget.Add(lSource[I]);
end;


procedure TPasAssignedLattice.Transfer(aNode: TPasCFGNode; aState: TObject);

begin
  FAnalyzer.TransferNode(aNode, TFPList(aState));
end;


function TPasAssignedLattice.SameState(aLeft,aRight: TObject): Boolean;

var
  I: Integer;
  lLeft, lRight: TFPList;

begin
  lLeft := TFPList(aLeft);
  lRight := TFPList(aRight);
  Result := lLeft.Count = lRight.Count;
  if not Result then Exit;
  for I := 0 to lLeft.Count - 1 do
    if lRight.IndexOf(lLeft[I]) < 0 then
      Exit(False);
end;


{ TPasDataFlowAnalyzer }

constructor TPasDataFlowAnalyzer.Create(AResolver: TPasResolver);
begin
  FResolver := AResolver;
  FTracked := TFPList.Create;
  FAssigned := TFPList.Create;
  FReported := TFPList.Create;
  FResultVars := TFPList.Create;
  FResultPos := TFPList.Create;
end;


destructor TPasDataFlowAnalyzer.Destroy;
begin
  FTracked.Free;
  FAssigned.Free;
  FReported.Free;
  FResultVars.Free;
  FResultPos.Free;
  inherited Destroy;
end;


function TPasDataFlowAnalyzer.GetResultCount: Integer;
begin
  Result := FResultVars.Count;
end;


function TPasDataFlowAnalyzer.GetResult(Index: Integer): TPasDataFlowResult;
begin
  Result.Variable := TPasVariable(FResultVars[Index]);
  Result.PosEl := TPasElement(FResultPos[Index]);
end;


procedure TPasDataFlowAnalyzer.EmitMessage(MsgNumber: Integer;
  const Fmt: String; const Args: array of const; PosEl: TPasElement);
begin
  FResolver.LogMsg(20260715120000, mtWarning, MsgNumber, Fmt, Args, PosEl);
end;


function TPasDataFlowAnalyzer.IsTracked(El: TPasElement): Boolean;
begin
  Result := (El <> nil) and (FTracked.IndexOf(El) >= 0);
end;


function TPasDataFlowAnalyzer.IsAssigned(V: TPasElement): Boolean;
begin
  Result := FState.IndexOf(V) >= 0;
end;


procedure TPasDataFlowAnalyzer.MarkAssigned(V: TPasElement);
begin
  if FAssigned.IndexOf(V) < 0 then
    FAssigned.Add(V);
  if FState.IndexOf(V) < 0 then
    FState.Add(V);
end;


procedure TPasDataFlowAnalyzer.MarkAsmIdents(const S: String);
var
  I, StartPos, J: Integer;
  Ident: String;
begin
  I := 1;
  while I <= Length(S) do
    if S[I] in ['A'..'Z', 'a'..'z', '_'] then
    begin
      StartPos := I;
      while (I <= Length(S)) and
            (S[I] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
        Inc(I);
      Ident := Copy(S, StartPos, I - StartPos);
      for J := 0 to FTracked.Count - 1 do
        if SameText(TPasVariable(FTracked[J]).Name, Ident) then
          MarkAssigned(TPasElement(FTracked[J]));
    end
    else
      Inc(I);
end;


procedure TPasDataFlowAnalyzer.ReportUninit(V: TPasVariable; PosEl: TPasElement);
begin
  if FReported.IndexOf(V) >= 0 then Exit;
  FReported.Add(V);
  FResultVars.Add(V);
  FResultPos.Add(PosEl);
  EmitMessage(nUninitializedVariable, sUninitializedVariable, [V.Name], PosEl);
end;


procedure TPasDataFlowAnalyzer.HandleRef(Expr: TPasExpr);
var
  Ref: TResolvedReference;
  Decl: TPasElement;
begin
  if not (Expr.CustomData is TResolvedReference) then Exit;
  Ref := TResolvedReference(Expr.CustomData);
  Decl := Ref.Declaration;
  if not IsTracked(Decl) then Exit;
  case Ref.Access of
    rraRead:
      if not IsAssigned(Decl) then
        ReportUninit(TPasVariable(Decl), Expr);
    // Any write-ish access initializes the variable. rraVarParam/rraOutParam
    // (var/out param, incl. FillChar/Move and the Self of an object method) and
    // rraReadAndAssign (inc/dec, +=) are treated as definitions and are NOT
    // flagged as reads — conservatively, to avoid false positives.
    rraAssign, rraReadAndAssign, rraVarParam, rraOutParam,
    rraParamToUnknownProc:
      MarkAssigned(Decl);
  else
    ; // rraNone: not a use of the variable's value
  end;
end;


procedure TPasDataFlowAnalyzer.ProcessExpr(Expr: TPasExpr);
var
  I: Integer;
begin
  if Expr = nil then Exit;

  if Expr is TBinaryExpr then
  begin
    // Left then right = evaluation order (also correct for and/or short-circuit).
    ProcessExpr(TBinaryExpr(Expr).left);
    ProcessExpr(TBinaryExpr(Expr).right);
  end
  else if Expr is TUnaryExpr then
    ProcessExpr(TUnaryExpr(Expr).Operand)
  else if Expr is TParamsExpr then
  begin
    ProcessExpr(TParamsExpr(Expr).Value);
    for I := 0 to High(TParamsExpr(Expr).Params) do
      ProcessExpr(TParamsExpr(Expr).Params[I]);
  end
  else if Expr is TInlineSpecializeExpr then
    ProcessExpr(TInlineSpecializeExpr(Expr).NameExpr)
  else if Expr is TArrayValues then
  begin
    for I := 0 to High(TArrayValues(Expr).Values) do
      ProcessExpr(TArrayValues(Expr).Values[I]);
  end
  else if Expr is TRecordValues then
  begin
    for I := 0 to High(TRecordValues(Expr).Fields) do
      ProcessExpr(TRecordValues(Expr).Fields[I].ValueExp);
  end;

  // A leaf identifier reference (simple var, or the head of a member access)
  // carries the tracked-variable reference directly.
  HandleRef(Expr);
end;


procedure TPasDataFlowAnalyzer.ProcessStmt(El: TPasElement);
var
  I: Integer;
begin
  if El = nil then Exit;

  if El is TPasImplAssign then
  begin
    // RHS is evaluated before the store to the LHS target.
    ProcessExpr(TPasImplAssign(El).Right);
    ProcessExpr(TPasImplAssign(El).Left);
  end
  else if El is TPasImplSimple then
    ProcessExpr(TPasImplSimple(El).Expr)
  else if El is TPasImplIfElse then
    ProcessExpr(TPasImplIfElse(El).ConditionExpr)
  else if El is TPasImplWhileDo then
    ProcessExpr(TPasImplWhileDo(El).ConditionExpr)
  else if El is TPasImplRepeatUntil then
    ProcessExpr(TPasImplRepeatUntil(El).ConditionExpr)
  else if El is TPasImplForLoop then
  begin
    ProcessExpr(TPasImplForLoop(El).StartExpr);
    ProcessExpr(TPasImplForLoop(El).EndExpr);
    // The loop control variable is assigned by the loop header.
    ProcessExpr(TPasImplForLoop(El).VariableName);
  end
  else if El is TPasImplCaseOf then
    ProcessExpr(TPasImplCaseOf(El).CaseExpr)
  else if El is TPasImplWithDo then
  begin
    for I := 0 to TPasImplWithDo(El).Expressions.Count - 1 do
      ProcessExpr(TPasExpr(TPasImplWithDo(El).Expressions[I]));
  end
  else if El is TPasImplRaise then
  begin
    ProcessExpr(TPasImplRaise(El).ExceptObject);
    ProcessExpr(TPasImplRaise(El).ExceptAddr);
  end
  else if El is TPasImplAsmStatement then
  begin
    // A tracked variable named anywhere in the asm block counts as defined.
    for I := 0 to TPasImplAsmStatement(El).Tokens.Count - 1 do
      MarkAsmIdents(TPasImplAsmStatement(El).Tokens[I]);
    for I := 0 to TPasImplAsmStatement(El).ModifierTokens.Count - 1 do
      MarkAsmIdents(TPasImplAsmStatement(El).ModifierTokens[I]);
  end;
end;


procedure TPasDataFlowAnalyzer.TransferNode(aNode: TPasCFGNode; aState: TFPList);
var
  I: Integer;
begin
  FState := aState;
  try
    for I := 0 to aNode.StatementCount - 1 do
      ProcessStmt(aNode.Statements[I]);
  finally
    FState := nil;
  end;
end;


function TPasDataFlowAnalyzer.IsSimpleVarType(V: TPasVariable): Boolean;
var
  Resolved: TPasResolverResult;
begin
  Result := False;
  if V.VarType = nil then Exit;
  FResolver.ComputeElement(V.VarType, Resolved, [rcType]);
  if Resolved.BaseType in (btAllInteger + btAllFloats + btAllBooleans +
       btAllChars + [btPointer, btCurrency]) then
    Result := True
  else if (Resolved.BaseType = btContext) and
          (Resolved.LoTypeEl is TPasEnumType) then
    Result := True;
end;


procedure TPasDataFlowAnalyzer.CollectLocals(Decls: TPasDeclarations;
  List: TFPList);
var
  I: Integer;
  El: TPasElement;
begin
  if Decls = nil then Exit;
  for I := 0 to Decls.Declarations.Count - 1 do
  begin
    El := TPasElement(Decls.Declarations[I]);
    // Only plain, simple-typed variables (not typed consts, properties, args)
    // without an initializer expression — those are already initialized.
    // An absolute variable aliases another one, so it is not tracked.
    if (El.ClassType = TPasVariable) and (TPasVariable(El).Expr = nil) and
       (TPasVariable(El).AbsoluteExpr = nil) and
       IsSimpleVarType(TPasVariable(El)) then
      List.Add(El);
  end;
end;


procedure TPasDataFlowAnalyzer.AnalyzeRoutine(Body: TPasElement; Locals: TFPList);
var
  lCFG: TPasCFG;
  lEngine: TPasDataFlowEngine;
  lLattice: TPasAssignedLattice;
begin
  if not (Body is TPasImplBlock) or (Locals.Count = 0) then Exit;
  FTracked.Clear;
  FTracked.Assign(Locals);
  FAssigned.Clear;
  FReported.Clear;
  lCFG := TPasCFG.Create(TPasImplBlock(Body));
  try
    lLattice := TPasAssignedLattice.Create(Self);
    try
      lEngine := TPasDataFlowEngine.Create;
      try
        lEngine.Run(lCFG, lLattice);
      finally
        lEngine.Free;
      end;
    finally
      lLattice.Free;
    end;
  finally
    lCFG.Free;
  end;
end;


procedure TPasDataFlowAnalyzer.AnalyzeProcs(Decls: TPasDeclarations);
var
  I: Integer;
  El: TPasElement;
  Proc: TPasProcedure;
  Locals: TFPList;
begin
  if Decls = nil then Exit;
  for I := 0 to Decls.Declarations.Count - 1 do
  begin
    El := TPasElement(Decls.Declarations[I]);
    if not (El is TPasProcedure) then Continue;
    Proc := TPasProcedure(El);
    if (Proc.Body = nil) then Continue;
    Locals := TFPList.Create;
    try
      CollectLocals(Proc.Body, Locals);
      AnalyzeRoutine(Proc.Body.Body, Locals);
    finally
      Locals.Free;
    end;
    // Nested procedures.
    AnalyzeProcs(Proc.Body);
  end;
end;


procedure TPasDataFlowAnalyzer.AnalyzeModule(aModule: TPasModule);
var
  Globals: TFPList;
begin
  FResultVars.Clear;
  FResultPos.Clear;
  if aModule = nil then Exit;

  if aModule is TPasProgram then
  begin
    // Procedures declared in the program section.
    AnalyzeProcs(TPasProgram(aModule).ProgramSection);
    // The main begin..end block, analysed over the program's global variables.
    Globals := TFPList.Create;
    try
      CollectLocals(TPasProgram(aModule).ProgramSection, Globals);
      AnalyzeRoutine(aModule.InitializationSection, Globals);
    finally
      Globals.Free;
    end;
  end
  else
  begin
    // Unit/library: analyse procedure bodies only (interface + implementation).
    // Init/finalization sections need whole-program analysis and are left alone.
    AnalyzeProcs(aModule.InterfaceSection);
    AnalyzeProcs(aModule.ImplementationSection);
  end;
end;

end.
