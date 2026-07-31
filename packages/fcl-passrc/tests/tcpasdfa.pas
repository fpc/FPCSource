{
  FPCUnit tests for PasDataFlow (uninitialized-variable analysis).
}
unit tcpasdfa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasTree, PScanner, PasResolver, PasResolveEval,
  tcbaseparser, tcresolver, PasCFG, PasDataFlow;

type

  { TTestDataFlowAnalyzer — collects the emitted diagnostics }

  TTestDataFlowAnalyzer = class(TPasDataFlowAnalyzer)
  public
    Msgs: TStringList; // one entry per diagnostic: '<MsgNumber>|<text>'
  protected
    procedure EmitMessage(MsgNumber: Integer; const Fmt: String;
      const Args: array of const; PosEl: TPasElement); override;
  end;

  { TCustomTestDataFlow }

  TCustomTestDataFlow = Class(TCustomTestResolver)
  private
    FAnalyzer: TTestDataFlowAnalyzer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure AnalyzeProgram; virtual;
    procedure AnalyzeUnit; virtual;
    // Assert a diagnostic with the given number and text was emitted.
    procedure CheckHint(MsgNumber: Integer; const MsgText: string); virtual;
    // Assert nothing was flagged.
    procedure CheckNoHints; virtual;
    // Body of the parsed program's routine named aName.
    function RoutineBody(const aName: string): TPasImplBlock;
    // Reaching-node-index states of every node of aCFG, as 'index={set}' entries.
    function EngineStates(aCFG: TPasCFG;
      aDirection: TPasDataFlowDirection): string;
  public
    property Analyzer: TTestDataFlowAnalyzer read FAnalyzer;
  end;

  { TTestDataFlow }

  TTestDataFlow = Class(TCustomTestDataFlow)
  published
    procedure TestDF_ReadBeforeAssignInRepeat;
    procedure TestDF_AssignBeforeRead;
    procedure TestDF_GlobalNeverAssigned;
    procedure TestDF_ConditionalAssignSuppresses;
    procedure TestDF_VarParamIsDefinition;
    procedure TestDF_StructuredTypeNotFlagged;
    procedure TestDF_WhileConditionUse;
    procedure TestDF_ForLoopVarNotFlagged;
    procedure TestDF_AsmAssignIsDefinition;
    procedure TestDF_AsmDefinesOnlyNamedLocals;
    procedure TestDF_AsmReadIsNotAUse;
    procedure TestDF_AbsoluteVarNotTracked;
    procedure TestDF_ForInEnumerableUseFlagged;
    procedure TestDF_ForInLoopVarNotFlagged;
    procedure TestDF_TryAssignCountsAsAssignedInFinally;
    procedure TestDF_ResultsCarryVariableAndPosition;
    procedure TestDF_UnreachableStatementStillAnalysed;
    procedure TestDF_EngineForwardConverges;
    procedure TestDF_EngineForwardLoopNeedsSecondPass;
    procedure TestDF_EngineBackwardReachesEntry;
    procedure TestDF_EngineUnreachableNodeGetsState;
  end;

implementation

type

  { TDFNodeSet — a set of CFG node indices with a deterministic text form }

  TDFNodeSet = class
  private
    FBits: array of Boolean;
  public
    constructor Create(aSize: Integer);
    procedure Add(aIndex: Integer);
    procedure MergeFrom(aSource: TDFNodeSet);
    function SameAs(aOther: TDFNodeSet): Boolean;
    function AsText: string;
  end;

  { TDFTestLattice — the node indices reaching a node, in either direction }

  TDFTestLattice = class(TPasDataFlowLattice)
  private
    FDirection: TPasDataFlowDirection;
    FSize: Integer;
  public
    constructor Create(aDirection: TPasDataFlowDirection; aSize: Integer);
    function Direction: TPasDataFlowDirection; override;
    function CreateState: TObject; override;
    function CopyState(aState: TObject): TObject; override;
    procedure FreeState(aState: TObject); override;
    procedure Merge(aTarget,aSource: TObject); override;
    procedure Transfer(aNode: TPasCFGNode; aState: TObject); override;
    function SameState(aLeft,aRight: TObject): Boolean; override;
  end;

{ TDFNodeSet }

constructor TDFNodeSet.Create(aSize: Integer);
begin
  SetLength(FBits, aSize);
end;

procedure TDFNodeSet.Add(aIndex: Integer);
begin
  FBits[aIndex] := True;
end;

procedure TDFNodeSet.MergeFrom(aSource: TDFNodeSet);
var
  I: Integer;
begin
  for I := 0 to Length(FBits) - 1 do
    if aSource.FBits[I] then
      FBits[I] := True;
end;

function TDFNodeSet.SameAs(aOther: TDFNodeSet): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(FBits) - 1 do
    if FBits[I] <> aOther.FBits[I] then Exit(False);
  Result := True;
end;

function TDFNodeSet.AsText: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(FBits) - 1 do
    if FBits[I] then
    begin
      if Result <> '' then Result := Result + ',';
      Result := Result + IntToStr(I);
    end;
end;

{ TDFTestLattice }

constructor TDFTestLattice.Create(aDirection: TPasDataFlowDirection;
  aSize: Integer);
begin
  FDirection := aDirection;
  FSize := aSize;
end;

function TDFTestLattice.Direction: TPasDataFlowDirection;
begin
  Result := FDirection;
end;

function TDFTestLattice.CreateState: TObject;
begin
  Result := TDFNodeSet.Create(FSize);
end;

function TDFTestLattice.CopyState(aState: TObject): TObject;
begin
  Result := TDFNodeSet.Create(FSize);
  TDFNodeSet(Result).MergeFrom(TDFNodeSet(aState));
end;

procedure TDFTestLattice.FreeState(aState: TObject);
begin
  aState.Free;
end;

procedure TDFTestLattice.Merge(aTarget,aSource: TObject);
begin
  TDFNodeSet(aTarget).MergeFrom(TDFNodeSet(aSource));
end;

procedure TDFTestLattice.Transfer(aNode: TPasCFGNode; aState: TObject);
begin
  TDFNodeSet(aState).Add(aNode.Index);
end;

function TDFTestLattice.SameState(aLeft,aRight: TObject): Boolean;
begin
  Result := TDFNodeSet(aLeft).SameAs(TDFNodeSet(aRight));
end;

{ TTestDataFlowAnalyzer }

procedure TTestDataFlowAnalyzer.EmitMessage(MsgNumber: Integer;
  const Fmt: String; const Args: array of const; PosEl: TPasElement);
begin
  Msgs.Add(IntToStr(MsgNumber) + '|' + Format(Fmt, Args));
  if Posel=nil then ;
end;

{ TCustomTestDataFlow }

procedure TCustomTestDataFlow.SetUp;
begin
  inherited SetUp;
  FAnalyzer := TTestDataFlowAnalyzer.Create(ResolverEngine);
  FAnalyzer.Msgs := TStringList.Create;
end;

procedure TCustomTestDataFlow.TearDown;
begin
  if FAnalyzer <> nil then
    FreeAndNil(FAnalyzer.Msgs);
  FreeAndNil(FAnalyzer);
  inherited TearDown;
end;

procedure TCustomTestDataFlow.AnalyzeProgram;
begin
  ParseProgram;
  Analyzer.AnalyzeModule(Module);
end;

procedure TCustomTestDataFlow.AnalyzeUnit;
begin
  ParseUnit;
  Analyzer.AnalyzeModule(Module);
end;

procedure TCustomTestDataFlow.CheckHint(MsgNumber: Integer;
  const MsgText: string);
var
  I: Integer;
  Want: string;
begin
  Want := IntToStr(MsgNumber) + '|' + MsgText;
  for I := 0 to Analyzer.Msgs.Count - 1 do
    if Analyzer.Msgs[I] = Want then Exit;
  Fail('Data-flow hint not found: {' + Want + '}; got: [' +
    StringReplace(TrimRight(Analyzer.Msgs.Text), LineEnding, ' | ', [rfReplaceAll]) + ']');
end;

procedure TCustomTestDataFlow.CheckNoHints;
begin
  if Analyzer.Msgs.Count > 0 then
    Fail('Expected no data-flow hints, got: [' +
      StringReplace(TrimRight(Analyzer.Msgs.Text), LineEnding, ' | ', [rfReplaceAll]) + ']');
end;

function TCustomTestDataFlow.RoutineBody(const aName: string): TPasImplBlock;
var
  I: Integer;
  lDecl: TPasElement;
begin
  Result := nil;
  for I := 0 to PasProgram.ProgramSection.Declarations.Count - 1 do
  begin
    lDecl := TPasElement(PasProgram.ProgramSection.Declarations[I]);
    if (lDecl is TPasProcedure) and SameText(lDecl.Name, aName) then
    begin
      if TPasProcedure(lDecl).Body = nil then
        Fail('Routine ' + aName + ' has no body');
      Exit(TPasProcedure(lDecl).Body.Body);
    end;
  end;
  Fail('No routine named ' + aName);
end;

function TCustomTestDataFlow.EngineStates(aCFG: TPasCFG;
  aDirection: TPasDataFlowDirection): string;
var
  I: Integer;
  lEngine: TPasDataFlowEngine;
  lLattice: TDFTestLattice;
  lState: TObject;
begin
  Result := '';
  lLattice := TDFTestLattice.Create(aDirection, aCFG.NodeCount);
  try
    lEngine := TPasDataFlowEngine.Create;
    try
      lEngine.Run(aCFG, lLattice);
      for I := 0 to aCFG.NodeCount - 1 do
      begin
        if I > 0 then Result := Result + ' ';
        lState := lEngine.StateOf(aCFG.Nodes[I]);
        AssertNotNull('state of node ' + IntToStr(I), lState);
        Result := Result + IntToStr(I) + '={' + TDFNodeSet(lState).AsText + '}';
      end;
    finally
      lEngine.Free;
    end;
  finally
    lLattice.Free;
  end;
end;

{ TTestDataFlow }

procedure TTestDataFlow.TestDF_ReadBeforeAssignInRepeat;
begin
  StartProgram(false);
  Add([
  'procedure p;',
  'var',
  '  counter: longint;',
  '  c1: longint;',
  'begin',
  '  repeat',
  '    c1 := counter;',    // counter read before it is assigned
  '    counter := 15;',
  '  until counter >= 10;',
  'end;',
  'begin']);
  AnalyzeProgram;
  CheckHint(nUninitializedVariable,
    'Variable "counter" does not seem to be initialized');
end;

procedure TTestDataFlow.TestDF_AssignBeforeRead;
begin
  StartProgram(false);
  Add([
  'procedure p;',
  'var',
  '  counter: longint;',
  '  c1: longint;',
  'begin',
  '  counter := 0;',
  '  c1 := counter;',
  'end;',
  'begin']);
  AnalyzeProgram;
  CheckNoHints;
end;

procedure TTestDataFlow.TestDF_GlobalNeverAssigned;
begin
  StartProgram(false);
  Add([
  'var',
  '  j, i: longint;',
  'begin',
  '  j := 1;',
  '  if (j = 1) and (i = 0) then j := 2;']); // i never assigned
  AnalyzeProgram;
  CheckHint(nUninitializedVariable,
    'Variable "i" does not seem to be initialized');
end;

procedure TTestDataFlow.TestDF_ConditionalAssignSuppresses;
begin
  // Assigned in one branch before use: the optimistic analysis must not flag it.
  StartProgram(false);
  Add([
  'var',
  '  j, i: longint;',
  'begin',
  '  j := 1;',
  '  if j = 1 then i := 1;',
  '  if j = 1 then i := i + 1;']);
  AnalyzeProgram;
  CheckNoHints;
end;

procedure TTestDataFlow.TestDF_VarParamIsDefinition;
begin
  // Passing to a var parameter counts as a definition.
  StartProgram(false);
  Add([
  'procedure init(var x: longint);',
  'begin',
  '  x := 0;',
  'end;',
  'procedure p;',
  'var',
  '  i: longint;',
  'begin',
  '  init(i);',
  '  if i = 0 then i := 1;',
  'end;',
  'begin']);
  AnalyzeProgram;
  CheckNoHints;
end;

procedure TTestDataFlow.TestDF_StructuredTypeNotFlagged;
begin
  // A record-typed variable is never flagged, even read before assignment.
  StartProgram(false);
  Add([
  'type',
  '  TRec = record a: longint; end;',
  'var',
  '  r, s: TRec;',
  'begin',
  '  s := r;']);
  AnalyzeProgram;
  CheckNoHints;
end;

procedure TTestDataFlow.TestDF_WhileConditionUse;
begin
  // Use in a while condition, before any assignment, is flagged.
  StartProgram(false);
  Add([
  'procedure p;',
  'var',
  '  i: longint;',
  'begin',
  '  while i > 0 do i := 0;',
  'end;',
  'begin']);
  AnalyzeProgram;
  CheckHint(nUninitializedVariable,
    'Variable "i" does not seem to be initialized');
end;

procedure TTestDataFlow.TestDF_ForLoopVarNotFlagged;
begin
  // The for-loop control variable is defined by the loop header.
  StartProgram(false);
  Add([
  'procedure p;',
  'var',
  '  i, s: longint;',
  'begin',
  '  s := 0;',
  '  for i := 1 to 10 do s := s + i;',
  'end;',
  'begin']);
  AnalyzeProgram;
  CheckNoHints;
end;

procedure TTestDataFlow.TestDF_AsmAssignIsDefinition;
begin
  // A local mentioned in an asm block counts as defined.
  StartProgram(false);
  Add([
  'procedure p;',
  'var',
  '  i, j: longint;',
  'begin',
  '  asm',
  '    mov i, 1',
  '  end;',
  '  j := i;',
  'end;',
  'begin']);
  AnalyzeProgram;
  CheckNoHints;
end;

procedure TTestDataFlow.TestDF_AsmDefinesOnlyNamedLocals;
begin
  // A local the asm block does not name keeps its unassigned state.
  StartProgram(false);
  Add([
  'procedure p;',
  'var',
  '  i, j, k: longint;',
  'begin',
  '  asm',
  '    mov i, 1',
  '  end;',
  '  j := i + k;',
  'end;',
  'begin']);
  AnalyzeProgram;
  CheckHint(nUninitializedVariable,
    'Variable "k" does not seem to be initialized');
end;

procedure TTestDataFlow.TestDF_AsmReadIsNotAUse;
begin
  // An asm mention is never a use.
  StartProgram(false);
  Add([
  'procedure p;',
  'var',
  '  i: longint;',
  'begin',
  '  asm',
  '    mov eax, i',
  '  end;',
  'end;',
  'begin']);
  AnalyzeProgram;
  CheckNoHints;
end;

procedure TTestDataFlow.TestDF_AbsoluteVarNotTracked;
begin
  // A variable declared absolute another one has no storage of its own.
  StartProgram(false);
  Add([
  'var',
  '  a: longint;',
  '  b: longint absolute a;',
  '  c: longint;',
  'begin',
  '  a := 1;',
  '  c := b;']);
  AnalyzeProgram;
  CheckNoHints;
end;

procedure TTestDataFlow.TestDF_ForInEnumerableUseFlagged;
begin
  StartProgram(false);
  Add([
  'var',
  '  i, n, s: longint;',
  'begin',
  '  s := 0;',
  '  for i in [1..n] do s := s + i;']);
  AnalyzeProgram;
  CheckHint(nUninitializedVariable,
    'Variable "n" does not seem to be initialized');
end;

procedure TTestDataFlow.TestDF_ForInLoopVarNotFlagged;
begin
  // The for..in control variable is defined by the loop header.
  StartProgram(false);
  Add([
  'var',
  '  i, s: longint;',
  'begin',
  '  s := 0;',
  '  for i in [1,2] do s := s + i;']);
  AnalyzeProgram;
  CheckNoHints;
end;

procedure TTestDataFlow.TestDF_TryAssignCountsAsAssignedInFinally;
begin
  // finally is walked after the try body.
  StartProgram(false);
  Add([
  'procedure p;',
  'var',
  '  i, j: longint;',
  'begin',
  '  try',
  '    i := 1;',
  '  finally',
  '    j := i;',
  '  end;',
  'end;',
  'begin']);
  AnalyzeProgram;
  CheckNoHints;
end;

procedure TTestDataFlow.TestDF_ResultsCarryVariableAndPosition;
var
  R: TPasDataFlowResult;
begin
  // The result channel is filled independently of EmitMessage.
  StartProgram(false);
  Add([
  'procedure p;',
  'var',
  '  counter: longint;',
  '  c1: longint;',
  'begin',
  '  c1 := counter;',
  'end;',
  'begin']);
  AnalyzeProgram;
  AssertEquals('ResultCount', 1, Analyzer.ResultCount);
  R := Analyzer.Results[0];
  AssertNotNull('Results[0].Variable', R.Variable);
  AssertEquals('Results[0].Variable.Name', 'counter', R.Variable.Name);
  AssertNotNull('Results[0].PosEl', R.PosEl);
  AssertTrue('Results[0].PosEl is TPrimitiveExpr, got ' + R.PosEl.ClassName,
    R.PosEl is TPrimitiveExpr);
  AssertEquals('Results[0].PosEl value', 'counter',
    TPrimitiveExpr(R.PosEl).Value);
  // A second run reports the same finding once, not twice.
  Analyzer.AnalyzeModule(Module);
  AssertEquals('ResultCount after re-analysis', 1, Analyzer.ResultCount);
end;

procedure TTestDataFlow.TestDF_UnreachableStatementStillAnalysed;
begin
  // The statement after an exit is in a node the entry cannot reach.
  StartProgram(false);
  Add([
  'procedure p;',
  'var',
  '  counter: longint;',
  '  c1: longint;',
  'begin',
  '  exit;',
  '  c1 := counter;',
  'end;',
  'begin']);
  AnalyzeProgram;
  CheckHint(nUninitializedVariable,
    'Variable "counter" does not seem to be initialized');
end;

procedure TTestDataFlow.TestDF_EngineForwardConverges;
var
  lCFG: TPasCFG;
begin
  StartProgram(false);
  Add([
  'procedure Alpha;',
  'var',
  '  i: longint;',
  'begin',
  '  if i > 0 then i := 1 else i := 2;',
  'end;',
  'begin']);
  ParseProgram;
  lCFG := TPasCFG.Create(RoutineBody('Alpha'));
  try
    AssertEquals('forward states of an if/else',
      '0={0} 1={0,1} 2={0,1,2} 3={0,1,3} 4={0,1,2,3,4} 5={0,1,2,3,4,5}',
      EngineStates(lCFG, dfdForward));
  finally
    lCFG.Free;
  end;
end;

procedure TTestDataFlow.TestDF_EngineForwardLoopNeedsSecondPass;
var
  lCFG: TPasCFG;
begin
  // The loop header only learns of its back edge after the body is transferred.
  StartProgram(false);
  Add([
  'procedure Alpha;',
  'var',
  '  i: longint;',
  'begin',
  '  while i > 0 do i := 1;',
  'end;',
  'begin']);
  ParseProgram;
  lCFG := TPasCFG.Create(RoutineBody('Alpha'));
  try
    AssertEquals('forward states of a while loop',
      '0={0} 1={0,1} 2={0,1,2,3} 3={0,1,2,3} 4={0,1,2,3,4} 5={0,1,2,3,4,5}',
      EngineStates(lCFG, dfdForward));
  finally
    lCFG.Free;
  end;
end;

procedure TTestDataFlow.TestDF_EngineBackwardReachesEntry;
var
  lCFG: TPasCFG;
begin
  StartProgram(false);
  Add([
  'procedure Alpha;',
  'var',
  '  i: longint;',
  'begin',
  '  if i > 0 then i := 1 else i := 2;',
  'end;',
  'begin']);
  ParseProgram;
  lCFG := TPasCFG.Create(RoutineBody('Alpha'));
  try
    AssertEquals('backward states of an if/else',
      '0={0,1,2,3,4,5} 1={1,2,3,4,5} 2={2,4,5} 3={3,4,5} 4={4,5} 5={5}',
      EngineStates(lCFG, dfdBackward));
  finally
    lCFG.Free;
  end;
end;

procedure TTestDataFlow.TestDF_EngineUnreachableNodeGetsState;
var
  lCFG: TPasCFG;
begin
  StartProgram(false);
  Add([
  'procedure Alpha;',
  'var',
  '  i: longint;',
  'begin',
  '  exit;',
  '  i := 1;',
  'end;',
  'begin']);
  ParseProgram;
  lCFG := TPasCFG.Create(RoutineBody('Alpha'));
  try
    AssertFalse('node 3 is unreachable', lCFG.Reachable(lCFG.Nodes[3]));
    AssertEquals('the unreachable node is transferred too',
      '0={0} 1={0,1} 2={0,1,2,3} 3={3}',
      EngineStates(lCFG, dfdForward));
  finally
    lCFG.Free;
  end;
end;

initialization
  RegisterTests([TTestDataFlow]);

end.
