{
  FPCUnit tests for PasDataFlow (uninitialized-variable analysis).
}
unit tcpasdfa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasTree, PScanner, PasResolver, PasResolveEval,
  tcbaseparser, tcresolver, PasDataFlow;

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
  end;

implementation

{ TTestDataFlowAnalyzer }

procedure TTestDataFlowAnalyzer.EmitMessage(MsgNumber: Integer;
  const Fmt: String; const Args: array of const; PosEl: TPasElement);
begin
  Msgs.Add(IntToStr(MsgNumber) + '|' + Format(Fmt, Args));
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

initialization
  RegisterTests([TTestDataFlow]);

end.
