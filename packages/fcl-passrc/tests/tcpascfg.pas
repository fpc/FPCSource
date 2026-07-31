{
  FPCUnit tests for PasCFG (control-flow graph and its textual dump).
}
unit tcpascfg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasTree, pparser, tcbaseparser, PasCFG;

type

  { TTestCFG }

  TTestCFG = class(TTestParser)
  private
    procedure AssertDump(const aMessage: string; const aSource: array of string;
      const aExpected: array of string);
    function BodyOf(const aName: string): TPasImplBlock;
    procedure CollectCoveredClasses(aCovered: TStrings);
    function DeclaredDescendants: TStringList;
    function Dump(const aLines: array of string): string;
    procedure ParseCoverageFixtures;
    procedure ParseRoutines(const aLines: array of string);
    procedure ParseTwoRoutines;
  published
    procedure TestCFG_EmptyBody;
    procedure TestCFG_NilBody;
    procedure TestCFG_AsmBodyIsOneStatement;
    procedure TestCFG_StraightLine;
    procedure TestCFG_IdenticalRoutinesAreByteIdentical;
    procedure TestCFG_RepeatedSerialisationIsStable;
    procedure TestCFG_NoAddressAndNoPositionByDefault;
    procedure TestCFG_SourcePositionsOption;
    procedure TestCFG_ReachableAndNodeOf;
    procedure TestCFG_NodeOfForeignElementIsNil;
    procedure TestCFG_IfWithoutElse;
    procedure TestCFG_IfElse;
    procedure TestCFG_IfBothBranchesTerminate;
    procedure TestCFG_CaseWithElse;
    procedure TestCFG_CaseWithoutElse;
    procedure TestCFG_WhileDo;
    procedure TestCFG_RepeatUntil;
    procedure TestCFG_ForTo;
    procedure TestCFG_ForIn;
    procedure TestCFG_WithDoDoesNotBranch;
    procedure TestCFG_BreakInLoop;
    procedure TestCFG_ContinueInLoop;
    procedure TestCFG_ContinueInRepeat;
    procedure TestCFG_BreakOutsideLoop;
    procedure TestCFG_Exit;
    procedure TestCFG_ExitWithValue;
    procedure TestCFG_Halt;
    procedure TestCFG_GotoAndLabel;
    procedure TestCFG_GotoUndefinedLabel;
    procedure TestCFG_UnreachableBlock;
    procedure TestCFG_NestedLoopsBreakTargetsInner;
    procedure TestCFG_ContinueAfterNestedLoop;
    procedure TestCFG_TryFinallyEdgesFromEveryStatement;
    procedure TestCFG_BranchInsideProtectedBody;
    procedure TestCFG_GotoOutOfTryBodyOrdersEdges;
    procedure TestCFG_TryExceptEdgesFromEveryStatement;
    procedure TestCFG_TryExceptOnHandlersWithElse;
    procedure TestCFG_TryExceptOnHandlersWithoutElse;
    procedure TestCFG_TryExceptOnHandlersWithoutElseInsideOuterTry;
    procedure TestCFG_NestedTryFinally;
    procedure TestCFG_StatementInsideTryHasANode;
    procedure TestCFG_RaiseInProtectedBody;
    procedure TestCFG_BareRaiseInHandler;
    procedure TestCFG_BareRaiseInNestedHandler;
    procedure TestCFG_RaiseOutsideTry;
    procedure TestCFG_RaiseAsBranch;
    procedure TestCFG_ExitInsideTryFinally;
    procedure TestCFG_GotoIntoTryBody;
    procedure TestCFG_OnHandlerWithEmptyBody;
    procedure TestCFG_EveryImplElementDescendantIsCovered;
  end;

implementation

type
  TExemptImplElement = record
    ClassName: string;
    Reason: string;
  end;

const
  cImplElementRoot = 'TPasImplElement';
  cStatementPrefix = '  stmt ';

  cExemptImplElements: array[0..7] of TExemptImplElement = (
    (ClassName: 'TPasImplCommandBase'; Reason: 'base class the parser never instantiates'),
    (ClassName: 'TPasImplBlock';       Reason: 'base class the parser never instantiates'),
    (ClassName: 'TPasImplStatement';   Reason: 'base class the parser never instantiates'),
    (ClassName: 'TPasImplTryHandler';  Reason: 'base class the parser never instantiates'),
    (ClassName: 'TPasImplCommands';    Reason: 'AddCommands has no caller in this tree'),
    (ClassName: 'TPasLabels';          Reason: 'ParseLabels files it under the routine declarations, never in a body'),
    (ClassName: 'TInitializationSection'; Reason: 'module section: a CFG root, never a statement inside a body'),
    (ClassName: 'TFinalizationSection';   Reason: 'module section: a CFG root, never a statement inside a body')
  );

{ TTestCFG }

procedure TTestCFG.AssertDump(const aMessage: string; const aSource: array of string;
  const aExpected: array of string);

var
  lCFG: TPasCFG;

begin
  ParseRoutines(aSource);
  lCFG:=TPasCFG.Create(BodyOf('Alpha'));
  try
    AssertEquals(aMessage,Dump(aExpected),lCFG.AsText([ctoSourcePositions]));
  finally
    lCFG.Free;
  end;
end;


function TTestCFG.BodyOf(const aName: string): TPasImplBlock;

var
  I: Integer;
  lDecl: TPasElement;

begin
  Result:=Nil;
  for I:=0 to PasProgram.ProgramSection.Declarations.Count-1 do
    begin
    lDecl:=TPasElement(PasProgram.ProgramSection.Declarations[I]);
    if (lDecl is TPasProcedure) and SameText(lDecl.Name,aName) then
      Exit(TPasProcedure(lDecl).Body.Body);
    end;
  Fail('No routine named '+aName);
end;


procedure TTestCFG.CollectCoveredClasses(aCovered: TStrings);

var
  I,J: Integer;
  lDecl: TPasElement;
  lCFG: TPasCFG;
  lText: TStringList;
  lName: string;

begin
  lText:=TStringList.Create;
  try
    for I:=0 to PasProgram.ProgramSection.Declarations.Count-1 do
      begin
      lDecl:=TPasElement(PasProgram.ProgramSection.Declarations[I]);
      if not (lDecl is TPasProcedure) then
        Continue;
      lCFG:=TPasCFG.Create(TPasProcedure(lDecl).Body.Body);
      try
        lText.Text:=lCFG.AsText([]);
      finally
        lCFG.Free;
      end;
      for J:=0 to lText.Count-1 do
        if Copy(lText[J],1,Length(cStatementPrefix))=cStatementPrefix then
          begin
          lName:=Copy(lText[J],Length(cStatementPrefix)+1,Length(lText[J]));
          if aCovered.IndexOf(lName)<0 then
            aCovered.Add(lName);
          end;
      end;
  finally
    lText.Free;
  end;
end;


function TTestCFG.DeclaredDescendants: TStringList;

var
  I: Integer;
  lPath,lAncestor: string;
  lEngine: TTestEngine;
  lModule: TPasModule;
  lClass: TPasClassType;
  lPairs: TStringList;
  lGrown: Boolean;

begin
  lPath:=ExpandFileName('..'+DirectorySeparator+'src'+DirectorySeparator+'pastree.pp');
  if not FileExists(lPath) then
    Fail('The syntax tree unit is not readable at '+lPath);
  Result:=TStringList.Create;
  try
    lPairs:=TStringList.Create;
    try
      lEngine:=TTestEngine.Create;
      try
        lModule:=ParseSource(lEngine,[lPath],{$I %FPCTARGETOS%},{$I %FPCTARGETCPU%},[]);
        for I:=0 to lModule.InterfaceSection.Classes.Count-1 do
          begin
          if not (TObject(lModule.InterfaceSection.Classes[I]) is TPasClassType) then
            Continue;
          lClass:=TPasClassType(lModule.InterfaceSection.Classes[I]);
          if lClass.IsForward or (lClass.AncestorType=Nil) then
            Continue;
          if lClass.AncestorType.Name='' then
            Fail(lClass.Name+' has an ancestor with no simple name, so its descendants cannot be derived');
          lPairs.Values[lClass.Name]:=lClass.AncestorType.Name;
          end;
      finally
        lEngine.Free;
      end;
      repeat
        lGrown:=False;
        for I:=0 to lPairs.Count-1 do
          begin
          lAncestor:=lPairs.ValueFromIndex[I];
          if (Result.IndexOf(lPairs.Names[I])<0)
             and (SameText(lAncestor,cImplElementRoot) or (Result.IndexOf(lAncestor)>=0)) then
            begin
            Result.Add(lPairs.Names[I]);
            lGrown:=True;
            end;
          end;
      until not lGrown;
    finally
      lPairs.Free;
    end;
    Result.Sort;
  except
    Result.Free;
    raise;
  end;
end;


function TTestCFG.Dump(const aLines: array of string): string;

var
  I: Integer;

begin
  Result:='';
  for I:=Low(aLines) to High(aLines) do
    Result:=Result+aLines[I]+LineEnding;
end;


procedure TTestCFG.ParseCoverageFixtures;

begin
  ParseRoutines([
    '{$goto on}',
    '{$modeswitch inlinevars}',
    'procedure Straight;',
    'begin',
    '  DoOne;',
    '  a:=1;',
    'end;',
    'procedure Nested;',
    'begin',
    '  begin',
    '  DoOne;',
    '  end;',
    'end;',
    'procedure Branch;',
    'begin',
    '  if c then',
    '    A',
    '  else',
    '    B;',
    // An omitted then branch is the only construct that yields a TPasImplCommand.
    '  if c then else B;',
    'end;',
    'procedure Selection;',
    'begin',
    '  case x of',
    '  1:',
    '    A;',
    '  else',
    '    B;',
    '  end;',
    'end;',
    'procedure Loops;',
    'begin',
    '  while c do',
    '    A;',
    '  repeat',
    '    B;',
    '  until c;',
    '  for i:=1 to 3 do',
    '    C;',
    'end;',
    'procedure Scoped;',
    'begin',
    '  with r do',
    '    A;',
    'end;',
    'procedure InlineVar;',
    'begin',
    '  var v := 1;',
    'end;',
    'procedure Jump;',
    'label L;',
    'begin',
    '  goto L;',
    '  L:',
    '  A;',
    'end;',
    'procedure Protect;',
    'begin',
    '  try',
    '    A;',
    '  finally',
    '    F;',
    '  end;',
    '  try',
    '    B;',
    '  except',
    '    on E1 do C;',
    '  else',
    '    D;',
    '  end;',
    'end;',
    'procedure Throw;',
    'begin',
    '  raise E;',
    'end;',
    'procedure Machine; assembler;',
    'asm',
    '  nop',
    'end;']);
  AssertErrorCount('coverage fixtures parse cleanly',0);
end;


procedure TTestCFG.ParseRoutines(const aLines: array of string);

begin
  StartProgram('');
  Add(aLines);
  Add('begin');
  ParseModule;
end;


procedure TTestCFG.ParseTwoRoutines;

begin
  ParseRoutines([
    'procedure Alpha;',
    'begin',
    '  DoOne;',
    '  a:=1;',
    'end;',
    'procedure Beta;',
    'begin',
    '  DoOne;',
    '  a:=1;',
    'end;']);
end;


procedure TTestCFG.TestCFG_EmptyBody;

var
  lCFG: TPasCFG;

begin
  ParseRoutines(['procedure Alpha;','begin','end;']);
  lCFG:=TPasCFG.Create(BodyOf('Alpha'));
  try
    AssertEquals('empty body',Dump([
      'block 0 entry',
      '  succ 1',
      'block 1 exit']),lCFG.AsText([]));
  finally
    lCFG.Free;
  end;
end;


procedure TTestCFG.TestCFG_NilBody;

var
  lCFG: TPasCFG;

begin
  lCFG:=TPasCFG.Create(Nil);
  try
    AssertEquals('nil body',Dump([
      'block 0 entry',
      '  succ 1',
      'block 1 exit']),lCFG.AsText([]));
  finally
    lCFG.Free;
  end;
end;


procedure TTestCFG.TestCFG_AsmBodyIsOneStatement;

var
  lCFG: TPasCFG;

begin
  ParseRoutines(['procedure Alpha; assembler;','asm','  nop','end;']);
  lCFG:=TPasCFG.Create(BodyOf('Alpha'));
  try
    AssertEquals('asm body',Dump([
      'block 0 entry',
      '  succ 1',
      'block 1',
      '  stmt TPasImplAsmStatement',
      '  succ 2',
      'block 2 exit']),lCFG.AsText([]));
  finally
    lCFG.Free;
  end;
end;


procedure TTestCFG.TestCFG_StraightLine;

var
  lCFG: TPasCFG;

begin
  ParseTwoRoutines;
  lCFG:=TPasCFG.Create(BodyOf('Alpha'));
  try
    AssertEquals('straight-line body',Dump([
      'block 0 entry',
      '  succ 1',
      'block 1',
      '  stmt TPasImplSimple',
      '  stmt TPasImplAssign',
      '  succ 2',
      'block 2 exit']),lCFG.AsText([]));
  finally
    lCFG.Free;
  end;
end;


procedure TTestCFG.TestCFG_IdenticalRoutinesAreByteIdentical;

var
  lAlpha,lBeta: TPasCFG;

begin
  ParseTwoRoutines;
  lAlpha:=TPasCFG.Create(BodyOf('Alpha'));
  try
    lBeta:=TPasCFG.Create(BodyOf('Beta'));
    try
      AssertEquals('identical routines',lAlpha.AsText([]),lBeta.AsText([]));
    finally
      lBeta.Free;
    end;
  finally
    lAlpha.Free;
  end;
end;


procedure TTestCFG.TestCFG_RepeatedSerialisationIsStable;

var
  lFirst,lSecond: TPasCFG;
  lBody: TPasImplBlock;
  lText: string;

begin
  ParseTwoRoutines;
  lBody:=BodyOf('Alpha');
  lFirst:=TPasCFG.Create(lBody);
  try
    lSecond:=TPasCFG.Create(lBody);
    try
      lText:=lFirst.AsText([]);
      AssertEquals('first graph, second call',lText,lFirst.AsText([]));
      AssertEquals('second graph, first call',lText,lSecond.AsText([]));
      AssertEquals('second graph, second call',lText,lSecond.AsText([]));
    finally
      lSecond.Free;
    end;
  finally
    lFirst.Free;
  end;
end;


procedure TTestCFG.TestCFG_NoAddressAndNoPositionByDefault;

var
  lCFG: TPasCFG;
  lText: string;

begin
  ParseTwoRoutines;
  lCFG:=TPasCFG.Create(BodyOf('Alpha'));
  try
    lText:=lCFG.AsText([]);
    AssertEquals('no address in '+lText,0,Pos('$',lText));
    AssertEquals('no position in '+lText,0,Pos('@',lText));
  finally
    lCFG.Free;
  end;
end;


procedure TTestCFG.TestCFG_SourcePositionsOption;

var
  lAlpha,lBeta: TPasCFG;

begin
  ParseTwoRoutines;
  lAlpha:=TPasCFG.Create(BodyOf('Alpha'));
  try
    lBeta:=TPasCFG.Create(BodyOf('Beta'));
    try
      AssertEquals('positions requested',Dump([
        'block 0 entry',
        '  succ 1',
        'block 1',
        '  stmt TPasImplSimple@4',
        '  stmt TPasImplAssign@5',
        '  succ 2',
        'block 2 exit']),lAlpha.AsText([ctoSourcePositions]));
      AssertTrue('positions separate the two routines',
        lAlpha.AsText([ctoSourcePositions])<>lBeta.AsText([ctoSourcePositions]));
    finally
      lBeta.Free;
    end;
  finally
    lAlpha.Free;
  end;
end;


procedure TTestCFG.TestCFG_ReachableAndNodeOf;

var
  lCFG: TPasCFG;
  lBody: TPasImplBlock;
  lNode: TPasCFGNode;

begin
  ParseTwoRoutines;
  lBody:=BodyOf('Alpha');
  lCFG:=TPasCFG.Create(lBody);
  try
    lNode:=lCFG.NodeOf(TPasElement(lBody.Elements[0]));
    AssertNotNull('first statement has a node',lNode);
    AssertSame('both statements share the node',lNode,
      lCFG.NodeOf(TPasElement(lBody.Elements[1])));
    AssertTrue('entry reachable',lCFG.Reachable(lCFG.EntryNode));
    AssertTrue('body reachable',lCFG.Reachable(lNode));
    AssertTrue('exit reachable',lCFG.Reachable(lCFG.ExitNode));
  finally
    lCFG.Free;
  end;
end;


procedure TTestCFG.TestCFG_NodeOfForeignElementIsNil;

var
  lCFG: TPasCFG;

begin
  ParseTwoRoutines;
  lCFG:=TPasCFG.Create(BodyOf('Alpha'));
  try
    AssertNull('statement of another body',
      lCFG.NodeOf(TPasElement(BodyOf('Beta').Elements[0])));
    AssertFalse('nil node',lCFG.Reachable(Nil));
  finally
    lCFG.Free;
  end;
end;


procedure TTestCFG.TestCFG_IfWithoutElse;

begin
  AssertDump('if without else',[
    'procedure Alpha;',
    'begin',
    '  if c then',
    '    A;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplIfElse@4',
    '  succ 2',
    '  succ 3',
    'block 2',
    '  stmt TPasImplSimple@5',
    '  succ 3',
    'block 3',
    '  stmt TPasImplSimple@6',
    '  succ 4',
    'block 4 exit']);
end;


procedure TTestCFG.TestCFG_IfElse;

begin
  AssertDump('if with else',[
    'procedure Alpha;',
    'begin',
    '  if c then',
    '    A',
    '  else',
    '    B;',
    '  C;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplIfElse@4',
    '  succ 2',
    '  succ 3',
    'block 2',
    '  stmt TPasImplSimple@5',
    '  succ 4',
    'block 3',
    '  stmt TPasImplSimple@7',
    '  succ 4',
    'block 4',
    '  stmt TPasImplSimple@8',
    '  succ 5',
    'block 5 exit']);
end;


procedure TTestCFG.TestCFG_IfBothBranchesTerminate;

begin
  AssertDump('both branches exit, so no join',[
    'procedure Alpha;',
    'begin',
    '  if c then',
    '    Exit',
    '  else',
    '    Exit;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplIfElse@4',
    '  succ 2',
    '  succ 3',
    'block 2',
    '  stmt TPasImplSimple@5',
    '  succ 4',
    'block 3',
    '  stmt TPasImplSimple@7',
    '  succ 4',
    'block 4 exit']);
end;


procedure TTestCFG.TestCFG_CaseWithElse;

begin
  AssertDump('case with else',[
    'procedure Alpha;',
    'begin',
    '  case x of',
    '  1:',
    '    A;',
    '  2:',
    '    B;',
    '  else',
    '    C;',
    '  end;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplCaseOf@4',
    '  succ 2',
    '  succ 3',
    '  succ 4',
    'block 2',
    '  stmt TPasImplCaseStatement@5',
    '  stmt TPasImplSimple@6',
    '  succ 5',
    'block 3',
    '  stmt TPasImplCaseStatement@7',
    '  stmt TPasImplSimple@8',
    '  succ 5',
    'block 4',
    '  stmt TPasImplCaseElse@9',
    '  stmt TPasImplSimple@10',
    '  succ 5',
    'block 5',
    '  succ 6',
    'block 6 exit']);
end;


procedure TTestCFG.TestCFG_CaseWithoutElse;

begin
  AssertDump('case without else falls through when no label matches',[
    'procedure Alpha;',
    'begin',
    '  case x of',
    '  1:',
    '    A;',
    '  end;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplCaseOf@4',
    '  succ 2',
    '  succ 3',
    'block 2',
    '  stmt TPasImplCaseStatement@5',
    '  stmt TPasImplSimple@6',
    '  succ 3',
    'block 3',
    '  stmt TPasImplSimple@8',
    '  succ 4',
    'block 4 exit']);
end;


procedure TTestCFG.TestCFG_WhileDo;

begin
  AssertDump('while loop',[
    'procedure Alpha;',
    'begin',
    '  while c do',
    '    A;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  succ 2',
    'block 2',
    '  stmt TPasImplWhileDo@4',
    '  succ 3',
    '  succ 4',
    'block 3',
    '  stmt TPasImplSimple@5',
    '  succ 2',
    'block 4',
    '  stmt TPasImplSimple@6',
    '  succ 5',
    'block 5 exit']);
end;


procedure TTestCFG.TestCFG_RepeatUntil;

begin
  AssertDump('repeat loop',[
    'procedure Alpha;',
    'begin',
    '  repeat',
    '    A;',
    '  until c;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  succ 2',
    'block 2',
    '  stmt TPasImplSimple@5',
    '  succ 3',
    'block 3',
    '  stmt TPasImplRepeatUntil@4',
    '  succ 4',
    '  succ 2',
    'block 4',
    '  stmt TPasImplSimple@7',
    '  succ 5',
    'block 5 exit']);
end;


procedure TTestCFG.TestCFG_ForTo;

begin
  AssertDump('for loop',[
    'procedure Alpha;',
    'begin',
    '  for i:=1 to 3 do',
    '    A;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  succ 2',
    'block 2',
    '  stmt TPasImplForLoop@4',
    '  succ 3',
    '  succ 4',
    'block 3',
    '  stmt TPasImplSimple@5',
    '  succ 2',
    'block 4',
    '  stmt TPasImplSimple@6',
    '  succ 5',
    'block 5 exit']);
end;


procedure TTestCFG.TestCFG_ForIn;

begin
  AssertDump('for..in loop',[
    'procedure Alpha;',
    'begin',
    '  for i in a do',
    '    A;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  succ 2',
    'block 2',
    '  stmt TPasImplForLoop@4',
    '  succ 3',
    '  succ 4',
    'block 3',
    '  stmt TPasImplSimple@5',
    '  succ 2',
    'block 4',
    '  stmt TPasImplSimple@6',
    '  succ 5',
    'block 5 exit']);
end;


procedure TTestCFG.TestCFG_WithDoDoesNotBranch;

begin
  AssertDump('with does not branch',[
    'procedure Alpha;',
    'begin',
    '  with r do',
    '    A;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplWithDo@4',
    '  stmt TPasImplSimple@5',
    '  stmt TPasImplSimple@6',
    '  succ 2',
    'block 2 exit']);
end;


procedure TTestCFG.TestCFG_BreakInLoop;

begin
  AssertDump('break leaves the loop',[
    'procedure Alpha;',
    'begin',
    '  while c do',
    '    begin',
    '    Break;',
    '    end;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  succ 2',
    'block 2',
    '  stmt TPasImplWhileDo@4',
    '  succ 3',
    '  succ 4',
    'block 3',
    '  stmt TPasImplBeginBlock@5',
    '  stmt TPasImplSimple@6',
    '  succ 4',
    'block 4',
    '  stmt TPasImplSimple@8',
    '  succ 5',
    'block 5 exit']);
end;


procedure TTestCFG.TestCFG_ContinueInLoop;

begin
  AssertDump('continue returns to the header, leaving the rest of the body unreached',[
    'procedure Alpha;',
    'begin',
    '  while c do',
    '    begin',
    '    Continue;',
    '    A;',
    '    end;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  succ 2',
    'block 2',
    '  stmt TPasImplWhileDo@4',
    '  succ 3',
    '  succ 4',
    'block 3',
    '  stmt TPasImplBeginBlock@5',
    '  stmt TPasImplSimple@6',
    '  succ 2',
    'block 4',
    '  stmt TPasImplSimple@9',
    '  succ 5',
    'block 5 exit',
    'block 6',
    '  stmt TPasImplSimple@7',
    '  succ 2']);
end;


procedure TTestCFG.TestCFG_ContinueInRepeat;

begin
  AssertDump('continue in a repeat returns to the until condition',[
    'procedure Alpha;',
    'begin',
    '  repeat',
    '    Continue;',
    '    A;',
    '  until c;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  succ 2',
    'block 2',
    '  stmt TPasImplSimple@5',
    '  succ 3',
    'block 3',
    '  stmt TPasImplRepeatUntil@4',
    '  succ 4',
    '  succ 2',
    'block 4',
    '  stmt TPasImplSimple@8',
    '  succ 5',
    'block 5 exit',
    'block 6',
    '  stmt TPasImplSimple@6',
    '  succ 3']);
end;


procedure TTestCFG.TestCFG_BreakOutsideLoop;

begin
  AssertDump('break with no enclosing loop is a plain statement',[
    'procedure Alpha;',
    'begin',
    '  Break;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplSimple@4',
    '  stmt TPasImplSimple@5',
    '  succ 2',
    'block 2 exit']);
end;


procedure TTestCFG.TestCFG_Exit;

begin
  AssertDump('exit reaches the exit node, not the loop it stands in',[
    'procedure Alpha;',
    'begin',
    '  while c do',
    '    Exit;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  succ 2',
    'block 2',
    '  stmt TPasImplWhileDo@4',
    '  succ 3',
    '  succ 4',
    'block 3',
    '  stmt TPasImplSimple@5',
    '  succ 5',
    'block 4',
    '  stmt TPasImplSimple@6',
    '  succ 5',
    'block 5 exit']);
end;


procedure TTestCFG.TestCFG_ExitWithValue;

begin
  AssertDump('exit with a value reaches the exit node and ends flow',[
    'function Alpha: Integer;',
    'begin',
    '  Exit(1);',
    '  A;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplSimple@4',
    '  succ 2',
    'block 2 exit',
    'block 3',
    '  stmt TPasImplSimple@5',
    '  succ 2']);
end;


procedure TTestCFG.TestCFG_Halt;

begin
  AssertDump('halt leaves the exit node unreached',[
    'procedure Alpha;',
    'begin',
    '  Halt;',
    '  A;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplSimple@4',
    'block 2 exit',
    'block 3',
    '  stmt TPasImplSimple@5',
    '  succ 2']);
end;


procedure TTestCFG.TestCFG_GotoAndLabel;

begin
  AssertDump('goto reaches the label block',[
    '{$goto on}',
    'procedure Alpha;',
    'label L;',
    'begin',
    '  goto L;',
    '  A;',
    '  L:',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplGoto@6',
    '  succ 2',
    'block 2',
    '  stmt TPasImplLabelMark@8',
    '  stmt TPasImplSimple@9',
    '  succ 3',
    'block 3 exit',
    'block 4',
    '  stmt TPasImplSimple@7',
    '  succ 2']);
end;


procedure TTestCFG.TestCFG_GotoUndefinedLabel;

begin
  AssertDump('goto to a label that is never marked ends flow',[
    '{$goto on}',
    'procedure Alpha;',
    'label L;',
    'begin',
    '  goto L;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplGoto@6',
    'block 2 exit']);
end;


procedure TTestCFG.TestCFG_UnreachableBlock;

var
  lCFG: TPasCFG;
  lBody: TPasImplBlock;

begin
  ParseRoutines([
    'procedure Alpha;',
    'begin',
    '  Exit;',
    '  A;',
    '  B;',
    'end;']);
  lBody:=BodyOf('Alpha');
  lCFG:=TPasCFG.Create(lBody);
  try
    AssertEquals('statements after an exit form an unreachable block',Dump([
      'block 0 entry',
      '  succ 1',
      'block 1',
      '  stmt TPasImplSimple@4',
      '  succ 2',
      'block 2 exit',
      'block 3',
      '  stmt TPasImplSimple@5',
      '  stmt TPasImplSimple@6',
      '  succ 2']),lCFG.AsText([ctoSourcePositions]));
    AssertTrue('the exit statement is reachable',
      lCFG.Reachable(lCFG.NodeOf(TPasElement(lBody.Elements[0]))));
    AssertFalse('the block after it is not',
      lCFG.Reachable(lCFG.NodeOf(TPasElement(lBody.Elements[1]))));
  finally
    lCFG.Free;
  end;
end;


procedure TTestCFG.TestCFG_NestedLoopsBreakTargetsInner;

begin
  AssertDump('break targets the inner loop',[
    'procedure Alpha;',
    'begin',
    '  while c do',
    '    begin',
    '    while d do',
    '      Break;',
    '    end;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  succ 2',
    'block 2',
    '  stmt TPasImplWhileDo@4',
    '  succ 3',
    '  succ 7',
    'block 3',
    '  stmt TPasImplBeginBlock@5',
    '  succ 4',
    'block 4',
    '  stmt TPasImplWhileDo@6',
    '  succ 5',
    '  succ 6',
    'block 5',
    '  stmt TPasImplSimple@7',
    '  succ 6',
    'block 6',
    '  succ 2',
    'block 7',
    '  stmt TPasImplSimple@9',
    '  succ 8',
    'block 8 exit']);
end;


procedure TTestCFG.TestCFG_ContinueAfterNestedLoop;

begin
  AssertDump('continue after an inner loop returns to the outer header',[
    'procedure Alpha;',
    'begin',
    '  while c do',
    '    begin',
    '    while d do',
    '      A;',
    '    Continue;',
    '    end;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  succ 2',
    'block 2',
    '  stmt TPasImplWhileDo@4',
    '  succ 3',
    '  succ 7',
    'block 3',
    '  stmt TPasImplBeginBlock@5',
    '  succ 4',
    'block 4',
    '  stmt TPasImplWhileDo@6',
    '  succ 5',
    '  succ 6',
    'block 5',
    '  stmt TPasImplSimple@7',
    '  succ 4',
    'block 6',
    '  stmt TPasImplSimple@8',
    '  succ 2',
    'block 7',
    '  stmt TPasImplSimple@10',
    '  succ 8',
    'block 8 exit']);
end;


procedure TTestCFG.TestCFG_TryFinallyEdgesFromEveryStatement;

begin
  AssertDump('every statement of a finally-protected body edges to the finally entry',[
    'procedure Alpha;',
    'begin',
    '  try',
    '    A;',
    '    B;',
    '    C;',
    '  finally',
    '    F;',
    '  end;',
    '  D;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplTry@4',
    '  succ 2',
    'block 2',
    '  stmt TPasImplSimple@5',
    '  succ 3',
    '  succ 5',
    'block 3',
    '  stmt TPasImplSimple@6',
    '  succ 4',
    '  succ 5',
    'block 4',
    '  stmt TPasImplSimple@7',
    '  succ 5',
    'block 5',
    '  stmt TPasImplTryFinally@8',
    '  stmt TPasImplSimple@9',
    '  stmt TPasImplSimple@11',
    '  succ 6',
    'block 6 exit']);
end;


procedure TTestCFG.TestCFG_BranchInsideProtectedBody;

begin
  AssertDump('a branch inside a protected body edges to the handler after both its own edges',[
    'procedure Alpha;',
    'begin',
    '  try',
    '    if c then',
    '      A;',
    '    B;',
    '  finally',
    '    F;',
    '  end;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplTry@4',
    '  succ 2',
    'block 2',
    '  stmt TPasImplIfElse@5',
    '  succ 3',
    '  succ 4',
    '  succ 5',
    'block 3',
    '  stmt TPasImplSimple@6',
    '  succ 4',
    '  succ 5',
    'block 4',
    '  stmt TPasImplSimple@7',
    '  succ 5',
    'block 5',
    '  stmt TPasImplTryFinally@8',
    '  stmt TPasImplSimple@9',
    '  succ 6',
    'block 6 exit']);
end;


procedure TTestCFG.TestCFG_GotoOutOfTryBodyOrdersEdges;

begin
  AssertDump('a resolved goto edge precedes the handler edge of the same block',[
    '{$goto on}',
    'procedure Alpha;',
    'label L;',
    'begin',
    '  try',
    '    goto L;',
    '  finally',
    '    F;',
    '  end;',
    '  L:',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplTry@6',
    '  succ 2',
    'block 2',
    '  stmt TPasImplGoto@7',
    '  succ 4',
    '  succ 3',
    'block 3',
    '  stmt TPasImplTryFinally@8',
    '  stmt TPasImplSimple@9',
    '  succ 4',
    'block 4',
    '  stmt TPasImplLabelMark@11',
    '  stmt TPasImplSimple@12',
    '  succ 5',
    'block 5 exit']);
end;


procedure TTestCFG.TestCFG_TryExceptEdgesFromEveryStatement;

begin
  AssertDump('every statement of an except-protected body edges to the handler entry',[
    'procedure Alpha;',
    'begin',
    '  try',
    '    A;',
    '    B;',
    '    C;',
    '  except',
    '    H;',
    '  end;',
    '  D;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplTry@4',
    '  succ 2',
    'block 2',
    '  stmt TPasImplSimple@5',
    '  succ 3',
    '  succ 5',
    'block 3',
    '  stmt TPasImplSimple@6',
    '  succ 4',
    '  succ 5',
    'block 4',
    '  stmt TPasImplSimple@7',
    '  succ 6',
    '  succ 5',
    'block 5',
    '  stmt TPasImplTryExcept@8',
    '  stmt TPasImplSimple@9',
    '  succ 6',
    'block 6',
    '  stmt TPasImplSimple@11',
    '  succ 7',
    'block 7 exit']);
end;


procedure TTestCFG.TestCFG_TryExceptOnHandlersWithElse;

begin
  AssertDump('the handler entry dispatches to the on branches and then to the else',[
    'procedure Alpha;',
    'begin',
    '  try',
    '    A;',
    '  except',
    '    on E1 do B;',
    '    on E2 do C;',
    '  else',
    '    D;',
    '  end;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplTry@4',
    '  succ 2',
    'block 2',
    '  stmt TPasImplSimple@5',
    '  succ 7',
    '  succ 3',
    'block 3',
    '  stmt TPasImplTryExcept@6',
    '  succ 4',
    '  succ 5',
    '  succ 6',
    'block 4',
    '  stmt TPasImplExceptOn@7',
    '  stmt TPasImplSimple@7',
    '  succ 7',
    'block 5',
    '  stmt TPasImplExceptOn@8',
    '  stmt TPasImplSimple@8',
    '  succ 7',
    'block 6',
    '  stmt TPasImplTryExceptElse@9',
    '  stmt TPasImplSimple@10',
    '  succ 7',
    'block 7',
    '  succ 8',
    'block 8 exit']);
end;


procedure TTestCFG.TestCFG_TryExceptOnHandlersWithoutElse;

begin
  AssertDump('without an else the dispatch gets no edge past the on branches',[
    'procedure Alpha;',
    'begin',
    '  try',
    '    A;',
    '  except',
    '    on E1 do B;',
    '  end;',
    '  D;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplTry@4',
    '  succ 2',
    'block 2',
    '  stmt TPasImplSimple@5',
    '  succ 5',
    '  succ 3',
    'block 3',
    '  stmt TPasImplTryExcept@6',
    '  succ 4',
    'block 4',
    '  stmt TPasImplExceptOn@7',
    '  stmt TPasImplSimple@7',
    '  succ 5',
    'block 5',
    '  stmt TPasImplSimple@9',
    '  succ 6',
    'block 6 exit']);
end;


procedure TTestCFG.TestCFG_TryExceptOnHandlersWithoutElseInsideOuterTry;

begin
  AssertDump('the unmatched exception continues to the enclosing finally',[
    'procedure Alpha;',
    'begin',
    '  try',
    '    try',
    '      A;',
    '    except',
    '      on E1 do B;',
    '    end;',
    '  finally',
    '    F;',
    '  end;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplTry@4',
    '  succ 2',
    'block 2',
    '  stmt TPasImplTry@5',
    '  succ 3',
    '  succ 8',
    'block 3',
    '  stmt TPasImplSimple@6',
    '  succ 7',
    '  succ 4',
    'block 4',
    '  stmt TPasImplTryExcept@7',
    '  succ 5',
    '  succ 8',
    'block 5',
    '  stmt TPasImplExceptOn@8',
    '  succ 6',
    '  succ 8',
    'block 6',
    '  stmt TPasImplSimple@8',
    '  succ 7',
    '  succ 8',
    'block 7',
    '  succ 8',
    'block 8',
    '  stmt TPasImplTryFinally@10',
    '  stmt TPasImplSimple@11',
    '  succ 9',
    'block 9 exit']);
end;


procedure TTestCFG.TestCFG_NestedTryFinally;

begin
  AssertDump('the inner finally is protected by the outer one',[
    'procedure Alpha;',
    'begin',
    '  try',
    '    try',
    '      A;',
    '    finally',
    '      F;',
    '    end;',
    '  finally',
    '    G;',
    '  end;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplTry@4',
    '  succ 2',
    'block 2',
    '  stmt TPasImplTry@5',
    '  succ 3',
    '  succ 6',
    'block 3',
    '  stmt TPasImplSimple@6',
    '  succ 4',
    'block 4',
    '  stmt TPasImplTryFinally@7',
    '  succ 5',
    '  succ 6',
    'block 5',
    '  stmt TPasImplSimple@8',
    '  succ 6',
    'block 6',
    '  stmt TPasImplTryFinally@10',
    '  stmt TPasImplSimple@11',
    '  succ 7',
    'block 7 exit']);
end;


procedure TTestCFG.TestCFG_StatementInsideTryHasANode;

var
  lCFG: TPasCFG;
  lTry: TPasImplTry;

begin
  ParseRoutines([
    'procedure Alpha;',
    'begin',
    '  try',
    '    A;',
    '  finally',
    '    F;',
    '  end;',
    'end;']);
  lTry:=TPasImplTry(BodyOf('Alpha').Elements[0]);
  lCFG:=TPasCFG.Create(BodyOf('Alpha'));
  try
    AssertNotNull('the protected statement has a node',
      lCFG.NodeOf(TPasElement(lTry.Elements[0])));
    AssertTrue('and that node is reachable',
      lCFG.Reachable(lCFG.NodeOf(TPasElement(lTry.Elements[0]))));
  finally
    lCFG.Free;
  end;
end;


procedure TTestCFG.TestCFG_RaiseInProtectedBody;

begin
  AssertDump('a raise reaches the finally and ends flow',[
    'procedure Alpha;',
    'begin',
    '  try',
    '    raise E;',
    '    A;',
    '  finally',
    '    F;',
    '  end;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplTry@4',
    '  succ 2',
    'block 2',
    '  stmt TPasImplRaise@5',
    '  succ 3',
    'block 3',
    '  stmt TPasImplTryFinally@7',
    '  stmt TPasImplSimple@8',
    '  succ 4',
    'block 4 exit',
    'block 5',
    '  stmt TPasImplSimple@6',
    '  succ 3']);
end;


procedure TTestCFG.TestCFG_BareRaiseInHandler;

begin
  AssertDump('a re-raise in the outermost handler ends flow, so its block is last but reachable',[
    'procedure Alpha;',
    'begin',
    '  try',
    '    A;',
    '  except',
    '    raise;',
    '  end;',
    '  D;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplTry@4',
    '  succ 2',
    'block 2',
    '  stmt TPasImplSimple@5',
    '  succ 3',
    '  succ 5',
    'block 3',
    '  stmt TPasImplSimple@9',
    '  succ 4',
    'block 4 exit',
    'block 5',
    '  stmt TPasImplTryExcept@6',
    '  stmt TPasImplRaise@7']);
end;


procedure TTestCFG.TestCFG_BareRaiseInNestedHandler;

begin
  AssertDump('a re-raise reaches the enclosing handler, not its own',[
    'procedure Alpha;',
    'begin',
    '  try',
    '    try',
    '      A;',
    '    except',
    '      raise;',
    '    end;',
    '  except',
    '    H;',
    '  end;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplTry@4',
    '  succ 2',
    'block 2',
    '  stmt TPasImplTry@5',
    '  succ 3',
    '  succ 7',
    'block 3',
    '  stmt TPasImplSimple@6',
    '  succ 4',
    '  succ 5',
    'block 4',
    '  succ 8',
    'block 5',
    '  stmt TPasImplTryExcept@7',
    '  succ 6',
    '  succ 7',
    'block 6',
    '  stmt TPasImplRaise@8',
    '  succ 7',
    'block 7',
    '  stmt TPasImplTryExcept@10',
    '  stmt TPasImplSimple@11',
    '  succ 8',
    'block 8',
    '  succ 9',
    'block 9 exit']);
end;


procedure TTestCFG.TestCFG_RaiseOutsideTry;

begin
  AssertDump('an unprotected raise gets no successor',[
    'procedure Alpha;',
    'begin',
    '  raise E;',
    '  A;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplRaise@4',
    'block 2 exit',
    'block 3',
    '  stmt TPasImplSimple@5',
    '  succ 2']);
end;


procedure TTestCFG.TestCFG_RaiseAsBranch;

begin
  AssertDump('a raise branch does not join',[
    'procedure Alpha;',
    'begin',
    '  if c then',
    '    raise E;',
    '  B;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplIfElse@4',
    '  succ 2',
    '  succ 3',
    'block 2',
    '  stmt TPasImplRaise@5',
    'block 3',
    '  stmt TPasImplSimple@6',
    '  succ 4',
    'block 4 exit']);
end;


procedure TTestCFG.TestCFG_ExitInsideTryFinally;

begin
  AssertDump('an exit is not sequenced through the finally it crosses',[
    'procedure Alpha;',
    'begin',
    '  try',
    '    Exit;',
    '  finally',
    '    F;',
    '  end;',
    '  D;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplTry@4',
    '  succ 2',
    'block 2',
    '  stmt TPasImplSimple@5',
    '  succ 4',
    '  succ 3',
    'block 3',
    '  stmt TPasImplTryFinally@6',
    '  stmt TPasImplSimple@7',
    '  stmt TPasImplSimple@9',
    '  succ 4',
    'block 4 exit']);
end;


procedure TTestCFG.TestCFG_GotoIntoTryBody;

begin
  AssertDump('a goto reaches a label block inside a try body',[
    '{$goto on}',
    'procedure Alpha;',
    'label L;',
    'begin',
    '  goto L;',
    '  try',
    '  L:',
    '    A;',
    '  finally',
    '    F;',
    '  end;',
    '  C;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplGoto@6',
    '  succ 2',
    'block 2',
    '  stmt TPasImplLabelMark@8',
    '  succ 3',
    '  succ 4',
    'block 3',
    '  stmt TPasImplSimple@9',
    '  succ 4',
    'block 4',
    '  stmt TPasImplTryFinally@10',
    '  stmt TPasImplSimple@11',
    '  stmt TPasImplSimple@13',
    '  succ 5',
    'block 5 exit',
    'block 6',
    '  stmt TPasImplTry@7',
    '  succ 2']);
end;


procedure TTestCFG.TestCFG_OnHandlerWithEmptyBody;

begin
  AssertDump('an on handler with an empty body joins after the try',[
    'procedure Alpha;',
    'begin',
    '  try',
    '    A;',
    '  except',
    '    on E1 do ;',
    '  end;',
    '  D;',
    'end;'],[
    'block 0 entry',
    '  succ 1',
    'block 1',
    '  stmt TPasImplTry@4',
    '  succ 2',
    'block 2',
    '  stmt TPasImplSimple@5',
    '  succ 5',
    '  succ 3',
    'block 3',
    '  stmt TPasImplTryExcept@6',
    '  succ 4',
    'block 4',
    '  stmt TPasImplExceptOn@7',
    '  succ 5',
    'block 5',
    '  stmt TPasImplSimple@9',
    '  succ 6',
    'block 6 exit']);
end;


procedure TTestCFG.TestCFG_EveryImplElementDescendantIsCovered;

var
  I: Integer;
  lName: string;
  lCovered,lDeclared,lExempt,lOffenders: TStringList;

begin
  ParseCoverageFixtures;
  lOffenders:=TStringList.Create;
  try
    lCovered:=TStringList.Create;
    try
      CollectCoveredClasses(lCovered);
      lExempt:=TStringList.Create;
      try
        for I:=Low(cExemptImplElements) to High(cExemptImplElements) do
          lExempt.Values[cExemptImplElements[I].ClassName]:=cExemptImplElements[I].Reason;
        lDeclared:=DeclaredDescendants;
        try
          for I:=0 to lDeclared.Count-1 do
            begin
            lName:=lDeclared[I];
            if (lCovered.IndexOf(lName)<0) and (lExempt.IndexOfName(lName)<0) then
              lOffenders.Add(lName+' is declared in pastree.pp but is neither covered by a fixture nor exempt');
            if (lCovered.IndexOf(lName)>=0) and (lExempt.IndexOfName(lName)>=0) then
              lOffenders.Add(lName+' is both covered by a fixture and listed as exempt as '+lExempt.Values[lName]);
            end;
          for I:=0 to lCovered.Count-1 do
            if lDeclared.IndexOf(lCovered[I])<0 then
              lOffenders.Add(lCovered[I]+' reached the graph but pastree.pp declares no such '+cImplElementRoot+' descendant');
          for I:=Low(cExemptImplElements) to High(cExemptImplElements) do
            if lDeclared.IndexOf(cExemptImplElements[I].ClassName)<0 then
              lOffenders.Add(cExemptImplElements[I].ClassName+', exempt as '+cExemptImplElements[I].Reason
                             +', is declared by pastree.pp as no such '+cImplElementRoot+' descendant');
        finally
          lDeclared.Free;
        end;
      finally
        lExempt.Free;
      end;
    finally
      lCovered.Free;
    end;
    if lOffenders.Count>0 then
      Fail(cImplElementRoot+' coverage sweep:'+LineEnding+lOffenders.Text);
  finally
    lOffenders.Free;
  end;
end;


initialization
  RegisterTests([TTestCFG]);
end.
