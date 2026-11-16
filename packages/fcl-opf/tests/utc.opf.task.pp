unit utc.opf.task;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry,
{$ENDIF}
  Opf.Task;

Type
  TFailAt = (fNone,fExecute,fUndo,fBoth);

  { TFailTask }

  TFailTask = Class(TOPTask)
  Private
    FDoAutoUndo: Boolean;
    FFail : TFailAt;
  Protected
    Procedure DoExecute; override;
    Procedure DoUndo; override;
    Function GetAutoUndo: Boolean; override;
  Public
    Constructor Create(aFailAt : TFailAt);
    Property DoAutoUndo : Boolean Read FDoAutoUndo Write FDoAutoUndo;
  end;

  { TTestOPTask }

  { TBaseTestOPTask }

  TBaseTestOPTask = Class(TTestCase)
  Public
    Class Procedure AssertEquals(Msg : String; aExpected,aActual : TOPTaskStatus); overload;
  end;

  TTestOPTask = Class(TBaseTestOPTask)
  private
    FTask: TOPTask;
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    Procedure Init(aFailAt : TFailAt);
    Property Task : TOPTask Read FTask;
  Published
    Procedure TestEmpty;
    Procedure TestTaskOK;
    Procedure TestTaskNOK;
    Procedure TestTaskNOKAutoUndo;
    Procedure TestTaskNOKAutoUndoFail;
    Procedure TestTaskUndoFail;
  end;

  { TTestOPTaskList }

  TTestOPTaskList = Class(TBaseTestOPTask)
  private
    FListTask: TOPListTask;
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    Procedure Init(aCount : Integer; aFailAtIndex : Integer = -1);
    Property List : TOPListTask Read FListTask;
  Published
    Procedure TestEmpty;
    Procedure TestAddSingle;
    Procedure TestAddArray;
    Procedure TestAddFPList;
    Procedure TestAddList;
    Procedure TestExecuteAllOK;
    Procedure TestExecuteNallOK;
    Procedure TestUndoAfterSuccessUndoesAll;
    Procedure TestUndoAfterFailureUndoesExecuted;
  end;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.TypInfo;
{$ELSE}
  typinfo;
{$ENDIF}

{ TBaseTestOPTask }

class procedure TBaseTestOPTask.AssertEquals(Msg: String; aExpected, aActual: TOPTaskStatus);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TOPTaskStatus),Ord(aExpected)),GetEnumName(TypeInfo(TOPTaskStatus),Ord(aActual)));
end;

{ TTestOPTaskList }

procedure TTestOPTaskList.SetUp;
begin
  inherited SetUp;
  FListTask:=TOPListTask.Create;
end;

procedure TTestOPTaskList.TearDown;
begin
  FreeandNil(FListTask);
  inherited TearDown;
end;

procedure TTestOPTaskList.Init(aCount: Integer; aFailAtIndex: Integer);

Const
  FailAts : Array[Boolean] of TFailAt = (fNone,fExecute);

Var
  I : Integer;

begin
  for I:=0 to aCount-1 do
    List.Add(TFailTask.Create(FailAts[i=aFailAtIndex]));
end;


procedure TTestOPTaskList.TestEmpty;
begin
  AssertNotNull('Have task',List);
end;

procedure TTestOPTaskList.TestAddSingle;

Var
  T : TOPTask;

begin
  T:=TFailTask.Create(fNone);
  List.Add(T);
  AssertEquals('Count',1,List.Count);
  AssertSame('Item',T,List[0]);
end;

procedure TTestOPTaskList.TestAddArray;
Var
  T1,t2 : TOPTask;

begin
  T1:=TFailTask.Create(fNone);
  T2:=TFailTask.Create(fNone);
  List.Add([T1,T2]);
  AssertEquals('Count',2,List.Count);
  AssertSame('Item 0',T1,List[0]);
  AssertSame('Item 1',T2,List[1]);
end;

procedure TTestOPTaskList.TestAddFPList;

Var
  L : TFPList;
  i : integer;

begin
  L:=TFPList.Create;
  try
    For I:=1 to 3 do
      L.Add(TFailTask.Create(fNone));
    List.Add(L);
    AssertEquals('Count',L.Count,List.Count);
    For I:=0 to L.Count-1 do
      AssertSame('Item '+IntToStr(i),L[i],List[i]);
  finally
    L.Free;
  end;
end;

procedure TTestOPTaskList.TestAddList;

Var
  L : TList;
  i : integer;

begin
  L:=TList.Create;
  try
    For I:=1 to 3 do
      L.Add(TFailTask.Create(fNone));
    List.Add(L);
    AssertEquals('Count',L.Count,List.Count);
    For I:=0 to L.Count-1 do
      AssertSame('Item '+IntToStr(i),L[i],List[i]);
  finally
    L.Free;
  end;
end;

procedure TTestOPTaskList.TestExecuteAllOK;

Var
  i : Integer;

begin
  init(3,-1);
  List.Execute;
  AssertEquals('Status OK',tsExecuted,List.Status);
  For I:=0 to List.Count-1 do
    AssertEquals('Status '+IntToStr(I)+' OK',tsExecuted,List[i].Status);
end;

procedure TTestOPTaskList.TestExecuteNallOK;

Var
  i : Integer;

begin
  init(3,1);
  AssertException('Exception',Exception,@List.Execute);
  AssertEquals('Status OK',tsExecuteFailed,List.Status);
  AssertEquals('Status 0 OK',tsExecuted,List[0].Status);
  AssertEquals('Status 1 OK',tsExecuteFailed,List[1].Status);
  AssertEquals('Status 2 OK',tsTodo,List[2].Status);
end;

procedure TTestOPTaskList.TestUndoAfterSuccessUndoesAll;
var
  I: Integer;
begin
  Init(3);
  List.Execute;
  List.Undo;
  for I:=0 to 2 do
    AssertEquals('Subtask '+IntToStr(I)+' undone', tsUndone, List[I].Status);
end;

procedure TTestOPTaskList.TestUndoAfterFailureUndoesExecuted;
begin
  Init(3, 1);
  try
    List.Execute;
  except
    on E: Exception do ;
  end;
  List.Undo;
  AssertEquals('Executed subtask undone', tsUndone, List[0].Status);
  AssertEquals('Failed subtask undone', tsUndone, List[1].Status);
  AssertEquals('Subtask never run stays', tsTodo, List[2].Status);
end;

{ TTestOPTask }


procedure TTestOPTask.SetUp;

begin
  inherited SetUp;
  Init(fNone);
end;

procedure TTestOPTask.TearDown;

begin
  FreeAndNil(FTask);
  Inherited TearDown;
end;

procedure TTestOPTask.Init(aFailAt: TFailAt);

begin
  FreeAndNil(FTask);
  FTask:=TFailTask.Create(aFailAt);
end;

procedure TTestOPTask.TestEmpty;
begin
  AssertNotNull('Have task',FTask);
end;

procedure TTestOPTask.TestTaskOK;
begin
  FTask.Execute;
  AssertEquals('Done',tsExecuted,Task.Status);
end;

procedure TTestOPTask.TestTaskNOK;
begin
  Init(fExecute);
  AssertException('Fail',Exception,@FTask.Execute);
  AssertEquals('Done',tsExecuteFailed,Task.Status);
end;

procedure TTestOPTask.TestTaskNOKAutoUndo;
begin
  Init(fExecute);
  TFailTask(Task).DoAutoUndo:=True;
  AssertException('Task fails',Exception,@FTask.Execute);
  AssertEquals('Done',tsUndone,Task.Status);
end;

procedure TTestOPTask.TestTaskNOKAutoUndoFail;
begin
  Init(fBoth);
  TFailTask(Task).DoAutoUndo:=True;
  try
    FTask.Execute;
    Fail('Task fails');
  except
    on E: Exception do
      AssertEquals('The execute error, not the undo error', 'Failing at execute', E.Message);
  end;
  AssertEquals('Done',tsUndoFailed,Task.Status);
end;

procedure TTestOPTask.TestTaskUndoFail;
begin
  Init(fUndo);
  AssertException('Undo fails',Exception,@FTask.Undo);
  AssertEquals('Done',tsUndoFailed,Task.Status);
end;

{ TFailTask }

procedure TFailTask.DoExecute;
begin
  if FFail in [fExecute,fBoth] then
    Raise Exception.Create('Failing at execute');
  if Status<>tsExecuting then
    TAssert.Fail('Status is not correct');
end;

procedure TFailTask.DoUndo;
begin
  if FFail in [fUndo,fBoth] then
    Raise Exception.Create('Failing at undo');
  if Status<>tsUndoing then
    TAssert.Fail('Status is not correct');
end;

function TFailTask.GetAutoUndo: Boolean;
begin
  Result:=FDoAutoUndo;
end;


constructor TFailTask.Create(aFailAt: TFailAt);
begin
  FFail:=aFailAt;
end;

initialization
  RegisterTests([TTestOPTask,TTestOPTaskList]);
end.

