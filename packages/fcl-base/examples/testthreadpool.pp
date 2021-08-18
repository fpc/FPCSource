{$mode objfpc}
{$h+}

program testthreadpool;

uses {$ifdef unix}cThreads, {$ENDIF} sysutils, fpthreadpool;

type

  { TMyTask }

  TMyTask = Class(TThreadPoolTask)
    FID : Integer;
    destructor destroy; override;
    procedure DoQueued; override;
    Procedure DoExecute; override;
    Constructor Create(aID : Integer);
    Function ToString : string; override;
   end;


{ TMyTask }

destructor TMyTask.destroy;
begin
  Writeln(FID,': Destroy : ',ToString);
  Flush(Output);
  inherited destroy;
end;

procedure TMyTask.DoQueued;
begin
  Writeln(FID,': Queued : ',ToString);
  Flush(Output);
  inherited DoQueued;
end;

procedure TMyTask.DoExecute;

Var
  I,Sec: Integer;

begin
  Sec:=3+Random(3);
  Writeln(FID,': Task ',ToString,' waiting ',Sec,' seconds.');
  Flush(Output);
  I:=1;
  While (I<=Sec) and Not Terminated do
    begin
    Sleep(Sec);
    Inc(I);
    end;
  Writeln(FID,': Task ',ToString,' done waiting (',Sec,' seconds). ');
  Flush(Output);
end;

constructor TMyTask.Create(aID: Integer);
begin
  FID:=AID;
end;

function TMyTask.ToString: string;
begin
  Result:=ClassName+' '+HexStr(Self)+' : '+IntToStr(FID);
end;

procedure RunTest(aPool : TFPCustomSimpleThreadPool);

Var
  I : Integer;
  T : TMyTask;

begin
  For I:=1 to 200 do
    begin
    T:=TMyTask.Create(I);
    if not aPool.AddTask(T) then
      begin
      Writeln('Task not accepted, freeing');
      Flush(Output);
      T.Free;
      end;
    end;
end;

Var
  MyPool : TFPSimpleThreadPool;

begin
  MyPool:=TFPSimpleThreadPool.Create;
  try
    MyPool.AddTimeout:=40;
    MyPool.AutoCheckQueuedInterval:=50;
    // RunTest(MyPool);
    RunTest(TFPSimpleThreadPool.Instance);
  finally
    MyPool.Free;
  end;
end.

