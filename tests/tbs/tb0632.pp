{%skiptarget=$nothread }
{ Note: needs multi threading }
program tb0632;

{$mode objfpc}

uses
{$ifdef unix}
  cthreads,
{$endif}
  classes;

type
  TTest = class
  private
    fValue: LongInt;
    procedure DoTest;
  public
    procedure Test;
  end;

  TDummyThread = class(TThread)
  public
    constructor Create;
  protected
    procedure Execute; override;
  end;

{ TDummyThread }

constructor TDummyThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

procedure TDummyThread.Execute;
begin
  { empty }
end;

{ TTest }

procedure TTest.DoTest;
begin
  Inc(fValue);
end;

procedure TTest.Test;
begin
  TThread.Queue(Nil, @DoTest);
  if fValue <> 1 then
    Halt(1);
  TThread.ForceQueue(Nil, @DoTest);
  if fValue <> 1 then
    Halt(2);
  CheckSynchronize;
  if fValue <> 2 then
    Halt(3);
  Writeln('Ok');
end;

var
  t: TTest;
begin
{$ifdef FPC_HAS_FEATURE_THREADING}
  { ensure that the RTL is in multi threading mode, otherwise CheckSynchronize
    ignores the queue }
  TDummyThread.Create.Start;

  t := TTest.Create;
  try
    t.Test;
  finally
    t.Free;
  end;
{$endif}
end.
