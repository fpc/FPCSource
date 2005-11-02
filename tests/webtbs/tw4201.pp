{ Source provided for Free Pascal Bug Report 4201 }
{ Submitted by "Gergely Nagy" on  2005-07-19 }
{ e-mail: gergely.nagy@softreal.hu }
{ TThread.Synchronize on an abstract virtual method bug demonstration
  by Gergely Nagy <gergely.nagy@softreal.hu> }

{$mode delphi}
program fp_thread_bug_test;

uses
{$ifdef unix}
  CThreads,
{$endif unix}
  Classes;

type
  TBuggedBaseThread = class;

  TBuggedBaseThread = class (TThread)
  protected
    procedure Execute; override;
    function ExecuteOperation: Boolean; virtual; abstract;
    procedure EndOperation; virtual; abstract;
  public
    constructor Create;
    procedure StopThread;
  end;

  TBuggedThread = class (TBuggedBaseThread)
  protected
    function ExecuteOperation: Boolean; override;
    procedure EndOperation; override;
  end;

constructor TBuggedBaseThread.Create;
begin
  inherited Create(True);
end;

procedure TBuggedBaseThread.StopThread;
begin
  Terminate;
  Suspended:= False;
  WaitFor;
  Free;
end;

procedure TBuggedBaseThread.Execute;
begin
  WriteLn ('# Execute...');
  ExecuteOperation;
  WriteLn ('# 	...Going to sync...');
  Synchronize(EndOperation);
  Terminate;
end;

procedure TBuggedThread.EndOperation;
begin
  WriteLn ('# 	EndOperation');
end;

function TBuggedThread.ExecuteOperation: Boolean;
begin
  WriteLn ('# 	ExecuteOperation');
  Result:= True;
end;

var
  t: TBuggedThread;

begin
  t:= TBuggedThread.Create;

  t.Execute;
  
  t.Free();
end.
