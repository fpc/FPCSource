{
    $Id$

    Fake GDBCon unit (including base from GDBInt)

 **********************************************************************}
unit GDBCon;
interface

type
  TGDBInterface=object
    constructor Init;
    destructor  Done;
    { Hooks }
    procedure DoSelectSourceline(const fn:string;line:longint);virtual;
    procedure DoStartSession;virtual;
    procedure DoBreakSession;virtual;
    procedure DoEndSession(code:longint);virtual;
    procedure DoDebuggerScreen;virtual;
    procedure DoUserScreen;virtual;
  end;

  PGDBController=^TGDBController;
  TGDBController=object(TGDBInterface)
    progname   : pchar;
    in_command,
    init_count : longint;
    debugger_started,
    call_reset : boolean;
    constructor Init;
    destructor  Done;
    procedure Command(const s:string);
    procedure Reset;
    procedure StartTrace;
    procedure TraceStep;
    procedure TraceNext;
    { needed for dos because newlines are only #10 (PM) }
    procedure WriteErrorBuf;
    procedure WriteOutputBuf;
    function  LoadFile(const fn:string):boolean;
    procedure ClearSymbols;
  end;


implementation

constructor TGDBController.Init;
begin
  inherited Init;
end;


destructor TGDBController.Done;
begin
  inherited Done;
end;


procedure TGDBController.Command(const s:string);
begin
end;


procedure TGDBController.Reset;
begin
end;


function TGDBController.LoadFile(const fn:string):boolean;
begin
end;


var
  stepline : longint;
procedure TGDBController.StartTrace;
begin
  stepline:=1;
  DoSelectSourceLine('test.pas',stepline);
end;


procedure TGDBController.TraceStep;
begin
  inc(stepline);
  DoUserScreen;
  DoDebuggerScreen;
  DoSelectSourceLine('test.pas',stepline);
end;


procedure TGDBController.TraceNext;
begin
  inc(stepline,2);
  DoUserScreen;
  DoDebuggerScreen;
  DoSelectSourceLine('test.pas',stepline);
end;


procedure TGDBController.ClearSymbols;
begin
end;


procedure TGDBController.WriteErrorBuf;
begin
end;


procedure TGDBController.WriteOutputBuf;
begin
end;


constructor TGDBInterface.Init;
begin
end;


destructor TGDBInterface.Done;
begin
end;


procedure TGDBInterface.DoSelectSourceline(const fn:string;line:longint);
begin
end;


procedure TGDBInterface.DoStartSession;
begin
end;


procedure TGDBInterface.DoBreakSession;
begin
end;


procedure TGDBInterface.DoEndSession(code:longint);
begin
end;


procedure TGDBInterface.DoDebuggerScreen;
begin
end;


procedure TGDBInterface.DoUserScreen;
begin
end;



end.
{
  $Log$
  Revision 1.1  1999-01-28 19:56:12  peter
    * moved to include compiler/gdb independent of each other

  Revision 1.1  1999/01/22 10:24:17  peter
    + gdbcon fake unit

}

