{

    Fake GDBCon unit (including base from GDBInt)

 **********************************************************************}
unit GDBCon;
interface
uses
  GdbInt;

type
  PGDBController=^TGDBController;
  TGDBController=object(TGDBInterface)
    progname   : pchar;
    progargs   : pchar;
    in_command,
    init_count : longint;
    constructor Init;
    destructor  Done;
    procedure CommandBegin(const s:string);virtual;
    procedure Command(const s:string);
    procedure CommandEnd(const s:string);virtual;
    procedure Reset;virtual;
    procedure StartTrace;
    procedure Run;virtual;
    procedure TraceStep;virtual;
    procedure TraceNext;virtual;
    procedure TraceStepI;virtual;
    procedure TraceNextI;virtual;
    procedure Continue;virtual;
    { needed for dos because newlines are only #10 (PM) }
    procedure WriteErrorBuf;
    procedure WriteOutputBuf;
    function  GetOutput : Pchar;
    function  GetError : Pchar;
    function  LoadFile(const fn:string):boolean;
    procedure SetDir(const s : string);
    procedure SetArgs(const s : string);
    procedure ClearSymbols;
  end;

  { gdb does not allow \ in dir or filenames }
  procedure UnixDir(var s : string);


implementation


procedure UnixDir(var s : string);
var i : longint;
begin
  for i:=1 to length(s) do
    if s[i]='\' then s[i]:='/';
end;


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


procedure TGDBController.CommandBegin(const s:string);
begin
end;


procedure TGDBController.CommandEnd(const s:string);
begin
end;


procedure TGDBController.Reset;
begin
end;


function TGDBController.LoadFile(const fn:string):boolean;
begin
  LoadFile:=true;
end;

procedure TGDBController.SetArgs(const s : string);
begin
end;


procedure TGDBController.SetDir(const s : string);
begin
end;

procedure TGDBController.StartTrace;
begin
  Run;
end;


procedure TGDBController.Run;
begin
end;


procedure TGDBController.TraceStep;
begin
end;


procedure TGDBController.TraceNext;
begin
end;


procedure TGDBController.TraceStepI;
begin
end;


procedure TGDBController.TraceNextI;
begin
end;


procedure TGDBController.Continue;
begin
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


function  TGDBController.GetOutput : Pchar;
begin
  GetOutput:=nil;
end;


function  TGDBController.GetError : Pchar;
begin
  GetError:=nil;
end;

end.
