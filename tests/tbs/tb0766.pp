{ %target=win32,win64,linux }

{ the end of a thread writes out each of its standard files -- Output,
  ErrOutput, StdOut and StdErr -- before the memory manager's DoneThread and
  again after it, and leaves alone one that was closed: all four open, then
  StdErr closed. Recorded without allocating: each file's device notes its
  letter (o e s r), in capitals when called with something to write; m the
  memory manager's DoneThread, M its end. 0 when every cell's log is as
  expected; otherwise the cell's number times ten plus: 1 it differs, 3
  DoneThread was not called once, 4 the thread did not end }
program tb0766;

{$mode objfpc}
{$I-}

{$ifdef unix}
uses
  cthreads;
{$endif unix}

var
  OldMM, HookMM: TMemoryManager;
  Armed: TThreadID;
  { a shortstring: nothing allocated }
  Log: shortstring;
  DoneCalls: longint;
  { whether the thread closes StdErr }
  CloseErr: boolean;

procedure Note(c: char);
begin
  if GetCurrentThreadId=Armed then
    Log:=Log+c;
end;

procedure Called(var t: TextRec; c: char);
begin
  if t.BufPos<>0 then
    Note(UpCase(c))
  else
    Note(c);
  t.BufPos:=0;
end;

procedure DevOutput(var t: TextRec);
begin
  Called(t,'o');
end;

procedure DevErrOutput(var t: TextRec);
begin
  Called(t,'e');
end;

procedure DevStdOut(var t: TextRec);
begin
  Called(t,'s');
end;

procedure DevStdErr(var t: TextRec);
begin
  Called(t,'r');
end;

procedure HookDone;
begin
  Note('m');
  if GetCurrentThreadId=Armed then
    begin
      Write(Output,'h');
      Write(ErrOutput,'h');
      Write(StdOut,'h');
      if not CloseErr then
        Write(StdErr,'h');
    end;
  if assigned(OldMM.DoneThread) then
    OldMM.DoneThread();
  Note('M');
  if GetCurrentThreadId=Armed then
    inc(DoneCalls);
end;

procedure Use(var f: Text; dev: CodePointer);
begin
  TextRec(f).InOutFunc:=dev;
  TextRec(f).FlushFunc:=nil;
end;

function Body(p: pointer): ptrint;
begin
  Armed:=GetCurrentThreadId;
  Use(Output,@DevOutput);
  Use(ErrOutput,@DevErrOutput);
  Use(StdOut,@DevStdOut);
  Use(StdErr,@DevStdErr);
  { closing writes the file out once; the standard handle stays open }
  if CloseErr then
    Close(StdErr);
  Write(Output,'x');
  Write(ErrOutput,'x');
  Write(StdOut,'x');
  if not CloseErr then
    Write(StdErr,'x');
  Body:=0;
end;

procedure Cell(n: longint; closed: boolean; const expected: shortstring);
var
  h: TThreadID;
begin
  CloseErr:=closed;
  Log:='';
  DoneCalls:=0;
  h:=BeginThread(@Body);
  if WaitForThreadTerminate(h,20000)<>0 then
    halt(n*10+4);
  CloseThread(h);
  Armed:=TThreadID(0);
  if DoneCalls<>1 then
    halt(n*10+3);
  if Log<>expected then
    begin
      writeln('cell ',n,': ',Log,' expected ',expected);
      halt(n*10+1);
    end;
end;

begin
{$if defined(windows) and not defined(FPC_USE_TLS_DIRECTORY)}
  { without a TLS directory nothing tells the run time library of the end of a
    thread that returns from its body, so there is nothing to test }
  writeln('skipped: the compiler makes no TLS directory');
  exit;
{$endif}
{$ifdef FPC_STDOUT_TRUE_ALIAS}
  writeln('skipped: StdOut and StdErr are Output and ErrOutput here');
  exit;
{$endif FPC_STDOUT_TRUE_ALIAS}
  Armed:=TThreadID(0);
  GetMemoryManager(OldMM);
  HookMM:=OldMM;
  HookMM.DoneThread:=@HookDone;
  SetMemoryManager(HookMM);
  Cell(1,false,'OESRmMOESR');
  Cell(2,true,'rOESmMOES');
  SetMemoryManager(OldMM);
end.
