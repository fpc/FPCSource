{
  $Id$
}
unit initc;

interface


 {$LINKLIB cygwin}
 {$linklib kernel32}

{ this unit is just ment to run
  startup code to get C code to work correctly PM }


implementation

uses
  windows;

{$i textrec.inc}


procedure cygwin_crt0(p : pointer);cdecl;external;

{
procedure do_global_dtors;cdecl;external;
 this does not work because
 do_global_dtors is a static C function PM
 it is inserted into the atexit chain,
 but how do we call this from FPC ???
 it seems to be done in exit function
 but that one ends with _exit that is system dependent !! }

{ avoid loading of cygwin _exit code
  so that exit returns
  apparently this is not enough anymore
  use longjmp instead PM }
var
  entryjmpbuf,exitjmpbuf : jmp_buf;
const
  exitjmpbufset : boolean = false;

procedure _exit(status : longint);cdecl;
begin
  if exitjmpbufset then
    longjmp(exitjmpbuf,1)
  else
    RunError(status);
end;

procedure C_exit(status : longint);popstack;external name '_exit';

const
   STD_INPUT_HANDLE = $fffffff6;
   STD_OUTPUT_HANDLE = $fffffff5;
   STD_ERROR_HANDLE = $fffffff4;


procedure UpdateStdHandle(var t:TextRec;var stdHandle:longint;newHandle:longint);
{ Check if the stdHandle is the same as the one in the TextRec, then
  also update the TextRec }
begin
  if t.Handle=stdHandle then
   t.Handle:=newHandle;
  stdHandle:=newHandle;
end;

function entry  : longint;
begin
  longjmp(entryjmpbuf,1);
  entry:=0;
end;
var
  ConsoleMode: DWORD;
  ConsoleModeValid : boolean;

initialization
  ConsoleModeValid:=GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), @ConsoleMode);
  if setjmp(entryjmpbuf)=0 then
    begin
      cygwin_crt0(@entry);
    end;

  if ConsoleModeValid then
    SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), ConsoleMode);
{ Reinitialize std handles that can be changed }
  UpdateStdHandle(TextRec(Input),StdInputHandle,GetStdHandle(STD_INPUT_HANDLE));
  UpdateStdHandle(TextRec(Output),StdOutputHandle,GetStdHandle(STD_OUTPUT_HANDLE));
  TextRec(StdOut).Handle:=StdOutputHandle;
  UpdateStdHandle(TextRec(Stderr),StdErrorHandle,GetStdHandle(STD_ERROR_HANDLE));

finalization
{ should we pass exit code ?
  its apparently only used by _exit so it doesn't matter PM }
if setjmp(exitjmpbuf)=0 then
  begin
    exitjmpbufset:=true;
    { C_exit(errorcode);
      this code does not work correctly anymore
      C function _exit is not called at end of exit function
      thus the code of exit does not return at all
      disabled PM }
  end;
end.
{
  $Log$
  Revision 1.7  2002-09-07 16:01:28  peter
    * old logs removed and tabs fixed

}
