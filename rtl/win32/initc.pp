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

function GetStdHandle(nStdHandle:DWORD):longint;external 'kernel32' name 'GetStdHandle';

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

initialization
  if setjmp(entryjmpbuf)=0 then
    begin
      cygwin_crt0(@entry);
    end;
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
  Revision 1.5  2001-09-22 11:15:31  peter
    * merged v10 version for exit fixes

  Revision 1.1.2.4  2001/09/19 15:23:39  pierre
   * work for newer cygwin version

  Revision 1.1.2.3  2001/04/23 01:15:44  carl
  - removed unused and useless ifdef

  Revision 1.1.2.2  2001/04/02 13:30:14  pierre
   * Remove call to C exit procedure as it does not return anymore

  Revision 1.1.2.1  2000/12/30 17:49:48  peter
    * update std handles after initializing c

}
