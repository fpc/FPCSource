{ %target=win32,win64 }
{ %needlibrary }
{ %norun }
{ %neededafter }

{ a library with its own run time library and 64 MB of threadvars, used by the
  program of tlib4b.pp; tostdout sets this run time library's
  WriteErrorsToStdErr, which decides where its error is written }
library tlib4a;

threadvar
  tv: array[0..64*1024*1024-1] of byte;

procedure touch;
begin
  tv[0]:=1;
end;

procedure tostdout;
begin
  WriteErrorsToStdErr:=false;
end;

exports touch, tostdout;

begin
  tv[0]:=1;
end.
