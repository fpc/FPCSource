{ Source provided for Free Pascal Bug Report 2595 }
{ Submitted by "Michalis Kamburelis" on  2003-07-24 }
{ e-mail: michalis@camelot.homedns.org }

{ With fpc 1.1 (from snapshot downloaded at 23.07.2003) this program causes compilation error  "Error: Wrong number of parameters specified"
  near the "F(1)" statement. But you can see everything is ok and there is no error.
  (Of course, this particular program would cause runtime error because F is not initialized, but it's semantically correct).
  Error is only under DELPHI and TP modes.
  Change declaration
    TFuncByObject = function(i:Integer):boolean of object;
  to
    TFuncByObject = procedure(i:Integer);
  (make procedure instead of a function) and everything will compile ok.
  Change it to
    TFuncByObject = function(i:Integer):boolean;
  (no longer "by object") and again everything will compile ok.
  It has to be "function" and "by object" to cause the bug.

  Observed with FPC under win32 and linux (i386).
}

{$mode DELPHI}

type
  TFuncByObject = function(i:Integer):boolean of object;

var F:TFuncByObject;
  i : integer;
begin
  i:=0;
  if i=1 then
    F(1);
end.
