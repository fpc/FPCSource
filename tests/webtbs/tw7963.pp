{ %fail }
program fpcbug;

{$mode objfpc}

type
  TCallback = function(const a, b, c: pointer): integer; stdcall;

var
  Proc: TCallback;
  a,b: Pointer;

begin
  Proc(a, b);
  Proc(a, b, a, b);
end.
