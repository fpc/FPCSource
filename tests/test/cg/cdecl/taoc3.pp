{ %FAIL }
{ second simple array of const test }
{ there is no way know how many args
  are psuhed for a cdecl'ared araay of const
  the compiler should complain here }

{$mode objfpc}


program test_cdecl_array_of_const;

var
 l : longint;

const
  has_errors : boolean = false;

procedure test(format : pchar; const args : array of const);cdecl;
begin
 l:=high(args);
end;

begin
 l:=4;
 test('dummy',[234]);
 if l<>1 then
   has_errors:=true;
 if has_errors then
   halt(1);
end.
