
{ first simple array of const test }

{$mode objfpc}


program test_cdecl_array_of_const;

var
 l : longint;

const
  has_errors : boolean = false;

procedure test(var ll : longint;format : pchar; const args : array of const);cdecl;
begin
 ll:=5;
end;

begin
 l:=4;
 test(l,'dummy',[]);
 if l<>5 then
   has_errors:=true;
 l:=4;
 test(l,'dummy',[345]);
 if l<>5 then
   has_errors:=true;
 if has_errors then
   halt(1);
end.
