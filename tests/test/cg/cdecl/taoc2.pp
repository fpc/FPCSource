{ %FAIL }
{ second simple array of const test }
{ there is no way to know the address of anything
  as the array of const is pushed directly }

{$mode objfpc}


program test_cdecl_array_of_const;

var
 l : longint;

const
  has_errors : boolean = false;

procedure test(format : pchar; const args : array of const; var ll : longint);cdecl;
begin
 ll:=5;
end;

begin
 l:=4;
 test('dummy',[],l);
 if l<>5 then
   has_errors:=true;
 l:=4;
 test('dummy',[345],l);
 if l<>5 then
   has_errors:=true;
 if has_errors then
   halt(1);
end.
