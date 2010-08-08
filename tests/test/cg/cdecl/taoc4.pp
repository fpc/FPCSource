{ %fail }
{ %cpu=i386 }

{ This test expects values on the stack, which is i386 only }

{ This test should fail, because it can indeed only ever work on i386,
  and even there the default typing by the compiler is wrong.  }

{ fourth simple array of const test }

{$mode objfpc}


program test_cdecl_array_of_const;

var
 l : longint;

const
  has_errors : boolean = false;

procedure test_one_longint(args : array of const);cdecl;
var
  p : pptrint;
begin
 p:=pptrint(@args);
 l:=p^;
end;

procedure test_two_longints(args : array of const);cdecl;
var
  p : pptrint;
begin
 p:=pptrint(@args);
 inc(pointer(p),sizeof(ptrint));
 l:=p^;
end;

begin
 l:=4;
 test_one_longint([345]);
 if l<>345 then
   has_errors:=true;
 l:=4;
 test_one_longint([345,245]);
 if l<>345 then
   has_errors:=true;
 l:=4;
 test_one_longint([345,245,678]);
 if l<>345 then
   has_errors:=true;
 l:=4;
 test_two_longints([345,456]);
 if l<>456 then
   has_errors:=true;
 if has_errors then
   begin
     Writeln('cdecl array of const problem');
     halt(1);
   end;
end.
