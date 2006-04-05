{ %cpu=i386 }

{ This test expects values on the stack, which is i386 only }

{ fifth simple array of const test }

{$mode objfpc}


program test_cdecl_array_of_const;

var
 l : double;

const
  has_errors : boolean = false;

procedure test_one_double(args : array of const);cdecl;
type
  pdouble = ^double;
var
  p : pdouble;
begin
 p:=pdouble(@args);
 l:=p^;
end;

procedure test_two_doubles(args : array of const);cdecl;
var
  p : pdouble;
begin
 p:=pdouble(@args);
 inc(pointer(p),sizeof(double));
 l:=p^;
end;

begin
 l:=4.0;
 test_one_double([double(3.45)]);
 if abs(l-3.45)>0.01 then
   has_errors:=true;
 l:=4.0;
 test_one_double([double(3.45),double(2.45)]);
 if abs(l-3.45)>0.01 then
   has_errors:=true;
 l:=4;
 test_one_double([double(3.45),double(24.25),double(678.8)]);
 if abs(l-3.45)>0.01 then
   has_errors:=true;
 l:=4;
 test_two_doubles([double(3.45),double(4.56)]);
 if abs(l-4.56)>0.01 then
   has_errors:=true;
 if has_errors then
   begin
     Writeln('cdecl array of const problem');
     halt(1);
   end;
end.
