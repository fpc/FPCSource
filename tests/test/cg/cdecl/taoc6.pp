{ %cpu=i386 }

{ This test expects values on the stack, which is i386 only }

{ sixth simple array of const test
  for int64 values }

{$mode objfpc}

program test_cdecl_array_of_const;

type
  pint64 = ^int64;
var
  l : int64;



const
  has_errors : boolean = false;

procedure test_one_int64(args : array of const);cdecl;
var
  p : pint64;
begin
 p:=pint64(@args);
 l:=p^;
end;

procedure test_two_int64(args : array of const);cdecl;
var
  p : pint64;
begin
 p:=pint64(@args);
 inc(pointer(p),sizeof(int64));
 l:=p^;
end;


var
 i,j : int64;

begin
 i:=$65ffffff;
 i:=i*5698;
 j:=2*i;
 test_one_int64([i]);
 if l<>i then
   has_errors:=true;
 l:=4;
 test_one_int64([j,i]);
 if l<>j then
   has_errors:=true;
 l:=4;
 test_one_int64([i+j,i,j]);
 if l<>i+j then
   has_errors:=true;
 l:=4;
 test_two_int64([i+j,j-i]);
 if l<>j-i then
   has_errors:=true;
 if has_errors then
   begin
     Writeln('cdecl array of const problem for int64');
     halt(1);
   end;
end.
