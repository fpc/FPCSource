{ Source provided for Free Pascal Bug Report 3143 }
{ Submitted by "Martin Schreiber" on  2004-06-06 }
{ e-mail:  }
program project1;

{$mode objfpc}{$H+}

uses
  Classes;

type
 testrecty = record
  refcount: integer;
 end;

var
 fonts: array of testrecty;
 test: integer;

function registerfont(var info: integer): integer;
begin
 result:= 0;
 if result = 0 then begin
  result:= length(fonts)+1;
  setlength(fonts,result);
 end;
 test:= result;    //test = 1
 with fonts[result-1] do begin
   refcount:= 1;
 end;
 test:= result;    //exp: test = 1
                   //act: test = 4617532
end;

var
 int1: integer;
begin
 test:= registerfont(int1);
 writeln(test);
 if test<>1 then
   halt(1);
end.
