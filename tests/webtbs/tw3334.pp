{ Source provided for Free Pascal Bug Report 3334 }
{ Submitted by "Martin Schreiber" on  2004-09-28 }
{ e-mail:  }
program project1;

{$mode objfpc}{$H+}

uses
  Classes;

procedure p1;  
type
 integerarty = array of integer;
var
 ar1,ar2: integerarty;
begin
 setlength(ar1,4);
 ar2:= copy(ar1,1,2);
end;

var
 mem1,mem2 : longint;
begin
  mem1:=heapsize-memavail;
  p1;
  mem2:=heapsize-memavail;
  writeln(mem1,' - ',mem2);
  if mem1<>mem2 then
    halt(1);
end.


