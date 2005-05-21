{ Source provided for Free Pascal Bug Report 3334 }
{ Submitted by "Martin Schreiber" on  2004-09-28 }
{ e-mail:  }
program project1;

{$mode objfpc}{$H+}

uses
  erroru,
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
 mem : sizeint;
begin
  domem(mem);
  p1;
  if domem(mem)<>0 then
    halt(1);
end.
