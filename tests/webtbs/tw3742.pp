{ %OPT=-gh }
{ Source provided for Free Pascal Bug Report 3742 }
{ Submitted by "Martin Schreiber" on  2005-03-04 }
{ e-mail:  }
program project1;

{$mode objfpc}{$H+}
//compile with -glh

uses
  Classes;

type
 integerarty = array of integer;
 scopestackcachety = record
  startscope: integer;
  stack: integerarty;
 end;

 scopestackcachearty = array of scopestackcachety;

var
 ar2: scopestackcachearty;

procedure testproc;

var
 ar1: integerarty;

begin
 setlength(ar1,2);
 setlength(ar2,2);
 ar2[0].stack:= copy(ar1,0,1);
 ar2[1].stack:= copy(ar1,0,1);
 writeln('refcount a 0: ',pinteger(pchar(pointer(ar2[0].stack)-8))^);
 writeln('refcount a 1: ',pinteger(pchar(pointer(ar2[1].stack)-8))^);
end;

begin
 HaltOnNotReleased := true;
 testproc;
 writeln('refcount b 0: ',pinteger(pchar(pointer(ar2[0].stack)-8))^);
 writeln('refcount b 1: ',pinteger(pchar(pointer(ar2[1].stack)-8))^);
 finalize(ar2);
end.
