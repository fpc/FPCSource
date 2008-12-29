{ %cpu=i386,powerpc}
{ %target=linux,win32,go32v2,os2,beos,haiku,morphos }
{ %opt=-ghcl }

{ Source provided for Free Pascal Bug Report 3348 }
{ Submitted by "Martin Schreiber" on  2004-10-10 }
{ e-mail:  }
program project1;

{$mode objfpc}{$H+}
// compile with -gh -gc -gl
uses
  Classes;
type
 integerarty = array of integer;

procedure proc(ar: array of integer);
begin
end;

var
 ar1: integerarty;
begin
 HaltOnNotReleased := true;
 ar1:= nil;
 proc(ar1); // checkpointer error (nil!)
end.
