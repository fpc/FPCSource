{ Source provided for Free Pascal Bug Report 3185 }
{ Submitted by "Martin Schreiber" on  2004-06-25 }
{ e-mail:  }
program project1;

{$mode objfpc}{$H+}

uses
  Classes;
type
 enum1ty = (a1,b1,c1,d1);
 set1ty = set of enum1ty;
 enum2ty = (a2=ord(a1),b2=ord(b1),c2=ord(c1),d2=ord(d1),e2);
 set2ty = set of enum2ty;

procedure proc(aset: set2ty);
begin
 if aset = [b2] then begin
  writeln(longword(aset),' ok');
 end
 else begin
  writeln(longword(aset),' error');
  halt(1);
 end;
end;

var
 en1: enum1ty;

begin
 en1:= b1;
 proc(set2ty(longword([en1]))); //exp: 2 ok act: 1 error
end.
