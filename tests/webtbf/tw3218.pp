{ %fail }

{ Source provided for Free Pascal Bug Report 3218 }
{ Submitted by "Vincent Snijders" on  2004-07-20 }
{ e-mail: vslist@zonnet.nl }
{$mode objfpc}

uses
  classes;

type TAProc = procedure(const s: string; o: TObject);
var AProc: TAProc;
procedure A(const s: string; c: TComponent);
begin
  c.Name := s;
end;

var
  o1: TObject;
begin
  AProc:=@A; //this line should generate an error
  o1 := TObject.Create;
  AProc('',o1);
end.
