{ Source provided for Free Pascal Bug Report 3190 }
{ Submitted by "Martin Schreiber" on  2004-06-28 }
{ e-mail:  }
program test;

{$mode delphi}

uses
 TypInfo,Classes;
var
 po1: ptypeinfo;
begin
 po1:= typeinfo(tcomponent);   //compile error: 'Illegal parameter list'
 if po1 <> nil then begin      //compiles on kylix, result: 'TComponent'
  writeln(po1^.name);
 end
 else begin
  writeln('typeinfo(tcomponent) = nil');
 end;
 po1:= typeinfo(tobject);      //compile error: 'Illegal parameter list'
 if po1 <> nil then begin      //compiles on kylix, result: 'TObject'
  writeln(po1^.name);
 end
 else begin
  writeln('typeinfo(tobject) = nil');
 end;
end.
