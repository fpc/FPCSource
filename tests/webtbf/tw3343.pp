{ %fail }

{ Source provided for Free Pascal Bug Report 3343 }
{ Submitted by "Martin Schreiber" on  2004-10-06 }
{ e-mail:  }
program project1;

{$mode objfpc}{$H+}

uses
  Classes;

type

 itest = interface
 end;

 ttestclass = class(tinterfacedobject)
 end;

var
 testclass: ttestclass;
 testintf: itest;
begin
 testclass:= ttestclass.create;
 testintf:= itest(testclass);
 //fpc: project1.pas(21,13) Warning: Class types "ttestclass" and "itest" are not related
 //kylix: [Error] project1.pas(21): Incompatible types: 'itest' and 'ttestclass'
end.
