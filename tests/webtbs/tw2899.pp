{ Source provided for Free Pascal Bug Report 2899 }
{ Submitted by "Mattias Gaertner" on  2004-01-17 }
{ e-mail: mattias@freepascal.org }
program StringCallByRef;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

uses
  Classes, SysUtils;

procedure DoSomething(const AString: string);

  procedure NestedProc(var Dummy: string);
  begin
    Dummy:=Dummy; // dummy statement, no change
  end;

var
  s: String;
begin
  s:=copy(AString,5,11);
  writeln('Before NestedProc: "',s,'"');
  NestedProc(s);
  writeln('After NestedProc: "',s,'"'); // s is now emtpy
  if s<>'AStrangeBug' then
    halt(1);
end;

begin
  DoSomething('WhatAStrangeBug');
end.
