{ Source provided for Free Pascal Bug Report 3433 }
{ Submitted by "Mattias Gaertner" on  2004-12-06 }
{ e-mail: mattias@freepascal.org }
program CompareFunctionTypes;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  TMyFunction = function(Item: Pointer): integer of object;

procedure DoSomething(const Func1, Func2: TMyFunction);
begin
  if Func1=Func2 then exit;
end;

begin
  DoSomething(nil,nil);
end.
