{ %FAIL }
{ Source provided for Free Pascal Bug Report 4529 }
{ Submitted by "Vincent Snijders" on  2005-11-20 }
{ e-mail: vsnijders@quicknet.nl }
program Project1;

{$mode objfpc}{$H+}{$static on}

uses
  Classes
  { add your units here };

type
  MyClass = class(TComponent)
    FClassVar: integer; static;
  end;

begin
end.
