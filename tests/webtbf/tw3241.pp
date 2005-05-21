{ %fail }

{ Source provided for Free Pascal Bug Report 3241 }
{ Submitted by "Mattias Gaertner" on  2004-08-09 }
{ e-mail: mattias@freepascal.org }
program TwoDefaults;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  TMyClass = class
  private
    function GetA1(Index: integer): integer;
    function GetA2(Index: integer): integer;
  public
    property A1[Index: integer]: integer read GetA1; default;
    { Next line should give an error }
    property A2[Index: integer]: integer read GetA2; default;
  end;

{ TMyClass }

function TMyClass.GetA1(Index: integer): integer;
begin
  Result:=0;
end;

function TMyClass.GetA2(Index: integer): integer;
begin
  Result:=0;
end;

begin
end.
