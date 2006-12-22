{ %fail }
program test;

{$mode objfpc}

uses
  Classes, SysUtils;

type
  IMyInterface = interface
    function test1: integer;
    function test2: single;
    function test3: double;
  end;

  TMyObject = class(TInterfacedObject, IMyInterface)
    function test1: byte;
    function test2: double;
    function test3: integer;
  end;

function TMyObject.test1: byte;
begin
  Result := 0;
end;

function TMyObject.test2: double;
begin
  Result := 0;
end;

function TMyObject.test3: integer;
begin
  Result := 0;
end;

begin
end.
