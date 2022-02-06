program tfuncref17;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch typehelpers}

uses
  ufuncref10;

type
  TTestFunc8 = class(TInterfacedObject, ITestFunc8)
    function Invoke: LongInt;
    function Foobar: LongInt;
  end;

  TLongIntHelper = type helper for LongInt
    function Foobar: LongInt;
  end;

function TTestFunc8.Invoke: LongInt;
begin
  Result := 21;
end;

function TTestFunc8.Foobar: LongInt;
begin
  Result := 42;
end;

function TLongIntHelper.Foobar: LongInt;
begin
  Result := 2;
end;

var
  i: ITestFunc8;
begin
  i := TTestFunc8.Create;
  { non-Delphi modes don't calls ITestFunc8.Invoke and thus execute Foobar
    of ITestFunc8 }
  if i.Foobar <> 42 then
    Halt(1);
end.
