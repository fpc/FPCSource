program tfuncref16;

{$mode delphi}{$H+}
{$modeswitch functionreferences}

uses
  ufuncref10;

type
  TTestFunc8 = class(TInterfacedObject, ITestFunc8)
    function Invoke: LongInt;
    function Foobar: LongInt;
  end;

  TLongIntHelper = record helper for LongInt
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
  { Delphi mode calls ITestFunc8.Invoke and thus applies Foobar to the result
    type LongInt }
  if i.Foobar <> 2 then
    Halt(1);
end.
