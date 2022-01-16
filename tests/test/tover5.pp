{ %FAIL }

program tover5;

{$mode objfpc}

type
  TTestClass = class
    procedure Test(aArg1: LongInt);
    function Test(aArg1: LongInt): LongInt;
  end;

procedure TTestClass.Test(aArg1: LongInt);
begin
end;

function TTestClass.Test(aArg1: Longint): LongInt;
begin
end;

begin
end.
