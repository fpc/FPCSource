{ %NORUN }

program tgeneric119;

{$mode objfpc}
{$warn 5024 error}
{$warn 5036 error}

type
  generic TTest<T> = class
    procedure Test(aArg: T);
  end;

{$push}
{$warn 5024 off}
procedure TTest.Test(aArg: T);
var
  v: LongInt;
begin
  {$push}
  {$warn 5036 off}
  Writeln(v);
  {$pop}
end;
{$pop}

type
  TTestLongInt = specialize TTest<LongInt>;

var
  t: TTestLongInt;
begin
  t := TTestLongInt.Create;
  t.Free;
end.
