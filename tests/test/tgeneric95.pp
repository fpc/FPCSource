program tgeneric95;

{$mode objfpc}

type
  generic TTest<T> = record
    f: T;
  end;

function Test(aArg: Integer): Integer;
type
  TTest_Word = specialize TTest<Word>;
var
  t: TTest_Word;
begin
  Result := SizeOf(t.f);
end;

function Test(aArg: String): Integer;
type
  TTest_String = specialize TTest<String>;
var
  t: TTest_String;
begin
  Result := SizeOf(t.f);
end;

procedure DoError(const aMessage: String);
begin
  Writeln(aMessage);
  ExitCode := 1;
  Halt;
end;

begin
  if Test(42) <> SizeOf(Word) then
    DoError('Unexpected size of field');
  if Test('Test') <> SizeOf(String) then
    DoError('Unexpe size of field');
end.
