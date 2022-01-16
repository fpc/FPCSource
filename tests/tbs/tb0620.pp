{ %NORUN }

program tb0620;

type
  generic TTest<T, S> = object
    procedure Test(aArg: T; aArg2: S);
  end;

procedure TTest.Test(aArg: T; aArg2: S);
begin
  Delete(aArg, aArg2, 4);
  Insert('Test', aArg, aArg2);
  Writeln(aArg);
end;

type
  TTestShortString = specialize TTest<ShortString, LongInt>;
  TTestUnicodeString = specialize TTest<UnicodeString, LongInt>;

var
  tss: TTestShortString;
  tus: TTestUnicodeString;
begin
  tss.Test('Hello World', 3);
  tus.Test('Hello World', 4);
end.
