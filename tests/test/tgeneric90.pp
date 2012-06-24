{ %NORUN }

program tgeneric90;

{$mode delphi}

type
  TTest = record

  end;

  TTest<T> = record

  end;

  TTest<T, S> = record

  end;

  PTestLongInt = ^TTest<LongInt>;
  PTestLongIntLongInt = ^TTest<LongInt, LongInt>;
  PTest = ^TTest;

begin

end.
