{ %NORUN }

program tgeneric89;

{$mode delphi}

type
  TTest<T> = record

  end;

  PTestLongInt = ^TTest<LongInt>;
  PTestBoolean = ^TTest<Boolean>;

begin

end.
