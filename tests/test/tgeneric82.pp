{ %NORUN }

program tgeneric82;

{$mode delphi}

type
  TTest = record
  end;
  TTest<T, S> = record
  end;
  TTest<T> = record
  end;
  PTest = ^TTest;

begin

end.
