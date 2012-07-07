{ %NORUN }

program tgeneric81;

{$mode delphi}

type
  PTest = ^TTest;
  TTest<T, S> = record
  end;
  TTest<T> = record
  end;
  TTest = record
  end;

begin

end.
