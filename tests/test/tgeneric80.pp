{ %NORUN }

program tgeneric80;

{$mode delphi}

type
  TTest<T, S> = record
  end;
  TTest<T> = record
  end;
  PTest = ^TTest;
  TTest = record
  end;

begin

end.
