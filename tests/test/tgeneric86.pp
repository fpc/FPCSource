{ %NORUN }

program tgeneric86;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

type
  generic TTest<T> = record
  type
    PTest = ^TTest;
  end;

begin

end.

