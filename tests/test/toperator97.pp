{ %NORUN }

program toperator97;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = record
    class operator :=(aArg: TTest): Pointer;
  end;

class operator TTest.:=(aArg: TTest): Pointer;
begin
end;

var
  b: PByte;
  t: TTest;
begin
  b := t;
end.
