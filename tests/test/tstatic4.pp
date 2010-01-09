{ %FAIL}
program tstatic4;
{$APPTYPE console}
{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

type

  { TSomeClass }

  TSomeClass = class
  public
    class procedure StaticProc; static;
    procedure RegularProc;
  end;


{ TSomeClass }

procedure TSomeClass.RegularProc;
begin

end;

class procedure TSomeClass.StaticProc;
begin
  RegularProc;
end;

begin
end.