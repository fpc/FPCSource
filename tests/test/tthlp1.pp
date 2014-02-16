{ %NORUN }

{ this checks that the basic type helper syntax is parsed correctly }

program tthlp1;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

type
  TTest = type helper for LongInt
    procedure Test;
  end;

procedure TTest.Test;
begin

end;

begin

end.

