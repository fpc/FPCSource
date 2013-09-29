{ %FAIL }

program tb0232;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = record
    procedure Test; static;
  end;

procedure TTest.Test;
begin

end;

begin

end.
