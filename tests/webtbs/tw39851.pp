{ %OPT=-O1 -OoPEEPHOLE }
{$mode delphi}

{$C+}

program tw39851;

function Fn1: Boolean;
begin
  Result := True;
end;

procedure TestCmpErr;
var
  I: Integer;
begin
  I := 0;
  if (I < 0) or (not Fn1()) then
  begin // this branch should NOT be executed
    ASSERT((I <= 0) and (not Fn1()));
    Halt(1);
  end;
end;

begin
  TestCmpErr;
  WriteLn('ok');
end.
