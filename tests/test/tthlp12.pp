{ this checks that inheritance for type helper is working correctly }

program tthlp12;

{$mode objfpc}
{$modeswitch typehelpers}

type
  TLongIntHelper = type helper for LongInt
    function Test: Integer;
  end;

  TLongIntHelperSub = type helper(TLongIntHelper) for LongInt
    function Test: Integer;
  end;

function TLongIntHelper.Test: Integer;
begin
  Result := 21;
end;

function TLongIntHelperSub.Test: Integer;
begin
  Result := inherited Test * 2;
end;

var
  i: LongInt;
begin
  if i.Test <> 42 then
    Halt(1);
  Writeln('OK');
end.
