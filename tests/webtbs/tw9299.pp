{$mode objfpc}
{$R+}

function GetShiftedCard(const c: Cardinal): Cardinal;
begin
  Result := c shl 24;
end;

function GetShiftedByte(const c: Byte): Cardinal;
begin
  Result := c shl 24;
end;

begin
  WriteLn(GetShiftedCard(200));

  WriteLn(GetShiftedByte(200));

end.
