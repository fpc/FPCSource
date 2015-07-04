{$mode delphi}

program TestAbsolute; {$apptype console}

function IsInt16 (L : longint) : boolean;
var W : smallint absolute L;
begin
  Result := longint (W) = L;
end;

function IsInt32 (Q : int64) : boolean;
var L : longint absolute Q;
begin
  Result := int64 (L) = Q;
end;

const VL1 : longint = -1;
      VL2 : longint = $12345678;
      VQ1 : int64   = -1;
      VQ2 : int64   = $123456781234;

begin
  if not IsInt16 (VL1) then
    halt(1);
  if IsInt16 (VL2) then
    halt(2);
  if not IsInt32 (VQ1) then
    halt(3);
  if IsInt32 (VQ2) then
    halt(4);
end.
