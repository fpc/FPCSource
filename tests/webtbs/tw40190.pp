{ %opt=-O2 }
{ %norun }
function InternalInt64ToInt8(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): Boolean;
begin
  if (Value and $FFFFFFFFFFFFFF80 <> 0) and (Value and $FFFFFFFFFFFFFF80 <> $FFFFFFFFFFFFFF80) then begin
  // remainder not relevant
  end;
end;  

begin
end.
