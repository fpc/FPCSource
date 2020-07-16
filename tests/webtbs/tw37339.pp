{$mode objfpc}
uses
  variants;
   
function VarToObj(const Value: Variant): TObject;
begin
  Result := TObject(Pointer(NativeInt(Value)));
end; 

begin
end.
  