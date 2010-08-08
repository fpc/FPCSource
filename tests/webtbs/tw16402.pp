uses typinfo;
type
  TEnum = (Name1,Name2,Name3);
begin
  if GetEnumNameCount(TypeInfo(TEnum))<>3 then
    halt(1);
end.
