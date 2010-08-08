program tobject5;

{$apptype console}
{$mode objfpc}{$H+}
var
  Obj: TObject;
begin
  Obj := TObject.Create;
  if not Obj.Equals(Obj) then
    halt(1); // true
  WriteLn(Obj.GetHashCode); // PtrInt(Obj)
  if Obj.UnitName<>'System' then
    halt(2); // System
  if Obj.ToString<>'TObject' then
    halt(3); // TObject
  Obj.Free;
end.

