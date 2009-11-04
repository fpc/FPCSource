program tobject1;

{$apptype console}
{$mode objfpc}{$H+}
var
  Obj: TObject;
begin
  Obj := TObject.Create;
  WriteLn(Obj.Equals(Obj)); // true
  WriteLn(Obj.GetHashCode); // PtrInt(Obj)
  WriteLn(Obj.UnitName); // System
  WriteLn(Obj.ToString); // TObject
  Obj.Free;
end.

