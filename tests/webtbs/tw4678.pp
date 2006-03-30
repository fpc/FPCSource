{ %OPT=-Sd }

{ Source provided for Free Pascal Bug Report 4678 }
{ Submitted by "Phil H." on  2006-01-09 }
{ e-mail: pjhess@purdue.edu }
program TestVarBug2;

uses
  Variants;

type
  TMyClass = class
  private
    function GetValue(AnInt : Integer) : Variant;
  public
    property Value[AnInt : Integer] : Variant read GetValue;
  end;
  
function TMyClass.GetValue(AnInt : Integer) : Variant;
begin
  if AnInt < 0 then
    Result := Null
  else
    Result := AnInt;
end;

var
  AClass : TMyClass;  
  VarVal : Variant;
begin
  AClass := TMyClass.Create;

   // This statement throws an exception with FPC.
   // Should assign Null to VarVal as per Delphi rule:
   // "any operation on a Null variant produces a Null variant".
  VarVal := AClass.Value[5] + AClass.Value[-1] + 1;

end.


