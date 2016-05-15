{ %recompile }

{$mode objfpc}

{$inline on}

uses
  uw18121;

var
  IntO: TPointerList2;
begin
  IntO := TPointerList2.Create;
  IntO.SetF(PInteger(nil));
  IntO.WriteLn;
  IntO.Free;
end.
