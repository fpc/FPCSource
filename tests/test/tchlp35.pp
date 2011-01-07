{ tests virtual methods inside class helpers }
program tchlp35;

uses
  uchlp35;

type
  TObjectHelperB = class helper(TObjectHelperA) for TObject
    function VirtualTest: Integer; override;
  end;

function TObjectHelperB.VirtualTest: Integer;
begin
  Result := 2;
end;

var
  o: TObject;
  res: Integer;
begin
  o := TObject.Create;
  res := o.Test;
  if res <> 2 then
    Halt(1);
end.

