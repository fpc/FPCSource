{$mode objfpc}
program fpbug;

uses
  SysUtils;

var
  Value:   Cardinal;
  Success: Boolean;
begin
  Success := TryStrToDword('7795000000', Value);
  if Success then
    halt(1);
end.
