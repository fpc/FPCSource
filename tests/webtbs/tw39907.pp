{ %NORUN }
{ %RECOMPILE }

{$mode objfpc}
{$modeswitch functionreferences}

program tw39907;
uses
  uw39907;

var
  obj: TObject;
  proc: reference to procedure;
begin
  obj := TObject.Create;
  proc := @obj.Free;
end.

