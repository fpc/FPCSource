{ %OPT=-O2 }
{$MODE ObjFpc}

function IsPos(const v:Double):Boolean;
begin
  Result := (PChar(@v)+6)^ > #0;
end;

begin
  WriteLn(IsPos(23));
end.                               