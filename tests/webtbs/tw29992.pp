{ %norun }

{$mode objfpc}

procedure Test(constref AParam: String = 'def_param');
begin
  writeln(AParam);
end;

begin
end.
