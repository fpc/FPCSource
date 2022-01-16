{ %opt=-Sew }
{ %norun }

const
  OT_SIZE_MASK = $3000001F;
var
  l: longint;
begin
  l:=1;
  case l and OT_SIZE_MASK of
    1: writeln;
  end;
end.
