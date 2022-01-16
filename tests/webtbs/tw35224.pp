{$mode objfpc}
{$OPTIMIZATION LOOPUNROLL}

type TList = array [0..3] of integer;

var worklist, tmplist : TList;

function flip (const list1: TList; var list2: TList): integer; inline;
var i: integer;
begin
  for i := 0 to 3 do list2[i] := list1[i];
end;

begin
  flip (worklist, tmplist);
end.
