{ %OPT=-O1 }
program tw40647;
var
  k: longint = 2;
  x: longint;
begin
  { Tests faulty LEA optimisations that manifest under -O1 since the node
    tree doesn't simplify k + k in this instance }
  x := k + k + 1;
  if x <> 5 then
    begin
      WriteLn('FAIL: Expected 5 but got ', x);
      Halt(1);
    end;
    
  WriteLn('ok');
end.
