{ Old file: tbs0041.pp }
{  shows the if then end. problem                      OK 0.9.9 (FK) }

var
 b1: boolean;
Begin
  begin
     If b1 then      { illegal expression }
  end;
  while b1 do
End.
