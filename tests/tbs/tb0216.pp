{ Old file: tbs0255.pp }
{ internal error 10 with in and function calls         OK 0.99.12 (FK) }


function a: char;
begin
  a:='c';
end;

begin
  if #12 in [a, a, a, a, a] then ; { <--- }
end.
