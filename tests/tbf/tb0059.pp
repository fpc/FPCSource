{ %FAIL }
{ Old file: tbf0310.pp }
{ local and para dup are not detected                    OK 0.99.15 (FK) }

procedure p(s:string);
var
  s : string;
begin
  writeln(s);
end;

begin
  p('test');
end.
