{ %FAIL }
{ Old file: tbf0314.pp }
{  }

procedure p(var b);
begin
end;

var
  s : string;
begin
  p(@s[1]);
end.
