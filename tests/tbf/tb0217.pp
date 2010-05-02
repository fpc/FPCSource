{ %fail }

{$p+}
{$v+}
type
  tstr = string[8];

{ FPC used to convert the following parameter into an openstring }
procedure test(var str: tstr);
begin
end;

var
  s: string[20];
begin
  test(s);
end.
