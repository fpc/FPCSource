{ Old file: tbs0093.pp }
{ Two Cardinal type bugss                                0K 0.99.1 (FK/MvC) }

{ Two cardinal type bugs }
var
  c : cardinal;
  l : longint;
  b : byte;
  s : shortint;
  w : word;
begin
  b:=123;
  w:=s;
  l:=b;
  c:=b;         {generates movzbl %eax,%edx instead of movzbl %al,%edx}

  c:=123;
  writeln(c);   {Shows '0' outline right! instead of '123' outlined left}
  c:=$7fffffff;
  writeln(c);   {Shows '0' outline right! instead of '123' outlined left}
end.
