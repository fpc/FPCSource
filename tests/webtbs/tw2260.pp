{ %version=1.1 }

{$mode delphi}

procedure test(x: integer = 2);
begin
 writeln(x);
end;

begin
 test(1);
 test;
end.
