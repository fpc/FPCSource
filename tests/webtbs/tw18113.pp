{ %target=go32v2 }
{ %interactive }

{ check whether this program writes '*' }

Procedure Confuse;
begin

end;


Procedure TestBug(chr:word);
begin
Confuse; {if you comment it, everything is fine even in Level 2}
Mem[$B800:0]:=byte(chr);
end;

begin
writeln(#13#10#13#10);
TestBug(42); {should print '*'}
readln;
end.
