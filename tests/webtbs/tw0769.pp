{ %OPT=-Sew -vw }
{ this should generate no warning }

{$mode tp}

Program test;

var x,y:integer;

begin
y:=5;
for x:=0 to 10 do if x<y then writeln(x);
end.
