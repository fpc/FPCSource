{ Old file: tbs0157.pp }
{ Invalid compilation and also crashes                  OK 0.99.7 (PFV) }

{ this should be rejected because we only accept integer args }

program write_it;
var x,y:real;
begin
x:=5.6;
y:=45.789;
write(y:2:3,x:3:4);
{write(y:3.2,x:5.2);}
end.
