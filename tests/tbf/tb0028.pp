{ %FAIL }
{ Old file: tbf0157.pp }
{ Invalid compilation and also crashes                  OK 0.99.7 (PFV) }

{ this should be rejected because we only accept integer args }

program write_it;
var x,y:real;
    i : longint;
    s : string;
begin
x:=5.6;
y:=45.789;
write(y:2:3,' ',x:3:4);
write(i:5);
s:='short';
write(s:11);
write(i:5:2);
write(s:25:3);
write(x:5.2);
end.
