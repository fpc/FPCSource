program bug;
var i:byte;
    e:extended;
    s:string;
begin
e:=103; (*1003,100003,1000003*)
for i:=0 to 17 do
    begin
    str(e:0:i,s);
    writeln(s);
    end;

end.
