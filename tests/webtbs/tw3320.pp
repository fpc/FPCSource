{ %version=1.1 }

var
  a,b:array of integer;
  i :integer;
  err : boolean;
begin
  setlength(a,3);
  a[0]:=1;
  a[1]:=2;
  a[2]:=3;
  b:=a;
  writeln('len b= ',length(b)); // output is 3: OK
  if length(b)<>3 then
    err:=true;
  setlength(a,0);
  writeln('len a= ',length(a)); // output is 0: OK
  if length(a)<>0 then
    err:=true;
  for i:=1 to length(b) do writeln(b[i-1]); // output is 1: BAD
  writeln('len b= ',length(b)); // output is 1: BAD, must be 3
  if length(b)<>3 then
    err:=true;
  if err then
    halt(1);
end.
