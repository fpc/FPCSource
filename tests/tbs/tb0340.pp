{$mode objfpc}
var
  v : tvarrec;
  error : boolean;
procedure p(a:array of const);
var
  i : integer;
begin
  for i:=low(a) to high(a) do
   with a[i] do
    begin
      case vtype of
        vtInteger :
          begin
            writeln('Integer: ',VInteger);
            if VInteger=1000 then
             Error:=false;
          end;
        else
          writeln('Error!');
      end;
    end;
end;

begin
  error:=true;
  v.vtype:=vtInteger;
  v.VInteger:=1000;
  p(v);
  if Error then
   Halt(1);
end.
