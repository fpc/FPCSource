uses uregexpr;

var
  res1, res2 : boolean;
begin
  res1:=ExecRegExpr('^[\da-f]{8}(-[\da-f]{4}){3}-[\da-f]{12}$','1f20d5ba-3618-432e-9ba6-f050dc12602d');
  Writeln('1: Matches ? ',res1);
  res2:=ExecRegExpr('^[\da-f]{8}-[\da-f]{4}-[\da-f]{4}-[\da-f]{4}-[\da-f]{12}$','1f20d5ba-3618-432e-9ba6-f050dc12602d');
  Writeln('2: Matches ? ',res2);
  if not res1 or not res2 then
    begin
      Writeln('ExecRegExpr is wrong');
      halt(1);
    end;
end.

