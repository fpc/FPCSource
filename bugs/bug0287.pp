var
  b : boolean;
begin
  b:=(true > false);
  if b then

   writeln('ok 1');
  b:=true;
  b:=(b > false);
  if b then

   writeln('ok 2');
end.
