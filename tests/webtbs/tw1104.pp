var
  r1,r2 : extended;
  code : integer;
begin
  val('.',r1,code);
  if r1<>0.0 then
   writeln('error with val(".")');
  val('.E',r2,code);
  if r2<>0.0 then
   writeln('error with val(".E")');
  if (r1<>0.0) or (r2<>0.0) then
   halt(1);
end.
