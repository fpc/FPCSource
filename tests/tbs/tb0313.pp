uses ub0313;

var
  arec : rec;

begin
  arec.nrs:=one;
  if arec.nrs<>one then
    begin
       Writeln('Error with enums inside objects');
       Halt(1);
    end;
end.
