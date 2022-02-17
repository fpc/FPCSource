{ %OPT=-O- }
uses
  math,sysutils;
var
  d : double;  
  s : string;    
begin
  SetExceptionMask([exInvalidOp]);
  d:=Infinity;
  str(frac(d),s);
  writeln(s);
  if pos('Nan',s)=0 then
    halt(1);
  d:=-Infinity;
  str(frac(d),s);
  writeln(s);
  if pos('Nan',s)=0 then
    halt(1);
  d:=NaN;
  str(frac(d),s);
  writeln(s);
  if pos('Nan',s)=0 then
    halt(1);
end.
