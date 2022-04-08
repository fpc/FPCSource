program tw39656;

uses
  sysutils,math;
var
  r: double;
  ar: array of double = (0.001, 0.5, 0.7, 0.999);
  idx: LongInt;
begin
  {Write('good:');
  for r in ar do Write(FloatToStr(r), ' ');
  Writeln;}

  //Write('bad:');
  idx:=0;
  for r in [0.001, 0.5, 0.7, 0.999] do begin
    //Write(FloatToStr(r), ' ');
    if not SameValue(r,ar[idx]) then
      Halt(idx+1);
    Inc(idx);
  end;
  //Writeln;
end.

