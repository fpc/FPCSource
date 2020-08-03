{ %CPU=i386 }
{ %OPT=-Cp80386 }
program NaNTest;
{$mode objfpc}
{$SAFEFPUEXCEPTIONS on} // does not change anything
uses Math;
var
  B: Boolean;
  N1, N2: Extended;
  S: string;
begin
  N1 := NaN;
  try
    B := N1<4;
  except
    halt(0);
  end;
  halt(1);
end.
