{ %skiptarget=go32v2 }
{ %opt=-CE -Oonofastmath }

{$ifdef fpc}{$mode objfpc}{$endif}
program test_fpu_excpetions;

uses
  SysUtils;

  function mysqrt(x : real) : real;

    begin
      try
        mysqrt:=sqrt(x);
      except
        on e : exception do
          mysqrt:=0;
      end;
    end;

  var
    x, y,z : real;

begin
  x:=6.5;
  y:=5.76;
  z:=3.1;
  Writeln('Testing mysqrt (x) = sqrt(x) if x >= 0');
  Writeln('                   = 0       if x <  0');
  Writeln(' 6.5+5.76*mysqrt(3.1) = ',x+y*mysqrt(z):0:6);
  Writeln(' 6.5+5.76*mysqrt(-3.1) = ',x+y*mysqrt(-z):0:6);
  if (x+y*mysqrt(-z)<>x) then
    begin
      writeln('Error: mysqrt does not return zero for negative argument');
      halt(1);
    end;
end.
