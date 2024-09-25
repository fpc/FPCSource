program Project1;

{$mode objfpc}{$H+}

uses
  math, sysutils;
var
  a,b: double;
begin
  a := 0;
  b := -3;
  try
    try
      writeln(power(a,b));
    except
      on e: EZeroDivide do begin
        writeln('EZeroDivide Exception: ',e.Message);
        writeln(Infinity);
      end;
      on e: EMathError do begin
        writeln('MathError Exception: ',e.Message);
        writeln(Infinity);
      end;
    end;
  except
    on e:Exception do begin
      writeln('Error: exception at wrong level: ',e.Message);
      halt(1);
    end;
  end;
  writeln('ok');
end.
