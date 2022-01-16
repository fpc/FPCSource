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
        writeln(Infinity);
      end;
    end;
  except
    halt(1);
  end;
  writeln('ok');
end.
