program intpow;
{$apptype console}
{$mode delphi}

uses sysutils, math;
var
  x,d: double;
begin
{$ifdef FPC_HAS_TYPE_DOUBLE}
  writeln('Spurious overflows in intpower');
  try
    d := 10;
    x := intpower(d,-314);
    writeln('10^(-314) = ',x); //should be 1e-315
  except
    on E: Exception do writeln('10^(-314) (should be 1e-314): ', E.Message);
  end;
  try
    d := 2;
    x := intpower(d,-2000);
    writeln('2^(-2000) = ',x); //should be 0
  except
    on E: Exception do writeln(' 0.5^2000 (should be 0) : ', E.Message);
  end;
{$endif FPC_HAS_TYPE_DOUBLE}
end.
