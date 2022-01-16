program Project1;

{$mode objfpc}{$H+}

uses math, sysutils
  { you can add units after this };

begin
  try
    writeln(power(0, -4));
  except
    on e: Exception do ClearExceptions(false);
  end;
  try
    writeln(power(0, -3));
  except
    on e: Exception do ClearExceptions(false);
  end;
  try
    writeln(power(0, -4));
  except
    on e: Exception do ClearExceptions(false);
  end;

  writeln('caught');
  writeln(power(16, 0.5));
  writeln('done');
end.
