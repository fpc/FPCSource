program Project1;

{$mode delphi}

uses
   SysUtils;

var
   v : Double;
begin
  try
    TryStrToFloat('9e9999', v);
  except
    on e: Exception do begin
      writeln('Unexpected exception thrown: ',e.ClassName);
      halt(1);
    end;
  end;
  writeln('ok')
end.
