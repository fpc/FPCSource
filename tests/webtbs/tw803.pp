{$MODE objfpc}
program FileExc;
uses SysUtils, Classes;
var
  f: TFileStream;
begin
  try
    f := TFileStream.Create('a nonexistent file', fmOpenRead);
  except
    on e: Exception do begin
      f.Free;
      halt(0);
    end;
  end;
  writeln('Error');
  halt(1);
end.
