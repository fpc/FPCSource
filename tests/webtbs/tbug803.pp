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
      WriteLn(e.Message);
      f.Free;
    end;
  end;
end.