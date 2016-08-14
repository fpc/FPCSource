program createlst;

uses
  SysUtils;

var
  i: LongInt;
  sr: TSearchRec;
  path: String;
begin
  if ParamCount = 0 then begin
    Writeln('createlst PATH [PATH [...]]');
    Exit;
  end;

  for i := 1 to ParamCount do begin
    path := IncludeTrailingPathDelimiter(ParamStr(i));
    if FindFirst(path + 't*.pp', 0, sr) = 0 then begin
      repeat
        Writeln(path + sr.Name);
      until FindNext(sr) <> 0;

      FindClose(sr);
    end;
  end;
end.
