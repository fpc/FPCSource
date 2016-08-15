program createlst;

uses
  SysUtils, Classes;

var
  i: LongInt;
  sr: TSearchRec;
  path: String;
  sl: TStringList;
begin
  if ParamCount = 0 then begin
    Writeln('createlst PATH [PATH [...]]');
    Exit;
  end;

  sl := TStringList.Create;

  for i := 1 to ParamCount do begin
    path := IncludeTrailingPathDelimiter(ParamStr(i));
    if FindFirst(path + 't*.pp', 0, sr) = 0 then begin
      repeat
        sl.Add(path + sr.Name);
      until FindNext(sr) <> 0;

      FindClose(sr);
    end;
  end;

  sl.Sort;
  for i := 0 to sl.Count - 1 do
    Writeln(sl[i]);

  sl.Free;
end.
