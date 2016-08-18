program createlst;

uses
  SysUtils, Classes;

var
  i, j, ioerror : LongInt;
  outfile : text;
  sr: TSearchRec;
  path, path2: String;
  sl: TStringList;
begin
  if ParamCount < 2 then
    begin
      Writeln('createlst OUTPUTFILE PATH [PATH [...]]');
      Halt(1);
    end;

  sl := TStringList.Create;

{$i-}
  assign(outfile,paramstr(1));
  rewrite(outfile);
  ioerror:=IOResult;

  if ioerror<>0 then
    begin
      Writeln('Rewrite(',ParamStr(1),') failed, IOResult=',ioerror);
      Halt(2);
    end;

  for i := 2 to ParamCount do
    begin
      path := IncludeTrailingPathDelimiter(ParamStr(i));
      { Generate path2, with all DirectorySeparators
        converted to / to get same output as with
        script before }
      path2:=path;
      if DirectorySeparator<>'/' then
        for j:=1 to length(path2) do
          if path2[j]=DirectorySeparator then
            path2[j]:='/';
      if FindFirst(path + 't*.pp', 0, sr) = 0 then
        begin
          repeat
            sl.Add(path2 + sr.Name);
          until FindNext(sr) <> 0;
          FindClose(sr);
        end;
    end;

  sl.Sort;
  for i := 0 to sl.Count - 1 do
    begin
      Writeln(outfile,sl[i]);
      ioerror:=IOResult;
      if ioerror<>0 then
        begin
          Writeln('write to file ',ParamStr(1),' failed, IOResult=',ioerror);
          Halt(3);
        end;
    end;

  close(outfile);
  ioerror:=IOResult;
  if ioerror<>0 then
    begin
      Writeln('Close(',ParamStr(1),') failed, IOResult=',ioerror);
      Halt(4);
    end;
  sl.Free;
end.
