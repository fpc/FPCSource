uses
  sysutils;
var
  ExecutableName: String;
begin
  ExecutableName:=ExpandFileName(paramstr(0));
  if 3600*24*(now()-FileDateToDateTime(FileAge(ExecutableName)))>7200 then
    begin
      writeln('FileAge returns: ',FileDateToDateTime(FileAge(ExecutableName)));
      writeln('Executable file time and run time differ too much, SysUtils.FileAge buggy?');
      halt(1);
    end;

  { test with relative path }
  if 3600*24*(now()-FileDateToDateTime(FileAge(ExtractRelativePath(GetCurrentDir+DirectorySeparator,ExecutableName))))>7200 then
    begin
      writeln('FileAge returns: ',FileDateToDateTime(FileAge(ExecutableName)));
      writeln('Executable file time (relative path) and run time differ too much, SysUtils.FileAge buggy?');
      halt(1);
    end;
end.
